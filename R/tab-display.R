# PURPOSE: THE DISPLAY GRAMMAR -- how a built cell decides what to print.
# ROLE: The `{}` template writer and its declared vocabulary (DISPLAY_TOKENS), plus the base-count
#   and add_pct materialisation the exporters run. A display is an OVERLAY: get_num(), the colour
#   engine and the Excel bypass all keep reading the PRIMARY field, so changing a display never
#   changes a number.
# KEY CONSTRAINTS:
#   - display_write_col() is THE per-column template writer, shared by build-time `tab(display =)`
#     and post-hoc `set_display(col, "base_ci")`. It refuses to print one geometry's estimate beside
#     another's bracket, but a LEVEL names no comparison and so constrains the bracket not at all --
#     "48% [-3;+4]" is tabxplor's flagship cell, not a mismatch.
#   - A TOKEN MAY CARRY ITS OWN PRECISION, "{base:1}" / "{est:3}", which beats every declared
#     default (DISPLAY_TOKENS$min_digits, EST_SCALES$base_digits, the interval floor). Digits ARE a
#     display property, and the cell's `digits` field is one number for a whole cell, so the template
#     is the only place that can say "the estimate at three decimals, its aside at one". Parsed in
#     parse_display_template(), applied by the composite expander (R/fmt_class.R).
#   - A composite has a PRIMARY token -- the first one outside brackets, so an aside may be written
#     FIRST ("({base}) {est}") without ceasing to be an aside. It is what carries the stars, what
#     get_num() and Excel return, and the only part the colour paints by default. The rule lives in
#     parse_display_template() (R/fmt_class.R); the presets below are spelt to obey it.
#     ⚠ ITS CONVERSE: a template with NO token outside brackets has no primary at all, and the whole
#     cell renders as an aside -- the Total cell reduced to "({n_range})" where its block does not
#     sum. Its type TAG still names the count (a label drops the primary's own brackets): the column
#     holds a count, and the cell reads it as an aside, which are two questions.
#   - tab_narrow_default_display() states the one rule that UNDOES a default: a spread multiplies the
#     columns, so a layout nobody named falls back to its scale's own estimate. It runs at the spread
#     and tab() applies `display =` after it, which is the whole of "the user's word wins".
#   - DISPLAY_PRESETS + display_resolve() are the ONE named-layout table, read by tab() and by
#     tab_reg() alike, so a display learnt on a crosstab means the same on a regression. A preset may
#     declare one arm per column ROLE, which is where the crude/model mirror is stated -- so it holds
#     for a post-hoc set_display() as much as for a build-time one.
#   - A COMPOSITE NEVER PRINTS THE SAME FIELD TWICE. `{est}` and `{base}` are scale-RELATIVE, so two
#     tokens of one template can land on one field; such an aside is dropped exactly as a void one
#     is, and the preset degrades to the bare estimate.
#   - THE VOID RULE HAS TWO HALVES. Per CELL, a token with nothing to show renders BLANK AND KEEPS
#     ITS WIDTH, so one missing aside never breaks the column's alignment; per COLUMN, a token void
#     everywhere leaves the template with its whole bracket group, padding and all, and the note
#     names the argument that would have filled it. A void token never silently substitutes the
#     column's own primary field. display_template_keep() (R/fmt_class.R) is the shared rule.
#   - WARNING: the two template writers loop over COLUMNS, never dplyr::across() -- across() runs per
#     GROUP on a grouped tab, and "is this field empty in the whole column" would then be answered
#     per sub-table, pruning an aside out of a one-row group while its neighbours keep it.
#   - DISPLAY_TOKENS' row ORDER is a contract, and its `doc=` / `source=` strings are user-facing
#     documentation: ?tabxplor-display (declared at the bottom of this file) is THE user page for
#     the grammar, and display_tokens_rd() / display_presets_rd() fill it from these tables through
#     `@eval`, so the taught vocabulary cannot drift from the shipped one. ?tab and ?tab_reg point
#     at it rather than repeating it; ?fmt keeps the exhaustive token list, for a reader of fields.
#   - EVERY TOKEN CARRIES ITS SHORT NAME (`label=`), and that is what names a COLUMN: fmt_display_
#     label() (R/fmt_class.R) walks the column's own template and substitutes labels for tokens, so
#     the console type tag, the exports' unit header row and an Excel aside column's header all say
#     the same thing -- "row% (n)", "OR (adj%)". A label may be a closure where the name depends on
#     the column: `pct` and `mean` say WHOSE level it is on a regression column (`role` --
#     reg_role_qualifier(): observed or adjusted, where a crosstab says its row/col axis), and
#     `n_range` collapses to "n" where no range actually renders.
#   - A TEMPLATE IS STAMPED WHEREVER IT RENDERS ANYTHING, and a composite RENDERS wherever any of its
#     pieces did (format()). One column is then one layout: a numeric predictor's observed cell has
#     an odds ratio and no risk difference, and gating either on the PRIMARY left it showing a bare
#     estimate under a header promising a difference. A cell with nothing of the template at all
#     keeps its own token, and still blanks -- which is the half that matters.
#   - A LEGACY SPELLING IS RESOLVED AT A BOUNDARY, never carried by a rendering branch: `OR` is an
#     ALIAS ROW of `or` (read-side, display_primary()), and `or_pct` / `OR_pct` are alias PRESETS of
#     `or_base`, normalised by fmt() and display_resolve(). Nothing downstream knows they existed.
#   - THE BASE COUNT IS ONE DISPLAY-TIME FACT, for both producers. Nothing stores it: a crosstab
#     cell already carries its block's base in `tot_n` (a mean's in `n`), and a regression's model
#     columns carry each level's own `n`. fmt_cell_base() reads whichever applies, tab_base_range()
#     reduces it per row over a col_group, and `n = "range" | "min" | "no"` then chooses between
#     folding it into the Total cell (tab_fold_base_n), giving it a column (tab_base_n_cols, and
#     the Excel branch of tab_base_n_pct) or dropping it. A range needs no literal and no second
#     field: `{n_range}` renders `n`..`tot_n`, so format()'s per-template padding still aligns.
#     A continuous predictor has no level to count, so that cell is empty by construction -- which
#     is where its observed shape is drawn instead (mat_reg_spark, R/tab_classes.R), as a literal in
#     the cell's own template. `n = "no"` therefore takes the sparkline with it.
#   - THE TOTAL'S "100 %" HAS TWO GATES (tab_totcol_sums), and both are declared: the block's cells
#     must SHOW a level (the primary token's `geometry`, else the scale's `kind`), and its estimates
#     must ADD UP to the Total. A conjunction on purpose -- a `display =` change can remove a "100 %"
#     the cells no longer show, and can never add one the estimates do not support.
# See: CLAUDE.md § tabxplor architecture (the display grammar); R/fmt_class.R (the fields shown).

#' @keywords internal
#' @noRd
# A SPREAD MULTIPLIES THE COLUMNS -- one per level of the spread variable -- so every cell has a
# fraction of the width it had. A column still wearing the DEFAULT layout its leaf chose, nobody
# having asked for one, therefore falls back to the bare estimate ITS OWN SCALE declares
# (EST_SCALES$default_display); a layout someone NAMED is never touched. Measured on the numeric
# default that motivated this, `mean_cv`: a coefficient of variation is a second number per cell,
# and a spread is exactly where a cell can least afford one.
#
# ⚠ THE TEST IS "IS THIS STILL THE LEAF'S OWN CHOICE?", recomputed from the column's own values
# (num_default_display) rather than recorded -- which is what keeps the rule out of every other code
# path. Nothing is stored and no flag is threaded, so `display =`, `ci =` and a post-hoc
# set_display() each keep their layout by simply not matching. And it runs at the SPREAD, which
# tab() performs BEFORE tab_apply_display(), so an explicit `display =` is applied over it and wins;
# on the built table set_display() wins the same way.
#' @keywords internal
#' @noRd
tab_narrow_default_display <- function(tabs) {
  one <- function(col) {
    if (!is_fmt(col)) return(col)
    d <- unique(get_display(col))
    # a COMPOSITE default only: a bare token has nothing to drop, and a per-cell mix was never a leaf
    # default (the leaves write one layout per column).
    if (length(d) != 1L || is.na(d) || !grepl("{", d, fixed = TRUE)) return(col)
    if (!identical(d, num_default_display(get_mean(col)))) return(col)
    bare <- EST_SCALES[[get_scale(col)]]$default_display
    if (is.null(bare) || is.na(bare) || identical(bare, d)) return(col)
    fmt_set_display(col, bare)
  }
  # column by column, never dplyr::across(): across() runs PER GROUP on a grouped tab, and "the
  # column's own default" is a question about the whole column (same reason as tab_apply_display).
  for (nm in names(tabs)) tabs[[nm]] <- one(tabs[[nm]])
  tabs
}

tab_apply_display <- function(tabs, display) {
  ds <- display_resolve(display)
  if (is.null(ds)) return(tabs)
  # ⚠ INTERSECTED over the columns that could have shown the field, never unioned: "empty in this
  # table" must mean empty EVERYWHERE. A mixed factor / numeric table has no percentage on its mean
  # column, and a union made `display = "{pct} ({n})"` announce that `pct` was void while four
  # columns of the same table were printing it.
  missing_tok <- NULL
  write_col <- function(col) {
    r <- display_write_col(col, ds)
    if (!is.null(r$missing))
      missing_tok <<- if (is.null(missing_tok)) r$missing else intersect(missing_tok, r$missing)
    r$col
  }
  # WARNING: column by column, NOT dplyr::across() -- across() runs PER GROUP on a grouped tab, and
  # display_write_col()'s "empty in the WHOLE column" rule must be answered over the column, or a
  # one-row sub-table loses an aside its neighbours keep and the column stops lining up.
  set_one <- function(tab) {
    for (nm in names(tab)) if (is_fmt(tab[[nm]])) tab[[nm]] <- write_col(tab[[nm]])
    tab
  }
  out <- if (is.data.frame(tabs)) set_one(tabs) else purrr::map(tabs, set_one)
  if (length(missing_tok)) display_note_empty(missing_tok)
  out
}

# Returns the column, plus the fields empty in the WHOLE column (only the table-level caller can
# note those once). `tmpl` is a validated {} template (a preset is already resolved to one). Per-
# column adaptation needs no branch here: "{base} {ci}" IS what "each value with its interval" means,
# and each column answers `{base}` with its own level field.
#' @keywords internal
#' @noRd
display_write_col <- function(col, tmpl) {
  seg  <- parse_display_template(tmpl)
  d    <- get_display(col)
  # Only genuine value cells; p-value / blank / total-marker cells keep their own token. Read through
  # display_primary(), so a cell ALREADY carrying a composite is re-templatable: since regression
  # columns default to a two-token layout, "not re-templatable" would silently no-op the post-hoc
  # `set_display()` recipe on exactly the tables that need it most.
  elig <- display_primary(d) %in% DISPLAY_VALUE_CELLS
  # NULL, not character(0): this column holds no value cell at all, so it does not get a VOTE on
  # which fields are empty (see tab_apply_display()).
  if (!any(elig)) return(list(col = col, missing = NULL))
  display_refuse_mismatch(col, seg$fields, tmpl)
  have  <- lapply(seg$fields, function(f) !is.na(get_num(fmt_set_display(col, f))))
  empty <- vapply(have, function(h) all(!h[elig]), logical(1))
  # DESIGN: A COMPOSITE NEVER PRINTS THE SAME FIELD TWICE. The scale-relative tokens are what make a
  # preset family-agnostic, and the price is that two of them can land on one field: `{est}` IS
  # `{base}` on a level column, and IS `{coef}` on an additive one. Such an aside is dropped exactly
  # as a void one is -- so a preset degrades to the bare estimate instead of doubling it -- but it is
  # NOT reported as missing: the field is there, it is simply already printed.
  tok <- fmt_resolve_scale_tokens(seg$fields, fmt_scale_row(col))
  dup <- tok == tok[[seg$primary]]
  dup[[seg$primary]] <- FALSE
  # DESIGN: THE VOID RULE HAS TWO HALVES, and they are not the same rule. Per CELL (format()'s job) a
  # void aside renders BLANK AND KEEPS ITS WIDTH, so a total row missing its difference interval
  # still lines up with the rows that have one. Per COLUMN (here) a field void EVERYWHERE is not
  # padding worth keeping: its whole bracket group leaves the template, and a template left with one
  # bare token collapses onto that token.
  tmpl2 <- display_prune_template(seg, empty | dup)
  f2    <- parse_display_template(tmpl2)$fields
  if (!length(f2)) return(list(col = col, missing = seg$fields[empty]))
  # DESIGN: a ONE-FIELD "composite" must render as the pipeline's own bare token -- the composite
  # renderer calls format(special_formatting = FALSE), dropping the odds ratio's 1/x form and its
  # reference-cell annotation. DISPLAY_BARE_TOKENS only: the other fields have no simple renderer.
  bare <- if (length(f2) == 1L && identical(tmpl2, paste0("{", f2, "}")) &&
              f2 %in% DISPLAY_BARE_TOKENS) f2 else tmpl2
  # THE STAMPING IS GATED ON "DOES THIS TEMPLATE RENDER ANYTHING HERE", not on the primary: a cell
  # whose primary is void but whose asides are not still belongs to the column's layout -- a numeric
  # predictor's OBSERVED cell has an odds ratio and no risk difference, and gating on the primary
  # left it showing a bare `est` while every other row showed `{diff} [{or}] ({base})`, i.e. two
  # different quantities in one column. A cell with NOTHING of the template keeps its own token,
  # which is the half that matters: never blank a cell that had something to say.
  # ⚠ ...AND NEITHER IS A TOKEN A CELL WAS DELIBERATELY GIVEN. Where the primary is void, a cell
  # keeps its OWN token if that token is not the column's ESTIMATE, i.e. it was stamped with
  # something else on purpose: a regression's baseline row holds the level its column's effects
  # operate on (EST_SCALES$const_display), a real number in another field, and a template gated on
  # the asides alone printed it "(51%)" -- an aside with nothing in front of it. The counter-example
  # above still takes the template, because there the cell's own token IS the estimate.
  own_tok  <- fmt_resolve_scale_tokens(display_primary(d), fmt_scale_row(col))
  keep_own <- !have[[seg$primary]] & !is.na(own_tok) &
    own_tok != fmt_resolve_scale_tokens("est", fmt_scale_row(col))[[1]] & !is.na(get_num(col))
  elig <- elig & purrr::reduce(have, `|`) & !keep_own
  d[elig] <- bare
  list(col = fmt_set_display(col, d), missing = seg$fields[empty])
}

# fmt_blank_fields() -- the helper rows / columns copy a real column and re-display it, so every
# field describing the ORIGINAL cell must go: keeping them would let a display switch, a colour
# measure or a tooltip read a number belonging to another quantity. `pct` is the only variable.
#' @keywords internal
#' @noRd
fmt_blank_fields <- function(col, pct = FALSE) {
  col <- set_diff(col, NA_real_) |> set_ci(NA_real_) |> set_mean(NA_real_)
  if (pct) col <- set_pct(col, NA_real_)
  # ⚠ EVERY comparison field, not only the additive ones: a base-count column that kept its source
  # cell's `ratio` and `or` had a hover reading "ratio: x1.00 ; ref" over a count.
  set_ctr(col, NA_real_) |> set_var(NA_real_) |> set_ratio(NA_real_) |> set_or(NA_real_)
}

# WHOSE level is this? A regression column states it in `role`, and that is what a LEVEL token says
# on one instead of a row/col axis it does not have: the OBSERVED (crude) quantity, or the ADJUSTED
# prediction the model makes. "" on a crosstab and on every helper column, where the crosstab reading
# stands -- which is also the fallback when the metadata is missing.
#' @keywords internal
#' @noRd
reg_role_qualifier <- function(x, sep = "") {
  # a split-off aside keeps what it was carved from ("aside:emp"), so it prints the same qualifier
  # its source does -- a crude column's level is an "obs%" whichever column of the pair it sits in.
  r <- sub("^aside:", "", tryCatch(get_role(x) %||% "", error = function(e) ""))
  if (!nzchar(r) || identical(r, "aside")) return("")
  # a RANK column's percentage is a probability of SUPERIORITY, not an adjusted prediction of a
  # category -- 50 % means "no difference" there, so the abbreviation must not read "adj%". Both
  # roles take it: the crude twin measures the same thing, and its header already says whose it is.
  fam <- tryCatch(fmt_attr(x, "model_family") %||% "", error = function(e) "")
  if (identical(REG_FAMILIES[[fam]]$level %||% "", "rank")) return(paste0(gettext("sup"), sep))
  if (identical(r, "emp"))   return(paste0(gettext("obs"), sep))
  if (identical(r, "model")) return(paste0(gettext("adj"), sep))
  ""
}

# =====================================================================================================
# DISPLAY_TOKENS -- THE per-token relation of the display grammar (a token names what a cell PRINTS).
#
# ⚠ ROW ORDER IS A CONTRACT. The `user` rows come first, in the order validate_display_template()'s
# "Valid fields" message and ?tab print them; the `bare` ones are the head of that run. Both derive by
# FILTERING, so the order is preserved by construction.
#
# `OR` is an ALIAS row, so `display = "OR"` and `"{OR}"` are the acronym spelling of the one token
# `or` -- resolved on READ by display_primary(), never stored. An acronym names a MEASURE elsewhere
# in the package (`color = `, `measure = `); here it names the FIELD of the same name, and `or` is the
# only token whose canonical name is itself an acronym, which is why it is the only such row.
#
# ⚠ WHY `settable` EXISTS: get_num() had arms set_num() lacked, and vec_arith goes through set_num(),
# so arithmetic on a column displaying `pvalue` / `or_pct` silently returned it UNCHANGED
# (`x * 2` == `x`, no warning). The stopifnot() at the tail of this file keeps the three in step.
#
# THE HOT PATH STAYS HAND-WRITTEN: get_num()/set_num() are vectorised mask writes and format() is
# ~15 rendering-class masks crossed with the column's `scale`. This table drives the VOCABULARIES.
#
# COLUMNS
#   field      the fmt field get_num() reads. NA = the token has none of its own: `resid` is DERIVED
#              (fmt_resid(), from pvalue + sign(ctr)) and `blank` prints nothing.
#   settable   set_num() writes the field back. FALSE only where there is nothing to write.
#   user       may be typed inside a {} template (and is named in the "Valid fields" message).
#   bare       a one-field template collapses onto this token, inheriting its own rendering.
#   value_cell display_write_col() may re-template a cell showing this. TRUE on every token that
#              carries a VALUE of the table; FALSE only on the four that are not one -- a p-value, a
#              model-fit statistic, the `n_min` blank, and the synthesised base count -- which keep
#              their own token whatever `display =` asks for.
#   footer     a footer STATISTIC, not data: it never carries a significance star, and a row whose
#              every cell is one is a regression's model-fit block (read black + bold, not greyed).
#   colour     may a cell showing this be coloured. `pvalue` is TRUE here while `footer` is also TRUE,
#              on purpose -- it is coloured as a significance warning (fmt_color_slots()). That one
#              disagreement is why this is two columns and not one "numberless".
#   geometry   which effect geometry the token NAMES, for the mismatch refusal. NA = it names none:
#              `ci` IS the bracket, and `ctr`/`var`/`resid`/`obs` are not estimates of a contrast.
#   comparison the colour MEASURE the token names, for the `color` -> `display` -> difference chain.
#   min_digits the fewest decimals this token is READABLE at, applied only when the cell asks for 0:
#              a ratio read against x1.2 / x1.5 thresholds is meaningless at "1", a standardized
#              residual against +/-2 / +/-3 unreadable as "-2". NA = the cell's own `digits` stands.
#              WARNING: it CANNOT be stored on the cell instead -- one `digits` serves every display
#              of that cell, and a percentage wanting 0 shares it with the ratio wanting 2.
#   source     the argument that would fill an empty field, for the void note. NA where it always
#              exists (pct / n / wn), which display_note_empty() drops.
#   unit       "pct" where the stored field is a proportion the cell prints x100 with a "%" -- the one
#              statement of it, read by format() instead of a hard-coded list of tokens.
#   self_named the RENDERED cell already carries the token's own name ("cv 35%"), so a header that
#              repeated it would say it twice: the export header drops such an aside, while the
#              console type tag keeps it (there the tag is the only thing naming the layout).
#   alias      this row is not a token but a legacy SPELLING of one, resolved by display_primary().
#   label      the token's SHORT name, the one word that says what the number is: the console type
#              tag, the exports' unit line and an Excel aside column's header all read it through
#              fmt_display_label(). A string, or a `function(x)` where the name depends on the column
#              (`pct` carries its direction, `n_range` collapses when no range renders, `var` is the
#              sd in a twin column, `mean` names its inline sd tail). NA = the token names nothing.
#   doc        what the token shows, one phrase, for the GENERATED ?fmt / ?tab sections
#              (display_tokens_rd()) -- user-facing documentation, written as such.
#
# The defaults below are the documentation: a row states only what is unusual about it.
#' @keywords internal
#' @noRd
.dtok <- function(field = NA_character_, settable = TRUE, user = FALSE, bare = FALSE,
                  value_cell = FALSE, footer = FALSE, colour = TRUE, geometry = NA_character_,
                  comparison = NA_character_, min_digits = NA_integer_, source = NA_character_,
                  alias = NA_character_, label = NA_character_, unit = NA_character_,
                  self_named = FALSE, doc = NA_character_)
  list(field = field, settable = settable, user = user, bare = bare, value_cell = value_cell,
       unit = unit, self_named = self_named,
       footer = footer, colour = colour, geometry = geometry, comparison = comparison,
       min_digits = min_digits, source = source, alias = alias, label = label, doc = doc)

#' @keywords internal
#' @noRd
DISPLAY_TOKENS <- list(
  # --- the ones a user may type, IN THE ORDER THEY ARE LISTED TO THEM ---------------------------
  pct     = .dtok("pct" , user = TRUE, bare = TRUE, value_cell = TRUE, geometry = "level",
                  # the ONE token that names its direction of reading, which is why the type tag has
                  # no pct-type of its own to add once a percentage is actually printed. On a
                  # REGRESSION column there is no row/col axis to name, and what the reader needs
                  # instead is WHOSE percentage it is (`role`): the observed one, or the adjusted
                  # prediction. Falls back to the crosstab reading wherever `role` says nothing.
                  label = function(x) {
                    q <- reg_role_qualifier(x)
                    if (nzchar(q)) return(paste0(q, "%"))
                    p <- get_pct_type(x)
                    if (identical(p, "none")) "%" else paste0(p, "%")
                  },
                  doc = 'the percentage'),
  n       = .dtok("n"   , user = TRUE, bare = TRUE, value_cell = TRUE, geometry = "level",
                  label = "n",
                  doc = 'the count'),
  wn      = .dtok("wn"  , user = TRUE, bare = TRUE, value_cell = TRUE, geometry = "level",
                  label = "wn",
                  doc = 'the weighted count'),
  mean    = .dtok("mean", user = TRUE, bare = TRUE, value_cell = TRUE, geometry = "level",
                  source = 'a numeric col_var',
                  # on a regression column, WHOSE mean it is (see `pct` above). The sd / cv tail is
                  # an ordinary aside token now, so the composite name builder appends it.
                  label = function(x) paste0(reg_role_qualifier(x, " "), "mean"),
                  doc = 'the mean'),
  # the two SCALE-RELATIVE tokens: they name a ROLE, and each column answers with the token it has
  # always rendered (EST_SCALES' `est_display` / `base_display`, resolved by
  # fmt_resolve_scale_tokens()). That is what makes one display template work on every family.
  est     = .dtok(         user = TRUE, bare = TRUE, value_cell = TRUE,
                  doc = paste('the estimate, whatever this column estimates --- an odds ratio, a',
                              'risk difference, a coefficient, a percentage. The one token that',
                              'means the same thing on every table')),
  base    = .dtok(         user = TRUE, bare = TRUE, value_cell = TRUE, geometry = "level",
                  source = 'a column that has a level beside its estimate',
                  doc = paste('the level the estimate sits on: the percentage, the mean or the',
                              'count. On a plain percentage table it is the same number as',
                              '`est`; beside a regression effect it is the adjusted prediction')),
  diff    = .dtok("diff" , user = TRUE, bare = TRUE, value_cell = TRUE, geometry = "difference",
                  comparison = "difference",
                  source = 'a `ref` to compare to, and pct = "row" / "col"',
                  label = "diff",
                  doc = 'the difference from the reference'),
  # 2 decimals like the odds ratio beside it: a ratio's information sits in the digits AFTER the
  # constant "1.", so at 1 decimal three distinct effects collapse onto one string.
  ratio   = .dtok("ratio", user = TRUE, bare = TRUE, value_cell = TRUE, geometry = "ratio",
                  comparison = "ratio",
                  min_digits = 2L,
                  source = 'a `ref` to compare to, and pct = "row" / "col"',
                  label = "ratio",
                  doc = 'the ratio to the reference (relative risk, or a ratio of means)'),
  ci      = .dtok("ci"   , user = TRUE, bare = TRUE, value_cell = TRUE,
                  source = 'ci = "ref"  (or ci = "cell" for each cell\'s own interval)',
                  label = "ci",
                  doc = 'the confidence interval of whatever the column compares, as `[low;high]`'),
  # the SAME field as `ci`, the other notation. Two forms, two tokens: a token names what a cell
  # PRINTS, so neither of them reads an option to decide which of the two it is.
  moe     = .dtok("ci"   , user = TRUE, bare = TRUE, value_cell = TRUE,
                  source = 'ci = "ref"  (or ci = "cell" for each cell\'s own interval)',
                  label = "moe",
                  doc = paste('the margin of error --- the same interval as `ci`, written as the',
                              'half-width `+/-x` around the estimate. Void where the column compares',
                              'a RATIO: a ratio\'s interval is symmetric on the LOG scale, so it has',
                              'no half-width')),
  or      = .dtok("or"   , user = TRUE, bare = TRUE, value_cell = TRUE, geometry = "ratio",
                  comparison = "odds_ratio",
                  min_digits = 2L,
                  source = 'pct = "row" / "col"  (an odds ratio needs a percentage base)',
                  label = "OR",
                  doc = 'the odds ratio'),
  ctr     = .dtok("ctr"  , user = TRUE, value_cell = TRUE,
                  source = 'test = TRUE  (the contributions come from the chi-squared)',
                  label = "ctr",
                  doc = "the cell's contribution to the chi-squared"),
  var     = .dtok("var"  , user = TRUE, value_cell = TRUE, source = 'a numeric col_var',
                  label = "var",
                  doc = 'the variance'),
  # DERIVED from `var`, like `resid` and `gap` from theirs: the sd is sqrt(variance) and nothing is
  # stored twice. Read-only -- writing one back would mean writing a variance.
  sd      = .dtok(         user = TRUE, settable = FALSE, value_cell = TRUE, geometry = "level",
                  source = 'a numeric col_var',
                  label = "sd",
                  doc = 'the standard deviation, in the variable\'s own unit'),
  # DERIVED too, from `var` AND `mean`: the spread as a share of the level, so two columns measured
  # in different units can be compared for how dispersed they are. Void where the mean is not
  # strictly positive -- a ratio to something at or below zero says nothing (see ?tab).
  cv      = .dtok(         user = TRUE, settable = FALSE, value_cell = TRUE,
                  source = 'a numeric col_var whose mean is positive',
                  label = "cv", unit = "pct", self_named = TRUE,
                  doc = paste('the coefficient of variation --- the standard deviation as a',
                              'percentage of the mean')),
  resid   = .dtok(          user = TRUE, settable = FALSE, value_cell = TRUE, min_digits = 1L,
                  source = 'test = TRUE  (the residual comes from the chi-squared)',
                  label = "resid",
                  doc = paste('the adjusted standardized residual -- whether the cell departs from',
                              'independence. Derived from the p-value and the sign of `ctr`, so it',
                              'is read-only')),
  obs     = .dtok("obs"  , user = TRUE, value_cell = TRUE,
                  source = 'tab_reg(empirical = TRUE)  (an observed effect to compare the model to)',
                  label = "obs",
                  doc = paste('the OBSERVED (crude) effect a modelled one is compared to.',
                              '`tab_reg()` tables only')),
  # DERIVED where the column is multiplicative: log(estimate) IS the coefficient the model fitted, so
  # nothing needs storing. Settable all the same -- the write mirrors the read through exp().
  # Its LABEL names the quantity rather than the artefact -- "log(OR)", never "coef" (fmt_coef_label).
  coef    = .dtok("diff"  , user = TRUE, value_cell = TRUE, min_digits = 2L,
                  source = 'a `tab_reg()` column (a crosstab estimates no coefficient)',
                  label = function(x) fmt_coef_label(x),
                  doc = paste('the estimate on the model\'s LINK scale --- the coefficient a linear',
                              'or log-link model fitted. The same number as `est` where the column',
                              'is already additive, its logarithm where the column shows a ratio')),
  # DERIVED, like `resid`: the gap IS fmt_adjustment_score(), the number color = "adjustment" grades,
  # so a printed gap and its shade cannot disagree. Read-only -- nothing to write a gap back into.
  gap     = .dtok(         user = TRUE, settable = FALSE, value_cell = TRUE,
                  source = 'tab_reg(empirical = TRUE)  (a model effect and its observed counterpart)',
                  label = "gap",
                  doc = paste('how far adjustment moved the effect: the gap between the modelled',
                              'estimate and its observed counterpart, on the estimate\'s own scale.',
                              'What `color = "adjustment"` grades --- readable in print and Excel,',
                              'not only in an html tooltip')),
  # --- the ones the PIPELINE writes; never user-typed --------------------------------------------
  pvalue  = .dtok("pvalue", footer = TRUE,           # footer, yet deliberately coloured
                  label = "p",
                  doc = "a test's p-value"),
  gof     = .dtok("diff"  , footer = TRUE, colour = FALSE,
                  label = "fit",
                  doc = 'a model-fit statistic (N, R2, AIC, BIC, dispersion)'),
  # the same cell, MARKED: a model check past the convention its REG_CHECKS row declares. A separate
  # token rather than a per-cell flag, because "what a cell shows" is exactly what `display` is for,
  # and it is the one thing the colour engine already dispatches on.
  gof_warn = .dtok("diff" , footer = TRUE,
                  label = "fit",
                  doc = 'a model-fit statistic past the threshold its check is read against'),
  # The base count as the reader needs it: ONE number when every column block of the table rests on
  # the same population, `min-max` when they differ (several col_vars losing different NAs, several
  # models). Both ends are ordinary fields -- `n` the smallest base, `tot_n` the largest -- written
  # by mat_base_n() at display time, so this token renders whatever it is given and reads no option.
  n_range = .dtok("n"     , settable = FALSE, colour = FALSE, geometry = "level",
                  # it IS the base count; it only says "range" where a range actually renders
                  label = function(x) {
                    hi <- get_tot_n(x); lo <- get_n(x)
                    if (any(!is.na(hi) & !is.na(lo) & hi != lo)) "n_range" else "n"
                  },
                  doc = 'the unweighted base: one count, or a `min-max` range over the table'),
  blank   = .dtok(          settable = FALSE, footer = TRUE, colour = FALSE,
                  label = "",
                  doc = 'nothing: a cell masked by `n_min`'),
  # --- legacy SPELLINGS, resolved to their token by display_primary() ----------------------------
  rr      = .dtok(          settable = FALSE, alias = "ratio",
                  doc = 'the legacy synonym of `ratio`, still accepted'),
  OR      = .dtok(          settable = FALSE, alias = "or",
                  doc = 'the acronym spelling of `or`, still accepted')
)

#' @keywords internal
#' @noRd
.dtok_chr <- function(field)
  vapply(DISPLAY_TOKENS, function(r) r[[field]] %||% NA_character_, character(1))
#' @keywords internal
#' @noRd
.dtok_lgl <- function(field) vapply(DISPLAY_TOKENS, function(r) isTRUE(r[[field]]), logical(1))
# a real token, as opposed to an alias row -- every set below is of tokens
.dtok_real <- is.na(.dtok_chr("alias"))
#' @keywords internal
#' @noRd
.dtok_which <- function(field) names(DISPLAY_TOKENS)[.dtok_real & .dtok_lgl(field)]
#' @keywords internal
#' @noRd
.dtok_map <- function(field) {m <- .dtok_chr(field); m[!is.na(m)]}

# --- the vocabularies, DERIVED (each keeps its own name, shape and order) ------------------------
#' @keywords internal
#' @noRd
DISPLAY_USER_FIELDS    <- .dtok_which("user")
#' @keywords internal
#' @noRd
DISPLAY_ALIASES        <- .dtok_map("alias")
#' @keywords internal
#' @noRd
DISPLAY_BARE_TOKENS    <- .dtok_which("bare")
#' @keywords internal
#' @noRd
DISPLAY_VALUE_CELLS    <- .dtok_which("value_cell")
#' @keywords internal
#' @noRd
DISPLAY_FOOTER_TOKENS  <- .dtok_which("footer")
#' @keywords internal
#' @noRd
DISPLAY_NO_COLOR       <- names(DISPLAY_TOKENS)[.dtok_real & !.dtok_lgl("colour")]
# THE model-fit tokens: a footer statistic living in the `diff` field. Derived, so the marked twin
# and any future sibling reach every reader (get_num/set_num, the padding, the tooltip) at once.
#' @keywords internal
#' @noRd
DISPLAY_GOF_TOKENS     <- names(DISPLAY_TOKENS)[
  .dtok_real & .dtok_lgl("footer") &
    vapply(DISPLAY_TOKENS, function(r) identical(r$field, "diff"), logical(1))]
#' @keywords internal
#' @noRd
DISPLAY_SETTABLE       <- .dtok_which("settable")
# The tokens whose stored field is a PROPORTION, printed x100 with a "%": one statement, read by
# format() instead of a hard-coded list of token names.
#' @keywords internal
#' @noRd
DISPLAY_PCT_TOKENS     <- names(DISPLAY_TOKENS)[
  vapply(DISPLAY_TOKENS, function(r) identical(r$unit %||% NA_character_, "pct"), logical(1))]
# The tokens whose rendered cell already says their own name ("cv 35%"): an export HEADER drops such
# an aside rather than repeating it, while the console type tag keeps it (there it is the only thing
# naming the layout).
#' @keywords internal
#' @noRd
DISPLAY_SELF_NAMED     <- names(DISPLAY_TOKENS)[
  vapply(DISPLAY_TOKENS, function(r) isTRUE(r$self_named), logical(1))]
#' @keywords internal
#' @noRd
DISPLAY_TOKEN_GEOMETRY <- .dtok_map("geometry")
# The tokens that render each FIELD. "Does the cell already show this quantity?" is a question about
# the field, not the token: `diff` and `coef` are one number written two ways, `ci` and `moe` one
# interval, `n` and `n_range` one count.
#' @keywords internal
#' @noRd
DISPLAY_FIELD_TOKENS <- {
  f <- .dtok_chr("field")
  f <- f[.dtok_real & !is.na(f)]
  split(unname(names(f)), unname(f))
}
#' @keywords internal
#' @noRd
DISPLAY_COMPARISON     <- .dtok_map("comparison")
# ... and its INVERSE: the token that renders each colour measure. A measure NAME is therefore a legal
# `display` value ("difference" -> "{diff}", "odds_ratio" -> "{or}"), which is what lets one word mean
# the same quantity in `color =` and in `display =` instead of one of them silently meaning a third
# thing. Derived from the same `comparison` column, so a new measure needs no second declaration.
# A token spelling that is ALSO a measure name ("ratio") is caught by DISPLAY_BARE_TOKENS first and
# resolves identically, so the two vocabularies cannot disagree.
#' @keywords internal
#' @noRd
DISPLAY_MEASURE_TOKENS <- {
  m <- DISPLAY_COMPARISON[names(DISPLAY_COMPARISON) %in% .dtok_which("bare")]
  stopifnot(!anyDuplicated(unname(m)))          # a measure must name ONE token, or it names none
  stats::setNames(names(m), unname(m))
}
#' @keywords internal
#' @noRd
DISPLAY_FIELD_SOURCE   <- .dtok_map("source")
#' @keywords internal
#' @noRd
DISPLAY_MIN_DIGITS     <- {
  m <- vapply(DISPLAY_TOKENS, function(r) as.integer(r$min_digits %||% NA_integer_), integer(1))
  m[!is.na(m)]
}
# The short names, kept as a LIST because a label may be a closure over the column (see the `label`
# gloss above). Read through display_token_label(), never subset directly.
#' @keywords internal
#' @noRd
DISPLAY_TOKEN_LABELS   <- {
  l <- lapply(DISPLAY_TOKENS, function(r) r$label)
  l[!vapply(l, function(v) is.null(v) || (is.character(v) && is.na(v)), logical(1))]
}


# =====================================================================================================
# DISPLAY_PRESETS -- the named cell LAYOUTS, one table read by BOTH producers.
#
# A preset is a name for a template, nothing more: `tab()` and `tab_reg()` resolve the same names to
# the same layouts, so a display learnt on a crosstab means the same thing on a regression. They are
# spelt with the scale-relative tokens, which is what makes them work on every family and on both a
# crude and a modelled column -- there is no per-family preset left.
#
# The WORD ORDER is the order in the cell: `est_base` prints "1/1.63 (31.5%)", `base_est` the reverse.
# They are MIRRORS of one reading -- the estimate is the subject, the level its aside -- because the
# ESTIMATE stays the primary token in both (the parenthesis says which is the aside; see
# parse_display_template() in R/fmt_class.R). That is what lets a crude column print "(31%) 1/1.69"
# beside a model column's "1/1.63 (31.5%)", with the two estimates adjacent and the stars on both.
# The other reading -- the LEVEL as the subject, graded by the effect -- is `base` / `base_ci`.
#
# `est_ci` is an ordinary composite like the rest: `{ci}` renders the interval on the column's OWN
# scale (inverted bounds on a ratio, blank where none was computed), so the estimate keeps the stars
# and the colour and the per-token padding lines the estimates up.
#
# A preset may hold ONE template, or one per column ROLE (`default` plus an override). That is where
# the crude/model MIRROR is declared -- `est_base` is the regression default, and its `emp` arm is
# what puts the two estimates side by side across the pair. Declaring it here rather than in a
# builder is what makes the mirror survive a post-hoc set_display() and reach every producer.
#
# COLUMNS: `template` (a string, or one per role), `alias` (this row is a legacy SPELLING of another
# preset -- no template, no doc, not listed), `doc` (the user-facing phrase display_presets_rd()
# emits into ?tab, so the documented list cannot drift from the shipped one).
#' @keywords internal
#' @noRd
DISPLAY_PRESETS <- tx_grid(tibble::tribble(
  ~key,              ~template,                                              ~alias,        ~doc,
  "est",             "{est}",                                                NA_character_, "the estimate alone",
  "est_ci",          "{est} {ci}",                                           NA_character_, "the estimate with its confidence interval",
  "est_base",        c(default = "{est} ({base})", emp = "({base}) {est}"),  NA_character_, "the estimate and, in parentheses, the level it sits on",
  # `est_base` with the level stated ONCE, by the observed column: the default where several
  # predictor subsets are compared, so the model columns sit side by side with nothing between them.
  "est_base_once",   c(default = "{est}", emp = "({base}) {est}"),           NA_character_, "the estimate alone --- the level is stated once, by the observed column beside it",
  "est_coef",        "{est} ({coef})",                                       NA_character_, "the estimate and, in parentheses, the model's own coefficient",
  "base_est_mdiff",  c(default = "{est} ({diff})", emp = "({base}) {est}"),  NA_character_, "the estimate and, in parentheses, the same comparison as a difference",
  "base_est_mratio", c(default = "{est} ({ratio})", emp = "({base}) {est}"), NA_character_, "the estimate and, in parentheses, the same comparison as a ratio",
  # the crude effect INSIDE the model cell (`empirical = "cell"`): the aside comes FIRST, like every
  # other observed-then-modelled layout here, so the two comparable numbers read left to right.
  "est_obs",         "({obs}) {est}",                                        NA_character_, "the estimate and, before it in parentheses, the observed (crude) effect it is compared to",
  "base_est",        "({base}) {est}",                                       NA_character_, "the level, then the estimate --- the mirror of `est_base`, which sets a crude and a modelled effect side by side",
  "base",            "{base}",                                               NA_character_, "the level alone: the percentage, the mean or the count",
  "base_ci",         "{base} {ci}",                                          NA_character_, "the level with its confidence interval",
  "base_moe",        "{base} {moe}",                                         NA_character_, "the level with its margin of error",
  "base_diff",       "{base} ({diff})",                                      NA_character_, "the level and, in parentheses, its difference to the reference",
  "base_ratio",      "{base} ({ratio})",                                     NA_character_, "the level and, in parentheses, its ratio to the reference",
  "base_or",         "{base} ({or})",                                        NA_character_, "the level and, in parentheses, its odds ratio",
  "or_base",         "{or} ({base})",                                        NA_character_, "the odds ratio and, in parentheses, the percentage it rests on",
  # the two numeric-column layouts. `mean` needs no preset: it is already a bare token.
  "mean_sd",         "{mean} (\u03c3{sd})",                                                                   NA_character_, "the mean and, in parentheses, its standard deviation",
  "mean_cv",         "{mean} (cv {cv})",                                     NA_character_, "the mean and, in parentheses, its coefficient of variation --- the spread as a percentage of the mean, comparable between columns measured in different units (the default where every mean is positive)",
  # legacy SPELLINGS: the 1.x `OR = "or_pct"` layout, the value the jamovi display ComboBox writes,
  # and the word spelt out. `est` stays canonical -- an alias resolves before anything is stored, so
  # no `display` field ever holds "estimate".
  "or_pct",          NA_character_,                                          "or_base",     NA_character_,
  "OR_pct",          NA_character_,                                          "or_base",     NA_character_,
  "estimate",        NA_character_,                                          "est",         NA_character_,
))

#' @keywords internal
#' @noRd
DISPLAY_PRESET_ALIASES <- {
  a <- vapply(DISPLAY_PRESETS, function(r) r$alias %||% NA_character_, character(1))
  a[!is.na(a)]
}

# THE display boundary, shared by tab(display =), tab_reg(display =) and set_display().
# Returns NULL for "leave every cell's own token alone" (the default, and the jamovi ComboBox's idle
# value), a preset's template, "{tok}" for a bare token name, or the validated template as typed.
# `role` is the column's own `role` attribute ("" on a crosstab, "emp" on a crude column): an unknown
# or absent role takes the `default` arm, so only a preset that declares an override ever branches.
#' @keywords internal
#' @noRd
display_resolve <- function(display, role = NULL) {
  if (is.null(display) || length(display) == 0L) return(NULL)
  d <- as.character(display)[[1]]
  if (is.na(d) || d %in% c("", "no", "auto")) return(NULL)
  if (d %in% names(DISPLAY_PRESET_ALIASES)) d <- unname(DISPLAY_PRESET_ALIASES[[d]])
  if (d %in% names(DISPLAY_PRESETS)) return(display_preset_arm(DISPLAY_PRESETS[[d]]$template, role))
  if (d %in% DISPLAY_BARE_TOKENS) return(paste0("{", d, "}"))
  # a colour MEASURE's own name reaches the token that renders it, so `color` and `display` share one
  # spelling for one quantity. After the bare-token test, so a word that is both stays the token.
  if (d %in% names(DISPLAY_MEASURE_TOKENS)) return(paste0("{", DISPLAY_MEASURE_TOKENS[[d]], "}"))
  validate_display_template(d)
}

# One preset entry -> the template this role gets.
#' @keywords internal
#' @noRd
display_preset_arm <- function(entry, role = NULL) {
  if (length(entry) == 1L) return(unname(entry[[1]]))
  role <- if (is.null(role) || !length(role)) "" else as.character(role)[[1]]
  unname(entry[[if (role %in% names(entry)) role else "default"]])
}


# --- the GENERATED help section -----------------------------------------------------------------
# Called from a roxygen `@eval` block, so the help cannot drift from the tokens the package has.
#   user_only = TRUE -> ?tabxplor-display: the twelve a user may type. FALSE -> ?fmt: every token
#   (its prose refers to the fmt fields glossed above it on that page, so it only works there).
#' @keywords internal
#' @noRd
display_tokens_rd <- function(user_only = TRUE) {
  esc <- function(s) {
    s <- gsub("%", "\\\\%", gsub("\\", "\\\\", s, fixed = TRUE))
    gsub("`([^`]+)`", "\\\\code{\\1}", s)
  }
  toks <- names(DISPLAY_TOKENS)
  toks <- if (user_only) intersect(toks, DISPLAY_USER_FIELDS) else toks
  # ?fmt already glosses each same-name field above (fmt_fields_rd): name it here, gloss it there.
  same_name <- if (user_only) character(0) else intersect(toks, fmt_field_names)
  toks      <- setdiff(toks, same_name)
  line <- function(tk) {
    r    <- DISPLAY_TOKENS[[tk]]
    doc  <- if (is.na(r$doc)) "" else paste0(" --- ", esc(r$doc))
    need <- if (!user_only || is.na(r$source)) "" else paste0(". Needs ", esc(r$source))
    paste0("  \\item \\code{", tk, "}", doc, need, ".")
  }
  c(if (user_only) "@section Display fields:" else "@section Every display token:",
    if (user_only)
      c("The fields a \\code{\\{\\}} template may name, and \\code{display} may name on their own.")
    else
      c("Generated from the package's own display table, so it cannot drift from what",
        "\\code{get_num()} reads. Each of",
        paste0(paste0("\\code{", same_name, "}", collapse = ", "), " shows the field of the same"),
        "name, described above. The rest are composed or derived by the pipeline itself, and",
        "the last few are not meant to be typed:"),
    "\\itemize{", vapply(toks, line, character(1)), "}")
}

# The named LAYOUTS, generated from the same table display_resolve() reads, so ?tab's list cannot go
# stale (the hand-written one had missed three presets). Alias rows are skipped: a legacy spelling is
# accepted, not taught.
#' @keywords internal
#' @noRd
display_presets_rd <- function() {
  esc  <- function(s) {
    s <- gsub("%", "\\\\%", gsub("\\", "\\\\", s, fixed = TRUE))
    gsub("`([^`]+)`", "\\\\code{\\1}", s)
  }
  brc  <- function(s) gsub("([{}])", "\\\\\\1", s)          # Rd needs \{ and \}
  # DESIGN: the PDF manual is LaTeX, which can set no sigma, so a template carrying one is written
  # twice -- the html reading keeping what the console actually prints. WHOLE, around the span:
  # \ifelse is a text tag and checkRd refuses it inside \code{}. sigma_sign (R/utils.R) stays the
  # one declaration of the glyph.
  span <- function(s) {
    if (!grepl(sigma_sign, s, fixed = TRUE)) return(paste0("\\code{", s, "}"))
    paste0("\\ifelse{latex}{\\code{", gsub(sigma_sign, "SD", s, fixed = TRUE), "}}",
           "{\\code{", s, "}}")
  }
  keep <- names(DISPLAY_PRESETS)[
    !names(DISPLAY_PRESETS) %in% names(DISPLAY_PRESET_ALIASES)]
  line <- function(nm) {
    r <- DISPLAY_PRESETS[[nm]]
    paste0("  \\item \\code{\"", nm, "\"} (", span(brc(display_preset_arm(r$template))), ")",
           if (is.na(r$doc)) "" else paste0(" --- ", esc(r$doc)), ".")
  }
  c("@section Display layouts:",
    "The named layouts \\code{display} accepts. They are spelt with the scale-relative",
    "\\code{\\{est\\}} / \\code{\\{base\\}} fields, so one name means the same thing on a crosstab",
    "and on a \\code{\\link{tab_reg}} table:",
    "\\itemize{", vapply(keep, line, character(1), USE.NAMES = FALSE), "}")
}


#' What a table cell shows: the display grammar
#'
#' @description
#' Every function that builds a table takes a `display` argument, and [set_display()] changes it
#' afterwards. This page is its vocabulary: the fields a cell may show, and the named layouts that
#' arrange them.
#'
#' Choosing a display never triggers a computation and never changes a number --- every field is
#' already stored in the cell (see [fmt]), so `set_display()` on a finished table gives exactly what
#' asking for it in the call would have.
#'
#' @details
#' Three ways to ask, from the shortest:
#' \itemize{
#'   \item a **named layout**: `display = "est_ci"`, `"base_ratio"`, `"mean_sd"`.
#'   \item a **single field**: `display = "ci"`, `"diff"`, `"n"`.
#'   \item a **`{}` template** of your own: `"{est} ({base})"`, `"{pct} [{n}]"`.
#' }
#'
#' In a template, the **primary** field is the first one written *outside* brackets --- so an aside
#' may come first, `"({base}) {est}"`, without ceasing to be an aside. The primary carries the
#' significance stars, it is what Excel writes and what `get_num()` returns, and it is the part the
#' colours paint.
#'
#' A field may carry **its own precision**, `"{est:3} ({base:1})"`, which beats every default ---
#' the only way to set an aside's decimals independently of the estimate's.
#'
#' `est` and `base` are **scale-relative**: `est` is whatever the column estimates (a percentage, a
#' mean difference, an odds ratio) and `base` the level it sits on. That is what lets one layout
#' name mean the same thing on a [tab()] crosstab and on a [tab_reg()] regression table.
#'
#' A field with nothing to show renders blank but keeps its width, so the column stays aligned; a
#' field empty in the whole column is dropped, and a note says which argument would have filled it.
#'
#' @eval display_tokens_rd(user_only = TRUE)
#' @eval display_presets_rd()
#'
#' @seealso [set_display()] and [get_display()] change or read it on a built table; [tab()] and
#'   [tab_reg()] set it in the call; [fmt] describes every field a cell stores, and
#'   [tabxplor-options] the session-wide defaults.
#' @name tabxplor-display
NULL


#' @keywords internal
#' @noRd
display_note_empty <- function(fields) {
  hints <- DISPLAY_FIELD_SOURCE[fields]
  hints <- hints[!is.na(hints)]
  cli::cli_inform(c(
    "i" = "{cli::qty(length(fields))}{.arg display} field{?s} {.val {fields}} {?is/are} empty here.",
    if (length(hints))
      stats::setNames(paste0("{.field ", names(hints), "} needs ", unname(hints), "."),
                      rep("i", length(hints)))
    else character(0)
  ))
}

# Refuse a `{ci}` bracket whose geometry differs from the estimate beside it: the stored interval is
# the one this column's comparison is tested on, so `x1.8 ([2;4]%)` -- a ratio over a percentage-POINT
# interval -- aborts instead of printing. The `scale` and the token's geometry make it a lookup.
#' @keywords internal
#' @noRd
display_refuse_mismatch <- function(col, fields, tmpl) {
  if (!"ci" %in% fields) return(invisible(NULL))
  est <- intersect(fields, names(DISPLAY_TOKEN_GEOMETRY))
  if (!length(est)) return(invisible(NULL))
  have <- EST_SCALES[[get_scale(col)]]$geometry
  if (is.null(have)) return(invisible(NULL))
  want <- unname(DISPLAY_TOKEN_GEOMETRY[est[1]])
  if (identical(have, want)) return(invisible(NULL))
  # WARNING: a LEVEL names no comparison, so it constrains the bracket not at all -- "48% [-3;+4]"
  # (a percentage beside the difference interval it was tested on) is tabxplor's flagship cell, and
  # `display = "base_ci"` is exactly that template. The class this refusal closes is TWO EFFECT
  # geometries disagreeing, never a level.
  if ("level" %in% c(have, want)) return(invisible(NULL))
  cli::cli_abort(c(
    "{.arg display} = {.val {tmpl}} prints a {.field {want}} beside a {.field {have}} interval.",
    "i" = paste0("Ask for the matching comparison ({.code color = \"",
                 # a colour measure IS the geometry, bar a LEVEL and any MEASURES cannot name.
                 if (identical(have, "level")) "no"
                 else if (have %in% names(MEASURES)) have else "difference",
                 "\"} / {.code ci = }), or drop the {.code {{ci}}} bracket.")
  ))
}


# tab_append_pctcol_rows() -- under pct = "col" the base-count / add_pct extras are ROWS: a re-displayed
# copy of EACH sub-table's total row, spliced in after its own source row. `transform` returns the
# row(s) to insert; `role` is the row_kind ("pct" / "n") stamped on their cells (NA = don't stamp).
# Slicing runs UNGROUPED -- slice() would index within each group, the total-row index is global.
# WARNING: the group column must NOT be relabelled -- the copy keeps its sub-table's `row_var` value
# so it stays inside that group; `transform` only relabels tab_get_vars()$row_var.
tab_append_pctcol_rows <- function(tab, transform, role = NA_character_) {
  gv   <- dplyr::group_vars(tab)
  flat <- dplyr::ungroup(tab)
  n0   <- nrow(flat)
  tot  <- is_totrow(flat) & !is_placeholder_var(tab_get_vars(flat)$row_var)
  if (!any(tot)) return(tab)
  gid  <- if (length(gv) > 0) dplyr::group_indices(tab) else rep(1L, n0)
  grps <- unique(gid[tot])
  # SOURCE = each sub-table's last total row; ANCHOR = the END of that sub-table. They differ once a
  # previous pass inserted an extra (add_pct runs first); anchoring on the end keeps the order.
  src    <- vapply(grps, function(g) { i <- which(tot & gid == g); i[[length(i)]] }, integer(1))
  anchor <- vapply(grps, function(g) { i <- which(gid == g);       i[[length(i)]] }, integer(1))
  ord    <- order(src)
  src    <- src[ord]; anchor <- anchor[ord]
  added  <- transform(dplyr::slice(flat, src))
  if (!is.na(role))
    added <- dplyr::mutate(added, dplyr::across(dplyr::where(is_fmt), ~ set_row_kind(., role)))
  out    <- dplyr::bind_rows(flat, added)
  # splice: bind_rows put the new rows at the very end, so re-order by "just after my sub-table".
  reord  <- order(c(seq_len(n0), anchor + 0.5))
  out    <- dplyr::slice(out, reord)
  if (length(gv) > 0) dplyr::group_by(out, dplyr::across(tidyselect::all_of(gv))) else out
}


# tab_base_n_pct() -- append the base-count column and/or the col%/row% companion (add_pct) to each
# built factor table (the tabs_text LIST, one entry per row_var); shared by tab_many() and
# tab_counts(). The `n` COLUMN is "xl"-only -- Excel wants a real, editable number and no composite
# cell -- while text folds the base into the Total cell instead (tab_fold_base_n). The pct = "col"
# rows are backend-invariant.
tab_base_n_pct <- function(tabs_text, base_n, add_pct, backend = "xl") {
  add_n <- !identical(base_n, "no")
  if (!add_n && !add_pct) return(tabs_text)

    # cols, with pct = "row"
    last_totcols_pct_rows <- tabs_text |>
      purrr::imap_chr(
        ~ dplyr::last(names(.x)[is_totcol(.x) & get_pct_type(.x) == "row" &
                                  is_real_col_var(get_col_var(.x)) &
                                  !is_placeholder_var(tab_get_vars(.)$row_var)]) |>
          purrr::set_names(.y)
      )

    last_totcols_pct_rows <- last_totcols_pct_rows[!is.na(last_totcols_pct_rows)]

    if (length(last_totcols_pct_rows) > 0) {
      if (add_pct) {
        tabs_text <- tabs_text |>
          purrr::map2(
            last_totcols_pct_rows,
            ~ dplyr::mutate(
              .x,
              col_pct := dplyr::mutate(
                !!rlang::sym(.y),
                pct = get_wn(!!rlang::sym(.y)) /
                  dplyr::last(get_wn(!!rlang::sym(.y)),
                  )
              ) |>
                set_scale("level_pct") |> set_pct_type("col") |>
                as_totcol(FALSE) |> set_color("no") |>
                # a whole-table helper: no col_var, and its `role` says what it is
                set_col_var("") |> set_role("pct") |>
                fmt_blank_fields(pct = FALSE)
            )
          )
      }

      if (add_n && !identical(backend, "text")) {
        tabs_text <- tabs_text |>
          purrr::map2(last_totcols_pct_rows, function(tb, nm) {
            # WARNING: `[[<-`, not mutate() -- the table may be GROUPED, and mutate() would try to
            # recycle this whole-table column inside each group.
            tb[["n"]] <- fmt_base_n_cell(tb[[nm]], tb, base_n) |>
              set_display("n_range") |>
              set_count_col() |> as_totcol(FALSE) |> set_color("no") |>
              set_col_var("") |> set_role("n") |>
              fmt_blank_fields(pct = TRUE)
            tb
          })
      }

    }


    # rows, with pct = "col"
    last_totrow <- tabs_text |>
      purrr::map_int(
        ~ dplyr::last(which(is_totrow(.) & !is_placeholder_var(tab_get_vars(.)$row_var)),
                      default = NA_integer_)
      )
    last_totrow <- last_totrow[!is.na(last_totrow)]
    if (length(last_totrow) > 0) {


      last_totrow_pct_cols <- tabs_text |>
        purrr::map(~ names(.)[get_pct_type(.) == "col" & is_real_col_var(get_col_var(.)) &
                                 names(.) != "col_pct"] )
      last_totrow_pct_cols_no_empty <- purrr::map_lgl(last_totrow_pct_cols, ~ length(.) > 0)


      if (any(last_totrow_pct_cols_no_empty)) {

        if (add_pct) {
          tabs_text <-
            purrr::pmap(
              list(tabs_text, last_totrow_pct_cols_no_empty, last_totrow_pct_cols),
              ~ {
                totcols_ref <- purrr::map_chr(detect_totcols(..1), as.character)
                val_cols    <- ..3
                row_lab     <- tab_get_vars(..1)$row_var
                if (..2) {
                  tab_append_pctcol_rows(..1, function(src) {
                    src |>
                      dplyr::mutate(
                        dplyr::across(
                          where(is_fmt),
                          ~ dplyr::mutate(
                            .,
                            pct = get_wn(.) /
                              get_wn(rlang::eval_tidy(
                                rlang::sym(totcols_ref[[dplyr::cur_column()]])
                              ))
                          )
                        ),
                        dplyr::across(where(is_fmt),
                                      ~ fmt_blank_fields(as_totrow(., FALSE), pct = FALSE)),
                        dplyr::across(
                          where(is_fmt) & -tidyselect::all_of(val_cols),
                          ~ set_num(., value = NA_real_)
                        ),
                        dplyr::across(
                          all_of(row_lab),
                          # WARNING: a declared index column must keep its declaration AND its type
                          ~ lvl_add_label(., "row_pct")
                        )
                      )
                  }, role = "pct")
                } else {
                  ..1
                }
              }
            )
        }

        if (add_n) {
          tabs_text <-
            purrr::pmap(list(tabs_text, last_totrow_pct_cols_no_empty, last_totrow_pct_cols),
                        ~ {
                          val_cols <- ..3
                          row_lab  <- tab_get_vars(..1)$row_var
                          if (..2) {
                            tab_append_pctcol_rows(..1, function(src) {
                              src |> set_display("n") |>
                                dplyr::mutate(
                                  dplyr::across(where(is_fmt),
                                                ~ fmt_blank_fields(as_totrow(., FALSE), pct = TRUE)),
                                  dplyr::across(
                                    where(is_fmt) & -tidyselect::all_of(val_cols),
                                    ~ set_num(., value = NA_real_)
                                  ),
                                  dplyr::across(
                                    all_of(row_lab),
                                    # WARNING: a declared index column must keep its declaration AND its type
                                    ~ lvl_add_label(., "n")
                                  )
                                )
                            }, role = "n")
                          } else {
                            ..1
                          }
                        }
            )
        }

      }

    }


  tabs_text
}


# tab_row_totcols() -- the row-% total columns a base count can be folded into: one per col_group,
# so a spread table answers per sub-population instead of letting the last one speak for all.
#' @keywords internal
#' @noRd
tab_row_totcols <- function(tab) {
  if (!is.data.frame(tab)) return(character(0))
  names(tab)[purrr::map_lgl(tab, ~ is_fmt(.) && is_totcol(.) && get_pct_type(.) == "row" &&
                              is_real_col_var(get_col_var(.)))]
}

# tab_totcol_sums() -- does this Total column REALLY total the quantity its block is about? TWO
# gates, in this order, and both must pass:
#
#   1. THE BLOCK'S CELLS MUST SHOW A LEVEL. A "100 %" beside cells that print ratios or differences
#      totals nothing they show. Declared, never guessed: the primary token's own `geometry` where it
#      names one, else the column scale's `kind` -- so `{ci}` and `{est}`, which name no geometry of
#      their own, fall through to the scale exactly as before, and `ci = "cell"` keeps its "100 %".
#   2. THE BLOCK'S ESTIMATES MUST ADD UP TO IT, on the rows where every cell of the block has one, so
#      an n_min blank cannot make an honest total look dishonest. This is what catches
#      `levels = "first"`, where the other levels were dropped after the tests.
#
# ⚠ GATE 2 READS THE ESTIMATE, NOT get_num() -- what the column ESTIMATES (EST_SCALES' `est_field`,
# via fmt_est_of()), never what it happens to PRINT: reading the display there made `ci = "cell"` drop
# the "100%". And the pair is a CONJUNCTION on purpose: a `display =` change can REMOVE a "100 %" that
# the cells no longer show, and can never ADD one the estimates do not support.
# ⚠ ... AND IT ASKS ON THE LEVEL TWIN. "Do the parts add up to the whole" is a question about LEVELS
# and about nothing else. A reference interval stamps the INTERVAL's scale on every column of the
# block, the Total included (tab-leaf.R, `scale_1` is column-invariant) -- so under
# `color = "ratio"` gate 2 was summing `ratio`: 3.04 against the Total's 1, and the 100 % vanished
# from a table still printing percentages. A difference only ever passed because it happens to be
# additive (0 against 0). The level twin is declared (EST_SCALES$level_twin), so both read `pct`.
#' @keywords internal
#' @noRd
tab_totcol_sums <- function(tab, tot_nm) {
  col   <- tab[[tot_nm]]
  block <- names(tab)[purrr::map_lgl(tab, ~ is_fmt(.) && !is_totcol(.) && !fmt_is_helper_col(.) &&
                                       get_col_var(.)   == get_col_var(col) &&
                                       get_col_group(.) == get_col_group(col))]
  if (!length(block)) return(FALSE)
  if (!all(purrr::map_lgl(tab[block], fmt_shows_level))) return(FALSE)
  s   <- purrr::reduce(purrr::map(tab[block], fmt_est_of, "level"), `+`)
  tot <- fmt_est_of(col, "level")
  ok  <- !is.na(s) & !is.na(tot)
  if (!any(ok)) return(TRUE)                       # nothing to judge on: keep today's "100%"
  all(abs(s[ok] - tot[ok]) < 1e-6)
}

# Do this column's value cells print a LEVEL (a share, a mean, a count) rather than a deviation? The
# token decides where it names a geometry; where it names none (`ci`, and `est` before its scale
# answers) the column's scale does.
#' @keywords internal
#' @noRd
fmt_shows_level <- function(col) {
  scl  <- fmt_scale_row(col)
  toks <- unique(fmt_resolve_scale_tokens(display_primary(get_display(col)), scl))
  toks <- toks[!is.na(toks) & !toks %in% DISPLAY_FOOTER_TOKENS]
  if (!length(toks)) return(TRUE)
  geo  <- DISPLAY_TOKEN_GEOMETRY[toks]
  all(ifelse(is.na(geo), identical(scl$kind, "level"), geo == "level"))
}

# fmt_base_n_cell() -- put the base(s) of a cell's own block INTO the cell that reports them: the
# smallest in `n`, the largest in `tot_n`, so `{n_range}` prints one number when the whole block
# rests on the same people and `6 712-9 838` when it does not. `"min"` simply withholds the largest.
# WARNING: this writes fields, so it may only ever run on the EPHEMERAL materialised copy.
#' @keywords internal
#' @noRd
fmt_base_n_cell <- function(col, tab, mode) {
  rng <- tab_base_range(tab, tab_base_cols(tab, group = get_col_group(col)))
  set_tot_n(set_n(col, rng$min), if (identical(mode, "range")) rng$max else NA_real_)
}

# tab_fold_base_n() -- the base count folded into the Total cell, for the backends that have no room
# for a column of its own. The cell gets the SMALLEST base of its block in `n` and the largest in
# `tot_n`, so `{n_range}` prints one number when the whole block rests on the same people and
# `6 712-9 838` when it does not -- an unequal base can then never pass unnoticed. Where the block
# does not sum, the "100%" is simply a lie and only the count is printed.
# WARNING: this writes fields, so it may only ever run on the EPHEMERAL materialised copy.
# WARNING: runs BEFORE tab_pvalue_lines(), so the Total column has only data/total cells.
#' @keywords internal
#' @noRd
tab_fold_base_n <- function(tab, mode) {
  tab    <- dplyr::select(tab, -tidyselect::any_of("n"))
  tot_nm <- tab_row_totcols(tab)
  if (!length(tot_nm)) return(tab)
  for (nm in tot_nm) {
    col  <- fmt_base_n_cell(tab[[nm]], tab, mode)
    d    <- get_display(col)
    elig <- !is.na(get_num(set_display(col, "n_range")))
    d[elig] <- if (tab_totcol_sums(tab, nm)) "{pct} ({n_range})" else "({n_range})"
    tab[[nm]] <- set_display(col, d)
  }
  tab
}

# tab_drop_totcol() -- a row-% Total column holds two things: a constant (the 100 % its block sums
# to, when it sums at all) and the base count folded into it. Once the count lives elsewhere, only
# the constant is left. It lives elsewhere in three cases: Excel gives it a real column, `n = "no"`
# asks for none, and SEVERAL SUB-POPULATIONS give it one column per block (tab_base_n_cols) -- and
# there the whole set goes, because four "100 %" columns earn no width and invite reading across
# blocks that do not add up. In the first two cases only the columns that never summed are dropped,
# since an honest "100 %" still tells a reader the block is a distribution.
#' @keywords internal
#' @noRd
tab_drop_totcol <- function(tab, backend, base_n) {
  if (!is.data.frame(tab)) return(tab)
  tot_nm <- tab_row_totcols(tab)
  if (!length(tot_nm)) return(tab)
  drop <-
    if (tab_base_blocks(tab) > 1L)                                     tot_nm
    else if (identical(backend, "xl") || identical(base_n, "no"))
      tot_nm[!purrr::map_lgl(tot_nm, ~ tab_totcol_sums(tab, .))]
    else                                                               character(0)
  if (length(drop)) tab <- dplyr::select(tab, -tidyselect::all_of(drop))
  tab
}

# tab_base_blocks() -- how many SUB-POPULATIONS the value columns rest on: one on an ordinary
# table, one per spread level once a `tab_vars` axis went to column. The one test that decides
# whether the base count is folded into a Total cell or given a column of its own.
#' @keywords internal
#' @noRd
tab_base_blocks <- function(tab) {
  cols <- tab_base_cols(tab)
  if (!length(cols)) return(0L)
  length(unique(purrr::map_chr(tab[cols], get_col_group)))
}

# tab_base_n_cols() -- the base count as a COLUMN, one per sub-population, for the two tables that
# have no Total cell to fold it into: a regression (which has no Total column at all) and a spread
# crosstab (whose per-block Total columns are four constants). Synthesised from the `n` the value
# columns already carry -- a level's own count, a model's N on the Constant row. One group and the
# count belongs beside the levels it counts; several and the block goes to the RIGHT of the values,
# where the counts can be read against each other instead of pushing the estimates away.
#' @keywords internal
#' @noRd
tab_base_n_cols <- function(tab, mode) {
  vals <- tab_base_cols(tab)
  if (!length(vals)) return(tab)
  tab  <- dplyr::select(tab, -tidyselect::any_of("n"))
  vals <- intersect(vals, names(tab))
  grps <- unique(purrr::map_chr(tab[vals], get_col_group))
  for (g in grps) {
    cols <- vals[purrr::map_chr(tab[vals], get_col_group) == g]
    rng  <- tab_base_range(tab, cols)
    ref  <- tab[[cols[[1]]]]
    # `in_refrow` is ANDed across every column by tab_bold_rows(): a helper column that forgot it
    # would un-bold the reference row of the whole table.
    tab[[if (nzchar(g)) paste0("n_", g) else "n"]] <- fmt(
      n = as.integer(rng$min), tot_n = if (identical(mode, "range")) rng$max else NA_real_,
      display = "n_range", digits = 0L, scale = "level_n", color = "", color_signif = "ignore",
      col_var = "n", col_group = g, comp_all = FALSE, role = "n", in_refrow = is_refrow(ref),
      row_kind = get_row_kind(ref))
  }
  # one group = one count, and it belongs beside the values it counts; several = the block goes to
  # the right of them, where the counts can be read against each other.
  if (length(grps) == 1L) {
    idx <- which(!purrr::map_lgl(tab, is_fmt))
    if (length(idx)) tab <- dplyr::relocate(tab, tidyselect::all_of("n"), .after = max(idx))
  }
  tab
}


# fmt_cell_base() -- the population a cell rests on, whatever kind of column it sits in: the
# percentage base for a proportion, the count for a mean, and for a regression column its level's
# own n. THE one reader of that fact, shared by the n_min filter and by the base-count display.
#' @keywords internal
#' @noRd
fmt_cell_base <- function(col) {
  if (fmt_var_kind(col) == "mean" || get_role(col) %in% c("model", "emp")) get_n(col)
  else get_tot_n(col)
}

# tab_base_cols() -- the value columns whose base the reader is being told about: the row-oriented
# ones (a row / all-tabs percentage, or a mean) plus a regression's estimate columns. Totals and the
# display-time helper columns are excluded -- they REPORT the base, they are not a population.
#' @keywords internal
#' @noRd
tab_base_cols <- function(tab, group = NULL) {
  nms <- names(tab)[purrr::map_lgl(tab, is_fmt)]
  if (length(nms) == 0) return(character(0))
  keep <- purrr::map_lgl(tab[nms], function(col) {
    if (is_totcol(col) || fmt_is_helper_col(col)) return(FALSE)
    if (get_role(col) %in% c("model", "emp")) return(TRUE)
    get_pct_type(col) %in% c("row", "all", "all_tabs") || fmt_var_kind(col) == "mean"
  })
  nms <- nms[keep]
  if (is.null(group)) return(nms)
  nms[purrr::map_chr(tab[nms], get_col_group) == group]
}

# Does this table show PERCENTAGES? (any value column resting on a percentage base). The one test
# behind "row percentages that do not sum to 100 %", read by mat_base_n().
#' @keywords internal
#' @noRd
tab_is_pct <- function(tab) {
  cols <- tab_base_cols(tab)
  length(cols) > 0L &&
    any(purrr::map_chr(tab[cols], get_pct_type) %in% c("row", "col", "all", "all_tabs"))
}

# tab_base_range() -- per ROW, the smallest and the largest base among `cols`. They coincide whenever
# every block rests on the same population; they differ when col_vars lose different NAs, or when
# several models were fitted on different complete-case sets. A column with no base of its own is
# skipped (not treated as zero, and not as infinite: it simply says nothing about the population).
#' @keywords internal
#' @noRd
tab_base_range <- function(tab, cols) {
  if (length(cols) == 0) return(list(min = rep(NA_real_, nrow(tab)), max = rep(NA_real_, nrow(tab))))
  b <- purrr::map(tab[cols], ~ as.double(fmt_cell_base(.)))
  list(min = purrr::reduce(b, pmin, na.rm = TRUE), max = purrr::reduce(b, pmax, na.rm = TRUE))
}

# tab_base_notes() -- the per-block breakdown behind a `min-max` base: which column variable (or
# model) each end belongs to. Only the whole table knows it, so it travels to the tooltip as a note
# rather than through a field. NULL where every block rests on the same people -- there is then
# nothing to break down, and the cell already prints the one number.
#' @keywords internal
#' @noRd
tab_base_notes <- function(tab, col_name) {
  col <- tab[[col_name]]
  if (!is_fmt(col)) return(NULL)
  cols <- tab_base_cols(tab, group = get_col_group(col))
  if (length(cols) < 2L) return(NULL)
  blocks <- split(cols, purrr::map_chr(tab[cols], get_col_var))
  if (length(blocks) < 2L) return(NULL)
  per <- purrr::map(blocks, ~ tab_base_range(tab, .)$max)
  if (length(unique(purrr::map(per, ~ round(., 6)))) < 2L) return(NULL)
  # rendered through format(), so the thousands mark is the one every count in the table uses.
  out <- purrr::imap(per, ~ paste0(.y, ": ", format(fmt(n = as.integer(.x), display = "n",
                                                        digits = 0L))))
  txt <- do.call(paste, c(unname(out), list(sep = " ; ")))
  ifelse(is.na(get_n(col)), "", txt)
}

# tab_apply_n_min() -- the small-base display filter. A PURE DISPLAY helper: it recomputes NOTHING
# (no fields, no chi2/ANOVA, no CI), it just strips the noise of unreliable small-base cells.
# Rule: for row-oriented columns (a row / all-tabs percentage, or a mean) drop a row only if its
# LARGEST base across them is < n_min, then blank each surviving cell whose OWN base is < n_min; for
# col-oriented ones (pct = "col") drop the whole column when its base is < n_min. Orientation comes
# from stored facts, so mixed tables just work. Base = get_tot_n() for proportions, get_n() for
# means; an NA base is never weak. NEVER drops: total rows/tables, the total column, the base-count /
# add_pct helper rows and columns, or the p-value line.
tab_apply_n_min <- function(tab, n_min) {
  if (length(n_min) == 0 || is.na(n_min[1]) || n_min[1] <= 0) return(tab)
  n_min <- n_min[1]
  if (!is.data.frame(tab)) return(tab)

  fmt_names <- names(tab)[purrr::map_lgl(tab, is_fmt)]
  if (length(fmt_names) == 0) return(tab)

  base   <- purrr::map_chr(tab[fmt_names], get_pct_type)
  vkind  <- purrr::map_chr(tab[fmt_names], fmt_var_kind)
  row_like <- base %in% c("row", "all") | vkind == "mean"
  totcol <- purrr::map_lgl(tab[fmt_names], is_totcol)

  # WARNING: n_min keeps its OWN NA rule -- "an NA base is never weak", so it maxes over Inf, where
  # tab_base_range() skips an NA. The shared fact is fmt_cell_base(), not the reduce.
  cell_base <- fmt_cell_base

  # --- protected rows (never dropped) --------------------------------------------------------
  # n_min runs at build, on the CORE table: the helper rows and columns only exist at display time.
  fmt_all <- tab[fmt_names]
  totrow  <- purrr::reduce(purrr::map(fmt_all, is_totrow), `|`)
  tottab  <- purrr::reduce(purrr::map(fmt_all, is_tottab), `|`)
  protect <- totrow | tottab

  # --- row-drop + cell-blank on row-oriented columns -----------------------------------------
  row_cols <- fmt_names[row_like]                           # totcol INCLUDED in the max
  if (length(row_cols) > 0) {
    bases    <- purrr::map(tab[row_cols], ~ { b <- cell_base(.); b[is.na(b)] <- Inf; b })
    row_base <- purrr::reduce(bases, pmax)
    keep     <- protect | !(row_base < n_min)
    if (!all(keep)) {
      # a grouped_tab would split the length-n `keep` per group: ungroup, filter, restore.
      gv  <- dplyr::group_vars(tab)
      tab <- dplyr::ungroup(tab)
      tab <- dplyr::filter(tab, keep)
      if (length(gv) > 0) tab <- dplyr::group_by(tab, dplyr::across(tidyselect::all_of(gv)))
    }
  }
  blank_cols <- fmt_names[row_like & !totcol]
  blank_cols <- intersect(blank_cols, names(tab))
  if (length(blank_cols) > 0) {
    tab <- dplyr::mutate(tab, dplyr::across(
      tidyselect::all_of(blank_cols),
      ~ {
        b <- cell_base(.)
        w <- !is.na(b) & b < n_min
        if (any(w)) .[w] <- set_display(.[w], "blank")
        .
      }
    ))
  }

  # --- column-drop on col-oriented columns (pct = "col") -------------------------------------
  drop_cols <- fmt_names[base == "col" & !totcol]
  drop_cols <- intersect(drop_cols, names(tab))
  if (length(drop_cols) > 0) {
    weak <- purrr::map_lgl(tab[drop_cols], ~ {
      mb <- suppressWarnings(max(get_tot_n(.), na.rm = TRUE))
      is.finite(mb) && mb < n_min
    })
    if (any(weak)) tab <- dplyr::select(tab, -tidyselect::all_of(drop_cols[weak]))
  }

  tab
}


# =====================================================================================================
# BUILD-TIME EXHAUSTIVENESS -- the two switches and the table say the same thing, or the package does
# not install.
#
# ⚠ This block must sit HERE, not in R/fmt_class.R: get_num()/set_num() live there, DISPLAY_TOKENS
# lives here, and fmt_class.R sorts FIRST in R's C collation -- this is the first file where all
# three are in scope. Move it and the package stops installing.
#
# ⚠ SCOPE: get_num() and set_num() only. Both are pure per-token maps, so every length-1 character
# constant in their bodies IS a token, making the check TWO-directional (an undeclared arm and an
# unhandled row both fail). format() is excluded: its body is full of unrelated constants, so the
# same walk would assert nothing.
#' @keywords internal
#' @noRd
display_switch_tokens <- function(fn) {
  out <- character()
  walk <- function(e) {
    if (is.character(e) && length(e) == 1L) out <<- c(out, e)
    else if (is.call(e) || is.pairlist(e)) for (i in seq_along(e)) walk(e[[i]])
  }
  walk(body(fn))
  unique(out)
}

local({
  declared <- names(DISPLAY_TOKENS)[.dtok_real]
  # the scale-relative tokens are handled by the ONE resolver both maps run first, so it counts as
  # part of each of them -- otherwise `est` / `base` would read as unhandled.
  resolver <- display_switch_tokens(fmt_resolve_scale_tokens)
  read     <- c(display_switch_tokens(get_num), resolver)
  written  <- c(display_switch_tokens(set_num), resolver)
  # `n` is get_num()'s fall-through initialiser, so it never appears there as a literal.
  stopifnot(
    "every token get_num() reads must be declared in DISPLAY_TOKENS" =
      all(read %in% declared),
    "every declared token must be read by get_num()" =
      all(setdiff(declared, "n") %in% read),
    "every token set_num() writes must be declared in DISPLAY_TOKENS" =
      all(written %in% declared),
    "every token declared `settable` must have a set_num() arm" =
      all(DISPLAY_SETTABLE %in% written)
  )
})
