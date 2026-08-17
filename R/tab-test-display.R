# PURPOSE: The ONE shared framework that renders the `test` table attribute as a readable, aligned
#   summary table -- both the console block (a GFM pipe table printed above the tibble) and the inline
#   export rows. It unifies what used to be four ad-hoc renderers split by (crosstab vs reg) x (console
#   vs export): print_chi2 / print_reg_footer (console) and tab_pvalue_lines / reg_footer_lines (export).
# ROLE: Phase 16a. Three shared layers, each used by BOTH crosstab and regression:
#   1. CONTENT   -- test_display_rows() (which test), test_cell_label_weak() (label + min_e<5 flag),
#                   the formatters (test_fmt_*) and the fmt-cell builders (pvalue_line_fmt / reg_gof_cell
#                   / stat_line_fmt / reg_blank_cell) + reg_footer_spec().
#   2. CONSOLE   -- test_summary_grid() (tidy `test` + tab_render_vars / reg_meta -> a backend-free grid)
#                   and test_render_console() (grid -> GFM).
#   3. EXPORT    -- tab_append_footer() = the ONE fmt-frame append engine behind BOTH inline-row
#                   appenders (tab_pvalue_lines / reg_footer_lines live in R/tab_classes.R, next to their
#                   tab_materialize_extras orchestrator, and are now thin arm-specific configs over it).
# KEY CONSTRAINTS:
#   - Fast: the grid is rebuilt on every console print, so it is base-R indexing over the (small) test
#     tibble -- no tidyr, no per-cell dplyr.
#   - The crosstab-vs-reg arm keys off the STORED table kind (tab_is_reg(), R/table-spec.R).
#   - Weak chi2 test flag: min_e < 5 -> a trailing " !" on the p-value cell (standard validity caveat).
# See: CLAUDE.md Phase 16a; the `test` attribute is documented at R/tab_classes.R (new_test_tibble).

# === SECTION: shared formatters =====================================================================

# The chi2-validity threshold: a chi2 whose smallest expected cell count is below this is flagged weak.
test_weak_min_e <- 5

# A p-value as a percentage string, matching the historical reg-footer rule (tab_classes.R): "<0.01%"
# below 1e-4, else 3 significant figures. Element-wise (formatC() on a vector pads to a common width,
# which would inject stray spaces into the cells -- the renderer pads per column itself).
test_fmt_pvalue <- function(p) {
  vapply(p, function(pi)
    if (is.na(pi)) NA_character_
    else if (pi < 1e-4) "<0.01%"
    else paste0(trimws(formatC(pi * 100, format = "g", digits = 3)), "%"),   # "g" left-pads a scalar
    character(1))
}

# A test statistic value: adaptive precision (integers over 100, else 1-2 decimals), no thousands
# grouping (the convention for chi2 / F statistics). Vectorised.
test_fmt_stat_value <- function(v) {
  av <- abs(v)
  digits <- ifelse(is.na(v), 0L, ifelse(av >= 100, 0L, ifelse(av >= 10, 1L, 2L)))
  vapply(seq_along(v), function(i)
    if (is.na(v[i])) NA_character_ else formatC(v[i], format = "f", digits = digits[i]),
    character(1))
}

# The statistic cell with its degrees of freedom: "1911 (df 6)" (chi2, one df) / "127 (df 2; 2029)"
# (F, numerator; denominator). df2 rounded to a whole number (a Welch df is fractional). Vectorised.
test_fmt_stat <- function(statistic, df1, df2) {
  st <- test_fmt_stat_value(statistic)
  df_txt <- ifelse(is.na(df2),
                   paste0("(df ", formatC(df1, format = "d"), ")"),
                   paste0("(df ", formatC(df1, format = "d"), "; ",
                          formatC(round(df2), format = "d"), ")"))
  ifelse(is.na(statistic), NA_character_, paste0(st, " ", df_txt))
}

# A count / GOF number with thin-space thousands grouping (N, AIC, BIC), fixed decimals. Scalar.
test_fmt_num <- function(v, digits = 0L) {
  if (is.na(v)) return(NA_character_)
  prettyNum(formatC(v, format = "f", digits = as.integer(digits)), big.mark = " ")
}

# The single red-helper predicate: a p-value at or above 0.05 (a non-significant result). Vectorised.
test_is_nonsig <- function(p) !is.na(p) & p >= 0.05

# The short symbol for each effect-size measure (ASCII, so every backend renders it): Cramer's V,
# phi (2x2), eta^2 (numeric / ANOVA). Phase 18j.
test_es_symbol <- function(es_type)
  switch(es_type %||% "", "cramer_v" = "V", "phi" = "phi", "eta2" = "eta2", NA_character_)

# The console effect-size cell: "V = 0.18" / "eta2 = 0.05" (symbol prefix aids the learner; the export
# cell is the bare number, the column type already telling V from eta2). Vectorised; NA -> NA. Two
# decimals is the effect-size convention (values live in [0, 1]).
test_fmt_es <- function(effect_size, es_type) {
  vapply(seq_along(effect_size), function(i) {
    v <- effect_size[i]; sym <- test_es_symbol(es_type[i])
    if (is.na(v) || is.na(sym)) NA_character_
    else paste0(sym, " = ", formatC(v, format = "f", digits = 2L))
  }, character(1))
}

# The bare in-cell test label ("Chi2" / "Chi2 !" / "F, Welch"), shared by the console grid and the
# inline export p-value cell (pvalue_line_fmt wraps it in parens). A trailing " !" flags a weak chi2
# (smallest expected count < 5, the standard chi2-validity caveat); `min_e` is NA for an F test.
test_cell_label_weak <- function(test, min_e = NA_real_) {
  base <- test_cell_label(test)
  if (is.na(base)) return(NA_character_)
  if (!is.na(min_e) && min_e < test_weak_min_e) paste0(base, " !") else base
}

# The parenthesised label for the console p-value cell: "(Chi2 !)" / "(F, Welch)" / "" (unknown test).
test_pvalue_label <- function(test, min_e = NA_real_) {
  lbl <- test_cell_label_weak(test, min_e)
  if (is.na(lbl)) "" else paste0("(", lbl, ")")
}


# === SECTION: TEST_ROWS -- what kind of statistical row this is ====================================
# Phase 20c (KEY 5). THE vocabulary of the `test` attribute's discriminator column: one row per kind
# of test row, for BOTH producers. The `test` tibble is a 15-column union type carrying 39 kinds of
# row (new_test_tibble(), R/tab_classes.R); before this table only the regression half was declared
# (reg_footer_spec(), 31 of the 39) and the crosstab half lived as string literals in four consumers,
# the `compare_*` keys were paste0()-generated in one file and hand-enumerated in another, and the
# `interact_*` labels sat in a third literal map.
#
# THE RULE, the same one TAB_ARGS states one level out:
#   *** TEST_ROWS OWNS WHAT A ROW IS. THE PRODUCER OWNS WHAT IT COMPUTES. ***
# Nothing here decides a statistic; every column answers "how is this row selected, named, keyed or
# rendered". The arithmetic stays in chi2_compute_test() / reg_glance() / svy_omnibus_one() / the
# five checks.
#
# THE COLUMNS
#   producer    "tab" (a crosstab omnibus test) | "reg". Partitions every consumer.
#   kind        "gof" (a plain number, the `gof` display token) | "pvalue" | NA on a `line` row.
#               The row_kind vocabulary (R/row-model.R), checked in R/zzz-fact-keys.R.
#   digits      gof rows only. ⚠ DELIBERATELY absent on pvalue rows: only `kind == "gof"` ever reads
#               it (test_grid_reg(), reg_footer_lines()), and reg_footer_plan()'s `%||% 0L` then
#               gives every p-value row the same 0L instead of two values for one unread fact.
#   render      "grid" = a footer ROW (a cell per model column) | "line" = a table-wide footer
#               SENTENCE (a pooled test belongs to no single column).
#   noun        the label's subject, a BARE MSGID -- gettext at render, never at load (a top-level
#               gettext() freezes the build locale; REG_CHECKS' header states the same rule).
#   instrument  the label's parenthetical, a bare msgid or NA. The label IS
#               reg_check_label(noun, instrument) for every reg row -- ONE rule for all 34, which is
#               why 21 hand-written labels, a 3-arm switch and reg_check_spec_entries()' own paste
#               all collapse into it.
#   stat        WHICH `stats =` KEY REQUESTS THIS ROW (reg only). This is the many-to-one that makes
#               the user's vocabulary smaller than the storage's: linearity_lr/_f/_wald all carry
#               "linearity", the four compare_baseline* rows all carry "compare_baseline". NA on a
#               crosstab row -- an omnibus test is asked for with `tab(test = TRUE)`, not by name.
#   method      "lr" | "f" | "wald" | "aic" -- WHICH INSTRUMENT fired, as a key. Exactly one of a
#               `stat` block's rows is written per fit, and (stat, method) is unique, so
#               test_row_key()/test_row_types() replace the paste0() generation and the three
#               hand-written `types = c(wald=, f=, lr=)` maps.
#   design      TRUE = the survey-design variant of a crosstab test.
#   var_kind    "pct" | "mean" -- which column kind a crosstab row describes (EST_SCALES' vocabulary).
#   anova       "welch" | "classic" -- which options(tabxplor.anova) value SELECTS this row. Both are
#               computed and stored; the choice is pure display. NA = not an ANOVA F.
#   cell_label  the in-cell test label ("Chi2", "F, Welch"), crosstab only.
#   word        the same test's name inside the p-value ROW name ("Chi2", "Welch F", "ANOVA F").
#               Two columns because they genuinely differ: the cell is cramped, the row name is prose.
#
# ⚠ THE ORDER IS THE CONTRACT: rows 1-31 in this order ARE reg_footer_spec()'s display order, which
#   is also its fallback order. REG_GOF_KEYS is derived from it, so it took this order too (its only
#   reader is an "Available: ..." message; one table, one order, never a second order column).
# ⚠ WHAT IS NOT A ROW: Fisher's exact p and the weak-chi2 " !" flag are CONDITIONS ON the chi2 row
#   (`pvalue_exact` and `min_e` columns of it), which is what keeps the tidy shape and the row count
#   stable. They stay in test_cell_label_weak() / test_pvalue_descriptor()'s bodies.
# ⚠ THE SCHEMA STAYS DECLARED IN new_test_tibble(): test_group_cols() reads every UNdeclared column
#   as a grouping variable. This table declares the ROWS; that one declares the COLUMNS.

# The five literal blocks. The sixth (model checks) is GENERATED from REG_CHECKS below, because that
# table owns facts this one must not (`families`, `weighted_ok`, `panel`, and the two taught-but-
# never-scored checks that have a panel and NO row here at all).
#
# `block` = WHICH PRODUCER WRITES THIS ROW, stamped per block rather than per row. It is the column
# that makes the union type readable ("who emits a `dispersion` row?") and it is what REG_GOF_KEYS is
# derived from: `stat` cannot serve, because a single-instrument row's `stat` IS its own name
# (`dispersion`, `compare_baseline`), so "the rows reg_glance() emits" is not expressible in it.
# `producer` follows from it (only `omnibus` is a crosstab block) and is stamped with it -- one
# encoding, one convenience name, tied by an assertion at the file tail.
#' @keywords internal
#' @noRd
TEST_ROWS <- local({
  # ⚠ every member is NA when unset, never NULL: the defaulting below is utils::modifyList(), which
  # REMOVES an entry whose value is NULL rather than setting it.
  reg_gof <- function(noun, digits, stat, instrument = NA_character_, method = NA_character_)
    list(producer = "reg", kind = "gof", digits = as.integer(digits), render = "grid",
         noun = noun, instrument = instrument, stat = stat, method = method)
  reg_p <- function(noun, stat, instrument = NA_character_, method = NA_character_)
    list(producer = "reg", kind = "pvalue", render = "grid",
         noun = noun, instrument = instrument, stat = stat, method = method)
  reg_line <- function(instrument, stat, method)
    list(producer = "reg", kind = NA_character_, render = "line",
         noun = NA_character_, instrument = instrument, stat = stat, method = method)
  tab_p <- function(var_kind, cell_label, word, design = FALSE, anova = NA_character_)
    list(producer = "tab", kind = "pvalue", render = "grid", noun = NA_character_,
         instrument = NA_character_, stat = NA_character_, method = NA_character_,
         design = design, var_kind = var_kind, anova = anova,
         cell_label = cell_label, word = word)

  blocks <- list(
    # --- goodness of fit, one per model column (reg_glance) --------------------------------------
    glance = list(
      n             = reg_gof("N",              0L, stat = "n"),
      lr_null       = reg_p  ("LR vs null",         stat = "lr_null"),
      wald_null     = reg_p  ("Wald vs null",       stat = "wald_null"),
      f_model       = reg_p  ("F",                  stat = "f_model"),
      r2            = reg_gof("R2",             3L, stat = "r2"),
      r2_adj        = reg_gof("Adjusted R2",    3L, stat = "r2_adj"),
      mcfadden_r2   = reg_gof("McFadden R2",    3L, stat = "mcfadden_r2"),
      nagelkerke_r2 = reg_gof("Nagelkerke R2",  3L, stat = "nagelkerke_r2"),
      cox_snell_r2  = reg_gof("Cox-Snell R2",   3L, stat = "cox_snell_r2"),
      sigma         = reg_gof("Residual SD",    2L, stat = "sigma"),
      aic           = reg_gof("AIC",            0L, stat = "aic"),
      bic           = reg_gof("BIC",            0L, stat = "bic"),
      # `phi` is the EXACT Pearson dispersion; `dispersion` names the CHECK (max robust/model SE).
      phi           = reg_gof("Pearson dispersion", 2L, stat = "phi", instrument = "phi")),
    # --- model comparison ------------------------------------------------------------------------
    # Four instruments x two modes. The USER key is the `stat` (`compare_baseline` /
    # `compare_sequential`); which of the four rows is written is the model's business, and
    # reg_compare_rows() looks it up through test_row_key(stat, method).
    compare = list(
      compare_baseline      = reg_p("LR vs baseline",   stat = "compare_baseline",   method = "lr"),
      compare_baseline_f    = reg_p("F vs baseline",    stat = "compare_baseline",   method = "f"),
      compare_baseline_wald = reg_p("Wald vs baseline", stat = "compare_baseline",   method = "wald"),
      compare_baseline_aic  = reg_gof("Delta-AIC vs baseline", 0L,
                                      stat = "compare_baseline", method = "aic"),
      compare_seq           = reg_p("LR vs previous",   stat = "compare_sequential", method = "lr"),
      compare_seq_f         = reg_p("F vs previous",    stat = "compare_sequential", method = "f"),
      compare_seq_wald      = reg_p("Wald vs previous", stat = "compare_sequential", method = "wald"),
      compare_seq_aic       = reg_gof("Delta-AIC vs previous", 0L,
                                      stat = "compare_sequential", method = "aic")),
    # --- the per-predictor overall-association test ----------------------------------------------
    global = list(
      global_lr   = reg_p("Overall association", stat = "global", instrument = "LR",   method = "lr"),
      global_f    = reg_p("Overall association", stat = "global", instrument = "F",    method = "f"),
      global_wald = reg_p("Overall association", stat = "global", instrument = "Wald", method = "wald")),
    # --- the five model checks, GENERATED from REG_CHECKS ----------------------------------------
    check = test_rows_from_checks(),
    # --- the aggregated effect-modification test: a footer LINE, not rows -------------------------
    # Its `instrument` is the phrase reg_interaction_lines() prints ("a likelihood ratio test"), the
    # only label these rows have -- they carry no footer row, so `noun` is free.
    interaction = list(
      interact_lr   = reg_line("likelihood ratio", stat = "interaction", method = "lr"),
      interact_f    = reg_line("F test",           stat = "interaction", method = "f"),
      interact_wald = reg_line("Wald test",        stat = "interaction", method = "wald")),
    # --- the crosstab omnibus tests ---------------------------------------------------------------
    # ⚠ exactly one row per (var_kind x anova x design) -- asserted below. That invariant is what
    # lets a third ANOVA F be added as one row, with no code change anywhere.
    omnibus = list(
      chi2        = tab_p("pct",  "Chi2",             "Chi2"),
      chi2_design = tab_p("pct",  "Chi2, Rao-Scott",  "Chi2",    design = TRUE),
      F_welch     = tab_p("mean", "F, Welch",         "Welch F", anova = "welch"),
      F_classic   = tab_p("mean", "F",                "ANOVA F", anova = "classic"),
      # z16-iii: the flat and the full design run the SAME survey estimator (svyglm + regTermTest
      # Wald), so there is one design row, and its `word` must not claim a Welch F.
      F_design    = tab_p("mean", "F, survey", "F", design = TRUE))
  )
  # the members a block did not set, defaulted once rather than repeated on every row
  defaults <- list(digits = NA_integer_, design = FALSE, var_kind = NA_character_,
                   anova = NA_character_, cell_label = NA_character_, word = NA_character_,
                   stat = NA_character_, method = NA_character_)
  unlist(lapply(names(blocks), function(b)
    lapply(blocks[[b]], function(r) utils::modifyList(defaults, c(r, list(block = b))))),
    recursive = FALSE)
})

# The row readers. `.trow_chr` / `.trow_lgl` project one member over every row (the .dtok_* idiom of
# R/tab-display.R); `.trow_keys` is the workhorse -- the names of the rows a predicate keeps, IN
# DECLARATION ORDER, which is what makes every derived vector below order-stable.
#' @keywords internal
#' @noRd
.trow_chr <- function(member)
  vapply(TEST_ROWS, function(r) as.character(r[[member]])[[1]], character(1))
#' @keywords internal
#' @noRd
.trow_lgl <- function(member) vapply(TEST_ROWS, function(r) isTRUE(r[[member]]), logical(1))
# ⚠ which(), not [keep]: most members are NA on the rows they do not apply to, and `NA == "lr"` is
# NA -- which logical indexing turns into a phantom NA element rather than dropping the row. That is
# what made test_row_key() see two matches for a pair it declares unique.
#' @keywords internal
#' @noRd
.trow_keys <- function(keep) names(TEST_ROWS)[which(keep)]

# The derived key sets. Each replaces a hand-written literal; each keeps its old name so no call site
# moved (the DISPLAY_TOKENS precedent).
#' @keywords internal
#' @noRd
TEST_REG_KEYS      <- .trow_keys(.trow_chr("producer") == "reg")          # tab_kind()'s fallback
#' @keywords internal
#' @noRd
TEST_FOOTER_KEYS   <- .trow_keys(.trow_chr("producer") == "reg" &
                                   .trow_chr("render") == "grid")         # reg_footer_spec()
#' @keywords internal
#' @noRd
TEST_CROSSTAB_KEYS <- .trow_keys(.trow_chr("producer") == "tab")

# test_row_key(stat, method) -- THE (requesting key, instrument) -> discriminator lookup. It is total
# because (stat, method) is asserted unique below, which is what lets reg_compare_rows() stop
# paste0()-ing a key out of a `tag` and a suffix.
#' @keywords internal
#' @noRd
test_row_key <- function(stat, method) {
  k <- .trow_keys(.trow_chr("stat") == stat & .trow_chr("method") == method)
  if (length(k) != 1L)
    stop("tabxplor: no single TEST_ROWS row for stat '", stat, "', method '", method, "'.")
  k
}

# test_row_types(stat) -- the same block as the `c(wald = , f = , lr = )` map reg_term_tests() takes.
# Three literal copies (the interaction test, the global test, the linearity check) become this call.
#' @keywords internal
#' @noRd
test_row_types <- function(stat) {
  k <- .trow_keys(.trow_chr("stat") == stat & !is.na(.trow_chr("method")))
  stats::setNames(k, .trow_chr("method")[k])
}

# The label of any reg row: ONE rule, applied to `noun` + `instrument`, translated at render.
#' @keywords internal
#' @noRd
test_row_label <- function(key) {
  r <- TEST_ROWS[[key]]
  reg_check_label(r$noun, r$instrument)
}


# === SECTION: displayed-row selection, cell builders, reg footer spec ===============================
# (Phase 16a moved these here from R/tab_classes.R so all `test`-attribute display lives in one module.)

# tab_anova() -- WHICH one-way ANOVA F a table displays for its mean col_vars. Both F rows are
# computed and stored in the `test` attribute, so this is a pure DISPLAY choice: the table's own
# stated intent (`tab(anova =)` -> meta$render_extras$anova) if it has one, else the global option.
# Phase 19k: it exists so `anova` can stop travelling as a global set around a build -- the jamovi
# backend's last options()/on.exit dance, and a stale-cache hazard (the choice was baked into the
# tier-3 base key although the p-value line is materialised at DISPLAY).
#' @keywords internal
#' @noRd
tab_anova <- function(x) {
  a <- get_render_extras(x)[["anova"]]
  if (is.null(a) || !nzchar(a[[1]])) tx_option("anova") else a[[1]]
}

# Pick the DISPLAYED test row per (subtable x col_var): chi2 for factor col_vars, and for mean
# col_vars the chosen ANOVA F (Welch by default) -- see tab_anova(), which both callers pass.
# Phase 18j: a weak chi2 (min_e < 5) carries a `pvalue_exact` column = the Fisher-exact p on that
# same row; the p-value cell shows that reliable exact p (labelled "Fisher") instead of the flagged
# chi2 one. `pvalue_exact` is NA on a strong chi2 / on an older `test` attribute without the column.
test_display_rows <- function(test_tbl, anova = tx_option("anova")) {
  # Phase 18j: a design-based table carries chi2_design (factor) or F_design (numeric) INSTEAD of
  # the classic chi2 / F_welch|F_classic -- one family present per table, so filter on all of them.
  # (z16-iii: four discriminators became two, because the flat and the full design run the SAME
  # survey estimator; which one a table used is meta$inference$basis, not a second encoding here.)
  # Phase 20c: the four literals are TEST_ROWS' own selection rule -- every crosstab row whose
  # `anova` slot is unset (it is not an ANOVA F) or names the chosen one. Adding a third F is a row.
  keep <- test_crosstab_displayed(anova)
  disp <- dplyr::filter(test_tbl, .data$test %in% keep)
  if (is.null(disp[["pvalue_exact"]])) disp$pvalue_exact <- NA_real_
  disp
}

# The crosstab discriminators DISPLAYED under a given `anova` choice. Both F rows are computed and
# stored; picking one is pure display (tab_anova()).
#' @keywords internal
#' @noRd
test_crosstab_displayed <- function(anova = tx_option("anova")) {
  av <- .trow_chr("anova")
  .trow_keys(.trow_chr("producer") == "tab" & (is.na(av) | av == anova))
}

# Build the fmt "pvalue" cells for a p-value display row. Phase 17c: the p lives HONESTLY in the
# dedicated `pvalue` FIELD (was overloaded into pct/var, with a fake diff = -0.5 non-sig flag and a
# write-only col_var = "chi2_cols" marker). Its colour is now an explicit rule in fmt_color_slots()
# (a non-significant test -> deep-red warning), reading the real p -- so it fires under EVERY
# color_signif policy, not only the default (defect 5). Vectorised over p.
# Phase 12f: `label` (per col_var, e.g. "Chi2" / "F, Welch") turns the cell into the composite
# display "{pvalue} (<label>)" -- the in-cell test label that self-documents a mixed factor/mean row.
# NA / "" leaves the bare "pvalue" token. The label is a text-backend suffix only (Excel keeps the raw p).
pvalue_line_fmt <- function(p, label = NA_character_) {
  disp <- ifelse(is.na(label) | !nzchar(label), "pvalue", paste0("{pvalue} (", label, ")"))
  fmt(display = disp, scale = "level_n", n = NA_integer_, pvalue = p, digits = 2L)
}

# The label shown in a crosstab p-value cell for each test type (Phase 12f). NA -> no in-cell label.
# Phase 18j / z16-iii: the design-based variant names its method (Rao-Scott / svyglm Wald F).
# Phase 20c: the 5-arm switch is TEST_ROWS' `cell_label`. gettext() is applied uniformly now (it was
# on "F, survey" alone) -- a label is a msgid, and English is byte-identical because an untranslated
# msgid returns itself. The notation ("Chi2", "F") is simply never translated, which is a translator
# decision rather than a code one.
test_cell_label <- function(test) {
  r <- TEST_ROWS[[test]]
  if (is.null(r) || is.na(r$cell_label)) return(NA_character_)
  gettext(r$cell_label)
}

# Phase 18m: the p-value ROW NAME (was an in-cell suffix -- moved out of the cell so a
# mixed factor/numeric row no longer wastes width, and the table-level test type is stated ONCE). Names
# the test(s) used across the group's columns: factor side "Chi2" (or "Fisher" when the exact test ran),
# numeric side "Welch F" / "ANOVA F"; a single robust suffix "; survey-design" (Rao-Scott / svyglm --
# the same estimator whether the design came from a weight column or from svydesign(), z16-iii).
# Examples: "pvalue (Chi2)", "pvalue (Chi2, Welch F)", "pvalue (ANOVA F)",
# "pvalue (Chi2, Welch F; survey-design)".
# Phase 18w: the prose is translatable (gettext, ambient locale). Notation ("Chi2", "F") is kept;
# proper names ("Welch", "Fisher", "Rao-Scott") stay as-is. English is byte-identical.
# Phase 20c: the three literal `%in%` sets are TEST_ROWS' `var_kind` and `design`, and each test's
# name in the row is its declared `word`. That closed a live defect: the numeric arm read
# `if (any(num == "F_classic")) "ANOVA F" else "Welch F"`, and after the survey overlay a design
# table carries ONLY `F_design` -- so it printed "Welch F" for a test that is a svyglm + regTermTest
# Wald F. `F_design` declares `word = "F"`, and the "; survey-design" suffix already names it.
# ⚠ `tests` is always the DISPLAYED rows (test_display_rows() ran first, at both call sites), so at
# most one row of each var_kind reaches here -- which is why one `word` per side is the whole rule.
test_pvalue_descriptor <- function(tests, used_exact = FALSE, weak = FALSE) {
  tests <- unique(tests[!is.na(tests)])
  tests <- tests[tests %in% TEST_CROSSTAB_KEYS]
  vk    <- vapply(tests, function(t) TEST_ROWS[[t]]$var_kind, character(1))
  fac   <- tests[vk == "pct"]
  num   <- tests[vk == "mean"]
  parts <- character(0)
  # a weak chi2 (smallest expected count < 5) with no exact companion keeps a " !" validity caveat.
  # "Fisher" / "Chi2 !" are CONDITIONS ON the chi2 row (its `pvalue_exact` / `min_e` columns), never
  # rows of their own -- which is what keeps the tidy shape and the row count stable.
  if (length(fac)) parts <- c(parts, if (used_exact) gettext("Fisher") else if (weak) gettext("Chi2 !")
                                     else gettext(TEST_ROWS[[fac[[1]]]]$word))
  if (length(num)) parts <- c(parts, gettext(TEST_ROWS[[num[[1]]]]$word))
  if (!length(parts)) return(gettext("pvalue"))
  robust <- if (any(vapply(tests, function(t) isTRUE(TEST_ROWS[[t]]$design), logical(1))))
    gettext("; survey-design") else ""
  enc2utf8(gettextf("pvalue (%s%s)", paste(parts, collapse = ", "), robust))
}

# Phase 18m: the effect-size ROW NAME = the measure(s) present, so no separate "effect size" text is
# needed. Cramer's V (larger factor tables) / phi (2x2) / eta^2 (numeric ANOVA); mixed -> "Cramer's V, eta2".
test_es_measure <- function(es_types) {
  es_types <- unique(es_types[!is.na(es_types)])
  if (!length(es_types)) return(gettext("effect size"))
  # Phase 18w: "Cramer's V" translates ("V de Cramer"); phi/eta2 are notation, kept.
  lbl <- vapply(es_types, function(t)
    switch(t, "cramer_v" = gettext("Cram\u00e9r's V"), "phi" = "phi", "eta2" = "eta2", t), character(1))
  enc2utf8(paste(unique(lbl), collapse = ", "))
}

# --- Regression model-summary footer (Phase 12f) -----------------------------------------------------
# GOF stats travel in the whole-table `test` attribute with reg-specific discriminators (built by
# reg_gof_tibble() / reg_compare_rows() in R/tab_reg.R), DISJOINT from the crosstab "chi2"/"F_*" so the
# same `test` attribute drives both. One entry per footer stat: its row label + how the cell renders.
# kind "gof" -> a plain number (the "gof" display token reading `statistic`); kind "pvalue" -> a p-value
# cell. `digits` applies to gof cells. Order = TEST_ROWS' declaration order = the display order.
#
# Phase 20c (KEY 5): DERIVED from TEST_ROWS, contents and order intact, so no consumer moved. What
# went with the 21 hand-written entries: the `global` label switch, reg_check_spec_entries()' own
# paste, and the two-encodings pair between this list and reg_compare_rows()' paste0()-built keys.
# ⚠ IT MUST STAY A FUNCTION. Every label is gettext()'d HERE, at render, under the ambient locale;
# a top-level list would evaluate once at load and freeze with_legend_lang()'s LANGUAGE switch.
# Phase 18w: notation (N / F / R2 / AIC / BIC and the named pseudo-R2s) is its own translation;
# the "vs null / baseline / previous" prose translates. English is byte-identical.
reg_footer_spec <- function() {
  stats::setNames(lapply(TEST_FOOTER_KEYS, function(k) {
    r   <- TEST_ROWS[[k]]
    out <- list(label = test_row_label(k), kind = r$kind)
    # ⚠ the member is ABSENT on a pvalue row, never NA: reg_footer_plan()'s `s$digits %||% 0L` is
    # what turns that into the 0L every p-value row has always carried, and only `kind == "gof"`
    # ever reads it (test_grid_reg(), reg_footer_lines()).
    if (identical(r$kind, "gof")) out$digits <- r$digits
    out
  }), TEST_FOOTER_KEYS)
}
reg_footer_test_types <- function() TEST_FOOTER_KEYS
# Phase 18z8: the interaction discriminators are NOT in reg_footer_spec() (they render as a
# table-wide footer LINE, not as rows -- see reg_interaction_rows), but a table carrying only them
# (stats = FALSE) is still a reg table, so tab_kind()'s DEGRADED fallback must know them. Since 20c
# that fallback asks TEST_ROWS for every `producer == "reg"` key (TEST_REG_KEYS) and stops having to
# know which of them render as rows.
# Phase 19g (KEY 6): is_reg_footer() is GONE. "Is this a regression" is a stored fact
# (meta$spec$kind, R/table-spec.R) that every consumer reads through tab_is_reg(); sniffing the
# `test` tibble survives only inside tab_kind(), as the fallback for a table that lost its metadata.

# A `test` column read NA-safely as a character key: absent -> all "", NA -> "".
# WARNING: test by NAME, never `tt$<col>` -- a tibble WARNS ("Unknown or uninitialised column")
# before returning NULL, so the `$` form leaked a warning out of every degraded table.
#' @keywords internal
test_key_col <- function(tt, col) {
  if (!col %in% names(tt)) return(rep("", nrow(tt)))
  ifelse(is.na(tt[[col]]), "", as.character(tt[[col]]))
}

# Phase 19g (KEY 6): the SUB-POPULATION columns of a `test` tibble -- everything outside the fixed
# schema. They are named after the grouping variable in BOTH arms (a crosstab's tab_vars, a
# regression's tab_vars), which is what lets one rule read them: a row with no group column
# describes the whole table / whole model.
#' @keywords internal
test_group_cols <- function(tt) {
  if (is.null(tt)) return(character(0))
  nms <- setdiff(names(tt), names(new_test_tibble()))
  # WARNING: the two renderers add their own scratch keys (`.grp`, `.term`) to the slice they work
  # on, so a "not in the schema" rule alone would read one of them as a grouping variable and split
  # the footer into one block per predictor. Dot-prefixed names are render scratch, never data.
  nms[!startsWith(nms, ".")]
}

# THE regression-footer row plan: one row per (test, term), in spec order then term order, with its
# rendered label. Both row renderers read it -- the appended export rows (reg_footer_lines(), in
# R/tab_classes.R) and the console grid (test_grid_reg(), below) -- so a per-predictor check cannot
# render one way in the console and another in Excel.
#
# WARNING: it is built from the WHOLE `reg` slice, never per split group, so `K` (the block height) is
# constant across groups -- tab_append_footer() requires that, and a group missing one predictor must
# show a blank cell, not a shorter block.
#
# Phase 19g: the per-predictor key IS `var` -- one dimension, the same one a crosstab row uses.
# It could not be, while `row_var` meant the SPLIT-GROUP LEVEL on a reg row; the level rides a
# column named after the split variable now, exactly like a crosstab's tab_vars.
#' @keywords internal
reg_footer_plan <- function(reg) {
  spec <- reg_footer_spec()
  tm   <- test_key_col(reg, "var")
  keep <- reg$test %in% names(spec)
  if (!any(keep)) return(NULL)
  k <- unique(data.frame(test = reg$test[keep], term = tm[keep], stringsAsFactors = FALSE))
  k <- k[order(match(k$test, names(spec)), k$term), , drop = FALSE]
  sp <- spec[k$test]
  lab <- vapply(sp, `[[`, character(1), "label")
  k$label  <- ifelse(nzchar(k$term), paste0(lab, ": ", k$term), lab)
  k$kind   <- vapply(sp, `[[`, character(1), "kind")
  k$digits <- vapply(sp, function(s) as.integer(s$digits %||% 0L), integer(1))
  rownames(k) <- NULL
  k
}

# A single footer cell (one fmt value), for the appended export rows. gof -> the "gof" token (value in
# `diff`); pvalue -> the pvalue_line_fmt shape (no in-cell label: the reg row label already names the
# stat). A missing stat -> a "blank" cell (renders "").
reg_gof_cell   <- function(value, digits) fmt(display = "gof", scale = "level_n", n = NA_integer_,
                                              diff = value, digits = as.integer(digits))
reg_pvalue_cell <- function(p) pvalue_line_fmt(p)
reg_blank_cell  <- function() fmt(display = "blank", scale = "level_n", n = NA_integer_)

# The inline-export STATISTIC cell (the `tabxplor.test_lines = "stat"` row) -- a "gof" number carrying
# the test statistic with adaptive precision (integers over 100, else 1-2 decimals). The df is dropped
# in exports (the p-value row's "(Chi2)"/"(F, Welch)" label names the test; the console keeps the full
# "1911 (df 6)"). Vectorised; an NA statistic -> a blank cell.
stat_line_fmt <- function(statistic) {
  d <- ifelse(is.na(statistic), 0L,
              ifelse(abs(statistic) >= 100, 0L, ifelse(abs(statistic) >= 10, 1L, 2L)))
  cells <- lapply(seq_along(statistic), function(i)
    if (is.na(statistic[i])) reg_blank_cell()
    else fmt(display = "gof", scale = "level_n", n = NA_integer_, diff = statistic[i], digits = d[i]))
  do.call(vctrs::vec_c, cells)
}


# === SECTION: the display grid ======================================================================

# Build the backend-independent summary grid from a built table's `test` attribute, or NULL when there
# is nothing to show (no test attribute / all p-values NA / a degraded frame). One structure for both
# crosstabs and regressions:
#   list(label_headers = chr(L), stat_header = chr(1), value_headers = chr(V),
#        groups = list( list(label_lines = list(chr per label col, placed top-down),
#                            rows = list( list(label = chr(1), cells = chr(V), nonsig = lgl(V)) )) ))
test_summary_grid <- function(x) {
  test_tbl <- get_test(x)
  if (is.null(test_tbl) || nrow(test_tbl) == 0) return(NULL)
  if (tab_is_reg(x)) test_grid_reg(x, test_tbl) else test_grid_crosstab(x, test_tbl)
}

# --- crosstab arm: chi2 / ANOVA-F, one row-group per (`var` x tab_var level) ------------------------
test_grid_crosstab <- function(x, test_tbl) {
  disp <- test_display_rows(test_tbl, tab_anova(x))  # chi2 + the chosen F, one per (subtab, cv)
  disp <- disp[!is.na(disp$pvalue), , drop = FALSE]
  if (nrow(disp) == 0) return(NULL)

  rv <- tab_render_vars(x)
  # Phase 19n: a value column of this grid is a column BLOCK -- the (col_var, col_group) pair, not
  # the col_var alone. After a spread two blocks show the SAME variable for two sub-populations, so
  # keying on `col` alone would match both to one and emit a single p-value column for a table that
  # has two. The key is composed by fmt_col_block(); the header is its one-line label.
  disp$.key <- fmt_col_block(disp$col, test_key_col(disp, "col_group"))$key
  blocks <- if (isFALSE(rv$degrade)) tab_col_blocks(x) else NULL
  value_keys <- if (!is.null(blocks) && nrow(blocks)) intersect(blocks$key, unique(disp$.key))
                else                                  unique(disp$.key)
  value_keys <- value_keys[value_keys %in% disp$.key]
  if (length(value_keys) == 0) return(NULL)
  # the header of each block: the table's own label when it is known, else recompose from the test
  # row (a degraded table has no columns left to ask).
  value_cols <- if (!is.null(blocks) && nrow(blocks)) blocks$label[match(value_keys, blocks$key)]
                else fmt_col_block(disp$col, test_key_col(disp, "col_group"))$label[
                       match(value_keys, disp$.key)]

  # tab_vars present in the test tibble = comp = "tab" (a per-subtable column); their absence with
  # tab_vars on the table = comp = "all" (one whole-table p-value, the group named "<var> x tab_vars").
  tab_vars      <- if (isFALSE(rv$degrade)) rv$tab_vars else character(0)
  tabvars_in_tt <- intersect(tab_vars, names(disp))
  comp_all      <- length(tab_vars) > 0 && length(tabvars_in_tt) == 0

  # leading label columns + the key that splits row-groups (first-appearance order)
  key_cols   <- c("var", tabvars_in_tt)
  keys       <- unique(disp[key_cols])
  # header row: blank for `var` (its values ARE the variable names), the variable name for a tab_var
  label_headers <- c("", tabvars_in_tt)

  groups <- lapply(seq_len(nrow(keys)), function(g) {
    sel <- rep(TRUE, nrow(disp))
    for (kc in key_cols) sel <- sel & disp[[kc]] == keys[[kc]][g]
    sub <- disp[sel, , drop = FALSE]

    # the leading label cell(s): the row variable's name, then each tab_var level (or the collapsed
    # comp="all" label "<var> x tab1, tab2" in the single leading column)
    if (comp_all) {
      lab <- paste0(keys[["var"]][g], " \u00d7 ", paste(tab_vars, collapse = ", "))
      label_lines <- list(lab)
    } else {
      label_lines <- c(list(keys[["var"]][g]),
                       lapply(tabvars_in_tt, function(tc) as.character(keys[[tc]][g])))
    }

    # per value col: the source test row (there is exactly one displayed test per col_var here)
    idx  <- match(value_keys, sub$.key)
    n    <- vapply(sub$n[idx], test_fmt_num, character(1), digits = 0L)
    # effect size: columns may be absent on a degraded / older `test` attribute -> NA vector.
    es_v  <- if (!is.null(sub[["effect_size"]])) sub$effect_size[idx] else rep(NA_real_, length(idx))
    es_ty <- if (!is.null(sub[["es_type"]]))     sub$es_type[idx]     else rep(NA_character_, length(idx))
    es    <- test_fmt_es(es_v, es_ty)
    # a weak chi2 shows its Fisher-exact p in place of the flagged chi2 one; the test TYPE label now lives
    # in the row name (test_pvalue_descriptor), not the cell -- so the cell is the bare p-value.
    p_exact <- if (!is.null(sub[["pvalue_exact"]])) sub$pvalue_exact[idx] else rep(NA_real_, length(idx))
    p_show  <- ifelse(!is.na(p_exact), p_exact, sub$pvalue[idx])
    pval <- test_fmt_pvalue(p_show)
    pcell <- ifelse(is.na(pval), "", pval)
    nonsig <- test_is_nonsig(p_show)

    # Phase 18m: no "statistic" row (ambiguous once effect size shares the block); order = p-value then
    # effect size; the test type moves into the p-value row NAME, the measure into the effect-size row NAME.
    weak   <- any(!is.na(sub$min_e[idx]) & sub$min_e[idx] < test_weak_min_e & is.na(p_exact))
    p_lab  <- test_pvalue_descriptor(sub$test[idx[!is.na(idx)]], any(!is.na(p_exact)), weak)
    es_lab <- test_es_measure(es_ty[!is.na(idx)])
    rows <- c(
      list(list(label = "N",    cells = ifelse(is.na(n), "", n), nonsig = rep(FALSE, length(idx)))),
      list(list(label = p_lab,  cells = pcell,                   nonsig = nonsig)),
      if (any(!is.na(es)))
        list(list(label = es_lab, cells = ifelse(is.na(es), "", es), nonsig = rep(FALSE, length(idx))))
    )
    list(label_lines = label_lines, rows = rows)
  })

  list(label_headers = label_headers, stat_header = "Tests",
       value_headers = value_cols, groups = groups)
}

# --- reg arm: GOF footer, one row-group per split level (or a single group) -------------------------
test_grid_reg <- function(x, test_tbl) {
  spec <- reg_footer_spec()
  reg  <- test_tbl[test_tbl$test %in% names(spec), , drop = FALSE]
  if (nrow(reg) == 0) return(NULL)
  meta <- reg_call(x)

  # model columns (value cols) = the distinct fit columns, first-appearance order; headers = the
  # outcome names when their count matches, else the column key itself.
  # Phase 19l: the `"^Model .+ \\((.+)\\)$"` strip that stood here is DELETED. It matched an English
  # word plus a space, and NO producer has emitted that shape since Phase g -- the names are
  # "Model_OR" / "Model_OR [married]" (reg_column) and the col_var is reg_shared_col_var()'s
  # "<dep>: <positive_level>". So it always fell through and returned `cv` unchanged, which is what
  # the code does now, without pretending to parse.
  value_cols <- unique(reg$col)
  # Phase 19m-ii: the outcome each value column estimates, read off the ROW'S OWN KEY. 19m-i marked
  # the length coincidence this replaces as a MISSING JOIN KEY: `meta$outcome` enumerated the
  # OUTCOMES and `unique(reg$col)` the FITS, two different enumerations paired only when they happened
  # to be the same length.
  #
  # THE RULE: an outcome names a column only when it IDENTIFIES it -- one model per outcome. A model
  # COMPARISON gives every column the same outcome, so there the column key (the model label) is the
  # header. That is strictly better in the one case the coincidence got wrong: a single-model
  # comparison (1 dep, 1 col) used to be headed by the OUTCOME rather than by the model.
  dep_col <- if ("outcome" %in% names(reg)) reg$outcome else rep(NA_character_, nrow(reg))
  outcome_of  <- vapply(value_cols, function(cv) {
    d <- unique(dep_col[reg$col == cv])
    d <- d[!is.na(d) & nzchar(d)]
    if (length(d) == 1L) d else NA_character_
  }, character(1), USE.NAMES = FALSE)
  value_headers <- if (!anyNA(outcome_of) && !anyDuplicated(outcome_of)) outcome_of else value_cols

  # the ordered footer rows actually present: one per (stat, term), spec order then term order
  plan <- reg_footer_plan(reg)
  if (is.null(plan) || !nrow(plan)) return(NULL)
  reg$.term <- test_key_col(reg, "var")

  # split levels (the group key): Phase 19g -- the column NAMED after the split variable, read by
  # the same rule the crosstab arm uses for its tab_vars. "" (no split) -> one unnamed group.
  gc       <- test_group_cols(reg)
  rv_key   <- if (!length(gc)) rep("", nrow(reg)) else test_key_col(reg, gc[1])
  reg$.grp <- rv_key
  grp_lv   <- unique(rv_key)
  is_split <- any(nzchar(grp_lv))

  # shared-predictors column (outcome-vector / single model). A model COMPARISON has per-column
  # predictors that a row-dimension column cannot hold -> omit it (columns already name the models).
  show_preds <- !is.null(meta) && !isTRUE(meta$comparison) && length(meta$predictors) > 0

  label_headers <- c(if (is_split) "" else NULL, if (show_preds) "predictors" else NULL)

  n_rows <- nrow(plan)
  pred_lines <- if (show_preds) test_wrap_items(meta$predictors, n_rows) else NULL

  groups <- lapply(grp_lv, function(g) {
    sub <- reg[reg$.grp == g, , drop = FALSE]
    rows <- lapply(seq_len(n_rows), function(k) {
      pk <- plan[k, ]
      hit <- function(cv) sub[sub$col == cv & sub$test == pk$test & sub$.term == pk$term, ,
                              drop = FALSE]
      cells <- vapply(value_cols, function(cv) {
        r <- hit(cv)
        if (nrow(r) == 0) return("")
        if (identical(pk$kind, "gof")) test_fmt_num(r$statistic[1], pk$digits)
        else {
          p <- test_fmt_pvalue(r$pvalue[1]); if (is.na(p)) "" else p
        }
      }, character(1))
      nonsig <- vapply(value_cols, function(cv) {
        r <- hit(cv)
        identical(pk$kind, "pvalue") && nrow(r) > 0 && test_is_nonsig(r$pvalue[1])
      }, logical(1))
      list(label = pk$label, cells = cells, nonsig = nonsig)
    })
    label_lines <- c(if (is_split) list(g) else NULL, if (show_preds) list(pred_lines) else NULL)
    list(label_lines = label_lines, rows = rows)
  })

  list(label_headers = label_headers, stat_header = "Model fit",
       value_headers = value_headers, groups = groups)
}

# Greedily pack comma-separated items into at most `n_rows` lines of <= `width` chars, for the wrapped
# predictors column. Past 6 items, keep 6 (an ellipsis on the 6th) + a "+N vars" tail. If the packed
# lines still exceed n_rows, the overflow is merged onto the last line (never dropped silently).
test_wrap_items <- function(items, n_rows, width = 20L) {
  items <- as.character(items)
  extra <- 0L
  if (length(items) > 6L) {
    extra <- length(items) - 6L
    items <- c(items[1:5], paste0(items[6], "\u2026"))
  }
  tail_txt <- if (extra > 0L) paste0("+", extra, " vars") else NULL
  toks <- c(items, tail_txt)
  # greedy fill
  lines <- character(0); cur <- ""
  for (i in seq_along(toks)) {
    piece <- if (i < length(toks) || is.null(tail_txt)) paste0(toks[i], if (i < length(items)) "," else "")
             else toks[i]
    cand <- if (nzchar(cur)) paste(cur, piece) else piece
    if (nchar(cand) > width && nzchar(cur)) { lines <- c(lines, cur); cur <- piece }
    else cur <- cand
  }
  if (nzchar(cur)) lines <- c(lines, cur)
  if (length(lines) > n_rows && n_rows >= 1L) {
    keep <- lines[seq_len(n_rows - 1L)]
    lines <- c(keep, paste(lines[n_rows:length(lines)], collapse = " "))
  }
  lines
}


# === SECTION: console renderer (GFM pipe table) =====================================================

# Print the grid as a GFM markdown table above the tibble (Phase 16a, decision Q1 = GFM). Leading label
# columns + the stat column are left-aligned; value columns are right-aligned with an empty separator
# column between them (mirrors the md export col separator). A dashed row separates row-groups.
# Non-significant p-values (>= 0.05) are coloured red with cli, AFTER padding so ANSI never breaks the
# alignment. Returns invisibly; prints via cli.
test_render_console <- function(grid) {
  if (is.null(grid)) return(invisible(NULL))
  L <- length(grid$label_headers)
  V <- length(grid$value_headers)

  # 1. assemble the plain (uncoloured) text of every logical column, plus a parallel "red" mask over
  #    value cells. Columns in order: L label cols, 1 stat col, then V value cols.
  n_body <- 0L
  for (g in grid$groups) n_body <- n_body + length(g$rows)
  n_sep  <- length(grid$groups) - 1L                       # dashed separators between groups
  n_out  <- n_body + n_sep

  lab   <- matrix("", nrow = n_out, ncol = L)
  stat  <- character(n_out)
  val   <- matrix("", nrow = n_out, ncol = V)
  red   <- matrix(FALSE, nrow = n_out, ncol = V)
  dashr <- logical(n_out)                                   # which output rows are group separators

  r <- 0L
  for (gi in seq_along(grid$groups)) {
    g <- grid$groups[[gi]]
    for (ri in seq_along(g$rows)) {
      r <- r + 1L
      row <- g$rows[[ri]]
      for (cl in seq_len(L)) {
        lines <- g$label_lines[[cl]]
        lab[r, cl] <- if (!is.null(lines) && ri <= length(lines)) lines[[ri]] else ""
      }
      stat[r]   <- row$label
      val[r, ]  <- row$cells
      red[r, ]  <- row$nonsig
    }
    if (gi < length(grid$groups)) { r <- r + 1L; dashr[r] <- TRUE }
  }

  # 2. per-column widths on the PLAIN text (headers included)
  lab_w  <- vapply(seq_len(L), function(c) max(nchar(c(grid$label_headers[c], lab[, c]))), integer(1))
  stat_w <- max(nchar(c(grid$stat_header, stat)))
  val_w  <- vapply(seq_len(V), function(c) max(nchar(c(grid$value_headers[c], val[, c]))), integer(1))
  sep_w  <- 1L

  padl <- function(s, w) formatC(s, width = w, flag = "-")    # left-align
  padr <- function(s, w) formatC(s, width = w)                # right-align

  # 3. one GFM row from a set of already-padded cells
  emit <- function(cells) paste0("| ", paste(cells, collapse = " | "), " |")

  # value cols interleaved with a separator col between consecutive value cols
  interleave_val <- function(cells) {
    if (V == 0) return(character(0))
    out <- character(0)
    for (c in seq_len(V)) {
      out <- c(out, cells[c])
      if (c < V) out <- c(out, strrep(" ", sep_w))
    }
    out
  }

  # header
  header <- emit(c(vapply(seq_len(L), function(c) padl(grid$label_headers[c], lab_w[c]), character(1)),
                   padl(grid$stat_header, stat_w),
                   interleave_val(vapply(seq_len(V), function(c) padr(grid$value_headers[c], val_w[c]),
                                         character(1)))))
  # alignment row: label + stat left, value right, separator centred. Each marker FILLS the whole
  # "| x |" slot (content width + the 2 surrounding spaces) with no gaps, so the pipes still line up
  # with the header/data rows -- the clean `|:---|` form.
  markers <- c(vapply(seq_len(L), function(c) mk_align(lab_w[c], "left"), character(1)),
               mk_align(stat_w, "left"),
               local({
                 out <- character(0)
                 for (c in seq_len(V)) {
                   out <- c(out, mk_align(val_w[c], "right"))
                   if (c < V) out <- c(out, mk_align(sep_w, "center"))
                 }
                 out
               }))
  align <- paste0("|", paste(markers, collapse = "|"), "|")

  lines_out <- c(header, align)
  for (r in seq_len(n_out)) {
    if (dashr[r]) {
      lines_out <- c(lines_out, emit(c(
        vapply(lab_w, function(w) strrep("-", w), character(1)),
        strrep("-", stat_w),
        interleave_val(vapply(seq_len(V), function(c) strrep("-", val_w[c]), character(1))))))
      next
    }
    vcells <- vapply(seq_len(V), function(c) {
      cell <- padr(val[r, c], val_w[c])
      if (red[r, c]) cli::col_red(cell) else cell
    }, character(1))
    lines_out <- c(lines_out, emit(c(
      vapply(seq_len(L), function(c) padl(lab[r, c], lab_w[c]), character(1)),
      padl(stat[r], stat_w),
      interleave_val(vcells))))
  }

  cli::cat_line(lines_out)
  cli::cat_line()
  invisible(NULL)
}

# One GFM alignment marker filling a "| x |" slot: content width `w` + the 2 surrounding spaces. left
# ":---", right "---:", center ":--:", so the marker's pipes align with the padded header/data cells.
mk_align <- function(w, side) {
  W <- w + 2L
  switch(side,
         left   = paste0(":", strrep("-", W - 1L)),
         right  = paste0(strrep("-", W - 1L), ":"),
         center = paste0(":", strrep("-", W - 2L), ":"),
         strrep("-", W))
}

# a small null-coalescing helper (spec$digits may be absent for pvalue rows)
`%||%` <- function(a, b) if (is.null(a)) b else a


# === SECTION: inline export rows (the shared append engine) ==========================================
# tab_append_footer() is the ONE fmt-frame append behind BOTH the crosstab p-value rows
# (tab_pvalue_lines) and the regression GOF footer (reg_footer_lines): it inserts a block of K footer
# rows at the END of each group's rows, on plain field-vectors (no per-cell record reconstruction),
# then rebuilds each column once. The two callers differ only in HOW they build a footer cell / label
# and how rows map to groups -- passed in as `fmt_cell` / `nonfmt_val` / `group_of`.
#   group_of   : length-nrow(tabs) character, each existing row's group id (row order preserved).
#   K          : number of footer rows per group.
#   fmt_cell(nm, g)   : the K fmt cells for fmt column `nm` in group `g` (a length-K tabxplor_fmt).
#   nonfmt_val(nm, g) : the K footer strings for non-fmt column `nm` in group `g`.
#   attrs      : named list of table attributes threaded into new_tab() (subtext / vars / ...).
#   regroup    : character vector of columns to dplyr::group_by() on the rebuilt table.
#   footer_groups : the groups that actually GET a footer block (default all). A crosstab subtable with
#                   no computable test (e.g. a total-table group) is excluded, so it keeps its data rows
#                   with no p-value row appended.
tab_append_footer <- function(tabs, group_of, fmt_cell, nonfmt_val, attrs, regroup,
                              footer_groups = unique(group_of), row_role = NULL) {
  grp_lv  <- unique(group_of)
  fmt_nms <- names(tabs)[purrr::map_lgl(tabs, is_fmt)]

  # per fmt column: interleave [group field-frame, group footer-frame] over groups, stack once.
  # Phase 19f: `row_role(g)` is STAMPED on the footer cells' own `row_kind` field ("pvalue" / "gof" /
  # "blank"), so the appended rows say what they are and ride every later slice. 17c had to extend a
  # positional meta$vars$row_roles vector here, in the same group-interleaved order build_nonfmt
  # uses -- two orderings that had to be kept in step by hand.
  build_col <- function(nm) {
    meta   <- purrr::set_names(
      lapply(fmt_col_attrs, function(a) attr(tabs[[nm]], a, exact = TRUE)), fmt_col_attrs)
    frames <- unlist(lapply(grp_lv, function(g) {
      idx <- which(group_of == g)
      of  <- fmt_data_wn(tabs[[nm]][idx])
      if (!g %in% footer_groups) return(list(of))
      cell <- fmt_cell(nm, g)
      if (!is.null(row_role)) cell <- set_row_kind(cell, row_role(g))
      list(of, fmt_data_wn(cell))
    }), recursive = FALSE)
    fmt_stack_frames(frames, meta)
  }
  # non-fmt column: each group's original values then (for a footer group) its K footer strings.
  # WARNING: a declared index column (tabxplor_lvl) must be REBUILT with its declaration -- the bare
  # `factor()` below drops the class, and with it the column's role.
  build_nonfmt <- function(nm) {
    orig     <- tabs[[nm]]
    combined <- unlist(lapply(grp_lv, function(g)
      c(as.character(orig)[group_of == g],
        if (g %in% footer_groups) nonfmt_val(nm, g) else character(0))))
    if (is.factor(orig)) {
      lv  <- levels(orig)
      out <- factor(combined, levels = c(lv, setdiff(unique(combined), lv)))
      lvl_restore(out, orig)
    } else combined
  }

  out   <- purrr::set_names(lapply(names(tabs), function(nm)
    if (nm %in% fmt_nms) build_col(nm) else build_nonfmt(nm)), names(tabs))
  tabs2 <- tibble::new_tibble(out, nrow = length(out[[1]]))
  do.call(new_tab, c(list(tabs2), attrs)) |> dplyr::group_by(!!!rlang::syms(regroup))
}


# === SECTION: TEST_ROWS build-time checks + potools anchor ==========================================
# At the file TAIL, the DISPLAY_TOKENS layout: everything the assertions read (TEST_ROWS itself, its
# derived key sets, ROW_KINDS from R/row-model.R and REG_CHECKS from R/reg-assumptions.R -- both
# earlier in C collation) is in scope here and nowhere earlier.

stopifnot(exprs = {
  # S1 -- a row is named, once
  !is.null(names(TEST_ROWS))
  all(nzchar(names(TEST_ROWS)))
  !anyDuplicated(names(TEST_ROWS))

  # S2 -- the enums
  all(.trow_chr("producer") %in% c("tab", "reg"))
  all(.trow_chr("block") %in% c("glance", "compare", "global", "check", "interaction", "omnibus"))
  # `producer` is the convenience name of `block`: only the crosstab block is not a regression one
  identical(.trow_chr("producer") == "tab", .trow_chr("block") == "omnibus")
  all(.trow_chr("render")   %in% c("grid", "line"))
  all(is.na(.trow_chr("kind")) | .trow_chr("kind") %in% ROW_KINDS)
  all(is.na(.trow_chr("method")) | .trow_chr("method") %in% c("lr", "f", "wald", "aic"))
  all(is.na(.trow_chr("var_kind")) | .trow_chr("var_kind") %in% c("pct", "mean"))

  # S3 -- `digits` is the gof rows' fact, and ONLY theirs. A pvalue row must leave the member
  # ABSENT so reg_footer_plan()'s `%||% 0L` gives every one of them the same 0L (see the header).
  all(vapply(TEST_ROWS, function(r)
    !identical(r$kind, "gof") || !is.na(r$digits), logical(1)))
  all(vapply(TEST_ROWS, function(r)
    identical(r$kind, "gof") || is.na(r$digits), logical(1)))

  # S4 -- (stat, method) is UNIQUE, which is what makes test_row_key() total
  !anyDuplicated(paste(.trow_chr("stat"), .trow_chr("method"))[
    !is.na(.trow_chr("stat")) & !is.na(.trow_chr("method"))])

  # S5 -- EXACTLY ONE crosstab row per (var_kind x anova choice x design). This is the invariant
  # test_grid_crosstab() states only in a comment ("one displayed test per col_var"), and it is what
  # lets a third ANOVA F be added as one row with no code change anywhere.
  all(vapply(c("welch", "classic"), function(a)
    all(table(vapply(test_crosstab_displayed(a),
                     function(k) paste(TEST_ROWS[[k]]$var_kind, TEST_ROWS[[k]]$design), character(1)))
        == 1L), logical(1)))

  # S6 -- the producer partition: a crosstab row is asked for by `tab(test = TRUE)`, never by name,
  # and it is the only kind that names a column kind and carries the two rendered labels.
  identical(.trow_chr("producer") == "tab", is.na(.trow_chr("stat")))
  identical(.trow_chr("producer") == "tab", !is.na(.trow_chr("var_kind")))
  identical(.trow_chr("producer") == "tab", !is.na(.trow_chr("cell_label")))
  identical(.trow_chr("producer") == "tab", !is.na(.trow_chr("word")))

  # S7 -- a footer LINE has no cell, so it has no row kind, and it is outside the footer spec
  all(is.na(.trow_chr("kind")[.trow_chr("render") == "line"]))
  length(intersect(TEST_FOOTER_KEYS, .trow_keys(.trow_chr("render") == "line"))) == 0L

  # S8 -- the generated check block round-trips: every discriminator REG_CHECKS can emit is a row
  # here, and every row keyed by a check name is one of them.
  setequal(reg_check_types(), .trow_keys(.trow_chr("block") == "check"))
})

# The labels are gettext()'d DYNAMICALLY (gettext(r$noun), gettext(r$cell_label)), which potools
# cannot see -- the msgids left reg_footer_spec()'s static gettext() calls when they became TEST_ROWS
# data. This dead branch states each one exactly once so `Rscript dev/update_translations.R` extracts
# them; nothing here ever runs. Same device as reg_check_msgid_anchor() / legend_measure_word()'s.
#' @keywords internal
#' @noRd
test_rows_msgid_anchor <- function() {
  if (FALSE) c(
    # the reg footer row nouns
    gettext("N"), gettext("LR vs null"), gettext("Wald vs null"), gettext("F"),
    gettext("R2"), gettext("Adjusted R2"), gettext("McFadden R2"), gettext("Nagelkerke R2"),
    gettext("Cox-Snell R2"), gettext("Residual SD"), gettext("AIC"), gettext("BIC"),
    gettext("LR vs baseline"), gettext("F vs baseline"), gettext("Wald vs baseline"),
    gettext("Delta-AIC vs baseline"), gettext("LR vs previous"), gettext("F vs previous"),
    gettext("Wald vs previous"), gettext("Delta-AIC vs previous"),
    # the interaction line's instruments
    gettext("likelihood ratio"), gettext("F test"), gettext("Wald test"),
    # the crosstab cell labels and row-name words ("F, survey" already translated; the rest is
    # notation and proper names, which a translator is expected to leave alone -- gettext() PERMITS
    # a translation, it does not ask for one)
    gettext("Chi2"), gettext("Chi2, Rao-Scott"), gettext("F, Welch"), gettext("F, survey"),
    gettext("Welch F"), gettext("ANOVA F")
  )
  invisible(NULL)
}
