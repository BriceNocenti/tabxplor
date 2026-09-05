# PURPOSE: the ONE renderer of the `test` table attribute -- the console block and the export footer
#   rows, for a crosstab and for a regression alike.
# ROLE: three shared layers, each used by both producers.
#   1. CONTENT -- test_display_rows() (which rows this table shows), test_cell_label_weak() (the
#      label and the weak-chi2 flag), the test_fmt_* formatters and the fmt-cell builders.
#   2. CONSOLE -- test_summary_grid() turns the tidy `test` tibble into a backend-free grid;
#      test_render_console() prints it as a GFM pipe table above the tibble.
#   3. EXPORT  -- tab_append_footer() is the one fmt-frame append engine behind both inline-row
#      appenders (tab_pvalue_lines / reg_footer_lines, R/tab_classes.R, thin configs over it).
# KEY CONSTRAINTS:
#   - TEST_ROWS declares what a ROW IS; new_test_tibble() (R/tab_classes.R) declares the COLUMNS, and
#     the producer decides what it computes. WARNING: test_group_cols() reads every UNDECLARED column
#     as a grouping key, so a column added to the tibble without being declared there breaks grouping.
#   - The crosstab-versus-regression arm keys off the STORED table kind (tab_is_reg()), never a label.
#   - REG_CHECKS (R/reg-assumptions.R) owns each model check's title, threshold and default. This
#     file points at it and restates none of its rows.
#   - The grid is rebuilt on every console print, so it is base-R indexing over a small tibble: no
#     tidyr, no per-cell dplyr.
#   - WARNING: the check labels must stay inside a FUNCTION. gettext() has to run at render, under
#     the ambient locale; a top-level list would freeze the locale the namespace was loaded in.
#   - A p-value is formatted ELEMENT-WISE: formatC() over a vector pads to a common width, which
#     would inject stray spaces into the cells, and the renderer pads per column itself.
#   - A weak chi2 -- one whose smallest expected cell count is under 5 -- takes a trailing " !": the
#     standard validity caveat, shown rather than hidden.
# See: CLAUDE.md section "tabxplor architecture" (exports and rendering); R/tab_classes.R
#      (new_test_tibble, which declares the attribute's columns).

# === SECTION: shared formatters =====================================================================

# An expected count below this flags a chi2 weak.
test_weak_min_e <- 5

# Percentage-style p: "<0.01%" below 1e-4, else 3 significant figures. Element-wise: formatC() on a
# vector pads to a common width, so this pads per call instead.
test_fmt_pvalue <- function(p) {
  vapply(p, function(pi)
    if (is.na(pi)) NA_character_
    else if (pi < 1e-4) "<0.01%"
    else paste0(trimws(formatC(pi * 100, format = "g", digits = 3)), "%"),   # "g" left-pads a scalar
    character(1))
}

# Adaptive precision (integers >=100, 1 decimal >=10, else 2), no thousands grouping (the chi2/F
# convention).
test_fmt_stat_value <- function(v) {
  av <- abs(v)
  digits <- ifelse(is.na(v), 0L, ifelse(av >= 100, 0L, ifelse(av >= 10, 1L, 2L)))
  vapply(seq_along(v), function(i)
    if (is.na(v[i])) NA_character_ else formatC(v[i], format = "f", digits = digits[i]),
    character(1))
}

# "1911 (df 6)" (chi2) / "127 (df 2; 2029)" (F, numerator; denominator). df2 rounded (a Welch df is
# fractional).
test_fmt_stat <- function(statistic, df1, df2) {
  st <- test_fmt_stat_value(statistic)
  df_txt <- ifelse(is.na(df2),
                   paste0("(df ", formatC(df1, format = "d"), ")"),
                   paste0("(df ", formatC(df1, format = "d"), "; ",
                          formatC(round(df2), format = "d"), ")"))
  ifelse(is.na(statistic), NA_character_, paste0(st, " ", df_txt))
}

# Thin-space thousands grouping (N, AIC, BIC).
test_fmt_num <- function(v, digits = 0L) {
  if (is.na(v)) return(NA_character_)
  prettyNum(formatC(v, format = "f", digits = as.integer(digits)), big.mark = " ")
}

# Non-significant: p >= 0.05.
test_is_nonsig <- function(p) !is.na(p) & p >= 0.05

# A model-check number past its own CONVENTION (REG_CHECKS$<k>$flag) earns the faintest under-shade,
# "look at this"; a non-significant test earns the deepest. NA threshold = the check names none.
test_is_flagged <- function(value, flag) {
  if (length(flag) != 1L || is.na(flag)) return(FALSE)
  !is.na(value) && value >= flag
}

# ASCII symbol per effect-size measure, so every backend renders it.
test_es_symbol <- function(es_type)
  switch(es_type %||% "", "cramer_v" = "V", "phi" = "phi", "eta2" = "eta2", NA_character_)

# "V = 0.18" -- the symbol prefix aids the console reader; exports show the bare number, since the
# column type already tells V from eta2.
test_fmt_es <- function(effect_size, es_type) {
  vapply(seq_along(effect_size), function(i) {
    v <- effect_size[i]; sym <- test_es_symbol(es_type[i])
    if (is.na(v) || is.na(sym)) NA_character_
    else paste0(sym, " = ", formatC(v, format = "f", digits = 2L))
  }, character(1))
}

# The bare in-cell label ("Chi2" / "Chi2 !" / "F, Welch"), shared by the console grid and the export
# p-value cell (pvalue_line_fmt wraps it in parens). " !" flags a weak chi2 (min expected count under
# test_weak_min_e); `min_e` is NA for an F test.
test_cell_label_weak <- function(test, min_e = NA_real_) {
  base <- test_cell_label(test)
  if (is.na(base)) return(NA_character_)
  if (!is.na(min_e) && min_e < test_weak_min_e) paste0(base, " !") else base
}

# Parenthesised for the console p-value cell: "(Chi2 !)" / "" when the test is unknown.
test_pvalue_label <- function(test, min_e = NA_real_) {
  lbl <- test_cell_label_weak(test, min_e)
  if (is.na(lbl)) "" else paste0("(", lbl, ")")
}


# === SECTION: TEST_ROWS -- what kind of statistical row this is ====================================
# THE vocabulary of the `test` attribute's discriminator column: one row per kind of test row, for
# BOTH producers. The `test` tibble is a 15-column union type carrying 42 kinds of row
# (new_test_tibble(), R/tab_classes.R).
#
# Nothing here decides a statistic -- the arithmetic stays in chi2_compute_test() / reg_glance() /
# svy_omnibus_one() / the five checks.
#
# THE COLUMNS
#   producer    "tab" (a crosstab omnibus test) | "reg". Partitions every consumer.
#   kind        "gof" (a plain number, the `gof` display token) | "pvalue" | NA on a `line` row.
#               The row_kind vocabulary (R/row-model.R), checked in R/zzz-fact-keys.R.
#   digits      gof rows only. ⚠ DELIBERATELY absent (never NA) on pvalue rows: reg_test_rows_plan()'s
#               `%||% 0L` relies on the absence.
#   render      "grid" = a footer ROW (a cell per model column) | "line" = a table-wide footer
#               SENTENCE (a pooled test belongs to no single column) | "record" = recorded in the
#               `test` tibble and rendered NOWHERE. Only the model N is a record: the `n` column
#               shows it beside the levels it counts, and reg_plot_nobs() reads it back from here as
#               the guard that a user-supplied `data` reproduces the fit.
#   noun        the label's subject, a bare msgid (gettext at render, never at load).
#   instrument  the label's parenthetical, a bare msgid or NA. The label is
#               reg_check_label(noun, instrument) for every reg row -- one rule for all 34.
#   stat        WHICH `stats =` KEY REQUESTS THIS ROW (reg only). This is the many-to-one that makes
#               the user's vocabulary smaller than the storage's: linearity_lr/_f/_wald all carry
#               "linearity", the four compare_baseline* rows all carry "compare_baseline". NA on a
#               crosstab row -- an omnibus test is asked for with `tab(test = TRUE)`, not by name.
#   method      "lr" | "f" | "wald" | "aic" -- WHICH INSTRUMENT fired. Exactly one of a `stat`
#               block's rows is written per fit; (stat, method) is unique, which is what makes
#               test_row_key() total.
#   design      TRUE = the survey-design variant of a crosstab test.
#   var_kind    "pct" | "mean" -- which column kind a crosstab row describes (EST_SCALES' vocabulary).
#   anova       "welch" | "classic" -- which options(tabxplor.anova) value SELECTS this row. Both are
#               computed and stored; the choice is pure display. NA = not an ANOVA F.
#   cell_label  the in-cell test label ("Chi2", "F, Welch"), crosstab only.
#   word        the same test's name inside the p-value ROW name ("Chi2", "Welch F", "ANOVA F").
#               Two columns because they genuinely differ: the cell is cramped, the row name is prose.
#
# ⚠ THE ORDER IS THE CONTRACT: this declaration order IS the footer's display order. A reader
#   travels OUTWARD FROM THE DATA, in four movements: 1. N (what was fitted on); 2. the CHECKS, worst
#   first (proportionality/linearity break what the number MEANS, dispersion breaks every star in the
#   table, collinearity/influence only say how fragile it is); 3. the CONTENT (overall association,
#   interaction); 4. the COMPARISON (how good is this model, and against what).
# ⚠ WHAT IS NOT A ROW: Fisher's exact p and the weak-chi2 " !" flag are CONDITIONS ON the chi2 row
#   (`pvalue_exact` and `min_e` columns of it), which is what keeps the tidy shape and the row count
#   stable. They stay in test_cell_label_weak() / test_pvalue_descriptor()'s bodies.

# The model-check rows are GENERATED from REG_CHECKS in three slots, because the exact Pearson
# dispersion sits between two of them. Assertion S8 refuses a check that reaches no slot.
#
# `block` names WHICH PRODUCER WRITES THE ROW, stamped per block: `stat` cannot serve that role,
# since a single-instrument row's `stat` IS its own name (`dispersion`, `compare_baseline`).
# `producer` follows from it (only `omnibus` is a crosstab block).
#' @noRd
TEST_ROWS <- local({
  # ⚠ every member is NA when unset, never NULL: the defaulting below is utils::modifyList(), which
  # REMOVES an entry whose value is NULL rather than setting it.
  reg_gof <- function(noun, digits, stat, instrument = NA_character_, method = NA_character_,
                      render = "grid")
    list(producer = "reg", kind = "gof", digits = as.integer(digits), render = render,
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
    # --- 1. what was fitted on -------------------------------------------------------------------
    size = list(
      n             = reg_gof("N",              0L, stat = "n")),
    # --- 2. the checks: can the table be read as printed? ----------------------------------------
    check_meaning = test_rows_from_checks(c("proportionality", "linearity")),
    check_se      = test_rows_from_checks("dispersion"),
    # `phi` is the EXACT Pearson dispersion; `dispersion` names the CHECK (max robust/model SE) --
    # two readings of one question, so they sit adjacent. This one is reg_glance()'s row.
    disp_exact = list(
      phi           = reg_gof("Pearson dispersion", 2L, stat = "phi", instrument = "phi")),
    check_fragile = test_rows_from_checks(c("collinearity", "influence")),
    # --- 3. the content: what the table's rows say jointly ---------------------------------------
    # --- the per-predictor overall-association test ----------------------------------------------
    global = list(
      global_lr   = reg_p("Overall association", stat = "global", instrument = "LR",   method = "lr"),
      global_f    = reg_p("Overall association", stat = "global", instrument = "F",    method = "f"),
      global_wald = reg_p("Overall association", stat = "global", instrument = "Wald", method = "wald")),
    # --- the interaction between two PREDICTORS: one row per crossed pair, keyed to its model
    # column. It is a model COMPARISON with the additive counterpart (R/reg-cross.R), so a combined
    # factor -- which has no interaction TERM to drop -- is tested like any other.
    interaction = list(
      cross_lr   = reg_p("Interaction", stat = "interaction", instrument = "LR",   method = "lr"),
      cross_f    = reg_p("Interaction", stat = "interaction", instrument = "F",    method = "f"),
      cross_wald = reg_p("Interaction", stat = "interaction", instrument = "Wald", method = "wald")),
    # --- 4. goodness of fit and model comparison (reg_glance) ------------------------------------
    glance = list(
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
      bic           = reg_gof("BIC",            0L, stat = "bic")),
    # --- model comparison ------------------------------------------------------------------------
    # Four instruments x two modes. The user's key is `stat` (`compare_baseline` /
    # `compare_sequential`); which of the four rows is written is the model's business, looked up
    # through test_row_key(stat, method).
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
    # --- the aggregated effect-modification test ACROSS tab_vars GROUPS: a footer LINE, not rows --
    # `instrument` is the phrase reg_interaction_lines() prints ("a likelihood ratio test"); `noun`
    # is free since these rows carry no footer row of their own.
    # ⚠ its key is `group_interaction`, not `interaction`: that one names the test of a crossed PAIR
    # of predictors, which IS a footer ROW and is in the default set on a glm.
    group_interaction = list(
      group_interact_lr   = reg_line("likelihood ratio", stat = "group_interaction", method = "lr"),
      group_interact_f    = reg_line("F test",           stat = "group_interaction", method = "f"),
      group_interact_wald = reg_line("Wald test",        stat = "group_interaction", method = "wald")),
    # --- the crosstab omnibus tests ---------------------------------------------------------------
    # ⚠ exactly one row per (var_kind x anova x design) -- asserted below. That invariant is what
    # lets a third ANOVA F be added as one row, with no code change anywhere.
    omnibus = list(
      chi2        = tab_p("pct",  "Chi2",             "Chi2"),
      chi2_design = tab_p("pct",  "Chi2, Rao-Scott",  "Chi2",    design = TRUE),
      F_welch     = tab_p("mean", "F, Welch",         "Welch F", anova = "welch"),
      F_classic   = tab_p("mean", "F",                "ANOVA F", anova = "classic"),
      # the flat and the full design run the SAME survey estimator (svyglm + regTermTest Wald), so
      # there is one design row, and its `word` must not claim a Welch F.
      F_design    = tab_p("mean", "F, survey", "F", design = TRUE))
  )
  # SLOT -> PRODUCER. A slot is a position in the reading order; `block` is who writes the row, and
  # the two are not one-to-one (see the ordering note above).
  producer_of <- c(size = "glance", check_meaning = "check", check_se = "check",
                   disp_exact = "glance", check_fragile = "check", global = "global",
                   interaction = "interaction", glance = "glance", compare = "compare",
                   group_interaction = "group_interaction", omnibus = "omnibus")
  stopifnot(setequal(names(blocks), names(producer_of)))
  defaults <- list(digits = NA_integer_, design = FALSE, var_kind = NA_character_,
                   anova = NA_character_, cell_label = NA_character_, word = NA_character_,
                   stat = NA_character_, method = NA_character_)
  unlist(lapply(names(blocks), function(b)
    lapply(blocks[[b]], function(r)
      utils::modifyList(defaults, c(r, list(block = unname(producer_of[[b]])))))),
    recursive = FALSE)
})

# The row readers. `.trow_chr` / `.trow_lgl` project one member over every row (the .dtok_* idiom of
# R/tab-display.R); `.trow_keys` is the workhorse -- the names of the rows a predicate keeps, IN
# DECLARATION ORDER, which is what makes every derived vector below order-stable.
#' @noRd
.trow_chr <- function(member)
  vapply(TEST_ROWS, function(r) as.character(r[[member]])[[1]], character(1))
#' @noRd
.trow_lgl <- function(member) vapply(TEST_ROWS, function(r) isTRUE(r[[member]]), logical(1))
# ⚠ which(), not [keep]: most members are NA where they do not apply, and `NA == "lr"` is NA --
# logical indexing would turn that into a phantom element instead of dropping the row.
#' @noRd
.trow_keys <- function(keep) names(TEST_ROWS)[which(keep)]

#' @noRd
TEST_REG_KEYS      <- .trow_keys(.trow_chr("producer") == "reg")          # tab_kind()'s fallback
#' @noRd
TEST_FOOTER_KEYS   <- .trow_keys(.trow_chr("producer") == "reg" &
                                   .trow_chr("render") == "grid")         # reg_footer_spec()
#' @noRd
TEST_CROSSTAB_KEYS <- .trow_keys(.trow_chr("producer") == "tab")

# test_row_key(stat, method) -- the (requesting key, instrument) -> discriminator lookup. Total
# because (stat, method) is unique (asserted below).
#' @noRd
test_row_key <- function(stat, method) {
  k <- .trow_keys(.trow_chr("stat") == stat & .trow_chr("method") == method)
  if (length(k) != 1L)
    cli::cli_abort("No single TEST_ROWS row for stat {.val {stat}}, method {.val {method}}.",
                   .internal = TRUE)
  k
}

# test_row_types(stat) -- the `c(wald=, f=, lr=)` map shape reg_term_tests() expects.
#' @noRd
test_row_types <- function(stat) {
  k <- .trow_keys(.trow_chr("stat") == stat & !is.na(.trow_chr("method")))
  stats::setNames(k, .trow_chr("method")[k])
}

# The label of any reg row: one rule, applied to `noun` + `instrument`, translated at render.
#' @noRd
test_row_label <- function(key) {
  r <- TEST_ROWS[[key]]
  reg_check_label(r$noun, r$instrument)
}


# === SECTION: displayed-row selection, cell builders, reg footer spec ===============================

# Which one-way ANOVA F a table displays for its mean col_vars. Both F rows are computed and stored;
# this is a pure DISPLAY choice: the table's own stated intent, else the global option.
#' @noRd
tab_anova <- function(x) {
  a <- get_render_extras(x)[["anova"]]
  if (is.null(a) || !nzchar(a[[1]])) tx_option("anova") else a[[1]]
}

# Picks the DISPLAYED test row per (subtable x col_var): chi2 for factor col_vars, the chosen ANOVA F
# for mean ones. A weak chi2 (min_e < 5) shows its Fisher-exact `pvalue_exact` instead of the flagged
# chi2 p.
test_display_rows <- function(test_tbl, anova = tx_option("anova")) {
  # A design-based table carries chi2_design / F_design INSTEAD of the classic rows -- one family
  # per table. Which estimator ran is a per-column attribute (tab_stamp_inference()), not a second
  # encoding here.
  keep <- test_crosstab_displayed(anova)
  disp <- dplyr::filter(test_tbl, .data$test %in% keep)
  if (is.null(disp[["pvalue_exact"]])) disp$pvalue_exact <- NA_real_
  disp
}

#' @noRd
test_crosstab_displayed <- function(anova = tx_option("anova")) {
  av <- .trow_chr("anova")
  .trow_keys(.trow_chr("producer") == "tab" & (is.na(av) | av == anova))
}

# The colour comes from an explicit rule in fmt_color_slots() reading the real `pvalue` field, so it
# fires under every color_signif policy. `label` composes "{pvalue} (<label>)" as a text-backend
# suffix only -- Excel keeps the raw p.
pvalue_line_fmt <- function(p, label = NA_character_) {
  disp <- ifelse(is.na(label) | !nzchar(label), "pvalue", paste0("{pvalue} (", label, ")"))
  fmt(display = disp, scale = "level_n", n = NA_integer_, pvalue = p, digits = 2L)
}

# Notation ("Chi2", "F") stays untranslated by translator choice, not by code.
test_cell_label <- function(test) {
  r <- TEST_ROWS[[test]]
  if (is.null(r) || is.na(r$cell_label)) return(NA_character_)
  gettext(r$cell_label)
}

# The p-value ROW NAME: names the test(s) across the group's columns -- factor side "Chi2"/"Fisher",
# numeric side "Welch F"/"ANOVA F", a shared "; survey-design" suffix.
# ⚠ `tests` is always the DISPLAYED rows, so at most one row per var_kind reaches here.
test_pvalue_descriptor <- function(tests, used_exact = FALSE, weak = FALSE) {
  tests <- unique(tests[!is.na(tests)])
  tests <- tests[tests %in% TEST_CROSSTAB_KEYS]
  vk    <- vapply(tests, function(t) TEST_ROWS[[t]]$var_kind, character(1))
  fac   <- tests[vk == "pct"]
  num   <- tests[vk == "mean"]
  parts <- character(0)
  # "Fisher" / "Chi2 !" are conditions on the chi2 row, not rows of their own.
  if (length(fac)) parts <- c(parts, if (used_exact) gettext("Fisher") else if (weak) gettext("Chi2 !")
                                     else gettext(TEST_ROWS[[fac[[1]]]]$word))
  if (length(num)) parts <- c(parts, gettext(TEST_ROWS[[num[[1]]]]$word))
  if (!length(parts)) return(gettext("pvalue"))
  robust <- if (any(vapply(tests, function(t) isTRUE(TEST_ROWS[[t]]$design), logical(1))))
    gettext("; survey-design") else ""
  enc2utf8(gettextf("pvalue (%s%s)", paste(parts, collapse = ", "), robust))
}

# The effect-size ROW NAME is just the measure(s) present: Cramer's V (larger tables) / phi (2x2) /
# eta2 (numeric ANOVA); mixed -> "Cramer's V, eta2".
test_es_measure <- function(es_types) {
  es_types <- unique(es_types[!is.na(es_types)])
  if (!length(es_types)) return(gettext("effect size"))
  # "Cramer's V" translates; phi/eta2 are notation, kept untranslated.
  lbl <- vapply(es_types, function(t)
    switch(t, "cramer_v" = gettext("Cram\u00e9r's V"), "phi" = "phi", "eta2" = "eta2", t), character(1))
  enc2utf8(paste(unique(lbl), collapse = ", "))
}

# --- Regression model-summary footer -------------------------------------------------------------
# GOF stats travel in the `test` attribute with reg-specific discriminators (reg_gof_rows() /
# reg_compare_rows(), R/tab_reg.R), disjoint from the crosstab rows so one attribute drives both.
# ⚠ MUST STAY A FUNCTION: labels are gettext()'d HERE, at render -- a top-level list would freeze
# the translation at load.
reg_footer_spec <- function() {
  stats::setNames(lapply(TEST_FOOTER_KEYS, function(k) {
    r   <- TEST_ROWS[[k]]
    out <- list(label = test_row_label(k), kind = r$kind)
    # digits is ABSENT (never NA) on a pvalue row: reg_test_rows_plan()'s `%||% 0L` relies on that.
    if (identical(r$kind, "gof")) {
      out$digits <- r$digits
      out$flag   <- as.numeric(r$flag %||% NA_real_)   # the "worth a look" threshold, or none
    }
    out
  }), TEST_FOOTER_KEYS)
}
reg_footer_test_types <- function() TEST_FOOTER_KEYS
# Interaction discriminators render as a footer LINE, absent from reg_footer_spec(); tab_kind()'s
# degraded fallback still needs every `producer == "reg"` key (TEST_REG_KEYS).

# NA-safe character read of a `test` column: absent -> "", NA -> "".
# ⚠ read by NAME, never `tt$<col>` -- `$` WARNS on an unknown column.
test_key_col <- function(tt, col) {
  if (!col %in% names(tt)) return(rep("", nrow(tt)))
  ifelse(is.na(tt[[col]]), "", as.character(tt[[col]]))
}

# The sub-population columns of a `test` tibble, outside the fixed schema -- named after the grouping
# variable in both arms, so a row with no group column describes the whole table/model.
test_group_cols <- function(tt) {
  if (is.null(tt)) return(character(0))
  nms <- setdiff(names(tt), names(new_test_tibble()))
  # ⚠ the two renderers add scratch keys (`.grp`, `.term`); dot-prefixed names are never data.
  nms[!startsWith(nms, ".")]
}

# One row per (test, term), in spec then term order.
# ⚠ built from the WHOLE `reg` slice, so K (the block height) stays constant across split groups
# (tab_append_footer() requires it); a group missing one predictor shows a blank cell.
reg_test_rows_plan <- function(reg) {
  spec <- reg_footer_spec()
  tm   <- test_key_col(reg, "var")
  keep <- reg$test %in% names(spec)
  if (!any(keep)) return(NULL)
  k <- unique(data.frame(test = reg$test[keep], term = tm[keep], stringsAsFactors = FALSE))
  # grouped by KIND (TEST_ROWS' order) then BUILD order -- the model's own term order, never
  # alphabetical.
  k <- k[order(match(k$test, names(spec)), seq_len(nrow(k))), , drop = FALSE]
  sp <- spec[k$test]
  lab <- vapply(sp, `[[`, character(1), "label")
  k$label  <- ifelse(nzchar(k$term), paste0(lab, ": ", k$term), lab)
  k$kind   <- vapply(sp, `[[`, character(1), "kind")
  k$digits <- vapply(sp, function(s) as.integer(s$digits %||% 0L), integer(1))
  k$flag   <- vapply(sp, function(s) as.numeric(s$flag %||% NA_real_), numeric(1))
  rownames(k) <- NULL
  k
}

# gof -> the "gof" token; pvalue -> pvalue_line_fmt (no in-cell label); a missing stat -> blank.
reg_gof_cell   <- function(value, digits, flag = NA_real_)
  fmt(display = if (test_is_flagged(value, flag)) "gof_warn" else "gof",
      scale = "level_n", n = NA_integer_, diff = value, digits = as.integer(digits))
reg_pvalue_cell <- function(p) pvalue_line_fmt(p)
reg_blank_cell  <- function() fmt(display = "blank", scale = "level_n", n = NA_integer_)

# Adaptive precision; df is dropped (the p-value row's label already names the test).
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
  # a value column here is a BLOCK -- (col_var, col_group), not col_var alone: after a spread, two
  # blocks can share a col_var for two sub-populations, and keying on `col` alone would collapse
  # them into one p-value column. The key is composed by fmt_col_block().
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

  key_cols   <- c("var", tabvars_in_tt)
  keys       <- unique(disp[key_cols])
  # header row: blank for `var` (its values ARE the variable names), the variable name for a tab_var
  label_headers <- c("", tabvars_in_tt)

  groups <- lapply(seq_len(nrow(keys)), function(g) {
    sel <- rep(TRUE, nrow(disp))
    for (kc in key_cols) sel <- sel & disp[[kc]] == keys[[kc]][g]
    sub <- disp[sel, , drop = FALSE]

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
    # a weak chi2 shows its Fisher-exact p instead of the flagged chi2 one; the test-type label lives
    # in the row name (test_pvalue_descriptor), so the cell is the bare p-value.
    p_exact <- if (!is.null(sub[["pvalue_exact"]])) sub$pvalue_exact[idx] else rep(NA_real_, length(idx))
    p_show  <- ifelse(!is.na(p_exact), p_exact, sub$pvalue[idx])
    pval <- test_fmt_pvalue(p_show)
    pcell <- ifelse(is.na(pval), "", pval)
    nonsig <- test_is_nonsig(p_show)

    # no separate "statistic" row (ambiguous once effect size shares the block): p-value then effect
    # size, each row named for its own test/measure.
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

  # model columns (value cols) = the distinct fit columns, first-appearance order; header = the
  # outcome name, read off the ROW'S OWN KEY, where it IDENTIFIES the column (one model per outcome)
  # -- else the column key (the model label) itself, which is what a model COMPARISON needs, since
  # every column there shares the same outcome.
  value_cols <- unique(reg$col)
  dep_col <- if ("outcome" %in% names(reg)) reg$outcome else rep(NA_character_, nrow(reg))
  outcome_of  <- vapply(value_cols, function(cv) {
    d <- unique(dep_col[reg$col == cv])
    d <- d[!is.na(d) & nzchar(d)]
    if (length(d) == 1L) d else NA_character_
  }, character(1), USE.NAMES = FALSE)
  value_headers <- if (!anyNA(outcome_of) && !anyDuplicated(outcome_of)) outcome_of else value_cols

  plan <- reg_test_rows_plan(reg)
  if (is.null(plan) || !nrow(plan)) return(NULL)
  reg$.term <- test_key_col(reg, "var")

  # split levels (the group key): the column NAMED after the split variable, the same rule the
  # crosstab arm uses for its tab_vars. "" (no split) -> one unnamed group.
  gc       <- test_group_cols(reg)
  rv_key   <- if (!length(gc)) rep("", nrow(reg)) else test_key_col(reg, gc[1])
  reg$.grp <- rv_key
  grp_lv   <- unique(rv_key)
  is_split <- any(nzchar(grp_lv))

  # shared-predictors column (outcome-vector / single model). A model COMPARISON has per-column
  # predictors that a row-dimension column cannot hold -> omit it (columns already name the models).
  show_preds <- !is.null(meta) && !isTRUE(meta$comparison) && length(meta$predictors) > 0

  label_headers <- c(if (is_split) "" else NULL, if (show_preds) gettext("predictors") else NULL)

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
      warn <- vapply(value_cols, function(cv) {
        r <- hit(cv)
        test_is_flagged(if (nrow(r) > 0) r$statistic[1] else NA_real_, pk$flag)
      }, logical(1))
      list(label = pk$label, cells = cells, nonsig = nonsig, warn = warn)
    })
    label_lines <- c(if (is_split) list(g) else NULL, if (show_preds) list(pred_lines) else NULL)
    list(label_lines = label_lines, rows = rows)
  })

  list(label_headers = label_headers, stat_header = gettext("Model fit"),
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

# Prints the grid as a GFM markdown table above the tibble. Label + stat columns left-aligned, value
# columns right-aligned with an empty separator column (mirrors the md exporter). Colour is applied
# AFTER padding, so ANSI codes never break the column alignment.
test_render_console <- function(grid) {
  if (is.null(grid)) return(invisible(NULL))
  L <- length(grid$label_headers)
  V <- length(grid$value_headers)

  # 1. assemble the plain (uncoloured) text of every logical column: L label cols, 1 stat col, V
  #    value cols, plus a parallel "red"/"warn" mask over the value cells.
  n_body <- 0L
  for (g in grid$groups) n_body <- n_body + length(g$rows)
  n_sep  <- length(grid$groups) - 1L
  n_out  <- n_body + n_sep

  lab   <- matrix("", nrow = n_out, ncol = L)
  stat  <- character(n_out)
  val   <- matrix("", nrow = n_out, ncol = V)
  red   <- matrix(FALSE, nrow = n_out, ncol = V)
  warn  <- matrix(FALSE, nrow = n_out, ncol = V)
  dashr <- logical(n_out)

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
      warn[r, ] <- row$warn %||% FALSE
    }
    if (gi < length(grid$groups)) { r <- r + 1L; dashr[r] <- TRUE }
  }

  # 2. per-column widths on the plain text (headers included), then 3. emit padded GFM rows.
  lab_w  <- vapply(seq_len(L), function(c) max(nchar(c(grid$label_headers[c], lab[, c]))), integer(1))
  stat_w <- max(nchar(c(grid$stat_header, stat)))
  val_w  <- vapply(seq_len(V), function(c) max(nchar(c(grid$value_headers[c], val[, c]))), integer(1))
  sep_w  <- 1L

  padl <- function(s, w) formatC(s, width = w, flag = "-")
  padr <- function(s, w) formatC(s, width = w)

  emit <- function(cells) paste0("| ", paste(cells, collapse = " | "), " |")

  interleave_val <- function(cells) {
    if (V == 0) return(character(0))
    out <- character(0)
    for (c in seq_len(V)) {
      out <- c(out, cells[c])
      if (c < V) out <- c(out, strrep(" ", sep_w))
    }
    out
  }

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
    # nonsig -> deepest under-shade (a verdict); warn -> faintest (a caution). One ladder.
    vcells <- vapply(seq_len(V), function(c) {
      cell <- padr(val[r, c], val_w[c])
      if (red[r, c]) cli::col_red(cell)
      else if (warn[r, c]) test_warn_style()(cell)
      else cell
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

# The console style of a flagged model check: the palette's FAINTEST under slot, read from the
# console's own detected theme so it matches the cells above it. Falls back to the plain string
# wherever the palette cannot be resolved -- a footer must never fail to print.
test_warn_style <- function() {
  hex <- tryCatch(get_color_style("color_code", type = "text")[[5L]], error = function(e) NA)
  if (is.null(hex) || length(hex) != 1L || is.na(hex)) return(identity)
  tryCatch(cli::make_ansi_style(hex), error = function(e) identity)
}

# Fills a "| x |" slot: width `w` + 2 surrounding spaces, so the marker's pipes align with the padded
# header/data cells. left ":---", right "---:", center ":--:".
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
# tab_append_footer() is the ONE fmt-frame append behind the crosstab p-value rows (tab_pvalue_lines)
# and the regression GOF footer (reg_footer_lines): it inserts K footer rows at the end of each
# group's rows on plain field-vectors, then rebuilds each column once. `group_of` gives each existing
# row's group id; `fmt_cell(nm, g)`/`nonfmt_val(nm, g)` build the K footer cells/strings per column;
# `footer_groups` (default all) is which groups actually get a block -- a subtable with no computable
# test is excluded, so it keeps its data rows with no p-value row appended.
tab_append_footer <- function(tabs, group_of, fmt_cell, nonfmt_val, attrs, regroup,
                              footer_groups = unique(group_of), row_role = NULL) {
  grp_lv  <- unique(group_of)
  fmt_nms <- names(tabs)[purrr::map_lgl(tabs, is_fmt)]

  # per fmt column: interleave [group field-frame, group footer-frame] over groups, stack once.
  # `row_role(g)` stamps the footer cells' own `row_kind` ("pvalue"/"gof"/"blank"), so the appended
  # rows say what they are and ride every later slice.
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
  all(.trow_chr("block") %in% c("glance", "compare", "global", "check", "interaction",
                                "group_interaction", "omnibus"))
  # `producer` is the convenience name of `block`: only the crosstab block is not a regression one
  identical(.trow_chr("producer") == "tab", .trow_chr("block") == "omnibus")
  all(.trow_chr("render")   %in% c("grid", "line", "record"))
  all(is.na(.trow_chr("kind")) | .trow_chr("kind") %in% names(ROW_KINDS))
  all(is.na(.trow_chr("method")) | .trow_chr("method") %in% c("lr", "f", "wald", "aic"))
  all(is.na(.trow_chr("var_kind")) | .trow_chr("var_kind") %in% c("pct", "mean"))

  # S3 -- digits is gof-only; absent (not NA) on a pvalue row (see reg_footer_spec()).
  all(vapply(TEST_ROWS, function(r)
    !identical(r$kind, "gof") || !is.na(r$digits), logical(1)))
  all(vapply(TEST_ROWS, function(r)
    identical(r$kind, "gof") || is.na(r$digits), logical(1)))

  # S4 -- (stat, method) is UNIQUE, which is what makes test_row_key() total
  !anyDuplicated(paste(.trow_chr("stat"), .trow_chr("method"))[
    !is.na(.trow_chr("stat")) & !is.na(.trow_chr("method"))])

  # S5 -- exactly one crosstab row per (var_kind x anova x design); lets a third ANOVA F be added as
  # one row with no code change.
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

# The labels are gettext()'d DYNAMICALLY, invisible to potools's static extraction. This dead branch
# states each msgid once so `Rscript dev/update_translations.R` can find them; nothing here runs.
# Same device as reg_check_msgid_anchor() / legend_measure_word().
#' @noRd
test_rows_msgid_anchor <- function() {
  if (FALSE) c(
    gettext("N"), gettext("LR vs null"), gettext("Wald vs null"), gettext("F"),
    gettext("R2"), gettext("Adjusted R2"), gettext("McFadden R2"), gettext("Nagelkerke R2"),
    gettext("Cox-Snell R2"), gettext("Residual SD"), gettext("AIC"), gettext("BIC"),
    gettext("LR vs baseline"), gettext("F vs baseline"), gettext("Wald vs baseline"),
    gettext("Delta-AIC vs baseline"), gettext("LR vs previous"), gettext("F vs previous"),
    gettext("Wald vs previous"), gettext("Delta-AIC vs previous"),
    gettext("likelihood ratio"), gettext("F test"), gettext("Wald test"),
    # notation and proper names are gettext()'d too -- it permits a translation, not demands one
    gettext("Chi2"), gettext("Chi2, Rao-Scott"), gettext("F, Welch"), gettext("F, survey"),
    gettext("Welch F"), gettext("ANOVA F")
  )
  invisible(NULL)
}
