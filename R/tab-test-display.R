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
#   - The crosstab arm keys crosstab-vs-reg off is_reg_footer(); a reg table carries reg_meta.
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
# phi (2x2), eta^2 (numeric / ANOVA). Last Phase j.
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


# === SECTION: displayed-row selection, cell builders, reg footer spec ===============================
# (Phase 16a moved these here from R/tab_classes.R so all `test`-attribute display lives in one module.)

# Pick the DISPLAYED test row per (subtable x col_var): chi2 for factor col_vars, and for mean
# col_vars the option-selected ANOVA F (Welch by default). Both F rows are stored; this chooses one.
# Last Phase j: a weak chi2 (min_e < 5) carries a `pvalue_exact` column = the Fisher-exact p on that
# same row; the p-value cell shows that reliable exact p (labelled "Fisher") instead of the flagged
# chi2 one. `pvalue_exact` is NA on a strong chi2 / on an older `test` attribute without the column.
test_display_rows <- function(test_tbl, anova = getOption("tabxplor.anova", "welch")) {
  keep_f <- paste0("F_", anova)
  # Last Phase j: a robust table carries chi2_kish/chi2_svy (factor) or F_kish/F_svy (numeric) INSTEAD
  # of the classic chi2 / F_welch|F_classic -- one family present per table, so filter on all of them.
  disp   <- dplyr::filter(test_tbl,
                          .data$test %in% c("chi2", "chi2_kish", "chi2_svy",
                                            keep_f, "F_kish", "F_svy"))
  if (is.null(disp[["pvalue_exact"]])) disp$pvalue_exact <- NA_real_
  disp
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
  fmt(display = disp, type = "n", n = NA_integer_, pvalue = p, digits = 2L)
}

# The label shown in a crosstab p-value cell for each test type (Phase 12f). NULL -> no in-cell label.
# Last Phase j: the robust variants name their method (Kish n_eff / Rao-Scott survey design).
test_cell_label <- function(test) {
  switch(test,
         "chi2" = "Chi2", "F_welch" = "F, Welch", "F_classic" = "F",
         "chi2_kish" = "Chi2, Kish", "chi2_svy" = "Chi2, Rao-Scott",
         "F_kish" = "F, Kish", "F_svy" = "F, survey",
         NA_character_)
}

# Last Phase m: the p-value ROW NAME (was the in-cell "(Chi2, Kish)" suffix -- moved out of the cell so a
# mixed factor/numeric row no longer wastes width, and the table-level test type is stated ONCE). Names
# the test(s) used across the group's columns: factor side "Chi2" (or "Fisher" when the exact test ran),
# numeric side "Welch F" / "ANOVA F"; a single robust suffix "; Kish" (n_eff rescale) or "; survey-design"
# (Rao-Scott / svyglm). Examples: "pvalue (Chi2)", "pvalue (Chi2, Welch F)", "pvalue (ANOVA F)",
# "pvalue (Chi2, Welch F; Kish)", "pvalue (Chi2, Welch F; survey-design)".
test_pvalue_descriptor <- function(tests, used_exact = FALSE, weak = FALSE) {
  tests <- unique(tests[!is.na(tests)])
  fac   <- tests[tests %in% c("chi2", "chi2_kish", "chi2_svy")]
  num   <- tests[tests %in% c("F_welch", "F_classic", "F_kish", "F_svy")]
  parts <- character(0)
  # a weak chi2 (smallest expected count < 5) with no exact companion keeps a " !" validity caveat.
  if (length(fac)) parts <- c(parts, if (used_exact) "Fisher" else if (weak) "Chi2 !" else "Chi2")
  if (length(num)) parts <- c(parts, if (any(num == "F_classic")) "ANOVA F" else "Welch F")
  if (!length(parts)) return("pvalue")
  robust <- if      (any(tests %in% c("chi2_kish", "F_kish"))) "; Kish"
            else if (any(tests %in% c("chi2_svy",  "F_svy")))  "; survey-design"
            else                                               ""
  paste0("pvalue (", paste(parts, collapse = ", "), robust, ")")
}

# Last Phase m: the effect-size ROW NAME = the measure(s) present, so no separate "effect size" text is
# needed. Cramer's V (larger factor tables) / phi (2x2) / eta^2 (numeric ANOVA); mixed -> "Cramer's V, eta2".
test_es_measure <- function(es_types) {
  es_types <- unique(es_types[!is.na(es_types)])
  if (!length(es_types)) return("effect size")
  lbl <- vapply(es_types, function(t)
    switch(t, "cramer_v" = "Cram\u00e9r's V", "phi" = "phi", "eta2" = "eta2", t), character(1))
  paste(unique(lbl), collapse = ", ")
}

# --- Regression model-summary footer (Phase 12f) -----------------------------------------------------
# GOF stats travel in the whole-table `test` attribute with reg-specific discriminators (built by
# reg_gof_tibble() / reg_compare_rows() in R/tab_reg.R), DISJOINT from the crosstab "chi2"/"F_*" so the
# same `test` attribute drives both. One entry per footer stat: its row label + how the cell renders.
# kind "gof" -> a plain number (the "gof" display token reading `statistic`); kind "pvalue" -> a p-value
# cell. `digits` applies to gof cells. Order here = the display / fallback order.
reg_footer_spec <- function() list(
  n                    = list(label = "N",                    kind = "gof",    digits = 0L),
  lr_null              = list(label = "LR vs null",           kind = "pvalue"),
  wald_null            = list(label = "Wald vs null",         kind = "pvalue"),
  f_model              = list(label = "F",                    kind = "pvalue"),
  r2                   = list(label = "R2",              kind = "gof",   digits = 3L),
  r2_adj               = list(label = "Adjusted R2",     kind = "gof",   digits = 3L),
  mcfadden_r2          = list(label = "McFadden R2",     kind = "gof",   digits = 3L),
  nagelkerke_r2        = list(label = "Nagelkerke R2",   kind = "gof",   digits = 3L),
  cox_snell_r2         = list(label = "Cox-Snell R2",    kind = "gof",   digits = 3L),
  sigma                = list(label = "Residual SD",          kind = "gof",   digits = 2L),
  aic                  = list(label = "AIC",                  kind = "gof",   digits = 0L),
  bic                  = list(label = "BIC",                  kind = "gof",   digits = 0L),
  dispersion           = list(label = "Dispersion",           kind = "gof",   digits = 2L),
  brant_po             = list(label = "Brant PO test",         kind = "pvalue"),
  compare_baseline     = list(label = "LR vs baseline",       kind = "pvalue"),
  compare_baseline_f   = list(label = "F vs baseline",        kind = "pvalue"),
  compare_baseline_wald = list(label = "Wald vs baseline",    kind = "pvalue"),
  compare_baseline_aic = list(label = "Delta-AIC vs baseline", kind = "gof",  digits = 0L),
  compare_seq          = list(label = "LR vs previous",       kind = "pvalue"),
  compare_seq_f        = list(label = "F vs previous",        kind = "pvalue"),
  compare_seq_wald     = list(label = "Wald vs previous",     kind = "pvalue"),
  compare_seq_aic      = list(label = "Delta-AIC vs previous", kind = "gof",  digits = 0L)
)
reg_footer_test_types <- function() names(reg_footer_spec())
reg_footer_labels     <- function() unname(vapply(reg_footer_spec(), `[[`, character(1), "label"))
is_reg_footer <- function(test_tbl)
  !is.null(test_tbl) && nrow(test_tbl) > 0 && any(test_tbl$test %in% reg_footer_test_types())

# A single footer cell (one fmt value), for the appended export rows. gof -> the "gof" token (value in
# `diff`); pvalue -> the pvalue_line_fmt shape (no in-cell label: the reg row label already names the
# stat). A missing stat -> a "blank" cell (renders "").
reg_gof_cell   <- function(value, digits) fmt(display = "gof", type = "n", n = NA_integer_,
                                              diff = value, digits = as.integer(digits))
reg_pvalue_cell <- function(p) pvalue_line_fmt(p)
reg_blank_cell  <- function() fmt(display = "blank", type = "n", n = NA_integer_)

# The inline-export STATISTIC cell (the `tabxplor.test_lines = "stat"` row) -- a "gof" number carrying
# the test statistic with adaptive precision (integers over 100, else 1-2 decimals). The df is dropped
# in exports (the p-value row's "(Chi2)"/"(F, Welch)" label names the test; the console keeps the full
# "1911 (df 6)"). Vectorised; an NA statistic -> a blank cell.
stat_line_fmt <- function(statistic) {
  d <- ifelse(is.na(statistic), 0L,
              ifelse(abs(statistic) >= 100, 0L, ifelse(abs(statistic) >= 10, 1L, 2L)))
  cells <- lapply(seq_along(statistic), function(i)
    if (is.na(statistic[i])) reg_blank_cell()
    else fmt(display = "gof", type = "n", n = NA_integer_, diff = statistic[i], digits = d[i]))
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
  if (is_reg_footer(test_tbl)) test_grid_reg(x, test_tbl) else test_grid_crosstab(x, test_tbl)
}

# --- crosstab arm: chi2 / ANOVA-F, one row-group per (row_var x tab_var level) ----------------------
test_grid_crosstab <- function(x, test_tbl) {
  disp <- test_display_rows(test_tbl)               # chi2 + the option-chosen F, one per (subtab, cv)
  disp <- disp[!is.na(disp$pvalue), , drop = FALSE]
  if (nrow(disp) == 0) return(NULL)

  rv <- tab_render_vars(x)
  # canonical col_var order from the table; fall back to first appearance in the test tibble
  value_cols <- if (isFALSE(rv$degrade)) intersect(rv$col_vars, unique(disp$col_var))
                else                     unique(disp$col_var)
  value_cols <- value_cols[value_cols %in% disp$col_var]
  if (length(value_cols) == 0) return(NULL)

  # tab_vars present in the test tibble = comp = "tab" (a per-subtable column); their absence with
  # tab_vars on the table = comp = "all" (one whole-table p-value, the group named "row_var x tab_vars").
  tab_vars      <- if (isFALSE(rv$degrade)) rv$tab_vars else character(0)
  tabvars_in_tt <- intersect(tab_vars, names(disp))
  comp_all      <- length(tab_vars) > 0 && length(tabvars_in_tt) == 0

  # leading label columns + the key that splits row-groups (first-appearance order)
  key_cols   <- c("row_var", tabvars_in_tt)
  keys       <- unique(disp[key_cols])
  # header row: blank for row_var (its values ARE the variable names), the variable name for a tab_var
  label_headers <- c("", tabvars_in_tt)

  groups <- lapply(seq_len(nrow(keys)), function(g) {
    sel <- rep(TRUE, nrow(disp))
    for (kc in key_cols) sel <- sel & disp[[kc]] == keys[[kc]][g]
    sub <- disp[sel, , drop = FALSE]

    # the leading label cell(s): the row_var name, then each tab_var level (or the collapsed comp="all"
    # label "row_var x tab1, tab2" in the single leading column)
    if (comp_all) {
      lab <- paste0(keys[["row_var"]][g], " \u00d7 ", paste(tab_vars, collapse = ", "))
      label_lines <- list(lab)
    } else {
      label_lines <- c(list(keys[["row_var"]][g]),
                       lapply(tabvars_in_tt, function(tc) as.character(keys[[tc]][g])))
    }

    # per value col: the source test row (there is exactly one displayed test per col_var here)
    idx  <- match(value_cols, sub$col_var)
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

    # Last Phase m: no "statistic" row (ambiguous once effect size shares the block); order = p-value then
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
  meta <- get_reg_meta(x)

  # model columns (value cols) = the distinct fit col_vars, first-appearance order; headers = the
  # dependent names when their count matches, else the col_var string with a "Model <eff> (dep)" strip.
  value_cols <- unique(reg$col_var)
  deps <- if (!is.null(meta)) meta$dependent else NULL
  value_headers <- if (!is.null(deps) && length(deps) == length(value_cols)) deps
                   else vapply(value_cols, reg_strip_model_prefix, character(1))

  # the ordered stats actually present (spec order)
  stats_present <- names(spec)[names(spec) %in% unique(reg$test)]
  if (length(stats_present) == 0) return(NULL)

  # split levels (the group key), from reg$row_var; "" (no split) -> a single unnamed group
  rv_key   <- if (is.null(reg$row_var)) rep("", nrow(reg)) else ifelse(is.na(reg$row_var), "", reg$row_var)
  reg$.grp <- rv_key
  grp_lv   <- unique(rv_key)
  is_split <- any(nzchar(grp_lv))

  # shared-predictors column (dependent-vector / single model). A model COMPARISON has per-column
  # predictors that a row-dimension column cannot hold -> omit it (columns already name the models).
  show_preds <- !is.null(meta) && !isTRUE(meta$comparison) && length(meta$predictors) > 0

  label_headers <- c(if (is_split) "" else NULL, if (show_preds) "predictors" else NULL)

  n_rows <- length(stats_present)
  pred_lines <- if (show_preds) test_wrap_items(meta$predictors, n_rows) else NULL

  groups <- lapply(grp_lv, function(g) {
    sub <- reg[reg$.grp == g, , drop = FALSE]
    rows <- lapply(stats_present, function(s) {
      sp <- spec[[s]]
      cells <- vapply(value_cols, function(cv) {
        r <- sub[sub$col_var == cv & sub$test == s, , drop = FALSE]
        if (nrow(r) == 0) return("")
        if (identical(sp$kind, "gof")) test_fmt_num(r$statistic[1], sp$digits %||% 0L)
        else {
          p <- test_fmt_pvalue(r$pvalue[1]); if (is.na(p)) "" else p
        }
      }, character(1))
      nonsig <- vapply(value_cols, function(cv) {
        r <- sub[sub$col_var == cv & sub$test == s, , drop = FALSE]
        identical(sp$kind, "pvalue") && nrow(r) > 0 && test_is_nonsig(r$pvalue[1])
      }, logical(1))
      list(label = sp$label, cells = cells, nonsig = nonsig)
    })
    label_lines <- c(if (is_split) list(g) else NULL, if (show_preds) list(pred_lines) else NULL)
    list(label_lines = label_lines, rows = rows)
  })

  list(label_headers = label_headers, stat_header = "Model fit",
       value_headers = value_headers, groups = groups)
}

# Strip a "Model OR (dependent)" fit col_var down to the dependent name; leave a bare "Model OR" as-is.
reg_strip_model_prefix <- function(cv) {
  m <- regmatches(cv, regexec("^Model .+ \\((.+)\\)$", cv))[[1]]
  if (length(m) == 2) m[2] else cv
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
tab_append_footer <- function(tabs, group_of, K, fmt_cell, nonfmt_val, attrs, regroup,
                              footer_groups = unique(group_of), row_role = NULL) {
  grp_lv  <- unique(group_of)
  fmt_nms <- names(tabs)[purrr::map_lgl(tabs, is_fmt)]

  # Phase 17c: extend the stored row-role vector (meta$vars$row_roles) in the SAME group-interleaved
  # order build_nonfmt uses, so the appended footer rows carry their stored kind. row_role(g) returns
  # the K roles of group g's footer block ("pvalue" / "gof"). Consumers then read tab_row_roles()
  # instead of matching the English footer label.
  if (!is.null(row_role)) {
    rr_in <- attrs$meta$vars$row_roles
    if (is.null(rr_in) || length(rr_in) != length(group_of))
      rr_in <- dplyr::if_else(is_totrow(tabs), "total", "data")
    attrs$meta$vars$row_roles <- unlist(lapply(grp_lv, function(g) {
      base <- rr_in[group_of == g]
      if (g %in% footer_groups) c(base, row_role(g)) else base
    }))
  }

  # per fmt column: interleave [group field-frame, group footer-frame] over groups, stack once.
  build_col <- function(nm) {
    meta   <- purrr::set_names(
      lapply(fmt_col_attrs, function(a) attr(tabs[[nm]], a, exact = TRUE)), fmt_col_attrs)
    frames <- unlist(lapply(grp_lv, function(g) {
      idx <- which(group_of == g)
      of  <- fmt_data_wn(tabs[[nm]][idx])
      if (!g %in% footer_groups) return(list(of))
      fr  <- fmt_data_wn(fmt_cell(nm, g))
      list(of, fr)
    }), recursive = FALSE)
    fmt_stack_frames(frames, meta)
  }
  # non-fmt column: each group's original values then (for a footer group) its K footer strings.
  build_nonfmt <- function(nm) {
    orig     <- tabs[[nm]]
    combined <- unlist(lapply(grp_lv, function(g)
      c(as.character(orig)[group_of == g],
        if (g %in% footer_groups) nonfmt_val(nm, g) else character(0))))
    if (is.factor(orig)) {
      lv <- levels(orig); factor(combined, levels = c(lv, setdiff(unique(combined), lv)))
    } else combined
  }

  out   <- purrr::set_names(lapply(names(tabs), function(nm)
    if (nm %in% fmt_nms) build_col(nm) else build_nonfmt(nm)), names(tabs))
  tabs2 <- tibble::new_tibble(out, nrow = length(out[[1]]))
  do.call(new_tab, c(list(tabs2), attrs)) |> dplyr::group_by(!!!rlang::syms(regroup))
}
