# PURPOSE: Single source of truth for tab()'s argument-overwrite cascade (Phase 7b).
# ROLE: Pure, data-free resolution of the colour cascade shared by tab_build() and
#   tab_counts(): color = "auto" -> a concrete measure; the contrib / diff-family forcing
#   of chi2 / totrow / ci; and the split of the one `color` argument into the per-step
#   sub-passes each leaf/step reads (color_diff_OR / color_ctr / color_ci / color_num).
# KEY CONSTRAINTS:
#   - Pure function of (argument values, column CLASS metadata) -> settings. It never reads
#     column *values*. This is the boundary the Jamovi `.js` mirrors and the Phase 7c cache
#     keys on (a change in these inputs is exactly what forces a recompute).
#   - Byte-identical to the former inline cascade it replaces
#     (tab.R ~L1146-1203 + ~L1344-1374 ; tab-counts.R ~L278-293).
#   - DATA-DEPENDENT resolution stays at the leaf builders (tab_plain/tab_num), NOT here:
#     `levels = "auto"` (needs the real level count), `ref` literal/regex (matched against
#     built row labels), `na` dropping, and the leaf `tot`/`totaltab` forcing + warnings.
#   - The numeric `color = "auto"` arm (resolve_color_auto_num) is type-specific and lives
#     here too, but is invoked from tab_num() (means path), not from this settings pass.
# See: dev/tabxplor_argument_computation_map.md (the full argument -> computation map),
#      CLAUDE.md § "Phase 7b".

# Why this exists: before Phase 7b the same colour cascade was re-implemented in tab_build(),
# tab_counts() and (partly) tab_num(); the "diff-family colour needs a difference CI" rule
# alone lived in four places. Consolidating it here is what lets jmvtab drive the identical
# rules from `.js` and lets the cache invalidate on a single, well-defined settings object.
#
# @param color         Legacy text-channel colour string, recycled over row_vars
#   ("no"/"auto"/"diff"/"diff_ci"/"after_ci"/"contrib"/"OR"). `normalize_color_spec()` has
#   already collapsed the two-channel `color` spec into this before we run.
# @param color_signif  The NORMALIZED significance policy ("ignore"/"grey_non_signif"/
#   "guaranteed_effect"), i.e. `normalize_color_spec()$signif`. Phase 14a: the parser can only
#   fold the policy into `color` for an explicit "diff"/"ratio" measure -- `color = TRUE`/"auto"
#   must stay "auto" for the per-type dispatch below -- so the policy arrives separately and the
#   "a gated colour needs the difference CI" rule is applied here, on the RESOLVED colour.
# @param OR,ci,chi2    Row-axis argument vectors (recycled over row_vars). `chi2` logical.
# @param ref           Per-row_var reference spec (from resolve_ref_vector()); only its
#   symbolic emptiness ("no"/""/NA) is inspected here, never a literal/regex value.
# @param pct_vect      List (one element per row_var) of the per-col_var `pct` vectors.
# @param col_vars_text Logical over col_vars: which columns are factors (vs numeric).
# @param totrow        Logical vector (recycled over row_vars) OR NULL. When NULL (the
#   tab_counts caller, which drives total rows through its own `tot`), the contrib->totrow
#   forcing is skipped -- preserving that caller's existing behaviour.
# @param na,wt_name,other_if_less_than,comp,tab_vars,row_vars,col_vars,filter_expr
#   Phase 7d-ii cache-key inputs (DATA-FREE argument values / variable NAMES, never column
#   values). Defaulted so pre-7d-ii callers and the colour-cascade tests are unaffected; used
#   only to build `$cache_keys`. `wt_name`/`tab_vars`/`row_vars`/`col_vars` are character names;
#   `filter_expr` a symbolic string (or NA).
# @param color_ratio_ci Scalar logical (Phase 14b): the PCT text channel carries the `ratio`
#   measure, so the stored cell-vs-reference interval is the Katz one on the ratio scale. From
#   `color_pct_text_is_ratio()`; like `color_signif` it cannot ride the `color` string.
# @param stars Scalar logical (Phase 16f): the resolved `stars` setting. When TRUE it forces ci = "diff"
#   on the columns that can carry a difference CI (so the per-cell pvalue the stars are cut from exists),
#   unless ci was set explicitly or it is an OR table (its own pvalue via the OR path).
# @return list(color, chi2, ci, ci_scale, totrow, color_diff_OR, color_ctr, color_ci, color_num,
#   cache_keys). `ci_scale` ("diff"/"ratio", over row_vars) = the scale the difference CI is
#   expressed on. `cache_keys` = the symbolic key material for the persisted jmvtab cache tiers
#   0-2 (dev/tabxplor_jmvtab_cache_design.md §3); the tier-2 shaped-aggregate hash + population
#   hashes are added by the module (Phase 7e).
# @keywords internal
# @noRd
tab_resolve_settings <- function(color, OR, ci, chi2, ref, pct_vect, col_vars_text,
                                 totrow = NULL, color_signif = "ignore",
                                 color_ratio_ci = FALSE, stars = FALSE,
                                 na = "keep", wt_name = character(),
                                 other_if_less_than = 0, comp = "tab",
                                 tab_vars = character(), row_vars = character(),
                                 col_vars = character(), filter_expr = NA_character_) {

  # Hoisted out of the `color = "auto"` case_when below, because the Phase 14a `color_signif`
  # forcing needs the SAME predicates and must run BEFORE it (see there).
  pct_rowcol <- purrr::map_lgl(pct_vect, ~ all(.[col_vars_text] %in% c("row", "col")))
  auto_or    <- purrr::map2_lgl(
    pct_vect, OR,
    ~ all(.x[col_vars_text] %in% c("row", "col") &
            .y[col_vars_text] %in% c("OR", "OR_pct", "or", "or_pct"))
  )
  num_only   <- sum(col_vars_text) == 0

  # Phase 14a: a `color_signif` policy must force the difference CI it gates on -- BEFORE the
  # `color = "auto"` resolution below, so that `tab(color = TRUE, color_signif = <policy>)` is
  # identical to the explicit `tab(color = TRUE, ci = "diff", color_signif = <policy>)` the user
  # has to write today. Why it belongs here and not in normalize_color_spec(): the parser folds the
  # policy into the legacy string ("diff" -> "diff_ci"/"after_ci") only for an EXPLICIT diff/ratio
  # measure; `color = TRUE`/"auto" must arrive as "auto" (it dispatches per column type just below),
  # so the policy could not ride the string -- ci stayed "no", fmt_color_plan()'s gate saw NA bounds,
  # and EVERY cell went grey on the DEFAULT color = TRUE.
  # Gated == the colour will be diff-family: an explicit diff (legacy_union() never emits "ratio"),
  # or an "auto" that resolves to row/col percentages here, or to the numeric arm (num_only -> "auto"
  # survives and tab_num()/resolve_color_auto_num() turns it into a diff).
  # NEVER forced for the other two measures, and both exclusions matter:
  #   contrib -- has no difference CI at all (documented gap); pct_rowcol is FALSE for it anyway.
  #   OR      -- carries its OWN ci_type = "or" bounds (centre 1). It IS pct = "row"/"col", so it
  #              matches pct_rowcol; forcing ci = "diff" would overwrite those bounds with a
  #              difference CI centred on 0, whose inf is then tested against the OR neutral 1 ->
  #              never significant -> the policy would grey the WHOLE table. Hence `& !auto_or`.
  # WARNING: `auto_or` / `pct_rowcol` are all() over the FACTOR col_vars, so on a numeric-only table
  # they are all(logical(0)) == TRUE -- vacuously. Hence the num_only arm must be tested FIRST and on
  # its own: an "auto" numeric-only table is never an OR table (a mean has no OR notion; the OR branch
  # of the case_when below is itself guarded by `!num_only`), and its colour is resolved later by
  # tab_num()/resolve_color_auto_num() into a diff -- so it IS gated.
  signif_on <- !identical(color_signif, "ignore") && !is.na(color_signif[1])
  if (signif_on) {
    gated <- color %in% c("diff", "diff_ci", "after_ci") |
      (color == "auto" & (num_only | (!auto_or & pct_rowcol)))
    if (any(gated & ci == "cell")) {
      cli::cli_abort(c(
        "{.arg color_signif} = {.val {color_signif}} gates the colour on the DIFFERENCE confidence interval, but {.arg ci} = {.val cell} asks for the cell one.",
        "i" = "Use {.code ci = \"diff\"} (the default under a {.arg color_signif} policy), or {.code color_signif = \"ignore\"}."
      ))
    }
    ci[gated & ci != "diff"] <- "diff"
  }

  # DESIGN: color = "auto" resolves from the pct/OR/ci settings of the FACTOR col_vars ONLY:
  # OR-type -> "OR"; row/col pct + ci = "diff" -> "after_ci"; row/col pct -> "diff";
  # counts/all -> "contrib". A numeric-only table (no factor col_vars) keeps "auto" here and
  # is resolved by tab_num() via resolve_color_auto_num() (a mean has no contrib/OR notion).
  color_auto_text <- color == "auto" & ! num_only
  if (any(color_auto_text)) color <- dplyr::case_when(
    auto_or                   ~ "OR",

    pct_rowcol & ci == "diff" ~ "after_ci",
    pct_rowcol                ~ "diff"    ,
    purrr::map_lgl(pct_vect, ~ all(.[col_vars_text] %in% c("", "no", "all", "all_tabs"))) ~ "contrib" ,
    TRUE                                                                                  ~ "no" ,
  )

  # WARNING: contrib colouring paints the signed chi2 residual, which needs (a) total rows to
  # store each cell's contribution to variance and (b) a chi2 pass. Force both ON. The totrow
  # half is skipped for callers that pass totrow = NULL (tab_counts drives totals via `tot`).
  if (!is.null(totrow)) {
    ctr_no_row <- color == "contrib" & totrow == FALSE
    if (any(ctr_no_row)) {
      warning("total rows were added, since color == 'contrib' needs them ",
              "to store information about mean contributions to variance")
      totrow[ctr_no_row] <- TRUE
    }
  }
  chi2[color == "contrib" & chi2 == FALSE] <- TRUE

  # DESIGN: a difference colour compares each cell to a reference row/column, so `ref` is
  # mandatory; and the significance-gated variants (diff_ci / after_ci) additionally need the
  # difference confidence interval, so they force ci = "diff".
  if (any(color %in% c("diff", "diff_ci", "after_ci") & (ref %in% c("no", "") | is.na(ref)))) {
    cli::cli_abort(c(
      "With a difference {.arg color}, {.arg ref} must be provided.",
      "i" = "{.code color = \"diff\"} / {.code \"diff_ci\"} / {.code \"after_ci\"} compare each cell to a reference."
    ))
  }
  ci[color %in% c("diff_ci", "after_ci") & ci != "diff"] <- "diff"

  # Phase 16f: significance stars are cut from a stored per-cell pvalue, which exists ONLY where a
  # difference CI is computed (tab_ci / tab_num compute it under ci = "diff"; with ci = "no" the whole
  # tab_ci step is skipped, so stars silently print nothing). `stars = TRUE` (with or without colours)
  # therefore forces ci = "diff" on the columns that can carry one -- unless the user set ci explicitly
  # (ci != "no"), or it is an OR table (which stores its OWN ci_type = "or" pvalue via the OR path).
  # Runs AFTER the colour resolution above so it never flips a plain "diff" colour into the gated
  # "after_ci": stars must not change the colour MEASURE, only surface the pvalue. Placed before the
  # ci_scale pass so a ratio-coloured table's forced CI still rides the ratio (Katz) scale it displays.
  # NB: `or_on` (not `auto_or`) -- OR reaches this pass as a LOGICAL (it is stringified only in the leaf,
  # tab_plain), so `auto_or`'s string test is FALSE for it and cannot be reused to exclude OR here.
  if (isTRUE(stars)) {
    or_on <- if (is.logical(OR)) OR else OR %in% c("OR", "or", "OR_pct", "or_pct")
    ci[ci == "no" & !or_on & (num_only | pct_rowcol)] <- "diff"
  }

  # Phase 14b: which SCALE the cell-vs-reference interval is expressed on. The interval belongs to
  # the measure the reader SEES (the text channel): when that is the ratio, the bounds are Katz
  # log-RR ones on the ratio scale (ci_type = "ratio", neutral 1) and a background diff channel
  # derives from them; otherwise they are the difference methods (neutral 0) and a ratio channel
  # derives -- which is what happened for every ratio until now. `color_ratio_ci` already means
  # "the PCT text channel is the ratio" (color_pct_text_is_ratio(), R/tab.R); it is scalar because
  # the colour axis is globalised over row_vars (§5), and it says nothing about numeric columns --
  # tab_ci() applies it only where a proportion CI is what is being computed.
  # Only where a difference CI is computed at all: `ci = "cell"` is a one-proportion interval with
  # no reference, so it has no ratio counterpart.
  ci_scale <- rep("diff", length(ci))
  if (isTRUE(color_ratio_ci)) ci_scale[ci == "diff"] <- "ratio"

  # Split the one resolved colour into the sub-pass each step reads: the diff/OR colour ->
  # tab_plain(); the contrib colour -> tab_chi2(); the ci colour -> tab_ci(); the numeric
  # colour -> tab_num(). "auto" only survives here for numeric-only tables (see above).
  color_diff_OR <- dplyr::case_when(
    color %in% c("OR", "or")     ~ "OR",
    color %in% c("diff", "auto") ~ "diff",
    TRUE                         ~ "no"
  )
  color_ctr  <- dplyr::recode(color,
                              "no"       = "no"  ,
                              "auto"     = "auto",
                              "diff"     = "no"  ,
                              "diff_ci"  = "no"  ,
                              "after_ci" = "no"  ,
                              "contrib"  = "all" ,
                              "OR"       = "no"   )
  color_ci   <- dplyr::recode(color,
                              "no"       = "no"      ,
                              "auto"     = dplyr::if_else(any(ci == "diff"), "after_ci", "no"),
                              "diff"     = "no"      ,
                              "diff_ci"  = "diff_ci" ,
                              "after_ci" = "after_ci",
                              "contrib"  = "no"      ,
                              "OR"       = "no"
  )
  color_num <- dplyr::recode(color,
                             "contrib"  = "no" ,
                             "OR"       = "no" ,
                             .default = color   )

  # Phase 7d-ii: DATA-FREE cache-key material for the persisted jmvtab cache tiers 0-2
  # (dev/tabxplor_jmvtab_cache_design.md §3). Symbolic only: the module (Phase 7e) turns the
  # `population` descriptor into a hash and appends the tier-2 shaped-aggregate hash.
  # `population` = "full" for na in {keep, drop}; a {mode, vars} descriptor for the population
  # modes drop_all (listwise on all selected vars) / common_base (row_var + first col_var +
  # tab_vars) -- §3.1. `grain` = the sorted tab_vars (the tab-var rollup axis).
  cache_keys <- tab_cache_keys(na = na, wt_name = wt_name,
                               other_if_less_than = other_if_less_than, comp = comp,
                               tab_vars = tab_vars, row_vars = row_vars, col_vars = col_vars,
                               filter_expr = filter_expr)

  list(color = color, chi2 = chi2, ci = ci, ci_scale = ci_scale, totrow = totrow,
       color_diff_OR = color_diff_OR, color_ctr = color_ctr,
       color_ci = color_ci, color_num = color_num,
       cache_keys = cache_keys)
}

# Build the symbolic (data-free) cache-key material for the persisted jmvtab cache tiers 0-2.
# Split out of tab_resolve_settings() only for readability; it is the same "one place computes
# the cache-key material the .js mirrors" boundary (dev/tabxplor_jmvtab_cache_design.md §8).
# @keywords internal
# @noRd
tab_cache_keys <- function(na = "keep", wt_name = character(), other_if_less_than = 0,
                           comp = "tab", tab_vars = character(), row_vars = character(),
                           col_vars = character(), filter_expr = NA_character_) {
  row_vars <- as.character(row_vars)
  col_vars <- as.character(col_vars)
  tab_vars <- as.character(tab_vars)
  wt_key   <- if (length(wt_name) == 0) "" else as.character(wt_name)[1]
  grain    <- sort(tab_vars)

  population <- if (na %in% c("keep", "drop")) {
    "full"
  } else if (na == "drop_all") {
    list(mode = "drop_all",
         vars = sort(unique(c(row_vars, col_vars, tab_vars))))
  } else if (na == "common_base") {
    list(mode = "common_base",
         vars = c(row_vars, if (length(col_vars) != 0) col_vars[1] else NULL, tab_vars))
  } else {
    "full"
  }

  list(
    tier0 = list(na = na, wt = wt_key, filter = filter_expr, population = population),
    tier1_common = list(grain = grain, wt = wt_key,
                        other_if_less_than = other_if_less_than, population = population),
    tier2 = list(comp = comp)
  )
}


# Numeric (means) arm of color = "auto". Kept separate from the factor cascade above because
# a mean has no contrib / OR notion: numeric "auto" keys only on whether a difference is
# possible (a real `ref` and ci != "cell"). Placeholder axes ("no_row_var" / "no_col_var")
# colour nothing. Byte-identical to the former inline block in tab_num() (tab.R ~L3319-3325).
#
# @param color   Scalar legacy colour string for this (single) numeric row_var.
# @param ref,ci  Scalars for this row_var (`ci` may be NULL at this stage).
# @param row_var,col_vars  Character name(s), used only to detect the synthetic placeholder
#   axes tab() injects when a row_var / col_var is absent.
# @return The resolved scalar colour ("after_ci"/"diff"/""/passed-through).
# @keywords internal
# @noRd
resolve_color_auto_num <- function(color, ref, ci, row_var, col_vars) {
  if (row_var == "no_row_var" || "no_col_var" %in% col_vars) return("")
  ci_diff <- if (!is.null(ci)) ci == "diff" else FALSE
  ci_cell <- if (!is.null(ci)) ci == "cell" else FALSE
  dplyr::case_when(
    color == "auto" & !ref %in% c("no", "") & ci_diff  ~ "after_ci",
    color == "auto" & !ref %in% c("no", "") & !ci_cell ~ "diff",
    color == "auto"                                    ~ "",
    TRUE                                               ~ color
  )
}
