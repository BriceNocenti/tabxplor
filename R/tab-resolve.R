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
#   already collapsed the two-channel `color`/`color_signif` spec into this before we run.
# @param OR,ci,chi2    Row-axis argument vectors (recycled over row_vars). `chi2` logical.
# @param ref           Per-row_var reference spec (from resolve_ref_vector()); only its
#   symbolic emptiness ("no"/""/NA) is inspected here, never a literal/regex value.
# @param pct_vect      List (one element per row_var) of the per-col_var `pct` vectors.
# @param col_vars_text Logical over col_vars: which columns are factors (vs numeric).
# @param totrow        Logical vector (recycled over row_vars) OR NULL. When NULL (the
#   tab_counts caller, which drives total rows through its own `tot`), the contrib->totrow
#   forcing is skipped -- preserving that caller's existing behaviour.
# @return list(color, chi2, ci, totrow, color_diff_OR, color_ctr, color_ci, color_num).
# @keywords internal
# @noRd
tab_resolve_settings <- function(color, OR, ci, chi2, ref, pct_vect, col_vars_text,
                                 totrow = NULL) {

  # DESIGN: color = "auto" resolves from the pct/OR/ci settings of the FACTOR col_vars ONLY:
  # OR-type -> "OR"; row/col pct + ci = "diff" -> "after_ci"; row/col pct -> "diff";
  # counts/all -> "contrib". A numeric-only table (no factor col_vars) keeps "auto" here and
  # is resolved by tab_num() via resolve_color_auto_num() (a mean has no contrib/OR notion).
  color_auto_text <- color == "auto" & ! sum(col_vars_text) == 0
  if (any(color_auto_text)) color <- dplyr::case_when(
    purrr::map2_lgl(
      pct_vect, OR,
      ~ all(.x[col_vars_text] %in% c("row", "col") &
              .y[col_vars_text] %in% c("OR", "OR_pct", "or", "or_pct")
      )
    )
    ~ "OR",

    purrr::map_lgl(pct_vect, ~ all(.[col_vars_text] %in% c("row", "col"))) & ci == "diff" ~ "after_ci",
    purrr::map_lgl(pct_vect, ~ all(.[col_vars_text] %in% c("row", "col")))                ~ "diff"    ,
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

  list(color = color, chi2 = chi2, ci = ci, totrow = totrow,
       color_diff_OR = color_diff_OR, color_ctr = color_ctr,
       color_ci = color_ci, color_num = color_num)
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
