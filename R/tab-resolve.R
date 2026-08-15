# PURPOSE: THE argument boundary of the crosstab producers -- validation (Phase 19i) and the
#          argument-overwrite cascade (Phase 7b), each stated once.
# ROLE: two layers, in this file, in this order.
#   (1) tab_resolve_common_args() + TAB_ARG_VALUES / tab_validate_args() (Phase 19i, at the BOTTOM
#       of this file): what every producer must do to its arguments before any of them means
#       anything -- the `chi2` -> `test` rename, the vocabularies, the sizes, the "NULL -> option"
#       resolutions, the `OR` route, the colour spec + D28, `tot` -> (totrow, totcol),
#       `total_names`. tab() / tab_plain() / tab_num() / tab_counts() call it; five hand-written
#       copies that had already drifted are one.
#   (2) tab_resolve_settings(): the pure, data-free CASCADE shared by tab_build() and tab_counts():
#       color = "auto" -> a concrete MEASURE (through MEASURES' declared `auto_for` contexts), then
#       that measure's declared `requires` applied to chi2 / totrow / ci / ref.
# Phase 19c (KEY 4): it returns ONE resolved measure, not four per-step sub-passes. The old
#   color_diff_OR / color_ctr / color_ci / color_num split was a fossil of the pre-2.0.0 four-step
#   pipeline: it routed WHICH step stamped the colour attribute, in four hand-written recodes over
#   measure literals, one of which (color_ci) existed only to receive a legacy combined string the
#   cascade manufactured one step after 17d had decoded such strings away. Each consumer now asks
#   the measure what it needs -- measure_builds() for the contribution pass, measure_applies() for the
#   numeric one -- so adding a measure touches no step.
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
#   - VALIDATION happens once, in layer (1), before the cascade: the cascade may then assume its
#     inputs are legal. `ci` is the exception, and deliberately so -- its vocabulary carries a
#     soft-deprecation, so validating it means REWRITING it, which is resolve_ci_value()'s job.
# See: dev/tabxplor_argument_computation_map.md (the full argument -> computation map),
#      CLAUDE.md § "Phase 7b" / "Phase 19i".

# Why this exists: before Phase 7b the same colour cascade was re-implemented in tab_build(),
# tab_counts() and (partly) tab_num(); the "diff-family colour needs a difference CI" rule
# alone lived in four places. Consolidating it here is what lets jmvtab drive the identical
# rules from `.js` and lets the cache invalidate on a single, well-defined settings object.
#
# @param color         The pipeline text-channel MEASURE, recycled over row_vars
#   ("no"/"auto"/"diff"/"contrib"/"OR"). `normalize_color_spec()` has already collapsed the
#   two-channel `color` spec into this before we run -- and, since 17d, decoded the legacy combined
#   strings into a clean measure plus `color_signif`, so no composite can arrive here.
# @param color_signif  The NORMALIZED significance policy ("ignore"/"grey_non_signif"/
#   "guaranteed_effect"), i.e. `normalize_color_spec()$signif`. Phase 14a: the parser can only
#   fold the policy into `color` for an explicit "diff"/"ratio" measure -- `color = TRUE`/"auto"
#   must stay "auto" for the per-type dispatch below -- so the policy arrives separately and the
#   "a gated colour needs the difference CI" rule is applied here, on the RESOLVED colour.
# @param ci,chi2       Row-axis argument vectors (recycled over row_vars). `chi2` logical. `ci` is
#   the PUBLIC anchor vocabulary ("auto"/"no"/"cell"/"ref", + the soft-deprecated "diff"/"ratio");
#   what comes BACK is the step vocabulary tab_ci() speaks ("no"/"cell"/"diff").
# @param display_measure Scalar: the comparison the `display` template's primary token names
#   ("difference"/"ratio"/"odds_ratio"), or NA. The SECOND link of the comparison chain (see the
#   body): `color` names it, else `display` does, else it is the difference.
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
# @return list(color, chi2, ci, ci_scale, or_ci, comparison, color_signif, stars, totrow,
#   cache_keys). `color` is the RESOLVED measure over row_vars; every consumer derives its own need
#   from it through the MEASURES accessors. `comparison` is THE geometry this table compares on
#   (Phase 19d), `or_ci` says the LEAF owns the interval (the Woolf log-OR one) rather than tab_ci(),
#   `ci_scale` ("diff"/"ratio") the scale of tab_ci()'s own. `color_signif`/`stars` come back because
#   `ci = "cell"` DISABLES them (D28), and that ruling has to reach the build. `cache_keys` = the symbolic key material for the persisted jmvtab cache tiers
#   0-2 (dev/tabxplor_jmvtab_cache_design.md §3); the tier-2 shaped-aggregate hash + population
#   hashes are added by the module (Phase 7e).
# @keywords internal
# @noRd
tab_resolve_settings <- function(color, ci, chi2, ref, pct_vect, col_vars_text,
                                 display_measure = NA_character_,
                                 totrow = NULL, color_signif = "ignore",
                                 color_ratio_ci = FALSE, stars = FALSE,
                                 na = "keep", wt_name = character(),
                                 other_if_less_than = 0, comp = "tab",
                                 tab_vars = character(), row_vars = character(),
                                 col_vars = character(), filter_expr = NA_character_) {

  # Phase 19d: the PUBLIC `ci` vocabulary is the ANCHOR question and nothing else --
  # "auto" / "no" / "cell" / "ref". `"diff"` / `"ratio"` were geometries, and the geometry is
  # `color`'s to name (see `geom` below), so they are soft-deprecated onto "ref" here, at the one
  # boundary both producers share. `ci = "ratio"` stays LOSSLESS on the way through (it also pins the
  # ratio scale) so an existing call keeps its Katz bounds while the message teaches `color = "ratio"`.
  ci_ratio_req <- ci == "ratio"
  ci <- resolve_ci_value(ci)

  # Hoisted out of the `color = "auto"` case_when below, because the `color_signif` forcing needs the
  # SAME predicates and must run BEFORE it (see there).
  pct_rowcol <- purrr::map_lgl(pct_vect, ~ all(.[col_vars_text] %in% c("row", "col")))
  num_only   <- sum(col_vars_text) == 0
  # A MEAN column can always be compared to its reference row -- it needs no percentage base. So "can
  # this table carry a comparison interval" is per-column-KIND, not per-table: `pct_rowcol` answers for
  # the factor columns, `has_num` for the numeric ones. Collapsing the two (a table is "comparable"
  # only if its FACTOR columns are on row/col %) is what left `tab(..., sup_cols = <numeric>,
  # color_signif = ...)` with no interval and therefore every cell greyed.
  has_num    <- any(!col_vars_text)
  # Phase 19d: the odds ratio is computed on EVERY row/col-percentage table now, so "is this an OR
  # table" stopped being an input. `color = "auto"` therefore never resolves to the odds ratio: the
  # automatic reading of a percentage table is its difference, and an odds ratio is asked for by name.
  auto_or    <- rep(FALSE, length(pct_vect))

  # DESIGN: color = "auto" resolves from the pct settings of the FACTOR col_vars ONLY, through the
  # declared `auto_for` contexts: row/col percentages -> "difference"; counts / all-% -> "contrib".
  # A numeric-only table (no factor col_vars) keeps "auto" here and is resolved by tab_num() via
  # resolve_color_auto_num() (a mean has no contrib notion).
  # Phase 19c: WHICH measure answers a context is MEASURES' own `auto_for`, shared with the per-column
  # repaint (resolve_col_measures) and with tab_reg() -- one table for what used to be three cascades.
  # Phase 19c (defect): the assignment is SCOPED to the "auto" entries, so a per-row_var vector mixing
  # "auto" with an explicit measure no longer re-derives the explicit one from its `pct`.
  color_auto_text <- color == "auto" & ! num_only
  if (any(color_auto_text)) {
    context <- dplyr::case_when(
      auto_or    ~ "or_table",
      pct_rowcol ~ "pct",
      purrr::map_lgl(pct_vect, ~ all(.[col_vars_text] %in% c("", "no", "all", "all_tabs"))) ~ "counts",
      TRUE       ~ NA_character_
    )
    resolved <- vapply(context, function(cx)
      if (is.na(cx)) "no" else {
        m <- measure_auto(cx, "text"); if (nzchar(m)) m else "no"
      }, character(1), USE.NAMES = FALSE)
    color[color_auto_text] <- resolved[color_auto_text]
    # Phase 19d, the chain's SECOND link (study SS8.6 caveat 3): what the table SHOWS names the
    # comparison when the colour does not. A user who asks to see odds ratios and leaves the colour
    # automatic means "colour the odds ratios" -- and, below, gets odds-ratio stars and an odds-ratio
    # interval, which is D26 stated positively.
    if (!is.na(display_measure))
      color[color_auto_text & pct_rowcol] <- display_measure
  }

  # Phase 19d (KEY 8a): THE comparison this table makes, resolved ONCE, as a declared chain --
  # `color`'s text channel -> `display`'s primary token -> the difference. Everything that used to
  # ask the question separately (the colour gate, the stars, the interval geometry, the leaf's
  # odds-ratio branch) reads this one answer, which is what makes D26 unrepresentable: `stars` and
  # `color_signif` cannot disagree about what an odds-ratio table compares, because neither is asked.
  measure_of <- vapply(color, measure_key, character(1), USE.NAMES = FALSE)
  measure_of[is.na(measure_of)] <- ""
  if (!is.na(display_measure))
    measure_of[!nzchar(measure_of) & pct_rowcol] <- display_measure
  # `geom` = which of the three geometries owns the stored interval. "or" -> the Woolf log-OR bounds
  # the LEAF computes (ci_type/scale `odds_ratio`); "ratio" -> Katz / ratio-of-means; "diff" -> the
  # difference methods. `color_ratio_ci` says the two-channel spec put the ratio on the TEXT channel
  # (the measure the reader sees owns the interval); a deprecated `ci = "ratio"` pins it directly.
  geom <- measure_geometry(measure_of, color_ratio_ci, ci_ratio_req)

  # Phase 19d (D28): `ci = "cell"` and the significance machinery, from ONE rule. The cell interval
  # answers "how precise is this 26 %", which no comparison can be tested against -- so `stars` and
  # `color_signif` are INFORMED AND DISABLED (maintainer's ruling), where before `color_signif`
  # aborted and `stars` was silently dropped: two consumers of one fact, two behaviours.
  signif_on <- !identical(color_signif, "ignore") && !is.na(color_signif[1])
  d <- ci_disable_signif(ci, color_signif, stars)
  color_signif <- d$color_signif ; stars <- d$stars
  signif_on <- !identical(color_signif, "ignore") && !is.na(color_signif)

  # WHERE the interval sits. A measure declares `requires["ci"] == "gated"` = "only when a policy is
  # in force, since that is what reads the interval"; `stars` reads the same interval's p-value; and
  # `ci = "ref"` is the explicit opt-in (a forest plot with bounds but no colour gating). `ci = "auto"`
  # -- the default -- is exactly that union, promoted from a hidden forcing cascade to a documented
  # value. contrib declares no `ci` requirement (it has no interval at all).
  # WARNING: on a NUMERIC-ONLY table `color` is still the unresolved "auto"; its measure is the one
  # tab_num()/resolve_color_auto_num() will pick, so the requirement is read off THAT.
  gate_measure <- dplyr::if_else(color == "auto", measure_auto("num", "text"), color)
  gated <- signif_on &
    vapply(gate_measure, measure_forces, logical(1), "ci", TRUE, USE.NAMES = FALSE)
  # a comparison interval needs a comparison: a reference, and columns that can carry one.
  can_compare <- (num_only | pct_rowcol | has_num) & !(ref %in% c("no", "") | is.na(ref))
  # ONLY "auto" resolves. An explicit "no" is the user's answer to the anchor question and stands --
  # ci_disable_signif() above has already turned off whatever wanted to read an interval, so `gated`
  # and `stars` are FALSE by here and the two resolvers agree by construction.
  want_ref <- (gated | isTRUE(stars)) & can_compare
  was_auto <- ci == "auto"
  ci[was_auto] <- "no"
  ci[want_ref & was_auto] <- "ref"
  # nothing to anchor a reference interval to -> say so rather than silently computing nothing.
  ci[ci == "ref" & !can_compare] <- "no"

  # WARNING: contrib colouring paints the signed chi2 residual, which needs (a) total rows to
  # store each cell's contribution to variance and (b) a chi2 pass -- its declared
  # `requires = c(chi2 = "always", totrow = "always")`. The totrow half is skipped for callers that
  # pass totrow = NULL (tab_counts drives totals via `tot`).
  needs_totrow <- vapply(color, measure_forces, logical(1), "totrow", USE.NAMES = FALSE)
  needs_chi2   <- vapply(color, measure_forces, logical(1), "chi2",   USE.NAMES = FALSE)
  if (!is.null(totrow)) {
    ctr_no_row <- needs_totrow & totrow == FALSE
    if (any(ctr_no_row)) {
      warning("total rows were added, since color == 'contrib' needs them ",
              "to store information about mean contributions to variance")
      totrow[ctr_no_row] <- TRUE
    }
  }
  chi2[needs_chi2 & chi2 == FALSE] <- TRUE

  # DESIGN: a comparison colour compares each cell to a reference row/column, so `ref` is mandatory --
  # the measure's declared `requires["ref"] == "always"`. Phase 19d: the odds ratio declares it too,
  # so its silent leaf warn-and-repair became this one abort.
  if (any(vapply(color, measure_forces, logical(1), "ref", USE.NAMES = FALSE) &
          (ref %in% c("no", "") | is.na(ref)))) {
    cli::cli_abort(c(
      "With a comparison {.arg color}, {.arg ref} must be provided.",
      "i" = "{.code color = \"difference\"} / {.code \"ratio\"} / {.code \"odds_ratio\"} compare each cell to a reference."
    ))
  }

  # Phase 19d: the resolved `ci` splits into what each producer must do. The LEAF computes the Woolf
  # log-OR interval when the comparison is the odds ratio (`or_ci`); tab_ci() computes the cell or the
  # difference/ratio one otherwise. They are mutually exclusive by construction -- one cell, one
  # interval -- which is the whole reason the geometry had to be resolved before either was asked.
  or_ci    <- geom == "or" & ci == "ref"
  ci       <- dplyr::case_when(or_ci ~ "no", ci == "ref" ~ "diff", TRUE ~ ci)
  # Which SCALE the cell-vs-reference interval is expressed on: Katz log-RR bounds (neutral 1) when
  # the reader sees the ratio, the difference methods (neutral 0) otherwise. Only where a difference
  # CI is computed at all -- `ci = "cell"` is a one-proportion interval with no reference.
  # ... and only WHERE one is built: `ci` is per row_var, `geom` follows `color` (scalar unless the
  # caller varied it), so the two are recycled together and an entry with no reference interval
  # ("cell"/"no") keeps the neutral "diff".
  ci_scale <- ifelse(vctrs::vec_recycle(geom, length(ci)) == "ratio" & ci == "diff", "ratio", "diff")

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

  list(color = color, chi2 = chi2, ci = ci, ci_scale = ci_scale, or_ci = or_ci,
       comparison = measure_of, color_signif = color_signif, stars = stars, totrow = totrow,
       cache_keys = cache_keys)
}

# resolve_ci_value() -- Phase 19d (KEY 8a): THE `ci` argument vocabulary, in one place.
#
# `ci` asks WHERE the interval sits, and only that:
#   "auto"  (the default) -- a reference interval when a comparison is being TESTED
#                            (`color_signif`, `stars`) or explicitly asked for; else none.
#   "no"    -- none.
#   "cell"  -- each cell's OWN interval (how precise is this 26 %?). Unchanged since 1.x, and the
#              one value that never moved.
#   "ref"   -- the interval of the comparison this table makes. WHICH comparison is `color`'s to
#              name (study SS8.3: it already decided it correctly for all three geometries).
# `"diff"` / `"ratio"` were geometries wearing an anchor's name, and the second was a pure duplicate
# of `color = "ratio"`. Both soft-deprecate onto "ref"; the caller keeps `ci == "ratio"` separately
# so the deprecation stays LOSSLESS (it still pins the Katz scale) while the message teaches the
# replacement.
# TAB_CI_STEP_VALUES -- the vocabulary of the superseded STEP `tab_ci(ci = )`, declared beside the
# public one it deliberately differs from. `"diff"` is the step's own computational word (the
# pipeline hands it that value), so it carries no deprecation here even though the same spelling is
# soft-deprecated on the ANCHOR surface above; `"ref"` is its anchor synonym, `"ratio"` additionally
# pins the Katz scale. Phase 19i: one declared list instead of a hand-written stopifnot whose
# contents no message ever named.
#' @keywords internal
#' @noRd
TAB_CI_STEP_VALUES <- c("auto", "no", "cell", "diff", "ratio", "ref")

#' @keywords internal
#' @noRd
resolve_ci_value <- function(ci, user_env = rlang::caller_env(2)) {
  ci <- as.character(ci)
  ci[is.na(ci) | ci %in% c("", "FALSE")] <- "no"
  old <- ci %in% c("diff", "ratio")
  if (any(old)) {
    lifecycle::deprecate_soft(
      "2.0.0", I(paste0("tab(ci = \"", unique(ci[old])[1], "\")")),
      with = I(if (any(ci == "ratio")) "tab(ci = \"ref\", color = \"ratio\")" else "tab(ci = \"ref\")"),
      details = "`ci` says WHERE the interval sits; WHICH comparison it measures comes from `color`.",
      user_env = user_env)
    ci[old] <- "ref"
  }
  bad <- !ci %in% c("auto", "no", "cell", "ref")
  if (any(bad)) {
    cli::cli_abort(c("Unknown {.arg ci} value {.val {unique(ci[bad])}}.",
                     "i" = 'Valid: {.val {c("auto", "no", "cell", "ref")}}.'))
  }
  ci
}

# measure_geometry() -- Phase 19d/19e: WHICH of the three geometries owns a table's stored interval,
# given the comparison it makes. "or" -> the Woolf log-OR bounds the LEAF computes; "ratio" -> Katz /
# ratio-of-means; "diff" -> the difference methods. Stated ONCE because the jamovi cache tuple must
# agree with the pipeline about it (a diff <-> ratio toggle changes the interval, so it cannot be a
# cache re-paint). `color_ratio_ci` says the two-channel spec put the ratio on the TEXT channel (the
# measure the reader sees owns the interval); `ci_ratio_req` is a deprecated `ci = "ratio"` pinning it.
#' @keywords internal
#' @noRd
measure_geometry <- function(measure, color_ratio_ci = FALSE, ci_ratio_req = FALSE) {
  ifelse(measure == "odds_ratio", "or",
  ifelse(measure == "ratio" | isTRUE(color_ratio_ci) | ci_ratio_req, "ratio", "diff"))
}

# ci_disable_signif() -- Phase 19d (D28), THE single statement of the rule, so its three consumers
# (the pipeline resolver, the leaf resolver, and tab()'s own argument boundary -- which must apply it
# too, because the stored `color_signif` attribute is written from the color SPEC, not from what the
# resolver decided) cannot drift. `ci` is the ANCHOR question; `stars` and `color_signif` READ the
# interval it anchors. So the two `ci` values that leave nothing to read INFORM and DISABLE them:
#   "cell" -- the interval answers "how precise is this 26 %", which no comparison can be tested against
#   "no"   -- there is no interval at all
# Before, `color_signif` aborted on "cell" and `stars` was silently dropped -- two consumers of one
# fact, two behaviours. Idempotent, so the boundary applying it first silences the resolvers.
#
# Phase 19d-tail: "no" joined "cell" here, which is what makes the rule ONE rule. It used to be
# answered the opposite way, and in two places that disagreed: the pipeline resolver silently
# UPGRADED an explicit `ci = "no"` to "ref" whenever stars/gating wanted an interval, while the leaf
# resolver upgraded only "auto". Measured consequence: `tab(ci = "no", stars = TRUE)` carried a
# difference interval that `tab_plain(ci = "no", stars = TRUE)` did not, and the jamovi tier-3 tuple
# recorded `ci = "no"` for a carrier that held a reference-DEPENDENT interval -- so a reference toggle
# re-ref'd everything except the bounds, and the cached table kept the old reference's CI and
# p-values. Overruling what the user typed explicitly was the root of it; the anchor wins now.
#' @keywords internal
#' @noRd
CI_NO_INTERVAL_TO_TEST <- c("cell", "no")

#' @keywords internal
#' @noRd
ci_disable_signif <- function(ci, color_signif = "ignore", stars = FALSE) {
  out <- list(color_signif = color_signif, stars = stars)
  signif_on <- length(color_signif) > 0L && !is.na(color_signif[1]) &&
    !identical(color_signif[1], "ignore")
  hit <- intersect(CI_NO_INTERVAL_TO_TEST, ci[!is.na(ci)])
  if (length(hit) == 0L || !(signif_on || isTRUE(stars))) return(out)
  why <- if ("cell" %in% hit)
    gettext("stores each cell's own interval, so there is nothing to test a comparison against")
  else gettext("computes no interval, so there is nothing for a significance test to read")
  cli::cli_inform(c(
    "i" = paste0("{.code ci = \"", hit[[1]], "\"} ", why,
                 ": {.arg stars} and {.arg color_signif} are disabled here."),
    "i" = "Use {.code ci = \"ref\"} to test each cell against its reference."
  ))
  list(color_signif = "ignore", stars = FALSE)
}

# display_comparison() -- Phase 19d: which comparison a `display` template NAMES, read from its
# PRIMARY token. The second link of the comparison chain (`color` -> `display` -> the difference):
# a user who asks to SEE odds ratios and leaves the colour automatic gets odds-ratio colours, stars
# and interval. NA = the template names no comparison (or there is no template).
# It is deliberately the PRIMARY token only: "{or} ({pct})" is an odds-ratio cell annotated with a
# percentage, not a percentage cell.
#' @keywords internal
#' @noRd
DISPLAY_COMPARISON <- c(or = "odds_ratio", ratio = "ratio", diff = "difference")

#' @keywords internal
#' @noRd
display_comparison <- function(display) {
  if (is.null(display) || length(display) == 0L) return(NA_character_)
  d <- display[[1]]
  if (is.na(d) || !nzchar(d) || d %in% c("no", "auto", "num_ci")) return(NA_character_)
  tok <- tryCatch(parse_display_template(validate_display_template(d))$fields[1],
                  error = function(e) NA_character_)
  if (length(tok) == 0L || is.na(tok)) return(NA_character_)
  unname(DISPLAY_COMPARISON[tok] %||% NA_character_)
}


# resolve_leaf_ci() -- Phase 19d: the SAME three rules the pipeline resolver applies (D28's
# "a ci with nothing to test informs and disables", the measure's `requires["ci"] == "gated"`, and "a
# reference interval needs a reference"), for a leaf called DIRECTLY. Only "auto" resolves here, and
# that is now the pipeline's rule too -- until the 19d tail, tab_resolve_settings() ALSO upgraded an
# explicit "no", so the same call built an interval through tab() and none through tab_plain(). This
# resolver was the correct half; the fix was to stop the other one overruling the user. This is what closes D29: 14a applied the
# gated forcing inside tab_resolve_settings() only, so `tab_num(color = "diff", color_signif =
# "grey_non_signif")` with no explicit `ci` computed no interval and the policy greyed every cell.
# Returns the ANCHOR value ("no"/"cell"/"ref"); each leaf maps it to what it must compute.
#' @keywords internal
#' @noRd
resolve_leaf_ci <- function(ci, color, color_signif = "ignore", stars = FALSE, ref = "tot") {
  ci        <- resolve_ci_value(if (is.null(ci)) "auto" else ci)[1]
  d         <- ci_disable_signif(ci, color_signif, stars)
  color_signif <- d$color_signif ; stars <- d$stars
  signif_on <- !identical(color_signif[1], "ignore") && !is.na(color_signif[1])
  can_compare <- !(ref[1] %in% c("no", "")) && !is.na(ref[1])
  gated <- signif_on && measure_forces(color, "ci", TRUE)
  if (identical(ci, "auto")) ci <- if ((gated || isTRUE(stars)) && can_compare) "ref" else "no"
  if (identical(ci, "ref") && !can_compare) ci <- "no"
  list(ci = ci, stars = isTRUE(stars),
       color_signif = if (signif_on) color_signif[1] else "ignore")
}

# tab_leaf_comparison() -- Phase 19d: the comparison chain, for a leaf called DIRECTLY (the
# superseded tab_plain() / tab_num(), which have no settings spine to read it off). Same order as
# the resolver's: `color`'s measure -> `display`'s primary token -> the difference. "" when the
# column can carry no comparison at all.
#' @keywords internal
#' @noRd
tab_leaf_comparison <- function(color, display, pct, ref) {
  if (!pct[1] %in% c("row", "col") || ref[1] %in% c("no", "") || is.na(ref[1])) return("")
  k <- measure_key(color[1])
  if (!is.na(k) && nzchar(k) && k != "contrib") return(k)
  d <- display_comparison(display)
  if (!is.na(d)) return(d)
  ""
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
# colour nothing.
#
# Phase 19c: it no longer emits `"after_ci"`. That was the numeric twin of the factor cascade's own
# manufactured composite -- and here it was not merely redundant, it was a live defect on BOTH of the
# two paths that reach it with `color` still spelled `"auto"` (the string, not `TRUE`):
#   * `tab_num(color = "auto", ci = "diff")` stored `"after_ci"` in the `color` ATTRIBUTE, which
#     fmt_color_plan() cannot match against names(MEASURES) -> it returned NULL and the table came
#     out entirely UNCOLOURED (measured: every slot 0);
#   * with a `color_signif` policy, the per-column repaint then handed the unresolved sentinel to
#     set_color(), which ABORTED ("Unknown color measure").
# The two arms also collapse into one: `ci = "diff"` implies `ci != "cell"`, so they only ever
# differed in which of the two spellings of the same measure they returned.
#
# @param color   Scalar colour measure for this (single) numeric row_var, or the "auto" sentinel.
# @param ref,ci  Scalars for this row_var (`ci` may be NULL at this stage).
# @param row_var,col_vars  Character name(s), used only to detect the synthetic placeholder
#   axes tab() injects when a row_var / col_var is absent.
# @return The resolved scalar colour ("diff" / "" / passed-through).
# @keywords internal
# @noRd
resolve_color_auto_num <- function(color, ref, ci, row_var, col_vars) {
  if (is_placeholder_var(row_var) || any(is_placeholder_var(col_vars))) return("")
  ci_cell <- if (!is.null(ci)) ci == "cell" else FALSE
  dplyr::case_when(
    # the numeric pipeline measure is the diff BUILD class (num_core computes the difference fields);
    # WHICH of that class's measures is finally shown is the per-column repaint's answer ("ratio").
    color == "auto" & !ref %in% c("no", "") & !ci_cell ~ measure_of_build("diff"),
    color == "auto"                                    ~ "",
    TRUE                                               ~ color
  )
}


# === THE ARGUMENT BOUNDARY (Phase 19i) ==========================================================
# Five entry points -- tab() / tab_many() / tab_plain() / tab_num() / tab_counts(), plus the jamovi
# one -- used to re-implement the same boundary by hand, and had drifted: `tot`'s "both" expansion
# was written four times (one of them differently), `total_names`'s recycling four times, `na`'s
# allow-list three times with three contents, `pct`'s vocabulary three times (tab_counts checked the
# SIZE only), and `totaltab` / `n_min` / `conf_level` were validated nowhere at all -- so
# `tab(totaltab = "tabel")` silently meant "no total table".
#
# TAB_ARG_VALUES is the vocabulary as DATA: one entry per argument, `values` (what tab()/tab_many()
# accept), `leaf` (the restricted set for the leaves and tab_counts, NULL = same), `size` (1L, or NA
# for a per-col_var vector) and `na_ok`. Adding a value is one edit, and no two producers can
# disagree about what a word means.
#
# NOT here, deliberately: `ci`. Its vocabulary carries a soft-deprecation (`"diff"`/`"ratio"` ->
# `"ref"`), so validating it means RESOLVING it, and that is resolve_ci_value()'s job -- called by
# every producer's own resolver (tab_resolve_settings / resolve_leaf_ci / tab_ci). One validator, in
# the one place that can also rewrite the value.
#' @keywords internal
#' @noRd
TAB_ARG_VALUES <- list(
  pct      = list(values = c("no", "row", "col", "all", "all_tabs"),   leaf = NULL, size = NA,  na_ok = TRUE),
  na       = list(values = c("keep", "drop", "drop_all", "common_base"),
                  leaf = c("keep", "drop"),                                         size = 1L,  na_ok = FALSE),
  levels   = list(values = c("all", "first", "auto"),                  leaf = NULL, size = NA,  na_ok = FALSE),
  comp     = list(values = c("tab", "all", ""),                        leaf = NULL, size = 1L,  na_ok = TRUE),
  tot      = list(values = c("row", "col", "both", "no", ""),          leaf = NULL, size = NA,  na_ok = FALSE),
  totaltab = list(values = c("line", "table", "no", ""),               leaf = NULL, size = 1L,  na_ok = FALSE),
  totcol   = list(values = c("last", "each", "all_col_vars", "no", ""), leaf = NULL, size = 1L, na_ok = FALSE),
  output   = list(values = c("single", "list"),                        leaf = NULL, size = 1L,  na_ok = FALSE),
  # Phase 19k: `anova` -- which one-way F a mean col_var's p-value line shows. NULL never reaches
  # here (an unsupplied argument is not checked), and NULL is what means "the global option".
  anova    = list(values = c("welch", "classic"),                      leaf = NULL, size = 1L,  na_ok = FALSE)
)

# tab_validate_args() -- check the supplied arguments against TAB_ARG_VALUES, aborting on the first
# unknown value with the valid list in the message. Arguments not supplied are not checked; `fn`
# selects the full or the leaf vocabulary. The numeric arguments are checked here too, beside the
# vocabularies, because "what may this argument be" is one question.
#' @keywords internal
#' @noRd
tab_validate_args <- function(fn = "tab", ..., conf_level = NULL, n_min = NULL) {
  args <- list(...)
  full <- fn %in% c("tab", "tab_many")
  for (nm in intersect(names(args), names(TAB_ARG_VALUES))) {
    v <- args[[nm]]
    if (is.null(v)) next
    spec <- TAB_ARG_VALUES[[nm]]
    ok   <- if (!full && !is.null(spec$leaf)) spec$leaf else spec$values
    # a LIST is a shape error, not a vocabulary one: as.character() would turn it into a deparsed
    # string and report an "unknown value" that no vocabulary could ever contain. The producer that
    # accepts a list on some axis says so itself, in its own words (see tab()'s `pct`).
    if (is.list(v)) next
    if (!is.na(spec$size) && length(v) != spec$size)
      cli::cli_abort(c("{.arg {nm}} must be a single value in {.fn {fn}}.",
                       "i" = "Got {length(v)}."), call = NULL)
    v <- as.character(v)
    bad <- !v %in% ok & !(isTRUE(spec$na_ok) & is.na(v))
    if (any(bad))
      cli::cli_abort(c("Unknown {.arg {nm}} value {.val {unique(v[bad])}}.",
                       "i" = "Valid: {.val {ok}}."), call = NULL)
  }
  # A confidence LEVEL is a probability. `conf_level = 95` used to reach the interval engine, where
  # `stopifnot(conf_level <= 1)` fired -- but only if an interval was actually computed, so on most
  # tables it was silently taken as 95 %'s complement or worse.
  if (!is.null(conf_level)) {
    if (length(conf_level) != 1L || !is.numeric(conf_level) || is.na(conf_level) ||
        conf_level <= 0 || conf_level >= 1)
      cli::cli_abort(c("{.arg conf_level} must be a single probability strictly between 0 and 1.",
                       "i" = if (is.numeric(conf_level) && length(conf_level) == 1L &&
                                 !is.na(conf_level) && conf_level > 1)
                         "Got {conf_level}; did you mean {conf_level / 100}?"
                       else "Got {.val {conf_level}}."), call = NULL)
  }
  if (!is.null(n_min)) {
    if (length(n_min) != 1L || !is.numeric(n_min) || is.na(n_min) || n_min < 0)
      cli::cli_abort(c("{.arg n_min} must be a single non-negative number (0 = off).",
                       "i" = "Got {.val {n_min}}."), call = NULL)
  }
  invisible(TRUE)
}


# tab_resolve_common_args() -- THE argument boundary, run once per call, by every producer.
#
# It validates first (tab_validate_args) and derives second, in the order tab()'s own boundary
# proved correct. WARNING (19c): the colour spec must be DECODED before it is normalised --
# normalize_color_spec() does both, in that order; never split them.
#
# Every argument is optional: a producer passes what it has, and reads back the subset it needs.
# `missing()` rather than a NULL default, because several of these arguments mean something
# specific when NULL (`stars = NULL` = "read the option", `cleannames = NULL` likewise).
#
# @param fn  the producer's name, for the messages AND for the leaf-vs-full vocabularies.
# @return a named list holding only what was supplied, resolved:
#   test         `chi2` folded in, then svy_check_test()'d to a plain logical
#   cleannames, stars, ci_method   the three "NULL -> option / named-vector" resolutions
#   color_spec   the parsed two-channel spec, its policy already subject to D28
#   stars        likewise disabled when `ci` anchors nothing to test
#   display, ref, ref2             the retired `OR` argument's route
#   tot          VALIDATED but not expanded -- "both" means c("row","col") to tab()/tab_counts()
#                and "row" to the numeric leaf, so each expands it itself, next to its own totals
#   totrow, totcol                 the (row, col) translation tab() and tab_counts() share
#   total_names  recycled to 2
#' @keywords internal
#' @noRd
tab_resolve_common_args <- function(fn = "tab",
                                    test, chi2, color, color_signif, ci, stars, conf_level,
                                    ci_method, method_cell, method_diff, cleannames,
                                    OR, display, ref, ref2, tot, total_names,
                                    na, levels, pct, comp, totaltab, totcol, output, n_min, anova,
                                    user_env = rlang::caller_env()) {
  out <- list()

  # 1. the renamed argument, folded before anything reads `test`.
  if (!missing(chi2) && lifecycle::is_present(chi2)) {
    lifecycle::deprecate_soft("2.0.0", I(paste0(fn, "(chi2 = )")), I(paste0(fn, "(test = )")),
                              user_env = user_env)
    test <- chi2
  }
  # `test` says only WHETHER to test; the BASIS (n / weights / design) is derived in tab_setup().
  if (!missing(test)) out$test <- svy_check_test(test)

  # 2. validation.
  tab_validate_args(
    fn,
    pct      = if (missing(pct))      NULL else pct,
    na       = if (missing(na))       NULL else na,
    levels   = if (missing(levels))   NULL else levels,
    comp     = if (missing(comp))     NULL else comp,
    tot      = if (missing(tot))      NULL else tot,
    totaltab = if (missing(totaltab)) NULL else totaltab,
    totcol   = if (missing(totcol))   NULL else totcol,
    output   = if (missing(output))   NULL else output,
    anova    = if (missing(anova))    NULL else anova,
    conf_level = if (missing(conf_level)) NULL else conf_level,
    n_min      = if (missing(n_min))      NULL else n_min
  )
  # the validated values pass straight through, so a caller reads ONE object.
  if (!missing(pct))        out$pct        <- pct
  if (!missing(na))         out$na         <- na
  if (!missing(levels))     out$levels     <- levels
  if (!missing(comp))       out$comp       <- comp
  if (!missing(totaltab))   out$totaltab   <- totaltab
  if (!missing(output))     out$output     <- output
  if (!missing(conf_level)) out$conf_level <- conf_level
  if (!missing(n_min))      out$n_min      <- n_min

  # 3. the three "NULL -> option" / named-vector resolutions.
  if (!missing(cleannames)) out$cleannames <- resolve_cleannames(cleannames)
  # `stars` is resolved HERE, at the boundary, and not four layers down: resolve_leaf_ci() tests
  # `isTRUE(stars)`, so tab_num()'s late resolution (inside num_core) meant
  # options(tabxplor.stars = TRUE) built a reference interval through tab_plain() and none through
  # tab_num(). One place, one timing.
  if (!missing(stars)) stars <- resolve_stars(stars)
  if (!missing(ci_method))
    out$ci_method <- resolve_ci_method(ci_method,
                                       if (missing(method_cell)) NULL else method_cell,
                                       if (missing(method_diff)) NULL else method_diff, fn)

  # 4. the retired `OR`, routed to what it was: a display, a 2x2 and a reference.
  if (!missing(OR)) {
    route   <- tab_deprecate_or(OR,
                                if (missing(display)) NULL else display,
                                if (missing(ref2))    "first" else ref2,
                                if (missing(ref))     "auto"  else ref)
    display <- route$display ; ref2 <- route$ref2 ; ref <- route$ref
  }
  if (!missing(display)) out$display <- display
  if (!missing(ref))     out$ref     <- ref
  if (!missing(ref2))    out$ref2    <- ref2

  # 5. the colour spec, then D28 on it. This must run on the SPEC, not on the resolver's copy: the
  # stored `color_signif` attribute is written from the spec by finalize_color_spec(), so a policy
  # the resolver silently disabled would still be stamped on every column -- the table claiming a
  # gate it does not apply. tab_counts() built and finalised a spec without ever applying the rule.
  if (!missing(color)) {
    spec <- normalize_color_spec(color, if (missing(color_signif)) "ignore" else color_signif)
    if (!missing(ci)) {
      off <- ci_disable_signif(ci, spec$signif, if (missing(stars)) FALSE else stars)
      spec$signif <- off$color_signif
      if (!missing(stars)) stars <- off$stars
    }
    out$color_spec <- spec
    out$color      <- spec$legacy
  }
  if (!missing(stars)) out$stars <- stars

  # 6. totals. `tot` comes back VALIDATED but NOT expanded (see @return).
  if (!missing(tot)) {
    out$tot    <- tot
    out$totrow <- "row" %in% tot || identical(tot[1], "both")
    out$totcol <- if ("col" %in% tot || identical(tot[1], "both")) "last" else "no"
  }
  if (!missing(total_names)) out$total_names <- vctrs::vec_recycle(total_names, 2)

  out
}
