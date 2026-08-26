# PURPOSE: THE argument boundary of the crosstab producers -- validation, then the settings cascade.
# ROLE: Two layers, in this file, in this order.
#   (1) tab_resolve_common_args() + tab_validate_args(): what every producer must do to its
#       arguments before any of them means anything -- the renames, the vocabularies, the sizes, the
#       "NULL -> option" resolutions, the colour spec, `tot` -> (totrow, totcol), and the four
#       synthetic labels. tab() / tab_plain() / tab_num() / tab_counts() all call it, so there is
#       ONE answer instead of one per producer. The vocabularies it checks are TAB_ARG_VALUES,
#       derived in R/tab-args.R from each argument's own declaration.
#   (2) tab_resolve_settings(): the pure, data-free CASCADE shared by tab_build() and tab_counts().
#       color = "auto" resolves to a concrete MEASURE through MEASURES' declared `auto_for`
#       contexts, and that measure's declared `requires` then applies to chi2 / totrow / ci / ref.
#       Each consumer asks the measure what it needs, so adding a measure touches no step here.
# KEY CONSTRAINTS:
#   - PURE function of (argument values, column CLASS metadata) -> settings. It never reads column
#     VALUES. This is the boundary the jamovi UI mirrors and the live cache keys on: a change in
#     these inputs is exactly what forces a recompute.
#   - VALIDATION happens first, in layer (1); the cascade may then assume its inputs are legal. `ci`
#     is the deliberate exception -- its vocabulary carries a soft-deprecation, so validating it
#     means REWRITING it, which is resolve_ci_value()'s job.
#   - WARNING: the colour spec must be DECODED before it is normalised. normalize_color_spec() does
#     both, in that order; never split them.
#   - Only "auto" resolves. An explicit "no" is the user's answer to the anchor question and stands.
#   - DATA-DEPENDENT resolution stays at the leaves, NOT here: `levels = "auto"` (needs the real
#     level count), a literal or regex `ref` (matched against built row labels), `na` dropping, and
#     the leaf's tot / totaltab forcing.
# See: CLAUDE.md § tabxplor architecture (the declarative architecture);
#      dev/tabxplor_argument_computation_map.md (the full argument -> computation map).

# @param  The row_var-wise argument vectors (`color`/`color_signif` already decoded and split by
#   normalize_color_spec(); `totrow = NULL` skips the contrib forcing), the per-col_var `pct` and
#   factor/numeric metadata, and the data-free cache-key material (values and variable NAMES).
# @return color, chi2, ci, ci_scale, or_ci, comparison, color_signif, stars, totrow, cache_keys.
tab_resolve_settings <- function(color, ci, chi2, ref, pct_vect, col_vars_text,
                                 display_measure = NA_character_,
                                 totrow = NULL, color_signif = "ignore",
                                 color_ratio_ci = FALSE, stars = FALSE,
                                 na = "keep", wt_name = character(),
                                 other_if_less_than = 0, comp = "tab",
                                 tab_vars = character(), row_vars = character(),
                                 col_vars = character(), filter_expr = NA_character_) {

  ci_ratio_req <- ci == "ratio"
  ci <- resolve_ci_value(ci, warn = FALSE)

  # DESIGN: hoisted out of the case_when below -- the `color_signif` forcing needs these too.
  pct_rowcol <- purrr::map_lgl(pct_vect, ~ all(.[col_vars_text] %in% c("row", "col")))
  num_only   <- sum(col_vars_text) == 0
  # DESIGN: "can this table carry a comparison interval" is per-column-KIND, not per-table: a MEAN
  # needs no percentage base, so folding `has_num` into `pct_rowcol` greyed numeric sup_cols.
  has_num    <- any(!col_vars_text)

  # DESIGN: "auto" reads the FACTOR col_vars' `pct` through MEASURES' `auto_for` (row/col % ->
  # difference, counts -> contrib), scoped to the "auto" entries; numeric-only is tab_num()'s.
  color_auto_text <- color == "auto" & ! num_only
  if (any(color_auto_text)) {
    context <- dplyr::case_when(
      pct_rowcol ~ "pct",
      purrr::map_lgl(pct_vect, ~ all(.[col_vars_text] %in% c("", "no", "all", "all_tabs"))) ~ "counts",
      TRUE       ~ NA_character_
    )
    resolved <- vapply(context, function(cx)
      if (is.na(cx)) "no" else {
        m <- measure_auto(cx, "text"); if (nzchar(m)) m else "no"
      }, character(1), USE.NAMES = FALSE)
    color[color_auto_text] <- resolved[color_auto_text]
    if (!is.na(display_measure))
      color[color_auto_text & pct_rowcol] <- display_measure
  }

  # DESIGN: THE comparison this table makes, resolved ONCE: `color`'s measure, else what `display`
  # shows, else the difference. Gate, stars, geometry and leaf all read this one answer.
  measure_of <- vapply(color, measure_key, character(1), USE.NAMES = FALSE)
  measure_of[is.na(measure_of)] <- ""
  if (!is.na(display_measure))
    measure_of[!nzchar(measure_of) & pct_rowcol] <- display_measure
  geom <- measure_geometry(measure_of, color_ratio_ci, ci_ratio_req)

  signif_on <- !identical(color_signif, "ignore") && !is.na(color_signif[1])
  d <- ci_disable_signif(ci, color_signif, stars)
  color_signif <- d$color_signif ; stars <- d$stars
  signif_on <- !identical(color_signif, "ignore") && !is.na(color_signif)

  # `ci = "auto"` = the union of a `requires["ci"] == "gated"` measure with a policy in force,
  # `stars`, and the explicit `ci = "ref"`; contrib has no interval at all. WARNING: on a
  # NUMERIC-ONLY table `color` is still "auto", so the requirement is read off tab_num()'s measure.
  gate_measure <- dplyr::if_else(color == "auto", measure_auto("num", "text"), color)
  gated <- signif_on &
    vapply(gate_measure, measure_forces, logical(1), "ci", TRUE, USE.NAMES = FALSE)
  can_compare <- (num_only | pct_rowcol | has_num) & !(ref %in% c("no", "") | is.na(ref))
  # Only "auto" resolves: an explicit "no" is the user's answer to the anchor question and stands.
  want_ref <- (gated | isTRUE(stars)) & can_compare
  was_auto <- ci == "auto"
  ci[was_auto] <- "no"
  ci[want_ref & was_auto] <- "ref"
  ci[ci == "ref" & !can_compare] <- "no"

  # WARNING: contrib paints the signed chi2 residual: `requires = c(chi2 = "always", totrow =
  # "always")`, total rows storing each contribution. Skipped when totrow = NULL (tab_counts).
  needs_totrow <- vapply(color, measure_forces, logical(1), "totrow", USE.NAMES = FALSE)
  needs_chi2   <- vapply(color, measure_forces, logical(1), "chi2",   USE.NAMES = FALSE)
  if (!is.null(totrow)) {
    ctr_no_row <- needs_totrow & totrow == FALSE
    totrow[ctr_no_row] <- TRUE
  }
  chi2[needs_chi2 & chi2 == FALSE] <- TRUE

  # A comparison colour compares to a reference row/column: `requires["ref"] == "always"`.
  if (any(vapply(color, measure_forces, logical(1), "ref", USE.NAMES = FALSE) &
          (ref %in% c("no", "") | is.na(ref)))) {
    cli::cli_abort(c(
      "With a comparison {.arg color}, {.arg ref} must be provided.",
      "i" = "{.code color = \"difference\"} / {.code \"ratio\"} / {.code \"odds_ratio\"} compare each cell to a reference."
    ))
  }

  # DESIGN: one cell, one interval -- the LEAF owns the Woolf log-OR one, tab_ci() the cell or the
  # difference/ratio one. `ci_scale` follows `geom` recycled over `ci`; no reference CI -> "diff".
  or_ci    <- geom == "or" & ci == "ref"
  ci       <- dplyr::case_when(or_ci ~ "no", ci == "ref" ~ "diff", TRUE ~ ci)
  ci_scale <- ifelse(vctrs::vec_recycle(geom, length(ci)) == "ratio" & ci == "diff", "ratio", "diff")

  cache_keys <- tab_cache_keys(na = na, wt_name = wt_name,
                               other_if_less_than = other_if_less_than, comp = comp,
                               tab_vars = tab_vars, row_vars = row_vars, col_vars = col_vars,
                               filter_expr = filter_expr)

  list(color = color, chi2 = chi2, ci = ci, ci_scale = ci_scale, or_ci = or_ci,
       comparison = measure_of, color_signif = color_signif, stars = stars, totrow = totrow,
       cache_keys = cache_keys)
}

# THE public `ci` vocabulary: WHERE the interval sits and only that -- "auto" (a reference interval
# when a comparison is tested, else none) / "no" / "cell" (each cell's own) / "ref" (the
# comparison's, which one being `color`'s to name). "diff"/"ratio" soft-deprecate onto "ref", the
# caller keeping `ci == "ratio"` so the deprecation stays lossless (it pins the Katz scale).
# TAB_CI_STEP_VALUES is the superseded STEP tab_ci()'s vocabulary: there "diff" is computational.
#' @keywords internal
#' @noRd
TAB_CI_STEP_VALUES <- c("auto", "no", "cell", "diff", "ratio", "ref")

#' @keywords internal
#' @noRd
# @param warn  FALSE when the caller already said the deprecation with the right `user_env`. The
#   REWRITE still happens here: `ci = "ratio"`'s second effect (the Katz scale) reads the RAW value.
resolve_ci_value <- function(ci, user_env = rlang::caller_env(2), warn = TRUE) {
  ci <- as.character(ci)
  ci[is.na(ci) | ci %in% c("", "FALSE")] <- "no"
  old <- ci %in% c("diff", "ratio")
  if (any(old) && isTRUE(warn)) {
    lifecycle::deprecate_soft(
      "2.0.0", I(paste0("tab(ci = \"", unique(ci[old])[1], "\")")),
      with = I(if (any(ci == "ratio")) "tab(ci = \"ref\", color = \"ratio\")" else "tab(ci = \"ref\")"),
      details = "`ci` says WHERE the interval sits; WHICH comparison it measures comes from `color`.",
      user_env = user_env)
  }
  ci[old] <- "ref"
  bad <- !ci %in% c("auto", "no", "cell", "ref")
  if (any(bad)) {
    cli::cli_abort(c("Unknown {.arg ci} value {.val {unique(ci[bad])}}.",
                     "i" = 'Valid: {.val {c("auto", "no", "cell", "ref")}}.'))
  }
  ci
}

# Which geometry owns the stored interval: "or" (Woolf log-OR, built by the leaf) / "ratio" (Katz) /
# "diff". Stated once because the jamovi cache tuple must agree -- a toggle is not a re-paint.
#' @keywords internal
#' @noRd
measure_geometry <- function(measure, color_ratio_ci = FALSE, ci_ratio_req = FALSE) {
  ifelse(measure == "odds_ratio", "or",
  ifelse(measure == "ratio" | isTRUE(color_ratio_ci) | ci_ratio_req, "ratio", "diff"))
}

# `stars`/`color_signif` READ the interval `ci` anchors, so values with nothing to read ("cell" --
# precision, not comparison; "no") inform and disable both. Idempotent, so it may run twice.
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
  tx_inform_once(paste0("ci_no_test_", hit[[1]]), c(
    "i" = paste0("{.code ci = \"", hit[[1]], "\"} ", why,
                 ": {.arg stars} and {.arg color_signif} are off."),
    "i" = '{.code ci = "ref"} tests each cell against its reference.'))
  list(color_signif = "ignore", stars = FALSE)
}

# Deliberately the PRIMARY token only: "{or} ({pct})" is an odds-ratio cell annotated with a pct.
#' @keywords internal
#' @noRd
display_comparison <- function(display) {
  d <- tryCatch(display_resolve(display), error = function(e) NULL)
  if (is.null(d)) return(NA_character_)
  tok <- parse_display_template(d)$fields[1]
  if (length(tok) == 0L || is.na(tok)) return(NA_character_)
  unname(DISPLAY_COMPARISON[tok] %||% NA_character_)
}


# The same rules as the cascade above, for a leaf called DIRECTLY: only "auto" resolves here too.
#' @keywords internal
#' @noRd
resolve_leaf_ci <- function(ci, color, color_signif = "ignore", stars = FALSE, ref = "tot") {
  ci        <- resolve_ci_value(if (is.null(ci)) "auto" else ci, warn = FALSE)[1]
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

# The symbolic (data-free) cache-key material the jamovi `.js` mirrors, computed in this one place.
tab_cache_keys <- function(na = "keep", wt_name = character(), other_if_less_than = 0,
                           comp = "tab", tab_vars = character(), row_vars = character(),
                           col_vars = character(), filter_expr = NA_character_) {
  row_vars <- vars_chr(row_vars)
  col_vars <- vars_chr(col_vars)
  tab_vars <- vars_chr(tab_vars)
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


# Numeric (means) arm of color = "auto": a mean has no contrib / OR notion, so it keys only on
# whether a difference is possible (a real `ref`, `ci` not "cell"); placeholder axes colour nothing.
resolve_color_auto_num <- function(color, ref, ci, row_var, col_vars) {
  if (is_placeholder_var(row_var) || any(is_placeholder_var(col_vars))) return("")
  ci_cell <- if (!is.null(ci)) ci == "cell" else FALSE
  dplyr::case_when(
    # the diff BUILD class: WHICH of its measures is shown is the per-column repaint's answer.
    color == "auto" & !ref %in% c("no", "") & !ci_cell ~ measure_of_build("diff"),
    color == "auto"                                    ~ "",
    TRUE                                               ~ color
  )
}


# === THE ARGUMENT BOUNDARY ======================================================================
# TAB_ARG_VALUES (R/tab-args.R, derived from TAB_ARGS) is the vocabulary as DATA -- accepted values,
# the `leaf` subset, `size`, `na_ok` -- so no two producers can disagree about a word. NOT declared
# there, deliberately: `ci`, whose soft-deprecation makes validating it REWRITING it, and whose
# message must name the `user_env` only this boundary knows. It is idempotent, so the resolvers,
# reachable without the boundary, safely call it again.

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
    # WARNING: a LIST is a SHAPE error, not a vocabulary one -- as.character() would deparse it and
    # report an "unknown value" no vocabulary could hold; a list-accepting producer says so itself.
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
  # A confidence LEVEL is a probability: `conf_level = 95` otherwise only fails deep in the interval
  # engine, and only when an interval is computed at all -- silently wrong everywhere else.
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


# Validates first, derives second (the numbered steps are that order). Every argument is optional: a
# producer passes what it has and reads back what it needs. `missing()` rather than a NULL default,
# because several of these mean something specific when NULL (`stars = NULL` = "read the option").
#' @keywords internal
#' @noRd
tab_resolve_common_args <- function(fn = "tab",
                                    test, chi2, color, color_signif, ci, stars, conf_level,
                                    ci_method, method_cell, method_diff, cleannames,
                                    OR, display, ref, ref2, tot,
                                    total_names, totaltab_name, other_level,
                                    na, levels, pct, comp, totaltab, totcol, output, n_min, anova,
                                    n, add_n,
                                    user_env = rlang::caller_env()) {
  out <- list()

  # 1. the renamed argument, folded before anything reads `test` (which says only WHETHER to test --
  # the basis, n / weights / design, is derived in tab_setup()).
  if (!missing(chi2) && lifecycle::is_present(chi2)) {
    lifecycle::deprecate_soft("2.0.0", I(paste0(fn, "(chi2 = )")), I(paste0(fn, "(test = )")),
                              user_env = user_env)
    test <- chi2
  }
  if (!missing(test)) out$test <- svy_check_test(test)

  # 2. validation, then the validated values straight through.
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
  if (!missing(pct))        out$pct        <- pct
  if (!missing(na))         out$na         <- na
  if (!missing(levels))     out$levels     <- levels
  if (!missing(comp))       out$comp       <- comp
  if (!missing(totaltab))   out$totaltab   <- totaltab
  if (!missing(output))     out$output     <- output
  if (!missing(conf_level)) out$conf_level <- conf_level %||% conf_level_default()
  if (!missing(n_min))      out$n_min      <- n_min

  # 3. the "NULL -> option" resolutions. DESIGN: `stars` resolves HERE, not four layers down: it
  # gates resolve_leaf_ci(): a late one built a reference CI in tab_plain() but none in tab_num().
  if (!missing(cleannames)) out$cleannames <- resolve_cleannames(cleannames)
  if (!missing(stars)) stars <- resolve_stars(stars)
  if (!missing(ci_method))
    out$ci_method <- resolve_ci_method(ci_method,
                                       if (missing(method_cell)) NULL else method_cell,
                                       if (missing(method_diff)) NULL else method_diff, fn,
                                       user_env = user_env)

  # 3b. `ci` -- SAID here (only this boundary knows the `user_env` the message must name), REWRITTEN
  # downstream, because `ci = "ratio"` also pins the Katz scale and the resolvers read the raw word.
  if (!missing(ci)) invisible(resolve_ci_value(ci, user_env = user_env))

  # 4. the retired `OR`, routed to what it was: a display, a 2x2 and a reference.
  if (!missing(OR)) {
    route   <- tab_deprecate_or(OR,
                                if (missing(display)) NULL else display,
                                if (missing(ref2))    "first" else ref2,
                                if (missing(ref))     "auto"  else ref,
                                user_env = user_env)
    display <- route$display ; ref2 <- route$ref2 ; ref <- route$ref
  }
  if (!missing(display)) out$display <- display
  if (!missing(ref))     out$ref     <- ref
  if (!missing(ref2))    out$ref2    <- ref2

  # 5. the colour spec, then the "nothing to test" rule ON THE SPEC, not on the resolver's copy:
  # finalize_color_spec() stamps the stored `color_signif` attribute from the spec, so a policy the
  # resolver silently disabled would still claim a gate the table does not apply.
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

  # 6. totals. `tot` comes back VALIDATED but NOT expanded: "both" means c("row", "col") to
  # tab()/tab_counts() and "row" to the numeric leaf, so each expands it beside its own totals.
  if (!missing(tot)) {
    out$tot    <- tot
    out$totrow <- "row" %in% tot || identical(tot[1], "both")
    out$totcol <- if ("col" %in% tot || identical(tot[1], "both")) "last" else "no"
  }
  # 7. the four synthetic labels from the option; the three released arguments win where given.
  lbl <- tab_total_names()
  if (!missing(total_names) && !is.null(total_names)) {
    tab_deprecate_total_label(fn, "total_names", user_env)
    lbl[c("row", "col")] <- vctrs::vec_recycle(as.character(total_names), 2)
  }
  if (!missing(totaltab_name) && !is.null(totaltab_name)) {
    tab_deprecate_total_label(fn, "totaltab_name", user_env)
    lbl[["tab"]] <- as.character(totaltab_name)[[1]]
  }
  if (!missing(other_level) && !is.null(other_level)) {
    tab_deprecate_total_label(fn, "other_level", user_env)
    lbl[["other"]] <- as.character(other_level)[[1]]
  }
  out$total_names   <- unname(lbl[c("row", "col")])
  out$totaltab_name <- unname(lbl[["tab"]])
  out$other_level   <- unname(lbl[["other"]])

  # 8. the base count. One MODE, resolved here so no consumer re-derives it; the deprecated logical
  # only ever said "off", which is the "no" mode.
  if (!missing(n) || !missing(add_n)) {
    base_n <- if (!missing(n) && !is.null(n)) as.character(n)[[1]] else tx_option("n")
    if (!missing(add_n) && !is.null(add_n)) {
      lifecycle::deprecate_soft(
        "2.0.0", I(paste0(fn, "(add_n = )")), I(paste0(fn, '(n = "no")')),
        details = paste0("The base count is a display choice now, with a global twin: ",
                         "options(tabxplor.n = \"range\" / \"min\" / \"no\")."),
        user_env = user_env)
      if (isFALSE(add_n) && (missing(n) || is.null(n))) base_n <- "no"
    }
    out$base_n <- base_n
  }

  out
}

# Completed from the declared default, so a PARTIAL option leaves the other slots alone.
#' @keywords internal
#' @noRd
tab_total_names <- function() tab_total_names_merge(getOption("tabxplor.total_names"))

#' @keywords internal
#' @noRd
tab_total_names_merge <- function(got) {
  base <- tx_option_default("total_names")
  if (is.null(got)) return(base)
  # WARNING: stats::setNames, not as.character() -- the latter STRIPS the names, and every slot
  # would then be read positionally, so `c(other = "Autres")` would rename the total ROW.
  got <- stats::setNames(as.character(got), names(got))
  if (is.null(names(got))) {                       # an unnamed vector fills row, col, tab, other
    got <- stats::setNames(got, names(base)[seq_along(got)])
  }
  bad <- setdiff(names(got), names(base))
  if (length(bad))
    cli::cli_abort(c("Unknown {.code options(tabxplor.total_names)} slot{?s} {.val {bad}}.",
                     "i" = "Valid: {.val {names(base)}}."), call = NULL)
  base[names(got)] <- got
  base
}

# Names the OPTION, not just the deprecation: the user needs to know where the label lives now.
#' @keywords internal
#' @noRd
tab_deprecate_total_label <- function(fn, arg, user_env) {
  slot <- switch(arg, total_names = "row/col", totaltab_name = "tab", other_level = "other")
  lifecycle::deprecate_soft(
    "2.0.0", I(paste0(fn, "(", arg, " = )")),
    I('options(tabxplor.total_names = )'),
    details = paste0("The four synthetic labels are one option now: set the `", slot,
                     "` slot, e.g. options(tabxplor.total_names = c(tab = \"Ensemble\", ",
                     "other = \"Autres\"))."),
    user_env = user_env)
}
