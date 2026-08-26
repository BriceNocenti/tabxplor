
# === SECTION: the crosstab live-UI cache ==========================================================

withr::local_options(lifecycle_verbosity = "quiet", .local_envir = testthat::teardown_env())


jmv_opts <- function(...) {
  # Phase 19k: the option names + values are tab()'s own -- `chi2` is `test`, the retired `OR` is a
  # `display` + `ref2` route, `ci` speaks the anchor vocabulary, `color` the full measure words.
  o <- list(row_vars = character(), col_vars = character(), tab_vars = character(), wt = character(),
            pct = "no", color = "no", color_signif = "ignore", test = FALSE,
            na = "keep", levels = "all", ref = "auto", ref2 = "first", comp = "tab", ci = "auto",
            conf_level = 0.95, stars = TRUE,   # design_effect: absent -> the global option decides
            # the jamovi UI keeps one ComboBox per interval kind; jmv_ci_method() folds them
            ci_method_cell = "wilson", ci_method_diff = "newcombe",
            ci_method_mean_diff = "welch", ci_method_mean_ratio = "robust",
            totaltab = "line", digits = 0, add_pct = FALSE,
            # 20g-ii: `n_min` was MISSING here although .opts() sets it and it is in the tier-3
            # `reapplied` set -- the fixture must carry every key that vector names, or the D12
            # assertion below cannot see the real invariant.
            n_min = 0,
            subtext = "", output_list = FALSE, cleannames = FALSE, display = "auto",
            anova = "welch",
            # 22g-iii: the per-numeric-variable cut. Like `levels_collapse` it is a PRE-aggregate
            # recode and therefore sits in the tier-1 keys, so the oracle must pass it too.
            shape = NULL,
            # 20g-i: ONE key of the option's own shape (it was three constants mirroring three
            # arguments that no longer exist). `.opts()` fills it with the module's translations.
            total_names = c(row = "Total", col = "Total", tab = "Ensemble", other = "Others"))
  # WARNING: `modifyList()` keeps the FIRST of two same-named entries, and every `o0(...)`/`mk(...)`
  # wrapper below splices its own defaults beside the caller's `...` -- so a later override was
  # silently swallowed. Measured on the pre-fix tree: `o0(color = "ratio")` at L318 built with
  # `color = "difference"`, and the trailing `ref = "1"` at L381 was dead, making case `b` identical to
  # case `a`. Keep the LAST, which is R's ordinary override semantic.
  args <- list(...)
  if (length(args)) args <- args[!duplicated(names(args), fromLast = TRUE)]
  utils::modifyList(o, args)
}


# The no-cache oracle: tab() with jmvtab_build()'s exact arg mapping (dummy vars, color, ci forcing).
jmv_oracle <- function(opts, data) {
  if (length(opts$row_vars) == 0L) { data$no_row_var <- factor("no_row_var"); opts$row_vars <- "no_row_var" }
  if (length(opts$col_vars) == 0L) { data$no_col_var <- factor("n");          opts$col_vars <- "no_col_var" }
  color <- switch(opts$color, "no" = FALSE, "auto" = TRUE, opts$color)
  # The RESOLVED anchor, from the same shared resolver jmvtab_build() calls -- not a hand-mirrored
  # `if (a policy) ci <- "diff"`, which was one more copy of a rule that has moved twice since it was
  # written.
  ci <- resolve_leaf_ci(opts$ci, jmv_tab3_measure(color), opts$color_signif, opts$stars,
                        if (length(opts$ref)) opts$ref else "auto")$ci
  wt_sym <- if (length(opts$wt)) rlang::sym(opts$wt) else NULL
  # Phase 19k: no translation left -- every option is passed under the name tab() gives it.
  # 20b/20g-i: the four synthetic labels are an option, installed exactly as jmv_tab3_build_armed()
  # installs them.
  if (length(opts$total_names)) {
    .old <- options(tabxplor.total_names = tabxplor:::tab_total_names_merge(opts$total_names))
    on.exit(options(.old), add = TRUE)
  }
  rlang::inject(tab(
    data, row_vars = tidyselect::all_of(opts$row_vars), col_vars = tidyselect::all_of(opts$col_vars),
    tab_vars = tidyselect::all_of(opts$tab_vars), wt = !!wt_sym, pct = opts$pct, color = color,
    color_signif = opts$color_signif, display = opts$display, test = opts$test, na = opts$na,
    levels = opts$levels, ref = opts$ref, ref2 = opts$ref2, comp = opts$comp, ci = ci,
    conf_level = opts$conf_level, stars = opts$stars, anova = opts$anova,
    ci_method = c(cell = opts$ci_method_cell, diff = opts$ci_method_diff),
    cleannames = FALSE, totaltab = opts$totaltab, digits = opts$digits,
    n = opts[["n"]], add_pct = opts$add_pct,
    subtext = opts$subtext, output_list = isTRUE(opts$output_list), shape = opts$shape
  ))
}


gss <- fx_gss()

gssw <- dplyr::mutate(gss, w = as.numeric(1 + (as.integer(marital) %% 3)))

# Phase 19m-i: a BINARY col_var. It is the shape on which "which column is the total" changes the
# answer rather than merely the bookkeeping -- two levels + a total is `binary` (each level's odds
# ratio against the OTHER level), three columns counted as levels is not.
gssb <- dplyr::mutate(gss, white = forcats::fct_other(race, keep = "White",
                                                      other_level = "Non-white"))


test_that("store: per-entry byte ceiling skips oversized entries", {
  s <- jmv_cache_new()
  # A genuinely large blob (the ceiling measures SERIALIZED bytes -- an ALTREP compact sequence would
  # serialize tiny regardless of length, which is correct: only the real persisted cost counts).
  agg_ceiling <- JMVTAB_CFG$entry_bytes[["agg"]]
  big <- list(cols = list(x = strrep("z", agg_ceiling + 1e5)), keys = "x")
  expect_gt(length(serialize(big, connection = NULL)), agg_ceiling)
  s <- jmv_cache_put(s, "agg", "big", big)
  expect_length(s$agg, 0L)  # not persisted (over the ceiling)
})


test_that("the weighted basis reaches the cached FACTOR aggregate too (design_effect)", {
  # Phase 18z16-iiiii. jmv_cache_aggregate()'s factor tier-1 emitted only (n, wn), while its
  # numeric twin emitted the moment triples -- so in jamovi, ticking `design_effect` corrected the
  # MEAN cell intervals, left the PERCENTAGES on the raw n, corrected NEITHER p-value (the omnibus
  # gate skipped the whole grid whenever a cached `.fine` was present, so a mixed table lost its
  # numeric F_design as well), and the footer then denied the one correction that had happened.
  # Failing-first: without the `w2` column and its rollup, `n_eff` is all-NA on the cache path.
  withr::local_options(tabxplor.design_effect = TRUE)
  o <- jmv_opts(row_vars = "marital", col_vars = c("race", "tvhours"), wt = "w",
                pct = "row", test = TRUE, na = "drop")
  cold <- jmvtab_build(gssw, o, NULL)
  warm <- jmvtab_build(gssw, o, cold$store)
  expect_equal(cold$tabs, jmv_oracle(o, gssw))    # the cache path IS the oracle, correction included
  expect_equal(warm$tabs, cold$tabs)
  t  <- cold$tabs
  ne <- get_n_eff(t[[which(purrr::map_lgl(t, ~ is_fmt(.) && get_pct_type(.) == "row"))[[1]]]])
  expect_gt(sum(is.finite(ne)), 0L)               # non-vacuous: percentages ARE corrected now
  expect_identical(tabxplor:::tab_inference_basis(t), "weights")
  expect_true(all(c("chi2_design", "F_design") %in% get_test(t)$test))
})


# Phase 15c: na = "drop_all" used to collapse na_num to a SCALAR "keep", but jmv_cache_aggregate()'s
# numeric tier-1 loop indexes ctx$na_num[[i]] per row_var -> "subscript out of bounds" for a 2nd
# row_var with a numeric col_var. tab_prepare_pop() now emits a per-row_var list; still byte-identical.
test_that("na = 'drop_all' builds with >=2 row_vars + numeric col (no subscript error)", {
  cases <- list(
    jmv_opts(row_vars = c("marital", "relig"), col_vars = c("race", "tvhours"),
             pct = "row", na = "drop_all"),
    jmv_opts(row_vars = c("marital", "relig"), col_vars = "tvhours", na = "drop_all"),
    jmv_opts(row_vars = c("marital", "relig", "race"), col_vars = "partyid", na = "drop_all")
  )
  for (o in cases) {
    expect_no_error(cold <- jmvtab_build(gss, o, NULL))
    expect_equal(cold$tabs, jmv_oracle(o, gss))
  }
})


test_that("weighted build is byte-identical (tolerant on wn)", {
  o <- jmv_opts(row_vars = "marital", col_vars = "race", pct = "row", wt = "w",
                color = "auto", color_signif = "grey_non_signif")
  cold <- jmvtab_build(gssw, o, NULL)
  expect_equal(cold$tabs, jmv_oracle(o, gssw))
  expect_equal(jmvtab_build(gssw, o, cold$store)$tabs, cold$tabs)
})


# --- tier-1 reuse -------------------------------------------------------------------------
test_that("goal (a): adding a col_var reuses the prior pair (na = keep)", {
  s  <- jmvtab_build(gss, jmv_opts(row_vars = "marital", col_vars = "race", pct = "row"), NULL)$store
  r2 <- jmvtab_build(gss, jmv_opts(row_vars = "marital", col_vars = c("race", "partyid"), pct = "row"), s)
  expect_true(r2$hits$agg[["marital\rrace"]])       # reused
  expect_false(r2$hits$agg[["marital\rpartyid"]])   # fresh
})


test_that("factor keep <-> drop SHARE the aggregate; numeric keep <-> drop DO NOT", {
  # factor: keep then drop -> tier-1 hit (NA-kept aggregate, post-aggregate cell delete)
  s  <- jmvtab_build(gss, jmv_opts(row_vars = "marital", col_vars = "race", pct = "row", na = "keep"), NULL)$store
  rf <- jmvtab_build(gss, jmv_opts(row_vars = "marital", col_vars = "race", pct = "row", na = "drop"), s)
  expect_true(rf$hits$agg[["marital\rrace"]])
  # numeric: keep then drop -> tier-1 MISS (pre-scan na.omit -> different population)
  sn <- jmvtab_build(gss, jmv_opts(row_vars = "marital", col_vars = "tvhours", na = "keep"), NULL)$store
  rn <- jmvtab_build(gss, jmv_opts(row_vars = "marital", col_vars = "tvhours", na = "drop"), sn)
  expect_false(rn$hits$agg[["marital\r<num>"]])
})


# --- tier-2 reuse -------------------------------------------------------------------------
test_that("tier-2 test is reused across pct/ref toggles and matches a fresh chi2", {
  r1 <- jmvtab_build(gss, jmv_opts(row_vars = "marital", col_vars = "race", pct = "row", test = TRUE), NULL)
  expect_equal(sum(r1$hits$test), 0)                       # cold: computed
  # stored test is populated (real chi2, not an empty placeholder)
  expect_equal(nrow(r1$store$test[[1]]$value), 1L)
  expect_identical(r1$store$test[[1]]$value$test, "chi2")

  r2 <- jmvtab_build(gss, jmv_opts(row_vars = "marital", col_vars = "race", pct = "col", test = TRUE), r1$store)
  expect_equal(sum(r2$hits$test), 1)                       # pct change reuses the test
  r3 <- jmvtab_build(gss, jmv_opts(row_vars = "marital", col_vars = "race", pct = "row",
                                   ref = "1", test = TRUE), r2$store)
  expect_equal(sum(r3$hits$test), 1)                       # ref change reuses the test
  # a cached-test run is byte-identical to a fresh chi2 run (jmv_opts sets stars = TRUE, so the
  # expected tab() must too -- stars are opt-in / storage-driven since the bug-fix)
  expect_equal(r3$tabs, tab(gss, marital, race, pct = "row", ref = "1", test = TRUE, ci = "auto",
                            stars = TRUE, cleannames = FALSE, anova = "welch"))
})


test_that("contrib coloring does NOT use the tier-2 cache (recomputes per-cell fields)", {
  o <- jmv_opts(row_vars = "marital", col_vars = "race", pct = "row", test = TRUE, color = "contrib")
  r1 <- jmvtab_build(gss, o, NULL)
  r2 <- jmvtab_build(gss, o, r1$store)
  expect_equal(sum(r2$hits$test), 0)                       # never a test hit under contrib
  expect_equal(r2$tabs, jmv_oracle(o, gss))
})


# Phase 20g-i: what defect D11 was, pinned. A `ci = "cell"` table with MIXED col_vars must stamp
# `pct_ci` on the factor columns and `mean_ci` on the numeric one, on BOTH paths -- jmv_apply_display()
# used to write `pct_ci` on the mean column too (whose `pct` is NA -> an EMPTY cell) and to run AFTER
# the display ComboBox, so it overrode the user. 19j moved that stamp into the leaf and 19k deleted
# the copy; this is the fixture that says so, and the reason the standing "reproduce it in 20g" note
# is closed rather than carried.
test_that("D11: ci = 'cell' with mixed col_vars stamps the same displays as tab()", {
  o  <- jmv_opts(row_vars = "marital", col_vars = c("race", "tvhours"), pct = "row", ci = "cell")
  jr <- suppressMessages(jmvtab_build(gss, o, NULL))$tabs
  ot <- suppressMessages(jmv_oracle(o, gss))
  disp <- function(t) vapply(t[vapply(t, is_fmt, logical(1))],
                             function(x) get_display(x)[[1]], character(1))
  expect_identical(disp(jr), disp(ot))
  # one token for every column type since the pipeline composites were retired: `ci = "cell"` writes
  # the ordinary `{ci}`, and a percentage or a mean column answers it with its own scale.
  expect_identical(unname(disp(jr)), rep("ci", 5L))
})


test_that("numeric-valued col_vars become mean columns (match R; jamovi factors integers)", {
  # jamovi hands a nominal/ordinal integer to the module ALREADY factored, so tvhours would wrongly
  # become one column per value. jmv_coerce_numeric_cols() restores the numeric type -> a mean column.
  d <- gss
  d$tvhours_f <- factor(d$tvhours)                         # simulate jamovi's factor delivery
  r <- jmvtab_build(d, jmv_opts(row_vars = "marital", col_vars = "tvhours_f"), NULL)
  fmt <- setdiff(names(r$tabs)[purrr::map_lgl(r$tabs, is_fmt)], "n")
  expect_identical(fmt, "tvhours_f")                       # ONE mean column, not one per value
  expect_true(tabxplor:::fmt_var_kind(r$tabs[["tvhours_f"]]) == "mean")
  # a genuine categorical (non-numeric levels) is untouched -> columns
  r2 <- jmvtab_build(d, jmv_opts(row_vars = "marital", col_vars = "race", pct = "row"), NULL)
  expect_gt(length(names(r2$tabs)[purrr::map_lgl(r2$tabs, is_fmt)]), 2)
})


test_that("cleannames at display preserves the tab class on grouped/compacted tables", {
  # Multi-row_var (compacted -> grouped by the `row_var` indicator) and tab_vars tables are grouped;
  # cleannames must clean labels WITHOUT downgrading the tabxplor class (regression: across() can't
  # select grouping columns, and base [[<-/names<- drop the class).
  cases <- list(
    list(lbl = "2 row_vars",  o = jmv_opts(row_vars = c("marital", "relig"), col_vars = "race",
                                            pct = "row", cleannames = TRUE)),
    list(lbl = "tab_vars",    o = jmv_opts(row_vars = "marital", col_vars = "race",
                                           tab_vars = "relig", pct = "row", cleannames = TRUE))
  )
  for (case in cases) {
    off <- case$o; off$cleannames <- FALSE
    want <- class(suppressMessages(jmvtab_build(gss, off, NULL))$tabs)
    got  <- class(suppressMessages(jmvtab_build(gss, case$o, NULL))$tabs)
    expect_identical(got, want, info = case$lbl)
    expect_no_error(tab_kable(suppressMessages(jmvtab_build(gss, case$o, NULL))$tabs))
  }
})


test_that("defer_level_merge: levels = 'first' tests FULL levels", {
  d <- dplyr::filter(gss, !is.na(marital), !is.na(race))
  jf <- jmvtab_build(d, jmv_opts(row_vars = "marital", col_vars = "race", pct = "row",
                                 levels = "first", test = TRUE), NULL)
  # race has 3 levels -> full-level df = (nlevels(marital)-1)*(3-1) = 10. tab(levels="first") would
  # give (nlevels(marital)-1)*(2-1) = 5. The cached test carries the FULL-level df.
  expect_equal(jf$store$test[[1]]$value$df1, 10)   # full 3-level race x 6-level marital
  # displayed table keeps only the first race level
  fmt_cols <- setdiff(names(jf$tabs)[purrr::map_lgl(jf$tabs, is_fmt)], c("n", "wn"))
  expect_true("Other" %in% fmt_cols)
  expect_false("White" %in% fmt_cols)
})


test_that("Phase 7f: colour / digits toggles are byte-identical to a fresh tab()", {
  o0 <- function(...) jmv_opts(row_vars = "marital", col_vars = "race", pct = "row",
                               color = "difference", test = TRUE, ...)
  for (o in list(o0(digits = 2), o0(color = "ratio"), o0(color = "auto"),
                 o0(color_signif = "grey_non_signif"), o0(color_signif = "guaranteed_effect"))) {
    expect_equal(jmvtab_build(gss, o, NULL)$tabs, jmv_oracle(o, gss))
  }
  # numeric means: digits + significance policy
  on <- function(...) jmv_opts(row_vars = "marital", col_vars = "tvhours", color = "ratio", ...)
  for (o in list(on(digits = 2), on(color_signif = "grey_non_signif"))) {
    expect_equal(jmvtab_build(gss, o, NULL)$tabs, jmv_oracle(o, gss))
  }
})


test_that("Phase 7f: a base change rebuilds (tier-3 miss) but stays byte-identical", {
  base <- jmv_opts(row_vars = "marital", col_vars = "race", pct = "row", color = "difference")
  st   <- jmvtab_build(gss, base, NULL)$store
  for (o in list(
    jmv_opts(row_vars = "marital", col_vars = "race", pct = "col", color = "difference"),               # pct
    jmv_opts(row_vars = "marital", col_vars = "race", pct = "row", color = "difference", na = "drop")   # na
  )) {
    r <- jmvtab_build(gss, o, st)
    expect_false(isTRUE(r$hits$tab3))
    expect_equal(r$tabs, jmv_oracle(o, gss))
  }
  # Phase 9b-7: a REFERENCE change is now a tier-3 RE-REF (hit), not a rebuild -- still byte-identical.
  r <- jmvtab_build(gss, jmv_opts(row_vars = "marital", col_vars = "race", pct = "row",
                                  color = "difference", ref = "1"), st)
  expect_true(isTRUE(r$hits$tab3))
  expect_equal(r$tabs, jmv_oracle(jmv_opts(row_vars = "marital", col_vars = "race", pct = "row",
                                           color = "difference", ref = "1"), gss))
})


# --- Phase 9b-7: instant reference re-ref (jmv_tab3_reref on the raw carrier) --------------
# A ref/ref2-only change recomputes diff/ratio/CI from the cached carrier's ref-independent base
# (no O(cells) rebuild) -- hits$tab3 == TRUE, byte-identical to the REBUILD it replaces. The gate is
# `warm A -> B == a fresh jmvtab_build(B)` (both share the tier-4 tail, so it is valid for every case
# incl. ci = "cell" where jmv_apply_display diverges from a plain tab()). warm A -> B.
test_that("Phase 9b-7: a reference change re-refs (tier-3 hit) and equals the rebuild", {
  o0 <- function(...) jmv_opts(row_vars = "marital", col_vars = "race", pct = "row",
                               color = "difference", test = TRUE, ...)
  cases <- list(
    list(d = "gss",  a = o0(),                b = o0(ref = "1")),                  # tot -> first
    list(d = "gss",  a = o0(ref = "1"),       b = o0(ref = "3")),                  # index -> index
    list(d = "gss",  a = o0(),                b = o0(ref = "Divorced")),           # tot -> label
    # Phase 19k: a RATIO comparison IS re-ref-eligible. The re-ref builds the interval with
    # leaf_ci_plain(), the leaf's own producer, which takes `ci_scale` -- so it rebuilds the Katz
    # log-RR bounds and restamps the column's `scale` / `ci_method` from the same CI_GEOMS row the
    # leaf reads. (19d/19j had left it a rebuild because the re-ref still went through tab_ci(), the
    # DIFFERENCE engine.)
    list(d = "gss",  a = o0(color = "ratio"), b = o0(color = "ratio", ref = "2")), # ratio colour
    list(d = "gss",  a = o0(color = "auto"),  b = o0(color = "auto", ref = "1")),  # auto colour
    list(d = "gss",  a = o0(color_signif = "grey_non_signif"),
                     b = o0(color_signif = "grey_non_signif", ref = "1")),         # significance policy
    list(d = "gss",  a = o0(ci = "cell"),     b = o0(ci = "cell", ref = "1")),     # cell CI
    list(d = "gss",  a = o0(ci = "no"),       b = o0(ci = "no", ref = "1")),       # no CI
    list(d = "gss",  a = o0(stars = FALSE),   b = o0(stars = FALSE, ref = "1")),   # stars off
    list(d = "gss",  a = o0(tab_vars = "year"),
                     b = o0(tab_vars = "year", ref = "1")),                        # grouped (tab_vars)
    list(d = "gssw", a = o0(wt = "w"),        b = o0(wt = "w", ref = "1")),        # weighted
    list(d = "gss",  a = o0(col_vars = c("race", "partyid")),
                     b = o0(col_vars = c("race", "partyid"), ref = "1")),          # multi col_var
    # Phase 19m-i: a RENAMED total column. `grp` holds FINAL (post leaf_rename_totals) names, so
    # tab_apply_reference()'s old `nm == "Total"` matched nothing here and the odds ratio's 2x2 was
    # built against the wrong column -- a real defect masked only because po/R-fr.po translates
    # "Total" -> "Total". The stored `totcol` attribute is the fact; this case is what proves it.
    list(d = "gssb", a = o0(col_vars = "white", total_names = c(row = "Total", col = "Ensemble")),
                     b = o0(col_vars = "white", total_names = c(row = "Total", col = "Ensemble"),
                            ref = "1"))                                            # renamed total col
  )
  for (cs in cases) {
    dat <- get(cs$d)
    st  <- suppressMessages(jmvtab_build(dat, cs$a, NULL))$store
    r   <- suppressMessages(jmvtab_build(dat, cs$b, st))                          # warm A -> B (reref)
    rebuild <- suppressMessages(jmvtab_build(dat, cs$b, NULL))$tabs              # fresh B (rebuild)
    expect_equal(isTRUE(r$hits$tab3), cs$hit %||% TRUE,
                 info = paste("reref hit:", cs$b$ref, cs$b$color))
    expect_equal(r$tabs, rebuild, info = paste("reref == rebuild:", cs$b$ref, cs$b$color))
  }
})


# Phase 19k: `color = "auto"` beside an explicit `ci = "ref"` IS re-ref-eligible. It used to be
# excluded because that pair resolved to the composite "after_ci" -- a ref-DEPENDENT colour stamped
# by the CI step, which the re-ref could not reproduce. 19c deleted the resolution, 19j the step.
test_that("Phase 19k: color = auto + an explicit ci re-refs (and equals the rebuild)", {
  o0 <- function(...) jmv_opts(row_vars = "marital", col_vars = "race", pct = "row",
                               color = "auto", ci = "ref", test = TRUE, ...)
  st <- suppressMessages(jmvtab_build(gss, o0(), NULL))$store
  r  <- suppressMessages(jmvtab_build(gss, o0(ref = "1"), st))
  expect_true(isTRUE(r$hits$tab3))
  expect_equal(r$tabs, suppressMessages(jmvtab_build(gss, o0(ref = "1"), NULL))$tabs)
})


# Phase 19k (D12): the four interval-method options are re-applied, not structural, and a method
# change is served by the RE-REF -- leaf_ci_plain() rebuilds the bounds with the new engine and
# restamps the column's `ci_method`. They used to land in the base key under a name that is not an
# option key at all, so every toggle rebuilt and the cheap path was unreachable.
test_that("Phase 19k: a CI-method change re-refs (and equals the rebuild)", {
  o0 <- function(...) jmv_opts(row_vars = "marital", col_vars = "race", pct = "row",
                               color = "difference", ci = "ref", test = TRUE, ...)
  st <- suppressMessages(jmvtab_build(gss, o0(), NULL))$store
  r  <- suppressMessages(jmvtab_build(gss, o0(ci_method_diff = "ac"), st))
  expect_true(isTRUE(r$hits$tab3))
  expect_equal(r$tabs, suppressMessages(jmvtab_build(gss, o0(ci_method_diff = "ac"), NULL))$tabs)
  expect_equal(r$tabs, jmv_oracle(o0(ci_method_diff = "ac"), gss))
})


test_that("Phase 9b-7: a re-ref'd table equals a plain tab() (independent anchor)", {
  # jmv_oracle = a plain tab(); valid where jmvtab_build == tab() (display = auto, ci = auto).
  o0 <- function(...) jmv_opts(row_vars = "marital", col_vars = "race", pct = "row", test = TRUE, ...)
  # Phase 19k: the ratio comparison is served by the carrier too (the re-ref recomputes its Katz
  # interval). The equality below is the real lock either way.
  for (cs in list(list(a = o0(color = "difference"),  b = o0(color = "difference",  ref = "1")),
                  list(a = o0(color = "ratio"), b = o0(color = "ratio", ref = "2")),
                  list(a = o0(color = "difference", tab_vars = "year"),
                       b = o0(color = "difference", tab_vars = "year", ref = "1")))) {
    st <- suppressMessages(jmvtab_build(gss, cs$a, NULL))$store
    r  <- suppressMessages(jmvtab_build(gss, cs$b, st))
    expect_equal(isTRUE(r$hits$tab3), cs$hit %||% TRUE)
    expect_equal(r$tabs, jmv_oracle(cs$b, gss))
  }
})


test_that("Phase 9b-7: a second identical reference is an exact re-paint hit", {
  oA <- jmv_opts(row_vars = "marital", col_vars = "race", pct = "row", color = "difference", test = TRUE)
  oB <- utils::modifyList(oA, list(ref = "1"))
  s  <- suppressMessages(jmvtab_build(gss, oA, NULL))$store
  r1 <- suppressMessages(jmvtab_build(gss, oB, s))          # reref, stored under the new tuple
  r2 <- suppressMessages(jmvtab_build(gss, oB, r1$store))   # same ref again -> exact-tuple re-paint
  expect_true(isTRUE(r2$hits$tab3))
  expect_equal(r2$tabs, jmv_oracle(oB, gss))
})


test_that("Phase 9b-7: non-rerefable ref changes rebuild but stay byte-identical", {
  mk <- function(...) jmv_opts(row_vars = "marital", col_vars = "race", ...)
  cases <- list(
    list(a = mk(pct = "col", color = "difference"),                      b = mk(pct = "col", color = "difference", ref = "Black")),
    list(a = jmv_opts(row_vars = "marital", col_vars = "tvhours", pct = "row", color = "auto"),
         b = jmv_opts(row_vars = "marital", col_vars = "tvhours", pct = "row", color = "auto", ref = "1")),  # numeric
    list(a = mk(pct = "row", color = "difference", levels = "first"),    b = mk(pct = "row", color = "difference", levels = "first", ref = "1")),
    list(a = mk(pct = "row", color = "difference", add_pct = TRUE),      b = mk(pct = "row", color = "difference", add_pct = TRUE, ref = "1")),
    list(a = mk(pct = "row", display = "or", color = "odds_ratio"),
         b = mk(pct = "row", display = "or", color = "odds_ratio", ref = "1")),   # odds ratio
    list(a = jmv_opts(row_vars = "marital", col_vars = "race", tab_vars = "year", pct = "row", color = "difference", test = TRUE, comp = "all"),
         b = jmv_opts(row_vars = "marital", col_vars = "race", tab_vars = "year", pct = "row", color = "difference", test = TRUE, comp = "all", ref = "1"))
  )
  # suppressWarnings: some cases deliberately warn (comp = "all" announces the added total table;
  # ref = "1" matches no row label and falls back). Both are correct behaviour and asserted in
  # test-tab.R; here they are incidental -- this test is about rebuild-vs-reref, and `ref = "1"`
  # only has to DIFFER from case `a`'s ref to force the tier-3 miss.
  for (cs in cases) {
    st <- suppressWarnings(suppressMessages(jmvtab_build(gss, cs$a, NULL)))$store
    r  <- suppressWarnings(suppressMessages(jmvtab_build(gss, cs$b, st)))
    expect_false(isTRUE(r$hits$tab3), info = paste("rebuild (not reref):", cs$b$pct, cs$b$color))
    expect_equal(r$tabs, suppressWarnings(jmv_oracle(cs$b, gss)))
  }
})


test_that("Phase 9b-7: re-ref works off a $state-restored carrier", {
  oA <- jmv_opts(row_vars = "marital", col_vars = "race", pct = "row", color = "difference", test = TRUE)
  oB <- utils::modifyList(oA, list(ref = "1"))
  st   <- suppressMessages(jmvtab_build(gss, oA, NULL))$store
  back <- unserialize(serialize(st, connection = NULL))       # jamovi $state gzip-RDS round-trip
  r    <- suppressMessages(jmvtab_build(gss, oB, back))
  expect_true(isTRUE(r$hits$tab3))
  expect_equal(r$tabs, jmv_oracle(oB, gss))
})


test_that("Phase 7f: adding a col_var is a base change (new tier-3 entry, other pairs' aggregate reused)", {
  st <- jmvtab_build(gss, jmv_opts(row_vars = "marital", col_vars = "race", pct = "row"), NULL)$store
  r  <- jmvtab_build(gss, jmv_opts(row_vars = "marital", col_vars = c("race", "partyid"), pct = "row"), st)
  expect_false(isTRUE(r$hits$tab3))                 # different col_var set -> different base-key -> rebuild
  expect_true(r$hits$agg[["marital\rrace"]])        # but the race count aggregate (tier 1) is reused
})


test_that("Phase 7g: n_min is a tier-4 re-derive that never corrupts the cached armed table", {
  o0   <- jmv_opts(row_vars = "marital", col_vars = "race", pct = "row")
  full <- jmvtab_build(gss, o0, NULL)

  # apply n_min on the SAME store: fewer rows, and == applying the helper to the full built table
  oN    <- utils::modifyList(o0, list(n_min = 5000))
  small <- jmvtab_build(gss, oN, full$store)
  expect_lt(nrow(small$tabs), nrow(full$tabs))
  expect_equal(small$tabs, tab_apply_n_min(full$tabs, 5000))

  # re-deriving with n_min = 0 gives the full table back -> the cached armed table was NOT filtered
  back <- jmvtab_build(gss, o0, small$store)
  expect_equal(back$tabs, full$tabs)
})


# Phase 19k: `anova` is display INTENT (which of the two stored F rows the p-value line shows), so
# it is re-applied at tier 4 and a toggle is a cheap re-derive -- it used to sit in the base key and
# rebuild the whole table. The lock is the usual one: the re-derive must equal a cold build, and the
# displayed test must really follow the option.
test_that("Phase 19k: anova is a tier-4 re-derive (welch <-> classic re-paints)", {
  base <- jmv_opts(row_vars = "marital", col_vars = "tvhours", test = TRUE)
  oW <- utils::modifyList(base, list(anova = "welch"))
  oC <- utils::modifyList(base, list(anova = "classic"))
  s <- jmvtab_build(gss, oW, NULL)$store
  r <- jmvtab_build(gss, oC, s)
  expect_true(isTRUE(r$hits$tab3))                       # no O(cells) rebuild
  expect_equal(r$tabs, jmvtab_build(gss, oC, NULL)$tabs) # ... and identical to a cold build
  expect_identical(tab_anova(r$tabs), "classic")
  # the DISPLAYED test row follows it (both F rows are stored either way)
  expect_true(all(c("F_welch", "F_classic") %in% get_test(r$tabs)$test))
  expect_identical(test_display_rows(get_test(r$tabs), tab_anova(r$tabs))$test, "F_classic")
})


test_that("Phase 7g: the reference-level picker vector drives the cache like any ref change", {
  base <- jmv_opts(row_vars = "marital", col_vars = "race", pct = "row")
  full <- jmvtab_build(gss, base, NULL)
  ref  <- jmvtab_ref_vector(list(list(var = "marital", ref = "Divorced")), "auto")
  r    <- jmvtab_build(gss, utils::modifyList(base, list(ref = ref)), full$store)
  # a named ref differs from "auto" -> tier-3 tuple change -> rebuild, but tier-1 aggregate reused
  expect_false(isTRUE(r$hits$tab3))
  expect_true(r$hits$agg[["marital\rrace"]])
})


# --- Phase 7g-ii: level reordering (post-aggregate, tier-3 input) -------------------------
# The oracle is a plain tab() on PRE-RELEVELED microdata: the jmvtab reorder relevels the shaped
# aggregate in memory (stored blob stays raw), which must be byte-identical to having fct_relevel'd
# the data before tab(). A reorder reuses tiers 1-2 (aggregate + test) and only rebuilds the fmt.
mar_ord  <- c("Married", "Divorced", "Widowed", "Separated", "Never married", "No answer")

race_ord <- c("White", "Black", "Other", "Not applicable")   # raw first = "Other"

re_relevel <- function(d, spec) {
  for (v in names(spec)) d[[v]] <- forcats::fct_relevel(d[[v]], spec[[v]])
  d
}


test_that("jmvtab_levels_order(): picker Array -> named ordered list, empties dropped", {
  lo <- list(
    list(var = "marital", levels = list("Divorced", "Married")),
    list(var = "",        levels = list("x")),           # empty var -> dropped
    list(var = "race",    levels = list()),              # empty levels -> dropped
    list(var = "relig",   levels = list("None", ""))     # blank level filtered
  )
  spec <- jmvtab_levels_order(lo)
  expect_identical(names(spec), c("marital", "relig"))
  expect_identical(spec$marital, c("Divorced", "Married"))
  expect_identical(spec$relig, "None")
  expect_null(jmvtab_levels_order(list()))
  expect_null(jmvtab_levels_order(list(list(var = "race", levels = list()))))
})


test_that("reorder is byte-identical to tab() on pre-releveled microdata", {
  cases <- list(
    list(o = jmv_opts(row_vars = "marital", col_vars = "race", pct = "row", test = TRUE),
         spec = list(marital = mar_ord, race = race_ord)),
    list(o = jmv_opts(row_vars = "marital", col_vars = "tvhours"),                 # factor x numeric
         spec = list(marital = mar_ord)),
    list(o = jmv_opts(row_vars = "relig", col_vars = "race", tab_vars = "marital", pct = "row"),
         spec = list(marital = mar_ord, race = race_ord)),                         # subtable reorder
    list(o = jmv_opts(row_vars = "marital", col_vars = "race", pct = "row", na = "drop"),
         spec = list(marital = mar_ord, race = race_ord)),
    list(o = jmv_opts(row_vars = "marital", col_vars = "race", pct = "col"),
         spec = list(marital = mar_ord, race = race_ord))
  )
  for (cs in cases) {
    built <- jmvtab_build(gss, utils::modifyList(cs$o, list(levels_order = cs$spec)), NULL)
    expect_equal(built$tabs, jmv_oracle(cs$o, re_relevel(gss, cs$spec)))
  }
})


test_that("reorder + levels = 'first' keeps the reordered-first level", {
  o <- jmv_opts(row_vars = "marital", col_vars = "race", pct = "row", levels = "first")
  built <- jmvtab_build(gss, utils::modifyList(o, list(levels_order = list(race = race_ord))), NULL)
  expect_true("White" %in% names(built$tabs))     # reordered first
  expect_false("Other" %in% names(built$tabs))    # raw first, dropped
  expect_equal(built$tabs, jmv_oracle(o, re_relevel(gss, list(race = race_ord))))
})


test_that("a reorder reuses tiers 1-2 (only the tier-3 armed table rebuilds)", {
  o <- jmv_opts(row_vars = "marital", col_vars = "race", pct = "row", test = TRUE)
  s <- jmvtab_build(gss, o, NULL)$store
  r <- jmvtab_build(gss, utils::modifyList(o, list(levels_order = list(marital = mar_ord))), s)
  expect_true(r$hits$agg[["marital\rrace"]])       # aggregate reused (raw fingerprint unchanged)
  expect_false(isTRUE(r$hits$tab3))                # armed table rebuilt (base-key changed)
})


test_that("levels_order = NULL leaves the build byte-identical (no-op)", {
  o <- jmv_opts(row_vars = "marital", col_vars = "race", pct = "row", test = TRUE)
  expect_equal(jmvtab_build(gss, utils::modifyList(o, list(levels_order = NULL)), NULL)$tabs,
               jmvtab_build(gss, o, NULL)$tabs)
})


# --- Phase 20g-ii: level MERGING (pre-aggregate; the reorder's opposite in every cache respect) ---
# Same free oracle as the reorder -- a plain tab() on microdata the user collapsed themselves -- but
# the tier contract is INVERTED: a merge changes the counts, so it must MISS tier 1.
mar_merge <- list(marital = list("Not married" = c("Never married", "Divorced", "Separated")))

re_collapse <- function(d, spec) {
  for (v in names(spec)) d[[v]] <- forcats::fct_collapse(d[[v]], !!!spec[[v]])
  d
}


test_that("jmvtab_levels_collapse(): picker Array -> the fct_collapse spec, empties dropped", {
  lc <- list(
    list(var = "marital", label = "Not married", levels = list("Divorced", "Separated")),
    list(var = "marital", label = "",            levels = list("Married", "Widowed")),
    list(var = "",        label = "x",           levels = list("a", "b")),   # empty var -> dropped
    list(var = "race",    label = "y",           levels = list("White")),    # a run of ONE -> dropped
    list(var = "relig",   label = "z",           levels = list("None", ""))  # blank filtered -> one
  )
  spec <- jmvtab_levels_collapse(lc)
  expect_identical(names(spec), "marital")
  expect_identical(spec$marital[["Not married"]], c("Divorced", "Separated"))
  # an empty label is defaulted in R, once (the JS only shows it as a placeholder)
  expect_identical(spec$marital[["Married, Widowed"]], c("Married", "Widowed"))
  expect_null(jmvtab_levels_collapse(list()))
})


test_that("a merge is byte-identical to tab() on pre-collapsed microdata", {
  cases <- list(
    list(o = jmv_opts(row_vars = "marital", col_vars = "race", pct = "row", test = TRUE),
         spec = mar_merge),
    list(o = jmv_opts(row_vars = "marital", col_vars = "tvhours"),               # factor x numeric
         spec = mar_merge),
    list(o = jmv_opts(row_vars = "relig", col_vars = "race", tab_vars = "marital", pct = "row"),
         spec = mar_merge),                                                     # merged sub-table
    list(o = jmv_opts(row_vars = "marital", col_vars = "race", pct = "col"),
         spec = list(race = list("Non-white" = c("Black", "Other"))))           # a COLUMN merge
    # ⚠ the "merge, THEN lump" case left with the control: Phase 22g-vi retired the panel's
    # `other_if_less_than` (the per-variable table merges by hand, and the two fought each other),
    # so jamovi can no longer produce it. The ORDERING itself is tab()'s and is locked in
    # test-row-model.R.
  )
  for (cs in cases) {
    built <- jmvtab_build(gss, utils::modifyList(cs$o, list(levels_collapse = cs$spec)), NULL)
    expect_equal(built$tabs, jmv_oracle(cs$o, re_collapse(gss, cs$spec)))
  }
})


test_that("a merge MISSES tier 1 -- the declared cost of the pre-aggregate ruling", {
  # ⚠ the inverse of the reorder's contract two tests up, and it is the point: the fingerprints in
  # ce$fp_map are taken BEFORE tab() runs, so the tier-1 keys must name the merge spec themselves.
  o <- jmv_opts(row_vars = "marital", col_vars = "race", pct = "row", test = TRUE)
  s <- jmvtab_build(gss, o, NULL)$store
  r <- jmvtab_build(gss, utils::modifyList(o, list(levels_collapse = mar_merge)), s)
  expect_false(r$hits$agg[["marital\rrace"]])      # the aggregate itself changed
  expect_false(isTRUE(r$hits$tab3))
})


test_that("a merge composes with a reorder, and the displayed order is the merged one", {
  # The JS writes the RAW order it shows; jmv_order_after_collapse() maps it onto the merged levels.
  raw   <- list(marital = mar_ord)
  built <- jmvtab_build(gss, utils::modifyList(
    jmv_opts(row_vars = "marital", col_vars = "race", pct = "row"),
    list(levels_order = jmv_order_after_collapse(raw, mar_merge),
         levels_collapse = mar_merge)), NULL)
  lv <- levels(built$tabs$marital)
  expect_identical(lv[1:3], c("Married", "Not married", "Widowed"))
  expect_false("Divorced" %in% lv)
})


test_that("jmv_order_after_collapse(): a run becomes ONE label, first occurrence wins", {
  expect_identical(jmv_order_after_collapse(list(marital = mar_ord), mar_merge)$marital,
                   c("Married", "Not married", "Widowed", "No answer"))
  expect_null(jmv_order_after_collapse(NULL, mar_merge))
  expect_identical(jmv_order_after_collapse(list(marital = mar_ord), NULL)$marital, mar_ord)
})


test_that("levels_collapse = NULL leaves the build byte-identical (no-op)", {
  o <- jmv_opts(row_vars = "marital", col_vars = "race", pct = "row", test = TRUE)
  expect_equal(jmvtab_build(gss, utils::modifyList(o, list(levels_collapse = NULL)), NULL)$tabs,
               jmvtab_build(gss, o, NULL)$tabs)
})


# --- Phase 19k: the boundary defects, each with the fixture that fails without the fix ------

# D11: a MEAN column under `ci = "cell"` rendered EMPTY. jmv_apply_display() forced every non-count
# fmt column to the `pct_ci` display, whose `pct` field is NA on the numeric leaf -- and it ran AFTER
# the display ComboBox, so it also overrode what the user picked. Since 19j the LEAF stamps the right
# display where it builds the cell interval, so the whole block was deletable.
test_that("Phase 19k (D11): ci = 'cell' on a numeric col_var renders the mean, not an empty cell", {
  o <- jmv_opts(row_vars = "marital", col_vars = "tvhours", ci = "cell", conf_level = 0.95)
  tb <- suppressMessages(jmvtab_build(gss, o, NULL))$tabs
  num <- names(tb)[vapply(tb, function(x) is_fmt(x) && est_var_kind(get_scale(x)) == "mean",
                          logical(1))]
  expect_gt(length(num), 0L)
  cells <- format(tb[[num[[1]]]])
  expect_true(all(nzchar(trimws(cells))))          # nothing renders void
  expect_true(any(grepl("[;\u00b1]", cells)))     # ... and the interval is visible
  # ... and the module now agrees with tab() cell for cell, which the forced pct_ci made impossible
  # (a plain tab() has never had that block).
  expect_equal(tb, jmv_oracle(o, gss))
  # the display ComboBox behaves exactly as tab(display =) does on the same table -- whatever that
  # is. (It is `tab()`'s own eligibility rule: a cell already showing a composite keeps it. What the
  # defect was is that jamovi applied a SECOND, different rule on top.)
  o2 <- utils::modifyList(o, list(display = "n"))
  expect_equal(suppressMessages(jmvtab_build(gss, o2, NULL))$tabs, jmv_oracle(o2, gss))
})


# D13: `filter` was hashed as a hardcoded NA_character_, so two calls differing only by their filter
# produced the SAME tier-0 / tier-1 keys -- a filter change never invalidated the aggregate cache.
test_that("Phase 19k (D13): the filter reaches the cache keys", {
  k1 <- tab_cache_keys(row_vars = "marital", col_vars = "race", filter_expr = "year == 2000")
  k2 <- tab_cache_keys(row_vars = "marital", col_vars = "race", filter_expr = "year == 2006")
  k0 <- tab_cache_keys(row_vars = "marital", col_vars = "race")
  expect_false(identical(k1$tier0, k2$tier0))
  expect_false(identical(k1$tier0, k0$tier0))
  # ... and tab() really passes its own filter down (it used to hand tab_cache_keys() a literal NA
  # whatever the user wrote, so the two builds below shared every tier-0/tier-1 key).
  n_of <- function(x) sum(as.numeric(get_n(x[[2]])), na.rm = TRUE)
  expect_false(identical(n_of(tab(gss, marital, race, filter = year == 2000)),
                         n_of(tab(gss, marital, race, filter = year == 2006))))
})


# --- Phase 22g-iii: `shape`, the second PRE-aggregate recode -----------------------------------
# It changes what is COUNTED while `fp_map` fingerprints the raw columns, so it has to be in the
# tier-1 / tier-2 keys or a cut would be served the un-cut aggregate. Both halves are asserted: the
# table is right, and the aggregate was genuinely rebuilt.
test_that("a `shape` cut equals tab() and MISSES tier 1", {
  o1 <- jmv_opts(row_vars = "age", col_vars = "marital", pct = "row",
                 shape = c(age = "terciles"))
  o2 <- utils::modifyList(o1, list(shape = c(age = "quintiles")))
  b1 <- suppressMessages(jmvtab_build(gss, o1, NULL))
  expect_identical(as.character(b1$tabs[[1]]), as.character(suppressMessages(jmv_oracle(o1, gss))[[1]]))
  # 3 groups + NA + Total, then 5 + NA + Total: the cut really reached tab_prepare_pop()
  expect_identical(nrow(b1$tabs), 5L)
  b2 <- suppressMessages(jmvtab_build(gss, o2, b1$store))
  expect_identical(nrow(b2$tabs), 7L)
  expect_false(any(unlist(b2$hits)))                 # a different cut is a different aggregate
  expect_identical(as.character(b2$tabs[[1]]), as.character(suppressMessages(jmv_oracle(o2, gss))[[1]]))
})


# ⚠ A numeric-KEEPING shape renames its column (`sqrt_tvhours`), and `fp[["sqrt_tvhours"]]` is NULL
# -- silently, because `[[` on a list returns NULL for an unknown name. Without ctx$shape_renames the
# SOURCE column's fingerprint would drop out of the key and a data edit would not move it.
test_that("a transformed col_var still carries its SOURCE column's fingerprint", {
  o  <- jmv_opts(row_vars = "marital", col_vars = "tvhours", pct = "no", digits = 2,
                 shape = c(tvhours = "sqrt"))
  b1 <- suppressMessages(jmvtab_build(gss, o, NULL))
  d2 <- dplyr::mutate(gss, tvhours = tvhours * 4)    # same NAME, different DATA
  b2 <- suppressMessages(jmvtab_build(d2, o, b1$store))
  expect_false(any(unlist(b2$hits)))
  expect_equal(get_num(b2$tabs[[2]])[[1]], 2 * get_num(b1$tabs[[2]])[[1]])
})


# === SECTION: the jamovi export helpers ===========================================================

gss <- fx_gss()

tabs <- tab(gss, marital, race, pct = "row")


# --- Export-folder detection & the Documents resolver (Phase 18o) -----------------------
# export_documents_dir() is a robust per-OS known-folder resolver backed by the doc_* detectors (the
# rest of the jmvtest diagnostic toolkit is archived in dev/jamovi/jmvtest.b.R). Detectors must NEVER
# error on any OS (off-platform methods return NA); the resolver must always return one usable dir.

testthat::test_that("every Documents detector returns a single path or NA and never errors", {
  detectors <- list(
    doc_win_reg_shell, doc_win_reg_usershell, doc_win_regexe,
    doc_xdg, doc_xdg_file, doc_home_documents
  )
  for (f in detectors) {
    v <- testthat::expect_no_error(f())
    testthat::expect_true(is.character(v) && length(v) == 1L)     # a single path or NA_character_
  }
  testthat::expect_false(is.na(doc_home_documents()))             # the baseline is always concrete
})


testthat::test_that("export_writable(): existing+writable is TRUE, nonexistent / NA / '' are FALSE", {
  testthat::expect_true(export_writable(tempdir()))
  testthat::expect_false(export_writable(file.path(tempdir(), "no_such_dir_xyz_tabxplor")))
  testthat::expect_false(export_writable(NA_character_))
  testthat::expect_false(export_writable(""))
})


testthat::test_that("export_documents_dir() returns one usable directory and never errors", {
  d <- testthat::expect_no_error(export_documents_dir())
  testthat::expect_true(is.character(d) && length(d) == 1L && !is.na(d) && nzchar(d))
  # usable = exists+writable OR its parent is writable (jmvtab_export creates it)
  testthat::expect_true(export_writable(d) || export_writable(dirname(d)))
})


testthat::test_that("resolveExportPath routes the Documents sentinels through the resolver, real paths not", {
  # Compare like with like on EVERY platform: resolveExportPath returns normalizePath() output
  # (BACKslashes on Windows) but the test reads it through dirname(), which always emits "/". So
  # both sides go through one winslash = "/" normaliser -- otherwise the assertion is unsatisfiable
  # on Windows regardless of what the code does.
  norm_dir <- function(p) normalizePath(p, winslash = "/", mustWork = FALSE)

  # blank / "~" / "~/Documents" / "auto" all mean "my Documents" -> the SAME resolved folder
  dirs <- vapply(c("", "~", "~/Documents", "~/documents", "auto"),
                 function(s) dirname(resolveExportPath(s, "x", "md")), character(1))
  testthat::expect_length(unique(dirs), 1L)
  testthat::expect_identical(unique(dirs), norm_dir(export_documents_dir()))

  # a real typed folder is respected, NOT rerouted to Documents. tempdir(), not a "/tmp/..." literal:
  # a leading-slash path is DRIVE-RELATIVE on Windows and resolves under the current drive.
  typed <- file.path(tempdir(), "tabxplor_xyz")
  testthat::expect_identical(dirname(resolveExportPath(typed, "x", "md")), norm_dir(typed))
  # a real ~-path still expands to the OS home (NOT the Documents sentinel)
  p <- resolveExportPath("~/Desktop", "x", "md")
  testthat::expect_false(grepl("^~", p))
  testthat::expect_match(p, "Desktop")
})


testthat::test_that("jmvtab_export gives a friendly error when the folder can't be created", {
  # a path we can't create -> a clear, actionable message (not a raw connection error).
  # A directory UNDER A REGULAR FILE is uncreatable on Windows, macOS and Linux alike -- unlike the
  # old "/proc/..." fixture, which is only unwritable on Linux (on Windows it is a drive-relative
  # D:\proc\... and creation legitimately succeeds, so the friendly error never fired there).
  f <- withr::local_tempfile()
  writeLines("x", f)
  bad <- file.path(f, "sub", "Table.md")
  testthat::expect_error(jmvtab_export(tabs, "md", bad), "folder", ignore.case = TRUE)
})


# --- "Replace" rule + honest reported path (this-phase) -----------------------------------

testthat::test_that("export_number_path: replace keeps the name, else auto-numbers past existing files", {
  tmp <- withr::local_tempdir()
  p   <- file.path(tmp, "Tableau.xlsx")
  testthat::expect_identical(export_number_path(p, replace = FALSE), p)  # free -> unchanged
  file.create(p)
  testthat::expect_identical(export_number_path(p, replace = TRUE), p)   # replace -> unchanged
  testthat::expect_identical(export_number_path(p, replace = FALSE), file.path(tmp, "Tableau1.xlsx"))
  file.create(file.path(tmp, "Tableau1.xlsx"))
  testthat::expect_identical(export_number_path(p, replace = FALSE), file.path(tmp, "Tableau2.xlsx"))
  # extension-agnostic + robust to a dotted directory
  d2 <- file.path(tmp, "a.b"); dir.create(d2); q <- file.path(d2, "t.md"); file.create(q)
  testthat::expect_identical(export_number_path(q, replace = FALSE), file.path(d2, "t1.md"))
})


testthat::test_that("jmvtab_export honours replace and RETURNS the path really written (md/html/excel)", {
  for (fmt in c("md", "html", "excel")) {
        if (fmt == "excel" && !requireNamespace("openxlsx2", quietly = TRUE)) next
    tmp <- withr::local_tempdir()
    ext <- switch(fmt, md = "md", html = "html", excel = "xlsx")
    p   <- file.path(tmp, paste0("Tableau.", ext))
    a1  <- jmvtab_export(tabs, fmt, p, replace = FALSE)          # first write -> the requested path
    testthat::expect_identical(a1, p)
    testthat::expect_true(file.exists(a1))
    a2  <- jmvtab_export(tabs, fmt, p, replace = FALSE)          # not replacing -> a NEW, numbered file
    testthat::expect_identical(a2, file.path(tmp, paste0("Tableau1.", ext)))
    testthat::expect_true(file.exists(a2))
    a3  <- jmvtab_export(tabs, fmt, p, replace = TRUE)           # replacing -> back to the requested path
    testthat::expect_identical(a3, p)
  }
})


testthat::test_that("export_status_html: bold green success with the path, bold red failure, escaped", {
  ok <- export_status_html("D:/Documents/Tableau1.xlsx", ok = TRUE)
  testthat::expect_match(ok, "font-weight:bold")
  testthat::expect_match(ok, "#1a7f37")                         # green
  testthat::expect_match(ok, "Saved to: ", fixed = TRUE)
  testthat::expect_match(ok, "Tableau1.xlsx", fixed = TRUE)     # the REAL (numbered) path
  bad <- export_status_html("boom <x> & <y>", ok = FALSE)
  testthat::expect_match(bad, "#c62828")                        # red
  testthat::expect_match(bad, "Export failed: ", fixed = TRUE)
  testthat::expect_match(bad, "&lt;x&gt; &amp; &lt;y&gt;", fixed = TRUE)   # HTML-escaped
})


testthat::test_that("a named ref vector drives the reference end-to-end (matches a direct call)", {
  rl  <- list(list(var = "marital", ref = "Divorced"))
  ref <- jmvtab_ref_vector(rl, "auto")
  via_picker <- tab(gss, marital, race, pct = "row", ref = ref)
  direct     <- tab(gss, marital, race, pct = "row", ref = c(marital = "Divorced"))
  testthat::expect_equal(via_picker, direct)
})


testthat::test_that("a metacharacter level label matches exactly (rincome '$25000 or more')", {
  # the reported bug: a raw "$25000 or more" was treated as a (broken) regex, so the reference never
  # shifted. diff_index()'s exact-match-first now selects it literally, and the stored `ref` attribute
  # stays human-readable (no anchored/escaped token leaking into the colour legend).
  gss2    <- dplyr::filter(gss, !is.na(rincome))
  ref     <- jmvtab_ref_vector(list(list(var = "rincome", ref = "$25000 or more")))
  testthat::expect_identical(unname(ref[["rincome"]]), "$25000 or more")   # raw, human-readable
  shifted <- tab(gss2, rincome, race, pct = "row", ref = ref, color = "diff")
  default <- tab(gss2, rincome, race, pct = "row", color = "diff")         # ref = "auto" -> total
  testthat::expect_false(isTRUE(all.equal(shifted, default)))              # the reference moved
})


testthat::test_that("a col_var-named ref drives per-col_var references under pct = 'col'", {
  # .b.R keys the picker by col_var under pct="col"; each col_var gets its OWN reference column.
  ref <- jmvtab_ref_vector(list(list(var = "race",  ref = "Black"),
                                list(var = "relig", ref = "None")))
  testthat::expect_named(ref, c("race", "relig"))
  tc    <- tab(gss, marital, c(race, relig), pct = "col", ref = ref, color = "diff")
  marks <- is_refcol(tc)   # exactly one reference column marked per col_var (Black / None)
  testthat::expect_setequal(names(marks)[marks %in% TRUE], c("Black", "None"))
})


testthat::test_that("the jamovi results content carries the width chrome, once, in front", {
  # ⚠ THE rule the whole results width rests on: jamovi pins an Html result at
  # `.jmv-results-html{width:500px}`, so a table wider than that is reported at the app's 620 px floor
  # and clipped by the iframe. Un-pinning it is what makes the panel size itself from the TABLE.
  # See dev/jamovi_results_width.md.
  out <- jmv_results_content("", jmv_results_scrollbox("<table></table>"))
  testthat::expect_match(out, "^<style>", fixed = FALSE)                     # chrome first
  testthat::expect_match(out, ".jmv-results-html{width:max-content;}", fixed = TRUE)
  testthat::expect_identical(lengths(regmatches(out, gregexpr("<style>", out, fixed = TRUE))), 1L)
  testthat::expect_match(out, "tx-scrollbox", fixed = TRUE)
  # the box hugs the table and is capped only by the runaway guard
  testthat::expect_match(out, "width:max-content;max-width:4000px;overflow-x:auto;", fixed = TRUE)

  # empty / NULL fragments drop out, so a caller passes its status line unconditionally
  testthat::expect_identical(jmv_results_content(NULL, "", "<b>x</b>"),
                             paste0(jmv_results_style(), "<b>x</b>"))
})


testthat::test_that("prose cannot size the panel: every fragment is a tx-note", {
  # a wrapping block's max-content is its WHOLE text on one line, so now that the Html element hugs
  # its content an unconstrained hint would report ~1300 px with no table on screen.
  testthat::expect_match(jmv_results_style(), ".tx-note{max-width:520px;}", fixed = TRUE)
  testthat::expect_match(export_status_html("/a/b.xlsx"), 'class="tx-note"', fixed = TRUE)
  testthat::expect_match(export_status_html("boom", ok = FALSE), 'class="tx-note"', fixed = TRUE)

  # THE gate, read from the source: the two placeholders are private methods of a jmvcore R6 class, so
  # the file is the only reach -- and a backend hand-writing a <div> has bypassed jmv_results_note().
  for (f in c("jmvtab.b.R", "jmvtabreg.b.R")) {
    src <- readLines(src_path("R", f), warn = FALSE)
    testthat::expect_length(grep("<div", src, value = TRUE), 0L)
  }
})


testthat::test_that("every backend setContent() goes through the content boundary", {
  # a new code path writing the Html element directly would silently re-pin the panel at 620 px.
  for (f in c("jmvtab.b.R", "jmvtabreg.b.R")) {
    src <- readLines(src_path("R", f), warn = FALSE)
    one  <- paste(src, collapse = "\n")
    hits <- regmatches(one, gregexpr("html_table[$]setContent[(][[:space:]]*[A-Za-z_.]*", one))[[1]]
    testthat::expect_true(length(hits) > 0)
    testthat::expect_true(all(endsWith(hits, "jmv_results_content")),
                          info = paste(f, paste(hits, collapse = " | ")))
  }
})


testthat::test_that("a table title cannot size a shrink-to-fit container", {
  # `.tabxplor-caption` is a block sibling of the <table>; inside jamovi's max-content scroll box its
  # own max-content (the whole title on one line) would drive the width. Same idiom as `.tx-foot`.
  css <- tab_css()
  testthat::expect_match(css, "\\.tabxplor-caption\\{[^}]*width:0;min-width:100%;\\}")
  testthat::expect_match(css, ".tabxplor-tab .tx-foot{width:0;min-width:100%;}", fixed = TRUE)
})


testthat::test_that("population descriptor encodes each na mode (\u00a73.1)", {
  # keep / drop -> full population (per-pair reuse is widest).
  testthat::expect_identical(
    tabxplor:::tab_cache_keys(na = "keep", row_vars = "a", col_vars = "b")$tier0$population,
    "full")
  testthat::expect_identical(
    tabxplor:::tab_cache_keys(na = "drop", row_vars = "a", col_vars = "b")$tier1_common$population,
    "full")

  # drop_all -> listwise on ALL selected vars (sorted, unique).
  pop_da <- tabxplor:::tab_cache_keys(
    na = "drop_all", row_vars = "marital", col_vars = c("race", "partyid"),
    tab_vars = "year")$tier0$population
  testthat::expect_identical(pop_da$mode, "drop_all")
  testthat::expect_identical(pop_da$vars, sort(c("marital", "race", "partyid", "year")))

  # common_base -> row_var + FIRST col_var + tab_vars (secondary col_vars keep their own NAs).
  pop_cb <- tabxplor:::tab_cache_keys(
    na = "common_base", row_vars = "marital", col_vars = c("race", "partyid"),
    tab_vars = "year")$tier1_common$population
  testthat::expect_identical(pop_cb$mode, "common_base")
  testthat::expect_identical(pop_cb$vars, c("marital", "race", "year"))
})


# === SECTION: the crosstab live-UI cache ==========================================================

withr::local_options(lifecycle_verbosity = "quiet", .local_envir = testthat::teardown_env())


jmv_opts <- function(...) {
  # Phase 19k: the option names + values are tab()'s own -- `chi2` is `test`, the retired `OR` is a
  # `display` + `ref2` route, `ci` speaks the anchor vocabulary, `color` the full measure words.
  o <- list(row_vars = character(), col_vars = character(), tab_vars = character(), wt = character(),
            pct = "no", color = "no", color_signif = "ignore", test = FALSE,
            na = "keep", levels = "all", ref = "auto", ref2 = "first", comp = "tab", ci = "auto",
            conf_level = 0.95, stars = TRUE,   # design_effect: absent -> the global option decides
            # the jamovi UI keeps one ComboBox per interval kind; jmv_ci_method() folds them
            ci_method_cell = "wilson", ci_method_diff = "newcombe",
            ci_method_mean_diff = "welch", ci_method_mean_ratio = "robust",
            totaltab = "line", digits = 0, add_pct = FALSE,
            # 20g-ii: `n_min` was MISSING here although .opts() sets it and it is in the tier-3
            # `reapplied` set -- the fixture must carry every key that vector names, or the D12
            # assertion below cannot see the real invariant.
            n_min = 0,
            subtext = "", output_list = FALSE, cleannames = FALSE, display = "auto",
            anova = "welch",
            # 22g-iii: the per-numeric-variable cut. Like `levels_collapse` it is a PRE-aggregate
            # recode and therefore sits in the tier-1 keys, so the oracle must pass it too.
            shape = NULL,
            # 20g-i: ONE key of the option's own shape (it was three constants mirroring three
            # arguments that no longer exist). `.opts()` fills it with the module's translations.
            total_names = c(row = "Total", col = "Total", tab = "Ensemble", other = "Others"))
  # WARNING: `modifyList()` keeps the FIRST of two same-named entries, and every `o0(...)`/`mk(...)`
  # wrapper below splices its own defaults beside the caller's `...` -- so a later override was
  # silently swallowed. Measured on the pre-fix tree: `o0(color = "ratio")` at L318 built with
  # `color = "difference"`, and the trailing `ref = "1"` at L381 was dead, making case `b` identical to
  # case `a`. Keep the LAST, which is R's ordinary override semantic.
  args <- list(...)
  if (length(args)) args <- args[!duplicated(names(args), fromLast = TRUE)]
  utils::modifyList(o, args)
}


# The no-cache oracle: tab() with jmvtab_build()'s exact arg mapping (dummy vars, color, ci forcing).
jmv_oracle <- function(opts, data) {
  if (length(opts$row_vars) == 0L) { data$no_row_var <- factor("no_row_var"); opts$row_vars <- "no_row_var" }
  if (length(opts$col_vars) == 0L) { data$no_col_var <- factor("n");          opts$col_vars <- "no_col_var" }
  color <- switch(opts$color, "no" = FALSE, "auto" = TRUE, opts$color)
  # The RESOLVED anchor, from the same shared resolver jmvtab_build() calls -- not a hand-mirrored
  # `if (a policy) ci <- "diff"`, which was one more copy of a rule that has moved twice since it was
  # written.
  ci <- resolve_leaf_ci(opts$ci, jmv_tab3_measure(color), opts$color_signif, opts$stars,
                        if (length(opts$ref)) opts$ref else "auto")$ci
  wt_sym <- if (length(opts$wt)) rlang::sym(opts$wt) else NULL
  # Phase 19k: no translation left -- every option is passed under the name tab() gives it.
  # 20b/20g-i: the four synthetic labels are an option, installed exactly as jmv_tab3_build_armed()
  # installs them.
  if (length(opts$total_names)) {
    .old <- options(tabxplor.total_names = tabxplor:::tab_total_names_merge(opts$total_names))
    on.exit(options(.old), add = TRUE)
  }
  rlang::inject(tab(
    data, row_vars = tidyselect::all_of(opts$row_vars), col_vars = tidyselect::all_of(opts$col_vars),
    tab_vars = tidyselect::all_of(opts$tab_vars), wt = !!wt_sym, pct = opts$pct, color = color,
    color_signif = opts$color_signif, display = opts$display, test = opts$test, na = opts$na,
    levels = opts$levels, ref = opts$ref, ref2 = opts$ref2, comp = opts$comp, ci = ci,
    conf_level = opts$conf_level, stars = opts$stars, anova = opts$anova,
    ci_method = c(cell = opts$ci_method_cell, diff = opts$ci_method_diff),
    cleannames = FALSE, totaltab = opts$totaltab, digits = opts$digits,
    n = opts[["n"]], add_pct = opts$add_pct,
    subtext = opts$subtext, output_list = isTRUE(opts$output_list), shape = opts$shape
  ))
}


gss <- fx_gss()


gssw <- dplyr::mutate(gss, w = as.numeric(1 + (as.integer(marital) %% 3)))


# Phase 19m-i: a BINARY col_var. It is the shape on which "which column is the total" changes the
# answer rather than merely the bookkeeping -- two levels + a total is `binary` (each level's odds
# ratio against the OTHER level), three columns counted as levels is not.
gssb <- dplyr::mutate(gss, white = forcats::fct_other(race, keep = "White",
                                                      other_level = "Non-white"))


# --- documented divergences ---------------------------------------------------------------
test_that("cleannames at display: colliding levels stay separate (vs tab() summing)", {
  df <- data.frame(
    g = factor(rep(c("A-Foo", "B-Foo (x)", "C-Bar"), each = 20)),
    y = factor(rep(c("yes", "no"), 30))
  )
  jr <- jmvtab_build(df, jmv_opts(row_vars = "g", col_vars = "y", pct = "row", cleannames = TRUE), NULL)
  ot <- tab(df, g, y, pct = "row", cleannames = TRUE)
  expect_equal(sum(as.character(jr$tabs[["g"]]) == "Foo"), 2)  # jmvtab: two same-labelled rows
  expect_equal(sum(as.character(ot[["g"]])      == "Foo"), 1)  # tab(): one summed row
})


test_that("Phase 7f: tier-3 armed table survives the $state round-trip and is size-bounded", {
  st   <- jmvtab_build(gss, jmv_opts(row_vars = "marital", col_vars = "race", pct = "row",
                                     color = "difference", test = TRUE), NULL)$store
  expect_length(st$tab3, 1L)
  expect_lt(st$tab3[[1]]$bytes, JMVTAB_CFG$entry_bytes[["tab3"]]) # a real survey table fits the ceiling
  back <- unserialize(serialize(st, connection = NULL))          # jamovi $state gzip-RDS round-trip
  # Phase 19d-tail: the toggle is a significance policy, not `color = "ratio"`. What this test is for
  # is "the RESTORED carrier serves a re-paint", and since 19d a diff -> ratio toggle changes the
  # stored interval (Katz vs percentage points), so it rebuilds by design and would prove nothing here.
  o2 <- jmv_opts(row_vars = "marital", col_vars = "race", pct = "row",
                 color = "difference", color_signif = "guaranteed_effect", test = TRUE)
  r <- suppressMessages(jmvtab_build(gss, o2, back))
  expect_true(isTRUE(r$hits$tab3))                               # re-paint from the RESTORED armed table
  expect_equal(r$tabs, suppressMessages(jmv_oracle(o2, gss)))
})


# --- Phase 7g-ii: level reordering (post-aggregate, tier-3 input) -------------------------
# The oracle is a plain tab() on PRE-RELEVELED microdata: the jmvtab reorder relevels the shaped
# aggregate in memory (stored blob stays raw), which must be byte-identical to having fct_relevel'd
# the data before tab(). A reorder reuses tiers 1-2 (aggregate + test) and only rebuilds the fmt.
mar_ord  <- c("Married", "Divorced", "Widowed", "Separated", "Never married", "No answer")


race_ord <- c("White", "Black", "Other", "Not applicable")   # raw first = "Other"


re_relevel <- function(d, spec) {
  for (v in names(spec)) d[[v]] <- forcats::fct_relevel(d[[v]], spec[[v]])
  d
}


# --- Phase 20g-ii: level MERGING (pre-aggregate; the reorder's opposite in every cache respect) ---
# Same free oracle as the reorder -- a plain tab() on microdata the user collapsed themselves -- but
# the tier contract is INVERTED: a merge changes the counts, so it must MISS tier 1.
mar_merge <- list(marital = list("Not married" = c("Never married", "Divorced", "Separated")))


re_collapse <- function(d, spec) {
  for (v in names(spec)) d[[v]] <- forcats::fct_collapse(d[[v]], !!!spec[[v]])
  d
}


# === SECTION: the jamovi export helpers ===========================================================

gss <- fx_gss()


tabs <- tab(gss, marital, race, pct = "row")


testthat::test_that("jamovi html carries hover tooltips by default, and tooltips = FALSE overrides", {
  # Phase 18 (pre-release): the two hard-coded tooltips = FALSE were removed -- both jamovi html
  # paths now follow the option default (tabxplor.tab_kable_tooltips, seeded TRUE). The non-popover
  # attrs ride the native `title=` attribute, so they work with no bootstrap JS in the webview.
  h <- tab_html_string(tabs)
  testthat::expect_match(h, 'data-toggle="tooltip"', fixed = TRUE)
  testthat::expect_match(h, ' title="', fixed = TRUE)
  # the ... override path still works (and is the user's option escape hatch)
  h_off <- tab_html_string(tabs, tooltips = FALSE)
  testthat::expect_no_match(h_off, 'data-toggle="tooltip"', fixed = TRUE)

  # results panel: jmv_backend_render_html only reads wrap_rows/wrap_cols off self$options,
  # so a plain list stands in for the R6 self
  self <- list(options = list(wrap_rows = 35, wrap_cols = 15))
  hr <- as.character(jmv_backend_render_html(self, tabs))
  testthat::expect_match(hr, "tx-scrollbox", fixed = TRUE)               # scroll box intact
  testthat::expect_match(hr, 'data-toggle="tooltip"', fixed = TRUE)      # tooltips on by default
})


testthat::test_that("jmvtab_export writes Markdown", {
  tmp <- withr::local_tempdir()
  p   <- file.path(tmp, "t.md")
  out <- jmvtab_export(tabs, "md", p)
  testthat::expect_true(file.exists(p))
  testthat::expect_identical(out, p)
  lines <- readLines(p)
  testthat::expect_true(any(grepl("\\|", lines)))     # a markdown table row
})


testthat::test_that("jmvtab_export writes self-contained HTML", {
  tmp <- withr::local_tempdir()
  p   <- file.path(tmp, "t.html")
  jmvtab_export(tabs, "html", p)
  testthat::expect_true(file.exists(p))
  txt <- paste(readLines(p), collapse = "\n")
  testthat::expect_true(grepl("<table", txt))
  testthat::expect_true(grepl("<style", txt))
})


# --- Reference-level picker helpers (Phase 7g-iii) ----------------------------------------

testthat::test_that("jmvtab_ref_vector: a chosen level -> named vector; none -> free-text", {
  # one explicit level -> named vector keyed by var (raw label; diff_index matches it exactly),
  # unset entries -> "auto"
  rl <- list(list(var = "marital", ref = "Divorced"), list(var = "race", ref = NULL))
  out <- jmvtab_ref_vector(rl, free_text_ref = "auto")
  testthat::expect_named(out, c("marital", "race"))
  testthat::expect_identical(unname(out[["marital"]]), "Divorced")
  testthat::expect_identical(unname(out[["race"]]), "auto")

  # an explicit "tot" (Total) passes through
  rl_tot <- list(list(var = "marital", ref = "tot"))
  testthat::expect_identical(unname(jmvtab_ref_vector(rl_tot)[["marital"]]), "tot")

  # no explicit level chosen -> fall back to the expert free-text ref
  rl2 <- list(list(var = "marital", ref = NULL), list(var = "race", ref = ""))
  testthat::expect_identical(jmvtab_ref_vector(rl2, "tot"), "tot")

  # empty picker -> free-text
  testthat::expect_identical(jmvtab_ref_vector(list(), "first"), "first")
})


testthat::test_that("tab_resolve_settings returns cache_keys alongside the colour cascade", {
  out <- tabxplor:::tab_resolve_settings(
    color = "diff", ci = "no", chi2 = FALSE, ref = "tot",
    pct_vect = list("row"), col_vars_text = TRUE, totrow = TRUE,
    na = "keep", wt_name = character(), other_if_less_than = 0, comp = "tab",
    tab_vars = character(), row_vars = "marital", col_vars = "race"
  )
  testthat::expect_true("cache_keys" %in% names(out))
  testthat::expect_named(out$cache_keys, c("tier0", "tier1_common", "tier2"))
  # wt absent -> "" (never NA / missing), grain empty for no tab_vars.
  testthat::expect_identical(out$cache_keys$tier0$wt, "")
  testthat::expect_identical(out$cache_keys$tier1_common$grain, character())
})
