# Phase 7e: the jmvtab live-UI multi-tier cache. Tests drive the engine-free jmvtab_build() core
# (no live jamovi session needed) and lock: byte-identity to tab(cleannames = FALSE), tier-1/tier-2
# reuse (add-a-variable, pct/ref toggles), the store round-trip, and the two documented divergences
# (cleannames-at-display collisions, defer_level_merge full-level tests). See
# dev/tabxplor_jmvtab_cache_design.md.

# --- helpers ------------------------------------------------------------------------------
# Phase 20a: this file calls functions deprecated in 2.0.0 on purpose -- what it asserts is their
# arithmetic, which the leaf shares with them and which does NOT go away in 2.1.0.
# ⚠ This quiets the TOP-LEVEL calls only: testthat 3e runs local_reproducible_output() inside
# every test_that(), which forces lifecycle_verbosity = "warning" again, so the in-block calls
# still warn. Migrating them to tab() is the corpus sweep routed to Phase 20h.
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
            totaltab = "line", digits = 0, other_if_less_than = 0, add_n = TRUE, add_pct = FALSE,
            subtext = "", output_list = FALSE, cleannames = FALSE, display = "auto",
            anova = "welch",
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
    other_if_less_than = opts$other_if_less_than, add_n = opts$add_n, add_pct = opts$add_pct,
    subtext = opts$subtext, output_list = isTRUE(opts$output_list)
  ))
}

gss <- forcats::gss_cat
gssw <- dplyr::mutate(gss, w = as.numeric(1 + (as.integer(marital) %% 3)))
# Phase 19m-i: a BINARY col_var. It is the shape on which "which column is the total" changes the
# answer rather than merely the bookkeeping -- two levels + a total is `binary` (each level's odds
# ratio against the OTHER level), three columns counted as levels is not.
gssb <- dplyr::mutate(gss, white = forcats::fct_other(race, keep = "White",
                                                      other_level = "Non-white"))


# --- store primitives ---------------------------------------------------------------------
test_that("store lifecycle: new / migrate / schema mismatch / round-trip", {
  s <- jmv_cache_new()
  expect_identical(s$schema, JMVTAB_CACHE_SCHEMA)
  expect_length(s$agg, 0L)
  expect_identical(jmv_cache_migrate(NULL)$schema, JMVTAB_CACHE_SCHEMA)  # NULL -> fresh
  bad <- s; bad$schema <- 999L
  expect_length(jmv_cache_migrate(bad)$agg, 0L)                          # mismatch -> discarded

  s <- jmv_cache_put(s, "agg", "k1", list(cols = list(n = 1:3), keys = "g"))
  got <- jmv_cache_fetch(s, "agg", "k1")
  expect_true(got$hit)
  expect_identical(got$value$cols$n, 1:3)
  expect_false(jmv_cache_fetch(s, "agg", "nope")$hit)
  # gzip-RDS round-trip preserves the store
  back <- unserialize(serialize(got$store, connection = NULL))
  expect_true(jmv_cache_fetch(back, "agg", "k1")$hit)
})

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


# --- byte-identity to tab() ---------------------------------------------------------------
test_that("cold build == tab(cleannames = FALSE); warm == cold", {
  cases <- list(
    jmv_opts(row_vars = "marital", col_vars = "race", pct = "row", test = TRUE),
    jmv_opts(row_vars = "marital", col_vars = "tvhours", test = TRUE),
    jmv_opts(row_vars = "marital", col_vars = c("race", "tvhours"), pct = "row", test = TRUE),
    jmv_opts(row_vars = "relig", col_vars = "race", tab_vars = "marital", pct = "row"),
    jmv_opts(row_vars = "marital", col_vars = "race", pct = "row", na = "drop"),
    jmv_opts()  # bare table
  )
  for (o in cases) {
    cold <- jmvtab_build(gss, o, NULL)
    warm <- jmvtab_build(gss, o, cold$store)
    expect_equal(cold$tabs, jmv_oracle(o, gss))
    expect_equal(warm$tabs, cold$tabs)
  }
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
  ne <- get_n_eff(t[[which(purrr::map_lgl(t, ~ is_fmt(.) && get_pct_base(.) == "row"))[[1]]]])
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
  expect_identical(unname(disp(jr)), c(rep("pct_ci", 4L), "mean_ci"))
})


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


# --- Phase 7f: tier-3 built-table cache (display / colour re-paint) ------------------------
# A change that touches only display / colour reuses the cached ARMED table (pre-finalize fmt cells)
# and re-paints -- hits$tab3 == TRUE, no O(cells) rebuild -- while staying byte-identical to a fresh
# tab(). A base change (pct / na / levels) or a reference change rebuilds (hits$tab3 == FALSE).
test_that("Phase 7f: display / colour toggles re-use the armed table (warm == cold, tier-3 hit)", {
  # warm (re-paint from a base-warmed store) must equal a cold full-pipeline build; a tier-3 hit
  # means no O(cells) rebuild. (display / cleannames are modelled by the full jmvtab pipeline, not by
  # jmv_oracle = tab(), so they are locked warm==cold; the fresh-tab() lock is the next test.)
  o0   <- function(...) jmv_opts(row_vars = "marital", col_vars = "race", pct = "row",
                                 color = "difference", test = TRUE, ...)
  st   <- jmvtab_build(gss, o0(), NULL)$store
  # Phase 19d-tail: `ratio` is not an exact RE-PAINT -- since 19d the stored interval follows the
  # comparison (percentage-POINT bounds vs Katz log-RR), so painting a ratio over a difference
  # carrier would show the wrong bracket and the wrong stars. Phase 19k: it is a tier-3 HIT again,
  # by the other route -- the RE-REF recomputes the bounds on the other scale (leaf_ci_plain takes
  # `ci_scale`) and restamps the column's `scale` / `ci_method` with them. The gate is the same one
  # every other case has: warm == cold.
  reference <- c(same = TRUE, digits = TRUE, display = TRUE, ratio = TRUE, auto = TRUE,
                 grey = TRUE, color_all = TRUE, cleannames = TRUE)
  toggles <- list(
    "same"       = o0(),
    "digits"     = o0(digits = 2),
    "display"    = o0(display = "n"),
    "ratio"      = o0(color = "ratio"),
    "auto"       = o0(color = "auto"),
    "grey"       = o0(color_signif = "grey_non_signif"),
    "color_all"  = o0(color_signif = "guaranteed_effect"),
    "cleannames" = o0(cleannames = TRUE)
  )
  for (nm in names(toggles)) {
    r    <- suppressMessages(jmvtab_build(gss, toggles[[nm]], st))
    cold <- suppressMessages(jmvtab_build(gss, toggles[[nm]], NULL)$tabs)
    expect_equal(isTRUE(r$hits$tab3), unname(reference[[nm]]), info = paste("tier-3 reuse:", nm))
    expect_equal(r$tabs, cold, info = paste("warm == cold:", nm))
  }
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
