
# === SECTION: the dplyr wall: every verb preserves the class ======================================

withr::local_options(lifecycle_verbosity = "quiet", .local_envir = testthat::teardown_env())


tabs <- tab(fx_gss(), race, marital)


grouped_tabs <- fx_gss() |>
  dplyr::filter(year %in% c(2000, 2014)) |>
  tab(race, marital, year)


# --- Data-driven verb-coverage registry ----------------------------------------------------
# Extensible guardrail for the 2.0.0 refactors (esp. the tab()/tab_many() merge): each verb is
# checked to preserve BOTH tab classes. A failure names the exact verb whose class-preserving
# S3 method is missing/broken. To add a new verb, append one closure here (works identically
# for a flat and a grouped tab) -- see the `/dplyr-method` skill. Complements the explicit
# per-verb tests above.
verb_coverage <- list(
  mutate      = function(x) dplyr::mutate(x, Married = sum(Married)),
  filter      = function(x) dplyr::filter(x, is_totrow(Married)),
  slice       = function(x) dplyr::slice(x, 1:2),
  arrange     = function(x) dplyr::arrange(x, Married),
  distinct    = function(x) dplyr::distinct(x),
  select      = function(x) dplyr::select(x, dplyr::everything()),
  relocate    = function(x) dplyr::relocate(x, Divorced, .after = Married),
  rename_with = function(x) dplyr::rename_with(x, toupper),
  rowwise     = function(x) dplyr::rowwise(x)
)


cov_flat    <- tab(fx_gss(), race, marital)

cov_grouped <- fx_gss() |>
  dplyr::filter(year %in% c(2000, 2014)) |>
  tab(race, marital, year)


for (vname in names(verb_coverage)) {
  local({
    v <- vname
    testthat::test_that(paste0("verb-coverage keeps tabxplor_tab: ", v), {
      testthat::expect_s3_class(verb_coverage[[v]](cov_flat), "tabxplor_tab")
    })
    testthat::test_that(paste0("verb-coverage keeps tabxplor_grouped_tab: ", v), {
      testthat::expect_s3_class(verb_coverage[[v]](cov_grouped), "tabxplor_grouped_tab")
    })
  })
}


# --- Table-attribute survival + class up/down-grade (2.0.0 tab()/tab_many() merge net) -------
# The blocks above check only that the tab CLASS survives a verb; they do NOT check the two
# table-level attributes: `subtext` (the legend) and `chi2` (the test-results tibble that
# Phase 3 renames to `test`). A verb method could silently reset either to its new_tab()
# default and every test above would still pass. These blocks close that hole -- the most
# valuable coverage before the Phase 6 class-model rewrite touches every reattach site.
#
# tab_plain() |> tab_chi2() is the REAL populator of the chi2 attribute (tab(test = TRUE) does
# NOT fill it for simple tables -- see the DESIGN note in test-calculations.R). subtext has no
# lightweight real populator (the subtext= arg stores whole population data), so a sentinel is
# set directly; that still faithfully exercises the carry path (methods do
# `subtext = get_subtext(.data)`). Both attributes are thus non-default, so "survives" is a
# real assertion, not a vacuous empty == empty.
cov_flat_attr <- tab_plain(fx_gss(), race, marital, pct = "row") |> tab_chi2()

attr(cov_flat_attr, "subtext") <- "phase0 sentinel subtext"


cov_grouped_attr <- dplyr::filter(fx_gss(), year %in% c(2000, 2014)) |>
  tab_plain(race, marital, year, pct = "row") |>
  tab_chi2()

attr(cov_grouped_attr, "subtext") <- "phase0 sentinel subtext"


attr_fixtures <- list(tabxplor_tab = cov_flat_attr, tabxplor_grouped_tab = cov_grouped_attr)

for (cls in names(attr_fixtures)) {
  for (vname in names(verb_coverage)) {
    local({
      fx    <- attr_fixtures[[cls]]
      klass <- cls
      v     <- vname
      testthat::test_that(paste0("verb keeps subtext + chi2 (", klass, "): ", v), {
        out <- verb_coverage[[v]](fx)
        testthat::expect_identical(get_subtext(out), get_subtext(fx))
        testthat::expect_identical(get_test(out),    get_test(fx))
      })
    })
  }
}


test_that("tab_spread() keeps the weight footer, and narrows only tab_vars", {
  skip_if_no_gettext()
  d <- fx_gss()[!is.na(fx_gss()$tvhours) & fx_gss()$tvhours > 0, ]
  withr::local_options(list(tabxplor.design_effect = TRUE, tabxplor.lang = "en"))
  flat <- tab(d, marital, race, relig, wt = tvhours, pct = "row")
  wide <- tab(d, marital, race, relig, wt = tvhours, pct = "row", spread_vars = relig)
  expect_identical(tabxplor:::tab_inference_basis(flat), "weights")   # non-vacuous
  expect_identical(tabxplor:::tab_inference_basis(wide), "weights")
  expect_identical(tab_weight_line(wide), tab_weight_line(flat))
  # Phase 19f/19g: the variable MODEL is derived from the columns; `vars` keeps only what none can
  # carry, and the pivot leaves all of it alone.
  v_flat <- tabxplor:::get_vars_attr(flat); v_wide <- tabxplor:::get_vars_attr(wide)
  expect_identical(v_wide$wt,       v_flat$wt)
  expect_identical(tabxplor:::tab_render_vars(wide)$tab_vars, character(0))
})


test_that("tab_weight_line() reads the STORED basis, never the .svy_weights column name", {
  skip_if_no_gettext()
  withr::local_options(list(tabxplor.lang = "en"))
  g <- fx_gss()[!is.na(fx_gss()$tvhours) & fx_gss()$tvhours > 0, ]
  t <- tab(g, marital, race, wt = tvhours)
  expect_match(tab_weight_line(t), "unweighted sample size")           # basis "n" = the default
  # forge the internal design weight name with NO stored inference: the line is DROPPED, the internal
  # name is never printed, and no claim about the intervals is invented.
  v <- get_vars_attr(t); v$wt <- ".svy_weights"
  t2 <- tabxplor:::set_meta_field(tabxplor:::set_vars_attr(t, v), "inference", NULL)
  expect_null(tab_weight_line(t2))
})


# === SECTION: the regression rebuild sites (Phase 18z16-iiiii) =================================

test_that("a weighted tab_reg(tab_vars=) keeps its inference, spread or stacked", {
  skip_if_no_gettext()
  skip_if_not_installed("survey")
  withr::local_options(list(tabxplor.lang = "en"))
  set.seed(11)
  n <- 400L
  d <- tibble::tibble(
    g = factor(sample(c("a", "b"), n, TRUE)),
    s = factor(sample(c("north", "south"), n, TRUE)),
    y = factor(sample(c("no", "yes"), n, TRUE)),
    w = stats::rgamma(n, 2, 2)
  )
  mk <- function(...) suppressMessages(
    tab_reg(d, outcome = "y", predictors = "g", family = "binomial", wt = "w", ...))
  flat <- mk()
  # The auto-spread is the shape that lost everything: it routes through tab_spread(), whose bare
  # new_tab() literal dropped the whole meta, so the table asserted "intervals use the unweighted
  # sample size" while its models came from svyglm. The stacked shape (several models per group) is
  # checked beside it.
  wide <- mk(tab_vars = "s")
  tall <- suppressMessages(tab_reg(d, outcome = "y", predictors = list(m1 = "g", m2 = "g"),
                                   family = "binomial", wt = "w", tab_vars = "s"))
  expect_identical(tabxplor:::tab_inference_basis(flat), "weights")   # non-vacuous
  expect_identical(tabxplor:::tab_inference_basis(wide), "weights")
  expect_identical(tabxplor:::tab_inference_basis(tall), "weights")
  expect_match(tab_weight_line(wide), "account for the weighting")
  expect_identical(tab_weight_line(wide), tab_weight_line(flat))
})


test_that("a split tab_reg()'s columns name the same interval methods as an unsplit one", {
  set.seed(12)
  n <- 300L
  d <- tibble::tibble(
    g = factor(sample(c("a", "b"), n, TRUE)),
    s = factor(sample(c("north", "south"), n, TRUE)),
    y = stats::rnorm(n)
  )
  # ⚠ `empirical = "column"` on both sides: since 22g-ii `tab_vars` resolve the default TRUE to
  # "tooltip", so the split table would carry no crude column and the two method sets could not be
  # compared at all -- which is not what is under test.
  mk <- function(...) suppressMessages(
    tab_reg(d, outcome = "y", predictors = "g", family = "gaussian", empirical = "column", ...))
  # the split branch used to write a THREE-key reduction of the six the unsplit branch writes, so a
  # split gaussian/poisson table's legend could not name the interval its Obs_* columns print.
  # Phase 19b: the methods ride the COLUMNS, so a rebuild site cannot lose them at all.
  meth <- function(t) sort(unique(get_ci_method(t)[purrr::map_lgl(t, is_fmt)]))
  expect_identical(meth(mk(tab_vars = "s")), meth(mk()))
})


test_that("tab_reg() on a survey design keeps the design's degrees of freedom", {
  skip_if_not_installed("survey")
  set.seed(13)
  n <- 400L
  dd <- tibble::tibble(
    psu   = rep(1:10, each = n / 10L),
    strat = rep(1:2,  each = n / 2L),
    w     = stats::rgamma(n, 2, 2),
    g     = factor(sample(c("a", "b", "c"), n, TRUE)),
    y     = factor(sample(c("no", "yes"), n, TRUE))
  )
  des <- survey::svydesign(ids = ~psu, strata = ~strat, weights = ~w, data = dd, nest = TRUE)
  tr <- suppressMessages(
    tab_reg(des, outcome = "y", predictors = "g", family = "binomial", empirical = TRUE))
  tt <- suppressMessages(tab(des, g, y, pct = "row", ci = "cell"))
  # A CROSSTAB's cells all refer to the design's own df, so the table answers with it.
  # (svy_degf() stores it as a double; survey::degf() returns an integer)
  expect_identical(tabxplor:::tab_inference_degf(tt), as.double(survey::degf(des)))
  # A REGRESSION's do not: since 22b-xiii-2 each column carries the df ITS OWN interval was referred
  # to -- `degf + 1 - p`, survey's own rule, which is what confint.svyglm() uses and what the gap SE
  # is recovered with. So the table-level answer is the weakest of those, strictly below the design's,
  # and the design's own df lives in the model record instead (it is what the "Model:" line prints).
  expect_lt(tabxplor:::tab_inference_degf(tr), as.double(survey::degf(des)))
  fit <- suppressWarnings(survey::svyglm(I(y == "yes") ~ g, design = des,
                                         family = stats::quasibinomial()))
  expect_identical(get_degf(tr[["Model_OR"]]), as.double(stats::df.residual(fit)))
  expect_identical(reg_call(tr)$design_degf, as.double(survey::degf(des)))
})


# === SECTION: a stripped table still renders ======================================================

withr::local_options(lifecycle_verbosity = "quiet", .local_envir = testthat::teardown_env())


df <- fx_gss() |> dplyr::filter(!is.na(rincome), rincome != "No answer")


tc <- tab(df, race, marital, pct = "row", color = TRUE, test = TRUE, stars = TRUE,
          subtext = "A note")

# suppressWarnings: tvhours is over-dispersed (~2.04); the dispersion warning is expected and
# unrelated to what these tests exercise.
tr <- suppressWarnings(
  tab_reg(df, outcome = "tvhours", predictors = c("race", "marital"),
          family = "poisson", empirical = TRUE))


strip_attr  <- function(x, a) { attr(x, a) <- NULL; x }

strip_class <- function(x) { class(x) <- c("tbl_df", "tbl", "data.frame"); x }

md          <- function(x) as.character(suppressMessages(tab_export(x, "md", css = FALSE)))


# Exercise print + every export backend, asserting no error. Suggests-guarded backends skip cleanly.
expect_all_backends_ok <- function(x) {
  quiet <- function(expr) capture.output(suppressMessages(expr))  # swallow cat()/message noise
  expect_no_error(quiet(print(x)))
  expect_no_error(quiet(tab_export(x, "md")))
  expect_no_error(quiet(tab_export(x, "html")))
  if (requireNamespace("openxlsx2", quietly = TRUE))
    expect_no_error(quiet(
      tab_xl(x, path = withr::local_tempfile(fileext = ".xlsx"),
             open = FALSE, replace = TRUE)))
}


test_that("a table stripped of `meta` still refers its intervals to the design df", {
  # Phase 20h: tab_ci() IS the subject here -- what this pins is the STEP's degf fallback (it
  # reconstructs the plan from the columns, which is what a wrapper is for), so there is nothing
  # to migrate it to.
  withr::local_options(lifecycle_verbosity = "quiet")   # the subject IS the deprecated call
  # Phase 18z16-iiiii: THE reason `degf` and `basis` left meta$inference for the fmt columns.
  # A number must not depend on a table attribute: `meta` is dropped by any rebuild that does not
  # carry it (two such sites were found in this very phase) and by plenty of ordinary data-frame
  # handling, and tab_ci() then silently fell back to z -- measured 9 % too narrow at 13 PSUs.
  skip_if_not_installed("survey")
  set.seed(41)
  n  <- 320L
  dd <- tibble::tibble(
    psu   = rep(1:8, each = n / 8L),
    strat = rep(1:2, each = n / 2L),
    w     = stats::rgamma(n, 2, 2),
    g     = factor(sample(c("a", "b"), n, TRUE)),
    y     = factor(sample(c("no", "yes"), n, TRUE))
  )
  des <- survey::svydesign(ids = ~psu, strata = ~strat, weights = ~w, data = dd, nest = TRUE)
  t   <- suppressMessages(tab_plain(des, g, y, pct = "row"))
  bare <- strip_attr(t, "meta")
  expect_null(tabxplor:::get_meta(bare))                          # non-vacuous: the metadata is gone
  # ... and the fact survives anyway, because it rides the columns
  expect_identical(tabxplor:::tab_inference_basis(bare), "design")
  expect_identical(tabxplor:::tab_inference_degf(bare), as.double(survey::degf(des)))
  ci_full <- suppressMessages(tab_ci(t,    ci = "cell"))
  ci_bare <- suppressMessages(tab_ci(bare, ci = "cell"))
  expect_identical(get_ci_inf(ci_bare[["yes"]]), get_ci_inf(ci_full[["yes"]]))
  # and t(degf) is genuinely WIDER than the z the stripped table used to fall back to
  ci_z <- suppressMessages(tab_ci(bare, ci = "cell", degf = Inf))
  hw   <- function(x) (get_ci_sup(x) - get_ci_inf(x)) / 2
  ok   <- is.finite(hw(ci_bare[["yes"]])) & is.finite(hw(ci_z[["yes"]]))
  expect_gt(sum(hw(ci_bare[["yes"]])[ok] > hw(ci_z[["yes"]])[ok] + 1e-9), 0L)
})


# === SECTION: the dplyr wall: every verb preserves the class ======================================

withr::local_options(lifecycle_verbosity = "quiet", .local_envir = testthat::teardown_env())


tabs <- tab(fx_gss(), race, marital)


grouped_tabs <- fx_gss() |>
  dplyr::filter(year %in% c(2000, 2014)) |>
  tab(race, marital, year)


# --- Data-driven verb-coverage registry ----------------------------------------------------
# Extensible guardrail for the 2.0.0 refactors (esp. the tab()/tab_many() merge): each verb is
# checked to preserve BOTH tab classes. A failure names the exact verb whose class-preserving
# S3 method is missing/broken. To add a new verb, append one closure here (works identically
# for a flat and a grouped tab) -- see the `/dplyr-method` skill. Complements the explicit
# per-verb tests above.
verb_coverage <- list(
  mutate      = function(x) dplyr::mutate(x, Married = sum(Married)),
  filter      = function(x) dplyr::filter(x, is_totrow(Married)),
  slice       = function(x) dplyr::slice(x, 1:2),
  arrange     = function(x) dplyr::arrange(x, Married),
  distinct    = function(x) dplyr::distinct(x),
  select      = function(x) dplyr::select(x, dplyr::everything()),
  relocate    = function(x) dplyr::relocate(x, Divorced, .after = Married),
  rename_with = function(x) dplyr::rename_with(x, toupper),
  rowwise     = function(x) dplyr::rowwise(x)
)


cov_flat    <- tab(fx_gss(), race, marital)


cov_grouped <- fx_gss() |>
  dplyr::filter(year %in% c(2000, 2014)) |>
  tab(race, marital, year)


for (vname in names(verb_coverage)) {
  local({
    v <- vname
    testthat::test_that(paste0("verb-coverage keeps tabxplor_tab: ", v), {
      testthat::expect_s3_class(verb_coverage[[v]](cov_flat), "tabxplor_tab")
    })
    testthat::test_that(paste0("verb-coverage keeps tabxplor_grouped_tab: ", v), {
      testthat::expect_s3_class(verb_coverage[[v]](cov_grouped), "tabxplor_grouped_tab")
    })
  })
}


# --- Table-attribute survival + class up/down-grade (2.0.0 tab()/tab_many() merge net) -------
# The blocks above check only that the tab CLASS survives a verb; they do NOT check the two
# table-level attributes: `subtext` (the legend) and `chi2` (the test-results tibble that
# Phase 3 renames to `test`). A verb method could silently reset either to its new_tab()
# default and every test above would still pass. These blocks close that hole -- the most
# valuable coverage before the Phase 6 class-model rewrite touches every reattach site.
#
# tab_plain() |> tab_chi2() is the REAL populator of the chi2 attribute (tab(test = TRUE) does
# NOT fill it for simple tables -- see the DESIGN note in test-calculations.R). subtext has no
# lightweight real populator (the subtext= arg stores whole population data), so a sentinel is
# set directly; that still faithfully exercises the carry path (methods do
# `subtext = get_subtext(.data)`). Both attributes are thus non-default, so "survives" is a
# real assertion, not a vacuous empty == empty.
cov_flat_attr <- tab_plain(fx_gss(), race, marital, pct = "row") |> tab_chi2()


attr(cov_flat_attr, "subtext") <- "phase0 sentinel subtext"


cov_grouped_attr <- dplyr::filter(fx_gss(), year %in% c(2000, 2014)) |>
  tab_plain(race, marital, year, pct = "row") |>
  tab_chi2()


attr(cov_grouped_attr, "subtext") <- "phase0 sentinel subtext"


attr_fixtures <- list(tabxplor_tab = cov_flat_attr, tabxplor_grouped_tab = cov_grouped_attr)


for (cls in names(attr_fixtures)) {
  for (vname in names(verb_coverage)) {
    local({
      fx    <- attr_fixtures[[cls]]
      klass <- cls
      v     <- vname
      testthat::test_that(paste0("verb keeps subtext + chi2 (", klass, "): ", v), {
        out <- verb_coverage[[v]](fx)
        testthat::expect_identical(get_subtext(out), get_subtext(fx))
        testthat::expect_identical(get_test(out),    get_test(fx))
      })
    })
  }
}


# ---- Phase 17a janitorial fixes: failing-first fixture ----

test_that("grouped ptype2 reconciles BOTH operands' attributes (Defect 4, Phase 17a)", {
  # gtab_ptype2()/gtab_cast() used to take attributes from a single side (tab_attrs(x)/tab_attrs(to)),
  # unlike the plain path (tab_bind_attrs). So the `test` block and `subtext` of the other operand
  # were lost. They now reconcile both sides: `test` row-bound (vec_rbind), `subtext` unioned.
  g1 <- tab(dplyr::filter(fx_gss(), year %in% 2000), marital, race, year, test = TRUE)
  g2 <- tab(dplyr::filter(fx_gss(), year %in% 2006), marital, race, year, test = TRUE)
  expect_equal(nrow(get_test(g1)), 1L)
  attr(g1, "subtext") <- "AAA"
  attr(g2, "subtext") <- "BBB"

  p <- gtab_ptype2(g1, g2)
  expect_s3_class(p, "tabxplor_grouped_tab")
  expect_equal(nrow(get_test(p)), 2L)                    # both test blocks survive
  expect_setequal(get_subtext(p), c("AAA", "BBB"))       # subtext unioned
})


test_that("bind reconciles color_breaks per named scale", {
  a <- set_color_breaks_attr(new_tab(tibble::tibble(x = 1:2)),
                             resolve_color_breaks_arg(list(pct_diff = c(0.05, 0.1, 0.2))))
  b <- set_color_breaks_attr(new_tab(tibble::tibble(x = 3:4)),
                             resolve_color_breaks_arg(list(pct_ratio = list(over = 2))))
  merged <- tab_meta_bind(get_meta(a), get_meta(b))$color_breaks
  expect_true(all(c("pct_diff", "pct_ratio") %in% names(merged)))   # both scales survive the bind
})


test_that("a stored caption precedes reg_title in the markdown export", {
  t   <- tab(fx_gss(), marital, race)
  md0 <- tab_md(t, print = FALSE)
  md1 <- tab_md(set_caption(t, "STORED CAP"), print = FALSE)
  expect_false(grepl("STORED CAP", md0, fixed = TRUE))
  expect_true(grepl("STORED CAP", md1, fixed = TRUE))     # rendered as a pandoc caption line
})


# === SECTION: meta must SURVIVE every table rebuild (Phase 18z16-iv, W-A) ======================
# THE guard is field-AGNOSTIC on purpose: it stamps a sub-field that no constructor, no getter and no
# bind rule knows about. Any re-enumeration of `meta` (a fresh `meta = list(a, b, c)` literal in a
# rebuilder) drops it and fails here -- which is exactly how meta$inference was lost in tab_compact(),
# where it inverted the footer sentence and cost the exported step path its `degf`.

test_that("every table rebuild carries an UNKNOWN meta sub-field (no re-enumeration)", {
  withr::local_options(list(lifecycle_verbosity = "quiet"))   # tab_transpose() is soft-deprecated
  probe <- function(x) tabxplor:::set_meta_field(x, "zz_probe", list(kept = TRUE))
  kept  <- function(x) tabxplor:::get_meta(x)[["zz_probe"]]
  tl <- tab(fx_gss(), c(marital, relig), race, pct = "row", output_list = TRUE)
  expect_null(kept(tl[[1]]))                                     # non-vacuous: absent before
  tl <- purrr::map(tl, probe)
  expect_identical(kept(tab_compact(tl)),                    list(kept = TRUE))  # the >=2 row_var merge
  expect_identical(kept(dplyr::bind_rows(tl[[1]], tl[[1]])), list(kept = TRUE))  # the vctrs reconcile
  expect_identical(kept(tab_transpose(tl[[1]])),             list(kept = TRUE))  # rewrites vars only
  expect_identical(kept(dplyr::filter(tl[[1]], TRUE)),       list(kept = TRUE))  # a dplyr verb
  # Phase 18z16-iiiii (defect 1): tab_spread() -- exported, AND what tab(spread_vars =) calls --
  # ended in a bare new_tab(tabs, subtext =, test =) literal, so EVERY spread table silently lost its
  # whole meta. It was the SECOND rebuild-from-a-literal site, which z16-iv's record said did not exist.
  ts <- probe(tab(fx_gss(), marital, race, relig, pct = "row"))
  expect_identical(kept(tab_spread(ts, relig)), list(kept = TRUE))
})


test_that("bind_rows() on two GROUPED tabs keeps subtext / test / meta (Phase 19a, D16)", {
  # THE FIFTH instance of "a rebuild site drops table-level facts", and the one that took the
  # 15-verb carrier score from 14/15 to 15/15. dplyr's dplyr_reconstruct generic runs `data` through
  # dplyr_new_data_frame() BEFORE dispatch, so the method received a payload with no attributes at
  # all; restoring from it gave back a correctly-classed grouped tab carrying NOTHING -- no weight
  # footer, no CI legend, no inference basis, no test summary. Fixed by restoring from `template`.
  # It is ALSO the only carrier on that path: dplyr's own vec_ptype2.grouped_df.grouped_df wins over
  # vec_ptype2.tabxplor_grouped_tab.tabxplor_grouped_tab, which a bind therefore never reaches.
  g <- tab(fx_gss(), marital, race, relig, pct = "row", test = TRUE)
  expect_s3_class(g, "tabxplor_grouped_tab")                       # non-vacuous: really grouped
  expect_false(is.null(attr(g, "subtext", exact = TRUE)))          # ... and really populated
  expect_false(is.null(tabxplor:::get_test(g)))
  expect_false(is.null(tabxplor:::get_meta(g)))

  b <- dplyr::bind_rows(g, g)
  expect_s3_class(b, "tabxplor_grouped_tab")
  expect_identical(attr(b, "subtext", exact = TRUE), attr(g, "subtext", exact = TRUE))
  expect_identical(tabxplor:::get_test(b), tabxplor:::get_test(g))
  expect_identical(tabxplor:::get_vars_attr(b), tabxplor:::get_vars_attr(g))

  # and the field-AGNOSTIC probe, so a future `meta` sub-field is covered without an edit here
  p <- tabxplor:::set_meta_field(g, "zz_probe", list(kept = TRUE))
  expect_identical(tabxplor:::get_meta(dplyr::bind_rows(p, p))[["zz_probe"]], list(kept = TRUE))
})


test_that("a >=2 row_var table keeps meta$inference (the footer cannot invert)", {
  skip_if_no_gettext()
  d <- fx_gss()[!is.na(fx_gss()$tvhours) & fx_gss()$tvhours > 0, ]
  withr::local_options(list(tabxplor.design_effect = TRUE, tabxplor.lang = "en"))
  one <- tab(d, marital, race, wt = tvhours, pct = "row")
  two <- tab(d, c(marital, relig), race, wt = tvhours, pct = "row")
  expect_identical(tabxplor:::tab_inference_basis(one), "weights")   # non-vacuous
  expect_identical(tabxplor:::tab_inference_basis(two), "weights")
  # the sentence the merged table prints must be the one the numbers earned
  expect_identical(tab_weight_line(two), tab_weight_line(one))
  expect_match(tab_weight_line(two), "account for the weighting")
})


test_that("the weakest-claim rule lives on the COLUMN reconcile now", {
  # Phase 18z16-iiiii: `inference` left `meta` for two per-column attributes, so the bind algebra
  # left tab_inference_bind() for vec_ptype2.tabxplor_fmt.tabxplor_fmt() -- where it fires on every
  # c() / bind / group without anyone having to call it.
  mk <- function(b, d = NA_real_) tabxplor:::set_degf(tabxplor:::set_basis(fmt(1:2), b), d)
  bs <- function(x, y) tabxplor:::get_basis(c(x, y))
  expect_identical(bs(mk("design"),         mk("weights")), "weights")
  expect_identical(bs(mk("weights"),        mk("design")),  "weights")   # symmetric
  expect_identical(bs(mk("n"),              mk("design")),  "n")
  expect_identical(bs(mk("design_partial"), mk("design")),  "design_partial")
  expect_identical(bs(mk("design"),         mk("design")),  "design")
  # the widest critical value wins: the SMALLEST design df survives a bind
  expect_identical(tabxplor:::fmt_degf_attr(c(mk("design", 30), mk("design", 12))), 12)
  expect_identical(tabxplor:::fmt_degf_attr(c(mk("design", 30), mk("design"))), 30)  # NA is not a claim
  expect_true(is.na(tabxplor:::fmt_degf_attr(c(mk("design"), mk("design")))))
  # and it reaches a whole TABLE through the derived reader
  t <- tab(fx_gss(), marital, race, pct = "row")
  expect_identical(tabxplor:::tab_inference_basis(t), "n")
  expect_identical(tabxplor:::tab_inference_degf(t), Inf)
})


# === SECTION: a stripped table still renders ======================================================

withr::local_options(lifecycle_verbosity = "quiet", .local_envir = testthat::teardown_env())


df <- fx_gss() |> dplyr::filter(!is.na(rincome), rincome != "No answer")


tc <- tab(df, race, marital, pct = "row", color = TRUE, test = TRUE, stars = TRUE,
          subtext = "A note")


# suppressWarnings: tvhours is over-dispersed (~2.04); the dispersion warning is expected and
# unrelated to what these tests exercise.
tr <- suppressWarnings(
  tab_reg(df, outcome = "tvhours", predictors = c("race", "marital"),
          family = "poisson", empirical = TRUE))


strip_attr  <- function(x, a) { attr(x, a) <- NULL; x }


strip_class <- function(x) { class(x) <- c("tbl_df", "tbl", "data.frame"); x }


md          <- function(x) as.character(suppressMessages(tab_export(x, "md", css = FALSE)))


# Exercise print + every export backend, asserting no error. Suggests-guarded backends skip cleanly.
expect_all_backends_ok <- function(x) {
  quiet <- function(expr) capture.output(suppressMessages(expr))  # swallow cat()/message noise
  expect_no_error(quiet(print(x)))
  expect_no_error(quiet(tab_export(x, "md")))
  expect_no_error(quiet(tab_export(x, "html")))
  if (requireNamespace("openxlsx2", quietly = TRUE))
    expect_no_error(quiet(
      tab_xl(x, path = withr::local_tempfile(fileext = ".xlsx"),
             open = FALSE, replace = TRUE)))
}


# Phase 19m-i: "is this a regression" (the STORED kind) and "does it still carry its model recipe"
# are two questions, and a meta-stripped table is exactly where they diverge. Three sites asked the
# second while claiming to ask the first; one of them said so out loud in its abort message.
test_that("a meta-stripped regression keeps its KIND, and each consumer says which question it asks", {
  no_meta <- strip_attr(tr, "meta")
  expect_true(tab_is_reg(no_meta))                       # the kind survives (it rides `test`)
  expect_null(reg_call(no_meta))                         # the recipe does not

  # the plot axis word comes from the COLUMN (model_family + scale), so it survives the strip
  mcol <- grep("^Model_", names(no_meta), value = TRUE)[[1]]
  expect_identical(reg_eff_word_of(no_meta, mcol), reg_eff_word_of(tr, mcol))
  expect_false(is.na(reg_eff_word_of(no_meta, mcol)))

  # reg_check_plots() refits, so it genuinely needs the recipe -- but it must say THAT, not "this is
  # not a tab_reg() table". A crosstab still gets the other message.
  expect_error(reg_check_plots(no_meta, df), "model record")
  expect_error(reg_check_plots(tc, df), "not a")

  # the "Model:" line has nothing to describe, and correctly writes none
  expect_identical(reg_model_lines(no_meta), character(0))
})


test_that("tab_render_vars / tab_get_vars degrade without error on degenerate frames", {
  fmt_less <- tibble::tibble(a = 1:3)
  fct_less <- tibble::tibble(a = tc[[2]])                # an fmt column, no factor

  expect_no_error(rv <- tabxplor:::tab_render_vars(fmt_less))
  expect_true(isTRUE(rv$degrade))
  expect_no_error(tab_get_vars(fmt_less))

  expect_no_error(rv2 <- tabxplor:::tab_render_vars(fct_less))
  expect_true(isTRUE(rv2$degrade))
  expect_no_error(gv <- tab_get_vars(fct_less))
  expect_length(gv$row_var, 0L)                          # no stray NULL row_var
})


test_that("binding tables tolerates a missing `test` attribute", {
  a <- tab(df, race, marital, pct = "row", test = TRUE)
  b <- strip_attr(a, "test")
  expect_no_error(dplyr::bind_rows(a, b))
  expect_no_error(vctrs::vec_rbind(a, b))
})


# === Phase 22g-vii: what a downgraded table still knows about itself ===============================

testthat::test_that("a regression is recognised by its COLUMNS once meta and test are gone", {
  d <- fx_gss() |>
    dplyr::mutate(married = factor(dplyr::if_else(marital == "Married", "Married", "Not married")))
  t <- suppressMessages(tab_reg(d, "married", c("race", "relig"), family = "binomial"))
  bare <- tibble::as_tibble(t)
  # `role` is a per-COLUMN fmt attribute, so it outlives every table attribute dplyr drops
  testthat::expect_identical(tabxplor:::tab_kind(bare), "regression")
  testthat::expect_identical(tabxplor:::tab_kind(tibble::as_tibble(
    tab(fx_gss(), race, marital, pct = "row"))), "crosstab")
  # ... which is what keeps its base-count column on export
  testthat::expect_match(as.character(tab_html(bare)), ">n</th>", fixed = TRUE)
})


testthat::test_that("the rules between row_var blocks survive a class strip", {
  tc  <- tab(fx_gss(), c(race, marital), partyid, pct = "row")
  bare <- tc
  class(bare) <- c("tbl_df", "tbl", "data.frame")
  n_rule <- function(x) {
    h <- sub("^.*</style>", "", as.character(tab_html(x)))
    lengths(regmatches(h, gregexpr("tx-bb2", h)))
  }
  # they used to ride the dplyr grouping, which a strip silently flattens to one group
  testthat::expect_gt(n_rule(tc), 1L)
  testthat::expect_identical(n_rule(bare), n_rule(tc))
})


# === SECTION: the dplyr wall: every verb preserves the class ======================================

withr::local_options(lifecycle_verbosity = "quiet", .local_envir = testthat::teardown_env())


tabs <- tab(fx_gss(), race, marital)


grouped_tabs <- fx_gss() |>
  dplyr::filter(year %in% c(2000, 2014)) |>
  tab(race, marital, year)


# --- Data-driven verb-coverage registry ----------------------------------------------------
# Extensible guardrail for the 2.0.0 refactors (esp. the tab()/tab_many() merge): each verb is
# checked to preserve BOTH tab classes. A failure names the exact verb whose class-preserving
# S3 method is missing/broken. To add a new verb, append one closure here (works identically
# for a flat and a grouped tab) -- see the `/dplyr-method` skill. Complements the explicit
# per-verb tests above.
verb_coverage <- list(
  mutate      = function(x) dplyr::mutate(x, Married = sum(Married)),
  filter      = function(x) dplyr::filter(x, is_totrow(Married)),
  slice       = function(x) dplyr::slice(x, 1:2),
  arrange     = function(x) dplyr::arrange(x, Married),
  distinct    = function(x) dplyr::distinct(x),
  select      = function(x) dplyr::select(x, dplyr::everything()),
  relocate    = function(x) dplyr::relocate(x, Divorced, .after = Married),
  rename_with = function(x) dplyr::rename_with(x, toupper),
  rowwise     = function(x) dplyr::rowwise(x)
)


cov_flat    <- tab(fx_gss(), race, marital)


cov_grouped <- fx_gss() |>
  dplyr::filter(year %in% c(2000, 2014)) |>
  tab(race, marital, year)


for (vname in names(verb_coverage)) {
  local({
    v <- vname
    testthat::test_that(paste0("verb-coverage keeps tabxplor_tab: ", v), {
      testthat::expect_s3_class(verb_coverage[[v]](cov_flat), "tabxplor_tab")
    })
    testthat::test_that(paste0("verb-coverage keeps tabxplor_grouped_tab: ", v), {
      testthat::expect_s3_class(verb_coverage[[v]](cov_grouped), "tabxplor_grouped_tab")
    })
  })
}


# --- Table-attribute survival + class up/down-grade (2.0.0 tab()/tab_many() merge net) -------
# The blocks above check only that the tab CLASS survives a verb; they do NOT check the two
# table-level attributes: `subtext` (the legend) and `chi2` (the test-results tibble that
# Phase 3 renames to `test`). A verb method could silently reset either to its new_tab()
# default and every test above would still pass. These blocks close that hole -- the most
# valuable coverage before the Phase 6 class-model rewrite touches every reattach site.
#
# tab_plain() |> tab_chi2() is the REAL populator of the chi2 attribute (tab(test = TRUE) does
# NOT fill it for simple tables -- see the DESIGN note in test-calculations.R). subtext has no
# lightweight real populator (the subtext= arg stores whole population data), so a sentinel is
# set directly; that still faithfully exercises the carry path (methods do
# `subtext = get_subtext(.data)`). Both attributes are thus non-default, so "survives" is a
# real assertion, not a vacuous empty == empty.
cov_flat_attr <- tab_plain(fx_gss(), race, marital, pct = "row") |> tab_chi2()


attr(cov_flat_attr, "subtext") <- "phase0 sentinel subtext"


cov_grouped_attr <- dplyr::filter(fx_gss(), year %in% c(2000, 2014)) |>
  tab_plain(race, marital, year, pct = "row") |>
  tab_chi2()


attr(cov_grouped_attr, "subtext") <- "phase0 sentinel subtext"


attr_fixtures <- list(tabxplor_tab = cov_flat_attr, tabxplor_grouped_tab = cov_grouped_attr)


for (cls in names(attr_fixtures)) {
  for (vname in names(verb_coverage)) {
    local({
      fx    <- attr_fixtures[[cls]]
      klass <- cls
      v     <- vname
      testthat::test_that(paste0("verb keeps subtext + chi2 (", klass, "): ", v), {
        out <- verb_coverage[[v]](fx)
        testthat::expect_identical(get_subtext(out), get_subtext(fx))
        testthat::expect_identical(get_test(out),    get_test(fx))
      })
    })
  }
}


testthat::test_that("group_split on a grouped tab returns class-preserving tabs", {
  parts <- dplyr::group_split(cov_grouped_attr)
  testthat::expect_true(all(vapply(parts, is_tab, logical(1))))
})


test_that("meta (vars / render_extras) survives a dplyr pipeline", {
  t <- tab(fx_gss(), marital, race, ci = "auto")
  out <- t |>
    dplyr::filter(TRUE) |>
    dplyr::mutate(.zzz = 1) |>
    dplyr::arrange(dplyr::desc(.data[[names(t)[[1]]]])) |>
    dplyr::select(-".zzz")
  expect_false(is.null(get_vars_attr(out)))
  expect_false(is.null(get_render_extras(out)))
  expect_identical(get_vars_attr(out), get_vars_attr(t))
})


test_that("set_render_extras(NULL) clears ONLY render_extras, keeping vars", {
  t <- tab(fx_gss(), marital, race, ci = "auto")
  expect_false(is.null(get_render_extras(t)))
  t2 <- set_render_extras(t, NULL)
  expect_null(get_render_extras(t2))
  expect_false(is.null(get_vars_attr(t2)))          # untouched
})


test_that("color_breaks now rides meta -> survives a dplyr chain (defect 7)", {
  t <- tab(fx_gss(), marital, race,
           color_breaks = list(pct_diff = c(0.05, 0.1, 0.2)))
  expect_false(is.null(get_color_breaks_attr(t)))
  t2 <- t |> dplyr::filter(TRUE) |> dplyr::mutate(.z = 1) |> dplyr::select(-".z")
  expect_false(is.null(get_color_breaks_attr(t2)))   # was dropped pre-17b
  expect_identical(get_color_breaks_attr(t2), get_color_breaks_attr(t))
})


test_that("an UNWEIGHTED merge still carries no inference (absent-when-unset)", {
  m <- tab(fx_gss(), c(marital, relig), race, pct = "row")
  expect_false("inference" %in% names(tabxplor:::get_meta(m)))
})


# === SECTION: a stripped table still renders ======================================================

withr::local_options(lifecycle_verbosity = "quiet", .local_envir = testthat::teardown_env())


df <- fx_gss() |> dplyr::filter(!is.na(rincome), rincome != "No answer")


tc <- tab(df, race, marital, pct = "row", color = TRUE, test = TRUE, stars = TRUE,
          subtext = "A note")


# suppressWarnings: tvhours is over-dispersed (~2.04); the dispersion warning is expected and
# unrelated to what these tests exercise.
tr <- suppressWarnings(
  tab_reg(df, outcome = "tvhours", predictors = c("race", "marital"),
          family = "poisson", empirical = TRUE))


strip_attr  <- function(x, a) { attr(x, a) <- NULL; x }


strip_class <- function(x) { class(x) <- c("tbl_df", "tbl", "data.frame"); x }


md          <- function(x) as.character(suppressMessages(tab_export(x, "md", css = FALSE)))


# Exercise print + every export backend, asserting no error. Suggests-guarded backends skip cleanly.
expect_all_backends_ok <- function(x) {
  quiet <- function(expr) capture.output(suppressMessages(expr))  # swallow cat()/message noise
  expect_no_error(quiet(print(x)))
  expect_no_error(quiet(tab_export(x, "md")))
  expect_no_error(quiet(tab_export(x, "html")))
  if (requireNamespace("openxlsx2", quietly = TRUE))
    expect_no_error(quiet(
      tab_xl(x, path = withr::local_tempfile(fileext = ".xlsx"),
             open = FALSE, replace = TRUE)))
}


test_that("stripping any single table-level attribute never errors (crosstab, mean, reg)", {
  tm <- tab(df, race, tvhours, color = TRUE, test = TRUE)
  for (base in list(tc, tm, tr)) {
    for (a in c("test", "meta", "subtext")) {
      expect_all_backends_ok(strip_attr(base, a))
    }
  }
})


test_that("losing `subtext` drops the note and nothing else", {
  expect_true(any(grepl("A note", md(tc))))
  no_sub <- md(strip_attr(tc, "subtext"))
  expect_false(any(grepl("A note", no_sub)))
  expect_true(any(grepl("pvalue", no_sub)))              # summary still there
})


test_that("a regression losing `meta` drops its title/effect wording, keeps the cells", {
  full <- md(tr)
  expect_true(any(grepl("Poisson regression", full)))    # caption + Model: line
  no_meta <- md(strip_attr(tr, "meta"))
  expect_false(any(grepl("^: Poisson regression", no_meta)))  # caption gone
  expect_true(any(grepl("Model_IRR", no_meta)))          # the estimate columns remain
  expect_true(any(grepl("\\{\\.[pm][1-4]", no_meta)))    # colours still there
})


# === SECTION: the dplyr wall: every verb preserves the class ======================================

withr::local_options(lifecycle_verbosity = "quiet", .local_envir = testthat::teardown_env())


tabs <- tab(fx_gss(), race, marital)


grouped_tabs <- fx_gss() |>
  dplyr::filter(year %in% c(2000, 2014)) |>
  tab(race, marital, year)


# --- Data-driven verb-coverage registry ----------------------------------------------------
# Extensible guardrail for the 2.0.0 refactors (esp. the tab()/tab_many() merge): each verb is
# checked to preserve BOTH tab classes. A failure names the exact verb whose class-preserving
# S3 method is missing/broken. To add a new verb, append one closure here (works identically
# for a flat and a grouped tab) -- see the `/dplyr-method` skill. Complements the explicit
# per-verb tests above.
verb_coverage <- list(
  mutate      = function(x) dplyr::mutate(x, Married = sum(Married)),
  filter      = function(x) dplyr::filter(x, is_totrow(Married)),
  slice       = function(x) dplyr::slice(x, 1:2),
  arrange     = function(x) dplyr::arrange(x, Married),
  distinct    = function(x) dplyr::distinct(x),
  select      = function(x) dplyr::select(x, dplyr::everything()),
  relocate    = function(x) dplyr::relocate(x, Divorced, .after = Married),
  rename_with = function(x) dplyr::rename_with(x, toupper),
  rowwise     = function(x) dplyr::rowwise(x)
)


cov_flat    <- tab(fx_gss(), race, marital)


cov_grouped <- fx_gss() |>
  dplyr::filter(year %in% c(2000, 2014)) |>
  tab(race, marital, year)


for (vname in names(verb_coverage)) {
  local({
    v <- vname
    testthat::test_that(paste0("verb-coverage keeps tabxplor_tab: ", v), {
      testthat::expect_s3_class(verb_coverage[[v]](cov_flat), "tabxplor_tab")
    })
    testthat::test_that(paste0("verb-coverage keeps tabxplor_grouped_tab: ", v), {
      testthat::expect_s3_class(verb_coverage[[v]](cov_grouped), "tabxplor_grouped_tab")
    })
  })
}


# --- Table-attribute survival + class up/down-grade (2.0.0 tab()/tab_many() merge net) -------
# The blocks above check only that the tab CLASS survives a verb; they do NOT check the two
# table-level attributes: `subtext` (the legend) and `chi2` (the test-results tibble that
# Phase 3 renames to `test`). A verb method could silently reset either to its new_tab()
# default and every test above would still pass. These blocks close that hole -- the most
# valuable coverage before the Phase 6 class-model rewrite touches every reattach site.
#
# tab_plain() |> tab_chi2() is the REAL populator of the chi2 attribute (tab(test = TRUE) does
# NOT fill it for simple tables -- see the DESIGN note in test-calculations.R). subtext has no
# lightweight real populator (the subtext= arg stores whole population data), so a sentinel is
# set directly; that still faithfully exercises the carry path (methods do
# `subtext = get_subtext(.data)`). Both attributes are thus non-default, so "survives" is a
# real assertion, not a vacuous empty == empty.
cov_flat_attr <- tab_plain(fx_gss(), race, marital, pct = "row") |> tab_chi2()


attr(cov_flat_attr, "subtext") <- "phase0 sentinel subtext"


cov_grouped_attr <- dplyr::filter(fx_gss(), year %in% c(2000, 2014)) |>
  tab_plain(race, marital, year, pct = "row") |>
  tab_chi2()


attr(cov_grouped_attr, "subtext") <- "phase0 sentinel subtext"


attr_fixtures <- list(tabxplor_tab = cov_flat_attr, tabxplor_grouped_tab = cov_grouped_attr)


for (cls in names(attr_fixtures)) {
  for (vname in names(verb_coverage)) {
    local({
      fx    <- attr_fixtures[[cls]]
      klass <- cls
      v     <- vname
      testthat::test_that(paste0("verb keeps subtext + chi2 (", klass, "): ", v), {
        out <- verb_coverage[[v]](fx)
        testthat::expect_identical(get_subtext(out), get_subtext(fx))
        testthat::expect_identical(get_test(out),    get_test(fx))
      })
    })
  }
}


testthat::test_that("group_by upgrades tabxplor_tab -> tabxplor_grouped_tab, keeping attrs", {
  # group_by.tabxplor_tab (tab_classes.R) has a registered method but was untested: the
  # flat -> grouped UPGRADE. race has >1 level, so it does not immediately re-downgrade.
  gb <- dplyr::group_by(cov_flat_attr, race)
  testthat::expect_s3_class(gb, "tabxplor_grouped_tab")
  testthat::expect_identical(get_subtext(gb), get_subtext(cov_flat_attr))
  testthat::expect_identical(get_test(gb),    get_test(cov_flat_attr))
})


testthat::test_that("grouped_tab auto-downgrades to plain tabxplor_tab at one group left", {
  # lv1_group_vars(): filtering a 2-group tab down to a single group drops the other group,
  # so n_groups() == 1 -> the dplyr_row_slice/reconstruct trio downgrades grouped -> plain.
  # Only the explicit ungroup() path was tested before; this pins the AUTOMATIC trip.
  one <- dplyr::filter(cov_grouped_attr, year == 2000)
  testthat::expect_lte(dplyr::n_groups(one), 1L)
  testthat::expect_s3_class(one, "tabxplor_tab")
  testthat::expect_false(inherits(one, "tabxplor_grouped_tab"))
  testthat::expect_identical(get_subtext(one), get_subtext(cov_grouped_attr))
  testthat::expect_identical(get_test(one),    get_test(cov_grouped_attr))
})


test_that("an unset table carries NO meta attribute (absent-when-unset)", {
  e <- new_tab(tibble::tibble(a = 1:2))
  expect_null(attr(e, "meta", exact = TRUE))
  # emptying the last meta field removes the whole attribute
  t  <- set_vars_attr(new_tab(tibble::tibble(a = 1:2)), new_vars_attr(wt = "a"))
  expect_false(is.null(attr(t, "meta", exact = TRUE)))
  t0 <- set_vars_attr(t, NULL)
  expect_null(attr(t0, "meta", exact = TRUE))
})


test_that("set_caption / get_caption round-trip and survive dplyr; NA removes it", {
  t <- tab(fx_gss(), marital, race)
  expect_null(get_caption(t))
  tc <- set_caption(t, "My caption")
  expect_identical(get_caption(tc), "My caption")
  tc2 <- tc |> dplyr::filter(TRUE) |> dplyr::mutate(.z = 1) |> dplyr::select(-".z")
  expect_identical(get_caption(tc2), "My caption")
  expect_null(get_caption(set_caption(tc, NA)))
  expect_null(get_caption(set_caption(tc, NULL)))
})


test_that("get_test reads the top-level test attr", {
  t <- tab(fx_gss(), marital, race)
  expect_s3_class(get_test(t), "tbl_df")
})


# === SECTION: a stripped table still renders ======================================================

withr::local_options(lifecycle_verbosity = "quiet", .local_envir = testthat::teardown_env())


df <- fx_gss() |> dplyr::filter(!is.na(rincome), rincome != "No answer")


tc <- tab(df, race, marital, pct = "row", color = TRUE, test = TRUE, stars = TRUE,
          subtext = "A note")


# suppressWarnings: tvhours is over-dispersed (~2.04); the dispersion warning is expected and
# unrelated to what these tests exercise.
tr <- suppressWarnings(
  tab_reg(df, outcome = "tvhours", predictors = c("race", "marital"),
          family = "poisson", empirical = TRUE))


strip_attr  <- function(x, a) { attr(x, a) <- NULL; x }


strip_class <- function(x) { class(x) <- c("tbl_df", "tbl", "data.frame"); x }


md          <- function(x) as.character(suppressMessages(tab_export(x, "md", css = FALSE)))


# Exercise print + every export backend, asserting no error. Suggests-guarded backends skip cleanly.
expect_all_backends_ok <- function(x) {
  quiet <- function(expr) capture.output(suppressMessages(expr))  # swallow cat()/message noise
  expect_no_error(quiet(print(x)))
  expect_no_error(quiet(tab_export(x, "md")))
  expect_no_error(quiet(tab_export(x, "html")))
  if (requireNamespace("openxlsx2", quietly = TRUE))
    expect_no_error(quiet(
      tab_xl(x, path = withr::local_tempfile(fileext = ".xlsx"),
             open = FALSE, replace = TRUE)))
}


test_that("as_tibble() keeps the table-level attributes and the coloured output", {
  at <- tibble::as_tibble(tc)
  expect_false(is_tab(at))
  expect_false(is.null(attr(at, "test")))
  expect_false(is.null(attr(at, "meta")))
  expect_identical(md(at), md(tc))
})


test_that("losing `test` drops the summary block and nothing else", {
  expect_true(any(grepl("pvalue", md(tc))))              # summary present when test is
  no_test <- md(strip_attr(tc, "test"))
  expect_false(any(grepl("pvalue", no_test)))            # summary gone
  expect_true(any(grepl("\\{\\.[pmou][1-4]", no_test)))  # colours still there
  expect_true(any(grepl("A note", no_test)))             # subtext still there
})
