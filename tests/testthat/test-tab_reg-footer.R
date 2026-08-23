# Phase 12f: the regression model-summary footer (GOF stats in the `test` attribute, materialised as a
# console block / export rows at display) + multi-model comparison, plus the shared bits: the "gof"
# fmt display token and the crosstab in-cell test label. GOF parity is checked against broom::glance /
# base logLik/AIC/BIC / stats::anova run on the SAME model tab_reg fits.

reg_data <- function() {
  forcats::gss_cat |>
    dplyr::mutate(
      married = factor(dplyr::if_else(marital == "Married", "Married", "Not married"))
    )
}

# ---- the "gof" display token ----------------------------------------------------------------

test_that("the 'gof' token renders a plain big-mark number (per-cell digits) and is uncolored", {
  x   <- fmt(display = "gof", scale = "level_n", n = NA_integer_,
             diff = c(5231, 0.033, 28766.4), digits = c(0L, 3L, 0L))
  txt <- format(x)
  expect_equal(trimws(txt), c("5 231", "0.033", "28 766"))
  expect_false(any(grepl("%|\\+", txt)))                 # no percentage / no "+" sign
  expect_true(all(fmt_color_channels(x)$text == 0))      # gof is forced uncoloured
  expect_true(all(fmt_color_channels(x)$background == 0))
})

# ---- footer parity: binomial (glm) ----------------------------------------------------------

test_that("binomial footer N/LR-null/McFadden/AIC/BIC match a hand-fit glm; display-only", {
  skip_if_not_installed("broom")
  d   <- reg_data()
  t1  <- tab_reg(d, "married", c("race", "rincome"), family = "binomial", cleannames = FALSE)
  tst <- get_test(t1)
  cv  <- "Model_OR"
  gv  <- function(s) tst$statistic[tst$col == cv & tst$test == s]

  # the footer is DISPLAY-ONLY: the built object is the coefficient skeleton, no "Model fit" rows
  expect_false(any(as.character(t1$var) == "Model fit"))
  expect_true(tab_is_reg(t1))

  dm <- d |> dplyr::filter(!is.na(married), !is.na(race), !is.na(rincome))
  dm$married <- forcats::fct_rev(forcats::fct_drop(factor(dm$married)))
  dm$race    <- forcats::fct_drop(dm$race); dm$rincome <- forcats::fct_drop(dm$rincome)
  g  <- stats::glm(married ~ race + rincome, data = dm, family = stats::binomial())
  g0 <- stats::glm(married ~ 1,              data = dm, family = stats::binomial())

  expect_equal(gv("n"),   as.numeric(stats::nobs(g)))
  expect_equal(gv("aic"), stats::AIC(g), tolerance = 1e-6)
  expect_equal(gv("bic"), stats::BIC(g), tolerance = 1e-6)
  lr <- g$null.deviance - g$deviance
  expect_equal(gv("lr_null"), lr, tolerance = 1e-6)
  expect_equal(tst$pvalue[tst$col == cv & tst$test == "lr_null"],
               stats::pchisq(lr, g$df.null - g$df.residual, lower.tail = FALSE), tolerance = 1e-6)
  expect_equal(gv("mcfadden_r2"),
               1 - as.numeric(stats::logLik(g)) / as.numeric(stats::logLik(g0)), tolerance = 1e-6)
})

test_that("the footer does not alter the built coefficient skeleton (stats= toggle)", {
  skip_if_not_installed("broom")
  d       <- reg_data()
  with    <- tab_reg(d, "married", "race", family = "binomial", cleannames = FALSE)
  without <- tab_reg(d, "married", "race", family = "binomial", stats = FALSE, cleannames = FALSE)
  col <- "Model_OR"
  expect_equal(nrow(with), nrow(without))
  expect_equal(get_or(with[[col]]), get_or(without[[col]]))
  expect_equal(sum(!is.na(get_pvalue(with[[col]]))), sum(!is.na(get_pvalue(without[[col]]))))
  expect_false(tab_is_reg(tab(forcats::gss_cat, marital, race, pct = "row")))

  # materialised only at display (backend text/xl, pvalue = TRUE)
  mat <- tab_materialize_extras(with, backend = "text", pvalue = TRUE)
  expect_true(any(as.character(mat$var) == "Model fit"))
  # the model N is the FIRST footer row: the Constant row is a profile, not the population
  expect_true("N" %in% as.character(mat$levels))
  expect_gt(nrow(mat), nrow(with))
})

test_that("stats= picks and the footer keeps only the requested stats", {
  skip_if_not_installed("broom")
  tst <- get_test(tab_reg(reg_data(), "married", "race", family = "binomial",
                          stats = c("n", "aic"), cleannames = FALSE))
  expect_setequal(unique(tst$test), c("n", "aic"))
})

# ---- footer parity: gaussian (lm) -----------------------------------------------------------

test_that("gaussian footer (N/R2/adjR2/F/sigma) matches broom::glance", {
  skip_if_not_installed("broom")
  d   <- reg_data()
  t1  <- tab_reg(d, "tvhours", c("age", "race"), family = "gaussian", cleannames = FALSE)
  tst <- get_test(t1); cv <- "Model_diff"
  gv  <- function(s) unname(tst$statistic[tst$col == cv & tst$test == s])

  # the lm default footer set (D7): N + R2 + adjR2 + F + residual SD (no AIC/BIC unless stats= asks),
  # plus z13's overall-association rows and z15's model-check rows (both in the default set).
  expect_setequal(setdiff(unique(tst$test),
                          c(tabxplor:::reg_global_types(), tabxplor:::reg_check_types())),
                  c("n", "r2", "r2_adj", "f_model", "sigma"))
  dm <- d |> dplyr::filter(!is.na(tvhours), !is.na(age), !is.na(race))
  g  <- broom::glance(stats::lm(tvhours ~ age + race, data = dm))
  expect_equal(gv("r2"),      g$r.squared,     tolerance = 1e-6)
  expect_equal(gv("r2_adj"),  g$adj.r.squared, tolerance = 1e-6)
  expect_equal(gv("f_model"), unname(g$statistic), tolerance = 1e-6)
  expect_equal(gv("sigma"),   g$sigma,         tolerance = 1e-6)

  # AIC/BIC are available on request (stats=)
  t2 <- tab_reg(d, "tvhours", c("age", "race"), family = "gaussian",
                stats = c("n", "aic", "bic"), cleannames = FALSE)
  t2t <- get_test(t2)
  expect_equal(unname(t2t$statistic[t2t$test == "aic"]), g$AIC, tolerance = 1e-6)
  expect_equal(unname(t2t$statistic[t2t$test == "bic"]), g$BIC, tolerance = 1e-6)
})

# ---- footer parity: poisson dispersion ------------------------------------------------------

test_that("poisson footer carries a Pearson dispersion matching sum(pearson^2)/df + warns", {
  skip_if_not_installed("broom")
  d <- reg_data()
  expect_warning(t1 <- tab_reg(d, "tvhours", c("age", "race"), family = "poisson",
                               cleannames = FALSE), "dispersion")
  tst <- get_test(t1); cv <- "Model_IRR"
  dm  <- d |> dplyr::filter(!is.na(tvhours), !is.na(age), !is.na(race))
  m   <- stats::glm(tvhours ~ age + race, data = dm, family = stats::poisson())
  # z15: the exact Pearson dispersion keeps its own row under the key `phi` (the key `dispersion` now
  # names the robust/model-SE CHECK). n - rank, never df.residual(fit) -- they agree for a glm.
  phi <- sum(stats::residuals(m, "pearson")^2) / stats::df.residual(m)
  expect_equal(tst$statistic[tst$col == cv & tst$test == "phi"], phi, tolerance = 1e-6)
})

# ---- multi-model comparison -----------------------------------------------------------------

test_that("compare='baseline' adds an LR-vs-baseline row matching anova() (same-N nested models)", {
  skip_if_not_installed("broom")
  d  <- reg_data()
  mc <- tab_reg(d, "married",
                    predictors = list(demo = c("race", "age"), full = c("race", "age", "rincome")),
                    stats = "compare_baseline", cleannames = FALSE)
  cmp <- get_test(mc) |> dplyr::filter(test == "compare_baseline")
  expect_equal(nrow(cmp), 1L)                             # only the non-baseline column

  dm <- d |> dplyr::filter(!is.na(married), !is.na(race), !is.na(age), !is.na(rincome))
  dm$married <- forcats::fct_rev(forcats::fct_drop(factor(dm$married)))
  dm$race <- forcats::fct_drop(dm$race); dm$rincome <- forcats::fct_drop(dm$rincome)
  m1 <- stats::glm(married ~ race + age,           data = dm, family = stats::binomial())
  m2 <- stats::glm(married ~ race + age + rincome, data = dm, family = stats::binomial())
  an <- stats::anova(m1, m2, test = "Chisq")
  expect_equal(cmp$statistic, an$Deviance[2],       tolerance = 1e-6)
  expect_equal(cmp$pvalue,    an[["Pr(>Chi)"]][2],  tolerance = 1e-6)
})

test_that("compare falls back to Delta-AIC (with a message) when N differs across models", {
  skip_if_not_installed("broom")
  d <- reg_data()                                        # tvhours has NAs -> different N than race-only
  # Phase 18z13 (D1): the DEFAULT `na = "drop_by_outcome"` now puts both models on one population, so
  # the likelihood-ratio test fires -- which is the point of that default. The AIC fallback is what the
  # opt-in per-model drop still needs, so that is what this test exercises.
  expect_message(
    mc <- tab_reg(d, "married",
                      predictors = list(a = "race", b = c("race", "tvhours")),
                      stats = "compare_baseline", cleannames = FALSE, na = "drop_by_model"),
    "not nested or N differs"
  )
  expect_true("compare_baseline_aic" %in% get_test(mc)$test)
})

test_that("D1: the shared-population default makes the likelihood-ratio comparison fire", {
  skip_if_not_installed("broom")
  d  <- reg_data()
  mc <- suppressMessages(tab_reg(d, "married",
                                     predictors = list(a = "race", b = c("race", "tvhours")),
                                     stats = "compare_baseline", cleannames = FALSE))
  tt <- get_test(mc)
  expect_true(any(grepl("^compare_", tt$test)))
  expect_false("compare_baseline_aic" %in% tt$test)      # a real test, not the degraded difference
  # equal N is the reason it can: both models are fitted on the same people
  expect_equal(length(unique(tt$n[tt$test == "n"])), 1L)
})

test_that("compare no-ops (message) for a single model", {
  skip_if_not_installed("broom")
  expect_message(
    tab_reg(reg_data(), "married", "race", family = "binomial", stats = "compare_baseline",
            cleannames = FALSE),
    "at least two models"
  )
})

# ---- footer renders through every backend ---------------------------------------------------

test_that("the regression footer renders (console block + export rows) without error", {
  skip_if_not_installed("broom")
  t1 <- tab_reg(reg_data(), "married", c("race", "age"), family = "binomial")
  expect_output(print(t1), "Model fit")                  # the console footer block (Phase 16a GFM table)
  # Phase g (A7): a styled md table's label cells use non-breaking spaces; normalise for text greps.
  md <- gsub(intToUtf8(160L), " ", tab_md(t1, print = FALSE), fixed = TRUE)
  expect_true(any(grepl("Model fit", md)))               # the export footer rows
  expect_true(any(grepl("McFadden", md)))
  expect_no_error(tab_kable(t1))
  skip_if_not_installed("openxlsx2")
  xf <- withr::local_tempfile(fileext = ".xlsx")
  expect_no_error(tab_xl(t1, path = xf, replace = TRUE))
})

# ---- the shared crosstab in-cell test label -------------------------------------------------

test_that("a crosstab p-value cell embeds its test label ('(Chi2)')", {
  ct <- tab(forcats::gss_cat, marital, race, pct = "row", test = TRUE)
  md <- tab_md(ct, print = FALSE)
  expect_true(any(grepl("\\(Chi2", md)))                 # "(Chi2)" or "(Chi2 !)" (Phase 16a weak flag)
})

test_that("a mixed factor/mean table names both tests in the p-value row (Phase 18m)", {
  ct <- tab(forcats::gss_cat, marital, c(race, tvhours), pct = "row", test = TRUE)
  # a styled md (the non-significant p-value cell is coloured) uses U+00A0 in labels -> normalise.
  md <- gsub(intToUtf8(160L), " ", tab_md(ct, print = FALSE), fixed = TRUE)
  # the test type is now stated ONCE in the p-value row name, not per cell: "pvalue (Chi2, Welch F)"
  expect_true(any(grepl("pvalue \\(Chi2", md)))          # the factor test
  expect_true(any(grepl("Welch F", md)))                 # the mean test (Welch ANOVA F)
})

# ---- Phase 14q: readability -----------------------------------------------------------------

test_that("ordinal footer carries a Brant PO test p-value row (Item I)", {
  skip_if_not_installed("MASS")
  skip_if_not_installed("brant")
  d <- forcats::gss_cat |>
    dplyr::mutate(rincome = forcats::fct_recode(rincome, NULL = "No answer", NULL = "Refused",
                                                NULL = "Don't know", NULL = "Not applicable") |>
                    forcats::fct_relevel(sort))
  # Phase 20f: the Brant test fits J-1 binary logits, so it is opt-in (REG_CHECKS$cost == "refit")
  # and computed HERE rather than at fit time.
  suppressWarnings(t <- tab_reg(d, "rincome", c("marital", "race"), family = "ordinal",
                                stats = c("n", "proportionality")))
  tst <- get_test(t)
  # z15: the Brant p, as the "Proportionality (Brant)" model check
  expect_true("proportionality" %in% tst$test)
  p <- tst$pvalue[tst$test == "proportionality"]
  expect_true(all(!is.na(p) & p >= 0 & p <= 1))
  # renders as a "Proportionality (Brant)" export row (A7: normalise nbsp for the text grep)
  md <- gsub(intToUtf8(160L), " ", tab_md(t, print = FALSE), fixed = TRUE)
  expect_true(any(grepl("Proportionality (Brant)", md, fixed = TRUE)))
})

test_that("reg reference cells and GOF footer render black + bold, data cells stay grey (Items D/J)", {
  skip_if_not_installed("broom")
  d <- reg_data()
  t <- tab_reg(d, "married", c("race", "rincome"), family = "binomial", empirical = TRUE)
  rd <- tabxplor:::tab_export_prep(t, backend = "kable", wrap = NULL)$tables[[1]]
  tabm <- rd$tab

  emp   <- "Obs_OR"
  ann_e <- rd$ann[[emp]]
  refr  <- tabxplor::is_refrow(tabm[[emp]])
  expect_true(any(refr))
  # the empirical-column reference CATEGORY cells are kept black + bold (were greyed: ref_type "tot"
  # yet marked per-category via in_refrow, so get_reference("all_totals") missed them)
  expect_true(all(ann_e$keep_black[refr]))
  expect_true(all(ann_e$bold[refr]))

  # the GOF footer rows (all fmt cells are gof/pvalue/blank) are black + bold in EVERY column
  fmt_nm <- names(tabm)[purrr::map_lgl(tabm, tabxplor::is_fmt)]
  footer <- purrr::reduce(purrr::map(fmt_nm, ~ tabxplor:::display_primary(
    tabxplor:::get_display(tabm[[.x]])) %in% c("gof", "pvalue", "blank")), `&`)
  expect_true(any(footer))
  for (nm in fmt_nm) {
    expect_true(all(rd$ann[[nm]]$keep_black[footer]))
    expect_true(all(rd$ann[[nm]]$bold[footer]))
  }

  # a plain uncoloured, non-reference, non-footer data cell is STILL greyable (keep_black FALSE) --
  # greying still makes the coloured cells pop
  data_rows <- !refr & !footer
  expect_false(all(ann_e$keep_black[data_rows]))
})

# ---- Phase 18z13 (SS7.2): the per-predictor global test ------------------------------------------

test_that("stats='global' IS drop1() on the fit already in hand, as per-predictor footer rows", {
  skip_if_not_installed("broom")
  d <- reg_data()
  t <- suppressMessages(tab_reg(d, "married", c("race", "rincome"), family = "binomial",
                                cleannames = FALSE, stats = c("n", "global")))
  tt <- get_test(t)
  g  <- tt[tt$test %in% tabxplor:::reg_global_types(), , drop = FALSE]
  # Phase 19g: the predictor rides `var`, the same dimension a crosstab test row uses; a split-group
  # level rides a column named after the split variable, so the two can no longer collide
  expect_setequal(g$var, c("race", "rincome"))           # one per multi-level predictor
  expect_identical(tabxplor:::test_group_cols(g), character(0))   # no split -> no group column
  expect_identical(unique(g$test), "global_lr")

  # the numbers ARE drop1's, no extra fit involved
  dm <- d |> dplyr::filter(!is.na(married), !is.na(race), !is.na(rincome))
  dm$married <- forcats::fct_rev(forcats::fct_drop(factor(dm$married)))
  dm$race <- forcats::fct_drop(dm$race); dm$rincome <- forcats::fct_drop(dm$rincome)
  fit <- stats::glm(married ~ race + rincome, data = dm, family = stats::binomial())
  d1  <- stats::drop1(fit, scope = c("race", "rincome"), test = "Chisq")
  expect_equal(g$pvalue[match(rownames(d1)[-1], g$var)],
               d1[["Pr(>Chi)"]][-1], tolerance = 1e-8)
  expect_equal(g$statistic[match(rownames(d1)[-1], g$var)],
               d1[["LRT"]][-1], tolerance = 1e-8)

  # z15: they are GOF ROWS now -- one per (model column x predictor), labelled "<stat>: <predictor>"
  expect_true(all(tabxplor:::reg_global_types() %in% names(tabxplor:::reg_footer_spec())))
  md <- gsub(intToUtf8(160L), " ", tab_md(t, print = FALSE), fixed = TRUE)
  expect_true(any(grepl("Overall association (LR): race", md, fixed = TRUE)))
  expect_true(any(grepl("Overall association (LR): rincome", md, fixed = TRUE)))
})

test_that("the global test skips 1-df terms, unsupported engines and stats = FALSE", {
  skip_if_not_installed("broom")
  d <- reg_data()
  # a numeric predictor's overall p IS its single cell's p -- a row for it would be noise
  t <- suppressMessages(tab_reg(d, "married", c("race", "age"), family = "binomial",
                                cleannames = FALSE, stats = c("n", "global")))
  g <- get_test(t); g <- g[g$test %in% tabxplor:::reg_global_types(), ]
  expect_setequal(g$var, "race")
  # opt out
  t0 <- suppressMessages(tab_reg(d, "married", "race", family = "binomial", stats = FALSE,
                                 cleannames = FALSE))
  expect_false(any(tabxplor:::reg_global_types() %in% get_test(t0)$test))
  # an engine with no drop1/regTermTest degrades to no row, never to an error (the z8 contract)
  skip_if_not_installed("nnet")
  tm <- suppressMessages(suppressWarnings(
    tab_reg(d, "marital", "race", family = "multinomial", cleannames = FALSE,
            stats = c("n", "global"))))
  expect_false(any(tabxplor:::reg_global_types() %in% get_test(tm)$test))
})

# ---- Phase 18z15: the `term` retrofit fixes a live split-table defect ----------------------------

test_that("on a split table the per-predictor rows name the predictors, not the split level", {
  skip_if_not_installed("broom")
  d <- reg_data()
  d$grp <- factor(ifelse(d$year < 2006, "early", "late"))
  t <- suppressMessages(tab_reg(d, "married",
                                list(m1 = c("race", "rincome"), m2 = c("race", "rincome")),
                                tab_vars = "grp", family = "binomial", cleannames = FALSE,
                                stats = c("n", "global")))
  tt <- get_test(t)
  g  <- tt[tt$test %in% tabxplor:::reg_global_types(), , drop = FALSE]
  expect_true(nrow(g) > 0)
  # BEFORE z15 the predictor rode `row_var`, which reg_build's split branch overwrote wholesale with
  # the group level -- so every item printed the group's name, repeated, instead of "race"/"rincome".
  # Phase 19g: the two facts ride two columns that cannot collide -- `var` and a column NAMED after
  # the split variable.
  expect_setequal(unique(g$var), c("race", "rincome"))
  expect_setequal(unique(g$grp), c("early", "late"))
  expect_false(any(g$var %in% c("early", "late")))
})

# --- Phase 22b-iv: the footer follows the model ---------------------------------------------------

test_that("per-predictor footer rows keep the model's term order, not the alphabet", {
  d <- suppressWarnings(gss_cat_data_formatting())
  t <- suppressMessages(tab_reg(d, "married", c("race", "rincome", "relig"), family = "binomial",
                                stats = c("n", "global")))
  lab <- tabxplor:::reg_footer_plan(get_test(t))$label
  glob <- sub("^.*: ", "", grep("^Overall association", lab, value = TRUE))
  testthat::expect_equal(glob, c("race", "rincome", "relig"))
})

test_that("the footer and the crude tooltips key onto the MODEL column, never the spliced Obs_ one", {
  skip_if_not_installed("nnet")
  d <- suppressWarnings(gss_cat_data_formatting())
  t <- suppressMessages(tab_reg(d, "party3", c("race", "relig"), family = "multinomial",
                                empirical = "column"))
  mdl <- names(t)[vapply(t, function(x) is_fmt(x) && identical(get_role(x), "model"), logical(1))]
  testthat::expect_true(all(unique(get_test(t)$col) %in% mdl))
  tips <- attr(t, "meta")$empirical_tips
  testthat::expect_gt(nrow(tips), 0L)
  testthat::expect_true(all(unique(tips$col) %in% mdl))
})

# --- Phase 22b-xviii: the footer reads outward from the data -------------------------------------

test_that("the footer is N, then the checks worst-first, then the content, then the comparison", {
  skip_if_not_installed("broom")
  d <- suppressWarnings(gss_cat_data_formatting())
  t <- suppressMessages(tab_reg(d, "married", c("race", "rincome", "age"), family = "binomial",
                                stats = "all"))
  k <- tabxplor:::reg_footer_plan(get_test(t))$test
  pos <- function(x) which(k == x)[[1]]
  expect_identical(k[[1]], "n")
  # what the number MEANS, then whether the standard errors are trustworthy, then how fragile it is
  expect_lt(pos("linearity_lr"), pos("dispersion"))
  expect_lt(pos("dispersion"), pos("collinearity"))
  expect_lt(pos("collinearity"), pos("influence"))
  # the two readings of ONE question sit together, wherever the family emits both (the declared
  # order is the display order, and `phi` is only produced where dispersion can be estimated)
  fk <- tabxplor:::TEST_FOOTER_KEYS
  expect_identical(which(fk == "phi"), which(fk == "dispersion") + 1L)
  # then the content, then the comparison
  expect_lt(pos("influence"), pos("global_lr"))
  expect_lt(pos("global_lr"), pos("lr_null"))
  expect_lt(pos("lr_null"), pos("aic"))
})

test_that("the joint test is opt-in and Brant is an ordinal default", {
  skip_if_not_installed("broom")
  expect_false("global" %in% reg_footer_stats("binomial", FALSE, FALSE, NULL))
  expect_true ("global" %in% reg_footer_stats("binomial", FALSE, FALSE, "all"))
  expect_true ("global" %in% reg_footer_stats("binomial", FALSE, FALSE, c("n", "global")))
  # a refit, and still a default: a cumulative OR that fails it is not one number but a fiction
  expect_true ("proportionality" %in% reg_footer_stats("ordinal", FALSE, FALSE, NULL))
  expect_false("proportionality" %in% reg_footer_stats("binomial", FALSE, FALSE, NULL))
  # ... but never on a weighted fit, where the Brant test has no meaning
  expect_false("proportionality" %in% reg_footer_stats("ordinal", TRUE, FALSE, NULL))
})

# --- Phase 22g-ii: the comparison is the DEFAULT -------------------------------------------------
# Writing several `predictors` sets is something a reader does on purpose, and the row saying whether
# the added variables bought anything is the point of writing them. Which comparison is read off the
# models: a chain where each nests in the next is tested against the PREVIOUS, anything else against
# the first. Naming any footer statistic drops it, as naming any other argument value does.

cmp_rows <- function(t) {
  tt <- get_test(t)
  if (is.null(tt)) character(0) else unique(tt$test[grepl("compare", tt$test)])
}
cmp_msgs <- function(expr) {
  m <- character(0)
  withCallingHandlers(expr, message = function(e) {
    m <<- c(m, conditionMessage(e)); invokeRestart("muffleMessage") })
  m
}

test_that("several predictor sets compare by default, sequential where they nest", {
  skip_if_not_installed("broom")
  d <- reg_data()
  chain <- list(m1 = "race", m2 = c("race", "rincome"), m3 = c("race", "rincome", "relig"))
  expect_true(any(grepl("compare_seq", cmp_rows(
    suppressMessages(tab_reg(d, "married", chain, family = "binomial"))))))
  # not a chain -> each model against the first
  loose <- list(a = "race", b = "rincome")
  expect_true(any(grepl("compare_baseline", cmp_rows(
    suppressMessages(tab_reg(d, "married", loose, family = "binomial"))))))
  # naming the footer statistics drops the comparison, the way naming any value drops the rest
  expect_length(cmp_rows(suppressMessages(
    tab_reg(d, "married", chain, family = "binomial", stats = c("n", "aic")))), 0L)
})

test_that("the default comparison never speaks up, and never refuses a table", {
  skip_if_not_installed("broom")
  d <- reg_data()
  # one model: nothing to compare, and no lecture about it
  expect_length(cmp_rows(suppressMessages(tab_reg(d, "married", "race", family = "binomial"))), 0L)
  expect_false(any(grepl("compare", cmp_msgs(
    tab_reg(d, "married", "race", family = "binomial")), fixed = TRUE)))
  # several outcomes: a between-model test has no meaning, so it degrades rather than aborting
  # (an EXPLICIT key still refuses -- that message names `stats`)
  expect_length(cmp_rows(suppressMessages(
    tab_reg(d, c("married", "age"), "race", family = c("binomial", "gaussian")))), 0L)
  expect_error(suppressMessages(
    tab_reg(d, c("married", "age"), "race", family = c("binomial", "gaussian"),
            stats = "compare_baseline")), "share one")
})

test_that("an ordinary table keeps its parallelism and drops its fits", {
  skip_if_not_installed("broom")
  d <- reg_data()
  # ⚠ `compare != "none"` is what turns BOTH off (reg_specs_independent, and the fit kept on the
  # product), so a default that left "auto" live would cost every table both.
  sh <- suppressMessages(tab_reg(d, c("married", "age"), c("race", "rincome"),
                                 family = c("binomial", "gaussian")))
  expect_identical(tabxplor:::reg_call(sh)$compare %||% "none", "none")
})
