# Phase 22b-ix: interactions -- a crossed pair prepared as a VARIABLE before the fit.
#
# The claims the whole design rests on: the presentation is a choice and never a statistic (the
# three parametrisations are ONE fit); the crude companion of a combined factor IS the observed cell
# table; and the nested arm's slopes are the fit's own, per the unit the row names.

skip_if_not_installed("broom")

cr_data <- function() {
  d <- forcats::gss_cat
  d$race    <- forcats::fct_drop(d$race)
  d$married <- factor(as.integer(d$marital == "Married"), labels = c("no", "yes"))
  d$age4    <- cut(d$age, breaks = stats::quantile(d$age, (0:4) / 4, na.rm = TRUE),
                   include.lowest = TRUE, labels = c("18-33", "34-46", "47-59", "60+"))
  droplevels(d[stats::complete.cases(d[, c("race", "relig", "age", "married", "age4")]), ])
}
quiet <- function(x) suppressWarnings(suppressMessages(x))
cx    <- function(t) as.character(t$var)

# ---- the fit is the same fit ---------------------------------------------------------------

test_that("a combined factor is the SAME fit as the star parametrisation", {
  d <- cr_data()
  t <- quiet(tab_reg(d, "married", c("race*age4", "relig"), family = "binomial", stats = FALSE))
  star <- stats::glm(married ~ race * age4 + relig, d, family = stats::binomial())
  comb <- stats::glm(married ~ relig + interaction(race, age4), d, family = stats::binomial())
  expect_equal(as.numeric(stats::logLik(star)), as.numeric(stats::logLik(comb)), tolerance = 1e-10)
  expect_equal(star$rank, comb$rank)
  expect_lt(max(abs(stats::fitted(star) - stats::fitted(comb))), 1e-12)
  # and the table is that fit: one row per observed cell, plus the Constant
  expect_equal(sum(cx(t) == "race*age4"), nlevels(forcats::fct_drop(interaction(d$race, d$age4))))
})

test_that("the crude column of a combined factor IS the observed cell table", {
  d <- cr_data()
  t <- quiet(tab_reg(d, "married", "race*age4", family = "binomial", empirical = TRUE,
                     stats = FALSE))
  i  <- cx(t) == "race*age4"
  lv <- as.character(t$levels[i])
  cell <- paste(as.character(d$race), as.character(d$age4), sep = " \u00b7 ")
  # the observed odds ratio of each cell against the first one, computed straight off the data
  p  <- as.vector(tapply(d$married == levels(d$married)[[1]], cell, mean)[lv])
  or <- (p / (1 - p)) / (p[[1]] / (1 - p[[1]]))
  expect_equal(unname(get_or(t$Obs_OR[i])), or, tolerance = 1e-9)
  # every cell carries its own count, which is what a crossed continuous predictor never had
  expect_equal(unname(get_n(t$Model_OR[i])), as.integer(as.vector(table(cell)[lv])))
})

# ---- the surface ---------------------------------------------------------------------------

test_that("`a*b` is an interaction in `predictors`, bare or quoted", {
  d  <- cr_data()
  t1 <- quiet(tab_reg(d, "married", c("race*age4", "relig"), family = "binomial", stats = FALSE))
  t2 <- quiet(tab_reg(d, "married", c(race*age4, relig), family = "binomial", stats = FALSE))
  expect_identical(t1, t2)
  pv <- c("race*age4", "relig")
  expect_identical(t1, quiet(tab_reg(d, "married", pv, family = "binomial", stats = FALSE)))
  # the block keeps the position it was written at, and is named with the times sign
  expect_identical(unique(cx(t1))[[2]], "race*age4")
  expect_match(reg_formulas(t1)$formula, "race*age4", fixed = TRUE)
})

test_that("`:` is refused by name: it is a different model in R, not a synonym", {
  d <- cr_data()
  # `a:b` is the interaction term WITHOUT its main effects -- which for a continuous parent is a
  # different fit and depends on the anchor, and is what reg_cross_resolve() exists to refuse.
  expect_error(tab_reg(d, "married", c(race:age4), family = "binomial", stats = FALSE),
               "not how an interaction is written")
  expect_error(tab_reg(d, "married", c("race:age4"), family = "binomial", stats = FALSE),
               "not how an interaction is written")
  a <- stats::glm(married ~ age:race,  d, family = stats::binomial())
  b <- stats::glm(married ~ age * race, d, family = stats::binomial())
  expect_gt(abs(as.numeric(stats::logLik(a)) - as.numeric(stats::logLik(b))), 1)
})

test_that("the ORDER picks the presentation, never the model", {
  d <- cr_data()
  # R's `*` is symmetric in the FIT -- and tabxplor uses the order to say which variable the rows
  # are about, so the two tables are one model shown two ways.
  A <- stats::glm(married ~ race * age4, d, family = stats::binomial())
  B <- stats::glm(married ~ age4 * race, d, family = stats::binomial())
  expect_equal(as.numeric(stats::logLik(A)), as.numeric(stats::logLik(B)), tolerance = 1e-10)
  expect_lt(max(abs(stats::fitted(A) - stats::fitted(B))), 1e-12)
  t1 <- quiet(tab_reg(d, "married", "race*age4", family = "binomial", stats = FALSE))
  t2 <- quiet(tab_reg(d, "married", "age4*race", family = "binomial", stats = FALSE))
  expect_identical(unique(cx(t1))[[2]], "race*age4")
  expect_identical(unique(cx(t2))[[2]], "age4*race")
  # same cells, ordered the other way round -- so the same multiset of estimates
  expect_equal(sort(get_or(t1$Model_OR)), sort(get_or(t2$Model_OR)), tolerance = 1e-8)
})

test_that("the refusals name the cure", {
  d <- cr_data()
  f <- function(p) tab_reg(d, "married", p, family = "binomial", stats = FALSE)
  expect_error(f(c("race", "race*age4")), "beside an interaction")   # the parent rule
  expect_error(f("race*nope"), "does not exist")
  expect_error(f("race*race"), "with itself")
  # a shape that keeps the moderator CONTINUOUS is the user's own choice, so the abort stands
  expect_error(tab_reg(d, "married", "age*tvhours", family = "binomial", stats = FALSE,
                       shape = c(tvhours = "sqrt")), "categorical moderator")
})

test_that("only the ORDER wrong: swap to the one table that can exist, and say so", {
  d <- cr_data()
  expect_message(tab_reg(d, "married", "race*age", family = "binomial", stats = FALSE),
                 "read as .age\\*race.")
  t <- quiet(tab_reg(d, "married", "race*age", family = "binomial", stats = FALSE))
  # the block is named as the SWAP, which is what the table prints...
  expect_true("age*race" %in% cx(t))
  expect_false("race*age" %in% cx(t))
  # ...and the moderator keeps its own block, because the model contains it
  expect_true("race" %in% cx(t))
  # `*` is symmetric in the FIT, so the swap decides a presentation and never a model
  expect_identical(reg_render_ish <- get_or(t$Model_OR),
                   get_or(quiet(tab_reg(d, "married", "age*race", family = "binomial",
                                        stats = FALSE))$Model_OR))
})

test_that("two continuous parents: the moderator is cut, never silently", {
  d <- cr_data()
  expect_message(tab_reg(d, "married", "age*tvhours", family = "binomial", stats = FALSE),
                 "no cells to cross")
  t <- quiet(tab_reg(d, "married", "age*tvhours", family = "binomial", stats = FALSE))
  # the cut IS the table: one slope per quartile of the moderator, and the moderator's own block
  expect_equal(sum(cx(t) == "age*tvhours"), 4L)
  expect_true("tvhours" %in% cx(t))
  # a user shape on the moderator wins outright -- their choice, no message
  q5 <- quiet(tab_reg(d, "married", "age*tvhours", family = "binomial", stats = FALSE,
                      shape = c(tvhours = "quintiles")))
  expect_equal(sum(cx(q5) == "age*tvhours"), 5L)
  expect_silent(suppressWarnings(tab_reg(d, "married", "age*tvhours", family = "binomial",
                                         stats = FALSE, shape = c(tvhours = "quintiles"))))
  # ...and so does an unnamed fallback, which reg_resolve_shape() has already spread: it cuts BOTH
  # parents, so the pair becomes the ordinary cells arm (3 x 3) and nothing is auto-cut.
  q3 <- quiet(tab_reg(d, "married", "age*tvhours", family = "binomial", stats = FALSE, shape = 3))
  expect_equal(sum(cx(q3) == "age*tvhours"), 9L)
  expect_silent(suppressWarnings(tab_reg(d, "married", "age*tvhours", family = "binomial",
                                         stats = FALSE, shape = 3)))
})

test_that("`shape = \"sd_bands\"` bands at the mean and one SD either side", {
  d <- cr_data()
  t <- quiet(tab_reg(d, "married", c("age", "race"), family = "binomial", stats = FALSE,
                     shape = c(age = "sd_bands")))
  lv <- as.character(t$levels)[cx(t) == "age"]
  expect_length(lv, 4L)
  # every label carries BOTH facts: the real cut points, and where the band sits on the mean/SD scale
  expect_match(lv[[1]], "^\\[.*\\) < m-sd$")
  expect_match(lv[[2]], "m-sd\\.\\.m$")
  expect_match(lv[[3]], "m\\.\\.m\\+sd$")
  expect_match(lv[[4]], "> m\\+sd$")
  # the cuts really are the landmarks
  m <- mean(d$age); s <- stats::sd(d$age)
  br <- attr(tabxplor:::reg_cut_sd_bands(d$age, var = "age"), "tabxplor_breaks")
  expect_equal(br[2:4], c(m - s, m, m + s), tolerance = 1e-8)
  # ⚠ unlike quantiles the bands are NOT balanced: a landmark outside the data is DROPPED rather
  # than asked of cut(), and a variable with none left is refused, naming the balanced cure.
  set.seed(1)
  ex <- stats::rexp(5000, 1 / 3e4)                       # mean - sd is below the minimum
  expect_length(levels(tabxplor:::reg_cut_sd_bands(ex, var = "x")), 3L)
  # the mean is always inside the range of a variable that varies, so the only refusal left is one
  # that does not vary at all.
  expect_error(tabxplor:::reg_cut_sd_bands(rep(1, 100), var = "x"), "to vary")
})

test_that("the parent rule is PER MODEL, so with/without is one comparison", {
  d <- cr_data()
  t <- quiet(tab_reg(d, "married",
                     list(additive = c(race, age4), crossed = c(race*age4)),
                     family = "binomial", stats = "compare_sequential"))
  expect_true(all(c("additive", "crossed") %in% names(t)))
  tt <- get_test(t)
  # the sequential comparison and the interaction row are the SAME two models, hence one p-value
  lr <- tt$pvalue[grepl("^compare_", tt$test)]
  ix <- tt$pvalue[tt$test == "cross_lr"]
  expect_equal(lr[!is.na(lr)], ix[!is.na(ix)], tolerance = 1e-12)
})

# ---- the nested arm ------------------------------------------------------------------------

test_that("a crossed continuous predictor gives the fit's own slope per moderator level", {
  d <- cr_data()
  t <- quiet(tab_reg(d, "married", c("age*race", "relig"), family = "binomial", stats = FALSE))
  i <- cx(t) == "age*race"
  expect_equal(sum(i), nlevels(d$race))
  nest <- stats::glm(married ~ race + relig + race:age, d, family = stats::binomial())
  k    <- tabxplor:::reg_predictor_sd(d$age)
  b    <- stats::coef(nest)[paste0("race", as.character(t$levels[i]) |>
                                     sub(pattern = "^.*\u2014 ", replacement = ""), ":age")]
  # C4-2: `multiplier` reaches a crossed slope -- it was left at one raw unit before this phase.
  # ⚠ tab_reg models the FIRST outcome level and glm() the second, so the two are reciprocals.
  expect_equal(unname(get_or(t$Model_OR[i])), unname(exp(-b * k)), tolerance = 1e-9)
  expect_match(as.character(t$levels[i])[[1]], "^age per [0-9.]+ \\(SD\\)")
  # its `n` is the moderator level's, and the moderator keeps a row block of its own
  expect_equal(unname(get_n(t$Model_OR[i])), as.integer(as.vector(table(d$race))))
  expect_true("race" %in% cx(t))
})

test_that("a crossed slope has a subgroup AME, and it is marginaleffects'", {
  skip_if_not_installed("marginaleffects")
  d <- cr_data()
  t <- quiet(tab_reg(d, "married", c("age*race", "relig"), family = "binomial",
                     effect = "marginal", stats = FALSE))
  i <- cx(t) == "age*race"
  nest <- stats::glm(married ~ race + relig + race:age, d, family = stats::binomial())
  k    <- tabxplor:::reg_predictor_sd(d$age)
  ref  <- marginaleffects::avg_comparisons(nest, variables = list(age = k), by = "race")
  # tab_reg models the FIRST outcome level, glm() the second, so the two differ by sign only
  expect_equal(sort(abs(get_diff(t$Model_mRD[i]))), sort(abs(ref$estimate)), tolerance = 1e-8)
  # and each row carries its group's adjusted prediction, which a bare slope never had
  expect_false(anyNA(get_pct(t$Model_mRD[i])))
})

# ---- the footer ----------------------------------------------------------------------------

test_that("the interaction test is the additive-vs-crossed model comparison", {
  d  <- cr_data()
  t  <- quiet(tab_reg(d, "married", c("race*age4", "relig"), family = "binomial"))
  tt <- get_test(t)
  r  <- tt[tt$test == "cross_lr", ]
  expect_equal(nrow(r), 1L)
  expect_identical(r$var, "race*age4")
  a <- stats::glm(married ~ race + age4 + relig, d, family = stats::binomial())
  b <- stats::glm(married ~ relig + interaction(race, age4), d, family = stats::binomial())
  an <- stats::anova(a, b, test = "Chisq")
  expect_equal(r$pvalue, an[["Pr(>Chi)"]][[2]], tolerance = 1e-10)
  expect_equal(r$df1, an$Df[[2]])
  # C4-1: a non-syntactic block name keeps its overall-association row (it was silently lost)
  expect_true("race*age4" %in% tt$var[tt$test == "global_lr"])
})

test_that("the interaction row is free on a glm and opt-in where it costs a second fit", {
  expect_true("interaction" %in% reg_footer_stats("binomial", FALSE, FALSE, NULL))
  expect_true("interaction" %in% reg_footer_stats("gaussian", FALSE, FALSE, NULL))
  # a 3+ level outcome refits multinom / polr, which roughly doubles the fitting time
  expect_false("interaction" %in% reg_footer_stats("multinomial", FALSE, FALSE, NULL))
  expect_false("interaction" %in% reg_footer_stats("ordinal", FALSE, FALSE, NULL))
  expect_true("interaction" %in% reg_footer_stats("multinomial", FALSE, FALSE, "interaction"))
  # the tab_vars effect-modification test kept its own key, so the two cannot collide
  expect_true("group_interact_lr" %in% names(TEST_ROWS))
  expect_true("cross_lr" %in% names(TEST_ROWS))
})

# ---- jamovi ---------------------------------------------------------------------------------

test_that("the jamovi picker folds a pair into `predictors`, replacing its parents", {
  pool <- c("race", "age4", "relig")
  k <- jmvtab_reg_cross_keys(list(list(var1 = "race", var2 = "age4")), pool)
  expect_identical(k, "race*age4")
  expect_identical(jmvtab_reg_cross_keys(list(list(var1 = "race", var2 = "nope")), pool),
                   character(0))
  expect_identical(jmvtab_reg_models(list(), pool, k), c("race*age4", "relig"))
  # a model card that does not hold both parents is untouched -- what makes with/without expressible
  m <- jmvtab_reg_models(list(list(label = "a", vars = c("race", "age4")),
                              list(label = "b", vars = c("race", "relig"))), pool, k)
  expect_identical(m$a, "race*age4")
  expect_identical(m$b, c("race", "relig"))
})
