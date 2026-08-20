# THE BASELINE ROW ("Constant") -- what it holds, how it renders, and what it rests on.
#
# The one rule: it holds THE QUANTITY THE COLUMN'S EFFECTS OPERATE ON, which `EST_SCALES$const_display`
# declares -- the baseline odds on an odds column, the level everywhere else, the link-scale intercept
# on a log one. So it never wears a comparison sign or a multiplicative glyph, since it has no
# reference to compare to.

bl_data <- function() {
  forcats::gss_cat |>
    dplyr::mutate(
      married = factor(dplyr::if_else(marital == "Married", "Married", "Not married"))
    )
}
bl_first <- function(t) t[[grep("^Model", names(t))[[1]]]]
bl_cst   <- function(t) which(as.character(t$var) == "Constant")

test_that("the baseline row renders a level, never an effect", {
  skip_if_not_installed("broom")
  d <- bl_data()
  # family x measure -> what the Constant is stamped with, and what it prints
  cases <- list(
    list(a = list(family = "binomial"),                        tok = "or",   pat = "^1/[0-9]"),
    list(a = list(family = "binomial", measure = "ratio"),     tok = "pct",  pat = "^[0-9]+%$"),
    list(a = list(family = "binomial", measure = "difference"),tok = "pct",  pat = "^[0-9.]+%$"),
    list(a = list(family = "binomial", measure = "log"),       tok = "coef", pat = "^-?[0-9.]+$"),
    list(a = list(family = "gaussian"),                        tok = "mean", pat = "^[0-9.]+$"),
    list(a = list(family = "gaussian", measure = "ratio"),     tok = "mean", pat = "^[0-9.]+$"),
    list(a = list(family = "poisson"),                         tok = "mean", pat = "^[0-9.]+$"),
    list(a = list(family = "poisson",  measure = "log"),       tok = "coef", pat = "^[0-9.]+$")
  )
  for (cs in cases) {
    y <- if (cs$a$family == "gaussian") "age" else if (cs$a$family == "poisson") "tvhours" else "married"
    t <- suppressWarnings(do.call(
      tab_reg, c(list(d, y, c("race", "rincome")), cs$a, list(stats = FALSE))))
    col <- bl_first(t); i <- bl_cst(t)
    expect_identical(get_display(col)[i], cs$tok, info = cs$a$family)
    expect_match(format(col)[i] |> trimws(), cs$pat)
    # ...and a baseline shown as a LEVEL carries no test: there is no null for a percentage or a mean
    expect_identical(is.na(get_pvalue(col)[i]),
                     identical(EST_SCALES[[get_scale(col)]]$const_display,
                               EST_SCALES[[get_scale(col)]]$base_display))
  }
})

test_that("an odds column keeps the baseline ODDS, with its level as the cell's aside", {
  skip_if_not_installed("broom"); skip_if_not_installed("marginaleffects")
  t   <- suppressMessages(tab_reg(bl_data(), "married", c("race", "rincome"),
                                  family = "binomial", empirical = TRUE, stats = FALSE))
  col <- bl_first(t); i <- bl_cst(t)
  o   <- get_or(col)[i]
  expect_true(is.finite(o))
  # the level beside it IS that odds read as a probability, so the two cannot disagree
  expect_equal(get_pct(col)[i], o / (1 + o), tolerance = 1e-9)
  expect_match(format(col)[i], "\\([0-9]+%\\)")
})

test_that("the marginal and at-reference baselines land on the same field as the coefficient one", {
  skip_if_not_installed("broom"); skip_if_not_installed("marginaleffects")
  d <- bl_data()
  for (eff in c("marginal", "at_reference")) {
    t <- suppressMessages(tab_reg(d, "married", c("race", "rincome"), family = "binomial",
                                  effect = eff, stats = FALSE))
    col <- bl_first(t); i <- bl_cst(t)
    expect_identical(get_display(col)[i], EST_SCALES[[get_scale(col)]]$const_display)
    expect_true(is.finite(get_num(col)[i]))
    expect_false(grepl("^[+]", trimws(format(col)[i])))     # a baseline never wears a sign
  }
})

test_that("a summed score's RISK ratio has its own scale, not the odds ratio's", {
  skip_if_not_installed("broom")
  d  <- bl_data() |> dplyr::mutate(score = pmin(as.integer(tvhours), 10L))
  or <- suppressWarnings(tab_reg(d, "score", "race", family = "binomial", trials = 10,
                                 stats = FALSE))[["Model_OR"]]
  rr <- suppressWarnings(tab_reg(d, "score", "race", family = "binomial", trials = 10,
                                 measure = "ratio", stats = FALSE))[["Model_RR"]]
  expect_identical(get_scale(or), "score_odds_ratio")
  expect_identical(get_scale(rr), "score_ratio")
  expect_identical(fmt_center_field(or), "or")
  expect_identical(fmt_center_field(rr), "ratio")
  # the reported defect: sharing one row printed every RR with the odds ratio's "1/x" glyph
  f <- format(rr)[!is.na(get_ratio(rr)) & get_ratio(rr) < 1]
  if (length(f)) expect_true(all(grepl(div_glyph, f, fixed = TRUE)))
  expect_false(any(grepl("1/", format(rr), fixed = TRUE)))
  # both baselines state the mean SCORE, the level a battery of items is read in
  expect_true(all(is.finite(c(get_mean(or)[bl_cst(or)], get_mean(rr)[1L]))))
})

test_that("the baseline's own base is the profile's count, or the population, or nothing", {
  skip_if_not_installed("broom")
  d  <- bl_data()
  nn <- function(t) {
    m <- tabxplor:::tab_materialize_extras(t, backend = "text", pvalue = FALSE)
    get_n(m[["n"]])[as.character(t$var) == "Constant"]
  }
  # every predictor categorical -> the reference profile IS a subgroup, and it is counted
  t1 <- suppressMessages(tab_reg(d, "married", c("race", "rincome"), family = "binomial",
                                 stats = FALSE))
  fr <- tidyr::drop_na(d[, c("married", "race", "rincome")])
  expect_equal(nn(t1), sum(fr$race == levels(forcats::fct_drop(fr$race))[[1]] &
                             fr$rincome == levels(forcats::fct_drop(fr$rincome))[[1]]))
  # a continuous predictor -> nobody is at the mean, by definition
  t2 <- suppressMessages(tab_reg(d, "married", c("race", "age"), family = "binomial",
                                 stats = FALSE))
  expect_true(is.na(nn(t2)))
  # ...and under `marginal` the row IS the population
  skip_if_not_installed("marginaleffects")
  t3 <- suppressMessages(tab_reg(d, "married", c("race", "age"), family = "binomial",
                                 effect = "marginal", stats = FALSE))
  expect_equal(nn(t3), nrow(tidyr::drop_na(d[, c("married", "race", "age")])))
})

test_that("a model check past its convention is MARKED, at the faintest shade", {
  skip_if_not_installed("broom"); skip_if_not_installed("car")
  d <- bl_data()
  set.seed(1)
  d$age2 <- as.numeric(d$age) + stats::rnorm(nrow(d), 0, 0.01)   # collinear on purpose
  t <- suppressMessages(tab_reg(d, "married", c("race", "age", "age2"), family = "binomial"))
  m <- tabxplor:::tab_materialize_extras(t, backend = "text", pvalue = TRUE)
  col <- m[[grep("^Model", names(m))[[1]]]]
  disp <- get_display(col)
  expect_true("gof_warn" %in% disp)                    # max VIF >= 10
  slot <- fmt_color_channels(col)$text_slot[disp == "gof_warn"]
  pl   <- tabxplor:::resolve_color_channel_plans(col)$text
  expect_identical(unique(slot), min(pl$under_slots[pl$under_slots > 0L]))
  # an ordinary model-fit number is not marked, and takes no colour at all
  expect_true(any(disp == "gof"))
  expect_true(all(fmt_color_channels(col)$text_slot[disp == "gof"] == 0L))
})


test_that("a logged MARGINAL column's baseline is the log of what its twin shows", {
  # `log_coef` is one row shared by every logged measure, so the column cannot say on its own whether
  # its exponential is an odds or a level -- and the baseline differs by exactly that. The estimand
  # records what it is the log OF (`log_of`), and the baseline is built on that scale, then logged.
  skip_if_not_installed("marginaleffects")
  d <- bl_data()
  arg <- list(d, "married", c("race", "rincome"), family = "binomial", effect = "marginal",
              stats = FALSE)
  lg <- suppressMessages(do.call(tab_reg, c(arg, list(measure = "log_risk"))))
  rr <- suppressMessages(do.call(tab_reg, c(arg, list(measure = "ratio"))))
  i  <- bl_cst(lg)
  lc <- bl_first(lg); rc <- bl_first(rr)
  expect_identical(get_scale(lc), "log_coef")
  # the twin shows the baseline LEVEL (a risk ratio multiplies the level), so the log column shows
  # its log -- and the interval with it, so `Constant + effect` is coherent on the link scale.
  expect_equal(get_diff(lc)[i], log(get_pct(rc)[i]), tolerance = 1e-10)
  expect_true(is.finite(get_ci_inf(lc)[i]) && is.finite(get_ci_sup(lc)[i]))
  expect_true(get_ci_inf(lc)[i] <= get_diff(lc)[i] && get_diff(lc)[i] <= get_ci_sup(lc)[i])
  # ...and it is still a baseline, so it carries no test.
  expect_true(is.na(get_pvalue(lc)[i]))
})

test_that("an odds-scale baseline logs to the log(OR) column's own Constant", {
  # the other arm of the same rule: an odds ratio multiplies ODDS, so its logged twin's baseline is
  # the log-odds, not the log of the probability.
  skip_if_not_installed("nnet")
  d <- bl_data()
  d$p3 <- factor(dplyr::case_when(grepl("dem", d$partyid, ignore.case = TRUE) ~ "Dem",
                                  grepl("rep", d$partyid, ignore.case = TRUE) ~ "Rep",
                                  TRUE ~ "Ind"), levels = c("Ind", "Dem", "Rep"))
  or <- suppressMessages(tab_reg(d, "p3", "race", family = "multinomial",
                                 effect = "at_reference", measure = "odds_ratio", stats = FALSE))
  lg <- suppressMessages(tab_reg(d, "p3", "race", family = "multinomial",
                                 effect = "at_reference", measure = "log_odds", stats = FALSE))
  i  <- bl_cst(or)
  oc <- or[[names(or)[vapply(or, is_fmt, logical(1))][[1]]]]
  lc <- lg[[names(lg)[vapply(lg, is_fmt, logical(1))][[1]]]]
  expect_identical(get_scale(oc), "odds_ratio")
  expect_identical(get_scale(lc), "log_coef")
  expect_equal(get_diff(lc)[i], log(get_or(oc)[i]), tolerance = 1e-10)
})
