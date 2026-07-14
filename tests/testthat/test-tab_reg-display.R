# Phase 12h: estimate-cell display layouts (the est_ci CI bracket + the OR+prob / OR+ame folds), the
# Excel in-cell test label, and the split_var export footer. The est_ci bracket renders under
# special_formatting = TRUE (the main display path: console / kable / md); the folds are {} composites
# rendered on every backend.

reg_data <- function() {
  forcats::gss_cat |>
    dplyr::mutate(married = factor(dplyr::if_else(marital == "Married", "Married", "Not married")))
}
first_fmt <- function(t) t[[names(t)[vapply(t, is_fmt, logical(1))][1]]]

# ---- estimate_display = "ci" : visible confidence-interval bracket ---------------------------

test_that("estimate_display='ci' shows a visible CI bracket for OR and beta", {
  skip_if_not_installed("broom")
  d  <- reg_data()
  oc <- first_fmt(tab_logit(d, "married", c("race", "age"), estimate_display = "ci"))
  txt <- format(oc, special_formatting = TRUE)
  expect_true(any(grepl("\\[.*;.*\\]", txt)))            # "<or> [<lo>;<hi>]"
  expect_equal(get_num(oc), get_or(oc))                  # primary value = the odds ratio (no reciprocal)

  bc <- first_fmt(tab_reg(d, "tvhours", c("race", "age"), family = "gaussian",
                          estimate_display = "ci"))
  expect_true(any(grepl("\\[.*;.*\\]", format(bc, special_formatting = TRUE))))
  expect_equal(get_num(bc), get_diff(bc))                # beta point estimate
})

test_that("est_ci bracket reads the stored asymmetric bounds", {
  skip_if_not_installed("broom")
  oc  <- first_fmt(tab_logit(reg_data(), "married", "age", estimate_display = "ci"))
  txt <- format(oc, special_formatting = TRUE)
  # a non-reference cell's bracket contains the rounded ci_inf / ci_sup
  i   <- which(!is.na(get_ci_inf(oc)))[1]
  lo  <- formatC(get_ci_inf(oc)[i], format = "f", digits = 2)
  expect_match(txt[i], lo, fixed = TRUE)
})

# ---- estimate_display = "prob" / "ame" : OR + predicted probability / marginal effect ---------

test_that("estimate_display='prob' folds the predicted probability into the OR cell", {
  skip_if_not_installed("broom"); skip_if_not_installed("marginaleffects")
  oc  <- first_fmt(tab_logit(reg_data(), "married", "race", estimate_display = "prob"))
  txt <- format(oc)
  expect_true(any(grepl("\\([0-9]", txt)))               # "(16%)" prediction
  expect_equal(get_num(oc), get_or(oc))                  # OR is still the primary field
  expect_true(any(!is.na(get_pct(oc))))                  # the prediction is stored in `pct`
})

test_that("estimate_display='ame' folds the average marginal effect into the OR cell", {
  skip_if_not_installed("broom"); skip_if_not_installed("marginaleffects")
  oc  <- first_fmt(tab_logit(reg_data(), "married", "race", estimate_display = "ame"))
  expect_true(any(grepl("\\([-+][0-9]", format(oc))))    # "(-21%)" / "(+1%)" marginal effect
  expect_equal(get_num(oc), get_or(oc))
})

test_that("estimate_display prob/ame degrade to 'ci' for non-binomial (message)", {
  skip_if_not_installed("broom")
  d <- reg_data()
  expect_message(
    tab_reg(d, "tvhours", "race", family = "gaussian", estimate_display = "prob"),
    "binomial coefficient")
  # and ignored (with a message) for marginal-effects output
  skip_if_not_installed("marginaleffects")
  expect_message(
    tab_reg(d, "married", "race", family = "binomial", effect = "ame", estimate_display = "ci"),
    "ignored")
})

# ---- Excel in-cell test label ----------------------------------------------------------------

test_that("Excel folds the in-cell test label into the numFmt literal", {
  skip_if_not_installed("openxlsx2")
  d  <- reg_data()
  ct <- suppressWarnings(tab(d, race, marital, pct = "row", chi2 = TRUE))
  f  <- tempfile(fileext = ".xlsx")
  tab_xl(ct, path = f, replace = TRUE)
  tmp <- tempfile(); dir.create(tmp); utils::unzip(f, exdir = tmp)
  sx  <- paste(readLines(file.path(tmp, "xl", "styles.xml"), warn = FALSE), collapse = "")
  expect_match(sx, "Chi2")                               # "0.00%\" (Chi2)\"" landed in the numFmt
})

# ---- split_var export footer -----------------------------------------------------------------

test_that("split_var tables get a per-group export footer; plain tables one footer at the end", {
  skip_if_not_installed("broom")
  d <- reg_data()
  t_split <- tab_logit(d, "married", "age", split_var = "race")
  md_s <- tab_md(t_split)
  expect_true(grepl("Model fit", md_s))
  # one "Model fit" block per split group -> the footer labels repeat once per group
  n_groups <- nlevels(forcats::fct_drop(as.factor(d$race)))
  expect_equal(length(gregexpr("McFadden R2", md_s)[[1]]), n_groups)

  t_plain <- tab_logit(d, "married", "age")
  md_p <- tab_md(t_plain)
  expect_true(grepl("Model fit", md_p))
  expect_equal(length(gregexpr("McFadden R2", md_p)[[1]]), 1L)  # a single block

  # split export renders through kable too
  expect_s3_class(suppressWarnings(tab_kable(t_split)), "kableExtra")
})
