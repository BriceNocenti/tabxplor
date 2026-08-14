# Phase 14w: reg table titles, legends, and headers. Everything is driven by the ONE table-level
# `reg_meta` attribute (family / effect / dependent / reference / predictors). Never uses pc18.

w14_data <- function() {
  forcats::gss_cat |>
    dplyr::mutate(married = factor(dplyr::if_else(marital == "Married", "Married", "Not married")))
}

w14_mnl <- function() {                                     # nominal 3-level party, Ind = reference
  forcats::gss_cat |>
    dplyr::mutate(party3 = factor(dplyr::case_when(
        grepl("democrat", partyid)   ~ "Dem",
        grepl("republican", partyid) ~ "Rep",
        partyid %in% c("Independent", "Ind,near rep", "Ind,near dem") ~ "Ind"),
      levels = c("Ind", "Dem", "Rep")))
}

# ---- reg_meta: the model record -------------------------------------------------------------

test_that("tab_reg() records reg_meta; a crosstab records none", {
  skip_if_not_installed("broom")
  t  <- tab_reg(w14_data(), "married", c("race", "rincome"), family = "binomial",
                cleannames = FALSE)
  m  <- tabxplor:::reg_call(t)
  expect_type(m, "list")
  expect_identical(m$family, "binomial")
  expect_identical(m$effect, "coefficient")
  expect_false(m$comparison)
  expect_identical(m$dependent, "married")
  expect_identical(unname(m$positive_level), "Married")   # inverse_two_level: first level modelled
  expect_setequal(m$predictors, c("race", "rincome"))

  ct <- tab(forcats::gss_cat, marital, race, pct = "row")
  expect_null(tabxplor:::reg_call(ct))
})

test_that("reg_meta survives dplyr verbs and footer materialisation", {
  skip_if_not_installed("broom")
  t <- tab_reg(w14_data(), "married", "race", family = "binomial", cleannames = FALSE)
  expect_false(is.null(tabxplor:::reg_call(dplyr::mutate(t, x = 1))))
  # reg_footer_lines() drops `test`; is_reg must NOT depend on it -> reg_meta must survive
  mat <- tabxplor:::tab_materialize_extras(t, backend = "text", pvalue = TRUE)
  expect_false(is.null(tabxplor:::reg_call(mat)))
})

# ---- titles / sheet names -------------------------------------------------------------------

test_that("reg_title names the model family, dependent and predictors", {
  skip_if_not_installed("broom")
  rt <- function(...) tabxplor:::reg_title(tabxplor:::reg_call(tab_reg(..., cleannames = FALSE)))
  expect_identical(rt(w14_data(), "married", c("race", "rincome"), family = "binomial"),
                   "Logistic regression: married by race, rincome")
  expect_identical(rt(forcats::gss_cat, "tvhours", "race", family = "gaussian"),
                   "Linear regression: tvhours by race")
  expect_true(startsWith(
    rt(forcats::gss_cat, "marital", "race", family = "multinomial"),
    "Multinomial logistic regression: marital by race"))
})

test_that("a model comparison title carries the dependent, reference level and effect", {
  skip_if_not_installed("broom")
  t  <- multi_logit(w14_data(), "married", models = list(demo = "race", full = c("race", "rincome")),
                    cleannames = FALSE)
  m  <- tabxplor:::reg_call(t)
  expect_true(m$comparison)
  expect_identical(tabxplor:::reg_title(m),
                   "Logistic regressions (models comparison): married, 'Married' (OR)")
})

test_that("reg_sheet_name is the compact tag", {
  skip_if_not_installed("broom")
  sn <- function(t) tabxplor:::reg_sheet_name(tabxplor:::reg_call(t))
  expect_identical(sn(tab_reg(w14_data(), "married", c("race", "rincome"),
                              family = "binomial", cleannames = FALSE)),
                   "logit_married_race_rincome")
  expect_identical(sn(suppressWarnings(tab_reg(forcats::gss_cat, "tvhours", "race",
                              family = "poisson", cleannames = FALSE))),
                   "poisson_tvhours_race")
})

# ---- headers (item 3) -----------------------------------------------------------------------

test_that("binomial: model + empirical columns share ONE outcome col_var; model named 'Model_OR'", {
  skip_if_not_installed("broom")
  t   <- tab_reg(w14_data(), "married", c("race", "rincome"), family = "binomial",
                 empirical = TRUE, cleannames = FALSE)
  # Phase 18z13: reg_fmt_cols() drops the per-level `n` column (add_n = TRUE by default) -- this
  # test is about the OUTCOME span shared by the model and its crude companions, and `n` is a row
  # descriptor that deliberately carries its own col_var so a border separates it.
  fmt <- reg_fmt_cols(t)
  expect_setequal(fmt, c("Obs_%", "Obs_OR", "Model_OR"))
  cvs <- purrr::map_chr(t[fmt], ~ tabxplor:::get_col_var(.x)[1])
  expect_equal(length(unique(cvs)), 1L)                       # one span, no border between them
  expect_identical(unname(cvs[["Model_OR"]]), "married: Married")
})

test_that("numeric outcome col_var is the dependent name alone", {
  skip_if_not_installed("broom")
  t <- tab_reg(forcats::gss_cat, "tvhours", "race", family = "gaussian", cleannames = FALSE)
  expect_identical(tabxplor:::get_col_var(t[["Model_\u03b2"]])[1], "tvhours")
})

test_that("multinomial: category names drop the repeated ': OR'; one shared col_var", {
  skip_if_not_installed("broom")
  skip_if_not_installed("nnet")
  t   <- tab_reg(w14_mnl(), "party3", "race", family = "multinomial", cleannames = FALSE)
  fmt <- reg_fmt_cols(t)
  expect_false(any(grepl(": OR$", fmt)))                       # stripped
  expect_true(all(grepl(" vs ", fmt)))                         # "<cat> vs <ref>"
  cvs <- unique(purrr::map_chr(t[fmt], ~ tabxplor:::get_col_var(.x)[1]))
  expect_equal(length(cvs), 1L)
  expect_identical(cvs, "party3: OR")
})

# ---- legend (items 2 + 5) -------------------------------------------------------------------

test_that("the 'Model:' line renders BEFORE the colour legend (md footer)", {
  skip_if_not_installed("broom")
  md <- tab_md(tab_reg(w14_data(), "married", "race", family = "binomial", cleannames = FALSE))
  lines <- strsplit(md, "\n")[[1]]
  model_at  <- grep("^Model: logistic regression", lines)[1]
  colour_at <- grep("Shades of (blue|yellow)", lines)[1]
  expect_true(!is.na(model_at) && !is.na(colour_at) && model_at < colour_at)
})

test_that("a reg legend says 'reference category', never 'Total row' (AME included)", {
  skip_if_not_installed("broom")
  skip_if_not_installed("marginaleffects")
  leg <- tabxplor:::tab_color_legend(
    tab_reg(w14_data(), "married", "race", family = "binomial", effect = "ame",
            cleannames = FALSE),
    medium = "md", style = "prose")
  expect_true(any(grepl("reference category", leg)))
  expect_false(any(grepl("Total row", leg)))
})

test_that("item 5: an Obs_IRR / model IRR legend names the RATE-ratio, not the odds-ratio", {
  skip_if_not_installed("broom")
  leg <- tabxplor:::tab_color_legend(
    suppressWarnings(tab_reg(forcats::gss_cat, "tvhours", "race", family = "poisson",
                             empirical = TRUE, cleannames = FALSE)),
    medium = "md", style = "prose")
  # Phase 16d: Obs_IRR + Model_IRR now fold into ONE legend line ("Obs_IRR, Model_IRR - ...", the
  # prefix names joined with no-break spaces), so match the line by "IRR" rather than a space-anchored prefix.
  irr <- leg[grepl("IRR", leg)]
  expect_true(length(irr) >= 1)
  expect_true(all(grepl("rate-ratio", irr)))
  expect_false(any(grepl("odds-ratio", irr)))
})
