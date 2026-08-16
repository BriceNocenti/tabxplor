# Phase 12h: estimate-cell display layouts (the est_ci CI bracket + the OR+prob / OR+ame folds), the
# Excel in-cell test label, and the split_var export footer. The est_ci bracket renders under
# special_formatting = TRUE (the main display path: console / kable / md); the folds are {} composites
# rendered on every backend.

reg_data <- function() {
  forcats::gss_cat |>
    dplyr::mutate(married = factor(dplyr::if_else(marital == "Married", "Married", "Not married")))
}
# Phase 18z13: skip the per-level `n` column (add_n = TRUE by default) -- see helper-reg.R.
first_fmt <- function(t) reg_first_fmt(t)

# ---- display = "ci" : visible confidence-interval bracket ---------------------------

test_that("display='ci' shows a visible CI bracket for OR and beta", {
  skip_if_not_installed("broom")
  d  <- reg_data()
  oc <- first_fmt(tab_reg(d, "married", c("race", "age"), display = "ci"))
  txt <- format(oc, special_formatting = TRUE)
  expect_true(any(grepl("\\[.*;.*\\]", txt)))            # "<or> [<lo>;<hi>]"
  expect_equal(get_num(oc), get_or(oc))                  # primary value = the odds ratio (no reciprocal)

  bc <- first_fmt(tab_reg(d, "tvhours", c("race", "age"), family = "gaussian",
                          display = "ci"))
  expect_true(any(grepl("\\[.*;.*\\]", format(bc, special_formatting = TRUE))))
  expect_equal(get_num(bc), get_diff(bc))                # beta point estimate
})

test_that("est_ci bracket reads the stored asymmetric bounds", {
  skip_if_not_installed("broom")
  oc  <- first_fmt(tab_reg(reg_data(), "married", "age", display = "ci"))
  txt <- format(oc, special_formatting = TRUE)
  # a non-reference cell's bracket contains the rounded ci_inf / ci_sup
  i   <- which(!is.na(get_ci_inf(oc)))[1]
  lo  <- formatC(get_ci_inf(oc)[i], format = "f", digits = 2)
  expect_match(txt[i], lo, fixed = TRUE)
})

# ---- display = "prob" / "ame" : OR + predicted probability / marginal effect ---------

test_that("display='prob' folds the predicted probability into the OR cell", {
  skip_if_not_installed("broom"); skip_if_not_installed("marginaleffects")
  oc  <- first_fmt(tab_reg(reg_data(), "married", "race", display = "prob"))
  txt <- format(oc)
  expect_true(any(grepl("\\([0-9]", txt)))               # "(16%)" prediction
  expect_equal(get_num(oc), get_or(oc))                  # OR is still the primary field
  expect_true(any(!is.na(get_pct(oc))))                  # the prediction is stored in `pct`
})

test_that("display='ame' folds the average marginal effect into the OR cell", {
  skip_if_not_installed("broom"); skip_if_not_installed("marginaleffects")
  oc  <- first_fmt(tab_reg(reg_data(), "married", "race", display = "ame"))
  expect_true(any(grepl("\\([-+][0-9]", format(oc))))    # "(-21%)" / "(+1%)" marginal effect
  expect_equal(get_num(oc), get_or(oc))
})

test_that("estimate_display prob/ame degrade to 'ci' for non-binomial (message)", {
  skip_if_not_installed("broom")
  d <- reg_data()
  expect_message(
    tab_reg(d, "tvhours", "race", family = "gaussian", display = "prob"),
    "binomial coefficient")
  # and ignored (with a message) for marginal-effects output
  skip_if_not_installed("marginaleffects")
  expect_message(
    tab_reg(d, "married", "race", family = "binomial", effect = "marginal", display = "ci"),
    "ignored")
})

# ---- Excel in-cell test label ----------------------------------------------------------------

test_that("Excel names the test in the p-value row label, not the numFmt (Phase 18m)", {
  skip_if_not_installed("openxlsx2")
  d  <- reg_data()
  ct <- suppressWarnings(tab(d, race, marital, pct = "row", test = TRUE))
  f  <- tempfile(fileext = ".xlsx")
  tab_xl(ct, path = f, replace = TRUE)
  tmp <- tempfile(); dir.create(tmp); utils::unzip(f, exdir = tmp)
  # the test type moved OUT of the cell numFmt and INTO the row label (a text cell)
  sx  <- paste(readLines(file.path(tmp, "xl", "styles.xml"), warn = FALSE), collapse = "")
  expect_no_match(sx, "Chi2")                            # no longer folded into a numFmt literal
  xmls <- list.files(tmp, pattern = "\\.xml$", recursive = TRUE, full.names = TRUE)
  all_xml <- paste(unlist(lapply(xmls, readLines, warn = FALSE)), collapse = "")
  expect_match(all_xml, "pvalue \\(Chi2")               # the p-value row name (text) states the test
})

# ---- split_var export footer -----------------------------------------------------------------

test_that("split_var tables get a per-group export footer; plain tables one footer at the end", {
  skip_if_not_installed("broom")
  d <- reg_data()
  # a models list keeps the STACKED per-group footer (the auto-spread side-by-side shape is tested
  # separately). The two models share ONE "Model fit" block per group, keyed on the group.
  t_split <- tab_reg(d, "married", list(m1 = "age", m2 = "age"), family = "binomial",
                     tab_vars = "race")
  # Phase g (A7): a styled md table's label cells use non-breaking spaces; normalise for text greps.
  md_s <- gsub(intToUtf8(160L), " ", tab_md(t_split, print = FALSE), fixed = TRUE)
  expect_true(grepl("Model fit", md_s))
  # one "Model fit" block per split group -> the footer labels repeat once per group
  n_groups <- nlevels(forcats::fct_drop(as.factor(d$race)))
  expect_equal(length(gregexpr("McFadden R2", md_s)[[1]]), n_groups)

  t_plain <- tab_reg(d, "married", "age")
  md_p <- gsub(intToUtf8(160L), " ", tab_md(t_plain, print = FALSE), fixed = TRUE)
  expect_true(grepl("Model fit", md_p))
  expect_equal(length(gregexpr("McFadden R2", md_p)[[1]]), 1L)  # a single block

  # split export renders through kable too
  expect_s3_class(suppressWarnings(tab_kable(t_split)), "kableExtra")
})

# ---- Phase 14r: tooltips + the AME NA bug ----------------------------------------------------

# Data with an ORDERED-factor income predictor whose levels contain " - " ($20000 - 24999, ...).
ame_data <- function() {
  forcats::gss_cat |>
    dplyr::mutate(
      married = factor(dplyr::if_else(marital == "Married", "01-Married", "02-Not married")),
      rincome = forcats::fct_recode(rincome, NULL = "No answer", NULL = "Refused",
                                    NULL = "Don't know", NULL = "Not applicable") |>
        forcats::fct_relevel(sort) |> as.ordered()
    )
}

test_that("an ordered-factor predictor's AME is non-NA on every non-reference level (Item E)", {
  skip_if_not_installed("broom")
  skip_if_not_installed("marginaleffects")
  d <- ame_data()
  suppressWarnings(t <- tab_reg(d, "married", c("race", "rincome"), family = "binomial",
                                effect = "marginal", cleannames = FALSE))
  col  <- first_fmt(t)
  rin  <- as.character(t[[2]]) %in% levels(d$rincome)   # rincome level rows
  # the '-' levels ($20000 - 24999, $15000 - 19999, $10000 - 14999) used to be NA; only the reference is
  ame  <- get_diff(col)
  expect_false(any(is.na(ame[rin]) & !is_refrow(col)[rin]))
  # the AME tooltip carries the model OR too (Item E)
  tips <- tabxplor:::tab_kable_print_tooltip(col)
  expect_true(any(grepl("OR: ", tips)))
})

test_that("an ordered-factor predictor's coefficient OR is non-NA (was all-NA)", {
  skip_if_not_installed("broom")
  d <- ame_data()
  suppressWarnings(t <- tab_reg(d, "married", c("race", "rincome"), family = "binomial",
                                cleannames = FALSE))
  col <- first_fmt(t)
  rin <- as.character(t[[2]]) %in% levels(d$rincome)
  or  <- get_or(col)
  expect_true(all(!is.na(or[rin] | is_refrow(col)[rin])))     # every rincome level keyed
  expect_gt(sum(!is.na(or[rin])), 0)
})

test_that("model effect columns drop the whole-model n; footer cells have no tooltip (Items D/L6)", {
  skip_if_not_installed("broom")
  d <- reg_data()
  t <- tab_reg(d, "married", c("race", "age"), family = "binomial")
  # no whole-model "n:" on the coefficient column (built table)
  tips0 <- tabxplor:::tab_kable_print_tooltip(first_fmt(t))
  expect_false(any(grepl("n: ", tips0)))
  # the GOF footer rows are materialised at display -> materialise, then check they carry no tooltip
  # (no nonsense "diff: +6378526%" on an AIC stored in the diff field)
  tm   <- tabxplor:::tab_materialize_extras(t, backend = "text", pvalue = TRUE)
  col  <- first_fmt(tm)
  tips <- tabxplor:::tab_kable_print_tooltip(col)
  disp <- tabxplor:::display_primary(get_display(col))
  foot <- disp %in% c("gof", "blank")
  expect_true(any(foot))
  expect_true(all(!nzchar(tips[foot])))
})

test_that("empirical columns keep the per-LEVEL n in the tooltip (Item D)", {
  skip_if_not_installed("broom")
  d <- reg_data()
  t <- tab_reg(d, "married", c("race", "rincome"), family = "binomial", empirical = TRUE)
  tips <- tabxplor:::tab_kable_print_tooltip(t[["Obs_OR"]])
  expect_true(any(grepl("n: ", tips)))          # per-level counts survive
})
