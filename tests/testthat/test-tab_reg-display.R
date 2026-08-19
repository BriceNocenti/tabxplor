# The estimate-cell layouts: the est_ci bracket and the {} folds, in tab()'s own preset vocabulary
# (R/tab-display.R, DISPLAY_PRESETS). The est_ci bracket renders under special_formatting = TRUE (the
# main display path: console / kable / md); the folds are {} composites rendered on every backend.

reg_data <- function() {
  forcats::gss_cat |>
    dplyr::mutate(married = factor(dplyr::if_else(marital == "Married", "Married", "Not married")))
}
# Phase 18z13: skip the per-level `n` column (add_n = TRUE by default) -- see helper-reg.R.
first_fmt <- function(t) reg_first_fmt(t)

# ---- display = "est_ci" : visible confidence-interval bracket ---------------------------

test_that("display='est_ci' shows a visible CI bracket for OR and beta", {
  skip_if_not_installed("broom")
  d  <- reg_data()
  oc <- first_fmt(tab_reg(d, "married", c("race", "age"), display = "est_ci"))
  txt <- format(oc, special_formatting = TRUE)
  expect_true(any(grepl("\\[.*;.*\\]", txt)))            # "<or> [<lo>;<hi>]"
  expect_equal(get_num(oc), get_or(oc))                  # primary value = the odds ratio

  bc <- first_fmt(tab_reg(d, "tvhours", c("race", "age"), family = "gaussian",
                          display = "est_ci"))
  expect_true(any(grepl("\\[.*;.*\\]", format(bc, special_formatting = TRUE))))
  expect_equal(get_num(bc), get_diff(bc))                # beta point estimate
})

test_that("est_ci bracket reads the stored asymmetric bounds, on the cell's own scale", {
  skip_if_not_installed("broom")
  oc  <- first_fmt(tab_reg(reg_data(), "married", "age", display = "est_ci"))
  txt <- format(oc, special_formatting = TRUE)
  i   <- which(!is.na(get_ci_inf(oc)))[1]
  # a multiplicative bound below 1 prints as its inverse, exactly like the estimate it surrounds.
  lo  <- get_ci_inf(oc)[i]
  lo_s <- if (lo > 0 && lo < 1) paste0("1/", formatC(1 / lo, format = "f", digits = 2))
          else formatC(lo, format = "f", digits = 2)
  expect_match(txt[i], lo_s, fixed = TRUE)
})

test_that("options(tabxplor.ratio_print = 'raw') restores the plain bracket", {
  skip_if_not_installed("broom")
  withr::local_options(tabxplor.ratio_print = "raw")
  oc  <- first_fmt(tab_reg(reg_data(), "married", "age", display = "est_ci"))
  txt <- format(oc, special_formatting = TRUE)
  expect_false(any(grepl("1/", txt, fixed = TRUE)))
})

# ---- the folds: the estimate beside the level it sits on -------------------------------

test_that("display='est_base' folds the adjusted probability into the OR cell", {
  skip_if_not_installed("broom"); skip_if_not_installed("marginaleffects")
  oc  <- first_fmt(tab_reg(reg_data(), "married", "race", display = "est_base"))
  txt <- format(oc)
  expect_true(any(grepl("\\([0-9]", txt)))               # "(16%)" prediction
  expect_equal(get_num(oc), get_or(oc))                  # OR is still the primary field
  expect_true(any(!is.na(get_pct(oc))))                  # the prediction is stored in `pct`
  # the composite KEEPS the inverse form -- it is the one rule, in every rendering path
  expect_true(any(grepl("1/", txt, fixed = TRUE)))
})

test_that("display='base_est' shows the adjusted probability, graded by the effect", {
  skip_if_not_installed("broom"); skip_if_not_installed("marginaleffects")
  oc  <- first_fmt(tab_reg(reg_data(), "married", "race", display = "base_est"))
  expect_true(any(grepl("%", format(oc), fixed = TRUE)))
  # on the templated rows the LEVEL is now the primary field (the Constant keeps its own token)
  tpl <- get_display(oc) == "{base} ({est})"
  expect_equal(get_num(oc)[tpl], get_pct(oc)[tpl])
})

test_that("a {diff} fold puts the marginal effect beside the odds ratio", {
  skip_if_not_installed("broom"); skip_if_not_installed("marginaleffects")
  oc  <- first_fmt(tab_reg(reg_data(), "married", "race", display = "{est} ({diff})"))
  expect_true(any(grepl("\\([-+][0-9]", format(oc))))    # "(-21%)" / "(+1%)" marginal effect
  expect_equal(get_num(oc), get_or(oc))
})

test_that("the folds reach EVERY family: a gaussian cell folds an adjusted MEAN", {
  skip_if_not_installed("broom"); skip_if_not_installed("marginaleffects")
  bc <- first_fmt(tab_reg(reg_data(), "tvhours", "race", family = "gaussian",
                          display = "est_base"))
  expect_true(any(grepl("\\([0-9]", format(bc))))        # "(2.37)" the adjusted mean
  expect_true(any(!is.na(get_mean(bc))))                 # written into `mean`, not `pct`
  expect_equal(get_num(bc), get_diff(bc))                # the coefficient stays primary
})

test_that("display reaches the marginal path too -- one grammar, both builders", {
  skip_if_not_installed("broom"); skip_if_not_installed("marginaleffects")
  t <- suppressMessages(
    tab_reg(reg_data(), "married", "race", family = "binomial", effect = "marginal",
            display = "est_ci"))
  col <- t[[grep("^Model", names(t))[[1]]]]
  expect_true(any(get_display(col) == tabxplor:::DISPLAY_PRESETS[["est_ci"]]))
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

test_that("model effect columns carry their level's n; footer cells have no tooltip (Items D/L6)", {
  skip_if_not_installed("broom")
  d <- reg_data()
  t <- tab_reg(d, "married", c("race", "age"), family = "binomial")
  # each level's own base is on hover, and the Constant row carries the model N (Phase 22b-i)
  tips0 <- tabxplor:::tab_kable_print_tooltip(first_fmt(t))
  expect_true(any(grepl("n: ", tips0)))
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

# ---- Phase 22b-iii: every geometry of one comparison, on both roles ----------------------

test_that("a regression cell carries both geometries of its own comparison", {
  skip_if_not_installed("broom")
  d <- reg_data()
  # the pair is (level, reference level), so `diff` and `ratio` are filled on both roles and on
  # every measure -- only the column's OWN estimate field keeps what was fitted.
  for (meas in c("odds_ratio", "ratio", "difference")) {
    t <- suppressMessages(tab_reg(d, "married", "race", family = "binomial",
                                  measure = meas, empirical = TRUE))
    for (nm in c("Obs_OR", "Obs_RR", "Obs_RD", "Model_OR", "Model_RR", "Model_RD")) {
      if (!nm %in% names(t)) next
      col <- t[[nm]]
      i   <- which(!is_refrow(col) & as.character(t$var) == "race")
      expect_true(all(!is.na(get_diff(col)[i])),  label = paste(meas, nm, "diff"))
      expect_true(all(!is.na(get_ratio(col)[i])), label = paste(meas, nm, "ratio"))
    }
  }
})

test_that("the crude column's derived geometries ARE the observed ones", {
  skip_if_not_installed("broom")
  d   <- reg_data()
  t   <- suppressMessages(tab_reg(d, "married", "race", family = "binomial", empirical = TRUE))
  col <- t[["Obs_OR"]]
  # computed from the data, not from the grid: a parity check, not a tautology
  p   <- tapply(d$married == "Married", d$race, mean)
  p   <- stats::setNames(as.vector(p), names(p))          # tapply() returns a 1-d array
  ref <- levels(d$race)[1]
  lv  <- setdiff(as.character(t$levels)[as.character(t$var) == "race"], ref)
  i   <- match(lv, as.character(t$levels))
  expect_equal(get_diff(col)[i],  unname(p[lv] - p[[ref]]), tolerance = 1e-12)
  expect_equal(get_ratio(col)[i], unname(p[lv] / p[[ref]]), tolerance = 1e-12)
  expect_equal(get_diff(col)[match(ref, as.character(t$levels))], 0)   # the reference's neutral
  expect_equal(get_ratio(col)[match(ref, as.character(t$levels))], 1)
})

test_that("`base_est_mdiff` gives each ROLE its own arm, and never prints one field twice", {
  skip_if_not_installed("broom")
  d <- reg_data()
  t <- suppressMessages(tab_reg(d, "married", "race", family = "binomial", empirical = TRUE,
                                display = "base_est_mdiff"))
  expect_identical(unique(get_display(t[["Model_OR"]])[!is_refrow(t[["Model_OR"]])]),
                   "{est} ({diff})")
  expect_identical(unique(get_display(t[["Obs_OR"]])[!is_refrow(t[["Obs_OR"]])]),
                   "({base}) {est}")
  # on a risk-DIFFERENCE column `{est}` IS `{diff}`: the aside collapses instead of doubling
  rd <- suppressMessages(tab_reg(d, "married", "race", family = "binomial", measure = "difference",
                                 display = "base_est_mdiff"))
  # ... and what is left is the pipeline's own bare token, `{est}` resolved by the column's scale
  expect_identical(unique(get_display(rd[["Model_RD"]])[!is_refrow(rd[["Model_RD"]])]), "est")
})

test_that("the Model: footer names the aside the cell actually prints", {
  skip_if_not_installed("broom")
  d <- reg_data()
  line <- function(...) tabxplor:::reg_model_lines(
    suppressMessages(tab_reg(d, "married", "race", family = "binomial", empirical = TRUE, ...)))
  expect_match(line(display = "est_base"),        "adjusted predicted probability")
  expect_match(line(display = "base_est_mdiff"),  "as a difference")
  expect_match(line(display = "base_est_mratio"), "as a ratio")
  expect_false(grepl("in parentheses", line(display = "est")))
  # a gaussian outcome answers `{base}` with a mean, so the same clause words itself
  expect_match(tabxplor:::reg_model_lines(
    suppressMessages(tab_reg(d, "tvhours", "race", family = "gaussian", empirical = TRUE))),
    "adjusted predicted mean")
})

test_that("choosing a display changes no number (D11), presets included", {
  skip_if_not_installed("broom")
  d <- reg_data()
  t <- suppressMessages(tab_reg(d, "married", c("race", "age"), family = "binomial",
                                empirical = TRUE))
  for (p in c("est", "est_base", "base_est_mdiff", "base_est_mratio", "est_coef")) {
    for (nm in names(t)[purrr::map_lgl(t, is_fmt)]) {
      before <- t[[nm]]; after <- set_display(before, p)
      for (f in setdiff(tabxplor:::fmt_field_names, "display"))
        expect_identical(vctrs::field(after, f), vctrs::field(before, f),
                         label = paste(p, nm, f))
    }
  }
})
