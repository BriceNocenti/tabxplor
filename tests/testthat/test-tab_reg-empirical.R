# Phase 14v: the empirical (crude / unadjusted) companion framework.
# The governing statistical claim: the crude quantity IS the model quantity when there is ONE
# predictor. These tests prove, per family, that a SINGLE-predictor model estimate == the empirical
# column == the tab() (no-model) quantity. gss_cat-derived data only (never pc18 / ct13).

emp_data <- function() {
  d <- forcats::gss_cat
  d$race    <- forcats::fct_drop(d$race)
  d$married <- factor(as.integer(d$marital == "Married"), labels = c("no", "yes"))
  d$party3  <- factor(dplyr::case_when(grepl("dem", d$partyid, ignore.case = TRUE) ~ "Dem",
                                       grepl("rep", d$partyid, ignore.case = TRUE) ~ "Rep",
                                       TRUE ~ "Ind"),
                      levels = c("Ind", "Dem", "Rep"))
  d$inc3    <- factor(dplyr::case_when(d$rincome %in% c("$25000 or more") ~ "hi",
                                       d$rincome %in% c("Not applicable", "No answer",
                                                        "Don't know", "Refused") ~ NA_character_,
                                       TRUE ~ "lo"),
                      levels = c("lo", "hi"), ordered = FALSE)
  d$spectrum <- factor(d$party3, levels = c("Ind", "Dem", "Rep"), ordered = TRUE)
  d <- d[!is.na(d$tvhours) & !is.na(d$race) & !is.na(d$party3), , drop = FALSE]
  tibble::as_tibble(d)
}

# --- PARITY: single-predictor model == empirical column == tab() -----------------------------------

test_that("gaussian: single-predictor beta == crude mean-diff (Emp. diff) == tab() diff", {
  d <- emp_data()
  t <- tab_reg(d, "tvhours", "race", family = "gaussian", empirical = TRUE, cleannames = FALSE)
  emp_diff <- get_diff(t[["Emp. diff"]]); names(emp_diff) <- as.character(t$levels)
  emp_mean <- get_mean(t[["Emp. mean"]]); names(emp_mean) <- as.character(t$levels)

  beta <- stats::coef(stats::lm(tvhours ~ race, d))          # treatment contrasts: diff vs level 1
  # tab() crude: mean per level + difference vs the first level (ref = 1)
  tb   <- tab(d, race, tvhours, ref = 1)
  tab_diff <- get_diff(tb[["tvhours"]]); names(tab_diff) <- as.character(tb$race)
  tab_mean <- get_mean(tb[["tvhours"]]); names(tab_mean) <- as.character(tb$race)

  for (l in names(beta)[-1]) {                               # raceBlack, raceWhite, ...
    lev <- sub("^race", "", l)
    expect_equal(unname(emp_diff[lev]), unname(beta[l]),      tolerance = 1e-6)
    expect_equal(unname(emp_diff[lev]), unname(tab_diff[lev]), tolerance = 1e-6)
  }
  # the base mean column matches tab()'s mean, level by level
  common <- intersect(names(emp_mean), names(tab_mean))
  expect_equal(unname(emp_mean[common]), unname(tab_mean[common]), tolerance = 1e-6)
})

test_that("poisson: single-predictor IRR == crude rate-ratio (Emp. IRR) == tab() ratio", {
  d <- emp_data()
  t <- suppressWarnings(tab_reg(d, "tvhours", "race", family = "poisson", empirical = TRUE,
                                cleannames = FALSE))
  emp_irr <- get_or(t[["Emp. IRR"]]); names(emp_irr) <- as.character(t$levels)

  m   <- suppressWarnings(stats::glm(tvhours ~ race, d, family = stats::poisson()))
  irr <- exp(stats::coef(m))
  means <- tapply(d$tvhours, d$race, mean)                   # crude rate = mean count per level
  for (l in names(irr)[-1]) {
    lev <- sub("^race", "", l)
    expect_equal(unname(emp_irr[lev]), unname(irr[l]),                       tolerance = 1e-6)
    expect_equal(unname(emp_irr[lev]), unname(means[lev] / means[1]),        tolerance = 1e-6)
  }
})

# tab_reg's `inverse_two_level_factors` can model P(first level), so determine the modelled positive
# level empirically from Emp. % (it IS P(positive | level)); the hand quantities then match exactly.
emp_positive_level <- function(t, d, levcol) {
  r1 <- levels(d$race)[1]
  e1 <- unname(get_pct(t[["Emp. %"]])[match(r1, as.character(t$levels))])   # P(positive | race == r1)
  p_first <- mean(d[[levcol]][d$race == r1] == levels(d[[levcol]])[1])
  if (isTRUE(all.equal(e1, p_first, tolerance = 1e-6))) levels(d[[levcol]])[1]
  else                                                  levels(d[[levcol]])[2]
}

test_that("binomial coefficient: single-predictor OR == crude OR (Emp. OR) == model OR", {
  d <- emp_data()
  t <- tab_reg(d, "married", "race", family = "binomial", empirical = TRUE, cleannames = FALSE)
  emp_or <- get_or(t[["Emp. OR"]]); names(emp_or) <- as.character(t$levels)
  mod_nm <- setdiff(grep(": OR$", names(t), value = TRUE), "Emp. OR")      # the single-pred model col
  mod_or <- get_or(t[[mod_nm]]);    names(mod_or) <- as.character(t$levels)

  pos <- emp_positive_level(t, d, "married")
  m   <- stats::glm((married == pos) ~ race, d, family = stats::binomial())
  or  <- exp(stats::coef(m))
  for (l in names(or)[-1]) {
    lev <- sub("^race", "", l)
    expect_equal(unname(emp_or[lev]), unname(or[l]),      tolerance = 1e-6)
    expect_equal(unname(emp_or[lev]), unname(mod_or[lev]), tolerance = 1e-6)  # crude == adjusted (1 pred)
  }
})

test_that("binomial AME: single-predictor risk-diff (Emp. diff) == observed risk difference", {
  d <- emp_data()
  t <- tab_reg(d, "married", "race", family = "binomial", effect = "ame", empirical = TRUE,
               cleannames = FALSE)
  emp_diff <- get_diff(t[["Emp. diff"]]); names(emp_diff) <- as.character(t$levels)

  # a saturated single-predictor logit reproduces the observed proportions -> AME == observed risk diff
  pos  <- emp_positive_level(t, d, "married")
  p    <- tapply(as.integer(d$married == pos), d$race, mean)
  rdif <- p - p[1]
  for (lev in names(rdif)[-1]) {
    expect_equal(unname(emp_diff[lev]), unname(rdif[lev]), tolerance = 1e-6)
  }
  # the AME column carries "Emp. diff", NOT "Emp. OR", and the header names the (model %)
  expect_true("Emp. diff" %in% names(t))
  expect_false("Emp. OR"  %in% names(t))
  expect_true(any(grepl("model %", names(t), fixed = TRUE)))
})

test_that("multinomial: single-predictor RRR per category == crude 2x2 odds ratio", {
  skip_if_not_installed("nnet")
  d  <- emp_data()
  t  <- tab_reg(d, "party3", "race", family = "multinomial", cleannames = FALSE)
  m  <- nnet::multinom(party3 ~ race, d, trace = FALSE)
  co <- stats::coef(m)                                      # rows = non-ref categories, cols = terms
  yref <- levels(d$party3)[1]                               # "Ind"
  for (j in rownames(co)) {                                 # "Dem", "Rep"
    col <- get_or(t[[paste0(j, " vs ", yref, ": OR")]]); names(col) <- as.character(t$levels)
    for (term in colnames(co)[-1]) {
      lev  <- sub("^race", "", term)
      # crude OR from the {j, ref-cat} x {level, ref-level} 2x2
      sub  <- d[d$party3 %in% c(j, yref), ]
      a <- sum(sub$party3 == j   & sub$race == lev); b <- sum(sub$party3 == yref & sub$race == lev)
      cc<- sum(sub$party3 == j   & sub$race == levels(d$race)[1])
      e <- sum(sub$party3 == yref& sub$race == levels(d$race)[1])
      crude <- (a / b) / (cc / e)
      expect_equal(unname(col[lev]), unname(exp(co[j, term])), tolerance = 1e-5)
      expect_equal(unname(col[lev]), crude,                    tolerance = 1e-5)
    }
  }
})

test_that("ordinal: single-predictor cumulative OR brackets the cut-specific crude ORs", {
  skip_if_not_installed("MASS")
  d <- emp_data()
  o <- suppressWarnings(MASS::polr(spectrum ~ race, d, Hess = TRUE, method = "logistic"))
  cum_or <- exp(stats::coef(o))                             # one cumulative OR per race level
  # ordinal has no single tab() crude analogue; under proportional odds the pooled cumulative OR should
  # lie within the range of the cut-specific crude ORs (documented approximate check, not exact parity).
  lvls <- levels(d$spectrum)
  for (term in names(cum_or)) {
    lev <- sub("^race", "", term)
    cut_ors <- vapply(seq_len(length(lvls) - 1L), function(k) {
      hi <- as.integer(d$spectrum) > k
      a <- sum(hi & d$race == lev); b <- sum(!hi & d$race == lev)
      cc<- sum(hi & d$race == levels(d$race)[1]); e <- sum(!hi & d$race == levels(d$race)[1])
      (a / b) / (cc / e)
    }, numeric(1))
    expect_gte(unname(cum_or[term]), min(cut_ors) * 0.5)
    expect_lte(unname(cum_or[term]), max(cut_ors) * 2)
  }
})

# --- FEATURE tests --------------------------------------------------------------------------------

test_that("gaussian empirical: Emp. mean uncoloured, Emp. diff coloured by SD(Y) (matches beta)", {
  d <- emp_data()
  t <- tab_reg(d, "tvhours", "race", family = "gaussian", empirical = TRUE, cleannames = FALSE)
  expect_identical(get_color(t[["Emp. mean"]]), "")          # base descriptive: uncoloured
  expect_identical(get_type(t[["Emp. diff"]]),  "coef")
  expect_identical(get_color(t[["Emp. diff"]]), "diff")
  # var = var(Y) (constant), so the std-diff colour matches the model beta column exactly
  vy <- stats::var(d$tvhours)
  vv <- get_var(t[["Emp. diff"]]); vv <- vv[!is.na(vv)]
  expect_equal(unique(round(vv, 8)), round(vy, 8))
})

test_that("poisson empirical: Emp. rate (ratio colour) + Emp. IRR", {
  d <- emp_data()
  t <- suppressWarnings(tab_reg(d, "tvhours", "race", family = "poisson", empirical = TRUE,
                                cleannames = FALSE))
  expect_true(all(c("Emp. rate", "Emp. IRR") %in% names(t)))
  expect_identical(get_color(t[["Emp. rate"]]), "ratio")
  expect_identical(get_type(t[["Emp. IRR"]]),   "row")
})

test_that("empirical works with a VECTOR of dependents (one crude companion per dependent)", {
  d  <- emp_data()
  d$black <- factor(as.integer(d$race == "Black"), labels = c("no", "yes"))
  expect_no_message(
    t <- tab_reg(d, c("married", "black"), "inc3", family = "binomial", empirical = TRUE,
                 cleannames = FALSE),
    message = "not available")
  # per-dependent empirical columns, names suffixed by the dependent
  expect_true(any(grepl("Emp. % \\(married\\)", names(t))))
  expect_true(any(grepl("Emp. % \\(black\\)",   names(t))))
})

test_that("multinomial empirical: tooltip-only via the empirical_tips attribute; no crosstab leak", {
  skip_if_not_installed("nnet")
  d <- emp_data()
  t <- tab_reg(d, "party3", "race", family = "multinomial", empirical = TRUE, cleannames = FALSE)
  et <- tabxplor:::get_empirical_tips(t)
  expect_false(is.null(et))                                  # attribute set
  expect_true(all(c("col", "var", "level", "tip") %in% names(et)))
  expect_false(any(grepl("^Emp\\.", names(t))))              # NOT columns (tooltip only)

  # the html render carries the "crude:" fragment; a crosstab carries none (no field leak).
  # Use the WRAP the real tab_kable() path uses -- tab_wrap_text renames columns (spaces -> U+202F), so
  # the emp_tips keys must follow it (the wrap-rekey; a no-wrap prep would silently miss that bug).
  rd <- tabxplor:::tab_export_prep(
    t, backend = "kable", compute = c("refs", "colors", "bold", "range"),
    wrap = list(rows = 35, cols = 15, exdent = 2, whitespace_only = FALSE,
                unbreakable_spaces = TRUE, brk = "<br>"))$tables[[1]]
  h  <- paste(as.character(tabxplor:::render_html_engine(
    rd, meta = list(theme = "light"), subtext = "", caption = NULL,
    tooltips = TRUE, popover = FALSE, get_data = FALSE)), collapse = "\n")
  expect_match(h, "crude:")

  ct <- tab(d, party3, race, pct = "row")
  expect_null(tabxplor:::get_empirical_tips(ct))
})

test_that("ordinal empirical is ignored with a message (no clean crude analogue)", {
  d <- emp_data()
  expect_message(
    t <- suppressWarnings(tab_reg(d, "spectrum", "race", family = "ordinal", empirical = TRUE,
                                  cleannames = FALSE)),
    "not available")
  expect_false(any(grepl("^Emp\\.", names(t))))
})

test_that("display = 'ratio' renders the ratio field; legacy 'rr' still works", {
  d <- tab(forcats::gss_cat, race, tvhours, ref = 1, color = "ratio")
  r_ratio <- format(set_display(d[["tvhours"]], "ratio"))
  r_rr    <- format(set_display(d[["tvhours"]], "rr"))
  expect_identical(r_ratio, r_rr)                            # canonical == legacy
  expect_false(any(grepl("^\\s*[0-9]{3,}", r_ratio)))       # NOT the `n` count (the old bug)
  expect_true(any(grepl("\u00d7", r_ratio)))                # shows a ratio (multiply sign)
})

test_that("empirical_OR is a soft-deprecated alias for empirical", {
  d <- emp_data()
  lifecycle::expect_deprecated(
    t <- tab_reg(d, "married", "race", family = "binomial", empirical_OR = TRUE, cleannames = FALSE),
    "empirical")
  expect_true(all(c("Emp. %", "Emp. OR") %in% names(t)))
})
