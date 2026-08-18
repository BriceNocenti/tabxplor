# PURPOSE: Phase 18z8 -- the significance test of the `between_groups` gap. Two halves:
#   A. the per-cell test: the 21st fmt field `gap_se`, the MEASURES `bounds` closure it feeds, and the
#      three `color_signif` policies it unlocks for `color = "between_groups"`.
#   B. the AGGREGATED companion: one pooled `predictor x split_var` interaction test per predictor,
#      rendered as a table-wide footer LINE (not footer rows -- a pooled test belongs to no column).
# ROLE: the behavioural lock. The governing claim is again that nothing is invented: the SEs come from
#       the Wald intervals the table already prints, so the test can never contradict them.
# KEY CONSTRAINTS:
#   - The two split groups are DISJOINT, so sqrt(SE_A^2 + SE_B^2) is EXACT, not an approximation. The
#     fixtures compare against two hand-fitted glms, not against remembered numbers.
#   - `color_signif = "ignore"` must be BYTE-IDENTICAL to z5 (the descriptive reading is unchanged);
#     the other two policies gate on the gap's own interval, never on the model estimate's.
#   - The colour SIGN stays "away from vs toward the null" under EVERY policy, including the
#     guaranteed_effect floor -- a raw-signed bound would colour a protective effect backwards.
#   - The interaction rows are deliberately absent from reg_footer_spec(): the existing GOF footer must
#     be row-for-row unchanged when they are present.
# See: dev/model_vs_observed_gap_test.md (SS5 between_groups, SS5.3 the aggregated test).

gap_data <- function() {
  d <- forcats::gss_cat
  d$race    <- forcats::fct_drop(d$race)
  d$married <- factor(as.integer(d$marital == "Married"), labels = c("no", "yes"))
  d$party3  <- factor(dplyr::case_when(grepl("dem", d$partyid, ignore.case = TRUE) ~ "Dem",
                                       grepl("rep", d$partyid, ignore.case = TRUE) ~ "Rep",
                                       TRUE ~ "Ind"),
                      levels = c("Ind", "Dem", "Rep"))
  d <- d[!is.na(d$race) & !is.na(d$party3) & !is.na(d$age), , drop = FALSE]
  tibble::as_tibble(d)
}

gap_tab <- function(d, policy = "ignore", preds = "race", ...)
  suppressMessages(tab_reg(d, outcome = "married", predictors = preds, tab_vars = "party3",
                           family = "binomial", color = c(TRUE, "between_groups"),
                           color_signif = policy, ...))

# --- A. gap_se IS sqrt(SE_A^2 + SE_B^2), from the printed intervals --------------------------------

test_that("gap_se equals the quadrature of the two groups' model SEs", {
  d  <- gap_data()
  sp <- gap_tab(d)
  fc <- reg_fmt_cols(sp)
  testthat::expect_length(fc, 3L)

  hand <- function(g) {
    f <- stats::glm(married ~ race, stats::binomial, data = d[d$party3 == g, ])
    summary(f)$coefficients[, "Std. Error"]                 # (Intercept), raceBlack, raceWhite
  }
  se_ref <- hand("Ind")
  # skeleton rows are Constant, then race Other (the reference level, no SE) / Black / White
  for (g in c("Dem", "Rep")) {
    expected <- sqrt(hand(g)^2 + se_ref^2)
    got      <- get_gap_se(sp[[reg_group_col(sp, g)]])
    testthat::expect_equal(got[c(1L, 3L, 4L)], unname(expected), tolerance = 1e-6)
    testthat::expect_true(is.na(got[2L]))                   # the reference level has no interval
  }
  testthat::expect_true(all(is.na(get_gap_se(sp[[reg_group_col(sp, "Ind")]]))))  # a group is not compared to itself
})

test_that("gap_se is NA on every table that has no counterpart", {
  d <- gap_data()
  # a crosstab
  ct <- tab(d, race, party3, pct = "row")
  testthat::expect_true(all(vapply(ct[reg_fmt_cols(ct)],
                                   function(c) all(is.na(get_gap_se(c))), logical(1))))
  # a reg table with no split_var
  t <- suppressMessages(tab_reg(d, outcome = "married", predictors = "race", family = "binomial"))
  testthat::expect_true(all(is.na(get_gap_se(t$Model_OR))))
  # profile-likelihood bounds are not est +/- crit*se, so no SE is recovered from them
  pr <- gap_tab(d, ci_method = "profile")
  testthat::expect_true(all(is.na(get_gap_se(pr[[reg_group_col(pr, "Dem")]]))))
})

test_that("the gap p-value is the z test of the quadrature SE", {
  d  <- gap_data()
  sp <- gap_tab(d)
  g  <- log(get_or(sp[[reg_group_col(sp, "Rep")]])) - log(get_obs(sp[[reg_group_col(sp, "Rep")]]))
  testthat::expect_equal(fmt_gap_p(sp[[reg_group_col(sp, "Rep")]]),
                         2 * stats::pnorm(-abs(g / get_gap_se(sp[[reg_group_col(sp, "Rep")]]))))
})

# --- A. the three policies ------------------------------------------------------------------------

test_that("`ignore` is byte-identical to the descriptive z5 reading", {
  d  <- gap_data()
  sp <- gap_tab(d, "ignore")
  # the score, hence the slot, uses only `obs` -- gap_se cannot move it
  for (nm in reg_fmt_cols(sp)) {
    col  <- sp[[reg_group_col(sp, nm)]]
    bare <- set_gap_se(col, rep(NA_real_, length(col)))
    testthat::expect_identical(fmt_color_channels(col)$bg_slot,
                               fmt_color_channels(bare)$bg_slot)
  }
})

test_that("`grey_non_signif` greys exactly the non-significant gaps", {
  d  <- gap_data()
  ig <- gap_tab(d, "ignore")
  gn <- gap_tab(d, "grey_non_signif")
  seen <- 0L
  for (nm in c("Dem", "Rep")) {
    p   <- fmt_gap_p(gn[[reg_group_col(gn, nm)]])
    sig <- !is.na(p) & p < 0.05
    s_i <- fmt_color_channels(ig[[reg_group_col(ig, nm)]])$bg_slot
    s_g <- fmt_color_channels(gn[[reg_group_col(gn, nm)]])$bg_slot
    testthat::expect_true(all(s_g[!sig] == 0L))       # every non-significant cell is grey
    testthat::expect_identical(s_g[sig], s_i[sig])    # a significant one keeps the observed intensity
    seen <- seen + sum(sig & s_i > 0L)
  }
  testthat::expect_gt(seen, 0L)                       # the fixture must actually exercise the gate
})

test_that("`guaranteed_effect` colours the CI FLOOR of the gap, on the null-direction pole", {
  d  <- gap_data()
  ig <- gap_tab(d, "ignore")
  gu <- gap_tab(d, "guaranteed_effect")
  checked <- 0L
  for (nm in c("Dem", "Rep")) {
    # is_refrow excludes the regression Constant (a baseline is never an effect -- MEASURES$gate_row),
    # which the gap test can perfectly well find significant.
    p   <- fmt_gap_p(gu[[reg_group_col(gu, nm)]])
    sig <- !is.na(p) & p < 0.05 & !is_refrow(gu[[reg_group_col(gu, nm)]])
    if (!any(sig)) next
    s_i <- fmt_color_channels(ig[[reg_group_col(ig, nm)]])$bg_slot
    s_g <- fmt_color_channels(gu[[reg_group_col(gu, nm)]])$bg_slot
    testthat::expect_true(all(s_g[!sig] == 0L))
    testthat::expect_true(all(s_g[sig] > 0L))         # coloured <=> significant, the mode's invariant
    # the floor is dimmer than the point estimate, and on the SAME side of the palette
    over_i <- s_i[sig] %in% 1:4; over_g <- s_g[sig] %in% 1:4
    testthat::expect_identical(over_g, over_i)
    checked <- checked + sum(sig)
  }
  testthat::expect_gt(checked, 0L)
})

test_that("the score's sign wins over the raw gap's, so a protective effect folds correctly", {
  # est 0.50 attenuated to 0.60 -> the raw gap is POSITIVE (log .6 > log .5) but the effect moved
  # TOWARD the null, so both the colour and its interval must land on the under side.
  x <- fmt(n = 1L, or = 0.60, obs = 0.50, gap_se = 0.05, scale = "odds_ratio", pct_type = "row",
           display = "or", color = "between_groups", color_signif = "grey_non_signif")
  testthat::expect_lt(fmt_adjustment_score(x), 1)              # attenuated
  testthat::expect_gt(fmt_gap_raw(x), 0)                       # ... though the raw gap is positive
  b <- tabxplor:::fmt_gap_bounds(x)
  testthat::expect_lt(b$hi, 1)                                 # the whole interval sits below 1
  testthat::expect_true(fmt_color_channels(x)$text_slot %in% 5:8)
})

# --- A. legend + tooltip --------------------------------------------------------------------------

test_that("the legend names the gap's own test, per channel", {
  d   <- gap_data()
  leg <- function(t) tab_color_legend(t, medium = "plain", style = "prose", lang = "en")
  l   <- leg(gap_tab(d, "guaranteed_effect"))
  testthat::expect_true(any(grepl("reference group's effect", l, fixed = TRUE)))
  testthat::expect_true(any(grepl("two independent estimates", l, fixed = TRUE)))
  # the background's own tail must NOT borrow the model's interval name. Phase 18z13 (D7): pick the
  # line that HAS a background -- the reference group's own column now says "reference group" instead of
  # printing a ladder no cell of it can reach, and forms its own legend line.
  with_bg <- grep("the reference group's effect", l, fixed = TRUE, value = TRUE)
  testthat::expect_gt(length(with_bg), 0L)
  bg <- sub(".*Background colour", "", with_bg[[1]])
  testthat::expect_true(grepl("two independent estimates", bg, fixed = TRUE))
  # ... and the baseline column says what it is, rather than naming unreachable thresholds
  testthat::expect_true(any(grepl("reference group", l, fixed = TRUE)))
})

test_that("the tooltip carries the gap, its interval and its p", {
  d <- gap_data()
  h <- as.character(tab_html(gap_tab(d, "ignore"), tooltips = TRUE))
  tips <- unlist(regmatches(h, gregexpr('title="[^"]*"', h)))
  gaps <- grep("gap: ", tips, fixed = TRUE, value = TRUE)
  testthat::expect_gt(length(gaps), 0L)
  testthat::expect_true(all(grepl("p = ", gaps, fixed = TRUE)))
  testthat::expect_true(all(grepl("[", gaps, fixed = TRUE)))
  # never on a table with no counterpart
  h2 <- as.character(tab_html(tab(d, race, party3, pct = "row"), tooltips = TRUE))
  testthat::expect_false(grepl("gap: ", h2, fixed = TRUE))
})

# --- B. the aggregated interaction test ------------------------------------------------------------

test_that("the interaction test IS drop1() on the pooled model", {
  d <- gap_data()
  t <- suppressMessages(tab_reg(d, outcome = "married", predictors = c("race", "age"),
                                tab_vars = "party3", family = "binomial",
                                stats = c("n", "interaction")))
  it <- get_test(t)
  it <- it[it$test %in% tabxplor:::reg_interaction_types(), , drop = FALSE]
  # Phase 19g: the predictor rides `var`; the split-group level rides a column named after split_var
  testthat::expect_identical(sort(it$var), c("age", "race"))
  testthat::expect_identical(unique(it$test), "interact_lr")

  g  <- stats::glm(married ~ (race + age) * party3, stats::binomial, data = d)
  d1 <- stats::drop1(g, scope = c("race:party3", "age:party3"), test = "Chisq")
  testthat::expect_equal(it$pvalue[match(c("race", "age"), it$var)],
                         as.numeric(d1[["Pr(>Chi)"]][match(c("race:party3", "age:party3"),
                                                           rownames(d1))]))
  testthat::expect_equal(it$statistic[match(c("race", "age"), it$var)],
                         as.numeric(d1[["LRT"]][match(c("race:party3", "age:party3"), rownames(d1))]))
})

test_that("the footer line reaches every medium, once per model", {
  d <- gap_data()
  t <- gap_tab(d, "ignore", preds = c("race", "age"))
  ln <- tabxplor:::reg_interaction_lines(t, "en")
  testthat::expect_length(ln, 1L)
  testthat::expect_match(ln, "Interaction with party3")
  testthat::expect_match(ln, "race p = ")
  # it survives footer MATERIALISATION (which drops `test`) into md and html
  testthat::expect_true(any(grepl("Interaction with party3",
                                  strsplit(tab_md(t), "\n")[[1]], fixed = TRUE)))
  h <- as.character(tab_html(t))
  testthat::expect_true(grepl("Interaction with party3", h, fixed = TRUE))
  # and its p-values are entity-safe: a bare "<0.01%" in a raw-html footer is at a parser's mercy,
  # and the stars must not read as markdown emphasis on a knitted page
  line <- regmatches(h, regexpr("Interaction with party3.*?<br>", h, perl = TRUE))
  testthat::expect_length(line, 1L)
  testthat::expect_match(line, "&lt;0")                       # the p-value, entity-encoded
  testthat::expect_false(grepl("<0", line, fixed = TRUE))
  testthat::expect_false(grepl("*",  line, fixed = TRUE))     # stars are &#42;
})

test_that("`color = 'between_groups'` turns the interaction test on; `stats=` asks for it alone", {
  d <- gap_data()
  testthat::expect_message(
    t <- tab_reg(d, outcome = "married", predictors = "race", tab_vars = "party3",
                 family = "binomial", color = c(TRUE, "between_groups")),
    "interaction test")
  testthat::expect_length(tabxplor:::reg_interaction_lines(t, "en"), 1L)
  # off by default
  t0 <- suppressMessages(tab_reg(d, outcome = "married", predictors = "race", tab_vars = "party3",
                                 family = "binomial"))
  testthat::expect_length(tabxplor:::reg_interaction_lines(t0, "en"), 0L)
})

test_that("the interaction rows leave the GOF footer row-for-row unchanged", {
  d <- gap_data()
  base <- suppressMessages(tab_reg(d, outcome = "married", predictors = "race",
                                   tab_vars = "party3", family = "binomial"))
  with <- gap_tab(d, "ignore")
  gof  <- function(t) { tt <- get_test(t); tt[tt$test %in% tabxplor:::reg_footer_test_types(), ] }
  testthat::expect_equal(gof(base), gof(with))
  # and the rendered footer BLOCK has the same rows
  rows <- function(t) grep("Model fit|^\\| *\\|", strsplit(tab_md(t), "\n")[[1]], value = TRUE)
  testthat::expect_identical(length(rows(base)), length(rows(with)))
})

test_that("the statistic follows compare=: F for gaussian, design-based Wald when weighted", {
  d <- gap_data()
  gs <- suppressMessages(tab_reg(d[!is.na(d$tvhours), ], outcome = "tvhours", predictors = "race",
                                 tab_vars = "party3", family = "gaussian",
                                 stats = c("n", "interaction")))
  testthat::expect_identical(unique(get_test(gs)$test[get_test(gs)$test %in%
                                                        tabxplor:::reg_interaction_types()]),
                             "interact_f")
  d$w <- 1 + (as.integer(d$race) %% 3) / 2                    # deterministic weights
  wt <- suppressWarnings(suppressMessages(
    tab_reg(d, outcome = "married", predictors = "race", tab_vars = "party3",
            family = "binomial", wt = "w", stats = c("n", "interaction"))))
  it <- get_test(wt); it <- it[it$test %in% tabxplor:::reg_interaction_types(), ]
  testthat::expect_identical(unique(it$test), "interact_wald")
  testthat::expect_true(all(!is.na(it$pvalue)))
})

test_that("an unsupported engine degrades to no line, never to an error", {
  d <- gap_data()
  mn <- suppressWarnings(suppressMessages(
    tab_reg(d, outcome = "party3", predictors = "race", tab_vars = "marital",
            family = "multinomial", stats = c("n", "interaction"))))
  testthat::expect_length(tabxplor:::reg_interaction_lines(mn, "en"), 0L)
  testthat::expect_no_error(tab_md(mn))
})

# --- the at = "reference" estimand fix (a z5 defect) -----------------------------------------------

test_that("at = 'reference' writes no `obs`: the two columns are different estimands", {
  skip_if_not_installed("marginaleffects")
  d <- gap_data()
  testthat::expect_message(
    t <- tab_reg(d, outcome = "married", predictors = c("race", "party3"), family = "binomial",
                 effect = "at_reference", empirical = TRUE),
    "reference profile")
  mcol <- reg_fmt_cols(t)[[1]]
  testthat::expect_true(all(is.na(get_obs(t[[mcol]]))))
  testthat::expect_true("Obs_%" %in% names(t))          # the crude columns are still shown
})

# --- Phase 18z13: D7 (the reference group is choosable) / D11 (no writes without a reader) --------

test_that("D7: `reference` picks the split_var baseline instead of the first level", {
  skip_if_not_installed("broom")
  d  <- gap_data()
  # the baseline group is the one with no `obs` (a group is not compared to itself)
  base_of <- function(t) {
    fc <- reg_fmt_cols(t)
    fc[vapply(fc, function(nm) all(is.na(get_obs(t[[reg_group_col(t, nm)]]))), logical(1))]
  }
  b0 <- base_of(gap_tab(d))
  b1 <- base_of(gap_tab(d, ref = c(party3 = "Rep")))
  testthat::expect_true(grepl("Ind", b0[[1]], fixed = TRUE))   # the first level, by default
  testthat::expect_true(grepl("Rep", b1[[1]], fixed = TRUE))   # ... and it is choosable
  # z5/z8 sent `ref = NULL` into the split recursion and left tab_vars out of the relevelable
  # set, so the only way to move the baseline was to relevel the data upstream.
  testthat::expect_false(identical(b0, b1))
})

test_that("D11: obs / gap_se are written only where a gap measure reads them", {
  skip_if_not_installed("broom")
  d  <- gap_data()
  sp <- suppressMessages(tab_reg(d, "married", list(m1 = "race", m2 = "race"),
                                 tab_vars = "party3", family = "poisson",
                                 empirical = TRUE, color = c(TRUE, "between_groups")))
  fc <- reg_fmt_cols(sp)
  mdl <- fc[get_role(sp[fc]) == "model"]
  emp <- fc[get_role(sp[fc]) == "emp"]
  testthat::expect_gt(length(mdl), 0L)
  testthat::expect_gt(length(emp), 0L)
  # the model columns declare the measure, so they carry the comparison...
  testthat::expect_true(any(vapply(mdl, function(nm) any(!is.na(get_obs(sp[[reg_group_col(sp, nm)]]))), logical(1))))
  # ... the Obs_* companions colour on their own diff / OR measure and never read `obs`: writing it
  # there stored a value with no consumer, and put an "obs:" tooltip line on the observed column itself.
  for (nm in emp) {
    testthat::expect_true(all(is.na(get_obs(sp[[reg_group_col(sp, nm)]]))), info = nm)
    testthat::expect_true(all(is.na(get_gap_se(sp[[reg_group_col(sp, nm)]]))), info = nm)
  }
})
