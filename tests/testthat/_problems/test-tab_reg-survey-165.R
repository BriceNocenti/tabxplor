# Extracted from test-tab_reg-survey.R:165

# prequel ----------------------------------------------------------------------
skip_if_not_installed("survey")
skip_if_not_installed("broom")
reg_survey_data <- function() {
  set.seed(42)
  n <- 1200L
  strata <- sample(c("A", "B", "C"), n, replace = TRUE)
  psu    <- paste0(strata, "-", sample(1:5, n, replace = TRUE))   # psu nested in strata
  x1     <- factor(sample(c("lo", "mid", "hi"), n, replace = TRUE))
  x2     <- rnorm(n)
  eta    <- -0.3 + 0.8 * (x1 == "hi") - 0.5 * (x1 == "mid") + 0.4 * x2
  y      <- rbinom(n, 1, plogis(eta))
  w      <- runif(n, 0.4, 3)
  tibble::tibble(y = factor(y), yb = factor(dplyr::if_else(y == 1, "event", "no")),
                 x1 = x1, x2 = x2, w = w, psu = psu, strata = strata)
}
or_col <- function(tab) {
  nm <- grep("^Model_", names(tab), value = TRUE)[1]
  vapply(tab[[nm]], tabxplor::get_num, numeric(1))
}
reg_survey_multi_data <- function() {
  set.seed(7); n <- 900L
  x1 <- factor(sample(c("lo", "mid", "hi"), n, replace = TRUE)); x2 <- rnorm(n)
  lp <- 0.6 * (x1 == "hi") - 0.4 * (x1 == "mid") + 0.3 * x2
  yo <- cut(lp + rnorm(n), breaks = c(-Inf, -0.5, 0.5, Inf),
            labels = c("low", "mid", "high"), ordered = TRUE)
  yn <- factor(sample(c("A", "B", "C"), n, replace = TRUE))
  w  <- runif(n, 0.5, 3)
  tibble::tibble(yo = yo, yn = yn, x1 = x1, x2 = x2, w = w)
}
reg_split_data <- function() {
  set.seed(11); n <- 1500L
  g  <- factor(sample(c("north", "south"), n, replace = TRUE))
  x1 <- factor(sample(c("a", "b", "c"), n, replace = TRUE)); x2 <- rnorm(n)
  y  <- rbinom(n, 1, plogis(-0.2 + 0.5 * (x1 == "b") + 0.3 * x2 + 0.4 * (g == "south")))
  tibble::tibble(y = factor(y), g = g, x1 = x1, x2 = x2, w = runif(n, 0.5, 3))
}

# test -------------------------------------------------------------------------
d <- reg_split_data()
t <- tab_logit(d, "y", c("x1", "x2"), split_var = "g")
expect_s3_class(t, "tabxplor_grouped_tab")
expect_setequal(dplyr::group_vars(t), c("g", "var"))
