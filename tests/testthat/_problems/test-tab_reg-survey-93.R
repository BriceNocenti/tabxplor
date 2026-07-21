# Extracted from test-tab_reg-survey.R:93

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
  nm <- grep("^Model ", names(tab), value = TRUE)[1]
  vapply(tab[[nm]], tabxplor::get_num, numeric(1))
}

# test -------------------------------------------------------------------------
d  <- reg_survey_data()
t0 <- tab_logit(d, "y", c("x1", "x2"))
hand <- stats::glm(as.integer(y == levels(y)[1]) ~ x1 + x2, data = d, family = binomial())
tv <- or_col(t0)
