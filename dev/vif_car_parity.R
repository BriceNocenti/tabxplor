# PURPOSE: pin tx_vif() against the real car::vif() it was vendored from, on every engine tab_reg()
#   can hand it and on both shapes the call sites read.
# ROLE: the standing evidence for R/reg-assumptions.R's vendored VIF. `car` is deliberately NOT a
#   tabxplor dependency any more, so this cannot live in tests/ -- a test may not depend on an
#   undeclared package. Re-run it after ANY change to tx_vif().
# USAGE: Rscript dev/vif_car_parity.R      (needs car, survey, MASS, nnet installed)
#   Exits non-zero on any mismatch, so it can be dropped into dev/release_checklist.md.

suppressMessages(devtools::load_all(".", quiet = TRUE))
stopifnot(requireNamespace("car", quietly = TRUE))

d <- forcats::gss_cat |>
  dplyr::filter(!is.na(age), !is.na(tvhours)) |>
  dplyr::mutate(married = as.integer(marital == "Married"),
                cnt     = tvhours,
                z       = as.numeric(scale(age)),
                ord     = factor(cut(tvhours, c(-1, 1, 3, 100), labels = c("lo", "mid", "hi")),
                                 levels = c("lo", "mid", "hi"), ordered = TRUE)) |>
  droplevels()
des <- survey::svydesign(ids = ~1, weights = ~1, data = d)

fits <- list(
  "lm"                    = stats::lm(tvhours ~ age + race + rincome, data = d),
  "lm (weighted)"         = stats::lm(tvhours ~ age + race, data = d, weights = rep(c(1, 2), length.out = nrow(d))),
  "lm (no intercept)"     = stats::lm(tvhours ~ 0 + age + z, data = d),
  "glm binomial"          = stats::glm(married ~ age + race + rincome, data = d, family = stats::binomial()),
  "glm binomial (1df)"    = stats::glm(married ~ age + tvhours, data = d, family = stats::binomial()),
  "glm poisson"           = stats::glm(cnt ~ age + race, data = d, family = stats::poisson()),
  "glm quasipoisson"      = stats::glm(cnt ~ age + race, data = d, family = stats::quasipoisson()),
  "glm + I(z^2)"          = stats::glm(married ~ z + I(z^2) + race, data = d, family = stats::binomial()),
  "glm + interaction"     = stats::glm(married ~ age * race, data = d, family = stats::binomial()),
  "svyglm quasibinomial"  = survey::svyglm(married ~ age + race, design = des, family = stats::quasibinomial()),
  "svyglm gaussian"       = survey::svyglm(tvhours ~ age + race, design = des),
  "polr (multi-df)"       = MASS::polr(ord ~ age + race, data = d, Hess = TRUE, method = "logistic"),
  "polr (all 1df)"        = MASS::polr(ord ~ age + z, data = d, Hess = TRUE, method = "logistic"),
  "svyolr"                = survey::svyolr(ord ~ age + race, design = des)
)

ok <- TRUE
cat(sprintf("%-24s %-9s %-9s %s\n", "fit", "car", "tx_vif", "verdict"))
for (nm in names(fits)) {
  f  <- fits[[nm]]
  cv <- tryCatch(suppressMessages(suppressWarnings(car::vif(f))), error = function(e) NULL)
  tv <- tx_vif(f)
  shp <- function(x) if (is.null(x)) "NULL" else if (is.matrix(x)) "matrix" else "vector"
  eq  <- isTRUE(all.equal(cv, tv, tolerance = 1e-13, check.attributes = TRUE))
  if (!eq) ok <- FALSE
  cat(sprintf("%-24s %-9s %-9s %s\n", nm, shp(cv), shp(tv), if (eq) "identical" else "*** DIFFERS ***"))
  if (!eq) { print(cv); print(tv) }
}

cat("\n-- refusals: tx_vif() must be NULL wherever car errors or returns NaN --\n")
d$age2 <- d$age
refuse <- list(
  "fewer than 2 terms" = stats::glm(married ~ race, data = d, family = stats::binomial()),
  "aliased"            = stats::glm(married ~ age + age2 + race, data = d, family = stats::binomial()),
  "multinom (block V)" = nnet::multinom(marital ~ age + race, data = d, trace = FALSE)
)
for (nm in names(refuse)) {
  cv <- tryCatch(suppressMessages(suppressWarnings(car::vif(refuse[[nm]]))),
                 error = function(e) structure("error", class = "carfail"))
  car_unusable <- inherits(cv, "carfail") || anyNA(cv) || any(is.nan(as.numeric(cv)))
  tv_null <- is.null(tx_vif(refuse[[nm]]))
  good <- car_unusable && tv_null
  if (!good) ok <- FALSE
  cat(sprintf("%-22s car unusable=%-5s tx_vif NULL=%-5s %s\n", nm, car_unusable, tv_null,
              if (good) "ok" else "*** DIFFERS ***"))
}

cat(if (ok) "\nVIF PARITY OK\n" else "\nVIF PARITY FAILED\n")
if (!ok) quit(status = 1L)
