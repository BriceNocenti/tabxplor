# Diagnostic plots for a linear / generalized-linear model

**\[experimental\]**

A modern ggplot2 version of the base `plot.lm()` 2x2 diagnostic panel:
Residuals vs Fitted, Normal Q-Q, Scale-Location, and Residuals vs
Leverage (with Cook's-distance contours). Pass a fitted model, or a data
frame plus the variable names to fit one on the fly.

## Usage

``` r
lm_plots(
  object,
  dependent = NULL,
  predictors = NULL,
  family = "gaussian",
  wt = NULL,
  ...
)
```

## Arguments

- object:

  A fitted model (`lm` / `glm` / `svyglm`), OR a data frame (then supply
  `dependent` and `predictors`).

- dependent, predictors:

  When `object` is a data frame: the response and predictor column
  names.

- family:

  Model family for the data-frame form (default `"gaussian"`; e.g.
  `"binomial"`).

- wt:

  Optional weight column name for the data-frame form.

- ...:

  Unused, for future extension.

## Value

Invisibly, the assembled `gtable` (drawn on the current graphics
device).

## Examples

``` r
m <- stats::lm(tvhours ~ age, data = forcats::gss_cat)
if (requireNamespace("ggplot2", quietly = TRUE) &&
    requireNamespace("gridExtra", quietly = TRUE)) {
  lm_plots(m)
}
```
