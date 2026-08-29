# The model formulas a regression table fitted

Shows the formula behind every column of a
[`tab_reg()`](https://bricenocenti.github.io/tabxplor/reference/tab_reg.md)
table — exactly what reached
[`stats::glm()`](https://rdrr.io/r/stats/glm.html),
[`survey::svyglm()`](https://rdrr.io/pkg/survey/man/svyglm.html),
[`nnet::multinom()`](https://rdrr.io/pkg/nnet/man/multinom.html) or
[`MASS::polr()`](https://rdrr.io/pkg/MASS/man/polr.html). Use it to
check what a `shape =`, a `trials =` or a model formula really built.

## Usage

``` r
reg_formulas(x)
```

## Arguments

- x:

  A table built by
  [`tab_reg()`](https://bricenocenti.github.io/tabxplor/reference/tab_reg.md).

## Value

A tibble with one row per model: `model` (its name in the table),
`outcome`, `family` (the outcome family), `link` (the measure that model
estimates — the word `link =` takes), `fit` (the R call it was fitted
with) and `formula`. A `svyglm()` row also means robust (Huber-White)
standard errors: survey's design-based variance IS the sandwich, which
is why a ratio or a difference on a binary outcome is fitted through it.

## Details

One row per model: several `outcome`s give one each, a `predictors` list
one per model. Two things the list does not repeat: under `tab_vars` the
same formula is fitted **within each group**, and
`color = "between_groups"` (or `stats = "group_interaction"`) fits one
extra pooled model for the footer test only.

A summed score (`trials =`) is fitted on a success / failure pair, so
its formula names the two internal columns tabxplor builds for it
(`.gb_succ`, `.gb_fail`, and `.gb_trials` in the offset of the
risk-ratio link).

The formula names the columns as the user wrote them, but a continuous
predictor is fitted **anchored** at its `ref` (its mean by default), and
a `shape =` may have recoded it — neither changes any effect, only what
the Constant row means.

## See also

[`tab_reg()`](https://bricenocenti.github.io/tabxplor/reference/tab_reg.md),
[`reg_measures()`](https://bricenocenti.github.io/tabxplor/reference/reg_measures.md)
(what an outcome can be modelled as).

## Examples

``` r
# \donttest{
d <- forcats::gss_cat
d$married <- as.integer(d$marital == "Married")
reg_formulas(tab_reg(d, "married", c("race", "age"), family = "binomial"))
#> # A tibble: 1 × 6
#>   model       outcome family   link       fit                        formula    
#>   <chr>       <chr>   <chr>    <chr>      <chr>                      <chr>      
#> 1 married: OR married binomial odds_ratio "glm(binomial(\"logit\"))" married ~ …
# }
```
