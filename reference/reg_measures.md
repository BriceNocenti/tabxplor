# What can this outcome be modelled as?

Lists what
[`tab_reg()`](https://bricenocenti.github.io/tabxplor/reference/tab_reg.md)
can report for one outcome: which models it could fit, and which measure
of deviation each of them yields. It reads the same runtime table the
argument validator and the error messages read, so what it prints is
what the function does. The section below is the same table for every
kind of outcome, read without any data.

## Usage

``` r
reg_measures(data, outcome, family = "auto", link = "auto")
```

## Arguments

- data:

  A data frame (or a `survey` design), as for
  [`tab_reg()`](https://bricenocenti.github.io/tabxplor/reference/tab_reg.md).

- outcome:

  The outcome column name.

- family:

  The model family. `"auto"` (default) lists every family this kind of
  outcome can take, the detected one first — which is the choice to make
  before the others.

- link:

  Which measure the model estimates. `"auto"` (default) reads the table
  at each family's own model; `"all"` adds every other link it fits;
  name one to read it at that model alone.

## Value

A tibble of `family` (only when several are listed), `link`, `measure`,
`effect`, `header` (the column name it would produce) and `reads_as`
(what that header's acronym means). With `link = "all"`, a `base_link`
column says which model is the family's own (`NA` on the prediction
rows, which belong to no link in particular).

## Details

**The table has two blocks**, because the grid factors:

- one row per model you could fit — its **`link`**, and the measure that
  model's own coefficients carry (`effect = "conditional"`);

- then the measures read off the model's **predictions**, which are the
  same whichever model you fit — `link` reads `"(any)"` there.

So `link` is the choice that matters, and it decides only which measure
comes with a *coefficient*: everything else is available from any of
them.

By default only each family's **own** model is listed — the one it fits
unless told otherwise. `link = "all"` adds the others, which are
specialist choices, and marks the family's own with `base_link`.

Only what can be built is listed. A measure this kind of outcome does
not have simply has no row, and the message says why (an odds ratio
needs a probability to take the odds of). One state exists only at run
time: a link that does not converge on your data.
[`tab_reg()`](https://bricenocenti.github.io/tabxplor/reference/tab_reg.md)
says so and, for the risk difference, falls back to the linear
probability model.

## Which models each outcome offers, and which measures

Generated from the package's own resolution table, so it cannot drift
from what
[`tab_reg()`](https://bricenocenti.github.io/tabxplor/reference/tab_reg.md)
builds. A measure that IS the model's own is read off its coefficients;
any other is computed from its predictions (`effect = "marginal"` or
`"at_reference"`).

- **gaussian** — a cell is a mean. Models: `link = "difference"`, the
  default (linear regression); `link = "ratio"` (log-link mean
  regression (Poisson pseudo-likelihood, robust standard errors)).
  Reported: `measure = ``"difference"`, `"ratio"`, `"raw_coefficient"`.

- **binomial** — a cell is a percentage. Models: `link = "odds_ratio"`,
  the default (logistic regression); `link = "ratio"` (modified Poisson
  regression); `link = "difference"` (additive-risk regression (identity
  link, robust standard errors)). Reported: `measure = ``"ratio"`,
  `"difference"`, `"odds_ratio"`, `"raw_coefficient"`.

- **poisson** — a cell is a count. Models: `link = "ratio"`, the default
  (Poisson regression). Reported: `measure = ``"difference"`, `"ratio"`,
  `"raw_coefficient"`.

- **multinomial** — a cell is a percentage. Models:
  `link = "odds_ratio"`, the default (multinomial logistic regression).
  Reported: `measure = ``"ratio"`, `"difference"`, `"odds_ratio"`,
  `"raw_coefficient"`.

- **ordinal** — a cell is a position on an ordered scale. Models:
  `link = "odds_ratio"`, the default (ordinal logistic regression).
  Reported: `measure = ``"difference"`, `"ratio"`, `"odds_ratio"`,
  `"raw_coefficient"`.

## See also

[`tab_reg()`](https://bricenocenti.github.io/tabxplor/reference/tab_reg.md)
to build the table,
[`reg_formulas()`](https://bricenocenti.github.io/tabxplor/reference/reg_formulas.md)
to see the formula each column was fitted with.

## Examples

``` r
reg_measures(car_arrests, "released")
#> ℹ "released" is a `family = "binomial"` outcome.
#> ℹ Any of these also reads on the model's own scale: `measure =
#>   "raw_coefficient"`.
#> ℹ `link = "all"` lists the other models each family can fit.
#> # A tibble: 4 × 5
#>   link       measure    effect                header    reads_as                
#>   <chr>      <chr>      <chr>                 <chr>     <chr>                   
#> 1 odds_ratio odds_ratio conditional           Model_OR  odds ratio              
#> 2 (any)      difference marginal|at_reference Model_mRD marginal risk difference
#> 3 (any)      ratio      marginal|at_reference Model_mRR marginal risk ratio     
#> 4 (any)      odds_ratio marginal|at_reference Model_mOR marginal odds ratio     
reg_measures(car_salaries, "salary")
#> ℹ "salary" is a `family = "gaussian"` outcome.
#> ℹ It can also be asked as `family = "binomial"` or `family = "poisson"`.
#> ℹ `family = "binomial"` reads it as a score out of q items: pass `trials`.
#> ℹ Any of these also reads on the model's own scale: `measure =
#>   "raw_coefficient"`.
#> ℹ `link = "all"` lists the other models each family can fit.
#> # A tibble: 10 × 6
#>    family   link       measure    effect                header      reads_as    
#>    <chr>    <chr>      <chr>      <chr>                 <chr>       <chr>       
#>  1 gaussian difference difference conditional           Model_diff  mean differ…
#>  2 gaussian (any)      difference marginal|at_reference Model_mdiff marginal me…
#>  3 gaussian (any)      ratio      marginal|at_reference Model_mRoM  marginal ra…
#>  4 binomial odds_ratio odds_ratio conditional           Model_OR    odds ratio  
#>  5 binomial (any)      difference marginal|at_reference Model_mRD   marginal ri…
#>  6 binomial (any)      ratio      marginal|at_reference Model_mRR   marginal ri…
#>  7 binomial (any)      odds_ratio marginal|at_reference Model_mOR   marginal od…
#>  8 poisson  ratio      ratio      conditional           Model_IRR   incidence-r…
#>  9 poisson  (any)      difference marginal|at_reference Model_mdiff marginal me…
#> 10 poisson  (any)      ratio      marginal|at_reference Model_mIRR  marginal in…
```
