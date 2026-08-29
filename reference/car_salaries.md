# Salaries of US college professors, 2008-09

Nine months' salary for 397 professors at one US college, collected by
the institution to monitor a pay gap. Used in the *Reading a regression*
article to show a gap that **grows** under adjustment, and then a
**mediator**: rank explains the gap away, but rank is itself part of
what is unequal.

## Usage

``` r
car_salaries
```

## Format

A tibble of 397 rows and 7 columns.

- rank:

  AsstProf, AssocProf, then Prof.

- discipline:

  "A" (theoretical) or "B" (applied).

- yrs.since.phd:

  Years since the PhD.

- yrs.service:

  Years of service.

- sex:

  Female or Male.

- salary:

  Nine-month salary, in US dollars.

- is_prof:

  Full professor or not: the same information as `rank`, asked as the
  yes/no question a percentage can answer. Added by tabxplor.

## Source

The complete `Salaries` data of the carData package (John Fox, Sanford
Weisberg and Brad Price), GPL (\>= 2) – with thanks. tabxplor's copy
adds `is_prof` and orders `rank` from assistant to full professor.

## Examples

``` r
tab_reg(car_salaries, "salary", c("sex", "discipline"))
#> ℹ "salary": continuous outcome detected -> `family = "gaussian"` (linear); it
#>   is integer-valued, so `family = "poisson"` if it is a count.
#> This message is displayed once per session.
#> | predictors      | Model fit                    |    salary |
#> |:----------------|:-----------------------------|----------:|
#> | sex, discipline | N                            |       397 |
#> |                 | Dispersion (robust/model SE) |      1.00 |
#> |                 | Collinearity (max VIF)       |      1.00 |
#> |                 | Influence (max dfbetas)      |      0.29 |
#> |                 | F                            |   0.0159% |
#> |                 | R2                           |     0.043 |
#> |                 | Adjusted R2                  |     0.039 |
#> |                 | Residual SD                  | 29 699.35 |
#> 
#> # A tabxplor tab: 5 × 5
#> # Outcome:        salary
#> # Groups:         var [3]
#>   var        levels              n             Obs_diff           Model_diff
#>                                <n>    <(obs mean) diff>    <diff (adj mean)>
#> 1 Constant   Reference profile  18                                 95 914   
#> 
#> 2 sex        Female             39 (101 002)       0          0    (101 056)
#> 3 sex        Male              358 (115 090) +14 088*** +14 029*** (115 085)
#> 
#> 4 discipline A                 181 (108 548)       0          0    (108 565)
#> 5 discipline B                 216 (118 029)  +9 480***  +9 449*** (118 014)
#> # Model: linear regression; diff: mean difference (vs the reference category); obs mean: observed mean; adj mean: adjusted/predicted mean.
#> # Obs_diff, Model_diff: diff in SD (ref.): -0.8 -0.4 -0.2 -0.1 +0.1 +0.2 +0.4 +0.8 [grey: non-significant or under ±0.1 SD]
#> # ***: significantly different from the reference category (in bold) at the 99% confidence level; **: at the 95% level; *: at the 90% level; no star: not significant.
```
