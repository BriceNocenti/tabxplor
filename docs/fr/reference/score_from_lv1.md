# Create a score variable by counting factors at their first level

Builds an integer score column that counts, for each row, how many of
the listed factors sit at their **first level**. Each factor contributes
1 when it equals its first level and 0 otherwise, so the score ranges
from 0 to `length(vars_list)`. This is the natural way to turn a battery
of yes/no (or agree/disagree) survey items into a single summed score,
and it feeds the grouped-binomial outcome of
[`tab_reg()`](https://bricenocenti.github.io/tabxplor/fr/reference/tab_reg.md)
(its `trials` argument).

## Usage

``` r
score_from_lv1(data, name, vars_list)
```

## Arguments

- data:

  A data.frame.

- name:

  The name of the score variable to create (unquoted or a string); an
  existing column of that name is replaced.

- vars_list:

  The factors to count, as a character vector. For each one only its
  **first level** counts (as 1); every other level, including missing
  values, counts as 0.

## Value

`data` with the integer score column `name` added (or replaced).

## Details

The "first level" is `levels(as.factor(x))[1]` – the reference level of
the factor. Non-factor columns are coerced with
[`as.factor()`](https://rdrr.io/r/base/factor.html). Missing values are
folded into an explicit `"NA"` level before counting (via
[`forcats::fct_na_value_to_level()`](https://forcats.tidyverse.org/reference/fct_na_value_to_level.html)),
so an `NA` never matches the first level and contributes 0.

## See also

[`tab_reg()`](https://bricenocenti.github.io/tabxplor/fr/reference/tab_reg.md)
and its `trials` argument for modelling a summed score as a grouped
binomial;
[`vignette("tabxplor-programming")`](https://bricenocenti.github.io/tabxplor/fr/articles/tabxplor-programming.md)
for a worked example.

## Examples

``` r
data <- tibble::tibble(group = factor(c("G1", "G1", "G2", "G2", "G3", "G3")),
                       a = factor(c("Oui", "Oui", "Oui", "Oui", "Non", "Oui")),
                       b = factor(c("Oui", "Non", "Non", "Oui", "Non", "Oui")),
                       c = factor(c("Oui", "Oui", "Non", "Non", "Oui", "Oui")))
data |>
  score_from_lv1("score", vars_list = c("a", "b", "c")) |>
  tab(group, score, digits = 1)
#> # A tabxplor tab: 4 × 2
#>   group       score
#>   <fct>      <mean>
#> 1 G1     0.5 (σ0.7)
#> 2 G2     1.5 (σ0.7)
#> 3 G3     1.0 (σ1.4)
#> 4 Total  1.0 (σ0.9)
```
