# Read a table's statistical tests

The tests a table carries and prints under itself: a crosstab's
chi-squared or ANOVA, a regression's model-fit statistics and global
tests. `get_test()` hands them back as a tidy tibble — one row per test,
keyed by `var` (the row variable, the predictor, or `""` for the whole
table) and `col` (the column variable it keys under) — so a test can be
filtered, reshaped or reported like any other data. A new kind of test
is new rows, never new columns.

The remaining columns name the statistic (`test`, `statistic`, `df1`,
`df2`, `pvalue`), the base it was computed on (`n`, `min_e`, `deff`) and
its effect size (`effect_size`, `es_type`). Per-CELL contributions to
the chi-squared are not here: they are the `ctr` field of the cells
themselves (`tabs$Total$ctr`).

## Usage

``` r
get_test(x)
```

## Arguments

- x:

  A `tabxplor_tab`.

## Value

A tibble of tests — empty, with the same columns, when the table ran
none (build them with `tab(test = TRUE)`); `NULL` only when `x` has lost
its attributes.

## See also

[`tab()`](https://bricenocenti.github.io/tabxplor/reference/tab.md) for
`test =`,
[`tab_structure()`](https://bricenocenti.github.io/tabxplor/reference/tab_structure.md)
and
[`tab_columns()`](https://bricenocenti.github.io/tabxplor/reference/tab_columns.md)
for the rest of a table's metadata.

## Examples

``` r
tabs <- tab(forcats::gss_cat, race, marital, test = TRUE)
get_test(tabs)
#> # A tibble: 1 × 14
#>   var   col     test  statistic   df1   df2    pvalue     n min_e effect_size
#>   <chr> <chr>   <chr>     <dbl> <dbl> <dbl>     <dbl> <dbl> <dbl>       <dbl>
#> 1 race  marital chi2       997.    10    NA 7.44e-208 21483  1.55       0.152
#> # ℹ 4 more variables: es_type <chr>, pvalue_exact <dbl>, deff <dbl>,
#> #   outcome <chr>
```
