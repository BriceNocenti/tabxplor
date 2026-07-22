# arrange method for class tabxplor_tab

arrange method for class tabxplor_tab

## Usage

``` r
# S3 method for class 'tabxplor_tab'
arrange(
  .data,
  ...,
  .by_group = TRUE,
  .by_totals = TRUE,
  .only_main_display = TRUE,
  .locale = NULL
)
```

## Arguments

- .data:

  A tibble of class tabxplor_tab.

- ...:

  \<[`data-masking`](https://rlang.r-lib.org/reference/args_data_masking.html)\>
  Variables, or functions of variables. Use
  [`desc()`](https://dplyr.tidyverse.org/reference/desc.html) to sort a
  variable in descending order.

- .by_group:

  By default, will sort first by grouping variable. Set to `FALSE` to
  avoid this behaviour.

- .by_totals:

  By default, will put totals at the end of their group. Set to `FALSE`
  to avoid this behaviour.

- .only_main_display:

  By default, only the rows with the same display than the first row are
  arranged : if the first row of the group displays percentages, rows
  with n or pvalues are kept at the same place (typically, at the end of
  the group). Rows with the text `"row_pct"`, `"n"` or `"pvalue"` in the
  `row_var` name are also kept at the same place. Set to `FALSE` to
  avoid this behaviour.

- .locale:

  The locale to sort character vectors in.

## Value

A tibble of class `tabxplor__tab` or `tabxplor_grouped_tab`.
