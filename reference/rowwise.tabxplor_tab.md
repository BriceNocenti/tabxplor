# rowwise method for class tabxplor_tab

rowwise method for class tabxplor_tab

## Usage

``` r
# S3 method for class 'tabxplor_tab'
rowwise(data, ...)
```

## Arguments

- data:

  A tibble of class `tabxplor_tab`.

- ...:

  Variables to be preserved when calling
  [`summarise()`](https://dplyr.tidyverse.org/reference/summarise.html).
  This is typically a set of variables whose combination uniquely
  identify each row.

## Value

A tibble of class `tabxplor_grouped_tab` and `rowwise_df`.
