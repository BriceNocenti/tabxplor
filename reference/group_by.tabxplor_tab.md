# group_by method for class tabxplor_tab

group_by method for class tabxplor_tab

## Usage

``` r
# S3 method for class 'tabxplor_tab'
group_by(.data, ..., .add = FALSE, .drop = dplyr::group_by_drop_default(.data))
```

## Arguments

- .data:

  A tibble of class `tabxplor_tab`.

- ...:

  Variables or computations to group by.

- .add:

  When `FALSE`, the default,
  [`group_by()`](https://dplyr.tidyverse.org/reference/group_by.html)
  will override existing groups. To add to the existing groups, use
  `.add = TRUE`.

- .drop:

  Drop groups formed by factor levels that don't appear in the data? The
  default is `TRUE` except when `.data` has been previously grouped with
  `.drop = FALSE`.

## Value

A tibble of class `tabxplor_grouped_tab`.
