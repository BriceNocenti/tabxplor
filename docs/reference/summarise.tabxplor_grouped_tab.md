# summarise method for class tabxplor_grouped_tab

summarise method for class tabxplor_grouped_tab

## Usage

``` r
# S3 method for class 'tabxplor_grouped_tab'
summarise(.data, ..., .groups = NULL)
```

## Arguments

- .data:

  A tibble of class `tabxplor_tab`.

- ...:

  Name-value pairs of summary functions. The name will be the name of
  the variable in the result.

- .groups:

  Grouping structure of the result.

## Value

An object of class `tabxplor_grouped_tab`.
