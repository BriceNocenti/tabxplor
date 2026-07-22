# rename_with method for class tabxplor_grouped_tab

rename_with method for class tabxplor_grouped_tab

## Usage

``` r
# S3 method for class 'tabxplor_grouped_tab'
rename_with(.data, .fn, .cols = dplyr::everything(), ...)
```

## Arguments

- .data:

  A tibble of class `tabxplor_tab`.

- .fn:

  A function used to transform the selected `.cols`. Should return a
  character vector the same length as the input.

- .cols:

  Columns to rename; defaults to all columns.

- ...:

  Additional arguments passed onto `.fn`.

## Value

An object of class `tabxplor_grouped_tab`.
