# select method for class tabxplor_grouped_tab

select method for class tabxplor_grouped_tab

## Usage

``` r
# S3 method for class 'tabxplor_grouped_tab'
select(.data, ...)
```

## Arguments

- .data:

  A tibble of class `tabxplor_tab`.

- ...:

  One or more unquoted expressions separated by commas. Variable names
  can be used as if they were positions in the data frame, so
  expressions like `x:y` can be used to select a range of variables.

## Value

An object of class `tabxplor_grouped_tab`.
