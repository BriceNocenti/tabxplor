# rowwise method for class tabxplor_grouped_tab

rowwise method for class tabxplor_grouped_tab

## Usage

``` r
# S3 method for class 'tabxplor_grouped_tab'
rowwise(data, ...)
```

## Arguments

- data:

  A tibble of class `tabxplor_tab`.

- ...:

  Variables to be preserved when calling summarise(). This is typically
  a set of variables whose combination uniquely identify each row.

## Value

An object of class `tabxplor_grouped_tab` and `rowwise_df`.
