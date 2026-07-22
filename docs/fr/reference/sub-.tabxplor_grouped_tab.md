# subset method for class tabxplor_grouped_tab

subset method for class tabxplor_grouped_tab

## Usage

``` r
# S3 method for class 'tabxplor_grouped_tab'
x[i, j, drop = FALSE]
```

## Arguments

- x:

  A tabxplor_grouped_tab object.

- i, j, ...:

  Indices

- drop:

  For matrices and arrays. If TRUE the result is coerced to the lowest
  possible dimension (see the examples). This only works for extracting
  elements, not for the replacement.

## Value

An object of class `tabxplor_grouped_tab`.
