# mutate method to access vctrs::fields of tabxplor_fmt vectors

mutate method to access vctrs::fields of tabxplor_fmt vectors

## Usage

``` r
# S3 method for class 'tabxplor_fmt'
mutate(.data, ...)
```

## Arguments

- .data:

  A tabxplor_fmt column.

- ...:

  Name-value pairs. The name gives the name of the column in the output
  (do not change it).

  The value can be:

  - A vector of length 1, which will be recycled to the correct length.

  - A vector the same length as the current group (or the whole data
    frame if ungrouped).

## Value

An object of class `tabxplor_fmt`.
