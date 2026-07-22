# Pillar_shaft method to print class fmt in a [`tibble`](https://tibble.tidyverse.org/reference/tibble.html) column

Pillar_shaft method to print class fmt in a
[`tibble`](https://tibble.tidyverse.org/reference/tibble.html) column

## Usage

``` r
# S3 method for class 'tabxplor_fmt'
pillar_shaft(x, ..., .ref = NULL)
```

## Arguments

- x:

  A fmt object.

- ...:

  Other parameter.

- .ref:

  Internal: precomputed reference masks, as
  `list(cells =, all_totals =)`, threaded to
  [`format()`](https://rdrr.io/r/base/format.html) to avoid deriving
  them again (exporters compute them once for the whole table). `NULL`
  (the default, and the console path) recomputes them. Not for direct
  use.

## Value

A fmt printed in a pillar.
