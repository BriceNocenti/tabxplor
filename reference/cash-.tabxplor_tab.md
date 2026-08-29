# Extract a column of a tabxplor tab (with the n/add_pct back-compat shim)

Extract a column of a tabxplor tab (with the n/add_pct back-compat shim)

## Usage

``` r
# S3 method for class 'tabxplor_tab'
x$name

# S3 method for class 'tabxplor_tab'
x[[i, ...]]

# S3 method for class 'tabxplor_tab'
pull(.data, var = -1, name = NULL, ...)

# S3 method for class 'tabxplor_grouped_tab'
pull(.data, var = -1, name = NULL, ...)
```

## Arguments

- x:

  A `tabxplor_tab`.

- name:

  For `$`, a column name. For
  [`dplyr::pull`](https://dplyr.tidyverse.org/reference/pull.html), the
  column to use to name the result – see its documentation.

- i:

  A column name.

- ...:

  Passed on.

- .data:

  A `tabxplor_tab`.

- var:

  See [`dplyr::pull`](https://dplyr.tidyverse.org/reference/pull.html).

## Value

The column, or the reconstructed n/add_pct column (deprecated), or the
base method's value.
