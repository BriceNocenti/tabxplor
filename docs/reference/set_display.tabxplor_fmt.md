# Set the "display" vctrs::field of a `fmt` vector.

Set the "display" vctrs::field of a `fmt` vector.

## Usage

``` r
# S3 method for class 'tabxplor_fmt'
set_display(x, value)
```

## Arguments

- x:

  The object to test, to get a field in, or to modify.

- value:

  The value you want to inject in some `fmt` vector's vctrs::field or
  attribute using a given "set" function.

## Value

A fmt vectors with the wanted display.

## Details

The special value `value = "num_ci"` is a type-adaptive alias for the
`"\{base\} \{ci\}"` composite: it writes `"\{pct\} \{ci\}"` on
percentage/frequency columns and `"\{mean\} \{ci\}"` on numeric (mean)
columns, so each value cell shows its base value followed by whatever
confidence interval the table carries (a cell, difference or ratio CI,
as driven by `ci = ` / `color`). It is a display overlay: cells with no
CI show the bare base value.
