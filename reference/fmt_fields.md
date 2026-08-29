# Per-cell fields of a `fmt` vector

Read and write the values that vary from cell to cell: the displayed
number, its decimals, its p-value, and the kind of row each cell sits
in. Every `fmt` vector carries all of them, an inapplicable one stored
as `NA` — so these always answer, even on a column where the field means
nothing.

Use them on a single `fmt` vector or, through
`dplyr::across(where(is_fmt), ...)`, on a whole table. To reach a field
these do not name, use `x$<field>` or
[`vctrs::field()`](https://vctrs.r-lib.org/reference/fields.html).

## Usage

``` r
get_num(x)

set_num(x, value)

is_totrow(x, ...)

get_row_kind(x)

set_row_kind(x, row_kind)

as_totrow(x, in_totrow = TRUE)

is_tottab(x, ...)

as_tottab(x, in_tottab = TRUE)

set_display(x, value)

is_refrow(x, ...)

as_refrow(x, in_refrow = TRUE)

get_digits(x)

get_pvalue(x)

set_digits(x, value)

set_pvalue(x, value)
```

## Arguments

- x:

  The object to test, to get a field in, or to modify.

- value:

  The value you want to inject in some `fmt` vector's vctrs::field or
  attribute using a given "set" function.

- ...:

  In
  [`fmt()`](https://bricenocenti.github.io/tabxplor/reference/fmt.md),
  it exists only for the arguments retired in tabxplor 2.0.0: `type` is
  translated into `scale` + `pct_type` (see
  [`tabxplor-type`](https://bricenocenti.github.io/tabxplor/reference/tabxplor-type.md)),
  `ci_type` gets an error naming its replacement. In the accessor
  methods below, to add arguments in the future.

- row_kind:

  The kind of row a cell sits in (see `get_row_kind`).

- in_totrow:

  **\[deprecated\]** Use `row_kind = "total"`.

- in_tottab:

  `TRUE` when the cell is part of a total table

- in_refrow:

  `TRUE` when the cell is part of a reference row (cf. `ref`)

## Value

A getter returns a vector the length of `x`; a setter the modified `fmt`
vector. Given a data.frame, a getter answers once per `fmt` column.

## Functions

- `get_num()`: get the currently displayed field

- `set_num()`: set the currently displayed field (not changing display
  type)

- `is_totrow()`: test function to detect cells in total rows (at `fmt`
  level or `tab` level)

- `get_row_kind()`: get the "row_kind" field: what kind of row each cell
  sits in (one of `"data"`, `"total"`, `"n"`, `"pct"`, `"pvalue"`,
  `"gof"`, `"blank"`).

- `set_row_kind()`: set the "row_kind" field

- `as_totrow()`: set the "total" row kind (belong to total row)

- `is_tottab()`: test function to detect cells in total tables (at `fmt`
  level or `tab` level)

- `as_tottab()`: set the "in_tottab" field (belong to total table)

- `set_display()`: set the "display" vctrs::field of a `fmt` vector, or
  of all of them in the whole tibble.

- `is_refrow()`: test function to detect cells in reference rows (at
  `fmt` level or `tab` level)

- `as_refrow()`: set the "in_refrow" field (belong to reference row)

- `get_digits()`: get the "digits" field

- `get_pvalue()`: get the per-cell p-value (what the significance stars
  read)

- `set_digits()`: set the "digits" field

- `set_pvalue()`: set the per-cell p-value. `set_pvalue(x, NA_real_)` is
  how a duplicated, purely descriptive copy of a column loses its stars:
  the stored p-value is their only source.

## See also

[`fmt()`](https://bricenocenti.github.io/tabxplor/reference/fmt.md) for
what every field means and how to build a cell;
[fmt_attributes](https://bricenocenti.github.io/tabxplor/reference/fmt_attributes.md)
for the per-column facts;
[`vignette("tabxplor-programming")`](https://bricenocenti.github.io/tabxplor/articles/tabxplor-programming.md).
