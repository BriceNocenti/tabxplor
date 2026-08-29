# Column types, the tabxplor 1.x spelling

**\[superseded\]**

In tabxplor 1.x a `fmt` column carried one `type` attribute, whose seven
values conflated two facts. Since 2.0.0 they are two attributes:
[`get_scale()`](https://bricenocenti.github.io/tabxplor/reference/fmt_attributes.md)
/
[`set_scale()`](https://bricenocenti.github.io/tabxplor/reference/fmt_attributes.md)
say **what the column estimates** (a key into the declared scale table),
and
[`get_pct_type()`](https://bricenocenti.github.io/tabxplor/reference/fmt_attributes.md)
/
[`set_pct_type()`](https://bricenocenti.github.io/tabxplor/reference/fmt_attributes.md)
say **which kind of percentage** it holds. These two functions
translate, so 1.x code keeps running; they are defunct in tabxplor
2.1.0.

`get_type()` re-fuses what 2.0.0 split, so it is a reading aid rather
than an accessor: `level_mean` reads back `"mean"`, `level_n` reads
`"n"`, a level percentage reads its own `pct_type`, and every effect
scale (a difference, a ratio, an odds ratio, a coefficient) reads
`"coef"` — distinctions 1.x could not make are lost on the way back.

## Usage

``` r
set_type(x, type)

get_type(x, ...)
```

## Arguments

- x:

  A `fmt` vector, or a data frame of them.

- type:

  One of `"row"`, `"col"`, `"all"`, `"all_tabs"`, `"mean"`, `"n"`,
  `"coef"`.

- ...:

  Used in methods to add arguments in the future.

## Value

`get_type()` a character vector; `set_type()` a modified `fmt` vector.

## Functions

- `set_type()`: set the retired `type` attribute of a `fmt` vector

- `get_type()`: get the retired `type` of `fmt` columns

## See also

[`get_scale()`](https://bricenocenti.github.io/tabxplor/reference/fmt_attributes.md),
[`set_scale()`](https://bricenocenti.github.io/tabxplor/reference/fmt_attributes.md),
[`get_pct_type()`](https://bricenocenti.github.io/tabxplor/reference/fmt_attributes.md),
[`set_pct_type()`](https://bricenocenti.github.io/tabxplor/reference/fmt_attributes.md),
[`fmt_attr()`](https://bricenocenti.github.io/tabxplor/reference/fmt_attr.md).

## Examples

``` r
x <- fmt(n = c(10, 20), pct = c(0.3, 0.7), scale = "level_pct", pct_type = "row")
get_type(x)
#> [1] "row"
```
