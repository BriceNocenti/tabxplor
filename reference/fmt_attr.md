# Read or write one `fmt` column attribute, by name

The generic form of the `get_*()` / `set_*()` family: one function
covering every per-column attribute a `tabxplor_fmt` vector carries, so
a helper can loop over them instead of naming each. The named accessors
([`get_scale()`](https://bricenocenti.github.io/tabxplor/reference/fmt_attributes.md),
[`get_col_var()`](https://bricenocenti.github.io/tabxplor/reference/fmt_attributes.md),
[`is_totcol()`](https://bricenocenti.github.io/tabxplor/reference/fmt_attributes.md),
…) remain the readable way to address one known attribute.

## Usage

``` r
fmt_attr(x, name)

fmt_attr(x, name) <- value
```

## Arguments

- x:

  A `tabxplor_fmt` vector, or a data.frame (then every `fmt` column is
  read).

- name:

  The attribute: one of `"scale"`, `"comp_all"`, `"ref"`, `"pct_type"`,
  `"col_var"`, `"col_group"`, `"totcol"`, `"refcol"`, `"color"`,
  `"color_signif"`, `"model_family"`, `"role"`, `"conf_level"`,
  `"degf"`, `"basis"`, `"ci_method"`. An unknown name is an error naming
  the set.

- value:

  The new value. Written through the attribute's own setter, so it is
  validated exactly as
  [`set_scale()`](https://bricenocenti.github.io/tabxplor/reference/fmt_attributes.md)
  and friends validate it.

## Value

The stored value (its declared default when the attribute is unset). On
a data.frame, one entry per `fmt` column, named. Writing returns the
modified vector.

## See also

[`tab_columns()`](https://bricenocenti.github.io/tabxplor/reference/tab_columns.md)
for every column's attributes at once;
[`fmt()`](https://bricenocenti.github.io/tabxplor/reference/fmt.md) for
what each one means.

## Examples

``` r
x <- fmt(n = c(10, 20), pct = c(0.3, 0.7), scale = "level_pct", pct_type = "row")
fmt_attr(x, "scale")
#> [1] "level_pct"
fmt_attr(x, "col_var") <- "region"
fmt_attr(x, "col_var")
#> [1] "region"
```
