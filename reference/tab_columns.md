# Every `fmt` column of a table, and what it carries

**\[experimental\]**

One row per numeric (`tabxplor_fmt`) column, with the per-column
attributes that decide what it shows, what it estimates, how it is
coloured and how its confidence interval was computed. The column-axis
companion of
[`tab_structure()`](https://bricenocenti.github.io/tabxplor/reference/tab_structure.md),
which describes the table as a whole.

## Usage

``` r
tab_columns(x)
```

## Arguments

- x:

  A `tabxplor_tab` / `tabxplor_grouped_tab`, or any data.frame holding
  `fmt` columns.

## Value

A tibble, one row per `fmt` column:

- `column`:

  the column name.

- `col_var`, `col_group`:

  the column variable, and the sub-population its block belongs to (`""`
  when the table was never spread).

- `scale`, `pct_type`:

  what the column estimates, and on which percentage base.

- `display`:

  the display template(s) its cells carry.

- `ref`, `comp_all`, `totcol`, `refcol`:

  the comparison model: which baseline, whether it compares across
  sub-tables, and whether this column is a total or the reference.

- `color`, `color_bg`, `color_signif`:

  the colour measure of each channel and the significance policy.

- `conf_level`, `degf`, `basis`, `ci_method`:

  how this column's interval was computed — the level, the degrees of
  freedom it is referred to (`NA` = the normal quantile), whether it
  rests on the raw count, the weights or the survey design, and by which
  method.

- `model_family`, `role`:

  for a
  [`tab_reg()`](https://bricenocenti.github.io/tabxplor/reference/tab_reg.md)
  table: the column's model family, and whether it holds the model
  estimate (`"model"`) or its observed counterpart (`"emp"`).

## See also

[`tab_structure()`](https://bricenocenti.github.io/tabxplor/reference/tab_structure.md)
for the table's own structure;
[`fmt_attr()`](https://bricenocenti.github.io/tabxplor/reference/fmt_attr.md)
to read or write one attribute;
[`fmt()`](https://bricenocenti.github.io/tabxplor/reference/fmt.md) for
what each attribute means.

## Examples

``` r
# \donttest{
t <- tab(forcats::gss_cat, marital, race, pct = "row", ci = "ref")
tab_columns(t)
#> # A tibble: 4 × 19
#>   column col_var col_group scale  pct_type display ref   comp_all totcol refcol
#>   <chr>  <chr>   <chr>     <chr>  <chr>    <chr>   <chr> <lgl>    <lgl>  <lgl> 
#> 1 Other  race    ""        points row      pct     tot   FALSE    FALSE  FALSE 
#> 2 Black  race    ""        points row      pct     tot   FALSE    FALSE  FALSE 
#> 3 White  race    ""        points row      pct     tot   FALSE    FALSE  FALSE 
#> 4 Total  race    ""        points row      pct     tot   FALSE    TRUE   FALSE 
#> # ℹ 9 more variables: color <chr>, color_bg <chr>, color_signif <chr>,
#> #   conf_level <dbl>, degf <dbl>, basis <chr>, ci_method <chr>,
#> #   model_family <chr>, role <chr>
# }
```
