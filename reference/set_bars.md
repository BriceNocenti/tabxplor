# Draw a column as data bars

Names the columns whose cells carry a horizontal bar behind their figure
— a bar chart inside the table, at no cost in width, in html and in
Excel. Total and footer rows take none.

**One reference per column**: every bar in a column is a share of the
same ceiling, or two bars could not be compared. That ceiling is the
column's own largest data cell by default, which is what spreads the
bars over the width available; `max` states it instead, so that two
tables can be read against each other.

It is a display intent, like a caption: the numbers are untouched, and a
medium with nowhere to put a bar (the console, a pipe table) ignores it.

## Usage

``` r
set_bars(x, cols, max = NULL)

get_bars(x)
```

## Arguments

- x:

  A `tabxplor_tab`.

- cols:

  Column names, or `NULL` to remove.

- max:

  The ceiling a full bar means, in the column's **stored** unit — a
  percentage is stored between 0 and 1, so `max = 1` means 100 %. `NULL`
  (the default) or `NA` takes the column's largest data cell. Unnamed
  values are recycled over `cols`, a named one applies to that column.

## Value

`x`, with its bar columns set (`set_bars`) ; a named vector of their
ceilings, `NA` where the column's largest is used, or `NULL`
(`get_bars`).

## See also

[`new_tab()`](https://bricenocenti.github.io/tabxplor/reference/new_tab.md)
for the whole `meta` record.

## Examples

``` r
t <- tab(forcats::gss_cat, race, marital, pct = "row")
t <- set_bars(t, "Married")
get_bars(t)
#> Married 
#>      NA 

# a full bar means 100 %, whatever the column holds
get_bars(set_bars(t, "Married", max = 1))
#> Married 
#>       1 
```
