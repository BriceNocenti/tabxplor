# The estimates of a table, one row per (table row x value column)

The long model behind
[`forest_plot`](https://bricenocenti.github.io/tabxplor/reference/forest_plot.md):
every plotted number, its interval, its p-value, its scale and its
colour, read from the table with the same accessors the printed table
used. Nothing is computed and no model is re-fitted, so it agrees with
what the table shows by construction. Reachable as
`forest_plot(x, return_data = TRUE)`.

## Usage

``` r
tab_estimates(
  x,
  columns = NULL,
  what = c("auto", "effect", "level"),
  observed = c("auto", "band", "point", "ci", "none"),
  intercept = FALSE,
  totals = FALSE,
  theme = NULL
)
```

## Arguments

- x:

  A table from
  [`tab`](https://bricenocenti.github.io/tabxplor/reference/tab.md) or
  [`tab_reg`](https://bricenocenti.github.io/tabxplor/reference/tab_reg.md).

- columns:

  Value columns to keep, by name. `NULL` (the default) keeps the model
  columns of a regression table and every value column of a cross-table.

- what:

  `"auto"` (the quantity the stored interval is centred on – so a
  `ci = "cell"` table gives percentages, a `ci = "ref"` table
  differences, an odds-ratio table odds ratios), `"effect"` or
  `"level"`.

- observed:

  `"auto"`, `"band"`, `"point"`, `"ci"` or `"none"` – whether the
  observed (crude) counterpart of a regression estimate is included.

- intercept:

  Keep the regression `Constant` row.

- totals:

  Keep total rows and total columns.

- theme:

  Palette theme for the colour columns (`"light"` / `"dark"` / a
  publication palette; `NULL` follows
  `getOption("tabxplor.export_theme")`).

## Value

A tibble with one row per plotted cell.
