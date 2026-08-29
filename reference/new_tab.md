# Build a `tabxplor_tab` around `fmt` columns

Build a `tabxplor_tab` around `fmt` columns

## Usage

``` r
new_tab(
  tabs = tibble::tibble(),
  subtext = "",
  test = new_test_tibble(),
  chi2 = NULL,
  meta = NULL,
  ...,
  class = character()
)

new_grouped_tab(
  tabs = tibble::tibble(),
  groups,
  subtext = "",
  test = new_test_tibble(),
  chi2 = NULL,
  meta = NULL,
  ...,
  class = character()
)
```

## Arguments

- tabs:

  A table, stored into a
  [`tibble`](https://tibble.tidyverse.org/reference/tibble.html)
  data.frame. It is generally made with
  [`tab`](https://bricenocenti.github.io/tabxplor/reference/tab.md),
  [`tab_reg`](https://bricenocenti.github.io/tabxplor/reference/tab_reg.md)
  or
  [`tab_plain`](https://bricenocenti.github.io/tabxplor/reference/tab_plain.md).

- subtext:

  A character vector to print legend lines under the table.

- test:

  A tidy tibble storing whole-table test results (Chi2 for factor
  columns, ANOVA F for mean columns), filled by
  [`tab_chi2`](https://bricenocenti.github.io/tabxplor/reference/tab_chi2.md).

- chi2:

  **\[deprecated\]** Soft-deprecated alias of `test`.

- meta:

  The table's metadata, as a single named list gathering (all optional,
  `NULL` when unset):

  - `render_extras` – display-only intent for the base count and the
    `add_pct` companion, `list(n =, add_pct =)`, materialised at
    print/export time from this attribute rather than baked into the
    table.

  - `spec` – the table's identity, `list(kind =, vars =, call =)`: its
    `kind` (`"crosstab"` or `"regression"`); `vars`, what no column can
    carry (`list(wt =, caption =, outcomes =, var_labels =)` – see
    [`set_caption`](https://bricenocenti.github.io/tabxplor/reference/set_caption.md)),
    the rest of the variable model being derived from the declared index
    columns and from the columns' own `col_var`; and `call`, the
    producer's own recipe (a regression's model record – family,
    outcome, predictors, reference level, and the `fit_spec`
    [`reg_check_plots`](https://bricenocenti.github.io/tabxplor/reference/reg_check_plots.md)
    refits from).

  - `empirical_tips` – multinomial crude-companion tooltip data (a
    `tibble` keyed by column, predictor and level), set by
    `tab_reg(empirical = TRUE)`.

  - `assumptions` – one record PER OUTCOME, keyed by it, each holding
    the observed curve of every continuous predictor (weighted quantile
    bins of the outcome on the family's link scale, one block per
    `tab_vars` group), set by
    [`tab_reg`](https://bricenocenti.github.io/tabxplor/reference/tab_reg.md):
    the data behind the sparkline – drawn in a continuous predictor's
    `n` cell, or in the shape table below the footer – and behind
    [`reg_check_plots`](https://bricenocenti.github.io/tabxplor/reference/reg_check_plots.md)'s
    linearity panel.

  - `color_breaks` – a per-table override of the colour break scales
    (see
    [`set_color_breaks`](https://bricenocenti.github.io/tabxplor/reference/set_color_palette.md)),
    merged over the global option at render time.

  `meta` sub-fields left `NULL` are dropped, so a table given nothing
  carries no attribute.

- ...:

  Needed to implement subclasses.

- class:

  Needed to implement subclasses.

- groups:

  The grouping data.

## Value

A `tibble` of class `tabxplor_tab`.

A `tibble` of class `tabxplor_grouped_tab`.
