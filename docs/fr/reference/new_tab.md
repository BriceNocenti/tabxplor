# A constructor for class tabxplor_tab

A constructor for class tabxplor_tab

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
  [`tab`](https://bricenocenti.github.io/tabxplor/fr/reference/tab.md),
  [`tab_many`](https://bricenocenti.github.io/tabxplor/fr/reference/tab_many.md)
  or
  [`tab_plain`](https://bricenocenti.github.io/tabxplor/fr/reference/tab_plain.md).

- subtext:

  A character vector to print legend lines under the table.

- test:

  A tidy tibble storing whole-table test results (Chi2 for factor
  columns, ANOVA F for mean columns), filled by
  [`tab_chi2`](https://bricenocenti.github.io/tabxplor/fr/reference/tab_chi2.md).
  Renamed from `chi2` in tabxplor 1.4.0.

- chi2:

  **\[deprecated\]** Soft-deprecated alias of `test`.

- meta:

  The table's metadata, as a single named list gathering (all optional,
  `NULL` when unset):

  - `render_extras` – display-only intent for the `add_n` / `add_pct`
    extras, `list(add_n =, add_pct =)`. Since tabxplor 1.4.0 those
    rows/columns are materialised at print/export time from this
    attribute rather than baked into the table.

  - `ci_settings` – display-only metadata for the colour legend,
    `list(conf_level =, method_cell =, method_diff =, ...)`: which
    confidence level and confidence-interval methods were actually used.
    Absent makes the legend fall back to defaults.

  - `vars` – the table's variable roles,
    `list(row_vars =, col_vars =, tab_vars =, compacted =, wt =, caption =)`,
    recorded at build rather than guessed back afterwards (see
    [`set_caption`](https://bricenocenti.github.io/tabxplor/fr/reference/set_caption.md)
    for `caption`).

  - `empirical_tips` – multinomial crude-companion tooltip data (a
    `tibble` keyed by column, predictor and level), set by
    `tab_reg(empirical = TRUE)`.

  - `reg_meta` – a regression table's model record (family, effect,
    dependent, reference level, predictors, ...), set by
    [`tab_reg`](https://bricenocenti.github.io/tabxplor/fr/reference/tab_reg.md);
    drives the reg title/caption, the "Model:" legend line and the
    colour-legend wording.

  - `color_breaks` – a per-table override of the colour break scales
    (see
    [`set_color_breaks`](https://bricenocenti.github.io/tabxplor/fr/reference/tab_many.md)),
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
