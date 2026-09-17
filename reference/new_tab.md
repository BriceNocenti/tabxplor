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

  The footer's text, as a template: one element per line, every
  `<placeholder>` tabxplor generates and every line you write, in the
  order they print. See
  [`set_subtext`](https://bricenocenti.github.io/tabxplor/reference/set_subtext.md).

- test:

  A tidy tibble storing whole-table test results (Chi2 for factor
  columns, ANOVA F for mean columns), filled by
  [`tab_chi2`](https://bricenocenti.github.io/tabxplor/reference/tab_chi2.md).

- chi2:

  **\[deprecated\]** Soft-deprecated alias of `test`.

- meta:

  The table's metadata, as a single named list gathering (all optional,
  `NULL` when unset):

  - `spec` — the table's identity, `list(kind =, vars =, call =)`: its
    `kind` (`"crosstab"` or `"regression"`); `vars`, what no column can
    carry (the weight, the caption, the outcomes, the variable labels —
    see
    [`set_caption`](https://bricenocenti.github.io/tabxplor/reference/set_caption.md)),
    the rest of the variable model being derived from the declared index
    columns and from the columns' own `col_var`; and `call`, the
    producer's own recipe (a regression's model record — family,
    outcome, predictors, reference level, and the `fit_spec`
    [`reg_check_plots`](https://bricenocenti.github.io/tabxplor/reference/reg_check_plots.md)
    refits from).

  - `render_extras` — display-only intent for the base count and the
    `add_pct` companion, `list(n =, add_pct =)`, materialised at
    print/export time rather than baked into the table.

  - `empirical_tips` — multinomial crude-companion tooltip data (a
    `tibble` keyed by column, predictor and level), set by
    `tab_reg(empirical = TRUE)`.

  - `assumptions` — one record PER OUTCOME, keyed by it, each holding
    the observed curve of every continuous predictor (weighted quantile
    bins of the outcome on the family's link scale, one block per
    `tab_vars` group), set by
    [`tab_reg`](https://bricenocenti.github.io/tabxplor/reference/tab_reg.md):
    the data behind the shape table under the footer, and behind
    [`reg_check_plots`](https://bricenocenti.github.io/tabxplor/reference/reg_check_plots.md)'s
    linearity panel.

  - `color_breaks` — a per-table override of the colour break scales,
    set by `tab(color_breaks =)` and merged over the global option
    ([`set_color_breaks`](https://bricenocenti.github.io/tabxplor/reference/set_color_palette.md))
    at render time.

  - `legend_words` — what this table's colour legend CALLS each measure
    — naming only, never a number (see
    [`set_legend_words`](https://bricenocenti.github.io/tabxplor/reference/set_legend_words.md)).

  - `footer_tabs` — the tables and notes rendered UNDER this one by
    every medium, set by
    [`set_footer_tabs`](https://bricenocenti.github.io/tabxplor/reference/set_footer_tabs.md):
    a `tabxplor_tab` renders as a table, any other data.frame as a grey
    note
    ([`tab_note`](https://bricenocenti.github.io/tabxplor/reference/tab_note.md)).
    In the console they print ABOVE the table, so the last thing printed
    is the object you can go on to pipe. A footer table's own are never
    rendered.

  - `bars` — the columns drawn as data bars in html and in Excel, each
    named with the ceiling a full bar means (`NA` = the column's own
    largest data cell, so that one reference serves the whole column —
    [`set_bars`](https://bricenocenti.github.io/tabxplor/reference/set_bars.md)).

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
