# Package index

## Main functions

- [`tab()`](https://bricenocenti.github.io/tabxplor/reference/tab.md) :
  Cross-tables with color helpers
- [`tab_reg()`](https://bricenocenti.github.io/tabxplor/reference/tab_reg.md)
  : All-in-one tables for regressions, with each modelled effect beside
  its observed one
- [`tab_counts()`](https://bricenocenti.github.io/tabxplor/reference/tab_counts.md)
  : Cross-tables from already-aggregated counts

## Point-and-click interface (jamovi)

The two analyses tabxplor adds to [jamovi](https://www.jamovi.org/) –
everything the package does, driven by menus. Install jamovi, open the
modules menu (the `+` at the top-right), choose *jamovi library*, and
install **tabxplor**.

- [`jmvtab()`](https://bricenocenti.github.io/tabxplor/reference/jmvtab.md)
  : Crosstables
- [`jmvtabreg()`](https://bricenocenti.github.io/tabxplor/reference/jmvtabreg.md)
  : Regressions

## Reshaping and combining

Change the shape of a finished table; nothing is recomputed.

- [`tab_spread()`](https://bricenocenti.github.io/tabxplor/reference/tab_spread.md)
  : Turn a sub-table variable into columns
- [`tab_compact()`](https://bricenocenti.github.io/tabxplor/reference/tab_compact.md)
  : Bind a list of tables into one
- [`tab_transpose()`](https://bricenocenti.github.io/tabxplor/reference/tab_transpose.md)
  : Swap the rows and columns of a cross-table

## Export a finished table

- [`tab_export()`](https://bricenocenti.github.io/tabxplor/reference/tab_export.md)
  : Export a table to html, Excel or Markdown (wrapper)
- [`tab_html()`](https://bricenocenti.github.io/tabxplor/reference/tab_html.md)
  [`tab_kable()`](https://bricenocenti.github.io/tabxplor/reference/tab_html.md)
  : Render a table as html
- [`tab_md()`](https://bricenocenti.github.io/tabxplor/reference/tab_md.md)
  : Render a table as Markdown
- [`tab_xl()`](https://bricenocenti.github.io/tabxplor/reference/tab_xl.md)
  : Write a table to an Excel workbook
- [`tab_css()`](https://bricenocenti.github.io/tabxplor/reference/tab_css.md)
  : The stylesheet an html table needs

## Charts

(Both need **ggplot2**.)

- [`forest_plot()`](https://bricenocenti.github.io/tabxplor/reference/forest_plot.md)
  : Forest plot of any tabxplor table
- [`reg_check_plots()`](https://bricenocenti.github.io/tabxplor/reference/reg_check_plots.md)
  : Diagnostic plots of a regression model

## Data-prep and text helpers

Small utilities for preparing data or laying out a table.

- [`score_from_lv1()`](https://bricenocenti.github.io/tabxplor/reference/score_from_lv1.md)
  : Score a set of factors by counting their first level
- [`shape_numeric_var()`](https://bricenocenti.github.io/tabxplor/reference/shape_numeric_var.md)
  : Cut or transform a numeric variable
- [`tab_wrap_text()`](https://bricenocenti.github.io/tabxplor/reference/tab_wrap_text.md)
  : Wrap column names and long labels
- [`tab_parallel_stop()`](https://bricenocenti.github.io/tabxplor/reference/tab_parallel_stop.md)
  : Stop the tabxplor parallel worker pool

## Superseded entry points

They still work and give the same numbers;
[`tab()`](https://bricenocenti.github.io/tabxplor/reference/tab.md) has
absorbed all three, and `scale` + `pct_type` have absorbed the fourth.

- [`tab_many()`](https://bricenocenti.github.io/tabxplor/reference/tab_many.md)
  **\[superseded\]** : Many cross-tables as one, with color helpers
- [`tab_num()`](https://bricenocenti.github.io/tabxplor/reference/tab_num.md)
  **\[superseded\]** : Means table
- [`tab_plain()`](https://bricenocenti.github.io/tabxplor/reference/tab_plain.md)
  **\[superseded\]** : Plain single cross-table
- [`set_type()`](https://bricenocenti.github.io/tabxplor/reference/tabxplor-type.md)
  [`get_type()`](https://bricenocenti.github.io/tabxplor/reference/tabxplor-type.md)
  **\[superseded\]** : Column types, the tabxplor 1.x spelling

## What a cell shows, and how it is coloured

The vocabulary of `display =`, then of `color =`.

- [`tabxplor-display`](https://bricenocenti.github.io/tabxplor/reference/tabxplor-display.md)
  : What a table cell shows: the display grammar
- [`set_color_palette()`](https://bricenocenti.github.io/tabxplor/reference/set_color_palette.md)
  [`set_color_style()`](https://bricenocenti.github.io/tabxplor/reference/set_color_palette.md)
  [`get_color_style()`](https://bricenocenti.github.io/tabxplor/reference/set_color_palette.md)
  [`set_color_breaks()`](https://bricenocenti.github.io/tabxplor/reference/set_color_palette.md)
  [`get_color_breaks()`](https://bricenocenti.github.io/tabxplor/reference/set_color_palette.md)
  : Colours: palettes, styles and breaks
- [`conf_level_to_z()`](https://bricenocenti.github.io/tabxplor/reference/conf_level_to_z.md)
  : Convert confidence levels into z thresholds

## What a model can be, and what it fitted

- [`reg_measures()`](https://bricenocenti.github.io/tabxplor/reference/reg_measures.md)
  : What can this outcome be modelled as?
- [`reg_formulas()`](https://bricenocenti.github.io/tabxplor/reference/reg_formulas.md)
  : The model formulas a regression table fitted

## Options

Every default lives in a `tabxplor.*` option.

- [`tabxplor-options`](https://bricenocenti.github.io/tabxplor/reference/tabxplor-options.md)
  [`tabxplor.options`](https://bricenocenti.github.io/tabxplor/reference/tabxplor-options.md)
  : The tabxplor options, and their defaults

## Inspect a table

What have I got? – the table as a whole, then column by column.

- [`tab_structure()`](https://bricenocenti.github.io/tabxplor/reference/tab_structure.md)
  **\[experimental\]** : The structure of a table

- [`tab_columns()`](https://bricenocenti.github.io/tabxplor/reference/tab_columns.md)
  **\[experimental\]** :

  Every `fmt` column of a table, and what it carries

- [`get_test()`](https://bricenocenti.github.io/tabxplor/reference/get_test.md)
  : Read a table's statistical tests

- [`is_tab()`](https://bricenocenti.github.io/tabxplor/reference/is_tab.md)
  : Is this a tabxplor table?

- [`as.matrix(`*`<tabxplor_tab>`*`)`](https://bricenocenti.github.io/tabxplor/reference/tabxplor-base-coercion.md)
  [`as.table(`*`<tabxplor_tab>`*`)`](https://bricenocenti.github.io/tabxplor/reference/tabxplor-base-coercion.md)
  : Hand a table to base R

## The type system

What a table and its cells are made of: the `tabxplor_fmt` vctrs record
behind every numeric cell, where a **field** varies from cell to cell
and an **attribute** holds for a whole column, and the attributes the
table itself carries.

- [`fmt()`](https://bricenocenti.github.io/tabxplor/reference/fmt.md)
  [`is_fmt()`](https://bricenocenti.github.io/tabxplor/reference/fmt.md)
  :

  Create an `fmt` vector, the tabxplor cell

- [`get_num()`](https://bricenocenti.github.io/tabxplor/reference/fmt_fields.md)
  [`set_num()`](https://bricenocenti.github.io/tabxplor/reference/fmt_fields.md)
  [`is_totrow()`](https://bricenocenti.github.io/tabxplor/reference/fmt_fields.md)
  [`get_row_kind()`](https://bricenocenti.github.io/tabxplor/reference/fmt_fields.md)
  [`set_row_kind()`](https://bricenocenti.github.io/tabxplor/reference/fmt_fields.md)
  [`as_totrow()`](https://bricenocenti.github.io/tabxplor/reference/fmt_fields.md)
  [`is_tottab()`](https://bricenocenti.github.io/tabxplor/reference/fmt_fields.md)
  [`as_tottab()`](https://bricenocenti.github.io/tabxplor/reference/fmt_fields.md)
  [`set_display()`](https://bricenocenti.github.io/tabxplor/reference/fmt_fields.md)
  [`is_refrow()`](https://bricenocenti.github.io/tabxplor/reference/fmt_fields.md)
  [`as_refrow()`](https://bricenocenti.github.io/tabxplor/reference/fmt_fields.md)
  [`get_digits()`](https://bricenocenti.github.io/tabxplor/reference/fmt_fields.md)
  [`get_pvalue()`](https://bricenocenti.github.io/tabxplor/reference/fmt_fields.md)
  [`set_digits()`](https://bricenocenti.github.io/tabxplor/reference/fmt_fields.md)
  [`set_pvalue()`](https://bricenocenti.github.io/tabxplor/reference/fmt_fields.md)
  :

  Per-cell fields of a `fmt` vector

- [`get_scale()`](https://bricenocenti.github.io/tabxplor/reference/fmt_attributes.md)
  [`set_scale()`](https://bricenocenti.github.io/tabxplor/reference/fmt_attributes.md)
  [`get_pct_type()`](https://bricenocenti.github.io/tabxplor/reference/fmt_attributes.md)
  [`set_pct_type()`](https://bricenocenti.github.io/tabxplor/reference/fmt_attributes.md)
  [`is_totcol()`](https://bricenocenti.github.io/tabxplor/reference/fmt_attributes.md)
  [`as_totcol()`](https://bricenocenti.github.io/tabxplor/reference/fmt_attributes.md)
  [`get_comp_all()`](https://bricenocenti.github.io/tabxplor/reference/fmt_attributes.md)
  [`set_comp_all()`](https://bricenocenti.github.io/tabxplor/reference/fmt_attributes.md)
  [`get_ref_type()`](https://bricenocenti.github.io/tabxplor/reference/fmt_attributes.md)
  [`set_ref_type()`](https://bricenocenti.github.io/tabxplor/reference/fmt_attributes.md)
  [`set_diff_type()`](https://bricenocenti.github.io/tabxplor/reference/fmt_attributes.md)
  [`get_col_var()`](https://bricenocenti.github.io/tabxplor/reference/fmt_attributes.md)
  [`set_col_var()`](https://bricenocenti.github.io/tabxplor/reference/fmt_attributes.md)
  [`get_col_group()`](https://bricenocenti.github.io/tabxplor/reference/fmt_attributes.md)
  [`get_model_family()`](https://bricenocenti.github.io/tabxplor/reference/fmt_attributes.md)
  [`set_model_family()`](https://bricenocenti.github.io/tabxplor/reference/fmt_attributes.md)
  [`get_ci_method()`](https://bricenocenti.github.io/tabxplor/reference/fmt_attributes.md)
  [`is_refcol()`](https://bricenocenti.github.io/tabxplor/reference/fmt_attributes.md)
  [`as_refcol()`](https://bricenocenti.github.io/tabxplor/reference/fmt_attributes.md)
  [`get_color()`](https://bricenocenti.github.io/tabxplor/reference/fmt_attributes.md)
  [`get_color_bg()`](https://bricenocenti.github.io/tabxplor/reference/fmt_attributes.md)
  [`get_color_signif()`](https://bricenocenti.github.io/tabxplor/reference/fmt_attributes.md)
  [`set_color()`](https://bricenocenti.github.io/tabxplor/reference/fmt_attributes.md)
  [`set_color_signif()`](https://bricenocenti.github.io/tabxplor/reference/fmt_attributes.md)
  :

  Per-column attributes of a `fmt` vector

- [`fmt_attr()`](https://bricenocenti.github.io/tabxplor/reference/fmt_attr.md)
  [`` `fmt_attr<-`() ``](https://bricenocenti.github.io/tabxplor/reference/fmt_attr.md)
  :

  Read or write one `fmt` column attribute, by name

- [`new_tab()`](https://bricenocenti.github.io/tabxplor/reference/new_tab.md)
  [`new_grouped_tab()`](https://bricenocenti.github.io/tabxplor/reference/new_tab.md)
  :

  Build a `tabxplor_tab` around `fmt` columns

- [`set_caption()`](https://bricenocenti.github.io/tabxplor/reference/set_caption.md)
  [`get_caption()`](https://bricenocenti.github.io/tabxplor/reference/set_caption.md)
  : Store a caption on a table

- [`fmt_get_color_code()`](https://bricenocenti.github.io/tabxplor/reference/fmt_get_color_code.md)
  :

  The html color code of a `fmt` vector
