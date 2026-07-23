# Package index

## The two main functions

Start here.
[`tab()`](https://bricenocenti.github.io/tabxplor/reference/tab.md)
builds a colour-coded cross-table;
[`tab_reg()`](https://bricenocenti.github.io/tabxplor/reference/tab_reg.md)
a regression table that looks and behaves like one.

- [`tab()`](https://bricenocenti.github.io/tabxplor/reference/tab.md) :
  Cross-table with color helpers
- [`tab_reg()`](https://bricenocenti.github.io/tabxplor/reference/tab_reg.md)
  : Regression table (effect measures) as a tabxplor table

## Variants of tab()

The same idea as
[`tab()`](https://bricenocenti.github.io/tabxplor/reference/tab.md), for
a particular input:
[`tab_num()`](https://bricenocenti.github.io/tabxplor/reference/tab_num.md)
for numeric variables,
[`tab_counts()`](https://bricenocenti.github.io/tabxplor/reference/tab_counts.md)
for pre-aggregated counts,
[`tab_plain()`](https://bricenocenti.github.io/tabxplor/reference/tab_plain.md)
for a single bare cross-table.

- [`tab_num()`](https://bricenocenti.github.io/tabxplor/reference/tab_num.md)
  : Means table
- [`tab_counts()`](https://bricenocenti.github.io/tabxplor/reference/tab_counts.md)
  : Cross-table from already-aggregated counts ("from the middle")
- [`tab_plain()`](https://bricenocenti.github.io/tabxplor/reference/tab_plain.md)
  : Plain single cross-table

## Regression shortcuts and plots

[`tab_logit()`](https://bricenocenti.github.io/tabxplor/reference/tab_logit.md)
and
[`multi_logit()`](https://bricenocenti.github.io/tabxplor/reference/multi_logit.md)
are thin wrappers around
[`tab_reg()`](https://bricenocenti.github.io/tabxplor/reference/tab_reg.md)
for logistic models.
[`or_plot()`](https://bricenocenti.github.io/tabxplor/reference/or_plot.md)
and
[`lm_plots()`](https://bricenocenti.github.io/tabxplor/reference/lm_plots.md)
draw plots from a finished regression table.

- [`tab_logit()`](https://bricenocenti.github.io/tabxplor/reference/tab_logit.md)
  : Logistic-regression table (odds ratios)
- [`multi_logit()`](https://bricenocenti.github.io/tabxplor/reference/multi_logit.md)
  : Compare several logistic-regression models (odds ratios side by
  side)
- [`or_plot()`](https://bricenocenti.github.io/tabxplor/reference/or_plot.md)
  **\[experimental\]** : Odds-ratio forest plot of a tabxplor regression
  table
- [`lm_plots()`](https://bricenocenti.github.io/tabxplor/reference/lm_plots.md)
  **\[experimental\]** : Diagnostic plots for a linear /
  generalized-linear model

## Export a finished table

[`tab_export()`](https://bricenocenti.github.io/tabxplor/reference/tab_export.md)
is the one function most users need.
[`tab_html()`](https://bricenocenti.github.io/tabxplor/reference/tab_html.md),
[`tab_md()`](https://bricenocenti.github.io/tabxplor/reference/tab_md.md),
[`tab_xl()`](https://bricenocenti.github.io/tabxplor/reference/tab_xl.md)
and
[`tab_plot()`](https://bricenocenti.github.io/tabxplor/reference/tab_plot.md)
are the per-format exporters it wraps
([`tab_kable()`](https://bricenocenti.github.io/tabxplor/reference/tab_html.md)
is an alias of
[`tab_html()`](https://bricenocenti.github.io/tabxplor/reference/tab_html.md));
[`tab_css()`](https://bricenocenti.github.io/tabxplor/reference/tab_css.md)
/
[`tab_md_css()`](https://bricenocenti.github.io/tabxplor/reference/tab_md_css.md)
emit the colour stylesheet for a whole document.

- [`tab_export()`](https://bricenocenti.github.io/tabxplor/reference/tab_export.md)
  : Export a tabxplor table to Excel, HTML, Markdown, or a plot

- [`tab_html()`](https://bricenocenti.github.io/tabxplor/reference/tab_html.md)
  [`tab_kable()`](https://bricenocenti.github.io/tabxplor/reference/tab_html.md)
  : Print a tabxplor table in html

- [`tab_md()`](https://bricenocenti.github.io/tabxplor/reference/tab_md.md)
  : Export a tabxplor table to a markdown table

- [`tab_xl()`](https://bricenocenti.github.io/tabxplor/reference/tab_xl.md)
  : Excel output for tabxplor tables, with formatting and colors

- [`tab_plot()`](https://bricenocenti.github.io/tabxplor/reference/tab_plot.md)
  **\[superseded\]** : Print a tabxplor table as plot

- [`tab_css()`](https://bricenocenti.github.io/tabxplor/reference/tab_css.md)
  : Generate the tabxplor stylesheet

- [`tab_md_css()`](https://bricenocenti.github.io/tabxplor/reference/tab_md_css.md)
  :

  CSS for the colour spans of `tab_md`

## Captions, options and data

Colour scales and palettes are customised with
[`set_color_breaks()`](https://bricenocenti.github.io/tabxplor/reference/tab_many.md),
[`set_color_palette()`](https://bricenocenti.github.io/tabxplor/reference/tab_many.md)
and
[`set_color_style()`](https://bricenocenti.github.io/tabxplor/reference/tab_many.md)
(documented on the
[`tab_many()`](https://bricenocenti.github.io/tabxplor/reference/tab_many.md)
page).

- [`set_caption()`](https://bricenocenti.github.io/tabxplor/reference/set_caption.md)
  [`get_caption()`](https://bricenocenti.github.io/tabxplor/reference/set_caption.md)
  : Store a caption on a tabxplor table

- [`tabxplor-options`](https://bricenocenti.github.io/tabxplor/reference/tabxplor-options.md)
  [`tabxplor.options`](https://bricenocenti.github.io/tabxplor/reference/tabxplor-options.md)
  : tabxplor global options

- [`gss_cat_data_formatting()`](https://bricenocenti.github.io/tabxplor/reference/gss_cat_data_formatting.md)
  :

  [`forcats::gss_cat`](https://forcats.tidyverse.org/reference/gss_cat.html)
  test dataframe, from US General Social Survey, but formatted with
  merged levels for cleaner tables, and first levels chosen to be used
  as references (for color helpers, regressions, etc.)

## Point-and-click interface (jamovi)

Everything tabxplor does, without writing R code. Install the free
open-source [jamovi](https://www.jamovi.org/download.html), open the
modules menu (the `+` at the top-right), choose *jamovi library*, and
install **tabxplor**: it adds a *Crosstables* and a *Regressions*
analysis, driven entirely by menus.

- [`jmvtab()`](https://bricenocenti.github.io/tabxplor/reference/jmvtab.md)
  : Crosstables
- [`jmvtabreg()`](https://bricenocenti.github.io/tabxplor/reference/jmvtabreg.md)
  : Regressions

## The engine, reshaping and combining

[`tab_many()`](https://bricenocenti.github.io/tabxplor/reference/tab_many.md)
is the vectorised engine behind
[`tab()`](https://bricenocenti.github.io/tabxplor/reference/tab.md) (and
the home of the colour helpers
[`set_color_breaks()`](https://bricenocenti.github.io/tabxplor/reference/tab_many.md)
/
[`set_color_palette()`](https://bricenocenti.github.io/tabxplor/reference/tab_many.md)
/
[`set_color_style()`](https://bricenocenti.github.io/tabxplor/reference/tab_many.md)).
The rest reshape and combine finished tables.

- [`tab_many()`](https://bricenocenti.github.io/tabxplor/reference/tab_many.md)
  [`tab_get_vars()`](https://bricenocenti.github.io/tabxplor/reference/tab_many.md)
  [`is_tab()`](https://bricenocenti.github.io/tabxplor/reference/tab_many.md)
  [`set_color_palette()`](https://bricenocenti.github.io/tabxplor/reference/tab_many.md)
  [`set_color_style()`](https://bricenocenti.github.io/tabxplor/reference/tab_many.md)
  [`get_color_style()`](https://bricenocenti.github.io/tabxplor/reference/tab_many.md)
  [`set_color_breaks()`](https://bricenocenti.github.io/tabxplor/reference/tab_many.md)
  [`get_color_breaks()`](https://bricenocenti.github.io/tabxplor/reference/tab_many.md)
  **\[superseded\]** : Many cross-tables as one, with color helpers
- [`tab_spread()`](https://bricenocenti.github.io/tabxplor/reference/tab_spread.md)
  : Spread a tab, passing a tab variable to column
- [`tab_compact()`](https://bricenocenti.github.io/tabxplor/reference/tab_compact.md)
  : Bind a list of tabs with the same col_vars (and no tab_vars) into a
  single tab
- [`tab_transpose()`](https://bricenocenti.github.io/tabxplor/reference/tab_transpose.md)
  **\[deprecated\]** : Transpose a cross-table (swap its rows and
  columns)
- [`complete_partial_totals()`](https://bricenocenti.github.io/tabxplor/reference/complete_partial_totals.md)
  : Complete partial total rows

## The fmt cell type

The `tabxplor_fmt` vctrs record behind every numeric cell, and its
constructors.

- [`fmt()`](https://bricenocenti.github.io/tabxplor/reference/fmt.md)
  [`is_fmt()`](https://bricenocenti.github.io/tabxplor/reference/fmt.md)
  [`get_num()`](https://bricenocenti.github.io/tabxplor/reference/fmt.md)
  [`set_num()`](https://bricenocenti.github.io/tabxplor/reference/fmt.md)
  [`get_type()`](https://bricenocenti.github.io/tabxplor/reference/fmt.md)
  [`set_type()`](https://bricenocenti.github.io/tabxplor/reference/fmt.md)
  [`is_totrow()`](https://bricenocenti.github.io/tabxplor/reference/fmt.md)
  [`as_totrow()`](https://bricenocenti.github.io/tabxplor/reference/fmt.md)
  [`is_tottab()`](https://bricenocenti.github.io/tabxplor/reference/fmt.md)
  [`as_tottab()`](https://bricenocenti.github.io/tabxplor/reference/fmt.md)
  [`set_display()`](https://bricenocenti.github.io/tabxplor/reference/fmt.md)
  [`is_totcol()`](https://bricenocenti.github.io/tabxplor/reference/fmt.md)
  [`as_totcol()`](https://bricenocenti.github.io/tabxplor/reference/fmt.md)
  [`is_refrow()`](https://bricenocenti.github.io/tabxplor/reference/fmt.md)
  [`as_refrow()`](https://bricenocenti.github.io/tabxplor/reference/fmt.md)
  [`get_comp_all()`](https://bricenocenti.github.io/tabxplor/reference/fmt.md)
  [`set_comp_all()`](https://bricenocenti.github.io/tabxplor/reference/fmt.md)
  [`get_ref_type()`](https://bricenocenti.github.io/tabxplor/reference/fmt.md)
  [`set_diff_type()`](https://bricenocenti.github.io/tabxplor/reference/fmt.md)
  [`get_ci_type()`](https://bricenocenti.github.io/tabxplor/reference/fmt.md)
  [`set_ci_type()`](https://bricenocenti.github.io/tabxplor/reference/fmt.md)
  [`get_col_var()`](https://bricenocenti.github.io/tabxplor/reference/fmt.md)
  [`set_col_var()`](https://bricenocenti.github.io/tabxplor/reference/fmt.md)
  [`get_model_family()`](https://bricenocenti.github.io/tabxplor/reference/fmt.md)
  [`set_model_family()`](https://bricenocenti.github.io/tabxplor/reference/fmt.md)
  [`is_refcol()`](https://bricenocenti.github.io/tabxplor/reference/fmt.md)
  [`as_refcol()`](https://bricenocenti.github.io/tabxplor/reference/fmt.md)
  [`get_color()`](https://bricenocenti.github.io/tabxplor/reference/fmt.md)
  [`get_color_bg()`](https://bricenocenti.github.io/tabxplor/reference/fmt.md)
  [`get_color_signif()`](https://bricenocenti.github.io/tabxplor/reference/fmt.md)
  [`set_color()`](https://bricenocenti.github.io/tabxplor/reference/fmt.md)
  [`set_color_signif()`](https://bricenocenti.github.io/tabxplor/reference/fmt.md)
  [`get_digits()`](https://bricenocenti.github.io/tabxplor/reference/fmt.md)
  [`set_digits()`](https://bricenocenti.github.io/tabxplor/reference/fmt.md)
  : Create a vector of class formatted numbers
- [`new_tab()`](https://bricenocenti.github.io/tabxplor/reference/new_tab.md)
  [`new_grouped_tab()`](https://bricenocenti.github.io/tabxplor/reference/new_tab.md)
  : A constructor for class tabxplor_tab
- [`fmt_get_color_code()`](https://bricenocenti.github.io/tabxplor/reference/fmt_get_color_code.md)
  : Get HTML Color Code of a fmt vector

## The step-by-step pipeline (superseded)

The individual steps
[`tab_many()`](https://bricenocenti.github.io/tabxplor/reference/tab_many.md)
now fuses into one. Each is still exported and usable on its own, but
[`tab()`](https://bricenocenti.github.io/tabxplor/reference/tab.md) /
[`tab_num()`](https://bricenocenti.github.io/tabxplor/reference/tab_num.md)
supersede them.

- [`tab_prepare()`](https://bricenocenti.github.io/tabxplor/reference/tab_prepare.md)
  :

  Prepare data for `tab_plain`.

- [`tab_pct()`](https://bricenocenti.github.io/tabxplor/reference/tab_pct.md)
  **\[superseded\]** :

  Add percentages and diffs to a
  [`tab`](https://rdrr.io/pkg/tabxplor/man/tab.html)

- [`tab_tot()`](https://bricenocenti.github.io/tabxplor/reference/tab_tot.md)
  **\[superseded\]** :

  Add totals to a [`tab`](https://rdrr.io/pkg/tabxplor/man/tab.html)

- [`tab_totaltab()`](https://bricenocenti.github.io/tabxplor/reference/tab_totaltab.md)
  **\[superseded\]** :

  Add total table to a
  [`tab`](https://rdrr.io/pkg/tabxplor/man/tab.html)

- [`tab_ci()`](https://bricenocenti.github.io/tabxplor/reference/tab_ci.md)
  :

  Add confidence intervals to a
  [`tab`](https://rdrr.io/pkg/tabxplor/man/tab.html)

- [`tab_chi2()`](https://bricenocenti.github.io/tabxplor/reference/tab_chi2.md)
  :

  Add Chi2 summaries to a
  [`tab`](https://rdrr.io/pkg/tabxplor/man/tab.html)

## Data-prep and text helpers

Small utilities for preparing data or laying out a table.

- [`score_from_lv1()`](https://bricenocenti.github.io/tabxplor/reference/score_from_lv1.md)
  : Create a score variable by counting factors at their first level

- [`fct_recode_helper()`](https://bricenocenti.github.io/tabxplor/reference/fct_recode_helper.md)
  : fct_recode helper to recode multiple variables

- [`tab_wrap_text()`](https://bricenocenti.github.io/tabxplor/reference/tab_wrap_text.md)
  : Wrap column names and character/factor variables.

- [`tab_get_wrapped_dimensions()`](https://bricenocenti.github.io/tabxplor/reference/tab_get_wrapped_dimensions.md)
  :

  Get the number of actual rows and the max character length of a table
  after being wrapped (count `\n` as a linebreak).

- [`tab_parallel_stop()`](https://bricenocenti.github.io/tabxplor/reference/tab_parallel_stop.md)
  : Stop the tabxplor parallel worker pool

- [`kable_tabxplor_style()`](https://bricenocenti.github.io/tabxplor/reference/kable_tabxplor_style.md)
  **\[deprecated\]** : Print a tabxplor table in html
