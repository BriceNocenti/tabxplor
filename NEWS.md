
# tabxplor 2.0.0

## New features

* **`tab()` is now the unified entry point.** It accepts **several `row_vars` / `col_vars`**
  (e.g. `tab(data, c(race, relig), marital)`), merged into one table by default or returned as a list
  with `output_list = TRUE`. `tab_many()` is kept as a soft-deprecated alias.
* **Redesigned colour API.** Position picks the visual channel (1st value → text, 2nd → background),
  names pick the column type (`pct` / `mean`); `color = TRUE` is the smart per-type default. New OKLCH
  light/dark palettes, 24-bit truecolor console, `set_color_palette()` (replaces `set_color_style()`),
  and a per-table `color_breaks =` argument.
* **Dark mode.** `theme = "auto"` on `tab_html()` / `tab_md()` / `tab_css()` / `tab_export()` follows
  whoever is reading the table (their browser, or the editor for the Viewer). The console also
  auto-detects a dark editor (RStudio and Positron).
* **A new, dependency-free HTML engine, now the default** for `tab_html()` (about 3× faster and much
  lighter than kableExtra, which becomes optional). Its geometry is CSS classes, so your own CSS can
  restyle it.
* **`options(tabxplor.print = "html")`** — auto-print every table as its html version: in the Viewer
  pane in RStudio/Positron, and as a real colored table in rmarkdown/Quarto chunks (`"kable"` is kept
  as a synonym). New `options(tabxplor.tab_kable_tooltips = FALSE)` switches the per-cell hover
  tooltips off document-wide. The vignettes now showcase the live html tables.
* **Significance stars and correct confidence intervals.** Stars are opt-in (`stars =`); cell / difference
  / mean intervals are now the proper asymmetric intervals (Wilson, Newcombe, Welch) and the stars read
  the same interval. `ci` gains `"ratio"`; `method_cell` / `method_diff` / `method_mean_diff` /
  `method_mean_ratio` / `method_ratio` choose the interval.
* **Mean columns get a whole-table test** — a one-way ANOVA (Welch or classic, `options(tabxplor.anova)`),
  the counterpart of the chi-squared for factor columns.
* **Effect sizes, Fisher's exact and survey-robust p-values.** `test = TRUE` now carries Cramér's V / phi
  or eta²; a small sparse table uses Fisher's exact; `test = "survey"` (with `ids` / `strata` / `fpc` /
  `nest`, or a `survey::svydesign` as `data`) and `options(tabxplor.kish_neff = TRUE)` give design-based /
  effective-sample-size p-values and confidence intervals.
* **Readable colour legends and footers**, fully translatable to **French**
  (`options(tabxplor.lang = "fr")`, a `lang =` argument, or the R/OS locale).
* **Labelled-data (`haven`) support.** Value labels become the factor levels;
  `options(tabxplor.var_labels = TRUE)` shows variable labels instead of names in exports.
* **New arguments on `tab()`**: `na` gains `"common_base"` /,
  `spread_vars =`, `n_min =` (hide small-base cells), `display =` (composite cells like `"{pct} (n={n})"`),
  `common_totrow =`, a per-`col_var` / positional `ref`, and `parallel =` (opt-in, needs `mirai`).
* **`tab_counts()`** — build a full colour-coded table from already-aggregated counts (long, wide, `table`,
  or frequencies + base N) instead of microdata.
* **`tab_reg()`** — colour-coded regression tables (linear / logistic / Poisson / multinomial / ordinal),
  with survey weights, model comparison, average marginal effects, and Excel / HTML / Markdown export.
  See the regression vignette. `tab_logit()` / `multi_logit()` are thin wrappers; `or_plot()` /
  `lm_plots()` draw it.
* **`tab_export()`** — one entry point for every export format. **`tab_html()`** is the new name for
  `tab_kable()` (kept as a permanent alias). **`tab_css()`** generates one stylesheet for a whole document;
  its cell-colour rules survive Bootstrap-based host pages (pkgdown, Quarto), which style table cells
  themselves. **`set_caption()` / `get_caption()`** store a caption that survives a pipeline.
* **`tab_transpose()` / `transpose = TRUE`** — flip a table, mainly for the column-percentage inversion
  workflow. Also: **French vignettes on a bilingual pkgdown website**.
* **New jamovi "Regression models" analysis (`jmvtabreg`)** for `tab_reg()`. The Crosstables module (`jmvtab`)
  gains a reference-level picker, export, a live cache, and the new options. The jamovi html results and
  exports now show the per-cell hover tooltips (counts, confidence intervals, differences;
  `options(tabxplor.tab_kable_tooltips = FALSE)` to disable).

## Changes that may affect existing code

* **Excel export now uses `openxlsx2`** (Suggests) instead of `openxlsx`.
* **Dependencies reshuffled.** `magrittr` / `stringr` / `crayon` are dropped, so **`%>%` is no longer
  re-exported** — use the base `|>` pipe (or load `magrittr`/`dplyr`). `kableExtra` and `DescTools` move to
  Suggests; `survey` / `nnet` / `MASS` / `broom` become hard dependencies (weighted, multinomial, ordinal
  and basic `tab_reg()` work out of the box).
* **Significance stars are opt-in** (off by default) in `tab()`; `tab_reg()` still shows them.
* **`add_n`, `add_pct` and chi-squared / ANOVA p-values are now drawn at display/export time**, not stored
  as columns/rows in the built object. Read them via `get_n()` on the Total column and the `test` attribute
  (`get_test()`).
* For **numeric (mean) columns**, the `diff` field is now a real **difference**; the cell/reference ratio
  moved to the `ratio` field.
* **`tab(na = "drop")` with several `col_vars`** now drops each column's own missing values (the old shared
  base is now `na = "drop_all"`).
* A few options got clearer names (`tabxplor.kable_css` → `tabxplor.tab_kable_css`, plus
  `tabxplor.console_theme` / `tabxplor.export_theme`); the old names still work.

## Bug fixes

* A factor carrying **`NA` as a real level** (`factor(..., exclude = NULL)`, common in imported data) no
  longer crashes `print()` / `format()` / any export.
* `tab()` accepts a **`data.table`** as input, and a **logical `col_var`**.
* **Clearer errors** for an unknown named `ref`, a variable used as both a tab and a row/column variable,
  and an all-zero / all-`NA` weight.
* The **`lang` argument now works on Linux** (`lang = "fr"` used to return an English legend).

## Deprecations

Soft-deprecated (still work):

* `tab_many()` (use `tab()` with several `row_vars` / `col_vars`); singular `row_var` / `col_var`;
  `tab(sup_cols =)` (use `col_vars =`); `tab(filter =)` (filter upstream).
* `tab_pct()` / `tab_tot()` / `tab_totaltab()`; `tab_transpose()` (use `transpose = TRUE`); `tab_plot()`.
* Renamed arguments: `chi2` → `test`, `tab_xl(print_color_legend =)` →`color_legend =`.
* The combined colour strings `"diff_ci"` / `"after_ci"` / `"ci"` (use `color = "diff"` +
  `color_signif =`); `color_type` (now inert).

Removed / defunct (now error):

* `tab_xl(n_min =, hide_near_zero =)` (long inert); the little-used `totcol` vector
  forms; the `tabxplor.compact` option (use `output_list =`).


# tabxplor 1.3.1

* `jmvtab()` : added "Export to Excel" button to use `tab_xl()` in Jamovi UI.

* Small bug corrections.


# tabxplor 1.3.0

## Added
* `tab_many()` : with several `row_vars`, adding `compact = TRUE` bind all tables
 in a single one (but only works if no `tab_vars` are provided).
 `tab_compact()` can be used to do the same on `tab_plain()`.
* by default, chi2 pvalue is now added as a row in the tables (below total rows):
  there is no more chi2 table in attribute by default (but you can still add it
  manually with `tab_chi2()`). `tab_pvalue_lines()` do that from `attr(tabs, "chi2")`.
* by default with `tabxplor_tab`, `arrange()` now keeps the order of groups and totals
* in `tab_num()`, if all means < 10, display 1 digit ; if all means < 1, display 2 digits
* `tab_kable()` now works with a `list` of `tab`, if all `col_vars` are the same and there are no `tab_vars`

## Bug corrections
* in `tab_kable()`, escape signif stars * with \ in  .rmd only, otherwise it breaks the html
* `ci = "cell"` didn't work with `pct = "col"` with non pct rows


# tabxplor 1.2.1

## Added
* in `tab()` and `tab_many()`, possibility to add unweighted counts (`add_n = TRUE`) ; and with row and column percentages a row or column with the other kind of percentage (`add_pct = TRUE`)
* `kable_tabxplor_style()` : same html table style than `tab_kable()`, but for any data.frame.
* with `color = "diff"`, a `×2` color rule was added
* enhanced printing of confidence intervals for differences
* enhanced printing of pvalue (`<0.01%` style), Chi2 number added in Chi2 tables.

## Bug corrections
* reference columns were lost with `pct = "col"`
* `tab_kable()`and `tab_plot()` : removed unwanted bold formatting



# tabxplor 1.2.0

## Added
* `jmvtab()` : implementation of `tab_many()` for Jamovi, with a user interface
* `tab_plot()` : print tables as `ggplot`, to export as images
* `tab_wrap_text()` : function to wrap text in rownames and colnames

# tabxplor 1.1.3

## Bug corrections
* `tab_kable()` : html tags in tables were no longer working and were appearing as text (`knitr::kable()` now needs a `format = "html"` argument)



# tabxplor 1.1.2

## Added
* `tab_kable()` : a `color_legend` argument have been added, to possibly remove the legend.

## Bug corrections
* `tab_color_legend` had a mistake causing an error



# tabxplor 1.1.1

## Added
* `fct_recode_helper()` : helper function to recode multiple variables with `forcats::fct_recode`.
* `complete_partial_totals()` : complete partial total rows, total tables, and reference rows.

## Bug corrections
* `tab_spread` : incomplete subtables led to partial total rows, total tables and reference rows.
* `tab_xl` : with `sheets = "unique"`, multiple empty sheets were created anyway
* `crayon()` error with colors in tabs printing on R 4.2.2
* color printing was not working with only one numeric `col_var`


# tabxplor 1.1.0

## Added
* `tab_plain` have been separated in two functions, `tab_plain` for factors, `tab_num` for numeric variables
* `tab_plain` and `tab_num` have been rewrited in `data.table` to gain speed with big databases.


# tabxplor 1.0.3

## Added
* Remove rows with missing values or 0 in `wt` (weight), for them not to be added in counts (except in `tab_plain`)
* `fmt_get_color_code()` : get the html color codes of a table as a character vector

## Bug corrections
* `tab_many` : bug with totaltab when two numeric column variables (and a tabs_var)
* `tab_spread` not working with two `tab_vars`. Ok with a workaround, but would need to calculate one subtotal for each level of `spread_vars` in * `tab_totaltab` to fully work (and, then, to fully hierarchise total tables...).
* `wt` argument procudes missing values with NA ; NA in weight variable are now automatically removed (excepted in `tab_plain`)
* Addition between `fmt` vectors wasn't working no more with percentages
* In `tab_plain`, `col_var` was not sorted anymore (`names_sort = TRUE` added in `pivot_wider`)
* `tab_color_legend()` was not working when some cols were colored and some not colored
* In `tab()` functions, correction was made to remove a R 4.1.2 `dplyr` warning message (data frame results in `filter()` are deprecated, use `if_any()` or `if_all()`).


# tabxplor 1.0.2

## Added
* With `tab_kable`, option to use html `popover` instead of `tooltips`, to be able to use it in rmarkdown with a floating table of content.
* Two new 24 bits color styles for hmtl tables (`"blue_red"` and `"green_red"`).
* Possibility to provide a custom color palette for color styles, using `set_color_style()`.
* `tab_core` was deprecated and renamed `tab_plain` for more clarity. Added options to render a table with normal numeric vectors instead of fmt, and to render a plain data.frame instead of a tibble.
* Two way to print confidence intervals, using global option `"tabxplor.ci_print"` : `"moe"`, for margin of errors, prints as `12%±1.1` ; `"ci"` prints the interval `11·13%`.
* In `tab_kable`, confidence intervals of type `"cell` with print type `"moe"` appear in subscript.
* In `tab_xl`, colors now are the same and works in the same way that `tab` and `tab_kable`.

## Bug corrections
* With `tab` argument `color = "after_ci"`, when `diff` is negative, cells between 0 and -5% don't get colors.
* Problems in `tab_plain` with zero-rows dataframes
* With `color = "contrib"`, no color when contribution is equal to the mean contribution (or a multiple of it).
* With `tab_kable`, white spaces are producing unwanted text wrapping (in the middle of numbers)
* In tabs and tooltips, `diff` not printing good with `type = "mean"`.


# tabxplor 1.0.1
* Add possibility to export tables in html using `kableExtra`.
* Ensure functions do not write by default in the user's home filespace.

## Bug corrections
* Change color style not working in R CMD check : add possibility to change color style with global options.
* Total rows appear even when not wanted in `tab` and `tab_many`.
* `tab_many` not working with `listed = "TRUE"`


# tabxplor 1.0.0
* This is the first stable and published version of `tabxplor`.
