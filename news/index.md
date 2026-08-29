# Changelog

## tabxplor 2.0.0

### New features

- **[`tab()`](https://bricenocenti.github.io/tabxplor/reference/tab.md)
  is the unified entry point.** It takes several `row_vars` and
  `col_vars`, merged into one table or returned as a list with
  `output_list = TRUE`, and composes with `tab_vars`; `col_vars = a*b`
  crosses two column variables.
  [`tab_many()`](https://bricenocenti.github.io/tabxplor/reference/tab_many.md)
  is a soft-deprecated alias.
- **[`tab_reg()`](https://bricenocenti.github.io/tabxplor/reference/tab_reg.md)
  — colour-coded regression tables** (linear, logistic, Poisson,
  multinomial, ordinal), on weighted and survey data, with model
  comparison, marginal effects and every export format. Its estimand is
  a cascade, `family` → `link` → `measure` → `effect`. See
  [`vignette("tabxplor-reg")`](https://bricenocenti.github.io/tabxplor/articles/tabxplor-reg.md).
  - **Every modelled effect sits beside its observed (crude)
    counterpart** — the same quantity fitted with one predictor instead
    of all of them — so what adjustment changed is read across the
    table. `color = "adjustment"` colours that gap and tests it;
    `empirical = FALSE` turns it off.
  - **Every regression table checks itself**: linearity, proportional
    odds, dispersion, influence and collinearity, one footer row per
    model. `shape =` is the cure — fit a continuous predictor as
    quantile groups, a curve, or a log / sqrt transformation.
  - **jamovi**: a new “Regression models” analysis (`jmvtabreg`);
    Crosstables gains a reference-level picker, level merging, export,
    tooltips and the new options, each named after its R argument.
- **[`tab_counts()`](https://bricenocenti.github.io/tabxplor/reference/tab_counts.md)**
  builds a full colour-coded table from already-aggregated counts
  instead of microdata.
  **[`forest_plot()`](https://bricenocenti.github.io/tabxplor/reference/forest_plot.md)**
  draws any tabxplor table as estimates with their intervals, stars and
  own cell colours, returning a modifiable `ggplot`;
  **[`reg_check_plots()`](https://bricenocenti.github.io/tabxplor/reference/reg_check_plots.md)**
  draws the model checks.
- **Weights and survey designs.** Pass a
  [`survey::svydesign()`](https://rdrr.io/pkg/survey/man/svydesign.html)
  as `data` and strata, clusters, `fpc` and calibration reach every
  interval, test and colour; `options(tabxplor.design_effect = TRUE)`
  does the same, exactly, for a plain weight column. A weighted table’s
  footer states its basis.
- **Correct confidence intervals**, asymmetric where they should be
  (Wilson, Newcombe, Welch, Katz), chosen with one named vector
  `ci_method = c(cell =, diff =, mean_diff =, mean_ratio =)`. `ci` says
  only *where* the interval sits; significance stars are opt-in
  (`stars =`) and read that interval.
- **Whole-table tests**: an effect size (Cramér’s V, phi, eta²),
  Fisher’s exact on a sparse table, a one-way ANOVA for mean columns,
  and Haberman’s adjusted residual behind `color = "contrib"`.
- **Redesigned colour API.** Position picks the visual channel (1st
  value → text, 2nd → background), `color = TRUE` is the smart
  per-column-type default, and every ladder is the same ladder written
  in another measure, so a shade means the same deviation whichever
  measure a table is read on. OKLCH palettes,
  [`set_color_palette()`](https://bricenocenti.github.io/tabxplor/reference/set_color_palette.md),
  `color_breaks`, a `theme = "auto"` dark mode, and black-and-white
  publication palettes (`theme = "print_ready"`) saying it all
  typographically.
- **One display grammar for both producers.** Named layouts (`"est"`,
  `"est_ci"`, `"est_base"`, …) built on
  [`{}`](https://rdrr.io/r/base/Paren.html) tokens —
  `display = "{pct} (n={n})"` — where `{est}` is whatever the column
  estimates and `{base}` the level it sits on. It is post-hoc:
  [`set_display()`](https://bricenocenti.github.io/tabxplor/reference/fmt_fields.md)
  on a finished table gives the same table as asking at build time.
  Every cell of a percentage table now carries its odds ratio.
- **`shape =` decides how a number enters a table**: quantile groups,
  bands at the mean and one standard deviation either side, one level
  per value, or a `"log"` / `"sqrt"` transformation. A numeric
  `row_vars` / `tab_vars` is grouped rather than exploded; a mean cell
  shows `49 (cv 36%)`.
- **New
  [`tab()`](https://bricenocenti.github.io/tabxplor/reference/tab.md)
  arguments**:
  - `spread_vars` (each level of a sub-table variable becomes a block of
    columns, with one `Total` row and one `n` column per block)
  - `n = c("range", "min", "no")` (how many people the table is about)
  - `n_min`, `common_totrow` and `na = "common_base"`.
- **[`tab_export()`](https://bricenocenti.github.io/tabxplor/reference/tab_export.md)**
  is one entry point for every format, and
  **[`tab_html()`](https://bricenocenti.github.io/tabxplor/reference/tab_html.md)**
  (the new name for
  [`tab_kable()`](https://bricenocenti.github.io/tabxplor/reference/tab_html.md))
  renders through a new dependency-free engine, about 3× faster and
  restylable because its geometry is CSS classes;
  **[`tab_css()`](https://bricenocenti.github.io/tabxplor/reference/tab_css.md)**
  writes one stylesheet for a whole document. Also new:
  `options(tabxplor.print = "html")` with hover tooltips, `caption =`,
  `transpose = TRUE`.
- **Excel export moved to `openxlsx2`**: a ratio stays a real number
  that sorts and filters while printing `1/2.11`, column widths fit
  their content, a secondary number becomes a column of its own, and
  `tab_xl(check = "auto")` draws the model-check plots under a
  regression table.
- **Introspection accessors.**
  [`tab_structure()`](https://bricenocenti.github.io/tabxplor/reference/tab_structure.md)
  says what a table is and what can be done with it,
  [`tab_columns()`](https://bricenocenti.github.io/tabxplor/reference/tab_columns.md)
  what every numeric column estimates and how it is coloured,
  [`fmt_attr()`](https://bricenocenti.github.io/tabxplor/reference/fmt_attr.md)
  any one column fact by name,
  [`reg_measures()`](https://bricenocenti.github.io/tabxplor/reference/reg_measures.md)
  and
  [`reg_formulas()`](https://bricenocenti.github.io/tabxplor/reference/reg_formulas.md)
  the same for models.
- **[`as.matrix()`](https://rdrr.io/r/base/matrix.html) and
  [`as.table()`](https://rdrr.io/r/base/table.html) hand a table to base
  R** — the numbers as a plain matrix, or a base `table` with named
  dimnames — dropping the totals and the display-time rows, because a
  correspondence analysis or a chi-squared test run on a table’s own
  margins is wrong:
  `FactoMineR::CA(as.matrix(tab(gss_cat, race, marital)))`.
- **French translations** of every legend, footer and message
  (`options(tabxplor.lang = "fr")`, a `lang =` argument, or the locale),
  on a bilingual pkgdown website. **Labelled data (`haven`)**: value
  labels become factor levels. **Parallel builds** with
  `options(tabxplor.parallel = TRUE)`.

### Changes that may affect existing code

- **Dependencies reshuffled.** `magrittr` / `stringr` / `crayon` are
  dropped, so **`%>%` is no longer re-exported** — use the base `|>`
  pipe. `kableExtra` and `DescTools` move to Suggests, Excel export from
  `openxlsx` to `openxlsx2`, and `survey` / `nnet` / `MASS` become hard
  dependencies.
- **The base count, `add_pct` and the chi-squared / ANOVA p-values are
  drawn at display time**, not stored as columns and rows. Read them
  with `Total$n` and
  [`get_test()`](https://bricenocenti.github.io/tabxplor/reference/get_test.md).
- For **numeric (mean) columns** the `diff` field is a real
  **difference**, the cell/reference ratio moving to `ratio`, and a cell
  shows a coefficient of variation instead of a standard deviation; a
  numeric `row_vars` / `tab_vars` is grouped rather than exploded.
  `shape = "values_to_levels"` and `display = "mean_sd"` restore the old
  output exactly.
- **`tab(na = "drop")` with several `col_vars`** drops each column’s own
  missing values (the old shared base is now `na = "drop_all"`).
- **The colour thresholds moved** for `pct_ratio`, `mean_ratio` and
  `mean_diff`, and the background channel keeps the two ratio scales’
  loudest rungs only.
  [`set_color_breaks()`](https://bricenocenti.github.io/tabxplor/reference/set_color_palette.md)
  restores any of them.
- **A weighted table’s whole-table test and effect size are computed on
  the weighted table** (only those two were still unweighted), Fisher’s
  exact is skipped under weights, and `color = "contrib"` significance
  reads the adjusted residual on the unweighted *n*.
- **Everything past the variable roles must be named**,
  [`tab()`](https://bricenocenti.github.io/tabxplor/reference/tab.md)
  and its siblings taking `...` right after their variable arguments: an
  unnamed extra argument is refused, a typo gets a suggestion, and an
  abbreviation that used to partial-match silently is refused. An
  unknown value now aborts too.
- **A variable with a level named `"Total"` (or `"Ensemble"`) is
  refused**, naming the level: those are tabxplor’s own total-row
  labels. Rename it, or move them with
  `options(tabxplor.total_names =)`.
- **`fmt` column attributes**: `type` is split into `scale` (what the
  column estimates) and `pct_type` (which kind of percentage), `ci_type`
  is gone, and a new `col_group` names a column block’s sub-population.
  Only code building or inspecting `fmt` vectors is affected; see
  [`vignette("tabxplor-programming")`](https://bricenocenti.github.io/tabxplor/articles/tabxplor-programming.md).
- **[`tab_transpose()`](https://bricenocenti.github.io/tabxplor/reference/tab_transpose.md)
  is a supported reshape operation again** (it was soft-deprecated in
  the pre-release): it is the way to get a transposed *object*, and the
  only way to put a mean on a row, since a number given to `row_vars` is
  always cut into levels. Use the exporters’ `transpose = TRUE` when
  only the output matters. A transposed column now claims only what its
  parts agreed on, by the same rule every
  [`bind_rows()`](https://dplyr.tidyverse.org/reference/bind_rows.html)
  follows.
- **Options renamed**, the old names still read: `tabxplor.total_names`
  replaces the `total_names` / `totaltab_name` / `other_level`
  arguments, and `tabxplor.stars` carries the star ladder.

### Bug fixes

- **[`tab()`](https://bricenocenti.github.io/tabxplor/reference/tab.md)
  could not use a column whose name was not syntactic**:
  `` tab(data, x, `my var`) `` aborted saying the column did not exist.
- A factor carrying **`NA` as a real level**
  (`factor(..., exclude = NULL)`, common in imported data) no longer
  crashes [`print()`](https://rdrr.io/r/base/print.html),
  [`format()`](https://rdrr.io/r/base/format.html) or any export.
- **`pct = "col"` and `na = "drop_all"` failed on an `ordered` row
  variable**, on printing or exporting; and
  **`tab(pct = "all", ci = "cell")` errored**, weighted or not.
- **The `lang` argument now works on Linux**, and switching language
  mid-session keeps the new one.
- **[`tab_spread()`](https://bricenocenti.github.io/tabxplor/reference/tab_spread.md)
  lost the table’s whole test summary**, and
  [`dplyr::bind_rows()`](https://dplyr.tidyverse.org/reference/bind_rows.html)
  on two grouped tables lost the weight footnote, colour legend and
  caption.
- **`ref` / `ref2` accept `"last"`**, and errors are clearer for an
  unknown named `ref`, a variable used on two axes, or an all-zero
  weight.
  [`tab()`](https://bricenocenti.github.io/tabxplor/reference/tab.md)
  accepts a `data.table` and a logical `col_var`.
- **[`dplyr::bind_rows()`](https://dplyr.tidyverse.org/reference/bind_rows.html)
  on two tables with different columns crashed on printing**: the
  missing cells come back all-`NA`, which the colour engine could not
  read. They now render blank everywhere.
- **A column holding unlike quantities** — a transposed table with
  percentage AND mean columns, or a bind of the two — **coloured a mean
  difference on the percentage-point ladder**, sending it to the deepest
  shade. It now grades only the cells its ladder can read, and says so
  once; `color = "ratio"` grades them all.

### Deprecations

#### Soft-deprecated

- Functions:
  [`tab_many()`](https://bricenocenti.github.io/tabxplor/reference/tab_many.md)
  (a shim over
  [`tab()`](https://bricenocenti.github.io/tabxplor/reference/tab.md))
  and the step-by-step chain
  [`tab_pct()`](https://bricenocenti.github.io/tabxplor/reference/tab_pct.md)
  /
  [`tab_tot()`](https://bricenocenti.github.io/tabxplor/reference/tab_tot.md)
  /
  [`tab_totaltab()`](https://bricenocenti.github.io/tabxplor/reference/tab_totaltab.md)
  /
  [`tab_ci()`](https://bricenocenti.github.io/tabxplor/reference/tab_ci.md)
  /
  [`tab_chi2()`](https://bricenocenti.github.io/tabxplor/reference/tab_chi2.md).
- [`tab()`](https://bricenocenti.github.io/tabxplor/reference/tab.md)
  arguments: singular `row_var` / `col_var`, `sup_cols` (use
  `col_vars`), `filter` (filter upstream), `names_prefix` / `names_sort`
  (they belong to
  [`tab_spread()`](https://bricenocenti.github.io/tabxplor/reference/tab_spread.md)),
  `add_n` (use `n =`), `total_names` / `totaltab_name` / `other_level`
  (use `options(tabxplor.total_names =)`), `OR` (use `display = "{or}"`
  / `ref2 = "cumulative"`), `ci = "diff"` / `"ratio"` (use
  `ci = "ref"`), `chi2` (use `test`), `method_cell` / `method_diff` (use
  `ci_method`), and `"diff_ci"` / `"after_ci"` / `"ci"` (use `color` +
  `color_signif`).
- Export arguments, accepted and ignored with a message: `color_type`,
  `html_24_bit`, `html_font`, `full_width`, `position`, and
  `tab_xl(n_min =, hide_near_zero =)` — width, font and placement are
  CSS rules now
  ([`tab_css()`](https://bricenocenti.github.io/tabxplor/reference/tab_css.md)),
  and `n_min` is a
  [`tab()`](https://bricenocenti.github.io/tabxplor/reference/tab.md)
  argument. Every other unknown argument of
  [`tab_html()`](https://bricenocenti.github.io/tabxplor/reference/tab_html.md)
  /
  [`tab_md()`](https://bricenocenti.github.io/tabxplor/reference/tab_md.md)
  /
  [`tab_xl()`](https://bricenocenti.github.io/tabxplor/reference/tab_xl.md)
  /
  [`tab_css()`](https://bricenocenti.github.io/tabxplor/reference/tab_css.md)
  /
  [`forest_plot()`](https://bricenocenti.github.io/tabxplor/reference/forest_plot.md)
  is an error with a suggestion, as it already was on
  [`tab()`](https://bricenocenti.github.io/tabxplor/reference/tab.md).
- Elsewhere: `tab_xl(print_color_legend =)` → `color_legend =`,
  [`set_diff_type()`](https://bricenocenti.github.io/tabxplor/reference/fmt_attributes.md)
  →
  [`set_ref_type()`](https://bricenocenti.github.io/tabxplor/reference/fmt_attributes.md),
  the `in_totrow` cell field → `row_kind`
  ([`is_totrow()`](https://bricenocenti.github.io/tabxplor/reference/fmt_fields.md)
  and `x$in_totrow` are unchanged), and
  `options(tabxplor.signif_levels)` / `options(tabxplor.signif_labels)`.
- The `fmt` attribute `type`, which said both what a column estimates
  and which percentage it is:
  [`get_type()`](https://bricenocenti.github.io/tabxplor/reference/tabxplor-type.md)
  /
  [`set_type()`](https://bricenocenti.github.io/tabxplor/reference/tabxplor-type.md)
  and `fmt(type =)` translate into `scale` + `pct_type` (see
  `?tabxplor-type`) and are defunct in 2.1.0.

#### Hard-deprecated (defunct in 2.1.0)

- The step-by-step chain warns on every call. What goes away is the
  *chaining API*, not the statistics:
  [`tab()`](https://bricenocenti.github.io/tabxplor/reference/tab.md)
  and
  [`tab_num()`](https://bricenocenti.github.io/tabxplor/reference/tab_num.md)
  compute the same numbers, in one pass.
- [`tab_prepare()`](https://bricenocenti.github.io/tabxplor/reference/tab_prepare.md),
  [`complete_partial_totals()`](https://bricenocenti.github.io/tabxplor/reference/complete_partial_totals.md)
  and
  [`fct_recode_helper()`](https://bricenocenti.github.io/tabxplor/reference/fct_recode_helper.md)
  will become internal or be removed;
  [`tab_prepare()`](https://bricenocenti.github.io/tabxplor/reference/tab_prepare.md)’s
  work is done by
  [`tab()`](https://bricenocenti.github.io/tabxplor/reference/tab.md)
  itself.

#### Removed (now an error)

- Functions:
  [`tab_plot()`](https://bricenocenti.github.io/tabxplor/reference/tab_plot.md)
  and
  [`kable_tabxplor_style()`](https://bricenocenti.github.io/tabxplor/reference/kable_tabxplor_style.md)
  — use
  [`tab_export()`](https://bricenocenti.github.io/tabxplor/reference/tab_export.md)
  and
  [`tab_html()`](https://bricenocenti.github.io/tabxplor/reference/tab_html.md).
- **The `kableExtra` HTML engine**, with `engine =` (accepted and
  ignored) and the options `tabxplor.tab_kable_engine`,
  `tabxplor.always_add_css_in_tab_kable`, `tabxplor.kable_html_font`.
  kableExtra stays optional, and is no longer what opens a table in the
  Viewer:
  [`tab_html()`](https://bricenocenti.github.io/tabxplor/reference/tab_html.md)
  paints that page itself, to match the table’s theme.
- Options: `method_ratio` and its siblings, `tabxplor.ci_print` (use
  `display = "base_ci"` / `"base_moe"`), `tabxplor.compact`,
  `tabxplor.color_style_type`.
- **The `fmt` attribute `ci_type`**, with `get_ci_type()` /
  `set_ci_type()` and
  [`fmt()`](https://bricenocenti.github.io/tabxplor/reference/fmt.md)’s
  `ci_type =` argument: the stored interval is always on the estimate’s
  own `scale`.

## tabxplor 1.3.1

CRAN release: 2025-09-26

- [`jmvtab()`](https://bricenocenti.github.io/tabxplor/reference/jmvtab.md)
  : added “Export to Excel” button to use
  [`tab_xl()`](https://bricenocenti.github.io/tabxplor/reference/tab_xl.md)
  in Jamovi UI.

- Small bug corrections.

## tabxplor 1.3.0

CRAN release: 2025-03-09

### Added

- [`tab_many()`](https://bricenocenti.github.io/tabxplor/reference/tab_many.md)
  : with several `row_vars`, adding `compact = TRUE` bind all tables in
  a single one (but only works if no `tab_vars` are provided).
  [`tab_compact()`](https://bricenocenti.github.io/tabxplor/reference/tab_compact.md)
  can be used to do the same on
  [`tab_plain()`](https://bricenocenti.github.io/tabxplor/reference/tab_plain.md).
- by default, chi2 pvalue is now added as a row in the tables (below
  total rows): there is no more chi2 table in attribute by default (but
  you can still add it manually with
  [`tab_chi2()`](https://bricenocenti.github.io/tabxplor/reference/tab_chi2.md)).
  [`tab_pvalue_lines()`](https://bricenocenti.github.io/tabxplor/reference/tab_pvalue_lines.md)
  do that from `attr(tabs, "chi2")`.
- by default with `tabxplor_tab`,
  [`arrange()`](https://dplyr.tidyverse.org/reference/arrange.html) now
  keeps the order of groups and totals
- in
  [`tab_num()`](https://bricenocenti.github.io/tabxplor/reference/tab_num.md),
  if all means \< 10, display 1 digit ; if all means \< 1, display 2
  digits
- [`tab_kable()`](https://bricenocenti.github.io/tabxplor/reference/tab_html.md)
  now works with a `list` of `tab`, if all `col_vars` are the same and
  there are no `tab_vars`

### Bug corrections

- in
  [`tab_kable()`](https://bricenocenti.github.io/tabxplor/reference/tab_html.md),
  escape signif stars \* with  in .rmd only, otherwise it breaks the
  html
- `ci = "cell"` didn’t work with `pct = "col"` with non pct rows

## tabxplor 1.2.1

CRAN release: 2024-10-04

### Added

- in [`tab()`](https://bricenocenti.github.io/tabxplor/reference/tab.md)
  and
  [`tab_many()`](https://bricenocenti.github.io/tabxplor/reference/tab_many.md),
  possibility to add unweighted counts (`add_n = TRUE`) ; and with row
  and column percentages a row or column with the other kind of
  percentage (`add_pct = TRUE`)
- [`kable_tabxplor_style()`](https://bricenocenti.github.io/tabxplor/reference/kable_tabxplor_style.md)
  : same html table style than
  [`tab_kable()`](https://bricenocenti.github.io/tabxplor/reference/tab_html.md),
  but for any data.frame.
- with `color = "diff"`, a `×2` color rule was added
- enhanced printing of confidence intervals for differences
- enhanced printing of pvalue (`<0.01%` style), Chi2 number added in
  Chi2 tables.

### Bug corrections

- reference columns were lost with `pct = "col"`
- [`tab_kable()`](https://bricenocenti.github.io/tabxplor/reference/tab_html.md)and
  [`tab_plot()`](https://bricenocenti.github.io/tabxplor/reference/tab_plot.md)
  : removed unwanted bold formatting

## tabxplor 1.2.0

CRAN release: 2024-08-30

### Added

- [`jmvtab()`](https://bricenocenti.github.io/tabxplor/reference/jmvtab.md)
  : implementation of
  [`tab_many()`](https://bricenocenti.github.io/tabxplor/reference/tab_many.md)
  for Jamovi, with a user interface
- [`tab_plot()`](https://bricenocenti.github.io/tabxplor/reference/tab_plot.md)
  : print tables as `ggplot`, to export as images
- [`tab_wrap_text()`](https://bricenocenti.github.io/tabxplor/reference/tab_wrap_text.md)
  : function to wrap text in rownames and colnames

## tabxplor 1.1.3

CRAN release: 2024-03-08

### Bug corrections

- [`tab_kable()`](https://bricenocenti.github.io/tabxplor/reference/tab_html.md)
  : html tags in tables were no longer working and were appearing as
  text ([`knitr::kable()`](https://rdrr.io/pkg/knitr/man/kable.html) now
  needs a `format = "html"` argument)

## tabxplor 1.1.2

CRAN release: 2024-02-08

### Added

- [`tab_kable()`](https://bricenocenti.github.io/tabxplor/reference/tab_html.md)
  : a `color_legend` argument have been added, to possibly remove the
  legend.

### Bug corrections

- `tab_color_legend` had a mistake causing an error

## tabxplor 1.1.1

CRAN release: 2024-01-22

### Added

- [`fct_recode_helper()`](https://bricenocenti.github.io/tabxplor/reference/fct_recode_helper.md)
  : helper function to recode multiple variables with
  [`forcats::fct_recode`](https://forcats.tidyverse.org/reference/fct_recode.html).
- [`complete_partial_totals()`](https://bricenocenti.github.io/tabxplor/reference/complete_partial_totals.md)
  : complete partial total rows, total tables, and reference rows.

### Bug corrections

- `tab_spread` : incomplete subtables led to partial total rows, total
  tables and reference rows.
- `tab_xl` : with `sheets = "unique"`, multiple empty sheets were
  created anyway
- `crayon()` error with colors in tabs printing on R 4.2.2
- color printing was not working with only one numeric `col_var`

## tabxplor 1.1.0

CRAN release: 2022-06-15

### Added

- `tab_plain` have been separated in two functions, `tab_plain` for
  factors, `tab_num` for numeric variables
- `tab_plain` and `tab_num` have been rewrited in `data.table` to gain
  speed with big databases.

## tabxplor 1.0.3

CRAN release: 2022-04-09

### Added

- Remove rows with missing values or 0 in `wt` (weight), for them not to
  be added in counts (except in `tab_plain`)
- [`fmt_get_color_code()`](https://bricenocenti.github.io/tabxplor/reference/fmt_get_color_code.md)
  : get the html color codes of a table as a character vector

### Bug corrections

- `tab_many` : bug with totaltab when two numeric column variables (and
  a tabs_var)
- `tab_spread` not working with two `tab_vars`. Ok with a workaround,
  but would need to calculate one subtotal for each level of
  `spread_vars` in \* `tab_totaltab` to fully work (and, then, to fully
  hierarchise total tables…).
- `wt` argument procudes missing values with NA ; NA in weight variable
  are now automatically removed (excepted in `tab_plain`)
- Addition between `fmt` vectors wasn’t working no more with percentages
- In `tab_plain`, `col_var` was not sorted anymore (`names_sort = TRUE`
  added in `pivot_wider`)
- [`tab_color_legend()`](https://bricenocenti.github.io/tabxplor/reference/tab_color_legend.md)
  was not working when some cols were colored and some not colored
- In [`tab()`](https://bricenocenti.github.io/tabxplor/reference/tab.md)
  functions, correction was made to remove a R 4.1.2 `dplyr` warning
  message (data frame results in
  [`filter()`](https://dplyr.tidyverse.org/reference/filter.html) are
  deprecated, use
  [`if_any()`](https://dplyr.tidyverse.org/reference/across.html) or
  [`if_all()`](https://dplyr.tidyverse.org/reference/across.html)).

## tabxplor 1.0.2

CRAN release: 2021-10-21

### Added

- With `tab_kable`, option to use html `popover` instead of `tooltips`,
  to be able to use it in rmarkdown with a floating table of content.
- Two new 24 bits color styles for hmtl tables (`"blue_red"` and
  `"green_red"`).
- Possibility to provide a custom color palette for color styles, using
  [`set_color_style()`](https://bricenocenti.github.io/tabxplor/reference/set_color_palette.md).
- `tab_core` was deprecated and renamed `tab_plain` for more clarity.
  Added options to render a table with normal numeric vectors instead of
  fmt, and to render a plain data.frame instead of a tibble.
- Two way to print confidence intervals, using global option
  `"tabxplor.ci_print"` : `"moe"`, for margin of errors, prints as
  `12%±1.1` ; `"ci"` prints the interval `11·13%`.
- In `tab_kable`, confidence intervals of type `"cell` with print type
  `"moe"` appear in subscript.
- In `tab_xl`, colors now are the same and works in the same way that
  `tab` and `tab_kable`.

### Bug corrections

- With `tab` argument `color = "after_ci"`, when `diff` is negative,
  cells between 0 and -5% don’t get colors.
- Problems in `tab_plain` with zero-rows dataframes
- With `color = "contrib"`, no color when contribution is equal to the
  mean contribution (or a multiple of it).
- With `tab_kable`, white spaces are producing unwanted text wrapping
  (in the middle of numbers)
- In tabs and tooltips, `diff` not printing good with `type = "mean"`.

## tabxplor 1.0.1

CRAN release: 2021-09-14

- Add possibility to export tables in html using `kableExtra`.
- Ensure functions do not write by default in the user’s home filespace.

### Bug corrections

- Change color style not working in R CMD check : add possibility to
  change color style with global options.
- Total rows appear even when not wanted in `tab` and `tab_many`.
- `tab_many` not working with `listed = "TRUE"`

## tabxplor 1.0.0

- This is the first stable and published version of `tabxplor`.
