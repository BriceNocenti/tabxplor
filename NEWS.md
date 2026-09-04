# tabxplor 2.0.0.9000 (development version)

## New features

* **A wide html table now scrolls instead of widening the page.** `tab_html()` wraps every table in
  a `.tx-scrollbox`, and `tab_css()` styles it: the table keeps its own width up to the space it has
  and scrolls sideways past it, in a document, on a pkgdown site, in the RStudio/Positron Viewer and
  in jamovi alike. Its title stays outside the box, so it does not scroll away, and `@media print`
  lifts the clip. Opt out with `.tx-scrollbox { overflow-x: visible; max-width: none; }`.

* **`tab_html(cells = )` — write back what `get_data = TRUE` reads.** Hand back the same data.frame
  with some cells edited and each edit is written verbatim into its `<td>`: the cell keeps its
  classes (colour, alignment, borders) and its tooltip, and loses only the decorations that belonged
  to the text it replaced (the bold split, the background pill, the sparkline). A value still equal
  to the one the table renders means "keep", so the round trip
  `tab_html(x, cells = tab_html(x, get_data = TRUE))` renders `x` unchanged. This is the supported
  way to splice foreign markup — an input box, a link, a badge — into a tabxplor table, in place of
  parsing its html back out.

## Minor improvements and fixes

* **The weight footer no longer mentions intervals and tests that are not there.** A weighted table
  showing no confidence interval, no significance star, no test and no significance-gated colour now
  prints only "Weighted by `<wt>`."; the caveat about what the intervals rest on is kept for the
  tables that actually have one.


# tabxplor 2.0.0

## New features

* **`tab()` is the unified entry point.** It takes several `row_vars` and `col_vars`, merged into one
  table or returned as a list with `output_list = TRUE`, and composes with `tab_vars`;
  `col_vars = a*b` crosses two column variables. `tab_many()` is a soft-deprecated alias.
* **`tab_reg()` — colour-coded regression tables** (linear, logistic, Poisson, multinomial, ordinal),
  on weighted and survey data, with model comparison, marginal effects and every export format. Its
  estimand is a cascade, `family` → `link` → `measure` → `effect`. See `vignette("tabxplor-reg")`.
  - **Every modelled effect sits beside its observed (crude) counterpart** — the same quantity fitted
  with one predictor instead of all of them — so what adjustment changed is read across the table.
  `color = "adjustment"` colours that gap and tests it; `empirical = FALSE` turns it off.
  - **Every regression table checks itself**: linearity, proportional odds, dispersion, influence and
  collinearity, one footer row per model. `shape =` is the cure — fit a continuous predictor as
  quantile groups, a curve, or a log / sqrt transformation.
  - **jamovi**: a new "Regression models" analysis (`jmvtabreg`); Crosstables gains a reference-level
  picker, level merging, export, tooltips and the new options, each named after its R argument.
* **`tab_counts()`** builds a full colour-coded table from already-aggregated counts instead of
  microdata. **`forest_plot()`** draws any tabxplor table as estimates with their intervals, stars and
  own cell colours, returning a modifiable `ggplot`; **`reg_check_plots()`** draws the model checks.
* **Weights and survey designs.** Pass a `survey::svydesign()` as `data` and strata, clusters, `fpc`
  and calibration reach every interval, test and colour; `options(tabxplor.design_effect = TRUE)` does
  the same, exactly, for a plain weight column. A weighted table's footer states its basis.
* **Correct confidence intervals**, asymmetric where they should be (Wilson, Newcombe, Welch, Katz),
  chosen with one named vector `ci_method = c(cell =, diff =, mean_diff =, mean_ratio =)`. `ci` says
  only *where* the interval sits; significance stars are opt-in (`stars =`) and read that interval.
* **Whole-table tests**: an effect size (Cramér's V, phi, eta²), Fisher's exact on a sparse table, a
  one-way ANOVA for mean columns, and Haberman's adjusted residual behind `color = "contrib"`.
* **Redesigned colour API.** Position picks the visual channel (1st value → text, 2nd → background),
  `color = TRUE` is the smart per-column-type default, and every ladder is the same ladder written in
  another measure, so a shade means the same deviation whichever measure a table is read on. OKLCH
  palettes, `set_color_palette()`, `color_breaks`, a `theme = "auto"` dark mode, and black-and-white
  publication palettes (`theme = "print_ready"`) saying it all typographically.
* **One display grammar for both producers.** Named layouts (`"est"`, `"est_ci"`, `"est_base"`, …)
  built on `{}` tokens — `display = "{pct} (n={n})"` — where `{est}` is whatever the column estimates
  and `{base}` the level it sits on. It is post-hoc: `set_display()` on a finished table gives the
  same table as asking at build time. Every cell of a percentage table now carries its odds ratio,
  and `display = "odds"` prints the odds it is a ratio of.
* **`shape =` decides how a number enters a table**: quantile groups, bands at the mean and one
  standard deviation either side, one level per value, or a `"log"` / `"sqrt"` transformation. A
  numeric `row_vars` / `tab_vars` is grouped rather than exploded; a mean cell shows `49 (cv 36%)`.
* **New `tab()` arguments**:
  - `spread_vars` (each level of a sub-table variable becomes a block of columns, with one 
    `Total` row and one `n` column per block)
  - `n = c("range", "min", "no")` (how many people the table is about)
  - `n_min`, `common_totrow` and `na = "common_base"`.
* **`tab_export()`** is one entry point for every format, and **`tab_html()`** (the new name for
  `tab_kable()`) renders through a new dependency-free engine, about 3× faster and restylable because
  its geometry is CSS classes; **`tab_css()`** writes one stylesheet for a whole document. Also new:
  `options(tabxplor.print = "html")` with hover tooltips, `caption =`, `transpose = TRUE`.
* **Excel export moved to `openxlsx2`**: a ratio stays a real number that sorts and filters while
  printing `1/2.11`, column widths fit their content, a secondary number becomes a column of its own,
  and `tab_xl(check = "auto")` draws the model-check plots under a regression table.
* **Introspection accessors.** `tab_structure()` says what a table is and what
  can be done with it, `tab_columns()` what every numeric column estimates and how it is coloured,
  `fmt_attr()` any one column fact by name, `reg_measures()` and `reg_formulas()` the same for models.
* **`as.matrix()` and `as.table()` hand a table to base R** — the numbers as a plain matrix, or a
  base `table` with named dimnames — dropping the totals and the display-time rows, because a
  correspondence analysis or a chi-squared test run on a table's own margins is wrong:
  `FactoMineR::CA(as.matrix(tab(gss_cat, race, marital)))`.
* **French translations** of every legend, footer and message (`options(tabxplor.lang = "fr")`, a
  `lang =` argument, or the locale), on a bilingual pkgdown website. **Labelled data (`haven`)**:
  value labels become factor levels. **Parallel builds** with `options(tabxplor.parallel = TRUE)`.


## Changes that may affect existing code

* **Dependencies reshuffled.** `magrittr` / `stringr` / `crayon` are dropped, so **`%>%` is no longer
  re-exported** — use the base `|>` pipe. `kableExtra` and `DescTools` move to Suggests, Excel export
  from `openxlsx` to `openxlsx2`, and `survey` / `nnet` / `MASS` become hard dependencies.
* **The base count, `add_pct` and the chi-squared / ANOVA p-values are drawn at display time**, not
  stored as columns and rows. Read them with `Total$n` and `get_test()`.
* For **numeric (mean) columns** the `diff` field is a real **difference**, the cell/reference ratio
  moving to `ratio`, and a cell shows a coefficient of variation instead of a standard deviation; a
  numeric `row_vars` / `tab_vars` is grouped rather than exploded. `shape = "values_to_levels"` and
  `display = "mean_sd"` restore the old output exactly.
* **`tab(na = "drop")` with several `col_vars`** drops each column's own missing values (the old
  shared base is now `na = "drop_all"`).
* **The colour thresholds moved** for `pct_ratio`, `mean_ratio` and `mean_diff`, and the background
  channel keeps the two ratio scales' loudest rungs only. `set_color_breaks()` restores any of them.
* **A weighted table's whole-table test and effect size are computed on the weighted table** (only
  those two were still unweighted), Fisher's exact is skipped under weights, and `color = "contrib"`
  significance reads the adjusted residual on the unweighted *n*.
* **Everything past the variable roles must be named**, `tab()` and its siblings taking `...` right
  after their variable arguments: an unnamed extra argument is refused, a typo gets a suggestion, and
  an abbreviation that used to partial-match silently is refused. An unknown value now aborts too.
* **A variable with a level named `"Total"` (or `"Ensemble"`) is refused**, naming the level: those
  are tabxplor's own total-row labels. Rename it, or move them with `options(tabxplor.total_names =)`.
* **`fmt` column attributes**: `type` is split into `scale` (what the column estimates) and `pct_type`
  (which kind of percentage), `ci_type` is gone, and a new `col_group` names a column block's
  sub-population. Only code building or inspecting `fmt` vectors is affected; see
  `vignette("tabxplor-programming")`.
* **`tab_transpose()` is a supported reshape operation again** (it was soft-deprecated in the
  pre-release): it is the way to get a transposed *object*, and the only way to put a mean on a row,
  since a number given to `row_vars` is always cut into levels. Use the exporters' `transpose = TRUE`
  when only the output matters. A transposed column now claims only what its parts agreed on, by the
  same rule every `bind_rows()` follows.
* **Options renamed**, the old names still read: `tabxplor.total_names` replaces the `total_names` /
  `totaltab_name` / `other_level` arguments, and `tabxplor.stars` carries the star ladder.

## Bug fixes

* **`tab()` could not use a column whose name was not syntactic**: ``tab(data, x, `my var`)`` aborted
  saying the column did not exist.
* A factor carrying **`NA` as a real level** (`factor(..., exclude = NULL)`, common in imported data)
  no longer crashes `print()`, `format()` or any export.
* **`pct = "col"` and `na = "drop_all"` failed on an `ordered` row variable**, on printing or
  exporting; and **`tab(pct = "all", ci = "cell")` errored**, weighted or not.
* **The `lang` argument now works on Linux**, and switching language mid-session keeps the new one.
* **`tab_spread()` lost the table's whole test summary**, and `dplyr::bind_rows()` on two grouped
  tables lost the weight footnote, colour legend and caption.
* **`ref` / `ref2` accept `"last"`**, and errors are clearer for an unknown named `ref`, a variable
  used on two axes, or an all-zero weight. `tab()` accepts a `data.table` and a logical `col_var`.
* **`dplyr::bind_rows()` on two tables with different columns crashed on printing**: the missing
  cells come back all-`NA`, which the colour engine could not read. They now render blank everywhere.
* **A column holding unlike quantities** — a transposed table with percentage AND mean columns, or a
  bind of the two — **coloured a mean difference on the percentage-point ladder**, sending it to the
  deepest shade. It now grades only the cells its ladder can read, and says so once; `color = "ratio"`
  grades them all.

## Deprecations

### Soft-deprecated

* Functions: `tab_many()` (a shim over `tab()`) and the step-by-step chain `tab_pct()` /
  `tab_tot()` / `tab_totaltab()` / `tab_ci()` / `tab_chi2()`.
* `tab()` arguments: singular `row_var` / `col_var`, `sup_cols` (use `col_vars`), `filter` (filter
  upstream), `names_prefix` / `names_sort` (they belong to `tab_spread()`), `add_n` (use `n =`),
  `total_names` / `totaltab_name` / `other_level` (use `options(tabxplor.total_names =)`), `OR` (use
  `display = "{or}"` / `ref2 = "cumulative"`), `ci = "diff"` / `"ratio"` (use `ci = "ref"`), `chi2`
  (use `test`), `method_cell` / `method_diff` (use `ci_method`), and `"diff_ci"` / `"after_ci"` /
  `"ci"` (use `color` + `color_signif`).
* Export arguments, accepted and ignored with a message: `color_type`, `html_24_bit`,
  `html_font`, `full_width`, `position`, and `tab_xl(n_min =, hide_near_zero =)` — width, font and
  placement are CSS rules now (`tab_css()`), and `n_min` is a `tab()` argument. Every other unknown
  argument of `tab_html()` / `tab_md()` / `tab_xl()` / `tab_css()` / `forest_plot()` is an error
  with a suggestion, as it already was on `tab()`.
* Elsewhere: `tab_xl(print_color_legend =)` → `color_legend =`, `set_diff_type()` →
  `set_ref_type()`, the `in_totrow` cell field → `row_kind` (`is_totrow()` and `x$in_totrow` are
  unchanged), and `options(tabxplor.signif_levels)` / `options(tabxplor.signif_labels)`.
* The `fmt` attribute `type`, which said both what a column estimates and which percentage it is:
  `get_type()` / `set_type()` and `fmt(type =)` translate into `scale` + `pct_type` (see
  `?tabxplor-type`) and are defunct in 2.1.0.

### Hard-deprecated (defunct in 2.1.0)

* The step-by-step chain warns on every call. What goes away is the *chaining API*, not the
  statistics: `tab()` and `tab_num()` compute the same numbers, in one pass.
* `tab_prepare()`, `complete_partial_totals()` and `fct_recode_helper()` will become internal or be
  removed; `tab_prepare()`'s work is done by `tab()` itself.

### Removed (now an error)

* Functions: `tab_plot()` and `kable_tabxplor_style()` — use `tab_export()` and `tab_html()`.
* **The `kableExtra` HTML engine**, with `engine =` (accepted and ignored) and the options
  `tabxplor.tab_kable_engine`, `tabxplor.always_add_css_in_tab_kable`, `tabxplor.kable_html_font`.
  kableExtra stays optional, and is no longer what opens a table in the Viewer: `tab_html()`
  paints that page itself, to match the table's theme.
* Options: `method_ratio` and its siblings, `tabxplor.ci_print` (use `display = "base_ci"` /
  `"base_moe"`), `tabxplor.compact`, `tabxplor.color_style_type`.
* **The `fmt` attribute `ci_type`**, with `get_ci_type()` / `set_ci_type()` and `fmt()`'s
  `ci_type =` argument: the stored interval is always on the estimate's own `scale`.


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
