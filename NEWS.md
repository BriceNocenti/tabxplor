# tabxplor 1.4.0 (in development)

## New features
* New `tab_counts()` --- build a full color-coded cross-table from **already-aggregated counts**
  instead of microdata. It accepts long tidy counts (e.g. a `dplyr::count()` result), a wide
  `data.frame` of counts (with `cols` / `col_name`), a `table` / `xtabs` / `matrix` object, and
  frequencies + base N (`input = "pct"`, `base`). All the usual calculations (percentages,
  differences, confidence intervals, chi-squared, colors, totals) are done on the counts, and the
  result is identical to the table `tab()` would build from the underlying microdata. For weighted
  data, give the real unweighted count in `counts` and the weighted count in `wt_counts` (estimates
  are weighted, inference uses the real unweighted sample size). Input whose counts are not whole
  numbers (frequency-only / weighted-only) still shows percentages and colors, but confidence
  intervals and chi-squared are disabled with a message.
* Redesigned, faster colors. The `color` argument now separates **what** is measured from **how**
  significance is shown. `color` accepts `TRUE` (a smart per-column-type default: percentage-point
  difference on the text + a "×2" relative-risk highlight on the background for factors, mean ratio
  for numerics), a single measure (`"diff"`, `"ratio"`, `"contrib"`, `"or"`), or a two-channel
  `c("diff", "ratio")` / `c(text = "diff", background = "ratio")`. A separate `color_signif`
  argument (`"ignore"` / `"grey_non_signif"` / `"color_all_signif"`) replaces the old
  `"diff_ci"` / `"after_ci"` modes (which still work). Numeric `color = "diff"` now colors the
  standardized (SD-scaled) difference; the old ratio colouring is `color = "ratio"`. Color breaks
  are set with a named list `set_color_breaks(list(pct_diff =, pct_ratio =, mean_diff =,
  mean_ratio =, contrib =))` (the old `pct_breaks` / `mean_breaks` / `contrib_breaks` arguments are
  soft-deprecated). The colour engine was rewritten around `findInterval`, making console printing
  and `tab_kable()` dramatically faster on tall tables (the old per-cell resolver was O(n²)).
* Significance stars for `ci = "diff"`. Each cell now shows `*` / `**` / `***` (p < 0.10 / 0.05 /
  0.01, customisable via `options("tabxplor.signif_levels")` / `"tabxplor.signif_labels")`) for the
  difference from its reference, in the console, `tab_md()` and `tab_kable()`. Significance is read
  from the same confidence interval that is displayed, so the stars and the `[inf; sup]` bracket can
  never disagree. Controlled by the new `stars` argument (default `TRUE`; `NULL` uses
  `options("tabxplor.stars")`). `ci = "cell"` intervals are descriptive and carry no stars.
* Confidence intervals are now correct **asymmetric** intervals. Percentage cell intervals use the
  Wilson score interval and percentage-difference intervals now default to the **Newcombe** method
  (was Agresti-Caffo); mean-difference intervals use the Welch t interval when stars are on. The
  printed `[inf; sup]` bracket reads the real lower and upper bounds (previously a symmetric bracket
  reconstructed from a single half-width, which mis-drew Wilson/Newcombe intervals). `ci = "cell"`
  also draws an interval on the total column now.
* New `method_cell` / `method_diff` arguments on `tab()` (already on `tab_many()`/`tab_ci()`):
  `method_diff` accepts `"newcombe"` (default), `"ac"` or `"wald"`.
* Optional Kish effective sample size for weighted numeric (mean) confidence intervals /
  significance, via `options("tabxplor.kish_neff" = TRUE)`. Off by default (weighted estimate with
  the unweighted count, as before).
* **Mean (numeric) columns now get a whole-table significance test** — a one-way ANOVA, the
  counterpart of the Chi-squared test for factor columns. Both **Welch's F** (default, robust to
  unequal group variances) and the classic pooled F are computed; `options("tabxplor.anova")`
  (`"welch"` / `"classic"`) chooses which p-value is shown. A p-value row now appears under mean
  columns as it already did under factor columns.

## Internal
* Rewrote the Chi-squared / ANOVA computation onto a fast, vectorised engine (`R/tab-agg.R`:
  `agg_chi2()`, `agg_anova()`): every (sub)table is tested in a single grouped `data.table` pass
  instead of a per-table `stats::chisq.test()` loop, making `tab_chi2()` about 2.5× faster (it was
  the single biggest cost of `tab()`/`tab_many()`). Chi-squared results match `chisq.test()` exactly
  (including the Yates correction on 2×2 tables); Welch's / classic F match `stats::oneway.test()`.
  Also fixes `tab_chi2()` on a table that already carries `add_n` columns/rows.
* The table-level test results moved from the `chi2` attribute to a tidy **`test`** attribute (one row
  per sub-table × column × test, holding Chi2 and ANOVA F together). This is an internal contract:
  `attr(x, "chi2")` is renamed, but the `get_chi2()` accessor still works (it reads the new `test`
  attribute), and the low-level `new_tab(chi2 = )` argument still works too (both are soft-deprecated
  aliases). Rebuild any table saved from an older version rather than relying on the raw attribute.
* Rewrote confidence-interval computation onto a fast, vectorised, closed-form engine
  (`R/tab-agg.R`), replacing the per-cell `DescTools` calls in `tab_ci()`. `DescTools` moved from
  Imports to Suggests (used only for test parity). `tab_ci()` and `tab_num()` now share the engine.
* Started the 1.4.0 aggregate-core (Phase 2). `tab_num()` now computes mean tables from **moment
  sums** (`n`, weighted `n`, `Sigma wx`, `Sigma wx^2`) in a single grouped pass, deriving the mean
  and variance afterwards (`R/tab-agg.R`), instead of the old per-group `weighted.var()` helper that
  recomputed the weighted mean on every call (a double scan). The total rows and total table are
  now roll-ups of that additive aggregate rather than two additional full-data scans. Output is
  unchanged (variances match to floating-point tolerance). The unweighted (sample, n-1) vs weighted
  (ML) variance definitions are preserved for now; unifying them is a later step.
* Each percentage cell now stores its own base: the `tot_n` field holds the cell's unweighted
  percentage base (its row / column / grand total, depending on `pct`; `NA` for count tables and
  mean cells), and a new `get_tot_wn()` accessor (also `$tot_wn`) recovers the weighted base as
  `wn / pct`. This makes a built table self-sufficient for computing exact statistics without
  re-scanning it for a total column. Table output is unchanged.
* Reshaped the internal `tabxplor_fmt` record from 15 to 18 per-cell fields (preparation for the
  1.4.0 aggregate-core): added `ci_inf`, `ci_sup`, `pvalue`, `tot_n`; renamed the never-used `rr`
  field to `ratio`; the confidence interval is now stored as bounds instead of a dedicated `ci`
  field. Table output is unchanged. Retro-compatibility for user code that reads fmt fields: `$ci`
  and `get_ci()` still return the CI half-width (recomputed from the bounds) and the `fmt(ci=)`
  argument still works; `$rr` is renamed `$ratio`; the low-level `vctrs::field(x, "ci")` (reading or
  setting the raw `ci` field) no longer works.
* Added a retro-compatibility test safety net before internal refactors: a `tabxplor_fmt`
  field/attribute contract test, a golden characterization harness for `tab()`/`tab_many()`
  output, and format-vs-Excel export-parity tests.
* Added an informational (never-failing) small-benchmark test that prints `tab()` pipeline
  timings, plus a standalone 8M-row performance harness (`dev/benchmarks/run_bench.R`).
* Experimental opt-in fast path for `tab_many()` on very large data: one shared finest-grain
  aggregate reused across all factor tables instead of one scan per `row_var` × `col_var`. Off by
  default (byte-identical output); enable with `options(tabxplor.fuse_min_rows = <n_rows>)`. Modest
  gain (~1.05–1.30× at 15M rows, more at larger N / sparser data).

## Changes that may affect existing code
* For **numeric (mean) columns**, the `diff` field is now a real **difference** (`cell_mean -
  ref_mean`); the cell/reference **ratio** (the old numeric-`diff` value) moved to the `ratio`
  field. Code reading `$diff` on mean columns now gets a difference — use `$ratio` for the ratio.
  Percentage-column `diff` is unchanged. Cell coloring is unchanged (`color = "diff"` on mean
  columns still colors the ratio for now).

## Bug corrections
* Mean tables (`tab_num()`) are now dramatically faster and lighter: computing sufficient moment
  sums in a single grouped pass (no more weighted-variance double scan) and building the totals /
  total table as roll-ups of that aggregate (instead of two extra full-data scans) makes an 8M-row
  mean table about 5–6× faster and use ~6× less memory unweighted, and about 8× faster and ~11×
  less memory weighted. Output is unchanged.
* Big weighted tables were dozens of times slower than unweighted ones: the internal
  label-collision guard scanned whole data columns instead of just factor levels, coercing an
  8M-row weight column to strings. Fixed — weighted `tab()` on 8M rows drops from ~30s to ~0.2s,
  and unweighted tables (and their memory use) also improve. Output is unchanged.

## Deprecations



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
