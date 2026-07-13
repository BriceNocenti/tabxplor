# tabxplor (development version)

# tabxplor 1.4.0 (in development)

## New features
* New `tab_logit()` and `multi_logit()` --- **logistic-regression tables** as color-coded
  `tabxplor` tables. `tab_logit(data, dependent, predictors)` fits a binary logit per dependent
  variable and shows the odds ratios (one column per dependent) grouped by predictor, with the
  reference level as `1`; `multi_logit(data, dependent, models = list(...))` compares several models
  side by side (one column each). Each odds ratio carries its 95% confidence interval and p-value, so
  the table prints with significance stars, greys out non-significant odds ratios, shows odds ratios
  below 1 as `1/x`, and exports to Excel / HTML / Markdown like any other table. Survey weights are
  supported (`wt =`, via `survey::svyglm`). `method = "wald"` (default) or `"profile"` chooses Wald
  vs profile-likelihood intervals and tests; `color_signif` controls how significance drives the
  colours. `broom` and `survey` (and `MASS` for `method = "profile"`) are optional dependencies.
* `tab()` odds-ratio columns (and any odds ratio) now print values below 1 as `1/x` (e.g. `1/4`
  instead of `0.25`), so they compare symmetrically with odds ratios above 1.
* `tab_md()` now exports **colored** markdown. A table built with colors (e.g. `tab(..., color = "diff")`)
  renders each cell as a short pandoc bracketed span `[value]{.class}`, so it shows up colored in Quarto,
  R Markdown and pandoc. The class names are readable and describe the color break --- `p5`/`p10`/`p20`
  (over-represented), `m5`/... (under), `x2`/`x1_5` and `d2`/... (ratios), `sd0_2`/... (standardized mean
  differences); the background channel uses the same names prefixed `bg`. Numbers still line up in a
  monospace editor. `color = FALSE` gives plain monochrome markdown, and an uncolored table is unchanged.
* New `tab_md_css()` --- generate the CSS that styles those spans, matching the exact color breaks and
  palette of your table (with a `prefers-color-scheme: dark` block). Use `tab_md(css = TRUE)` to embed it
  inline, or include the stylesheet in your document.
* `tab_md()` gains a `caption` argument (rendered as a pandoc table caption) and, by default,
  `wrap_rows = NULL` no longer truncates long row labels (pass a number to cap them).
* `tab_kable()` gains a faster, dependency-free HTML render engine. The new `engine` argument
  (`"kableExtra"`, the default, or `"html"`) selects it; `engine = "html"` produces a self-contained,
  inline-CSS `<table>` that needs no external stylesheet --- about 3x faster and much lighter than the
  kableExtra output, and used by the jamovi live display. Set a session default with
  `options(tabxplor.tab_kable_engine = "html")`.
* `tab_kable()` now renders a **list of non-mergeable tables** (different column variables, or tables
  with sub-tables) one after another, instead of stopping with an error.
* `tab_kable()` is faster overall: the hover tooltips are computed only for the fields a column actually
  has (roughly a 30% speed-up on colored tables), and empty cells now render as blank in every context
  (knitr, R Markdown, ...) instead of occasionally showing "NA".
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
* New `parallel` argument in `tab()` / `tab_many()` for the "build many tables at once" workflow.
  With several `row_vars`, `parallel = TRUE` (or an integer worker count) builds the per-`row_var`
  tables on a persistent pool of background R processes, byte-identical to the sequential result.
  It is off by default and opt-in (set a session default with `options(tabxplor.parallel =)`); it
  pays off for many tables on a small-to-medium survey (roughly 10k--60k rows) and is a loss for a
  handful of tables or multi-million-row data. It needs the suggested **mirai** package; release the
  worker pool with the new `tab_parallel_stop()`.
* New `tab_transpose()` --- flip a table so its rows become columns and its columns become rows. The
  main use is the **column-percentage inversion** workflow: to color a `pct = "col"` table with
  several row variables (which the coloring machinery cannot do directly), build it the other way
  (swap the variables and use `pct = "row"`), then `tab_transpose()` gives the column-percentage
  layout for export. Percentages, differences, confidence intervals and colors ride along; the total
  row/column and reference row/column are swapped, and the whole-table test is re-keyed. It handles a
  single table (one row variable, one column variable, at most one total row/column).
* `tab_xl()` Excel export was rewritten on the actively-maintained **openxlsx2** engine (replacing
  openxlsx), and now takes a single table or a list. It gains `transpose = TRUE` (transpose each table
  before export) and an experimental `conditional_format =` (reserved; currently a no-op with a
  message). Significance stars now show in the exported cells (folded into the Excel number format, so
  the cell stays a real number). Colors and number styles are applied over the fewest possible cell
  ranges. The deprecated `n_min` / `hide_near_zero` arguments are still accepted but do nothing (use
  `tab(n_min = )`).
* New `tab_export()` --- one entry point for every export format:
  `tab_export(x, format = c("kable", "md", "xl", "plot"))` dispatches to `tab_kable()`, `tab_md()`,
  `tab_xl()` or `tab_plot()` (pass a `path` to write the file). The four exporters now share the same
  display arguments and defaults: `color` (set `FALSE` for a monochrome table), `color_legend`,
  `transpose` (transpose the table at export) and `caption` / `theme` / `color_type` are available
  consistently across all of them. `tab_xl()` is now **theme-aware** (`theme = "dark"`), and
  `tab_plot()` renders a non-mergeable list as a **list of plots** (like the other exporters) instead
  of stopping with an error.
* New `display` argument in `tab()` for an opt-in **composite display** showing several fields per cell,
  written as a `{}` template listing the fields to combine: `display = "{pct} (n={n})"` prints each
  percentage with its count (e.g. `76% (n=13)`), `"{n} ({pct})"` the reverse, `"{diff} [{ci}]"` a
  difference with its interval. Valid fields: `pct`, `n`, `wn`, `mean`, `diff`, `ratio`, `ci`, `or`,
  `ctr`, `var` (the first is the primary). It is a display overlay for text output (the console,
  `tab_kable()`, `tab_md()`) -- colors, differences and the underlying fields are unchanged, and Excel
  keeps the primary field.
* The exporters (`tab_kable()`, `tab_md()`, `tab_plot()`, `tab_xl()`) and the print methods no longer
  crash on a plain `data.frame` or a table with no factor / no formatted columns: they render the
  plain table with a short message explaining that tabxplor formatting was skipped. Variable-role
  detection for rendering is now position-independent (a factor moved after the value columns is no
  longer mis-read).
* `tab_md()` now renders a **list of tables one after another** when they cannot be merged --- e.g.
  a `tab()` with several `row_vars` and a `tab_vars` (which returns a list of subtabled tables), or a
  list of tables with different `col_vars`. Each table keeps its own `tab_vars` sub-tables. A list of
  tables sharing the same `col_vars` (and no `tab_vars`) is still merged into one, as before. (This
  replaces the previous "same col_vars / no tab_vars" errors for `tab_md()`.)
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
  and `tab_kable()` dramatically faster on tall tables (the old per-cell resolver was O(n²)). All
  exporters (`tab_kable()`, `tab_plot()`, `tab_xl()`) now render both colour channels at once (text
  colour + background fill), and the colour legend was reworked to read the canonical break scales
  directly, so numeric `diff` legends show the SD-based thresholds actually used (they previously
  showed a ratio scale).
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
  `method_cell` accepts `"wilson"` (default) or `"wald"` (the normal approximation, commonly taught);
  `method_diff` accepts `"newcombe"` (default), `"ac"` or `"wald"`.
* New `n_min` argument on `tab()` --- hide small-base rows/columns to read a table without the noise
  of unreliable cells. A row is dropped only when its **largest** base across the column variables is
  below `n_min`; surviving cells whose own base is below `n_min` are blanked; under `pct = "col"` weak
  columns are dropped. It is a pure display filter: totals, the added-`n` row/column and the p-value
  line are always kept, and nothing (percentages, tests, intervals) is recomputed.
* Optional Kish effective sample size for weighted numeric (mean) confidence intervals /
  significance, via `options("tabxplor.kish_neff" = TRUE)`. Off by default (weighted estimate with
  the unweighted count, as before).
* **Mean (numeric) columns now get a whole-table significance test** — a one-way ANOVA, the
  counterpart of the Chi-squared test for factor columns. Both **Welch's F** (default, robust to
  unequal group variances) and the classic pooled F are computed; `options("tabxplor.anova")`
  (`"welch"` / `"classic"`) chooses which p-value is shown. A p-value row now appears under mean
  columns as it already did under factor columns.
* **`tab()` is now the unified entry point** and accepts **several** `row_vars` and `col_vars`
  (e.g. `tab(data, c(race, relig), marital)`). With several `row_vars` the mirror tables are
  **merged into one** by default; the new `output_list = TRUE` returns a list of one table per
  `row_var` instead. `tab_many()` still works and keeps its historical list return (it is now a
  soft-deprecated alias of `tab()`).
* **`levels`** in `tab()` (`"all"` / `"first"` / `"auto"`, per `col_var`) --- controls which levels
  of each column variable are kept, restoring the compact "keep only the first level of each column
  variable" summary tables. Replaces the (now soft-deprecated) `sup_cols` argument.
* **`na` gains `"common_base"` and `"drop_all"`** in `tab()`. `"common_base"` fixes a single
  population (observations non-missing on the `row_vars` and the *first* `col_vars`, plus
  `tab_vars`) shared by every column, while secondary `col_vars` keep their own `NA`'s as a level
  within it --- reproducing the historical `tab()` behaviour. `"drop_all"` drops every observation
  missing on the `row_vars`, *any* `col_vars` or a `tab_vars` (all columns then share one base).
  `na = "drop"` now correctly drops each column's own `NA` (so bases can differ between columns).
  Available from microdata only.
* **`spread_vars`** in `tab()` --- pivot a subset of `tab_vars` into columns (via
  `tab_spread()`), with optional `names_prefix` / `names_sort`.
* **Per-column-variable reference under `pct = "col"`.** A `ref` vector *named by column variable*
  (e.g. `tab(data, x, c(race, relig), pct = "col", ref = c(race = "Black", relig = "None"))`) now
  gives each column variable its own reference column, instead of a single reference shared by all.
  A chosen level is matched by exact equality, so labels containing regular-expression characters
  (e.g. `"$25000 or more"`) work as references.

## Internal
* The jamovi module (`jmvtab`) gained several user-facing features: a **reference-level picker** (choose the comparison level of each variable from a compact Material list, with "Total" as the visible default; it covers the row variables under row percentages and the column variables under column percentages, follows the level-reordering panel, and shows a second-reference section only when odds ratios are requested); **export to Excel, HTML or Markdown** (pick a format, the button label follows, and the file is written to a typed path defaulting to your Documents folder, with a confirmation notice); an **`n_min`** control to hide small-base rows/columns; a **Wald** option for the cell confidence interval; and a clearer **statistical-test** toggle (Chi-square for categorical columns, ANOVA F for numeric ones) with a Welch-vs-classic ANOVA choice.
* The jamovi module (`jmvtab`) UI is now consistent with what the analysis actually computes: options that have no effect given the others are greyed out (e.g. the total-table and comparison-table choices when there are no table variables; the significance-stars and difference-CI method when cell intervals are chosen; the significance policy when colors are off; the count/percentage extras when there are no percentages), always keeping their value so it returns when they become relevant again. The number-of-digits control is now a dropdown, and the legend/path text boxes fill their row. The significance policy and the confidence interval are no longer wired to fight each other — choosing "grey non-significant" simply colors accordingly (the needed interval is computed automatically), and never silently changes the CI setting.
* The jamovi module (`jmvtab`) now uses a live multi-tier cache: after the first table, changing an option (percentages, reference, colors, display, adding a variable) reuses the cached counts and chi-squared/ANOVA instead of recomputing everything, so results update near-instantly on normal survey data. The Jamovi HTML render also drops the per-cell hover tooltips (inert in Jamovi and roughly half the render time). The module drives the same `tab()` pipeline with the cache injected (no separate code path), so its tables stay identical to `tab()`. Beyond the counts/tests, changing only the **display or colours** (number of digits, the displayed value, the colour measure `"diff"`/`"ratio"`, or the `color_signif` significance policy) now reuses the already-built table and only re-paints it, skipping the whole cell rebuild — these toggles are effectively instant even on a big table-of-tables (e.g. a colour change on a 9-table grid dropped from ~1.1 s to ~0.04–0.19 s). Building `tab()` / `tab_num()` tables is also a little faster overall (the per-cell format assembly hoists its constant work out of the inner loop).
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
* **The unweighted-count `add_n` and the `add_pct` distribution are now display-time additions.** With
  `add_n = TRUE` (the default), the base count no longer sits in a separate `n` column of the built
  table: on the console, `tab_kable()` and `tab_md()` it now appears **inside the Total cell** as
  `100% (n=1120)`; `tab_xl()` still writes a separate numeric `n` column. Likewise `add_pct` is drawn
  only when the table is displayed/exported. The built object therefore no longer contains the `n` /
  `col_pct` columns (nor the `pct = "col"` `n` / `row_pct` rows). Old code reading `tabs$n`,
  `tabs[["n"]]` or `pull(tabs, "n")` still works — the column is reconstructed from the Total column
  with a one-time deprecation message — but will stop being reconstructed in a future version; prefer
  the displayed/exported table, or `get_n()` on the `Total` column. A global option
  `options(tabxplor.totcol_range = "range")` (or `"min"`) makes the in-cell base show the cross-column
  base range `[min;max]` when a table's column variables have different bases.
* **Chi-squared / ANOVA p-values are now a display-time addition.** The table built by `tab()` keeps the
  test results (its `test` attribute) but no longer contains the p-value *rows* themselves; they are
  drawn when the table is displayed or exported. In the R **console** the p-values now appear as a
  compact test line above the table (e.g. `# race: Chi2=997 (df=10) p=…`), while `tab_kable()`,
  `tab_md()`, `tab_xl()` and jamovi still render them as p-value **rows** exactly as before. Code that
  read the p-value rows out of the built object (they had an empty count) will no longer find them; use
  the `test` attribute (`get_test()`), or `tab_pvalue_lines()` to materialize the rows on demand.
* `tab(na = "drop")` with **several `col_vars`** now drops each column variable's own missing
  values (bases can differ between columns), matching its documentation and `tab_many()`. It
  previously dropped every observation missing on *any* column variable, giving one shared base ---
  that behaviour is now the explicit `na = "drop_all"`. Single-`col_var` tables are unaffected.
* For **numeric (mean) columns**, the `diff` field is now a real **difference** (`cell_mean -
  ref_mean`); the cell/reference **ratio** (the old numeric-`diff` value) moved to the `ratio`
  field. Code reading `$diff` on mean columns now gets a difference — use `$ratio` for the ratio.
  Percentage-column `diff` is unchanged. Cell coloring is unchanged (`color = "diff"` on mean
  columns still colors the ratio for now).
* `tab_xl()` now derives its Excel number formats from `format()` (the same source of truth as the
  console and the other exporters), instead of a separate internal routine. Practically identical for
  the usual percentage / count / mean tables, but it **fixes two cases where the Excel display used to
  disagree with the console**: a difference shown on a percentage column now formats as a percentage,
  and p-value cells keep their percentage scaling. Number-of-decimals for count and odds-ratio columns
  also follow the console exactly now.
* **Excel export now uses `openxlsx2` instead of `openxlsx`** (a Suggests-only dependency). If you
  export to Excel, install `openxlsx2`. The produced workbooks look essentially the same.

## Bug corrections
* `tab()` with two or more row variables AND two or more column variables no longer errors ("pct can't be recycled"); percentages are recycled correctly across the table.
* A reference level whose label contains regular-expression characters (e.g. `"$25000 or more"`) is
  now matched exactly, so it correctly selects its row/column (it was silently ignored before). A
  reference vector named for a single variable (e.g. `c(race = "Black")`) no longer leaks that level
  to the other variables. Confidence intervals for a difference now use the same reference column as
  the difference itself.
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
* The combined `color` strings `"diff_ci"`, `"after_ci"` and `"ci"` are soft-deprecated: use
  `color = "diff"` with the new `color_signif` argument (`"grey_non_signif"` for `"diff_ci"`,
  `"color_all_signif"` for `"after_ci"`/`"ci"`). They still work unchanged.
* `get_color_breaks()` now returns the canonical named list of positive-only scales
  (`pct_diff`, `pct_ratio`, `mean_diff`, `mean_ratio`, `contrib`) --- the same shape
  `set_color_breaks()` accepts, so it round-trips. Pass `type = "all"` for the mirrored
  (signed) thresholds. This changes its return shape from the previous flat vectors.
* `tab_many()` is **soft-deprecated** in favour of `tab()` (which now takes several `row_vars` /
  `col_vars`). It keeps working and keeps its historical list return for several `row_vars`
  (`tab()` merges them by default; use `output_list = TRUE` for a list).
* Singular `row_var` / `col_var` in `tab()` are **soft-deprecated** aliases of the plural
  `row_vars` / `col_vars` (which now accept several variables). They still work.
* `tab(sup_cols =)` is **soft-deprecated**: pass those columns in `col_vars` and set
  `levels = "first"` (`col_vars` already accepts several variables). It still works.
* `tab_many(totrow =)` and `tab_many(totcol =)` are **soft-deprecated**: a total row is always
  computed and exactly one total column is shown by default; drop/move them afterwards with dplyr
  (`dplyr::filter(!is_totrow(.))`). Old `totcol` values (`"each"`, `"no"`, names) still work.
* The `tabxplor.compact` **option is removed**, superseded by the `output_list` argument of
  `tab()`. `tab_many(compact =)` still works.
* `tab_pct()`, `tab_tot()` and `tab_totaltab()` are **superseded**: percentages, differences and
  totals are computed directly by `tab()` / `tab_plain()` / `tab_num()`. They still work on an
  existing table.
* `tab_plot()` is **superseded**: its ggplot rendering is limited and no longer actively developed.
  It keeps working; prefer `tab_kable()` (HTML), `tab_md()` (markdown) or `tab_xl()` (Excel).
* `tab_xl(n_min =)` and `tab_xl(hide_near_zero =)` are **soft-deprecated** and now inert (they no
  longer grey out small-n / near-zero cells). For the small-n case use `tab(n_min = )`, which blanks
  or drops small-n cells at display and flows into the Excel export. Both arguments still accept their
  old values without error (a message is shown when a non-default value is passed).
* `tab_md(title =)` is **soft-deprecated**, renamed to `tab_md(caption =)` (a single caption name
  shared by every exporter). The old argument still works.
* `tab_xl(print_color_legend =)` is **soft-deprecated**, renamed to `tab_xl(color_legend =)` (the name
  the other exporters use). The old argument still works.

## Bug corrections (Phase 6)
* Fixed a crash in `tab_num(<tab_vars>, ci = "cell")` (and thus in `tab()` / the Jamovi module
  with numeric columns, confidence intervals and subtables): the grand-total-only path built an
  empty total block and failed reordering by the tab variable.

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
