
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
* **A black-and-white publication palette.** `theme = "print"` (on `tab_html()` / `tab_md()` /
  `tab_xl()` / `tab_export()` / `tab_css()`) renders the colour measures typographically — bold for
  over-represented cells, italic for under-represented ones, an underline for the strongest threshold,
  a grey fill for a second measure — because a greyscale print turns the two colour directions into the
  same shades of grey. It reaches Excel as real font attributes, and is written as `<b>`/`<i>`/`<u>`
  markup as well as CSS, so it survives a paste into Word. Every stylesheet also carries it in an
  `@media print` block, so a coloured html table already prints publication-ready
  (`options(tabxplor.print_rules = FALSE)` to opt out).
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
* **Effect sizes and Fisher's exact.** `test = TRUE` now carries Cramér's V / phi or eta²; a small
  sparse table uses Fisher's exact.
* **Survey designs.** Pass a `survey::svydesign()` as `data` and the whole table follows it: the
  estimates, the Chi2 / ANOVA F p-values, and **every confidence interval, star and colour threshold**
  (strata, clusters, `fpc` and calibration alike — so a design can also make an interval *narrower*).
  This now includes `tab_reg(empirical = TRUE)`'s observed (`Obs_*`) columns, so the model and its
  observed counterpart are finally compared under one inferential regime. Without a design,
  `options(tabxplor.kish_neff = TRUE)` still rescales the intervals and tests to Kish's effective
  sample size, which corrects for unequal weighting only. See `?tab` and `?tab_reg`.
* **Standardized residuals for `color = "contrib"`.** Which cells depart from independence is now
  answered with the **adjusted standardized residual** (Haberman — SPSS's "adjusted residual", R's
  `chisq.test()$stdres`), on the package's usual inference base (unweighted *n*, or Kish `n_eff`).
  `color_signif = "guaranteed_effect"` switches the colour to that residual on an absolute ±2 / ±3
  scale that means the same thing in every table, while the default keeps the correspondence-analysis
  reading (each cell's share of the table's chi-squared). The residual can also be printed
  (`display = "{pct} ({resid})"`) and appears in html tooltips. New `zscore` colour-break scale and
  `conf_level_to_z()` to write it in confidence levels.
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
* **Every regression table now checks itself.** The footer carries five model checks — **Linearity**
  (per continuous predictor), **Proportionality (Brant)**, **Dispersion (robust/model SE)**,
  **Influence (max dfbetas)** and **Collinearity (max VIF)** — computed for every model, with no
  argument to remember and one row per model column so a comparison reads down. They matter: on the
  model used throughout the regression vignette, letting `age` curve moves the top income category's
  odds ratio by a quarter and flips another income level's verdict, and nothing in the table used to
  say so. Any of them can be dropped through `stats =`; `stats = "collinearity"` needs the new
  suggested package `car`. The per-predictor overall-association test (`stats = "global"`) moved from
  a footer sentence to footer rows for the same reason.
* **Colour the gap between the modelled and the observed effect.** `tab_reg(empirical = TRUE)` already
  prints the crude effect beside the adjusted one; `color = c("OR", "adjustment")` now colours *how far
  apart they are*, so a whole table of "what did adjusting change?" reads at a glance. With
  `split_var`, `color = "between_groups"` does the same against the first group (effect modification,
  row by row). `color_signif` applies to both, so a gap can be greyed when it is no bigger than chance,
  and the html tooltip gives its confidence interval and p-value. The gap is also printable
  (`display = "{or} (obs {obs})"`). Part of an **odds-ratio** gap is non-collapsibility rather than
  confounding, so there the colours stay descriptive and `tab_reg()` says so once: use marginal effects
  (`effect = "ame"` / `"ame_ratio"`) or risk ratios (`family = "poisson"`) for a comparison the test can
  read.
* **Every outcome now has an observed counterpart.** `tab_reg(empirical = TRUE)` used to go quiet on
  three families. A **summed score** (`trials =`) now shows its mean score plus the odds ratio of the
  summed items; an **ordinal** outcome shows `Obs_cumOR`, the cumulative odds ratio of the same model
  with one predictor; a **multinomial** outcome would need one crude column per category, so its
  observed effect is folded into the model cell instead — `2.31 (obs 2.05)`. `color = "adjustment"`
  therefore works everywhere, and on the marginal paths (`effect = "ame"` / `"ame_ratio"`) of a 3+ level
  outcome the gap now carries a real significance test. One rule covers all of it: *the observed effect
  is the model's own effect, fitted with a single predictor*.
* **`tab(OR = "cumOR")`** — the descriptive twin of that ordinal model: one **cumulative odds ratio per
  cut point** ("at or below level j") for an `ordered` col_var, with no proportional-odds assumption.
  The spread of the odds ratios across a row *is* the departure from proportional odds.
* **`ordered` factors now survive `tab()`.** They used to be silently stripped to plain factors. Note
  that the synthetic `Total` / `Ensemble` / `NA` levels are appended after the real ones, so on an
  ordered grouping column they compare as the greatest levels — they are labels, not scale points.
* **Observed effects for continuous predictors too.** `tab_reg(empirical = TRUE)` used to leave every
  continuous predictor blank — often the rows where adjustment bites hardest. They now carry their
  observed (univariable) effect, on the model's own scale, so `color = "adjustment"` and its
  significance test work there as well. One rule now covers every predictor: the `Obs_*` columns show
  the **observed, unadjusted (univariable)** effect.
* **Continuous predictors are now scaled per standard deviation by default** (`multiplier = "sd"`).
  Per one unit their effect is usually too small to read or to colour — a year of age barely moves an
  odds ratio, a whole standard deviation multiplies it by 0.66. The row label names the unit
  (`age (per 1 SD (13.5))`). `multiplier` accepts a single value for all continuous predictors or a
  named vector overriding some (`"sd"`, `"2sd"`, or a number of units); **`multiplier = 1` restores the
  per-one-unit reading**, which is what you want when comparing a cell against `exp(coef(glm(...)))`.
* **Does this predictor act differently between subgroups?** With `split_var`,
  `stats = c(..., "interaction")` adds one aggregated test per predictor to the footer — the classic
  effect-modification test, asked once for all a predictor's levels, so it carries none of the
  multiplicity of a per-cell reading. `color = "between_groups"` turns it on for you.
* **Risk ratios, two ways.** With a common outcome an odds ratio is not a "times more likely", and it
  cannot be compared across nested models. `tab_reg(effect = "ame_ratio")` reports the **marginal risk
  ratio** from the usual logistic fit (with the adjusted probability in parentheses), and
  `tab_reg(family = "poisson")` on a **binary** outcome fits a **modified Poisson** regression (robust
  standard errors) whose coefficients are risk ratios. Both are opt-in — a binary outcome still defaults
  to logistic — and `empirical = TRUE` gives the matching crude `Obs_RR`.
* **Regression tables now show the numbers behind the estimates.** An `n` column gives each predictor
  level its unadjusted count (`add_n = FALSE` to drop it), and the footer answers "is this variable
  associated with the outcome at all?" with one overall test per multi-level predictor (it costs no
  extra model fit; `stats = FALSE` or an explicit `stats =` vector opts out).
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

* **`tab_reg(stats = "dispersion")` now names the model check, not the Pearson dispersion.** The exact
  Pearson dispersion of a count model keeps its footer row under `stats = "phi"`, and it is now correct
  for weighted models too (it used to divide by a survey design's degrees of freedom, reading about 20
  where it should read about 1).
* **`tab_reg()` fits every model of an outcome on the same people, by default.** The new
  `na = "drop_by_outcome"` shares one complete-case population across the models of a given outcome
  (`"drop_by_model"` restores the per-model drop, `"drop_all"` shares one across the whole call). This
  is what makes the observed (`empirical = TRUE`) columns comparable to the model beside them, and it
  lets the likelihood-ratio comparison run where it used to degrade to an AIC difference; it also
  changes N, and therefore the estimates, when compared models have different missingness. Where the
  populations still differ, no observed effect is attached at all — a coloured "gap" would be listwise
  deletion rather than adjustment.
* **`tab_reg(family = "auto")` reads an integer-valued outcome as gaussian** instead of refusing to
  guess, so age in years, a summed score or income in whole units no longer need an explicit family
  (the message names `"poisson"` for a genuine count).
* **The `color = "adjustment"` / `"between_groups"` thresholds now follow the estimate's own scale**:
  a difference in the outcome's own units is compared in standard deviations of that outcome
  (`adj_diff_std`), so the same model on an outcome recorded in hours, minutes or days reads the same
  way. Their break labels are signed (`+2`, `-5`) on an additive scale instead of `×0.02`.
* **`conf_level` now reaches the gap greying** as well as the printed intervals and the stars.
* **A weighted table's whole-table test and effect size are now computed on the weighted table.**
  Every other figure beside them already was: the confidence intervals are `Wilson(weighted %,
  unweighted n)` and the mean F uses the weighted group moments. Only the chi-squared and Cramér's V
  were still fully unweighted, so a weighted table reported a p-value and an effect size describing a
  population you had not asked about. Unweighted tables are unchanged. Fisher's exact test is skipped
  when weights are used (an exact test counts whole observations).
* **Survey designs: `tab(ids =, strata =, fpc =, nest =)` and the same arguments on `tab_reg()` /
  `tab_logit()` / `multi_logit()` are removed**, together with `test = "survey"`. They reached the
  whole-table p-value and nothing else. Build the design once with `survey::svydesign()` and pass it as
  `data` instead — it says everything those arguments did, plus calibration:
  `tab(svydesign(ids = ~psu, strata = ~region, weights = ~w, data = d), x, y, test = TRUE)`.
  `test` is now simply `TRUE` / `FALSE`: the **kind** of test follows what you passed — weights, or
  weights plus `options(tabxplor.kish_neff = TRUE)`, or a design. Replicate-weight (`svrepdesign`) and
  two-phase designs are refused with a message rather than failing obscurely.
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
* **`color = "contrib"` with a `color_signif` policy** now tests the adjusted standardized residual
  rather than the Pearson one, so more cells are (correctly) flagged as significant; with weights it
  uses the unweighted *n* rather than the sum of weights, which previously made every cell "significant"
  as soon as weights carried population scale. `color_signif = "guaranteed_effect"` also changes what it
  colours (the residual, on the new absolute `zscore` scale). The default
  `color_signif = "ignore"` — the correspondence-analysis reading — is unchanged.
* **`tab_reg()` reports a continuous predictor's effect per standard deviation by default**
  (`multiplier = "sd"`, see above). Pass `multiplier = 1` for the previous per-one-unit reading.

## Bug fixes

* A factor carrying **`NA` as a real level** (`factor(..., exclude = NULL)`, common in imported data) no
  longer crashes `print()` / `format()` / any export.
* `tab()` accepts a **`data.table`** as input, and a **logical `col_var`**.
* **Clearer errors** for an unknown named `ref`, a variable used as both a tab and a row/column variable,
  and an all-zero / all-`NA` weight.
* The **`lang` argument now works on Linux** (`lang = "fr"` used to return an English legend).
* In `tab_reg()`, a **logical predictor** rendered as an empty row, and the `Constant` row lost its bold
  when `empirical = TRUE`.
* `color = TRUE` with `OR = TRUE` and **two or more factor `col_vars`** silently coloured on the
  difference instead of the odds ratio.
* HTML **tooltips no longer repeat what the cell already prints**: a composite cell (`"{pct} (n={n})"`,
  or an average-marginal-effect cell) used to show its own bracket again on hover.
* **`tab_reg()` on a survey design now weights everything it should.** The observed (`empirical = TRUE`)
  columns were computed *unweighted* beside a design-weighted model column — the one comparison the
  feature exists for, made on two different populations — and `effect = "ame"` returned a
  sample-average instead of a population-average marginal effect (a 13% error in our test case). The
  per-standard-deviation scaling of numeric predictors and the model-vs-observed gap test were
  unweighted too, and the footer never said the table was weighted at all.
* **A calibrated survey design** (`survey::calibrate()` / post-stratified) no longer errors in
  `tab_reg()` as soon as any row has a missing value. It also no longer loses the model-vs-observed
  gap test (`color = "adjustment"`) on such a table, and `effect = "ame"` no longer returned a wrong
  gap standard error there.
* **`tab_reg(split_var = )` on a survey design** errored outright whenever the groups had unequal
  sizes, and on a *calibrated* design it silently fitted each group with the wrong respondents'
  weights (up to 38% off in our test case).
* **The design-based p-value now describes the table you see**: it was computed on the design's
  original data, ignoring `filter =`, rare-level lumping (`other_if_less_than`) and `cleannames`
  relabelling, so a lumped table could report the p-value of the unlumped one.
* `tab_num()`, `tab_plain()` and `tab_many()` **accept a survey design** as `data` (only `tab()` and
  `tab_reg()` did); `tab_counts()` explains why it cannot.

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
* `ids` / `strata` / `fpc` / `nest` on `tab()`, `tab_reg()`, `tab_logit()` and `multi_logit()`, and
  `test = "survey"` (pass a `survey::svydesign()` as `data` — see above).


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
