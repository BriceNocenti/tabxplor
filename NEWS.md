
# tabxplor 1.4.0 (in development)

## New features
* **Compact legends in exports, and a fuller plot footer.** A new option
  `options(tabxplor.legend_style = "terse")` renders the compact one-line colour legend (the form the
  console uses) in Markdown / HTML / Excel exports, instead of the default full sentences (`"prose"`).
  `tab_plot()` now draws the *same* footer as the other exports below the plot -- the "Weighted by ...",
  "Model: ..." and significance-stars lines and the user subtext, not only the colour legend -- and
  draws its `caption` (a regression plot auto-captions itself), both of which it previously ignored.
* **Clearer odds ratios and a dedicated OR colour scale.** For a **binary** factor, `OR = TRUE` now
  shows an odds ratio for *both* levels (each versus the other, reciprocals of one another) instead of
  forcing the first level to a meaningless `1`; both carry a Woolf confidence interval. `ref2` is now
  only needed for factors with 3+ levels. Odds ratios get their own symmetric colour scale
  `odds_ratio` (`set_color_breaks(odds_ratio = ...)`), so `pct_ratio` is free to be set asymmetrically
  (e.g. the "×2 rule") without changing OR colours. Because odds ratios don't sum to 100%, an OR
  table's total column drops its "100%" and shows only the base `n` (nothing when `add_n = FALSE`).
  A `color_signif = "guaranteed_effect"` colour channel whose scale has a single break (which would
  paint every significant cell one flat shade) is now disabled along with its legend, unless it is the
  only channel.
* **Readable summary statistics.** The chi-squared / ANOVA tests of a crosstab (`test = TRUE`) and the
  model-fit statistics of a `tab_reg()` regression now print above the table as a clean, aligned
  summary table (column variables in columns, row variables / sub-tables / `split_var` groups in rows)
  instead of dense unformatted lines. Non-significant p-values (≥ 5%) are shown in red, and a chi-squared
  whose smallest expected count is below 5 is flagged `!` (its result may be unreliable). In exports
  (Markdown / HTML / Excel) a weak chi-squared carries the same `!`; the new option
  `options(tabxplor.test_lines = "stat")` adds a test-statistic row above the p-value row. A regression
  `split_var` (the population each model was fit on) now appears in HTML and Excel as a merged, vertical
  first column, so exported split models are self-documenting.
* **Clearer, de-duplicated colour legends and table footers.** When several columns share the same
  colour scale — most visibly a `tab_reg(empirical = TRUE)` table's crude (`Emp.`) and modelised
  (`Model`) columns — they now share a **single** legend line, prefixed by the columns it covers
  (e.g. `Emp. OR, Model OR — …`, the first six then `… +N vars`). The `color = "contrib"` legend now
  reads correctly ("×N the **mean contribution**" on *both* sides, no longer a misleading `÷`); a
  custom `color_breaks = list(mean_diff = …)` scale is described as a raw difference instead of wrongly
  saying "standardized … SD"; and legend column names no longer break mid-word in HTML. A new
  **significance-stars legend** ("\*\*\*: …at the 99% confidence level; …") is printed whenever a table
  shows stars, and `color = "contrib"` no longer prints stars it never opted into. When a table is
  weighted, its footer now opens with **"Weighted by `<wt>`."**. For a `tab_reg(empirical = TRUE)`
  logistic table, the crude risk-difference companion now uses the two-proportion **Wald** interval
  (matching the model's marginal-effect interval), so the shared legend names one honest method.
* **New jamovi "Regressions" analysis (`jmvtabreg`).** A live, user-friendly jamovi UI for `tab_reg()`
  covering every family (linear / logistic / Poisson / multinomial / ordinal), multiple outcomes,
  survey weights (with an advanced cluster / strata / fpc panel), a per-predictor reference-level picker,
  the crude-vs-model (`empirical`) companion columns, and Excel / HTML / Markdown export. Changing the
  display, colours or a predictor's reference level re-renders instantly by reusing the fitted model
  (references are recomputed without refitting). A **model-comparison builder** lets you add several
  named models by ticking which predictors each one includes, with a comparison test (versus a baseline
  or the previous model), plus per-predictor **effect scaling** (odds ratio / coefficient per *k* units)
  and grouped / summed-score **binomial trials** — reusing already-fitted models so only new ones are fit.
* **Confidence intervals for ratios and richer CI methods.** `color = "ratio"` on a numeric mean now
  produces a real **ratio-of-means** confidence interval (it used to silently show the *difference*
  interval). Three new arguments choose the interval method, each reproducing the matching regression:
  `method_mean_diff` (`"welch"` default, or `"student"` = a linear-regression coefficient interval);
  `method_mean_ratio` (`"robust"` default, `"quasipoisson"` = a quasi-Poisson regression, or naive
  `"poisson"`); and `method_ratio` for the proportion ratio (`"katz"`). Each CI's significance stars come
  from the same interval, and `tab_color_legend()` now names the method used. Numeric mean intervals are
  now Student-t throughout (they estimate the variance), matching `t.test()` / `lm()`; on large samples
  this is indistinguishable from before.
* **`tab_reg()` handles over-dispersion.** An unweighted Poisson or grouped/summed-score binomial model
  now reports dispersion-scaled standard errors by default (matching a quasi-Poisson / quasi-binomial
  fit) while keeping the AIC / McFadden / LR / BIC footer — so over-dispersed counts get honest,
  wider confidence intervals automatically.
* **Crude (`empirical`) columns get confidence intervals.** With `empirical = TRUE`, the descriptive
  "Emp." companion columns now carry a confidence interval, significance stars and significance-based
  colour, computed with the same method as the model — so crude and adjusted effects are directly
  comparable. The multinomial crude tooltip gains its intervals too. Every crude quantity is now computed
  on **exactly the same complete-case population as the model** (listwise-complete on the outcome, all
  predictors and any design variable), so the crude-vs-adjusted comparison is never confounded by
  differing missingness.
* **`effect = "ame"` shows a genuine adjusted percentage.** The predicted probability in parentheses beside
  each average marginal effect is now a *marginal-standardized* prediction (`avg_predictions(variables=)`),
  which is truly covariate-adjusted and coheres with the effect (adjusted-%(reference) + AME = adjusted-%
  (level)). The header names it `(adjusted %)`. A new optional `predicted_unadjusted = TRUE` (binomial AME)
  adds a `Model % (unadj.)` control column showing the model's unadjusted predicted % (which equals the
  crude `Emp. %` on the same population — a built-in cross-check).
* **`color_signif` and significance stars now work on empirical odds ratios in crosstabs.**
  `tab(..., color = "OR")` gained a confidence interval (Woolf log-odds-ratio), so
  `color_signif = "grey_non_signif"` / `"guaranteed_effect"` grey out or highlight odds-ratio cells by
  significance, and `stars = TRUE` shows stars — exactly as they already did for differences and ratios.
  For a factor with 3 or more levels the odds ratio compares each level to the `ref2` baseline (the crude
  counterpart of a multinomial level-vs-baseline odds ratio). Tables without a colour policy are unchanged.
* **`tab_reg()` tables are clearer to read.** Regression tables now title themselves ("Logistic
  regression: `<outcome>` by `<predictors>`", one sheet name per model in Excel), print a plain
  "Model: …" description **above** the colour legend, and their headers are less repetitive: a model
  column and its `empirical` companions sit under one outcome header ("`<outcome>: <level>`") with no
  border between them, the model column reads "Model OR" / "Model β" / …, and a multinomial table drops
  the repeated ": OR"/": AME" from each category column. Colour-legend wording is now model-accurate —
  a marginal-effect (AME) table compares to "the reference category" (not "the Total row"), and a
  Poisson rate is described as a rate-ratio (not an odds-ratio).
* **Variable names are written once, and you choose which ones.** A table built from several
  `row_vars` used to repeat the row-variable name on every single row, under a meaningless `row_var`
  header. Every exporter now names each block **once**: `tab_kable()` spans one cell over the block and
  writes the name vertically (so a long name costs no column width), `tab_xl()` merges and rotates it
  90 degrees, `tab_md()` writes it in italics, and the `row_var` header is gone. A kept `tab_vars`
  column is merged the same way, but never rotated -- its values are levels you read, not a name. The
  new **`var_names`** argument (`"both"` by default, `"rows"`, `"cols"` or `"none"`; also
  `options("tabxplor.var_names")`) picks which variable names to write, on `tab_kable()` / `tab_md()` /
  `tab_xl()` / `tab_plot()` / `tab_export()`. Level headers always keep their name.
* **One Total row for several variables.** A table built from several `row_vars` used to repeat an
  identical `Total` row once per variable. When those totals render identically (the usual case --
  `na = "keep"`, `"drop_all"` or `"common_base"` share one population), tables now show a **single**
  Total row; only `na = "drop"`, which can make the bases genuinely differ, keeps every Total (with a
  message). The p-value row of such a table is now also placed **once per variable** (each variable's
  own chi-squared), instead of collapsing into a single mis-placed row.
* **Redesigned colours & breaks API.** The `color` argument now has a simple grammar: **position
  picks the visual channel** (1st value -> text, 2nd -> background) and **names pick the column type**
  (`pct` / `mean`). So `color = c("diff", "ratio")` colours the text by the difference and the
  background by the ratio; `color = c(pct = "diff", mean = "ratio")` colours factors by difference and
  numeric means by ratio; `color = list(pct = c("diff", "ratio"), mean = "ratio")` combines both.
  `color = TRUE` is the smart per-type default. The significance policy `"color_all_signif"` is
  renamed **`"guaranteed_effect"`**. Colour thresholds accept **signed / reciprocal literals** (a
  one-sided vector auto-mirrors; a two-sided one is used as-is) and a `list(over =, under =)` escape
  hatch for asymmetric scales -- e.g. `pct_ratio = list(over = 2)` is the "only x2" rule (the new
  factor default). A new per-table `color_breaks =` argument on `tab()` overrides the global thresholds
  for one table. New OKLCH light/dark, text/background palettes are the default (customise with the new
  `set_color_palette()`); console output is 24-bit truecolor (falling back to an 8-bit palette only in
  the RStudio console). `set_color_style()` (and its `custom_palette`/`html_24_bit` machinery) is
  replaced by `set_color_palette()`; the export functions keep an inert `html_24_bit` argument.
* **A new HTML table engine, and it is now the default.** `tab_kable()` (and `tab_export()`) render
  with tabxplor's own dependency-free engine instead of kableExtra: a self-contained `<table>` plus one
  stylesheet, about 3x faster and much smaller, and the only engine that can follow `theme = "auto"`.
  Its output opens in the Viewer and knits like kableExtra's. The look is **restyleable**: the table's
  geometry is CSS classes rather than inline styles, so your own CSS can override any of it -- no
  `!important` needed. It uses DejaVu Sans Condensed for text and DejaVu Sans for numbers -- switching
  numbers to a **monospace** font (matching `tab_xl()`) only when the table shows significance stars, so
  the stars line up -- hugs background colours around the text with rounded corners rather than flooding
  the cell, and highlights the hovered row. `options(tabxplor.tab_kable_engine = "kableExtra")` (or
  `tab_kable(engine = "kableExtra")`) restores the previous renderer.
* **The console follows your editor's theme.** `set_color_palette(theme = "auto")` detects whether
  your console is light or dark and picks the matching palette; it is also what tabxplor does on load,
  so a dark editor gets dark-suited colours with no setting. It reads RStudio's theme, **Positron's**
  theme (which no R package has been able to detect --- `rstudioapi::getThemeInfo()` is still
  unsupported there), or the terminal's `COLORFGBG`. Best-effort by nature: anything it cannot
  establish stays `"light"`, and it never warns. Resolved once, so call it again after switching
  themes. (Exports are unaffected: their `theme = "auto"` follows the *reader's* browser.)
* **Dark mode.** `theme` gains **`"auto"`** on `tab_kable()` / `tab_md()` / `tab_css()` /
  `tab_export()`: the table follows whoever is **reading** it, flipping live as they switch. In a file
  or a knitted document that means the reader's browser -- their operating system, plus any dark-mode
  toggle of the page it is embedded in (Quarto, Bootstrap 5.3, Tailwind). Printed to the **Viewer** it
  means your editor: a Viewer webview reports the *operating system* rather than the editor's colour
  theme, so tabxplor resolves the theme in R instead (RStudio's or Positron's), and the page around the
  table is painted to match -- a `theme = "dark"` table no longer sits in a white pane. Dark tables are
  `#CECDC3` on `#222222`. `theme` stays **`"light"` by default and `"auto"` is opt-in**: unlike the
  console, an export is read who-knows-where, so a dark table is always a deliberate choice. Set the
  default globally with `options(tabxplor.theme = "auto")` (or `"dark"`). `"auto"` needs a stylesheet,
  so it applies to `tab_kable(engine = "html")` and `tab_md()`; the static backends (`tab_xl()`,
  `tab_plot()`, and the kableExtra engine, whose themes are baked at render time) resolve it to
  `"light"`. In a document with many tables, emit `tab_css(theme = "auto")` once at the top and set
  `options(tabxplor.kable_css = FALSE)` -- the stylesheet is the same for every table.
  *Note:* the html engine now sets the table's text, background and border colours explicitly. Light
  tables therefore gain a white background, and borders are one colour instead of inheriting each
  cell's colour (a `+20%` cell no longer has a coloured border).
* **A real confidence interval for the ratio.** When the ratio is the measure you display -- the text
  channel, i.e. `color = "ratio"` or `color = c("ratio", "diff")` on percentage columns -- the stored
  interval is now **Katz's log-risk-ratio interval**, on the ratio scale and centred on the ratio
  itself, instead of a difference interval converted after the fact with the reference held fixed.
  Significance stars, greying and `"guaranteed_effect"` thresholds all read it, and the legend names
  it. The interval belongs to the measure the reader sees: a second (background) channel derives from
  it, which is what the ratio channel has always done. **Nothing changes unless you ask for it** --
  `color = TRUE`, `"diff"` and `c("diff", "ratio")` keep the difference interval exactly as before.
  Percentage columns only: a mean keeps its difference interval, a ratio of means being a different
  problem (Fieller's theorem). Available directly as `tab_ci(ci_scale = "ratio")`.
* **Meaningful colour legends.** The colour legend below each table is now a readable sentence -- e.g.
  *"Shades of blue: cells >= the Total row +5; +10; +20; +30 points. ... Grey: not significantly
  different from the Total row (Newcombe score interval, 95% confidence)."* -- with each break-word
  shown in its own colour. It names the reference (row/column Total or the reference category), the
  thresholds, the significance policy and the exact confidence-interval method and level actually used.
  A **French translation** is included and selected automatically from the R/OS locale (English
  otherwise); a new `lang` argument (`"en"` / `"fr"`) on `tab_kable()` / `tab_md()` / `tab_xl()` /
  `tab_plot()` / `tab_export()` forces the language. Excel legends are now colour-coded (rich text),
  `tab_md()` gained a colour legend, and regression tables (`tab_reg()`) get correct wording
  (β with SD thresholds and its reference category, IRR vs OR on their own scales). The break-words
  are **bold** in every medium, matching the coloured numbers they describe -- and in HTML and
  Markdown so is every coloured cell, which `tab_css()` / `tab_md_css()` now style for you. Where the
  legend cannot fill a word (Excel rich text, `tab_plot()`), a background break-word is drawn in a
  darker variant of its fill instead of being unreadable on a white page (customise with
  `set_color_palette(bg_legend_colors =)`).
* **Exports & display polish.** HTML, Excel and Markdown exports now show the **column-variable name**
  in a spanning header above its level columns (contiguous same-variable columns merged into one cell),
  and the level names drop the disambiguating `_<variable>` suffix (e.g. `Other_race` -> `Other`). A
  multi-table result (from `tab(output_list = TRUE)`, several row variables with tab-variables, or
  `tab_many()`) is now a **`tabxplor_tabs`** list that prints like a single table (and `list |>
  tab_kable()` opens in the Viewer) while still behaving like a plain list. Composite displays such as
  `"{pct} (n={n})"`, and numeric `mean (sd)` cells, are padded so the numbers line up -- including a
  mean whose sd is missing, which used to slide out of line -- and only the first field stays bold in
  total / reference rows. Ratios now print with a multiply / divide sign (`x2`, `/2`); a colour = c("diff",
  "ratio") table's tooltip shows the ratio correctly. **Excel**: `ci = "cell"` intervals and odds
  ratios (with the `1/x` reciprocal) export as readable text; a new `tab_xl(or_numeric = TRUE)` keeps
  odds ratios as numbers; differences and contributions get an explicit `+`/`-` sign, ratios a leading
  `x`; each numeric variable exports a mean column plus a separate `<var>_sd` column. **Markdown**: a
  coloured `tab_md()` now renders cleanly in a Quarto / Bootstrap document -- it no longer inherits a
  black line under every row, its spacer columns collapse, and the rule under the variable-name row and
  the sub-table separators are thin borders instead of rows of dashes. A coloured table also carries the
  `::: {.tabxplor-tab}` wrapper even with `css = FALSE`, so a document-level `tab_css()` styles it; a
  plain uncoloured `tab_md()` is unchanged.
* New `tab_reg()` --- **regression tables** as color-coded `tabxplor` tables, over one engine with a
  `family` argument: linear coefficients (`"gaussian"`), odds ratios (`"binomial"`, logistic) or
  incidence-rate ratios (`"poisson"`), one row per predictor level grouped by predictor. Pass a
  character vector of `predictors` (one model; `dependent` may itself be a vector -> one column per
  outcome), or a **named list** of predictor sets (one column per model, for comparing
  specifications). Each cell carries its 95% confidence interval and p-value, so the table prints
  with significance stars, greys out non-significant effects, and exports to Excel / HTML / Markdown
  like any other table. Effect measures are exponentiated per family by default
  (`exponentiate = "nongaussian"`); odds/rate ratios below 1 show as `1/x`; linear coefficients are
  coloured by their standardized effect size. Survey weights (`wt =`, via `survey::svyglm`),
  per-variable reference levels (`reference = c(var = "level")`), and Wald vs profile-likelihood
  intervals (`method =`) are supported. `broom` and `survey` (and `MASS` for `method = "profile"`)
  are optional dependencies. A summed-score outcome (a count of "yes" out of a fixed number of items)
  is fit as a grouped binomial by passing `trials =` (the number of items). Power users can pass a
  model **formula** as `dependent` (e.g. `tab_reg(data, y ~ x1 + poly(x2, 2) + x1:x3)`) instead of a
  `predictors` vector.
* `tab_reg()` now also handles **3+ level categorical outcomes**. A nominal (unordered) outcome is fit
  as one multinomial logit (`family = "multinomial"`, `nnet::multinom`), giving one odds-ratio column
  per outcome category versus the reference (`reference = c(outcome = "level")` sets the baseline
  category). An ordered outcome is fit as a proportional-odds cumulative logit (`family = "ordinal"`,
  `MASS::polr`), giving one cumulative-odds-ratio column; the parallel-lines assumption is tested with
  the Brant test (install the `brant` package) and a warning is issued if it is violated. `family =
  "auto"` now detects these from the outcome type. `nnet` and `brant` are optional dependencies.
* `tab_reg()` gains an `effect = "ame"` mode: instead of the model coefficient, each cell shows the
  **average marginal effect** with the adjusted **predicted probability** in parentheses (e.g.
  `-8%*** (16%)`) --- a probability-scale, cross-model-comparable interpretation. It works for
  logistic / multinomial / ordinal (percentage points, one column per outcome category), poisson
  (expected-count change) and gaussian (the coefficient) outcomes, and honours survey weights. Needs
  the `marginaleffects` package (a new optional dependency).
* `tab_reg()` gains an `at = "reference"` option (needs `marginaleffects`): evaluate at a **reference
  profile** (every other predictor held at its reference level / mean) instead of averaging. With
  `effect = "ame"` this gives the marginal effect *at reference* (the column reads "MER"); with a
  **multinomial** `effect = "coefficient"` it gives the odds ratio of each outcome category *versus the
  rest* at that profile (one column per category).
* `tab_logit()` and `multi_logit()` are now thin wrappers of `tab_reg()` for the binomial family,
  keeping the curated binary-outcome interface (`tab_logit(data, dependent, predictors)` for one
  logit per dependent; `multi_logit(data, dependent, models = list(...))` for model comparison).
* `tab_reg()` tables now show a **model-summary footer** (below the coefficients): the number of
  observations, a likelihood-ratio test versus the null model, McFadden's pseudo-R square and AIC/BIC
  for logistic / poisson / multinomial / ordinal models; the R square, adjusted R square, overall
  F-test and residual SD for linear models; and a Pearson-dispersion flag (with a warning) for poisson
  and grouped-binomial models. The `stats =` argument picks and orders the statistics, or hides the
  footer (`stats = FALSE`). Weighted models show a survey-appropriate reduced set.
* `tab_reg()` / `multi_logit()` gain a `compare =` argument for **model comparison**: `"baseline"`
  tests each model against a chosen `baseline` column, `"sequential"` against the previous model
  (likelihood-ratio test, F for linear models; an AIC difference with a message when the models are
  not nested or fit on different numbers of observations).
* `tab_reg()` (and `tab_logit()` / `multi_logit()`) gain full **survey-design** support: pass a weight
  column with `wt =` plus optional `ids =` / `strata =` / `fpc =` / `nest =` for clustered / stratified
  designs, or pass a **prebuilt `survey::svydesign()` / `svrepdesign()` object as `data`**. Estimation
  is design-based (`survey::svyglm`), so raw population weights need no rescaling and the point estimates
  match the weighted crosstabs. Weighted models show a survey-appropriate footer (design-based Wald test
  vs the null, Nagelkerke pseudo-R square, Rao-Scott AIC) and support model comparison (a design-based
  Wald test). Survey-weighted **ordinal** (`survey::svyolr`) and **multinomial** (needs the new optional
  `svyVGAM` package) outcomes are now supported too.
* `tab_reg()` gains a `split_var =` argument --- the regression analogue of `tab()`'s `tab_vars`: the
  same model is fitted **within each level** of a grouping variable and the per-group tables are stacked.
  Use `tab_spread()` on the grouping variable to place the groups side by side for an across-group
  comparison.
* `tab_reg()` gains `multiplier =` (a named vector like `c(age = 10)` showing a continuous predictor's
  effect **per k units**, e.g. the odds ratio per decade) and **`empirical =`** --- the crude
  (unadjusted, single-predictor) companion of the model effect, the standard "crude vs adjusted"
  comparison. It now works across families: **binomial** adds `Emp. %` + `Emp. OR` (coefficient) or
  `Emp. %` + `Emp. diff` (AME); **gaussian** adds `Emp. mean` + `Emp. diff` (coloured on the same
  SD-standardized scale as the model beta); **poisson** adds `Emp. rate` + `Emp. IRR`; **multinomial**
  shows the crude % + difference per category **in the HTML tooltip** (one column per category would be
  too many). It also works with a vector of dependents (one crude companion per outcome). The old
  `empirical_OR =` argument is now defunct (it errors, pointing to `empirical =`) --- it never shipped
  in a released version, so there is no code depending on it.
* `tab_logit()` and `multi_logit()` gain `na =` (`"keep"` / `"drop_all"`), forwarded to `tab_reg()`.
  `multi_logit()` also accepts **several `dependent`s**: the model comparison is then run once per
  dependent and returned as a list of tables (one Excel sheet each).
* Fixed `set_display("ratio")` (and `fmt(display = "ratio")`): it printed the raw count instead of the
  ratio. `"ratio"` is now the canonical display token (matching the field name); the legacy `"rr"` keeps
  working.
* `tab_reg()` (and `tab_logit()` / `multi_logit()`) gain an `estimate_display =` argument controlling
  what each effect cell shows beside the estimate: `"ci"` adds a visible confidence-interval bracket
  (`2.34 [1.20; 4.50]`); for logistic models `"prob"` folds the adjusted predicted probability into the
  odds-ratio cell (`2.34 (16%)`) and `"ame"` folds the average marginal effect (`2.34 (+8%)`). The
  probability folds need the `marginaleffects` package.
* New `or_plot()` --- a finalfit-style **odds-ratio forest plot** of a `tab_logit()` / `tab_reg()` table
  (log-scale point-and-interval plot beside a table of the estimates), and `lm_plots()` --- a `ggplot2`
  2x2 **diagnostic panel** (Residuals vs Fitted, Normal Q-Q, Scale-Location, Residuals vs Leverage) for a
  fitted `lm` / `glm`. Both need `ggplot2` and `gridExtra` (optional dependencies).
* Regression tables built with `split_var =` now also show their **per-group model-summary footer** when
  exported (kable / Markdown / Excel), one block per group (previously only the console showed it).
* Excel export now keeps the **in-cell test label** on p-value cells (e.g. a chi-square p-value shows as
  `2.9% (Chi2)`), folded into the cell number format, instead of dropping the label.
* `tab()` crosstab p-value rows now label each cell with the test it ran (e.g. `2.9% (Chi2)`,
  `1.4% (F, Welch)`), so a table mixing categorical and numeric columns is self-documenting.
* `tab()` odds-ratio columns (and any odds ratio) now print values below 1 as `1/x` (e.g. `1/4`
  instead of `0.25`), so they compare symmetrically with odds ratios above 1.
* `tab_md()` now exports **colored** markdown. A table built with colors (e.g. `tab(..., color = "diff")`)
  renders each cell as a short pandoc bracketed span `[value]{.class}`, so it shows up colored in Quarto,
  R Markdown and pandoc. The class names are short and uniform --- `p1`-`p4` (over-represented text),
  `m1`-`m4` (under-represented text), `o1`-`o4` / `u1`-`u4` for the background channel --- so numbers
  still line up in a monospace editor. `color = FALSE` gives plain monochrome markdown, and an
  uncolored table is unchanged.
* New `tab_css()` --- generate the stylesheet for those spans (and for `tab_kable(engine = "html")`).
  It takes no table: the class names identify a *palette shade*, not a threshold, so **one stylesheet
  styles every table in a document**, whatever their color breaks. Use `tab_md(css = TRUE)` /
  `tab_kable(css = TRUE)` to embed it inline, or emit `tab_css()` once at the top of a document.
  (`tab_md_css()` is a thin wrapper on it.)
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
* **Numbers switch to a monospace font when (and only when) a table shows significance stars.** A plain
  table keeps the proportional **DejaVu Sans** it always had (compact, better-looking); a table with
  stars uses a **monospace** font (default **Cascadia Mono**) so the stars -- and composite cells like
  `100% (n=  849)` -- line up in every column, which a proportional `*` (narrower than a digit) cannot.
  Text (row labels, headers) always stays DejaVu Sans Condensed. This applies to the HTML engine, Excel
  and `tab_plot()`; in the HTML engine a starred table's numbers are also nudged one step larger
  (Cascadia reads small) with no change to the row height. `tab_md()` keeps no font of its own, so it
  aligns a value's inner padding with a **figure space** (a digit wide, unlike the collapsing ASCII
  space) so `(n=...)` columns line up once the markdown is rendered to HTML. Each font is an option to
  change: `options(tabxplor.tab_kable_num_font)` / `options(tabxplor.tab_kable_num_font_stars)` (CSS
  font-family stacks for the HTML engine), `options(tabxplor.xl_font_num)` /
  `options(tabxplor.xl_font_num_stars)` (Excel; also the `font_num` / `font_num_stars` arguments of
  `tab_xl()`), and `options(tabxplor.plot_num_font)` (a graphics-device family; `""` for the ggpubr
  default). A model-fit footer's summary numbers (N, AIC, ...) now reach the column edge instead of
  reserving space for stars they never carry.
* **The Excel fonts are settable**, via `options(tabxplor.xl_font_text)`,
  `options(tabxplor.xl_font_num)` and `options(tabxplor.xl_font_num_stars)` (defaulting to
  `"DejaVu Sans Condensed"`, `"DejaVu Sans"` and `"Cascadia Mono"`). Note that xlsx, unlike HTML/CSS,
  has no font-fallback list: only one name is recorded, so set these to a font installed on the machine
  that will open the workbook.
* **The console picks up a dark editor automatically in Positron.** Console colours already follow the
  editor's light/dark theme (RStudio, and now Positron), but Positron's environment variables are
  unreliable across its terminal / kernel / extension-host processes, so a dark theme was sometimes
  missed. Detection now keys on Positron's own settings cache (`~/.positron-server`), not just those
  variables. As before, a mid-session theme switch is picked up with `set_color_palette(theme = "auto")`.
* **`display` accepts a bare field name.** `display = "ci"` (to show the confidence interval) now works
  as a shorthand for `display = "{ci}"`; likewise `"diff"`, `"pct"`, etc. — any single field can be given
  without the braces.
* **Bold reference/total cells in the console** (like the HTML/Excel exports already do). Enabled by
  default only in front-ends that render bold at a fixed character width — Positron and VS Code — and off
  in RStudio and unknown consoles, where bold is drawn wider and would break column alignment. Toggle it
  anywhere with `options(tabxplor.console_bold = TRUE/FALSE)`.

## Internal
* **Unified colour-legend + footer system.** The colour legend, the test/GOF summary and the three
  footer one-liners (weight / `Model:` / significance stars) used to be re-ordered by hand in each of
  the five rendering backends. They now flow through one ordered model (`tab_footer_streams()` +
  `render_footer()`), and the colour-legend engine groups columns by their *rendered* text (so a legend
  line can no longer drift from what it describes) and reads a single per-measure fact table. No change
  to any table's output.
* **Dependency footprint trimmed and clarified.** `broom` moved from `Suggests` to `Imports`, so
  `tab_reg()` / `tab_logit()` / `multi_logit()` now work out of the box for the common models
  (linear / logistic / Poisson) with no extra install; the model-specific back-ends (`survey`,
  `nnet`, `MASS`, `marginaleffects`, `brant`, `svyVGAM`/`VGAM`) stay optional `Suggests`. `crayon`
  (superseded) was dropped entirely — the console colours are now built with `cli` (already a
  dependency). The unused `grDevices` `Suggests` entry was removed. `htmltools` and `knitr` moved to
  `Imports` (they are used on the core render paths), which lets `kableExtra` move to `Suggests`.
* **New `?tabxplor-options` help page** documents every `tabxplor.*` global option (defaults,
  purpose) in one place, cross-linked from `?tab`.
* Fixed two option-default inconsistencies: `tabxplor.totcol_range` is now set once in `.onLoad`
  (default `"off"`) and read from a single place, and `tabxplor.cleannames`'s fallback is `FALSE`
  everywhere (a stray `TRUE` in `tab_reg()` disagreed with the load-time default). No behaviour change.
* Examples that need a `Suggests` package (`tab_reg()`, `tab_logit()`, `multi_logit()` → **broom**,
  plus **marginaleffects** / **nnet** / **MASS** for the AME, multinomial and ordinal cases;
  `tab_xl()` → **openxlsx2**; `tab_plot()` → **ggpubr**/**gtable**/**ggplot2**) are now wrapped in
  `requireNamespace()`, so they skip instead of failing where those packages are absent. The three
  slow `tab_reg()` examples moved into `\donttest{}` (they are still checked, just not in the timed
  pass): `tab_reg` examples went from ~22 s to ~1.3 s.
* Fixed a spurious deprecation warning: using the current colour API on a numeric column — e.g.
  `tab(df, x, num_var, color = "ratio", color_signif = "grey_non_signif")` — internally builds the
  legacy string `"diff_ci"` and used to re-check it against the deprecation gate, blaming the user for
  a value the pipeline itself wrote. Invisible in normal use, but it surfaced in the test suite of any
  package calling `tab()`. The genuine deprecation of `color = "diff_ci"`/`"after_ci"`/`"ci"` still
  fires for real user calls.
* `tab_xl()` no longer triggers openxlsx2's "removing illegal characters found in sheet name" warning:
  sheet titles are sanitised with the same substitution openxlsx2 would apply (each of `\ / ? * : [ ]`
  becomes a space), so the workbook is unchanged. Regression tables hit this routinely, since
  `tab_reg()` names odds-ratio columns `"<level> vs <reference>: OR"`.
* Silenced a tidyselect 1.1.0 deprecation ("using an external vector in selections") on the jamovi
  cache's numeric-aggregate path, and one raised by `dplyr::rename_with()` on a grouped tabxplor table
  (`NextMethod()` forwarded the column selection as a bare symbol).
* `VGAM` and `pkgload` are now declared in `Suggests` (they were used but undeclared), and survey-weighted
  multinomial models check for `VGAM` explicitly alongside `svyVGAM`.
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
* **`kableExtra` is now an optional (`Suggests`) dependency.** The default HTML engine is tabxplor's
  own dependency-free renderer, so most users are unaffected and installs are lighter (no
  `kableExtra` and its `xml2`/`rvest`/`svglite`/`systemfonts` chain). Only the legacy
  `tab_kable(engine = "kableExtra")` (and `kable_tabxplor_style()`) now require `kableExtra` to be
  installed — they raise a clear message pointing to `engine = "html"` if it is absent.
* **The `%>%` pipe is no longer re-exported, and the dependency list was slimmed down.** `magrittr` and
  `stringr` are dropped entirely — tabxplor now uses the base R `|>` pipe internally and `stringi` for
  string operations — and `labelled` is no longer suggested (variable labels are read with base
  `attr()`). Practical effect: code relying on `library(tabxplor)` alone to provide `%>%` should switch
  to the base `|>` pipe, or load `dplyr`/`magrittr` explicitly (most scripts already do). In exchange,
  `survey`, `nnet` and `MASS` are now hard dependencies, so weighted, multinomial and ordinal
  `tab_reg()` models work out of the box; `marginaleffects`, `svyVGAM`, `VGAM` and `brant` remain
  optional `Suggests` for the less common paths.
* **Significance stars are now opt-in (off by default).** A plain `tab()` no longer prints `*`/`**`/`***`
  after the cells: pass `stars = TRUE` (or set `options(tabxplor.stars = TRUE)`) to get them. Regression
  tables from `tab_reg()`/`tab_logit()`/`multi_logit()` still show stars by default (pass
  `stars = FALSE` to turn them off). When shown, stars are **right-padded** so the numbers stay aligned
  in a monospace font, and they no longer leak into `tab_kable()` tooltips (only the primary value is
  starred). A table built without stars stores no per-cell `pvalue` (`$pvalue` is `NA`); the colour
  significance policies (`color_signif`) are unaffected — they read the confidence bounds, not the stars.
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
* **`stars = TRUE` now shows stars even without a confidence interval.** Significance stars are read from
  a per-cell p-value that is only computed alongside a difference interval, so `tab(..., stars = TRUE)`
  used to show nothing unless `ci = "diff"` was also set. `stars = TRUE` now requests that interval
  itself (on percentage and mean columns), so the stars appear on their own.
* **`options(tabxplor.conf_level = )` now sets the confidence level everywhere, as documented.** It used
  to affect only the `contrib` colour-significance test; it is now the default for the confidence
  intervals and significance tests of `tab()`, `tab_num()`, `tab_ci()` and `tab_reg()` (each function's
  `conf_level =` argument still overrides it). The default is unchanged (`0.95`).
* **`levels = "first"` now handles missing values consistently.** A two-level column variable used to
  keep its `NA` column visible under `na = "keep"`, and a column variable with three or more levels used
  to keep the dropped-`NA` observations *inside* the percentage base under `na = "drop"`. Now, for any
  number of levels, `na = "keep"` counts the missing values in the base but hides the `NA` column (like
  the non-first levels), and `na = "drop"` removes them from the base --- matching `levels = "all"`. The
  same applies to soft-deprecated `sup_cols` (which use `levels = "first"`).
* **The space between a mean and its `(sd)` aligns in exports.** The narrow no-break space that joined
  `2.8` and `(σ2.3)` was narrower than a digit, so the `(sd)` slipped out of the column in `tab_md()`
  and `tab_kable()`; it is now a figure space (exactly one digit wide) in html and markdown, and a plain
  space in the (monospace) console.
* **Colour legend wording is clearer for the `grey_non_signif` policy.** The note now names the actual
  first colour threshold --- "Uncoloured: either not significant, or a difference under ±5 points"
  (or "×1.15", "±0.2 SD", … depending on the measure) --- and the compact console tag reads
  `[grey: non-significant or under ±5 points]`.
* **Excel numbers really render in the number font now.** They were named `DejaVu Sans` in the file
  but drawn in `DejaVu Sans Condensed`: every font tabxplor wrote was tagged as "the theme's body
  font", so Excel resolved it from the workbook theme (which is Condensed) and ignored the name.
* **The Excel title reads naturally**, with the dependent variable first (`"race by marital"` under
  `pct = "row"`, the reverse under `pct = "col"`) and at most two variable names before `"+N more"`.
  It also no longer says `"levels by ..."` on a table built from several row-variables.
* **The Excel colour legend's background swatches are legible.** The break-words describing the
  background channel are drawn as text (an Excel cell can carry a font colour but no fill), and were
  too pale to read on the white sheet; they are darker and more saturated now.
* **The Excel `sd` column is no longer as wide as the mean.** A numeric mean's `sd` sibling holds a
  short value under an `sd` header, so it takes a narrower column.
* **HTML tables are compact again.** They stretched to the full width of the pane, padding every column
  with blank -- but only when a colour legend was shown, which is why it looked erratic. The legend sits
  in a cell spanning the whole table, and its one long line of prose, not the data, was deciding how
  wide the table wanted to be. The footnote no longer takes part in sizing. Column widths are now left
  entirely to the browser as well (the levels and Total columns had a fixed minimum width that was
  usually too wide); to pin one yourself, add e.g. `.tabxplor-tab .tx-rv { min-width: 10em; }` to your
  own CSS -- see the new "Restyling a table" section of `?tab_css`.
* **A numeric column no longer says its name twice.** A mean column sat under a spanning header bearing
  the variable's name *and* repeated that name as its own header. The header now says which statistic:
  `mean (sd)`, or `mean` and `sd` in Excel, where the two are separate columns.
* **`tab_md()` output was not valid pandoc.** The column-variable name was written as a *second header
  row*, which pipe tables do not have: pandoc gave up on the whole table and rendered it as a
  line-block followed by a paragraph of pipes. Every markdown table carrying a column-variable name
  (that is, every normal one) was affected. The name is now the first body row, in italics, and a
  new `tab_md(col_var_names = FALSE)` drops it. Two smaller invalidities went with it: the thin spacer
  column between column variables now holds a dash on the delimiter row (a blank one is not a legal
  delimiter cell), and a `|` inside a level name is escaped instead of opening a spurious cell.
* **Coloured markdown cells wasted four spaces inside every span.** A bold row made the whole column
  reserve room for its `**`, but that room was added *inside* the brackets (`[    38%]{.p2}`) --
  spaces pandoc discards, and which pushed the number out of line with the bold one in the raw file.
  Cells are now padded by their visible width, so the numbers line up and the markup grows leftwards
  into the padding.
* **A wrapped column header showed its line break as text.** A long header name is wrapped with
  `<br>`, which the HTML engine escaped along with everything else, printing a literal
  `Télé:<br>occasionnel`.
* **Coloured cells no longer draw coloured borders** in the HTML engine: the `border-right: 1px solid`
  shorthand reset the border colour to the cell's own text colour, so a `+20%` cell got a blue border
  and a greyed-out one a grey border. Every border now takes the table's border colour, in both
  themes. (Announced in an earlier 1.4.0 development version, where only half of it was fixed: moving
  the rule off the cell's `style` attribute left the shorthand, and the shorthand was the cause.)
* **Tables know their own variables.** A table now records which variables are its rows, columns and
  tab-variables instead of leaving each function to guess them back from the column types. The guess
  could not survive `tab()` merging several row variables into one table -- that merge renames the
  first column to `levels` and keeps the variable names only as values of a `row_var` column, which the
  guess then read as a tab-variable. Three consequences are fixed: `tab_transpose()` refused such a
  table, citing tab-variables it did not have (it now transposes it -- each row variable becomes a
  column variable with its own total); the Excel title read *"levels by multi (tabbed by row_var)"*,
  naming nothing (titles now name the real variables, listing up to three then "+N more", and no
  longer fall through to a literal `NA`); and `tab_get_vars()` reported the merge's scaffolding.
* **`tab_xl()` now tells you where the file went** (a `cat`-style message), and no longer opens the
  wrong file: on its fallback paths it resolved the path twice, and with `replace = FALSE` the second
  resolution auto-numbered *past* the file it had just written.
* **A list of tables is never merged at export.** `tab_kable()` / `tab_md()` / `tab_xl()` /
  `tab_plot()` used to glue a list back into one table when its column variables happened to match --
  overriding a user who had asked to keep them apart (`output_list = TRUE`, `tab_many()`, or their own
  `list()`). `tab()` still merges its own row variables at build time, as before.
* **`transpose = TRUE` now works with several variables and numeric columns.** It used to flip the
  table's underlying data, which broke on anything but a single simple table: numeric-variable cells
  were wrongly coloured, each variable's total became a separate `Total_<variable>` column, and the
  count row landed last. Transposing now happens at the display stage, after the colours are computed
  --- so numeric cells keep their own colour, there is a single `Total` column, the count row sits
  right after it and numeric variables come last, and a single-variable transpose still matches the
  equivalent column-percentage table exactly (its base is an `n` row, not `100% (n=849)` in the total
  cell). Available on `tab_kable()` / `tab_md()` / `tab_xl()` / `tab_plot()` / `tab_export()`. (In
  Excel, transposed cells are written as coloured text rather than editable numbers.)
* **Excel mean/sd headers.** A numeric variable exported `NB_MUSIQUES` and `NB_MUSIQUES_sd` under a
  `NB_MUSIQUES` spanning header -- the name three times. The columns are now headed `mean` and `sd`.
* **`tab_plot()`'s colour legend** printed raw HTML fragments as text (`color:#02A5B3 !important;">+5`)
  in uniform black: it recovered the legend by scraping regexes back out of the HTML rendering, and
  those had silently stopped matching when the legend was rewritten. It now reads the legend's own
  colour data.
* **The console colour legend ignored its `theme` argument**, silently rendering the palette from
  `options(tabxplor.color_style_theme)` instead of the one asked for.
* **Mean differences no longer print a multiplication sign.** A `diff` on a numeric column showed
  `×-0.2` — a multiplicative glyph on an additive quantity, indistinguishable from a ratio. The field
  has held a real difference (cell mean − reference mean) since the 1.4.0 aggregate rewrite; the
  display now matches it, in the variable's own units and with an explicit sign (`+1.2` / `-0.22`),
  exactly like a percentage difference minus the `%`. Excel follows. The `×` now belongs to the ratio
  alone. The sd-standardized view the colours use stays a colour device: the legend names its
  thresholds, and cell tooltips gained a `std diff:` line so you can read them off a cell.
* **Cell tooltips (`tab_kable()`).** Several fixes: a mean difference showed the multiplication sign
  twice (`diff: ××-0.2`); a reference cell said `diff: ref ; ratio: ×1` — the same thing twice, plus a
  vacuous ratio — and now says `ref` once, keeping its `n:`; a Total column, where every cell is its
  own base, printed `ratio: ×1` on every row and now prints none; and values arrived padded with the
  column's alignment spaces (`ratio:   ×1`). A mean column now shows the ratio it is coloured by
  (it was suppressed). Tooltips are also reachable on the last columns again — they reorient when they
  would overflow the window instead of always opening rightwards — and no longer wrap to four lines.
  `tab_kable(engine = "html", popover = TRUE)` showed its own HTML attributes as the popover text.
* **`color_signif` no longer greys out the whole table.** Asking for a significance policy without
  also writing `ci = "diff"` by hand produced an all-grey table: the confidence interval the policy
  gates on was never computed. `color_signif = "grey_non_signif"` / `"guaranteed_effect"` now request
  that interval themselves, for every form of `color` — including the default `color = TRUE`, which
  was the worst hit. `tab(color = TRUE, color_signif = "grey_non_signif")` is now identical to
  `tab(color = TRUE, ci = "diff", color_signif = "grey_non_signif")`. Combining a policy with an
  explicit `ci = "cell"` (which measures something else) is now an error rather than silent grey.
* **`color_signif = "guaranteed_effect"` now colours every significant cell.** Its thresholds start at
  the neutral value (`0`, or `1` for a ratio): the mode colours the effect you are confident of *at
  least*, so a cell whose interval excludes 0 is by definition a guaranteed effect and must be
  coloured. It previously reused the ordinary thresholds, leaving a significant-but-modest cell (e.g.
  `+7%`, interval `[+0.4; +16.6]`) grey. The default `5; 10; 20; 30` scale becomes `0; 5; 15; 25`
  under this policy — as the colour legend already claimed. The legend follows automatically.
* **A statistical test no longer fails on a variable with a single category.** `test = TRUE` (formerly
  `chi2 = TRUE`) errored with `invalid 'times' argument` when a row variable had exactly one non-total
  row — e.g. after `na = "drop"` emptied its other levels. Such a table is degenerate and now yields
  an `NA` test, like any other degenerate table. Under `parallel = TRUE` the same bug surfaced as an
  opaque `mirai_map()` error; it was never parallel-specific.
* **The `n` row is back on `pct = "col"` tables with several row variables.** With two or more row
  variables the `add_n` (and `add_pct`) row was silently dropped. Each sub-table now gets its own `n`
  row, directly under its own Total row.
* `tab(parallel = )` now works when the package is loaded with `devtools::load_all()` (it used to fail
  with `object 'tab_build_one' not found` for calls with two or more row variables). This only affected
  package development, not installed versions.
* Fixed a spurious warning (`longer object length is not a multiple of shorter object length`) on
  tables with several row variables and several column variables whose counts do not divide (e.g. 3 × 4).
  Output was already correct; the warning is gone.
* **`color_signif = "color_all_signif"` on a ratio (or two-channel `color = c("diff", "ratio")`) table
  no longer mis-colours.** The "guaranteed effect" colouring fed the difference confidence bound into
  the ratio (multiplicative) scale, so nearly every significant cell — including over-represented ones
  — got the strongest *under-represented* colour. It now colours the guaranteed **ratio**, so the
  colour direction always matches the cell (over-represented → over-colour, under → under-colour).
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
* On Linux, the `lang` argument of `tab_kable()`, `tab_md()`, `tab_xl()`, `tab_plot()` and
  `tab_export()` silently had no effect: `lang = "fr"` returned an English colour legend. Setting the
  `LANGUAGE` environment variable is not enough on its own, because glibc caches translated strings;
  the cache is now flushed around the switch. (Windows and macOS were unaffected, which is why this
  went unnoticed.) Note that gettext ignores `LANGUAGE` entirely when the locale is `C`, so `lang`
  cannot translate under `LANG=C`.
* The colour legend's HTML no longer depends on `kableExtra`, so `tab_kable(engine = "html")` is now
  genuinely self-contained (as documented) and its output is stable across `kableExtra` versions.
* A single-variable table (`tab(relig)`, `tab(relig, pct = "col")`) again shows its count column, as
  in versions <= 1.3.1: the `n` (and, when weighted, `wn`) column had disappeared from the display,
  and the internal `no_col_var` placeholder could surface as a spanning column header. Both are fixed.
* `fct_recode_helper(freq = TRUE)` no longer errors with "object 'pct' not found".
* Regression tables (`tab_reg()`) are more readable: the model-fit footer (N, AIC, ...) and the
  reference-category cells now render black and bold instead of greyed out, and an ordinal model's
  footer gains a **Brant proportional-odds test** p-value row.
* The `color_signif = "grey_non_signif"` colour legend was statistically misleading. It said an
  un-coloured cell is "not significantly different from the reference", but such a cell can be
  significant yet too small to colour. The legend now states the true guarantee -- a *coloured* cell
  is significantly different -- and this is documented under `color_signif` in `?tab`.
* `tab_reg()` now handles **ordered-factor predictors**: an ordered predictor (e.g.
  `as.ordered(income)`) used to produce polynomial contrasts that broke the coefficient column
  (all-NA odds ratios) and left some average-marginal-effect (AME) cells empty. Ordered predictors are
  coerced to ordinary factors internally (the ordinal *outcome* is unaffected), so no manual
  de-ordering is needed. AME cells for levels whose label contains `" - "` (e.g. `"$20000 - 24999"`)
  are also fixed.
* `tab_reg()` tooltips are cleaner: the model-fit footer rows no longer show a nonsensical tooltip; the
  per-cell `n:` is the row-level count where one exists (e.g. `empirical_OR = TRUE`) instead of the
  whole-model N; and an `effect = "ame"` column still shows the model odds ratio on hover.
* Multinomial `tab_reg()` tables no longer draw borders between the outcome-category columns of one
  model (they now share one spanning header naming the model). And a regression table whose columns are
  named after the model no longer repeats that name in a redundant spanning row above the headers.
* `tab_reg()`'s `empirical_OR` argument is renamed **`empirical`** (the old name still works with a
  deprecation warning). It now also adds the crude percentage / odds-ratio companion columns to an
  `effect = "ame"` table (was coefficient-only), and is a message rather than an error for the families
  where the crude companion is not yet available (gaussian, poisson, multinomial).
* `tab_reg()` accepts **several dependents AND a list of models at once**: it returns one model-comparison
  table per dependent (as a list), so `tab_export("xl")` writes one sheet per dependent. Two more
  model-comparison fixes: when one model is a superset of the others ("complete"), its predictor order is
  kept; a superset used as the `baseline` is now recognised as nested (a likelihood-ratio test, not the
  AIC fallback); and a new **`na = "drop_all"`** fits every model on one shared complete-case population
  so nested models get equal N.

## Bug fixes
* `tab()` now accepts a **`data.table`** as its `data` input (it used to error with "Selections can't
  have missing values" whenever a numeric column was tabulated).
* `color = "contrib"` now works **with a significance policy** (`color_signif = "grey_non_signif"` or
  `"color_all_signif"`): it colours only the cells whose contribution to the Chi-squared is significant
  (a standardized-residual test). It previously coloured nothing in that combination.
* A **weight column literally named `"wt"`** no longer corrupts weighted numeric-mean tables (it used to
  leak a spurious column and warn). Using the same column as both the weight and a row/column/tab
  variable now raises a clear error instead of an obscure one.
* The **jamovi** module no longer aborts on 0-row data (e.g. mid-selection or a fully filtered dataset)
  and no longer prints a misleading "the table has no tabxplor_fmt columns" message when a formatted
  table is present.

## Deprecations
* `tab_transpose()` is **soft-deprecated** in favour of the exporters' `transpose = TRUE` argument.
  It flips the underlying data, which cannot represent a transposed column's mixed cell types, so it
  mis-transposes tables with several variables or numeric columns. `transpose = TRUE` flips the display
  instead and handles them. The function is kept for the single-variable round-trip it always supported.
* `kable_tabxplor_style()` is **soft-deprecated** in favour of `tab_kable()`, which renders any
  table -- `tabxplor_tab` or plain data.frame -- and shares the exporter machinery. The old function
  predates it: it finds total rows and columns by matching the literal strings `"Total"`/`"Ensemble"`
  (so it only works in English and French), and renders no colours, tooltips or spanning headers.
* `tab_md(col_var_names =)` is **soft-deprecated** in favour of the shared `var_names` argument, which
  every exporter takes and which also governs the row-variable name: `col_var_names = FALSE` is
  `var_names = "rows"` (or `"none"`). It still works.
* `chi2` is renamed **`test`** in `tab()` and `tab_counts()` (soft-deprecated; `chi2` still works).
  The whole-(sub)table test is a Chi-squared only for factor `col_vars` — a numeric one gets Welch's
  F (one-way ANOVA) — so the old name described half of what the argument does.
  `tab_many()` keeps `chi2` (it is itself soft-deprecated).
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
* `color_type` is **soft-deprecated and now inert** on every exporter (`tab_kable()`, `tab_md()`,
  `tab_xl()`, `tab_plot()`, `tab_css()`, `tab_export()`, `tab_md_css()`), together with the
  `tabxplor.color_style_type` option. It globally repointed the *text* channel into the *fill*
  palette (fill-coloured font); the visual channel is now chosen by position in the `color` argument
  (`color = c(text, background)`). Both still accept their old values (a message is shown).
  As a side effect this corrects an inconsistency: `tab_xl()` used to ignore the option while
  `tab_export(., "xl")` honoured it -- neither does now.

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
