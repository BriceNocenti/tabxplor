
# tabxplor 2.0.0

## New features

* **`tab()` is now the unified entry point.** It accepts **several `row_vars` / `col_vars`**
  (e.g. `tab(data, c(race, relig), marital)`), merged into one table by default or returned as a list
  with `output_list = TRUE`. `tab_many()` is kept as a soft-deprecated alias. Several `row_vars` and
  `tab_vars` now **compose** — `tab(data, c(race, relig), marital, tab_vars = year)` returns a table
  where it used to silently return a list.
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
  the same interval. One named vector, `ci_method = c(cell =, diff =, mean_diff =, mean_ratio =)`,
  chooses each interval's method.
* **Mean columns get a whole-table test** — a one-way ANOVA, the counterpart of the chi-squared for
  factor columns. New `tab(anova = "welch" | "classic")` chooses which F is *shown*
  (`options(tabxplor.anova)` remains the default): both are always computed and stored, so this
  changes a display, never a number.
* **`tab_plain()` gains `ci =` and `ci_method =`**, so it builds its own intervals like `tab_num()`
  does, instead of needing `|> tab_ci()`. It resolves them exactly as `tab()` does, so
  `tab_plain(ci = "cell")` and `tab(ci = "cell")` agree cell for cell.
* **Effect sizes and Fisher's exact.** `test = TRUE` now carries Cramér's V / phi or eta²; a small
  sparse table uses Fisher's exact.
* **`tab_shape()` and `tab_supports()`** answer "what have I got, and what can I do with it?" before
  you try. A table reports whether it is merged (several row variables), grouped (sub-tables), or a
  list, and which of `tab_compact()` / `tab_transpose()` / `transpose = TRUE` accept that shape — a
  support matrix that used to exist only as scattered error messages.
* **`tab_columns()`** does the same for the columns: one row per numeric column with what it
  estimates, how it is coloured, and — side by side for the first time — the confidence level, the
  degrees of freedom, the basis (raw count / weights / survey design) and the method its interval was
  built with. **`fmt_attr(x, name)`** reads or writes any one of those facts by name.
* **`forest_plot()` — a chart of any tabxplor table**, cross-table or regression: every estimate with
  its confidence interval, its stars and *its own cell colour*, one panel per column of the table.
  It reads the finished table and re-computes nothing, so the figure cannot disagree with the numbers
  you printed: the gridlines are your `set_color_breaks()` ladder, the colour key is the table's own
  legend, and it returns an ordinary `ggplot` you can `+ theme()` and `ggsave()`. By default it draws
  whatever the table computed (`ci = "cell"` → percentages with their intervals, `ci = "ref"` →
  differences from the reference, `display = "{or}"` → odds ratios on a log axis). On a regression
  table with `empirical = TRUE` it draws the observed effect with the margin of error **of the gap**
  between the two, so "is the point outside the bracket?" is exactly the table's own gap test — rather than the
  two overlapping intervals that reading invites and that are wrong for correlated estimates.
  Also reachable as `tab_export(format = "forest")`.
* **Weights, and survey designs.** A weighted `tab()` estimates the population, and now **says in its
  footer** what its confidence intervals and tests are based on. By default that is still the raw
  number of respondents (no design effect). `options(tabxplor.design_effect = TRUE)` makes every
  weighted interval, star, colour threshold and p-value **account for the unequal weighting, exactly**
  (per call: `tab(design_effect = TRUE)`) —
  a weight column *is* a survey design (the flat one), so this reproduces `survey` to the last digit
  rather than approximating it. Pass a `survey::svydesign()` as `data` and the whole table follows the
  full design instead: strata, clusters, `fpc`, calibration (so a design can also make an interval
  *narrower*), each interval referred to the design's own degrees of freedom. `tab_reg()`'s observed
  (`Obs_*`) columns are **always** on the same basis as the `Model_*` column beside them, so the two are
  finally comparable by construction — turn the option on if you want a `tab()` percentage to match them.
  See `?tab` and `?tab_reg`.
* **Standardized residuals for `color = "contrib"`.** Which cells depart from independence is now
  answered with the **adjusted standardized residual** (Haberman — SPSS's "adjusted residual", R's
  `chisq.test()$stdres`), on the package's usual inference base (the unweighted *n*, or — when weights
  or a design are accounted for — that *n* divided by the association's own design effect, the same
  Rao-Scott one the table's Chi-2 line reports; one base per table, so a counts table and a
  percentage table of the same data give the same residuals).
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
  `common_totrow =`, a per-`col_var` / positional `ref`, and `parallel =` (opt-in, needs `mirai`;
  also on `tab_reg()`, where it builds the models / groups / outcomes of one call in parallel).
* **`tab_counts()`** — build a full colour-coded table from already-aggregated counts (long, wide, `table`,
  or frequencies + base N) instead of microdata.
* **`tab_reg()`** — colour-coded regression tables (linear / logistic / Poisson / multinomial / ordinal),
  with survey weights, model comparison, average marginal effects, and Excel / HTML / Markdown export.
  See the regression vignette. `tab_logit()` / `multi_logit()` are thin wrappers; `forest_plot()` /
  `reg_check_plots()` draw it.
* **Every regression table now checks itself.** The footer carries five model checks — **Linearity**
  (per continuous predictor), **Proportionality (Brant)**, **Dispersion (robust/model SE)**,
  **Influence (max dfbetas)** and **Collinearity (max VIF)** — one row per model column, so a
  comparison reads down. They matter: on the model used throughout the regression vignette, letting
  `age` curve moves the top income category's odds ratio by a quarter and flips another income
  level's verdict, and nothing in the table used to say so. The three that cost nothing are shown by
  default; the two that fit a model (Linearity, Proportionality) are asked for by name —
  `stats = c("n", "aic", "linearity")`, or **`stats = "all"` for every statistic and every check the
  model allows**. `stats = "collinearity"` needs the new suggested package `car`. The per-predictor
  overall-association test (`stats = "global"`) moved from a footer sentence to footer rows for the
  same reason.
* **A continuous predictor's row shows the shape of its effect**, as a small curve in its own label —
  ten bins of the outcome against the predictor, with no model in it (`options(tabxplor.spark = FALSE)`
  to switch it off, `"ascii"` for a font without block characters). In HTML it becomes an inline SVG.
* **`tab_reg(shape =)` fits a continuous predictor as something other than a line** — the cure for what
  the linearity row finds. A named vector: `"quintiles"` / `"quartiles"` / an integer cuts it into
  quantile groups (it becomes an ordinary factor, with one estimate, observed companion, count and
  colour per group); `"quadratic"` adds a curvature term, giving the predictor two rows; `"log"` /
  `"sqrt"` fit the transformed column. The observed `Obs_*` companion is fitted with the same shape, so
  the model-versus-observed comparison still compares like with like.
* **`reg_check_plots()`** draws those five checks — one panel each, faceted across every model in the
  table, in the light / dark / print themes. A teaching companion: every verdict it illustrates is
  already a footer row. It takes a `tab_reg()` table plus its data, or a fitted model directly.
* **Colour the gap between the modelled and the observed effect.** `tab_reg(empirical = TRUE)` already
  prints the crude effect beside the adjusted one; `color = c("OR", "adjustment")` now colours *how far
  apart they are*, so a whole table of "what did adjusting change?" reads at a glance. With
  `split_var`, `color = "between_groups"` does the same against the first group (effect modification,
  row by row). `color_signif` applies to both, so a gap can be greyed when it is no bigger than chance,
  and the html tooltip gives its confidence interval and p-value. The gap is also printable
  (`display = "{or} (obs {obs})"`). Part of an **odds-ratio** gap is non-collapsibility rather than
  confounding, so there the colours stay descriptive and `tab_reg()` says so once: use marginal effects
  (`effect = "marginal"`) or risk ratios (`measure = "ratio"`) for a comparison the test can read.
* **Every outcome now has an observed counterpart.** `tab_reg(empirical = TRUE)` used to go quiet on
  three families. A **summed score** (`trials =`) now shows its mean score plus the odds ratio of the
  summed items; an **ordinal** outcome shows `Obs_cumOR`, the cumulative odds ratio of the same model
  with one predictor; a **multinomial** outcome would need one crude column per category, so its
  observed effect is folded into the model cell instead — `2.31 (obs 2.05)`. `color = "adjustment"`
  therefore works everywhere, and on the marginal paths (`effect = "marginal"`) of a 3+ level
  outcome the gap now carries a real significance test. One rule covers all of it: *the observed effect
  is the model's own effect, fitted with a single predictor*.
* **The odds ratio is always there.** On any `pct = "row"` / `"col"` table every cell now carries its
  odds ratio, so seeing one is a display choice rather than a build option: `display = "{or}"` (or
  `"{or} ({pct})"`), `color = "odds_ratio"` to colour it, `ref2` to pick which level the 2×2 compares
  against. The `OR =` argument is soft-deprecated onto exactly that. With `levels = "first"` the table
  shows one level against the merged rest, so its odds ratio is the true binary one.
* **`tab(ref2 = "cumulative")`** — the descriptive twin of that ordinal model: one **cumulative odds
  ratio per cut point** ("at or below level j") for an `ordered` col_var, with no proportional-odds
  assumption. The spread of the odds ratios across a row *is* the departure from proportional odds.
* **`ci` asks one question: where does the interval sit?** `"auto"` (the new default — an interval
  whenever something reads it), `"no"`, `"cell"` (each cell's own) or `"ref"` (against the reference).
  *Which* comparison it measures is `color`'s to say, so the old `"diff"` / `"ratio"` are
  soft-deprecated onto `"ref"`. `ci = "no"` and `ci = "cell"` leave nothing to test a comparison
  against, so they inform you and disable `stars` / `color_signif` instead of overruling what you
  typed. `display` also accepts a bare field name (`display = "n"`, the same as `"{n}"`).
* **One display grammar for `tab()` and `tab_reg()`.** The same named layouts everywhere ---
  `"est"`, `"est_ci"`, `"est_base"`, `"base_est"`, `"base"`, `"base_ci"` --- built on two new
  scale-relative tokens: `{est}` is whatever the column estimates and `{base}` the level it sits on
  (a percentage, a mean, a count), so one template works on every family. `{gap}` shows how far
  adjustment moved a regression effect, in print and Excel as well as in a tooltip. On `tab_reg()`
  the layouts now reach every family, not just binomial coefficient models.
* **One rule for multiplicative cells.** A value below its reference prints as its inverse
  (`1/2.67` for an odds ratio, `÷2.67` for a risk / rate / mean ratio, the measure's own glyph) in
  *every* rendering --- a bare cell, a `{}` composite and the `est_ci` bracket alike. Composites
  used to drop it, so a table could show `1/2.67` and `0.37` for the same quantity. A reference cell prints a bare
  `1`, so its row stands out. Set
  `options(tabxplor.ratio_print = "raw")` for the journal convention.
* **`ci_method = c(mean_diff = "ols")`** pools the variance over *every* level of the variable, so
  the interval is the one a linear regression gives that coefficient (`"student"` pools the two
  compared groups only). `c(mean_ratio = "quasipoisson")` now likewise uses the single dispersion a
  quasi-Poisson regression estimates. `tab_reg(empirical = TRUE)`'s observed columns use them
  automatically on an unweighted table, and the design-based forms on a weighted one --- so an
  observed effect and its interval are exactly the univariable model's.
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
* **`tab_reg()` speaks `tab()`'s vocabulary.** The arguments both producers share are now spelled the
  same way: `dependent` is **`outcome`**, `split_var` is **`tab_vars`**, `reference` is **`ref`**
  (`c(var = "level")`, predictors and `tab_vars`), and `method` is **`ci_method`** — the named vector
  `tab()` takes, whose fifth slot is the regression's (`ci_method = c(model = "profile")`, or just
  `"profile"`). `inverse_two_level_factors` (a logical that toggled level *order*) is
  **`outcome_level`**, which names the level: `outcome_level = c(married = "Married")` for the level
  MODELLED on a binomial outcome, the baseline category on a multinomial one, refused on an ordinal
  one. It is the twin of `ref`, and the pair asks opposite questions — *`ref` names the level you
  compare AGAINST, `outcome_level` the level you MODEL*. Every retired spelling aborts naming its
  replacement.
* **One `stats =` for the whole model-summary footer.** `compare` and `baseline` are gone: the
  comparison is a footer key like any other, and the baseline model is that key's value —
  `stats = c("n", "aic", "compare_sequential")`, `stats = c("n", compare_baseline = "Model 1")`, or
  `stats = "compare_baseline"` for the first model. A comparison key *adds* a row and restricts
  nothing. Note `stats = FALSE` / `"none"` now hides the comparison too, which `compare` did not.
* **`tab_reg()` no longer documents `.fit_cache`** (jamovi-internal; it rides `...`).
* **`tab_reg()` asks two questions instead of four.** An estimand is *which contrast* × *which effect
  measure*, so that is what the arguments are: **`effect = c("coefficient", "marginal",
  "at_reference")`** and **`measure = c("auto", "odds_ratio", "ratio", "difference", "log")`**, both
  resolved per dependent like `family`. `measure` takes the full word or the discipline's acronym
  (`"RR"` / `"IRR"` / `"RD"` / `"OR"`), and the column header keeps the acronym. This **replaces**
  `exponentiate` (→ `measure = "log"`), `at` (→ `effect = "at_reference"`), `effect = "ame"` /
  `"ame_ratio"` (→ `"marginal"`, with `measure = "ratio"`) and `estimate_display` (→ `display`,
  which also takes a `"{or} ({pct})"` template). The retired names abort with the new spelling.
* **Risk ratios, risk differences and ratios of means, through the front door.** `measure = "ratio"`
  on a **binary** outcome fits the **modified Poisson** (robust standard errors) — it used to require
  naming the wrong distribution, `family = "poisson"`, which still works; on a **continuous** outcome
  it gives a **ratio of adjusted means** (Poisson pseudo-likelihood), which `tab_reg()` refused
  outright although `tab()` has given one for years. `measure = "difference"` on a binary outcome
  gives the **risk difference** from an identity-link fit (falling back to the linear probability
  model, with a message, if it does not converge). `effect = "marginal", measure = "ratio"` is the
  **marginal** risk ratio from the usual logistic fit, and is now available for every outcome.
  `empirical = TRUE` gives the matching crude companion in every case.
* **New `reg_measures(data, dependent)`** lists what an outcome can be modelled as: every
  `effect` × `measure` cell with its status — *available*, *not defined* (an odds ratio needs a
  probability), or *not offered* — and the header it would produce. It is the same runtime table
  the argument validator, the error messages and `?tab_reg`'s own generated section read.
* **`tab_reg(color =)` can no longer contradict the column.** The colour ladder comes from what the
  column estimates, so the geometry values are gone: `color = TRUE` grades each cell on its own
  scale, and what is left to choose is what to compare it *to* — `c(TRUE, "adjustment")` (was
  `c("OR", "adjustment")`) or `c(TRUE, "between_groups")`.
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
* **Marginal effects are much faster.** `tab_reg(effect = "marginal")` computes its estimates,
  adjusted predictions and confidence intervals itself, from an analytic derivative, instead of
  through a numerical one: measured 10.0 s → 1.2 s on a four-predictor logistic regression over
  21 000 rows, and 45.2 s → 5.2 s on a three-level multinomial. The printed numbers are unchanged
  (they match `marginaleffects` to eight decimal places, which the tests pin).
* **New jamovi "Regression models" analysis (`jmvtabreg`)** for `tab_reg()`. The Crosstables module (`jmvtab`)
  gains a reference-level picker, export, a live cache, and the new options. The jamovi html results and
  exports now show the per-cell hover tooltips (counts, confidence intervals, differences;
  `options(tabxplor.tab_kable_tooltips = FALSE)` to disable).
* **Levels can be merged from the jamovi panels**: tick a level to fold it into the one above,
  chain ticks to merge a run, and name the result. Available on every crosstab axis (beside the
  existing level reordering) and on each factor predictor of a regression. In R, do the same with
  `forcats::fct_collapse()` before calling `tab()` / `tab_reg()` — it is the very same operation.
* **The jamovi panels now name every option after the R argument it drives** (`outcome`,
  `tab_vars`, `ci_method`, `multiplier`, `shape`, `ref`, `stats`…), so clicking through the module
  still teaches the R API. Two controls were added: the model checks that refit the model
  (`stats = "all"`) and the N per predictor level (`add_n`). ⚠ jamovi keys an analysis's saved
  settings by option name, so **a `.omv` file saved with an earlier development build loses the
  values of the renamed options** and falls back to their defaults.

## Changes that may affect existing code

* **A variable with a level named `"Total"` (or `"Ensemble"`) is now refused**, naming the level.
  `tab()` uses those labels for its own total rows and read such a level back AS one — bold, out of
  the percentage base, and printed twice, with no warning. Rename the level, or move tabxplor's
  labels with `options(tabxplor.total_names = c(row = "..."))`. A level named `"NA"` or `"Others"` is
  still fine.
* **`tab_reg(stats =)`: the two model checks that fit a model are now opt-in**, and `"all"` means
  all. Linearity refits once per continuous predictor and the Brant proportional-odds test fits its
  own auxiliary logits; between them they were most of the cost of a regression table (a
  200 000-row, 6-predictor logit went from 12.3 s to 3.4 s). Ask for them by name —
  `stats = c("n", "aic", "linearity")` — or take everything with `stats = "all"`, which previously
  meant only the default set. Dispersion, Influence and Collinearity are unchanged and still shown
  by default, `reg_check_plots()` still draws **every** panel, and the observed curve in each
  continuous predictor's own row label needs no model at all. One consequence: an ordinal table no
  longer warns about a rejected proportional-odds assumption unless you asked for the check.
* **Everything past the variable roles must now be named.** `tab()`, `tab_plain()`, `tab_num()` and
  `tab_counts()` take `...` right after their variable arguments, so an unnamed extra argument is
  refused by name instead of landing in whatever formal sat at that position. Every named call keeps
  working, and a typo now gets a suggestion (`tab(colour = TRUE)` → *did you mean `color`?*) where R
  used to say only "unused argument". One consequence worth knowing: an argument sitting after `...`
  is matched **exactly**, so an abbreviation that used to partial-match silently (`color_br =`) is
  now refused — with the full name in the message.
* **The four synthetic labels are one option**: `options(tabxplor.total_names = c(row =, col =,
  tab =, other =))` replaces the `total_names` / `totaltab_name` / `other_level` arguments (which
  still work, with a message). A partial vector is allowed, so a French document can set
  `c(tab = "Ensemble", other = "Autres")` once and leave the rest alone.
* **`options(tabxplor.stars)` now carries the star ladder too**: `FALSE`, `TRUE`, or your own
  `c("*" = 0.05, "**" = 0.01)`. It replaces `tabxplor.signif_levels` + `tabxplor.signif_labels`
  (still read if you set them). The stars are a render-time reading of each cell's stored p-value,
  so changing the ladder re-reads tables you have already built.
* **`fmt` columns carry a 16th attribute, `col_group`** --- which sub-population a column's block
  belongs to, after `tab(spread_vars =)` / `tab_spread()` or `tab_reg(split_var =)` (`""` otherwise).
  Read it with `get_col_group()`. Those columns used to fold the level into their `col_var` as
  `"{level}<br>{variable}"`; `get_col_var()` returns the plain variable name now, and the two facts
  together identify a column *block*. Rendered output is unchanged. Only code reading `col_var` off a
  *spread* table is affected.
* **`ci = "cell"` now shows the total (reference) row's own interval too.** A cell interval compares
  each cell to 0 %, not to a reference, so every cell has one — including the total row, which is the
  best-estimated cell in the table. Numeric tables already printed it; percentage tables left it
  blank.
* **`tab_reg()` now checks four arguments it never checked.** `conf_level = 95` reached the interval
  engine as a probability, a typo in `stats` was silently dropped (so a footer row simply went
  missing, with no message), `color_signif = "grey"` was stored on every column as a policy no
  consumer knows, and a `baseline` given without `compare = "baseline"` was ignored in silence. Each
  now aborts — or, for the last, says why it cannot be used.
* **An unknown argument value now aborts instead of being silently ignored.** `totaltab`, `n_min` and
  `conf_level` were validated nowhere at all, so `tab(totaltab = "tabel")` quietly meant "no total
  table" and `conf_level = 95` reached the interval engine as a probability. Every crosstab producer
  (`tab()`, `tab_plain()`, `tab_num()`, `tab_counts()`) now checks its arguments against one declared
  vocabulary and names the valid set in the message; `conf_level = 95` suggests `0.95`.
* **`tab_counts(ci_method = c(mean_diff = ))` now aborts.** A counts table has no mean columns, so the
  two mean slots were accepted and did nothing; `cell` and `diff` are unaffected.
* **A weighted table now says, in its footer, what its intervals and tests are based on** — and the
  default position ("the raw number of respondents") is stated rather than left silent. The
  development-only option `tabxplor.kish_neff` is **renamed `tabxplor.design_effect`** (it was never
  released) and no longer approximates: it now computes the weighting's own design effect exactly. It
  is scoped to `tab()` and its leaves; `tab_reg()` never reads it, its observed columns being always
  corrected. Two consequences on numbers: with the option **on**, a weighted table's intervals and
  p-values change slightly (an approximation became exact, in either direction, and a table weighted by
  a *constant* gets an effective n a whisker below the raw one, `survey`'s own finite-sample factor); and
  `tab_reg(empirical = TRUE)` on weighted data now widens its observed intervals **unconditionally**,
  which is what makes them match the model column beside them.
* **`tab(pct = "all", ci = "cell")` used to error** ("`false` must be a vector, not NULL"), weighted or
  not. Fixed.
* **What `tab()` returns is now decided by `output_list` alone.** `options(tabxplor.output_kable)` was
  read inside a build stage and changed the *class* of the returned object; it now only renders the
  result with `tab_html()`, as documented.
* **`pct` is vectorised over `col_vars`**, like `levels` and `digits` (`tab(d, x, c(a, b), pct =
  c("row", "col"))`). A per-`row_var` list is refused with a message: that axis is global in `tab()`.
* **`totcol = "each"` and `"all_col_vars"` no longer error and no longer build per-`col_var` totals** —
  exactly one total column is shown since 2.0.0, and they are now spellings of it.
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
  weights plus `options(tabxplor.design_effect = TRUE)`, or a design. Replicate-weight (`svrepdesign`)
  and two-phase designs are refused with a message rather than failing obscurely, and passing `wt =`
  *and* a design is now an error (a design already carries its own weights).
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

* **`tab_plain(color_signif =)` did nothing.** The superseded factor leaf never applied the colour
  spec it resolved, so the significance policy was stored as `"ignore"` whatever you asked, a
  composite like `color = "diff_ci"` kept its measure and lost its test, and a two-channel
  `color = c(<text>, <background>)` failed outright. `tab()` and `tab_num()` were unaffected.
* **`tab_md(lang =)` and `tab_xl(lang =)` did nothing.** The colour legend followed the ambient
  locale whatever you passed. `tab_html()`, `tab_plot()` and `forest_plot()` were unaffected.
* **A survey-design ANOVA row called itself a Welch F.** On a weighted / `svydesign` table the
  numeric p-value row printed `pvalue (Chi2, Welch F; survey-design)` for a test that is a
  design-based Wald F. It now says `F`, the `; survey-design` suffix naming the estimator.
* **Arithmetic silently did nothing on a `pct_ci`, `mean_ci` or `pvalue` column.** `x * 2` returned
  `x` unchanged, with no warning, on display values `?fmt` documents — so `mutate()` over the fmt
  columns of a table showing confidence intervals quietly left them alone. They now write back to the
  field they display, like every other token.
* **The two `tab_reg()` estimands added in 2.0.0 got no model checks at all.** `measure = "difference"`
  on a binary outcome and `measure = "ratio"` on a continuous one are fitted through a different
  *link*, and the assumption checks (`stats =`, `reg_check_plots()`) were keyed on that link rather
  than on the distribution behind it — so those two tables silently reported no linearity, dispersion,
  influence or collinearity row and drew no diagnostic panel.
* **A partial per-outcome vector aborted instead of defaulting.** `tab_reg(data, c("a", "b"),
  family = c(a = "binomial"))` — and the same shape of `inverse_two_level_factors` — died with
  "subscript out of bounds" where the documented rule is that an unnamed outcome takes the default. A
  *positional* `inverse_two_level_factors` was unusable for the same reason.
* **A model formula given beside `predictors` reported the wrong error.** `tab_reg(data, y ~ x,
  list(m1 = "a"))` died on an internal assertion instead of saying "provide either a formula in
  `dependent` or `predictors`, not both".
* **A `tab_reg()` table's own record could contradict its own column header.** With
  `color = "adjustment"` (which turns `empirical` on), the stored effect word was captured before that
  and the column header after it, so the two disagreed (`AME` vs `Model_AME (adjusted %)`).
* **A table with no column variable could not be transposed**: `tab_html(tab(data, marital),
  transpose = TRUE)` aborted with "subscript out of bounds".
* **A custom total-column name containing a regular-expression character** (e.g.
  `total_names = c("Total", "Total (n)")`) was interpolated into a pattern when the lone total column
  was renamed, so it could fail to be recognised.
* **`options(tabxplor.stars = TRUE)` did not reach `tab_num()`**, although it reached `tab()` for the
  same table: the option was read too late to decide whether a reference interval is needed, so the
  two produced different numbers.
* **`tab_num()` and `tab_plain()` recorded nothing about the table they built** — no table kind, and
  no weight name, so a directly-built weighted `tab_num()` printed no "Weighted by …" footer.
* **`tab_num(color = "after_ci")` dropped the significance policy** the combined value carries, so its
  cells were coloured without the greying `tab()` applies to the same request.
* **`tab_counts()` stored a `color_signif` policy it never applied** when `ci` anchored nothing to test
  (`"cell"` / `"no"`): the table claimed a significance gate its colours did not use. It now informs
  and disables it, as `tab()` does.
* **`tab(filter = )` accepted only a character string.** A bare expression (`filter = !is.na(x)`) was
  evaluated in the caller's frame instead of the data, and aborted with "object not found" — although
  that is the form the documentation shows. Both forms work now, and an expression may reference the
  caller's own variables.
* **A transposed regression table's model-fit footer rendered grey in HTML**, where the untransposed
  one keeps it black: the transpose dropped the per-cell "reading anchor" flags, and a silent fallback
  hid it.
* **`theme = "print"` on `tab_html(engine = "kableExtra")` rendered a black table** — the
  black-and-white publication palette got the dark theme.
* **`tab_spread()` left the table's tests pointing at columns that no longer exist**, so a spread
  cross-table lost its whole test summary (chi-squared, effect size, p-value).
* **`tab_plot()`'s colour legend ignored the palette's typography** and forced every token bold, so
  under `theme = "print"` — where direction is encoded as bold vs italic on black text — the legend
  became unreadable.

* **`color = "auto"` works beside `color_signif`**, and now means exactly what `color = TRUE` means.
  The combination used to abort with *"Unknown color measure"* — on cross-tables and on mean tables
  alike.
* **`tab_num(ci = "ref")` colours its cells.** With the default `color = "auto"` the table came out
  entirely uncoloured.
* **`dplyr::bind_rows()` on two subtabled (grouped) tables no longer loses everything below the
  table**: the weight footnote, the colour legend, the confidence-interval note, the test summary and
  a stored caption all survive now. Plain tables were already fine.
* **`ref` / `ref2` accept `"last"`**, the mirror of `"first"`: the last *level* of the row or column
  variable (a total row/column is never selected — that is `"tot"`). It used to be silently treated
  as a regular expression, so it matched nothing and produced an empty comparison plus a confusing
  warning.
* Adding a **count column to a percentage column** (`tab$n + tab$pct`) warned about the mismatch and
  then aborted; it now just warns.
* A factor carrying **`NA` as a real level** (`factor(..., exclude = NULL)`, common in imported data) no
  longer crashes `print()` / `format()` / any export.
* `ci_method = c(cell = "beta")` under a `survey` design now applies Korn & Graubard's
  degrees-of-freedom rescale, as `survey::svyciprop(method = "beta")` does. On a design built on few
  clusters its intervals were measurably too short. Unchanged without a design.
* `tab()` accepts a **`data.table`** as input, and a **logical `col_var`**.
* **Clearer errors** for an unknown named `ref`, a variable used as both a tab and a row/column variable,
  and an all-zero / all-`NA` weight.
* The **`lang` argument now works on Linux** (`lang = "fr"` used to return an English legend).
* In `tab_reg()`, a **logical predictor** rendered as an empty row, and the `Constant` row lost its bold
  when `empirical = TRUE`.
* `color = TRUE` on an odds-ratio table with **two or more factor `col_vars`** silently coloured on
  the difference instead of the odds ratio.
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
* **`color = "contrib"` significance now accounts for the sample design.** It always used the
  weighting-only effective size, so a stratified + clustered table and a flat one gave identical
  cell p-values while their confidence intervals differed. Where the row variable is defined at the
  cluster level (a geography, a school, an establishment — the commonest reason to have clusters)
  this overstated the residual by a factor 2.5 in our test case, colouring cells that should be
  greyed. Note some cells may now go **uncoloured** where the smaller base takes their expected count
  below 1.
* **A table with several `row_vars` kept none of its inference metadata**, so its footer stated the
  *opposite* of what was computed (an interval accounting for the weighting, described as not doing
  so), and a `tab_plain(design) |> tab_ci()` pipeline lost the design's degrees of freedom — intervals
  9% too narrow with few clusters.
* **A weighted table with `tab_vars` and `totaltab = "table"`** silently lost the whole-table
  ("Ensemble") test row.
* **Pre-aggregated counts** (`tab_counts(wt_counts = )`) no longer report a design-based p-value they
  cannot support: such a table states, and now consistently uses, the unweighted sample size.
* One degraded survey design anywhere in a session used to mislabel **every later `tab_reg()`** as
  having failed to compute its design variance.
* `tab_reg()`'s observed (`Obs_*`) columns now **store the effective sample size** their intervals were
  computed on, in the `n_eff` field, instead of discarding it; and the multinomial crude tooltip uses
  the same interval method as the column beside it.
* `tab_reg(trials = "<column name>")` now gives a clear error naming the argument, instead of failing
  deep inside `glm()`.
* **The colour legend no longer names an interval method the bounds were not built with.** Each column
  now records its own (`get_ci_method()`), so a mean's one-sample cell interval is called a Student t
  interval rather than a Welch one, and a Poisson crude rate ratio is called Katz rather than Wald.

## Deprecations

### Soft-deprecated

* `tab(total_names =)`, `tab(totaltab_name =)` and `tab(other_level =)` — use
  `options(tabxplor.total_names =)`. Same on `tab_plain()`, `tab_num()` and `tab_counts()`.
* `options(tabxplor.signif_levels)` / `options(tabxplor.signif_labels)` — give the ladder to
  `options(tabxplor.stars)` as a named vector.

### Removed

* **The `kableExtra` HTML engine is gone.** `tab_html()` / `tab_kable()` render through the
  dependency-free engine that has been the default since the beta, whose every look is a CSS class
  you can restyle (`tab_css()`) and which is the only one that can follow a `theme = "auto"` toggle.
  `engine =` is accepted and ignored with a message, and the options
  `tabxplor.tab_kable_engine`, `tabxplor.always_add_css_in_tab_kable` and
  `tabxplor.kable_html_font` are removed. **kableExtra** remains an optional dependency: its print
  method is what opens a table in the Viewer and binds the tooltips.
* **`kable_tabxplor_style()` is defunct** — use `tab_html()`, which renders any table (a `tabxplor_tab`
  or a plain data.frame) with colours, tooltips and spanning headers.
* **`color_type` and `html_24_bit` are no longer arguments** of `tab_html()` / `tab_md()` /
  `tab_xl()` / `tab_plot()` / `tab_css()` / `tab_export()`; nor are `html_font` and `full_width`.
  They had been inert since the beta. Passing one still works and reports it once: the colour
  *channel* is chosen by `color = c(text, background)`, and font and width are CSS rules
  (see `tab_css()`).
* **`tab_css(chrome =)` is now `tab_css(format = c("html", "md"))`**, which says what it is for:
  `"html"` (the default) is the full stylesheet, `"md"` the colour classes only. **`tab_md_css()` is
  removed** — it was `tab_css(format = "md")` under a name you had to already know.
* **`tab_logit()` and `multi_logit()` are removed** — use `tab_reg(family = "binomial")`
  (`multi_logit(models = )` is `tab_reg(predictors = <named list>)`). They were thin wrappers that
  exposed only part of `tab_reg()`, so `effect`, `measure`, `compare`, `baseline`, `reference` and
  `color` were out of reach through them.
* **The option `tabxplor.color_style_type` is removed** (it was documented but never set, and only
  ever emitted its own deprecation warning). The colour channel is `color = c(text, background)`.

### Hard-deprecated (defunct in 2.1.0)

* **The step-by-step chain** — `tab_pct()` / `tab_tot()` / `tab_totaltab()` / `tab_ci()` /
  `tab_chi2()` — now warns on every call. What goes away is the *chaining API*, not the statistics:
  `tab()` and `tab_num()` compute percentages, differences, confidence intervals and the whole-table
  test in one pass, with the same arithmetic, and the numbers are identical.
* `tab_prepare()`, `complete_partial_totals()` and `fct_recode_helper()` will become internal or be
  removed in 2.1.0. `tab_prepare()`'s work is done by `tab()` itself (`na_drop_all` is
  `filter = !is.na(...)`; `cleannames`, `other_if_less_than` and `other_level` are `tab()` arguments).

### Soft-deprecated

* **The `in_totrow` cell field is replaced by `row_kind`**, which says what kind of row a cell sits
  in (`"data"` / `"total"` and the synthetic display rows `"n"`, `"pct"`, `"pvalue"`, `"gof"`,
  `"blank"`). `is_totrow()` and `as_totrow()` are unchanged, `x$in_totrow` still returns the logical,
  and `fmt(in_totrow = )` is soft-deprecated in favour of `fmt(row_kind = )`.
* `tab_many()` — now a thin shim over `tab()`, translating the five arguments that were renamed
  (`chi2` → `test`, `totrow` / `totcol` → `tot`, `compact` → `output_list`, and
  `na_drop_all = c(a, b)` → `filter = !is.na(a) & !is.na(b)`). Only `data`, `row_vars`, `col_vars`,
  `tab_vars` and `wt` may be passed by position; everything else must be named.
* Singular `row_var` / `col_var`; `tab(sup_cols =)` (use `col_vars =`); `tab(filter =)` (filter
  upstream); `tab(names_prefix =, names_sort =)` (they belong to `tab_spread()`).
* `tab_pct()` / `tab_tot()` / `tab_totaltab()` / **`tab_ci()`** / **`tab_chi2()`** — the whole
  step-by-step chain is superseded. Confidence intervals and the whole-table test are computed by the
  build itself, from `tab()`'s `ci` / `ci_method` / `conf_level` / `stars` / `test` / `color`
  arguments. The steps still work on an existing table and give the same numbers.
* `tab_transpose()` (use `transpose = TRUE`); `tab_plot()`.
* Renamed arguments: `chi2` → `test`, `tab_xl(print_color_legend =)` →`color_legend =`,
  `method_cell` / `method_diff` → `ci_method = c(cell =, diff =)`. `set_diff_type()` → `set_ref_type()`,
  which shares its stem with `get_ref_type()` and with the `ref` attribute both address.
* The combined colour strings `"diff_ci"` / `"after_ci"` / `"ci"` (use `color = "difference"` +
  `color_signif =`); `color_type` (now inert).
* **`tab(OR =)`** — `"OR"` / `"OR_pct"` map to `display = "{or}"` / `"{or} ({pct})"`, `"cumOR"` to
  `ref2 = "cumulative"`. **`ci = "diff"` / `"ratio"`** map to `ci = "ref"` (`"ratio"` also keeps its
  Katz bounds; `color = "ratio"` is the way to ask for them).
* `color`'s canonical values are the full words — `"difference"`, `"ratio"`, `"odds_ratio"`,
  `"contrib"`. The acronyms (`"diff"`, `"OR"`, `"or"`) are permanent aliases, not deprecations, but a
  built table now stores and its legend now names the full word.

Removed / defunct (now error):

* `tab_xl(n_min =, hide_near_zero =)` (long inert); the little-used `totcol` vector
  forms; the `tabxplor.compact` option (use `output_list =`); `tab_num(ci_scale =)` (a duplicate of
  `color = "ratio"`).
* `method_ratio` / `method_mean_diff` / `method_mean_ratio` (never released; use `ci_method`).
  A proportion *ratio* has only one interval (Katz), so it was never a choice.
* `or_plot()` (never released; use `forest_plot()`, which draws every family and effect, follows
  `set_color_breaks()` and returns a modifiable `ggplot`).
* `ids` / `strata` / `fpc` / `nest` on `tab()`, `tab_reg()`, `tab_logit()` and `multi_logit()`, and
  `test = "survey"` (pass a `survey::svydesign()` as `data` — see above).
* **The `fmt` column attributes `type` and `ci_type`, with `get_type()` / `set_type()` /
  `get_ci_type()` / `set_ci_type()` and `fmt()`'s `type =` / `ci_type =` arguments.** `type`
  conflated two facts and is **split in two**: `scale =` says what the column estimates
  (`"level_pct"`, `"level_mean"`, `"level_n"`, `"points"`, `"mean_diff"`, `"raw_diff"`,
  `"pct_ratio"`, `"mean_ratio"`, `"odds_ratio"`, `"log_coef"`), and `pct_type =` which kind of
  percentage it is (`"row"` / `"col"` / `"all"` / `"all_tabs"` / `"none"`), read with `get_scale()` /
  `get_pct_type()`. `ci_type` is gone rather than renamed: the stored interval is always on the
  estimate's own scale, and whether a column *has* one is read from its bounds. `fmt()` answers a
  `type =` / `ci_type =` call with the mapping to the new arguments. This only concerns code that
  builds or inspects `fmt` vectors directly (see `vignette("tabxplor-programming")`); every `tab()`
  and `tab_reg()` table is unchanged, cell for cell.


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
