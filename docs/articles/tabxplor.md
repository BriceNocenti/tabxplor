# Introduction to tabxplor

The first time only, install the package :

``` r
install.packages("tabxplor", dependencies = TRUE)
```

At the start of each new R session, load it :

``` r
library(tabxplor)
```

`tabxplor` helps you **explore data with cross-tables, coloring the
cells so you can read a table at a glance**. Over-represented cells use
shades of blue, under-represented ones turn red/orange, so patterns jump
out without you squinting at every number.

Everything is a `tibble`, so the result works with the usual `dplyr`
verbs, and tables export to Excel, HTML and Markdown with their color
helpers. Underlying heavy computations run on `data.table`

Throughout this vignette we use `gss_simple`, a cleaned-up version of
the US General Social Survey
([`forcats::gss_cat`](https://forcats.tidyverse.org/reference/gss_cat.html))
with factors levels merged and reordered.

``` r
gss_simple <- gss_cat_data_formatting()
```

## Your first cross-tables

[`tab()`](https://bricenocenti.github.io/tabxplor/reference/tab.md)
needs a data frame, a **row variable** and a **column variable**. By
default it shows counts:

``` r
tab(gss_simple, marital, race)
```

[TABLE]

This is tabxplor’s **html table** — what you get in the RStudio or
Positron Viewer pane with the recommended session option, used
throughout this vignette:

``` r
options(tabxplor.print = "html")
```

Without the option, the same table prints in the **console**, as a
colored `tibble` — same information, lighter display:

``` r
tab(gss_simple, marital, race)
```

``` r-output
#> # A tabxplor tab: 7 × 5
#>   marital        White Black Other  Total
#>   <fct>            <n>   <n>   <n>    <n>
#> 1 Married        8 316   869   932 10 117
#> 2 Separated        437   196   110    743
#> 3 Divorced       2 676   495   212  3 383
#> 4 Widowed        1 475   262    70  1 807
#> 5 Never married  3 478 1 305   633  5 416
#> 6 NA                13     2     2     17
#> 7 Total         16 395 3 129 1 959 21 483
```

Add `pct = "row"` for row percentages (or `"col"` for column
percentages). A **Total** row/column and a count column (`n`) are added
automatically:

``` r
tab(gss_simple, marital, race, pct = "row")
```

[TABLE]

When the column variable is **numeric**,
[`tab()`](https://bricenocenti.github.io/tabxplor/reference/tab.md)
shows its **mean** in each row instead of percentages:

``` r
tab(gss_simple, marital, age)
```

|               | age       |
|---------------|-----------|
| marital       | mean (sd) |
| Married       | 49 (σ15)  |
| Separated     | 45 (σ13)  |
| Divorced      | 51 (σ13)  |
| Widowed       | 72 (σ13)  |
| Never married | 34 (σ13)  |
| NA            | 52 (σ17)  |
| Total         | 47 (σ17)  |

You can pass **several row and column variables at once**.

``` r
tab(gss_simple, c(race, relig), c(party3, tvhours), na = "drop", pct = "row")
```

[TABLE]

`levels = "first"` keeps only the first level of each column factor,
which is handy to display many binary factors, like survey questions
with multiple answers, all at once, in a compact way :

``` r
tab(gss_simple, relig, c(married, black, income25k), pct = "row", levels = "first", na = "drop", cleannames = TRUE)
```

|                   | married | black | income25k       |
|-------------------|---------|-------|-----------------|
| relig             | Married | Black | \$25000 or more |
| Protestant        | 50%     | 21%   | 32%             |
| Catholic          | 50%     | 4%    | 35%             |
| Other christian   | 44%     | 18%   | 35%             |
| Jewish            | 51%     | 3%    | 43%             |
| Buddhist/Hinduist | 51%     | 5%    | 47%             |
| Muslim            | 53%     | 34%   | 32%             |
| Other             | 37%     | 13%   | 37%             |
| None              | 37%     | 11%   | 37%             |
| Total             | 47%     | 15%   | 34%             |

A few other everyday arguments: `na = "drop"` to drop missing values
from the base, `digits =` for the number of decimals, and
`cleannames = TRUE` to strip prefixes like `"1-"` from level names. See
[`?tab`](https://bricenocenti.github.io/tabxplor/reference/tab.md) for
the full list.

## Weights

The argument `wt =` adds a survey weight. Every percentage and mean is
then **weighted**, while the sample size behind the confidence intervals
stays the real, **unweighted** number of cases — the honest default
basis for uncertainty.

``` r
data(hdv2003, package = "questionr")
tab(hdv2003, nivetud, occup, wt = poids, pct = "row", na="drop", digits = 1)
```

Under **unequal** weights that default interval carries no design
effect, so it runs a little too narrow. Kish’s *effective sample size*
fixes this : it counts each observation by how much it really
contributes, `n_eff = (sum of w)² / (sum of w²)` (always at most the
real *n*), and uses `n_eff` in place of the raw *n*. Turn it on and
every weighted confidence interval in the table — proportions and means
alike — widens honestly:

``` r
options(tabxplor.kish_neff = TRUE)
```

This is a simple single-stage approximation (it needs the individual
weights, so it is not available for tables built from pre-aggregated
counts). A **fully design-based** result — clusters, strata, exact
standard errors — is available for Chi2 pvalues (factors) and ANOVA F
pvalues (numeric column variables) only with `test = "survey"` and the
related arguments (see the `test =` argument) : confidence intervals are
not covered. See the [survey](https://CRAN.R-project.org/package=survey)
package for more informations about survey design.
[`tab_reg()`](https://bricenocenti.github.io/tabxplor/reference/tab_reg.md)
regressions tables with weights always use design-effect for standard
errors
([`vignette("tabxplor-reg")`](https://bricenocenti.github.io/tabxplor/articles/tabxplor-reg.md)),
but the base version only use Kish’s effective sample size.

## Sub-tables

Give [`tab()`](https://bricenocenti.github.io/tabxplor/reference/tab.md)
a third variable as `tab_vars` and it builds **one sub-table per group**
(here, one per income group). The result is *grouped*: `dplyr`
operations then run within each sub-table.

``` r
tab(gss_simple, race, party3, rincome, na = "drop", pct = "row")
```

[TABLE]

When you pass several **row variables** *without* `tab_vars`,
[`tab()`](https://bricenocenti.github.io/tabxplor/reference/tab.md)
merges the mirror tables into a single table by default.
`output_list = TRUE` returns instead a **list with one table per row
variable** (with `tab_vars`, the result is always a list):

``` r
tab(gss_simple, c(married, income25k), race, pct = "row", output_list = TRUE)
```

## colors: reading helpers

One you the main purposes of `tabxplor` is to provide a full palette of
color helpers for data exploration. `color = "diff"` colors each cell by
**how far it sits from its reference** — by default the Total of its row
or column. Cells clearly above the average turn **blue**, cells clearly
below turn **red/orange** — the further a cell sits from its reference,
the stronger the shade — and a color legend is printed underneath.

``` r
tab(gss_simple, race, party3, pct = "row", color = "diff")
```

[TABLE]

`color = TRUE` picks a sensible scheme automatically for each column
type (both differences and ratio for percentages, only ratios for means,
…), check which one in the legend:

``` r
tab(gss_simple, rincome, c(party3, marital), pct = "row", color = TRUE)
```

[TABLE]

Numeric columns are colored the same way, on their **means** (here,
hours of TV per day by income):

``` r
tab(gss_simple, rincome, tvhours, color = "diff")
```

[TABLE]

**Which cell is the reference for comparison ?** By default each cell is
compared to the relevant Total (Total row for row percentages and Total
column for column percentages) to highlight over-representations and
under-representations. Two useful alternatives:

- `ref = 1` compares each row to the **first row** — perfect for reading
  an evolution over time or an ordinal factor.
- with sub-tables, `comp = "all"` compares against the **overall** Total
  instead of each sub-table’s own Total.

``` r
tab(gss_simple, year, marital, pct = "row", color = "diff", ref = 1)
```

[TABLE]

``` r
tab(gss_simple, rincome, party3, race, na = "drop", pct = "row", color = TRUE, comp="all")
```

[TABLE]

**A different reference for each variable.** `ref` is reinterpreted by
`pct`. Under **row** percentages (or means) it picks a reference
**row**, so a *named* vector gives each row variable its own — here
`race` is read against its first row, `relig` against its Total:

``` r
tab(gss_simple, c(race, relig), party3, pct = "row", color = "diff",
    ref = c(race = 1, relig = "tot"), na = "drop")
```

[TABLE]

Under **column** percentages `ref` picks a reference **column** instead,
vectorised over the column variables — either named
(`ref = c(party3 = "first", marital = "tot")`) or positional, one value
per column variable:

``` r
tab(gss_simple, race, c(party3, marital), pct = "col", color = "diff",
    ref = c("first", "tot"), na = "drop")
```

[TABLE]

Color thresholds and the palette can be customised : set them **once for
the whole session** with
[`set_color_breaks()`](https://bricenocenti.github.io/tabxplor/reference/tab_many.md)
and
[`set_color_palette()`](https://bricenocenti.github.io/tabxplor/reference/tab_many.md).

## colors that respect significance

The colors above show the *size* of a deviation, but not whether it is
**statistically reliable**. On small samples a big-looking difference
can be pure noise. The `color_signif` argument brings significance into
the coloring:

- `"ignore"` (default): color every deviation by its observed size. Grey
  out small differences below a certain threshold.
- `"grey_non_signif"`: color by size of the effect, grey out small
  effects below a certain threshold, but also **grey out cells with
  important effects that are not significant**. Every colored cell is
  then guaranteed to be significantly different from its reference,
  without being bothered by very small significant differences.
- `"guaranteed_effect"`: color only by the part of the effect you can be
  confident about (its confidence bound), with dimmer, conservative
  colors. Use it on **small samples** to **highlight all the differences
  you have the right to interpret**. Everything colored is significant ;
  nothing grey is.

``` r
tab(gss_simple, race, party3, pct = "row", color = "diff", color_signif = "grey_non_signif")
```

[TABLE]

``` r
gss_simple |>
  dplyr::filter(year == "2012") |> # n=1 974
  tab(race, party3, pct = "row", color = "diff", color_signif = "guaranteed_effect")
```

[TABLE]

On **small samples** a big-looking percentage can rest on a handful of
respondents. `n_min =` is a purely visual filter, applied last: it hides
cells whose (unweighted) base is below the threshold and drops a row
entirely when its largest base falls short. Here the rarest religions
drop out:

``` r
tab(gss_simple, relig, race, pct = "row", n_min = 400)
```

[TABLE]

An alternative is to keep the small rows and cols but group them all in
a “Other” level :

``` r
tab(gss_simple, relig, race, pct = "row",  other_if_less_than = 400)
```

[TABLE]

## Confidence intervals, tests and contributions

Print confidence intervals for the percentage or mean of each cell with
`ci = "cell"` :

``` r
tab(gss_simple, race, party3, pct = "row", ci = "cell") # by default, conf_level = 0.95
```

[TABLE]

Print the confidence intervals of the **difference** with a reference,
used to calculate significance (if 0 belongs to the confidence interval,
the cell is not significantly different from the reference, here the
Total row) :

``` r
gss_simple |>
  dplyr::filter(year == "2012") |> # n=1 974
  tab(race, party3, pct = "row", color = TRUE, color_signif = "guaranteed_effect",
      display = "num_ci" # "{pct} {ci}"
  )
```

[TABLE]

`display = "num_ci"` is a type-adaptive shorthand for this: it shows
each value with whatever confidence interval the table computes —
`{pct} {ci}` on percentage columns and `{mean} {ci}` on numeric columns,
chosen per column — so it works for a mix of factors and numbers in one
call:

Add significance stars with `stars = TRUE`. They tell the same story
than confidence intervals of the difference with the reference, but for
different confidence levels (99%, 95%, 90%) :

``` r
gss_simple |>
  dplyr::filter(year == "2012") |> # n=1 974
  tab(rincome, c(party3, tvhours), pct = "row", display = "num_ci", stars = TRUE)
```

[TABLE]

**Show two numbers in one cell.** `display` is not just for confidence
intervals: it takes a **[`{}`](https://rdrr.io/r/base/Paren.html)
template** that combines any cell fields. For example
`display = "{pct} ({diff})"` prints each percentage followed by its
difference from the reference, and `"{pct} (n={n})"` follows it with the
count:

``` r
tab(gss_simple, race, party3, pct = "row", color = "diff", display = "{pct} ({diff})")
```

[TABLE]

The first field in the template is the *primary* one — the value Excel
keeps and the one the colours read.
[`vignette("tabxplor-programming")`](https://bricenocenti.github.io/tabxplor/articles/tabxplor-programming.md)
explains the full grammar and lists every field you can combine.

`test = TRUE` adds a statistical test of independence per (sub-)table —
**Chi-squared** for factor columns, **Welch’s F ANOVA** for numeric
variables (`options(tabxplor.anova = "classic")` switches to the pooled
F):

``` r
tab(gss_simple, race, c(party3, tvhours), pct = "row", test = TRUE)
```

[TABLE]

`color = "contrib"` colors cells by their **contribution to the
Chi-squared** — the cells that would stand out in a correspondence
analysis :

``` r
tab(gss_simple, race, party3, color = "contrib")
```

[TABLE]

``` r
# tab(gss_simple, race, party3, pct="row", color = "contrib") # works with pct, but independent from rows/columns
```

See below for the detail of how confidence intervals and colors can be
composed.

## Hover tooltips (html tables)

Every html table carries per-cell hover **tooltips** with the numbers
behind the cell: the unweighted count, the difference from the
reference, the ratio, the confidence interval… They are on by default in
the Viewer and in reports — this vignette only switched them off
document-wide with `options(tabxplor.tab_kable_tooltips = FALSE)`, to
keep the page light. Hover the cells of the table below, where they are
switched back on with `tooltips = TRUE`:

``` r
tab(gss_simple, race, party3, pct = "row", color = "diff") |>
  tab_html(tooltips = TRUE)
```

[TABLE]

**A note on weights.** With a weight (`wt =`), every proportion or mean
is weighted, but by default the sample size behind the confidence
intervals and tests stays the real, **unweighted** number of
observations. Under unequal weights it carries no design effect, so it
runs a little too narrow : opt in to Kish’s effective sample size with
`options(tabxplor.kish_neff = TRUE)` (see [Weights](#weights)) to widen
every interval honestly and switch the whole-table Chi2 tests to a
Rao–Scott correction.

## Exporting

A finished table exports with its colors to Excel, HTML or Markdown:

``` r
tabs <- tab(gss_simple, race, party3, pct = "row", color = "diff")
tab_export(tabs) # default : html table (RStudio Viewer, .Rmd/.qmd, etc.)
tab_export(tabs, format = "xl", path = "table") # Excel export 
tab_export(tabs, format = "md", path = "table") # flat markdown file (pipes tables)
```

Two options are worth knowing:

- `theme = "auto"` lets an HTML or Markdown export **follow the reader’s
  light/dark mode** (it flips live). For the console,
  `set_color_palette(theme = "auto")` detects the editor (RStudio,
  Positron, etc.) and picks the matching palette — it is applied
  automatically when the package loads.

``` r
tab_export(tabs, theme = "auto") # HTML that follows the reader's light/dark modes
```

- Since numeric variables can only be passed in columns, some complex
  layout with numeric variables in rows need to transpose the table
  during export using `transpose = TRUE` :

``` r
tab(gss_simple, party3, c(race, tvhours), pct = "row") |>
  tab_html(transpose = TRUE)
```

[TABLE]

- **One stylesheet for a whole document.** In an `.Rmd`/`.qmd` report,
  [`tab_css()`](https://bricenocenti.github.io/tabxplor/reference/tab_css.md)
  writes the colour CSS once and every later table emits only classes,
  so a single `theme` — including `"auto"`, which follows the reader’s
  light/dark mode — styles every table at once. This very vignette does
  exactly that (with `theme = "light"`):

``` r
options(tabxplor.tab_kable_css = FALSE)
tab_css(theme = "auto")   # emit once, near the top of the document
```

Nothing is written inline on a cell, so any look is overridable with
plain CSS afterwards (column widths, fonts…); see
[`?tab_css`](https://bricenocenti.github.io/tabxplor/reference/tab_css.md)
for the role classes (`.tx-rv`, `.tx-tot`, `.tx-num`).

## Working with the result

[`tab()`](https://bricenocenti.github.io/tabxplor/reference/tab.md)
returns a `tibble` (of class `tabxplor_tab`), so `dplyr` verbs just
work. Use the helper
[`is_totrow()`](https://bricenocenti.github.io/tabxplor/reference/fmt.md)
to keep the Total row in place when you re-order (it flags total rows,
so sorting on it first sends them to the bottom):

``` r
library(dplyr)
tab(gss_simple, race, marital, pct = "row") |>
  arrange(desc(Married))
```

[TABLE]

**Titling and annotating.** `subtext =` prints one or more legend lines
under a table (a data source, a note).
[`set_caption()`](https://bricenocenti.github.io/tabxplor/reference/set_caption.md)
gives a table a **title that survives a dplyr pipeline**, and every
exporter uses it as the table title:

``` r
tab(gss_simple, race, marital, pct = "row", subtext = "Source: GSS, 2000-2014") |>
  set_caption("Custom title")
```

Custom title

[TABLE]

## Confidence intervals and colors composition : variable_type × `color` × `color_signif`

This section is the reference behind the two color sections above. It
shows how the parameters fit together — the **variable type**, the
**measure** you color (`color`), and the **significance policy**
(`color_signif`).

The **type** is set by the column variable — a **factor** (percentages
are computed, `pct`) or a **numeric** (means are computed, `mean`). The
row variable is always turned into a factor.

Every colored table answers three questions:

- **How to measure deviation?** `color =` picks what a color *means*:
  `"diff"` (distance from the reference), `"ratio"` (relative risk for
  percentages, mean ratio for means), `"contrib"` (weight in the
  Chi-squared), `"OR"` (odds ratio). `color = TRUE` picks a sensible one
  per column type.
- **How confident are we in this measure?** every color reads one
  **confidence interval** at `conf_level` (95% by default). A cell is
  *significant* when that interval excludes its neutral value — **0**
  for a difference, **1** for a ratio or odds ratio. The printed
  bracket, the significance stars and the greying all read that same
  interval, so they can never disagree.
- **How to show significance?** `color_signif` — `"ignore"`,
  `"grey_non_signif"` or `"guaranteed_effect"` ; `stars = TRUE` to use
  significance stars instead of, or stacked with, colors.

**The confidence interval used for the colors and stars** compares each
cell to its reference cell (by default, the corresponding cell in the
Total row or Total column):

| type | color | what the color measures | confidence interval (default) |
|----|----|----|----|
| pct | `diff` | cell % - reference % (percentage points) | Newcombe hybrid-score |
| pct | `ratio` | cell % / reference % (relative risk) | Katz log-risk-ratio |
| pct | `OR` | empirical odds ratio | Woolf log-odds-ratio |
| pct | `contrib` | signed χ² contribution (no reference) | — (standardized residual) |
| mean | `diff` | cell mean - reference mean (SD units) | Welch *t* |
| mean | `ratio` | cell mean / reference mean | robust ratio-of-means |

Alternative interval methods (see
[`?tab_ci`](https://bricenocenti.github.io/tabxplor/reference/tab_ci.md)):

- `method_diff = "ac"` (Agresti-Caffo) or `"wald"` for a percentage
  difference;
- `method_mean_diff = "student"` (pooled, the OLS two-group interval)
  for a mean difference;
- `method_mean_ratio = "quasipoisson"` or `"poisson"` for a mean ratio.
- The relative-risk (`method_ratio = "katz"`) and odds-ratio (Woolf)
  intervals have no alternative.
- A mean **difference** is colored **standardized** — Glass’s Δ, the
  difference divided by the reference’s standard deviation — so the
  `mean_diff` color breaks are read in SD units, unless the user
  provides a custom break scale.
- For a factor with **3 or more levels**, the odds ratio (and its
  interval) compares each level to the `ref2` baseline level : it is a
  relative risk ratio (RRR) (the same observed quantity that is
  modelised by a multinomial logistic regression). The OR interval is
  computed only when `color_signif` or `stars` needs it.

**Simple, cell-by-cell confidence intervals** (`ci = "cell"`) compare
each cell to 0 % (or a mean of 0), *not* to a reference:

| type | color  | confidence interval (default) | other method                    |
|------|--------|-------------------------------|---------------------------------|
| pct  | `cell` | Wilson score interval         | `method_cell = "wald"` (normal) |
| mean | `cell` | one-sample Student *t* (n-1)  | —                               |

Because they compare to 0 and not to a reference, cell intervals are
purely descriptive: they carry **no significance and no stars**.
`method_cell` chooses `"wilson"` (default) or `"wald"` for percentages;
a mean cell interval is always the one-sample Student *t*.

**`color_signif` turns that interval into a coloring policy.**

- `"ignore"` colors **every** cell by its **observed effect** size, for
  example the observed difference compared to the Total row. Grey cells
  have an observed effect below the threshold (for example, differences
  of less than ±5 points of percentages).
- `"grey_non_signif"` and `"guaranteed_effect"` both color **only
  significant cells**, but differ in the *intensity basis*:
  - `grey_non_signif` colors by the **observed** effect, like “ignore”,
    greying out small deviations, but it also **greys out any large
    deviation that turns to be non-significant**. Ideal for large
    samples.
  - `guaranteed_effect` colors by the **guaranteed** effect — the
    confidence-bound (CI-floor), the **smaller deviation that is assured
    at a given confidence level** (default 95%) — so its colors are
    dimmer and conservative, but **all significative differences are
    colored**, which is ideal for small samples.

| type | color | `="ignore"` | `="grey_non_signif"` | `="guaranteed_effect"` |
|----|----|----|----|----|
| pct | `diff` | observed diff | grey if the diff-CI contains 0 | diff CI-floor |
| pct | `ratio` | observed ratio | grey if the ratio-CI contains 1 | ratio CI-floor |
| pct | `OR` | observed OR | grey if the OR-CI contains 1 | OR CI-floor |
| pct | `contrib` | χ² contribution | grey if residual \< 1.96 (conf 95%) | residual \>= 1.96 (conf 95%) |
| mean | `diff` | observed diff | grey if the diff-CI contains 0 | diff CI-floor |
| mean | `ratio` | observed ratio | grey if the ratio-CI contains 1 | ratio CI-floor |

Examples :

``` r
# --- factors: percentages -------------------------------------------------
tab(gss_simple, race, party3, pct = "row", color = "diff",  color_signif = "grey_non_signif")
tab(gss_simple, race, party3, pct = "row", color = "ratio", color_signif = "guaranteed_effect")
tab(gss_simple, rincome, married, pct = "row", color = "OR", OR = TRUE, ref2 = 1)
tab(gss_simple, rincome, party3, color = "contrib")   # works with pct = "row"/"col" too

# --- numerics: means ------------------------------------------------------
tab(gss_simple, rincome, tvhours, color = "diff",  color_signif = "guaranteed_effect")
tab(gss_simple, rincome, tvhours, color = "ratio", color_signif = "grey_non_signif")

#    a custom scale for differences in means, and a "first row" reference
tab(gss_simple, rincome, tvhours, color = "diff", color_signif = "grey_non_signif",
    color_breaks = list(mean_diff = c(0.4, 0.8, 1.6)), ref = 1)
```

## Session options

A handful of [`options()`](https://rdrr.io/r/base/options.html) set your
preferred defaults once for the whole session — put them at the top of a
script, or in your `.Rprofile`. Each one has a per-call argument too;
the option just changes the default. The everyday ones:

- `options(tabxplor.print = "html")` — print tables not in console, but
  as html in RStudio or Positron Viewer Pane by default (recommended)
- `options(tabxplor.cleannames = TRUE)` — strip `"1-"`-style prefixes
  from level names everywhere.
- `options(tabxplor.parallel = 8)` — parallelise tables with multiples
  variables on different CPU cores by default (needs `mirai`)
- `options(tabxplor.var_labels = TRUE)` — in exports, show a variable’s
  label (from `haven`/`labelled` data) instead of its bare name.
- `options(tabxplor.theme = "auto")` — the export theme
  (`"light"`/`"dark"`/`"auto"`); `set_color_palette(theme = "auto")`
  does the same for the console.
- `options(tabxplor.stars = TRUE)` — show significance stars in every
  table (like `stars = TRUE`).
- `options(tabxplor.conf_level = 0.9)` — the confidence level for
  intervals and tests (default `0.95`).
- `options(tabxplor.ci_print = "moe")` — print a confidence interval as
  a `pct ± margin of error` instead of a `[low; high]` bracket.
- `options(tabxplor.lang = "fr")` — the language of the colour legends
  and footers (`"auto"`/`"en"`/`"fr"`).

Colour thresholds and palettes have their own helpers,
[`set_color_breaks()`](https://bricenocenti.github.io/tabxplor/reference/tab_many.md)
and
[`set_color_palette()`](https://bricenocenti.github.io/tabxplor/reference/tab_many.md).
`?tabxplor-options` documents every option, and
[`vignette("tabxplor-programming")`](https://bricenocenti.github.io/tabxplor/articles/tabxplor-programming.md)
covers the more advanced ones (export fonts, parallel builds…).

## A point-and-click interface (jamovi)

Everything above is also available **without writing R code**, through a
[jamovi](https://www.jamovi.org/download) module. jamovi is a free,
open-source statistical software : install it, open the modules menu
(the **`+`** at the top-right), choose **jamovi library**, and install
*tabxplor*. It adds a **Crosstables** analysis — and a **Regressions**
analysis powered by
[`tab_reg()`](https://bricenocenti.github.io/tabxplor/reference/tab_reg.md)
— with the same coloured, exportable tables, driven entirely by menus.
Handy for teaching, or for colleagues who do not use R.

## Where to go next

- [`vignette("tabxplor-reg")`](https://bricenocenti.github.io/tabxplor/articles/tabxplor-reg.md)
  — regression tables with
  [`tab_reg()`](https://bricenocenti.github.io/tabxplor/reference/tab_reg.md),
  and comparing modelled to observed effects.
- [`vignette("tabxplor-programming")`](https://bricenocenti.github.io/tabxplor/articles/tabxplor-programming.md)
  — the `tabxplor_fmt` cell type and how to program with its fields.
- [`?tab`](https://bricenocenti.github.io/tabxplor/reference/tab.md) for
  every argument (grouped by purpose),
  [`?tab_ci`](https://bricenocenti.github.io/tabxplor/reference/tab_ci.md)
  for confidence-interval methods, and `?tabxplor-options` for the
  package-wide defaults.
