# Cross-tables with color helpers

`tab()` builds a cross-table of one or several row variables by one or
several column variables, and colors the cells so the table is easy to
read at a glance — in the R console, or exported to Excel, HTML or Word.
Cells can show counts, row or column percentages, or (for a numeric
column variable) means, optionally with differences, confidence
intervals and tests.

The result is a `tibble` (of class `tabxplor_tab`), so every dplyr verb
keeps working on it.

New to the package? Four arguments are enough to begin — `data`,
`row_vars`, `col_vars` and `pct` — then add `color` when you want
reading helpers. The
[Introduction](https://bricenocenti.github.io/tabxplor/articles/tabxplor.html)
([`vignette("tabxplor")`](https://bricenocenti.github.io/tabxplor/articles/tabxplor.md))
works through them. Package-wide defaults are
[`options()`](https://rdrr.io/r/base/options.html), listed at
[tabxplor-options](https://bricenocenti.github.io/tabxplor/reference/tabxplor-options.md).

## Usage

``` r
tab(
  data,
  row_vars,
  col_vars,
  tab_vars,
  wt,
  ...,
  pct = "no",
  color = "no",
  color_signif = "ignore",
  test = FALSE,
  na = "keep",
  levels = "all",
  cleannames = NULL,
  other_if_less_than = 0,
  ref = "auto",
  ref2 = "first",
  comp = "tab",
  ci = "auto",
  conf_level = NULL,
  stars = NULL,
  ci_method = NULL,
  anova = NULL,
  design_effect = NULL,
  totaltab = "line",
  common_totrow = FALSE,
  n = NULL,
  n_min = 0,
  add_pct = FALSE,
  subtext = "",
  caption = NULL,
  digits = 0,
  display = NULL,
  color_breaks = NULL,
  output_list = FALSE,
  spread_vars,
  filter
)
```

## Arguments

- data:

  A data frame.

- row_vars:

  \<[tidy-select](https://tidyr.tidyverse.org/reference/tidyr_tidy_select.html)\>
  The row variable(s), printed with one level per line, and the column
  variable(s), one level per column. A numeric variable gives a single
  column of means. Each accepts one variable or several,
  `c(var1, var2)`.

- col_vars:

  \<[tidy-select](https://tidyr.tidyverse.org/reference/tidyr_tidy_select.html)\>
  The column variable(s) — see `row_vars`. **An interaction** is written
  `a*b`, as in
  [`tab_reg()`](https://bricenocenti.github.io/tabxplor/reference/tab_reg.md),
  and only `col_vars` takes one: two factors give one column per
  observed cell of the pair, a number crossed with a factor one mean
  column per level. See
  [`vignette("tabxplor")`](https://bricenocenti.github.io/tabxplor/articles/tabxplor.md).

- tab_vars:

  \<[tidy-select](https://tidyr.tidyverse.org/reference/tidyr_tidy_select.html)\>
  Tab variables: one subtable per combination of their levels. Leave
  empty for a simple cross-table.

- wt:

  A weight variable, of class numeric. Leave empty for unweighted
  results.

- ...:

  Arguments taken by name, and kept out of the signature. Past the
  variable roles every argument must be named, and an unknown name is
  refused with a suggestion. **`tot`** Which totals to show:
  `c("row", "col")` or `"both"` (default), `"row"`, `"col"`, or `"no"`
  (removed after the calculations that need them). **`shape`** How a
  **numeric** variable enters the table. Cut it into groups and it
  becomes an ordinary factor — one row (or column) per group. One value
  for every numeric variable, or one per variable:
  `shape = c(age = "quintiles")`. On the row and tab axes a number
  always gets one, `"auto"` by default; a numeric `col_vars` keeps its
  means.
  [`shape_numeric_var`](https://bricenocenti.github.io/tabxplor/reference/shape_numeric_var.md)
  lists the whole vocabulary. **`shape_name`** Whether a shaped variable
  writes its own name onto its **first** level (`"age: [18,30) low"`),
  so a table whose leading text columns are stripped still says what the
  levels are levels of. `FALSE` by default. **Retired in 2.0.0**, still
  taken by name, each warning once and saying what to use instead:
  `row_var`, `col_var`, `sup_cols`, `totaltab_name`, `total_names`,
  `OR`, `chi2`, `method_cell`, `method_diff`, `add_n`, `names_prefix`,
  `names_sort`, `other_level`. The dot-prefixed names (`.cache`,
  `.defer_level_merge`, `.return_armed`, `.levels_order`,
  `.levels_collapse`) are internal plumbing, not user arguments.

- pct:

  The percentages to calculate, as a single string or a vector the same
  length as `col_vars`: `"row"`, `"col"`, `"all"` (frequencies within
  each subtable), `"all_tabs"` (frequencies over every table) or `"no"`
  (default, counts). Everything else — the reference, the interval, the
  colour — follows from this choice.

- color:

  Which **measure of deviation** to color — a deviation being how far a
  cell sits from its reference, the measure which of the ways of
  expressing it you read. `"no"` (default, `FALSE` equivalently) prints
  no color; `TRUE` picks one per column type. Otherwise:

  - `"difference"` (`"RD"`, `"diff"`): the cell's difference from its
    reference (percentage points for factors, Glass's \\\Delta\\ for
    means).

  - `"ratio"` (`"RR"`, `"IRR"`, `"RoM"`): relative risk (factors) or
    mean ratio (numerics) vs the reference.

  - `"odds_ratio"` (`"OR"`): the odds ratio, on percentage tables,
    coloured on its own symmetric scale.

  - `"contrib"`: signed contribution to the chi-squared
    (reference-free).

  The acronyms in brackets are permanent aliases, the same words
  [`tab_reg`](https://bricenocenti.github.io/tabxplor/reference/tab_reg.md)'s
  `measure` takes. An acronym here always names a **measure**, where
  `display =` names a *field* and `ref2 =` a *level*. **Position picks
  the channel** (1st value -\> text, 2nd -\> background) and **names
  pick the column type**: `c("difference", "ratio")`,
  `c(pct = "difference", mean = "ratio")`, or both with a
  [`list()`](https://rdrr.io/r/base/list.html). Only `difference` /
  `ratio` may go on the background; thresholds come from
  [`set_color_breaks`](https://bricenocenti.github.io/tabxplor/reference/set_color_palette.md).

- color_signif:

  How significance gates the color, as a single string:

  - `"ignore"` (default): color every deviation by its observed size.

  - `"grey_non_signif"`: color by the observed size, but grey out cells
    whose deviation is not significant at `conf_level`. A coloured cell
    is then significantly different from its reference; a grey one may
    still be significant, only too small to colour.

  - `"guaranteed_effect"`: color by the guaranteed (confidence-bound)
    effect – only cells whose interval clears the threshold, with
    dimmer, conservative colors.

  With `color = "contrib"`, which has no interval to floor, the first
  two color the **relative** contribution and `"guaranteed_effect"` the
  **adjusted standardized residual**. See
  [`vignette("tabxplor")`](https://bricenocenti.github.io/tabxplor/articles/tabxplor.md).

- test:

  Set to `TRUE` to test each (sub)table for independence:
  **Chi-squared** for factor `col_vars`, **Welch's F** for numeric ones,
  with an effect size beside it. Needed by `color = "contrib"`, and
  added automatically for it. The footer names the test you actually got
  — see
  [`vignette("tabxplor-weights")`](https://bricenocenti.github.io/tabxplor/articles/tabxplor-weights.md).

- na:

  The policy to adopt for missing values, as a single string :

  - `"keep"` (default): every `NA` becomes an explicit `"NA"` level.

  - `"drop"`: each column is computed on its own non-missing
    observations, so bases can differ between `col_vars`.

  - `"drop_all"`: drop every observation missing on any variable, so all
    columns share one base.

  - `"common_base"`: one population — non-missing on the `row_vars` and
    the **first** `col_vars` — while the other `col_vars` keep their own
    `NA`'s as a level within it. Microdata only.

- levels:

  The levels of `col_vars` to keep, as a single string or a vector the
  same length as `col_vars`: `"all"` (default), `"first"` (only the
  first level of each — a compact summary of many items), or `"auto"`
  (the first level of a two-level variable, all of them otherwise). For
  finer selections use
  [`dplyr::select`](https://dplyr.tidyverse.org/reference/select.html)
  on the finished table.

- cleannames:

  Set to `TRUE` to clean level names, by removing prefix numbers like
  "1-" and text in parentheses. `NULL` (default) reads
  `options(tabxplor.cleannames)` — `FALSE`.

- other_if_less_than:

  When set to a positive integer, levels with less count than it will be
  merged into an "Others" level.

- ref:

  The reference cell that differences and ratios are computed against:

  - `"auto"` (default): the corresponding total for a difference, the
    first row (or column) for an odds ratio. `"tot"`: always the total.

  - `"first"` / `"last"`: the first or last **level** — useful to color
    a temporal development. A total is not a level and is never
    selected.

  - an **integer**, the nth row (or column); a **string**, a regular
    expression matched against the row (or column) names. `"no"`: no
    reference.

  One reference per `row_vars` with a named vector,
  `ref = c(race = "first")`; an unnamed one goes by position.

- ref2:

  The second reference level for odds ratios, needed only for a factor
  with **3 levels or more** (the "OR of each level versus `ref2`"); the
  first level by default. Ignored for a **binary** factor, where each
  level's OR is taken against the other. Same values as `ref`.

- comp:

  What each cell is compared with: `"tab"` (default) compares it inside
  its own `tab_vars` subtable, `"all"` against the total table's own
  reference line.

- ci:

  **What the confidence interval is anchored on**. Its *geometry* is not
  asked here: it follows the comparison the table makes, so an
  odds-ratio table gets an odds-ratio interval.

  - `"auto"` (default): on the comparison where the table makes one, on
    the cell for plain frequencies, none where nothing needs one.

  - `"ref"`: on the cell's deviation from its reference. `"cell"`: on
    the cell's own percentage or mean. `"no"`: none.

  `"cell"` and `"no"` anchor nothing to compare, so `stars` and
  `color_signif` are disabled (with a message). The method is chosen
  with `ci_method` and named in the table's legend.

- conf_level:

  The confidence level, as a single numeric between 0 and 1. `NULL`
  (default) reads `options(tabxplor.conf_level)` — 0.95.

- stars:

  Logical. With `ci = "ref"`, print significance stars for each cell's
  difference from its reference, read from the displayed interval
  itself. `NULL` (default) reads `options(tabxplor.stars)` — `FALSE`.

- ci_method:

  The interval method, one kind at a time, as ONE named vector –
  partial, like `ref` or `pct`, so an unnamed kind keeps its default.
  Example: `ci_method = c(cell = "beta", diff = "ac")`.

  - `cell`, a proportion's own interval: `"wilson"` (default), `"wald"`,
    `"beta"`.

  - `diff`, a proportion minus its reference: `"newcombe"` (default),
    `"ac"`, `"wald"`.

  - `mean_diff`: `"welch"` (default), `"student"`, `"ols"`.

  - `mean_ratio`: `"robust"` (default), `"quasipoisson"`, `"poisson"`.

  A proportion *ratio* has only one method (Katz), so it is not a
  choice.

- anova:

  Which one-way ANOVA **F** the p-value line shows for *numeric*
  `col_vars`: `"welch"` (does not assume equal variances) or `"classic"`
  (the pooled F). `NULL` (default) reads `options(tabxplor.anova)` —
  `"welch"`. Both are always computed, so this only chooses which row is
  shown.

- design_effect:

  Whether the intervals, stars and colour thresholds of a **weighted**
  table account for the weighting's own design effect instead of using
  the raw sample size. `NULL` (default) reads
  `options(tabxplor.design_effect)` — `FALSE`. Ignored without `wt`. See
  [`vignette("tabxplor-weights")`](https://bricenocenti.github.io/tabxplor/articles/tabxplor-weights.md).

- totaltab:

  The total table, when `tab_vars` makes subtables: `"line"` (default, a
  general total line), `"table"` (a complete total table — `row_vars` by
  `col_vars`, without `tab_vars`) or `"no"`.

- common_totrow:

  With several `row_vars`, `FALSE` (default) shows one Total row per row
  variable; `TRUE` collapses the identical ones into a single shared
  Total. Genuinely different totals (which only `na = "drop"` can
  produce) are never merged.

- n:

  How many people this table is about. `NULL` (default) reads
  `options(tabxplor.n)` — `"range"`, which prints the unweighted base
  beside the `Total` cell, `100% (9 838)`, or the whole range where the
  columns do not rest on the same people, `100% (6 712-9 838)`, so an
  unequal base cannot pass unnoticed. `"min"` prints the smallest base
  only; `"no"` none.

- n_min:

  A single positive integer (default `0`, off). A pure display filter
  applied last: it hides cells resting on too few people, without
  recomputing anything. Totals and the p-value line are always kept.

- add_pct:

  Set to `TRUE` to add a column with the frequencies of the row variable
  (for `pct = "row"`) or a row with the frequencies of the column
  variable (for `pct = "col"`).

- subtext:

  A character vector to print rows of legend under the table.

- caption:

  A title for the table. It is **stored on the table**, so it survives a
  dplyr pipeline and travels into every export — html, Markdown, Excel,
  [`forest_plot()`](https://bricenocenti.github.io/tabxplor/reference/forest_plot.md)
  — where an exporter's own `caption` still wins.
  [`set_caption()`](https://bricenocenti.github.io/tabxplor/reference/set_caption.md)
  attaches one after the fact,
  [`get_caption()`](https://bricenocenti.github.io/tabxplor/reference/set_caption.md)
  reads it back.

- digits:

  The number of digits to print, as a single integer, or an integer
  vector the same length as `col_vars`.

- display:

  What each value cell shows (text output only – the console,
  [`tab_kable`](https://bricenocenti.github.io/tabxplor/reference/tab_html.md)
  and
  [`tab_md`](https://bricenocenti.github.io/tabxplor/reference/tab_md.md);
  Excel falls back to the primary field). `NULL` (default) keeps each
  cell's plain value. Three ways to ask: a **named layout** (`"est_ci"`,
  `"base_ratio"`), a **single field** (`"ci"`, `"diff"`), or a **{}
  template** of your own (`"\{pct\} (n=\{n\})"`). The whole vocabulary
  is in
  [tabxplor-display](https://bricenocenti.github.io/tabxplor/reference/tabxplor-display.md);
  [`set_display`](https://bricenocenti.github.io/tabxplor/reference/fmt_fields.md)
  changes it on a table already built. A layout showing an interval
  prints the one the table computed, so pair it with a `ci = ` value or
  a `color` that needs one.

- color_breaks:

  A per-table override of the colour thresholds, in the form
  [`set_color_breaks`](https://bricenocenti.github.io/tabxplor/reference/set_color_palette.md)
  accepts; unset scales keep the global ones.

- output_list:

  Logical (default `FALSE`). With several `row_var`, `FALSE` merges the
  mirror tables into a single `tabxplor_tab`; `TRUE` returns a list with
  one table per `row_var`. With `tab_vars`, tables stay a list
  regardless.

- spread_vars:

  \<[tidy-select](https://tidyr.tidyverse.org/reference/tidyr_tidy_select.html)\>
  The `tab_vars` to show ACROSS the page instead of down it: each of
  their levels becomes a block of columns, and the table becomes as
  compact as it can be. A variable named here alone is added to
  `tab_vars` for you. Pair it with `comp = "all"` to compare every block
  against the overall total, and with `levels = "first"` to keep one
  column per block. Because the columns are multiplied, a cell layout
  you did not ask for narrows to its bare estimate — a numeric column
  shows its mean alone, without the coefficient of variation; name a
  layout with `display =` to keep one.

- filter:

  **\[superseded\]** A
  [`dplyr::filter`](https://dplyr.tidyverse.org/reference/filter.html)
  to apply to the data first, as a single string. Prefer filtering
  upstream of `tab()`.

## Value

A `tibble` of class `tabxplor_tab`. Every numeric column is an
[`fmt`](https://bricenocenti.github.io/tabxplor/reference/fmt.md) vector
holding all the data behind the number it shows; the `row_vars` and
`tab_vars` columns are factors. Any factor column you add later is
treated as a `tab_vars` and used for grouping, so keep added text
columns as `character`.

## Details

**Ordered factors** stay ordered through the whole pipeline, which is
what lets `ref2 = "cumulative"` pick its column variables by class. One
consequence is worth knowing: the synthetic `"Total"` and `"NA"` levels
are appended *after* the real ones, so on an ordered column they compare
as the greatest. They are labels, not points on the scale.

**Weights and survey designs.** A weight (`wt`) weights the estimates;
the intervals still use the raw number of cases unless
`design_effect = TRUE`. Pass a
[`survey::svydesign`](https://rdrr.io/pkg/survey/man/svydesign.html) as
`data` and strata, clusters, `fpc` and calibration reach every interval,
star and colour threshold. The footer always names what you got. See
[Weighted and survey
data](https://bricenocenti.github.io/tabxplor/articles/tabxplor-weights.html)
([`vignette("tabxplor-weights")`](https://bricenocenti.github.io/tabxplor/articles/tabxplor-weights.md)).

## Significance stars

With `stars = TRUE` and an interval anchored on the comparison (see
`ci`), each cell says how sure we can be that its deviation from the
reference is real and not sampling noise: `*` at the 10% level, `**` at
5%, `***` at 1%. The exact p-value is stored per cell, readable with
`$pvalue` or
[`get_pvalue()`](https://bricenocenti.github.io/tabxplor/reference/fmt_fields.md).

No separate test runs behind the scenes: a cell is significant exactly
when the interval it prints no longer contains zero, so the stars and
the `[inf; sup]` bracket can never contradict each other. Which
classical test that amounts to follows `ci_method`, and the table's
legend names it. An absolute cell interval compares nothing, so it
carries no stars.

## See also

[`tab_reg()`](https://bricenocenti.github.io/tabxplor/reference/tab_reg.md)
(regression tables) and
[`tab_counts()`](https://bricenocenti.github.io/tabxplor/reference/tab_counts.md)
(pre-aggregated counts); the superseded
[`tab_num()`](https://bricenocenti.github.io/tabxplor/reference/tab_num.md)
(numeric variables) and
[`tab_plain()`](https://bricenocenti.github.io/tabxplor/reference/tab_plain.md)
(one bare cross-table).
[tabxplor-display](https://bricenocenti.github.io/tabxplor/reference/tabxplor-display.md)
says what a cell can show;
[`set_color_breaks()`](https://bricenocenti.github.io/tabxplor/reference/set_color_palette.md)
/
[`set_color_palette()`](https://bricenocenti.github.io/tabxplor/reference/set_color_palette.md)
customise the colours;
[`tab_structure()`](https://bricenocenti.github.io/tabxplor/reference/tab_structure.md)
reports what a finished table is and what accepts it. Export it with
[`tab_xl()`](https://bricenocenti.github.io/tabxplor/reference/tab_xl.md)
(Excel),
[`tab_kable()`](https://bricenocenti.github.io/tabxplor/reference/tab_html.md)
(HTML) or
[`tab_md()`](https://bricenocenti.github.io/tabxplor/reference/tab_md.md)
(Markdown), and chart it with
[`forest_plot()`](https://bricenocenti.github.io/tabxplor/reference/forest_plot.md).
Package-wide defaults live in
[tabxplor-options](https://bricenocenti.github.io/tabxplor/reference/tabxplor-options.md).

`color = "contrib"` reads as a heatmap of the association pattern. For
the specialist contingency-table models built on top of it —
quasi-independence, Goodman's RC association models, UNIDIFF — see the
logmult package (<https://cran.r-project.org/package=logmult>).

## Examples

``` r
# A simple cross-table of counts:
tab(car_arrests, colour, released)
#> # A tabxplor tab: 3 × 4
#>   colour   Yes  No Total
#>            <n> <n>   <n>
#> 1 White  3 379 559 3 938
#> 2 Black    955 333 1 288
#> 3 Total  4 334 892 5 226

# \donttest{
# Row percentages, with the difference to the total coloured:
tab(questionr_hdv, qualif, cinema, pct = "row", na = "drop", color = "difference")
#> # A tabxplor tab: 8 × 4
#>   qualif                      Oui    Non        Total
#>                            <row%> <row%>   <row% (n)>
#> 1 Cadre                       65%    35% 100% (  260)
#> 2 Ouvrier specialise          22%    78% 100% (  203)
#> 3 Ouvrier qualifie            25%    75% 100% (  292)
#> 4 Technicien                  43%    57% 100% (   86)
#> 5 Profession intermediaire    46%    54% 100% (  160)
#> 6 Employe                     44%    56% 100% (  594)
#> 7 Autre                       47%    53% 100% (   58)
#> 8 Total                       42%    58% 100% (1 653)
#> # difference (Total): -30 -15 -5 +5 +15 +30

# One subtable per level of a third variable, and colour only what is significant:
tab(questionr_hdv, qualif, cinema, sexe, pct = "row", na = "drop",
    color = "difference", color_signif = "grey_non_signif")
#> # A tabxplor tab: 17 × 5
#> # Groups:         sexe [3]
#>    sexe     qualif                      Oui    Non        Total
#>                                      <row%> <row%>   <row% (n)>
#>  1 Homme    Cadre                       61%    39% 100% (  145)
#>  2 Homme    Ouvrier specialise          26%    74% 100% (   96)
#>  3 Homme    Ouvrier qualifie            24%    76% 100% (  229)
#>  4 Homme    Technicien                  42%    58% 100% (   66)
#>  5 Homme    Profession intermediaire    41%    59% 100% (   88)
#>  6 Homme    Employe                     50%    50% 100% (   96)
#>  7 Homme    Autre                       48%    52% 100% (   21)
#>  8 Homme    Total Homme                 39%    61% 100% (  741)
#> 
#>  9 Femme    Cadre                       71%    29% 100% (  115)
#> 10 Femme    Ouvrier specialise          19%    81% 100% (  107)
#> 11 Femme    Ouvrier qualifie            27%    73% 100% (   63)
#> 12 Femme    Technicien                  45%    55% 100% (   20)
#> 13 Femme    Profession intermediaire    51%    49% 100% (   72)
#> 14 Femme    Employe                     43%    57% 100% (  498)
#> 15 Femme    Autre                       46%    54% 100% (   37)
#> 16 Femme    Total Femme                 43%    57% 100% (  912)
#> 
#> 17 Ensemble Total Ensemble              42%    58% 100% (1 653)
#> # difference (Total): -30 -15 -5 +5 +15 +30 [grey: non-significant or under ±5 points]

# Several col_vars at once, mixing factors and numeric variables (means):
tab(car_salaries, rank, c(discipline, salary, yrs.service), pct = "row")
#> # A tabxplor tab: 4 × 6
#>   rank           A      B      Total           salary yrs.service
#>             <row%> <row%> <row% (n)>      <mean (cv)> <mean (cv)>
#> 1 AsstProf     36%    64% 100% ( 67)  80 776 (cv 10%)  2 (cv 63%)
#> 2 AssocProf    41%    59% 100% ( 64)  93 876 (cv 15%) 12 (cv 84%)
#> 3 Prof         49%    51% 100% (266) 126 772 (cv 22%) 23 (cv 51%)
#> 4 Total        46%    54% 100% (397) 113 706 (cv 27%) 18 (cv 74%)

# `levels = "first"` keeps one column per variable: a compact summary of many items.
tab(facto_tea, SPC, c(breakfast, evening, home), pct = "row", levels = "first")
#> # A tabxplor tab: 8 × 5
#>   SPC            n breakfast_lv evening_lv home_lv
#>                <n>       <row%>     <row%>  <row%>
#> 1 employee      59          49%        44%     97%
#> 2 middle        40          60%        30%    100%
#> 3 non-worker    64          44%        20%     97%
#> 4 other worker  20          40%        40%    100%
#> 5 senior        35          63%        31%     91%
#> 6 student       70          43%        44%     97%
#> 7 workman       12          25%        17%    100%
#> 8 Total        300          48%        34%     97%

# Each cell's contribution to the table's variance, as in a correspondence analysis:
tab(questionr_hdv, qualif, cinema, na = "drop", color = "contrib")
#> |        | Tests         |   cinema |
#> |:-------|:--------------|---------:|
#> | qualif | N             |    1 653 |
#> |        | pvalue (Chi2) |   <0.01% |
#> |        | Cramér's V    | V = 0.28 |
#> 
#> # A tabxplor tab: 8 × 4
#>   qualif                   Oui Non Total
#>                            <n> <n>   <n>
#> 1 Cadre                    170  90   260
#> 2 Ouvrier specialise        45 158   203
#> 3 Ouvrier qualifie          73 219   292
#> 4 Technicien                37  49    86
#> 5 Profession intermediaire  73  87   160
#> 6 Employe                  262 332   594
#> 7 Autre                     27  31    58
#> 8 Total                    687 966 1 653
#> # contribution to Chi2 (vs the mean): ×5 ×2 ×1 ×1 ×2 ×5

# The result is a tibble, so every dplyr verb works on it. Keep the total rows last:
tab(questionr_hdv, qualif, cinema, pct = "row", na = "drop") |>
  dplyr::arrange(is_totrow(dplyr::pick(dplyr::everything())), dplyr::desc(Oui))
#> # A tabxplor tab: 8 × 4
#>   qualif                      Oui    Non        Total
#>                            <row%> <row%>   <row% (n)>
#> 1 Cadre                       65%    35% 100% (  260)
#> 2 Autre                       47%    53% 100% (   58)
#> 3 Profession intermediaire    46%    54% 100% (  160)
#> 4 Employe                     44%    56% 100% (  594)
#> 5 Technicien                  43%    57% 100% (   86)
#> 6 Ouvrier qualifie            25%    75% 100% (  292)
#> 7 Ouvrier specialise          22%    78% 100% (  203)
#> 8 Total                       42%    58% 100% (1 653)
# }
```
