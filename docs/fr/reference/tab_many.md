# Many cross-tables as one, with color helpers

**\[superseded\]**

Superseded (2.0.0) by
[`tab()`](https://bricenocenti.github.io/tabxplor/fr/reference/tab.md),
the unified entry point (it accepts several row_vars / col_vars).
`tab_many()` keeps working and keeps its historical list return for \>=2
row_vars (tab() merges them by default; pass `output_list = TRUE` for a
list).

A full-featured function to create, manipulate and format many
cross-tables as one, using colors to make the printed tab more easily
readable (in R terminal or exported to Excel with
[`tab_xl`](https://bricenocenti.github.io/tabxplor/fr/reference/tab_xl.md)).
Since objects of class `tabxplor_tab` are also of class `tibble`, you
can then use all dplyr verbs to modify the result, like
[`select`](https://dplyr.tidyverse.org/reference/select.html),
[`arrange`](https://dplyr.tidyverse.org/reference/arrange.html),
[`filter`](https://dplyr.tidyverse.org/reference/filter.html) or
[`mutate`](https://dplyr.tidyverse.org/reference/mutate.html).

Color breaks are a named list of the six measure scales `pct_diff`,
`pct_ratio`, `odds_ratio`, `mean_diff`, `mean_ratio` and `contrib`. Each
is a vector of positive-only thresholds (the under-represented side is
mirrored automatically), 1 to 5 values, one per color step: `pct_diff`
colors percentage-point differences, `pct_ratio` the relative risk (the
"x2 rule"), `odds_ratio` the odds ratio (`color = "OR"`; symmetric by
default), `mean_diff` the standardized mean difference (Glass's delta)
by default (supply data-unit values for absolute coloring), `mean_ratio`
the mean ratio, `contrib` the chi2 contribution. An empty/`NULL` scale
drops that measure for its column type.

## Usage

``` r
tab_many(
  data,
  row_vars,
  col_vars,
  tab_vars,
  wt,
  pct = "no",
  color = "no",
  OR = "no",
  chi2 = FALSE,
  na = "keep",
  levels = "all",
  na_drop_all,
  cleannames = NULL,
  compact = NULL,
  other_if_less_than = 0,
  other_level = "Others",
  ref = "auto",
  ref2 = "first",
  comp = "tab",
  ci = "no",
  conf_level = getOption("tabxplor.conf_level", 0.95),
  stars = NULL,
  method_cell = "wilson",
  method_diff = "newcombe",
  method_ratio = "katz",
  method_mean_diff = "welch",
  method_mean_ratio = "robust",
  totaltab = "line",
  totaltab_name = "Ensemble",
  totrow = TRUE,
  totcol = "last",
  total_names = "Total",
  add_n = TRUE,
  add_pct = FALSE,
  common_totrow = FALSE,
  digits = 0,
  subtext = "",
  n_min = 0,
  color_signif = "ignore",
  color_breaks = NULL,
  parallel = NULL,
  filter
)

tab_get_vars(tabs, vars = c("row_var", "col_vars", "tab_vars"))

is_tab(x)

set_color_palette(
  text_colors = NULL,
  text_colors_neg = NULL,
  background_colors = NULL,
  background_colors_neg = NULL,
  dark_text_colors = NULL,
  dark_text_colors_neg = NULL,
  dark_background_colors = NULL,
  dark_background_colors_neg = NULL,
  bg_legend_colors = NULL,
  bg_legend_colors_neg = NULL,
  theme = NULL
)

set_color_style(
  type = c("text", "bg"),
  theme = NULL,
  html_24_bit = NULL,
  custom_palette = NULL
)

get_color_style(
  mode = c("crayon", "color_code"),
  type = NULL,
  theme = NULL,
  ...
)

set_color_breaks(breaks = NULL, ...)

get_color_breaks(brk, type = c("positive", "all"))
```

## Arguments

- data:

  A data frame.

- row_vars:

  The row variable, which will be printed with one level per line. If
  numeric, it will be converted to factor. If more than one row_var if
  provided, a different table is made for each of them.

- col_vars:

  \<[tidy-select](https://tidyr.tidyverse.org/reference/tidyr_tidy_select.html)\>
  One column is printed for each level of each column variable. For
  numeric variables means are calculated, in a single column. To pass
  many variables you may use syntax
  `col_vars = c(col_var1, col_var2, ...)`.

- tab_vars:

  \<[tidy-select](https://tidyr.tidyverse.org/reference/tidyr_tidy_select.html)\>
  One subtable is made for each combination of levels of the tab
  variables. To pass many variables you may use syntax
  `tab_vars = c(tab_var1, tab_var2, ...)`. All tab variables are
  converted to factor. Leave empty to make a simple table.

- wt:

  A weight variable, of class numeric. Leave empty for unweighted
  results.

- pct:

  The type of percentages to calculate :

  - `"row"`: row percentages.

  - `"col"`: column percentages.

  - `"all"`: frequencies for each subtable/group, if there is
    `tab_vars`.

  - `"all_tabs"`: frequencies for the whole (set of) table(s).

  The argument is vectorised over both `row_vars` and `col_vars`. You
  can then write as the following :
  `pct = list(row_var1 = list("row", "col", "col"), row_var2 = list("col", "row", "row"))`

- color:

  Which measure(s) to color, on which visual channel – see
  [`tab`](https://bricenocenti.github.io/tabxplor/fr/reference/tab.md)
  for the full grammar (`FALSE`/`TRUE`, a measure such as `"diff"`, a
  positional two-channel `c("diff", "ratio")`, or a per-type
  `c(pct = , mean = )` / `list(pct = , mean = )`). The old combined
  strings `"diff_ci"`/`"after_ci"`/`"ci"` still work (superseded by
  `color` + `color_signif`). Applies to all `row_vars`.

- OR:

  With `pct = "row"` or `pct = "col"`, calculate and print odds ratios:
  for a binary variable the usual odds ratio; for a variable with 3
  levels or more, the odds ratio of each level versus the reference
  level (the empirical analogue of the "OR (j vs reference)" from a
  multinomial
  [`tab_reg()`](https://bricenocenti.github.io/tabxplor/fr/reference/tab_reg.md)
  model).

  - `"no"`: by default, no OR are calculated.

  - `"OR"`: print OR (instead of percentages).

  - `"OR_pct"`: print OR, with percentages in bracket.

  Odds ratios don't add up to 100\\ `n` (console), exports the base-`n`
  column only, or nothing when `add_n = FALSE`.

- chi2:

  Set to `TRUE` to calculate Chi2 summaries with
  [`tab_chi2`](https://bricenocenti.github.io/tabxplor/fr/reference/tab_chi2.md).
  Useful to print metadata, and to color cells based on their
  contribution to variance (`color = "contrib"`). Vectorised over
  `row_vars`.

- na:

  The policy to adopt with missing values. It must be a single string.

  - `na = "keep"`: by default, prints `NA`'s as explicit `"NA"` level.

  - `na = "drop"`: removes `NA` levels before making each table (tabs
    made with different column variables may have a different number of
    observations, and won't exactly have the same total columns).

  - `"drop_all"`: remove `NA`'s for all variables before making the
    tables.

- levels:

  The levels of `col_vars` to keep (for more complex selections use
  [`dplyr::select`](https://dplyr.tidyverse.org/reference/select.html)).
  The argument is vectorised over `col_vars`.

  - `"all"`: by default, all levels are kept.

  - `"first"`: only keep the first level of each `col_vars`

  - `"auto"`: keep the first level when `col_var` is only two levels,
    keep all levels otherwise

- na_drop_all:

  \<[tidy-select](https://tidyr.tidyverse.org/reference/tidyr_tidy_select.html)\>
  Removes all observations with a `NA` in any of the chosen variables,
  for all tables (tabs for each column variable will have the same
  number of observations).

- cleannames:

  Set to `TRUE` to clean levels names, by removing prefix numbers like
  "1-", and text in parenthesis. All data formatting arguments are
  passed to
  [`tab_prepare`](https://bricenocenti.github.io/tabxplor/fr/reference/tab_prepare.md).

- compact:

  With several `row_vars`, set to `TRUE` to bind all tables in a single
  `tabxplor_tab` (`FALSE` by default). The `tabxplor.compact` option has
  been removed; use the `output_list` argument of
  [`tab()`](https://bricenocenti.github.io/tabxplor/fr/reference/tab.md)
  instead (the unified entry point, which merges by default).

- other_if_less_than:

  When set to a positive integer, levels with less count than it will be
  merged into an "Others" level.

- other_level:

  The name of the "Other" level, as a single string.

- ref:

  The reference cell to calculate differences and ratios (used to print
  `colors`) :

  - `"auto"`: by default, cell difference from the corresponding total
    (rows or cols depending on `pct = "row"` or `pct = "col"`) is used
    for `diff` ; cell ratio from the first line (or col) is use for `OR`
    (odds ratio/relative risks ratio).

  - `"tot"`: totals are always used.

  - `"first"`: calculate cell difference or ratio from the first cell of
    the row or column (useful to color temporal developments).

  - `n`: when `ref` is an integer, the nth row (or column) is used for
    comparison.

  - `"regex"`: when `ref` is a string, it it used as a regular
    expression, to match with the names of the rows (or columns). Be
    precise enough to match only one column or row, otherwise you get a
    warning message.

  - `"no"`: not use ref and not calculate diffs to gain calculation
    time.

- ref2:

  The second reference level for odds ratios (or relative risk ratios),
  needed only for a factor with **3 levels or more** (the "OR of each
  level versus `ref2`"). The first level is used by default. For a
  **binary** factor `ref2` is ignored: each level's OR is computed
  against the *other* level, so both levels show a value (reciprocals of
  one another) instead of one being forced to `1`. See `ref` above for
  the list of possible values.

- comp:

  The comparison level : by subtables/groups, or for the whole table.
  Vectorised over `row_vars`.

  - `"tab"`: by default, contributions to variance, row differences from
    totals/first cells, and row confidence intervals for these
    differences, are calculated for each `tab_vars` group.

  - `"all"`: compare cells to the general total line (provided there is
    a total table with a total row), or with the reference line of the
    total table when `ref = "first"`, an integer or a regular
    expression.

- ci:

  The type of confidence intervals to calculate, passed to
  [`tab_ci`](https://bricenocenti.github.io/tabxplor/fr/reference/tab_ci.md).
  Vectorised over `row_vars`.

  - `"cell"`: absolute confidence intervals of cells percentages.

  - `"diff"`: confidence intervals of the difference between a cell and
    the relative total cell (or relative first cell when
    `ref = "first"`).

  - `"ratio"`: like `"diff"`, but the interval is on the *ratio*
    (relative risk / mean ratio) scale between a cell and its reference
    (the Katz interval).

  - `"auto"`: `ci = "diff"` for means and row/col percentages,
    `ci = "cell"` for frequencies ("all", "all_tabs").

  Confidence intervals use fast closed-form methods. For percentages,
  `ci = "cell"` uses the Wilson score interval and `ci = "diff"` the
  Newcombe method-10 hybrid-score interval (its dual, so the bracket and
  the significance stars always agree); means use the Welch t interval.
  These can be changed with `method_cell` / `method_diff`. By default
  the interval is printed in the `[inf;sup]` form; set
  `options("tabxplor.ci_print" = "moe")` to print `pct +- moe` instead.

- conf_level:

  The confidence level, as a single numeric between 0 and 1. Default to
  0.95 (95%).

- stars:

  Logical. When `TRUE` (opt-in; default `FALSE`) and `ci = "diff"`, each
  cell shows significance stars for the difference from its reference
  (`*` p\<0.10, `**` p\<0.05, `***` p\<0.01, customisable via
  `options("tabxplor.signif_levels")` / `"tabxplor.signif_labels"`).
  Significance is read from the same interval that is displayed
  (universal CI-inclusion), so stars and bracket never disagree. `FALSE`
  skips the significance computation entirely. `NULL` uses
  `options("tabxplor.stars")`.

- method_cell:

  Character string, the proportion confidence-interval method for
  `ci = "cell"`. Either `"wilson"` (the score interval, default) or
  `"wald"` (the normal approximation, commonly taught – degenerate at
  cell proportions of 0 or 1).

- method_diff:

  Character string, the proportion confidence-interval method for
  `ci = "diff"`. One of `"newcombe"` (default, the hybrid-score
  interval, dual of the two-proportion score test), `"ac"`
  (Agresti-Caffo) or `"wald"`. Whatever method is chosen, the stars come
  from that same interval, so they always agree with the bracket.

- method_ratio:

  Character string, the proportion *ratio* interval (`color = "ratio"`):
  `"katz"` (the log risk-ratio interval, the only value for now).

- method_mean_diff:

  Character string, the numeric mean-*difference* interval: `"welch"`
  (default, each group's own variance, Welch–Satterthwaite df) or
  `"student"` (pooled variance, df \\= n_1 + n_2 - 2\\, reproducing a
  linear-regression coefficient interval).

- method_mean_ratio:

  Character string, the numeric mean-*ratio* interval (`color = "ratio"`
  on a mean): `"robust"` (default, each group's own variance =
  modified/robust Poisson), `"quasipoisson"` (Poisson SE scaled by the
  dispersion, reproducing a quasi-Poisson regression) or `"poisson"`
  (naive Var = mean). All are the log-scale Wald/t interval, exp-back.
  As for the other methods, the significance stars come from the same
  interval so bracket and stars always agree.

- totaltab:

  The total table, if there are subtables/groups (i.e. when `tab_vars`
  is provided). Vectorised over `row_vars`.

  - `"line"`: by default, add a general total line (necessary for
    calculations with `comp = "all"`)

  - `"table"`: add a complete total table (i.e. `row_var` by `col_vars`
    without `tab_vars`).

  - `"no"`: not to draw any total table.

- totaltab_name:

  The name of the total table, as a single string.

- totrow:

  By default, total rows are printed. Set to `FALSE` to remove them
  (after calculations if needed). Vectorised over `row_vars`.

- totcol:

  The policy with total columns. Vectorised over `col_vars`.

  - `"last"`: by default, only prints a total column for the last column
    variable (of class factor, not numeric).

  - `"each"`: print a total column for each column variable.

  - `"no"`: remove all total columns (after calculations if needed).

- total_names:

  The names of the totals, as a character vector of length one or two.
  Use syntax of type `c("Total row", "Total column")` to set different
  names for rows and cols.

- add_n:

  For `pct = "row"` or `pct = "col"`, set to `FALSE` not to add another
  column or row with unweighted counts (`n`).

- add_pct:

  Set to `TRUE` to add a column with the frequencies of the row variable
  (for `pct = "row"`) or a row with the frequencies of the column
  variable (for `pct = "col"`).

- common_totrow:

  With several `row_vars`, `FALSE` (the default) shows one Total row per
  row variable; `TRUE` collapses the identical Total rows into a single
  shared Total in its own group.

- digits:

  The number of digits to print, as a single integer, or an integer
  vector the same length as `col_vars`. The argument is vectorisez over
  `col_vars`.

- subtext:

  A character vector to print rows of legend under the table.

- n_min:

  A single positive integer (default `0`, off). A pure display filter –
  see
  [`tab`](https://bricenocenti.github.io/tabxplor/fr/reference/tab.md) –
  that hides small-base rows/cells (largest base below `n_min` drops the
  row; own base below `n_min` blanks the cell) without recomputing
  anything.

- color_signif:

  How significance gates the color – see
  [`tab`](https://bricenocenti.github.io/tabxplor/fr/reference/tab.md).

- color_breaks:

  A per-table colour-threshold override – see
  [`tab`](https://bricenocenti.github.io/tabxplor/fr/reference/tab.md).

- parallel:

  Opt-in parallel build of the per-`row_var` tables (Suggests-only
  mirai); see
  [`tab`](https://bricenocenti.github.io/tabxplor/fr/reference/tab.md).
  `NULL` (default) reads the `tabxplor.parallel` option.

- filter:

  **\[superseded\]** A
  [`dplyr::filter`](https://dplyr.tidyverse.org/reference/filter.html)
  to apply to the data frame first, as a single string (which will be
  converted to code, i.e. to a call). Prefer filtering the data with
  [`dplyr::filter`](https://dplyr.tidyverse.org/reference/filter.html)
  upstream of
  [`tab()`](https://bricenocenti.github.io/tabxplor/fr/reference/tab.md);
  this argument is kept for back-compatibility (e.g. printing multiple
  tabs from a
  [`tibble::tribble`](https://tibble.tidyverse.org/reference/tribble.html)).

- tabs:

  A `tibble` of class `tab`, made with
  [`tab`](https://bricenocenti.github.io/tabxplor/fr/reference/tab.md),
  `tab_many` or
  [`tab_plain`](https://bricenocenti.github.io/tabxplor/fr/reference/tab_plain.md).

- vars:

  In `tab_get_vars`, a character vector containing the wanted vars
  names: `"row_var"`, `"col_vars"` or `"tab_vars"`.

- x:

  A object to test with `is_tab`.

- text_colors, text_colors_neg, background_colors,
  background_colors_neg:

  Light-theme palettes (4 hex each): the text (font) and background
  (fill) colours for the over- (`*_colors`) and under-represented
  (`*_colors_neg`) sides.

- dark_text_colors, dark_text_colors_neg, dark_background_colors,
  dark_background_colors_neg:

  The dark-theme counterparts (4 hex each).

- bg_legend_colors, bg_legend_colors_neg:

  (4 hex each) The FONT stand-in for `background_colors` in the colour
  legend of media that cannot fill a run (Excel,
  [`tab_plot`](https://bricenocenti.github.io/tabxplor/fr/reference/tab_plot.md));
  the defaults are the background colours at -0.2 OKLCH lightness.
  Setting `background_colors` without these makes them follow it
  unchanged (readable only if your fills already are). There is no dark
  counterpart: an Excel legend cell is on a white page whatever the
  theme, and the dark fills read there as-is.

- theme:

  `"light"` or `"dark"`; defaults to the current setting. (A palette is
  always one or the other: the export theme `"auto"` resolves to
  `"light"` here.)

- type:

  Default `"positive"` returns a readable form: a plain vector of
  magnitudes when the scale is symmetric, or a `list(over =, under =)`
  of magnitudes otherwise. Set to `"all"` to get the signed / reciprocal
  thresholds the engine compares against (`c(-x, x)` for additive
  scales, `c(1/x, x)` for multiplicative ones).

- html_24_bit:

  **\[deprecated\]** Inert since 2.0.0 (exports are always 24-bit).

- custom_palette:

  **\[deprecated\]** A former 10/11-slot palette; its 4 over- and 4
  under-represented colours are mapped onto `set_color_palette()`.

- mode:

  By default, `get_color_style` returns a list of terminal (ANSI)
  coloring functions (the historical value `"crayon"`, now built with
  cli). Set to `"color_code"` to return html color codes.

- ...:

  Scales passed individually and named, e.g.
  `set_color_breaks(pct_diff = c(0.05, 0.1, 0.2), mean_ratio = c(1.15, 1.5, 2, 4))`.
  Each value is either a plain vector of signed / reciprocal literals
  (negatives, or ratios \< 1, are the under-represented side; a
  one-sided vector auto-mirrors; `NA` skips an intensity slot) or a
  `list(over =, under =)` of magnitudes (no mirror; omit a side to
  switch it off, e.g. `list(over = 2)` for the "only x2" rule). The old
  `pct_breaks` / `mean_breaks` / `contrib_breaks` arguments are
  soft-deprecated but still work (mapped onto the new scales).

- breaks:

  A named list of scales to set, e.g.
  `list(pct_diff = c(0.05, 0.1, 0.2, 0.3), pct_ratio = list(over = 2))`.
  Unset scales keep their current value.

- brk:

  When missing, return the full named list of break scales (`pct_diff`,
  `pct_ratio`, `odds_ratio`, `mean_diff`, `mean_ratio`, `contrib`) – the
  same shape `set_color_breaks` accepts, so it round-trips. Specify one
  scale name to return only its breaks. The old aliases `"pct"` (-\>
  `pct_diff`) and `"mean"` (-\> `mean_ratio`) are still accepted.

## Value

A `tibble` of class `tab`, possibly with colored reading helpers. When
there are two `row_vars` or more, a list of `tibble` of class `tab`. All
non-text columns are of class
[`fmt`](https://bricenocenti.github.io/tabxplor/fr/reference/fmt.md),
storing all the data necessary to print formats and colors. Columns with
`row_var` and `tab_vars` are of class `factor` : every added `factor`
will be considered as a `tab_vars` and used for grouping. To add text
columns without using them in calculations, be sure they are of class
`character`.

A list with the variables names.

A single logical.

Sets the internal color palettes (invisibly) and the option
`"tabxplor.color_style_theme"`.

A list of 8 terminal (ANSI) color-style functions, or a vector of 8
color html codes.

Sets the global option "tabxplor.color_breaks" (a named list of scales)
and returns it invisibly.

The color breaks as a double vector or a `list(over =, under =)`, or a
named list of these.

## Functions

- `tab_get_vars()`: Get the variables names of a tabxplor `tab`

- `is_tab()`: a test function for class tabxplor_tab

- `set_color_palette()`: customise the color palette used to print
  [`tab`](https://bricenocenti.github.io/tabxplor/fr/reference/tab.md).
  Each palette is 4 hex codes ordered faint -\> strong. Provide only the
  ones you want to change; the OKLCH defaults are used otherwise. The
  ANSI styles are (re)built once, not per cell.

- `set_color_style()`: **\[deprecated\]** Superseded by
  `set_color_palette()`. Kept as a back-compat shim: `type`/`theme`
  still take effect (as options); `custom_palette` maps its over/under
  colours onto the new 4+4 palette; `html_24_bit` is inert (exports are
  always 24-bit).

- `get_color_style()`: get the color palette as terminal (ANSI) style
  functions or html codes: an 8-element vector (4 over-represented
  intensities then 4 under-represented), indexed by the engine slot.

- `set_color_breaks()`: set the breaks used to print colors.

- `get_color_breaks()`: get the color breaks currently in use, in the
  canonical Phase-5 shape.

## Significance stars

With `ci = "diff"` and `stars = TRUE`, each cell shows how sure we can
be that its difference from the reference is real and not just sampling
noise: `*` means significant at the 10\\ p-value is stored per cell in
the `pvalue` field of the `fmt` vectors, readable with `$pvalue` or
`get_pvalue()`.

There is no separate statistical test run behind the scenes: the
significance is read straight from the confidence interval that is
displayed. A cell is significant at a given level exactly when its
interval at that confidence level no longer contains zero, so the stars
and the printed `[inf; sup]` bracket can never contradict each other.
Which test this amounts to depends on the interval:

- **percentage difference** (default, `method_diff = "newcombe"`):
  inverting the Newcombe hybrid-score interval. This is, to a very close
  approximation, the classical two-sample test of proportions (the score
  / "N-1" chi-squared test).

- **percentage difference** with `method_diff = "ac"` or `"wald"`:
  inverting the Agresti-Caffo (adjusted Wald) or the Wald interval – an
  (adjusted) two-proportion z-test.

- **mean difference**: the **Welch two-sample t-test** (for groups with
  unequal variances); inverting the Welch t interval is exactly this
  well-known test.

- `ci = "cell"` (an absolute cell interval, not a difference) is purely
  descriptive, so it carries no stars and its `pvalue` is `NA`.

On weighted data the estimate is weighted but the sample size used is
the real (unweighted) number of cases, unless you opt in to Kish's
effective sample size with `options("tabxplor.kish_neff" = TRUE)`.

## Examples

``` r
# Make a summary table with many col_vars, showing only one specific level :
# \donttest{
library(dplyr)
first_lvs <- c("Married", "$25000 or more", "Strong republican", "Protestant")
data <- forcats::gss_cat |> mutate(across(
  where(is.factor),
  ~ forcats::fct_relevel(., first_lvs[first_lvs %in% levels(.)])
))
tab_many(data, race, c(marital, rincome, partyid, relig, age, tvhours),
         levels = "first", pct = "row", chi2 = TRUE, color = "auto")
#> |      | Tests                  |  marital |   |  rincome |   |  partyid |   |    relig |   |         age |   |     tvhours |
#> |:-----|:-----------------------|---------:|:-:|---------:|:-:|---------:|:-:|---------:|:-:|------------:|:-:|------------:|
#> | race | N                      |   21 483 |   |   21 483 |   |   21 483 |   |   21 483 |   |      21 407 |   |      11 337 |
#> |      | pvalue (Chi2, Welch F) |   <0.01% |   |   <0.01% |   |   <0.01% |   |   <0.01% |   |      <0.01% |   |      <0.01% |
#> |      | Cramér's V, eta2       | V = 0.16 |   | V = 0.06 |   | V = 0.15 |   | V = 0.25 |   | eta2 = 0.03 |   | eta2 = 0.04 |
#> 
#> # A tabxplor tab: 4 × 7
#>   race   Married `$25000 or more` `Strong republican` Protestant      age
#>   <fct>   <row%>           <row%>              <row%>     <row%>   <mean>
#> 1 Other      48%              32%                  4%        20% 39 (σ14)
#> 2 Black      28%              28%                  2%        73% 44 (σ16)
#> 3 White      51%              36%                 13%        50% 49 (σ17)
#> 4 Total      47%              34%                 11%        50% 47 (σ17)
#> # ℹ 1 more variable: tvhours <mean>
#> # marital, rincome, partyid, relig: difference (Total): -30 -20 -10 -5 +5 +10 +20 +30
#> # age, tvhours: standardized difference (Total): -0.8 -0.5 -0.2 +0.2 +0.5 +0.8
# }

# Can be used with map and tribble to program several tables with different parameters
#  all at once, in a readable way:
# \donttest{
library(purrr)
library(tibble)
pmap(
  tribble(
    ~row_var, ~col_vars       , ~pct , ~filter              , ~subtext               ,
    "race"  , "marital"       , "row", NULL                 , "Source: GSS 2000-2014",
    "relig" , c("race", "age"), "row", "year %in% 2000:2010", "Source: GSS 2000-2010",
    NA_character_, "race"     , "no" , NULL                 , "Source: GSS 2000-2014",
  ),
  .f = tab_many,
  data = forcats::gss_cat, color = "auto", chi2 = TRUE)
#> [[1]]
#> |      | Tests           |  marital |
#> |:-----|:----------------|---------:|
#> | race | N               |   21 483 |
#> |      | pvalue (Chi2 !) |   <0.01% |
#> |      | Cramér's V      | V = 0.15 |
#> 
#> # A tabxplor tab: 4 × 8
#>   race   `No answer` `Never married` Separated Divorced Widowed Married
#>   <fct>       <row%>          <row%>    <row%>   <row%>  <row%>  <row%>
#> 1 Other           0%             32%        6%      11%      4%     48%
#> 2 Black           0%             42%        6%      16%      8%     28%
#> 3 White           0%             21%        3%      16%      9%     51%
#> 4 Total           0%             25%        3%      16%      8%     47%
#> # ℹ 1 more variable: Total <row%>
#> # difference (Total): -30 -20 -10 -5 +5 +10 +20 +30
#> # Source: GSS 2000-2014
#> 
#> [[2]]
#> |       | Tests                    |     race |   |         age |
#> |:------|:-------------------------|---------:|:-:|------------:|
#> | relig | N                        |   16 971 |   |      16 909 |
#> |       | pvalue (Chi2 !, Welch F) |   <0.01% |   |      <0.01% |
#> |       | Cramér's V, eta2         | V = 0.26 |   | eta2 = 0.04 |
#> 
#> # A tabxplor tab: 16 × 6
#>    relig                    Other  Black  White           Total      age
#>    <fct>                   <row%> <row%> <row%>          <row%>   <mean>
#>  1 No answer                  12%    17%    71% 100% (n=    76) 47 (σ15)
#>  2 Don't know                 29%    29%    43% 100% (n=     7) 35 (σ9 )
#>  3 Inter-nondenominational     2%    26%    72% 100% (n=   103) 40 (σ14)
#>  4 Native american            67%     0%    33% 100% (n=    15) 40 (σ14)
#>  5 Christian                  12%    18%    70% 100% (n=   435) 39 (σ13)
#>  6 Orthodox-christian          1%     1%    98% 100% (n=    80) 49 (σ15)
#>  7 Moslem/islam               41%    34%    24% 100% (n=    82) 36 (σ11)
#>  8 Other eastern              21%     8%    71% 100% (n=    24) 47 (σ15)
#>  9 Hinduism                   84%     2%    14% 100% (n=    49) 38 (σ13)
#> 10 Buddhism                   54%     7%    39% 100% (n=   115) 44 (σ17)
#> 11 Other                      11%    10%    79% 100% (n=   172) 41 (σ14)
#> 12 None                        9%    11%    81% 100% (n= 2 614) 41 (σ16)
#> 13 Jewish                      2%     2%    96% 100% (n=   320) 52 (σ18)
#> 14 Catholic                   18%     4%    78% 100% (n= 4 074) 46 (σ17)
#> 15 Protestant                  3%    21%    76% 100% (n= 8 805) 49 (σ17)
#> 16 Total                       9%    14%    77% 100% (n=16 971) 47 (σ17)
#> # race: difference (Total): -30 -20 -10 -5 +5 +10 +20 +30
#> # age: standardized difference (Total): -0.8 -0.5 -0.2 +0.2 +0.5 +0.8
#> # Source: GSS 2000-2010
#> 
#> [[3]]
#> # A tabxplor tab: 2 × 5
#>   no_row_var Other Black  White  Total
#>   <fct>        <n>   <n>    <n>    <n>
#> 1 no_row_var 1 959 3 129 16 395 21 483
#> 2 Total      1 959 3 129 16 395 21 483
#> # Source: GSS 2000-2014
#> 
# }
set_color_palette(text_colors = c("#02a5b3", "#0891c9", "#0267c7", "#300dfd"))
set_color_breaks(
  pct_diff   = c(0.05, 0.15, 0.3),
  pct_ratio  = list(over = 2),
  mean_ratio = c(1.15, 2, 4),
  contrib    = c(1, 2, 5)
)
```
