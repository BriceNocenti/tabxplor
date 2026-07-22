# Programming with tabxplor

``` r
library(tabxplor)
library(dplyr)
#> 
#> Attaching package: 'dplyr'
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union

options(cli.num_colors = 256)
set_color_palette(theme = "light")
```

This vignette is for users who want to **program with** `tabxplor` —
write their own helpers, pull the underlying numbers out, or reshape a
table cell by cell. If you only want to make and read tables,
[`vignette("tabxplor")`](https://bricenocenti.github.io/tabxplor/articles/tabxplor.md)
is the place to start.

Every numeric column of a `tabxplor` table is a single vector of class
**`tabxplor_fmt`** (“formatted number”). It is a [`vctrs`
record](https://vctrs.r-lib.org/reference/new_rcrd.html): behind the one
value you see printed, each cell stores **all the data needed to compute
the displayed number, its format and its colour** — counts, percentages,
differences, relative risks, confidence-interval bounds, odds-ratios,
and so on. Because it is a proper vector, it survives every `dplyr`
verb, and you can read or rewrite any of its fields.

``` r
gss_simple <- gss_cat_data_formatting()
tabs <- tab(gss_simple, race, marital, pct = "row", color = "diff")
```

## Getting the plain numbers out

The quickest way to recover the underlying numbers as ordinary numeric
vectors is
[`get_num()`](https://bricenocenti.github.io/tabxplor/reference/fmt.md),
which extracts whatever field is currently displayed:

``` r
tabs |> mutate(across(where(is_fmt), get_num))
```

``` r-output
#> # A tabxplor tab: 4 × 8
#>   race  Married Separated Divorced Widowed `Never married`     `NA` Total
#>   <fct>   <dbl>     <dbl>    <dbl>   <dbl>           <dbl>    <dbl> <dbl>
#> 1 White   0.507    0.0267    0.163  0.0900           0.212 0.000793     1
#> 2 Black   0.278    0.0626    0.158  0.0837           0.417 0.000639     1
#> 3 Other   0.476    0.0562    0.108  0.0357           0.323 0.00102      1
#> 4 Total   0.471    0.0346    0.157  0.0841           0.252 0.000791     1
```

To get the character strings instead (formatted, but without colours),
use [`format()`](https://rdrr.io/r/base/format.html):

``` r
tabs |> mutate(across(where(is_fmt), format))
```

Individual fields are read most simply with `$` on the fmt column (see
“Reading and changing fields” below):

``` r
tabs$Married$pct
```

``` r-output
#> [1] 0.5072278 0.2777245 0.4757529 0.4709305
```

## The fields of a cell

A `tabxplor_fmt` cell carries **19 fields** (many are `NA` when the
relevant quantity was not requested). The user-facing ones are:

| Field | Meaning |
|:---|:---|
| `n` | unweighted count (integer) |
| `wn` | weighted count |
| `pct` | percentage |
| `mean` | mean (numeric column variables) |
| `diff` | difference from the total / reference cell |
| `ratio` | ratio to the reference (the “×2 rule”; relative risk) |
| `ci_inf`, `ci_sup` | confidence-interval bounds |
| `pvalue` | per-cell significance p-value (feeds the stars) |
| `or` | odds ratio / relative-risk ratio |
| `ctr` | contribution to the chi-squared (`color = "contrib"`) |
| `var` | variance (numeric columns; chi-squared variance with `pct`) |
| `tot_n` | the cell’s own base — the count its percentage is computed on |
| `n_eff` | effective sample size used for the CI, with `options(tabxplor.kish_neff = TRUE)` |
| `digits` | number of decimals to display (per cell) |
| `display` | which field is shown (per cell) |
| `in_totrow`, `in_tottab`, `in_refrow` | is the cell in a total row / total table / reference row (logical) |

See the whole underlying data frame of a column with
[`vctrs::vec_data()`](https://vctrs.r-lib.org/reference/vec_data.html):

``` r
vctrs::vec_data(tabs$Married)
```

``` r-output
#>       n display digits wn       pct mean         diff     ratio ctr var ci_inf
#> 1  8316     pct      0 NA 0.5072278   NA  0.036297310 1.0770757  NA  NA     NA
#> 2   869     pct      0 NA 0.2777245   NA -0.193205991 0.5897357  NA  NA     NA
#> 3   932     pct      0 NA 0.4757529   NA  0.004822432 1.0102402  NA  NA     NA
#> 4 10117     pct      0 NA 0.4709305   NA  0.000000000 1.0000000  NA  NA     NA
#>   ci_sup pvalue or tot_n n_eff in_totrow in_tottab in_refrow
#> 1     NA     NA NA 16395    NA     FALSE     FALSE     FALSE
#> 2     NA     NA NA  3129    NA     FALSE     FALSE     FALSE
#> 3     NA     NA NA  1959    NA     FALSE     FALSE     FALSE
#> 4     NA     NA NA 21483    NA      TRUE     FALSE     FALSE
```

## Reading and changing fields

Read a field with `$` (the friendliest way), or
[`vctrs::field()`](https://vctrs.r-lib.org/reference/fields.html):

``` r
tabs$Married$pct
tabs |> mutate(across(where(is_fmt), ~ .$pct))
tabs |> mutate(across(where(is_fmt), ~ vctrs::field(., "pct")))
```

Confidence intervals are stored as their two bounds, the `ci_inf` and
`ci_sup` fields:

``` r
ci_tab <- tab(gss_simple, race, marital, pct = "row", ci = "cell")
ci_tab$Married$ci_inf
```

``` r-output
#> [1] 0.4995743 0.2623114 0.4537069        NA
```

``` r
ci_tab$Married$ci_sup
```

``` r-output
#> [1] 0.5148780 0.2936827 0.4978939        NA
```

Switch the displayed field with
[`set_display()`](https://bricenocenti.github.io/tabxplor/reference/fmt.md):

``` r
tabs |> set_display("diff")
tabs |> mutate(across(where(is_fmt), ~ set_display(., "diff")))
```

To change a field, the easiest route is
[`dplyr::mutate()`](https://dplyr.tidyverse.org/reference/mutate.html)
**on the fmt vector itself** — a `tabxplor_fmt` behaves like a little
data frame of its fields. For example, round every cell to two decimals:

``` r
tabs |> mutate(across(where(is_fmt), ~ mutate(., digits = 2L)))
```

A fuller example: turn a means table’s variance into a standard
deviation and add it as a new, un-coloured column shown to one decimal:

``` r
tab_num(gss_simple, race, c(age, tvhours), digits = 1L) |>
  mutate(across(
    c(age, tvhours),
    ~ mutate(., var = sqrt(var), display = "var", digits = 1L) |> set_color("no"),
    .names = "{.col}_sd"
  ))
```

## Composite display: combining fields

[`set_display()`](https://bricenocenti.github.io/tabxplor/reference/fmt.md)
chooses *one* field to show. To show **several fields in one cell**,
give it a [`{}`](https://rdrr.io/r/base/Paren.html) **template** instead
of a bare field name — the same template you can pass to
`tab(display =)` when you build the table. The template is plain text
with `{field}` tokens; each token is replaced by that field, formatted
as usual:

``` r
tabs |> set_display("{pct} ({diff})")
```

``` r-output
#> # A tabxplor tab: 4 × 8
#>   race      Married Separated  Divorced  Widowed `Never married`     `NA`
#>   <fct>      <row%>    <row%>    <row%>   <row%>          <row%>   <row%>
#> 1 White  51% ( +4%)  3% (-1%) 16% (+1%) 9% (+1%)      21% ( -4%) 0% (+0%)
#> 2 Black  28% (-19%)  6% (+3%) 16% (+0%) 8% (-0%)      42% (+16%) 0% (-0%)
#> 3 Other  48% ( +0%)  6% (+2%) 11% (-5%) 4% (-5%)      32% ( +7%) 0% (+0%)
#> 4 Total  47% ( +0%)  3% (+0%) 16% (+0%) 8% (+0%)      25% ( +0%) 0% (+0%)
#> # ℹ 1 more variable: Total <row%>
#> # difference (Total): -30 -20 -10 -5 +5 +10 +20 +30
```

The rules:

- **Valid fields** are `pct`, `n`, `wn`, `mean`, `diff`, `ratio`, `ci`,
  `or`, `ctr`, `var` — the same set as the single-field displays.
- The **first** token is the *primary* field: it is what
  [`get_num()`](https://bricenocenti.github.io/tabxplor/reference/fmt.md)
  returns, the value Excel keeps (Excel shows the primary only), and the
  field the colours read.
- A **bare field name** is shorthand for its own template, so
  `set_display("ci")` is exactly `set_display("{ci}")`.
- It is a **display overlay** for text output (the console,
  [`tab_kable()`](https://bricenocenti.github.io/tabxplor/reference/tab_html.md),
  [`tab_md()`](https://bricenocenti.github.io/tabxplor/reference/tab_md.md)):
  the stored fields are untouched, so you can switch back at any time.
- The [ci](https://github.com/GegznaV/ci) field already prints its own
  `[…;…]` brackets, so write `"{pct} {ci}"` — **not** `"{pct} [{ci}]"`,
  which would double them.

## Creating a column from another field

Because switching the display recomputes nothing — every field is
already stored in the cell — you can spin off a **new column that shows
a different field**. A percentage table has a reference by default (the
Total row), so its `diff` field is already filled; a difference twin of
every percentage column is then one
[`mutate()`](https://dplyr.tidyverse.org/reference/mutate.html):

``` r
tab(gss_simple, race, marital, pct = "row") |>
  mutate(across(where(is_fmt), ~ set_display(., "diff"), .names = "{.col}_diff"))
```

``` r-output
#> # A tabxplor tab: 4 × 15
#>   race   Married Separated Divorced Widowed `Never married`   `NA`  Total
#>   <fct>   <row%>    <row%>   <row%>  <row%>          <row%> <row%> <row%>
#> 1 White      51%        3%      16%      9%             21%     0%   100%
#> 2 Black      28%        6%      16%      8%             42%     0%   100%
#> 3 Other      48%        6%      11%      4%             32%     0%   100%
#> 4 Total      47%        3%      16%      8%             25%     0%   100%
#> # ℹ 7 more variables: Married_diff <row%-diff>, Separated_diff <row%-diff>,
#> #   Divorced_diff <row%-diff>, Widowed_diff <row%-diff>,
#> #   `Never married_diff` <row%-diff>, NA_diff <row%-diff>, Total_diff <row%>
```

`.names = "{.col}_diff"` keeps the original percentage columns and adds
a `<name>_diff` twin next to each. (If a `diff` column comes out empty,
the source table had no reference cell — build it with `ref =` /
`comp =`, or with `color = "diff"`.)

## Total rows, reference rows and columns

Helper predicates let you act on structural parts of a table:
[`is_totrow()`](https://bricenocenti.github.io/tabxplor/reference/fmt.md)
/
[`is_tottab()`](https://bricenocenti.github.io/tabxplor/reference/fmt.md)
/
[`is_refrow()`](https://bricenocenti.github.io/tabxplor/reference/fmt.md)
(cell-level, logical vectors), and
[`is_totcol()`](https://bricenocenti.github.io/tabxplor/reference/fmt.md)
/
[`is_refcol()`](https://bricenocenti.github.io/tabxplor/reference/fmt.md)
(column-level). They are what keeps totals in place when you re-order or
when you write conditional formatting:

``` r
# more decimals on the total row than on the body:
tab(gss_simple, race, marital, race, pct = "row") |>
  mutate(across(
    where(is_fmt),
    ~ if_else(is_totrow(.), mutate(., digits = 1L), mutate(., digits = 2L))
  ))
```

## Column attributes

Besides its per-cell fields, each `fmt` column carries a few
**column-level attributes**, read and set with `get_*` / `set_*` (or
`is_*` / `as_*` for the logical ones):

- `type` —
  [`get_type()`](https://bricenocenti.github.io/tabxplor/reference/fmt.md)
  /
  [`set_type()`](https://bricenocenti.github.io/tabxplor/reference/fmt.md):
  `"n"`, `"mean"`, `"row"`, `"col"`, `"all"`, `"all_tabs"` (or `"coef"`
  for regression betas). It drives which calculations `tab_*` functions
  perform.
- `color` —
  [`get_color()`](https://bricenocenti.github.io/tabxplor/reference/fmt.md)
  /
  [`set_color()`](https://bricenocenti.github.io/tabxplor/reference/fmt.md):
  the colour measure of the column (`""`, `"no"`, `"diff"`, `"ratio"`,
  `"contrib"`, `"OR"`).
- `col_var` —
  [`get_col_var()`](https://bricenocenti.github.io/tabxplor/reference/fmt.md):
  the name of the column variable (a table can hold several).
- `comp_all` —
  [`get_comp_all()`](https://bricenocenti.github.io/tabxplor/reference/fmt.md):
  with `tab_vars`, is the comparison reference the sub-table (`FALSE`)
  or the whole table (`TRUE`)?
- `totcol` / `refcol` —
  [`is_totcol()`](https://bricenocenti.github.io/tabxplor/reference/fmt.md)
  /
  [`is_refcol()`](https://bricenocenti.github.io/tabxplor/reference/fmt.md):
  is this a total column / a reference column?

## Building cells from scratch

[`fmt()`](https://bricenocenti.github.io/tabxplor/reference/fmt.md)
constructs an `fmt` vector directly from numbers — handy for tests, or
to add a hand-computed column. Everything the display and colour engine
needs can be supplied:

``` r
fmt(n = c(10L, 20L, 30L), pct = c(0.1, 0.2, 0.7), display = "pct", digits = 0L)
```

``` r-output
#> <fmt-n-pct[3]>
#> [1] 10% 20% 70%
```

## Tables from pre-aggregated counts

Sometimes the data already arrives **cross-tabulated** — a counts table
from a report, a [`table()`](https://rdrr.io/r/base/table.html), a
matrix of frequencies.
[`tab_counts()`](https://bricenocenti.github.io/tabxplor/reference/tab_counts.md)
builds the same coloured `tabxplor` table as
[`tab()`](https://bricenocenti.github.io/tabxplor/reference/tab.md), but
from those counts instead of the raw records; every calculation runs on
the counts, so the result is identical to what
[`tab()`](https://bricenocenti.github.io/tabxplor/reference/tab.md)
would have produced from individual records.

The commonest input is a **tidy counts** data frame (one row per
combination, the count in a column):

``` r
counts <- dplyr::count(gss_simple, marital, race)
tab_counts(counts, marital, race, counts = n, pct = "row", color = TRUE)
```

``` r-output
#> # A tabxplor tab: 7 × 5
#>   marital        White  Black  Other           Total
#>   <fct>         <row%> <row%> <row%>          <row%>
#> 1 Married          82%     9%     9% 100% (n=10 117)
#> 2 Separated        59%    26%    15% 100% (n=   743)
#> 3 Divorced         79%    15%     6% 100% (n= 3 383)
#> 4 Widowed          82%    14%     4% 100% (n= 1 807)
#> 5 Never married    64%    24%    12% 100% (n= 5 416)
#> 6 NA               76%    12%    12% 100% (n=    17)
#> 7 Total            76%    15%     9% 100% (n=21 483)
#> # difference (Total): -30 -20 -10 -5 +5 +10 +20 +30; bg ratio: ÷4 ÷2 ÷1.5 ×1.5 ×2 ×4
```

``` r
# identical to tab(gss_simple, marital, race, pct = "row", color = "diff")
```

It also melts a [`table()`](https://rdrr.io/r/base/table.html) /
[`xtabs()`](https://rdrr.io/r/stats/xtabs.html) / matrix automatically,
and reads a **wide** frame (one column per column-variable level) via
`cols =` / `col_name =`:

``` r
tab_counts(table(gss_simple$marital, gss_simple$race), pct = "row", color = "diff")

wide <- tidyr::pivot_wider(counts, names_from = race, values_from = n)
tab_counts(wide, row_var = marital, cols = c(White, Black, Other),
           col_name = "race", pct = "row", color = "diff")
```

Weighting works as in
[`tab()`](https://bricenocenti.github.io/tabxplor/reference/tab.md):
pass the unweighted count in `counts` and the weighted count in
`wt_counts` — estimates use the weighted count, confidence intervals and
Chi-2 use the unweighted N. (Kish’s effective sample size,
`options(tabxplor.kish_neff = TRUE)`, needs the individual weights,
which pre-aggregated counts no longer carry, so it does not apply here —
the CIs use the unweighted N as shown.) When the only figures available
are non-whole numbers (percentages × a base, or weighted-only counts),
CIs and the Chi-2 test are disabled with a message.

## Pivoting a grouped table into columns

A **grouped** table (built with `tab_vars`) stacks one sub-table per
group.
[`tab_spread()`](https://bricenocenti.github.io/tabxplor/reference/tab_spread.md)
pivots a grouping variable’s levels into **side-by-side columns** —
ideal to compare one measure across groups. Select the measure column
you want first, then spread:

``` r
tab(gss_simple, relig, marital, year, pct = "row", totaltab = "no", tot = "row") |>
  dplyr::select(year, relig, Married) |>
  tab_spread(year)
```

``` r-output
#> # A tabxplor tab: 10 × 9
#>    relig                 `2000` `2002` `2004` `2006` `2008` `2010` `2012` `2014`
#>    <fct>                 <row%> <row%> <row%> <row%> <row%> <row%> <row%> <row%>
#>  1 "1-Protestant"           47%    48%    54%    50%    51%    47%    50%    49%
#>  2 "2-Catholic"             47%    49%    57%    53%    49%    44%    47%    47%
#>  3 "3-Other christian"      47%    41%    51%    38%    59%    39%    42%    43%
#>  4 "4-Jewish"               49%    44%    53%    51%    44%    54%    64%    50%
#>  5 "5-Buddhist/Hinduist"    40%    41%    65%    50%    59%    50%    60%    51%
#>  6 "6-Muslim"               42%    38%    69%    71%    31%    36%    62%    67%
#>  7 "7-Other"                30%    48%    43%    30%    25%    33%    42%    39%
#>  8 "8-None"                 38%    33%    40%    37%    38%    36%    32%    37%
#>  9 "NA"                     75%    37%    67%    38%    67%    31%    29%    50%
#> 10 "TOTAL "                 45%    46%    53%    48%    48%    44%    46%    46%
```

`tab(..., spread_vars = year)` does the same in one call. The regression
vignette’s `split_var` produces a grouped table you can spread the same
way — see
[`vignette("tabxplor-reg")`](https://bricenocenti.github.io/tabxplor/articles/tabxplor-reg.md).

## A score from several factors

[`score_from_lv1()`](https://bricenocenti.github.io/tabxplor/reference/score_from_lv1.md)
turns a battery of yes/no (or agree/disagree) factors into a single
**summed score**: for each row it counts how many of the listed factors
sit at their **first level**. Here we score how many of two markers —
being married, and being in the higher income bracket — each respondent
has:

``` r
gss_simple |>
  score_from_lv1("score", vars_list = c("married", "income25k")) |>
  tab(relig, score)
```

``` r-output
#> # A tabxplor tab: 10 × 2
#>    relig                      score
#>    <fct>                     <mean>
#>  1 1-Protestant        0.82 (σ0.72)
#>  2 2-Catholic          0.85 (σ0.73)
#>  3 3-Other christian   0.79 (σ0.71)
#>  4 4-Jewish            0.94 (σ0.75)
#>  5 5-Buddhist/Hinduist 0.99 (σ0.76)
#>  6 6-Muslim            0.85 (σ0.71)
#>  7 7-Other             0.74 (σ0.69)
#>  8 8-None              0.74 (σ0.73)
#>  9 NA                  0.60 (σ0.68)
#> 10 Total               0.81 (σ0.73)
```

The score ranges from 0 to the number of factors (missing values never
count). Such a summed score is exactly the input a **grouped-binomial**
regression models — see `trials` in
[`vignette("tabxplor-reg")`](https://bricenocenti.github.io/tabxplor/articles/tabxplor-reg.md).

## Building many tables at once

[`tab_many()`](https://bricenocenti.github.io/tabxplor/reference/tab_many.md)
— the engine behind
[`tab()`](https://bricenocenti.github.io/tabxplor/reference/tab.md) —
accepts **lists of arguments** to build several differently-shaped
tables in one call. For a fully data-driven batch,
[`purrr::pmap()`](https://purrr.tidyverse.org/reference/pmap.html) over
a small specification table is the idiom — one row per table, one column
per argument:

``` r
specs <- tibble::tribble(
  ~row_var, ~col_var,  ~pct,
  "race",   "marital", "row",
  "relig",  "party3",  "row",
)
purrr::pmap(specs, \(row_var, col_var, pct)
            tab(gss_simple, all_of(row_var), all_of(col_var), pct = pct))
```

## Advanced options

Beyond the everyday defaults
([`vignette("tabxplor")`](https://bricenocenti.github.io/tabxplor/articles/tabxplor.md)
→ *Session options*), these
[`options()`](https://rdrr.io/r/base/options.html) tune the exporters
and the build. HTML /
[`tab_kable()`](https://bricenocenti.github.io/tabxplor/reference/tab_html.md):

- `tabxplor.tab_kable_engine` — `"html"` (dependency-free, the default)
  or `"kableExtra"` (legacy).
- `tabxplor.tab_kable_css` — inline the stylesheet with each table
  (`TRUE`); set `FALSE` in a many-table document and call
  [`tab_css()`](https://bricenocenti.github.io/tabxplor/reference/tab_css.md)
  once instead.
- `tabxplor.tab_kable_num_font` / `tabxplor.kable_html_font` — the
  number and text CSS font stacks.
- `tabxplor.kable_popover` — click popovers instead of hover tooltips.

Excel / `tab_export("xl")`:

- `tabxplor.xl_font_text` / `tabxplor.xl_font_num` /
  `tabxplor.xl_font_num_stars` — the label, number and starred-number
  fonts.
- `tabxplor.xl_or_numeric` — keep odds ratios as numbers rather than
  `1/x` text.

Console, stats and paths:

- `tabxplor.console_bold` — embolden reference / total / coloured cells
  (auto-detected per editor).
- `tabxplor.signif_levels` / `tabxplor.signif_labels` — the p-value
  cut-offs and the star labels.
- `tabxplor.totcol_range` — how a Total column prints when the column
  variables have different bases.
- `tabxplor.plot_num_font` —
  [`tab_plot()`](https://bricenocenti.github.io/tabxplor/reference/tab_plot.md)’s
  number font; `tabxplor.export_dir` — the default export directory.

Performance and integration: - `tabxplor.parallel` — build one worker
per row variable on a background pool (needs `mirai`);
`tabxplor.parallel_min` sets the smallest row-variable count worth
dispatching. Release the pool with
[`tab_parallel_stop()`](https://bricenocenti.github.io/tabxplor/reference/tab_parallel_stop.md).

See `?tabxplor-options` for the full list and every default.

## See also

- [`vignette("tabxplor")`](https://bricenocenti.github.io/tabxplor/articles/tabxplor.md)
  — cross-tables and the colour helpers.
- [`vignette("tabxplor-reg")`](https://bricenocenti.github.io/tabxplor/articles/tabxplor-reg.md)
  — regression tables.
- [`?fmt`](https://bricenocenti.github.io/tabxplor/reference/fmt.md),
  [`?tab_num`](https://bricenocenti.github.io/tabxplor/reference/tab_num.md),
  [`?tab_ci`](https://bricenocenti.github.io/tabxplor/reference/tab_ci.md)
  for the reference documentation of the type and its building blocks.
