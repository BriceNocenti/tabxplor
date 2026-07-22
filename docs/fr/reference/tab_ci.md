# Add confidence intervals to a [`tab`](https://bricenocenti.github.io/tabxplor/fr/reference/tab.md)

Add confidence intervals to a
[`tab`](https://bricenocenti.github.io/tabxplor/fr/reference/tab.md)

## Usage

``` r
tab_ci(
  tabs,
  ci = "auto",
  comp = NULL,
  conf_level = getOption("tabxplor.conf_level", 0.95),
  color = "no",
  visible = FALSE,
  stars = NULL,
  method_cell = "wilson",
  method_diff = "newcombe",
  method_ratio = "katz",
  method_mean_diff = "welch",
  method_mean_ratio = "robust",
  ci_scale = "diff"
)
```

## Arguments

- tabs:

  A `tibble` of class `tab` made with
  [`tab_plain`](https://bricenocenti.github.io/tabxplor/fr/reference/tab_plain.md)
  or
  [`tab_many`](https://bricenocenti.github.io/tabxplor/fr/reference/tab_many.md).

- ci:

  The type of ci to calculate. Set to "cell" to calculate absolute
  confidence intervals. Set to "diff" to calculate the confidence
  intervals of the difference between a cell and the relative total cell
  (or the reference cell, when `ref` is not `"tot"` in
  [`tab_plain`](https://bricenocenti.github.io/tabxplor/fr/reference/tab_plain.md)
  or
  [`tab_num`](https://bricenocenti.github.io/tabxplor/fr/reference/tab_num.md)).
  Set to "ratio" for the same interval on the *ratio* (relative risk /
  mean ratio) scale (the Katz interval) rather than the difference
  scale. By default, "diff" ci are calculated for means and row and col
  percentages, "cell" ci for frequencies ("all", "all_tabs"). By
  default, with `ci = "cell"`, the result is printed in the `[inf;sup]`
  form. Set `options("tabxplor.ci_print" = "moe")` to print `pct +- moe`
  instead.

- comp:

  Comparison level. When `tab_vars` are present, should the
  contributions to variance be calculated for each subtable/group (by
  default, `comp = "tab"`) ? Should they be calculated for the whole
  table (`comp = "all"`) ? `comp` must be set once and for all the first
  time you use
  [`tab_plain`](https://bricenocenti.github.io/tabxplor/fr/reference/tab_plain.md),
  [`tab_num`](https://bricenocenti.github.io/tabxplor/fr/reference/tab_num.md)
  or
  [`tab_chi2`](https://bricenocenti.github.io/tabxplor/fr/reference/tab_chi2.md)
  with rows, or `tab_ci`.

- conf_level:

  The confidence level, as a single numeric between 0 and 1. Default to
  0.95 (95%).

- color:

  The type of colors to print, as a single string.

  - `"no"`: by default, no colors are printed

  - `"diff_ci"`: color pct and means based on cells differences from
    totals or first cells, removing coloring when the confidence
    interval of this difference is higher than the difference itself

  - `"after_ci"`: idem, but cut off the confidence interval from the
    difference

- visible:

  By default confidence intervals are calculated and used to set colors,
  but not printed. Set to `TRUE` to print them in the result.

- stars:

  Logical (opt-in; default `FALSE`, or `options("tabxplor.stars")` when
  `NULL`). With `ci = "diff"`, store and print per-cell significance
  stars for the difference from the reference, read from the same
  interval that is displayed (universal CI-inclusion), so the stars and
  the bracket never disagree. `FALSE` skips the significance
  computation.

- method_cell:

  Character string, the proportion CI method for `ci = "cell"`: either
  `"wilson"` (the score interval, default) or `"wald"` (the normal
  approximation).

- method_diff:

  Character string, the proportion CI method for `ci = "diff"`: one of
  `"newcombe"` (default, hybrid-score, dual of the two-proportion score
  test), `"ac"` (Agresti-Caffo) or `"wald"`. Whatever the method, the
  stars come from that interval. It selects among the *difference*
  methods only – see `ci_scale`.

- method_ratio, method_mean_diff, method_mean_ratio:

  Character strings, the ratio / numeric-mean CI methods – see
  [`tab`](https://bricenocenti.github.io/tabxplor/fr/reference/tab.md).
  `method_ratio`: proportion ratio (`"katz"`). `method_mean_diff`: mean
  difference (`"welch"` / `"student"`). `method_mean_ratio`: mean ratio
  (`"robust"` / `"quasipoisson"` / `"poisson"`).

- ci_scale:

  Character string, the scale the `ci = "diff"` interval is expressed
  on: `"diff"` (default) for a difference interval (neutral 0, one of
  the `method_diff` methods), or `"ratio"` for a ratio interval (neutral
  1), stored as `ci_type = "ratio"` and centred on the cell/reference
  ratio – Katz's log-risk-ratio for proportions (`method_ratio`), or a
  ratio-of-means interval for numeric means (`method_mean_ratio`).
  [`tab()`](https://bricenocenti.github.io/tabxplor/fr/reference/tab.md)
  sets it from the colour: the measure the reader sees owns the
  interval, so `color = "ratio"` (or `c("ratio", "diff")`) asks for the
  ratio one.

## Value

A `tibble` of class `tab`, colored based on differences (from
totals/first cells) and confidence intervals.

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
# A typical workflow with tabxplor step-by-step functions :
# \donttest{
data <- dplyr::starwars |>
  tab_prepare(sex, hair_color, gender, other_if_less_than = 5,
              na_drop_all = sex)

data |>
  tab_plain(sex, hair_color, gender, tot = c("row", "col"),
    pct = "row", comp = "all") |>
    tab_ci("diff", color = "after_ci")
#> # A tabxplor tab: 8 × 8
#>   gender    sex              black  brown   none Others   `NA`  Total
#>   <fct>     <fct>           <row%> <row%> <row%> <row%> <row%> <row%>
#> 1 feminine  female             19%    31%    31%    19%     0%   100%
#> 2 feminine  none                0%     0%   100%     0%     0%   100%
#> 3 feminine  Total feminine     18%    29%    35%    18%     0%   100%
#> 4 masculine male               15%    18%    48%    17%     2%   100%
#> 5 masculine none                0%     0%    40%     0%    60%   100%
#> 6 masculine Others              0%     0%     0%     0%   100%   100%
#> 7 masculine Total masculine    14%    17%    47%    15%     8%   100%
#> 8 Ensemble  Total Ensemble     14%    19%    45%    16%     6%   100%
#> # difference (Total): -25 -15 -5 -0 +0 +5 +15 +25 [all that is significant is colored, error-adjusted]
  # }
```
