# Add confidence intervals to a [`tab`](https://bricenocenti.github.io/tabxplor/reference/tab.md)

**\[deprecated\]**

Deprecated in 2.0.0, defunct in 2.1.0 – confidence intervals are
computed directly by
[`tab()`](https://bricenocenti.github.io/tabxplor/reference/tab.md),
through its `ci` / `ci_method` / `conf_level` / `stars` arguments.
`tab_ci()` still works on an existing tab, reconstructing that plan from
the table's own markers.

## Usage

``` r
tab_ci(
  tabs,
  ci = "auto",
  comp = NULL,
  conf_level = conf_level_default(),
  color = "no",
  visible = FALSE,
  stars = NULL,
  ci_method = NULL,
  method_cell = NULL,
  method_diff = NULL,
  ci_scale = "diff",
  degf = NULL
)
```

## Arguments

- tabs:

  A `tibble` of class `tab` made with
  [`tab_plain`](https://bricenocenti.github.io/tabxplor/reference/tab_plain.md)
  or [`tab`](https://bricenocenti.github.io/tabxplor/reference/tab.md).

- ci:

  What the interval is anchored on : `"ref"` (the comparison with the
  reference cell), `"cell"` (the cell's own value), `"no"`, or `"auto"`
  — a comparison interval for means and row/column percentages, a cell
  interval for plain frequencies. `"diff"` and `"ratio"` are the older
  spellings of `"ref"`. With `ci = "cell"` the result prints as
  `[inf;sup]`; `display = "base_moe"` writes it as
  `pct +- margin of error` instead. See
  [`tab`](https://bricenocenti.github.io/tabxplor/reference/tab.md),
  which is where this is normally set.

- comp:

  Comparison level, when `tab_vars` are present : the interval compares
  within each subtable/group (by default, `comp = "tab"`) or over the
  whole set of tables (`comp = "all"`). It must be set once and for all
  the first time you use
  [`tab_pct`](https://bricenocenti.github.io/tabxplor/reference/tab_pct.md)
  with rows, `tab_ci` or
  [`tab_chi2`](https://bricenocenti.github.io/tabxplor/reference/tab_chi2.md).

- conf_level:

  The confidence level, as a single numeric between 0 and 1. Default to
  0.95 (95%).

- color:

  The type of colors to print, as a single string: `"no"` (the default),
  `"diff_ci"` (colour percentages and means by their difference from the
  total or first cell, dropping the colour when the interval of that
  difference is wider than the difference itself) or `"after_ci"` (idem,
  but cutting the interval off the difference first) — the 1.x spelling
  of [`tab`](https://bricenocenti.github.io/tabxplor/reference/tab.md)'s
  `color = "difference"` plus `color_signif` set to `"grey_non_signif"`
  / `"guaranteed_effect"`.

- visible:

  By default confidence intervals are calculated and used to set colors,
  but not printed. Set to `TRUE` to print them in the result.

- stars:

  Logical (opt-in; default `FALSE`, or `options("tabxplor.stars")` when
  `NULL`). Print per-cell significance stars for the difference from the
  reference, read from the same interval that is displayed, so the stars
  and the bracket never disagree.

- ci_method:

  The method of each kind of interval, as ONE named vector
  (`c(cell = , diff = , mean_diff = , mean_ratio = )`, partial) – see
  [`tab`](https://bricenocenti.github.io/tabxplor/reference/tab.md). The
  `cell` slot also takes `"beta"` (Korn-Graubard), the textbook
  design-based cell interval, conservative near 0 and 1.

- method_cell, method_diff:

  **\[deprecated\]** Use `ci_method = c(cell = , diff = )` instead.

- ci_scale:

  The scale a comparison interval is expressed on: `"diff"` (default, a
  difference interval, neutral 0) or `"ratio"` (a ratio interval,
  neutral 1 — Katz's log-risk-ratio for proportions, a ratio of means
  for numeric variables).
  [`tab()`](https://bricenocenti.github.io/tabxplor/reference/tab.md)
  sets it from the colour: the measure the reader sees owns the
  interval.

- degf:

  The design's degrees of freedom, the reference distribution of every
  interval (`#PSU - #strata`). `NULL` (default) takes the value the
  table itself carries when it was built from a
  [`survey::svydesign`](https://rdrr.io/pkg/survey/man/svydesign.html);
  `Inf` is the large-sample normal pivot.

## Value

A `tibble` of class `tab`, colored based on differences (from
totals/first cells) and confidence intervals.

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

## Examples

``` r
# A typical workflow with tabxplor step-by-step functions :
# \donttest{
data <- dplyr::starwars |> dplyr::filter(!is.na(sex))

data |>
  tab_plain(sex, hair_color, gender, tot = c("row", "col"),
    pct = "row", comp = "all") |>
    tab_ci("diff", color = "after_ci")
#> Warning: `tab_ci()` was deprecated in tabxplor 2.0.0.
#> ℹ Please use the `ci` argument of `tab()` instead.
#> The step-by-step chain is superseded: tab() / tab_num() compute this in one
#> pass.
#> ℹ The arithmetic is shared, so the numbers are identical -- only the chaining
#>   API goes.
#> # A tabxplor tab: 8 × 15
#>   gender    sex             auburn `auburn, grey` `auburn, white`  black  blond
#>                             <row%>         <row%>          <row%> <row%> <row%>
#> 1 feminine  female              6%             0%              0%    19%     0%
#> 2 feminine  none                0%             0%              0%     0%     0%
#> 3 feminine  Total feminine      6%             0%              0%    18%     0%
#> 4 masculine none                0%             0%              0%     0%     0%
#> 5 masculine hermaphroditic      0%             0%              0%     0%     0%
#> 6 masculine male                0%             2%              2%    15%     5%
#> 7 masculine Total masculine     0%             2%              2%    14%     5%
#> 8 Ensemble  Total Ensemble      1%             1%              1%    14%     4%
#> # ℹ 8 more variables: blonde <row%>, brown <row%>, `brown, grey` <row%>,
#> #   grey <row%>, none <row%>, white <row%>, `NA` <row%>, Total <row% (n)>
#> # difference (Total Ensemble): -15 -5 -0 +0 +5 +15 [all that is significant is colored, error-adjusted]
  # }
```
