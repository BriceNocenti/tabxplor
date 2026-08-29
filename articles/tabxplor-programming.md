# Programming with tabxplor

``` r

library(tabxplor)

# Pin the legend language: it defaults to "auto" = the ambient locale, so building this English
# vignette on a French machine silently renders French legends and captions (the -fr articles pin
# "fr" for the same reason). Output must not depend on where it is built.
options(tabxplor.lang = "en")
# The shape table a continuous predictor draws under the footer is not this vignette's subject.
options(tabxplor.shape_table = "no")
Sys.setenv(LANGUAGE = "en")   # the test-summary / model-fit row labels go through gettext, not this option
library(dplyr)

# Tables render as tabxplor's real html tables (the recommended everyday setting); the shared
# stylesheet is emitted once by tab_css() below, and the hover tooltips are kept off here.
options(tabxplor.print = "html")
options(tabxplor.tab_kable_css = FALSE)
options(tabxplor.tab_kable_tooltips = FALSE)

# Console outputs (vectors, fields...) keep their terminal colors, turned to html by fansi.
options(cli.num_colors = 256)
set_color_palette(theme = "light")
```

*Une version française de ce document est disponible : [Programmer avec
tabxplor](https://bricenocenti.github.io/tabxplor/articles/tabxplor-programming-fr.html).*

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
tabs <- tab(gss_simple, race, marital, pct = "row", color = "difference")
```

## Getting the plain numbers out

The quickest way to recover the underlying numbers as ordinary numeric
vectors is
[`get_num()`](https://bricenocenti.github.io/tabxplor/reference/fmt_fields.md),
which extracts whatever field is currently displayed:

``` r

tabs |> mutate(across(where(is_fmt), get_num))
```

| race | Married | Separated | Divorced | Widowed | Never married | NA | Total |
|----|----|----|----|----|----|----|----|
| White | 0.507227813357731 | 0.0266544678255566 | 0.163220494053065 | 0.0899664531869472 | 0.212137846904544 | 0.000792924672156145 | 1 |
| Black | 0.277724512623841 | 0.0626398210290828 | 0.158197507190796 | 0.0837328219878555 | 0.417066155321189 | 0.000639181847235539 | 1 |
| Other | 0.475752935171006 | 0.0561510974987238 | 0.108218478815722 | 0.035732516590097 | 0.32312404287902 | 0.00102092904543134 | 1 |
| Total | 0.470930503188568 | 0.0345854861983894 | 0.157473351021738 | 0.0841130195968906 | 0.252106316622446 | 0.000791323371968533 | 1 |

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

To hand the whole table to base R — a correspondence analysis,
[`chisq.test()`](https://rdrr.io/r/stats/chisq.test.html),
[`mosaicplot()`](https://rdrr.io/r/graphics/mosaicplot.html) —
[`as.matrix()`](https://rdrr.io/r/base/matrix.html) and
[`as.table()`](https://rdrr.io/r/base/table.html) do it in one call.
They keep only the data cells: the total row, the total columns and the
display-time rows (the base count, `add_pct`, the p-values) go, because
an analysis run on a table’s own margins is wrong. Pass `totals = TRUE`
to keep them.

``` r

as.matrix(tab(gss_simple, race, marital))
```

``` r-output
#>       Married Separated Divorced Widowed Never married NA
#> White    8316       437     2676    1475          3478 13
#> Black     869       196      495     262          1305  2
#> Other     932       110      212      70           633  2
```

## The fields of a cell

A `tabxplor_fmt` cell carries **21 fields** (many are `NA` when the
relevant quantity was not requested). The user-facing ones are:

| Field | Meaning |
|:---|:---|
| `n` | unweighted count (integer) |
| `wn` | weighted count |
| `pct` | percentage |
| `mean` | mean (numeric column variables) |
| `diff` | difference from the total / reference cell |
| `ratio` | ratio to the reference (relative risk, or a ratio of means) |
| `ci_inf`, `ci_sup` | confidence-interval bounds |
| `pvalue` | per-cell significance p-value (feeds the stars) |
| `or` | odds ratio / relative-risk ratio |
| `ctr` | contribution to the chi-squared (`color = "contrib"`) |
| `var` | variance (numeric columns; chi-squared variance with `pct`) |
| `tot_n` | the cell’s own base — the count its percentage is computed on |
| `n_eff` | effective sample size used for the CI, with `options(tabxplor.design_effect = TRUE)` or a `survey` design |
| `obs` | [`tab_reg()`](https://bricenocenti.github.io/tabxplor/reference/tab_reg.md) only: the observed (crude) effect the modelled one is compared to |
| `gap_se` | [`tab_reg()`](https://bricenocenti.github.io/tabxplor/reference/tab_reg.md) only: the standard error of the gap between the estimate and `obs` |
| `digits` | number of decimals to display (per cell) |
| `display` | which field is shown (per cell) |
| `row_kind` | what kind of row the cell sits in: `"data"`, `"total"`, or one of the synthetic display rows `"n"` / `"pct"` / `"pvalue"` / `"gof"` / `"blank"` |
| `in_tottab`, `in_refrow` | is the cell in a total table / a reference row (logical) |

(`row_kind` replaced a logical `in_totrow` field in 2.0.0. `x$in_totrow`
still returns that logical, and
[`is_totrow()`](https://bricenocenti.github.io/tabxplor/reference/fmt_fields.md)
/
[`as_totrow()`](https://bricenocenti.github.io/tabxplor/reference/fmt_fields.md)
are unchanged.)

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
#>   ci_sup pvalue or tot_n n_eff obs gap_se row_kind in_tottab in_refrow
#> 1     NA     NA  1 16395    NA  NA     NA     data     FALSE     FALSE
#> 2     NA     NA  1  3129    NA  NA     NA     data     FALSE     FALSE
#> 3     NA     NA  1  1959    NA  NA     NA     data     FALSE     FALSE
#> 4     NA     NA  1 21483    NA  NA     NA    total     FALSE     FALSE
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
#> [1] 0.4995743 0.2623114 0.4537069 0.4642615
```

``` r

ci_tab$Married$ci_sup
```

``` r-output
#> [1] 0.5148780 0.2936827 0.4978939 0.4776099
```

Switch the displayed field with
[`set_display()`](https://bricenocenti.github.io/tabxplor/reference/fmt_fields.md):

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

tab(gss_simple, race, c(age, tvhours), digits = 1L) |>
  mutate(across(
    c(age, tvhours),
    ~ mutate(., var = sqrt(var), display = "var", digits = 1L) |> set_color("no"),
    .names = "{.col}_sd"
  ))
```

## Composite display: combining fields

[`set_display()`](https://bricenocenti.github.io/tabxplor/reference/fmt_fields.md)
chooses *one* field to show. To show **several fields in one cell**,
give it a [`{}`](https://rdrr.io/r/base/Paren.html) **template** instead
of a bare field name — the same template you can pass to
`tab(display =)` when you build the table. The template is plain text
with `{field}` tokens; each token is replaced by that field, formatted
as usual:

``` r

tabs |> set_display("{pct} ({diff})")
```

[TABLE]

The rules:

- **Valid tokens** are `pct`, `n`, `wn`, `mean`, `est`, `base`, `diff`,
  `ratio`, `ci`, `moe`, `or`, `ctr`, `var`, `sd`, `cv`, `coef`, `resid`,
  `obs`, `gap` — the same set as the single-field displays. Most name a
  stored field; a few are **derived** and so read-only — `resid` (from
  the p-value and the sign of `ctr`), `sd` and `cv` from `var`, `gap`
  from `est` and `obs`.
- Two of them are **scale-relative**, and are what makes one template
  work on every table: `{est}` is whatever the column estimates (a
  percentage, a difference, an odds ratio) and `{base}` the level it
  sits on (a percentage, a mean, a count). `"{est} ({base})"` reads the
  same on a crosstab and on a regression.
- Common layouts have **names**: `"est"`, `"est_ci"`, `"est_base"`,
  `"base_est"`, `"base"`, `"base_ci"`, `"base_moe"`, `"mean_sd"`,
  `"mean_cv"`. They are the same names in
  [`tab()`](https://bricenocenti.github.io/tabxplor/reference/tab.md),
  in
  [`tab_reg()`](https://bricenocenti.github.io/tabxplor/reference/tab_reg.md)
  and in
  [`set_display()`](https://bricenocenti.github.io/tabxplor/reference/fmt_fields.md).
- The first token **outside brackets** is the *primary* one: it is what
  [`get_num()`](https://bricenocenti.github.io/tabxplor/reference/fmt_fields.md)
  returns, the value Excel keeps, and the only part the colours paint. A
  template with no token outside brackets has no primary, and renders
  whole as an aside.
- A token may carry **its own precision** — `"{est} ({base:1})"` — which
  beats the column’s `digits`.
- A **bare field name** is shorthand for its own template, so
  `set_display("ci")` is exactly `set_display("{ci}")`.
- It is a **display overlay** for text output (the console,
  [`tab_html()`](https://bricenocenti.github.io/tabxplor/reference/tab_html.md),
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

[TABLE]

`.names = "{.col}_diff"` keeps the original percentage columns and adds
a `<name>_diff` twin next to each. (If a `diff` column comes out empty,
the source table had no reference cell — build it with `ref =` /
`comp =`, or with `color = "difference"`.)

The same recipe puts an **adjusted prediction** beside a regression
effect. Two extra calls make the copy purely *descriptive*:
`set_color("")` drops its colour (the ladder belongs to the effect, and
the same ladder twice is noise), and `set_pvalue(NA_real_)` drops its
stars — the stored p-value is their only source, so erasing it is the
direct way to say “this copy tests nothing”:

``` r

tab_reg(gss_simple, "married", c("race", "rincome"), family = "binomial",
        display = "est_base") |>
  mutate(Model_pct = Model_OR |> set_display("{base}") |> set_color("") |>
                       set_pvalue(NA_real_),
         .after = Model_OR)
```

## Total rows, reference rows and columns

Helper predicates let you act on structural parts of a table:
[`is_totrow()`](https://bricenocenti.github.io/tabxplor/reference/fmt_fields.md)
/
[`is_tottab()`](https://bricenocenti.github.io/tabxplor/reference/fmt_fields.md)
/
[`is_refrow()`](https://bricenocenti.github.io/tabxplor/reference/fmt_fields.md)
(cell-level, logical vectors), and
[`is_totcol()`](https://bricenocenti.github.io/tabxplor/reference/fmt_attributes.md)
/
[`is_refcol()`](https://bricenocenti.github.io/tabxplor/reference/fmt_attributes.md)
(column-level). They are what keeps totals in place when you re-order or
when you write conditional formatting:

``` r

# fewer decimals on the total row than on the body:
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

- `scale` —
  [`get_scale()`](https://bricenocenti.github.io/tabxplor/reference/fmt_attributes.md)
  /
  [`set_scale()`](https://bricenocenti.github.io/tabxplor/reference/fmt_attributes.md):
  **what the column estimates**. A level (`"level_n"`, `"level_pct"`,
  `"level_mean"`), a difference (`"points"` for percentage points,
  `"mean_diff"`, `"raw_diff"` for a regression coefficient in the
  outcome’s own units), a ratio (`"pct_ratio"`, `"mean_ratio"`,
  `"odds_ratio"`) or a link-scale coefficient (`"log_coef"`). It says
  which field holds the estimate, what its null value is, and which
  colour ladder it is read on.
- `pct_type` —
  [`get_pct_type()`](https://bricenocenti.github.io/tabxplor/reference/fmt_attributes.md)
  /
  [`set_pct_type()`](https://bricenocenti.github.io/tabxplor/reference/fmt_attributes.md):
  for a percentage, what it is a percentage *of* — `"row"`, `"col"`,
  `"all"`, `"all_tabs"`, or `"none"` for counts, means and coefficients.
- `ci_method` —
  [`get_ci_method()`](https://bricenocenti.github.io/tabxplor/reference/fmt_attributes.md):
  which interval engine built this column’s bounds (`"wilson"`,
  `"newcombe"`, `"welch"`, `"katz"`, …; `""` when the column carries no
  interval).
- `color` —
  [`get_color()`](https://bricenocenti.github.io/tabxplor/reference/fmt_attributes.md)
  /
  [`set_color()`](https://bricenocenti.github.io/tabxplor/reference/fmt_attributes.md):
  the colour measure of the column — `""`, `"no"`, or one of the measure
  names `"difference"`, `"ratio"`, `"odds_ratio"`, `"contrib"` (plus
  `"adjustment"` / `"between_groups"` on a
  [`tab_reg()`](https://bricenocenti.github.io/tabxplor/reference/tab_reg.md)
  table). The discipline’s acronyms (`"diff"`, `"OR"`, `"or"`, `"RR"`,
  `"RD"`) are permanent shorthands you may *type*, but a built table
  always stores — and its legend always names — the full word.
- `col_var` —
  [`get_col_var()`](https://bricenocenti.github.io/tabxplor/reference/fmt_attributes.md):
  the name of the column variable (a table can hold several).
- `col_group` —
  [`get_col_group()`](https://bricenocenti.github.io/tabxplor/reference/fmt_attributes.md):
  which sub-population the column’s block belongs to, after a
  `spread_vars` /
  [`tab_spread()`](https://bricenocenti.github.io/tabxplor/reference/tab_spread.md)
  pivot or a `tab_reg(tab_vars =)` split (`""` otherwise). Together with
  `col_var` it identifies a column *block*: two blocks can show the same
  variable for two sub-populations, and exports head them on two lines.
- `comp_all` —
  [`get_comp_all()`](https://bricenocenti.github.io/tabxplor/reference/fmt_attributes.md):
  with `tab_vars`, is the comparison reference the sub-table (`FALSE`)
  or the whole table (`TRUE`)?
- `totcol` / `refcol` —
  [`is_totcol()`](https://bricenocenti.github.io/tabxplor/reference/fmt_attributes.md)
  /
  [`is_refcol()`](https://bricenocenti.github.io/tabxplor/reference/fmt_attributes.md):
  is this a total column / a reference column?

## Knowing what you have before you touch it

Generic code cannot assume the shape of the table it is handed. Three
accessors answer that, so a function can branch on facts rather than
guess from column names.

[`tab_structure()`](https://bricenocenti.github.io/tabxplor/reference/tab_structure.md)
says **what the object is** — a crosstab or a regression table, merged
or not, grouped or not, and which variables sit on each axis:

``` r

t <- tab(gss_simple, race, marital, pct = "row", ci = "ref", color = "difference")
tab_structure(t)
```

``` r-output
#> $container
#> [1] "table"
#> 
#> $kind
#> [1] "crosstab"
#> 
#> $merged
#> [1] FALSE
#> 
#> $grouped
#> [1] FALSE
#> 
#> $row_vars
#> [1] "race"
#> 
#> $tab_vars
#> character(0)
#> 
#> $col_vars
#> [1] "marital"
#> 
#> $same_col_vars
#> [1] TRUE
#> 
#> $same_tab_vars
#> [1] TRUE
```

[`tab_columns()`](https://bricenocenti.github.io/tabxplor/reference/tab_columns.md)
is the column-level view: one row per column, with everything the
exporters and the colour engine read — what it estimates, its reference,
its confidence-interval method, its role:

``` r

tab_columns(t) |> dplyr::select(column, scale, pct_type, ref, ci_method, totcol)
```

``` r-output
#> # A tibble: 7 × 6
#>   column        scale  pct_type ref   ci_method totcol
#>   <chr>         <chr>  <chr>    <chr> <chr>     <lgl> 
#> 1 Married       points row      tot   newcombe  FALSE 
#> 2 Separated     points row      tot   newcombe  FALSE 
#> 3 Divorced      points row      tot   newcombe  FALSE 
#> 4 Widowed       points row      tot   newcombe  FALSE 
#> 5 Never married points row      tot   newcombe  FALSE 
#> 6 NA            points row      tot   newcombe  FALSE 
#> 7 Total         points row      tot   newcombe  TRUE
```

And
[`fmt_attr()`](https://bricenocenti.github.io/tabxplor/reference/fmt_attr.md)
reads or writes **any one of those facts by name**, which is the
programmatic twin of the `get_*` / `set_*` family above — useful when
the attribute is itself a variable:

``` r

fmt_attr(t$Married, "scale")
```

``` r-output
#> [1] "points"
```

``` r

fmt_attr(t$Married, "ci_method")
```

``` r-output
#> [1] "newcombe"
```

On a regression table, `reg_measures(data, outcome)` plays the same role
for the model: it lists the `effect` × `measure` combinations that
outcome can be asked for, and why the others are refused.

## Building cells from scratch

[`fmt()`](https://bricenocenti.github.io/tabxplor/reference/fmt.md)
constructs an `fmt` vector directly from numbers — handy for tests, or
to add a hand-computed column. Everything the display and colour engine
needs can be supplied:

``` r

fmt(n = c(10L, 20L, 30L), pct = c(0.1, 0.2, 0.7), display = "pct", digits = 0L)
```

``` r-output
#> <fmt-%[3]>
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

[TABLE]

``` r

# identical to tab(gss_simple, marital, race, pct = "row", color = "difference")
```

It also melts a [`table()`](https://rdrr.io/r/base/table.html) /
[`xtabs()`](https://rdrr.io/r/stats/xtabs.html) / matrix automatically,
and reads a **wide** frame (one column per column-variable level) via
`cols =` / `col_name =`:

``` r

tab_counts(table(gss_simple$marital, gss_simple$race), pct = "row", color = "difference")

wide <- tidyr::pivot_wider(counts, names_from = race, values_from = n)
tab_counts(wide, row_var = marital, cols = c(White, Black, Other),
           col_name = "race", pct = "row", color = "difference")
```

Weighting works as in
[`tab()`](https://bricenocenti.github.io/tabxplor/reference/tab.md):
pass the unweighted count in `counts` and the weighted count in
`wt_counts` — estimates use the weighted count, confidence intervals and
Chi-2 use the unweighted N. (`options(tabxplor.design_effect = TRUE)`
needs the individual weights, which pre-aggregated counts no longer
carry, so it does not apply here — the CIs use the unweighted N, and the
table’s footer says so rather than claiming a correction it does not
have.) When the only figures available are non-whole numbers
(percentages × a base, or weighted-only counts), CIs and the Chi-2 test
are disabled with a message.

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

|  | marital |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |
|----|----|----|----|----|----|----|----|----|----|----|----|----|----|----|----|----|
| relig | 2000 | 2002 | 2004 | 2006 | 2008 | 2010 | 2012 | 2014 | 2000 | 2002 | 2004 | 2006 | 2008 | 2010 | 2012 | 2014 |
|  | \<row%\> | \<row%\> | \<row%\> | \<row%\> | \<row%\> | \<row%\> | \<row%\> | \<row%\> | \<n\> | \<n\> | \<n\> | \<n\> | \<n\> | \<n\> | \<n\> | \<n\> |
| 1-Protestant | 47% | 48% | 54% | 50% | 51% | 47% | 50% | 49% | 1 521 | 1 460 | 1 483 | 2 328 | 1 040 | 973 | 916 | 1 125 |
| 2-Catholic | 47% | 49% | 57% | 53% | 49% | 44% | 47% | 47% | 679 | 673 | 656 | 1 114 | 470 | 482 | 444 | 606 |
| 3-Other christian | 47% | 41% | 51% | 38% | 59% | 39% | 42% | 43% | 51 | 88 | 84 | 116 | 78 | 98 | 126 | 143 |
| 4-Jewish | 49% | 44% | 53% | 51% | 44% | 54% | 64% | 50% | 63 | 48 | 55 | 78 | 39 | 37 | 28 | 40 |
| 5-Buddhist/Hinduist | 40% | 41% | 65% | 50% | 59% | 50% | 60% | 51% | 25 | 27 | 26 | 42 | 22 | 22 | 15 | 39 |
| 6-Muslim | 42% | 38% | 69% | 71% | 31% | 36% | 62% | 67% | 12 | 13 | 16 | 17 | 13 | 11 | 13 | 9 |
| 7-Other | 30% | 48% | 43% | 30% | 25% | 33% | 42% | 39% | 64 | 58 | 77 | 50 | 20 | 45 | 38 | 36 |
| 8-None | 38% | 33% | 40% | 37% | 38% | 36% | 32% | 37% | 398 | 379 | 403 | 739 | 332 | 363 | 387 | 522 |
| NA | 75% | 37% | 67% | 38% | 67% | 31% | 29% | 50% | 4 | 19 | 12 | 26 | 9 | 13 | 7 | 18 |
| Total | 45% | 46% | 53% | 48% | 48% | 44% | 46% | 46% | 2 817 | 2 765 | 2 812 | 4 510 | 2 023 | 2 044 | 1 974 | 2 538 |

`tab(..., spread_vars = year)` does the same in one call.
`tab_reg(..., tab_vars =)` produces a grouped table you can spread the
same way — see
[`vignette("tabxplor-reg")`](https://bricenocenti.github.io/tabxplor/articles/tabxplor-reg.md).

## Building many tables at once

[`tab()`](https://bricenocenti.github.io/tabxplor/reference/tab.md)
accepts **vectors of arguments** to build several differently-shaped
tables in one call (`output_list = TRUE` keeps them as a list rather
than merging them). For a fully data-driven batch,
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
→ *Global R options*), these
[`options()`](https://rdrr.io/r/base/options.html) tune the exporters
and the build. HTML /
[`tab_html()`](https://bricenocenti.github.io/tabxplor/reference/tab_html.md):

- `tabxplor.tab_kable_css` — inline the stylesheet with each table
  (`TRUE`); set `FALSE` in a many-table document and call
  [`tab_css()`](https://bricenocenti.github.io/tabxplor/reference/tab_css.md)
  once instead (this vignette does).
- `tabxplor.tab_kable_tooltips` — the per-cell hover tooltips (`TRUE`);
  set `FALSE` once per document to switch them off everywhere.
- `tabxplor.tab_kable_num_font` — the number CSS font stack (everything
  else is
  [`tab_css()`](https://bricenocenti.github.io/tabxplor/reference/tab_css.md)’s
  business).
- `tabxplor.kable_popover` — click popovers instead of hover tooltips.

Excel / `tab_export("xl")`:

- `tabxplor.xl_font_text` / `tabxplor.xl_font_num` /
  `tabxplor.xl_font_num_stars` — the label, number and starred-number
  fonts.
- `tabxplor.xl_ratio_cells` — how a multiplicative cell reaches Excel:
  `"fold"` (default, the signed fold as a number), `"raw"` (the
  untransformed ratio) or `"text"` (the exact display string).

Console, stats and paths:

- `tabxplor.console_bold` — embolden reference / total / coloured cells
  (auto-detected per editor).
- `tabxplor.stars` — the significance ladder, as one named vector:
  `options(tabxplor.stars = c("*" = 0.05, "**" = 0.01))`.
- `tabxplor.export_dir` — the default export directory.

Performance and integration:

- `tabxplor.parallel` — build one worker per row variable on a
  background pool (needs `mirai`); `tabxplor.parallel_min` sets the
  smallest row-variable count worth dispatching. Release the pool with
  [`tab_parallel_stop()`](https://bricenocenti.github.io/tabxplor/reference/tab_parallel_stop.md).

See `?tabxplor-options` for the full list and every default.

## See also

- [`vignette("tabxplor")`](https://bricenocenti.github.io/tabxplor/articles/tabxplor.md)
  — cross-tables and the colour helpers.
- [`vignette("tabxplor-reg")`](https://bricenocenti.github.io/tabxplor/articles/tabxplor-reg.md)
  — regression tables.
- [`vignette("tabxplor-weights")`](https://bricenocenti.github.io/tabxplor/articles/tabxplor-weights.md)
  — weighted and survey data.
- [`?fmt`](https://bricenocenti.github.io/tabxplor/reference/fmt.md) and
  `?tabxplor-vctrs` for the cell type, `?tabxplor-options` for every
  option,
  [`?tab`](https://bricenocenti.github.io/tabxplor/reference/tab.md) for
  the arguments and the display tokens.
