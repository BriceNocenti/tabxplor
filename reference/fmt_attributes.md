# Per-column attributes of a `fmt` vector

Read and write the facts that hold for a whole column: what it
estimates, which percentage base it rests on, which reference it is
compared to, how it is coloured, and how its confidence interval was
built. They are stored on the vector, not on the table, so a column
keeps them when it is extracted, renamed or piped through `dplyr`.

[`fmt_attr()`](https://bricenocenti.github.io/tabxplor/reference/fmt_attr.md)
reaches any of them by name — these are the readable way to address one
you know.
[`tab_columns()`](https://bricenocenti.github.io/tabxplor/reference/tab_columns.md)
shows them all, for every column of a table at once.

## Usage

``` r
get_scale(x, ...)

set_scale(x, scale)

get_pct_type(x, ...)

set_pct_type(x, pct_type)

is_totcol(x, ...)

as_totcol(x, totcol = TRUE)

get_comp_all(x, replace_na = TRUE)

set_comp_all(x, comp_all = FALSE)

get_ref_type(x, ...)

set_ref_type(x, ref)

set_diff_type(x, ref)

get_col_var(x, ...)

set_col_var(x, col_var)

get_col_group(x, ...)

get_model_family(x, ...)

set_model_family(x, model_family)

get_ci_method(x, ...)

is_refcol(x, ...)

as_refcol(x, refcol = TRUE)

get_color(x, ...)

get_color_bg(x, ...)

get_color_signif(x, ...)

set_color(x, color)

set_color_signif(x, color_signif)
```

## Arguments

- x:

  The object to test, to get a field in, or to modify.

- ...:

  In
  [`fmt()`](https://bricenocenti.github.io/tabxplor/reference/fmt.md),
  it exists only for the arguments retired in tabxplor 2.0.0: `type` is
  translated into `scale` + `pct_type` (see
  [`tabxplor-type`](https://bricenocenti.github.io/tabxplor/reference/tabxplor-type.md)),
  `ci_type` gets an error naming its replacement. In the accessor
  methods below, to add arguments in the future.

- scale:

  What the column estimates, as a single string (an attribute, not a
  field): one key into the declared library of estimate scales. It says
  which field holds the estimate, what its null value is, whether the
  scale is additive or multiplicative, and which colour ladder it reads.

  - `"level_n"`: counts

  - `"level_pct"`: percentages (`pct_type` says of what)

  - `"level_mean"`: means (from numeric variables)

  - `"points"`: a difference between two percentages, in percentage
    points

  - `"mean_diff"`: a difference between two means, in the outcome's own
    units

  - `"raw_diff"`: a regression coefficient / marginal effect in the
    outcome's units

  - `"pct_ratio"`, `"mean_ratio"`: the ratio of two percentages / two
    means

  - `"odds_ratio"`: a multiplicative effect (odds ratio, risk ratio,
    rate ratio)

  - `"log_coef"`: a link-scale coefficient (a log-odds, a log-rate)

  - `"mixed"`: what binding columns of unlike scales collapses to

- pct_type:

  For a percentage column, what the percentage is a percentage OF, and
  hence which axis its reference lies on (as a single string): `"row"`,
  `"col"`, `"all"` (frequencies by subtable / group, i.e. by
  `tab_vars`), `"all_tabs"` (frequencies for the whole table), or
  `"none"` (counts, means, coefficients).

- totcol:

  `TRUE` when the vector is a total column

- replace_na:

  By default, `get_comp_all` takes NA in comparison level to be a
  `FALSE` (=comparison at subtables/groups level). Set to `FALSE` to
  avoid this behavior.

- comp_all:

  `FALSE` when the comparison level is the subtable/group, `TRUE` when
  it is the whole table

- ref:

  The type of difference of the vector. Cf.
  [`tab`](https://bricenocenti.github.io/tabxplor/reference/tab.md).

- col_var:

  The name of the `col_var` used to calculate the vector

- model_family:

  For regression tables
  ([`tab_reg`](https://bricenocenti.github.io/tabxplor/reference/tab_reg.md)):
  the column's model family (`"binomial"`, `"gaussian"`, `"poisson"`,
  `"multinomial"`, `"ordinal"`), as a single string. Empty (`""`) on
  cross-tables. Lets a table mix several outcomes with different
  families, each column keeping its own effect wording.

- refcol:

  `TRUE` when the vector is a reference column

- color:

  The colour measure, as a single string — how a cell's value is
  compared to colour it (significance is handled separately by
  `color_signif`):

  - `"no"`: no colors are printed.

  - `"diff"` (`"difference"`): the cell's difference from the reference
    (a total, or the first cell when `ref = "first"`) — percentage
    points for factors, a standardized difference for means.

  - `"ratio"`: the ratio to the reference (relative risk for
    percentages, mean ratio).

  - `"or"` (`"odds_ratio"`): the odds ratio, for row/col percentages.

  - `"contrib"`: the cell's contribution to the table's variance. Under
    `color_signif = "guaranteed_effect"` it switches to the absolute
    adjusted standardized residual — see
    [`tab`](https://bricenocenti.github.io/tabxplor/reference/tab.md).

  - `"adjustment"` / `"between_groups"`: the two
    [`tab_reg`](https://bricenocenti.github.io/tabxplor/reference/tab_reg.md)
    measures, which compare a cell to *another column* rather than to a
    reference row. A hand-built column may carry them, provided it fills
    the `obs` field they score.

  The value is **validated and normalised**: every accepted spelling —
  the discipline's acronyms included (`"RD"`, `"RR"`, `"IRR"`, `"RoM"`,
  `"OR"` and their lowercase twins) — is stored as its canonical measure
  name, and an unknown one is an error. The tabxplor 1.x combined
  strings `"diff_ci"` / `"after_ci"` still work but are superseded by
  the `color` + `color_signif` pair; here they resolve to their
  *measure* half only, so pass the significance policy through
  `color_signif`.

- color_signif:

  How significance gates the color, as a single string (`"ignore"` /
  `"grey_non_signif"` / `"guaranteed_effect"`). See
  [`tab`](https://bricenocenti.github.io/tabxplor/reference/tab.md).

## Value

A getter returns the stored value — its declared default where the
attribute was never set (`""` for a name, `FALSE` for a flag, `NA` where
there is none) — and answers once per `fmt` column when given a
data.frame. A setter returns the modified `fmt` vector.

## Functions

- `get_scale()`: get the estimate scale of fmt columns (at `fmt` level
  or `tab` level)

- `set_scale()`: set the estimate scale attribute of a `fmt` vector

- `get_pct_type()`: get which kind of percentage fmt columns hold (at
  `fmt` level or `tab` level)

- `set_pct_type()`: set the percentage-type attribute of a `fmt` vector

- `is_totcol()`: test function for total columns (at `fmt` level or
  `tab` level)

- `as_totcol()`: set the "totcol" attribute of a `fmt` vector

- `get_comp_all()`: get comparison level of fmt columns

- `set_comp_all()`: set the comparison level attribute of a `fmt` vector

- `get_ref_type()`: get differences type of fmt columns (at `fmt` level
  or `tab` level)

- `set_ref_type()`: set the reference attribute of a `fmt` vector —
  which row or column a comparison is made against. It is the writer of
  the attribute `get_ref_type()` reads.

- `set_diff_type()`: **\[deprecated\]** Use `set_ref_type()`, which
  shares its stem with the getter `get_ref_type()` and with the `ref`
  attribute both of them address.

- `get_col_var()`: get names of column variable of fmt columns (at `fmt`
  level or `tab` level)

- `set_col_var()`: set the "col_var" attribute of a `fmt` vector

- `get_col_group()`: get the sub-population of fmt columns (at `fmt`
  level or `tab` level)

- `get_model_family()`: get the regression model family of fmt columns
  (at `fmt` or `tab` level)

- `set_model_family()`: set the "model_family" attribute of a `fmt`
  vector (the per-column regression family, "" on crosstabs)

- `get_ci_method()`: get the interval method of fmt columns (at `fmt`
  level or `tab` level)

- `is_refcol()`: test function for reference columns (at `fmt` level or
  `tab` level)

- `as_refcol()`: set the "ref_col" attribute of a `fmt` vector

- `get_color()`: get color (at `fmt` level or `tab` level)

- `get_color_bg()`: get the background-channel color measure (`NA` when
  there is none)

- `get_color_signif()`: get the significance policy (`"ignore"` /
  `"grey_non_signif"` / `"guaranteed_effect"`)

- `set_color()`: set the "color" attribute of a `fmt` vector

- `set_color_signif()`: set the significance policy attribute of a `fmt`
  vector

## See also

[`fmt_attr()`](https://bricenocenti.github.io/tabxplor/reference/fmt_attr.md)
to address an attribute by name;
[`tab_columns()`](https://bricenocenti.github.io/tabxplor/reference/tab_columns.md)
for a whole table;
[`fmt()`](https://bricenocenti.github.io/tabxplor/reference/fmt.md) for
what every attribute means;
[fmt_fields](https://bricenocenti.github.io/tabxplor/reference/fmt_fields.md)
for the per-cell values.
