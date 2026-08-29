# What a table cell shows: the display grammar

Every function that builds a table takes a `display` argument, and
[`set_display()`](https://bricenocenti.github.io/tabxplor/reference/fmt_fields.md)
changes it afterwards. This page is its vocabulary: the fields a cell
may show, and the named layouts that arrange them.

Choosing a display never triggers a computation and never changes a
number — every field is already stored in the cell (see
[fmt](https://bricenocenti.github.io/tabxplor/reference/fmt.md)), so
[`set_display()`](https://bricenocenti.github.io/tabxplor/reference/fmt_fields.md)
on a finished table gives exactly what asking for it in the call would
have.

## Details

Three ways to ask, from the shortest:

- a **named layout**: `display = "est_ci"`, `"base_ratio"`, `"mean_sd"`.

- a **single field**: `display = "ci"`, `"diff"`, `"n"`.

- a **[`{}`](https://rdrr.io/r/base/Paren.html) template** of your own:
  `"{est} ({base})"`, `"{pct} [{n}]"`.

In a template, the **primary** field is the first one written *outside*
brackets — so an aside may come first, `"({base}) {est}"`, without
ceasing to be an aside. The primary carries the significance stars, it
is what Excel writes and what
[`get_num()`](https://bricenocenti.github.io/tabxplor/reference/fmt_fields.md)
returns, and it is the part the colours paint.

A field may carry **its own precision**, `"{est:3} ({base:1})"`, which
beats every default — the only way to set an aside's decimals
independently of the estimate's.

`est` and `base` are **scale-relative**: `est` is whatever the column
estimates (a percentage, a mean difference, an odds ratio) and `base`
the level it sits on. That is what lets one layout name mean the same
thing on a
[`tab()`](https://bricenocenti.github.io/tabxplor/reference/tab.md)
crosstab and on a
[`tab_reg()`](https://bricenocenti.github.io/tabxplor/reference/tab_reg.md)
regression table.

A field with nothing to show renders blank but keeps its width, so the
column stays aligned; a field empty in the whole column is dropped, and
a note says which argument would have filled it.

## Display fields

The fields a [`{}`](https://rdrr.io/r/base/Paren.html) template may
name, and `display` may name on their own.

- `pct` — the percentage.

- `n` — the count.

- `wn` — the weighted count.

- `mean` — the mean. Needs a numeric col_var.

- `est` — the estimate, whatever this column estimates — an odds ratio,
  a risk difference, a coefficient, a percentage. The one token that
  means the same thing on every table.

- `base` — the level the estimate sits on: the percentage, the mean or
  the count. On a plain percentage table it is the same number as `est`;
  beside a regression effect it is the adjusted prediction. Needs a
  column that has a level beside its estimate.

- `diff` — the difference from the reference. Needs a `ref` to compare
  to, and pct = "row" / "col".

- `ratio` — the ratio to the reference (relative risk, or a ratio of
  means). Needs a `ref` to compare to, and pct = "row" / "col".

- `ci` — the confidence interval of whatever the column compares, as
  `[low;high]`. Needs ci = "ref" (or ci = "cell" for each cell's own
  interval).

- `moe` — the margin of error — the same interval as `ci`, written as
  the half-width `+/-x` around the estimate. Void where the column
  compares a RATIO: a ratio's interval is symmetric on the LOG scale, so
  it has no half-width. Needs ci = "ref" (or ci = "cell" for each cell's
  own interval).

- `or` — the odds ratio. Needs pct = "row" / "col" (an odds ratio needs
  a percentage base).

- `ctr` — the cell's contribution to the chi-squared. Needs test = TRUE
  (the contributions come from the chi-squared).

- `var` — the variance. Needs a numeric col_var.

- `sd` — the standard deviation, in the variable's own unit. Needs a
  numeric col_var.

- `cv` — the coefficient of variation — the standard deviation as a
  percentage of the mean. Needs a numeric col_var whose mean is
  positive.

- `resid` — the adjusted standardized residual – whether the cell
  departs from independence. Derived from the p-value and the sign of
  `ctr`, so it is read-only. Needs test = TRUE (the residual comes from
  the chi-squared).

- `obs` — the OBSERVED (crude) effect a modelled one is compared to.
  [`tab_reg()`](https://bricenocenti.github.io/tabxplor/reference/tab_reg.md)
  tables only. Needs tab_reg(empirical = TRUE) (an observed effect to
  compare the model to).

- `coef` — the estimate on the model's LINK scale — the coefficient a
  linear or log-link model fitted. The same number as `est` where the
  column is already additive, its logarithm where the column shows a
  ratio. Needs a
  [`tab_reg()`](https://bricenocenti.github.io/tabxplor/reference/tab_reg.md)
  column (a crosstab estimates no coefficient).

- `gap` — how far adjustment moved the effect: the gap between the
  modelled estimate and its observed counterpart, on the estimate's own
  scale. What `color = "adjustment"` grades — readable in print and
  Excel, not only in an html tooltip. Needs tab_reg(empirical = TRUE) (a
  model effect and its observed counterpart).

## Display layouts

The named layouts `display` accepts. They are spelt with the
scale-relative `{est}` / `{base}` fields, so one name means the same
thing on a crosstab and on a
[`tab_reg`](https://bricenocenti.github.io/tabxplor/reference/tab_reg.md)
table:

- `"est"` (`{est}`) — the estimate alone.

- `"est_ci"` (`{est} {ci}`) — the estimate with its confidence interval.

- `"est_base"` (`{est} ({base})`) — the estimate and, in parentheses,
  the level it sits on.

- `"est_base_once"` (`{est}`) — the estimate alone — the level is stated
  once, by the observed column beside it.

- `"est_coef"` (`{est} ({coef})`) — the estimate and, in parentheses,
  the model's own coefficient.

- `"base_est_mdiff"` (`{est} ({diff})`) — the estimate and, in
  parentheses, the same comparison as a difference.

- `"base_est_mratio"` (`{est} ({ratio})`) — the estimate and, in
  parentheses, the same comparison as a ratio.

- `"est_obs"` (`({obs}) {est}`) — the estimate and, before it in
  parentheses, the observed (crude) effect it is compared to.

- `"base_est"` (`({base}) {est}`) — the level, then the estimate — the
  mirror of `est_base`, which sets a crude and a modelled effect side by
  side.

- `"base"` (`{base}`) — the level alone: the percentage, the mean or the
  count.

- `"base_ci"` (`{base} {ci}`) — the level with its confidence interval.

- `"base_moe"` (`{base} {moe}`) — the level with its margin of error.

- `"base_diff"` (`{base} ({diff})`) — the level and, in parentheses, its
  difference to the reference.

- `"base_ratio"` (`{base} ({ratio})`) — the level and, in parentheses,
  its ratio to the reference.

- `"base_or"` (`{base} ({or})`) — the level and, in parentheses, its
  odds ratio.

- `"or_base"` (`{or} ({base})`) — the odds ratio and, in parentheses,
  the percentage it rests on.

- `"mean_sd"` (`{mean} (σ{sd})`) — the mean and, in parentheses, its
  standard deviation.

- `"mean_cv"` (`{mean} (cv {cv})`) — the mean and, in parentheses, its
  coefficient of variation — the spread as a percentage of the mean,
  comparable between columns measured in different units (the default
  where every mean is positive).

## See also

[`set_display()`](https://bricenocenti.github.io/tabxplor/reference/fmt_fields.md)
and
[`get_display()`](https://bricenocenti.github.io/tabxplor/reference/get_display.md)
change or read it on a built table;
[`tab()`](https://bricenocenti.github.io/tabxplor/reference/tab.md) and
[`tab_reg()`](https://bricenocenti.github.io/tabxplor/reference/tab_reg.md)
set it in the call;
[fmt](https://bricenocenti.github.io/tabxplor/reference/fmt.md)
describes every field a cell stores, and
[tabxplor-options](https://bricenocenti.github.io/tabxplor/reference/tabxplor-options.md)
the session-wide defaults.
