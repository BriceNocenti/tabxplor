# tabxplor 2.0.0 — performance review

This is the Phase 22h review: what every tabxplor default costs, measured on real-world use cases, so that the ones which are expensive can be made opt-in and the ones which are cheap can stay on. It also answers the second question the phase asks — **how much faster 2.0.0 is than the 1.3.1 the users have today**, on the same data and the same tables.

Two scripts produce everything here, and nothing in this file was typed by hand:

```bash
Rscript dev/benchmarks/phase22h_perf_review.R --tag=desktop       # measure  -> results_2.0.0/*.csv
Rscript dev/benchmarks/phase22h_report_tables.R                   # read     -> the tables below
```

Re-run both after any change and the whole grid regenerates. The harness takes **about 4 minutes per machine profile** (146 cases, ~135 s of that actually inside tabxplor), which is what keeps it re-runnable rather than a once-ever exercise.

---

## 1. How to read this

Each case is **one user-visible call** — never an internal function. A default is judged by the wait it puts on a person, not by where the time goes inside the package.

Three fixtures stand for three real situations:

| fixture | rows      | what it stands for                                                           |
|---------|-----------|------------------------------------------------------------------------------|
| `gss`   | 21 483    | the teaching size: `forcats::gss_cat`, one class, one exercise               |
| `gss10` | 214 830   | a national survey (Enquête Emploi, EU-SILC): same levels, ten times the rows |
| `big`   | 8 000 000 | the stress fixture (`gen_big_df.R`), 336 MB in memory, 8 columns             |

`gss10` is `gss` replicated ten times **on purpose**: the level structure — and therefore the number of *cells* — is identical, so a difference between the two is a difference in `n` and nothing else. That single control turns out to answer the biggest question in the review (§3.1).

Measurement rules, all of them chosen because the first draft got caught by their absence:

- one warm-up run per case, **discarded**; then at least three measured runs, or one if the case is slower than 2 s;
- **`min` is the headline** (the least-noise estimate); the median is kept in the CSV beside it;
- `data.table` is pinned to **one thread** unless the case is about threads — a number that silently used six cores cannot be compared with the same number on a student's machine. ⚠ This makes the 8M *factor* rows about 1.2–1.6× pessimistic against a default R session, and the small-table rows slightly optimistic; §6.3 measures both directions.
- a whole throwaway table (and a throwaway `tab_reg`, print, html, markdown and Excel export) is built before the first case, so R's byte-compiler and the first-use cost of every render path land nowhere.

⚠ Two artefacts were found and removed while building this, and both are worth remembering before reading any benchmark of this package: a **median of two runs is a mean**, so a single cold run halves into the headline (the console-print case read 0.29 s instead of 0.087 s that way); and the **first** operation of a kind in a process can cost 5–10× the second (the default confidence interval briefly looked 17× more expensive than it is).

### The machine

|            |                                                                        |
|------------|------------------------------------------------------------------------|
| CPU        | AMD Ryzen 7 5800X3D, 8 cores / 16 threads (12 logical exposed to WSL2) |
| RAM        | 31 GB inside WSL2                                                      |
| OS / R     | Ubuntu 26.04 (WSL2), R 4.6.1                                           |
| BLAS       | OpenBLAS **pthread** build, 12 threads by default                      |
| data.table | 6 threads by default (50 % of cores); `setDTthreads(0)` gives 12       |

Three profiles were run: **12 cores** (everything available), **2 cores** (`taskset -c 0,1`), **1 core** (`taskset -c 0`).

---

## 2. The verdict, one default at a time

This is the table the phase exists for. "Cost" is the marginal cost of the option on the `gss` fixture, against the same table with the option at its default.

| default                              | shipped | what it costs                     | verdict                             |
|--------------------------------------|---------|-----------------------------------|-------------------------------------|
| `ci = "auto"` (with `stars`)         | ON      | **+0.000 s**                      | ✓ keep — free, and stars need it    |
| `stars = TRUE`                       | ON      | +0.000 s                          | ✓ keep                              |
| `test = FALSE` (chi² / ANOVA)        | OFF     | +0.034 s (**×1.85**)              | ✓ keep off — the dearest of them    |
| `color = "no"`                       | OFF     | +0.001 s (×1.02)                  | ✓ cost argues neither way           |
| `color_signif = "ignore"`            | ON      | +0.000 s                          | ✓ keep                              |
| `design_effect = FALSE`              | OFF     | +0.001 s (weighted)               | ⚠ **cost is no reason to keep off** |
| `totaltab = "line"`                  | ON      | `"table"` +0.026 s (×1.65)        | ✓ keep                              |
| `comp = "tab"`                       | ON      | `"all"` +0.000 s                  | ✓ keep                              |
| `n` / `digits` / `cleannames` / `na` | —       | +0.000 s each                     | ✓ free, all of them                 |
| `shape = "auto"` (numeric row_var)   | ON      | +0.015 s (×1.38)                  | ✓ keep — it makes a number readable |
| `spread_vars`                        | OFF     | ×2.80 when used                   | ✓ explicit ask, explicit price      |
| `tabxplor.parallel = FALSE`          | OFF     | ×2.4–3.8 warm; see §7             | ✓ keep off — §7.5                   |
| `tabxplor.spark = "all"`             | ON      | +0.004 s                          | ✓ keep — genuinely free             |
| `tab_kable_tooltips = TRUE`          | ON      | +32 % of an html export           | ✓ keep                              |
| `tab_reg(stats =)` default set       | ON      | binom +16 %, ord +99 %, MNL +62 % | ⚠ **revisit for MNL** §6.2          |
| `tab_reg(empirical = FALSE)`         | OFF     | +33 %                             | ⚠ cheap; keep off for meaning       |
| `tab_reg(color = TRUE)`              | ON      | +0.005 s                          | ✓ keep                              |
| `tab_reg(effect = "auto")`           | —       | `at_reference` ×2.7 `marginal`    | ✓ keep — `auto` picks the cheap one |

**Summary: the crosstab defaults are already right.** Not one of them is expensive, `test = FALSE` is the correct call for the only one that is, and the interval that 2.0.0 turned on by default — the thing most likely to have cost something — costs nothing measurable. The only defaults worth a decision are on the regression side, and only for one family.

---

## 3. Where the time goes

### 3.1 A crosstab does not care how big your data is

| case                       | 12 cores | 2 cores | 1 core | 1c/12c |
|----------------------------|----------|---------|--------|--------|
| counts only [gss]          | 0.034    | 0.034   | 0.035  | 1.03x  |
| pct=row, defaults [gss]    | 0.040    | 0.040   | 0.041  | 1.02x  |
| pct=row+test+color [gss]   | 0.075    | 0.075   | 0.076  | 1.01x  |
| counts only [gss10]        | 0.036    | 0.036   | 0.037  | 1.03x  |
| pct=row, defaults [gss10]  | 0.041    | 0.042   | 0.043  | 1.05x  |
| pct=row+test+color [gss10] | 0.066    | 0.067   | 0.069  | 1.05x  |
| counts only [big]          | 0.111    | 0.112   | 0.113  | 1.02x  |
| pct=row, defaults [big]    | 0.117    | 0.118   | 0.119  | 1.02x  |
| pct=row+test+color [big]   | 0.142    | 0.143   | 0.144  | 1.01x  |

Read the three fixtures down each block. From 21 483 rows to 214 830 — **ten times the data** — a default table goes from 0.040 s to 0.041 s. It takes **8 million** rows before `n` shows up at all, and even there the table costs 0.117 s.

This is the single most useful fact in the review: **tabxplor's cost is O(cells), not O(rows)**. A table's price is set by how many cells it has and how many things each cell must know, not by the size of the survey behind it. Everything else here follows from it — it is why the marginal-cost table below barely moves between fixtures, why parallelism helps (it splits *tables*, not rows), and why a student on a small machine and a researcher on a big survey wait about the same time.

### 3.2 The marginal cost of one crosstab option

Each row is the same table with exactly one option moved.

*`gss`, 21 483 rows — baseline `tab(pct = "row")` = 0.040 s*

| option                          | group   | seconds | vs baseline | x baseline |
|---------------------------------|---------|---------|-------------|------------|
| stars=FALSE (no interval)       | ci      | 0.040   | 0.000 s     | 1.00x      |
| ci='no' explicit                | ci      | 0.040   | 0.000 s     | 1.00x      |
| ci='cell'                       | ci      | 0.041   | 0.001 s     | 1.02x      |
| test=TRUE                       | test    | 0.074   | 0.034 s     | 1.85x      |
| color=TRUE                      | color   | 0.041   | 0.001 s     | 1.02x      |
| color='contrib'                 | color   | 0.085   | 0.045 s     | 2.12x      |
| color_signif='guaranteed'       | color   | 0.040   | 0.000 s     | 1.00x      |
| wt=                             | weights | 0.044   | 0.004 s     | 1.10x      |
| wt= + design_effect             | weights | 0.045   | 0.005 s     | 1.12x      |
| survey design (ids=~1)          | weights | 0.050   | 0.010 s     | 1.25x      |
| comp='all'                      | shape   | 0.039   | -0.001 s    | 0.97x      |
| tab_vars (3-6 subtables)        | shape   | 0.067   | 0.027 s     | 1.68x      |
| totaltab='table'                | shape   | 0.066   | 0.026 s     | 1.65x      |
| spread_vars                     | shape   | 0.112   | 0.072 s     | 2.80x      |
| n='range'                       | display | 0.040   | 0.000 s     | 1.00x      |
| digits=1                        | display | 0.040   | 0.000 s     | 1.00x      |
| cleannames=FALSE                | display | 0.041   | 0.001 s     | 1.02x      |
| na='drop'                       | shape   | 0.040   | 0.000 s     | 1.00x      |
| numeric col_var (mean)          | numeric | 0.034   | -0.006 s    | 0.85x      |
| numeric col_var stars=F         | numeric | 0.034   | -0.006 s    | 0.85x      |
| numeric col_var display={mean}  | numeric | 0.036   | -0.004 s    | 0.90x      |
| numeric row_var (auto shape)    | numeric | 0.055   | 0.015 s     | 1.38x      |
| numeric row_var shape=quartiles | numeric | 0.061   | 0.021 s     | 1.52x      |

*`gss10`, 214 830 rows — baseline `tab(pct = "row")` = 0.043 s*

| option                          | group   | seconds | vs baseline | x baseline |
|---------------------------------|---------|---------|-------------|------------|
| stars=FALSE (no interval)       | ci      | 0.043   | 0.000 s     | 1.00x      |
| ci='no' explicit                | ci      | 0.042   | -0.001 s    | 0.98x      |
| ci='cell'                       | ci      | 0.042   | -0.001 s    | 0.98x      |
| test=TRUE                       | test    | 0.066   | 0.023 s     | 1.53x      |
| color=TRUE                      | color   | 0.043   | 0.000 s     | 1.00x      |
| color='contrib'                 | color   | 0.080   | 0.037 s     | 1.86x      |
| color_signif='guaranteed'       | color   | 0.043   | 0.000 s     | 1.00x      |
| wt=                             | weights | 0.048   | 0.005 s     | 1.12x      |
| wt= + design_effect             | weights | 0.050   | 0.007 s     | 1.16x      |
| survey design (ids=~1)          | weights | 0.067   | 0.024 s     | 1.56x      |
| comp='all'                      | shape   | 0.042   | -0.001 s    | 0.98x      |
| tab_vars (3-6 subtables)        | shape   | 0.069   | 0.026 s     | 1.60x      |
| totaltab='table'                | shape   | 0.069   | 0.026 s     | 1.60x      |
| spread_vars                     | shape   | 0.116   | 0.073 s     | 2.70x      |
| n='range'                       | display | 0.043   | 0.000 s     | 1.00x      |
| digits=1                        | display | 0.043   | 0.000 s     | 1.00x      |
| cleannames=FALSE                | display | 0.043   | 0.000 s     | 1.00x      |
| na='drop'                       | shape   | 0.041   | -0.002 s    | 0.95x      |
| numeric col_var (mean)          | numeric | 0.037   | -0.006 s    | 0.86x      |
| numeric col_var stars=F         | numeric | 0.038   | -0.005 s    | 0.88x      |
| numeric col_var display={mean}  | numeric | 0.039   | -0.004 s    | 0.91x      |
| numeric row_var (auto shape)    | numeric | 0.072   | 0.029 s     | 1.67x      |
| numeric row_var shape=quartiles | numeric | 0.078   | 0.035 s     | 1.81x      |

What this says:

- **The default interval is free.** `stars = FALSE`, `ci = "no"` and the default all measure the same 0.040 s. Whatever the reference interval costs, it is below the noise floor of a table build. This matters because it is the one 2.0.0 default that 1.3.1 did not have (1.3.1's `ci` defaults to `"no"`), so it was the obvious suspect for a regression — and it is not one.
- **`test = TRUE` is the expensive crosstab option**, and the only one: ×1.85 at 21k, ×1.53 at 215k. It is off by default. `color = "contrib"` costs ×2.12 purely because it turns the test on.
- **Weights are nearly free** (×1.10), and the **design effect on top of them is nearly free again** (×1.12 total). A real `survey` design costs ×1.25 at 21k and ×1.56 at 215k — still small, and it buys the whole design-based variance.
- **Shape options cost what they add**: a second axis of sub-tables (`tab_vars`, `totaltab = "table"`) ×1.65, a spread ×2.80.
- **Display options are free**: `n`, `digits`, `cleannames`, `na`, `comp` all land within noise.
- A numeric column variable is **cheaper** than a factor one (×0.85): it aggregates to moment sums instead of a contingency grid.

### 3.3 The table-of-tables, and what parallelism is worth

The exploratory workflow — 8 row variables × 3 column variables, 24 tables, percentages + test + colour in one call:

*12 cores, `gss`*

| mode       | seconds | vs serial |
|------------|---------|-----------|
| serial     | 1.549   | 1.00x     |
| parallel=2 | 1.010   | 1.53x     |
| parallel=4 | 0.588   | 2.63x     |
| parallel=8 | 0.537   | 2.88x     |

*12 cores, `gss10`*

| mode       | seconds | vs serial |
|------------|---------|-----------|
| serial     | 1.539   | 1.00x     |
| parallel=2 | 0.905   | 1.70x     |
| parallel=4 | 0.612   | 2.51x     |
| parallel=8 | 0.590   | 2.61x     |

*2 cores, `gss`*

| mode       | seconds | vs serial |
|------------|---------|-----------|
| serial     | 1.577   | 1.00x     |
| parallel=2 | 1.094   | 1.44x     |
| parallel=4 | 1.021   | 1.54x     |
| parallel=8 | 1.342   | 1.18x     |

*2 cores, `gss10`*

| mode       | seconds | vs serial |
|------------|---------|-----------|
| serial     | 1.538   | 1.00x     |
| parallel=2 | 0.920   | 1.67x     |
| parallel=4 | 0.941   | 1.63x     |
| parallel=8 | 1.546   | 0.99x     |

On 12 cores, parallelism is worth **×2.9**, and almost all of it arrives by 4 workers (×2.6). On 2 cores it is worth **×1.7 at 2 workers** and then goes backwards — **8 workers on 2 cores is slower than 4, and on the bigger fixture it is no better than serial at all**.

That is the case for keeping `tabxplor.parallel = FALSE` as the default, and for what `TRUE` resolves to: physical cores − 1, capped at 8. On a 4-core student machine that is 3 workers, which is on the good side of the curve. The failure mode to avoid is a user writing `parallel = 8` on a small laptop.

Note that the gain is identical on `gss` and `gss10` — parallelism here splits *tables across workers*, so it scales with the number of tables, not with `n`.

### 3.4 Exports and printing

| case                       | 12 cores | 2 cores | 1 core | 1c/12c |
|----------------------------|----------|---------|--------|--------|
| print to console (1 table) | 0.087    | 0.088   | 0.087  | 1.00x  |
| tab_html (1 table)         | 0.149    | 0.151   | 0.156  | 1.05x  |
| tab_html tooltips=FALSE    | 0.113    | 0.116   | 0.119  | 1.05x  |
| tab_html (5 row_vars)      | 0.284    | 0.280   | 0.287  | 1.01x  |
| tab_kable (1 table)        | 0.150    | 0.153   | 0.156  | 1.04x  |
| tab_md (1 table)           | 0.111    | 0.111   | 0.111  | 1.00x  |
| tab_xl (1 table)           | 0.373    | 0.382   | 0.392  | 1.05x  |
| tab_xl (5 row_vars)        | 0.870    | 0.859   | 0.893  | 1.03x  |
| tab_plot (1 table)         | 2.485    | 2.462   | 2.573  | 1.04x  |

The number to notice is the first one. **Printing a table to the console costs 0.087 s — about twice what building it cost (0.040 s).** In an interactive session every table is printed, so for a user exploring data, rendering *is* the wait, not computing. That is not a defect (0.087 s is imperceptible), but it sets the scale for anything that might be added to the console renderer later.

The rest:

- `tab_md` 0.111 s, `tab_html` 0.149 s, `tab_kable` 0.150 s — all in the same band;
- tooltips cost **+32 %** of an html export (0.113 → 0.149 s) and are on by default; that is a fair price for the hover, and the option to turn them off exists;
- `tab_xl` has a **fixed cost of ~0.37 s** for one table, and grows slowly (0.87 s for five) — that is openxlsx2 building a workbook, not tabxplor;
- `tab_plot` is **2.49 s**, the most expensive single call in the whole review. It is ggplot2 building and drawing a grob table. Always explicit, never a default.

### 3.5 Regressions

| case                                       | 12 cores | 2 cores | 1 core | 1c/12c |
|--------------------------------------------|----------|---------|--------|--------|
| binomial, 3 predictors (default)           | 0.165    | 0.108   | 0.110  | 0.67x  |
| binomial, stats='none'                     | 0.142    | 0.091   | 0.093  | 0.65x  |
| binomial, stats='all'                      | 0.276    | 0.201   | 0.214  | 0.78x  |
| binomial, color='no'                       | 0.160    | 0.111   | 0.110  | 0.69x  |
| binomial + empirical=TRUE                  | 0.220    | 0.179   | 0.172  | 0.78x  |
| binomial + empirical + adjustment colour   | 0.239    | 0.191   | 0.197  | 0.82x  |
| binomial, 5 predictors                     | 0.449    | 0.409   | 0.419  | 0.93x  |
| binomial measure='ratio' (RR fit)          | 0.234    | 0.196   | 0.209  | 0.89x  |
| binomial measure='difference' (RD fit)     | 0.167    | 0.116   | 0.145  | 0.87x  |
| binomial effect='marginal' RD              | 0.174    | 0.115   | 0.144  | 0.83x  |
| binomial effect='at_reference'             | 0.478    | 0.470   | 0.482  | 1.01x  |
| gaussian (numeric outcome)                 | 0.131    | 0.121   | 0.125  | 0.95x  |
| poisson (count outcome)                    | 0.087    | 0.087   | 0.089  | 1.02x  |
| multinomial (relig, 13 levels)             | 6.592    | 6.351   | 6.574  | 1.00x  |
| ordinal (rincome, 4 levels)                | 0.477    | 0.419   | 0.421  | 0.88x  |
| ordinal, stats='none' (no Brant refit)     | 0.240    | 0.271   | 0.223  | 0.93x  |
| 2 outcomes                                 | 0.199    | 0.180   | 0.183  | 0.92x  |
| 4 nested models                            | 0.563    | 0.565   | 0.467  | 0.83x  |
| tab_vars (3 groups)                        | 0.200    | 0.207   | 0.212  | 1.06x  |
| numeric predictor + sparkline              | 0.076    | 0.069   | 0.067  | 0.88x  |
| numeric predictor shape='quartiles'        | 0.098    | 0.083   | 0.085  | 0.87x  |
| numeric predictor, spark='no'              | 0.072    | 0.067   | 0.068  | 0.94x  |
| binomial 3 predictors [gss10 215k]         | 0.964    | 0.796   | 0.722  | 0.75x  |
| binomial + empirical [gss10 215k]          | 1.111    | 0.865   | 0.890  | 0.80x  |
| binomial effect='marginal' RD [gss10 215k] | 0.931    | 0.751   | 0.793  | 0.85x  |
| binomial stats='none' [gss10 215k]         | 0.741    | 0.621   | 0.654  | 0.88x  |
| reg_check_plots(check='auto')              | 0.618    | 0.606   | 0.620  | 1.00x  |
| survey design, binomial                    | 0.326    | 0.288   | 0.289  | 0.89x  |

⚠ This is the one suite where the **12-core column is the slowest** — `glm()` goes through BLAS, and on this box's OpenBLAS-pthread build more cores means more threads spawned for a matrix too small to need them (§6.4). It is an artefact of this machine, not of `tab_reg()`; read the 2-core column as the fair one.

The three things worth acting on, in order:

1. **A multinomial table costs 6.6 s** — 40× a binomial one, and it is the only case in the entire review that a user would actually notice. §6.2 takes it apart.
2. **An ordinal table pays a refit it does not always need**: 0.477 s against 0.240 s with `stats = "none"` — the Brant proportionality test, which is `cost = "refit"` and still `footer_default = TRUE`. That is a deliberate, documented exception ("a cumulative odds ratio that fails it is not one number but a fiction") and at 21k rows it is defensible. At survey `n` it doubles the wait for a table.
3. **`effect = "at_reference"` is ×2.7 `effect = "marginal"`** (0.478 vs 0.174 s), because it goes out to `marginaleffects` while the marginal path uses tabxplor's own analytic g-computation. Worth knowing; `auto` never picks it.

And the things that are **not** problems, which is just as useful to record:

- the default footer checks cost **+16 %** on a binomial (0.165 vs 0.142 s) — the `cost = "free"` label is honest here;
- `empirical = TRUE`, the package's headline feature, costs **+33 %** (0.220 vs 0.165 s) and only +15 % at 215k rows. It is off by default for reasons of meaning, and nothing about its price argues either way;
- the **observed-shape sparkline is free**: 0.076 s with it, 0.072 s with `spark = "no"`;
- `colour`, `stars` and the two gap measures are all within noise;
- at 215k rows a regression table costs ~1 s, and **`glm()` itself is most of it** (`stats = "none"` is still 0.741 s) — tabxplor is not the bottleneck there, and cannot be.

### 3.6 jamovi, which is what the students will actually use

| case                       | 12 cores | 2 cores | 1 core | 1c/12c |
|----------------------------|----------|---------|--------|--------|
| jmv_build_baseline [small] | 0.006    | 0.006   | 0.006  | 1.00x  |
| jmv_change_pct [small]     | 0.106    | 0.106   | 0.111  | 1.05x  |
| jmv_change_color [small]   | 0.027    | 0.027   | 0.028  | 1.04x  |
| jmv_change_ref [small]     | 0.037    | 0.037   | 0.039  | 1.05x  |
| jmv_change_digits [small]  | 0.006    | 0.006   | 0.006  | 1.00x  |
| jmv_render_kable [small]   | 0.211    | 0.212   | 0.219  | 1.04x  |
| jmv_build_baseline [big]   | 0.008    | 0.009   | 0.009  | 1.12x  |
| jmv_change_pct [big]       | 0.335    | 0.351   | 0.364  | 1.09x  |
| jmv_change_color [big]     | 0.301    | 0.321   | 0.316  | 1.05x  |
| jmv_change_ref [big]       | 0.335    | 0.352   | 0.354  | 1.06x  |
| jmv_change_digits [big]    | 0.009    | 0.009   | 0.009  | 1.00x  |
| jmv_render_kable [big]     | 0.228    | 0.228   | 0.233  | 1.02x  |

With the live cache warm, changing one option in the Crosstables panel costs **6 ms to 0.34 s**, and the render is a further 0.21–0.23 s. A change that the cache can re-apply without rebuilding (`digits`, and the no-op baseline) is **6–9 ms** — effectively instant. A change that forces a rebuild (`pct`, `color`, `ref`) costs 0.03–0.35 s depending on the size of the table-of-tables.

So a jamovi interaction lands between **0.2 s and 0.6 s end to end**, dominated by the kable render rather than by the statistics. That is comfortably inside "feels responsive", and it will be roughly 1.5× that on a student machine (§5) — still fine.

### 3.7 The huge dataframe

| case           | 12 cores | 2 cores | 1 core | 1c/12c |
|----------------|----------|---------|--------|--------|
| tab_row_pct    | 0.093    | 0.093   | 0.093  | 1.00x  |
| tab_ci         | 0.093    | 0.093   | 0.093  | 1.00x  |
| tab_chi2       | 0.119    | 0.117   | 0.118  | 0.99x  |
| tab_num_mean   | 0.225    | 0.229   | 0.253  | 1.12x  |
| tab_num_w      | 0.646    | 0.644   | 0.654  | 1.01x  |
| tab_many_multi | 0.170    | 0.170   | 0.176  | 1.04x  |
| tab_weighted   | 0.169    | 0.173   | 0.170  | 1.01x  |

Eight million rows, one thread: every operation is **under 0.7 s**, and the plain ones are under 0.2 s. `tab_num_w` (two weighted numeric column variables, means + variance + design-effective base) is the heaviest at 0.65 s.

### 3.8 The cold session

| case                       | 12 cores | 2 cores | 1 core | 1c/12c |
|----------------------------|----------|---------|--------|--------|
| R startup only (the floor) | 0.139    | 0.114   | 0.112  | 0.81x  |
| + library(tabxplor)        | 0.453    | 0.457   | 0.432  | 0.95x  |
| + first tab()              | 0.605    | 0.619   | 0.586  | 0.97x  |
| + first tab_reg()          | 0.820    | 0.756   | 0.706  | 0.86x  |
| + first tab_xl()           | 1.289    | 1.390   | 1.252  | 0.97x  |

Measured in a fresh `Rscript` against an **installed** build, which is what a user actually runs:

- R itself starts in 0.14 s;
- `library(tabxplor)` adds **0.31 s**;
- the first `tab()` adds a further 0.15 s → **0.60 s from cold to a coloured table on screen**;
- the first `tab_reg()` → 0.82 s;
- the first `tab_xl()` → 1.29 s (openxlsx2 loads).

That is the real "first impression" number, and it is good: under a second from launching R to a finished table.

---

## 4. 1.3.1 → 2.0.0, same data, same table

The A/B runs CRAN's 1.3.1 (installed in its own library) and this source in two processes over **byte-identical fixtures** — which is why the harness builds its own `fx_gss()` out of plain dplyr/forcats instead of calling `gss_cat_data_formatting()`, a helper 1.3.1 does not have.

Pairs are written so that both sides ask for the *same table*: 1.3.1 merges with `tab_many(compact = TRUE)` where 2.0.0 uses `tab()`, spells the test `chi2 =` rather than `test =`, and defaults `ci = "no"` — so `ci` is stated explicitly on both sides. Row and column variable sets are disjoint, because 1.3.1's `tab_many(compact = FALSE)` aborts on a variable that is both.

| case                            | 1.3.1 | 2.0.0 | speedup |
|---------------------------------|-------|-------|---------|
| 1 table, counts [gss]           | 0.037 | 0.035 | 1.06x   |
| 1 table, counts [gss10]         | 0.055 | 0.035 | 1.57x   |
| 1 table, pct + ci [gss]         | 0.086 | 0.040 | 2.15x   |
| 1 table, pct + ci [gss10]       | 0.104 | 0.041 | 2.54x   |
| 1 table, pct+chi2+color [gss]   | 0.187 | 0.072 | 2.60x   |
| 1 table, pct+chi2+color [gss10] | 0.205 | 0.064 | 3.20x   |
| 1 table, pct=row [gss]          | 0.046 | 0.040 | 1.15x   |
| 1 table, pct=row [gss10]        | 0.064 | 0.041 | 1.56x   |
| 15 tables merged [gss]          | 1.874 | 0.835 | 2.24x   |
| 15 tables merged [gss10]        | 2.090 | 0.784 | 2.67x   |
| 15 tables, list [gss]           | 1.825 | 0.782 | 2.33x   |
| 15 tables, list [gss10]         | 2.027 | 0.763 | 2.66x   |
| numeric means (tab_num) [gss]   | 0.038 | 0.037 | 1.03x   |
| numeric means (tab_num) [gss10] | 0.068 | 0.040 | 1.70x   |
| weighted col% [gss]             | 0.076 | 0.046 | 1.65x   |
| weighted col% [gss10]           | 0.243 | 0.049 | 4.96x   |

*The 2.0.0 default of the same call — with an interval 1.3.1 never computed*

| case                                      | seconds |
|-------------------------------------------|---------|
| 1 table, pct=row (2.0.0 defaults) [gss]   | 0.039   |
| 15 tables merged (2.0.0 defaults) [gss]   | 0.800   |
| 1 table, pct=row (2.0.0 defaults) [gss10] | 0.040   |
| 15 tables merged (2.0.0 defaults) [gss10] | 0.788   |

**2.0.0 is between 1.0× and 5.0× faster, and 2.2–2.7× on the workflow that matters** — the fifteen-table exploration, which is what an exploratory session actually is. The gains concentrate exactly where the work is:

- **the exploratory table-of-tables: ×2.2 to ×2.7** (1.87 s → 0.84 s at 21k; 2.09 s → 0.78 s at 215k);
- **a table with a test and colour: ×2.6 to ×3.2**;
- **weighted percentages at survey n: ×5.0** (0.243 s → 0.049 s) — the single biggest win, and it grows with `n`;
- **a bare count table: ×1.0 to ×1.6** — it was already fast in 1.3.1 and there was nothing to win.

Notice that 1.3.1's cost *does* grow with `n` where 2.0.0's does not: 1.3.1 goes 0.076 → 0.243 s on weighted percentages from 21k to 215k (×3.2), 2.0.0 goes 0.046 → 0.049 s (×1.06). That is the O(cells) property of §3.1 arriving, and it means the gap widens with the size of the survey.

The last table is the honest one: **2.0.0's default is faster than 1.3.1's `ci`-less call while computing a confidence interval 1.3.1 never computed.** The interval, the stars, the design-effective base and the richer cell are all free relative to the old version.

If you want a single sentence for `NEWS.md`: *the exploratory workflow is about two and a half times faster than 1.3.1, weighted survey tables up to five times, and the default table now carries a confidence interval at no cost.*

### Against the frozen 8M references

`dev/benchmarks/` holds three earlier reference grids. Comparing against them is the internal history, not the 1.3.1 comparison:

| operation      | 07-01 ref | 07-08 pre | 07-08 post | now 1thr |
|----------------|-----------|-----------|------------|----------|
| tab_row_pct    | 0.535     | 0.116     | 0.116      | 0.093    |
| tab_ci         | 0.557     | 0.152     | 0.146      | 0.093    |
| tab_chi2       | 0.671     | 0.390     | 0.264      | 0.119    |
| tab_num_mean   | 1.080     | 1.092     | 0.196      | 0.225    |
| tab_num_w      | --        | 2.941     | 0.353      | 0.646    |
| tab_many_multi | 0.854     | 0.202     | 0.187      | 0.170    |
| tab_weighted   | 27.934    | 0.197     | 0.183      | 0.169    |

- `07-01 ref` is `baseline.csv`, the 2.0.0-dev state on 1 July — **not** 1.3.1.
- `07-08 pre` / `post` bracket Phase 2 (the moment-sums rewrite of `tab_num`).
- The `tab_weighted` line (27.9 s → 0.17 s) is Phase 2's headline, already reported there.
- ⚠ `tab_num_mean` reads 0.196 s in the post-Phase-2 file and **0.225 s** now at one thread — comparable. The 8M numeric path is the one case whose timing moves a lot with process state and thread count (§6.3), so treat that row as a range rather than a point.

---

## 5. What a cheap university all-in-one would do

The target machine is the class of all-in-one French universities bought around 2019 — an HP ProOne 400 G5, Dell OptiPlex 5270 AiO, Lenovo ThinkCentre AiO and similar. The standard configuration is an **Intel Core i3-9100T (4 cores) or i5-9500T (6 cores), 8 GB of DDR4, a 256 GB SSD**, Windows 10/11.

Two measurements make the projection much more reliable than a guess:

**a. Core count barely matters.** Pinning this box to a single core changed nothing: the median ratio across all 138 cases is **1.00×**, and the largest single effect was 1.12×. tabxplor's serial path is genuinely single-threaded, so an i3's four cores are not a handicap for one table.

**b. So the projection is essentially the single-thread ratio.** PassMark single-thread ratings (cpubenchmark.net): Ryzen 7 5800X3D **3 233**, i3-9100T **2 114**, i5-9500T **2 108** — a factor of about **×1.5**. Memory is single-channel DDR4-2666 against dual-channel DDR4-3200, so the multi-million-row cases (which stream memory rather than compute) should be given **×2**.

Projected wait on the student machine (estimate, marked as such):

| what the student does                       | here      | projected  |
|---------------------------------------------|-----------|------------|
| one cross-table with percentages and colour | 0.04 s    | ~0.06 s    |
| the same with a chi² test                   | 0.075 s   | ~0.11 s    |
| the 15-table exploration                    | 0.82 s    | ~1.2 s     |
| printing it to the console                  | 0.087 s   | ~0.13 s    |
| a jamovi option change + render             | 0.2–0.6 s | ~0.3–0.9 s |
| a binomial regression, 3 predictors         | 0.17 s    | ~0.25 s    |
| **a multinomial regression, 13 categories** | **6.6 s** | **~10 s**  |
| an Excel export                             | 0.37 s    | ~0.56 s    |
| `tab_plot`                                  | 2.49 s    | ~3.7 s     |
| cold start to first table                   | 0.60 s    | ~0.9 s     |
| a 1M-row table (interpolated, not measured) | ~0.05 s   | ~0.1 s     |

**Everything a student does in a class is under two seconds except the multinomial, and `tab_plot`.** Those two are the only places where a cheap machine turns "quick" into "noticeable".

Three notes that make the student machine *better* off than a naive scaling suggests:

- ⚠ **Windows R ships the reference BLAS, which is single-threaded.** The OpenBLAS-pthread overhead measured here (§6.4) does not exist on their machines.
- `data.table` on 4 cores defaults to 2 threads, so the small-table threading loss of §6.3 is smaller there than the 6-thread loss measured here — and a student's tables are all small ones.
- `parallel = TRUE` on an i3-9100T resolves to 3 workers — the good part of the curve in §3.3.

And one that makes it worse: **8 GB of RAM is the real constraint, not the CPU.** The 8M-row fixture is 336 MB in memory and every parallel worker gets a copy of the prepared population. A student will not have 8M rows; but `parallel = TRUE` on a large dataset in 8 GB is the way to make an all-in-one swap. That is another argument for the option staying off by default.

---

## 6. Findings — bottlenecks and cheap wins

### 6.1 `design_effect` is nearly free, so its default is a statistical question, not a performance one

`design_effect = TRUE` on a weighted table costs **+0.001 s at 21k and +0.002 s at 215k** — 2 % of the table. It is currently `FALSE`.

Nothing in this review argues for keeping it off. If the design-based interval is the more honest one on weighted data (and `dev/weights_only_design_effect_soundness.md` is where that was argued), then **cost is no objection to turning it on by default for weighted tables**. Recording it here so that the decision is taken on its merits rather than on an assumption about its price.

### 6.2 A multinomial table costs 20–40× a binomial one — the one default set worth revisiting

Measured with a controlled, repeated run — `stats = "none"` isolates the default footer checks and the fit is timed on its own. (The 13-category row is the same case as suite E's 6.59 s; this run is a median of three where the harness takes one, which is why it reads a little higher.)

| outcome                           | table (default) | `stats = "none"` | the checks        | `nnet::multinom` alone |
|-----------------------------------|-----------------|------------------|-------------------|------------------------|
| 13 categories (`relig`, raw)      | 7.02 s          | 4.33 s           | **2.69 s (38 %)** | 1.31 s                 |
| 8 categories (`relig`, collapsed) | 2.91 s          | 1.86 s           | **1.05 s (36 %)** | 0.55 s                 |

Two separate facts, and both are actionable:

**The number of outcome categories is the driver.** Collapsing `relig` from 13 to 8 categories more than halves everything. This is worth one sentence in the regression vignette: *a multinomial model costs roughly the square of its category count; collapse the outcome before you model it.*

**`cost = "free"` in `REG_CHECKS` conflates "no refit" with "cheap", and multinomial is the case that breaks it.** Dispersion and influence are both `footer_default = TRUE` and both `cost = "free"` — true in that neither refits, false in wall-clock: they need a (K−1)-block coefficient covariance (one Hessian, 0.81 s at 13 categories) and then an O(n·p) influence sweep with p = (K−1) × terms. On a binomial with p ≈ 12 that adds +16 % to the table; on a multinomial with p ≈ 36 it adds +62 %.

Three honest cures, in increasing order of change:

1. **Document it** and leave the defaults alone.
2. **Let `footer_default` be per-family** — the fact table already carries `families` per check, so a `footer_default = c(multinomial = FALSE)` form is a small, declarative extension, and it keeps the rule "what a table must say and what it costs are two questions" while admitting that for one family the answer differs.
3. **Add a third `cost` value** — `"scan"`, for a check that is O(n·p) rather than O(1) on the fit in hand — and let the default set drop `"scan"` checks above some p. This is the most faithful to the fact-table design and the most work.

Recommendation: **(2)**, and only if the maintainer agrees a multinomial footer can lose its dispersion and influence rows. Nothing here is urgent — 6.6 s is slow, not broken.

### 6.3 data.table threads help big factor tables and hurt everything else

This one changed its answer twice while being measured, and how it was measured is the finding as much as the numbers are.

⚠ **`setDTthreads()` must never be flipped inside a timing loop.** It tears down and rebuilds data.table's OpenMP pool, and the rebuild lands on whatever is timed next. An interleaved loop reported the 8M numeric case **2.2× slower** with threads; the correct design reports it unstable. ⚠ **And two whole harness runs are not a clean A/B either** — by the time a late suite runs, the two processes differ in heap and pool history by more than the effect being measured (that pair reported the same case 2.8× slower).

The design that answers it is `dev/benchmarks/phase22h_threads.R`: **one thread setting per process**, every case warmed before any timing, min of five runs, conditions alternated so drift cannot favour either.

| case                    | 1 thread | auto   | verdict      |
|-------------------------|----------|--------|--------------|
| gss 21k  pct=row        | 0.0370   | 0.0400 | 1.08x slower |
| gss 21k  pct+test+color | 0.0690   | 0.0750 | 1.09x slower |
| gss 21k  15 tables      | 0.4000   | 0.5170 | 1.29x slower |
| 8M  counts              | 0.1130   | 0.0710 | 1.59x faster |
| 8M  pct=row             | 0.1180   | 0.1020 | 1.16x faster |
| 8M  weighted pct=col    | 0.2470   | 0.2160 | 1.14x faster |
| 8M  numeric means x2    | 0.3410   | 0.5610 | 1.65x slower |

**It depends on the shape of the table, and the split is clean:**

- **every small and medium table is slower with threads** — ×1.08 for one table, ×1.09 with a test, and **×1.29 for the fifteen-table exploration**, which is the workflow people actually run;
- **8M factor tables are faster with threads** — ×1.59 for counts, ×1.16 with percentages, ×1.14 weighted;
- the **8M numeric path is unstable** across process states (0.22–0.35 s at one thread, 0.31–0.64 s at auto) and no conclusion should be drawn from it.

The reason for the split is structural: tabxplor's aggregations have very few groups (5–15) and very few columns, and a table build issues *many* small `data.table` calls. Below a few hundred thousand rows, OpenMP's fork/join per call costs more than the parallel scan saves; at eight million rows the scan finally dominates.

**So there is no free win here, only a trade.** Pinning `setDTthreads(1L)` inside `tab_build()` would buy ×1.1–1.3 on the tables everyone builds and give back ×1.2–1.6 on the very large ones — and the package already does exactly that inside its mirai daemons (`R/tab-parallel.R:222`), where each worker holds one table's worth of work and threads have nothing to win.

Worth noting for a later phase, not worth acting on in this one. If it is ever taken up, the shape of the change is a `local_dt_threads()` helper with an `on.exit()` restore (a set-and-restore pair costs **42 µs**, measured — 0.1 % of a table) driven by a `tabxplor.dt_threads` option defaulting to "leave alone", and gated on the size of the prepared population rather than applied flatly.

### 6.4 OpenBLAS thread overhead on small model fits (this box only)

On an OpenBLAS-pthread build, pinning BLAS to one thread makes `tab_reg()` about **×1.2 faster** in steady state (controlled interleaved run: 0.147 s → 0.123 s for a 3-predictor binomial). The whole suite E re-run with `--blas_threads=1` shows the same modest gain.

Two cautions, because a first pass overstated this badly:

- the **first** `glm()` in a process pays OpenBLAS's thread-pool creation, which can look like a 5–10× difference if it lands on the first measured run. It is a one-off, not a rate.
- **this is an environment property, not a tabxplor one**, and it does not exist on Windows R (reference BLAS, single-threaded), which is where the students are.

No change recommended. Worth a line in a dev note for anyone benchmarking regressions on Linux. The harness carries `--blas_threads=` so the counterfactual can always be measured.

### 6.5 A defect found and fixed: a numeric column was being stringified on every build

The first 8M numeric table measured in this phase took **21 s**. Profiling put 67 % of it on one line, `R/row-model.R:238`:

```r
lv <- if (is.factor(x)) levels(x) else unique(as.character(x))   # before
```

`lvl_check_reserved()` refuses a source level named "Total" (it would be read back as a total row). The pipeline's call site restricts it to `vars_not_numeric`; **the leaf's own call site (`R/tab-leaf.R:165`) passes its numeric column variables too** — so every numeric column was coerced to character and hashed, on every build. A numeric column cannot carry a level named "Total", so the work was not merely expensive but pointless.

```r
lv <- if (is.factor(x)) levels(x) else if (is.character(x)) unique(x) else next   # after
```

Measured on the 8M fixture:

| case                                         | before  | after  |         |
|----------------------------------------------|---------|--------|---------|
| one numeric column variable                  | 8.57 s  | 1.23 s | **×7**  |
| two numeric column variables, `comp = "all"` | 21.00 s | 0.31 s | **×67** |
| `tab_num(region, score + income, response)`  | 21.00 s | 0.50 s | **×42** |

The character path still aborts on a level named "Total" (verified). This is why suite G's `tab_num_mean` is back in line with the post-Phase-2 reference in §4 — without the fix, that row would have read 21 s.

**Why it went unnoticed:** it only bites at a scale nobody runs interactively, `dev/benchmarks/run_bench.R` had not been re-run since 8 July, and the in-suite benchmark is opt-in (`TABXPLOR_BENCH`). A standing 8M-row numeric case in this harness is the guard against the next one.

### 6.6 Smaller observations, no action proposed

- **`tab_plot` is 2.49 s**, by far the most expensive single call. ggplot2 building a grob table; nothing tabxplor-side to win.
- **`tab_xl` has a ~0.37 s fixed cost**, essentially all openxlsx2 workbook construction; it grows slowly with content (0.87 s for five tables).
- **Printing costs about twice building.** Not a problem at 0.087 s, but it means the console renderer, not the aggregation engine, is where a future slowdown would be felt first.
- **`effect = "at_reference"` is ×2.7 `marginal`** because it leaves for `marginaleffects`; `auto` never picks it.
- **The Brant refit doubles an ordinal table.** Declared, deliberate, and fine at teaching sizes — but it is the one `cost = "refit"` check that is on by default, so it is worth re-checking against a large survey before release.

---

---

## 7. Should `parallel` be on by default? (Phase 22h follow-up)

Everything in this section is measured by `dev/benchmarks/phase22h_parallel.R` (one worker count per process, pool warmed before timing, min of three) on the same three machine profiles. The short answer is: **the gain is real and bigger than expected, the first call is the problem, one blocking defect had to be fixed first, and the core-count rule needs to change before any of this is safe.**

### 7.1 A blocking defect, found and fixed: a worker did not pin its BLAS

Before any of the rest was worth asking, this had to be fixed. `tab_pmap()`'s `everywhere()` block set `data.table::setDTthreads(1L)` in each daemon but said nothing about BLAS. A daemon is a fresh R process, so on a threaded BLAS (Debian/Ubuntu's default OpenBLAS-pthread) the first `glm()` in a worker opens **one thread per core** — W workers × C cores of spinning threads on C cores.

`tab()` never noticed, because its units are data.table-bound and data.table *was* pinned. `tab_reg()`'s units are `glm`-bound, and there the contention is not marginal:

| 3 outcomes, 21k rows, 12 cores               | seconds   |
|----------------------------------------------|-----------|
| serial                                       | 0.81      |
| parallel W=3, BLAS unpinned (**as shipped**) | **56.91** |
| parallel W=3, BLAS pinned to 1 in the worker | **0.29**  |

**A 70× slowdown**, reproducible, identical at every combination of `stats` and `empirical`, and independent of data size — the tell that it was contention rather than work. `R/tab-parallel.R` now pins BLAS beside data.table, guarded on `RhpcBLASctl` (already a Suggest). ⚠ The *runtime* call is the only lever that works: OpenBLAS-pthread fixes its thread count from the environment at process start, so an `OMP_NUM_THREADS` set after the daemon exists is ignored — the same fact `tests/testthat/setup.R` already relies on.

**Why the suite did not catch it:** `test-parallel-parity.R` asserts byte-identity, never wall-clock, and identity is exactly what a thread-thrashed worker still delivers. It passes in 26/26 both before and after.

⚠ **And pinning the worker alone was not enough — it broke the byte-identity contract, which is how the second half of this fix was found.** A worker at 1 BLAS thread and a main process at 12 disagree in the last bits of every coefficient. Measured on a 2-outcome `tab_reg()`:

|                                                | parallel == serial? |
|------------------------------------------------|---------------------|
| main 12 threads, workers 1 (worker-only pin)   | **FALSE**           |
| main 1 thread, workers 1                       | TRUE                |
| *and*: a **serial** build at 12 vs at 1 thread | **FALSE**           |

That last row is the one that explains it, and it was true *before* any of this: `glm()` through a threaded BLAS already gave a slightly different answer at a different thread count, so "byte-identical" only ever held because both branches happened to use the same one. `local_blas_threads(1L)` now pins for the duration of a build — **in `tab_pmap()` for BOTH branches**, plus the one serial unit-loop that bypasses it (`reg_stage_specs()`) — and restores the user's setting on exit through a base `on.exit` in the caller's frame (`withr` is Suggests-only). All three rows above are now TRUE, which means a `tab_reg()` result no longer depends on how the machine's BLAS was built — strictly better than before, and it is what the test suite already assumed, since `tests/testthat/setup.R` pins BLAS there.

⚠ **This is the reason `tabxplor.parallel` could not have been turned on by default before now**, and the reason to be careful about turning it on at all: the failure was silent, produced correct output, and lived in the one axis the tests exercise least.

### 7.2 What parallelism is actually worth

Speedup against serial, pool warm, 21k rows, 2 column variables — rows are the number of TABLES, columns the number of workers:

*12 cores*

| tables | 2 workers | 3 workers | 4 workers | 6 workers | 8 workers |
|--------|-----------|-----------|-----------|-----------|-----------|
| 2      | 1.39x     | 1.43x     | 1.43x     | 1.54x     | 1.37x     |
| 3      | 1.29x     | 2.01x     | 1.78x     | 2.09x     | 1.78x     |
| 4      | 1.66x     | 1.58x     | 2.44x     | 2.64x     | 2.19x     |
| 6      | 1.68x     | 2.04x     | 2.23x     | 3.10x     | 2.93x     |
| 8      | 1.74x     | 2.17x     | 2.63x     | 2.80x     | 3.22x     |
| 12     | 1.73x     | 2.20x     | 2.81x     | 3.57x     | 3.08x     |
| 24     | 1.72x     | 2.32x     | 2.75x     | 3.66x     | 3.80x     |

Read the shape rather than any single cell:

- **it pays from 2 tables** — ×1.4 on 12 cores, ×1.55–1.59 on 2 and 4 cores — and reaches ×1.7 by 4 tables at only 2 workers;
- **4 workers is the knee**: ×2.4–2.8 from 4 tables up, and the whole rest of the machine buys ×3.7 at 6 workers and ×3.8 at 8. The 8-worker column is worth about **+38 % over 4 workers at 24 tables, and nothing at all below 8 tables**;
- **more workers than tables never helps** — the diagonal is visible in the top-left.

On the machines that matter for teaching:

*4 cores (the student all-in-one)*

| tables | 2 workers | 3 workers | 4 workers | 6 workers |
|--------|-----------|-----------|-----------|-----------|
| 2      | 1.59x     | 1.40x     | 1.40x     | 1.56x     |
| 3      | 1.32x     | 2.14x     | 2.09x     | 2.12x     |
| 4      | 1.74x     | 1.74x     | 2.29x     | 2.69x     |
| 6      | 1.74x     | 2.35x     | 2.39x     | 2.32x     |
| 8      | 1.74x     | 2.19x     | 2.87x     | 2.65x     |
| 12     | 1.72x     | 2.42x     | 2.84x     | 2.69x     |
| 24     | 1.72x     | 2.41x     | 2.82x     | 2.76x     |

*2 cores*

| tables | 2 workers | 3 workers | 4 workers |
|--------|-----------|-----------|-----------|
| 2      | 1.55x     | 1.57x     | 1.45x     |
| 3      | 1.32x     | 1.47x     | 1.57x     |
| 4      | 1.77x     | 1.56x     | 1.62x     |
| 6      | 1.76x     | 1.75x     | 1.66x     |
| 8      | 1.77x     | 1.73x     | 1.71x     |
| 12     | 1.74x     | 1.72x     | 1.76x     |
| 24     | 1.76x     | 1.79x     | 1.76x     |

**On 4 cores, 4 workers (×2.8) clearly beats 2 (×1.7)**, and 6 workers regresses. **On 2 cores everything above 2 workers is flat** (×1.75), with no penalty until well past 4. So the honest reading of your proposal — *4-core machine → 2 workers → ~1.5×* — is that the **gain is 1.72×, better than you guessed, but it leaves 40 % of the available win on the table** (4 workers would give 2.8×). That is a real trade: 4 workers on 4 cores saturates the machine, and a student's all-in-one is also running the jamovi UI.

### 7.3 The first call is the problem

Starting the pool blocks the session, and there is no way around it:

| workers | 12 cores | 4 cores | 2 cores |
|---------|----------|---------|---------|
| 2       | 1.253    | 0.870   | 0.870   |
| 3       | 1.381    | 1.363   | 1.201   |
| 4       | 1.459    | 1.460   | 1.954   |
| 6       | 1.716    | 1.858   | --      |
| 8       | 1.974    | --      | --      |

`mirai::daemons()` **returns only once the daemons have connected** — 1.16 s with a dispatcher, 0.78 s without. There is no fire-and-forget variant, so "start the engine at tabxplor start" cannot be made free; it can only be moved.

That makes the first parallel table *slower than serial*, always:

| 8 tables, 12 cores                                 | serial  | parallel W=4 |
|----------------------------------------------------|---------|--------------|
| pool already warm                                  | 0.962 s | 0.366 s      |
| **counting the spawn (first call of the session)** | 0.962 s | **1.83 s**   |

Break-even, at W=4: **~2.5 calls** for an 8-table build (saves 0.60 s each against a 1.46 s spawn), but **~17 calls** for a 2-table build (saves 0.09 s each). So for a student who makes a handful of small tables in a session, defaulting parallel ON is a net **loss**; for an analyst sweeping a survey, it is a clear win from the third table.

**Do not spawn the pool at `.onLoad()`.** Three reasons, in order: CRAN policy is explicit that a package should not start external processes when loaded; it would add ~1.3 s to every `library(tabxplor)` including in knitr, in `R CMD check` and inside jamovi; and each daemon costs memory whether or not a table is ever built.

| workers | data MB | pool idle | after a build |
|---------|---------|-----------|---------------|
| 2       | 21.3    | 638.7     | 712.2         |
| 4       | 21.3    | 903.5     | 1036.8        |
| 8       | 21.3    | 1434.1    | 1703.8        |

**~133 MB per idle worker**, plus its share of the shipped population. Eight workers is **1.6 GB of R** before any data — on an 8 GB all-in-one that is a quarter of the machine for something the user did not ask for.

### 7.4 The core-count rule `detectCores()` gives, and why it had to go

`tabxplor.parallel = TRUE` currently resolves to `min(max(1, detectCores(logical = FALSE) - 1), 8)`. Measured:

|                               | `parallel::detectCores(logical = FALSE)` | `parallelly::availableCores()` |
|-------------------------------|------------------------------------------|--------------------------------|
| this machine                  | 12                                       | 12                             |
| under `taskset -c 0,1`        | **12**                                   | **2**                          |
| under `_R_CHECK_LIMIT_CORES_` | **12**                                   | **2**                          |

**`detectCores()` does not see an affinity mask or a cgroup quota.** In a Docker container with `--cpus=2`, on an HPC job with 2 allocated cores, or on a CI runner, `TRUE` would spawn 8 workers onto 2 cores — which suite C measured as a **25 % loss**, and which is antisocial on a shared machine besides. R CMD check happens to be covered, because `tab_parallel_workers()` reads `_R_CHECK_LIMIT_CORES_` itself and caps at 2.

**`parallelly::availableCores()` gets all of them right** (cgroups v1/v2, `taskset`, `_R_CHECK_LIMIT_CORES_`, `options(mc.cores)`, and the SLURM / PBS / SGE / LSF allocation variables), it is a zero-dependency package, and it is the futureverse's canonical answer to exactly this question. ⚠ mirai could not supply it instead: mirai imports `nanonext` alone and exports no core count. It is now a Suggest, inside the cascade of §7.5.

### 7.5 What was implemented

Parallelism **stays opt-in** — `tabxplor.parallel` is still `FALSE` by default, for the reason in §7.3: the pool spawn blocks, so a default-on first table would always be slower than serial. What changed is that the two things that were *wrong* are now right, and that saying yes gives a sensible answer.

**1. A worker pins its BLAS** (§7.1) — the 70× fix.

**2. The core count is what this R may actually use, not what the box has.** `tab_available_cores()` is a cascade, one rung per case `detectCores()` gets wrong:

| rung                           | what it catches                                                  |
|--------------------------------|------------------------------------------------------------------|
| `_R_CHECK_LIMIT_CORES_`        | CRAN's 2-core rule for examples / tests / vignettes              |
| `options(mc.cores)`            | base R's own convention — a user who set it has already answered |
| `parallelly::availableCores()` | cgroups v1/v2, affinity masks, SLURM / PBS / SGE / LSF           |
| `nproc`                        | affinity-aware on Unix, the fallback when `parallelly` is absent |
| `detectCores()`                | last resort, and on Windows usually right anyway                 |

⚠ `parallelly` had to become a **Suggest**: mirai imports `nanonext` alone and exports no core count of its own, so gating on mirai would not have bought this. It is a zero-dependency package and the fallback rungs mean nothing breaks without it. Only the machine rungs are memoised — the two option rungs are re-read every call, so a user can still change their mind mid-session.

**3. `"auto"` (and `TRUE`) now mean half the usable cores, floored at 2, capped at 4:**

```r
tab_auto_workers <- function(avail) if (avail <= 1L) 1L else min(4L, max(2L, avail %/% 2L))
```

| cores this R may use | 1          | 2 | 3 | 4     | 6 | 8 | 12 | 64 |
|----------------------|------------|---|---|-------|---|---|----|----|
| workers              | 1 (serial) | 2 | 2 | **2** | 3 | 4 | 4  | 4  |

Each clause is a measured decision: the **cap at 4** because 8 workers is +38 % over 4 at 24 tables and nothing below 8 tables, for four more processes and ~530 MB; **half the cores** because a build must not saturate the machine it runs on (it costs ×2.8 → ×1.7 on a 4-core box and buys a usable UI); the **floor of 2** because `%/% 2` gives 1 on a dual-core machine, which is serial, and 2 cores is exactly where 2 workers give ×1.75 with no penalty; and **1 core stays serial**, since 2 workers there would only oversubscribe.

Verified end to end: `taskset -c 0,1` → 2 cores → 2 workers; `taskset -c 0` → 1 → serial; `_R_CHECK_LIMIT_CORES_` → 2 workers even when 8 were asked for; `options(mc.cores = 2)` → 2; `FALSE` / `NULL` / `"no"` / `0` → serial; `3` and `"3"` (jamovi passes strings) → 3; a jmvtab cache present → serial regardless.

**How to use it.** One line at the top of a script, which is also what to tell students:

```r
options(tabxplor.parallel = "auto")   # or a number: 2, 3, 4 …
```

The first table of the session pays the pool spawn (0.9–2.0 s) and is slower than serial; from roughly the third it is ahead, and a 15-table exploration then runs ~2.4–2.8× faster. `tab_parallel_stop()` releases the workers.

**Still open** (§10): a `tab_parallel_start()` twin, so the spawn can be paid at a moment the user chooses rather than on their first table.

### 7.6 Caveats, all of them

- **Version skew.** Daemons load the *installed* tabxplor. Two libraries, or an upgrade mid-session, and a worker errors on a function it does not have — which is exactly how the BLAS investigation started (`could not find function "tab_cnd_strip"`). With `FALSE` as the default this never bites; make it a default and it bites some users.
- **The first call is slower. Always.** §7.3.
- **~133 MB per worker**, plus a copy of the prepared population per worker per call (§7.3, and the shipping cost is measured in §7.7).
- **Declared fidelity losses** (already documented in `tab-parallel.R`): a relayed error has no backtrace, worker messages are replayed *after* collection rather than interleaved, and the replay stops at the first failing unit.
- **Constrained machines** (§7.4) — ~~the current rule over-spawns~~ fixed in §7.5; `availableCores()` now answers, with `nproc` and `detectCores()` behind it.
- **User options always win.** `options(tabxplor.parallel = FALSE)` beats any auto rule, and `options(mc.cores)` caps it — the cascade reads `mc.cores` on its own rung, above the machine, so it also wins where `parallelly` is absent.
- **CRAN.** `_R_CHECK_LIMIT_CORES_` is the cascade's first rung and caps at 2 whatever was asked for (verified). Nothing in examples, tests or vignettes spawns a pool, because the default is still `FALSE`; `test-parallel-parity.R` is `skip_on_cran()`.
- **Orphaned daemons.** A pool survives a crashed script until its socket dies. `tab_parallel_stop()` exists and `.onUnload()` cleans up, but a default-on pool means users who never heard of either.

### 7.7 The per-call shipping cost

The population is shipped to every worker on **every** parallel build, not once per session:

| rows    | data MB | serial | 4 workers | speedup |
|---------|---------|--------|-----------|---------|
| 21483   | 2.2     | 0.534  | 0.220     | 2.43x   |
| 214830  | 21.3    | 0.582  | 0.247     | 2.36x   |
| 1074150 | 106.6   | 0.667  | 0.378     | 1.76x   |

Parallelism still wins at a million rows (×1.8 at 4 workers), but the margin narrows as the frame grows — ×2.4 at 21k, ×2.4 at 215k, ×1.8 at 1.07M — because the shipping is per call and per worker. On 2 cores the same three read ×1.53, ×1.52 and ×1.11: at a million rows on two cores, shipping has eaten almost all of it. This is the second argument for a modest worker count: shipping cost scales with W, the gain does not.

---

## 8. jamovi: should the panels go parallel?

### 8.1 What actually becomes a parallel unit

This is the crux, and the two producers differ:

| producer                  | maps over               | `tab_vars`                | several outcomes     |
|---------------------------|-------------------------|---------------------------|----------------------|
| `tab()` / `jmvtab`        | **`row_vars` only**     | bigger units, no new ones | (not applicable)     |
| `tab_reg()` / `jmvtabreg` | outcomes, groups, specs | one unit per group        | one unit per outcome |

Measured on `jmvtab` shapes, 12 cores, W = 3:

| shape                           | serial | 3 workers | speedup |
|---------------------------------|--------|-----------|---------|
| 1 row_var, 3 col_vars           | 0.095  | 0.091     | 1.04x   |
| 1 row_var, 3 col_vars, tab_vars | 0.182  | 0.181     | 1.01x   |
| 3 row_vars, 3 col_vars          | 0.313  | 0.166     | 1.89x   |
| 3 row_vars, tab_vars            | 0.465  | 0.242     | 1.92x   |

So for the Crosstables panel your instinct is **half right**: several `tab_vars` do *not* parallelise (they make one unit twice as big — 0.095 → 0.182 s), while several `row_vars` give ×1.9. And a single row variable is always serial, because `parallel_min = 2`.

For the Regressions panel your instinct is **exactly right** — and it is the better case of the two:

| case                      | serial | W=2   | W=2 gain | W=3   | W=3 gain |
|---------------------------|--------|-------|----------|-------|----------|
| 1 outcome [21k]           | 0.282  | 0.282 | 1.00x    | 0.286 | 0.99x    |
| 3 outcomes [21k]          | 0.997  | 0.612 | 1.63x    | 0.376 | 2.65x    |
| 3 outcomes [215k]         | 9.030  | 5.198 | 1.74x    | 3.601 | 2.51x    |
| tab_vars: 3 groups [21k]  | 0.317  | 0.321 | 0.99x    | 0.325 | 0.98x    |
| tab_vars: 3 groups [215k] | 1.806  | 1.813 | 1.00x    | 1.870 | 0.97x    |
| 4 nested models [21k]     | 0.548  | 0.367 | 1.49x    | 0.334 | 1.64x    |

**Several outcomes is the best axis in the whole package** (×2.65 at 3 workers, and it holds at survey `n`: 9.03 s → 3.60 s). The code says so too — "the cleanest of the three parallel axes, each unit returning a FINISHED table with no cross-unit step at all". ⚠ **The `tab_vars` groups axis measures no gain at all**, at either size, despite dispatching — its own comment already hedges ("clears 2× only when the groups are EVEN and the frame is survey-size"); at 215k with three even groups it still does not. Worth a look, but not a reason to hold anything up.

### 8.2 jamovi's process model — the fact that settles it

Read out of the installed 2.7.36 flatpak rather than inferred. `jamovi/server/session.py:109` builds `Scheduler(1, 3, …)` — `n_init_slots = 1` plus `n_run_slots = 3` — and `EngineManager` creates one `jamovi-engine` process per slot at session start. So:

- **jamovi runs exactly 4 engine processes per session**, each a separate OS process embedding R, created once and long-lived;
- they are **shared by every analysis**, of every module — jamovi's own developer puts it as *"jamovi will assign separate analyses to separate cores, but analyses themselves typically don't use multiple cores"*;
- and `enginemanager.py::_run_analysis` dispatches a request to the slot where **that same analysis is currently running**, otherwise **to the first free slot**. There is no persistent affinity between an analysis and an engine.

That answers the three questions directly, and two of the answers are the ones we did not want:

1. **Is the same R session there across live passes?** There *is* a persistent session — but **not the same one**. Each pass goes wherever a slot is free, so a pool spawned in engine 1 is invisible to engines 2–4, and the next pass of the same analysis may pay the spawn again. In the steady state the pool ends up spawned **in all four engines**.
2. **Does each analysis get its own R session?** No — they share those four. Which is worse rather than better: a pool tabxplor spawns **persists in that engine and is inherited by every other module's analysis** that later lands on it. That is a side effect on software we do not own.
3. **Can the pool be started when the analysis is created, or stopped when comparison mode is left?** No, for the same reason. There is no hook that runs in a *known* engine, and the "leaving" pass may execute in a different engine from the one holding the pool — `tab_parallel_stop()` there is a no-op on an empty profile while the real pool is orphaned somewhere else. **The lifecycle cannot be managed from inside the module.**

And the memory: 4 engines × W workers × ~133 MB. At W = 2 that is **8 daemons ≈ 1.06 GB**; at W = 4, **16 daemons ≈ 2.1 GB** — on top of Electron and the four engines, on the 8 GB machine this is all for.

### 8.3 The arithmetic kills it before any of that

Even setting the process model aside, the numbers do not work for the case you want to speed up:

| 4 predictor lists, 21k rows                              | seconds   |
|----------------------------------------------------------|-----------|
| serial, no cache                                         | 0.608     |
| serial, fit cache warm                                   | 0.421     |
| parallel W=2 (measured on the equivalent `tab_reg` call) | 0.367     |
| **cold pool spawn, W=2**                                 | **0.870** |

**The spawn costs more than the entire build.** Spawning per run to save 0.18 s is a 5× net loss; keeping the pool alive would need ~7 comparison runs *in the same engine* to break even — and with four engines each needing its own pool, ~28 runs in one session. Nobody runs 28 model comparisons in one jamovi session.

You are right that comparison mode is no longer the slow thing it was. That is the other half of the answer: **the case that motivated parallelising is already fast enough.** 0.6 s here, ~0.9 s projected on a student all-in-one — for an action behind a *Run comparison* button, where the user has already accepted a wait.

### 8.4 So: do not parallelise the jamovi panels

That is my recommendation, and it is not a close call. Four shared engine processes with no affinity, no lifecycle hook, a pool inherited by other modules' analyses, 1–2 GB of daemons on an 8 GB machine — against a build that takes 0.6 s. If any of those were the only problem it would be worth engineering around; together they are a wall.

What I would do instead, in order of value:

1. **Nothing, for now.** Comparison mode is 0.6 s and the live UI is 0.33 s.
2. If a future case genuinely needs it, the only safe shape is a pool created **and destroyed inside one build** (`tab_parallel_stop()` in the same function's `on.exit()`), so nothing outlives the call — and then the spawn is paid every time, which the table above says is a loss. Revisit only if a build ever costs more than ~5 s, where a 0.9 s spawn is amortised.
3. Tell the *script* users about `options(tabxplor.parallel = "auto")`. That is where the ×2–3 lives, and it costs jamovi nothing.

### 8.5 The caches are not one object, and should not share a fate

⚠ First, a correction to a reading of §8.1: the "tier-1 aggregate is 0.5 % of the build" figure is the cost of a cache **hit**, not the value of the cache. What the cache is worth is what it *saves*, measured here by re-running each interaction with the store withheld:

|                            | re-apply (same options)    | a changed option       | store        |
|----------------------------|----------------------------|------------------------|--------------|
| **Crosstabs**, 1 row_var   | 0.006 vs 0.107 s → **×18** | 0.107 vs 0.109 → ×1.02 | 0.08 MB      |
| **Crosstabs**, 3 row_vars  | 0.010 vs 0.340 s → **×34** | 0.340 vs 0.357 → ×1.05 | 0.19 MB      |
| **Reg**, 1 model           | 0.095 vs 0.196 s → ×2.3    | 0.282 vs 0.314 → ×1.1  | **6.28 MB**  |
| **Reg**, 4 predictor lists | 0.421 vs 0.608 s → ×1.4    | —                      | **15.89 MB** |

Two different objects with two different verdicts:

- **Keep the Crosstables cache exactly as it is.** 0.19 MB buys ×18–34 on a re-applied change — which is most of what clicking around a panel does — and the rebuild path it does not help costs 2–5 %, i.e. nothing lost by its presence. This is a very good trade.
- **The Regressions fit cache is the one to cut.** It buys ×2.3 at best and ×1.1 on the changes a user actually makes, and it pays for that with **6–16 MB serialised into jamovi's `$state` on every UI round-trip** — which is precisely the freeze `jmvtabreg-cache.R`'s own comment describes at four models. Comparison mode already sets `use_cache = FALSE`; these numbers say why that was right, and that the same reasoning extends further.

So your instinct is right for the regressions and wrong for the crosstabs, and the split is not a matter of taste: **it is the ratio of what the store holds (KB of aggregates vs MB of raw fits) to what it saves.**

Concretely, and this wants its own phase rather than a change here: keep the regression cache **only for the cheap re-apply path** — the reference-invariant digest the code already knows how to build ("a KB digest instead of a 10 MB fit") — and stop persisting raw fits at all. If that turns out to be most of the machinery, removing the regression cache outright is defensible: at ×1.1 on a changed option it is close to free to lose, and 16 MB per round-trip is a real cost paid on every click.

> **RESOLVED — Phase 22j.** The cache was kept and its *payload* replaced: one tier of distilled
> `tabxplor_fitdigest` records, keyed on the model alone. Measured after: **3.3 KB per record**,
> **29.3 KB** for a binomial store and **92.4 KB** for a multinomial one, against the 6.28 / 15.89 MB
> above. Because the estimand left the key, the ×1.1 row became a hit: a `measure` / `effect` change
> on a **multinomial is 14.35 s → 1.90 s (×7.6)**. A reference change is now an honest refit
> (0.295 s vs 0.203 s served), which is what paid for deleting the 13-clause reparametrisation gate.
> jamovi also stopped silently losing its model checks, its global tests and its `adjustment` gap SE
> on that path. See CLAUDE.md > Phase 22j.

## 9. What this phase changed

**Fixes**

- `lvl_check_reserved()` in `R/row-model.R` (§6.5): a numeric column was coerced to character and hashed on every build — up to **×67** on numeric tables at scale, no behaviour change.
- The missing **BLAS pin**, `R/tab-parallel.R` (§7.1): a parallel `tab_reg()` on a threaded BLAS was **70× slower than serial**, silently and with correct output. The fix has two halves — the worker pin (speed) and `local_blas_threads(1L)` on both branches of `tab_pmap()` plus `reg_stage_specs()`'s serial loop (byte-identity, which the worker-only pin broke). A `tab_reg()` result is now independent of the machine's BLAS thread count.

**The parallel default (§7.5)** — still opt-in, but now with a sane answer when asked

- `tab_available_cores()`: the affinity-aware cascade (`_R_CHECK_LIMIT_CORES_` → `options(mc.cores)` → `parallelly::availableCores()` → `nproc` → `detectCores()`), replacing a `detectCores()` that returned 12 on two allocated cores.
- `tab_auto_workers()`: `"auto"` / `TRUE` = **half the usable cores, floored at 2, capped at 4**; one core stays serial.
- `parallelly` added to **Suggests** (mirai imports `nanonext` alone and has no core count of its own).
- `?tabxplor-options` rewritten for `tabxplor.parallel`: what `"auto"` resolves to, which global settings it honours, and why it stays opt-in.

**Tests** — `test-options.R` gains the worker-count rule (pure arithmetic, so it runs everywhere, mirai or not) and the option boundary; `test-parallel-parity.R` gains the **BLAS-pin regression guard**, which had to be written as `mirai_map()` rather than `everywhere()` because the latter runs for side effects and returns nothing. ⚠ Byte-identity was never the missing check — a thread-thrashed worker still delivers it, which is why 26/26 passed throughout the 70× regression.

**Harness** — four reusable scripts under `dev/benchmarks/`: `phase22h_perf_review.R` (the defaults grid, 146 cases × 3 machine profiles + a 1.3.1 engine), `phase22h_threads.R` (data.table threads, one condition per process), `phase22h_parallel.R` (spawn, the workers × tables grid, shipping, pool memory, which shape parallelises, the three `tab_reg()` axes, jamovi's warm-interaction profile), and `phase22h_report_tables.R` (emits every table in this file). Runs are in `dev/benchmarks/results_2.0.0/phase22h_*.csv`.

Full suite: **FAIL 1 | PASS 9817**. The one failure is `test-jamovi-vocabulary.R:288` ("STALE generated block") and is **not from this phase** — it is the maintainer's own in-flight Phase 22g-ii hand-edit of the generated block in `jamovi/js/jmvtabreg.js`, which no longer matches what `REG_FAMILIES` / `reg_link_ui_labels()` emit. It needs the R fact tables changed or `dev/generate_jamovi_js.R` re-run; touching it here would undo those edits.

## 10. What is left to decide

**Defaults**

| # | question                                                                                      | where |
|---|-----------------------------------------------------------------------------------------------|-------|
| 1 | Should `design_effect` default to `TRUE` on weighted tables, now that it is known to be free? | §6.1  |
| 2 | Should `dispersion` / `influence` leave the default footer set for `multinomial` only?        | §6.2  |
| 3 | Is the ordinal Brant refit still the right default at survey `n`?                             | §6.6  |

**Parallel**

| # | question                                                                                     | where |
|---|----------------------------------------------------------------------------------------------|-------|
| 4 | `tab_parallel_start()`, so the spawn is paid when the user chooses, not on their first table | §7.5  |
| 5 | Raise `parallel_min` from 2 to 4? (2 units saves 0.09 s, 4 units 0.29 s)                     | §7.3  |
| 6 | **Do not parallelise the jamovi panels** — recommended against, on four grounds              | §8.4  |

**The jamovi caches**

| # | question                                                                                               | where |
|---|--------------------------------------------------------------------------------------------------------|-------|
| 7 | Keep the **Crosstables** cache untouched — 0.19 MB buys ×18–34 on a re-apply                           | §8.5  |
| 8 | ~~Cut the **Regressions** fit cache to the KB digest~~ — **DONE, Phase 22j**: 6–16 MB → 29–92 KB | §8.5  |

**Documentation**

| #  | what                                                                                        | where |
|----|---------------------------------------------------------------------------------------------|-------|
| 9  | One sentence in the regression vignette: collapse a multinomial outcome before modelling it | §6.2  |
| 10 | Teach `options(tabxplor.parallel = "auto")` in the intro vignette — one line, ×2.4–2.8      | §7.5  |
| 11 | Size-gated `setDTthreads(1L)` in `tab_build()` — a trade, not a win; a later phase          | §6.3  |

Nothing here blocks the release. **(8) had the user-visible payoff, and Phase 22j took it** — it is the freeze `jmvtabreg-cache.R`'s own comment describes — and (1) and (2) are the two real defaults questions the measurements turned up.
