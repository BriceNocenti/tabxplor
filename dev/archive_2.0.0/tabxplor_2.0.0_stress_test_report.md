# tabxplor 2.0.0 — stress-test report

Date: 2026-07-22
Tested commit: `033f94e` ("Phase 18k — last new features 2, labelled-data"), clean worktree.
Method: adversarial, script-driven testing against the live source (`devtools::load_all`) across six
areas — core `tab()`/`tab_many()`, leaves/references/counts, the `tabxplor_fmt` type system + dplyr,
the colour engine + legend, the export stack, `tab_reg()`, and the statistical aggregate core. Roughly
**450 individual cases** were run. Every finding below was re-verified by hand against this commit;
agent-harness artefacts have been separated out (§6) so they are not chased.

---

## 1. Executive summary

The package is in strong shape. The **statistical core is essentially flawless** — every CI estimator
(Wilson, Newcombe, Katz log-RR, Woolf log-OR, Welch/Student, mean-ratio) and every test (χ², Welch/
classic ANOVA-F) matches a hand-rolled reference and `DescTools`/`stats` to machine precision, degenerate
inputs return clean `NA` (never `NaN`/`Inf`), and `tab_reg()` reproduces `lm`/`glm`/`multinom`/`polr` +
`broom` across all families to `maxrel = 0`. The colour engine's break selection is internally
consistent and golden-locked. dplyr class/attribute preservation holds across ~18 verbs. Excel numeric
round-trip parity holds.

The defects that remain are concentrated in **input-robustness and error-message quality at the edges**,
not in the numbers. The one genuine crash worth fixing before release is the **NA-factor-level print
crash** (§2.1). A handful of reasonable inputs (logical/Date `col_var`) produce obscure internal errors
rather than informative ones. There is one **silent-correctness trap** in raw `tabxplor_fmt` arithmetic
(§3.1) that is niche but real. Everything else is polish.

| Severity | Count | Items |
|----------|-------|-------|
| Major (crash on reasonable input) | 1 | NA-factor-level print/export crash |
| Design / statistical (silent or surprising, defensible) | 4 | fmt-arithmetic field staleness; weighted-CI design effect; poisson overdispersion divergence; `summarise` attribute loss |
| Minor (obscure errors, API gaps, leaked warnings, cosmetics) | 8 | see §4 |

---

## 2. Confirmed bugs

### 2.1 MAJOR — a factor with a real `NA` *level* crashes print/format/every export

A table built from a factor that carries `NA` as an actual level (not merely `NA` values) **builds
successfully** but then **throws on `print()`, `format()`, and consequently every exporter**.

```r
library(tabxplor); library(dplyr)
d <- tibble(r = factor(c("a","b",NA), exclude = NULL), c = factor(c("x","y","x")))
t <- tab(d, r, c)          # builds fine
format(t)                  # Error: NAs are not allowed in subscripted assignments
print(t)                   # same
tab_md(t); tab_kable(t)    # same (all go through format)
```

- **Observed**: `Error in out[ok & tot] <- ... : NAs are not allowed in subscripted assignments`.
- **Expected**: either drop/relabel the `NA` level at build (as `na = "keep"` does for `NA` *values*,
  which works fine — see §5), or render it. A validly-built table must be printable.
- **Root cause**: `pillar_shaft.tabxplor_fmt()` at `R/fmt_class.R:2486` —
  `out[ok & tot] <- cli::style_bold(out[ok & tot])`. When a row label is `NA`, the total-row detection
  mask `tot` contains `NA`, so `ok & tot` is `NA` and the subscripted assignment aborts.
- **Fix direction**: coerce the total-row mask with `tot & !is.na(tot)` (or `%in% TRUE`) before
  indexing; or normalise an `NA` factor level to a visible label (e.g. `"NA"`/the `na` text) during
  `tab_prepare()`. Note `exclude = NULL` factors are the common way `haven`/imported data arrives, so
  this is reachable from real data, not only synthetic.

### 2.2 MINOR/MAJOR — logical and Date `col_var` produce an obscure internal error

```r
tab(tibble(r = factor(rep(c("a","b"),50)), lg = rep(c(TRUE,FALSE),50)), r, lg)
# Error in UseMethod(): no applicable method for 'n_groups' applied to an object of class "NULL"
tab(tibble(r = factor(rep(c("a","b"),50)), dt = rep(as.Date("2020-01-01")+0:1,50)), r, dt)
# same obscure error
```

- **Observed**: a cryptic `n_groups`/`NULL` error deep in the pipeline.
- **Expected**: an informative "`col_var` must be a factor, character or numeric — got `logical`/`Date`"
  message, **or** support them (a logical is a perfectly natural 2-level cross-tab variable, and
  `tab_plain()` called directly *does* accept a logical `col_var` — see §6 — so `tab()` is
  inconsistent with its own leaf).
- **Impact**: low frequency, but the error gives the user no idea what to fix.

---

## 3. Design / statistical concerns (defensible, but worth a decision or a doc line)

### 3.1 DESIGN — raw `tabxplor_fmt` arithmetic updates only the *displayed* field, leaving the others stale

Scalar arithmetic on an fmt vector scales the field currently being displayed and silently leaves the
sibling count fields untouched, producing an internally inconsistent record:

```r
f <- fmt(n = c(10,20), wn = c(11,22), display = "n")
g <- f * 2
vctrs::field(g, "n")    # 20 40   (scaled)
vctrs::field(g, "wn")   # 11 22   (STALE — not 22 44)
```

For a `pct`-display cell, `* 2` scales `pct` but leaves both `n` and `wn` untouched. Because `wn`
(weighted n) is the statistically meaningful count on weighted tables, `mutate(new = colA + colB)` over
weighted count columns yields a cell whose visible value is right but whose `wn` field is wrong — and
CLAUDE.md explicitly states users read/`mutate` fmt fields, so this is reachable.

- **Severity**: medium-as-a-trap, low-as-a-frequency. Not a numbers-in-the-table bug (the *displayed*
  value is correct); a hidden-field-consistency bug for downstream `$`/`field()` consumers.
- **Options**: (a) document that fmt arithmetic operates on the display field only and does not maintain
  the full record; (b) propagate the same scaling to `n`/`wn`/`pct` together; (c) `NA` the fields that
  are no longer coherent after an arithmetic op. Any is fine — the current silent partial update is the
  worst of the three.
- Note `sum()`/`+` **do** correctly carry the colour/`color_signif`/`model_family`/`role` attributes
  (verified), so only the numeric fields are affected.

### 3.2 DESIGN — default weighted CIs carry no design-effect correction

Verified: a weighted cell CI is exactly `Wilson(weighted p, unweighted n = tot_n)`. This treats the
weighted proportion as if it came from `tot_n` independent Bernoulli trials, so under unequal weights
the interval is **too narrow** (no design effect). This is a *documented, deliberate* choice — `tot_n`
is unweighted by contract and the Kish `n_eff` widening is an opt-in that works correctly (verified:
`n_eff = (Σw)²/Σw²`, CIs widen, extreme weights widen a lot). Flagging only so the decision is conscious
at release: the default weighted CI understates uncertainty, and the footer names it plainly as
"wilson" without signalling the design-naivety. Consider a one-line note in `?tab` near the weighting
paragraph. No code change required.

### 3.3 DESIGN (positive) — `tab_reg(family = "poisson")` auto-switches to overdispersion-scaled SEs

Verified and **transparent**: with an overdispersed count outcome (Pearson dispersion 2.04), a
`family = "poisson"` fit returns CIs/p-values **identical to `family = "quasipoisson"`** (SEs scaled by
`√dispersion`), and it **emits a warning saying exactly that**. At equidispersion (≈1.0) it matches a
standard `glm(poisson)` z-CI. So `tab_reg` poisson results diverge from `broom::tidy(glm(poisson))`
*only* when overdispersion is present, by design, with notice. This is good, robust behaviour — the only
action item is to make sure the R-side `?tab_reg` documents it (the jamovi side already intends it per
Last-Phase-h notes), so a user comparing to a hand-fit `glm` isn't surprised. Not a bug.

### 3.4 DESIGN — `summarise()` silently downgrades a tab to `tbl_df` and drops `meta`/`subtext`/`test`

Of ~18 dplyr verbs tested, only `summarise()` loses the class and all table attributes (every other verb
— `filter`/`arrange`/`select`/`rename`/`relocate`/`mutate`/`transmute`/`distinct`/`slice*`/`group_by`/
`rowwise`/`pull`/`bind_rows` — preserves them). This is defensible (summarise destroys the row/column
structure a tab depends on) and matches the CLAUDE.md "no S3 method ⇒ downgrade" note, but the loss is
silent. Acceptable to leave as-is; documenting "aggregating verbs return a plain tibble" would remove the
surprise.

---

## 4. Minor issues and rough edges

1. **`tab()` has no `totcol` argument.** `tab(df, r, c, totcol = "each")` → `unused argument (totcol=)`.
   `totcol` lives only on `tab_many()`. A user wanting a per-`col_var` total column from the friendly
   wrapper hits an ugly stock R error. Either forward it or document that total-columns beyond `tot=`
   need `tab_many()`.
2. **Per-`col_var` `pct` list-mode is rejected**, contradicting the "list-mode arguments" framing:
   `tab_many(gss,"race",c("marital","relig"), pct = list("row","col"))` →
   *"pct can't be recycled to the lengths of row_vars and col_vars"*. A length-matching character vector
   `pct = c("row","col")` is accepted. Worth aligning the settings-frame recycling (17e) so `pct`
   accepts the same per-pair forms the docs imply, or trimming the docs.
3. **Bad named-`ref` name → cli internal error.** A `ref = c(badname = "x")` on `tab_many` surfaces
   *"Multiple quantities for pluralization"* (a raw `cli` pluralisation failure) instead of a message
   about the unknown variable name.
4. **`tab_md()`/exporters on a non-table (`NULL`) → cryptic** *"attempt to set an attribute on NULL"*
   (`R/tab_classes.R:222`, via `set_meta_field`). Not reachable from normal use, but an `is_tab()` guard
   at the export entry points would turn it into a clear message.
5. **`row_var` also listed in `tab_vars` → obscure `tidyselect` error** ("Element `marital` doesn't
   exist") rather than "a variable cannot be both a row and a tab variable" (the weight-collision case
   *does* get a clean message — mirror it).
6. **All-zero / all-`NA`-weight tables → generic** *"data is of length 0 (possibly after filter or
   na = 'drop_all')"*. Correct outcome (nothing to tabulate) but the message never mentions weights;
   a user who passed `wt` with all zeros won't connect it.
7. **Leaked base-R warning on an all-`NA` numeric column**: `tab(..., <all-NA numeric>)` emits
   *"no non-missing arguments to max; returning -Inf"* from base R instead of a handled message.
8. **Cosmetic**: `median(<fmt>)` errors "not implemented" while `sort`/`unique`/`c` work; an `Inf`
   proportion renders as `"Inf%"`; empty-`tidyselect` selections (`col_vars = <nothing matched>`) throw
   `length(pct) >= 1 is not TRUE` / `subscript out of bounds` rather than "no columns selected".

None of these are release-blockers; they are the difference between an obscure stack trace and a
one-line "here's what's wrong".

---

## 5. Verified-clean areas (coverage map)

These were attacked specifically and held up — recording them so effort isn't re-spent:

- **CI engine** (`R/tab-agg.R`): Wilson, Wald, Newcombe (+p/CI dual), Katz log-RR, Woolf log-OR,
  Welch/Student mean-diff, mean-ratio (robust/poisson/quasipoisson) — all match manual formulas and
  `DescTools` to ~1e-12. Zero-cell OR/RR → `NA` with **no silent 0.5 continuity fudge**. `df ≤ 0`,
  `se = 0`, `n = 1`, `p ∈ {0,1}` all → clean `NA`, never `NaN`/`Inf`. A full degenerate-table sweep of
  all 18 fmt fields found **no `NaN`/`Inf` leakage**.
- **χ² / ANOVA**: `statistic`/`df`/`p` match `chisq.test(correct=FALSE)` and `oneway.test`; 2×2 Yates
  applied and matches; contributions sum to X²; **empty factor levels are dropped so `df` stays correct**
  (tab() is *more* robust here than a raw `chisq.test`, which returns `NaN` on the zero-margin);
  single-row/col tables → `NA` gracefully; per-subtable and weighted (design-naive on unweighted counts,
  as documented) all correct.
- **`tab_reg()` statistical parity**: gaussian β, binomial OR, poisson IRR, quasipoisson, multinomial
  (per-category OR), ordinal (cumulative OR), `multiplier`, per-variable `reference=` releveling, and the
  full **empirical** companion (crude % / crude OR vs hand-computed 2×2, Woolf CI, two-proportion Wald
  risk-diff CI, gaussian means/diff, poisson rates/IRR, mixed-family dependents with correct per-column
  `family`/`role`) — all `maxrel = 0` vs `lm`/`glm`/`multinom`/`polr` + `broom`. `var(Y)` effect-size
  field correct.
- **Colour engine**: break-boundary selection is a single strict convention (`at-break → lower band`)
  applied uniformly across `pct_diff`/`pct_ratio`/`odds_ratio`/`contrib`/`mean_ratio`, mirror-symmetric,
  golden-locked; `set_color_breaks` validation is thorough and informative (rejects non-increasing,
  duplicate, zero-neutral, >4-per-side, unknown-scale, mid-vector `NA` on two-sided); significance
  gating (`grey_non_signif`/`guaranteed_effect`) colours iff significant; the honest p-value cell colours
  a non-significant test row as designed; two-channel and per-type (`pct=`/`mean=`) specs work; the
  deprecated colour strings (`diff_ci`/`after_ci`/`ci`) warn and decode correctly.
- **Reference / OR system**: `ref` = tot/first/integer/regex/named-vector, `comp = "tab"/"all"`,
  `ref` reinterpreted as a column under `pct="col"` (with the collapse message), `ref2`; OR values and
  Woolf CIs match hand-computed odds ratios. Out-of-range/no-match `ref` → clear warning + graceful
  degrade.
- **Excel round-trip**: written numeric values equal `get_num()` for simple/mean/grouped/counts/
  transpose tables; CI/OR cells are text by design (correctly, since a "32% [30;34]" cell is not a single
  number). Transpose writes without error.
- **`tabxplor_fmt` vctrs contracts**: `c()`/`vec_cast`/`vec_ptype2` (symmetric attr reconciliation),
  `sort`/`unique`/`vec_order`/comparisons, `[`/`[[`/`[<-`/`length<-`/`rep`, `if_else`/`case_when`, and
  arithmetic attribute carry — all sound; bad combinations (`c(fmt,"chr")`, out-of-range subscript) give
  informative vctrs errors.
- **dplyr class/attribute preservation**: 17 of 18 verbs preserve class + `meta`+`subtext`+`test`
  (exception `summarise`, §3.4).
- **Argument grid**: the full `pct × tot` matrix (20 combinations) builds; `ci`/`conf_level`/`chi2`
  combinations; `na = keep/drop`; `output_list`; `spread_vars`/`tab_spread`; per-`row_var` named `ref`;
  structural-zero cells with χ²/CI on — all produce sane numbers (row%/counts/totals reconcile).

---

## 6. Dismissed leads (agent-harness artefacts — do **not** chase)

During the sweep several apparent failures traced to the test harness, not the package:

- **"md-parity FAIL — values missing"**: the harness's markdown column-splitter mis-handled the
  multi-space cell padding; the values were present and correct in the emitted markdown.
- **"chi2 parity FAIL / per-subtable NaN"**: the harness compared against `chisq.test` *without* dropping
  empty factor levels, so the reference side returned `NaN`/wrong `df`; `tab()` was correct.
- **"weighted cell CI FAIL"**: re-verified equal to `Wilson(weighted p, tot_n)` — matches (§3.2).
- **contrib/`mean_ratio`/refrow-gate boundary "FAIL"s**: the harness assumed an *inclusive* at-break
  convention for those measures; the engine uses the *strict* convention uniformly (the `got` values are
  internally consistent with `pct_diff`/`odds_ratio`, which the same harness passed).
- **"color='OR' without OR silently colours garbage"**: `color="OR"` legitimately triggers OR
  computation; the `or` field is populated with real odds ratios, not stale values.
- **`OR=`/`or=` "unused argument" errors**: API confusion in the harness — the argument is `OR`
  (values `"no"/"OR"/"OR_pct"/…`) and the second reference is `ref2`; both work.
- **`tab_reg` "poisson CI FAIL vs broom"**: intended overdispersion scaling, with a warning (§3.3).
- The `totcol` "unused argument" errors in the core sweep were the agent passing `totcol` to `tab()`
  (which lacks it) — the underlying observation is real and kept as §4.1, but the *count* of failures
  there was inflated by the wrong function.

---

## 7. Recommended actions before release

1. **Fix §2.1** (NA-factor-level print crash) — small, and it is reachable from imported data.
2. **Turn §2.2 and the §4 obscure errors into informative messages** — mechanical, high user-experience
   payoff, matches the 2.0.0 "informative validation" standard already present elsewhere.
3. **Decide §3.1** (fmt-arithmetic staleness): document, propagate, or `NA`-out.
4. **Add one doc line each for §3.2 (weighted CI design effect) and §3.3 (poisson overdispersion)** so
   the two deliberate statistical choices are visible to R users, not only jamovi.
5. Everything in §5 is solid; no action.

Repro scripts for every finding are under
`…/scratchpad/{core,leaves,fmt,color,export,reg,stats,verify}/` (the `verify/V1–V3.R` scripts isolate
the confirmed items above).
