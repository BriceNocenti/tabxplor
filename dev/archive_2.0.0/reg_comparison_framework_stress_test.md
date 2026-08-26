# Stress test of the regression comparison framework — findings

Date: 2026-08-10. **Status: FULLY IMPLEMENTED (Phase 18z13, 2026-08-11) — see §11.**
Originally: research only, no R source modified. Every number below was
measured on this box today with `devtools::load_all()` at HEAD (`b6efb08`, Phase 18z10), on
`gss_cat_data_formatting()` (a `set.seed(1)` sample of 4 000 or 6 000 rows, stated per probe).
Scratch scripts were one-off and are not kept; each finding carries the code that reproduces it.

Scope: the whole "model effect vs observed effect" and "model vs model" apparatus — `empirical`,
`color = "adjustment"`, `color = "between_groups"`, `gap_se`, `obs`, `predictors = list(...)`,
`split_var`, several dependents, every family and every `effect`.

Companions, read as the design record this report tests against:
`dev/model_vs_observed_effect_colour.md` (z5, the descriptive measure),
`dev/model_vs_observed_gap_test.md` (z7/z8/z10, the test + the 3+ level families),
`dev/numeric_predictors_crude_counterparts.md` (z9), `dev/poisson_vs_logistic_binary_outcome.md` (z3).

---

## 0. Executive summary

**The statistics that were designed are sound and they verify.** Three independent checks reproduced
exactly: the gap p-value is the z-test of `(est − obs)/gap_se` to machine precision; `between_groups`'
`gap_se` equals `sqrt(SE_A² + SE_B²)` recovered from the printed intervals to five digits; and
`multiplier` scales the model estimate and its observed twin identically, so ruling Q6 holds on real
data. The collapsibility gate fires exactly where the design says it should, on every family.

**What fails is not the mathematics — it is the boundary between the framework and the rest of
`tab_reg()`.** Eleven defects, of which four change what a user reads off a published table:

| #    | finding                                                                                                                   | severity      |
|------|---------------------------------------------------------------------------------------------------------------------------|---------------|
| D1   | In model comparison the crude reference is computed on a **different sample** than the model column it colours             | **severe**    |
| D2   | The additive adjustment scale is **not invariant to the outcome's unit** — hours/minutes/days give opposite readings       | **severe**    |
| D3   | `conf_level =` moves the model interval and the stars, but **not** the gap test (pinned at the option)                     | **major**     |
| D4   | The additive gap is rendered with **multiplicative glyphs** (`×0.02` for "2 percentage points")                            | **major**     |
| D5   | Model comparison puts **two `color_signif` policies in one table** (only the last model gets a test)                       | major         |
| D6   | Two silent no-ops: `poisson` + `effect = "ame"` + `adjustment`; `between_groups` without `split_var`                       | moderate      |
| D7   | The reference column/group legend **names a scale it cannot use** (the column is always uncoloured)                        | moderate      |
| D8   | A partially-tested column **greys its untested rows** as "non-significant" (z9's open item, now reachable everywhere)      | moderate      |
| D9   | Degenerate `split_var` / dependent combinations abort with `In index: 1.`                                                  | moderate      |
| D10  | `family = "auto"` **aborts** on an integer-valued numeric outcome (`age`, `tvhours`)                                       | moderate      |
| D11  | Minor: `Constant` star wording, multinomial `obs` ≠ `tab()` on the same data, `obs` written but unread on `Obs_*` columns  | minor         |

**Two of them contradict the package's own documentation.** The reg vignette (line 404) states that
crude and modelled are computed "on the *same people* (the same complete cases)" — under
`predictors = list(...)` with the default `na = "drop_by_model"` that is false, and the framework
*knows* it is false (it uses the row-count mismatch to switch the test off) yet still colours the gap.
And the vignette teaches `guaranteed_effect` as "adjustment moved this effect by at least ×1.1" while
on an additive measure the same legend prints `×0.02` for two percentage points.

**Statistically, three caveats are inherent and correctly handled, and one is under-stated.** The
non-collapsibility gate (Q1(b)), the between-group unequal-heterogeneity trap (§5.2 of the gap-test
report, independently confirmed by Williams 2009's measured 681/1000 false rejections) and the
per-cell multiplicity position are all defensible and documented. The under-stated one is D1's
class: *the framework tests whether two numbers differ, and the literature it operationalises — the
change-in-estimate criterion — is itself contested as a confounder-selection rule* (a 2024 paper is
titled "the change in estimate fallacy"). The docs say this once; given that `color = "adjustment"`
is the headline feature, it deserves to be said where the colour is taught, not only in the annex.

**Against `gtsummary` and `finalfit`, the gap is not in the comparison machinery — tabxplor is ahead
there — it is in the boring table furniture.** Five absences would stop a real user cold: **no N per
predictor level** (STROBE asks for it; both competitors always print it), **no per-predictor global
p-value** (`add_global_p`, the single most-missed item for multi-level factor predictors, which is
tabxplor's whole audience), **no survival/Cox**, **no mixed models**, **no multiple imputation**.
Three cheap QoL items would buy most of the "all-in-one" feeling: `n` per level, a per-predictor
omnibus test, and a crude-vs-adjusted overlay in `or_plot()`.

---

## 1. What was probed, and how

Nine families × effects (`binomial` coef / `ame` / `ame_ratio` / `exponentiate = FALSE`, `rr`,
`gaussian`, `poisson` counts coef / `ame`, `grouped_binomial`, `multinomial` coef / `ame` /
`ame_ratio`, `ordinal` coef / `ame` / `ame_ratio`) crossed with ten use cases (single model, nested
models, several dependents, `split_var` stacked, `split_var` spread, weights, survey design,
`method = "profile"`, `at = "reference"`, numeric predictors with and without `multiplier`).

For each cell the probe recorded: does it build; how many rows carry an estimate, an `obs` and a
`gap_se`; which messages fire; what the legend says; and what the colour slots are. Where a number
could be checked independently it was (against `glm`, `svyglm`, hand quadrature, and `tab()`).

---

## 2. The coverage matrix

`e` = rows with an estimate, `o` = rows with `obs`, `s` = rows with `gap_se`, out of the table's rows.
n = 4 000. `color = "adjustment"` throughout except where stated.

| path                                        | model column                | e/o/s      | descriptive colour | gap test |
|---------------------------------------------|-----------------------------|------------|--------------------|----------|
| binomial, coefficient (**the default**)     | `Model_OR`                  | 16/15/**0**| yes                | **no** (Q1(b) non-collapsible) |
| binomial, `exponentiate = FALSE`            | `Model_β`                   | 16/15/**0**| yes                | no (same estimand) |
| binomial, `effect = "ame"`                  | `Model_AME`                 | 15/15/12   | yes                | **yes**  |
| binomial, `effect = "ame_ratio"`            | `Model_RR`                  | 15/15/12   | yes                | **yes**  |
| `rr` (`family = "poisson"`, binary outcome) | `Model_RR`                  | 16/15/12   | yes                | **yes**  |
| gaussian                                    | `Model_β`                   | 16/15/12   | yes (**but D2**)   | **yes**  |
| poisson counts, coefficient                 | `Model_IRR`                 | 16/15/12   | yes                | **yes**  |
| poisson counts, `effect = "ame"`            | `Model_AME`                 | 15/**0**/0 | **none, silent**   | no (**D6**) |
| grouped binomial (`trials =`)               | `Model_OR`                  | 12/11/**0**| yes                | no (non-collapsible) |
| multinomial, coefficient                    | one col per category        | 12/11/**0**| yes (in-cell `obs`)| no (non-collapsible) |
| multinomial, `ame` / `ame_ratio`            | one col per category        | 11/11/9    | yes                | **yes**  |
| ordinal, coefficient                        | `Model_OR` + `Obs_cumOR`    | 11/9/**0** | yes                | no (non-collapsible) |
| ordinal, `ame` / `ame_ratio`                | one col per category        | 11/9/9     | yes                | **yes**  |
| numeric predictor (any family)              | its row                     | ✓/✓/✓      | yes                | **yes**  |
| weighted / survey design                    | unchanged                   | ✓/✓/✓      | yes                | **yes** (design-based) |
| `method = "profile"`                        | unchanged                   | ✓/✓/✓      | yes                | yes (Wald gap ≠ profile CI, documented) |
| `at = "reference"`                          | `Model_MER`                 | 11/**0**/0 | none (**messaged**)| no       |
| `split_var`, `between_groups`               | per group                   | ✓/✓/✓      | yes                | **yes** (quadrature) |
| `predictors = list(...)`                    | m1 … mk                     | ✓/✓/**last only** | yes (**but D1**) | last model only (**D5**) |

Read the table as the design intends and it is coherent: **the test exists on exactly the collapsible
estimands**, and the two blank rows (`at = "reference"`, poisson-AME) are estimand mismatches, not
bugs in the variance. The problem is that one of the two is announced and the other is not, and that
the ubiquitous **default** path (binomial coefficient) is the one with no test at all — so the modal
user of the headline feature never sees `color_signif` work.

---

## 3. The defects

### D1 — In model comparison the crude reference is on a different sample than the model (severe)

```r
d <- gss_cat_data_formatting(); set.seed(1); ds <- dplyr::slice_sample(d, n = 6000)
t <- tab_reg(ds, "married",
             list(m1 = "race", m2 = c("race","rincome"), m3 = c("race","rincome","relig")),
             empirical = TRUE, color = "adjustment")
get_num(t$m1)[3]   # 0.4216   <- model estimate,  fitted on 6000 rows
get_obs(t$m1)[3]   # 0.4895   <- its "observed" twin, computed on 3591 rows
```

`m1` **is** `married ~ race`, i.e. the crude model itself. Its true adjustment gap is exactly **zero**.
The table colours it **÷1.16**, past the first `adj_ratio` break (×1.1) — a coloured cell asserting
"adjustment moved this effect by 16 %" when no adjustment happened at all. The entire gap is
listwise deletion: the model uses its own complete cases (n = 6 000), the `Obs_*` block uses the
union frame over all models' predictors (n = 3 591).

The fix is already in the package and it proves the diagnosis:

```r
t2 <- tab_reg(ds, "married", list(m1 = "race", m2 = ..., m3 = ...),
              empirical = TRUE, color = "adjustment", na = "drop_all_models")
get_num(t2$m1)[3]  # 0.4895
get_obs(t2$m1)[3]  # 0.4895   <- identical; score = 1, no colour. Correct.
```

Per-model N under the default: m1 = 6 000, m2 = 3 604, m3 = 3 591.

**Why this is worse than an ordinary caveat.** The framework already detects the mismatch — the
`nrow(mdata) == nrow(f$data)` clause in `reg_gap_se_columns()` is exactly this check, and it is why
m1/m2 get no `gap_se`. So the code knows the two estimates are not on the same rows, switches the
*test* off, and **still colours the gap**. The reg vignette then states (line 404) that the two
numbers are computed "on the *same people* (the same complete cases)", and (line 359) presents the
degraded behaviour as "the colours stay descriptive" — but a descriptive colour of a
sample-composition artefact is not descriptive of anything.

Directions, in order of how little they change:

1. **Gate `obs` with the same clause that gates `gap_se`.** One condition, at the single point z5
   attaches the crude effect. A model on a different frame gets no `obs`, hence no colour, hence no
   false claim. Costs the descriptive reading on m1/m2 — which is the reading that is wrong.
2. **Default to `na = "drop_all_models"` when `empirical = TRUE` (or `adjustment` is requested) and
   `predictors` is a list**, with one `cli_inform`. This is what a careful analyst does anyway, it is
   what makes the LR comparison rows appear (they are currently suppressed for the same reason), and
   it makes the vignette's sentence true. Riskier: it silently changes N.
3. At minimum, **emit the message that already exists for the LR test** (`reg_compare_rows` says it
   clearly) on the colour path too. Today the colour path is silent.

Option 1 + 3 is the honest minimum; option 2 is what makes the feature *work* in comparison mode
rather than merely stop lying.

### D2 — The additive adjustment scale is not unit-invariant (severe, gaussian and count AMEs)

Same data, same model, same substantive adjustment; only the outcome's **unit** changes.

```r
ds$tv_min <- ds$tvhours * 60 ; ds$tv_day <- ds$tvhours / 24
f <- function(v) tab_reg(ds, v, c("race","relig","rincome"), family = "gaussian",
                         empirical = TRUE, color = "adjustment")
```

| outcome unit | gap (3 first coloured rows)     | colour slots |
|--------------|---------------------------------|--------------|
| hours        | −0.091, −0.160, −0.285          | 6, 7, 8      |
| **minutes**  | −5.47, −9.61, −17.09            | **8, 8, 8**  |
| **days**     | −0.0038, −0.0067, −0.0119       | **0, 0, 0**  |

In minutes every cell saturates at the deepest break; in days **the feature is entirely dark**. The
cause is that `adj_diff = c(0.02, 0.05, 0.1, 0.2)` is an **absolute** ladder calibrated for a
probability-scale AME (2/5/10/20 percentage points) and applied verbatim to a β in the outcome's own
units. `sd(tvhours) = 2.55`, so on this outcome the *first* break is 0.008 SD — everything is
"large".

The inconsistency is visible inside a single table: the crude column of the same table is
SD-standardized and says so (`Obs_diff: standardized difference (ref.): -0.8 -0.5 -0.2 …`), while its
adjustment gap is raw (`Model_β: adjustment: ÷0.2 … ×0.2`). `fmt_color_plan()` has the standardizing
block already — it is gated on `measure == "diff"` (`R/fmt_class.R` ~3130) — and `get_var(x)` carries
`var(Y)` on exactly these columns, put there by Phase 12c for precisely this purpose.

Direction: make the additive gap standardize the same way the additive `diff` measure does —
divide by `sqrt(get_var(x))` when the estimate is *not* on the probability scale, leave percentage
points alone when it is (`effect = "ame"` on binomial/multinomial/ordinal). That is one predicate
(`reg_fam_prob()` already exists) plus the existing block, and it would give the gap ladder a fixed
meaning in every table — which is the property the z4 `zscore` rework was adopted for.

*Note*: `between_groups` shares `fmt_adjustment_score()` and both scales, so it has the same defect on
gaussian and count outcomes. The multiplicative half (`adj_ratio`, OR/RR/IRR) is unit-free and fine.

### D3 — `conf_level =` does not reach the gap test (major)

```r
for (cl in c(0.95, 0.99)) {
  t <- tab_reg(ds, "married", c("race","relig"), effect = "ame", empirical = TRUE,
               color = "adjustment", color_signif = "grey_non_signif", conf_level = cl)
  x <- t[["Model_AME (adjusted %)"]]
  c(model_ci_width = diff(range(...)), gap_bounds_width = diff(unlist(fmt_gap_bounds(x))[...]))
}
```

| `conf_level` | model CI width (one row) | gap-bounds width | implied z on the gap |
|--------------|--------------------------|------------------|----------------------|
| 0.95         | 0.06378                  | 0.01777          | 1.96                 |
| **0.99**     | **0.08382**              | **0.01777**      | **1.96**             |

At `conf_level = 0.99` the printed intervals, the stars and the `diff`/`or` greying all move to 99 %;
the **adjustment/between_groups greying stays at 95 %**, because `fmt_gap_bounds()` re-inflates the
stored `gap_se` with `getOption("tabxplor.conf_level")` rather than the table's own level. One table,
two confidence levels, silently.

This is the z4-recorded "the colour engine is per-column, so it reads the option not the argument"
limitation — but it bites harder here than anywhere else, because for these two measures the *whole
interval* is manufactured in the engine (nothing 99 %-wide is stored anywhere to fall back on).
`reg_gap_se_of()` already receives the right `conf_level` to **invert** the printed bounds; the
inflation half then uses a different one.

Direction: store the level, or store the gap bounds instead of the SE. The cheapest honest fix is to
have `tab_reg()` record its `conf_level` in `meta$ci_settings` (it already does) and have the two gap
measures read that; if a per-column read is impossible, the second-cheapest is to make `tab_reg()`
warn when `conf_level` differs from the option while a gap measure is in play.

### D4 — Additive gaps are rendered with multiplicative glyphs (major, display)

```
# Model_AME (adjusted %): adjustment: ÷0.2 ÷0.1 ÷0.05 ÷0.02 ×0.02 ×0.05 ×0.1 ×0.2
#                                     [grey: non-significant or under ×0.02]
```

`×0.02` here means **"+2 percentage points"**. Two lines above, the `diff` measure renders its own
additive ladder correctly as `-30 -20 -10 -5 +5 +10 +20 +30`. The cause is that the `adjustment` and
`between_groups` `MEASURES` rows declare `threshold_mult = TRUE`, `break_over = ×`, `break_under = ÷`
**statically**, while their scale is chosen **dynamically** by `std_when = "additive"`. So the glyphs
describe the multiplicative branch and are printed on the additive one too.

The same three facts already vary per column elsewhere (z8 made the reference phrase and the interval
name per-channel for exactly this reason), so the fix is the established pattern: resolve
`threshold_mult` / `break_over` / `break_under` from the *selected scale*, not from the measure row.
`unit_kind = "diff"` would then also let the legend say "points" the way the `diff` line does.

The vignette's own teaching line — `"guaranteed_effect"` = "adjustment moved this effect by at least
×1.1" — is only true on the multiplicative branch; on the additive one the same sentence should read
"by at least 2 points".

### D5 — Model comparison shows two significance policies in one table (major)

```
# m1, m2: adjustment: ÷0.2 ÷0.1 ÷0.05 ÷0.02 ×0.02 ×0.05 ×0.1 ×0.2
# m3: adjustment: ÷0.2 … ×0.2 [grey: non-significant or under ×0.02]
```

`gap_se` coverage across the same table: m1 **0/16**, m2 **0/16**, m3 **12/16**. Because
`fmt_gap_force_policy` reads an all-NA `gap_se` as "no test here", m1 and m2 read under `ignore` while
m3 reads under the user's `grey_non_signif`. The reader is invited to compare an attenuation path
across three columns whose colours mean different things — and the legend, to its credit, says so,
which is how the defect was found.

Mechanically this is D1's row-count gate again (only the model whose frame equals the union frame can
be tested). Fixing D1 by option 2 fixes D5 as a side effect: with `na = "drop_all_models"` every model
is on the union frame, so every column gets a test and one policy governs the table.

### D6 — Two silent no-ops (moderate)

```r
tab_reg(ds, "tvhours", P, family = "poisson", effect = "ame",
        color = "adjustment", color_signif = "grey_non_signif")
#  -> Model_AME: obs 0/16, gap_se 0/16, no colour, NO message
tab_reg(ds, "married", P, color = "between_groups")     # no split_var
#  -> no colour, NO message
```

The first is `reg_same_estimand()` correctly refusing to pair an **additive** count AME with the crude
**rate ratio** (the z8-B defect closure — the right call). The second is a user asking for a
between-group comparison without groups. Both produce a table that looks like the colour argument was
ignored. Compare with the two paths that *do* announce themselves: `at = "reference"` and the
non-collapsible OR both emit a precise, actionable `cli_inform`. The rule should be uniform: **if a
colour measure was requested and cannot be computed, say why, once.**

### D7 — The reference column/group legend names a scale it cannot use (moderate)

```
# White tvhours: between groups: ÷0.2 ÷0.1 ÷0.05 ÷0.02 ×0.02 ×0.05 ×0.1 ×0.2
# Black tvhours, Other tvhours: between groups: … [grey: non-significant or under ×0.02]
```

`White` is the reference group: its `obs` is NA on every row (measured: 0/13), so the column is
uncoloured by construction and no break in that ladder can ever fire. Printing the scale for it is
the legend describing a colouring that does not exist — and it costs the grouping (the two lines
would otherwise merge). The honest line is "White: reference group", the same way `has_ref_lead`
already words a reference elsewhere.

Related, and a genuine QoL gap: **the reference group cannot be chosen.** `reference = c(race =
"Black")` is silently ignored for the split axis (measured: `White` is still the uncoloured
reference); the only way to change it is to relevel the data. `split_var` has no reference argument.

### D8 — A partially-tested column greys its untested rows (moderate)

Measured on a `between_groups` spread table (`married ~ relig + rincome`, `split_var = race`):

| level              | est   | obs   | gap_se | slot |
|--------------------|-------|-------|--------|------|
| 4-Jewish           | 0.000 | 0.916 | **NA** | 0    |
| 5-Buddhist/Hinduist| 0.000 | 0.640 | **NA** | 0    |
| others             | …     | …     | 0.27–1.25 | …  |

A group with an empty cell yields an infinite log-interval, hence no recoverable SE. Under
`grey_non_signif` those rows render **identically to a tested-and-non-significant row** (both land in
slot 0, measured above): the table says *not significant* where the truth is *not tested*, and the
reader has no way to tell the two apart. This is z9's recorded open item; it is worth re-recording
that it is **not confined to mixed predictor kinds**: any zero cell, any `method = "profile"` group,
and any model-comparison column reaches it. The two candidate fixes remain the ones z9 named (a
per-row `force_policy`, or dropping the whole column's test); the measurement here just says the case
is common.

### D9 — Degenerate `split_var` / dependent combinations abort with `In index: 1.` (moderate)

```r
tab_reg(ds, "married", c("race","relig"), split_var = "black", color = "between_groups")
#> Error in purrr::map(sl, ...): ℹ In index: 1.
#> Caused by error in `contrasts<-`: contrasts can only be applied to factors with 2 or more levels

tab_reg(ds, c("married","black"), "relig", split_var = "race", color = "between_groups")
#> Error: ℹ In index: 1. / ℹ In index: 2. / The dependent variable "black" must be binary (2 levels).
```

Both are real data problems (a predictor, or a dependent, that is constant inside a split group), and
splitting by a coarsening of a predictor is a *common* first attempt. The inner message is
informative in the second case and cryptic in the first; both arrive wrapped in `purrr` index noise.
The pattern `tab()` already uses for its own degenerate cases (Phase 18p) applies directly: check
before fitting, name the group and the variable.

### D10 — `family = "auto"` aborts on an integer-valued numeric outcome (moderate)

```r
tab_reg(ds, "age",     c("race","relig"))   # Error: Cannot auto-detect the model family for "age".
tab_reg(ds, "tvhours", c("race","relig"))   # Error: Cannot auto-detect the model family for "tvhours".
```

`reg_detect_family()`'s gaussian branch requires `any(y %% 1 != 0)`, so **every integer-stored
continuous variable** — age in years, years of education, income in whole currency units, a Likert
sum — falls through to the abort. Two consequences: an all-in-one package errors on one of the two
most common outcome types, and **the R side disagrees with the jamovi side**, whose family selector
was explicitly told (Phase 18h) to auto-select `poisson` for integers.

The jamovi ruling is the natural resolution: integer → `poisson` with a message naming `gaussian`;
non-integer numeric → `gaussian`. Whatever is chosen, R and jamovi should choose the same thing.

### D11 — Three minor items

- **The `Constant` row is starred under a legend that does not describe it.** The footer says
  "significantly different from the reference category (in bold)"; the intercept has no reference
  category — its star means "baseline odds ≠ 1". Either suppress the star on that row or word it.
- **The multinomial in-cell `obs` is not the number `tab()` prints on the same data.** Measured:
  `obs = 0.3965` vs `tab(ds, race, party3, pct = "row", OR = "OR")` = `1/2.49 = 0.4016`. The
  difference is entirely the complete-case frame (the reg crude drops rows missing on *any*
  predictor). Correct behaviour, but the vignette teaches the identity without the caveat.
- **`obs`/`gap_se` are written on `Obs_*` columns under `between_groups` and never read** (those
  columns colour on their own `diff`/`or` measure). Harmless, but it is a stored value with no
  consumer — worth either using (a two-channel `c("diff", "between_groups")` would) or not writing.

---

## 4. Statistical soundness, family by family

### 4.1 What verifies

| check                                                                   | result                        |
|-------------------------------------------------------------------------|-------------------------------|
| gap p == `2·Φ(−|est − obs| / gap_se)`                                    | exact, all 12 rows            |
| `between_groups` `gap_se` == `sqrt(SE_A² + SE_B²)` from printed bounds   | exact (0.27147 vs 0.27147)    |
| `multiplier` scales estimate and `obs` together                          | 1.1487/1.1704 at `"sd"`; 1.008/1.0091 at 1 |
| collapsibility gate fires on and only on conditional probability-scale coefficients | all 9 family × effect combos |
| ordinal / multinomial marginal paths carry a test; their coefficient paths do not | as designed |
| weighted and survey-design paths keep a test (design-based)              | ✓                             |

The premise the whole apparatus rests on — *the observed effect is the model's own effect fitted with
one predictor* — held everywhere it was probed, including the z9 numeric and z10 ordinal fits.

### 4.2 The three inherent caveats, and whether the docs carry their weight

**(a) Non-collapsibility on the odds-ratio path.** Handled by ruling Q1(b): no test, plus a message
naming the three fixes. This is the correct call and it is well implemented. External support is
unambiguous — the change-in-estimate method "can spuriously detect confounding when it is based on a
noncollapsible measure of association", and the OR "tends to move further away from 1 when adjusting
for more and more variables, even in the absence of confounding". Nothing to change.

The residual product problem is that the **default** path is the non-testable one, so a user who
never reads the message experiences `color_signif` as inert. Worth considering: when
`color = "adjustment"` is requested on a binomial coefficient, the message could *offer* rather than
merely name the alternative, or `tab_reg()` could document `effect = "ame_ratio"` as the recommended
pairing for this colour.

**(b) Between-group differences are differences in that measure, not in a structural effect.** The
gap-test report measured 77 % rejection with identical structural β and unequal residual variance, on
all three scales. This is exactly Allison (1999) / Williams (2009), and Williams' own simulation puts
it at **681 of 1 000** false indications for a 40 % scale difference — an independent confirmation of
the package's number. The docs carry this. What they do not carry is the *pointer*: for a
sociology audience the next step is the heterogeneous-choice model (`oglm`-style) or, for nested
models, **KHB**. One sentence with two names would be a real service.

**(c) Multiplicity.** Per-cell at `conf_level`, uncorrected, measured at "one table in five shows a
spurious significant gap" (7 cells, collapsible scale). Consistent with every other per-cell
significance in the package, correctly refused a correction (it would break the
`guaranteed_effect` invariant). Stated once in `?tab_reg`; given that the aggregated
`stats = "interaction"` line exists for `split_var` and is the multiplicity-free reading, the
docs could point at it more loudly as the *confirmatory* companion to the per-cell colour.

### 4.3 What the framework is, in the literature's terms — and one missing pointer

The gap test is a **stacked-influence-function (M-estimation) test of the difference between two
estimators on the same data**. For OLS that is exactly the setting of **Clogg, Petkova & Haritou
(1995)**, whose closed-form standard error for the crude-minus-adjusted difference is the canonical
sociology reference (with **Allison (1995)**'s comment on random predictors); the influence-function
route generalises it to GLMs, survey designs and marginal effects, which is precisely what
CPH's own §"results also given for the class of generalized linear models" gestures at.

**No tabxplor document cites CPH.** For the target audience (French sociology) that is the one
citation that would make the feature legible as a known method rather than a package invention. It
belongs in `?tab_reg` and in the gap-test report's reference list.

Two further pointers worth one line each in the docs:

- **KHB (Karlson–Holm–Breen 2012; Kohler, Karlson & Holm 2011)** for nested logit comparison: it
  separates the change due to confounding from the change due to rescaling, which is the exact
  decomposition the `predictors = list(...)` colour cannot do. The z5 report already recommends
  pointing at it; the shipped docs do not.
- **Marginal standardization / g-computation** as the principled OR fix — already implemented three
  ways (`ame`, `ame_ratio`, `family = "poisson"`), but not named with the term the literature uses,
  which is what a reviewer will ask for.

### 4.4 The framing risk

The framework operationalises the **change-in-estimate criterion**, and that criterion is contested
as a confounder-selection rule: the 10 % cut-off is folklore traceable to a single Maldonado &
Greenland (1993) simulation, the appropriate cut-off is known to vary with effect size, n, error SD
and exposure–confounder correlation, and a 2024 critique is titled *"the change in estimate fallacy"*.

tabxplor's position is already the defensible one — it reports *how much two numbers differ and
whether that difference exceeds noise*, and refuses to flag "confounder detected" (z5 §1.1). But the
default `adj_ratio` ladder **starts at ×1.1**, i.e. it literally draws the 10 % rule as its first
colour break, and the vignette's reading ("if a predictor's model OR is much closer to 1 than its
crude OR, the raw association was largely explained by the other predictors") is the causal reading
the literature warns about. Two cheap mitigations: say in the vignette (not only the annex) that the
break is conventional and not a decision rule, and avoid the word "explained" where "attenuated" is
meant.

---

## 5. Stars and colour mean different things in the same cell

Under `color = "adjustment"` a cell carries two independent significance statements: **stars** =
"this effect differs from the reference level", **colour/greying** = "the adjustment gap differs from
zero". All four combinations occur in one small table (binomial AME, `race + relig`, n = 6 000):

| level             | stars | p(effect) | p(gap) | coloured |
|-------------------|-------|-----------|--------|----------|
| Black             | ***   | 0.0000    | 0.0058 | no (gap below the break) |
| 2-Catholic        | —     | 0.2960    | 0.0000 | **yes** |
| 6-Muslim          | *     | 0.0570    | 0.5585 | no      |
| 8-None            | ***   | 0.0000    | 0.0000 | yes     |

`2-Catholic` is the instructive one: no effect worth reporting, but adjustment moved it a lot and
significantly — a *coloured, unstarred* cell. This is not a defect (the vignette states it: "the
significance stars in the cells keep reading each estimate's own p-value … not the gap's"), but it is
the single most likely misreading of the feature, and it currently lives in a paragraph of the
"expert" section. It deserves the worked four-row table above, in the main text, because the
combination is genuinely useful once seen: *starred + uncoloured* = a robust effect, *unstarred +
coloured* = an effect that only the crude table showed.

---

## 6. Against the all-in-one competitors

What the comparison machinery does, tabxplor does better than anything on CRAN: `gtsummary` and
`finalfit` both produce crude-beside-adjusted tables (`tbl_uvregression()` + `tbl_regression()` +
`tbl_merge()`; `finalfit()`'s univariable/multivariable columns), but **neither quantifies, tests or
colours the gap** — the reader is left to eyeball two columns, which is exactly the manual work
`color = "adjustment"` removes. Nothing else ships a design-based test of the crude-vs-adjusted
difference at all.

The gap is everywhere else:

| capability                                  | tabxplor `tab_reg()`             | gtsummary                       | finalfit                     |
|---------------------------------------------|----------------------------------|---------------------------------|------------------------------|
| crude beside adjusted                       | **yes, one call**                | `tbl_merge()` of two tables     | `finalfit()`, one call       |
| **gap quantified / coloured / tested**      | **yes**                          | no                              | no                           |
| **N per predictor level**                   | **no** (carried, never shown)    | always                          | always                       |
| **per-predictor global p** (3+ level factor)| **no**                           | `add_global_p()`                | via `glm` + `anova`          |
| N events                                    | no                               | `add_nevent()`                  | yes (dependent counts)       |
| FDR / multiplicity adjustment               | no (deliberate)                  | `add_q()`                       | no                           |
| VIF / collinearity                          | no                               | `add_vif()`                     | no                           |
| model GOF block                             | **yes** (rich, per family)       | `add_glance_table()`            | `metrics = TRUE`             |
| model comparison rows (LR / F / ΔAIC)       | **yes**                          | manual                          | manual                       |
| survey designs                              | **yes** (native `wt/ids/strata/fpc`) | `tbl_svysummary` + `svyglm`  | no                           |
| multinomial / ordinal as first-class        | **yes** (one column per category)| via `broom` tidiers             | no                           |
| AME / marginal effects                      | **yes** (`ame`, `ame_ratio`, `at`)| via `marginaleffects` tidier   | no                           |
| **Cox / survival**                          | **no**                           | yes                             | yes (`coxph`, `hr_plot`, KM) |
| **mixed / random effects**                  | **no**                           | yes (`lme4`)                    | yes (`random_effect =`)      |
| **multiple imputation**                     | **no**                           | `tbl_regression` on `mice` pool | `mice` workflow documented   |
| interactions as first-class                 | escape hatch only (`formula =`)  | yes                             | yes                          |
| missing-data reporting                      | N per model only                 | via `tbl_summary`               | `missing_pattern()`, `ff_glimpse()` |
| forest plot                                 | `or_plot()` (no crude overlay)   | `plot()`                        | `or_plot()`, `hr_plot()`     |
| variable labels                             | **yes** (Phase k, opt-in)        | yes                             | `ff_label()`                 |
| Excel / html / md export, colour            | **yes, unmatched**               | gt/flextable                    | knitr                        |
| point-and-click UI                          | **yes (jamovi)**                 | no                              | no                           |

---

## 7. Missing features that would be a no-go for many users

Ranked by how many users would abandon the package over them, given a sociology/survey audience.

1. **N per predictor level.** STROBE explicitly asks for the unadjusted numbers behind the
   association, and every competitor prints `n (%)` per level. tabxplor **already has the number** —
   `get_n()` on the `Obs_%` column returned 4582 / 838 / 538 — it is simply never displayed, and it
   is absent entirely when `empirical = FALSE` (the default). This is the cheapest high-value fix in
   the whole report: an `add_n`-style display token or column, mirroring `tab()`'s own `add_n`.
2. **A per-predictor global p-value.** For a 8-level `relig` the table shows 7 stars against a
   reference and no answer to "is religion associated with marriage at all?". `add_global_p()` is
   gtsummary's most-used add-on for exactly this reason, and tabxplor's audience is *entirely*
   multi-level categorical predictors. The machinery exists: `reg_interaction_rows()` already runs
   `drop1()` / `regTermTest()` and renders a per-predictor footer **line**; a `stats = "global"`
   using the same producer is a small increment.
3. **Survival / Cox.** A hard no-go for medical and demographic users. Large job (`coxph`, tidy,
   PH diagnostics, a `hr_plot`), and arguably out of scope — but it should be stated as out of scope
   rather than left as an apparent omission.
4. **Mixed / multilevel models.** Comparative-sociology work on pooled country surveys expects
   `(1 | country)`. Also large; `broom.mixed` would carry most of it.
5. **Multiple imputation.** The framework's own D1 is a missingness problem, so the absence is
   pointed: the honest answer to different complete-case frames is imputation, and tabxplor cannot
   consume a `mice` pool.
6. **Interactions as first-class terms.** Available only through the compound-`formula` escape hatch,
   which then disables `empirical` and every comparison feature. Given that `stats = "interaction"`
   already exists for `split_var`, a predictor-level interaction would be a natural extension.

Items 1, 2 and 6 are in scope and cheap-to-medium; 3, 4 and 5 are strategic decisions.

---

## 8. Quality-of-life features that would give the all-in-one feeling

Ordered by value per line of code. All of them ride machinery that already exists.

1. **`n` per level** (see §7.1) — one display token, data already carried.
2. **A crude-vs-adjusted overlay in `or_plot()`.** The classic figure of this literature, and the
   plot already reads fmt fields with no refit; `obs` now carries the crude estimate *in the model
   column*, so the second point-and-whisker needs no second column. z5 §11.5 recommended it; today
   `or_plot()` on an `empirical = TRUE` table just picks one column and says so
   (`ℹ Several odds-ratio columns; plotting "Obs_OR"`).
3. **A reference-group argument for `split_var`** (D7): today the reference group is the first level
   and `reference =` is ignored for it.
4. **A `Δ` / attenuation display token.** `display = "{or} ({obs})"` exists; the *gap* itself
   (`×1.13`, `+3 pts`) has no token, only the tooltip. One token would let a user put the gap in a
   cell when the colour is not enough (print, black-and-white, Excel).
5. **Sort by attenuation.** `arrange()` on the field already works; one vignette line.
6. **A `stats = "global"` per-predictor test** (§7.2) — QoL and correctness at once.
7. **A one-line footer sentence for the comparison** ("adjustment moved 4 of 11 effects
   significantly"). The maintainer ruled this "noise" in z8 Q7; worth re-opening *only* for the
   `predictors = list(...)` case, where the per-column policies differ (D5) and a single sentence
   would say which columns were actually tested.
8. **A `crude_only` / `model_only` convenience** for the common "I want the classic
   univariable-then-multivariable table" — tabxplor can already produce it; it is not named.

---

## 9. Open questions for the maintainer

- **Q1 (D1).** Gate `obs` with the same row-count clause that gates `gap_se` (colour disappears where
  the frames differ), **or** default `na = "drop_all_models"` under `empirical`/`adjustment` with a
  model list, **or** both? Doing nothing leaves a coloured artefact and a false sentence in the
  vignette.
- **Q2 (D2).** Standardize the additive gap by `sqrt(get_var(x))` on non-probability scales
  (recommended — it makes the ladder mean the same thing in every table and reuses the existing
  block), or introduce a separate unit-aware break scale, or document the unit dependence?
- **Q3 (D3).** Should the gap measures follow the table's `conf_level` rather than the global option?
  If a per-column read is impossible, is a warning acceptable?
- **Q4 (D4).** Resolve `threshold_mult` / `break_over` / `break_under` from the selected scale rather
  than the measure row — confirm, since it changes rendered legends (a conscious snapshot regen).
- **Q5 (D6).** Adopt a uniform rule: any requested colour measure that cannot be computed emits one
  `cli_inform` naming the reason. Two paths already do; two do not.
- **Q6 (D10).** Make `family = "auto"` fall back for integer outcomes, matching jamovi's own rule
  (integer → poisson, message naming gaussian)?
- **Q7 (§7).** Are Cox/survival, mixed models and multiple imputation **out of scope** (to be stated
  as such in `?tab_reg` and the vignette), or roadmap items?
- **Q8 (§4.3).** Add the Clogg–Petkova–Haritou citation (and the KHB pointer) to `?tab_reg` and the
  gap-test report — the feature currently reads as a package invention rather than a known method.
- **Q9 (§7.1–7.2).** `n` per level and a per-predictor global p: both are small, both are the two
  things a gtsummary/finalfit user will look for first. Ship in 2.0.0 or after?

---

## 10. References

### Comparing crude and adjusted estimates
- Clogg, C. C., Petkova, E. & Haritou, A. (1995). *Statistical Methods for Comparing Regression
  Coefficients between Models.* American Journal of Sociology 100(5), 1261–1293.
- Allison, P. D. (1995). *The Impact of Random Predictors on Comparisons of Coefficients Between
  Models: Comment on Clogg, Petkova, and Haritou.* American Journal of Sociology 100(5), 1294–1305.
- Maldonado, G. & Greenland, S. (1993). *Simulation study of confounder-selection strategies.*
  American Journal of Epidemiology 138(11), 923–936.
- Lee, P. H. (2014). *Is a Cutoff of 10% Appropriate for the Change-in-Estimate Criterion of
  Confounder Identification?* Journal of Epidemiology 24(2), 161–167.
- (2024). *The mockery that confounds better treatment of confounding in epidemiology: the change in
  estimate fallacy.* Global Epidemiology.

### Collapsibility, marginal vs conditional
- Greenland, S., Robins, J. M. & Pearl, J. (1999). *Confounding and Collapsibility in Causal
  Inference.* Statistical Science 14(1), 29–46.
- Martinussen, T. & Vansteelandt, S. (2013). *On collapsibility and confounding bias.*
- Pang, M., Kaufman, J. S. & Platt, R. W. (2013). *Studying noncollapsibility of the odds ratio with
  marginal structural and logistic regression models.*
- Greenland, S. (2010). *Marginalia: comparing adjusted effect measures.* Epidemiology 21(6).

### Comparing coefficients across models and across groups
- Karlson, K. B., Holm, A. & Breen, R. (2012). *Comparing Regression Coefficients Between Same-sample
  Nested Models Using Logit and Probit.* Sociological Methodology 42(1), 286–313.
- Kohler, U., Karlson, K. B. & Holm, A. (2011). *Comparing coefficients of nested nonlinear
  probability models.* Stata Journal 11(3), 420–438.
- Mood, C. (2010). *Logistic Regression: Why We Cannot Do What We Think We Can Do, and What We Can Do
  About It.* European Sociological Review 26(1), 67–82.
- Allison, P. D. (1999). *Comparing Logit and Probit Coefficients Across Groups.* Sociological Methods
  & Research 28(2), 186–208.
- Williams, R. (2009). *Using Heterogeneous Choice Models to Compare Logit and Probit Coefficients
  Across Groups.* Sociological Methods & Research 37(4), 531–559.
- Altman, D. G. & Bland, J. M. (2003). *Interaction revisited: the difference between two estimates.*
  BMJ 326, 219.

### Reporting standards and comparator packages
- von Elm, E. et al. (2007). *STROBE Statement: Explanation and Elaboration.* PLoS Medicine 4(10),
  e297 — item 16(a): report unadjusted and confounder-adjusted estimates, with the numbers behind them.
- `gtsummary` reference index (`tbl_regression`, `tbl_uvregression`, `tbl_merge`, `add_global_p`,
  `add_n`, `add_nevent`, `add_q`, `add_vif`, `add_glance_table`).
- `finalfit` getting-started vignette (`finalfit()`, `summary_factorlist()`, `fit2df()`,
  `finalfit_merge()`, `or_plot()`, `hr_plot()`, `metrics = TRUE`, `random_effect =`).
- `sjPlot::tab_model()`, `modelsummary` — multi-model side-by-side tables.

### In-repo companions
- `dev/model_vs_observed_effect_colour.md` (z5) — the descriptive measure, collapsibility audit.
- `dev/model_vs_observed_gap_test.md` (z7/z8/z10) — the test, the influence functions, §13 for the
  3+ level families.
- `dev/numeric_predictors_crude_counterparts.md` (z9) — numeric crude twins, `multiplier`.
- `dev/poisson_vs_logistic_binary_outcome.md` (z3) — the RR routes and the Goodman terminology trap.
- `dev/new_colors_UI.md` — the per-cell significance position (W11) this report checks against.


---

## 11. Implementation (Phase 18z13, 2026-08-11)

All eleven defects closed, plus the two §7 items the maintainer opted into. Suite green in both
locales (`fr_FR.UTF-8`: FAIL 0, WARN 0, SKIP 4, PASS 4979; CI-equivalent `LC_ALL=C.UTF-8 LANGUAGE=en`:
FAIL 0, SKIP 8, PASS 4962). The only snapshot that moved is `_snaps/fmt-contract.md` (the column-
attribute list); the 36 structural `_golden/*.rds` were regenerated with the delta proved minimal over
1787 cells. `_snaps/golden.md`, `_snaps/render-html.md` and every `_color_golden/*.rds` are untouched.

### The maintainer's rulings (they decide §9)

| # | ruling |
|---|--------|
| Q1 (D1) | A shared complete-case population becomes the DEFAULT; per-model becomes the opt-in exception. AND gate `obs` where the populations differ. "Shared" means per OUTCOME by default. |
| Q2 (D2) | Dispatch the gap scale from the estimate's own scale. |
| Q3 (D3) | Add the 12th column attribute `conf_level`. |
| Q4 (D4) | Glyphs and threshold form follow the SELECTED SCALE. |
| D8 | Honest legend wording only — no engine change, no extra message. |
| Q6 (D10) | integer outcome → gaussian; align jamovi to R, not the reverse. |
| Q9 (§7) | Ship `n` per level (`add_n = TRUE`) and `stats = "global"` (on by default). |

### Two corrections to this report, established during implementation

- **D1's "the colour path is silent" is wrong.** `tab_reg()` already emitted the `drop_all_models`
  advice on exactly that path, asserted by `test-adjustment-gap.R:189`. What was missing was the GATE.
- **One defect this report does not list**, in the same framework: `or_plot()` filtered crude columns
  with `grepl("^Emp\\.")`, a prefix Phase g renamed to `Obs_`, so every crude column had counted as a
  model one since — both for the default pick and for the "Several odds-ratio columns" message. Now
  reads the stored `role` (Phase 17c), which exists precisely so nothing matches a rendered label.

### What each fix turned out to be

- **D1/D5** — `na` became a three-value family named for the grain at which rows are dropped
  (`drop_by_outcome` / `drop_by_model` / `drop_all`), and it needed NO new mechanism: z9's
  `reg_fit(drop_extra =)` is exactly "variables the fit must be complete on without modelling", so the
  shared population is `drop_extra = all_predictors`. The old `drop_all_models` pre-pass on `data` is
  gone, and with it the "ignored for a prebuilt survey design" caveat — a pre-filtered frame breaks a
  prebuilt design's keep_mask, `drop_extra` does not. `reg_same_frame()` is the twin of
  `reg_same_estimand()` and gates the same two things; it reads `f$nobs` when `f$data` is absent, so
  the jamovi digest path keeps its `obs`. D5 dissolved: every column of an outcome now carries a test.
- **D2** — the (c)-vs-(d) discrimination is NOT `reg_fam_prob(model_family)`: a poisson count AME and a
  raw poisson coefficient are byte-identical in `(type, ci_type, model_family)`, and `is_logcoef`
  claims both. The separator is `var` — var(Y) is written exactly on the columns whose estimate lives
  in the outcome's own units, which is also the SD the standardization needs. `fmt_gap_scale_key()`'s
  ORDER is therefore the contract. A log-scale coefficient reads `log_odds_scale(adj_ratio)`: the same
  helper, so a user's `set_color_breaks(adj_ratio =)` reaches both twins.
- **D4** — deriving the glyphs from `plan$center` was rejected after evaluation: 2 of the 4 legacy
  measures (`or`, `contrib`) need an exception, and it cannot express `break_scale` or `unit_kind`. The
  per-scale override (`by_scale`, folded by `measure_facts(measure, policy, scale_key)`) mirrors the
  existing `guar` mechanism and is byte-identical for every pre-z13 measure BY CONSTRUCTION. It also
  let `contrib`'s `guar` shed the glyph entries its scale swap already implied.
- **D3** — the raw/resolved accessor split is load-bearing: the six reconcilers read
  `fmt_conf_level_attr()` so a bind carries "unknown" forward, the four engine thresholds read
  `get_conf_level()`. `vec_ptype2` needs the `is.na()` guard (two NAs compare NA; a bare `if (NA)`
  errors — the `same_comp` trap, second instance).
- **D7** — the "this column IS the baseline" predicate must key on the stored `obs` being empty, not on
  the plan's gate: under `grey_non_signif` a fully comparable column with no significant gap also
  gates nothing, and must still show its ladder.
- **D11** — the dead-write gate must read `fmt_color_attr()` (the whole length-≤2 vector), not
  `get_color()`: a gap measure almost always rides the BACKGROUND channel.
- **§7.1** — `add_n = TRUE` shifts every positional column reference in the reg tests. The fix was a
  shared role-aware selector (`tests/testthat/helper-reg.R`), not 50 patched call sites. Two engine
  consequences were mandatory: `reg_spread_models()` must skip role "n" when keying the GOF footer,
  and the `[dep]` bracket strip must cover it. Under `split_var + spread_models` the spread columns
  are now named `Model_OR_<group>` rather than `<group>`, and each group gains its N.
- **§7.2** — `reg_interaction_rows()` and the global test are the SAME computation (a per-predictor
  term test on a fit) differing only in which fit and which terms are dropped, so the ladder, the
  drop1 handling and the row shape were extracted into `reg_term_tests()`, and the two line renderers
  into `reg_term_test_line()`. The global test costs no extra fit and is emitted only for terms with
  2+ coefficients (a 1-df term's global p IS its cell's p).

### Still open

- §7.3-7.5 (Cox/survival, mixed models, multiple imputation) are now stated as **out of scope** in
  `?tab_reg` and both vignettes, rather than left as an apparent omission.
- §8.2 (a crude-vs-adjusted overlay in `or_plot()`) was NOT taken (maintainer's scope choice); only
  the stale-prefix repair landed.
- **Maintainer step:** `jamovi/jmvtabreg.a.yaml` changed (the three `na` values + the new default), so
  `jmvtools::prepare()` must regenerate `R/jmvtabreg.h.R`. Until then the live UI keeps the old
  `drop_by_model` default, which is still a valid value — no breakage, just the old behaviour.
