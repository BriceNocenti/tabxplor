# Standardised χ² cell residuals in tabxplor — design study

Date: 2026-08-05 (rev. 3 — maintainer's framing + the three decisions of §9 taken). Status:
**IMPLEMENTED** (Phase 18z4). One naming drift since: the break scale this report calls `residual`
shipped under that name and was renamed **`zscore`** in Phase 18z8 (it is a z scale, and a second
measure could want it). Read every `residual` scale key below as `zscore`; the statistic itself is
still the adjusted standardized residual.

Rev. 2 re-organised the report around **which real question each quantity answers** rather than
around "what is mathematically available". Rev. 3 records the maintainer's three rulings (§9) and
reworks §5 around them.

**Settled frame:** `contrib` + `ignore` is the *correspondence-analysis* reading and is correct
as-is; significance comes from standardised residuals; no new vctrs field; the *absolute* residual
scale is reached through the **policy axis** (`guaranteed_effect`), not through a second measure.

---

## 0. Executive summary

The feature is **not** "add a residual measure". It is **"make the significance of a χ² contribution
honest, and give the reader an absolute scale when they ask for one"** — one measure (`contrib`),
three policies that now each answer a distinct real question:

| `color_signif`      | what the colour encodes                                                     | the question it answers                                                                   |
|---------------------|-----------------------------------------------------------------------------|-------------------------------------------------------------------------------------------|
| `ignore`            | relative contribution, × the mean cell contribution (floats with the table) | "which cells build **this** association?" — the CA reading, **unchanged**                 |
| `grey_non_signif`   | the same relative contribution, greyed where not significant                | "…and which of them can I trust?"                                                         |
| `guaranteed_effect` | **the adjusted standardised residual, on an absolute scale**                | "which cells are notably off independence — comparably across tables?" — the SPSS reading |

Four changes deliver it:

1. **Always compute the per-cell p-value from the adjusted standardised (Haberman) residual**, not
   from the Pearson residual. Today's gate tests `(o−e)/√e` at 1.96, but that statistic's variance is
   `(1−p_i·)(1−p·_j) < 1`, so it is systematically conservative — measured factor **1.10–3.09** on one
   3×4 table, and on the package's own example it misses a cell whose adjusted residual is **−3.91**
   (p ≈ 9·10⁻⁵). This single fix repairs *both* significance policies.
2. **Compute that residual on the package's own inference rule** — weighted estimate, unweighted
   (or Kish-effective) base — instead of on the weighted N. Today, population-scale weights make every
   stored cell p-value literally **0.000** (measured), which makes `color_signif` unusable on real
   survey data.
3. **`guaranteed_effect` scores the residual on a new absolute `zscore` break scale** (default
   `c(2, 3, 4, 6)`) — the maintainer's ruling: `|z|` is comparable *across* tables where the ×-mean
   contribution is intrinsically relative to one, and ±2/±3 is what SPSS-trained readers know. §5.3.
4. **The residual becomes readable**: shown in the HTML tooltip, and printable in cells via the
   existing composite-display grammar (`display = "{pct} ({resid})"`) — one more token beside the
   `ctr` token that already exists.

`contrib` keeps its name and its `ignore` behaviour untouched — the relative contribution that floats
with the table *is* the Le Roux/Benzécri reading and is right for a measure that makes no significance
claim. Nothing about the CA use case changes.

**Zero new fmt fields** (§6): the residual is recoverable from the stored p-value as
`|z| = -qnorm(p/2)` — a derived display field exactly as `ci` already is. Storing z *in* the `pvalue`
field was considered and rejected (§6).

Multiplicity control is **deferred and documented** (§3.4), not shipped.

---

## 1. The framework: which real question does each quantity answer?

This is the section that decides everything else. A cell of a cross-tab supports exactly four
questions that users actually ask, and tabxplor should serve those four — not the cartesian product
of every statistic that could be computed.

| # | the real question                                                          | the right quantity                                                                     | tabxplor                                                       |
|---|----------------------------------------------------------------------------|----------------------------------------------------------------------------------------|----------------------------------------------------------------|
| 1 | "How does this cell differ from a comparison group I chose?"               | difference / ratio / OR vs a reference                                                 | `diff`, `ratio`, `or` + their CIs and stars                    |
| 2 | "Which cells build the association **in this table**?"                     | **relative contribution to χ²** (share of inertia) — necessarily relative to its table | `contrib` + `ignore` ✔ **already right**                       |
| 3 | "…and which of those can I trust?"                                         | the same contribution, gated by a **calibrated test**                                  | `contrib` + `grey_non_signif` — currently the wrong test ✘     |
| 4 | "Which cells are notably off independence — **comparably across tables**?" | the **adjusted standardised residual** on an absolute ±2/±3 scale                      | `contrib` + `guaranteed_effect` — currently a relative scale ✘ |

The key insight, and the reason rev. 1 over-designed: **questions 2-4 are not three measures the user
must choose between — they are one measure read three ways, and tabxplor's API already has the axis
for that.** `color` says what is measured, `color_signif` says how significance enters; so the
adjusted residual belongs on the **`color_signif` axis** — as the *test* in question 3 and as the
*scale* in question 4 — not as a second entry on the `color` axis.

This resolves the naming worry directly (§4.3): the measure is still the contribution
(Benzécri/Le Roux), so `"contrib"` is the correct name and needs no alias. The residual never becomes
a measure name; it becomes the correct test behind the greying, and the absolute scale behind
`guaranteed_effect`.

### 1.1 Why the "floating scale" is a feature here, not a bug

`contrib`'s score is `share of χ² × k`, so its b-th break is algebraically `|r| ≥ √(b·χ²/k)` —
a threshold that moves with the table (measured: `|r| ≥ 0.46` on a null table, `|r| ≥ 14.5` on
`race × party3`). Rev. 1 called this a defect. **With the maintainer's framing it is not**: a share of
inertia is *intrinsically* relative to its table, exactly as in a correspondence analysis, and under
`color_signif = "ignore"` no significance is claimed, so there is nothing to be inconsistent with.
A reader of a CA-style table is asking "which cells carry this association", and "1× the mean cell
contribution" is a meaningful, scale-free answer to that.

The incoherence appears **only** when significance enters — and it is then a problem of the *gate*,
not of the scale. Two consequences worth stating plainly:

- The "on a null table, `auto` colours 4 of 12 cells" observation from rev. 1 is **not a defect**:
  the shares sum to `k` by construction, so some cell is always above the mean, and under `ignore`
  that is an honest statement about where such association as exists is concentrated. It becomes a
  problem only if a user reads colour as significance — which is what `grey_non_signif` exists for,
  and which the fixed gate now handles correctly (measured: 0 of 12 cells survive the adjusted-residual
  gate on that table).
- Therefore **rev. 1's "should `color = "auto"` on count tables switch away from `contrib`?" question
  is withdrawn.** It should not. `auto` → `contrib` + `ignore` is the CA reading and is right.

---

## 2. What tabxplor computes today, and what is actually broken

### 2.1 The pipeline (verified)

`color = "contrib"` forces `chi2 = TRUE` and `totrow = TRUE` (`R/tab-resolve.R` ~L195-210); then
`tab_chi2(calc = c("ctr","p"))` → **`chi2_write_contrib()`** (`R/tab.R` ~L5972) writes, per cell, from
**weighted** counts:

```r
# var_contrib_ctr_signed()  (R/tab.R ~L5928)
observed_freq <- xwn / N                            # N = weighted grand total
expected_freq <- rowtot_w * coltot_w / N^2
spread        <- observed_freq - expected_freq
var  <- sign(spread) * spread^2 / expected_freq     # signed χ² contribution / N
ctr  <- var / Σ|var| over the subtable              # the cell's SIGNED SHARE of χ²
                                                    #   (total rows carry 1/k = the mean share)
pvalue <- 2 * pnorm(-sqrt(|var| * N))               # = 2 * pnorm(-|PEARSON residual|)   ← the defect
```

Colour score (`MEASURES$contrib$raw`, `R/fmt_class.R` ~L3072): `ctr / mean_contrib` = signed share × k.
Breaks `contrib = c(1, 2, 5, 10)`, additive, `center = 0`, auto-mirrored.

### 2.2 Defect A — the gate uses the wrong residual (measured)

The stored p is exactly `2·pnorm(−|Pearson residual|)`. Verified to all printed digits against
`chisq.test()$residuals` on `race × rincome` (NA dropped):

| column              | tabxplor stored p           | `2·pnorm(−\|Pearson\|)`     | `2·pnorm(−\|adjusted\|)`    |
|---------------------|-----------------------------|-----------------------------|-----------------------------|
| `1-Lt $10000`       | 1.5e-03 · 1.4e-04 · 2.4e-05 | 1.5e-03 · 1.4e-04 · 2.4e-05 | 1.5e-12 · 6.4e-06 · 1.1e-06 |
| `2-$10000 to 14999` | 0.0669 · 0.0012 · 0.2346    | 0.0669 · 0.0012 · 0.2346    | 9.1e-05 · 2.5e-04 · 1.9e-01 |
| `4-$25000 or more`  | 3.5e-04 · 9.6e-08 · 4.7e-04 | 3.5e-04 · 9.6e-08 · 4.7e-04 | 2.3e-28 · 2.2e-18 · 2.3e-08 |

The Pearson residual is **not** asymptotically N(0,1) — its variance is `(1−p_i·)(1−p·_j)`. Testing it
at 1.96 under-rejects by `1/√((1−p_i·)(1−p·_j))`, measured **1.10 → 3.09** on this one table, worst
where margins are most unequal (max factor 2 on a balanced 2×2). Live miss on the package's example:
`White / $10000-14999`, Pearson −1.83 (**not flagged**) vs adjusted **−3.91** (p ≈ 9·10⁻⁵).

Anchor for the docs (`matrix(c(30,20,15,35),2,2)`): Pearson ±1.58/±1.43, adjusted **±3.02** everywhere,
χ² = 9.09 (p = 0.0026). A ±2 rule on the Pearson residual finds nothing in a table significant at
p < 0.003.

### 2.3 Defect B — the residual is computed on the weighted N (measured)

Same data, same proportions, only the weight *scale* changed:

| weights                                | Σw          | stored per-cell p-values    |
|----------------------------------------|-------------|-----------------------------|
| `runif(.3, 3)`                         | 35 442      | 5.8e-06 · 1.2e-14 · 8.3e-04 |
| the same × 12 000 (population weights) | 425 300 663 | **0 · 0 · 0**               |

Every cell becomes "infinitely significant" as soon as weights carry population scale. This is the
defect that makes `color_signif` unusable on real survey data today.

### 2.4 Defect C — no absolute reading exists anywhere

Today, for contrib, `fmt_color_plan()` takes a special branch (`md$sig_source == "pvalue"`): score =
the observed contribution multiple, gate = significant, and the shared `offset_guaranteed_breaks()`
shifts the scale to `c(0, 1, 4, 9)` so every significant cell gets at least slot 1. The *set* of
coloured cells is right, but the *intensity* is still the table-relative share — so **every one of the
three policies reads on a scale that exists only inside its own table**, and two tables can never be
compared. Measured symptom: the most significant cell of `race × rincome` (adjusted z = **11.0**)
scores 1.18× the mean, while a z = 4.87 cell scores 1.66× — and neither number means anything in a
different table.

That is what §9-Q1 fixes by giving `guaranteed_effect` the absolute residual scale: not because
"guaranteed" demands a CI floor, but because a package that only ever colours on relative scales gives
its users no way to say "this cell is a ±3 cell" in the way SPSS-trained readers expect.

### 2.5 What is NOT broken

- The share itself, and `ignore` (§1.1).
- The requirement for total rows and a χ² pass (they carry the mean-contribution seed).
- The exclusion of contrib columns from significance **stars** (`fmt_stars_applicable()`), which is
  what makes the `pvalue` field free to be repurposed on those columns.

---

## 3. The statistics — only what bears on the design

### 3.1 The family, and the one identity that matters

For `o`, `e = r_i·c_j/N`, `p_i· = r_i/N`, `p·_j = c_j/N`:

| name                                               | formula                           | null variance              | role                                                                              |
|----------------------------------------------------|-----------------------------------|----------------------------|-----------------------------------------------------------------------------------|
| Pearson ("standardised" in SPSS/R `$residuals`)    | `(o−e)/√e`                        | **`(1−p_i·)(1−p·_j) < 1`** | `Σ r² = χ²` — the decomposition. *Not* a calibrated test.                         |
| **adjusted standardised (Haberman 1973)**          | `(o−e)/√(e(1−p_i·)(1−p·_j))`      | **1**                      | the calibrated test — the ±2 rule is only correct here                            |
| moment-corrected (García-Pérez & Núñez-Antón 2003) | Pearson rescaled to unit variance | 1                          | practically identical to adjusted across 2×2…8×12 — **not worth a second option** |

Two identities the design leans on:

- `r_pearson = sign(o−e)·√(raw contribution)` — the Pearson residual **is** the "standardised raw χ²
  contribution" (the maintainer's phrase, exactly). It orders cells identically to the current
  `contrib` score, so it adds no information the package lacks.
- `raw contribution = (1−p_i·)(1−p·_j)·z²` — the contribution and the adjusted residual differ by a
  per-cell margin factor. This is what makes §5.3's floor exact.

So the French tradition's "Pearson residual ≈ absolute contribution, divided by the total to get the
relative contribution" is *correct*, and it is precisely why the Pearson residual cannot double as the
test: it inherits the contribution's dependence on the margins.

### 3.2 The terminology trap (must be in the doc)

| tool                             | "standardized residual"  | "adjusted (standardized) residual"                      |
|----------------------------------|--------------------------|---------------------------------------------------------|
| SPSS Crosstabs                   | Pearson `(o−e)/√e`       | Haberman — **the ±2 rule refers to this one**           |
| R `chisq.test()`                 | `$residuals` = Pearson   | `$stdres` = Haberman (R's help calls it "standardized") |
| `questionr::chisq.residuals()`   | default = Pearson        | `std = TRUE` → `$stdres` = Haberman                     |
| Sharpe (2015) and most textbooks | Pearson, threshold \|2\| | often not distinguished                                 |

R's `$stdres` and SPSS's "adjusted" are the same number; R's help calling it "standardized" is why
half the applied literature reports the wrong one. One doc sentence must name the formula and its
SPSS/R equivalents, or French users arriving from `questionr` will mis-read it.

### 3.3 Thresholds

±2 is the working rule (≈ the 95 % normal quantile); ±3 is recommended for large tables (Haberman;
Sharpe 2015). Both fall out of `conf_level` via the existing `zscore_formula()` — **never hardcode
1.96** (`dev/new_colors_UI.md` §4.6 already fixed this as a rule).

### 3.4 Multiple comparisons — real, and quantified

| table (from the package's examples) | cells | expected false positives at α = .05 | Bonferroni α | ⇒ \|z\| |
|-------------------------------------|-------|-------------------------------------|--------------|---------|
| `relig × rincome`                   | 32    | 1.6                                 | 0.0016       | 3.16    |
| `marital × rincome`                 | 20    | 1.0                                 | 0.0025       | 3.02    |
| `partyid × rincome`                 | 32    | 1.6                                 | 0.0016       | 3.16    |

On a routine table, one or two spuriously flagged cells is the *expectation*. Note the coincidence
worth a doc sentence: **Bonferroni at k = 20–32 lands almost exactly on the classic ±3 rule.** Whether
to ship an opt-in correction is question **Q3** in §9. `dev/new_colors_UI.md` W11 already designed the
slot for it (at the `conf_level` layer, uniform across measures), so it would not be an ad hoc addition.

One finding that *removes* work: the modern recommendation is **not** to condition cellwise residuals
on a significant omnibus χ² (García-Pérez & Núñez-Antón 2014, *"another nail in the coffin of
conditional approaches to significance testing"*). So no "only grey when the table is significant"
gate should be added.

### 3.5 Sparse cells — the risk, precisely located

The danger is small **expected** counts, not small observed ones. Measured, a rare row (4 obs) against
a skewed column distribution:

```text
             x      y      z      w
observed  1.00   0.00   0.00   3.00
expected  2.76   0.81   0.20   0.22
pearson  -1.06  -0.90  -0.45   5.90
adjusted -1.91  -1.01  -0.46   6.08        # min expected in the table: 0.20
```

`|z| = 6.08` at `e = 0.2` — the normal approximation simply does not hold. In a benign case
(`e = 3.18`, `o = 5`) the adjusted residual is a calm 1.49, so this is **not** a general "small counts
get over-flagged" problem. Note the risk already exists today in a worse form: the contribution is
`r²`-based, so a sparse cell's inflated residual is *squared* into a large share.

tabxplor already computes `min_e` per table (`agg_chi2()`, in the `test` tibble), already uses a
`test_weak_min_e` threshold to trigger Fisher's exact (Phase 18j), and already has `n_min`. The
guard exists; it only needs wiring — see §5.5.

---

## 4. Answers to the maintainer's points

### 4.1 "`contrib` + `ignore` floats with the table to match a correspondence analysis; it's consistent because it ignores significance."

Agreed, and this is now the frame of the whole design (§1.1). Rev. 1's criticism of the floating scale
is withdrawn for the `ignore` case, and with it the proposal to change `color = "auto"` on count
tables. **Nothing about the CA use case changes.**

### 4.2 "Wouldn't it be consistent to use standardised residuals in both `grey_non_signif` and `guaranteed_effect`? … Wouldn't we simply need to always calculate the pvalue from standardised residuals?"

**Yes — and that is the core of the feature.** One test, computed once, used by both policies; the
magnitude axis is untouched. Concretely:

| policy              | magnitude (score)                                        | significance                             |
|---------------------|----------------------------------------------------------|------------------------------------------|
| `ignore`            | contribution multiple (CA), floats with the table        | none                                     |
| `grey_non_signif`   | contribution multiple (CA) — *unchanged*                 | grey where `\|z_adj\| < z(conf_level)`   |
| `guaranteed_effect` | **the adjusted residual `\|z\|`, absolute scale** (§5.3) | the scale itself starts at the threshold |

This is exactly your description of `grey_non_signif` ("colour still comes from the relative
contribution and floats with the table, grey out any cell under the threshold").

**`guaranteed_effect` = the absolute residual scale — decided (§9-Q1), and the reasoning is right.**
I had argued for a contribution *floor* (`raw × (1 − z_α/|z|)²`, the exact test-inversion floor,
which would have kept both policies in the same units and deleted an engine special case). The
maintainer's counter-argument wins on the criterion this phase is about — real use cases over formal
symmetry: **a table needs an absolute reading somewhere.** `× the mean contribution` is intrinsically
relative to its own table, so two tables can never be compared on it; `|z|` can. And ±2/±3 is the
scale SPSS-trained readers already have in their heads. Placing that reading on the `guaranteed_effect`
policy — rather than on a second measure — keeps one measure name, and a user who wants the relative
scale *with* significance still has `grey_non_signif` (and may set a `zscore` scale of their own if
they want to reshape the ladder).

The cost, accepted knowingly and to be documented: the colour **units change with the policy**
(× mean contribution → z), so `color` and `color_signif` are no longer strictly orthogonal for this
measure. `?tab`'s `color_signif` entry must therefore describe the `contrib` case explicitly rather
than relying on the generic "CI-floored, conservative effect" wording, which does not apply here.
One consequence worth one sentence in the docs: the absolute-z reading is only reachable *together
with* the significance gate. In practice this changes nothing — with a first break at 2 the cells the
gate removes (1.96 ≤ |z| < 2) are below the first break anyway.

**Caveats and inconsistencies I can see, stated honestly:**

- **The magnitude and the test are not the same quantity, and can disagree in ordering.** A cell can
  be highly significant and a small contributor (measured: `White / $10000-14999`, z = −3.91, 0.31×
  the mean contribution). That is a *fact about the statistics*, not a bug — the contribution is not
  a test statistic — but the legend must not imply otherwise. Under `grey_non_signif` the reader sees
  "coloured = contributes a lot **and** is significant"; that conjunction is the honest reading and
  should be the legend wording.
- **`grey_non_signif` becomes strictly more permissive than today** (the old gate was ~1.1–3.1× too
  strict), so tables will show *more* colour after the fix, not less. Deliberate, and a NEWS entry.
- **The `2·pnorm` p is two-sided while the colour has a direction.** Direction is taken from
  `sign(contribution)`, as today; with a two-sided p at α that is the standard convention and matches
  how the CI-based measures gate (`ci_inf > 0` / `ci_sup < 0`). No inconsistency, but it is worth one
  code comment.
- **Multiplicity applies to both policies** (§3.4) and is currently unaddressed — Q3.
- **Sparse cells** inflate the residual (§3.5) — §5.5.

### 4.3 Naming: keep `"contrib"`, or rename to `"resid"` with an alias?

**Keep `"contrib"`, no alias, no rename.** With the residual living on the significance axis, what is
coloured genuinely *is* the contribution (Le Roux/Benzécri), so the name is accurate. Renaming would
break back-compat for an inaccurate gain, and an alias would create two names for one measure — a
drift risk for no benefit.

For SPSS users the answer is documentation plus **visibility of the number**, not a second vocabulary
(§9-Q2 decided): one line in `?tab` saying *"significance is the adjusted standardised residual
(SPSS's 'adjusted residual', R's `chisq.test()$stdres`), tested at `conf_level`"*; the residual shown
**in the HTML tooltip** next to the contribution it already displays; and a **`resid` display token**
so it can be printed in the cells (`display = "{pct} ({resid})"`) exactly as `ctr` can be today (§5.4).
So an SPSS user gets the familiar number, on the familiar ±2/±3 scale (§5.3), under the familiar
measure name — without a second measure or a second argument value.

### 4.4 Weights: how to get both a meaningful contribution and a meaningful p-value?

Your 1.3.1 philosophy generalises cleanly, and the answer is **exactly the package's existing rule**
(`?tab`, Phase 18s) — nothing new to invent:

> **weighted estimate + unweighted (or Kish-effective) base**

Applied here:

- **The contribution / share stays WEIGHTED** — it is an *estimate* of the population table's inertia
  decomposition, which is what a weighted CA reads. Unchanged from 1.3.1. ✔
- **The residual and its p-value use the weighted PROPORTIONS with the unweighted base `n`**, or
  `n_eff = (Σw)²/Σw²` under `options(tabxplor.kish_neff = TRUE)` — i.e. residuals divided by `√deff`.
  This is the same ladder as every CI in the package, so the legend already has vocabulary for it and
  the doc needs no new concept.

**Verified feasible with zero new plumbing:** the `n_eff` field (19th, Phase 18s) is already
populated on the grand-total cell of the total column — measured `n_eff = 10 613` vs raw `n = 13 015`
on a `runif(.3,3)`-weighted table (deff = 1.23 → residuals shrink by 1.11×), and `NA` when the option
is off, giving a natural fallback to `n`.

Remaining caveats, all documentable in one paragraph:

- **The omnibus χ² is unweighted, the shares are weighted**, so the shares do not decompose the
  reported χ² exactly. Under the "weighted estimate / unweighted inference" rule this is intentional
  and coherent (the share is a description, the p-value a test), but it must be *stated*, not left for
  a user to discover.
- **Under weights, `r_pearson ≠ sign·√(contribution)` any more** (they now sit on different bases). The
  identity survives only for unweighted tables. Harmless — nothing in the code depends on it — but it
  means the residual must be computed from its own formula rather than derived from `var`. It already
  is.
- **`test = "survey"`** (Phase 18j) makes the *omnibus* p design-based but leaves cells at best
  Kish-corrected. The affordable extra step is a first-order Rao-Scott rescale (divide residuals by
  `√(mean deff)`, which the overlay already computes); anything better needs `svyby` per cell and
  violates the "test from the aggregate" architecture. Recommend: document the limit; add the
  Rao-Scott rescale only if the survey overlay is on.

### 4.5 Storage: no new field — can the residual be derived cheaply, or should we store z in `pvalue`?

**No new field, and no field-meaning hack: store the p-value (the field stays honest) and derive
`|z| = -qnorm(p/2)` where needed.** Full reasoning in §6, including the one trap that makes the naive
version wrong (`qnorm(1 - p/2)` saturates to `Inf` for any `|z| > 8.2` — common in surveys —
whereas `-qnorm(p/2)` is exact to `|z| ≈ 37`).

The decisive fact: **z is needed only in the `guaranteed_effect` branch.** `ignore` needs nothing;
`grey_non_signif` needs only `p < α`. So the cost is one vectorised `qnorm()` on one column under one
policy — not a hot-path concern and not a white elephant.

Storing z *in* `pvalue` and recomputing the p on demand was considered and rejected: it is not faster
in any path that matters, `fmt_stars_applicable()` already means no star ever reads that field on a
contrib column (so there is nothing to gain), and it would put a z-statistic in a field named
`pvalue` — the "two meanings in one field, keyed by an attribute" pattern the Phase-17 rules forbid,
and a surprise for any user reading `$pvalue` with `mutate()`.

---

## 5. The design

### 5.1 Shape of the change

| # | change                                                                          | user-visible?                         | code                               |
|---|---------------------------------------------------------------------------------|---------------------------------------|------------------------------------|
| 1 | `contrib_pvalue()` computes the **adjusted** residual                           | yes (more cells survive the gate)     | `R/tab.R`                          |
| 2 | that residual uses the **unweighted / `n_eff`** base, from weighted proportions | yes (weighted tables stop saturating) | `R/tab.R`                          |
| 3 | `guaranteed_effect` scores `\|z\|` against a new **`zscore` break scale**     | yes (new absolute reading)            | `R/fmt_class.R`, `R/tab_classes.R` |
| 4 | `resid` display token + HTML tooltip                                            | yes (opt-in / additive)               | `R/fmt_class.R`, `R/tab_classes.R` |
| 5 | legend wording + `?tab` + vignette + NEWS                                       | yes                                   | docs                               |

No new measure, no new argument, no new option, **no new fmt field**, no cache-schema bump. One new
break scale (the 7th), one new display token — both additive on existing, whitelisted lists.

### 5.2 The residual, and where it is computed

Entirely inside `chi2_write_contrib()` (`R/tab.R` ~L5972), in the loop that already runs per eligible
column per subtable. It already holds, per cell: `xwn` (weighted count), `twn` (the total column =
row totals, `twn[n]` = grand total), `is_totrow`/`is_tottab`; `get_n()` (unweighted count) is one call
away on the same column, and the column total is the value at that column's total row. So:

```r
p_i <- rowtot / N ;  p_j <- coltot / N ;  e <- rowtot * coltot / N     # weighted proportions
z   <- (o - e) / sqrt(e * (1 - p_i) * (1 - p_j)) * sqrt(N_base / N)    # rebased to n or n_eff
pvalue <- 2 * pnorm(-abs(z))
```

where `N_base` is `n` (default) or `n_eff` (kish). Equivalently and more clearly: form the residual
from the weighted *proportions* and multiply by `√N_base`. **No new inputs, no new pass, no signature
change.** Cost O(cells), vectorised, inside an existing loop — `contrib` colouring already costs ~+97 %
over a plain build (the χ² pass itself); this is invisible beside it.

### 5.3 `guaranteed_effect`: the absolute residual scale

The score becomes the **signed adjusted residual**, read against a new, 7th break scale `residual`:

```r
score <- z                                  # signed adjusted standardised residual
gate  <- abs(z) > z_alpha                   # zscore_formula(conf_level), never a hardcoded 1.96
breaks<- color_scales(x)$residual           # additive, center 0, auto-mirrored: default c(2, 3, 4, 6)
```

**Default breaks `c(2, 3, 4, 6)`.** Rationale: 2 ≈ the 95 % rule and the SPSS/questionr convention,
3 the large-table rule (and ≈ Bonferroni at k = 20-32, §3.4), 4 and 6 for the extremes survey-sized
tables produce. Readable round numbers beat the exact quantiles `c(1.96, 2.58, 3.29, 3.89)`, and
because the *gate* is `z(conf_level)` the "significant but uncoloured" band is only
1.96 ≤ |z| < 2 at the default — invisible in practice.

**One implementation decision to make at plan time (not a maintainer question — either is defensible,
I recommend the first):**

- **Skip `offset_guaranteed_breaks()` for this scale.** Then the thresholds are literally the numbers
  the user typed: colour steps at |z| ≥ 2, 3, 4, 6. Predictable, and "what you type is what you get".
  Requires expressing the exception as a `MEASURES`/scale FIELD (e.g. `guar_offset = FALSE`), i.e. as
  data, not as a `switch` arm — rule 5 compliant.
- **Or keep the shared offset and score `|z| − z_α`.** Then thresholds are `z_α + (b_i − b_1)`, so
  with the default scale and `conf_level = 0.95` they land on 1.96 / 2.96 / 3.96 / 5.96 — the whole
  ladder shifts coherently if `conf_level` changes, and the policy's invariant ("every cell that
  clears the threshold IS coloured", Phase 14a) holds exactly. Costs a small mismatch between the
  numbers typed and the thresholds applied.

Either way `fmt_color_plan()`'s existing `sig_source == "pvalue"` branch stays (it is what supplies
`sig_pos`/`sig_neg` from the stored p), but its `guaranteed_effect` arm changes from
`score <- raw` to `score <- z`.

**Why not a `color = "resid"` measure instead?** Because it would be a second name for the same
underlying deviation, reachable only by learning a second vocabulary, and because the question it
answers ("is this cell notable?") is a *significance* question — which is what the `color_signif` axis
is for. Putting it on the policy axis keeps one measure, one legend family, and one break-scale
namespace. (Recorded in §10 so it is not re-proposed.)

### 5.4 Reading the number: the `resid` display token

`tabxplor_display_fields` (`R/fmt_class.R` ~L1193) is the whitelist of tokens usable in the composite
`display` grammar — it already contains `ctr`, so *the contribution can already be printed in cells*.
Adding `resid` beside it makes the residual printable the same way:

```r
tab(d, race, rincome, color = "contrib", color_signif = "guaranteed_effect",
    display = "{pct} ({resid})")
```

- The value is **derived, not stored**: `sign(get_ctr(x)) · -qnorm(get_pvalue(x)/2)`. There is an
  exact precedent — `ci` is in the same whitelist and is itself derived from the `ci_inf`/`ci_sup`
  bounds by a shim (`get_ci()`), not read from a field.
- Read-only: `set_num()` must reject/ignore the `resid` token (writing a residual back would mean
  writing a p-value; nothing needs it).
- Digits: 1 or 2 decimals, signed (the `signed` display path already handles `ctr`/`diff`).
- **Tooltip**: the HTML tooltip already shows the contribution (`R/tab_classes.R` ~L2215) — add the
  residual beside it. One line, no API, benefits every contrib table whether or not the user sets
  `display`.

### 5.5 Legend

Two quantities are involved, so the legend says two things:

- magnitude, per policy: *"coloured by the contribution to χ², × the mean cell contribution"*
  (`ignore`, `grey_non_signif` — unchanged) or *"coloured by the standardised residual to
  independence: ±2 / ±3 / ±4 / ±6"* (`guaranteed_effect`);
- significance: *"grey below the adjusted standardised residual threshold (±1.96 at 95 %)"*.

The `MEASURES` row already carries the magnitude wording (`ref_kind = "indep"` → "vs the mean"); the
`guaranteed_effect` line needs its own wording because the unit changes with the policy — which is
exactly the trade accepted in §4.2, and the legend is where it must be made visible to the reader.
French: *"résidu standardisé ajusté"* — one `po/R-fr.po` entry.

### 5.6 Sparse-cell guard

Recommendation (cheap, no new argument): leave cells whose **own expected count is `< 1`** ungated —
i.e. treat them as non-significant — so an `e = 0.2` cell cannot be flagged at `|z| = 6.08`. Reuses
the quantities already in the loop; `min_e` already flags the table in the p-value row descriptor
(Phase 18j's `!` marker), so the table-level signal already exists. `n_min` remains the user-facing
lever for hiding small bases entirely. Alternative: do nothing and document — but then the package
ships a known false-positive generator on sparse tables.

### 5.7 jamovi

**No `.a.yaml`/`.u.yaml` change at all.** The `color` dropdown and the Significance pane already
expose `contrib` and the three policies, so the whole feature reaches jamovi users through options
they already have. `R/jmvtab.h.R` untouched, no `prepare()` step, no cache-schema bump (nothing
stored changes shape). Worth one line in the jamovi help text for the `contrib` entry, no more.

---

## 6. Storage — the reasoning in full

`MEASURES$<m>$raw` is a closure of **one column**, and the residual's ingredients (row totals, column
totals, N) live across columns — so it must be computed at write time and stored. Three options were
weighed:

|                                    | mechanism                                           | verdict                                                                                                                                                                                                                         |
|------------------------------------|-----------------------------------------------------|---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| **A. store the p-value, derive z** | `\|z\| = -qnorm(p/2)`, sign from `sign(get_ctr(x))` | **recommended** — no new field, field name stays truthful, z needed in one branch only                                                                                                                                          |
| B. store z in the `pvalue` field   | recompute `p = 2·pnorm(-\|z\|)` on demand           | rejected — no path benefits (no star ever reads that field on a contrib column), and it puts a z in a field named `pvalue`: two meanings keyed by an attribute, and `$pvalue` read via `mutate()` would silently change meaning |
| C. a 20th fmt field                | full `/vctrs-field` pass                            | rejected — the maintainer's constraint, and A makes it unnecessary                                                                                                                                                              |

**The trap in A, stated so it is not re-discovered at implementation time:** the naive
`qnorm(1 - p/2)` saturates — `1 - p/2` is exactly `1` in double precision for `p < 2.2e-16`, i.e. for
any `|z| > 8.2`, which is routine in survey-sized tables (measured z = 11.0 on `gss_cat`). The correct
call is **`-qnorm(p/2)`** (or `qnorm(p/2, lower.tail = FALSE)`), exact down to `p ≈ 1e-300`
(`|z| ≈ 37`); beyond that `p` underflows to 0 and z is `Inf`, which the guaranteed factor
`(1 − z_α/Inf)² = 1` handles gracefully (maximum colour, correct).

Cost: one vectorised `qnorm()` per coloured column, **only** under `guaranteed_effect`. `ignore` needs
no z; `grey_non_signif` needs only `p < α`. This is not a white elephant — it is one line in one
branch, and it buys back a whole vctrs field.

---

## 7. Byte-identity and test impact

- **`color = "contrib"` with the default `color_signif = "ignore"`: byte-identical.** This is the CA
  use case and the majority of existing usage — including `color = "auto"` on count tables.
- **`grey_non_signif`**: changes (more cells survive; the old gate was 1.1–3.1× too strict). The
  *colour scale* is unchanged. Conscious regen of the contrib-gated fixtures in
  `test-color-golden.R`; NEWS entry.
- **`guaranteed_effect`**: changes the most — a different quantity on a different scale. Same regen.
  This is the one place where a user's existing table will look substantially different, so it needs
  an explicit NEWS paragraph, not a bullet.
- **Weighted contrib-gated tables**: change substantially (they were saturated at p = 0). Same regen.
- Every `diff`/`ratio`/`or` colour, every CI, every star, every unweighted `ignore` table, and every
  table that does not use `color = "contrib"`: untouched.

Failing-first fixtures, both ready-made from the measurements above (Phase-17 rule 7):
`White / $10000-14999` on `race × rincome` (adjusted −3.91 vs Pearson −1.83 — must be coloured after
the fix, grey before) for defect A; the ×12 000 population-weight table (every p = 0 before, sane
after) for defect B. Add: a direct parity test against `chisq.test()$stdres` on an unweighted table;
a kish/no-kish ratio test (`z_kish / z_raw == √(n_eff/n)`); a weight-SCALE invariance test (multiplying
every weight by a constant must not move a single p-value — the regression test for defect B); and a
cross-table comparability test for the new `guaranteed_effect` scale (the same `|z|` in two tables of
different χ² must get the same slot — the property that motivated the ruling).

---

## 8. Integration surface (for the plan step)

| file                                 | change                                                                                                                                                                                                                                                                                                                               |
|--------------------------------------|--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| `R/tab.R`                            | `contrib_pvalue()` → adjusted residual + the `n`/`n_eff` base; `chi2_write_contrib()` passes the margins and `get_n()`/`get_n_eff()`                                                                                                                                                                                                 |
| `R/fmt_class.R`                      | `fmt_color_plan()`: `guaranteed_effect` scores `z` on the `zscore` scale (+ the offset decision, §5.3); `tabxplor_display_fields` + the `resid` read arm in `get_num()`/`format()`; legend wording per policy; gettext anchor                                                                                                      |
| `R/tab_classes.R`                    | `default_color_scales()` + the `valid` scale whitelist (~L3615) gain `residual`; `set_color_breaks()`/`get_color_breaks()` docs; tooltip shows the residual (~L2215)                                                                                                                                                                 |
| `po/R-fr.po`                         | *résidu standardisé ajusté* + the new legend line                                                                                                                                                                                                                                                                                    |
| tests                                | `test-color-golden.R` (conscious regen), `test-color-config.R` (the 7th scale validates), `test-calculations.R` (parity vs `$stdres`, kish ratio, weight-scale invariance), `test-color-engine.R` (edge cases: `\|z\| = z_α`, `p = 0` → `z = Inf`, `e < 1`, cross-table comparability), `test-display-grammar.R` (the `resid` token) |
| docs                                 | `?tab` (`color_signif`'s contrib case + the weighting paragraph + the SPSS/R naming line), `?set_color_breaks` (the 7th scale), intro vignette (one paragraph), NEWS                                                                                                                                                                 |
| `.claude/skills/color-mode/SKILL.md` | the per-policy significance/scale line for `contrib`                                                                                                                                                                                                                                                                                 |
| jamovi                               | nothing (§5.7)                                                                                                                                                                                                                                                                                                                       |

---

## 9. Decisions taken (2026-08-05)

**Q1 — `guaranteed_effect`'s magnitude → the ABSOLUTE residual scale** (`|z|` against a new `residual`
break scale), not the contribution floor I had recommended. Maintainer's reasoning, and it is the
right criterion for this phase: *"we absolutely need absolute contrib / absolute residual, and z is
comparable across tables, while × mean contribution always floats with / is relative to the table.
It's closer to what SPSS users do. And if the user wants to keep the × mean contrib scale, he can use
`grey_non_signif`."* Consequence accepted: the colour units change with the policy for this measure
(§4.2), so the docs must describe the `contrib` case of `color_signif` explicitly.

**Q2 — Expose the residual: tooltip AND a printable value** (§5.4). The `resid` display token joins
`tabxplor_display_fields` beside the existing `ctr`, derived from the stored p-value like `ci` is
derived from its bounds. No `color = "resid"` measure.

**Q3 — Multiplicity: defer and document** (§3.4). Colouring stays per-cell at `conf_level`, consistent
with every CI and star in the package; the docs state the expected false-positive count and the
"±3 ≈ Bonferroni at k = 20-32" rule of thumb. `options(tabxplor.cell_adjust)` is recorded in §10 as a
designed-but-unbuilt slot, not forgotten.

Settled earlier and **not** re-opened: keep the name `contrib` (§4.3); the contribution stays weighted
while the residual uses the `n`/`n_eff` base (§4.4); no new fmt field (§6); no omnibus-conditioning
gate (§3.4); sparse cells with `e < 1` ungated (§5.6).

One micro-decision is deliberately left to the plan step because both answers are defensible and it
is pure implementation: whether `guaranteed_effect` skips `offset_guaranteed_breaks()` for the
`zscore` scale (thresholds exactly as typed — recommended) or keeps it and scores `|z| − z_α`
(thresholds shift with `conf_level`). §5.3.

---

## 10. Explicitly NOT proposed (recorded so they are not re-opened)

- **Moment-corrected residuals** — measurably equivalent to adjusted; a second option with no
  practical difference.
- **A `residual_type` argument** — one calibrated statistic, no knob.
- **A separate `color = "resid"` measure** — the absolute-z reading is reached through the
  `color_signif` axis instead (§5.3, §9-Q1). A second measure name would mean two vocabularies for
  one deviation, and the question it answers is a significance question.
- **A multiplicity option now** — deferred (§9-Q3), but the design slot is recorded:
  `options(tabxplor.cell_adjust = "none"|"BH"|"bonferroni")`, `p.adjust()` per subtable at write time,
  in the `conf_level` layer that `dev/new_colors_UI.md` W11 already reserved. ~3 lines if it is ever
  wanted, uniform across measures.
- **PEM (Cibois) / `o/e` attraction** — genuinely interesting for this package's French audience
  (`GDAtools`, `descriptio`, Trideux), but a *third* framework (neither χ²-decomposition nor test
  statistic), i.e. a new axis rather than an integration. Candidate for a later phase.
- **Fully design-based cellwise residuals** (`svyby` per cell) — violates the "test from the
  aggregate" architecture; the first-order Rao-Scott rescale is the affordable 90 %.
- **Deviance / Freeman-Tukey residuals** — model-fit tools, not crosstab practice.
- **Changing `color = "auto"` on count tables** — withdrawn from rev. 1; `contrib` + `ignore` is the
  CA reading and is right (§1.1).
- **Conditioning cell significance on a significant omnibus χ²** — contrary to current methodological
  advice (§3.4).

---

## Sources

Methodological:

- [IBM — Interpreting adjusted residuals in Crosstabs cell statistics](https://www.ibm.com/support/pages/interpreting-adjusted-residuals-crosstabs-cell-statistics) (the SPSS ±2 rule refers to the *adjusted* residual)
- [Cornell CSCU — Using Adjusted Standardized Residuals for Contingency Tables](https://cscu.cornell.edu/wp-content/uploads/conttableresid.pdf)
- [Sharpe (2015), *Your Chi-Square Test Is Statistically Significant: Now What?*, PARE 20(8)](https://files.eric.ed.gov/fulltext/EJ1059772.pdf)
- [García-Pérez & Núñez-Antón (2003), *Cellwise Residual Analysis in Two-Way Contingency Tables*, EPM 63(5)](https://journals.sagepub.com/doi/10.1177/0013164403251280)
- [García-Pérez & Núñez-Antón (2014), *Analysis of residuals in contingency tables: another nail in the coffin of conditional approaches to significance testing*](https://pubmed.ncbi.nlm.nih.gov/24788323/)
- [Bonferroni as a post hoc for a significant chi-square — tutorial](https://jfqhc.ssu.ac.ir/article-1-1182-en.html) · [Fisher's exact post hoc, PLOS ONE](https://journals.plos.org/plosone/article?id=10.1371%2Fjournal.pone.0188709)
- [survey::svychisq — Rao-Scott corrections](https://r-survey.r-forge.r-project.org/survey/html/svychisq.html) · [Rao & Scott (1984)](https://projecteuclid.org/journals/annals-of-statistics/volume-12/issue-1/On-Chi-Squared-Tests-for-Multiway-Contingency-Tables-with-Cell/10.1214/aos/1176346391.full) · [Lumley — *Tables with zeroes*](https://notstatschat.rbind.io/2022/04/17/tables-with-zeroes/)
- [questionr::chisq.residuals](https://rdrr.io/cran/questionr/man/chisq.residuals.html) (default = Pearson; `std = TRUE` = `chisq.test()$stdres`)
- [Cibois (1993), *Le PEM, pourcentage de l'écart maximum*, BMS 40](https://journals.sagepub.com/doi/10.1177/075910639304000103) · [GDAtools::pem](https://www.rdocumentation.org/packages/GDAtools/versions/1.7.2/topics/pem)
- [Barnier — *Analyse bivariée* (the French teaching idiom for residuals)](https://juba.github.io/tidyverse/04-bivarie.html)

Codebase (verified this session): `chi2_write_contrib()` / `var_contrib_ctr_signed()` /
`contrib_pvalue()` `R/tab.R` ~L5928-6080; `agg_chi2()` `R/tab-agg.R` ~L510-545; `MEASURES`,
`fmt_color_plan()` and `offset_guaranteed_breaks()` `R/fmt_class.R` ~L2700-3080;
`default_color_scales()` `R/tab_classes.R` ~L3687; colour resolution `R/tab-resolve.R` ~L185-215;
`fmt_stars_applicable()` `R/fmt_class.R` ~L1572; `n_eff` availability on the grand-total cell
confirmed empirically.
