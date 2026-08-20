# The colour ladders — what a break means, and why the `base × measure` grid is out of step

## 0. What this document is for

Phase **22b-xiv-2**. `dev/reg_family_measure_effect.md` §7 measured that the three ladders a percentage column can be read on — `pct_diff`, `pct_ratio`, `odds_ratio` — disagree by a factor of six in mean intensity, and that a marginal risk-ratio table on a common outcome comes out **100 % grey**. It stopped at propositions P5, P6 and P7.

This document proposes the architecture. Its claim is that **the ladders are not badly chosen — they are undeclared**: each is a literal vector with no statement of what quantity it grades, at what anchor, or by what shape, and the three that were written at different times were transposed at different, unstated anchors. Once those three facts are stated, every ladder in the package follows from **one number per scale**, the shipped odds-ratio ladder is reproduced exactly, and the remaining questions (the under side, `mean_diff`'s missing rung, the ratio's decimals) each have an answer with evidence rather than a preference.

**Scope.** The break ladders (`COLOR_SCALES`), how a column selects one (`EST_SCALES$ladder` × `MEASURES$scale`), and the rendering precision of a multiplicative cell. It says nothing about the estimand surface (Phase 22b-xiv-1, `dev/reg_estimand_api_redesign.md`), the palettes, or the significance policies.

**Why it is on the critical path.** 22b-xiv-1's decision **P8** makes `ratio` the prediction default for every logit-family outcome. Under the shipped `pct_ratio` ladder that default renders the flagship table grey. This phase is what makes P8 usable, so it should land **before or with** it.

**Prerequisites.** `dev/reg_family_measure_effect.md` §7 (the first measurements) and `CLAUDE.md § tabxplor architecture` → *The colour system*. Per-file design is in the headers of `R/tab_classes.R` (the ladders, `COLOR_SCALES`) and `R/fmt_class.R` (`MEASURES`, `EST_SCALES`, the selection engine).

**Status.** A proposal for review. §11 lists what is decided, what is recommended and what is left open.

---

## 1. The evidence base

Every figure below was measured against the working tree at `d6d4eb5`; none is recalled. Four corpora:

| corpus                       | what it is                                                             |     n |
|------------------------------|------------------------------------------------------------------------|------:|
| crosstab percentage cells    | 7 tables (gss ×4, `Arrests`, hdv ×2), reference bases 0.5 %–82.9 %     |   120 |
| crosstab mean cells          | 8 tables (gss, hdv, `Salaries`, `Arrests`), Glass Δ against the total  |    85 |
| regression columns           | 10 tables: OR, marginal RR, marginal RD, poisson IRR, gaussian β       |   131 |
| regression additive cells    | gaussian β and poisson AME, standardized by SD(Y)                      |    50 |

Throughout, **u** is the *relative deviation* of a cell from its reference: `u = |cell − ref| / ref`. It is the one quantity both sides of a multiplicative ladder are really about, and naming it is what makes the rest of this document short.

---

## 2. The diagnosis: four facts that were never stated

### 2.1 A ladder grades a quantity, and no scale says which one

`COLOR_SCALES` declares `center`, `strict`, `std`, `settable`, `default`, `legacy`, `alias`, `derive`. It does **not** declare *what the numbers in `default` are measurements of*. So `pct_ratio = c(NA, 1.5, 2, 4)` and `mean_diff = c(0.2, 0.5, 0.8)` are two literals with no shared grammar, and the only way to know whether a new scale's default is well chosen is to guess.

That is the schema defect, and it is the reason for every other item below.

### 2.2 The shape is already uniform, and nobody knew

Read every shipped ladder as *"how much bigger is each rung than the previous, in the measure's own metric"* (the value itself for an additive scale, its logarithm for a multiplicative one):

| ladder         | default                | step ratios in its own metric |
|----------------|------------------------|-------------------------------|
| `pct_diff`     | 0.05 / 0.1 / 0.2 / 0.3 | ×2.00 ×2.00 ×1.50             |
| `contrib`      | 1 / 2 / 5 / 10         | ×2.00 ×2.50 ×2.00             |
| `adj_diff`     | 0.02 / 0.05 / 0.1 / 0.2| ×2.50 ×2.00 ×2.00             |
| `adj_diff_std` | 0.05 / 0.1 / 0.2 / 0.4 | ×2.00 ×2.00 ×2.00             |
| `odds_ratio`   | 1.2 / 1.5 / 2 / 4      | ×2.22 ×1.71 ×2.00             |
| `mean_ratio`   | 1.2 / 1.5 / 2 / 4      | ×2.22 ×1.71 ×2.00             |
| `adj_ratio`    | 1.1 / 1.25 / 1.5 / 2   | ×2.34 ×1.82 ×1.71             |
| `pct_ratio`    | (—) 1.5 / 2 / 4        | ×1.71 ×2.00                   |
| `mean_diff`    | 0.2 / 0.5 / 0.8        | **×2.50 ×1.60**               |
| `zscore`       | 1.96 / 2.58 / 3.89 / 6 | ×1.32 ×1.51 ×1.54             |

Eight of the ten sit between ×1.5 and ×2.5 — **each rung is about twice the previous one**. The two that do not are the two with a declared external convention: `zscore` is written in confidence levels and says so, and `mean_diff` is Cohen's 0.2 / 0.5 / 0.8.

So the package already has a shape rule. It has never been written down, which is why a new scale is a guess and why `pct_ratio` could lose a rung without anything noticing.

### 2.3 The multiplicative ladders are transpositions of the point ladder — at different, undeclared anchors

Take the reference ladder `pct_diff` = 5 / 10 / 20 / 30 points and ask what the *same cell* would print as a ratio and as an odds ratio, at a reference of p₀:

| p₀       | as a risk ratio (over side) | as an odds ratio (over side)   |
|----------|-----------------------------|--------------------------------|
| **10 %** | **1.50 / 2.00 / 3.00 / 4.00** | 1.59 / 2.25 / 3.86 / 6.00     |
| 20 %     | 1.25 / 1.50 / 2.00 / 2.50   | 1.33 / 1.71 / 2.67 / 4.00      |
| 33 %     | 1.15 / 1.30 / 1.60 / 1.90   | 1.24 / 1.53 / 2.29 / 3.46      |
| **50 %** | 1.10 / 1.20 / 1.40 / 1.60   | **1.22 / 1.50 / 2.33 / 4.00**  |
| 65 %     | 1.08 / 1.15 / 1.31 / 1.46   | 1.26 / 1.62 / 3.05 / 10.2      |

The shipped `odds_ratio` ladder is **1.2 / 1.5 / 2 / 4** — the p₀ = 50 % row, rounded. The shipped `pct_ratio` ladder is **1.5 / 2 / 4** — the p₀ = 10 % row, with one rung dropped.

§7.1 of the earlier study called the ratio ladder *"calibrated to nothing"*. It was calibrated — to a **rare-outcome** anchor, while its sibling was calibrated to a **balanced** one. Neither anchor is recorded anywhere, so nothing could have kept them in step.

Three consequences fall out of the same table and are worth keeping:

- **p₀ = 50 % is the only anchor at which an odds-ratio ladder is symmetric.** The logit is antisymmetric about 0.5, so ±d points give reciprocal odds ratios there and only there. That is why `odds_ratio` is declared symmetric and why the declaration is sound.
- **An anchor below the ladder's top rung has no under side at all.** At p₀ = 10 % a −20-point cell is negative: the transposition is `Inf`. So an anchor must exceed the largest break (0.30), which leaves p₀ = 50 % as the natural — nearly the only — choice.
- **The odds ratio is the least anchor-dependent of the three.** Its first rung moves between 1.22 and 1.33 over p₀ ∈ [20 %, 65 %]; the risk ratio's moves between 1.08 and 1.25, a factor of two. A risk-ratio ladder is *intrinsically* base-dependent, which is a fact to document rather than a defect to fix.

### 2.4 The mirror is not neutral: the two sides print different numbers for one deviation

The engine folds a multiplicative cell to a magnitude — `ratio` above the neutral, `1/ratio` below — and `mk_color_scale()` mirrors a one-sided vector onto both sides. That looks symmetric and is not:

| u (relative deviation) |  5 % | 10 % | 20 % | 30 % | 40 % | 50 % | 60 % | 75 % | 80 % |
|------------------------|-----:|-----:|-----:|-----:|-----:|-----:|-----:|-----:|-----:|
| an over cell prints ×   | 1.05 | 1.10 | 1.20 | 1.30 | 1.40 | 1.50 | 1.60 | 1.75 | 1.80 |
| an under cell prints ÷  | 1.05 | 1.11 | 1.25 | 1.43 | 1.67 | 2.00 | 2.50 | 4.00 | 5.00 |
| the mirror's bias       | 1.00 | 1.01 | 1.04 | 1.10 | 1.19 | 1.33 | 1.56 | 2.29 | 2.78 |

For the same relative deviation the under side always prints the larger number, because `1/(1−u) > 1+u`. **The bias is negligible at the first rung (1 %) and explosive at the top (×2.8 at u = 80 %).** Equivalently, a mirrored threshold *t* selects `u > t − 1` above the reference and `u > 1 − 1/t` below it:

| threshold *t*     |  1.10 |  1.20 |  1.50 |  2.00 |  4.00 |
|-------------------|------:|------:|------:|------:|------:|
| over selects u >  | 10.0 % | 20.0 % | 50.0 % | 100.0 % | 300.0 % |
| under selects u > |  9.1 % | 16.7 % | 33.3 % |  50.0 % |  75.0 % |

This is arithmetic, not data. It is measured in §5, and it is the whole of the under-side question.

### 2.5 What this costs, measured

On the 120 crosstab percentage cells (share at each intensity, |slot| 0 = uncoloured):

| ladder                          | grey   |  s1  |  s2  |  s3  |  s4  | mean \|slot\| |
|---------------------------------|-------:|-----:|-----:|-----:|-----:|--------------:|
| `pct_diff` 5/10/20/30 *(ref.)*  | 65.0 % | 13.3 | 18.3 |  2.5 |  0.8 | 0.61          |
| `odds_ratio` 1.2/1.5/2/4        | 43.3 % | 25.8 | 15.0 | 15.0 |  0.8 | 1.04          |
| `pct_ratio` (—)/1.5/2/4         | **84.2 %** | 11.7 |  3.3 |  0.8 |  0.0 | **0.21**  |

And where it matters most — the tables Phase 22b-xiv-1's P8 makes the default:

| table                                      | shipped `pct_ratio`  | its own risk-difference reading |
|--------------------------------------------|---------------------:|--------------------------------:|
| `Arrests`, marginal RR (outcome 82.9 %)     | **100 % grey**       | 20 % grey, spread over s1–s2    |
| gss `married`, marginal RR (outcome 48 %)   | **92.3 % grey**      | 30.8 / 38.5 / 30.8              |
| gss `tvhours`, poisson IRR                  | 84.6 % grey          | —                               |
| `tab()` mean ratios, 85 cells               | 71.8 % grey          | —                               |

**A live defect the same ladder causes, outside `tab_reg()` entirely.** `ratio` is the *auto* text measure of a numeric column (`MEASURES$ratio$auto_for$text = "num"`), read on `mean_ratio` = 1.2/1.5/2/4. So:

```r
tab(gss_simple, race, c(age, tvhours), na = "drop", color = TRUE)
#> mean age 49 / 44 / 39 against a total of 47 — every cell uncoloured
#> legend: "ratio (Total): /4 /2 /1.5 /1.2  x1.2 x1.5 x2 x4"
```

The `Other` cell is 7.7 years below the total — **0.45 SD**, which `color = "difference"` grades at once. As a ratio it is ÷1.20, one hundredth below the first rung. A 20 % gap in a survey mean is a large finding; a ladder whose first rung is 20 % is a ladder that fires almost never.

---

## 3. The three keys

Three statements. Together they turn ten free vectors into ten declared numbers plus one rule.

> **K1 — THE ANCHOR.** Every ladder in the package is the point ladder, written in another measure at **one reference cell of 50 %**. A binary variable at p = 0.5 has SD 0.5, so 5 / 10 / 20 / 30 points is *also* 0.1 / 0.2 / 0.4 / 0.6 SD, ×1.10 / ×1.20 / ×1.40 / ×1.60 as a ratio, and ×1.22 / ×1.50 / ×2.33 / ×4.00 as an odds ratio. The anchor is a constant of the package, never of the table — a per-table anchor would buy exactness and lose the one thing the ladders are for, which is meaning the same thing in every table.

> **K2 — THE SHAPE.** A ladder is **four rungs, each about twice the previous in the measure's own metric**, starting from a declared first rung: *the smallest deviation worth noticing on this scale*. The first rung is the only per-scale decision; the rest is the rule, rounded to a readable number.

> **K3 — THE SIDES.** A ladder mirrors when the measure's own metric is symmetric, **and only then**. A difference is symmetric (±d). A log-odds is symmetric (odds run to ∞ both ways). A **ratio of two levels is not**: for one relative deviation the two sides print different numbers (§2.4), and above the reference a percentage is capped at `1/base` while below it runs to zero.

**K1 is corroborated, not assumed.** Two independent checks:

- Transposing `pct_diff` at 50 % reproduces the shipped `odds_ratio` ladder to rounding (1.22 → 1.2, 2.33 → 2), and that ladder was written years before this analysis.
- On the 120 crosstab cells, grading `diff / SD(reference)` on the SD ladder **0.1 / 0.2 / 0.4 / 0.8** and grading `diff` on `pct_diff` **5/10/20/30** give the **same intensity for 88.3 % of cells and never differ by more than one** (100 % within ±1). The two additive ladders are one ladder in two units — as K1 says they must be.

---

## 4. What each ladder becomes

The `base × measure` grid, with the current default, the proposal, and what carries it. `ladder` is the column's declared key into `MEASURES$<m>$scale`; `first rung` is the only free number under K2.

| base (`ladder`) | measure      | scale key    | today                | proposed             | first rung is       |
|-----------------|--------------|--------------|----------------------|----------------------|---------------------|
| percentage      | difference   | `pct_diff`   | .05/.10/.20/.30      | **unchanged**        | 5 points (taught)   |
| percentage      | ratio        | `pct_ratio`  | (—)/1.5/2/4          | **1.1/1.2/1.5/2**    | u = 10 %            |
| percentage      | odds ratio   | `odds_ratio` | 1.2/1.5/2/4          | **unchanged**        | 5 points at p₀ = ½  |
| mean / count    | difference   | `mean_diff`  | 0.2/0.5/0.8          | **0.1/0.2/0.4/0.8**  | 0.1 SD = 5 pts at ½ |
| mean / count    | ratio        | `mean_ratio` | 1.2/1.5/2/4          | **1.1/1.2/1.5/2**    | u = 10 %            |
| link scale      | difference   | `log_odds`   | log(`odds_ratio`)    | **unchanged**        | derived             |
| any             | contribution | `contrib`    | 1/2/5/10             | **unchanged**        | 1× the mean         |
| any             | z            | `zscore`     | conf. levels         | **unchanged**        | declared exception  |
| gap, multipl.   | adjustment   | `adj_ratio`  | 1.1/1.25/1.5/2       | under side only (§5.6)| u = 10 % (literature)|
| gap, additive   | adjustment   | `adj_diff`   | .02/.05/.10/.20      | **unchanged**        | 2 points            |
| gap, additive   | adjustment   | `adj_diff_std`| .05/.10/.20/.40     | **unchanged**        | 0.05 SD             |

Six of eleven rows do not move. The three that do are the three §2.3 shows were never anchored.

### 4.1 `pct_ratio` and `mean_ratio` → 1.1 / 1.2 / 1.5 / 2

**The first rung is forced.** The case that motivates the phase — a marginal risk ratio on a common outcome — has a maximum RR of 1.145 on `Arrests`. *Any* first rung above 1.14 leaves that table entirely grey. 1.1 is also the transposition of the 5-point rung at the anchor, and the number `adj_ratio` already uses for the epidemiological change-in-estimate rule; two independent derivations landing on one value.

**The rest is K2**, doubling in log from 1.1 — 1.10 / 1.21 / 1.46 / 2.14 — rounded to the package's own round numbers. It reads in words: *10 % more, 20 % more, half again, twice*.

Measured (share uncoloured / share at the deepest slot):

| corpus                          | shipped     | 1.1/1.2/1.4/1.6 | **1.1/1.2/1.5/2** | 1.1/1.3/1.8/3 |
|---------------------------------|------------:|----------------:|------------------:|--------------:|
| crosstab, all cells             | 84 % / 0 %  | 39 % / 12 %     | **39 % / 4 %**    | 39 % / 1 %    |
| crosstab, reference base < 5 %  | 83 % / 0 %  | 21 % / 17 %     | **21 % / 12 %**   | 21 % / 0 %    |
| reg. marginal RR, common (83 %) | 100 % / 0 % | 40 % / 0 %      | **40 % / 0 %**    | 40 % / 0 %    |
| reg. marginal RR, spread (hdv)  | 25 % / 0 %  | 12 % / 75 %     | **12 % / 38 %**   | 12 % / 12 %   |
| poisson IRR                     | 85 % / 0 %  | 46 % / 0 %      | **46 % / 0 %**    | 46 % / 0 %    |
| `tab()` mean ratios (85)        | 72 % / 1 %  | 48 % / 1 %      | **46 % / 1 %**    | —             |

The grey share is set by the first rung alone, so all three candidates are identical there; the top rung only decides saturation. ×2 is the maintainer's choice (decided, §11) and is the K2 value.

**Two keys, not one scale.** `pct_ratio` and `mean_ratio` get the same default but stay two settable keys: they carry the `legacy`/`alias` names `pct_breaks` and `mean` (`get_color_breaks("mean")`), and a user may legitimately want a ratio of incomes graded differently from a ratio of shares. What is stated once is the *rule*, not the vector.

### 4.2 `odds_ratio` does not move, and that is the corroboration

1.2 / 1.5 / 2 / 4 is simultaneously the transposition at the anchor (1.22 / 1.50 / 2.33 / 4.00) and the K2 doubling from 1.2 (1.20 / 1.44 / 2.07 / 4.30). Leaving it alone is what makes the proposal a *statement of the existing design* rather than a re-tuning of it — and it keeps every odds-ratio golden byte-identical.

### 4.3 `mean_diff` → 0.1 / 0.2 / 0.4 / 0.8 SD (P7)

Cohen's 0.2 / 0.5 / 0.8 is an experimental-psychology convention for a *treatment* effect. Group differences in survey data are not that:

| corpus                                  | median | 75 % | 90 % | 95 % |  max |
|-----------------------------------------|-------:|-----:|-----:|-----:|-----:|
| crosstab mean cells, \|Glass Δ\| (85)    |   0.16 | 0.34 | 0.47 | 0.63 | 1.42 |
| regression additive cells, \|Δ\| (50)    |   0.11 | 0.26 | 0.44 | 0.49 | 1.52 |

| ladder                 | crosstab means            | regression additive       |
|------------------------|---------------------------|---------------------------|
| 0.2/0.5/0.8 *(today)*  | 56.5 / 35.3 / — / 4.7 / 3.5 | 68.0 / 26.0 / — / 4.0 / 2.0 |
| **0.1/0.2/0.4/0.8**    | **32.9 / 23.5 / 25.9 / 14.1 / 3.5** | **44.0 / 24.0 / 16.0 / 14.0 / 2.0** |
| 0.1/0.2/0.4/0.6        | 32.9 / 23.5 / 25.9 / 11.8 / 5.9 | 44.0 / 24.0 / 16.0 / 14.0 / 2.0 |
| 0.2/0.4/0.8/1.6        | 56.5 / 25.9 / 14.1 / 3.5 / 0.0 | —                         |

Three rungs also mean the second palette intensity is **never used** (`intensity_slots(3)` = 1, 3, 4), so the shipped ladder spends three of four shades and its top two on 8 % of cells.

The proposal is not a rejection of Cohen — it **keeps his outer landmarks and completes the grid**: 0.2 is his *small*, 0.8 his *large*, 0.4 replaces the off-grid 0.5, and 0.1 is added below because that is what 5 points is at the anchor (§3, K1). Sawilowsky's (2009) extension of Cohen keeps 0.2 / 0.5 / 0.8 and adds *very small* below and *very large* (1.2) above, so a four-rung ladder that reaches under 0.2 is inside a published vocabulary rather than outside one.

⚠ `mean_diff` is read by **two** `EST_SCALES` rows — `mean_diff` (a crosstab mean difference, standardized by the reference cell's SD) and `raw_diff` (a gaussian coefficient or a count marginal effect, standardized by SD(Y)). Both move together, which is correct: they are the same ladder in the same units. The `null_default` mechanism (`std = TRUE`) is untouched; only the three numbers inside it change to four.

### 4.4 What is deliberately not touched

- `pct_diff` — the reference ladder, the one taught in the intro vignette (*"differences of less than ±5 points"*). Its top rung 0.3 is a **declared temper** of K2's 0.4; it stays, and K2's check must allow it.
- `contrib` and `zscore` — additive, symmetric, and each already declares its own convention (multiples of the mean contribution; confidence levels).
- `adj_diff` and `adj_diff_std` — already exactly K2 (×2.5/×2/×2 and ×2/×2/×2) and already anchored on stated quantities.

---

## 5. The under side

The maintainer asked: *why not a 3-rung under side, dropping the first break, which over-fires?* — and whether the rule should differ between `tab()` and `tab_reg()`, and between a ratio of means and a risk ratio. This section answers all three from the measurements.

### 5.1 The first rung is not where the sides disagree

§2.4's second table is the direct answer. A mirrored threshold of 1.10 selects `u > 10.0 %` above the reference and `u > 9.1 %` below it — a **1 % discrepancy**. The same mirror at 2.00 selects `u > 100 %` above and `u > 50 %` below — a **factor of two**. The mirror's bias is at the **top** rung, not the bottom.

So dropping the faintest under rung raises the under side's *entry* threshold from ×1.1 to ×1.2 while leaving every remaining rung mis-graded: a cell 33 % below its reference would still take the third shade where a cell 33 % above takes the second.

### 5.2 What each candidate does, measured

Over side fixed at 1.1 / 1.2 / 1.5 / 2 throughout; the figures are the **share of cells coloured at all** on each side. `U2` is the maintainer's proposal, `U3`/`U4` the u-matched ones (under rung = `1/(1−u)` for the same u as the over rung).

| under side                              | tab() all | base > 40 % | base < 20 % | tab_reg pct_ratio |
|-----------------------------------------|----------:|------------:|------------:|------------------:|
| *(the over side, for comparison)*       |    55 %   |      37 %   |      72 %   |            69 %   |
| **U1** mirror ÷1.1 ÷1.2 ÷1.5 ÷2         |    67 %   |      52 %   |      84 %   |            80 %   |
| **U2** drop faintest — ÷1.2 ÷1.5 ÷2     |    50 %   |      37 %   |      68 %   |            40 %   |
| **U3** u-matched ÷1.11 ÷1.25 ÷2         |    67 %   |      52 %   |      84 %   |            70 %   |
| **U4** u-matched + cap ÷1.11 ÷1.25 ÷2 ÷4|    67 %   |      52 %   |      84 %   |            70 %   |

And the intensity distribution on the 120 crosstab cells (over side: 45.0 / 18.3 / 23.3 / 13.3 / 0.0):

| under side | grey   |   s1 |   s2 |   s3 |  s4 |
|------------|-------:|-----:|-----:|-----:|----:|
| U1 mirror  | 33.3 % | 16.7 | 31.7 | 10.0 | 8.3 |
| U2         | 50.0 % |  0.0 | 31.7 | 10.0 | 8.3 |
| U3         | 33.3 % | 26.7 | 31.7 |  8.3 | 0.0 |
| U4         | 33.3 % | 26.7 | 31.7 |  6.7 | 1.7 |

Read together: **U2 equalises how much colour each side shows, and leaves the intensities wrong.** It buys the appearance of balance by removing a shade. **U3/U4 equalise the intensities exactly — both sides then grade the same u — and leave the coloured share where the data put it** (more cells sit well below their reference than well above; see §5.4). U4 also brings the deepest under slot down from 8.3 % to 1.7 %, matching the over side's 0 %.

### 5.3 Why the u-matched under side is naturally three-and-a-bit rungs

With the over side at u = 10 / 20 / 50 / 100 %, the under counterparts are ÷1.111 / ÷1.25 / ÷2 / **÷∞**: a cell cannot be more than 100 % below its reference. So the fourth under rung has to be decided rather than derived, and there are two honest completions:

- **U3** — three rungs, slots 1–3, and the deepest shade is an *over-side-only* shade. Purest, but a colour-first package that never reaches its darkest red below the reference is paying a real price.
- **U4** — cap the fourth rung at **÷4** (u = 75 %). Round; it is where the under side's real data reaches (measured maximum ÷4.39 over 120 cells, against a maximum of ×1.83 on the over side); and it is the same ×4 the odds-ratio ladder tops at.

**Recommended: U4** — `list(over = c(1.1, 1.2, 1.5, 2), under = c(1.111, 1.25, 2, 4))`. Three rungs matched on u, the fourth placed where the measure can actually go.

### 5.4 The over side has a ceiling; the under side does not

At a reference base *b*, the largest ratio a cell can print is `1/b`. Measured across the 120 cells:

| reference base | n  | largest ratio ABOVE | ceiling 1/b | largest magnitude BELOW |
|----------------|---:|--------------------:|------------:|------------------------:|
| < 5 %          | 24 |               ×1.36 |       ×205  |                   ÷2.61 |
| 10–20 %        | 13 |               ×1.83 |       ×6.1  |                   ÷2.49 |
| 20–35 %        | 19 |               ×1.55 |       ×4.9  |                   ÷4.39 |
| 35–50 %        | 29 |               ×1.67 |       ×2.8  |                   ÷1.95 |
| > 50 %         | 35 |               ×1.33 |       ×2.0  |                   ÷1.69 |

No cell of the corpus fell in the 5–10 % band. Over-side quantiles: median ×1.12, 90th ×1.54, **maximum ×1.83**. Under-side: median ÷1.20, 90th ÷1.88, **maximum ÷4.39**.

So in real `tab()` crosstabs the top over rung of ×2 is **never reached** — the deepest over shade is unused there — while the under side reaches ÷4.4. That is not a data accident: it is `1/b`. It is also the strongest argument *against* U2, which would remove a shade from the side that has the range and leave it on the side that does not.

⚠ It is an argument the maintainer may want to weigh the other way: if the deepest over shade should fire in `tab()`, the over side's top rung must come down to about ×1.6 (the pure transposition), at the cost of saturating strong tables. §11 records it as open.

### 5.5 Does the same happen, hidden, in `tab_reg()`? And is a ratio of means different?

**Producer.** The arithmetic bias of §2.4 is identical in both producers — it is a property of `1/(1−u)`, not of how the table was built. Measured on regression `pct_ratio` columns: the mirror colours 80 % of under cells against 69 % of over cells; the u-matched under side brings it to 70 %. Same bias, same size, same fix. **No per-producer rule is warranted, and the differences that do exist are reachability, not bias:**

- in `tab()` every column has its own reference (its total row), so a table mixes several bases and several ceilings;
- in `tab_reg()` every row of a column shares **one** reference level, so one ceiling `1/p_ref` governs the whole column. When the outcome is rare that ceiling is high and the top over rung really does fire — measured 18.8 % of over cells at the deepest slot on the regression corpus, against 0 % in `tab()`.

**Scale.** A percentage is bounded above by 100 %, so a risk ratio has the ceiling `1/b`. A **mean or a count is not bounded above**, so a mean ratio and an incidence-rate ratio have no ceiling — only the arithmetic bias, which the u-matched under side removes. Measured on the 85 crosstab mean cells: largest ratio ×1.44 above, ÷1.50 below — near-symmetric, exactly as the absence of a ceiling predicts.

So: **one rule for `pct_ratio`, `mean_ratio` and `score_ratio` alike** (u-matched under side), and the percentage's ceiling shows up as *"the top over rung is harder to reach on a percentage than on a mean"*, which is a sentence for the documentation rather than a second ladder.

### 5.6 Two consequences of K3 elsewhere

- **`odds_ratio` stays mirrored**, and now for a stated reason: its metric is the log-odds, which is symmetric, so `×1.4` and `1/1.4` genuinely are the same deviation. This is the one multiplicative ladder that is *right* to mirror.
- **`adj_ratio` is a ratio of two estimates**, so K3 applies to it too — and the literature's own definition of the rule it encodes is `|adjusted − crude| / crude`, i.e. **u**. Its u-matched under side would be ÷1.11 / ÷1.33 / ÷2 / ÷4. Lower priority than the data ladders (the gap reading is a specialist one and its first rung, where the bias is 1 %, is the rung the literature fixes), but it is the same fix and should not be left inconsistent for long.

### 5.7 What must **not** be done: a `tab()`-only under side

Tempting, because it would quieten low-base crosstab columns. It should be refused, for three reasons — and the third is the one that settles it:

- it makes a break mean two different things depending on which function built the table, which is precisely what §2.3 shows goes wrong when nobody writes the anchor down;
- the same low-base column can be built by either producer (a rare outcome in `tab()`, the same rare outcome as a `tab_reg()` risk ratio), so the two would disagree about one dataset;
- **the low-base noise is already handled, by the mechanism designed for it.** Measured on three crosstabs against rare religions (54 cells with a reference base under 10 %): `color_signif = "ignore"` colours **76 %** of them; `color_signif = "grey_non_signif"` colours **33 %**, at which point low-base and ordinary cells are equally coloured (33 % against 27 %). A large relative deviation on a base of 3 % is exactly what a wide confidence interval is for, and `n_min` is the second line. Mutilating the ladder would grey those cells whether or not they are real.

---

## 6. The rendering half — P5, and a conflict to resolve

### 6.1 The measurement

A ratio's `min_digits` floor is **1**; an odds ratio's is **2** (`DISPLAY_TOKENS`). The floor applies only where the cell asks for 0 decimals, which is exactly what `REG_CELL_DIGITS` gives a `pct_ratio` column. The same seven values, rendered three ways:

```text
values           0.938   1.145   1.110   1.370   0.614   2.000   3.075
as a ratio (1)    /1.1    x1.1    x1.1    x1.4    /1.6    x2.0    x3.1
as a ratio (2)   /1.07   x1.15   x1.11   x1.37   /1.63   x2.00   x3.08
as an odds ratio 1/1.07    1.15    1.11    1.37  1/1.63    2.00    3.08
```

Three distinct marginal effects spanning 16 percentage points collapse onto two glyph-identical strings. **The rendering carries the same backwards asymmetry as the ladder**: the measure that compresses hardest is given the *fewer* decimals, and the measure that inflates is given more.

### 6.2 ⚠ It contradicts a decision already taken

Phase **22b-x** explicitly asked for one decimal — *"change the minimum digits for mean differences, pct differences, mean ratio and pct ratio to 1 too"* — and its DONE summary records `DISPLAY_TOKENS$ratio$min_digits` moving 2 → 1, together with the accepted consequence that a crosstab `display = "ratio"` column prints `×1.3` rather than `×1.29`. P5 reverses that half.

The two requests are not really in conflict about the same thing: 22b-x was about **levels and differences** in a regression parameters table (`2.72` → `2.7`, `+0.53` → `+0.5`), where one decimal is right, and the ratio rode along. A ratio is the one quantity whose information sits in the digits *after* the constant `1.`, and how many it needs depends on how far it is from 1.

Three ways to resolve it, in increasing cost:

- **R1 — floor of 2 for every multiplicative token** (`ratio` joins `or`). One line. Uniform, aligned, and a ratio then prints exactly like the odds ratio beside it. Cost: `×2.50` where 22b-x wanted `×2.5`, and the crosstab `display = "ratio"` column returns to `×1.29`.
- **R2 — the floor follows the ladder**: two decimals below the top rung (×2), one above. Principled — *"the second decimal carries the reading only while the deviation is small"* — but it gives one column two decimal counts, which the padding aligns but the eye notices.
- **R3 — the floor follows the column's own range**: two decimals when every value in the column is within [÷2, ×2], one otherwise. Uniform within a column, adaptive between them; the same kind of adaptivity `pillar` already applies to any numeric column, and it is computable in `format()`, which sees the whole vector.

**Recommended: R1**, with R3 named as the fallback if `×2.50` proves irritating in review. R1 is the only one of the three that needs no new mechanism, and *"a ratio and an odds ratio print with the same precision"* is a rule a reader can hold.

⚠ Whichever is chosen, `REG_CELL_DIGITS` (the *cell's* own `digits`) is a separate fact and 22b-x's values there — `mean_ratio = 1`, `raw_diff = 1`, `mean_diff = 1` — are not in question. Only the floor for a cell that asks for zero is.

---

## 7. Architecture

Nothing below adds a mechanism. The engine already does every hard part: `mk_color_scale()` accepts `list(over =, under =)` with independent values, `parse_color_side()` accepts an `NA` slot-skip, `fmt_color_slots()` reads `over_breaks` / `under_breaks` separately, and `legend_break_tokens()` renders each side from its own vector. **An asymmetric ladder was verified to render correctly, cells and legend, with no change to any of them** (`÷5 ÷2 ÷1.25 ÷1.11 | ×1.1 ×1.2 ×1.5 ×2`).

### 7.1 `COLOR_SCALES` gains two declared columns

| column     | meaning                                                                                     |
|------------|---------------------------------------------------------------------------------------------|
| `quantity` | what the numbers measure: points / sd / relative / log_odds / z / contrib |
| `origin`   | how the default was obtained: the first rung and its provenance, e.g. "5 points at p0 = 0.5" |

This is the schema fix of §2.1 and the cheapest part of the phase. It also gives `?set_color_breaks` something true to say per scale instead of a prose list, and it is what a future scale reads to know what its default should look like.

### 7.2 One build-time check replaces the guessing

Beside the existing `stopifnot()` walls, assert over the declared table that every ladder is K2-shaped: four rungs per side, each between **×1.5 and ×2.5** of the previous in the scale's own metric, with a declared exemption list (`zscore`, and `pct_diff`'s tempered top rung). A ladder that drifts off the grid then fails the install rather than a user's table — exactly what `zzz-fact-keys.R` does for names.

### 7.3 Should the ladders be *derived* at runtime, or literal with declared provenance?

The package already derives two scales at plan time (`log_odds` from `odds_ratio`, `adj_diff_log` from `adj_ratio`, through `COLOR_SCALES$derive` and `color_scale_resolve()`), precisely so that a user's `set_color_breaks()` reaches them. The same hook could compute `pct_ratio` and `odds_ratio` from `pct_diff` at the anchor.

- **For.** One number to tune for the whole package; the transposition cannot drift; `set_color_breaks(pct_diff = c(0.03, 0.06, 0.12, 0.25))` would move every reading of the same table coherently.
- **Against.** The exact transposition is **1.22 / 1.50 / 2.33 / 4.00**, not the round 1.2 / 1.5 / 2 / 4 — so either every odds-ratio legend and golden moves, or a rounding rule has to be invented, and a rounding rule that reproduces the shipped ladder exactly does not exist (`1.44` rounds to 1.4, not 1.5, on any reasonable grid). And a user lowering `pct_diff` usually means *"colour differences more readily"*, not *"move my odds-ratio thresholds"*.

**Recommended: literal defaults with declared provenance** (`origin`) plus §7.2's check. The derivation is stated, tested and documented; it is not executed. If runtime derivation is wanted later, the clean shape is a `derived = TRUE` flag inside the canonical scale record that `set_color_breaks()` clears and an explicit `"auto"` value restores — one boolean, no new state machine.

### 7.4 What reads these ladders, and therefore moves

| reader                                   | effect                                                                |
|------------------------------------------|-----------------------------------------------------------------------|
| `fmt_color_plan()` → `fmt_color_slots()` | the cell shades — the point of the change                             |
| `legend_break_tokens()`                  | the legend ladder; already renders the two sides independently        |
| `legend_threshold_phrase()`              | ⚠ reads `plan$over_breaks[[1]]` only — see below                      |
| `forest_plot()` / `tab_estimates()`      | gridlines come from `EST_SCALES$break_key`, so the axis moves with it |
| `fmt_scale_of()` (`R/fmt_class.R:1981`)  | reads `color_scales()[[break_key]]` for the plot's axis               |
| `tab_css()` / the print palettes         | nothing: a slot is a slot                                             |

⚠ `legend_threshold_phrase()` builds the grey-note threshold from the **over** side's first break alone, so under an asymmetric ladder it would name one side of two. One line: name both, or write it as "±".

### 7.5 Documentation that states a now-false thing

- `vignettes/tabxplor-programming.Rmd:91` names `ratio` *"the ×2 rule"*. Under the proposal the ratio ladder starts at ×1.1 and the name has to go — one line, both languages.
- `?set_color_breaks`'s prose describes `pct_ratio` as *"the relative risk (the ×2 rule)"* and `mean_diff` as Glass's Δ *"0.2 / 0.5 / 0.8"*. Both need the new numbers; `@param` names and order are untouched.
- The intro vignette teaches *"differences of less than ±5 points"* — still true, and under K1 it becomes the sentence the whole system hangs on. Worth one added clause naming the anchor: *"the same 5 points, written as a ratio at a 50 % reference, is ×1.1"*.

### 7.6 What moves in the fixtures

- **Goldens.** Any case with `color = "ratio"`, `color = TRUE` on a numeric column, or `color = "difference"` on a mean column. `_golden/*.rds` store fields and attributes, not slots, so most should be unaffected; `_snaps/golden.md` carries rendered colour and **will** move. Review the diff by ladder, not by table.
- **`_snaps/render-html.md`** if R1 lands (the ratio tooltips gain a decimal).
- **Tests.** `test-color-legend.R` and any test pinning `get_color_breaks()` values. A new test should pin K2 (§7.2) and the u-matched under side, so neither can be re-guessed.
- **No regression golden builds a `tab_reg()` table**, so the regression side is fixture-blind — which is why §1's corpora had to be built by hand and why `dev/breaks_balance_probe.R` should be extended to carry them (the four corpora, the per-side profiles of §5.2, and the reachability table of §5.4).

---

## 8. Adjacent defects found while measuring — reported, not fixed here

1. **`set_color_breaks(get_color_breaks())` is not a round trip, and one of its losses changes numbers.** `get_color_breaks()` returns bare magnitudes, dropping both the slot-skip marker and the `std` flag. Measured: `pct_ratio`'s slots go 2/3/4 → 1/3/4, and **`mean_diff$std` goes `TRUE` → `FALSE`** — so a call that looks like a no-op silently converts the standardized (Glass Δ) ladder into an absolute one, and `0.2` stops meaning 0.2 SD and starts meaning 0.2 years. The roxygen explicitly promises the round trip (*"so it round-trips"*). The proposal removes one half of it by giving `pct_ratio` four rungs; the `std` half needs its own fix.
2. **The auto colour measure of a numeric column is `ratio`** (§2.5). With the ladder repaired the default table stops being grey, but the deeper question stands: a percentage column defaults to `difference` and a numeric one to `ratio`, which is an inconsistency in the same `base × measure` grid this phase is about. A Glass Δ is comparable across variables and a ratio of means is not; `difference` would also make *"the default colour measure is the difference, everywhere"* true. A user-facing default change, so it is named rather than proposed.
3. **`?set_color_breaks` says "1 to 5 values"**; `parse_color_side()` aborts above 4 (*"at most 4 color breaks per side"*). Documentation bug.
4. **The internal canonical shape is not accepted by the setter**: `set_color_breaks(default_color_scales())` aborts. Not user-facing, but it means the two shapes cannot be interconverted, which is what defect 1 is a symptom of.

---

## 9. A note on the idea this document does *not* propose

The reciprocal notation is what makes a ratio near 1 unreadable: `÷1.07` spends four characters to say *"7 % less"*. Writing a ratio as a **relative change** — `+14 %`, `−6 %` — would be readable at exactly the regime where P8's default lives, would be symmetric in appearance and in the quantity (`u`), would make the ladder legend read `±10 % ±20 % ±50 %`, and is the sentence a sociologist writes.

It is not proposed, for three reasons, recorded so it is not re-invented blind:

- **`+14 %` collides with percentage points.** A `points` cell already prints `+18.5%`, so on the one column type where both readings exist the reader could not tell 14 points from 14 % relative. Fixing that means renaming the point difference's unit everywhere (`+18.5 pts`), which touches every crosstab, every golden and both vignettes.
- It degrades where the ratio does not: `+200 %` is worse than `×3`.
- The `×` / `÷` glyph pair is a taught tabxplor signature, shared by the cells, the legend ladder, the forest axis and the print palettes.

⚠ One narrow version *is* attractive and costs almost nothing: the **gap** tokens (`{gap}`, `color = "adjustment"`), where the quantity is by definition a relative change in an estimate, there is no percentage-points collision, and *"adjustment shrank the effect by 23 %"* is the sentence the reading is for. Worth a separate look, not part of this phase.

---

## 10. Implementation order

Each step is independently verifiable and the earlier ones carry no risk.

1. **`DISPLAY_TOKENS$ratio$min_digits`** (§6, R1) — one value; `_snaps/render-html.md` moves. Settles the conflict with 22b-x first, because everything else is read against the rendered cells.
2. **`COLOR_SCALES` gains `quantity` and `origin`** (§7.1) and the K2 check (§7.2) — pure declaration, no behaviour, and it is what makes the next step reviewable.
3. **`mean_diff` → 0.1 / 0.2 / 0.4 / 0.8** (§4.3) — one ladder, two `EST_SCALES` readers; `_snaps/golden.md` moves on mean columns.
4. **`pct_ratio` and `mean_ratio` → over 1.1 / 1.2 / 1.5 / 2, under ÷1.11 / ÷1.25 / ÷2 / ÷4** (§4.1, §5.3) — the substance.
5. **`legend_threshold_phrase()`** to name both sides (§7.4).
6. **Documentation** (§7.5) in both languages, and `dev/breaks_balance_probe.R` extended to the four corpora (§7.6).
7. *Optional, same principle:* `adj_ratio`'s under side (§5.6).

---

## 11. Decided, recommended, open

**Decided by the maintainer (2026-08-21).**

- The ratio ladder's over side is **1.1 / 1.2 / 1.5 / 2** — the K2 doubling, round numbers.
- The under side is to be **studied**, with the 3-rung variant considered; §5 is that study.
- P5 (the ratio's decimals) — *"obviously"*.
- No base-rate-aware breaks; the under side weighed separately.

**Recommended here.**

- K1 / K2 / K3 as declared facts (§3), with `quantity` + `origin` columns and a build-time shape check.
- The under side **U4**: ÷1.11 / ÷1.25 / ÷2 / ÷4 — three rungs matched on `u`, the fourth at the round ÷4 where the data reaches (§5.3).
- **One rule for both producers and for both scales**; the percentage's ceiling is documented, not compensated (§5.5).
- `mean_diff` → 0.1 / 0.2 / 0.4 / 0.8 (§4.3).
- P5 as **R1**, a floor of 2 for every multiplicative token (§6.2).
- `odds_ratio`, `pct_diff`, `contrib`, `zscore`, `adj_diff`, `adj_diff_std` unchanged.

**Open, for the maintainer.**

1. **U4 or U3** — whether the deepest shade should exist below the reference at all (§5.3). U4 is recommended; U3 is the purist reading.
2. **The top over rung, once more.** Measured, ×2 is never reached in `tab()` (largest over ratio ×1.83 over 120 cells), so the deepest *over* shade is a `tab_reg()`-only shade there. Lowering it to ×1.6 would make both sides reachable in `tab()` and would saturate strong tables. Recorded because the measurement arrived after the choice (§5.4).
3. **`adj_ratio`'s under side** (§5.6) — same fix, lower urgency; in or out of this phase.
4. **Defect 2 of §8** — whether the auto colour measure of a numeric column should become `difference`. Adjacent, user-facing, and it is the last cell of the `base × measure` grid that is still inconsistent.
5. **R1's cost** — `×2.50` instead of `×2.5`; R3 (per-column range) is the fallback if that reads badly.

---

## 12. Re-running the evidence

`dev/breaks_balance_probe.R` produces §2.5's first table today. Everything else in this document comes from four corpora built the same way and should be folded into it:

- **cells** — `tab(pct = "row")` on gss `race×party3`, `relig×married`, `rincome×party3`, `marital×relig`, `Arrests` `c(colour,sex,employed,citizen)×released`, hdv `qualif×cinema`, `qualif×sport`; keep `pct`, `diff`, `ratio`, `or`, drop total rows and total columns, and recover each cell's reference base as `pct − diff`.
- **means** — the same shape on numeric column variables (gss `age`/`tvhours` by `race`/`relig`/`rincome`/`marital`, hdv `age`/`heures.tv`/`freres.soeurs` by `qualif`/`sexe`, `Salaries` `salary`/`yrs.since.phd`, `Arrests` `checks`), with Glass Δ = `diff / sqrt(get_ref_var())`.
- **regressions** — `tab_reg()` binomial OR / marginal RR / marginal RD on `Arrests`, hdv `cinema`, gss `married`; poisson IRR and gaussian β on gss; read `get_num()`, `get_diff()`, `get_ratio()`, `get_scale()`, `get_role()`, dropping reference rows.
- the slot function is `findInterval(magnitude, breaks, left.open = TRUE)` per side, which is what `fmt_color_slots()` does.

⚠ Two traps cost time and are worth recording: `purrr::imap_dfr()` over a `tabxplor_tab` iterates its **rows** under vctrs semantics, so wrap the columns in `as.list()`; and `tibble()` evaluates sequentially, so a column named `col` shadows the loop variable of the same name and every accessor after it silently reads a string.

## 13. References

- Cohen, J. (1988), *Statistical Power Analysis for the Behavioral Sciences*, 2nd ed. — 0.2 / 0.5 / 0.8.
- Sawilowsky, S. (2009), "New Effect Size Rules of Thumb", *Journal of Modern Applied Statistical Methods* 8(2) — the extension that adds a rung below 0.2 and names 1.2.
- Chen, H., Cohen, P. & Chen, S. (2010), "How Big is a Big Odds Ratio?", *Communications in Statistics* 39(4) — why an odds-ratio threshold cannot be read as a difference threshold.
- Norton, E. & Dowd, B. (2018), "Log Odds and the Interpretation of Logit Models", *Health Services Research* 53(2).
- Mickey, R. M. & Greenland, S. (1989), "The impact of confounder selection criteria on effect estimation", *American Journal of Epidemiology* 129(1) — the 10 % change-in-estimate rule `adj_ratio` encodes, defined as a relative change on the crude estimate.
