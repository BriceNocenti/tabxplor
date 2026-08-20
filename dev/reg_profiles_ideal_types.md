# Profiles — comparing ideal types with one fitted model

## 0. What this document is for

`dev/reg_family_measure_effect.md` §6.3 found that `effect = "at_reference"` can compare **opposed ideal types**, that the comparison is meaningful, that the literature asks for it, and that tabxplor has no first-class way to express it. Its proposition **P9** — the roadmap's **Phase 22b-xiv-3** — asks four questions:

1. is the right call a **list of `ref` profiles** rather than one `ref` vector;
2. would that let the feature ride the **model-comparison framework**, with colours and a significance gap;
3. **how easy** is it, and what are the caveats;
4. is it a **white elephant** when the coefficients do not move and only the Constant does — in which case would a simpler "adjusted proportions at several profiles" mechanism be the better call.

This document answers all four, proposes an architecture, and records what it measured. It is a **study with recommendations**, not a decision record: every proposition is numbered `R1`–`R10` in §1 and argued in place.

**Scope.** The argument surface, the column axis, the row axis, the engine, the colour, the guards and the teaching. It does **not** re-open `family` × `measure` × `effect` (that is 22b-xiv-1) nor the colour ladders (22b-xiv-2) — but §3 shows that this phase and 22b-xiv-1 need **one and the same predicate**, so they should agree on it before either is implemented.

**Re-running the evidence.** Every figure below was produced against the working tree on `questionr::hdv2003` (INSEE *Histoire de vie*); the running model is `cinema ~ qualif + sexe + age`, 1 653 complete cases, and §4.6 and §6 use its `age4` (banded) variant. The snippets are inline; none of the numbers is recalled.

---

## 1. Decision register

| # | proposition | recommendation |
|:--|:------------|:---------------|
| **R1** | A profile becomes a first-class object, named by a new `profiles =` argument | **yes** |
| **R2** | `profiles` is separate from `ref`, not a list-valued `ref` | **yes** — §4.2 |
| **R3** | Profiles are **columns** from **one fit** — the per-category pattern, not the models one | **yes** — §4.4 |
| **R4** | g-computation gains a `newdata` grid, so `at_reference` leaves `marginaleffects` | **yes** — §5.1 |
| **R5** | One declared predicate, *profile-invariance*, shared with 22b-xiv-1's P1/P2 | **yes** — §3 |
| **R6** | A `between_profiles` gap colour with stars | **no** — §6, measured: the sign reverses |
| **R7** | A declared note saying whether a between-column difference is *implied* or *measured* | **yes** — §6.3 |
| **R8** | The "portrait" table is the Constant row of the same object, not a second object | **yes** — §4.6 |
| **R9** | Eight guards, each naming its cure | **yes** — §4.8 |
| **R10** | jamovi waits: an Array-of-Groups control, after the R surface settles | **defer** — §5.4 |

**The one-sentence answer to P9.** Yes to a list of profiles, no to putting it inside `ref`, yes to the column layout, **no to the significance gap** — and it is not a white elephant, because the two combinations where profile columns would be identical are exactly the two that 22b-xiv-1 must settle anyway.

---

## 2. The question, restated

### 2.1 What a profile is

`effect = "at_reference"` evaluates the model at **one built individual**: every factor at its reference level, every number at its anchor. Weber's *ideal type* — a deliberate simplification, built to think with. Long and Freese teach exactly this device; Long and Mustillo argue for it against coefficients; King, Tomz and Wittenberg made scenario contrasts standard under the name *quantities of interest*.

Today that individual is a by-product of `ref`, there is one of them per call, and it is unnamed.

### 2.2 The trap, and what it costs

Two ideal types are two calls today, and `ref` moves **two things at once**: the level each contrast is measured **from**, and the point the model is evaluated **at**. Reproduced — the same call twice, changing only the `sexe` reference:

```text
tab_reg(hdv, "cinema", c("qualif","sexe","age"), effect = "at_reference", ref = c(sexe = "Homme"))
 9 sexe  Homme    741     0%       <- rows read "vs Homme"
10 sexe  Femme    912  +0.8%

tab_reg(hdv, "cinema", c("qualif","sexe","age"), effect = "at_reference", ref = c(sexe = "Femme"))
 9 sexe  Femme    912     0%       <- the block FLIPPED: rows now read "vs Femme"
10 sexe  Homme    741  -1.9%
```

The two tables are not comparable row by row, and nothing says so. The user must know to hold the focal reference fixed and move only the others — a rule that exists nowhere in the documentation because there is no feature to document it on.

### 2.3 The two readings a profile supports

| reading | question | what it needs |
|:--------|:---------|:--------------|
| **portrait** | what does the model predict for these four people | one number per profile |
| **effect at a profile** | is this effect bigger here or there in social space | one column per profile |

They are **not two features**: the portrait is the Constant row of the effect table (§4.6). That is the integration argument, and it is why this document proposes one object.

---

## 3. The predicate that answers the white-elephant question

### 3.1 Profile-invariance, defined

> A column is **profile-invariant** when the measure it reports is the contrast the fitted model's **link is additive in**, and the predictor enters the linear predictor **additively**.

A difference under an identity link, a ratio under a log link, an odds ratio under a logit: those are the contrasts a link holds constant by construction, so they are the same wherever you stand. Everything else bends with the curve. An interaction or a non-linear `shape` breaks the additivity clause, and then even the link's own contrast varies.

### 3.2 Measured

Same model, same rows, two profiles differing only on a third predictor (`occup`), reading the `qualif` rows only:

| family × measure at `at_reference`   | fit's link | max abs gap between the two profiles | verdict       |
|:-------------------------------------|:-----------|-------------------------------------:|:--------------|
| gaussian × `difference`              | identity   | `7.1e-15`                            | **invariant** |
| poisson × `ratio`                    | log        | `6.7e-16`                            | **invariant** |
| gaussian × `ratio`                   | identity   | `2.4e-02`                            | varies        |
| binomial × `difference`              | logit      | `1.6e-01` (16 points)                | varies        |
| binomial × `ratio`                   | logit      | `1.5e+00`                            | varies        |
| multinomial × `odds_ratio` (vs rest) | logit      | `0.34` on one row                    | varies        |

The gaussian × `ratio` row is the one that shows the predicate must read the **fit**, not the family's habit. `reg_formulas()` says why: at `effect = "coefficient"` a gaussian ratio is fitted as `mr` (log-link pseudo-ML), so it *is* `exp(b)` and invariant; at `at_reference` the fit is the plain `gaussian` **identity** link and the reported ratio is a ratio of two linear predictions, which moves with the profile. Verified on one row: `exp(coef())` of the log-link fit gives `0.9544`, the `coefficient` column gives `0.9544`, and the `at_reference` column gives `0.9443`.

So there are exactly **two** invariant cells in the whole grid: `gaussian × at_reference × difference` and `poisson × at_reference × ratio`. Under an interacted or `shape`d model, neither is invariant any more.

### 3.3 One predicate, three uses — and this is what makes the phase a net simplification

The same question is asked three times across three sub-phases, and today it has three unrelated answers:

| asked by | question | today |
|:---------|:---------|:------|
| 22b-xiv-1 **P2** | does `at_reference` duplicate the coefficient | nothing — the cell builds a duplicate |
| 22b-xiv-1 **P1** | does `marginal` duplicate the coefficient | a static `status`, wrong on interacted models |
| **this phase** | do two profiles give different columns | — |

All three are *"is this estimand invariant to where you stand?"*. One declared fact answers them: a **`link_measure`** column on `REG_FAMILIES` — the measure the link is additive in, nine values, foreign-key checked against `REG_MEASURES_VALUES`:

| fit key | `gaussian` | `binomial` | `poisson`, `quasipoisson` | `multinomial` | `ordinal` | `rr` | `rd` | `mr` |
|:--------|:-----------|:-----------|:--------------------------|:--------------|:----------|:-----|:-----|:-----|
| link's own | `difference` | `odds_ratio` | `ratio` | `odds_ratio` | `odds_ratio` | `ratio` | `difference` | `ratio` |

and one reader, `reg_profile_invariant(fit, measure, builder, additive)`, with two guards worth stating: the multinomial `vsrest` builder is **never** invariant (its contrast is category-vs-rest, which the logit is not additive in — measured above), and `additive` is FALSE as soon as the predictor appears in a `cross` or under a value-recoding `shape`.

⚠ **This is a coordination point.** If 22b-xiv-1 lands a different mechanism for the same question, the package will carry two, and they will disagree on interacted models. The predicate should be built once, by whichever sub-phase runs first.

### 3.4 The answer to the white-elephant question

The maintainer's doubt — *"for most models the coefficients are all the same and only the Constant moves"* — is exactly right for the two invariant cells, and only for them. Everywhere else the effect columns genuinely move, and often enormously: on the hdv model the class gap is **43.6 points among the young against 31.7 among the old**, and as a ratio **÷1.9 against ÷6.7**.

So the guard is `R9`'s refusal, not a redesign: under a profile-invariant estimand, `profiles` aborts and names its two cures (`measure =`, or `display = "base"` if the predictions are what is wanted). Once 22b-xiv-1 has settled P2, those two cells are already refused for a different reason, and the guard becomes free.

---

## 4. The design

### 4.1 The central move

> **`ref` says where you measure *from*. A profile says where you *stand*. Keeping them apart is what makes two profile columns comparable.**

That single separation fixes §2.2's trap by construction: no profile ever relevels a factor, so every column of the table carries the same rows, the same reference levels and the same skeleton — the model-comparison guarantee, obtained for free.

It is also why a profile need only name **what differs**. Everything it does not name falls back to `ref`'s value, so the focal predictor stays anchored, and two profiles differ in exactly the variables the user wrote.

### 4.2 The argument

```r
tab_reg(hdv, "cinema", c("qualif", "sexe", "age"), effect = "at_reference", measure = "difference",
        profiles = list("young man"   = c(sexe = "Homme", age = 30),
                        "older woman" = c(sexe = "Femme", age = 70)))
```

**Grammar — `ref`'s own, reused verbatim** through `reg_per_predictor()` (`R/reg-resolve.R:155`): a named element overrides that predictor, an unnamed one (or one named `default`) is a fallback whose *kind* is read from its value — a number or `"mean"` / `"median"` / `"min"` / `"max"` for a continuous predictor, `"first"` / `"last"` for a factor. So `c("last", age = 30)` is legal inside a profile and means what it means in `ref`. Nothing new to learn, nothing new to parse.

**Values.** A numeric is written in the variable's own units (`age = 30`); internally it becomes `30 - anchor`, because Phase 22b-viii shifted the column. A profile may name any predictor of the model, and only those.

**Naming.** Names become the column names, exactly as `predictors = list()` does. An unnamed list is auto-named from the values that differ (`"Homme, 30"`), which keeps a quick exploratory call readable without inventing labels.

**A single profile is legal**: `profiles = c(sexe = "Homme")` is one column, and `profiles = NULL` is today's behaviour — one column at the reference profile. So the argument is a **strict generalisation** of the status quo, with no special first case.

#### Why not a list-valued `ref` (R2)

The maintainer's own suggestion, and it is tempting: no new argument, and the plural of a named vector is naturally a list of them, exactly as `predictors` pluralises. Three measured objections decide against it.

- **A relevel cannot be done twice in one fit.** `ref` relevels factors and shifts numerics *before* the fit (`R/reg-resolve.R:686` and `:662`). A list of `ref`s would either need N fits — which is the two-call situation, with the trap intact — or would have to declare that in list form `ref` silently stops releveling, which is a second meaning the user cannot see.
- **The rows would stop matching.** A different baseline is a different set of contrasts and a different reference row, so the shared skeleton — the thing that makes columns comparable — breaks. §2.2 shows the block flipping.
- **`ref` is shared vocabulary with `tab()`**, where it names the reference row or column. Overloading it here would split a word the package has spent two phases unifying.

`profiles` is the runner-up name to `at =` (marginaleffects, emmeans and Stata all use `at()`). `profiles` wins on being self-documenting and on matching the row label the package already prints — *Reference profile* since 22b-viii.

### 4.3 What a profile column contains

Nothing new. It is the ordinary `at_reference` column, evaluated at its own grid:

| row | holds |
|:----|:------|
| a factor level | the effect vs the level's reference, at this profile, with its interval and stars |
| a numeric predictor | the `multiplier`-unit slope contrast, read **from that profile's own value** |
| `Constant` | the model's predicted outcome at this profile, and its own count |
| `{base}` | the adjusted prediction for that level at this profile |

Two asymmetries worth stating in the docs, because they follow from what a profile is:

- For a **factor**, the profile's value for that variable is irrelevant on its own rows — its levels are swept. It matters only on *other* variables' rows.
- For a **number**, the profile's value *does* set where its own slope is read, which is the point of a profile.

### 4.4 Layout — one fit, N columns (R3)

Profiles must **not** be modelled as several specs. A models comparison fits one model per spec; profiles share one fit and one set of coefficients. The right pattern already exists four times in the codebase — one fit, several columns — and is used for multinomial categories (`reg_columns_multinom()`), per-category AMEs (`reg_cols_ame()`), the `vs rest` contrasts (`reg_cols_vsrest()`) and the crude/model pair. `reg_marginal_column()` computes nothing; it slices one sweep. Profiles are one more outer `map()` over that slice.

The **visual** layout, however, is the model-comparison one, and it already renders correctly — a duplicate-model call was run as a shape test and produced exactly this skeleton. Filled with the measured numbers of §4.6, and with the default `{est} ({base})` cell, a two-profile table reads:

```text
   var      levels                    n          young man          older man
 1 Constant Reference profile     110-175            89.9%              37.1%
 2 qualif   Cadre                     260        0% (89.9%)         0% (37.1%)
 3 qualif   Ouvrier specialise        203  -43.6%*** (46.3%)  -31.7%*** ( 5.4%)
 4 qualif   Ouvrier qualifie          292  -36.1%*** (53.8%)  -29.9%*** ( 7.2%)
 5 qualif   Technicien                 86  -19.6%*** (70.3%)  -23.6%*** (13.6%)
 6 qualif   Profession interm.        160  -14.4%*** (75.5%)  -20.1%*** (17.0%)
 7 qualif   Employe                   594  -18.5%*** (71.3%)  -22.9%*** (14.2%)
```

Two things fall out of it and are worth stating.

The **Constant row's count is a range**, because each profile matches a different set of real people (110 and 175 here) while every other row rests on the same respondents. That is exactly the case Phase 22b-i's `n_range` token was built for, so it needs nothing new: the range prints, and the html tooltip names each profile's own count.

And the table **already says where its own reading is fragile**: `Technicien` reverses between the two columns (−19.6 against −23.6) while `Ouvrier specialise` does not (−43.6 against −31.7). That is §6.2's warning made visible, and it is why the note of §6.3 matters more than a colour would.

**Column identity.** `col_var` stays the shared outcome span (`reg_shared_col_var()`), so no border splits the profile block and the html header reads *cinema: Oui* over the profile names; `col_group` takes the profile label; the column name is the profile label, uniquified.

⚠ `set_col_group()` is called **nowhere** in the regression builders today, so `fmt_col_block()` collapses to `col_var` on every `tab_reg()` table. Profiles are the first legitimate user of that slot — which is what the attribute was defined for ("which sub-population the block belongs to").

**The `n` column.** `reg_base_n_cols()` (`R/tab-display.R:771`) emits one synthesised count column **per `col_group`**. Under `tab_vars` that is right — the groups are disjoint samples. Under profiles it is wrong: the sample is the same, so the N columns would be identical. The fix is a derived rule, three lines, and it is honest on its own terms: *a count column says how many people each row rests on, so two blocks resting on the same people get one column.* Groups whose count vectors are identical collapse to a single `n`.

**The footer.** The model-fit rows (N, AIC, LR, the checks) describe the model, not the profile, so they stay keyed to the first model column and appear once. Nothing per-profile is needed there: the profile's own count already lands on its Constant row (§4.6).

### 4.5 The row axis

One display point needs a decision. A numeric predictor's row label currently reads `per SD/15.8 (at mean/48.2)` — the unit **and** the anchor. With profiles, each column reads that slope at its own value, and a row label cannot say four things. **Recommendation:** under `profiles` the anchor clause is dropped from the row label and the unit clause stays; each column states its own profile (§4.7). Cheap, and it removes a statement that would otherwise be wrong for every column but one.

### 4.6 The Constant row is the portrait (R8)

Read across the profile columns, the `Constant` row is exactly the ideal-type table Long and Freese teach. Measured on `cinema ~ qualif + sexe + age4`, four profiles:

| adjusted P(goes to the cinema) | young man | young woman | older man | older woman |
|:-------------------------------|----------:|------------:|----------:|------------:|
| Cadre                          | 89.9 %    | 90.5 %      | 37.1 %    | 38.8 %      |
| Ouvrier specialise             | 46.3 %    | 48.0 %      |  5.4 %    |  5.8 %      |
| Ouvrier qualifie               | 53.8 %    | 55.5 %      |  7.2 %    |  7.7 %      |
| Technicien                     | 70.3 %    | 71.7 %      | 13.6 %    | 14.4 %      |
| Employe                        | 71.3 %    | 72.7 %      | 14.2 %    | 15.1 %      |

That whole grid is `display = "base"` on the same object; the `Cadre` line is the Constant row. So the "simpler mechanism to get the adjusted proportions at several reference profiles" the maintainer asks about is **not a second feature** — it is this one, read with one argument. Nothing else is needed, and a separate producer would duplicate the guards, the counts and the export path.

Two consequences worth teaching:

- **The profile's own count is already there.** `reg_constant_count()` (`R/tab_reg.R:1979`) counts the real people matching the profile when every predictor is categorical, and leaves the cell empty when a continuous predictor makes the profile a place nobody occupies. It needs one change — per column instead of per spec. Measured: the four profiles above match 110, 136, 175 and 198 real respondents, and the *Cadre* cell of the first rests on 8. That is the caveat §7 needs, supplied by the table itself.
- **On a multinomial outcome, the Constant row across profiles is the model's own cross-table**: one predicted share per outcome category per ideal type, each profile summing to 100 %. That closes the round trip the teaching article is built on — cross-table, model, cross-table.

### 4.7 Where the profile's composition is stated

The column name is a label the user chose; the composition must be readable somewhere. **Recommendation:** one line per profile appended to the table's `subtext`, and the same string as the html tooltip of the Constant cell:

```text
young man: sexe = Homme, age = 30 (qualif at its reference; 110 respondents match).
```

`subtext` is an existing, NULL-safe table attribute that every backend already prints, so this costs one composer and no plumbing.

### 4.8 Guards and refusals (R9)

Each names its cure, in the package's own style.

| guard | the message names |
|:------|:------------------|
| a profile-invariant estimand (§3) | `measure = "difference"` / `"ratio"`, or `display = "base"` for the predictions |
| `profiles` with another `effect` | sets `at_reference` and says so once — a profile indexes only that column |
| a variable that is not a predictor of this model | the model's own predictor list |
| a factor level that does not exist | the levels the variable has |
| `profiles` with `predictors = list(...)` | two column axes; run one model, or one profile |
| `profiles` with `tab_vars` | real groups and imagined ones both want the spread; pick one |
| a model with **one** predictor | a profile holds the *other* predictors, and this model has none |
| a numeric value outside the observed range | informs, not an error: an extrapolation; the count will be 0 |

### 4.9 What profiles deliberately do not get

- **No observed companion.** `at_reference` already has none — the model is conditional on one profile while the crude columns stay marginal over the whole sample — so `empirical`, `{obs}` and `color = "adjustment"` are unavailable, as documented. One tempting extension is recorded and rejected in §9.3.
- **No `marginal` and no `coefficient` arm.** A marginal effect averages over *real* people; a coefficient is profile-free by construction. A profile can only index an `at_reference` column, which is why the argument sets `effect` rather than combining with it.

---

## 5. The engine

### 5.1 The one missing primitive, and it pays for itself (R4)

`at = "reference"` **cannot reach g-computation today**: `reg_marginal()` (`R/tab_reg.R:1706`) gates the fast path on `at == "average"`, so every `at_reference` column goes through `marginaleffects` and its numerical jacobian. `REG_ESTIMANDS`' own comment says why — *"its contrast lives on a one-row profile grid that g-computation does not build"*. That is a gap in `reg_gcomp_maker()`, not a mathematical obstacle: it counterfactually rewrites **the fitted frame**, and has no `newdata`.

Its sibling already does. `reg_gcomp_baseline(fit, data, wt, newdata =)` (`R/reg-influence.R:460`) is exactly *"evaluate this fit on any frame, with the analytic jacobian its interval needs"*, and it covers `multinom` and `polr` as well as the GLM families. Giving `reg_gcomp_maker()` the same `newdata` slot is the whole of the work, and it buys three things at once:

1. **`at_reference` stops needing `marginaleffects`.** It is the only path that does. Measured: `at_reference` 0.554 s against `marginal` 0.277 s on this model — so the speed gain is about 2×, not the 25× the average case sees, but the *dependency* gain is total.
2. **Profiles cost one prediction each**, not one fit each. One fit per table, N grid evaluations.
3. **The influence functions become available for a profile**, which is the only route to an honest SE for a difference between two of them (§6.1). `marginaleffects`' numerical jacobian gives each column its own SE and no covariance.

⚠ One caveat inherited from `reg_gcomp_baseline()`: with `newdata` given it **drops the weights** (correctly — a grid row is not a respondent), and it refuses an `offset` term rather than approximating it. Both must carry over.

### 5.2 Implementation map

Roughly 300–400 lines of R plus documentation. Every item is an extension of a function that exists.

| file | function | change |
|:-----|:---------|:-------|
| `R/reg-resolve.R` | `reg_per_predictor()` | reused unchanged, once per profile |
| | `reg_resolve_fit_plan()` | a new block after U0: parse `profiles`, validate names and values |
| | `new_reg_args` / `new_reg_shared` | one `profiles` slot, carried to the builders |
| `R/tab_reg.R` | `reg_reference_grid_values()` | take an override list; keep the two arms |
| | `reg_profile_row()` | same override; already builds from `data[1L, ]` |
| | `reg_marginal()` | accept `at = <named list>` beside `"average"` / `"reference"` |
| | `reg_marginal_me()` | pass `ref_vals` in rather than compute them (already does `modifyList`) |
| | `reg_cols_ame()` / `reg_cols_vsrest()` | one outer `map()` over profiles; labels and `col_group` |
| | `reg_constant_baseline()` / `reg_constant_count()` | per profile instead of per spec |
| | `reg_stage_rows()` | drop the anchor clause under `profiles` (§4.5) |
| | `reg_color_notes()` | the implied-vs-measured note (§6.3) |
| `R/reg-influence.R` | `reg_gcomp_maker()` | the `newdata` slot (§5.1) |
| `R/reg-estimand.R` | `REG_FAMILIES` | one `link_measure` column; `reg_profile_invariant()` |
| | `reg_marginal_engine()` | `at_reference` may resolve to `"gcomp"` once the grid exists |
| `R/tab-display.R` | `reg_base_n_cols()` | collapse `col_group`s with identical counts (§4.4) |
| `R/zzz-fact-keys.R` | `TAB_FOREIGN_KEYS` | `REG_FAMILIES$link_measure` into `REG_MEASURES_VALUES` |

Untouched: `EST_SCALES`, `MEASURES`, the colour engine, `format()`, every exporter, `tab()`.

### 5.3 Cost

One fit per table, whatever the number of profiles. Each profile adds one grid evaluation per predictor — the same order as the per-level prediction grid `reg_marginal_me()` already builds. Measured proxy, four identical models (four fits, the worst case a profiles implementation must beat): 1.83 s against 0.55 s for one. Four profiles on one fit should land near the single-model figure.

### 5.4 jamovi (R10)

A list of profiles is an Array of Groups in the YAML — the shape `crosses` already uses since 22b-ix — plus a folder in `jmvtabreg-cache.R` and a bump of `JMVREG_CACHE_SCHEMA`. Nothing is inert or wrong meanwhile: without the control the module simply has one profile, which is today's behaviour. **Recommendation: ship the R surface first**, and add the control in a later `jmvtools::prepare()` batch once the argument has settled.

---

## 6. Colour, and why the gap should not be tested (R6)

**The ordinary colour needs nothing.** A profile column is an `at_reference` column, so `color = TRUE` grades each cell by its own effect exactly as it does today, on the same ladder, with the same greying and the same legend — that half of the maintainer's question is answered by building nothing. What follows is only about the *second* channel: grading one profile column against another.

### 6.1 The machinery would work — that is not the question

The gap seam is already generic. `obs` and `gap_se` are plain per-cell fields; every consumer downstream — `fmt_gap_parts()`, `fmt_adjustment_score()`, `fmt_gap_bounds()`, `fmt_gap_p()`, the legend, the forest band, the reference-column bolding — reads only the column's own scale and never asks where `obs` came from. A third gap measure is one `MEASURES` row, one arm in `measure_own_ref()`, one in `legend_ref_phrase()`, and one writer.

⚠ **But it must not reuse `between_groups`' arithmetic.** That writer adds the two legs' variances **in quadrature**, which is exact for `tab_vars` groups because they are disjoint samples — and wrong for two profiles of one fit, which are correlated through β. Measured, for the class gap of *Ouvrier specialise* at the young-man and older-man profiles:

| | estimate | SE | z | p |
|:--|---:|---:|---:|---:|
| exact, delta method on the full vcov | −0.1186 | 0.0519 | −2.28 | **0.0224** |
| quadrature (`between_groups`' formula) | −0.1186 | 0.0640 | −1.85 | **0.0637** |

The correlation between the two legs is +0.349, the quadrature SE is **1.23× too wide**, and the conclusion flips. So the honest implementation needs the influence functions of §5.1 — which is the second reason `R4` is not optional if any of this lands.

### 6.2 The reason to refuse anyway — measured, and the sign reverses

The model above contains **no interaction**. Its class effect nevertheless differs between profiles, and significantly so, purely through the curvature of the logit. Fit the model that actually *measures* effect modification and the answer changes sign:

| model | RD, young man | RD, older man | gap | p |
|:------|--------------:|--------------:|----:|---:|
| additive `qualif + sexe + age4` | −43.6 pts | −31.7 pts | **−11.9 pts** | 0.022 |
| interacted `qualif*age4 + sexe` | −28.7 pts | −44.3 pts | **+15.6 pts** | 0.211 |

And the interaction is real: `anova()` on the two fits gives `p = 0.0014`.

So a starred between-profile gap in an additive model would have told the reader, at the 95 % level, that **class bites hardest among the young** — while the model that measures it says the opposite and cannot distinguish it from zero. The significance is genuine but it is the precision of β, not evidence of modification; the quantity carries **no information at all** about effect modification when the model is additive.

This is the same scale-dependence VanderWeele and Knol describe: on the two response-scale measures the additive model even disagrees with itself about *where* class matters most — the difference says "among the young" (−43.6 vs −31.7), the ratio says "among the old" (÷1.9 vs ÷6.7) — while the odds ratio, measured, is identical to three decimals in every profile (`0.097` throughout).

**Recommendation: no `between_profiles` measure, and no test.** It is also the cheap answer: nothing is built, nothing is refused later, and the user who wants a tested comparison is pointed at `a*b` in `predictors`, which 22b-ix already ships and which measures the thing.

### 6.3 What to ship instead — a declared note (R7)

The columns still sit side by side and still invite the comparison, so the table should say what the comparison is worth. `reg_color_notes()` (`R/tab_reg.R:167`) is the existing home for exactly this kind of statement, and the fact is computable — does the model contain a cross between any profile variable and this table's predictors:

> *Differences between profile columns come from the curvature of the logit: `qualif` and `age4` are not interacted in this model, so no effect modification has been measured. Write `qualif*age4` in `predictors` to test it.*

and, when the interaction is there:

> *`qualif*age4` is in the model, so the difference between these columns is measured, not implied.*

One note, two arms, and it converts the trap into the teaching moment.

### 6.4 The simplification available beside it

`adjustment` and `between_groups` are byte-identical `MEASURES` rows except for three fields (`ref_kind`, `method`, `caveat`) and their `requires` key. A third would have been identical again. Whether or not the third is ever built, the pattern says the package holds **one gap measure with several baselines**, and the declarative architecture would state that once — a `GAP_BASELINES` table with a row per baseline (its `ref_kind`, its requirement, its method phrase, its caveat, its legend phrase) folded into one shared `MEASURES` row template. That is a net deletion, independent of this phase, and it is the shape the file is already asking for. Recorded here rather than proposed, since it belongs to whoever next touches `MEASURES`.

---

## 7. Caveats to teach

1. **A profile is built, not found.** Read its count before quoting it. The four profiles above match 110–198 respondents; their *Cadre* cells rest on 8 to 44. With a continuous predictor the count vanishes entirely, which is the honest answer rather than a gap.
2. **Between-column differences are implied unless the interaction is in the model** (§6.2). The note says which.
3. **The measure decides where the effect looks biggest.** Difference and ratio disagree on the same fit; the odds ratio says "the same everywhere" by construction. Choose the measure before reading the comparison, not after.
4. **A profile outside the data is an extrapolation.** The model will answer; the answer rests on the functional form alone.
5. **Multiplicity.** N profiles multiply the cells, and every one is tested against its own reference with no correction. The article's rule applies harder: read the pattern, quote the footer test.
6. **The average-case debate.** Hanmer and Kalkan argue for observed-value marginal effects *against* average-case profiles precisely because the average person does not exist. Profiles answer a different question — "what about this figure" — and the docs should say so rather than present them as a better AME.

---

## 8. Scope options

| option | what ships | cost | verdict |
|:-------|:-----------|:-----|:--------|
| **A** — docs only | the two-call recipe and the focal-reference rule | ~0 | the trap survives; aligned by hand |
| **B** — portrait only | a `Profiles` row block of predictions | small | cheap; cannot say where an effect is bigger |
| **C** — profile columns | §4 in full, no gap colour | ~300-400 lines | **recommended** |
| **D** — C plus the gap colour | one `MEASURES` row, a writer, influence functions | +~120 lines | refused by §6.2 |

**Option B deserves a fair hearing**, because it is genuinely attractive: it works under `effect = "coefficient"` — the default, the most common table — where a portrait block beside an ordinary odds-ratio column would be very useful, and it never raises the curvature question. It is rejected only because it is **contained in C**: with profiles as columns, the portrait is the Constant row and `display = "base"` is the whole grid, so B would be a second object answering a subset of the same question. If the maintainer prefers to land the cheap half first, B is a coherent staging of C rather than an alternative to it — the same parser, the same guards, the same `reg_gcomp_baseline()` call, without the column axis.

**Recommendation: C, staged as C1 then C2.**

- **C1** — `profiles`, the columns, the guards, the Constant row and its counts, `reg_base_n_cols()`'s collapse rule, the note of §6.3, and the documentation. Runs on `marginaleffects` as `at_reference` does today.
- **C2** — the `newdata` grid in `reg_gcomp_maker()`, which drops the dependency for the whole `at_reference` path. Independently valuable, and a prerequisite for D if D is ever revisited.

---

## 9. What competes with it, and what was rejected

### 9.1 Interactions do the measured version

`a*b` in `predictors` (22b-ix) answers the same sociological question with data rather than with a link: one row per cell of the pair against one common reference, each with its count, its observed rate and its adjusted one. For **two categorical variables it is strictly better evidence**, and §6.2 is the demonstration.

Profiles are the complement, not the rival, and the docs should position them so: they hold **many** variables at once, they accept **continuous** ones, and they cost **no degrees of freedom**. An interaction measures one pair and pays for it. A profile describes a figure and assumes the model.

### 9.2 `shape = "quartiles"` does the readable version

Turning a continuous moderator into a factor and crossing it is the package's own recommended cure, and it produces a cell table with real counts. Where a user's real question is "does class matter more for the young", that is the answer to reach for first.

### 9.3 An observed companion for a profile — rejected

Tempting, because a profile *does* have a crude counterpart: the observed rate among the people who match it. It would even be a model check — does the model reproduce the cells it claims. Measured on the four profiles above, restricted to cells with at least 10 respondents, the observed shares track the predictions loosely (young man, *Ouvrier specialise*: 60 % observed against 46 % predicted; older man, *Cadre*: 48 % against 37 %).

Rejected for this phase: the cells are small by construction — that is what an ideal type is — so an `Obs_` column would print mostly `NA` and, where it printed a number, would invite a comparison the sample cannot support. If a model-fit check on ideal types is ever wanted, it belongs with `REG_CHECKS`, not with the crude/adjusted pair.

### 9.4 Profiles as rows via the `tab_vars` machinery — rejected

Reusing the split-and-spread plumbing would give the user the orientation choice for free through `tab_spread()`. It is rejected because `reg_stage_split()` fits **one model per group**, and profiles share one fit — and because `tab_vars` means "real sub-populations", which profiles are not. Calling an imagined group a `tab_var` would make the `n` column, the group interaction test and the `between_groups` colour all quietly wrong.

---

## 10. Open decisions for the maintainer

1. **Scope**: C1 alone, C1 + C2, or B as a first stage (§8).
2. **The predicate**: does 22b-xiv-1 or this phase build `link_measure` and `reg_profile_invariant()` (§3.3). They must not both build one.
3. **The gap colour**: accept the refusal of §6.2, or ship D with the exact influence-function SE and a legend that names the curvature.
4. **The `n` column rule**: collapse identical `col_group` counts (§4.4), or give profiles a single `n` by declaring them a non-splitting axis.
5. **The row label under profiles**: drop the anchor clause (§4.5), or keep it and accept that it describes one column only.
6. **Names**: `profiles` or `at`; and whether the portrait reading is taught as *ideal types* (Weber, Long and Freese) in the vignettes, as the teaching article already does.

---

## 11. What a next session should not have to re-derive

- The profile-invariance predicate and its two invariant cells (§3.2), and that it must read the **fit's link**, not the family's habit — the gaussian × `ratio` measurement is the proof.
- That `at_reference` never reaches g-computation, and that `reg_gcomp_baseline(newdata =)` is the primitive that already does what `reg_gcomp_maker()` lacks (§5.1).
- That `between_groups`' quadrature SE is **1.23× too wide** for two profiles of one fit, and flips a p-value from 0.022 to 0.064 (§6.1).
- That an additive model's between-profile gap **reverses sign** against the interacted model that measures it (§6.2). This is the whole argument for R6.
- That `set_col_group()` is unused in the regression builders, so `col_group` is free for profiles (§4.4).
- That the portrait table is the Constant row of the same object (§4.6), so no second producer is needed.

---

## 12. References

- Weber, M. (1904), *Die "Objektivität" sozialwissenschaftlicher und sozialpolitischer Erkenntnis* — the ideal type.
- Long, J. S. and Freese, J. (2014), *Regression Models for Categorical Dependent Variables Using Stata*, 3rd ed. — predicted probabilities for ideal types.
- Long, J. S. and Mustillo, S. A. (2021), "Using Predictions and Marginal Effects to Compare Groups in Regression Models for Binary Outcomes", *Sociological Methods and Research* 50(3).
- Williams, R. (2012), "Using the `margins` Command to Estimate and Interpret Adjusted Predictions and Marginal Effects", *Stata Journal* 12(2).
- King, G., Tomz, M. and Wittenberg, J. (2000), "Making the Most of Statistical Analyses", *AJPS* 44(2) — quantities of interest.
- Hanmer, M. and Kalkan, K. O. (2013), "Behind the Curve", *AJPS* 57(1) — observed-value against average-case.
- Mood, C. (2010), "Logistic Regression: Why We Cannot Do What We Think We Can Do", *European Sociological Review* 26(1).
- VanderWeele, T. J. and Knol, M. J. (2014), "A Tutorial on Interaction", *Epidemiologic Methods* 3(1) — effect modification is scale-dependent.
- Knol, M. J. and VanderWeele, T. J. (2012), "Recommendations for presenting analyses of effect modification and interaction", *International Journal of Epidemiology* 41(2).
