# `family` × `measure` × `effect` — vocabulary, order, facts, and what is inconsistent

## 0. What this document is for

`tab_reg()` asks the user three questions, through three arguments. This document settles **what those questions are**, **in which order they should be asked**, and **what is currently wrong or arbitrary** in how the package answers them for itself (the `"auto"` picks, the refusals, the colour ladders).

It is a **study, not a decision record**. Sections 1–6 are established: the vocabulary is settled, and every factual claim was measured against the running package. Sections 7–9 are open — they name propositions and the evidence for and against, so that a later session can weigh them freshly rather than inherit a conclusion.

**Scope.** It covers the argument *surface* and the *vocabulary*, not the estimand machinery — for that, `R/reg-estimand.R`'s header and `CLAUDE.md § tabxplor architecture` are the reference. It says nothing about `ref`, `shape`, `multiplier` (see `dev/reg_interactions_and_predictor_terms.md`) or the crude/adjusted pair (`dev/reg_crude_adjusted_and_display_integration.md`).

**Re-running the evidence.** `dev/breaks_balance_probe.R` regenerates §7's measurements; the rest of the figures are reproduced by the snippets inline.

---

## 1. The vocabulary

Settled while writing `vignettes/articles/tabxplor-all-else-equal.Rmd`, and adopted there.

| word                         | meaning                                | notes                                      |
| :--------------------------- | :------------------------------------- | :----------------------------------------- |
| **deviation**                | distance from the reference            | the umbrella; `DESCRIPTION` uses it so     |
| **difference / ratio / OR**  | three *measures* of one deviation      | subtract, divide, divide the odds          |
| **effect**                   | a deviation, attributed to a predictor | *observed* alone, *adjusted* with the rest |
| **the family's own measure** | what its arithmetic produces unasked   | `REG_ESTIMANDS[[fam]]$default`             |
| **unit**                     | reserved for `multiplier`              | per SD, per 10 — a *measure* is not a unit |
| **base**                     | what an estimate sits on               | `{base}` — never call it a "level"         |

---

## 2. The order: `family` × `measure` × `effect`

**The three questions, in the order a user can actually answer them:**

1. **`family`** — what kind of number is the outcome? (a share, a quantity, a count, a choice, a rank)
2. **`measure`** — which kind of deviation do you want? (subtract, divide, divide the odds)
3. **`effect`** — should the model fit that deviation directly, or work it out from the family's own measure?

**Why measure comes second and effect third:**

- `measure` is a question about **what you want to know**, and it can be answered from a cross-table with no model at all.
- `effect` is a question about **how to obtain it**, and it cannot be asked until a model exists.
- The package already leans this way: **`reg_measures()` prints measure-major** (`expand.grid(effect = …, measure = …)` varies effect fastest), and the reference vignette's grid already puts measure in the **rows** and effect in the **columns**. Only the signature and one sentence say otherwise.

**What `effect` really selects — where the number comes from:**

| `effect`         | where the number comes from                                        | describes                    |
| :--------------- | :----------------------------------------------------------------- | :--------------------------- |
| `"coefficient"`  | the model's own coefficients — chosen so they **are** the measure  | anybody                      |
| `"marginal"`     | from the model's predictions, for every real person, then averaged | the people actually surveyed |
| `"at_reference"` | worked out the same way, at an ideal-typical profile               | that profile alone           |

This framing is better than "which contrast" (the reference vignette's current wording) because it **explains why the coefficient row can change the fit**: "fitted directly" means "pick a model whose coefficient is this measure".

---

## 3. Measured facts

Every figure below was produced against the working tree; none is recalled.

### 3.1 The grid is sparse in both directions

Neither order is dead-end free. Available cells per family:

Reading **measure first** — which effects remain available:

| family      | `odds_ratio`  | `ratio`          | `difference`     |
| :---------- | :------------ | :--------------- | :--------------- |
| gaussian    | **none**      | all 3            | coef, at_ref     |
| binomial    | **coef only** | all 3            | all 3            |
| poisson     | **none**      | coef, at_ref     | marginal, at_ref |
| multinomial | coef, at_ref  | marginal, at_ref | marginal, at_ref |
| ordinal     | **coef only** | marginal, at_ref | marginal, at_ref |

Reading **effect first** — which measures remain available:

| family      | `coefficient`  | `marginal`     | `at_reference` |
| :---------- | :------------- | :------------- | :------------- |
| gaussian    | ratio, diff    | **ratio only** | ratio, diff    |
| binomial    | all 3          | ratio, diff    | ratio, diff    |
| poisson     | **ratio only** | **diff only**  | ratio, diff    |
| multinomial | **OR only**    | ratio, diff    | all 3          |
| ordinal     | **OR only**    | ratio, diff    | ratio, diff    |

The measure-first "none" cells are the *honest* kind (an odds ratio needs a probability; a quantity has none) and are worth teaching. Effect-first has five single-option cells.

### 3.2 "Equals the coefficient" is a property of the FITTED MODEL, not of the triple

This is the central finding.

| combination                                | equals the coefficient?           | measured                            |
| :----------------------------------------- | :-------------------------------- | :---------------------------------- |
| gaussian × marginal × diff, **additive**   | yes, exactly                      | diff `0.00e+00`                     |
| gaussian × marginal × diff, **interacted** | **no**                            | AME `8 419.81` vs coef `13 840.22`  |
| gaussian × marginal × diff, **quadratic**  | **no**                            | AME `1 126.85` vs coef `1 178.12`   |
| poisson × marginal × ratio, **additive**   | yes, exactly                      | diff `0.00e+00`                     |
| poisson × marginal × ratio, **interacted** | **no**                            | `1.3648` vs `1.1639`                |
| gaussian × at_reference × difference       | **yes, always — even interacted** | identical to the digit              |
| binomial × at_reference × odds_ratio       | yes (constant-OR by construction) | —                                   |
| binomial × at_reference × difference       | **no**                            | at_ref `−13.31` vs marginal `−7.95` |
| multinomial × at_reference × odds_ratio    | **no — another contrast**         | 2 cols *vs pivot* -> 3 *vs rest*    |

**Why:** an additive log or identity link is collapsible, so `exp(b)` factors out of the averaging. Once the predictor sits in an interaction or a non-linear `shape`, `b` varies by profile and the average is a genuinely different quantity. And a coefficient in an interacted model already *is* the effect at the other variable's reference level — which is why `at_reference` matches the coefficient more robustly than `marginal` does.

### 3.3 Reachability changed under the package's feet

22b-vi declared the redundancy statically. **22b-ix then added interactions** (`a*b` in `predictors`), which falsify it. The static rule predates the feature that breaks it.

---

## 4. The inconsistencies

### 4.1 Three mechanisms for one idea

"This combination equals the coefficient" is currently expressed three different ways:

| mechanism                | applied to                                     | behaviour                               |
| :----------------------- | :--------------------------------------------- | :-------------------------------------- |
| `status = "redundant"`   | gaussian marginal diff; poisson marginal ratio | aborts, with a teaching message         |
| `status` absent (no row) | binomial and ordinal × at_reference × OR       | aborts with "does not offer" + a list   |
| **nothing**              | gaussian × at_reference × difference           | **builds, duplicating the coefficient** |

### 4.2 Two of the three are misapplied

- **The redundancy refusal misfires.** `tab_reg()` refuses `effect = "marginal"` on an interacted or `shape`d gaussian/poisson model, saying *"returns the coefficient itself … averaging changes nothing"* — which §3.2 shows is **false** there. This is a live defect, and the refused quantity is exactly the one a user *should* report for an interacted model.
- **The backwards case.** gaussian `at_reference × difference` duplicates the coefficient *always*, yet is allowed; gaussian `marginal × difference` sometimes differs, yet is refused. `CLAUDE.md` justifies keeping at_reference by its differing `{base}` aside — but that argument applies equally to marginal, whose Constant row is a population average rather than a reference profile.

### 4.3 `?tab_reg` overstates orthogonality

`R/tab_reg.R:3777` states that `effect` and `measure` are **orthogonal**. With a sparse grid (§3.1) and an effect-keyed default (§5), they are not. Needs correcting whatever else is decided.

---

## 5. The `"auto"` measure, and a proposal that was challenged

### 5.1 What it does now

`REG_ESTIMANDS[[fam]]$default` is keyed **per family AND per effect** — so the mechanism for per-family tuning already exists; only the values are at issue. **Four of five families flip**:

| family      | coefficient | marginal                        | at_reference |
| :---------- | :---------- | :------------------------------ | :----------- |
| gaussian    | difference  | difference *(the refused cell)* | difference   |
| binomial    | odds_ratio  | **difference**                  | difference   |
| poisson     | ratio       | **difference**                  | difference   |
| multinomial | odds_ratio  | **difference**                  | odds_ratio   |
| ordinal     | odds_ratio  | **difference**                  | difference   |

The organising principle is legible once seen: **the default avoids the cell that duplicates the coefficient.** Gaussian's marginal default lands *on* the refused cell deliberately, so that a bare `effect = "marginal"` teaches rather than silently switches measure.

### 5.2 The proposition considered

Make marginal/at_reference default to **`ratio`** on binomial / multinomial / ordinal (and on gaussian), the rationale being that *"x times more likely"* is the sentence a reader wants, and that a ratio is the readable counterpart of a log-odds coefficient.

### 5.3 Why it was challenged — measured

On a **common outcome** a risk ratio is compressed toward 1. On `Arrests` (baseline **82.9 %** released), three marginal effects spanning 16 points collapse onto one printed value:

| row            | marginal difference | risk ratio (raw) | as printed |
| :------------- | ------------------: | ---------------: | :--------- |
| Black          |         −5.2 points |            0.938 | `÷1.1`     |
| employed = Yes |    **+10.9 points** |             1.14 | `×1.1`     |
| citizen = Yes  |         +8.1 points |             1.11 | `×1.1`     |

Two consequences, both measured: the printed value stops discriminating, and — because the shipped `pct_ratio` ladder starts at **×1.5** — **100 % of that table's cells fall in the uncoloured slot**. A colour-first package would render its flagship table grey by default.

**The asymmetry is what decides it.** A difference never *understates*: a small difference on a rare outcome is honestly small. A ratio understates badly whenever the outcome is common, and survey outcomes usually are. So `difference` is the safer default, and *"use `measure = "ratio"` when the outcome is rare"* is a **documentation** fix rather than a default change.

⚠ Two of the three symptoms are **not** the default's fault and are fixable independently — see §7. Fixing them weakens (but does not remove) the objection.

---

## 6. Real-world use cases, in a sociologist's terms

Asked because the technical answer ("AME vs MER") is not the useful one.

### 6.1 When a quantitative sociologist wants `effect = "marginal"`

1. **The headline number for an abstract.** *"Net of education, income and age, women are 5 points less likely to …"* — a quantity in the unit the discipline argues in, describing the population actually surveyed.
2. **Because odds ratios are not comparable between models.** Mood (2010) is the standard sociological warning: an OR moves when you add a covariate even with nothing to confound, so a crude-vs-adjusted OR comparison is partly arithmetic. Marginal effects on the probability scale are comparable across nested models — which matters enormously for `empirical = TRUE`, the package's distinctive feature.
3. **Commensurability across outcomes.** A 5-point effect on marriage and a 5-point effect on voting can be set beside each other. Their odds ratios cannot.
4. **Counting people.** 5 points on a 20 000-person survey is 1 000 people. An odds ratio supports no such sentence, and impact arguments need one.
5. **When the outcome is common** — the regime where an OR most overstates.

### 6.2 When a sociologist wants `effect = "at_reference"`

1. **Ideal types and portraits.** Describing a *specific social figure* — a working-class woman with no diploma in a rural area — and giving her predicted probability. This is close to a long-standing sociological habit of constructing types, and Long & Freese teach it as "ideal types": predicted probabilities for substantively interesting hypothetical individuals.
2. **When the average describes nobody.** Williams (2012): *averages can obscure differences in effects across cases*. If education matters hugely among the young and not at all among the old, the AME is a fiction and profile-specific effects are the honest report.
3. **Inside an interaction.** "The gender gap **among graduates**" is a profile question, not an average one.
4. **The multinomial gift, specific to tabxplor.** `at_reference` on a multinomial unlocks the **vs-rest** contrast: *what makes someone choose this option rather than any other?* — a different sociological question from *this option rather than the reference option*, and one the coefficient path cannot express. Measured: 2 columns vs the pivot become 3 columns vs the rest.
5. ⚠ **With the caveat the package can check.** A reference profile may describe almost nobody — on a four-factor `Arrests` model it is **7 people**. Read its `n` before quoting it. And with a continuous predictor the count vanishes entirely, because nobody sits at one exact value.

### 6.3 Can `at_reference` compare two OPPOSED ideal types? (open question, tested)

**Yes, and the result is more interesting than expected — but the package has no first-class way to do it.**

`ref` is what builds the profile, so two contrasting figures are two calls. Measured on `cinema ~ qualif + sexe + age` (hdv2003), reading `effect = "at_reference"`:

| profile built with `ref`                            | predicted share going to the cinema |         age slope |
| :-------------------------------------------------- | ----------------------------------: | ----------------: |
| `qualif="Cadre", sexe="Homme", age=30`              |                          **87.6 %** | −15.2 points / SD |
| `qualif="Ouvrier specialise", sexe="Femme", age=65` |                           **7.3 %** |  −4.4 points / SD |

**The striking part is that the model contains no interaction at all.** Isolating the comparison — holding `qualif`'s baseline at *Cadre* in both, and moving only the other predictors — the odds ratios come out **identical to three decimals** (`0.092`, `0.132`, `0.266`, `0.364`, …), while the point-differences do not:

| contrast, vs *Cadre*     | at a man of 30 (base 87.6 %) | at a woman of 70 (base 38.3 %) |
| :----------------------- | ---------------------------: | -----------------------------: |
| Ouvrier specialise       |             **−48.1 points** |               **−32.9 points** |
| Ouvrier qualifie         |                        −39.5 |                          −30.7 |
| Profession intermediaire |                        −15.6 |                      **−19.8** |
| Employe                  |                        −21.5 |                          −23.6 |

So **the same odds ratio is a different-sized fact in different regions of social space**, purely through the curvature of the link — and the asymmetry does not run one way: a *large* effect bites harder from a high baseline (48 vs 33 points), a *small* one from a baseline nearer 50 % (15.6 vs 19.8). That is a genuinely sociological reading — where in the social structure does this mechanism actually do work? — and it is invisible on the coefficient row, which is constant by construction.

**Do quantitative social scientists do this?** Yes, and it is a live methodological argument rather than a niche habit:

- **Long & Mustillo (2021)**, *Sociological Methods & Research* — the direct reference: comparing groups through **predictions and marginal effects on probabilities** rather than coefficients, precisely because coefficients are *"[not] unaffected by the scalar identification"* while probabilities are *"expressed in the natural metric of the outcome"*. This is the Mood (2010) scaling problem, answered with profiles.
- **Long & Freese (2014)** teach predicted probabilities for **ideal types** — substantively interesting hypothetical individuals — as a standard interpretive device.
- **Williams (2012)**: *averages can obscure differences in effects across cases*, which is the argument for reporting at representative values rather than only the AME.
- **King, Tomz & Wittenberg (2000)** made scenario-contrasts standard in political science under the name *quantities of interest*.

**Two caveats the tests exposed.**

1. ⚠ **Changing `ref` moves two things at once**: the profile the effect is *evaluated at*, and the level each contrast is *measured from*. In the first table above, profile B's rows read "vs Ouvrier specialise" while A's read "vs Cadre" — so that pair is not a clean profile comparison. To isolate the profile, hold the focal predictor's reference fixed and move only the others, as the second table does.
2. ⚠ **A profile is built, not found.** Its `n` should be read before it is quoted (§6.2, point 5), and with a continuous predictor there is no `n` at all.

**Open proposition — see P9.**

**Honest limits.** On a linear model `at_reference` is the coefficient (§3.2), so it adds only its `{base}` aside; its value is real on curved links (binomial, poisson, ordinal) and on multinomial.

---

## 7. The colour ladders — a separate, measurable problem

Discovered while challenging §5.2, and worth its own treatment because it affects `tab()` as much as `tab_reg()`.

### 7.1 The OR ladder is calibrated; the ratio ladder is not

Transposing the `pct_diff` breaks (5/10/20/30 points) at a base rate of 50 %:

| base rate                       | +5 pts | +10 pts | +20 pts | +30 pts |
| :------------------------------ | -----: | ------: | ------: | ------: |
| **as an odds ratio**, p0 = 50 % |  ×1.22 |   ×1.50 |   ×2.33 |   ×4.00 |
| **as a risk ratio**, p0 = 50 %  |  ×1.10 |   ×1.20 |   ×1.40 |   ×1.60 |

The shipped `odds_ratio` ladder is **1.2 / 1.5 / 2 / 4** — i.e. the diff ladder transposed, clearly by design. The shipped `pct_ratio` ladder is **1.5 / 2 / 4** — three breaks, calibrated to nothing, and *coarser* than the OR's despite the OR being the scale that inflates faster. That is backwards.

⚠ The correspondence is base-rate dependent (at p0 = 20 % the same points give ×1.25/1.5/2/2.5), so no single ladder is right everywhere. p0 = 50 % is the natural anchor but not a neutral one.

### 7.2 What the ladders actually do to real cells

From `dev/breaks_balance_probe.R`, 97 crosstab data cells (gss, Arrests, hdv):

| ladder                               | uncoloured |   s1 |   s2 |  s3 |  s4 |     mean |
| :----------------------------------- | ---------: | ---: | ---: | --: | --: | -------: |
| `pct_diff` 5/10/20/30 *(reference)*  |     71.1 % | 12.4 | 12.4 | 3.1 | 1.0 |     0.51 |
| `pct_ratio` 1.5/2/4 *(shipped)*      | **88.7 %** | 10.3 |  0.0 | 1.0 | 0.0 | **0.13** |
| `odds_ratio` 1.2/1.5/2/4 *(shipped)* |     55.7 % | 22.7 | 11.3 | 9.3 | 1.0 | **0.77** |

**17.5 %** of cells are coloured by the difference but left grey by the ratio. The three scales disagree by a factor of six in mean intensity.

### 7.3 Candidate ladders, and why one may not suffice

| ladder                         | crosstab, all | crosstab **under** side | reg. mRR, common |      reg. RR, strong |
| :----------------------------- | ------------: | ----------------------: | ---------------: | -------------------: |
| shipped 1.5/2/4                |   88.7 % grey |             85.3 % grey |   **100 % grey** |          12.5 % grey |
| 1.1/1.2/1.4/1.6                |        60.8 % |              **38.2 %** |           33.3 % | **87.5 % at slot 4** |
| 1.1/1.25/1.5/2 *(`adj_ratio`)* |        60.8 % |                  38.2 % |           33.3 % |       50 % at slot 4 |
| **1.15/1.35/1.75/2.5**         |    **68.0 %** |                  50.0 % |           83.3 % |     12.5 % at slot 4 |
| 1.2/1.5/2/4 *(the OR ladder)*  |        73.2 % |                  58.8 % |           83.3 % |        0 % at slot 4 |

Three things fall out:

- **The under side is the hazard.** At 1.1-based breaks only **38 %** of under-side cells stay grey against **73 %** of over-side ones — low-percentage cells produce extreme ratios, so a sensitive ladder lights up noise, exactly as suspected.
- **`1.15/1.35/1.75/2.5` best matches the `pct_diff` reference profile** for crosstabs (68.0 / 20.6 / 7.2 / 3.1 / 1.0 against 71.1 / 12.4 / 12.4 / 3.1 / 1.0).
- **No single ladder serves both.** A marginal RR on a common outcome lives in [0.83, 1.15]; a crosstab ratio in [0.2, 5]. Only 1.1-based breaks animate the first, and they saturate the second.

### 7.4 The rendering half is independent

`DISPLAY_TOKENS$ratio$min_digits` is **1**, against **2** for `or`. So a risk ratio of 1.14 prints `×1.1` while an odds ratio of 1.14 prints `1.14`. Raising the ratio's floor to 2 fixes the *readability* symptom on its own, with no ladder change and no effect on any stored value. `REG_CELL_DIGITS` gives `pct_ratio = 0`, `mean_ratio = 1`, so the floor is what is doing the work in both cases.

---

## 8. Open propositions

None of these is decided. Each names its own evidence and its own cost.

### Phase 22b-xiv-1 — The `family` × `measure` × `effect` framework: consistency, user-friendliness and how to teach it

**P1 — Make redundancy dynamic.** Decide "does this equal the coefficient?" from the fitted model (is the predictor in an interaction, or under a non-identity `shape`?) rather than from a static `REG_ESTIMANDS` row. Fixes the three wrong refusals of §4.2, and lets gaussian keep `difference` as its marginal default — it would then *build* when it differs and *refuse* when it does not, which is exactly right. **Cost:** the check must run after the fit plan is resolved; the abort message becomes conditional. **Test:** one additive/interacted pair per affected row of §3.2.
**Maintainer’s decision: authorise the currently wrong refusals, let gaussian keep difference as its marginal default, but do not make redundancy dynamic (the user will see himself nothing has changed, it’s ok, he’s an adult.) ?** But at the same time, is reopen near-useless flexibility, that is white elephants, unreadable, where the user don’t really know what he’s chosing, and is a bit lost ?

**P2 — Unify the three mechanisms** of §4.1 into one, and fix the backwards gaussian `at_reference` case. Open question: should a duplicate be *refused*, or *built with a note*? The `{base}` argument says the table differs even when the estimate does not.
**Maintainer’s decision: yes, make it consistent. Refused, or note, or let the user to his thing, I’m not sure, this needs studing, or maybe we need to find a framework when this question doesn’t have to arise ?**

**P3 — Teach `family` × `measure` × `effect` everywhere.** Signature, `@param` order, the `:3651` argument map, the reference vignette's heading and section order, `reg_measures()` column order. **Nearly free in R** — `effect`/`measure` are always passed by name (181/158 occurrences, never positionally) and the grid and `reg_measures()` rows are already measure-major. ⚠ **Caveat: jamovi.** `TABX_ESTIMANDS` nests family → effect → measures and `measureOffered(ui, effect, measure)` gates measures *given* effect; reversing means transposing the generated structure and inverting the gating, in untested JS. Worse, the effect-keyed default (§5.1) is most confusing in a *live* UI — pick a measure, pick an effect, watch the measure change under you. **Proposition: change the docs and the R surface, leave the jamovi UI effect-first, and say why.** A click-through UI and a written explanation have different constraints; that is a defensible divergence rather than a sloppy one.
**Maintainer’s decision: I’m not sure anymore**, because there is a contradiction between the statistical and sociological thinking (starting from the observed base and chosing a measure to compare on, like `tab()` colors do) and the modelisation workflow (take a family, choose a model and how to use it, derive some other measures from the coefficient if you want to). **This is fully open to study**: should the user more clearly choose a model (family + link ; and the link is the first measure), then if he needs to, choose to derive something from the coefficients (two remaining choices: average or at reference ? ; which second measure (new or same) ?) ? This is the whole technical workflow, but the aim of  `family` × `measure` × `effect` was to think about a more user-friendly, shorter pipeline, with no white elephants and useless possibilities, staying closer to the "start from observed base then choose a measure of deviation to compare on" which is tabxplor philosophy. Given all the current contradictions and inconsistencies, would there be a better way to achieve that goal ? What should be the arguments and their order and dependencies for that, and for them to be really helpful (no white elephants, no useless flexibility, meaningful readable choices) ?

**P4 — Correct `?tab_reg`'s "orthogonal" claim** (§4.3). Independent of everything else.
**Maintainer’s decision: yes.**

 **P8 — Should `difference` stays the marginal default ? + Document the rare-outcome case.** Should `difference` stays the marginal default (§5.3) ? If so,`?tab_reg` and the regression vignettes should say plainly: *when the outcome is rare, read the ratio instead*. This is the honest half of the rejected proposition.
**Maintainer’s decision: I want "ratio" as a marginal + at_reference default for all families whose base link is log(OR).** "difference" stays for poisson and gaussian marginal and at_reference default. **Document** *when the outcome is rare, read the ratio instead*, but also *when the outcome is common, think about reading the difference instead*, and finally *ratio is the right way to rightly say "all other chosen variables being equal, A is x times more likely than B to have...* (use the most precise wording if it’s not the right one) ; in base regression vignettes + `vignettes/articles/tabxplor-all-else-equal.Rmd` (use "ratio" marginals in one example in this vignette if one example is relevant for that).


### Phase 22b-xiv-2 — 8.2 The `measure` ladders balance problem
**P5 — Ratio rendering: `min_digits` 1 → 2** (§7.4). Small, isolated, fixes the `×1.1` symptom. ⚠ check the golden files: a crosstab `display = "ratio"` column and its tooltips will move.
**Maintainer’s decision: obviously.**

**P6 — Ratio ladder: restore a fourth break and recalibrate** (§7.1–7.3). The evidence supports *some* change; which is open.
- **P6a — one compromise ladder.** `1.15/1.35/1.75/2.5` best matches `pct_diff`'s profile on crosstabs. Leaves marginal RRs on common outcomes pale.
- **P6b — split the scale.** A regression risk-ratio column gets its own break key, distinct from a crosstab's `pct_ratio`. Precedent exists: `adj_ratio` is already a separate key for gap ladders. Costs one `EST_SCALES`/`COLOR_SCALES` key and a foreign-key row.
- **P6c — base-rate-aware breaks**, derived per table so a ratio always corresponds to the same point difference. Principled, and it is exactly the transposition of §7.1 — but it **breaks comparability between tables**, which the intro vignette explicitly promises for the residual scale. Probably disqualifying; recorded so it is not re-proposed blind.
- ⚠ Whatever is chosen, weigh the **under side** separately (§7.3). An asymmetric ladder is thinkable and currently unexplored — the `over`/`under` split already exists in `color_breaks`.
- ⚠ The value of the first break was left unspecified in the request; §7.3 gives the evidence for choosing it, not a decision.
**Maintainer’s decision: weigh the under side separately anyway ; no base-rate-aware breaks**

**P7 — Reconsider `mean_diff`.** It ships **three** breaks (0.2/0.5/0.8) where `pct_diff` has four. Not investigated here; the same "restore the fourth slot" question applies.
**Maintainer’s decision: to be studied more thoroughly.**

### Phase 22b-xiv-3 — 8.3 `at_reference` as a first-class way to compare ideal types ?

**P9 — A first-class way to compare ideal types.** §6.3 shows the comparison is meaningful, wanted by the literature, and currently only reachable as two separate `tab_reg()` calls whose tables the user must align by hand — with a trap (changing `ref` moves the contrast baseline as well as the profile). Options, unexplored: a `profiles =` argument taking a named list of `ref` vectors and emitting one column per profile, reusing the model-comparison layout that `list()` in `predictors` already produces; or documenting the two-call recipe and the "hold the focal reference fixed" rule and going no further. ⚠ Weigh against scope: the machinery would have to fit one model and sweep it at several grids, which `reg_marginal(at = )` already does for one.
**Maintainer’s decision: so the right call would be to take a list of ref profiles, not only just a ref vector ? And it would permits to use the current model comparison’s framework, with colors, significance gap, etc. ?** How easy would it be to implement this ? Are there caveats ? White elephants if for most models the coefficients are all the same and only the Constant moves (in which case a simpler mechanism to get the adjusted proportions/means at difference reference profiles would be a better call ?) ?



---

## 9. What a next session should not have to re-derive

- The vocabulary of §1, and the two "marginal" glosses.
- §3.2's table — the identities are exact and were measured, not argued.
- That the OR ladder is the diff ladder transposed at p0 = 50 % (§7.1). This is the key that makes the whole ladder question tractable.
- That `reg_measures()` and the reference vignette's grid are **already** measure-major, so P3 is smaller than it looks.
- That the `redundant` status predates interactions and is falsified by them (§3.3).

## 10. References

- Mood, C. (2010), "Logistic Regression: Why We Cannot Do What We Think We Can Do", *European Sociological Review* 26(1).
- Norton, E. & Dowd, B. (2018), "Log Odds and the Interpretation of Logit Models", *Health Services Research* 53(2); Norton, Dowd & Maciejewski (2019), *JAMA* 321(13).
- Williams, R. (2012), "Using the `margins` Command to Estimate and Interpret Adjusted Predictions and Marginal Effects", *Stata Journal* 12(2).
- Long, J. S. & Freese, J. (2014), *Regression Models for Categorical Dependent Variables Using Stata*, 3rd ed. — "ideal types".
- Long, J. S. & Mustillo, S. A. (2021), "Using Predictions and Marginal Effects to Compare Groups in Regression Models for Binary Outcomes", *Sociological Methods & Research* 50(3).
- King, G., Tomz, M. & Wittenberg, J. (2000), "Making the Most of Statistical Analyses", *AJPS* 44(2) — *quantities of interest*.
- Hanmer, M. & Kalkan, K. O. (2013), "Behind the Curve", *AJPS* 57(1) — observed-value vs average-case.
- Karlson, K. B. & Jann, B. (2023), "Marginal Odds Ratios", *Sociological Science* 10 — the counter-move, if an OR-flavoured collapsible measure is ever wanted.
