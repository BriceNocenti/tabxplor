# The regression estimand API — one cascade, one derivation

**The reference document for Phase 22b-xv.** Part I is the design — the idea, the argument surface, the architecture. Part II is the implementation roadmap. Part III holds the studies, the measurements and the blast radius, for a session that needs the evidence behind a decision.

---

**PART I — THE DESIGN**  ·  the idea, the argument surface, the architecture

## 1. What this document is

Phase 22b-xiv-1 asked for a way out of a contradiction. `dev/reg_family_measure_effect.md` established the vocabulary and measured the facts, then stopped at propositions; its P3 named two workflows — *"start from the observed base, then choose a measure of deviation"* (tabxplor's philosophy) and *"take a family, choose a model, derive from the coefficients"* (the modelling workflow) — and asked for a third way that keeps the first without lying about the second.

**There is no third way to invent: the two are the two halves of one rule the code already implements and has never stated.** Once stated, it derives the whole estimand library, deletes three refusal mechanisms, and settles the argument surface for a reason rather than a preference.

**Status: BUILT.** Part I describes the framework as the package implements it (Phase 22b-xv-1); §7's register is settled, including the four decisions taken at implementation time; §8.2 is the prose that remains, with its migration table. What is still open is stated as such where it appears.

**Scope.** The estimand surface — `family`, `link`, `measure`, `effect`, their defaults, their availability and the fact tables behind them. Not the colour ladders (Phase 22b-xiv-2) nor the ideal-type comparison (22b-xiv-3), except where they bind — §8.3.

**Prerequisites.** `dev/reg_family_measure_effect.md` §1–§6 (the vocabulary and the measured facts, not restated here) and `CLAUDE.md` § *tabxplor architecture*. Per-file design is in the R headers.

---

## 2. The idea — a measure is a link

### 2.1 One vocabulary for the model and for the report

`difference`, `ratio` and `odds_ratio` are not three unrelated words. They are **one operation on three scales**: compare two numbers after transforming them by the identity, the log, or the logit. And a **link is a measure** — the one the model estimates directly. So the argument that names the model takes *the same words* as the argument that names the report, and the statistician's vocabulary never surfaces:

| the word         | glm link   | what the model's coefficient then is         |
|------------------|------------|----------------------------------------------|
| `"difference"`   | `identity` | a difference                                 |
| `"ratio"`        | `log`      | a ratio                                      |
| `"odds_ratio"`   | `logit`    | an odds ratio                                |
| `"hazard_ratio"` | `cloglog`  | a hazard ratio — not offered today, see §6.2 |

> **THE RULE.** `link` names the measure the **model estimates**; `measure` names the measure **reported**. A coefficient exists only where the two agree; otherwise the measure is applied to the model's predictions.

⚠ **That table is complete, and the completeness matters.** Those four are the *only* glm links whose coefficient names a measure of deviation. `probit`, `cauchit`, `sqrt`, `inverse` and `1/mu²` name none — a probit coefficient is a latent-scale quantity with no reportable name, which is precisely why probit models are reported through marginal effects. `cloglog` → hazard ratio is the discrete-time proportional-hazards model (Prentice & Gloeckler 1978), and after it the well runs dry. The glm spellings stay accepted silently; the package teaches its own.

`measure = "log"` is not a fifth link: it is the same estimand shown on its link scale rather than exponentiated back, which is what `reg_estimand()` already enforces (`R/reg-estimand.R:830-835`).

⚠ **The package already says this on the observed side and nowhere else.** `REG_EMPIRICAL` declares a `link` column — `"identity"` / `"log"` / `"logit"` — on **every one of its 24 crude shapes** (`R/reg-empirical.R:477-531`). The crude column has spoken this vocabulary since 22a-ii. The model column has not, which is why the model side needed 43 hand-written rows to say what the crude side says in one column.

**⚠ A vocabulary note, settled here.** The three kinds of level are **`pct` / `mean` / `count`** — not "share", not "proportion". `EST_SCALES$var_kind` **already declares exactly this fact** (`R/fmt_class.R:1736`: *"what the column summarises: `pct` | `mean` | `count` | `coef`"*, with 105 readers across `R/`), so a new `level` field with a new word would be a second name for an existing one — the defect this phase deletes. In prose the word is **"percentage"**, which is what the user-facing documentation already says (32 uses in the article, 19 in the regression vignette, against 9 for "proportion"); reserve "proportion" for the places where statistical precision needs it, such as *a difference of proportions*.

### 2.2 The prediction routes fit the model, not the measure

The second half of the rule was **already true of the package before the cascade named it**: every `effect = "marginal"` and `effect = "at_reference"` row carried the family's own `fit` key, and only the coefficient rows ever named `rr` / `rd` / `mr`.

What it was never allowed to do, and now can: run a prediction route on a **non-default** fit. A row's `fit` is `fits[[link]]` on **every** route, so `link = "ratio", measure = "difference"` is a marginal risk difference computed from the modified-Poisson fit — two axes, separately chosen. ⚠ this is why `reg_marginal()` takes the REPORTED comparison's link rather than inferring one from the family, and why `reg_formulas()` is the way to see which model actually ran.

The old surface said otherwise in prose: `vignettes/tabxplor-reg.Rmd:583` still claims *"`effect` and `measure` change the model that is fitted"*, which its own line 670 corrects 90 lines later — §8.2's first deletion.

### 2.3 A model column already carries the crosstab's own pair

Since 22b-iii every regression column stores the pair a `tab()` cell stores — an adjusted level and its reference level — and derives the other geometries from it: `reg_fill_base()` writes the adjusted prediction (`R/tab_reg.R:1528`), `reg_geometry_fields()` derives `diff` and `ratio` from the pair (`R/tab_reg.R:1581-1588`), and the guard is that neither may overwrite the column's **own estimate field**, which holds what was *fitted*.

So the estimand choice, stated at its deepest:

> A regression column holds the same two numbers a crosstab cell holds. `measure` says which geometry of that pair is **promoted to the estimate** — the one that carries the interval, the star and the colour. Everything else rides as an aside, exactly as in `tab()`.

And promoting a geometry means being able to attach an interval to it, which is what the two routes are: a **fit whose coefficient is that geometry**, or a **sweep over the model's predictions** with its delta-method / influence-function variance. Nothing else in the estimand system is a real choice.

This is the reconciliation the maintainer asked for. The tabxplor workflow ("start from the observed base and pick a measure of deviation") is **literally what the prediction routes do**; the modelling workflow ("choose a model, read its coefficient") is **literally what the coefficient route does**. They are not competing framings of one API — they are the two branches of one argument, and the current surface hides which branch the user is on.

---

## 3. The argument chain

Four arguments, and only one of them is typed by an ordinary reader. This section is the user-facing surface; §5 is the machinery behind it.

### 3.1 The chain

```text
outcome ──auto──▶ family ──auto──▶ link ──auto──▶ measure ──auto──▶ effect
```

**`"auto"` means "follow from the left".** One sentence, and it is the whole argument surface:

| argument  | the question it answers                 | `"auto"` resolves to              | who types it               |
|-----------|-----------------------------------------|-----------------------------------|----------------------------|
| `family`  | what kind of number is the outcome      | detected from the outcome         | common on numeric outcomes |
| `link`    | which measure the **model** estimates   | the family's own                  | only experts               |
| `measure` | which measure do you want **reported**? | the link's                        | **everyone**               |
| `effect`  | where is that number taken from?        | coefficient if any, else marginal | rarely; for `at_reference` |

Set any one and everything to its right re-derives. Nothing depends on an argument to its right — which is what makes the chain teachable — with exactly one documented exception, §3.4's clause, which fires only where `effect` was named explicitly.

### 3.2 What the two measures mean together

|                           | `measure` **=** the link              | `measure` **≠** the link              |
|---------------------------|---------------------------------------|---------------------------------------|
| `effect = "auto"`         | the model's own coefficient           | from its predictions, sample-averaged |
| `effect = "marginal"`     | that effect, averaged over the sample | (what auto already gives)             |
| `effect = "at_reference"` | at one profile                        | at one profile                        |
| `effect = "coefficient"`  | (what auto already gives)             | ⚠ refused, with the two cures named   |

The bottom-right refusal is the only one the chain adds, and it is a good message rather than a wall: *"the model estimates an odds ratio, so a risk difference cannot be read off its coefficients. Drop `effect`, or set `link = \"difference\"`."*

### 3.3 What is available, and why

> **What a measure needs.** A measure exists for an outcome when the outcome's **level** can be transformed by that measure's link: a **percentage** has an identity, a log and a logit, so all three; a **mean** or a **count** has no odds, so no odds ratio.
>
> **What a link needs.** `link` may name any measure the package can **fit** for that outcome, which is not always all of them — there is no identity-link multinomial here. That set is one declared field per family (§5.3).
>
> **What a route adds.** Nothing. Once the model exists, `measure` is applied to two predicted numbers, and every measure the level supports is available. A **coefficient** is the one reading that needs more: it exists only where `measure` equals `link`, because a coefficient *is* the link's measure.

Every current refusal, and every current hole, is one of those two clauses — and each says something a user can act on:

| what happens today                          | which clause                    | the message it can now give |
|---------------------------------------------|---------------------------------|-----------------------------|
| gaussian / poisson × OR → *not defined*     | no logit on a mean or a count   | unchanged, now derived once |
| multinom. / ordinal × coef × ratio or diff  | no such link is fitted here     | use `effect = "marginal"`   |
| poisson × coef × difference                 | same                            | same                        |
| gaussian / poisson × marginal → *redundant* | **none — a spurious mechanism** | deleted (§11)                |

⚠ **Two axes, and today they are collapsed into one.** Which model is fitted, and which measure is reported, are separate questions; today a prediction route always runs on the family's own fit, so a non-default link cannot be marginalised at all. §3 separates them — that is what `link` is for.

The third row is the payoff for the user: today it is a bare *"tabxplor does not offer..."* plus a menu of nine lines. Under the rule it becomes one sentence naming the cure, and the cure is the route that always works.

### 3.4 What `measure = "auto"` resolves to, and the one clause it needs

`measure = "auto"` is **the link's measure**. That is the cascade, and it makes the earlier draft's "prediction default" (decision 5 / P8) dissolve: there is no second default table, only "follow from the left".

⚠ **One clause, with its own reason: `"auto"` never lands on a PREDICTED non-collapsible measure.** A *marginal odds ratio* is a specialist quantity — Karlson & Jann had to write a paper to define it (§10) — so it must be asked for by name, never arrived at by default. Where the link's measure is non-collapsible **and** the user named a prediction route (`marginal` *or* `at_reference` — decision H2), `"auto"` falls to the outcome level's own measure: a **percentage** reads as "x times as likely" (`ratio`), a **quantity** or a **count** in its own units (`difference`).

This is not a new fact: `REG_WORDS$noncollapsible` already declares it, and `reg_estimand_collapsible()` already uses it to decide whether the adjustment gap may be *tested*. One fact, now with two readers — the house style.

⚠ **This is the one place an argument reads one to its RIGHT**, and the reason it is not circular: the clause fires only where `effect` was named EXPLICITLY. Under `effect = "auto"` the measure resolves to the link's, and `effect` then resolves to `conditional` — so the cascade still runs strictly left to right on a default call.

What it gives, per family, on a bare prediction route:

| family                | link (auto) | collapsible? | `measure` auto | `marginal`    | `at_reference`  |
|-----------------------|-------------|--------------|----------------|---------------|-----------------|
| gaussian              | difference  | yes          | the link's     | `Model_mdiff` | `Model_refdiff` |
| poisson               | ratio       | yes          | the link's     | `Model_mIRR`  | `Model_refIRR`  |
| binomial              | odds_ratio  | **no**       | the level's    | `Model_mRR`   | `Model_refRR`   |
| multinomial / ordinal | odds_ratio  | **no**       | the level's    | `Model_mRR` ¹ | `Model_refRR`   |

¹ on a 3+ category outcome a predicted odds ratio is refused outright, not merely un-defaulted (§10.5): it has no complement, so it must be asked "versus what?" first. The one answer is `multinomial × at_reference × odds_ratio`, the vs-rest builder, and it is reached by naming `measure = "odds_ratio"`.

So decision 5 survives, but only as this clause, and only where it has a reason. ⚠ The alternative — a pure cascade with no clause — is coherent too, but it hands a marginal odds ratio to anyone who types `effect = "marginal"` on a binary outcome, and on a multinomial it would hand them an abort. The clause is one sentence and buys both away.

### 3.5 What `effect` becomes

The maintainer's question: with `link` naming the model, does `effect = "coefficient"` become meaningless?

**As a knob, essentially yes. As a concept and as a diagnostic, no** — and that is the right outcome rather than a loose end:

- **Nobody types it.** `"auto"` picks the coefficient whenever the measure equals the link, which is exactly when a coefficient exists. There is no call in which typing `effect = "coefficient"` changes an answer.
- **It stays the name of the reading**, in prose, in `reg_measures()`, in the vignette grids and in the unmarked header (`Model_OR` vs `Model_mOR`). Deleting the value would leave the default reading nameless.
- **It earns its keep as an assertion.** `link = "odds_ratio", measure = "difference", effect = "coefficient"` is a user saying something impossible; keeping the value is what lets the package answer *"the model estimates an odds ratio; a risk difference must come from its predictions (marginal or at_reference)"* rather than *"unknown value"*.

So the value survives; what it stops being is a choice. **Whether the ARGUMENT should keep its name is a separate question, and §9 tests it.**

### 3.6 The signature and the vocabulary

```r
tab_reg(data, outcome, predictors = NULL, tab_vars = NULL, wt = NULL,
        family = "auto", link = "auto", measure = "auto", effect = "auto",
        trials = NULL, empirical = FALSE, ...)
```

The four estimand arguments sit in **cascade order**, so the signature reads left to right the way §3.1 teaches it. That order is not a preference: each argument's `"auto"` reads the one before it, so any other order would put an argument before the thing it inherits from.

It is free in R: **0 of 757 `tab_reg()` call sites in this repository pass `effect` or `measure` positionally** (they are formals 7 and 8), and the maximum positional-argument count anywhere is 4. ⚠ `reg_estimand()` itself is called positionally at 29 sites, so **its** argument order must not move.

- **`link` takes `measure`'s values** — `"difference"` / `"ratio"` / `"odds_ratio"` — with the glm spellings accepted silently and never taught. This is what keeps four arguments feeling like two, and it is why a family object was rejected (§6.4). It takes the per-outcome grammar the other three already have (scalar / positional vector / named-by-outcome), through the existing `reg_per_outcome()`.
- **`effect` keeps its name and gains `"auto"`**; its first value is renamed **`"coefficient"` → `"conditional"`** (§3.5).
- **`family`'s values are unchanged**, and `family = "poisson"` on a binary outcome becomes a **redundant spelling of `link = "ratio"`**. Its message should now name `link = "ratio"`, and the `rr_promoted` special case (`R/reg-resolve.R:424-447`) is then deletable — taking §13's defect (1) with it.

---

## 4. What changes for a user

### 4.1 Worked calls

| call                                      | before                              | now                                |
|-------------------------------------------|-------------------------------------|------------------------------------|
| `tab_reg(d, "married", x)`                | `Model_OR`                          | **identical**                      |
| `… measure = "ratio"`                     | `Model_RR`, refits modified Poisson | `Model_mRR`, marginal on the logit |
| `… measure = "difference"`                | `Model_RD`, refits identity link    | `Model_mRD` — the AME              |
| `… link = "ratio"`                        | only via `family = "poisson"`       | `Model_RR`, modified Poisson       |
| `… link="ratio", measure="difference"`    | **impossible**                      | `Model_mRD`, mod.-Poisson fit      |
| `… effect = "marginal"`                   | `Model_mRD` (points)                | `Model_mRR` — see §3.4             |
| `tab_reg(d, "age", x, effect="marginal")` | **refused** as redundant            | `Model_mdiff`                      |
| `tab_reg(d, "party3", x, measure="diff")` | **abort**, a nine-line menu         | `Model_mRD`                        |

**The default call does not move.** `family` auto → binomial; `link` auto → odds_ratio; `measure` auto → odds_ratio; `effect` auto → conditional. Verified byte-identical on all five families — names, every rendered cell and the footer — which is what let the chain be adopted without moving a single golden.

**And the user always knows what was modelled**, which was the maintainer's Problem A: either they set `link`, or it is the family's own — and the footer names it either way. `measure` can no longer change the model behind their back, because changing the model is now a different argument with a different name.

### 4.2 The defaults — there is only one rule left

`"auto"` means **follow from the left** (§3.1), plus §3.4's single clause. That deletes the ten hand-written values of `REG_ESTIMANDS[[fam]]$default` and replaces the earlier draft's two-column default table with one derivation:

| family      | `level` | `link` auto (its first `fits` entry) | `measure` auto | a bare `effect = "marginal"` |
|-------------|---------|--------------------------------------|----------------|------------------------------|
| gaussian    | `mean`  | `difference`                         | `difference`   | `Model_mdiff`                |
| binomial    | `pct`   | `odds_ratio`                         | `odds_ratio`   | `Model_mRR` ¹                |
| poisson     | `count` | `ratio`                              | `ratio`        | `Model_mIRR`                 |
| multinomial | `pct`   | `odds_ratio`                         | `odds_ratio`   | `Model_mRR` ¹                |
| ordinal     | `pct`   | `odds_ratio`                         | `odds_ratio`   | `Model_mRR` ¹                |

¹ §3.4's clause: `"auto"` never marginalises a non-collapsible measure, so it falls to the level's own.

**Decision 5 (P8) therefore dissolves into the cascade** and survives only as that clause — which was the maintainer's suspicion, confirmed. ⚠ Its consequence is unchanged and still binds: three families now report a **ratio** where they reported points, and that is **coupled to Phase 22b-xiv-2** (§8.3).

### 4.3 Caveats — the honest list

1. ⚠ **`effect = "marginal"` changed meaning**, from "the AME in percentage points" to "the model's own effect, averaged" — on a binary outcome `Model_mRR`, not `Model_mRD`. It is the **only** spelling that changes an answer without saying so, and it hits the most-taught idiom in the package. The replacement is one argument and clearer (`measure = "difference"`), but every passage teaching it moves, and its printed numbers with it — §8.2.1.
2. **Three spellings changed which model they fit**: `measure = "ratio"` and `measure = "difference"` on a binomial, and `measure = "ratio"` on a gaussian, no longer refit — they report from the family's own model. The conditional readings they used to give are `link = "..."`, and asking for them the old way now aborts naming the cure.
3. **Four arguments where the phase set out to simplify.** The defence is that three of the four are `"auto"` in every teaching example and the fourth (`measure`) is the only one a reader types — but the signature is longer, and jamovi gains a control.
4. **`link` is a technical word** even with friendly values. `model =` was the runner-up, rejected because `predictors = list(...)` already produces "models" in the comparison sense; decision H1 kept `link` and settled the internal naming around it.
5. **A prediction route is not free**: it pays a counterfactual sweep plus an analytic jacobian where a coefficient route pays a `tidy()`. Measured, with the surprise that two spellings got *cheaper* — §14.3.
6. **A weighted 3+ level outcome can only be read on its coefficients**: its marginal quantities have no survey method, so any `measure` other than the model's own aborts. The refusal covers both ways of asking (naming the contrast, or naming the measure) and says so in those words.
7. **It does not, on its own, fix the ladder** — see §8.3, an ordering constraint against Phase 22b-xiv-2 and unchanged by this restructure.

---

## 5. The architecture — `REG_ESTIMANDS` is a generator

`reg_compose_library()` emits one row per **buildable** `(link, effect, measure)`. A refusal is **not a row**: `reg_estimand()` derives it from the clause that failed, so a hole and its reason cannot drift apart.

### 5.1 The composed row, member by member

| member         | comes from                                                              |
|----------------|-------------------------------------------------------------------------|
| `link`         | the key into the family's `fits`; `"auto"` = its first entry             |
| `effect`       | the contrast: `conditional` / `marginal` / `at_reference`                |
| `measure`      | what is REPORTED; `"log"` on a logged row                                |
| `base_measure` | what a logged row is the log OF (itself elsewhere) — **the lookup key**  |
| `measure_link` | the link the REPORTED comparison is taken on; == the model's on a coef   |
| `fit`          | `fits[[link]]` — on EVERY route, which is what `link` buys               |
| `builder`      | `coef`; else `ame`, or `vsrest` for a profile odds ratio                 |
| `exp`          | the measure is multiplicative and not logged                             |
| `scale`, `word`| `REG_LEVEL_MEASURES[[level]][[measure]]`, the family's `words` overriding|
| `crude_fam` / `crude_shape` | the block's shape on `measure_link` (§5.2)                  |
| `comparison`   | `measure_link`'s marginaleffects spelling; `lnor` on the vs-rest arm      |
| `note`         | the family's qualifier on a coefficient, `est_note_marginal()` otherwise |
| `log_of`       | on a logged row, the scale it is the log of                              |
| `status`       | `"ok"` — every row in the table builds                                   |

⚠ **`base_measure` is not decoration.** Under one `(link, effect)` a ratio and an odds ratio both log, so `measure = "log"` alone would name two rows; the lookup keys on the base, and `"log"` / `"log_odds"` / `"log_risk"` / `"log_rate"` are the spellings that pick one.

There is **no `engine`, `obs` or `display` member**: `reg_marginal_engine()` derives the engine from the contrast, `reg_estimand_obs()` the crude permission, and the display was write-only.

The scale map, read off every row:

| level kind | `difference` | `ratio`      | `odds_ratio` |
|------------|--------------|--------------|--------------|
| `pct`      | `points`     | `pct_ratio`  | `odds_ratio` |
| mean       | `raw_diff`   | `mean_ratio` | — (no odds)  |
| count      | `raw_diff`   | `mean_ratio` | — (no odds)  |

plus `log` → `log_coef`, and the existing `REG_SCALE_GROUPED` remap for a `trials =` outcome. The word map is the same shape: `pct` → `RD` / `RR` / `OR`, mean → `diff` / `RoM`, count → `diff` / `IRR`.

### 5.2 The crude companion derives too, and that was not expected

`crude_fam` and `crude_shape` look like the one genuinely per-cell fact. They are not. **Every `REG_EMPIRICAL` block holds at most one shape per (link, logged) pair** — checked on all eight blocks (`R/reg-empirical.R:477-531`):

| block              | identity | log         | logit   |
|--------------------|----------|-------------|---------|
| `binomial`         | `ame`    | —           | `or`    |
| `rr`               | `ame`    | `rr`        | —       |
| `mr`               | —        | `mr`        | —       |
| `gaussian`         | `diff`   | —           | —       |
| `poisson`          | `diff`   | `irr`       | —       |
| `grouped_binomial` | `ame`    | `rr`        | `or`    |
| `multinomial`      | `ame`    | `ame_ratio` | `or`    |
| `ordinal`          | `ame`    | `ame_ratio` | `cumor` |

So `crude_shape` **is** the block's shape on the measure's link (with `_log` when logged), and the two cross-family borrows that look ad hoc are one rule: *when the outcome's own block has no shape on that link, take the block named by the fit for that link* — which is why `binomial` × `ratio` borrows `rr` and `gaussian` × `ratio` borrows `mr`. The borrow is `family$fits[[measure]]`, a value the coefficient route already needs.

Two derived objects carry it, and both have exactly two readers:

- **`REG_EMP_BY_LINK`** (`R/reg-empirical.R`) indexes the table above by `(block, link, logged)`, read off `REG_EMPIRICAL$*$link` itself. `reg_compose_crude()` picks each row's shape through it, which is what makes *"a model row and its observed twin state one estimand"* true by construction rather than by two declarations agreeing.
- **`reg_emp_block()`** (`R/reg-estimand.R`) answers "which block does this family-or-fit key resolve to" — its own if it has one, else the outcome family it belongs to. ⚠ **`reg_crude_key()` calls it**, so its two hardcoded remaps (`quasipoisson → poisson`, `rd → binomial`) are one declared fact with a compose-time and a run-time reader. `tx_fk_emp_reachable()` likewise calls `reg_emp_block_of()` rather than re-implementing the selection.

### 5.3 The declared residue

What a family declares, beyond its names, and what the whole library is composed from. `level` **reuses an existing vocabulary rather than inventing one**: it takes `EST_SCALES$var_kind`'s own words (`pct` / `mean` / `count`), which already declare exactly this distinction (§2.1).

```r
REG_FAMILIES$binomial <- list(
  # ... display / short / ui / ui_binary / outcome / outcome_level, unchanged ...
  level = "pct",                       # pct | mean | count -- which measures exist, and the level's own
  fits  = c(odds_ratio = "binomial",   # THE VALUE SET OF `link`, measure-keyed -> the fit key.
            ratio      = "rr",         # ORDER IS LOAD-BEARING: the first entry is the family's own
            difference = "rd"),        # link, which is what `link = "auto"` resolves to
  odds_pred = "complement",            # HOW a PREDICTED odds ratio is defined here (below)
  note = function() gettext("vs the reference category")   # the coefficient route's footer clause
)
```

- **`words`** is a per-measure header override, and only `ordinal` declares one (`list(odds_ratio = "cumOR")`): a cumulative odds ratio is not a plain one, and the header says so.
- **`odds_pred`** states §10.5's rule where it belongs — *an odds ratio needs a percentage **and its complement***. `"complement"` (binomial: both prediction routes), `"vsrest"` (multinomial: at a profile only, each category versus the rest — the existing builder), absent (ordinal, and every non-`pct` level: refused, with the reason).
- **`crude`** names the `REG_EMPIRICAL` block when it is not the family's own name. Only `quasipoisson` needs it (`"poisson"`): it differs in the variance assumption, not in what it estimates.
- **Declaring `fits` IS the fact "a user family"** — `REG_USER_FAMILIES` filters on it, and the internal link keys (`rr` / `rd` / `mr`) declare none.

The other families: `gaussian` is `level = "mean"`, `fits = c(difference = "gaussian", ratio = "mr")`; `poisson` `level = "count"`, `fits = c(ratio = "poisson")`; `multinomial` and `ordinal` `level = "pct"`, `fits = c(odds_ratio = <own>)`.

Two shared tables carry the rest:

```r
# the link <-> measure map of section 2.1 -- the ONE place the statistician's word appears
REG_MEASURE_LINK   <- c(odds_ratio = "logit", ratio = "log", difference = "identity")
# what each kind of LEVEL can be compared by, what that comparison is called and which EST_SCALES
# row carries it. ORDER IS LOAD-BEARING: the first entry is the LEVEL'S OWN measure.
REG_LEVEL_MEASURES <- list(
  pct   = list(ratio      = c(scale = "pct_ratio",  word = "RR"),
               difference = c(scale = "points",     word = "RD"),
               odds_ratio = c(scale = "odds_ratio", word = "OR")),
  mean  = list(difference = c(scale = "raw_diff",   word = "diff"),
               ratio      = c(scale = "mean_ratio", word = "RoM")),
  count = list(difference = c(scale = "raw_diff",   word = "diff"),
               ratio      = c(scale = "mean_ratio", word = "IRR")))
```

⚠ `REG_LEVEL_MEASURES`' first element **is** load-bearing: it is the level's own measure, which §3.4's one clause falls back to. `REG_LINKS_VALUES` (`link`'s value set) is `c("auto", names(REG_MEASURE_LINK))`, and `REG_LINK_ALIASES` accepts the glm spellings silently — ⚠ NOT folded into `REG_MEASURE_ALIASES`, because on `link` the word `"log"` means the LOG LINK while on `measure` it means "un-exponentiated": the one word the two vocabularies do not share.

**`reg_estimand(family, link, measure, effect)`** keeps its typed-refusal contract, and its formals are in **cascade order** so a call reads the way the arguments resolve. Every argument is validated against its own vocabulary, so a stale positional call aborts rather than quietly meaning something else. Every foreign key in `zzz-fact-keys.R` now checks the composed rows over the same enumerated grid.

**What this buys, concretely**: adding a family is one row of declared facts instead of 6-8 hand-written rows whose 15 members must each be right; the positional-constructor hazard is gone with the constructor; and a family **cannot** declare a scale, a header word and a crude shape that disagree, because it no longer declares them.

### 5.4 What is deliberately NOT there

Mechanisms that used to exist and must not be re-added, because the composition is what replaced them:

| gone                                        | because                                                   |
|---------------------------------------------|-----------------------------------------------------------|
| a hand-written row per estimand, and `est_row()` | composed from the facts of §5.3                      |
| `status = "redundant"` and its post-processor | a marginal contrast that equals the coefficient builds (§11) |
| the `engine` / `obs` / `display` members    | derived, or write-only                                     |
| `REG_ESTIMANDS[[fam]]$default`              | `"auto"` follows from the left                             |
| declared `impossible` rows                  | generated from the clause that failed (§3.3)               |
| `reg_default_measure()`                     | `reg_auto_measure(family, link, effect)` answers it        |
| `rr_promoted`, and `family = "poisson"` on a binary outcome | it **is** `link = "ratio"` (H3)            |
| `TABX_DEFAULT_MEASURE`                      | generated and read nowhere                                 |

⚠ **The one addition is `link` itself** — one argument, one jamovi control, one `TAB_ARGS` row. Everything it makes possible (a non-default model, and reporting a different measure from it) is otherwise expressed by *deleting* two indirect spellings.

What is **kept and untouched**: `reg_estimand()`'s typed-refusal contract, every `REG_WORDS` / `REG_CONTRASTS` composition rule, `REG_MEASURE_ALIASES`, `REG_EMPIRICAL`, `EST_SCALES`, `MEASURES`, and the colour engine.

---

## 6. Future-proofing — a new model must be rows, never arguments

The maintainer's constraint: **do not extend to the rest of the glm families now, but leave a framework that can, later, with no friction and no API change.** This section is the test of the §3 design against that constraint, and the answer is that the cascade passes it — because `link` and `measure` take *words*, not model specifications, so a new model changes what the words resolve to and never what the user types.

### 6.1 The rule

> **A new model is a row in a declared table. It is never a new argument, a new value the user must learn, or a new spelling of an existing idea.**

Three things follow, and each is checked below: a new **link** is one row of the §2.1 map; a new **family** is one row of `REG_FAMILIES`; and neither touches `family` / `link` / `measure` / `effect`, whose value sets are *generated* from those tables exactly as `TABX_ESTIMANDS` already is.

### 6.2 Adding a link — one row

`cloglog` → hazard ratio is the only credible candidate (§2.1). Landing it:

| what                                                                     | size    |
|--------------------------------------------------------------------------|---------|
| one row in `REG_MEASURE_LINK`                                            | 1 line  |
| one cell per level kind in `REG_LEVEL_MEASURES` (its scale + its acronym) | 1 line  |
| `"hazard_ratio"` in `REG_MEASURE_ALIASES`, and `cloglog` in `REG_LINK_ALIASES` | 2 lines |
| `HR` in `REG_WORDS` with its `long`                                      | 1 line  |
| one row in **`REG_LINK_FUNS`** (`h`, `h'`, the domain guard)             | 1 line  |
| `fits = c(…, hazard_ratio = "cloglog")` on the families that can fit it  | 1 field |
| a `reg_fit()` `switch` arm, and a `REG_CHECKS$families` membership       | 2 edits |

And **nothing else**: the availability rule (§3.3) derives, the scale and the header word come from `REG_LEVEL_MEASURES`, both g-computation makers read `REG_LINK_FUNS` (so the marginal contrast needs no arm), and the crude counterpart keys on the measure (§6.5). ⚠ **`REG_LINK_FUNS` is the one that fails loudly if forgotten**: a foreign key checks `REG_EMPIRICAL$*$link` against its names at load, because an undeclared link would otherwise drop the gap test silently rather than error. The user's vocabulary gains one word and loses none.

### 6.3 Adding a family — one row plus two edges

The motivating case is health-cost modelling: a skewed positive outcome fitted on the log scale and reported in the outcome's own units. Basu & Rathouz (2005) is titled, exactly, *"Estimating marginal and incremental effects on health outcomes using flexible link and variance function models"*, and its abstract states the separation this design is built on: *"Rather than focusing on the regression coefficients, the purpose of these models is inference about the mean of the outcome as a function of a set of covariates, and various functionals of the mean."*

⚠ **And the §3 cascade already delivers that case with no new family**: `link = "ratio", measure = "difference"` on a gaussian outcome is the existing `mr` fit (Poisson pseudo-ML, robust SEs — PPML) reported as a marginal difference in the outcome's units. What a `Gamma` family would add is the *variance assumption*, not the capability — which is why decision 7d ("no new families for now") costs little.

When one is wanted, the work is:

| what                                                          | size                         |
|---------------------------------------------------------------|------------------------------|
| one `REG_FAMILIES` row: `level`, `fits`, display, short       | 1 row                        |
| its `REG_CHECKS$families` memberships                         | edits to an existing column  |
| its footer statistics (`reg_glance()` / `reg_footer_stats()`) | **the real work** (§6.6)     |
| its crude cells                                               | **0 new mathematics** (§6.5) |

### 6.4 Why not R family objects

`family = binomial(link = "log")` was studied and **rejected**: it imports the statistician's vocabulary into an argument the package deliberately keeps in its own words, and it would give the *link* two spellings (`binomial(link = "log")` and `link = "ratio"`) — two names for one quantity, which is the defect this phase exists to delete. It would also need five plumbing rules of its own, one of them a silent trap: measured, `reg_per_outcome(binomial("log"), "married", 1, "auto")` returns **`"auto"`**, because a family object is a named list of length 13 and the named-vector branch misses.

**The capability is not lost with it.** What a family object would have expressed — *fit on one scale, report on another* — is exactly what `link` ≠ `measure` expresses in §3, in the user's own vocabulary and with no new syntax. The object route is therefore not a deferred feature but a rejected spelling.

⚠ One fact from that study is worth keeping, because it is what makes §6.1 true: **`reg_fit()`'s `switch()` already builds family objects**, so the internal link keys are `(outcome, family object)` pairs — `rr` and `mr` are both `quasipoisson("log")`, distinguished only by the outcome, and `rd` is `binomial("identity")`. So §5.3's `fits` column is already a map to a family object in all but name, and a future family slots into it without a new mechanism.

### 6.5 The evidence: the engines are already generic

This is why "no friction later" is a claim and not a hope. Every measurement below is on `gss_cat` (n = 21 407) or a simulated Gamma cost outcome.

**The g-computation engine never knew which link it was on.** `reg_gcomp_maker()` reads `stats::family(fit)` and uses its `linkinv` / `mu.eta`. Handed four families the package has never supported, with no change:

| fit                   | AME      | half-width | note                                    |
|-----------------------|----------|------------|-----------------------------------------|
| `binomial("logit")`   | −0.19092 | 0.01930    | today's route                           |
| `binomial("probit")`  | −0.18986 | 0.01924    | unreachable today                       |
| `binomial("cloglog")` | −0.19459 | 0.01952    | unreachable today                       |
| `gaussian("log")`     | +3.56347 | 0.24270    | on a simulated skewed cost outcome      |
| `Gamma("log")`        | +3.56345 | 0.26074    | unreachable today — the cost standard   |
| `quasipoisson("log")` | +3.56346 | 0.24863    | tabxplor's `mr`, today coefficient-only |

Point estimates barely move across links while **standard errors do** — the link is an efficiency choice and the marginal effect is the estimand, shown rather than argued.

**The crude counterpart's closed forms ARE the saturated univariable glm's own Wald interval.** A saturated fit has the same fitted values under every link (verified to 10 decimals across logit / probit / log / identity / cloglog — they *are* the cell proportions), so:

| crude interval, Black vs Other     | estimate | lower    | upper    |
|------------------------------------|----------|----------|----------|
| Woolf closed form (log OR)         | 0.423707 | 0.376454 | 0.476892 |
| `glm(y ~ race, binomial("logit"))` | 0.423707 | 0.376454 | 0.476892 |
| Katz closed form (log RR)          | 0.583758 | 0.542570 | 0.628072 |
| `glm(y ~ race, binomial("log"))`   | 0.583758 | 0.542571 | 0.628072 |

So **a new family needs no new closed form**: the refit gives the identical number, and that path already exists (`reg_empirical_fit()`, extended to the design rung in 22b-xiii-2). Note too that the *saturated* log-binomial converges where the adjusted one fails — a crude column never meets the convergence edge a model column can.

**And what varies across families is the variance function, not the link.** Same saturated ratio of means, four variance assumptions:

| univariable fit           | RoM      | se(log)  |
|---------------------------|----------|----------|
| `gaussian("log")`         | 1.405057 | 0.019366 |
| `quasipoisson("log")`     | 1.405057 | 0.019634 |
| `Gamma("log")`            | 1.405057 | 0.020346 |
| `inverse.gaussian("log")` | 1.405057 | 0.021535 |

Identical point estimate — it is a cell mean — and only the interval moves. **So the crude grid is (variance function × measure), never (family × link × measure × route)**: links contribute zero rows, and the grid a future family joins is five rows, not two hundred and fifty.

| variance function (family)       | difference      | ratio                  | odds ratio    |
|----------------------------------|-----------------|------------------------|---------------|
| binomial / quasibinomial (pct)   | ✓ `ame`, Wald   | ✓ `rr`, Katz           | ✓ `or`, Woolf |
| gaussian (mean)                  | ✓ `diff`, ols   | ✓ `mr`, quasi-Poisson  | n/a           |
| poisson / quasipoisson (count)   | ✓ `diff`, Welch | ✓ `irr`, quasi-Poisson | n/a           |
| **Gamma** (mean)                 | route to refit  | route to refit         | n/a           |
| **inverse.gaussian** (mean)      | route to refit  | route to refit         | n/a           |

The refit path is measured exact against `marginaleffects::avg_comparisons()` on estimate **and** standard error for `gaussian("log")` (+3.01909 / 0.16038), `Gamma("log")` (+3.01909 / 0.17784) and `quasipoisson("log")` (+3.01909 / 0.16688).

**The adjustment gap is generic too**, which was the surprise. `reg_coef_if_maker()` (`R/reg-influence.R:83-101`) builds the model leg's influence function from `model.matrix(fit)`, `fit$weights` — the IRLS working weights, which is exactly where the variance function and `mu.eta` already live — and `residuals(type = "working")`. Verified to return a working influence function for `binomial`, `poisson`, `quasipoisson`, `Gamma`, `inverse.gaussian` and `gaussian`, unchanged. The crude leg (`:129`) is the nonparametric influence function of a weighted cell mean, so it is family-agnostic by construction; its only link-dependent piece is `g'(μ)` for `logit` / `log` / `identity` — **the measure's link, not the model's**, and the same map §10.4 needs anyway.

**Cost**, per predictor at n = 21 407: a univariable `glm` refit 2.5–2.7 ms, `reg_marginal_gcomp()` with SE 4.9 ms, `reg_coef_if_maker()` 0.1 ms — about **7.5 ms per predictor**, ~38 ms for a five-predictor table, against the +131 ms that 22b-xiii-2 measured and accepted for the design-rung crude refit. Not a performance question.

### 6.6 What is NOT free — and it is not the inference layer

⚠ The two subsystems that carry per-family knowledge R itself does not supply:

- **`reg_glance()` / `reg_footer_stats()`** — the goodness-of-fit rows. R² and sigma for a linear model, McFadden and LR-null for a likelihood one, the Pearson dispersion `phi` for a count or a grouped binomial. A new family needs its own set, and there is no generic answer.
- **`REG_CHECKS$families`** — dispersion, linearity, influence, collinearity, proportionality. Each check declares which families it applies to, and a new family must be placed in or out of each.

Both are already declared tables with a `families` column, so a new family is an edit to an existing column rather than a new mechanism — but they are genuine per-family *statistics*, and that is the honest reason decision 7d ("no new families for now") is right.

### 6.7 The extension checklist

What a future session must do to add a model, and what it must **not**:

| must do                                                | must NOT do                                        |
|--------------------------------------------------------|----------------------------------------------------|
| one `REG_FAMILIES` row (`level`, `fits`, `note`, names)| add an argument                                    |
| a `REG_LINK_FUNS` row, for any link not already there  | add a `measure` or `effect` value the user learns  |
| place it in `REG_CHECKS$families`                      | write a crude closed form                          |
| give it footer statistics                              | touch `reg_gcomp_maker()` or `reg_coef_if_maker()` |
| one parity test vs `confint(glm())`                    | hand-write an estimand row                         |
| regenerate the jamovi vocabulary block                 | hand-edit the generated `.h.R` or JS               |

⚠ Two boundaries to keep deliberately: `quasi()` and `MASS::negative.binomial()` stay out (an arbitrary variance function has no footer statistics and no check semantics), and the eight declared crude blocks keep their closed forms — they are free, exact, and what the goldens pin.

---

## 7. Decisions register

| #  | possible decision                                       | status: maintainer’s call                          |
|----|---------------------------------------------------------|----------------------------------------------------|
| 1  | Delete `status = "redundant"`; the 3 cells build        | **decided** (this session)                         |
| 2  | Derive `REG_ESTIMANDS` from 4 facts per family          | **decided** — §5, verified row by row              |
| 3  | `effect = "auto"`, measure-first routing                | **decided** — §3                                   |
| 4  | `measure = "auto"` = the base-link measure (coef route) | **decided** — §3.4                                 |
| 5  | Prediction defaults: pct → `ratio`, else `diff` (P8)    | **decided** — §4.2, it dissolves into §3.4         |
| 6  | Marginal OR: binary only, 3+ category refused           | **decided: binary only** — §10.5                   |
| 7a | `link =` as a fourth argument                           | **decided** — §3.1 adopts it                       |
| 7b | Fit on one link, marginalise on another                 | **decided** — §4.1, now free                       |
| 7c | If opened: `family = binomial(link = "log")`            | **no, it fights tabxplor vocabulary** — §6.4       |
| 7d | Interval the blocker? **No** — §6.5                     | **no new families for now**: needs new checks etc. |
| 8  | Reorder to `family` → `link` → `measure` → `effect`     | **decided** — §3.6                                 |
| 9  | Keep `effect`, rename "coefficient" to "conditional"    | **decided** — §3.5, §9; docs teach it = the coeffs |
| 10 | Correct the "orthogonal" claim (P4)                     | **decided** earlier; falls out of §2               |
| H1 | The fourth argument's NAME, one more look (§4.3(4))     | **`link`** — see the note below                     |
| H2 | Does §3.4's clause cover `at_reference` too?            | **both prediction routes** — see the note below     |
| H3 | Keep `family = "poisson"` on a binary outcome?          | **retired** — `link = "ratio"` is the one route     |
| H4 | `reg_measures()`'s shape under the cascade              | **a `link =` argument**, one model per call         |

⚠ H1–H4 were taken at implementation time (Phase 22b-xv-1) and are what the package does.

- **H1.** The row member that means *the reported comparison's* link is named `measure_link`, so three words stay distinct: `link` (the user's, naming the model), `fit` (the internal key it selects), `measure_link` (what is reported).
- **H2.** One rule — **`"auto"` never resolves to a PREDICTED odds ratio.** `binomial` / `ordinal` × `at_reference` therefore default to `refRR`, and multinomial's vs-rest OR is reached by naming `measure = "odds_ratio"`.
- **H3.** The spelling aborts naming both things it could have meant, and `rr_promoted` goes with it — taking §13's defect (1).
- **H4.** The grid stays `effect × measure`, read at one model, with the other models named in the message.

**Remaining problems — answered in §3 and §6.** The maintainer's own sketch turned out to be the design; what follows is where each question is answered and what the study added to it.

- **"`measure` first, but it would override the `link`, and the user won't know what he has modelled."** Answered by making `link` an argument: `measure` can no longer touch the model, because changing the model now has its own name. **§3.1, §4.1.**
- **"Is identity / log / logit all, or are there other cases?"** **One more, and then the well runs dry**: `cloglog` → **hazard ratio**, the discrete-time proportional-hazards model (Prentice & Gloeckler 1978). `probit`, `cauchit`, `sqrt`, `inverse` and `1/mu²` name **no** measure of deviation — which is exactly why probit models are reported through marginal effects. **§2.1.**
- **"`link =` as a fourth argument taking the same options as `measure`."** Adopted, and it is the key: **a link IS a measure**, so the two arguments share one vocabulary and the statistician's spellings never surface. **§2.1.**
- **"Would naming the default `model_link` be clearer than `auto`?"** **No — keep `"auto"`, and the reason is the cascade.** `"auto"` means the same thing on all four arguments: *follow from the left*. A second spelling on one of them would be two names for one idea, which is the defect this phase deletes; and `"model_link"` would be wrong on `link` itself, where auto means *the family's own*. **§3.1.**
- **"`effect = "coefficient"` becomes a bit meaningless — am I wrong?"** **You are right, as a knob.** Nobody will ever type it, because `"auto"` picks it whenever it exists. It is kept for two other jobs: it names the default reading in prose, in `reg_measures()` and in the unmarked header; and it is what lets an impossible request get a real answer instead of *"unknown value"*. **§3.5.**
- **"Only a choice between marginal and at_reference, then? Readable? User-friendly? Caveats?"** Effectively yes, and it reads better than today because the user reaches the marginal route by naming what they want (`measure = "difference"`) rather than by naming a mechanism (`effect = "marginal"`). **The caveats are real and listed at §4.3** — the biggest is that `effect = "marginal"` changes meaning and moves the most-taught idiom in the package.
- **"Do you see a better possibility?"** Three were weighed and rejected: folding `link` into `family` as an R family object (§6.4 — it fights the vocabulary and gives the link two spellings); one `measure` argument taking a `c(model, report)` pair (a positional vector whose two elements mean different things is harder to teach than two named arguments); and dropping `effect` entirely (impossible — something must express "marginalise even though the measure matches the link", which is the AME-vs-coefficient comparison and the marginal odds ratio).

**"Should we extend tabxplor to all glm families?"** Not now — and §6 is the answer to the second half, that the framework must be able to later. The design passes: a new **link** is one row of the §2.1 map plus three one-line entries (§6.2); a new **family** is one `REG_FAMILIES` row (§6.3). Neither adds an argument or a value the user must learn, because `link` and `measure` take *words*, not model specifications. The engines are already generic — measured on four families the package has never supported (§6.5) — and the crude grid a new family joins is **(variance function × measure)**, five rows, not the ~250 of family × link × measure × route. ⚠ What is genuinely per-family, and the honest reason to wait, is the **footer statistics and the model checks** (§6.6), not the inference layer.

---

**PART II — IMPLEMENTATION**  ·  the Phase 22b-xv roadmap

## 8. The roadmap

Two phases. The first is all of the code and the reference documentation that lives beside it; the second is the teaching prose, which can only be written once the numbers it quotes can be re-run. Each phase plans itself in plan mode from this document; what follows organises the work and deliberately does not prescribe it.

### 8.1 Phase 22b-xv-1 — the estimand engine and the argument cascade

**Landed.** Part I describes what it built; the per-file design is in the R headers, and the phase's own record is `CLAUDE.md`'s DONE summary. What is worth carrying forward from it into any later work here:

- the composed grid is swept against a snapshot of the retired table by `dev/verify_estimand_library.R`, which is how a future change to §5.3's facts is checked;
- `test-reg-invariants.R` sweeps the three invariants over **every** composed row, link axis included — that is what catches a row whose `(scale, word)` no longer matches its crude shape, the tightest coupling in the subsystem;
- the default `tab_reg()` call is byte-identical to the pre-cascade one, and should stay so.

### 8.2 Phase 22b-xv-2 — teaching the cascade everywhere

**The prose**: both regression vignettes and their French twins, `vignettes/articles/tabxplor-all-else-equal.Rmd`, and one `NEWS.md` bullet (already written — check it rather than rewrite it).

The shape of the work, from §12.1: the combination grid gains a `link` column and loses the `≡` marker; the headline rule at `vignettes/tabxplor-reg.Rmd:583` (*"`effect` and `measure` change the model that is fitted"*) becomes true **by deletion** — only `link` changes the model; and `effect = "conditional"` is introduced as *the model's own coefficients*, with `"auto"` explained as choosing it when the reported measure is the model's own.

⚠ **The vignette must say which risk ratio to use when** (maintainer's requirement). `link = "ratio"` is the modified Poisson's **conditional** risk ratio; `measure = "ratio"` is the **marginal** one, g-computed from the ordinary logistic fit. They are different estimands, and the prose has to say when each is the one to report — not merely that both exist.

⚠ **Method** — the Phase 22h rule, not optional here because the article quotes figures in prose in at least eight places: re-run every table into a scratch file first, quote from that file, then re-check the rendered HTML against it.

#### 8.2.1 The migration table

Gathered by porting the whole test suite. Every row is a spelling that **still runs** but now means something else, or that has moved — which is exactly what a reader of the old vignettes will type.

| was                                                | now                                | why                                     |
|-----------------------------------------------------|-------------------------------------|------------------------------------------|
| `effect = "coefficient"`                            | drop it, or `"conditional"`         | the value names the quantity              |
| `measure = "ratio"` (wanting the modified Poisson)  | `link = "ratio"`                    | only `link` changes the model             |
| `measure = "difference"` (wanting the identity link)| `link = "difference"`               | idem                                      |
| `measure = "ratio"` on a gaussian (the `mr` fit)    | `link = "ratio"`                    | idem                                      |
| `family = "poisson"` on a **binary** outcome        | `link = "ratio"`                    | `family` never picks a link (H3)          |
| `trials = k, measure = "ratio"` / `"difference"`    | `trials = k, link = ...`            | idem, on a summed score                   |
| `measure = "log_risk"` on a binomial                | `link = "ratio", measure = "log"`   | a `log_*` spelling pins the MEASURE       |
| `effect = "marginal"` (wanting percentage points)   | add `measure = "difference"`        | ⚠ **the one silent change** (below)       |
| `effect = "at_reference"` on a multinomial          | add `measure = "odds_ratio"`        | `"auto"` never predicts an OR (§3.4)      |

⚠ **Only one row changes an answer without saying so**: a bare `effect = "marginal"` now reports the level's own measure — a **ratio** on a probability, where it used to give percentage points. Everything else on the list either aborts naming its cure, or is a rename. The taught idiom therefore becomes `measure = "difference"` (shorter than the old `effect = "marginal"`, and it says what it wants), and the passages to move are `vignettes/tabxplor-reg.Rmd:137`, `:504` and `vignettes/articles/tabxplor-all-else-equal.Rmd:339`, `:433`, `:458` — **with their printed numbers**.

**Three ways to check what a table actually did**, worth teaching once because they are what makes the cascade legible:

- the **header marker** — unmarked = the model's own coefficient, `m` = marginal, `ref` = at a profile;
- **`reg_formulas(t)`** — the formula and the internal `fit` key of every column, i.e. what really reached `glm()`;
- **`reg_call(t)$link` / `$measure` / `$effect`** — what the table remembers asking for.

And **`reg_measures(data, outcome, link = )`** is the discovery tool: it lists what one outcome offers at one model, with a reason wherever it offers nothing.

### 8.3 What belongs to other phases

- **`jmvtools::prepare()`** — batched at **Phase 22g** as every jamovi-visible change of Phase 22 is. Until it runs, a new YAML option is **inert**, not merely undocumented.
- **The colour ladders** — **Phase 22b-xiv-2**. ⚠ Under §4.2 three families report a **ratio** where they reported points, and `dev/reg_family_measure_effect.md` §5.3 measured that against the shipped `pct_ratio` ladder **100 % of a real marginal-RR table falls in the uncoloured slot**. *Maintainer's decision: keep this order — the tables stay grey for one phase, which is acceptable.*
- **Comparing ideal types** — **Phase 22b-xiv-3**. If it lands `at = list(…)`, §9's future trigger reopens `effect`'s name with the profile feature paying for it.
- **The French translation sweep** (`po/`) — **Phase 23f**; new msgids from this phase's messages join the existing backlog.
- **The message clean-up sweep** — **Phase 23c**; this phase writes its own aborts, 23c reviews the package's.
- **Vignette reorganisation** — **Phase 23a**; 22b-xv-2 updates the existing structure rather than reshaping it.

---

**PART III — REFERENCE**  ·  the studies, the measurements, the blast radius

## 9. Naming the argument — `effect_at`, and every alternative tested

**First, what the argument really does.** Once `link` and `measure` are fixed, it answers one question: *how is the reported number obtained from the fitted model?* — read off its parameters, or computed from its predictions and, if so, over whom.

⚠ **That is two questions fused, and the fusion is what makes naming hard.** `"coefficient"` means *do not marginalise*; `"marginal"` and `"at_reference"` mean *marginalise over X*, for two values of X. Any single-axis name therefore fits two of the three values and strains on the third. This is not a wording problem to be solved by a better word — it is a property of the value set.

**The test that settles most of it: does every value read naturally after the name?**

| candidate           | `= "coefficient"`    | `= "marginal"`    | `= "at_reference"`    | verdict                 |
|---------------------|----------------------|-------------------|-----------------------|-------------------------|
| **`effect`**        | ✓ conditional effect | ✓ marginal effect | ✓ effect at a profile | **all three read**      |
| `read`              | ✓                    | ~                 | ~                     | a verb; reads as I/O    |
| `report`            | ✓                    | ~                 | ~                     | `measure` is the report |
| `at`                | ✗                    | ~                 | ✓ `at="reference"`    | values become places    |
| `effect_at`         | ✗                    | ✗                 | ✓                     | see below               |
| `over`/`population` | ✗                    | ✓ `"sample"`      | ✓ `"reference"`       | values become places    |

**`effect` is the only name under which all three current values read**, and that is not luck: the three values *are* three kinds of effect — a conditional effect, a marginal effect, an effect at a profile — so the argument is named for what its values are. 

**On `effect_at` specifically.** ⚠ It is **stylistically fine** — tabxplor already carries `color_signif`, `conf_level`, `ci_method`, `outcome_level`, `tab_vars`, so a compound name is in keeping. It fails **semantically**: it announces that the value is a *location*, and one of the three is not. `effect_at = "coefficient"` reads as *"the effect at coefficient"*. Adopting it therefore forces the values to become locations too — `c("model", "sample", "reference")` or similar — and that is where the real cost lands.

**Renaming the values breaks type-vs-read, and that is the decisive objection.** `marginal` and `at_reference` are not free labels: they are the source of the header **marker** and of every sentence the reader sees. A user who typed `effect_at = "sample"` would read `Model_mRR` in the header and *"marginal risk ratio"* in the footer, and would have to learn that "sample" means "marginal" — **two words for one quantity**, which is the exact defect 22a-iii's vocabulary rule exists to prevent and that this phase deletes elsewhere. The marker cannot follow the rename either: `m` for marginal is the literature's own letter, and it is taught in both regression vignettes and in the article.

**And the cascade makes this the lowest-value rename available.** Under §3.1, `effect` is the **least-typed of the four arguments**: a reader reaches the marginal route by naming what they want (`measure = "difference"`), and touches `effect` only for `at_reference`, or to force an average on the model's own scale. Renaming the argument nobody types, at the cost of the vocabulary everybody reads, is the wrong trade — and it is a *worse* trade than it was before the cascade, because the cascade is what demoted the argument.

**Recommendation: keep `effect`, keep its three values, add `"auto"` as the default.** `effect_at` is not useless, but it is confusing in the one place it matters: it promises a location and delivers a route.

**The one rename genuinely worth weighing is a VALUE, not the argument: `"coefficient"` → `"conditional"`.** It is the only change that makes the list internally consistent — today it is one *artefact* (a coefficient is a model parameter) beside two *quantities* (a marginal effect, an effect at a profile), where `c("conditional", "marginal", "at_reference")` is three quantities and is the literature's own conditional / AME / MER axis. It costs **nothing** in the marker vocabulary, because the conditional reading is the *unmarked* one — no letter changes, no footer sentence moves.

⚠ Against it, and why the lean is still to keep `"coefficient"`: it is the more teachable word for the audience the package names first; it is what both vignettes and the article already teach; and **`link` now makes it concrete** — with `link` naming which coefficient the model estimates, *"the model's coefficient"* is something a reader can point at, where *"conditional"* is a property they must first be taught. Phase 22h reached the same conclusion by a different route.

**The future trigger.** If Phase 22b-xiv-3 lands ideal-type comparison as `at = list(cadre = …, ouvrier = …)`, then `at` acquires a job `effect` cannot do, and the clean shape becomes **two arguments** — `effect = c("coefficient", "marginal")` for the binary, `at =` for the population — which is the honest decomposition of the two fused questions above. That is the moment to reopen the name, with the profile feature paying for it; not now, when it would be churn for its own sake.

**Maintainer’s decision: the reading "conditional effect, marginal effect, effect at a profile" is the right reason to keep `effect` as it, and the right thing to teach in the vignettes and documentation.** It becomes an expert knob only, nearly never used in the vignettes examples, so `"conditional"` is the right call, but it’s documentation (and the small part of the vignette speaking about it) must teach that it means the model coefficients and that "auto" chooses it when the model link is the chosen measure.

---

## 10. The marginal odds ratio — what the literature actually uses it for

Asked for, because completing the prediction rectangle means offering `odds_ratio` under `marginal` / `at_reference`, and the maintainer's question was whether that is a real quantity or a white elephant. Researched rather than reasoned; the sources are in §14.

### 10.1 What it is

The odds ratio of the two **adjusted predictions**: fit the model, predict everyone's probability as if they were at the level, average; do the same at the reference; take the odds ratio of those two averages. It is the same standardization the package already performs for the marginal risk ratio, with `logit` in place of `log`. It has a settled name — the **marginal odds ratio** — a settled definition in potential-outcome terms, and a settled implementation: `marginaleffects::avg_comparisons(m, comparison = "lnoravg", transform = "exp")`, the exact sibling of the `"lnratioavg"` this package already passes.

### 10.2 The real-world use cases

**(1) Sociology — saving the odds ratio from Mood (2010).** Karlson & Jann (2023), *Sociological Science*, is a whole paper on precisely this, and its abstract states the use case: *"As sociologists are increasingly turning away from using odds ratios, reporting average marginal effects is becoming more popular. We aim to restore the use of odds ratios in sociological research by introducing marginal odds ratios. Unlike conventional odds ratios, marginal odds ratios are not affected by omitted covariates in arbitrary ways. Marginal odds ratios thus behave like average marginal effects but retain the relative effect interpretation of the odds ratio."* Their recommendation is to report it *"as a complement to the reporting of average marginal effects"*. The paper's keywords — confounding, logit, marginal effects, mediation, odds ratio, regression — are this package's own subject matter.

**(2) Comparing nested or differently-adjusted models.** This is the operational half of (1) and it is the one that bears directly on tabxplor: a conditional odds ratio moves when a covariate is added *even with nothing to confound*, so a crude-vs-adjusted OR comparison is partly arithmetic. The literature states it bluntly for the conditional case — a non-collapsible conditional estimand *"cannot be combined in any indirect treatment comparison or compared between studies because [it varies] across different covariate adjustment sets"*. The marginal odds ratio does not have that defect.

⚠ **This is the tabxplor-specific case, and it is not a small one.** `empirical = TRUE` is the package's distinctive feature, `color = "adjustment"` grades the crude-to-adjusted movement, and `reg_estimand_collapsible()` currently **refuses to test that movement on any odds-ratio column** (`R/tab_reg.R:150-151`), with the article devoting a whole section to why (`vignettes/articles/tabxplor-all-else-equal.Rmd:568-604`: an apparent ×1.6 growth in a class gap that is entirely the odds ratio rescaling itself). A **marginal** odds ratio is the one odds-flavoured measure on which that gap is a real quantity — so it is the only route by which a user who must report odds ratios can also use the package's adjustment colour honestly.

**(3) Clinical trials and regulators — the estimand framework.** ICH E9(R1) and the FDA's 2023 covariate-adjustment guidance push analysts to pre-specify whether the target is a **marginal (unconditional)** or a conditional treatment effect, precisely because of non-collapsibility for binary outcomes. Where a trial's estimand is an odds ratio, the marginal one is the covariate-adjusted quantity that answers it.

**(4) A technical advantage.** Marginal odds ratios have been shown to be **less susceptible to finite-sample bias** than conditional ones (see §14) — relevant to sparse cells, which survey subgroups produce routinely.

### 10.3 The case against, and it is strong

The whole direction of travel in applied statistics is *away* from odds ratios of every flavour. Norton, Dowd, Garrido & Maciejewski (2024), *Requiem for Odds Ratios*, records that **Health Services Research now asks authors to report marginal effects instead of odds ratios**, and recommends *"ending the reporting of odds ratios in the scientific literature for most research studies, except for case–control studies with matched samples"* — because odds ratios *"are not only confusing to non-researchers, but researchers themselves often misinterpret them"*. That is the same argument the package's own article makes at `vignettes/articles/tabxplor-all-else-equal.Rmd:212` (*"an odds ratio of 2 does not mean 'twice as likely'"*).

So the two literatures agree on the diagnosis — the **conditional** odds ratio is the problem — and disagree on the cure: marginalise it (Karlson & Jann) or retire it (Norton et al.). A reader wanting a relative effect that is comparable across models is better served by the **marginal risk ratio**, which tabxplor already offers, is easier to read, and has none of the OR's interpretive hazards. On that view the marginal odds ratio's only irreplaceable use is (1): *you must publish an odds ratio because your field does, and you want one that behaves*.

### 10.4 What it costs here

The engine change is small and generalising: `reg_gcomp_maker()`'s `ratio` **boolean becomes the measure's link**, and the arms collapse into `REG_LINK_FUNS` — one declared row per link carrying `h`, the delta-method factor `h'(M)` (`1` for identity, `1/M` for log, `1/(M(1-M))` for logit) and its DOMAIN guard. The sweep then reads

```text
est = h(M1) - h(M0)     emp = w((m1-M1)h'(M1) - (m0-M0)h'(M0))/sw     G likewise
```

with **two** code paths rather than three: the general form above, and the identity link written out so its arithmetic is bit-for-bit what it was (no AME moves by an ulp). Three readers share the table — both g-computation makers and `reg_crude_if_maker()`, whose private `switch(link, …)` this replaces. That is §2.1's rule made operational, and it is worth doing for its own sake even if only one new cell is exposed.

The crude companion is where the real cost sits, and it is **not uniform**:

| cell                                 | crude twin                                         | cost         |
|--------------------------------------|----------------------------------------------------|--------------|
| binomial × `marginal` × OR           | the existing `or` shape — vs-rest IS vs-complement | free         |
| binomial × `at_reference` × OR       | none needed (`obs` is withheld at a profile)       | free         |
| grouped_binomial, both routes        | the existing `or` / `score_odds_ratio` shapes      | free         |
| multinom. / ordinal × `at_reference` | `obs` withheld — the existing `vsrest` situation   | free         |
| **multinom. × `marginal` × OR**      | ⚠ `or` is vs the PIVOT, a predicted OR vs the REST | a new shape, |
| **ordinal × `marginal` × OR**        | ⚠ `cumor` is CUMULATIVE, not per-category          | or no `obs`  |

⚠ The last two are a genuine landmine rather than a gap: `reg_same_estimand()` compares the scale and the measure word, and both sides would read `odds_ratio` / `OR`, so **it would pair them and print the wrong contrast as the observed effect**. This is the same vs-rest / vs-pivot confusion 22b-xiii-2 fixed inside `reg_crude_if_maker()`. Whatever is decided, those two cells must either declare a per-category **vs-rest** crude shape or withhold `obs` — and withholding it deliberately relaxes the load-bearing assert at `R/reg-estimand.R:772-774`, whose own comment anticipates exactly this (*"the day an estimand needs `obs = FALSE` for another reason, this line is what must be relaxed, deliberately"*).

### 10.5 Decision

**Decided: binary only** — `binomial`, and a `trials =` summed score. Those cells are free (the existing `or` crude shape is exactly right, because on a binary outcome vs-rest *is* vs-complement), they are well attested, and they cover every use case in §10.2. The 3+ category cells are **not** offered: they would cost a deliberate relaxation of the `obs` assert, they have no literature behind them, and their crude twin would be the wrong contrast (§10.4).

The rule then reads **"an odds ratio needs a percentage and its complement"**, which honestly describes why a 3+ category outcome has to be asked "versus what?" first — and it is the same sentence that keeps `"auto"` from ever resolving to a marginal odds ratio (§3.4's clause).

---

## 11. Deleting the redundancy refusal

**Decided this session.** `status = "redundant"` and `reg_mark_redundant()` (`R/reg-estimand.R:526-561`) go; `gaussian × marginal × difference` and `poisson`/`quasipoisson × marginal × ratio` build.

Why it is right rather than merely permitted:

- **It is a live defect.** The refusal says *"averaging changes nothing"*, which §5.2 of the earlier study measured false the moment the predictor sits in an interaction or under a non-identity `shape` (AME 8 419.81 against a coefficient of 13 840.22) — and 22b-ix made interactions a first-class argument. It refuses exactly the quantity a user should report there.
- **The one-name-per-quantity rule is not breached.** That rule (22a-iii) forbids one *estimand* having two names. Here two genuinely different routes — the model's parameter, and its predictions averaged — coincide *numerically* under a collapsibility condition on some models. `at_reference` was already exempted on precisely this ground, because its `{base}` comes from its own sweep; a marginal column's `{base}` and its Constant row (a population average, not a reference profile) differ in the same way.
- **It removes a mechanism, not a check.** Gone: the post-processor, a `status` value, an abort branch, the `reg_measures()` drop (`:1084`), the `≡` marker and its legend in both regression vignettes, and the paragraph at `vignettes/tabxplor-reg.Rmd:595` that teaches the refusal. `test-reg-estimand.R:44-48` pins the resulting set and should assert the empty set instead.
- **Nothing replaces it.** No message: under §2's framing there is nothing to warn about, and 22b-vi already established that a message which changes nothing does not earn its place. That a linear model's coefficient *is* its average marginal effect stays a teaching point in the vignette — and becomes a demonstrable one, since the two columns can now be built side by side and shown to agree on an additive model and to diverge on an interacted one.

---

## 12. Blast radius — documentation, tests, jamovi

### 12.1 Documentation

Larger than the code change, and it is where the risk of a stale claim lives. In descending order of exposure:

- ✅ **`?tab_reg`** — done with the code: `@param link` added, `family` / `measure` / `effect` rewritten, the `@details` argument map re-divided (`link` under *The model*), and the `a:b` → `a*b` correction made. Both `@eval` sections regenerate themselves from the resolver.
- **`vignettes/tabxplor-reg.Rmd`** (and its French twin) — the headline rule at `:583`, the grid and its two legends at `:603-641`, the "when do the three differ" paragraph at `:595`, the "which route to take" box at `:690-692` (which already argues *for* the marginal route and becomes the rule's own statement), and the worked examples at `:183`, `:197`, `:291` whose spellings change meaning under §4.3(1).
- **`vignettes/articles/tabxplor-all-else-equal.Rmd`** — its `:407-411` order paragraph becomes *correct by construction* rather than a deliberate divergence from the help page, which is a simplification of the prose rather than a rewrite. But **every printed number in its §3 "one model, four readings" moves** if the marginal default changes, and the article quotes figures in prose in at least eight places. The Phase 22h method applies: re-run every table into a scratch file first, quote from it, then re-check the rendered HTML against it.
- ⚠ **The `effect = "marginal"` idiom moves**, and it is the largest single documentation cost of the proposal (§4.3(1)). Every passage teaching `effect = "marginal"` → percentage points becomes `measure = "difference"`: `vignettes/tabxplor-reg.Rmd:137`, `:504`, and `vignettes/articles/tabxplor-all-else-equal.Rmd:339`, `:433`, `:458`, with their printed numbers. The replacement is shorter and clearer, but it is not a rename — the numbers change too.
- ✅ **`NEWS.md`** — written with the code (the cascade, the two risk ratios, the marginal odds ratio, `reg_measures(link =)`). Check it against the vignettes rather than rewriting it.

Two stale strings still to fix in the same pass, since they sit in the paragraphs being edited: `vignettes/tabxplor-reg.Rmd:50` names `dependent`, an argument that now errors, and `:940` / `:964` print the retired `per SD/13.5` label format. (The third, `?tab_reg`'s `a:b`, went with the code.)

- **P4**: the orthogonality claim (`R/tab_reg.R:3895-3897`) becomes the true statement — `link` says what the model estimates, `measure` what is reported, and a coefficient exists only where they agree. The internal twin at `R/reg-estimand.R:1007-1008` and the `reg_effect_key()` abort's `effect × measure` grid go with it.
- The `@details` argument map (`R/tab_reg.R:3766-3778`) files `effect × measure` under *"What each cell shows"*, beside `display`. Under the cascade `link` belongs to **"The model"** and `measure` / `effect` to **"What each cell shows"** — which is finally a true division, and the misfiling this phase set out to correct.
- `vignettes/tabxplor-reg.Rmd:583` (*"`effect` and `measure` change the model that is fitted"*) becomes simply **false and easy to replace**: only `link` changes the model. Its own `:670` and `:690` already say the right thing.
- The grid's `†` legend (`:628`) claims one default per family; under §4.2 the grid gains a `link` column and the footnote goes.

### 12.2 Tests and fixtures

- `tests/testthat/test-reg-estimand.R` pins the redundant set (`:44-48`) and the grid; it becomes the derivation's own test — the composed-vs-declared sweep of §12.4(1), then the invariants.
- `tests/testthat/test-jamovi-vocabulary.R` asserts the generated JS block; `dev/generate_jamovi_js.R` must be re-run, and its `c("auto", meas[ok])` line (which never gates `"auto"`) should gate it properly once `effect = "auto"` exists.
- **No `_golden/*.rds` case builds a `tab_reg()` table**, so the regression side is golden-blind — as every phase since 22b-viii has recorded. The parity tests are what move, and only where a default moves.
- `zzz-fact-keys.R`'s `tx_fk_emp_reachable()` enumerates the reachable crude keys (26 today); it must enumerate the composed grid instead, and will grow by the new cells.

### 12.3 jamovi

Everything here is generated (`TABX_ESTIMANDS`, `TABX_DEFAULT_MEASURE`) or declared in YAML, so it costs one `jmvtools::prepare()` — already batched as **Phase 22g**. Two substantive UI changes: the radio groups reorder to measure-then-effect (§4.2), and an `auto` effect value joins the group. ⚠ Until `prepare()` runs, a YAML option the stale `.h.R` does not carry is **inert, not merely undocumented**.

### 12.4 The three claims that had to be measured, and where the measurement lives

Each was a derivation from the declared tables rather than a runtime fact, so none was trusted:

1. **The composed grid equals the declared one** where both exist — `dev/verify_estimand_library.R`, which sweeps every `(family, effect, measure)` against a snapshot of the retired table. Every difference is a named one: the three redundant cells now `ok`, the new `odds_ratio` cells, H2's `auto` defaults, and the refusals that gained a reason.
2. **What the cascade costs** — `dev/benchmarks/results_2.0.0/phase22b-xv-1_cascade_cost.txt`. Answered in §14.3: no route got slower, and one spelling got 38 % cheaper.
3. **The logit arm's interval**, against `marginaleffects::avg_comparisons(comparison = "lnoravg")` on an additive and an interacted fit: **1.1e-16** on the estimate, **1.5e-08** on the SE — the contract the `lnratioavg` arm already met. Pinned in `test-reg-estimand.R`.

---

## 13. Defects found while writing this

**(1) The modified-Poisson message is unconditional; the promotion it announces is not.** `family = "poisson"` on a binary outcome informs *"fitting a modified Poisson regression (robust standard errors) -> risk ratios"*, but the promotion only rewrites `measure` **when `measure` is `"auto"`** (`R/reg-resolve.R:424-447`). Measured on `gss_cat` with `reg_formulas()`:

| call                                                              | message says     | actually fits          |
|-------------------------------------------------------------------|------------------|------------------------|
| `family = "poisson"`                                              | modified Poisson | `rr` ✓                 |
| `family = "poisson", measure = "difference"`                      | modified Poisson | `rd` — identity link ✗ |
| `family = "poisson", effect = "marginal", measure = "difference"` | modified Poisson | `binomial` — logit ✗   |

The family is rewritten to `"binomial"` unconditionally and the message is emitted unconditionally, so the two disagree the moment an explicit `measure` is given. ✅ **Fixed in 22b-xv-1 by deletion** (decision H3): `family = "poisson"` on a binary outcome is refused, naming the two things it could have meant, and `rr_promoted` is gone.

**(2) Nothing states that a prediction route drops the link.** `effect = "marginal", measure = "ratio"` on a gaussian outcome silently runs the plain `lm`, where the same measure on the coefficient route runs the `mr` fit (§6.4's table). That is by design, but no message, `@param` or vignette line said so, and `reg_formulas()` was the only way to find out. ✅ **Fixed in 22b-xv-1** — and the cascade makes it a stronger statement than the clause this asked for: a prediction route runs on the model **`link` names**, so it is now a user's own choice rather than a silent fallback. `@param effect` says it, and points at `reg_formulas()`.

---

## 14. Three questions asked after implementation

Answered by measurement against the built package (Phase 22b-xv-1), not by reasoning.

### 14.1 Is `link` vectorised over `outcome`, and can one outcome be modelled twice?

**Vectorised: yes, identically to `family`.** All four estimand arguments go through the one slicer, `reg_per_outcome()`, so each takes a **scalar** (every outcome), a **positional vector** (aligned to `outcome`), or a **named vector** keyed by outcome — and a partial named vector defaults the rest. Verified:

```r
tab_reg(d, c(married, tvhours), c(race, age),
        family = c("binomial", "poisson"), link = c("ratio", "auto"))
#> Model_RR [married]   Model_IRR [tvhours]
tab_reg(d, c(married, tvhours), c(race, age),
        family = c(tvhours = "poisson"), link = c(married = "difference"))
#> Model_RD [married]   Model_IRR [tvhours]
```

**Naming one outcome twice: no, and it fails SILENTLY today.** `outcome` is tidy-selected, and `tidyselect::eval_select()` **deduplicates positions**, so `c(married, married)` resolves to one column before any tabxplor code sees it:

```r
tab_reg(d, c(married, married), c(race, age),
        family = "binomial", link = c("odds_ratio", "ratio"))
#> Model_OR          <- one column; the second link is dropped without a word
```

⚠ **That is a defect worth fixing**: a duplicated `outcome` should abort naming the reason, since the user plainly asked for two things and got one. It is not this phase's, and it is one guard in `reg_select_outcome()`.

**Would the surrounding features still mean anything if it worked?** Per feature:

| feature | on two links of one outcome |
|---|---|
| `empirical` / `Obs_*` | **yes** — each column pairs with a crude twin on *its own* measure, which IS the point |
| `color = "adjustment"` | **yes** — the gap is per column, model vs its own crude twin |
| `color = "between_groups"` | yes, orthogonal (it compares across `tab_vars` groups) |
| `tab_vars` | yes, orthogonal |
| `predictors = list(...)` | **no** — the model-COMPARISON axis is single-outcome AND single-estimand (see below) |
| `stats = "compare_*"` | **no** — a logit and a modified Poisson are neither nested nor likelihood-comparable |

`predictors = list(...)` varies the PREDICTORS, so every model in the list shares one `link` / `measure` / `effect`; and a likelihood-ratio or Δ-AIC comparison needs two models of the same response nested in one another, which a logit and a quasi-likelihood modified Poisson are not. So the honest shape of the feature is *"two columns side by side, no footer test"* — which is what a manual bind already gives:

```r
a <- tab_reg(d, married, c(race, age), family = "binomial")
b <- tab_reg(d, married, c(race, age), family = "binomial", link = "ratio")
dplyr::bind_cols(a, b[setdiff(names(b), c("var", "levels"))])
#> Model_OR   Model_RR   -- both ladders, both legend blocks, side by side
```

⚠ with one caveat, and it is why this is a workaround rather than the answer: the bound table keeps the **first** table's `meta` and `test`, so the footer names only the first model and the colour legend reads the first column's effect word for both. Making it first-class would mean letting `link` / `measure` / `effect` vary **within one outcome** — i.e. a second comparison axis beside `predictors`. **§14.4 works that out**: the disambiguation rule, what already exists, the blast radius and what such a table could and could not claim.

### 14.2 Should `outcome` and `family` merge?

**No.** The proposal is `outcome = c(poisson = "tvhours", binomial = "married", "age")`, on the ground that `family` has to repeat the outcome names anyway. ⚠ this is the form where the **name is the family**; §14.5 answers the corrected one, where the name is the variable. Three findings against it, in increasing weight:

**(a) The name slot on `outcome` is already taken, by tidyselect, and taking it back reverses a decision made two phases ago.** `outcome` is tidy-selected since 22b-vi, and in tidyselect a name is a **rename**:

```r
names(tidyselect::eval_select(quo(c(poisson = tvhours)), d))
#> "poisson"                       <- tidyselect renames the column
tab_reg(d, c(poisson = tvhours), ...)
#> Error: Can't rename variables in this context.
```

That error is deliberate: `tidy_select_chr(allow_rename = FALSE)` was added in 22b-vi precisely because a silent rename (`c(m = "race")` returning `"m"`, a column that does not exist) was a real trap. Making the name mean a family would give one syntax two meanings distinguished only by **whether the name happens to match a column** — the exact ambiguity that phase deleted.

**(b) The maintainer's own objection stands, and is the decisive one for the audience.** `c(poisson = "tvhours")` asks a reader to know that the *name* is the model and the *value* the variable — backwards from every other named vector in the package, where the name is the thing being described (`ref = c(age = "median")`, `shape = c(age = "quadratic")`, `multiplier = c(age = 10)`). For the literary student the package names first, that is a worse first page than one extra argument.

**(c) It would fold ONE of six per-outcome arguments and leave five.** `family`, `link`, `measure`, `effect`, `trials` and `outcome_level` are all keyed by outcome through the same slicer. Merging one buys no systematic simplification; it buys one fewer argument in the *rare* call that needs it — and `family` is auto-detected, so the case where it must be named at all is a genuine count, a forced link, or a mixed-family table, which additionally requires a character `predictors` (a list is single-outcome, hence single-family).

**What is already the minimal spelling**, and what the documentation should teach instead — a **partial named vector**, which defaults every outcome it does not name:

```r
tab_reg(d, c(married, tvhours, age), c(race, relig),
        family = c(tvhours = "poisson"))     # married and age auto-detect
```

So the repetition the question worries about is one name, in one argument, in the one call that needs it. **Decision: keep them separate.** If the surface is ever reduced, the candidate is not `outcome` + `family` but the six per-outcome arguments as a whole — and 22b-vii already weighed and rejected that shape (a `pred()`-style constructor, E6) for the predictor axis, for reasons that transfer.

### 14.3 Why did the first cost measurement read +30 %?

**Because it compared two ROUTES, not two versions.** Nothing in `tab_reg()` became slower. Measured against a `git archive HEAD` build, the same estimand asked for in each API's own words costs the same to the millisecond:

| the same estimand | old | new |
|---|---|---|
| the default call (odds ratio, coefficient) | 0.523 s | 0.495 s |
| the marginal risk difference | 0.542 s | 0.502 s |
| the modified Poisson's risk ratio | 0.834 s | 0.816 s |

The engines were **generalised, not extended**: `reg_gcomp_maker()`'s two arms became one link table, and the identity arm keeps its exact arithmetic on purpose, so no AME moves by an ulp.

**What moved is which route a SPELLING takes**, and the maintainer's guess is right — with one correction worth having, because it goes the other way from the first report:

| spelling | old route | new route | old | new |
|---|---|---|---|---|
| `measure = "ratio"` | an `rr` **refit** | a log-scale sweep on the logit fit | 0.842 s | 0.867 s |
| `measure = "difference"` | an `rd` **refit** | an identity-link sweep on the logit fit | 0.817 s | **0.509 s** |

So `measure = "difference"` got **38 % cheaper** (a sweep costs less than an identity-link binomial refit with its convergence fallback) and `measure = "ratio"` costs what its refit did. The "+30 %" of the first report was the cost of a *prediction route against a coefficient route* — real, and the right thing to know before setting `effect = "marginal"` — but it is not a regression, and it is not what any existing call now pays.

Full figures in `dev/benchmarks/results_2.0.0/phase22b-xv-1_cascade_cost.txt`.

### 14.4 One outcome, several models: `link` / `family` as a comparison axis

The proposal: **when there is a single outcome**, an unnamed multi-valued `family` or `link` means *one model per entry*, side by side.

```r
tab_reg(d, married, c(race, age), family = "binomial", link = c("odds_ratio", "ratio"))
tab_reg(d, tvhours, c(race, age), family = c("gaussian", "poisson", "binomial"), trials = 6)
```

**The disambiguation rule is clean, and it is the package's own.** `reg_per_outcome()` already reads a **named** vector as *keyed by outcome* and an **unnamed** one as the value itself; the per-predictor grammar (22b-viii) makes the same split. So the rule is one sentence — *unnamed and longer than one, with a single outcome, means one model per entry* — and it collides with nothing.

⚠ **And the slot is free because today that input is silently wrong.** Measured: `tab_reg(d, married, …, link = c("odds_ratio", "ratio"))` returns **one** column, `Model_OR`; `reg_per_outcome()` falls to `x[[i]]` with `i = 1` and the second link is dropped without a word. Implementing this therefore *fixes a silent drop* rather than repurposing a working behaviour — the same defect class as the duplicated `outcome` of §14.1.

**Most of the machinery exists.** `reg_resolve_specs()` already builds several specs from one `deps` row (the model-comparison branch) or one spec per `deps` row (the normal branch), and the normal branch's label is already `"<outcome>: <word>"` — which for two links reads `married: OR` / `married: RR`, **distinct without any new naming rule**. The implementation is: let `reg_resolve_estimands()` emit one row per (outcome × estimand) instead of per outcome.

⚠ **The blast radius is `deps$outcome` used as a unique key**, in five places that would silently take the first of a duplicate: `reg_color_notes()`'s three `setNames(…, deps$outcome)` arguments, the multinomial `match(mnl, deps$outcome)`, and — the one that matters — `families = setNames(deps$family, deps$outcome)` in the model record, which `reg_meta_estimand()` reads by name for the footer, the legend and the plots. Those would have to key by the spec **label** rather than by the outcome. Contained, but it reaches the table's own narrative record.

**What it would and would not give.** Per feature, the same audit as §14.1: `empirical` / `Obs_*` and `color = "adjustment"` work (each block pairs with a crude twin on its own measure), `tab_vars` is orthogonal, and each block keeps its **own footer gof rows** — which is the comparison a reader actually makes by eye. What it does **not** give is `stats = "compare_*"`: a logit and a modified Poisson are neither nested nor likelihood-comparable, and neither are a gaussian and a Poisson fit of the same response.

⚠ **And that footer needs one caveat or it teaches a mistake**: AIC / BIC are comparable only between models of the **same response on the same scale**. Across `family = c("gaussian", "poisson")` they are not (a density against a probability mass function), and a `trials =` grouped binomial changes the response outright. So the honest reading of such a table is *per-block* — the dispersion, the R², the checks — never a cross-block AIC ranking.

**Recommendation: coherent, and worth doing — but as its own phase, not inside 22b-xv.** It is a genuine feature (a second comparison axis) with a genuine blast radius, and 2.0.0 is at "last features before release". Two things make deferring cheap: the silent drop should be turned into an **abort** in the meantime (one guard, beside §14.1's), and the 80 % is already available as two calls plus `dplyr::bind_cols()` — with the footer caveat of §14.1.

**One cheaper variant worth weighing first.** `predictors = list(...)` is *already* the model-comparison axis, with labels, per-model specs and per-model footer rows. Letting the estimand arguments be keyed by **model label** there —

```r
tab_reg(d, married, predictors = list(logit = c(race, age), modP = c(race, age)),
        link = c(logit = "odds_ratio", modP = "ratio"))
```

— reuses that axis entirely, needs no change to `deps`, and makes "same predictors, different model" expressible. ⚠ it needs one guard of its own: `stats = "compare_*"` must be refused when the links differ, since the comparison it runs there is a likelihood-ratio test. And it needs a rule for a label that happens to equal an outcome name.

### 14.5 Removing `family`, and letting `outcome` carry it

The proposal, in its **corrected** direction — the name is the variable, the value the family:

```r
outcome = "tvhours"
outcome = c("tvhours", "age")
outcome = c(tvhours = "poisson", married = "binomial", "age")
```

**The convention is right, and better than §14.2's.** *Name = the thing described, value = its setting* is tabxplor's own (`ref = c(age = "median")`, `shape = c(age = "quadratic")`, `multiplier = c(age = 10)`), and §14.2 argued against the **inverted** form (`c(poisson = "tvhours")`), not against this one.

**And there is a precedent, in a package tabxplor already uses.** `marginaleffects::comparisons(variables = )` takes a bare character vector for "just these variables" and a **named list** for "these variables, with this contrast" — selection and configuration in one argument, exactly this shape. So the idea is not a hack in the abstract.

⚠ **But it does not survive contact with tidy-select, and that is decisive.** `outcome` is tidy-selected (22b-vi), and **tidyselect names every selection by column name**:

```r
tidyselect::eval_select(quo(c(married, tvhours)), d)
#> married tvhours
#>      10       9        <- already named, by tabxplor's own selection
```

So "does this element have a name?" **cannot** separate *the user configured a family* from *tidyselect named the column*. Telling them apart means inspecting the unevaluated expression instead of the value — which breaks the moment the outcome comes from a variable (`outcome = v`), the exact case 22b-vi made work.

Three further costs, in decreasing weight:

- **It would cost `outcome` its bare-name grammar in the configured form.** Families are strings, so `c(tvhours = poisson, age)` cannot work; the whole vector has to be quoted the moment one family is named. An argument that silently changes grammar depending on whether one element is configured is worse to teach than two arguments.
- **It removes one of six per-outcome arguments.** `link`, `measure`, `effect`, `trials` and `outcome_level` keep the separate convention, so the call still repeats outcome names wherever it needs to — there is no systematic saving, only one fewer argument in the rare call.
- **The readability gain lands on the wrong reader.** The beginner never sets `family` at all: it is auto-detected, and the message says what it detected. The person who sets it is doing something deliberate — and is exactly the reader least confused by a separate argument.

**"Difficult to program with?"** Mildly, and it fails loudly rather than silently: every base-R set operation on a character vector drops names, so an `unname()` / `setdiff()` / `union()` in a caller turns `"poisson"` into an outcome name and tidyselect aborts with *"Column `poisson` doesn't exist"*. Marginaleffects avoids even that by using a **different container** for the configured form (a list, not a named vector) — which is the only version of this idea that is safe, and which buys nothing here because `family = c(tvhours = "poisson")` already works and is shorter.

**Recommendation: keep them separate.** What the documentation should teach instead is that a **partial** named vector defaults every outcome it does not name, so the repetition is one name, in one argument, in the one call that needs it:

```r
tab_reg(d, c(married, tvhours, age), c(race, relig), family = c(tvhours = "poisson"))
```

---

## 15. References

**The marginal odds ratio**

- Karlson, K. B. & Jann, B. (2023), "Marginal Odds Ratios: What They Are, How to Compute Them, and Why Sociologists Might Want to Use Them", *Sociological Science* 10: 332–347 — <https://sociologicalscience.com/articles-v10-10-332/>
- Jann, B. & Karlson, K. B. (2023), the technical companion on estimation — <https://boris-portal.unibe.ch/bitstreams/1bb4ecbd-dc3e-4526-b263-cd5fd2d07240/download>
- Norton, E. C., Dowd, B. E., Garrido, M. M. & Maciejewski, M. L. (2024), "Requiem for Odds Ratios", *Health Services Research* 59(4) — <https://onlinelibrary.wiley.com/doi/10.1111/1475-6773.14337>
- "Decreased susceptibility of marginal odds ratios to finite-sample bias" — <https://pmc.ncbi.nlm.nih.gov/articles/PMC8338772/>
- Covariate adjustment, marginal vs conditional estimands, ICH E9(R1) / FDA 2023 — <https://pmc.ncbi.nlm.nih.gov/articles/PMC12584542/> and <https://dx.doi.org/10.1177/17407745241251568>

**The measure-first API precedent**

- `risks` (CRAN), `riskratio()` / `riskdiff()` and `approach = "auto"` → `margstd_delta` — <https://stopsack.github.io/risks/reference/riskratio.html>
- Standardized binomial models for risk ratios and differences, *International Journal of Epidemiology* 44(5): 1660 — <https://academic.oup.com/ije/article/44/5/1660/2594568>
- `marginaleffects`, `comparison = "lnoravg"` — <https://marginaleffects.com/chapters/comparisons.html>

**Fitting on one link and reporting on another (§3, §6)**

- Prentice, R. L. & Gloeckler, L. A. (1978) — the discrete-time proportional-hazards model, i.e. why `cloglog` names a hazard ratio (§2.1)

- Basu, A. & Rathouz, P. J. (2005), "Estimating marginal and incremental effects on health outcomes using flexible link and variance function models", *Biostatistics* 6(1): 93–109 — <https://academic.oup.com/biostatistics/article-abstract/6/1/93/379511>
- Manning, W. G. & Mullahy, J. (2001), and the health-cost modelling literature after it — <https://www.york.ac.uk/media/economics/documents/herc/wp/10_01.pdf>
- Rosenblum, M. & van der Laan, M. J. (2010), "Simple, Efficient Estimators of Treatment Effects in Randomized Trials Using Generalized Linear Models to Leverage Baseline Variables", *International Journal of Biostatistics* — standardization stays consistent for the marginal effect under working-model misspecification, in a randomized trial
- VanderWeele, T. J. & Knol, M. J., "A Tutorial on Interaction" — reporting on the additive and the multiplicative scale from one fit — <https://www.degruyterbrill.com/document/doi/10.1515/em-2013-0005/html>
- Zou, G. (2004), the modified Poisson — the reason `rr` is a quasi-Poisson with robust SEs rather than `binomial(link = "log")`

**Already cited in `dev/reg_family_measure_effect.md` §12** and not repeated here: Mood (2010), Norton & Dowd (2018), Williams (2012), Long & Freese (2014), Long & Mustillo (2021), King, Tomz & Wittenberg (2000), Hanmer & Kalkan (2013).
