# The regression estimand API — one rule, one derivation, one argument surface

## 0. What this document is for

Phase 22b-xiv-1. `dev/reg_family_measure_effect.md` established the vocabulary, measured the facts and listed the inconsistencies; it stopped at propositions. **This document proposes the architecture**, and it exists because the earlier study's own P3 could not be answered inside the frame it was asked in: the maintainer named two workflows — *"start from the observed base, then choose a measure of deviation"* (tabxplor's philosophy) and *"take a family, choose a model, derive from the coefficients"* (the modelling workflow) — and asked for a third way that keeps the first without lying about the second.

The claim of this document is that **there is no third way to invent: the two workflows are the two halves of one rule that the code already implements and has never stated.** Once stated, it derives the whole estimand library, it deletes three refusal mechanisms, and it settles the order of the arguments for a reason rather than a preference.

**Scope.** The estimand surface — `family`, `effect`, `measure`, their defaults, their availability, their vocabulary, and the fact tables behind them. It says nothing about the colour ladders (Phase 22b-xiv-2, `dev/reg_family_measure_effect.md` §7) or the ideal-type comparison (22b-xiv-3, §6.3), except where an ordering constraint binds them — see §10.2.

**Prerequisites.** `dev/reg_family_measure_effect.md` §1–§6 (the vocabulary and the measured facts; not restated here) and `CLAUDE.md` § *tabxplor architecture* (the regression subsystem). Per-file design is in the R headers.

**Status.** A proposal for review. §11 lists what is already decided, what is recommended, and what is left open.

---

## 1. The missing key

Three statements. The first is the idea; the second and third are the evidence that the package is already built on it.

### 1.1 A measure is a link

`difference`, `ratio` and `odds_ratio` are not three unrelated words. They are **one operation applied on three scales**: compare two numbers after transforming them by the identity, by the log, or by the logit.

| `measure`      | transform g | the comparison g(a) − g(b) | the name it has when a model is FITTED on it |
|----------------|-------------|----------------------------|----------------------------------------------|
| `"difference"` | identity    | a − b                      | identity link                                |
| `"ratio"`      | log         | log(a/b)                   | log link                                     |
| `"odds_ratio"` | logit       | log of the odds ratio      | logit link                                   |

That is the whole vocabulary, and it explains the one thing the current documentation cannot: **why `measure` sometimes changes the model and sometimes does not.**

> **THE RULE.** `measure` names a link. `effect` says whether the model is **fitted on that link** (`"coefficient"`) or whether the link is **applied afterwards to the model's predictions** (`"marginal"` / `"at_reference"`).

Everything in §2 follows from that one sentence.

`measure = "log"` is not a fourth link: it is the same estimand shown on its link scale rather than exponentiated back. It applies wherever the link is not already additive, which is exactly what `reg_estimand()` already enforces (`R/reg-estimand.R:830-835`).

⚠ **The package already says this on the observed side and nowhere else.** `REG_EMPIRICAL` declares a `link` column — `"identity"` / `"log"` / `"logit"` — on **every one of its 24 crude shapes** (`R/reg-empirical.R:477-531`). The crude column has spoken this vocabulary since 22a-ii. The model column has not, which is why the model side needed 43 hand-written rows to say what the crude side says in one column.

### 1.2 The prediction routes already fit the family's own model

This is a measured fact about the current table, not a proposal. Every `effect = "marginal"` and `effect = "at_reference"` row in `REG_ESTIMANDS` carries the family's own `fit` key:

| row                                  | `fit`        | not                       |
|--------------------------------------|--------------|---------------------------|
| `binomial` × `marginal` × `ratio`    | `"binomial"` | `"rr"` (modified Poisson) |
| `gaussian` × `marginal` × `ratio`    | `"gaussian"` | `"mr"` (log-link mean)    |
| `poisson` × `at_reference` × `ratio` | `"poisson"`  | —                         |
| every other `ame` / `vsrest` row     | the family's | —                         |

Only the **coefficient** rows ever name `rr` / `rd` / `mr`. So the second half of the rule — *"a measure changes your model only under `effect = \"coefficient\"`"* — is **already true of the running package**. It is simply never stated, never used to organise the argument surface, and contradicted by the reference vignette's own headline rule (`vignettes/tabxplor-reg.Rmd:583`: *"`effect` and `measure` change the model that is fitted"*, which its own line 670 corrects 90 lines later).

### 1.3 A model column already carries the crosstab's own pair

Since 22b-iii every regression column stores the pair a `tab()` cell stores — an adjusted level and its reference level — and derives the other geometries from it: `reg_fill_base()` writes the adjusted prediction (`R/tab_reg.R:1528`), `reg_geometry_fields()` derives `diff` and `ratio` from the pair (`R/tab_reg.R:1581-1588`), and the guard is that neither may overwrite the column's **own estimate field**, which holds what was *fitted*.

So the estimand choice, stated at its deepest:

> A regression column holds the same two numbers a crosstab cell holds. `measure` says which geometry of that pair is **promoted to the estimate** — the one that carries the interval, the star and the colour. Everything else rides as an aside, exactly as in `tab()`.

And promoting a geometry means being able to attach an interval to it, which is what the two routes are: a **fit whose coefficient is that geometry**, or a **sweep over the model's predictions** with its delta-method / influence-function variance. Nothing else in the estimand system is a real choice.

This is the reconciliation the maintainer asked for. The tabxplor workflow ("start from the observed base and pick a measure of deviation") is **literally what the prediction routes do**; the modelling workflow ("choose a model, read its coefficient") is **literally what the coefficient route does**. They are not competing framings of one API — they are the two branches of one argument, and the current surface hides which branch the user is on.

---

## 2. What follows: the availability rule, in one paragraph

> **What a measure needs.** A measure exists for an outcome when the outcome's **level** can be transformed by that measure's link: a **share** has an identity, a log and a logit, so all three; a **mean** or a **count** has no odds, so no odds ratio.
>
> **What a route adds.** Under `effect = "marginal"` / `"at_reference"` nothing more is needed: the link is applied to two predicted numbers. Under `effect = "coefficient"` the package must additionally be able to **fit** that link for that outcome, and it cannot always (there is no identity-link multinomial here).

Every current refusal, and every current hole, is one of those two clauses — and each says something a user can act on:

| what happens today                          | which clause                    | the message it can now give |
|---------------------------------------------|---------------------------------|-----------------------------|
| gaussian / poisson × OR → *not defined*     | no logit on a mean or a count   | unchanged, now derived once |
| multinom. / ordinal × coef × ratio or diff  | no such link is fitted here     | use `effect = "marginal"`   |
| poisson × coef × difference                 | same                            | same                        |
| gaussian / poisson × marginal → *redundant* | **none — a spurious mechanism** | deleted (§7)                |

⚠ **The rule is about which measures exist, not about which model a prediction route runs on.** Today a prediction route always runs on the family's own fit, so a non-default link cannot be marginalised at all — a second axis, collapsed into the first. Whether it should stay collapsed is §5.4.

The third row is the payoff for the user: today it is a bare *"tabxplor does not offer..."* plus a menu of nine lines. Under the rule it becomes one sentence naming the cure, and the cure is the route that always works.

---

## 3. The derivation: `REG_ESTIMANDS` becomes a generator

`REG_ESTIMANDS` is 43 rows over 146 declared lines plus a 21-line post-processor (`R/reg-estimand.R:370-561`), with a documented hazard — its constructor's first eight arguments are positional at all 36 call sites (`R/reg-estimand.R:324-325`). Below, every column is checked against the actual rows to see whether it is a **fact** or a **consequence**.

### 3.1 What derives

| column        | derives from                                         | verified                                  |
|---------------|------------------------------------------------------|-------------------------------------------|
| `scale`       | (level kind, measure)                                | **18/18 rows** — map below                |
| `word`        | (level kind, measure); coefficient takes the link's  | **17/18** — ordinal's `cumOR` overrides   |
| `exp`         | measure is multiplicative and not logged             | all rows                                  |
| `builder`     | `"coef"`, else `"ame"` (`"vsrest"` for a profile OR) | all rows                                  |
| `fit`         | coefficient → the family's link table; else its own  | all rows (§1.2)                           |
| `comparison`  | the measure's link, on prediction routes only        | all rows                                  |
| `obs`         | `!at_reference`                                      | **already asserted at load** (`:772-774`) |
| `engine`      | `at_reference` → `marginaleffects`, else `gcomp`     | **no row ever sets it** — all 36 default  |
| `status`      | the two clauses of §2                                | the 2 `impossible` rows and every hole    |
| `why`         | which clause failed                                  | generated, better than today's menu       |
| `note`        | (route, measure, level kind)                         | `est_note_marginal()` already does this   |
| `crude_fam`   | the measure's link + the borrow rule                 | **see §3.2 — the surprise**               |
| `crude_shape` | the measure's link + logged                          | **8/8 blocks**                            |

The scale map, read off every row:

| level kind | `difference` | `ratio`      | `odds_ratio` |
|------------|--------------|--------------|--------------|
| share      | `points`     | `pct_ratio`  | `odds_ratio` |
| mean       | `raw_diff`   | `mean_ratio` | — (no odds)  |
| count      | `raw_diff`   | `mean_ratio` | — (no odds)  |

plus `log` → `log_coef`, and the existing `REG_SCALE_GROUPED` remap for a `trials =` outcome. The word map is the same shape: share → `RD` / `RR` / `OR`, mean → `diff` / `RoM`, count → `diff` / `IRR`.

### 3.2 The crude companion derives too, and that was not expected

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

### 3.3 The residue, and the proposed tables

What is left to declare, per family, is four facts:

```r
REG_FAMILIES$binomial <- list(
  # ... display / short / ui / outcome_level, unchanged ...
  level = "share",                     # share | mean | count -> which measures exist, and the default
  fits  = c(odds_ratio = "binomial",   # measure -> the fit key. ORDER IS LOAD-BEARING: first = the
            ratio      = "rr",         # family's own link, which is what `measure = "auto"` resolves
            difference = "rd"),        # to on the coefficient route
  words = NULL,                        # per-link header override; ordinal declares c(odds_ratio = "cumOR")
  coef_note = NULL                     # per-family qualifier for the coefficient route's footer clause
)
```

and `gaussian` is `level = "mean"`, `fits = c(difference = "gaussian", ratio = "mr")`; `poisson` is `level = "count"`, `fits = c(ratio = "poisson")`; `multinomial` and `ordinal` are `level = "share"`, `fits = c(odds_ratio = <own>)`, the second declaring `words = c(odds_ratio = "cumOR")`.

Two small shared tables carry the rest:

```r
REG_MEASURE_LINK  <- c(difference = "identity", ratio = "log", odds_ratio = "logit")
REG_LEVEL_MEASURE <- list(share = c("difference","ratio","odds_ratio"),   # first = the prediction-route default
                          mean  = c("difference","ratio"),
                          count = c("difference","ratio"))
```

⚠ `REG_LEVEL_MEASURE`'s first element is **not** the prediction default under decision P8 — see §8.1, where the default is stated as its own one-line rule rather than smuggled into an ordering.

`reg_estimand(family, effect, measure)` keeps its signature and its typed-refusal contract exactly (29 positional call sites depend on the signature; every consumer reads the returned row). It stops *looking a row up* and starts *composing one*. Every foreign key in `zzz-fact-keys.R` still has something to check — it checks the composed rows instead of the declared ones, over the same enumerated grid `tx_fk_emp_reachable()` already walks (`R/zzz-fact-keys.R:88-105`).

**What this buys, concretely**: adding a family becomes one row of four facts instead of 6-8 hand-written `est_row()` calls whose 15 members must each be right; the positional-argument hazard disappears with `est_row()`; and a family cannot declare a scale, a word and a crude shape that disagree with each other, because it no longer declares them.

---

## 4. `effect = "auto"` — the study

### 4.1 What it is

One new rule, in the place where `family = "auto"` and `measure = "auto"` already live:

> `effect = "auto"` (the new default) resolves to `"coefficient"` when the measure asked for **is the family's own link**, and to `"marginal"` otherwise.

In prose, for the user: **`measure` never changes your model. The model is your outcome's usual one, and any comparison other than the one it reports by itself is worked out from its predictions. `effect = "coefficient"` is the one thing that refits.**

Nothing else about the argument changes: the three values stay, `"marginal"` keeps its `m` marker and its whole `REG_WORDS` vocabulary, and an explicit `effect =` always wins.

### 4.2 What the default call does, before and after

| call                                               | today                    | with `effect = "auto"`        |
|----------------------------------------------------|--------------------------|-------------------------------|
| `tab_reg(d, "married", x)`                         | `Model_OR` (logit coef)  | **identical**                 |
| `tab_reg(d, "married", x, measure = "log")`        | `Model_log(OR)`          | **identical**                 |
| `tab_reg(d, "married", x, measure = "ratio")`      | `Model_RR`, refits `rr`  | `Model_mRR`, on the logit fit |
| `tab_reg(d, "married", x, measure = "difference")` | `Model_RD`, refits `rd`  | `Model_mRD` (the AME)         |
| `tab_reg(d, "party3", x, measure = "difference")`  | **abort**, 9-line menu   | `Model_mRD`                   |
| `tab_reg(d, "age", x, measure = "ratio")`          | `Model_RoM`, refits `mr` | `Model_mRoM`, on the `lm`     |
| `tab_reg(d, "tvhours", x, measure = "difference")` | **abort**                | `Model_mdiff`                 |

The default table is **unchanged**, which is the point: `measure = "auto"` still resolves to the family's own link, so `effect = "auto"` still resolves to `"coefficient"`, and every existing default call, golden and vignette output is byte-identical. What moves is only the calls that name a *non-default* measure — three of which abort today.

**Maintainer’s comment: it is the very right call, because it makes the common options more direct and accessible, and the less common models with bizarre link accessible by adding effect="coefficient" (or the less used "at_reference" for that matter).**

### 4.3 Why it is more readable and more user-friendly

**(a) It removes a real trap, not a theoretical one.** Today the friendliest possible request — *"give me percentage points"* — is `measure = "difference"` on a binomial, and it silently fits an **identity-link binomial**, a model documented as *"unbounded and can fail to converge"*, with a linear-probability-model fallback and a message when it does (`R/tab_reg.R:1353-1358`). The robust answer to the same request is `effect = "marginal"`, and today the user has to know that. Under the rule they get it by asking for what they want.

**(b) One argument, one meaning.** Today `measure` means *"which arithmetic"* under two of the three effects and *"which model"* under the third — a real ambiguity that the phase title calls unreadability. Under the rule `measure` is *always* the measure of deviation, and choosing a model is the separate, explicit act of typing `effect = "coefficient"`.

**(c) It puts the arguments in the order the user can answer them.** With `effect` no longer required to be answered first, the pipeline reads `family` → `measure` → `effect`, which is exactly the order the maintainer's own article already teaches (`vignettes/articles/tabxplor-all-else-equal.Rmd:407-411`) and exactly the order `reg_measures()` and both vignette grids are already laid out in. The order stops being a documentation preference and becomes what the code does.

**(d) It is the tabxplor pipeline.** `tab()`'s user picks a measure of deviation and reads a table. Under the rule `tab_reg()`'s user does the same, and the model is a detail that only surfaces when they choose to make it one. That is P3's "shorter, more user-friendly pipeline" — obtained by *removing* a required choice, not by adding a mechanism.

**(e) It is the state of the art in the field where this exact question is settled.** See §4.7.

### 4.4 Which inconsistencies it resolves

| inconsistency (earlier study)                 | how the rule resolves it                                           |
|-----------------------------------------------|--------------------------------------------------------------------|
| §4.3 the "orthogonal" claim                   | true statement: independent on the prediction routes only      |
| §3.1 five single-option cells                 | the user never enters through `effect`; that axis is not met       |
| §5.1 "auto" is effect-keyed                   | two rules with two reasons, not a table of ten values (§8.1)       |
| §4.1 three mechanisms, one idea               | two of the three go (§7); the third becomes §2's second clause     |
| jamovi's "watch the measure change under you" | the UI leads with measure and lets effect follow, as in R          |

### 4.5 Reaching the modified Poisson, and `at_reference`

Yes to the maintainer's question, and the spelling is exactly the one proposed: **name the measure first, then upgrade the route.**

```r
tab_reg(d, "married", x, measure = "ratio")                          # marginal RR  -> Model_mRR
tab_reg(d, "married", x, measure = "ratio", effect = "coefficient")  # modified Poisson -> Model_RR
tab_reg(d, "married", x, measure = "ratio", effect = "at_reference") # at a profile -> Model_refRR
```

Three properties worth stating, because they are what makes this an upgrade path rather than two unrelated calls:

- **`effect` is monotone in commitment.** `"auto"` asks for the number; `"coefficient"` additionally asks for it to be the model's own parameter (and accepts the modelling assumption that comes with it — a constant risk ratio across profiles); `"at_reference"` additionally fixes where it is evaluated. Each step is a deliberate opt-in, and each is visible in the header (`mRR` → `RR` → `refRR`).
- **The refusal, when it comes, is now informative.** `measure = "difference"` + `effect = "coefficient"` on a multinomial cannot be fitted, and the message names the cure the user already knows how to spell: drop `effect` and take the marginal route.
- **`family = "poisson"` on a binary outcome keeps working and keeps informing** (`R/reg-resolve.R:424-433`), and its message already names `measure = "ratio"` as the canonical spelling. Under the rule that message needs one more clause — that the canonical spelling now gives the *marginal* risk ratio, and that this call is the conditional one.

### 4.6 What `measure = "auto"` resolves to

Yes — it stays the family's base-link measure on the coefficient route, so the default call does not move. The full rule is two lines, and neither is arbitrary:

> `measure = "auto"` is **the measure this route reports best**.
> On the **coefficient** route that is the model's own link — there is no choice to make, since the coefficient *is* that measure.
> On a **prediction** route it is the outcome level's own natural comparison: a **share** reads as "x times as likely" (`ratio`); a **quantity** or a **count** reads in its own units (`difference`).

This is decision **P8** restated as a derivation instead of a table. It gives exactly what P8 asked for — `ratio` for binomial / multinomial / ordinal, `difference` for gaussian / poisson — from one fact (`REG_FAMILIES$level`) that is already needed for §2's first clause.

⚠ It is still true that `"auto"` resolves differently under different routes. What changes is that this stops being *"the default depends on the other argument"* and becomes *"there are two routes, and each has one natural measure, for its own reason"* — a sentence a user can hold. And under `effect = "auto"` the user rarely meets it from the effect side at all: they name a measure, and the route follows.

### 4.7 The precedent, and it is exact

The CRAN package **`risks`** (Stopsack & Rosner; the method is *IJE* 2015) exists to answer this one question for binary outcomes, and its API is the same shape as the proposal:

- the **measure** is the entry point — `riskratio()` and `riskdiff()` are two functions, not one function with a link argument;
- the **route** is a secondary argument, `approach =`, whose values include `"robpoisson"` (a modified Poisson — tabxplor's `rr`), `"glm"` (a log-binomial or identity-link fit — tabxplor's `rd`) and `"margstd_delta"` / `"margstd_boot"` (marginal standardization after a **logistic** fit, delta-method or bootstrap intervals);
- and its default is **`approach = "auto"`, which returns `"margstd_delta"`** — marginal standardization after the logistic model — *"unless interaction terms between exposure and confounders are included, in which case `margstd_boot`"*.

So the default route to a risk ratio, in the package built for risk ratios, is **not** the modified Poisson: it is the logistic fit read through its predictions with a delta-method interval — which is precisely tabxplor's `effect = "marginal"` on the `gcomp` engine. `risks` also documents that it *"will converge whenever logistic models converge"*, which is the same convergence argument as (a) above.

The mapping is one-to-one:

| `risks`                              | tabxplor under the rule                          |
|--------------------------------------|--------------------------------------------------|
| `riskratio(approach = "auto")`       | `measure = "ratio"` (route resolves to marginal) |
| `riskratio(approach = "robpoisson")` | `measure = "ratio", effect = "coefficient"`      |
| `riskdiff(approach = "auto")`        | `measure = "difference"`                         |
| `riskdiff(approach = "glm")`         | `measure = "difference", effect = "coefficient"` |

⚠ One honest difference: `risks` switches its *engine* (bootstrap) when the model is interacted, because the delta method on a bootstrap-free standardization is the thing it is careful about. tabxplor's `gcomp` engine computes an analytic jacobian and its delta-method interval already reproduces `marginaleffects` to 1e-8 on interacted fits (the `engine` documentation at `R/reg-estimand.R:281-294`), so no engine switch is implied — but it is worth a test on an interacted model rather than an assumption.

### 4.8 Caveats — the honest list

1. **Three call spellings change meaning**, silently in the sense that no error is raised: `measure = "ratio"` and `measure = "difference"` on a binomial, and `measure = "ratio"` on a gaussian, stop refitting and become marginal. The header says so (`Model_mRR` where `Model_RR` stood) and the footer names the model, but a user who typed those calls before 2.0.0 ships gets a different estimand. **`tab_reg()` is unreleased, so nothing is owed a deprecation** — but the two regression vignettes contain worked examples on exactly these spellings (`vignettes/tabxplor-reg.Rmd:183`, `:197`, `:291`) and their prose and printed numbers must move with them.
2. **A conditional risk ratio becomes two words longer.** Epidemiological practice is the modified Poisson, and an epidemiologist reading `tab_reg(d, y, x, measure = "ratio")` will get a marginal RR unless they add `effect = "coefficient"`. Mitigation: the footer already names the fitted model, and `?tab_reg` gains one line under `measure`. The counter-argument is §4.7 — the epidemiological package's own default made the same choice.
3. **The cheap call gets more expensive.** A marginal route pays a g-computation sweep plus influence functions where a coefficient route pays a `tidy()`. Measured elsewhere in this subsystem: the always-populated auxiliary sweep costs ~0.24 s on a coefficient table (22a-ii) and the design-rung crude refit ~0.13 s (22b-xiii-2); the SE sweep is the larger term and **has not been measured for this change** — it should be, on `Arrests` (n = 5 226) and `gss_simple` (n ≈ 21 400), before the rule lands.
4. **One route becomes unreachable by `"auto"`**: a *weighted* multinomial or ordinal outcome refuses every non-`coef` builder (`R/reg-resolve.R:452-461`), so `measure = "difference"` there would resolve to a route that then aborts. The rule must consult that refusal and fall back to the coefficient route — or, better, keep the abort but say *"a weighted 3+ level outcome can only be read on its coefficients"*, which is the true statement.
5. **`effect = "auto"` is a fourth value that never appears in a header.** It resolves before anything is stamped, exactly as `family = "auto"` and `measure = "auto"` do, so `REG_CONTRASTS` and the `m` / `ref` markers are untouched. It does need one line in `REG_EFFECTS_VALUES`'s neighbourhood and one in the jamovi radio group.
6. **It does not, on its own, fix the ladder.** Under P8 a binomial marginal default is a risk ratio, and `dev/reg_family_measure_effect.md` §5.3 measured that the shipped `pct_ratio` ladder leaves **100 % of a real marginal-RR table uncoloured**. See §10.2 — this is an ordering constraint, not a caveat about the rule.

### 4.9 The alternative, for the record

Keeping `effect = "coefficient"` as the default and merely *deriving* the grid (§3) would still deliver most of the architecture: the same deletions, the same messages, the same one-paragraph rule. What it would not deliver is (a) — a bare `measure = "difference"` would keep landing on the fragile identity link — nor the measure-first pipeline. The recommendation is the rule; the derivation is worth having either way.

---

## 5. `link =` — both possibilities

The maintainer's P3 asked whether the user should more explicitly choose a model (family + link). Two shapes are possible; the evidence is set out here and the decision is left open.

### 5.1 Shape A — no `link` argument (three arguments)

`family` and `measure` + `effect = "coefficient"` name the fit between them; the link stays an internal key (`rr` / `rd` / `mr`) and is named to the user only in the footer's own words (*"modified Poisson regression"*, *"additive-risk regression (identity link, robust standard errors)"*).

**For.** Three arguments, one vocabulary, and the vocabulary is the user's (a measure of deviation) rather than the statistician's (a link function) — which matters for audience #1. It is what every user-facing package in this space does: `risks` names the measure in the function and the route in `approach`; `gtsummary` has `exponentiate`; Stata's `binreg y x, rr` names the measure. Only `stats::glm(family = binomial(link = "log"))` names the link, and that is a fitting interface, not a reporting one. It also keeps one-way dependency: `measure` constrains what `effect = "coefficient"` can do, and nothing constrains `measure`.

**Against.** The fit is chosen by a side effect of two arguments, which is harder to *discover* than an argument named after it; and an expert who thinks in links has to translate.

### 5.2 Shape B — an explicit `link =` (four arguments)

`family` + `link` name the model; `effect` + `measure` name the reading. `measure` becomes a pure selection with no second meaning anywhere.

**For.** The cleanest conceptual separation, and the one that most directly answers *"choose a model, then derive from it"*. It also makes an expert route explicit that is currently reachable only by inference, and it would let a future link (`probit`, `cloglog`) be added without touching `measure`.

**Against.** Under `effect = "coefficient"` the coefficient *is* the link's measure, so `link` and `measure` must agree — a two-way constraint replacing today's one-way one. (In practice it is mild: `measure = "auto"` under the coefficient route simply resolves to the link's own measure, and only an explicit conflicting pair need be refused.) It costs a jamovi control, a fifth per-outcome column in the Model table, and a fourth thing on the `?tab_reg` argument map.

⚠ **A first draft of this section argued that "exactly one of `link` and `measure` is live at any time". That is wrong, and §5.4 is why**: under a *prediction* route both are live and independent — `link` chooses the working model, `measure` chooses the quantity reported from it. The real argument against Shape B is narrower than the draft's, and it is about demand, not coherence.

### 5.3 A middle path, if Shape B's discoverability is what is wanted

Keep three arguments and let `measure` **accept the link words as aliases** — `measure = "logit"` / `"log"` / `"identity"` resolving to `odds_ratio` / `ratio` / `difference`. `REG_MEASURE_ALIASES` already carries 24 spellings for this exact purpose (`R/reg-estimand.R:64-71`), the foreign key that keeps every printed acronym typeable back into the argument would still hold, and an expert could then write what they think. ⚠ One collision to resolve first: `"log"` already means *un-exponentiated* in this argument, so `"log"` cannot become the log link's alias — only `"identity"` and `"logit"` are free, which makes the set incomplete and probably not worth having.

### 5.4 Fit on one link, report on another — the case that decides Shape B

**What is forbidden today, and by §4's rule as stated.** A prediction route always runs on the family's own fit (§1.2), so a non-default link is **coefficient-only**. Measured on the working tree with `reg_formulas()`:

| call                                               | family     | fit                     |
|----------------------------------------------------|------------|-------------------------|
| gaussian, `measure = "ratio"`                      | `gaussian` | `mr` — log-link PPML    |
| gaussian, `effect = "marginal", measure = "ratio"` | `gaussian` | `gaussian` — a plain lm |

So `mr`, `rr` and `rd` **cannot be marginalised at all**, and the proposal of §4 does not change that: it decides *which route* a measure takes, never *which model* a prediction route runs on. Those are two different axes, and today they are collapsed into one.

**Is the restriction principled? The literature says the two decisions are genuinely separate**, and it says so in three places.

**(1) Health economics — the canonical case, and it is not marginal.** Costs and expenditures are skewed and positive, so the standard practice since Manning & Mullahy (2001) is a GLM with a **log link** (gamma or Poisson pseudo-likelihood — tabxplor's `mr` is the latter), chosen because the mean function is multiplicative and because it avoids the retransformation problem. And then the quantity that is **reported** is the incremental or marginal effect **in dollars**. Basu & Rathouz (2005) is titled, exactly, *"Estimating marginal and incremental effects on health outcomes using flexible link and variance function models"*, and its abstract states the separation as the paper's whole point: *"Rather than focusing on the regression coefficients, the purpose of these models is inference about the mean of the outcome as a function of a set of covariates, and various functionals of the mean."* A ratio of means is one such functional; an incremental effect in the outcome's own units is another.

**(2) Epidemiology — the scale of interaction.** A model can be parsimonious on one scale and relevant on another: a logistic or log-link model with no interaction term implies interaction on the risk-difference scale, and the additive scale is the one that matters for policy — *"estimates of additive interaction are more useful than those of multiplicative interaction for identifying target subpopulations for the most effective use of resources"*. VanderWeele & Knol recommend reporting on **both** scales from one fit. tabxplor already does this in one direction (a logit fit reported as a marginal risk difference); the case is for the direction being a choice rather than a fixture.

**(3) Covariate adjustment — the working model as a nuisance.** Rosenblum & van der Laan (2010) show that regression standardization is **consistent for the marginal treatment effect even when the logistic working model is misspecified**, in a randomized trial: the link becomes an efficiency choice, not an identification one. ⚠ That robustness rests on randomization. In observational survey data — tabxplor's usual case — g-computation needs the working model to be roughly right, which **cuts in favour** of letting the user pick the better-fitting link and still report a marginal effect, not against it.

**What is actually missing, stated precisely.** Not much, and §1.3 says why. A `measure = "ratio"` coefficient column on a gaussian outcome is fitted by `mr` and **already stores its adjusted predicted means** (`reg_fill_base()`), so `display = "{est} ({base})"` shows the modelled means in dollars today, and `reg_geometry_fields()` even derives their difference into the `diff` field. What cannot be had is **an interval on that difference** — which is exactly §1.3's "which geometry is promoted to the estimate". The gap is one estimand, not a subsystem.

**Three ways to allow it, if it is wanted.**

- **(a) Shape B, `link =`.** The honest home for it: `link` names the working model, `measure` the reported quantity, and they are independent except on the coefficient route.
- **(b) Let `family` carry the link, in R's own vocabulary.** No new argument: `family` additionally accepts a **family object**, exactly as `glm()` does. Strictly better than (a), and studied on its own in **§5.5**. The package already does this once in disguise — `family = "poisson"` on a binary outcome *is* "binomial with a log link" (`R/reg-resolve.R:424-433`).
- **(c) Status quo, documented.** One sentence in `?tab_reg`: a non-default link is read on its coefficients; to report a different quantity, marginalise the family's own model instead.

**Recommendation: (c) now, and record the trigger.** The demand measured here concentrates in **one cell** — a skewed positive outcome fitted on the log scale and reported in its own units — and tabxplor's stated audiences are a sociology student and a survey researcher, not a health economist. Adding a fourth argument for one cell is the white elephant the phase exists to avoid. But the reason to say no is *demand*, not *principle*: the principle is on the other side, and this is the case that should reopen Shape B if it is ever reopened. ⚠ Whichever is chosen, the restriction should be **stated** rather than left to be discovered — today nothing tells a user that `effect = "marginal"` silently drops the link they asked for (§10.7).

### 5.5 The family object route — `family = binomial(link = "log")`

**The proposal.** `family` keeps taking a string, and additionally accepts an **R family object**:

- `family = "binomial"` — the base link. `measure` then selects on the model's predictions, and `effect = "coefficient"` changes the link to whichever one reports that measure directly.
- `family = binomial(link = "log")` — the link is **given**. `effect = "coefficient"` reads its coefficient; `measure = "difference"` reports the marginal risk difference **computed from that log-link fit**.

A readable default for everybody, expert tweaking in the vocabulary experts already know. Assessed below on the maintainer's own question: would it work, or is it a never-ending pain of exceptions?

**The package is already there, literally.** `reg_fit()`'s `switch()` **already returns family objects**, so the internal link keys ARE `(outcome, family object)` pairs:

| internal key | what it builds         | what it really is                            |
|--------------|-------------------------|-----------------------------------------------|
| `binomial`   | `binomial("logit")`    | `quasibinomial("logit")` when weighted       |
| `poisson`    | `poisson("log")`       | `quasipoisson("log")` when weighted          |
| `rr`         | `quasipoisson("log")`  | on a BINARY outcome + robust SE — Zou (2004) |
| `mr`         | `quasipoisson("log")`  | on a CONTINUOUS outcome + robust SE — PPML   |
| `rd`         | `binomial("identity")` | + robust SE                                   |

So `rr` and `mr` are **the same family object**, distinguished only by the outcome — which is exactly what §3's derivation already keys on. §3.3's `fits` column stops being a map to an opaque key and becomes a map **to a family object**, and `reg_fit()`'s `switch()` collapses into it. Another deletion, not an addition.

**Measured: the engines are already link-agnostic.** `reg_gcomp_maker()` reads `stats::family(fit)` and uses its `linkinv` / `mu.eta`, so it never knew which link it was on. Run directly on `gss_cat` (n ≈ 21 400) — the AME of Black vs White with its delta-method half-width:

| fit                    | AME      | half-width | note                                      |
|------------------------|----------|------------|--------------------------------------------|
| `binomial("logit")`    | −0.19092 | 0.01930    | today's route                             |
| `binomial("probit")`   | −0.18986 | 0.01924    | **unreachable today**                     |
| `binomial("cloglog")`  | −0.19459 | 0.01952    | **unreachable today**                     |
| `binomial("log")`      | —        | —          | ⚠ **did not converge**                    |
| `binomial("identity")` | —        | —          | ⚠ **did not converge**                    |
| `gaussian("identity")` | +3.56033 | 0.24266    | on a simulated skewed cost outcome        |
| `gaussian("log")`      | +3.56347 | 0.24270    | **unreachable today**                     |
| `Gamma("log")`         | +3.56345 | 0.26074    | **unreachable today** — the cost standard |
| `quasipoisson("log")`  | +3.56346 | 0.24863    | tabxplor's `mr`, but coefficient-only     |

Two readings, both supporting the design. The **point estimates barely move across links** while the **standard errors do** — which is §5.4(3) shown rather than argued: the link is an efficiency choice and the marginal effect is the estimand. And the engine needed **no change at all** for four families it has never been given.

**What it unlocks that neither Shape A nor Shape B does.** `binomial(link = "probit")` with `effect = "marginal"` is *the* standard way to report a probit — its coefficients are uninterpretable, its AMEs are not — and the same holds for `cloglog` (discrete-time survival). `Gamma(link = "log")` is the health-cost default (§5.4). None is reachable today, and none needs a new mechanism.

**And it makes the availability rule simpler, not harder.** With an object the link is given, so `effect = "coefficient"` needs no `fits` lookup: the measure **is** the object's link. A link that names no measure — `probit`, `cloglog`, `cauchit`, `sqrt`, `inverse` — is then **prediction-route only**, derived rather than declared. That refusal is a feature: it refuses exactly the coefficients nobody can interpret, and names the route that works.

**What does not break** — each checked, not assumed:

| subsystem                     | why it is unaffected                                          |
|-------------------------------|----------------------------------------------------------------|
| g-computation, influence fns  | reads `stats::family(fit)` — measured above                   |
| the crude companion           | keys on the **measure's** link, never the model's (§3.2)      |
| the interval                  | read off the fitted object since 22b-xiii-2                   |
| `EST_SCALES` stamping         | keys on (level kind, measure) — §3.1                          |
| survey                        | `svyglm()` takes a family object natively                     |
| the per-outcome grammar       | a **named list** of family objects already slices correctly   |

**What needs a declared rule — six, each small:**

1. **Robust vs model-based SEs.** Today keyed on the internal key; it must key on the (outcome, family) *mismatch* — a quasi-likelihood fitted on an outcome it does not describe needs a sandwich, which is what Zou (2004) requires and what `rr` / `mr` / `rd` already do.
2. **The link → measure map** for the coefficient route (`logit` → OR, `log` → ratio, `identity` → difference; everything else names none).
3. **The footer display name**, generated (*"binomial regression, log link"*) rather than looked up in `REG_FAMILIES$display`.
4. **The digest key must hash `paste(fam$family, fam$link)`**, never the object — it holds closures whose environments make a hash unstable, which would silently break jamovi's fit cache.
5. **`reg_per_outcome()` must test `inherits(x, "family")` first.** ⚠ Measured: `reg_per_outcome(binomial("log"), "married", 1, "auto")` returns **`"auto"`** today — a family object is a named list of length 13, so the named-vector branch looks for the outcome's name, misses and falls to the default. A bare family object would be **silently ignored**.
6. **The check / gof mapping** (`REG_CHECKS$families`, `reg_footer_stats()`) keys on `fam$family`.

**Where the pain actually is — three places, and only the second is open-ended:**

- ⚠ **Convergence, the sharp edge.** Measured above: `binomial("log")` and `binomial("identity")` **both failed on real data**. Those are precisely the models `rr` and `rd` exist to avoid, so the expert door hands the user the failure the shorthand was built to route around. Acceptable for an expert door — R's own `glm()` behaves identically — but it must be said in one line, and the `rd` → linear-probability-model fallback must **not** silently fire for a user-named family: an explicit family means that family or an error.
- ⚠ **Family sprawl — the real containment question.** `quasi(variance = "mu^3", link = "inverse")` is a legal family object, and every footer statistic, every model check and every crude `ci_method` assumes a known variance structure. Without a boundary, this is where the "never-ending pain" would live. **The containment is one declared column**: which `$family` names are accepted, each with its level kind (`binomial` / `quasibinomial` → share, `gaussian` / `Gamma` / `inverse.gaussian` → mean, `poisson` / `quasipoisson` → count), everything else refused by name with the list. Same shape as `REG_USER_FAMILIES` today — one row per family, not an open door.
- **3+ level outcomes have no family object.** `multinom` / `polr` / `svyVGAM` are not glm families, so the object route is glm-only and `family = "multinomial"` stays a string. An asymmetry to document, not to fix.
- **jamovi is strings-only.** Fine, with precedent: the `predictors` list and the compound formula are both R-only expert doors.

**Verdict on the maintainer's question.** It works, and it is **not** a pain of exceptions (§5.6 takes the intervals apart one by one, with measurements) — because the estimand system gets *more* uniform, not less: the object is what `reg_fit()` already constructs, the crude counterpart keys on the measure rather than on the fit, and the interval already reads the fitted object. The pain is confined to the edges (footer stats, checks, family sprawl) and all three are contained by one allow-list column. Two things must be accepted deliberately: the convergence edge, and the glm-only asymmetry.

### 5.6 Is the CI the hard part?

The maintainer's question, and the right one to press on: three intervals have to exist for every combination — the **model column's**, the **crude counterpart's**, and the **adjustment gap's** — and if each needed its own derivation the family-object route would be endless research for dozens of unused cells. Each is taken in turn below, and every claim is measured on `gss_cat` (n = 21 407) rather than argued.

**The answer up front: the interval is the part that is already done.** The inference layer was built on the glm framework from the start, so it reads facts off the fitted object rather than off a family name. What is *not* free is the closed-form **shortcuts** and the **surroundings** — the footer statistics and the model checks — and those are where the containment has to be.

**(A) The model column — free.** On the coefficient route the interval is Wald from `vcov(fit)`, and since 22b-xiii-2 `reg_wald_crit()` already reads `df.residual(fit)` and the dispersion off the fitted object. The single per-family fact is `disp_known`, today `f %in% c("binomial", "poisson")` keyed on a string; for an object it is `fam$family %in% c("binomial", "poisson")` — **R's own rule**, since `summary.glm()` fixes the dispersion at 1 for exactly those two families. One line. On the prediction route it is the delta method on the g-computation jacobian, measured in §5.5 for probit, cloglog, `Gamma("log")` and `gaussian("log")`. Zero change.

**(B) The crude counterpart — and here is the finding that decides the whole question.**

⚠ **The closed forms are not separate statistical inventions. They ARE the saturated univariable glm's own Wald interval.** Two measurements:

1. A **saturated** univariable fit has the same fitted values under *every* link — they are the observed cell proportions. Verified to 10 decimals across `logit` / `probit` / `log` / `identity` / `cloglog` on `married ~ race`.
2. Therefore the closed form and the refit agree exactly:

| crude interval, Black vs Other      | estimate | lower    | upper    |
|-------------------------------------|----------|----------|----------|
| Woolf closed form (log OR)          | 0.423707 | 0.376454 | 0.476892 |
| `glm(y ~ race, binomial("logit"))`  | 0.423707 | 0.376454 | 0.476892 |
| Katz closed form (log RR)           | 0.583758 | 0.542570 | 0.628072 |
| `glm(y ~ race, binomial("log"))`    | 0.583758 | 0.542571 | 0.628072 |

So **a new family needs no new closed form**: the refit gives the identical number, and the refit path (`reg_empirical_fit()`) already exists and was extended to the design rung in 22b-xiii-2. The closed forms stay where they are declared, as the fast exact path; anything else routes through the refit. Note also that the *saturated* log-binomial **converges** where the adjusted one fails (§5.5), because a saturated fit is just the cell means — so the crude column never meets the convergence edge that the model column does.

⚠ **And what varies across families is the variance function, not the link.** Measured on a Gamma-distributed cost outcome, the same saturated ratio of means:

| univariable fit          | RoM      | se(log)  | interval          |
|--------------------------|----------|----------|--------------------|
| `gaussian("log")`        | 1.405057 | 0.019366 | [1.3527 ; 1.4594] |
| `quasipoisson("log")`    | 1.405057 | 0.019634 | [1.3520 ; 1.4602] |
| `Gamma("log")`           | 1.405057 | 0.020346 | [1.3501 ; 1.4622] |
| `inverse.gaussian("log")`| 1.405057 | 0.021535 | [1.3470 ; 1.4656] |

The point estimate is **identical** — it is a cell mean — and only the interval moves, by about 11 % across the four. That is the containment, stated as a rule: **the crude grid is (variance function × measure), never (family × link × measure × route).** Links contribute **zero** rows.

And the refit path is already generic. The univariable AME and its SE from `reg_marginal_gcomp()`, against `marginaleffects::avg_comparisons()`:

| univariable fit       | crude AME | half-width | marginaleffects       |
|-----------------------|-----------|------------|------------------------|
| `gaussian("log")`     | +3.01909  | 0.16038    | +3.01909 / se 0.16038 |
| `Gamma("log")`        | +3.01909  | 0.17784    | +3.01909 / se 0.17784 |
| `quasipoisson("log")` | +3.01909  | 0.16688    | +3.01909 / se 0.16688 |

Exact on estimate *and* standard error, for two families the package has never been given.

**(C) The adjustment gap — already generic, and this was the surprise.** `reg_coef_if_maker()` (`R/reg-influence.R:83-101`) builds the model leg's influence function from `model.matrix(fit)`, `fit$weights` — the IRLS working weights, which is exactly where the family's variance function and the link's `mu.eta` already live — and `residuals(type = "working")`. All three exist for **every** glm. Verified: it returns a working influence function for `binomial`, `poisson`, `quasipoisson`, `Gamma`, `inverse.gaussian` and `gaussian`, with no change.

The crude leg (`reg_crude_if_maker()`, `:129`) is the *nonparametric* influence function of a weighted cell mean, so it is family-agnostic by construction; its only link-dependent piece is `gp`, i.e. **g'(μ) for `logit` / `log` / `identity`** — the **measure's** link, not the model's. ⚠ That is **the same table** as the g'(M) factor §6.4 needs for the g-computation logit arm: one declared map, two readers.

**The work map.** Which crude engines exist, per variance function × measure — the only grid that matters:

| variance function (family)      | difference       | ratio                    | odds ratio  |
|---------------------------------|------------------|--------------------------|-------------|
| binomial / quasibinomial (share)| ✓ `ame`, Wald    | ✓ `rr`, Katz             | ✓ `or`, Woolf |
| gaussian (mean)                 | ✓ `diff`, ols    | ✓ `mr`, quasi-Poisson    | n/a         |
| poisson / quasipoisson (count)  | ✓ `diff`, Welch  | ✓ `irr`, quasi-Poisson   | n/a         |
| **Gamma** (mean)                | **new**          | **new**                  | n/a         |
| **inverse.gaussian** (mean)     | **new**          | **new**                  | n/a         |

**Four new cells, and none needs new mathematics** — all four route to the refit, which is measured above to reproduce `marginaleffects` exactly. Against a naive count of family × link × measure × route (7 × ~4 × 3 × 3 ≈ 250), this is the difference between a contained change and endless tweaking.

**Cost, measured at n = 21 407**, per predictor:

| step                                        | time     |
|---------------------------------------------|----------|
| Woolf closed form                           | ~0.0 ms  |
| univariable `glm(binomial)` refit           | 2.5 ms   |
| univariable `glm(Gamma("log"))` refit       | 2.7 ms   |
| `reg_marginal_gcomp()` on it, estimate + SE | 4.9 ms   |
| `reg_coef_if_maker()` on it                 | 0.1 ms   |

So about **7.5 ms per predictor**, ~38 ms for a five-predictor table — against the +131 ms that 22b-xiii-2 measured and accepted for the design-rung crude refit. Not a performance question.

**So what IS the work?**

| item                                              | size                    | note                                   |
|---------------------------------------------------|-----------------------|-----------------------------------------|
| `disp_known` read from `fam$family`               | 1 line                  | R's own rule                           |
| the allow-list column (family → level kind)       | 1 column, ~7 rows       | **the containment**                    |
| crude engine, Gamma / inverse.gaussian            | 0 new maths — route it  | measured identical to the refit        |
| the g'(μ) map, shared with §6.4                   | 1 table, 2 readers      | needed for the marginal OR anyway      |
| robust-vs-model SE rule                           | 1 declared rule         | §5.5(1)                                |
| **footer statistics per family**                  | **the real work**       | `reg_glance()` assumes known families  |
| **model checks (`REG_CHECKS$families`)**          | **the real work**       | dispersion / linearity / influence     |
| display name, digest key, `reg_per_outcome()` guard | 3 small               | §5.5(3–5)                              |

⚠ **The endless-tweaking risk is not in the inference layer — it is in the footer and the checks**, the two subsystems carrying per-family knowledge that R itself does not supply. Both are already declared tables with a `families` column, so the containment is the same allow-list, not a second mechanism.

**Verdict, answering the question as asked.** The interval is *not* the hard part: it is the part the glm framework already solved, and the measurements above show the existing engines reproducing `marginaleffects` exactly on families the package has never been given. The gap interval is not the other hard part either: `reg_coef_if_maker()` is family-generic by construction and the crude leg keys on the measure. It is **not** endless research for dozens of useless possibilities, because the grid that matters is variance function × measure — five rows, four new cells, all routed rather than derived. It is, for the inference layer, **near done because of the pre-existing glm integration**; the remaining work is the footer statistics, the model checks and one allow-list column.

**Four honest caveats.** The four new crude cells are *routed*, not *validated* — each needs a parity test against `confint(glm())` and `marginaleffects`, and Gamma's dispersion estimate is known to be poorly behaved in small samples. `quasi()` and `MASS::negative.binomial()` stay out by the allow-list, deliberately. The closed forms must stay for the eight declared blocks — they are free, exact, and what the goldens pin. And 22b-xiii-2's rule stands unchanged: at the `design` rung the crude column is a refit whatever the family, because the closed form's independence assumption fails there and no variance function repairs it.

### 5.7 Recommendation

**Shape A now**, with §5.3 noted and not taken. But §5.5 replaces §5.4's option (a) as the answer *if* the door is ever opened: a family object is **strictly better than a `link =` argument** — no new argument, R's own vocabulary, per-outcome for free, an availability rule that gets simpler rather than harder, and a shorthand table that becomes a literal map to what the code already builds. The recommendation to wait rests on **demand**, measured in §5.4, and on nothing else; if that demand appears, the route to take is the family object, not a fourth argument.

---

## 6. The marginal odds ratio — what the literature actually uses it for

Asked for, because completing the prediction rectangle means offering `odds_ratio` under `marginal` / `at_reference`, and the maintainer's question was whether that is a real quantity or a white elephant. Researched rather than reasoned; the sources are in §12.

### 6.1 What it is

The odds ratio of the two **adjusted predictions**: fit the model, predict everyone's probability as if they were at the level, average; do the same at the reference; take the odds ratio of those two averages. It is the same standardization the package already performs for the marginal risk ratio, with `logit` in place of `log`. It has a settled name — the **marginal odds ratio** — a settled definition in potential-outcome terms, and a settled implementation: `marginaleffects::avg_comparisons(m, comparison = "lnoravg", transform = "exp")`, the exact sibling of the `"lnratioavg"` this package already passes.

### 6.2 The real-world use cases

**(1) Sociology — saving the odds ratio from Mood (2010).** Karlson & Jann (2023), *Sociological Science*, is a whole paper on precisely this, and its abstract states the use case: *"As sociologists are increasingly turning away from using odds ratios, reporting average marginal effects is becoming more popular. We aim to restore the use of odds ratios in sociological research by introducing marginal odds ratios. Unlike conventional odds ratios, marginal odds ratios are not affected by omitted covariates in arbitrary ways. Marginal odds ratios thus behave like average marginal effects but retain the relative effect interpretation of the odds ratio."* Their recommendation is to report it *"as a complement to the reporting of average marginal effects"*. The paper's keywords — confounding, logit, marginal effects, mediation, odds ratio, regression — are this package's own subject matter.

**(2) Comparing nested or differently-adjusted models.** This is the operational half of (1) and it is the one that bears directly on tabxplor: a conditional odds ratio moves when a covariate is added *even with nothing to confound*, so a crude-vs-adjusted OR comparison is partly arithmetic. The literature states it bluntly for the conditional case — a non-collapsible conditional estimand *"cannot be combined in any indirect treatment comparison or compared between studies because [it varies] across different covariate adjustment sets"*. The marginal odds ratio does not have that defect.

⚠ **This is the tabxplor-specific case, and it is not a small one.** `empirical = TRUE` is the package's distinctive feature, `color = "adjustment"` grades the crude-to-adjusted movement, and `reg_estimand_collapsible()` currently **refuses to test that movement on any odds-ratio column** (`R/tab_reg.R:150-151`), with the article devoting a whole section to why (`vignettes/articles/tabxplor-all-else-equal.Rmd:568-604`: an apparent ×1.6 growth in a class gap that is entirely the odds ratio rescaling itself). A **marginal** odds ratio is the one odds-flavoured measure on which that gap is a real quantity — so it is the only route by which a user who must report odds ratios can also use the package's adjustment colour honestly.

**(3) Clinical trials and regulators — the estimand framework.** ICH E9(R1) and the FDA's 2023 covariate-adjustment guidance push analysts to pre-specify whether the target is a **marginal (unconditional)** or a conditional treatment effect, precisely because of non-collapsibility for binary outcomes. Where a trial's estimand is an odds ratio, the marginal one is the covariate-adjusted quantity that answers it.

**(4) A technical advantage.** Marginal odds ratios have been shown to be **less susceptible to finite-sample bias** than conditional ones (see §12) — relevant to sparse cells, which survey subgroups produce routinely.

### 6.3 The case against, and it is strong

The whole direction of travel in applied statistics is *away* from odds ratios of every flavour. Norton, Dowd, Garrido & Maciejewski (2024), *Requiem for Odds Ratios*, records that **Health Services Research now asks authors to report marginal effects instead of odds ratios**, and recommends *"ending the reporting of odds ratios in the scientific literature for most research studies, except for case–control studies with matched samples"* — because odds ratios *"are not only confusing to non-researchers, but researchers themselves often misinterpret them"*. That is the same argument the package's own article makes at `vignettes/articles/tabxplor-all-else-equal.Rmd:212` (*"an odds ratio of 2 does not mean 'twice as likely'"*).

So the two literatures agree on the diagnosis — the **conditional** odds ratio is the problem — and disagree on the cure: marginalise it (Karlson & Jann) or retire it (Norton et al.). A reader wanting a relative effect that is comparable across models is better served by the **marginal risk ratio**, which tabxplor already offers, is easier to read, and has none of the OR's interpretive hazards. On that view the marginal odds ratio's only irreplaceable use is (1): *you must publish an odds ratio because your field does, and you want one that behaves*.

### 6.4 What it would cost here

The engine change is small and generalising: `reg_gcomp_maker()`'s `ratio` **boolean becomes the measure's link**, with the delta-method factor g'(M) — `1` for identity, `1/M` for log, `1/(M(1-M))` for logit — and one domain guard (0 < M < 1) mirroring the existing M > 0 one. Three arms instead of two, and the `emp` influence-function term and the `G` jacobian follow the same pattern the ratio arm already sets. That is §1.1's rule made operational, and it is worth doing for its own sake even if only one new cell is exposed.

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

### 6.5 Recommendation

**Add the engine arm; expose the cell for binary outcomes (`binomial`, and `trials =`); offer it on 3+ category outcomes only with the vs-rest contrast named in the footer and `obs` withheld.** The binary cells are free, well-attested and cover every use case in §6.2; the 3+ category ones cost a deliberate relaxation and have no literature behind them, so they are offered for the rule's completeness and for the multinomial's own "versus the rest" reading, which the package already values at the reference profile.

If the maintainer prefers to hold the line on white elephants, **binary only** is the defensible cut, and the rule then reads *"`odds_ratio` needs a share and its complement"* — which honestly describes why a 3+ category outcome has to be asked "versus what?" first.

---

## 7. The redundancy refusal — deleting it

**Decided this session.** `status = "redundant"` and `reg_mark_redundant()` (`R/reg-estimand.R:526-561`) go; `gaussian × marginal × difference` and `poisson`/`quasipoisson × marginal × ratio` build.

Why it is right rather than merely permitted:

- **It is a live defect.** The refusal says *"averaging changes nothing"*, which §3.2 of the earlier study measured false the moment the predictor sits in an interaction or under a non-identity `shape` (AME 8 419.81 against a coefficient of 13 840.22) — and 22b-ix made interactions a first-class argument. It refuses exactly the quantity a user should report there.
- **The one-name-per-quantity rule is not breached.** That rule (22a-iii) forbids one *estimand* having two names. Here two genuinely different routes — the model's parameter, and its predictions averaged — coincide *numerically* under a collapsibility condition on some models. `at_reference` was already exempted on precisely this ground, because its `{base}` comes from its own sweep; a marginal column's `{base}` and its Constant row (a population average, not a reference profile) differ in the same way.
- **It removes a mechanism, not a check.** Gone: the post-processor, a `status` value, an abort branch, the `reg_measures()` drop (`:1084`), the `≡` marker and its legend in both regression vignettes, and the paragraph at `vignettes/tabxplor-reg.Rmd:595` that teaches the refusal. `test-reg-estimand.R:44-48` pins the resulting set and should assert the empty set instead.
- **Nothing replaces it.** No message: under §1's framing there is nothing to warn about, and 22b-vi already established that a message which changes nothing does not earn its place. That a linear model's coefficient *is* its average marginal effect stays a teaching point in the vignette — and becomes a demonstrable one, since the two columns can now be built side by side and shown to agree on an additive model and to diverge on an interacted one.

---

## 8. Defaults, order and names

### 8.1 Defaults

Two derived rules replace the ten hand-written values of `REG_ESTIMANDS[[fam]]$default` (§4.6). The resulting table, which is decision **P8**:

| family      | `level` | coefficient default (its own link) | prediction default (its level's) |
|-------------|---------|------------------------------------|----------------------------------|
| gaussian    | mean    | `difference`                       | `difference`                     |
| binomial    | share   | `odds_ratio`                       | **`ratio`**                      |
| poisson     | count   | `ratio`                            | `difference`                     |
| multinomial | share   | `odds_ratio`                       | **`ratio`**                      |
| ordinal     | share   | `odds_ratio`                       | **`ratio`**                      |

⚠ Three of those five prediction defaults move (`difference` → `ratio`), and **that change is coupled to Phase 22b-xiv-2** — see §10.2.

### 8.2 Order

`family` → `measure` → `effect`, in the signature, in the `@param` order, in the `@details` argument map, and in the jamovi control order. Under §1's rule this is no longer a preference: `effect`'s question is *phrased in terms of the measure* ("should the model be fitted so its coefficients are this comparison, or should it be applied to its predictions?"), so it cannot be asked first. It also aligns the code with what `reg_measures()`, both vignette grids and the maintainer's article already do.

It is free in R: **0 of 757 `tab_reg()` call sites in this repository pass `effect` or `measure` positionally** (they are formals 7 and 8), and the maximum positional-argument count anywhere is 4. ⚠ `reg_estimand()` itself is called positionally at 29 sites, so **its** argument order must not move — keep `reg_estimand(family, effect, measure)`.

For jamovi the same reorder applies to the radio groups and to `applyModelEnables()`; `measureOffered(ui, effect, measure)` inverts into "which effects does this measure offer", and `TABX_ESTIMANDS` transposes. Under `effect = "auto"` the UI gets simpler rather than harder: the measure radios are gated by family alone, and only the effect radios need the second gate.

### 8.3 Names

**Do not rename `effect` or its values.** This was researched and settled in Phase 22h: `coefficient` / `marginal` / `at_reference` map onto the literature's own conditional / AME / MER axis, no comparable tool has a better single word, and `REG_WORDS`'s whole composition rule — the `m` marker, `Model_mRR`, "marginal risk difference" — hangs off `marginal`. `measure`'s values are equally settled and are the taught vocabulary in two vignettes and an article.

### 8.4 The `?tab_reg` corrections that fall out

- **P4**: the orthogonality claim (`R/tab_reg.R:3895-3897`) becomes the true statement — independent under the prediction routes; under `coefficient` the measure is the link, so not every measure has a coefficient route. The same claim's internal twin at `R/reg-estimand.R:1007-1008` and the `reg_effect_key()` abort's `effect × measure` grid go with it.
- The `@details` argument map (`R/tab_reg.R:3766-3778`) currently files `effect × measure` under *"What each cell shows"*, beside `display`. That is the misfiling this whole phase is about: `measure` under `effect = "coefficient"` changes the fit. Move `measure` up to *"The model"* with a one-clause qualifier, or state the rule in the group itself.
- `vignettes/tabxplor-reg.Rmd:583` (*"`effect` and `measure` change the model that is fitted"*) is false for `effect` and is contradicted by its own `:670` and `:690`. Replace it with §1's rule; `:668-682`'s "the measure chooses the model" grid is already correct and becomes the rule's illustration.
- The grid's `†` legend (`:628`) claims one default per family; it is per route. Under §8.1 the grid gains one column instead of one footnote.

---

## 9. The deletion inventory

Everything below is removed or derived, not moved. Nothing in this phase adds a mechanism.

| what                                       | where                             | why it goes                       |
|--------------------------------------------|-----------------------------------|-----------------------------------|
| 43 declared `est_row()` calls (146 lines)  | `R/reg-estimand.R:370-524`        | composed from 4 facts (§3)        |
| `est_row()` + its positional hazard        | `:324-335`                        | no rows left to write by hand     |
| `reg_mark_redundant()` + `redundant`       | `:526-561`, `:825`, `:870-873`    | decided (§7)                      |
| the `engine` column                        | `:281-294`, FK at `zzz:233`       | **no row ever sets it**; one line |
| the `obs` column                           | `:295-296`                        | asserted = `!at_reference`        |
| `est$display`                              | `:848`                            | **write-only — no reader**        |
| `reg_effect_key()`'s vestigial `measure`   | `:1011-1018`, `reg-resolve.R:441` | a retired `effect` spelling       |
| `REG_ESTIMANDS[[fam]]$default`             | `:374, :406, :438, :467, :495`    | derived from `level` + `fits`     |
| the two `status = "impossible"` rows       | `:386-388`, `:448-450`            | generated by §2's first clause    |
| the `≡` marker, its legend, its paragraph  | both reg vignettes                | the refusal is gone               |
| the effect-first `TABX_ESTIMANDS` gating   | `jamovi/js/jmvtabreg.js:902-931`  | transposed, simplified (§8.2)     |

What is **kept and untouched**: `reg_estimand()`'s signature and typed-refusal contract, every `REG_WORDS` / `REG_CONTRASTS` composition rule, `REG_MEASURE_ALIASES`, `REG_EMPIRICAL`, `EST_SCALES`, `MEASURES`, the colour engine, and every foreign key in `zzz-fact-keys.R` (which now checks composed rows over the same enumerated grid).

---

## 10. Caveats, couplings and blast radius

### 10.1 What must be measured before this is implemented

Three claims in this document are derivations from the declared tables, not runtime measurements, and each should be pinned by a test rather than trusted:

1. **The composed grid equals the declared one** where both exist. Sweep every `(family, effect, measure)` and compare the composed row member by member against the current `REG_ESTIMANDS` — the whole derivation of §3 stands or falls on this, and it is one script. Expected differences: the three redundant cells (now `ok`), the new `odds_ratio` cells, and nothing else.
2. **The cost of `effect = "auto"`.** A marginal route pays a sweep plus influence functions where a coefficient route pays a `tidy()`. Unmeasured for this change; measure on `Arrests` (n = 5 226) and `gss_simple` (n ≈ 21 400), coefficient against marginal, and record it under `dev/benchmarks/results_2.0.0/`.
3. **The logit arm's interval**, against `marginaleffects::avg_comparisons(comparison = "lnoravg", transform = "exp")`, on an additive and on an interacted fit — the same parity contract the `lnratioavg` arm already meets to 1e-8.

### 10.2 ⚠ The coupling with Phase 22b-xiv-2 — an ordering constraint, not a caveat

§8.1 makes `ratio` the prediction default for binomial / multinomial / ordinal. `dev/reg_family_measure_effect.md` §5.3 **measured** that on a common outcome a marginal risk ratio compresses toward 1 — three effects spanning 16 percentage points all printing `×1.1` — and that against the shipped `pct_ratio` ladder (1.5 / 2 / 4) **100 % of that table's cells fall in the uncoloured slot.**

So if this phase lands before 22b-xiv-2, **the package's default binomial marginal table renders entirely grey**, in a package whose thesis is that colour is how a table is read. Two of the three symptoms are 22b-xiv-2's to fix (P5's `min_digits` 1 → 2, and P6's fourth break with a recalibrated ladder). **Recommendation: land 22b-xiv-2 first, or land the two together and review the flagship tables in one pass.** This is the single most important sequencing fact in this document.

### 10.3 Documentation blast radius

Larger than the code change, and it is where the risk of a stale claim lives. In descending order of exposure:

- **`?tab_reg`** — `@param effect`, `@param measure`, the `@details` argument map, and the four corrections of §8.4. Both `@eval` sections (`reg_measures_rd()`, `reg_words_rd()`) regenerate themselves from the resolver and need no edit.
- **`vignettes/tabxplor-reg.Rmd`** (and its French twin) — the headline rule at `:583`, the grid and its two legends at `:603-641`, the "when do the three differ" paragraph at `:595`, the "which route to take" box at `:690-692` (which already argues *for* the marginal route and becomes the rule's own statement), and the worked examples at `:183`, `:197`, `:291` whose spellings change meaning under §4.8(1).
- **`vignettes/articles/tabxplor-all-else-equal.Rmd`** — its `:407-411` order paragraph becomes *correct by construction* rather than a deliberate divergence from the help page, which is a simplification of the prose rather than a rewrite. But **every printed number in its §4 "one model, four readings" moves** if the marginal default changes, and the article quotes figures in prose in at least eight places. The Phase 22h method applies: re-run every table into a scratch file first, quote from it, then re-check the rendered HTML against it.
- **`NEWS.md`** — one bullet: the estimand grid is derived, `measure` no longer changes the model unless `effect = "coefficient"`, and the marginal defaults.

Three stale strings already found and worth fixing in the same pass, since they are in the paragraphs being edited: `vignettes/tabxplor-reg.Rmd:50` names `dependent`, an argument that now errors; `:940` and `:964` print the retired `per SD/13.5` label format; and `?tab_reg`'s `@param tab_vars` (`R/tab_reg.R:3976`) tells the reader to write `a:b`, which 22b-ix refuses by name in favour of `a*b`.

### 10.4 Tests and fixtures

- `tests/testthat/test-reg-estimand.R` pins the redundant set (`:44-48`) and the grid; it becomes the derivation's own test — the composed-vs-declared sweep of §10.1(1), then the invariants.
- `tests/testthat/test-jamovi-vocabulary.R` asserts the generated JS block; `dev/generate_jamovi_js.R` must be re-run, and its `c("auto", meas[ok])` line (which never gates `"auto"`) should gate it properly once `effect = "auto"` exists.
- **No `_golden/*.rds` case builds a `tab_reg()` table**, so the regression side is golden-blind — as every phase since 22b-viii has recorded. The parity tests are what move, and only where a default moves.
- `zzz-fact-keys.R`'s `tx_fk_emp_reachable()` enumerates the reachable crude keys (26 today); it must enumerate the composed grid instead, and will grow by the new cells.

### 10.5 jamovi

Everything here is generated (`TABX_ESTIMANDS`, `TABX_DEFAULT_MEASURE`) or declared in YAML, so it costs one `jmvtools::prepare()` — already batched as **Phase 22g**. Two substantive UI changes: the radio groups reorder to measure-then-effect (§8.2), and an `auto` effect value joins the group. ⚠ Until `prepare()` runs, a YAML option the stale `.h.R` does not carry is **inert, not merely undocumented**.

### 10.6 A staging that keeps each step verifiable

1. **The derivation alone** (§3), behaviour-identical: compose the same 43 rows, assert member-by-member equality against the declared table, delete `est_row()` / `engine` / `obs` / `est$display`. Nothing user-visible moves; the sweep is the proof.
2. **Delete `redundant`** (§7). Three cells start building; one test flips to the empty set.
3. **The engine's link arm** (§6.4) with its parity test, exposing nothing yet.
4. **`effect = "auto"`** (§4) plus the derived defaults (§8.1) plus the new `odds_ratio` cells (§6.5) — the user-visible step, and the one that must be reviewed against 22b-xiv-2's ladder (§10.2).
5. **Order, names, messages, documentation** (§8, §10.3), and `dev/generate_jamovi_js.R`.

Steps 1–3 are net deletions with no output change and could land on their own if the surface decisions need more time.

### 10.7 Two defects found while writing this

**(1) The modified-Poisson message is unconditional; the promotion it announces is not.** `family = "poisson"` on a binary outcome informs *"fitting a modified Poisson regression (robust standard errors) -> risk ratios"*, but the promotion only rewrites `measure` **when `measure` is `"auto"`** (`R/reg-resolve.R:424-447`). Measured on `gss_cat` with `reg_formulas()`:

| call                                                              | message says     | actually fits          |
|-------------------------------------------------------------------|------------------|-------------------------|
| `family = "poisson"`                                              | modified Poisson | `rr` ✓                 |
| `family = "poisson", measure = "difference"`                      | modified Poisson | `rd` — identity link ✗ |
| `family = "poisson", effect = "marginal", measure = "difference"` | modified Poisson | `binomial` — logit ✗   |

The family is rewritten to `"binomial"` unconditionally and the message is emitted unconditionally, so the two disagree the moment an explicit `measure` is given. Small and self-contained: emit the message only where `rr_promoted` actually binds, or better, state what was fitted rather than what was assumed.

**(2) Nothing states that a prediction route drops the link.** `effect = "marginal", measure = "ratio"` on a gaussian outcome silently runs the plain `lm`, where the same measure on the coefficient route runs the `mr` fit (§5.4's table). That is by design, but no message, `@param` or vignette line says so, and `reg_formulas()` is the only way to find out. One clause under `@param effect`.

---

## 11. Where each decision stands

| #  | decision                                                     | status                                  |
|----|--------------------------------------------------------------|-----------------------------------------|
| 1  | Delete `status = "redundant"`; the three cells build         | **decided** (this session)              |
| 2  | Derive `REG_ESTIMANDS` from four facts per family            | recommended — §3, verified row by row   |
| 3  | `effect = "auto"`, measure-first routing                     | **studied** — §4; recommended, see §4.8 |
| 4  | `measure = "auto"` = the base-link measure on the coef route | recommended — §4.6                      |
| 5  | Prediction defaults: share → `ratio`, else `difference` (P8) | recommended — §8.1 ⚠ see §10.2          |
| 6  | Marginal OR: binary free, 3+ category with `obs` withheld    | **open** — §6; binary-only is the cut   |
| 7a | `link =` as a fourth argument                                | **open, both studied** — §5; Shape A    |
| 7b | Fit on one link, marginalise on another                      | **open** — §5.4; recommend (c) now      |
| 7c | If opened: `family = binomial(link = "log")`                 | **preferred over `link =`** — §5.5      |
| 7d | Is the interval the blocker? **No** — §5.6                   | 4 new crude cells, all routed           |
| 8  | Reorder to `family` → `measure` → `effect`                   | recommended — §8.2, free in R           |
| 9  | Keep `effect` and its three value names                      | settled in Phase 22h; not reopened      |
| 10 | Correct the "orthogonal" claim (P4)                          | **decided** earlier; falls out of §1    |

---

## 12. References

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

**Fitting on one link and reporting on another (§5.4, §5.5)**

- Basu, A. & Rathouz, P. J. (2005), "Estimating marginal and incremental effects on health outcomes using flexible link and variance function models", *Biostatistics* 6(1): 93–109 — <https://academic.oup.com/biostatistics/article-abstract/6/1/93/379511>
- Manning, W. G. & Mullahy, J. (2001), and the health-cost modelling literature after it — <https://www.york.ac.uk/media/economics/documents/herc/wp/10_01.pdf>
- Rosenblum, M. & van der Laan, M. J. (2010), "Simple, Efficient Estimators of Treatment Effects in Randomized Trials Using Generalized Linear Models to Leverage Baseline Variables", *International Journal of Biostatistics* — standardization stays consistent for the marginal effect under working-model misspecification, in a randomized trial
- VanderWeele, T. J. & Knol, M. J., "A Tutorial on Interaction" — reporting on the additive and the multiplicative scale from one fit — <https://www.degruyterbrill.com/document/doi/10.1515/em-2013-0005/html>
- Zou, G. (2004), the modified Poisson — the reason `rr` is a quasi-Poisson with robust SEs rather than `binomial(link = "log")`

**Already cited in `dev/reg_family_measure_effect.md` §10** and not repeated here: Mood (2010), Norton & Dowd (2018), Williams (2012), Long & Freese (2014), Long & Mustillo (2021), King, Tomz & Wittenberg (2000), Hanmer & Kalkan (2013).
