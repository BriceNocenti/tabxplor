
## Questions


### 1. Is `link` vectorised over `outcome`, and can one outcome be modelled twice?

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

| feature                    | on two links of one outcome                                                            |
|----------------------------|----------------------------------------------------------------------------------------|
| `empirical` / `Obs_*`      | **yes** — each column pairs with a crude twin on *its own* measure, which IS the point |
| `color = "adjustment"`     | **yes** — the gap is per column, model vs its own crude twin                           |
| `color = "between_groups"` | yes, orthogonal (it compares across `tab_vars` groups)                                 |
| `tab_vars`                 | yes, orthogonal                                                                        |
| `predictors = list(...)`   | **no** — the model-COMPARISON axis is single-outcome AND single-estimand (see below)   |
| `stats = "compare_*"`      | **no** — a logit and a modified Poisson are neither nested nor likelihood-comparable   |

`predictors = list(...)` varies the PREDICTORS, so every model in the list shares one `link` / `measure` / `effect`; and a likelihood-ratio or Δ-AIC comparison needs two models of the same response nested in one another, which a logit and a quasi-likelihood modified Poisson are not. So the honest shape of the feature is *"two columns side by side, no footer test"* — which is what a manual bind already gives:

```r
a <- tab_reg(d, married, c(race, age), family = "binomial")
b <- tab_reg(d, married, c(race, age), family = "binomial", link = "ratio")
dplyr::bind_cols(a, b[setdiff(names(b), c("var", "levels"))])
#> Model_OR   Model_RR   -- both ladders, both legend blocks, side by side
```

⚠ with one caveat, and it is why this is a workaround rather than the answer: the bound table keeps the **first** table's `meta` and `test`, so the footer names only the first model and the colour legend reads the first column's effect word for both. Making it first-class would mean letting `link` / `measure` / `effect` vary **within one outcome** — i.e. a second comparison axis beside `predictors`. **§14.4 works that out**: the disambiguation rule, what already exists, the blast radius and what such a table could and could not claim.

### 2 Should `outcome` and `family` merge?

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

### 3 Why did the first cost measurement read +30 %?

**Because it compared two ROUTES, not two versions.** Nothing in `tab_reg()` became slower. Measured against a `git archive HEAD` build, the same estimand asked for in each API's own words costs the same to the millisecond:

| the same estimand                          | old     | new     |
|--------------------------------------------|---------|---------|
| the default call (odds ratio, coefficient) | 0.523 s | 0.495 s |
| the marginal risk difference               | 0.542 s | 0.502 s |
| the modified Poisson's risk ratio          | 0.834 s | 0.816 s |

The engines were **generalised, not extended**: `reg_gcomp_maker()`'s two arms became one link table, and the identity arm keeps its exact arithmetic on purpose, so no AME moves by an ulp.

**What moved is which route a SPELLING takes**, and the maintainer's guess is right — with one correction worth having, because it goes the other way from the first report:

| spelling                 | old route         | new route                               | old     | new         |
|--------------------------|-------------------|-----------------------------------------|---------|-------------|
| `measure = "ratio"`      | an `rr` **refit** | a log-scale sweep on the logit fit      | 0.842 s | 0.867 s     |
| `measure = "difference"` | an `rd` **refit** | an identity-link sweep on the logit fit | 0.817 s | **0.509 s** |

So `measure = "difference"` got **38 % cheaper** (a sweep costs less than an identity-link binomial refit with its convergence fallback) and `measure = "ratio"` costs what its refit did. The "+30 %" of the first report was the cost of a *prediction route against a coefficient route* — real, and the right thing to know before setting `effect = "marginal"` — but it is not a regression, and it is not what any existing call now pays.

Full figures in `dev/benchmarks/results_2.0.0/phase22b-xv-1_cascade_cost.txt`.

### 4 One outcome, several models: `link` / `family` as a comparison axis

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

### 5 Removing `family`, and letting `outcome` carry it

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

### 6 Should `measure` and `color` become `deviation`?

**Recommendation: no — keep `measure`, and teach `deviation` as the umbrella it already is.** The two words are not rivals competing for one slot: they are a **noun and its object**. `deviation` is the *quantity* — how far a group sits from its reference; `measure` is *which of the three ways to express it* — subtract, divide, or divide the odds. The article already writes the pair correctly, in one sentence: *"In `tab_reg()` this choice has a name — `measure` — and it is worth reading it in full, as the **measure of deviation**."* Collapsing the phrase into one word would name an argument after the quantity while its values name the measure of the quantity.

Three further findings, in increasing weight.

**(a) The literature this package cites says *measure*.** Epidemiology's term of art is *effect measure* / *measure of association*, and every source in §14 uses it — Karlson & Jann, Norton et al., VanderWeele & Knol. 22a-iii's rule is "one name per quantity, and the discipline's name where there is one"; `measure` is that name. `deviation` is tabxplor's own philosophical umbrella, which is exactly why it belongs in the prose (`DESCRIPTION`: *"color helpers to highlight **deviations**"*; the intro vignette's *"**How to measure deviation?**"*) rather than in the argument.

**(b) The cascade's own slogan breaks.** §2.1's rule is *"a link **IS** a measure"* — the one the model estimates directly. Under the rename it becomes *"a link is a deviation"*, which is false: a link is not a deviation, it is the scale on which one is measured. That slogan is what lets `link` and `measure` share a vocabulary and keeps four arguments feeling like two; it should not be spent on a rename.

**(c) The collision with *standard deviation* is real, and it is measured.** In `vignettes/tabxplor-reg.Rmd` **6 of the 7 current occurrences of "deviation" are "standard deviation"** — and the same tables print `per 17.3 (SD)`, `(σ2.4)` and a `Residual SD` footer row, with `multiplier = "sd"` one argument away. In French the same pair is *écart* / *écart-type*. It is a documentation-discipline hazard rather than a blocker (the article carries 15 umbrella uses beside 2 SD ones without confusion), but it is a cost with nothing bought against it.

| document                                       | "deviation" | of which "standard deviation" |
|------------------------------------------------|------------:|------------------------------:|
| `vignettes/articles/tabxplor-all-else-equal.Rmd` |          17 |                             2 |
| `vignettes/tabxplor.Rmd`                        |           7 |                             1 |
| `vignettes/tabxplor-reg.Rmd`                    |           7 |                             6 |
| `DESCRIPTION`                                   |           1 |                             0 |

**And the blast radius is not small.** `measure` is 699 lines of `R/` and **33 defined identifiers** (`MEASURES`, `measure_key()`, `measure_facts()`, `measure_policy()`, `measure_own_ref()`, `EST_SCALES$label_meas`, `REG_MEASURE_LINK`, `REG_LEVEL_MEASURES`, `REG_MEASURE_ALIASES`, the exported `reg_measures()`, …). Renaming the *argument* without them would leave two words for one idea — the defect this phase deletes elsewhere — so the rename is all-or-nothing, and "all" is ~700 lines plus one exported function, close to release.

#### 6.1 `color` is a separate question, and its answer is also no — for a different reason

⚠ **`color` is genuinely overloaded, but not with `measure`.** Measured across the 13 man pages that document it, the name carries **three unrelated kinds of value**:

| sense                         | value                           | where                                    |
|-------------------------------|---------------------------------|------------------------------------------|
| which deviation, and paint it | a `MEASURES` name               | `tab()`, `tab_plain/num/counts()`, `fmt()` |
| compared **to** what          | `adjustment` / `between_groups` | `tab_reg()`, `jmvtabreg`                 |
| paint at all?                 | a bare logical                  | `tab_export/html/md/xl()`, the two plots |

Renaming sense 1 to `deviation` leaves senses 2 and 3 on `color`, and sense 2 **still takes `MEASURES` names** — so the package would end with `deviation` *and* a `color` that names measures, which is the split half-made. It also lands the whole churn on the **stable** half: `color = "<measure>"` appears **150 times in the user documentation** (76 `"difference"`, 30 `"contrib"`, 15 `"adjustment"`, 8 `"ratio"`, 8 `"between_groups"`, 6 `"odds_ratio"`) plus 36 `TRUE` / `c(TRUE, …)` spellings, against 97 for `measure = "…"` on the regression side where no back-compatibility is owed. `color` is the signature argument of the package; it is not the one to spend a rename on.

**What is worth doing instead**, and it is what the question was really pointing at: `tab(color =)` does more than paint — it sets the column's estimand. That is §8, reported as a defect rather than fixed here.

#### 6.2 The documentation rule that replaces the rename

One sentence, applied everywhere, so the two words stop drifting:

> **A deviation is the quantity a cell shows a group at; a measure is which of the three ways it is expressed.** Write *"measure of deviation"* the first time the argument appears in a document, and `measure` alone thereafter.

`dev/french_glossary.md` already fixes the pair (*écart* / *mesure (de l'écart)*), and *écart* is Cibois's own word (*les écarts à l'indépendance*), so the French twins need nothing new. What is missing is uniformity in English: `vignettes/tabxplor.Rmd` teaches *"How to measure deviation?"* for `color`, the article teaches *"measure of deviation"* for `measure`, and `vignettes/tabxplor-reg.Rmd` teaches neither. → **Phase 23a / 23b.**

---

### 7 One alias table for the measure vocabulary

**The question — "can the short aliases work while the long form is taught?" — is already the package's stated policy, in `R/reg-estimand.R`'s own words**: *"The acronyms are permanent aliases, never deprecated: the argument teaches the concept word (`ratio`), the header keeps the discipline's (`RR` / `IRR` / `RoM`)."* What is missing is not the policy but its **uniformity**: the mechanism exists **four times**, with four different coverages.

| mechanism                              | home             | resolver            | serves                  |
|----------------------------------------|------------------|---------------------|-------------------------|
| `COLOR_ALIASES`                        | `fmt_class.R`    | `measure_key()`     | `tab*(color=)`, `fmt()` |
| `REG_MEASURE_ALIASES` + `REG_LOG_BASE` | `reg-estimand.R` | `reg_measure_key()` | `tab_reg(measure=)`     |
| `REG_LINK_ALIASES`                     | `reg-estimand.R` | `reg_link_key()`    | `tab_reg(link=)`        |
| `DISPLAY_TOKENS$alias` + legacy rows   | `tab-display.R`  | `display_primary()` | `display=`              |

#### 7.1 The acceptance matrix, measured

Every cell below was run against the loaded package. `✓` builds, `✗` aborts. `set_color()` is omitted: it shares `measure_key()` with `tab(color=)` and behaves identically in every row.

| spelling                              | `tab(color=)` | `fmt(color=)` | `measure=` | `link=`      | reg `color=` |
|---------------------------------------|:-------------:|:-------------:|:----------:|:------------:|:------------:|
| `difference` / `ratio` / `odds_ratio` | ✓             | ✓             | ✓          | ✓            | ✗            |
| `diff`, `RD`, `RR`, `or`, `OR`        | ✓             | ✓             | ✓          | ✓            | ✗            |
| `rd`, `rr`                            | **✗**         | ✓ *unval.*    | ✓          | ✓            | ✗            |
| `IRR`, `irr`, `RoM`, `MR`, `mr`       | **✗**         | ✓ *unval.*    | ✓          | ✓            | ✗            |
| `risk_ratio`, `rate_ratio`            | **✗**         | ✓ *unval.*    | ✓          | ✓            | ✗            |
| `cumOR`                               | **✗**         | ✓ *unval.*    | ✓          | ✓            | ✗            |
| `log`                                 | ✗             | ✓ *unval.*    | ✓          | ✓ *log link* | ✗            |
| `log_odds`, `log_risk`, `log_rate`, … | ✗             | ✓ *unval.*    | ✓          | **✗**        | ✗            |
| `identity`, `logit`                   | ✗             | ✓ *unval.*    | ✗          | ✓            | ✗            |
| `Difference`, `DIFF`                  | ✗             | ✓ *unval.*    | **✓**      | **✓**        | ✗            |
| `contrib`                             | ✓             | ✓             | n/a        | n/a          | ✗            |
| `adjustment`, `between_groups`        | ✗ *reg-only*  | ✓             | n/a        | n/a          | ✓            |
| `diff_ci`, `after_ci`, `ci`           | ✓ *legacy*    | ✓             | ✗          | ✗            | ✗            |

Four defects fall straight out of it.

1. **`tab(color = "rr")` aborts while `tab_reg(measure = "rr")` builds.** Same word, same measure, two answers — because `COLOR_ALIASES` lists **five** non-canonical spellings (`or`, `OR`, `diff`, `RD`, `RR`) where `REG_MEASURE_ALIASES` lists **fifteen**.
2. **`reg_measure_key()` case-folds and `measure_key()` does not.** Measured: `tab_reg(measure = "Difference")` and `measure = "DIFF"` both build. That fallback makes `ODDS_RATIO`, `Rom` and `Cumor` legal too, none of which is a spelling anyone should be taught.
3. ⚠ **`fmt()` does not validate `color` at all** — `fmt(n = 1L, color = "banana")` stores `"banana"`, and `fmt(n = 1L, color = "IRR")` stores `"IRR"`; `measure_key()` then returns `NA` and the column silently colours **nothing**. `CLAUDE.md` describes the constructor chain as *"`fmt()` (public, validates) → `new_fmt()`"*, so this is a contract the code does not keep. It is also the **only** reason `measure_key()` must normalise on read: both producers already normalise at their boundary (verified — `tab(color = "OR")` stores `odds_ratio`, `tab_reg(measure = "rr")` stores `ratio`), so `fmt()` is the last un-normalised writer.
4. **`REG_MEASURE_ALIASES` is internally uneven.** It carries the lowercase twin of `OR`, `RR`, `IRR`, `MR` and `RD` but not of `RoM` or `cumOR` — an omission, not a decision.

#### 7.2 What an acronym means, and why it is safe to accept everywhere

⚠ **An acronym names a measure *as applied to one kind of level*, while the argument is level-agnostic.** `RD` is a *risk* difference (a percentage), `RoM` a ratio of *means*, `IRR` a rate ratio (a count). So a mismatched acronym has to resolve to something. Measured, it resolves to the outcome's own word and **the header says so**:

| call                                       | header prints |
|--------------------------------------------|---------------|
| `family = "gaussian", measure = "IRR"`     | `Model_mRoM`  |
| `family = "gaussian", measure = "RD"`      | `Model_diff`  |
| `family = "poisson",  measure = "RoM"`     | `Model_IRR`   |
| `family = "binomial", measure = "IRR"`     | `Model_mRR`   |
| `family = "binomial", measure = "cumOR"`   | `Model_OR`    |
| `family = "gaussian", measure = "OR"`      | **aborts** — an odds ratio needs a percentage |

That self-correction is what makes the permissive table safe and is why **no message is needed** (22b-vi: a message that changes nothing does not earn its place): the acronym is a request, the header is the answer, and the one genuinely impossible ask aborts. The same holds on `tab()`, where the legend prints the concept word (`# ratio (Total): ÷2 ÷1.5 …`), never the acronym — so `tab(color = "RD")` on a mean column cannot mislabel itself.

⚠ **The one exception is `cumOR`**, which should stay regression-only: a crosstab has no cumulative odds ratio, and its legend would print `OR` without saying that the request was silently widened.

#### 7.3 The proposal — one declared table, two scoped views

**Not one merged lookup**: `log` is a regression pseudo-measure with no colour ladder and no `MEASURES` row, and the three legacy `diff_ci` / `after_ci` / `ci` spellings carry a **policy** rather than a measure, so a single flat table would either need a hollow `MEASURES` row or a scope column that most rows do not use. The house shape is one **declared acronym table** plus the two small vocabularies that genuinely differ:

```r
# R/fmt_class.R, beside MEASURES -- THE acronym vocabulary, shared by every argument that names a
# measure. It IS the REG_WORDS set: what a header can print is what the argument can be typed.
# One entry per acronym; the all-lowercase twin is DERIVED, so a row cannot be forgotten.
MEASURE_ACRONYMS     <- c(RD = "difference", diff = "difference",
                          RR = "ratio", IRR = "ratio", RoM = "ratio",
                          OR = "odds_ratio")
MEASURE_ACRONYMS_REG <- c(cumOR = "odds_ratio")   # a crosstab has no cumulative odds ratio
# R/reg-estimand.R -- the internal fit keys, so that what reg_formulas() PRINTS is typeable back
# into `link` (measured: rr / rd / mr already round-trip; binomial / gaussian / poisson do not --
# they are the family's own link, i.e. `link = "auto"`).
REG_FIT_SPELLINGS    <- c(rr = "ratio", rd = "difference", mr = "ratio")
```

- **`measure_key()`** (colour side) reads `MEASURE_ACRONYMS` + its lowercase twins + `MEASURES` names + the three legacy policy spellings. `tab(color = "rr")`, `"IRR"`, `"RoM"` start working; nothing that works today stops.
- **`reg_measure_key()`** reads the same table + `MEASURE_ACRONYMS_REG` + the `log*` family + `auto`. Its `tolower()` fallback is **deleted**, so `Difference` / `ODDS_RATIO` / `Rom` stop being legal — a hard break on the regression side, where none is owed, in exchange for one taught vocabulary. `risk_ratio`, `rate_ratio` and `MR` go with it (none is a header word), and the fit keys move to `link`, where they mean something.
- **`reg_link_key()`** reads the shared table + `REG_FIT_SPELLINGS` + `REG_LINK_ALIASES`. The last stays separate, because `"log"` means the **log link** on `link` and *"un-exponentiated"* on `measure` — the one word the two vocabularies do not share, already documented at the declaration.
- **`DISPLAY_TOKENS`** is **not** touched, and the rule is stated once: **an acronym names a MEASURE, never a display token.** `display =` names *fields* (`or`, `diff`, `ratio`, `pct`, …), and `{est}` is already scale-relative, which is what makes a preset family-agnostic; adding `RD` / `RR` there would be a fifth vocabulary saying what `est` already says. ⚠ The overlap that exists (`or`, `diff`, `ratio` are legal in both, `rr` and `OR` are legacy display rows) is harmless — different arguments, different vocabularies — but it should be said in `?tab` rather than discovered.

**Three foreign keys**, replacing the one that exists today (*"every `REG_WORDS` acronym must be an accepted `measure` spelling"*) — each one an *"what the package printed can be typed back"* invariant:

- every `REG_WORDS` name resolves through the shared table — so **what a header prints can be typed back into `measure`**, on both producers;
- every internal fit key `reg_formulas()` prints resolves through `REG_FIT_SPELLINGS` or is a family's own link — so **what `reg_formulas()` prints can be typed back into `link`**;
- every `MEASURES` name is reachable from every argument its own `producers` column allows — which is what turns the current `tab()` / `tab_reg()` scope refusals into derived messages rather than hand-written ones.

#### 7.4 Validating `fmt()`, and what it then simplifies

`fmt()` should resolve `color` through `measure_key()` and abort on an unknown spelling, exactly as `tab()` does. Two consequences:

- **`fmt(color = "IRR")` starts colouring**, where today it silently colours nothing;
- **every stored `color` attribute becomes canonical**, so `measure_key()`'s job on read is over. It can stay — measured, it costs **4.7 µs per call** and is called **85 times building** a 324-cell table, **55 on print** and **129 in `tab_html()`**, i.e. per (column × channel × backend), never per cell — but its `WARNING: on the hot path […] keep it a lookup, never a regex` is over-cautious and should be restated for what it really guards.

⚠ The honest cost: an unknown value in `fmt(color =)` becomes an error where it was a silent no-op. That is a contract the documentation already promises, and `fmt()`'s soft-deprecated spellings (`diff_ci`, `after_ci`, `ci`) all resolve, so no released spelling breaks.

#### 7.5 Decision register

| #  | decision                                                              | status                          |
|----|-----------------------------------------------------------------------|---------------------------------|
| I1 | Rename `measure` → `deviation`                                        | **no** — §6, keep the noun/object pair |
| I2 | Rename `tab(color =)` → `deviation`                                   | **no** — §6.1                   |
| I3 | Teach *"measure of deviation"* uniformly (EN + FR)                    | **yes** — Phase 23a / 23b       |
| I4 | One declared acronym table, two scoped views                          | **yes** — §7.3                  |
| I5 | Explicit rows only; delete `reg_measure_key()`'s `tolower()` fallback  | **yes** — no case folding       |
| I6 | Lowercase twin of every acronym, **derived** rather than listed        | **yes** — §7.3                  |
| I7 | Drop `risk_ratio` / `rate_ratio` / `MR`; fit keys move to `link` | **yes** — §7.3, they are not header words |
| I8 | `cumOR` stays regression-only                                         | **yes** — §7.2                  |
| I9 | `fmt(color =)` validates and normalises                               | **yes** — §7.4                  |
| I10| Acronyms in `display =`                                         | **no** — `display` names fields, not measures |

---

### 8 Measured on the way — `tab(color =)` owns the estimand axis

**Reported, not fixed** (maintainer's call: answer the two questions, record this for a later phase).

`tab(color =)` is documented as *"which measure(s) to color, on which visual channel"*, with one half-admission — *"`color` also names the table's COMPARISON, and so decides which interval `ci = \"auto\"` builds"*. Measured, it does considerably more than that: **it sets the column's `scale`, hence what `{est}` means, which CI method is run, what the stars test and which ladder the legend prints.** It is the same axis `tab_reg(measure =)` owns.

| call                                     | stored `scale` | `ci_method` |
|------------------------------------------|----------------|-------------|
| `tab(…, pct = "row", color = "difference")` | `points`    | `newcombe`  |
| `tab(…, pct = "row", color = "ratio")`      | `pct_ratio` | `katz`      |
| `tab(…, pct = "row", color = "odds_ratio")` | `odds_ratio`| `woolf`     |
| `tab(…, tvhours, color = "difference")`     | `mean_diff` | `welch`     |
| `tab(…, tvhours, color = "ratio")`          | `mean_ratio`| `robust`    |

Two live consequences.

**(1) The estimand and the paint can disagree, under the documented default.** `color = TRUE` resolves the automatic measure **per column, after** the scale has been chosen (`resolve_col_measures()` → `auto_col_measures()`, `R/tab.R:606-631`), and `MEASURES$ratio$auto_for$text = "num"` makes `ratio` the automatic text measure of a numeric column. So the column estimates a mean **difference** and is graded on the **ratio** ladder:

```r
tab(forcats::gss_cat, race, tvhours, color = TRUE, ci = "ref", display = "est_ci", stars = TRUE)
#>   race               tvhours
#>   <fct>          <mean-diff>
#> 1 Other  -0.2*** [-0.4;-0.1]
#> …
#> # ratio (Total): ÷2 ÷1.5 ÷1.2 ÷1.1 ×1.1 ×1.2 ×1.5 ×2      <- a ladder the cells do not show
```

The cells print a mean difference with a Welch interval; the legend below them describes the ratio ladder. With `color = "ratio"` written out, everything lines up (`mean_ratio`, `robust`, `÷1.1*** [÷1.14;÷1.02]`). ⚠ **Re-checked after Phase 22b-xvi: it still reproduces.** That phase fixed the *calibration* half `dev/color_ladders_balance.md` §2.5 records — the recalibrated ratio ladder now fires (`÷2 ÷1.5 ÷1.2 ÷1.1 ×1.1 …`) — so what is left is the **scale / interval / stars mismatch**, which is not in that document and is the half that makes the legend untrue.

**(2) `display` and `color` can name two different geometries, with no reconciliation.** `tab(…, display = "ratio", color = "difference", ci = "ref")` prints `×1.01` in every cell while the scale, the interval, the stars and the legend are all the difference. That is legitimate as a *display* choice — `display` is post-hoc by contract — but nothing states which of the two the reader should believe.

**Why `tab_reg()` cannot have either defect**, and what that suggests: there, `measure` names the estimand and `color` names only what to compare it to (`TRUE`, `adjustment`, `between_groups`), so the two questions have two arguments. The same split is already **latent in `MEASURES`**: `difference` / `ratio` / `odds_ratio` set the scale, while `contrib`, `adjustment` and `between_groups` never do (measured: `color = "contrib"` leaves `scale = points`). Splitting `tab()`'s two axes the same way would fix both consequences by construction and give both producers one argument name per question — at the cost of moving 150 documented `color = "<measure>"` spellings, on the half of the package that has users.

→ **Phase 22c** owns the `tab()` argument surface and is where both belong: consequence (1) is one resolution-order fix inside `auto_col_measures()`; consequence (2) needs the axis split, which is the maintainer's call.

---
