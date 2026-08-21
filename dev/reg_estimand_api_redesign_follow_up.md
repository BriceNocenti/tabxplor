
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
