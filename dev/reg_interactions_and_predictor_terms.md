# Interactions, and the per-predictor argument surface — design study

Phase 22b-vii. **Research and design only; nothing under `R/` is changed by this study.** Every number
below was measured on this box against the running package and is reproduced verbatim in §7.

The phase covers three questions that turned out to be one: how a predictor's own settings are
spelled (Part A), what a continuous predictor's *reference* is (Part B), and how an interaction enters
a table at all (Part C). Part C's answer needs Part B's reference, and Part B's `ref` is one of Part
A's three arguments — so they are designed together and implemented together.

---

## 0. Executive summary

### 0.1 The verdict

1. **Part A — keep the three arguments, unify their grammar.** A `terms = list(age = pred(...))`
   constructor buys almost nothing once `center` is dropped (Part B removes the need for it) and
   costs a second mini-language, a jamovi redesign, and `ref` diverging from `tab()`. What is
   genuinely broken is that the three arguments accept three different grammars, one of which cannot
   express "this default for all, that one for `age`" at all. §2.
2. **Part B — make the anchor declarable, and realise it by preparing the variable, not by predicting afterwards.** `ref` extends to continuous predictors; the column is shifted to that anchor at the
   preparation boundary, so the fit's own coefficients are already anchored and the Constant row is
   the intercept again. Predicting at the profile gives the same number for that one row (measured
   identical to 2.8e-17) but **fixes only that row**: in a model with an interaction every lower-order
   term is still an effect at zero, and one of them moves by ×2.74 on the odds scale between the two
   anchorings. §3.
3. **Part C — an interaction is not a model term, it is a compound PREDICTOR.** Materialise it as a
   real variable before the fit, exactly as the maintainer proposed, and it stops being a special
   case: the row axis, the per-cell counts, the crude companion, the adjustment gap with its
   influence-function interval, the colour ladders, the legend, survey designs, multinomial and
   ordinal outcomes and the jamovi cache all work **untouched**. §4.
4. **Parts B and C are the same move, made twice.** Prepare the variable — shift it to its anchor,
   or build the pair into one column — and let the fit's own output be what the table shows. That is
   why they belong in one design: each removes a piece of post-hoc machinery that the other would
   otherwise need. §3.4, §4.5.
5. **And the colour system gains a sentence it could not say before.** Because the no-interaction
   model is already fitted for the footer test, the background channel can grade each cell's
   departure from additivity, greyed when it could be chance — one `MEASURES` row, no new `fmt`
   field, and the additive-scale twin plus a RERI footer row come with it. §4.15.

### 0.2 The missing key, in one sentence

> **An interaction is a predictor whose levels are combinations, and whose univariable model is its own
> saturated fit.**

That single sentence is what makes every subsystem work with no new machinery, because each of them
already knows what to do with a predictor:

| subsystem       | what it needs                            | what a compound predictor gives it              |
|-----------------|------------------------------------------|-------------------------------------------------|
| row model       | one row per level                        | one row per cell                                |
| `n` / `{base}`  | a count and a level per row              | the cell count, the observed and adjusted rates |
| crude companion | a univariable model on the same estimand | the saturated cell fit — closed form, exact     |
| adjustment gap  | crude and adjusted from one fitted frame | both, so the influence-function CI applies      |
| colour + stars  | an estimate, a null, an interval         | unchanged — it is the same measure              |
| survey design   | a formula `svyglm` accepts               | an ordinary factor term                         |
| jamovi cache    | a fingerprint of the prepared frame      | the new column moves the key by construction    |

The same sentence generalises one level up, and that is what ties Part B to Part C: **decide the
parametrisation while the data is being prepared, and the fit's own output is already the table.** An
anchor is that rule applied to one column, an interaction is it applied to two.

It is also, independently, the presentation that the epidemiological literature recommends: Knol &
VanderWeele (2012) ask for one row per `(G, E)` stratum against **a single common reference
category**, with the actual risks shown in the cells — which is precisely the table tabxplor already
prints for a combined factor, `{base}` asides included. §4.3.

### 0.3 What was measured

Ten scripts, all on `gss_cat_data_formatting()` (n = 12 912–12 960 complete cases). The three results
that decide the design:

- `y ~ X*M`, `y ~ M/X` and `y ~ combined_factor` are **the same fit** — identical log-likelihood and
  rank, fitted values within 5.6e-16. Choosing between them is a choice of *presentation*, never of
  statistics.
- The univariable fit of a combined factor **is** the observed cell table: its coefficients match the
  two-way closed form to **1.4e-12** (estimates) and **9.3e-8** (standard errors). The crude column
  is therefore free and exact, not an approximation.
- A combined factor already renders a complete tabxplor table today, with crude and adjusted columns,
  per-cell counts, observed and adjusted rates, stars, colours and an **adjustment gap that
  populates** (18 of 21 cells under a collapsible estimand). §7.4.

---

## 1. Decision register

Settled with the maintainer during this phase; the evidence is the section named.

| #      | decision                                                                                       | where |
|--------|------------------------------------------------------------------------------------------------|-------|
| **F1** | Keep the three arguments; unify their grammar. No `pred()` constructor.                        | §2.3  |
| **F2** | `ref` extends to continuous predictors; the default profile value is the **mean**.             | §3.3  |
| **F3** | The Constant row is anchored at the declared reference — revised below, see R1/R2/R3.          | §3.3  |
| **F4** | Under `effect = "marginal"` the Constant row is the **population-average** prediction instead. | §3.5  |
| **F5** | Interactions get a first-class argument, `cross =`; the formula hatch is not the answer.       | §4    |

⚠ **F3 was revised inside this study.** It was first written as *"the model's prediction at the
reference profile; never centre the data"*. The maintainer then asked what happens with an
interaction, and the measurement in §3.3 shows that prediction fixes the Constant row and leaves
every lower-order term anchored at zero. The decision stands — the row is anchored at the declared
reference — but the *route* is now P8.

Proposed by this study, for the maintainer to confirm:

| #       | proposal                                                                                           | where      |
|---------|----------------------------------------------------------------------------------------------------|------------|
| **P1**  | "The rest" is spelled with a reserved `default` name: `multiplier = c(default = "2sd", age = 10)`. | §2.3       |
| **P2**  | `cross` materialises a **compound predictor column**; it does not add a model term.                | §4.5       |
| **P3**  | Both parents categorical → a combined **factor**. One continuous → **nested slopes**.              | §4.6, §4.7 |
| **P4**  | The compound predictor's reference is composed from its parents' own `ref` — no new grammar.       | §4.12      |
| **P5**  | The interaction test is one extra additive fit, reusing `reg_interaction_rows()`'s pattern.        | §4.11      |
| **P6**  | The compound column is nameable in `predictors`, so a comparison can omit it.                      | §4.12      |
| **P7**  | A new colour measure grades each cell's departure from **no interaction**, on the bg channel.      | §4.15      |
| **P8**  | The anchor is realised by SHIFTING the column before the fit (R2), not by predicting after.        | §3.4       |
| **P9**  | `shape` recodes first, the anchor applies to the result; a quantile shape is never shifted.        | §3.7       |
| **P10** | A re-anchor is an exact linear reparametrisation — the jamovi fast path, not the semantics.        | §3.6       |

---

## 2. Part A — the per-predictor argument surface

### 2.1 What the three arguments accept today

Measured against the resolvers, not against the prose.

|                        | `multiplier`                                     | `shape`                   | `ref`                                                |
|------------------------|--------------------------------------------------|---------------------------|------------------------------------------------------|
| declared               | `TAB_ARGS` :633                                  | `TAB_ARGS` :634           | `TAB_ARGS` :208                                      |
| resolver               | `reg_resolve_multiplier()`                       | `reg_resolve_shape()`     | `reg_apply_references()`                             |
| defined at             | `R/tab_reg.R:293`                                | `R/reg-assumptions.R:545` | `R/tab_reg.R:565`                                    |
| default                | `"sd"`                                           | `NULL`                    | `NULL`                                               |
| bare scalar            | accepted — applies to every continuous predictor | **aborts**                | **aborts**                                           |
| fully named            | accepted                                         | accepted                  | accepted                                             |
| partially named        | names win; the rest silently revert to `"sd"`    | **aborts**                | the rest keep their own first level                  |
| unknown name           | aborts                                           | aborts                    | warns and ignores                                    |
| applies to             | continuous predictors                            | continuous predictors     | factor predictors, `tab_vars`, the multinomial pivot |
| a continuous predictor | —                                                | —                         | warns and ignores                                    |

Three grammars for one idea. The asymmetry is not cosmetic: `shape` refusing a scalar means
`shape = "quintiles"` — "cut every continuous predictor into five groups", a perfectly ordinary
request and the package's own recommended cure for non-linearity — is simply not expressible.

### 2.2 The gap that has no workaround

`reg_resolve_multiplier()` reads:

```r
scalar <- if (is.null(multiplier)) default
          else if (is.null(names(multiplier))) multiplier[[1]]
          else default
```

The moment any name is present the user's own scalar is discarded and `default` (`"sd"`) takes its
place. So "every continuous predictor per 2 SD, except `age` per decade" cannot be written: the only
way to reach it is to name every predictor individually. `?tab_reg` describes this accurately
("a named vector **overriding chosen ones**"), so it is a design gap rather than a bug — but it is
the gap the uniform grammar has to close.

### 2.3 The uniform grammar

One rule, three arguments, and one shared resolver:

- a **bare scalar** is the value for every predictor the argument applies to;
- a **named element** overrides that predictor;
- the reserved name **`default`** sets the fallback for everything not named;
- a name that matches no eligible predictor is an error naming the eligible set;
- anything not mentioned keeps the package default.

```r
multiplier = "2sd"                            # all continuous predictors
multiplier = c(default = "2sd", age = 10)     # all per 2 SD, age per decade
shape      = "quintiles"                      # cut every continuous predictor
shape      = c(default = "quintiles", age = "quadratic")
ref        = c(race = "Black", age = 0)       # a level for a factor, a value for a numeric (Part B)
```

`default` is preferred over a bare unnamed element inside a named vector (`c(2, age = 10)`) for two
reasons: a partially-named vector is exactly the shape `shape` rejects today, so it carries a history
of meaning "malformed"; and `default` reads the same way in the `list()` form a future argument might
need. The one cost is that a factor level genuinely called `"default"` cannot be a variable name —
which it cannot be anyway, since these are variable names, not levels.

**Why not a fact table of knobs.** The declarative rule is "a fact stated once, in one table". Here
the facts are already stated once: each argument has a `TAB_ARGS` row, and its vocabulary lives in
`REG_SHAPES` / `REG_MULTIPLIER_KEYWORDS` / the level set of the data. What is duplicated is the
*parsing*, three times — so the right object is one shared resolver
(`reg_resolve_per_predictor(value, eligible, default, what)`), not a new table. A knob table would
earn its place only if a fourth and fifth knob appeared; `cross` (§4) makes four, which is worth
re-testing at implementation time but is not enough on its own.

### 2.4 jamovi

Nothing forces a change. The three per-variable controls are already `Array` of
`Group{var, value}` (`jamovi/jmvtabreg.a.yaml:273`, `:286`, `:299`) and their folders
(`jmvtab_reg_ref_vector()`, `jmvtab_reg_shape_vector()`, `jmvtab_reg_mult_vector()` in
`R/jmvtabreg-cache.R`) already emit fully-named vectors, which the uniform grammar accepts unchanged.
A `default` row could be offered later as one more entry with an empty `var`; it is not required.

### 2.5 Defects found, reported not fixed

- **A2-1 — `reg_reference_grid_values()` uses an unweighted `mean()`** (`R/tab_reg.R:1425-1433`)
  while `multiplier` and `shape` measure their centre with `reg_weighted_mean()` on the frozen frame
  (`R/reg-resolve.R:530-563`). With weights the reference profile therefore sits at a different place
  from the unit the same table prints. Part B makes this visible, since the profile starts driving a
  printed row. More generally, do avoid these inconsistencies, **you must check that the weighted path always use weighted means**. For example, is `multiplier = "sd"` a weighted sd when `wt` is provided ?
- **A2-2 — `shape` rejects a partially-named vector** (`R/reg-assumptions.R:547`), because the same
  guard tests `is.null(names())` and `!all(nzchar(names()))`. The uniform grammar needs the second
  half of that test removed.

---

## 3. Part B — the reference profile and the Constant row

### 3.1 What exists

Two different profiles coexist, and neither is declarable.

- The **Constant row** is the fitted equation's intercept: every factor at its reference level, every
  continuous predictor at **zero** (`reg_column()`, `R/tab_reg.R:1256-1272`). For an age or an
  income, zero is outside the data.
- The **reference grid** that `effect = "at_reference"` evaluates at puts a continuous predictor at
  its **mean** (`reg_reference_grid_values()`, `R/tab_reg.R:1425-1433`).

So one table can contain both conventions, and **`?tab_reg` promises the second while the Constant row delivers the first**. The row is also empty under `effect = "marginal"` / `"at_reference"` (there is no intercept in a sample-averaged table) and for an ordinal outcome (a cumulative logit has thresholds, not one intercept).

The main goal of this phase is to ensure **every `effect =` have all quantities calculated at the right and relevant point for it**.

### 3.2 The measurement that decides it

On `married ~ race + relig + age` (logistic, n = 12 960), with **no interaction**:

| quantity                                | estimate      | standard error | as an odds |
|-----------------------------------------|---------------|----------------|------------|
| raw intercept (age = 0)                 | -0.8209236    | 0.06845486     | 0.44       |
| intercept after centring `age`          | 0.2369388     | 0.02878313     | 1.2674     |
| **prediction at the reference profile** | **0.2369388** | **0.02878313** | **1.2674** |

The centred intercept and the profile prediction are the same number to the last printed digit, the
standard error included — as they must be, since on the link scale a prediction is a linear function
of the coefficients and `x'Vx` is exactly its variance, with no delta-method approximation involved.
The raw intercept is off by a factor **2.88** on the odds scale. The identity survives an interaction
too: on `married ~ race * age + relig` the two agree to 2.8e-17.

**So for the Constant row alone the two routes are interchangeable. They are not interchangeable for
the table.**

### 3.3 Why predicting is not enough — the interaction case

A prediction fixes the row it is computed for. In a model with an interaction the anchoring problem is
not confined to the intercept: **every lower-order term is an effect at zero**, and a prediction does
nothing for those rows, because they come from the tidied coefficient.

Measured on `married ~ race * age + relig`:

| `race` level | raw coefficient | as OR | centred coefficient | as OR     | moves by  |
|--------------|-----------------|-------|---------------------|-----------|-----------|
| Black        | -1.0957954848   | 0.334 | -0.9119008590       | 0.402     | ×1.20     |
| Other        | -0.8587570881   | 0.424 | +0.1507274276       | **1.163** | **×2.74** |

The `Other` row goes from "0.42, a strong negative effect" to "1.16, essentially nothing" — and the
raw number is the one tabxplor prints today (§7.3 shows it in a real table, at OR 2.36 / 2.99 in
another specification). A table whose Constant row is anchored at the mean while its `race` rows are
anchored at age = 0 is *internally inconsistent*, which is worse than being uniformly wrong.

There is a route that fixes those rows without touching the data — evaluate each row as a **contrast
at the profile**, `L'β` with `L` the difference of two model-matrix rows. Measured, it gives exactly
the centred coefficient: agreement **1.58e-15** on estimates and **6.94e-17** on standard errors. So
the three routes all produce one set of numbers, and the choice is purely about plumbing:

| route                                  | what it fixes                    | what it costs                                    |
|----------------------------------------|----------------------------------|--------------------------------------------------|
| **R1** prediction at the profile       | the Constant row only            | leaves every lower-order term at zero — rejected |
| **R2** shift the column before the fit | every row, from the fit's output | a refit; two readers need un-shifting            |
| **R3** reparametrise the fitted object | every row, exactly               | cannot produce profile-likelihood bounds         |

### 3.4 The verdict — prepare the variable, let the fit do its job

**R2.** It is the same move Part C makes for interactions, and the reason is the same: if the
parametrisation is decided *before* the fit, then the fit's own output is already what the table
shows, and no consumer downstream needs a contrast engine, a special Constant arm, or a rule about
which rows are anchored. One pipeline stage — beside `reg_shape_apply()`, which already rewrites
columns at that exact boundary — replaces machinery in `reg_column()`, in the tidy step and in every
family's arm.

This overturns four of the five arguments the first draft of this study made for predicting, and it is
worth saying why each fails, because they are the arguments a future reader will re-invent:

1. *"No refit."* There is no extra fit: the model is fitted once, on the prepared data. Measured, the
   refit is exact for `glm` and `svyglm` (6.9e-18) and drifts by **4.6e-07** (`multinom`) and
   **6.9e-07** (`polr`) — iterative optimisers restarted on a shifted design. Invisible in print, but
   it is the one genuine cost, and it is what R3 exists to avoid.
2. *"It works where there is no intercept."* `polr` has none either way and its Constant row stays
   empty; a multinomial has k−1 intercepts and gets them anchored; a marginal table is answered by
   §3.6, not by an intercept.
3. *"It leaves the crude column's unit system alone."* **Wrong**, and this is the argument that made
   the first draft go the wrong way: the crude companion is fitted by the same `reg_fit()` producer on
   the same prepared frame, so it is centred too — and a slope is invariant anyway (measured
   2.4e-17). There is only ever one unit system.
4. *"It collides with `reg_shape_term()`'s frozen centre."* It does not, once the order is declared:
   `shape` recodes the column first (it is what defines the model's variable), the shift applies to
   the result, and the quadratic's centre is then measured on the same frame — so its `m` is ≈ 0 and
   nothing is centred twice.
5. *"It is right where centring would be wrong."* This one stands but is off-target: the polynomial-
   contrast divergence (1.057862 on the log-odds, §7.7) is about which **factor** level the profile
   sits at, which shifting a numeric column never claimed to address. tabxplor uses treatment
   contrasts, and the Constant row is the first level by construction.

And centring buys one thing prediction never could: **it repairs the collinearity check**. Max VIF on
the interacted fit is **11.74** raw against **1.28** centred — the false alarm the roadmap already
recorded for `shape = "quadratic"` is the same phenomenon, and one rule now fixes both.

### 3.5 What is invariant, and the two things that need the offset back

Everything that is an **estimate** is invariant under the shift, or improved by it. Measured:

| quantity                                   | max abs. difference              |
|--------------------------------------------|----------------------------------|
| fitted values                              | 5.55e-16                         |
| log-likelihood                             | 0                                |
| the slope itself                           | 2.43e-17                         |
| the interaction coefficients and their SEs | 9.54e-18 / 8.67e-19              |
| the LR test of the interaction             | bit-identical (p = 2.435877e-05) |
| the AME of the slope, and of a factor      | 0 / 2.78e-17                     |
| the crude univariable slope                | 2.43e-17                         |
| adjusted predictions                       | 3.33e-16                         |
| `multiplier = "sd"`                        | SD is invariant by construction  |

So the rule is short enough to state once: **an estimate never needs un-centring; only a descriptive
reading of the variable's own values does.** There are exactly two such readers in the package:

- `reg_spec_tips_num()` (`R/reg-spec-build.R:294-298`) — the numeric predictor's tooltip, which prints
  the variable's mean, its SD and its conditional means. On shifted data the mean reads 0.
- `reg_panel_linearity()` (`R/plots.R:499-507`) — the assumption plot's x axis, which is the binned
  predictor. The **sparkline is unaffected**: `reg_curves()` bins the same values shifted by a
  constant, so the curve's shape is identical and no axis is printed.

### 3.6 The identity that makes a re-anchor free

A change of anchor is an **exact linear reparametrisation of the fitted object**: the two design
matrices span the same column space, so `X_raw = X_centred · T` for a triangular `T`, and
`β_c = T·β_raw`, `V_c = T·V_raw·T'`. Measured: `max |T·b_raw − b_ctr| = 1.02e-14` and
`max |T·V·T' − V_ctr| = 2.22e-16`.

That is the same fact `reg_reref_fit_res()` already exploits for a **factor** reference change
(`R/tab_reg.R:2271-2290`), and it has two consequences worth designing for:

- **the jamovi live path** can treat a numeric `ref` change as a cache HIT, reparametrising instead of
  refitting, exactly as it does for a factor reference today; **but it would be worth checking if it works
  with interactions, which is doubtful.**
- **the two routes are provably the same numbers**, so R3 is an optimisation of R2 rather than a rival,
  **unless there are interactions with numeric vars ?**

⚠ The one thing R3 cannot do, and the reason it must not become the semantics: a
**profile-likelihood interval is not a linear map**. `method = "profile"` is a supported option
(`R/tab_reg.R:834`, `:881`, `:1197-1209`), so under R3 those bounds would have to be recomputed by a
refit anyway.

### 3.7 `ref` for a continuous predictor

`ref` becomes the one *reference-profile* argument, with one meaning per variable kind:

- a **factor**: the level every other level's effect is measured against (unchanged);
- a **continuous predictor**: the value it is anchored at — realised by shifting the column at the
  preparation boundary, so the fit's own coefficients are already anchored there.

Its reach is worth one sentence of documentation, because it is not the same in both cases and the difference is exactly what §3.3 is about: **the predictor's own slope never moves** (a slope is the same wherever you start from), but **the Constant row and every term the predictor interacts with do** — and in a model with no interaction the second half is just the Constant row, which is why the anchor looks cosmetic until an interaction is added.

Values worth accepting, and no more:

| value             | meaning                         | why                                                     |
|-------------------|---------------------------------|---------------------------------------------------------|
| a number          | the anchor, verbatim            | `ref = c(tvhours = 0)` — a count's meaningful zero      |
| `"mean"`          | the weighted mean (**default**) | what `at_reference` already uses; a real, central value |
| `"median"`        | the weighted median             | robust where the mean is dragged by a tail              |
| `"min"` / `"max"` | the observed extreme            | the natural anchor for a duration or an exposure        |

Rejected: quantile keywords (`"q25"`), which invite a mini-language for a value a user can type; and
`"zero"`, which is just `0`. ⚠ `"mean"` must be the **weighted** mean, closing defect A2-1.

⚠ **Ordering, once and declared.** `shape` recodes the column first — `log()` / `sqrt()` / quantile
groups define what the model's variable *is*, and a quantile shape turns it into a factor, which is
never shifted. The anchor then applies to the result, so `ref` on a `"log"`-shaped predictor anchors
the log. Any other order makes `log(x - mean(x))` undefined for half the sample.

### 3.8 What the Constant row holds

| effect         | single-equation GLM                            | multinomial              | ordinal           |
|----------------|------------------------------------------------|--------------------------|-------------------|
| `coefficient`  | the **intercept**, now anchored at the profile | one per outcome category | empty (see below) |
| `at_reference` | the same                                       | the same                 | empty             |
| `marginal`     | the population-average prediction (§3.9)       | the same                 | the same          |

**The point of R2 is that this table needs almost no code.** Under `coefficient` the quantity is the
fit's own intercept, read by the ordinary term match that `reg_column()` already performs — the `L'β`
machinery the first draft proposed is not needed at all, and neither is a special arm per family.
`polr` keeps an empty row because it has thresholds rather than an intercept, whichever
parametrisation is used; a multinomial gets its k−1 intercepts anchored for free. The p-value and its
stars are the fit's own Wald test of the intercept, so the existing `tab_constant_null()` rule
(`R/fmt_class.R:5588`) is unchanged: the null is the scale's own neutral, `0` or `1`.

Only the `marginal` arm needs a quantity that is not in the tidy, and §3.9 is what supplies it.

### 3.9 The population-average arm

Under `effect = "marginal"` a single-profile value would sit oddly beside sample-averaged effects, so
the row shows the **average predicted outcome** — the baseline the whole marginal table is read
against, and the exact analogue of a crosstab's Total row.

Measured, and worth knowing before it is implemented: for a canonical link with an intercept the
average prediction equals the observed rate *exactly* (logit: 0.4872685 both ways; weighted logit:
0.4889446 both ways, with weights). It is **not** an identity in general — a log-link binomial gives
0.4859015 against an observed 0.4872685. So the row must be computed as the model's average
prediction, never short-cut to the observed mean.

Its interval is the influence-function / delta-method standard error of the average prediction, which
is what `reg_marginal(want_pred = TRUE)` already returns.

### 3.10 The label

*"Reference population"* over-promises today, since the row describes nobody (numerics at zero). Once
the profile is declarable and defaults to the mean the label becomes nearly true — "nearly", because
a profile at the mean of `age` and the modal-by-convention first level of each factor is still a
constructed person. **Maintainer’s choice: **`Reference profile`**, which claims exactly what it is.**

Under `effect = "marginal"` the row is a different quantity and should say so : **`Population average`** (an alternative would be **`Sample average`**, but the model do really modelise the whole *population* average with inference and CIs.)
- The `var` key stays `"Constant"` in every case: it is the skeleton's key, read by `forest_plot(intercept =)`, `tab_constant_null()` and `reg_level_counts()`, and none of them should learn about labels.

### 3.11 What moves

- Every table with a continuous predictor gets a different Constant row, and every table with an
  *interacted* continuous predictor gets different lower-order rows as well (measured: up to ×2.74 on
  the odds scale). ⚠ **Correction, verified at implementation time: no golden and no snapshot moves.**
  No `_golden/*.rds` case and no `_snaps` file builds a `tab_reg()` table, so the regression side is
  golden-blind. What moves is the parity tests that include the intercept in their comparison.
- A `multinom` or `polr` golden may move in its **last digits** (4.6e-07 / 6.9e-07), because those
  fitters are restarted on a shifted design. Worth knowing before a snapshot diff is blamed on the
  logic.
- `?tab_reg`'s Constant paragraph (`R/tab_reg.R:3545-3550`) currently teaches the opposite and must
  be rewritten; `@param ref` gains the continuous half, including the sentence about what an anchor
  does and does not move; both regression vignettes gain a paragraph, because with interactions this
  stops being a detail.
- `reg_reference_grid_values()` becomes the one profile producer, reading declared references and
  weighted means; with the column already shifted, a continuous predictor's grid value is `0`.
- **The two descriptive readers of §3.5 need the offset added back, and nothing else does**.
- The jamovi cache fingerprints the prepared frame, so a numeric `ref` change is a MISS until §3.6's
  reparametrisation is wired into `reg_reref_fit_res()`; that is an optimisation, not a correctness
  requirement.

---

## 4. Part C — interactions

### 4.1 The problem, stated in tabxplor's own terms

tabxplor's row axis is *one row per level of one variable*, and every subsystem downstream reads that
shape: the crude companion is a univariable model **of that variable**, the colour ladder compares a
level **to its reference level**, the count is the level's own `n`, `{base}` is the level's own rate.
An interaction has no variable to be about, which is why the compound-formula hatch prints rows
labelled `raceBlack:age` with no count, no crude column and no unit (§7.3) — the fit is right and
everything around it is empty.

So the question is not "how do we fit an interaction" (tabxplor already can) but **"what is the variable an interaction is about?"**

### 4.2 What the framework already answers, and where it stops

`tab_vars` + `color = "between_groups"` is already an effect-modification feature: one effect per
group, the background grading how far each sits from the baseline group's, greyed when the difference
could be chance, and a per-predictor **interaction test** in the footer built from a pooled
`(x1 + x2) * g` fit (`reg_interaction_rows()`, `R/tab_reg.R:2108-2145`). It is complete and it is
taught. What it cannot do is put the interaction *inside* one model's rows: the groups are separate
fits, so a third variable cannot be adjusted for across them, and the comparison is between whole
sub-populations rather than between two predictors.

The compound-formula hatch (`outcome = y ~ a * b`) covers the rest by abandoning the framework: 48
gates across the codebase turn off `multiplier`, `shape`, `empirical`, the adjustment gap, the
linearity check, the per-predictor tests, the jamovi fast path and the declarative skeleton. 
Nearly everything interesting in tabxplor.

### 4.3 The statistical background

**An interaction is scale-dependent, and tabxplor prints several scales.** A model with no
interaction on the odds-ratio scale generally has one on the probability scale, and the reverse. This
is not a defect of any method: it is what non-collapsibility and the link function do. Measured on
`married ~ race * age + relig`: the odds-ratio interaction coefficient for Black vs White is
**+0.00447**, while the difference of the two sample-averaged probability-scale effects is
**+0.000181 with p = 0.836** — the same data, two honest answers to two different questions. This is
Ai & Norton's point (2003) that in a nonlinear model the interaction *effect* is not the interaction
*term*: the cross-partial varies observation by observation and can differ in sign from the
coefficient.

The design consequence is not to pick a winner but to **make the scale visible**, which tabxplor already
does: `measure` names the scale, the legend states it, and the same table can be printed on the
multiplicative and the additive scale (§7.4 shows both).

**And the presentation is a solved problem in the literature.** Knol & VanderWeele (2012) recommend
that an interaction table give an effect for **each `(G, E)` stratum against a single common
reference category**, plus the actual risks in the cells, and that both the multiplicative and the
additive scale be reported. That is a description of the table in §7.4 — including the `{base}`
asides, which are the "actual risks in the cells".

**A lower-order term is only interpretable relative to a stated anchor**, and this is where Part B
stops being a detail. Measured on the same fit, moving `age` from 0 to its mean moves the `race`
coefficients from OR 0.334 / 0.424 to 0.402 / **1.163** — the second row changes by ×2.74 and reverses
its reading — while the interaction coefficient is bit-identical. §3.3 is the full measurement, and it
is why Part B's anchor has to be realised **before** the fit rather than by predicting afterwards.

Two consequences specific to Part C. The collinearity check false-alarms on any interacted fit — max
VIF **11.74** raw, **1.28** anchored — so Part B's shift repairs a check that Part C would otherwise
break. And the compound-predictor design of §4.6 sidesteps the problem entirely for the crossed pair:
**a combined factor has no lower-order terms, so there is nothing left to misread** (its own max VIF
is **1.02**). The anchor still matters for every *other* continuous predictor in the model, and for
the nested-slope arm of §4.7, where the moderator's own rows are read at the anchor.

### 4.4 Three parametrisations, one fit

The decisive measurement. For a numeric predictor crossed with a factor:

| model                                              | log-likelihood | rank | max abs. fitted difference vs `race*age` |
|----------------------------------------------------|----------------|------|------------------------------------------|
| `y ~ race * age + rincome + relig`                 | -8454.563      | 16   | —                                        |
| `y ~ race / age + rincome + relig`                 | -8454.563      | 16   | 5.55e-16                                 |
| `y ~ race + age_White + age_Black + age_Other + …` | -8454.563      | 16   | 5.55e-16                                 |

and for two factors, `race * inc3` / `race / inc3` / a combined `interaction(race, inc3)` factor:
identical log-likelihood (-8468.788) and rank (14), fitted values within 5.0e-16.

**They are the same fit.** So the choice among them is a choice of what the *coefficients mean*, and
therefore of what a row says — never a statistical choice. Three readings are available:

- `X * M` — a main effect plus a difference of effects (the interaction coefficient);
- `M / X` — **the effect of `X` within each level of `M`**, directly as coefficients. Verified against
  `L'β` on the `X * M` fit: estimates agree to 1.39e-17, standard errors to 8.67e-19, p-values to
  4.03e-23. No contrast algebra is needed to obtain simple slopes;
- a **combined factor** — every cell against one common reference cell.

`M / X` and the materialised nested columns are the same thing written two ways (agreement 1.39e-17),
so there are really **two** presentations: *effects within groups*, and *cells against one reference*.

### 4.5 The missing key

> **An interaction is a predictor whose levels are combinations, and whose univariable model is its
> own saturated fit.**

Every subsystem then works because none of them has to learn anything:

- **Rows** — the compound predictor's levels. The 2-tier `var` / `level` row model is enough: `var` is
  `race × age`, `level` is `Black · 34-46`.
- **`n`** — the cell count, which a factor predictor already gets from `reg_level_counts()`. A
  continuous predictor has no count today; crossing it with a factor *gives it one*.
- **The crude companion** — the compound predictor's own univariable model. When both parents are
  categorical that model is **saturated over the cells**, so it is the observed cell table and the
  existing closed form computes it: measured agreement **1.4e-12** on estimates, **9.3e-8** on
  standard errors (§7.2). Exact by construction, not by a mirrored formula.
- **The adjustment gap** — crude and adjusted come from one fitted frame on one estimand, so
  `reg-influence.R`'s influence-function standard error applies unchanged. Measured: `gap_se`
  populates on **18 of 21** rows of a compound-predictor table under a collapsible estimand (§7.4).
- **Colour, stars, the bold reference, the legend, the header word** — unchanged. A cell of a
  compound predictor is the same measure as any other cell, so `MEASURES`, `EST_SCALES`,
  `REG_WORDS` and `REG_ESTIMANDS` are untouched.
- **And one thing becomes possible that no other design offers**: the background channel can grade
  each cell's departure from *no interaction*, reusing the `adjustment` gap's own shape — §4.15.
- **Survey designs** — an ordinary factor term in an ordinary formula; `svyglm` needs nothing.
- **Multinomial and ordinal outcomes** — likewise (verified for `nnet::multinom` and `MASS::polr`).
- **The jamovi cache** — the key fingerprints the *prepared* frame, so a new column moves it by
  construction (`jmvreg_fit_key()`, `R/jmvtabreg-cache.R:86`).

This is the property nothing else in the candidate space has: not "the integration is cheap", but
**there is no integration to do**.

### 4.6 The categorical case — a combined factor

Both parents categorical (or made so): `cross` builds one factor whose levels are the observed cells,
ordered so that the **keyed** variable varies fastest and the moderator groups.

```text
var          level              n            Obs_OR      Model_OR
race × age   White · 18-33      2 495 (35%)   1           1     (36%)
             Black · 18-33        607 (17%)   1/2.58***   1/2.80*** (17%)
             Other · 18-33        475 (32%)   1/1.14      1/1.14  (33%)
             White · 34-46      2 287 (57%)   2.48***     2.42*** (57%)
             Black · 34-46        481 (38%)   1.13        1.01    (36%)
             …
```

Read directly: *"Black people aged 18–33 have 1/2.80 the odds of being married of White people of the
same age, and 17 % against 36 % predicted"* — one comparison per row, one common reference, the
observed value beside the adjusted one. The same table on the additive scale
(`measure = "difference"`, `effect = "marginal"`) gives risk differences in points, which is the
second half of what Knol & VanderWeele ask for. Both are printed verbatim in §7.4.

Properties, all measured: saturated in the pair (k·m − 1 parameters, exactly the `X*M` count), the
Collinearity check reads **1.02** instead of 11.74, and the fit is if anything *faster* than the star
parametrisation (19.5 ms against 26.8 ms, mean of 20 fits at n ≈ 13 000).

### 4.7 The continuous case — nested slopes

A continuous predictor cannot join a combined factor without being cut. Two honest answers, and both
should exist:

- **Cut it** — `shape = c(age = "quartiles")`, which the package already documents as the most
  readable answer to a continuous predictor, and which then gives §4.6's table exactly. This is the
  route §7.4 uses.
- **Keep the slope** — `cross` emits the nested form `M / X`, so the rows are the predictor's slope
  within each level of the moderator, straight out of the fit:

```text
var          level                n        Obs_OR      Model_OR
age × race   per SD/13.5 · White  9 846    1.40***     1.31***
             per SD/13.5 · Black  1 860    1.48***     1.39***
             per SD/13.5 · Other  1 254    1.89***     1.79***
```

(A sketch of the rendering, but not of the numbers: the odds ratios are the measured nested
coefficients of `married ~ race / age + rincome + relig` and of its univariable twin, exponentiated
at SD(age) = 13.4586, and the counts are the real level counts.)

Its crude companion is the univariable nested fit `y ~ M / X` through the same `reg_fit()` producer,
so it shares estimand, link and CI rule by construction — the rule the crude column already lives by.
Its `n` is the moderator level's count, which the compound predictor makes available for the first
time. The one thing it does *not* have is a closed form (a continuous predictor never did: the
Cornfield/discriminant approximation lands at ratios 1.003 / 1.034 / 0.9998 against the fit on this
data, degrading with skew, and the crude column's exactness rule forbids shipping it).

The two arms are one declared rule keyed on `reg_is_factor_var()` — the predicate the package uses
everywhere — in the same spirit as `REG_EMPIRICAL` declaring the crude column's shape per family.

### 4.8 The designs that lose, and why

- **Simple slopes by contrast (`L'β` on `X * M`).** Numerically identical to §4.7 (1.39e-17), but it
  needs a contrast engine at every consumer, and it leaves the crude companion, the counts and the
  levels unsolved — the parametrisation is the cheap part.
- **Interaction contrasts (the raw terms).** The fewest rows, and the fit's own coefficients — but
  the reader must combine two numbers to get an effect, the reference row is gone, and a "difference
  of odds ratios" has no crude counterpart at all. It is also the presentation Ai & Norton warn is
  the easiest to misread on a nonlinear scale. Worth keeping as the *footer* test, which is what it
  already is.
- **Stratified columns from one pooled fit.** No new row machinery, and `between_groups` applies
  as-is — but the non-crossed predictors repeat identical numbers in every column block, and the
  result is nearly `tab_vars`, which already exists. The measured gain over `tab_vars` is small: the
  exact gap standard error beats the Altman–Bland approximation the group gap uses today by
  **0.3 % and 0.1 %** on this data (the two slopes correlate at only 0.008), so "exactness" is not
  the argument it looked like.
- **The compound-formula hatch.** Keeps working, and should keep working as the expert exit door for
  a specification the argument surface cannot express (custom contrasts, hand-written offsets,
  three-way terms). It should not be the answer to interactions, and §7.3 shows why.

### 4.9 The criteria matrix

`+` works today or falls out; `~` needs work; `-` is a real loss.

| criterion                          | combined factor | nested slopes   | contrast `L'β` | raw interaction terms | stratified columns |
|------------------------------------|-----------------|-----------------|----------------|-----------------------|--------------------|
| readable without arithmetic        | +               | +               | +              | -                     | +                  |
| rows fit the 2-tier row model      | +               | +               | +              | +                     | +                  |
| row count                          | k·m             | m               | m or k·m       | (k−1)(m−1)            | unchanged          |
| estimate, SE, p from the fit       | +               | +               | ~              | +                     | +                  |
| crude companion                    | + closed form   | ~ nested refit  | -              | -                     | ~ per group        |
| per-row `n`                        | +               | ~ group `n`     | -              | -                     | +                  |
| adjustment gap + its CI            | +               | ~               | -              | -                     | +                  |
| colour, stars, legend, header word | +               | +               | +              | ~                     | +                  |
| `effect = "marginal"`              | +               | ~               | ~              | -                     | +                  |
| `at_reference`                     | +               | ~               | ~              | -                     | +                  |
| omnibus interaction test           | ~ one extra fit | ~ one extra fit | + `drop1`      | + `drop1`             | + exists           |
| survey designs                     | +               | +               | +              | +                     | +                  |
| multinomial / ordinal              | +               | +               | ~              | +                     | +                  |
| `multiplier` reaches it            | n/a             | - see C4-2      | -              | -                     | +                  |
| collinearity check                 | + 1.02          | ~               | ~ 11.74        | ~ 11.74               | +                  |
| sparse cells                       | -               | +               | +              | +                     | ~                  |
| jamovi + cache                     | +               | +               | ~              | ~                     | +                  |
| new code                           | least           | small           | large          | small                 | medium             |

### 4.10 The crude companion — four routes, measured

The maintainer asked for this explicitly. All four were run on the same data.

| route                                       | what it is                           | result                                |
|---------------------------------------------|--------------------------------------|---------------------------------------|
| **saturated closed form** on the cells      | observed cell odds ratios, Woolf     | **exact**: 1.4e-12 est., 9.3e-8 SE    |
| **univariable nested fit** `y ~ M / X`      | the same `reg_fit()` producer        | both arms; the only continuous route  |
| **the materialised variable, crude engine** | no new route — it *is* a factor      | works today, unchanged (§7.4)         |
| **subgroup g-computation**                  | `avg_comparisons(by = M)`, crude fit | works; only for `effect = "marginal"` |

Two conclusions. For the categorical case the first three collapse into **one** route — materialising
the variable *is* using the closed form, because the closed form is what the crude engine already
runs on a factor predictor. And a closed form for a *continuous* crossed predictor is not available
at the exactness the crude column requires: the classical discriminant approximation is exact only
for a normally distributed predictor and drifts with skew (measured 1.003 at skew 0.24, 1.034 at skew
0.49), which is the same conclusion `dev/numeric_predictors_crude_counterparts.md` reached for the
uncrossed case.

### 4.11 What is actually missing

Everything below is what §7.4's table does **not** already give. It is a short list, which is the point.

1. **The argument** — `cross`, its resolver, its `TAB_ARGS` row, its validation and its abort
   messages (§4.12).
2. **Building the column** — one stage in `reg_prepare_data()`, beside `reg_shape_apply()`, which
   already rewrites columns at the same boundary (`R/reg-resolve.R:255-265`). It must also write into
   `design_obj$variables` for a survey design, exactly as the shape stage does.
3. **The level order and labels** — cells ordered with the keyed variable fastest; a declared
   separator; empty cells dropped and reported.
4. **The interaction test row** — one extra additive fit. Measured: `anova(additive, combined)` gives
   LRT 7.9963 on 4 df, p = 0.09171, **identical** to `drop1(star, ~ race:inc3)`. This is exactly the
   pattern `reg_interaction_rows()` already uses (a second fit whose only purpose is a footer row),
   and it should join `TEST_ROWS` beside the existing `interact_lr`.
5. **The nested-slope arm** — the `M / X` formula, its skeleton rows, its labels, its `n` and its
   `multiplier` (defect C4-2 below).
6. **Documentation** — `?tab_reg`, both regression vignettes, and one sentence in the intro vignette,
   in two languages.

### 4.12 The `cross =` argument surface

```r
cross = c(race = "age")            # race's effect is allowed to vary with age
cross = "party3"                   # every eligible predictor crossed with party3
```

- **Grammar** — Part A's, unchanged: a bare scalar names the moderator for every eligible predictor, a
  named element keys the **modified** predictor with its moderator as the value, and `default` sets
  the fallback. Keying by the modified predictor removes the symmetry problem E5 flagged: a predictor
  has at most one moderator, so a mirrored duplicate cannot be written.
- **The block's name and row order** — `race × age`; the keyed variable varies fastest inside each
  moderator level, so the rows read as "compare these, within that".
- **The reference cell** — composed from the parents' own `ref` (**P4**), so `ref = c(race = "Black")`
  moves it with no new grammar. Knol & VanderWeele's suggestion of taking the lowest-risk stratum as
  the reference is then one `ref` away rather than a policy.
- **A continuous moderator** aborts, naming the cure the package already teaches:
  `shape = c(age = "quartiles")`. Keeping the row axis discrete is what makes the whole design work.
- **A continuous modified predictor** takes the nested-slope arm (§4.7); crossing it *and*
  `shape = "quadratic"` aborts, since the squared term would sit outside the interaction.
- **Composition with a predictors list (P6)** — because `cross` materialises a real column, that
  column is addressable by name in `predictors`, so "with and without the interaction" is an ordinary
  model comparison. ⚠ **Open**: whether a model that names both parents should get the compound
  automatically, or only when it names it — §5.
- **Composition with `tab_vars`** — orthogonal and already working: §7.5 shows a compound predictor
  inside `tab_vars` groups with `between_groups` colouring and the group interaction test
  (`p = 1.94 %`), i.e. a readable three-way table.
- **Not spelled inside `predictors`.** `predictors = c(race, "race:age")` would be the tidiest
  spelling, but 22b-vi made `predictors` a tidy-select surface and `:` is tidyselect's range
  operator, so `race:age` selects the columns between them. A separate argument avoids a collision
  that no escape hatch removes cleanly.

### 4.13 Caveats and limitations, stated

- **Saturation.** k·m − 1 parameters: 8 for 3×3, 23 for 8×3, 63 for 8×8. The pair is fitted
  saturated, which is the same cost as `X * M` but is spent whether or not the interaction is needed.
- **Sparse cells.** Measured on real data: `race × relig` gives 24 cells, none empty but **6 under 30
  observations and a minimum of 4**; `race × party3` gives 9 cells with a minimum of 150. So a
  compound predictor is comfortable for small pairs and needs a warning — and empty cells must be
  dropped rather than left to produce a rank-deficient fit.
- **One reference cell is a choice, not a neutral fact.** Every effect is measured against
  `White · 18-33`; a reader who wants "the effect of age within each race" reads the nested arm
  instead. Both readings exist, and the table should not pretend one is the other.
- **The scale carries the interaction.** §4.3: the same data has an odds-ratio interaction and
  essentially no probability-scale one. Printing both scales is the honest answer, not choosing.
- **The omnibus test costs a second fit**, as the `between_groups` interaction row already does.
- **`effect = "marginal"` over a compound predictor** gives each cell's sample-averaged effect against
  the reference cell — coherent, but the marginal effect of one parent *alone* is no longer available
  from that table, because the model no longer contains it as a separate variable.

### 4.14 Defects found while measuring

- **C4-1 — a variable whose name needs backticks loses its footer test row.** `reg_global_rows()`
  filters `have %in% sp$predictors` (`R/tab_reg.R:2207`) where `have` comes from
  `attr(terms(fit), "term.labels")` and is therefore *backticked* whenever the name is not syntactic,
  while `sp$predictors` holds the bare name. Measured: `race_age` gets its
  `Overall association (LR)` row (LR 1310.85, 11 df); the identical model with the column named
  `race x age` gets **none**, silently. Pre-existing and general — it hits any variable with a space —
  and it matters here because `race × age` is the natural name for a compound predictor. The fix is
  the same backtick strip already applied to `td$term` in `reg_fit()`.
- **C4-2 — `multiplier` cannot reach a nested slope.** `reg_tidy_rescale()` matches `td$term == v`
  exactly (`R/tab_reg.R:728-741`), so `raceBlack:age` is left at one raw unit while `age` would be
  scaled — verified directly. Only bites if the nested arm lands, but it would be silent.
- **C4-3 — the collinearity check false-alarms on any interacted fit** (max VIF 11.74 against 1.28
  centred), which is already recorded in the roadmap for the quadratic case and is confirmed here for
  interactions. The compound-predictor parametrisation happens to avoid it entirely (1.02).

### 4.15 What this design makes possible that nothing else does — an interaction COLOUR

The maintainer's brief asked for the colour and significance system, not only the numbers. Once the
interaction is a predictor, tabxplor's **two-channel** language has an obvious second thing to say
about each cell, and it needs no new field:

- the **text** grades the cell's own effect against the reference cell — the ordinary measure;
- the **background** grades how far the cell departs from **no interaction at all**, greyed by
  `color_signif` when that departure could be chance.

The no-interaction expectation is the *additive* model's estimate for that cell — and that model is
already being fitted, for the omnibus test row (§4.11). So the measure is exactly the shape
`color = "adjustment"` already has: `obs` holds the value the cell is compared to, `est` the value it
shows, `gap_se` the standard error of their difference. Both are nested fits on one frame, which is
the same situation `reg-influence.R` already solves for crude-versus-adjusted — so the existing
influence-function machinery applies rather than a new derivation. **One new `MEASURES` row, no new `fmt` field, no new legend grammar.**

Demonstrated on `married ~ race × age4 + relig` (the departure is the ratio of the saturated cell
odds ratio to the additive model's, with a stacked influence-function standard error):

```text
          cell additive_OR interacted_OR departure se_log     p
 Black · 18-33       0.345         0.311     0.902 0.0833 0.210
 Other · 34-46       2.407         2.219     0.922 0.0586 0.170
 Black · 47-59       0.828         0.741     0.896 0.0634 0.082
   White · 60+       1.579         1.513     0.958 0.0197 0.031
   Black · 60+       0.545         0.621     1.138 0.0786 0.099
   Other · 60+       1.526         1.781     1.167 0.1258 0.220

omnibus interaction test (LR, additive vs saturated): 6 df, deviance 8.8628, p = 0.1814
```

The reading is immediate and is what an interaction table is *for*: one cell (`White · 60+`) departs
from additivity at p = 0.03 while the omnibus test says the pattern as a whole does not (p = 0.18) —
so the colour shows the shape and the footer keeps the honest summary, which is exactly how
tabxplor already asks `between_groups` to be read.

⚠ The standard error above is a stacked-influence-function demonstration, not the final rule: the
implementation should route through `reg-influence.R`'s existing nested-pair machinery so the
design-based case comes for free, as it does for the crude/adjusted gap.

Two more things follow at no extra cost, both of which Knol & VanderWeele ask for and neither of which
tabxplor prints today:

- the **additive-scale** departure is the same measure with `measure = "difference"` — the
  interaction-on-the-additive-scale their recommendation insists on reporting alongside the
  multiplicative one;
- a **RERI** footer row (relative excess risk due to interaction) is then one more `TEST_ROWS` entry
  computed from the same two fits, beside the omnibus test.

---

## 5. Maintainer’s answers to the open questions

1. **The compound predictor and a predictors list.** When `predictors` is a list of models and
   `cross = c(race = "age4")` is set, should a model that names both parents automatically get the
   compound column, or only a model that names the compound by name? Automatic is friendlier;
   by-name makes "with and without the interaction" an ordinary comparison and is the reason **P6**
   exists. A third option is automatic with an explicit opt-out. This also settles the open
   "per-model `shape`" question the roadmap deferred, because a materialised column has exactly the
   same property.
   **Maintainer’s decision: study the possibility to pass interactions in `predictors =` as "var1:var2". It would permit to compare model with an interaction with the model with no interaction**
2. **The `×` in the block name.** `race × age` reads best and is safe in every exporter, but it is a
   non-ASCII character in a value the package constructs. `race:age` and `race_x_age` are the
   alternatives. (The separator between cell levels — ` · ` in every capture here — is the same
   question one level down.)
   **Maintainer’s decision: even if the user pass "var1:var2", reading `race × age` in the rows names is more clear.**
3. **Whether to offer the nested-slope arm at all in the first implementation**, or ship the
   categorical arm alone and name `shape = c(age = "quartiles")` as the route for a continuous
   predictor. Shipping one arm is materially less work and loses the simple-slope reading.
   **Maintainer’s decision: offer the nested-slope arm.**
4. **Sparse cells.** Warn above what threshold, and drop or keep a cell below it? The measured
   `race × relig` case has a cell of 4.
   **Maintainer’s decision: don’t warn, the `n` column is made for the user to check.**
5. **The Constant row's label** under each contrast (§3.10), and whether `Overall` is worth a second
   label at all.
   **Maintainer’s decision: see above, `Reference profile`, and for `effect = "marginal"` `Population average`.**
6. **Whether the interaction test row is on by default** or only under `stats = "interaction"`, as
   the `tab_vars` one is under `between_groups`.
   **Maintainer’s decision: study the speed lost when adding `stats = "interaction"` before we decide.**
7. **Is the anchor's shift visible enough?** With R2 the stored predictor column is not the user's
   own any more. `reg_formulas()` would print `y ~ race * age` with `age` shifted, and the two
   descriptive readers of §3.5 get the offset back — but a user reading `reg_call()` or exporting the
   prepared frame sees shifted values. Options: say it in the row's tooltip beside `per SD/13.5`,
   record the anchor in `meta`, or leave it to the documentation.
   **Maintainer’s decision: leave it to the documentation.**
8. **Should `multinom` / `polr` avoid the shift** to keep their goldens bit-stable (4.6e-07 /
   6.9e-07), taking §3.6's reparametrisation for those two families instead? It buys exactness at the
   cost of a second route, and the drift is far below anything printed.
   **Maintainer’s decision: they do not avoid the shift, no second route.**
9. **The interaction colour of §4.15** — wanted at all, and if so in the first implementation or as a
   second step? It is the part of the design with the most upside and the least precedent: it needs
   one `MEASURES` row and the nested-pair influence function, and it is what turns an interaction
   table from "read the footer p-value" into something the eye can scan.
   **Maintainer’s decision: I’m not sure. If the interactions rows already have their ci and pvalues, what does it adds ? How is it useful in real-world use cases ?**

---

## 6. Proposed implementation phases

Two phases, in this order, because the second needs the first's reference profile.

**Phase 22b-viii — the per-predictor grammar and the anchor** (Parts A and B). One shared
per-predictor resolver and the uniform grammar on `multiplier` / `shape` / `ref`; `ref` extended to
continuous predictors; **the anchor realised as a column shift at the preparation boundary, beside
`reg_shape_apply()` and after it**; the Constant row then read from the fit's own intercept, with the
population-average arm under `marginal`; the offset added back in the two descriptive readers of
§3.5; the label; defects A2-1 and A2-2. Goldens move, including the last digits of `multinom` /
`polr` cases. `?tab_reg` and both vignettes get the new paragraph. §3.6's reparametrisation is an
optional follow-up for the jamovi fast path, not part of the semantics.

**Phase 22b-ix — interactions** (Part C). `cross` and its boundary stage; the compound column and its
levels; the categorical arm first, the nested arm second; the interaction test row; defects C4-1 and
C4-2; the jamovi option; documentation in two languages. Every subsystem it touches is listed in
§4.11, and that list is short precisely because the design was chosen to make it short.

---

## 7. Captures

All scripts under the session scratchpad, run as `OMP_NUM_THREADS=1 Rscript <file>.R` against
`devtools::load_all()`. Data: `gss_cat_data_formatting()`, complete cases.

### 7.1 One fit, three parametrisations

```text
== M1: same fit? ==
         model    logLik df      dev
1     race*age -8454.563 16 16909.13
2     race/age -8454.563 16 16909.13
3 materialised -8454.563 16 16909.13
max |fitted diff| star vs nest : 5.551115e-16
max |fitted diff| star vs mat  : 5.551115e-16

== M2: does M/X give the SIMPLE SLOPES as coefficients? ==
              group      nest_est        nest_se          nest_p        Lb_est          Lb_se
raceWhite:age White 0.01995158494 0.001574371377 8.374711012e-37 0.01995158494 0.001574371377
raceBlack:age Black 0.02458607907 0.004089415832 1.831070646e-09 0.02458607907 0.004089415832
raceOther:age Other 0.04307997970 0.005101658816 3.058511196e-17 0.04307997970 0.005101658816

max |est diff| = 1.387779e-17   max |se diff| = 8.673617e-19   max |p diff| = 4.032505e-23
materialised == nest? est: 1.387779e-17  se: 8.673617e-19
```

### 7.2 The crude companion is the closed form

```text
=== M4a: univariable NESTED fit vs the two-way CLOSED FORM (factor x factor) ===
               term          fit       closed        fit_se     closed_se
  raceWhite:inc3mid 0.1680681329 0.1680681329 0.06439450641 0.06439450642
  raceBlack:inc3mid 0.3464176391 0.3464176391 0.15060373641 0.15060382921
  raceOther:inc3mid 0.3490031975 0.3490031975 0.16555777953 0.16555778186
 raceWhite:inc3high 0.5946910255 0.5946910255 0.04955723368 0.04955723371
 raceBlack:inc3high 0.8241495476 0.8241495476 0.12244089089 0.12244093327
 raceOther:inc3high 0.8774365376 0.8774365376 0.13256403761 0.13256404061
max |est diff| = 1.395883e-12    max |se diff| = 9.280813e-08
```

and, for a continuous predictor, the approximation that disqualifies a closed form:

```text
route (b') Cornfield/discriminant closed form vs the fit:
  race       fit    closed    ratio     skew
 White 0.0250206 0.0251071 1.003456 0.244918
 Black 0.0291808 0.0301593 1.033532 0.489878
 Other 0.0474056 0.0473968 0.999814 0.692348
```

### 7.3 What the compound-formula hatch prints today

`tab_reg(d, married ~ race/age + relig, family = "binomial", empirical = TRUE)` — the fit is right and
everything around it is empty: raw coefficient names, no counts, no crude column, no unit, and the
`race` block reporting its effect at **age = 0**.

```text
ℹ `empirical` (crude descriptive companion) needs one predictor per row; a compound formula
  (`poly()` / interactions / `I()`) has none, so it is ignored here.
   var      levels                       n  Model_OR
 1 Constant Reference population    12 960   2.04***
 2 race     White                    9 846      1
 3 race     Black                    1 860   2.99***
 4 race     Other                    1 254   2.36***
…
13 race:age raceWhite:age                  1/1.02***
14 race:age raceBlack:age                  1/1.03***
15 race:age raceOther:age                  1/1.05***
| Collinearity (max VIF)       |   11.74 |
```

### 7.4 What a compound predictor prints today, with no new code

`tab_reg(d, "married", c("race x age", "relig"), family = "binomial", empirical = TRUE,`
`color = c(TRUE, "adjustment"))`, where `race x age` is `interaction(race, age4)`:

```text
   var        levels                       n          Obs_OR        Model_OR
 1 Constant   Reference population    12 960                 1/1.50***
 2 race x age White · 18-33            2 495 (35%)      1         1    (36%)
 3 race x age Black · 18-33              607 (17%) 1/2.58*** 1/2.80*** (17%)
 4 race x age Other · 18-33              475 (32%) 1/1.14    1/1.14    (33%)
 5 race x age White · 34-46            2 287 (57%)   2.48***   2.42*** (57%)
 6 race x age Black · 34-46              481 (38%)   1.13      1.01    (36%)
 7 race x age Other · 34-46              389 (58%)   2.54***   2.56*** (59%)
 8 race x age White · 47-59            2 437 (58%)   2.63***   2.49*** (58%)
 9 race x age Black · 47-59              431 (38%)   1.14    1/1.01    (36%)
10 race x age Other · 47-59              215 (60%)   2.86***   2.88*** (62%)
11 race x age White · 60+              2 627 (58%)   2.60***   2.39*** (57%)
12 race x age Black · 60+                341 (37%)   1.10    1/1.07    (35%)
13 race x age Other · 60+                175 (64%)   3.33***   3.13*** (64%)
| Collinearity (max VIF)          |    1.02 |
```

the same table on the additive scale (`measure = "difference"`, `effect = "marginal"`):

```text
   var        levels                       n            Obs_RD         Model_mRD
 2 race x age White · 18-33            2 495 (34.8%)     0%        0%    (36.1%)
 3 race x age Black · 18-33              607 (17.1%) -17.7%*** -19.2%*** (16.9%)
 4 race x age Other · 18-33              475 (32.0%)  -2.8%     -2.9%    (33.2%)
 5 race x age White · 34-46            2 287 (57.0%) +22.1%*** +21.4%*** (57.5%)
…
```

and the gap that `color = "adjustment"` grades really is computed:

```text
=== gap_se under a COLLAPSIBLE estimand (marginal RD) on a combined factor ===
obs filled: 20  gap_se filled: 18  of 21
```

### 7.5 A compound predictor inside `tab_vars`, with the group interaction test

```text
   var      levels               `Model_OR_1-Democrat` `Model_OR_2-Independent…` `Model_OR_3-Republican`
 2 race_age White · young                         1                          1                       1
 3 race_age Black · young                    1/2.32***                  1/2.52***               1/3.54***
 5 race_age White · middle                     1.99***                    2.07***                 1.95***
 6 race_age Black · middle                     1.04                     1/1.04                  1/1.96***
# Interaction with party3 (likelihood ratio): race_age p = 1.94%**.
```

### 7.6 The interaction test under each parametrisation

```text
drop1() on the STAR fit (scope = the interaction):
          Df Deviance   AIC    LRT Pr(>Chi)
race:inc3  4    17193 17217 7.9963  0.09171

LR of combined vs additive (same numbers as star vs additive):
  Resid. Df Resid. Dev Df Deviance Pr(>Chi)
1     12948      17193
2     12944      17185  4   7.9963  0.09171
```

### 7.7 Part B — the anchor, and why it must be set before the fit

The Constant row, on a model with no interaction:

```text
raw intercept        : -0.8209236  SE 0.06845486
centred intercept    :  0.2369388  SE 0.02878313
profile prediction   :  0.2369388  SE 0.02878313
as OR: raw 0.44  at the mean 1.2674  -> ratio 2.88

ordered factor: intercept -1.075033  profile prediction -0.01717  -> differ by 1.057862

observed rate                 : 0.4872685
logit  mean predicted         : 0.4872685
log-link mean predicted       : 0.4859015
identity mean predicted       : 0.4872685
weighted logit, wtd mean pred : 0.4889446   wtd observed: 0.4889446
```

**With an interaction** (`married ~ race * age + relig`, n = 12 960, mean(age) = 42.38032) the
Constant row's identity survives — `|prediction − centred intercept| = 2.775558e-17` — but the
lower-order rows are the ones that matter, and a prediction does nothing for them:

```text
 level      raw_coef       raw_se      ctr_coef        ctr_se            Lb         Lb_se
 Black -1.0957954848 0.1899601596 -0.9119008590 0.05650550814 -0.9119008590 0.05650550814
 Other -0.8587570881 0.2127670778  0.1507274276 0.06950454895  0.1507274276 0.06950454895

as odds ratios -- raw: 0.334 0.424  centred: 0.402 1.163
centred coefficient == L'b at the profile?  est: 1.582068e-15   se: 6.938894e-17
raw vs centred: the SAME ROW moves by a factor 1.202 2.744 on the odds scale
```

Everything that is an estimate is invariant under the shift:

```text
fitted values                      max |diff| = 5.551e-16
logLik                             max |diff| = 0.000e+00
the slope itself                   max |diff| = 2.429e-17
the interaction coefficients       max |diff| = 9.541e-18
their SEs                          max |diff| = 8.674e-19
LR test of the interaction  raw: 2.435877e-05  centred: 2.435877e-05
max VIF  raw: 11.74   centred: 1.28

AME of the slope        raw 0.00594167  centred 0.00594167
AME of race             max |diff| = 2.775558e-17
crude univariable slope max |diff| = 2.428613e-17
adjusted predictions    max |diff| = 3.330669e-16
```

A re-anchor is an exact linear reparametrisation, and a refit is exact for the direct fitters and
drifts in the seventh decimal for the iterative ones:

```text
max |Xc %*% T - Xr| = 2.273737e-13
max |T b_raw - b_ctr|   = 1.015854e-14
max |T V T' - V_ctr|    = 2.220446e-16

glm       slope invariance: max |diff| = 6.939e-18
multinom  slope invariance: max |diff| = 4.576e-07
polr      slope invariance: max |diff| = 6.920e-07
svyglm    slope invariance: max |diff| = 6.939e-18
```

### 7.8 Survey designs, and 3+ level outcomes

```text
=== M6: survey design under nesting ===
max |fitted diff| star vs nest = 5.551115e-16
raceWhite:age 0.02334150 0.00167669  0
raceBlack:age 0.02726966 0.00410991  0
raceOther:age 0.04650607 0.00572429  0
L'b on the star fit, Black slope: 0.02726966  se: 0.004109913
stratified design OK; Black slope se = 0.004089

=== M7: 3+ level outcomes under nesting ===
multinom OK. terms: raceWhite:age | raceBlack:age | raceOther:age   (broom::tidy works: TRUE)
polr OK. slope terms: raceWhite:age | raceBlack:age | raceOther:age
```

### 7.9 Cost and sparsity

```text
saturated interaction parameters, k x m (k, m = 2..8):
     [,1] [,2] [,3] [,4] [,5] [,6] [,7]
[1,]    3    5    7    9   11   13   15
[2,]    5    8   11   14   17   20   23
…
[7,]   15   23   31   39   47   55   63

fit time (ms, mean of 20): additive 24  star 26.8  combined 19.5  univ.combined(crude) 22.3

race x relig  : 24 cells, 0 empty, 6 under 30, min = 4
race x party3 :  9 cells, 0 empty, 0 under 30, min = 150
```

### 7.10 The two measured defects

```text
underscore name, global rows:
       var      col      test statistic df1        pvalue     n outcome
1 race_age Model_OR global_lr 1310.8532  11 2.043270e-274 21307 married
2    relig Model_OR global_lr  218.0568   7  1.705371e-43 21307 married

spaced name, global rows:
    var      col      test statistic df1       pvalue     n outcome
1 relig Model_OR global_lr  218.0568   7 1.705371e-43 21307 married

`multiplier = c(age = 10)` applied to a tidy holding a nested term:
           term estimate std.error
1           age       10        10
2 raceBlack:age        1         1
```

---

## 8. References

- Ai, C. & Norton, E. C. (2003). Interaction terms in logit and probit models. *Economics Letters*
  80(1), 123–129.
- Norton, E. C., Wang, H. & Ai, C. (2004). Computing interaction effects and standard errors in logit
  and probit models. *The Stata Journal* 4(2), 154–167.
- Knol, M. J. & VanderWeele, T. J. (2012). Recommendations for presenting analyses of effect
  modification and interaction. *International Journal of Epidemiology* 41(2), 514–520.
- VanderWeele, T. J. & Knol, M. J. (2014). A tutorial on interaction. *Epidemiologic Methods* 3(1),
  33–72.
- Mize, T. D. (2019). Best practices for estimating, interpreting, and presenting nonlinear
  interaction effects. *Sociological Science* 6, 81–117.
- Schielzeth, H. (2010). Simple means to improve the interpretability of regression coefficients.
  *Methods in Ecology and Evolution* 1(2), 103–113.
- Echambadi, R. & Hess, J. D. (2007). Mean-centering does not alleviate collinearity problems.
  *Marketing Science* 26(3), 438–445.
