# tabxplor — Phase 20: the surface — how the package is asked, and what it exposes

**The plan of plans for the last development stretch before the 2.0.0 CRAN release** — goals, design
and architecture decisions first, then the phased roadmap.

Written 2026-08-15 from the two assessments that precede it (`dev/tabxplor_phase19_assessment.md`
= Phase 19o, and `dev/tabxplor_phase19p_api_review.md` = Phase 19p), plus the maintainer's rulings
taken while this plan was written. **Those two documents are the evidence; this one is the plan.**
Where they disagree with this document, this one wins — it carries decisions they left open, and
five corrections to their measurements (§7.2).

**Companion documents** — read the one that matches what you touch:

| document                                        | what it holds                                                                                  |
|-------------------------------------------------|------------------------------------------------------------------------------------------------|
| `dev/tabxplor_phase19_assessment.md`            | 19o: what Phase 19 achieved and cost; keys α–η; the white-elephant ledger; the perf root-cause |
| `dev/tabxplor_phase19p_api_review.md`           | 19p: the review of the *ask* — fields, exports, arguments, options; keys A–D                   |
| `dev/tabxplor_phase19_ecosystem_integration.md` | Phase 19's plan of plans — the hard rules this phase inherits                                  |
| `dev/ecosystem_keys_2.md`                       | the Phase 19 study: measurements, eight keys, defect ledger                                    |
| `dev/tabxplor_ecosystem_simplification.md`      | round 1 (Phase 17) — the disease patterns                                                      |
| `dev/tabxplor_architecture.md`                  | the **current** architecture — update it as phases land                                        |
| `dev/release_checklist.md`                      | the release mechanics (the release phase, after Phase 22)                                      |
| `dev/tabxplor_roadmap_DONE_PHASES.md`     | the already-implemented phases' "DONE" summaries, including all of Phase 19                    |

⚠ Every `file:line` in 19o, 19p and here is an anchor from **2026-08-15**. `tab.R`, `tab_reg.R`,
`fmt_class.R` drift by ±20 lines per phase. **Re-grep before editing.**

---

## 1. Why Phase 20 exists — the mission

Phase 19 delivered a complete, explicit data model: a cell knows what it holds, a column knows what
it estimates, a row knows what it is, a table knows what kind it is, and ~15 declared fact tables
hold the vocabulary. It closed ~30 defects and made several classes of them *unrepresentable*.

It also grew `R/` by **11.9 %** and never touched the two things that make the package feel large to
a user. 19o stated the finding in one line:

> **Phase 19 unified how facts are STORED and how rules are DECLARED. It did not unify how the
> package is ASKED.**

19p sharpened it into the sentence that governs every phase below:

> **Every remaining duplication in tabxplor's public surface is the same shape: a fact is declared
> once in an R table, and re-typed by hand in the place a user meets it** — in a formal, in a
> `@param` block, in an option name, in an accessor.

The package has already solved that problem **four times** — `fmt_fields_rd()`,
`display_tokens_rd()` ×2, `reg_measures_rd()`, each built because the hand-written copy had drifted.
**Phase 20 applies the same solution to the surface.** Concretely, after it:

- an **argument** is declared once — its producers, its legal values, its option twin, its one-line
  gloss — and the signature, the reference page and the jamovi vocabulary all read that declaration;
- a **producer variant** stops re-declaring the entry point's arguments;
- the **two producers ask the same question with the same word**, everywhere they ask the same
  question;
- an **exported name** exists because a user story reads it, not because a fact became stored;
- a **fact table's cross-references are checked at load**, not remembered in a comment;
- `tab_reg()` gets a **staged build** and stops paying for a marginal-effect variance it already
  computes analytically.

**Phase 20 is not a feature phase either**, with exactly two exceptions, both deliberate: the jamovi
level-collapsing UI (20g) and — only if the measurement justifies it — `tab_reg()` parallelisation
(20f).

### The hard rules (inherited from Phase 19 §1, all still binding)

1. **Simplify and integrate — never add another ad hoc layer.** Extend the shared model or the fact
   table; never bolt a special case onto a call site. Delete the traces of the old implementation in
   the same phase — no commented-out corpses, no "kept just in case" branch.
2. **Never guess what something is.** No behaviour may depend on matching a rendered English label,
   a name prefix, a positional vector, or a magic value. If the fact is not stored, **storing it is
   the task**.
3. **One resolver, one model, taken to completion.** Re-deriving downstream is the disease.
4. **Facts live in ONE table.** Two encodings "kept in sync by comment" is forbidden. **Phase 20
   extends this to documentation**: a value list written in roxygen beside the table that declares
   it is the same offence one level up.
5. **Never leave a representation half-migrated.** Split the *session*, never the migration.
6. **Internals and outputs are redesigned as radically as needed.** `tab_reg()`'s back-compat is
   waived entirely, user API included. `tab()`'s CRAN-released surface gets deprecation shims,
   never silent breakage — and "released" means **CRAN 1.3.1**, not the dev head (§7.2).
7. **A claimed fix ships with the fixture that fails without it.**
8. **Golden discipline** — each phase declares which goldens may move and proves the delta with
   `dev/verify_golden_field_delta.R`.
9. **End-of-phase documentation discipline** (CLAUDE.md § "The last step of every implementation").
   The phase **"DONE" summary goes in CLAUDE.md and ONLY there**.

### What must survive, unchanged in spirit

The five differentiators (Phase 19 §1): per-cell metadata → lossless display switching · colour that
reads significance · crude-vs-model comparison · the jamovi teaching path · dplyr citizenship.

**Differentiator 4 is the one at risk in this phase.** The jamovi UI shows R argument names *on
purpose*, so a user learns the R API by clicking. Every rename in 20b/20c therefore has to reach
`jamovi/*.a.yaml` in 20g, or the teaching path starts lying. That coupling is why 20g is late and
why `test-jamovi-vocabulary.R` is a hard gate rather than a convention.

---

## 2. The measured starting state

Measured on `5bba715` (Phase 19p). Everything here is from 19o §2 / 19p §0, plus this session's
re-measurements.

| fact                                                | value                                                                                             |
|-----------------------------------------------------|---------------------------------------------------------------------------------------------------|
| R source                                            | **44 278** lines · 21 749 code · 19 263 comment (43.5 %)                                          |
| top-level functions                                 | **1 066**, median 17 lines                                                                        |
| the two biggest                                     | `reg_build` **534 deparsed lines** (7 local closures) · `plain_core` 482                          |
| user messages                                       | **197** — `cli_abort` 149 · `cli_warn` 11 · `cli_inform` 37                                       |
| … at an argument boundary                           | **121 of 197 (61 %)**, across 7 files                                                             |
| `tab()` formals                                     | **52** — 9 deprecated, 4 internal dot-args, 39 live                                               |
| `tab_counts` / `tab_plain` / `tab_num` / `tab_reg`  | 40 · 29 · 28 · 29                                                                                 |
| mirrored formals                                    | **83 of the 149 crosstab formals** are the same argument written a 2nd–4th time                   |
| `man/`                                              | **8 930** lines · `tab_reg.Rd` 722 · `tab.Rd` 695 · `fmt.Rd` 693 · `tab_many.Rd` 448              |
| exports                                             | **93** — released baseline **CRAN 1.3.1 = 63**, so **35 new, 5 removed**                          |
| exports in no vignette and no README                | **52 of 93**                                                                                      |
| global options                                      | **35** documented · 34 seeded · 1 documented-but-never-seeded                                     |
| declared fact tables                                | ~15, with **7** build-time `stopifnot` blocks, of which **2** are cross-table                     |
| unchecked cross-table foreign keys                  | **≥12** (all currently intact; one has already broken in a shipped commit)                        |
| `R/tab-steps-legacy.R`                              | **1 433 lines**, **zero real callers in `R/`** (44+15 textual hits are all comment/roxygen prose) |
| `effect = "marginal"` on 21 483 rows × 4 predictors | **15.32 s**, of which **85 % is `marginaleffects::get_jacobian`**                                 |

**The two most diagnostic numbers**: 83 of 149 crosstab formals are duplicates of an argument that
is already declared, and 61 % of everything the package says to a user is still said while
negotiating arguments. The first is what Phase 20 deletes; the second is what it makes declarable.

⚠ **The metric warning, carried from 19o §9 and restated because it will be tempting to forget**:
Phase 19 grew 11.9 % and got substantially better. **Do not count lines as the simplification
metric.** Phase 20 will also grow `R/` — `TAB_ARGS`, `TEST_ROWS`, `outcome_level`, three generators
and two harnesses all cost lines. The metrics that track reality here are:

> *formals per producer · duplicated `@param` blocks · `man/` lines · exports with zero external
> callers · cross-table keys unchecked · aborts vs informs.*

`man/` is the one surface that shrinks unambiguously: **8 930 → ~7 300 (−18 %)**, estimated per file
and summed, never rounded up.

---

## 3. The nine keys, in one page

19o and 19p each lettered their own keys (α–η and A–D). **Two lettering schemes for one body of
work is the disease this phase exists to cure**, so they are re-stated here as one numbered set.
The `source` column keeps the traceability.

| key       | the missing fact / unstated rule                                                                         | what it stores or states                                                                                           | source          | lands in            |
|-----------|----------------------------------------------------------------------------------------------------------|--------------------------------------------------------------------------------------------------------------------|-----------------|---------------------|
| **KEY 1** | *which producer takes which argument, what it means, what it may be, and which option is its default*    | `TAB_ARGS` — the argument surface as data; the signature, the `@param` block and the value list all derive from it | 19o α · 19p C+D | **20b**             |
| **KEY 2** | *a key written in one declared table and read in another is a foreign key*                               | ~14 cross-table checks at load time, ~30 lines                                                                     | 19o β           | **20a**             |
| **KEY 3** | *which accessors exist* — the exported get/set family is the last hand-written mirror of `fmt_col_attrs` | one generic `fmt_attr()` pair + a measured keep-list of named accessors + `tab_columns()`                          | 19p A           | **20a**             |
| **KEY 4** | *if two producers ask the same question, they ask it with the same word*                                 | `tab_vars`, `ref`, `ci_method`, `footer` on both                                                                   | 19o ε · 19p §5  | **20c**             |
| **KEY 5** | *what kind of statistical row this is, what it is about, and how it renders*                             | `TEST_ROWS` — the crosstab half of the footer subsystem finally declared                                           | 19o δ           | **20c**             |
| **KEY 6** | *which stage of a regression build produced which part of the table*                                     | `new_reg_ctx()` + five named stages, mirroring `tab_build()`                                                       | 19o γ           | **20e**             |
| **KEY 7** | *which estimands tabxplor can differentiate analytically*                                                | a declared `se = analytic \| numeric` column; the AME stops being computed twice                                   | 19o ζ           | **20d**             |
| **KEY 8** | *the export surface re-declares seven arguments five times*                                              | `TAB_ARGS` covers the exporters too — **not** a `tab_style()` bundle (§4 ★)                                        | 19p §4.7        | **20b**             |
| **KEY 9** | *a package whose whole value is a data model states that model in one place*                             | `?tabxplor-model` + one reader naming convention                                                                   | 19o η           | **Phase 22b** (§10) |

**KEY 1 is the keystone of this phase**, the way KEY 5 was of Phase 19. Everything else is either a
prerequisite for it (KEY 2, KEY 3), a second instance of it in another subsystem (KEY 4, KEY 5) —
or, in KEY 8's case, **the same instance**: the export surface's duplication turned out to be
KEY 1's, one subsystem further out, and it is answered by extending the same table rather than by a
new concept. Only KEY 6 and KEY 7 are independent of it.

---

## 4. Settled decisions — do not re-open

All rulings in force. 19o §11's eight (marked ○), 19p's eleven (marked ◆), and the eight taken while
this plan was written (marked ★).

| decision                                                        | ruling                                                                                                                                                                                                                                                |
|-----------------------------------------------------------------|-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| ○ `tab()`'s 9 deprecated formals                                | **move into `...`**, caught by name, with an **abort on an unnamed 6th argument**                                                                                                                                                                     |
| ○ the legacy step API                                           | **hard-deprecate now**, defunct in 2.1.0. The *computations* moved into the leaf in 19j; what is deprecated is the exported *chaining API*                                                                                                            |
| ○ `tab_reg(split_var =)`                                        | **→ `tab_vars`**, with `split_var` a permanent silent alias                                                                                                                                                                                           |
| ○ `tab(color =)`'s default                                      | the `"no"` vs `TRUE` asymmetry with `tab_reg()` is **deliberate and NOT documented**                                                                                                                                                                  |
| ○ KEY 7's tolerance                                             | a change in the last printed decimal of a standard error is **acceptable**                                                                                                                                                                            |
| ○ setters stay exported                                         | …reconciled with ★ below: the *keep-list* includes `set_row_kind()`                                                                                                                                                                                   |
| ○ the five `tab_kable_*` option renames                         | **stay dropped** (the 19m-iii ruling stands) — do not re-propose                                                                                                                                                                                      |
| ○ comment archaeology                                           | **its own phase, numbered 22** — out of Phase 20                                                                                                                                                                                                      |
| ◆ `tab_logit()` / `multi_logit()`                               | **deleted** (unreleased — verified against CRAN 1.3.1, §7.2)                                                                                                                                                                                          |
| ◆★ `tab_reg(reference =)`                                       | **→ `ref`**, `c(var = "level")`, **predictors only** (plus `tab_vars`). It **cannot** absorb `inverse_two_level_factors`: a predictor is a row variable and the outcome is a column variable, and the two ask *opposite* questions — for a predictor the user picks the level compared **against**, for an outcome the level **modelled**. ⚠ Correction to 19p, measured: `reference` **already** keys on the outcome today, but only for multinomial (`relevelable <- union(union(all_predictors, split_var), deps$dep[families == "multinomial"])`, `reg-resolve.R:613-614`). That half moves out too — see the row below |
| ★ **`outcome_level`** (new)                                     | **The outcome's own level argument**, `c(outcome = "level")` keyed by outcome name so several outcomes work. **binomial** → the level **modelled** (the probability the model estimates; it becomes the column header), replacing `inverse_two_level_factors`, which is **deleted**. **multinomial** → the level singled out as the baseline every category column is compared to, taking over what `reference` does today. **ordinal** → **refused**, with its reason (an ordinal outcome must keep its order). ⚠ §7.1 item 12 states the one non-uniformity and how to declare it. Precedent: SAS `PROC LOGISTIC` has exactly this pair, `EVENT=` beside `REF=`; `yardstick` has `event_level` |
| ★ `tab_reg(dependent =)` → **`outcome`**                        | **package-wide rename**, argument and internals (`dep` / `n_dep` / `deps$dep` / `reg_per_dep()` / the `test` tibble's declared `dep` column / the jamovi `dep*` options). It is what makes `outcome` + `outcome_level` a visible pair in the signature. ⚠ see §7.1 item 11 — one rendered string and one declared schema column are in the blast radius |
| ◆ `tab(ref / ref2)`                                             | **unchanged** — two arguments, different per-axis defaults                                                                                                                                                                                            |
| ◆ `na`'s two vocabularies                                       | **keep both** — they describe different operations — but generate both value lists from their declaring table                                                                                                                                         |
| ◆ `tabxplor.stars`                                              | absorbs `signif_levels` + `signif_labels`, **and becomes a per-call ladder**                                                                                                                                                                          |
| ◆★ `options(tabxplor.total_names = c(row=, col=, tab=, other=))` | **new** — three hard-coded label defaults in five signatures, in two languages (`"Total"` / `"Ensemble"` / `"Others"`), with no option twin; **and the related arguments leave the main public-facing functions**. ⚠ Measured: `total_names`, `totaltab_name` **and** `other_level` are **all three CRAN 1.3.1 formals of `tab()`** (`86320287:R/tab.R:305,309,310`) and documented `@param`s — so this is a *released* removal and takes the same route as the 9 deprecated formals: soft-deprecate → caught by name in `...` → defunct in 2.1.0. Not a silent drop |
| ◆ `tabxplor.color_style_type`                                   | **deleted** (documented, never seeded, read only to emit its own warning)                                                                                                                                                                             |
| ◆ `@inheritDotParams`                                           | **never** — it inlines. `tab_many.Rd` is the 448-line proof                                                                                                                                                                                           |
| ◆ `...`                                                         | on **wrappers and superseded producers only**. `tab()` and `tab_reg()` keep every live formal                                                                                                                                                         |
| ◆ teach, do not cut                                             | `tab_shape()` / `tab_supports()` / `reg_measures()` (+ the new `tab_columns()` / `fmt_attr()`) get an *"Inspect a table"* vignette section                                                                                                            |
| ★ **the inference bundle**                                      | **NO BUNDLE.** 19p §4.2's `tab_inference()` is **rejected**. `ci_method` / `design_effect` / `anova` stay flat formals with their option twins; the only change is `tab_reg(method =)` **→ `ci_method`** with a declared `model` slot in `CI_METHODS` |
| ★ `tab_style()` (the exporters' bundle)                         | **REJECTED** (2026-08-16, after re-explanation). It fails the same test `tab_inference()` failed, and for a sharper reason: `tab_export(format =)` **already** unifies the backends and the `tabxplor.*` options **already** unify per-document defaults, so a style object is a *third* mechanism for one thing. `tab_md(t, style = tab_style(var_names = "none"))` is longer than `tab_md(t, var_names = "none")`. **The measured problem was never the signatures — it was 35 hand-written `@param` blocks for 7 concepts**, which is KEY 1, so `TAB_ARGS` covers the exporters in **20b** instead: same Rd saving, no new concept, no deprecation shim on five released functions |
| ★ `TEST_ROWS`                                                   | **inside Phase 20**, pre-release — 20c, together with its first consumer `footer =`                                                                                                                                                                   |
| ★ `tab_reg()` parallelisation                                   | **inside Phase 20** — its own phase 20f, gated on the re-measurement after 20d and 20e                                                                                                                                                                   |
| ★ the jamovi level-collapse UI                                  | **inside Phase 20, pre-release**, both modules, generated once — 20g                                                                                                                                                                                  |
| ★ `tab_many()`                                                  | **stays soft-deprecated** — it was the documented main function through 1.3.1 and real scripts call it. Only its `.Rd` is fixed                                                                                                                       |
| ★ `new_lvl()` / `is_lvl()`                                      | **exported** since the user see the new custom vector class                                                                                                                                                                                                               |
| ★ `tab_prepare()`                                               | **off the public surface** — `@keywords internal`, un-exported. ⚠ It **is** CRAN 1.3.1 (`86320287:NAMESPACE`) with 9 test callers, so rule 6 applies: soft-deprecate + `@keywords internal` in 2.0.0, un-export in 2.1.0 — the `complete_partial_totals()` route. A silent un-export of a released name is the one thing this plan's own rules forbid |
| ★ the accessor family                                           | **generic mostly, a few named ones kept — the most used** (`get_col_var()` was named as the example). The keep-list is decided from measured usage at plan time and **must include `set_row_kind()`** (○)                                             |
| ★ `pct`'s `"no"` default                                        | **unchanged** — say so explicitly in `?tab` so it stops reading like an oversight                                                                                                                                                                     |

⚠ **Two rulings reverse a proposal outright — and they are the same ruling.** There is no
`tab_inference()` and no `tab_style()`. 19p proposed both as tier-2 bundles; both were rejected,
three weeks apart in the same reading, and the shared reason is the **general test for every future
bundle**:

> **A bundle must make the common call shorter, not only the signature.**

`inference = tab_inference(ci_method = c(diff = "wald"))` is more typing than
`ci_method = c(diff = "wald")`, on an argument with 19 corpus uses. `tab_md(t, style =
tab_style(var_names = "none"))` is more typing than `tab_md(t, var_names = "none")` — and worse,
`tab_export(format =)` **already** unifies the backends while the `tabxplor.*` options **already**
unify per-document defaults, so the object would have been a *third* mechanism for one thing, which
hard rule 1 forbids outright.

**The lesson generalises past bundles**, and it is the correction this plan makes to 19p: *a
mirrored formal is not automatically a problem*. Seven defaulted arguments repeated across five
exporters cost a **user** nothing — nobody is forced to type them. What was actually measured is
**35 hand-written `@param` blocks for 7 concepts**, which is a *documentation* duplication, and
KEY 1 already answers it. **Before proposing to collapse a signature, check whether the duplication
is in the signature or only in its documentation.**

---

## 5. The target architecture — the image after Phase 20

### 5.1 One declaration per argument

```
                                    TAB_ARGS  (one row per crosstab argument)
                                        |
        +---------------------+---------+---------+---------------------+
        |                     |                   |                     |
   the signature        the @param block     the value list        the option twin
   (which producer      (#' @eval            (#' @eval             (one declared
    declares it,         tab_args_rd())       <table>_rd())         `option` column,
    which takes `...`)                                              not 12 hand-written
                                                                     formal defaults)
                                        |
                                 tab_check_dots()
                          (a typo in `...` gets a "did you mean")
```

The same shape already exists for the *values*: `MEASURES`, `CI_METHODS`, `COLOR_SCALES`,
`REG_ESTIMANDS`, `REG_CHECKS`, `DISPLAY_TOKENS`, `TAB_ARG_VALUES` each declare a vocabulary that
roxygen currently re-types by hand for ~15 arguments. After KEY 1 those are `@eval` generators, and
the biggest single one — **`color_measures_rd(producer =)`** — replaces 69 argument lines in `?tab`
and **101** in `?tab_reg`, where the six measures are prose-written zero, one or two extra times
depending on the page.

### 5.2 One word per question, both producers

Phase 19 §5.2 unified the **geometry** words end to end. Phase 20 finishes the job for the other
six questions:

```
   question                    tab()                tab_reg()            after Phase 20
   --------                    -----                ---------            --------------
   which sub-populations       tab_vars             split_var            tab_vars   (alias kept)
   which ROW baseline          ref                  reference            ref   c(var = "level")
   which OUTCOME level         ref2                 inverse_two_level_   outcome_level
                               (the OR numerator)   factors (a logical)        c(outcome = "level")
   how is the interval         ci_method (4 slots)  method               ci_method (5th slot: model)
   what rides the footer       test (an omnibus     stats + compare      test  |  footer
                               test — a different   + baseline
                               question, stays)
   default colour              "no"                 TRUE                 unchanged (deliberate)
   missing data                keep/drop/…          drop_by_outcome/…    unchanged, both generated
```

**The row/outcome split is the point.** `ref` and `outcome_level` are *not* two spellings of one
question, and that is why they stay two arguments:

> **`ref` names the level you compare AGAINST. `outcome_level` names the level you MODEL.**

On a predictor the user picks the baseline; on an outcome the user picks the thing whose probability
is estimated — the level that becomes the column header. Those are opposite roles, so one argument
would have carried two meanings, which is the `type` / `ci_type` disease 19b cured.

⚠ The naming is not invented: **SAS `PROC LOGISTIC` has exactly this pair** — `EVENT=` ("PROC
LOGISTIC models the probability of the event category") beside a separate `REF=`; tidymodels'
`yardstick` has `event_level`. tabxplor spells the first `outcome_level` rather than `event` because
`dependent` is renamed to **`outcome`** in the same phase, so the argument and its level read as one
pair (`outcome = "married"`, `outcome_level = c(married = "Married")`) and a beginner never meets a
second word for the same variable.

⚠ **`tab(ref2 =)` asks a near-identical question** (which column level is the odds ratio's
numerator) under a name that says nothing. It is **ruled unchanged** for 2.0.0 — recorded here only
so a later cycle can see that the convergence exists and was declined on purpose, not missed.

### 5.3 One exposed model

```
  fmt_col_attrs (16)  ->  fmt_attr(x, name) / `fmt_attr<-`     the programmatic surface
                      ->  ~8 named accessors, measured          the taught surface
                      ->  tab_columns(x)                        the inspection surface
  tab_shape(x) / tab_supports(x, op)                            what have I got, what can I do
  reg_measures(data, dep)                                       what can this outcome be modelled as
                      ->  ?tabxplor-model                       one page, generated from the tables
```

### 5.4 `tab_reg()` reaches parity with `tab()`

```
  tab_build(ctx)                              reg_build(ctx)                    after 20e
  ------------------------------------        ---------------------------------------------
  new_ctx()  71 declared keys                 new_reg_ctx()  declared keys
  tab_setup / prepare_pop / aggregate /       reg_stage_split / _fit / _columns /
  build_tables / transform / assemble         _empirical / _footer / reg_finalize
                                                        ^
                                              the ONE place a fit happens = the parallel seam
```

---

## 6. Anti-propositions — what NOT to do

All of Phase 19's, plus 19o's and 19p's:

- **Do not route regression columns through the aggregate core** · do not go sparse on the record ·
  do not merge fmt fields · do not replace the S3-per-verb model · do not force `pillar_shaft`
  through the render model · do not re-open the settled perf verdicts (scan fusion, chi2
  marshalling, the `.fine` seam) · do not add a fifth label-block shape.
- **Do not delete `tab_ci()` / `tab_chi2()` as computations.** They are superseded wrappers whose
  arithmetic is shared since 19j. What Phase 20 hard-deprecates is the exported *chaining API* — a
  different object, and the distinction must be stated in the deprecation message itself.
- **Do not count lines as the simplification metric** (§2).
- **Do not add a fact table without a foreign-key check** (KEY 2 exists because the last two tables
  added each broke a cross-table key).
- **Do not export an accessor because a fact became stored.** Ask "what user story reads this?"
  before `@export`.
- **Do not use `@inheritDotParams`.**
- **Do not introduce a bundle whose fields are also flat formals**, and do not introduce one that
  makes the common call longer (§4).
- **Do not move the jamovi JS rules into R** — generate them.
- **Do not hand-edit a generated `.h.R`.** The last time that was done the compiler found a latent
  bug in the mirror.
- **Do not "improve" a statistic while passing through.** KEY 7's claim is *identical maths,
  cheaper route*, and it must be demonstrated, not assumed.
- **NEW — do not propose a removal without checking the released baseline.** It is **CRAN 1.3.1**
  (commit `86320287`, 63 exports), not `v1.2.0` and not the dev head (§7.2).

---

## 7. Caveats, risks, and the honest gaps

### 7.1 Named risks

1. **`tab()`'s 9 deprecated formals leaving the signature is the single highest-risk item in Phase
   20.** It breaks positional calls past argument 5 (`sup_cols` is currently 6th). Ruled: do it,
   *with* the unnamed-6th-argument abort. Three sub-hazards, all verified in source and all silent
   if missed: `names_prefix` / `names_sort` are badged deprecated but **still live** (forwarded to
   the spread path at three sites) and must be forwarded from `...` or moved onto `tab_spread()`
   — Phase 19's own open question #4, which 19h left unsettled; `method_cell` / `method_diff` are
   read with `missing()`, which does not work through `...`, so they need `NULL` defaults **first**;
   and five of the nine already have translating shims whose behaviour must not change.
2. **Hard-deprecating the step API removes nothing in this cycle.** `R/tab-steps-legacy.R`'s 1 433
   lines stay until defunct in 2.1.0. Do not expect the deletion reward in Phase 20 — expect one
   lifecycle warning per function, a `NEWS.md` entry, and `test-steps-legacy.R` becoming a
   deprecation test.
3. **KEY 7 changes printed standard errors in the last decimals.** Ruled acceptable. Reg tables are
   value-asserted rather than snapshotted, so most tests will not move — but the phase must declare
   it and prove it with a tolerance-explicit fixture, and the `se` column must be **conservative**:
   default `"numeric"`, opt a row in only with a test that pins it against `marginaleffects`.
4. **20f may legitimately end with "do not parallelise".** If 20d takes the measured call
   from 15.3 s to ~2 s, a process pool may cost more than it saves on the common one-model call.
   The phase must be allowed to reach that verdict and record it, exactly as Phase 9c did for scan
   fusion.
5. **The jamovi UI (20g) is the one item only the maintainer can finish.** Everything in `R/`,
   `jamovi/*.a.yaml`, `*.u.yaml` and `jamovi/js/` can be written and gated here; the generated
   `.h.R`, the rebuild and the live pass cannot. **Any phase that edits a `.a.yaml` / `.u.yaml`
   leaves it inert until the next `jmvtools::prepare()`** — say so in the DONE summary rather than
   claiming the UI changed.
6. ⚠ **The `.h.R` regeneration owed since 19k is still outstanding.** Until it runs, `measure`,
   `shapes` and the renamed `test` read `NULL` in the running module. 20g is where it lands, and it
   is a prerequisite for the release.
7. **The jamovi vocabulary is coupled to every rename.** `test-jamovi-vocabulary.R` asserts that
   each List option's value set EQUALS the R vocabulary it names. Every rename in 20b/20c turns it
   red until `dev/generate_jamovi_js.R` and the `.a.yaml` follow. That is the gate working, not a
   failure — but it means 20g cannot be skipped.
8. **There is still no JS syntax gate** and there cannot be one on this box (no `node`, no `V8`).
   Declined in 19n; recorded so it is not re-proposed as work.
9. **`pct`'s default stays `"no"`** — so the most-used argument in the corpus keeps a default users
   rarely want. Ruled; the mitigation is one explicit sentence in `?tab`.
10. **Two live `FIXME`s remain in the colour engine** (`R/fmt_class.R:6508` *"is the AND right?"*,
    `:6521` *"suspect."*). They are the only open `FIXME`s in `R/` (the other two are
    retrospective). Resolve or state them in 20a — an unanswered question in the engine's own
    comments is the archaeology problem in its acute form.
11. **`dependent` → `outcome` is a wide rename with two sharp edges**, and 20c must find both before
    it starts. (i) The `test` tibble's **`dep` column is DECLARED** in `new_test_tibble()`, and
    `test_group_cols()` reads every *undeclared* column as a grouping variable — so renaming it to
    `outcome` without moving the declaration turns the outcome into a phantom grouping variable, the
    defect class 19g and 19m-ii each hit once. (ii) `tx_strip_dep_suffix()` strips a literal
    `" [dep]"` from rendered labels — **check whether that suffix is user-visible before renaming
    it**; if it is, the goldens move and the move must be declared. Everything else (`n_dep`,
    `deps$dep`, `reg_per_dep()`, `reg_measures(data, dependent)`, the jamovi `dep*` options) is
    mechanical. `tab_reg()` is unreleased, so the *user-facing* half is free.
12. **`outcome_level` has one non-uniformity, and it is forced by arithmetic, not by taste.**
    With **two** levels, singling one out *is* choosing what is modelled — the other becomes the
    baseline automatically. With **k > 2** you can only choose the pivot, so on a multinomial
    outcome `outcome_level` names the **baseline** category (what `reference` does today), which is
    the opposite role. Do not paper over it with prose in two `@param` blocks — **declare it**: a
    column on `REG_FAMILIES` (`outcome_level_means = "modelled" | "baseline" | NA`) whose `NA` row
    *is* the ordinal refusal, read by the resolver, the abort message and the generated
    documentation alike. That is this phase's own method applied to its own new argument, and it is
    the difference between a stated rule and the `type` / `ci_type` disease.

### 7.2 Corrections to 19o and 19p

Recorded so the ledger stops carrying them. **Items 6-10 were measured in Phase 20a** and correct
this document as well as its two sources.

1. ⚠ **The released baseline is CRAN 1.3.1, not `v1.2.0`.** The only git tag is `v1.2.0` (59
   exports), which is two releases old; `master` carries `1.3.1.9000`, a dev version. The CRAN
   snapshot is commit **`86320287`**, `Version: 1.3.1`, **63 exports**. Against it, HEAD has **35
   new exports and 5 removed** (`%>%`, `get/set_type`, `get/set_ci_type`) — not 19p's "40 added, 6
   removed" against `v1.2.0`, nor "24 new in the 2.0.0 line". **Every delete-vs-deprecate call must
   be checked against `86320287`.**
2. ⚠ **`kable_tabxplor_style()` IS CRAN-released** (it is in 1.3.1's NAMESPACE). 19p §3.2 filed it
   under *"Delete — unreleased, free"*. It is already a defunct stub (19l) and already out of the
   pkgdown index (19n), which is the correct treatment for a released function. **No deletion, no
   further action.** The row is closed.
3. **Deleting `tab_logit()` / `multi_logit()` is not free of migration work.** They are genuinely
   unreleased (absent from 1.3.1) so the deletion is right — but 19p's *"Nothing else references
   them"* is wrong: **59 call sites across 9 test files** (`test-tab_logit.R` 25,
   `test-tab_reg-survey.R` 17, `test-tab_reg-display.R` 5, …). The cost is 59 mechanical migrations
   to `tab_reg(family = "binomial")`, not one `NEWS.md` line. *(Vignette uses: genuinely 0.)*
4. **The step functions' "zero callers in `R/`" needs its comment filter stated.** A naive grep
   reports 44 hits for `tab_ci` and 15 for `tab_chi2` in `R/`; **every one is roxygen or comment
   prose**. The claim is correct, the raw count looks like it refutes it. Any re-measurement must
   filter comment lines or it will re-open a closed question.
5. **19o's option census was over-counted; 19p already corrected it and this plan uses 19p's
   numbers**: 35 documented, 34 seeded, **exactly one** documented-but-never-seeded
   (`tabxplor.color_style_type`). And `tabxplor.totcol_range` is **neither seeded nor read** — both
   lines are commented out — so it needs no action at all, contrary to 19o §6.
6. ⚠ **`tab_md_css()` is NOT released.** It is in neither `v1.2.0` nor CRAN 1.3.1, so 19p §3.4's
   "soft-deprecate, released in v1.2.0" was wrong. **Deleted in 20a**, together with the argument
   that made it necessary (`tab_css(chrome =)` → `tab_css(format = c("html", "md"))`).
7. ⚠ **`basis` is already surfaced** — `tab_weight_line()` switches on all four values and prints a
   sentence. 19p §2.4's "the legend names neither the df nor the basis" is half right: the item was
   **`degf` alone**, and 20a added it, gated on a design basis so a flat table is byte-identical.
8. ⚠ **KEY 2's block cannot live at `reg-estimand.R`'s tail** (19o §5 KEY β). `COLOR_SCALES` is in
   `tab_classes.R` and `REG_EMPIRICAL` in `tab_reg.R`, both of which sort *after* it in C collation,
   so it sees neither. 20a put it in a new last-sorting file, `R/zzz-fact-keys.R`. There are also
   **34** cross-table edges, not 12, and **9** build-time `stopifnot` blocks, not 7.
9. ⚠ **The `@keywords internal` sweep of the accessor family has no target.** The ~23 accessors are
   `@describeIn fmt`, i.e. **one** Rd page already, so they occupy no reference-index line to demote.
   The measurable duplication was elsewhere: **53 S3-method Rd stubs**, of which 20a removed 36
   (`@noRd` beside `@export` keeps the `S3method()` registration).
10. ⚠ **"one `withr::local_options(lifecycle_verbosity = "quiet")` per test file" does not work**, so
    §7.1 item 2's cost estimate for the step hard-deprecation was optimistic: testthat 3e runs
    `local_reproducible_output()` inside every `test_that()`, which forces `"warning"` again. The
    file-level line covers top-level calls only. **Measured after 20a: 57 deprecation warnings in the
    suite** — migrating those calls to `tab()` is the corpus sweep, and it belongs to **20h**.

### 7.3 One stale defect note — CLOSED in 20g-i, it does not reproduce

CLAUDE.md's draft 20d recorded: *"On the jamovi cache path a table built with `ci = "cell"` and MIXED
col_vars renders its numeric column with the `pct_ci` display token where plain `tab()` renders
`mean_ci`."* That is the shape of **D11**, which 19k reports as closed (`jmv_apply_display()` was
deleted for `tab_apply_display()`, and since 19j the leaf stamps that display itself).

**Reproduced in 20g-i, and it does not happen**: `tab()` and `jmvtab_build()` both give
`pct_ci pct_ci pct_ci pct_ci mean_ci` on `marital × c(race, tvhours)`. The note is deleted, and the
measurement is now a fixture (`test-jmvtab-cache.R`, "D11: ci = 'cell' with mixed col_vars stamps the
same displays as tab()") so the question cannot come back as prose.

---

## 8. Verification discipline

Deliberately light, on Phase 19's model. These are floors, not ceilings.

- **Per phase the default is targeted**: the test files your change touches (`filter =`) plus the
  sentinels the phase entry names. **Do not run the full suite after every edit.**
- **Full suite** (CLAUDE.md § Testing recipe — `OMP_NUM_THREADS=1`, `TESTTHAT_CPUS=8`, a temp
  runner outside `tests/`) at **three checkpoints: end of 20b, end of 20e, end of 20i.**
- **`devtools::check()` once, at the end of 20i** — not as a release gate (that is the release
  phase's job, after Phase 22) but so **Phase 22 does not inherit a broken tree**. 19n found three
  `check()`-only failures invisible to the suite; three minutes here is cheap insurance.
- **The CI-locale run** (`LC_ALL=C.UTF-8 LANGUAGE=en`) belongs to the release phase, not here.
- **Goldens**: each phase entry names which families may move. Prove the delta with
  `dev/verify_golden_field_delta.R`, teaching it one new mode per new kind of delta.
- **Byte-identical phases** (20a, most of 20b, KEY 6) tolerate **zero** golden churn.

### The harnesses

Four exist; **two are new in 20a and are the gate for 20b and 20c**.

| harness                           | covers                                                                                                                                        | status                        |
|-----------------------------------|-----------------------------------------------------------------------------------------------------------------------------------------------|-------------------------------|
| `dev/verify_golden_field_delta.R` | 1 788 cells × 36 goldens: every field, column attribute, `test` column, `meta` sub-field                                                      | exists                        |
| `dev/verify_color_attrs.R`        | 293 cases: every stored colour attribute + both resolved slot vectors                                                                         | exists                        |
| `dev/verify_reg_specs.R`          | 291 cases: the messages **in order**, the specs, `reg_call()`, column attributes, labels, test keys                                           | exists                        |
| `dev/verify_no_ghost_functions.R` | comments naming functions that no longer exist                                                                                                | exists (a report, not a gate) |
| `dev/verify_tab_args.R`           | 167 resolver cases + 52 built tables' stored attributes + **the messages, in order**, of 30 invalid or legacy calls                            | **built in 20a**              |
| `dev/census_exports.R`            | one row per export: released_in (v1.2.0 / CRAN 1.3.1 / dev) · callers in R/ tests/ vignettes/ README/ dev/ · Rd lines · pkgdown section        | **built in 20a**              |

⚠ **Two measurement traps, both of which produced a wrong census while 19p was written**: run every
`sort` / `uniq` / `comm` census under **`LC_ALL=C`** (the box is `fr_FR.UTF-8`, whose collation does
not group identifiers containing `_` / `.`), and **never use `grep -w` on a pattern ending in `(`**
— it reported nine live exports as having zero callers. Use `(^|[^a-zA-Z0-9._])name\(`. Both are
encoded in `dev/census_exports.R` now, so the census cannot be re-derived wrongly.

⚠ **A third trap, and it is the opposite one: NEVER run the test suite under `LC_ALL=C`.** That rule
belongs to the censuses alone. `C` is a **non-UTF-8** native encoding, harsher than any CI runner,
and it produced four `_snaps/render-html.md` failures in 20a that reproduced on a pristine worktree
of HEAD and read exactly like pre-existing drift. The same tree is **FAIL 0** in the normal locale.
The CI-locale run uses **`C.UTF-8`** (CLAUDE.md § Testing).

---

## 9. The roadmap

**Nine phases.** Each is *plan-then-implement*, starting in plan mode, in its own fresh session.
The maintainer commits between phases and pushes at the end of Phase 20.

The order below is the recommended one; what is **binding** is the dependency list.

| phase   | title                                                                                | must land after       | because                                                               |
|---------|--------------------------------------------------------------------------------------|-----------------------|-----------------------------------------------------------------------|
| **20a** | The floor: referential integrity, the exposed surface, the dead weight               | —                     | KEY 2 protects every later table edit; the two harnesses gate 20b/20c |
| **20b** | KEY 1 + KEY 8 — the argument surface as data, producers and exporters alike          | 20a                   | needs `verify_tab_args.R`                                             |
| **20c** | KEY 4 + KEY 5 — one word per question, and the footer's model                        | 20a                   | independent of 20b; do it after so `TAB_ARGS`' idiom exists           |
| **20d** | KEY 7 — marginal effects, computed once and computed fast                            | 20c                   | the surface must settle before the internals move; ⚠ its second half is **research**, web searches included |
| **20e** | KEY 6 — `reg_build()` becomes a staged build                                         | 20d                   | 20d changes what the marginal path computes, and restructuring around a moving target is how a "pure refactor" stops being provably pure |
| **20f** | `tab_reg()` parallelisation: measure, then decide                                    | 20d, 20e              | ⚠ **re-measure first**; a pool attaches to `reg_stage_fit()`, which 20e creates, and "do not parallelise" is a legitimate outcome |
| **20f-ii** | the same question at the MODEL level: measure the three axes                       | 20f-i                 | ✅ measured; G and R are dispatchable today but only clear ≥2× on an even axis, and the S axis — where the 2×+ shapes are — cannot be dispatched as written |
| **20f-iii** | the S axis: `reg_spec_build()`, and the parallelism it unlocks                   | 20f-ii, 20e           | ✅ done — the per-model half is ONE declared product and `parallel` is a shared argument over all three model axes; the message-order price it budgeted for was measured at **zero** |
| **20f-iiii** | the reg framework: finished, and CLEAN under parallelisation                   | 20f-iii               | ⚠ the three serial-only shapes are the last thing about `tab_reg()` a user must *remember*; each has a measured route out, and "keep it, with the number" is a complete answer |
| **20g** | jamovi: the level-collapse UI, the boundary, the rebuild                             | 20b, 20c, 20d         | it carries every new vocabulary into the UI, and 20d is what un-freezes the marginal option |
| **20h** | Harvest 1: the deletion pass                                                         | everything structural | it measures what the finished surface made unnecessary                |
| **20i** | Harvest 2: open integration                                                          | 20h                   | ⚠ creative, and deliberately its OWN session: a deletion pass and a design pass want different frames of mind (the 19l / 19m precedent) |

⚠ **There is deliberately NO documentation phase in Phase 20.** CLAUDE.md already carries
**Phase 22 — documentation integration and simplification** (22a–22g), which owns the architecture
document, the vignettes, the roxygen sweep, the comment rewrite, `NEWS.md`, the tests and `dev/`.
A Phase 20 doc phase would be a second pass over the same files — the duplication this plan exists
to delete. §10 maps every documentation item to its Phase 22 home instead. What each Phase 20 phase
still owes is the standing rule 9 discipline: **update the docstrings and `@param`s you changed, in
the phase that changed them** — and after KEY 1 most `@param` blocks are *generated*, so a rename
documents itself.

**Mapping from the CLAUDE.md draft** (so nothing is lost): old **20d** (jamovi UI) → new **20g** ·
old **20e** (the marginal-effects freeze) → new **20d**, where it is root-caused as KEY 7 · old
**20f** (parallelisation) → new **20f**, unchanged in content but now *gated on* 20d and 20e rather
than opening with them. Phase 22 and the release phase are unchanged.

⚠ **The three `tab_reg()` phases are deliberately separate sessions.** They are one story but three
different frames of mind: 20d is a **numerical-parity** phase (research, closed forms, tolerance
fixtures), 20e is a **pure structural refactor** whose whole proof is `verify_reg_specs.R` printing
IDENTICAL, and 20f is a **measurement** that may conclude "no". Interleaving them is how a refactor
and a numeric change land in one diff and neither can be verified.

---

#### Phase 20a — The floor: referential integrity, the exposed surface, and the dead weight

**Goal**: a clean floor and two harnesses. Make a dangling cross-table key a *load* failure, stop
the accessor family growing with the attribute table, delete everything with zero readers, and
build the two characterisation harnesses the argument phases need.

**Read first**: 19o §5 KEY β + §6 (the white-elephant ledger) · 19p §2.3 (KEY A), §3 (the export
census), §7 items 1–2 · §4 and §7.2 of this document (the rulings and the corrections).

**Contents**

- **KEY 2 — the foreign keys.** ~14 cross-table references (19o §5 KEY β lists them), one check
  block, at the tail of the last file where every table is in scope — the precedent
  `R/reg-estimand.R` already sets. A dangling key must be a **load** failure, not a runtime one:
  the two checks that exist were both added *reactively*, after the key had already dangled in a
  shipped commit. ⚠ Read the tables with `[["…"]]`, never `$` — `MEASURES$adjustment` has
  `scale_from` and no `scale`, so `$scale` partial-matches to `"gap"`. State that rule where the
  tables live. Include the reverse check (a `COLOR_SCALES` row nothing references is dead weight).
- **KEY 3 — the accessor surface.** ★ *Generic mostly, a few named ones kept.* So: one documented
  generic pair `fmt_attr(x, name)` / `` `fmt_attr<-` ``, validated against `fmt_col_attrs`,
  dispatching on an fmt column *or* a data.frame; a **measured** keep-list of named accessors
  (decide it at plan time from corpus + test + vignette usage — it must include `get_col_var()`,
  named in the ruling, and `set_row_kind()`, named in 19o §11 q6); the rest become internal. Plus:
  the three missing inference getters (`get_conf_level` / `get_degf` / `get_basis` — one of four
  "how was this column's interval computed" attributes has a getter and three do not);
  **`set_diff_type` → `set_ref_type`** with the old name a soft-deprecated alias (it is in 1.3.1);
  and a new **`tab_columns(x)`** — one row per fmt column × its attributes, the column-axis mirror
  of `tab_shape()`, which is the inspection user story ~12 individual getters are used for today.
  **State the admission test in the header**: *storing a fact is internal; exporting its accessor
  is a user contract — name the user story first.*
- **The last stored Phase 19 fact that is surfaced nowhere**: the legend names the confidence level
  and the method but **not `degf` and not `basis`**, although `degf` is exactly what makes a
  design-based interval differ from a flat one and `basis` is what the whole z16 subsystem exists to
  be honest about. One sentence, one reader (`legend_method_name()` / `tab_footer_streams()`).
- **The deletions and demotions** (⚠ check each against CRAN 1.3.1 — §7.2 corrections 1–2):
  - **delete** `tab_logit()` + `multi_logit()` (unreleased; 523 Rd lines; **59 test call sites to
    migrate** to `tab_reg(family = "binomial")`; they are also a *capability hole* — a user who
    found `tab_logit()` cannot reach `effect = "marginal"`, `measure = "ratio"`, `compare`,
    `baseline`, `reference` or `color`);
  - **`new_lvl()` / `is_lvl()` STAY EXPORTED** (★, reversing 19p §8 q2) — a user meets the
    `tabxplor_lvl` class the moment they look at an index column, so its constructor and predicate
    are part of the model's public face, like `fmt()`. They keep `@keywords internal` only if the
    reference index is genuinely better without them; **prefer teaching them** in 22b's
    *"Inspect a table"* section beside `tab_columns()`;
  - **un-export** `complete_partial_totals()` (released → soft-deprecate the export, keep it
    internal; its one caller is tabxplor's own) and **`tab_prepare()`** (★) — ⚠ **both are CRAN
    1.3.1**, so both take the deprecate-now / un-export-in-2.1.0 route, never a silent drop
    (§4). While there, move `tab_prepare()` out of the pkgdown *"Superseded entry points and
    steps"* section, which reads as a verdict it has not been given;
  - **`@keywords internal`**: `tab_get_wrapped_dimensions()`, and
    `tabxplor.jmv_full_hash` → an internal constant;
  - **soft-deprecate** `fct_recode_helper()` (0 callers in `R/`, a forcats convenience that is not
    tabxplor's job) and `tab_md_css()` (≡ `tab_css(chrome = FALSE)`);
  - **corpses**: `auto_or` (`R/tab-resolve.R:116`, pinned to `FALSE`) **and** the now-unreachable
    `"or_table"` context it feeds (`R/fmt_class.R:4461`) — rule 1, delete both together; and
    `tabxplor.color_style_type` (documented, never seeded, read only to warn — deleting it makes
    `?tabxplor-options`' own "keep in sync with `.onLoad()`" promise true).
- **`tab_many.Rd`: `@inheritDotParams` → a plain `@param ... Passed to [tab()].`** — a one-line,
  zero-risk, **−390 Rd line** change, and the proof of the anti-proposition.
- **The stale claim in the vignettes**: both reg vignettes still say *"`tab_reg()` has no `display`
  argument"* (`vignettes/tabxplor-reg.Rmd:241`, `vignettes/articles/tabxplor-reg-fr.Rmd:248`) — and
  **contradict themselves 430 lines later** by documenting `display = "ci"`. 19e gave it one; 19n
  fixed the identical claim in `?tab` and missed the vignettes. It is in the vignette that teaches
  differentiator #3.
- **Small documentation truths**: `FMT_FIELD_DOC$var` states the **rule** (*"the column's variance
  quantity — which one is given by its `scale`"*) instead of enumerating the three cases, which is
  the shape of a comment that drifts; `color_breaks` demoted in `?tab` to one line plus a link
  (0 corpus uses, but it is the only route to the per-table `meta` slot, so it is **kept**);
  `?fmt`'s `ctr` / `obs` double-gloss removed (19n's own open item).
- **The two live `FIXME`s** in the colour engine (§7.1 item 10): answer them or convert each into a
  stated design note. Do not leave a question mark in the engine's comments.
- **The two harnesses** (§8): `dev/verify_tab_args.R` and the export-usage census. Build them here,
  prove they are deterministic against the unchanged tree (`check` must print IDENTICAL before they
  are trusted), and commit them.

**Verification**: targeted, plus a fixture per deletion that proves the replacement works. **Zero
golden churn** — everything here is byte-identical except the accessor renames. Sentinels:
`test-fmt_class.R`, `test-tab_logit.R` (which becomes a `tab_reg()` test file or disappears),
`test-degraded-attrs.R`, `test-color-config.R`.

---

#### Phase 20b — KEY 1 + KEY 8: the argument surface as data, producers and exporters alike

**Goal**: an argument is declared once. The signature, the reference page, the value list and the
option twin all read that declaration — and 83 mirrored formals stop existing.

**Read first**: 19o §5 KEY α · 19p §4.3 (KEY C), §4.4 (KEY D), §4.5, §4.7, §4.8 · §7.1 item 1 and
§4's bundle ruling here.

**This phase has a declared internal seam.** Part 1 is byte-identical to behaviour and all its
churn is in `man/`; part 2 is the user-visible signature surgery. A session that runs short stops
at the seam; the next resumes there. **Do not interleave them** — the seam is what makes part 2's
diff reviewable.

**Contents — part 1: the declaration (no behaviour change)**

- **Grow `TAB_ARG_VALUES` into `TAB_ARGS`**: one row per crosstab argument, gaining `producers` ·
  `group` · `default` · `option` · `status` (`live` / `deprecated` / `internal`) · `doc` beside the
  existing `values` / `leaf` / `size` / `na_ok`.
- **Generate the `@param` blocks** — `#' @eval tab_args_rd(producer = "…")`, the fourth use of the
  `reg_measures_rd()` pattern. `tab.Rd` groups by `group`. ⚠ Keep `doc` to **one sentence** per
  argument and let long prose stay in `@details` / `@section`, or you have re-invented roxygen
  inside a list.
- ⚠ **Routed here by 20a**: `TAB_ARG_VALUES$pct` spells the "no percentage" value `"no"` while
  `PCT_BASES` (the stored attribute's vocabulary) spells it `"none"` — one concept, two words, one of
  which a user types and the other of which a column carries. `TAB_ARGS` is where that is stated.
- **Generate the ~15 argument value lists** (19p §4.3 tabulates them): `color`, `color_signif`,
  `ci_method`, `pct`, `na`, `levels`, `tot`, `totaltab`, `comp`, `totcol`, `measure`, `effect`,
  `family`, `stats`/`check`, `shape`, the `color_breaks` scale names, `theme`. **The biggest single
  one is `color_measures_rd(producer = c("tab", "reg"))`** — `?tab` spends 69 argument lines on
  colour and `?tab_reg` **101**, and the second is not a copy but the two reg-only measures
  described from scratch, which is worse. Every measure's `word` / `subject` / `caveat` /
  `channels` / `requires` / `auto_for` already exist in `MEASURES`; after this, each is described
  exactly once, in the file that declares it.
- **`option` becomes a column**, so `conf_level = conf_level_default()` and its ~11 siblings
  (`stars`, `cleannames`, `ci_method`, `design_effect`, `anova`, `theme`, `lang`, `var_names`, …)
  stop being hand-written formal defaults in three different idioms (a `*_default()` call in the
  formal, a `NULL` + `getOption()` in the body, a `%||%`). **`?tabxplor-options` is generated from
  the same table.**
- **KEY 8 — `TAB_ARGS` covers the EXPORTERS too**, which is the whole of what was going to be
  `tab_style()` (§4 ★). Measured: `theme` · `color` · `color_legend` · `lang` · `caption` ·
  `transpose` · `var_names` are declared on all five of `tab_html` / `tab_md` / `tab_xl` /
  `tab_plot` / `tab_export` = **35 formal slots and 35 `@param` blocks for 7 concepts**, plus
  `wrap_rows` / `wrap_cols` / `whitespace_only` on three, `css` on two, and nine Excel-typography
  formals on `tab_xl` alone. **The signatures do not change** — every one of those arguments is
  defaulted, so a user is never forced to type them, and five CRAN-released functions keep their
  API exactly. Only the *declaration* is unified: one `TAB_ARGS` row per export argument, an
  `@eval tab_args_rd(producer = "tab_xl")` per exporter, and `resolve_export_opts()` gains the
  `option` column as its declared source. ⚠ **Do not smuggle the declined option renames in**
  (`kable_popover` → `tooltips`, `legend_style` → `color_legend`, the five `tab_kable_*`, the three
  `xl_font_*`): they are ruled dropped (○), and a documentation phase is not the place to re-open
  them.

**Contents — part 2: the signatures (user-visible)**

- **`...` on the three superseded producers**, forwarded to the shared resolver
  (`tab_resolve_common_args()`, which 19i already made the single resolution point):
  `tab_counts(data, counts, cols, col_name, base, input, wt_counts, ...)` ·
  `tab_plain(data, row_var, col_var, tab_vars, wt, num, df, ...)` ·
  `tab_num(data, row_var, col_vars, tab_vars, wt, num, df, ...)`.
  ⚠ **Mandatory: `tab_check_dots()`**, matching every name against `TAB_ARGS` and aborting with a
  "did you mean". That validator is what makes this a net gain rather than a loss — today a typo
  produces R's bare *"unused argument"*; afterwards it produces a suggestion.
- **`tab()` 52 → ~37 named formals + `...`**: the **9 deprecated** formals and the **4 internal
  dot-args** (`.cache`, `.defer_level_merge`, `.return_armed`, `.levels_order`) move into `...`, and
  the three total-label formals leave with them (below). ⚠ **The three cautions of §7.1 item 1 are
  not optional**: the unnamed-6th-argument abort; `names_prefix` / `names_sort` forwarded or moved
  to `tab_spread()`; `method_cell` / `method_diff` converted from `missing()` to `NULL` defaults
  **first**.
- **The two option changes** (◆): **`tabxplor.stars` absorbs `signif_levels` + `signif_labels`
  and becomes a per-call ladder** (`FALSE` / `TRUE` / `c("*" = 0.10, "**" = 0.05, "***" = 0.01)`) —
  today the ladder is option-only, so one table in a document cannot use a different ladder from
  the next although `stars =` is already a per-call argument on four producers; and
  **`options(tabxplor.total_names = c(row =, col =, tab =, other =))`**, which takes over from
  `total_names` / `totaltab_name` / `other_level` as formals. The three label defaults are
  hard-coded literals in five signatures, **and not even in one language**
  (`"Total"` / `"Ensemble"` / `"Others"`), with no option twin at all — for a French-authored
  package with a French audience that is a real gap, and it is why two of the three are formals
  nobody sets (3 and 2 corpus uses).
  ⚠ **All three are CRAN 1.3.1 formals of `tab()` with documented `@param`s**
  (`86320287:R/tab.R:305,309,310`), so removing them is a *released* change and takes the same
  route as the 9 deprecated formals: caught by name in `...`, soft-deprecated with a message that
  names the option, defunct in 2.1.0. ⚠ And check `tab_many()` before starting — it carried all
  three too, so its shim must translate them rather than drop them.
- **One asymmetry closed while there**: `var_names` is both an option and a per-call argument on
  five exporters; `var_labels` is option-only, although they are the same kind of display decision
  about the same names.
- **`pct`'s `"no"` default is stated, not changed** (★) — one explicit sentence in `?tab`.

**Depends on**: 20a (`dev/verify_tab_args.R`). **Unblocks**: 20g (the jamovi vocabulary).

**Verification**: part 1 must be **byte-identical** — `document()` idempotent,
`tools::checkDocFiles()` silent, zero golden churn, zero `_snaps/` churn; the only diff is `man/`.
Part 2 is gated by `dev/verify_tab_args.R` printing IDENTICAL except for the declared delta. **Full
suite at the end of the phase.** ⚠ `test-jamovi-vocabulary.R` goes red on any renamed value and
stays red until 20g — expected, and it must be *stated* in the DONE summary, not silenced.

**Estimated effect**: `tab.Rd` 695 → ~490 · `tab_plain.Rd` 279 → ~80 · `tab_num.Rd` 208 → ~70 ·
`tab_counts.Rd` 137 → ~110 · `?tab_reg`'s colour block −90 · **83 mirrored formals → ~10**.

---

#### Phase 20c — KEY 4 + KEY 5: one word per question, and the footer's model

**Goal**: the two producers stop asking the same question with different words, and the last
subsystem with no model gets one.

**Read first**: 19o §5 KEY ε and KEY δ · 19p §4.6, §5, §2.7 · `R/reg-resolve.R`'s header (19m-ii's
six stages and its 23 `H1`..`H23` ordering constraints — **the reg boundary's order is its design**,
and every rename here lands inside it).

**Contents**

- **KEY 4 — the renames** (`tab_reg()` is unreleased, so these are renames, not deprecations):
  - **`split_var` → `tab_vars`** (○), permanent silent alias. Since 19f `tab_reg()` already
    *stamps* `split_var` as a `tab_var` role on the index column — the storage was unified two
    phases ago and only the argument was not.
  - **`reference` → `ref`** (◆), taking the same `c(var = "level")` grammar `tab(ref =)` already
    accepts — **predictors and `tab_vars` only**. It does **not** absorb the outcome: see the next
    two items. `tab(ref / ref2)` stay two arguments (◆).
  - **`dependent` → `outcome`, package-wide** (★). The argument, and the internals that shadow it:
    `deps$dep`, `n_dep`, `reg_per_dep()`, `reg_measures(data, dependent)`, the `[dep]` label strip,
    the jamovi `dep*` options — and ⚠ **the `test` tibble's DECLARED `dep` column**, which cannot
    simply be renamed in place (§7.1 item 11: an undeclared column is read as a grouping variable).
    The rename is what makes the next item read as a pair in the signature.
  - **`inverse_two_level_factors` → `outcome_level`** (★) — a **new argument**, not a rename of
    `ref`, because the two ask opposite questions: *`ref` names the level you compare against;
    `outcome_level` names the level you model.* `outcome_level = c(married = "Married")`, keyed by
    outcome name so a vector of outcomes works, exactly like `ref`'s grammar.
    - **binomial** → the modelled level: the probability the model estimates, and the string that
      becomes the column header (`reg_shared_col_var()` already builds `"<outcome>: <level>"`).
      Replaces a 25-character logical with 0 corpus uses that encoded the same choice *by reversing
      factor level order* — naming the level is checkable, is what the user knows, and reads in a
      sentence. ⚠ It is already per-dependent under the hood (`reg_per_dep()`), so the named-vector
      grammar is not new machinery.
    - **multinomial** → the level singled out as the baseline. ⚠ This **takes over from
      `reference`**, which keys on the outcome for multinomial *today*
      (`reg-resolve.R:613-614`) — so this is a move, not an addition, and `ref`'s `relevelable` set
      shrinks to predictors + `tab_vars`.
    - **ordinal** → **refused**, with its reason (an ordinal outcome must keep its order; the
      refusal already exists as a comment at `reg-resolve.R:606-607` and becomes a real message).
    - ⚠ **§7.1 item 12 is the design constraint**: "modelled" for two levels and "baseline" for
      k > 2 is an inversion forced by arithmetic. **Declare it** as a `REG_FAMILIES` column whose
      `NA` row *is* the ordinal refusal — resolver, abort message and generated documentation all
      read the one declaration. Do not write it twice in prose.
    - ⚠ Also check the **0/1 numeric outcome** path: `inverse_two_level_factors` is currently a
      silent **no-op** there (`tab_reg.R:746-749` builds the labels before the reverse branch).
      `outcome_level` must either work or refuse, but not silently do nothing.
  - **`method` → `ci_method`** with a declared **`model`** slot in `CI_METHODS` (★, replacing the
    rejected bundle). One argument, one grammar, one declared default, both producers.
  - **`.fit_cache` → `...`** (it is tier-3 internal and currently a documented formal with 4 Rd
    lines).
- **KEY 5 — `TEST_ROWS`, with `footer =` as its first consumer** (★, and 19p §8 q5's "cheaper
  together than apart" is the reason they are one phase):
  - **`stats` + `compare` + `baseline` → one `footer =`** — three arguments for one concept, *what
    rides the model-summary footer*.
  - **`TEST_ROWS`**: one row per test kind — `producer` (`tab`/`reg`) · `scope`
    (`table`/`model`/`term`/`cell`) · `label` (a gettext closure) · `kind`
    (`pvalue`/`gof`/`effect`) · `digits` · `stat_glyph` · `render` (`grid`/`line`) · `family`.
    The `test` attribute is a **15-column tibble carrying ≥20 kinds of row** under one `test`
    discriminator, and only the reg half has a declaring table (`reg_footer_spec()` becomes the
    `producer == "reg"` slice). The crosstab half is **string literals in its consumers**:
    `test_display_rows()` filters on a hard-coded list and `tab_kind()`'s degraded fallback sniffs
    the same literals from a different file. That is the last surviving instance of "a subsystem's
    vocabulary lives in its consumers".
  - ⚠ **Two things `TEST_ROWS` must NOT change.** (i) `pvalue_exact` on the chi2 row is *good
    design* — Fisher's exact stored **on** the chi2 row keeps the tidy shape and the row count
    stable; declare it as a **column of that row**, never a row of its own. (ii) The dynamically
    added grouping columns are read as `setdiff(names(tt), names(new_test_tibble()))`, so the schema
    **must** stay declared in `new_test_tibble()` — 19g and 19m-ii each fixed a defect caused by an
    undeclared column being read as a grouping variable.
  - The Welch/classic pair being *both stored* and picked at display is **exactly the right
    pattern** (differentiator 1 applied to tests). Declaring it is what tells the next person they
    may add a third F without a code change.
- **`tab_reg()`'s remaining `@param` prose trimmed against `reg_measures_rd()`**, which already
  generates the estimand section and currently duplicates part of it (`family` + `effect` +
  `measure` = 101 Rd lines beside a generated section stating the same table).

**Depends on**: 20a. **Unblocks**: 20d/20e (the surface must settle before the internals move), 20g.

**Verification**: **`dev/verify_reg_specs.R` must print IDENTICAL** except for the declared rename
delta — it dumps the messages in order as well as the specs, which is exactly what a boundary
rename can silently reorder. ⚠ Its 291 cases are written in the **old** vocabulary, so migrate the
harness and re-`save` a baseline *before* the rename, then `check` after; a harness rewritten in the
same commit as the change it gates proves nothing. Golden delta for the `test` tibble (`TEST_ROWS`
must not move a value, and the `dep` → `outcome` column rename must be the *only* schema delta).
Sentinels: `test-tab_reg*.R`, `test-test-display.R`, `test-reg-checks.R`, plus
`test-jamovi-vocabulary.R` — expected red until 20g (§7.1 item 7).

**Estimated effect**: `tab_reg()` 29 → ~26 named formals + `...` (`stats`+`compare`+`baseline` →
`footer` −2 · `.fit_cache` → `...` −1 · `inverse_two_level_factors` → `outcome_level` ±0 ·
`reference` → `ref`, `method` → `ci_method`, `dependent` → `outcome`, `split_var` → `tab_vars` all
renames, ±0) · `tab_reg.Rd` 722 → ~550 · **six cross-producer name collisions → zero**.

---

#### Phase 20d — KEY 7: marginal effects, computed once and computed fast

**Goal**: `effect = "marginal"` stops being the option that freezes the module. The measured target
is **15.3 s → ~2 s** from the SE alone; the phase's *research question* is whether it can go
substantially below that.

**Read first**: 19o §5 KEY ζ (the `Rprof`, the 7× measurement, the analytic IF) ·
`R/reg-influence.R`'s header in full · `rd_link_y()` and `rd_wquantile()` in `R/reg-assumptions.R`
(the g-computation primitives the package already owns) · `reg_marginal()` / `reg_marginal_column()`
in `R/tab_reg.R`.

**The measurement, and what it says.** `effect = "marginal"` takes **15.32 s** on 21 483 rows × 4
predictors against **1.06 s** for coefficients, and `Rprof` puts **85 % in
`marginaleffects::get_jacobian`** — a *numerical* derivative, one pass per coefficient. Against
`marginaleffects` directly, `avg_comparisons(vcov = FALSE)` is **7× faster with identical
estimates**. And tabxplor **already owns the exact analytic standard error for that quantity**:
`reg_ame_if_maker()` is pinned to `marginaleffects`' SE **to 10 decimals** by the package's own
tests — it is simply only called in the gap-test path today.

> *tabxplor computes the AME's variance analytically for the colour, then pays `marginaleffects` to
> compute it again, numerically, for the printed interval.*

**Part 1 — take the SE from the influence function.** Where `reg_ame_if_maker()` /
`reg_ame_if_cat_maker()` apply, call `avg_comparisons(..., vcov = FALSE)` and supply the SE from
the influence function. **Declare where it applies as a `REG_ESTIMANDS` column**
(`se = "analytic" | "numeric"`) rather than as an `if` — that is what makes this a key and not a
patch.

**Part 2 — the research question: can `marginaleffects` leave the hot path entirely? Are there, at least, performance improvements to be done, closed-forms to be founds, etc. ?**
⚠ **This half is research first, implementation second. Use web searches — they are expected here,
not a fallback.** The remaining 0.85 s is *not* arithmetic: an AME over n rows is a mean of a
difference of two `plogis()` calls, which is milliseconds. Find out where the rest goes (`newdata`
construction? repeated `model.matrix()`? `insight::get_predicted()`?) and whether the estimate has
a closed form tabxplor can compute itself. Known leads, to verify rather than assume:

- **The contrast AME of a GLM is g-computation over the fitted linear predictor** —
  `mean_i[ g^-1(η_i + Δ_i) − g^-1(η_i) ]`, one pass, no jacobian, no refit. tabxplor already has the
  inverse-link machinery (`rd_link_y()`) and already builds the counterfactual in
  `reg_ame_if_maker()`'s two-term form.
- **For a continuous predictor with no interaction or nonlinear term in it**, the logit AME collapses
  further to `β_j · mean_i[p_i(1−p_i)]` — a textbook closed form. ⚠ The *"no interaction, no
  polynomial, no spline, not the `shape =` quadratic term"* condition is the whole difficulty:
  **that predicate is the declaration**, and getting it wrong is a wrong number, not a slow one.
- **Search what other implementations do**: `marginaleffects`' own performance guidance and whether
  it has since gained an analytic-jacobian path worth using instead of hand-rolling; the older
  `margins` package's delta-method implementation; `mfx`; Stata's `margins` documentation on
  analytic derivatives. **Prefer an upstream fast path over our own** if one exists — a dependency
  that is fast is better than a second implementation to maintain.
- If a closed form does land, `marginaleffects` becomes the **reference implementation the tests pin
  against** rather than a runtime dependency on the hot path — which is the same relationship the
  package already has with `DescTools` for the CI engines. That is the shape to aim at.

**⚠ Non-negotiables for both parts.**
- **Be conservative.** `reg_ame_if_maker()` covers lm/glm/svyglm; `reg_ame_if_cat_maker()` covers
  multinom/polr; `effect = "at_reference"` profiles and `measure = "ratio"` marginals need checking
  **one by one**. Default every new declared column to the slow-but-known route and opt a row in
  **only** with a test that pins it against `marginaleffects` at a stated tolerance.
- **This is *identical maths by a cheaper route*** and must be **demonstrated**, not assumed. Do not
  "improve the statistic while passing through" (§6).
- Ruled (○): a change in the **last printed decimal** of a standard error is acceptable. A change
  anywhere else is a defect.
- **This closes the old 20e** ("marginal effects for a logit regression is neverending"). It is
  neither a cache problem nor a jamovi problem.

**Depends on**: 20c. **Unblocks**: 20f (the re-measurement that decides whether a pool is worth
anything at all).

**Verification**: a tolerance-explicit parity fixture per declared `se` / estimate route, against
`marginaleffects` on the same fit · `dev/verify_reg_specs.R` (the printed values move only where
declared) · `test-benchmark.R` plus a recorded before/after in `dev/benchmarks/results_2.0.0/`.
Sentinels: `test-tab_reg.R`, `test-tab_reg-display.R`, `test-reg-influence*.R`.

---

#### Phase 20e — KEY 6: `reg_build()` becomes a staged build

**Goal**: `tab_reg()` reaches the structural parity `tab()` has had since 17e — which is also what
gives 20f somewhere to attach.

**Read first**: 19o §5 KEY γ (the 534 deparsed lines, the seven local closures, the eleven unnamed
phases) · `new_ctx()` + `ctx_settings_locals()` in `R/tab.R` and 19i's DONE summary (the idiom being
mirrored) · `new_reg_args()` / `new_reg_shared()` in `R/reg-resolve.R`.

**The measurement.** `reg_build()` is the **largest function in the package** — 534 deparsed lines,
with **seven local closures** (`cols_ame`, `cols_vsrest`, `cols_coef`, `emp_frame_of`, `emp_of`,
`set_obs_if`, `add_emp_cols`) against **three** in the entire 670-line factor leaf. It is eleven
sequential phases with no names. `tab_build()` has had `new_ctx()` (71 declared keys) and six named
stages since 17e/19i, and **the asymmetry is not stylistic**: it is why `dev/verify_reg_specs.R` has
to characterise `tab_reg()` through its *output* rather than at a stage boundary.

```r
reg_build <- function(ctx) {
  ctx <- reg_stage_split(ctx)      # the tab_vars recursion (or a no-op)
  ctx <- reg_stage_fit(ctx)        # fits + skeleton + reref      <- THE parallel seam (20f)
  ctx <- reg_stage_columns(ctx)    # the 3 per-spec builders
  ctx <- reg_stage_empirical(ctx)  # crude twins + obs + gap_se + tips + numeric overlay
  ctx <- reg_stage_footer(ctx)     # GOF + comparison + global + checks + curves
  reg_finalize(ctx)
}
```

`new_reg_ctx()`'s formals are the contract (`new_ctx()` / `new_reg_shared()` / `new_reg_args()` —
the idiom is now three times proven) and the `globalVariables()` mirror is **derived** from them,
as `tab.R`'s tail already does. ⚠ The recursion stays at the **top**, exactly as
`tab_build_tables()` does. ⚠ `.fit_cache` is an environment and the ruling is *keep as is, do not
improve* — thread it untouched; `reg_reref_fit_res`'s byte-identity is a hard contract.

**What it also buys**: the `empirical` subsystem finally gets a name. ~100 inline lines plus
`reg_empirical()` (193 deparsed) plus `reg_empirical_columns()` (244) is the third-biggest
subsystem in the package and is currently spelled as an `if` block.

**Depends on**: 20c, and **20d before it** — 20d changes what the marginal path computes, and
restructuring around a moving target is how a pure refactor stops being provably pure.

**Verification**: **pure refactor, no user-visible change** → `dev/verify_reg_specs.R` must print
**IDENTICAL**, which is exactly what that harness was built for. Zero golden churn. **Full suite at
the end of the phase.**

⚠ **Routed here by 20a**: `REG_ESTIMANDS$builder` (`"coef"` / `"ame"` / `"vsrest"`) has **no declared
vocabulary** — it is read by a bare `switch()` in `reg_build()`, so it is the one column of that table
whose legal values live in a consumer. Declaring it belongs with the stage split.

---

#### Phase 20f — `tab_reg()` parallelisation: measure, then decide

**Goal**: answer whether a process pool is worth anything **now that 20d and 20e have landed** — and
be willing to answer no.

**Read first**: CLAUDE.md's old 20f brief in full (it is reproduced below and nothing in it is
superseded) · `R/tab-parallel.R` · Phase 9c's DONE summary (the precedent for a measured "no") ·
Phase o's freeze root-cause.

⚠ **Re-measure first, and write the study before writing any code.** If 20d took the measured call
from 15.3 s to ~2 s or below, the case for a pool may have evaporated — and **"do not parallelise"
is a legitimate and expected outcome**, exactly as Phase 9c concluded for scan fusion after
measuring it. **Write the study in a new `dev/*.md`, pause, and only then plan and implement what
the measurement justifies.**

The brief's constraints are unchanged and all still binding:

- **candidate payloads**, each with very different granularity: per-predictor crude fits (z9's
  numeric `Obs_*`, z10's ordinal `Obs_cumOR` — measured at 2.5× the full model's own cost), per-fit
  (model comparison / several outcomes / `tab_vars` groups), per-contrast, the
  `stats = "interaction"` pooled fits;
- **shipping cost is the known hazard, already measured**: ~10 MB per raw fit, ~41.5 MB serialized
  per jamovi round-trip (Phase o's freeze root-cause). A worker must return `reg_build_digest()`,
  **never a fit**;
- **`.fit_cache` is an env** and cannot cross a process boundary — decide how parallel and cached
  interact before writing any worker;
- **byte-identity and stable ORDER** (`vec_rbind` of split parts, `fit_first_idx`/`fit_ncol`);
- **jamovi viability is not assumed**: mirai's dispatcher needs sockets, which is why
  `test-parallel-parity.R` already fails under the bwrap sandbox. Confirm a pool works inside
  flatpak Electron *before* designing for it; if not, the feature is R-session-only and jamovi keeps
  the serial path;
- **reuse, do not duplicate**: `tab_pool_ensure()` / `tab_parallel_workers()` /
  `tab_parallel_stop()` and the `tab_pmap()` trampoline are the existing infrastructure. A second
  pool, or a second Suggests-guard idiom, would be exactly the ad hoc layer rule 1 forbids;
- if a threshold is the answer, it is a **declared** one (`tabxplor.reg_parallel_min`), and what was
  *not* parallelised is `log()`ged, never silent.

**20e is what makes this tractable**: `reg_stage_fit()` is the only stage that fits models and the
only one whose payload crosses a process boundary, so the pool has exactly one place to attach.

**Depends on**: 20d and 20e, both landed and measured.

**Verification**: `test-parallel-parity.R` extended with a reg operation (⚠ **run it unsandboxed** —
bwrap's `--unshare-net` breaks mirai's dispatcher, CLAUDE.md § Testing) · byte-identity serial vs
parallel · `test-benchmark.R` · and, if the verdict is "no", a recorded measurement in
`dev/benchmarks/results_2.0.0/` plus the reason in the DONE summary. **A measured "no" is a
complete phase**, not a failed one.

---

#### Phase 20f-ii — `tab_reg()` parallelisation at the model level ?

If Phase 20f-i have proven that paralellisation is useless inside a same outcome, is there a performance interest to parallelise it for each model (each outcome when several outcomes ; each predictors list ; each tab_vars) ?

**MEASURED — see `dev/tabxplor_reg_performance.md` §6.** The answer is *axis by axis*, and the
structural finding matters more than the timings:

- **`tab_vars` groups (G)** and **several outcomes × a models list (R)** are the two places
  `tab_reg()` already recurses. Each unit returns a **finished table** — fit-free, KB-sized, the
  cross-unit work already after the loop, the message stream already unit-major. **Dispatchable
  today, with no restructure** — but they clear a ≥2× bar only for an *even* axis at survey scale
  (G 8 even waves 2.28×, G 4 uneven race groups 1.23×), where the whole saving is about a second.
- **The S axis** (several outcomes in ONE table · a models list) is where the ≥2× shapes actually
  are — **2.86× at four outcomes, 2.33× at three balanced models** — and it is the one that cannot
  be dispatched as written: its unit returns the raw fit, and `emp_by_fit[[i]]` carries `$frame` +
  `$fits`, **60–100 MB at n = 200 000**. Hence 20f-iii.
- ⚠ **Balance, not unit count, is the variable**, and **transport is not the obstacle** (shipping
  the 16 MB fixture 0.05 s, a warm round-trip 0.003 s; the 1.6 s is one-off dispatcher setup).
  The ceilings carry ±0.1–0.35× of run-to-run noise, so only the S axis is clear of the bar by more
  than the noise.

**What shipped in 20f-ii regardless**: the crude-block de-duplication (comparison mode recomputed
spec 1 for every spec and read only spec 1 — `reg_empirical` / `reg_empirical_fit` 3 → 1,
`reg_fit` 9 → 5) and a guard on a latent defect beside it (`compare` was gated nowhere, so several
outcomes plus a comparison key reached `reg_compare_rows()` with two different responses).

---

#### Phase 20f-iii — the S axis: `reg_spec_build()`, and the parallelism it unlocks

**Goal**: make the per-spec work of a `tab_reg()` table a **declared product** rather than seven
scattered loops — and, because that is what the payload constraint requires, make the S axis
dispatchable.

**Read first**: `dev/tabxplor_reg_performance.md` **§6.1 and §6.5** (the shape of each axis, and the
four constraints, each read in the code rather than assumed) · 20e's DONE summary in CLAUDE.md (the
staged build this refines, and `new_reg_ctx()`'s idiom) · `R/tab-parallel.R` (`tab_pmap()` is
generic and needs no change) · 20f-i's §3 reason 2 (the message-order contract).

**This is "20e one grain finer."** 20e named the *stages*; this names what each *spec* contributes to
them. Today six stages each carry their own `map(specs, …)` / `for (i in seq_along(specs))` —
`_columns` · `_footer`'s gof / global / check rows · `_rows`' `add_n` · `_empirical` · `_tips`'
numeric block — so "which parts of the table are per-model and which are between-models" is
answerable only by reading four files. **That is the phase's standing win, and it lands even if the
dispatch does not.**

```r
reg_spec_build(sp, i, ctx) -> new_reg_spec_product(cols, gof_rows, global_rows, check_rows,
                                                   emp, tips, nobs, positive_level, y_ref, var_y)
```

⚠ It must return **no fit and nothing holding a reference to one** — `$fit` + `$data` is ~10 MB, and
`emp_by_fit[[i]]`'s `$frame` + `$fits` is 60–100 MB at survey scale (§6.1). That is why
`reg_set_obs()` / `reg_gap_se_columns()` move *into* the per-spec builder: leaving `_assemble` to do
them on the main process would require the fits back, which is the whole thing the constraint
forbids. ⚠ And no dot-prefixed key on the record: `as.list(environment())` defaults to
`all.names = FALSE` and drops them silently (20e's own measured defect).

**The four constraints, and what each excludes** (§6.5):

1. **`reg_compare_rows()` is NOT ported.** It needs two fit *objects* — `stats::anova(m_lo, m_hi)`,
   the `method = "Wald"` → `regTermTest` arm on a survey fit — and re-implementing survey's Wald
   arithmetic would make tabxplor a second producer of a survey quantity (hard rule 5), the same
   class as 20f-i's measured `drop1` vs `anova` divergence (12.47 against 14.25). It stays as it is
   and **forces the serial path**. This is a fact about the statistic, not a limitation: *a
   between-model test needs the models together.* It returns early on `compare == "none"` — the
   default — so it excludes far less than it sounds.
2. **Comparison mode with a crude block stays serial**: spec 1's block is every column's `obs` and
   carries the 60–100 MB frame.
3. **A compound formula stays serial**: the shared skeleton comes from `fits[[1]]`.
4. **The label rewrite.** `built[[k]]$label` is *pre*-`make.unique` while every `test` row's `col`
   key is *post*- (`fit_first_col`). A worker cannot know it, so per-spec rows carry a spec-index
   placeholder that `_footer` rewrites once `labels` exists.

All four live behind **one declared predicate**, `reg_specs_independent(specs, ctx)` — `NULL` when a
spec needs nothing from another spec, else the **reason**, which is `log()`ged so what was *not*
parallelised is never silent (20f-i's own rule). What that leaves parallel: **several outcomes**
(any `empirical`), and **the default models list**, where `compare = "none"` and
`empirical = FALSE` mean there is no shared crude block at all.

**Surface**: `parallel` becomes a shared argument of both producers — one row edit,
`producers = c("tab")` → `c("tab", "tab_reg")` in `R/tab-args.R`, so the generated `@param`, the
`tabxplor.parallel` option, the worker-count rule, the pool and `tab_parallel_stop()` are all the
*same* ones (KEY 4). jamovi stays serial by construction: pass `.fit_cache` as
`tab_parallel_workers()`'s `cache_env`. ⚠ Its declared doc sentence must say where it pays, on
`tab()`'s precedent — here: *many even units against a survey-size frame*, a loss otherwise.

⚠ **Do G and R at the same time or not at all.** They need no restructure (a top-level
`reg_build_group()` + `tab_pmap()`, ~60 lines) and they are provably byte- **and**
message-identical. Shipping `parallel` on `tab_reg()` while `tab_vars` silently ignored it would be
exactly the surface inconsistency Phase 20 exists to remove.

**Depends on**: 20f-ii (the measurement and the payload map), 20e (the staged build).
⚠ **Its own session** — the three `tab_reg()` phases are deliberately separate frames of mind, and
this one is a structural refactor whose proof is a harness, not a benchmark.

**Verification**: ⚠ **re-save `dev/verify_reg_specs.R`'s baseline first.** Every spec, `reg_call`,
column attribute, label and test key must still match, and the messages must be **set-equal** — but
a per-spec design turns the stream **stage-major → spec-major**, so the harness prints *"(same set,
different ORDER)"* instead of IDENTICAL for multi-spec cases. **That order diff is hand-audited and
declared in the DONE summary**; it is the one irreducible price, and it is why this cannot ride
along with a phase that claims IDENTICAL. Plus `test-parallel-parity.R` extended with a reg case
(⚠ **unsandboxed** — bwrap's `--unshare-net` breaks mirai's dispatcher) and a re-run of
`dev/benchmarks/phase20f2_reg_model_axis.R` measuring the *achieved* speedup against §6.2's ceiling.

---

#### Phase 20f-iiii — the reg framework: finished, and CLEAN under parallelisation

**Goal**: finish what 20f-iii routed, and take the reg framework from *"parallel, with three
exceptions you have to remember"* to *"parallel, or one stated reason"*. The exceptions are not a
correctness problem — each is declared, each is reported, and 20f-iii's fixtures pin them — but
**three exceptions is a thing a user carries in their head**, and Phase 20 exists to remove exactly
that kind of load. Each has a route out that is a *measurement*, not a guess, and for each **"keep
it, with the number written down" is a complete answer** (20f's own rule).

**Read first**: `dev/tabxplor_reg_performance.md` §6–§7 (the axes, the achieved numbers, the two
still-routed redundancies) · 20f-iii's DONE summary in CLAUDE.md (the payload rule, the two
placeholders, the honest concerns) · `R/reg-spec-build.R` (all three refusals are one function,
`reg_specs_independent()`).

---

**A — THE THREE REFUSALS, each measured before it is kept or removed.**

1. **`compare != "none"`** — `reg_compare_rows()` needs two fit OBJECTS. 20f-ii §6.3 measured
   transport at **0.05 s for a 16 MB frame**, so returning S fits may cost nothing at all and the
   refusal may be pure caution. ⚠ **but measure the right thing**: a `glm` keeps `$data`, `$model`
   and `$qr`, so serialising one can drag the whole frame — that is the number to get, not the
   round-trip. If it is expensive, the second route is the idiom the jamovi path already uses:
   a **`reg_compare_digest()`** carrying only what `anova()` reads (deviance, df.residual, terms,
   nobs, and for the survey arm what `regTermTest` needs). That would be a genuine simplification —
   a between-model test computable from KB — and it is the same distillation `reg_build_digest()`
   performs for the reref path. ⚠ it must reproduce `anova.glm` / `anova.svyglm` **bit for bit**,
   with the 20f `reg_nested_test()` precedent as the template (and its warning: `drop1.glm`'s
   dispersion is not `summary()`'s).

2. **comparison + `empirical`** — and this one is a *modelling* mistake, not a payload one:
   **the crude block is not spec 1's, it is the OUTCOME's.** Every input to it is table-wide or
   per-outcome, and comparison mode has exactly one outcome — which is precisely why 20f-ii could
   stop rebuilding it. 20f-iii kept it inside the loop only because 20f-ii's `break` had put it
   there. **Lift it out**: build it once (in `reg_stage_setup()`, or its own small stage) and hand
   it to every spec as a `.ship` constant. The refusal disappears, the loop stops carrying state,
   and the code finally says what the block is. ⚠ the one thing to verify first: it reads
   `f$positive_level` and `f$y_ref` off the fit — `reg_positive_level()` is already the fit-free
   producer of the first (`R/tab_reg.R`), and `y_ref` is the outcome's reference category, so both
   look liftable; **confirm, do not assume**.

3. **the deferred skeleton** (an all-coefficient table with a compound formula) — it needs
   `reg_skeleton_from_fit(fits[[1]]$fit)`. Route: `stats::terms(formula, data)` + `model.matrix()`
   on a tiny frame gives the same term names with no IRLS. ⚠ **this is the one most likely to end
   in a declared "no"** — a compound formula is the escape hatch, and a table that uses one is
   rarely a four-model comparison, so the honest outcome may be to keep it and say so in one line.

**The metric for A**: after it, `reg_specs_independent()` returns `NULL` for every shape a user
actually builds, or names **one** reason with a measurement behind it.

---

**B — THE TWO HONEST CONCERNS 20f-iii LEFT, which are one fix.**

4. **A worker's ERRORS are not relayed the way its messages are.** `tab_pmap_trampoline()` collects
   conditions deliberately and not errors (mirai's `[.stop]` re-throws the first), so under
   `parallel` a failing model surfaces with a different call stack than serially and, with several
   units, possibly a *different unit's* error. Shared with `tab()`, so the fix lands in
   `R/tab-parallel.R` and both producers get it.

5. **…and it restores what 20f-iii lost.** Its nine changed messages dropped purrr's
   `i With name: m1.`, so an error in the 2nd model of a list no longer names the model. A
   `reg_spec_build()` that wraps its own body and re-throws with the SPEC's label
   (`rlang::try_fetch` + `cli_abort(parent = )`) fixes it in **both** branches — and names the
   model rather than an index, which is better than what was lost.

---

**C — INTEGRATION AND SIMPLIFICATION (the reward of the restructure).**

6. **Census `new_reg_spec_product()`'s slots for orphans.** Several facts moved into the builder and
   may no longer need to leave it (`emp$crude_key`, `emp$fac_preds`), and the ctx's `fit_of_col` /
   `fit_ncol` should be re-checked for readers now that `_assemble` is four lines. ⚠ the `fit` slot
   is the phase's own subject (A1) — if A1 removes the refusal it stops being conditional.

7. **The nesting rule, stated once.** `tab_vars` × specs × outcomes NEST, and only the OUTERMOST
   axis dispatches — a worker forces `parallel = FALSE` so it never spawns nested daemons. That is
   currently three literals in three files; it should be one declared fact, the way
   `tab_rowvar_ctxs()` states it for `tab()`.

8. **The two still-routed redundancies** (`dev/tabxplor_reg_performance.md` §7.4): the multinomial
   comparison's per-spec grid rebuild (byte-identical to reuse only if every spec resolves the same
   `y_ref` — true today, stated nowhere), and `reg_global_rows()`'s `drop1` reduced-model refits
   (⚠ the only cheaper route is a Wald test, which is a *different number* — so this one is very
   likely a declared keep). Item 2 above may delete the first for free.

9. **`reg_interaction_rows()`**, the fourth fitting site: it lives after the split barrier and needs
   the POOLED data across groups, so it cannot join a per-spec product. Declare that where it sits,
   so the next reader does not re-derive it.

10. **jamovi**: the reg bridge must never send `parallel`, and the live cache forces serial
    (`tab_parallel_workers(cache_env =)`). Today that is a comment; make it **one fixture**.

---

**Surface**: none expected. `parallel` is already the shared argument, and A removes *refusals*, not
arguments — so a table that used to be serial simply becomes faster, and its doc sentence gets
shorter.

**Verification**: `dev/verify_reg_specs.R` (declared deltas only — and A1's digest route, if taken,
must be pinned against the real `anova()` at `expect_identical`) · `test-parallel-parity.R`
extended to the shapes A un-refuses (⚠ **unsandboxed**) · a re-run of section **1d** of
`dev/benchmarks/phase20f2_reg_model_axis.R`, which now measures the achieved speedup directly, with
a comparison-mode row added.

**Depends on**: 20f-iii. ⚠ **its own session**, and the same reason as its three predecessors: A is
a numerical-parity question, B is a plumbing fix in a shared file, and C is a census. Interleaving
them is how a refactor and a numeric change land in one diff and neither can be verified.

---

#### Phase 20g — jamovi: the level-collapse UI, the boundary, and the rebuild

**Goal**: the module speaks the 2.0.0 vocabulary, gains the level-collapsing UI on **both**
analyses, and is actually rebuilt and driven.

**Read first**: `dev/tabxplor_2.0.0_jamovi_dev.md` · `dev/jamovi/` (including the live console
captures) · CLAUDE.md § "Jamovi module development" in full (the `ELECTRON_RUN_AS_NODE` trap, the
`R_LIBS_USER` trap, the WSLg copy-mode fix, the pinned/masked 2.7.36, the `jmvtools` 2.7.26 pin) ·
19k's DONE summary · §7.1 items 5–8 and §7.3 here.

**Contents**

- **Carry every new vocabulary into `jamovi/*.a.yaml` / `*.u.yaml` / `js/`**: 20b's renames and the
  `stars` ladder, 20c's `footer` / `ci_method` / `tab_vars` / `ref`, plus anything 20d exposes.
  `test-jamovi-vocabulary.R` is the gate — every List option's value set must **equal** the R
  vocabulary it names. **Generate what can be generated** (`dev/generate_jamovi_js.R`), never
  hand-write a second copy.
- **Add what is missing and remove what is dead.** The UIs have not been swept since before Phase
  19; several arguments added in 19d–19m have no control and several controls name retired values.
  ⚠ **Ask the maintainer** when a control's presence is a *teaching* decision rather than a
  mechanical mapping — the jamovi UI is differentiator #4, and not every R argument belongs in it
  (`filter` was deliberately removed; the `.`-prefixed internals never belonged).
- **The level-collapse UI (★, both analyses, generated once).** A tick-box per level (none ticked
  by default; **no tick-box on a variable's first level**) meaning *collapse this level into the
  previous one*; chained ticks collapse a run; a text box spanning the full vertical extent of the
  collapsed run names the merged level. The chain must respect the user's chosen level order.
  - **It is a `tabxplor_lvl` operation, so the R half belongs to the row model**, not to the
    module: build the collapse as a real, testable R operation over declared levels and let the
    module call it. That is what stops it being written twice — the exact history of
    `detectFamily()`, which 19k had to delete from two `.js` files.
  - The same widget serves `jmvtab` (row/col/tab vars) and `jmvtabreg` (predictors). **Emit it into
    both `.u.yaml`s from one generator**, for UI consistency and so it cannot drift.
  - **Layout consequence, stated in the brief**: to give the text box horizontal room, `row_vars` /
    `col_vars` / `tab_vars` each need their own full-width row rather than two side-by-side panes.
    Design that first — it is a whole-panel change, not a widget addition.
  - ⚠ Consider and record the caveats at plan time: what a collapse means for a *total* level, for
    an `"Others"` level created by `other_if_less_than`, for `levels = "first"`, for the tier-3
    cache tuple (a collapse changes the aggregate, so it cannot be a re-paint), and whether the
    existing `.levels_order` / `jmv_relevel_cols` seam is the right carrier or a second one.
- **The export folder should be readable, not clever.** Today `~` works but is opaque to the
  non-expert audience the module is for. Invert it: at load, resolve the Documents folder and
  **write the actual path into the text box** so the user sees where exports will go (the "default
  path" button does exactly the same thing); `~` keeps working and keeps meaning home; and a
  network drive is shown the readable way (`S:/…`), never as a UNC path.
- **The `ci = "cell"` + mixed col_vars display divergence** — ⚠ **reproduce it before fixing it**
  (§7.3). It is the shape of D11, which 19k reports as closed.
- **The outstanding rebuild.** `jmvtools::prepare()` + `jmvtools::install(home = "flatpak")` + a
  live pass, owed since 19k. Until it runs, `measure`, `shapes` and the renamed `test` read `NULL`
  in the running module. **This is a maintainer step and it is a prerequisite for the release.**

**Depends on**: 20b, 20c, 20d. **Verification**: `test-jamovi-vocabulary.R` green ·
`dev/generate_jamovi_js.R check` clean · the jamovi cache cold + warm + re-ref lock ·
`test-jmvtab-cache.R` · **and a live pass by the maintainer**, whose result belongs in the DONE
summary. ⚠ Bump `JMVTAB_CACHE_SCHEMA` for anything that changes what a carrier stores.

---

#### Phase 20h — Harvest 1: the deletion pass

**Goal**: reap it. **Part 1 — the deletion pass.** Re-run the censuses, delete the shapes the new declarations made unnecessary.

- **Re-run §2's censuses** and report the delta honestly, including **what did not shrink**. That
  report is the phase's primary product, not a footnote.
- **Hunt the shapes the new facts made unnecessary**, with the sweeps 19l committed: a zero-caller
  sweep (⚠ under `LC_ALL=C`, ⚠ never `grep -w` on a pattern ending in `(`), a "what still guesses"
  sweep (rendered labels, name prefixes, positional picks, in-band separators, silent length
  fallbacks), and `dev/verify_no_ghost_functions.R`.
- **The named candidates**, all measured by 19o §7 and none urgent on its own:
  - **`plain_core()` is 482 deparsed lines and does eleven things** — 19l's declaration block made
    it *readable*; `leaf_reshape()` / `leaf_compare()` / `leaf_infer()` / `leaf_totals()` would make
    it *navigable*;
  - **the `"Total"` sentinel** — settled correctly in 19m-iii as the leaf's internal pre-rename key,
    but a source factor level genuinely named `"Total"` is indistinguishable from it until
    `leaf_rename_totals()`. The honest fix is one `cli_abort` (a reserved-name check at
    `tab_prepare()`), not a redesign;
  - **the `wn` field's population rule** — an *unweighted* `color = "contrib"` table has `wn`
    populated and an unweighted plain one does not (19j's preserved `set_wn(col, get_wn(col))`
    quirk). Harmless (`get_wn()` coalesces) but it is a field whose population depends on an
    unrelated argument: state the rule or drop the write;
  - **the transpose's synthetic render model** — 19h made it a *modification* of `rd` rather than a
    39-slot literal, which closed D1, but it remains a second render model every backend tolerates,
    and `tab_transpose()` on a regression aborts: the one cross-producer operation that did not
    become uniform;
  - the ~20 **dead formals sitting at positional argument slots** that 19l deliberately left (they
    need a call-site-by-call-site read — 19l dropped one and the i18n tests caught it);
  - **routed here by 20a**: `materialize_specs()$kind` has **no reader at all** and none of its five
    values is a `ROW_KINDS` value, although its header claims it matches "the stored row-role
    vocabulary"; `tab_totcol_range()` (`R/tab-export-prep.R`) is an orphan producer kept alive only
    by its own test, its consumer commented out with the dormant `tabxplor.totcol_range`; and **the
    deprecated-call corpus migration** — 57 warnings, all from calls the test suite makes to the
    hard-deprecated steps and to `tab_prepare()` (§7.2 item 10).

**Verification**: both proofs with **empty** declaration sets (`verify_golden_field_delta.R`,
`verify_color_attrs.R`) — a deletion pass that moves a value has stopped being a deletion pass.
Targeted tests only; the full suite and `check()` belong to 20i.

---

#### Phase 20i — Harvest 2: open integration

**Goal**: reap it. **Part 2 — open integration.** ⚠ *Creative; ask before building.* Think freely about what the finished surface makes possible.

Now that an argument is declared, a row and a column self-describe, a table states its kind, both
producers speak one vocabulary and the footer has a model — **what becomes possible that was not to further simplify and integrate the package code ecosystem ?**

#### Phase 20j — Harvest 3: open cleaning

**Goal**: reap it. **Part 3 — open cleaning.** Think freely about what the finished surface makes possible: what last cleaning steps needs to be done to remove trace of past implementation altogether, reduce code length, ? 



---

## 10. The hand-over: what Phase 20 owes to Phase 22 and to the release

CLAUDE.md already carries **Phase 22 — documentation integration and simplification** (22a–22g) and
a release phase after it. The sequence is **Phase 20 → Phase 22 → release**. Nothing below is a new
phase; each row names the *existing* home of an item Phase 20 deliberately does not do.


### 10.2 To the release phase

`dev/release_checklist.md` is the procedure and it is complete; what Phase 20 hands it is the
**gate set** and three maintainer items.

- **The gates, all on the final tree**: full suite · the **CI-locale run**
  (`LC_ALL=C.UTF-8 LANGUAGE=en` — the French blocks must **skip**, not fail) · the five harnesses ·
  `document()` idempotent · `tools::checkDocFiles()` silent · **`devtools::check()` Status OK** ·
  `pkgdown::build_site()` + `check_pkgdown()`. ⚠ 19n found three `check()`-only failures invisible
  to the suite (a test reading `.Rbuildignore`d `jamovi/*.yaml`; an undeclared `Suggests`; an
  undeclared data.table NSE symbol) — **`check()` is not a formality here.**
- **The branch mechanics** (do not improvise them): `release/x.y.z` off `dev`; one strip commit
  (`dev/`, `.claude/`, `.vscode/`, `CLAUDE.md`, `air.toml`); a PR to `master` merged with **a merge
  commit, never squash** (squash breaks the merge-base and the next release re-conflicts on every
  dev-only file); ⚠ `.Rbuildignore` identical on both branches; then CRAN, then
  `git tag vx.y.z <merge-commit-sha>`. ⚠ **The only existing tag is `v1.2.0`** — 1.3.0 and 1.3.1
  were never tagged, which is why §7.2's baseline correction was needed. Tag this one.
- **Maintainer items owed**: the **README hero screenshot** (`man/figures/README-hero.jpg` predates
  the 2.0.0 OKLCH palettes; I cannot re-shoot one — reproduce it with the first `tab()` call in
  `README.Rmd` under `set_color_palette(theme = "light")`); `cran-comments.md` /
  `CRAN-SUBMISSION`; and the jamovi rebuild + live pass if 20g left it open.
  ⚠ **`devtools::build_readme()` is the wrong tool** — it renders `github_document`, strips the
  YAML header and hard-wraps every paragraph (+1329 lines of churn). The committed `README.md` is
  `knitr::knit("README.Rmd", "README.md")` with the package **loaded** first.

### 10.3 Deliberately not done at all

| item                                               | why                                                                                                        |
|----------------------------------------------------|------------------------------------------------------------------------------------------------------------|
| making the legacy step functions **defunct**       | 2.1.0 — they are hard-deprecated in 20a, which removes nothing yet (§7.1 item 2)                           |
| a **JS syntax / lint gate**                        | no `node`, no `V8` on this box. Declined in 19n; the record was corrected twice (§7.1 item 8)              |
| changing **`pct`'s default**                       | ruled: state it instead (★)                                                                                |
| a **`tab_inference()` bundle**                     | rejected outright (★) — and the reason is the general test for every future bundle (§4)                    |
| a **`tab_style()` bundle**                         | rejected outright (★) — same test, plus `tab_export()` and the options already do the job. **KEY 8 lands as a `TAB_ARGS` extension in 20b instead**, which was always what the measurement asked for |
| the **`tab_kable_*` / `xl_font_*` option renames** | dropped (○, the 19m-iii ruling stands). **Delete the row rather than re-propose it a third time**          |
| **column-axis `ordered`**                          | 19f deferred it with 19b's own admission test: a 17th attribute with no reader is not a fact, it is weight |
| **`tab_many()` hard-deprecation**                  | ★ stays soft — it was the documented main function through 1.3.1                                           |

---

## 11. What this plan is not

- **Not a statistics review.** Phase 19, 19o and 19p each found **no soundness problem**, and
  nothing here changes a number except KEY 7 — which is explicitly *the same maths by a cheaper
  route* and must be demonstrated as such.
- **Not a re-litigation of a settled ruling.** §4 lists thirty; where this document disagrees
  with 19o or 19p it is on a **measurement** (§7.2), not on a decision — except for the two
  bundles, which the maintainer rejected outright, and `outcome_level`, which replaced 19p's
  "absorb it into `ref`" with a second argument because the two questions are opposites.
- **Not a substitute for each phase's own plan.** Every entry above states *what* and *why*, and
  deliberately leaves *how* to the focused session that plans it. If a phase entry reads as a
  specification, it is over-written — trim it rather than follow it literally.
