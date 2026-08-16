# tabxplor — Phase 20: the surface — how the package is asked, and what it exposes

**The plan of plans for the last development stretch before the 2.0.0 CRAN release** — goals, design
and architecture decisions first, then the phased roadmap.

Written 2026-08-15 from the two assessments that precede it (`dev/tabxplor_phase19_assessment.md`
= Phase 19o, and `dev/tabxplor_phase19p_api_review.md` = Phase 19p), plus the maintainer's rulings
taken while this plan was written. **Those two documents are the evidence; this one is the plan.**
Where they disagree with this document, this one wins — it carries decisions they left open, and
five corrections to their measurements (§7.2).

**Companion documents** — read the one that matches what you touch:

| document | what it holds |
|---|---|
| `dev/tabxplor_phase19_assessment.md` | 19o: what Phase 19 achieved and cost; keys α–η; the white-elephant ledger; the perf root-cause |
| `dev/tabxplor_phase19p_api_review.md` | 19p: the review of the *ask* — fields, exports, arguments, options; keys A–D |
| `dev/tabxplor_phase19_ecosystem_integration.md` | Phase 19's plan of plans — the hard rules this phase inherits |
| `dev/ecosystem_keys_2.md` | the Phase 19 study: measurements, eight keys, defect ledger |
| `dev/tabxplor_ecosystem_simplification.md` | round 1 (Phase 17) — the disease patterns |
| `dev/tabxplor_architecture.md` | the **current** architecture — update it as phases land |
| `dev/release_checklist.md` | the release mechanics (Phase 21) |

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
level-collapsing UI (20f) and — only if the measurement justifies it — `tab_reg()` parallelisation
(20d part 3).

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
`jamovi/*.a.yaml` in 20f, or the teaching path starts lying. That coupling is why 20f is late and
why `test-jamovi-vocabulary.R` is a hard gate rather than a convention.

---

## 2. The measured starting state

Measured on `5bba715` (Phase 19p). Everything here is from 19o §2 / 19p §0, plus this session's
re-measurements.

| fact | value |
|---|---|
| R source | **44 278** lines · 21 749 code · 19 263 comment (43.5 %) |
| top-level functions | **1 066**, median 17 lines |
| the two biggest | `reg_build` **534 deparsed lines** (7 local closures) · `plain_core` 482 |
| user messages | **197** — `cli_abort` 149 · `cli_warn` 11 · `cli_inform` 37 |
| … at an argument boundary | **121 of 197 (61 %)**, across 7 files |
| `tab()` formals | **52** — 9 deprecated, 4 internal dot-args, 39 live |
| `tab_counts` / `tab_plain` / `tab_num` / `tab_reg` | 40 · 29 · 28 · 29 |
| mirrored formals | **83 of the 149 crosstab formals** are the same argument written a 2nd–4th time |
| `man/` | **8 930** lines · `tab_reg.Rd` 722 · `tab.Rd` 695 · `fmt.Rd` 693 · `tab_many.Rd` 448 |
| exports | **93** — released baseline **CRAN 1.3.1 = 63**, so **35 new, 5 removed** |
| exports in no vignette and no README | **52 of 93** |
| global options | **35** documented · 34 seeded · 1 documented-but-never-seeded |
| declared fact tables | ~15, with **7** build-time `stopifnot` blocks, of which **2** are cross-table |
| unchecked cross-table foreign keys | **≥12** (all currently intact; one has already broken in a shipped commit) |
| `R/tab-steps-legacy.R` | **1 433 lines**, **zero real callers in `R/`** (44+15 textual hits are all comment/roxygen prose) |
| `effect = "marginal"` on 21 483 rows × 4 predictors | **15.32 s**, of which **85 % is `marginaleffects::get_jacobian`** |

**The two most diagnostic numbers**: 83 of 149 crosstab formals are duplicates of an argument that
is already declared, and 61 % of everything the package says to a user is still said while
negotiating arguments. The first is what Phase 20 deletes; the second is what it makes declarable.

⚠ **The metric warning, carried from 19o §9 and restated because it will be tempting to forget**:
Phase 19 grew 11.9 % and got substantially better. **Do not count lines as the simplification
metric.** Phase 20 will also grow `R/` — `TAB_ARGS`, `TEST_ROWS`, `tab_style()`, three generators
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

| key | the missing fact / unstated rule | what it stores or states | source | lands in |
|---|---|---|---|---|
| **KEY 1** | *which producer takes which argument, what it means, what it may be, and which option is its default* | `TAB_ARGS` — the argument surface as data; the signature, the `@param` block and the value list all derive from it | 19o α · 19p C+D | **20b** |
| **KEY 2** | *a key written in one declared table and read in another is a foreign key* | ~14 cross-table checks at load time, ~30 lines | 19o β | **20a** |
| **KEY 3** | *which accessors exist* — the exported get/set family is the last hand-written mirror of `fmt_col_attrs` | one generic `fmt_attr()` pair + a measured keep-list of named accessors + `tab_columns()` | 19p A | **20a** |
| **KEY 4** | *if two producers ask the same question, they ask it with the same word* | `tab_vars`, `ref`, `ci_method`, `footer` on both | 19o ε · 19p §5 | **20c** |
| **KEY 5** | *what kind of statistical row this is, what it is about, and how it renders* | `TEST_ROWS` — the crosstab half of the footer subsystem finally declared | 19o δ | **20c** |
| **KEY 6** | *which stage of a regression build produced which part of the table* | `new_reg_ctx()` + five named stages, mirroring `tab_build()` | 19o γ | **20d** |
| **KEY 7** | *which estimands tabxplor can differentiate analytically* | a declared `se = analytic \| numeric` column; the AME stops being computed twice | 19o ζ | **20d** |
| **KEY 8** | *which arguments are a rendering style rather than a table* | `tab_style()` — 28 mirrored formals across five exporters | 19p §4.7 | **20e** |
| **KEY 9** | *a package whose whole value is a data model states that model in one place* | `?tabxplor-model` + one reader naming convention | 19o η | **Phase 22b** (§10) |

**KEY 1 is the keystone of this phase**, the way KEY 5 was of Phase 19. Everything else is either a
prerequisite for it (KEY 2, KEY 3), a second instance of it in another subsystem (KEY 4, KEY 5,
KEY 8), or independent of it (KEY 6, KEY 7).

---

## 4. Settled decisions — do not re-open

All rulings in force. 19o §11's eight (marked ○), 19p's eleven (marked ◆), and the eight taken while
this plan was written (marked ★).

| decision | ruling |
|---|---|
| ○ `tab()`'s 9 deprecated formals | **move into `...`**, caught by name, with an **abort on an unnamed 6th argument** |
| ○ the legacy step API | **hard-deprecate now**, defunct in 2.1.0. The *computations* moved into the leaf in 19j; what is deprecated is the exported *chaining API* |
| ○ `tab_reg(split_var =)` | **→ `tab_vars`**, with `split_var` a permanent silent alias |
| ○ `tab(color =)`'s default | the `"no"` vs `TRUE` asymmetry with `tab_reg()` is **deliberate and NOT documented** |
| ○ KEY 7's tolerance | a change in the last printed decimal of a standard error is **acceptable** |
| ○ setters stay exported | …reconciled with ★ below: the *keep-list* includes `set_row_kind()` |
| ○ the five `tab_kable_*` option renames | **stay dropped** (the 19m-iii ruling stands) — do not re-propose |
| ○ comment archaeology | **its own phase, numbered 22** — out of Phase 20 |
| ◆ `tab_logit()` / `multi_logit()` | **deleted** (unreleased — verified against CRAN 1.3.1, §7.2) |
| ◆ `tab_reg(reference =)` | **→ `ref`**, taking `c(var = "level")`, **absorbing `inverse_two_level_factors`** |
| ◆ `tab(ref / ref2)` | **unchanged** — two arguments, different per-axis defaults |
| ◆ `na`'s two vocabularies | **keep both** — they describe different operations — but generate both value lists from their declaring table |
| ◆ `tabxplor.stars` | absorbs `signif_levels` + `signif_labels`, **and becomes a per-call ladder** |
| ◆ `options(tabxplor.total_names = c(row=, col=, tab=, other=))` | **new** — three hard-coded label defaults in five signatures, in two languages, with no option twin |
| ◆ `tabxplor.color_style_type` | **deleted** (documented, never seeded, read only to emit its own warning) |
| ◆ `@inheritDotParams` | **never** — it inlines. `tab_many.Rd` is the 448-line proof |
| ◆ `...` | on **wrappers and superseded producers only**. `tab()` and `tab_reg()` keep every live formal |
| ◆ teach, do not cut | `tab_shape()` / `tab_supports()` / `reg_measures()` (+ the new `tab_columns()` / `fmt_attr()`) get an *"Inspect a table"* vignette section |
| ★ **the inference bundle** | **NO BUNDLE.** 19p §4.2's `tab_inference()` is **rejected**. `ci_method` / `design_effect` / `anova` stay flat formals with their option twins; the only change is `tab_reg(method =)` **→ `ci_method`** with a declared `model` slot in `CI_METHODS` |
| ★ `tab_style()` (the exporters' bundle) | **inside Phase 20**, pre-release — 20e |
| ★ `TEST_ROWS` | **inside Phase 20**, pre-release — 20c, together with its first consumer `footer =` |
| ★ `tab_reg()` parallelisation | **inside Phase 20** — 20d part 3, gated on the re-measurement after KEY 7 and KEY 6 |
| ★ the jamovi level-collapse UI | **inside Phase 20, pre-release**, both modules, generated once — 20f |
| ★ `tab_many()` | **stays soft-deprecated** — it was the documented main function through 1.3.1 and real scripts call it. Only its `.Rd` is fixed |
| ★ `new_lvl()` / `is_lvl()` | **un-exported** (new in 2.0.0, so free) |
| ★ `tab_prepare()` | **`@keywords internal`** — keep the export, drop it from the reference index |
| ★ the accessor family | **generic mostly, a few named ones kept — the most used** (`get_col_var()` was named as the example). The keep-list is decided from measured usage at plan time and **must include `set_row_kind()`** (○) |
| ★ `pct`'s `"no"` default | **unchanged** — say so explicitly in `?tab` so it stops reading like an oversight |

⚠ **The one ruling that reverses a proposal outright**: there is **no `tab_inference()`**. The
reason is worth keeping, because it is the general test for every future bundle — `ci_method` has 19
corpus uses and is a documented per-call argument on four producers, so
`inference = tab_inference(ci_method = c(diff = "wald"))` is strictly more typing than
`ci_method = c(diff = "wald")`, and the key's own anti-proposition forbids keeping it in both
places. **A bundle must make the common call shorter, not only the signature.** `tab_style()`
(20e) survives that test because its fields are genuinely per-document and rarely set; the
inference one did not.

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
   question                 tab()                 tab_reg()             after Phase 20
   --------                 -----                 ---------             --------------
   which sub-populations    tab_vars              split_var             tab_vars   (alias kept)
   which baseline           ref / ref2            reference +           ref  (c(var = "level"))
                                                  inverse_two_level_…
   how is the interval      ci_method (4 slots)   method                ci_method  (5th slot: model)
   what rides the footer    test (an omnibus      stats + compare       test  |  footer
                            test — a different     + baseline
                            question, stays)
   default colour           "no"                  TRUE                  unchanged (deliberate)
   missing data             keep/drop/…           drop_by_outcome/…     unchanged, both generated
```

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
  tab_build(ctx)                              reg_build(ctx)                    after 20d
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
4. **20d part 3 may legitimately end with "do not parallelise".** If KEY 7 takes the measured call
   from 15.3 s to ~2 s, a process pool may cost more than it saves on the common one-model call.
   The phase must be allowed to reach that verdict and record it, exactly as Phase 9c did for scan
   fusion.
5. **The jamovi UI (20f) is the one item only the maintainer can finish.** Everything in `R/`,
   `jamovi/*.a.yaml`, `*.u.yaml` and `jamovi/js/` can be written and gated here; the generated
   `.h.R`, the rebuild and the live pass cannot. **Any phase that edits a `.a.yaml` / `.u.yaml`
   leaves it inert until the next `jmvtools::prepare()`** — say so in the DONE summary rather than
   claiming the UI changed.
6. ⚠ **The `.h.R` regeneration owed since 19k is still outstanding.** Until it runs, `measure`,
   `shapes` and the renamed `test` read `NULL` in the running module. 20f is where it lands, and it
   is a prerequisite for the release.
7. **The jamovi vocabulary is coupled to every rename.** `test-jamovi-vocabulary.R` asserts that
   each List option's value set EQUALS the R vocabulary it names. Every rename in 20b/20c turns it
   red until `dev/generate_jamovi_js.R` and the `.a.yaml` follow. That is the gate working, not a
   failure — but it means 20f cannot be skipped.
8. **There is still no JS syntax gate** and there cannot be one on this box (no `node`, no `V8`).
   Declined in 19n; recorded so it is not re-proposed as work.
9. **`pct`'s default stays `"no"`** — so the most-used argument in the corpus keeps a default users
   rarely want. Ruled; the mitigation is one explicit sentence in `?tab`.
10. **Two live `FIXME`s remain in the colour engine** (`R/fmt_class.R:6508` *"is the AND right?"*,
    `:6521` *"suspect."*). They are the only open `FIXME`s in `R/` (the other two are
    retrospective). Resolve or state them in 20a — an unanswered question in the engine's own
    comments is the archaeology problem in its acute form.

### 7.2 Five corrections to 19o and 19p

Recorded so the ledger stops carrying them.

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

### 7.3 One stale defect note to verify before acting on it

CLAUDE.md's draft 20d records: *"On the jamovi cache path a table built with `ci = "cell"` and MIXED
col_vars renders its numeric column with the `pct_ci` display token where plain `tab()` renders
`mean_ci`."* That is the shape of **D11**, which 19k reports as closed (`jmv_apply_display()` was
deleted for `tab_apply_display()`, and since 19j the leaf stamps that display itself).
**Reproduce it first in 20f.** If it no longer reproduces, delete the note; if it does, it is a
real defect with a fixture, and the two closures diverged again.

---

## 8. Verification discipline

Deliberately light, on Phase 19's model. These are floors, not ceilings.

- **Per phase the default is targeted**: the test files your change touches (`filter =`) plus the
  sentinels the phase entry names. **Do not run the full suite after every edit.**
- **Full suite** (CLAUDE.md § Testing recipe — `OMP_NUM_THREADS=1`, `TESTTHAT_CPUS=8`, a temp
  runner outside `tests/`) at **three checkpoints: end of 20b, end of 20d, end of 20g.**
- **`devtools::check()` once, at the end of 20g** — not as a release gate (that is the release
  phase's job, after Phase 22) but so **Phase 22 does not inherit a broken tree**. 19n found three
  `check()`-only failures invisible to the suite; three minutes here is cheap insurance.
- **The CI-locale run** (`LC_ALL=C.UTF-8 LANGUAGE=en`) belongs to the release phase, not here.
- **Goldens**: each phase entry names which families may move. Prove the delta with
  `dev/verify_golden_field_delta.R`, teaching it one new mode per new kind of delta.
- **Byte-identical phases** (20a, most of 20b, KEY 6) tolerate **zero** golden churn.

### The harnesses

Four exist; **two are new in 20a and are the gate for 20b and 20c**.

| harness | covers | status |
|---|---|---|
| `dev/verify_golden_field_delta.R` | 1 788 cells × 36 goldens: every field, column attribute, `test` column, `meta` sub-field | exists |
| `dev/verify_color_attrs.R` | 293 cases: every stored colour attribute + both resolved slot vectors | exists |
| `dev/verify_reg_specs.R` | 291 cases: the messages **in order**, the specs, `reg_call()`, column attributes, labels, test keys | exists |
| `dev/verify_no_ghost_functions.R` | comments naming functions that no longer exist | exists (a report, not a gate) |
| **`dev/verify_tab_args.R`** | every crosstab producer's **resolved** settings over a call grid — `tab_resolve_common_args()`'s return plus the stored per-column attributes | **build in 20a** |
| **the export-usage census** | so "this export has no caller" is re-measurable rather than re-derived (⚠ and re-derived wrongly — see the `grep -w` trap below) | **build in 20a** |

⚠ **Two measurement traps, both of which produced a wrong census while 19p was written**: run every
`sort` / `uniq` / `comm` census under **`LC_ALL=C`** (the box is `fr_FR.UTF-8`, whose collation does
not group identifiers containing `_` / `.`), and **never use `grep -w` on a pattern ending in `(`**
— it reported nine live exports as having zero callers. Use `(^|[^a-zA-Z0-9._])name\(`.

---

## 9. The roadmap

**Seven phases.** Each is *plan-then-implement*, starting in plan mode, in its own fresh session.
The maintainer commits between phases and pushes at the end of Phase 20.

The order below is the recommended one; what is **binding** is the dependency list.

| phase | title | must land after | because |
|---|---|---|---|
| **20a** | The floor: referential integrity, the exposed surface, the dead weight | — | KEY 2 protects every later table edit; the two harnesses gate 20b/20c |
| **20b** | KEY 1 — the argument surface as data | 20a | needs `verify_tab_args.R` |
| **20c** | KEY 4 + KEY 5 — one word per question, and the footer's model | 20a | independent of 20b; do it after so `TAB_ARGS`' idiom exists |
| **20d** | `tab_reg()`: the analytic marginal SE, the staged build, the parallelisation verdict | 20c | the surface must settle before the internals restructure |
| **20e** | KEY 8 — `tab_style()` and the exporters' mirror | 20a | independent; shares KEY 1's shape, different verification |
| **20f** | jamovi: the level-collapse UI, the boundary, the rebuild | 20b, 20c, 20d | it carries every new vocabulary into the UI |
| **20g** | Harvest: the deletion pass and open integration | everything structural | it measures and exploits the finished surface |

⚠ **There is deliberately NO documentation phase in Phase 20.** CLAUDE.md already carries
**Phase 22 — documentation integration and simplification** (22a–22g), which owns the architecture
document, the vignettes, the roxygen sweep, the comment rewrite, `NEWS.md`, the tests and `dev/`.
A Phase 20 doc phase would be a second pass over the same files — the duplication this plan exists
to delete. §10 maps every documentation item to its Phase 22 home instead. What each Phase 20 phase
still owes is the standing rule 9 discipline: **update the docstrings and `@param`s you changed, in
the phase that changed them** — and after KEY 1 most `@param` blocks are *generated*, so a rename
documents itself.

**Mapping from the CLAUDE.md draft** (so nothing is lost): old **20d** (jamovi UI) → new **20f** ·
old **20e** (the marginal-effects freeze) → new **20d part 1**, where it is root-caused as KEY 7 ·
old **20f** (parallelisation) → new **20d part 3**. Phase 22 and the release phase are unchanged.

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
  - **un-export** `new_lvl()` / `is_lvl()` (★) and `complete_partial_totals()` (released → deprecate
    the export, keep it internal; its one caller is tabxplor's own);
  - **`@keywords internal`**: `tab_prepare()` (★ — and move it out of the pkgdown *"Superseded
    entry points and steps"* section, which reads as a verdict it has not been given),
    `tab_get_wrapped_dimensions()`, `tabxplor.jmv_full_hash` → an internal constant;
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

#### Phase 20b — KEY 1: the argument surface as data

**Goal**: an argument is declared once. The signature, the reference page, the value list and the
option twin all read that declaration — and 83 mirrored formals stop existing.

**Read first**: 19o §5 KEY α · 19p §4.3 (KEY C), §4.4 (KEY D), §4.5, §4.8 · §7.1 item 1 here.

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
  `totaltab_name` + `other_level` fold into the `total_names` named vector (below). ⚠ **The three
  cautions of §7.1 item 1 are not optional**: the unnamed-6th-argument abort;
  `names_prefix` / `names_sort` forwarded or moved to `tab_spread()`; `method_cell` / `method_diff`
  converted from `missing()` to `NULL` defaults **first**.
- **The two option changes** (◆): **`tabxplor.stars` absorbs `signif_levels` + `signif_labels`
  and becomes a per-call ladder** (`FALSE` / `TRUE` / `c("*" = 0.10, "**" = 0.05, "***" = 0.01)`) —
  today the ladder is option-only, so one table in a document cannot use a different ladder from
  the next although `stars =` is already a per-call argument on four producers; and
  **`options(tabxplor.total_names = c(row =, col =, tab =, other =))`** — the three label defaults
  are hard-coded literals in five signatures, **and not even in one language**
  (`"Total"` / `"Ensemble"` / `"Others"`), with no option twin at all. For a French-authored package
  with a French audience that is a real gap, and it is the reason `totaltab_name` and `other_level`
  exist as formals nobody sets.
- **One asymmetry closed while there**: `var_names` is both an option and a per-call argument on
  five exporters; `var_labels` is option-only, although they are the same kind of display decision
  about the same names.
- **`pct`'s `"no"` default is stated, not changed** (★) — one explicit sentence in `?tab`.

**Depends on**: 20a (`dev/verify_tab_args.R`). **Unblocks**: 20f (the jamovi vocabulary).

**Verification**: part 1 must be **byte-identical** — `document()` idempotent,
`tools::checkDocFiles()` silent, zero golden churn, zero `_snaps/` churn; the only diff is `man/`.
Part 2 is gated by `dev/verify_tab_args.R` printing IDENTICAL except for the declared delta. **Full
suite at the end of the phase.** ⚠ `test-jamovi-vocabulary.R` goes red on any renamed value and
stays red until 20f — expected, and it must be *stated* in the DONE summary, not silenced.

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
    accepts, **and absorbing `inverse_two_level_factors`** as an entry for the *outcome*:
    `ref = c(race = "White", married = "Not married")`. That logical is a 25-character formal with
    0 corpus uses that encodes which level of a binary outcome is modelled *by toggling level
    order*; naming the level is strictly better — it is what the user knows, it is checkable, and it
    is one grammar with the predictors' baselines beside it. `tab(ref / ref2)` stay two arguments
    (◆).
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

**Depends on**: 20a. **Unblocks**: 20d (the surface must settle before `reg_build` is staged), 20f.

**Verification**: **`dev/verify_reg_specs.R` must print IDENTICAL** except for the declared rename
delta — it dumps the messages in order as well as the specs, which is exactly what a boundary
rename can silently reorder. Golden delta for the `test` tibble (`TEST_ROWS` must not move a
value). Sentinels: `test-tab_reg*.R`, `test-test-display.R`, `test-reg-checks.R`.

**Estimated effect**: `tab_reg()` 29 → ~25 named formals + `...` · `tab_reg.Rd` 722 → ~550 · six
cross-producer name collisions → zero.

---

#### Phase 20d — `tab_reg()`: the analytic marginal SE, the staged build, and the parallelisation verdict

**Goal**: the regression producer stops paying for a variance it already computes, gains the staged
build `tab()` has had since 17e — and then, and only then, is measured again to decide whether a
process pool is worth anything.

**Read first**: 19o §5 KEY ζ (the `Rprof`, the 7× measurement, the analytic IF) and KEY γ (the 534
deparsed lines, the seven local closures, the eleven unnamed phases) · CLAUDE.md's old 20f brief in
full (the candidate payloads, the shipping-cost hazard, the `.fit_cache` seam, the jamovi/flatpak
socket question) · `R/reg-influence.R`'s header.

**Three parts, in this order, each gated before the next.**

**Part 1 — KEY 7: the AME is computed twice.** Measured: `effect = "marginal"` takes **15.32 s**
against 1.06 s for coefficients, and `Rprof` puts **85 % in `marginaleffects::get_jacobian`** — a
numerical derivative, one pass per coefficient. Directly against `marginaleffects`,
`avg_comparisons(vcov = FALSE)` is **7× faster with identical estimates**. And tabxplor **already
owns the exact analytic standard error for that quantity**: `reg_ame_if_maker()` is pinned to
`marginaleffects`' SE **to 10 decimals** by the package's own tests — it is simply only called in
the gap-test path today. *So tabxplor computes the AME's variance analytically for the colour and
then pays `marginaleffects` to compute it again, numerically, for the printed interval.*
- Take the SE from the influence function where it applies, and **declare where it applies as a
  `REG_ESTIMANDS` column** (`se = "analytic" | "numeric"`) rather than as an `if` — that is what
  makes this a key and not a patch.
- ⚠ **Be conservative.** `reg_ame_if_maker()` covers lm/glm/svyglm; `reg_ame_if_cat_maker()` covers
  multinom/polr; `effect = "at_reference"` profiles and `measure = "ratio"` marginals need checking
  one by one. Default the column to `"numeric"` and opt a row in **only** with a test that pins it.
- ⚠ This is *identical maths by a cheaper route* and must be **demonstrated**, not assumed.
- **This closes the old 20e** ("marginal effects for a logit regression is neverending"). It is
  neither a cache problem nor a jamovi problem.

**Part 2 — KEY 6: the staged build.** `reg_build()` is the largest function in the package (534
deparsed lines) with **seven local closures** against three in the entire 670-line factor leaf, and
it is eleven sequential phases with no names. `new_reg_ctx()` + named stages, mirroring
`tab_build()` one for one:

```r
reg_build <- function(ctx) {
  ctx <- reg_stage_split(ctx)      # the split_var/tab_vars recursion (or a no-op)
  ctx <- reg_stage_fit(ctx)        # fits + skeleton + reref      <- THE parallel seam
  ctx <- reg_stage_columns(ctx)    # the 3 per-spec builders
  ctx <- reg_stage_empirical(ctx)  # crude twins + obs + gap_se + tips + numeric overlay
  ctx <- reg_stage_footer(ctx)     # GOF + comparison + global + checks + curves
  reg_finalize(ctx)
}
```

`new_reg_ctx()`'s formals are the contract (`new_ctx()` / `new_reg_shared()` / `new_reg_args()`, the
idiom is now three times proven) and the `globalVariables()` mirror is **derived** from them.
⚠ The recursion stays at the **top**, exactly as `tab_build_tables()` does. ⚠ `.fit_cache` is an
environment and the ruling is *keep as is, do not improve* — thread it untouched;
`reg_reref_fit_res`'s byte-identity is a hard contract. **Pure refactor, no user-visible change** →
`dev/verify_reg_specs.R` must print IDENTICAL, which is exactly what that harness was built for.
It also gives the `empirical` subsystem a name: ~100 inline lines plus `reg_empirical()` (193) plus
`reg_empirical_columns()` (244) is the third-biggest subsystem in the package and is currently
spelled as an `if` block.

**Part 3 — the parallelisation verdict.** ⚠ **Re-measure first.** If part 1 takes the measured call
from 15.3 s to ~2 s, the case for a pool may evaporate — and **"do not parallelise" is a legitimate
and expected outcome** (Phase 9c reached exactly that verdict for scan fusion). Write the study in
a new `dev/*.md`, **pause**, then plan and implement only what the measurement justifies. The
brief's constraints are unchanged and all still binding:
- **candidate payloads**, each with very different granularity: per-predictor crude fits (z9's
  numeric `Obs_*`, z10's ordinal `Obs_cumOR` — measured at 2.5× the full model's own cost), per-fit
  (model comparison / several dependents / `tab_vars` groups), per-contrast, the `stats =
  "interaction"` pooled fits;
- **shipping cost is the known hazard, already measured**: ~10 MB per raw fit, ~41.5 MB serialized
  per jamovi round-trip (Phase o's freeze root-cause). A worker must return `reg_build_digest()`,
  **never a fit**;
- **`.fit_cache` is an env** and cannot cross a process boundary — decide how parallel and cached
  interact before writing any worker;
- **byte-identity and stable ORDER** (`vec_rbind` of split parts, `fit_first_idx`/`fit_ncol`);
- **jamovi viability is not assumed**: mirai's dispatcher needs sockets, which is why
  `test-parallel-parity.R` already fails under the bwrap sandbox. Confirm a pool works inside
  flatpak Electron *before* designing for it; if not, the feature is R-session-only;
- **reuse, do not duplicate**: `tab_pool_ensure()` / `tab_parallel_workers()` /
  `tab_parallel_stop()` and the `tab_pmap()` trampoline are the existing infrastructure. A second
  pool would be the ad hoc layer rule 1 forbids.
- If a threshold is the answer, it is a declared one (`tabxplor.reg_parallel_min`), and what was
  *not* parallelised is `log()`ged, not silent.

**Depends on**: 20c. **Verification**: `verify_reg_specs.R` IDENTICAL for part 2; a
tolerance-explicit fixture for part 1; `test-benchmark.R` plus a new reg operation for part 3.
**Full suite at the end of the phase.**

---

#### Phase 20e — KEY 8: `tab_style()` and the exporters' mirror

**Goal**: the render surface stops declaring the same seven arguments five times.

**Read first**: 19p §4.7 in full (including its two honest reasons for caution) · `resolve_export_opts()` (`R/tab-export-prep.R`) · §4's bundle test in this document.

**Contents**

- **The measurement**: `theme` · `color` · `color_legend` · `lang` · `transpose` · `caption` ·
  `var_names` = 7 arguments × 5 functions (`tab_html`, `tab_md`, `tab_xl`, `tab_plot`,
  `tab_export`) = **28 mirrored formals**, plus `wrap_rows` / `wrap_cols` / `whitespace_only` on
  three, plus 9 more on `tab_xl` alone (21 formals). One resolver already exists.
- **`tab_style()`** — a tier-2 bundle of the genuinely per-document fields
  (`var_names`, `var_labels`, `lang`, `test_lines`, `legend_style`, `tooltips`, `popover`, `css`,
  fonts, `wrap_*`), with `options(tabxplor.style)` as the session default — and **`theme`, `color`,
  `caption`, `transpose`, `path` staying flat**, because those are per-call decisions and the §4
  bundle test forbids moving them.
- ⚠ **All five exporters are CRAN-released**, so every moved formal needs a `...`-caught alias with
  a deprecation. This is the one phase where the deprecation shim count is large.
- ⚠ **Two signals to stay conservative**, both recorded in 19p: the maintainer declined the
  neighbouring option folds (`kable_popover` → `tooltips`, `legend_style` → `color_legend`), and
  the `tab_kable_*` renames stay dropped (○). `tab_style()` is a *bundle*, not a rename — keep it
  that way, and do not smuggle the declined renames in as fields.
- Estimated: `tab_xl` 21 → ~8 · `tab_md` 17 → ~9 · `tab_html` 14 → ~8 · `tab_plot` 9 → ~6 · `man/`
  ~−190 lines.

**Depends on**: 20a. Independent of 20b/20c/20d — but it shares KEY 1's *shape*, so read 20b's DONE
summary before planning it.

**Verification**: the export goldens and every `_snaps/*.md` must be **byte-identical** — this
phase moves arguments, never rendering. Sentinels: `test-tab_xl.R`, `test-render-html.R`,
`test-export-parity.R`, `test-md.R`.

---

#### Phase 20f — jamovi: the level-collapse UI, the boundary, and the rebuild

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

#### Phase 20g — Harvest: the deletion pass and open integration

**Goal**: reap it. Re-run the censuses, delete the shapes the new declarations made unnecessary,
and then think freely about what the finished surface makes possible.

This is 19l + 19m's model, and it is **two halves in one phase**.

**Half 1 — the deletion pass.**

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
    need a call-site-by-call-site read — 19l dropped one and the i18n tests caught it).

**Half 2 — open integration.** ⚠ *Creative; ask before building.*

Now that an argument is declared, a row and a column self-describe, a table states its kind, both
producers speak one vocabulary and the footer has a model — **what becomes possible that was not?**
The phase's job is to *look*, propose, and only then build. Prompts, not a specification:

- what can now be **generated** that is still hand-written (the jamovi `.a.yaml` argument blocks
  themselves? the `?tabxplor-options` page? a `tab_args()` runtime lister beside `reg_measures()`?);
- what can now be **asked** that could not be (a user asking "why is this cell that colour?" — the
  plan, the ladder, the break and the basis are all stored; a `tab_explain(x, row, col)`);
- what can now be **checked** (a single `tab_validate()` walking every declared table and every
  stored attribute of a built table — the runtime twin of KEY 2);
- where the two producers are **still asymmetric** for no reason left (the transpose; `spread`;
  `tab_compact()` on a regression; `tab_estimates()` on a crosstab);
- what a **third producer** would need (the honest test of whether the model is really uniform);
- and the standing question: is any declared table now derivable from another?

**Verification**: both proofs with **empty** declaration sets (`verify_golden_field_delta.R`,
`verify_color_attrs.R`) — a harvest that moves a value has stopped being a harvest. Plus the
**full suite** and **one `devtools::check()`**, so Phase 22 starts from a known-good tree.

---

## 10. The hand-over: what Phase 20 owes to Phase 22 and to the release

CLAUDE.md already carries **Phase 22 — documentation integration and simplification** (22a–22g) and
a release phase after it. The sequence is **Phase 20 → Phase 22 → release**. Nothing below is a new
phase; each row names the *existing* home of an item Phase 20 deliberately does not do.

### 10.1 To Phase 22

| item | goes to | note |
|---|---|---|
| **KEY 9 — `?tabxplor-model`** | **22b** | A doc-only page on the `?tabxplor-options` precedent: the four-carrier table (cell / column / row / table), the declared relations, and **the graph between them** — which is KEY 2's foreign keys, drawn. `@eval`-generated from the tables so it cannot drift. Today the largest single description of the model is CLAUDE.md's ~400-line repository map, which a user never sees |
| **one reader naming convention** | **22c** | `measure_facts()` · `est_var_kind()` · `ci_geom()` · `reg_estimand()` · `fmt_col_block()` · `tab_supports()` — six shapes for one idea. ⚠ Renaming readers is churn: **state** the convention, apply it to new readers, rename only where a name actively misleads |
| **the "Inspect a table" vignette section** (◆) | **22b** | `tab_shape()` · `tab_supports()` · `reg_measures()` · `tab_columns()` · `fmt_attr()` — five exports answering real user questions that appear in no vignette and no README. ~30 lines, and the right answer to "52 of 93 exports are untaught" for these five |
| **the taught-surface sweep for every 20b/20c rename** | **22b + 22d** | ⚠ After KEY 1 the `@param` blocks and value lists are **generated**, so a rename documents itself. What lags is free prose: `?tab` / `?tab_reg` `@details`, both vignette pairs, README |
| **the `family × effect × measure` table** | **22b** | already in the 22b brief; after 20c it can be **generated** from `REG_ESTIMANDS` rather than hand-written — `reg_measures_rd()` is most of it already |
| **`NEWS.md`** | **22e** | ⚠ Phase 20 is the biggest deprecation batch of the cycle (9 formals into `...`, the step API, five exporters' aliases). Each phase writes its `NEWS.md` lines **as it lands**; 22e then compresses the whole file. Do not defer the *writing*, only the *compression* |
| **the tests** | **22f** | ⚠ Phase 20 adds fixtures (rule 7) and 20a adds two harnesses. 22f's "full suite below 20 s" target is measured **after** Phase 20, not against today's tree |
| **`dev/`** | **22g** | this document and 19o/19p all become 2.0.0 archive material |

⚠ **One gap in the Phase 22 plan, flagged rather than filled**: **i18n appears nowhere in 22a–22g
nor in the release phase.** Every rename in 20b/20c and every new abort adds msgids, so
`po/R-fr.po` + the `.mo` recompile + `inst/po/en@quot` need a home — 19n did them as one pass and
the traps are recorded in its DONE summary (⚠ `inst/po/en@quot` is **derived**, step 5 of
`dev/update_translations.R`; ⚠ `po_update()` carries near-matches over as fuzzy and **some are
wrong** — rewrite each, never accept; ⚠ the extraction anchor in `R/reg-assumptions.R` is **not**
deletable, verify with `potools::get_message_data()` before touching any anchor). Recommend adding
it as **22h**, after 22c/22d have finished moving strings around.

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
  `CRAN-SUBMISSION`; and the jamovi rebuild + live pass if 20f left it open.
  ⚠ **`devtools::build_readme()` is the wrong tool** — it renders `github_document`, strips the
  YAML header and hard-wraps every paragraph (+1329 lines of churn). The committed `README.md` is
  `knitr::knit("README.Rmd", "README.md")` with the package **loaded** first.

### 10.3 Deliberately not done at all

| item | why |
|---|---|
| making the legacy step functions **defunct** | 2.1.0 — they are hard-deprecated in 20a, which removes nothing yet (§7.1 item 2) |
| a **JS syntax / lint gate** | no `node`, no `V8` on this box. Declined in 19n; the record was corrected twice (§7.1 item 8) |
| changing **`pct`'s default** | ruled: state it instead (★) |
| a **`tab_inference()` bundle** | rejected outright (★) — and the reason is the general test for every future bundle (§4) |
| the **`tab_kable_*` / `xl_font_*` option renames** | dropped (○, the 19m-iii ruling stands). **Delete the row rather than re-propose it a third time** |
| **column-axis `ordered`** | 19f deferred it with 19b's own admission test: a 17th attribute with no reader is not a fact, it is weight |
| **`tab_many()` hard-deprecation** | ★ stays soft — it was the documented main function through 1.3.1 |

---

## 11. What this plan is not

- **Not a statistics review.** Phase 19, 19o and 19p each found **no soundness problem**, and
  nothing here changes a number except KEY 7 — which is explicitly *the same maths by a cheaper
  route* and must be demonstrated as such.
- **Not a re-litigation of a settled ruling.** §4 lists twenty-seven; where this document disagrees
  with 19o or 19p it is on a **measurement** (§7.2), not on a decision.
- **Not a substitute for each phase's own plan.** Every entry above states *what* and *why*, and
  deliberately leaves *how* to the focused session that plans it. If a phase entry reads as a
  specification, it is over-written — trim it rather than follow it literally.
