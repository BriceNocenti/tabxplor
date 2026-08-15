# tabxplor — Phase 19: ecosystem integration, round 2

**The plan of plans for the last development stretch of v2.0.0** — goals, design and architecture
decisions first, then the phased roadmap.

Written 2026-08-13 from `dev/ecosystem_keys_2.md` (the study: seven parallel audits + three
measurement passes + the maintainer's rulings). That study is the **evidence**; this document is the
**plan**. Where the two disagree, this one wins — it carries decisions the study left open.

**Companion documents**, read the section that matches what you touch:

| document                                    | what it holds                                                               |
|---------------------------------------------|-----------------------------------------------------------------------------|
| `dev/ecosystem_keys_2.md`                   | the measurements, the eight keys in full, the defect ledger, the appendices |
| `dev/tabxplor_ecosystem_simplification.md`  | round 1 (Phase 17) — the disease patterns this phase inherits               |
| `dev/tabxplor_2.0.0_decisions.md`           | the architecture decisions of the whole 2.0.0 cycle                         |
| `dev/tabxplor_architecture.md`              | the **current** architecture — update it as phases land                     |
| `dev/tabxplor_2.0.0_roadmap_DONE_PHASES.md` | every landed phase's DONE record                                            |

⚠ Every `file:line` in the study and here is an anchor from **2026-08-13**. `tab.R`, `tab_reg.R`,
`fmt_class.R` drift by ±20 lines per phase. **Re-grep before editing.**

---

## 1. Why Phase 19 exists — the mission

Phase 17 (round 1) cured five disease patterns and left the package measurably better. Since then
**+8 000 lines** landed — survey designs, the gap tests, crude twins, model checks, the print palette,
the French layer, the forest plot — and the *shape* of the remaining complexity moved. Round 2 asks a
single question, and the eight answers are this phase's content:

> **What are the missing keys — the small number of stored facts or stated rules that would each
> collapse many scattered special cases at once?**

Phase 19 is **not a feature phase**. It exists to make the package's own model explicit, so that:

- a **row** describes itself, the way a **column** already does;
- a **column** says what it estimates, instead of six switches re-deriving it;
- a **measure** declares what it needs, instead of four allow-lists that disagree;
- an **argument** is a choice, not a consequence with a message attached;
- a **table** says what kind of table it is, once, for both producers;
- the **two producers share one vocabulary** end to end — the argument that asks, the attribute that
  stores, the legend that names and the plot axis that draws all use the same words.

### The hard rules (they override convenience, every phase)

1. **Simplify and integrate — never add another ad hoc layer.** Extend the shared model or the fact
   table; never bolt a special case onto a call site. Delete the traces of the old implementation in
   the same phase — no commented-out corpses, no "kept just in case" branch.
2. **Never guess what something is.** No behaviour may depend on matching a rendered English label, a
   name prefix, a positional vector, or a magic field value. If the fact is not stored yet, **storing
   it is the task**.
3. **One resolver, one model, taken to completion.** A setting resolves ONCE and is consumed
   everywhere. Re-deriving downstream is the disease, not the cure.
4. **Facts live in ONE table.** Two encodings of one rule "kept in sync by comment" is forbidden —
   derive both consumers from one source.
5. **Never leave a representation half-migrated.** The value of KEY 1 is entirely in *deleting* the
   four label-block shapes. A fifth representation added beside them is worse than doing nothing.
   The same applies to every key: if the migration cannot be finished in the phase, split the phase,
   do not split the migration.
6. **Internals and outputs are redesigned as radically as needed.** `tab_reg()`'s back-compat is
   waived entirely (user API included). `tab()`'s CRAN-released surface gets soft-deprecation shims,
   never silent breakage.
7. **A claimed fix ships with the fixture that fails without it.** Assert non-zero counts; never let a
   test pass vacuously.
8. **Golden discipline.** Each phase declares which goldens are allowed to move and proves the delta
   is exactly what it claims (`dev/verify_golden_field_delta.R`, which learns one new mode per new
   kind of delta).
9. **End-of-phase documentation discipline**: see CLAUDE.md "## The last step of every implementation, during the final test suite") ; The phase **"DONE" summary goes in CLAUDE.md and ONLY there**.

### What must survive, unchanged in spirit

The five differentiators the internals exist to serve (study §1):

1. per-cell metadata, so display is switchable losslessly and dplyr verbs keep it;
2. colour that reads significance (the three `color_signif` policies);
3. crude-vs-model comparison (`empirical = TRUE`, `color = "adjustment"`);
4. the jamovi teaching path — R argument names visible in the UI **on purpose**;
5. tibble/dplyr citizenship.

Differentiator 1 is the one most at risk in this phase: it *means* that every geometry is present in
every cell and the user selects afterwards. **No phase may make the user choose a geometry at build
time.** (This is exactly why the pass-2 `compare` argument was demolished — study §KEY 8.)

---

## 2. The measured starting state

| fact                                | value                                                                           |
|-------------------------------------|---------------------------------------------------------------------------------|
| R source                            | 38 784 lines, 19 853 code, **41 % comment**                                     |
| top-level functions                 | 900, median 17 lines                                                            |
| the two biggest functions           | `reg_build` 1 307 L · `tab_reg` 763 L (**484 code**)                            |
| user messages                       | **163**, of which **72 % in the two argument boundaries**                       |
| `tab()` formals                     | **51** (34 genuine settings); `tab_reg()` **30**                                |
| the taught hot surface              | **~12 arguments**                                                               |
| exported functions never taught     | **48 of 84**                                                                    |
| global options                      | 42                                                                              |
| fmt record                          | **21 fields**, **14 column attributes**, 3 table attributes                     |
| `ctx`                               | 53 declared fields, **~83 live**                                                |
| carrier robustness (15 dplyr verbs) | field **15/15** · column attr **14/15** · `meta` **13/15** · bare attr **7/15** |

**The single most diagnostic number**: nearly three quarters of everything the package says to a user
is said while *negotiating arguments*, before any computation. That is the shape of the disease, and
KEY 3 / KEY 8 are its cure.

---

## 3. The eight keys, in one page

Each key is *a fact the code needs but does not store, or a rule it applies but does not state*.

| key       | the missing fact                                             | what it stores / states                                                                          | lands in |
|-----------|--------------------------------------------------------------|--------------------------------------------------------------------------------------------------|----------|
| **KEY 1** | *what a row is*                                              | a typed label column (role/var/ordered) + a `row_kind` field                                     | 19f      |
| **KEY 2** | *which field holds the estimate, on which scale*             | column attributes `scale` + `pct_base` + `ci_method`; `ci_type` deleted                          | 19b      |
| **KEY 3** | *the derivation graph between arguments*                     | the graph as data — (a) reg argument collapse, (b) the forcings in MEASURES                      | 19c, 19e |
| **KEY 4** | *what a colour measure requires and is called*               | MEASURES gains `requires`/`channels`/`auto_for`/`method`/`subject`/`word`                        | 19c      |
| **KEY 5** | *2.0.0's own keystone — one aggregate core*                  | CI + test computed in the leaf, from the plan                                                    | 19j      |
| **KEY 6** | *what kind of table this is, and which variables it has*     | one `meta$spec` with `kind` + a uniform variable model                                           | 19g      |
| **KEY 7** | *what `tab()` returns*                                       | one entry point, a predictable class, one capability predicate                                   | 19h      |
| **KEY 8** | *where the comparison is named* — and it differs by producer | `tab()`: `color` names it (a selection) · `tab_reg()`: `measure` names it (a modelling decision) | 19d, 19e |

**KEY 8's principled divergence is the intellectual core of this phase** and must not be
re-collapsed by a later session: on a crosstab every geometry is a function of the same sufficient
statistics, so asking for one is a **selection** over facts already computed; on a regression a
geometry is a *different fit or estimator*, so it is a **modelling decision** and must live in an
argument. *Changing `display` must never change the model.*

---

## 4. Settled decisions — do not re-open

Maintainer rulings from `dev/ecosystem_keys_2.md` §10 plus the seven taken on 2026-08-13 when this
plan was written (marked ★).

| decision                                                                       | ruling                                                                                                                                         |
|--------------------------------------------------------------------------------|------------------------------------------------------------------------------------------------------------------------------------------------|
| ★ **KEY 1 carrier**                                                            | **Option C** — a typed *factor subclass* label column carrying column attributes. Not A (a 4th table attribute), not B (a record with fields). |
| ★ **KEY 1 naming**                                                             | friendly single-variable names **stay** (`tab$marital`) — C decouples naming from robustness, so nothing is traded                             |
| ★ **`ordered`**                                                                | stored **per variable in the declared column attributes, both axes**; the merged `levels` column stays a plain factor                          |
| ★ **KEY 2 naming**                                                             | **`scale` + `pct_base`**; `ci_type` **deleted**; `get_type()` / `get_ci_type()` survive as **derived, soft-deprecated** accessors              |
| ★ **`ci` anchor values**                                                       | **`ci = c("auto", "no", "cell", "ref")`** — `"ref"`, not `"comparison"` (which reads as a sibling of the unrelated `comp =`)                   |
| ★ **`spread`**                                                                 | one implementation, `tab_spread()` keeps its name and absorbs `reg_spread_models()`; **one argument name on both producers**                   |
| ★ **KEY 5**                                                                    | **in Phase 19**, sequenced late, after KEY 1, gated on the jamovi cold+warm+reref lock                                                         |
| ★ **release**                                                                  | **all of Phase 19 lands before the 2.0.0 CRAN release** — one set of shims, introduced once                                                    |
| KEY 7 entry points                                                             | (b): `tab_many()` becomes a one-line deprecated shim;                                                                                          |
| `.fit_cache` / reref                                                           | (a) keep as is — 450 lines and the 11-conjunct predicate stay; do not "improve" it in this phase                                               |
| jamovi boundary                                                                | (b) a shared resolver both boundaries call + a **generated** table for the JS eligibility rules                                                |
| `exponentiate`                                                                 | **deleted** → `measure = "log"`, old name kept as a documented synonym                                                                         |
| `at`                                                                           | **folded** into `effect = "at_reference"`                                                                                                      |
| `estimate_display`                                                             | **replaced** by a real `display =` on `tab_reg()`, the four values kept as documented shorthands                                               |
| `tab(OR =)`                                                                    | **deleted** (soft-deprecated); the `or` field becomes **unconditional** on `type ∈ {row, col}` columns; `ref2` alone picks the dichotomisation |
| `cumOR`                                                                        | `ref2 = "cumulative"`                                                                                                                          |
| `color` canonical values                                                       | migrate to the **full words**, short ones kept as aliases both ways, the full word taught                                                      |
| a mismatched `{ci}` bracket                                                    | **refused**, never converted — a printed interval must be *the* interval                                                                       |
| a `display` token whose field is empty                                         | renders **void**, plus a one-time note naming the argument that would fill it                                                                  |
| `ci = "cell"` beside `stars` / `color_signif`                                  | **inform and disable**, from ONE rule (today: one aborts, one is silent)                                                                       |
| `color` alone triggering the comparison interval                               | **no** — measured +38 % on a build; the trigger stays `color_signif` / `stars` / explicit `ci = "ref"`                                         |
| capability gaps                                                                | **closed** — gaussian ratio-of-means, identity-link risk difference; the legality table becomes three-state and ships as a runtime object      |
| `filter`                                                                       | **keep** on `tab()` (the tribble + `pmap` workflow); **remove from the jamovi UI**                                                             |
| `tab_totcol_range()`                                                           | keep, dormant                                                                                                                                  |
| `tab_get_wrapped_dimensions`                                                   | keep (personal tooling)                                                                                                                        |
| `method = "profile"`, `quasipoisson`, the compound-formula hatch, `mnl_vsrest` | keep — previously settled, not re-opened                                                                                                       |

---

## 5. The target architecture — the global image after Phase 19

### 5.1 The metadata model

```
                        stored, per CELL          stored, per COLUMN            stored, per TABLE
                        ----------------          ------------------            -----------------
what it is              row_kind                  scale, pct_base, role,        meta$spec$kind
                                                  model_family, ordered
what it measures        n, pct, mean, diff,       scale (one key into           —
                        ratio, or, ctr, obs       the declared library)
how it was computed     n_eff, gap_se             conf_level, degf, basis,      —
                                                  ci_method
where it sits           in_tottab, in_refrow      col_var, totcol, refcol,      meta$spec$vars
                                                  comp_all, ref
```

**21 fields** (`in_totrow` → `row_kind`, one replaced not added) · **16 column attributes**
(`type` → `scale` + `pct_base`, `ci_type` deleted, `ci_method` + `ordered` added) · **3 table
attributes** (`subtext`, `test`, `meta`), with `meta` holding one `spec`.

Two rules govern every future addition:

- **one key plus a declared library, never one attribute per fact.** The admission test for a new
  attribute: *does it name a fact no other attribute can derive, and does a reader exist?*
- **a fact goes on the most robust carrier that can hold it** — field (15/15) > column attribute
  (14/15) > `meta` (13/15). Table-level facts have been silently dropped by a rebuild site **five
  times in three phases**; that count, not any single incident, is the rule's justification.

### 5.2 The one vocabulary

The words are the same at every layer. This is what "integration" means concretely:

```
   geometry        tab()                    tab_reg()              stored scale row          plot axis
   --------        -----                    ---------              ----------------          ---------
   difference      color = "difference"     measure = "difference" points | raw_diff |       linear
                                                                   mean_diff
   ratio           color = "ratio"          measure = "ratio"      pct_ratio | mean_ratio    log
   odds_ratio      color = "odds_ratio"     measure = "odds_ratio" odds_ratio                log
   log             —                        measure = "log"        log_coef                  linear
   level           (the cell itself)        —                      level_pct | level_mean |  —
                                                                   level_n
   contrib         color = "contrib"        —                      —                         —
   adjustment      —                        color = "adjustment"   —                         —
   between_groups  —                        color = "between_groups" —                       —
```

> **the argument names the `geometry`; the attribute names the `row`.**

⚠ Two words at two grains is the `type` / `ci_type` collision this phase exists to end. The
attribute is **`scale`** (the row); the argument is **`measure`** (the geometry). Never swap them.
And the colour **measure** (`MEASURES`) is genuinely separate from the estimate **geometry** — a
column can print a percentage and be coloured by its ratio; keeping them apart is what lets
`color = c("odds_ratio", "adjustment")` mean something.

### 5.3 The resolution spine

Arguments resolve ONCE, at the boundary, into stored facts:

```
user arguments  →  the settings spine (one row per row_var × col_var)  →  the leaf
                        ↑                                                     ↓
              MEASURES$requires declares what a measure forces        stamped column attributes
              SCALES declares what a geometry resolves to             stamped row kinds
```

`tab_resolve_common_args()` is the one place `tab()`, `tab_many()`, `tab_num()`, `tab_counts()` and
jamovi resolve the shared arguments. Nothing downstream re-derives, re-recycles or re-validates.

### 5.4 The render path

Rows and columns both self-describing means the render model stops guessing: `tab_render_vars()` reads
declarations, `collapse_totals` compares keys not rendered strings, the transpose flips a declared
index instead of hand-copying 39 slots, and every consumer outside the render path (`tab_estimates()`,
`forest_plot()`, and whatever comes next) gets real roles instead of an English-label fallback.

### 5.5 jamovi

The kernel is already right (one store, one LRU, two configs, no forked statistics). What changes is
the **boundary**: the seven hand-mirrored rules collapse onto the shared resolver, and the three JS
rules are **generated from R** rather than hand-written in a language with no test harness here.

---

## 6. Anti-propositions — what NOT to do

Restated from the study §7, all still binding:

- **Do not route regression columns through the aggregate core.** What is genuinely shared (the nine
  `ci_*` engines, the record, the colour engine, the legend, the footer, the exporters) **is already
  shared**. What is not shared is table assembly, and that is correct — a fit has no count aggregate.
- **Do not go sparse on the record** (z6 measured it; +1 field changes nothing).
- **Do not merge fmt fields.** `diff`/`or`/`ratio` are a discriminated union — store the *tag*
  (KEY 2's `scale`), keep the fields. Re-open only past ~30 fields.
- **Do not replace the S3-per-verb model**, and do not force `pillar_shaft` through the render model.
- **Do not re-open the settled perf verdicts** (scan fusion, chi2 marshalling, the `.fine` seam).
- **Do not "fix" the four label-block shapes by adding a fifth** (rule 5).
- **Do not delete `tab_ci()` / `tab_chi2()` as exported functions** — supersede them, move the
  computation.
- **Do not move the jamovi JS rules into R.** They exist for latency, which is a real requirement.
  Generate them from R instead.
- **Do not make `display` change what is computed** on `tab_reg()` — and, after KEY 8a, do not make
  it change what is computed on `tab()` either. The odds ratio becoming unconditional is what removes
  the temptation.

---

## 7. Caveats, risks, and the honest gaps

**Named here so no phase discovers them late.**

1. **The always-on odds ratio rests on one measurement** — 300 ms vs 340 ms, 5 replicates, one
   3-`row_var` × 2-`col_var` table. It is structurally plausible (the 2×2 is four numbers the wide
   table already holds, computed in the sweep that already produces `diff` and `ratio`), but Phase 19d
   must **re-measure on a wide table and on the big fixture before committing**, and back out to
   "computed when asked" if it is not free at scale. `ref2 = "last"` (D27) must be fixed *first*, or
   the warning fires on tables that never asked for an odds ratio.
2. **`ci = "cell"` + `color_signif` is a documented abort today.** Ruling: inform-and-disable. That is
   a deliberate *loosening* of a released behaviour — announce it in `NEWS.md`, and make sure the one
   rule really is one rule (today `color_signif` aborts and `stars` is silent).
3. **`set_type()` is exported.** `get_type()` becomes derived, which is easy; the *setter* needs a
   decision at plan time: it is invertible (`"row"` → `scale = level_pct`, `pct_base = "row"`), so it
   can stay as a soft-deprecated setter that writes both attributes. `set_ci_type()` has zero users
   anywhere and is simply cut (19a).
4. **Option C makes `class(tab$marital)` = `c("tabxplor_lvl", "factor")`.** `is.factor()` stays TRUE
   and forcats keeps working, but user code testing `identical(class(x), "factor")` sees the change,
   and `str()`/`dput()` output moves. Documented, not avoidable.
5. **KEY 1 is atomic or it is harmful** (rule 5). Its consumers — the leaves' tails, compact, spread,
   transpose, the export prep's label runs, `tab_reg`'s assembler — migrate in the same phase. The
   declared seam (19f-i / 19f-ii) splits *sessions*, never the migration.
6. ✅ **RESOLVED 2026-08-13 — the generated `.h.R` was stale and is now regenerated.** It had been
   *shipping inert controls*, which is what made D9 and D10 user-visible. A `jmvtools::prepare()`
   ran (inside `jmvtools::install()` while provisioning the laptop dev box) and, measured against
   HEAD: `design_effect` **0 → 11** occurrences in `jmvtab.h.R` (**D9** — declared in the YAML,
   absent from the generated file, so `isTRUE(NULL)` made the checkbox do nothing), and every dead
   option went to zero (**D10** — `test_robust` 10→0, `method_ratio` 10→0, `na = "drop_all_models"`
   1→0, `ids` 13→0, `strata` 13→0, `fpc` 12→0). So **19a inherits a clean generated layer** and only
   **19k** still needs a `prepare()` + rebuild. The standing rule survives the fix: **any phase that
   edits a `.a.yaml`/`.u.yaml` leaves it inert until the next `prepare()`** — say so in the DONE
   summary instead of claiming the UI changed. ⚠ Never hand-edit a `.h.R` to "keep it in sync";
   the last time that was done the compiler found a latent bug in the mirror.
7. **The jamovi cache is the tripwire for KEY 2, KEY 5 and KEY 8a.** All three change what a carrier
   stores. `JMVTAB_CACHE_SCHEMA` (currently **12**) bumps in each; the cold+warm+reref lock is the
   gate. `reg_reref_fit_res`'s byte-identity is a hard contract — do not touch it.
8. **i18n rides every legend change.** `po/R-fr.po` + the `.mo` recompile, and the
   `if (FALSE) gettext(...)` extraction anchor is itself a hand-maintained duplicate — KEY 4 should
   **generate** it from the measure table rather than shadow it.
9. **The locale trap in the doc build** (recorded at Phase 18z2): `tabxplor.lang` defaults to the
   ambient locale, so knitting English documents on this `fr_FR.UTF-8` box silently produces French
   legends unless both `options(tabxplor.lang=)` and `LANGUAGE` are pinned. Every document already
   carries its pin — do not remove them.
10. **One suspected live bug the study surfaced but did not confirm**: `settings$cols$lvs` is stale
    the moment `tab_prepare_pop()` re-resolves levels, *and it is the stale copy that is shipped to
    every parallel worker* (study §4.2.2). **Verify in 19a**; if live, fix it there rather than in
    19i, because parallel results diverging from serial ones is a correctness issue, not tidiness.

---

## 8. Verification discipline

Deliberately light. A fresh session judges this well; these are floors, not ceilings.

- **Per phase, the default is targeted**: run the test files your change touches
  (`devtools::test(filter = "…")`) plus the sentinel named in the phase entry. **Do not run the full
  suite after every edit** — it costs ~56 s of wall clock and much more of attention.
- **Full suite** (`OMP_NUM_THREADS=1`, `TESTTHAT_CPUS=8`, temp runner outside `tests/`, exactly the
  CLAUDE.md § Testing recipe) at four checkpoints: **end of 19d** (the first user-visible batch),
  **end of 19f** (the structural one), **end of 19j** (KEY 5), and **19n**.
- **The CI-locale run** (`LC_ALL=C.UTF-8 LANGUAGE=en`) **once, in 19n.** Not before — it is a
  release check, and running it per phase is the over-verification this plan is trying to avoid.
- **`devtools::check()`** once, in 19n.
- **Goldens**: each phase entry names which families may move and why. Prove the delta with
  `dev/verify_golden_field_delta.R`, teaching it one new mode per new kind of delta (it already knows
  field / attribute / `test`-column / `meta` sub-field; this phase adds *populated field*,
  *row-index block* and *attribute rename*).
- **Byte-identity phases** (19a, 19c, parts of 19b) tolerate **zero** golden churn — investigate any
  diff rather than accepting it.

---

## 9. The roadmap

Fourteen phases. Each is *plan-then-implement*, starting in plan mode, with its own fresh session.
The maintainer commits between phases and pushes at the end.

**The order above is the recommended one; what is *binding* is the dependency list.** A session may
re-order two phases that do not depend on each other (19c and 19e, for instance), but never one that
does.

| phase                              | must land after                             | because                                                                    |
|------------------------------------|---------------------------------------------|----------------------------------------------------------------------------|
| **19a** floor / E1                 | —                                           | it is the prerequisite for everything that adds or reconciles an attribute |
| **19b** KEY 2                      | 19a                                         | needs generic attribute carry                                              |
| **19c** KEY 4                      | 19a                                         | needs generic attribute carry                                              |
| **19d** KEY 8a — `tab()`           | 19b, 19c, **and D27 from 19a**              | the stored scale closes D21/D23; `requires` is how `color` declares        |
| **19e** KEY 8b — `tab_reg()`       | 19b, **and the family predicates from 19a** | `measure` resolves into a stored scale row                                 |
| **19f** KEY 1 — the row model      | 19a                                         | independent of the argument work; the largest structural item              |
| **19g** KEY 6 — table identity     | 19e, 19f                                    | `spec$vars` must not be built before 19f makes half of it derived          |
| **19h** KEY 7 + the export stack   | 19f                                         | the return shape and the render model both read the declared row index     |
| **19i** pipeline + `tab_counts`    | 19f                                         | the leaves' tails move in 19f                                              |
| **19j** KEY 5 — one aggregate core | 19f, 19i                                    | the leaf needs the row identity and the finished spine                     |
| **19k** jamovi                     | 19d, 19e, 19i                               | it consolidates onto the resolver and carries both new vocabularies        |
| **19l / 19m** harvest              | everything structural                       | they measure and exploit the finished model                                |
| **19n** docs / i18n / release      | last                                        | one `.mo` recompile, one CI-locale run, one `check()`                      |

---

#### Phase 19a — The floor: enabling moves, dead weight, and the cheap defects

**Goal**: a clean floor. Make "add a column attribute" a two-line change, close the defects that need
no redesign, and delete everything with zero readers — so the design phases work on solid ground.

**Read first**: study §4.1.1 (E1), §4.2, §5 "cut" rows, §11 (the defect ledger), §2.6 (why D16 matters).

**Contents**

- **E1 — make attribute carry generic.** The four reconstructors (`vec_cast`, `vec_ptype2`,
  `vec_arith`, `vec_math`) enumerate 14 attributes by hand, and the two leaves pass 9 of 14. Drive
  them from `fmt_col_attrs` plus a small **declared reconcile rule per attribute**
  (`same-or-neutral` / `weakest` / `min`), exactly as `meta_bind_rules` already does for the table
  `meta`. Byte-identical. **This is the prerequisite for 19b, 19c and 19g** — do it first.
- **D16** — `bind_rows()` on two *grouped* tabs silently drops `subtext`, `test` and the whole `meta`;
  `vec_rbind()` returns a bare `grouped_df`. A contributing cause is plainly wrong on its own terms:
  `dplyr_reconstruct.tabxplor_grouped_tab` restores from `data`, not from `template`, contrary to
  dplyr's contract. Fifth instance of the "a rebuild site drops table-level facts" class.
- **D27** — `ref2 = "last"` does not resolve. **Prerequisite for 19d.**
- **Verify §7.10** (the stale `settings$cols$lvs` shipped to workers). Fix here if live.
- **The dead-weight cuts** (§5, "cut" rows): `tab_assemble()`, `ctx$levels_order` out of the ctx,
  `set_tot_n` / `set_n_eff` / `set_model_family` / `get_ref_means` / `get_ref_pct`,
  `complete_partial_totals`, `set_ci_type`, `reg_meta$shape` / `$model_labels`,
  `plain_resolve`'s dead `tot` forcing block (6 unreachable `warning()`s).
- **The free single-sourcing**: `resolve_cleannames()` beside `resolve_stars()`/`force_comp()` (the
  rule is written 4×); the `conf_level = getOption(...)` formal default (6×); one `fmt_base(x)`
  accessor for the `n_eff → tot_n → n` coalesce (5 sites); `inference` becomes a **required**
  argument on `plain_core`/`num_core`/`tab_apply_tests` instead of a lazy default that silently
  re-reads the option; `meta` passed explicitly in the step tails.
- **Three family predicates** — `reg_fam_glm()`, `reg_fam_overdispersed()`, `reg_fam_disp_known()` —
  absorbing 14 of the 21 hard-coded family whitelists. *Prerequisite for 19e.*
- **Cheap defects**: D5 (a message naming a value removed in z13), D7 (an unreachable NULL guard),
  D14 (`@param` for an argument that does not exist), D15 (a comment saying the opposite of the code),
  D18 (dead arms in the live `has_ci` predicate — document `"cell"`'s deliberate absence while there).

**Maintainer step**: none — ✅ the `prepare()` this phase used to need already ran (2026-08-13), so
**D9 and D10 are closed before 19a starts** (§7.6). Do not re-open them; verify with
`grep -c design_effect R/jmvtab.h.R` (expect 11, not 0).

**Verification**: targeted + the fixtures for D16/D27. **Zero golden churn** — everything here is
byte-identical except the two defect fixtures.


---

#### Phase 19b — KEY 2: what a column estimates

**Goal**: store the estimate's identity, so six switches, seven derived predicates, one
order-dependent dispatch and a whole vocabulary can be deleted.

**Read first**: study §KEY 2 in full (especially (a) the two disagreeing rules, (b) why `ci_type` is
deleted rather than renamed, and the library table), §6, §11 D8/D17/D18/D19. `EST_SCALES`
(`fmt_class.R:3307`) — **this key is finishing z17's phase, not opening one.**

**Contents**

- **The library becomes the stored fact.** `EST_SCALES` is already the declared library with the right
  rows; what is missing is that its key is *recomputed by a dispatch* instead of being *stored*. Add
  the `level_n` row it lacks (`type = "n"` currently borrows `level_pct`, whose `est_field` is `pct` —
  the code documents the fudge), and fold `raw_diff`/`mean_diff` into one row with `sd_from` as a
  field, since they differ only in where the SD comes from.
- **Three attributes in, one vocabulary out**: `scale` (one key into the library) + `pct_base`
  (`type`'s other half) + `ci_method` (a per-column fact stored table-wide today). **`ci_type` is
  deleted** — the stored interval is always on the estimate's own scale, and "is there an interval
  here" is a data fact (`!all(is.na(ci_inf))`), not a second vocabulary.
- **`get_type()` and `get_ci_type()` become derived, soft-deprecated accessors** returning all their
  old values unchanged — the only variant with no silent breakage of a released, vignette-taught
  surface. `set_type()` needs the plan-time decision in §7.3.
- **What gets deleted by construction**: `fmt_est_field`'s six copies; `ci_center()`'s
  `ci_type`-then-`type` fallback chain; `fmt_color_plan`'s `is_mean` / `is_std_diff` / `is_logcoef` /
  `ci_mult` / `has_ci` / `ci_neutral` / `sd_ref`; the legend's private re-derivations of four of them
  (one of which computes `is_std` from a *different* scale than the plan uses — a latent divergence
  that happens to agree today); **`fmt_gap_scale_key()`'s order-dependent `var` sniffing**, and with
  it the comment warning that branch order is the contract; the `gof` special case, which becomes a
  declared `geometry = "none"` (uncoloured by declaration, not by a rule that exists to undo a
  storage decision); and the `log_odds` swap's literal `measure == "diff"` test.
- **`meta$ci_settings` is emptied** (`conf_level` is already per column; `ci_method` joins it) and
  D8's silent fall-through — which can print a CI method the bounds were never built with — becomes
  impossible.
- **`ordered`** joins the column attributes on the col_var axis here (§4 ★), since this is the phase
  that touches every stamping site. The row axis gets its half in 19f.
- D17, D18, D19 close by construction.

**Depends on**: 19a (E1). **Unblocks**: 19d, 19e, and the forest plot's axis correctness.

**Verification**: the structural goldens **will** move (an attribute rename plus one added) — prove
the delta is exactly that, and that no rendered output moves. `test-fmt-contract.R`'s record-shape
snapshot is a conscious regen. Sentinels: `test-tab-estimates.R`, `test-forest-plot.R`,
`test-color-legend.R`, the jamovi cold+warm+reref lock (schema bump).



---

#### Phase 19c — KEY 4: what a measure declares it needs

**Goal**: move the colour vocabulary out of the code and into the table, so that adding a measure is
genuinely one row — which the `/color-mode` skill already (wrongly) claims.

**Read first**: study §KEY 4 in full, §KEY 3(b), §11 D4. The real checklist today is **10 mandatory
edit sites across 5 files, rising to ~30 across 8** for a comparison measure.

**Contents**

- **MEASURES gains its vocabulary**: `requires` (which build steps this measure forces — `ci`, `ref`,
  `totrow`, `chi2`, `empirical`), `channels` (eligibility — **one** list replacing four, two of which
  currently disagree: `color = c("OR","adjustment")` is legal in `tab_reg()` and illegal in `tab()`,
  D4), `auto_for` (the `color = TRUE` / `"auto"` defaults), `method` (the CI method the legend names),
  `subject` (the legend's noun), and `word` — from which the i18n extraction anchor is **generated**
  rather than shadowed by a hand-maintained duplicate.
- **The colour break scale table gains `center` / `strict` / `std` as columns** instead of three
  name-keyed lists.
- **Two fossils die.** (i) The 4-way split `color_diff_OR` / `color_ctr` / `color_ci` / `color_num`
  exists because the pre-2.0.0 pipeline had four steps; three of them are now one aggregate core and
  the fourth is KEY 5. (ii) The resolver still *speaks the legacy vocabulary internally* — 17d decoded
  `diff_ci`/`after_ci`/`ci` once at the boundary, and the cascade immediately re-encodes the decoded
  pair back into a legacy string.
- **`color = TRUE` stops being resolved twice with the first answer thrown away.** Stage 1 survives
  today only for its *side effects* (forcing `chi2`, `totrow`, `ci`) — which is exactly what
  `requires` declares. `color_signif` and `color_ratio_ci` stop being threaded *beside* the colour
  string because the legacy encoding cannot carry them.
- **The 18 spellings of `color` shrink** to the documented ones plus aliases (`"ci"` is a pure
  synonym of `"after_ci"` — cut). `names(MEASURES)` becomes the allow-list, the jamovi list and the
  `/color-mode` checklist, all three.
- Update `.claude/skills/color-mode/SKILL.md` so its claim becomes true.

**Depends on**: 19a. **Unblocks**: 19d (the `requires` mechanism is how `color` and `display` declare).

**Verification**: byte-identical target — the plan and the legend are golden-locked. Sentinels:
`test-color-legend.R`, `test-color-config.R`, the `_color_golden/*.rds` family.



---

#### Phase 19d — KEY 8a: the `tab()` comparison surface

**Goal**: every `tab()` argument becomes a meaningful choice. Nothing on the surface says *what to
compute* any more — only what to compare against, what to test, and what to show.

**Read first**: study §KEY 8.2 – §8.7 and §8.16 (the rulings), §11 D20–D23, D26, D27, D28.

**Handed over by 19c** (found while removing the 4-way split; both are `ci`'s surface, i.e. this
phase's):

- **D29 — the direct `tab_num()` path never forces the difference CI a policy gates on.** 14a fixed
  that inside `tab_resolve_settings()` only, so `tab_num(color = "diff", color_signif =
  "grey_non_signif")` (no explicit `ci`) computes no interval and the policy greys every cell.
  19c declared the rule (`requires = c(ci = "gated")`) and applies it in the resolver; applying it on
  the leaf path too is a behaviour change, so it was left for this phase. One line at
  `num_resolve()`, plus a fixture.
- **`color = "auto"` vs `color = TRUE` are still not synonyms on a factor table** — the string gives
  ONE channel (`diff`), the logical gives two (`diff` + `ratio` bg), because only the logical takes
  `mode = "auto"` in `normalize_color_spec()`. 19c made them agree wherever a `color_signif` policy is
  set (the combination used to abort outright); making them agree unconditionally moves goldens, and
  belongs with this phase's migration of `color`'s canonical values.

**The resulting surface** (seven arguments, each a genuine question):

```r
pct           what is in the cell                       n | row% | col% | all% | mean
ref, ref2,
comp          what it is compared to                    ref2 also carries "cumulative"
color         WHICH comparison, and how it is coloured  difference | ratio | odds_ratio | contrib | …
ci            WHERE the interval sits                   auto | no | cell | ref
stars         do I want stars                           logical
color_signif  how significance changes the colour       ignore | grey_non_signif | guaranteed_effect
display       what is printed                           the {} grammar
```

**Contents**

- **The odds ratio becomes unconditional** on `type ∈ {row, col}` columns, joining `diff` and `ratio`
  as a third always-present comparison. ⚠ **Gate this on the re-measurement in §7.1 and on D27 being
  fixed.** Three declared consequences: the html tooltip gains an `OR:` line on every percentage cell
  (visible, moves `_snaps/render-html.md`); the structural goldens gain a populated field; and `ref2`
  becomes always-in-force.
- **`OR` is soft-deprecated** with the six mechanical routes (`"OR"` → `display = "{or}"`,
  `"OR_pct"` → `display = "{or} ({pct})"`, `"cumOR"` → `ref2 = "cumulative"`). It was `color` +
  `display` + `ref2` welded, and the weld is where D20 and D21 live.
- **`ci` keeps its *anchor* question and loses its geometry**: `c("auto", "no", "cell", "ref")`.
  `ci = "cell"` **does not move at all** — same name, same value, same behaviour, no deprecation.
  `"diff"`/`"ratio"` soft-deprecate onto `"ref"`; *which* comparison comes from `color`, which
  already decides it correctly for all three geometries.
- **`tab_num(ci_scale =)` is cut** — a pure duplicate of `ci = "ratio"`, used 0 times anywhere.
- **`color`'s canonical values migrate to the full words**, short ones kept as aliases both ways.
- **One rule closes the whole D21/D23 class**: *the stored interval is the one the table's comparison
  is tested on; a `{ci}` bracket renders that interval; a mismatched geometry is **refused**.* This
  needs KEY 2's stored scale — it cannot be done by any argument, because `display` must stay free.
- **D22**: a `display` token whose field is empty renders **void** plus a one-time note naming the
  argument that would fill it — today it silently *substitutes another quantity*, which is neither of
  the two defensible behaviours.
- **D26**: `stars` and `color_signif` currently disagree about what an odds-ratio table compares, and
  `stars` wins — so the colour gate ends up testing a *difference* on cells displaying *odds ratios*.
  Under this design it is unrepresentable: `color` names the comparison once and every consumer reads
  that one interval.
- **D28**: `ci = "cell"` beside `stars`/`color_signif` — **inform and disable, from one rule**.

**Depends on**: 19b, 19c, and D27 from 19a. **Unblocks**: 19k (the jamovi vocabulary).

**Verification**: **full-suite checkpoint.** Conscious regens: `_snaps/render-html.md` (the tooltip
line) and the structural goldens (the populated `or`). Sentinels: `test-export-parity.R`,
`test-display-grammar.R`, `test-cumor-ordered.R`, the jamovi lock (schema bump).



---

#### Phase 19e — KEY 8b + KEY 3a: the `tab_reg()` estimand surface

**Goal**: replace a four-argument product in which most cells are inapplicable with the minimal
non-redundant parameterisation of an estimand — **(which contrast) × (which measure)** — and make a
sound, already-implemented estimand reachable through a front door.

**Read first**: study §KEY 8.8 – §8.14, §KEY 3(a) Shape 1, §4.4, §11 D5/D6/D25. Back-compat is
**waived** here (maintainer ruling), so this is a clean break with documented synonyms.

**The two axes** (measured genuinely orthogonal — a binomial *coefficient* asked as a ratio and a
binomial *marginal* asked as a ratio land on the **same stored scale row**):

```r
effect  = c("coefficient", "marginal", "at_reference")    # WHICH contrast  (absorbs `at`)
measure = c("odds_ratio", "ratio", "difference", "log")   # WHICH effect measure
                                                          #  (absorbs `exponentiate`, deletes
                                                          #   `ame_ratio`, front-doors `family = "rr"`)
```

**Contents**

- `measure`'s values accept **both the full word and the acronym** (`"ratio"` ≡ `"RR"`/`"IRR"`,
  `"odds_ratio"` ≡ `"OR"`, `"difference"` ≡ `"RD"`/`"diff"`, `"log"` ≡ `"log_odds"`), teaching the
  full word while the column header keeps the discipline's acronym — so **the table prints the
  mapping between the two every time it renders**.
- **Delete** `exponentiate` (a silent no-op on the whole marginal path, 0 of 49 taught calls, and its
  `FALSE` value *is* a measure) and `at` (degraded away in three separate blocks). Keep both as
  documented synonyms in the shim and in `?tab_reg`'s "how this is called elsewhere" line, so an
  expert's existing knowledge is a ramp in.
- **`family` loses its scale half**: `family = "rr"` and `"quasipoisson"` stop being user values.
  ⚠ Today the **only** route to a risk ratio is to name the wrong distribution — `family = "rr"` is
  refused when asked for directly. This is the change with the largest ratio of user value to work in
  the whole phase.
- **`estimate_display` → a real `display =`** mirroring `tab()`'s grammar, the four values kept as
  documented shorthands. This *deletes a preset layer* rather than adding machinery — it already is
  one (`"prob"` → `"{or} ({pct})"`).
- **`empirical` is forced by `color = "adjustment"`** — declared, not a guard block.
- **Close the capability gaps** (ruling): gaussian ratio-of-means (`tab()` already gives one happily;
  only `tab_reg()` refuses, although the scale row, the ladder and three CI engines all exist) and the
  identity-link risk difference on the coefficient path. This makes the legality table **three-state**
  — *we don't offer that* / *that cannot be done* / *the link did not converge on your data* — and it
  must ship as a **runtime object** with four consumers: the generated `?tab_reg` section, the error
  message (enumerated from the table, delivered at the moment of the mistake), a lister the user can
  call on their own outcome, and the jamovi eligibility rule. The package already does exactly this
  twice (`reg_detect_family()`'s announcement, `OR = "cumOR"`'s message **followed by the `mutate()`
  line that fixes it**) — that is the standard to copy.
- **D25**: `tab_reg()`'s `color` currently accepts geometry values that contradict what the column
  estimates. Once `measure` names the estimand and KEY 2 stores its scale, a reg table's `color` needs
  only `TRUE`/`FALSE` + `"adjustment"` / `"between_groups"` — the ladder comes from the column.
- **D6** (the multi-dependent × model-list recursion drops `spread_models` and `.fit_cache`) — an
  argument-threading bug, fixed while the threading is being rewritten.
- Keep `trials` as its own formal (it *is* a family variant internally, but a separate formal reads
  better in jamovi and in R).

**Depends on**: 19a (family predicates), 19b (the stored scale). **Unblocks**: 19g, 19k.

**Verification**: targeted — reg tables are value-asserted, not snapshotted, so
`test-tab_reg*.R` + `test-jmvtabreg-cache.R`'s byte-identity lock are the sentinels. `?tab_reg` and
both reg vignettes get their real update in 19n, but the **runtime resolution table must be complete
here** (it is what generates them).

##### Addendum — read this before planning

Two phases landed **out of the roadmap's order** while you were not run, so the repository you are
about to plan against is not the one the plan-of-plans describes. Verify each claim below in the
code before relying on it; they are stated to stop you planning against a clean slate, not to be
trusted blindly.

###### 1. Nothing of 19e exists yet — the whole scope is yours

The session that held 19e's slot never started it. It found the tree red from 19d's partial commit,
which its own summary warned against building on, and spent itself driving **FAIL 48 → 8**. Its
report says plainly: *"Nothing of 19e's own content was implemented."* Confirmed at this commit —
`exponentiate` (28 uses), `estimate_display` (31), `ame_ratio` (47) are all still live, and there is
no `measure` argument anywhere.

So: the full 19e entry, unchanged in scope.

###### 2. 19g already landed, and the roadmap says it should have come after you

Its dependency line reads *"19g must land after 19e, 19f"*. It did not. 19g is committed
(`c3c3c25`), the suite is at **FAIL 8 / PASS 6001**, and the 8 are the pre-existing
`test-jmvtab-cache.R` failures — not 19g's doing.

The practical consequence is that **19e must fit itself to structures that already exist** rather
than introduce them. What 19g built, all of it live:

- **`meta$spec`** — `R/table-spec.R`, `new_spec(kind, vars, call)`. `kind` is *stated* by the
  producer and read through `tab_kind()` / `tab_is_reg()`; `is_reg_footer()` is deleted. The old
  `meta$reg_meta` is gone: **`spec$call` is the producer's recipe now**, and it is what
  `reg_check_plots()` refits from (`fit_spec`). Your new estimand arguments are part of that recipe —
  a table must remember the `effect` × `measure` it was built with, or a refit silently changes the
  estimand.
- **`new_reg_shared()`** — the `shared` bag is a typed record whose **formals are the contract**, and
  `fmt_class.R`'s `globalVariables()` mirror is derived from them. Add `effect`/`measure` to the
  constructor; do not thread a loose argument past it.
- **One map, per-spec builders.** The three column builders (AME / MNL-vs-rest / coefficient) sit
  behind a single map with a **per-spec** choice, replacing a table-scalar `if`. `family` has been
  per-dependent since 15e. **`measure` and `effect` must be per-spec on the same footing** —
  scalar / vector / named vector, resolved exactly where `family_for` is. A table-scalar estimand
  would re-introduce the degradation 19g just removed.
- **`REG_GOF_KEYS` + `reg_stat_keys()` + `reg_validate_stat_keys()`** — one vocabulary, one
  validator, for `stats =` and `check =`. If your capability table or `measure` vocabulary needs the
  same treatment, copy this shape; do not add a second hand-written list.
- **The `test` tibble is re-keyed**: `var` (which variable the row is about, `term` folded into it),
  `col`, and the sub-population in a column named after the grouping variable. 13 columns, not 14.

**Report, in your DONE summary, whether 19g needs a corrective pass** now that the estimand is named:
specifically whether `spec$call` records enough to reproduce the estimand, and whether
`spec$vars` — built before `measure` existed — is still complete. Do not silently patch 19g's work
into your own diff without saying so.

###### 3. 19b's stored scale is in — use it, do not re-derive

KEY 2 landed: `get_scale()` / `get_pct_base()` are live and `EST_SCALES` / `est_scale_key()` are the
stored library. 19e's own text depends on this ("once `measure` names the estimand and KEY 2 stores
its scale, a reg table's `color` needs only `TRUE`/`FALSE` + `adjustment`/`between_groups`"). Read
the scale off the column; never sniff it back from `var`, a label, or a display string.

###### 4. Out of scope — do not be drawn in

- **The 8 `test-jmvtab-cache.R` failures.** Pre-existing, quarantined, unchanged through 19f and 19g,
  and explicitly assigned to **19l** as a mechanical pass. Leave them red. If your work makes them
  worse, that is yours to fix; closing them is not.
- **The ~124 deprecation WARNINGs** from the test corpus still calling `ci = "diff"` etc. A known
  19d follow-up, not 19e's.

###### 5. Sequence the work so a partial commit is still coherent

The previous four sessions each spent \$19–25 and two were cut off mid-phase. Order 19e so the
highest value lands first and any budget-forced stop leaves a consistent state rather than a
half-migrated one:

1. **`family = "rr"` through the front door.** The plan calls this "the change with the largest ratio
   of user value to work in the whole phase" — today the only route to a risk ratio is naming the
   wrong distribution, and asking for it directly is refused.
2. `effect` × `measure` with the synonym table, and the deletion of `exponentiate` / `at` /
   `ame_ratio`.
3. `estimate_display` → a real `display =`.
4. The three-state capability table as a runtime object with its four consumers.

If the budget runs short, stop at a boundary between these, commit with `— partial`, and say in the
summary exactly which of the four landed and what state the rest is in — the way 19d did. A truthful
partial is worth far more than a rushed whole.



---

#### Phase 19f — KEY 1: the row model (Option C)

**Goal**: give the row axis the same treatment the column axis already has. A tabxplor column is
exhaustively self-describing; rows have **nothing**, and what a row is gets re-derived from four
unrelated sources — a per-cell flag, a *display-time* positional vector, a magic-named label column
with three naming conventions, and comparisons of **rendered `format()` strings**.

**This is the largest structural item in Phase 19.** Read study §KEY 1 in full, including §2.6
(carrier robustness), Option C's measurements, and Appendix B (how dplyr, gtsummary and gt answer the
same question — all three converge on *"the truth is in the data, the index is derived"*).

**The shape** (settled, §4 ★):

```r
# the shared half — a field, one per cell, replacing in_totrow (the record stays at 21 fields)
row_kind ∈ {data, total, n, pct, pvalue, gof, blank}
is_totrow(x) == get_row_kind(x) == "total"          # derived, no field added

# the declaration — a factor subclass whose facts are ordinary column attributes
tabxplor_lvl : class = c("tabxplor_lvl", "factor")   # it IS a factor: is.factor() stays TRUE
attr(col, "role")     "level" | "var" | "tab_var"
attr(col, "var")      the variable its labels belong to
attr(col, "ordered")  per-variable, so a merged table stops losing it
```

**Why C**: measured 15/15 verb survival with ~4 short methods (only `bind_rows`/`vec_c` and a
`droplevels` round-trip need one — `[`, `filter`, `arrange`, `mutate`, `as.data.frame` and forcats'
`fct_drop`/`fct_rev`/`fct_relevel` all preserve class *and* attributes with **zero code**); no
`is.factor` migration; friendly `tab$marital` survives; and it is a strict **prefix** of Option B, not
a fork, if the single-leading-column shape is ever wanted.

**Contents**

- The record change (`in_totrow` → `row_kind`), and `fmt_row_flag()` gains a "first non-`data` wins"
  reduce beside its `all`/`any` ones. **R3 is why the kind cannot leave the record**:
  `fmt_color_plan()` calls `is_totrow(x)` on a *lone column* with no table in scope, and
  `test-degraded-attrs.R` locks that.
- The class, its ~4 methods, and the stamping — **every producer**: both leaves, `tab_counts()`,
  `tab_reg()`, the materialisers (which stop appending to a positional vector and start passing
  `row_kind =` to the `fmt()` call they already make).
- **Every consumer migrates in this phase** (rule 5): `tab_render_vars()` and `tab_row_roles()` stop
  guessing; `tab_collapse_total_rows` compares **keys**, not rendered strings; the transpose stops
  re-deriving; `tab_estimates()` gets real roles instead of the English-label fallback it uses today;
  the five scattered abort sites read one predicate.
- **`meta$vars` becomes derived** — `row_vars` from the `var`-role column's values, `compacted` from
  its presence, `row_roles` from the kinds, `tab_vars` from the columns that declare themselves
  (which, unlike deriving from `group_vars()`, survives `ungroup()`). `meta$vars` keeps only what no
  column can carry: `wt`, `caption`, `var_labels`.
- **`tab_reg()` stops punning.** Today `tab_render_vars()` reports a regression's *predictor* as
  `tab_vars = "var"`, `row_var = "levels"` — a fake sub-table variable, because that is the only slot
  the grouped-tab machinery offers.
- **The composition limit lifts.** `tab(d, c(marital, relig), race, tab_vars = black)` silently
  returns a **list** today, because merging several row_vars and sub-tabling compete for the one dplyr
  grouping slot (`can_merge <- length(tab_vars) == 0`). With blocks derived from the declared column,
  grouping is left to real `tab_vars` alone. **This is a documented product limitation disappearing.**
- **Degraded mode is required, not optional**: `mutate(levels = as.character(levels))` drops back to
  today's heuristics, clearly marked as fallback-only — 17c's proven pattern.

**Seam if the session runs long**: 19f-i = the record change + the class + every producer;
19f-ii = every consumer. **The migration is never split — only the sessions.**

**Depends on**: 19a. **Unblocks**: 19g, 19h, 19i, 19j.

**Verification**: **full-suite checkpoint.** The *structural* goldens move (a row-index block delta —
teach `verify_golden_field_delta.R` its next mode); **rendered output must not**. Sentinels:
`test-tab_classes.R` (verb survival), `test-degraded-attrs.R`, `test-transpose*.R`,
`test-meta-attr.R`'s field-agnostic probe, `test-export-parity.R`.



---

#### Phase 19g — KEY 6: one table identity, and `reg_build`'s assemblers

**Goal**: one statement of *what kind of table this is and which variables are in it*, for both
producers — and the end of `reg_build`'s four parallel assemblers.

**Read first**: study §KEY 6, §4.4 items 3–6, §5 (`reg_meta` rows), §11 D5.

**Contents**

- **`meta$spec = list(kind, vars, call)`.** `reg_meta` becomes `spec$call` for
  `kind = "regression"`; `is_reg_footer()` becomes `spec$kind == "regression"` — today it decides "is
  this a regression" by asking whether the `test` tibble happens to contain a reg-flavoured
  discriminator, *in the same file whose header comment says a reg table carries `reg_meta`*. Two
  encodings of one fact, one of them unused.
- `vars` is **uniform** across producers and, after 19f, largely **derived** — `spec$vars` states only
  what no column carries.
- **`reg_check_plots()`'s `fit_spec` generalises**: a table remembers how it was made. That is the one
  genuinely good idea in `reg_meta`, and it is what any future "recompute at a different reference"
  path wants.
- **`reg_build`'s four parallel assemblers** (AME, MNL-vs-rest, coefficient, and the **complete
  duplicate** split branch with its own `new_tab`/`meta`/`tab_stamp_inference` — which has already
  drifted once) become one.
- **The `shared` bag becomes a typed record**: 24 keys, documented as 20, mirrored in `fmt_class.R` to
  silence `R CMD check`, partially re-listed twice, with 2 fields declared nowhere.
- **One `stats` / `check` vocabulary** — `tab_reg(stats =)` and `reg_check_plots(check =)` are the
  same `names(REG_CHECKS)` behind two argument names and two validators.
- **The `test` tibble stops overloading `row_var`** (it means the row variable for a crosstab row and
  the split-group level for a regression row — which is why z15-i had to add a 13th column, `term`).
  Key it on `(scope, var, level, col)`, which 19f makes expressible.
- Cut `reg_meta$conf_level` — a stale duplicate of a per-column attribute — and read the column
  attribute instead (the maintainer's "use it to unlock further simplification").

**Depends on**: 19a (E1), 19e, 19f. **Unblocks**: 19l.

**Verification**: targeted; `test-meta-attr.R`'s field-agnostic probe and `test-tab_reg-footer.R` are
the sentinels. Structural goldens may move (`meta` reshape) — prove the delta.



---

#### Phase 19h — KEY 7: one entry point, one return shape, one render model

**Goal**: make what comes back predictable, collapse the four spellings of one table, and finish the
export stack's integration onto the shared render model.

**Read first**: study §KEY 7, §4.3 (all ten items), §5 (the entry-point and export rows), §11 D1/D2.

**Contents — the shape half**

- **One documented crosstab entry point.** `tab()` for everything; `tab_many()` becomes a one-line
  deprecated shim (it is already soft-deprecated yet still carries the **old** vocabulary — `chi2`,
  `totrow`, `totcol` — so four public functions document four spellings of one table);
  `tab_plain()`/`tab_num()` stays public. `tab_counts()` stays public — its *inputs*
  genuinely differ. That removes ~68 formals of drifting mirror surface.
- **A predictable return.** After 19f, `tab_vars` × several `row_vars` compose, so the list fallback
  disappears; `output_list` becomes the **only** thing that changes the shape, and
  `options(tabxplor.output_kable)` — a *display* option read inside a *build* stage — **loses its
  power to change the class of the returned object** (the option itself stays).
- **One capability predicate**, `tab_shape(x) ∈ {single, merged, grouped, list}` plus a supported-ops
  table, read by all five abort sites — so "can I transpose a grouped table?" has one answer to read
  instead of being discovered.
- **`spread` unification** (§4 ★): one implementation, `tab_spread()` keeps its name and absorbs
  `reg_spread_models()` (whose hand re-keying of the GOF block dies with it); **one argument name on
  both producers** — `spread_vars` is CRAN-released since v1.1.1, so keeping that name costs no
  deprecation and `spread_models` (never published) is simply deleted.
- **`totcol = "each"` / `"all_col_vars"` deprecate to the base behaviour** (keep one total, never
  error). While there, fix the parser: it returns a **character** for `"last"` and a **list** for the
  others, so the `identical()` comparisons never fire for the default and `"some"` is the default arm;
  `"all_col_vars"` as an input value can never produce `tot_cols_type == "all_col_vars"`; and the
  string carries **two unrelated meanings**.
- `sup_cols` keeps its deprecation and stops mirroring; `tab_md_css()` keeps the alias and drops the
  argument documented as ignored; `tab_export(format =)`'s documentation stops naming a value it does
  not accept; `tabxplor_tabs` is kept but not grown (its one behavioural bit could key on
  `!is.null(names(x))`); **verify** whether `names_prefix`/`names_sort` belong to the spread path and,
  if so, leave them there and drop them from `tab()`.
- **Owed by 19d, deliberately parked until the mirrors collapse**: `?tab`'s `OR` / `ci` / `color`
  blocks still describe the pre-19d surface, in **three** mirrored copies (`tab`, `tab_many`,
  `tab_plain`) plus four of `ci`. Rewriting them before this phase means writing the prose three
  times and deleting two. Do it here, once, as the last item — `NEWS.md` is already up to date.

**Contents — the render half**

- **The transpose builds `rd2` by modifying `rd`, not by literal reconstruction.** The 39-slot literal
  has already lost slots twice and **is losing `ann$keep_black` today**, masked by a silent fallback —
  so a transposed reg table's footer rows are wrongly greyed in HTML and nothing tests it (**D1**).
- **D2** — `theme = "print"` on the kableExtra engine renders `kable_material_dark`: a *black* table
  for the black-and-white publication palette.
- **One `"auto"` theme downgrade** — re-implemented five times with different rules, and the theme
  *option pair* differs between the export and console paths, so a footer rendered outside
  `rd_footer()` silently picks the console theme.
- **One number-font decision** (three options answer "what font do starred numbers use", although
  `roles$has_stars` is already in the model — merge to one, alias the others).
- xl reads `rd$subtext` / `rd_caption()` / `start_col_var` instead of re-deriving them; md reads
  `roles$new_col_var` instead of rebuilding it; `tab_plot` **translates** the footer model's typography
  instead of forcing `face = "bold"` and discarding the print palette's italic/underline; the two
  definitions of "is this coloured" inside one function become one; the `" [dep]"` strip regex stops
  being written twice with each copy commenting on the other; the dead model slots go
  (`vars$col_vars_levels`, `roles$no_totrows`, `ann$anchor`; `range_totcol` stays **dormant**).

**Depends on**: 19f. **Seam if long**: shape half / render half.

**Verification**: targeted, then the export-parity and transpose≡native locks. Conscious regens
limited to the md/html/xl families the render items name.



---

#### Phase 19i — The build pipeline and the `tab_counts` boundary

**Goal**: finish Phase 17e's settings spine — make it the **only** interface — and stop `tab_counts()`
re-doing `tab()`'s boundary by hand.

**Read first**: study §4.2 (twelve items), §4.6, §2.5. ⚠ §7.10's suspected live bug is verified in 19a;
if it was deferred, it is fixed here first.

**Contents**

- **The spine becomes the only interface.** `ctx$settings` is read by **exactly one function** today;
  every downstream consumer reads the *flat duplicate* written by the same `ctx_update`. Delete the
  20+ flat duplicates.
- **`na_text`/`na_num`/`lvs` join `pairs`/`cols`.** The spine's own comment *promises* `na` is there;
  `tab_prepare_pop()` never touches `settings`, and `settings$cols$lvs` is **stale** the moment
  prepare_pop re-resolves levels — and it is the stale copy shipped to every worker.
- **`new_ctx()` declares every live key, or the stage products move out of the ctx** (53 declared vs
  ~83 live; `pct_vect`/`ref_vect` undeclared while the sibling `OR_vect` is declared, which is what
  makes a NULL guard unreachable). 17 ctx fields are read by exactly one stage — they become locals.
- **Shared `leaf_head()` + `leaf_finish()`**: the inference/basis preamble and the ~30-line result tail
  are the same code twice.
- `num_core`'s ~90 inline lines of moment-sum totals fold into the shared
  `build_total_rows()`/`finalize_total_rows()`; `num_core` records `meta$vars` (it records none today,
  so `tab_num()` falls back to the last-factor heuristic `new_vars_attr` was introduced to replace).
- **`tab_resolve_common_args()`** returning the resolved
  `(color_spec, ci_method, cleannames, totrow/totcol, total_names, test)` bundle, called by `tab()`,
  `tab_many()`, `tab_num()`, `tab_counts()` **and jamovi** — this is the object 19k consolidates onto.
  `tab_counts()`'s ~15 copy-pasted boundary lines die with it.
- **Validation gets placed consistently** (study §KEY 3, fourth symptom): today `na`/`levels` are
  checked twice, `ci` only in `tab_ci()`, `pct` only at the leaf, and `totaltab`/`n_min`/`conf_level`
  never — `totaltab = "tabel"` silently means "no total table".
- `tab_counts()`'s half-gated limits become real: a user passing `ci_method = c(mean_diff = "student")`
  gets silent acceptance and no effect today.

**Depends on**: 19f (the leaves' tails move there). **Unblocks**: 19j, 19k.

**Verification**: byte-identical target — pure re-plumbing. Sentinels: `test-parallel-parity.R`
(unsandboxed), `test-cache-keys.R`, `test-fuse-parity.R`, `test-counts-parity.R`, the multi × multi
shapes.



---

#### Phase 19j — KEY 5: one aggregate core

**Goal**: honour 2.0.0's own keystone. `tab_ci()` and `tab_chi2()` are still live steps running on the
assembled table, **re-deriving from fmt markers** facts that were known in the leaf 1 500 lines
earlier.

**Read first**: study §KEY 5 in full — **including its caveat, which is real**: this is the key with
the highest ratio of *behaviour that must not move* to *lines deleted*.

**Contents**

- Compute the interval and the test **where the plan is** — in the leaf, from the aggregate. The
  `.fine`/`tab_counts()` seam already proves a leaf can be driven from a pre-aggregate.
- `tab_ci()` / `tab_chi2()` stay as **superseded public wrappers** that reconstruct a plan from markers
  for the exported step path only, exactly as `tab_pct()` now is. **They are CRAN-public and the step
  path is documented — do not delete them.**
- What dies: `detect_totcols` / `detect_refcol` / `detect_firstcol` plus the 8-branch `case_when` that
  exist only to reconstruct the plan; `tab_ci()`'s second fold of `ci = "ratio"`; its third resolution
  of `stars` from the option; and the re-plumbing scar tissue (`ci_scale`, `degf`, `conf_level` and the
  CI-method vector were each threaded in by hand, in z14 and z16, each time re-discovering that the
  step has no access to the spine).
- **The jamovi tier-3 re-reference stops needing to re-run `tab_ci()` on a carrier** — the cache's most
  delicate path exists in that shape *because* the CI is not part of the build.

**Hard gates.** The ordering invariant (`tab_chi2` and `tab_ci` are independent, but the
non-first-level drop must happen after **both**) and the jamovi tier-3 carrier both depend on today's
shape. **If `test-jmvtab-cache.R`'s cold + warm + reref lock cannot be kept green, abandon the phase
rather than force it** — and record why, so the next attempt starts from the finding.

**Depends on**: 19f, 19i.

**Verification**: **full-suite checkpoint**, plus the jamovi lock as a hard gate. Byte-identical
target for every rendered output.



---

#### Phase 19k — The jamovi boundary

**Goal**: the module stops re-implementing the R boundary. Seven rules are hand-mirrored, three of them
in JS, one of them with a **semantic shift**.

**Handed over by 19c**: `jmv_tab3_rerefable()`'s exclusion of `color = "auto"` + `ci = "diff"`
(`R/jmvtab-cache.R`) is **vestigial**. It existed because that combination resolved to the composite
`"after_ci"`, which made `tab_ci()` stamp a ref-dependent CI colour the re-ref could not reproduce.
19c deleted that resolution — the pipeline hands `tab_ci()` `color = "no"` unconditionally now — so
the case is in fact re-referable. It was deliberately NOT lifted: doing so changes which cache PATH a
live jamovi toggle takes (rebuild → re-ref), which is the seam 19c was told not to move. Lift it here,
with the cold + warm + reref lock. Also: `jmvtab_build()` (`:984-997`) still hand-mirrors the
resolver's colour→`ci` cascade; it now has `measure_forces()` / `measure_builds()` to call instead.

**Handed over by the 19d-tail (the green-light pass)** — the tier-3 cache is green and its value
assertions are all locked, so what is left here is purely *which path serves a toggle*:

- **A `diff` ↔ `ratio` colour toggle rebuilds**, and must, because since 19d the stored interval
  follows the comparison (percentage points vs Katz log-RR). The re-ref could serve it by recomputing
  the interval on the other scale (`tab_ci(ci_scale = "ratio")` — every input is already in the
  carrier); an exact re-paint never can. Four assertions in `test-jmvtab-cache.R` now state the
  rebuild explicitly (`hit = FALSE` / the `reference` vector), so lifting it means flipping them back.
- `jmv_apply_display()` now delegates to `tab_apply_display()`, so **the display vocabulary gap above
  is half closed**: a bare token and a `{}` template both work. What remains for the presets is the
  four ComboBox values that are not fields at all (`pct_ci` / `mean_ci` / `OR` / `OR_pct`).
- `jmv_oracle()` in the test file calls `tab_deprecate_or()` and `resolve_leaf_ci()` directly, so when
  the `.a.yaml` learns the new vocabulary the oracle needs no edit — only `jmv_opts()`'s defaults.

**Read first**: study §4.5 (nine items), §11 D9/D10/D11/D12/D13/D15, §7's anti-proposition on the JS
rules. Ruling: **(b)** — a shared resolver both boundaries call, plus a **generated** table for the JS
eligibility rules.

**Contents**

- Both `.b.R` boundaries call `tab_resolve_common_args()` (19i). The verbatim mirrors die: the
  stars → `ci` forcing (whose comment admits the duplication), the digits magnitude floor (byte-
  duplicated), `jmv_population_descriptor()` (a line-for-line copy of the real one, *in the file that
  also reads the real one*), the multiplier keyword set.
- **The trials-max rule's semantic shift is a defect, not a duplication**: R takes `max()` only when
  the user asks; jamovi takes it silently for any integer outcome.
- **Family detection exists three times** — `reg_detect_family()`, a jamovi R fallback, and
  `detectFamily()` in JS with its own "matches the R side exactly" note. The JS keeps its latency role
  but is **generated** from R's table, along with `familyOptionsFor`/`anyProbScale` and 19e's
  three-state capability grid.
- **The `display` vocabulary gap is structural and must be closed properly**: the ComboBox offers
  `pct_ci`/`mean_ci`/`OR`/`OR_pct`, which `validate_display_template()` would reject, so
  `jmv_apply_display()` cannot call the shared `tab_apply_display()` — which is the source of **D11**
  (a mean column with `ci = "cell"` renders **empty**). 19d's display presets are the fix: the UI
  offers presets that show the meaningful label **and** the `{}` template, which is also
  differentiator #4 doing its job.
- **`anova` becomes an argument** — the last option travelling as a global (`options()` + `on.exit`),
  and a stale-cache hazard. `.run()` shrinks to weights → build → render.
- **D12** (a `reapplied` key that is not a key of `opts`, so every CI-method toggle forces a full
  tier-3 rebuild and the cheap re-ref path is unreachable), **D13** (`filter`'s cache key is hardcoded
  `NA_character_`, so a filter change never invalidates), **D15** (a comment saying the opposite of the
  code).
- **`jmvtab_reg_staged()`** exists as the shared predicate and **its own caller inlines the predicate
  instead**, so only tests call it — one line to call it, and the two copies cannot drift.
- `jmvtab_reg_build()` passes `stats = opts$stats`, which `.opts()` never sets.
- Remove `filter` from the jamovi UI (ruling); carry 19d's and 19e's vocabulary into the YAML.

**Maintainer step**: `jmvtools::prepare()` + rebuild, then a live pass (the collapse-box, export and
eligibility selectors are best-guess against a DOM only the running app has).

**Depends on**: 19d, 19e, 19i. **Verification**: targeted + the jamovi cache locks; the live pass is
the maintainer's.



---

#### Phase 19l — Harvest 1: the deletion pass

**Goal**: **reap.** Every key in this phase was justified by the special cases it would let us delete.
This phase goes back and deletes them — measured, not assumed.

This is a *deliberately open* phase. It has a method, not a checklist.

**Method**

1. **Re-run the censuses of study §2** and diff against the starting state: total lines, code vs
   comment, function count and median length, the 163 user messages (how many are left at the two
   boundaries?), `tab()`'s and `tab_reg()`'s formal counts, the ctx's declared-vs-live keys, the
   emptiness matrix of the record.
2. **Search for the shapes the new facts made unnecessary**, not for the sites the phases already
   touched. The diagnostic greps: anything still matching a rendered label or a name prefix; any
   remaining `switch` on `type`/`ci_type`/a measure literal; every surviving hard-coded family
   whitelist (21 at the start, 14 absorbed in 19a — where are the other 7?); every `exists()` guard;
   every predicate computed in two places.
3. **Delete the fallbacks that no longer have a path to them.** 17c's pattern is to keep the old
   heuristic as a clearly-marked fallback; some of those are now genuinely unreachable and should go,
   and some must stay for the degraded contract (`test-degraded-attrs.R`). Tell them apart by
   *reading the contract*, not by guessing.
4. **Report what did not shrink**, and why. A key that did not pay for itself is a finding worth
   recording, not something to hide. If an additional step is needed to reap the harvest, tell it.

**Handed over by 19k**:

- **D22's "renders void" note is PER COLUMN but reads as PER TABLE.** Measured:
  `tab(display = "num_ci")` on a table that *does* have intervals still emits the note, because the
  `add_n` total column carries none. Correct by D22's own rule ("a field empty in the whole
  column"), misleading as a message. Either scope the note to columns the template actually
  *changes*, or word it per column.
- **`jamovi/js/*.js` has no syntax check in this repo** (no node/V8 on the box; the `node` R package
  ships a Windows binary). The suite balance-checks brackets only. A real `node --check` in CI, or a
  V8-based test, would close the last gap in "the JS is generated but not verified".
- **The reg fit-cache digest path is now unreachable for `color = "adjustment"` and for any
  `shape`** — both correct (they need the fitted object / a different model), but both are now
  reachable FROM THE UI, so the live panel refits where it used to serve a digest. Measure it before
  assuming it is fine; `dev/model_vs_observed_gap_test.md` §6 has the recipe for a digest-based
  adjustment arm.

**Handed over by 19j** (each stated in the code where it lives, none half-done):

- **`measure_stage()` is misnamed now.** Its `"chi2"` value means "the CONTRIBUTION pass stamps this
  measure", not "the chi2 step does" — the test step is gone. The distinction is real (the
  contribution is a different computation), so the function stays; only the vocabulary is stale.
  Renaming it churns `test-color-config.R`, so it was deliberately not done in 19j.
- **`if (!all(is.na(a[[11]]))) "woolf"`** (`plain_core`'s `ci_method` stamp) — a magic-value test that
  should read the plan (`or_ci`) instead, but flipping it changes the stamp on a degenerate all-NA OR
  table.
- **`tab_ci()` NAs the reference cell's BASE; `num_core()` NAs its RESULTS.** They genuinely disagree
  on a mean *cell* interval's reference row. Unifying them is a behaviour change wearing a refactor's
  clothes — decide it consciously, with a fixture.
- **The whole-table chi2 is one `agg_chi2()` call per col_var** now, not one batched call for all of
  them (values identical, `table_id` already partitioned by col_var). Cost unmeasured — re-run
  `test-benchmark.R` on a wide table.
- **`dev/verify_golden_field_delta.R` compared table attributes ORDER-sensitively** until 19j fixed
  it. Any earlier phase that merely reordered an attribute would have been reported as a regression.

**Verification**: targeted per deletion. Zero golden churn is the expectation — if a deletion moves
output, it was not dead.

##### Handed forward BY 19l (see CLAUDE.md's DONE summary for the measurements)

**One hand-over 19l declined, with its reason — do not re-issue it as written.** 19j asked that
`plain_core`'s `if (!all(is.na(a[[11]]))) "woolf"` (`R/tab.R`) read the plan (`or_ci`) instead.
Measured: **the hand-over is wrong.** `ci_method` is a column-SCALAR, and the reference column, the
total column and any degenerate 2×2 carry all-NA bounds *by construction* (the two masks in
`tab_apply_reference`). Reading `or_ci` would stamp `"woolf"` on columns whose bounds were never
computed, and `legend_method_name()` would then print *"Wald interval on the log odds-ratio"* for
them — the exact **D8** failure the surrounding comment cites as its reason to exist. A real fix
threads the `CI_GEOMS` row per column; that is a different, larger change.

**Behaviour decisions, each needing its own fixture (→ 19m):**

- **`tab_ci()` NAs the reference cell's BASE; `num_core()` NAs its RESULTS.** Re-measured: a
  `ci = "cell"` mean keeps its reference-row interval through `tab_num()` and loses it through the
  step path — and `leaf_ci_plain()` sides with `tab_ci()`, so it is **`num_core` alone vs the other
  two**, not legacy vs modern. Either unification changes real cells.
- **D22's "renders void" note is per COLUMN but reads as per TABLE** (19k's hand-over, unchanged).

**Newly found in the sweep (→ 19m):**

- **`emp_tips` is a positional per-row vector** (`tab-export-prep.R`, `prep_one_table`), carried
  through `ungroup → drop_tab_vars → wrap` on the strength of a comment saying those three never
  reorder. The same shape as the `row_roles` vector 19f deleted, one layer down. (19l fixed its
  *key* — it sniffed for a column named `"var"` — but not its carrier.)
- **`spread_relabel()` welds two facts into one string with an HTML tag** (`tab.R`:
  `paste0(g, "<br>", get_col_var(...))`), which three downstream sites then sniff for
  (`tab_xl.R` ×2, `fmt_class.R`). A magic in-band separator carrying a fact that wants its own
  attribute.
- **`grp_col[1]` in `tab_collapse_total_rows()`** (`tab_classes.R`) assumes the first grouping
  variable is the merge's variable column, but `tab_compact()` groups by
  `c(merge_tab_vars, "row_var")` — so on a compacted **+ `tab_vars`** table (a shape 19f newly made
  possible) it keys on the tab_var. `tab_declared_vars(tab)$var_col` is already called in the same
  function. **Suspected live, not reproduced** — needs a fixture first. Opt-in path
  (`common_totrow`), so not on the default one.
- **`is_reg` names two different questions**: `tab_is_reg()` (the stored kind) vs `fmt_class.R`'s
  `!is.null(reg_call(x))` ("does it still carry the recipe"). They diverge on a `meta`-stripped reg
  table, which `test-degraded-attrs.R` deliberately locks — a **rename**, not a merge, and the
  comment calling the second "robust" is misleading.
- **The `"Total"` build-time sentinel family** (`tab.R`: `leaf_totrow_tottab`, the `totcol_vector`,
  the total-row scans) matches the literal the leaf itself minted before `leaf_rename_totals()`.
  Not a rendered label, but the last place "is this the total" is a string, and a source level
  genuinely named `"Total"` is indistinguishable. Lower priority than it looks.
- **The silent length-fallback guards** (`tab-render-html.R` ×2, `tab_md.R`, transpose ×3) — the
  class that masked D1's greyed footer for two phases. Delete-or-promote-to-`stopifnot`, consciously.
- **`reg_crude_key()` returns `"binomial"` for an `rd` fit with `trials`**, where a grouped binomial
  would get `"grouped_binomial"`. Harmless today (`reg_fit()` does not fit `rd` as grouped either),
  but the two now disagree in one stated place instead of three unstated ones.

**Corrections to the record — both overstate what exists:**

- **There is no committed JS bracket-balance check.** CLAUDE.md and §19k both say *"the suite
  balance-checks brackets"*; `tests/` opens no `.js` file. `test-jamovi-vocabulary.R` verifies only
  the **generated blocks** — a few dozen lines out of `jamovi/js/`'s 1610. The three live JS bugs 19k
  fixed were found by hand, which is the class an unverified 1560 lines produces. A real gate needs
  `V8` (Suggests + `skip_if_not_installed`) or a CI-only `node --check`.
- **The deprecation corpus is ~136 sites, not 385.** Of the raw grep hits, **177 are permanent silent
  aliases** (`color = "diff"` 156, `color = "OR"` 21 — `COLOR_ALIASES`, never deprecated by design).
  The genuine migration is `ci = "diff"` 70, `OR =` 35, `tab_many()` 22, `chi2 =` 17, `sup_cols` 12.
  ⚠ `tests/testthat/setup.R` records that `options(lifecycle_verbosity = "quiet")` **does not work**
  (`local_reproducible_output()` resets it per `test_that()`), so the only levers are migrating the
  call or `suppressWarnings()` where the deprecated form *is* the subject.

**Measurements still owed** (19j/19k asked, 19l did not run them): the per-`col_var` `agg_chi2` cost
— which needs a **new op** in `benchmark_small_ops()`, not a new harness, since nothing in-suite
tests >1 col_var *with* a test; the reg fit-cache digest path now unreachable for
`color = "adjustment"` / any `shape`; and 19d's odds-ratio cost on a wide table.


---

#### Phase 19m — Harvest 2: open integration

**Goal**: **think out of the box.** With rows and columns both self-describing, one vocabulary end to
end, and one resolution spine, *what becomes possible that was not worth attempting before?*

Also deliberately open. Start in plan mode, propose, and **ask the maintainer before building**
anything user-visible. Candidate directions, none of them mandates:

- **Cross-producer features that were blocked by the row pun.** `forest_plot()`, `tab_estimates()`,
  `reg_check_plots()`, `tab_spread()` and `tab_plot()` now work on either kind of table with no
  branch — is there an obvious feature sitting on top of that?
- **A generated help surface.** 19e's capability table already ships as a runtime object with four
  consumers. Does the same treatment pay for `display`'s tokens (the roxygen documents **11 of 22**),
  the colour measures, or the options list?
- **The transpose as a pure flip** of a declared index — 19h makes `rd2` a modification; can the two
  role models finally become one builder?
- **`tab_compact()`'s cross-call merge** is the one thing `tab()`'s built-in merge cannot express, and
  it is undocumented; with a declared row index, is it still a separate concept?
- **What does a table remember?** KEY 6's `spec$call` + `fit_spec` means a table knows how it was
  made. `tab_recompute(x, ref = …)` / a re-reference without a rebuild is now expressible — is it
  wanted, or is it a white elephant in waiting? (Apply the §5 test honestly: *who sets it, how often,
  and what happens if they never do?*)
- **The `n`/`wn`/`tot_n`/`n_eff` quartet** is four slots for ~two facts. §6 ruled *one accessor, not a
  merge* — 19a builds the accessor. Is there anything left worth doing here, or is that closed?
- **Options.** 42 global options is a lot. After 19h merges the three number-font knobs, is there a
  second cluster answering one question? Or globally useless options that we could remove before release ?

**The discipline that keeps this phase honest**: every proposal must name the *fact* it stores or the
*rule* it states, and the scattered special cases it collapses. A proposal that only adds capability
belongs in a feature phase, not here.

**Verification**: whatever the accepted proposals need.



---

#### Phase 19n — Documentation, i18n, and release readiness

**Goal**: the taught surface matches the shipped one, in both languages, and the package is ready for
CRAN.

**Contents**

- **`?help`**: every changed argument (`tab()`'s seven-argument surface, `tab_reg()`'s two axes,
  the derived `get_type()`/`get_ci_type()`, `tab_spread()`, the superseded entry points), plus the
  generated sections 19e's capability table feeds. `?tabxplor-options` re-synced with `.onLoad`.
- **The six vignettes** (3 EN + 3 FR, kept mirrored): the new `tab()` surface in the intro; `measure`
  × `effect` and the "how this is called elsewhere" lines in the regression vignettes; the programming
  vignette's field/attribute lists (`row_kind`, `scale`, `pct_base`, `ci_method`, `ordered`).
  ⚠ Pin `options(tabxplor.lang=)` **and** `LANGUAGE` in every document (§7.9).
- **The cold-but-good documentation debt** the study named: `levels = "auto"` documented as *one level
  for binaries, all levels when 3+* (the useful default nobody knows about), and the per-col_var
  vector forms of `levels`/`digits`.
- **i18n**: `po/R-fr.po` + `.mo` recompiled once, at the end (`Rscript dev/update_translations.R`;
  `msgfmt` must be installed). KEY 4's generated extraction anchor replaces the hand-maintained one.
- **`NEWS.md`**: the deprecations (soft: `OR`, `ci`'s geometry values, `tab_many()`, `type`/`ci_type`
  accessors; hard on the reg side: `exponentiate`, `at`, `ame_ratio`, `family = "rr"`), the new
  arguments (`measure`, `ci = "ref"`, `ref2 = "cumulative"`, `tab_reg(display =)`), the behaviour
  changes (the unconditional odds ratio, the inform-and-disable rule, the predictable return class).
  Keep it to the Phase-y standard: **no dev detail at all**.
- **`README.Rmd`** re-knit (it renders live coloured tables; check the hero screenshot is still
  representative).
- **Release checks, in this order**: full suite in the normal locale → **the CI-locale run, once**
  (`LC_ALL=C.UTF-8 LANGUAGE=en`) → `devtools::document()` → `devtools::check()` → `pkgdown` bilingual
  build. `dev/release_checklist.md` governs the branch mechanics.

**Verification**: this phase *is* the verification.



---

## 10. Where each defect lands

From study §11. D3 was withdrawn (verified: `meta` survives the step tails — *undesigned, not broken*,
so passing it explicitly is hygiene); D24 was checked and **not** confirmed.

| defect                         | phase   | defect                      | phase |
|--------------------------------|---------|-----------------------------|-------|
| D16 (bind_rows drops attrs)    | **19a** | D20 (`OR` + `ci="cell"`)    | 19d   |
| D27 (`ref2 = "last"`)          | **19a** | D21 (mismatched interval)   | 19d   |
| D5, D7, D14, D15, D18, "D3"    | 19a     | D22 (`{or}` prints `pct`)   | 19d   |
| D8 (legend method name)        | **19b** | D23 (display ≠ interval)    | 19d   |
| D17, D19                       | **19b** | D26 (stars vs color_signif) | 19d   |
| D4 (allow-lists disagree)      | 19c     | D28 (`ci="cell"` no stars)  | 19d   |
| D25 (reg colour contradiction) | 19e     | D6 (recursion drops args)   | 19e   |
| D1 (transpose `keep_black`)    | 19h     | D2 (print + kableExtra)     | 19h   |
| D9, D10 (stale `.h.R`)         | DONE †  | D11, D12, D13               | 19k   |

† **already cleared** by the `prepare()` of 2026-08-13 (§7.6), before Phase 19 starts; re-verified
in 19k after that phase's own `prepare()` + rebuild.

---

## 11. Open questions deliberately left to plan time

Not oversights — decisions that need the phase's own context to make well.

1. **`set_type()`** (19b) — soft-deprecated setter writing both attributes, or removed? It is exported
   and invertible, so both are defensible (§7.3).
2. **The unconditional odds ratio** (19d) — the go/no-go is a **measurement**, taken in the phase
   (§7.1), not a design decision taken here.
3. **KEY 1's fallback surface** (19f) — exactly which of today's heuristics stay as marked fallbacks
   for the degraded contract, and which become unreachable and go. Read `test-degraded-attrs.R`'s
   contract, do not guess.
4. **`names_prefix` / `names_sort`** (19h) — verify whether they belong to the spread path; if so they
   stay there and leave `tab()`.
5. **`tab_many()`'s shim body** (19h) — `function(...) tab(..., output_list = TRUE)` is the study's
   sketch, but `tab_many()` carries four formals `tab()` does not have; the shim must map them or
   deprecate them explicitly.
6. **Everything in 19m** — by construction.

---

## 12. What this phase is *not*

- Not a feature phase. The only genuinely new user capability is `measure = "ratio"` on outcomes that
  refuse it today, and that exists because the estimand is already implemented and merely unreachable.
- Not a performance phase. The settled perf verdicts stay settled; the one measurement that gates a
  decision (§7.1) is a *no-regression* check, not an optimisation.
- Not a statistics phase. The study found **no soundness problem anywhere** — every issue is
  structural. Do not "improve" a statistic while passing through.
