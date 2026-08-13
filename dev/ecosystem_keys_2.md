# tabxplor — the remaining keys

**An ecosystem integration study, post `Last Phase z16-iiiii` (2026-08-13)**

This is the second end-of-cycle design review. The first
(`dev/tabxplor_ecosystem_simplification.md`, 2026-07-20) produced Phase 17, and most of its
propositions have landed. Since then the package gained survey designs, the gap tests, crude twins for
every family, model checks, the print palette and the French layer — roughly **+8 000 lines** — and the
shape of the remaining complexity has moved.

This document does **not** re-audit what Phase 17 fixed. It asks one question:

> *What are the missing keys — the small number of stored facts or stated rules that would each
> collapse many scattered special cases at once?*

Backward compatibility is deliberately set aside in §3–§6 and reinstated in §9.

**How to read.** §1 grounds the analysis in real use. §2 is the measurement sheet — the numbers every
later claim rests on. **§3 is the substance: seven keys**, each stated as *the fact that is missing*,
with its evidence, what it unlocks, and its honest cost. §4 maps the remaining subsystem-level
simplifications. §5 is the white-elephant list, §6 the field/attribute verdicts, §7 the
anti-propositions, §8 the caveats, §9 the sequencing, §10 the decisions the maintainer must take.
§11 lists the defects found in passing.

**Method.** Seven parallel deep audits (crosstab argument surface; `tab_reg`; the fmt record; the
colour engine and legend; the export stack; the build pipeline; jamovi + the multi-table story +
`tab_counts`), plus direct probes of built objects. Evidence is `file:line` from the working tree of
2026-08-13. Where two audits disagreed, the claim was re-verified by running it (one such claim is
recorded, corrected, in §11 D3).

⚠ `R/tab.R`, `R/tab_reg.R` and `R/tab-agg.R` were under concurrent edit during this audit; line
numbers may drift by ±20, structure does not. **Re-grep before acting.**

---

## 1. Ground truth — what must survive any simplification

The vignettes, README, jamovi UI and the maintainer's own review scripts converge on a narrow hot
surface. Everything proposed below is judged against it.

**What tabxplor is for.** A sociologist (or a literary-studies student in a jamovi lab) explores a
survey: cross-tabs with percentages, *coloured so the table reads at a glance*, with significance
respected; then means; then confidence intervals and tests; then the same association *adjusted* by a
regression, with the crude number printed beside the modelled one; then export to Word/Excel/HTML with
the colours intact.

**The five differentiators the internals exist to serve:**

1. **Per-cell statistical metadata** — a cell carries everything, so display is switchable losslessly
   (`display = "{pct} ({n})"`), and `dplyr` verbs keep it.
2. **Colour that reads significance** — the three `color_signif` policies. No mainstream package does
   this.
3. **Crude-vs-model comparison** (`empirical = TRUE`, `color = "adjustment"`) — genuinely unique.
4. **The jamovi teaching path** — R argument names are visible in the UI *on purpose*
   (`"color = <color helpers>"`), because the module is a ramp to R.
5. **tibble/dplyr citizenship.**

**Two facts about the surface, measured (§2.3):** the hot path is ~12 arguments wide, while `tab()`
now has **51 formals** and `tab_reg()` **30**. And a large *cold-but-good* set exists — `tab_counts()`,
`transpose=`, `tab_css()`, `split_var=` — which is under-taught, not unwanted. Distinguish the two
before cutting.

---

## 2. The measurement sheet

### 2.1 Size and shape

|                           | value                                                                                  | source                    |
|---------------------------|----------------------------------------------------------------------------------------|---------------------------|
| R source                  | **38 784 lines**, 19 853 code, **15 909 comment (41 %)**                               | census                    |
| top-level functions       | 900, median **17 lines**                                                               | census                    |
| the four big files        | `tab.R` 7 680 · `tab_reg.R` 5 747 · `fmt_class.R` 5 696 · `tab_classes.R` 4 197        | —                         |
| the two biggest functions | `reg_build` 1 307 L (432 code) · `tab_reg` 763 L (**484 code**)                        | `tab_reg.R:3595`, `:4902` |
| next four                 | `num_core` 700 · `plain_core` 616 · `format.tabxplor_fmt` 555 · `md_render_one` 425    | —                         |
| user messages             | **163** `cli_abort/warn/inform`, of which **67 in `tab_reg.R` + 50 in `tab.R` = 72 %** | grep                      |
| exported functions        | 84 (+ 120 S3 methods)                                                                  | `NAMESPACE`               |
| global options            | **42**                                                                                 | `.onLoad`, `utils.R:53`   |

The message distribution is the single most diagnostic number in the table: **nearly three quarters of
everything the package says to a user is said while negotiating arguments**, before any computation.

### 2.2 The five crosstab entry points

| function       | formals | in `tab()` but not here                                        | here but not in `tab()`                                        |
|----------------|---------|----------------------------------------------------------------|----------------------------------------------------------------|
| `tab()`        | **51**  | —                                                              | —                                                              |
| `tab_many()`   | 41      | 14 (`test`, `tot`, `display`, `output_list`, `spread_vars`, …) | 4 (`chi2`, `totrow`, `totcol`, `compact`, `na_drop_all`)       |
| `tab_counts()` | 40      | 16 (`wt`, `levels`, `design_effect`, `parallel`, …)            | 6 (`counts`, `wt_counts`, `cols`, `col_name`, `base`, `input`) |
| `tab_plain()`  | 26      | —                                                              | `num`, `df`, `.fine`, `.by_table`                              |
| `tab_num()`    | 27      | —                                                              | `ci_scale`, `num`, `df`, `.fine`, `.by_table`                  |

`tab()`'s 51 decompose as **6 variable roles + 4 hard-deprecated + 2 soft-deprecated + 1 superseded +
4 internal dot-args = 34 genuine settings**. `tab_many()` is already soft-deprecated (it warns and
points at `tab()`), yet it still carries the **old** vocabulary (`chi2`, `totrow`/`totcol`) that
`tab()` renamed. Four public functions therefore document four slightly different spellings of one
table.

### 2.3 What is actually taught

Exported functions **never used in any vignette or the README**: **48 of 84**. Five are used nowhere at
all (vignettes, tests *and* `R/`): `complete_partial_totals`, `set_ci_type`,
`tab_get_wrapped_dimensions`, plus the two jamovi entry points (expected).

Arguments with **zero uses in every corpus** (7 vignettes/README + ~90 test files + 8
`dev/review_manual/` scripts):

- **`filter`** — and its documented use case (tribble + `pmap`) is exercised at `test-tab.R:325-343`
  *without it*.
- **`names_prefix`, `names_sort`** (on both `tab()` and `tab_counts()`).
- **`levels = "auto"`** (only `"first"` 43×, `"all"` 3×), and the per-col_var **vector** forms of
  `levels` and `digits`.
- **`tab_many(na_drop_all=)`, `tab_many(compact=)`, `tab_many(totrow=)`**.
- `totaltab_name`, `total_names`, `other_level` — used **only** inside jmvtab option lists
  (`helper-benchmark.R:51`, `test-jmvtab-cache.R:17,39`), never in a hand-written call.

### 2.4 The record

**21 fields** (`fmt_class.R:1731`), **14 column attributes** (derived, `fmt_class.R:1741`), **3 table
attributes** (`subtext`, `test`, `meta`).

Measured emptiness by path (probe, `gss_simple`):

| column                 | fields entirely NA/FALSE                                                                           |
|------------------------|----------------------------------------------------------------------------------------------------|
| `tab()` row-% cell     | `wn`(unweighted) `mean` `ctr` `var` `pvalue`\* `or` `n_eff` `obs` `gap_se` `in_tottab` `in_refrow` |
| `tab()` mean cell      | `wn` `pct` `ctr` `pvalue` `or` `tot_n` `n_eff` `obs` `gap_se` …                                    |
| `tab_reg()` `Model_OR` | `n` `wn` `pct` `mean` `diff` `ratio` `ctr` `var`† `tot_n` `n_eff` `gap_se` `in_totrow` `in_tottab` |

\* non-NA once `ci = "diff"` or stars are on. † non-NA on the coefficient path (`var(Y)`).

**Path-exclusive fields:** `tot_n` (crosstab-factor only), `ctr` (chi2 only), `obs`/`gap_se`
(regression only), `pct` (never on the numeric leaf), `mean` (never on the factor leaf).

### 2.5 The build

`tab_build()` runs six stages over a `ctx`. `new_ctx()` declares **53 fields**; a fully populated `ctx`
carries **~83 keys** — 30 added by stages with no declared default. The Phase-17e settings spine
(`ctx$settings = list(rows, cols, pairs)`) is read by **exactly one function**, `tab_rowvar_ctxs()`
(`tab.R:1659,1660,1685`); every downstream consumer reads the *flat duplicate* written by the same
`ctx_update` at `tab.R:2114-2127`.

---

## 3. The seven keys

Each key is stated as **a fact the code needs but does not store, or a rule it applies but does not
state**. Ordered by leverage, not by cost.

---

### KEY 1 — Rows have no model. Columns do

**The fact that is missing:** *what a row is*.

A tabxplor column is exhaustively self-describing: 14 attributes say what it measures (`type`,
`ci_type`, `col_var`), what it is for (`role`, `model_family`), how it was computed (`conf_level`,
`degf`, `basis`), how it is coloured (`color`, `color_signif`), and where it sits (`totcol`, `refcol`,
`comp_all`, `ref`). Any single column, extracted alone, still formats and colours itself — a tested
contract (`test-degraded-attrs.R`).

Rows have **none of this**. What a row is gets re-derived, everywhere, from four unrelated sources:

| what is needed                              | how it is answered today                                                        | site                      |
|---------------------------------------------|---------------------------------------------------------------------------------|---------------------------|
| "is this a total row"                       | a per-**cell** field `in_totrow`, broadcast across the row                      | `fmt_class.R:650`         |
| "is this a synthetic n / p-value / GOF row" | a **positional character vector** `meta$vars$row_roles` (17c)                   | `tab_classes.R:234-239`   |
| "which variable does this row belong to"    | a **magic-named label column** — `row_var`, or the tab_var's own name, or `var` | measured below            |
| "are two rows structurally the same"        | comparing their **rendered `format()` strings**                                 | `tab_classes.R:1546-1549` |

The label block has **four shapes** with three naming conventions (measured directly):

| table                                     | leading columns               | grouping  | `meta$vars`                           |
|-------------------------------------------|-------------------------------|-----------|---------------------------------------|
| `tab(d, marital, race)`                   | `marital`                     | —         | `row_vars = marital`                  |
| `tab(d, c(marital, relig), race)`         | **`row_var`** \| **`levels`** | `row_var` | `row_vars = c(…)`, `compacted = TRUE` |
| `tab(d, marital, race, tab_vars = black)` | `black` \| `marital`          | `black`   | `tab_vars = black`                    |
| `tab_reg(d, "married", …)`                | **`var`** \| **`levels`**     | `var`     | **absent**                            |

`tab_render_vars()` exists solely to guess which of these it is looking at, and it answers the reg
table with a **pun**: `tab_vars = "var"`, `row_var = "levels"` — a regression's *predictor* is reported
as a sub-table variable because that is the only slot the grouped-tab machinery offers.

**What this costs today, measured:**

1. **A structural limit users hit.** `tab(d, c(marital, relig), race, tab_vars = black)` silently
   returns a **list**, not a table — `can_merge <- length(tab_vars) == 0` (`tab.R:2727`). Merging
   several row_vars and sub-tabling both need the one grouping column, so they compete. The comment
   calls it "deferred, §7"; it is deferred because there is nowhere to put a second row axis.
2. **The `test` tibble's keys are overloaded.** `row_var` means the row variable for a crosstab row and
   the **split-group level** for a regression row (`tab-test-display.R:492-499`). Phase z15-i had to
   add a 13th column, `term`, because `row_var` was taken — a fact that phase's own design doc got
   wrong and only implementation revealed.
3. **Positional roles are fragile by construction.** `row_roles` is a bare character vector indexed by
   position; `tab_collapse_total_rows` slices it, the transpose re-derives it, and any row reordering
   by a user desynchronises it silently.
4. **Rendered-string comparison.** `collapse_totals` runs a full `format()` pass over every fmt column
   of every block just to decide two Total rows are "the same as displayed"
   (`tab_classes.R:1546-1549`).
5. **`num_core` records no `vars` at all** (`tab.R:5773, 5777`), so a `tab_num()` table falls back to
   the last-factor heuristic that `new_vars_attr` was introduced to replace.
6. **The support matrix is enforced by five separate aborts and written down nowhere** —
   `tab_compact` (`tab_classes.R:1259`, `:1273`), `tab_transpose` (`tab.R:2999`, `:3007`),
   `tx_transpose_render` (`tab-transpose-render.R:33`). "Can I transpose a grouped table?" has no
   single answer to read.

**The key.** Give the row axis the same treatment the column axis already has: **a declared index
block**. One leading block of columns, each with a stored role, in a fixed order —

```
[ tab_var levels … ] [ var ] [ level ]        roles: "tab_var" | "var" | "level"
```

— plus, per row, a stored `row_kind ∈ {data, total, subtotal, n, pct, pvalue, gof, check}`. Where it
lives matters less than that it be **declared and keyed, not positional**: the cheapest carrier is a
`meta$rows` tibble keyed by the same `(var, level)` tuple the body already carries, which survives
`filter()`/`arrange()` because it joins rather than indexes.

**What it unlocks (in one motion):**

- `tab_vars` × several `row_vars` **compose** instead of competing — the list fallback disappears, and
  with it a documented product limitation.
- `tab_reg()` stops punning: a predictor is `role = "var"`, not a fake sub-table. `or_plot()`,
  `reg_check_plots()`, `tab_spread()`, `tab_plot()` then work on either kind of table with no branch.
- `tab_collapse_total_rows` compares **keys**, not rendered strings.
- The transpose becomes a flip of a declared index (KEY 7 / §4.3), not a 250-line hand-copy.
- The `test` tibble keys on `(scope, var, level, col)` and stops overloading `row_var`.
- One `tab_shape(x)` capability predicate replaces the five scattered aborts, and makes the missing
  combinations explicit rather than discovered.
- The French/i18n hazard that 17c half-closed is fully closed: nothing left matches labels.

**Honest cost.** The largest structural item in this document. It touches the leaves' tails, the
compact/spread/transpose trio, the export prep's label runs, and `tab_reg`'s assembler. It is **not**
an fmt-record change and needs no rendered-output movement, but it will move the *structural* goldens
and needs `dev/verify_golden_field_delta.R` extended once more. Estimated: two sessions, three with
the reg side.

**Caveat.** Do not implement this as a *fifth* representation added beside the existing four. The value
is entirely in deleting the other four. If the merged / tab_vars / reg shapes are not all migrated,
this becomes the ad hoc layer this roadmap exists to avoid.

---

### KEY 2 — A column does not say what it estimates

**The fact that is missing:** *which field holds the estimate, on which scale, against which null.*

Every tabxplor cell that carries a comparison has an estimate, an interval, a p-value, a base and a
null value. Interval, p-value and base are stored honestly. **The estimate's identity is not** — it is
re-derived from `ci_type` by a switch written out six times:

```r
fmt_est_field <- function(ci_type)
  switch(as.character(ci_type)[1], "or" = "or", "ratio" = "ratio", "diff")   # fmt_class.R:2015
```

repeated at `get_num()`'s `est_ci` arm (`fmt_class.R:536-540`), `set_num()` (`:576-584`), `ci_center()`
(`:1838-1844`), `fmt_gap_parts()` (`:2024-2035`) and `reg_gap_se_of()` (`tab_reg.R:3463`).

Three symptoms prove the fact is genuinely absent, not merely un-factored:

1. **`diff` is quadruple-booked.** It holds a cell-vs-reference difference, a raw regression
   coefficient (`display = "coef"`), a **model goodness-of-fit statistic** (`display = "gof"` — N, AIC,
   R²) and the coefficient again under `est_ci`. `fmt_color_slots()` then has to *special-case `gof`
   back out* (`fmt_class.R:3543-3545`) because a large AIC in the `diff` field would otherwise score to
   the strongest colour slot. **A rule that exists only to undo a storage decision is the definition of
   a symptom.**
2. **`var` is used as a type discriminator.** `fmt_gap_scale_key()` (`fmt_class.R:3277-3283`) must
   sniff `!all(is.na(get_var(x)))` to separate a Poisson **count AME** from a raw Poisson
   **coefficient**, and its own comment says why: *"they are identical in `type`, `ci_type` and
   `model_family`; only `var` separates them."* The comment also warns that **the order of the branches
   is the contract** — a dispatch whose correctness depends on branch order is under-determined input.
   And `var` on a reg column is `rep(var_y, n_rows)`: a **per-column constant stored in a per-cell
   field**, load-bearing for scale selection.
3. **`type` is doing two jobs.** Its eight values mix a *percentage base*
   (`row`/`col`/`all`/`all_tabs`) with a *column kind* (`n`/`mean`/`coef`). The collision is visible in
   the code: `is_mean <- type %in% c("mean","n")` (`fmt_class.R:3310`) puts a **count** column in the
   "mean" bucket, while `is_rm <- type %in% c("row","mean")` (`:4996`) puts a **mean** column in the
   "row %" bucket. `"coef"` was added explicitly *"without abusing mean/row"* — i.e. as an acknowledged
   escape hatch. And `type = "mixed"`, produced by the reconcilers, is a value its own setter
   `set_type()` rejects (`fmt_class.R:626`).

**The key.** Store the estimand. One column attribute carrying `(field, scale, null)`:

| column                          | `estimand`                                                |
|---------------------------------|-----------------------------------------------------------|
| row %                           | `field = "pct"`, `scale = "proportion"`, `null = NA`      |
| cell-vs-ref difference          | `field = "diff"`, `scale = "additive"`, `null = 0`        |
| RR / mean ratio                 | `field = "ratio"`, `scale = "multiplicative"`, `null = 1` |
| OR / IRR                        | `field = "or"`, `scale = "multiplicative"`, `null = 1`    |
| β / AME                         | `field = "diff"`, `scale = "additive"`, `null = 0`        |
| log-OR (`exponentiate = FALSE`) | `field = "diff"`, `scale = "log"`, `null = 0`             |
| GOF statistic                   | `field = "diff"`, `scale = "none"`                        |

and split `type` into `pct_base ∈ {row, col, all, all_tabs, none}` × `col_kind ∈ {count, pct, mean,
coef}`.

**What it unlocks:** `fmt_est_field`/`fmt_est_of` become one attribute read; `ci_center()`'s
`ci_type`-then-`type` fallback chain disappears; `fmt_color_plan`'s `ci_mult` (`:3333`), `ci_neutral`
(`:3386`), `has_ci` (`:3385`) and `sd_ref` (`:3367`) all read one place; **`fmt_gap_scale_key()`'s
order-dependent `var` sniffing is deleted outright**; the `gof` special case becomes `scale = "none"`
(uncoloured by declaration); and the `log_odds` scale swap (`fmt_class.R:3345`, a literal
`measure == "diff"` test) becomes `scale == "log"`.

**Honest cost.** Adding a column attribute is cheap *in principle* — 17a derived `fmt_col_attrs` from
`new_fmt()`'s formals so the list cannot drift — but **the four reconstructors still enumerate all 14
attributes by hand**: `vec_cast` (`fmt_class.R:5266-5303`), `vec_ptype2` (`:5152-5224`), `vec_arith`
(`:5451-5571`), `vec_math` (`:5608-5693`); and **the two leaves pass only 9 of the 14**
(`tab.R:4470-4473`, `5746-5752`), the rest arriving from defaults plus the `tab_stamp_inference()`
sweep. So the enabling move must come first:

> **Enabling move E1 — make attribute carry generic.** Drive the four reconstructors from
> `fmt_col_attrs` plus a small declared reconcile rule per attribute (`same-or-neutral`, `weakest`,
> `min`), exactly as `meta_bind_rules` (`tab_classes.R:355-358`) already does for the table `meta`.
> A byte-identical refactor that turns "add an attribute" from a 6-site checklist into a 2-line
> change, and the prerequisite for KEY 2, KEY 4 and KEY 6.

**Should the fields themselves be merged into one `est`?** *Probably not, and the precedent cuts both
ways.* In favour: `diff`/`or`/`ratio` are provably a discriminated union tagged by `ci_type`, and the
package **already serves derived fields through `$`** — `$ci` was dropped as a stored field in Phase 1a
and is recomputed by `get_ci()`, `$tot_wn` is not a field at all, `$wn` falls back to `n`
(`fmt_class.R:3162-3181`) — so the user contract is *not* the blocker it looks like. Against: Phase z6
measured the whole sparse-record question and found the memory and speed cases empty, and the same
arithmetic applies; and `diff` ↔ `ratio` are simultaneously present and *both read* by the colour
engine's bound rescale (`fmt_class.R:3427-3440`). **Recommendation: store the estimand, keep the
fields.** Revisit only past ~30 fields (z6's own re-open threshold).

---

### KEY 3 — Most arguments are consequences, not choices

**The rule that is applied but not stated:** *the derivation graph between arguments.*

163 user messages, 72 % of them in the two argument boundaries. `tab_reg()`'s body is **484 lines of
code containing 92 `if`, 30 `is.null` and 32 CLI messages** whose entire product is a 13-key `spec`
list and a 24-key `shared` list; the actual work is one call at `tab_reg.R:5579`. `tab()`'s boundary
plus `tab_setup()` is the same shape.

Almost none of that is validation. It is **derivation**: computing an argument the user did not set,
from arguments they did, and telling them about it. The graph, gathered from both boundaries:

| derived                          | from                                                                                                          | site                                                     |
|----------------------------------|---------------------------------------------------------------------------------------------------------------|----------------------------------------------------------|
| `exponentiate`                   | `family != "gaussian"` — its *only* real use, 8 code sites                                                    | `tab_reg.R:5145`                                         |
| `at = "reference"`               | degraded to `"average"` in **three** separate blocks                                                          | `tab_reg.R:5091, 5126, 5127`                             |
| `estimate_display ∈ {prob, ame}` | degraded away when `effect ∈ {ame, ame_ratio}`                                                                | `tab_reg.R:5169-5178`                                    |
| `empirical`                      | forced `TRUE` by `color = "adjustment"`                                                                       | `tab_reg.R:5261`                                         |
| `trials`                         | *is* a family variant (`crude_key = "grouped_binomial"`)                                                      | `tab_reg.R:257-262`                                      |
| `ci = "diff"`                    | forced by `color_signif ≠ ignore`, by `stars = TRUE`, by `color ∈ diff-family`, and again at the numeric leaf | `tab-resolve.R:118-127, 165, 177-182`; `tab.R:5163-5171` |
| `totrow`, `chi2`                 | forced `TRUE` by `color = "contrib"` (with a `warning()`)                                                     | `tab-resolve.R:146-154`                                  |
| `ref`                            | *required* by a difference colour (abort); and `ci = "diff"` forces `ref = "tot"`                             | `tab-resolve.R:159-165`; `tab.R:5173-5176`               |
| `ref` **meaning**                | reinterpreted by `pct`: a reference **row** under `"row"`, a reference **column** under `"col"`               | `tab.R:1910-1932`                                        |
| `totaltab`                       | forced by `comp = "all"`, in **both** leaves, with warnings                                                   | `tab.R:3919-3927`, `5206-5213`                           |
| `tot_cols_type`                  | forced to `"no_delete"` by `pct`/`ci`/`chi2`/`OR`                                                             | `tab.R:1975-1988`                                        |
| `color`                          | `TRUE` → per-column-type measures; `"auto"` → OR / after_ci / diff / contrib                                  | `tab.R:1050-1073`; `tab-resolve.R:129-141`               |
| `basis`                          | `wt` × design × `design_effect`                                                                               | `survey-design.R:143-151`                                |
| `comp`                           | `"all"` collapses to `"tab"` without `tab_vars`                                                               | `tab.R:1522`                                             |
| `na = "common_base"`             | desugared into `na_drop_all = c(row_var, col_var[1], tab_vars)` + `na = "keep"`                               | `tab.R:738-743`                                          |

**Three consequences of leaving the graph implicit.**

*First, it is stated more than once.* `use_f = c("gaussian","quasipoisson")` appears **four** times
(`tab_reg.R:3090, 3209, 3320`, `reg-assumptions.R:314`); `disp_known = c("binomial","poisson")`
**three** (`tab_reg.R:1339, 1362, 3368`); `c("multinomial","ordinal")` = "not a single glm equation"
**five** times; the glm-family list twice (`tab_reg.R:5351` vs `:5510`). That is **21 hard-coded family
whitelists** outside any fact table, beside three predicates (`reg_fam_binary`/`_prob`/`_logscale`,
`tab_reg.R:161-167`) that prove the pattern was already recognised. Three more one-line predicates —
`reg_fam_glm()`, `reg_fam_overdispersed()`, `reg_fam_disp_known()` — absorb 14 of the 21.

The same disease on the crosstab side: `cleannames`'s `NULL → getOption` rule is written out **four**
times (`tab.R:618, 1748, 3438`, `tab-counts.R:304`), and `conf_level = getOption(...)` as a formal
default **six** times — although `resolve_stars()` / `force_comp()` (`tab.R:1517-1524`) exist precisely
to stop this.

*Second, some of it is dead.* `plain_resolve`'s entire `tot` forcing block with its **six `warning()`
calls** (`tab.R:3859-3894`) **cannot fire from `tab()` or `tab_many()`**, because `tab_transform`
hard-codes `tot = c("row","col")` at `tab.R:2482`. It is reachable only by a direct `tab_plain()`
caller.

*Third, and worst: the boundary is where the user-visible complexity is manufactured.* A user reading
`?tab_reg` sees 30 knobs; four of them (`exponentiate`, `at`, `estimate_display`'s two folds,
`empirical`) are not choices at all.

**The key.** State the graph as data, once, and let both boundaries execute it. Two halves:

**(a) Collapse the derived arguments into the argument they derive from.** For `tab_reg()` — where the
maintainer has explicitly waived back-compat:

```r
effect = c("coefficient", "ame", "mer", "ame_ratio", "mnl_vsrest")   # absorbs `at`
scale  = c("ratio", "link")                                          # absorbs `exponentiate`
# `empirical` becomes a consequence of `color`; estimate_display's prob/ame folds become `effect` values
```

That removes **4 formals and ~8 guard blocks**, and roughly a third of the 32 messages become
unreachable because the illegal combination is unrepresentable. The `effect × at` grid has 6 cells of
which **4 are legal** — exactly the shape that should be an enum.

**(b) Declare the forcings.** The crosstab forcings already live in one function,
`tab_resolve_settings()` (`tab-resolve.R:59-250`), but as a cascade of `case_when`/`recode` over
**measure-name literals**. Folding them into the MEASURES fact table is KEY 4.

**Caveat.** Do not over-collapse. `pct`, `ref`, `comp`, `na`, `ci`, `conf_level` are genuine
independent choices and users set them. The test: *can the value be computed from another argument with
no loss?* — `exponentiate` yes, `pct` no.

**A fourth, smaller symptom worth naming: validation is placed inconsistently.** `na` and `levels` are
checked at the `tab()` boundary (`tab.R:716, 718`) **and again** in `tab_setup` (`:1754`); `ci` is
checked only in `tab_ci` (`:5955`); `pct` only at the leaf (`plain_resolve`, `:3841`); and `totaltab`,
`n_min`, `conf_level` are **never** checked — `totaltab = "tabel"` silently means "no total table"
(`tab.R:4166`).

---

### KEY 4 — A measure does not declare what it needs

**The fact that is missing:** *the requirements and the vocabulary of a colour measure.*

Phase 17d's `MEASURES` table is real and it works: six rows × ~19 facts drive the scoring half of the
engine (`raw`, `scale`, `std_when`, `sig_source`, `bounds`, `gate_row`, `force_policy`), and
`measure_facts()` / `measure_policy()` are a genuine single-accessor discipline that keeps the plan and
the legend on one row. The `by_scale` / `guar` / `force_policy` override mechanism is general in form
(`modifyList` + closures).

But the `/color-mode` skill claims adding a measure is *"one row"*, and that is **wrong end to end**.
The audit found the real checklist: **10 mandatory edit sites across 5 files minimum, rising to ~30
across 8 files** for a comparison-to-another-column measure. The reason is that the table holds the
*arithmetic* and the code still holds the *vocabulary*:

| what is still code                                                              | where                                                                                                                                                                                             |
|---------------------------------------------------------------------------------|---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| **four separate allow-lists** for "is this a legal measure"                     | `fmt_class.R:1361`, `tab.R:906`; for the background channel `tab.R:944` vs `fmt_class.R:1367` — **which disagree**: `color = c("OR","adjustment")` is legal in `tab_reg()` and illegal in `tab()` |
| which build steps a measure forces (`ci`, `chi2`, `totrow`, `ref`, `empirical`) | `tab-resolve.R:116-181`; `tab_reg.R:5256-5275`                                                                                                                                                    |
| the 4-way split `color_diff_OR` / `color_ctr` / `color_ci` / `color_num`        | `tab-resolve.R:202-227`                                                                                                                                                                           |
| which CI method the legend names                                                | `legend_method_name()` `fmt_class.R:4126-4183` — 4 measure literals; **without an arm it silently names a method the bounds were never built with**                                               |
| the legend subject, the "partial test" clause, the honesty caveat               | `fmt_class.R:4229`, `:4661-4663`, `:4376-4381`                                                                                                                                                    |
| the i18n extraction anchor — a hand-maintained duplicate of every `word`        | `fmt_class.R:4203-4204`                                                                                                                                                                           |
| the scale's `center` / `strict` / `std`                                         | `tab_classes.R:3878, 3879, 3884` — three name-keyed lists                                                                                                                                         |
| the jamovi vocabulary                                                           | `jmvtab.a.yaml:112-113,129` + the generated `.h.R`                                                                                                                                                |

**The key.** Move the vocabulary into the table. Each measure row gains:

```r
requires  = c(ci = "diff", ref = TRUE, totrow = TRUE, chi2 = TRUE, empirical = TRUE)
channels  = c("text", "bg")            # eligibility — ONE list instead of four
auto_for  = c(pct = ..., mean = ...)   # the `color = TRUE` / "auto" defaults
method    = <the CI method name the legend prints>
subject   = <the legend's noun>
```

and the scale table gains `center`/`strict`/`std` as columns rather than three name lists.
`names(MEASURES)` then *is* the allow-list, the jamovi list, and the `/color-mode` checklist.

**What it unlocks:** adding a measure genuinely becomes one row plus a translation; the two disagreeing
background allow-lists collapse to one (a live inconsistency, not a hypothetical); and
`legend_method_name`'s silent fall-through — which can print a wrong method name — becomes impossible.

**Two fossils to delete while there.**

*The 4-way colour split.* `color_diff_OR` / `color_ctr` / `color_ci` / `color_num`
(`tab-resolve.R:202-227`) split one resolved colour into four sub-passes because the **pre-2.0.0
pipeline had four steps** (`tab_plain` → `tab_chi2` → `tab_ci` → `tab_num`). Three of those are now one
aggregate core; the fourth is KEY 5. The split is a fossil of a pipeline that no longer exists.

*The legacy vocabulary inside the resolver.* 17d decoded `diff_ci`/`after_ci`/`ci` once at the boundary
and said so — but the *internal* cascade still speaks it (`tab-resolve.R:118, 134-141, 202-227`), so
the decoded pair is re-encoded into a legacy string immediately after being decoded.

**And the boundary is where the 18 spellings live.** `normalize_color_spec()` (`tab.R:899-1029`)
accepts **18 distinct spellings** of `color` — `TRUE`/`FALSE`, six measure strings, `"or"`, `"auto"`,
`"no"`, `""`, `NA`, three legacy composites (`"diff_ci"`, `"after_ci"`, `"ci"` — and `"ci"` is a *pure
synonym* of `"after_ci"`), positional `c(text, bg)`, deprecated `c(text=, background=)`, per-type
`c(pct=, mean=)`, and per-type-×-channel `list(pct = c(...), mean = ...)`. Docs use six of them.

Worse, `color = TRUE` is resolved **twice and the first answer is thrown away**: stage 1
(`tab-resolve.R:129-141`) picks `"OR"`/`"after_ci"`/`"diff"`/`"contrib"`, then stage 2
(`resolve_col_measures`, `tab.R:1063-1073`) repaints per column and **overwrites everything except
`OR` and `contrib`**. Stage 1 survives only for its *side effects* — forcing `chi2`, `totrow`, `ci` —
which is exactly what `requires` would declare. Two further facts (`color_signif`, `color_ratio_ci`)
must be threaded *beside* the colour string because the legacy encoding cannot carry them.

---

### KEY 5 — The build still has a second pass

**The rule that is not honoured:** *2.0.0's own keystone — "the step chain collapses into one aggregate
core".*

Phase 17f quarantined `tab_pct`/`tab_tot`/`tab_totaltab` into `R/tab-steps-legacy.R`. But **`tab_ci()`
and `tab_chi2()` are still live steps that run on the assembled table**, inside `tab_apply_tests()`
(`tab.R:7275-7304`). They do not receive the plan; they **re-derive the structure from fmt markers**:

```r
get_vars   <- tab_get_vars(tabs)                  # tab.R:5978  — re-read the variable roles
ci <- purrr::map_chr(tabs, ~ ci[get_col_var(.)])  # tab.R:5987  — re-recycle per column
comp       <- tab_validate_comp(tabs, ...)        # tab.R:5997  — re-validate comp
tot_cols   <- detect_totcols(tabs)                # tab.R:6000  — re-find the total columns
ref_cols   <- detect_refcol(tabs)                 # tab.R:6009  — re-find the reference column
```

Every one of those facts was known in the leaf, 1 500 lines earlier.

**What the second pass costs, measured:**

1. **A whole re-derivation chain** — `detect_totcols` / `detect_refcol` / `detect_firstcol`
   (`fmt_class.R:2197-2247`) plus an 8-branch `case_when` exist only to reconstruct the plan.
2. **The jamovi tier-3 re-reference has to re-run `tab_ci()` on a carrier** (`jmvtab-cache.R:826`) —
   the cache's most delicate path exists in that shape because the CI is not part of the build.
3. **`ci_scale`, `degf`, `conf_level` and the CI-method vector all had to be re-plumbed into `tab_ci()`
   by hand** (z14, z16), each time re-discovering that the step has no access to the spine. `tab_ci()`
   even folds `ci = "ratio"` → `ci = "diff"` + `ci_scale = "ratio"` a *second* time (`tab.R:5959-5964`)
   so it can be "a self-contained entry point too" — the same rule as `tab-resolve.R:71-72`.
4. **`tab_ci()` re-resolves `stars` from the option** (`tab.R:5965`) and `num_core` does it again
   (`tab.R:5551`), although `tab_setup` already resolved it (`tab.R:1751`).
5. **A latent metadata hazard.** The step tails call `new_tab(tabs, subtext, test)` with **no `meta`
   argument** (`tab.R:6244, 6255, 6366, 6377`; same in `tab-steps-legacy.R:148, 365, 369, 671, 673`).
   *Verified by running it: `meta` survives anyway*, because `tibble::new_tibble()` preserves the
   incoming object's attributes — `tab_plain() |> tab_chi2() |> tab_ci()` keeps `vars`, and a
   `set_caption()` survives the chain. So this is **not** a live bug. But it is undesigned: the same
   shape of code is what silently dropped `meta$inference` in z16-iv and the whole `meta` in
   `tab_spread()` and `reg_build()`'s split branch in z16-iiiii. Passing `meta` explicitly costs six
   lines and removes the class.

**The key.** Compute the interval and the test **where the plan is** — in the leaf, from the aggregate
— and keep `tab_ci()`/`tab_chi2()` as *superseded public wrappers* that reconstruct a plan from markers
for the exported step path only, exactly as `tab_pct()` now is. The `.fine`/`tab_counts` seam already
proves a leaf can be driven from a pre-aggregate.

**Caveat, and it is real.** The ordering invariant (`tab_chi2` and `tab_ci` are independent, but the
non-first-level drop must happen after both) and the jamovi tier-3 carrier both depend on today's
shape. This is the key with the highest ratio of *behaviour that must not move* to *lines deleted*. It
should be sequenced **after** KEY 1 (which gives the leaf the row identity it needs) and gated on the
`test-jmvtab-cache.R` cold+warm+reref lock staying green.

---

### KEY 6 — One table, two identities

**The fact that is missing:** *what kind of table this is, and which variables are in it* — stated
once, for both producers.

A crosstab records its variables in `meta$vars` (`row_vars`, `col_vars`, `tab_vars`, `wt`, `caption`,
`var_labels`, `row_roles`). A regression records **none of that** — measured: a `tab_reg()` table's
`meta` is `list(empirical_tips, assumptions, ci_settings, reg_meta)`, **with no `vars` at all**.
Instead `reg_meta` carries a parallel vocabulary — `dependent`, `predictors`, `split_var`,
`predictor_types`, `model_labels`, `families`, … **20 fields**.

And the *kind* of table is not stored either. `is_reg_footer()` (`tab-test-display.R:232-234`) decides
"is this a regression" by asking whether the `test` tibble happens to **contain a reg-flavoured
discriminator** — even though the same file's header comment says *"a reg table carries `reg_meta`"*.
Two encodings of one fact, in one file, one of them unused.

**Symptoms measured:**

- **Five `reg_meta` fields have no production reader at all** (`shape`, `model_labels`, `conf_level`)
  or only tests (`predictor_types`, `multiplier`). `conf_level` is separately stamped on every column
  by `tab_stamp_inference`, so `reg_meta$conf_level` is a stale duplicate of a per-column attribute.
- The `test` tibble carries **14 columns**, of which the crosstab arm writes 13 and the reg arm 10 —
  two schemas in one tibble, reconciled by `vec_rbind` and kept apart by a discriminator vocabulary
  spread over four files.
- Exporters that need "which variable is on which axis" (`var_labels` swap, caption, the merged
  `row_var` column, transpose) get an answer for crosstabs and a fallback for regressions.
- `spread_models` — the reg twin of `spread_vars` — is the least-read public formal in the package
  (**7 code sites**), and `tab_spread()` had to be taught reg tables separately: `reg_spread_models()`
  must **re-key the GOF block by hand** (`tab_reg.R:3540-3554`) because `tab_spread` pivots data only.

**The key.** One `meta$spec` per table, with a `kind` and a uniform variable model:

```r
meta$spec = list(
  kind = "crosstab" | "regression",
  vars = list(rows = …, cols = …, groups = …, weight = …, labels = …),   # uniform
  call = list(…)                                                          # the producer's own settings
)
```

`reg_meta` becomes `spec$call` for `kind = "regression"`; `ci_settings` becomes `spec$call$inference`;
`is_reg_footer()` becomes `spec$kind == "regression"`. `reg_check_plots()`'s `fit_spec` (~4 KB of
strings — the one genuinely good idea in `reg_meta`) generalises: **a table remembers how it was
made**, which is what a future `reg_plot()` (`dev/regression_effect_plots.md`) and any
"recompute at a different reference" path both want.

**What it unlocks:** every consumer that today branches on table kind reads one field; the caption /
footer / label / i18n paths stop having a crosstab arm and a regression fallback; and — with KEY 1 —
the reg table stops being structurally exceptional at all.

---

### KEY 7 — The shape of what comes back is a function of the arguments

**The rule that is not stated:** *what `tab()` returns.*

Measured, from one function:

| call                                                | class                                         |
|-----------------------------------------------------|-----------------------------------------------|
| `tab(d, marital, race)`                             | `tabxplor_tab`                                |
| `tab(d, c(marital, relig), race)`                   | `tabxplor_grouped_tab`                        |
| `tab(d, marital, race, tab_vars = black)`           | `tabxplor_grouped_tab`                        |
| `tab(d, c(marital, relig), race, tab_vars = black)` | **`tabxplor_tabs` (a list)**                  |
| `tab(…, output_list = TRUE)`                        | `tabxplor_tabs`                               |
| `options(tabxplor.output_kable = TRUE)`             | **changes the merge decision** (`tab.R:2726`) |

The last row deserves emphasis: a *display* option, read inside a *build* stage, changes the **shape of
the returned object**. The fourth row is KEY 1's limit surfacing as an unpredictable return type.

**And the plurality is systemic.** The package has **nine ways to say "more than one table"**, six of
them distinct object shapes: a merged/compacted tab, a grouped tab (`tab_vars`), a bare list, a
`tabxplor_tabs`, spread columns, a transposed table (in **two** implementations — object-level
`tab_transpose()`, soft-deprecated, and render-level `tx_transpose_render()`), `split_var` in
`tab_reg`, several dependents in one table, and several dependents × a models list. Which operation
supports which shape is **written down nowhere** and enforced by five separate abort sites (§KEY 1.6).

`tabxplor_tabs` in particular carries **exactly one behavioural bit** outside printing —
`tab_xl.R:214`, `sheets = "tabs"` — plus four printing methods.

Downstream, **14 reachable export entry points, 10 exported**, including two pure aliases
(`tab_kable ≡ tab_html`, `tab_md_css ≡ tab_css(chrome = FALSE)`), two standalone-page builders
(`tx_kable_page` vs `tab_html_string`) and a `tab_export()` facade whose `format` argument does not
accept the value its own documentation names (`tab-export.R:26` still says `"kable"`).

**The key.** Three rules, all cheap:

1. **One documented crosstab entry point.** `tab()` for everything; `tab_many()` finishes its
   deprecation as a one-line shim; `tab_plain()`/`tab_num()` get a superseded badge and stop mirroring
   `tab()`'s formals (they already *are* wrappers over the cores since 17f — `tab.R:3828`, `:5148`);
   `tab_counts()` stays public because its **inputs** genuinely differ. That removes ~68 formals of
   drifting mirror surface and the four-spelling problem (§2.2).
2. **A predictable return.** Either always a `tabxplor_tab` (once KEY 1 lets `tab_vars` and several
   `row_vars` compose), or `output_list` as the *only* thing that changes the shape. Remove the
   option's ability to change it.
3. **One capability predicate.** `tab_shape(x) ∈ {single, merged, grouped, list}` + a supported-ops
   table, read by all five abort sites.

---

## 4. Subsystem map — the remaining simplifications

Ranked within each subsystem by (payoff ÷ churn). "BI" = byte-identical target.

### 4.1 The fmt record and the colour engine

| # | item                                                                                | evidence                                                                                                                                                                                       |
|---|-------------------------------------------------------------------------------------|------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| 1 | **E1: derive the 4 reconstructors from `fmt_col_attrs` + declared reconcile rules** | `fmt_class.R:5152, 5266, 5451, 5608` enumerate 14 attrs by hand; the leaves pass 9 of 14. BI. *Unblocks KEY 2/4/6*                                                                             |
| 2 | `estimand` attribute + the `type` split (KEY 2)                                     | `fmt_class.R:2015, 3277-3283, 3310, 3543`                                                                                                                                                      |
| 3 | MEASURES gains `requires`/`channels`/`auto_for`/`method`/`subject` (KEY 4)          | 4 allow-lists, 2 of which disagree. BI                                                                                                                                                         |
| 4 | scale table: `center`/`strict`/`std` become columns                                 | `tab_classes.R:3878, 3879, 3884`. BI                                                                                                                                                           |
| 5 | the legend stops re-deriving `is_coef`/`is_mean`/`is_pct`/`is_logcoef`/`is_std`     | `fmt_class.R:4631-4640` duplicates the plan's `:3310, 3315, 3324` — and **`is_std` is computed from a different scale than the plan uses**, a latent divergence that happens to agree today    |
| 6 | delete `set_tot_n`, `set_n_eff`, `set_model_family`, `get_ref_means`, `get_ref_pct` | zero callers                                                                                                                                                                                   |
| 7 | one `fmt_base(x)` accessor for the `n_eff → tot_n → n` coalesce                     | written out at all 5 read sites, `tab.R:6106-6116`                                                                                                                                             |
| 8 | one `display` token registry                                                        | `get_num`/`set_num` are a hand-written 22-arm map (`fmt_class.R:494-585`); **the roxygen documents 11 of the 22 tokens**. 17d deferred this — only the `get_num`/`set_num` half is a clean map |

### 4.2 The build pipeline

| #  | item                                                                                                                                                           | evidence                                                                                                                                                                                                                                                 |
|----|----------------------------------------------------------------------------------------------------------------------------------------------------------------|----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| 1  | the settings spine becomes the **only** interface (delete the 20+ flat duplicates)                                                                             | written `tab.R:2090-2109`, read only at `:1659, 1660, 1685`; every consumer reads the flat copy at `:2114-2127`                                                                                                                                          |
| 2  | `na_text`/`na_num`/`lvs` join `pairs`/`cols`                                                                                                                   | the spine comment at `tab.R:2083-2084` **promises** `na` is there; `tab_prepare_pop` never touches `settings`, and `settings$cols$lvs` is **stale** the moment prepare_pop re-resolves levels — and it is the stale copy that is shipped to every worker |
| 3  | `new_ctx()` declares all ~83 keys, or the stage products move out of ctx                                                                                       | 53 declared vs ~83 live; `pct_vect`/`ref_vect` undeclared while sibling `OR_vect` is — making the guard at `tab.R:2401` **unreachable**                                                                                                                  |
| 4  | 17 ctx fields read by exactly one stage → locals                                                                                                               | the five `*_quo`, `with_filter`, `other_level`, `totaltab_name`, `n_min`, `spread_vars`, `names_prefix`, `names_sort`, `add_n`, `add_pct`, `common_totrow`, `agg_only`, `ci_method`, `design_effect`                                                     |
| 5  | shared `leaf_head()` + `leaf_finish()`                                                                                                                         | the inference/basis preamble and the **~30-line result tail** are the same code twice (`tab.R:3947-3970` ≈ `5235-5274`; `4479-4536` ≈ `5759-5791`)                                                                                                       |
| 6  | `num_core`'s ~90 inline lines of moment-sum totals → `build_total_rows`/`finalize_total_rows`                                                                  | `tab.R:5353-5442` vs the shared `:4169-4200`                                                                                                                                                                                                             |
| 7  | `num_core` records `meta$vars`                                                                                                                                 | `tab.R:5773, 5777`                                                                                                                                                                                                                                       |
| 8  | pass `meta` explicitly in the step tails                                                                                                                       | `tab.R:6244, 6255, 6366, 6377` — currently safe only by accident (§11 D3)                                                                                                                                                                                |
| 9  | delete `tab_assemble()` (no caller), `ctx$levels_order` (never read in `tab.R`)                                                                                | `tab.R:2548-2550`, `:1469-1506`                                                                                                                                                                                                                          |
| 10 | `resolve_cleannames()` beside `resolve_stars()`/`force_comp()`                                                                                                 | the rule is written 4×                                                                                                                                                                                                                                   |
| 11 | `inference = new_inference()` as a **lazy default** on `plain_core`/`num_core`/`tab_apply_tests` silently re-reads the option if a caller forgets the argument | `tab.R:3941, 5227, 7277` — make it required                                                                                                                                                                                                              |
| 12 | delete `plain_resolve`'s dead `tot` forcing block (6 unreachable `warning()`s)                                                                                 | `tab.R:3859-3894` vs the hard-coded `tab.R:2482`                                                                                                                                                                                                         |

### 4.3 The export stack

| #  | item                                                                         | evidence                                                                                                                                                                                                                                                                                                                                    |
|----|------------------------------------------------------------------------------|---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| 1  | **transpose builds `rd2` by modifying `rd`, not by literal reconstruction**  | `tab-transpose-render.R:239-277` is a 39-slot literal; it has **already lost slots twice** (fixed at `:271-273`) and is **losing `ann$keep_black` today**, masked by a silent fallback at `tab-render-html.R:430` — so a transposed reg table's footer rows are wrongly greyed in HTML and nothing tests it                                 |
| 2  | delete the dead model slots                                                  | `range_totcol` (hard-coded NULL at `tab-export-prep.R:510`, with a live 45-line producer `tab_totcol_range()` kept alive only by its own unit test), `vars$col_vars_levels` (no reader), `roles$no_totrows` (assigned then dropped), `ann$anchor` (prep-internal but shipped)                                                               |
| 3  | xl reads `rd$subtext` and `rd_caption()`                                     | it re-derives both (`tab_xl.R:233-234, 249-268`) and rebuilds `start_col_var` (`:445-446`)                                                                                                                                                                                                                                                  |
| 4  | md reads `roles$new_col_var`                                                 | it rebuilds it (`tab_md.R:262-273`) and re-blanks headers the prep already blanked (`:375-378`)                                                                                                                                                                                                                                             |
| 5  | **`theme = "print"` renders `kable_material_dark` on the kableExtra engine** | `tab-render-html.R:294-309` — a black table for the black-and-white publication palette; the comment claiming "only light/dark here" is stale since z11                                                                                                                                                                                     |
| 6  | one `"auto"` downgrade                                                       | re-implemented **five** times with different rules (`tab-export-prep.R:748`, `tab_classes.R:923-929`, `tab-css.R:100-104`, `tab-render-html.R:667`, `tab_reg_plots.R:48`), and the theme **option pair** differs between the export path and the console path — so a footer rendered outside `rd_footer()` silently picks the console theme |
| 7  | one number-font decision                                                     | three options (`tab_kable_num_font`, `xl_font_num_stars`, `plot_num_font`) answer "what font do starred numbers use", although `roles$has_stars` is already in the model                                                                                                                                                                    |
| 8  | `tab_plot` honours the footer model's typography                             | `tab_classes.R:2151` forces `face = "bold"` on every legend token, discarding the print palette's italic/underline — the one backend that overrides rather than translates                                                                                                                                                                  |
| 9  | two definitions of "is this coloured" inside one function                    | `roles$color_cols` vs `md_has_color()` (`tab_md.R:243` vs `:335`); and `color_cols`/`any_bg` are defined differently in the prep and the transpose                                                                                                                                                                                          |
| 10 | the `" [dep]"` strip regex is written twice, each commenting on the other    | `tab-export-prep.R:646`, `fmt_class.R:4774`                                                                                                                                                                                                                                                                                                 |

### 4.4 tab_reg

| # | item                                                                                                                            | evidence                                                                                                                                                                                                                                      |
|---|---------------------------------------------------------------------------------------------------------------------------------|-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| 1 | **collapse `at` → `effect`, `exponentiate` → `scale`, `estimate_display{prob,ame}` → `effect`, `empirical` ← `color`** (KEY 3a) | removes 4 formals and ~8 guard blocks of `tab_reg.R:5089-5178, 5256-5262`                                                                                                                                                                     |
| 2 | three family predicates (`reg_fam_glm`, `_overdispersed`, `_disp_known`)                                                        | absorbs 14 of the 21 hard-coded whitelists                                                                                                                                                                                                    |
| 3 | `reg_build`'s four parallel assemblers                                                                                          | AME `:3768-3826`, MNL-vs-rest `:3827-3846`, coefficient `:3847-3877`, and the **complete duplicate** split branch `:3620-3699` with its own `new_tab`/`meta`/`tab_stamp_inference` — which has already drifted once (comment at `:3676-3690`) |
| 4 | the `shared` bag → a typed record                                                                                               | 24 keys, documented as 20 (`:3598-3603`), mirrored in `fmt_class.R:55-58` to silence `R CMD check`, partially re-listed twice (`:3904, 3911`), with 2 fields declared nowhere                                                                 |
| 5 | drop the 5 dead `reg_meta` fields                                                                                               | `shape`, `model_labels`, `conf_level` (no reader); `predictor_types`, `multiplier` (tests only)                                                                                                                                               |
| 6 | one `stats`/`check` vocabulary                                                                                                  | `tab_reg(stats=)` and `reg_check_plots(check=)` are the same `names(REG_CHECKS)` with two argument names and two validators                                                                                                                   |
| 7 | **the `.fit_cache`/`reref` path is ~450 lines and an 11-conjunct correctness predicate**                                        | `tab_reg.R:5343-5356` + `reg_build_digest` + `reg_reref_fit_res` + the digest tier. Exists only for the jamovi live UI, and **silently shows fewer footer rows** than the R path (every check returns NULL without a fit). See §10 Q4         |

### 4.5 jamovi

The **kernel is the good part**: one store lifecycle, one LRU, two configs (`JMVTAB_CFG` 3 tiers /
`JMVREG_CFG` 2 tiers), `resolve_ci_method`/`normalize_color_spec`/`finalize_color_tail` reuse, the
`jmv_backend_*` extraction, and `test-jmvtab-cache.R` locking byte-identity. **No statistics are
forked.** What is duplicated is the **boundary**:

| # | item                                                                                                                                     | evidence                                                                                                                                                                                                                                                                       |
|---|------------------------------------------------------------------------------------------------------------------------------------------|--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| 1 | the stars→`ci="diff"` forcing is re-implemented **verbatim**                                                                             | `jmvtab-cache.R:990-992` mirrors `tab-resolve.R:167-182`; the comment admits it                                                                                                                                                                                                |
| 2 | the digits magnitude floor is **byte-duplicated**                                                                                        | `jmv_reapply_digits()` `:923-938` vs `tab.R:5723-5727`                                                                                                                                                                                                                         |
| 3 | `jmv_population_descriptor()` is a line-for-line copy of `tab_cache_keys()`'s branch — **in the same file that also reads the real one** | `:638-646` vs `tab-resolve.R:261-273`; the real one at `:288`                                                                                                                                                                                                                  |
| 4 | family detection exists **three** times                                                                                                  | `reg_detect_family()` (R), `jmvtab_reg_dep_family()` fallback (R), and **`detectFamily()` in JS** (`jamovi/js/jmvtabreg.js:396-405`) with its own "matches the R side exactly" note. Ditto `familyOptionsFor`/`anyProbScale` vs R's own aborts                                 |
| 5 | the trials-max rule is duplicated **with a semantic shift**                                                                              | R takes `max()` only when the user asks (`trials = TRUE`, `tab_reg.R:5213`); jamovi takes it silently for any integer outcome (`jmvtabreg-cache.R:214-227`)                                                                                                                    |
| 6 | the multiplier keyword set is copied                                                                                                     | `jmvtab_reg_mult_vector()` `:166-177` vs `reg_multiplier_value()` `tab_reg.R:355-368`                                                                                                                                                                                          |
| 7 | `anova` is the last option travelling as a **global**                                                                                    | `jmvtab.b.R:34-36` (`options()` + `on.exit`); `design_effect` was already converted to an argument. Converting `anova` too shrinks `.run()` to weights → build → render, and removes a stale-cache hazard (it is baked at build and covered only indirectly, via `structural`) |
| 8 | the jamovi `display` vocabulary ≠ `tab(display=)`'s                                                                                      | the ComboBox offers `pct_ci`/`mean_ci`/`OR`/`OR_pct`, which `validate_display_template()` would reject — so `jmv_apply_display()` cannot call the shared `tab_apply_display()`. An honest structural gap, not an oversight — and the source of §11 D11                         |
| 9 | dead / dangling                                                                                                                          | `jmvtab_reg_staged()` exists so the staged predicate matches, and `jmvtabreg.b.R:36` **inlines the predicate instead** (only tests call it); `jmvtab_reg_build()` passes `stats = opts$stats`, which `.opts()` never sets                                                      |

### 4.6 tab_counts

`tab_counts()` does the hard part right: it enters at the `.fine` seam, reuses `new_ctx`/`tab_setup`/
`tab_build_tables`/`finalize_color_tail`, and forks no statistics — the design its header claims, and
it holds. The remaining weight is that **~50 % of the file is a shape dispatcher** and the constructor
re-does `tab()`'s boundary: **8 rules copy-pasted**, ~15 lines (`chi2`→`test` deprecation, `cleannames`
NULL→option, `vec_assert`s, `tot` validation + `"both"` expansion, `tot`→`(totrow, totcol)`,
`total_names` recycling — which `tab_setup()` then does *again*). None drift today; nothing prevents it,
and the two boundaries are already asymmetric in ways a reader cannot distinguish from deliberate.

The natural fix is the one already applied to the ctx: a `tab_resolve_common_args()` returning the
resolved `(color_spec, ci_method, cleannames, totrow/totcol, total_names, test)` bundle, called by
`tab()`, `tab_many()`, `tab_num()` and `tab_counts()`.

Its limits are half-gated: three are explicit runtime aborts (survey design, non-integer counts,
`na = "drop_all"`) and `design_effect` is genuinely single-sourced through `agg_only`; the rest are
enforced by **argument omission plus one roxygen paragraph** — and a user passing
`ci_method = c(mean_diff = "student")` gets silent acceptance and no effect.

---

## 5. White elephants — the honest list

"Cut" = free now (unreleased or internal). "Deprecate" = CRAN etiquette. "Keep" = suspicion checked and
dismissed.

| item                                                                           | evidence                                                                                                                                                                                                                                                                                                                                                                                                                                        | verdict                                                                                                             |
|--------------------------------------------------------------------------------|-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|---------------------------------------------------------------------------------------------------------------------|
| **`filter`**                                                                   | **0 uses in every corpus**; its documented use case is tested without it; and its cache key is hardcoded `NA_character_` (`tab.R:2066`), so a filter **never invalidates the jamovi cache**                                                                                                                                                                                                                                                     | **cut** (already doc-superseded) — or at minimum fix the cache key                                                  |
| `names_prefix`, `names_sort`                                                   | 0 uses anywhere, on both `tab()` and `tab_counts()`                                                                                                                                                                                                                                                                                                                                                                                             | **cut**                                                                                                             |
| `levels = "auto"`, per-col_var `levels`/`digits` vectors                       | 0 uses                                                                                                                                                                                                                                                                                                                                                                                                                                          | **cut the grammars**, keep the scalars                                                                              |
| `tab_many()`'s 41 formals                                                      | soft-deprecated already, but the only place `chi2`/`totrow`/`totcol` survive; `na_drop_all`/`compact`/`totrow` have 0 uses                                                                                                                                                                                                                                                                                                                      | **finish it**: `function(...) tab(..., output_list = TRUE)` + the deprecation                                       |
| `tab_plain()` / `tab_num()` as documented API                                  | 0 vignette uses; literally wrappers over the cores since 17f                                                                                                                                                                                                                                                                                                                                                                                    | **supersede** (keep exported, badge, stop mirroring `tab()`'s arguments)                                            |
| `sup_cols`                                                                     | tests only (9 hits); mechanically identical to `col_vars` + `levels = "first"` + `pct = "row"`, and the code says so                                                                                                                                                                                                                                                                                                                            | keep the deprecation, stop mirroring it                                                                             |
| `totcol = "each"` / `"all_col_vars"`                                           | tests only. Worse: the parser returns a **character** for `"last"` and a **list** for the others, so the `identical()` comparisons never fire for the default and **`tot_cols_type == "some"` is the default arm**; `"all_col_vars"` as an *input value* can never produce `tot_cols_type == "all_col_vars"`; and the string `"all_col_vars"` carries **two unrelated meanings** (the total-column tag and the add_n/add_pct helper-column tag) | **cut `"each"`** (≈4 lines) and the unreachable `"all_col_vars"` handler (10 lines); rename one of the two meanings |
| `tab_totcol_range()` + `range_totcol`                                          | producer alive, consumer commented out, kept by its own unit test                                                                                                                                                                                                                                                                                                                                                                               | **cut both** (the option is DORMANT since x2)                                                                       |
| `ctx$levels_order`                                                             | read by no stage in `tab.R` (only `jmvtab-cache.R:405`)                                                                                                                                                                                                                                                                                                                                                                                         | **cut from ctx**, pass directly                                                                                     |
| `tab_assemble()`                                                               | no caller anywhere                                                                                                                                                                                                                                                                                                                                                                                                                              | **cut**                                                                                                             |
| `set_tot_n`, `set_n_eff`, `set_model_family`, `get_ref_means`, `get_ref_pct`   | zero callers                                                                                                                                                                                                                                                                                                                                                                                                                                    | **cut**                                                                                                             |
| `jmvtab_reg_staged()`                                                          | its own caller inlines the predicate instead                                                                                                                                                                                                                                                                                                                                                                                                    | **cut or adopt**                                                                                                    |
| `complete_partial_totals`, `set_ci_type`, `tab_get_wrapped_dimensions`         | exported, zero use anywhere                                                                                                                                                                                                                                                                                                                                                                                                                     | maintainer call (personal tooling?)                                                                                 |
| `reg_meta$shape`, `$model_labels`, `$conf_level`                               | no reader                                                                                                                                                                                                                                                                                                                                                                                                                                       | **cut**                                                                                                             |
| `estimate_display = "prob"/"ame"`, `at`, `exponentiate`                        | duplicate `effect`/`family`; degraded away in 3 blocks / 8 sites                                                                                                                                                                                                                                                                                                                                                                                | **fold** (KEY 3a)                                                                                                   |
| `color = "ci"`                                                                 | a pure synonym of `"after_ci"`                                                                                                                                                                                                                                                                                                                                                                                                                  | **cut** (both already legacy-decoded)                                                                               |
| `tabxplor_tabs`                                                                | one behavioural bit (`tab_xl.R:214`) + four print methods                                                                                                                                                                                                                                                                                                                                                                                       | keep, but do not grow it; the bit could key on `!is.null(names(x))`                                                 |
| `spread_models`                                                                | 7 code sites, least-read public formal                                                                                                                                                                                                                                                                                                                                                                                                          | keep (the reg twin of `spread_vars`), re-key on KEY 1                                                               |
| `tab_md_css()`                                                                 | one-line alias of `tab_css(chrome = FALSE)` whose `tabs` argument is documented as ignored                                                                                                                                                                                                                                                                                                                                                      | keep as alias, drop the argument                                                                                    |
| `tab_kable()`                                                                  | pure alias of `tab_html()`                                                                                                                                                                                                                                                                                                                                                                                                                      | keep (maintainer's explicit z-g decision)                                                                           |
| kableExtra engine                                                              | legacy — and the one that breaks under `theme = "print"`                                                                                                                                                                                                                                                                                                                                                                                        | keep + **fix D2**                                                                                                   |
| `tabxplor.output_kable`                                                        | changes the **shape of the built object** from a display option                                                                                                                                                                                                                                                                                                                                                                                 | keep the option (maintainer ruling), **remove its build-shape power**                                               |
| the 3 number-font options                                                      | one decision, three knobs                                                                                                                                                                                                                                                                                                                                                                                                                       | merge to one, alias the others                                                                                      |
| `method = "profile"`, `quasipoisson`, the compound-formula hatch, `mnl_vsrest` | previously settled                                                                                                                                                                                                                                                                                                                                                                                                                              | keep                                                                                                                |

**And the converse — cold but good, do not cut:** `tab_counts()`, `tab_css()`, `transpose=`, `n_min=`,
`split_var=`, `score_from_lv1()`, `common_totrow`, `tab_compact()`'s cross-call merge (which is the one
thing `tab()`'s built-in merge cannot express, and it is undocumented — its own `@examples` shows the
case `tab()` now does by default).

---

## 6. Fields and attributes — the verdicts

**Add (2):**

| what                                               | why                                                                                                                                             | cost                    |
|----------------------------------------------------|-------------------------------------------------------------------------------------------------------------------------------------------------|-------------------------|
| column attribute `estimand = (field, scale, null)` | KEY 2 — deletes `fmt_est_field`, the `var`-sniff, the `ci_mult`/`ci_neutral`/`has_ci`/`sd_ref` re-derivations and the `gof` colour special case | one attribute, after E1 |
| a **row index model** (keyed, not positional)      | KEY 1                                                                                                                                           | structural, see §9      |

**Change (2):**

| what                                        | to what                                                |
|---------------------------------------------|--------------------------------------------------------|
| `type` (8 values, two jobs)                 | `pct_base` × `col_kind`                                |
| `meta` (crosstab) + `reg_meta` (regression) | one `meta$spec` with `kind` + a uniform `vars` (KEY 6) |

**Remove (0 fields, 3 meta sub-fields):** `reg_meta$shape`, `$model_labels`, `$conf_level`.

**Do not merge any field.** `diff`/`or`/`ratio` are provably a discriminated union, and `$` can serve
derived fields (the `$ci` precedent, `fmt_class.R:3162-3181`) — but z6 already measured the payoff
(≈0.03 % of a build; ~92 KB on the largest realistic table), and the estimand attribute captures the
*structural* benefit without touching storage, the goldens, or the user contract. Likewise
`n`/`wn`/`tot_n`/`n_eff` are four slots for ~two facts, but the fix is **one accessor** (`fmt_base(x)`
folding the `coalesce` written out at all five read sites), not a merge.

**One field-adjacent smell worth naming:** `fmt_gap_force_policy()` uses `all(is.na(get_gap_se(x)))` as
a boolean *"does this column have a gap test?"* (`fmt_class.R:2135`) — a **column-level fact encoded as
an all-NA per-cell field**. It works, and it is elegant in its way; with `estimand` in place it becomes
a declared fact.

---

## 7. Anti-propositions — what NOT to do

- **Do not route regression columns through the aggregate core.** Restated from the 2026-07 audit and
  still right. The genuinely shared things — the nine `ci_*` engines, the fmt record, the colour
  engine, the legend, the footer, the exporters — **are already shared** (measured: `ci_pivot`,
  `ci_katz_rr`, `ci_mean_diff2`, `ci_mean_ratio`, `ci_or`, `ci_wilson` all have callers in both
  `tab.R` and `tab_reg.R`). What is not shared is *table assembly*, and that is correct: a fit has no
  count aggregate.
- **Do not go sparse on the record.** z6 measured it; nothing has changed except +1 field.
- **Do not merge fmt fields** (§6).
- **Do not replace the S3-per-verb model.**
- **Do not force `pillar_shaft` through the render model** — the console owns its layout.
- **Do not re-open the settled perf verdicts** (scan fusion, chi2 marshalling, the `.fine` seam).
- **Do not "fix" the four label-block shapes by adding a fifth.** KEY 1 is worth doing only if the
  other four are migrated.
- **Do not delete `tab_ci()`/`tab_chi2()` as exported functions.** They are CRAN-public and the step
  path is documented. Supersede them; move the computation.
- **Do not move the jamovi JS rules into R.** They exist for latency (grey a control before the
  round-trip), which is a real requirement. But the *rule* should ship as a small table **generated
  from R**, not hand-mirrored in a language with no test harness in this repo.

---

## 8. Caveats and risks

1. **`meta` sub-fields are optional by contract.** `test-degraded-attrs.R` locks that a table which
   lost `subtext`/`test`/`meta` still prints and exports. Any new stored fact (KEY 1's rows, KEY 6's
   spec) must degrade to today's heuristic, not error. The heuristics must therefore stay as
   *fallbacks*, clearly marked — which is what 17c did, and it worked.
2. **Column attributes are required, not optional** — a standalone extracted column must format and
   colour itself. So `estimand` needs a default (`c(field = "diff", scale = "additive", null = 0)`)
   that reproduces today's behaviour for every existing column.
3. **The jamovi cache is the tripwire for KEY 2 and KEY 5.** Both change what a carrier stores; both
   need a schema bump (`JMVTAB_CACHE_SCHEMA`, currently **12**) and the cold+warm+reref lock.
   `reg_reref_fit_res`'s byte-identity is a hard contract. Note the schema discipline is *fragile*:
   nothing mechanically ties the constant to the payload's structure, and bumps 8, 9 and 12 all
   happened for exactly this reason.
4. **i18n**: anything that changes a legend string needs `po/R-fr.po` + `.mo` recompiled, and the
   `if (FALSE) gettext(...)` extraction anchor (`fmt_class.R:4203`) is itself a hand-maintained
   duplicate — a measure fact table that carries `word` should *generate* that anchor, not shadow it.
5. **Golden discipline.** KEY 1 and KEY 6 move *structural* goldens without moving rendered output.
   `dev/verify_golden_field_delta.R` has been taught to prove a field delta, an attribute delta, a
   `test`-column delta and a `meta` sub-field delta; it will need "a row-index block delta" next.
6. **The parallel session.** This audit ran against a working tree under concurrent edit. Re-grep every
   line reference before acting.

---

## 9. Sequencing

**Free now, byte-identical, no design risk** (several are prerequisites):

- **E1** — derive the four reconstructors from `fmt_col_attrs` (§4.1.1). *Unblocks KEY 2, 4, 6.*
- The §11 defect list — **D9 first** (a shipped jamovi control that does nothing).
- The dead-weight cuts (§5 "cut" rows).
- The 4× `cleannames` / 6× `conf_level` single-sourcing; `inference` as a required argument; `meta`
  passed explicitly in the step tails.

**One phase each, contained:**

- **KEY 4** — MEASURES gains its vocabulary; the four allow-lists collapse to one; the
  `color_diff_OR`/`color_ctr`/`color_ci`/`color_num` fossil dies. BI target.
- **KEY 3a** — the `tab_reg()` argument collapse (back-compat explicitly waived) + the three family
  predicates.
- **KEY 2** — `estimand` + the `type` split. Needs E1.
- **KEY 6** — `meta$spec`. Needs E1; pairs naturally with KEY 3a (both touch `reg_meta`).

**Structural, in this order:**

- **KEY 1** (the row model) — two to three sessions. The enabler for KEY 5 and for the
  `tab_vars` × several-`row_vars` composition, and what makes `tab_reg()` stop being structurally
  exceptional.
- **KEY 5** (move CI/test into the leaf) — after KEY 1, gated on the jamovi lock.
- **KEY 7** — mostly falls out of KEY 1 (the return shape); otherwise a doc + deprecation pass.

**Relative to the release.** Everything that changes a *public argument* or a *return class* is free
only until the 2.0.0 CRAN release: that is KEY 3a (regression — already waived), KEY 7's entry points,
and the §5 argument cuts. Everything else is internal and can land after.

---

## 10. Decisions for the maintainer

**Q1 — the row model (KEY 1): now, or after the release?**
The highest-leverage item, and *internal* (rendered output does not move), so not release-gated. But
also the biggest. Recommendation: **do E1 + the free items now**, and schedule KEY 1 as its own phase
with the transpose and compact migration folded in — doing it without them creates a fifth
representation.

**Q2 — how far to collapse the crosstab entry points (KEY 7.1)?**
(a) status quo; (b) `tab_many()` becomes a one-line deprecated shim and `tab_plain`/`tab_num` get a
superseded badge and stop mirroring `tab()`'s formals; (c) also make `tab_plain`/`tab_num` internal.
Recommendation: **(b)** — removes the four-spelling problem at zero user cost; (c) would break the
programming vignette's "build cells from scratch" story.

**Q3 — the `tab_reg()` argument collapse (KEY 3a): which enum?**
Proposal: `effect = c("coefficient","ame","mer","ame_ratio","mnl_vsrest")` + `scale = c("ratio","link")`,
absorbing `at`, `exponentiate` and `estimate_display`'s two folds. A hard break of an unreleased
surface, which the maintainer has authorised. One judgement call: `trials` — fold into `family` or keep
separate? It *is* a family variant internally (`crude_key = "grouped_binomial"`), but a separate formal
reads better in jamovi. Recommendation: **keep `trials`, fold the other three.**

**Q4 — the `.fit_cache`/`reref` path: is jamovi worth ~450 lines and an 11-conjunct predicate?**
It buys a live UI that does not refit on a reference change. It costs a parallel fitter, a duplicated
`disp_known`, a byte-identity obligation, and a **silent footer degradation** (checks vanish on the
digest path because it keeps no fit). Options: (a) keep as is; (b) keep, but make the degradation
explicit (a footer note "model checks need a refit"); (c) drop the digest tier, cache raw fits only,
accept a refit on reference change. Recommendation: **(b) now, revisit (c) if the 11 conjuncts ever
need a 12th** — each new estimand feature must add one or silently return wrong numbers.

**Q5 — `estimand` (KEY 2): attribute now, or wait for a display change to ride along?**
The `var`-sniffing in `fmt_gap_scale_key()` is the strongest argument for "now": an order-dependent
dispatch on an under-determined input, in the newest and least-exercised part of the colour engine.
Recommendation: **now, right after E1.**

**Q6 — the jamovi boundary: consolidate or accept the mirror?**
Seven rules are hand-mirrored (§4.5), three of them in JS. Options: (a) accept, document each mirror;
(b) a shared `tab_resolve_display_settings()` both boundaries call, plus a generated JSON table for the
JS eligibility rules; (c) generate the JS rules only. Recommendation: **(b)**, and convert `anova` to a
`tab()` argument while there — it is the last global-option mutation and the one stale-cache hazard
left in the layer.

**Q7 — is anything in this document a white elephant *of mine*?**
Two candidates I would push back on myself: (i) the `display` token registry (§4.1.8) — 17d deferred it
for good reasons and only the `get_num`/`set_num` half is genuinely a map; (ii) collapsing
`n`/`wn`/`tot_n`/`n_eff` — an accessor is right, a merge is not.

---

## 11. Defects found in passing — fix regardless of any redesign

| #      | defect                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           | site                                                       | severity                  |
|--------|------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|------------------------------------------------------------|---------------------------|
| **D9** | **the jamovi `design_effect` checkbox does nothing.** It is declared in `jmvtab.a.yaml:205` and rendered in `jmvtab.u.yaml:231`, but **absent from the stale generated `R/jmvtab.h.R`**, so `self$options$design_effect` is `NULL` and `isTRUE(NULL)` is `FALSE`. Every claim in the `.a.yaml` help and the `.b.R`/`jmvtab-cache.R` comments about it "moving every interval in the table" is currently untrue in the running module. Pending a maintainer `jmvtools::prepare()`                                 | `jmvtab.h.R` vs `jmvtab.a.yaml:205`                        | **user-visible**          |
| D1     | transposed reg tables lose `ann$keep_black` → footer rows wrongly greyed in HTML; masked by a silent fallback, untested                                                                                                                                                                                                                                                                                                                                                                                          | `tab-transpose-render.R:212-223` → `tab-render-html.R:430` | user-visible              |
| D2     | `theme = "print"` + `engine = "kableExtra"` renders `kable_material_dark`                                                                                                                                                                                                                                                                                                                                                                                                                                        | `tab-render-html.R:294-309`                                | user-visible              |
| D11    | jamovi `display` on a **mean** column with `ci = "cell"` sets `pct_ci`, which reads the `pct` field — `NA` on the numeric leaf → the cell renders **empty**. `tab_ci()` had already set `mean_ci` correctly                                                                                                                                                                                                                                                                                                      | `jmvtab-cache.R:523-528` vs `tab.R:6181`                   | user-visible              |
| D4     | background-channel allow-lists disagree: `c("OR","adjustment")` legal in `tab_reg()`, illegal in `tab()`                                                                                                                                                                                                                                                                                                                                                                                                         | `tab.R:944` vs `fmt_class.R:1367`                          | inconsistency             |
| D5     | `tab_reg()`'s `na` message names `"drop_all_models"`, removed in z13                                                                                                                                                                                                                                                                                                                                                                                                                                             | `tab_reg.R:3152`                                           | message                   |
| D6     | the multi-dependent × model-list recursion drops `spread_models` and `.fit_cache`                                                                                                                                                                                                                                                                                                                                                                                                                                | `tab_reg.R:4948-4956`                                      | silent                    |
| D7     | the `ref_vect` NULL guard is unreachable (`ref_vect` is not a declared ctx field, so `is.null()` errors first)                                                                                                                                                                                                                                                                                                                                                                                                   | `tab.R:2401` vs `new_ctx()` `:1469`                        | latent                    |
| D8     | `legend_method_name()` falls through silently → can print a CI method the bounds were never built with                                                                                                                                                                                                                                                                                                                                                                                                           | `fmt_class.R:4179-4183`                                    | latent                    |
| D10    | the stale generated `R/jmvtabreg.h.R` still declares the removed `na = "drop_all_models"` — the live UI can pass a value `tab_reg()` rejects; it also carries four dead options (`ids`/`strata`/`fpc`/`nest`) and `jmvtab.h.R` carries two (`test_robust`, `method_ratio`)                                                                                                                                                                                                                                       | `jmvtabreg.h.R:184-190, 431-434`                           | pending `prepare()`       |
| D12    | `jmv_tab3_base_key()`'s `reapplied` list contains `"ci_method"`, which is **not a key of `opts`**; the four `method_*` keys therefore land in `structural` and force a full tier-3 rebuild, making the cheap re-ref path unreachable for CI-method toggles                                                                                                                                                                                                                                                       | `jmvtab-cache.R:666-668` vs `:712`                         | perf, no correctness risk |
| D13    | `tab(filter =)`'s cache key is hardcoded `NA_character_`, so a filter change never invalidates the jamovi cache                                                                                                                                                                                                                                                                                                                                                                                                  | `tab.R:2066`                                               | latent (0 known users)    |
| D14    | `@param other_if_less_than` documents an argument `tab_counts()` does not have                                                                                                                                                                                                                                                                                                                                                                                                                                   | `tab-counts.R:222`                                         | doc                       |
| D15    | stale comment: `jmvtab-cache.R:858-861` says `design_effect` "rides the global option, set around the build" — `.b.R:38-41` says the opposite and passes it as an argument                                                                                                                                                                                                                                                                                                                                       | `jmvtab-cache.R:858-861`                                   | doc                       |
| **D3** | **claim withdrawn.** An earlier draft of this study asserted that `tab_ci()`/`tab_chi2()` **drop** `meta` on the exported step path. They pass no `meta` argument (`tab.R:6244, 6255, 6366, 6377`), but *verified by running it*, `meta` survives — `tibble::new_tibble()` preserves the incoming object's attributes, and `tab_plain() | > tab_chi2() | > tab_ci()` keeps `vars` and a `set_caption()`. It is **undesigned, not broken**; passing`meta` explicitly costs six lines and removes the hazard class | `tab.R:6244` etc.                                          | none (latent)             |

---

## Appendix — what this study did *not* find

Absence of a finding is evidence too.

- **The CI engines are genuinely shared.** All nine `ci_*` functions in `R/tab-agg.R` are called from
  both the crosstab and the regression paths. There is no duplicated statistics anywhere in the
  package.
- **The fact tables that exist are good.** `MEASURES`, `REG_EMPIRICAL`, `REG_CHECKS`, `CI_METHODS`,
  `materialize_specs`, `meta_bind_rules`, `reg_footer_spec` all earn their place. The criticism in
  KEY 4 is that the pattern **stopped halfway**, not that it was wrong.
- **The footer model is complete for ordering.** Every backend goes through `tab_footer_streams()` /
  `render_footer()`; the leaks are placement (xl) and typography (plot), not order.
- **The jamovi cache kernel is right.** One store lifecycle, one LRU, two configs, no forked math,
  byte-identity locked by tests. The duplication is in the *boundary*, not the engine.
- **`tab_counts()` enters at the right seam** and forks no statistics — the design its header claims,
  and it holds.
- **The inference triple (`conf_level`, `degf`, `basis`) is the cleanest part of the whole model** —
  one writer (`tab_stamp_inference`), derived readers, and an algebra that fires inside `vec_ptype2`
  without anyone calling it. **It is the template every key in this document is asking for.**
- **No statistical soundness problem was found.** Every issue here is structural.
