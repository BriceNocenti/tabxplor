# tabxplor — the remaining keys

**An ecosystem integration study, post `Phase 18z16-iiiii` (2026-08-13)**

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
later claim rests on. **§3 is the substance: eight keys**, each stated as *the fact that is missing*,
with its evidence, what it unlocks, and its honest cost. §4 maps the remaining subsystem-level
simplifications. §5 is the white-elephant list, §6 the field/attribute verdicts, §7 the
anti-propositions, §8 the caveats, §9 the sequencing, §10 the decisions the maintainer must take.
§11 lists the defects found in passing.

**Method.** Seven parallel deep audits (crosstab argument surface; `tab_reg`; the fmt record; the
colour engine and legend; the export stack; the build pipeline; jamovi + the multi-table story +
`tab_counts`), plus direct probes of built objects. Evidence is `file:line` from the working tree of
2026-08-13. Where two audits disagreed, the claim was re-verified by running it (one such claim is
recorded, corrected, in §11 D3).

**Second pass (2026-08-13, same day).** KEY 1 and KEY 2 were the two keys whose *design* the first
pass left open, so they were re-studied against running code: the carrier-robustness matrix (§2.6),
a working prototype of the typed row-index vector, and a column-by-column verification of what a
stored estimate scale would reproduce. §KEY 1 and §KEY 2 below are the result and are **written as
two studied options each**, with the facts needed to choose between them; §10 Q1/Q5 record the
choice as still open. Four defects surfaced during that pass (§11 D16–D19), one of them
user-visible.

**Third pass (2026-08-13).** §KEY 8 was re-opened from scratch — *"restate the whole problem and find
the real key; I would rather have a well-argued demolition than a polished defence."* It is a
demolition: the pass-2 `compare` argument is withdrawn on three measured grounds (the name is already
a `tab_reg()` formal; on `tab()` the comparison geometry is **not an input**; and the D20/D21 defect
class is reachable with no `OR`/`ci` conflict at all). **§KEY 8 is rewritten** around the replacement —
one vocabulary, two hosts, and a *principled divergence* between the two producers.

A **follow-up round** the same day settled the two questions the rewrite left open, both by
measurement: the odds ratio becomes **unconditional** (measured free — 300 ms against the difference
path's 340 ms), which is what makes `tab(OR =)` deletable; and `ci` is **not** a logical (the first
draft's proposal, withdrawn on the maintainer's objection) but keeps its *anchor* question,
`c("auto", "no", "cell", "comparison")`, with `"cell"` unchanged. `color` alone must **not** trigger
the comparison interval — measured **+38 %** on a build.

**Six defects surfaced** across the third pass (§11 D22, D23, D25, D26, D27, D28), three of them
user-visible; D24 was checked and **not** confirmed — `OR = "cumOR"` messages correctly, and its
message is the model this study recommends elsewhere. Corrections propagated to §KEY 2's naming
sub-question, §KEY 3's derived-arguments table, §KEY 3a's Shape 1, §5, §9 and §10 Q7.

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

### 2.6 Carrier robustness — the measurement KEY 1 rests on

Four carriers can hold a fact about a tabxplor table. They are **not** equally robust, and the
difference is measurable, not a matter of taste. A grouped, merged table
(`tab(gss_cat, c(marital, relig), race)`) was stamped with one probe in each carrier and put through
15 dplyr verbs:

| carrier                                             | survives                                                                                                  | fails                                                      | score     |
|-----------------------------------------------------|-----------------------------------------------------------------------------------------------------------|------------------------------------------------------------|-----------|
| per-cell **field** (`vctrs::field`)                 | everything — it *is* data, sliced with the rows                                                           | —                                                          | **15/15** |
| per-column **attribute** (`col_var`, `conf_level`…) | filter arrange mutate select slice rename relocate group_by ungroup `[` head as_tibble distinct summarise | `bind_rows` (§11 D16)                                      | **14/15** |
| the three **declared** table attributes (`meta`…)   | the same, minus `as_tibble`                                                                               | `bind_rows` (D16), `as_tibble`                             | **13/15** |
| a **bare** custom table attribute                   | group_by ungroup `[` head                                                                                 | 11 verbs — `tab_restore()` carries only the declared three | **7/15**  |

Three consequences, each of which decides part of the design:

1. **A new table-level attribute is the least robust thing that can be added**, unless it is wired
   into `tab_attrs()` / `tab_bind_attrs()` / `tab_restore()` — at which point it behaves exactly like
   `meta`, no better. There is no "more robust table attribute" available: 13/15 is the ceiling, and
   D16 is what stands between 13 and 15.
2. **A length-`n` attribute is not a carrier at all.** Measured: `attr(x, "row_kind") <- <5 values>`
   on an fmt column, then `vec_slice(x, c(5L, 1L))` → the attribute comes back **length 5, in the
   original order**, beside a length-2 column; `filter()` does the same. vctrs carries attributes
   *whole*; only fields are sliced. So "a long column attribute with one value per row, keyed on
   rows" (floated in §10 Q1) is the positional-vector disease one level down — with the same silent
   failure, one layer harder to see. The only per-row carriers that track the rows are **a field of a
   record column** and **a column**.
3. **dplyr's grouping cannot be borrowed, but its rule can be copied.** Measured: an extra column
   stamped into `attr(d, "groups")` is *accepted* by `dplyr::validate_grouped_df()` and then treated
   as a **grouping variable** — `group_vars()` returns a phantom `NA`, and `filter`/`mutate`/`slice`/
   `select` all error. And `dplyr_reconstruct.grouped_df` is literally
   `grouped_df(data, group_intersect(template, data))`: the `groups` tibble is **regenerated from the
   named columns on every verb**. That is the entire source of dplyr's robustness, and it is
   copyable in three rules — **store names, never positions; recompute the index through one funnel;
   degrade silently when a named column is gone.** What is *not* copyable is the container.

---

## 3. The eight keys

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
3. **The role vector is positional — and, worse, it only exists at display time.** Correction to this
   study's first draft, measured: `row_roles` is **not carried on a built table**. It is created
   inside `tab_materialize_extras()` (`tab_classes.R:1393`) at render and lives one render pass —
   `tab(gss_cat, c(marital, relig), race, test = TRUE)` has `row_roles = NULL`, so there is nothing
   for a user's `arrange()` to desynchronise. That makes it less dangerous than the draft claimed and
   **more** damaging structurally: every consumer *outside* the render path gets no roles at all and
   falls back to `tab_row_roles()`'s English-label matching (`tab.R:3245-3259`). `tab_estimates()`
   already reads it that way (`plots.R:213`). The i18n hazard 17c closed for the exporters is still
   open for everything else — **by design, not by drift** — and every new consumer inherits it.
4. **Rendered-string comparison.** `collapse_totals` runs a full `format()` pass over every fmt column
   of every block just to decide two Total rows are "the same as displayed"
   (`tab_classes.R:1543-1545`).
5. **`num_core` records no `vars` at all** (`tab.R:5773, 5777`) — verified: `tab_num()`'s
   `meta$vars` is `NULL`, so it falls back to the last-factor heuristic `new_vars_attr` was
   introduced to replace. `tab_reg()` records none either (its `meta` is
   `list(ci_settings, reg_meta)` — §KEY 6).
6. **The support matrix is enforced by five separate aborts and written down nowhere** —
   `tab_compact` (`tab_classes.R:1259`, `:1273`), `tab_transpose` (`tab.R:2999`, `:3007`),
   `tx_transpose_render` (`tab-transpose-render.R:33`). "Can I transpose a grouped table?" has no
   single answer to read.
7. **A merged table loses `ordered`.** `tab_stack_tables()` strips the class when several row_vars are
   stacked (`tab_classes.R:1223-1225`) — correct as written, because vctrs rightly refuses to combine
   two ordered factors with different level sets, but the cause is that one text column is being
   asked to hold several variables' levels with no way to say which belongs to which.

**Consumer surface, for sizing:** `is_totrow(` 51 sites · `is_tottab(` 25 · `tab_get_vars(` 27 ·
`get_vars_attr(` 14 · `is_refrow(` 13 · `tab_render_vars(` 8 · `tab_row_roles(` 7 ·
`set/get_row_roles` 6.

**The key.** Give the row axis the same treatment the column axis already has: **a declared index
block** — one leading block of columns, each with a stored role, in a fixed order — plus, per row, a
stored `row_kind ∈ {data, total, n, pct, pvalue, gof, blank}`.

```
[ tab_var levels … ] [ var ] [ level ]        roles: "tab_var" | "var" | "level"
```

The design question the first draft left open, and which the rest of this section answers, is
**where each of those two facts is carried**. (The first draft's answer — a `meta$rows` tibble joined
on `(var, level)` — is withdrawn: that key is not unique, since total rows repeat across blocks and
every level repeats across `tab_vars`, so the join is no more robust than the vector it replaces.)

---

#### What any solution must satisfy

From §2.6, plus one contract:

- **R1 — per-row facts live in fields or columns, never in attributes.** (§2.6.2: a length-`n`
  attribute is carried whole and never sliced.)
- **R2 — store names, recompute indexes, degrade silently.** (§2.6.3, dplyr's own rule.)
- **R3 — a lone extracted fmt column must still format and colour itself** (`test-degraded-attrs.R`).
  This is decisive and easy to miss: `fmt_color_plan()` calls `is_totrow(x)` on a *column*
  (`fmt_class.R:3580`, and the `gate_row` MEASURES fact), with no table in scope. **So the total-row
  flag cannot leave the record**, whichever carrier is chosen for the rest.

R3 is why the three options below are not really "field *or* column": all three need the record
change, and they differ only in where the **declaration** — *which column is what* — is carried, and
in whether the **variable identity** moves into the label column.

---

#### The shared half — `row_kind` replaces `in_totrow` (all three options)

`in_totrow` is a two-valued encoding of a fact that has seven values. Replacing it costs nothing:

```r
row_kind ∈ {data, total, n, pct, pvalue, gof, blank}      # a FIELD, one per cell, like in_totrow
is_totrow(x)  ==  get_row_kind(x) == "total"              # derived, no field added
```

- **The record stays at 21 fields** — one field replaced, not added (z6's re-open threshold is
  untouched). Measured cost of widening it from logical to character on a 51 × 6 table (204 fmt
  cells): **+0.9 KB on a 68.6 KB object**, i.e. z6's verdict is unchanged and no encoding trick
  (factor, integer codes) is worth its complexity.
- `fmt_row_flag()` (`fmt_class.R:658`) already reduces a per-cell row flag to a per-row vector for
  exactly these three fields; it gains a "first non-`data` wins" reduce beside its existing
  `all`/`any` ones. Measured cost of that reduce today: **27 µs** on a 51-row table — a render-time
  price, paid once.
- The materialisers stop appending to a vector and start passing `row_kind =` to the `fmt()` call
  they already make; `tab_collapse_total_rows` stops slicing a parallel vector (it slices rows, and
  the kinds ride along); the transpose stops re-deriving.
- `in_tottab` (a *scope*: this row is in the total table) and `in_refrow` (a *marker*: this row is the
  reference) are orthogonal to kind and stay as they are.
- **Robustness: 15/15** (§2.6). Users read it as `tab$Black$row_kind`, which is the surface the
  README already teaches for `$pct`.

Everything below is about the other half: *which variable does this row belong to, and what shape is
the label block*.

---

#### Option A — a declared row spec in a table attribute

```r
attr(tab, "rows") <- new_row_spec(
  index = c("partyid", "row_var", "levels"),     # the leading block, in order
  roles = c("tab_var",  "var",     "level")      # what each column IS
)
```

Wired into `tab_attrs()` / `tab_bind_attrs()` / `tab_restore()` as the **fourth declared attribute**,
with a bind rule in `meta_bind_rules`' style. One `tab_reconstruct()` funnel recomputes everything
*derived* (block boundaries, label runs, separators, `compacted`) at render; nothing derived is
stored. Absent spec → today's heuristics, marked as fallback (17c's proven pattern).

**Strengths**

- Cheapest by a wide margin: no new class, no `is.factor` migration, no user-visible column change.
  One session plus D16.
- It is the *same* mechanism the package already trusts three times over, so its failure modes are
  known and already tested (`test-degraded-attrs.R`, `test-meta-attr.R`'s field-agnostic probe).
- It carries facts that genuinely have no column to live in (which column is the `tab_var`, which is
  the `var`) without inventing a place for them.

**Caveats, measured**

- **13/15 is the ceiling** (§2.6). It is not, and cannot be made, "as robust as dplyr's grouping" —
  because dplyr's grouping is not robust *as an attribute*: it is robust because it is **regenerated
  from the data**. A spec that names columns can be regenerated the same way **only if the names are
  recoverable from the data**, which is exactly what a fixed naming convention buys. Under the
  friendly-names convention the spec is a *truth* that can be lost; under `row_var | levels` it is a
  *cache* that can be rebuilt — and only then does it inherit dplyr's property.
- **The recurrence count is the honest argument against it.** Table-level facts have been silently
  dropped by a rebuild site **five times in three phases**: `tab_compact()` (z16-iv, lost
  `meta$inference`), `tab_spread()` and `reg_build()`'s split branch (z16-iiiii, lost the whole
  `meta`), and now `bind_rows()` on a grouped table plus `dplyr_reconstruct.tabxplor_grouped_tab`
  restoring from the wrong argument (§11 D16, found by this pass). Each was fixed; the class was not.
  z16-iiiii's own conclusion was to move the inference triple **off** the table for this reason, and
  §Appendix calls that "the template every key in this document is asking for".
- The variable identity stays split across three naming conventions unless the naming question
  (below) is answered too.

---

#### Option B — a typed row-index column, and derive the rest

The level column becomes a light vctrs record, the same shape as `tabxplor_fmt` one axis over:

```r
tabxplor_lvl :  fields(label <fct>, var <chr>, kind <chr>)
```

`kind` here is the *display* copy of `row_kind` (one per row instead of one per cell); the record
keeps `row_kind` for R3. Every table-level fact that can be derived from the column is **derived**,
not stored.

**Feasibility — prototyped and measured** (vctrs 0.7.3 / dplyr 1.2.1), all green unless noted:

| operation                           | result                                                                                                     |
|-------------------------------------|------------------------------------------------------------------------------------------------------------|
| print                               | `<lvl>` column, plain labels                                                                               |
| `filter(levels == "Total")`         | works — `vec_ptype2`/`vec_cast` with character                                                             |
| `arrange(levels)`                   | follows **each variable's own factor order** (`vec_proxy_order` → the factor)                              |
| `group_by(tab_var, levels)`         | works — vctrs #1318 (mixed vctrs/non-vctrs grouping) is fixed                                              |
| `distinct` `count` `left_join(by=)` | work                                                                                                       |
| `bind_rows` / `vec_rbind`           | work                                                                                                       |
| `mutate(levels = levels)`           | keeps the type                                                                                             |
| `write.csv`                         | writes the label                                                                                           |
| `$` field access                    | needs a `$.tabxplor_lvl` method — the package ships one for fmt already                                    |
| `vec_c(lvl, factor)`                | **errors without a ptype2 method** — `tab_stack_tables()` needs it                                         |
| base `rbind()` on the data frame    | fails — **but it already fails on every tabxplor table today** (fmt is a rcrd), so this is not a new limit |
| `is.factor()`                       | **FALSE** — the migration cost (39 `is.factor` sites in `R/`, ~10 of them about the label block)           |

**What becomes derived** (the maintainer's ask — measured: `unique()` on the derived column costs
**2.5 µs** against **1.5 µs** for an attribute read on a 51-row table; both are noise):

| table-level fact today | derived from                                        |
|------------------------|-----------------------------------------------------|
| `meta$vars$row_vars`   | `unique(levels$var)`                                |
| `meta$vars$compacted`  | `length(unique(levels$var)) > 1`                    |
| `meta$vars$row_roles`  | `levels$kind`                                       |
| `tab_vars`             | `dplyr::group_vars()` minus the index               |
| `col_vars`             | already derived from the `col_var` column attribute |
| `in_totrow`            | `row_kind == "total"` (the shared half)             |

`meta$vars` then holds only what no column can carry: `wt`, `caption`, `var_labels`. The
72 `meta$vars` reader sites become reads of a derived accessor with the same name — the change is in
where the answer comes from, not in what consumers call.

**Strengths beyond A**

- **The four label-block shapes become one.** That is what makes `tab_reg()` stop punning
  (`tab_render_vars()` reports `row_var = "levels", tab_vars = "var"` on a regression today) and what
  lets the transpose flip a declared index instead of hand-copying 39 slots.
- **It releases the grouping column, which is what the composition limit is really about.** A merged
  table is a `grouped_df` grouped by `row_var` — so "several row_vars" and "tab_vars" compete for the
  one slot dplyr grouping gives, and `can_merge <- length(tab_vars) == 0` is the surrender. With the
  variable inside the label column, blocks come from the data and grouping is left to real
  `tab_vars` alone. (Option A can also fix this — by grouping on two columns — but the competition
  remains, and every consumer that assumes one grouping column must learn about two.)
- **`ordered` survives a merge** (cost 7 above): the proxy orders by `(var, level)`, so each
  variable keeps its own order instead of the class being stripped.
- **Symmetry, and it is not decorative.** Columns describe themselves through attributes; rows would
  describe themselves through fields. `tab$levels$kind` reads exactly like `tab$Black$pct`, which is
  the programming surface the package was designed around and the vignette teaches
  (`tabxplor-programming.Rmd:77`).
- The i18n hazard closes **completely**: no fallback matches a translated label, because the only
  text left in the index is a variable *name*.

**Caveats**

- One new class, ~8 S3 methods (`format`, `as.character`, `vec_ptype2`/`vec_cast` with character and
  factor, the three proxies, `pillar_shaft`, `$`). The fmt record is the template, so this is
  well-trodden — but it is a second public-ish vctrs type in a package whose first one is its
  hardest-to-change contract.
- The `is.factor` heuristics must migrate in the same session, not later:
  `tab_get_vars()`/`tab_render_vars()` identify the row axis as "the last factor column"
  (`tab.R:3201-3214`), and a typed column is not a factor. This is the one place where Option B is
  *forced* to complete what Option A may leave half-done — which, per §7's anti-proposition, is an
  argument in its favour.
- 2–3 sessions, and it moves the *structural* goldens (not rendered output).
- Degraded mode: `mutate(levels = as.character(levels))` drops back to the label heuristic — the same
  fallback both options need anyway.

---

#### Option C — a typed label column with **no fields**: the declaration moves onto the column

The middle point of the design space, and the one this study did not consider in its first draft.
Keep the label column a **factor**; give it a light class; and put the facts that are *per column*
(what this column is for) into **column attributes** — the same carrier the fmt columns already use
for `col_var`, `role`, `conf_level`, `basis`. Nothing per-row is added: the row kind rides
`row_kind` (the shared half), and "which variable does this row belong to" is what the `row_var`
column's *values* already say.

```r
tabxplor_lvl :  class = c("tabxplor_lvl", "factor")      # it is still a factor
attr(col, "role")  "level" | "var" | "tab_var"           # what this column IS
attr(col, "var")   the variable its labels belong to     # NA on a merged `levels` column
```


**Measured on a prototype** (same 15 verbs, plus base and forcats interop):

| fact                                        | measured                                                                                                                                                                                                                                                    |
|---------------------------------------------|-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| `is.factor()`                               | **TRUE** — it *is* a factor. So the 39 `is.factor` sites, `levels()`, `as.character()`, `arrange()`'s factor order, `filter(levels == "Total")`, `group_by()` and printing all keep working **with no method written at all**                               |
| survival with **zero** methods defined      | `[`, `filter`, `arrange`, `mutate`, `as.data.frame` **and** forcats' `fct_drop` / `fct_rev` / `fct_relevel` all keep the class *and* the attributes                                                                                                         |
| what actually needs a method                | only `bind_rows` / `vec_c` (→ `vec_ptype2` + `vec_cast`, two short methods), and `droplevels()` / a `factor()` round-trip (one method). Verified: the package's own `droplevels`/`fct_drop` calls are all on **source data**, never on a built label column |
| total                                       | **15/15 with ~4 short methods**, against Option B's ~8 (three proxies, `$`, `pillar_shaft`, format, as.character, ptype2/cast)                                                                                                                              |
| the same attribute with **no class at all** | **13/15** — everything except `bind_rows`. So the class buys exactly one verb, and it is the same verb D16 already breaks for the table attributes                                                                                                          |

**What it unlocks — everything Option A unlocks, plus most of B:**

- Role detection becomes **exact**: no last-factor heuristic, no positional vector, no English-label
  matching. `tab_render_vars()` reads each column's own declaration and stops guessing.
- `meta$vars` becomes derivable exactly as under B — `row_vars` from the `var`-role column's values,
  `compacted` from its presence, `tab_vars` from the columns that declare themselves. **Better than B
  on one point:** a `tab_var` that declares itself on the column survives `ungroup()`, whereas
  deriving `tab_vars` from `group_vars()` does not.
- The composition limit lifts for the same reason as under B: the merged table's blocks come from
  the declared `var`-role column instead of from the single dplyr grouping slot, so `tab_vars` stops
  competing with several `row_vars`.
- `num_core()` and `tab_reg()` record their variable model by **stamping one attribute at build** —
  the same one-liner the fmt columns already get — instead of assembling a `vars` list.
- **It decouples the naming question from the robustness question.** Under Option A, naming 1 is
  *required* for the spec to be re-derivable (§ below); under C a single-variable table can keep the
  friendly `tab$marital` **and** have exact detection, because the column says what it is regardless
  of what it is called.

**What it may or may not unlocks but needs additional research:**
- **`ordered` fix ?** A merged `levels` column still stacks several variables' levels in one
  factor, so `tab_stack_tables()` must still strip the class (`tab_classes.R:1223-1225`). C can keep
  `ordered` on a single-variable table — where nothing strips it today either — so this is *unchanged*,
  not improved. Is there a way to fix that, and store the ordered information somewhere when some levels are levels of an ordered variable and some others are not ? It’s actually the same for factor col_vars : would there be a cheap and reliable way to store the `ordered` information, so that special things can be done for ordinal variables in the future ?

**What it does NOT unlock (the honest difference from B):**
- **No single leading column.** C keeps `row_var | levels` on a merged table, so the four label-block
  shapes become **two declared shapes**, not one.
- **No `$` row surface.** With no fields there is no `tab$levels$var`. `row_kind` (the shared half)
  still gives `tab$Black$row_kind`, and the per-row variable is a column's values — arguably more
  readable than a field, and it is what users already read today.
- It does not make the row axis *self-describing per element*: a row torn out of its table (a
  `vec_slice` of one label column alone) still knows its kind only through the fmt cells beside it.

**Two honest caveats of its own.** (i) `class(tab$marital)` becomes `c("tabxplor_lvl", "factor")`, so
user code testing `identical(class(x), "factor")` (rather than `is.factor(x)`) would see the
difference, and `str()`/`dput()` output changes — the same, smaller, version of what B costs.
(ii) Like B, C leaves `meta$vars` in place for the facts no column can carry (`wt`, `caption`,
`var_labels`); what it removes from that list is the whole *variable model*, which is what was being
re-derived by heuristics.

---

#### The naming sub-question (independent of the carrier; A alone is coupled to it)

| option                                  | what it buys                                                                                                                                                                                                                   | what it costs                                                                                                                                                                                                                                        |
|-----------------------------------------|--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| **1. always `row_var` \| `levels`**     | the spec becomes **re-derivable** from the data, so Option A finally inherits dplyr's property; one shape for every producer; the reg pun dies. Available under all three, and still worth having under C for the single shape | `filter(t, marital == "Married")` stops working on a single-row_var table. **Measured: no vignette, README or test does this** — the only `filter()`s in the docs are on the source data before `tab()`. So the breakage is in unseen user code only |
| **2. one typed column** (Option B only) | `row_var` stops existing as a stored column and becomes `levels$var` — nothing to keep in sync, and the grouping slot is freed (above)                                                                                         | furthest from today's user code; `group_by(row_var)` needs either a derived column or (better) the render-time block derivation                                                                                                                      |

Note the coupling: **naming 1 is what makes Option A robust** (it is what lets the spec be rebuilt
from the data rather than merely stored), and naming 2 is only available under Option B. **Option C
removes the coupling entirely** — a column that declares its own role can be called anything, so the
friendly `tab$marital` survives with no loss of exactness. A + friendly names is the one combination
that keeps every documented example working *and* keeps the fragility the maintainer objected to.

---

#### The three options side by side

| criterion                               | **A** — declared spec in a table attribute           | **C** — typed label column, attributes only | **B** — typed row-index column with fields |
|-----------------------------------------|------------------------------------------------------|---------------------------------------------|--------------------------------------------|
| where the **declaration** lives         | a 4th table attribute                                | column attributes                           | column attributes + per-element fields     |
| measured robustness of that carrier     | 13/15 (15/15 only after D16)                         | **15/15** (~4 short methods)                | **15/15**                                  |
| row **kind** carrier                    | `row_kind` field (forced by R3)                      | `row_kind` field                            | `row_kind` field (+ a display copy)        |
| per-row **variable identity**           | the `row_var` column's values                        | the `row_var` column's values               | a field of the label column                |
| `is.factor` migration                   | none                                                 | **none** (it *is* a factor)                 | ~10 sites, forced                          |
| new S3 methods                          | 0                                                    | ~4                                          | ~8                                         |
| kills the positional role vector        | yes                                                  | yes                                         | yes                                        |
| kills the last-factor / label guessing  | yes                                                  | yes                                         | yes                                        |
| `meta$vars` becomes derived             | partly (the spec is still stored)                    | **yes**                                     | **yes**                                    |
| `tab_vars` survives `ungroup()`         | yes (named in the spec)                              | **yes** (declared on the column)            | only if the tab_var columns are typed too  |
| unlocks `tab_vars` × several `row_vars` | yes, via two grouping columns                        | yes, blocks derived from the data           | yes, blocks derived from the data          |
| four label shapes become                | one (with naming 1) / four declared (friendly names) | **two declared**                            | **one**                                    |
| friendly `tab$marital` can stay         | only at the cost of robustness                       | **yes, no cost**                            | no (naming 2)                              |
| fixes the merged-table `ordered` strip  | no                                                   | no                                          | **yes**                                    |
| `$` row surface (`tab$levels$var`)      | no                                                   | no                                          | **yes**                                    |
| estimated cost                          | ~1 session + D16                                     | **~1.5 sessions**                           | 2–3 sessions                               |

---

#### What any of the three unlocks (in one motion)

- `tab_vars` × several `row_vars` **compose** instead of competing — the list fallback disappears, and
  with it a documented product limitation.
- `tab_reg()` stops punning: a predictor is `role = "var"`, not a fake sub-table. `forest_plot()`,
  `reg_check_plots()`, `tab_spread()`, `tab_plot()` then work on either kind of table with no branch.
- `tab_collapse_total_rows` compares **keys**, not rendered strings.
- The transpose becomes a flip of a declared index (KEY 7 / §4.3), not a 250-line hand-copy.
- The `test` tibble keys on `(scope, var, level, col)` and stops overloading `row_var`.
- One `tab_shape(x)` capability predicate replaces the five scattered aborts, and makes the missing
  combinations explicit rather than discovered.
- Consumers outside the render path (`tab_estimates()`, and anything added later) get real roles
  instead of an English-label fallback.

**Honest cost.** The largest structural item in this document whichever option is taken. It touches the leaves'
tails, the compact/spread/transpose trio, the export prep's label runs, and `tab_reg`'s assembler. It
is **not** a change to the fmt record's *size* (one field replaced) and needs no rendered-output
movement, but it will move the *structural* goldens and needs `dev/verify_golden_field_delta.R`
extended once more.

**Caveat.** Do not implement any of them as a *fifth* representation added beside the existing four. The
value is entirely in deleting the other four. If the merged / tab_vars / reg shapes are not all
migrated, this becomes the ad hoc layer this roadmap exists to avoid.

**Recommendation, on the facts above — revised now that C is measured: Option C.**

The reasoning is that **C takes almost all of B's structural wins at little more than A's cost, and
it is the only one of the three with no downside against today's behaviour.** Specifically:

- Against **A**: C scores 15/15 where A's ceiling is 13/15 (and only reaches that after D16), it makes
  `meta$vars` genuinely derived rather than stored-and-hopefully-carried, and it removes the coupling
  that forced naming 1. Cost difference: ~4 short S3 methods, three of which are one-liners.
- Against **B**: C gives up exactly three things — the merged-table `ordered` fix, the single leading
  column, and a `$` field surface for rows — and buys back the entire `is.factor` migration (39 sites,
  including the two variable detectors), half the method count, and the ability to keep
  `tab$marital` as the friendly single-variable name. None of the three things given up is on the hot
  surface of §1; the `is.factor` migration is squarely in the middle of it.
- The decisive measurement is that **C needs almost no methods to be robust**: a factor subclass
  already survives `[`, `filter`, `arrange`, `mutate`, `as.data.frame` and forcats' relevel/drop/rev
  with zero code, so the class exists only to make `bind_rows` work. That is a much smaller surface
  to get wrong than a record type with three proxies.

**If the `ordered` behaviour or the single-column shape turn out to matter more than expected, B is
still reachable from C** — the migration C performs (every consumer reading a declared role instead
of guessing) is exactly the migration B needs, so C is a strict prefix of B rather than a fork.
**Option A remains the honest minimum** if KEY 1 must fit in one session: it kills the positional
vector and the guessing, which is most of the value, and leaves both C and B reachable — but it
stores what the other two derive, and it needs naming 1 to be as robust as it sounds.

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

**Two more facts, measured this pass, that change the shape of the answer.** The second was
challenged by the maintainer after the first draft and re-tested; the result is a correction, and it
simplifies the key rather than complicating it.

**(a) There are already TWO estimate-field rules, and they disagree on 178 of 190 columns.**
`fmt_est_field(ci_type)` (`fmt_class.R:2014`) answers `"diff"` for every column with no contrast
interval; `fmt_center_field()` (`:3415`, via `EST_SCALES`) answers `"pct"` or `"mean"` for the same
columns. Measured across the 36 structural goldens (190 fmt columns, 9 distinct `(type, ci_type)`
pairs): they differ everywhere except the 12 effect columns. Neither is wrong — one answers *"which
field is the effect"* and is only ever called on effect columns, the other answers *"which field is
the interval centred on"*. **But nothing in the code says so**, and the only thing keeping them apart
is which function a caller happened to reach for.

**(b) ONE scale is enough — `ci_type` can be deleted, not renamed.** An earlier draft of this section
claimed the estimate's scale and the stored interval's scale were two independent facts and that the
key therefore needed two attributes. **That claim is withdrawn**; it was tested column by column and
does not survive. The interval is always on the estimate's own scale, and each apparent
counter-example turns out to be something else:

| apparent counter-example                                                                                              | what it actually is                                                                                                                                                                                                                                                                                                   |
|-----------------------------------------------------------------------------------------------------------------------|-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| the **reference** column of an OR table (`ci_type = ""` while its siblings say `"or"`)                                | not a scale difference at all: the column's bounds are NA, so what varies is *whether an interval exists here*, not what scale it is on. Measured: `fmt_scale_key()` already answers `"or"` for all four columns                                                                                                      |
| a **two-channel** column ("only one interval is stored, the second channel derives from it", `fmt_class.R:3597-3601`) | a *measure*-vs-bounds difference, not an estimate-vs-bounds one. Measured on `color = c("diff","ratio")` + `ci = "diff"`: `ci_type = "diff"`, scale `points` — the stored interval **is** on the column's own scale, and the second channel's rescale is MEASURES' job, already handled                               |
| z17 calling one dispatch **with and without `display`**                                                               | that branch is gated on `!nzchar(ci_type)` (`fmt_class.R:3381`), so it only fires when **no interval is stored** — i.e. it is the first row of this table again, not a second fact                                                                                                                                    |
| **`OR = "OR"` + `ci = "diff"`**                                                                                       | the one real divergence — and it is a **defect**, not a design fact (§11 D21): the column prints odds ratios (`1.00`, `2.06`, `1.78`) while storing a percentage-**point** interval, so `ci_center()` returns the difference and `fmt_scale_of()` would give a forest plot a `pct_diff` axis for an odds-ratio column |

So the maintainer's reading is the correct one, and it is a bigger simplification than the draft
proposed: **one attribute, and `ci_type` is deleted rather than renamed.** It reproduces every value
`ci_type` carries — `"or"` → `odds_ratio`, `"ratio"` → `pct_ratio`/`mean_ratio` (the `type`
disambiguation the old value needed disappears), `"diff"` → `points`/`mean_diff`, `"cell"` →
`level_pct`/`level_mean` *with* an interval, `""` → no interval — and it needs no `refcol` routing:
under a **stamped** scale the reference column simply carries `odds_ratio` like its siblings, and its
all-NA bounds produce exactly today's behaviour (`has_ci` FALSE, no significance) with no special
case anywhere.

The one thing lost is the ability to *represent* "estimate on scale A, interval on scale B" — for
which no legitimate use was found, and whose only existing instance is D21. **Making the incoherent
state unrepresentable is what surfaces the defect**, which is the argument for it rather than against.

**Hidden complexities, named honestly** (there are three, and all are small):

1. **"Is there an interval" has to come from somewhere.** Derive it from the data —
   `!all(is.na(ci_inf))` — the same idiom `fmt_gap_force_policy()` already uses for `gap_se`
   (`fmt_class.R:2135`). Verified behaviour-identical: today the reference column stamps `""` → `has_ci`
   FALSE; under derivation all-NA bounds → FALSE. Likewise a column whose bounds are all NA for a
   *statistical* reason (`df <= 0` → `ci_pivot()` returns NA) is stamped `"diff"` today but still
   scores no significance, so the derived answer matches there too.
2. **`tab_ci()` must stamp intent, not record its argument.** Adding a difference interval to a
   percentage column *changes what that column is* (`level_pct` → `points`), which is honest but is a
   semantic upgrade to a step that currently only writes a label. The place to be careful is the
   jamovi tier-3 re-reference, which re-runs `tab_ci()` on a carrier (`jmvtab-cache.R:826`).
3. **The `OR` × `ci` combination needs a defined behaviour**, because it can no longer be silently
   half-represented: either build the Woolf OR interval (the machinery exists — `ci_or()` already runs
   when `color_signif`/stars are on) or refuse the combination. Today it does neither, and there are
   two distinct silent outcomes (D20, D21).

**(c) A live predicate with dead arms.** `has_ci <- cit %in% c("diff", "diff_row", "diff_col", "or",
"ratio")` (`fmt_class.R:3565`) tests two values that can never be stored — `tab_ci()` strips
`_row`/`_col` before stamping (`tab.R:6209`). And `"cell"` is deliberately absent, which is correct
(a cell interval is not a contrast) but is nowhere stated.

**The key.** Store the scale, as **one key into a declared library** — the `REG_CHECKS` /
`REG_EMPIRICAL` / `CI_METHODS` shape the package already uses five times. The library gains columns;
the record does not gain attributes. `EST_SCALES` (z17, `fmt_class.R:3307`) **is that library
already**, and `est_scale_key()` is precisely the dispatch this key deletes:

```r
attr(col, <name>)      "odds_ratio" | "pct_ratio" | "mean_ratio" | "pct_diff" | "mean_diff" |
                       "raw_diff"   | "log_coef"  | "level_pct"  | "level_mean" | "level_n"
attr(col, "pct_base")  "row" | "col" | "all" | "all_tabs" | "none"
# and `ci_type` is DELETED, not renamed -- see (b): the stored interval is always on this same
# scale, and "is there an interval here" is a data fact (all-NA bounds), not a second vocabulary

SCALES[["odds_ratio"]] = list(field = "or", geometry = "ratio", null = 1, is_pct = FALSE,
                              trans = "log10", unit = "or", numeric = FALSE,
                              breaks = "odds_ratio", gap_breaks = "adj_ratio", sd_from = NA)
```

**The library in plain words — and the fact that it is already shared.** Every row was measured on a
real column of both producers; the "produced by" column is what makes the point:

| row              | what it is, in plain words                                                                                                                                                                                                                                                                                                                                                                               | geometry   | produced by — **measured**                                                                                 |
|------------------|----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|------------|------------------------------------------------------------------------------------------------------------|
| `odds_ratio`     | a **multiplicative model effect**: how many times more likely / more frequent. ⚠ Its name is a misnomer — measured, an OR, an **RR** and an **IRR** are all this one row, with the *same* `model_family`, told apart only by the table-level `meta$effect` (`legend_reg_eff_word`, `fmt_class.R:4223-4225`). A neutral row name (`ratio_effect`) would be honest; the estimand's *word* is a legend fact | ratio      | `tab(OR = "OR")` · `Model_OR` · `Model_IRR` · `Model_RR` (both `family = "rr"` and `effect = "ame_ratio"`) |
| `pct_ratio`      | the ratio of two **proportions** (the "×2 rule" comparison a crosstab colours by)                                                                                                                                                                                                                                                                                                                        | ratio      | crosstab `color = "ratio"`                                                                                 |
| `mean_ratio`     | the ratio of two **means** (a rate ratio)                                                                                                                                                                                                                                                                                                                                                                | ratio      | `tab_num(ci_scale = "ratio")` · the poisson crude twin                                                     |
| `points`         | a difference between two **percentages**, in percentage **points** (+5 pts, not "5 % more")                                                                                                                                                                                                                                                                                                              | difference | `tab(pct = "row", ci = "diff")` · `Model_AME` on a binomial                                                |
| **`raw_diff`**   | a difference in the **outcome's own units** — hours, dollars, counts. "raw" = not converted to points or to a ratio. Coloured on the SD-standardised ladder, the SD coming from the column's stored `var(Y)`                                                                                                                                                                                             | difference | gaussian **β** · a poisson **count AME**                                                                   |
| **`mean_diff`**  | the crosstab twin of `raw_diff`: the same difference-in-units on the same ladder, but standardised by the **reference cell's** variance, because a crosstab stores no `var(Y)`                                                                                                                                                                                                                           | difference | `tab(marital, tvhours, ci = "diff")`                                                                       |
| `log_coef`       | the raw **link-scale** coefficient (`exponentiate = FALSE`): a log-odds or log-rate                                                                                                                                                                                                                                                                                                                      | log        | `tab_reg(exponentiate = FALSE)` → `Model_β` on a non-gaussian family                                       |
| **`level_pct`**  | **not a comparison at all** — the column holds a *level*: a percentage. There is no null to draw and no ladder (a level column's colour grades its *difference*; putting a ladder on the level axis would be a lie)                                                                                                                                                                                      | level      | `tab(pct = "row")` · `tab(pct = "row", ci = "cell")` · every crude `Obs_%`                                 |
| **`level_mean`** | the same, for a **mean**                                                                                                                                                                                                                                                                                                                                                                                 | level      | `tab(marital, tvhours)` · every crude `Obs_mean`                                                           |
| `level_n`        | the same, for a **count** — the row the library is missing today (`type = "n"` currently borrows `level_pct`, whose `est_field` is `pct`; `fmt_class.R:3304` documents the fudge)                                                                                                                                                                                                                        | level      | `tab(pct = "no")`                                                                                          |

Two things fall out of that table. First, **the library is already the shared vocabulary between
`tab()` and `tab_reg()`** — `odds_ratio`, `points`, `raw_diff` and `mean_diff` each serve a crosstab
column *and* a regression column today, which is why storing the key integrates the two producers
rather than describing them separately. Second, **`raw_diff` and `mean_diff` are one scale, not two**:
identical geometry, identical unit, identical ladder, differing only in where the SD comes from —
and that is derivable from a stored fact (`model_family` is non-empty on a regression column), so the
library can ship 9 rows rather than 10 and the `sd_from` split becomes a field of the row instead of
a reason to have two.

Verified against the goldens: the library reproduces **exactly** what the engine derives today —
`kind == "effect"` == `has_ci`, `mult` == `ci_mult`, `null` == `ci_neutral`, `est_field` ==
`fmt_center_field()`, on all 190 columns, the 8 OR-**reference** cells being the only difference —
and there the library is the *more* correct answer (it calls them odds-ratio columns, which is what
they are; today's `ci_type` calls them nothing because their bounds are NA). One gap found: `type = "n"` currently resolves to
`level_pct` with `est_field = "pct"` (`fmt_class.R:3304` documents the fudge) — the library needs a
`level_n` row.

**What it unlocks:** `fmt_est_field`/`fmt_est_of` become one attribute read, and (a)'s two rules
become one; `ci_center()`'s `ci_type`-then-`type` fallback chain disappears; `fmt_color_plan`'s
`is_mean` (`:3490`), `is_std_diff` (`:3495`), `is_logcoef` (`:3504`), `ci_mult` (`:3513`), `has_ci`
(`:3565`), `ci_neutral` (`:3566`) and `sd_ref` (`:3547`) all become one lookup; the legend stops
re-deriving its own copies of four of them (§4.1.5) so they cannot diverge; **`fmt_gap_scale_key()`'s
order-dependent `var` sniffing is deleted outright**, and with it the comment warning that the branch
order is the contract; the `gof` special case becomes a declared `geometry = "none"` (uncoloured by
declaration, not by a rule that exists to undo a storage decision); and the `log_odds` swap
(`fmt_class.R:3525`, a literal `measure == "diff"` test) becomes `scale == "log_coef"`.

---

#### Naming — including the maintainer's proposal to keep `type`

The proposal (§10 Q5): since most of `type`'s content goes to `pct_base`, let the **rest keep the
name `type`**. It adds no vocabulary at all, and it reads better still now that (b) deletes `ci_type`
— there is no second "type" left to confuse it with. The one fact that has to be weighed against it:

> **`type` is a released, exported, documented surface.** `get_type()` / `set_type()` are exported
> (`NAMESPACE:147,178`), and the programming vignette teaches the value list verbatim:
> *"`type` — `get_type()` / `set_type()`: `"n"`, `"mean"`, `"row"`, `"col"`, `"all"`, `"all_tabs"`
> (or `"coef"`…)"* (`tabxplor-programming.Rmd:197`). Any user script that tests `get_type(x) == "row"`
> keeps running and starts being **silently wrong**, because the value changes rather than the name.

And the symmetric trap: assigning `type` to the *base* half instead is no safer — `type == "mean"` is
tested at ~15 internal sites and would break the same way. **Whichever half keeps the name, the
released meaning changes.** So there are three options, and the third dissolves the problem:

| option                                                     | attributes                                       | back-compat                                                                                                                |
|------------------------------------------------------------|--------------------------------------------------|----------------------------------------------------------------------------------------------------------------------------|
| **1. `type` = the scale** (maintainer's proposal)          | `type` + `pct_base`, `ci_type` deleted → **14**  | `get_type()` silently returns new values; `get_ci_type()` (also exported) has to be derived or deprecated                  |
| **2. `type` = the base**, new name for the scale           | `type` + `scale`, `ci_type` deleted → **14**     | `get_type()` silently returns new values (`"mean"`/`"n"`/`"coef"` are not bases and must go somewhere)                     |
| **3. two honest names; `type` becomes a derived accessor** | `scale` + `pct_base`, `ci_type` deleted → **14** | **exact**: `get_type()` and `get_ci_type()` are both *computed* and return all their old values unchanged, soft-deprecated |

Note the count: because (b) deletes `ci_type`, **all three options land on 14 attributes — the same
number as today** (`type` splits in two, `ci_type` goes), before `ci_method` takes it to 15 below.

Option 3 is exact because both old accessors are a function of the new two. `get_type()`:
`level_pct` + base → `"row"`/`"col"`/`"all"`/`"all_tabs"`; `level_mean`/`mean_*` → `"mean"`;
`level_n` → `"n"`; the coefficient scales → `"coef"`. `get_ci_type()`: no interval → `""`; a level
scale with one → `"cell"`; `odds_ratio` → `"or"`; the ratio scales → `"ratio"`; the difference scales
→ `"diff"`. The reconcilers' `"mixed"` neutral becomes each attribute's own neutral, which also
retires a value `set_type()` itself rejects (`fmt_class.R:626`). Only option 3 costs no user
breakage, and it lets the fact be *named* for what it is, which is the whole point of the key.

On the name itself, if 3 is chosen: **`ci_scale` is already the package's own word for this** —
`tab_num(ci_scale = "ratio")` is a public argument meaning "the scale the interval is on"
(`tab.R:5020`) — so `scale` is the word with the least new vocabulary and the one z17 already uses
(`EST_SCALES`, `fmt_scale_of()`). Its price is one word doing double duty with the colour **break**
scales (`color_scales()`, `set_color_breaks()`). `quantity` and `metric` avoid the collision at the
price of that continuity.

#### The link with KEY 3a / KEY 8 — the argument asks a **geometry**, the attribute stores a **row**

KEY 8 (third pass) proposes `tab_reg(measure = c("odds_ratio", "ratio", "difference", "log"))`, and
`tab(color = <the same words>)`; KEY 2 stores one of ten row names. Four values against ten looks like
two vocabularies, and the natural worry is that they will need a lookup table kept in sync. They do
not, because they are **the same vocabulary at two granularities**:

> the argument names the **`geometry` column** of the library; the attribute names the **row**.

⚠ **This is also why KEY 2's attribute must NOT be called `measure`, and KEY 8's argument must not be
called `scale`.** One word at two grains is exactly the `type` / `ci_type` collision this key exists to
end. Pass 3 settles it: the attribute is **`scale`** (the row), the argument is **`measure`** (the
geometry) — and `measure` is already the package's own internal word for that vocabulary (`MEASURES`,
`measure_facts()`), which is what lets `tab()`'s `color` and `tab_reg()`'s `measure` share it.

`geometry` is already a column of the record (see the code block above), it already takes exactly the
values the argument would take — `ratio` · `difference` · `log`, plus `level` for the three
level rows — and the resolution is a lookup on **two columns the library already carries**:

```
row  =  the SCALES row whose  geometry == <what the user asked>
                        and   unit     == <what the outcome is measured in>
```

Measured end to end, on real columns of both producers — and note the last row, which is where the
first draft of this section was wrong:

| the user asks | outcome is measured in | resolved row | column built today                                                                                                                                                                                           |
|---------------|------------------------|--------------|--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| `ratio`       | a probability          | `odds_ratio` | `Model_OR`, and `Model_RR` under `effect = "ame_ratio"`                                                                                                                                                      |
| `ratio`       | counts                 | `odds_ratio` | `Model_IRR`                                                                                                                                                                                                  |
| `difference`  | a probability          | `points`     | `Model_AME` (binomial)                                                                                                                                                                                       |
| `difference`  | counts / outcome units | `raw_diff`   | `Model_AME` (poisson), gaussian **β**                                                                                                                                                                        |
| `log`         | the link scale         | `log_coef`   | `exponentiate = FALSE`                                                                                                                                                                                       |
| `ratio`       | outcome units (a mean) | `mean_ratio` | **nothing** — refused today (`effect = "ame_ratio"` aborts: *"needs a probability-scale outcome"*), although the row, the ladder and three CI engines all exist. A **gap, not an impossibility** — see KEY 8 |

Three consequences, in increasing order of value:

1. **The legality grid stops being hand-written.** §KEY 3a's caveat was "9 cells, 6 legal, so one
   legality table plus jamovi cross-greying". With this link, a combination is *available* exactly
   when a row exists with that geometry for that outcome — derived from the library, and the jamovi
   eligibility rule is generated from it rather than mirrored in JS (§7's standing anti-proposition).
   ⚠ **"Available" is not "meaningful"**: KEY 8 shows that several cells this study first called
   illegal are perfectly sound statistics that tabxplor simply does not offer yet.
2. **`effect` drops out of the resolution entirely.** Measured above: a binomial coefficient asked as
   a ratio and a binomial *marginal* asked as a ratio resolve to the **same row** (`odds_ratio`) —
   `effect` picks the *estimator*, geometry picks the *scale*, and they are genuinely orthogonal.
   That is the strongest available evidence for KEY 3a's Shape 1 over Shape 2: the flat list has to
   spell the geometry once per contrast precisely because it treats as one question what the storage
   model already treats as two.
3. **The same question is being asked on the crosstab side, through four different arguments** — and
   that is where D20/D21 come from. `OR = "OR"` asks for a **ratio** geometry; `ci = "diff"` asks for
   a **difference**; `ci_scale = "ratio"` asks for a ratio on a mean; `ci = "cell"` asks for a
   **level**. Nothing reconciles them, so when two disagree one silently wins — `OR` + `ci = "cell"`
   loses the odds ratios (D20), `OR` + `ci = "diff"` keeps the OR display over a percentage-point
   interval (D21). **Those two defects are the crosstab's version of exactly the confusion KEY 3a
   removes from `tab_reg()`**, and a shared resolution target is what makes them representable as a
   conflict instead of a coin toss.

**How far to take the integration — the honest limit.** Give `tab()` a `scale =` argument too? No.
A crosstab user asks with `pct` / `OR` / `ci`, which are the right questions *for a crosstab*, and
`OR` additionally picks a **dichotomisation** (which 2×2), which is not a geometry at all. What
should be shared is the **resolution target**, not the argument: both producers' arguments resolve
into one library row, exactly as `ref = "auto"` and `color = TRUE` already resolve into stored
values. The shared thing is the vocabulary of *answers*, not of *questions*.

**And one thing that must NOT be folded in**: the colour **measure** (`color = "diff"` / `"ratio"` /
`"OR"`, MEASURES) asks a similar-sounding question and is genuinely separate — a column can print a
percentage and be coloured by its *ratio* to the reference, and a two-channel column does two at once.
Estimate geometry and colour measure look alike and are not; keeping them apart is what lets
`color = c("OR", "adjustment")` mean something.

**Naming note.** This integration works whatever KEY 2's attribute ends up being called — including
the maintainer's option 1, where it keeps the name `type`. What is shared is the **geometry values**
(`ratio` / `difference` / `log` / `level`), not the attribute's name. So the naming choice above and
the KEY 3a argument name can be decided independently.

#### The "super type" question — what else should a column store?

The maintainer's question is whether this attribute can absorb enough to shrink the table-level
`meta` and reduce gating. The discipline that keeps it from becoming un-understandable is the one
above — **one key plus a declared library, never one attribute per fact** — with a single admission
test: *does this name a fact no other attribute can derive, and does a reader exist?*

| candidate                             | verdict                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     |
|---------------------------------------|-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| `scale` (the estimate)                | **yes** — the whole key                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     |
| `pct_base`                            | **yes** — a `points` difference can be a row% or a col% difference; `tab_ci()` picks the diff orientation from it (`tab.R:6025-6029`), and the contribution base reads it                                                                                                                                                                                                                                                                                                                                                                                   |
| `ci_type` / `ci_scale`                | **no — deleted.** (b) tested it column by column: the stored interval is always on the estimate's own scale, and "is there an interval here" is a data fact (all-NA bounds), not a vocabulary. `get_ci_type()` survives as a derived accessor                                                                                                                                                                                                                                                                                                               |
| `model_family`                        | **yes, already stored** — OR / IRR / RR share one scale and differ only by family, so the legend cannot derive its word from the scale                                                                                                                                                                                                                                                                                                                                                                                                                      |
| `role` (`model`/`emp`)                | **yes, already stored**                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     |
| **`ci_method`** (new)                 | **yes, and it pays for itself twice**: the CI method is a per-column fact (a cell interval used `method_cell`, a diff column `method_diff`, a mean diff `method_mean_diff`), yet it is stored table-wide in `meta$ci_settings` and picked back by *measure* in `legend_method_name()` — whose silent fall-through can print a method the bounds were never built with (§11 D8). Storing it per column makes D8 impossible **and empties `meta$ci_settings`**, since `conf_level` is already a column attribute. One attribute in, one `meta` sub-field out. |
| the **effect** (coefficient/marginal) | **no** — its only current reader is the `var`-sniff, which this key deletes; `reg_same_estimand()`'s pairing becomes `scale ==`. Revisit only when KEY 3a makes `effect` a first-class argument, and only if a reader appears                                                                                                                                                                                                                                                                                                                               |
| `conf_level` / `degf` / `basis`       | **already there and complete** — §Appendix calls the triple the template                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    |

Net: **14 → 15 attributes** (`type` splits into `pct_base` + `scale`, `ci_type` is deleted,
`ci_method` is added), **−1 `meta` sub-field**, and one order-dependent dispatch, six duplicated
field rules, seven derived predicates, one whole vocabulary (`ci_type`'s five values) and three
silent-divergence hazards deleted. One attribute more than today, several *rules* fewer — and, with
`ci_type` gone, one concept fewer as well.

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
ways.* In favour: `diff`/`or`/`ratio` are provably a discriminated union tagged by the scale, and the
package **already serves derived fields through `$`** — `$ci` was dropped as a stored field in Phase 1a
and is recomputed by `get_ci()`, `$tot_wn` is not a field at all, `$wn` falls back to `n`
(`fmt_class.R:3162-3181`) — so the user contract is *not* the blocker it looks like. Against: Phase z6
measured the whole sparse-record question and found the memory and speed cases empty, and the same
arithmetic applies; and `diff` ↔ `ratio` are simultaneously present and *both read* by the colour
engine's bound rescale (`fmt_class.R:3427-3440`). **Recommendation: store the scale, keep the
fields.** Revisit only past ~30 fields (z6's own re-open threshold).

**Recommendation, on the facts above.** **One** scale attribute — not two: `ci_type` is deleted,
which is the maintainer's reading and is better supported by the measurements than the draft's
two-attribute proposal. `pct_base` takes `type`'s other half, and `ci_method` is folded in as the one
extra attribute that pays for itself by emptying `meta$ci_settings` and closing D8. On the name,
option 3 (two honest names, with `get_type()` **and** `get_ci_type()` kept as derived soft-deprecated
accessors) is the only variant with no silent breakage of a released surface; `scale` is the word
with the least new vocabulary, and if its collision with the colour **break** scales is judged too
costly in prose, `quantity` is the same design under a different word. The shape of the key does not
depend on the name, so that choice can be made at implementation time without re-opening anything
above.

---

### KEY 3 — Most arguments are consequences, not choices

**The rule that is applied but not stated:** *the derivation graph between arguments.*

163 user messages, 72 % of them in the two argument boundaries. `tab_reg()`'s body is **484 lines of
code containing 92 `if`, 30 `is.null` and 32 CLI messages** whose entire product is a 13-key `spec`
list and a 24-key `shared` list; the actual work is one call at `tab_reg.R:5579`. `tab()`'s boundary
plus `tab_setup()` is the same shape.

Almost none of that is validation. It is **derivation**: computing an argument the user did not set,
from arguments they did, and telling them about it. The graph, gathered from both boundaries:

| derived                          | from                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     | site                                                     |
|----------------------------------|----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|----------------------------------------------------------|
| `exponentiate`                   | `family != "gaussian"`, 8 code sites. ⚠ **Twice corrected.** Pass 2 removed this argument from the cut list ("presentation only, a genuine choice"); pass 3 puts it back and the maintainer has **ruled delete** — measured, it is a silent no-op on the whole marginal path, is set in 0 of 49 taught calls, and its `FALSE` value *is* a measure (the model line calls it "log-odds coefficients"). It becomes `measure = "log"`, with `exponentiate = FALSE` kept as a documented synonym (§KEY 8.10) | `tab_reg.R:5145`                                         |
| `at = "reference"`               | degraded to `"average"` in **three** separate blocks                                                                                                                                                                                                                                                                                                                                                                                                                                                     | `tab_reg.R:5091, 5126, 5127`                             |
| `estimate_display ∈ {prob, ame}` | degraded away when `effect ∈ {ame, ame_ratio}`                                                                                                                                                                                                                                                                                                                                                                                                                                                           | `tab_reg.R:5169-5178`                                    |
| `empirical`                      | forced `TRUE` by `color = "adjustment"`                                                                                                                                                                                                                                                                                                                                                                                                                                                                  | `tab_reg.R:5261`                                         |
| `trials`                         | *is* a family variant (`crude_key = "grouped_binomial"`)                                                                                                                                                                                                                                                                                                                                                                                                                                                 | `tab_reg.R:257-262`                                      |
| `ci = "diff"`                    | forced by `color_signif ≠ ignore`, by `stars = TRUE`, by `color ∈ diff-family`, and again at the numeric leaf                                                                                                                                                                                                                                                                                                                                                                                            | `tab-resolve.R:118-127, 165, 177-182`; `tab.R:5163-5171` |
| `totrow`, `chi2`                 | forced `TRUE` by `color = "contrib"` (with a `warning()`)                                                                                                                                                                                                                                                                                                                                                                                                                                                | `tab-resolve.R:146-154`                                  |
| `ref`                            | *required* by a difference colour (abort); and `ci = "diff"` forces `ref = "tot"`                                                                                                                                                                                                                                                                                                                                                                                                                        | `tab-resolve.R:159-165`; `tab.R:5173-5176`               |
| `ref` **meaning**                | reinterpreted by `pct`: a reference **row** under `"row"`, a reference **column** under `"col"`                                                                                                                                                                                                                                                                                                                                                                                                          | `tab.R:1910-1932`                                        |
| `totaltab`                       | forced by `comp = "all"`, in **both** leaves, with warnings                                                                                                                                                                                                                                                                                                                                                                                                                                              | `tab.R:3919-3927`, `5206-5213`                           |
| `tot_cols_type`                  | forced to `"no_delete"` by `pct`/`ci`/`chi2`/`OR`                                                                                                                                                                                                                                                                                                                                                                                                                                                        | `tab.R:1975-1988`                                        |
| `color`                          | `TRUE` → per-column-type measures; `"auto"` → OR / after_ci / diff / contrib                                                                                                                                                                                                                                                                                                                                                                                                                             | `tab.R:1050-1073`; `tab-resolve.R:129-141`               |
| `basis`                          | `wt` × design × `design_effect`                                                                                                                                                                                                                                                                                                                                                                                                                                                                          | `survey-design.R:143-151`                                |
| `comp`                           | `"all"` collapses to `"tab"` without `tab_vars`                                                                                                                                                                                                                                                                                                                                                                                                                                                          | `tab.R:1522`                                             |
| `na = "common_base"`             | desugared into `na_drop_all = c(row_var, col_var[1], tab_vars)` + `na = "keep"`                                                                                                                                                                                                                                                                                                                                                                                                                          | `tab.R:738-743`                                          |

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
maintainer has explicitly waived back-compat. Two shapes were studied (§10 Q3); both remove the same
**4 formals and ~8 guard blocks**, and both make roughly a third of the 32 messages unreachable
because the illegal combination becomes unrepresentable. They differ in *how* the two questions are
put to the user.

The two questions are genuinely orthogonal, which is the finding that drives the choice:

1. **Which contrast?** the conditional model coefficient · the marginal effect averaged over the
   sample · the marginal effect at a reference profile.
2. **On which scale?** multiplicative (OR / IRR / RR) · additive (a difference in probability, in
   counts, in the outcome's units) · the link scale (log).

Today question 2 is asked **twice, in two different languages**: `exponentiate = FALSE` on the
coefficient path, and `effect = "ame_ratio"` on the marginal path. That is one concept with two
spellings — the disease, in the argument surface.

**Shape 1 — two arguments, one shared vocabulary.** ⚠ **Pass 3 chose this shape and named it**; the
second argument is `measure`, not `scale` (which KEY 2's attribute takes), and it gains an
`odds_ratio` value distinct from `ratio` — because on a binary outcome an OR and an RR are two
different statistics that users routinely conflate (§KEY 8.9). Read the block below as the sketch that
§KEY 8.9 finishes.

```r
effect  = c("coefficient", "marginal", "at_reference")            # WHICH contrast  (absorbs `at`)
measure = c("odds_ratio", "ratio", "difference", "log")           # WHICH effect measure
                                                                  #   (absorbs `exponentiate`,
                                                                  #    deletes `ame_ratio`,
                                                                  #    front-doors `family = "rr"`)
```

| combination              | today                  |
|--------------------------|------------------------|
| coefficient + ratio      | `exponentiate = TRUE`  |
| coefficient + log        | `exponentiate = FALSE` |
| coefficient + difference | gaussian β             |
| marginal + difference    | `effect = "ame"`       |
| marginal + ratio         | `effect = "ame_ratio"` |
| at_reference + ratio     | `at = "reference"`     |

*Simplification strength:* highest. `ame_ratio` disappears as a value rather than being kept, so the
grid shrinks as it grows: adding a scale later is one value, not N. And — the integration point —
its three values **are the `geometry` column of KEY 2's library**, so the argument that asks, the
attribute that stores, the legend that names and the plot axis that draws are one vocabulary end to
end, and the legality grid is derived from the library rather than written by hand (§KEY 2, "The link
with KEY 3a").
*Readability:* two short questions, each with three answers, neither of which mentions a link
function. It maps directly onto how the reg vignette already explains the choice.
*Caveats:* the grid has 9 cells, of which **6 are implemented today**. ⚠ The first draft of this
section called the other three "illegal"; KEY 8 re-checked that and it is **wrong** — a gaussian
`ratio` is a ratio of means (sound, and tabxplor already owns the scale and three CI engines for it),
and a binary `coefficient + difference` is an identity-link risk-difference model. What the missing
cells share is not meaninglessness but *a different fit*: see KEY 8. `at_reference + difference` is a
MER the code supports but does not currently label. In jamovi that
means one control greying out values of the second dropdown from the first — the pattern
`familyOptionsFor()` / `anyProbScale()` already implement. But the legality table itself **does not
have to be written**: see §KEY 2 *"The link with KEY 3a"*, where `scale` is shown to be the
`geometry` column of KEY 2's declared library, so a combination is legal exactly when a row exists
with that geometry for that outcome — derived, not maintained. The same section measures the fact
that most favours this shape: a binomial **coefficient** asked as a ratio and a binomial **marginal**
asked as a ratio resolve to the *same stored row*, so `effect` and the scale really are orthogonal.

**Shape 2 — one flat list, ratio variants inline** (the `ame` / `ame_ratio` pattern generalised).

```r
effect = c("coefficient", "coefficient_ratio", "coefficient_log",
           "ame", "ame_ratio", "mer", "mer_ratio")
```

*Simplification strength:* real but smaller — it still deletes `at` and `exponentiate`, but it keeps
question 2 spelled once per contrast, so it grows multiplicatively: a fourth contrast or a third
scale adds several values, and each new value must be taught, translated, added to the jamovi
ComboBox and matched in every `%in%` test.
*Readability:* flattest possible in jamovi (one dropdown, no cross-greying, no illegal
combinations to explain) and the value names are self-describing at the call site. Against that: the
same word means different statistics per family (`_ratio` is an OR for a logit, an IRR for a Poisson,
an RR under `family = "rr"`), and nothing in the argument tells the reader that
`coefficient_ratio` and `ame_ratio` share a scale — which is the fact the colour legend, the forest
plot axis and the crude-twin pairing all depend on.
*Caveats:* it cannot share KEY 2's vocabulary, so the mapping from the argument to the stored column
scale becomes a lookup table that has to be kept in sync — a small one, but the kind this document
exists to remove. Measured cost of that choice: because `coefficient + ratio` and `marginal + ratio`
land on the *same* stored row, the flat list spells one question twice and the lookup has to encode
that two of its values mean the same scale (§KEY 2, "The link with KEY 3a").

**On the honest trade.** Shape 1 is the better *model* and the better fit with KEY 2; shape 2 is the
better *widget*. The deciding question is whether jamovi's cross-greying is acceptable — and the
module already does exactly this for `family` × `effect` × `exponentiate`, so the machinery is
present, and shape 1 would *reduce* the number of controls it has to grey.

**A third fold, which the maintainer asked for and which turns out to be free.**
`estimate_display` is **already** a preset over `tab()`'s display grammar: `"ci"` sets
`display = "est_ci"`, `"prob"` sets `"{or} ({pct})"`, `"ame"` sets `"{or} ({diff})"`
(`reg_apply_estimate_display`, `tab_reg.R:1436-1463`). So replacing it with a real
`display =` argument mirroring `tab()`'s is not new machinery — it is *deleting a preset layer* and
exposing what is underneath, with the four current values kept as documented shorthands. It also
states a rule worth generalising to the crosstab side: **the display template declares which
quantities the column must carry, and the builder computes exactly those** — which is what
`estimate_display = "prob"` already does when it calls `reg_marginal()` to fill the `pct` field it is
about to print, and what `tab()` makes the user do by hand today (`display = "{pct} ({ci})"` does not
imply `ci = "cell"`; the user must ask for both, or get a blank bracket).

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
the returned object** (it was only meant to be used in .Rmd/.qmd with knitr but stays quite dangerous).
The fourth row is KEY 1's limit surfacing as an unpredictable return type.

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

### KEY 8 — A table makes one comparison; only `tab_reg()` has to be told which

**Third pass (2026-08-13), re-opened from scratch at the maintainer's request** — *"restate the whole
problem and find the real key; I would rather have a well-argued demolition than a polished defence."*
It is a demolition, and it produced a replacement that is **smaller** than what it replaces.

**The fact that is missing:** not a name for the comparison — *a rule about where the comparison is
named.* And the rule is different on the two producers, for a reason that is mechanical, not
stylistic:

> On a **crosstab**, every comparison geometry is a function of the *same* sufficient statistics the
> aggregate already holds. Asking for one is a **selection**, and the package already has three
> selectors for it.
> On a **regression**, a geometry is a *different fit or a different estimator*. Asking for one is a
> **modelling decision**, and it has nowhere to live but an argument.
>
> KEY 8 as written in passes 1–2 gave both producers the same argument. That is the over-collapse.

---

#### 8.1 Three claims from the first two passes, withdrawn

| claim (passes 1–2)                                                                                | status after re-measurement                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            |
|---------------------------------------------------------------------------------------------------|------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| *"D20 and D21 are not two unrelated bugs; they are one missing argument."*                        | **Withdrawn.** Measured: the same incoherence is reachable with **no `OR` and no `ci` conflict at all** — `tab(…, ci = "diff", display = "{ratio} ({ci})")` prints `×1.8 ([2;4]%)`, a ratio over a percentage-**point** interval (§11 D23). The class is *display-geometry ≠ interval-geometry*, and `display` must stay free (differentiator #1), so **no argument can close it**. It is closed by KEY 2 + one rule (§8.6).                                                                                           |
| *"`compare` is a clean −2 formals on `tab()`, where it displaces four tabxplor-only spellings."*  | **Withdrawn.** Three of the four "spellings" are not spellings of one question: `display`, `color` and `ci` select *what is printed*, *what is coloured* and *what is tested* — three genuinely independent questions over the same cell, which is the whole point of the record. A fourth argument over the same facts is the disease, not the cure.                                                                                                                                                                  |
| *"`compare` … on both producers"* (and the whole §"the asymmetry" attempt to rescue the reg half) | **Withdrawn on the name, and the name matters:** `compare` is **already a `tab_reg()` formal** — `compare = c("none", "baseline", "sequential")`, the model-comparison footer (`tab_reg.R:4928`, `man/tab_reg.Rd:30,429`). Two passes proposed an argument name that collides head-on with a live documented one. That is not a coincidence: in a regression table "compare" means *compare the models*, and in a crosstab it means *compare to the reference*. The word cannot carry the estimand on either producer. |

---

#### 8.2 The measurement that decides the crosstab half

On `tab()`, **the comparison geometry is not an input.** Measured on `gss_simple`, one plain call with
no `ci`, no `OR`, no `color`:

```r
tab(d, race, marital, pct = "row")        # column "Separated"
    pct     0.0267   0.0626   0.0562   0.0346
    diff   -0.0079   0.0281   0.0216   0.0000     <- the difference, computed
    ratio   0.7707   1.8112   1.6235   1.0000     <- the ratio, computed
```

Both geometries are in **every cell of every percentage table, unconditionally**, and the user selects
between them afterwards — measured, all three selectors work today and are independent:

| selector  | how the geometry is named       | measured output                         |
|-----------|---------------------------------|-----------------------------------------|
| `display` | `"{diff}"` / `"{ratio}"`        | `-1% +3% +2%` · `÷1.3 ×1.8 ×1.6`        |
| `color`   | `"diff"` / `"ratio"`            | display unchanged (`pct`) — colour only |
| `ci`      | `"diff"` / `"ratio"` / `"cell"` | `ci_type` `diff` / `ratio` / `cell`     |

The odds ratio is the one geometry that is **not** free — it needs a *dichotomisation* (which 2×2),
which is what `ref2` supplies. That is a genuine build input. It is also the only one.

**This is not an accident of implementation; it is differentiator #1.** "A cell carries everything, so
display is switchable losslessly" *means* the geometries are all present. An argument that made the
user choose one at build time would delete the differentiator.

---

#### 8.3 `color` already asks the question — and already answers it correctly

The maintainer's reading (*"maybe it's a `color` + `display` thing that does not need `OR` and `ci`"*)
is right, and the redundancy is **larger than either earlier pass claimed**. Four measurements:

| measured                                                                                                                                                                     | consequence                                                                                                                       |
|------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|-----------------------------------------------------------------------------------------------------------------------------------|
| `tab(…, color = "OR")` **with no `OR =` argument** → the `or` field is computed (`1.00 4.29 2.25 1.40`)                                                                      | `color` **already** forces the odds-ratio computation. The `requires` mechanism KEY 4 proposes is, for this measure, already live |
| `color = "diff"` + `color_signif` → `ci_type = "diff"`; `color = "ratio"` → `ci_type = "ratio"`; `color = "OR"` → `ci_type = "or"` (a real Woolf interval, `3.58 1.80 1.24`) | `color` **already** selects the correct interval geometry, for all three geometries, with no `ci` argument                        |
| `stars = TRUE`, `color = "no"` → `ci_type = "diff"`                                                                                                                          | `stars` declares its own requirement too                                                                                          |
| `color = "diff"` + `color_signif` + `ci = "cell"` → **a clean abort**                                                                                                        | the incoherent combination is *already* refused in one place (17d). The rule exists; it is just not general                       |

So on `tab()` the question "which comparison does this table make?" **is already asked, once, by
`color`** — which is the argument the intro vignette sets **36 times**, against `ci` 8, `display` 8 and
`OR` **2**. Adding `compare` would put a fourth word beside the hottest argument's own vocabulary.

---

#### 8.4 `OR` is `color` + `display` welded — and the odds ratio should just always be computed

Its five values decompose exactly, measured value by value:

| `OR =`                  | the measure | the display                               | the dichotomisation                            |
|-------------------------|-------------|-------------------------------------------|------------------------------------------------|
| `"OR"` / `"or"`         | odds ratio  | `{or}` → `1.00 4.29 2.25`                 | `ref2` (a level)                               |
| `"OR_pct"` / `"or_pct"` | odds ratio  | `{or} ({pct})` → `1 (51%)` in the ref row | `ref2` (a level)                               |
| `"cumOR"`               | odds ratio  | `{or}`                                    | **cumulative** (an `ordered` col_var, per-cut) |

One measure, two displays, two dichotomisations. Nothing else. **`OR` is not a fourth question; it is
three existing answers welded into one argument**, and the weld is where D20 and D21 live.

**The maintainer's proposal — "always calculate the OR with difference and ratio if it's cheap
enough" — is right, and it is measured to be free.** Timed on a 3-`row_var` × 2-`col_var` table,
5 replicates, isolating the computation (`color = "OR"` fills the `or` field without changing the
display):

```
color = "diff"   (no odds ratio)     340 ms   (min 240)
color = "OR"     (or field filled)   300 ms   (min 223)
```

The odds-ratio path is **not measurably more expensive** — it is within noise of the difference path,
and marginally faster in the median. That is structurally unsurprising: the 2×2 it needs is
`(cell, ref-level, ref-row, ref-row × ref-level)`, four numbers the wide table already holds, and it
is computed in the same `tab_apply_reference()` sweep that already produces `diff` and `ratio`.

**The scope is self-limiting, measured**: `color = "OR"` on a counts table (`pct = "no"`) and on a
mean column both leave `or` **all-NA**. So "always" means *on `type ∈ {row, col}` percentage columns*
— exactly where an odds ratio is defined, and exactly the gate the html tooltip already uses
(`type %in% c("col", "row") & !is.na(get_or(x))`, `tab_classes.R:2338`).

**So `OR` is deleted** (soft-deprecated — it is CRAN-released, §8.6.6). The odds ratio joins `diff` and
`ratio` as a third always-present comparison; `display = "{or}"` shows it; `color = "odds_ratio"`
colours by it; **`ref2` alone chooses the dichotomisation** (`"first"` / `"last"` / an index /
`"cumulative"` — ruling (b)). One field, one dichotomisation per table, no argument.

**Three consequences to declare before implementing, all measured:**

1. **The html tooltip gains an `OR:` line on every row/col-% cell.** Its gate is exactly
   "`or` is not NA" (above), so populating the field turns it on. Defensible — the tooltip already
   carries `diff`, `ratio` and `contrib`, and the odds ratio joins them — but it is a **visible**
   change and it moves `_snaps/render-html.md`.
2. **The structural goldens move**: an all-NA field becomes populated on every percentage table.
   `dev/verify_golden_field_delta.R` needs one more mode — *"the only delta is a populated `or`
   column"* — beside the field / attribute / `test`-column / `meta`-sub-field modes it already has.
3. **`ref2` becomes always-in-force**, where today it is consulted only when an OR is asked. The
   default `"first"` always exists, so the ordinary path is unaffected — but ⚠ **`ref2 = "last"` is
   broken today** (measured: *"in ref2 = 'last', no columns were found as reference for comparison"*,
   and the `or` field comes back all-NA). Under always-compute that warning would fire on tables that
   never asked for an odds ratio. **Fix `ref2 = "last"` first**; it is a one-line reference-matching
   bug, not a design question (§11 D27).

**And the discoverability question the maintainer raised is the real cost of this change.** `OR = "OR"`
was the *findable* way to get an odds ratio; `display = "{or}"` is currently "expert stuff". The
maintainer's own answer is the right one and it is already half-built: **make `display` a central
control, in jamovi beside `pct` and `color`, offered as presets that show the meaningful label **and**
the `{}` template.** That is not new machinery — `tab_reg()`'s `estimate_display` is *already* exactly
that (a preset layer over the grammar: `"prob"` → `"{or} ({pct})"`, `tab_reg.R:1436-1463`), and KEY 3a's
third fold already proposes replacing it with a real `display =`. So the two converge on **one
`display` grammar for both producers, with presets in the UI** — and a preset list that shows
`Odds ratio  {or}` teaches the R spelling at the moment of use, which is the module's stated purpose
(§1, differentiator #4).

---

#### 8.5 `ci` is not a geometry either — but it is not a logical, and the maintainer is right about why

Pass 3's first draft proposed collapsing `ci` to `TRUE`/`FALSE`. **The maintainer's objection is
correct and the proposal is withdrawn:** *"`ci = TRUE` with no colour does not clearly state that this
is the cell/level CI, and the right default differs depending on whether a comparison is being
made."* A bare logical cannot say *which* interval, and there are two, they are mutually exclusive in
storage, and they serve different consumers:

| interval           | what it answers                       | who reads it                                      |
|--------------------|---------------------------------------|---------------------------------------------------|
| **level** ("cell") | how precise is this 26 %?             | `display` only — never colour, never stars        |
| **comparison**     | is this different from the reference? | `color_signif`, `stars`, `forest_plot`, `display` |

There is **one** `ci_inf`/`ci_sup` pair, so they cannot coexist. That makes "where does the interval
sit" a genuine, single, meaningful question — which is what `ci` has always meant, minus the geometry:

```r
ci = c("auto", "no", "cell", "comparison")
```

- The **geometry is gone**: `"diff"` / `"ratio"` are soft-deprecated onto `"comparison"`, and *which*
  comparison comes from `color`, which already decides it correctly for all three geometries (§8.3).
- **`ci = "cell"` does not move at all** — same name, same value, same behaviour. It is the most-used
  value and the beginner path, and it needs no deprecation. (This is a better outcome than the
  maintainer's `ci_cell = TRUE`, which would add a formal, split one question across two arguments,
  and create an illegal `ci_cell = TRUE, ci = "comparison"` cell.)
- **`"auto"`** (the default) = a comparison interval when a comparison is being *tested*
  (`color_signif` ≠ `"ignore"`, or `stars`), else none. That is **exactly today's hidden forcing
  cascade**, promoted to a documented default.
- **`"comparison"`** is the explicit opt-in — a forest plot with intervals but no colour-gating, or
  bounds to read programmatically.

**On "should `color` alone trigger the comparison CI?" — measured: no.** This is the one place where
the answer is a number, and it is decisive:

```
ci = "no"                  226 ms
ci = "diff"                312 ms        --> +86 ms, +38 %
ci = "diff" + stars        332 ms
wide table, ci = "no"      422 ms
wide table, ci = "diff"    466 ms        --> +44 ms, +10 %
```

The comparison interval is **the single largest avoidable cost in the crosstab build**, and `color` is
the hot argument (36 uses in the intro vignette). Triggering intervals on every coloured table would
tax the common case for a benefit only `forest_plot()` and `color_signif` use. **So the trigger stays
`color_signif` / `stars` / an explicit `ci = "comparison"`** — today's rule, now justified by a
measurement instead of by accident, and `ci = "comparison"` is the cheap explicit route the maintainer
was willing to accept ("I can live with forest plots not giving CI unless…").

**"Without `color`, how does `stars` know the comparison?" — measured: it falls back to the
difference**, and correctly so: `pct = "row"`, `pct = "col"` and a mean column all give
`ci_type = "diff"` with `color = "no"`. The difference is the right default comparison.

**But the fallback exposes a live inconsistency (§11 D26).** `stars` and `color_signif` disagree about
what an odds-ratio-coloured table compares, **and `stars` wins**:

```r
tab(d, race, marital, pct = "row", color = "OR", stars = TRUE)
#   ci_type = "diff"   bounds 0.0276  0.0198  0.0044      <- percentage POINTS
tab(d, race, marital, pct = "row", color = "OR", color_signif = "grey_non_signif")
#   ci_type = "or"     bounds 3.576   1.803   1.237       <- the Woolf OR interval
tab(d, race, marital, pct = "row", color = "OR", stars = TRUE, color_signif = "grey_non_signif")
#   ci_type = "diff"   <- stars wins; the colour gate now tests a difference
#                         on a table whose cells display odds ratios
```

Two consumers of one fact, two answers. It is D21's shape reached by a third route, and unlike D21 it
is **not** latent — it decides which cells are greyed. Under this design it is unrepresentable:
`color` names the comparison **once**, and `stars`, `color_signif`, `display` and `forest_plot` all
read that one interval.

**And `ci = "cell"` must disable the significance machinery with a message** — the maintainer's point,
and today the two consumers behave differently: `color_signif` + `ci = "cell"` **aborts** with a good
message, while `stars = TRUE` + `ci = "cell"` **silently drops the stars** (measured: `pvalue` all NA,
nothing rendered — §11 D28). Unify them: one message, both disabled, from one rule. Recommendation:
*inform and disable* rather than abort — `ci = "cell"` is an explicit later choice and the user should
get their table.

---

#### 8.6 The caveats, honestly

1. **One cell, one interval — so colour and display compete.** `ci_inf`/`ci_sup` hold *one* geometry.
   "Colour by the difference" (needs a difference interval) and "print the level's bracket" (needs a
   cell interval) cannot both be satisfied. This is **not introduced by the redesign** — it is
   measured today, and today it already aborts with a good message. What the redesign changes is that
   the conflict becomes representable in *one* place instead of three, so the general rule can be
   stated: **the stored interval is the one the table's comparison is tested on; a `{ci}` bracket
   renders that interval.** With KEY 2's stored scale, `format()` can then refuse — or convert — a
   bracket whose geometry differs, which is what closes D21 and D23.
2. **A conversion is available, and it is currently used in only one of the two places it is needed.**
   `rescale_bound()` (`fmt_class.R:3605-3618`) already maps a difference bound onto a ratio bound and
   back, so the colour engine can colour by the ratio while the interval is on the difference. `format()`
   has no such step, which is exactly why D21/D23 print a mismatched bracket. Whether that rescale is
   exact enough to *print* (it is a linear re-anchoring, adequate for a threshold) must be settled
   before choosing "convert" over "refuse". **Recommendation: refuse, and let `color`/`display` agree
   by construction** — a printed interval must be the interval, not an approximation of one.
3. **`color` is the primary geometry source, not the only one.** `color = "no"` must keep meaning
   "don't colour", so the resolution is a declared chain: **`color`'s text channel → `display`'s primary
   token → `difference`**. One order, declared once, in the same table as KEY 4's `requires`. This is
   the same shape as `ref = "auto"`; it is not a new mechanism.
4. **Two-channel colour still works, and pins the rule.** `color = c("difference", "ratio")` colours
   the text by one geometry and the background by the other. The **text channel names the tested
   comparison** (today's behaviour); the background is a secondary reading and rides `rescale_bound`.
   This is the reason the geometry belongs in `color` rather than in a separate argument: a separate
   argument would have to be reconciled with the channel vector, and the channel vector would win.
5. **`cumOR` needs a home, and `ref2` already is one.** It is not a different measure — it is a
   different *dichotomisation of the column variable* (per-cut `P(Y ≤ j)` vs `P(Y > j)` instead of
   level-vs-reference), and `ref2` is precisely "what is each level compared against, within the column
   variable". So `ref2 = "cumulative"` beside `"first"` / `"last"` / an index. **That removes the last
   obstacle to retiring `OR`.**
6. **`OR` and `ci` are CRAN-released** (both present in `v1.2.0`'s `tab()` — verified from the tag), so
   they are **soft-deprecated with routes**, not deleted:

   | old                           | new                                                   |
   |-------------------------------|-------------------------------------------------------|
   | `OR = "OR"`                   | `display = "{or}"` (the field is now always computed) |
   | `OR = "OR_pct"`               | `display = "{or} ({pct})"`                            |
   | `OR = "cumOR"`                | `ref2 = "cumulative"`                                 |
   | `ci = "cell"`                 | **unchanged — the value survives verbatim**           |
   | `ci = "diff"` / `"ratio"`     | `ci = "comparison"`, geometry from `color`            |
   | `tab_num(ci_scale = "ratio")` | `color = "ratio"` (the value is cut outright)         |

   Only two of the six are real deprecations for a user who ever typed them; `ci = "cell"`, the
   most-used value, does not move.

---

#### 8.7 The resulting `tab()` surface — every argument a meaningful choice

```r
pct           what is in the cell                        n | row% | col% | all% | mean
ref, ref2,
comp          what it is compared to                     ref2 now also carries "cumulative"
color         WHICH comparison, and how it is coloured   difference | ratio | odds_ratio | contrib | …
ci            WHERE the interval sits                    auto | no | cell | comparison
stars         do I want stars                            logical
color_signif  how significance changes the colour        ignore | grey_non_signif | guaranteed_effect
display       what is printed                            the {} grammar, 12 tokens
```

Seven arguments, each a genuine question with one answer. **`diff`, `ratio` and now `or` are all
computed unconditionally**, so nothing here says *what to compute* — only what to compare against,
what to test, and what to show.

Deleted from the surface: **`OR`** (three existing answers, welded — §8.4) and
**`tab_num(ci_scale =)`**, a *pure duplicate*: measured, `tab(…, ci = "ratio")` already yields
`ci_type = "ratio"`, and the resolver desugars `ci = "ratio"` into `ci = "diff"` + `ci_scale = "ratio"`
internally (`tab-resolve.R:71-72`). `ci_scale` is an exposed internal, used **0 times** anywhere.

---

#### 8.8 Why `tab_reg()` cannot take the same argument — the principled divergence

This is the answer to the maintainer's tension #2, and it is mechanical rather than a matter of taste.
Measured, on `tab_reg()` the geometry is **not** a selection over stored facts:

| geometry                      | mechanism                                                      | cost                      |
|-------------------------------|----------------------------------------------------------------|---------------------------|
| odds ratio                    | a logit fit                                                    | **a fit**                 |
| risk ratio (conditional)      | a log-link / modified-Poisson fit (`family = "rr"`)            | **a different fit**       |
| risk difference (conditional) | an identity-link fit                                           | **a different fit**       |
| risk ratio (marginal)         | g-computation over the logit fit (`comparison = "lnratioavg"`) | **a different estimator** |
| risk difference (marginal)    | the AME over the same fit                                      | a different estimator     |
| log scale                     | the same numbers, unexponentiated                              | free                      |

Only the last row is free. So on `tab_reg()` the choice **is** an input, it **is** a modelling
decision, and it must not be hidden inside a formatting argument — *changing `display` must never
change the model.* That single line is what separates the two producers, and it is why one shared
argument was always going to be an over-collapse.

Measured, today's four-argument encoding of that one decision is 4× redundant:

> `family` × `effect` × `at` × `exponentiate` = **36 combinations → 9 distinct estimands, 8 aborts**,
> and ~19 cells in which at least one argument is **silently ignored**.
> (`exponentiate` is a no-op on the entire marginal path; `at` is a no-op on the entire coefficient
> path — both measured this pass.)

That is the real disease on the reg side. It is not that the arguments are redundant *spellings*; it
is that the space is a **product in which most cells are inapplicable**, and each new argument
multiplies it.

---

#### 8.9 The reg-side proposal: two axes, and only two

The minimal non-redundant parameterisation of an estimand is **(which contrast) × (which measure)**:

```r
effect  = c("coefficient", "marginal", "at_reference")   # WHICH contrast   (absorbs `at`)
measure = c("odds_ratio", "ratio", "difference", "log")  # WHICH effect measure
                                                         #   (absorbs `exponentiate`, `ame_ratio`,
                                                         #    and `family`'s scale half)
```

Everything else is either the outcome's own property (`family`, auto-detected and already announced)
or a component of one of the two. Measured, the two are genuinely orthogonal: a binomial
**coefficient** asked as a ratio and a binomial **marginal** asked as a ratio land on the *same stored
scale row* — `effect` picks the estimator, `measure` picks the scale.

| the user writes                          | today                                        | column      |
|------------------------------------------|----------------------------------------------|-------------|
| *(nothing — binary outcome)*             | `family = "binomial"`                        | `Model_OR`  |
| `measure = "ratio"`                      | `family = "poisson"` **on a binary outcome** | `Model_RR`  |
| `measure = "log"`                        | `exponentiate = FALSE`                       | `Model_β`   |
| `effect = "marginal"`                    | `effect = "ame"`                             | `Model_AME` |
| `effect = "marginal", measure = "ratio"` | `effect = "ame_ratio"`                       | `Model_RR`  |
| `effect = "at_reference"`                | `at = "reference"`                           | `Model_MER` |

Note row 2. **`family = "rr"` is refused when asked for directly** — measured: *"`family` must be one
of gaussian, binomial, poisson, quasipoisson, multinomial, ordinal"* — so the only route to a risk
ratio is to name the wrong distribution. `measure = "ratio"` is the front door, and it is the change
with the largest ratio of user value to work in this whole key.

**On the name.** `measure` (not `scale`, not `compare`):

- **`compare` is taken** (§8.1) and means something else on this very function.
- **`scale` is taken by KEY 2 at a different granularity** — KEY 2's column attribute stores a
  *library row* (`odds_ratio`, `points`, `raw_diff`, `level_pct`…); this argument names the row's
  **geometry**. Same word at two grains is exactly the `type` / `ci_type` confusion KEY 2 exists to end.
- **`measure` is the package's own internal word** for this vocabulary (`MEASURES`, `measure_facts()`,
  `measure_policy()`), and it is the discipline's word ("effect measure": RD / RR / OR / IRR).

**On the values** — the maintainer's ask (*"both the acronym and the full word working, teaching the
full word"*) is exactly right, and it is what makes the value set stop growing with the family:

| canonical      | aliases accepted           | what the table prints    |
|----------------|----------------------------|--------------------------|
| `"odds_ratio"` | `"OR"`, `"or"`             | `Model_OR`               |
| `"ratio"`      | `"RR"`, `"IRR"`, `"rr"`    | `Model_RR` / `Model_IRR` |
| `"difference"` | `"diff"`, `"RD"`           | `Model_AME` / `Model_β`  |
| `"log"`        | `"log_odds"`, `"log_rate"` | `Model_β`                |

The full word is family-independent (so the legal set does not multiply, which is what sank KEY 3a's
Shape 2); the acronym is what the reader knows and what the column header shows. **The table therefore
prints the mapping between the two every time it renders**, which is the honest answer to
"not everyone reads vignettes" — and it is why the *argument* should teach the concept word while the
*output* keeps the discipline's acronym.

---

#### 8.10 `exponentiate` is deleted — maintainer's ruling, and the measurements support it

This **reverses the second pass**, which recommended keeping it. The evidence that changed:

- it is a **silent no-op on the whole marginal path** — measured, `effect = "ame_ratio",
  exponentiate = FALSE` returns the exponentiated `Model_RR` unchanged;
- it is set in **0 of the 49** `tab_reg()` calls in the regression vignette (one prose mention);
- its `FALSE` value **is a measure** — the package's own model line calls it *"log-odds coefficients"*;
- keeping it beside `measure` creates contradictory cells (`measure = "odds_ratio", exponentiate = FALSE`).

`measure = "log"` replaces it. The transfer cost is real (`exponentiate` is the one portable name in
broom / parameters / gtsummary / easystats) and is paid **one-directionally**: accept
`exponentiate = FALSE` as a documented synonym for `measure = "log"` in the deprecation shim and in
`?tab_reg`'s "how this is called elsewhere" line, so an expert's existing knowledge is a *ramp in*
rather than something to unlearn. Same for `link = "log"` → `measure = "ratio"` on the coefficient path.

⚠ **Correction to §KEY 3.** Its derived-arguments table carries a note (added in pass 2) saying
`exponentiate` "does **not** belong on the cut list". That note is now wrong on the maintainer's
ruling and on the measurements above; §KEY 3's original reading — that `exponentiate` is a
consequence — was closer, though for the wrong reason (its *default* is derived from `family`; its
*value* is a measure, not a presentation).

---

#### 8.11 Closing the capability gaps — maintainer's ruling

Confirmed by measurement, and it is a genuine inconsistency **between the two producers**:

```r
tab(d, race, tvhours, ci = "ratio")     # a ratio of means: works, ci_type = "ratio"
tab_reg(d, "tvhours", …, effect = "ame_ratio")
#   x `effect = "ame_ratio"` needs a probability-scale outcome
```

tabxplor already owns the `mean_ratio` scale row, its break ladder and three `ci_mean_ratio` engines —
`tab()` uses them; only `tab_reg()` refuses. So `measure = "ratio"` on a gaussian outcome (a ratio of
means: standard for log-wages and any positive outcome) and `measure = "difference"` on the
coefficient path for a binary outcome (the identity-link additive-risk model) both become available.

**This makes the legality table three-state, and that is the point.** A user must be able to tell
*"we don't offer that"* from *"that cannot be done"* from *"the link did not converge on your data"* —
today the first two produce the same abort. The third state is new and unavoidable: an identity or log
link can fail to converge where a logit does not.

**The part of this that is not optional.** The resolution table must ship as a **runtime object**, not
prose, with four consumers: the generated `?tab_reg` section, the error message (enumerated from the
table, therefore always correct, delivered at the moment of the mistake), a lister the user can call on
their own outcome, and the jamovi eligibility rule (generated, not hand-mirrored in JS — §7's standing
anti-proposition). **The package already does exactly this twice**, which is the model to copy:
`reg_detect_family()` announces what it detected, and `OR = "cumOR"` on a non-`ordered` column prints
*"needs an `<ordered>` col_var with 3+ levels; it is skipped here"* **followed by the `mutate()` line
that fixes it** (measured). That is the standard.

---

#### 8.12 The integration — one vocabulary, two hosts

The two producers do not share the *argument*; they share the **words**, and the words are already the
package's own:

```
        the vocabulary          tab()                       tab_reg()
        --------------          -----                       ---------
        difference / diff / RD  color = "difference"        measure = "difference"
        ratio      / RR  / IRR  color = "ratio"             measure = "ratio"
        odds_ratio / OR         color = "odds_ratio"        measure = "odds_ratio"
        log                     —  (a crosstab has no link) measure = "log"
        contrib                 color = "contrib"           —
        adjustment              —                           color = "adjustment"
        between_groups          —                           color = "between_groups"
```

`tab()` names it in `color` because there it *is* a selection (and `color` already forces the
computation, §8.3). `tab_reg()` names it in `measure` because there it is an estimand. Both resolve
into **one KEY 2 library row**, so the argument that asks, the attribute that stores, the legend that
names and the forest-plot axis that draws are one vocabulary end to end — which is what KEY 8 was
reaching for, obtained without a third vocabulary and without a shared argument.

**And it removes a silently-wrong state on the reg side.** Measured: `tab_reg(color = "diff")` on an
odds-ratio column, and `tab_reg(color = "OR")` on an `exponentiate = FALSE` β column, are both
*accepted* and store a colour measure that contradicts what the column estimates (§11 D25). Once
`measure` names the estimand and KEY 2 stores its scale, `tab_reg()`'s `color` needs only
`TRUE`/`FALSE` + `"adjustment"` / `"between_groups"`: the ladder comes from the column. **−2 values,
and one nonsense state made unrepresentable.**

---

#### 8.13 The white-elephant test, per argument

| argument                   | who sets it, how often                                                                                         | if they never do                                | verdict                                    |
|----------------------------|----------------------------------------------------------------------------------------------------------------|-------------------------------------------------|--------------------------------------------|
| `tab(color =)`             | **36×** in the intro vignette — the hot argument                                                               | no colour, the differentiator is off            | **keep, extend**                           |
| `tab(ci =)`                | 8×                                                                                                             | no intervals                                    | **keep; drop the geometry, keep `"cell"`** |
| `tab(display =)`           | 8×                                                                                                             | the default per scale                           | **keep, make declarative**                 |
| `tab(OR =)`                | **2×**                                                                                                         | `color`/`display` already reach it              | **deprecate**                              |
| `tab_num(ci_scale =)`      | **0×** anywhere                                                                                                | `ci = "ratio"` is the same thing                | **cut**                                    |
| a new `tab(compare =)`     | **0× — it does not exist and nobody asked for it**                                                             | —                                               | **do not add**                             |
| `tab_reg(effect =)`        | 5 of 49                                                                                                        | the coefficient                                 | **keep, absorb `at`**                      |
| `tab_reg(family =)`        | 13 of 49, and it announces its own auto-detection                                                              | detected                                        | **keep, drop its scale half**              |
| `tab_reg(exponentiate =)`  | **0 of 49**, and a no-op on 2/3 of the grid                                                                    | —                                               | **delete**                                 |
| `tab_reg(at =)`            | rare; degraded away in **three** separate blocks                                                               | `"average"`                                     | **fold into `effect`**                     |
| a new `tab_reg(measure =)` | would be the front door to a measure that is **currently unreachable except by naming the wrong distribution** | today: `family = "poisson"` on a binary outcome | **add**                                    |

The row that matters is the last: **the only genuinely new argument in this key is the one that makes
an existing, sound, already-implemented estimand askable.** Everything else is subtraction.

---

#### 8.14 Teachability

The transferable lesson is the two decisions every framework forces, and neither is tabxplor's:

| framework                              | "which contrast?"                             | "which measure?"                                                                                     |
|----------------------------------------|-----------------------------------------------|------------------------------------------------------------------------------------------------------|
| **Stata**                              | `logistic` then `margins, dydx()`             | `binreg y x, or ǀ rr ǀ rd ǀ hr` — one option per measure, **`or` assumed when no link is specified** |
| **marginaleffects** (0.32.0, verified) | `coef()` vs `avg_comparisons()`               | `comparison = "difference" ǀ "ratio" ǀ "lnratio" ǀ "lnor" ǀ "lift" …` (30 shortcuts)                 |
| **`risks`** (Stopsack)                 | `approach =` (incl. marginal standardization) | **the function name**: `riskratio()` / `riskdiff()`                                                  |
| **emmeans**                            | `emmeans()` + `contrast()`                    | `type = "link" ǀ "response"`                                                                         |
| **broom / parameters / gtsummary**     | —                                             | `exponentiate = TRUE/FALSE`                                                                          |
| **epidemiology**                       | conditional vs marginal                       | *the effect measure*: RD · RR · OR · IRR                                                             |

Two things follow. First, **there is no standard argument *name*** — every framework invents one — but
the *concept* has a standard name, "effect measure", which is what `measure` says. Second, `risks` is
worth noting for what it puts first: **the measure is the primary choice and the fitting mechanism is
secondary** (`approach =` covers log-binomial, modified Poisson, case duplication *and* marginal
standardization as ways to get one RR). tabxplor's `effect` axis is genuinely a different estimand
rather than a mere approach, so it stays visible — but the ordering is a useful check that `measure`
deserves to be the prominent argument, not a suffix on `effect`.

A one-line "how this is called elsewhere" per value costs nothing and is what makes the lesson travel:

> `measure = "ratio"` on a binary outcome ≡ Stata `binreg …, rr` ≡ `glm(family = binomial("log"))`
> (or the modified Poisson) ≡ marginaleffects `comparison = "ratio"`.

**The hard part is not the naming, and no key fixes it.** *Conditional vs marginal* has to be taught,
and *"ratio" on a binary outcome is a **risk** ratio, not an odds ratio* is the distinction users
routinely conflate — which is precisely why the two must be two values and not one. tabxplor's own
`empirical = TRUE` + `color = "adjustment"` is arguably the best teaching device for the first in any
package, and the model line already names the mechanism in words, unprompted:

> *Model: logistic regression; odds ratios (vs the reference category).*
> *Model: logistic regression; marginal risk ratios (the ratio of adjusted predicted probabilities)…*
> *Model: modified Poisson regression; risk ratios (vs the reference category).*

**The argument names the intent; the model line names the mechanism.** The only obligation this key
adds is that the line keeps its precision when the arguments are re-spelled.

---

#### 8.15 What this key actually deletes, counted honestly

| producer    | out                                                                                                             | in        | net                           |
|-------------|-----------------------------------------------------------------------------------------------------------------|-----------|-------------------------------|
| `tab()`     | `OR` (deprecated — the `or` field becomes unconditional), `ci`'s two geometry values → `"comparison"`           | —         | **−1 formal**, −2 enum values |
| `tab_num()` | `ci_scale` (cut — a pure duplicate)                                                                             | —         | **−1 formal**                 |
| `tab_reg()` | `exponentiate`, `at`; `effect = "ame_ratio"`; `family = "rr"` / `"quasipoisson"`; `color`'s two geometry values | `measure` | **−1 formal**, −5 enum values |

**−3 formals and ~11 enum values across the three functions.** But the count is not the point, and
pass 2 was right to say so. What this buys is:

- **three conflations ended** — a scale hidden inside `family`, a contrast hidden inside an `effect`
  value, a measure hidden inside `exponentiate`;
- **two welded arguments unwelded** — `OR` (= colour + display + dichotomisation) and `ci` (= anchor +
  geometry);
- **one field promoted to unconditional**, so an odds ratio stops being something you *ask for* and
  becomes something the cell simply *has*, like its difference and its ratio — measured free (§8.4);
- **six silent-wrong states made unrepresentable** — D20, D21, D23, D25, and the two found by this
  pass's follow-up: D26 (`stars` and `color_signif` disagree about an OR table's comparison, and
  `stars` wins) and D28 (`ci = "cell"` silently drops the stars);
- **one estimand made reachable** through a front door instead of by naming the wrong distribution;
- and a `tab()` surface in which **every argument is a choice** — *what is in the cell* (`pct`),
  *compared to what* (`ref`/`ref2`/`comp`), *which comparison* (`color`), *do I want an interval*
  (`ci`), *how significance shows* (`color_signif`), *what is printed* (`display`).

---

#### 8.16 Honest cost, dependencies and what stays open

**Cost.** Public arguments on three functions. `tab_reg()`'s back-compat is waived; `tab()`'s `OR` and
`ci` are **CRAN-released since v1.2.0** (verified from the tag) and therefore need soft-deprecation
shims — all six routes are mechanical (§8.6.6). The jamovi `.a.yaml` / `.u.yaml` change on both
modules, so a maintainer `jmvtools::prepare()` is required, and `JMVTAB_CACHE_SCHEMA` bumps (the
carrier stores the colour attributes).

**Dependencies.** This key now **depends on KEY 2 rather than duplicating it**:

- KEY 2 stores the scale → the *default display* of a column with an interval becomes a lookup (**D20**),
  and `ci` on an odds-ratio column routes to the Woolf interval that already exists (**D21**);
- KEY 2 + one rule ("the bracket renders the stored interval; a mismatched geometry is refused")
  closes **D23**;
- KEY 4's `requires` is the mechanism by which `color` and `display` declare what they need — and for
  `color = "OR"` it is **already live** (§8.3), so this is finishing a mechanism, not starting one.

Land it **after** KEY 2 and **with** KEY 4; the `tab()` half and the `tab_reg()` half are independent
and can be separate sessions.

**Maintainer's rulings, and what each one settled:**

- **(a) `display` does NOT become declarative** — *"display is the user's responsibility; if a column
  is full of NA it is still displayed as void. Always calculate OR with difference and ratio if it's
  cheap enough?"* **Accepted, and the second half is now measured and adopted (§8.4): the odds ratio
  becomes unconditional, at no measurable cost.** That dissolves the `{or}` case entirely — the field
  is never empty on a percentage column — and it is a better answer than the declarative rule, because
  it makes the quantity *present* rather than making the formatter *demand* it.
  ⚠ **One premise to flag, because it does not hold today.** "A full-NA column still displays as void"
  is what a reader would expect, but it is **not** what happens: measured, `display = "{or}"` on a
  table with no odds ratio prints the **percentage**, and the stored `display` attribute comes back
  `pct` — a silent *substitution*, not a blank (§11 D22). Always-computing the OR removes the
  motivating instance, but the class survives for every other token (`{ci}` with no interval silently
  drops the bracket; `{obs}` and `{ctr}` on a plain crosstab). So D22 still needs a decision, and it is
  now a **bug fix rather than a design change**, since today's behaviour matches neither of the two
  defensible options. Recommendation: make it render void, which is exactly the maintainer's stated
  expectation.
- **(b) `cumOR` lives in `ref2`.** Settled — `ref2 = "cumulative"` beside `"first"` / `"last"` / an
  index. With (a) this is what makes `OR` deletable: one field, one dichotomisation, named by `ref2`.
- **(c) `color`'s canonical values migrate to the full words**, short ones kept as aliases both ways,
  the full word taught. Settled — this is what makes `tab(color =)` and `tab_reg(measure =)` one
  vocabulary (§8.12).
- **(d) A mismatched bracket is refused**, not converted. Settled — a printed interval must be the
  interval, not a re-anchoring of one.
- **`OR` is deleted** and the odds ratio computed unconditionally on `type ∈ {row, col}` columns —
  measured free (300 ms vs 340 ms against the difference path; counts and means keep an all-NA field).
  Three consequences declared in §8.4: the html tooltip gains an `OR:` line, the structural goldens
  move, and **`ref2 = "last"` must be fixed first** (it is broken today, §11 D27).
- **`ci` is NOT a logical** — the maintainer's objection was right and pass 3's first draft is
  withdrawn. It stays an argument with the *anchor* question and loses only its geometry:
  `ci = c("auto", "no", "cell", "comparison")`. **`ci = "cell"` does not move at all.** This is
  preferred over the proposed `ci_cell = TRUE`, which would add a formal and create an illegal
  `ci_cell = TRUE, ci = "comparison"` cell.
- **`color` alone must NOT trigger the comparison interval** — measured **+86 ms on a 226 ms build
  (+38 %)**, the largest avoidable cost in the crosstab path, against `color`'s 36 uses in the intro
  vignette. The trigger stays `color_signif` / `stars` / an explicit `ci = "comparison"` for `forest_plot` without `color`.
- **`stars` without `color` falls back to the difference** — measured, on factors, on means and on
  `pct = "col"`, and that default is right. But the fallback exposed **D26**: `stars` and
  `color_signif` disagree about an odds-ratio table's comparison and **`stars` wins**, so the colour
  gate ends up testing a difference on a table displaying odds ratios.
- D22's shape: should a `display` token whose field is empty render **void** (the maintainer's
  stated expectation) or **abort**? Today it silently substitutes another quantity, which is neither.
  **Maintainer’s decision: void, plus a one-time note naming the argument that would fill it.**
- `ci = "cell"` beside `stars` / `color_signif`: **inform and disable**, or **abort**? Today
  the two disagree — `color_signif` aborts, `stars` is silently dropped (§11 D28).
  **Maintainer’s decision: inform and disable, from one rule — `ci = "cell"` is an explicit later choice and the user should still get their table**

---

## 4. Subsystem map — the remaining simplifications

Ranked within each subsystem by (payoff ÷ churn). "BI" = byte-identical target.

### 4.1 The fmt record and the colour engine

| # | item                                                                                | evidence                                                                                                                                                                                       |
|---|-------------------------------------------------------------------------------------|------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| 1 | **E1: derive the 4 reconstructors from `fmt_col_attrs` + declared reconcile rules** | `fmt_class.R:5152, 5266, 5451, 5608` enumerate 14 attrs by hand; the leaves pass 9 of 14. BI. *Unblocks KEY 2/4/6*                                                                             |
| 2 | **`scale` + the `type` split + `ci_method`; `ci_type` deleted** (KEY 2)             | `fmt_class.R:2015, 3277-3283, 3310, 3543`; the library already exists (`EST_SCALES` `:3307`)                                                                                                   |
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

| # | item                                                                                                                                                        | evidence                                                                                                                                                                                                                                      |
|---|-------------------------------------------------------------------------------------------------------------------------------------------------------------|-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| 1 | **collapse `at` → `effect`, `exponentiate` → the scale axis, `estimate_display` → a real `display =`, `empirical` ← `color`** (KEY 3a — two shapes studied) | removes 4 formals and ~8 guard blocks of `tab_reg.R:5089-5178, 5256-5262`; `estimate_display` is *already* a preset over the display grammar (`:1436-1463`), so that fold deletes a layer rather than adding one                              |
| 2 | three family predicates (`reg_fam_glm`, `_overdispersed`, `_disp_known`)                                                                                    | absorbs 14 of the 21 hard-coded whitelists                                                                                                                                                                                                    |
| 3 | `reg_build`'s four parallel assemblers                                                                                                                      | AME `:3768-3826`, MNL-vs-rest `:3827-3846`, coefficient `:3847-3877`, and the **complete duplicate** split branch `:3620-3699` with its own `new_tab`/`meta`/`tab_stamp_inference` — which has already drifted once (comment at `:3676-3690`) |
| 4 | the `shared` bag → a typed record                                                                                                                           | 24 keys, documented as 20 (`:3598-3603`), mirrored in `fmt_class.R:55-58` to silence `R CMD check`, partially re-listed twice (`:3904, 3911`), with 2 fields declared nowhere                                                                 |
| 5 | drop the 5 dead `reg_meta` fields                                                                                                                           | `shape`, `model_labels`, `conf_level` (no reader); `predictor_types`, `multiplier` (tests only)                                                                                                                                               |
| 6 | one `stats`/`check` vocabulary                                                                                                                              | `tab_reg(stats=)` and `reg_check_plots(check=)` are the same `names(REG_CHECKS)` with two argument names and two validators                                                                                                                   |
| 7 | **the `.fit_cache`/`reref` path is ~450 lines and an 11-conjunct correctness predicate**                                                                    | `tab_reg.R:5343-5356` + `reg_build_digest` + `reg_reref_fit_res` + the digest tier. Exists only for the jamovi live UI, and **silently shows fewer footer rows** than the R path (every check returns NULL without a fit). See §10 Q4         |

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

"Cut" = free now (unreleased or internal). "Deprecate" = CRAN etiquette. "Keep" = suspicion checked and dismissed.

| item                                                                                                       | evidence                                                                                                                   | verdict                                                                             |
|------------------------------------------------------------------------------------------------------------|----------------------------------------------------------------------------------------------------------------------------|-------------------------------------------------------------------------------------|
| **`filter`**                                                                                               | **0 uses**; cache key is hardcoded `NA_character_` (`tab.R:2066`), **never invalidates jamovi cache**                      | **keep**, used with tribble with different args/pop for tab(); remove from jamovi UI|
| `names_prefix`, `names_sort`                                                                               | 0 uses anywhere, on both `tab()` and `tab_counts()`                                                                        | **verify** : does it comes from tab_spead (then let it there, not in `tab()`) ?     |
| `levels = "auto"`, per-col_var `levels`/`digits` vectors                                                   | 0 uses                                                                                                                     | **document** : one level for binaries, all levels when 3+ is the most useful default|
| `tab_many()`'s 41 formals                                                                                  | soft-deprecated already, but the only place `chi2`/`totrow`/`totcol` survive; `na_drop_all`/`compact`/`totrow` have 0 uses | **finish it**: `function(...) tab(..., output_list = TRUE)` + the deprecation       |
| `tab_plain()` / `tab_num()` as documented API                                                              | 0 vignette uses; literally wrappers over the cores since 17f                                                               | **keep**                                                                            |
| `sup_cols`                                                                                                 | tests only (9 hits); mechanically identical to `col_vars` + `levels = "first"` + `pct = "row"`, and the code says so       | keep the deprecation, stop mirroring it                                             |
| `totcol = "each"` / `"all_col_vars"`                                                                       | tests only (details below)                                                                                                 | **deprecate**, should not error but give the base behaviour (keep one total)        |
| `tab_totcol_range()` + `range_totcol`                                                                      | producer alive, consumer commented out, kept by its own unit test                                                          | **keep** to maybe implement later (the option is DORMANT since x2)                  |
| `ctx$levels_order`                                                                                         | read by no stage in `tab.R` (only `jmvtab-cache.R:405`)                                                                    | **cut from ctx**, pass directly                                                     |
| `tab_assemble()`                                                                                           | no caller anywhere                                                                                                         | **cut**                                                                             |
| `set_tot_n`, `set_n_eff`, `set_model_family`, `get_ref_means`, `get_ref_pct`                               | zero callers                                                                                                               | **cut**                                                                             |
| `jmvtab_reg_staged()`                                                                                      | its own caller inlines the predicate instead                                                                               | **I don’t understand**                                                              |
| `complete_partial_totals`, `set_ci_type`,                                                                  | exported, zero use anywhere                                                                                                | **cut**                                                                             |
| `tab_get_wrapped_dimensions`                                                                               | exported, zero use anywhere                                                                                                | **keep** (personal tooling)                                                         |
| `reg_meta$shape`, `$model_labels`,                                                                         | no reader                                                                                                                  | **cut**                                                                             |
| `$conf_level`                                                                                              | no reader                                                                                                                  | **use it to unlock further simplication** (added as column attr for that, not implemented yet)|
| `estimate_display = "prob"/"ame"`, `at`, **`exponentiate`**, `family = "rr"`/`"quasipoisson"`, `tab(OR =)` | duplicate `effect`/`family`/`measure` (details below)                                                                      | **fold** (KEY 3a + KEY 8)                                                           |
| **`tab_num(ci_scale =)`**                                                                                  | **0 uses anywhere.** A *pure duplicate*: measured, `tab(…, ci = "ratio")` already gives `ci_type = "ratio"`                | **cut** (KEY 8.7)                                                                   |
| `color = "ci"`                                                                                             | a pure synonym of `"after_ci"`                                                                                             | **cut** (both already legacy-decoded)                                               |
| `tabxplor_tabs`                                                                                            | one behavioural bit (`tab_xl.R:214`) + four print methods                                                                  | keep, but do not grow it; the bit could key on `!is.null(names(x))`                 |
| `spread_models`                                                                                            | 7 code sites, least-read public formal                                                                                     | **merge with `spread_vars`** ?, re-key on KEY 1                                     |
| `tab_md_css()`                                                                                             | one-line alias of `tab_css(chrome = FALSE)` whose `tabs` argument is documented as ignored                                 | keep as alias, drop the argument                                                    |
| `tab_kable()`                                                                                              | pure alias of `tab_html()`                                                                                                 | keep (maintainer's explicit z-g decision)                                           |
| kableExtra engine                                                                                          | legacy — and the one that breaks under `theme = "print"`                                                                   | keep + **fix D2**                                                                   |
| `tabxplor.output_kable`                                                                                    | changes the **shape of the built object** from a display option                                                            | keep the option ; **remove its build-shape power ?**                                |
| the 3 number-font options                                                                                  | one decision, three knobs                                                                                                  | merge to one, alias the others                                                      |
| `method = "profile"`, `quasipoisson`, the compound-formula hatch, `mnl_vsrest`                             | previously settled                                                                                                         | keep                                                                                |

**`totcol = "each"` / `"all_col_vars"` — details.** Tests only. Worse: the parser returns a
**character** for `"last"` and a **list** for the others, so the `identical()` comparisons never fire
for the default and **`tot_cols_type == "some"` is the default arm**; `"all_col_vars"` as an *input
value* can never produce `tot_cols_type == "all_col_vars"`; and the string `"all_col_vars"` carries
**two unrelated meanings** (the total-column tag and the add_n/add_pct helper-column tag).

**The `tab_reg()` / `tab(OR =)` fold — details.** They duplicate `effect` / `family` / `measure`;
`at` is degraded away in 3 separate blocks; `family = "rr"` is a *measure* reachable only by naming
the wrong distribution; and `tab(OR =)` is measured to be `color` + `display` + `ref2` welded
(§KEY 8.4) — with the `or` field becoming unconditional at **no measurable cost**, which is what makes
it deletable rather than merely re-spelled. ⚠ **`exponentiate` IS on this list**: pass 2 removed it,
pass 3 measured it a no-op on the whole marginal path (and 0 of 49 taught calls), and the maintainer
ruled delete.

**And the converse — cold but good, do not cut:** `tab_counts()`, `tab_css()`, `transpose=`, `n_min=`,
`split_var=`, `score_from_lv1()`, `common_totrow`, `tab_compact()`'s cross-call merge (which is the one
thing `tab()`'s built-in merge cannot express, and it is undocumented — its own `@examples` shows the
case `tab()` now does by default).

---

## 6. Fields and attributes — the verdicts

**Add (3 attributes, 0 fields):**

| what                                                       | why                                                                                                                                                                | cost                    |
|------------------------------------------------------------|--------------------------------------------------------------------------------------------------------------------------------------------------------------------|-------------------------|
| column attribute `scale` (one key into a declared library) | KEY 2 — deletes `est_scale_key`'s order-dependent dispatch, the `var`-sniff, `fmt_est_field`'s six copies, the seven derived predicates and the `gof` special case | one attribute, after E1 |
| column attribute `pct_base`                                | KEY 2 — `type`'s other half; a `points` column can be a row% or a col% difference                                                                                  | one attribute, after E1 |
| column attribute `ci_method`                               | KEY 2 — a per-column fact stored table-wide today; **empties `meta$ci_settings`** and makes D8's silent fall-through impossible                                    | one attribute, after E1 |
| a **row index model** (per-element, not positional)        | KEY 1                                                                                                                                                              | structural, see §9      |

**Change (3):**

| what                                        | to what                                                                                                                                     |
|---------------------------------------------|---------------------------------------------------------------------------------------------------------------------------------------------|
| `type` (8 values, two jobs)                 | `pct_base` + `scale`; `get_type()` survives as a **derived, soft-deprecated accessor** returning the same 8 values (KEY 2, naming option 3) |
| `ci_type`                                   | **deleted** — folded into `scale` (KEY 2 b); `get_ci_type()` becomes a derived, soft-deprecated accessor                                    |
| field `in_totrow` (logical)                 | field `row_kind` (7 values) — **the record stays at 21 fields**, `is_totrow()` becomes a derived read (KEY 1, shared half)                  |
| `meta` (crosstab) + `reg_meta` (regression) | one `meta$spec` with `kind` + a uniform `vars` (KEY 6)                                                                                      |

**Remove (0 fields, 4 meta sub-fields):** `reg_meta$shape`, `$model_labels`, `$conf_level`, and
`meta$ci_settings` (emptied by `ci_method` + the already-per-column `conf_level`).

**Do not merge any field.** `diff`/`or`/`ratio` are provably a discriminated union, and `$` can serve
derived fields (the `$ci` precedent, `fmt_class.R:3162-3181`) — but z6 already measured the payoff
(≈0.03 % of a build; ~92 KB on the largest realistic table), and the stored scale captures the
*structural* benefit without touching storage, the goldens, or the user contract. Likewise
`n`/`wn`/`tot_n`/`n_eff` are four slots for ~two facts, but the fix is **one accessor** (`fmt_base(x)`
folding the `coalesce` written out at all five read sites), not a merge.

**One field-adjacent smell worth naming:** `fmt_gap_force_policy()` uses `all(is.na(get_gap_se(x)))` as
a boolean *"does this column have a gap test?"* (`fmt_class.R:2135`) — a **column-level fact encoded as
an all-NA per-cell field**. It works, and it is elegant in its way; with `scale` in place it becomes
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
   colour itself. So `scale` needs a default (`"level_pct"`) that reproduces today's behaviour for
   every existing column, and — the constraint that shapes KEY 1 — the **total-row flag cannot leave
   the record**, because `fmt_color_plan()`'s `gate_row` calls `is_totrow(x)` on a lone column
   (`fmt_class.R:3580`) with no table in scope.
2b. **Table-level facts have been silently lost five times in three phases** (§KEY 1, Option A
   caveats): `tab_compact()`, `tab_spread()`, `reg_build()`'s split branch, and now `bind_rows()` on
   a grouped table + `dplyr_reconstruct.tabxplor_grouped_tab` (§11 D16). Each was fixed; the class
   was not. That count — not any single incident — is the argument for putting a fact on a column or
   in a cell whenever it *can* live there.
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

---

## 9. Sequencing

**Free now, byte-identical, no design risk** (several are prerequisites):

- **E1** — derive the four reconstructors from `fmt_col_attrs` (§4.1.1). *Unblocks KEY 2, 4, 6.*
- The §11 defect list — **D9 first** (a shipped jamovi control that does nothing), then **D16**
  (`bind_rows()` on a grouped table loses every table attribute) — D16 is also the difference between
  13/15 and 15/15 in §2.6, so it is a prerequisite for KEY 1 Option A being worth choosing.
- The dead-weight cuts (§5 "cut" rows).
- The 4× `cleannames` / 6× `conf_level` single-sourcing; `inference` as a required argument; `meta`
  passed explicitly in the step tails.

**One phase each, contained:**

- **KEY 4** — MEASURES gains its vocabulary; the four allow-lists collapse to one; the
  `color_diff_OR`/`color_ctr`/`color_ci`/`color_num` fossil dies. BI target.
- **KEY 2** — `scale` + `pct_base` + `ci_method`, `ci_type` deleted, `get_type()`/`get_ci_type()` demoted to derived accessors. Needs E1.
- **KEY 3a + KEY 8** — the argument collapse. ⚠ **Pass 3 split this into two independent sessions**,
  and reordered it **after** KEY 2 (it now *depends on* KEY 2 rather than duplicating it — KEY 2's
  stored scale is what closes D20/D21/D23, and KEY 4's `requires` is the mechanism `color` and
  `display` declare through):
  - **KEY 8a — `tab()`**: retire `OR` and compute the odds ratio unconditionally (measured free,
    §KEY 8.4 — **fix `ref2 = "last"` first**, §11 D27); drop `ci`'s geometry values for
    `ci = c("auto", "no", "cell", "comparison")`; cut `tab_num(ci_scale =)`; migrate `color`'s values
    to the full words with aliases. **No new argument.** Soft-deprecation
    shims required — `OR` and `ci` are CRAN-released since **v1.2.0** (verified from the tag), so this is
    *not* free-because-unreleased; the six routes are mechanical.
  - **KEY 8b — `tab_reg()`**: `measure` × `effect`; delete `exponentiate` and `at`; close the capability
    gaps; ship the resolution table as a runtime object. Back-compat waived. Needs the three family
    predicates (§4.4.2).
- **KEY 6** — `meta$spec`. Needs E1; pairs naturally with KEY 3a (both touch `reg_meta`).

**Structural, in this order:**

- **KEY 1** (the row model) — one to three sessions depending on the option chosen (A ~1 + D16, C ~1.5, B 2–3). The enabler for KEY 5 and for the
  `tab_vars` × several-`row_vars` composition, and what makes `tab_reg()` stop being structurally
  exceptional.
- **KEY 5** (move CI/test into the leaf) — after KEY 1, gated on the jamovi lock.
- **KEY 7** — mostly falls out of KEY 1 (the return shape); otherwise a doc + deprecation pass.

**Relative to the release.** Everything that changes a *public argument* or a *return class* is free
only until the 2.0.0 CRAN release: that is KEY 8b (regression — already waived), KEY 7's entry points,
and the §5 argument cuts. Everything else is internal and can land after. ⚠ **KEY 8a is the exception
that is *not* free either way**: `tab(OR =)` and `tab(ci =)` shipped in v1.2.0, so they carry
soft-deprecation whenever it lands — which is an argument for landing it *before* the release, so the
shims are introduced once rather than on top of a second released spelling.

---

## 10. Decisions for the maintainer

**Q1 — the row model (KEY 1): now, or after the release?**
The highest-leverage item, and *internal* (rendered output does not move), so not release-gated. But
also the biggest. Recommendation: **do E1 + the free items now**, and schedule KEY 1 as its own phase
with the transpose and compact migration folded in — doing it without them creates a fifth
representation.
**Maintainer’s decision: the diagnostic is right, the room for simplification very real, but it seems very dangerous to store something as important as this in a fragile table-level attribute.** We’ll do it, but let’s really think about the reliable solution.
- It could work at table level if the thing is *as robust and as polished as dplyr group attributes and keys* ; maybe even extending part of this dplyr infrastructure for tabxplor purposes (would it be possible ?). It would still need a fallback to still print when the special class is lost, in a well thought degraded mode ?
- Another possibility would be to create a light custom vctrs vector based on factor, handling ordered, robust to vector transformations, that would use the text columns to carry the informations about rows (if so, it needs a fallback for cases where the custom class is lost, and export functions need to keep the fallback to work even with a normal tibble with normal vectors).

**Second pass — both possibilities are now written up and measured in §KEY 1** (Option A: a declared
row spec in its own table attribute + `row_kind` as a field; Option B: a typed row-index column +
`row_kind`, with the table-level facts derived), **plus a third that was added on the maintainer's
request and now carries the recommendation** (Option C: a *typed label column with no fields* — a
factor subclass whose facts are ordinary column attributes). **Still open: the choice**, plus the
naming sub-question. Five facts the choice can rest on, all measured:

1. **Extending dplyr's group infrastructure is not possible.** An extra column stamped into
   `attr(d, "groups")` passes `validate_grouped_df()` and is then read as a grouping variable —
   `group_vars()` returns a phantom `NA` and filter/mutate/slice/select all error (§2.6.3). What
   *is* borrowable is the rule, not the container: names, one recompute funnel, silent degrade.
2. **A table attribute cannot be made more robust than `meta`** — 13/15 verbs is the ceiling, and
   only after D16; a bare one scores 7/15 (§2.6). A per-column attribute scores 14/15, a field 15/15.
3. **The "long column attribute keyed on rows" variant does not work at all**: vctrs carries
   attributes whole and never slices them, so it desynchronises by construction (§2.6.2). Per-row
   data must be a *field* or a *column*.
4. **All three options need the same record change** (`in_totrow` → `row_kind`), because a lone
   extracted column must still colour itself. So the real delta between them is only *where the
   declaration lives*.
5. **A factor subclass is robust with almost no code** (Option C): measured, `[`, `filter`,
   `arrange`, `mutate`, `as.data.frame` and forcats' `fct_drop`/`fct_rev`/`fct_relevel` all preserve
   the class and its attributes with **zero methods written**; only `bind_rows`/`vec_c` need one
   (`vec_ptype2` + `vec_cast`). And because it *is* a factor, `is.factor()` stays TRUE — the 39-site
   migration Option B forces simply does not arise.

**Q2 — how far to collapse the crosstab entry points (KEY 7.1)?**
(a) status quo; (b) `tab_many()` becomes a one-line deprecated shim and `tab_plain`/`tab_num` get a
superseded badge and stop mirroring `tab()`'s formals; (c) also make `tab_plain`/`tab_num` internal.
Recommendation: **(b)** — removes the four-spelling problem at zero user cost; (c) would break the
programming vignette's "build cells from scratch" story.
**Maintainer’s decision: (b)**

**Q3 — the `tab_reg()` argument collapse (KEY 3a): which enum?**
Proposal: `effect = c("coefficient","ame","mer","ame_ratio","mnl_vsrest")` + `scale = c("ratio","link")`,
absorbing `at`, `exponentiate` and `estimate_display`'s two folds. A hard break of an unreleased
surface, which the maintainer has authorised. One judgement call: `trials` — fold into `family` or keep
separate? It *is* a family variant internally (`crude_key = "grouped_binomial"`), but a separate formal
reads better in jamovi. Recommendation: **keep `trials`, fold the other three.**
**Maintainer’s decision: a simplification like this is defititely needed, but I wonder about the `scale` part, which is hard to understand (and seems only to mimic exponentiate in an even less clear way ?). Can you think of a more consistent and more readable way to do the same thing ? Also add a proper `display` argument to `tab_reg()`, mirroring `tab()`, so the expert user can customise eveyrhing.**

**Second pass — two shapes are now written up in §KEY 3(a)**, with their simplification strength,
readability and caveats: (1) two arguments over two genuinely orthogonal questions
(`effect` = which contrast × the KEY 2 word = on which scale), which deletes `ame_ratio` as a value
and shares one vocabulary with the stored column attribute; (2) one flat `effect` list with the ratio
variants inline, on the `ame`/`ame_ratio` model. Short version: (1) is the better model and the
better fit with KEY 2, (2) is the better jamovi widget — and the module already does the
cross-greying (1) needs. **Still open: the choice.** The `display` request is answered and is free:
`estimate_display` is *already* a preset over the display grammar (`"prob"` → `"{or} ({pct})"`,
`tab_reg.R:1436-1463`), so a real `display =` deletes a layer rather than adding one.

**Q4 — the `.fit_cache`/`reref` path: is jamovi worth ~450 lines and an 11-conjunct predicate?**
It buys a live UI that does not refit on a reference change. It costs a parallel fitter, a duplicated
`disp_known`, a byte-identity obligation, and a **silent footer degradation** (checks vanish on the
digest path because it keeps no fit). Options: (a) keep as is; (b) keep, but make the degradation
explicit (a footer note "model checks need a refit"); (c) drop the digest tier, cache raw fits only,
accept a refit on reference change. Recommendation: **(b) now, revisit (c) if the 11 conjuncts ever
need a 12th** — each new estimand feature must add one or silently return wrong numbers.
**Maintainer’s decision: (a)**

**Q5 — `estimand` (KEY 2): attribute now, or wait for a display change to ride along?**
The `var`-sniffing in `fmt_gap_scale_key()` is the strongest argument for "now": an order-dependent
dispatch on an under-determined input, in the newest and least-exercised part of the colour engine.
Recommendation: **now, right after E1.**
**Maintainer’s decision: this column attribute now, separated from the percentage base. But the `estimand` name is not clear enough, propose me something else, simple, working for both tab and tab_reg. I also think this one must be thought thoroughly, to really find the shape of this attribute that would really unlock simplifications and integrations, while still being human readable.**
- To make it clean once and for all, would there be an interest to store the model family and the effect type as column attributes too ? What else ? Would it unlock some additional simplifications and increase reliability ? Would it permits, at cheap cost, to reduce the size of the table level meta attribute and increase reliability ? Could it become a super "type" of column attribute, efficient enough to carry core informations about how to handle the column in many cases, reducing gating / forking / conditions / etc. complexities in the code ?

**Maintainer’s follow-up: since most of `type`'s content goes to `pct_base`, and the rest to whatever
`estimand` is named, couldn’t we just continue to use the `type` name for the `estimand` content?**

**Second pass — §KEY 2 is rewritten around three measured findings** that change the shape of the
answer: the estimate field is already computed by *two rules that disagree on 178 of 190 columns*;
z17's `EST_SCALES` is already the declared library this key needs, so the work is to **store the key
its dispatch computes**, not to invent a record; and — after the maintainer challenged the draft's
claim that the estimate's scale and the stored interval's scale were two independent facts — that
claim was re-tested column by column and **withdrawn**, so the key is **one** attribute and `ci_type`
is *deleted* rather than renamed (§KEY 2 b).
On the follow-up: keeping the name `type` is written up as option 1 of three, with the one fact
against it — `get_type()`/`set_type()` are exported and the programming vignette teaches the 8 values
verbatim, so re-pointing the name makes old user code silently wrong rather than broken. Option 3
(two honest names; `get_type()` kept as a *derived* soft-deprecated accessor that still returns all 8
old values) costs the same one attribute and breaks nothing — recommended, but **still open**.
The "super type" question is answered by an admission test rather than a list: one key plus a
declared library, and an attribute is admitted only if it names a fact no other attribute derives
*and* has a reader. That admits `pct_base` and a new `ci_method` (which empties `meta$ci_settings`
and closes D8), keeps `model_family`/`role`, and rejects both storing the *effect* and keeping a
separate interval-scale attribute — on your follow-up question, one scale is enough and **`ci_type`
is deleted rather than renamed** (§KEY 2 b re-tested the claim that they were two facts, and
withdrew it).

**Q7 — KEY 8: fold the geometry into one argument?**
*Asked in pass 2 while reviewing KEY 2; **re-opened from scratch in pass 3** at the maintainer's
request — "restate the whole problem and find the real key; I would rather have a well-argued
demolition than a polished defence." It is a demolition. §KEY 8 is rewritten around the result.*

**The pass-2 answer (`compare` on both producers) is withdrawn**, on three measured grounds:
`compare` is **already a `tab_reg()` formal** (model comparison); on `tab()` the geometry is **not an
input at all** (`diff` and `ratio` are computed in every cell unconditionally, and `display`/`color`/
`ci` are three deliberately independent selectors over them — differentiator #1); and the D20/D21
defect class is reachable with **no `OR` and no `ci` conflict** (§11 D23), so no argument closes it.

**What replaced it.** The rule is different on the two producers, for a mechanical reason: on a
crosstab every geometry is a function of the *same sufficient statistics*, so asking for one is a
**selection**; on a regression it is a *different fit or estimator*, so it is a **modelling decision**
— and *changing `display` must never change the model*. Hence:

- **`tab()` gains nothing.** The geometry is already named by `color`, which — measured —
  **already computes the odds ratio with no `OR =` argument** and **already selects the matching
  interval for all three geometries**. `OR` is `color` + `display` + `ref2` welded (§KEY 8.4);
  `ci` keeps its *anchor* question and loses only its geometry — `c("auto", "no", "cell", "comparison")`,
  with `"cell"` unchanged (§KEY 8.5); `tab_num(ci_scale =)` is a pure duplicate of `ci = "ratio"`.
  And the `or` field becomes **unconditional**, measured free, which is what makes `OR` deletable.
- **`tab_reg()` gains `measure`**, the two-axis Shape 1 of KEY 3a, finished and named.

**Maintainer's rulings, pass 3:** **`exponentiate` is deleted** (→ `measure = "log"`, with the old
name kept as a documented synonym); **the capability gaps are closed** (gaussian ratio-of-means,
identity-link risk difference), which makes the legality table three-state and requires the resolution
table to ship as a **runtime object** (help + error + lister + jamovi), not prose.

**Rulings (a)–(d), given 2026-08-13** and recorded in §KEY 8.16: **(a) no**, `display` stays the
user's responsibility — *and instead the odds ratio becomes unconditional*, which the follow-up
measured to be free and which dissolves the motivating case; **(b)** `cumOR` lives in
`ref2 = "cumulative"`; **(c)** `color`'s canonical values migrate to the full words, short ones kept
as aliases both ways; **(d)** a mismatched bracket is **refused**, not converted.

**The follow-up settled two more things by measurement:** `OR` is deleted and the `or` field computed
unconditionally on percentage columns (measured free — 300 ms vs 340 ms for the difference path); and
`ci` is **not** a logical — pass 3's first draft is withdrawn on the maintainer's objection. It keeps
its name and its *anchor* question, losing only the geometry:
`ci = c("auto", "no", "cell", "comparison")`, with **`ci = "cell"` unchanged**. `color` alone must not
trigger the interval — measured **+38 %** on a build, the largest avoidable cost in the crosstab path.

**Still open:** (e) should a `display` token whose field is empty render **void** (the maintainer's own
expectation — today it silently substitutes another quantity, §11 D22) or abort? (f) `ci = "cell"`
beside `stars`/`color_signif`: inform-and-disable, or abort as `color_signif` already does (§11 D28)?

**Two pass-2 claims corrected inside other sections.** §KEY 3's derived-arguments table said
`exponentiate` does **not** belong on the cut list — reversed (it is a silent no-op on the whole
marginal path, 0 of 49 taught calls, and its `FALSE` value *is* a measure). And the pass-1 claim that
three of the nine `effect × measure` cells are "illegal" stays withdrawn: a gaussian `ratio` is a ratio
of means — measured, `tab(race, tvhours, ci = "ratio")` gives one happily while `tab_reg()` refuses —
and a binary coefficient `difference` is the identity-link additive-risk model.

**Q6 — the jamovi boundary: consolidate or accept the mirror?**
Seven rules are hand-mirrored (§4.5), three of them in JS. Options: (a) accept, document each mirror;
(b) a shared `tab_resolve_display_settings()` both boundaries call, plus a generated JSON table for the
JS eligibility rules; (c) generate the JS rules only. Recommendation: **(b)**, and convert `anova` to a
`tab()` argument while there — it is the last global-option mutation and the one stale-cache hazard
left in the layer.
**Maintainer’s decision: (b)**

---

## 11. Defects found in passing — fix regardless of any redesign

| #       | defect                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   | site                                                       | severity                                                                        |
|---------|----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|------------------------------------------------------------|---------------------------------------------------------------------------------|
| **D9**  | **the jamovi `design_effect` checkbox does nothing.** It is declared in `jmvtab.a.yaml:205` and rendered in `jmvtab.u.yaml:231`, but **absent from the stale generated `R/jmvtab.h.R`**, so `self$options$design_effect` is `NULL` and `isTRUE(NULL)` is `FALSE`. Every claim in the `.a.yaml` help and the `.b.R`/`jmvtab-cache.R` comments about it "moving every interval in the table" is currently untrue in the running module. Pending a maintainer `jmvtools::prepare()`                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         | `jmvtab.h.R` vs `jmvtab.a.yaml:205`                        | **user-visible**                                                                |
| D1      | transposed reg tables lose `ann$keep_black` → footer rows wrongly greyed in HTML; masked by a silent fallback, untested                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  | `tab-transpose-render.R:212-223` → `tab-render-html.R:430` | user-visible                                                                    |
| D2      | `theme = "print"` + `engine = "kableExtra"` renders `kable_material_dark`                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                | `tab-render-html.R:294-309`                                | user-visible                                                                    |
| D11     | jamovi `display` on a **mean** column with `ci = "cell"` sets `pct_ci`, which reads the `pct` field — `NA` on the numeric leaf → the cell renders **empty**. `tab_ci()` had already set `mean_ci` correctly                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              | `jmvtab-cache.R:523-528` vs `tab.R:6181`                   | user-visible                                                                    |
| **D16** | **`bind_rows()` on two GROUPED tabs silently drops every table attribute** — `subtext`, `test` and the whole `meta` (so: no weight footer, no inference basis, no CI legend, no test summary, no caption). Measured: `bind_rows(tp, tp)` on a plain tab keeps all four; on a grouped tab keeps none. `vec_rbind()` on two grouped tabs is worse — it returns a bare `grouped_df`, so `vec_ptype2.tabxplor_grouped_tab.tabxplor_grouped_tab` is not reached at all, while `vec_cast` is and works. At least a contributing cause is plainly wrong on its own terms: **`dplyr_reconstruct.tabxplor_grouped_tab` restores from `data`, not from `template`** (`tab_classes.R:2933-2936`: `tab_restore(out, data)`), contrary to dplyr's contract — it survives the in-place verbs only because they hand it a modified copy of the original that still carries the attributes, and fails the moment a verb builds a fresh frame. **This is the fifth instance of the "a rebuild site drops table-level facts" class** (after `tab_compact` z16-iv, `tab_spread` and `reg_build`'s split branch z16-iiiii) and it is the gap between 13/15 and 15/15 in §2.6 | `tab_classes.R:2933`, `:3120`                              | **user-visible**                                                                |
| **D20** | **`tab(OR = "OR", ci = "cell")` silently drops the odds ratios.** The `or` field is computed (measured: `1, 2.06, 1.78`) but the display reverts to `pct_ci`, so the table prints `[40;43]%` where the user asked for odds ratios, and no message is emitted. `ci = "diff"` keeps the OR display; only `"cell"` overrides it                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             | measured; `tab.R:6180-6215`                                | **user-visible**                                                                |
| D21     | **`tab(OR = "OR", ci = "diff")` puts a percentage-point interval on an odds-ratio column.** The cells print `1.00 / 2.06 / 1.78` while the stored bounds are differences (`0.067`, `-0.007`), so `ci_center()` returns the *difference* and `fmt_scale_of()` resolves the column to the `points` scale — which would hand `forest_plot()` a percentage-point axis with `pct_diff` gridlines for a column of odds ratios. The significance gate stays correct by luck (all three scales test the same null, `fmt_class.R:3552-3559`), so nothing visibly breaks in the *table*                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            | measured; `fmt_class.R:3396`, `tab.R:6209`                 | latent (plot)                                                                   |
| **D22** | **`display = "{or}"` on a table with no odds ratio silently prints the percentage.** Measured: `tab(d, race, marital, pct = "row", display = "{or}")` renders `3% 6% 6%`, and the *stored* `display` attribute comes back `pct` — the template is accepted, the field is all-NA, and the column silently falls back. The user gets a plausible table that is not what they asked for. Same class: `display = "{pct} ({ci})"` without `ci =` silently drops the bracket. Both are fixed by the same rule — **a display template declares what the column must carry** (§KEY 8.16 a), the mechanism `color` already uses                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   | measured; `tab.R:844-861`, `fmt_class.R:1488`              | **user-visible**                                                                |
| **D23** | **the D21 incoherence is reachable with no `OR` and no `ci` conflict.** Measured: `tab(d, race, marital, pct = "row", ci = "diff", display = "{ratio} ({ci})")` prints `×1.8 ([2;4]%)` — a **ratio** over a percentage-**point** interval. So the defect class is *display-geometry ≠ interval-geometry*, not *`OR` vs `ci`*, and no argument can close it (`display` must stay free). It is closed by KEY 2's stored scale plus one rule: the bracket renders the stored interval, and a mismatched geometry is refused. Note the conversion already exists on the colour side only — `rescale_bound()` maps a diff bound to a ratio bound for `fmt_color_plan()`, and `format()` has no equivalent step                                                                                                                                                                                                                                                                                                                                                                                                                                                | measured; `fmt_class.R:3605-3618` vs `format.tabxplor_fmt` | **user-visible** (latent in pass 2, where it was mis-attributed to D21's cause) |
| **D25** | **`tab_reg()`'s `color` accepts geometry values that contradict what the column estimates**, silently. Measured: `tab_reg(…, color = "diff")` on an odds-ratio column stores `color = diff`; `tab_reg(…, color = "OR", exponentiate = FALSE)` stores `color = OR` on a `Model_β` (`log_coef`) column. Neither is refused and neither is meaningful. Once `measure` names the estimand and KEY 2 stores its scale, a reg table's `color` needs only `TRUE`/`FALSE` + `"adjustment"`/`"between_groups"` — the ladder comes from the column (§KEY 8.12)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     | measured; `tab_reg.R` colour boundary                      | latent (silently wrong)                                                         |
| **D26** | **`stars` and `color_signif` disagree about what an odds-ratio table compares — and `stars` wins.** Measured: `tab(…, color = "OR", stars = TRUE)` stores `ci_type = "diff"` with percentage-**point** bounds (`0.0276 0.0198 0.0044`); `tab(…, color = "OR", color_signif = "grey_non_signif")` stores `ci_type = "or"` with the Woolf bounds (`3.576 1.803 1.237`); **both together give `"diff"`**. So asking for stars on an OR-coloured table silently downgrades the interval and the colour gate then tests a *difference* on cells that display *odds ratios*. Unlike D21 this is **not latent** — it decides which cells are greyed (the two Wald tests share a null but not a p-value, so borderline cells can flip)                                                                                                                                                                                                                                                                                                                                                                                                                           | measured; `tab-resolve.R:167-182` vs `:94-127`             | **user-visible**                                                                |
| **D28** | **`ci = "cell"` silently drops the stars.** Measured: `tab(…, ci = "cell", stars = TRUE)` returns `pvalue` all-NA and renders no stars. The *same* conflict with `color_signif` **aborts** with a good message (17d). Two consumers of one fact, two behaviours — one loud, one silent                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   | measured; `tab.R` ci/stars forcing                         | **user-visible**                                                                |
| **D27** | **`ref2 = "last"` does not resolve.** Measured: `tab(…, OR = "OR", ref2 = "last")` warns *"no columns were found as reference for comparison"* and returns an all-NA `or` field, while `"first"` and an integer index both work. Latent today (few users set `ref2`), but it becomes a **prerequisite** if the odds ratio is computed unconditionally (§8.4), because the warning would then fire on tables that never asked for one                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     | measured; `tab_apply_reference()`                          | latent → blocking for §8.4                                                      |
| D17     | **two estimate-field rules that disagree on 178 of 190 golden columns** — `fmt_est_field(ci_type)` answers `"diff"` where `fmt_center_field()` answers `"pct"`/`"mean"`. Both are right for their own caller ("the effect field" vs "the field the interval is centred on"), nothing states the difference, and only the caller's context keeps them apart. Fixed by construction under KEY 2                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            | `fmt_class.R:2014` vs `:3415`                              | latent                                                                          |
| D18     | `has_ci` tests two `ci_type` values that can never be stored (`"diff_row"`/`"diff_col"` are stripped at `tab.R:6209` before stamping) — dead arms in a live predicate, and the reader cannot tell whether `"cell"`'s absence is deliberate (it is)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       | `fmt_class.R:3565`                                         | doc / latent                                                                    |
| D19     | `tab(pct = "col", OR = "OR", color_signif = …)` stamps `ci_type = "or"` on three columns and `""` on the **reference** column, because its own OR bounds are NA by construction — so the stored "what interval is this" attribute varies within one col_var for a reason unrelated to what the columns estimate. Harmless today (z17's `display` clause covers the colour path), but it is why the scale must be stored rather than sniffed                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              | measured, `tab.R:6215-6234`                                | latent                                                                          |
| D4      | background-channel allow-lists disagree: `c("OR","adjustment")` legal in `tab_reg()`, illegal in `tab()`                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 | `tab.R:944` vs `fmt_class.R:1367`                          | inconsistency                                                                   |
| D5      | `tab_reg()`'s `na` message names `"drop_all_models"`, removed in z13                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     | `tab_reg.R:3152`                                           | message                                                                         |
| D6      | the multi-dependent × model-list recursion drops `spread_models` and `.fit_cache`                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        | `tab_reg.R:4948-4956`                                      | silent                                                                          |
| D7      | the `ref_vect` NULL guard is unreachable (`ref_vect` is not a declared ctx field, so `is.null()` errors first)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           | `tab.R:2401` vs `new_ctx()` `:1469`                        | latent                                                                          |
| D8      | `legend_method_name()` falls through silently → can print a CI method the bounds were never built with                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   | `fmt_class.R:4179-4183`                                    | latent                                                                          |
| D10     | the stale generated `R/jmvtabreg.h.R` still declares the removed `na = "drop_all_models"` — the live UI can pass a value `tab_reg()` rejects; it also carries four dead options (`ids`/`strata`/`fpc`/`nest`) and `jmvtab.h.R` carries two (`test_robust`, `method_ratio`)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               | `jmvtabreg.h.R:184-190, 431-434`                           | pending `prepare()`                                                             |
| D12     | `jmv_tab3_base_key()`'s `reapplied` list contains `"ci_method"`, which is **not a key of `opts`**; the four `method_*` keys therefore land in `structural` and force a full tier-3 rebuild, making the cheap re-ref path unreachable for CI-method toggles                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               | `jmvtab-cache.R:666-668` vs `:712`                         | perf, no correctness risk                                                       |
| D13     | `tab(filter =)`'s cache key is hardcoded `NA_character_`, so a filter change never invalidates the jamovi cache                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          | `tab.R:2066`                                               | latent (0 known users)                                                          |
| D14     | `@param other_if_less_than` documents an argument `tab_counts()` does not have                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           | `tab-counts.R:222`                                         | doc                                                                             |
| D15     | stale comment: `jmvtab-cache.R:858-861` says `design_effect` "rides the global option, set around the build" — `.b.R:38-41` says the opposite and passes it as an argument                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               | `jmvtab-cache.R:858-861`                                   | doc                                                                             |
| **D3**  | **claim withdrawn.** An earlier draft of this study asserted that `tab_ci()`/`tab_chi2()` **drop** `meta` on the exported step path. They pass no `meta` argument (`tab.R:6244, 6255, 6366, 6377`), but *verified by running it*, `meta` survives — `tibble::new_tibble()` preserves the incoming object's attributes, and `tab_plain() | > tab_chi2() | > tab_ci()` keeps `vars` and a `set_caption()`. It is **undesigned, not broken**; passing`meta` explicitly costs six lines and removes the hazard class                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         | `tab.R:6244` etc.                                          | none (latent)                                                                   |

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
- **z17's `EST_SCALES` is KEY 2, three quarters built.** The declared library exists, is well
  documented, and already has the right nine rows; what is missing is only that the key is
  *recomputed by a dispatch* instead of being *stored*. KEY 2 is finishing a phase, not opening one.
- **No statistical soundness problem was found.** Every issue here is structural.

---

## Appendix B — how other packages answer KEY 1

Checked because the row-model question is not tabxplor-specific, and three neighbours have already
answered it. All three converge on the same rule and none of them uses a positional vector.

- **dplyr** — the grouping index is a `groups` tibble holding the group **key columns** plus `.rows`,
  and it is *regenerated from the named columns on every verb* through one funnel
  (`dplyr_reconstruct.grouped_df` = `grouped_df(data, group_intersect(template, data))`). Truth in
  the data, cache in the attribute, one recompute point, silent degrade when a named column is gone.
  Measured limits: the attribute is validated (`validate_grouped_df()`) and every non-`.rows` column
  is read as a grouping variable, so it cannot be extended with foreign data (§2.6.3).
- **gtsummary** — `.$table_body` carries `variable`, `var_label`, `label` and **`row_type`** as
  ordinary **columns**, with `.$table_styling` saying which are hidden at print. Exactly the
  "declared index block, as columns" design, and the closest precedent for KEY 1's Option A.
  The difference that matters for tabxplor: gtsummary's `table_body` is *internal* (the object the
  user holds is a list), so hidden columns are free. tabxplor's table **is** the user-facing tibble,
  so an extra `row_type` column would be visible clutter — which is precisely why the kind is better
  off inside a cell field (`row_kind`) or inside the label column.
- **gt** — the stub is declared by *column name* (`rowname_col`, `groupname_col`), and row groups can
  alternatively be taken from a `grouped_df` handed to `gt()`. Again: names, not positions.

Nothing found suggests a fourth mechanism. The design space really is the three carriers of §2.6, and
the packages that got this right all chose "the truth is in the data, the index is derived".
