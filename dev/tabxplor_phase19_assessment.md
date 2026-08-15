# Phase 19o — Assessment of Phase 19, and the keys that remain

**Written 2026-08-15, on commit `261d558` (Phase 19n).** This is the *assessment* asked for by
CLAUDE.md § "Phase 19o": did Phase 19 actually simplify the package, what simplification is still
available, and where are the remaining ad hoc mazes and white elephants?

Everything numeric below was **measured on this tree** (or on the named commit, via
`git ls-tree | git show`), not copied from a phase summary. Where a claim is *suspected* rather than
measured, it says so.

**Companion documents** — read the one that matches what you touch:

| document                                    | what it holds                                              |
|---------------------------------------------|------------------------------------------------------------|
| `dev/tabxplor_phase19p_api_review.md`       | 19p: the review of the ASK — fields/attrs, exports, arguments, options (and three corrections to the measurements below) |
| `dev/ecosystem_keys_2.md`                   | the Phase 19 study: measurements, eight keys, defect ledger |
| `dev/tabxplor_phase19_ecosystem_integration.md` | the Phase 19 plan of plans + every phase brief          |
| `dev/tabxplor_ecosystem_simplification.md`  | round 1 (Phase 17) — the disease patterns                   |
| `dev/tabxplor_architecture.md`              | the current architecture                                    |

---

## 0. The verdict, in one page

**Phase 19 did what it set out to do, and it made the package bigger.** Both halves are true and
neither cancels the other.

What it did: the package now has, for the first time, a **complete and explicit data model** —
a cell knows what it holds (21 fields), a column knows what it estimates and how (16 attributes),
a row knows what it is (`row_kind` + `tabxplor_lvl`), a table knows what kind it is
(`meta$spec`) — plus **~15 declared fact tables** that hold the vocabulary. Roughly 30 defects were
closed, several classes of them made *unrepresentable*. That is real, and it is the thing that makes
future work cheap.

What it cost, measured across the 17 Phase 19 commits:

| | before 19a (`7f95a6c`) | after 19n (`261d558`) | Δ |
|---|---|---|---|
| `R/` total lines | 39 586 | **44 278** | **+4 692 (+11.9 %)** |
| code (non-comment, non-blank) | 20 289 | **21 749** | +1 460 (+7.2 %) |
| comment lines | 16 235 | **19 263** | +3 028 (+18.6 %) |
| top-level functions | 915 | **1 066** | +151 (+16.5 %) |
| `cli_*` messages | 170 | **197** | +27 (+15.9 %) |
| exported functions | 84 | **93** | +9 |
| `tab()` formals | 51 | **52** | +1 |

**Sixteen of the seventeen commits added lines. Only 19l (the deletion pass) subtracted (−670).**

The honest reading is not "Phase 19 failed" — it is that **Phase 19 bought correctness and
future edit-cost with size, and it never attacked the two things that make the package feel large to
a user: the argument surface, and the number of exported concepts.** Those were out of its declared
scope (§12 of the plan: *"not a feature phase"*), and they are exactly what Phase 20 names.

The single most important structural finding of this assessment:

> **Phase 19 unified how facts are STORED and how rules are DECLARED. It did not unify how the
> package is ASKED.** Four crosstab producers still hand-declare 120 formals of which ~85 are the
> same argument written four times; two producers ask the same six questions under different names;
> and 9 of `tab()`'s 52 formals are deprecated arguments that are still *in the signature*.

Six remaining keys are proposed in §5. The two with the best value/effort ratio are **KEY α** (the
argument surface as data → `tab()`'s reference page drops from 695 Rd lines to ~250, and three
producers stop mirroring) and **KEY β** (build-time referential integrity between the fact tables —
~30 lines, and it would have caught the one Phase 19 defect that shipped as a hard error).

---

## 1. How this was measured

```bash
# per-commit census
for tag in <the 17 Phase-19 commits>; do
  git ls-tree -r --name-only $tag -- R/ | while read f; do git show $tag:"$f"; done | wc -l
done
```

Plus, on the working tree: `Rscript` introspection of `formals()`, `deparse()` length per exported
and internal function, the `NAMESPACE`, a scan of every `tabxplor.*` option's seed/read/doc counts,
a per-field emptiness matrix over seven representative tables, a referential-integrity probe across
the fact tables (`/tmp/.../fk2.R`), a cross-producer operation matrix, and a `Rprof` of the marginal
path. Scripts are in the session scratchpad; the ones worth keeping are named in §10.

---

## 2. The census, phase by phase

| commit | phase | `R/` lines | code | comment | fns | `cli_*` | exports |
|---|---|---|---|---|---|---|---|
| `7f95a6c` | *before 19* | 39 586 | 20 289 | 16 235 | 915 | 170 | 84 |
| `4d79c6b` | 19a floor / E1 | 39 750 | 20 214 | 16 469 | 926 | 170 | 84 |
| `d7850bb` | 19b KEY 2 | 39 981 | 20 278 | 16 627 | 931 | 173 | 85 |
| `28fd89f` | 19c KEY 4 | 40 427 | 20 404 | 16 928 | 947 | 173 | 85 |
| `a227fc2` | 19d KEY 8a | 40 852 | 20 584 | 17 156 | 956 | 178 | 85 |
| `a3b807c` | 19f KEY 1 | 41 182 | 20 674 | 17 370 | 978 | 179 | 89 |
| `c3c3c25` | 19g KEY 6 | 41 397 | 20 729 | 17 510 | 991 | 179 | 89 |
| `f9534bb` | 19e KEY 8b | 42 372 | 21 222 | 17 952 | 1008 | 189 | 90 |
| `547378b` | 19h KEY 7 | 42 708 | 21 382 | 18 101 | 1020 | 193 | 92 |
| `05d6027` | 19i pipeline | 43 053 | 21 494 | 18 323 | 1027 | 199 | 92 |
| `e97a236` | 19j KEY 5 | 43 450 | 21 666 | 18 512 | 1034 | 200 | 92 |
| `15442e9` | 19k jamovi | 43 667 | 21 691 | 18 700 | 1036 | 198 | 92 |
| `366398a` | **19l harvest 1** | **42 997** | **21 359** | **18 431** | 1032 | 192 | 92 |
| `300df05` | 19m-i | 43 291 | 21 416 | 18 659 | 1039 | 193 | 92 |
| `c50d482` | 19m-ii reg boundary | 43 734 | 21 539 | 18 940 | 1054 | 197 | 92 |
| `c9a391a` | 19m-iii display | 44 029 | 21 651 | 19 118 | 1061 | 197 | 92 |
| `261d558` | 19n docs / i18n | 44 278 | 21 749 | 19 263 | 1066 | 197 | 93 |

### 2.1 The message diagnostic — the precise reading

The study's headline diagnostic was *"72 % of everything the package says to a user is said while
negotiating arguments"*. 19l reported that share fell to 46 %. Measured now, split by kind:

| | before 19a | after 19n |
|---|---|---|
| `cli_abort` (refusal) | 122 | **149** |
| `cli_warn` (negotiation) | 11 | **11** |
| `cli_inform` (negotiation) | 37 | **37** |

**Every one of the +27 messages is an abort. The count of warnings and informs is byte-identical.**

That is the honest and rather good result, and it is *not* what "72 % → 46 %" says. What Phase 19
did was:

- **turn silence into refusal** (+27): `totaltab = "tabel"`, `conf_level = 95`, `n_min = -1`,
  `tab_reg(stats = "typo")`, `color_signif = "grey"` on the reg path, an unvalidated `baseline`,
  a formula `dependent` entering the multi-dependent recursion — all of these used to be accepted
  and silently do the wrong thing. **This is the single most user-valuable thing Phase 19 shipped**,
  and no line count captures it;
- **not reduce negotiation at all.** The 48 warnings/informs that say *"I changed what you asked for
  because of something else you asked for"* are the same 48. KEY 3 ("most arguments are consequences,
  not choices") declared the derivation graph as data (`MEASURES$requires`) — which is a genuine
  maintainability win and stops the four cascades disagreeing — but *the user still experiences every
  one of those consequences*.

And on the "which file" reading: those messages did not concentrate as much as reported either.
Counting every file that is an argument boundary today —

```
tab.R 23 · tab_reg.R 33 · reg-resolve.R 33 · tab-resolve.R 7 · tab-counts.R 10
tab-deprecate.R 4 · reg-estimand.R 11                              = 121 of 197 (61 %)
```

The 46 % figure counted only the two *original* files (`tab.R` + `tab_reg.R` = 56 of 197 = 28 %,
or 46 % on the study's own denominator). The messages moved to files whose names say "resolve" —
which is the right place — but **the amount of argument negotiation in the package has not gone
down**. Say it that way in future reports.

### 2.2 The other surfaces

| surface | measured now |
|---|---|
| `man/*.Rd` | 8 930 lines; `tab_reg.Rd` 722, `tab.Rd` 695, `fmt.Rd` 693, `tab_many.Rd` 448 |
| vignettes | 3 613 lines (3 EN + 3 FR mirrors) |
| tests | 22 716 lines, 90 files |
| `dev/*.md` | 38 608 lines |
| `jamovi/js` | 1 610 lines (no syntax gate — declined in 19n) |
| `jamovi/*.yaml` | 2 483 lines |
| global options | **46 names appear in `R/`; 33 are actually seeded at load; 39 are documented in `?tabxplor-options`**; 24 are read at exactly one site; 6 exist only inside historical comments. So **6 documented options are never seeded** (they are read with a fallback, or — like `tabxplor.color_style_type` — only to emit a deprecation) |
| exports never named in a vignette or README | **52 of 93** (was 48 of 84) |
| comment lines naming a phase number | **1 987** (10 % of all comments) |
| `# DESIGN:` / `# WARNING:` tags | 254 |

Comment density is 43.5 % of `R/`. That is a deliberate house style and this document does not
argue with it — but note that ~2 000 comment lines are now *archaeology* ("Phase 18z5: …",
"Phase 19c: …"). They are load-bearing while a phase is fresh and become noise afterwards; §7.6
proposes what to do about them.

---

## 3. What Phase 19 genuinely achieved

Stated plainly, because §0's headline number under-sells it.

### 3.1 The model is complete and explicit

Before, "what is this?" was answered by re-derivation at every read. Now:

```
per CELL      21 fields incl. row_kind (data|total|n|pct|pvalue|gof|blank)
per COLUMN    16 attributes: scale · pct_base · ci_method · conf_level · degf · basis ·
              col_var · col_group · totcol · refcol · ref · comp_all · color · color_signif ·
              model_family · role
per ROW       tabxplor_lvl (a factor subclass): role · var · ordered
per TABLE     meta$spec = list(kind, vars, call) + test + subtext
```

and the carrier hierarchy is *stated* (field 15/15 > column attribute 14/15 > `meta` 13/15 > bare
attribute 7/15), with an admission test for new attributes. That is a model a newcomer — human or
machine — can hold in their head. It did not exist in July.

### 3.2 The vocabulary is data, not code

Fifteen substantial declared relations, plus ~35 derived vocabularies:

`EST_SCALES` (11 rows × 16 cols) · `MEASURES` (6 × ~20) · `DISPLAY_TOKENS` (23 × 12) ·
`CI_GEOMS` · `CI_METHODS` · `COLOR_SCALES` (12) · `COLOR_ALIASES` · `REG_ESTIMANDS` ·
`REG_EMPIRICAL` · `REG_FAMILIES` · `REG_CHECKS` · `REG_OUTCOME_KINDS` · `TAB_ARG_VALUES` ·
`TAB_OPS` · `ROW_KINDS` · `fmt_attr_rules` · `meta_bind_rules` · `reg_footer_spec()`.

Twelve build-time `stopifnot()` blocks keep each table exhaustive on its own key set. Four roxygen
`@eval` generators render documentation *from* those tables (`fmt_fields_rd`, `display_tokens_rd` ×2,
`reg_measures_rd`) — a pattern that is now the package's standard and should be extended (§5.1).

### 3.3 Classes of defect became unrepresentable

Not "fixed" — *unrepresentable*. The ones worth naming:

- a printed CI method the bounds were never built with (D8) — the method is stamped where the
  interval is computed;
- `stars` and `color_signif` disagreeing about what a table compares (D26) — one resolved
  comparison, read by every consumer;
- a colour measure whose allow-list differs between producers (D4) — `names(MEASURES)` **is** the
  allow-list;
- a table-level fact silently dropped by a rebuild site (five instances in three phases) — the facts
  moved onto the columns, and `tab_meta_merge()` is the one rebuild idiom;
- arithmetic silently no-op'ing on `pct_ci`/`mean_ci`/`pvalue` columns (19m-iii) — one build-time
  assert ties `get_num()`, `set_num()` and `DISPLAY_TOKENS` both ways;
- a regression estimand reachable only by naming the wrong distribution (`family = "rr"`).

### 3.4 Two capability gaps closed, at almost no cost

`measure = "ratio"` on a binary outcome (modified Poisson, by name) and `measure = "difference"`
(identity-link additive risk), plus the ratio of adjusted means. Each is one `reg_fit` arm because
`EST_SCALES`, the CI engines and the crude-twin table already existed. That is the *payoff shape*
Phase 19 was aiming at, and it worked.

### 3.5 The keystone landed

`tab_apply_tests()` is deleted; the leaf computes cells, interval and test in one pass; the whole
pre-2.0.0 step chain is quarantined in one file that nothing on the build path calls. 2.0.0's own
stated goal is met.

---

## 4. Where the +4 692 lines went — five mechanisms

Understanding *why* it grew is what tells you whether Phase 20 will grow it again.

**4.1 A declared table costs more lines than the code it replaces, and its prose costs more still.**
`EST_SCALES` is 11 rows × 16 columns ≈ 140 lines of data plus ~55 lines of header prose, replacing
~6 dispatches of 8–20 lines each. Line-for-line it is roughly a wash; with the prose it is a net
add. That is the *right* trade (the dispatches could disagree; the table cannot) but it must be
budgeted honestly rather than reported as simplification.

**4.2 Every stored fact grew an accessor pair, and most got exported.** Phase 19 added 9 exports
net: `get_scale`/`set_scale`, `get_pct_base`/`set_pct_base`, `get_row_kind`/`set_row_kind`,
`get_col_group`, `is_lvl`, `new_lvl`, `tab_shape`, `tab_supports`, `reg_measures`,
`conf_level_to_z`. Measured, the setters have **zero test callers and zero external users**:
`set_scale` 5 internal / 0 tests, `set_pct_base` 5 / 0, `set_row_kind` 3 / 0, `set_col_var` 5 / 0.
There was an admission test for a new *attribute*; there was none for **exporting its accessor**.

**4.3 New modules were added, none removed.** `row-model.R`, `table-spec.R`, `tab-shape.R`,
`reg-estimand.R`, `reg-resolve.R`, `tab-leaf.R`, `tab-chi2.R`, `tab-display.R`, `tab-deprecate.R`.
The four `tab.R` splits are pure relocation (good); the five genuinely new ones are ~2 500 lines.
Meanwhile `tab-steps-legacy.R` (1 433 lines) was *quarantined* rather than removed, and the
deprecated formals stayed in the signatures.

**4.4 The mirrors were resolved but not declared.** 19i gave four producers one *resolver*. It did
not give them one *declaration*: `tab_counts()` still writes out 34 of `tab()`'s formals with 34
`@param` blocks. That is the biggest single block of duplicated surface left (§5.1).

**4.5 Deletion was one phase out of seventeen.** 19l removed 670 lines; the fifteen phases around it
added 5 362. A "harvest" phase after each *pair* of structural phases, rather than one at the end,
would have kept the count flat — and would have caught 19d's dangling foreign key three phases
earlier.

---

## 5. The remaining keys

Same discipline as Phase 19: **each key names a fact the code needs but does not store, or a rule it
applies but does not state**, and the scattered special cases it collapses. Ordered by
value ÷ effort.

---

### KEY α — the argument surface is not modelled

> **The fact**: *which producer takes which argument, what it means, what it may be, and which option
> is its session default.*

#### The measurement

| producer | formals | of which shared with `tab()` | own |
|---|---|---|---|
| `tab()` | **52** | — | 52 |
| `tab_counts()` | **40** | **34** | 6 (`counts`, `wt_counts`, `cols`, `col_name`, `base`, `input`) |
| `tab_plain()` | **29** | **25** | 4 |
| `tab_num()` | **28** | **24** | 4 |
| `tab_reg()` | 29 | 8 | 21 |

**~83 of the 149 crosstab formals are the same argument written a second, third and fourth time**,
each with its own `@param` block. Measured across `R/`: `@param color` appears **15 times**,
`@param ref` 9, `@param conf_level` 8, `@param stars` 8, `@param na` 7, `@param display` 7,
`@param comp` 7, `@param color_signif` 7. Only **one** `@inheritDotParams` exists in the whole
package, and `@inheritParams` is used 42 times (36 of them inside `fmt`'s own topic).

`tab()`'s own 52 break down as:

| cluster | n | arguments |
|---|---|---|
| population | 11 | `data row_vars col_vars tab_vars wt filter na levels other_if_less_than other_level cleannames` |
| cell content | 6 | `pct digits display add_n add_pct n_min` |
| comparison | 6 | `ref ref2 comp color color_signif color_breaks` |
| inference | 7 | `test anova ci conf_level stars ci_method design_effect` |
| **totals** | **5** | `tot totaltab totaltab_name total_names common_totrow` |
| output / layout | 4 | `output_list spread_vars subtext parallel` |
| **deprecated, still formals** | **9** | `sup_cols OR chi2 row_var col_var names_prefix names_sort method_cell method_diff` |
| internal dot-args | 4 | `.cache .defer_level_merge .return_armed .levels_order` |

`TAB_ARG_VALUES` (19i) already declares 9 arguments — but only `values`/`leaf`/`size`/`na_ok`. It
has no `producers`, no `option`, no `group`, no `doc`, no `default`.

#### The proposal

Grow `TAB_ARG_VALUES` into **`TAB_ARGS`**: one row per crosstab argument, columns
`producers` · `group` · `default` · `option` · `values` · `size` · `na_ok` · `status`
(`live`/`deprecated`/`internal`) · `doc`. Then:

1. **Generate the `@param` blocks** with `#' @eval tab_args_rd(producer = "tab_counts")` — the
   fourth use of the `reg_measures_rd()` pattern the package already standardised on. `tab.Rd`
   groups by `group`; the three superseded producers document only their own 4–6 formals plus
   `@inheritDotParams tab`.
2. **The three superseded producers take `...`**, forwarded to the shared resolver:
   `tab_counts(data, counts, cols, col_name, base, input, wt_counts, ...)`. ⚠ **Mandatory**: a
   `tab_check_dots()` that matches every name against `TAB_ARGS[producers]` and aborts with a
   "did you mean" — otherwise a typo that errors today would silently vanish. That check is
   *better* than today's behaviour, not merely equal, because it can suggest.
3. **The 9 deprecated formals leave the signature** and are caught by name in `...`. `tab()`
   52 → **39**, and `?tab` stops opening with nine badge blocks. ⚠ Three cautions, all verified:
   - positional calls past argument 5 break (`sup_cols` is currently 6th). Acceptable in a major
     version *if* `tab()` aborts on a 6th unnamed argument with a clear message — the discipline
     `tab_many()`'s shim already uses;
   - `sup_cols`, `OR`, `chi2`, `row_var`/`col_var` already have real translating shims
     (`tab_deprecate_sup_cols/_or/_many`); **`names_prefix`/`names_sort` do not** — they are badged
     but still *live*, forwarded straight through to the spread path (`tab.R:615, 832, 1270`). They
     must either be forwarded from `...` or moved onto `tab_spread()`, which is Phase 19's own open
     question #4 that 19h left unsettled;
   - `method_cell`/`method_diff` are read with `missing()` (`tab-resolve.R:637-638`), which does not
     work through `...`. Convert to a `NULL` default first.
4. **`option` becomes a column**, so `conf_level = conf_level_default()` and its eleven siblings
   stop being hand-written formal defaults, and `?tabxplor-options` is generated from the same
   table. This closes the last "two encodings kept in sync by comment" in the argument layer.

#### Expected effect (estimated, not measured)

`tab.Rd` 695 → ~250 · `tab_many.Rd` 448 → ~60 · `tab_plain.Rd` 279 → ~70 ·
`tab_num.Rd` 208 → ~60 · `tab_counts.Rd` 137 → ~70. Roughly **−1 300 Rd lines and −80 formals of
mirror**, with `tab()`'s own taught surface untouched.

#### Caveats, honestly

- **`...` costs IDE completion** on the three superseded producers. That is why `tab()` itself keeps
  every live formal — completion on the one taught entry point is worth more than the symmetry.
- **jamovi reads formal names.** `jmvtab.a.yaml`'s option names are `tab()`'s arguments; nothing in
  the module introspects `formals()`, so this is safe — but verify before moving a formal.
- **`TAB_ARGS$doc` puts prose in a data table.** Keep it to one sentence per argument and let long
  prose stay in `@details`/`@section` — otherwise you have re-invented roxygen inside a list.
- This is the change that most needs a **characterisation harness first** (the
  `dev/verify_reg_specs.R` pattern): dump every producer's resolved settings over a grid of calls,
  `save` before, `check` after.

---

### KEY β — the fact tables have no referential integrity

> **The rule**: *a key written in one declared table and read in another is a foreign key, and
> foreign keys are checked at build time.*

#### The measurement

There are **7 top-level build-time `stopifnot()` blocks** (`tab-agg.R:443`, `fmt_class.R:2216`,
`:2355`, `:4543`, `:4668`, `tab-display.R:800`, `reg-estimand.R:907`). **Five are intra-table**
("does this table cover its own key set?"). Only **two are genuine cross-table checks**
(`COLOR_BUILD_ORDER` ↔ `MEASURES$builds`, and `REG_FIT_FAMILY` ↔ `REG_CHECK_FAMILIES`) — and both
were added *reactively*, after the corresponding key had already dangled in a shipped commit. There
are **at least 12 more cross-table foreign keys and none of them is checked**:

| from | column | to |
|---|---|---|
| `EST_SCALES` | `label_meas` | `names(MEASURES)` |
| `EST_SCALES` | `break_key`, `gap_key` | `COLOR_SCALES` |
| `EST_SCALES` | `geometry` | the `measure`/`color` vocabulary |
| `MEASURES` | `scale` (3 slots), `guar$scale` | `COLOR_SCALES` |
| `MEASURES` | `builds` | `COLOR_BUILD_ORDER` *(this one **is** checked)* |
| `CI_GEOMS` | `scale_key` | `EST_SCALES` |
| `CI_GEOMS` | `method_slot` | `CI_METHODS` |
| `REG_ESTIMANDS` | `scale` | `EST_SCALES` |
| `REG_ESTIMANDS` | `crude_fam` | `REG_EMPIRICAL` |
| `REG_ESTIMANDS` | `display` | `DISPLAY_TOKENS` |
| `REG_ESTIMANDS` | `fit` | `REG_FIT_FAMILY` / `REG_CHECK_FAMILIES` *(checked since 19l)* |
| `DISPLAY_TOKENS` | `comparison` | `names(MEASURES)` |
| `DISPLAY_TOKENS` | `field` | `fmt_field_names` |
| `COLOR_SCALES` | `derive` | `COLOR_SCALES` |

**All of them are currently intact** — I probed every one. But **one has already broken in
production**: 19d's rename of the colour measures to full words did not reach
`EST_SCALES$label_meas`, the forest plot lost its `1/2` glyphs and *errored on lookup*, and the fix
shipped with a comment that now reads

```r
#   label_meas which MEASURES row supplies the break glyphs …
#              WARNING: a MEASURES KEY -- 19d's full-word rename had to reach here, and did not
```

A `WARNING:` comment telling the next person to remember a cross-table key **is hard rule 4's
forbidden pattern ("two encodings kept in sync by comment") applied one level up.** The same shape
appears in `REG_CHECK_FAMILIES`, which 19l had to fix with a build-time assert *after* discovering
that 19e's two new estimands silently had no model checks at all.

#### The proposal

One block, ~30 lines, at the tail of `R/reg-estimand.R` (the last file where every table is in
scope, following the precedent already established there):

```r
# Foreign keys between the declared tables. A dangling key is a LOAD failure, not a runtime one:
# every one of these is written by hand in one table and read by name in another.
tx_fk <- function(what, values, ok) {
  v <- unique(values[!is.na(values) & nzchar(values)])
  if (length(setdiff(v, ok)))
    stop("tabxplor: dangling key in ", what, ": ", paste(setdiff(v, ok), collapse = ", "))
}
tx_fk("EST_SCALES$label_meas", vapply(EST_SCALES, `[[`, "", "label_meas"), names(MEASURES))
...
```

Plus the reverse checks that are meaningful (a `COLOR_SCALES` row no table references is dead
weight; measured, there are currently none).

#### Caveats

- ⚠ **`$` partial-matches on lists.** `MEASURES$adjustment` has `scale_from` and no `scale`, so
  `md$scale` returns `"gap"`. `fmt_color_plan()` guards it with an explicit
  `if (identical(md$scale_from, "gap"))` — correct today, but it means the tables must be read with
  `[["…"]]`, not `$`, in any generic checker. State that rule where the tables live.
- This key is cheap and has no user-visible effect. It is the highest value/effort item in this
  document.

---

### KEY γ — `tab_reg()` has an argument boundary but no staged build

> **The fact**: *which stage of a regression build produced which part of the table.*

#### The measurement

19m-ii was excellent: `tab_reg()` 821 → 147 lines, 30 messages → 1, six declared resolution stages.
But it stopped at the boundary. `reg_build()` is still **the largest function in the package** —
534 deparsed lines (next: `plain_core` 482, `format.tabxplor_fmt` 338, `num_core` 310) — and it
contains **seven local closures** (`cols_ame`, `cols_vsrest`, `cols_coef`, `emp_frame_of`, `emp_of`,
`set_obs_if`, `add_emp_cols`) against **three** in the entire 670-line factor leaf.

Reading it, it is eleven sequential phases with no names:

```
split_var recursion → reref/skeleton → 3 column builders → GOF/comparison/global/checks
→ labels/shape/multiplier/curves → tibble + add_n → the empirical block (~100 L)
→ obs + gap_se → multinomial tips → numeric overlay → reg_finalize
```

Meanwhile `tab_build()` has had `new_ctx()` (71 declared keys) + six named stages since 17e/19i.
**The asymmetry is not stylistic**: it is why `dev/verify_reg_specs.R` had to characterise
`tab_reg()` through its *output* rather than at a stage boundary, and it is the reason Phase 20f
(parallelising `tab_reg`) has nowhere to attach — the fit stage is not addressable.

#### The proposal

`new_reg_ctx()` + named stages, mirroring `tab_build()` one-for-one:

```r
reg_build <- function(ctx) {
  ctx <- reg_stage_split(ctx)      # the split_var recursion (or a no-op)
  ctx <- reg_stage_fit(ctx)        # fits + skeleton + reref     <- THE parallel seam (20f)
  ctx <- reg_stage_columns(ctx)    # the 3 per-spec builders
  ctx <- reg_stage_empirical(ctx)  # crude twins + obs + gap_se + tips + numeric overlay
  ctx <- reg_stage_footer(ctx)     # GOF + comparison + global + checks + curves
  reg_finalize(ctx)
}
```

`new_reg_ctx()`'s formals are the contract (the `new_reg_shared()` / `new_ctx()` idiom, twice
proven), and the `globalVariables()` mirror is derived from them as `tab.R` already does.

#### What it unlocks

1. **Phase 20f becomes tractable.** `reg_stage_fit()` is the only stage that fits models, and it is
   the only one whose payload crosses a process boundary. The existing `tab_pmap()` trampoline
   and the named `"tabxplor"` mirai pool attach there with no second pool.
2. **The `empirical` subsystem gets a name.** ~100 inline lines in `reg_build` plus
   `reg_empirical()` (193 deparsed) plus `reg_empirical_columns()` (244) is the third-biggest
   subsystem in the package and it is currently spelled as an `if` block.
3. **A `spec$call`-driven recompute** (19m's open question) becomes expressible: replay from a
   stage rather than from the top.

#### Caveats

- **`reg_build` recurses** (`split_var`, and `tab_reg` recurses over model lists). A ctx makes that
  cleaner, not harder, but the recursion must stay at the top, exactly as `tab_build_tables()` does.
- **`.fit_cache` is an environment** and 19's ruling was *"keep as is, do not improve"*. A staged
  build must thread it untouched; `reg_reref_fit_res`'s byte-identity lock is a hard contract.
- This is a **pure refactor with no user-visible change** — it therefore *must* be gated on
  `dev/verify_reg_specs.R` printing IDENTICAL, which is exactly what that harness was built for.

---

### KEY δ — the footer/test subsystem is the last one with no model

> **The fact**: *what kind of statistical row this is, what it is about, and how it renders.*

#### The measurement

The `test` attribute is now a **15-column tibble** (`var col test statistic df1 df2 pvalue n min_e
effect_size es_type pvalue_exact deff dep col_group`) **plus** dynamically-added grouping columns
named after the `tab_vars`/`split_var`. It grew 13 → 14 → 15 during Phase 19 alone.

It carries **at least 20 kinds of row** under one `test` discriminator: `chi2`, `chi2_design`,
`F_welch`, `F_classic`, `F_design`, the GOF keys (`n`, `lr_null`, `wald_null`, `mcfadden_r2`,
`nagelkerke_r2`, `cox_snell_r2`, `r2`, `r2_adj`, `sigma`, `aic`, `bic`, `phi`), eight
`compare_*` keys, `global_lr`/`global_f`/`global_wald`, and the five `REG_CHECKS` rows. Different
`test` values populate different columns; nothing declares which.

**The reg half has a table** — `reg_footer_spec()`, 24 fixed entries + 2 generated blocks. **The
crosstab half has none**: `test_display_rows()` filters on a hard-coded
`c("chi2", "chi2_design", keep_f, "F_design")`, and `tab_kind()`'s degraded fallback sniffs the same
literals from a different file. That is the last surviving instance of "a subsystem's vocabulary
lives as string literals in its consumers".

#### The proposal

`TEST_ROWS`: one row per test kind, columns `producer` (`tab`/`reg`) · `scope`
(`table`/`model`/`term`/`cell`) · `label` (a gettext closure) · `kind` (`pvalue`/`gof`/`effect`) ·
`digits` · `stat_glyph` · `render` (`grid`/`line`) · `family` (which `EST_SCALES`/`REG_FAMILIES`
rows can produce it). `reg_footer_spec()` becomes the `producer == "reg"` slice; the crosstab filter
becomes `TEST_ROWS[producer == "tab" & render == "grid"]`; `tab_kind()`'s sniff reads the
`producer == "reg"` keys instead of a private list.

#### What it fixes / prevents

- The Welch/classic pair is currently *both stored* and one is picked at display by
  `options(tabxplor.anova)`. That is **exactly the right pattern** (differentiator 1 applied to
  tests) — but it is undeclared, so nobody knows they may add a third F without a code change.
- `min_e`, `deff`, `pvalue_exact`, `effect_size`, `es_type` are populated only on some rows and NA
  elsewhere, with no statement of which. On a 15-column union type that is a real reading cost.
- `tab_kind()`'s fallback and `test_display_rows()`'s filter are two hand-written lists of the same
  crosstab discriminators, in two files. They agree today.

#### Caveats

- **`pvalue_exact` on the chi2 row is good design, not a smell.** Fisher's exact is stored *on* the
  chi2 row rather than as a second row, so the tidy shape and row count are stable. Keep it; declare
  it as a *column* of the chi2 row, not a row of its own.
- The dynamically-added grouping columns are read by `setdiff(names(tt), names(new_test_tibble()))`.
  That works, and 19g/19m-ii both had to fix a defect caused by an undeclared column being read as a
  grouping variable. `TEST_ROWS` does not change that mechanism — but the schema *must* stay
  declared in `new_test_tibble()`, and the rule deserves one sentence in the header.

---

### KEY ε — one storage vocabulary, two argument vocabularies

> **The rule**: *if two producers ask the same question, they ask it with the same word.*

Phase 19's §5.2 unified the **geometry** words end to end (`difference`/`ratio`/`odds_ratio`/`log`
in the argument, the attribute, the legend and the plot axis). That was the intellectual core of the
phase and it worked. But it stopped at geometry. Measured, six questions are still asked twice:

| the question | `tab()` | `tab_reg()` | verdict |
|---|---|---|---|
| **which sub-populations** | `tab_vars` | `split_var` | **same concept, two names.** Since 19f, `tab_reg()` *stamps `split_var` as a `tab_var` role* — the storage is already unified, only the argument is not |
| **how is the interval computed** | `ci_method` (a named vector over 4 slots) | `method` (`wald`/`profile`) | same question, two names, two vocabularies |
| **default colour** | `color = "no"` | `color = TRUE` | opposite defaults for one argument |
| **default significance policy** | `color_signif = "ignore"` | `color_signif = NULL` → `grey_non_signif` | opposite defaults |
| **what goes in the footer** | `test` (logical) | `stats` + `compare` + `baseline` (+ `check` on the plots) | one concept, four arguments |
| **missing data** | `na = keep/drop/drop_all/common_base` | `na = drop_by_outcome/drop_by_model/drop_all` | one argument name, two disjoint vocabularies except `drop_all` |

`reference` (`tab_reg`: relevel a predictor's baseline) vs `ref`/`ref2` (`tab`: pick the comparison
row/column) is a *seventh* near-collision — genuinely different operations, confusingly close names.

#### The proposal

Back-compat on `tab_reg()` is waived, so this is nearly free:

1. **`split_var` → `tab_vars`.** One name for "one table per group", on both producers, matching the
   storage that already says so. (`tab_reg(tab_vars = region)`.)
2. **`method` → `ci_method`**, and extend the named-vector grammar:
   `ci_method = c(model = "profile")`. One argument, one grammar, `CI_METHODS` gains a `model` slot.
3. **Align the two defaults.** Either `tab_reg(color = "no")` or `tab(color = TRUE)` — the second is
   the better user story (tabxplor's whole point is colour) but it is a released behaviour change on
   `tab()`, so it needs a ruling. `color_signif` likewise.
4. **`stats`/`compare`/`baseline` → one `footer =`** taking the `TEST_ROWS` keys (KEY δ), with
   `compare`'s two values expressed as keys. Reduces `tab_reg()` 29 → 27 formals and gives KEY δ its
   consumer.
5. **`na`**: keep two vocabularies (they genuinely describe different operations) but say so in one
   place — `TAB_ARGS`/`REG_ARGS` `values` column, and one sentence in each `@param`.

#### Caveats

- ⚠ `tab_vars` on `tab_reg()` reads oddly to a modeller ("tab" is a crosstab word). The alternative
  is to rename **both** to something neutral (`by =`, `groups =`) — but `tab_vars` is CRAN-released
  since 1.0 and `by` collides with dplyr 1.1's `by =`. **Recommendation: `tab_vars` on both, with
  `split_var` as a permanent silent alias on `tab_reg()`.** Ask the maintainer.
- Item 3 is the only user-visible change in this key and it is the one worth doing carefully.

---

### KEY ζ — the package computes the average marginal effect twice, and pays for the slow one

> **The fact**: *which estimands tabxplor can differentiate analytically.*

This one is not a structural key — it is a **measured performance and UX defect** with a structural
cause, and it is the root of Phase 20e ("marginal effects for a logit regression is neverending").

#### The measurement

On `gss_simple` (21 483 rows), one binary outcome, four predictors:

| call | elapsed |
|---|---|
| `tab_reg(effect = "coefficient")` | **1.06 s** |
| `tab_reg(effect = "coefficient", empirical = TRUE)` | 0.80 s |
| `tab_reg(effect = "marginal")` | **15.32 s** |
| `tab_reg(effect = "marginal", empirical = TRUE)` | 12.35 s |
| a 3 × 2 coloured crosstab, for scale | 1.28 s |

`Rprof` over the marginal call (42.9 s of samples, profiling overhead included):

```
reg_build       99.8 %
 cols_ame       97.8 %
  reg_marginal  97.5 %
   marginaleffects::comparisons  80.9 %
    get_jacobian                 85.3 %   <- a NUMERICAL derivative, one pass per coefficient
```

And directly against `marginaleffects`, same fit, same variables:

```
avg_comparisons(f, newdata = f$model, variables = …)                 5.82 s
avg_comparisons(f, newdata = f$model, variables = …, vcov = FALSE)   0.85 s   (7x)
estimates identical: TRUE
```

**tabxplor already owns the exact analytic standard error for this quantity.**
`reg_ame_if_maker()` (`R/reg-influence.R`) is the two-term marginal influence function, and the
package's own tests pin it to `marginaleffects`' SE **to 10 decimals**. It is currently called only
in the gap-test path (`reg_gap_se_columns`), i.e. *tabxplor computes the AME's variance analytically
for the colour and then pays `marginaleffects` to compute it again, numerically, for the printed
interval.*

#### The proposal

Where `reg_ame_if_maker()` / `reg_ame_if_cat_maker()` applies, call
`avg_comparisons(..., vcov = FALSE)` and take the SE from the influence function. Declare
*where it applies* as a `REG_ESTIMANDS` column (`se = "analytic" | "numeric"`) rather than as an
`if` — that is what makes this a key and not a patch.

Expected: **15.3 s → ~2 s** on the measured call, and the jamovi Regressions panel stops freezing on
the second-most-used option. It also removes a heavy Suggests from the hot path (the estimate call
itself is 0.85 s and could later be replaced by `rd_link_y()`-style g-computation, which the package
also already has).

#### Caveats

- ⚠ **Not every marginal path has an analytic IF.** `reg_ame_if_maker()` covers lm/glm/svyglm;
  `reg_ame_if_cat_maker()` covers multinom/polr; `at = "reference"` profiles and
  `measure = "ratio"` marginals need checking one by one. The `se` column must be **conservative**:
  default `"numeric"`, opt a row in only with a test that pins it.
- ⚠ **This changes printed standard errors** in the last decimal even where the two agree to 1e-10.
  Reg tables are value-asserted, not snapshotted, so most tests will not move — but declare it and
  prove it with a tolerance-explicit fixture.
- ⚠ Do not "improve the statistic while passing through" (the standing Phase 19 rule). The claim is
  *identical maths, cheaper route*, and it must be demonstrated, not assumed.

---

### KEY η — there is no single statement of the model

> **The rule**: *a package whose whole value is a data model states that model in one place.*

The model of §3.1 is real but it is spread over `fmt_class.R` (cell + column + colour),
`row-model.R` (row), `table-spec.R` (table), `tab-display.R` (display), `tab-agg.R` (intervals),
`reg-estimand.R` (estimands) and `tab-resolve.R` (arguments) — plus ~15 fact tables in 8 files,
each with its own reader convention and its own "read ONLY through …" rule. A newcomer (or a fresh
agent session) cannot find "the model"; they find CLAUDE.md's repository map, which is now ~400
lines of prose and is itself the largest single description.

This is cheap to fix and pays every future session:

- **`R/tabxplor-model.R`** — a doc-only page (`?tabxplor-model`), on the `?tabxplor-options`
  precedent, with the four-carrier table of §3.1, the list of declared relations and **the graph
  between them** (which is KEY β's foreign keys, drawn), each `@eval`-generated from the tables so
  it cannot drift.
- **One naming convention for the readers**, stated once: `<TABLE>_<key>()` for "one row's fact",
  `<table>_keys()` for the vocabulary. Today the conventions are `measure_facts()`,
  `est_var_kind()`, `ci_geom()`, `reg_estimand()`, `fmt_col_block()`, `tab_supports()` — six shapes.
- **A rule for the archaeology.** ~2 000 comment lines name a phase. Proposal: a comment keeps its
  phase tag only while it explains *why the code is not the obvious thing*; a tag that merely records
  *when* something changed belongs in git. Sweep at each release, not each phase.

---

## 6. White elephants — the honest list, 2026-08-15

"Cut" = free (internal, unreleased, or zero users). "Deprecate" = CRAN etiquette first.
"Keep" = suspicion checked and dismissed.

| item | evidence (measured) | verdict |
|---|---|---|
| **`R/tab-steps-legacy.R`** (1 433 L, 776 code) | **zero callers in `R/`.** `tab_tot`, `tab_ci`, `tab_chi2`, `tab_pct`, `tab_totaltab`, `pct_formula`, `diff_formula`, `detect_firstcol`, `tab_match_groups_and_totrows`, `tab_add_totcol_if_no`, `tab_validate_comp`, `tab_match_comp_and_tottab` have no caller outside that file and `test-steps-legacy.R`. It carries a **second implementation of the plan** (an 8-branch `case_when`, a second `ci = "ratio"` fold, a third `stars` resolution, a `degf`-from-columns fallback, four table-MUTATING passes) that must track the leaf's arithmetic forever | **proposed: hard-deprecate in 2.0.0, defunct in 2.1.0** — ⚠ needs a ruling (§11 q. 2). Phase 19's anti-proposition says *"do not delete `tab_ci()`/`tab_chi2()` as exported functions — supersede them, move the computation"*; that has been done, and the question of whether the *chaining API itself* is still wanted is a separate one nobody has been asked. 3.2 % of `R/`, the largest single deletable block; the arithmetic is already shared (`ci_dispatch()`, `chi2_compute_test()`), so nothing is lost but the chaining API |
| **the 9 deprecated formals of `tab()`** | `sup_cols OR chi2 row_var col_var names_prefix names_sort method_cell method_diff` — all badged, all shimmed | **move into `...`** (KEY α item 3). `tab()` 52 → 43 |
| **`auto_or`** (`tab-resolve.R:116`) | `auto_or <- rep(FALSE, length(pct_vect))` — a local **pinned to FALSE**, so `MEASURES$odds_ratio$auto_for` still declares a context `"or_table"` that is now unreachable. A 19d corpse | **cut both** (rule 1: delete the traces in the same phase) |
| **`tabxplor.color_style_type`** | seeded **0** times, documented as an `\item`, read once — and that read emits a deprecation warning | **cut the option and its doc item** |
| **`kable_tabxplor_style()`** | a defunct stub since 19l; still exported, still in `_pkgdown.yml`'s reference index | **cut** (19n already flagged the pkgdown half) |
| **6 option names that exist only in historical comments** | `tabxplor.always_add_css_in_tab_kable`, `.compact`, `.fuse_min_rows`, `.kable_html_font`, `.kish_neff`, `.tab_kable_engine` — 0 seeds, 0 reads | **already gone**; the comments naming them are the archaeology of §5, KEY η |
| **the exported `set_*` accessors** | callers in `R/` / in `tests/`: `set_scale` 5/**0**, `set_pct_base` 5/**0**, `set_row_kind` 3/**0**, `set_col_var` 5/**0**, `set_model_family` **0**/4, `set_diff_type` 2/**0**. (`set_display` 38/49 and `set_num` 7/8 are genuinely used — keep those) | **un-export the six** (keep them internal). A user has no reason to *write* a column's scale; if they do, they break the invariant the phase existed to create. Four of the six are new in 2.0.0, so it is free |
| **`complete_partial_totals`** | exported, **1** internal caller, 0 tests, 0 vignette. 19a's "cut" verdict was reversed because a caller was found — but the caller is internal | **un-export** |
| **`tab_get_wrapped_dimensions`** | exported, **0** callers anywhere including tests | keep (maintainer's explicit ruling: personal tooling) — but it should be `@keywords internal` |
| **`tabxplor.totcol_range` + the dormant `totcol_range` blocks** | option seeded, 1 read, **never documented**; three unreachable `tmpl` branches follow from its hardcoded `rng <- NULL` | keep dormant (maintainer's ruling) — but **document the option or seed it not at all**; a seeded, undocumented option is worse than either |
| **`tabxplor.jmv_full_hash`** | 1 seed, 2 reads, jamovi-internal | make it an internal constant, not a user option |
| the five options still named after `tab_kable()` | `tab_kable_css`, `tab_kable_tooltips`, `tab_kable_num_font`, `kable_popover`, `output_kable` — for a function renamed `tab_html()` in 18g, under **three** different prefixes | ⚠ **ALREADY RULED ON — the renames were explicitly dropped in 19m-iii.** Recorded here only because Phase 20c re-opens the options question in general; `tx_getOption()` (17j) makes it a two-line rename with silent aliases if the ruling is ever revisited. **Do not re-issue it as work.** |
| **`tab_counts()`'s 34 mirrored formals** | measured above | **`...`** (KEY α) — the function itself stays, its six real arguments are its job |
| **the vignette claim "`tab_reg()` has no `display` argument"** | `vignettes/tabxplor-reg.Rmd:241` and `articles/tabxplor-reg-fr.Rmd:248` — **19e gave it one.** 19n fixed the identical claim in `?tab` and missed the vignette | **fix now** — it is in the vignette that teaches the package's third differentiator |
| the `wn` field's population rule | measured: an **unweighted** `color = "contrib"` table has `wn` populated (identical to `n`); an unweighted plain one does not. The `set_wn(col, get_wn(col))` quirk 19j preserved | harmless (`get_wn()` coalesces) but it is *a field whose population depends on an unrelated argument*. State the rule or drop the write |
| **the converse — cold but good, do not cut** | `tab_counts()`, `tab_css()`, `transpose =`, `n_min =`, `common_totrow`, `score_from_lv1()`, `tab_compact()`'s cross-call merge, `filter =`, `method = "profile"`, the compound-formula hatch | keep |

---

## 7. Remaining complexity that may not pay

Not defects — places where the reading cost is high and the benefit is unclear. Each needs a
maintainer judgement, not a refactor decision.

**7.1 `plain_core()` is 482 deparsed lines and does eleven things.** It is the aggregate core, so
length is partly inherent — but it opens with a `list2env()` of the inference setup, then a
data.table name round-trip with four internal sentinels (`"col_var"`, `"_colvarbis"`, the
`"n_"`/`"wn_"` prefixes, `"Total"`), then the wide reshape, the reference sweep, the interval, the
test, the totals and the finish. 19l's declaration block (the ~14 optional `tabs_*` locals) made it
*readable*; splitting it into `leaf_reshape()` / `leaf_compare()` / `leaf_infer()` / `leaf_totals()`
would make it *navigable*. Low urgency, real cost.

**7.2 The `"Total"` sentinel is the last string-as-declaration.** 19m-iii settled this correctly:
`"Total"` is the leaf's **pre-rename key**, the fourth of its internal names, and substituting
`total_names[1]` upstream would be a bug. But it means a source factor level genuinely named
`"Total"` is indistinguishable from the sentinel until `leaf_rename_totals()`. Untested, probably
rare, and the honest fix (a reserved-name check at `tab_prepare()`) is one `cli_abort`.

**7.3 The archaeology.** ~2 000 phase-tagged comment lines; `Phase 18z` alone appears 382 times.
See KEY η.

**7.4 Two `n`-ish quartets.** `n` / `wn` / `tot_n` / `n_eff` was ruled "one accessor, not a merge"
and `fmt_base()` delivered that. Measured on seven table shapes, `n_eff` and `gap_se` are empty in
*all* of them (they need `design_effect = TRUE` / a reg gap test). That is fine — they are the
differentiators — but it does mean the record's *typical* occupancy is 6–9 of 21 fields. The
"no sparse record" verdict stands (z6 measured the payoff at 0.03 %); recording the occupancy here
so nobody re-opens it.

**7.5 `?fmt` is 693 Rd lines** for one type, now partly generated. Two of its `@param` glosses
(`ctr`, `obs`) duplicate the generated section — 19n's own open item.

**7.6 The transpose is a synthetic model.** `tx_transpose_render()` is 207 deparsed lines producing
a `$tab` of plain characters with `$cells` pre-formatted. 19h made it a *modification* of `rd`
rather than a 39-slot literal, which closed D1 — but it remains a second render model that every
backend must tolerate. `tab_transpose()` on a **regression** table aborts (measured:
`"tab_transpose() does not support this table."`), which is correct-by-`TAB_OPS` but is also the
one cross-producer operation that did not become uniform.

**7.7 jamovi is two modules with one kernel.** Measured, that is fine: `jmv_backend_weights/_export/
_render_html/_notice` are shared, the LRU kernel is shared, the vocabularies are generated. The
remaining asymmetry is that `jmvtab` and `jmvtabreg` have separate `.a.yaml`s that will need the
same level-collapsing UI (Phase 20d) — build it once, in the generator.

---

## 8. Direct answers to the Phase 20 questions in CLAUDE.md

**20a — fields, column attributes, table attributes.** No merge is worth it (§7.4 re-measures z6's
verdict). Two real moves: (i) **un-export the setters** (§6) — the getters are the contract, the
setters are an invitation to break the invariant; (ii) `conf_level` being per-column is *already*
fully exploited (four thresholds read it, the reconcile carries it, `tab_stamp_inference()` stamps
it) — what is left is that **`degf` and `basis` deserve the same treatment `conf_level` got in the
legend**: `legend_method_name()` names the method and the level but not the df or the basis, so a
design-based table's legend does not say "referred to 42 design df". One sentence, one reader.

**20b — exported functions.** 52 of 93 exports appear in no vignette and no README. Concretely:
un-export the 6 `set_*` accessors of §6, `complete_partial_totals`, and `kable_tabxplor_style`
(defunct). Hard-deprecate the 6 legacy step functions (§6). That is 93 → ~79 with no user-visible
loss. `tab_shape()`/`tab_supports()`/`reg_measures()` are new and *should* be taught rather than
un-exported — they answer real user questions and currently appear nowhere.

**20c — arguments vs options.** This is KEY α and KEY ε in full. The concrete list:
`tab()` 52 → 39 by moving the 9 deprecated formals and the 4 dot-args into `...`; three producers
lose ~80 mirrored formals and ~1 300 Rd lines by `@inheritDotParams` + a generated `@param` block;
`tab_reg()` 29 → 27 by folding `stats`/`compare`/`baseline` into one `footer =`; six cross-producer
name collisions resolved. On options: 39 documented → ~34 by cutting `color_style_type` (documented,
never seeded, read only to deprecate), reconciling the **6 documented-but-unseeded** names with
`.onLoad` (`?tabxplor-options`'s own header promises they are in sync), making `jmv_full_hash`
internal, and — only if the 19m-iii ruling is revisited — renaming the five `kable_*` names with
silent aliases. **`option` becomes a declared
column of `TAB_ARGS`**, which is what makes "an argument that defaults to an option" one mechanism
instead of twelve hand-written formal defaults.

**20d — jamovi UI.** Out of scope for this assessment except for one structural note: the
level-collapsing UI wanted for both `jmvtab` and `jmvtabreg` is the same widget over the same
`tabxplor_lvl` model. Build it in `dev/generate_jamovi_js.R` and emit it into both `.u.yaml`s, or it
will be written twice and drift — which is the exact history of `detectFamily()`.
⚠ The **`.h.R` regeneration owed since 19k is still outstanding**; until `jmvtools::prepare()` runs,
`measure`, `shapes` and the renamed `test` read `NULL` in the running module.

**20e — the jamovi freeze on marginal effects.** **Root-caused: KEY ζ.** It is
`marginaleffects::get_jacobian` (85 % of a 15.3 s call), and tabxplor already owns the exact
analytic SE. Not a cache problem, not a jamovi problem.

**20f — `tab_reg()` parallelisation.** **Do KEY ζ first, then KEY γ, then measure again.** The
measured 15.3 s call becomes ~2 s from KEY ζ alone; parallelising a 15 s call whose 13 s is
avoidable would be optimising the wrong thing. After that, KEY γ gives the pool exactly one place to
attach (`reg_stage_fit()`), which is what the Phase 20f brief asks for ("pick the right level after
real measurement"). The shipping-cost hazard the brief names (~10 MB per raw fit) is unchanged and
is why the worker must return `reg_build_digest()`, never a fit.

---

## 9. Anti-propositions — what not to do

Carried forward from Phase 19 (all still binding), plus three new ones:

- **Do not route regression columns through the aggregate core**; do not go sparse on the record;
  do not merge fmt fields; do not replace the S3-per-verb model; do not re-open the settled perf
  verdicts; do not delete `tab_ci()`/`tab_chi2()` **as computations** (they are already superseded
  wrappers — §6 proposes deprecating the *exported step API*, which is a different thing).
- **NEW — do not add a fact table without a foreign-key check.** KEY β exists because the last two
  tables added (19d's rename, 19e's estimands) each broke a cross-table key.
- **NEW — do not export an accessor because a fact became stored.** Storing is internal; exporting is
  a user contract. Ask "what user story reads this?" before `@export`.
- **NEW — do not count lines as the simplification metric.** Phase 19 grew 11.9 % and got
  substantially better. The metrics that tracked reality here were: *aborts vs informs*,
  *formals per producer*, *duplicated `@param` blocks*, *functions with zero external callers*, and
  *cross-table keys unchecked*. Use those.

---

## 10. Suggested sequencing

Each item is plan-then-implement in its own session, with the characterisation harness named.

| # | item | key | harness | risk |
|---|---|---|---|---|
| 1 | foreign-key checks between the fact tables | β | none needed (load-time) | none |
| 2 | delete `auto_or`/`"or_table"`, `color_style_type`, `kable_tabxplor_style`; un-export the 6 setters + `complete_partial_totals`; fix the vignette `display` claim | §6 | `verify_color_attrs.R` | none |
| 3 | **KEY ζ** — analytic AME SE, declared per estimand | ζ | `verify_reg_specs.R` + a tolerance fixture | **medium** (changes printed SEs in the last decimals) |
| 4 | **KEY α** — `TAB_ARGS`, generated `@param`, `...` on the three superseded producers, 9 deprecated formals out of `tab()` | α | a new `dev/verify_tab_args.R` (dump resolved settings over a call grid) | medium |
| 5 | **KEY γ** — `new_reg_ctx()` + five named stages | γ | `verify_reg_specs.R` must print IDENTICAL | medium |
| 6 | **KEY ε** — `tab_vars` on both producers, `ci_method` on both, `footer =`, aligned defaults | ε | both harnesses | **user-visible** — needs the §11 rulings |
| 7 | **KEY δ** — `TEST_ROWS` | δ | golden delta (the `test` attribute) | low |
| 8 | hard-deprecate `R/tab-steps-legacy.R`'s six step functions | §6 | `test-steps-legacy.R` becomes a deprecation test | user-visible |
| 9 | **KEY η** — `?tabxplor-model`, one reader convention, the archaeology sweep | η | none | none |
| 10 | Phase 20f, re-measured after 3 and 5 | — | `test-benchmark.R` + a new reg op | — |

Items 1–2 are a single short session. Items 3 and 4 are independent of each other and of 5.

---

## 11. Open questions — these need a maintainer ruling before the work starts

1. **`tab()`'s 9 deprecated formals → `...`.** It breaks positional calls past argument 5. Do it in
   2.0.0 (with an abort on an unnamed 6th argument), or keep the signature at 52?
  **Maintainer’s decision: do it.**
2. **The legacy step API** (`tab_pct`/`tab_tot`/`tab_totaltab`/`tab_ci`/`tab_chi2` as *chainable*
   functions). Hard-deprecate now and defunct in 2.1.0, or keep 1 433 lines indefinitely? Note the
   *computations* are shared and safe either way — the question is only about the exported chaining
   API.
   **Maintainer’s decision: hard-deprecate now.**
3. **`tab_reg(split_var =)` → `tab_vars =`.** Same word on both producers (recommended, permanent
   alias), or keep the modeller-facing word?
   **Maintainer’s decision: tab_vars.**
4. **`tab(color =)`'s default.** `tab_reg()` defaults to `TRUE`, `tab()` to `"no"`. Aligning them
   means colour by default on `tab()` — which is the package's whole pitch, and a released
   behaviour change. Align, or document the asymmetry as deliberate?
   **Maintainer’s decision: asymmetry is deliberate, do not document.**
5. **KEY ζ's tolerance.** The analytic and numerical SEs agree to ~1e-10. Is a change in the last
   printed decimal acceptable, or must the numeric path stay available behind an option?
   **Maintainer’s decision: acceptable.**
6. **Un-exporting the 6 setters.** They are new in 2.0.0 and unreleased, so this is free — but
   confirm that no jamovi module or personal script writes a column's `scale`/`pct_base`/`row_kind`.
   **Maintainer’s decision: setters seem important to export, specially row_kind.**
7. **The five `tab_kable_*` option renames were explicitly DROPPED in 19m-iii.** Phase 20c re-opens
   the options question in general, so: does that ruling stand, or is a silent-alias rename in scope
   now? (If it stands, delete the row from §6 rather than leaving it to be re-proposed a third time.)
   **Maintainer’s decision: that ruling stands.**
8. **Comment archaeology.** Is a phase tag ("Phase 18z5: …") worth keeping once the phase is two
   releases old, or should each release sweep them into git history?
   **Maintainer’s decision: we’ll radically rewrite comments but in a later phase.**

---

## 12. What this assessment is not

- Not a statistics review. As in Phase 19, **no soundness problem was found**. KEY ζ is explicitly
  *the same maths by a cheaper route*, and must be demonstrated as such.
- Not a jamovi UI plan (Phase 20d owns that), beyond the one structural note in §8.
- Not a re-litigation of Phase 19's settled rulings. Where this document disagrees with a phase
  summary it is on a *measurement* (the message diagnostic, §2.1; the boundary-file share), not on a
  decision.
