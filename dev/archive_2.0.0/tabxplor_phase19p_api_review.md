# Phase 19p — API review: the fields, the exports, the arguments and the options

**Written 2026-08-15, on commit `261d558` (Phase 19n) + the 19o assessment.** This is the review
asked for by CLAUDE.md § "Phase 19p": *given the new framework, where is there still room for real
simplification and integration in what the package **exposes*** — its per-cell fields and per-column
attributes, its 93 exported functions, and the arguments and global options of its user-facing
functions.

Everything numeric below was **measured on this tree** (or on the named git object). Where a claim is
an *estimate* rather than a measurement it says so.

**Companion documents** — read the one that matches what you touch:

| document | what it holds |
|---|---|
| `dev/tabxplor_phase19_assessment.md` | 19o: what Phase 19 achieved and cost, and the six keys that remain |
| `dev/ecosystem_keys_2.md` | the Phase 19 study: measurements, eight keys, defect ledger |
| `dev/tabxplor_phase19_ecosystem_integration.md` | the Phase 19 plan of plans |
| `dev/tabxplor_architecture.md` | the current architecture |

---

## 0. The verdict, in one page

19o's one-line finding was: *Phase 19 unified how facts are **stored** and how rules are
**declared**; it did not unify how the package is **asked**.* This review is the map of the ask, and
it lands on a sharper statement:

> **Every remaining duplication in tabxplor's public surface is the same shape: a fact is declared
> once in an R table, and re-typed by hand in the place a user meets it** — in a formal, in a
> `@param` block, in an option name, in an accessor. The package has already solved this problem
> four times (`fmt_fields_rd()`, `display_tokens_rd()` ×2, `reg_measures_rd()`). It has not applied
> the solution to the surface.

The measured size of the ask, today:

| surface | measured |
|---|---|
| formals | `tab()` **52** · `tab_counts()` **40** · `tab_plain()` **29** · `tab_reg()` **29** · `tab_num()` **28** |
| of which mirrors | **83 of the 149 crosstab formals** are the same argument written a 2nd/3rd/4th time |
| `man/` | **8 930** lines; `tab_reg.Rd` 722 (63 % arguments) · `tab.Rd` 695 (58 %) · `fmt.Rd` 693 · **`tab_many.Rd` 448** |
| exports | **93** (released baseline `v1.2.0` = 59; **24 new in 2.0.0**); 52 appear in no vignette and no README |
| S3 methods | 135 |
| options | **35** documented, **34** seeded, exactly **1** documented-but-never-seeded |
| accessors | ~23 exported functions over 16 declared column attributes, with **6 asymmetries and 1 misnaming** |

**Four keys** are proposed (lettered so they do not collide with 19o's α–η):

| key | the missing fact / unstated rule | § |
|---|---|---|
| **A** | *which accessors exist* — the exported get/set family is the last hand-written mirror of the 16 declared column attributes | §2.3 |
| **B** | *what kind of question an argument is* — per-call, per-session, or internal. One bundle constructor + one option per tier-2 cluster | §4.1–§4.2 |
| **C** | *where an argument's legal values are written down* — ~15 arguments have theirs in an R fact table and re-typed by hand in roxygen | §4.3 |
| **D** | *which producers are entry points and which are variants* — the variants stop re-declaring 83 formals and take `...` | §4.4 |

Their combined effect, **estimated per file and summed**, never rounded up:

**`man/` 8 930 → ~7 300 (−18 %) · exports 93 → 89 · `tab()` 52 → 34 named formals + `...` ·
`tab_reg()` 29 → 23 named + `...` · documented options 35 → ~31 · 83 mirrored formals → ~10.**

The −18 % breaks down as: `tab_many.Rd` −388 · `tab_logit.Rd` + `multi_logit.Rd` −523 ·
`tab.Rd` −205 · `tab_reg.Rd` −170 · the three superseded producers −365. The optional `tab_style()`
(§4.7) would add a further ~−190.

And the honest counter-weight, carried from 19o: **`R/` will grow again.** A bundle constructor and
its help page cost lines; a generated `@param` block costs a generator. The metrics that track
reality here are the ones 19o named — *formals per producer, duplicated `@param` blocks, exports with
zero external callers, cross-table keys unchecked* — not the line count.

### Three corrections to the 19o assessment

Reported here so the ledger stops carrying them.

1. ⚠ **`@inheritDotParams` does not shrink documentation — it INLINES the parent's `@param` blocks.**
   19o's KEY α predicted `tab_many.Rd` would fall 448 → ~60 by giving the function `...`. It
   *already has* `...` (11 named formals beside it) **and** `@inheritDotParams tab`
   (`R/tab-deprecate.R:222`), and it is **448 lines**, 83 % of them the inlined parent arguments. The
   mechanism that actually shrinks is a plain `@param ... Passed to [tab()].` plus a dots validator
   (§4.4).
2. **The option census was over-counted.** 19o reported "46 names in `R/`, 33 seeded, 39 documented,
   6 documented-but-never-seeded". Counting distinct *settings* (excluding the `tabxplor.` /
   `tabxplor.options` prose artifacts and the three documented read-aliases `console_theme`,
   `export_theme`, `kable_css`): **35 documented, 34 seeded, and exactly ONE documented-but-never-
   seeded — `tabxplor.color_style_type`**, which is deprecated and read only to emit its own warning.
   The situation is much better than reported.
3. **`tabxplor.totcol_range` is neither seeded nor read.** 19o called it "seeded, 1 read, never
   documented". The `.onLoad()` line is commented out (`R/utils.R:137`) and the read site is
   commented out (`R/tab-display.R:639`). It is fully dormant, exactly as the maintainer ruled, and
   needs no action at all.

---

## 1. How this was measured

```bash
# formals, per producer
awk '/^tab <- function\(/,/^\) *\{/' R/tab.R

# per-argument documentation weight: brace-matched \item{} inside \arguments{} of each .Rd
python3 - <<'EOF'  # (the script is in §7's harness list)

# corpus frequency of each argument name
grep -roE "[(,] *<arg> *=" tests/ vignettes/ README.Rmd | wc -l

# export usage, with the released baseline
git show v1.2.0:NAMESPACE | grep '^export('        # 59 released names
grep -roE "(^|[^a-zA-Z0-9._])<fn>\(" R/ tests/ vignettes/

# options: documented vs seeded (comment lines excluded — this is what 19o missed)
grep -o 'tabxplor\.[a-z_0-9]*' R/tabxplor-options.R | sort -u
sed -n '86,276p' R/utils.R | grep -v '^\s*#' | grep -oE 'options\("tabxplor\.[a-z_0-9]*"'
```

⚠ **Run every census under `LC_ALL=C`.** The box is `fr_FR.UTF-8`, whose collation does not group
identifiers containing `_`/`.`, so `sort | uniq` and `comm` silently under-count. That is the same
trap 19l recorded, and it bit twice while writing this document.

⚠ **`grep -w` does not work on a pattern ending in `(`** (`-w` requires a word boundary on both ends
and `(` is not a word character). A first pass using it reported nine live exports as having zero
callers. Every count in this document was re-taken with `(^|[^a-zA-Z0-9._])name\(`.

---

## 2. Part A — the vctrs fields, the column attributes, the table attributes

### 2.1 The model as it stands

| carrier | count | contents | why it lives there |
|---|---|---|---|
| **per cell** (`vctrs::field`) | 21 | `n display digits wn pct mean diff ratio ctr var ci_inf ci_sup pvalue or tot_n n_eff obs gap_se row_kind in_tottab in_refrow` | varies within a column; user-contract (`$` / `mutate()`) |
| **per column** (`attr`) | 16 | `scale comp_all ref pct_base col_var col_group totcol refcol color color_signif model_family role conf_level degf basis ci_method` | scalar over a column, and **must survive extraction**: a lone `tabxplor_fmt` column has to `format()` and colour on its own |
| **per table** (`attr`) | 3 | `subtext` · `test` (a 15-column tibble) · `meta` (5 sub-fields: `spec` `render_extras` `empirical_tips` `assumptions` `color_breaks`) | facts no column can carry; all optional and NULL-safe |
| **per index column** (`tabxplor_lvl`) | 3 | `role` `var` `ordered` | what a *row* is (19f) |

Adding an attribute is two lines (`new_fmt()` formal + one `fmt_attr_rules` row) and a build-time
`stopifnot()` enforces it. Adding a `meta` sub-field is one getter. That is the machinery Phase 19
built, and it works.

### 2.2 What must NOT change — settled, restated so it stops being re-opened

- **No sparse record.** z6 measured the payoff at 0.03 %. Typical occupancy is 6–9 of 21 fields, and
  that is *by design*: `n_eff` is empty unless a design effect or a `survey` design is in force, and
  `gap_se` unless a `tab_reg()` gap test is. Those two fields are differentiators, not waste.
  (`dev/empty_vctrs_fields_sparse_record.md`.)
- **No field merge.** All 21 are user-contract.
- **`row_kind`, `in_tottab` and `in_refrow` stay FIELDS**, not `tabxplor_lvl` attributes. They are
  three *orthogonal* facts (which kind of row · which sub-table · is it the baseline), and
  `fmt_color_plan()` asks them of a lone extracted column with no table in scope. Correctly placed.
- **The four inference attributes stay four scalars, not one list.** Their merge rules genuinely
  differ (`same` / `min` / `weakest` / `same`), and 19a's parallel index vectors — computed once at
  build time from `fmt_attr_rules` — are what made `vec_ptype2` **234 µs → 125 µs**. Folding them
  into a list attribute would put a `switch` back on the hottest reconstructor.

### 2.3 KEY A — the accessor surface is the last hand-written mirror of `fmt_col_attrs`

`fmt_attr_rules` declares 16 attributes exhaustively, by build-time assertion. The **accessors** are
~23 hand-written exported functions written beside it, and they are neither exhaustive nor
consistent. Measured:

| attribute | exported getter | exported setter |
|---|---|---|
| `scale` `pct_base` `col_var` `comp_all` `model_family` `color` `color_signif` | ✓ | ✓ |
| `totcol` `refcol` | `is_totcol` `is_refcol` ✓ | `as_totcol` `as_refcol` ✓ |
| `ref` | `get_ref_type` ✓ | **`set_diff_type`** ✓ — *the pair does not share a stem* |
| `col_group` | `get_col_group` ✓ | ✗ |
| `ci_method` | `get_ci_method` ✓ | ✗ |
| `conf_level` · `degf` · `basis` | ✗ ✗ ✗ | ✗ ✗ ✗ |
| `role` | ✗ | ✗ |
| *(field)* `display` | ✗ — read with `x$display` | `set_display` ✓ |

So **one** of the four "how was this column's interval computed" attributes has an exported getter
and three do not, although the entire point of z13/z16-iiiii storing them per column was that the
answer varies per column. And `set_diff_type` writes the attribute `get_ref_type` reads.

The ruling of 19o §11 q6 is that setters **stay exported**, `row_kind` especially. So the proposal is
not to un-export — it is to **stop the family growing with the table**:

1. **One documented generic pair**, `fmt_attr(x, name)` / `` `fmt_attr<-`(x, name, value) ``,
   validated against `fmt_col_attrs`, dispatching on an fmt column *or* a data.frame (the existing
   `.data.frame` methods' idiom). It covers all 16 attributes and every future one with **zero new
   exports**, and it is what a programmer writing a generic helper actually wants.
2. **Rename `set_diff_type` → `set_ref_type`**, keeping the old name as a soft-deprecated alias — it
   is in `v1.2.0`, so this is etiquette-first. Measured usage: 2 internal callers, **0 tests, 0
   vignettes**.
3. **Add the three missing inference getters** (`get_conf_level`, `get_degf`, `get_basis`) — they
   have a real user story (§2.4) and they close the asymmetry with `get_ci_method`.
4. **State the admission test in the header**, beside the one that already governs new attributes:
   *storing a fact is internal; exporting its accessor is a user contract — name the user story
   first.* 19o measured that Phase 19 added 9 net exports, of which the six new `set_*` have **zero
   test callers and zero external users**; there was an admission test for a new attribute and none
   for exporting its accessor.
5. **New exported `tab_columns(x)`** — one row per fmt column × its attributes, the column-axis
   mirror of `tab_shape()`. This is the *inspection* user story that ~12 individual getters are being
   used for today, and it is the natural home for "which of my columns is on a design basis, at
   which level, by which method". It also gives `?fmt` (693 lines, 38 aliases) something to point at
   instead of asking a user to compose a dozen getters.

**Caveat, honestly.** A generic `fmt_attr()` beside 23 named accessors is *two ways to say one
thing*, which is the pattern hard rule 1 forbids. The distinction that makes it legitimate: the named
accessors are the **taught** surface (a user reads `get_scale(x)`), the generic is the
**programmatic** one (a helper loops over `fmt_col_attrs`). That distinction has to be stated in the
header, or it will be read as duplication. If the maintainer would rather not have both, the honest
alternative is the generic alone plus the ~8 named accessors that appear in a vignette.

### 2.4 `conf_level` per column — what it unlocked, and what is left

The quartet `conf_level` / `degf` / `basis` / `ci_method` is stamped by ONE sweep
(`tab_stamp_inference()`), reconciled by the weakest-claim rule on every bind, and read by the
engine's four thresholds. That is complete and it works. Three residual items, all small:

1. **The legend names the level and the method but not the df or the basis.** A design-based table's
   footer says *"95 % Newcombe interval"* and not *"…referred to 42 design degrees of freedom"* —
   although `degf` is exactly what makes that interval differ from the flat one, and `basis` is what
   the whole z16 subsystem exists to be honest about. One sentence, one reader
   (`legend_method_name()` / `tab_footer_streams()`). **This is the last stored fact of the Phase 19
   model that is not surfaced anywhere.**
2. **The getters are invisible** (§2.3 item 3).
3. **`conf_level = conf_level_default()` is a hand-written formal default in ~10 signatures**, and it
   is the *pattern* that repeats, not the value: `stars`, `cleannames`, `ci_method`,
   `design_effect`, `anova`, `theme`, `lang`, `var_names` all do the same thing by hand, each in its
   own idiom (`NULL` + a `getOption()` inside the body, or a `*_default()` call in the formal, or a
   `%||%`). One declared "this argument's option is X" — a column of the argument table 19o's KEY α
   proposes — would make it one mechanism. That is a *documentation-table* item, not a `conf_level`
   item; recorded here because this is where it is felt.

No structural change is proposed: the answer to *"is there room to simplify around `conf_level` now
that it is per-column?"* is **the storage is done; what is missing is the reading.**

### 2.5 The `var` field's declared overload

`var` carries the variance of a mean, the Chi-2 variance on a percentage, **and** `var(Y)` on a
regression column — three meanings disambiguated by the column's `scale`. This is the `type`/`ci_type`
disease 19b cured for *attributes*, surviving in a *field*.

**Verdict: keep.** The field is user-contract (`$var`), `scale` genuinely disambiguates, and 19b's own
admission test applies — there is no reader that would be simplified by splitting it. What should
change is one line of documentation: `FMT_FIELD_DOC$var` currently *enumerates the cases*
("the variance (of a mean; the Chi-2 variance on a percentage)"), which is the shape of a comment
that drifts. State the **rule** instead: *"the column's variance quantity — which one is given by its
`scale`"*, and let `?fmt`'s generated roll-call carry it.

### 2.6 `color_breaks` has three carriers, and one of them has never been used

`options(tabxplor.color_breaks)` (session) · `tab(color_breaks =)` (per call) · `meta$color_breaks`
(per table, carried through the pipeline, pushed/popped at render by `push_color_breaks()`).

That is the right architecture — but **`color_breaks =` has 0 uses in the entire corpus** (tests,
vignettes, README). Verdict: **keep** (it is the only route to the per-table `meta` slot, which the
render path depends on), and **demote it in `?tab`** to one line plus a link to the Colours page.
Its four Rd lines are not the problem; its presence in a 52-formal signature is.

### 2.7 The `test` tibble — 19o's KEY δ, unchanged

15 columns (`var col test statistic df1 df2 pvalue n min_e effect_size es_type pvalue_exact deff dep
col_group`) plus dynamically-added grouping columns, carrying ≥20 kinds of row under one `test`
discriminator, of which only the regression half has a declaring table (`reg_footer_spec()`).

Restated here **as flagged, not re-designed**, so the two documents do not fork: `TEST_ROWS` is 19o's
KEY δ and it stays there. The one addition this review makes is that §4.6's `footer =` argument on
`tab_reg()` would be `TEST_ROWS`' first consumer — which is the usual signal that a fact table is
worth building.

---

## 3. Part B — the exported surface

### 3.1 Census

**93 exports** (135 S3 methods, which pkgdown drops). Released baseline `v1.2.0` = **59**; since then
**40 added and 6 removed** (`%>%`, `get/set_type`, `get/set_ci_type`, `tab_xl_confidential`). Of the
40, **24 are new in the 2.0.0 line itself** (absent from the post-1.3.1 dev head, so never released):

```
conf_level_to_z forest_plot get_caption get_ci_method get_col_group get_model_family
get_pct_base get_row_kind get_scale gss_cat_data_formatting is_lvl jmvtabreg new_lvl
reg_check_plots reg_measures set_caption set_model_family set_pct_base set_row_kind
set_scale tab_css tab_html tab_shape tab_supports
```

and 7 were removed (`%>%`, `get/set_type`, `get/set_ci_type`, `lm_plots`, `or_plot`).

⚠ **The release status of each candidate decides delete-vs-deprecate**, and it must be checked
(`git show v1.2.0:NAMESPACE`) rather than assumed. Two verifications that matter:
`complete_partial_totals`, `fct_recode_helper`, `tab_get_wrapped_dimensions`, `set_diff_type`,
`tab_prepare` and the five step functions **are all in `v1.2.0`** — 19o proposed removing several of
them outright. `tab_reg` and its family are **not** released (the maintainer's standing ruling: no
back-compatibility needed there at all).

### 3.2 Delete — unreleased, free

| item | evidence | replacement |
|---|---|---|
| **`tab_logit()` + `multi_logit()`** *(ruled)* | 523 Rd lines (265 + 258). **0 uses in any vignette.** Each mirrors ~20 of `tab_reg()`'s 29 formals — and *only* ~20, so a user who found `tab_logit()` **cannot reach** `effect = "marginal"`, `measure = "ratio"`, `compare =`, `baseline =`, `reference =` or `color =`. A capability hole created purely by the mirror | `tab_reg(family = "binomial")` — one argument, and what both vignettes already teach. `multi_logit(models =)` → `tab_reg(predictors = list(...), family = "binomial")` |
| **`kable_tabxplor_style()`** | a defunct stub since 19l, already `@keywords internal` and already out of the pkgdown index (19n) | none — `tab_html()` |

Cost: `NEWS.md` under *Removed*, and one line in each reg vignette. Nothing else references them.

### 3.3 Demote to `@keywords internal` — exported, but out of the index

- **`tab_get_wrapped_dimensions()`** — 0 callers anywhere, tests included. The maintainer ruled *keep*
  (personal tooling), so: keep the export, add `@keywords internal` so it stops occupying a line of
  the reference index. It sits in "Data-prep and text helpers" today, next to two functions users
  actually call.
- **`new_lvl()` / `is_lvl()`** — already `@keywords internal`; the open question is whether they
  should be exported at all (§8).

### 3.4 Soft-deprecate — released in `v1.2.0`, etiquette first

- **The five legacy step functions** `tab_pct` `tab_tot` `tab_totaltab` `tab_ci` `tab_chi2` —
  **ruled in 19o §11 q2**. Restated with its measured consequence: `R/tab-steps-legacy.R` is **1 433
  lines (3.3 % of `R/`) with ZERO callers in `R/`** (verified: 0 for each of the five outside the
  quarantine file itself), and it carries a *second implementation of the plan* — an 8-branch
  `case_when`, a second `ci = "ratio"` fold, a third `stars` resolution, a `degf`-from-the-columns
  fallback and four table-**mutating** passes — which must track the leaf's arithmetic forever. The
  arithmetic is already shared (`ci_dispatch()` / `chi2_compute_test()`), so nothing is lost but the
  chaining API. ⚠ This does **not** contradict Phase 19's anti-proposition ("do not delete
  `tab_ci()`/`tab_chi2()` as *computations*") — the computations moved into the leaf in 19j; what is
  proposed is deprecating the exported *chaining API*, a different object.
- **`complete_partial_totals()`** — exported, **1 internal caller** (`tab.R:2852`, inside
  `tab_spread()`), **0 tests, 0 vignettes**. 19a reversed its "cut" verdict because a caller was
  found; the caller is tabxplor's own. Deprecate, keep internal.
- **`fct_recode_helper()`** — **0 callers in `R/`**, 7 in tests, 0 in vignettes. A forcats
  convenience that is not tabxplor's job.
- **`tab_md_css()`** — ≡ `tab_css(chrome = FALSE)`; 2 R callers, 11 tests, 0 vignettes. 19h already
  removed its one documented-as-ignored argument. One function, one job.

### 3.5 What should be *taught*, not cut

**52 of 93 exports appear in no vignette and no README.** Three of them are new, answer a real
question, and were built precisely so the answer would exist:

- **`tab_shape()` / `tab_supports()`** — "what have I got, and what can I do with it?" (19h);
- **`reg_measures(data, dependent)`** — "what can this outcome be modelled as?" (19e);
- plus the proposed **`tab_columns()`** and **`fmt_attr()`** (§2.3).

One short *"Inspect a table"* section in the programming vignette covers all five and costs ~30
lines. Un-exporting them would be the wrong reading of "52 of 93 are untaught".

### 3.6 The exporter family — the count is fine, the mirror is not

`tab_kable` is a bare alias of `tab_html` (`tab_classes.R:922`); `tab_export(format=)` is a facade
over five exported backends. All are released, all keep working: badge `tab_kable` superseded,
deprecate `tab_md_css`, keep the rest. **The real problem is not that there are six functions — it is
that they share 7 arguments and declare them 5 times** (§4.7).

---

## 4. Part C — the arguments and the global options

### 4.1 KEY B — THE RULE: three tiers

The direct answer to *"what should stay an argument, and what would be more user-friendly as a global
option?"*

> 1. **Per-call argument** — a question answered differently from one table to the next. A flat
>    formal. No option twin unless one exists today.
>    `pct` `color` `ref` `ci` `test` `stars` `conf_level` `digits` `na` `tot` `display` `wt`
>    `row_vars` `col_vars` `tab_vars` `levels` `comp` `add_n` `subtext` …
> 2. **Per-session setting** — a question answered once for a script or a document.
>    **One bundle constructor + one option, overridable per call by passing the bundle.**
>    `ci_method` `design_effect` `anova` `method` · the label words · the export style.
> 3. **Internal knob** — neither an option nor documented.
>    `jmv_full_hash` `parallel_min` · `.cache` `.defer_level_merge` `.return_armed` `.levels_order`
>    `.fit_cache` `.fine` `.by_table`.

The **precedent is the package's own**: `new_inference()` (`survey-design.R:174`), the internal
`color_spec` (`normalize_color_spec()`), `resolve_export_opts()` (`tab-export-prep.R:851`) and
`new_reg_args()` already build exactly these objects **internally**, at the argument boundary. The
proposal is only to let a user hand one in — and to make the *option* be that object, so "an argument
that defaults to an option" becomes one mechanism instead of a dozen hand-written formal defaults.

**The tier-1/tier-2 split is empirical, not aesthetic.** Corpus frequency (tests + vignettes +
README, `arg =` occurrences) separates them cleanly:

```
tier 1: pct 1129 · color 569 · ci 251 · na 201 · wt 182 · test 174 · ref 172 · display 154
        color_signif 95 · digits 64 · stars 63 · comp 51 · levels 44 · add_n 40 · conf_level 31
tier 2: ci_method 19 · anova 7 · total_names 6 · design_effect 4 · totaltab_name 3 · other_level 2
tier 3: names_prefix 0 · names_sort 0 · color_breaks 0 · inverse_two_level_factors 0
```

⚠ **The anti-proposition that governs this key: never introduce a bundle whose fields are ALSO flat
formals.** Two ways to say one thing is the ad-hoc layer hard rule 1 forbids. That is why
`conf_level` (31 uses), `ci` (251), `test` (174) and `stars` (63) stay **flat and out of the
bundle** — moving them would trade a real ergonomic loss for a cosmetic count, and keeping them in
both places would be worse than either.

### 4.2 `tab_inference()` — the tier-2 constructor

```r
tab_inference(ci_method = NULL, design_effect = NULL, anova = NULL, model = NULL)
```

| field | today | uses |
|---|---|---|
| `ci_method` | `tab()`/`tab_num()`/`tab_plain()`/`tab_counts()` formal, a named vector over `CI_METHODS`' 4 slots | 19 |
| `design_effect` | `tab()`/`tab_num()`/`tab_plain()` formal + `options(tabxplor.design_effect)` | 4 |
| `anova` | `tab()`/`tab_num()` formal + `options(tabxplor.anova)` | 7 |
| `model` | **`tab_reg(method = c("wald","profile"))`** — the same question in a different word | 13 |

Folding `tab_reg(method=)` in as the `model` slot of `ci_method` is 19o's **KEY ε item 2 delivered
structurally rather than by renaming**: one argument, one grammar, `CI_METHODS` gains a `model` slot
and its declared default.

⚠ **`conf_level` is deliberately NOT a field.** It stays a flat tier-1 argument on both producers
with `options(tabxplor.conf_level)` as its own option twin — putting it in the bundle *as well* would
be exactly the two-ways-to-say-one-thing this key's anti-proposition forbids, and putting it in the
bundle *instead* would make the most recognised statistical argument in R unreachable without a
constructor. So `tab_inference()` holds four fields and no more.

Effect: `tab()` **−3** formals, `tab_num()`/`tab_plain()` **−3** each, `tab_counts()` **−1**,
`tab_reg()` **−1**; `?tab` **−32** Rd lines; `?tabxplor-options` **−2** entries, **+1** for the
bundle; and **one inference story, on one page, shared by both producers** — where today `?tab` spends
98 of its 362 argument lines (27 %) on it and `?tab_reg` re-opens the subject in its own words.

**Caveat.** A constructor is a concept to teach. It earns its place here because (i) its fields are
genuinely per-session, (ii) it is the only way to give the two producers *one* spelling for
`ci_method`/`method` without renaming a released argument, and (iii) it replaces two options with
one. If it did only the first of those, it would not be worth it.

### 4.3 KEY C — every declared vocabulary documents itself

The package has **four** `@eval` documentation generators, and they exist because the hand-written
copies had drifted every time:

`fmt_fields_rd()` (?fmt hand-listed 11 of 22 fields and still named `in_totrow`, deleted in 19f) ·
`display_tokens_rd()` ×2 (?tab hand-copied a vector from a file 1 400 lines away) ·
`reg_measures_rd()`.

Measured, **~15 more arguments have their value list declared in an R fact table and re-typed by hand
in roxygen**:

| argument | declared in |
|---|---|
| `color` | `MEASURES` + `COLOR_ALIASES` |
| `color_signif` | `COLOR_SIGNIF_VALUES` |
| `ci_method` | `CI_METHODS` + `CI_METHOD_LABELS` |
| `pct` `na` `levels` `tot` `totaltab` `comp` `totcol` | `TAB_ARG_VALUES` |
| `measure` `effect` `family` | `REG_ESTIMANDS` / `REG_FAMILIES` |
| `stats` / `check` | `REG_GOF_KEYS` + `REG_CHECKS` |
| `shape` | `REG_SHAPES` |
| `color_breaks` scale names | `COLOR_SCALES` |
| `theme` | `tx_resolve_theme()` |

**This is 19o's KEY β one level up.** 19d's rename of the colour measures to full words broke
`EST_SCALES$label_meas` in *code* (the forest plot errored on lookup) — and it also left the value
list in `?tab` describing a spelling that no longer existed, which nothing caught because nothing
checks documentation against a table.

The single biggest instance: **`color_measures_rd(producer = c("tab", "reg"))`**. `?tab` spends 69
argument lines on colour and `?tab_reg` **101** — and the `?tab_reg` block is not a copy, it is the
*two reg-only measures* (`adjustment`, `between_groups`) described from scratch, which is worse: the
descriptions of `difference`/`ratio`/`odds_ratio`/`contrib`/`adjustment`/`between_groups` exist once
in `MEASURES` (`word`, `subject`, `caveat`, `channels`, `requires`, `auto_for`) and are prose-written
zero, one or two more times depending on the page. One generator, filtered by
`MEASURES[[m]]$producers`, and each measure is described exactly once, in the file that declares it.

Estimated effect: **−80 Rd lines on `?tab`, −90 on `?tab_reg`**, and the drift class becomes
unrepresentable — which is the part that matters.

### 4.4 KEY D — `...` on the superseded producers, and the `@inheritDotParams` trap

`tab_counts()` shares **34** of its 40 formals with `tab()`; `tab_plain()` **25** of 29; `tab_num()`
**24** of 28. Each mirror carries its own `@param` block: `@param color` is written **15 times**
across `R/`, `@param theme` 12, `@param conf_level` 8, `@param stars` 8, `@param na` 7,
`@param display` 7, `@param comp` 7, `@param color_signif` 7.

19i gave the four producers **one resolver** (`tab_resolve_common_args()`). It did not give them one
**declaration**. Proposal:

```r
tab_counts(data, counts, cols, col_name, base, input, wt_counts, ...)
tab_plain (data, row_var, col_var, tab_vars, wt, num, df, ...)
tab_num   (data, row_var, col_vars, tab_vars, wt, num, df, ...)
```

with `...` forwarded to the shared resolver and validated by a **`tab_check_dots()`** matching every
name against `TAB_ARG_VALUES` + `tab()`'s formals, aborting with a "did you mean". That validator is
what makes this a net gain rather than a loss: today a typo produces R's bare *"unused argument"*;
afterwards it produces a suggestion.

⚠ **Document `...` with a plain `@param ... Passed to [tab()]; see there.` — never
`@inheritDotParams`.** The measured proof is `tab_many()`, which already has 11 formals + `...` +
`@inheritDotParams tab` and whose `.Rd` is **448 lines** — 83 % of it the inlined parent arguments.
Changing that one tag is a **−390 Rd line, zero-risk, one-line change**, and it is item 1 of §7.

Estimated from each file's measured `\arguments{}` block (206 · 150 · 43 · 373 lines):
`tab_plain.Rd` 279 → ~80 · `tab_num.Rd` 208 → ~70 · `tab_counts.Rd` 137 → ~110 (its `@param`s are
already terse, so the win there is small) · `tab_many.Rd` 448 → ~60.
**~−750 Rd lines, and 83 mirrored formals → ~10.**

⚠ **`...` costs IDE completion.** That is precisely why `tab()` and `tab_reg()` keep every live
formal: completion on the two taught entry points is worth more than symmetry. Only the *superseded*
producers and the *wrappers* take `...`.

### 4.5 `tab()` — the verdict

The 52 formals break down as: population 11 · cell content 6 · comparison 6 · inference 7 · totals 5
· output 4 · **deprecated 9** · **internal dot-args 4**.

| move | formals | Rd |
|---|---|---|
| the **9 deprecated** formals → `...`, caught by name (5 already have translating shims: `tab_deprecate_or/_sup_cols/_many`) | −9 | −23 |
| the **4 dot-args** → `...` (`.cache` `.defer_level_merge` `.return_armed` `.levels_order`) | −4 | −9 |
| `ci_method` + `design_effect` + `anova` → `tab_inference()` (§4.2) | −3 | −32 |
| `totaltab_name` + `other_level` → the `total_names` named vector + its new option (§4.8) | −2 | −2 |
| value lists generated from the fact tables (§4.3) | 0 | ~−80 |
| colour prose → one shared page; `@param` one sentence + link | 0 | ~−60 |
| **`tab()` 52 → 34 named + `...` · `tab.Rd` 695 → ~490** | **−18** | **~−205** |

⚠ **Three cautions, all verified in source, all of which would silently break something:**

1. **Positional calls past argument 5 break.** `sup_cols` is currently the 6th formal. `tab()` must
   **abort on an unnamed 6th argument** — the discipline `tab_many()`'s shim already uses, and for
   the same reason.
2. **`names_prefix` / `names_sort` are badged deprecated but still LIVE.** They are forwarded to the
   spread path at `tab.R:615, 832, 1270` — 0 corpus uses, but a real code path. They must either be
   forwarded out of `...` or moved onto `tab_spread()` (which already has them), and that is Phase
   19's own open question #4, which 19h left unsettled.
3. **`method_cell` / `method_diff` are read with `missing()`** (`tab-resolve.R:637-638`), which does
   not work through `...`. Convert them to `NULL` defaults *before* moving them.

### 4.6 `tab_reg()` — the verdict

Unreleased, so every change here is a rename, not a deprecation.

- **`reference` → `ref`** *(ruled: partial merge)*, taking the same `c(var = "level")` grammar
  `tab(ref =)` already accepts as a per-`row_var` named vector — **and absorbing
  `inverse_two_level_factors`** as an entry for the *outcome*:

  ```r
  tab_reg(d, "married", c("race", "educ"),
          ref = c(race = "White", married = "Not married"))
  #                              ^ which level is the NON-event
  ```

  `inverse_two_level_factors` is a **25-character logical with 0 corpus uses** that encodes which
  level of a binary outcome is modelled, by toggling level *order*. Naming the level is strictly
  better: it is what the user knows, it is checkable, and it is one grammar with the predictors'
  baselines beside it. `tab(ref/ref2)` stay two arguments *(ruled)* — their per-axis defaults differ
  (`"auto"` vs `"first"`) and 19d made the odds ratio unconditional, so `ref2` is always in force.
- **`split_var` → `tab_vars`** *(ruled in 19o §11 q3)*. Since 19f, `tab_reg()` already **stamps**
  `split_var` as a `tab_var` role on the index column — the storage was unified two phases ago and
  only the argument was not.
- **`method` → the `tab_inference()` bundle** (§4.2).
- **`stats` + `compare` + `baseline` → one `footer =`** taking the `REG_GOF_KEYS` vocabulary
  (19o KEY ε item 4). Three arguments for one concept — *what rides the model-summary footer* — and
  the first real consumer of 19o's KEY δ (`TEST_ROWS`).
- **`.fit_cache` → `...`** (tier 3; currently a documented formal with 4 Rd lines).
- **`family` / `effect` / `measure` `@param` prose trimmed against `reg_measures_rd()`**, which
  already generates the estimand section and currently duplicates part of it (41 + 41 + 19 = 101 Rd
  lines beside a generated section that states the same table).

**`tab_reg()` 29 → 23 named formals + `...` · `tab_reg.Rd` 722 → ~550.**
(`method` −1 · `stats`/`compare`/`baseline` → `footer` −2 · `.fit_cache` −1 ·
`inverse_two_level_factors` −1 · `reference` → `ref` is a rename, ±0.)

### 4.7 The exporters' mirror — a second, optional tier-2 bundle

`theme` · `color` · `color_legend` · `lang` · `transpose` · `caption` · `var_names` = **7 arguments ×
5 functions** (`tab_html`, `tab_md`, `tab_xl`, `tab_plot`, `tab_export`) = **28 mirrored formals**,
plus `wrap_rows`/`wrap_cols`/`whitespace_only` on three, plus 9 more on `tab_xl` alone (21 formals:
`colnames_rotation`, `colwidth`, `sheets`, `titles`, three fonts, three text sizes, `or_numeric`).
One resolver already exists — `resolve_export_opts()`.

```r
tab_style(var_names = NULL, var_labels = NULL, lang = NULL, test_lines = NULL,
          legend_style = NULL, tooltips = NULL, popover = NULL, css = NULL,
          fonts = NULL, wrap_rows = NULL, wrap_cols = NULL, ...)
```

with `theme`, `color`, `caption`, `transpose`, `path` staying flat, and `options(tabxplor.style)` as
the session default. Estimated: `tab_xl` 21 → ~8 · `tab_md` 17 → ~9 · `tab_html` 14 → ~8 ·
`tab_plot` 9 → ~6.

**This is presented as a recommendation, not as settled work.** Two honest reasons for caution: the
maintainer declined the neighbouring option folds in this session (`kable_popover` → `tooltips`,
`legend_style` → `color_legend`), which is a signal to be conservative on the render side; and all
five exporters are **released**, so every moved formal needs a `...`-caught alias. It is the largest
single collapse left in the package and also the one with the least statistical consequence — which
makes it a good candidate for *after* the release rather than before. See §8.

### 4.8 The options — the verdict

**35 documented · 34 seeded · 1 documented-but-never-seeded.** (See §0's correction 2.) Measured by
actual `getOption()` / `tx_getOption()` call sites — not by name occurrences, which is what inflated
19o's figures — **13 of the 33 seeded options are read at exactly one site**, and the busiest
(`tabxplor.color_breaks`) at five.

**Approved in this session:**

- **`tabxplor.stars` absorbs `signif_levels` + `signif_labels`.**
  `FALSE` / `TRUE` / `c("*" = 0.10, "**" = 0.05, "***" = 0.01)` — and **as an argument as well**.
  Today the star ladder is option-only, so one table in a document cannot use a different ladder
  from the next, although `stars =` is a per-call argument on four producers. **−2 options**, and a
  capability that did not exist.
- **`options(tabxplor.total_names = c(row =, col =, tab =, other =))`.** The three label defaults are
  hard-coded literals in five producer signatures with **no option twin at all**, and they are not
  even in one language: `total_names = "Total"`, `totaltab_name = "Ensemble"`, `other_level =
  "Others"`. For a French-authored package with a French audience that is a real gap, and it is the
  reason `totaltab_name` (3 uses) and `other_level` (2 uses) exist as formals nobody sets.
  **+1 option, −3 hard-coded defaults, −2 formals**, and `total_names` becomes one named vector.

**Also proposed (uncontroversial):**

- **Delete `tabxplor.color_style_type`** — documented, **never seeded**, read once and only to emit
  its own deprecation warning. The one documented-but-unseeded name; deleting it makes
  `?tabxplor-options`' own header promise ("keep in sync with `.onLoad()`") true.
- **`tabxplor.jmv_full_hash` → tier 3** — jamovi-internal (1 seed, 2 reads), currently documented in
  a section of its own.
- **`tabxplor.design_effect` + `tabxplor.anova` → fields of `tabxplor.inference`** (§4.2).
  `tabxplor.conf_level` **stays its own option**: it is a tier-1 argument's default, and the rule
  permits exactly that.

**Explicitly declined this session and recorded so they are not re-proposed:** folding
`kable_popover` into `tooltips` and `legend_style` into `color_legend`; deprecating
`tabxplor.output_kable` in favour of `print = "html"`; renaming the five `tab_kable_*` names (the
19m-iii ruling) — and, by the same logic, the three `xl_font_*` / `plot_num_font` names.

**Net: 35 → ~31 documented**, and every survivor is either tier 1 (an argument's default) or tier 2
(a bundle field). *An option that is neither is the smell the rule exists to catch* — after this
pass, the only ones left in that state are the genuinely document-level display switches
(`ci_print`, `test_lines`, `legend_style`, `spark`, `print`, `print_rules`, `var_labels`), and each
has a defensible reason to be per-document rather than per-call.

**One asymmetry worth closing while there:** `var_names` is both an option **and** a per-call
argument on all five exporters; `var_labels` is option-only, although they are the same kind of
display decision about the same names. Give `var_labels` the per-call argument (or make it a
`tab_style()` field, §4.7).

---

## 5. Cross-producer convergence — 19o's KEY ε, finished

The six questions the two producers ask twice, with this review's disposition of each:

| the question | `tab()` | `tab_reg()` | disposition |
|---|---|---|---|
| which sub-populations | `tab_vars` | `split_var` | **`tab_vars` on both** *(ruled)* — the storage already says so since 19f |
| how is the interval computed | `ci_method` (4 slots) | `method` | **one `ci_method`, `model` as its 5th slot**, inside `tab_inference()` (§4.2) |
| which level is the baseline | `ref` / `ref2` | `reference` + `inverse_two_level_factors` | **`ref` on both**, `c(var = "level")`; `ref2` stays *(ruled)*; the logical is absorbed (§4.6) |
| default colour | `"no"` | `TRUE` | **deliberate asymmetry, not documented** *(ruled 19o §11 q4)* |
| default significance policy | `"ignore"` | `NULL` → `grey_non_signif` | same ruling |
| what rides the footer | `test` (logical) | `stats` + `compare` + `baseline` | **`footer =`** on `tab_reg()` (§4.6); `test` stays what it is on `tab()` — a different question (an omnibus test, not a GOF block) |
| missing data | `na = keep/drop/drop_all/common_base` | `na = drop_by_outcome/drop_by_model/drop_all` | **keep two vocabularies** — they describe genuinely different operations — but generate both value lists from their declaring table (§4.3) so the difference is stated once |

---

## 6. Caveats, risks and anti-propositions

Carried from Phase 19 and 19o (**all still binding**), plus four new ones:

- Do not route regression columns through the aggregate core · do not go sparse on the record · do
  not merge fmt fields · do not replace the S3-per-verb model · do not re-open the settled perf
  verdicts · do not count lines as the simplification metric · do not export an accessor because a
  fact became stored · do not add a fact table without a foreign-key check.
- **NEW — do not use `@inheritDotParams`.** It inlines; `tab_many.Rd` is the 448-line proof.
- **NEW — do not introduce a bundle whose fields are also flat formals.** Two ways to say one thing
  is the ad-hoc layer hard rule 1 forbids. This is why option C (full bundles, colour included) was
  rejected in this session, and why `conf_level`/`ci`/`test`/`stars` stay flat and **out** of
  `tab_inference()`.
- **NEW — `...` costs IDE completion, so it goes on wrappers and superseded producers only.**
  `tab()` and `tab_reg()` keep every live formal.
- **NEW — check the released baseline before proposing a removal.** `git show v1.2.0:NAMESPACE`.
  Five of 19o's cut candidates are CRAN-released and need deprecation, not deletion.

**The one genuine risk in this whole review** is §4.5 item 1: moving `tab()`'s 9 deprecated formals
into `...` breaks positional calls past argument 5. It is acceptable in a major version *only* with
the unnamed-6th-argument abort, and it is the item that most needs the characterisation harness.

---

## 7. Sequencing

Each row is plan-then-implement in its own session, with its characterisation harness named. Items
1–3 are one short session and are independent of everything else.

| # | item | key | harness | risk |
|---|---|---|---|---|
| 1 | `tab_many.Rd`: `@inheritDotParams` → plain `@param ...` | D | `document()` diff | **none** (−390 Rd) |
| 2 | delete `tab_logit`/`multi_logit` + `kable_tabxplor_style`; delete `tabxplor.color_style_type`; `jmv_full_hash` + `tab_get_wrapped_dimensions` → internal | §3 | full suite | none |
| 3 | generate the ~15 argument value lists by `@eval`, incl. `color_measures_rd()` | C | `document()` idempotence + `tools::checkDocFiles()` | none |
| 4 | accessor repairs: `fmt_attr()`, `set_diff_type`→`set_ref_type`, the 3 inference getters, `tab_columns()`, the legend's df/basis sentence | A | `test-fmt_class.R` + a golden delta | low |
| 5 | `tab_reg()` renames: `ref`, `tab_vars`, `footer`, `.fit_cache`→`...` | ε | **`dev/verify_reg_specs.R` must print IDENTICAL** | low (unreleased) |
| 6 | `stars` ladder + `tabxplor.total_names` | §4.8 | `verify_golden_field_delta.R` | low, user-visible |
| 7 | `...` + `tab_check_dots()` on the three superseded producers | D | **new `dev/verify_tab_args.R`** | medium |
| 8 | `tab_inference()` | B | `verify_tab_args.R` + `verify_reg_specs.R` | medium |
| 9 | the 9 deprecated formals out of `tab()`'s signature + the unnamed-6th abort | §4.5 | `verify_tab_args.R` | medium, user-visible |
| 10 | `tab_style()` — **only if approved**, and probably after the release | §4.7 | export goldens + `_snaps/` | medium |

**Two harnesses to commit**, both on the `dev/verify_reg_specs.R` model (save/check, dumping the
messages in order as well as the values):

- **`dev/verify_tab_args.R`** — dump every crosstab producer's *resolved* settings
  (`tab_resolve_common_args()`'s return + the stored per-column attributes) over a grid of calls.
  This is what stands between items 7–9 and a silent mis-resolution, and there is no such harness
  today: `dev/verify_color_attrs.R` covers the colour vocabulary only.
- **the export-usage census** of §3.1, so "this export has no caller" is re-measurable rather than
  re-derived (and re-derived wrongly — see §1's `grep -w` warning).

---

## 8. Open questions — a maintainer ruling before the work starts

Written in 19o §11's format so the answers can be pasted back under each item.

1. **`tab_style()` (§4.7)** — the biggest single collapse left (28+ mirrored formals across five
   released exporters), the least statistically consequential, and the one whose neighbouring option
   folds you declined this session. *Recommendation: yes, but scheduled **after** the 2.0.0 release,
   as the first item of 2.1.0.*
   **Maintainer's decision:**
2. **`new_lvl()` / `is_lvl()`** — exported, `@keywords internal`, 0 test and 0 vignette callers.
   `tabxplor_lvl` is the row model's internal carrier; a user has no reason to build one.
   *Recommendation: un-export both (they are new in 2.0.0, so it is free).*
   **Maintainer's decision:**
3. **`tab_prepare()`** — released in `v1.2.0`, 1 non-legacy caller in `R/`, 9 test callers, 0
   vignette. It is a *data* transformer (`na_drop_all`, `cleannames`, `other_if_less_than`,
   `other_level`) and it is the last public home of `na_drop_all`, which `tab_many()`'s shim
   translates away. *Recommendation: keep and teach it in one line of the programming vignette, or
   demote it to `@keywords internal` — but decide, because it currently sits in the "Superseded
   entry points and steps" section beside five functions we are about to hard-deprecate, which reads
   as a verdict it has not been given.*
   **Maintainer's decision:**
4. **`pct`'s `"no"` default** — the most-used argument in the whole corpus (**1 129** occurrences,
   twice the next one), and its default is the value users almost never want. Changing it is a
   released behaviour change. *Recommendation: leave it (a bare `tab(d, x, y)` giving counts is
   correct and teachable), but say so explicitly in `?tab` so it stops looking like an oversight.*
   **Maintainer's decision:**
5. **19o's KEY δ (`TEST_ROWS`)** — §4.6's `footer =` argument would be its first consumer, so the two
   are cheaper together than apart. *Recommendation: after the release; do §4.6's rename now with a
   hard-coded key list, and let `TEST_ROWS` absorb it in 2.1.0.*
   **Maintainer's decision:**
6. **`fmt_attr()` beside the 23 named accessors (§2.3)** — deliberately two ways to say one thing,
   split along a taught/programmatic line that has to be stated or it reads as duplication.
   *Recommendation: both, with the rule in the header. The alternative — the generic alone plus the
   ~8 accessors that appear in a vignette — is cleaner but overrules your "setters stay exported"
   ruling in substance.*
   **Maintainer's decision:**

---

## 9. What this review is not

- **Not a statistics review.** As in Phase 19 and 19o, no soundness problem was found. Nothing
  proposed here changes a number: every item is a formal, a name, a documentation source or an
  export.
- **Not a re-litigation of a settled ruling.** The eleven rulings in force (19o §11's eight + this
  session's four, minus one overlap) appear in this document only as *settled*: the deprecated
  formals go into `...`; the legacy step API is hard-deprecated; `split_var` → `tab_vars`; the
  `color` default asymmetry is deliberate and undocumented; setters stay exported; the
  `tab_kable_*` renames stay dropped; the comment archaeology waits; `reference` → `ref` partially;
  `tab_logit`/`multi_logit` are deleted; `stars` absorbs the ladder; `tabxplor.total_names` lands.
- **Not the jamovi UI review** (Phase 20d), beyond one note: §4.8's `stars` ladder and §4.6's
  `footer =` both change a `.a.yaml` vocabulary, and `test-jamovi-vocabulary.R` will fail until
  `dev/generate_jamovi_js.R` and a maintainer `jmvtools::prepare()` run. ⚠ **The `.h.R` regeneration
  owed since 19k is still outstanding** — until it runs, `measure`, `shapes` and the renamed `test`
  read `NULL` in the running module.
