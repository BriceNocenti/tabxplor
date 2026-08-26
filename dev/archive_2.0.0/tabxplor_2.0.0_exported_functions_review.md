# tabxplor 2.0.0 — the exported surface

## What this file is for

A map of everything `tabxplor` exports, and a ranked list of what could stop being exported — with, for each candidate, the work needed to remove it *without* leaving a user stuck. Phase 22m implemented the index and page-structure half of that review; the **unexport proposals in §5 are not implemented** and wait for a maintainer decision.

It answers three questions and no others: *what is exported*, *what does a user actually reach for*, *what would removing one cost*. The architecture of the `fmt` record, the pipeline and the colour engine lives in `CLAUDE.md § tabxplor architecture`; usage lives in the vignettes.

---

## 1. The shape of the surface

|                                                 |  count |
|:------------------------------------------------|-------:|
| `export()` entries in `NAMESPACE`               | **98** |
| — inherited from 1.3.1 (a released contract)    |     56 |
| — new in 2.0.0 (never on CRAN, free to drop)    |     42 |
| `S3method()` entries                            |    143 |
| Exported symbols carrying `@keywords internal`  |     14 |
| Public Rd topics in the pkgdown reference index | **43** |

Two facts bound every decision below.

**Only the 42 new symbols are free.** The 56 from 1.3.1 are a released contract. 2.0.0 already broke four of them deliberately (`get_type()` / `set_type()` / `get_ci_type()` / `set_ci_type()`, §7), which is the precedent — and its price: each needed an informative error written by hand.

**Unexporting an accessor buys nothing on the index.** All 41 `fmt` accessors sit on three Rd pages, so the index only shrinks by restructuring. What unexporting buys is a shorter `\usage{}` block, a smaller `NAMESPACE`, and one less thing to keep working forever.

---

## 2. The map

Read `R` / `t` / `v` as call sites in `R/`, `tests/testthat/`, `vignettes/`. **v = 0 is the signal that matters**: the vignettes are the taught surface, so a function absent from all of them has no user story on record.

### The producers and their variants

| symbol       | since |   R |    t |   v | verdict                                                       |
|:-------------|:------|----:|-----:|----:|:--------------------------------------------------------------|
| `tab`        | 1.3.1 | 254 | 1235 | 278 | keep — the headline                                           |
| `tab_reg`    | 2.0.0 | 142 |  836 | 312 | keep — the headline                                           |
| `tab_num`    | 1.3.1 |  19 |  118 |   3 | keep                                                          |
| `tab_counts` | 2.0.0 |  20 |   52 |  17 | keep                                                          |
| `tab_plain`  | 1.3.1 |  19 |  126 |   0 | keep, superseded — the smallest entry into the aggregate core |
| `tab_many`   | 1.3.1 |  19 |   24 |   0 | keep, superseded — a shim over `tab()`                        |

### The step chain — hard-deprecated, defunct in 2.1.0

| symbol                                                 | since |    R |     t | v | verdict                                                |
|:-------------------------------------------------------|:------|-----:|------:|--:|:-------------------------------------------------------|
| `tab_pct` `tab_tot` `tab_totaltab` `tab_ci` `tab_chi2` | 1.3.1 | 3–16 | 19–40 | 0 | keep exported until 2.1.0; **off the index since 22m** |

They warn on every call and share their arithmetic with the leaves, so a step and a build cannot give two answers. Their `R/` call sites are the shared helpers, not the steps themselves.

### Defunct stubs — exported so the old call gets an answer

| symbol                 | since | verdict                                                                                     |
|:-----------------------|:------|:--------------------------------------------------------------------------------------------|
| `tab_plot`             | 1.3.1 | `deprecate_stop()` → `tab_export()`. **Keep exported**: the stub *is* the migration message |
| `kable_tabxplor_style` | 1.3.1 | `deprecate_stop()` → `tab_html()`. Same                                                     |

### Exporters

| symbol                      | since         |            R |             t |          v | verdict                                          |
|:----------------------------|:--------------|-------------:|--------------:|-----------:|:-------------------------------------------------|
| `tab_export`                | 2.0.0         |           16 |            20 |         18 | keep — the one most users need                   |
| `tab_html` / `tab_kable`    | 2.0.0 / 1.3.1 |      36 / 20 |       52 / 87 |     18 / 0 | keep; `tab_kable` is a permanent alias, one page |
| `tab_md` `tab_xl` `tab_css` | 2.0.0         | 33 / 23 / 45 | 150 / 74 / 56 | 6 / 3 / 38 | keep                                             |

### Reshape and introspection

| symbol                                       | since                 |           R |           t |          v | verdict                                                 |
|:---------------------------------------------|:----------------------|------------:|------------:|-----------:|:--------------------------------------------------------|
| `tab_spread` `tab_compact` `tab_transpose`   | 1.3.1 / 1.3.1 / 2.0.0 | 10 / 10 / 9 | 9 / 11 / 12 | 10 / 0 / 0 | keep                                                    |
| `tab_structure` `tab_supports` `tab_columns` | 2.0.0                 |  14 / 5 / 5 |   3 / 2 / 4 |  2 / 3 / 5 | keep — the 2.0.0 introspection trio                     |
| `tab_get_vars`                               | 1.3.1                 |          29 |          22 |          0 | keep exported, **superseded by `tab_structure()`** (§7) |
| `is_tab`                                     | 1.3.1                 |           4 |           2 |          0 | keep — the class predicate                              |
| `tab_get_wrapped_dimensions`                 | 1.3.1                 |       **0** |       **0** |      **0** | **Tier 2 removal candidate**                            |

### Regression, charts, jamovi

| symbol                          | since         |       R |       t |       v | verdict                                                     |
|:--------------------------------|:--------------|--------:|--------:|--------:|:------------------------------------------------------------|
| `reg_measures` `reg_formulas`   | 2.0.0         |  8 / 15 |   5 / 5 | 22 / 18 | keep — both taught                                          |
| `forest_plot` `reg_check_plots` | 2.0.0         | 20 / 34 | 53 / 28 | 27 / 16 | keep                                                        |
| `jmvtab` `jmvtabreg`            | 1.3.1 / 2.0.0 |       0 |       0 |       0 | keep — jamovi's entry points, called from `jamovi/*.a.yaml` |

### Colour, caption, options, data

| symbol                                                                   | since         |              R |              t |              v | verdict                                       |
|:-------------------------------------------------------------------------|:--------------|---------------:|---------------:|---------------:|:----------------------------------------------|
| `set_color_palette` `set_color_breaks`                                   | 2.0.0 / 1.3.1 |        22 / 23 |        16 / 33 |        25 / 19 | keep                                          |
| `get_color_breaks`                                                       | 1.3.1         |              7 |             16 |              0 | keep — the getter of a taught setter          |
| `set_color_style` `get_color_style`                                      | 1.3.1         |         3 / 30 |         3 / 32 |              0 | keep, soft-deprecated → `set_color_palette()` |
| `conf_level_to_z`                                                        | 2.0.0         |              6 |              9 |              4 | keep                                          |
| `set_caption` `get_caption`                                              | 2.0.0         |          4 / 3 |          7 / 6 |          6 / 0 | keep                                          |
| `gss_cat_data_formatting`                                                | 2.0.0         |              3 |             49 |             10 | keep — used by examples and vignettes         |
| `score_from_lv1` `shape_numeric_var` `tab_wrap_text` `tab_parallel_stop` | mixed         | 3 / 3 / 13 / 4 | 9 / 25 / 2 / 4 | 12 / 0 / 0 / 3 | keep                                          |

### The `fmt` surface — 41 accessors on three pages

`?fmt` (the record) · `?fmt_fields` (15, per-cell) · `?fmt_attributes` (24, per-column) · `?fmt_attr` (any attribute by name).

**In a vignette** — the taught set: `fmt` `is_fmt` `get_num` `set_display` `get_scale` `set_scale` `get_pct_type` `set_pct_type` `get_col_var` `get_col_group` `get_comp_all` `get_ci_method` `get_color` `set_color` `set_pvalue` `is_totrow` `as_totrow` `is_totcol` `is_refrow` `is_refcol` `is_tottab` `fmt_attr`.

**Never in a vignette, but 1.3.1** — keep, whatever their use: `as_refcol` `as_refrow` `as_tottab` `get_digits` `set_digits` `get_ref_type` `set_col_var` `set_comp_all` `set_diff_type` `set_num` `get_color_style` `fmt_get_color_code` `new_grouped_tab`.

**Never in a vignette and new in 2.0.0** — the free set, and §5's Tier 1 draws from it: `get_color_bg` `get_color_signif` `set_color_signif` `get_model_family` `set_model_family` `get_pvalue` `get_row_kind` `set_row_kind` `set_ref_type` `get_caption` `new_lvl` `is_lvl`.

`set_ref_type` is **not** a candidate: `set_diff_type()` is soft-deprecated *onto it*, so it is the documented replacement. `get_pvalue` / `set_color_signif` / `get_color_signif` / `get_caption` are the getters or setters of taught facts and would leave a one-sided pair.

---

## 3. Already invisible: 14 exports carrying `@keywords internal`

They cost `NAMESPACE` and maintenance, never index space. Two groups:

**Deliberate, keep** — the two defunct stubs (`tab_plot`, `kable_tabxplor_style`), the five deprecated steps, `tab_get_vars` (superseded), and the three announced for 2.1.0 (`tab_prepare`, `complete_partial_totals`, `fct_recode_helper`).

**Not deliberate** — `new_lvl`, `is_lvl` (2.0.0, internal-only) and `tab_get_wrapped_dimensions` (1.3.1, zero call sites). These are exports nobody decided to make.

---

## 4. What Phase 22m implemented

No export was added or removed; `NAMESPACE` is byte-identical.

- The five deprecated steps and `tab_get_vars()` took `@keywords internal` — still exported, still working, off the index.
- `?fmt` (719 lines, 42 aliases) split into `?fmt` (445), `?fmt_fields` (127) and `?fmt_attributes` (225), on the type system's own line: a **field** varies per cell, an **attribute** over a column. Both new pages take their parameter prose from `fmt()`'s block through `@inheritParams`, and each replaces ~15 near-identical one-line `@return` tags with one statement of the rule.
- `reg_formulas()` and `shape_numeric_var()` joined `_pkgdown.yml`, which had never listed them.
- `?tab_plain` and `?tab_num` stopped teaching `tab_prepare()` and `tab_chi2()` in their examples.

---

## 5. Ranked removal candidates — NOT implemented

Ordered by cost-to-benefit. The rule the codebase already states (`R/fmt_class.R`, above `fmt_attr()`): *storing a fact is internal; exporting its accessor is a user contract — name the user story first.*

### Tier 1 — new in 2.0.0, no user story, zero migration cost

Nobody can be calling these: they were never released, appear in no vignette, and every one is reachable another way.

| symbol                                | reachable instead by                                                                                    | work needed                                                                                                           |
|:--------------------------------------|:--------------------------------------------------------------------------------------------------------|:----------------------------------------------------------------------------------------------------------------------|
| `new_lvl` `is_lvl`                    | internal only; users never build a `tabxplor_lvl`                                                       | drop `@export`; the `@keywords internal` Rd stays                                                                     |
| `get_model_family` `set_model_family` | `fmt_attr(x, "model_family")`, `tab_columns()$model_family`                                             | drop `@export`; move both off `?fmt_attributes`                                                                       |
| `set_row_kind`                        | `fmt_attr()` cannot (it is a *field*): `vctrs::field(x, "row_kind") <- v`, or `mutate(x, row_kind = v)` | drop `@export`; keep `get_row_kind()`, the readable half of the `in_totrow` → `row_kind` migration `NEWS.md` promises |
| `get_color_bg`                        | `fmt_attr(x, "color")[2]`, `tab_columns()$color_bg`                                                     | drop `@export`; its `@describeIn` line moves into `get_color()`'s                                                     |

**Total: 6 exports, 98 → 92.** No stub, no error message, no `NEWS.md` entry — nothing to migrate from.

⚠ The one argument against: `set_row_kind()` and `get_model_family()` are the *named* half of facts a user can meet in a table (`x$row_kind`, a mixed-family regression). Dropping them says "use `fmt_attr()` / `vctrs::field()`", which is the programmatic surface, not the taught one. It is a small loss of symmetry for a small gain.

### Tier 2 — 1.3.1, dead, but a released name

| symbol                                                      | evidence                                                                                                | work needed                                                                                                                                                                                                                        |
|:------------------------------------------------------------|:--------------------------------------------------------------------------------------------------------|:-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| `tab_get_wrapped_dimensions`                                | **zero call sites** in `R/`, tests, vignettes, `dev/`; no `NEWS.md` entry; already `@keywords internal` | Either drop `@export` outright (an unexported 1.3.1 name gives `could not find function`), or replace the body with `deprecate_stop("2.0.0", ...)` naming `tab_wrap_text()`. The stub is the user-friendly form and costs 4 lines. |
| `tab_prepare` `complete_partial_totals` `fct_recode_helper` | already `@keywords internal` + `deprecate_soft()`                                                       | **Do nothing.** `NEWS.md` promises them internal in **2.1.0**; pulling that forward breaks a written promise for no index gain, in a release that is already heavy on breakage.                                                    |

### Tier 3 — the legacy accessor wall — recommended against

Twelve 1.3.1 accessors never appear in a vignette (§2). Retiring each behind a `deprecate_stop()` stub would take `NAMESPACE` to ~80.

Reasons not to:

- **It buys no index space.** They all live on `?fmt_attributes` / `?fmt_fields`, which stay either way.
- **A stub is permanent code.** Twelve of them is twelve things that must keep building and keep being tested, forever, to save nothing a user sees.
- **"Not in a vignette" is weak evidence for an accessor.** `set_num()` and `get_digits()` are exactly what a `mutate()` over `fmt` columns reaches for — the vignette teaches the *pattern*, not every name in it.
- The one real cleanup here is already done: `set_diff_type()` is soft-deprecated onto `set_ref_type()`.

---

## 6. Defects found

| defect                                                                                                                               | status                                     |
|:-------------------------------------------------------------------------------------------------------------------------------------|:-------------------------------------------|
| `reg_formulas()` and `shape_numeric_var()` were absent from `_pkgdown.yml`, so pkgdown could not index them                          | **fixed** (22m)                            |
| `?tab_plain` taught `tab_prepare()` **and** `tab_chi2()`; `?tab_num` taught `tab_prepare()` — all hard-deprecated, on public pages   | **fixed** (22m)                            |
| `tab_get_vars()` (1.3.1) silently duplicated by `tab_structure()` (2.0.0), both advertised side by side                              | **fixed** (22m): superseded, off the index |
| `vignettes/tabxplor.Rmd:546` and `vignettes/tabxplor-programming.Rmd:349` describe `tab_plot()` as working; it is `deprecate_stop()` | **reported** — Phase 22l's territory       |
| `?fmt`'s `\value` was a stack of ~40 one-line paragraphs                                                                             | **fixed** (22m) by the split               |

An **index-completeness check** is what would have caught the first one before a pkgdown build. It fits in one script: {public Rd topics} must equal {topics listed under `reference:`}, in both directions. Worth running before any release.

---

## 7. Two decisions recorded, so they are not re-litigated

**`get_type()` / `set_type()` / `get_ci_type()` / `set_ci_type()` were removed with no stub.** Deliberate: `type` conflated *what a column estimates* with *which percentage it is*, and is split into `scale` + `pct_type`; `ci_type` is gone rather than renamed, the stored interval always being on the estimate's own scale. `fmt()` answers a `type =` / `ci_type =` call with the full mapping (`fmt_abort_legacy_args()`), which is why no accessor stub was needed. Documented in `NEWS.md`.

**`tab_get_vars()` is superseded, not deprecated.** `tab_structure()` returns the same three variable lists plus `container` / `kind` / `merged` / `grouped`, from the same declared model. `tab_get_vars()` keeps working and keeps its 29 internal call sites; it simply is not the answer a new user should find first.

---

## 8. `NEWS.md`

Nothing in §4 changes a user-callable contract: no export added or removed, no argument renamed, no behaviour altered. The only user-visible change is where things are documented, which `NEWS.md` does not track. **No entry needed.**

Should §5 Tier 1 ever land, one line under *Removed* suffices: the six names, and the sentence that `fmt_attr()` / `vctrs::field()` reach the same facts.
