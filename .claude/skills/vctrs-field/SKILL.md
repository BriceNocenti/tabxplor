---
name: vctrs-field
description: Add, remove, rename, or change a per-cell FIELD or per-column ATTRIBUTE of the tabxplor_fmt vctrs record. Use whenever touching new_fmt()'s field list, or changing how a field is computed, displayed, colored, or carried through arithmetic/casting.
paths: ["R/fmt_class.R"]
allowed-tools: Read, Grep, Edit
---

`tabxplor_fmt` is a `vctrs::new_rcrd()` with two kinds of members. **Both lists are single-sourced in
`R/fmt_class.R` — read them there rather than trusting this file's copy.**

- **FIELDS**: per-cell, length = `length(x)`, accessed via `vctrs::field()`. Currently **21**, listed
  verbatim in `fmt_field_names` (~L1524):
  `n, display, digits, wn, pct, mean, diff, ratio, ctr, var, ci_inf, ci_sup, pvalue, or, tot_n,`
  `n_eff, obs, gap_se, in_totrow, in_tottab, in_refrow`.
  - `ci` is **not** a field — `get_ci()` derives the half-width from `ci_inf`/`ci_sup` (the Phase 1a
    bounds-shim); the public `fmt(ci=)` arg and `$ci` still work.
  - `resid` is **not** a field either — `fmt_resid()` derives the adjusted standardized residual from
    `pvalue` + `sign(ctr)`. A derived quantity is read-only: it gets a `get_num()` arm and no
    `set_num()` one. **Prefer deriving over adding a field** when the value is a pure function of
    existing ones.
  - `rr` was renamed `ratio` (read-side alias only).
- **ATTRIBUTES**: scalar per column, accessed via `attr()`. Currently **11**, and the list is
  **DERIVED**, never hand-written: `fmt_col_attrs <- setdiff(names(formals(new_fmt)), c(fmt_field_names,
  "...", "class"))` (~L1533) = `type, comp_all, ref, ci_type, col_var, totcol, refcol, color,`
  `color_signif, model_family, role`. Adding an attribute to `new_fmt()`'s signature therefore adds it
  to every rebuild site automatically — that is the point, do not reintroduce a literal list.

Re-grep exact line numbers before editing; the anchors below drift.

## Ordered checklist — ADD a per-cell field X (all in `R/fmt_class.R` unless noted)

1. **`fmt_field_names`** (~L1524): add `"X"`. This is what keeps `fmt_col_attrs` correct.
2. **`new_fmt()`** (~L1387): add the parameter (default `NULL`) and `X = X,` to the `new_rcrd()` field
   list. Defaults are filled in the body from the shared `nas`/`fls` vectors — follow that pattern, do
   not add a per-field `case_when` (Phase 18z6 removed one that cost half the constructor).
3. **`fmt()`** public constructor (~L304): add the parameter, a `vec_cast` + `vec_recycle` line, and
   pass `X = X` to `new_fmt()`.
4. **Getter/setter** via the factories: `get_X <- fmt_field_factory("X")` and
   `set_X <- fmt_set_field_factory("X", cast = double())`. Mark internal unless it is user contract.
5. **If X is displayable**: add an arm to `get_num()` (~L455, the authoritative `display` → field map),
   a matching arm in `set_num()` (~L515) *unless X is derived*, and the rendering in
   `format.tabxplor_fmt()`. Keep those three in sync — that trio has drifted twice (`or_pct`/`OR_pct`).
6. **If X drives colour**: the engine is `fmt_color_plan()` → `fmt_color_slots()` →
   `fmt_color_channels()`/`fmt_channel_codes()`, all driven by the **`MEASURES` fact table** (~L3390).
   A new measure is ONE row there (`raw`, `scale`, `std_when`, `sig_source`, `bounds`, `gate_row`,
   `force_policy`, plus the legend facts) — never a new `switch` arm. Read `MEASURES` only through
   `measure_facts()` / `measure_policy()`. See the `/color-mode` skill.
7. **Arithmetic** — `vec_arith.tabxplor_fmt.tabxplor_fmt()`: decide per operation whether X is carried,
   reset to `NA`, or recomputed, for both `+/-` and `*//`; also `fmt.numeric` / `numeric.fmt`. Rule of
   thumb: raw data (`n`, `wn`) is carried; computed metadata (`diff`, `ci`, `ctr`, `var`, `n_eff`,
   `obs`, `gap_se`) is reset to `NA`; `pct`/`mean` recomputed when meaningful.
8. **Casting** — the fmt→fmt cast and `vec_ptype2` need **NO edit** since Phase 19a: the cast takes
   every field from `fmt_data_wn(x)` and every attribute from `fmt_attrs_of(to)`, and the ptype has
   no fields at all. Only the two `vec_math` sum/mean arms still list fields by hand — decide there
   whether X aggregates or resets to `NA`. `vec_proxy_equal()` / `vec_proxy_compare()` only if X
   affects equality/ordering.
9. **Populate X where it is computed.** The live producers are `plain_core()` / `num_core()` /
   `leaf_wide_pct()` / `tab_ci()` / `tab_chi2()` / `tab_apply_reference()` in `R/tab.R`, and
   `reg_column()` / `reg_marginal_column()` / `reg_empirical_columns()` in `R/tab_reg.R`.
   `R/tab-steps-legacy.R` holds the superseded `tab_pct()`/`tab_tot()`/`tab_totaltab()` trio — it is
   NOT on the aggregate path, but it is exported, so check whether it needs the field too.
10. **Docs** — roxygen in `fmt()`: add `@param X` and keep the field count in sync.
11. **EXPORT PARITY (critical)**: `format.tabxplor_fmt()` is the single source of truth for markdown
    (`tab_md.R`), HTML (`tab-render-html.R`) and the console (`pillar_shaft`). `tab_xl()` bypasses it
    for *values* (it writes `get_num()` raw and lets Excel format) but sources its number-format codes
    from `format(x, syntax = "excel")`, so a display change no longer needs mirroring there. Colour is
    safe everywhere — every backend calls `fmt_color_channels()` / `fmt_channel_codes()`.
12. **Verify**: the CLAUDE.md § Testing recipe (temp runner outside `tests/`, `OMP_NUM_THREADS=1`,
    `TESTTHAT_CPUS=8`, unsandboxed). A new field **changes the record shape**, so
    `tests/testthat/_snaps/fmt-contract.md` and all 36 `_golden/*.rds` must be consciously
    regenerated — prove the delta is only the new all-NA column with
    `dev/verify_golden_field_delta.R` (that script exists precisely for this). Then
    `devtools::document()` **unsandboxed** (`NAMESPACE`/`man/` are read-only in the sandbox).

## For a per-column ATTRIBUTE instead of a field

**Two lines, since Phase 19a.** All four reconstructor families are driven by one declared table.

1. **`new_fmt()`** (~L1600, after the fields): add the formal, with a length-1 constant default.
   `fmt_col_attrs` derives from the signature, so every carry site picks it up automatically.
2. **`fmt_attr_rules`** (~L1745, right below `fmt_col_attrs`): add ONE row —
   `list(neutral = , merge = , arith = , scalar = )`. The block comment there is the vocabulary
   (`merge`: `same` / `comp3` / `elementwise` / `min` / `weakest`; `arith`: `merge` / `neutral` / `x`).
   A build-time `stopifnot()` refuses to install the package if you skip this, and the E1 fixture in
   `test-fmt_class.R` covers the new row automatically — it loops over the rule table itself.

Optionally add a getter/setter (pattern near `get_type()`/`set_type()`, or the
`fmt_conf_level_attr()` / `get_conf_level()` pair when the RAW read the reconcilers need must differ
from the resolved one the engines want). **Do NOT hand-edit `vec_ptype2` / `vec_cast` / `vec_arith` /
`vec_math`** — the seven literal attribute lists that used to live there are exactly what E1 deleted;
re-introducing one is the regression the rule table exists to prevent.

An attribute must be present on a STANDALONE extracted column — `format()` and colour have to work on
`tab$col` outside its table. Regenerate the goldens and prove the delta with
`dev/verify_golden_field_delta.R` (declare it in `ADDED_ATTRS` + `EXPECTED_ATTR`, and **reset that
file's four declarations first**: one left behind from the previous phase reports its own
already-landed change as a problem).
