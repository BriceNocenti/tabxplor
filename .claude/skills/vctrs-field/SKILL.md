---
name: vctrs-field
description: Add, remove, rename, or change a per-cell FIELD or per-column ATTRIBUTE of the tabxplor_fmt vctrs record. Use whenever touching new_fmt()'s field list, or changing how a field is computed, displayed, colored, or carried through arithmetic/casting.
paths: ["R/fmt_class.R"]
allowed-tools: Read, Grep, Edit
---

tabxplor_fmt is a vctrs::new_rcrd() with two kinds of members:

- FIELDS: per-cell, length = length(x), accessed via vctrs::field(). Currently 18:
  n, display, digits, wn, pct, mean, diff, ratio, ctr, var, ci_inf, ci_sup, pvalue, or,
  tot_n, in_totrow, in_tottab, in_refrow.
  NOTE: `ci` is NOT a field — it is derived from ci_inf/ci_sup by get_ci() (a bounds-shim,
  Phase 1a); the public fmt(ci=) arg and $ci/get_ci() still work. `rr` was renamed `ratio`.
- ATTRIBUTES: scalar per-column, accessed via attr(). Currently 10:
  type, comp_all, ref, ci_type, col_var, totcol, refcol, color, color_signif, display_spec.
  (`color_signif` added Phase 5 = the significance policy; `display_spec` added Phase 10c = an
  opt-in composite display recipe like "pct (n)", NA by default, parsed only in format().)

Re-grep exact line numbers before editing; the anchors below are approximate.

## Ordered checklist — ADD a per-cell field X (all in R/fmt_class.R unless noted)

1. new_fmt() (~L967; new_rcrd() list ~L1037): add the parameter and add `X = X,` to the field list.
2. fmt() public constructor (~L216): add the parameter; add a vec_cast + vec_recycle line (~L251-266,
   pattern: `X <- vctrs::vec_recycle(vctrs::vec_cast(X, double()), size = max_size)`); pass `X = X` to
   new_fmt() (~L277).
3. Getter/setter via the factories: `get_X <- fmt_field_factory("X")` (~L1086-1138) and
   `set_X <- fmt_set_field_factory("X", cast = double())` (~L1285-1325). Adjust the cast type.
4. If X is displayable: extend get_num() (~L317-335) to select X, and add its formatting in
   format.tabxplor_fmt() (~L1375-1541).
5. If X drives cell color: extend fmt_color_selection() (~L1894) and color_formula().
6. Arithmetic — vec_arith.tabxplor_fmt.tabxplor_fmt() (~L3128-3244): decide per operation whether X is
   carried, reset to NA, or recomputed, for both +/- and */. Also handle fmt.numeric (~L3251) and
   numeric.fmt (~L3266) if X is numeric data. Rule of thumb from current code: raw data (n, wn) is
   carried/operated; computed metadata (diff, ci, ctr, var) is reset to NA; pct/mean recomputed when meaningful.
7. Casting — vec_cast.tabxplor_fmt.tabxplor_fmt() (~L2975-3002): copy X from x. vec_ptype2.*.* (~L2889) only
   if X is attribute-like. vec_proxy_equal() (~L3074) / vec_proxy_compare() (~L3082) only if X affects equality/ordering.
8. Populate X where it is computed, in R/tab.R:
   - tab_plain(): aggregation (~L3050-3209) + the new_fmt() call (~L2642-2681).
   - tab_pct(): set_*() calls (~L4346-4474).
   - tab_ci(): CI block (~L4828-4854).
   - tab_chi2(): var/ctr block (~L4990-5082).
9. Docs — roxygen in fmt() (~L36-123): add `@param X` and keep the field-count in sync (the roxygen text
   says "18 fields" — update the count and the field list when you add/remove/rename one).
10. EXPORT PARITY (critical): format.tabxplor_fmt() is the single source of truth for markdown (tab_md.R:130),
    knitr/HTML (tab_kable in tab_classes.R:615,639), and console (pillar_shaft). But tab_xl() BYPASSES it: it
    reads get_num()/get_display()/get_digits() directly (tab_xl.R:539, 587-588) and delegates numeric
    formatting to Excel. So any display-affecting field needs a matching edit in tab_xl.R. Color is safe —
    all exporters call the same fmt_color_selection().
11. Verify: source("tests/testthat.R", encoding = "UTF-8") — especially test-fmt_class.R (creation, printing,
    c(), arithmetic, casting) and test-tab.R. Then devtools::document().

## For a per-column ATTRIBUTE instead of a field

Add it to new_fmt()'s attribute args (~L1043); reconcile it in vec_ptype2.tabxplor_fmt.tabxplor_fmt()
(~L2889) and in the vec_arith attribute-merge block (~L3180-3187); add attribute getter/setter
(pattern near ~L365-904, e.g. get_type()/set_type()).
