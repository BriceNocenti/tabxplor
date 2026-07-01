---
name: dplyr-method
description: Add or fix an S3 method so a dplyr verb preserves the tabxplor_tab / tabxplor_grouped_tab class and its attributes (subtext, chi2, grouping). Use when a dplyr verb silently downgrades a table to tbl_df, or when a refactor adds a new verb that must keep the class.
paths: ["R/tab_classes.R", "NAMESPACE"]
allowed-tools: Read, Grep, Edit
---

`tabxplor_grouped_tab` extends `grouped_df` and `tabxplor_tab` extends `tbl_df`. dplyr rebuilds
objects via the low-level trio `dplyr_row_slice()` / `dplyr_col_modify()` / `dplyr_reconstruct()`,
plus a few verb-level methods. A missing method = **silent class downgrade** (the table becomes a
plain tibble and loses `subtext`/`chi2` and color-aware printing). Symptom to confirm the bug:
`class(some_verb(tab))` no longer contains `"tabxplor_tab"`.

Re-grep exact line numbers before editing; anchors below are approximate. All in R/tab_classes.R.

## The canonical pattern (copy it exactly)

Every class-preserving method: call `NextMethod()`, then rebuild via `lv1_group_vars()` (~L2385),
which decides whether only one grouping level is left (→ downgrade to `tabxplor_tab`) or it stays grouped:

```r
verb.tabxplor_grouped_tab <- function(data, ...) {
  out <- NextMethod()
  if (lv1_group_vars(out)) {
    new_tab(out, subtext = get_subtext(data), chi2 = get_chi2(data))
  } else {
    groups <- dplyr::group_data(out)
    new_grouped_tab(out, groups, subtext = get_subtext(data), chi2 = get_chi2(data))
  }
}
```

Constructors: `new_tab()` and `new_grouped_tab()` (~L93). Attribute carriers: `get_subtext()`,
`get_chi2()`. Reference implementations to copy from: `dplyr_row_slice.tabxplor_grouped_tab` (~L2407),
`dplyr_col_modify` (~L2426), `dplyr_reconstruct` (~L2444), `[.tabxplor_grouped_tab` (~L2465).

## Checklist — add a class-preserving method for verb V

1. Decide the target class(es). Most verbs only need a `*.tabxplor_grouped_tab` method (the trio +
   `dplyr_reconstruct` usually cover `tabxplor_tab`). Some verbs need an explicit `*.tabxplor_tab`
   too (see existing `arrange.tabxplor_tab` ~L27 in NAMESPACE, `group_by.tabxplor_tab`,
   `rowwise.tabxplor_tab`). Add `*.tabxplor_tab` only when the plain-tab path also downgrades.
2. Write the method following the canonical pattern above. If V changes grouping (like `group_by`,
   `ungroup`, `summarise`), model it on those existing methods rather than the trio — they recompute
   `groups` differently (see `ungroup.tabxplor_grouped_tab` ~L2196, `summarise` ~L2537).
3. roxygen: `#' @importFrom dplyr <verb>`, `#' @method <verb> tabxplor_grouped_tab`, `#' @export`,
   with `@param`/`@return` like the neighbours. Do NOT hand-edit NAMESPACE.
4. `devtools::document()` to regenerate the `S3method(<verb>,tabxplor_grouped_tab)` line.
5. Add a case to `tests/testthat/test-tab_classes.R` asserting the class survives V (and, for grouped
   input that collapses to one level, that it downgrades to `tabxplor_tab` — that IS the intended
   behaviour, not a bug). The extended data-driven loop in that file enumerates verbs; add V there.

## Verify

- `source("tests/testthat.R", encoding = "UTF-8")` — `test-tab_classes.R` must stay green, including
  the new verb case. Also run `test-golden.R` if the verb feeds an exporter.
- Quick manual check: `tab_many(...) |> V(...) |> class()` contains `tabxplor_tab` /
  `tabxplor_grouped_tab` as expected.
- NEWS.md bullet if the missing method was a user-visible bug.
