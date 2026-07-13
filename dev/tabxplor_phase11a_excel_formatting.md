# Phase 11a — Excel formatting diagnosis & fix (`tab_xl` / openxlsx2)

Date: 2026-07-13. Scope: the `review_tabxplor_1.4.0-dev.xlsx` formatting mess reported during the
Phase 11 manual review. This document records the diagnosis, the single root cause, the fix, the
verification against the 1.3.1 reference workbook, and the secondary findings (what is a real bug,
what is *intended* 1.4.0 behaviour, and what is deferred).

---

## TL;DR

- **One root cause** explains ~every "mess" symptom: `xl_apply_styles()` reused per-table style **names**
  (`txf1` / `txl1` / `txb1` / `txx1`, …) in openxlsx2's **workbook-global** styles manager. Its
  `get_*_id(name)` resolves a name to its **first** match, so from **table 2 onward every table was
  painted with table 1's style objects** (offset/missing borders, random font sizes, dead colours,
  broken subtext). Table 1 alone looked correct because it registered first.
- **Fix**: one **workbook-scoped style registrar** (`xl_style_registrar(wb)`) that dedups
  fonts/fills/borders/xfs by *content* and hands out **globally-unique** style names. `xl_apply_styles`
  became a thin apply loop threaded through `xl_write_table`. `R/tab_xl.R` only.
- **Verified**: `tab_xl` / export-parity / export / xl-backend / render-html suites — **299 pass, 0 fail**;
  the regenerated review workbook now matches 1.3.1 formatting on all 15 sheets (headers, borders,
  first-column bold/alignment, subtext).
- **Secondary findings** (not the collision):
  - Numeric `color = "diff"` colours **do apply** — they are just **sparse by design** (Glass's Δ,
    `mean_diff_breaks` 0.2–1.2 SD), which differs from 1.3.1's multiplicative ratio. **Intended** (Phase 5).
  - Factor colour **hex** differs from 1.3.1 (Phase 5 palette). **Intended**; exact breaks/which-cells is
    a Phase 11 *calculation*-review question, not a formatting bug.
  - `ci = "cell"` (sheets 10/11) renders the raw full-precision proportion with an Excel **`@` text**
    format — a genuine, separate limitation of the numeric-bypass architecture. **Deferred** (per the
    maintainer), documented below with a contained fix.

---

## 1. Symptoms (from the manual review)

The 1.4.0-dev workbook degenerated on **every sheet except the first table**, with and without `tab_vars`:

- borders offset or missing;
- some cells (numeric cells *and* column headers) randomly larger font;
- first column: subtotals not left-aligned, `row_var` level names shown **bold** (only subtotals should
  be bold);
- subtext lost its formatting — too bold, oversized, wrapped, **even coloured** (sheet 12);
- numeric `col_vars` with `color = "diff"` seemingly not coloured;
- `ci = "cell"` (secondary) shows the raw base number with all digits and no formatting.

The tell was **"only table 01 is OK, everything else is a mess"** — the classic signature of state
leaking across writes, not a per-table computation error.

---

## 2. Root cause — workbook-global style-name collision

### 2.1 The mechanism

`tab_xl()` writes every table into **one** `wbWorkbook`, whose `wb$styles_mgr` is **workbook-global**
(the xlsx `styles.xml` is shared by all sheets). The old `xl_apply_styles()` created its name counter and
its font/fill/border caches **fresh on every call** (i.e. every table):

```r
xl_apply_styles <- function(wb, s, styles) {
  sm  <- wb$styles_mgr
  fc  <- new.env(...); lc <- new.env(...); bc <- new.env(...); ctr <- 0L   # <-- reset PER TABLE
  uid <- function() { ctr <<- ctr + 1L; ctr }                              # -> txf1, txl1, txb1, txx1 ...
  ...
}
```

So table 2, table 3, … each re-minted the names `txf1`, `txl1`, `txb1`, `txx1`, … — **colliding with
table 1's**. openxlsx2's resolver returns the **first** matching name:

```r
# openxlsx2 styles_mgr private$get_id():
get_id <- function(df, name) { sel <- match(name, df$name); df$id[sel] }   # match() => FIRST hit
```

`styles_mgr$add(style, name, skip_duplicates = TRUE)` dedups by style *content*, but still appends a
`(typ, id, name)` row. When table 2 adds a **different** font under the reused name `txf1`, the map ends
up with **two** rows named `txf1` (ids 1 and 2), and `get_font_id("txf1")` returns **1 — table 1's
font**. The same happens for fills, borders, and the composed cell `xf`. Net effect:

> For every distinct style *i* in table *k ≥ 2*, `wb$set_cell_style(dims = <table k dims>,
> style = get_xf_id("txx<i>"))` applied **table 1's** *i*-th composed style to **table k's** cells.

Because table 1 and table *k* have different geometry (different row/column counts, `tab_vars`
subtotals, reference rows), mapping "table 1's *i*-th style → table *k*'s *i*-th coalesced range" is
garbage: wrong borders, wrong font sizes, wrong fills, mis-styled subtext. **Table 1 is the only table
that registers into a clean name space**, so it alone renders correctly — exactly the reported pattern.

### 2.2 Evidence

- **Resolver semantics** — `match(name, df$name)` returns the first hit (source above).
- **Minimal reproduction** — add font A as `txf1`, later add a *different* font B as `txf1`;
  `get_font_id("txf1")` returns **1** (A), not 2 (B).
- **Real `tab_xl` path** (3-table gss_cat export, read back): sheet 2's header cell **B2 font size = 10**
  (a data-cell size) instead of the correct **9**; sheets 1 and 3 differ arbitrarily — the "random
  bigger font on headers" symptom.
- **Subtext (sheet 12, pc18 review)** — buggy legend cell = `sz10 BOLD, colour C38C46`; correct =
  `sz9 normal black`. Precisely "too bold, oversized, coloured".
- **First column (sheet 02)** — buggy col-A bold/alignment mismatched; the correct pattern is
  *subtotals bold + left*, *level names non-bold* (see §4).

> Note: a `save()`→`wb_load()` round-trip discards openxlsx2's in-memory style *names* (they are not
> persisted to `styles.xml`), so the duplicate name-map is invisible in a reloaded file — but the
> **damage is baked into each cell's style index** and is what the reads above measure.

---

## 3. The fix — one workbook-scoped style registrar

`R/tab_xl.R` only. The per-table caches + counter were hoisted into a **single registrar created once
per workbook**, so style names are monotonic (never collide) and style objects are deduped **across all
tables** (fewer nodes, smaller file, correct by construction):

```r
xl_style_registrar <- function(wb) {
  sm  <- wb$styles_mgr
  fc <- new.env(...); lc <- new.env(...); bc <- new.env(...); xc <- new.env(...)
  ctr <- 0L
  uid <- function() { ctr <<- ctr + 1L; ctr }         # monotonic across the WHOLE workbook
  font_id   <- function(...) { ... }                  # dedup by content, unique name txf<uid>
  fill_id   <- function(...) { ... }                  #                              txl<uid>
  border_id <- function(...) { ... }                  #                              txb<uid>
  xf_id     <- function(fname,fsize,fbold,fcolor,fill, bt,bb,bl,br, ah,av,aw,ar) { ... }  # txx<uid>
  list(xf_id = xf_id)
}

xl_apply_styles <- function(wb, s, styles, reg) {     # thin apply loop
  for (i in seq_len(nrow(styles))) {
    r <- styles[i, ]; if (is.na(r$dims)) next
    wb$set_cell_style(sheet = s, dims = r$dims,
                      style = reg$xf_id(r$fname, r$fsize, r$fbold, r$fcolor, r$fill,
                                        r$bt, r$bb, r$bl, r$br, <alignment...>))
  }
}
```

`tab_xl()` builds `reg <- xl_style_registrar(wb)` once, then `walk(plans, xl_write_table, reg)`;
`xl_write_table(wb, plan, o, reg)` forwards `reg` to `xl_apply_styles`.

The numFmt merging pass is unchanged and remains **per-cell safe**: `wb$add_numfmt()` forks a new
combined xf per cell and repoints that cell — it never mutates a shared xf node — so global xf dedup is
safe (verified: three sheets keep distinct per-sheet numFmt codes over the deduped xfs).

Design/`# WARNING:` tags added at `xl_style_registrar` and in the file header explaining the
first-match trap so the per-table reset cannot be reintroduced.

---

## 4. Verification against the 1.3.1 reference

**Tests** — `devtools::test(filter = "tab_xl|export-parity|export|xl-backend|render-html")`:
**6 files, 299 passed, 0 failed, 0 error, 0 warning**. No golden regeneration (behaviour of the
single-table path is unchanged; only the multi-table name space is fixed).

**Regenerated review workbook** (`…_FIXED.xlsx`, pc18) vs `review_tabxplor_1.3.1.xlsx`, read back cell by
cell:

| Check (all 15 sheets)                         | Buggy 1.4.0                         | Fixed 1.4.0            | 1.3.1 ref |
|-----------------------------------------------|-------------------------------------|------------------------|-----------|
| Header cell font                              | random (e.g. sheet 2 = **sz10**)    | **sz9 bold** everywhere | sz9 bold  |
| Header top/bottom border                      | offset / missing                    | **[thin, thin]** everywhere | [thin, thin] |
| Col A subtotal rows ("Total …")               | inconsistent                        | **sz10 bold, left**    | sz10 bold, left |
| Col A `row_var` level names                   | often **bold**                      | **non-bold**           | non-bold  |
| Subtext / legend rows                         | **sz10 BOLD coloured**              | **sz9 normal black**   | sz9 normal black |
| Title rows                                    | mis-styled                          | **sz12 bold**          | sz12 bold |

Across all 62 true subtext rows, FIXED matches 1.3.1 in 60; the 2 differences are both on sheet 04
(`pct="col"`) where **FIXED is the more consistent one** (sz9-normal legend / sz12-bold field-table
title vs 1.3.1's own quirks) — not regressions.

---

## 5. Secondary findings

### 5.1 Numeric `color = "diff"` colours — apply, but sparse by design (INTENDED)

On the collision-free first table (sheet 01, identical buggy vs fixed) the numeric column `nb_livres`
**is** coloured where the effect size is large — e.g. the row with mean 24.16 vs the overall 13.65 →
blue `0891C9`. Only **1 of 4** numeric data cells colour because 1.4.0 numeric `diff` colours **Glass's
Δ** (`diff / sd_ref`, `mean_diff_breaks` = 0.2/0.5/0.8/1.2 SD), a strict effect-size threshold. 1.3.1
coloured the numeric column on a **multiplicative ratio** (`mean_breaks` 1.15/1.5/2/4), which trips far
more often. This is the **intended Phase 5 change** (numeric `diff` is now a real, standardized
difference). The maintainer's `03 tabv_ratio` case (`color = "ratio"`) is the way to recover the old
multiplicative colouring for means. **Not a formatting bug** — the collision merely made it *also* look
absent on sheets 2+.

### 5.2 Factor colour hex differs from 1.3.1 (INTENDED palette; calc-review separate)

Factor `diff` colours the same *direction* as 1.3.1 but with **different hex** (e.g. `C38C46` vs 1.3.1
`FFB300`) — the Phase 5 palette rewrite. Which cells cross which break is a **calculation**-parity
question for the Phase 11 manual review (weighted estimates, break boundaries), **out of scope for the
11a formatting fix**, which only restores that colours are correctly *applied*.

### 5.3 `ci = "cell"` in Excel (REAL, separate; deferred per maintainer)

Sheets 10/11 (`ci = "cell"`): each cell gets numFmt **`@`** (Excel TEXT) and the raw `get_num()` value
(e.g. `0.232341…`), so Excel shows the **full-precision proportion, unscaled, no interval**.

- **Root cause**: `format(x, syntax = "excel")` returns `"TEXT"` for a composite cell-CI display
  (`pct [lo;hi]`), which `tab_xl` maps to `"@"`; but the value written is the raw **number**, not the
  interval string. The numeric bypass writes one raw number + one numFmt and cannot represent
  `"23% [19;28]"`.
- **Contained fix (deferred)**: in `tab_xl_plan_one()`, for `code == "TEXT"` cells write the **string**
  `format(col)` (the same text the console/kable show) instead of the raw number, keeping numFmt `"@"`.
  This makes `ci = "cell"` render its interval in Excel. Belongs with a later `tab_xl` display pass
  (Phase 13 revisits stars/interval padding across exporters).

---

## 6. Files changed / how to regenerate

- **Changed**: `R/tab_xl.R` — added `xl_style_registrar()`, rewrote `xl_apply_styles()` as a thin loop
  taking `reg`, threaded `reg` through `xl_write_table()` and the `tab_xl()` assembly walk; updated the
  file-header docstring + `# WARNING:` tags. No other file touched; no golden regeneration.
- **Regenerate the review workbooks** (the canonical 1.4.0 file was **locked open in Excel** during this
  session, so a `…_FIXED.xlsx` sibling was written for inspection):

  ```sh
  # close both files in Excel first, then:
  Rscript dev/manual_review_131_vs_140.R dev         # -> review_tabxplor_1.4.0-dev.xlsx (fixed)
  Rscript dev/manual_review_131_vs_140.R installed    # -> review_tabxplor_1.3.1.xlsx (reference)
  ```

  A backup of the pre-fix workbook is kept at `dev/review_manual/review_tabxplor_1.4.0-dev_BUGGY_backup.xlsx`.

---

## 7. Status

- **Primary formatting bug: FIXED and verified.** All borders, font sizes, first-column bold/alignment,
  and subtext now match the 1.3.1 reference across all 15 sheets; 299 export tests green.
- **Deferred (documented above):** `ci = "cell"` Excel text rendering (§5.3).
- **Out of scope for 11a (Phase 11 calculation review):** exact colour breaks / weighted-estimate parity
  vs 1.3.1 for factor and numeric columns (§5.1–5.2).
