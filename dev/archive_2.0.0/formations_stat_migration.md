# Migrating `formations_stat` from tabxplor 1.3.1 to 2.0.0

What will break in the university course material at `~/github/formations_stat` (36 root `.Rmd`) and `~/github/formations_stat/books/M2_CES_Multi` (12 chapters + 3 exam files) when tabxplor 2.0.0 replaces 1.3.1. This is a **map, not a fix**: every pattern gets a failure mode, a blast radius and a verdict — either tabxplor should change before it ships, or the courses should be ported.

⚠ This is a one-off migration audit, not an eighth standing `dev/` guide. Once the port is done it belongs in `dev/archive_2.0.0/`.

---

## 1. Scope and method

**Scanned.** All 36 root `.Rmd`, the four sourced scripts (`R/setup.R`, `R/webex.R`, `R/logit_functions.R`, `R/set_RStudio_prefs.R`), the M2 book's 12 chapters plus `Examen_2025/`, both `_bookdown.yml`, `_output.yml`, `style.css` and `resources/tab.css`.

**Duplication caveat — raw counts are inflated.** `L3S2_02.Rmd` and `L3S2_03.Rmd` are byte-identical; `L3S2_04.Rmd` ≈ `M1S1_02.Rmd`; `L3S2_01.Rmd` ≈ `M1S1_01.Rmd` ≈ `M2_01.Rmd`; `M1S1_03.Rmd` ≈ `M2_03.Rmd`; `M2_06_07.Rmd` ≈ `books/M2_CES_Multi/01-Logits.Rmd`; `M1S2_01/02/03` ≈ `M2_04/05` ≈ `RestesM1.Rmd`. And `books/M2_CES_Multi/_main.Rmd` is a bookdown merge artifact — every construct in it is a second copy. **Counts below are raw occurrences; the number of distinct edits is roughly a third of them.**

**Verified how.** 1.3.1 is read from git commit `b812c5f` ("v 1.3.1"); there is no `v1.3.1` tag. 2.0.0 is the `dev` working tree. Every claim below was checked by reading both sources, never by running the courses. Claims that genuinely need a render to settle are collected in §13 rather than asserted.

**`books/M1_Demo_AGD_2024`** is out of the requested scope but shares the `mca_interpret()` + `kableExtra::remove_column()` pattern of §5.5. It will stay frozen because it’s legacy, superseded by the other geometrical data analysis course.

---

## 2. Reading key

| Class | Meaning                                                                                 |
|-------|-----------------------------------------------------------------------------------------|
| **E** | Hard error — the chunk aborts, the document does not knit                               |
| **N** | Silent no-op — the code runs and no longer does what it used to                         |
| **#** | Silent number change — different figures, no message                                    |
| **F** | Formatting or structure change — visible, usually an improvement, but prose may now lie |
| **P** | Prose drift — the course text describes something tabxplor no longer prints             |

| Verdict     | Meaning                                                                      |
|-------------|------------------------------------------------------------------------------|
| **FIX-PKG** | tabxplor's problem; changing the courses would be papering over a regression |
| **PORT**    | Adapt the courses — the 2.0.0 behaviour is the intended one                  |
| **ACCEPT**  | Nothing to do; the change is an improvement and breaks nothing               |

---

## 3. Executive map

Ranked by how much work each pattern represents, not by count.

| #    | Pattern                                                                  | Class | Raw sites              | Verdict                             |
|------|--------------------------------------------------------------------------|-------|------------------------|-------------------------------------|
| §5.1 | bookdown `(\#tab:x)` cross-references die — no `<caption>` element       | E/F   | whole M2 book          | **FIX-PKG**                         |
| §9   | `tab_logit()` / `multi_logit()` framework                                | E     | 148                    | **PORT** (replace with `tab_reg()`) |
| §5.2 | `fill_table()` HTML string surgery                                       | E/N   | 19 exercises           | **PORT**                            |
| §4.4 | `tab_plain(num = TRUE, df = TRUE)` returns a tibble, not a `data.frame`  | E     | 43                     | **PORT**                            |
| §5.3 | The CSS contract — vendored `tab.css` and `style.css` blocks inert       | F     | all docs               | **PORT**                            |
| §4.1 | `kable_tabxplor_style()` is defunct                                      | E     | 16 live                | **PORT**                            |
| §4.3 | `tabxplor:::get_chi2()` removed with no shim                             | E     | 4 live, 39 total       | **FIX-PKG**                         |
| §4.5 | `select(-n)`, `arrange(is_totrow(n), …)` — the `n` column is gone        | E     | ~20                    | **PORT**                            |
| §6.1 | `options(tabxplor.compact = TRUE)` is dead — taught verbatim to students | N     | ~18 files              | **PORT**                            |
| §5.4 | `tab_kable(position = )` silently swallowed                              | N     | 519                    | **FIX-PKG**                         |
| §7.1 | `method_diff` `"ac"` → `"newcombe"`, mean CIs → Welch                    | #     | every `after_ci` table | **ACCEPT**                          |
| §4.2 | `tab_plot()` is defunct                                                  | E     | 5                      | **PORT**                            |
| §6.2 | `options(tabxplor.ci_print)` is dead                                     | N     | ~30                    | **PORT**                            |
| §4.6 | `fmt(type = "OR")`, `fmt0(type = )`, `$rr`, `mutate(rr = )`              | E/N   | logit only             | **PORT**                            |
| §4.7 | Reserved level names `"Total"` / `"Ensemble"` now abort                  | E     | unknown                | **PORT**                            |
| §7.2 | `color = "auto"` / `TRUE` gained a background channel                    | # / F | ~10                    | **ACCEPT**                          |
| §7.4 | Numeric `row_vars` banded instead of exploded                            | F     | a few                  | **PORT**                            |
| §8   | ~15 deprecated-but-working spellings                                     | F     | ~2000                  | **PORT** (teaching)                 |
| §10  | ggfacto: modernised version uncommitted                                  | F     | 44                     | **PORT** (install first)            |
| §11  | Prose drift                                                              | P     | many                   | **PORT** (flag now, rewrite later)  |

---

## 4. Hard errors

### 4.1 `kable_tabxplor_style()` — defunct

```r
tabxplor::kable_tabxplor_style(position = "center") |>
  kableExtra::column_spec(2:5, background = "rgba(255, 61, 0, 0.2)")
```

`R/tab_classes.R:769-770` — `lifecycle::deprecate_stop("2.0.0", "kable_tabxplor_style()", "tab_html()")`. It aborts, it does not warn.

**16 live call sites**, all of the shape `kable_tabxplor_style(wrap_cols = 500)` or `kable_tabxplor_style(wrap_rows = Inf, wrap_cols = Inf, position = "left")`: `M1S2_01.Rmd:824, 1178, 1204, 1226`; `M1S2_03.Rmd:291, 309, 325, 367`; `M2_05.Rmd:893, 1247, 1273, 1295`; `L3S2_01.Rmd:1763, 2058`; `L3S2_02/03.Rmd:930, 1222`; `M1S1_01.Rmd:930, 1222`; `M2_01.Rmd:1261, 1593`; `M1S1_02.Rmd:2452`; `L3S2_04.Rmd:2452`.

**Port.** Replace with `tab_kable()` / `tab_html()`. `wrap_rows` and `wrap_cols` carry over unchanged (§5.6); `position` does not exist any more (§5.4) but is a no-op in practice for `"left"`.

**Verdict: PORT.** The replacement is a rename, and 1.3.1's function genuinely had no shared machinery with the new engine.

### 4.2 `tab_plot()` — defunct

`R/tab_classes.R:1438-1439` — `deprecate_stop("2.0.0", "tab_plot()", "tab_export()")`.

Five sites: `M1S1_04.Rmd:458`, `M2_03.Rmd:1913, 2287`, plus prose at `M2_04.Rmd:833` and `RestesM1.Rmd:828`. The course already warns students it is buggy (`M1S1_04.Rmd:457`: *"cette fonction n'est pas conseillée car elle a quelques bugs d'affichage"*).

**Port.** `tab_export(x, format = "plot")`, or drop the two live `ggsave()` examples — they exist to demonstrate exporting a table as an image, which `tab_xl()` and `tab_html()` already cover better.

**Verdict: PORT.**

### 4.3 `tabxplor:::get_chi2()` — removed with no shim

```r
tabxplor:::get_chi2(vartab1) %>%
  select(-row_var) %>%
  filter(!`chi2 stats` %in% c("cells", "count")) %>%
  knitr::kable() %>% kableExtra::kable_classic(html_font = "DejaVu Sans")
```

`grep -rn "get_chi2" R/ NAMESPACE` in 2.0.0 returns **nothing**. The replacement is `get_test()` (`R/tab_classes.R:113`), reading the `test` attribute — and `attr(tabs, "chi2")` is now always `NULL`, because `new_tab(chi2 = )` folds its value into `test` (`R/tab_classes.R:63`).

The schema is not compatible. 1.3.1 returned a wide frame with a `chi2 stats` column taking the values `tables` / `pvalue` / `df` / `cells` / `variance` / `count`. 2.0.0's `new_test_tibble()` (`R/tab_classes.R:340-362`) returns a long/tidy frame: `var`, `col`, `test`, `statistic`, `df1`, `df2`, `pvalue`, `n`, `min_e`, `effect_size`, `es_type`, `pvalue_exact`, `deff`, `outcome`, `col_group`.

| 1.3.1 `chi2 stats` | 2.0.0                                                                 |
|--------------------|-----------------------------------------------------------------------|
| `pvalue`           | `pvalue`                                                              |
| `df`               | `df1` (plus `df2` for F tests)                                        |
| `count`            | `statistic`                                                           |
| `cells`            | not carried — `n` is the raw base, `min_e` the minimum expected count |
| `variance`         | not carried — `effect_size` / `es_type` instead                       |

**Live sites: 4** — `04-AC.Rmd:235, 261` and `M2_02.Rmd:2422, 2461`. The `04-AC.Rmd` pair is a teaching demo about chi-squared variance decomposition built from hand-made `fmt()` tables (`04-AC.Rmd:226-233`, `:252-259`), and it is exactly the `variance` row that is no longer carried. 35 further sites are commented out.

**Port.** The demo can be rebuilt from `get_test()` plus `get_ctr()` (per-cell contributions, `R/fmt_class.R:2881`), which is where the variance decomposition actually lives now. That is a rewrite of the teaching, not a rename.

**Verdict: FIX-PKG (weak).** `get_chi2()` was internal, so tabxplor owes it nothing — but it was the *only* programmatic access to the test in 1.3.1, 39 sites in one corpus reached for it, and removing it with neither a shim nor a `NEWS.md` line naming it is harsher than any other removal in the release. A one-line deprecated wrapper mapping the four surviving rows would cost nothing. See §12.6.

### 4.4 `tab_plain(num = TRUE, df = TRUE)` — precedence flipped

```r
tab_plain(FES2017, CSER, PR2017ALL1, wt = w5, num = TRUE, df = TRUE) |> FactoMineR::CA()
```

Both versions gate on `if (df || num)`, then choose. **They choose differently.**

1.3.1, `b812c5f:R/tab.R:2328`:

```r
if (df) return(as_df_merge_rownames(tabs, rlang::as_name(row_var)))
```

2.0.0, `R/tab.R:2626-2634` (`leaf_extract_raw()`):

```r
leaf_extract_raw <- function(result, num, row_var) {
  fmt_cols <- names(result)[purrr::map_lgl(result, is_fmt)]
  nums <- dplyr::mutate(result, dplyr::across(tidyselect::all_of(fmt_cols), get_num))
  if (num) return(nums)
  out <- as_df_merge_rownames(data.table::as.data.table(nums), rlang::as_name(row_var))
  ...
}
```

`df = TRUE, num = TRUE` returned a `data.frame` **with row names** in 1.3.1 and returns a **tibble with the row_var still as a column** in 2.0.0. `FactoMineR::CA()` cannot take that: a tibble carries no row names, and the first column is a factor.

**43 sites**, every one of them feeding `CA()`: `04-AC.Rmd:374, 379, 418, 863, 975, 1075`; `08-Explo.Rmd:404, 414, 452, 468, 482, 501, 517, 580, 591`; plus the commented data-prep block in ~22 root files.

**2.0.0 already has the better answer.** The `as.matrix()` / `as.table()` methods (`R/tab_classes.R:1822-1839`, Phase 24e) hand a table to base R directly: `totals = FALSE` by default drops the Total row and column *and* the display-time n / pct / p-value rows, every cell contributes the number it shows, and the label columns become dimnames.

```r
# was: tab_plain(FES2017, CSER, PR2017ALL1, wt = w5, num = TRUE, df = TRUE) |> CA()
as.matrix(tab(FES2017, CSER, PR2017ALL1, wt = w5)) |> FactoMineR::CA()
```

`as.table()` gives the same matrix with its `dimnames` named after the two axes' variables.

**Verdict: PORT.** The `df` / `num` flags were 1.3.1's escape hatch; `as.matrix()` is the supported one.

### 4.5 The `n` column no longer exists in the object

1.3.1's `add_n = TRUE` (the default) added a real `n` **column** under `pct = "row"` and an `n` **row** under `pct = "col"` (`b812c5f:R/tab.R:1176-1188`). 2.0.0 replaces it with `n = "range"` (`options(tabxplor.n)`, `R/tab-options.R:110-117`) and draws the base count **at display time**, folded into the Total cell (`100% (9 838)`).

A shim reconstructs it — but only on three access paths (`tabxplor_deprecated_column()`, `R/tab_classes.R:260-278`):

| Expression                              | 2.0.0                                    |
|-----------------------------------------|------------------------------------------|
| `tabs$n`                                | ✓ works, soft-deprecation message        |
| `tabs[["n"]]`                           | ✓ works                                  |
| `dplyr::pull(tabs, n)`                  | ✓ works                                  |
| `dplyr::select(tabs, -n)`               | ✗ **error** — no shim on the select path |
| `dplyr::arrange(tabs, is_totrow(n), …)` | ✗ **error** — see below                  |
| `dplyr::filter(tabs, is_totrow(n))`     | ✗ **silently returns zero rows**         |

The mask cases are the nasty ones. dplyr builds its data mask with low-level `.subset2()`, which does **not** dispatch on `$.tabxplor_tab` / `[[.tabxplor_tab`. So a bare `n` inside `filter()`/`arrange()` resolves to the *function* `dplyr::n`; `is_totrow()`'s default method is `rep(FALSE, length(x))` (`R/fmt_class.R:785`), and `length(<closure>)` is 1 — so `filter()` gets a size-1 `FALSE` it happily recycles and **drops every row with no warning**, while `arrange()` aborts because a sort key must be size `nrow`.

Sites: `select(-n)` in `R/setup.R:200, 203` (inside `random_table_from_tab()`), `M1S1_02.Rmd:1862`, `L3S2_04.Rmd:1862`, `M2_02.Rmd:2263`, `L3S2_06_Examen_A.Rmd:591, 602, 615`. `arrange(is_totrow(n), desc(...))` in `M1S2_02.Rmd:1344, 1354, 1465, 1501`, `RestesM1.Rmd:1820, 1830, 1941, 1977`, `M2_04.Rmd:1996, 2006, 2117, 2153`. `select(-wn)` at `05-ACM.Rmd:207`.

`filter(!is_totrow(Total))` and `filter(Total$n >= 30)` are **fine** — the Total column is real and carries the `n` field.

**Port.** Delete the `select(-n)` (there is nothing to remove), and replace `is_totrow(n)` with `is_totrow(Total)` or the declared field read `get_row_kind(...) == "data"`.

**Verdict: PORT.** The `n` column going away is the headline design change of the release and `NEWS.md` says so.

### 4.6 `fmt(type = "OR")`, `fmt0(type = )`, `$rr`, `mutate(rr = )`

Four related failures, all in `R/logit_functions.R` and the two logit chapters. Detail in §9; the mechanisms:

- **`fmt(0, type = "OR", …)`** — `R/logit_functions.R:1497`. `fmt_type_legacy()`'s allow-list is exactly `c("row", "col", "all", "all_tabs", "mean", "n", "coef")` (`R/tab-deprecate.R:268`) and anything else is a `cli_abort`. `"OR"` was never in 1.x's list either — this call worked because 1.3.1's `fmt()` did not validate `type`. **Error.**
- **`tabxplor:::fmt0("pct", type = "row")`** — `R/logit_functions.R:1326, 1345`. The 2.0.0 signature is `fmt0(display = "n", digits = 0, scale = "level_n")` with no `...` (`R/fmt_class.R:2823`). **Error: unused argument.**
- **`mutate(., rr = round(…))`** — `01-Logits.Rmd:724`, `M2_06_07.Rmd:780`, and the `or = round(rr/first(rr), 2)` that follows at `:796` / `:852`. The field is `ratio` now. `mutate.tabxplor_fmt()` mutates the `vec_proxy()` data frame directly (`R/fmt_class.R:4338-4345`), so this **succeeds**, appends a dead 22nd field nothing reads, and `set_display("rr")` then displays the real (all-`NA`) `ratio` field. **The odds columns print empty**, and the inline answers derived from them (`M2_06_07.Rmd:783-784`) become `NA`. A record carrying an extra field will also fail to `c()` / `bind_rows()` with a normal `fmt`.
- **`$rr` as a read** — errors. `R/fmt_class.R:4356` carries a comment describing `$rr` as a read-side alias of `$ratio`, but `$.tabxplor_fmt` has branches only for `wn`, `ci`, `tot_wn` and `in_totrow` before falling through to `dplyr::pull(vec_proxy(x), name)`. See §12.4.

**Verdict: PORT**, except the `$rr` alias (§12.4, FIX-PKG).

### 4.7 Reserved level names now abort

`lvl_check_reserved()` (`R/row-model.R:247-267`) is new in 2.0.0 and **aborts** — it does not warn — when a factor or character variable used as `row_vars` / `col_vars` / `tab_vars` carries a level named:

- **`"Total"`** — always, even if every total-name slot is renamed. It is appended literally (`R/row-model.R:237`) because it is the leaf's pre-rename key.
- **`"Ensemble"`** — the default `tab` slot (`R/tab-options.R:147-148`, `c(row = "Total", col = "Total", tab = "Ensemble", other = "Others")`).

The check is **case-sensitive** (plain `intersect()`), looks at all *declared* factor levels including unused ones, and runs at the end of `tab_prepare()` and again after `shape_apply()` — so it also catches a collision that `cleannames = TRUE` or `other_if_less_than =` created.

`"Autres"` and `"Others"` are **not** reserved — the `other` slot is deliberately excluded from the data list.

**Risk, not a confirmed break.** These are French survey data sets (`pc18`, `pe22`, `pa17`, `FES2017`, `ee2013_20`) where a level named `"Ensemble"` is entirely plausible, and `other_level = "Autres"` is already used. It cannot be settled without the data.

**Diagnostic before porting** — run once per data set:

```r
purrr::iwalk(dplyr::select(pc18, where(is.factor), where(is.character)), \(x, nm) {
  bad <- intersect(if (is.factor(x)) levels(x) else unique(x), c("Total", "Ensemble"))
  if (length(bad)) cli::cli_inform("{nm}: {bad}")
})
```

**Verdict: PORT** if it fires — the abort message already names the cure (`options(tabxplor.total_names = c(tab = "..."))`).

### 4.8 `tab_prepare(rare_to_other = , n_min = )` — was already broken

```r
data <- tab_prepare(data, AGE_REVENU, PR2017ALL1, rare_to_other = TRUE, other_level = "Autres")
```

`rare_to_other` **has never existed** in either version, and `n_min` was never a `tab_prepare()` argument. Both fall into `...`, which is the tidy-select of variables (`R/tab.R:2375-2377`) — so `n_min = 30` is evaluated as a *column position* and `rare_to_other = TRUE` recycles to "all columns". This was silently wrong in 1.3.1 too; 2.0.0 just fails at a different point.

Three sites: `04-AC.Rmd:1072`, `08-Explo.Rmd:577, 588`. Note `04-AC.Rmd`'s prose at `:1067` documents three arguments the code does not pass, and `08-Explo.Rmd` uses `other_if_less_than = 30` instead — the two were already inconsistent.

`tab_prepare()` itself is soft-deprecated in 2.0.0 (`R/tab.R:2365-2369`); `other_if_less_than`, `other_level` and `n_min` are all `tab()` arguments now, `n_min` being a real display filter.

**Verdict: PORT.** Drop `tab_prepare()`, move the arguments onto `tab()`.

---

## 5. Rendering and HTML

This is the largest cluster, because 2.0.0 replaced `knitr::kable() + kableExtra::kable_classic()` with a hand-written, dependency-free `<table>` string builder (`R/tab-render-html.R`). Nothing about the emitted markup is the same.

### 5.1 bookdown table cross-references die

**The single most damaging finding, and the one tabxplor should fix.**

The M2 book and several root files caption tables with a bookdown label baked into the string:

```r
tab_kable(caption = "(\\#tab:musiques-revenu) Pratiques musicales selon le revenu")
```

then cross-reference them with `\@ref(tab:musiques-revenu)`.

bookdown resolves those in `parse_fig_labels()`, and for `type == "tab"` the rule is:

```r
tab = {
  if (length(grep("^\\s*<caption", content[i - 0:1])) == 0) next
  labs[[i]] = sprintf("<span id=\"%s\">%s</span>", lab, label_prefix(type, sep = ": ")(num))
}
```

**It requires the current line, or the one before it, to start with `<caption`.** 1.3.1 satisfied that: kableExtra emitted a real `<caption>` element on its own line.

2.0.0 does not. `R/tab-render-html.R:400-404`:

```r
# the title is a `<div>` sibling before the <table>, not a `<caption>` child -- a caption
# participates in the table's width. See R/tab-css.R (.tabxplor-caption).
cap <- if (!is.null(caption) && length(caption) && nzchar(caption)) {
  paste0('<div class="tabxplor-caption">', tx_html_escape(caption), '</div>')
```

and the whole table is one line — `paste0(cap, '<table class="', tbl_class, '">', …)` at `:416-423`, prefixed by `<style>…</style>\n` in `tab_kable_join()` (`:487`).

So bookdown hits `next`. **Every captioned table keeps the label in its visible title — as `(#tab:musiques-revenu)`, pandoc having stripped the backslash on the way — gets no "Table 3.2:" number, registers no anchor, and every `\@ref(tab:…)` renders as `??`.**

Affected: every `caption =` and every `tab.cap=` chunk option in the corpus — `R/setup.R:90, 95` (so all 19 `fill_table()` calls), `M1S2_01.Rmd` and `M2_05.Rmd`'s explicit `(\\#tab:…)` captions, and the `tab.cap=` chunks at `M1S1_02.Rmd:705, 721, 2299, 2303, 2325, 2341, 2358, 2373` and twins.

**Note the design reason is real** — a `<caption>` participates in the table's width, and a long French title would stretch a narrow table. But the two goals are not in conflict: `<caption>` accepts `white-space: normal` and a `width` of its own, and `.tabxplor-caption` already sets exactly that (`R/tab-css.R:399`).

**Verdict: FIX-PKG — DONE (Phase 24g).** Under bookdown the title is a real `<caption>` now, so `(\#tab:x)` resolves, the anchor lands and `\@ref(tab:…)` numbers again — verified on a rendered document. Everywhere else the `<div>` stays. See §12.1.

### 5.2 `fill_table()` — the HTML string surgery

`formations_stat/R/setup.R:88-152` builds the interactive fill-in-the-blank tables: it renders with `tab_kable(tooltips = FALSE, popover = FALSE, wrap_rows = Inf, wrap_cols = Inf, caption = caption)`, then dissects the HTML as a string and splices `webexercises::fitb()` inputs into selected cells.

**All eight of its assumptions are now false.**

| `fill_table()` assumes                 | 2.0.0 emits                                                |
|----------------------------------------|------------------------------------------------------------|
| the table is the first `< *table` line | a `<style>` block and/or a caption `<div>` come first      |
| splitting on `">"` is safe             | an unescaped `<0.01%` in chi-squared rows; `<br>`; `<svg>` |
| one `<tr>` in the header               | up to **three** — span row, level row, unit row            |
| every value is in a `</span>`          | almost none are — bare text in the `<td>`                  |
| the last `<tr>` is the Total row       | it is the `<tfoot>` legend row                             |
| `<td>` counts are stable per row       | label columns are `rowspan` cells, first row only          |
| reference cells carry `ref:`           | only under `display = "diff"`                              |
| `text-align:right` is inline           | it is the class `tx-r`, defined in the CSS                 |

Detail on the three that are least obvious. A `<span>` now appears only for a composite cell's aside, a background-channel pill, a publication-palette mark and the legend swatches — so on a default `pct = "row"` table the `</span` mask matches the Total column alone. The `ref:` token still exists (`R/fmt_class.R:3909`) but is gated on the cell's display literally being `diff`; the default shows a plain `%` and puts `ref` in the tooltip. And the unit header row shows the type tag (`<n>`, `<row%>`), which is itself unescaped text containing `<`.

Representative 2.0.0 cell, from `tests/testthat/_snaps/tab-render-html.md`:

```html
<td class="tx-r tx-num g1" data-toggle="tooltip" data-container="body"
    data-placement="auto right" title="diff: +3% ; ratio: x1.29 ; n: 2">12%</td>
```

Re-classing with `` `class<-`(c("kableExtra", "knitr_kable")) `` still "works" (the `format = "html"` attribute survives) but drops the `tabxplor_kable` class and with it the `tabxplor_theme` attribute and the `knit_print` method that carries the jQuery/Bootstrap dependencies into the document.

**19 exercises**: `L3S2_04.Rmd` and `M1S1_02.Rmd` (4 each), `M2_06_07.Rmd` and `01-Logits.Rmd` (3 each), one each in `L3S2_01/02/03.Rmd`, `M1S1_01.Rmd`, `M2_01.Rmd`, `M2_02.Rmd`.

**The supported path exists.** `tab_html(get_data = TRUE)` (`R/tab_classes.R:684`) returns the render model instead of a string — roles, faces, references, per-cell text — which is exactly what `fill_table()` reconstructs by regex. Rewriting against that is more work than patching the regexes but it is the only version that will not break again.

**Verdict: PORT.** String-parsing another package's HTML was never a contract.

### 5.3 The CSS contract

`R/setup.R:45-50` is the load-bearing rendering setup:

```r
options("tabxplor.kable_popover" = TRUE)
options("tabxplor.cleannames"    = TRUE)
options("tabxplor.always_add_css_in_tab_kable" = FALSE)
# # To do when tab.css changes in tabxplor
# file.copy(system.file('tab.css', package = 'tabxplor'), 'resources/tab.css', overwrite = TRUE)
```

CSS injection is disabled and tabxplor's stylesheet is instead vendored into `resources/tab.css`, loaded from every root file's YAML `css:` field. **Three things happen at once:**

1. **`tabxplor.always_add_css_in_tab_kable` is gone** — zero hits in 2.0.0's `R/`. Its successor is `tabxplor.tab_kable_css` (alias `tabxplor.kable_css`, argument `css`), and it **defaults to `TRUE`** (`R/tab-options.R:294-297`). So the setup line is a no-op and every `tab_kable()` call now prefixes its own full `<style>` block (`R/tab_classes.R:703, 739`; `R/tab-render-html.R:487`). A document with 40 tables repeats the stylesheet 40 times.
2. **The vendored `resources/tab.css` is inert.** Its selectors are `.lightable-classic caption`, `.lightable-classic tfoot`, `.lightable-classic table tbody tr td`, `.lightable-classic table thead tr th` — and `lightable-classic` appears nowhere in 2.0.0's output. Only its `.popover*` rules still match anything. (2.0.0 still *ships* `inst/tab.css`, unchanged, but no code reads it — the `htmltools::includeCSS(system.file("tab.css"))` call is gone.)
3. **`style.css` has the same dead block.** `style.css:493-600` re-declares the `.lightable-classic` selectors under `/* tabxplor's kable tables */`, including the caption's `font-size: 14px; font-weight: bold; font-style: italic; text-align: left !important` and the DejaVu font stack. All inert. The gitbook `.page-content table tbody tr td { … text-align: left }` overrides at `:724-770` appear to sit inside a comment block — **verify before assuming they are not fighting `.tabxplor-tab .tx-r`**.

**The net effect is not "tables lose their styling"** — they gain tabxplor's own, correct stylesheet, 40 times over. What is lost is the site-specific tuning: the caption typography, the DejaVu font, the tfoot size, the padding.

**Port, in one edit.** Replace the vendored file with the generator, once per document:

```r
options(tabxplor.tab_kable_css = FALSE)     # in R/setup.R
```

```r
# a chunk near the top of each document, results = "asis"
cat(tabxplor::tab_css())
```

then move any surviving site tuning onto `.tabxplor-tab` / `.tabxplor-caption` in `style.css`, after that block (tabxplor's own rules are `!important`-free by design, so a later rule wins).

**Verdict: PORT.**

### 5.4 `tab_kable(position = )` is silently swallowed

`tab_html()`'s formals (`R/tab_classes.R:677-690`) have no `position`. It is not in `TX_INERT_EXPORT_ARGS` either (`R/utils.R:188-195`, which lists only `color_type`, `html_24_bit`, `engine`, `html_font`, `full_width`), so it lands in `...` and is dropped with **no error and no warning**.

**519 raw sites** (`position = "left"` 504, `position = "center"` 15) — by count, the most frequent single change in the corpus.

**Mostly harmless in practice.** 1.3.1's kableExtra default was `position = "center"` with `margin-left:auto; margin-right:auto`, so the courses pass `"left"` to override it. 2.0.0's table has `margin:0` (`R/tab-css.R:384`) and is left-aligned by default — so `position = "left"` becoming a no-op produces the same look. Only the 15 `position = "center"` calls actually lose something.

**But the silence is the problem.** `tab_html()` calls `tx_deprecate_inert()` and then **discards its return value** (`R/tab_classes.R:691` is a bare call), and `...` is referenced nowhere else in the body. Same pattern in `tab_md()` (`R/tab_md.R:84`) and `tab_xl()` (`R/tab_xl.R:161`). So *any* unknown argument — a retired one, a typo, `position`, `n_min` — is accepted and ignored. The comment at `R/utils.R:186-187` claims *"a real typo still errors at the leaf"*, which is only true of `tab_export()`.

**Verdict: FIX-PKG — DONE (Phase 24g).** All five exporters route their `...` through the same `tab_check_dots()` the producers use: `position` warns as a retired name, a typo aborts with a suggestion. See §12.3.

### 5.5 kableExtra post-processing of tabxplor output

Three patterns, three outcomes.

**`tab_kable() %>% kableExtra::column_spec(...)`** — `M1S1_02.Rmd:782`, `L3S2_04.Rmd:782`, `M2_02.Rmd:1226`, plus the `kable_tabxplor_style()` chain at `M1S1_02.Rmd:2452-2454`. The class guard passes (`knitr_kable` survives, `R/tab-render-html.R:488`) so it dispatches, but it will not do the right thing:

- With `css = TRUE` the string starts with `<style>…</style>`, which libxml2 hoists into `<head>` — kableExtra's `xml_child(xml_child(read_html(x), 1), 1)` then finds the `<style>` node, not the `<table>`.
- With a caption, `<div class="tabxplor-caption">` is a second root-level sibling, so the `<table>` is body child 2.
- Positional column indexing is wrong anyway: `rowspan` label cells are absent from continuation rows, and `<thead>` row 1 is a `colspan` span row with fewer cells than columns.
- `border_right = TRUE` writes the inline *shorthand* `border-right:1px solid`, which resets `border-right-color` to `currentColor` — i.e. the cell's own palette hex. `R/tab-css.R:13-15` documents exactly this hazard and locks against it in tests.

**`kableExtra::remove_column(c(1, 8))` on `mca_interpret()`** — `05-ACM.Rmd:887, 943, 987, 1032`. **Unaffected.** `mca_interpret()` touches no tabxplor code; it is a pure dplyr + `kableExtra::kable()` pipeline and still yields 8 columns.

**`knitr::kable() |> kableExtra::kable_classic()` on plain data frames** — the exam scoring grids and the chi-squared side tables. **Unaffected**, they never were tabxplor objects. But note the chi-squared ones are fed by `get_chi2()` (§4.3).

**Verdict: PORT** for `column_spec()` (drop it; the borders it draws are `tab_css()`'s job now), **ACCEPT** for the rest.

### 5.6 What still works, unchanged

- **`wrap_cols` / `wrap_rows`** — same formals, same meaning, same `Inf` early exit (`R/tab_classes.R:685, 1477`). Defaults moved from 1.3.1's `35`/`15` — identical, in fact. Every one of the 100-odd calls carries over. Note `wrap_cols` breaks compound names at `_`, `.`, `*` and camelCase seams now, not only whitespace.
- **`tooltips = FALSE, popover = FALSE`** — both still formals, both still read `options(tabxplor.tab_kable_tooltips)` / `options(tabxplor.kable_popover)` (`R/tab-options.R:299-307`). `R/setup.R:45`'s `kable_popover = TRUE` still works.
- **`caption =`** — still a formal, still falls back to the knitr `tab.cap` chunk option. Only its *markup* changed (§5.1).
- **`color_legend = FALSE`**, **`theme =`** — still formals.
- **`tab_kable()` itself** — a permanent, non-deprecated alias of `tab_html()` (`R/tab_classes.R:749`). But `tab_html` and more clear and should be prefered.
- **`tab_xl()`** — `sheets = "unique"`, `colwidth =`, `path =`, `replace =`, `font_text =` all still formals. Backend moved `openxlsx` → `openxlsx2`, invisibly. `tab_xl(list(tab1, tab2))` still works.
- **`results = 'asis'` is still not needed** for tables; `knit_print` methods are registered for `tabxplor_kable`, `tabxplor_tab`, `tabxplor_grouped_tab` and `tabxplor_tabs`. It *is* needed for the new `cat(tab_css())` chunk (§5.3).
- **`tabxplor:::unbrk` and `tabxplor:::sigma_sign`** — character constants in **both** versions (`b812c5f:R/utils.R:1061-1062` vs `R/utils.R:690-691`); only the escaping style changed. `nbsp()`/`padx()` in `R/setup.R:68-84` and the inline `` `r tabxplor:::sigma_sign` `` in prose keep working.
- **`%>%`** — tabxplor no longer re-exports magrittr, but `R/setup.R:42` loads tidyverse before tabxplor, and the student start-of-script block does too. No break.

### 5.7 New: Bootstrap and jQuery are injected into the document

`knit_print.tabxplor_kable()` attaches `tx_html_deps()` as knitr meta (`R/tab_classes.R:528-530`), which is `rmarkdown::html_dependency_jquery()` + `rmarkdown::html_dependency_bootstrap(theme = "cosmo")` + `inst/tabxplor-1.0/tabxplor.js` (`R/tab-render-html.R:510-521`).

The root files render through `webexercises::webexercises_default2` (Bootstrap-based, so probably deduped) but the M2 book renders through `bookdown::gitbook`, which is **not** Bootstrap-based. Injecting `bootstrap-cosmo` into a gitbook could restyle the whole book.

**Not verified — needs a render** (§13). If it bites, `tooltips = FALSE, popover = FALSE` sidesteps the JS but not the dependency, which is attached unconditionally.

---

### 5.8 Quarto — where the courses are heading

The maintainer plans to move the `.Rmd` material to `.qmd`, so the caption question has a second half. Measured on **Quarto 1.10.18 + knitr 1.51** by rendering real `.qmd` files, not read off the docs.

**Quarto never uses `<caption>` to resolve a cross-reference.** There is no analogue of bookdown's `^\s*<caption` scan. `#| label: tbl-x` and `#| tbl-cap:` become a Pandoc fenced Div *around* the cell output — `::: {#tbl-x .cell tbl-cap='…'}` — which Quarto's crossref filter turns into `<figure>` + `<figcaption>`. So the `<div class="tabxplor-caption">` design is free under Quarto, and the bookdown breakage does not reproduce. The `::: {#tbl-x}` wrapper (caption as the last paragraph inside the fence) is the fallback for arbitrary content; for a knitr cell it is redundant.

Four things a package emitting raw HTML tables must nevertheless get right, all verified:

1. **Quarto re-parses the table and restyles it.** By default `html-table-processing` injects Bootstrap classes — the emitted `class="tabxplor-tab"` comes back as `tabxplor-tab cell caption-top table table-sm table-striped small` — and **`table-striped`'s zebra fill fights colour-coded cells**. `<colgroup>` is dropped unless its `<col>` carry an explicit width; `<tr class="odd">` and `data-quarto-table-cell-role` residue are added. The library-author lever is `data-quarto-disable-processing="true"` on the `<table>`: package-side, needs nothing from the user, and — contrary to the docs' warning, which applies only to labels embedded *inside* the table HTML — **it does not break cross-references**; a cell `label:` rides on the div.
2. **Two captions if both sides write one.** With `tbl-cap` set, the output is `<figcaption>Table 1: …</figcaption>` followed by tabxplor's own `<div class="tabxplor-caption">`. Both render. A `knit_print` method can read `knitr::opts_current$get()[["tbl-cap"]]` and stand down. Note a bare `label: tbl-x` with no `tbl-cap` still numbers the table, so tabxplor's own title stays useful there.
3. **The raw-HTML fence is fragile at the first character.** Quarto's `patch.R` wraps asis output in a `{=html}` raw block only when the string matches `^<\w+[ >]` and ends `</\w+>\s*$`. Without the fence the HTML is parsed as markdown: `*x*` becomes `<em>`, `x^2^` a superscript, `@tbl-y` a live cross-reference link, and an opening `<div>` swallows the closing `:::`, downgrading the float to a subfloat captioned `(a) :::`. tabxplor is on the good path (`knit_print` → `asis_output`, string starts `<style>` or `<div`), but **it must never gain a leading HTML comment**, and `cat()` under `results: asis` is not equivalent.
4. **`htmltools::htmlDependency` works** — passing it as `asis_output(x, meta = )` copies the stylesheet to `_files/libs/` and links it once in `<head>`, which is also the answer to the per-table `<style>` repetition of §5.3.

**Detection**, exact and reliable: `!is.null(knitr::opts_knit$get("quarto.version"))` — a hard-coded sentinel Quarto sets in `share/rmd/execute.R`, and the very test `knitr:::is_quarto()` uses. It is `NULL` under `rmarkdown::render()`.

**Done (Phase 24g):** points 1 and 3 shipped — every `<table>` tabxplor emits carries `data-quarto-disable-processing="true"`, and the first-character invariant is a `# WARNING:` at `tab_kable_join()` plus a test. Point 2 shipped as the Quarto arm of the caption rule: with `tbl-cap` set, tabxplor emits no title of its own. ⚠ Point 1 reaches the html engine only — `tab_md()`'s table is generated by pandoc from a pipe table, so a Quarto document exporting Markdown still has it re-processed; `html-table-processing: none` is the user-side lever there.

**Not tested:** PDF/Typst output, `tbl-subcap` with several tables per chunk, and whether bookdown shares the markdown-in-HTML behaviour of point 3.

## 6. Silent no-ops

Code that runs, produces no message, and no longer does what it used to.

### 6.1 `options(tabxplor.compact = TRUE)` — dead, and taught verbatim

In 1.3.1 this option was real (`b812c5f:R/utils.R:62`) and did something visible: it was the default of `tab_many(compact = )` (`b812c5f:R/tab.R:630-631`), so setting it turned a multi-`row_vars` `tab_many()` from a **list of tables** into **one merged table** (`b812c5f:R/tab.R:1440-1443` → `tab_compact()`).

In 2.0.0 it is removed and simply unread — no `TAB_OPTIONS` row, no alias, zero hits in `R/`. **Setting it produces no error, no warning, and no effect.** `tab_many()` pins the legacy list shape itself (`R/tab-deprecate.R:227-229`), so a `tab_many()` call that used to yield one merged table now yields a list.

**This is the taught student boilerplate**, reproduced verbatim in ~18 files:

```r
options(tabxplor.cleannames = TRUE) # nettoyer les noms par défaut
options(tabxplor.print = "kable")   # affichage html (comme Jamovi)
options(tabxplor.compact = TRUE)    # tableaux multiples dans un seul tableau
```

`L3S2_05_Exam_blanc.Rmd:316-318`, `L3S2_06_Examen_A/B/C.Rmd:371-373`, `L3S2_06_Examen_R.Rmd:378-380`, `M1S1_04.Rmd:905-907`, `M1S1_05_Exam_blanc.Rmd:318-320`, `M1S1_05_Exam_blanc_correction.Rmd:294-296`, `M1S1_06_Exam_2025.Rmd:378-380`, `M1S2_01.Rmd:330-332`, `M1S2_02.Rmd:313-315`, `M1S2_03.Rmd:208-210`, `M1S2_04-Analyse de données.R:28-30`, `index.Rmd:226-228`, `07-Examen_blanc.Rmd:80-82`, `AC-Correction_examen_blanc.Rmd:325-327`, `Examen_2025/Examen_2025.Rmd:681-683`, `Examen_2025/Correction_2025.Rmd:152-154`. Plus the document-level `options(tabxplor.compact = TRUE)` at `M1S2_02.Rmd:56`, `M1S2_03.Rmd:57`, `Exam_M2_2024.Rmd:1115`, `M1S1_06_Exam_2025.Rmd:559`.

**The 2.0.0 answer is better than the option**: `tab()` merges several `row_vars` into one table **by default** (`output_list = FALSE`, `R/tab.R:149`). The teaching line becomes unnecessary rather than needing a replacement — which is the point of the release.

**Verdict: PORT.** Delete the line from the student block; where a merged table is wanted from `tab_many()`, either pass `compact = TRUE` (soft-deprecated to `output_list = FALSE`) or switch the call to `tab()`.

### 6.2 `options(tabxplor.ci_print)` — dead

Removed; zero hits in `R/`. In 1.3.1 it switched the `ci` display between the bracketed interval (`"ci"`) and the half-width (`"moe"`), read at render time (`b812c5f:R/fmt_class.R:1401`).

2.0.0 makes them **two tokens, neither reading an option** (`R/fmt_class.R:3715-3717`): `{ci}` is always `[lo;hi]`, `{moe}` always `±x`. For the 1.3.1 "value then interval" look, the layout is `display = "base_ci"` or `"base_moe"` (`R/tab-display.R:614-616`).

~30 sites, always toggled around a confidence-interval teaching chunk: `M1S1_02.Rmd:481, 719, 761, 985, 1107, 1112, 1201, 1206, 1278, 1312, 1432, 1665` and its twin `L3S2_04.Rmd`; `M2_02.Rmd:895, 1163, 1204, 1403, 1498, 1503, 1599, 1604, 1678, 1718, 1833, 2061, 3088`; `M1S1_03.Rmd:1078-1133` / `M2_03.Rmd:1433-1489`. Plus prose at `M1S2_01.Rmd:423, 425`, `M2_04.Rmd:728, 730`, `RestesM1.Rmd:722, 724`.

**Consequence.** Every chunk that set `"moe"` now prints the bracketed interval instead of `± x` — a **formatting change the surrounding prose describes**, so it is §11 material as well.

One improvement to note in passing: a 2.0.0 interval bound now carries the estimate's own glyphs, so a difference reads `[+35;+45]` and a ratio `[÷1.15;×1.75]` (`R/fmt_class.R:3363-3375`).

**Verdict: PORT.** Replace each toggle with an explicit `display = "base_ci"` / `"base_moe"` on the call.

### 6.3 `options(tabxplor.always_add_css_in_tab_kable = FALSE)` — dead

Covered in §5.3. Its successor defaults the other way, so the effect is inverted: CSS is now inlined per table where the course expressly turned it off.

### 6.4 `options(tabxplor.pvalue_lines = FALSE)` — never existed

Set in `R/logit_functions.R:1024, 1094` inside `withr::with_options()`. Zero hits in **either** version — this was always a no-op. Harmless: the `tab_many()` call it wraps passes no `chi2`/`test`, so no test rows are produced anyway.

### 6.5 `filter(if_all(1, ~ . != "pvalue"))` — matches nothing

`R/setup.R:198`, inside `random_table_from_tab()`.

In 1.3.1 `tab_many()` appended p-value rows to the returned object unconditionally (`b812c5f:R/tab.R:1446-1451` → `tab_pvalue_lines()`), and the row's label cell was the literal string `"pvalue"`. The filter genuinely removed a row.

In 2.0.0 `tab_pvalue_lines()` still exists (`R/tab_classes.R:1268`) but is **never called from the build** — its only call site is the `footer` entry of the materialisation step table (`R/tab_classes.R:994-1002`), gated on `ctx$pvalue`, which is `TRUE` only on the export path. The console print explicitly turns it off and renders the test as a separate grid (`R/tab_classes.R:399-401`).

So the filter is a no-op that does not error. And even on an exported table the label is now a descriptor — `"pvalue (Chi-2)"` (`R/tab-test-display.R:366-380`) — not the bare word.

**Port.** Delete it. The 2.0.0 idiom for "drop the synthetic rows" is `get_row_kind()` (exported; values `data` / `total` / `n` / `pct` / `pvalue` / `gof` / `blank`), or `!is_totrow(.)` for totals alone.

### 6.6 `mutate(fmt_column, rr = …)` / `mutate(fmt_column, ci = …)`

Covered under §4.6. Both succeed, both append a dead field, both leave the intended field untouched. Note the distinction that matters: the **`fmt()` argument** `ci =` is alive and well (a symmetric half-width converted to bounds, `R/fmt_class.R:364, 455-463`) and `$ci` still reads the half-width back (`get_ci()`, `R/fmt_class.R:2895-2897`). It is only the per-cell **field** named `ci` that no longer exists.

### 6.7 `tab_xl(n_min = , hide_near_zero = )`

Not used in this corpus, but worth knowing: both are absent from the formals *and* from `TX_INERT_EXPORT_ARGS`, so they are silently accepted and ignored despite `NEWS.md:157` describing them as "Removed (now an error)". Same root cause as §5.4.

---

## 7. Silent number changes

### 7.1 Confidence-interval methods changed

Two defaults moved, both in `R/tab-agg.R:219-220`:

- **difference of proportions**: `"ac"` (Agresti–Caffo) → **`"newcombe"`**
- **difference of means**: pooled-t → **`"welch"`**

Every interval width changes, therefore every significance verdict at the margin, therefore every `color = "after_ci"` shading and every star.

Blast radius: every table using `color = "after_ci"` (dozens — `M1S1_03.Rmd:1243`, `M1S1_02.Rmd:1435`, `M2_02.Rmd:2151`, `04-AC.Rmd:759, 765`, `RestesM1.Rmd:2420`, `L3S2_04.Rmd:1763, 1796`, and every exam `tab_many(color = "after_ci")`), plus `ci = "cell"` tables where `method_cell` was left at its default.

`method_cell = "wald"` is passed explicitly at `M1S1_02.Rmd:481`, `L3S2_04.Rmd:484` and `M1S1_02.Rmd:724` — those keep working (soft-deprecated to `ci_method = c(cell = )`, `R/tab-agg.R:253-280`) and keep their numbers.

**Verdict: ACCEPT.** Newcombe and Welch are the better estimators, and `NEWS.md` says so. The consequence for the courses is that any **hard-coded number in the prose** read off a 1.3.1 table may now be off by a decimal — see §11.

### 7.2 `color = "auto"` / `TRUE` gained a background channel

1.3.1: counts → `"contrib"`; row/col percentages → `"after_ci"` when `ci = "diff"`, else `"diff"`. **One text channel.**

2.0.0: row/col percentages → text = `difference` **and** background = `ratio` (`MEASURES$difference$auto_for` / `$ratio$auto_for`, `R/fmt_class.R:4973, 4998`). Counts → `contrib`, unchanged. The automatic CI gating is gone (it is `color_signif =` now).

Sites: `tab_pct(color = TRUE)` in `04-AC.Rmd:228-233, 254-259` and `R/setup.R:220`, plus `tab(color = TRUE)`.

**Verdict: ACCEPT** — a visual upgrade — but the tables gain a colour dimension the surrounding teaching text does not explain.

### 7.3 Colour break ladders moved

`COLOR_SCALES` (`R/tab_classes.R:2685-2720`) versus 1.3.1's `.onLoad()` (`b812c5f:R/utils.R:41-44`):

| Ladder             | 1.3.1                                       | 2.0.0                                       |
|--------------------|---------------------------------------------|---------------------------------------------|
| percentages        | `pct_breaks = c(0.05, 0.1, 0.2, 2, 0.3)`    | `pct_diff = c(0.05, 0.1, 0.2, 0.3)`         |
| percentages, ratio | — (the `2` rung above)                      | `pct_ratio`, on the background channel      |
| means              | `mean_breaks = c(1.15, 1.5, 2, 4)`, a ratio | `mean_diff`, standardised — the new default |
| means, ratio       | —                                           | `mean_ratio = c(1.1, 1.2, 1.5, 2)`          |
| contributions      | `contrib_breaks = c(1, 2, 5, 10)`           | `contrib`, unchanged                        |

The `×2` rung that sat inside 1.3.1's percentage *difference* ladder has moved to the ratio channel, where it belongs.

`set_color_breaks(pct_breaks =, mean_breaks =, contrib_breaks =)` still works and maps onto the new scales (warns). But `get_color_breaks("pct_ci")` and `("mean_ci")` now **error**.

**Consequence.** Mean columns are the ones that visibly change: they grade on a standardised-difference ladder now, not a ratio one. Affects `NB_MUSIQUES` / `NB_ARTS` style columns (`M1S1_04.Rmd:750`, `Exam_blanc_M2_2024_correction.Rmd:890`).

**Verdict: ACCEPT.**

### 7.4 Numeric `row_vars` are banded, not exploded

1.3.1 gave one row per distinct value. 2.0.0 applies `shape = "auto"` (`options(tabxplor.shape_auto_max) = 12L`, `R/tab-options.R:207-213`): ≤12 distinct whole values keeps one level per value, otherwise it bands at the mean and one SD either side.

**`M1S1_03.Rmd:700` — `tab(pe22, row_var = AGE)` — is a teaching example** whose whole point is the shape of the output. It will now show bands where it showed ages. Same at `M2_03.Rmd`'s twin.

Restore the old shape with `shape = "values_to_levels"`, or better, teach the new one — banding a continuous variable is the correct default and the course elsewhere spends pages explaining why you must recode `AGE` before crossing it.

**Verdict: PORT.**

### 7.5 Mean cells show `cv`, not `σ`

A numeric `col_var`'s default layout is now `mean_cv` — the coefficient of variation as a percentage — where 1.3.1 printed `mean (σsd)` (`R/tab-display.R:620-621`). Restore with `display = "mean_sd"`.

This has a direct prose consequence (§11): three files explain the parenthesised number as a standard deviation, spelling out `` `r tabxplor:::sigma_sign` `` in the sentence — `M1S1_04.Rmd:740`, `M2_02.Rmd:2921`, `M2_03.Rmd:2963`.

**Verdict: PORT** — pass `display = "mean_sd"` to keep the lesson, or rewrite the lesson.

### 7.6 What does *not* change

Checked deliberately, so the port does not chase phantoms:

- **`na = "drop"` arithmetic.** 1.3.1's `tab()` pre-filtered complete cases on `row_var` + `col_var` + `tab_vars` (`b812c5f:R/tab.R:348-351`); 2.0.0 drops the `NA` column and the `NA` rows inside the leaf, before totals (`R/tab-leaf.R:395-416`). For **one row_var × one col_var these are the same population**, cell for cell. And `tab_many()` gave a **per-col_var base in both versions** (documented at `b812c5f:R/tab.R:417-420`). The only case that changed is `tab(sup_cols = , na = "drop")`, where 1.3.1 restricted the supplementary columns to the main col_var's complete cases and 2.0.0 gives each its own base — 2.0.0 names the old behaviour `na = "common_base"`. **`sup_cols` is not used in this corpus.**
- **`ref`.** Every 1.3.1 form works: `1`, `2`, `"first"`, `"tot"`, a level name, `c(1, 1, 1, "tot", 1)`, a variable holding such a vector. Vectorised over `row_vars` in both. Matching is now **exact-first, regex-fallback** (`R/tab.R:2684-2685`) where 1.3.1 was regex-only — strictly more forgiving, and it fixes level names containing regex metacharacters.
- **`levels = "first"` / `"auto"`** — same three values, same code (`R/tab.R:1340-1375` vs `b812c5f:R/tab.R:938-973`). Now a `tab()` formal too, not only `tab_many()`'s.
- **`subtext =`** — same argument, same lines. It renders through the new `<tfoot>` and now sits *after* the auto-generated legend lines rather than being the only footnote.
- **String variables and string weights** — `tab(pa17, "CS1", "ABS_P1", wt = "POIDS_FINAL")` works in both. 2.0.0 is more permissive: a *variable holding* a character vector also resolves now (`quo_peek_extern()`, `R/tab.R:2500-2517`).
- **`tab_many()`'s return** — a `tabxplor_tabs` instead of a bare `list`, but it is still a list underneath; `[[i]]`, `map()`, `tab_kable()` and `tab_xl()` all behave identically.
- **`get_num`, `set_num`, `set_digits`, `set_col_var`, `set_ctr`, `set_n`, `set_pct`, `get_ctr`, `get_pct`, `is_refrow`, `as_refrow`, `as_totcol`, `as_totrow`, `cleannames_condition`, `fmt_get_color_code`, `score_from_lv1`, `tab_get_vars`** — all present, same signatures.
- **`tab_compact()`, `new_tab(subtext = )`, `is_fmt()`, `is_totrow()`, `is_totcol()`** — unchanged. `is_totrow()` additionally folds `NA` to `FALSE` now, which is a fix.

---

## 8. Deprecated but working — the noisy surface

None of these break. All of them warn, and **all of them are what the courses teach students to type**, so a student pasting course code into their own console sees a `lifecycle` message that the course does not explain. `R/setup.R:13-16` sets `warning = FALSE, message = FALSE` globally, so the knitted documents stay clean — the noise is in the student's session only.

| 1.3.1 spelling                                  | Sites         | 2.0.0                    | Behaviour                                    |
|-------------------------------------------------|---------------|--------------------------|----------------------------------------------|
| `tab_many()`                                    | 385           | `tab()`                  | soft-deprecated; keeps the legacy list shape |
| `chi2 = TRUE`                                   | 168           | `test =`                 | warns, routes                                |
| `add_n = FALSE`                                 | 34            | `n = "no"`               | warns, routes; `add_n = TRUE` is a no-op     |
| `OR = "OR"` / `"or"` / `"OR_pct"`               | 12            | `display = "{or}"` etc.  | warns; see note                              |
| `ci = "diff"`                                   | some          | `ci = "ref"`             | warns, rewrites                              |
| `compact = TRUE`                                | 7             | `output_list = FALSE`    | warns, inverts                               |
| `method_cell = "wald"`                          | 3             | `ci_method = c(cell = )` | warns                                        |
| `row_var =` / `col_var =`                       | 14            | `row_vars` / `col_vars`  | partial-matches, warns                       |
| `tab_prepare()`                                 | 4             | `tab()` arguments        | warns                                        |
| `fct_recode_helper()`                           | 85            | —                        | soft-deprecated                              |
| `tab_pct()` `tab_tot()` `tab_ci()` `tab_chi2()` | 18            | the `tab()` pipeline     | **warns every call**                         |
| `fmt(type = )`, `set_type()`, `get_type()`      | 27            | `scale` + `pct_type`     | warns; defunct in 2.1.0                      |
| `fmt(in_totrow = )`                             | few           | `row_kind =`             | warns                                        |
| `set_color_style()`                             | 2 (commented) | `set_color_palette()`    | warns, then does nothing                     |

The `OR =` shim pins `ref = "first"` only when `ref` was still `"auto"`, so the corpus's explicit `ref = 4` at `01-Logits.Rmd:879` is preserved. The four legacy step functions are defunct in 2.1.0, as are `fmt(type = )` / `set_type()` / `get_type()`.

Two that warn **less** than they should, so the courses will not notice them at all:

- **`color = "diff_ci"` / `"after_ci"` / `"ci"`** are remapped to `color = "difference"` + a `color_signif` policy with **no warning**: `normalize_color_spec()` sets `uenv <- rlang::caller_env(2)` (`R/tab.R:419`) but is called from `tab_resolve_common_args()`, not from `tab()`, so lifecycle sees a same-package caller and stays silent. Numerically equivalent, so harmless here — but see §12.5.
- **The five inert exporter arguments** never warn either, for the same class of reason (`tx_deprecate_inert()` passes no `user_env`, `R/utils.R:198-207`).

Also worth flagging for the port: `get_type()` is **lossy** — every effect scale (difference, ratio, odds ratio, coefficient) collapses to `"coef"`. The corpus's eight `get_type(.) == "n"` tests (`M1S1_01.Rmd:1284`, `M1S1_02.Rmd:722`, `M2_01.Rmd:1654`, `M2_02.Rmd:1166`, `L3S2_01.Rmd:2121`, `L3S2_02/03.Rmd:1284`, `L3S2_04.Rmd:722`) still work, because `"n"` round-trips. And **`get_color()` now returns canonical names** — `"difference"`, not `"diff"` — so any `get_color(x) == "diff"` test silently becomes `FALSE`.

---

## 9. The regression framework: `tab_logit()` is `tab_reg()`'s ancestor

`R/logit_functions.R` (1700 lines) defines the course's own regression layer — `tab_logit()`, `multi_logit()`, `or_plot()`, `lm_plots()`, `svglm2()` — built on top of tabxplor 1.3.1's `fmt()` primitives. It is taught to students as *"une nouvelle fonction, lancée avec le script `logit_functions.R` au démarrage"*.

**tabxplor 2.0.0's `tab_reg()` is what that layer was reaching for.** The right move is not to patch `logit_functions.R` back to life — it is to retire it.

### 9.1 Why patching is not viable

Four hard errors, and they are load-bearing rather than incidental:

| Site         | Call                              | Failure                                                   |
|--------------|-----------------------------------|-----------------------------------------------------------|
| `:1326`      | `fmt0("pct", type = "row")`       | `fmt0(display, digits, scale)` — **unused argument**      |
| `:1345`      | `fmt0("or", type = "row")`        | same                                                      |
| `:1497`      | `fmt(0, type = "OR", …)`          | `"OR"` is not an accepted legacy `type` — **abort**       |
| `:1137-1138` | `select(-3) | > rename_with(...)` | a Total column is always present now — **duplicate name** |

Two of these have a second failure behind the first. `fmt(0, type = "OR", or = , diff = )` also cannot recycle: `n = 0` makes the reference size 1, so a length-*k* `or` has nowhere to go. And `select(-3)` fails because `tot` now defaults to `c("row", "col")`, so dropping the third column leaves **two** `fmt` columns for a `rename_with(.cols = where(is_fmt))` that expects one.

Plus one silent shape change: with a **single** predictor, `tab_many()` now returns a bare tibble where the 1.3.1 call (forced by `tabxplor.compact = FALSE`, itself now a no-op) returned a length-1 list. `logit_functions.R:1112` then `purrr::map()`s over it and iterates *columns*. That is exactly the single-predictor "OR empiriques" pattern the course teaches at `01-Logits.Rmd` / `M2_06_07.Rmd:969-983`, and `or_plot()`'s `tabs <- tabs[[1]]` (`logit_functions.R:271`) has the same problem.

The two `fmt0()` sites and the `type = "OR"` site are reached whenever `empirical_odds_ratio` or `add_pct` is TRUE — i.e. by **`tab_logit(full_table = TRUE)`** (30 occurrences) and by **`or_plot()`**, which is live at `01-Logits.Rmd:592` and `M2_06_07.Rmd:648` and hard-codes `add_pct = TRUE, empirical_odds_ratio = TRUE, add_n = TRUE, ci = TRUE` (`logit_functions.R:263-269`).

The file is also full of positional surgery on `tab_many()`'s output — `select(-3)`, `rename_with(.cols = 2)`, `nth(or, 5)` — that assumes a 1.3.1 column layout which no longer exists.

**Repairing all of this would recreate, badly, what `tab_reg()` already does natively.**

### 9.2 What `tab_reg()` covers

| `tab_logit()` produced             | `tab_reg()`                                                     |
|------------------------------------|-----------------------------------------------------------------|
| `pct` column                       | the observed column's bracketed level (`display = "est_base"`)  |
| `Empirical OR` column              | `empirical = TRUE`, the **default**                             |
| `OR model` column                  | the model column                                                |
| `n` column                         | `n = "range"`, the default                                      |
| marginal effects                   | `measure = "difference"` / `"ratio"`, `effect = "at_reference"` |
| stars, as a character column       | per-cell p-value, rendered at print time                        |
| grouped by `var`                   | a `tabxplor_grouped_tab` grouped by predictor                   |
| `subtext =`, `cleannames =`        | same arguments                                                  |
| `split_var =`                      | `tab_vars =`, plus `color = "between_groups"`                   |
| several `dependent =`              | `outcome = c(...)`                                              |
| `inverse_two_level_factors = TRUE` | `outcome_level =`, same default behaviour                       |
| CI columns (`ci = TRUE`)           | `display = "est_ci"`, `conf_level =`                            |
| weights, incl. the `svyglm` branch | `wt =`, or a `svydesign()` as `data`                            |
| `or_plot()`                        | `forest_plot()`                                                 |

Two of those rows are upgrades rather than equivalents. The observed column is computed on **exactly the model's complete cases**, which `tab_logit()`'s `tab_many()`-built version was not; and the marginal effects were never exposed by `tab_logit()`, which hard-codes `marginal_effects = FALSE`. Stars stop being a character column, so the `htmlEscape` hack at `logit_functions.R:1710` goes away.

`tab_reg()` adds, with no `tab_logit()` counterpart: `color = "adjustment"` (the model-vs-crude movement, tested), the model-fit footer, five model checks, `shape =` for non-linear predictors, `a*b` interactions, `reg_check_plots()`, the gaussian / poisson / multinomial / ordinal families, and Excel / html / markdown export.

**Nothing functional is lost.** The genuine gaps are cosmetic: the exact French column labels the course prints (`"THEATRE: OR"`, `"Empirical OR"`, the blank spacer column between the OR and marginal-effect blocks), and `tab_logit()`'s refusal of numeric predictors (`01-Logits.Rmd:498`) — which `tab_reg()` accepts, changing what the course can promise.

### 9.3 The shape of the port

**~53 distinct live `tab_logit()` calls** across 10 files (67 raw, before the `01-Logits.Rmd` ≡ `M2_06_07.Rmd` twin is counted once): `01-Logits.Rmd` (34), `Exam_blanc_M2_2024_correction.Rmd` (5), `Test.Rmd` (3), `AC-Correction_examen_blanc.Rmd` (2), `Correction_2025.Rmd` (2), `M1S1_05_Exam_blanc_correction.Rmd` (2), `L3S2_05_Exam_blanc_correction.Rmd` (2), `Exam_blanc_M2_2024.Rmd` (2), `Exam_M2_2024.Rmd` (1). Plus 2 live `or_plot()` calls and the commented `multi_logit()` demos.

They reduce to five shapes:

1. `tab_logit(data, dependent = "X", predictors = "Y")` — one outcome, one predictor
2. `tab_logit(data, dependent = "X", predictors = c(...))` — one outcome, several predictors
3. `tab_logit(data, dependent = c(...), predictors = c(...))` — several outcomes (36 sites carry `dependent = c(`)
4. any of the above `+ full_table = TRUE` (30) and/or `+ subtext =`
5. `or_plot(dependent = , predictors = , ...)` — the forest plot

For shape 2, the equivalent is:

```r
# tab_logit(pc18, dependent = "THEATRE", predictors = c("CRITAGE", "DIPLOM", "CSTOTR"), wt = "POND")
tab_reg(pc18, outcome = "THEATRE", predictors = c("CRITAGE", "DIPLOM", "CSTOTR"), wt = POND)
```

Everything `tab_logit()` did is `tab_reg()`'s default here: `family = "auto"` detects binomial, `outcome_level` takes the outcome's first level (= `inverse_two_level_factors = TRUE`), `measure = "auto"` gives the odds ratio, `stars = TRUE`, grouped by predictor, and the observed (crude) companion is on.

⚠ **`wt` is a symbol, not a string, in the natural `tab_reg()` idiom** — though `wt = "POND"` also works, `rlang::ensym()` accepting a string literal. All the corpus's `wt =` on `tab_logit()` are commented out anyway, always as strings.

### 9.4 Division of labour

- **Mechanical, AI-doable:** rewrite the ~53 call sites so each asks `tab_reg()` for *the same model on the same variables*, and delete `R/logit_functions.R`'s tabxplor-dependent half (`multi_logit`, `tab_logit`, `or_plot`). `lm_plots()` and `svglm2()` touch no tabxplor and can stay.
- **Manual, the maintainer's:** the teaching. `01-Logits.Rmd` / `M2_06_07.Rmd` explain regression *through* the shape of `tab_logit()`'s output — the empirical-vs-model OR pair, the racetrack odds derivation, the marginal-effect column. `tab_reg()` says all of that better and says more (the observed/adjusted round trip is its organising idea), so the chapter is a rewrite, not a translation. `vignettes/tabxplor-reading-a-regression.Rmd` is the reference for the vocabulary to teach.

**Verdict: PORT** (replace the framework).

---

## 10. ggfacto

The AGD/CAH material calls `HCPC_tab()` (19), `mca_interpret()` (20), `pca_interpret()` (5), `ggmca()`, `ggca()`, `ggi()`, and post-processes every `HCPC_tab()` with the same workaround:

```r
mutate(across(where(is_fmt), ~ set_color(., "diff"))) |>   # sinon bug (no color)
  new_tab(subtext = cah_subtext) |> group_by(variables) |> tab_kable()
```

**Install state matters here.** `~/github/ggfacto` HEAD (`ba401e5`) is CRAN-equivalent 0.3.2 code, and that is what is installed at `~/R/x86_64-pc-linux-gnu-library/4.6/ggfacto`. The modernisation done during tabxplor's Phase 24e is **uncommitted working-tree changes** (`DESCRIPTION` bumped to `tabxplor (>= 2.0.0)`, `R/geometrical_data_analysis.R`, `R/utils.R`, `NEWS.md`, both READMEs, two `man/` pages).

**CRAN 0.3.2 does work against tabxplor 2.0.0** — Phase 24e records `R CMD check` Status OK — but noisily: `lifecycle` warnings from `set_type()`, `fmt(type = )`, `tab_many()` and `add_n = FALSE` inside `HCPC_tab()`, `pca_interpret()` and `interactive_tooltips()`. Harmless under the course's global `warning = FALSE`.

**What lands with the modernised version:**

| Function            | Change                                                              |
|---------------------|---------------------------------------------------------------------|
| `HCPC_tab()`        | rebuilt on `tab()`; see below                                       |
| `pca_interpret()`   | grades a coordinate by size on the standardised ladder, not by sign |
| `mca_interpret()`   | **unchanged** — pure dplyr + kableExtra, still 8 columns            |
| `ggca()`            | documented input becomes `as.matrix(tabxplor::tab(...))`            |
| `ggmca()` / `ggi()` | three tooltip bug fixes; plots otherwise unchanged                  |

`HCPC_tab()` keeps its final orientation and its column names (`variables`, `lvs`, the clusters, `Ensemble`), so `group_by(variables)` still holds. What changes: the **duplicated `n` row is gone**; `% of population` and `n` become declared display rows, so they are neither coloured nor counted as data; the `color` default is spelt `"difference"`; and `...` now forwards to `tab()` rather than `tab_many()`, so `add_n =` / `compact =` / `chi2 =` passed through it would warn. **The `set_color(., "diff")` workaround becomes unnecessary** — harmless if left, but it will not colour the mean rows either way.

`mca_interpret()` being untouched is what keeps `kableExtra::remove_column(c(1, 8))` working at `05-ACM.Rmd:887, 943, 987, 1032`.

⚠ **A mixed table's mean rows lose their colour under the default.** 2.0.0 refuses to grade a mean difference on the percentage ladder (`mixed`-column gate, Phase 24e), so any `HCPC_tab()` mixing numeric and factor `row_vars` shows uncoloured mean rows. `color = "ratio"` colours every row — a ratio is the one comparison unlike quantities state alike.

**Action ordering: commit and install the modernised ggfacto before porting the AGD chapters**, or the port will be written against behaviour that is about to change. Sites: `06-CAH.Rmd:303, 320, 429, 453`; `07-Examen_blanc.Rmd:390`; `AC-Correction_examen_blanc.Rmd:460, 565, 584`; `Examen_2025/Examen_2025.Rmd:984`; `Examen_2025/Correction_2025.Rmd:272, 291, 366, 385`.

**Verdict: PORT**, after installing.

---

## 11. Prose drift

Course text that is now factually wrong about what tabxplor prints. **No test catches any of this.** The remedy for now is to *flag the sites in place* — a `<!-- TODO 2.0.0: ... -->` at each — so they surface when the chapters are rewritten.

| What the text says                      | 2.0.0                                         | Where                               |
|-----------------------------------------|-----------------------------------------------|-------------------------------------|
| "la colonne n"                          | folded into the Total cell, `100% (9 838)`    | many                                |
| `tabxplor.compact` merges tables        | dead option; `tab()` merges by default (§6.1) | ~18 files                           |
| the parenthesis holds `σ`, the SD       | it holds `cv`, in percent (§7.5)              | `M1S1_04.Rmd:740` + 2 twins         |
| `ci_print = "moe"` gives `± x`          | the token is `{moe}`; option dead (§6.2)      | `M1S2_01.Rmd:423-425` + 2           |
| `tab(pe22, AGE)` gives one row per age  | bands (§7.4)                                  | `M1S1_03.Rmd:700` + twin            |
| `tab_plot()` exports an image           | defunct (§4.2)                                | `M2_04.Rmd:833`, `RestesM1.Rmd:828` |
| `tab_num()` tabulates a number          | superseded; `tab()` does it                   | prose                               |
| the `tab()` argument reference card     | 5 arguments are deprecated spellings          | `04-AC.Rmd:172-181` + 2             |
| a figure quoted off an `after_ci` table | may have moved a decimal (§7.1)               | many                                |

The argument reference card is the one that matters most for teaching: `chi2`, `add_n`, `OR`, `sup_cols` and `method_cell` all still work but all warn, at `04-AC.Rmd:172-181`, `M1S1_04.Rmd:599` and `M1S2_04-Analyse de données.R:155-231` — the last of which is the file handed to students.

One special case: **the "find the 7 syntax errors" exercise** (`M1S1_03.Rmd:1533`, `M2_03.Rmd:2041`, plus commented variants) deliberately writes a broken `tab()` call. 2.0.0's stricter argument boundary changes *which* errors R reports and in what order — the exercise's answer key needs re-reading against the new messages, which are considerably better (`Unknown argument 'pc' … Did you mean 'pct'?`).

**Verdict: PORT**, deferred — flag now, rewrite with the chapters.

---

## 12. Package-change candidates

Ranked. Each is a place where changing tabxplor is more defensible than changing the courses. No patches here — symptom, blast radius, argument, risk.

**All of these are DONE**: they were the content of **Phase 24g**, whose summary in `CLAUDE.md` records what shipped. It also carried two items this audit did not anticipate — a build-time `caption` argument, and Quarto citizenship (§5.8). Each subsection below records its disposition.

### 12.1 Emit a real `<caption>` — **strongest** · DONE, host-aware

- **Symptom.** bookdown table cross-references (`(\#tab:x)` + `\@ref(tab:x)`) silently stop resolving; the raw label stays visible in the title (§5.1).
- **Who it hits.** Every bookdown/`html_document2` user who captions a table — which tabxplor's own `CLAUDE.md` names as the required rendering target.
- **Why it is tabxplor's.** The `<div class="tabxplor-caption">` decision is defensible on its own terms (a `<caption>` participates in table width), but it silently breaks an ecosystem contract that 1.3.1 honoured, and `NEWS.md` does not mention it.
- **What would change.** Emit `<caption class="tabxplor-caption">` as a `<table>` child, with the width behaviour restored by CSS (`.tabxplor-caption` already carries `white-space: normal` and the geometry it needs); or keep the `<div>` and add an option. The `^\s*<caption` grep also requires the caption to start its own line, so the string assembly at `R/tab-render-html.R:416-423` needs a newline before `<table>`.
- **Risk.** Low, and covered — the html render snapshots would move once, deliberately.
- **Done: host-aware, not unconditional.** A real `<caption>` under bookdown, no caption at all under Quarto when the cell sets `tbl-cap`, the `<div>` everywhere else — Quarto never needs a `<caption>` (§5.8), so the width motive is kept where it still applies.
- ⚠ **Measured while implementing it: the inner element must be a `<span>`, never a `<div>`.** bookdown's scan runs on the POST-pandoc html, and pandoc's writer gives every *block* tag a line of its own — so `<caption><div>…</div></caption>` puts the label two lines below `<caption>`, outside the `content[i - 0:1]` window, and does not resolve. A `<span class="tabxplor-caption">` stays on the text's line and still carries the width guard (`display:block;width:0;min-width:100%`). Two further facts from the same render: pandoc **unescapes** `\#` → `#`, which is why the token bookdown greps for is `(#tab:x)` and nothing in R must touch it; and `caption-side:top` is load-bearing, because Bootstrap puts a caption at the BOTTOM and tabxplor injects Bootstrap into every knitted document.

### 12.2 Restore `df` precedence in `tab_plain()` / `tab_num()` · DECLINED

- **Symptom.** `tab_plain(..., num = TRUE, df = TRUE)` returns a tibble where 1.3.1 returned a row-named `data.frame`; `FactoMineR::CA()` refuses it (§4.4).
- **Who it hits.** Anyone feeding a tabxplor count table to a matrix-oriented consumer — correspondence analysis, `chisq.test()`, `vcd`.
- **Why it is tabxplor's.** `df` names the *return type* and `num` the *cell contents*; the natural reading of both is "a data.frame of numbers", which is what 1.3.1 gave. The flip is undocumented and `NEWS.md` does not mention it.
- **What would change.** One line: check `df` before `num` in `leaf_extract_raw()` (`R/tab.R:2632`), matching `b812c5f:R/tab.R:2328`. The `WARNING:` comment at `R/tab.R:2624` documents the current order as deliberate, so this needs the maintainer's call rather than a silent revert.
- **Risk.** Low. Nothing in `R/` calls it with both flags.
- **Declined.** `as.matrix()` / `as.table()` (§4.4) give base R a better road than either flag, so the courses port to those and the precedence stays as its `WARNING:` comment describes.

### 12.3 Validate the exporters' `...` · DONE

- **Symptom.** `tab_html()`, `tab_md()` and `tab_xl()` silently accept and ignore any unknown argument — `position =` (519 sites here), `n_min =`, `hide_near_zero =`, and every typo (§5.4, §6.7).
- **Why it is tabxplor's.** The producers do the opposite: `tab_check_dots()` errors on an unknown name *with a suggestion*. Two halves of one API disagreeing about whether a typo is an error is the kind of thing the declarative architecture exists to prevent. And `R/utils.R:186-187` already asserts the behaviour that is missing.
- **What would change.** Assign `tx_deprecate_inert()`'s return value and refuse what remains, in the three exporters — `tab_export()` already does exactly this (`R/tab-export.R:55, 58`). Retired names (`position`, `n_min`, `hide_near_zero`) would join `TX_INERT_EXPORT_ARGS` so they warn rather than abort.
- **Risk.** Medium — it turns silence into errors for existing user code, which is the point, but it is a behaviour change at the last minute.
- **Done: error**, on all **five** entry points whose `...` only ever absorbed retired names — `tab_html()`/`tab_kable()`, `tab_md()`, `tab_xl()`, `tab_css()` and `forest_plot()`, not the three this section listed. `position` / `n_min` / `hide_near_zero` joined `TX_INERT_EXPORT_ARGS`, so the retired names warn rather than abort, and `tab_check_dots()`'s known set is the declared rows PLUS the producer's own formals (`EXPORT_ARGS` deliberately declares only the rows whose prose it needs). ⚠ `tab_export()` was left permissive on purpose: its `...` really is a pass-through, and the leaf validates it. The change caught four latent typos in `dev/tests/` on its first run.

### 12.4 `$rr` — the comment promises what the code omits · DONE, read alias only

- **Symptom.** `R/fmt_class.R:4356` describes `$rr` as a read-side alias of `$ratio`, but `$.tabxplor_fmt` has branches only for `wn`, `ci`, `tot_wn` and `in_totrow`. `x$rr` errors.
- **Why it is tabxplor's.** The other three renamed fields (`ci`, `in_totrow`, and `wn`'s totals twin) all have read aliases; `rr` was left out, and the comment says otherwise. Users who read `$rr` in 1.3.1 are exactly the audience the "the `fmt` fields should not break" rule in `CLAUDE.md` protects.
- **What would change.** One branch in `$.tabxplor_fmt`. Whether `mutate(rr = )` should also route to `ratio` is a separate and harder question — silently redirecting a write inside `mutate()` is not obviously right, but silently creating a dead field (§4.6) is clearly wrong; a refusal naming `ratio` would be better than either.
- **Risk.** Very low for the read alias.
- **Done: the read alias only.** `mutate()` stays permissive — refusing a retired field name would need a second declared map for a failure mode the alias and this document already surface.

### 12.5 Two deprecation warnings that never fire · HALF DONE

- **Symptom.** `color = "diff_ci"` / `"after_ci"` / `"ci"` are remapped with no message (`normalize_color_spec()` sets `uenv <- rlang::caller_env(2)` at `R/tab.R:419` but is called one frame deeper than it assumes), and the five inert exporter arguments never warn (`tx_deprecate_inert()` passes no `user_env`, `R/utils.R:198-207`).
- **Why it is tabxplor's.** Both are bugs in the deprecation plumbing, not design decisions — `resolve_ci_method()` (`R/tab-agg.R:261`) threads `user_env` correctly and does warn, which shows the intent.
- **What would change.** Thread `user_env` through both.
- **Risk.** Very low. Consequence: 1.x code starts warning on the corpus's most common argument.
- **Done: `tx_deprecate_inert()` only.** `normalize_color_spec()` stays silent on purpose — the aliases are numerically identical and the courses will be find-and-replaced in one pass, so the message would be pure noise. Recorded at the line so the next reader does not "fix" it.

### 12.6 A `get_chi2()` shim · DONE

- **Symptom.** Removed with no shim and no `NEWS.md` line (§4.3), where every other removal got one.
- **Why it is tabxplor's — weakly.** It was internal, so nothing is owed. But 39 sites in one corpus reached for it because it was the only programmatic access to the test in 1.3.1, and `get_test()` is a different schema, not a rename.
- **What would change.** A deprecated wrapper mapping the four rows that survive (`pvalue`, `df` → `df1`, `count` → `statistic`, plus `tables`), aborting on `cells` / `variance` with a pointer to `get_ctr()`.
- **Risk.** Very low, but it adds surface to a release that is trying to shed it.
- **Done, and simpler than a schema translation.** `get_chi2()` is a soft-deprecated internal alias returning `get_test(x)` — `test` being the new name for the extended old `chi2` attribute — and `get_test()` is **exported**. ⚠ its message points at the cells' own `ctr` field, not at `get_ctr()`, which is internal.

**Not candidates**, though they came up: `na = "drop"` (semantics are unchanged for real usage, §7.6), `tab_many()`'s `tabxplor_tabs` class (still a list), the reserved-level abort (a genuine correctness fix, with a message that names its own cure), the `n` column (the headline design change), and the CSS/markup rewrite (the release's whole point).

---

## 13. Claims that need a render to settle

Listed rather than asserted:

1. **bookdown numbering after a `<caption>` fix** — that the label resolves, the anchor lands, and `\@ref(tab:…)` renders.
2. **Bootstrap-cosmo injection into `bookdown::gitbook`** (§5.7) — whether the M2 book gets restyled.
3. **`kableExtra::column_spec()` on the new markup** (§5.5) — the four failure modes are reasoned from the markup, not observed.
4. **Reserved level collisions** (§4.7) — whether `pc18`, `pe22`, `pa17`, `FES2017` or `ee2013_20` actually carry a level named `"Total"` or `"Ensemble"`. The diagnostic is in §4.7.
5. **`style.css:724-770`** — whether the gitbook `.page-content table` overrides are live or inside a comment block, and if live whether they fight `.tabxplor-tab .tx-r`.
6. **`select(-wn)` at `05-ACM.Rmd:207`** — whether `wn` is still a real column of that particular `tab_many()` output.
7. **How much the `<style>` repetition costs** (§5.3) — a 40-table document inlining the full stylesheet 40 times.

---

## 14. Suggested port order

1. **Install the modernised ggfacto** (§10) — everything AGD depends on it.
2. **`R/setup.R`** — the CSS switch (§5.3), `random_table_from_tab()` (§4.5, §6.5), `get_int()`, and `fill_table()` (§5.2, the long one). Every root document depends on it.
3. **`R/logit_functions.R` → `tab_reg()`** (§9) — mechanical rewrite of the ~53 call sites; the two logit chapters' *teaching* is deferred to the maintainer.
4. **`books/M2_CES_Multi`** — `04-AC.Rmd` (`tab_plain`, `get_chi2`, `tab_prepare`), `08-Explo.Rmd` (same), `06-CAH.Rmd` and the exam files (ggfacto), `03-ACP.Rmd` / `05-ACM.Rmd` (mostly fine).
5. **The root files**, deduplicated first — port `M1S1_02.Rmd` once and propagate to `L3S2_04.Rmd`; likewise the `L3S2_01`/`M1S1_01`/`M2_01`, `M1S1_03`/`M2_03`, `M1S2_01-03`/`M2_04-05`/`RestesM1` and `M2_06_07`/`01-Logits` families.
6. **The student-facing boilerplate** (§6.1) — one edit, ~18 places.
7. **Flag the prose** (§11), rewrite later.

---

## Appendix A — evidence index

| Claim                                  | 1.3.1 (`b812c5f`)                   | 2.0.0 (`dev`)                                              |
|----------------------------------------|-------------------------------------|------------------------------------------------------------|
| caption element                        | kableExtra `<caption>`              | `R/tab-render-html.R:400-404`                              |
| bookdown's rule                        | —                                   | `bookdown:::parse_fig_labels()`, `tab` branch              |
| `df` / `num` precedence                | `R/tab.R:2328`                      | `R/tab.R:2626-2634`                                        |
| `kable_tabxplor_style()`               | exported                            | `R/tab_classes.R:769-770` (`deprecate_stop`)               |
| `tab_plot()`                           | exported                            | `R/tab_classes.R:1438-1439` (`deprecate_stop`)             |
| `get_chi2()`                           | exported internal                   | absent; `get_test()` at `R/tab_classes.R:113`              |
| test schema                            | `chi2 stats` wide frame             | `new_test_tibble()`, `R/tab_classes.R:340-362`             |
| `n` column                             | `R/tab.R:1176-1188`                 | `R/tab-options.R:110-117`; shim `R/tab_classes.R:260-278`  |
| reserved levels                        | —                                   | `R/row-model.R:232-267`                                    |
| `fmt` legacy `type` map                | one `type` attribute                | `R/tab-deprecate.R:264-286`                                |
| `fmt0()`                               | `fmt0(display, type)`               | `R/fmt_class.R:2823`                                       |
| `ci` field                             | one half-width field                | `ci_inf`/`ci_sup`; `get_ci()` at `R/fmt_class.R:2895-2897` |
| `rr` field                             | `rr`                                | `ratio`; comment at `R/fmt_class.R:4356`                   |
| `tabxplor.compact`                     | `R/utils.R:62`, `R/tab.R:1440-1443` | absent                                                     |
| `tabxplor.ci_print`                    | `R/fmt_class.R:1401`                | absent; `R/fmt_class.R:3715-3717`                          |
| `tabxplor.always_add_css_in_tab_kable` | `R/tab_classes.R:745-760`           | absent; `R/tab-options.R:294-297`                          |
| CI method defaults                     | `"ac"`, pooled t                    | `R/tab-agg.R:219-220`                                      |
| `color = "auto"`                       | `R/tab.R:186-190`                   | `R/fmt_class.R:4973, 4998`                                 |
| colour ladders                         | `R/utils.R:41-44`                   | `R/tab_classes.R:2685-2720`                                |
| `na = "drop"`                          | `R/tab.R:348-351`, `:417-420`       | `R/tab.R:321-326`, `R/tab-leaf.R:395-416`                  |
| `ref` matching                         | regex only, `R/tab.R:5583-5588`     | exact-first, `R/tab.R:2684-2685`                           |
| `levels`                               | `R/tab.R:938-973`                   | `R/tab.R:1340-1375`                                        |
| `tab_html()` formals                   | —                                   | `R/tab_classes.R:677-690`                                  |
| inert export args                      | —                                   | `R/utils.R:188-207`                                        |
| tooltip markup                         | Bootstrap popover DOM               | `R/tab-render-html.R:44-55`                                |
| html dependencies                      | `includeCSS(tab.css)`               | `tx_html_deps()`, `R/tab-render-html.R:510-521`            |
| `unbrk` / `sigma_sign`                 | `R/utils.R:1061-1062` (constants)   | `R/utils.R:690-691` (constants)                            |
| p-value rows                           | `R/tab.R:1446-1451`                 | `R/tab_classes.R:994-1002` (export only)                   |

---

## Appendix B — per-file triage

Which sections apply where. Twin files (right column) need the same edits; port the left one and propagate.

| File                                 | Applies                                           | Twins                                               |
|--------------------------------------|---------------------------------------------------|-----------------------------------------------------|
| `R/setup.R`                          | §4.5, §5.2, §5.3, §6.5, §7.2                      | —                                                   |
| `R/logit_functions.R`                | §9 (whole file)                                   | —                                                   |
| `R/set_RStudio_prefs.R`              | commented only; `tab_kable_multi()` never existed | its M2 copy                                         |
| `books/.../01-Logits.Rmd`            | §4.6, §6.1, §9, §5.2                              | `M2_06_07.Rmd`                                      |
| `books/.../04-AC.Rmd`                | §4.3, §4.4, §4.8, §7.2                            | —                                                   |
| `books/.../08-Explo.Rmd`             | §4.4, §4.8                                        | —                                                   |
| `books/.../06-CAH.Rmd`               | §10                                               | `07-Examen_blanc`, `AC-Correction`, `Examen_2025/*` |
| `books/.../03-ACP.Rmd`, `05-ACM.Rmd` | §10, §4.5 (`select(-wn)`), §5.5                   | —                                                   |
| `books/.../index.Rmd`                | §6.1                                              | —                                                   |
| `M1S1_02.Rmd`                        | §4.1, §4.5, §4.6, §5.2, §5.5, §6.2, §8            | `L3S2_04.Rmd`                                       |
| `M1S1_03.Rmd`                        | §6.2, §7.4, §11 (the 7-errors exercise)           | `M2_03.Rmd`                                         |
| `M1S1_04.Rmd`                        | §4.2, §6.1, §7.5, §11                             | —                                                   |
| `M1S1_01.Rmd`                        | §4.1, §5.2                                        | `L3S2_01/02/03.Rmd`, `M2_01.Rmd`                    |
| `M1S2_01.Rmd`                        | §4.1, §5.1 (`\#tab:` captions), §6.1, §6.2        | `M2_04/05.Rmd`, `RestesM1.Rmd`                      |
| `M1S2_02/03.Rmd`                     | §4.5 (`arrange(is_totrow(n))`), §6.1              | `M2_04/05.Rmd`, `RestesM1.Rmd`                      |
| `M2_02.Rmd`                          | §4.3, §4.5, §6.2, §5.5                            | —                                                   |
| the exam files                       | §6.1, §8, §9                                      | each other                                          |
| `M1S2_04-Analyse de données.R`       | §6.1, §11 (handed to students)                    | —                                                   |
