# tabxplor dependency policy

What tabxplor depends on, why, and what it would cost to change. This is a **policy** document, not a
phase log: it states the current rules and the current inventory, and it prices the options that were
weighed and *not* taken, so the next person does not re-derive them.

## The three rules

1. **Table building and core inference are always available; everything else is pay-as-you-go.**
   A user who only makes cross-tables must not compile an Excel writer or a plotting stack. Every
   optional feature is guarded at its entry point and degrades or refuses with a message naming the
   exact `install.packages()` line — never with a raw R error.
2. **A dependency must earn its place against the code it saves.** A package present for one
   function, one formula or one demo data set is a candidate for removal: write the function, vendor
   the formula with credit, or ship the data.
3. **The CRAN ceiling is 20, and headroom is the point.** `R CMD check --as-cran` sets
   `_R_CHECK_EXCESSIVE_IMPORTS_=20` and notes any package whose non-base Imports exceed it. Sitting
   at the limit means the next genuinely-needed Import triggers a NOTE on submission day.

Verify the count the check actually applies:

```r
d    <- read.dcf("DESCRIPTION")
imps <- trimws(gsub("\\s*\\(.*?\\)", "", strsplit(d[, "Imports"], ",")[[1]]))
length(setdiff(imps, tools:::.get_standard_package_names()$base))   # must be <= 20
```

⚠ `stats`, `utils` and **`grid`** are base packages: they cost nothing and do not count. `grid` is in
Imports rather than Suggests for exactly that reason — it is called unguarded in `forest_plot()`.

## Where things stand

| | count | note |
|:---|---:|:---|
| Imports (non-base) | **15** | 5 slots of headroom against CRAN's 20 |
| Suggests | **24** | |
| Hard install, recursive | **34** packages | what `install.packages("tabxplor")` pulls |
| `dependencies = TRUE`, recursive | **131** packages | what `tx_need_pkg()`'s advice pulls |

## Imports — what each is for

| package | why it cannot be a Suggest |
|:---|:---|
| `dplyr` | ~725 call sites, and 15 generics implemented as S3 methods in NAMESPACE |
| `purrr` | ~658 call sites |
| `rlang` | ~348 call sites, plus `importFrom(rlang, .data)` |
| `cli` | every message and abort in the package |
| `vctrs` | `import(vctrs)`: the `fmt` record and ~55 bare S3 method definitions |
| `tibble` | every table IS one |
| `tidyselect` | the argument surface's tidy-select |
| `pillar` | 4 generics (`pillar_shaft`, `tbl_sum`, `tbl_format_body`, `tbl_format_footer`) as S3 methods |
| `data.table` | `import(data.table)`: the aggregation core's `.SD` / `.N` / `:=` |
| `lifecycle` | the deprecation engine, and `badge()` in generated roxygen |
| `forcats` | 13 functions over ~80 sites, and `gss_cat` in `gss_cat_data_formatting()` |
| `tidyr` | `pivot_wider` / `pivot_longer` on `fmt` columns; `replace_na` ×17 |
| `survey` | ⚠ **not only a survey dependency** — `survey::regTermTest()` is the Wald engine for *unweighted* multinomial and ordinal footers too (`R/tab_reg.R`). `svyrecvar` owns the design-based variance algebra. |
| `nnet` | `nnet::multinom()`, the multinomial engine |
| `MASS` | `MASS::polr()`, the proportional-odds engine |

## Suggests — what each is for, and its guard

Guards live at the entry point. `tx_need_pkg(pkgs, what, severity)` (`R/utils.R`) is the one spelling:
`severity = "abort"` where the feature cannot degrade, `"inform"` where it can. It names **every**
missing package of one request at once and gives the exact install line.

| package | feature | guard |
|:---|:---|:---|
| `openxlsx2` | Excel export | abort, in `tab_xl()` |
| `kableExtra` | the themed Viewer page and its tooltip JS | inform, degrades to a plain print |
| `clipr` | `tab_md(clipboard = TRUE)` | inform |
| `ggplot2`, `gridExtra` | `forest_plot()`, `reg_check_plots()`, the Excel check images | abort (inform for the images) |
| `marginaleffects` | `effect = "at_reference"` only — the g-computation engine is tabxplor's own | abort |
| `VGAM`, `svyVGAM` | survey-weighted multinomial | abort, named together |
| `brant` | the proportional-odds test | inform, the row is simply absent |
| `mirai`, `RhpcBLASctl`, `parallelly`, `pkgload` | the parallel seam, opt-in behind `tabxplor.parallel` | inform / silent fallback |
| `jmvcore`, `R6` | the jamovi module | `requireNamespace()` at namespace-load |
| `rstudioapi` | console theme detection, `fct_recode_helper()` | `requireNamespace()`, base fallback |
| `fansi`, `rmarkdown`, `knitr` | the vignettes (`knitr` is also `VignetteBuilder`) | — |
| `testthat`, `withr`, `yaml`, `bench`, `DescTools` | the test suite | `skip_if_not_installed()` |

⚠ `knitr` is a **Suggest** although three `knit_print` methods are registered for it. That works because
`@exportS3Method knitr::knit_print` emits a *delayed* `S3method(knitr::knit_print, …)`, which R
resolves when knitr loads. Two consequences to remember:

- Chunk options are read through `tx_knitr_opt()` (`R/utils.R`), which returns `NULL` unless
  `getOption("knitr.in.progress")` is `TRUE` **and** knitr is installed.
- ⚠ `print.tabxplor_kable()` must load knitr **before** `NextMethod()`. An S3 method exists only once
  its own package is loaded, so an unguarded fall-through reaches `print.default()` and shows the raw
  character vector with its attributes. The method carries a one-line `cat()` fallback for the case
  where knitr is genuinely absent.

## The four example data sets

`facto_tea`, `questionr_hdv`, `car_arrests`, `car_salaries` (`R/data.R`, built by
`data-raw/DATASETS.R`). Each is the **complete** original — 44.6 KB for all four — with one editorial
change: in a two-level yes/no factor the "yes" answer goes first, because `tab()` and `tab_reg()`
model and show the first level.

⚠ **The name carries the credit.** The `facto_` / `questionr_` / `car_` prefix says where each came
from, and means attaching FactoMineR, questionr or carData beside tabxplor masks nothing. A same-name
copy would be worse than a rename: the level order deliberately differs from the original, so a user
could silently get different results from the same expression.

All four sources are GPL (>= 2), which tabxplor's GPL (>= 3) may redistribute. Every `@source` names
the package, its authors and the original study, and says how to get the untouched data.

## Vendored code, with credit

Two functions live in tabxplor rather than in a dependency. Both are GPL (>= 2) → GPL (>= 3), both
carry their attribution in a comment above them, and both were verified against the original.

| what | where | verified |
|:---|:---|:---|
| `htmltools::htmlEscape` → `tx_html_escape()` | `R/utils.R` | identical for both `attribute` modes, encoding included |
| `car::vif.default/polr/svyolr` → `tx_vif()` | `R/reg-assumptions.R` | `all.equal(tolerance = 1e-13)` on 14 fit shapes; `NULL` exactly where car errors or returns `NaN`. Re-run `dev/vif_car_parity.R` after **any** change to it. |

`tx_vif()` implements Fox & Monette (1992) GVIF and returns **both** of car's shapes, so both call
sites read it unchanged. It refuses rather than approximates: fewer than 2 terms, aliased
coefficients, a rank-deficient fit, a matrix-coefficient fit (multinomial) or a singular vcov all give
`NULL`, and the collinearity row is then simply absent.

## String handling

tabxplor uses base R throughout. Three primitives in `R/utils.R` carry what stringi used to:

- `tx_pad(str, width, side, pad)` — pads on **display width** (`nchar(type = "width")`), because the
  tables are aligned by eye and `pad` is often a figure space or a non-breaking space.
- `tx_str_wrap()` — **minimum-raggedness** word wrap by dynamic programming, not a greedy fill and
  not `strwrap()` (which formats a paragraph: it normalises whitespace runs and double-spaces after a
  full stop). Verified against `stri_wrap()` over 21 112 comparisons, 0 differences.
- `tx_str_trunc()`, and `tx_wrap_name()` for compound names.

⚠ **Four traps that a base-R rewrite must respect**, each of which was hit and fixed:

1. **stringi vectorises over `pattern`; base R does not.** `stri_detect_fixed(one_line, five_tags)`
   returns five logicals; `grepl(five_tags, one_line)` silently uses the first and warns. Build one
   alternation instead.
2. **`x |> stri_*()` cannot be reordered mechanically.** stringi takes the string first, `gsub()`
   takes the pattern first, so a piped call must be rewritten as an explicit one.
3. **`stri_trim(x, side = "left")` is one-sided**; `trimws()` defaults to both. Losing `which =` moved
   every markdown column.
4. **ICU classes are not PCRE classes.** `\P{Wspace}` has no PCRE name (use `[\h\v]`), and a `\uXXXX`
   escape inside an ICU *pattern* is a literal backslash-u in PCRE — write the character itself.

⚠ **stringi is still installed**, via `tidyr` → `stringr` → `stringi`. Dropping it from Imports bought
a CRAN slot and removed a direct coupling and 143 call sites; it did **not** remove the package from
anyone's library. Only dropping `tidyr` would do that.

## Costed options that were not taken

| option | what it buys | what it costs |
|:---|:---|:---|
| `forcats` → base R | 1 Import | 13 functions over ~80 sites. `fct_lump_min`, `fct_collapse`, `fct_relabel` are fiddly, and `gss_cat` would still be needed by `gss_cat_data_formatting()` and ~35 examples. |
| `tidyr` → base R | 1 Import, **and stringi with it** | `pivot_wider`/`pivot_longer` on `fmt` columns is the blocker — 9 sites, and the reshape must preserve every record field and column attribute. The biggest remaining win, and the hardest. |
| `MASS` / `nnet` → Suggests | 2 Imports, at no cost to any user (both are R Recommended, so always present) | guarding two `reg_check_deps()` clauses, plus skip guards on ~27 test sites. Only worth doing if the count nears 20. |
| `survey` → Suggests | 1 Import, and RcppArmadillo off the hard install | ⛔ **do not**: `regTermTest()` is the Wald engine for unweighted multinomial/ordinal footers, so those rows would silently vanish for users who never touch a survey design. |
| `DescTools` → out of Suggests | ~15 packages off `dependencies = TRUE` | its only use is CI parity in four test files, validating the closed-form CI engine against an independent implementation. That is worth keeping — but the tests could move to `dev/`, which is Phase 23e's job. |
| promoting any Suggest to Imports | the feature always works | ⛔ contradicts rule 1, and every candidate is huge (`VGAM`), compiled (`openxlsx2` → Rcpp + stringi) or opt-in-only (`mirai`, `RhpcBLASctl`, `parallelly`). `tx_need_pkg()` already gives a one-line install command. |

## Verification recipe

```bash
# the suite (alone, never beside another R process -- see CLAUDE.md § Testing)
OMP_NUM_THREADS=1 Rscript -e 'Sys.setenv(TESTTHAT_CPUS="8", NOT_CRAN="true"); devtools::test("~/github/tabxplor")'

# the vendored VIF, against the real car (car is NOT a dependency, so this cannot be a test)
OMP_NUM_THREADS=1 Rscript dev/vif_car_parity.R

# rebuild the data sets (needs FactoMineR, questionr, carData)
OMP_NUM_THREADS=1 Rscript data-raw/DATASETS.R
```

After any string-handling change, the proof is that `test-golden.R`, `test-export-parity.R`,
`test-fmt-contract.R`, `test-fuse-parity.R` and every `_snaps/` file stay **byte-identical**: the 36
`_golden/` fixtures exist for exactly this.
