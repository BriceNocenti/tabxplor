# tabxplor dependency policy

What tabxplor depends on, why, and what it would cost to change. This is a **policy** document, not a
phase log: it states the current rules and the current inventory, and it prices the options that were
weighed and *not* taken, so the next person does not re-derive them.

## The four rules

1. **Table building and core inference are always available; everything else is pay-as-you-go.**
   A user who only makes cross-tables must not compile an Excel writer or a plotting stack. Every
   optional feature is guarded at its entry point and degrades or refuses with a message naming the
   exact `install.packages()` line — never with a raw R error.
2. **A dependency must earn its place against the code it saves.** A package present for one
   function, one formula or one demo data set is a candidate for removal: write the function, vendor
   the formula with credit, or ship the data. ⚠ Price it by its **closure**, not by itself, and by
   the part of that closure that is neither already present nor common — see *Who pays* below.
3. **The CRAN ceiling is 20, and headroom is the point.** `R CMD check --as-cran` sets
   `_R_CHECK_EXCESSIVE_IMPORTS_=20` and notes any package whose non-base Imports exceed it. Sitting
   at the limit means the next genuinely-needed Import triggers a NOTE on submission day.
4. ⚠ **A Suggest is not free.** The jamovi module bundles `Depends + Imports + Suggests + LinkingTo`
   alike into the `.jmo`, so a **test-only** Suggest is pure shipped weight. Nothing goes into
   Suggests just because it is convenient in a test.

Verify the count the check actually applies:

```r
d    <- read.dcf("DESCRIPTION")
imps <- trimws(gsub("\\s*\\(.*?\\)", "", strsplit(d[, "Imports"], ",")[[1]]))
length(setdiff(imps, tools:::.get_standard_package_names()$base))   # must be <= 20
```

⚠ `stats`, `utils` and **`grid`** are base packages: they cost nothing and do not count. `grid` is in
Imports rather than Suggests for exactly that reason — it is called unguarded in `forest_plot()`.

## Where things stand

|                                  |                        count | note                                                                           |
|:---------------------------------|-----------------------------:|:-------------------------------------------------------------------------------|
| Imports (non-base)               |                       **15** | 5 slots of headroom against CRAN's 20                                          |
| Suggests                         |                       **22** | `DescTools`, `bench` (23e) and `kableExtra` all left; `htmltools` joined       |
| Hard install, recursive          |              **32** packages | what `install.packages("tabxplor")` pulls at runtime (34 counting `LinkingTo`) |
| `dependencies = TRUE`, recursive |              **94** packages | what `tx_need_pkg()`'s advice pulls                                            |
| jamovi `.jmo` payload            | **23** packages, **31.8 MB** | what jamovi does *not* already bundle                                          |
| ...of which test-only            |     **0** packages, **0 MB** | the 21.9 MB of `DescTools` + `bench` is gone                                    |

## Who pays for a dependency

Three audiences, and they do **not** pay the same way. Optimise for the third: it is the one the
package is written for.

1. **CRAN** cares only about the *count* of non-base Imports (the rule of 20 above).
2. **The jamovi module** pays for **every** package in DESCRIPTION, Suggests included, in megabytes
   inside the `.jmo`. See below — this is the surprising one.
3. **A student on a university machine** pays in *download time on a shared network*, and pays most
   for a package that is **exotic** (nobody's cache has it) and **compiled** (no binary for their
   platform means a toolchain and a wait). A common tidyverse leaf they already have costs nothing.

So the cost of a dependency is not its own size: it is **the size of the part of its tree that is
neither already present nor common**.

### The jamovi `.jmo`: every Suggest is payload

⚠ **`jamovi-compiler` reads `Depends`, `Imports`, `Suggests` and `LinkingTo`, concatenates all four,
and installs everything not already in jamovi's own bundled library into the module's build
directory.** There is no per-package opt-out; `skipDeps` skips dependency installation entirely.
Verified in `compilerr.js`. That is why a test-only Suggest is pure `.jmo` dead weight, and why
`DescTools` and `bench` no longer appear in either list.

**What jamovi already bundles is free.** jamovi 2.7.36 ships ~124 R packages, among them everything
tabxplor leans on hardest:

```text
cli  data.table  dplyr  fansi  fs  ggplot2  glue  gtable  htmltools  jmvcore  knitr  lifecycle
magrittr  MASS  Matrix  mgcv  nlme  nnet  openxlsx  pillar  pkgload  purrr  R6  Rcpp  rlang
rmarkdown  scales  stringi  stringr  survival  testthat  tibble  tidyr  tidyselect  vctrs  withr
yaml  broom  backports  base64enc  digest  evaluate  generics  jsonlite  lattice  xfun  ...
```

List it yourself — it is the authority, not a web page:

```bash
flatpak run --devel --command=sh org.jamovi.jamovi -c 'ls /app/lib/R/library'
```

⚠ **Two consequences that change the priorities:**

- **`stringi` was already bundled by jamovi.** Removing it from Imports bought a CRAN slot and
  removed 143 direct call sites — it bought the `.jmo` **nothing**. Same for `broom`, `htmltools`,
  `knitr`, `fs` and `gtable`: every one of them is in jamovi's library.
- **A test-only Suggest is pure `.jmo` dead weight.** `DescTools` costs **21.3 MB** and is used by
  four test files and nothing else.

**Measured, this phase:**

| jamovi `.jmo` payload | packages |        size |
|:----------------------|---------:|------------:|
| before Phase 22l      |      114 |    156.8 MB |
| after 22l             |       47 |     58.1 MB |
| after 23e             |       25 |     36.3 MB |
| after `kableExtra`    |   **23** | **31.8 MB** |
| **removed in total**  |   **91** | **125.0 MB** |

`ggpubr` (→ rstatix, ggsci, ggsignif, polynom), `FactoMineR` (→ DT, emmeans, leaps, scatterplot3d,
showtext, …), `questionr` (→ shiny, miniUI, styler, httpuv, promises, later) and `car` (→ lme4,
quantreg, nloptr, pbkrtest, SparseM, MatrixModels) accounted for most of it.

### What is left, and what each would save

Ranked by what dropping it removes from the `.jmo` — the closure, not the package.

| Suggest                                |                          `.jmo` saving | what it costs the user to lose                                                                                                                            |
|:---------------------------------------|---------------------------------------:|:----------------------------------------------------------------------------------------------------------------------------------------------------------|
| `DescTools` *(gone, 23e)*              |                            **21.3 MB** | ⚠ **nothing** — it was test-only. Its parity tests now live in `dev/tests/testthat/`, so the tree (readr, haven, httr, readxl, vroom, tzdb, bit64, e1071, mvtnorm, …) is gone with it. |
| `VGAM` + `svyVGAM`                     |                                 8.4 MB | survey-weighted multinomial, a specialist path                                                                                                            |
| `marginaleffects`                      | 7.0 MB (+ insight, checkmate, Formula) | `effect = "at_reference"` only; the g-computation engine is tabxplor's own                                                                                |
| `openxlsx2`                            |                                 4.5 MB | Excel export — ⚠ this one the jamovi module's own export button uses                                                                                      |
| `kableExtra` *(gone, 22l-bis)*         |                     4.5 MB (+ svglite) | ⚠ **nothing** — it had zero function calls in `R/`; tabxplor now ships its own html dependency (see *Vendored code*)                                       |
| `mirai` + `parallelly` + `RhpcBLASctl` |                    2.9 MB (+ nanonext) | parallelism — ⚠ of no use *inside* jamovi, which runs its own process model                                                                               |
| `gridExtra`                            |                                 0.7 MB | the model-check panel grid                                                                                                                                |
| `bench` *(gone, 23e)*                  |                     0.6 MB (+ profmem) | ⚠ nothing — test-only and opt-in even there; moved with `test-benchmark.R`                                                                                 |
| `survey`                               |        6.4 MB **of the Imports floor** | ⚠ dropping it alone saves **nothing** while `svyVGAM` is a Suggest — `svyVGAM` Depends on it. See the options table.                                      |
| `forcats`                              |                                 0.5 MB | ⚠ same trap: `haven` (inside `DescTools`) pulls it, so it is free only once DescTools goes                                                                |
| `brant`, `clipr`, `rstudioapi`, `yaml` |                                     ~0 | tiny, or already in jamovi's library                                                                                                                      |

⚠ **The `.jmo` floor is `survey`.** Of the 6.9 MB that Imports alone force into the bundle, **6.4 MB
is survey and its tree** — survey (4.1) → minqa, numDeriv, mitools → **DBI (1.6)**. Everything else
in Imports is already in jamovi's library. `forcats` is the only other 0.5 MB.

⚠ **The free wins were the three that cost a user no feature at all** — 26.4 MB in total.
`DescTools` + `bench` were test-only, and Phase 23e moved their parity tests to
`dev/tests/testthat/`, where an undeclared package is allowed. `kableExtra` shipped nothing but an
S3 class. Everything still on the list buys something real, so from here the trade stops being free.

⚠ **The same list ranks the third audience's cost.** Most of the **23** remaining payload
packages need compilation, and the heaviest are exactly the exotic ones a student's machine will
have to fetch and build from scratch — `VGAM`, `openxlsx2`, `survey`, `nanonext`. Nothing common is
left: every tidyverse-shaped package tabxplor uses is already
either installed on a machine that has any of them, or bundled by jamovi. So on a shared university
network the download is now dominated by **`VGAM`**, which a weighted multinomial really does need.

### Re-measuring

Every number in this document is the output of one script. **Re-run it after any DESCRIPTION
change** rather than trusting the figures above:

```bash
Rscript dev/dep_footprint.R
```

It reads jamovi's bundled list from the flatpak when it can (falling back to a recorded 2.7.36
snapshot), takes the runtime closure of Depends + Imports, and prices each Suggest by **its own
closure** — reporting, where a saving is zero, which other package still pulls it. Sizes come from
the local library, so it names anything not installed here and counted as 0 MB.

⚠ A `.jmo` is per (OS, architecture, jamovi series): a compiled dependency multiplies across every
platform you ship, which is a second reason to prefer a pure-R one. And `LinkingTo` counts for the
bundle even though it is build-only — which is why `RcppArmadillo` sits in the build directory,
dragged in by `survey`.

## Imports — what each is for

| package      | why it cannot be a Suggest                                                                                                                                                                                                                                                                                                                                                                                |
|:-------------|:----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| `dplyr`      | ~725 call sites, and 15 generics implemented as S3 methods in NAMESPACE                                                                                                                                                                                                                                                                                                                                   |
| `purrr`      | ~658 call sites                                                                                                                                                                                                                                                                                                                                                                                           |
| `rlang`      | ~348 call sites, plus `importFrom(rlang, .data)`                                                                                                                                                                                                                                                                                                                                                          |
| `cli`        | every message and abort in the package                                                                                                                                                                                                                                                                                                                                                                    |
| `vctrs`      | `import(vctrs)`: the `fmt` record and ~55 bare S3 method definitions                                                                                                                                                                                                                                                                                                                                      |
| `tibble`     | every table IS one                                                                                                                                                                                                                                                                                                                                                                                        |
| `tidyselect` | the argument surface's tidy-select                                                                                                                                                                                                                                                                                                                                                                        |
| `pillar`     | 4 generics (`pillar_shaft`, `tbl_sum`, `tbl_format_body`, `tbl_format_footer`) as S3 methods                                                                                                                                                                                                                                                                                                              |
| `data.table` | `import(data.table)`: the aggregation core's `.SD` / `.N` / `:=`                                                                                                                                                                                                                                                                                                                                          |
| `lifecycle`  | the deprecation engine, and `badge()` in generated roxygen                                                                                                                                                                                                                                                                                                                                                |
| `forcats`    | 13 functions over ~80 sites, and `gss_cat` in `gss_cat_data_formatting()`                                                                                                                                                                                                                                                                                                                                 |
| `tidyr`      | `pivot_wider` / `pivot_longer` on `fmt` columns; `replace_na` ×17                                                                                                                                                                                                                                                                                                                                         |
| `survey`     | ⚠ **not only a survey dependency** — `svyglm()` is the FITTER for the three quasi-likelihood families `rr` / `rd` / `mr` (`link = "ratio"` or `"difference"` on a binomial or gaussian outcome) **even with no weights**, because a misspecified likelihood needs robust standard errors; `regTermTest()` then supplies their `wald_null` footer row. `svyrecvar` owns the design-based variance algebra. |
| `nnet`       | `nnet::multinom()`, the multinomial engine                                                                                                                                                                                                                                                                                                                                                                |
| `MASS`       | `MASS::polr()`, the proportional-odds engine                                                                                                                                                                                                                                                                                                                                                              |

## Suggests — what each is for, and its guard

Guards live at the entry point. `tx_need_pkg(pkgs, what, severity)` (`R/utils.R`) is the one spelling:
`severity = "abort"` where the feature cannot degrade, `"inform"` where it can. It names **every**
missing package of one request at once and gives the exact install line.

| package                                           | feature                                                                     | guard                                  |
|:--------------------------------------------------|:----------------------------------------------------------------------------|:---------------------------------------|
| `openxlsx2`                                       | Excel export                                                                | abort, in `tab_xl()`                   |
| `rmarkdown` + `htmltools`                         | the Viewer page, and the tooltip / popover binding in a knitted document    | inform, degrades to a plain print      |
| `clipr`                                           | `tab_md(clipboard = TRUE)`                                                  | inform                                 |
| `ggplot2`, `gridExtra`                            | `forest_plot()`, `reg_check_plots()`, the Excel check images                | abort (inform for the images)          |
| `marginaleffects`                                 | `effect = "at_reference"` only — the g-computation engine is tabxplor's own | abort                                  |
| `VGAM`, `svyVGAM`                                 | survey-weighted multinomial                                                 | abort, named together                  |
| `brant`                                           | the proportional-odds test                                                  | inform, the row is simply absent       |
| `mirai`, `RhpcBLASctl`, `parallelly`, `pkgload`   | the parallel seam, opt-in behind `tabxplor.parallel`                        | inform / silent fallback               |
| `jmvcore`, `R6`                                   | the jamovi module                                                           | `requireNamespace()` at namespace-load |
| `rstudioapi`                                      | console theme detection, `fct_recode_helper()`                              | `requireNamespace()`, base fallback    |
| `fansi`, `rmarkdown`, `knitr`                     | the vignettes (`knitr` is also `VignetteBuilder`)                           | —                                      |
| `testthat`, `withr`, `yaml`                       | the test suite                                                              | `skip_if_not_installed()`              |

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

Three things live in tabxplor rather than in a dependency. All are GPL (>= 2) or MIT → GPL (>= 3),
all carry their attribution in a comment above them, and each was verified against the original.

| what                                         | where                 | verified                                                                                                                                                     |
|:---------------------------------------------|:----------------------|:-------------------------------------------------------------------------------------------------------------------------------------------------------------|
| `htmltools::htmlEscape` → `tx_html_escape()` | `R/utils.R`           | identical for both `attribute` modes, encoding included                                                                                                      |
| `car::vif.default/polr/svyolr` → `tx_vif()`  | `R/reg-assumptions.R` | `all.equal(tolerance = 1e-13)` on 14 fit shapes; `NULL` exactly where car errors or returns `NaN`. Re-run `dev/vif_car_parity.R` after **any** change to it. |
| kableExtra's `kePrint.js` → `inst/tabxplor-1.0/tabxplor.js` | `R/tab-render-html.R` (`tx_html_deps()`) | ten lines binding the bootstrap tooltip / popover plugins to attributes the html engine already writes. jQuery and bootstrap come from `rmarkdown`; kableExtra's `lightable.css` is deliberately not reproduced. |

`tx_vif()` implements Fox & Monette (1992) GVIF and returns **both** of car's shapes, so both call
sites read it unchanged. It refuses rather than approximates: fewer than 2 terms, aliased
coefficients, a rank-deficient fit, a matrix-coefficient fit (multinomial) or a singular vcov all give
`NULL`, and the collinearity row is then simply absent.

⚠ `tx_html_deps()` returns `NULL` when `rmarkdown` or `htmltools` is absent, and every caller
degrades. Nothing breaks when it does: the cells' `title=` attribute is a native browser tooltip on
its own, so a table still hovers — unstyled. **Popovers** are the part that genuinely needs the JS.
The Viewer branch is `interactive()`-only, so the suite can prove the dependency list and the print
predicate but never that a tooltip appears; that check is manual.

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

⚠ **stringi is still installed**, via `tidyr` → `stringr` → `stringi` — and jamovi bundles it
anyway. Dropping it from Imports bought a CRAN slot and removed a direct coupling and 143 call
sites; it removed the package from **nobody's** library and **nothing** from the `.jmo`. Only
dropping `tidyr` would change the first, and nothing would change the second.

## Costed options that were not taken

Ordered by value. The `.jmo` column is the whole point: it is where a Suggest actually costs
something, and it is not proportional to what CRAN or a plain `install.packages()` sees.

| option                              |    CRAN     |                                      `.jmo` | what it costs                                                                                                                                                                                                                                                                                                                                      |
|:------------------------------------|:-----------:|--------------------------------------------:|:---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| **`DescTools` out of Suggests**     |      —      |                                **−21.3 MB** | ✅ **DONE (Phase 23e).** Test-only: CI parity in four files, validating the closed-form CI engine against an independent implementation. That validation is kept, as `dev/tests/testthat/test-tab-agg-sweep.R` and `test-survey-variance-sweep.R`. |
| **`bench` out of Suggests**         |      —      |                                     −0.6 MB | ⚠ also test-only, and opt-in even there (`TABXPLOR_BENCH=true`). `helper-benchmark.R` already falls back to `system.time()`. Same phase, same argument.                                                                                                                                                                                            |
| `VGAM` + `svyVGAM` → refuse instead |      —      |                                     −8.4 MB | survey-weighted multinomial would stop working rather than degrade. A specialist path, but a real one.                                                                                                                                                                                                                                             |
| `marginaleffects` → refuse instead  |      —      |                                     −7.0 MB | `effect = "at_reference"` only; tabxplor's own g-computation covers `effect = "marginal"`.                                                                                                                                                                                                                                                         |
| `survey` → Suggests                 |     −1      | **−6.4 MB** (93% of the Imports-only floor) | `svyglm()` fits `link = "ratio"` / `"difference"` **even unweighted**, and those are exactly the measures the *All else equal* article teaches; `regTermTest()` supplies their `wald_null` row. Guarding them like `VGAM` is possible and would make a taught feature refuse on a bare install. Measured, not recommended — the maintainer's call. |
| `tidyr` → base R                    |     −1      |                                          ~0 | ⚠ **buys nothing for jamovi** (tidyr and stringi are both bundled) and nothing for CRAN beyond the slot. `pivot_wider`/`pivot_longer` on `fmt` columns is the blocker: 9 sites, and the reshape must preserve every record field and column attribute. Hard, and now clearly not worth it.                                                         |
| `forcats` → base R                  |     −1      |                                     −0.5 MB | 13 functions over ~80 sites; `fct_lump_min`, `fct_collapse`, `fct_relabel` are the fiddly ones, and `gss_cat` would still be needed by `gss_cat_data_formatting()` and ~35 examples.                                                                                                                                                               |
| `MASS` / `nnet` → Suggests          |     −2      |                                           0 | free to every user (both are R Recommended, and jamovi bundles them) but buys only CRAN slots: guarding two `reg_check_deps()` clauses plus skip guards on ~27 test sites. Do it only if the count nears 20.                                                                                                                                       |
| promoting any Suggest to Imports    | **+1 each** |                                           0 | ⛔ contradicts rule 1, and every candidate is huge (`VGAM`), compiled (`openxlsx2` → Rcpp) or opt-in-only (`mirai`, `RhpcBLASctl`, `parallelly` — and parallelism is of no use inside jamovi anyway). `tx_need_pkg()` already gives a one-line install command.                                                                                     |

⚠ **What this table says about the work already done:** the three Imports removed for CRAN's sake
(`stringi`, `broom`, `htmltools`) and `fs`/`gtable` were all **already bundled by jamovi**, so they
bought the `.jmo` nothing. What bought the `.jmo` its 98.6 MB was dropping `ggpubr`, `FactoMineR`,
`questionr` and `car` — four *Suggests*, none of them an Import. Optimise Suggests for size, Imports
for count; they are different problems.

## Verification recipe

```bash
# the suite (alone, never beside another R process -- see CLAUDE.md § Testing)
OMP_NUM_THREADS=1 Rscript -e 'Sys.setenv(TESTTHAT_CPUS="8", NOT_CRAN="true"); devtools::test("~/github/tabxplor")'

# the dependency footprint, for all three audiences (CRAN count, .jmo MB, plain install)
OMP_NUM_THREADS=1 Rscript dev/dep_footprint.R

# the vendored VIF, against the real car (car is NOT a dependency, so this cannot be a test)
OMP_NUM_THREADS=1 Rscript dev/vif_car_parity.R

# rebuild the data sets (needs FactoMineR, questionr, carData)
OMP_NUM_THREADS=1 Rscript data-raw/DATASETS.R
```

After any string-handling change, the proof is that `test-golden.R`, `test-export-parity.R`,
`test-fmt-contract.R`, `test-fuse-parity.R` and every `_snaps/` file stay **byte-identical**: the 36
`_golden/` fixtures exist for exactly this.
