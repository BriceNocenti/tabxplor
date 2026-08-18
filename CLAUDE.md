# tabxplor — AI Assistant Guide

## What tabxplor is, and why

`tabxplor` is a public CRAN R package (v2.0.0). it builds **colour-coded cross-tables and regression tables for data exploration** then a **publication-ready black and white version**. The one idea behind everything: colour or typography lets you *read a table at a glance*. Over-represented cells turn blue (or bold), under-represented ones red (or italics), deeper colour (or underlines) means a stronger deviation — and a cell is only coloured/typoed when the difference is statistically solid, so structure jumps out instead of being scanned number by number.

It sits at the intersection of three things most tools keep separate, and its architecture exists to unify them:

- a **display engine** — colour and typography that encode statistics (effect size *and* significance at once), the same visual language across console, HTML, Excel, Markdown/Quarto and plots;
- a **rich cell data-model** — every cell is a `vctrs` record carrying all the numbers behind the one it shows, so tables stay ordinary `dplyr`-manipulable tibbles and the display can switch losslessly;
- a **statistical-inference layer** — exact survey/design-effect variance (reproducing the `survey` package), named CI methods (Wilson, Newcombe, Katz, Woolf, Welch), Haberman adjusted residuals. **regression tables** get the same language plus the observed-vs-modelled comparison.

The target users are : 1. a "literary" social sciences student, not good at math, learning to read equiped crosstables and regression models using colors ; 2. a serious quantitative analyst — survey researcher, sociologist — often working with **weighted or complex-survey data**. That is why the inference layer is unusually deep for an exploration tool, and why `tab_reg()` pairs a model's adjusted effect with its **observed (crude) counterpart**, so "what did controlling for the other variables actually change" is visible in one table.

Two design principles underpin the whole package:

1. **Every cell carries all its statistical data.** A numeric cell is a `tabxplor_fmt` record (count, weighted count, percentage, difference, ratio, contribution, CI bounds, odds ratio, p-value, …). Changing what is displayed never recomputes or loses anything.
2. **Tables are tibbles.** Results inherit from `tibble` (`tabxplor_tab` / `tabxplor_grouped_tab`), so every `dplyr` verb works while table metadata and formatting survive.

**Performance:** aggregation runs on `data.table` internally; the user only ever sees tibbles of `fmt` columns.

**Dependencies are pay-as-you-go:** table building and core inference are always available (hard Imports include `data.table`, `broom`, and the stats engines `survey`/`nnet`/`MASS`); exporters, plotting, parallelism, jamovi and advanced regression backends are all Suggests, guarded at their entry points.

---

## Repository Map

R files (`R/`) are grouped into seven subsystems. Every file carries a header comment with fuller design detail: read it for more details.

**Core type system** — the `fmt` record, the table classes, the row/table identity.

- `fmt_class.R` — the `tabxplor_fmt` vctrs record (the rich cell): fields, attributes, arithmetic, colour engine; the `MEASURES` / `EST_SCALES` fact tables.
- `tab_classes.R` — `tabxplor_tab`/`grouped_tab` S3 classes, dplyr methods, print, palettes/breaks, `tab_compact()`/`tab_plot()`, the `test` footer; `COLOR_SCALES`.
- `row-model.R` — the row axis: `row_kind` field + `tabxplor_lvl` factor subclass; `ROW_KINDS`; level operations.
- `table-spec.R` — the table identity `meta$spec` (kind / vars / call).
- `tab-shape.R` — `tab_shape()`/`tab_supports()`/`tab_columns()`: which reshape ops accept which shape; `TAB_OPS`.

**Crosstab API and pipeline** — building a table from microdata.

- `tab.R` — `tab()` and the `tab_build()` staged pipeline; `tab_prepare`, `tab_spread`, `tab_transpose`, the settings spine, `new_ctx()`.
- `tab-leaf.R` — the aggregate core: `tab_plain`/`tab_num`, `plain_core`/`num_core`, the leaves' CI/chi2, total rows.
- `tab-agg.R` — sufficient-statistic aggregation + the CI engine; `CI_METHODS` / `CI_GEOMS`.
- `tab-chi2.R` — the whole-table chi²/ANOVA test and the per-cell contribution writer.
- `tab-display.R` — the `{}` display grammar and `add_n`/`add_pct`; `DISPLAY_TOKENS`.
- `tab-resolve.R` — the crosstab argument boundary (validation + the colour/settings cascade).
- `tab-counts.R` — `tab_counts()`, the from-aggregated-counts constructor.
- `tab-parallel.R` — serial/parallel row-axis dispatch (mirai, Suggests-only).
- `tab-deprecate.R` — the 1.x → 2.0 translation shims + the superseded `tab_many()`.
- `tab-steps-legacy.R` — the superseded dplyr-era step API (`tab_pct`/`tab_ci`/`tab_chi2`/…), sharing arithmetic with the leaves.

**Arguments, options, integrity** — the surface as data.

- `tab-args.R` — the argument surface: `TAB_ARGS` / `EXPORT_ARGS` drive signatures, value lists and `@param` prose.
- `tab-options.R` — the option subsystem: `TAB_OPTIONS` + the generated `?tabxplor-options` page.
- `zzz-fact-keys.R` — `TAB_FOREIGN_KEYS`: cross-table foreign-key checks run at load.
- `utils.R` — `.onLoad()` (seeds options), factor/list/string utilities, deprecation helpers.

**Regression** — `tab_reg()` and its model machinery.

- `tab_reg.R` — `tab_reg()`: fits per column, renders per-family effect measures, the staged `reg_build()`.
- `reg-resolve.R` — the `tab_reg()` argument boundary (`reg_resolve_args`, six stages).
- `reg-estimand.R` — effect × measure → fit/estimand; `REG_ESTIMANDS` / `REG_FAMILIES`; `reg_measures()`.
- `reg-empirical.R` — the observed/crude companion columns; `REG_EMPIRICAL`.
- `reg-influence.R` — influence-function math for the gap SE (g-computation, `svyrecvar`).
- `reg-assumptions.R` — model checks + `shape=` cures; `REG_CHECKS`; the plot primitives.
- `reg-spec-build.R` — the per-model product builder (`reg_spec_build`).

**Survey** — design-based inference.

- `survey-design.R` — the design boundary/unwrap, constructors, robust omnibus tests, the inference basis.
- `survey-variance.R` — design-based cell variance → the `n_eff` field; the flat closed form.

**Exporters and rendering** — one visual language, every medium.

- `tab-export.R` — the `tab_export()` dispatch facade.
- `tab-export-prep.R` — the shared exporter prep + ephemeral render model.
- `tab-render-html.R` — `tab_html()` + the dependency-free HTML `<table>` engine + tooltips.
- `tab_md.R` — the Markdown exporter (pandoc colour spans).
- `tab_xl.R` — the Excel exporter (openxlsx2, colours/bold, numFmt from `format(syntax = "excel")`).
- `tab-xl-backend.R` — openxlsx2 wrappers + the range coalescer.
- `tab-css.R` — the one CSS generator (`tab_css`); light/dark/print themes.
- `tab-test-display.R` — the shared `test`-attribute renderer (console + export footers); `TEST_ROWS`.
- `tab-transpose-render.R` — the render-level transpose seam.
- `tab-theme-detect.R` — best-effort console light/dark detection.
- `plots.R` — `forest_plot()`, `reg_check_plots()`, and the `tab_estimates()` chart model.

**Jamovi** — the point-and-click modules.

- `jmvtab.b.R` / `jmvtab.h.R` — Crosstables backend (R6) + generated options.
- `jmvtab-cache.R` — the crosstab live-UI cache + the engine-free build core.
- `jmvtab-export.R` — jamovi export helpers + the shared `jmv_backend_*` R6 helpers.
- `jmvtabreg.b.R` / `jmvtabreg.h.R` — Regressions backend + generated options.
- `jmvtabreg-cache.R` — the regression fit cache + `jmvtab_reg_build()`.

**Cross-cutting** (touch with care): `fmt_class.R` is the foundation of every column; `.onLoad()` in `utils.R` seeds every option; `format.tabxplor_fmt()` and `fmt_color_channels()` are the shared display/colour sources of truth across all backends.

**Other directories:** `vignettes/` (user + regression + programming vignettes, each with a French twin) · `tests/testthat/` (testthat v3) · `man/` (roxygen-generated, never edit) · `inst/i18n/` + `po/` (translations) · `jamovi/` (module definition) · `dev/` (architecture guide, dev scripts, perf harness, `.Rbuildignore`'d).

---

## tabxplor architecture

### How a table is built

Three kinds of input converge on one output through one pipeline:

```text
 data source           build                     result              render
 ───────────           ─────                     ──────              ──────
 microdata     ┐
 aggregated    ┼──►  aggregate core  ──►  fmt cells  ──►  tabxplor_tab  ──►  colour engine  ──►  console
 counts        │     (sufficient          (rich vctrs      (tibble +          (per cell)          html · xlsx
 a fitted model┘      statistics)           records)         metadata)                             markdown · plot
```

There are **two producers, one output type**. `tab()` builds crosstabs from microdata; `tab_reg()` builds regression tables from a model. Both emit a `tabxplor_tab` of `fmt` columns, so the colour engine, the accessors, the reshape operations and every exporter treat them identically — one visual language, one export path. `tab_counts()` is a third entry that starts "from the middle", building the same object from already-aggregated counts.

### The declarative architecture

The codebase is organised around **declared fact tables**. Instead of scattering literals and re-deriving `switch` statements, each fact — a colour measure, an option, an argument, an estimand, a display token, a kind of row — is stated **once, in one table**, and read through named accessors. The single rule a future change must respect: *every fact is stated once, in one declared table; a key one table reads out of another is a foreign key, checked at load* (`zzz-fact-keys.R` validates every edge when the namespace loads, so a rename that breaks a reference fails the install, not a user's table).

The payoff to internalise: **adding a measure, an option, an argument, an estimand is one new row — not N scattered edits.** Do not re-introduce ad-hoc branches; extend the table. The main fact tables:

| Fact table         | Home                 | Declares                                                                               |
|--------------------|----------------------|----------------------------------------------------------------------------------------|
| `MEASURES`         | `fmt_class.R`        | The colour measures (raw field, scale keys, significance source, legend, requirements) |
| `EST_SCALES`       | `fmt_class.R`        | What a column estimates (field, null, geometry, colour ladder, SD source)              |
| `DISPLAY_TOKENS`   | `tab-display.R`      | The `{}` display grammar (field source, geometry, aliases, placement)                  |
| `CI_METHODS`       | `tab-agg.R`          | The confidence-interval methods and geometries (with `CI_GEOMS`)                       |
| `COLOR_SCALES`     | `tab_classes.R`      | The break scales and palettes                                                          |
| `TAB_ARGS`         | `tab-args.R`         | The argument surface (signatures, values, option twins, prose; + `EXPORT_ARGS`)        |
| `TAB_OPTIONS`      | `tab-options.R`      | The package options and their defaults                                                 |
| `ROW_KINDS`        | `row-model.R`        | The row-kind vocabulary                                                                |
| `TEST_ROWS`        | `tab-test-display.R` | The footer / statistical-row catalogue                                                 |
| `TAB_OPS`          | `tab-shape.R`        | Which reshape operations accept which table shape                                      |
| `REG_ESTIMANDS`    | `reg-estimand.R`     | What a regression column estimates (family × effect × measure)                         |
| `REG_EMPIRICAL`    | `reg-empirical.R`    | The crude-companion column shapes per family                                           |
| `REG_CHECKS`       | `reg-assumptions.R`  | The model-check / assumption catalogue                                                 |
| `TAB_FOREIGN_KEYS` | `zzz-fact-keys.R`    | The cross-table foreign-key edges, checked at load                                     |

Three supporting mechanisms carry the same spirit: **typed contexts** (`new_ctx()`, `new_reg_ctx()`) declare every value a pipeline threads, so a stage cannot read an undeclared field; **single argument boundaries** (`tab_resolve_common_args()`, `reg_resolve_args()`) normalise every producer's arguments in one place; and **one table identity** — `meta$spec`, with three slots `kind` / `vars` / `call` — says what a table is, read through `tab_kind()` / `tab_is_reg()`.

### The type system

#### tabxplor_fmt — the rich cell

`tabxplor_fmt` (`R/fmt_class.R`) is a `vctrs::new_rcrd()` record and the foundation of the package: every numeric column of a table is an `fmt` vector. It has **21 per-cell fields** and **16 per-column attributes**.

**Fields** (per-cell, via `vctrs::field()`):

| Field       | Type | Meaning                                                                             |
|-------------|------|-------------------------------------------------------------------------------------|
| `n`         | int  | Unweighted count                                                                    |
| `wn`        | dbl  | Weighted count                                                                      |
| `pct`       | dbl  | Percentage, stored 0–1 (×100 only in `format()`)                                    |
| `mean`      | dbl  | Cell mean (numeric column variables; `NA` on pct columns)                           |
| `tot_n`     | dbl  | The cell's own unweighted percentage base (row/col/grand total per `pct`)           |
| `diff`      | dbl  | Difference from the reference                                                       |
| `ratio`     | dbl  | Ratio to the reference (the "×2" reference-relative ratio the colour engine reads)  |
| `or`        | dbl  | Odds ratio / relative-risk ratio                                                    |
| `obs`       | dbl  | The observed/crude value a `tab_reg` estimate is compared to (`NA` elsewhere)       |
| `gap_se`    | dbl  | SE of the gap between the estimate and `obs` (drives `color_signif` on adjustments) |
| `ctr`       | dbl  | Contribution to chi-squared variance                                                |
| `var`       | dbl  | Variance (CI / effect size)                                                         |
| `ci_inf`    | dbl  | Lower confidence-interval bound                                                     |
| `ci_sup`    | dbl  | Upper confidence-interval bound                                                     |
| `pvalue`    | dbl  | Per-cell significance p-value (CI-inversion; drives stars)                          |
| `n_eff`     | dbl  | Effective sample size for the CI (design-based; `NA` → raw base)                    |
| `display`   | chr  | Which field(s) to show — a `{}` template resolves to this                           |
| `digits`    | int  | Decimal places                                                                      |
| `row_kind`  | chr  | Kind of row: data / total / n / pct / pvalue / gof / blank                          |
| `in_tottab` | lgl  | Cell belongs to the total table                                                     |
| `in_refrow` | lgl  | Cell belongs to the reference row                                                   |

**Attributes** (per-column, via `attr()`):

| Attribute      | Type | Meaning                                                                                        |
|----------------|------|------------------------------------------------------------------------------------------------|
| `scale`        | chr  | What the column estimates — a key into `EST_SCALES` (`level_pct`/`mean_diff`/`odds_ratio`/…)   |
| `pct_base`     | chr  | What a percentage is OF (`row`/`col`/`all`/`all_tabs`/`none`) — the axis its reference lies on |
| `ci_method`    | chr  | Which interval engine built the bounds (`wilson`, `newcombe`, `welch`, `katz`, …; `""` = none) |
| `conf_level`   | dbl  | The confidence level the interval and thresholds were computed at                              |
| `degf`         | dbl  | Degrees of freedom the interval refers to (`NA` → refer to z)                                  |
| `basis`        | chr  | How the interval was computed: `n` / `weights` / `design` / `design_partial`                   |
| `col_var`      | chr  | Name of the column variable                                                                    |
| `col_group`    | chr  | Which sub-population the block belongs to (a spread level or `tab_vars` group; `""` otherwise) |
| `ref`          | chr  | Reference type (`tot` / `first`)                                                               |
| `comp_all`     | lgl  | Compare against the total table (TRUE) or the subtable (FALSE)                                 |
| `totcol`       | lgl  | This column is a total column                                                                  |
| `refcol`       | lgl  | This column is a reference column                                                              |
| `color`        | chr  | Colour measure (length 1, or 2 for a text+background channel pair)                             |
| `color_signif` | chr  | Significance policy: `ignore` / `grey_non_signif` / `guaranteed_effect`                        |
| `model_family` | chr  | A regression column's own family (`""` on crosstabs)                                           |
| `role`         | chr  | A regression column's role: `model` / `emp` / `n` (`""` on crosstabs)                          |

**The critical distinction:** fields vary per cell; attributes are scalar over a whole column. Do not confuse them. The record is deliberately **dense** — every column carries all 21 fields, an inapplicable one stored as `NA`, never absent — so the colour engine and tooltip builder read any field on any column and simply find `NA` where it does not apply (sparse fields buy almost nothing and would add a second encoding of "not applicable", so the shape is fixed).

The attribute list is **derived** from `new_fmt()`'s formals (attributes = formals that are not fields), and how each attribute is carried through casts, arithmetic and binds is itself a declared table (`fmt_attr_rules`: `neutral` / `merge` / `arith` / `scalar` / `write`). Adding an attribute is a `new_fmt()` formal plus one rule row; a build-time assertion refuses an attribute with no rule. Read/write any attribute by name with `fmt_attr()` / `` `fmt_attr<-` `` (the programmatic surface); the named `get_*`/`set_*` accessors are the taught surface. Constructor chain: `fmt()` (public, validates) → `new_fmt()` (internal).

**Adding a field** touches ~9 sites in `fmt_class.R` (the field list, `fmt()`, `new_fmt()`, the getters/setters, the four reconstructors) plus, for a *displayed* field, `get_num()`/`set_num()`, `format()`, `tab_xl` and a `DISPLAY_TOKENS` row — follow the `/vctrs-field` skill, which encodes the checklist.

#### tabxplor_tab — the table

`tabxplor_tab` is a `tibble` subclass; `tabxplor_grouped_tab` extends `grouped_df` when `tab_vars` split the table into sub-tables. Class and metadata survive `dplyr` through ~30 S3 methods, anchored by the `dplyr_row_slice()` / `dplyr_col_modify()` / `dplyr_reconstruct()` trio (a missing method silently downgrades to a plain tibble). A table carries three **optional, NULL-safe** attributes: `subtext` (legend text), `test` (a tibble of chi²/ANOVA/model-footer rows), and `meta` (one list holding `spec`, the variable model, CI settings, render intent, and any regression/assumption records). Every getter tolerates absence: a table stripped of `test` still prints, dropping only the summary it powered — cell fields and column attributes stay required, a standalone extracted `fmt` column formats and colours on its own.

#### The row model

Rows describe themselves the way columns do. The `row_kind` field (from `ROW_KINDS`: `data`/`total`/`n`/`pct`/`pvalue`/`gof`/`blank`) says what kind of row a cell sits in; `is_totrow()` is the derived read. The index columns are a `tabxplor_lvl` factor subclass carrying each level's `role` (level / variable / tab-variable) and originating `var`, so variable detection and rendering read stored facts rather than guessing from labels.

### The calculation pipeline

Both crosstab entry points are thin wrappers over `tab_build()`, a staged pipeline over a typed `ctx`:

```text
tab() / tab_many()                          [public; differ only in default output shape]
  └─ tab_build(ctx)
       ├─ tab_setup           resolve arguments → the settings spine (rows / cols / pairs)
       ├─ tab_prepare_pop     prepare the population once (filter, NA, lump, relabel, weights)
       ├─ tab_aggregate       sufficient-statistic aggregation (data.table)
       ├─ tab_build_tables    per row_var: tab_transform (cells + interval + test) → tab_assemble_tables
       └─ tab_assemble_output output shape (merge / spread / compact / unwrap)
```

**The settings spine** (`ctx$settings` = a `rows` / `cols` / `pairs` star schema) is where the row and column axes meet exactly once, so parallel argument vectors cannot recycle against each other. Each stage projects the spine into the bare names its resolution block reads, so a pre-resolution value can never leak into a computation.

**The aggregate core** (`tab-leaf.R` + `tab-agg.R`) is the single place microdata becomes cells: the leaves `plain_core()` (factors) and `num_core()` (numeric column variables) turn sufficient statistics into `fmt` fields, their confidence interval and the whole-table test in one pass — there is no separate step chain. The superseded dplyr-era steps (`tab_pct` → `tab_ci` → `tab_chi2` → …) are quarantined in `tab-steps-legacy.R`: still exported for back-compatibility, they reconstruct a plan from `fmt` markers but share the *arithmetic* (`ci_dispatch()`, `chi2_compute_test()`) with the leaves, so a step and a build cannot compute two different answers.

**The reference system:** `ref` picks the comparison baseline (`tot` / `first` / an index / a regex), reinterpreted by `pct` (a reference *row* under row%/means, a reference *column* under col%); `ref2` names the second level for odds ratios; `comp = "tab"` compares within each sub-table, `comp = "all"` against the total table. **Significance:** a cell is significant when its confidence interval excludes the null; the displayed p-value (and its stars) come from inverting that interval, so one CI-inclusion rule governs colour, greying and stars alike. Interval geometry (proportion pivot, mean difference, multiplicative log) is declared in `CI_GEOMS`, its method in `CI_METHODS`.

### The inference layer

**The survey-design boundary** (`survey-design.R`) is one unwrap point: a `survey` design passed as `data` becomes the microdata every engine already reads, plus its sampling weights and design metadata — so the crude columns, the AME, the tests and the footer are all design-weighted, and a `svyrepdesign`/`twophase` is refused rather than approximated.

**The inference basis** is the layer's central idea: how the *estimate* is computed (`wt`) and how the *interval and test* are computed (the basis) are **orthogonal**. The basis is one of `n` / `weights` / `design` / `design_partial`, and — with `conf_level`, `degf` and `ci_method` — it is stored **on each column, not on the table**, because `dplyr` drops table attributes and a number must never depend on one. A bind reconciles these by the weakest-claim rule, so a merge can only claim the inference its weakest part carried.

**Design-based cell variance** (`survey-variance.R`) feeds the existing `n_eff` field (effective sample size), so the ordinary CI machinery becomes design-aware with no new field. A plain weight column is a survey design at `ids = ~1`, where the general formula collapses to a per-cell closed form computed from the aggregate alone (Kish is its degenerate limit); a real design uses `survey::svyrecvar`. survey owns the variance algebra throughout.

### The colour system

Colour is decomposed into three orthogonal axes: a **measure** (what to compare — `diff` / `ratio` / `contrib` / `or`), a **channel** (text and/or background), and a **significance policy** (`color_signif`: `ignore` / `grey_non_signif` / `guaranteed_effect`). The engine has three layers:

1. **Palettes** — OKLCH colour ramps, hand-tuned so intensity levels are distinguishable, in light, dark, and 8-bit (non-truecolor terminal) variants, set via `set_color_palette()`.
2. **Breaks** — per-scale thresholds (`COLOR_SCALES`), mirrored for the under side; a break value above 1 means a *ratio* comparison (the "×2" rule), so the default pct breaks encode both additive and multiplicative thresholds.
3. **Selection** — a vectorised `findInterval` engine (`fmt_color_plan` → `fmt_color_slots` → `fmt_color_channels`) that folds each cell per side and picks the strongest matching threshold.

The measure's behaviour — its raw getter, scale keys, significance source and gating — lives in the `MEASURES` row, which drives both the plan and the legend (no per-measure branches). Every backend consumes the one artifact `fmt_color_channels` produces, which is why console, HTML, Excel, Markdown and plots colour identically.

### The regression subsystem

`tab_reg()` gives models the same visual language: it fits one model, renders per-family effect measures as `fmt` cells, and returns the same `tabxplor_tab` — same colours, same accessors, same exports. It reuses the 21 `fmt` fields unchanged; `obs` and `gap_se` carry the regression-specific facts, so there is no separate field set. The subsystem is seven files, each with a fuller header.

**The estimand — effect × measure** (`reg-estimand.R`, `REG_ESTIMANDS`). The user asks two questions: which **contrast** (`effect`: coefficient / marginal / at-reference) and which **measure** (odds ratio / ratio / difference / log). One declared row per `(family, effect, measure)` answers both: it names the internal fit/link, the exponentiate flag, the header word, and the stored `scale` the column carries. The **family is auto-detected** from the outcome — a 2-level factor → logistic/OR, numeric → linear/β, a count → Poisson/IRR, 3+ unordered → multinomial, ordered → cumulative-OR ordinal — but one table can mix families (each column stores its own `model_family`). `reg_measures(data, outcome)` lists what an outcome offers; a missing `(effect, measure)` combination aborts with the list of what it does offer.

**The observed companion — the distinctive feature** (`reg-empirical.R` + `reg-influence.R`). With `empirical = TRUE`, each model effect is placed beside its **crude/observed counterpart** on the same scale — so "what did adjustment change" is read directly. `REG_EMPIRICAL` declares, per family, the shape of the crude column and its CI method; the crude value comes either from a closed form on a per-cell grid or from a univariable refit through the same fitter, so the two share estimand, link and CI rule by construction. `reg-influence.R` computes the **standard error of the gap** between the adjusted and crude estimates (their covariance, which no arithmetic on the two printed intervals could recover) via influence functions — the package's only `survey::svyrecvar` caller — and that gap SE is what lets `color_signif` colour the adjustment itself.

**The argument boundary** (`reg-resolve.R`). `reg_resolve_args()` is the `tab_reg()` analogue of the crosstab boundary: six declared stages (validate → prepare data → resolve estimands → resolve output → resolve fit plan → resolve specs) that do every check and every rewrite of `data` in one ordered place, returning a typed record the builder reads.

**The staged build** (`tab_reg.R` + `reg-spec-build.R`). `reg_build()` runs over a typed `new_reg_ctx`, one named stage per part of the table it produces; the per-model half is a declared product (`reg_spec_build()`), so "what is per-model vs between-models" has one answer. The three nesting axes — `tab_vars` groups × models × outcomes — dispatch through the shared parallel seam. The stage order is the source order and is load-bearing: every fit may emit a message, and the characterisation harness compares the message stream in order.

**Effects and model checks.** Marginal effects (AME, and MER at a reference profile) are computed by analytic g-computation (`reg-influence.R`) or `marginaleffects`, chosen per estimand. `REG_CHECKS` catalogues the model checks (linearity, dispersion, influence, proportionality, collinearity), each with the `shape =` cure that fixes what it flags — the check and its cure are one object — and each priced (`free` runs by default, `refit`-cost checks are opt-in).

### Exports and rendering

`tab_export(x, format =)` (`tab-export.R`) is the facade over four backends: HTML (the default), Markdown, Excel and plot. They share one preparation step, `tab_export_prep()` (`tab-export-prep.R`), which builds an ephemeral render model (roles, references, bold/italic, header spans, variable-name blocks) that every backend consumes.

Display values reach the backends by one source of truth: `format.tabxplor_fmt()` renders the text for console, Markdown and HTML; `tab_xl()` writes the raw value and takes its number-format codes from the *same* `format(syntax = "excel")`, so a display change never needs mirroring. Colour is single-sourced too — every backend reads `fmt_color_channels`. HTML colour is a slot **class**, never inline hex, with the theme living in a `<style>` block from the one CSS generator (`tab-css.R`), so `theme = "auto"` (light/dark) and `theme = "print"` (bold/italic, black-and-white) work by stylesheet. `tab-transpose-render.R` flips a finished render model (a transposed column is heterogeneous and cannot be an `fmt` column), and `tab-theme-detect.R` best-effort-detects the console's light/dark scheme — a subsystem that must never error, because a wrong guess only mis-tints, never breaks.

### jamovi

Two point-and-click analyses mirror the two producers: `jmvtab` (Crosstables) and `jmvtabreg` (Regressions). Each is a thin `R6` backend (`*.b.R`) over an engine-free build core (`jmvtab_build()` / `jmvtab_reg_build()`) that drives `tab()` / `tab_reg()` through a content-addressed **live-UI cache** (`*-cache.R`), so a repeated interactive tweak re-paints instead of recomputing. Each option is named after the argument it drives, so the backend is a pass-through rather than a translation table. The generated `*.h.R` option headers are never hand-edited.

### Cross-cutting invariants

Rules that span subsystems — do not undo them without reading why:

- **A number must not depend on a table attribute.** Inference facts (`conf_level`/`degf`/`basis`/`ci_method`) live on columns; `dplyr` drops table attributes.
- **A merge claims only what its weakest part carried** — the `vec_ptype2` reconcile applies the weakest-claim rule to inference attributes so a bind cannot over-claim.
- **Public API is stable; internals are free.** Soft-deprecate public arguments; the `fmt` fields users read with `$`/`mutate()` must not break.
- **Facts live in one table.** Add a row to a fact table and read it through its accessor; a foreign key checked at load keeps cross-table references honest.
- **The `fmt` record is dense.** Every column carries all fields; "not applicable" is `NA`, never an absent field.
- **`format()` is the one display source of truth** — text backends and the Excel numFmt codes both come from it.
- **Levels drop after the tests.** Non-first levels (`levels = "first"`) are removed only after chi²/CI, so tests see the full level set.
- **Theme detection must never error** — it rests on no supported API; anything unknown resolves to "light".

### Key Dependency APIs to read up on

Before working on the `tabxplor_fmt` type system, arithmetic, or display, fetch the help pages for these via the `r-btw` MCP **docs** tools (or `?`) — the model's recall of their exact current contracts is the weakest link:

- `vctrs::new_rcrd`, `vctrs::field` — record type and per-cell field access
- `vctrs::vec_arith`, `vctrs::vec_cast`, `vctrs::vec_ptype2` — arithmetic and casting S3 contracts
- `pillar::pillar_shaft` — console display method
- `data.table` reference semantics (`:=`, `.SD`, `.N`) — internal aggregation
- `DescTools::BinomCI`, `DescTools::BinomDiffCI` — **now Suggests-only** (test parity only). Since Phase 3a the CI math is the closed-form engine in `R/tab-agg.R` (`ci_pivot`/`ci_wilson`/`ci_newcombe`); read it, not DescTools, before touching CI.

### Documentation ecosystem

The docs form one hierarchy, general to specific. **Each fact is stated at exactly one layer, referenced (never duplicated) across the others, and always written present-tense** — the current design is the reference point, never how it got there. The one place dev history is allowed is the roadmap "DONE" summaries.

- **`## tabxplor architecture`** (this file) — the cross-subsystem big picture: goals, data-flow, the declarative pattern, the type system, each subsystem's role and its meaningful "why". Rewritten only when the maintainer asks, never per session.
- **`## Repository Map`** (this file) — the file index: one role line per R file.
- **R file-header comments** — per-file subsystem design: current architecture, key constraints, a pointer up to this file.
- **Inline `# DESIGN:` / `# WARNING:` tags** — the non-obvious "why" at the exact line.
- **Vignettes** (`vignette("tabxplor")`, regression, programming) — usage and teaching, for users.
- **Roxygen man pages** (`?tab`, `?tabxplor-vctrs`, `?tabxplor-options`, `?tabxplor-data.table`) — user-facing reference: usage and the main use cases, never build/internals/history.
- **`dev/*.md`** (`.Rbuildignore`'d) — transversal or expert technical guides only.
- **Roadmap "DONE" summaries → `dev/tabxplor_2.0.0_roadmap_DONE_PHASES.md`** — the ONLY place dev history lives.

Inspect a built table at runtime through the accessors: `tab_shape()`, `tab_columns()`, `reg_measures()`, `fmt_attr()`, and the `get_*` / `set_*` family.


---

## Deprecation and retro-compatibility

### For main user-facing functions and arguments
- This package have a small but existing users base : **soft deprecate main user-facing functions and arguments carefully** to ensure retro-compatibility.
- **It’s always possible to modify the main API for user-friendliness and integration** by **routing old arguments to new ones** and do *ad hoc* back-compat *after* having found a new more user-friendly API.
- Some user code rely on `tabxplor_fmt` vctrs fields extracted with `$` or calculated with `mutate()` method for `tabxplor_fmt` (see readme), so **the vctrs fields should not break**: even when the fields are changed internally, the old accessor and setter must still work.

### For internal code and internal functions
- **Do not hesitate to propose radical redesign of internal code and internal workflows** for quality, simplicity, structure, performance and future-proofing, specially when they are too convoluted or have grown organically.
- **Always try to simplify, integrate and create smart shared subfunctions** instead of adding a new layer of confusion and ad-hoc solutions inside the code: your main aim is to simplify, to remove traces of old implementations altogether when they have become useless, to clarify, to help me make relevant architectural choices instead of piling up ad-hoc solutions, to integrate the new features in the current code seamlessly.

---

## Testing

### How to run the suite (the ONLY sanctioned recipe — 2026-07-16)

```bash
# In a temp .R file (outside tests/), then run it EXACTLY like this, unsandboxed:
#   OMP_NUM_THREADS=1 Rscript that_file.R
# The .R file:  Sys.setenv(TESTTHAT_CPUS = "8", NOT_CRAN = "true"); devtools::test("~/github/tabxplor")
```

✅ **Since 2026-07-23 the suite SELF-PINS its threads** — `tests/testthat/setup.R` pins data.table
(`setDTthreads(1L)`) AND BLAS/OpenMP (`RhpcBLASctl::blas_set_num_threads(1L)`, Suggests-guarded — the
runtime call is the ONLY thing that can pin an already-running worker, since OpenBLAS-pthread fixes its
count from the env at process startup), and `tests/testthat.R` sets `OMP_NUM_THREADS=1` + a non-CRAN
`TESTTHAT_CPUS` fallback before workers spawn. So `devtools::test()`, `devtools::check()`, GH Actions
and CRAN all get 1 thread/worker with no manual env. Keep the `OMP_NUM_THREADS=1` prefix anyway
(harmless belt for grandchild processes and RhpcBLASctl-less setups).

⚠ **The trap this guards against** (root-caused 2026-07-16, second session lost to it; hit again by
`devtools::check()` 2026-07-23 before the self-pin): `Config/testthat/parallel: true` runs each test
file in its own PROCESS, and **each process then multi-threads on its own**:

| thread source                                | per worker | x 8 workers | lever                                              |
|----------------------------------------------|------------|-------------|----------------------------------------------------|
| data.table (defaults to 50 % of cores)       | 6          | 48          | `setDTthreads(1L)` — in `tests/testthat/setup.R`   |
| OpenBLAS *pthread* build (`lm`/`glm`/ggplot) | ~10        | ~80         | `RhpcBLASctl` pin in setup.R + `OMP_NUM_THREADS=1` |

**Measured: 165 threads on 12 cores (~14x oversubscribed) -> the suite ran >26 min instead of ~50 s**,
two workers pegged at ~485 % CPU while the rest starved and the log went silent for 10 min. With both
levers: **47 threads, 48.9 s, FAIL 0.**

**Never run anything else while the suite runs.** A single `Rscript` repro uses ~4 cores here; racing
it against 8 workers is what turns "slow" into "apparently hung". Iterate with `filter =`, and run the
full suite once, alone.

⛔ **Before blaming the code for a slow run, check whether YOU are the cause** — this is the companion
to the orphan rule below. In order: (1) is another R of mine running? (2) `ps -eLo pid,args | grep -c
"[-]-no-readline --slave"` — is the THREAD count >> 12? (3) only then look for orphans. A worker at
485 % CPU is oversubscription, not a hang.

```r
# One/few files while iterating (cheap, safe to repeat):
devtools::test("~/github/tabxplor", filter = "tab")  # regex on test-<name>.R
```

⚠ **A green local suite does NOT mean a green CI — this box is `fr_FR.UTF-8`.** GNU gettext ignores
`LANGUAGE` entirely when `LC_MESSAGES` is `C`/`POSIX`, which is the state under `R CMD check` on
Linux (check.R forces `LANGUAGE=en`, and testthat's `local_reproducible_output()` pins `LANG`/
`LANGUAGE` to `"C"` per block) **and on the CRAN farm**. So every French assertion passes here and
fails there. That is why French output is guarded by `skip_if_no_gettext()`
(`tests/testthat/helper-i18n.R`) and why each i18n feature is tested twice — an UNGUARDED English
block (the guard-rail that keeps the goldens from moving; must run everywhere) plus a GUARDED French
one.

**Never simulate CI, even before committing something really locale-touching and heavily do translation: only do it when the user call for it, when you explicitely now the user will push (not for every commit), at release, etc.**

```bash
LC_ALL=C.UTF-8 LANGUAGE=en OMP_NUM_THREADS=1 Rscript <runner>.R   # the CI locale
```

Use `C.UTF-8`, not `C`: plain `C` is *harsher* than any CI runner (non-UTF-8 native encoding), and
makes `test-non-ascii.R`'s own fixtures fail for reasons no CI job will ever hit.

⚠ **Two test/tooling steps need `dangerouslyDisableSandbox` here — root-caused 2026-07-16 from the bwrap
command line, do not re-diagnose:**

- **`test-parallel-parity.R` fails sandboxed** (`fail=1 err=7`, ~0.7 s) with
  `nanonext::.dispatcher_start: 16 | Permission denied`. Cause: bwrap runs **`--unshare-net`**, and
  mirai's dispatcher needs sockets. **Not a regression** — it passes 11/11 unsandboxed. Any full-suite
  run inside the sandbox reports these 8 as failures; ignore them or run that file unsandboxed.
- **`devtools::document()` fails sandboxed** with *"cannot open file 'NAMESPACE': Read-only file
  system"*. Cause: bwrap `--ro-bind`s `NAMESPACE` and `man/` specifically (the rest of the repo is
  writable, which is why snapshot writes succeed). Run it unsandboxed.

⚠ Dev now runs **inside WSL2 Ubuntu 26.04** (`~/github/tabxplor` on ext4), not Windows. The old `d:/Statistiques/github/tabxplor` paths are dead — the Windows checkout survives **build-only** for Windows `.jmo` (see *Jamovi module development*). The `~46s` / `225s -> 56s` suite timings recorded here were measured on Windows/NTFS and have **not** been re-measured on ext4 — treat them as order-of-magnitude only.

**Measured on ext4 / WSL2, 2026-07-16 (per-file, serial): total `359 s`, 2357 passing; slowest
`test-tab_reg.R` `33.6 s`, then `counts-parity` / `calculations` / `color-legend` ~23-25 s, most files
1-13 s.** Under `Config/testthat/parallel: true` the wall clock is roughly the SLOWEST FILE, so the
recorded `56 s` is consistent and still right. **A multi-minute run means something else is wrong — look
for orphans (below) before blaming the code.** Pass `TESTTHAT_CPUS=8`: `parallel: true` alone picks only
~2 processes here.

⛔ **NEVER kill a test run by killing its parent — you orphan the workers, and they do NOT stop.**
Measured 2026-07-16: two `TaskStop`'d suites left 6 R processes (2 `--file=…` parents + 4
`--no-readline --slave` testthat/mirai workers) alive for **52 minutes at ~860 % CPU** (one had burned
174 min of CPU time). They silently starve every later run — a suite that "takes 15 minutes" is usually
this, not the code. Symptoms + rules:

- **Diagnose AND kill unsandboxed — bwrap runs `--unshare-pid --proc /proc`**, so each Bash tool call
  gets its OWN PID namespace (`ps` shows the shell as PID 1). Two consequences: `ps aux` **cannot see
  the orphans**, and a *sandboxed* `kill <host-pid>` cannot kill them — worse, a low PID like `34`
  usually DOES exist inside the namespace, so it would kill **the wrong process**. Both `ps` and `kill`
  must run unsandboxed. Identify yours by the parent's
  `--file=/tmp/claude-…/<session-id>/scratchpad/…` — never by name alone (Positron runs its own R, and
  killing that is destructive).
- **Never `pkill -f <pattern>`.** Measured: `pkill -f testthat` matched and killed the calling shell,
  and `pkill -f t9.R` is what orphaned the workers (parent SIGKILLed -> exit 137, children reparented
  and kept running). Read `ps` first, then `kill` explicit PIDs.
- **Prefer not to create them**: run the suite in the foreground with a long timeout, or
  `filter =` to the files you touched. `setsid nohup … &` is ALSO killed when the tool's shell exits.
- **Never pipe a long run through `tail`/`head`** — they buffer until EOF, so the output file stays
  empty and the run looks hung. Write the incremental log to a file and read that.
- ⚠ Killing PIDs needs the maintainer: the auto-mode classifier denies it (rightly — this is a shared
  dev box). Surface the `ps` evidence and hand over the exact `kill -9 <pids>`.

**Test files:**

| File                      | Coverage                                                                                        |
|---------------------------|-------------------------------------------------------------------------------------------------|
| `test-fmt_class.R`        | fmt creation, printing, type conversion, c(), arithmetic                                        |
| `test-tab.R`              | Core: plain tables, pct, totals, NA, CI, chi2, references, wrapping                             |
| `test-tab_classes.R`      | Class preservation through dplyr verbs                                                          |
| `test-tab_xl.R`           | Basic Excel export                                                                              |
| `test-tab_reg-binomial.R` | Binary outcomes: OR/CI/p parity vs glm/svyglm, 1/OR (was test-tab_logit.R)                      |
| `test-tab_reg.R`          | Phase 12c/12d/12e: beta/OR/IRR/MNL/ordinal + AME parity vs lm/glm/multinom/polr/marginaleffects |
| `test-tab_reg-display.R`  | Phase 12h: estimate_display (est_ci bracket / prob / ame folds), Excel test label, split footer |
| `test-tab_reg-plots.R`    | Phase 12h / z15: reg_check_plots() smoke tests (build a gtable without error)                   |
| `test-tab-estimates.R`    | Phase 18z17: the estimate model + fmt_scale_of() -- no graphics device                          |
| `test-forest-plot.R`      | Phase 18z17: forest_plot() -- ladder == gridlines, cell colour == point, gap band == test       |
| `test-reg-shape.R`        | Phase 18z15: `shape =`, the plot primitives, the stored curves and the row sparkline            |

---

## Jamovi module development

tabxplor currently use jamovi `2.6.44.0` (solid). Version 2.0.0 will also be tested on jamovi current "solid" version `2.7.37` afterwards (Phase 7i confirmed 2.7.37 ✓).

✅ **jamovi IS installed on BOTH dev machines** — flatpak `org.jamovi.jamovi` **2.7.36**, bundled R **4.5.0**: the desktop WSL2 (migration Phase C3, 2026-07-16) and the **laptop WSL2 (Ubuntu 26.04, 2026-08-13)**. Launch it with **`jamovi`** (the `~/.local/bin/jamovi` wrapper — never bare `flatpak run`, see below). The module builds with `jmvtools::install(home = "flatpak")` in ~2 min (~33 s once jamovi's R has the dep tree), and Crosstables is verified running on real data.

⚠ **2.7.36 is PINNED and MASKED, and that is now load-bearing.** Flathub has moved jamovi to a **new version scheme**: the current stable is **28.x** (28.2 as of 2026-08-13) and only **one** 2.7.x commit is still retained — `56eb8de3d468e093ac25cf0bb6236c51e0828fb1b5e8e5bce7b3df110cf49240` = 2.7.36 (2026-06-28); 2.7.32 is the other. `flatpak install` has **no `--commit` option** (only `flatpak update` does), so the recipe is install → downgrade → mask:

```bash
flatpak --user install -y flathub org.jamovi.jamovi
flatpak --user update  -y --commit=56eb8de3d468e093ac25cf0bb6236c51e0828fb1b5e8e5bce7b3df110cf49240 org.jamovi.jamovi
flatpak --user mask org.jamovi.jamovi     # else a routine `flatpak update` silently jumps to 28.x
```

The mask matters because 2.7.36 is the "solid" teaching target **and** because a 28.x jamovi would pair with a newer `jmvtools` whose compiler can emit a `jms` that 2.7.36 refuses (next note). Verify by mechanism, never by the version field — `flatpak info` reports a stale appstream `Version: 2.7.27`; `jamovi --version` reports the truth (`2.7.36.0`), and `--r-version` must equal the module's `rVersion` (`4.5.0-x64`).

✅ **The six "OPEN — maintainer step: regenerate `jmvtab.h.R`" items (Phases 7a, 7e, 7g-i, 7g-ii, 7g-iii, 7h) are CLOSED** — one `jmvtools::prepare()` covered all of them, and the compiled **`uijs` blob** means those UI changes are live in a running app for the first time.

✅ **A second `prepare()` ran on 2026-08-13** (as part of `jmvtools::install()` on the laptop) and closed every `.h.R` item accumulated since — z13's `jmvtabreg.a.yaml` (`na`'s three values), z16's `jmvtab.a.yaml` (`test_robust` → the `design_effect` checkbox) and z16-iiiii's (`method_ratio` removed). **Measured against HEAD**: `design_effect` went **0 → 11** occurrences in `jmvtab.h.R` (the checkbox was declared in the YAML but absent from the stale `.h.R`, so `isTRUE(NULL)` made it **inert** — every claim in its help text was untrue in the running module), and the dead options went to zero (`test_robust` 10→0, `method_ratio` 10→0, `na = "drop_all_models"` 1→0, `ids` 13→0, `strata` 13→0, `fpc` 12→0). `inst/i18n/fr.json` is regenerated from `jamovi/i18n/fr.po` at the same time: translated strings **72 → 159**; the ~21 that disappear are stale msgids for labels renamed across phases (`chi2 = <i>(Chi2 test)</i>`, `after_ci <i>(…)</i>`), and most of the 44 still untranslated are argument **values** (`all`, `auto`, `ci`, `at`) that stay English on purpose.

⚠ **`prepare()` proved the hand-edited `.h.R` had a latent bug**, so do not hand-edit it again. `R/jmvtab.h.R` was hand-mirrored to the YAML across ~7 commits; the compiler reproduced 778 of its 780 lines but corrected `exportExcel` (`type: Action`) from `NULL` → `FALSE` **and gave it a default it lacked** — without which `tabxplor::jmvtab()` called from R throws. The never-edit rule earned its keep.

⚠⚠ **`ELECTRON_RUN_AS_NODE` — do not debug jamovi without knowing this.** Claude Code/Positron export `ELECTRON_RUN_AS_NODE=1`; flatpak passes it into the sandbox and jamovi's Electron runs as **plain node** → **exit 0, no window, no error**, and `jmvtools::install()` dies `"bad option: --install"` (rc=9). `flatpak run --unset-env=` is NOT enough (zypak re-spawns children via the host); only `env -u` on the host works — which is what the `jamovi` wrapper does. In R: `Sys.unsetenv("ELECTRON_RUN_AS_NODE")` before `jmvtools::install()`. ⚠ `jmvtools::check()` passes regardless — it never reaches Electron — so a green `check()` proves nothing here.

⚠⚠ **`R_LIBS_USER` in `~/.Renviron` — the second environment trap, found on the laptop 2026-08-13.** jamovi's flatpak bundles **its own R** (4.5.0 for 2.7.36) and, having `filesystems=home`, it **reads your `~/.Renviron`**. A hard-coded library path there —

```sh
R_LIBS_USER=~/R/x86_64-pc-linux-gnu-library/4.6      # WRONG: pins one R version
```

— puts your **system R 4.6** packages on jamovi's R 4.5.0 `.libPaths()`, and `jmvtools::install(home = "flatpak")` dies at lazy-load with `data_table.so: undefined symbol: R_duplicateAsResizable` (a 4.6 symbol absent from 4.5.0). The assignment is **unconditional**, so **no env var passed to the child can override it** — `withr::with_envvar()` does not help; the file itself must be version-generic:

```sh
R_LIBS_USER=~/R/%p-library/%v      # R's own default: %p = platform, %v = major.minor
```

Same resolved path for system R (verify with `.libPaths()` before/after), while jamovi's R falls back to its bundled `/app/lib/R/library`. Diagnose in one line: `flatpak run --devel --command=sh org.jamovi.jamovi -c '/app/bin/R --vanilla --no-echo -e ".libPaths()"'` — anything outside `/app` is contamination. ⚠ This bites on **any** second R version, not just jamovi's.

⚠ **WSLg is in COPY MODE** (known WSL 2.7.x bug [microsoft/WSL#40618](https://github.com/microsoft/WSL/issues/40618)): windows can be slow or render blank (taskbar entry + penguin icon, `[WARN:COPY MODE]` in the title). **Not a jamovi problem** — plain `xmessage` fails identically. One-time fix, persists across reboots: `sudo mkdir -p /mnt/shared_memory && sudo mount -t tmpfs tmpfs /mnt/shared_memory`. ⚠ The bug is *unstable* — it sometimes renders fine without the mount, then regresses; a working window is not evidence the mount is unneeded.

⚠ **There are now TWO build paths, and they are not interchangeable — `.jmo` bundles are platform-specific** (migration Phase A1):

| Target                               | jamovi                                                  | Checkout                                                                    | Recipe                                                                                                                                                         |
|--------------------------------------|---------------------------------------------------------|-----------------------------------------------------------------------------|----------------------------------------------------------------------------------------------------------------------------------------------------------------|
| **Linux `.jmo`** (WSL, the dev path) | flatpak `org.jamovi.jamovi` **2.7.36 ✅ installed (C3)** | `~/github/tabxplor` — **authoritative for source**                          | `jmvtools::install(home = 'flatpak')` (setup doc §7.4; the SDK `org.freedesktop.Sdk//24.08` is REQUIRED — `flatpak run --devel` is how the compiler reaches R) |
| **Windows `.jmo`** (release only)    | Windows jamovi, **kept forever**                        | `D:\Statistiques\github\tabxplor` — **build-only: pull, build, never edit** | `options(jamovi_home='C:/Program Files/jamovi 2.6.44.0'); devtools::load_all(); jmvtools::install(); devtools::load_all()`                                     |

**A Linux jamovi cannot produce a Windows bundle**, so the Windows checkout survives *even if C3 fully succeeds* — this is not a C3-failure fallback. The rule that matters: **never edit tabxplor in both places.** Edit in WSL, pull on Windows, build there.

✅ **`jmvtools` is pinned to 2.7.26** (C3). ⚠ Never `install.packages("jmvtools", repos="https://repo.jamovi.org")` — that index serves 2.7.26 **and** 28.0-28.3, so R takes **28.3**, whose newer compiler can emit a `jms` version 2.7.36 refuses. Reinstall with the explicit tarball: `install.packages("https://repo.jamovi.org/src/contrib/jmvtools_2.7.26.tar.gz", repos = NULL, type = "source")` (install `node` from that repo first — `repos = NULL` resolves no deps).

⛔ **The 2.6.44 flatpak is GONE** (C3): Flathub retains only ~5 commits; 2.6.44 was built 2025-03-06 and is long pruned. **2.6-solid compatibility is verified on Windows only** — via the build-only Windows checkout, which is kept forever regardless. ⚠ **The retention window is now the constraint on 2.7.x itself**: as of 2026-08-13 the log holds 28.2 / 28.1 / 28 / **2.7.36** / 2.7.32, so 2.7.36 is *two commits from being pruned*. Once it goes, a fresh machine can no longer install it from Flathub — check `flatpak remote-info --log flathub org.jamovi.jamovi` before assuming a reinstall is possible, and keep the installed copy masked.

To know the real structure of the final .html and .js, check at this live capture done from dev console (for a basic table) :
- `dev/jamovi/dev_console_live_capture/Jamovi_tabxplor_1_3_1_basic_table.html` : the live html from tabxplor 1.3.1 jamovi module
- `dev/jamovi/dev_console_live_capture/127.0.0.1_56680_MAIN_ELECTRON/` : the exported main election scripts
- `dev/jamovi/dev_console_live_capture/127.0.0.1_56683_tabxplor_jmvtab_analysis_UI/` : the exported tabxplor jmvtab analysis UI scripts
- `dev/jamovi/dev_console_live_capture/127.0.0.1_56684_results/` : the exported jamovi "results" panel scripts (where the actual table appears)

To **capture new html** in the dev console, **ask the maintainer whenever you need**.

Look at `dev/tabxplor_2.0.0_jamovi_dev.md` and `@dev/jamovi/` for detailed informations.

---

## tabxplor github repo

Branches :
- `dev` is the branch where development happens
- `release/<version number>` is the version stripped of dev only files
- `master` is the public user-facing branch

Commits :
- **The maintainer makes the commits.** Do not commit unless explicitly asked.
- **Never add a `Co-Authored-By` trailer** (nor any "Generated with …" line) to a commit message.
  This overrides the default. The maintainer authors and signs every commit and is solely
  responsible for it; the assistant does not co-sign.
- The release procedure is `dev/release_checklist.md` — read it before touching a release branch.

---







## tabxplor version 2.0.0 roadmap : the current goal

Currently implementing tabxplor 2.0.0 (2.0.0 only if breaking changes land). **Update the sections below at the end of every work session.**

This roadmap is the **plan of plans**: the phased implementation order plus every open question. A fresh session asked for a *part* of the work should read, in order:

1. **This roadmap** — the phase your task belongs to, its bullets, and its pointers
2. **`dev/tabxplor_2.0.0_roadmap_DONE_PHASES.md`** – the detailed report of all the **already implemented phases of the roadmap**.  
3. **Top of this CLAUDE.md** — Repository Map, Architecture.

The performance harness lives in `dev/benchmarks/` (`.Rbuildignore`'d). Per the scope decision, save every phase's before/after runs under `dev/benchmarks/results_2.0.0/`.

Other long-form 2.0.0 docs live in `dev/` (all `.Rbuildignore`'d), never inline here — read the matching ones before you start.

#### Verification (every phase)

- **Byte-identity**: `devtools::test("~/github/tabxplor")` after each phase; `test-golden.R` + `test-export-parity.R` + `test-fmt-contract.R` + `test-fuse-parity.R` stay green. Intentional output changes → rerun `dev/make_golden.R`, review the `_golden/`/`_snaps/` diff consciously, `testthat::snapshot_accept()`.
- **Release gate**: `devtools::check()` (~3 min, run manually by maintainer) before CRAN.

---

### Phase 21 — documentation integration and simplification 1

The package documentation had grown cluttered with dev history and lost focus. This phase enforces the **documentation ecosystem** hierarchy (top of this file) across every layer: present-tense, history-free, general to specific, each fact stated once, referencing more global or more precise docs rather than duplicating them. Before writing any document, state what it is for, its focus within the ecosystem, and what belongs elsewhere. 21a integrated the architecture + Repository Map at the top of this file; 21b rewrites the R comments + roxygen; Phase 23 does the vignettes, `NEWS.md` and the `dev/` folder.

#### Phase 21a — `dev/tabxplor_architecture.md` and `CLAUDE.md` Repository Map drastic rewrite

`dev/tabxplor_architecture.md` is currently a complete mess of around 2400 lines : I want you to write a new .md file of **maximum** 400 lines from scratch, then remove the old one and rename the new one with the old name.
- The document should start with the real design goals, real-world usage and specifities of tabxplor, and present all the rest of the design and architecture from there.
- The big picture should come first, the subsystems next. The description of a subsystem should give only key technical informations, refering to a more detailed technical description of the framework in the R comments at the top of a given subsystem: it should focus on goals, meaningful "why" of the framework, design decisions, and links with the overall goals of tabxplor.
- It should be easily readable by both human and machine.
- markdown tables should never be more than 120 characters wide (otherwise the raw file in unreadable by humans).

It’s the same for `CLAUDE.md` "## Repository Map" : it’s currently around 1800 lines, and I want you to  **erase** it and rewrite a maximally focused version with **maximum** 100 lines, refering to the architecture document and the comments inside the .R files for details.

**DONE**. Rewrote `dev/tabxplor_architecture.md` from scratch (2389 → 216 lines) and the CLAUDE.md Repository Map (~1834 → 74 lines): both present-tense and history-free (no phase tags, no "was/now", no bug post-mortems, no `dev/*.md` decision-log pointers), ordered general → specific. The arch doc now holds only the CROSS-SUBSYSTEM big picture — goals · data-flow · the declarative fact-table pattern (add a row, not a switch) · the `fmt` type system · pipeline · inference · colour · a fuller regression section · exports · jamovi · cross-cutting invariants — and delegates per-file detail DOWN to the R file headers (Phase 21b rewrites those) and the file index to the Repository Map. The `fmt` field/attribute reference tables are kept but stripped of history; every fact-table / file / accessor name was verified against source, table rows ≤ 120 chars. This sets the documentation-ecosystem hierarchy for the rest of Phase 21: arch doc (cross-subsystem) → Repository Map (file index) → R headers (per-file) → inline `# DESIGN:` tags.


#### Phase 21b — R scripts comments drastic simplification/rewrite

I want you to **drastically** rewrite and simplify R scripts comments (including key decisions, etc.) **based on the final design, architecture and real-world usage**, for future development to never lose focus on them. Your should **divide their global length by at least 5**: measure it, find a maximum ratio of comments/code, make web searches if needed.
- In each phase, start by reading the `## tabxplor architecture` section (top of this file) to get the big picture around which everything should revolve.
- The *goal* is **not** to make a summary of current comments: your selection should be drastic (most of it is useless and should be removed anyway), what needs to be kept is a small subset of it,
- Rewritten comments should focus on : what explains design decisions, architecture choices, caveats ; everything needed to explain all the subsystems of the package without losing focus on the overall goals and architecture ; the "why" of the code, the way it integrates in the global functions ecosystem of the package, the way to use this ecosystem to avoid re-adding evergrowing exceptions and white elephants in the future, the way to modify it in the future, etc.
- Each subsystem must first come with a longer description of it’s currently implemented design choices and internal architecture, preferably at the beginning of the .R file: it should be more detailed than the subsystem description in the architecture document, but not too detailed as to take the place of proper before-functions or in-code comments. Do not hesisate to reorganise the .R files to make the subsystems clearer.
- Comments must be clear, focused, understandable by both machine and **human**: avoid being over-technical as much as you can for this kind of documents.
- The roxygen documentation part should also be reduced with the same logic (maybe not divided by 5, but divided as much as possible while staying clear and keeping enough explanation for beginners and literary students) : user-friendly = focused, hierarchy of documentation with references.


**Method for every 21b-ii … -vii sub-phase** (do not repeat it per phase): re-read the `## tabxplor architecture` section (this file) for the big picture; then, per file — (1) rewrite the header into a clean current-state subsystem description (keep the `# PURPOSE / ROLE / KEY CONSTRAINTS` skeleton, history-free; a longer subsystem essay is fine where warranted); (2) cut inline comments **≥5×** — delete ALL dev-history (phase tags, "was/now", post-mortems, "measured …", `dev/*.md` pointers) AND compress the surviving design/"why" prose to one-liners; (3) tighten the **roxygen** — a user-facing function's man page is about **USAGE, written with the main real-world use cases in mind; it never speaks of the build, the internals, or dev history**. Dedupe across pages and vs the code, defer detail/pedagogy DOWN to the vignettes and reference SIDEWAYS to `?tabxplor-vctrs` / `?tabxplor-options` / `?tabxplor-data.table`, but keep it clear for beginners/non-technical users (not a 5× target; preserve the CRAN arg/return contract; reduce generated `@eval` blocks via their fact-table `doc` fields, never the output); (4) run `devtools::document()` **unsandboxed** and confirm `man/` + `NAMESPACE` build. ⚠ Do NOT hard-wire vignette anchors — the roxygen→vignette pointers are finalized in Phase 23b, after 23a reorganizes the vignettes. Every `R/*.R` is covered exactly once; the two generated `jmvtab*.h.R` are untouched. **Before delegating anything, read rule 11 (execution economics) below** -- it sets the agent count, model, effort and turn discipline, and exists because 21b-iii cost ~2.0M subagent tokens.

**How to hit the /5 cut on the FIRST pass (Phase 21b-ii lessons — read before starting any 21b sub-phase).** The failure mode is *summarising* (shortening each existing comment, which under-cuts and keeps the old focus); the target is *rewriting* (deleting most, keeping a small load-bearing subset written from the final design):

1. **Rewrite, don't summarise.** For each comment ask: "if this file had none, would I add THIS one back, knowing only the current design + the header essay + the `## tabxplor architecture` section?" If no → **delete it**, don't shorten it.
2. **The WHAT/WHY test decides every comment.** Delete any comment that says WHAT the code does (a competent R reader infers it from the name + body). Keep only (a) WHY — a non-obvious choice, caveat, gotcha, or WARNING that yields a wrong result/crash if ignored; (b) SCHEMA — a fact-table column dictionary (what each declared column/field means).
3. **Write the header essay first.** It carries the file's design load, so most inline "why" then becomes redundant and deletable.
4. **Mechanical regions → near-zero comments.** Accessor stubs, the `vec_ptype2`/`vec_cast`/`vec_arith`/`vec_math` and dplyr coercion walls: ONE orienting line for the whole wall; delete the per-item/per-method comments.
5. **Delete ALL dev-history unconditionally:** phase tags / `KEY N` / `§N` / `D-`/`W-` items; "was / used to / no longer / replaced / renamed / deleted"; "measured …" + benchmark numbers; bug post-mortems; `dev/*.md` decision-log pointers; commented-out dead code and inert commented-out roxygen stubs.
6. **Measure the target on the BODY, not the whole file.** The `/5` is a GLOBAL aggregate over bodies. `grep -c "^\s*#[^']"` also counts the 15-28 line header essay this method mandates, so on a small file the header alone can be half the surviving lines and the whole-file ratio is unreachable BY CONSTRUCTION. State the target to an agent as *body* comments (whole-file count minus the header block) or it will burn its budget on impossible re-cuts. Measured 21b-iii: bodies cut 2.8-4.5x while the whole-file ratios read 1.6-5.5x. A dictionary-heavy file (fact tables = schema) and a trap-heavy file (the parallel seam, where nearly every WARNING records a real failure) floor genuinely higher -- clarity overrides the ratio; never gut a dictionary to reach a number.
7. **Roxygen is contract-bound — a separate rule, not a /5 target.** NEVER drop/rename/reorder `@param`/`@return`/`@export`/`@rdname`/`@describeIn`/`@eval`/`@method`/`\usage`; every argument stays documented. Reduce by de-archaeologising prose, compressing verbose params, MODERNISING wording (repoint superseded `tab_pct`/`tab_ci`/`tab_chi2` cross-refs to `tab()`/the accessor; foreground the current vocabulary), and deferring pedagogy to the vignettes / sideways to `?tabxplor-vctrs`.
8. **Respect the hierarchy.** The header NAMES and POINTS (down to the arch doc / vignettes, sideways to reference pages); it never re-tabulates the field/attribute tables that already live in the arch doc + roxygen.
9. **Delegation that works.** Hand-do the highest-judgment blocks yourself: a user-decision rewrite, the header essays, the fact-table dictionaries, the colour engine, and the primary man page. Delegate the mechanical bulk. Give every agent the WHAT/WHY test, an explicit KEEP list stated **by CONTENT, not by line number** (the file shifts under the agent's own edits), an explicit DELETE list, and a hard target number. Agents anchor conservative -- state the aggressive standard up front. Write the header essays FIRST and hand them to the agent: the header carries the file's design load, which is exactly what makes most inline "why" deletable.
10. **Verify cheaply -- no full test suite needed for comment-only edits.** Prove code identity by PARSING, since the parser drops comments: compare HEAD vs working **keyed on the name each top-level assignment binds** (`deparse()` each value, compare the name sets and every body). That form stays valid even if functions were reordered, which the whole-file `deparse` comparison does not -- and it names the offending function when it fails. TRUE means behaviour cannot have changed. Then `devtools::document()` **unsandboxed**; confirm NAMESPACE is unchanged and that every changed `man/*.Rd` keeps identical `\usage` and `\item{arg}` NAME SETS (prose inside an `\item{}` may change freely). ⚠ A `\usage` diff is not automatically yours: HEAD's `man/` can be stale against HEAD's `R/` (21b-iii found `tab_reg.R` declaring `outcome_level, ref` while the committed Rd documented `ref, outcome_level`) -- check the committed source order before assuming you caused it. Run the suite only when explicitly asked, or when code actually moved.

11. **Execution economics -- the delegation budget.** Measured on 21b-iii: ~2.0M subagent tokens (212K survey + 1.79M rewrite). Agent cost fits `= 75K fixed + 90 x file_lines`, so **a small file delegated alone is the worst value in the method** (318 tokens per file line on a 320-line file, against 107 on a 3,700-line one). At that method the four remaining sub-phases would cost ~4.2M; the four rules below bring it to ~1.3M. In leverage order:
    + **Batch by size, ~3 agents per sub-phase.** Group the small and medium files into one agent each (one brief, one context, files edited one at a time -- different files, so no write conflict); give any file over ~1,500 lines its own agent. On an 11-file sub-phase this alone saves ~600K of pure fixed overhead.
    + **One pass, `effort: "low"`, `model: "sonnet"`.** The brief carries the standard and the target; the agent does read -> rewrite -> verify ONCE and stops. **Forbid self-directed re-cutting**: the two priciest agents of 21b-iii spent ~750K between them on "two hard re-cuts" chasing a target that rule 6 shows was unreachable. Review the output yourself and re-engage only the files that genuinely missed, with a specific instruction. Sonnet is right because the judgment is front-loaded into the KEEP/DELETE list -- the agent applies a spec, it does not decide one.
    + **ONE survey agent per sub-phase** (not one per file or per group), at `effort: "low"` with a hard output cap: a structural map plus a KEEP list, no code excerpts. You still write the briefs from its map. The survey earns its keep -- in 21b-iii it is what surfaced the five documentation defects.
    + **Defects are REPORTED, never silently fixed.** A contradiction between two files, a dangling pointer, stranded roxygen, a comment describing the wrong function: the agent reports, the maintainer-facing decision stays yours. 21b-iii found five this way, including a comment asserting the chi2 was unweighted when the code passes it weighted counts.

##### Phase 21b-i — comments rewrite roadmap

Since the codebase is big, I first want you to create the roadmap of the work to do here and cut differents phases at the relevant seams.
- The different phases "##### Phase 21b-ii – ...", "##### Phase 21b-iii – ..." etc. should be cut at natural seams : everything that needs the same context must be done in the same Phase, and .R files that are the more related to each-other should be done in the same session ; when a subsystem or a selection or subsystems or some .R files are better done in a fresh Claude Code session, with their’s own context, and is long enough, it gets it’s own phase ; but the number of phases should stay low enough (since multipliyng the number of phases would add exploring/planning/verifying context cost several time anyway and be slower for the same reason). The maintainer will manually commit between phases, and only push at the very end.
- **The description of each phase should be focused and very short**: each Phase will itself starts in plan mode, so the content of the Phase itself should not be too detailed and too prescriptive, **the roadmap should not replace the proper plan of each Phase** (which is better done with the full right focused context).

Also plan for a reduction of the roxygen part, with the same logic : user-friendly = focused, hierarchy of documentation with references.
- roxygen doc maybe not divided by 5, but divided as much as possible while staying clear and keeping enough explanation for beginners and literary students.
- state clearly that main user-facing functions speaks about usage, with the main uses cases in mind ; they do not speak about build, internals, dev history, etc., and must be understandable by a litetary sociology researcher knowing almost nothing about programming and about R.

**DONE**. Cut the R-file documentation rewrite into **six** subsystem sub-phases (below), grouped by coherent context and balanced by measured load (internal comments ~15,420 lines = 32 % of the code, of which ~55–70 % is dev-history; roxygen ~5,040 lines). Roxygen is **folded into each sub-phase** (one documentation pass per file). The shared method is stated once here so the sub-phase entries stay short.


##### Phase 21b-ii — Core type system & colour engine

`fmt_class.R` · `tab_classes.R` · `row-model.R` · `table-spec.R` · `tab-shape.R`. The `fmt` record + arithmetic + colour engine + hosted fact tables; the table classes / dplyr / print / palettes; row & table identity; `?tabxplor-vctrs`. Heaviest phase; the `/vctrs-field` + `/color-mode` skills gate this area. (`fmt_class.R` centralizes package-wide NSE `globalVariables`; `row-model.R` also defines the pipeline's row axis.)

**DONE.** Rewrote comments + roxygen across the five files, present-tense and history-free: **0** phase tags / `KEY N` / `§N` / `dev/*.md` pointers / post-mortems / benchmark numbers remain, and dead commented-out code + inert commented-out roxygen stubs are gone. Headers → the `# PURPOSE / ROLE / KEY CONSTRAINTS` skeleton + a short design essay, each pointing to `dev/tabxplor_architecture.md` + the gating skill. Per the two decisions taken this session: header style = skeleton + short essay (uniform); roxygen wording MODERNISED — `fmt()`'s `@param color` rewritten to the current `diff`/`ratio`/`or`/`contrib` + `color_signif` vocabulary (1.x `diff_ci`/`after_ci` kept, noted superseded), and superseded `tab_pct`/`tab_ci`/`tab_chi2` cross-refs repointed to `tab()`. Kept the load-bearing subset: the fact-table column dictionaries (`EST_SCALES` / `MEASURES` + every `doc=` / `fmt_attr_rules` / `COLOR_SCALES`), the colour-engine design, the vctrs/dplyr/legend invariants, and the genuine `WARNING`/`DESIGN` caveats; the ~30-method dplyr wall collapsed to one `DESIGN` block. Plain-`#` comments: `fmt_class` 2190→1147, `tab_classes` 999→377, `row-model` 134→90, `table-spec` 54→39, `tab-shape` 62→48 — **aggregate 3439→1701 (2.0×)**. The ×5 is met by the archaeology-heavy + mechanical files; `fmt_class.R` floors higher (foundation file — type contract + 3 dictionaries + colour engine + vctrs/legend invariants; its residue is genuine design, and a further cut there of the ~200–300 remaining behavior-narration lines is the one open lever if a lower number is wanted over documentation density). **Verified (no test suite, per request):** all five files provably code-identical to HEAD (parse→`deparse` diff = TRUE, so behaviour cannot change), `devtools::document()` clean, NAMESPACE unchanged, only 4 man pages changed with prose-only edits (no `\usage` / arg-name changes). The reusable "hit /5 first-try" ruleset distilled from this session is recorded in the Phase 21b-i method block above.

##### Phase 21b-iii — Crosstab build pipeline & aggregate core

`tab.R` · `tab-leaf.R` · `tab-agg.R` · `tab-chi2.R` · `tab-display.R` · `tab-resolve.R` · `tab-counts.R` · `tab-parallel.R` · `tab-deprecate.R` · `tab-steps-legacy.R`. How `tab()` builds a table end to end, plus the quarantined legacy step API; `?tab`.


**DONE.** Rewrote comments + roxygen across the ten files, present-tense and history-free. Plain `#` comments **3930 -> 1173 (3.35x)**, comment/code ratio **0.68 -> 0.20**, code unchanged at 5744 lines; roxygen 989 -> 937. Per file (plain `#`): `tab` 1248->225 (5.5x) - `tab-leaf` 866->243 (3.6x) - `tab-agg` 366->141 - `tab-chi2` 181->57 - `tab-display` 270->139 - `tab-resolve` 340->95 - `tab-counts` 119->50 - `tab-parallel` 133->81 - `tab-deprecate` 91->43 - `tab-steps-legacy` 219->82.

**Why the aggregate is 3.35x, not 5x, and where the /5 IS met.** The two are the same story from opposite ends. `tab.R` (5.5x) and `tab-leaf.R` (3.6x) - the archaeology-heavy files the ratio was designed for - meet or beat it. The small files cannot, because this phase also gave every file a 15-28 line header essay that the same grep counts: on `tab-counts.R` 22 of the surviving 50 lines are the header plus its three mandated banners, so its BODY went 92->28. Measured on bodies only (excluding the new headers) the files run 2.8-4.5x. `tab-parallel.R` (1.6x) is a deliberate floor - almost every `WARNING` there records a real parallel-execution failure (the stale-namespace trap under `load_all()`, the `do.call(design =)` boundary drag, the never-nest rule), and the brief was to budget the least deletion there. `tab-display.R` (1.9x) and `tab-agg.R` (2.6x) host the `DISPLAY_TOKENS` / `CI_METHODS` / `CI_GEOMS` dictionaries, kept near-verbatim as schema. Both were hand-reviewed: no slack left that is not a named KEEP.

**Five documentation defects fixed, not just reworded.** (1) `tab-agg.R` asserted *"Chi2 is FULLY UNWEIGHTED (counts and n)"* - **false**: `chi2_compute_test()` feeds `agg_chi2()` the WEIGHTED counts rescaled to the raw n. The engines are agnostic and the caller decides; `tab-chi2.R` now owns the weighting rule. (2) `tab-leaf.R` pointed at `dev/tabxplor_architecture.md`, deleted in `d65eede`; all 28 `dev/*.md` + roadmap pointers replaced by the one `# See: CLAUDE.md § tabxplor architecture (...)` convention. (3) `CI_GEOMS`' column dictionary omitted `ref_cell` though the exhaustiveness `stopifnot` checks it - added. (4) `tab-leaf.R` had **three** stranded function headers (`leaf_inference()`'s prose and roxygen sat above `num_total_postprocess()`, which carried two blocks; same for `leaf_wide_pct` and `plain_resolve`) - each reattached. (5) `tab.R`'s render-time-detector comment described the wrong function.

**Two roxygen decisions.** `?tab`'s ~48 lines of statistical `@details` (Kish, `svyrecvar`, Rao-Scott delta-bar, finite-sample factors) compressed to one usage paragraph per topic - what the default does, what `design_effect = TRUE` or a `svydesign` changes, the one caveat that matters - deferring the derivation to the vignettes, which already cover it. And `@section Significance stars:` MOVED to `?tab`, with `tab_ci()` now carrying `@inheritSection tab Significance stars`: the primary man page no longer inherits its core statistical explanation from a superseded, quarantined function. Verified rendering identically on both pages. Superseded cross-refs modernised throughout (`OR` -> `display`, `method_cell`/`method_diff` -> `ci_method`).

**`tab.R` was NOT regrouped, deliberately.** The approved plan allowed moving functions; mapping all 57 definitions showed the file already ordered by pipeline stage (`tab()` -> colour spec -> ctx -> engine -> STAGE 1..5 -> spread/transpose -> variable readers -> labelled interop -> fmt carrier -> shared helpers -> `globalVariables()` last). The three regions flagged as problems were each already contiguous; they lacked BANNERS, not order. Thirteen `# === SECTION:` / `# === STAGE n/5:` banners now cover the file. Moving functions would have added diff noise and forfeited the cheap verification for no structural gain. The load-order constraints remain as documented: `CTX_SETTINGS_LOCALS` / `ctx_settings_locals()` / `SPINE_OWNED_INPUTS` before their `stopifnot`, and `utils::globalVariables()` last because `new_ctx()` calls `conf_level_default()`.

**Verified.** All **198 top-level definitions across the ten files are byte-identical to HEAD** (parse -> per-name `deparse` comparison, which stays valid under reordering), so behaviour cannot have changed. Full suite: **FAIL 0 | WARN 1 | SKIP 4 | PASS 7284**. `devtools::document()` clean, **NAMESPACE unchanged**, all ten changed `man/*.Rd` keep identical `\item{}` argument-name sets. ⚠ `man/tab.Rd` and `man/tab_reg.Rd` show `\usage` REORDERINGS that are **pre-existing drift, not from this phase**: at HEAD `R/tab_reg.R` declares `outcome_level, ref` while the committed `man/tab_reg.Rd` documented `ref, outcome_level` - formals were reordered without re-running `document()`. Regenerating corrects it; `R/tab_reg.R` is untouched here.

**Cost, and what it changed in the method.** This phase spent **~2.0M subagent tokens** (212K survey + 1.79M rewrite) across 13 agents, all on Opus at inherited effort -- about 6 % of a weekly limit for one sub-phase. Per-agent cost fits `= 75K fixed + 90 x file_lines`, so delegating a small file alone was the worst value in the run (318 tok/line on `tab-deprecate.R` vs 107 on `tab.R`), and the two largest agents spent ~750K between them on self-directed re-cuts chasing a whole-file ratio that the mandated header makes unreachable. The four remaining sub-phases would have cost ~4.2M unchanged. **Rules 6, 9, 10 and 11 of the method block above were rewritten from these measurements** (target measured on the body, batch ~3 agents per sub-phase, one pass at low effort on Sonnet, one cheap survey per sub-phase, defects reported not silently fixed), projecting ~1.3M for the remaining four.

##### Phase 21b-iv — Regression

`tab_reg.R` · `reg-resolve.R` · `reg-estimand.R` · `reg-empirical.R` · `reg-influence.R` · `reg-assumptions.R` · `reg-spec-build.R`. `tab_reg()` + the estimand / empirical-companion / influence / model-check machinery; `?tab_reg`, `reg_measures`. (`reg-estimand`'s `measure` vocabulary IS `fmt_class`'s `EST_SCALES` — note the bridge.)

##### Phase 21b-v — Shared foundations: inference, arguments, options, integrity

`survey-design.R` · `survey-variance.R` · `tab-args.R` · `tab-options.R` · `zzz-fact-keys.R` · `utils.R`. Cross-producer infrastructure: design-based inference · the declarative argument / option / foreign-key surface (`?tabxplor-options` generator) · utilities. Light; keep the survey-math and declarative-surface sub-groups distinct (`survey-variance` feeds the crosstab leaves but stays with its sibling; `zzz-fact-keys` is genuinely cross-cutting).

##### Phase 21b-vi — Exporters & rendering

`tab-export.R` · `tab-export-prep.R` · `tab-render-html.R` · `tab_md.R` · `tab_xl.R` · `tab-xl-backend.R` · `tab-css.R` · `tab-test-display.R` · `tab-transpose-render.R` · `tab-theme-detect.R` · `plots.R`. One shared prep model → the four backends + the footer / CSS / transpose / theme / plot support.

##### Phase 21b-vii — Jamovi modules

`jmvtab-cache.R` · `jmvtabreg-cache.R` · `jmvtab-export.R` · `jmvtab.b.R` · `jmvtabreg.b.R`. The two point-and-click modules + their live-UI caches (generated `*.h.R` untouched).


---


### Phase 22 — manual reviews and last features before release

Below are the results of the maintainer’s manual reviews of different features, stating the problems and what still needs to be changed before 2.0.0 release.
- Avoid *ad hoc* solutions, think about how to integrate the requested changes in the package ecosystem cleanly in a future-proof way. When framework changes are needed, state it clearly and plan for them. If you think they are too big and better done in their own Claude Code session, state it clearly and write a new phase in the @CLAUDE.md roadmap ("Phase 22x-ii", "Phase 22x-iii", etc.), but avoid to create too many different phases and regroup what is better done together (same context needed, or not big enough to get it’s own session).

#### Phase 22a — `tab_reg()` crude/adjusted comparison, family × effect × measure and display integration ?

I have some hints about how we could improve the consistency of the whole regression framework, which is a potential further statistical and display integration step. I want you to study this thoroughly, assess possible improvements, and flag misunderstandings and ideas that won’t work. This is a research and design task: write a new detailed and structured .md file in `dev/`.

1. regression model columns simplification ?

```r
gss_simple <- gss_cat_data_formatting()
tab_reg(gss_simple, outcome = "married", predictors = c("race", "rincome", "relig", "age"),
        family = "binomial", measure = "ratio", empirical = TRUE
)
```

- I think I’ve found a design flaw, please study this and correct me if I’m wrong. Having a different color measure for Obs_% (difference) and Obs_RR (ratio) is misleading (it’s also true with Obs_% and Obs_OR with `measure="odds_ratio"`), , but here they are colored with different measures. The Obs_% could color="ratio" to match the Obs_RR and the model one, but there’s a better solution. The right way to do it is to **only use one color measure for both empirical and modelised columns**: that way, there would be **only one legend block for everything**, which means everything is comparable and the field of comparison is homogeneous. Anything more is confusing.
  + The way to be even more clear, symmetric, and reduce the discrepancy between Obs_% and Obs_RR, is to **only have one empirical column and one model column** : same measure, same display and the "display" argument works on both, one legend block/only one consistent CI type, etc. (So if he wants, the user can still since duplicate the same column with a different display.) It would be further integration and simplification for readability.
  + after it’s done, we should review `tab_reg()` html tooltips, both in the empirical column and the model column. Which ones would be useful and are missing ? Which ones are useless (info already printed elsewhere on the table = noise, with some exceptions when it helps the user make the meaningful links in his mind) ?
  + To merge the legend of both the empirical column and the model column would maybe mean to change the legend builder a bit, please study that too.

2. family × effect × measure combinations
- "AME" name is not very clear. Or maybe, with `effect = "marginal"`, am I not sure how to *name* `measure = "difference"` versus `measure = "ratio"`. On sign that the confusion is on the current vocabulary, is that RR is used for both "ratio" coefficient and "ratio" AME, while the "difference" path differenciate betwenn "RD" and "AME". Average marginal effects *of the* difference, or something else ? versus average marginal effect *of the* ratio, average marginal effect ratio ? What are the standard names ? If the standard names are a bit misleading, what would be the most readable and precise names ?
- Adjusted percentages are currently treated as if they were outside of the whole `measure` thing: but statistically aren’t they the "base" "identity" `measure` (only working for "marginal" and "at_reference", not the model coefficients) ? Should we add them a proper `measure`, that would work for both proportions and means, or is it useless because it gives no confidence intervals and colors (useful to color the adjustement between observed proportion and adjusted proportion, or useless since this comparison would be done relative to each predictor’s reference level, so with the "difference" or "ratio" colors ?) ? If it’s a good idea, which I doubt a bit, would be the more readable and the best to teach the framework ( `measure = "value"`, `measure = "base"`, `measure = "identity"`, something else ? ; `measure = "pct"` and `measure = "mean"` would be too specific to the chosen family, so I’m looking for a readable generalisation) ? Is adjusted proportions or means only a display thing (that we them color with "difference" or "ratio" ?) ? Is it only available on the "marginal" or "at reference" paths, because "coefficient" doesn’t compute them at all ?
- The family × effect × measure combinations messages are not very clear. 'ℹ A "binomial" outcome offers:\nℹ `effect = "marginal", measure = "difference"` -> "AME"\nℹ `effect = "marginal", measure = "ratio"` -> "RR"' The choice should be clearer : '-> "AME"' should also give the full expression, average marginal effect of the difference ; '-> "RR"' should make clear that this is an AME ratio

3. `display` tokens and `display` presets
We now use the same display argument in `tab_reg()` than in `tab()`, taken both `{}` display tokens, and build-in presets, so **we shall think about the best built-in presets for the regressions use cases**.
- First, I want to be sure I understand well, correct me if I’m wrong. Let’s take binary binomial for example. With `effect = "coefficient"` and `display="ame"`, the SE and significance stars and colors are based on the OR, and the AME is just displayed to help interpretation ; but with `effect = "marginal"` (with a default `measure` to `"difference"`) the SE and significance stars and colors are recalculated for the AME itself in the `marginaleffects::` way (which is more robust if there AME are used to interpret the model).
- When effect is "coefficient" and "ame" is chosen in the `display` (preset or display token), it should be pre-calculated and it’s vctrd field populated not to print voids/NA, but since it does not need it’s own SE/CI/stars it’s very cheap to compute, right ? So the same way tab always populate "diff" and "ratio", we could just cheaply always populate the "diff" AME, the "ratio" AME and the adjusted p"ct or adjusted "mean", and they would always be available for "display" argument, manual change in "display" without new calculations needed, etc. ? And we could always print them in the tooltips (both in the crude side and the model side in mirror) when they are meaningful for the family ? That way, `display = "ame"` won’t be adequate since the choice isn’t marginal or not, but {value}. Study that and correct me where I’m wrong.
- `display = "value"` is the default, but is `display = "{value}"` working ? This display token would be important to do customisation but have a way to auto select the main modelised column, which can be diff, ratio, OR, or coeff (stored where ?), depending on the family and effect (and dependinng on the measure if the effect is "coefficient" ?) ?
- The crude proportion (or mean) versus model proportion (or mean) comparison is a relevant display, that needs to be thought about. The current `display= "prob"` works for proportions but not for means, which should be corrected. The related `tab` display token is "{num}", which is not very clear, and "value" won’t be much clearer and is already the default for the plain estimate of the model (both are a bit confusing anyway). There should be both a built-in preset (to only get the crude versus models proportions or mean displayed, with the colors/CI/stars of the `effect` and `measure`) and a {} display token (to be able to easily compose it with other fields ; instead of "{or} ({pct})", something like "{value} ({num})" ?). Can you think about other more readable names for this (make me propositions) ?

**STUDY DONE (design only, nothing implemented) → `dev/reg_crude_adjusted_and_display_integration.md`.** Every claim there is a capture from the running package, not a reconstruction. The finding that unifies the three questions: the observed column and the model column are the SAME estimand computed twice (one predictor vs all of them), so they must be one column *shape* built twice — same scale, same colour measure, same display template, same legend block — with the crude level (`%` / mean) folded into the crude cell exactly as the adjusted level already rides in the model cell. `display` then applies to both, every preset works on every family, and the crude/adjusted comparison is read ACROSS the table instead of needing a display of its own.

Decided in that session: **D1** two columns by default + an opt-in in-cell fold (which generalises today's multinomial `visible = FALSE` rule and DELETES it); **D2** the header names the measure with the contrast as a marker ON the measure — unmarked = conditional, `m` prefix = marginal (`mRR`, `mRD`), `@ref` suffix = at the reference profile; **D3** `measure = "log"` names what it logs (`log(OR)` / `log(IRR)` / `log(RoM)` / `log(cumOR)`), ending the five-way `Model_β` collision; **D4** ordinal is `cumOR` on the model side too; **D5** the display tokens are `{est}` (the column's own estimate, via the existing `fmt_center_field()`) and `{base}` (the adjusted prediction / observed level, by `var_kind`) — `{base}`, NOT `{num}`, because `get_num()` is published on `master` with a different meaning ("the currently displayed field") and because `base` is already the code's word for this quantity (`REG_EMPIRICAL$*$base`, and `fmt_class.R:830` already spells the composite `"{base} {ci}"`); it must work identically in `tab()` and `tab_reg()`; **D7** two free renames follow, neither name published: `display = "num_ci"` → **`"base_ci"`** (28 sites in 9 files incl. the generated `jmvtab.h.R`, so one `jmvtools::prepare()`; verified the ONLY `num`-flavoured preset — the other literal `"num"` strings are `MEASURES$applies_to` values meaning "a numeric column", left alone), and `pct_base` → **`pct_type`** ("which type of percentage": row / col / all / all_tabs / none — the qualifier keeps it clear of 1.x's bare `type`, which conflated the kind of number with the percentage base and which 2.0.0 split into `scale` + this attribute; 205 sites + `get_pct_base()` / `set_pct_base()` / `PCT_BASES`; `pct_base` is absent from `master`). ⚠ One follow-through while renaming: the live `type` deprecation abort (`R/fmt_class.R:457`) reads "`type` is now `scale` + `pct_base`" and must be reworded to name the SPLIT rather than the tokens, or it becomes "`type` is now `scale` + `pct_type`" and reads as a no-op; same for its `NEWS.md` bullet; **D6** the vignette grid prints acronym + meaning without the `Model_` prefix, with a first column for the outcome kind. `measure` gains NOTHING for adjusted percentages — they are `display = "{num}"`, a level printed and graded by the effect, exactly as `tab()` already does; a `measure = "identity"` row could not colour, could not be tested and could not take the marginal marker. Six questions stay open (gaussian `diff`/`mdiff` vs 22b's `Model_coeff`; preset names; the prose legend's joined method list; automatic vs explicit fold; a `{gap}` token for print/Excel; the stale `needs = "marginaleffects"` abort) — §8 of the doc.

The measured defects that phase must fix, with their causes: the two-ladder problem is exactly `binomial × {odds_ratio, ratio}` and `poisson × ratio` (a *coloured* base column on a different ladder from the effect column — `gaussian` / `grouped_binomial` / `binomial × difference` are already fine, so deleting the base column as a COLUMN fixes every family with no per-family branch); `display` is silently reset to `"value"` on the whole marginal path and the folds are binomial-only; `{value}` is not a token; ratio columns get NO interval and NO p-value in their tooltip (the one thing a hovering reader wants from an OR); the gap tooltip prints `-0.01` where the cell prints `-23.0%`; and the multiplicative inverse has three renderings (`1/x` for `or` only and only outside a composite, `÷x` for `ratio`, raw for `est_ci` and every composite) — which is why 22b's inverse-rendering rule should land BEFORE this phase's column merge. The legend needs almost nothing: the terse legend ALREADY merges `Obs_OR` with `Model_OR`, and deleting the base column is what produces the single block. ⚠ One claim in the study was corrected on the maintainer's challenge and re-measured: the crude closed forms ARE the univariable model's interval — Woolf vs `glm` Wald-on-log agree to 3e-13, Katz vs modified-Poisson HC0 to 8e-09, `student` vs `lm` to 6e-15 on a binary predictor — so the `katz` / `wald_log` split in the `prose` legend is a LABELLING defect (fix: label from the estimand, as `CI_METHOD_LABELS` already does for `woolf`), not honest divergence, and no grouping-key change is needed. The one genuine exception is `gaussian` / `poisson`, where the crude pools variance PAIRWISE while `lm` / quasi-Poisson pool globally — measured 8.9 % apart on a heteroscedastic 3-level predictor, which also makes the regression vignette's "to match those computed by linear regression" false for k > 2 levels. New open question Q7: switch those two crude effect columns to `from = "fit"` (parity by construction, one extra fit) or keep pairwise and correct the vignette.



#### Phase 22b — `tab_reg()` manual review

Below are the maintainer’s manual reviews.

```R
gss_simple <- gss_cat_data_formatting()
tab_reg(gss_simple, outcome = "married", predictors = c("race", "rincome", "relig", "age"),
        family = "binomial", empirical = TRUE
)
```

- is’t actually good that "age", being one line, have it’s variable name written horizontally, but it should be in bold to match the vertical ones.
- age numeric variable "levels" column is written in html "age(per 1 SD\n (13.5)) <curve>" : it shouldn’t wrap after SD if there are other "levels" with longer names, like in "Model fit" (here the would be space for it to fit on 1 line) ; I want a bit more concise display : "per SD/13.5" (age do not need to be repeated here, it’s already in the variable name column ; only print the number of SD when it’s not 1)
- Would it be possible to put the numeric predictors linearity check sparklines in the `n` column, that is always empty for numeric variables by design ? On html, put the sparkline in a small frame (with a smaller linewidth than the sparkline itself) for readability (otherwise, to the uninformed user, it looks more like stray pixels noise than plot).

```R
tab_reg(gss_simple, outcome = "married", predictors = c("race", "rincome", "relig", "age"),
        family = "binomial", effect = "marginal", measure = "ratio" , empirical = TRUE
)
```

- "Obs_RR" use the 1/x.xx display for < 1, but "Model_RR" does not and print raw "0.xx" : rule should be, always display the inverse for OR, RR, and more generally multiplicative scale ; add an opt-in global option to print "0.xx" instead for people who don’t love it `family = "binomial"` is ok, `family = "binomial", measure = "ratio"` is ok, but adding `effect = "marginal"` breaks it (`display = "ame"` breaks it too)
- even with only `family = "binomial"` the Model_OR "obs" field use "0.xx" : with multiplicative scale  and with option default it should print 1/x.xx instead for consistency (otherwise it’s confusing)
- `family = "binomial"` have a "Reference population" OR, and so does the same with `measure = "ratio"`, but with `effect = "marginal"` it gets an empty field : is there a statistically sound way to compute something here, use by other common regression packages or apps ?  `effect = "at_reference"` also have empty Reference population.

adjustment

```R
tab_reg(gss_simple, outcome = "married", predictors = c("race", "rincome", "relig", "age"),
        family = "binomial", effect = "marginal", measure = "ratio" , empirical = TRUE, color = "adjustment"
)
```

- with color = "adjustment", the colors seem quite off. I’m not sure if it’s errors or bad design
  + race "other" have Obs_RR 1/1.06* and Model_RR 1.05 but the color is orange :
  + relig "Jewish" have Obs_RR 1.04 and Model_RR 0.89 but the color is blue :
  + relig Buddhist/Hinduist have Obs_RR 1/1.04 and Model_RR 0.86 :
- with color="adjustment", the color of the Obs_RR column (or the future unique empirical column) is misleading, since it’s the reference for comparison (for Model_RR) and should be all bold/no colors. Too much different colors with different color scales is confusing the user. Use the reference detection and reference management subsystem to make a clean framework (`or` display already implement a two references system, with one reference in cols and one reference in rows).
- I’m also not sure about the stars, since it can have Obs_RR `1.18***` and Model_RR `1.18***`: clearly the cell is gray and the stars are not for the adjustment, so I’m not sure what to do. Keeping as is is a bit misleading if the colors ares for the adjustement ; but at the same time stars for the significance of the adjustement are non-standard and will mislead some users too. Please study the problem, and propose me a user-friendly solution. Should we accept this is the sole case where colors and stars are for two different things, and document it, or can you think about a better solution ?


several outcomes

```R
tab_reg(gss_simple, outcome = c("married", "tvhours"), predictors = c("race", "rincome", "relig", "age"),
        family = c("binomial", "poisson"), empirical = TRUE, 
)
```

- in the html export, the outcomes names [married] and [tvhours] are repeated on each column : they should never appear here since the name of the col_var is already written above in the first headers rows of the exports (if it’s the same in tab(), it needs be corrected here too). They only stay in console because column names can’t be duplicated there.


predictor’s list

```R
tab_reg(gss_simple, outcome = "married", 
        predictors = list(race  = "race", 
                          two   = c("race", "rincome"), 
                          three = c("race", "rincome", "relig"), 
                          full  = c("race", "rincome", "relig", "age") ),
        family = "binomial", effect = "marginal", measure = "ratio" , empirical = TRUE, color = "adjustment"
)
```

- the "overall association" lines do not follow the order of the predictors

tab_vars

```R
tab_reg(gss_simple, outcome = "married", tab_vars = "race",
        predictors = c("rincome", "relig", "age", "tvhours"),
        family = "binomial", effect = "marginal", measure = "ratio" , color ="between_groups"
)
```

- here, all the "n" actually being side-by-side is good/readable/necessary, since it’s the result of tab_spread.
  Put them after the models column (at their right), it would be even greater.
- The reference column for "between_groups" doesn’t follow tabxplor rule : in bold (refcol)
- remove the useless verbose message: "ℹ `color = "between_groups"` also adds the aggregated interaction test to the footer (one extra model fit).\nAsk for it without the colours with `stats = c(..., "interaction")`."
- when I add `empirical=TRUE` here, the stats footer appear in the Obs_%_* part rather than on the model columns.
- here too, the "Overall association (LR)" lines are not in the order of the predictors, which is a bit confusing.


Additional features requests
- "Model_β" is not easily understandable: change it do "Model_coeff" everywhere.
- In the model legend, I want you to explicitly state the equivalence between the full name and the OR/RR/IRR/etc. abreviation for clarity. For example, with `family = "binomial", measure = "ratio"`, the legend says "Model: modified Poisson regression; risk ratio (vs the refrence category)": I would want something like "Model: modified Poisson regression; RR = risk ratio (vs the refrence category)".
- handle the tidyselect in `tab_reg`, while preserving predictor’s list behaviour: otherwise the two main user-facing functions have different syntaxes for variables selection, which is confusing.
- Add table metadata and accessor, teached in reg vignette and used by experts to check when they have a doubt, to get the different models formulas that were finally passed to the base model function (`glm()` etc.).

Open-questions about possible additional features requests
- Would there be a simple way to permit to add interactions in models ?
- In predictor’s lists, how difficult would it be to add a comparison between quadratic and normal version for numeric predictors ? (I guess it would be quite difficult, so maybe not worthwhile).
- Since numeric predictors are now standardised by default, can you confirm they are also **always centered by default** ? Is it documented ?

Corrected more readable table for regression vignette ?

Arguments reviews
- in `tab_reg()` and in `tab()`, would it be possible to put the `parallel` argument in a `...` subfunction (if it’s needed at all and the option isn’t enough) ?

weigths and survey-design


##### Phase 22b-i — write the phase roadmap

##### Phase 22b-ii — `tab_reg()` ant `tab()` `n` column (or `n` row for `tab()` with `pct = "col"`) reworking
Looking at `tab_reg()` n columns, I think the right way is to finish the dormant feature to be able to keep a unique `n` column giving all the information at once. With several outcomes, having one n column per outcome is not concise enough and add clutter. With predictor’s list the current default is ok, there’s only one column since the population of each model is by default the same. For spread tab_vars the "one n column per tab_vars" is justifiable since it’s the result of tab_spread.
- For tab() Total columns with add_n, I want a small change to avoid wasting horizontal space : instead of "100% (n= 9 838)", I just want "100% (9 838)" (with the 100% still in bold). In exports only (not console), I want the column header to state "Total (n=)" (only for this particular case, not with every "{pct} ({n})" display, and respecting the custom total name). Also do the equivalent for Total rows, keeping the two lines in one cell printing where it’s implemented (to avoid wasting horizontal space).
- I want to keep the current display when the population of the different models or col_vars is the same, and with tab_spread. But with several outcomes or several col_vars with different populations due to NAs (or predictor’s lists with different populations), I want the default to be one unique column printing the range of `n`: something like "100% (6 712-9 838)". The html tooltip of the Total/n column should print the `n` of every model or col_var (at display/export time, without storing them). The html tooltip of the cell should print the `tot_n` after the cell own `n`.
- Since it’s now a display time thing, I want to soft-deprecate add_n argument in `tab` (no error but inactive), and remove it altogether in `tab_reg` (never released), and I want to use a global option instead. Ensure the implementation is efficient and doesn’t noticeably slow display/export. It should have these possibilities : default to `"range"` ; `"n_min"`, only printing the mininum in the row (or col) while stating "Total (n_min =)" in the exports’s header ; `"each"` for exports only, using one Total/n column for each col_var or model by reading `tot_n`, with a display token etc. equal to the one of the column existing in the data (not in console) ; `"no"` for no n display at all (no n displayed in the total column or row in `tab`, no `n` column at all for `tab_reg`).
- With `"each"`, the exports must not repeat the [married] [tvhours] variable tag in all n columns, but integrate each one inside it’s own col_var (where the col_var name appear as an additional merged column header).
- In tab_reg(), remove the `N` line of the footer altogether, the information is already in the column "N", reference population ; if the dada is needed somewhere to gate something, use the `n` field of the constant / reference population row, and ensure it’s populated for all models.
- The Total column of tab and the n column of tab_reg, which are structurally equivalent, have a different behaviour because Total is the last column and n is the first column: it’s a wanted behaviour (the 100% is after the columns because they sum-up, the n column without 100% of tab_reg is near the predictors levels names because it does not depend on the outcomes and is not part of the model). With `"each"`, it becomes part of each col_var block even in `tab_reg`, at display time : it means we should now store the `n` of each factor predictor level in the population in the `n` field (printing it in tooltips even with `"range"` or `"min_n"`).
- In tab(), with `levels="first"` (or `levels="auto"` with all factors having 2 levels or less), the `100%` is misleading since it does not sum up to 100%, so in this case we only want the "(9 828)" or "(6 712-9 838)" but without the "100%". With `levels="first"` plus a `tab_vars` that is a `spread_vars`, we want to match the current `tab_reg` with tab_vars and spread behaviour : one `n` column for each level of the `spread_vars`.

#### Phase 22c — tab manual review

 is the `tot = c("row", "col")` argument still needed at all, if the totals are always printed ? If it’s only soft-deprecation, would it be possible to put it in `...` with the other soft-deprecated arguments ? Also, document in roxygen and link where to find the soft-deprecated arguments documentation.

 Rewrite the short versions of legends ?

##### Phase 22c-ii — tab_spread reworking

There’s a bit work remaining for tab_spread to behave as a very compact yet readable table.

```r
tab(gss_simple, rincome, c(married, tvhours), tab_vars = race, spread_vars = race, 
    pct = "row", na = "drop", totaltab = "table", 
    color = TRUE, color_signif = "grey_non_signif", ref = "tot", comp = "all", levels = "first"
) # main use case: very compact table (spreading a tab_vars with comp = "all")
tab(gss_simple, rincome, c(married, tvhours), tab_vars = race, spread_vars = race, 
    pct = "row", na = "drop",
    color = TRUE, color_signif = "grey_non_signif", ref = "tot", comp = "all", levels = "first"
) # not good (see below)!
tab(gss_simple, rincome, c(married, tvhours), tab_vars = race, spread_vars = race, 
    pct = "row", na = "drop", totaltab = "no", 
    color = TRUE, color_signif = "grey_non_signif", ref = "tot", levels = "first"
) # should be ok (no Ensemble columns), but "Error: ! Build failed on "rincome". Caused by error in `dplyr::full_join()` at tabxplor/R/tab.R:2236:5:! Can't join `x$rincome` with `y$rincome` due to incompatible types. ℹ `x$rincome` is a <ordered<b3d35>>. ℹ `y$rincome` is a <ordered<c817b>>"
tab(gss_simple, rincome, party3, tab_vars = race, spread_vars = race, 
    pct = "row", na = "drop", color = TRUE, color_signif = "grey_non_signif", ref = "tot"
) # it should still be quite readable with 3+ levels factors, but anyway it won’t be the most readable one.
```

- totals are not well managed yet. without `totaltab = "table"` the near empty Ensemble column is not so bad, but a "Total Ensemble" row is created above the "TOTAL" row : the "49%" cell should be in the `TOTAL` row.
- colors are ok with `totaltab = "table"` and `comp = "tab"`. But with `comp = "all"`, which is the real comparison interest here, it’s quite misleading, because the basis of comparison is Married_Ensemble for every cell (good behaviour), but it’s not readable: the non-colored cells of the "TOTAL" row are in bold as if they where the reference, but the reference is a cell and is Married_Ensemble "49%" (it should be the only one to be both not-colored and bold). If you can manage to use/extend the current reference management and detection system without adding to much ad hoc stuff and clutter in it, it’s even better (otherwise, you can maybe find a good workaround). Legends should also state clearly what is the reference in these different use cases.
- The 4 totals columns are useless (four 100% columns) and misleading (it does sum up to 100% anymore). Here, I want near the same behaviour than the current `tab_reg` with `tab_vars` and spread behaviour: no total columns, but one `n` column per level of the `spread_vars` (all at the end / at the right, so that visual comparisons stay possible because they are regrouped together). Also ensure the same is working not too bad for `pct="col"`.
- "TOTAL" is written instead of "Total" (please fallback to the base total name).
- In html exports, and certainly other exports, the two headers rows repeat the same thing, which is not very useful. Here for `married` the normal header should print the tab_vars levels (White, Black, etc.), while the `col_var` is set to "married" for all four tab_vars levels and only one merged cells "married" appear in the first header row of exports.
- `tab(gss_simple, rincome, party3, spread_vars = race, ...` should work (auto-adding race to `tab_vars` since it’s not passed anywhere before `spread_vars` ; with several `tab_vars` I’m not sure if it’s prepend or append to get the most useful totals calculated and the less voids in the table).
- The main compact table use case should appear in the introduction vignette (binary factor + numeric col_var).




#### Phase 22d — Black and white publication print manual review
The grey fill carries no direction, and cannot. o1..o4 and u1..u4 are the same four greys; direction is only readable from the cell's own bold/italic. That's forced by the ruling and by Bertin, and it only bites for a table coloured on the background channel alone. The legend names it ("Grey fill" both sides).
The legend now collapses repeated break-words — print shows +5 and +20, not +5 +10 +20 +30, because slots 1–2 render identically. That is honest, but it does mean the print legend lists fewer thresholds than the colour one.

#### Phase 22e — assumptions plots manual review

#### Phase 22f — `forest_plot` manual review
D6 has a limit I could not design away. ggplot has one scale per aesthetic, so a key list describes one ladder. legend_guide_spec() returns NULL when the plotted columns form several legend_group_by_body() groups and the caption prints the prose legend instead — the same grouping the footer uses, so they can't disagree about how many ladders exist.
theme = "print" forced the one deviation from the table palettes — its text slots are all black (the table separates directions by bold vs italic, which a point can't be), so a mark borrows the print palette's grey ramp. Nothing is lost: in a forest plot direction is the position relative to the null line.
`or_plot()` was deleted with its inert `point_size`. Reimplement `point_size` in `forest_plot`, since the model columns now store the factor predictors levels `n` in `n` field.

#### Phase 22g — Jamovi UIs manual reviews and final modifications

#### Phase 22h — documentation reviews

In the introduction vignette: teach levels = "first" and levels = "auto" with the FactoMineR:: tea dataset; also teach to use tab_vars + spread_vars to make a very condensed table of on the tea dataset.

**PARTLY DONE (2) — the assumptions behind a non-default `measure` are now written down.** Both regression vignettes gained `### With`effect = "coefficient"`, the measure chooses the model`, before the caveats list. The heading is scoped on purpose: only the COEFFICIENT row lets a measure change the fit — verified against the table, exactly three rows do (gaussian ratio -> `mr`, binomial ratio -> `rr`, binomial difference -> `rd`), while every `marginal` / `at_reference` row runs on the family's own model and differs only in the averaging step. The section holds: a table of every `(family, measure)` at `effect = "coefficient"` with what it fits and what the coefficient assumes, then what the literature says about the three that change the LINK (binomial ratio / difference, gaussian ratio) — and about the defaults, which are not assumption-free either. The recommendation is stated with its reasons rather than as a preference: keep the model on the family's own scale and get the reported measure through `effect = "marginal"`, because the logit always converges and cannot predict a probability outside 0–100 %, a marginal effect imposes no constant-effect assumption on the reported scale, and one fit answers every measure. With the counterweight: conditional and marginal are different ESTIMANDS, not two spellings, so the coefficient route is right when the conditional quantity is what is wanted. The three link-changing routes are graded rather than lumped — the modified Poisson is standard practice (its limits are small/sparse samples and unbounded risks), the identity link is the fragile one, PPML is consistent under a correct mean function only. Also corrected: the LPM fallback message claimed "same estimand, robust standard errors"; it targets the same risk difference but is a different ESTIMATOR, and now says so.

**PARTLY DONE — `levels` and the summed score now run on the `tea` data.** `gss_cat` has no real multiple-answer question, so every place that taught a *battery of binary items* was faking one out of unrelated variables (`married` + `black` + `income25k`, and a 0–2 "score" from `married` + `income25k`). Both moved to `FactoMineR::tea`, which carries two real six-item batteries: the intro uses *when do you drink tea?* (`breakfast` … `always`), the regression vignette *where do you drink tea?* (`home` … `pub`) — the "where" battery because it actually separates groups (men `1/1.44***` per place, `senior` `1.44**`), while "when" produced a table with nothing significant to read. `score_from_lv1()` moved with them: its section LEFT the programming vignette (where it was taught in the abstract, away from any use case) and is now three sentences inside the intro vignette's new `### Multiple-answer questions: one column per item` subsection, where the battery it sums is already on screen — the score joins the six item columns of the same `levels = "first"` table as a mean. The regression vignette just uses it now and points there. **`levels = "auto"` was undocumented**; it now has the example that shows what it actually decides — a battery of binary items beside a 3+ level factor, where it collapses the first and keeps the second (`tot = "row"` drops the Total column, which would read a misleading 100 % there until Phase 22b-ii lands). `FactoMineR (>= 2.0)` added to Suggests; the shared prep chunk is inline and identical in all six files (a `tea_data_formatting()` twin of `gss_cat_data_formatting()` was considered and rejected — no new public API before CRAN).

Two details worth keeping. The prep puts the "yes" level FIRST (`fct_rev()` on the items whose level 1 starts with "Not"), because that is both what `levels = "first"` keeps and what `score_from_lv1()` counts — the one thing a reader must get right. `Sport` gets the same cleanup, since it is a predictor in the regression example. `?score_from_lv1`'s `@seealso` was repointed from the programming vignette to the intro one. ⚠ Still open from this phase: the `tab_vars` + `spread_vars` condensed table on `tea`, which waits on Phase 22b-ii.


#### Phase 22x — very last features before release
- direct Word export, or teach to go through Excel ?

---

### Phase 23 — documentation integration and simplification 2


#### Phase 23a — vignettes simplification and integration
- Document undocumented stuff. `spread_vars` in `tab()` ?
- Vignettes should not be neverending. If some aspects, either expert, or on the contrary pedagogical and near useless to experts, need to be placed in new vignettes, make me propositions. Point to `tab_shape()` · `tab_supports()` · `reg_measures()` · `tab_columns()` · `fmt_attr()`, etc., when relevant.


#### Phase 23b — roxygen2 documentation simplication
- Point to the right vignette for more details and pedagogy. Point to the introduction vignette in `?tab` description and the regression vignette in `?tab_reg` description. Start the english vignettes with a link to the French vignette to say it exists, if not already done.

#### Phase 23c — user messages simplication and focus
Many user messages print useless dev stuff (always remove) or internals (only accepted if the function is itself advanced programming) made for the maintainer: as a general case, messages should speak to the user, and they should speak about statistics, statistical soundness, real caveats, user-facing arguments and the like, in a simple and clear way that is really helpful to non-experts.

#### Phase 23d — drastic `NEWS.md` simplification
`NEWS.md` `# tabxplor 2.0.0 (in development)` was already drastically simplified in Phase 18y, but have since Phase z2 accumulated all dev history again. Most of it is really not user-facing and irrevelant here (and already in other dev documentation). A **drastic** reduction is needed here, no dev details **at all**, straight to the point, please **reduce around 400 lines to maximum 150 lines** :
- "## New features" should only list the most important things. New exported functions, like tab_counts() and others, should be presented in one quick sentence. New arguments in one quick sentence, rarely more. Everything about `tab_reg()` should be near-zero-words : in spirit, "possibility to do regressions added, see the vignette".
- Drastically reduce "## Changes that may affect existing code", only keep what is really important
- Drastically reduce bug fixes (same thing really), to only speak about very very few bugs that could have been hit by real user. Remove everything about any new argument or implementation. Make it small.
- Keep deprecation, reduce it’s size, list elements quickly, but keep differenciate what is soft deprecated and what is hard deprecated.

#### Phase 23e — Tests simplification
- testthat tests have grown organically, it was right for development, but would slow future dev for no real benefits: I want you to select the tests that are really necessary , and to move the others to a unique script not run with `test`. **The full suite must go below 20 seconds** (parallelised, on this computer).

#### Phase 23f — `dev/` folder
Files inside the `dev/` folder have grown organically, with many now useless files and outdated ones, which is very messy for future development : I want you to clean and reorganise the folder and main files.
- Put all files related to v 2.0.0 dev history and of no real use for future dev in an 2.0.0 archive subfolder. That should be most of them.
- Only keep at `dev/` root level a few selected .md files that explain in detail the architecture or functioning or use cases of some subsystems, and will be really useful for future dev : clean these files, simplify them by removing useless dev history and focusing on current architecture and usage, ensure they are up-to-date compared to the current design and code ;  organise them internally in such a way that goals, design and architecture decisions, usage, and everything giving the big picture come first, and details come next ; reference them in the architecture document.


#### Phase 23f — french translation


### Phase 24 — CRAN release











---

## The last step of every implementation: update the documentation

The final test suite is long, **so start updating docs and writing the "DONE" summary while it runs** (if a fix follows a failure, correct the affected docs once it passes). Keep everything present-tense and concise, respect the **documentation ecosystem** hierarchy (top of this file), and **edit the files yourself** — never hand the maintainer lines to paste. The ONLY place dev history is allowed is the "DONE" summary (step 4).

1. **File-header + inline comments** of every module you touched — make them state the CURRENT design, caveats and "why", never how it got there; add or adjust `# DESIGN:` / `# WARNING:` tags next to changed logic. Cut, don't accrete.
2. **Phase "DONE" summary** — under its own `#### Phase <x> — <title>` header in the roadmap. This is the ONE place dev-history detail belongs (what changed, why, measurements). CLAUDE.md is the ONLY place it goes; the maintainer moves finished phases to `dev/tabxplor_2.0.0_roadmap_DONE_PHASES.md` once the roadmap gets cluttered.
3. (**Repository Map** in this file — refresh a file's role line only if you added, removed or repurposed a file; keep is absolutely brief, never add clutter here, cut instead of accreting; otherwise skip.)
4. (**`NEWS.md`** — user-facing / CRAN-facing only (new or changed functions/arguments, deprecations, important user-facing fixes); radically minimal, usually skip.)
5. (**`README.Rmd`** — only before a CRAN release.)
