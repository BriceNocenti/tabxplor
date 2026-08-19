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
- `tab-display.R` — the `{}` display grammar, its named layouts and `add_n`/`add_pct`; `DISPLAY_TOKENS` / `DISPLAY_PRESETS`.
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
- `reg-estimand.R` — effect × measure → fit/estimand; `REG_ESTIMANDS` / `REG_FAMILIES` / `REG_WORDS`; `reg_measures()`.
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
| `DISPLAY_PRESETS`  | `tab-display.R`      | The named cell layouts both producers resolve (`est` / `est_ci` / `est_base` / …)      |
| `CI_METHODS`       | `tab-agg.R`          | The confidence-interval methods and geometries (with `CI_GEOMS`)                       |
| `COLOR_SCALES`     | `tab_classes.R`      | The break scales and palettes                                                          |
| `TAB_ARGS`         | `tab-args.R`         | The argument surface (signatures, values, option twins, prose; + `EXPORT_ARGS`)        |
| `TAB_OPTIONS`      | `tab-options.R`      | The package options and their defaults                                                 |
| `ROW_KINDS`        | `row-model.R`        | The row-kind vocabulary                                                                |
| `TEST_ROWS`        | `tab-test-display.R` | The footer / statistical-row catalogue                                                 |
| `TAB_OPS`          | `tab-shape.R`        | Which reshape operations accept which table shape                                      |
| `REG_ESTIMANDS`    | `reg-estimand.R`     | What a regression column estimates (family × effect × measure)                         |
| `REG_WORDS`        | `reg-estimand.R`     | The header acronyms and their expansions (with `REG_CONTRASTS`, the contrast markers)  |
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

| Attribute      | Type | Meaning                                                                                         |
|----------------|------|-------------------------------------------------------------------------------------------------|
| `scale`        | chr  | What the column estimates — a key into `EST_SCALES` (`level_pct`/`mean_diff`/`odds_ratio`/…)    |
| `pct_type`     | chr  | Which kind of percentage (`row`/`col`/`all`/`all_tabs`/`none`) — the axis its reference lies on |
| `ci_method`    | chr  | Which interval engine built the bounds (`wilson`, `newcombe`, `welch`, `katz`, …; `""` = none)  |
| `conf_level`   | dbl  | The confidence level the interval and thresholds were computed at                               |
| `degf`         | dbl  | Degrees of freedom the interval refers to (`NA` → refer to z)                                   |
| `basis`        | chr  | How the interval was computed: `n` / `weights` / `design` / `design_partial`                    |
| `col_var`      | chr  | Name of the column variable                                                                     |
| `col_group`    | chr  | Which sub-population the block belongs to (a spread level or `tab_vars` group; `""` otherwise)  |
| `ref`          | chr  | Reference type (`tot` / `first`)                                                                |
| `comp_all`     | lgl  | Compare against the total table (TRUE) or the subtable (FALSE)                                  |
| `totcol`       | lgl  | This column is a total column                                                                   |
| `refcol`       | lgl  | This column is a reference column                                                               |
| `color`        | chr  | Colour measure (length 1, or 2 for a text+background channel pair)                              |
| `color_signif` | chr  | Significance policy: `ignore` / `grey_non_signif` / `guaranteed_effect`                         |
| `model_family` | chr  | A regression column's own family (`""` on crosstabs)                                            |
| `role`         | chr  | A regression column's role: `model` / `emp` / `n` (`""` on crosstabs)                           |

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

**The estimand — effect × measure** (`reg-estimand.R`, `REG_ESTIMANDS`). The user asks two questions: which **contrast** (`effect`: coefficient / marginal / at-reference) and which **measure** (odds ratio / ratio / difference / log). One declared row per `(family, effect, measure)` answers both: it names the internal fit/link, the exponentiate flag, the base measure acronym, and the stored `scale` the column carries. The **family is auto-detected** from the outcome — a 2-level factor → logistic/OR, numeric → linear/mean difference, a count → Poisson/IRR, 3+ unordered → multinomial, ordered → cumulative-OR ordinal — but one table can mix families (each column stores its own `model_family`).

**One name per quantity** (`REG_WORDS` + `REG_CONTRASTS`). A header names the **measure**; the **contrast** is a marker on it and a log wraps the result, so the word is *composed* — `marker ∘ log-wrap ∘ acronym` gives `OR`, `mRR`, `refRD`, `log(cumOR)` — which is what stops two estimands sharing a header and one estimand being named twice. Each acronym's expansion is declared once and read by the header, the `Model:` footer ("`OR` = odds ratio (vs the reference category)"), `reg_measures()`, the abort and the generated `?tab_reg` sections. The crude companion and the colour legend both take the measure **without** the marker: a univariable effect has no adjustment to be marginal over, and a legend that named the contrast would split the crude/model pair into two blocks. `reg_measures(data, outcome)` lists what an outcome offers; a missing `(effect, measure)` combination aborts with the list of what it does offer.

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

The docs form one hierarchy, general to specific. **Each fact is stated at exactly one layer, referenced (never duplicated) across the others, and always written present-tense** — the current design is the reference point, never how it got there. The one place dev history is allowed is the roadmap "DONE" summaries. In R scripts, **the comments/code ratio should stay under 0.2**.

- **`## tabxplor architecture`** (this file) — the cross-subsystem big picture: goals, data-flow, the declarative pattern, the type system, each subsystem's role and its meaningful "why". Rewritten only when the maintainer asks,never per session ; mostly with targeted cuts and replacements rather than accretion.
- **`## Repository Map`** (this file) — the file index: one role line per R file. *Cut, don’t accrete.*
- **R file-header comments** — per-file subsystem design: current architecture, key constraints, a pointer up to this file.
- **Inline `# DESIGN:` / `# WARNING:` tags** — the non-obvious "why" at the exact line, caveats to avoid, etc.
- **Vignettes** (`vignette("tabxplor")`, regression, programming) — usage and teaching, for users.
- **Roxygen man pages** (`?tab`, `?tabxplor-vctrs`, `?tabxplor-options`, `?tabxplor-data.table`) — user-facing reference: *usage* and the main use cases, never build/internals/history.
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

**DONE.** Plain `#` comments **3849 → 1753 (2.19x)**, comment/code ratio **0.78 → 0.35**, code unchanged at 4948 lines; roxygen 1031 → 808. Per file (plain `#`): `tab_reg` 1689->594 (2.8x) · `reg-empirical` 520->194 (2.7x) · `reg-resolve` 474->200 (2.4x) · `reg-spec-build` 195->89 · `reg-estimand` 395->269 · `reg-assumptions` 360->255 · `reg-influence` 216->152.

**The archaeology is gone — that part is complete.** Zero phase tags, `KEY n`, `H12`, `SS14`, `D6`, `z10`, zero "was / used to / no longer", zero bug post-mortems, zero benchmark figures, zero `dev/*.md` decision-log pointers remain in the seven files (two pointers survive by design, to the gap-test derivation and the assumption-plot designs, which hold maths a header cannot restate). Three epitaphs for deleted functions are deleted with them (`reg_fam_logscale()`, `reg_global_lines()`/`reg_term_test_line()`, and the nine-line `tab_logit()`/`multi_logit()` block). Every header is a `# PURPOSE / ROLE / KEY CONSTRAINTS` essay pointing to `CLAUDE.md § tabxplor architecture`, and `tab_reg.R` now runs on **16** `# === SECTION:` banners instead of 11.

**Why 2.17x and not 5x, measured rather than asserted.** After the cut, the seven files hold **619 comment blocks outside their headers, and 524 of them are 3 lines or shorter** (959 lines, averaging 1.8 lines each) — one note per top-level definition plus roughly 1.5 inline notes, which IS the target style. Only 196 lines sit in blocks over 6 lines, and those are the protected dictionaries: `REG_ESTIMANDS` (42) and `REG_FAMILIES` (25) in `reg-estimand.R`, `REG_EMPIRICAL`'s 7-slot shape table, `REG_CHECKS`' 9 slots, and the four record dictionaries (`new_reg_ctx` 34 slots, `new_reg_shared` 21, `new_reg_spec` 11, `new_reg_spec_product` 11). The three files that floor hardest are exactly the dictionary- and trap-heavy ones: `reg-estimand.R` is 50 header + 67 dictionary + ~150 two-line notes over ~50 definitions, and `reg-influence.R` is almost entirely statistical *why* (the influence-function identity, the delta-vs-IF variance rule, the multinomial vcov ordering trap, the polr bread). **Reaching 0.20 from here would mean deleting ~400 two-line WHY notes or trimming the dictionaries — content, not history.** That call is the maintainer's; the lever is named here rather than pulled.

**`?tab_reg` rewritten by hand: 690 -> 468 roxygen lines (-32%), the Rd 790 -> 567.** Nothing left the CRAN contract — all 26 `@param`, both `@section`s, both `@eval`-generated sections, `@examples`, `@references` and `@return` are in place and in order, `\usage` is byte-identical and every `\item{}` name set matches. The cut is duplication first (`@details` restated five `@param`s in miniature; the modified-Poisson essay was written twice; the contrast-marker rule three times, once of them generated), then deferral: the ~100-line `color`/collapsibility essay to 30, the *Model checks* `\describe{}` to 26, `@param empirical` 63 to 30 — each keeping what changes a user's choice and leaving the reasoning to `vignette("tabxplor-reg")`, which already carries those sections. No vignette anchors were hard-wired (Phase 23b finalises them).

**Fifteen documentation defects fixed.** Six were dead function names inside live text: `reg_stage_fit()` (-> `reg_stage_setup()`), `reg_gof_tibble()` x2 (-> `reg_gof_rows()`), `reg_stage_empirical()` (-> `reg_stage_crude()`), `built_per_fit`'s "old role" (the identifier exists nowhere), and — worst, because it sat **inside a WARNING that had to be kept** — `reg-assumptions.R`'s i18n rule routing `gettext()` through `reg_check_spec_entries()`, which does not exist; the label builder is `reg_check_label()`. Four were stale facts: `reg-resolve.R`'s header naming `REG_DISPLAY_SHORTHANDS`, deleted in 22a-i; its S3 dictionary documenting a column `dep` when the tibble field is `outcome`; `reg-empirical.R`'s banner block documenting a grid shape (`emp_base` / `emp_var`) the function had stopped returning, contradicting its own dictionary 130 lines below; and `reg-influence.R`'s header placing `reg_gap_se_columns()` and the numeric crude fit in `tab_reg.R` when both are in `reg-empirical.R`. Three were stale vocabulary: `effect = "ame"` / `"ame_ratio"` in four comments (the values have been `coefficient` / `marginal` / `at_reference` since 19e), the "only `svyrecvar()` caller" claim (it routes through `svy_var_recvar()`), and `reg_build()`'s banner counting "seven stages" where it runs eight plus the split. Two were structural: three `#' @keywords internal` stubs stranded above plain comments so they attached to nothing (one a duplicate, deleted; two moved onto their objects), and three function descriptions sitting above the wrong function (`reg_model_line`, `reg_fit_multinom`, `reg_ordinal_diagnostic`), each re-attached.

**Two of the fifteen were in `?tab_reg` itself, and both were user-facing.** The argument map in `@details` and `@param outcome_level` both told the reader to use **`reference`** — a name retired before release, which now aborts with "unknown argument"; both now say `ref`. And `display = "\{est\}..."` was escaped in a way that renders as a literal `\{est\}`, backslashes visible, on the built page (verified through `Rd2txt`); the unescaped form is what Rd wants, since balanced braces need no escape.

**Also reported, out of scope**: `R/tab-test-display.R` names both dead functions (`reg_gof_tibble` at :426, `reg_check_spec_entries` at :126 and :432) — Phase 21b-vi owns that file. And the `@keywords internal` / `@noRd` convention is split three ways across the package, which is a package-wide call.

**Verified.** All **287 top-level definitions across the seven files are byte-identical to the pre-edit working tree** (parse -> per-name `deparse` comparison; the baseline is the WORKING TREE, not `HEAD`, because two of these files carried uncommitted 22a-iii code). Behaviour therefore cannot have changed, and no test run was needed. `devtools::document()` clean, **NAMESPACE unchanged**, only `man/tab_reg.Rd` rewritten with identical `\usage` and `\item{}` name sets; roxygen is byte-identical in six files and changed only in `tab_reg.R` (the rewritten page plus the one deleted duplicate stub).

**Cost, and the one method rule that failed.** ~2.7M subagent tokens (165K survey + 2.57M rewrite) across two surveys and three rewrite agents — the same order as 21b-iii, despite rule 11's changes, because **every agent had to be re-engaged for a second pass**. The cause is now known and is a flaw in the brief, not in the agents: **a KEEP list stated by CONTENT anchors an agent to keep each item at its CURRENT LENGTH.** All three first passes deleted the archaeology correctly and then stopped, each arguing (with correct arithmetic) that the named KEEPs already consumed the budget. The fix for 21b-v to -vii: state a per-block LINE BUDGET beside the KEEP list — "a kept item is its rule in 1-4 lines, never its derivation; only a named dictionary may exceed 6 lines" — which is what the second-pass message said, and what moved `reg-empirical.R` from 301 to 194 and `reg-resolve.R` from 320 to 200 in one round.

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

#### Phase 22a — one crude column, one model column, one display grammar

⚠ **Read `dev/reg_crude_adjusted_and_display_integration.md` before planning any sub-phase**: its §1 holds the goals and the full decision register with the evidence, §3 the measured diagnosis, §5 the target design, §6 the deletion inventory, §8 the captures. Every claim there is a capture from the running package. What follows is the big picture and the phase order only.

**The goal.** `tab_reg()` builds the observed column, the model column and the display as three separate systems that happen to sit side by side; `tab()` builds ONE column carrying every geometry of one comparison, with `display` choosing what to print. But the observed and the modelled column are the SAME estimand computed twice — once with one predictor, once with all of them — so they must be one column SHAPE built twice: same stored scale, same colour measure, same display template, same digits, same legend block, with the crude level (`%` / mean) folded into the crude cell exactly as the adjusted level already rides in the model cell. Three goals follow, in the order they matter to a reader of the table: (1) **one comparison, one ladder, one legend block** — the crude/adjusted comparison is then read ACROSS the table instead of needing a display of its own; (2) **one display grammar shared by `tab()` and `tab_reg()`**, where the same tokens and presets mean the same thing on every family and both columns, and choosing a display never triggers a computation nor changes a number; (3) **one name per quantity**. ⚠ **The phase is a NET DELETION**: nothing new is invented, every item is a row added to or removed from a fact table that already exists, and `MEASURES` / `EST_SCALES` / the colour engine are not touched at all — the two-ladder problem is fixed by removing a column, not by teaching the engine anything.

**The decisions** (full text + evidence in the doc's §1.2 — respect them without re-deriving them, and do not silently widen them).

- **Columns** — **D1** one crude + one model column from one shape · **D2** `empirical = FALSE | TRUE | "cell" | "column"`, the in-cell fold silently replacing the pair wherever per-category columns would multiply (this DELETES the multinomial `visible = FALSE` rule instead of adding a mode), `"column"` the expert exit door · **D3** the crude column takes the model column's colour measure, `REG_EMPIRICAL$*$color` deleted (this is the whole two-ladder problem, fixed for every family with no per-family branch) · **D4** under a gap measure the crude column IS the reference column: uncoloured by construction, `refcol = TRUE`, bold through a declared `get_reference()` arm · **D5** one legend block, the two-channel case included · **D6** stars stay on the estimate the cell prints.
- **Display** — **D7** two tokens, `{est}` (the column's own estimate, via the existing `fmt_center_field()`) and `{base}` (the adjusted prediction / observed level, by `var_kind`), identical in both producers · **D8** a `{gap}` token, so print / Excel / Markdown reach the gap at all · **D9** presets `est` / `est_ci` / `est_base` / `base_est` / `base` / `base_ci` in ONE shared alias table (`"value"`, `"prob"`, `"ame"` deleted) · **D10** an adjusted prediction is a display slot, NOT a `measure` · **D11** every field always populated, so `display` is post-hoc and never changes a number · **D12** ONE multiplicative-inverse rule (`mult = TRUE` → `1/x.xx` in every path) + an opt-out option, moved in from 22b · **D13** two renames: `num_ci` → `base_ci`, `pct_base` → `pct_type` (+ reword the `type` deprecation abort to name the SPLIT, else it reads as a no-op) · **D14** removing the stars from a descriptive copy is `set_pvalue(x, NA_real_)`, documented — no new API.
- **Vocabulary** — **D15** the header names the measure, the contrast is a marker ON the measure (unmarked = conditional, `m` prefix = marginal, `@ref` suffix = at the reference profile) · **D16** `measure = "log"` names what it logs · **D17** ordinal is `cumOR` on both sides · **D18** gaussian difference is `diff` / `mdiff`, which SUPERSEDES 22b's `Model_coeff` request (the word must be able to take the marginal marker; `mcoeff` is nonsense) · **D19** one declared `long` expansion per estimand row, read by every consumer · **D20** on per-category tables the measure lives in the `col_var` span · **D21** the vignette grid = acronym + meaning + a first column for the outcome kind.
- **Inference** — **D22** the crude interval is the univariable model's UNDER THE TABLE'S OWN BASIS. Measured: unweighted the model is `lm` / `glm` and the closed form is pooled over ALL k levels (agreement 4e-14); weighted it is `svyglm` and the closed form is the per-group variance on `n_eff` (3e-03); today's fixed pairwise `student` / `quasipoisson` is right in NEITHER basis (8.9e-02). So `CI_METHODS$mean_diff` gains `"ols"`, `mean_ratio`'s `"quasipoisson"` is redefined to the global Pearson dispersion it is named after, and the `basis` attribute every column already carries selects between them · **D23** the crude CI method is labelled from the estimand, not from the engine key (Katz IS the Wald interval on the log risk ratio), which is what merges the legend bodies with no grouping-key change.

Deferred elsewhere: the vignette prose for the new vocabulary (22h / 23a), one `jmvtools::prepare()` for every jamovi change of Phase 22 (22g), the `n` column's own semantics (22b-i).

##### Phase 22a-i — the shared display grammar and the crude-interval parity

**D7–D9, D12–D14, D22–D23** — everything that also changes `tab()`, and that the merge stands on: the three new tokens + the one shared preset table (and the end of `"value"`'s non-token exemption), the two renames, the one multiplicative-inverse rule, the basis-aware crude interval with its estimand-based legend label, and the `set_pvalue(NA_real_)` recipe. Expected golden movers: the inverse rendering, the renames, the crude gaussian / poisson intervals. Doc §1.3.

**DONE.** Suite **FAIL 0 | WARN 1 | SKIP 4 | PASS 7350** (7284 before; the WARN is the pre-existing over-dispersion note in `test-reg-estimand.R`). `devtools::document()` clean; NAMESPACE gains exactly `get_pvalue` / `set_pvalue` and the two renamed accessors.

**`{est}` / `{base}` are a display REWRITE, not new arms.** `EST_SCALES` gained two columns, `est_display` / `base_display`, each a `DISPLAY_TOKENS` key (FK-checked): the token a column renders "the estimate" / "the level it sits on" as. `fmt_resolve_scale_tokens()` applies them ONCE, at the top of `get_num()` / `set_num()` / `format()` / `fmt_ptype_label()` / the tooltip's `shows()`, so from there on `{est}` IS `or` / `diff` / `pct`… and every existing mask, glyph, reference annotation and Excel numFmt applies to it unchanged. `log_coef` declares `base_display = NA` (a link-scale coefficient sits over a probability on a logistic and a mean on a linear model — guessing would be a lie); it resolves to `blank`, so the token renders void and `display_note_empty()` fires. `{gap}` is derived like `resid`: `get_num()` returns `fmt_adjustment_score()`, the very number `color = "adjustment"` grades, and it joins `obs`'s per-column rendering three-way — which also fixes the doc's §3.5 defect (a `points` gap printed `-0.01` where the cell printed `-23.0%`; it now goes through the same ×100 / signed / `%` masks as `diff`).

**One multiplicative rendering, and it is not a new declaration.** The glyph pair was already stated once per measure (`MEASURES$<m>$break_over` / `break_under`), and both the token (`DISPLAY_TOKENS$comparison`) and the column (`EST_SCALES$label_meas`) already name their measure — so the rule reads them instead of adding a third copy. `pct_ratio` / `mean_ratio`'s `label_meas` was corrected from `"odds_ratio"` to `"ratio"` (a mean-ratio forest axis drew `1/2` while its cells drew `÷2`). The block moved OUT of `special_formatting`, which is what fixes the maintainer's Phase 22b report: a composite recursed with `special_formatting = FALSE` and printed a raw `0.37` beside a `1/2.67` for the same quantity. It now covers the bare token, every composite, `obs`, `gap` and the `est_ci` bracket — whose bounds invert but are NOT reordered (the glyph carries the direction, so `1/3.13` still sits left of `1/2.27`). `options(tabxplor.ratio_print = "raw")` restores the journal convention on cells and on the legend ladder at once, and `Obs_*` / `Model_*` `obs` fields invert too.

**Decimals are the CELL's, with one floor.** `DISPLAY_TOKENS` gained `min_digits` (2 on `ratio` / `or` / `or_pct` / `OR` / `OR_pct` / `est_ci`, 1 on `resid`), and it overrides **only `digits = 0`** — `digits = 1` prints `1/1.1`, `= 3` prints `1/1.062`. This also replaced the two hard-coded floor lines in `format()` with one declared lookup. ⚠ The floor CANNOT be stored on the cell instead, and the comment says so: `digits` is per-cell but one value serves every display of that cell, so a percentage wanting 0 shares it with the ratio wanting 2 — the adjustment has to happen where what is being *shown* is known. The `ratio` token's old trailing-zero trim is gone with it (it turned `1/1.06` into `1.1` at `digits = 0`).

**A REFERENCE cell prints a bare `1`** — no glyph, no decimals. `×` means "times the reference", which the reference is not, and the short bare number is what makes its row stand out among `×1.29` / `÷1.46`. A cell that merely *rounds* to the neutral keeps `×1.00`, so the reader can tell "this is the baseline" from "this happens to equal it". The mask is `get_reference(x, "all_totals")` — the one the odds-ratio cell has always used — now shared by every multiplicative token, which replaces that block's `"1\.0+"` regex. ⚠ It is ANDed with "the value rounds to the neutral": a regression's Constant row IS a reference row and its odds ratio is the baseline odds, a real value.

**The presets are ONE table.** `DISPLAY_PRESETS` + `display_resolve()` in `R/tab-display.R`, read by `tab()`, `tab_reg()` and `set_display()`. `REG_DISPLAY_SHORTHANDS`, the `"value"` sentinel and its `allow = "value"` foreign-key exemption, and the whole `num_ci` special-casing (four sites) are **deleted**: `base_ci` is now the ordinary template `"{base} {ci}"`, and per-column adaptation needs no branch because each column answers `{base}` itself. `tab_reg(display =)` defaults to `NULL` like `tab()`'s. ⚠ `display = "ci"` no longer means `est_ci` on a regression — `ci` is the *token* (the interval alone), as it always was in `tab()`; that is the point of one grammar.

**Display reach.** The binomial-only degrade is gone (both the per-column guard and its whole-call twin), so `est_base` / `base_est` / `base` fold an adjusted **mean** on gaussian / poisson exactly as they fold a probability on binomial — `reg_marginal(want_pred = TRUE)` already returned the prediction on the response scale for every family. The fold writes into the field the column's own `base_display` names, and a new guard refuses to write into the column's own estimate field. The marginal-path reset stays (22a-ii).

**D22, measured.** `ci_pool_disp()` computes the ONE dispersion a model estimates for a whole variable (`s_p² = Σ(n_g−1)v_g/(N−k)`; `φ = Σ(n_g−1)v_g/m_g/(N−k)`) and the two mean engines take it as `pool`; with none they fall back to the pair, which IS the level set at k = 2 — so `test-ci-engine.R`'s two-group parity blocks stayed green as written. Verified on a 3-level predictor: `mean_diff = "ols"` reproduces `confint(lm())` to **2e-14** and its pooled variance IS `sigma(lm())²` with `df.residual()`; the redefined `quasipoisson` reproduces `summary(glm(quasipoisson))$dispersion` to **1e-8** and its SE to **5e-9**. `REG_EMPIRICAL` lost the duplicated family-level `method_mean_diff` / `method_mean_ratio` and each effect shape gained `ci_method_design`: unweighted the crude column runs the model-based form, weighted the sandwich (`welch` / `robust`), selected from the `basis` the crude grid already computes. **Defect found and fixed**: `poisson$irr` / `$irr_log` declared `ci_method = "katz"` while `reg_empirical_columns()` actually ran `ci_mean_ratio(quasipoisson)` — the stored attribute and the prose legend both lied.

**D23 landed as decided** (same phrase + one rider). `CI_METHOD_WORDED$katz` now names the estimand, so `Obs_RR` and `Model_RR` render one body and merge with **no change to `legend_group_by_body()`**; `quasipoisson` joined it with an `IRR` arm (the dispersion-scaled closed form IS the quasi-Poisson model's own interval on the log rate-ratio), keeping "quasi-Poisson interval" for a plain `tab()` ratio-of-means. The rider is appended after grouping from one declared map (`CI_METHOD_CLOSED_FORM`), so a crosstab keeps "(Katz closed form)" and a merged reg block reads "— Katz closed form on the observed column".

**The two renames.** `pct_base` → `pct_type` across 40 package files (zero jamovi hits); `dev/verify_golden_field_delta.R` proved it value-preserving through `EXPECTED_ATTR`'s function form — **1788 cells across 36 cases, only the declared addition/removal differs** — before the goldens were regenerated. The `type` deprecation abort and its `NEWS.md` bullet were reworded to name the SPLIT. `num_ci` → `base_ci` everywhere including both `.a.yaml`; per the maintainer's call the generated `.h.R` stays for **22g**'s single `jmvtools::prepare()`, which the suite cannot see (the vocabulary test reads the YAML). Also updated: `jmvtabreg.a.yaml` / `.u.yaml` to the six layouts with an `auto` idle value.

**Defects reported and fixed on the way:** `get_pvalue()` was documented in `?tab` but not exported (both accessors are exported now); `R/utils.R`'s header still claimed `.onLoad()` was the source of truth for option defaults (`TAB_OPTIONS` has been since 20b); the reg vignette said the display folds "need the `marginaleffects` package" (the engine is the dependency-free `gcomp`).

**Cost measured, on the one hot path that grew.** `get_num()` gained one pass over the display vector: **+0.036 s per million cells** (1.27 → 1.47 s for 5 calls on 1e6 cells, +14 % of its own time); `format()` and a `tab()` build are unchanged within noise. `%chin%` was tried and measured *slower in context* than base `%in%`, so it was reverted.

##### Phase 22a-ii — the crude/adjusted column merge

**D1–D6, D10–D11** — `REG_EMPIRICAL` restructured (the 8 `base` rows, the 14 `color` and the 14 `display` declarations deleted; `base_field` / `base_ci_method` / `base_digits` added; one `fmt()` call per effect), `empirical`'s four values and the deletion of `shape_visible()` / `visible` / `emp_off`, always-populated fields + the `needs = "marginaleffects"` fix, the per-row-kind fold rule, colour / legend / reference, and the tooltip contract — one `fmt_center_field()`-driven "estimate + interval + p" fragment, which is what finally gives a logistic table's default column an interval on hover. Doc §1.3, §5.

**DONE.** Suite **FAIL 0 | WARN 1 | SKIP 4 | PASS 7402** (7350 before; the WARN is the pre-existing over-dispersion note). `devtools::document()` clean, **NAMESPACE unchanged**, `man/tab_reg.Rd` keeps identical `\usage` / `\item{}` name sets; `man/tabxplor-options.Rd` gains exactly one row.

**One column shape, built twice.** `REG_EMPIRICAL` lost its 6 `base` rows, its `color` column (28 values), its `display` column (28), its `pct_type` (28), its `digits` (28) and the dead `from` column — and gained NOTHING. Every fact it used to carry per shape is now read from where it already lived: the level a cell prints is `EST_SCALES$base_display`, the colour is the model column's (both channels), the display is the table's resolved one, the precision is `REG_CELL_DIGITS` and `pct_type` is derived from the scale. `reg_empirical_columns()` emits ONE column per effect, carrying the crude estimate, its interval, its p-value AND the level it sits on; `shape_visible()` / `visible = FALSE` / `emp_off` / `reg_crude_in_cell()` / `reg_display_folds()` / the marginal-path `display` reset / `REG_ESTIMANDS$display` / two foreign-key edges are deleted. The four probability families (binomial / rr / grouped_binomial / multinomial) now share ONE closed-form arm dispatching on the SHAPE, so a multinomial category and a binary outcome run the same arithmetic instead of two copies of it.

**`empirical` resolves to a MODE at the boundary** — `no` / `column` / `cell` — so no consumer re-derives where the crude effect goes. `TRUE` draws a column, except where one model column would need SEVERAL (a 3+ level outcome, whose crude effect is per outcome category): there the value rides in the model cell as `{est} ({obs})`, one template for every family. `"column"` forces the pair anyway and **builds the per-category crude columns** — named and spanned from the model column each mirrors, paired in `reg_spec_build()` where both are in hand.

**`display` is now post-hoc, and that is the phase's load-bearing invariant.** Every model column stores its adjusted prediction and its additive marginal effect whether or not the cell prints them (`reg_fill_base()`), so choosing a layout triggers no computation and changes no number — asserted directly, and the reason `reg_apply_display()` is a pure template writer using the crosstab's own per-cell eligibility rule. Cost measured: +0.24 s on a coefficient table, +0.09 s on a ratio-marginal one (`dev/benchmarks/results_2.0.0/phase22a-ii_always_populated.txt`). **A factor level's additive contrast is derived from the two adjusted predictions** rather than from the sweep's own contrast: exact (averaging commutes with an additive contrast) and reference-INVARIANT, which is what keeps jamovi's digest fast path a cache HIT on a reference change. The digest carries the sweep and gains `multiplier` in its key — a k-unit contrast on a non-identity link is not k times the one-unit one, measured 9e-4, so the alternative was a wrong number.

**The default is MIRRORED, and it is not cosmetic** (maintainer's decision this session): the crude cell prints `({base}) {est}`, the model cell `{est} ({base})`, so the two ESTIMATES end up adjacent and the row reads as the modelling pipeline itself — observed level → observed measure | modelled measure → adjusted level. Rationale recorded in the design doc's new §5.3.1. It needed **the primary rule**: the primary token of a composite is now *the first one outside brackets* (all bracketed → the first), so an aside may be written FIRST without ceasing to be an aside. Every template the package writes keeps the token it always centred on; only the new leading-parenthetical form changes, in the direction wanted. The primary is what carries the stars, what `get_num()` / Excel return, what the colour gates dispatch on — and `format(bold_split =)` now reports its character RANGE, not a prefix width, which the html and Markdown bold-splitters needed to stay correct.

**Only the primary field is coloured** (maintainer's request): a composite reads as one number with an aside, so the shade grades the number and the aside keeps the ordinary text colour. `options(tabxplor.color_secondary =)` takes `"black"` (default, theme-aware), any R colour name or hex code, or `"same"` (the pre-2.0.0 whole-cell colour). Console (ANSI) and html (a `tx-sec` CLASS, never an inline colour, so the stylesheet still decides); Markdown and Excel colour a cell as a whole, documented. Two snapshots moved, both reviewed: one new CSS rule, and the `class` added to a span that already existed.

**Two scale corrections, both statistical rather than cosmetic.** An **incidence-rate ratio is a ratio of means**, so poisson's IRR moved from `odds_ratio` to **`mean_ratio`** — the scale whose `unit` already said `"rate_ratio"`, whose breaks are the same 1.2/1.5/2/4, and whose level is a mean. A poisson IRR and a crosstab ratio of means now print identically (`÷2.5` / `×1.51`), which is the surviving notation of the descriptive rate column the merge deleted. And a **summed score** (`trials =`) gets ONE new `EST_SCALES` row, **`score_ratio`** — an odds ratio (or a risk ratio) of the per-item probability, sitting on the mean SCORE. It is the one place an estimate's geometry and its level live on different axes, so it cannot borrow `odds_ratio`'s row: `{base}` would fold a score into `pct` (×100, "%") and the column would claim `var_kind = "pct"` to every tooltip and plot. `REG_SCALE_GROUPED` is the one declared map that swaps it in, read by the model column and its crude twin alike so `reg_same_estimand()` cannot see a mismatch; the adjusted level is the predicted share × `trials`, which is exactly the quantity the crude column shows. ⚠ **`reg_color_auto_measure()` now returns the scale's own `label_meas`** rather than a coarse ratio/difference context — otherwise a `mean_ratio` column would be handed the odds-ratio measure and grade an empty `or` field.

**Colour, reference and legend.** The crude column takes the model column's measure, which is what collapses the three measured broken cases (`binomial × odds_ratio`, `binomial × ratio`, `poisson × ratio`) to one legend block — asserted on the block count. Under `color = "adjustment"` the crude column IS the baseline: `refcol = TRUE`, uncoloured by construction (no `obs` to score), bolded through one declared arm of `get_reference()`, and named in the legend as *"the observed effect (the reference for the adjustment)"*. ⚠ Scoped to `ref_kind == "observed"`: `between_groups` compares a cell to the same cell in another GROUP, where the crude column has a real counterpart and is graded like any other — so it now participates in that comparison, which is a deliberate behaviour change.

**Tooltips.** Every regression ratio column now has its interval and its p-value on hover — the largest measured gap, on the default column of every logistic table, and structural: the CI fold excluded `est_field == "or"` and the `ci:` line was level-scales-only, so **both** excluded them. One `est_ci`-driven fragment fills it, and the fold now fires only where the estimate really lives in `diff` (otherwise a ratio column's bracket would have been pasted onto its additive twin). The gap line renders through one shared helper (`fmt_gap_render()`), so a `points` gap reads `-23.0 pts` instead of `-0.01`.

**Four integration bugs found and fixed on the way**, each a real defect the merge surfaced: `tab_xl()` matched the raw display token, so a regression odds ratio would have exported as a bare number where the console prints `1/2.67`; `display_write_col()` refused to re-template a cell already carrying a composite, which would have made the taught post-hoc `set_display()` recipe a silent no-op on every regression table; the Excel numFmt of an `obs`/`gap` cell lost its decimals on empty cells; and `legend_reg_eff_word()` keyed on the scale NAME rather than on the declared glyph source.

**Accepted losses:** the crude stars now test the crude EFFECT (same 2×2 null, borderline cells differ); a gaussian crude column no longer prints `(σ1.83)` (its `var` is var(Y), the ladder's divisor); and a marginal reference row now prints its neutral where it printed nothing.

**Deferred as planned:** `jmvtools::prepare()` for the four-value `empirical` ComboBox (22g — until then the new values are inert in the app); the `n` column's tooltip line (22b-i); the vocabulary (22a-iii); the vignette PROSE (22h — only the sentences naming the deleted columns were repaired, in both languages).

##### Phase 22a-iii — the measure vocabulary

**D15–D21** — `REG_ESTIMANDS$word` rewritten to the measure + marker grid, the new `long` column feeding `reg_measures()` / the "what this outcome offers" abort / the footer note / `?tab_reg`, the measure moved into the `col_var` span, the `(adjusted %)` header suffix deleted, `@ref` verified through all four exporters and `make.names()`, and the acronym grid dropped into `?tab_reg` + both regression vignettes (the grid only). Last, because every regression column name moves here. Doc §1.3, §5.4.

**DONE.** Suite **FAIL 0 | WARN 1 | SKIP 4 | PASS 7533** (7402 before, +121 from the vocabulary tests added here; the WARN is the pre-existing over-dispersion note in `test-reg-estimand.R`). `devtools::document()` clean, **NAMESPACE unchanged**, `man/tab_reg.Rd` and `man/reg_measures.Rd` keep identical `\usage` / `\item{}` name sets.

**The header word is COMPOSED, not declared.** Two new fact tables in `R/reg-estimand.R` — `REG_WORDS` (seven acronyms: `OR` / `cumOR` / `RR` / `RD` / `IRR` / `RoM` / `diff`, each with its `long` expansion and a `noncollapsible` flag) and `REG_CONTRASTS` (one row per `effect`: no marker, the `m` prefix, the `ref` prefix, each with the phrase that wraps the expansion). `REG_ESTIMANDS$word` shrank to the BASE acronym and the rendered header is `marker ∘ log-wrap ∘ base` (`reg_word()`), so `binomial × marginal × difference` declares `RD` and prints `Model_mRD`, and `× coefficient × log` declares `OR` and prints `Model_log(OR)`. That is what ends the two collisions D15–D18 name: `Model_β` was five distinct quantities (the gaussian coefficient plus four `measure = "log"` paths) and `AME` / `MER` / `RD` were three names for one measure. Four foreign-key edges police it at load (`REG_ESTIMANDS$word` and `REG_EMPIRICAL$word` → `REG_WORDS`, `names(REG_CONTRASTS)` → `REG_EFFECTS_VALUES`, and `names(REG_WORDS)` → `REG_MEASURE_ALIASES` — every acronym a header prints can be typed back into `measure`, which is why `cumOR` joined the aliases).

**The log wrapper wraps the whole marked token** (`log(refOR)`, not `reflog(OR)`) while the expansion logs the measure and marks the contrast around it ("marginal log odds ratio"); each reads the way its own form is spoken. It also deleted the last `β` literal in the package: `reg_estimand()`'s `log_*` fallback used to overwrite `word` with the greek letter, and now keeps the row's base acronym and lets the wrapper do the work — so a pinned `measure = "log_risk"` on a binary outcome finally reads `Model_log(RR)`.

**Three decisions taken this session, refining D15–D21.**
- **The crude column is never marked** — `Obs_RR` beside `Model_mRR`. A crude column has TWO producers in one column: the saturated closed form (every factor level, where a marginal and a conditional contrast are the same number by construction) and a univariable refit (numeric predictors; every predictor under an ordinal outcome, where they can differ). One header cannot describe both, so it names the measure and the footer says the rest — and `Obs_*` names stay stable across `effect =`. The word is the crude SHAPE's own acronym, never the model's: a poisson AME sits beside a crude rate ratio, and `Obs_IRR | Model_mdiff` is what those two columns actually hold.
- **`at_reference` is a `ref` PREFIX**, not the doc's `@ref` suffix. `@` is safe in pandoc / HTML / Excel (verified), but `t$Model_RD@ref` parses in R as an S4 slot access and fails with an unrelated message. `Model_refRR` is a syntactic name: `t$Model_refRR` and `dplyr::select(t, Model_refRR)` both work bare, and `make.names()` leaves every column name untouched. Verified through all four exporters plus a real pandoc run (no citation is read out of the header).
- **The footer states the equivalence** — `Model: logistic regression; OR = odds ratio (vs the reference category).` This is D19 taken literally (one declared string, five consumers) and it answers Phase 22b's legend request in the same stroke. The `note` closures became the QUALIFIER clause only, and `reg_estimand_note()` composes `<word> = <long> (<qualifier>)`.

**One deviation from the doc's §5.4 grid, and the reason is the goal itself.** Poisson's marginal ratio is `mIRR` / `refIRR`, not `mRoM`: the crude companion of a count outcome is the family's own rate ratio, so `mRoM` would have printed `Obs_IRR` beside `Model_mRoM` — two acronyms for one quantity, which is exactly what D15 exists to end. A count outcome now has ONE ratio acronym.

**The legend names the measure, never the contrast**, and that is load-bearing rather than cosmetic: `legend_group_by_body()` groups columns by their rendered sentence, so a crude column reading `RR` beside a model column reading `mRR` would split the single legend block the 22a-ii merge exists to produce. Asserted on every path. `legend_reg_eff_word()`'s two hard-coded allow-lists (`c("RR","IRR","OR","RoM")` and `c("AME","MER","RoM","RD")`) collapsed into one `reg_legend_word()` call, its additive arm is now gated on `role == "model"` (an `n` column was borrowing the model's effect word), and `fmt_noncollapsible_caveat()` reads the declared `noncollapsible` flag instead of testing for `"OR"` or the greek letter. `.lg_beta` is gone. Two consequences to know: `reg_family_mult_word("ordinal")` is `"cumOR"` now (D17, and the ordinal legend says "cumulative"), and a gaussian coefficient column's legend reads `diff ≥ +0.2 SD` where it read `β ≥`.

**D20 is now a stated rule.** The per-category span (`"party3: OR"`, `"rincome: mRD"`) was a by-product of the spec label doing double duty; `reg_category_col_var()` states it once — the shared outcome span plus the measure — and `reg_cols_ame()`, `reg_cols_vsrest()` and `reg_columns_multinom()` all read it. Behaviour-preserving apart from the word. The `(adjusted %)` header suffix is deleted with `reg_eff_word()` itself (D10: `{base}` in the display is what names the adjusted prediction since 22a-i).

**Two defects found and fixed on the way.** `reg_measures()` built its grid over `measure = c("odds_ratio", "difference")` — the whole `ratio` row, a third of what the package offers, was missing from the user-callable lister; it reads `REG_MEASURES_VALUES` now and gained a `long` column. And `REG_EMPIRICAL`'s `nm` column was a hand-written full column name that could disagree with the acronym vocabulary; it is `word` now, FK-checked, with `reg_crude_col_name()` composing the `Obs_` prefix and the log wrapper from the shape's own scale.

**French kept working rather than deferred.** The rewrite moved ~23 msgids, so `po/R-fr.po` was re-merged (`tools::update_pkg_po()` + `msgmerge`) and the new vocabulary translated: the seven acronym expansions, the two contrast markers, the log wrapper and every qualifier. ⚠ **gettext has no gender facility**, and the marginal marker is an adjective composed onto a runtime string — so `"%s marginal"` would print *"différence de risque marginal"*. The fix is structural rather than a constraint on the vocabulary: the French marker is `"%s, effet marginal"`, whose adjective agrees with a CONSTANT noun, so an expansion of any gender composes correctly and `différence de risque` / `différence de moyennes` keep the standard French wording. One narrower constraint remains, with a translator note in the `.po`: `log du %s` needs a masculine consonant-initial expansion, which holds because only the five `rapport …` measures can be logged (an additive measure has no ratio to take the log of). The qualifier separator also became translatable (`"%s; %s"` → `"%s ; %s"`), since French spaces before a semicolon. The remaining ~104 untranslated msgids are pre-existing debt from earlier phases (the catalogue had never been re-merged), left for Phase 23f.

**Documentation.** `?tab_reg` gained a second GENERATED section (`reg_words_rd()`: acronym → meaning → which families print it, derived from `REG_WORDS` × `REG_ESTIMANDS` so the taught vocabulary cannot drift), and `@param effect` / `@param measure` name the markers once instead of listing `AME` / `MER` / `β` as headers. Both regression vignettes got the two grids rebuilt (D21): the outcome-kind × contrast grid — with `trials =` finally in it, the `Model_` prefix dropped and `†` marking the family default — and an acronym-with-meaning table plus a four-row marker table, every row ≤ 120 characters. The explanatory PROSE for the markers stays for Phase 22h; only the identifiers moved here.

**⚠ Still open for 22g**: `jmvtools::prepare()`. Nothing in the jamovi YAML carries an estimand word (the generated JS reads `reg_estimand()` at generation time and emits only the three-state grid), so the modules stay correct — but the JS should be regenerated with the rest of Phase 22.

**Two maintainer reviews of the full family × effect × measure matrix followed, and found twelve defects — six of them structural.** Suite after the fixes: **FAIL 0 | WARN 1 | SKIP 4 | PASS 7544**.

**(1) The crude shape was re-derived in four places instead of read.** `reg_empirical_columns()`'s arms picked a shape from `(marginal, do_exp)` while `reg_crude_shape()` read the estimand's declaration — so a gaussian marginal RATIO drew a mean-DIFFERENCE crude column beside a ratio model column (`Obs_diff` graded on a multiplicative ladder: the "±" the review caught), a poisson AME drew the LOGGED rate ratio (`Obs_log(IRR)`), and a summed-score risk ratio drew `Obs_OR`. `reg_crude_shape()` is now the only reader, `do_exp` inference is gone, and the two moment arms merged into one that dispatches on the declared SCALE — so `Obs_RoM | Model_mRoM`, `Obs_IRR | Model_mdiff`, `Obs_RR | Model_RR`. `grouped_binomial` gained the `rr` / `rr_log` shapes it lacked (a score's risk ratio is on the mean SCORE, so the estimand's `crude_fam = "rr"` borrow must land in that block — `reg_crude_shape()` enforces the precedence `reg_crude_key()` already stated).

**(2) D23 was only half-applied, which is what split fourteen legend blocks.** The rule "a crude closed form renders the interval its model twin renders" was written per engine (`katz`, and `quasipoisson` on an IRR), so every other pairing still showed two phrases for one arithmetic — Woolf's "Wald interval on the log odds-ratio" beside a plain "Wald interval" on a `log(OR)` column, "Student t interval, pooled…" beside "Wald interval" on a gaussian coefficient. It is one rule now, in `legend_method_name()`: on a **regression** column a `CI_METHOD_CLOSED_FORM` engine renders from the column's own scale (multiplicative → `wald_log`'s worded arm, otherwise the plain Wald one). `CI_METHOD_WORDED` shrank to `wald_log` alone — the model's engine, the only one that needs the effect word — and `katz` / `quasipoisson` / `ols` name themselves on a plain `tab()` column, where there is no model twin. `ols` joined the closed forms.

**(3) A summed score named its internal success code in the span.** `reg_shared_col_var()` appended the binomial "1" (`"tea_where: 1"`) while the crude block's span stayed `"tea_where"` — two spans for one comparison, hence three more split legends. A `trials` outcome has no level to name, which `reg_resolve_specs()` already stated; `reg_shared_col_var()` takes `trials` and says it too.

**(4) `legend_reg_adapter()` neutralised a word that now exists.** It dropped the model's additive word to match a crude column that had none — a premise 22a-iii falsified, so it was re-introducing the mismatch it was written to remove. Deleted; the crude column is named from ITS OWN shape (`legend_reg_eff_word`'s `role == "emp"` arm), which also repairs the `cumOR` regression this phase introduced into the ordinal marginal-ratio legend and keeps a poisson AME honestly on two blocks — a crude rate ratio and an additive AME are two estimands.

**Four smaller ones.** An ordinal crude column carried no level at all (`{base}` void), because the arm built an empty column and let the fit fill only the estimate — the observed share has a closed form and is in the grid, so the cell carries it now; a cumulative odds ratio still renders `{base}` void, by construction. A **risk ratio printed like an odds ratio**: the math was right (77 %/39 % = 2.00) but every RR sat on `odds_ratio`, whose glyph is `1/x` — so every RR estimand and its crude twin moved to **`pct_ratio`** (`÷ ×`, its own ladder, the level a %), the exact move 22a-ii made for the IRR, leaving `1/x` to mean *odds ratio* alone; `REG_SCALE_GROUPED` maps it to `score_ratio` for a summed score, and `reg_marginal_column()` stops hard-coding the scale and writes into the field the estimand declares. And a `$` in a level name (`"1-Lt $10000"`) broke the footer: pandoc reads `$…$` as inline math, and the legend's column-NAME token was not escaped — both the `$` and the names are escaped now, in md and html alike (a knitted page's raw html goes through pandoc too).

**(5) A poisson marginal effect had no observed counterpart, and should have.** It fell back to the family's coefficient shape, so an additive AME sat beside a crude rate RATIO (`Obs_log(IRR) | Model_refdiff`) that `reg_same_estimand()` then rightly refused to pair — an unpaired column on a second ladder. But a poisson marginal effect is a difference of expected COUNTS, whose crude counterpart is the observed difference of mean counts. Measured on `tvhours ~ race`: the closed form reproduces the univariable poisson AME to **2e-12**, and `welch` reproduces its ROBUST interval to **1.5e-03** (the model-based one is 49 % away — that gap IS the over-dispersion, and `tab_reg` dispersion-scales its poisson SEs, so the sandwich is the target in both bases; that is what makes it differ from gaussian's `ols`). `REG_EMPIRICAL$poisson` gained the `diff` shape it lacked and the two estimand rows point at it, so the pair reads `Obs_diff | Model_mdiff`, merges into one block, and **`color = "adjustment"` works on a poisson marginal effect for the first time**. `welch` and `robust` joined `CI_METHOD_CLOSED_FORM`: on a regression column the per-group forms ARE the sandwich the model reports.


**(6) A summed score's LEVEL, and then its UNIT.** A second review found the `trials =` path printing the per-item share where its own scale promised a mean score. Three causes: the `× trials` scaling lived only in the coefficient builder, so every marginal column showed the bare share; both marginal arms wrote the level into `pct` unconditionally instead of the field the scale names; and the crude arm dispatched on the scale's NAME (`"points"`) rather than its declared geometry. Fixed by one shared `reg_scale_pred()` and by writing every level into `EST_SCALES[[scale]]$base_display`.

That exposed a real fork — the difference measures come out of the fit as Δ(per-item probability) while the level beside them is a mean score, and `digits` is per-cell. **Decided: a summed score's additive effect is a difference of mean SCORES.** `E[score] = trials × p` makes the conversion exact (the interval scales by the same constant), so the column carries ONE unit throughout — "0.53 places fewer, out of 6, mean 2.27". It needs no new scale: `raw_diff` already declares exactly this shape (a raw difference whose level is a mean, on an SD ladder), so `REG_SCALE_GROUPED` maps `points → raw_diff` and the grouped block's `ame` shape declares it. Three consequences followed: the crude difference takes the MOMENT arm (the grid already holds `emp_mean` and `emp_var` in score units — measured against `var(tea_where)` by group), the moment arm's estimate is now `meanv - rmean` rather than the grid's `emp_diff` so that the estimate and its interval come from the same statistics, and `var(Y)` is stored for the ladder as it is for a gaussian AME. A grouped marginal reference cell carries the additive neutral, matching the gaussian arm rather than the probability-scale one.

Every family × effect × measure combination of the review renders **one** legend block. The `reg_same_estimand()` gate stays as the guard against a future fall-back writing one estimand into another's `obs`, asserted directly rather than through a table that no longer produces a mismatch.


**(7) A stale "no observed counterpart" note.** `reg_color_notes()` asked "does the marginal row reuse the coefficient row's crude shape?" as a proxy for "does this family declare a marginal crude twin?". Sharing that shape is the NORMAL case wherever the two contrasts are the same estimand — a linear model's AME IS its coefficient — so every gaussian marginal table under `color = "adjustment"` was told its crude effect "is a ratio" while both sides were a mean difference, and that the colour would stay empty while it was in fact fully populated (16 `obs` cells, 13 gap SEs). It now asks exactly what `reg_same_estimand()` asks at build time — does the declared crude shape's scale equal the one the column is stamped with — so the note and the gate cannot disagree. Verified over the whole grid: every REACHABLE marginal estimand pairs, so the note is now a dormant guard rather than a false positive.

⚠ **Reported, not fixed** (pre-existing, reproduced on an ordinary ungrouped binomial, so it is not from this phase): the Constant row of every risk-difference table renders `ref:NA***` — root-caused and owned by Phase 22b-ii (the rendering rule) and 22b-iii (what the Constant row should hold).


#### Phase 22b — `tab_reg()` manual review

Phase 22a merged the crude and the model column into one shape and gave both producers one display grammar. This phase is the review of what that left visible: the cell, the row axis, the column headers, the footer, and the argument surface. **Every claim below was reproduced against the running package at 22a-iii before being written down, and its root cause located** — a sub-phase inherits the diagnosis and plans its own fix, it does not re-derive it.

Read this framing, then your sub-phase. `dev/reg_crude_adjusted_and_display_integration.md` §5 still holds the crude/adjusted target design — read it before touching a crude column or a display token.

**Decisions taken with the maintainer (2026-08-19)** — respect them, do not silently widen them.

- **E1 — under `color = "adjustment"`, colour and stars keep meaning two different things, and that is taught rather than fixed.** The shade grades the adjustment, the stars grade the printed estimate against its own null, and the grey already says "the adjustment is not solid". No extra legend sentence: a star must mean the same thing in every tabxplor table, and removing it would cost the reader real information.
- **E2 — the stars sentence goes back to being readable.** Adding the Constant replaced *"significantly different from the reference category (in bold)"* with the cryptic *"significantly different from no effect (the reference category in bold; for the Constant, the null value)"* (`R/fmt_class.R:5390`). Restore the old clear wording for regression tables too, improve it now that the estimand metadata is available (the Constant's null is declared per scale — `0` or `1`), and append the Constant's case as a short aside, e.g. *"— from 0 for the Constant —"*. Concise; one sentence, not two.
- **E3 — `ratio` and `diff` are filled on BOTH the model and the crude column wherever they are meaningful.** The `fmt` record is dense: both fields already exist on every cell of every table and already hold an `NA`, so filling them costs no memory at all, only a few vectorised divisions at build. This keeps D11 (the display is post-hoc and never triggers a computation) true without exception, and turns the two requested presets into ordinary templates.
- **E4 — `{coeff}` is a DERIVED token, never a new `fmt` field.** β = log(OR) exactly, so nothing needs storing, and the record is not widened for every crosstab cell to serve a regression-only display. It derives like `{gap}` does. ⚠ It must render void on a MARGINAL column: `log(mRR)` is not the model's coefficient, and printing it as one would be a lie.
- **E5 — interactions get a first-class argument in 2.0.0, but the research and the design come first** (22b-vii), and a reasoned refusal stays open. How the term is KEYED is deliberately left to that research.
- **E6 — the per-predictor arguments are candidates for ONE merged spec, leaning towards a documented constructor function** (`terms = list(default = pred(...), age = pred(...))`) rather than raw nested lists. ⚠ Design task first, including the white-elephant assessment stated in 22b-vii, then the decision — with the fallback to *keep the arguments and merely make their grammar uniform* explicitly still open. A `center` knob belongs in it. jamovi is not a blocker: the existing reference-selection UI extends to one collapsed box per variable with several knobs inside, and not being able to override the package-wide default there is accepted. ⚠ **The SCOPE is itself open**: it may be a NUMERIC-predictor argument (and UI) only — `multiplier` / `shape` / `center` — leaving `ref`, which is factor-only and shares `tab()`'s grammar, as its own argument, and leaving interactions outside it entirely. The right level and design are still to be found.

**Still open, for the maintainer**: comparing a quadratic and a linear specification inside a predictors list needs `shape =` to become per-model (it is resolved once per table today). Recommendation: defer past 2.0.0 — the same comparison is one extra `tab_reg()` call away. ⚠ It is entangled with **E6**: if `shape` moves into the merged per-predictor spec, "per model" becomes a property of that spec rather than of an argument, so settle it in 22b-vii, not 22b-vi.

**Already closed by Phase 22a, and deliberately not repeated below** (do not re-open them): the multiplicative `1/x.xx` rendering on every path, the `obs` field included (22a-i, D12) · the crude column as the uncoloured, bold reference under `color = "adjustment"` (22a-ii, D4) · the acronym-to-meaning equivalence in the model footer, `OR = odds ratio (…)` (22a-iii) · `Model_β` becoming `Model_diff` / `Model_mdiff` (22a-iii, D18).

**Deferred to other phases**: one `jmvtools::prepare()` for every jamovi-visible change (22g) · the vignette PROSE for the new numeric-predictor unit, the centering rule and the interaction escape hatch (22h) · the message clean-up sweep (23c).

##### Phase 22b-i — the `n` column of `tab_reg()` and the Total column of `tab()`

⚠ First, because it moves columns and settles the tooltip contract Phase 22a-ii deferred here: once `n` is settled, the model column's tooltip mirrors whatever it becomes.

`tab_reg()` draws one `n` column per outcome, which is clutter as soon as there are two; the dormant feature that gives one column carrying everything should be finished instead. With a predictors list the current single column is already right (the populations coincide), and with spread `tab_vars` one column per level is right (it is what `tab_spread` means).

- With spread `tab_vars` the `n` columns sitting side by side is right and readable; moving the whole block to the RIGHT of the model columns would be better still.
- `tab()`'s Total cell wastes horizontal space: `100% (n= 9 838)` should read `100% (9 838)`, the `100%` still bold, and in EXPORTS ONLY the column header should say `Total (n=)` — for this case alone, not for every `"{pct} ({n})"` display, and respecting a custom total name. Same for Total rows, keeping the two-lines-in-one-cell printing where it exists.
- Keep the current display when the populations coincide and with `tab_spread`. With several outcomes, or several `col_var`s whose populations differ through NAs, or a predictors list with differing populations, the default becomes ONE column printing the range: `100% (6 712-9 838)`. The html tooltip of that column lists the `n` of every model / `col_var` (computed at display time, not stored); the cell tooltip prints `tot_n` after the cell's own `n`.
- It is a display-time fact now, so soft-deprecate `add_n` in `tab()` (inert, no error), drop it outright in `tab_reg()` (never released), and drive it from a package option: `"range"` (default) · `"n_min"` (minimum only, header `Total (n_min =)` in exports) · `"each"` (exports only — one column per `col_var` / model read from `tot_n`, same display token as the column in the data) · `"no"` (no `n` at all: no `n` in `tab()`'s total, no `n` column in `tab_reg()`). Keep display and export unaffected in speed.
- With `"each"`, exports must NOT repeat the `[married]` / `[tvhours]` tag on every `n` column, but fold each one into its own `col_var` block (see 22b-iv, which fixes the same repetition on the model columns).
- Remove the `N` footer row of `tab_reg()` altogether — the information is in the `n` column's Constant row. Root cause if something still gates on it: the value is `reg_glance()`'s first row (`R/tab_reg.R:1667`), kept by every family's default in `reg_footer_stats()` (`R/tab_reg.R:1808-1823`); read the `n` field of the Constant row instead, and make sure it is populated for every model.
- `tab()`'s Total column is last and `tab_reg()`'s `n` column is first, deliberately: the `100%` follows the columns it sums, while `n` sits by the predictor levels because it does not depend on the outcome. Under `"each"` the `n` becomes part of each `col_var` block in `tab_reg()` too, at display time — which means storing each factor predictor level's `n` in the `n` field, so it can also feed the tooltip under `"range"` and `"n_min"`. ⚠ `reg_level_counts()` (`R/reg-empirical.R:91-115`) deliberately leaves a numeric predictor's row `NA` — keep that, and see 22b-v, which wants that empty cell for the sparkline.
- In `tab()` with `levels = "first"` (or `levels = "auto"` where every factor has ≤2 levels) the `100%` is misleading because the row does not sum to it: print `(9 828)` / `(6 712-9 838)` alone. With `levels = "first"` plus a `tab_vars` that is also a `spread_vars`, match `tab_reg()`'s spread behaviour: one `n` column per `spread_vars` level.

**DONE.** Suite **FAIL 0 | WARN 1 | SKIP 4 | PASS 7547** (7544 before; the WARN is the pre-existing over-dispersion note in `test-reg-estimand.R`). `devtools::document()` clean, **NAMESPACE unchanged**, six man pages rewritten; the 36 `_golden/*.rds` and the `golden` / `render-html` snapshots regenerated and reviewed.

**One display-time fact, in both producers.** Nothing new is stored anywhere: a crosstab cell already carries its block's base in `tot_n` (a mean's in `n`), and a regression's model columns now carry each level's own `n` — one `set_n()` in `reg_spec_build_one()`, since every column of a fit rests on the same complete cases. `fmt_cell_base()` reads whichever applies, `tab_base_range()` reduces it per row over a `col_group`, and the mode chooses between folding it into the Total cell, giving it a column, or dropping it. Because it is display-time, `n` can be changed on a built table by an option, and the golden verifier reports **no field and no attribute moved on all 1788 cells of all 36 cases** — the only delta is `meta$render_extras`.

**`n = c("range", "min", "no")`, an argument with an option twin** (`options(tabxplor.n =)`), on `tab()` / `tab_counts()` / `tab_reg()`, replacing `add_n` in all three signatures — length-neutral. `add_n` rides `...`, soft-deprecated, and `add_n = FALSE` maps to `n = "no"`; it could NOT go fully inert, since it is honoured today by the fold, by the Excel column and by the OR total-column drop. Dropped outright from `tab_reg()` (never released). ⚠ Two hazards the one-letter name creates, both fixed and commented at the site: `$` **partial-matches on a list**, so `opts$n` in the jamovi caches returned `opts$na` (`[["n"]]` now); and **YAML 1.1 reads a bare `n` as the boolean FALSE**, so `name: "n"` is quoted in all four `.a`/`.u.yaml`.

**A range needs no literal and no second field.** `R/tab-display.R:598` had recorded why a `[min;max]` string was impossible — `format()` pads per unique template, so a per-row literal defeats the alignment. The answer is a TOKEN: one new `DISPLAY_TOKENS` row, `n_range`, reading `n`..`tot_n` and rendering one number when they agree, `6 712-9 838` when they do not. It is **dumb** — `"range"` vs `"min"` is one branch in the materializer (write the max, or leave `tot_n` NA), and `format()` reads no option. Excel keeps a real editable count and only turns TEXT on a genuine range. `set_tot_n()` is new, `@keywords internal`, with exactly one caller and the write-once comment amended to say so.

**The wrong-base bug, fixed by construction.** With several `col_var`s whose populations differ through NAs only ONE Total column survived the merge, silently carrying the LAST block's base (measured: `married` 13 015 and `party3` 11 044, one column printing 11 044 over both). The base is now reduced across every block of the group, so that column prints `11 044-13 015` and the html tooltip names which block is which (`married: 13 015 ; tvhours: 6 828`). A numeric `col_var` counts in the range through its own `n`, which is what makes a mixed factor/mean table honest for the first time.

**The misleading `100%` is settled by ONE stored-fact-free test** — *do the visible cells of the block add up to the Total?* (`tab_totcol_sums()`). It answers three questions at once: an odds-ratio table (cells are ratios), `levels = "first"` (the other levels are dropped after the tests), and any future display that stops summing. The build-time suppression at `R/tab.R:1075` is **deleted**: the column now survives and prints the base alone, which also fixes the two cases the old guard missed (a single `col_var`, and every `levels = "auto"` mix). `tab_is_or_display()`'s two callers collapse into `tab_drop_dishonest_totcol()`.

**`tab_reg()`: one `n` column, synthesised.** `reg_spec_build_one()` stage 4, the `n_col` product slot, `spec_plan$want_n` / `n_names`, the splice in `reg_stage_rows()`, `add_n` in `new_reg_shared` / `reg_validate_args` / `reg_resolve_args`, and `spread_relabel()`'s dead `role == "n"` skip are all **deleted**; `reg_base_n_cols()` builds the column from the model columns' `n` at display time. Two outcomes give one column printing `5 139-9 862` instead of two columns; with `tab_vars` there is one column per group and the block sits to the RIGHT of the models, as asked. ⚠ That single-value-column spread exposed a `tab_spread()` defect: `pivot_wider()` drops the value name when there is only one value column, so `Model_OR_White` had become a bare `White` — `names_glue` now restores the one naming rule for both cases.

**The `N` footer row is a RECORD, not a row.** `TEST_ROWS$render` gains a third value, `"record"` (rendered nowhere), and the `n` row takes it — so both renderers drop it from one declaration, while `get_test()` keeps it and `reg_plot_nobs()`'s hard "this data does not reproduce the fit" guard and the model-comparison equal-N check need no change at all. Reading the N back off the columns would have needed a column→model map that does not exist.

**Tooltips.** `fmt_display_shows()` maps `n_range` onto `n`, so a folded Total cell no longer repeats its own count; `.note` is a new optional fragment carrying what only the WHOLE table knows — the per-block breakdown behind a range. And every regression cell now has its level's base on hover, the Constant row the model N, which is the tooltip contract 22a-ii deferred here. `forest_plot(size = "n")` reads `e$n` directly and its fallback to the built column is deleted — what 22f asked for.

**Two maintainer decisions taken this session, and one deviation.** The `"each"` mode is **dropped entirely**: a user who wants a per-column count writes the token (`display = "{est} ({n})"`), and the Excel side of it became the study now in **Phase 22c-ii**. No header change either, console or export: `Total` stays a clean, backtick-free name, and naming a secondary display token is a general problem — every custom token is shown but named nowhere — which is the other half of 22c-ii. The deviation is the roadmap's html **header** tooltip: no header-tooltip machinery exists anywhere (`<th>` is emitted bare), so the breakdown went into the **cell** tooltip instead, which already exists, runs per row, and reaches kable as well as html.

**Accepted losses.** The base loses its `n=` cue in the cell (`100% (9 838)`) until 22c-ii names it. A transposed table gets no breakdown note (its tooltips are pre-built and flipped). And `stats = "n"` is now inert.

##### Phase 22b-ii — the composite cell: alignment, empty tokens and signs

Generic to `tab()`, not a regression fix — the reports came from `set_display()` on a regression table, but every rule below belongs to the display engine. Reproduced with `set_display(m, ...)` on `tab_reg(gss_simple, "married", c("race","rincome","relig","age"), family = "binomial", empirical = TRUE)`.

1. **The `+` sign is applied by two different rules + possible ref display integration ?**
- A pct difference prints `+` on everything that does not start with `-`, the neutral included (`+0%`); a gaussian mean difference prints no `+` at all, because `mean_diff` is simply not in the `diff_signed` mask (`R/fmt_class.R:2928,3110-3114`). Wanted: one rule for every additive estimand — `+` on a positive value, no sign on the neutral, so `+18.5%` / `0%` and `+3.33` / `0`. ⚠ While there: at `digits = 0` a tiny negative rounds to the literal string `"-0"`, which `print_num()`'s regex does not catch (`R/fmt_class.R:2758-2762`), so it escapes the re-signing and prints `-0%`. 
- **There is a design question left open: should we use the old `ref:` exception consistently across both `tab()` and `tab_reg()`** for all references (just putting the regressions row references `0` and `1` here gives the right display per measure and pct/mean ; here the displayed field, `diff`, `ratio` or `or`, already gives the measure, and the "base", `pct` or `mean`, gives the rest ; a special trim trailing zeros rule for references would transform 1.00 into 1 etc. ; `ref: 1` and `1` are both acceptable in regression though it would mean a different interpretation than in tab(), where `ref:` does not give the null but the "base" pct or mean), or **should we design specific regression rules ? What would be best for user-friendliness, and for simplification and visual integration of tabxplor framework ? Study this thoroughly and make me propositions.** Should be made consistent with "Every reference cell carries its measure's neutral" below, and designed together.

2. Small fixes and improvements
- **A missing non-primary token breaks the column's alignment.** Root cause: `display_write_col()` (`R/tab-display.R:48-74`) only writes the composite where EVERY field is non-NA, so a cell missing one keeps its bare primary display; `format()` then pads composite cells among themselves (`R/fmt_class.R:3319-3325`) and the bare ones separately, so the two never line up. Visible on `est_base` at a numeric predictor's row (`{base}` is NA), on `{est} ({diff})` at the Constant, and on every `measure` and `effect`. The maintainer's rule is the fix, stated once and generically: **trim all empty parenthesis, keep their celles padded and align if any other cell in the column still have content in it, and drop the padding entirely when they are all empty.**
- **`est_ci` colours and stars the whole string.** Root cause: `est_ci` is a single `DISPLAY_TOKENS` entry, so there is no primary sub-range for `paint_split()` to restrict to (`R/fmt_class.R:3340-3349`), and the `1/x.xx` form inside the bracket makes the estimate part a variable width, so estimates do not line up. The direction that removes the special case rather than adding one: make `est_ci` the ordinary composite `"{est} {ci}"`, exactly as 22a-i already did for `base_ci`; then the per-token padding aligns the estimate, and only `{est}` is coloured and starred. One declared fact in `DISPLAY_TOKENS`, so the rule holds for every composed CI display rather than for one preset.
- **A void field must render blank, never `NA`, and must not take stars.** `set_display("est_ci")` prints a bare `NA` on the crude column's Constant row, and every `measure = "difference"` table prints `ref:NA***` on its Constant row. Root cause: for a reference cell `format()` replaces the diff reading by the `base_display` one (`R/fmt_class.R:3184-3193`), which is unpopulated on the Constant, and stars are then appended to any non-empty string (`R/fmt_class.R:3280-3294`). What the Constant row should HOLD is 22b-iii's question; the rendering rule is this phase's, and it is generic.

- `{coeff}`, per decision E4: a derived token rendering the estimate on its own link scale — `log(est)` where the column's scale is multiplicative, the estimate itself where it is additive — void on a marginal column, and named in the tooltip as a coefficient rather than by the field's own word. And a `"est_coeff"` display preset printing `"{est} ({coeff})"`.

**DONE.** Suite **FAIL 0 | WARN 1 | SKIP 4 | PASS 7564** (7547 before; the WARN is the pre-existing over-dispersion note in `test-reg-estimand.R`). `devtools::document()` clean, **NAMESPACE unchanged**, four man pages with prose-only edits. The 36 `_golden/*.rds` did **not** move at all. Two snapshots did, both reviewed as aggregates: `_snaps/render-html.md` by exactly 3 tooltip cells (`diff: -0%` -> `diff: +0%`), and `_snaps/golden.md` by exactly 64 CSS declarations -- 32 `#000000` -> `#333333` (the `.tx-sec` rule, below) and 32 `#111111` -> `#333333` (⚠ **the maintainer's own uncommitted `R/tab-css.R` edit**, `grey2`, which the same stylesheet carries; accepted with it, and worth a glance).

**One predicate replaces four ad-hoc NA guards.** `fmt_rendered()` is now the single test of "this cell rendered something", and the rule it serves is stated once in `format()`'s header: **format() never pastes a string it did not render.** Every annotation reads it -- `ref:`, `ref:±-`, `mean:` (which also retires a `stri_replace("mean:Inf%|NA","")` hack), the reference `%` beside an odds ratio, and `disp_or_pct`, which was **unguarded** and would have printed `" (NA)"`. The stars mask reads it too, so a padded-blank cell takes no star. ⚠ It trims the UNICODE whitespace class rather than `trimws()`: the html/Excel pad glyph is a figure space (U+2007), which `trimws()` leaves standing. Also fixed on the way: a negative `var` (a design-based estimate can be) rendered `(σNaN)`; such a cell now takes the existing padding branch.

**`ref:` is a CROSSTAB annotation, and the gate is `role`, not the scale.** A crosstab reference cell has no other way to say what it sits at, so it keeps showing its own percentage in place of a difference that is 0 by construction (`ref:47%`, byte-identical). A regression column states its level through `{base}` -- which its default display already prints -- so there the reference cell shows its measure's **neutral**, and `ref:49% (49%)` never happens. ⚠ The gate cannot be `EST_SCALES$kind`: measured, `tab(ci = "ref")` stamps the very same `points` scale a regression risk-difference column carries, so a `kind == "effect"` gate would have silently deleted `ref:` from `tab()`. `role` (`""` on crosstabs) is the declared fact that separates them. This is what kills the reported `ref:NA***` on every `measure = "difference"` Constant row: that cell now prints its own value, `+39.1%***`, the baseline risk.

**One signing rule, and one baseline rule beside it.** The signed mask is now the declared list of ADDITIVE estimands -- `diff`, `coef`, `resid`, and an `obs`/`gap` on an additive scale -- so a gaussian coefficient is signed (`+1.31`) where it never was, its scale's `est_display` being `coef` and `coef` having been absent from the mask. `print_num()` collapses `-0` at **any** digit count (its old pattern missed `digits = 0`, where `sprintf` yields a bare `"-0"`), which is what made the three `diff: -0%` tooltips move to `+0%`. Per the maintainer's decision the rounded-to-zero `+0%` **stays**, for visual consistency with the ratio scale's `×1.00`; what drops the sign is being the BASELINE, which is the exact additive twin of the multiplicative bare-`"1"` rule and now shares its mask. That mask, `ref_base()`, is `all_totals` on a crosstab and `in_refrow` on a regression column -- `get_reference()` returns nothing at all on a `raw_diff` / `log_coef` column, whose `pct_type` is `"none"`, which is why a gaussian coefficient needed its own hard-coded `ref0 <- get_diff(x) == 0` block before. That block is **deleted**: the shared rule covers it, and covers it OUTSIDE `special_formatting`, so a composite no longer prints `+0 (2.95)` where the bare token prints `0`. One 22b-iii bullet landed here with it -- `reg_marginal_column()`'s `"prob"` arm writes the additive neutral on every scale, deleting the `scale == "raw_diff"` test -- because without it a marginal risk difference would have left the cell empty where its coefficient twin prints `0%`.

**An empty aside keeps its width; an aside empty everywhere is deleted.** `parse_display_template()` now walks the brackets character by character and returns a per-piece GROUP id, **splitting literal pieces at the top-level boundaries** (`" (n="` straddles the top level and group 1; `") ("` closes one group and opens the next -- without the split, dropping a spent group ate its neighbour's bracket). One shared rule, `display_template_keep()`, then decides which pieces survive, and it has **two** readers because there are two writers: `display_write_col()` rewrites the template per column, and `format()`'s expander prunes per cell -- ⚠ a raw `set_display(col, "{est} ({base})")` never reaches `display_write_col()`, and that path was printing literal empty parentheses (`-0.43 ()`). Per cell a spent bracket group renders as spaces of the same width, so the estimates stay in the same character column; per column a token void everywhere leaves the template with its group and its separator, collapsing to the bare token where one remains. `display_write_col()` now gates the stamping on the **primary token alone** -- an aside missing on SOME cells is exactly what the padding is for. ⚠ The primary's own group is never dropped: `tab_fold_base_n()` stamps `"({n_range})"`, whose only token is bracketed, and blanking it would blank the cell.

**A display is a property of the COLUMN, and `dplyr::across()` was answering per sub-table.** Both template writers now loop over columns. On a grouped tab `across()` runs per group, so "is this field empty in the whole column" was answered per sub-table: on a regression table the one-row `Constant` and `age` groups had their aside pruned away while `race` kept it, and the column did not line up. This is why the alignment defect survived a correct per-cell fix, and it is stated as a WARNING in both file headers.

**`{ci}` fixed on every scale, and `est_ci` deleted as a token.** Measured before: `{ci}` rendered `48% 2` on a crosstab percentage, `1/2.45*** 5` on an odds ratio and `1.31*** [111.38;150.38]%` on a gaussian coefficient -- right only on `points`. `fmt_ci_bracket()` is now THE interval renderer, on the column's own scale, keyed entirely on facts `EST_SCALES` declares: `is_pct` gives the ×100 and the `%` (replacing a `is_mean | ci_mult` guess), `mult` gives the measure's inverse glyph per bound (not reordered -- the glyph carries the direction), and NA bounds yield NA, never the centre value wearing brackets. `est_ci` is then the ordinary preset `"{est} {ci}"`: its `DISPLAY_TOKENS` row, its `get_num()` / `set_num()` arms (whose `switch(fmt_center_field(x), ...)` was the scale-token map spelled twice), its `mult_cells` membership, its `special_formatting` branch and its `tab_xl` `or_family` entry are all **gone**, and the tooltip no longer needs `special_formatting = TRUE`. The payoff is the reported defect: the stars and the colour now ride `{est}` alone (`1/2.45***` + a `tx-sec` bracket in html) and the per-token padding lines the estimates up. ⚠ `R/reg-resolve.R:435` compares the display **after** `reg_resolve_display()`, so it had to become `DISPLAY_PRESETS[["est_ci"]]` -- getting that wrong is invisible, costing only the jamovi live-reref fast path, which is why `test-jmvtabreg-cache.R` now asserts a cache HIT on `display = "est_ci"` and on a reference change under it (verified to FAIL with the stale literal restored). `R/tab-steps-legacy.R`'s build-time bare `ci` token gains a real bracket on mean and count columns.

**The CI decimals floor is a declared fact now** (`EST_SCALES$ci_digits`), replacing the hard-coded `ci_ratio -> 2` line in `format()` and `est_ci`'s `min_digits`. Values are today's -- 2 on the four multiplicative scales, 0 elsewhere, since 0 is the right default for a proportion bound -- and it applies to `{ci}`, `pct_ci`, `mean_ci` and `base_ci` alike, so one change reaches every interval the package prints. One value moved: `mean_diff` declares 1, because the cell itself already forced 1 decimal on a mean difference and the bracket printed 0, so `+5.2 [-7;17]` disagreed with itself.

**`value_cell` was under-set, and the est_ci merge exposed it.** It is documented as "a genuine value cell, as opposed to a p-value / blank / total-marker cell", but only six tokens carried it -- so `set_display(x, "est_ci")` silently no-opped on any column already showing `or`, `diff`, `ratio` or `pct_ci`, which is every crosstab built with `color = "OR"` or `ci = "cell"`. It is TRUE on every token that carries a value of the table now; the four that carry none (`pvalue`, `gof`, `n_range`, `blank`) keep their own token whatever `display =` asks for. That also closes the defect this phase would otherwise have had to report: `tab(ci = "cell", display = "ci")` used to be ignored while `set_display(x, "ci")` on the same table was obeyed.

**`{coef}` instead of E4's `{coeff}`, and no new attribute.** The package already had a non-user `coef` token (field `diff`), which `{est}` resolves to on gaussian and log columns; a second, near-homonymous `{coeff}` would have been two names for one idea. `coef` is user-typable now and scale-relative: the stored `diff` where the column is already additive (those columns are untouched), `log(estimate)` where it is multiplicative, since log(OR) IS the coefficient the model fitted -- derived, so the record is not widened, and settable, the write mirroring the read through `exp()`. One new preset, `est_coef = "{est} ({coef})"`. ⚠ E4 asked for it to render VOID on a marginal column; that needs a stored fact the column does not have (`role` is only model/emp/n), so per the maintainer's decision the token is documented as *the estimate on the model's LINK scale* -- true of a marginal ratio too -- rather than claiming to be a conditional coefficient. A 17th `fmt` attribute for a display-only concern was the alternative.

**The aside's colour left the option surface entirely** (maintainer's decision, this session). `tabxplor.color_secondary` is gone; what remains is **`tabxplor.color_whole_cell`**, a logical defaulting to `FALSE` -- one expert opt-out of the split, restoring the pre-2.0.0 whole-cell colour. **A colour is a PALETTE fact, not a per-cell option**: an aside now always takes the chrome's `grey2`, resolved per theme, and the only thing left to decide is whether the split happens at all. ⚠ For the record, since the request assumed otherwise: `grey2` is NOT unused elsewhere -- it is the colour of an uncoloured cell in a column with NO colour measure (`tx_chrome_hex()`, the `.g2` CSS rule, `tab-export-prep.R`'s `grey_this`, `tab_classes.R`'s `grey_color2`, `plots.R`). Sharing it is the point rather than a coincidence: both mean *present, but nothing is being said about it*, which is exactly what an aside is inside one cell -- so it now has two readers, and retuning it moves both. The reliable part is `color_secondary_hex(theme)`, one resolver read by every backend: the stylesheet emits three variants (`#333333` light, `#aeada5` dark, `#333333` print) exactly as every other chrome rule does, and the console reads the console's OWN detected theme rather than a colour baked for the other one. That is what fixes the reported dark export, which was painting asides in the near-white `text` colour. A greyed non-significant cell is unaffected: `pillar::style_subtle()` takes the WHOLE cell, aside included, which is right -- an aside must never be more prominent than a number the table has just greyed out.

**Cost measured, on the paths that grew** (`dev/benchmarks/results_2.0.0/phase22b-ii_composite_cell.txt`, 1e6 cells, against a `git archive` of HEAD): `format()` bare 9.14 -> 9.01 s, composite 26.49 -> 27.45 s (+3.6 %), `get_num()` composite 1.63 -> 1.70 s, a `tab()` build unchanged within noise. The only real cost is `fmt_rendered()`'s Unicode trim, one per token per cell; the bracket-group walk runs once per UNIQUE template per column and does not show up.

**Known limitations, documented rather than fixed.** `tab_md()` left-trims each cell, so a *leading* blanked group -- the `base_est` layout every crude regression column uses -- loses its pad in markdown; console and html align, markdown aligns for trailing asides. And the padding is by character count: exact in a monospace medium, within about one digit width in html/Excel, where a figure space is a digit wide but `(` and `)` are not. Same accepted trade-off as the existing `(σ sd)` tail.

**Deferred as planned:** the `base_est_mdiff` / `base_est_mratio` presets, which are role-dependent PAIRS and need `{diff}` / `{ratio}` filled on both columns -- 22b-iii, where the roadmap already conditions them; what the Constant row should HOLD (22b-iii -- this phase only makes its rendering honest, and deliberately does not let `format()` invent a policy: if that row should be blank it is `display = "blank"` stamped at build time, where the fact is known); and one `jmvtools::prepare()` for the new preset list (22g -- `est_ci` is still a valid preset NAME, so the modules stay correct meanwhile).

##### Phase 22b-iii — what a regression cell carries

Per decision E3, and it is the phase that makes `display` genuinely post-hoc on every regression column.

- **Fill `ratio` and `diff` on the model column and on the crude column wherever they are meaningful depending on `family` × `effect`.** Today `set_display("{est} ({ratio})")` prints nothing on either column of a default logistic table: a marginal ratio is not stored, and the crude column has neither. Both fall out of arithmetic already in hand — the model column's `ratio` is a per-cell identity on what it already stores, and the crude RR / RD both fall out of the 2×2 grid `prob_effect()` already computes.
- ~~**Every reference cell carries its measure's neutral.**~~ **DONE in 22b-ii**, with the signing rule it had to be designed against: `reg_marginal_column()`'s `"prob"` arm writes the additive neutral on every scale, and a reference cell prints the BARE neutral (`0` / `0%` / `1`) through one mask shared with the multiplicative rule.
- **The two role-dependent presets, deferred here from 22b-ii**: `base_est_mdiff` = `Obs: "({base}) {est}"` + `Model: "{est} ({diff})"`, and `base_est_mratio`, the same with `{ratio}`. ⚠ They are the first presets whose template depends on the column's `role`, so `DISPLAY_PRESETS` (a plain name -> template map today) needs a shape for that; `reg_default_display()` already mirrors `est_base` / `base_est` by role and is the precedent to follow.
- **Decide what the Constant row holds for each `(effect, measure)`.** Most of the time `effect = "coefficient"` already ship one Constant, but with exceptions (like ordinal). `family = "binomial"` and `measure = "ratio"` both give it a value; `effect = "marginal"` and `effect = "at_reference"` leave it empty, because the marginal sweep returns no `(Intercept)` row at all.
  + The question is statistical, not cosmetic: is there a sound quantity here — the adjusted prediction at the reference profile is the obvious candidate — and is it what other regression packages report? If `effect =  "marginal"` are calculated in average, in other packages and regression apps, what does it imply for the Constant ? Answer it, make web searches if need, then either populate it or leave it genuinely blank (22b-ii makes blank render as blank).
  + ⚠ Measured in 22b-v's centring study, and it widens the question beyond the marginal contrasts: even where the Constant row DOES print a value it is the raw intercept, so a numeric predictor sits at ZERO, not at its mean — on `married ~ race + rincome + relig + age` that is a newborn, OR `0.417` against `1.078` at the mean age. The row is labelled *Reference population* and `?tab_reg:3093` already promises "its mean", and `reg_reference_grid_values()` already builds that profile for `effect = "at_reference"`. So the same fix may serve every `(effect, measure)`: compute the row at the reference profile ? ⚠ That is a model PREDICTION at a grid, not an adjustment applied to a fitted coefficient — and 22b-v verifies it is the identical number, standard error and p that centring the data before the fit would have produced.

##### Phase 22b-iv — the table a reader scans: headers, footer order, reference and legend

Seven independent defects that share one context: what surrounds the numbers.

- **The footer rows are alphabetised, not ordered by the model.** `Overall association (LR): race, relig, rincome` for `predictors = c("race", "rincome", "relig", "age")`. Root cause: `reg_footer_plan()` sorts on the bare term name — `k[order(match(k$test, names(spec)), k$term), ]` (`R/tab-test-display.R:501`) — and both the console and the export read that one plan. The rows are BUILT in formula-term order (`R/tab_reg.R:2059-2079`); only the plan re-sorts them. Same defect with a predictors list.
- **With `empirical = TRUE` the whole footer attaches to the `Obs_` column.** Root cause: `reg_stage_specs()` derives `fit_first_col` from the flattened, already-spliced `cols` (`R/tab_reg.R:2774-2787`), and step 7b of `reg_spec_build_one()` (`R/reg-spec-build.R:202-214`) PREPENDS the crude column — so "the spec's first column" stops being the model column and every gof / global / check row is rekeyed onto the crude one. `color = "between_groups"` is incidental; `empirical = TRUE` alone reproduces it. The footer belongs on the model column.
- **The `[outcome]` tag is repeated on every export column header.** Root cause: it is baked into the `col_var` string itself by `reg_model_col_name()` when there are several outcomes (`R/tab_reg.R:507-510`), and the merged first header row already names the outcome. It is a console-only disambiguator (column names cannot repeat there), so exports should read the outcome out of the span and drop it from the level header — and it must fold the `n` columns in too, which is 22b-i's `"each"` requirement. ⚠ Verified NOT to affect `tab()`: a crosstab's `col_var` never carries the bracket.
- **`color = "between_groups"` marks no reference column.** The legend says *"White …: between groups: reference group"*, but no column is `refcol`, so the tabxplor rule that a reference column prints bold is not applied. Root cause: `as_refcol()` is called only for the crude baseline under `adjustment` (`R/reg-empirical.R:557`) and for a crosstab OR baseline, and `get_reference()`'s boosting arm fires only for `ref_kind == "observed"` (`R/fmt_class.R:5541`), while `between_groups` declares `ref_kind = "group"` (`R/fmt_class.R:4103`). Extend the declared reference framework rather than special-casing the exporter.
- **Remove the verbose `between_groups` message** (`R/reg-resolve.R:334-339`): it explains an internal cost, not a statistical caveat.
- **The stars legend sentence**, per decision E2 (`R/fmt_class.R:5390`).
- **The adjustment colours: a design limit, precisely located — not an arithmetic error.** `fmt_adjustment_score()` (`R/fmt_class.R:2552`) grades *amplified vs attenuated* (distance from the null), not *up vs down*. Measured on the reported cells: `race Other` crude `1/1.06` → model `1.05` scores `0.90` (read as attenuated, so orange) and `relig Jewish` crude `1.04` → model `0.89` scores `1.18` (read as amplified, so blue). Both are cases where the crude and the adjusted effect sit on OPPOSITE SIDES of the null — the adjustment reversed the effect's direction, which is the most important thing a reader could learn from the pair, and the amplify/attenuate framing cannot express it. `relig Buddhist/Hinduist` (`1/1.04` → `0.86`, both below the null) is graded correctly. Decide what the measure should say when the effect crosses the null: keep the framing and give a reversal its own maximal signal, or grade the signed move instead. Study this, and give examples to the maintainer so he can state what is the more readable solution. ⚠ Whatever is chosen, the gap SE and its interval are computed on the log-ratio (`fmt_gap_raw()`), so the score and its bounds must keep coming from one decomposition.

##### Phase 22b-v — the numeric predictor

Everything a continuous predictor's row does, end to end. ⚠ After 22b-i, which settles what the `n` column holds.

```r
tab_reg(gss_simple, outcome = "party3", predictors = c("race", "rincome", "relig", "age"),
        family = "multinomial")
tab_reg(gss_simple, outcome = "rincome", predictors = c("race", "marital", "relig", "age"),
        family = "ordinal")
```

- **Multinomial and ordinal print a per-1-unit effect**, which is unreadable next to a factor's contrast (`age` shows `1/1.01***`, and the row carries no unit label at all). Root cause: `need_mult` is gated on `reg_fam_glm()` (`R/reg-resolve.R:460-499`), a closed set that excludes both families, so `multiplier` comes back `NULL` — silently, since the informing message fires only when the user passes a non-default value. `reg_fit_multinom()` / `reg_fit_ordinal()` (`R/tab_reg.R:727`, `:774`) take no `multiplier` argument at all. The k-unit rescale is post-hoc arithmetic on the tidied coefficient (`estimate * k`, `se * |k|`, `R/tab_reg.R:1075-1086`), so it applies to their coefficients unchanged.
- **The unit label is verbose and wraps badly**: `age (per 1 SD (13.5))` repeats the variable name, which the variable column already carries, and word-wraps after `SD` when a longer label elsewhere (`Model fit`) would have left room on one line. Wanted: `per SD/13.5`, with the count printed only when it is not 1. Root cause of the wrap: the label goes through `tab_wrap_text()` like every other label (`R/tab-export-prep.R:304-312`); root cause of the text: `reg_multiplier_value()` + `reg_stage_rows()` (`R/tab_reg.R:272-284`, `:2835-2846`).
- **A one-row variable's name is not bold.** It is right that it stays horizontal, but it should match the vertical ones. Root cause: `vert <- ... & run$span > 1L` (`R/tab-render-html.R:352-361`) gives `tx-vname` only to a multi-row block, and bold is a row-level fact (`tx-b`) that a numeric predictor's row does not carry.
- **Move the linearity sparkline into the `n` cell**, which is empty for a numeric predictor by design, and give it a small frame in html (a thinner line than the sparkline itself) so an uninformed reader sees a plot rather than stray pixels. In html, make it just a bit wider (around ×1.5), a bit higher (around ×1.2), with a bigger linewidth. Root cause / material already in place: the glyph run is appended to the LEVEL cell (`R/tab_reg.R:2854-2864`) and html already swaps it for an inline `<svg>` (`tx_spark_svg()`, `R/tab-render-html.R:111-143`). ⚠ The glyphs are dropped entirely from the html level cell today — check what actually reaches each backend before moving it.

##### Phase 22b-vi — estimand, fit and the argument surface

- **`reg_spec_build_one()` tests the internal LINK key where it must test the outcome family.** `grouped <- sp_fam == "binomial" && !is.null(sp$trials) && !isTRUE(sp$compound)` (`R/reg-spec-build.R:149`) is exactly what the `⚠` above `reg_is_grouped_binomial()` (`R/tab_reg.R:100-104`) forbids: `rr` and `rd` are binomial fits under another link. Reproduced — `tab_reg(..., trials = k, measure = "ratio")` resolves to `fit = "rr"`, where the inline test answers FALSE and the declared predicate TRUE. The flag feeds `reg_gof_rows()` and `reg_check_rows()`, so a summed score on those two measures gets its Pearson-dispersion footer row and its dispersion check computed as an ungrouped binary logit. Every other caller uses the predicate.
- **The collapsible-link redundancy.** A marginal contrast equals the conditional one exactly when `measure` is the family's own coefficient default (`REG_ESTIMANDS[[fam]]$default[["coefficient"]]`, `R/reg-estimand.R:352-417`) — so `poisson × {marginal, at_reference} × ratio` runs a g-computation sweep and influence functions to return `exp(coef)` under a different name, which the header and legend then present as a distinct estimand. ⚠ A blanket refusal would be wrong twice over: `shape = poly(age, 2)` breaks the identity, and on gaussian the redundant cell IS the family's marginal default, so refusing it would make a bare `effect = "marginal"` an error on a numeric outcome — while the vignette teaches that identity as a pedagogical point. Recommendation: a message, not an abort, suppressed where `shape` makes the identity false. Whatever is decided moves one cell of the combination grid in `?tab_reg` and both regression vignettes.

  | combination                       | vs the coefficient | why                                                   |
  |-----------------------------------|--------------------|-------------------------------------------------------|
  | poisson × ratio *(its default)*   | **2.2e-16**        | log link: the averaging factorises out of `exp(b)`    |
  | gaussian × difference *(default)* | identical          | identity link                                         |
  | gaussian × ratio                  | 6.0e-03            | different FITS: an lm g-computed vs the log-link `mr` |
  | binomial × ratio                  | 1.8e-02            | logit g-computation vs the modified Poisson           |
  | binomial × difference             | 2.1e+00            | —                                                     |

- **Tidyselect in `tab_reg()`.** The two main user-facing functions must select variables the same way: `tab()` uses `rlang::enquo()` + `tidyselect::eval_select()` (`R/tab.R:286-312`), `tab_reg()` takes plain character (`R/reg-resolve.R:141-153`). ⚠ Both escape hatches must survive: the named list of character vectors (the model comparison) and the two-sided formula.
- **Expose the fitted formulas.** Table metadata plus an accessor, taught in the regression vignette and used by an expert to check what actually reached `glm()`. Root cause of the gap: for the ordinary path the assembled formula is never persisted — `reg_call(x)$fit_spec$specs` carries `outcome` / `predictors`, and `sp$formula` is non-NULL only for the compound escape hatch; the string is rebuilt on demand inside `reg_fit()` (`R/tab_reg.R:1019-1034`).
- **Move `parallel` into `...`** in `tab()` and `tab_reg()`, or drop it in favour of its option twin. It is a declared `TAB_ARGS` entry with an option (`R/tab-args.R:493-517`); both signatures already have a validated `...` (`tab_check_dots()`), so this is a small move that shortens two long signatures.
- ⚠ `multiplier`, `shape` and `ref` may be replaced by one merged spec in **22b-vii** (decision E6). Do not reshape them here — and the open per-model `shape =` question (see the framing) moves there with them.

##### Phase 22b-vii — the per-predictor argument surface, and interactions

Per decisions E5 and E6. The two are one phase because they constrain each other: a crossing term is a candidate member of the merged spec, and whether it fits IS the keying question. ⚠ **Both halves start with design research rather than code, and a reasoned refusal stays a legitimate outcome for either.** `tab_reg()` is unreleased, so the user API can change freely — no back-compatibility constrains this.

**Part A — one per-predictor spec instead of N named vectors.**

Three arguments key the same predictors separately today: `multiplier` (a scalar default **or** a named vector, numeric predictors), `shape` (named **only** — it aborts on a scalar, so the package's own "scalar = default for all" convention is not applied uniformly) and `ref` (named, factor predictors and `tab_vars`). The target is one argument grouping everything about one variable, with a `default` entry overriding the package-wide default and a named entry overriding that in turn. The leaning is a **documented constructor function** rather than raw nested lists — the tidyverse options-object pattern, which buys documented keys, name checking and autocomplete:

```r
tab_reg(..., terms = list(default = pred(per = "sd", center = TRUE),
                          age     = pred(per = 10, shape = "quadratic"),
                          tvhours = pred(center = FALSE)))
```

⚠ **The design comes first, and its first job is the white-elephant assessment**: knob by knob, which one creates such difficulty downstream — in the crude/empirical counterpart, in the adjustment and its gap SE, in the marginal sweep, in the stored formula, in the jamovi digest — that it should not be offered at all? Settle that per knob before settling the container. Then decide, with the fallback live: **keep the three arguments and merely make their grammar uniform** (scalar = default for all, named = per variable, applied to `shape` and to everything added later). That is the smaller change, and it is what gtsummary settled on facing the same problem across six per-variable settings.

What the design must answer, with evidence:

- **The four measured hazards of the raw-nested-list form**, each of which the constructor exists to remove: a named vector cannot carry mixed types (`c(per = "sd", center = TRUE)` silently coerces the logical to `"TRUE"`); a bare `list(per = "2sd", center = TRUE)` is ambiguous with the per-variable form the moment a column is named `center`, `shape` or `default`; there is no name checking or autocomplete on free-form keys; and `?tab_reg` would document one mini-language to a reader who struggles with nested lists.
- **Which knobs belong**: `per` / `multiplier`, `shape`, `ref`, `center`, and possibly the crossing term of Part B. What must NOT move in: anything that is not per-predictor (`na`, `stats`, `display`, `conf_level`).
- **Settle the SCOPE before the syntax — it may be narrower than the sketch.** `multiplier`, `shape` and `center` are all NUMERIC-predictor knobs, while `ref` is factor-only and deliberately shares `tab()`'s `c(var = "level")` grammar. A numerics-only spec is the cleaner object: one domain, every knob valid for every variable it keys, no type-dependent validation, and a name that says so — with `ref` staying put and staying consistent with `tab()`. ⚠ But that also shrinks the win to merging two existing arguments plus one new one, which the assessment must weigh honestly against the fallback; and it changes the jamovi picture, since a numerics-only spec is a SIBLING of the reference-selection box rather than an extension of it, so E6's UI sketch needs re-reading. **The right level and design for this are still to be found.**
- **`center` is the new knob and the sharpest test of the assessment.** 22b-v establishes that centring is a no-op on every displayed coefficient, that its one real effect is the Constant row (which 22b-iii fixes by prediction instead), that the right centre is a per-variable fact — a count's meaningful anchor is 0, not its mean — which is exactly why it belongs in a per-variable spec if anywhere rather than in a scalar option, and that transforming the fit puts the crude column, computed on RAW data, into a different unit system. So `center` must either un-centre every displayed level consistently, on both sides of the crude/adjusted pair, or be judged not worthwhile. Read 22b-v first.
- **jamovi is solved and is not a blocker** (maintainer's decision): extend the existing reference-selection UI into one collapsed box per variable, click to open, several knobs inside. Overriding the package-wide default is not offered there, which is accepted — it stays close enough to the R constructor to be taught as one idea. ⚠ It does need the 22g `jmvtools::prepare()`.
- **What `TAB_ARGS` and `?tab_reg` then declare**: one argument plus a fact table of knobs, so "one fact, one table" survives instead of dissolving into a parser. A knob must stay declarable — its values, its default, its doc — exactly as an argument is today.

**Part B — interactions.**

Per decision E5: a first-class way to put an interaction in a model, designed properly rather than bolted on.

⚠ **This needs real research and a well-thought design before any code, and a reasoned refusal stays a legitimate outcome.** It is the place in Phase 22 most exposed to producing a white elephant, so the plan must settle, with evidence, at least: what the user types and how it composes with `predictors` / a predictors list / `shape =` · WHICH cases are worth handling and which are not (two factors? two numerics? a numeric crossed with a factor? a factor crossed with `tab_vars`, which already has its own interaction test?) · how an interaction term renders on the row axis, which today has one row per level of one variable · whether it has a crude counterpart at all, and what `empirical = TRUE` should then draw · what a marginal sweep means over an interacted predictor, and whether the g-computation path and `marginaleffects` agree there · what the header word and the legend say · **at what centre a numeric predictor enters an interaction**, since a lower-order term is only interpretable relative to a stated one (measured in 22b-v: the `race` coefficient of `y ~ race * age` moves 22 % on the log-odds between the raw and the centred parametrisation, while the interaction term does not move at all).

⚠ Measured starting point: interactions ALREADY FIT today through the formula escape hatch — `outcome = y ~ a * b` is detected by `reg_parse_formula()` (`R/tab_reg.R:70-91`), which sets `formula_mode` on any term of order > 1 and passes the formula verbatim to the fitter. So the missing pieces are the discoverable argument and the rendering, not the model. A separate internal `cross =` mechanism already builds the pooled `predictors * tab_vars` fit behind `color = "between_groups"` (`R/tab_reg.R:1966-1993`) — read it before inventing a second one. If the design lands on "not worthwhile", document the escape hatch in `?tab_reg` and the vignette (22h) and close the item.
- ⚠ **How the term is keyed is deliberately left to this research** (E5): inside Part A's per-variable spec, or as its own pair-shaped argument. An interaction is symmetric and belongs to a PAIR, so a per-variable keying needs a stated rule for which side owns the term and validation against the mirrored duplicate — which is one of the inputs to Part A's container decision, not a detail after it. ⚠ It may also belong entirely OUTSIDE that spec: an interaction can involve factors, so it does not fit a numerics-only container at all — which is a further reason the two scopes have to be settled together.

**Part C - numeric predictors centering ?**

**Should numeric predictors be CENTERED by default? This is airst draft of the research, .** The four questions asked — good default? sound for counts as well as continuous? global option or vectorised argument? numeric outcomes? — answered from the code, a measurement and the literature.
- **Centring changes nothing tabxplor displays, except the intercept.** It is an exact reparameterisation of the linear predictor, so it holds for every family and every link, not just for OLS. Measured on `married ~ race + rincome + relig + age` (logistic, `gss_cat`): every non-intercept coefficient and every standard error is identical to machine precision — max |Δcoef| `3.3e-16`, max |Δse| `2.8e-17`. As a default data transformation it would therefore buy nothing at all, and "robust or not" does not arise: it is arithmetic, not an estimator choice.
- **The one place it bites is the Constant row, and there it bites hard.** Same fit: the raw intercept is OR `0.417` (p = 29.4 %), the centred one OR `1.078` (p = 51.9 %) — because the raw intercept describes a White, lowest-income, Protestant person **aged zero**. `?tab_reg` already promises the opposite ("the reference profile — every other predictor at its reference level **or its mean**", `R/tab_reg.R:3093`) and the row is labelled *Reference population*, so the documentation and the label are already written for a centred intercept while the number is not one. ⚠ The reference-PROFILE machinery is already correct: `reg_reference_grid_values()` puts a numeric predictor at `mean(x)`, so `effect = "at_reference"` already evaluates at the mean. Only the intercept row escapes it.
- **Centring before the fit and predicting at the reference grid are the SAME NUMBER, so this is a plumbing choice, not a statistical one.** Two established traditions answer the maintainer's objection, and both are standard: reparameterise so the quantity falls out of the fit with its own standard error (Kraemer & Blasey 2004 propose exactly a *default* centring strategy; Schielzeth 2010 gives precisely this rationale — refit a slightly modified structure and read the standard error straight off the model), versus leave the fit alone and ask it for the quantity (`emmeans` / `marginaleffects` reference grids, which hold numeric covariates at their means — literally the same profile). **Measured, they agree exactly**: on the same logistic fit the centred intercept and the grid prediction are both `-0.1735210332`, SE `0.0515704587`. And the agreement is exact rather than approximate — on the link scale a prediction is a linear function of the coefficients, so `x'Vx` IS its variance and no delta-method approximation is involved. ⚠ They agree only under TREATMENT contrasts: leave an ordered factor on its polynomial contrasts and the two differ by `0.2485` here, because the intercept then sits at that factor's grand mean rather than at its first level. tabxplor uses treatment contrasts (verified — an ordered `rincome` predictor renders its first level at OR `1.0000`), so this is a guard-rail worth asserting, not a live bug.
- **Prediction wins on plumbing, for four reasons specific to this package.** (1) It already exists and is already used — `reg_reference_grid_values()` plus the `at_reference` path — so it is not a new mechanism. (2) It is the only UNIFORM answer: `MASS::polr` has no intercept at all (verified — only thresholds) and a multinomial has k−1 of them, so "read the intercept off the fit" has nothing to read on an ordinal outcome. (3) It is the only route that also fills the row under `effect = "marginal"`, where the intercept coefficient is a CONDITIONAL quantity that does not belong beside sample-averaged effects — and that empty row is the reported bug. (4) It leaves the crude column's unit system alone, which is the maintainer's own consistency requirement: the crude effect is computed on RAW data, so with a raw fit there is nothing to un-centre and nowhere that can forget to. Centring would also collide with `reg_shape_term()`, which already freezes its own centre in the formula for `shape = "quadratic"` (double-centring, and its term label must match the fit's by deparse), and re-fitting on transformed data does not return bit-identical estimates from a numerically optimised fitter (measured: `polr` slopes move by `1e-4`).
- ⚠ **The honest counterweight, so the decision stays reversible.** If a coefficient table ever wants the Constant row's standard error and p straight from the fit rather than from a prediction, centring IS the cheaper route and Schielzeth's argument applies directly. Since the two numbers coincide, that is a maintainability preference, not a correctness one.
- **It would NOT be sound for count predictors, and the distribution is not the reason.** The arithmetic always works; the meaning does not. The guidance converges on centring only where zero is absent from the data or meaningless, and on preferring a substantively meaningful anchor where one exists — and a count is precisely the case where zero is both meaningful and observed (`tvhours = 0`, `children = 0`). "At 0 hours of television" is a better baseline than "at 1.97 hours", which describes nobody and is not even an attainable value. Skewness is a red herring: the mean is a poor anchor for a count because it is not a real value of the variable, not because the distribution is non-gaussian. (What genuinely does need care on a count is the per-SD CONTRAST, which the package already applies — an SD of 2.07 television hours is a defensible step, an SD of 0.5 children is not; `?tab_reg` already warns about the latter.)
- **A global option would be the wrong shape; only a vectorised argument could be right.** The correct centre is a per-variable fact — `age` at its mean, `tvhours` at 0, a Likert item at its scale midpoint — so one scalar switch would be right for some predictors of a model and silently wrong for others. The package already has the right precedent twice over: `multiplier` takes a scalar **or** a named vector (`"sd"`, `c(age = 10)`) and `shape` is per variable. Any centre argument would have to take that same shape — which is one more reason not to add one merely to repair an intercept. ⚠ Per decision **E6** this is exactly why a `center` knob, if it is offered at all, belongs in 22b-vii's merged per-predictor spec and never in a package option.
- **Numeric OUTCOMES: no, and it would actively break the display.** Centring an outcome also shifts only the intercept, so it buys nothing on the coefficients — but tabxplor SHOWS the outcome's own level: `{base}` renders the crude mean and the adjusted prediction, and a gaussian table's crude column prints `43.34` / `40.22` for mean age. Centred, those become `+0.9` / `-2.2` — a difference wearing the label of a level — and the crude/adjusted comparison that the whole of Phase 22a exists to make readable would be reading a subtracted quantity against an unsubtracted one. Leave the outcome raw.
- **Where a centre IS load-bearing: interactions — and the package already does it for the one curved term it has.** With `y ~ race * age` the `race` coefficient is the race contrast **at age = 0**; measured on the same data it moves from `-1.0745` uncentred to `-0.8780` centred (OR `0.34` → `0.42`) while the interaction coefficient is identical to the last digit. A lower-order term is only interpretable relative to a stated centre, which makes this a design constraint on **22b-vii**, not a default for today. The precedent to follow is `reg_shape_term()` (`R/reg-assumptions.R:513-540`): it centres and scales the squared term of `shape = "quadratic"` with the centre frozen as a literal in the formula, and leaves the linear term raw. Centre a term where its own parametrisation requires it, name the centre in the formula, leave everything else alone.
- ⚠ **Do not generalise the VIF argument.** That comment records the pair's own VIF falling from 38.7 to 1.2, which is true but is a property of the PARAMETRISATION, not of the fit — the fitted values, the highest-order coefficient and its standard error are identical either way. Centring does not reduce a model's collinearity (the determinant of `X'X` is unchanged); it stops the diagnostic false-alarming on a curved term. Read it that way.
- **What to ship instead** (22h): one sentence saying a numeric predictor is neither rescaled nor centered — `multiplier` is a post-hoc k-unit contrast, `f(x+k) − f(x)` or the coefficient times k — and one saying where the Constant row sits once 22b-iii has settled it. "Standardised by default" invites exactly the opposite assumption, which is why the sentence is needed.
- References: Kraemer & Blasey 2004, *Int. J. Methods Psychiatr. Res.* 13(3) — the case FOR a default centring strategy · Schielzeth 2010, *Methods in Ecology and Evolution* 1 — reparameterise and read the standard error off the model · Echambadi & Hess 2007, *Marketing Science* 26(3) — centring leaves collinearity, precision and R² unchanged · Iacobucci et al. 2016, *Behavior Research Methods* — the reconciliation · Gelman 2008, *Statistics in Medicine* 27 — centre and divide by 2 SD to make a continuous input comparable to a binary one, worth weighing against tabxplor's 1 SD default if the per-SD step is ever revisited · Frontiers in Psychology 2025, `10.3389/fpsyg.2025.1634152` — centring is unnecessary and raises the risk of reading a lower-order term as a main effect.


#### Phase 22c — tab manual review

 is the `tot = c("row", "col")` argument still needed at all, if the totals are always printed ? If it’s only soft-deprecation, would it be possible to put it in `...` with the other soft-deprecated arguments ? Also, document in roxygen and link where to find the soft-deprecated arguments documentation.




 Rewrite the short versions of legends ?

##### Phase 22c-i — tab_spread reworking

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

##### Phase 22c-ii — naming the secondary display tokens

A composite cell shows a secondary token in brackets — `100% (9 838)`, `1/1.63*** (31%)`, `{est} ({obs})` — and **nothing anywhere says what it is**. The primary token is named by the column header and by the legend; the aside is named nowhere, in any backend. 22b-i made this visible by dropping the `n=` cue from the Total cell (to save console width, and because a `Total (n=)` header would force backtick syntax in programming), and deliberately left the general problem here rather than adding a one-off header suffix.

Decide ONE rule and apply it to every secondary token, not just to the base count. Two directions to weigh:

1. **Name it where the column is named** — a short console legend line, and in exports a column name that mimics the cell's own bracket form (`Total (n)`), **when the secondary display is the same across (nearly) all rows of the column**. Cheap, and it puts the name where the eye already is.
2. **Name it in the legend, always** — short and long. This means one short legend block per group of columns sharing the same secondary field, which is a real addition to `legend_group_by_body()`'s grouping.

⚠ **And the Excel study that belongs with it**: Excel is the one backend that cannot show a composite cell as a composite, so it currently prints only the primary and the aside is LOST (`tab_xl()` writes the raw value plus a numFmt). Study giving every secondary display token its own **column** in Excel — as the base count already gets one — when the column carries the same secondary field on every row (or nearly). That is the same "is this token uniform down the column?" test as direction 1, which is why the two are one sub-phase.

Read `R/tab-display.R`'s header (the display grammar) and `R/fmt_class.R`'s `parse_display_template()` / `format()` composite expander before planning: the primary-token rule, the bracket-group model and the per-template padding are what any naming rule has to respect.

#### Phase 22d — Black and white publication print manual review
The grey fill carries no direction, and cannot. o1..o4 and u1..u4 are the same four greys; direction is only readable from the cell's own bold/italic. That's forced by the ruling and by Bertin, and it only bites for a table coloured on the background channel alone. The legend names it ("Grey fill" both sides).
The legend now collapses repeated break-words — print shows +5 and +20, not +5 +10 +20 +30, because slots 1–2 render identically. That is honest, but it does mean the print legend lists fewer thresholds than the colour one.

#### Phase 22e — assumptions plots manual review

#### Phase 22f — `forest_plot` manual review
D6 has a limit I could not design away. ggplot has one scale per aesthetic, so a key list describes one ladder. legend_guide_spec() returns NULL when the plotted columns form several legend_group_by_body() groups and the caption prints the prose legend instead — the same grouping the footer uses, so they can't disagree about how many ladders exist.
theme = "print" forced the one deviation from the table palettes — its text slots are all black (the table separates directions by bold vs italic, which a point can't be), so a mark borrows the print palette's grey ramp. Nothing is lost: in a forest plot direction is the position relative to the null line.
`or_plot()` was deleted with its inert `point_size`. Reimplement `point_size` in `forest_plot`, since the model columns now store the factor predictors levels `n` in `n` field.

#### Phase 22g — Jamovi UIs manual reviews and final modifications

⚠ **One `jmvtools::prepare()` for every jamovi-visible change of Phase 22**, batched here: the `display = "num_ci"` → `"base_ci"` rename and the new preset list (22a-i), `empirical`'s four values (22a-ii), the new estimand words (22a-iii), plus whatever Phase 22b adds to the option surface (**22b-i landed**: `add_n` Bool -> an `n` ComboBox with `range` / `min` / `no`, in all four YAMLs, on both modules -- until `prepare()` runs the module silently falls back to the option default; and the new display presets of 22b-ii, and any argument moved out of a signature in 22b-vi). Until it runs, a YAML option that the stale `.h.R` does not carry is INERT, not merely undocumented — see the "Jamovi module development" section above.

#### Phase 22h — documentation reviews

From Phase 22b: one clear sentence on what "standardised" means for a numeric predictor — it is a post-hoc k-unit contrast, the predictor is neither rescaled nor centered (22b-v) — and, if 22b-vii concludes that a first-class argument is not worthwhile, the formula escape hatch (`outcome = y ~ a * b`) documented in `?tab_reg` and the regression vignette instead.

From Phase 22a: the regression vignettes' PROSE for the new vocabulary (22a-iii ships only the acronym grid) — the crude/adjusted comparison read across the table, the `m` / `@ref` markers, the `{est}` / `{base}` display recipes, and `empirical = "column"` in the multinomial part (one sentence + one `eval = FALSE` chunk).

In the introduction vignette: teach levels = "first" and levels = "auto" with the FactoMineR:: tea dataset; also teach to use tab_vars + spread_vars to make a very condensed table of on the tea dataset.

**PARTLY DONE (2) — the assumptions behind a non-default `measure` are now written down.** Both regression vignettes gained `### With`effect = "coefficient"`, the measure chooses the model`, before the caveats list. The heading is scoped on purpose: only the COEFFICIENT row lets a measure change the fit — verified against the table, exactly three rows do (gaussian ratio -> `mr`, binomial ratio -> `rr`, binomial difference -> `rd`), while every `marginal` / `at_reference` row runs on the family's own model and differs only in the averaging step. The section holds: a table of every `(family, measure)` at `effect = "coefficient"` with what it fits and what the coefficient assumes, then what the literature says about the three that change the LINK (binomial ratio / difference, gaussian ratio) — and about the defaults, which are not assumption-free either. The recommendation is stated with its reasons rather than as a preference: keep the model on the family's own scale and get the reported measure through `effect = "marginal"`, because the logit always converges and cannot predict a probability outside 0–100 %, a marginal effect imposes no constant-effect assumption on the reported scale, and one fit answers every measure. With the counterweight: conditional and marginal are different ESTIMANDS, not two spellings, so the coefficient route is right when the conditional quantity is what is wanted. The three link-changing routes are graded rather than lumped — the modified Poisson is standard practice (its limits are small/sparse samples and unbounded risks), the identity link is the fragile one, PPML is consistent under a correct mean function only. Also corrected: the LPM fallback message claimed "same estimand, robust standard errors"; it targets the same risk difference but is a different ESTIMATOR, and now says so.

**PARTLY DONE — `levels` and the summed score now run on the `tea` data.** `gss_cat` has no real multiple-answer question, so every place that taught a *battery of binary items* was faking one out of unrelated variables (`married` + `black` + `income25k`, and a 0–2 "score" from `married` + `income25k`). Both moved to `FactoMineR::tea`, which carries two real six-item batteries: the intro uses *when do you drink tea?* (`breakfast` … `always`), the regression vignette *where do you drink tea?* (`home` … `pub`) — the "where" battery because it actually separates groups (men `1/1.44***` per place, `senior` `1.44**`), while "when" produced a table with nothing significant to read. `score_from_lv1()` moved with them: its section LEFT the programming vignette (where it was taught in the abstract, away from any use case) and is now three sentences inside the intro vignette's new `### Multiple-answer questions: one column per item` subsection, where the battery it sums is already on screen — the score joins the six item columns of the same `levels = "first"` table as a mean. The regression vignette just uses it now and points there. **`levels = "auto"` was undocumented**; it now has the example that shows what it actually decides — a battery of binary items beside a 3+ level factor, where it collapses the first and keeps the second (`tot = "row"` drops the Total column, which would read a misleading 100 % there until Phase 22b-i lands). `FactoMineR (>= 2.0)` added to Suggests; the shared prep chunk is inline and identical in all six files (a `tea_data_formatting()` twin of `gss_cat_data_formatting()` was considered and rejected — no new public API before CRAN).

Two details worth keeping. The prep puts the "yes" level FIRST (`fct_rev()` on the items whose level 1 starts with "Not"), because that is both what `levels = "first"` keeps and what `score_from_lv1()` counts — the one thing a reader must get right. `Sport` gets the same cleanup, since it is a predictor in the regression example. `?score_from_lv1`'s `@seealso` was repointed from the programming vignette to the intro one. ⚠ Still open from this phase: the `tab_vars` + `spread_vars` condensed table on `tea`, which waits on Phase 22b-i (the `n` / Total column) and 22c-ii (`tab_spread`).


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

**You should always start updating docs and writing the "DONE" summary while the final test suite runs**, since it’s now quite long (if a fix follows a failure, correct the affected docs once it passes).
- Keep everything **present-tense and concise**. Never clutter the docs with dev history (the ONLY place dev history is allowed is the "DONE" summary).
- Always respect the **documentation ecosystem** hierarchy (top of this file).
- Edit the files yourself (never hand the maintainer lines to paste).
- If you use a plan, do a real **documentation planning work** : define what goes where, avoid duplication, state what level of details and what focus the lines written in each document should have.

1. **File-header + inline comments** of every module you touched — make them state the CURRENT design, caveats and "why", never how it got there; add or adjust `# DESIGN:` / `# WARNING:` tags next to changed logic. *Cut, don't accrete.*
2. **Phase "DONE" summary** — under its own `#### Phase <x> — <title>` header in the roadmap. This is the ONE place dev-history detail belongs (what changed, why, measurements). CLAUDE.md is the ONLY place it goes; the maintainer moves finished phases to `dev/tabxplor_2.0.0_roadmap_DONE_PHASES.md` once the roadmap gets cluttered.
3. (**Repository Map** in this file — refresh a file's role line only if you added, removed or repurposed a file; keep it absolutely and utterly brief, *never* add clutter here, *cut, don’t accrete*; otherwise skip.)
4. (**`NEWS.md`** — user-facing / CRAN-facing only, new or changed functions/arguments, deprecations, important user-facing fixes; radically minimalistic, usually skipped.)
5. (**`README.Rmd`** — only before a CRAN release.)
