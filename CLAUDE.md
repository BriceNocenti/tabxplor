# tabxplor — AI Assistant Guide

## What tabxplor is, and why

`tabxplor` is a public CRAN R package (v2.0.0). it builds **colour-coded cross-tables and regression tables for data exploration** then a **publication-ready black and white version**. The one idea behind everything: colour or typography lets you *read a table at a glance*. Over-represented cells turn blue (or bold), under-represented ones red (or italics), deeper colour (or underlines) means a stronger deviation — and a cell is only coloured/typoed when the difference is statistically solid, so structure jumps out instead of being scanned number by number.

It sits at the intersection of three things most tools keep separate, and its architecture exists to unify them:

- a **display engine** — colour and typography that encode statistics (effect size *and* significance at once), the same visual language across console, HTML, Excel, Markdown/Quarto and plots;
- a **rich cell data-model** — every cell is a `vctrs` record carrying all the numbers behind the one it shows, so tables stay ordinary `dplyr`-manipulable tibbles and the display can switch losslessly;
- a **statistical-inference layer** — exact survey/design-effect variance (reproducing the `survey` package), named CI methods (Wilson, Newcombe, Katz, Woolf, Welch), Haberman adjusted residuals. **regression tables** get the same language plus the observed-vs-modelled comparison.

The target users are : 1. a "literary" social sciences student, not good at math, learning to read equiped crosstables and regression models using colors ; 2. a serious quantitative analyst — survey researcher, sociologist — often working with **weighted or complex-survey data**. That is why the inference layer is unusually deep for an exploration tool, and why `tab_reg()` pairs a model's adjusted effect with its **observed (crude) counterpart**, so "what did holding the other variables equal actually change" is visible in one table.

Two design principles underpin the whole package:

1. **Every cell carries all its statistical data.** A numeric cell is a `tabxplor_fmt` record (count, weighted count, percentage, difference, ratio, contribution, CI bounds, odds ratio, p-value, …). Changing what is displayed never recomputes or loses anything.
2. **Tables are tibbles.** Results inherit from `tibble` (`tabxplor_tab` / `tabxplor_grouped_tab`), so every `dplyr` verb works while table metadata and formatting survive.

**Performance:** aggregation runs on `data.table` internally; the user only ever sees tibbles of `fmt` columns.

**Dependencies are pay-as-you-go:** table building and core inference are always available (15 non-base Imports, including `data.table` and the stats engines `survey`/`nnet`/`MASS`); exporters, plotting, parallelism, jamovi and advanced regression backends are all Suggests, guarded at their entry points by `tx_need_pkg()`. The CRAN ceiling is 20 Imports and the headroom is deliberate; nothing is promoted from Suggests. See `dev/dependencies.md` for the inventory, the vendored code and its credits, and the costed options not taken.

---

## Repository Map

R files (`R/`) are grouped into seven subsystems. Every file carries a header comment with fuller design detail: read it for more details.

**Core type system** — the `fmt` record, the table classes, the row/table identity.

- `fmt_class.R` — the `tabxplor_fmt` vctrs record (the rich cell): fields, attributes, arithmetic, colour engine; the `MEASURES` / `EST_SCALES` fact tables.
- `tab_classes.R` — `tabxplor_tab`/`grouped_tab` S3 classes, dplyr methods, print, `tab_compact()`, the `test` footer; the palette/breaks API and `COLOR_SCALES`.
- `row-model.R` — the row axis: `row_kind` field + `tabxplor_lvl` factor subclass; `ROW_KINDS`; level operations.
- `table-spec.R` — the table identity `meta$spec` (kind / vars / call).
- `tab-structure.R` — `tab_structure()`/`tab_supports()`/`tab_columns()`: which reshape ops accept which table structure; `TAB_OPS`.
- `var-shape.R` — `shape`: the numeric-variable vocabulary, the cutters and their labels, shared by both producers; `VAR_SHAPES`, `shape_numeric_var()`.

**Crosstab API and pipeline** — building a table from microdata.

- `tab.R` — `tab()` and the `tab_build()` staged pipeline; `tab_prepare`, `tab_spread`, `tab_transpose`, the settings spine, `new_ctx()`.
- `tab-leaf.R` — the aggregate core: `tab_plain`/`tab_num`, `plain_core`/`num_core`, the leaves' CI/chi2, total rows.
- `tab-agg.R` — sufficient-statistic aggregation + the CI engine; `CI_METHODS` / `CI_GEOMS`.
- `tab-chi2.R` — the whole-table chi²/ANOVA test and the per-cell contribution writer.
- `tab-display.R` — the `{}` display grammar, its named layouts, the display-time base count, and the `?tabxplor-display` user page they fill; `DISPLAY_TOKENS` / `DISPLAY_PRESETS`.
- `tab-resolve.R` — the crosstab argument boundary (validation + the colour/settings cascade).
- `tab-counts.R` — `tab_counts()`, the from-aggregated-counts constructor.
- `tab-cross.R` — the `a*b` entries of `col_vars`: what each arm makes as COLUMNS, and where.
- `tab-parallel.R` — serial/parallel row-axis dispatch (mirai, Suggests-only).
- `tab-deprecate.R` — the 1.x → 2.0 translation shims + the superseded `tab_many()`.
- `tab-steps-legacy.R` — the superseded dplyr-era step API (`tab_pct`/`tab_ci`/`tab_chi2`/…), sharing arithmetic with the leaves.

**Arguments, options, integrity** — the surface as data.

- `aaa-grid.R` — how a declared table is WRITTEN (the grid rule) and `tx_grid()`, the fold every one of them uses; sorts first.
- `tab-args.R` — the argument surface: `TAB_ARGS` / `EXPORT_ARGS` drive signatures, value lists and `@param` prose.
- `tab-options.R` — the option subsystem: `TAB_OPTIONS` + the generated `?tabxplor-options` page.
- `zzz-fact-keys.R` — `TAB_FOREIGN_KEYS`: cross-table foreign-key checks run at load.
- `utils.R` — `.onLoad()` (seeds options), factor/list/string utilities (padding, wrapping, HTML escaping), deprecation and message helpers.
- `data.R` — the four example data sets and their source credits (built by `data-raw/DATASETS.R`).
- `tabxplor-package.R` — the `?tabxplor` landing topic (roxygen `_PACKAGE`; internal, so off the reference index).

**Regression** — `tab_reg()` and its model machinery.

- `tab_reg.R` — `tab_reg()`: fits per column, renders each estimand as cells, the staged `reg_build()`.
- `reg-resolve.R` — the `tab_reg()` argument boundary (`reg_resolve_args`, six stages + the tidy-select one).
- `reg-estimand.R` — the estimand cascade (family → link → measure → effect) and the library it composes; `REG_FAMILIES` / `REG_ESTIMANDS` / `REG_WORDS`; `reg_measures()`.
- `reg-digest.R` — `tabxplor_fitdigest`, the fit-free record of a fit; `REG_FIT_KINDS` / `REG_DIGEST_PARTS`.
- `reg-empirical.R` — the observed (crude) companion columns; `REG_EMPIRICAL` / `REG_EMP_BY_LINK`.
- `reg-influence.R` — the marginal engine (g-computation over `REG_LINK_FUNS`) + the gap-SE influence functions.
- `reg-assumptions.R` — model checks + `shape=` cures; `REG_CHECKS`; the plot primitives; the observed curves and their shape table.
- `reg-cross.R` — interactions, the shared surface for both producers: the peel, the validation, the autocut, `REG_CROSS_ARMS`.
- `reg-spec-build.R` — the per-model product builder (`reg_spec_build`).

**Survey** — design-based inference.

- `survey-design.R` — the design boundary/unwrap, constructors, robust omnibus tests, the inference basis.
- `survey-variance.R` — design-based cell variance → the `n_eff` field; the flat closed form.

**Exporters and rendering** — one visual language, every medium.

- `tab-export.R` — the `tab_export()` dispatch facade.
- `tab-export-prep.R` — the shared exporter prep + ephemeral render model.
- `tab-render-html.R` — `tab_html()` + the dependency-free HTML `<table>` engine.
- `tab-tooltip.R` — the hover tooltip: `TOOLTIP_LINES`, its shared gates, the one builder.
- `tab_md.R` — the Markdown exporter (pandoc colour spans).
- `tab_xl.R` — the Excel exporter (openxlsx2, colours/bold, numFmt from `format(syntax = "excel")`).
- `tab-xl-backend.R` — openxlsx2 wrappers + the range coalescer.
- `tab-css.R` — the one CSS generator (`tab_css`); light/dark/print themes.
- `tab-palettes.R` — every palette: the OKLCH colour ramps, the chrome, the publication grids (`PRINT_PALETTES`, `PRINT_READY`), the store.
- `tab-test-display.R` — the shared `test`-attribute renderer (console + export footers); `TEST_ROWS`.
- `tab-transpose-render.R` — the render-level transpose seam.
- `tab-theme-detect.R` — best-effort console light/dark detection.
- `plots.R` — `forest_plot()`, `reg_check_plots()`, and the `tab_estimates()` chart model.

**Jamovi** — the point-and-click modules.

- `jmvtab.b.R` / `jmvtab.h.R` — Crosstables backend (R6) + generated options.
- `jmvtab-cache.R` — the crosstab live-UI cache + the engine-free build core.
- `jmvtab-export.R` — jamovi export helpers + the shared `jmv_backend_*` R6 helpers.
- `jmvtabreg.b.R` / `jmvtabreg.h.R` — Regressions backend + generated options.
- `jmvtabreg-cache.R` — the regression fit-digest cache + `jmvtab_reg_build()`.

**Cross-cutting** (touch with care): `fmt_class.R` is the foundation of every column; `.onLoad()` in `utils.R` seeds every option; `format.tabxplor_fmt()` and `fmt_color_channels()` are the shared display/colour sources of truth across all backends.

**Other directories:** `vignettes/` (introduction, *Reading a regression*, regression, weights, programming; `vignettes/articles/` is pkgdown-only and holds the five French twins) · `tests/testthat/` (testthat v3, subsystem-named: the package's contract) · `man/` (roxygen-generated, never edit) · `data/` + `data-raw/` (the four example data sets and the script that builds them) · `inst/i18n/` + `po/` (translations) · `jamovi/` (module definition) · `dev/` (seven technical guides, the dev scripts and perf harness, `dev/tests/` — the second test suite — and `dev/archive_2.0.0/`, the 2.0.0 evidence base; all `.Rbuildignore`'d).

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

There are **two producers, one output type**. `tab()` builds crosstabs from microdata, `tab_reg()` regression tables from a model, and both emit a `tabxplor_tab` of `fmt` columns — so the colour engine, the accessors, the reshape operations and every exporter treat them identically, one visual language and one export path. `tab_counts()` starts "from the middle", building the same object from already-aggregated counts.

### The declarative architecture

The codebase is organised around **declared fact tables**: each fact — a colour measure, an option, an argument, an estimand, a display token, a kind of row — is stated **once, in one table** and read through named accessors, instead of being scattered across literals and `switch` statements. The single rule a future change must respect: *every fact is stated once, in one declared table; a key one table reads out of another is a foreign key, checked at load* — `zzz-fact-keys.R` validates every edge at namespace load, so a rename that breaks a reference fails the install, not a user's table.

The payoff to internalise: **adding a measure, an option, an argument, an estimand is one new row — not N scattered edits.** Do not re-introduce ad-hoc branches; extend the table.

**A declared table is written as a grid** (`R/aaa-grid.R`, which states the rule and sorts first because a grid is folded at source time): one row per fact, fields in one fixed order, aligned in columns, a column dictionary immediately above, and nothing about a row stated anywhere else — a row may run long, being read unwrapped. Where every field is a scalar the grid is a `tibble::tribble()`, folded by `tx_grid()` into the named list of named lists every accessor, roxygen generator and foreign-key check already reads (a `NULL` cell means "this row has no such field"); where a field is a closure or a paragraph of `doc` it stays a `list()` of one aligned block per row, the fixed field order being what makes it a grid. `TAB_ARGS` and `EXPORT_ARGS`, too ragged and too prose-heavy for a tribble, declare that order as `TAB_ARG_ORDER` and assert it at load. A tribble takes comment lines between rows, so a threshold's justification still sits on its own row.

The main fact tables:

| Fact table         | Home                 | Declares                                                                                |
|--------------------|----------------------|-----------------------------------------------------------------------------------------|
| `MEASURES`         | `fmt_class.R`        | The colour measures (raw field, scale keys, significance source, legend, requirements)  |
| `EST_SCALES`       | `fmt_class.R`        | What a column estimates (field, null, geometry, colour ladder, SD source, precision)    |
| `MEASURE_ACRONYMS` | `fmt_class.R`        | The discipline's acronyms: one spelling vocabulary for every argument naming a measure  |
| `DISPLAY_TOKENS`   | `tab-display.R`      | The `{}` display grammar (field source, geometry, aliases, placement)                   |
| `DISPLAY_PRESETS`  | `tab-display.R`      | The named cell layouts both producers resolve (`est` / `est_ci` / `est_base` / …)       |
| `CI_METHODS`       | `tab-agg.R`          | The confidence-interval methods and geometries (with `CI_GEOMS`)                        |
| `COLOR_SCALES`     | `tab_classes.R`      | The break scales and palettes                                                           |
| `COLOR_RAMPS`      | `tab-palettes.R`     | Every colour rung: channel x theme x direction x rung, its hex and its OKLCH coordinate |
| `PRINT_PALETTES`   | `tab-palettes.R`     | The black-and-white publication palettes: a row per break slot (ink, face, mark)        |
| `TAB_ARGS`         | `tab-args.R`         | The argument surface (signatures, values, option twins, prose; + `EXPORT_ARGS`)         |
| `TAB_OPTIONS`      | `tab-options.R`      | The package options and their defaults                                                  |
| `ROW_KINDS`        | `row-model.R`        | The row-kind vocabulary                                                                 |
| `TEST_ROWS`        | `tab-test-display.R` | The footer / statistical-row catalogue                                                  |
| `TOOLTIP_LINES`    | `tab-tooltip.R`      | The hover tooltip's lines: which token each renders, its label, its gates, their order  |
| `TAB_OPS`          | `tab-structure.R`    | Which reshape operations accept which table structure                                   |
| `VAR_SHAPES`       | `var-shape.R`        | How a numeric variable may enter a table or a model, and which producer may ask for it  |
| `REG_FAMILIES`     | `reg-estimand.R`     | Per family: the level kind, the links it fits, its names — the estimand library derives |
| `REG_ESTIMANDS`    | `reg-estimand.R`     | Composed from it: one row per buildable (link, effect, measure)                         |
| `REG_WORDS`        | `reg-estimand.R`     | The header acronyms' expansions (with `REG_CONTRASTS`, the contrast markers)            |
| `REG_FIT_KINDS`    | `reg-digest.R`       | One row per fitting backend: its classes, its influence engine, the parts it carries    |
| `REG_DIGEST_PARTS` | `reg-digest.R`       | One row per stored part of a fit digest, and the consumer that reads it                 |
| `REG_LINK_FUNS`    | `reg-influence.R`    | Per link: its transform and derivative — all a marginal contrast needs of one           |
| `REG_EMPIRICAL`    | `reg-empirical.R`    | The observed-companion column shapes per family                                         |
| `REG_CROSS_ARMS`   | `reg-cross.R`        | The two interaction shapes: a combined factor, or slopes nested in a moderator          |
| `REG_CHECKS`       | `reg-assumptions.R`  | The model-check / assumption catalogue                                                  |
| `TAB_FOREIGN_KEYS` | `zzz-fact-keys.R`    | The cross-table foreign-key edges, checked at load                                      |

Three supporting mechanisms carry the same spirit: **typed contexts** (`new_ctx()`, `new_reg_ctx()`) declare every value a pipeline threads, so a stage cannot read an undeclared field; **single argument boundaries** (`tab_resolve_common_args()`, `reg_resolve_args()`) normalise each producer's arguments in one place; and **one table identity**, `meta$spec` (`kind` / `vars` / `call`), read through `tab_kind()` / `tab_is_reg()`.

### The type system

#### tabxplor_fmt — the rich cell

`tabxplor_fmt` (`R/fmt_class.R`) is a `vctrs::new_rcrd()` record and the foundation of the package: every numeric column is an `fmt` vector, with **21 per-cell fields** and **16 per-column attributes**.

**Fields** (per-cell, via `vctrs::field()`):

| Field       | Type | Meaning                                                                             |
|-------------|------|-------------------------------------------------------------------------------------|
| `n`         | int  | Unweighted count                                                                    |
| `wn`        | dbl  | Weighted count                                                                      |
| `pct`       | dbl  | Percentage, stored 0–1 (×100 only in `format()`)                                    |
| `mean`      | dbl  | Cell mean (numeric column variables; `NA` on pct columns)                           |
| `tot_n`     | dbl  | The cell's own unweighted percentage base (row/col/grand total per `pct`)           |
| `diff`      | dbl  | Difference from the reference                                                       |
| `ratio`     | dbl  | Ratio to the reference (the "×2" comparison the colour engine reads)                |
| `or`        | dbl  | Odds ratio / relative-risk ratio                                                    |
| `obs`       | dbl  | The observed value a `tab_reg` estimate is compared to (`NA` elsewhere)             |
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
| `col_group`    | chr  | The sub-population a block belongs to (a spread level or `tab_vars` group; `""` otherwise)      |
| `ref`          | chr  | Reference type (`tot` / `first`)                                                                |
| `comp_all`     | lgl  | Compare against the total table (TRUE) or the subtable (FALSE)                                  |
| `totcol`       | lgl  | This column is a total column                                                                   |
| `refcol`       | lgl  | This column is a reference column                                                               |
| `color`        | chr  | Colour measure (length 1, or 2 for a text+background channel pair)                              |
| `color_signif` | chr  | Significance policy: `ignore` / `grey_non_signif` / `guaranteed_effect`                         |
| `model_family` | chr  | A regression column's own family (`""` on crosstabs)                                            |
| `role`         | chr  | A regression column's role: `model` / `emp` / `n` (`""` on crosstabs)                           |

**The critical distinction:** fields vary per cell, attributes over a whole column. The record is deliberately **dense** — every column carries all 21 fields, an inapplicable one stored as `NA` — so the colour engine and the tooltip builder read any field on any column and simply find `NA` where it does not apply.

The attribute list is **derived** from `new_fmt()`'s formals (attributes = formals that are not fields), and how each is carried through casts, arithmetic and binds is itself a declared table (`fmt_attr_rules`); adding one is a formal plus one rule row, and a build-time assertion refuses an attribute with no rule. Read or write any attribute by name with `fmt_attr()` (the programmatic surface); the `get_*`/`set_*` accessors are the taught one. Constructor chain: `fmt()` (public, validates) → `new_fmt()`.

**Adding a field** touches ~9 sites in `fmt_class.R` (the field list, `fmt()`, `new_fmt()`, the accessors, the four reconstructors) plus, for a *displayed* field, `get_num()`/`set_num()`, `format()`, `tab_xl` and a `DISPLAY_TOKENS` row — follow the `/vctrs-field` skill.

#### tabxplor_tab — the table

`tabxplor_tab` is a `tibble` subclass; `tabxplor_grouped_tab` extends `grouped_df` when `tab_vars` split the table into sub-tables. Class and metadata survive `dplyr` through ~30 S3 methods, anchored by the `dplyr_row_slice()` / `dplyr_col_modify()` / `dplyr_reconstruct()` trio (a missing method silently downgrades to a plain tibble). A table carries three **optional, NULL-safe** attributes: `subtext` (legend text), `test` (chi²/ANOVA/model-footer rows) and `meta` (`spec`, the variable model, CI settings, render intent, any regression records). Every getter tolerates absence — a stripped table still prints, dropping only what it powered — while cell fields and column attributes stay required, so an extracted `fmt` column formats and colours on its own.

#### The row model

Rows describe themselves the way columns do. The `row_kind` field (`ROW_KINDS`: `data`/`total`/`n`/`pct`/`pvalue`/`gof`/`blank`) says what kind of row a cell sits in, `is_totrow()` being the derived read; the index columns are a `tabxplor_lvl` factor subclass carrying each level's `role` and originating `var`, so variable detection and rendering read stored facts rather than guessing from labels.

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

**The settings spine** (`ctx$settings` = a `rows` / `cols` / `pairs` star schema) is where the row and column axes meet exactly once, so parallel argument vectors cannot recycle against each other. Each stage projects it into the bare names its resolution block reads, so a pre-resolution value cannot leak into a computation.

**How a number enters a table** is `shape` (`var-shape.R`), one vocabulary both producers read: cut it into quantile groups or into bands at the mean and one SD either side — it becomes an ordinary factor, and nothing downstream special-cases it — or transform it and keep it a number (`col_vars` only, and the column is renamed, because the mean of a logarithm is not the logarithm of a mean). It is **resolved** in `tab_setup()`, where the variable classification must already know what a column will be, and **applied once** in `tab_prepare_pop()`, after the filter and the NA policy and before any split, so the breaks describe the population tabulated and every sub-table is cut at the same places. On the row and tab axes a number always gets one — `"auto"` keeps one level per value for a counted number or a short scale and bands a continuous one — because one row per distinct value is a table nobody reads.

**An interaction is a variable, on both producers.** `col_vars = a*b` is spelt exactly as `tab_reg(predictors =)` spells it and reuses the whole surface (`reg-cross.R`: the peel before tidyselect, the validation, the autocut of two continuous parents, `REG_CROSS_ARMS`); `tab-cross.R` says only what each arm makes as COLUMNS — two factors give one column per observed **cell** of the pair, a number crossed with a factor one **mean column per level** beside the factor's own block. It is refused on every other axis, where it could not mean anything. The pipeline's own division of labour carries it: the arms are decided at the boundary, on what `shape` says a parent WILL be, and the columns are materialised in `tab_prepare_pop()` — the first point a cut moderator has levels — which **completes** the settings spine with them exactly as it already completes it with `lv1` and `na`. The typed key is the block's display identity (`col_var`), so the exporters need nothing.

**The aggregate core** (`tab-leaf.R` + `tab-agg.R`) is the single place microdata becomes cells: the leaves `plain_core()` (factors) and `num_core()` (numeric column variables) turn sufficient statistics into `fmt` fields, their confidence interval and the whole-table test in one pass. The superseded dplyr-era steps (`tab_pct` → `tab_ci` → `tab_chi2` → …) are quarantined in `tab-steps-legacy.R`: still exported, they share the *arithmetic* (`ci_dispatch()`, `chi2_compute_test()`) with the leaves, so a step and a build cannot compute two different answers.

**The reference system:** `ref` picks the baseline a deviation is measured from (`tot` / `first` / an index / a regex), reinterpreted by `pct` (a reference *row* under row%/means, a reference *column* under col%); `ref2` names the second level for odds ratios; `comp` compares within each sub-table or against the total table — and `comp = "all"` reads the total table's reference **of the row's own row_var**, the first one at or after it, since the total table closes each variable's run of sub-tables. **Significance:** a cell is significant when its confidence interval excludes the **neutral value** — 0 for a difference, 1 for a ratio — and the displayed p-value and stars come from inverting that same interval, so colour, greying and stars cannot disagree. Interval geometry is declared in `CI_GEOMS`, its method in `CI_METHODS`.

### The inference layer

**The survey-design boundary** (`survey-design.R`) is one unwrap point: a `survey` design passed as `data` becomes the microdata every engine already reads, plus its sampling weights and design metadata — so the observed columns, the marginal effects, the tests and the footer are all design-weighted, and a `svyrepdesign`/`twophase` is refused rather than approximated.

**The inference basis** is the layer's central idea: how the *estimate* is computed (`wt`) and how the *interval and test* are computed (the basis) are **orthogonal**. The basis is one of `n` / `weights` / `design` / `design_partial` and — with `conf_level`, `degf` and `ci_method` — is stored **on each column, not on the table**, because `dplyr` drops table attributes and a number must never depend on one. A bind reconciles them by the weakest-claim rule.

**Design-based cell variance** (`survey-variance.R`) feeds the existing `n_eff` field, so the ordinary CI machinery becomes design-aware with no new field. A plain weight column is a survey design at `ids = ~1`, where the general formula collapses to a per-cell closed form computed from the aggregate alone (Kish is its degenerate limit); a real design goes through `survey::svyrecvar`, which owns the variance algebra throughout. **`dev/inference.md`** derives that closed form and the chi2 cell residual, and states what a weights-only design effect can and cannot see.

### The display grammar

What a cell prints is a `{}` template over declared tokens (`DISPLAY_TOKENS`), resolved by one boundary `tab()`, `tab_reg()` and `set_display()` share, so a layout learnt on a crosstab means the same on a regression. `{est}` and `{base}` are **scale-relative** — the deviation a column estimates, and the level it sits on — which is what lets one named preset (`DISPLAY_PRESETS`) render an odds ratio, a mean difference and a percentage alike. A composite has a **primary** token, the first outside brackets: it carries the stars, it is what `get_num()` and Excel return, and it is the only part the colour paints — and its converse, a template with no token outside brackets, has no primary at all and renders whole as an aside. A token may also carry **its own precision** (`{base:1}`), which beats every declared default — digits are a display property, and the cell's one `digits` field cannot say that an estimate reads at three decimals and its aside at one. **A display is post-hoc** — every field a layout can print is populated at build, so choosing one triggers no computation and changes no number, and a token may be **derived** rather than stored (`resid`, `gap`, `sd`, `cv`). A numeric column's default layout is `mean_cv` — the spread as a percentage of the level, comparable between columns measured in different units — chosen per column and falling back to the bare mean where a mean is not positive. The **base count** is the display-time fact both producers share: folded into the Total cell when the table rests on one population, given one `n` column per block at the right when it rests on several (a spread, a regression's groups) — and the per-block Total columns then go, holding nothing but a repeated 100 %.

### The colour system

Colour has three orthogonal axes: a **measure** (which deviation to grade — `difference` / `ratio` / `odds_ratio` / `contrib`, or the two gap measures `adjustment` / `between_groups`), a **channel** (text and/or background), and a **significance policy** (`color_signif`: `ignore` / `grey_non_signif` / `guaranteed_effect`). The engine has three layers:

1. **Palettes** (`tab-palettes.R`, which holds every one of them) — OKLCH colour ramps, hand-tuned so intensity levels stay distinguishable, in light, dark and 8-bit variants, set via `set_color_palette()`; the **chrome** beside them (`tx_chrome_hex()`: the table's own ink, the greyed-out cell, the aside — of which the *ground* is the one a rendered table does not paint, following the page instead unless `tabxplor.background` says otherwise); and, where a page has no colour, three **publication palettes** (`PRINT_PALETTES`) saying the same thing typographically — one declared grid each, a row per break slot carrying its ink, face and mark, `theme = "print_ready"` choosing between them from what the table IS. A palette is always hex **and** face: a backend must never derive "is this bold" from "does this have a hex".
2. **Breaks** — per-scale thresholds (`COLOR_SCALES`). Every ladder is the SAME ladder written in another measure at one reference cell of 50 %, so a shade means the same size of deviation whichever measure a table is read on; each declares its `quantity`, its `anchor`, whether its two `sides` mirror (only where the quantity is unbounded above), and how many loud rungs it keeps on the background channel (`bg_keep` — a fill is the corrective voice). The shape rule is checked at load.
3. **Selection** — a vectorised `findInterval` engine (`fmt_color_plan` → `fmt_color_slots` → `fmt_color_channels`) that folds each cell per side and picks the strongest matching threshold.

The measure's behaviour — raw getter, scale keys, significance source, gating — lives in its `MEASURES` row, which drives both the plan and the legend with no per-measure branches; every backend then consumes the one artifact `fmt_color_channels` produces, which is why console, HTML, Excel, Markdown and plots colour identically. **`dev/colors.md`** derives where each ladder's anchor comes from, what colour-vision deficiency requires of a palette, and why a page with no colour needs a different palette rather than a desaturated one.

**The footer legend states the measure, not the palette.** One line is `[<columns> — ]<measure named in words>: <subject> ≥ <reference> <thresholds>; <subject> ≤ …`, then one clause saying what an *uncoloured* cell means — the reverse being a tautology the cells already show. The name is a per-(measure × ladder scale) fact on the same `MEASURES` row, in two registers (`word` for the console, `word_long` for the exports), because a difference of proportions, of means and of log odds are three quantities. A colour palette names no direction — its break-words *are* blue and red — while a publication palette, whose greyscale has no diverging ramp, keeps its two face words and its two sentences.

### The regression subsystem

`tab_reg()` gives models the same visual language: one model per column, each estimand rendered as `fmt` cells in the same `tabxplor_tab`. It reuses the 21 fields unchanged; `obs` and `gap_se` carry the regression-specific facts.

**A model column holds the crosstab's own pair.** It stores an adjusted level and its reference level, and derives both readings of that pair — additive (`diff`) and multiplicative (`ratio`); the observed column derives the same two from the counted pair. `measure` says which geometry is **promoted to the estimate** — the one carrying the interval, the stars and the colour — the others riding as asides exactly as in `tab()`. That is the round trip the package exists for: from an observed percentage out to a model and back to a percentage.

**The estimand is a cascade** (`reg-estimand.R`). **A link is a measure**: the one a model estimates directly — `difference` ↔ identity, `ratio` ↔ log, `odds_ratio` ↔ logit; the fourth reported measure, `raw_coefficient`, is not a peer but the model's own coefficient un-transformed, and it is TOTAL — where the link is already additive there is nothing to un-exponentiate, so it resolves to the additive row itself — and, being the fit's OWN number, it is conditional-only — so the argument naming the model takes the same words as the argument naming the report, and the statistician's vocabulary never surfaces. Four arguments, `family` → `link` → `measure` → `effect`, where `"auto"` means *follow from the left*, and one rule decides the rest: **a coefficient exists only where the reported measure IS the model's**; any other measure is applied to the model's predictions, averaged over the sample (`marginal`) or read at one constructed profile (`at_reference`, the ideal type). One clause qualifies it: `"auto"` never resolves to a *predicted* odds ratio, a specialist quantity asked for by name. Which model is fitted and which deviation is reported are two axes — `reg_formulas()` says what reached `glm()`, `reg_measures()` what an outcome can be asked. That lister **states the factorisation** rather than the product: a conditional row per fittable link (one model, one measure in its coefficients), then the prediction-based measures once, at no link in particular, because averaging fitted probabilities does not care which link produced them — with a block per family the outcome's kind offers, the detected one first.

`REG_ESTIMANDS` is **composed, not written**: `reg_compose_library()` emits one row per buildable `(link, effect, measure)` from four facts a family declares in `REG_FAMILIES` — its `level` kind (`pct` / `mean` / `count`), the `fits` it offers (the value set of `link`, first entry = its own), any header-word override and its footer qualifier — plus two shared maps: link ↔ measure, and what each kind of level can be compared by. A refusal is not a row but a derivation from the clause that failed, so a hole and its reason cannot drift apart. The family is auto-detected from the outcome (binary → logistic, unordered → multinomial, ordered → cumulative-OR ordinal) while a *number* is the user's call; one table can mix families, each column storing its own `model_family`. Hence the extension rule: **a new model is a row in a declared table, never a new argument or a word a user must learn** — a link is one map entry plus one `REG_LINK_FUNS` row (its transform and derivative — all a marginal contrast needs of a link, which is why the engine has no per-measure arm); a family is one `REG_FAMILIES` row, its footer statistics and model checks the only per-family work.

**One name per quantity** (`REG_WORDS` + `REG_CONTRASTS`). A header names the **measure**, the **contrast** is a marker on it and a log wraps the result, so the word is *composed* — `marker ∘ log-wrap ∘ acronym` gives `OR`, `mRR`, `refRD`, `log(cumOR)` — which stops two estimands sharing a header, or one estimand being named twice. The observed column and the colour legend take the measure **without** the marker — a univariable effect has no adjustment to be marginal over — so the observed/model pair stays one legend block.

**The observed companion — the distinctive feature** (`reg-empirical.R` + `reg-influence.R`). It is **on by default**, and one value decides where it goes: `column` (a crude column beside the model one), `cell` (the `est_obs` layout, inside it), `tooltip` (computed, printed nowhere) or `no` — `TRUE` resolving to `column` except where that would double a table already wide (`tab_vars` groups, a per-category outcome), which take `tooltip`. In every mode but `no` the value is stored in `obs` and read by `color = "adjustment"`, by `forest_plot()` and by the hover, so the mode is a layout decision and no arithmetic branches on it. Each modelled effect sits beside the **observed (crude)** one: the same estimand, on the same people, with one predictor instead of all of them — so *what did adjustment change* is read across the table. One column shape built twice, and the observed shape is composed rather than declared (`REG_EMP_BY_LINK` indexes `REG_EMPIRICAL` by the measure's link), so a model row and its twin cannot state two estimands; its value is a closed form on the per-cell grid where the univariable model is saturated, otherwise a refit through the very fitter the table came from. `reg-influence.R` computes the **standard error of the gap**: both estimators are fitted on the same rows, so only the difference of their influence functions carries the covariance — and that gap SE is what makes `color = "adjustment"` a test rather than a description. On a non-collapsible measure the movement is coloured but never tested: an odds ratio moves when any strong predictor is added, which is arithmetic, not confounding.

**A parametrisation is decided while the data is prepared.** An `a*b` entry in `predictors` is *a predictor whose levels are combinations, and whose univariable model is its own saturated fit*, so it is materialised as a column before the fit and every subsystem keeps reading an ordinary predictor; `REG_CROSS_ARMS` (`reg-cross.R`) declares its two shapes — a combined factor against one common reference, or slopes nested in a moderator. `shape` recodes a continuous predictor the same way, and `ref` shifts one to its anchor so the fit's own intercept is already the baseline the Constant row shows. Two more decisions are made there for the same reason: `family = "binomial"` on a 3+ level outcome collapses it to one level against the rest, and `na = "keep_for_predictors"` turns each predictor's missing values into a level — cutting a numeric one, since a number has no level to hold them. One rule covers all five: **the boundary defines the model's variables, then fixes their origin** — and the fit's own output is already the table. Whatever it recodes, `reg_prepare_replay()` redoes, or a diagnostic refits a different model on the same rows. Its converse names what does NOT belong there: **a predictor's level order decides the reference and nothing else**, because every factor predictor is fitted under treatment contrasts (`reg_fit_frame()` strips `ordered`, whose polynomial contrasts no per-level row could align). So the order of the remaining levels is display, applied to the row skeleton (`reg_skeleton_reorder()`) — which is why reordering them moves no number, in any family.

**A fit is distilled, not kept** (`reg-digest.R`). Everything the table goes on to compute — a marginal effect, a baseline, an influence function, a coefficient at any confidence level — needs a model's `coef`, `vcov`, `terms` and `family`, never the fitted object; so `reg_fit()` returns a **`tabxplor_fitdigest`** beside it, and every engine reads that. Which parts a digest holds is declared, one row per fitting backend (`REG_FIT_KINDS`) and one per stored part (`REG_DIGEST_PARTS`), so **a new model backend is a row**. Nothing length-`n` is stored: the model frame is rebuilt from the live data through the *same* `reg_fit_frame()` the fitter used, and the IRLS working weights and residuals are reconstructed from the parameters. What only a fitted object can answer — the model-fit statistics, the global tests, the assumption checks, each crossed pair's test — is computed **eagerly, while it lives**, and rides on the record; what a digest genuinely cannot serve buys its fit back through `reg_digest_revive()`. Hence the record's one estimand-dependent member is `tidy`, written per `(measure, conf_level)` by `reg_tidy_finalize()` from a native-scale estimate — which is what lets the jamovi cache key on the **model alone** and serve every estimand from one fit.

**The boundary and the build** (`reg-resolve.R`, `tab_reg.R` + `reg-spec-build.R`). `reg_resolve_args()` is the crosstab boundary's twin, with `data` *inside* it — `family = "auto"`, `multiplier = "sd"` and `shape` are answered by the data — and one grammar per axis: the four estimand arguments per outcome, `multiplier` / `shape` / `ref` per predictor (unnamed = the fallback, named = that variable). `reg_build()` then runs over a typed `new_reg_ctx`, its per-model half a declared product (`reg_spec_build()`), the three nesting axes — `tab_vars` groups × models × outcomes — dispatching through the shared parallel seam. **A model comparison is a default too**: several `predictors` sets are tested against each other without being asked, sequential where every model nests in the next and against the first otherwise, decided in `reg_compare_rows()` where the fits exist. ⚠ `compare != "none"` is what makes a build serial and makes it keep its fits, so the boundary degrades the automatic one to `"none"` wherever a comparison has no meaning.

**Effects and model checks.** A marginal quantity comes from tabxplor's own analytic g-computation, or from `marginaleffects` at a reference profile — derived from the contrast, never declared per row. `REG_CHECKS` catalogues the checks (linearity, dispersion, influence, proportionality, collinearity), each with the `shape =` cure that fixes what it flags — the check and its cure are one object — each priced (`cost`) and each declaring whether it runs by default (`footer_default`), because what a table must say and what it costs are two questions. The **observed shape** of a numeric predictor is the free half of the linearity check: one curve per outcome, binned with no fit at all, drawn in a window floored by the data's own sampling noise and by the first colour rung — so a flat run means flat. It goes in a small **shape table** below the footer, beside the range it is a picture of. **`dev/regression.md`** derives the gap SE, the one-column ordinal effect, when a risk ratio beats an odds ratio, why `predictors` is not R's formula language, and the specs of both chart families.

### Exports and rendering

`tab_export(x, format =)` (`tab-export.R`) is the facade over four backends: HTML (the default), Markdown, Excel and plot, sharing one preparation step — `tab_export_prep()` (`tab-export-prep.R`) builds an ephemeral render model (roles, references, faces, header spans, variable-name blocks) that every backend consumes. A spread swaps the two header bands, since after a spread a **column** is identified by its sub-population and a **block** by its variable: the column header takes the `col_group`, the span takes the `col_var` and, above it, the level only where that variable gives several columns per group. **Several `row_vars` stack row_var-major** — two row_vars are two tables over the same population, the `tab_vars` the sub-populations inside each — and **row order IS column order**, since label nesting is read off physical column position; a tab_var column is then dropped only where the level column alone is a complete row index, which one row_var is and a stacked pair is not.

Display values reach the backends by one source of truth: `format.tabxplor_fmt()` renders the text for console, Markdown and HTML, and `tab_xl()` writes a number with format codes from that *same* `format(syntax = "excel")`, so a display change never needs mirroring. **Excel keeps the cell a number and puts everything else in the code**: an aside becomes a column carrying its own segment (`(n={n})`), and every literal a template writes — the stars, the brackets, a sigma, a test label — folds into the numFmt, per section. A multiplicative cell holds its **reading value**, the signed fold, so `1/2.11` reaches the workbook without becoming text; text stays a property of a *cell*, not of a column. The exports' **unit row** is the console's own type tag (`<row%>`, `<n>`), written once per **block** — `tab_col_block_ids()`, the one definition of a block, which also decides where a vertical rule falls. Colour is single-sourced too — every backend reads `fmt_color_channels`. HTML colour is a slot **class**, never inline hex, the theme living in a `<style>` block from the one CSS generator (`tab-css.R`), so light/dark and the publication palettes work by stylesheet — except `print_marks`, whose signal is cell text and so comes from `format()` like the stars. **A table's title is one text with three placements, decided by the host**: a `<div>` sibling, the only shape that cannot size the table; a real `<caption>` under bookdown, which numbers a table only by scanning for one; and nothing at all under Quarto when the cell already wrote `tbl-cap` — and every `<table>` tabxplor opens carries `data-quarto-disable-processing`, since Quarto would otherwise restyle a table it did not build. `tab-transpose-render.R` flips a finished render model (a transposed column is heterogeneous and cannot be an `fmt` column), and `tab-theme-detect.R` best-effort-detects the console's scheme — a subsystem that must never error, because a wrong guess only mis-tints.

**How wide a thing is, and where it breaks, are measured from the rendered content.** A column name and a variable name are compound words, not prose, so they break at the seams a name is built from (`_`, `.`, `*`, camelCase) rather than at whitespace alone; a *variable* name is written vertically only where the rotation actually saves width — the names that cannot turn, a one-row block like `Constant`, set the floor every other name is weighed against, and a rotated one wraps to its block's own height. That decision is one prep fact both media read. Excel then has no fixed widths at all: each column is as wide as the widest thing in it that cannot wrap (a figure), while a header, a unit tag or a long label contributes its width divided by the lines it may use — measured per **sheet**, since a column index belongs to the sheet and not to the table sitting on it.

The **hover tooltip** (`tab-tooltip.R`) is that same rule read line by line: `TOOLTIP_LINES` declares one row per line — the token it renders, where its name comes from, which of the shared gates apply — and row order IS the reading order, so a line is named by its `DISPLAY_TOKENS$label`, exactly as the exports' unit row is, and one gate (non-empty · comparable · not the reference · not already shown · not already emitted) decides every one of them. It has **two rows**, declared the same way (`group`): the cell's own numbers, then the observed comparison — `obs` and the gap to it, a statement about another column — joined by a newline the stylesheet honours. It is **not translated**, deliberately: like the pillar type tags its words are the `fmt` field names, so the hover teaches the fields a user reads with `$`.

### jamovi

Two point-and-click analyses mirror the two producers: `jmvtab` (Crosstables) and `jmvtabreg` (Regressions). Each is a thin `R6` backend (`*.b.R`) over an engine-free build core (`jmvtab_build()` / `jmvtab_reg_build()`) driving `tab()` / `tab_reg()` through a content-addressed **live-UI cache** (`*-cache.R`), so an interactive tweak re-paints instead of recomputing. Each option is named after the argument it drives, so the backend is a pass-through, not a translation table — and where the panel asks a *simpler* question than the argument takes (a tick-box for `empirical`, two of `theme`'s seven values), R resolves the rest. An argument applied at RENDER (`theme`, `wrap_*`) is read straight off the options and deliberately kept out of `.opts()`, which is the crosstab cache key's complement. The regression store holds **distilled fit records** (kilobytes) keyed on the model alone — the model's own and each observed (crude) univariable one, one record shape told apart by its key — so every estimand change is a hit and nothing heavy crosses jamovi's `$state`. The generated `*.h.R` option headers are never hand-edited. **`dev/jamovi_module.md`** is the guide to the app itself — its runtime, its file formats, and the width chain that decides how much of a table a user actually sees.

### Cross-cutting invariants

Rules that span subsystems — do not undo them without reading why:

- **A number must not depend on a table attribute.** Inference facts (`conf_level`/`degf`/`basis`/`ci_method`) live on columns; `dplyr` drops table attributes.
- **A merge claims only what its weakest part carried** — the `vec_ptype2` reconcile applies the weakest-claim rule to inference attributes so a bind cannot over-claim.
- **Public API is stable; internals are free.** Soft-deprecate public arguments, and never break the `fmt` fields users read with `$`/`mutate()`.
- **Facts live in one table.** Add a row and read it through its accessor; the foreign keys checked at load keep cross-table references honest.
- **The `fmt` record is dense.** Every column carries all fields; "not applicable" is `NA`, never an absent field.
- **`format()` is the one display source of truth** — text backends and the Excel numFmt codes both come from it.
- **Levels drop after the tests.** Non-first levels (`levels = "first"`) are removed only after chi²/CI, so tests see the full level set.
- **Theme detection must never error** — it rests on no supported API; anything unknown resolves to "light".
- **A variable list is a list of SYMBOLS**: read it with `vars_chr()`, never `as.character()`, which deparses a non-syntactic name back into backticks.
- **A message is addressed to the person writing the call.** It says what is wrong, or what was decided for them, and the argument that changes it, written as code — one headline, at most one `x` and one `i`. It never explains the package's own reasoning, never names an internal, and never confirms what the user asked for or what the table already shows. An automatic decision goes through `tx_inform_once()`, whose id carries its *subject*, so it is said once per session per variable; a missing Suggests package goes through `tx_need_pkg()`, which names them all at once.

### Key Dependency APIs to read up on

Before working on the `fmt` type system, arithmetic or display, fetch these help pages via the `r-btw` MCP **docs** tools (or `?`) — recall of their exact contracts is the weakest link:

- `vctrs::new_rcrd`, `vctrs::field` — record type and per-cell field access
- `vctrs::vec_arith`, `vctrs::vec_cast`, `vctrs::vec_ptype2` — arithmetic and casting S3 contracts
- `pillar::pillar_shaft` — console display method
- `data.table` reference semantics (`:=`, `.SD`, `.N`) — internal aggregation
- `DescTools::BinomCI`, `DescTools::BinomDiffCI` — **Suggests-only**, for test parity: the CI math is the closed-form engine in `R/tab-agg.R` (`ci_pivot`/`ci_wilson`/`ci_newcombe`), which is what to read before touching CI.

### Documentation ecosystem

The docs form one hierarchy, general to specific. **Each fact is stated at exactly one layer, referenced (never duplicated) across the others, and always written present-tense** — the current design is the reference point, never how it got there. The one place dev history is allowed is the roadmap "DONE" summaries. In R scripts, **the comments/code ratio should stay under 0.2**.

- **`## tabxplor architecture`** (this file) — the cross-subsystem big picture: goals, data-flow, the declarative pattern, the type system, each subsystem's role and its meaningful "why". Rewritten only when the maintainer asks, by targeted cuts and replacements rather than accretion.
- **`## Repository Map`** (this file) — the file index: one role line per R file. *Cut, don’t accrete.*
- **R file-header comments** — per-file subsystem design: current architecture, key constraints, a pointer up to this file.
- **Inline `# DESIGN:` / `# WARNING:` tags** — the non-obvious "why" at the exact line, caveats to avoid, etc.
- **Vignettes** (`vignette("tabxplor")`, regression, programming) — usage and teaching, for users.
- **`vignettes/tabxplor-reading-a-regression.Rmd`** — the most precise account of what tabxplor's *philosophy*, *vocabulary*, *usage* and *real-world regression use cases* really are; its words (deviation, observed vs adjusted, the base, the round trip) are the package's own.
- **Roxygen man pages** (`?tab`, `?tabxplor-display`, `?tabxplor-vctrs`, `?tabxplor-options`, `?tabxplor-data.table`) — user-facing reference: *usage* and the main use cases, never build/internals/history. A `@param` states what the argument is, its values, and at most one sentence of when to change it; the rest is a link to the vignette that owns it. ⚠ The manual is LaTeX, so an Rd file is ASCII but for the few glyphs it can set (`— … × ÷`); `test-non-ascii.R` locks it.
- **`dev/*.md`** (`.Rbuildignore`'d) — transversal or expert technical guides only, and there are seven: `dependencies.md` (the dependency policy), `release_checklist.md`, `french_glossary.md`, `jamovi_module.md`, `colors.md`, `inference.md`, `regression.md`. Each holds what an `R/` header is too short to derive — a foreign system, a cross-file policy, a statistical derivation — and the header that needs it points at it by section. ⚠ The 2.0.0 evidence base is `dev/archive_2.0.0/` (indexed by its own `README.md`): a `dev/<name>.md` named in a DONE summary lives there, unchanged.
- **Roadmap "DONE" summaries → `dev/tabxplor_2.0.0_roadmap_DONE_PHASES.md`** — the ONLY place dev history lives.

Inspect a built table at runtime through the accessors: `tab_structure()`, `tab_columns()`, `reg_measures()`, `reg_formulas()`, `fmt_attr()`, and the `get_*` / `set_*` family.


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

`tests/testthat/` is the package's **contract**: it must fail when a user-visible fact changes, must not fail when an internal is redesigned, and must stay fast enough to run on every edit. Everything else lives in `dev/tests/`, which must *not* be run at the end of each session.

```bash
# The shipped suite. In a temp .R file (outside tests/), then: OMP_NUM_THREADS=1 Rscript that_file.R
#   Sys.setenv(TESTTHAT_CPUS = "6", NOT_CRAN = "true"); devtools::test("~/github/tabxplor")
#   devtools::test("~/github/tabxplor", filter = "tab-reg")   # one/few files while iterating

# The second suite -- only run it at release, or when the maintainer asks:
#   OMP_NUM_THREADS=1 Rscript dev/run_dev_tests.R
```

**Measured here:** shipped suite **~40 s** (46 files, 4 316 assertions, 6 workers; ~136 s serial).

### What belongs in which suite

A test earns its place in `tests/testthat/` when it would fail if a **user-visible fact** changed and would not fail when an internal is redesigned. Four kinds qualify: the **output goldens**; the **type and class contract** (`fmt` fields and attributes, the dplyr wall, degraded tables); the **public surface** (argument boundaries, declared fact tables and their foreign keys, options, exports); and **one canonical parity block per statistical engine** — `stats::lm`/`glm`, `nnet::multinom`, `MASS::polr`, `survey::svyglm`/`svyolr`, `svyVGAM`, `marginaleffects`, `brant` — because that is the only standing assurance the numbers are right.

Five kinds go to `dev/tests/`: **source-tree lint** (reads `R/`, `jamovi/`, so it can only skip under `R CMD check`); **phase-defect suites** (one historical 2.0.0 bug each); **internal-seam parity** (`carve`/`fuse`/`carrier` — the goldens already lock what those seams produce); the **secondary arm** of each engine's parity; and **exhaustive sweeps**, where the shipped file keeps a representative slice. `dev/verify_reg_invariants.R` is the older precedent for the last one.

⚠ **The shipped suite is subsystem-shaped**: a file is named for the subsystem it guards (`test-reg-estimand.R` ↔ `R/reg-estimand.R`), so a new test's home is derivable from the Repository Map. The exceptions are deliberate: `test-golden.R` / `test-color-golden.R` (the output tripwires), `test-edge-cases.R` (degenerate inputs across everything), `test-i18n.R`, `test-non-ascii.R`, `test-utils.R` and `test-plots.R`.

### One population, and when to leave it

`helper-fixtures.R` holds the suite's fixtures, memoised — testthat sources helpers **once per worker**, so a cached value is shared by every file that worker runs.

⚠ **There are two populations, and the reason is cost, not taste.** A crosstab aggregates on data.table and does not care how many rows it reads — measured, `tab()` and `tab_html()` take the same time on 3 000 rows as on 21 483. A model fit does care: `tab_reg()` is 5.3× faster on the sample, `nnet::multinom()` 14×. So the crosstab side reads the whole frame (`fx_gss()` / `fx_gss_fmt()`, which IS `forcats::gss_cat` — the goldens never had to move) and only the regression side reads the sample (`fx_reg_df()` / `fx_reg_fmt()`). **Moving a crosstab test onto the sample buys nothing and costs it its statistical power**; a regression test that genuinely needs power (a violated proportional-odds assumption, an O(1/n) agreement) takes the whole frame back.

### Threads and workers

✅ The suite **self-pins**: `tests/testthat/setup.R` pins data.table and BLAS/OpenMP per worker, and `tests/testthat.R` sets `OMP_NUM_THREADS=1` before they spawn. Keep the `OMP_NUM_THREADS=1` prefix anyway (grandchild processes, RhpcBLASctl-less setups).

⚠ **The trap this guards against**: `Config/testthat/parallel: true` runs each file in its own PROCESS, and each then multi-threads on its own — measured, 8 workers × (data.table ~6 + OpenBLAS ~10) = 165 threads on 12 logical cores, and a ~1 min suite ran >26 min.

⚠ **`detectCores()` counts SMT siblings.** This CPU reports 12 and has **6** real cores (`/sys/devices/system/cpu/cpu*/topology/thread_siblings_list` shows two siblings each); `parallelly::availableCores(logical = FALSE)` cannot tell either under WSL2. 8 workers beat 6 by ~6 % while oversubscribing a shared machine, so `tests/testthat.R` sizes the pool from the kernel topology. ⚠ **`devtools::test()` does not read `tests/testthat.R`** — set `TESTTHAT_CPUS` yourself there, which is what the recipe above does.

⚠ **`setDTthreads()` must never be called in a per-file loop**: it tears down and rebuilds data.table's OpenMP pool, which inflated a per-file timing harness ~2× and produced the misleading "before" figures in `dev/benchmarks/results_2.0.0/`.

**Never run anything else while the suite runs.** Before blaming the code for a slow run, check whether YOU are the cause: another R of yours running, then `ps -eLo pid,args | grep -c "[-]-no-readline --slave"` (thread count ≫ cores?), then orphans.

### Locale, sandbox, orphans

⚠ **A green local suite does NOT mean a green CI — this box is `fr_FR.UTF-8`.** GNU gettext ignores `LANGUAGE` when `LC_MESSAGES` is `C`/`POSIX`, which is the state under `R CMD check` on Linux and on the CRAN farm. So every French assertion passes here and fails there. French output is guarded by `skip_if_no_gettext()` (`tests/testthat/helper-i18n.R`), and each i18n feature is tested twice — an UNGUARDED English block plus a GUARDED French one. **Never simulate CI unless the maintainer asks**: `LC_ALL=C.UTF-8 LANGUAGE=en OMP_NUM_THREADS=1 Rscript <runner>.R` (use `C.UTF-8`, not `C`, which is harsher than any real runner).

⚠ **Two steps need `dangerouslyDisableSandbox`** — bwrap runs `--unshare-net` and `--ro-bind`s `NAMESPACE`/`man/`: `dev/tests/testthat/test-tab-parallel.R` (mirai's dispatcher needs sockets) and `devtools::document()`.

⛔ **NEVER kill a test run by killing its parent — you orphan the workers, and they do NOT stop.** Measured: two killed suites left 6 R processes alive for 52 minutes at ~860 % CPU, silently starving every later run.

- **Diagnose AND kill unsandboxed** — bwrap runs `--unshare-pid`, so each Bash call gets its own PID namespace: `ps aux` cannot see the orphans, and a sandboxed `kill <host-pid>` would hit the wrong process. Identify yours by the parent's `--file=/tmp/claude-…/<session-id>/scratchpad/…`, never by name alone (Positron runs its own R).
- **Never `pkill -f <pattern>`** — measured, `pkill -f testthat` killed the calling shell, and `pkill -f t9.R` is what orphaned the workers. Read `ps` first, then `kill` explicit PIDs.
- **Never pipe a long run through `tail`/`head`** — they buffer until EOF, so the log looks empty and the run looks hung. Write to a file and read that.
- ⚠ Killing PIDs needs the maintainer: surface the `ps` evidence and hand over the exact `kill -9 <pids>`.

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

Look at `dev/jamovi_module.md` and `@dev/jamovi/` for detailed informations.

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


### Phase 24 — About to release, last checks

#### Phase 24a — Unified theme framework ✓ DONE

**The site is themed, and the theme lives in a package.** tabxplor's pkgdown site was themed by nothing at all — stock Bootstrap dark, stock highlighting, `_pkgdown.yml`'s `template:` two lines — while the whole design sat unused in `dev/` as preview tools and generated `.scss` fragments. It is now delivered by **`~/github/txtheme`**, a new small R package + Quarto extension where a colour is decided once, in one declared grid, and every consumer reads what a generator wrote from it. tabxplor is its first consumer, not its home.

**What tabxplor's own YAML shrank to.** `_pkgdown.yml`'s `template:` gains one line, `package: txtheme` — no `bslib:` block, no `theme:`/`theme-dark:`, no brand file. `pkgdown/extra.scss` is back to the `.dropdown-menu` rule alone, and `DESCRIPTION` names `BriceNocenti/txtheme` in `Config/Needs/website` (never a Suggests: no R code of tabxplor's touches it, and `Config/Needs/*` never reaches CRAN).

**Departures from the brief** (`dev/unified_theme_framework.md`, moved to `txtheme/dev/design.md` and rewritten from proposal to description):

- **No `_brand.yml` for pkgdown, and no `brand.yml` dependency.** A template package's `extra.scss` arrives as a *rules* layer (`bslib::bs_add_rules()`), where a Sass variable is inert but a Bootstrap 5.3 **custom property** is not — and all eight chrome targets have `--bs-*` twins. The brand file bought pkgdown nothing; the brief's eight-line `bslib:` block became zero lines.
- **⚠ The `-rgb` twins, which the brief did not mention.** Bootstrap derives `--bs-secondary-color`/`--bs-tertiary-color` from the *literal* body-colour channels and `pkgdown.scss` reads `var(--bs-body-color-rgb)` in eleven places; `a` reads **only** `--bs-link-color-rgb`, so setting the link hex alone does nothing at all. The generator emits each twin beside its hex plus the two derived `rgba()` rules, and `--bs-link-hover-color` derived by Bootstrap's own 20 % tint.
- **One declared palette instead of a vendored 177-scope theme JSON + `overrides.yml`.** Four grids, split by the namespace each key belongs to: `TX_PALETTE` (colour), `TX_SLOTS` (where it is painted), `TX_TOKENS` (skylighting's 31, once each), `TX_BRAND` (Quarto role). Foreign keys checked at load, as in `zzz-fact-keys.R`.
- **The heading ladder is derived, not typed.** Each rung's row carries `spec = "oklch 0.950 0.10 100"` and `.onLoad()` rebuilds the hex from it — `warm-95-10`, floored on the ink at L 0.840.
- **The `.at` shim and the annotations sheet ship with the package**, through its own `inst/pkgdown/BS5/templates/in-header.html` (which keeps pkgdown's own line, and which a site can still shadow). ⚠ The annotation classes are **opt-in** (`template: params: txtheme: {annotations: true}`): `.non, .error {text-decoration: underline double}` would decorate every warned or errored example on a reference page, which emits `<span class="warning">` of its own. tabxplor does not opt in.
- **`heading_ladders.R`'s maths became txtheme's exported API** — one true source. The file keeps the 64 ladder proposals and attaches `txtheme::oklch_hex()` / `hex_oklch()` / `max_chroma()` / `apca()` / `contrast()` under the names the three previews already call, so nothing else changed. Dev-only: `dev/` is `.Rbuildignore`d and stripped from the release branch.
- **No recorded APCA column.** A slot knows the ground it is read on, a colour does not, so contrast is a report `build_theme()` prints per slot rather than a cell that could drift. `inst/prose/prose.scss` was dropped for the same reason: every prose rule is a `TX_SLOTS` row (colour + style), which beats a second hand-written file stating half of each rule.

**Two findings that cost a render each, both now recorded in `txtheme/dev/design.md` Appendix A:**

- ⚠ **libsass cannot compile CSS `min()` with mixed units.** The committed `max-height: min(80vh, 34rem)` in `pkgdown/extra.scss` aborted the whole site build with *"Incompatible units: 'rem' and 'vh'"* — a latent breakage found by this phase. Uppercase `MIN(...)` matches no Sass function and passes through; pkgdown's own `pkgdown.scss` writes `MAX(100%, 20rem)` for the same reason.
- ⚠ **A Quarto `color.palette` entry named after a brand role is promoted to that role, in both modes and silently.** A palette key `link` put the dark accent blue on the *light* stylesheet at Lc 30. The colour is called `accent`, and `.onLoad()` refuses any name in `BRAND_ROLES`. (A role that names only one mode *is* correctly ignored in the other — verified, so the unified `{dark:}`-only brand file is safe.)

**The acceptance test.** Stage 0 themed the site by hand as a golden file; stage 2 replaced it with `template: package: txtheme`. The compiled Bootstrap CSS carries the **identical 3112 rules**, the only difference being that `.dropdown-menu` moves from before to after the theme block — pkgdown's documented layering (a template package's `extra.scss` lands *before* the site's own, which is what keeps tabxplor an `!important`-free lever over txtheme). Verified further: 30 scoped token rules, `--bs-body-color-rgb: 205,203,188` (not Bootstrap's stock `222,226,230`), both assets copied, `txtheme-at.js` linked at the right relative root, the annotations sheet not linked, `check_pkgdown()` clean.

**Removed from tabxplor:** `dev/highlight-starless-monokai-{atom,pro,one}.scss` (their own header said "copy this to `pkgdown/extra.scss`" — `template: package:` *is* that copy), `dev/annotation_classes.css` (moved to `txtheme/inst/prose/annotations.scss`, minus its `:root` block, which the generator now writes), `dev/unified_theme_framework.md` (moved). `dev/site_theme_preview.R` lost `starless()`, the three port definitions, the writer loop and the on-page YAML box (578 → 485 lines): it reads txtheme's shipped flat `.scss` through its own existing parser, and its job narrows from "choose the theme by eye" to "check the shipped theme against real markup".

**txtheme itself:** `R CMD check` 0/0/0 in 12.6 s, 169 assertions. **No `Imports` at all** — pkgdown reads a shipped file rather than loading the namespace, so on a website build the package only has to install; `tx_tribble()`/`tx_grid()` reimplement the grid fold in ~25 lines of base R, and the JSON and YAML writers are hand-rolled for the same reason. `build_theme(check = TRUE)` rebuilds every output in memory and diffs it against disk (no dates in banners, sorted keys), and the suite asserts it. Verified on the Positron-bundled Quarto 1.10.18: a two-page project renders with `format: txtheme-html`, two syntax-highlighting stylesheets with different content as a `quarto-color-scheme`/`quarto-color-alternate` pair, and the brand hexes in the dark stylesheet from a bare `_brand.yml` at the root with no `brand:` key anywhere.

**The workflow's prune call is now guarded.** `.github/workflows/pkgdown.yaml` ran `source("dev/site_prune.R")` unconditionally, which would have failed on `master` after the release, where `dev/` is stripped — and where the prune is unnecessary anyway, `CLAUDE.md` being stripped too. It is `if (file.exists("dev/site_prune.R"))` now.

⚠ **One maintainer step remains**: rehearse tabxplor's change as a **PR** rather than pushing to a branch the workflow deploys from — `.github/workflows/pkgdown.yaml` runs on `pull_request` with its deploy step gated `if: github.event_name != 'pull_request'`, so a PR is a free full-fidelity check that pak resolves `BriceNocenti/txtheme` (a public repo: `secrets.GITHUB_TOKEN` cannot fetch a private one).

#### Phase 24b — the regression teaching article, retitled and brought level with its French twin ✓ DONE

**The French twin had moved ahead, and the English had not.** The maintainer manually rewrote `tabxplor-all-else-equal-fr.Rmd` over two commits (288 insertions / 213 deletions): a reorganisation into six sections, a dozen clarified approximations, two passages cut. This phase carried the meaningful half of that rewrite into the English — in English idiom, not translated back — and renamed both files, since the title no longer leads on "all else equal".

**Both files are renamed, and every reference with them.** `vignettes/tabxplor-all-else-equal.Rmd` → **`vignettes/tabxplor-reading-a-regression.Rmd`**, titled *Reading a regression without losing sight of the percentages*; the French twin → `vignettes/articles/tabxplor-reading-a-regression-fr.Rmd`, keeping its own title *Interpréter un modèle de régression sans perdre de vue les pourcentages*. ⚠ **This breaks published URLs and the `vignette()` name users type.** Updated in one pass: `_pkgdown.yml` (both navbar entries — the French one was stale against its own file's title — plus both `articles:` index rows and the "Start here" blurb), `README.Rmd`/`.md`, `pkgdown/index.Rmd`/`.md`, `R/tab_reg.R` and `R/data.R` roxygen (`devtools::document()` rewrote `tab_reg.Rd`, `questionr_hdv.Rd`, `car_salaries.Rd`), the four sibling vignettes' cross-links in both languages, `dev/regression.md`, `dev/french_glossary.md`, `dev/tabxplor-the-shape-of-a-number.Rmd`, and this file's Repository Map and documentation-ecosystem entry. `dev/archive_2.0.0/` keeps the old paths, being frozen. `pkgdown::check_pkgdown()` clean.

**The six-section progression, which is the reorganisation's whole point.** §1 is now the two cross-tables alone; §2 turns a percentage into a comparison and settles significance; §3 holds the other variables equal — choosing a model, the bridge, reading a row, then the numeric predictor and the reference profile, both moved down from §2; §4 comes back to a sayable sentence; **§5 is new**, gathering everything about adjustment (STROBE, the five things, block-by-block, colouring) which previously sat inside §3; §6 is what the model cannot settle. The gain: adjustment is now studied *after* the reader can say a number out loud, instead of before.

**What was ported, and what it fixes.** The racetrack derivation of odds (6.1-to-1 against 2.8-to-1, ratio 0.46, printed `1/2.11`) replacing a run-on paragraph — Cibois' image, already credited in the bibliography but never actually used. The **four-row odds-ratio scale table** (5→10 %, 50→68 %, 74→86 %, 90→95 %, all at OR 2.11) and the rule it earns: *an odds ratio means nothing until you know which percentage it starts from*. The **logit paragraph**, the first place the article explains the word *link*, with its punchline that the commonest model for the commonest kind of social-science variable works natively in the hardest deviation to read. The clinical-trial contrast that makes "significance is a permission slip" land. The **Model fit** report card, which was missing two of the rows it actually prints (`Dispersion`, `LR vs null`). The two safe English ways to say an odds ratio. A dedicated `##` on marginal effects, with the Cibois false friend (*effet marginal* = in percentage points, vs the original sense, averaged over the sample). Simpson's paradox, which the English never named. `outcome_level = "No"` demonstrating that a ratio is not symmetric.

**Two structural table changes.** The three `checks` families are now **one side-by-side `tab_reg()` call** on three copies of the column instead of three separate tables — the reader compares rather than scrolls, and it is one fit cheaper. The block-by-block model list now starts from `"colour" = "colour"` alone, so the progression runs −11.7 → −11.7 → −7.8 → −5.2 — and the repeated first number is the bridge shown twice, a univariable model *being* the cross-table.

**Three passages are commented out, not deleted** (maintainer's call, matching the French): the `link` expert knob, the Table 2 fallacy, and the `display = "base"` demo. ⚠ Their chunks carry `eval=FALSE, include=FALSE`, because **knitr evaluates chunks inside HTML comments** — pandoc never sees them, but the fits still run. The French file paid that cost on its own commented `link` block; fixed there too. **Westreich and Greenland, 2013** left both bibliographies with the section it anchored.

**Four errors found by rendering rather than by reading.** The English claimed a starless level was non-significant *at 95 %*; the star ladder is 90 / 95 / 99 % and the greying is decided at 95 %, so both are now stated. ⚠ The French said 90 % and **was right** — an earlier "correction" of it was reverted. The 2 SD step lands at 4.7 databases, not "about four". `reg_measures()`'s refusal message is suppressed by this article's `message = FALSE`, so the prose no longer promises it. And two search-and-replace scars — "of previous `car_arrests`, previous convictions" and "These are academic `car_salaries`" — plus a chunk passing `color = c(TRUE, "adjustment")` while its prose said `c("measure", "adjustment")`.

**Measured**: English renders in **15.2 s**, French in **14.7 s** (down from before, the commented block no longer fitting models). Shipped suite **FAIL 0 | PASS 4376**. Every figure quoted in new prose was read off the rendered tables, not carried over from the French: observed range 61–91 % (OR 6.7), the 2 SD odds of 3.5, the binomial 1.55, the marginal 1.34 at `outcome_level = "No"`, the CVs of 28–37 %.

#### Phase 24c — last manual review ✓ DONE

Fourteen items from the maintainer's own read-through of finished tables. No new feature: every fix
is a wrong number, a wrong emphasis, a clipped cell or a stale option, and each landed in a declared
fact or a boundary that already existed.

**The precision band, stated once.** `tab_reg(car_salaries, salary, …)` printed `(101 002.4)` and
`+14 088.0` — one decimal of noise on a six-figure mean. The rule is now one sentence: **a column
keeps between 2 and 3 significant figures of its own level unless the user says otherwise**, written
with one primitive, `tx_sig_digits()` (`R/fmt_class.R`). It applies to **unit scales** alone — those
whose level is in the outcome's own units, gated on the fact the grid already carries
(`EST_SCALES$base_display == "mean"`) — and never to a percentage, a point, a ratio or an odds ratio,
whose range is known and whose precision is declared. `reg_cell_digits(scale, level_mag)` clamps the
declared default; `fmt_magnitude_cap()` stops format()'s **four** floors (`DISPLAY_MIN_DIGITS`,
`est_digits`, `fmt_ci_digits`, the `diff_mean` 0→1 bump) putting the decimal back — measured, storing
0 alone was invisible, `est_ci` still printing `[2 853.0; 12 396.0]`.

⚠ **The magnitude is read once per SPEC, from the whole frame** (`reg_resolve_specs` →
`new_reg_spec(level_mag =)` → the three existing stamping sites). A post-hoc pass over the finished
table was designed first and is wrong three ways, each verified: grouping by *scale* merges two
outcomes of unlike magnitude (`tab_reg(car_salaries, c(salary, yrs.service), …)` is one table, four
`raw_diff` columns); grouping by `(scale, col_var)` splits a **model comparison** into singletons and
separates the crude column from its twin (`R/tab_reg.R` — the crude companions share the model's
`col_var` *except* in comparison mode); and `tab_vars` never reaches a shared finalize at all
(`reg_build_group()` recurses into a full `reg_build()` per group), so one group would print a
decimal the next did not. Read off the spec, a model column and its observed twin agree by
construction. `num_digits_floor()` (the band's lower edge) is deliberately left as it is: its bounds
are closed at the powers of ten, which is the one place the two edges do not line up.

**`min_digits` is now scoped, which fixes two reports at once.** It was applied *only where the
stored digits is 0*, and the stored value is the LEVEL's precision — so `{coef}`, an aside on
another scale entirely, inherited a mean's one decimal and printed a log odds as `+0.1`. It is a
**floor** for a token that is neither the column's estimate nor its level, and the old
default-on-unset rule where it is. The ordinal `<sup%>` column printing `49.1%` is the same cause on
the Excel side: `mat_aside_cols()` gives a split-off aside the display `"({base})"` and the test read
`raw_display == "base"` *literally*, so `EST_SCALES$base_digits` never applied. Both now key on the
template's own primary token (`prim_raw`), so `"base"` and `"({base})"` read alike.

**The shape table is a note, and says only what it can stand behind.** Its outcome cell names the
subject once and writes the formula on the letter — `p = %Married ; log(p/(1-p))` where it was
`log(%Married / (1 - %Married))`, half the width — with **two syntaxes from one producer**
(`rd_link_text(syntax =)`), the html one setting the qualifier as a real subscript. The header is
`outcome`: the cell says "model scale" by showing it. The block wears the aside ink (`grey2`) at 90 %
in every medium — a `.tx-shape` class in html, `cli::make_ansi_style()` on the console, a font colour
one point down in Excel — with a noisy row one step dimmer (`grey`).
⚠ **An ordinal or multinomial outcome now draws no curve here**: `rd_link_y()` reads it as
`Y != first`, which is one of its K−1 readings and the least trustworthy — measured on
`gss_cat$partyid` the reference category is **0.4 %** of the sample, so the row printed
`99-100% (OR 7.0) ns` over a flat run. The row stays, names the outcome, and its shape cell reads
`see reg_check_plots()`, which draws them all. In Excel the curve's merge went 2 → **3** columns and
the block is no longer clamped to the table's width: it is laid *over* the sheet, so on a narrow
table it runs one column past the right edge rather than cutting the one cell that cannot wrap.

**Less bold under the table.** A regression footer row is black and **not** bold — colour is the only
emphasis a model-fit number keeps, so a flagged check keeps its shade and a non-significant p-value
its red. Two lines in `tab-export-prep.R` decide it for every backend at once; `Model fit` stays bold
through the variable-name COLUMN, which html and Excel already bold in its own right. ⚠ Markdown
keeps its own divergence (`tab_md()` opts label columns out of `bold_rows`, for crosstab row-variable
names too): aligning it would move every Markdown golden for a change nobody asked for.

**`comp = "all"` with several `row_vars`.** The stacking bind promoted **every** total row to a
reference row, which bolded them all and — a reference row never being coloured
(`gate_row = "refrow"`) — took the sub-totals' over/under colours away. That promotion is
`comp = "tab"`'s rule and only its: under `comp = "all"` there is one reference per `row_var`, in the
total table. One guard in `promote_totrow_to_refrow()`. The *numbers* were always right —
`get_ref_field()` keys on `is_totrow & is_tottab`, never on `in_refrow`.

**`ref` survives a `shape` cut.** `na = "keep_for_predictors"` forces `sd_bands` on a numeric
predictor with missing values and `shape_apply()` makes it a factor — *before* `ref` is resolved,
and the two `ref` vocabularies are disjoint by kind, so `ref = c(tvhours = "min")` reached the factor
resolver and aborted (a bare `ref = "min"` aborted differently, for naming no eligible variable).
`reg_ref_after_cut()` translates the anchor into **the band that value falls in**, at the one point
where both readings of the column exist — exact for all four keywords and for a literal number,
since `reg_anchor_value()` already turns each into one. A bare default is translated the same way and
then dropped, having been honoured. The same failure under an explicit `shape = c(v = "quartiles")`
goes with it.

**Excel: `#####`, and the borders a merge swallowed.** Measured with `systemfonts` at the default
10 pt, converted with the file's own px model (`width * 7 + 5`): the base font's digit is 7 px and
**both** number fonts' is 8 px, so `XL_MONO_RATIO`'s `has_stars` gate was backwards — it
over-provisioned the starred path and left the plain one 14 % short, which is the whole `100%`
failure with no bold in sight. It is deleted for one `XL_NUM_RATIO` (1.15) on every figure column,
plus `XL_BOLD_RATIO` (1.12) applied **per cell** — bold deltas measured at `0%` 20→22 px, `100%`
36→40, `21 483` 44→50 — never per column, the Total row being bold and spanning every one of them.
`XL_PAD` stays 1.0. A TEXT cell is measured with the string it is actually written with (`xl_code()`
hoisted above the sizer), since an ordinary cell shows its numFmt and not `special_formatting`.
⚠ **Excel draws a merged range from its top-left cell**, so a border painted per ROW is simply not
drawn on a vertically merged label column: verified on a written workbook, `A3:A4` swallowed the rule
under the column names, `A5:A20` the rule between two `row_vars`, and `A21:A36` the table's own
closing rule — the label column "leaked" while every other column closed. Each range's edges are
folded onto the cell Excel reads, and the final `new_group` boundary is no longer dropped, so the
Excel bottom matches html's 2 px. `dev/xl_width_review.R` writes the eight edge cases as one workbook
for the maintainer's visual pass; arithmetically every column now clears its widest **bold** string
with ~1 character to spare.

**`ci_method = "profile"`** is wired end to end and has no loop; three real defects fixed. A family
whose dispersion is *estimated* (gaussian, every quasi-) has no profile interval and fell back to
Wald **silently**, while `reg_wald_method_name()` stamped the word from the *argument* — so a footer
claimed "profile" over Wald bounds. `reg_method_used()` reads the fit record's own `profile` flag
(already stored), and the third refusal now informs like the other two. `confint.profile.glm` ends in
`drop()`, so a **one-coefficient** fit returns a length-2 vector with no dimnames and `ci[idx, 1]`
aborted (swallowed on the crude path, fatal on the model one) — coerced back to a matrix. The cost
is by design and stays: the button says `profile = <i>(profile-likelihood ; not cached, long)</i>`.

**jamovi.** `0` joins the numeric `ref` picker (glm's own anchor; `reg_anchor_value()` already parsed
it); `ci` joins the `display` presets, before `est_ci`, needing nothing in R (`ci` is a bare token);
and `outcome_level` is gated on the family in the build core, exactly as `trials` already was — the
panel keeps a stored level across a family switch, so switching multinomial → ordinal used to abort
`tab_reg()`. Gating in R protects every caller and keeps the choice for switching back.
⚠ **One msgid carries one translation**: the working tree held `ref = <i>(reference)</i>` twice with
different French, which `msgfmt` refuses — breaking the *whole* French UI, not one label. Fixed at
the source, where it belongs: the regression panel's English now reads `(reference profile)`.
⚠ `jmvtools::prepare()` **deletes** `inst/i18n/fr.json` without rebuilding it; only a full
`install()` compiles the `.po` into it (285 → 288 entries, all four new strings translated).

**Measured.** Shipped suite **FAIL 0 | PASS 4457**, in ~40 s. The goldens were regenerated for the
working tree's `R/tab-palettes.R` re-tune of the eight light `bg` rungs, verified cell by cell first:
0 fixtures where WHICH cells carry a colour changed, 0 non-hex substitutions, exactly 8 distinct hex
moves, no text-ramp hex touched — the palette moved and the colour engine did not, which is what let
the LOCKED `_color_golden` cases be rewritten. `_snaps/golden.md` takes the same eight plus this
phase's four `.tx-shape` rules; the structural `_golden/*.rds` did not move at all. Both ledgers
(`dev/make_golden.R`, `dev/make_color_golden.R`) carry the argument. ⚠ The shipped fixtures clear
the new digits rule **by luck** (`age` ≈ 47, `tvhours` ≈ 3, `n_mean_w` max 10.23), so a green suite
was no evidence: 15 new assertions were added in their subsystem homes — `test-reg-estimand.R` (the
band's boundaries), `test-tab-reg.R` (the three shapes that break a post-hoc grouping),
`test-tab-display.R` (the foreign-token floor, the Excel `{base}` split, the six interval geometries,
the console `on_fill` ink and the background-only footer), `test-reg-resolve.R`
(`ref` across a cut), `test-tab-export-prep.R` (footer bold, `comp = "all"`), `test-tab-xl.R` (a
width regression asserted against the ratios, never a hard-coded number), `test-reg-assumptions.R`
(the two shape syntaxes, the rank-family pointer) and `test-i18n.R` (the shape table, both readings).

**Three more from a second read-through.**

*A fill with no text colour takes the theme's `on_fill` ink — in the CONSOLE too.* The rule is stated
once (`tx_chrome_hex()`) and was honoured by the exports and the footer legend but not by the cells:
the console paints a background channel as an ANSI fill, so a cell coloured on background alone kept
the TERMINAL's own foreground — light, on the dark theme whose fills are light panels, hence
unreadable. `pillar_shaft.tabxplor_fmt()` now composes it exactly as `fmt_get_color_code()` does.
`on_fill` is NA on the light theme, so that output is byte-identical.

*Found while testing it: a background-only colour crashed its own footer.* `color = c("no", ratio)`
has no TEXT measure, and `legend_ref_info()` read that absent one for its baseline — `ref_kind` came
back NULL and the whole legend aborted, taking the table's print with it. It reads the colouring
measure of whichever channel carries one, the same fallback the `policy` line beside it already made.

*An interval bound is now written in the same notation as the estimate it brackets* — `[+35;+45]%`
where it printed `[35;45]%` beside an estimate reading `+35`, and `[÷1.00;×1.37]` where the fold
showed but the multiply did not. ONE rule, no per-measure arm, composed from two facts already
declared: `EST_SCALES$neutral` says whether a bound names a SIDE at all — it is `NA` on a LEVEL
scale, where a bound is a percentage or a mean and must stay bare, and that NA is the whole gate —
and `MEASURES$break_over` / `$break_under` name it, the very pair the colour legend prints. A bound is
then (glyph for its side) + (its magnitude in the scale's own geometry). ⚠ That is also why the ODDS
RATIO is untouched, as it should be: it declares an EMPTY over-glyph, so `[1/4.45;1/2.19]` and
`[1/1.15;1.75]` come out of the same rule unchanged — the table is read, never a sign hard-coded.
`tabxplor.ratio_print_raw` switches every glyph off, estimate and bounds alike. No golden moved
(none displays a `{ci}` on an effect scale), which is precisely why the six geometries are asserted
directly in `test-tab-display.R`.

**A spread narrows a layout nobody named.** `spread_vars` multiplies the columns by the spread
variable's level count, so a cell has a fraction of the width it had — and a numeric column was
still spending it on a coefficient of variation. The rule is stated once
(`tab_narrow_default_display()`, `R/tab-display.R`): *a column still wearing the DEFAULT layout its
leaf chose falls back to the bare estimate its own scale declares* (`EST_SCALES$default_display`).
⚠ The test is **"is this still the leaf's own choice?"**, recomputed from the column's own values
(`num_default_display()`) rather than recorded — which is what keeps the rule out of every other code
path: nothing is stored, no flag is threaded, and `display =`, `ci =` and a post-hoc `set_display()`
each keep their layout by simply not matching. And it runs at the SPREAD, which `tab()` performs
BEFORE `tab_apply_display()`, so the ordering alone is what makes an explicit `display =` win. It
lives in `tab_spread()`, which is public, so a spread a user performs themselves reads the same.
Percentages are untouched: only a COMPOSITE default has an aside to drop.

**One line of air under a finished table, in every medium.** A table is a block of its own, and a
host that gives a `<table>` no bottom margin (`html_vignette`, jamovi, the Viewer page) welded the
next paragraph to the legend — so a document had to write its own line break after every table. One
declared value, `TX_TAIL_SPACE` (`R/tab-css.R`), read by both stylesheets: `.tabxplor-tab` (the
`<table>` of the html engine, the fenced `<div>` of `tab_md()`) carries `margin-bottom`, and the
legend rides in `<tfoot>` so the gap falls below it. Three things make it a rule rather than a nudge:
it is a **margin**, so adjacent vertical margins COLLAPSE and it is a FLOOR — where a host already
spaces its paragraphs more generously nothing moves; the `.tabxplor-tab table` half of the base rule
still zeroes the INNER table and out-specifies it at (0,1,1), so a markdown div adds no second gap;
and `tab_kable_join()` **drops its `<br>`**, the margin now separating two stacked parts as well as
trailing the last — one mechanism where there were two, which is what stops a table and its own
shape table drifting two lines apart. ⚠ jamovi moves the gap onto `.tx-scrollbox` and gives it up on
the last table inside (`jmv_results_style()`): `overflow-x:auto` makes the box a formatting context,
so the table's own margin would sit *above* the horizontal scrollbar instead of below everything.

⚠ **One maintainer step remains**: the Excel width workbook is a visual judgement — say where a
column reads too tight or too generous and the three constants move.

#### Phase 24d — the Reference page, read top to bottom ✓ DONE

**The index is now ordered by what a user reaches for first**, and a section says only what its
entries' titles cannot. Twelve visible groups: the two producers (`tab()`, `tab_reg()`, and
`tab_counts()` beside them), the jamovi analyses, then what can be DONE to a finished table
(reshape, export, chart), the small helpers, the superseded entry points, and last the vocabulary
and programming surface (display/colour, model estimands, options, introspection, the `fmt` type).
The `Variants of tab()` group is dissolved. Every `desc:` that renamed the functions listed under it
was cut — `Charts` now reads `Both need **ggplot2**`, which is the one thing two titles could not
say — and four groups whose title is already the whole sentence carry no `desc:` at all.

**Seventeen titles rewritten**, the index line being the only description most readers see:
`tab_reg()` "All-in-one tables for regressions, with each modelled effect beside its observed one"
(was 12 words of jargon), the four exporters down to a verb and an object ("Render a table as html",
"Write a table to an Excel workbook"), `tab_spread()` "Turn a sub-table variable into columns",
`new_tab()` and `fmt()` naming what they build rather than their class. `tab()` keeps its own title;
`tab_reg()`'s is written to sit under it.

**`tab_num()` is superseded**, badge and all, and moves beside `tab_many()` and `tab_plain()`:
`tab()` builds the same table whenever `col_vars` holds numeric variables. Nothing in `R/` ever
called it — the build reaches `num_core()` directly — so this is documentation, not a behaviour
change: no warning, no signature change. `tab()`'s `@seealso`, the `fmt` topic's example and the two
programming vignettes stop teaching it. ⚠ The `fmt` example gained `color = "auto"`: that is the one
place the two differ, `tab_num()` alone starting `color` at `"auto"` where `tab()` starts at `"no"`.

**`tab_supports()` is internal.** The predicate had no call site in `R/`, no test, and one user:
the programming vignettes. `TAB_OPS` and `tab_check_structure()` are untouched — the rules stay
declared, only the public predicate goes — and `tab_structure()`'s page stops advertising it.

**Three `experimental` badges removed** (`reg_check_plots()`, `shape_numeric_var()`, `fmt_attr()`);
`tab_structure()` and `tab_columns()` keep theirs, and `tab_transpose()` keeps its accurate
`deprecated` one in the reshape group where users look for it.

**The five example-data topics leave the index** through a section literally titled `internal` —
pkgdown's documented way to build a topic's page and keep it off the index — so `?car_salaries`, the
site search and every `\link{}` from an example still resolve.

⚠ **`forest_plot()` / `reg_check_plots()` were never duplicated**: one `contents:` entry and one
`\alias{}` each. What read as a duplicate was the section `desc:` naming both functions in prose
directly above the two entries that name them again — the redundancy this phase removed. (pkgdown
2.2.1 also emits a section's heading and its entry list as two sibling `<div class="section
level2">`, which widens the gap between them; that is stock markup.)

⚠ **`man/` was stale for three index titles** (`tab`, `tab_reg`, `tab_counts`): `R/` had been edited
without a `document()`, so the built site showed the previous wording. The rebuild fixes it.

**The two home pages are generated again, by one script.** `dev/build_readmes.R` knits both
`README.md` (GitHub, black-and-white) and `pkgdown/index.md` (the site, in colour) from their
`.Rmd` twins, against the WORKING TREE (`load_all(export_all = FALSE)`) rather than whatever
tabxplor is installed. `dev/build_site.R` runs its `index` half before every build, so the home
page cannot be stale. ⚠ It shells out to a subprocess rather than sourcing: each source pins
tabxplor options and `LANGUAGE` so the page cannot depend on who builds it, and those pins would
otherwise leak into every example and article rendered afterwards. Both `.Rmd` gained
`md_extensions: -implicit_figures` — pandoc turns a lone `![alt](src)` into a `<figure>`, which
would promote the hero image's alt text into a visible caption. `README.md` had drifted from its
own source (it still carried the pre-2.0.0 feature list), so the rebuild is a catch-up.

**The site opens light, whatever the reader's OS says.** The switch stays and a chosen mode is
still honoured for good — dark is opt-in, its ramps keeping less separation than the light ones,
and a coloured table being read on a white page by convention. One file does it,
`pkgdown/extra.js`: pkgdown links it from `<head>` right after its own `lightswitch.js` and
before the page paints, and it seeds the very `localStorage` key that switch reads. ⚠ Seeding
rather than only setting the attribute is the point: the attribute alone would leave the button
claiming *Auto* over a light page, and pkgdown's `prefers-color-scheme` listener would flip it
back on the next system change. It also keeps the TABLES in step — `tab_css(theme = "auto")`
emits an `@media (prefers-color-scheme: dark)` layer, which the `[data-bs-theme=light]` layer
after it out-specifies, so a dark-OS reader gets a light page AND light tables.

**More air between sections, in proportion to the heading's rank** (`pkgdown/extra.scss`, the
articles only — a reference page is looked up, not read through): 4 / 2.5 / 1.75 / 1.25 rem from
`h2` down to `h5`, where pkgdown stops at 1.5 and 1 and gives `h4` and below nothing at all. A
sub-section opening its parent keeps the small gap. Margins collapse, so each value is the whole
gap rather than an addition to the paragraph above.

**Measured.** `pkgdown::check_pkgdown()` clean; shipped suite **FAIL 0 | PASS 4468**. One NEWS edit,
approved: the *Introspection accessors* bullet no longer names `tab_supports()`. The jamovi titles
stay `Crosstables` / `Regressions` — they are generated from `jamovi/*.a.yaml` `title:`, the field
that also labels the ribbon item and the results heading, so renaming them would cost a
`jmvtools::install()` regeneration and a new msgid for a line the section heading already carries.

#### Phase 24e — ggfacto reverse dependency ✓ DONE

**The visible failure was the first of three.** `R CMD check` of CRAN `ggfacto` 0.3.2 stopped at
`'set_type' is not an exported object`; behind it sat `fmt(type = )` in `pca_interpret()` (exported
in 0.3.2, live example) and in `benzecri_mrv(fmt = TRUE)`, and behind those two real tabxplor
defects the revdep exposed. Everything below was measured, never assumed.

**The `type` vocabulary is translated, not refused** (`R/tab-deprecate.R`, whose scope line now says
so). `type` conflated two facts; the map back onto the `(scale, pct_type)` pair is stated ONCE, in
`fmt_type_legacy()`, and read by all three entry points that still admit the old word:
`set_type()`, `get_type()` and `fmt(type = )` — soft-deprecated, defunct in 2.1.0, on the Superseded
group of the Reference page (`?tabxplor-type`). `set_type()` writes through the validating setters,
never `attr<-`, so a shim cannot become a laxer way in; `"mean"` / `"n"` / `"coef"` also reset
`pct_type` to `"none"`, since 1.x, having one attribute, could not have claimed a percentage there.
`get_type()` is the LOSSY way back and says so in its own description: every effect scale reads
`"coef"`, a distinction 1.x could not make. ⚠ `ci_type` still aborts — the interval is always on the
estimate's own scale, so there is nothing to route — and `fmt()` refuses `type` alongside an explicit
`scale`/`pct_type` rather than silently picking one. The translation happens BEFORE `display` is
forced, `display`'s default being a promise reading `scale[1]`, which is what makes
`fmt(pct = , type = "all")` still come out as a percentage.

**Release blocker: an NA cell crashed the colour engine.** `dplyr::bind_rows()` NA-fills a `fmt`
column absent from one of its inputs, and EVERY field of those cells comes back NA — 4 lines to
reproduce, nothing to do with ggfacto. `fmt_color_slots()` then evaluated `any(is_wn)` on an NA and
the whole print aborted. The two display comparisons are guarded, and the sweep for siblings found
the real root: `is_totrow()` / `is_refrow()` / `is_tottab()` returned NA, poisoning the masked
assignments in `format()` and the `bold`/`keep_black` masks in `tab_export_prep()`. **An unknown row
is not a total row**, so the three predicates fold NA to FALSE at the source — one fix instead of
one per consumer, and it is the invariant `leaf_totrow_tottab()` already stated.

**A `mixed` column no longer grades a cell it cannot read.** `mixed` is what reconciling unlike
columns gives. It carries ONE ladder, so a mean difference was read on the percentage-point ladder
and a −1.9 landed at the deepest slot. A MULTIPLICATIVE measure needs nothing — `pct_ratio` and
`mean_ratio` carry the same over-breaks, so a ratio is the one comparison unlike quantities state
alike; an ADDITIVE one gates out the cells that are not on its ladder (the same per-cell lever the
blank/gof cells already use), keeping their number and losing only the shade, and
`tx_inform_once()` names `color = "ratio"`. ⚠ Data is never touched: the `diff` field is read by the
tooltip, `get_num()` and Excel. The 2.1.0 follow-up is recorded at the line: build the plan once per
family and pick per cell, which also needs the footer legend to print two measure lines for one
column.

⚠ **A latent trap in `tx_inform_once()`, found writing that message**: its `id` formal sits before
`...`, so `tx_inform_once("id", "i" = ...)` had the `"i"` bullet **partial-matched as the id** —
the line silently dropped and the id printed in its place. The formal is `.id` now, which no bullet
name can reach. The 20 existing call sites all passed one `c(...)` vector and were unaffected.

**`tab_transpose()` is a supported reshape operation again**, no badge, no warning. It has a job
nothing else does: a **profile table** (many variables down the page, few groups across it), and it
is the ONLY way to put a mean on a row — a number given to `row_vars` is always cut into levels.
Its `@description` says what it IS rather than what to use instead, with a section on columns of
unlike kinds; `tab_export(transpose = )` stays the recommendation where only the output matters.
⚠ **A transposed column IS a bind** — it stacks one cell from each source column — so it now claims
only what its parts agreed on, through the SAME declared reconcile every `bind_rows()` uses
(`fmt_attrs_merge()`, driven by `fmt_attr_rules`), instead of copying one representative column's
attributes onto all of them. That is what used to hand a mean cell a percentage column's scale and
ladder, and it is what makes the gate above fire. `old_base` is still read from the representative:
which percentage the table held is a fact about its AXIS, and merging with a mean column
(`pct_type = "none"`) would lose the row% ↔ col% flip. Measured: a homogeneous transpose is
byte-identical to the native table it mirrors.

**`as.matrix()` / `as.table()` hand a table to base R** (`R/tab_classes.R`). One shared internal,
two thin methods, and one decision the user should not have to remember: **a table's own margins are
not data**. A row survives iff at least one of its cells says `row_kind == "data"` — which drops the
Total row AND the display-time n / pct / p-value / gof rows in one test, and keeps an ordinary row
whose cells are partly NA — plus `!is_tottab()`, since `totaltab = "table"` writes a total table's
rows as ordinary ones. Total columns go, every cell contributes the number it SHOWS (`get_num()`),
and the label columns become dimnames through the same `as_df_merge_rownames()` the `df = TRUE`
leaves use, so a stacked several-`row_vars` table degrades predictably (`race_Other`). `totals =
TRUE` keeps everything. ⚠ `as.data.frame()` is deliberately NOT given a method: tibble and dplyr
call it internally on their own objects, and nothing in the tidyverse calls `as.matrix()`/
`as.table()` on a data frame in a load-bearing way.

**Measured.** Shipped suite **FAIL 0 | PASS 4548** (+80), in ~40 s. New assertions in their
subsystem homes: a new `test-tab-deprecate.R` (the seven `type` round trips as a declared map, the
three retired spellings of "no type", the `ci_type` refusal, the conflict, and the `.id` formal),
`test-fmt.R` (an NA-filled cell through `format()`/`print()`/html/tooltips/markdown, and the three
predicates), `test-tab-color.R` (the mixed column under both measures, and a homogeneous one
untouched), `test-tab-transpose-render.R` (not deprecated; a mixed round trip; a homogeneous mean
profile keeping `level_mean`) and `test-tab-classes.R` (the two coercions, `totals = TRUE`, the
display rows, the total table, several label columns, the empty case). **No golden moved.**

**The revdep, end to end.** CRAN ggfacto 0.3.2 built from its own release commit and checked against
this tabxplor in a throwaway library: **Status: OK** — `HCPC_tab()`, `pca_interpret()` and
`benzecri_mrv()` all run.

**ggfacto modernised for its next version** (a separate repo, `Imports: tabxplor (>= 2.0.0)`).
`HCPC_tab()` is built the way round it is READ — the variables' levels down the page, the clusters
across it — because `tab()` stacks several `row_vars` into one table by itself; the vendored
`tab_transpose()` copy, `set_type()`, the `n`-column surgery, the `Total_` rename and the
`$display == "mean"` branch are all gone, and so is the duplicated `n` row they produced. Numeric
variables keep a transpose, in their own homogeneous block. The two summary rows come from their own
one-variable table (`tab(data, clust, pct = "all") |> tab_transpose()`) and are declared as DISPLAY
rows carrying the table's own scale and colour — ⚠ a block claiming anything else reconciles BOTH
away on the bind, which is how every column came out `mixed` and uncoloured in the first attempt.
`pca_interpret()` colours a coordinate by its SIZE on the standardized ladder instead of the old
`diff = 3` / `1/9` sign hack. Three ggfacto bugs fell out of the testing: `ggmca(active_tables = )`
gave every level an empty tooltip; a level named like its own variable lost its tooltip (tabxplor
appends `_lv` to it, and the plot never sees that rename — undone through `fct_relabel()`, ⚠ because
everything downstream picks the tooltip pieces out by `is.character()` and a character `lvs` is
nested away with them); and a tooltip's `Frequency` line could read above 100 %, being divided by the
last row of the last table instead of by the population. 25 of 25 example paths green; its own
`R CMD check` is clean but for the pre-existing NEWS-heading NOTE.

⚠ **One maintainer step remains**: ggfacto's own release (version bump, `README.md` re-knit from
`README.Rmd`, CRAN submission) is not part of this phase.

#### Phase 24f — rhub and win-builder.r-project.org failures ✓ DONE

**The rhub red was never ours, and the evidence is one column of the log.** `clang19` / `clang20`
died at `.onLoad failed in loadNamespace() for 'vctrs' — symbol bindings not supported yet`, inside
vctrs's own `.onLoad`, during `R CMD build`'s install step, before any tabxplor code runs. Those two
images carry **R-devel r89629 (2026-03-15)** while every job that passed — `ubuntu-clang`,
`ubuntu-gcc12`, `ubuntu-release` — runs **r90452 (2026-08-27)**; the binary they install is
`vctrs 0.7.2`, released *after* that snapshot, and the error string exists in neither local R 4.6.1
nor R-devel r90246. So every package importing vctrs fails there identically. ⚠ The wider point,
now in `dev/release_checklist.md`: **tabxplor has no `src/`**, so `clang*` / `gcc*` / `c23` / `lto` /
`*-asan` / `valgrind` / `rchk` exercise a toolchain the package never uses. The platforms that vary
the RUNTIME are the ones worth spending: `nosuggests` (25 Suggests behind `tx_need_pkg()`), `nold`,
`atlas`, `mkl`, `donttest`, `ubuntu-next`, `ubuntu-release`. ⚠ On the run that followed, five of
those passed and `nosuggests` sat silent for **5h57m** and died at GitHub's 6 h limit — infrastructure
again, and provably: its mechanism is `_R_CHECK_DEPENDS_ONLY_=true`, which `tools:::.check_packages`
ALONE reads, so it is inert during the `R CMD build` the job hung in — the very build its twin image
`atlas` (same Fedora 42, same R-devel r90185) finished in **89 s**. But the variable bites at CHECK
time and so rehearses locally in one `withr::with_envvar()` -- and doing that found **three real
gaps the stalled job would have reported**, now fixed and the run **0/0/0**. What it hides is
`Depends + Imports + VignetteBuilder`, plus testthat for the tests step (`tools:::setRlibs`), so:
`VignetteBuilder: knitr` whitelisted knitr but not **rmarkdown**, which every vignette's
`output:` needs -- it is `knitr, rmarkdown` now, that field being R's own list of what a vignette
build may reach; two tests called the Excel exporter with no `skip_if_not_installed("openxlsx2")`
while their 26 siblings had one; and the ANSI-to-html knitr hook all five vignettes install called
`fansi::` unguarded, so four of them died on their first console table -- it now degrades to
stripped ASCII through one `ansi_html()`, and the one `forest_plot()` chunk that actually evaluates
takes `eval = requireNamespace("ggplot2")`, the idiom every Rd example already used. Every Rd
example passed untouched. The rehearsal is now a checklist step.

**win-builder's ERROR and WARNING were one cause: the manual is LaTeX.** Seven `Unicode character
not set up for use with LaTeX` — σ once, ⚠ six times — and nothing else in `man/` was at fault
(`—`, `…`, `×`, `÷` are all set by utf8 inputenc). It had never been seen locally because
**`devtools::check()` defaults to `manual = FALSE`**; with tinytex on this box the failure and its
fix both reproduce in one `R CMD Rd2pdf` (before: the same 7 errors, `Error in running
tools::texi2pdf()`; after: **103 pages, 0 errors**).

The two glyphs are not the same kind of thing, so they are not treated alike. **⚠ is decoration**:
dropped from the six user-facing Rd sites (five `@param` in `R/tab_reg.R`, the `tabxplor.shape_table`
`doc` string in `R/tab-options.R`, whose field `tab_options_rd()` alone reads), capitalising what
followed — the sentences already carried their own bold. Comments, `dev/` and this file keep it.
**σ is content**: `mean_sd` really prints `3.5 (σ1.2)` and `sd_bands` labels the same glyph, so
`display_presets_rd()` writes the template twice, `\ifelse{latex}{\code{… (SD{sd})}}{\code{… (σ{sd})}}`.
⚠ Verified in `tools:::Rd2latex` rather than assumed: it honours `\ifelse`, while **`\enc{}{}` falls
back only when the whole output encoding is ASCII** — the obvious mechanism, and the wrong one here.
⚠ And the split goes **around the code span, not inside it**: `\ifelse` is a text tag, so the first
attempt (`\code{… \ifelse{latex}{SD}{σ} …}`) built the PDF but traded the ERROR for
`checkRd: (7) Tag \ifelse is invalid in a \code block` — caught by running the gate, not by any
grep over the Rd.

**Three defects behind the HTML NOTE, all in `jamovi/jmvtab.a.yaml`.** A `description: R:` block is
**Rd**, and jmvtools reflows it. `ci` was written with a raw `<b>`, which roxygen turns into
`\if{html}{\out{<b>}}` and `Rd2HTML` opens *before* the paragraph — tidy's three warnings at
`man/jmvtab.Rd:165`; it is `\strong{}` now. `conf_level`'s reflow put `1.` at a line start, so
roxygen markdown made a numbered list of it: the param read *"between 0 and 1"* then a bullet, with
a stray `\%` — reworded so no wrapped line can begin `<digit>.`. And `R/tab.R:87` wrote `\%` in
HAND-WRITTEN roxygen, which escapes the backslash in turn and printed `10\% level` in `?tab` and
`?tab_ci` (one line, both pages, since `tab_ci` inherits the param). ⚠ **The opposite rule holds
inside an `@eval` doc string** (`R/tab-args.R:340`), inserted as raw Rd, which correctly writes
`\\%` — `man/tab.Rd:221` already rendered right. `jmvtools::install(home = "flatpak")` regenerated
`R/jmvtab.h.R` (3 lines) and left `inst/i18n/fr.json` byte-identical, the reworded prose being R
documentation and not a msgid.

**The URL NOTE is half a bug and half a schedule.** Measured: `www.jamovi.org/download` is **404**
and `/download.html` a **302 to the homepage** — the page is gone — so all eight links (5 vignettes,
`_pkgdown.yml`) now point at `https://www.jamovi.org/`, 200 with no redirect. The eleven
`bricenocenti.github.io` 404s are not fixable in code: `gh api …/pages` returns 404 and there is no
`gh-pages` branch, so the site has simply never deployed. ⚠ Submit only after the merge has
deployed it — now a Notes bullet in the release checklist.

**The guard is in `test-non-ascii.R`, whose rule this is the second half of.** Two blocks scan
`man/*.Rd` — an allow-list of the four glyphs LaTeX sets, and a ban on the double-escaped percent
R CMD check never mentions — both skipping where `man/` is not next to the tests, exactly as the two
R-source blocks do. ⚠ What must be ASCII is **what LaTeX is handed**, not what the file contains, so
the scanner asks `tools::Rd2latex` rather than imitating it: a regex resolving `\ifelse{latex}{A}{B}`
worked until the arms grew braces of their own, and would have had to grow `\if{html}` and `\enc`
too. The converter settles all three, and costs 0.9 s over 114 files. A third block proves it is not
vacuous, mirroring the file's existing scanner self-test: a bare σ and a ⚠ are caught, the wrapped
σ and the four allowed glyphs are not.

**Measured.** `devtools::check(manual = TRUE)` **0 errors, 0 warnings, 0 notes** in 3m11
(`checking Rd files`, `checking PDF version of manual`, `checking examples with --run-donttest` and
`checking HTML version of manual` all OK). `R CMD Rd2pdf` alone: 103 pages, 0 LaTeX errors, against
the same 7 before. ⚠ The HTML NOTE needed **HTML Tidy installed** to be verifiable at all — without
it R CMD check skips that step and says so, which is why it was checked directly too: `Rd2HTML` +
`tidy` over the whole manual, **0 findings on 114 pages**. `pkgdown::check_pkgdown()` clean; shipped
suite **FAIL 0 | PASS 4554** (+6, the new blocks) in ~40 s, the one WARN and two SKIP pre-existing
and environmental. No golden moved, and none could: every edit is roxygen prose, a doc generator, a
YAML description, a URL or a test — no number and no rendered cell is reachable from any of them.

⚠ **Maintainer steps remain**: `devtools::check(manual = TRUE)`; merge PR #3 and enable Pages;
re-run win-builder and rhub (without the compiler containers); then finish `cran-comments.md`, which
still claims "no NOTEs" everywhere and needs the real run links and whatever the fresh runs say.



#### Phase 24g — what the courses' migration audit asks of the package ✓ DONE

A 1.3.1-era corpus of university courses was audited against 2.0.0 (`dev/formations_stat_migration.md`,
a one-off audit that moves to `dev/archive_2.0.0/` once the port is done). Most of what it found is
the courses' to fix. This phase was the package's part — seven places where 2.0.0 broke a contract
1.3.1 honoured or said something in a comment the code did not do — plus **Quarto citizenship**,
because the courses are moving to `.qmd`. No redesign: every item landed in a declared table or a
boundary that already existed, and two of them REMOVED an inconsistency rather than adding a lever.

**A caption is a fact about the table, so a producer states one.** `caption` was an `EXPORT_ARGS` row
only, so the sole way to attach one at build time was `set_caption()` — backwards, `subtext` being a
`tab()` argument. One new `TAB_ARGS` row beside `subtext` (`producers = c("tab", "tab_reg",
"tab_counts")`) and one write per producer, through the existing `set_caption()`, which stays the only
writer of `meta$spec$vars$caption`; `rd_caption()` is untouched, so an exporter's own `caption =` keeps
precedence. `tab()` and `tab_reg()` gain a formal, `tab_counts()` **none** — it binds its whole surface
off `...` through `tab_dots_expand()`, so declaring the producer is the entire wiring. ⚠ The write sits
**before** `as_tabxplor_tabs()` / `new_tabxplor_tabs()` in all four returns: `set_caption()` maps over a
list and `purrr::map()` hands back a bare one, so the re-class after it is what keeps `output_list =
TRUE` and a multi-outcome `tab_reg()` their class. It is skipped on `.return_armed`, the jamovi
live-cache seam, which returns mid-build and never passes one. `set_caption(x, NULL)` is
attribute-identical to no call at all, so the unconditional call costs nothing and moved no golden.

**The caption's markup is host-aware — one rule, three hosts.** `bookdown:::parse_fig_labels()` numbers
a table only where a line matches `^\s*<caption`, and 2.0.0 emitted the title as a `<div>` sibling: so
**every captioned table in a bookdown document kept the raw label in its title, registered no anchor,
and every `\@ref(tab:…)` rendered `??`** — while tabxplor's own instructions name
`bookdown::html_document2()` as the required target. `tx_caption_host()` (`R/tab-render-html.R`) now
reads the two flags the ecosystem itself uses, through the existing `tx_knitr_opt()`: a real
`<caption>` under bookdown (`bookdown.internal.label`), **nothing** under Quarto when the cell wrote
`tbl-cap` (`quarto.version` + the cell option), the `<div>` everywhere else — including the Viewer,
`tab_export(file =)` and jamovi, where `tx_knitr_opt()` answers NULL.

⚠ **Three facts measured by rendering, not read off docs, and each now a `# WARNING:` at its line.**
(1) **The inner element must be a `<span>`, never a `<div>`** — the obvious shape, and the wrong one:
bookdown's scan runs on the POST-pandoc html, and pandoc's writer gives every *block* tag a line of its
own, pushing the label two lines below `<caption>` and out of the `content[i - 0:1]` window; a `<span>`
stays on the text's line and still carries the width guard, `display:block;width:0;min-width:100%` (the
`.tx-foot` idiom) being what stops a long title sizing a narrow table. (2) **`caption-side:top` is
load-bearing**: Bootstrap puts a caption at the BOTTOM and `tx_html_deps()` injects Bootstrap into every
knitted document, so without `.tabxplor-tab>caption{caption-side:top;padding:0;margin:0;}` the bookdown
arm's title would sit under its table. (3) Pandoc **unescapes** `\#` → `#`, which is why the token
bookdown greps for is `(#tab:x)` and nothing in R must touch it (`tx_html_escape()` cannot: it holds no
`& < >`). ⚠ A bare `label: tbl-x` with no `tbl-cap` still numbers a Quarto table, so tabxplor's own
title is kept there — only `tbl-cap` stands us down.

**Quarto citizenship.** Every `<table>` tabxplor emits opens through one `tx_table_open()` carrying
`data-quarto-disable-processing="true"` — the engine, the shape table and the degrade path, so a fourth
site cannot forget it. Measured on **Quarto 1.10.18 + knitr 1.51** through the real `asis_output` path:
without it the class comes back `tabxplor-tab cell caption-top table table-sm table-striped small` with
a `<tr class="odd">`, `table-striped`'s zebra fill fighting colour-coded cells; with it the markup
passes through byte-for-byte and cross-references still resolve. ⚠ It reproduces only on that path, not
on `cat()` under `results: asis` — and it reaches the html engine only: `tab_md()`'s table is generated
by pandoc from a pipe table, where `html-table-processing: none` is the user-side lever. The **raw-HTML
fence** is a `# WARNING:` at `tab_kable_join()`, the one producer of the final string: Quarto fences
asis output as `{=html}` only when it matches `^<\w+[ >]` and ends `</\w+>\s*$`, and a leading HTML
comment would be enough to have the whole thing parsed as markdown instead.

**The exporters refuse what they cannot use.** `tab_html()`, `tab_md()`, `tab_xl()`, `tab_css()` and
`forest_plot()` each called `tx_deprecate_inert()` and **discarded its return value**, then never looked
at `...` again — so `position =` (519 sites in one corpus), `n_min =` and every typo were accepted in
silence, while `tab()` errored with a did-you-mean. All five now go through one named operation,
`tx_export_dots()` — filter the retired names, then hand what is left to the same `tab_check_dots()`
the producers use. It is one call because doing either half without the other is exactly what went
wrong, and the error names the user's own call
(*Unknown argument `colwith` in `tab_xl()`. i Did you mean `colwidth`?*). Its `known` set widens from the declared rows to **the declared rows PLUS the
producer's own formals**: neither is the whole answer on its own, the grid being wider on `tab_counts()`
and the signature wider on an exporter, where `EXPORT_ARGS` declares only the rows whose prose it needs.
It is provably a no-op on the five crosstab producers, whose formals `tx_check_tab_args()` already
asserts are declared. ⚠ `tab_export()` is deliberately left permissive — its `...` really is a
pass-through and the leaf validates it; a check of its own would refuse a legitimate `css =`. ⚠ And an
ABBREVIATION still partial-matches on an exporter, whose `...` is written last, so only a real typo
lands in the dots — the opposite of `tab()`. The change caught **four latent typos** on its first run,
all in `dev/tests/` (`tab_kable(tooltip =)`, three `tab_html(print =)`), plus one in the shipped suite.

**Deprecations that fire, and a promise the code keeps.** `tx_deprecate_inert()` now takes `user_env`,
**with no default** — because no default is right: lifecycle's own lands on that function's frame, an
obvious one would land on the exporter's, and for either lifecycle sees an internal caller and says
NOTHING AT ALL. That is how five retired arguments went a whole release without ever warning. It has to
be `rlang::caller_env()` read in the exporter's own body, the one frame that names the person who typed
the argument — and **having no default is the guard**, since a caller then has to decide. ⚠ The silent
half is not assertable in the suite: under testthat, `deprecate_soft()` warns for the package under test
whatever frame it is given, so what the test locks is that a frame must be given at all. `position`, `n_min` and `hide_near_zero` join
`TX_INERT_EXPORT_ARGS`. `$rr` gains the read branch its own comment at `R/fmt_class.R` already promised,
beside `ci` / `tot_wn` / `in_totrow`; `mutate()` stays permissive. ⚠ `normalize_color_spec()`'s
`caller_env(2)` **stays two frames short on purpose** — a `# WARNING:` at the line says so: the colour
aliases it remaps compute identical numbers, so a message on the commonest argument there is would be
pure noise.

**`get_test()` is public, `get_chi2()` is shimmed.** `get_chi2()` was 1.x's only programmatic route to a
table's test and the one removal in the release with neither a shim nor a `NEWS.md` line; it is back as
an unexported soft-deprecated alias returning `get_test(x)` — the spelling the corpus uses is `:::` —
naming the two fields that moved (`df` → `df1`, `count` → `statistic`) and pointing the per-cell rows at
the cells' own `ctr` field, `get_ctr()` being internal. `get_test()` gains `@export` and a page stating
what the attribute IS — one tidy row per test, keyed `var` / `col`, a new kind of test being new ROWS —
without restating the column list `new_test_tibble()` owns. ⚠ A table that ran none carries the **empty**
tibble, same columns, not `NULL`. `set_test()` stays internal: `test` is row-bound and the vctrs
reconcilers `vec_rbind` it, so `new_tab(test =)` is the validating writer. `_pkgdown.yml`: `get_test`
joins *Inspect a table*, "Captions and options" becomes **Options**, and `set_caption` joins the block
beside `new_tab` / `fmt_attr`, retitled *The type system* to cover the table's own attributes.

**`NEWS.md` accuracy pass** — corrections only, no new bullet. `broom` is not a dependency at all;
`get_n()` is internal in both versions, so the base count reads `Total$n`; of the six functions listed as
removed only `tab_plot()` and `kable_tabxplor_style()` were ever exported in 1.3.1 (checked against
`b812c5f:NAMESPACE`), the other four being `master`-only non-events; `print.tabxplor_kable()`, not
kableExtra's, is what opens the Viewer page; and the inert export arguments move out of *Removed (now an
error)*, where none of them ever belonged, into *Soft-deprecated* with the three new names and the
strict-dots rule beside them. `caption =` replaces the `set_caption()` mention on the html line, so the
new argument is stated without a new bullet.

**Measured.** Shipped suite **FAIL 0 | PASS 4632** (+78) in ~40 s, the one WARN and two SKIP
pre-existing and environmental. Two snapshots moved, each verified line by line before acceptance:
`_snaps/tab-render-html.md` for exactly four `<table>` tags, `_snaps/golden.md` for exactly the two new
CSS rules across its 16 style blocks. The structural `_golden/*.rds` did not move and could not — no
number is reachable from any of this. New assertions in their subsystem homes:
`test-tab-render-html.R` (the three hosts, bookdown's own line-shape predicate, the attribute on all
three emit sites, the fence invariant over five shapes, the two CSS rules), `test-tab-args.R` (the row,
the three producers, `NULL` as a no-op, the class kept through a list, the widened `known` proven a
no-op on the producers), `test-tab-export-prep.R` (the fallback chain), `test-tab-deprecate.R`
(`get_chi2()`, all five exporters refusing and warning, `tab_export()` still forwarding),
`test-utils.R` (`tx_deprecate_inert()`, which had no test at all — which is how the `user_env` bug
stayed invisible) and `test-fmt.R` (`$rr`). ⚠ Two of them had to be rewritten once the run was
parallel: `deprecate_soft()` warns once per session per message, so a test naming a real exporter's
retired argument passes or fails on which file ran first — the probes use a label of their own. ⚠ **Verified by rendering, twice**: a
`bookdown::html_document2` document where a `tab(caption =)` and a `set_caption()` table both get
`Table 1.1:` / `Table 1.2:`, resolving `\@ref(tab:…)` and leaving no raw token; and a `.qmd` through
the Positron-bundled Quarto where the `tbl-cap` cell shows exactly one caption, the `label`-only cell
keeps tabxplor's title and is still numbered, and neither table is restyled.

⚠ **Out of scope, recorded so it is not re-derived**: whether `tx_html_deps()`'s bootstrap-cosmo
restyles a `bookdown::gitbook` book (`dev/formations_stat_migration.md` §5.7), and shipping the
stylesheet as an `htmlDependency` instead of a `<style>` per table — a dependency is per DOCUMENT and
`theme =` is per CALL, so it needs a design, not a patch.

#### Phase 24h — PR to master and pkgdown site online ✓ DONE

**`master` carries the release, and the site is live.** `master` is `725cc6c`, the merge commit of
PR #5 (`release/2.0.0` → `master`); https://bricenocenti.github.io/tabxplor/ answers from the
`gh-pages` branch the pkgdown workflow wrote on that push. The whole phase is `dev/release_checklist.md`
steps 1–4 plus the once-per-repo Pages setup — no package code changed, and no golden moved.

**The July branch was unusable, so it was re-cut rather than refreshed.** `release/2.0.0` dated from
2026-07-23, sat 168 commits behind `dev`, and its tree **predates `data/`** (the four example data sets
arrived in Phase 22l), so the package on it could not build its own examples; every check on PR #3 was
red from July. PR #3 was closed with that reason recorded, the remote branch deleted, and the branch
cut afresh from `dev` — one strip commit (`71d6f1b`, 465 deletions), exactly the checklist's step 2.
⚠ Re-cutting costs a `git branch -D` in the maintainer's own terminal, twice (the name is held by the
stale local branch before, and by the merged one after), and the second one fails with
`cannot delete branch used by worktree` unless the checkout is moved off it first.

**Two proofs replace reading the diff.** `git diff --name-only refs/heads/dev HEAD --` filtered of the
strip list must be empty, and `git ls-files -- dev .claude .vscode CLAUDE.md air.toml` must be empty:
together they say the release tree IS `dev` minus the strip list, which is what makes "dev-green means
release-green" a fact rather than a hope. Both held on the strip commit and again after the mid-flight
merge, with the tree objects of `R/ man/ tests/ vignettes/ data/ inst/ po/ jamovi/ pkgdown/` byte-identical
to `dev`'s and `.Rbuildignore` identical on both branches. ⚠ The first proof needs `refs/heads/dev`, not
`dev`: on a release branch the name is a revision AND the directory just stripped, and git refuses the
ambiguity.

**The second suite had never been updated after 24c/24e/24g — 23 failures, every one stale.** It is not
run per-edit, so three phases of deliberate change had accumulated in it while `tests/testthat/` was kept
current: the signed CI bounds (`[-12;+0]%`, 3 sites), `min_digits` as a floor on a foreign token,
`rd_link_text()`'s two-syntax rewrite and the ordinal shape row (5 sites), regression footer rows no longer
bold, the Excel width constants (2 sites), `data-quarto-disable-processing` on every `<table>` (2 sites),
the `width:0;min-width:100%` idiom moving from `.tx-foot` to `.tabxplor-caption` (2 sites), the dark-theme
ground rule, `quarto` now appearing in the light stylesheet, the `tx-sec` aside rule, and `tab_transpose()`
no longer deprecated. All deleted (35 lines, 10 files) rather than updated — the shipped suite already
locks each of those facts in its subsystem home. Two exceptions: `tab_transpose()`'s block kept its live
`expect_error(xpose(42))` and was retitled, the shipped suite covering only the not-deprecated half; and
one comment was rewritten where the assertion it described was the one deleted. ⚠ The `pvalue` case is
**not** a defect — `po/R-fr.po` translates `pvalue (%s%s)` to itself, the field name kept as notation by
translator choice, exactly as the tooltip's words are. Result **FAIL 0 | PASS 5885**.

**The shipped suite's one WARN was a warning leaking out of a test that was not about it.**
`test-tab-reg.R:370` asserts `reg_formulas()$fit`, and `tvhours ~ race` under Poisson genuinely is
over-dispersed — the package was right and the test wrapped only `suppressMessages()`, which does nothing
to a warning. Its two siblings making the identical fit (`test-tab-reg.R:96`, `test-tab-color.R:277`)
already had `suppressWarnings()`; line 370 was the one that missed it. **FAIL 0 | WARN 0 | PASS 4632**.
The two SKIPs stay: one needs a 24-bit-ANSI terminal, the other is the `svyVGAM`-not-installed fallback
and so can only run where the package is absent.

**The pkgdown PR check found the one thing nothing else could.** `R CMD check` never sees
`vignettes/articles/` (`.Rbuildignore`d) and pkgdown never runs on a `dev` push, so the PR was the first
build of the five French articles since the 24b rename — all 11 built, `txtheme 0.1.0` resolved from
GitHub (`@14f0c99`), both its assets copied, `site_prune.R` correctly skipped, deploy correctly gated off.
It also surfaced **three shipped vignettes whose `\VignetteIndexEntry{}` no longer matched their title**:
Phase 24d retitled `tabxplor.Rmd`, `tabxplor-reg.Rmd` and `tabxplor-reading-a-regression.Rmd` and updated
`_pkgdown.yml`'s navbar, but not the index entries. `R CMD check` does not fail on it — which is why all
five platforms and `check(manual = TRUE)` were green — yet `\VignetteIndexEntry{}` is exactly what
`vignette(package = "tabxplor")` and CRAN's package page display, so 2.0.0 would have shipped two names
per document. Fixed on `dev`, merged into the release branch, CI re-run: the 8 title warnings became 5,
and those five are the French articles correctly having no index entry at all, being articles and not
vignettes.

**Pages: deploy from a branch, `gh-pages` at root.** That is what the workflow already writes
(`JamesIves/github-pages-deploy-action`, `branch: gh-pages, folder: docs, clean: true`) and what the r-lib
template regenerates, so it costs no workflow change; the "GitHub Actions" source would mean rewriting
`pkgdown.yaml` around `upload-pages-artifact`/`deploy-pages` with a `github-pages` environment, and
`master`/`docs` cannot work at all — `docs/` is git-ignored by design. ⚠ The branch only exists AFTER the
first deploy, so Pages is enabled after the merge, never before. The site is then stable by construction:
the deploy step is gated `if: github.event_name != 'pull_request'`, so a `dev` push and a PR never touch
it — only a push to `master`, a published release, or a manual dispatch — and `clean: true` means each
deploy fully replaces the branch, so nothing stale accumulates.

**Verified live, and this is the gate Phase 25 cites.** All 11 `bricenocenti.github.io` URLs in the files
CRAN reads (`DESCRIPTION`, `README.md`, `man/`, `vignettes/`) answer **200 with no redirect** — checked
without `curl -L`, since a 301 is also a NOTE — where 24f recorded every one of them as 404. Nine core
pages, five English and five French articles all 200; `/CLAUDE.html` **404** and zero CLAUDE entries among
the sitemap's 132 `<loc>`s, the guarded prune having had nothing to do because `master` carries no root
`*.md` outside the four `pkgdown:::package_mds` already spares. The deployed content is post-24g, not a
stale build: `tabxplor-type`, `get_test`, `tabxplor-base-coercion` and `set_caption` are on the reference
index, `tab_num()` wears its superseded badge, and the example data sets are off the index while
`reference/car_salaries.html` still resolves — 24d's `internal`-section trick working as designed. Both
palettes render (62 colour-slot classes and 33 print-ready marks on the home page), every table carries
`data-quarto-disable-processing`, `extra.js` seeds `theme=light` so the site opens light whatever the
reader's OS says, and the two reworked articles each render 34 tables / 504 coloured cells / 39 legend
blocks in both languages, French accents served as UTF-8. The repo `homepage` field now points at the site.

⚠ **A `dev` branch cannot be private.** GitHub has no per-branch visibility — access follows the
repository — so `dev/`, `CLAUDE.md` and `.claude/` are public on `dev` and always have been. The strip
keeps `master` user-facing and keeps development files out of the CRAN tarball and off the site; it is not
concealment, and only a separate private repository would be.


### Phase 25 — CRAN submission

**DONE**.

#### Phase 6 — jamovi release

#### Phase 6a — jamovi release preparation








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
