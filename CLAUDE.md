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
- `tab_classes.R` — `tabxplor_tab`/`grouped_tab` S3 classes, dplyr methods, print, `tab_compact()`; the table attributes (`TAB_ATTRS`) and the palette/breaks API (`COLOR_SCALES`).
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
- `tab-legend.R` — the colour legend's SENTENCE: the spec, the two assemblers, the per-medium renderer.
- `tab-footer.R` — the REGION under a table: `FOOTER_BLOCKS`, the `subtext` template, the three kinds and their emitters, `tab_note()`.
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
| `FOOTER_BLOCKS`    | `tab-footer.R`       | The region under a table: one row per member, its `<placeholder>`, kind, and what it reads |
| `TAB_ATTRS`        | `tab_classes.R`      | The table attributes: gloss, bind rule, setter, and whether a subordinate keeps them    |
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

What a cell prints is a `{}` template over declared tokens (`DISPLAY_TOKENS`), resolved by one boundary `tab()`, `tab_reg()` and `set_display()` share, so a layout learnt on a crosstab means the same on a regression. `{est}` and `{base}` are **scale-relative** — the deviation a column estimates, and the level it sits on — which is what lets one named preset (`DISPLAY_PRESETS`) render an odds ratio, a mean difference and a percentage alike. A composite has a **primary** token, the first outside brackets: it carries the stars, it is what `get_num()` and Excel return, and it is the only part the colour paints — and its converse, a template with no token outside brackets, has no primary at all and renders whole as an aside. A token may also carry **its own precision** (`{base:1}`), which beats every declared default — digits are a display property, and the cell's one `digits` field cannot say that an estimate reads at three decimals and its aside at one. **A display is post-hoc** — every field a layout can print is populated at build, so choosing one triggers no computation and changes no number, and a token may be **derived** rather than stored (`resid`, `gap`, `sd`, `cv`, `odds`). A numeric column's default layout is `mean_cv` — the spread as a percentage of the level, comparable between columns measured in different units — chosen per column and falling back to the bare mean where a mean is not positive. The **base count** is the display-time fact both producers share: folded into the Total cell when the table rests on one population, given one `n` column per block at the right when it rests on several (a spread, a regression's groups) — and the per-block Total columns then go, holding nothing but a repeated 100 %.

### The colour system

Colour has three orthogonal axes: a **measure** (which deviation to grade — `difference` / `ratio` / `odds_ratio` / `contrib`, or the two gap measures `adjustment` / `between_groups`), a **channel** (text and/or background), and a **significance policy** (`color_signif`: `ignore` / `grey_non_signif` / `guaranteed_effect`). The engine has three layers:

1. **Palettes** (`tab-palettes.R`, which holds every one of them) — OKLCH colour ramps, hand-tuned so intensity levels stay distinguishable, in light, dark and 8-bit variants, set via `set_color_palette()`; the **chrome** beside them (`tx_chrome_hex()`: the table's own ink, the greyed-out cell, the aside — of which the *ground* is the one a rendered table does not paint, following the page instead unless `tabxplor.background` says otherwise); and, where a page has no colour, three **publication palettes** (`PRINT_PALETTES`) saying the same thing typographically — one declared grid each, a row per break slot carrying its ink, face and mark, `theme = "print_ready"` choosing between them from what the table IS. A palette is always hex **and** face: a backend must never derive "is this bold" from "does this have a hex".
2. **Breaks** — per-scale thresholds (`COLOR_SCALES`). Every ladder is the SAME ladder written in another measure at one reference cell of 50 %, so a shade means the same size of deviation whichever measure a table is read on; each declares its `quantity`, its `anchor`, whether its two `sides` mirror (only where the quantity is unbounded above), and how many loud rungs it keeps on the background channel (`bg_keep` — a fill is the corrective voice). The shape rule is checked at load.
3. **Selection** — a vectorised `findInterval` engine (`fmt_color_plan` → `fmt_color_slots` → `fmt_color_channels`) that folds each cell per side and picks the strongest matching threshold.

The measure's behaviour — raw getter, scale keys, significance source, gating — lives in its `MEASURES` row, which drives both the plan and the legend with no per-measure branches; every backend then consumes the one artifact `fmt_color_channels` produces, which is why console, HTML, Excel, Markdown and plots colour identically. **`dev/colors.md`** derives where each ladder's anchor comes from, what colour-vision deficiency requires of a palette, and why a page with no colour needs a different palette rather than a desaturated one.

**The footer legend states the measure, not the palette.** One line is `[<columns> — ]<measure named in words>: <subject> ≥ <reference> <thresholds>; <subject> ≤ …`, then one clause saying what an *uncoloured* cell means — the reverse being a tautology the cells already show. The name is a per-(measure × ladder scale) fact on the same `MEASURES` row, in two registers (`word` for the console, `word_long` for the exports), because a difference of proportions, of means and of log odds are three quantities. A colour palette names no direction — its break-words *are* blue and red — while a publication palette, whose greyscale has no diverging ramp, keeps its two face words and its two sentences. **A table may re-state those words** (`set_legend_words()`, `meta$legend_words`): the same fold `measure_facts()` already applies for the significance policy and the ladder, one layer further out, so a package grading the same ladder on another quantity — a contribution to an axis's variance rather than to a chi-squared — keeps the whole grammar and says its own nouns. ⚠ Naming only (`MEASURE_WORD_FIELDS`): never an engine fact nor a ladder glyph, because a table attribute must not change a number and an extracted `fmt` column must colour identically on its own.

### The footer

**Everything printed under a table is one region, and its TEXT is a template the table carries.** `FOOTER_BLOCKS` (`tab-footer.R`) declares it — one row per member, row order the reading order, each naming its `<placeholder>`, its `kind` (`line` / `note` / `tab` / `inline`), and *what it reads*. `tab()` writes the default template into `subtext`, so a reader sees what the footer is made of and can re-order it, interleave prose, or delete `<legend>` — which is the only per-table legend switch there is, and the only one the console can obey. A `subtext` naming no placeholder is appended to the default instead, which is what a note has always done. **The `reads` column is the load-checked edge** to `TAB_ATTRS` and the `fmt` attributes, and it *generates* `?tabxplor-footer`'s "how to change this" from `TAB_ATTRS$setter` rather than restating it — so adding a placeholder is one row, and a row is **gated on what it reads**, which is also the degradation contract: a table stripped of its metadata keeps the column-derived half of its footer (the legend, the stars key) and drops the rest, with no exception handling anywhere. Everything generated is composed at RENDER — the theme changes the ladder's length and the note's words, the medium chooses terse or prose, and a post-build `select()` or `set_display()` changes what there is to say — while a person's own line is frozen in the language they wrote it; the default template holds no prose, so nothing mixes unless it is written.

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

Display values reach the backends by one source of truth: `format.tabxplor_fmt()` renders the text for console, Markdown and HTML, and `tab_xl()` writes a number with format codes from that *same* `format(syntax = "excel")`, so a display change never needs mirroring. **Excel keeps the cell a number and puts everything else in the code**: an aside becomes a column carrying its own segment (`(n={n})`), and every literal a template writes — the stars, the brackets, a sigma, a test label — folds into the numFmt, per section. A multiplicative cell holds its **reading value**, the signed fold, so `1/2.11` reaches the workbook without becoming text; text stays a property of a *cell*, not of a column. The exports' **unit row** is the console's own type tag (`<row%>`, `<n>`), written once per **block** — `tab_col_block_ids()`, the one definition of a block, which also decides where a vertical rule falls. Colour is single-sourced too — every backend reads `fmt_color_channels`. HTML colour is a slot **class**, never inline hex, the theme living in a `<style>` block from the one CSS generator (`tab-css.R`), so light/dark and the publication palettes work by stylesheet — except `print_marks`, whose signal is cell text and so comes from `format()` like the stars. **Every html table is wrapped in a `.tx-scrollbox`** (`tx_scrollbox()`), so a table wider than the space it has scrolls instead of widening the page — one wrapper and one rule for a document, a pkgdown site, the Viewer and jamovi, which restates only its pixel cap. **A table's title is one text with three placements, decided by the host**: a `<div>` sibling, the only shape that cannot size the table; a real `<caption>` under bookdown, which numbers a table only by scanning for one; and nothing at all under Quarto when the cell already wrote `tbl-cap` — and every `<table>` tabxplor opens carries `data-quarto-disable-processing`, since Quarto would otherwise restyle a table it did not build. **A table may carry subordinate tables and notes** (`meta$footer_tabs`, written by `set_footer_tabs()`) — a fact that belongs to the table without being a row of it, such as the eigenvalues beside a factorial-analysis summary. A `tabxplor_tab` renders as a table (`tx_with_footer_tabs()` hands each exporter the LIST the table means, so the `list_method = TRUE` path renders it, a named one captioned by its name); **any other data.frame renders as a NOTE** — a grid of already-rendered character columns in the aside ink, which is what the regression's shape table now is, so its four hand-written emitters became everybody's (`tab_note()` overrides the headers, the alignment, a greyed row, a footnote or a sparkline column). **A subordinate is not a peer**: it renders what it carries and nothing generated, so a host and its subordinate show ONE colour legend, and it inherits the host's render options with no opt-out. ⚠ **In the console both print ABOVE the table** — the last thing printed is the R object you can go on to pipe — and below the footer in every export; a subordinate table takes the pipe-table shape there (`tab_pipe()`, which is `tab_md()` with three arguments fixed so the two cannot drift). **A column may be drawn as data bars** (`meta$bars`, `set_bars()`): a bar chart inside the table, each bar the cell's share of its column's largest. ⚠ Its LENGTH is the one inline `style` the html engine writes — a length is not a look, its ink is `currentColor` mixed in the stylesheet, and a class per percent would be a hundred rules. `tab-transpose-render.R` flips a finished render model (a transposed column is heterogeneous and cannot be an `fmt` column), and `tab-theme-detect.R` best-effort-detects the console's scheme — a subsystem that must never error, because a wrong guess only mis-tints.

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
- **Roadmap "DONE" summaries → `dev/tabxplor_roadmap_DONE_PHASES.md`** — the ONLY place dev history lives.

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

tabxplor's module targets both lines jamovi builds for: `jamovi/0000.yaml` declares `minApp: 2.4.0`, and the two dev machines cover the two channels — **Windows on *solid*** (2.6.44 verified, then 2.7.37 ✓ / 2.7.38), **WSL2 on *current*** (28.x). See the build-and-test policy below.

✅ **jamovi IS installed on BOTH dev machines** — flatpak `org.jamovi.jamovi`, launched with **`jamovi`** (the `~/.local/bin/jamovi` wrapper — never bare `flatpak run`, see below). The module builds with `jmvtools::install(home = "flatpak")` in ~2 min (~33 s once jamovi's R has the dep tree), and Crosstables is verified running on real data.

⚠ **This desktop WSL2 box runs jamovi 28.2, NOT 2.7.36 — measured 2026-08-30.** `jamovi --version` → **28.2.0.0**, `jamovi --r-version` → **4.6.0-x64**, commit `3cced0e1…` dated 2026-08-13, runtime and Sdk **25.08**. The installed `~/.jamovi/modules/tabxplor/jamovi.yaml` agrees (`rVersion: 4.6.0-x64`, rebuilt 2026-08-29), and `jmvtools` is **28.2**. This matches the decided policy — **Windows tests *solid*, WSL2 tests *current*** — because jamovi renumbered after 2.7.38 (the series is 28.x, not 2.8.x) and **Linux has no solid channel at all**. The series fixes the R version (2.7 → R 4.5.0, 28.x → R 4.6.0) and the frozen CRAN snapshot, so pin `jmvtools` to the app on each machine.

⚠ **Nothing is masked here any more, and the version FIELD lies.** `flatpak list` / `flatpak info` report `Version: 2.7.27` — upstream stopped updating the appstream `<releases>` block after the renumbering — while `/app/lib/jamovi/version` and `jamovi --version` both say `28.2.0.0`. `flatpak mask` prints nothing and `flatpak remote-info flathub org.jamovi.jamovi` returns the installed commit, so the app is simply current. **Verify by mechanism, never by the version field**: `jamovi --version`, `jamovi --r-version`, and `--r-version` must equal the module's `rVersion`.

⚠ **`flatpak run --devel` needs the freedesktop *Sdk*, not just the Platform**, and an update pulls only the Platform. jamovi 28.2 moved to the **25.08** runtime, so `jmvtools::install(home = "flatpak")` dies with `runtime/org.freedesktop.Sdk/x86_64/25.08 not installed` until you run `flatpak install -y flathub org.freedesktop.Sdk//25.08`. Check with `flatpak info org.jamovi.jamovi | grep -i runtime` after any jamovi update.

✅ **The six "OPEN — maintainer step: regenerate `jmvtab.h.R`" items (Phases 7a, 7e, 7g-i, 7g-ii, 7g-iii, 7h) are CLOSED** — one `jmvtools::prepare()` covered all of them, and the compiled **`uijs` blob** means those UI changes are live in a running app for the first time.

✅ **A second `prepare()` ran on 2026-08-13** (as part of `jmvtools::install()` on the laptop) and closed every `.h.R` item accumulated since — z13's `jmvtabreg.a.yaml` (`na`'s three values), z16's `jmvtab.a.yaml` (`test_robust` → the `design_effect` checkbox) and z16-iiiii's (`method_ratio` removed). **Measured against HEAD**: `design_effect` went **0 → 11** occurrences in `jmvtab.h.R` (the checkbox was declared in the YAML but absent from the stale `.h.R`, so `isTRUE(NULL)` made it **inert** — every claim in its help text was untrue in the running module), and the dead options went to zero (`test_robust` 10→0, `method_ratio` 10→0, `na = "drop_all_models"` 1→0, `ids` 13→0, `strata` 13→0, `fpc` 12→0). `inst/i18n/fr.json` is regenerated from `jamovi/i18n/fr.po` at the same time: translated strings **72 → 159**; the ~21 that disappear are stale msgids for labels renamed across phases (`chi2 = <i>(Chi2 test)</i>`, `after_ci <i>(…)</i>`), and most of the 44 still untranslated are argument **values** (`all`, `auto`, `ci`, `at`) that stay English on purpose.

⚠ **`prepare()` proved the hand-edited `.h.R` had a latent bug**, so do not hand-edit it again. `R/jmvtab.h.R` was hand-mirrored to the YAML across ~7 commits; the compiler reproduced 778 of its 780 lines but corrected `exportExcel` (`type: Action`) from `NULL` → `FALSE` **and gave it a default it lacked** — without which `tabxplor::jmvtab()` called from R throws. The never-edit rule earned its keep.

⚠⚠ **`ELECTRON_RUN_AS_NODE` — do not debug jamovi without knowing this.** Claude Code/Positron export `ELECTRON_RUN_AS_NODE=1`; flatpak passes it into the sandbox and jamovi's Electron runs as **plain node** → **exit 0, no window, no error**, and `jmvtools::install()` dies `"bad option: --install"` (rc=9). `flatpak run --unset-env=` is NOT enough (zypak re-spawns children via the host); only `env -u` on the host works — which is what the `jamovi` wrapper does. In R: `Sys.unsetenv("ELECTRON_RUN_AS_NODE")` before `jmvtools::install()`. ⚠ `jmvtools::check()` passes regardless — it never reaches Electron — so a green `check()` proves nothing here.

⚠⚠ **`R_LIBS_USER` — jamovi's R reads your library, and the old fix no longer separates them.** jamovi's flatpak bundles **its own R** and, having `filesystems=home`, it reads your `~/.Renviron` and R's default `R_LIBS_USER`. The 2026-08-13 fix was to make the path version-generic —

```sh
R_LIBS_USER=~/R/%p-library/%v      # R's own default: %p = platform, %v = major.minor
```

— which worked while jamovi was on R **4.5.0** and the system on **4.6**. ⚠ **jamovi 28.x is on R 4.6.0 and system R is 4.6.1, so `%v` is `4.6` for BOTH and they now share one library.** Measured 2026-08-30 from inside the sandbox: jamovi's `.libPaths()` is `/home/dev1/R/x86_64-pc-linux-gnu-library/4.6` **before** `/app/lib/R/library`, and it resolves `jmvcore` **2.7.35** (system) in place of its own bundled **2.7.38**, plus system `data.table` and `Rcpp`. The old crash signature (`data_table.so: undefined symbol: R_duplicateAsResizable`) will not recur — both are 4.6, so the ABI matches — but **version skew is silent**: building this study's probes, a script that let the system `jmvcore` win failed with `could not find function "RProtoBuf_new"` and succeeded once the path was pinned to `/app` — mechanism not pinned down, but the fix is the pin. There is no env-var cure (the assignment is unconditional, so `withr::with_envvar()` does not help): to get a clean read, pin the path in the call itself —

```r
.libPaths(c("/home/dev1/.jamovi/modules/tabxplor/R", "/app/lib/R/library"))
```

Diagnose in one line: `flatpak run --command=/app/bin/Rscript org.jamovi.jamovi -e '.libPaths()'` — anything outside `/app` is contamination. ⚠ This bites on **any** second R sharing a minor version.

⚠ **WSLg is in COPY MODE** (known WSL 2.7.x bug [microsoft/WSL#40618](https://github.com/microsoft/WSL/issues/40618)): windows can be slow or render blank (taskbar entry + penguin icon, `[WARN:COPY MODE]` in the title). **Not a jamovi problem** — plain `xmessage` fails identically. One-time fix, persists across reboots: `sudo mkdir -p /mnt/shared_memory && sudo mount -t tmpfs tmpfs /mnt/shared_memory`. ⚠ The bug is *unstable* — it sometimes renders fine without the mount, then regresses; a working window is not evidence the mount is unneeded.

⚠ **There are now TWO build paths, and they are not interchangeable — `.jmo` bundles are platform-specific** (migration Phase A1):

| Target                               | jamovi                                           | Checkout                                                                    | Recipe                                                                                                                                                         |
|--------------------------------------|--------------------------------------------------|-----------------------------------------------------------------------------|----------------------------------------------------------------------------------------------------------------------------------------------------------------|
| **Linux `.jmo`** (WSL, the dev path) | flatpak `org.jamovi.jamovi` **28.2 ✅ installed** | `~/github/tabxplor` — **authoritative for source**                          | `jmvtools::install(home = 'flatpak')` (setup doc §7.4; the SDK `org.freedesktop.Sdk//25.08` is REQUIRED — `flatpak run --devel` is how the compiler reaches R) |
| **Windows `.jmo`** (release only)    | Windows jamovi, **kept forever**                 | `D:\Statistiques\github\tabxplor` — **build-only: pull, build, never edit** | `options(jamovi_home='C:/Program Files/jamovi 2.6.44.0'); devtools::load_all(); jmvtools::install(); devtools::load_all()`                                     |

**A Linux jamovi cannot produce a Windows bundle**, so the Windows checkout survives *even if C3 fully succeeds* — this is not a C3-failure fallback. The rule that matters: **never edit tabxplor in both places.** Edit in WSL, pull on Windows, build there.

✅ **`jmvtools` is pinned per machine, to the app it builds for** — **28.2** on this WSL2 box (measured), **2.7.26** on Windows. ⚠ Never `install.packages("jmvtools", repos = "https://repo.jamovi.org")` — that index serves 2.7.26 **and** 28.0-28.3 at once, so a bare install silently takes the newest. Use the explicit tarball, e.g. `install.packages("https://repo.jamovi.org/src/contrib/jmvtools_2.7.26.tar.gz", repos = NULL, type = "source")` (install `node` from that repo first — `repos = NULL` resolves no deps). The two toolchains do **not** conflict: the bundled `jamovi-compiler` in 2.7.26 and in 28.2 both hard-pin `jms: '1.0'` and both accept `jas` in `(1.1, 1.2]`, so the git-tracked `jamovi/*.yaml` stay valid for both.

⛔ **Flathub keeps only ~5 commits, so old jamovis cannot be reinstalled.** 2.6.44 is long pruned, and 2.7.36 has since gone the same way — this box was updated to 28.2 on 2026-08-13. **2.6- and 2.7-solid compatibility is verified on Windows only**, via the build-only Windows checkout, which is kept forever for exactly that reason. Check `flatpak remote-info --log flathub org.jamovi.jamovi` before assuming any given version is still installable.

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







## tabxplor development roadmap

Currently implementing tabxplor 2.0.1. **Update the sections below at the end of every work session.**

#### Verification (every phase)

- **Byte-identity**: `devtools::test("~/github/tabxplor")` after each phase; `test-golden.R` + `test-export-parity.R` + `test-fmt-contract.R` + `test-fuse-parity.R` stay green. Intentional output changes → rerun `dev/make_golden.R`, review the `_golden/`/`_snaps/` diff consciously, `testthat::snapshot_accept()`.
- **Release gate**: `devtools::check()` (~3 min, run manually by maintainer) before CRAN.

---

### v2.0.1 — Phase 1 — `display = "odds"` **DONE**

Asked for by `formations_stat`'s M2 logit lesson, which teaches percentages -> odds -> odds ratios and
had no way to print the middle rung: 1.3.1 hijacked the `rr` field, retired in 2.0.0, and
`set_display("rr")` now falls back silently and prints wrong numbers.

**Derived, not stored.** The odds ARE `pct / (1 - pct)`, computable from one field of one column, so
`odds` joins `resid`, `gap`, `sd` and `cv` as a token with `field = NA` — no 22nd `fmt` field, no
golden churn. Three insertion points: the `DISPLAY_TOKENS` row, one arm in `get_num()` guarded by
`any()` so a display that never asks for odds pays nothing, and one entry in `fmt_mult_plan()`'s
`tok` map. `settable = FALSE`: writing an odds back would mean writing a percentage.

**It borrows the odds ratio's ladder** (`comparison = "odds_ratio"`) rather than naming a measure of
its own — an odds is a level, not a comparison, so it is not `bare` and stays out of
`DISPLAY_MEASURE_TOKENS`, whose `stopifnot` admits one token per measure. `geometry = "ratio"` gives
the pillar header `<row%-odds>` for free. Void where `pct` is 1, declared like `cv`'s and `moe`'s
voids rather than special-cased at render time.

**Watch the `%`**: a `doc =` string reaches the Rd through `@eval`, where a bare `%` comes out
double-escaped and `test-non-ascii.R` catches it. Say "the percentage is 1", not "100%".


### v2.0.1 — Phase 2 — l'italique de la légende **DONE**

Une ligne, `.tabxplor-caption{font-style:italic}`, ajoutée à `tab_css()` (`R/tab-css.R`). Elle
vivait depuis des années dans le `style.css` des cours, qui la restatait sous chaque document : un
sélecteur du domaine de tabxplor écrit ailleurs que dans tabxplor. La migration Quarto de
`formations_stat` (phase 1b) supprime cette feuille de style, et c'est ce qui a rendu la question
visible.

**Pourquoi c'est un défaut et pas une option.** Le titre d'un tableau est une *légende*, pas un
titre de section : il nomme le tableau au lieu d'ouvrir une partie. Sur une page où un titre est
déjà gras, l'italique est ce qui dit la différence au premier coup d'œil — et le gras et les 110 %
étaient déjà là.

Suite de tests verte ; `_snaps/golden.md` accepté (le seul instantané qui contient la feuille).

### v2.0.1 — Phase 3 — l'air au-dessus d'une légende **DONE**

Deux déclarations ajoutées à la même règle : `.tabxplor-caption{margin-top:1.2em;margin-bottom:0}`
(`R/tab-css.R`). Le titre d'un tableau était collé à la ligne qui le précède et se lisait comme sa
suite.

**Pourquoi ici et pas dans la feuille du document.** Une légende ouvre un tableau et lui appartient :
l'air qui sépare le couple du paragraphe au-dessus est celui du **tableau**, dans tous les médiums —
un document, un site pkgdown, jamovi. Un `margin-bottom` nul dit l'autre moitié de la même chose :
l'espace va au-dessus de la paire, jamais entre la légende et le tableau qu'elle nomme.

Suite de tests verte (4 651) ; `_snaps/golden.md` accepté — 16 écarts, tous la même ligne.

### v2.0.1 — Phase 4 — la scrollbox horizontale **DONE**

Demandé par `formations_stat` (phase 1p, constat C14) : 43 des 221 tableaux d'un corpus de cours ont
huit colonnes ou plus, et **aucune règle `overflow` ne les enveloppait** sous Quarto ni dans le
Viewer — un tableau trop large élargissait le document au lieu de défiler. La boîte existait
pourtant, trois fois et ailleurs : chez jamovi (`jmv_results_scrollbox()`), chez pkgdown
(`main table{display:block;overflow:auto}`, une règle de l'hôte que `tab_css()` se contentait de
compenser), nulle part ailleurs.

**Un seul balisage, une seule règle, quatre hôtes.** `tx_scrollbox()` (R/tab-render-html.R) enveloppe
les trois producteurs de `<table class="tabxplor-tab">` — le moteur, la table de forme, la sortie
dégradée — et `tab_css()` porte `display:block; width:max-content; max-width:100%; overflow-x:auto;
overscroll-behavior-x:contain`. `jmv_results_scrollbox()` disparaît ; `jmv_results_style()` ne
restate plus que le plafond en pixels, et la géométrie rendue dans jamovi est inchangée. Gain
collatéral : `jmvtabreg.b.R` n'enveloppait rien, les tableaux de régression jamovi gagnent la boîte.

**Trois points ne se devinent pas.** (1) `width:max-content` **avec** `max-width:100%` : la boîte
épouse un tableau étroit et se plafonne à la place disponible pour un large, ce qui est ce qui fait
déborder le contenu. (2) L'air passe sur la boîte (`TX_TAIL_SPACE`) et la table rend le sien :
`overflow-x` crée un contexte de formatage, donc la marge de la table se serait retrouvée **au-dessus**
de la barre de défilement. La variante jamovi (`:last-child`) n'avait de sens que pour une boîte
unique autour de tableaux empilés. (3) `.tx-scrollbox>.tabxplor-tab{display:table;overflow:visible}`
est **un reset d'hôte** — il neutralise la règle pkgdown (0,2,0 contre 0,0,2) au lieu de la
compenser, et sert tout site pkgdown, pas seulement le nôtre.

⚠ **Le titre reste dehors** : un `<div class="tabxplor-caption">` qui défilerait avec sa table n'est
plus un titre. ⚠ Sur papier la boîte ne doit pas rogner — une imprimante n'a pas de barre de
défilement — et la déclaration écrit `overflow`, pas `overflow-x` : un `overflow-y:auto` calculé
reforce `overflow-x` à `auto`. Elle vit dans `tx_print_block()`, à côté de `print-color-adjust` :
**une feuille n'a qu'un seul bloc `@media print`**, et un second, écrit dans `static`, cassait deux
contrats mesurés — `print_rules = FALSE` n'émet aucun `@media print`, et `test-tab-palettes-sweep.R`
lit la feuille « jusqu'au premier `@media print` ».

**Le pied de pondération dit désormais ce qu'il peut tenir.** « Weighted by w ; confidence intervals
and tests use the unweighted sample size. » s'imprimait sous *tout* tableau pondéré, y compris ceux
qui ne montrent ni intervalle, ni étoile, ni test, ni couleur conditionnée par la significativité
(155 fois dans le corpus des cours). `tab_shows_inference()` (R/fmt_class.R) lit ces quatre choses
sur les colonnes — un `ci` **affiché**, jamais seulement calculé, et `color_signif != "ignore"` —
et chaque base d'inférence gagne sa forme courte : « Weighted by w. » / « Design-based (survey):
weighted estimates. » Deux msgid, `po/R-fr.po` + `.mo` recompilé (325 traduits).

Suites vertes : la livrée (**4 665**) et celle de `dev/tests/` (**4 892**). `_snaps/golden.md`
accepté — 16 fois la règle nouvelle, plus une ligne de pied sur `n_mean_w` ; `_snaps/tab-render-html.md`
accepté — le `<div>` et rien d'autre. Trois assertions de `dev/tests/` étaient **périmées avant cette
phase** et sont remises à jour au passage : l'inventaire des jetons d'affichage, qui ignorait `odds`
(phase 1), et le motif du `.tabxplor-caption`, qui exigeait que `min-width:100%` ferme la règle alors
que la phase 3 lui a ajouté une marge.

### v2.0.1 — Phase 5 — un tableau subordonné (`meta$footer_tabs`) **DONE**

Demandé par `ggfacto` (phase 1u de `formations_stat`) : un résumé d'analyse géométrique doit porter le
tableau des valeurs propres **sous** celui des axes, dans les quatre médias, sans que la fonction rende
une liste — l'appelant veut un `tab` qu'il peut piper et filtrer.

**Rien de neuf à rendre : une expansion.** `set_footer_tabs(x, tabs)` / `get_footer_tabs(x)` écrivent
et lisent `meta$footer_tabs`, et `tx_with_footer_tabs()` rend aux exportateurs **la liste que la table
veut dire**. Le chemin `list_method = TRUE` fait donc tout le reste, déjà : css une fois, thème,
`subtext`, caption sur la première table, une pipe table par tableau en md, un `<table>` par tableau en
html, une feuille par tableau en Excel. Trois lignes d'appel (`tab_md()`, `tab_html()`, `tab_xl()`) et
une boucle dans `print.tabxplor_tab()`.

⚠ **Ce n'est pas la table de forme sous un autre nom, et les deux restent.** Une table de forme
(`meta$assumptions`, `reg_shape_table()`) est une **note** — colonnes de caractères, encre d'aparté,
pipe table en console délibérément —, d'où son émetteur écrit à la main par médium ; un footer tab est
un **tableau** de cellules `fmt`, avec ses couleurs et sa feuille Excel, et n'a donc rien à écrire à la
main. `?new_tab` énonce la différence.

⚠ **Le champ est retiré de la copie transmise**, ce qui interdit la récursion sans garde à maintenir :
les footer tabs d'un footer tab ne sont jamais rendus. Et **un nom est un titre** — `list("Base" = t)`
passe par `set_caption()`, le mécanisme qui existe déjà, plutôt que d'inventer un second titrage ; un
tableau qui porte déjà un titre le garde.

Suite livrée verte : **4 677** (4 665 avant, 12 assertions neuves dans `test-tab-classes.R`). Aucun
snapshot touché : la nouveauté est inerte sur une table qui ne porte pas le champ.

### v2.0.1 — Phase 6 — la pipe table en console, la barre de données, `<var>` **DONE**

Demandé par `ggfacto` (phase 1v de `formations_stat`). Trois corrections et un brouillon de conception.
Suite livrée verte : **4 702** (4 677 avant), `check` 0/0/0. Un seul snapshot réaccepté, `golden.md`
— la feuille gagne la règle de la barre.

**Un tableau subordonné s'imprime en pipe table, pas en second `tabxplor_tab`.** `tab_pipe()` est
exporté, et c'est **`tab_md()` avec trois arguments figés** (`css`, `color`, `subtext`) plutôt qu'un
second moteur : une pipe table qui dériverait de l'export markdown serait une seconde réponse à une
seule question. `print.tabxplor_tab()` s'en sert pour les `meta$footer_tabs`, dans l'encre d'aparté,
et la console retrouve la forme que la table de forme d'une régression y prend déjà : **une grille est
le tableau, ce qui voyage dessous est une note**. ⚠ `...` **écrase** les trois défauts au lieu d'entrer
en collision avec eux — `tab_pipe(t, color = TRUE)` doit atteindre `tab_md()` une fois.

**La barre de données** (`meta$bars`, `set_bars()`) : un graphique en barres dans le tableau, chaque
barre étant la part de la valeur dans le maximum de sa colonne, donc la plus haute remplit sa cellule.
Les lignes de total et de pied n'en portent pas — un total n'est pas à l'échelle de ce qu'il totalise.
⚠ **C'est le seul `style=` inline que le moteur html écrit, et la règle du fichier est réécrite pour le
dire** : ce qui est inline est une **longueur** (`--tx-bar`), jamais une couleur — l'encre est un
`currentColor` mélangé dans la feuille, donc `theme = "auto"` et les palettes de publication décident
toujours de son allure, et une classe par pourcent ferait cent règles. La fraction est calculée dans
`prep_one_table()` et voyage dans le modèle de rendu (`rd$bars`), là où vit tout fait d'affichage par
cellule ; `test-tab-export-prep.R` épingle le nom neuf.

**`<var>` au lieu de `<mean-var>`.** Une variance est une grandeur dans l'unité de la variable, pas
un écart à un axe : « la variance de la moyenne » nommait autre chose. ⚠ **Le préfixe se demande
désormais, il ne se déduit plus de `geometry`** : `NA` y veut dire « ne nomme aucune géométrie
d'effet », ce que lit le refus d'incompatibilité, et ce n'est pas la même question — `var` n'est ni un
contraste ni un écart. D'où une colonne `prefix` sur les lignes de `DISPLAY_TOKENS`, posée à `FALSE`
sur `var` seul ; `sd`, sa propre racine carrée, l'avait déjà par sa `geometry`. Une gloss périmée de
l'en-tête est corrigée au passage (`var` n'est pas « l'écart-type dans une colonne jumelle », `mean`
ne nomme aucune queue d'écart-type).


#### v2.0.1 — Phase 7 — le pied de tableau, un seul gabarit **DONE**

Demandé par `ggfacto` et `formations_stat`, qui butaient sur le même mur : **ce que tabxplor engendre
ne peut pas être redit par qui sait mieux, et ce qu'un paquet tiers porte ne peut pas être rendu par
tabxplor.** `mca_interpret()` colorie sur `color = "contrib"` et recevait « contribution to Chi2 »,
là où un axe factoriel n'a pas de chi² ; faute de pouvoir remplacer le mot, il coupait toute la
légende, écrivait du texte simple dans `subtext` — perdant les pastilles et les deux registres — et
en **console** ne pouvait rien couper du tout. Il collait aussi son échelle ×1 ×2 ×5 ×10 à la main
depuis `get_color_breaks()`, exactement la dérive que le paquet existe pour empêcher.

**Une seule idée : le TEXTE du pied est `subtext`, et `subtext` est un gabarit que `tab()` écrit.**
Tout ce que tabxplor engendre est un `<placeholder>`, tout ce qu'une personne écrit est une ligne, et
**l'ordre des lignes est l'ordre du pied**. `get_subtext()` le montre, `set_subtext()` le remplace ;
supprimer `<legend>` supprime la légende — **en console aussi**, ce qu'aucun argument d'exportateur ne
sait faire. ⚠ **La règle qui rend tout cela rétro-compatible** : un `subtext` qui ne nomme **aucun**
placeholder est simplement ajouté au gabarit par défaut, ce que fait une note depuis toujours ; un
`<…>` inconnu (`<b>`, `n < 30`, `<30 ans>`) passe verbatim et ne revendique rien.

**Le tout est une grille.** `FOOTER_BLOCKS` (`R/tab-footer.R`) déclare la région : une ligne par
membre, l'ordre des lignes étant l'ordre de lecture, chacune nommant son `<placeholder>`, son `kind`
(`line` / `note` / `tab` / `inline`), et **ce qu'elle lit**. La colonne `reads` n'est pas décorative :
elle est vérifiée au chargement contre `TAB_ATTRS` et les attributs `fmt`, et elle **engendre** la
colonne « pour changer ce qui est dit, utilisez… » de `?tabxplor-footer` depuis `TAB_ATTRS$setter`, au
lieu de la redire. C'est aussi **le contrat de dégradation** : une ligne est gated sur ce qu'elle lit,
donc une table qui a perdu ses attributs garde la moitié qui vient des COLONNES (la légende, la clé
des étoiles) et laisse tomber l'autre — sans un seul `tryCatch` (vérifié : `legend_specs()` passe par
`tab_get_vars()$col_vars_levels`, dérivé entièrement de l'attribut `col_var` des colonnes).

**Ce qui est engendré est composé au RENDU, ce qu'une personne écrit est figé.** Chiffré contre le
code : `lang =` sur `tab_md()`/`tab_html()`/`tab_xl()` est un bug déjà corrigé (phase 20h) ; le thème
change le TEXTE et pas seulement la couleur (une palette de publication raccourcit l'échelle, ajoute
« Underlined: »/« Italic: », remplace *Uncoloured* par *Unmarked*, et `print_marks` supprime la ligne
des étoiles) ; terse/prose est un registre par médium ; et un `select()`, un `set_display()` ou un
`set_color_breaks()` après coup ferait mentir une phrase figée — **c'est pourquoi `set_display()` n'a
besoin d'aucun crochet de légende**. jamovi y gagne aussi : `theme` et `wrap_*` restent HORS de la clé
de cache parce que ce sont des arguments de rendu. ⚠ **Rien ne mélange deux langues par défaut** : le
gabarit stocké ne contient que des placeholders, donc une table construite en français et rendue en
`lang = "en"` sort entièrement en anglais.

**Le vocabulaire se redit** (`set_legend_words()`, `meta$legend_words`) : le même fold que
`measure_facts()` applique déjà pour la politique de significativité et pour l'échelle, une couche
plus loin — donc les pastilles, les deux registres, les palettes de publication, la guide de graphique
et la console disent les noms de l'appelant sans une seule branche. Une chaîne nue est le `word`.
⚠ **Nommage seulement** (`MEASURE_WORD_FIELDS`, liste blanche vérifiée à l'écriture) : jamais un fait
du moteur ni un glyphe d'échelle, parce qu'un attribut de table ne doit pas changer un nombre et
qu'une colonne `fmt` extraite doit se colorier à l'identique. L'invariant est écrit à l'envers, à côté
de `MEASURES` : la liste blanche n'est PAS un sous-ensemble des champs déclarés (`lead_over` n'existe
que comme override), donc ce qui est vérifié est qu'aucun fait du moteur n'y figure. Deux nettoyages
en découlent : le mot de la référence « vs la moyenne » devient le champ déclaré `ref_word` (une
branche sur `unit_kind` en moins, et une lecture par échelle : `zscore` dit « vs independence »), et
`legend_lead_fn()` laisse un tiers fournir `lead_over`/`lead_under` en gabarits `%1$s`/`%2$s`/`%3$s`
tandis que les leads déclarés restent des closures — une phrase entière par cas, ce dont l'accord
français a besoin.

**Une note n'est plus un privilège de la régression.** `set_footer_tabs()` prend les deux : un
`tabxplor_tab` se rend en TABLEAU, **toute autre data.frame en NOTE** — une grille de colonnes
caractères déjà rendues, dans l'encre d'aparté. `reg_shape_table()` en est devenu un producteur
(`tab_note()`), et ses quatre émetteurs écrits à la main sont ceux de tout le monde
(`note_html()`, `note_xl()`, `tx_pipe_table()`), sans le double câblage `tab_is_reg() &&
get_assumptions()`. ⚠ Ce que la note dit par colonne est **déclaré** (`kind = "markup"/"spark"`) et
non plus reconnu au nom de la colonne. ⚠ **En console, notes et tableaux subordonnés s'impriment
AU-DESSUS** : le dernier élément imprimé est l'objet R que l'on peut piper ; en export ils lisent
sous le pied.

**Six divergences silencieuses, toutes du même défaut** (la région était assemblée en quatre
endroits), corrigées par construction : la légende imprimée **deux fois** sous un hôte + subordonné
coloré (et zéro fois en console) — un subordonné rend ce qu'il porte et rien d'engendré (`carried`) ;
`tab_html()` qui écrasait le titre d'un subordonné ; `tx_with_footer_tabs()` inopérant sur une liste ;
la table de forme filtrée par `is_tab(tabs)` donc absente en html/md sous un subordonné ; `rd$bars`
non re-clé au transpose (il disparaît maintenant explicitement — un bar est une échelle par COLONNE,
et une colonne transposée est un niveau de ligne) ; et les deux arithmétiques de la hauteur du bloc
en Excel, réduites à `note_xl_rows()`.

**Et la console ne divergeait que d'une chose**, contrairement à ce que le brouillon disait : le
`theme` n'atteignait que le *renderer*, pas le *builder*, donc sous une palette de publication elle
perdait les mots de direction et `print_marks` n'y supprimait pas la ligne des étoiles. `lang` n'a
jamais été cassé (`legend_resolve_lang(NULL)` lit `options(tabxplor.lang)`, qui EST le levier de la
console), et `legend_style` ignoré en console est un choix documenté — `<legend:terse|prose>` donne
désormais le contrôle par table sans changer le défaut.

**`TAB_ATTRS`** (`R/tab_classes.R`) : une ligne par attribut de table — `subtext`, `test` et chaque
champ de `meta` — avec `gloss` (qui engendre la liste de `?new_tab` par `@eval`, déjà périmée),
`bind` (qui absorbe `meta_bind_rules`, 2 lignes sur 7), `subordinate` (la règle de dépouillement,
écrite deux fois) et `setter` (que `FOOTER_BLOCKS$reads` dérive). Quatre colonnes, chacune remplaçant
quelque chose qui existait et dérivait déjà.

**Dettes fermées au passage :** le `" +N more"` non traduit (« cinema selon qualif, sexe +1 more ») —
un seul `tx_name_list()` remplace `tab_title_names()` et `legend_name_list()`, avec deux axes
déclarés (`join` : une liste en PROSE prend « et », une ÉTIQUETTE garde ses virgules ; `overflow` :
un titre compte ce qu'il n'a pas listé, une légende dit « etc. ») → « Régression logistique: cinema
selon qualif, sexe et age » / « … cinema, selon 4 prédicteurs » ; `reg_title()` reçoit enfin `lang`
(le seul endroit où deux langues se rencontraient vraiment dans un tableau) ; **les noms de variables
sont en gras** dans les lignes de légende — un test épinglait explicitement le contraire, il est
retourné et dit pourquoi (un nom est une étiquette, pas une mesure) ; et `color = FALSE` sous
`print_marks` n'émet plus les marques sans leur clé (`fmt_cell_suffix()` lit un thème `marks` qui est
`NULL` quand la couleur est coupée). `?new_tab` ne prétend plus que `set_color_breaks()` écrit
l'attribut par table.

**Ce que la phase n'a PAS fait, et pourquoi.** `md_plain_pipe()` et `tx_pipe_table()` ne sont pas
fusionnés : ils répondent à deux questions différentes (une frame qui n'est pas un tableau tabxplor,
vs une note que le tableau porte), n'ont ni le même padding ni la même bordure, et fusionner aurait
changé la sortie dégradée pour rien. Pas de `tab_text()` non plus : la prose d'un producteur est figée
dans sa langue, ce qui est le choix du mainteneur et évite le piège glibc du cache `(domain, msgid)`
d'un domaine étranger.

**Découpage.** `R/fmt_class.R` 7 353 → 5 928 lignes ; `R/tab-legend.R` (la PHRASE : specs, assembleurs,
renderer par médium) et `R/tab-footer.R` (la RÉGION : la grille, le gabarit, les trois kinds et leurs
émetteurs) sont nouveaux. Étape 1 vérifiée byte-identique.

Suite livrée verte : **4 765** (4 702 avant), `_snaps/` **inchangé** de bout en bout. Les 36 fixtures
`_golden/*.rds` bougent sur le seul attribut `subtext` (vérifié champ par champ contre `HEAD` : 0
fixture diffère d'autre chose), le gabarit y étant désormais stocké. Traductions : 328 traduites
(2 msgid neufs, 1 retiré). Deux fichiers de tests neufs, `test-tab-footer.R` (le gabarit, les
placeholders, la dégradation, **et un bloc de compatibilité CRAN-`ggfacto`-0.3.2**) et
`test-tab-legend.R` (le vocabulaire, la liste blanche, la survie à `dplyr` et `saveRDS`).

**Le cadre à transmettre** est écrit dans `dev/legend_and_side_tables.md` : la section 9 montre le
code AVANT/APRÈS que `ggfacto` écrirait (avec les sorties réelles), la section 10 est le texte à
donner à une session IA dans `~/github/ggfacto/` ou `~/github/formations_stat/`.


#### v2.0.1 — Phase 7b — further simplification and integration of the footer framework ?

The footer legends framework have just been totally reworked by "#### v2.0.1 — Phase 7 — le pied de tableau, un seul gabarit". I wonder if, starting from this new framework, **your can see further simplificationd and integrations**, that would make it more **readable and easy to create custom footer legends even outside of tabxplor**, while keeping `tab()` and `tab_reg()` legends reliable, readable, consise, etc.
- Look at `dev/legend_and_side_tables.md`, specially how other packages are supposed to use it.

?`tabxplor-footer` says :
```r
t <- tab(forcats::gss_cat, race, marital, pct = "row", color = "diff")
get_subtext(t)
#> "<weight>" "<model>" "<interaction>" "<legend>" "<stars>"
```
It’s strange, because : this table use no weights ; it’s not a model, it have no interaction footers. tab() and tab_reg() should write something a bi . And they should adapt it a bit depending on the current table arguments : for example wt is a global table-level argument, it can’t be added easily on an already built table, so it’s meaningful to not write "<weight>" in this particular case.
"<legend>" is the biggest block, so I wonder if more customisation could be easily achievable, using different "<>" tokens inside a same "..." string ?




#### v2.0.1 — Phase 8 — noms de col_vars plus compacts dans les exports

Les noms de variables en html rendent les tableaux `levels="first"` exportés (html, Excel, md) moins compacts, c’est un problème pour lequel je voudrais une solution générale user-friendly et bien pensée, pour utiliser l’espace horizontal quand il y en a, mais avoir un auto wrap intelligent quand l’espace est compté (et notamment quand chaque colonne à sa propre col_var avec `levels="first"`).
- Je voudrais une politique plus agressive, using the sum of the maximums length of all already wrapped columns names (from `wrap_cols =`) *grouped under the same col_var*. It should cut preferentially at " ", "_", etc. (assuming the general case is snake_case variables names), but cut inside a "word" anyway if a "word" is longer that the maximum length.
- Dans le cas spécifique où on a une succession de colonnes avec des col_var différentes (for example from `levels="first"`), je voudrais également une détection automatique des préfixes communs : par exemple, si la première variable de colonne est "CONCERT_CLASSIQUE" et qu’elle n’a qu’une seule colonne, et que la seconde est "CONCERT_ROCK", détecter le préfix commun `"CONCERT_"`, garder le nom de col_var complet de la première variable, et afficher un nom de col_var abrégé `"_ROCK"` pour la seconde variable, idem pour la suivante si elle a le même préfixe, et ainsi de suite jusqu’à ce qu’une colonne n’ait pas le même préfixe. (The leading `"_"` is a readable way to say to the user "it continues with the same prefix as the former name) Il faut prendre en compte les e*dges cases* où il pourrait y avoir des prefixes *nested* (MUS_CONCERT_CLASSIQUE, MUS_CONCERT_ROCK, MUS_CONCERT_JAZZ, MUS_FREQ, MUS_SUPPORT_VYNILE, MUS_SUPPORT_CD), en redonnant le nom complet à chaque fois que le préfixe commun change/s’allonge/se raccourcit. Also think about other possible edge cases. It must not change the col_var attribute itself, only the col_vars names header row built at export time.

Other related improvements, from `~github/ggfacto/` and `~github/formations_stat/` dev:
- A label column whose name contains a space silently loses its rowspan ("Axis label" fails, "Axis_label" works), and an empty name errors inside tab_label_runs() with replacement has length zero. It fails silently — the label just repeats down every row. The lookup should key on column identity, not on a name that gets split ? This is why the column is named Axe and not " " as the kableExtra table had it ?
- A rotated name should be allowed to wrap to several vertical lines ? tab_vname_plan() gives it exactly one, so rotation is unreachable for any heading longer than ~1.75 × block height. The horizontal path already wraps; letting the vertical one do the same would make Axe 1: 9.9% of variance (mod. 57%) turn in a 5-row block.

Dettes de la phase 6 à regler :
- **La barre de données en Excel.** `openxlsx2::wb_add_conditional_formatting(type = "dataBar")` est à
  portée ; c'est la géométrie d'une feuille `tab_xl()` qui est le sujet.
- **L'étiquette d'une colonne de contributions.** `<row%-ctr>` laisse croire qu'une contribution somme
  à 100 % par ligne, alors qu'elle somme à 1 **sur tout le sous-tableau** et qu'elle est identique que
  la table soit en `pct = "row"`, en `"col"` ou en comptages. Le même défaut touche peut-être `cv`,
  `resid`, `ci`, `moe`, `obs`, `gap` (vérifier). Trouver un cadre fiable pour le rendre flexible.
- (Le tag d'unité suit le jeton AFFICHÉ, donc `<ctr>` exige `display = "ctr"`, qui imprime la valeur
  signée. Un override par colonne — ou découpler le tag du jeton — permettrait `<ctr>` au-dessus d'une
  colonne qui affiche des pourcentages. Trop *ad hoc*, ou fiable/lisible ?)
- ⚠ **`tab-steps-legacy.R:561`** compare `fmt_kind_label(tabs)` à `"row"` / `"col"`, que la fonction ne
  renvoie jamais (elle rend `"row%"`), donc tout le bloc `ref` du chemin déprécié est sauté et
  `diff_formula()` retomberait sur `NA_real_`. Chemin déprécié et non testé — à corriger ou à retirer.
- **Un identifiant de colonne stable** serait la vraie réponse au piège que `follow_wrap()` rustine :
  tout ce que le modèle de rendu indexe par un nom de colonne (`bars`, `emp_tips`, `tooltips`) cesse de
  correspondre après `tab_wrap_text()`, en silence. Même sujet que le `rowspan` perdu par un nom
  espacé, en phase 8. ⚠ La phase 7 a tenu la leçon en clé **par mesure**, jamais par colonne.

#### Phase xx — jamovi 2.0.0 release









---

## The last step of every implementation: update the documentation

**You should always start updating docs and writing the "DONE" summary while the final test suite runs**, since it’s now quite long (if a fix follows a failure, correct the affected docs once it passes).
- Keep everything **present-tense and concise**. Never clutter the docs with dev history (the ONLY place dev history is allowed is the "DONE" summary).
- Always respect the **documentation ecosystem** hierarchy (top of this file).
- Edit the files yourself (never hand the maintainer lines to paste).
- If you use a plan, do a real **documentation planning work** : define what goes where, avoid duplication, state what level of details and what focus the lines written in each document should have.

1. **File-header + inline comments** of every module you touched — make them state the CURRENT design, caveats and "why", never how it got there; add or adjust `# DESIGN:` / `# WARNING:` tags next to changed logic. *Cut, don't accrete.*
2. **Phase "DONE" summary** — under its own `#### v2.<x.x> — Phase <x> — <title>` header in the roadmap. This is the ONE place dev-history detail belongs (what changed, why, measurements). CLAUDE.md is the ONLY place it goes; the maintainer moves finished phases to `dev/tabxplor_roadmap_DONE_PHASES.md` once the roadmap gets cluttered.
3. (**Repository Map** in this file — refresh a file's role line only if you added, removed or repurposed a file; keep it absolutely and utterly brief, *never* add clutter here, *cut, don’t accrete*; otherwise skip.)
4. (**`NEWS.md`** — user-facing / CRAN-facing only, new or changed functions/arguments, deprecations, important user-facing fixes; radically minimalistic, usually skipped.)
5. (**`README.Rmd`** — only before a CRAN release.)
