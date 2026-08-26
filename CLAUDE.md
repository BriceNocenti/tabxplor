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
- `tab_classes.R` — `tabxplor_tab`/`grouped_tab` S3 classes, dplyr methods, print, `tab_compact()`/`tab_plot()`, the `test` footer; the palette/breaks API and `COLOR_SCALES`.
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

- `tab-args.R` — the argument surface: `TAB_ARGS` / `EXPORT_ARGS` drive signatures, value lists and `@param` prose.
- `tab-options.R` — the option subsystem: `TAB_OPTIONS` + the generated `?tabxplor-options` page.
- `zzz-fact-keys.R` — `TAB_FOREIGN_KEYS`: cross-table foreign-key checks run at load.
- `utils.R` — `.onLoad()` (seeds options), factor/list/string utilities (padding, wrapping, HTML escaping), deprecation and message helpers.
- `data.R` — the four example data sets and their source credits (built by `data-raw/DATASETS.R`).

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

**Other directories:** `vignettes/` (introduction, *All else equal*, regression, weights, programming; `vignettes/articles/` is pkgdown-only and holds the French twins) · `tests/testthat/` (testthat v3) · `man/` (roxygen-generated, never edit) · `data/` + `data-raw/` (the four example data sets and the script that builds them) · `inst/i18n/` + `po/` (translations) · `jamovi/` (module definition) · `dev/` (architecture guide, dev scripts, perf harness, `.Rbuildignore`'d).

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

The payoff to internalise: **adding a measure, an option, an argument, an estimand is one new row — not N scattered edits.** Do not re-introduce ad-hoc branches; extend the table. The main fact tables:

| Fact table         | Home                 | Declares                                                                                |
|--------------------|----------------------|-----------------------------------------------------------------------------------------|
| `MEASURES`         | `fmt_class.R`        | The colour measures (raw field, scale keys, significance source, legend, requirements)  |
| `EST_SCALES`       | `fmt_class.R`        | What a column estimates (field, null, geometry, colour ladder, SD source, precision)   |
| `MEASURE_ACRONYMS` | `fmt_class.R`        | The discipline's acronyms: one spelling vocabulary for every argument naming a measure  |
| `DISPLAY_TOKENS`   | `tab-display.R`      | The `{}` display grammar (field source, geometry, aliases, placement)                   |
| `DISPLAY_PRESETS`  | `tab-display.R`      | The named cell layouts both producers resolve (`est` / `est_ci` / `est_base` / …)       |
| `CI_METHODS`       | `tab-agg.R`          | The confidence-interval methods and geometries (with `CI_GEOMS`)                        |
| `COLOR_SCALES`     | `tab_classes.R`      | The break scales and palettes                                                           |
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

**The reference system:** `ref` picks the baseline a deviation is measured from (`tot` / `first` / an index / a regex), reinterpreted by `pct` (a reference *row* under row%/means, a reference *column* under col%); `ref2` names the second level for odds ratios; `comp` compares within each sub-table or against the total table. **Significance:** a cell is significant when its confidence interval excludes the **neutral value** — 0 for a difference, 1 for a ratio — and the displayed p-value and stars come from inverting that same interval, so colour, greying and stars cannot disagree. Interval geometry is declared in `CI_GEOMS`, its method in `CI_METHODS`.

### The inference layer

**The survey-design boundary** (`survey-design.R`) is one unwrap point: a `survey` design passed as `data` becomes the microdata every engine already reads, plus its sampling weights and design metadata — so the observed columns, the marginal effects, the tests and the footer are all design-weighted, and a `svyrepdesign`/`twophase` is refused rather than approximated.

**The inference basis** is the layer's central idea: how the *estimate* is computed (`wt`) and how the *interval and test* are computed (the basis) are **orthogonal**. The basis is one of `n` / `weights` / `design` / `design_partial` and — with `conf_level`, `degf` and `ci_method` — is stored **on each column, not on the table**, because `dplyr` drops table attributes and a number must never depend on one. A bind reconciles them by the weakest-claim rule.

**Design-based cell variance** (`survey-variance.R`) feeds the existing `n_eff` field, so the ordinary CI machinery becomes design-aware with no new field. A plain weight column is a survey design at `ids = ~1`, where the general formula collapses to a per-cell closed form computed from the aggregate alone (Kish is its degenerate limit); a real design goes through `survey::svyrecvar`, which owns the variance algebra throughout.

### The display grammar

What a cell prints is a `{}` template over declared tokens (`DISPLAY_TOKENS`), resolved by one boundary `tab()`, `tab_reg()` and `set_display()` share, so a layout learnt on a crosstab means the same on a regression. `{est}` and `{base}` are **scale-relative** — the deviation a column estimates, and the level it sits on — which is what lets one named preset (`DISPLAY_PRESETS`) render an odds ratio, a mean difference and a percentage alike. A composite has a **primary** token, the first outside brackets: it carries the stars, it is what `get_num()` and Excel return, and it is the only part the colour paints — and its converse, a template with no token outside brackets, has no primary at all and renders whole as an aside. A token may also carry **its own precision** (`{base:1}`), which beats every declared default — digits are a display property, and the cell's one `digits` field cannot say that an estimate reads at three decimals and its aside at one. **A display is post-hoc** — every field a layout can print is populated at build, so choosing one triggers no computation and changes no number, and a token may be **derived** rather than stored (`resid`, `gap`, `sd`, `cv`). A numeric column's default layout is `mean_cv` — the spread as a percentage of the level, comparable between columns measured in different units — chosen per column and falling back to the bare mean where a mean is not positive. The **base count** is the display-time fact both producers share: folded into the Total cell when the table rests on one population, given one `n` column per block at the right when it rests on several (a spread, a regression's groups) — and the per-block Total columns then go, holding nothing but a repeated 100 %.

### The colour system

Colour has three orthogonal axes: a **measure** (which deviation to grade — `difference` / `ratio` / `odds_ratio` / `contrib`, or the two gap measures `adjustment` / `between_groups`), a **channel** (text and/or background), and a **significance policy** (`color_signif`: `ignore` / `grey_non_signif` / `guaranteed_effect`). The engine has three layers:

1. **Palettes** (`tab-palettes.R`, which holds every one of them) — OKLCH colour ramps, hand-tuned so intensity levels stay distinguishable, in light, dark and 8-bit variants, set via `set_color_palette()`; the **chrome** beside them (`tx_chrome_hex()`: the table's own ink, the greyed-out cell, the aside); and, where a page has no colour, three **publication palettes** (`PRINT_PALETTES`) saying the same thing typographically — one declared grid each, a row per break slot carrying its ink, face and mark, `theme = "print_ready"` choosing between them from what the table IS. A palette is always hex **and** face: a backend must never derive "is this bold" from "does this have a hex".
2. **Breaks** — per-scale thresholds (`COLOR_SCALES`). Every ladder is the SAME ladder written in another measure at one reference cell of 50 %, so a shade means the same size of deviation whichever measure a table is read on; each declares its `quantity`, its `anchor`, whether its two `sides` mirror (only where the quantity is unbounded above), and how many loud rungs it keeps on the background channel (`bg_keep` — a fill is the corrective voice). The shape rule is checked at load.
3. **Selection** — a vectorised `findInterval` engine (`fmt_color_plan` → `fmt_color_slots` → `fmt_color_channels`) that folds each cell per side and picks the strongest matching threshold.

The measure's behaviour — raw getter, scale keys, significance source, gating — lives in its `MEASURES` row, which drives both the plan and the legend with no per-measure branches; every backend then consumes the one artifact `fmt_color_channels` produces, which is why console, HTML, Excel, Markdown and plots colour identically.

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

**Effects and model checks.** A marginal quantity comes from tabxplor's own analytic g-computation, or from `marginaleffects` at a reference profile — derived from the contrast, never declared per row. `REG_CHECKS` catalogues the checks (linearity, dispersion, influence, proportionality, collinearity), each with the `shape =` cure that fixes what it flags — the check and its cure are one object — each priced (`cost`) and each declaring whether it runs by default (`footer_default`), because what a table must say and what it costs are two questions. The **observed shape** of a numeric predictor is the free half of the linearity check: one curve per outcome, binned with no fit at all, drawn in a window floored by the data's own sampling noise and by the first colour rung — so a flat run means flat. It goes in a small **shape table** below the footer, beside the range it is a picture of.

### Exports and rendering

`tab_export(x, format =)` (`tab-export.R`) is the facade over four backends: HTML (the default), Markdown, Excel and plot, sharing one preparation step — `tab_export_prep()` (`tab-export-prep.R`) builds an ephemeral render model (roles, references, faces, header spans, variable-name blocks) that every backend consumes. A spread swaps the two header bands, since after a spread a **column** is identified by its sub-population and a **block** by its variable: the column header takes the `col_group`, the span takes the `col_var` and, above it, the level only where that variable gives several columns per group.

Display values reach the backends by one source of truth: `format.tabxplor_fmt()` renders the text for console, Markdown and HTML, and `tab_xl()` writes a number with format codes from that *same* `format(syntax = "excel")`, so a display change never needs mirroring. **Excel keeps the cell a number and puts everything else in the code**: an aside becomes a column carrying its own segment (`(n={n})`), and every literal a template writes — the stars, the brackets, a sigma, a test label — folds into the numFmt, per section. A multiplicative cell holds its **reading value**, the signed fold, so `1/2.11` reaches the workbook without becoming text; text stays a property of a *cell*, not of a column. The exports' **unit row** is the console's own type tag (`<row%>`, `<n>`), written once per **block** — `tab_col_block_ids()`, the one definition of a block, which also decides where a vertical rule falls. Colour is single-sourced too — every backend reads `fmt_color_channels`. HTML colour is a slot **class**, never inline hex, the theme living in a `<style>` block from the one CSS generator (`tab-css.R`), so light/dark and the publication palettes work by stylesheet — except `print_marks`, whose signal is cell text and so comes from `format()` like the stars. `tab-transpose-render.R` flips a finished render model (a transposed column is heterogeneous and cannot be an `fmt` column), and `tab-theme-detect.R` best-effort-detects the console's scheme — a subsystem that must never error, because a wrong guess only mis-tints.

**How wide a thing is, and where it breaks, are measured from the rendered content.** A column name and a variable name are compound words, not prose, so they break at the seams a name is built from (`_`, `.`, `*`, camelCase) rather than at whitespace alone; a *variable* name is written vertically only where the rotation actually saves width — the names that cannot turn, a one-row block like `Constant`, set the floor every other name is weighed against, and a rotated one wraps to its block's own height. That decision is one prep fact both media read. Excel then has no fixed widths at all: each column is as wide as the widest thing in it that cannot wrap (a figure), while a header, a unit tag or a long label contributes its width divided by the lines it may use — measured per **sheet**, since a column index belongs to the sheet and not to the table sitting on it.

The **hover tooltip** (`tab-tooltip.R`) is that same rule read line by line: `TOOLTIP_LINES` declares one row per line — the token it renders, where its name comes from, which of the shared gates apply — and row order IS the reading order, so a line is named by its `DISPLAY_TOKENS$label`, exactly as the exports' unit row is, and one gate (non-empty · comparable · not the reference · not already shown · not already emitted) decides every one of them. It has **two rows**, declared the same way (`group`): the cell's own numbers, then the observed comparison — `obs` and the gap to it, a statement about another column — joined by a newline the stylesheet honours. It is **not translated**, deliberately: like the pillar type tags its words are the `fmt` field names, so the hover teaches the fields a user reads with `$`.

### jamovi

Two point-and-click analyses mirror the two producers: `jmvtab` (Crosstables) and `jmvtabreg` (Regressions). Each is a thin `R6` backend (`*.b.R`) over an engine-free build core (`jmvtab_build()` / `jmvtab_reg_build()`) driving `tab()` / `tab_reg()` through a content-addressed **live-UI cache** (`*-cache.R`), so an interactive tweak re-paints instead of recomputing. Each option is named after the argument it drives, so the backend is a pass-through, not a translation table — and where the panel asks a *simpler* question than the argument takes (a tick-box for `empirical`, two of `theme`'s seven values), R resolves the rest. An argument applied at RENDER (`theme`, `wrap_*`) is read straight off the options and deliberately kept out of `.opts()`, which is the crosstab cache key's complement. The regression store holds **distilled fit records** (kilobytes) keyed on the model alone — the model's own and each observed (crude) univariable one, one record shape told apart by its key — so every estimand change is a hit and nothing heavy crosses jamovi's `$state`. The generated `*.h.R` option headers are never hand-edited.

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
- **`vignettes/articles/tabxplor-all-else-equal.Rmd`** — the most precise account of what tabxplor's *philosophy*, *vocabulary*, *usage* and *real-world regression use cases* really are; its words (deviation, observed vs adjusted, the base, the round trip) are the package's own.
- **Roxygen man pages** (`?tab`, `?tabxplor-display`, `?tabxplor-vctrs`, `?tabxplor-options`, `?tabxplor-data.table`) — user-facing reference: *usage* and the main use cases, never build/internals/history. A `@param` states what the argument is, its values, and at most one sentence of when to change it; the rest is a link to the vignette that owns it.
- **`dev/*.md`** (`.Rbuildignore`'d) — transversal or expert technical guides only.
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
| `test-reg-shape.R`        | Phase 18z15: `shape =`, the plot primitives, the stored curves and the shape table              |
| `test-reg-rank.R`         | Phase 22c-vi: the ordinal superiority pair -- gradients, K=2 reduction, collapsibility, survey  |

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


### Phase 21 — documentation integration and simplification 1 (DONE)

The package documentation had grown cluttered with dev history and lost focus. This phase enforces the **documentation ecosystem** hierarchy (top of this file) across every layer: present-tense, history-free, general to specific, each fact stated once, referencing more global or more precise docs rather than duplicating them. Before writing any document, state what it is for, its focus within the ecosystem, and what belongs elsewhere. 21a integrated the architecture + Repository Map at the top of this file; 21b rewrites the R comments + roxygen; Phase 23 does the vignettes, `NEWS.md` and the `dev/` folder.


#### Phase 21b — R scripts comments drastic simplification/rewrite (DONE)

The task is to **drastically** rewrite and simplify R scripts comments (including key decisions, etc.) **based on the final design, architecture and real-world usage**, for future development to never lose focus on them. Their **global length should be divided by at least 5**: measure it, find a maximum ratio of comments/code, make web searches if needed.

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


### Phase 22 — manual reviews and last features before release (DONE)

#### Phase 22j — Package check() + resolve github action R CMD check failures — DONE

`R CMD check` returned **1 ERROR, 1 WARNING, 2 NOTEs** on all five CI platforms (and had for weeks), plus a red `test-coverage`. **`devtools::check()` now returns `Status: OK` — 0 errors, 0 warnings, 0 notes** (`devtools::test()`: FAIL 0 | PASS 9987). Not one failure was a defect in tabxplor's behaviour: every one was a test asserting something untrue of the environment `R CMD check` builds, or an upstream change. What the phase is worth recording is the five root causes, because three of them are traps any future test can fall into again.

⚠ **A test that reads the package SOURCE is a DEVELOPMENT test, and `../..` is not there.** `R CMD check` runs the tests from `<pkg>.Rcheck/tests/testthat`, where `../..` holds only the *installed* package — and `jamovi/` and `dev/` are `.Rbuildignore`d, so they are absent from the tarball too. Fourteen sites read the source; nine guarded it with an inline `skip_if_not(file.exists(…))` and **five did not**, which is the whole ERROR. Two more (`test-jmvtab-export.R:295, 304`) put the `skip_if()` *after* the `readLines()` that throws, so the guard was unreachable — `length(src) == 0` is only ever true for an empty file that exists. One spelling replaces all fourteen: **`src_path(...)`** in the new `tests/testthat/helper-source.R`, which resolves the path and skips when it is absent. Its `WARNING` states the rule: call it *instead of* `test_path()`, never after the read.

⚠ **`--as-cran` installs the package with Depends/Imports ONLY** (`_R_CHECK_INSTALL_DEPENDS_=TRUE`, set in `tools:::.check_packages`'s `if (as_cran)` block). The generated `R/jmvtab*.h.R` build their option classes under `if (requireNamespace("jmvcore", quietly = TRUE))` **at install time**, so under check `jmvtabOptions` is `NULL` and `$new()` gives *"attempt to apply non-function"* — with jmvcore 2.7.38 sitting right there in the library, which is what made it look impossible. Verified directly: `_R_CHECK_INSTALL_DEPENDS_=TRUE R CMD INSTALL -l <tmplib> .` then `is.null(tabxplor:::jmvtabOptions)` is TRUE, FALSE in a normal install. The test now skips on the *generator*, not on the package. ⚠ The same mechanism has a real-world edge left un-patched: a user who installs tabxplor **before** jmvcore gets NULL generators until tabxplor is reinstalled. Inside jamovi it cannot happen, and the `.h.R` are `jmvtools::prepare()` output that must never be hand-edited.

⚠ **Two upstream changes, each looking like a regression:**

- **R 4.6.0 rebuilt the glm influence measures** on Pearson residuals, dropping the leave-one-out dispersion where the dispersion is fixed (R NEWS, *"Several influence measures for `glm` objects…"*). tabxplor's engine IS the new definition, so `test-reg-checks.R`'s parity against `stats::dfbetas()` failed **on oldrel-1 alone** (`ref` 0.098 against an unchanged `got` 0.120). This was the maintainer's hunch about oldrel-1, and it is the whole of it: guarded with `skip_if(getRversion() < "4.6.0", …)`.
- **openxlsx2 1.29** stopped writing `<scheme>` on its own base font (CI has 1.29, this box 1.28). `test-tab_xl.R` asserted `sum(grepl("<scheme", fonts)) == 1L` — an assertion about openxlsx2's internals. It now states the invariant that is actually tabxplor's: **at most one** font carries a scheme, and if one does it is the base font. The third assertion beside it was deleted, being a tautology (it subset `fonts` on the negation of its own predicate).

**Two tolerances that asserted bit-identical floating point.** `test-reg-cross.R:246` compared a p-value from two *independently refitted* glms at `tolerance = 1e-10`; macOS/aarch64 differed by 2e-11 relative. Now `1e-7` — seven significant figures prove the p-value IS the additive-vs-crossed comparison; anything tighter tests the BLAS. And `test-19m3-defects.R` (covr-only, the last thing keeping `test-coverage` red) walks `body()` harvesting length-1 string constants, while covr rewrites every body as `if (TRUE) { covr:::count("<srcref key>"); … }` — whose keys are exactly that. It takes the `R_COVR` skip `test-parallel-parity.R` already uses; the identical guard at `R/tab-display.R:1285` runs at load, before covr instruments, so nothing is lost.

**The WARNING and the two NOTEs.** `carData` was used by four tests but declared only in `Config/Needs/website`, which `R CMD check` does not read → added to `Suggests` with a `skip_if_not_installed()` at each site. `checkRd: Lost braces` came from ONE stray cli inline markup in a fact-table `doc` field (`R/tab-options.R`, `shape_auto_max`) reaching Rd as bare braces — the only one in the package (swept `R/`, `man/`, every roxygen block and every `doc =`). And `no visible binding for 'add_n'`: `tab_counts()` binds its dots arguments through `list2env(tab_dots_expand(…))`, invisible to codetools, and `add_n` was the one name with no mirror — being deprecated, it never becomes a `ctx` field the way `R/tab.R:2862`'s derived declaration covers the rest. Fixed the same way, **derived**: `utils::globalVariables(tab_args_for("tab_counts"))`.

**CI hygiene**: `actions/checkout@v4` → `@v5` and `actions/upload-artifact@v4` → `@v5` in the three workflows that use them, clearing the Node-20 deprecation annotation.

⚠ **Not re-verified here: vignette building.** The check ran with `vignettes = FALSE` because a parallel session was rewriting `vignettes/` throughout (Phase 23a). Both vignette steps were already OK on CI; re-run a full `devtools::check()` once 23a lands.

#### Phase 22k — vignette rendering fixes — DONE

Two defects, both in `R/` and neither reachable from a vignette, fixed by **removing an asymmetry** in each case rather than adding a rule.

**The border under every row was one selector that could not reach half its own package.** `R/tab-css.R` already had the reset — `.tabxplor-tab table td,.tabxplor-tab table th{border-width:0;}` — but its selector needs a `table` **descendant** of `.tabxplor-tab`, which exists only in markdown (pandoc's `<div>` -> `<table>`). In the html engine `.tabxplor-tab` **is** the `<table>` (`render_html_engine()` emits `<table class="tabxplor-tab">`), so the reset never matched there and Bootstrap's `.table>:not(caption)>*>*` (0,1,1) kept its `border-bottom-width` — which the package's own `border-color` rule then painted black, giving a line under every row on the pkgdown site. Confirmed at the source: `pkgdown:::tweak_tables()` is `tweak_class_prepend(table, "table")`, unconditional, on every `<table>` it renders.

The fix is the medium-agnostic **`.tabxplor-tab th,.tabxplor-tab td{border-width:0;}`**, a REPLACEMENT — the md-only rule is gone, not kept beside it. Every border the package DRAWS is a role class at (0,2,0) or better (`thead th` (0,1,2), `> thead > tr:first-child > *:not(.tx-span)`, `.tx-span`, `.tx-br`/`.tx-bl`, `tr.tx-bt>*`, `tr.tx-bb>*`/`td.tx-bb`/`tx-bb2`/`tx-bt2`, `thead .tx-unit`, and the four md `.tabxplor-tab table > ...` edge rules), so the reset takes the host's borders and leaves the package's. Dropping from (0,1,2) to (0,1,1) also **retires a WARNING**: the reset no longer ties `.tabxplor-tab thead th` on specificity, so the header underline no longer depends on source order to win.

⚠ **The same Bootstrap rule paints an OPAQUE ground.** `--bs-table-bg` is `var(--bs-body-bg)`, and a cell that paints its own background paints over its row — which is what hid the row-hover highlight on the pkgdown site. `background-color` is now stated for the colour themes too, as **`transparent`**: exactly what a cell has with no rule at all, so nothing changes except that a host can no longer override it. It ties the host at (0,1,1) and wins on source order (the stylesheet is emitted in the body), and still loses to `.tabxplor-tab .o3` (0,2,0), so the background colour channel and `.tx-pill` are untouched. Bootstrap's `color` is left alone: for light/dark, following the page IS the stated intent, and the slot classes win anyway.

**A rotated name now gets ONE vertical line, and the budget says where it comes from.** `tab_vname_plan()` admitted a name at `w <= span * 4` and wrapped it to `span * 2` characters per vertical line, from `TX_VERT_CHARS_PER_ROW = 2L` x `TX_VERT_MAX_LINES = 2L` — two constants tied to no rendered height. ⚠ **Neither medium can recover from an overrun**: a browser does not grow a `rowspan`ned cell to hold vertical text that exceeds it (`.tx-lbl` centres it, so the name spills onto the blocks above AND below — the reported symptom), and Excel never auto-fits a merged row. So the budget is now derived from the stylesheet this package itself writes — a data row is `line-height:1.1` + `padding:3px` twice, so `1.1em + 6px`; a sideways latin character advances by its own glyph width, ~0.6em mixed-case; the name cell spends `padding:4px` twice of the BLOCK, a fixed 8px charged once and not per row — giving `(span * (1.1em + 6px) - 8px) / 0.6em` ~ `2*span - 1` characters, stable between 12 and 18px. Cut to `TX_VERT_CHARS_PER_ROW = 1.75` and `TX_VERT_PAD_CHARS = 1L` for headroom, read through one accessor, `tx_vert_capacity(span)`.

`TX_VERT_MAX_LINES` is **deleted**. Because `fits` now guarantees `w <= capacity`, `tx_wrap_name()` can never break a rotated name — which removes the only case whose rendered width a browser had to infer, and makes `tab_xl()`'s own width model TRUE where it was an assumption (`xl_col_widths()` sizes the name column from `chars` alone, on the stated ground that "a rotated run costs one vertical line"). A name that no longer rotates stays horizontal and wraps at `TX_VNAME_MAX = 13L` — a rendering both media already support, and where rows DO grow.

Measured on real tables: `marital` (7 chars / 7 rows) and `partyid` (7 / 11) keep rotating; the two-line `situation_<br> pro` (13 / 6) and `Model fit` (9 / 5) stop, widening their column by 1 to 5 characters; a regression's `tab_vars` levels (5 / 19) keep rotating. **No CSS guard was added**: a `max-height`/`overflow:hidden` on `.tx-vname` truncates a variable name silently, so the budget carries the guarantee and `R/tab-css.R` says so at the rule.

⚠ **The constant is derived, not measured.** No browser exists in this distro (`chromote::find_chrome()` finds none; `C:\Program Files` is not mounted), so two harness pages — the same tables before and after, wrapped in pkgdown's actual host CSS with `class="table"` stamped on — were written for the maintainer to open. If a name still spills, `TX_VERT_CHARS_PER_ROW` is the only lever.

**Tests.** `test-tab_md.R`'s "md-only rules" test now states the reset is medium-agnostic and asserts the old selector is GONE; `test-print-palette.R`'s "the colour themes do not force it" asserts no ink but a `transparent` ground. `_snaps/golden.md` regenerated: 16 cases, four diff lines, all four the two CSS rules and nothing else. Everything else stayed green unedited, including the no-shorthand lock (`border-width` is a width property, not the `border`/`border-top` shorthand that resets `border-*-color`) and the four existing rotation assertions.


#### Phase 22l — Imports and Suggests cleaning — DONE

**Imports 19 -> 15** (five slots of headroom against CRAN's 20), **Suggests 32 -> 24**, and
`install.packages("tabxplor", dependencies = TRUE)` -- which `tx_need_pkg()` actively teaches -- went
from pulling **205 packages to 131**. The hard install went 45 -> 34. Gone from a user's library:
`shiny`, `lme4`, `quantreg`, `mgcv`, `pbkrtest`, `DT`, `emmeans`, `rstatix`, `ggpubr`, `FactoMineR`,
`questionr`, `car`, `carData`, `showtext` and 60 more. The standing policy, the inventory and the
options priced but not taken now live in **`dev/dependencies.md`**, referenced from the architecture
section; this summary records only what moved and what it cost.

⚠ **Nothing was promoted.** The seven candidates would have taken Imports to 26, and each is huge
(`VGAM`), compiled (`openxlsx2` -> Rcpp + stringi) or dead weight unless `tabxplor.parallel = TRUE`
(`mirai`, `RhpcBLASctl`, `parallelly`). The freed slots are the deliverable.

⚠ **One premise of the brief did not hold: `survey` is not only a survey dependency.**
`survey::regTermTest()` is the Wald engine for *unweighted* multinomial and ordinal footers
(`R/tab_reg.R:2538`, `:3021`), so moving it would have silently removed footer rows for users who
never touch a design. It stays in Imports, and `dev/dependencies.md` records why.

**`tab_plot()` is defunct**, following `kable_tabxplor_style()`'s precedent (an exported stub with
`@keywords internal` and `lifecycle::deprecate_stop`). It was 346 lines plus a six-function vendored
`ggpubr` block in `R/utils.R`, the sole consumer of `ggpubr` / `cowplot` / `gtable` (~15 packages,
`rstatix` and `ggsci` among them), and the most expensive single call in the package at 2.49 s. Also
removed: `tab_export(format = "plot")`, the `tabxplor.plot_num_font` option, `tx_num_font()`'s `plot`
branch, and `"tab_plot"` from **11** `TAB_ARGS` producer vectors. Its source is parked as live,
un-commented R in **`dev/legacy/tab_plot_2.0.0.R`** -- not commented out in `R/`, which the
documentation discipline forbids and which would make it ungreppable -- with a header listing the
four things besides the file that a revival needs.

**Three Imports removed by writing a little code.**

- **`htmltools`** was one function. `tx_html_escape()` (`R/utils.R`) is a faithful port of
  `htmlEscape` (GPL >= 2, credited), verified identical for both `attribute` modes, encoding included.
  ⚠ The nine vignettes also called it, in their `fansi` knit hooks; they now escape inline, so
  htmltools leaves DESCRIPTION entirely rather than dropping to Suggests.
- **`broom`** was four call sites. `reg_tidy_coefmat()` / `reg_tidy_multinom()` / `reg_tidy_polr()` /
  `reg_glance_lm()` read the same numbers off `summary()`, verified identical against broom 1.0.13 on
  glm / lm / svyglm / multinom / polr / glance. ⚠ The spine of `reg_tidy_coefmat()` is
  `names(coef(fit))`, **not** the summary's rownames: an aliased coefficient is `NA` in `coef()` and
  absent from the summary, and `reg_crude_rows()` matches the skeleton by row position.
- **`knitr` -> Suggests.** `VignetteBuilder` is unaffected and the three `knit_print` methods were
  already *delayed* registrations. Chunk options now go through `tx_knitr_opt()`, and the one real
  call (`kable()` on the degrade path) became `md_plain_pipe()`.

⚠ **A live regression the knitr move exposed, worth remembering.** `print.tabxplor_kable()` ends in
`NextMethod()`, and an S3 method exists only once its own package is loaded. Previously
`knitr::opts_knit$get()` loaded knitr as a side effect; without it, the fall-through reached
`print.default()` and printed the raw character vector with its attributes. The method now loads
knitr before falling through, with a one-line `cat()` fallback -- which also collapsed the `"next"`
and `"degrade"` branches into one.

**`car` dropped, `tx_vif()` vendored** (`R/reg-assumptions.R`) -- overturning that file's own
"never a hand-rolled substitute" note, since `car` dragged `lme4`, `quantreg`, `mgcv` and `pbkrtest`
for one formula. It is Fox & Monette (1992) GVIF, returning **both** of car's shapes so both call
sites read it unchanged, and it **refuses rather than approximates** (fewer than 2 terms, aliased
coefficients, a rank-deficient fit, a matrix-coefficient fit, a singular vcov -> `NULL`, no row).
Measured `all.equal(tolerance = 1e-13)` against real `car::vif()` on **14** fit shapes, and `NULL`
exactly where car errors or returns `NaN`. That check cannot be a test (car is no longer declared),
so it is **`dev/vif_car_parity.R`**, to re-run after any change. The suite's own tests were re-pointed
at two *independent* derivations: `1/(1 - R2)` of the auxiliary regression on the model's IRLS
weights, and the determinant ratio taken on `cov.wt(X)` -- the data side, never `vcov()`.

**Four example data sets now ship** (`R/data.R`, `data-raw/DATASETS.R`), removing `FactoMineR`
(~18 packages for one data set), `questionr` (which pulls `shiny`) and `carData` from Suggests.
⚠ They are the **complete** originals -- 44.6 KB for all four, so trimming bought nothing -- with one
editorial change: in a two-level yes/no factor the "yes" answer goes first, which is what `tab()` and
`tab_reg()` model and show. ⚠ **The name carries the credit**: `facto_tea`, `questionr_hdv`,
`car_arrests`, `car_salaries`, so attaching the source package beside tabxplor masks nothing. A
same-name copy would have been *worse* than a rename, since the level order deliberately differs.
All four are GPL (>= 2); every `@source` names the package, its authors, the original study and how
to get the untouched data. The vignettes lost their preparation chunks and their `requireNamespace`
degrade guard; the tests lost four skip guards.

**`stringi` removed: 143 call sites, 19 functions, 16 files.** Three primitives in `R/utils.R` carry
the load -- `tx_pad()` (padding on **display width**, `nchar(type = "width")`, since `pad` is often a
figure space), `tx_str_wrap()` and `tx_str_trunc()`. ⚠ `tx_str_wrap()` is **minimum-raggedness by
dynamic programming**, not a greedy fill and not `strwrap()`: `stri_wrap()` minimises the sum of
squared trailing space, and a greedy fill produced visibly different tables. Verified over **21 112**
comparisons, 0 differences; `tx_pad()` likewise exact.

⚠ **Four traps, each hit and fixed, all now recorded in `dev/dependencies.md`:**

1. **stringi vectorises over `pattern`; base R does not.** `stri_detect_fixed(one_line, five_tags)`
   returns five logicals; `grepl(five_tags, one_line)` uses the first and warns.
2. **`x |> stri_*()` cannot be reordered mechanically** -- stringi takes the string first, `gsub()`
   the pattern. Fourteen piped calls had to be rewritten as explicit ones.
3. **`stri_trim(x, side = "left")` is one-sided**; losing `which =` moved every markdown column, which
   is what `test-golden.R` caught.
4. **ICU classes are not PCRE classes**: `\P{Wspace}` -> `[\h\v]`, and a `\uXXXX` escape inside an ICU
   *pattern* is a literal backslash-u in PCRE.

⚠ **What the stringi work did NOT buy**, stated plainly because it is easy to assume otherwise:
stringi is still installed, via `tidyr` -> `stringr` -> `stringi`. Dropping it bought a CRAN slot and
removed a direct coupling; only dropping `tidyr` would remove the package from a user's library.

**Two smaller items.** `fs` went, its three call sites already carrying complete base fallbacks -- so
an ad-hoc double code path went with it. `grid` moved Suggests -> Imports: it is a base package, so it
costs nothing and does not count toward the 20, and it was being called **unguarded** in
`forest_plot()`.

⚠ **`DescTools` was measured and deliberately kept.** It drags ~15 packages for CI-parity assertions
in four test files, validating the closed-form CI engine against an independent implementation --
which is worth keeping. Moving those tests to `dev/` is **Phase 23e**'s job, not this one.

**Verification.** `devtools::test()`: **FAIL 0 | PASS 9987**, with `test-golden.R`,
`test-export-parity.R`, `test-fmt-contract.R`, `test-fuse-parity.R` and every `_snaps/` file
byte-identical -- which is what proves the string rewrite. All nine vignettes render in a cold
`Rscript`, 0 ANSI escapes. `devtools::document()` clean; the three delayed
`S3method(knitr::knit_print, …)` registrations survive. The five remaining test warnings and four
skips are all pre-existing (over-dispersion teaching warnings, a non-converging synthetic glm,
opt-in benchmarks). ⚠ **Not run here: a full `devtools::check()`** -- it is the release gate and the
maintainer runs it.

**A defect fixed on the way, not a dependency matter.** `broom:::tidy.multinom` sets `y.level` to the
string `"1"` when the outcome has two levels instead of naming the category, and
`reg_columns_multinom()` filters on the real level name -- so **zero rows matched and the whole column
came out NA**. Reachable via `family = "multinomial"` on a binary outcome, or a 3-level outcome that
drops to 2 after complete-case filtering. `reg_tidy_multinom()` takes the name from `fit$lev[-1]`;
`test-tab_reg.R` now covers it.


#### Phase 22m — exported functions review — DONE

**The map is `dev/tabxplor_2.0.0_exported_functions_review.md`, and it is where the removal proposals stay.** Measured first, because it decided the whole shape of the phase: of the **98 exports, 56 come from 1.3.1** (a released contract) and only **42 are new in 2.0.0**. The maintainer's call was to implement the index and page structure now and leave every unexport as a written proposal for a separate session — which is the right split, because the second measurement is that **unexporting an accessor buys nothing on the reference index**: all 41 `fmt` accessors sat on ONE page, so the index only moves by restructuring.

**What was actually cluttering the page, then.** Not the export count — the **tail**. A whole section advertised the five hard-deprecated steps (`tab_pct` / `tab_tot` / `tab_totaltab` / `tab_ci` / `tab_chi2`), which warn on **every** call and are defunct in 2.1.0; and `?fmt` was a **719-line page carrying 42 aliases**, whose `\value` alone was a stack of ~40 one-line paragraphs ("A modified fmt vector." six times over). Four edits, no export added or removed, `NAMESPACE` byte-identical:

- **The five steps and `tab_get_vars()` took `@keywords internal`.** Still exported, still working, still warning — off the index. ⚠ That does NOT break a cross-reference: pkgdown builds every topic's page and only skips it in the *index*, so `?tab`'s own `\link{tab_chi2}` (`R/tab-args.R:266`) keeps resolving. `tab_many()` and `tab_plain()` stay visible, in one "Superseded entry points" group — a 1.3.1 user searching the site for `tab_many` must find it.
- **`?fmt` split in three, on the type system's own line** — a **field** varies per cell, an **attribute** over a whole column, which is exactly the distinction the docs were hiding. `?fmt` (445 lines: the record, `fmt()`, `is_fmt()`), `?fmt_fields` (127: the 15 per-cell accessors), `?fmt_attributes` (225: the 24 per-column ones). Verified by conservation: the 41 original aliases are **exactly** the union of the three pages, the three sets are disjoint, and every `\usage` line is byte-identical to HEAD's.
- **`reg_formulas()` and `shape_numeric_var()` joined `_pkgdown.yml`**, which had never listed them — two public topics pkgdown could not index. An **index-completeness check** (public Rd topics == topics under `reference:`, both directions) is what catches this; it is one script, worth running before any release.
- **`?tab_plain` and `?tab_num` stopped teaching deprecated functions.** Both demonstrated `tab_prepare()`, and `?tab_plain` also chained `tab_chi2()`. ⚠ The `tab_chi2()` half could NOT be rewritten as `tab_plain(test = TRUE)`: `test` is declared for `producers = c("tab", "tab_counts")` only, so `tab_check_dots()` refuses it — the example was deleted instead. `other_if_less_than` is likewise `tab`-only, so the three legacy-step examples lost it rather than moved it.

**Two roxygen mechanics worth keeping.** `@describeIn` **does** resolve against a `@name`-only topic (the `@name tabxplor-options` + `NULL` house pattern), which is what makes a split like this cheap. And `@inheritParams fmt` on each new page's block pulls in exactly the parameters its own functions use — `fmt_fields` got `x` / `value` / `...` / `row_kind` / `in_totrow` / `in_tottab` / `in_refrow`, `fmt_attributes` its twelve — so a field is still described once, in `fmt()`'s block.

**Defects recorded in the review file**, three of them fixed above. The fourth is Phase 22l's: `vignettes/tabxplor.Rmd:546` and `vignettes/tabxplor-programming.Rmd:349` still describe `tab_plot()` as a working function, when it is `deprecate_stop()`. Also recorded, so it is not re-litigated: `get_type()` / `set_type()` / `get_ci_type()` / `set_ci_type()` were 1.3.1 exports removed with **no stub** — deliberate, because `fmt()` answers the old call with the full mapping (`fmt_abort_legacy_args()`).

**The proposals left on the table** (review file §5): Tier 1 is six exports never released and reachable another way (`new_lvl`, `is_lvl`, `get_model_family`, `set_model_family`, `set_row_kind`, `get_color_bg`) — 98 → 92, no stub, no `NEWS` line. Tier 2 is `tab_get_wrapped_dimensions`, which has **zero call sites in `R/`, tests, vignettes and `dev/`**, plus the three already promised internal in 2.1.0 (leave them: the promise is written). Tier 3, retiring the twelve un-taught 1.3.1 accessors behind `deprecate_stop()` stubs, is argued **against** — it buys no index space and twelve permanent stubs is code kept forever to save nothing a user sees.

**Verification.** `devtools::document()` clean, `NAMESPACE` unchanged; alias and `\usage` conservation asserted; the index-completeness check green (43 public topics, none missing, none stale); the five edited example pages run, and the only lifecycle conditions left are each deprecated page warning about **itself**. `devtools::test(filter = "fmt|steps-legacy|tab-structure|tab$")`: **FAIL 0 | PASS 477**. `NEWS.md` untouched — nothing here changes a user-callable contract.


---

### Phase 23 — documentation integration and simplification 2

The package documentation had grown cluttered with dev history and have lost focus. This phase enforces the **documentation ecosystem** hierarchy (top of this file) across every layer: present-tense, history-free, general to specific, basic use cases to expert territory, each fact stated once, *referencing more global or more precise docs rather than duplicating them*. Before writing any document, state what it is for, its focus within the ecosystem, and what belongs elsewhere.
- **Reducing the overall size of the documentation is critical**, since the current documentation in very verbose, technical, non-integrated, and have grown organically during development.
- But the *goal* is **not** to make a summary of current documentation: it’s more often to rewrite them **based on the *final* design, architecture and real-world usage**
- In each phase, start by reading the `## tabxplor architecture` section (top of this file) to get the big picture around which everything should revolve. Then, read `vignettes/articles/tabxplor-all-else-equal.Rmd` and, when French is needed, `vignettes/articles/tabxplor-all-else-equal-fr.Rmd`: they are currently **the most precise account of what tabxplor's *philosophy*, *vocabulary*, *usage* and *real-world regression use cases* really are**; its words are the package's own.
- Documentation should be clear, simple, focused, direct, no-bullshit, understandable by both machine and **human**. It should really help the people who need it most, "literary" social sciences students that discover programmation, R, and tabxplor, have difficulties, and are looking for . More experts statistical users should know everything they need to know to use the package in the way it’s intended, but in a clear, direct, focused way and not over-technical way (they don’t need to be told about the internals, etc.).
- Documentation should **never lecture the user**, but should **be clear and simple enough to be pedagogical**. Also, **the main use cases should be readable, accessible (not lost in the middle of an uncomprehensible heap of docs), understandable by "literary" social sciences students that hates math and fear programming**.


#### Phase 23a — vignettes simplification and integration — DONE

**The set is now five English vignettes, ordered teaching-before-reference**, plus the four French twins (Phase 23f). 3 111 -> 3 131 lines, which is the point: ~500 lines of duplication, dev-history and misplaced reference were cut, and the space went into the two things that were missing (a `display` section, the introspection accessors) and into `all-else-equal` becoming reachable offline.

| file | lines | role |
|:-----|------:|:-----|
| `tabxplor.Rmd` | 727 -> 583 | introduction |
| `tabxplor-all-else-equal.Rmd` | 743 -> 761 | **promoted from `articles/`** — the regression teaching route |
| `tabxplor-reg.Rmd` | 1326 -> 1250 | the regression reference |
| `tabxplor-weights.Rmd` | **new, 179** | weighted and survey data, both producers |
| `tabxplor-programming.Rmd` | 315 -> 358 | programming with the `fmt` cell |

**The introduction was reordered, and that is the largest single gain.** Colour — the package's whole idea — used to arrive at line 289, behind a 92-line survey-methodology essay and a battery-of-items detour. It is now the second section. jamovi is named in the opening (the maintainer's call: a reader who wants menus should not have to reach the end to learn they exist). Deleted outright: the 115-line `variable_type x color x color_signif` grid, which called itself "the reference behind the two color sections above" and duplicates `?tab`'s generated `@param ci_method` (`R/tab-args.R:341-369`) verbatim -- Wilson / Wald / beta / Newcombe / Agresti-Caffo / Welch / Student / OLS / robust / quasipoisson / Katz are all documented there. Added: a `display` section (named layouts, the `{}` grammar, per-token precision `{base:1}`, `set_display()` post-hoc), `tab_counts()` for already-counted data, the Excel -> Word route (which `R/tab_xl.R` is already engineered for), and a `tab_vars` + `spread_vars` demo on `tea`.

⚠ **`tea_when` was kept over `tea_where` on measurement, not taste**: max |diff| 23.0 vs 21.2 points, mean 6.5 vs 5.3.

**The regression vignette got a spine.** It had 46 headings and **no top-level heading at all**; it now has seven numbered parts (what a table is · the four arguments · one part per kind of outcome · reading what adjustment did · shaping the table · checking the model · plots). Three near-identical "For the record: what exactly is tested" appendices became one, in part 4, absorbing the "which paths carry a test" table; the interactions appendix keeps its own independent-samples maths and points there for the shared `color_signif` policies. The teaching passages `all-else-equal` now owns were cut or compressed (its "Three ways to get this wrong", "How to read it"), and the three `####` measure sub-sections merged. It opens by naming the article as the way to *learn* and itself as the way to *look up*.

⚠ **The reorder broke a data dependency**: the `tea` setup lived in the grouped-binomial section (part 3) and is used in part 1. Hoisted to the preamble beside `gss_simple`. Any future reordering of that file must re-check chunk order.

**Corrections, each verified against live output rather than by reading:**

- **`multiplier` was documented wrong.** The prose said "the default is **per one standard deviation** (`per 17.3 (SD)`)" while the chunk three lines below printed `per 34.6 (2SD), at 47.2 (mean)` -- the default has been `"2sd"` since 22g-v (`R/tab-args.R:617`). Fixed in three places, with the *why* added (2 SD is roughly the span of a binary predictor, so a continuous row and a two-level row become comparable down the column). The shape section's `1.22 per standard deviation` was likewise `1.45` per 2SD.
- **Two dangling cross-references**: "see the annex" (no such section) and "see *Reading the table* below" (no such section).
- `split_var`, a removed development argument, was still cross-referenced in two vignettes -> `tab_vars`.
- ⚠ The roadmap's *"the reg vignette now prints 40 shape tables"* was **already stale**: `options(tabxplor.shape_table = "no")` is set in the setup chunk and flipped on only around the shape section. Exactly 3 chunks print one.

**Part 6 answers the "right shape vs impossible shape" ask without new data.** Measured: `tvhours` is genuinely straight (curvature LR p = 0.95) while `age` reverses (p = 1.5e-274). Both were already in the example table; only the prose failed to say so. It now names the contrast, and the linearity bullet points forward instead of repeating the damage figure.

**The weights vignette is a move plus a merge**, not a rewrite. The introduction's three-rung ladder joins the regression vignette's own weights section, so `tab_reg()` is always at level 2 (or 3) while `tab()` starts at level 1 — a sentence that was already written in the introduction's vocabulary while sitting in a different document. The introduction keeps ~18 lines and a pointer; the regression vignette ~12.

**`all-else-equal` is a real vignette.** ⚠ It keeps `hdv2003`, and both `carData` and `questionr` join `Suggests`. The cheaper option was falsified first: §5's non-collapsibility example needs an adjuster *balanced* across the predictor yet *strongly* predictive of the outcome, and an exhaustive scan of `gss_simple` (best ×1.13 inflation at adjR² 0.031) plus `Arrests`, `TitanicSurvival`, `Chile` (which mixes real confounding into the arithmetic), `Wells` and `Salaries` found nothing near `hdv2003`'s ×1.6 with a flat age profile. The chunks are guarded so the vignette degrades rather than fails where a Suggests package is absent. `vignettes/articles/.gitignore` now covers `*.html` (the 351 KB knitted copy sitting there turned out never to have been tracked; it is a stale local build artifact of the old path and can be deleted at leisure).

**The ANSI leak is fixed, and it was two hooks, not one.** Every vignette sets `options(cli.num_colors = 256)` for its console examples and overrode knitr's **`output`** hook only. A `cli_inform()` raises a *message* condition and a `cli_warn()` a *warning* one, each routed through its own hook — so with `collapse = TRUE` they landed inside the collapsed source block with raw SGR codes. Both hooks are now set, in all nine files. Measured: 0 ANSI escapes across all five rendered vignettes, and the deliberate teaching warnings (the Brant rejection) render with proper `ℹ` glyphs. The notes are *kept and coloured*, not suppressed — they are part of what the package teaches.

**Programming vignette: one addition, six corrections.** Added `## Knowing what you have before you touch it` — `tab_structure()` / `tab_supports()` / `tab_columns()` / `fmt_attr()` / `reg_measures()`, which were absent although `NEWS.md` presents them as headline 2.0.0 features and their own help says to use them "before trying". Corrected: `tabxplor.xl_or_numeric` does not exist (it is `xl_ratio_cells`, with a different value vocabulary); `signif_levels`/`signif_labels` are superseded by `tabxplor.stars`; `resid` was listed among the 21 stored fields although it is derived from the p-value **and the sign of `ctr`**, so `vctrs::field(x, "resid")` would fail; the token list omitted `moe`/`sd`/`cv`/`coef`; the primary token is the first one **outside brackets**, and a token may carry its own precision; and a comment said "more decimals on the total row" above code giving it fewer.

**Registration**: `DESCRIPTION` (`carData`, `questionr` -> Suggests; `Config/Needs/website` back to `pkgdown` alone), `_pkgdown.yml` (both the navbar menu and the `articles:` index, where an incomplete list is a hard error), `README.Rmd`. Every "Where to go next" was rewritten over the five documents.

**Verification**: all five vignettes render in a cold `Rscript`; 0 ANSI escapes; no unintended chunk warnings. No test suite — the changes are documentation, plus `DESCRIPTION`/`_pkgdown.yml` metadata.

#### Phase 23b — roxygen documentation simplication and full rewrite — DONE

**`?tab` 648 → 352 Rd lines, `?tab_reg` 848 → 458** (1.84x and 1.85x; on the body alone, `\usage`
being fixed at 39 and 31 lines, 1.93x and 1.87x). Two thirds of both pages was `@param` prose, so
the cut is an argument-by-argument rewrite, not a section deletion. The standard applied to every
one: **what the argument is, its values, at most one sentence of when to change it** — recorded as a
`DESIGN:` line at the top of `R/tab-args.R` and in the documentation-ecosystem list, so the next
`doc =` written follows it. Teaching goes to the vignette that owns it, linked **once per page**.

⚠ **One premise of the brief did not hold, and it removed all the cross-page risk.** `tab_args_rd()`
emits a `TAB_ARGS` row only where it is a **formal** of the target producer, and the leaves take
everything through `...`; the exporters read `EXPORT_ARGS`, a different table. So **none of the
fourteen big `?tab` arguments appears on any second page**, no `doc_for` override was needed
anywhere, and the four sibling pages that do share a row simply got shorter for free (`?tab_ci`
124 → 107, `?tab_num` 72 → 69, `?tab_counts` 90 → 89, `?tab_plain` 71 → 70).

**A new topic, `?tabxplor-display`** (`R/tab-display.R`, the `@name`-only house-page pattern of
`?tabxplor-options`). The `{}` field list and the named-layout list were rendered on **three** pages;
one user-facing question — *what can a cell show?* — now has one page, carrying the grammar (the
three ways to ask, the **primary** token being the first one *outside* brackets, the per-token
precision `{base:1}`, `est`/`base` being scale-relative) plus both generated lists. `?tab` and
`?tab_reg` drop 54 and 27 lines and point at it. ⚠ **`?fmt` keeps `display_tokens_rd(user_only =
FALSE)`**: that rendering's own prose refers to the `fmt` fields glossed above it *on that page*, so
it works nowhere else — it is the programmer's exhaustive inventory, a different fact for a
different reader. It loses only `display_presets_rd()`. `reg_measures_rd()` likewise moved off
`?tab_reg` to **`?reg_measures`**, whose output it literally is; `reg_words_rd()` stays, being the
reading key for the object `tab_reg()` returns.

**The `?tab_reg` examples were wrong, and the defect was the pre-`link` slogan left behind.** The old
comment read *"the CONDITIONAL risk ratio: `measure = "ratio"` … fits the modified Poisson"*, which
contradicts the page's own `@details`. Traced through `reg_estimand_row()` (`R/reg-estimand.R:1058`):
with `link = "auto"` on a binomial, `effect` resolves to `"marginal"`. **Verified by running both**:
`measure = "ratio"` gives `Model_mRR`, *"logistic regression"*, ÷1.10 / ÷1.36; `link = "ratio"` gives
`Model_RR`, *"modified Poisson regression"*, ÷1.11 / ÷1.24. The pair is now a real contrast and is
commented as one. Two more went with it: the `if (requireNamespace("marginaleffects"))` guard (only
`effect = "at_reference"` needs that package) and a redundant `effect = "marginal"` that made two
examples the same table twice.

**Both example blocks were rewritten on the four shipped data sets** — no `gss_cat_data_formatting()`
preamble, no `head(data, 3000)` speed boilerplate, one idea per call, following *All else equal*'s
own running examples. Measured: the non-`\donttest` example is **0.92 s** on `?tab_reg` and well
under a second on `?tab`, the whole `\donttest` block ~3.3 s. `?tab` went from eleven calls (four of
them `color_signif` / `comp` / `ref` variants of one table) to seven.

**Eleven `@param` blocks stated a default the formal does not carry** — all now read
``NULL` (default) reads `options(tabxplor.<key>)` — <value>`. On `?tab`: `conf_level`, `color`
(said `FALSE`, formal `"no"`), `n`, `stars` (head and body disagreed), `cleannames` (stated none) and
`anova` (named the option, not its value). On `?tab_reg`: `n`, `ref`, `shape`, `color_signif`
(⚠ `"grey_non_signif"` is hard-coded in `R/reg-resolve.R:534`, **not** an option — and differs from
`tab()`'s `"ignore"`, a contrast worth its clause) and `conf_level`, which was **doubly wrong**:
"Default `0.95`" on a `NULL` formal, *and* a claim that the value does not come from
`options("tabxplor.conf_level")` when the default path is exactly that.

**The vocabulary rules.** ⚠ half was already written — `TAB_ARGS$color$doc` already opened *"Which
measure(s) of deviation to color"* and already carried the acronym rule (*an acronym names a
**measure**, `display =` a **field**, `ref2 =` a **level***); the work was to preserve both through
the cut. What was missing is now on `?tab_reg`'s `@param measure`, which opens *"Which measure of
deviation is reported"* and defines the pair once.

**Vignette pointers** are `\href{}` web links to the pkgdown site: `?tab`'s description to the
Introduction, `?tab_reg`'s to *All else equal* (to **learn**) and the regression vignette (to **look
up**), plus one `see <vignette>` clause on each argument that lost an essay. ⚠ **the English
vignettes already link to their French twins** (`tabxplor.Rmd:42`, `-reg:66`, `-all-else-equal:70`,
`-programming:65`); only `tabxplor-weights.Rmd` has no twin, which is Phase 23f-ii's.

**Two stale lines fixed in `vignettes/tabxplor-reg.Rmd:1108`**, both factual: *"(It needs the `car`
package.)"* — Phase 22l vendored `tx_vif()` and dropped `car` entirely — and *"Above about 5"*, where
the footer marks collinearity from **10** (`REG_CHECKS$collinearity$flag`); 5 is the first of the
plot's two guide marks, and the sentence now says both.

⚠ **`devtools::document()` rewrites `jamovi/i18n/fr.po` on every run, reverting ten committed
translations** (`noms simplifiés` → `noms nettoyés`, `étoiles de signif.` → `étoiles de
significativité`, …). Reproduced deterministically; `pkgload::load_all()` alone does **not** do it,
so it is a roclet-stage side effect, and it predates this phase. Nothing here touches jamovi:
**restore it before committing** with `git checkout HEAD -- jamovi/i18n/fr.po`.

**Verification.** `devtools::document()` clean and `NAMESPACE` byte-identical (the new topic is
`@name`-only, so it exports nothing). Contract asserted mechanically on both pages: `\usage`
byte-identical to HEAD and the `\item{<arg>}` **name set** unchanged — 35 on `?tab`, 26 on
`?tab_reg`, every formal still documented; the only `\item` lines that disappeared are the five
`\describe{}` entries of the moved Model-checks section. **Code identity proved by parsing**: all
171 top-level expressions of `R/tab_reg.R` `deparse()`-identical to HEAD, and on the four other
edited files the only differences are the intended `doc =` strings plus `tab-display.R`'s one new
`NULL`. Every example of both pages run and read. Index completeness green (44 public topics, none
missing, none stale). `devtools::test(filter = "non-ascii|display|args|options")`: **FAIL 0 | PASS
753**, the single warning being the pre-existing over-dispersion teaching one.

⚠ **Not run here: `devtools::check()`** — the release gate, which the maintainer runs.


#### Phase 23c — user messages simplication and focus — DONE

**The rule is now a cross-cutting invariant** (§ *Cross-cutting invariants*): a message is addressed to the person writing the call — what is wrong, or what was decided for them, and the argument that changes it, as code; one headline, at most one `x` and one `i`; never the package's own reasoning, never an internal, never a confirmation of what the user asked for or of what the table already shows.

⚠ **What made the phase cheap, established before any edit: cli condition messages are NOT translated.** `Config/potools/style: explicit` extracts only literal `gettext()`/`gettextf()`, and cli's inline markup is incompatible with it — the `.pot`'s 355 msgids are all *table* content. Exactly three runtime msgids exist, all in `var-shape.R`, and all three came through unchanged, so no `.po`/`.mo` work was needed. ⚠ One of them, `"%s: cut into four bands..."`, has an empty `msgstr` in `po/R-fr.po` — a **pre-existing** gap (25 of 352 msgids are untranslated), left for the French phase rather than guessed at.

**Two helpers replace two families of ad-hoc code** (`R/utils.R`, § *user messages*):

- **`tx_inform_once(id, ...)`** — the automatic-decision note. ⚠ **The id carries the SUBJECT, not the kind of message**: `paste0("shape_auto_", var)`, never `"shape_auto"`, or the note would be silenced for the *next* variable of the same session. 16 call sites, including the 4 that already hand-rolled `.frequency = "once"` with their own id — one spelling now. Its companion **`tx_reset_messages()`** puts a session back to its first-call state, which is what lets a test still assert on a once-per-session note.
- **`tx_need_pkg(pkgs, what, severity)`** — the Suggests gate, replacing **eight** spellings (four consecutive `stop(paste0("Package \"ggpubr\" needed for this function to work..."))` in `tab_plot()` collapse to one call). It names **every** missing package of one request in a single message, gives the exact `install.packages(c(...))` line, and teaches `install.packages("tabxplor", dependencies = TRUE)` — deliberately the only message allowed three bullets, because it is rare, shown once, and aimed at a reader for whom installing is the hard part. `severity = "inform"` where the feature degrades (mirai, kableExtra, brant, clipr, the Excel check images).

**Deleted, not shortened** — a message that only confirms: the survey-design detection (passing a design *is* the request), `plots.R`'s "Using `d`, the data tab_reg was called with", the "the footer reports the dispersion" half of the over-dispersion warning, and `tab_xl`'s re-emission of a raw internal condition. `"Excel file written to <path>"` **stays**: it names something the user cannot otherwise see.

⚠ **A second population of messages was found only by grepping for bare `warning()`/`stop()`** — ~45 lowercase, un-cli, unthrottled calls in the older code, invisible to a `cli_` search. 26 of them announced a forced default (*"since pct == 'row', a total column was added"*). They are gone: the added row or column is plainly in the table. ⚠ Measured first, because it decided the risk: the `tab-leaf.R` set is **unreachable from `tab()`** — `tab_resolve_settings()` forces the totals earlier — so it fired only from the exported `tab_plain()`/`tab_num()`/legacy steps. The genuine caveats among them (a `ref`/`ref2` matching nothing, no data left after `filter`/`na`, `fmt` arithmetic across incompatible columns) were kept and converted to cli with the standard shape.

**The aborts**: the ~30 blocks over the cap were rewritten (`reg-cross.R:246` went 13 lines → 3), and SHOUTY CAPS, `--` asides and every "why the package works this way" clause were swept throughout. Five internal invariants that printed a literal `"Internal: …"` / `"tabxplor: …"` now use `cli_abort(.internal = TRUE)`. `zzz-fact-keys.R` is untouched: it is a load-time developer check that must fail the install.

**Two defects fixed on the way.** `tab-parallel.R:101` used `rlang::warn` with cli-style braces, so the user read *the `{mirai}` package* literally — it is now a `tx_need_pkg()` call. And a `{?it/them}` written in an `"i"` bullet with no quantity **in its own string** threw cli's *"Cannot pluralize without a quantity"* instead of the intended error; cli does not carry a quantity across bullets, so `cli::qty()` must open the bullet that pluralizes.

**Counts**: informs 45 → 25, bare `warning()`/`stop()` on live paths 45 → 1 (a deliberate condition re-raise), the eight Suggests spellings → 1. Aborts stay ~210 because five bare `stop()`s became `cli_abort`; what changed there is their shape, not their number.

**Verification.** `devtools::test()`: **FAIL 0 | PASS 9988**. 25 failures were fixed along the way, in two classes — messages whose *text* changed (regexps updated) and messages now said once per session, where the test calls the same path twice: those take `tabxplor:::tx_reset_messages()` before the assertion, so the throttle stays covered rather than being disabled under test. One test lost an assertion outright (`test-tab.R`'s *"full total table"*), the warning it asserted being one of the deleted confirmations.

#### Phase 23d — drastic `NEWS.md` simplification
`NEWS.md` `# tabxplor 2.0.0 (in development)` was already drastically simplified in Phase 18y, but have since Phase z2 accumulated all dev history again. Most of it is really not user-facing and irrevelant here (and already in other dev documentation). A **drastic** reduction is needed here, no dev details **at all**, straight to the point, please **reduce around 400 lines to maximum 150 lines** :
- "## New features" should only list the most important things. New exported functions, like tab_counts() and others, should be presented in one quick sentence. New arguments in one quick sentence, rarely more. Everything about `tab_reg()` should be near-zero-words : in spirit, "possibility to do regressions added, see the vignette".
- Drastically reduce "## Changes that may affect existing code", only keep what is really important
- Drastically reduce bug fixes (same thing really), to only speak about very very few bugs that could have been hit by real user. Remove everything about any new argument or implementation. Make it small.
- Keep deprecation, reduce it’s size, list elements quickly, but keep differenciate what is soft deprecated and what is hard deprecated.

#### Phase 23e — Tests simplification
testthat tests have grown organically: it was right for development, but would slow future dev for no real benefits. I want you to select the tests that are *really* necessary, and to move the others to a folder of `dev/` scripts not run with `test`. **The full suite must go below 20 seconds** (parallelised, on this desktop computer).
- Study what would be the best tests to keep in priority. The ones that measure outputs, rather that the one that overfits the current "how" of the code ? The ones that tests main architecture and design of tabxplor framework and ecosystem of functions, and ensure integration, rather than the ones that tests for a specific implementation of a function whose internals could be done differently without hurting package integration ? What else ? Do not hesisate to make web searches about packages tests, the right and the wrong kind of tests for long development, focusing on long term package maintenance and simplifying the dev tests at release.
- Identify long tests and, if they are really necessary, think about how to make them faster without losing their reliability.
- If the preparation steps are computed several times for several tests, make an efficient common preparation.

#### Phase 23f — french translations

The aim is to create a **compact, yet holistic and integrated translation**: avoiding word-do-word translation of the english version altogether is your highest priority.

#### Phase 23f-i — all-else_equal vignette french translation — DONE

**The article has a French twin, and the phase's real work was terminological rather than linguistic.** `vignettes/articles/tabxplor-all-else-equal-fr.Rmd` is a rewrite, not a transposition: same five parts, same order, same 44 chunks, every sentence of prose re-argued. The three existing twins sit within ~1 % of their source's line count; this one does not, and that was the brief.

⚠ **The primary source was the maintainer's own teaching, not a dictionary.** `~/github/formations_stat/M2_06_07.Rmd:310-2100` is a complete logit séance, and it already settles most of this vocabulary in French for exactly this readership. Four of its devices are adopted wholesale: **« Lecture : … »** (the INSEE reading-key — the English article's six blockquotes *are* that form, and every French reader of a table already knows how to read it) · the « Rappel / Attention / Note » callout registers · reading a table in order (source, champ, Total, l'intérieur) · median-dot inclusive forms. Its terms carry too: « démêler les corrélations cachées », « écart à la moyenne » (of which the article's « écart » is the generalisation), and the three-case crude-vs-modelled rule at `:1858` (« son effet persiste » · « totalement expliquée » · « dilué dans le tableau croisé »), which is the five-verb taxonomy in better French than a translation would have found.

**The register is his own, minus its classroom half.** « on » for the generic rule, « nous » for the shared analytic move, no *vous* and no *tu*. Measured over the séance: on ≈ 73, nous ≈ 36, vous ≈ 18, tu = 0 — and *vous* there is only ever a TD instruction (« À vous de réaliser le tableau »), which an article has none of. The choice was a confirmation, not a compromise.

⚠ **The odds-ratio problem — the phase's core, and what the article's own comment at line 461 asked for.** In ordinary French « n fois moins de chances » states a *risk ratio*, so an odds ratio must be disambiguated. Two routes, and the article teaches both because the maintainer asked for both: **the clause** (« … *plutôt que de ne pas l'avoir été* »), which is the séance's own template and the more striking; and **the noun** (« sa *cote* est 1,48 fois plus faible »), which carries the distinction by itself and shortens the sentence. The rule: *the clause may be dropped only when « cote » carries it instead — never neither, never both at once.* « cote » is taught once from the racetrack, as the séance already does. Terminology stays **« rapport de cotes »** (the OQLF's preferred term, and one shared msgid) although the séance prefers « rapport de chances »: the table prints the former, so the prose works with it.

**Four more terminological decisions, all recorded in `dev/french_glossary.md` § *Le vocabulaire d'enseignement*** (123 new lines, flagged for the maintainer's manual review):

- ⚠ **« effet marginal » is a false friend for this readership.** Cibois — cited in this article — uses it for an effect *in percentage points*; `effect = "marginal"` means *averaged over the sample*. Defused at first use, with « effet moyenné » / « effet moyenné sur l'échantillon » alternating. The compensation is real: the « marge » etymology is **stronger in French**, since it is literally a Total row's margin.
- ⚠ **« base » was unusable** — the article's own running variable is *le nombre de **bases** de données policières*. The base is **« le socle »**; `display = "base"` stays English like every argument value.
- **crude/adjusted is a family, not a pair** (the maintainer's own correction): « observé »/« ajusté », « brut »/« ajusté », « empirique »/« modélisé », chosen per context — but **one per sentence**, never the hedged triplet `-reg-fr` writes today. « effet net » is canonical in French sociology and is mentioned exactly once, so a reader recognises it elsewhere.
- **Fitting a model rotates** — *ajuster* (canon), *estimer*, *calculer*, and *réaliser* from the séance. ⚠ One guard: never both senses of *ajusté* in one sentence.

⚠ **Rendering the twin found three package defects, and one of them had two sites.** `Model fit` and `predictors` (`R/tab-test-display.R:739, :772`) were bare strings never wrapped in `gettext()`; `observed shape (central 95%)` (`R/reg-assumptions.R:1333`) *was* wrapped but its msgid was absent from `po/R-fr.po` (only the bare `"observed shape"` existed). All three are fixed — wrapped, translated (« Bilan du modèle » · « prédicteurs » · « forme observée (95 % central) »), and added to `.pot` / `.po` / recompiled `.mo`. ⚠ **`Model fit` needed a SECOND wrap**: `R/tab_classes.R:1374` builds the html header from its own literal, so wrapping only the console site left all 11 occurrences English. Measured 11 → 0.

⚠ **« Bilan du modèle », not « ajustement du modèle »** — *ajustement* is this article's central word (the crude→adjusted move), and making it mean model fit as well would have been fatal on a page about adjustment.

**The English article was corrected too** (all four side-tasks approved). Twelve typos and one factual slip — l. 224 said the odds were *of being **arrested** rather than not*, where it means **released**. Three further passages were **stale or false**, each found by rendering rather than by reading:

- `per 1.54 (SD)` — Phase 22g-v made `multiplier = "2sd"` the default and the article was never updated; the row actually prints `per 3.08 (2SD), at 1.64 (mean)`, which also names the anchor. Rewritten with the *why* (2 SD ≈ the span of a binary predictor, so a continuous row and a two-level row become comparable at a glance).
- `0.930` against `0.938` for the two risk ratios — the tables print `÷1.08` and `÷1.07`, never a raw decimal.
- ⚠ l. 672 claimed tabxplor "refuses to test it, **and says so in the table's own legend**". It does not: no rendered legend carries any mention of the refusal. Both versions now state the absence.

**The three `<!-- TODO -->` comments are resolved.** The verification notes are settled against a real render; **l. 416** ("when is an outcome too common for an OR?") is answered with a rule rather than a threshold — *an odds ratio is always further from 1 than the risk ratio of the same comparison, and the gap grows with how common the event you **name** is*, so naming the rare complement brings them together; **l. 461** is answered by the two-route rule above, and deleted. **l. 699's bibliography is written, in both languages**, one entry per source with the idea it contributes in parentheses — and the French edition **inverts the framing**: Cibois, Deauvieau and Selz & Maillochon open it as the tradition the article extends, the anglophone works following.

⚠ **A French term that names an argument is given WITH its English code name**, at the point where the French word is coined and nowhere else — « la **variable à expliquer** / `outcome` », « l'**écart** / `measure` », « le **socle** / `base` », « le **bilan du modèle** / `stats` ». The reader thinks in French and types in English, and an argument only ever named in French cannot be used; the same names serve jamovi, whose options mirror the arguments. Recorded as a rule in `dev/french_glossary.md` § Rules, and **applied to both twins** so the two teach the same thing. Checking it against the chunks also found a gap present in the English since it was written: `shape` and `trials` are demonstrated and were never named in prose, in either language. Both are now named. (`na` is left alone — incidental to a crosstab call, not a teaching point here.)

**What the French gains and loses.** It gains `questionr::hdv2003`, a French INSEE survey whose levels print in French — so §5's case study simply names « cadres » and « ouvrier·es spécialisé·es » where the English has to gloss them. It loses the other three fixtures, whose English levels sit inside French prose: glossed once each where the data is introduced, never again per table. Four hand-authored labels are translated (the three model-comparison names and `is_prof`); everything else in the chunks is byte-identical.

**Registration, and a standing rule finally applied.** `_pkgdown.yml` takes both edits (the navbar « En français » block, and the `articles:` index — an incomplete index is a hard pkgdown error). README's *Learn more* gains the article, which was missing from it entirely, in `README.Rmd` and the knitted `README.md`. And CLAUDE.md's long-standing *"start the english vignettes with a link to the French one"* rule is applied — to all four, for the first time.

**Verified by rendering, four times.** Final French render: **0 English strings in prose or tables**, 5 « Lecture : » blocks, heading count and level sequence identical to the English (32/32), chunk bodies differing only in the three setup chunks, the nine ASCII comments and the four translated labels. Typography swept by grep: no decimal point in prose outside a quoted cell value, no `[0-9]%` without a space, no `$` before a number (money is « 14 088 $ »), guillemets spaced.

#### Phase 23f-ii — other vignettes french translations

The two references for French vocabulary are `vignettes/articles/tabxplor-all-else-equal-fr.Rmd` and `dev/french_glossary.md`.

#### Phase 23g — Code housekeeping and future-proofing
All facts tables, parameters tables, options tables, etc., and other tibble::tribble or the like used in code, should be well aligned for human readability, a human should be able to easily modify them with all the relevant informations structured, condensed and at the same place, visible at first glance, using a tribble if necessary (see, for example, the "print" black and white palettes).


#### Phase 23h – pkgdown site

Update the pkgdown site with the package documentation for release.
- Check at pkgdown references: is there organisation and structure still meaningful, clear, readable, useful for new users, reflecting the current architecture and main uses cases of the package ? Are all exported functions organised in the outline, or a some new ones wandering around ?

#### Phase 23i — `dev/` folder
Files inside the `dev/` folder have grown organically, with many now useless files and outdated ones, which is very messy for future development : I want you to clean and reorganise the folder and main files.
- Put all files related to v 2.0.0 dev history and of no real use for future dev in an 2.0.0 archive subfolder. That should be most of them.
- Only keep at `dev/` root level a few selected .md files that explain in detail the architecture or functioning or use cases of some subsystems, and will be really useful for future dev : clean these files, simplify them by removing useless dev history and focusing on current architecture and usage, ensure they are up-to-date compared to the current design and code ;  organise them internally in such a way that goals, design and architecture decisions, usage, and everything giving the big picture come first, and details come next ; reference them in the architecture document.



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
