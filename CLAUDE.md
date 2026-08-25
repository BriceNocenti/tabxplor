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

**Dependencies are pay-as-you-go:** table building and core inference are always available (hard Imports include `data.table`, `broom`, and the stats engines `survey`/`nnet`/`MASS`); exporters, plotting, parallelism, jamovi and advanced regression backends are all Suggests, guarded at their entry points.

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
- `tab-display.R` — the `{}` display grammar, its named layouts, the display-time base count; `DISPLAY_TOKENS` / `DISPLAY_PRESETS`.
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

- `tab_reg.R` — `tab_reg()`: fits per column, renders each estimand as cells, the staged `reg_build()`.
- `reg-resolve.R` — the `tab_reg()` argument boundary (`reg_resolve_args`, six stages + the tidy-select one).
- `reg-estimand.R` — the estimand cascade (family → link → measure → effect) and the library it composes; `REG_FAMILIES` / `REG_ESTIMANDS` / `REG_WORDS`; `reg_measures()`.
- `reg-digest.R` — `tabxplor_fitdigest`, the fit-free record of a fit; `REG_FIT_KINDS` / `REG_DIGEST_PARTS`.
- `reg-empirical.R` — the observed (crude) companion columns; `REG_EMPIRICAL` / `REG_EMP_BY_LINK`.
- `reg-influence.R` — the marginal engine (g-computation over `REG_LINK_FUNS`) + the gap-SE influence functions.
- `reg-assumptions.R` — model checks + `shape=` cures; `REG_CHECKS`; the plot primitives; the observed curves and their sparkline.
- `reg-cross.R` — interactions: the `a*b` entries of `predictors`, prepared as a variable; `REG_CROSS_ARMS`.
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

**Other directories:** `vignettes/` (user + regression + programming vignettes, each with a French twin; plus `vignettes/articles/`, pkgdown-only, which also holds the *All else equal* teaching article) · `tests/testthat/` (testthat v3) · `man/` (roxygen-generated, never edit) · `inst/i18n/` + `po/` (translations) · `jamovi/` (module definition) · `dev/` (architecture guide, dev scripts, perf harness, `.Rbuildignore`'d).

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

**The aggregate core** (`tab-leaf.R` + `tab-agg.R`) is the single place microdata becomes cells: the leaves `plain_core()` (factors) and `num_core()` (numeric column variables) turn sufficient statistics into `fmt` fields, their confidence interval and the whole-table test in one pass. The superseded dplyr-era steps (`tab_pct` → `tab_ci` → `tab_chi2` → …) are quarantined in `tab-steps-legacy.R`: still exported, they share the *arithmetic* (`ci_dispatch()`, `chi2_compute_test()`) with the leaves, so a step and a build cannot compute two different answers.

**The reference system:** `ref` picks the baseline a deviation is measured from (`tot` / `first` / an index / a regex), reinterpreted by `pct` (a reference *row* under row%/means, a reference *column* under col%); `ref2` names the second level for odds ratios; `comp` compares within each sub-table or against the total table. **Significance:** a cell is significant when its confidence interval excludes the **neutral value** — 0 for a difference, 1 for a ratio — and the displayed p-value and stars come from inverting that same interval, so colour, greying and stars cannot disagree. Interval geometry is declared in `CI_GEOMS`, its method in `CI_METHODS`.

### The inference layer

**The survey-design boundary** (`survey-design.R`) is one unwrap point: a `survey` design passed as `data` becomes the microdata every engine already reads, plus its sampling weights and design metadata — so the observed columns, the marginal effects, the tests and the footer are all design-weighted, and a `svyrepdesign`/`twophase` is refused rather than approximated.

**The inference basis** is the layer's central idea: how the *estimate* is computed (`wt`) and how the *interval and test* are computed (the basis) are **orthogonal**. The basis is one of `n` / `weights` / `design` / `design_partial` and — with `conf_level`, `degf` and `ci_method` — is stored **on each column, not on the table**, because `dplyr` drops table attributes and a number must never depend on one. A bind reconciles them by the weakest-claim rule.

**Design-based cell variance** (`survey-variance.R`) feeds the existing `n_eff` field, so the ordinary CI machinery becomes design-aware with no new field. A plain weight column is a survey design at `ids = ~1`, where the general formula collapses to a per-cell closed form computed from the aggregate alone (Kish is its degenerate limit); a real design goes through `survey::svyrecvar`, which owns the variance algebra throughout.

### The display grammar

What a cell prints is a `{}` template over declared tokens (`DISPLAY_TOKENS`), resolved by one boundary `tab()`, `tab_reg()` and `set_display()` share, so a layout learnt on a crosstab means the same on a regression. `{est}` and `{base}` are **scale-relative** — the deviation a column estimates, and the level it sits on — which is what lets one named preset (`DISPLAY_PRESETS`) render an odds ratio, a mean difference and a percentage alike. A composite has a **primary** token, the first outside brackets: it carries the stars, it is what `get_num()` and Excel return, and it is the only part the colour paints. A token may also carry **its own precision** (`{base:1}`), which beats every declared default — digits are a display property, and the cell's one `digits` field cannot say that an estimate reads at three decimals and its aside at one. **A display is post-hoc** — every field a layout can print is populated at build, so choosing one triggers no computation and changes no number, and a token may be **derived** rather than stored (`resid`, `gap`, `sd`, `cv`). A numeric column's default layout is `mean_cv` — the spread as a percentage of the level, comparable between columns measured in different units — chosen per column and falling back to the bare mean where a mean is not positive. The **base count** is the display-time fact both producers share: folded into the Total cell when the table rests on one population, given one `n` column per block at the right when it rests on several (a spread, a regression's groups) — and the per-block Total columns then go, holding nothing but a repeated 100 %.

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

**A parametrisation is decided while the data is prepared.** An `a*b` entry in `predictors` is *a predictor whose levels are combinations, and whose univariable model is its own saturated fit*, so it is materialised as a column before the fit and every subsystem keeps reading an ordinary predictor; `REG_CROSS_ARMS` (`reg-cross.R`) declares its two shapes — a combined factor against one common reference, or slopes nested in a moderator. `shape` recodes a continuous predictor the same way, and `ref` shifts one to its anchor so the fit's own intercept is already the baseline the Constant row shows. Two more decisions are made there for the same reason: `family = "binomial"` on a 3+ level outcome collapses it to one level against the rest, and `na = "keep_for_predictors"` turns each predictor's missing values into a level — cutting a numeric one, since a number has no level to hold them. One rule covers all five: **the boundary defines the model's variables, then fixes their origin** — and the fit's own output is already the table. Whatever it recodes, `reg_prepare_replay()` redoes, or a diagnostic refits a different model on the same rows.

**A fit is distilled, not kept** (`reg-digest.R`). Everything the table goes on to compute — a marginal effect, a baseline, an influence function, a coefficient at any confidence level — needs a model's `coef`, `vcov`, `terms` and `family`, never the fitted object; so `reg_fit()` returns a **`tabxplor_fitdigest`** beside it, and every engine reads that. Which parts a digest holds is declared, one row per fitting backend (`REG_FIT_KINDS`) and one per stored part (`REG_DIGEST_PARTS`), so **a new model backend is a row**. Nothing length-`n` is stored: the model frame is rebuilt from the live data through the *same* `reg_fit_frame()` the fitter used, and the IRLS working weights and residuals are reconstructed from the parameters. What only a fitted object can answer — the model-fit statistics, the global tests, the assumption checks, each crossed pair's test — is computed **eagerly, while it lives**, and rides on the record; what a digest genuinely cannot serve buys its fit back through `reg_digest_revive()`. Hence the record's one estimand-dependent member is `tidy`, written per `(measure, conf_level)` by `reg_tidy_finalize()` from a native-scale estimate — which is what lets the jamovi cache key on the **model alone** and serve every estimand from one fit.

**The boundary and the build** (`reg-resolve.R`, `tab_reg.R` + `reg-spec-build.R`). `reg_resolve_args()` is the crosstab boundary's twin, with `data` *inside* it — `family = "auto"`, `multiplier = "sd"` and `shape` are answered by the data — and one grammar per axis: the four estimand arguments per outcome, `multiplier` / `shape` / `ref` per predictor (unnamed = the fallback, named = that variable). `reg_build()` then runs over a typed `new_reg_ctx`, its per-model half a declared product (`reg_spec_build()`), the three nesting axes — `tab_vars` groups × models × outcomes — dispatching through the shared parallel seam. **A model comparison is a default too**: several `predictors` sets are tested against each other without being asked, sequential where every model nests in the next and against the first otherwise, decided in `reg_compare_rows()` where the fits exist. ⚠ `compare != "none"` is what makes a build serial and makes it keep its fits, so the boundary degrades the automatic one to `"none"` wherever a comparison has no meaning.

**Effects and model checks.** A marginal quantity comes from tabxplor's own analytic g-computation, or from `marginaleffects` at a reference profile — derived from the contrast, never declared per row. `REG_CHECKS` catalogues the checks (linearity, dispersion, influence, proportionality, collinearity), each with the `shape =` cure that fixes what it flags — the check and its cure are one object — each priced (`cost`) and each declaring whether it runs by default (`footer_default`), because what a table must say and what it costs are two questions. The **observed shape** of a numeric predictor is the free half of the linearity check: one curve per outcome, binned with no fit at all, drawn as a sparkline in a window floored by the data's own sampling noise and by the first colour rung — so a flat run means flat. It goes in the predictor's own `n` cell where the table has one outcome and the medium can hold it, and otherwise in a small **shape table** below the footer.

### Exports and rendering

`tab_export(x, format =)` (`tab-export.R`) is the facade over four backends: HTML (the default), Markdown, Excel and plot, sharing one preparation step — `tab_export_prep()` (`tab-export-prep.R`) builds an ephemeral render model (roles, references, faces, header spans, variable-name blocks) that every backend consumes. A spread swaps the two header bands, since after a spread a **column** is identified by its sub-population and a **block** by its variable: the column header takes the `col_group`, the span takes the `col_var` and, above it, the level only where that variable gives several columns per group.

Display values reach the backends by one source of truth: `format.tabxplor_fmt()` renders the text for console, Markdown and HTML, and `tab_xl()` writes a number with format codes from that *same* `format(syntax = "excel")`, so a display change never needs mirroring. **Excel keeps the cell a number and puts everything else in the code**: an aside becomes a column carrying its own segment (`(n={n})`), and every literal a template writes — the stars, the brackets, a sigma, a test label — folds into the numFmt, per section. A multiplicative cell holds its **reading value**, the signed fold, so `1/2.11` reaches the workbook without becoming text; text stays a property of a *cell*, not of a column. The exports' **unit row** is the console's own type tag (`<row%>`, `<n>`), written once per **block** — `tab_col_block_ids()`, the one definition of a block, which also decides where a vertical rule falls. Colour is single-sourced too — every backend reads `fmt_color_channels`. HTML colour is a slot **class**, never inline hex, the theme living in a `<style>` block from the one CSS generator (`tab-css.R`), so light/dark and the publication palettes work by stylesheet — except `print_marks`, whose signal is cell text and so comes from `format()` like the stars. `tab-transpose-render.R` flips a finished render model (a transposed column is heterogeneous and cannot be an `fmt` column), and `tab-theme-detect.R` best-effort-detects the console's scheme — a subsystem that must never error, because a wrong guess only mis-tints.

The **hover tooltip** (`tab-tooltip.R`) is that same rule read line by line: `TOOLTIP_LINES` declares one row per line — the token it renders, where its name comes from, which of the shared gates apply — and row order IS the reading order, so a line is named by its `DISPLAY_TOKENS$label`, exactly as the exports' unit row is, and one gate (non-empty · comparable · not the reference · not already shown · not already emitted) decides every one of them. It has **two rows**, declared the same way (`group`): the cell's own numbers, then the observed comparison — `obs` and the gap to it, a statement about another column — joined by a newline the stylesheet honours. It is **not translated**, deliberately: like the pillar type tags its words are the `fmt` field names, so the hover teaches the fields a user reads with `$`.

### jamovi

Two point-and-click analyses mirror the two producers: `jmvtab` (Crosstables) and `jmvtabreg` (Regressions). Each is a thin `R6` backend (`*.b.R`) over an engine-free build core (`jmvtab_build()` / `jmvtab_reg_build()`) driving `tab()` / `tab_reg()` through a content-addressed **live-UI cache** (`*-cache.R`), so an interactive tweak re-paints instead of recomputing. Each option is named after the argument it drives, so the backend is a pass-through, not a translation table — and where the panel asks a *simpler* question than the argument takes (a tick-box for `empirical`, two of `theme`'s seven values), R resolves the rest. An argument applied at RENDER (`theme`, `wrap_*`) is read straight off the options and deliberately kept out of `.opts()`, which is the crosstab cache key's complement. The regression store holds **distilled fit records** (kilobytes) keyed on the model alone, so every estimand change is a hit and nothing heavy crosses jamovi's `$state`. The generated `*.h.R` option headers are never hand-edited.

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
- **Roxygen man pages** (`?tab`, `?tabxplor-vctrs`, `?tabxplor-options`, `?tabxplor-data.table`) — user-facing reference: *usage* and the main use cases, never build/internals/history.
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
| `test-reg-shape.R`        | Phase 18z15: `shape =`, the plot primitives, the stored curves and the row sparkline            |
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

### Phase 22 — manual reviews and last features before release

Below are the results of the maintainer’s manual reviews of different features, stating the problems and what still needs to be changed before 2.0.0 release.
- Avoid *ad hoc* solutions, think about how to integrate the requested changes in the package ecosystem cleanly in a future-proof way. When framework changes are needed, state it clearly and plan for them. If you think they are too big and better done in their own Claude Code session, state it clearly and write a new phase in the @CLAUDE.md roadmap ("Phase 22x-ii", "Phase 22x-iii", etc.), but avoid to create too many different phases and regroup what is better done together (same context needed, or not big enough to get it’s own session).



#### Phase 22g — Jamovi UIs manual reviews and final modifications

#### Phase 22g-i — update jamovi UIs — DONE

**The two panels now say what the final API says, and `jmvtools::prepare()` runs again.** It had been unrunnable for two phases, so every jamovi-visible change since 20g-ii was inert and the Regressions analysis aborted outright. Both halves are fixed, the module is rebuilt, and the maintainer's live click-through is all that remains. Full detail — the compiler traps, the reserved-name set, the three defects — is in `dev/tabxplor_2.0.0_jamovi_dev.md` § Phase 22g-i.

**The compiler crash was one line, and the class of defect is silent.** `jmvtab.u.yaml`'s spacer `LayoutBox` carried a bare `children:` — a YAML **null** — left when 22b-ii retired the `ci_print` radios. `uicompiler.js:209` guards with `!== undefined` rather than truthiness, so it recursed into the null and line 205 evaluated `null.length`. ⚠ `children: []` is not the cure: an empty container is spliced out and the compiler then **rewrites the file with `yaml.dump()`**, comments and all. The node is deleted, with the rule written where it will be read.

**A second rewrite trap was found by tripping it: a `optionPart` radio group must cover EVERY value of its List.** `jmvtabreg`'s `display` offered 7 of 10, so `prepare()` appended three loose radios and cost the file all 24 of its comments (restored from HEAD). The orphans — `est`, `est_coef`, `base_ratio` — had been declared by 22a-i / 22b-iii / 22c-ii and never given a button; all ten are offered now, and the suite walks both files comparing each `(optionName, optionPart)` set against the `.a.yaml`.

**`family =` and `link =` are the Model table's two headed columns.** They are questions about each OUTCOME, so `link` moved from a scalar 4-radio column to a per-outcome hidden Array beside `family`, and `modelTableCtrl` renders a header row naming both — the 4th cell stays unheaded because it holds whichever of `outcome_level` / `trials` the row calls for. `measure` and `effect`, the cascade's right half, stay scalar radios and gain the freed width. `reg_per_outcome()` already accepted a named vector, so no R-core change was needed. The drop-down offers exactly `TABX_LINKS[family]`, which makes an unfittable link **unreachable rather than greyed** — deleting `linkOffered()` and the link branch of `applyModelEnables()`; `measureOffered()` asks each outcome with its own link. Measured: one table carrying `Model_RR [married]` beside `Model_diff [tv]`. ⚠ the four link labels lose their French — a JS-rendered label is outside `catalog.pot`, the property `TABX_FAMILY_LABEL` has had since Phase h. They are still **generated**: `reg_link_ui_labels()` composes them from `REG_MEASURE_LINK` (the measure, then the glm spelling that map already carries), emitted as `TABX_LINK_LABEL`.

**The interactions picker is built.** `crosses` had been declared and folded since 22b-ix with no control anywhere. `crossPickerCtrl` — its own *Interactions* CollapseBox after Model — is a row per pair (two drop-downs over the predictor pool, a delete) writing the array `jmvtab_reg_cross_keys()` folds into `predictors` as `a*b`. Picking the variable already on the other side steps that side to the first free one, because `a*a` is a refusal.

**Three defects the wiring exposed, all reproduced and fixed:**

1. **`rlang::inject()` + `!!` hid every interaction.** `reg_cross_slots_quo()` fell back to `quo_peek_extern()`, which reads only a bare **symbol** — but `!!` splices a *literal* vector, so `a*b` was never seen and tidyselect tried to select a column of that name. `jmvtab_reg_build()` injects on purpose (a value cannot be hijacked by a same-named column), so the `crosses` fold could never have worked end to end. Fixed in `reg-cross.R`, where crosses are read.
2. **`empirical = "no"` was a declared value the validator refused.** `TAB_ARGS` declared `c("no", "cell", "column")` and `emp_on()` accepted `"no"`; only `reg_validate_args()` held a literal `c("cell", "column")` — so the picker's own off value aborted. It reads the fact table now.
3. **The per-outcome arrays must fold in ONE place.** `family` / `outcome_level` / `trials` resolve inside `jmvtab_reg_build()`; `link`'s fold sat in `.b.R` at first, so the raw array reached `tab_reg()` and the pick did nothing. All four resolve together in the build core — which is also what keeps it testable from the raw jamovi arrays.

**⚠ `check` is a reserved name, exactly as `levels` is.** `jmvcore::Options` defines a `check()` method, so an option of that name cannot be bound at all (`makeActiveBinding: symbol already has a regular binding`, raised from the generated `.h.R`, naming nothing). The Excel model-check control is **`xl_check`**, beside `xl_replace`; it drives `tab_xl(check =)` and `jmv_backend_export()` hands it the module's own `data`. Measured: 2 images in the workbook at `"auto"`, 0 at `"no"`.

**Smaller, all in the same sweep:** the crosstab `display` ComboBox gains `mean_sd` / `mean_cv` (22c-iii made `mean_cv` a numeric column's default cell and neither was offerable); `empirical`'s three values gain the one-clause titles its sibling Lists have; six unguarded `ui.<hidden option>` reads in both `.js` files gain the `if (!ui.x) return;` their neighbours already carry (an undefined read there throws and takes the whole `update` handler with it during the `.h.R` lag); and the stale `ci_print` entry leaves the test's UI-only allow-list.

**A syntax gate for the `.js`.** `node --check` on both files, skipped where `node` is absent. Declined in 19n for want of an interpreter — the box has one, and it is the only thing between a typo and an options panel that renders inert with no R-side symptom.

**The build chain ran here** — `prepare()` → `i18nUpdate("catalog")` → `i18nUpdate("fr")` → `install(home = "flatpak")` — so the committed `.h.R` / `0000.yaml` / `fr.po` / `inst/i18n/fr.json` are the compiler's own and `tabxplor_2.0.0.jmo` is installed. Verified by checksum that `prepare()` left both `.u.yaml` files untouched. **French:** 12 msgids disappeared (all genuinely retired — `add_n`, `ci_print`, the old `display` values) and the 37 new ones are translated by hand, following the catalogue's own rule of keeping the English argument name and translating the parenthetical; `dev/french_glossary.md`'s terms are used (« mesure de l'écart », « effet marginal / conditionnel », « prédiction ajustée », « risque relatif »). Compiled `fr.json`: 199 of 228 translated, the rest argument VALUES kept English on purpose.

**Deliberately out of scope, recorded under Phase 22x:** a `shape` picker for `jmvtab`, and `spread_vars`.

**Still open:** the maintainer's live pass — the Model table's two headed columns and its per-outcome drop-downs, the greying of `measure` / `effect` against a mixed-family outcome set, the Interactions box, the crosstab `display` ComboBox, an Excel export with `xl_check`, and the French UI on the Windows side.


#### Phase 22g-ii — a round of `tab_reg()` review to enable the next `jmvtabreg()` changes — DONE

**Two arguments became defaults, and that is the whole phase.** `empirical = TRUE` and an automatic model comparison are what Phase 22g-iii needs on the R side before it can turn the jamovi Regressions panel's `empirical` List into a checkbox and delete its `stats` control. Everything else here is a defect the new defaults exposed, or a review item that came with them.

**`empirical` is `TRUE`, and one value now says where the crude effect goes.** The mode vocabulary is `"no"` / `"tooltip"` / `"cell"` / `"column"`, and the three behaviours fall out of it with no second flag: only `"column"` draws a column (`emit()`, `reg-empirical.R`), only `"cell"` writes a layout, and all three still compute `obs` and `gap_se` — so `color = "adjustment"`, `forest_plot()` and the hover work in every one of them. `TRUE` resolves to `"column"`, except where drawing one would double a table already wide — **`tab_vars` groups** and a **per-category outcome** — which take `"tooltip"`: computed, printed nowhere, **silent** (the maintainer's call; a console-only reader is not told, the field is enough). Auto therefore no longer resolves to `"cell"` at all, which is what gives a **multinomial its `est_base` default** like every other family.

**The in-cell fold stopped being a per-cell rewrite.** `reg_set_obs()` wrote `dplyr::if_else(hit, "{est} ({obs})", d)` cell by cell — the one aside the layout could not report, which is why `reg_meta_obs_in_cell()` had to re-derive it from `meta$crude_keys` for the footer's `Model:` line. `"cell"` is now the **`est_obs` preset** (`DISPLAY_PRESETS`), `"({obs}) {est}"` — the aside **first**, matching `est_base`'s own `emp` arm and the maintainer's item 1 — so `reg_display_of()` resolves it like any other layout, `reg_meta_aside()` collapses to one line and `reg_meta_obs_in_cell()` is **deleted**.

⚠ **That exposed a reported defect and fixed it: `display = "{est} ({obs})"` was a silent no-op.** `display_write_col()` prunes a bracket group whose field is void on every row, and the column builders wrote the display **before** `reg_set_obs()` filled `obs` — so the preset degraded to `{est}`, exactly as a user's own template did (Phase 22i, defect 1). The display is now written **again** in step 5 of `reg_spec_build_one()`, the first point every field a model cell can print exists. Measured: `display = "{est} ({obs})"` and `"{est} ({gap})"` both render now.

⚠ **A second defect the default made visible on nearly every table: a baseline row printed `(51%)`.** `est_base` overwrote the Constant row's own `const_display`, whose field carries the number wherever `const_display == base_display` (`pct_ratio`, `mean_ratio`, `raw_diff`, `points` — i.e. every gaussian and every `measure = "ratio"` table), leaving the primary void and only the aside in brackets; `get_num()` returned `NA` there. `display_write_col()`'s per-cell rule gained one clause: **where the primary is void, a cell keeps its own token if that token is not the column's estimate** — i.e. it was stamped with something else on purpose. ⚠ the predicate must be "not the estimate", not "renders something": the documented counter-example (a numeric predictor's observed cell, which has an odds ratio and no risk difference) carries the estimate token itself and must still take the template — measured both ways.

**`<mixed>` column headers are gone** (maintainer's item 4). The root cause was the same fact from the other side: `fmt_display_label()` votes on the primary token of every data cell, and the Constant row's `const_display` differs from `est_display` on every ratio and difference scale — so `Model_diff` (a plain gaussian default) and any `measure = "ratio"` column rendered `<mixed>` with `empirical = FALSE`. **A column is named by what it estimates**: where the scale's `kind` is `"effect"` and the primaries disagree, only the templates carrying the scale's own `est` token vote. Four lines, two declared facts, no per-cell work, and it cannot reach a crosstab (every crosstab column's scale is `kind = "level"`). The maintainer's own proposal — an NA primary plus a composite Constant template — was not needed: nothing was wrong with the cells, only with the vote.

**The comparison is the default** (maintainer's item 3, with his own API: no new user-facing value). `reg_resolve_stats()` splits what used to be one bucket — `FALSE` / `"none"` → no footer at all; `NULL` / `TRUE` / `"all"` → `compare = "auto"`; a named character vector → no comparison, *because naming the footer statistics drops it*, the way naming any argument's values drops the rest. Which comparison is read off the models in `reg_compare_rows()`, the first place they exist: **sequential where every model nests in the next**, baseline-vs-the-first otherwise. It is silent throughout — no "needs at least two models" note on a one-model table, no ΔAIC lecture (the ΔAIC **row** still appears; it names itself).

⚠ **The default nearly cost every table its parallelism and its dropped fits.** `compare != "none"` is read in three places that have nothing to do with the footer: `reg_specs_independent()` refuses parallelism on it, `reg_spec_build()` **keeps the fit object** on it (which is exactly what Phase 22j stopped doing), and `reg_resolve_args()` **hard-aborts** on it with several outcomes. So `"auto"` is degraded to `"none"` at the one point where both facts are known — `!(prep$is_comparison && length(prep$predictors) >= 2L)` — and `prep$is_comparison` already implies one outcome, so the abort can never fire on a default. Verified: an ordinary and a multi-outcome table both still resolve `compare = "none"`. ⚠ the nesting test for the ARM reads **term sets only** (`reg_compare_chained()`), not `reg_compare_guard()`: the guard also requires equal `nobs`, which is a property of one `tab_vars` group's missing data, so it would have picked sequential in one group and baseline in the next and put two kinds of comparison row in one table. A differing N is still caught per pair, where it falls back to ΔAIC on that column alone.

**The tooltip has two rows** (maintainer's item 2). `TOOLTIP_LINES` gained one declared column, `group`; `obs` and `gap` take group 2, lines join with `" ; "` inside a row and with a newline between rows, and a cell with nothing to say in group 2 gets no trailing newline. `.tooltip-inner` / `.popover-body` move from `white-space: nowrap` to **`pre`** — identical for one-line content, which is why the only golden diff in the whole phase is those two CSS lines. The native `title=` path needs nothing: `htmlEscape(attribute = TRUE)` writes the newline as `&#10;`, which every browser renders as a line break. ⚠ group 2 must be the LAST row, because `reg_append_empirical_tip()` appends the multinomial crude level onto an already-joined string; that is asserted at load (`tx_check_tooltip_groups()`).

**The shape table's footer legend is one line, and only where it is earned** (maintainer's item 6). Three or four note lines became **`"ns": the curve is inside its own sampling noise -- read it as flat.`**, emitted only when a row wears the mark. The window moved into the header — **`observed shape (central 95%)`** — because it is sound: `rd_bin()` clips only the sparkline's x AXIS so one outlier cannot squash the curve, while the curve is binned on all the data. "on the model's scale" is dropped: the `range` cell already prints its own units (`43-87% (OR 8.7)`). ⚠ the ordinal / multinomial caveat is a **correctness** statement, not verbosity — it says the drawn curve is only the first of several — so rather than delete it, it moved into the `outcome` cell where it is read: `marital (1st curve)`. All four consumers (console / md / html / Excel, plus Excel's row-height reservation) now tolerate an empty note.

**Footer legends: a legend never emphasises itself more than the cells it describes** (maintainer's item 7). Exactly two emitters set the bold flag, both the column/level **name** prefix; both are plain now. The threshold **numbers** needed no change — they are coloured tokens whose weight comes from the palette face, i.e. exactly the face of the cells they grade, which is the maintainer's own criterion. With both gone the `b` flag was dead: `bold =` leaves `.lg_tok()`, `b` leaves both constructors, and `is_bold_tok()` is `tok_face(tk, "bold")`.

**`ci_method` rides `...`** (maintainer's item 8b), leaving 25 formals. Its `TAB_ARGS` row declares `dots = "tab_reg"`, `tab_dots_expand()` refills the declared `NULL`, `tab_check_dots()` already accepted it (it reads the declaration), and the `@param ci_method` prose moved into the hand-written `@param ...` block. A typo still says *"Unknown argument `ci_methd`. Did you mean `ci_method`?"*. **`na = "drop_all"` keeps its name** — the maintainer's own second thought: `drop_all_models` reads as "across all models", too close to `drop_by_model` and silent about the outcome axis it also collapses.

**Two more defects found and fixed on the way.** ⚠ **A refusal fired too late**: `measure = "ratio"` on a negative outcome is refused by the fitter, but the crude block runs first and now runs by default, so a user met `NaNs produced` before the honest abort. `reg_check_ratio_outcome()` is now called at the boundary, off `deps$est[[i]]$fit == "mr"`. And the `empirical` **degrade note** is explained only where a word asked for it (`is.character(empirical)`) — on by default, it would otherwise lecture every compound-formula and crude-less table about an argument nobody typed.

**Not done, by decision.** The **shape mini-plots** (maintainer's item 5): the maintainer chose to keep the current shape table as it is — one rendering, consistent across console / html / md / Excel — since a real plot needs a different answer per renderer and `reg_check_plots()` already serves anyone who wants axes. The x-axis window is not added either.

⚠ **Open for 22g-iii: two jamovi surfaces now say the wrong thing, and both controls are ones 22g-iii deletes.** `jmvtabreg.a.yaml`'s `empirical` List still defaults to `'no'`, so jamovi's default differs from R's; and `stats_compare = "none"` folds to `stats = NULL`, which now *compares* — a picker labelled "none" that produces a comparison. Neither yaml was touched here: any edit needs a `jmvtools::prepare()`, which 22g-iii will run anyway, and the maintainer has in-flight edits to the crosstab yaml files. Also recorded, and locked by a test: **a model comparison keeps its fits, so `reg_fit_cacheable()` refuses the store** — a comparison panel re-fits on every live edit, which is what the staged Run button already exists for, and is consistent with Phase 22h's recommendation to cut the regression cache.

**Tests.** `helper-reg.R` was the load-bearing repoint: `reg_first_fmt()` returned the first non-`n` fmt column, which with the new default is the **`Obs_`** one — so hundreds of assertions would have kept passing against the wrong numbers. It selects by the stored `get_role() == "model"` now, with `reg_first_emp()` beside it. Beyond that the triage was: a test *about* the bare layout gained an explicit `empirical = FALSE`; a test about crude *columns* under `tab_vars` gained `empirical = "column"`; the multinomial in-cell block split into the two modes it is now two of. New locks: the four modes and what each draws · `est_obs` renders the aside first and prunes an NA counterpart · the auto comparison's two arms, its silence and its degrade · a baseline row no longer names its column `<mixed>` · the tooltip's two rows and their order (and that a crosstab tooltip stays one line) · a legend's names plain and its break-words not. `_snaps/golden.md` regenerated: **64 changed lines, every one of them the two `white-space` rules.** Full suite **FAIL 1 | PASS 9905** — the one failure is `test-jamovi-vocabulary.R`'s STALE `jmvtabreg.js` block, the maintainer's own in-flight edit, unrelated and already recorded under Phase 22h.


#### Phase 22g-iii — jamovi UIs manual review — DONE

**Both panels now ask exactly what `tab()` / `tab_reg()` ask, and the widgets stop moving under the pointer.** The maintainer's click-through of the 22g-i build found three kinds of defect and all three are closed. Full technical detail — the compiler and jamovi facts that cost time to establish — is in `dev/tabxplor_2.0.0_jamovi_dev.md` § Phase 22g-iii.

⚠ **Scope decision (maintainer): the interaction picker is NOT folded into the model-comparison builder here.** That is **Phase 22g-iv**, below, with its design already chosen.

**`measure = "log"` is `measure = "coefficient"`, and it is now TOTAL.** The UI exposed a real R-side hole: `"log"` was refused on an identity link ("this outcome's coefficient is already additive"), so it greyed out the instant a gaussian outcome joined a binomial one — a mixed-family table could not be asked for its coefficients at all. The refusal became a **fall-through**: where the base link is already additive there is nothing to un-exponentiate, so the request resolves to the additive row itself, which IS the model's own coefficient. Four lines in `reg_estimand()`, and every `(family, link)` now answers. The value is renamed for what it is (`log`, `coef`, `coeff`, `log_odds` / `log_risk` / `log_rate` stay accepted spellings, the last three still pinning a base), and `REG_MEASURES_VALUES` is re-ordered simple→complex — `auto / difference / ratio / odds_ratio / coefficient` — as `tab()`'s own `color` reads. The header composition needed nothing: `reg_word_logged()` wraps only exponentiated measures, so a logit column stays `log(OR)` and a gaussian one reads `diff`. ⚠ **On `link`, `"log"` still means the LOG LINK** — `REG_LINK_ALIASES` is consulted first for exactly that reason, and it is the one word the two arguments do not share. ⚠ **A globally simple→complex LINK order is not available**: each family's `fits` order is load-bearing (its first entry is the family's own link, which `link = "auto"` resolves to), so the Model table's drop-down stays "auto, the family's own, then the rest". `REG_MEASURE_LINK` itself is reordered, which reorders `REG_LINKS_VALUES` and the generated link labels.

**`tab(shape =)` reached jamovi, and that meant the cache.** A numeric row variable was silently auto-banded because the panel never asked. The picker is the level box's numeric row — no collapsible, just `<b>var</b> : numeric` and a drop-down — and its two value lists are DERIVED from `VAR_SHAPES$produces` by the generator (`TABX_SHAPES_INDEX`: a row/tab variable can only be CUT; `TABX_SHAPES_COL`: a column variable may also keep a number), which is the same rule `shape_refuse_numeric_index()` enforces R-side. ⚠ `"auto"` leads the index list and stores NOTHING, because `shape_value()` aborts on it: it is the absence of an entry, not a value. Three new `VAR_SHAPES` rows — **`median` (k=2), `terciles` (3), `deciles` (10)** — give the drop-down the ladder an R user gets by typing an integer.

⚠ **The load-bearing half is the cache, and it had a silent trap.** `shape` is the second thing jamovi can change that recodes a column BEFORE it is counted (the level merge was the first), while `ce$fp_map` fingerprints the RAW columns — so without a key entry a cut would be served the un-cut aggregate. Both now travel as ONE per-variable slot (`jmv_cache_aggregate()`'s `recode()`) in the tier-1a, tier-1b and tier-2 keys; `JMVTAB_CACHE_SCHEMA` 19 → 20. **The trap**: a numeric-KEEPING shape (`log` / `sqrt`) on a column variable renames it to `log_age`, and `fp[["log_age"]]` is **NULL rather than an error** (`[[` on a list returns NULL for an unknown name), so the SOURCE column's fingerprint dropped silently out of the key and a data edit would not have moved it. `shape_rename_transformed()` now returns its rename map, `tab()` carries it on the ctx as `shape_renames`, and every fingerprint lookup in the aggregate goes through it. Both halves are locked by tests: a cut equals `tab()` and MISSES tier 1; a `sqrt` column's key still moves when its source data does.

**`stats =` is gone from the Regressions panel, not relabelled.** 22g-ii made the model comparison automatic, so `stats_compare = "none"` produced a comparison — a picker naming the opposite of what it did. All three controls are deleted (`stats_compare`, `stats_baseline`, `stats_checks`), `jmvtab_reg_stats()` with them, and the panel sends `stats = NULL`, i.e. `tab_reg()`'s own default. `stats_checks` only ever existed because the old digest hid the fit-based footer rows, and Phase 22j's eager stage removed that reason (a test now locks that those rows survive a cache HIT). ⚠ **`forceNaForCompare()` had to be re-keyed, not deleted**: it watched `stats_compare`, and without it a user comparing under `na = "drop_by_model"` silently gets a bare ΔAIC instead of an LR test. It reads the model-card COUNT now.

**One predictor subset with several outcomes builds.** `is_comparison <- is.list(predictors)`, and a comparison must have ONE outcome — so a single model card beside two outcomes was refused, although it is an ordinary per-outcome table. `jmvtab_reg_models(..., flatten =)` returns a character VECTOR there (the card's typed name is what that costs; the column is named by its outcome anyway); two cards and two outcomes is still refused, correctly, and the message names both cures. The builder greys `+ Add model` past one card while several outcomes are selected, so the error is reached only by a user who insists.

**The level box stops resizing, and all three causes were in one style string.** `grid-template-columns: 1fr auto minmax(96px,1fr)` sized column 2 to the tick-box (so it moved the moment one appeared) and grew column 3 with whatever was typed in it, while `overflow-y: auto` took width away once the list scrolled. Fixed pixels on the two right columns, `width: 100%`, and **`scrollbar-gutter: stable`**. The three column heads go `#555` → **black**, the stray `border-left` on the merged-name column goes, the summary reads "click to reorder / merge", and the box is titled **Reorder, merge and cut levels**. An **ORDERED** factor keeps every tick-box and gets its ▲/▼ **greyed**: merging two contiguous ordinal levels is meaningful, moving one is not — which is why `levelsCache` now carries `measureType` beside the labels.

**Three near-identical `requestData("column", …)` blocks per file became one `fetchLevels()`.** It is what caches `measureType`, and it is the kind of duplication the review kept tripping over.

**The two References pickers say only what their producer offers.** A still-numeric crosstab variable gets **no row at all** — the old "numeric — compared with its total" line named a choice `tab()` does not have. A numeric variable CUT by `shape` does get one, offering `Total` / `First group` / `Last group`: ⚠ its group labels are computed R-side from the data's own quantiles, so the JS cannot list them, and those three are the whole of what `ref =` accepts without a label. On the regression side the box **splits in two** — the same *Reorder, merge and cut levels* and *References (points of comparison)* boxes jmvtab has, driven by the same widgets — and a numeric predictor's **`ref` anchor** (`mean` / `median` / `min` / `max` / a typed number), reachable from `tab_reg()` but from no control until now, joins `multiplier` on its row. ⚠ **One deliberate difference**: the regression level box has no ▲/▼ bar at all, because `tab_reg()` has no `levels_order` argument — a move would write nowhere.

**Layout, both panels, in two equal columns throughout.** jmvtab: the CI-methods nested CollapseBox is dissolved into its parent behind a bold heading (two clicks became none), `conf_level` / `stars` become equal columns, `test` / `anova` / `design_effect` each carry their OWN label (the bold `test = …` title used to sit above all three and read as naming them), and *Other formatting* is rebuilt general-to-specific: `theme`, then `totaltab` | `add_pct`, then the two `wrap_*`, then `n` | `digits`. jmvtabreg: the per-outcome Model table sits in a grey material card with black heads, `display` (now a ComboBox, 10 radios gone) and the `empirical` tick-box take the cascade's own two columns below `measure` / `effect`, `color` moves in beneath them (its main use, `adjustment`, is a statement about the observed effect declared just above), and *Display* becomes *Other formatting* mirroring jmvtab. The jmvtab `display` ComboBox is retitled name-first (`base_ci <i>(value + interval)</i>`), shorter than before so the `color_signif` column beside it keeps its width.

**`theme` is one new control in both panels — and it is NOT in `.opts()`.** `light` / `print_ready`, read straight off `self$options` by `jmv_backend_theme()` and passed to `tab_html()` and to every exporter, exactly as `wrap_rows` / `wrap_cols` are: a theme is applied at RENDER, and `.opts()` IS the tier-3 cache key's complement (`JMV_TAB3_REAPPLIED` is a negative set), so putting it there would make a palette flip rebuild the table. `"print_ready"` resolves per table through `PRINT_READY`, so the one value is right for a crosstab and for a regression.

**`empirical` is a tick-box** (default TRUE, the argument's own), and R decides WHERE the crude effect goes. The one case jamovi can no longer express is a multinomial with `"column"` — expert territory, accepted. **`xl_check` is a tick-box** labelled *assumption checks plots*, in export row 1 beside Format, folding to `tab_xl(check = "auto")`; the `"all"` value stays in the R API.

**The hidden state carriers take no vertical space.** `visible: false` on `cache_state` (both analyses) and `compare_state`. Verified safe rather than assumed: the compiler's `Image` schema allows it, and `jmvcore`'s `ResultsElement$asProtoBuf()` writes `state` in a branch independent of the `visible` field — so the store round-trip is untouched. Still a live-check item.

⚠ **One defect the reorder created, and the gate that now catches it.** `jmvtabreg.js`'s `MEASURE_OF_RADIO` maps a radio's CONTROL NAME to the value it sets, and re-ordering `measure` moved every pair — so `applyModelEnables()` greyed the wrong buttons while every existing test stayed green (the value-coverage rule only asks that the five declared values get five buttons, not that the JS agrees with WHICH). `test-jamovi-vocabulary.R` now walks every `*_OF_RADIO` object literal in both `.js` files against the `(name, optionPart)` pairs of the `.u.yaml` that names those radios. Verified by re-breaking it: the gate fails with the control, the file and both values named.

**Tests: the overfitted ones cut, the structural ones added.** Deleted from `test-jamovi-vocabulary.R`: the three "retired options are GONE" blocks (pure history, pinning past renames in place) and the hardcoded `stats_compare` value list. Kept — and they earned it again — every block that derives its expectation from an R fact table. Added: the `shape` cache pair, the source-fingerprint guard, the `coefficient` fall-through (`reg_estimand("gaussian", measure = "coefficient")` is `identical()` to the `difference` row), the flatten rule and a mixed-family `coefficient` table, and that the fit-based footer rows survive a served digest. `test-var-shape.R` now also asserts a named quantile cut and its integer twin cannot disagree.

**The build chain ran here** — checksum → `dev/generate_jamovi_js.R` → `jmvtools::prepare()` → checksum (both `.u.yaml` **unchanged**) → `i18nUpdate("catalog")` → `i18nUpdate("fr")` → `install(home = "flatpak")` — so the committed `.h.R` / `0000.yaml` / `catalog.pot` / `fr.po` / `inst/i18n/fr.json` are the compiler's own and `tabxplor_2.0.0.jmo` is installed. **French: 201 of 208 translated**; the 7 left are bare argument VALUES / option NAMES kept English on purpose, plus the package DESCRIPTION blurb.

**Still open: the maintainer's live click-through** — the level box under repeated expand / tick / type (the width must not move), an ordinal variable's greyed arrows with live tick-boxes, a numeric row variable's `shape` picker and the table it makes, the two References boxes in both panels, the CI panel's two columns, `theme = "print_ready"` in the results and in an Excel export, the export row's *assumption checks plots*, the Model card and its promoted `display` / `empirical` / `color`, a two-outcome table with one predictor list, and that the state carriers take no vertical space **while the cache still hits**.

#### Phase 22g-iv — jamovi UIs polished "References and levels" common UI table — DONE

**The two boxes that listed the same variables twice are one table, and it is ONE widget both panels show.** `levelsCtrl` ("Reorder, merge and cut levels") and `refPickerCtrl` ("References") each enumerated the same variables, in two different row shapes, in two collapsibles — and shared `levelsCache` through an `afterFetch()` cross-call, so a merge tick in one repainted the other. They are now a single `varTableCtrl`: **one row per variable, aligned columns, the level list opening inline underneath the row it belongs to.** The NUMERIC row sets the layout and a factor fills the same cells: `<group>` · `levels / shape =` · `ref =` (· `multiplier =`, `jmvtabreg` only). One grid per group holds that group's head row AND its data rows, so a header cannot drift from the column it names — the defect `TABX.mtRow` / `TABX.mtHead` still guards with a comment.

**The live-UI defects the maintainer reported are all consequences of there being two controls, and they go with it.** A numeric row variable absent from the reference picker, a factor offering its pre-merge levels, a picker "always a few steps backwards": each was a signature disagreeing with the other control's writes. There is now ONE signature and it names **only what the table does not itself write** — the variable boxes, plus `pct` / `color` / `display` in `jmvtab` (which decide the reference AXIS and whether an odds ratio is in force). ⚠ `levels_order`, `levels_collapse`, `shape`, `ref_levels`, `ref2`, `multiplier` are deliberately OUT of it and repaint IN PLACE (`tabxvRefreshVar`): with one control, putting one back would rebuild the whole table on every merge tick and every reorder move, which IS the "2nd click does nothing, then all changes appear later" bug — the rebuild clobbers the edit that caused it. So a tick or a move now repaints that variable's reference cell (its choices are the post-merge levels) and nothing else.

**The shared half grew; the per-panel half is one declared object.** The generator's existing `BEGIN/END SHARED` mechanism carries it — `shared_block()` copies the span from `jmvtab.js` into `jmvtabreg.js` and `check` mode fails on drift — and it now holds the table itself, the level list, the column fetch (`fetchLevels` / `cachedLevels`, written twice before), the state, and the three generic option verbs `arrGet` / `arrWrite` / `reconcileArr` plus `makeSelect`, which existed only in `jmvtabreg.js` while `jmvtab.js` re-implemented them **six times** (`refSelected`/`writeRef`/`reconcileRefLevels`, `shapeSelected`/…, `storedOrder`/`writeOrder`/…). What is left outside is `VAR_TABLE_HOST`: the columns, the groups, the signature, what a `shape` drop-down offers, and the one genuinely divergent cell. Deleted outright: `renderTree`, `makeVarNode`, `makeNumericNode`, `makeTitledBox`, `buildVarBody`, `makeRegVarNode`, `renderLevelTree`, `renderRefPicker`, `renderRefVarCard`, `renderRef2Section`, `refLineControl`, `choicesHasRef`, both `refSig`s, `openState` / `mergeOpen`, three `last*Sig` variables and fifteen dead `TABX` style keys. **Net: `jmvtab.js` 1003 → 966 lines and `jmvtabreg.js` 1458 → 1511, of which 610 are the shared span** (274 before), i.e. ~340 lines of duplication became one copy.

⚠ **`levels_order` is reached through `host.orderOpt`, never by name.** `tab_reg()` has no such argument, so the shared code naming the option would make it claim one panel cannot declare — and the wiring test says so.

**Three behaviour rules, decided with the maintainer.** (1) In `jmvtab` an **off-axis** variable's reference cell is **EMPTY** — it still gets its levels row, but a reference the table does not use is not offered. (2) **`ref2` has no control of its own any more**: while an odds ratio is in force (`orIsActive()` — the colour names it, or the display's PRIMARY token is one), it borrows the reference cell of the **first variable of the other axis**, with a `title=` tooltip saying what it is. The standalone section and its explanatory line are gone; the hidden option is untouched. (3) A **cut** number takes positional references — `Total / First group / Last group` in `jmvtab`, `first` / `last` in `jmvtabreg` — because the group LABELS are computed R-side from the data's own quantiles. Both keywords are the package's own (`REG_LEVEL_KEYWORDS`), so `"last"` IS the maintainer's "first cut level in decreasing order"; no new vocabulary. ⚠ And a cut predictor **loses its `×` scaling**, which is not tidiness: `reg_check_continuous_names()` ABORTS on a `multiplier` naming a factor. `host.varSync()` drops a stored value whose vocabulary the new `shape` invalidated, so a pick can never leave an option that aborts the build.

⚠ **A gap the merge closed by accident, and it is worth knowing**: a numeric variable on an axis that cannot hold one (a crosstab's row / table axis) is ALWAYS cut — `shape = "auto"` still cuts, it is the ABSENCE of a stored value, not a refusal — so it has groups and now has a reference. The old picker gave it none.

**One structural gate added, and the vocabulary ones removed** (the maintainer's call, mid-phase). Added: *every `CustomControl` declared in a `.u.yaml` has `<name>_creating` / `<name>_updated` exported by its `.js`, and every such export names a declared control* — precisely the failure a rename across two files produces, and it is silent (jamovi renders an empty box and no R code runs). Removed: both `test_that("… speaks tab()/tab_reg()'s vocabularies")` blocks, which asserted that a List option's VALUE SET equals an R vocabulary **in content and in order**. A panel chooses which values to offer and how to order them for a reader (`no` last, simple before complex); pinning that to the R declaration made every ordinary UI edit a test failure — five of them were red at HEAD from the maintainer's own in-flight label work — while catching nothing a user meets. The value-coverage assertion is KEPT and rewritten, because it is not a vocabulary check: the ui compiler appends a control for every value a radio group leaves out and then **rewrites the `.u.yaml` with `yaml.dump()`**, deleting every comment. ⚠ It also gained the fix for a real latent trap: **YAML 1.1 reads a bare `optionPart: no` as the boolean FALSE**, so the check now spells booleans back rather than demanding the file quote them.

**One stated deviation from the spec.** `ref =` is the COLUMN HEADER rather than a prefix repeated on every row: the purpose given was to teach the R argument name, and a headed column teaches it once instead of once per row while leaving the drop-downs their full width. Same for `shape =`, in the `levels / shape =` head.

**The build chain ran here** — `dev/generate_jamovi_js.R` → `node --check` → `jmvtools::prepare()` → checksum (both `.u.yaml`, both `.a.yaml` and both `.h.R` **unchanged** — no `yaml.dump()` rewrite) → `i18nUpdate("catalog")` → `i18nUpdate("fr")` → `install(home = "flatpak")`. **French: 44 msgids translated by hand**, of which only 2 are this phase's (the two merged CollapseBox titles); the other 42 were the maintainer's in-flight label rewordings, whose old translations the regenerated `fr.po` had dropped. The 8 left untranslated are bare argument NAMES / VALUES kept English on purpose, plus the header. Full suite **FAIL 0 | PASS 9794**.

**A second round, after the maintainer's live pass, closed five more items.**

**The layout is one grid PER GROUP of variables, with a single header row whose first column is the group's own name** ("Row variables" / "Column variables" / "Predictors"), and air between them. The two stacked header rows are gone. Column 2 carries the whole level statement — `8 levels: click to relevel`, and just `8 levels` while the list is open — so the last column is free: `jmvtab` drops to **three** columns (it has no per-variable scaling and a 4th was dead width) and `jmvtabreg` keeps a narrow `multiplier =` one. ⚠ the count is the ORIGINAL one: a merge is a statement ABOUT those levels, not a new set of them.

⚠ **The bug where jmvtab showed no variable names was the grid template.** `minmax(0,1fr)` for the name column behind 510px of fixed columns collapses to zero in a narrow options pane — the names were rendered and invisible. Every name column now carries a `minmax(90px,1fr)` FLOOR, and the fixed columns are smaller (165/180/85). The same fix went to the Model table (`minmax(70px,1fr) 150px 120px 105px`), whose `outcome_level` / `trials` column fell outside the panel.

**Greys instead of white, so the widget reads as part of the options pane**: the table `#E4E4E4` (a shade darker than the pane), its header row `#CCCCCC`, an expanded level list `#F0F0F0` inside an `#E4E4E4` well. Drop-downs, text boxes and buttons stay pure white — now the only white, and therefore the thing that reads as "this is an input".

**`cleannames = TRUE` — the jamovi default — is honoured throughout the widget.** `cleannames_condition()` is transcribed into the `.js` (`TABXV_CLEAN`), and every level a human reads goes through it: the list, both reference drop-downs, the merged-name placeholder. ⚠ **Every stored value stays RAW** — `data-lab`, `<option value>`, `levels_collapse`, `levels_order`, `ref_levels` — only the text is cleaned. ⚠ The regex is built with `new RegExp` inside a `try`, because its lookbehind would be a PARSE error on an old engine and would take the whole file down rather than one label's prefix; the fallback arm drops the lookbehind. And the default name of a merged run changed **in R**, once, in `new_lvl_collapse()`: the first level whole, the followers cleaned — `1-Protestant, Catholic`, which `cleannames` then finishes into `Protestant, Catholic`. No ordering prefix can land mid-name, for a jamovi user or an R one.

**`jmvtabreg` gets the ▲/▼ bar, and with it the rule that a regression's baseline IS its first level.** `tab_reg()` has no `levels_order`, so the panel declares its own hidden option of that name and `jmvtab_reg_build()` **relevels the predictor columns before the fit** — in RAW level names, because `.levels_collapse` merges afterwards and `fct_collapse()` keeps the order of first appearance. It needs no cache entry: `jmvreg_fit_key()` fingerprints the prepared frame's levels, so a relevel moves the key by construction. The two controls are then two views of one fact, kept in sync in ONE place (`host.varSync()`): picking in `ref =` moves that level — or that whole merged run — to the front of the order, and any reorder or merge re-reads `ref` off the order. The first level is **bold** in the list.

⚠ **`theme` is a RESERVED jamovi option name, and that was the whole bug.** jamovi injects its own global `theme` (the app's plot-styling preference) into every analysis, so ours never held a value: no radio matched, a click reverted, and `jmv_backend_theme()` read the app's word and fell through to colour. Renamed **`tab_theme`** in both panels — the same cure as `levels` → `lvs` and `check` → `xl_check`, and the third member of a class worth remembering: *a jamovi option name can be taken with no error raised anywhere.*

**A third round fixed the sync itself, and the two defects it exposed were both mine.**

⚠ **The sync was one move behind because the ORDER was written last.** `tabxmBuildList`'s `move()`
did `commit()` (which fires `onCommit`) and only then `onOrder(order)` — so a host that DERIVES
something from the order read the option back before it had been written. In `jmvtabreg`, whose
reference IS the first level, that is exactly "click ▲ three times: the bold moves, the `ref =` cell
and the table keep the old one; click ▼ and the previous state finally lands". The order is written
FIRST now, with the reason stated where it will be read.

**And the reference cell had no way back into the list.** `regRefToFirst()` wrote the new order but
the OPEN level list was never rebuilt, so picking a baseline moved the model and left the list
showing the old order. `tabxvRebuildList()` does it — ⚠ only for a change made OUTSIDE the list:
calling it from the list's own `onCommit` would detach the grid that handler is about to repaint.

⚠ **`jmvtab` froze at startup on `ReferenceError: tabAxisVars is not defined`, and `node --check`
could not see it**: rewriting the host block took two helpers out with it (`tabAxisVars`,
`tabRef2Cell`), and a `.js` that PARSES but calls a function that is gone leaves the options pane
loading forever, with no R-side symptom at all. Both are restored, and the suite gained the cheap
static half of what a linter would do: **every identifier a `.js` CALLS must be declared somewhere
in it** (or be a known global). Verified by re-breaking it — the gate names the file and the missing
function. It is deliberately permissive about scope; the one thing worth catching is a top-level
helper that has vanished.

**The Model table overflowed its own right margin** — `width: 100%` PLUS `margin: 4px 6px` is 100% of the container plus 12px. Both `mtRow` and `mtHead` drop the width and let the block fill what the margins leave, so the row now ends where the card does.

#### Phase 22g-v — yet another regression manual review — DONE

**Sixteen review items, every one of them reproduced against `carData::Arrests` before a line was written.** Four were framework changes and the rest were defects with one named cause each. Four decisions were taken with the maintainer and are what the phase is built on: `reg_measures()` states the **factorisation** rather than the product; **digits join the display grammar**; `stats` gains an `"auto"` default so `NULL` can mean nothing; `raw_coefficient` is **conditional-only**.

**The vocabulary and the three defaults** (items 1, 6, 7, 10). `measure = "coefficient"` is **`"raw_coefficient"`**, with `raw_coef` / `raw_coeff` / `coefficient` / `coef` / `coeff` / `log` / the three `log_*` spellings all resolving to it — permanent aliases, which is also what lets the installed jamovi module keep sending its own word until 22g-vi renames the yaml. `multiplier` defaults to **`"2sd"`** (Gelman 2008: a binary predictor's contrast spans about 2 SD, so a continuous row and a factor row become comparable at a glance) — ⚠ the *fallback* was a second `"sd"` literal inside `reg_resolve_fit_plan()`, reached whenever `multiplier` is NULL, i.e. on every jamovi call; it reads `TAB_ARGS$multiplier$default` now. `color = "measure"` is the default spelling, one clause beside `"auto"` in `reg_normalize_color()`, which makes the two-channel headline `c("measure", "adjustment")` an ordinary character vector instead of the `c(TRUE, "adjustment")` that `c()` coerced anyway; ⚠ deliberately NOT a `MEASURES` row, since `measure_nameable("reg")` is what the refusal enumerates. And **`stats = NULL` now means no footer**: the signature default is the word `"auto"`, `NULL` / `FALSE` / `"no"` / `"none"` all hide the whole footer, which is what the vignettes' many `stats = NULL` calls always meant.

**`raw_coefficient` is conditional-only, and the rule had to be made uniform.** `reg_compose_log()` emits a log twin of the conditional rows alone (binomial 35 → 23 composed rows), and the refusal is derived, naming its cures — including, where the base is fittable, the link that estimates it. ⚠ **the cost is real and was paid**: `log(mRR)`, `log(mOR)` and `log(refOR)` no longer exist, and six assertions across three files were testing machinery through them (the crude block's borrowed Katz arm, the logged baseline's `log_of` rule). Each was moved to the model that estimates the measure — `link = "ratio"` for a log risk ratio — which exercises the same code and states the new rule; the one that was ABOUT a marginal log ratio now asserts the refusal. ⚠ The subtlety cost a round: 22g-iii's *identity-link fall-through* set `logged <- FALSE` before the refusal could fire, so `effect = "marginal"` was refused on a logit link and silently answered with a **marginal difference** on an identity one. `asked_raw` now outlives the fall-through — the rule is about the word the user typed, not about the row it lands on.

**Digits became a system, and no `fmt` field or column attribute was added** (items 8, 9). (1) ⚠ The `×1.4`-where-`×1.44`-was-meant defect is a **declaration** bug, and the first two attempts at it were wrong: in `format()` a stored `0` means *unset* and is what lets a token's own `min_digits` apply, so `REG_CELL_DIGITS["score_ratio"] = 1L` did not raise the `ratio` token's 2 — it **silenced** it. Applying the minimum with `pmax` instead fixes that but breaks `set_digits(x, 1L)`, which must mean 1; declaring 0 fixes both but strips the Constant row's mean of its decimal, because the cell's `digits` IS the level's precision (its own comment says so). The honest fact was simply missing: **`EST_SCALES$est_digits`**, the mirror of `base_digits`, for the three scales whose estimate is finer than the level it sits on (`score_odds_ratio`, `score_ratio`, `mean_ratio`) — a floor, not an assignment, so a user asking for more still gets more. (2) The `{}` grammar gained an optional **per-token precision**, `{base:1}` / `{est:3}`, parsed in `parse_display_template()`, range-checked in `validate_display_template()`, and applied through a new internal `format(.digits =)` that beats every declared default. (3) `tab_reg(digits = 0)` is the 26th formal: a scalar is a **floor** on the stored field, a named entry writes the suffix into that column's resolved template — matched both as written and as the column's scale resolves `{est}` / `{base}`, so `digits = c(ratio = 3)` finds a column whose template says `{est}`. Both halves run in one post-hoc pass (`reg_digits_write`) at `reg_stage_finalize`, the way `set_display()` is post-hoc. ⚠ Excel and the split-off aside needed two more lines: the top-level `format()` returns the numFmt **before** the composite expander, so it reads the primary token's own suffix itself (`display_primary_digits()`), and `mat_aside_cols()` writes the primary back braced when it carried one. Measured: `#,##0.0000;\1\/#,##0.0000` in the workbook, and `1/1.23 (2.7)` / `1/1.225 (2.696)` / `1/1.23 (2.696)` for the default, `digits = 3` and `digits = c(base = 3)` — the estimate at two decimals and the mean score at one, each raisable on its own.

**The numeric predictors** (items 5, 12, 13, 15, 16). `multiplier = c(checks = 1)` printed only `"at 0"` because ONE vector carried both the scaling factor and the label and `k == 1` was dropped from it; they are filtered separately now — the arithmetic still drops a no-op, the **label is descriptive** and says `"per 1, at 0"`. `rd_bin()` binned a 7-value predictor into 10 quantile bins and `rd_spark()` interpolated them onto 20 glyphs, drawing slopes between values nobody observed: a variable with no more distinct values than bins now gets **one bin per value**, and the sparkline draws at most one glyph per bin. `shape_cut_quantiles()` deduplicated its snapped breaks with `unique()`, which is why `quartiles` gave 3 groups where `quintiles` gave 4 **on the same column** — `shape_fill_breaks()` fills back up at the distinct values the quantiles missed, greedily and weight-aware, and a genuine shortfall is now stated once. And a whole-numbered column names its **values** instead of the interval holding them: `0`, `1 or 2`, `3 to 6` — safe exactly where it applies, since the breaks are already snapped to integers. Measured on `checks` (0–6): `quartiles` → `0 | 1 or 2 | 3 | 4 to 6`, `quintiles` → five groups, `deciles` → six with the message. ⚠ the labels are frozen into the spec and hashed into the jamovi cache key, so those keys move — correctly. Item 5's shape table blanked its `group` with `duplicated()` over the whole frame while the rows arrive **variable-major**, so every row of the second numeric variable read as a repeat: the rows are sorted `(outcome, group, var)` and blanked by RUN, within the outcome.

**Two new capabilities** (items 2, 3). `family = "binomial"` on a 3+ level outcome models `outcome_level` (the first level by default, said once) **against the rest, merged** — and the collapse is done at the argument boundary, beside the anchors and the relevels, NOT in `reg_fit_frame()`, because the crude block and `reg_check_plots()`'s replay rebuild the frame independently and all three must see one column. Parity checked against a hand-collapsed `glm()`. `na = "keep_for_predictors"` keeps every predictor's missing values as an ordinary `"NA"` level — the same `fct_na_value_to_level()` call and the same level name `tab()` uses at its three leaves — dropping only a missing **outcome**; a numeric predictor that has any is cut (`sd_bands` by default, any cut `shape` overriding, a numeric-keeping one refused with the cure named), decided where the shapes are resolved and applied after the cut. Measured on `hdv2003`: `qualif NA` (347) and `heures.tv NA` (5) get their own rows and their own effects. ⚠ both recodes join `reg_prepare_replay()`, or a diagnostic refits a different model with the same row count.

**`reg_measures()` went from 35 rows to 6** (item 11), because the grid factors: a conditional row is a property of the **link** (a model's coefficients carry exactly one measure) while a prediction-based row is a property of the **family** — g-computation averages fitted probabilities and does not care which link produced them. So the table is one conditional row per fittable link, then the prediction measures once at `link = "(any)"`, with `effect` reading `marginal|at_reference` where both exist. `status` and `note` are gone: an unavailable combination has no row, and the family-level refusal is one `cli` line. `family = "auto"` now lists **every family the outcome kind offers**, the detected one first, which is the choice a reader makes before any other — and the new one-vs-rest binomial appears there by itself, which is the declarative payoff.

**Two smaller ones** (items 14, 4). The doubled model-name header had a de-duplication guard all along (`tab-export-prep.R:687`); it failed in html and kable alone because `tab_export_prep()` wraps the column NAMES (U+202F per space) before the header is built and leaves the `col_var` attribute raw. It compares through a shared `tx_unwrap_text()` now — the second site with that exact blind spot, the first being `tx_strip_outcome_suffix()`. And `ci_method = "profile"` **works**: `get_ci_method()` returns `"profile"` and the bounds move (`[5.92;7.16]` Wald vs `[5.93;7.17]`); the stars did not change on the maintainer's table because the two intervals agree there. Nothing to fix.

**Tests.** Seven failures were **pre-existing at HEAD** — `test-var-shape.R:66-69` and `test-reg-cross.R:161-163`, both expecting band words (`"; < mean - \u03c3"`) that `shape_band_words()` stopped writing an earlier phase ago; they are corrected here since both files were being touched anyway. The genuine updates: the two `multiplier` tests that encoded `"sd"` and the old silent `k == 1`, the multinomial survey parity that needed `multiplier = 1`, the crude multinomial's own per-SD hand fit, the two `reg_estimand()` blocks the conditional-only rule moved, `reg_measures()`'s block rewritten against the new columns, and the one that asserted a 3+ level outcome REFUSES `family = "binomial"` — now the capability. ⚠ one real regression caught by the suite and fixed: the `stats` rewrite made the *named-footer-set* branch return `FALSE` instead of the vector, so `stats = c("n", "aic")` produced no footer at all — `none` (keep what was asked, drop the comparison) and `nothing` (no footer) are two lists now. ⚠ a second one, caught the same way: capping the sparkline at one glyph per bin (the other half of item 13) broke the shape table's own rule that **every curve is drawn at the same width**, so two of them can be compared — reverted, since `rd_bin()`'s value-binning is what removes the artifact and what it resamples is then an honest step function. Measured on `checks`: `████▇▇▇▆▆▆▅▄▄▃▃▂▂▁▁▁` became `███▇▇▇▆▆▅▅▄▃▃▂▂▁▁▁▁▁`.

**New locks**, kept to what the phase earns: the four `stats` spellings and that a named set keeps what it names · `digits` scalar / named / the `{est:4} ({base:1})` suffix, and that suffix reaching the Excel numFmt · a tied column giving k quantile groups, its shortfall message, and integer labels that hold the same rows as the interval ones · the shape table naming every group (⚠ verified by re-breaking it: the gate fails with the old `duplicated()` blanking) · the one-vs-rest binomial against a hand-collapsed `glm()`, and its announcement · `na = "keep_for_predictors"` keeping N, giving an `"NA"` row per predictor, and refusing a numeric-keeping `shape` · the model name printed once in every backend, html included · `ci_method = "profile"` moving the bounds and not the estimate.

Full suite **FAIL 0 | PASS 9572**, and **no golden or snapshot moved**: every output change in the phase is behind an argument nothing default reaches, except the digits of a grouped-binomial cell and the span row of a `predictors` comparison, neither of which a golden table builds.

**⚠ Open for 22g-vi, created here** (all one build-chain pass): the `measure` option value in `jmvtabreg.a.yaml` is still `coefficient`, and `dev/generate_jamovi_js.R` emits BOTH spellings into `TABX_ESTIMANDS` until it is renamed; the `digits` ComboBox (`jmvtab`'s own 0-6 control, one pass-through) is not in the Regressions panel; `na`'s new `keep_for_predictors` value is not in the picker; and `TABX_OUTCOME_OFFERS` now offers `binomial` on a 3+ level outcome, which the panel will show with no yaml change.



#### Phase 22g-vi — yet another round of jamovi manual review — DONE

**Both panels now say what the 22g-v API says, and three of the maintainer's live findings turned out to be one R defect each rather than a UI tweak.** The build chain ran once for the whole round. The jmvcore facts that cost time — and would cost it again — are in `dev/tabxplor_2.0.0_jamovi_dev.md` § Phase 22g-vi.

**Two review items had a false premise, and saying so is the finding.** ⚠ **`mean_sd` / `mean_cv` already apply to numeric columns only.** `display_write_col()` (`R/tab-display.R`) has a per-COLUMN void rule: a preset whose fields are all `NA` on a column prunes to nothing and that column keeps its OWN display. Measured on a mixed table — `age` takes `mean (σ17)`, every pct column stays `<row%>` — and symmetrically `base_or` on a numeric column leaves it `<mean>`. Nothing to build, in `tab()`, in `tab_reg()` or in `set_display()`; `set_display()` on one column still overrides absolutely, which is what the maintainer asked to keep. ⚠ And **`na = "keep_for_predictors"` was already in the picker** (`jmvtabreg.a.yaml`, radio `na_4`, and the generated `.h.R`) — that carried-over bullet was stale.

**What `base_diff` actually was: a crash.** It is offered by the `display` ComboBox and baked into `R/jmvtab.h.R`, and there was no such preset — `display_resolve()` fell through to `validate_display_template()` and aborted the analysis. It is one `DISPLAY_PRESETS` row now, `"{base} ({diff})"`, the missing sibling of `base_ratio` and `base_or`; a test walks **every** value the ComboBox offers through `display_resolve()`, which is the rule the crash broke.

**The `print_ready` shape table was a CSS collision, and the markup was never wrong.** Rendered under `light` and under `print_ready` the html is byte-identical. What differs is `tab_css()`: under every publication palette `.tx-sec` gains `display:inline-block` (load-bearing there — it is what takes an aside out of an ancestor's `text-decoration`), and `shape_html_table()` put that class **on the `<td>`**, which destroys `display:table-cell`. The cell drops out of the row and reflows under its neighbour — which is exactly "some curves in the outcome column". The grey moved to a `<span>` inside the cell, with the warning written where it will be read.

**Every jmvtab export died on `xl_check`.** ⚠ jmvcore's `$.Options` **`stop()`s** on an unknown option; it does not return `NULL`, which is what the code's own comment assumed. `xl_check` is Regressions-only. The one guarded read is now `jmv_opt(self, name, default)` (`R/jmvtab-export.R`), and the rule is stated in that file's header: a shared `jmv_backend_*` helper may only reach for an option both panels declare. Third member of a class worth remembering, after `levels` → `lvs` / `check` → `xl_check` and `theme` → `tab_theme`.

**The staged comparison that flickered and vanished: root-caused in jmvcore, not measured live.** `Image$asProtoBuf()` reports an image that HOLDS A STATE and rendered no file as `ANALYSIS_RENDERING` — and a hidden state carrier writes no file by design, so it is *always* in that state. Left VISIBLE the client then asks for the render, and that round-trip overwrote the run's own results: the table appeared and was replaced by the "Model comparison staged" banner. `compare_state` had its `visible: false` **commented out** (as had `jmvtab`'s `cache_state`); both are hidden again, and `ResultsElement$asProtoBuf()` writes `state` in a branch that never reads `visible`, so it costs nothing. ⚠ A second, independent hazard found in the same function: **`$state` warns past 5e5 compressed bytes**, and the render stored its whole HTML there. It now stores the signature always and the HTML while it fits, with a two-entry process-local mirror (`JMVREG_RENDERS`) that re-serves inside a live engine either way — strictly more robust than before in both directions.

**An odds ratio is read against a category, never against a total** (`R/tab-leaf.R`, `plain_resolve()`). `ref = "tot"` under one was not a choice but a leftover from the table the user was reading a moment ago — and it also wiped `refrows`, so the reference row was computed and never MARKED. A measure that declares its own `ref_auto` now overrides `"tot"` as well as `"auto"`, reading the fact off its `MEASURES` row. Here and not at the boundary: `ref` is still a per-`row_vars` vector there and `comparison` is unresolved, while this is upstream of both `calculate_refrows()` calls and of the wipe. `num_resolve()` needs no twin — an odds ratio wants a factor. The jmvtab picker mirrors it: while `orIsActive()`, `"tot"` leaves the choices **entirely** (for a factor and for a cut number alike), and a stored `"tot"` is deliberately left alone so turning the odds ratio off gives the user their own choice back. The `ref2` cell drops `"tot"` too and labels itself `ref2 =`, since it sits in a column headed `ref =`.

**`shape` is reordered and `levels` is `values_to_levels`.** `VAR_SHAPES` is declared numeric-first then cuts coarse-to-fine, `values_to_levels` last because it is the one that explodes the level count — and the header now says that the declaration order IS the offer order, since both jamovi pickers are emitted from it. The generator stops re-grouping (`col_shapes <- tab_shapes`). Labels are **derived** from `produces` (`TABX_SHAPE_LABEL`): `linear (numeric)`, `quadratic (num.)`, `sd_bands (cut)`. ⚠ **plain text, not `<i>`** — a native `<option>` paints `textContent` and markup would show literally. ⚠ `kind.defShape` is `offered[0]`, so `linear` staying first in the numeric lists and `auto` in the index list is load-bearing.

**The Model table's two headed columns.** `REG_FAMILIES$ui` drops the parenthetical that only repeated the family's own kind (`multinomial`, `ordinal`); `reg_link_ui_labels()` drops the glm spelling entirely — a link IS a measure, so the label is the measure's own word — and `auto` reads `auto (family based)`. ⚠ **Two orders that must not borrow each other**: `$fits` keeps its own (its first entry is what `link = "auto"` resolves to) while the drop-down takes `measure`'s (`auto, difference, ratio, odds_ratio`); the sort is in the generator, on the emitted `TABX_LINKS` alone, and the suite asserts both. ⚠ **The `outcome_level` picker was gated on "the outcome has exactly 2 levels"**, which hid it on the two families that most need it — 22g-v's one-vs-rest binomial and every multinomial. It is gated on the new `TABX_OUTCOME_LEVEL_ROLE` (from `REG_FAMILIES$<f>$outcome_level`), which also supplies the tooltip: *the level modelled* vs *the baseline category*. Its labels go through `tabxvClean()`; stored values stay RAW.

**The level box.** The opener reads `8 levels — click to relevel` / `— click to close`, the instruction half italic `#444444`; the list's three-cell header row is **gone**, the word *merge* written instead into the one tick cell the grid leaves empty by construction (the first level has nothing above it); and the expanded box is indented 26 px so it reads as a child of its row — ⚠ not the requested 1 cm, which is 38 px in the narrowest pane jamovi shows and would have eaten the merged-name box.

**Smaller, all in the same pass.** `measure = "coefficient"` → **`raw_coefficient`** in the yaml, in `MEASURE_OF_RADIO` and in the prose, with the generator's dual-spelling `alias` deleted · `color`'s `auto` → **`measure`**, which lets `.opts()` fold to `switch(color, "no" = FALSE, color)` · a **`digits`** ComboBox in the Regressions panel (`n | digits` on one row, `cleannames` below), passed as `tab_reg()`'s floor · **`other_if_less_than` deleted outright** from jmvtab — the option, the control, the `.opts()` read and the three cache-key sites — because the per-variable table merges by hand and the two fought each other; `n_min` and `cleannames` are now one row · `"(model default)"` → **`mean (default)`** on a numeric predictor's anchor · greys: `design_effect` on an empty `wt` (imperative, and the `wt` box gains the `change` event it alone lacked), `color = "between_groups"` on an empty `tab_vars` (imperative — the DSL cannot see an array's length), `color = "adjustment"` under `empirical = FALSE` (declarative `enable: (empirical)`).

**Not done, by decision.** The **drop-down latency** and the **reorder-that-refits** were scoped as investigate-only. The second is recorded in Phase 22k, which already owns the caching: `jmvreg_fit_key()` fingerprints the prepared frame's levels, so any reorder moves the key by construction; taking the non-baseline order out of it and reordering rows post-fit is real work with real risk on ordered factors and ordinal outcomes.

**Tests.** New locks: `jmv_opt()` against both real `Options` objects, including that the bare `$` errors · the Model table's labels and the two link orders · `TABX_OUTCOME_LEVEL_ROLE` emitted for exactly the two families that declare a role, and read by the picker · the render mirror over jmvcore's 5e5 ceiling · `base_diff`, plus **every** `display` value the ComboBox offers · an odds ratio reading `ref = "tot"` as its first level, marking the reference row, and no other measure moving · no `tx-sec` on a `<td>`, asserted beside the print rule that makes it matter. Updated: the shape vocabulary's order and `shape_auto()`'s return, the preset name list, and the jmvtab-cache "merge, THEN lump" case — which left **with its control**; the ordering itself is `tab()`'s and stays locked in `test-row-model.R`.

**The build chain ran here** — `dev/generate_jamovi_js.R` → `node --check` → checksum → `jmvtools::prepare()` → checksum (all four yaml **unchanged**, no `yaml.dump()` rewrite) → `i18nUpdate("catalog")` → `i18nUpdate("fr")` → `install(home = "flatpak")`. **French: 22 msgids translated by hand** (most of them the maintainer's in-flight label rewordings, whose translations the regenerated `fr.po` had dropped); compiled `fr.json` **209 of 210**, the one left being the package DESCRIPTION blurb.

**Still open: the maintainer's live click-through** — the level box's new opener text, its headerless list and its indent; the `shape` drop-down's annotations and its new order; the Model card's family/link labels and the outcome-level picker on a 3+ level binomial and on a multinomial; `digits` in the Regressions panel; an Excel export from **jmvtab** (the crash); a staged comparison left running a long while (the flicker); `tab_theme = "print_ready"` on a regression with a numeric predictor (the shape table); and the three new greys.


#### Phase 22g-vii — yet another round of exports manual review
- Is there a reliable way to set all Excel’s column widths (not relying on a flawn "auto" feature ?) to the exact point where they are **just enough** to print all the content of the cells (including auto-wrap of text cells) but just that, to save horizontal space and maximise compactness and readability ?
- 
```r
arrests |> # `vignettes/articles/tabxplor-all-else-equal.Rmd`
  tab(colour, released, pct = "row", ref = "first", stars = TRUE,
      color = "difference", color_signif = "grey_non_signif") |> 
  dplyr::mutate(difference = set_display(Yes, "diff"),
         ratio      = set_display(Yes, "ratio"),
         odds_ratio = set_display(Yes, "OR")    ) |> tab_export()
```
- Tables should not loose important properties when the table-level metadatas are lost. Compared to `tab_reg(arrests, "checks", c("colour", "employed", "citizen"), family = "gaussian", empirical = TRUE) |> tab_export()`, `tab_reg(arrests, "checks", c("colour", "employed", "citizen"), family = "gaussian", empirical = TRUE) |> tibble::as_tibble() |> tab_export()` currently loose: the footer (that is ok and excepted) ; horizontal borders between different predictors (it’s supposed to be base `tab()` behaviour with several `row_vars` or `tab_vars`), that we have all the required metadata in `lvl` and `fmt` custom vctrs columns and can compute without the table-level metadata ; the `n` column also disappear, but I’m not sure what to do with that one, it could be computed without table-level metadata I guess, but it would create a new default for when there are no `totcol` in the whole table. Maybe rule should be: if there is at least any totcol the n is folded into it, otherwise a new n column is printed with the current regressions only behaviour ? (The only case about which I can think is: only numeric `col_vars`.) What I’m not sure about is: would it be the right default, if the user don’t want the total column at all, but sees it come back in it’s export, so that may be the main reason to keep it regressions only. On the same time, the global option can set this to `"no"`. Study this and make me a proposition.

**exports variables names wrapping and non-wrapping**
- The vignette problem reveals a more general one, namely that when a `predictors` or `row_vars` only have two levels or so, like in `tab_reg(arrests, "checks", c("colour", "employed", "citizen"), family = "gaussian", empirical = TRUE) |> tab_export("html")`, where no names collides but it wastes a lot of vertical space. 
  + Very long predictors names never follow the `wrap_rows =` limitation and create an unreadable table: `tab_reg(arrests |> dplyr::rename(shenaniganing_colorous_property_of_the_skin = colour), "checks", c("shenaniganing_colorous_property_of_the_skin", "employed", "citizen"), family = "gaussian", empirical = TRUE) |> tab_export("html")`
 + So html and Excel should have a smart auto rule , and a nice way to wrap the vertical text / the variables(rows or cols) even when they are long snake_case with no spaes or really big words. Check if the right call is to use `wrap_rows =` or add something like `wrap_rows_vertical =` 
- If there are one-line row_vars, like "Constant" or numeric predictors in any model, or longer than that, than can never go vertical without wasting space, they should set the threshold under which the other variables go hozizontal anyway (there being vertical won’t save more space), and they should wrap like any "levels" at the `wrap_rows =` threshold anyway (regardless there are spaces on them or not, but with smart wrapping when there are spaces or "_" to avoid cuts in the middle of a word when possible, etc.). All wrap must be done with a one char indent (for the user to visually know it’s continuing on another line).
- If the row_vars or predictor name is "employed", and there are only two levels (Yes/No), even if "employed" is thinner than "Constant", it should be printed horizontally to not waste space. What would be a reliable way to detect this ?

**palettes and formatting**:
- the "print_marks" palette use so small + and - marks, that they are not visually striking enough when using the "grey2" color: they should always use pure black (create a new color value if needed, if `tx_chrome_hex()` if it’s the right place, because we should keep "grey2" for secondary display tokens etc. This new color should only apply to print palettes "marks", not to significance stars etc.).



#### Phase 22g-viii — the interactions picker, folded into the model builder

**Deferred from 22g-iii by decision, with its design already chosen.** Today `crosses` lives in its own *Interactions* CollapseBox: it takes too much horizontal space (the delete button can be unreachable), it is disconnected from the predictor subsets, and there is no way to build a with/without-the-interaction comparison.

**The design.** The `+ Add interaction` picker moves **once** to the top of the *Model comparison* box — two drop-downs producing a chip `age × race`, with a `×` to delete it. Each model card then lists that chip as **one more tick-box beside its predictors**; ticking it unticks the two parents in that card, unticking restores them. With zero cards the single live model takes every defined chip (today's behaviour). This is exactly the card's existing data model — one tick-box per thing a model may contain — so it reuses `setCardVars()` / `reconcileModels()` and needs no new option: **a card's `vars` may simply hold `"a*b"`**, which `jmvtab_reg_models()` already passes through. With/without falls out for free.

⚠ Prior art to read first: `jmvtab_reg_cross_keys()` / `jmvtab_reg_cross_fold()` / `jmvtab_reg_models()` (`R/jmvtabreg-cache.R`), `renderCrossPicker()` / `renderModelCard()` (`jamovi/js/jmvtabreg.js`), and 22g-i's defect 1 — `rlang::inject()` + `!!` splices a literal vector, which is why `reg_cross_slots_quo()` must not fall back to `quo_peek_extern()`.

⚠ Also open from 22g-iii, unrelated to interactions: **the jmvtab References picker cannot name a cut variable's group labels** (they are computed R-side from the data's quantiles), so it offers `Total` / `First group` / `Last group`. If that proves too coarse, the honest fix is for the backend to publish the resolved labels — which `jmvcore::Options` has no setter for, so it would have to travel through the results panel.



#### Phase 22i — documentation reviews

In the introduction vignette: teach levels = "first" and levels = "auto" with the FactoMineR:: tea dataset; also teach to use tab_vars + spread_vars to make a very condensed table of on the tea dataset.


Add a direct Word export ? No, teach to go through Excel: it’s recommended to store the tables with the real numbers, not the rounded ones, and do the last formattings if needed, then to copy-paste to Word (using the app, not the web browser, not to lose formatting).

**Still open**: the French twin (`vignettes/articles/tabxplor-all-else-equal-fr.Rmd`) and its `_pkgdown.yml` row in the "En français" group, after the maintainer's manual review. `dev/french_glossary.md` should settle the twins for the new plain-language vocabulary (outcome / predictor / adjusted / "all else equal" / composition effect -> *effet de structure*); the research for it is done — Cibois's *effet propre*, Deauvieau's *« le langage du tableau croisé »*, INSEE's *à structure constante*.

#### Phase 22k — the crude block's own cache: the last refits a served table still pays

⚠ **Two live-UI cost items were deferred here by Phase 22g-vi, and they belong to the same question.**
- **A reorder that does not move the baseline still REFITS.** `jmvreg_fit_key()` fingerprints the prepared frame's levels and `jmvtab_reg_build()` relevels before the fit, so ANY reorder moves the key by construction — although only the BASELINE changes a treatment-contrast fit's estimates; the rest is row order, which an `arrange()` on the cached table would do. Taking the non-baseline order out of the key means reordering rows post-fit, and it must be weighed against an ordered factor (polynomial contrasts) and an ordinal outcome, where the order is not display. Same question for a reference change and for merged levels: is there anything cheaper than a refit, and does it stay honest?
- **Drop-downs are slow to close.** Every `<select>` pick writes an option, which round-trips through jamovi and repaints the widget synchronously. Measure the `change` → `renderVarTable` / `fetchLevels` / `renderRows` path before assuming it is the panel's overall size; a one-tick deferral of the option write (`setTimeout(…, 0)`) in the shared `makeSelect()` is the cheap candidate, and was deliberately NOT shipped unmeasured.

**Goal.** Phase 22j cached the MODEL fit; the **observed (crude) block** is still rebuilt from scratch on every jamovi interaction, and its univariable refits are the largest thing left in a served build. Cache the crude fits the way 22j cached the model one — the same `reg_fit()` unit, the same distil / rehydrate, the same estimand-free key — so `empirical = TRUE` stops costing a refit per numeric predictor on every click.

**The measurements this rests on** (Phase 22j, `dev/benchmarks/phase22j_digest.R`; gss_cat, 21 407 rows). Times are for a **served** build (the model fit already a cache hit), so they are what is left to remove:

| case                                    | served | of which crude | share |
|-----------------------------------------|--------|----------------|-------|
| binomial, factors + 1 numeric           | 0.276 s| 0.073 s        | 27 %  |
| binomial, 3 numeric predictors          | 0.171 s| 0.055 s        | 32 %  |
| binomial, `effect = "marginal"` RD      | 0.459 s| **0.245 s**    | 53 %  |
| multinomial, factors + 1 numeric        | 3.845 s| **1.891 s**    | 49 %  |

⚠ **The win is concentrated, and the common case is already imperceptible.** ~0.07 s on an ordinary binomial table is not worth a wrong number; the two shapes that justify the phase are a **multinomial with any numeric predictor** (one `nnet::multinom` refit ≈ 1.9 s) and any **marginal estimand** (a `reg_marginal()` sweep per predictor on top of the refit). Decide up front whether that is enough, and say so in the DONE summary either way.

**WHAT ACTUALLY REFITS — the `saturated` fork, and it is most of the scoping.** `reg_crude_block()` (`R/tab_reg.R:3310`) answers it once per block through `reg_crude_saturated()` (`R/reg-empirical.R:354`), and only `fit_preds_e` costs anything:

- **numeric predictors — always**;
- **nested-cross variables — always**;
- **factor predictors — only when NOT saturated**: an ordinal (cumulative-OR) `shape`, or a design carrying STRUCTURE (clusters / strata / calibration) the closed form cannot see.

Everything else — a factor predictor on a flat design, the common case — is a **closed form** off the cell grid (`reg_empirical()`, `R/reg-empirical.R:168`) and needs no cache at all. Verified: `reg_crude_saturated("binomial", TRUE, NULL, NULL)` is `TRUE`, and three univariable multinomial refits cost 8.1 s where the measured block cost 1.9 s — because two of the three predictors were saturated.

**Why it is well-shaped: the unit is already the cached unit.** The per-predictor loop in `reg_empirical_fit()` (`R/reg-empirical.R:369-386`) calls the SAME `reg_fit()` the model path calls, and already keeps `list(fit, digest, data)`. So `reg_fit_distil()` / `reg_fit_rehydrate()` / `reg_digest_frame()` / `jmvreg_cached()` apply verbatim; the work is a key shape and a split, not new machinery.

**The design.**

1. **Cache the FIT, never the block.** `reg_crude_block()` is estimand-DEPENDENT in three declared places — `saturated` reads `reg_crude_shape(key, sp$est)`, `marginal` reads `sp$est$effect`, and `reg_empirical_columns()` (`R/reg-empirical.R:633`) takes `est` — so the block must keep being rebuilt. Only the univariable records go in the store, exactly as `f$tidy` is rewritten per estimand on the model path.
2. **A key per (outcome, predictor)**, not per spec. `jmvreg_fit_key()` (`R/jmvtabreg-cache.R:88`) keys on `sp$predictors`; a crude fit's model is ONE predictor (or, for a nested cross, its moderator plus the crossed term). ⚠ **`drop_extra` MUST be in the key**: it is `setdiff(other_preds, c(v, fp))`, a function of the WHOLE predictor set, and it decides the complete-case population — so the same `(outcome, v)` under a different predictor set is a DIFFERENT fit. Getting this wrong rebuilds the frame on the wrong rows.
3. **Rehydrate per predictor.** `fits[[v]]$data` is read by `reg_gap_se_columns()`'s numeric arm (`R/reg-empirical.R:988`), so a served record needs `reg_digest_frame()` before it is handed on.
4. **Thread the `refit` callback.** Under `marginal`, `reg_marginal()` runs on the crude fit and may fall through to `marginaleffects`, which needs a fitted object — the same `reg_digest_revive()` route `reg_cols_ame()` already takes.

**Traps, each one already paid for once in Phase 22j.**

- ⚠ **The sampling weights.** A distilled record finds them through its recipe (`design_spec$wt`) or the frame's own `.svy_weights`; a record built with an empty recipe silently weights by 1. That defect was measured at a **16× wrong gap SE** on a calibrated design, and it was in this very function (`reg_empirical_fit()` was dropping the digest).
- ⚠ **The frame rebuild is asserted, not assumed.** `reg_digest_frame()` refuses a frame whose row count is not the fit's. Keep that; it is what turns a key mistake into a refusal.
- ⚠ **`f$x <- NULL` removes the name and `$` then partial-matches.** Use `f[c(...)] <- list(NULL)`.
- ⚠ **A class dispatch a digest cannot satisfy is a wrong answer, not an error** — read `reg_model_kind()` / `reg_model_categorical()`, never `inherits(fit, "multinom")`. The one instance of this cost **123 s against 14 s** and looked like a hang.
- The store is per-analysis and serialised into jamovi's `$state`: crude records are the same ~3 KB each, but there is one per predictor, so re-check `JMVREG_CFG`'s ceilings on a wide predictor pool.

**Prior art to read first:** CLAUDE.md > Phase 22j (the whole DONE summary), `R/reg-digest.R`'s header, and `dev/tabxplor_2.0.0_performance_review.md` §8.5.


#### Phase 22x — very last features before release

⚠ **Open, deferred from 22g-i (a jamovi control the API has but the UI does not).** It was weighed and left out on purpose, and it does not block the release. (The other one, a `shape` picker for `jmvtab`, **is DONE** — Phase 22g-iii built it into the level box.)

⚠ **Open, found in 22e-i: a declared panel that cannot be drawn.** `REG_CHECKS$residuals$families` includes `"ordinal"`, so `check = "auto"` asks for a residual panel on a `polr` / `svyolr` fit — and `rd_resid()` has no ordinal arm, so it returns `NULL` and the grid silently loses a panel it declared. Two honest cures, pick one: give `rd_resid()` the ordinal randomised-quantile arm (a cumulative-probability draw between the two cut points, which is well defined), or drop `"ordinal"` from that row's `families` and from `normality`'s. ⚠ Whichever is chosen, the fact table and what can be drawn must agree — a family listed in a row is a promise `reg_panel_keys()` makes to the user.


---

### Phase 23 — documentation integration and simplification 2


#### Phase 23a — vignettes simplification and integration
- **One vocabulary rule for `deviation` / `measure`, decided in `dev/reg_estimand_api_redesign_follow_up.md` §6** (the rename to `deviation` is refused; §6.2 is the rule that replaces it): *a **deviation** is the quantity — how far a group sits from its reference; a **measure** is which of the three ways it is expressed.* Write "measure of deviation" the first time the argument appears in a document, `measure` alone thereafter. Today `vignettes/tabxplor.Rmd` teaches "How to measure deviation?" for `color`, the *All else equal* article teaches "measure of deviation" for `measure`, and `vignettes/tabxplor-reg.Rmd` teaches neither — while 6 of its 7 uses of the word are "standard deviation". French is already fixed in `dev/french_glossary.md` (*écart* / *mesure (de l'écart)*).
- Document undocumented stuff. `spread_vars` in `tab()` ?
- Vignettes should not be neverending. If some aspects, either expert, or on the contrary pedagogical and near useless to experts, need to be placed in new vignettes, make me propositions. Point to `tab_structure()` · `tab_supports()` · `reg_measures()` · `tab_columns()` · `fmt_attr()`, etc., when relevant.


#### Phase 23b — roxygen2 documentation simplication
- Carry §6.2's *deviation / measure* rule into `?tab`'s `@param color` and `?tab_reg`'s `@param measure`, and say once, in `?tab`, that an acronym names a **measure** while `display =` names a **field** (so `or` / `diff` / `ratio` are legal in both arguments with different meanings).
- Point to the right vignette for more details and pedagogy. Point to the introduction vignette in `?tab` description and the regression vignette in `?tab_reg` description. Start the english vignettes with a link to the French vignette to say it exists, if not already done.
- `?tab_reg` examples are now plain wrong, since the `family` --> `link` -->  `measure` --> `effect` argument cascade change.

#### Phase 23c — user messages simplication and focus
Many user messages print useless dev stuff (always remove) or internals (only accepted if the function is itself advanced programming) made for the maintainer: as a general case, messages should speak to the user, and they should speak about statistics, statistical soundness, real caveats, user-facing arguments and the like, in a simple and clear way that is really helpful to non-experts.
- "i Survey design detected: estimates and tests use the design." "ℹ The footer reports the dispersion; use `family = "quasipoisson"` for the fully quasi fit." Remove this kind of useless messages (if the user passed a design object as data, he expect it to work and need no confirmation ; if the footer report the dispersion, the message is useless; etc.).
- "ℹ `color = "adjustment"` compares each model effect to its observed one, so `empirical = TRUE` is turned on." Only once per R sessions for this kind of pedagogical messages ? For example `tab()` row_vars numeric vars autocut message: "ℹ age: cut into four bands, at its mean and one standard deviation either side." Only once per R session, and the user must know that `shape = c(age = "sd_bands")` is what does it without message. Concise, useful.

#### Phase 23d — drastic `NEWS.md` simplification
`NEWS.md` `# tabxplor 2.0.0 (in development)` was already drastically simplified in Phase 18y, but have since Phase z2 accumulated all dev history again. Most of it is really not user-facing and irrevelant here (and already in other dev documentation). A **drastic** reduction is needed here, no dev details **at all**, straight to the point, please **reduce around 400 lines to maximum 150 lines** :
- "## New features" should only list the most important things. New exported functions, like tab_counts() and others, should be presented in one quick sentence. New arguments in one quick sentence, rarely more. Everything about `tab_reg()` should be near-zero-words : in spirit, "possibility to do regressions added, see the vignette".
- Drastically reduce "## Changes that may affect existing code", only keep what is really important
- Drastically reduce bug fixes (same thing really), to only speak about very very few bugs that could have been hit by real user. Remove everything about any new argument or implementation. Make it small.
- Keep deprecation, reduce it’s size, list elements quickly, but keep differenciate what is soft deprecated and what is hard deprecated.

#### Phase 23e — Tests simplification
testthat tests have grown organically: it was right for development, but would slow future dev for no real benefits. I want you to select the tests that are *really* necessary, and to move the others to a folder of `dev/` scripts not run with `test`. **The full suite must go below 20 seconds** (parallelised, on this desktop computer).

#### Phase 23f — `dev/` folder
Files inside the `dev/` folder have grown organically, with many now useless files and outdated ones, which is very messy for future development : I want you to clean and reorganise the folder and main files.
- Put all files related to v 2.0.0 dev history and of no real use for future dev in an 2.0.0 archive subfolder. That should be most of them.
- Only keep at `dev/` root level a few selected .md files that explain in detail the architecture or functioning or use cases of some subsystems, and will be really useful for future dev : clean these files, simplify them by removing useless dev history and focusing on current architecture and usage, ensure they are up-to-date compared to the current design and code ;  organise them internally in such a way that goals, design and architecture decisions, usage, and everything giving the big picture come first, and details come next ; reference them in the architecture document.


#### Phase 23f — french translation

#### Phase 23f-i — vignettes

French translation here requires real web searches across French quantitative social sciences literature and papers, to find meaningful and *specific* ways to speak about regressions, without a bad word-by-word translation from english. 
The maintainer is not even sure how to translate "to *fit* a model" in an accessible, readable French: the first thing to do is to expand the glossary so that main regression vocabulary, but also typical expressions and interpretation sentences, have a well-designed readable, user-friendly, stylistical strong and simple French translation.

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
