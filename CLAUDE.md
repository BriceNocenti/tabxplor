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
- `jmvtabreg-cache.R` — the regression fit cache + `jmvtab_reg_build()`.

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
| `EST_SCALES`       | `fmt_class.R`        | What a column estimates (field, null, geometry, colour ladder, SD source)               |
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

What a cell prints is a `{}` template over declared tokens (`DISPLAY_TOKENS`), resolved by one boundary `tab()`, `tab_reg()` and `set_display()` share, so a layout learnt on a crosstab means the same on a regression. `{est}` and `{base}` are **scale-relative** — the deviation a column estimates, and the level it sits on — which is what lets one named preset (`DISPLAY_PRESETS`) render an odds ratio, a mean difference and a percentage alike. A composite has a **primary** token, the first outside brackets: it carries the stars, it is what `get_num()` and Excel return, and it is the only part the colour paints. **A display is post-hoc** — every field a layout can print is populated at build, so choosing one triggers no computation and changes no number, and a token may be **derived** rather than stored (`resid`, `gap`, `sd`, `cv`). A numeric column's default layout is `mean_cv` — the spread as a percentage of the level, comparable between columns measured in different units — chosen per column and falling back to the bare mean where a mean is not positive. The **base count** is the display-time fact both producers share: folded into the Total cell when the table rests on one population, given one `n` column per block at the right when it rests on several (a spread, a regression's groups) — and the per-block Total columns then go, holding nothing but a repeated 100 %.

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

**The estimand is a cascade** (`reg-estimand.R`). **A link is a measure**: the one a model estimates directly — `difference` ↔ identity, `ratio` ↔ log, `odds_ratio` ↔ logit — so the argument naming the model takes the same words as the argument naming the report, and the statistician's vocabulary never surfaces. Four arguments, `family` → `link` → `measure` → `effect`, where `"auto"` means *follow from the left*, and one rule decides the rest: **a coefficient exists only where the reported measure IS the model's**; any other measure is applied to the model's predictions, averaged over the sample (`marginal`) or read at one constructed profile (`at_reference`, the ideal type). One clause qualifies it: `"auto"` never resolves to a *predicted* odds ratio, a specialist quantity asked for by name. Which model is fitted and which deviation is reported are two axes — `reg_formulas()` says what reached `glm()`, `reg_measures()` what an outcome can be asked.

`REG_ESTIMANDS` is **composed, not written**: `reg_compose_library()` emits one row per buildable `(link, effect, measure)` from four facts a family declares in `REG_FAMILIES` — its `level` kind (`pct` / `mean` / `count`), the `fits` it offers (the value set of `link`, first entry = its own), any header-word override and its footer qualifier — plus two shared maps: link ↔ measure, and what each kind of level can be compared by. A refusal is not a row but a derivation from the clause that failed, so a hole and its reason cannot drift apart. The family is auto-detected from the outcome (binary → logistic, unordered → multinomial, ordered → cumulative-OR ordinal) while a *number* is the user's call; one table can mix families, each column storing its own `model_family`. Hence the extension rule: **a new model is a row in a declared table, never a new argument or a word a user must learn** — a link is one map entry plus one `REG_LINK_FUNS` row (its transform and derivative — all a marginal contrast needs of a link, which is why the engine has no per-measure arm); a family is one `REG_FAMILIES` row, its footer statistics and model checks the only per-family work.

**One name per quantity** (`REG_WORDS` + `REG_CONTRASTS`). A header names the **measure**, the **contrast** is a marker on it and a log wraps the result, so the word is *composed* — `marker ∘ log-wrap ∘ acronym` gives `OR`, `mRR`, `refRD`, `log(cumOR)` — which stops two estimands sharing a header, or one estimand being named twice. The observed column and the colour legend take the measure **without** the marker — a univariable effect has no adjustment to be marginal over — so the observed/model pair stays one legend block.

**The observed companion — the distinctive feature** (`reg-empirical.R` + `reg-influence.R`). With `empirical = TRUE` each modelled effect sits beside the **observed (crude)** one: the same estimand, on the same people, with one predictor instead of all of them — so *what did adjustment change* is read across the table. One column shape built twice, and the observed shape is composed rather than declared (`REG_EMP_BY_LINK` indexes `REG_EMPIRICAL` by the measure's link), so a model row and its twin cannot state two estimands; its value is a closed form on the per-cell grid where the univariable model is saturated, otherwise a refit through the very fitter the table came from. `reg-influence.R` computes the **standard error of the gap**: both estimators are fitted on the same rows, so only the difference of their influence functions carries the covariance — and that gap SE is what makes `color = "adjustment"` a test rather than a description. On a non-collapsible measure the movement is coloured but never tested: an odds ratio moves when any strong predictor is added, which is arithmetic, not confounding.

**A parametrisation is decided while the data is prepared.** An `a*b` entry in `predictors` is *a predictor whose levels are combinations, and whose univariable model is its own saturated fit*, so it is materialised as a column before the fit and every subsystem keeps reading an ordinary predictor; `REG_CROSS_ARMS` (`reg-cross.R`) declares its two shapes — a combined factor against one common reference, or slopes nested in a moderator. `shape` recodes a continuous predictor the same way, and `ref` shifts one to its anchor so the fit's own intercept is already the baseline the Constant row shows. One rule covers the three: **the boundary defines the model's variables, then fixes their origin** — and the fit's own output is already the table.

**The boundary and the build** (`reg-resolve.R`, `tab_reg.R` + `reg-spec-build.R`). `reg_resolve_args()` is the crosstab boundary's twin, with `data` *inside* it — `family = "auto"`, `multiplier = "sd"` and `shape` are answered by the data — and one grammar per axis: the four estimand arguments per outcome, `multiplier` / `shape` / `ref` per predictor (unnamed = the fallback, named = that variable). `reg_build()` then runs over a typed `new_reg_ctx`, its per-model half a declared product (`reg_spec_build()`), the three nesting axes — `tab_vars` groups × models × outcomes — dispatching through the shared parallel seam.

**Effects and model checks.** A marginal quantity comes from tabxplor's own analytic g-computation, or from `marginaleffects` at a reference profile — derived from the contrast, never declared per row. `REG_CHECKS` catalogues the checks (linearity, dispersion, influence, proportionality, collinearity), each with the `shape =` cure that fixes what it flags — the check and its cure are one object — each priced (`cost`) and each declaring whether it runs by default (`footer_default`), because what a table must say and what it costs are two questions. The **observed shape** of a numeric predictor is the free half of the linearity check: one curve per outcome, binned with no fit at all, drawn as a sparkline in a window floored by the data's own sampling noise and by the first colour rung — so a flat run means flat. It goes in the predictor's own `n` cell where the table has one outcome and the medium can hold it, and otherwise in a small **shape table** below the footer.

### Exports and rendering

`tab_export(x, format =)` (`tab-export.R`) is the facade over four backends: HTML (the default), Markdown, Excel and plot, sharing one preparation step — `tab_export_prep()` (`tab-export-prep.R`) builds an ephemeral render model (roles, references, faces, header spans, variable-name blocks) that every backend consumes. A spread swaps the two header bands, since after a spread a **column** is identified by its sub-population and a **block** by its variable: the column header takes the `col_group`, the span takes the `col_var` and, above it, the level only where that variable gives several columns per group.

Display values reach the backends by one source of truth: `format.tabxplor_fmt()` renders the text for console, Markdown and HTML, and `tab_xl()` writes a number with format codes from that *same* `format(syntax = "excel")`, so a display change never needs mirroring. **Excel keeps the cell a number and puts everything else in the code**: an aside becomes a column carrying its own segment (`(n={n})`), and every literal a template writes — the stars, the brackets, a sigma, a test label — folds into the numFmt, per section. A multiplicative cell holds its **reading value**, the signed fold, so `1/2.11` reaches the workbook without becoming text; text stays a property of a *cell*, not of a column. The exports' **unit row** is the console's own type tag (`<row%>`, `<n>`), written once per **block** — `tab_col_block_ids()`, the one definition of a block, which also decides where a vertical rule falls. Colour is single-sourced too — every backend reads `fmt_color_channels`. HTML colour is a slot **class**, never inline hex, the theme living in a `<style>` block from the one CSS generator (`tab-css.R`), so light/dark and the publication palettes work by stylesheet — except `print_marks`, whose signal is cell text and so comes from `format()` like the stars. `tab-transpose-render.R` flips a finished render model (a transposed column is heterogeneous and cannot be an `fmt` column), and `tab-theme-detect.R` best-effort-detects the console's scheme — a subsystem that must never error, because a wrong guess only mis-tints.

The **hover tooltip** (`tab-tooltip.R`) is that same rule read line by line: `TOOLTIP_LINES` declares one row per line — the token it renders, where its name comes from, which of the shared gates apply — and row order IS the reading order, so a line is named by its `DISPLAY_TOKENS$label`, exactly as the exports' unit row is, and one gate (non-empty · comparable · not the reference · not already shown · not already emitted) decides every one of them. It is **not translated**, deliberately: like the pillar type tags its words are the `fmt` field names, so the hover teaches the fields a user reads with `$`.

### jamovi

Two point-and-click analyses mirror the two producers: `jmvtab` (Crosstables) and `jmvtabreg` (Regressions). Each is a thin `R6` backend (`*.b.R`) over an engine-free build core (`jmvtab_build()` / `jmvtab_reg_build()`) driving `tab()` / `tab_reg()` through a content-addressed **live-UI cache** (`*-cache.R`), so an interactive tweak re-paints instead of recomputing. Each option is named after the argument it drives, so the backend is a pass-through, not a translation table. The generated `*.h.R` option headers are never hand-edited.

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


#### Phase 22f — exports and legends manual review


##### Phase 22f-i — footers legends review — DONE

**The footer legend now leads with the measure, in words, and says one thing about uncoloured cells.** One grammar for every case — `[<columns> — ]<HEAD><LADDER> <NOTE>` — replacing a form that led with the palette (`Shades of blue:`), spent a clause on the tautology *coloured ⇒ significant*, named the interval once per channel, and had no publication-palette variant of its "Grey:" note. Measured before/after on `tab(gss, rincome, marital, pct = "row", color = TRUE)`:

```text
- Shades of blue: cells ≥ the Total row +5; +10; +20; +30 points. Shades of yellow to red: cells ≤ …
+ Percentage points (risk) difference: cell ≥ the Total row +5; +10; +20; +30 points; cell ≤ the Total row -5; … points.
```

**The measure's name is a per-(measure × ladder scale) fact, on a slot that already existed.** `MEASURES$<m>$by_scale[[<scale>]]` was already keyed by `plan$scale_key` and already folded in by `measure_facts(measure, policy, scale_key)`; `legend_measure_word()` was the one caller dropping the key. It now passes it and reads **two registers**: `word` (short — `difference`, `ratio`, `OR`) for the console and a ggplot guide, `word_long` for the export footers. The grid: `pct_diff` → *percentage points (risk) difference*, `mean_diff` → *mean difference* (`word_long_std` → *standardized mean difference*), `log_odds` → *log-odds difference*, `pct_ratio` → *relative risk (ratio)*, `mean_ratio` → *ratio of means*, `odds_ratio` → *odds ratio*; `contrib` keeps its own `word`/`guar$word`. Each name carries **both** the discipline's term and the base measure, per the maintainer's ruling; `REG_WORDS` and the `mRD`/`mRR` header vocabulary are untouched, and a regression column still names itself by its acronym (so no line says the measure twice). The `names(MEASURES$by_scale)` foreign key already checks the new keys at load. `MEASURES$odds_ratio$subject = "OR"` was **retired** — the head does that job now, and keeping it made the line read *"Odds ratio: OR ≥ 1.2"*.

**Shade names dropped for colour palettes, kept for the publication ones.** A diverging ramp needs no words — the legend's break-words are themselves blue and red — while greyscale collapses it, so `print_minimalistic` / `print_emphasis` keep *Underlined:* / *Italic:* and stay two sentences. `legend_shade_names()` lost its default-palette branch entirely (the custom-palette path already returned `NA`, so the two merged). ⚠ `tests/testthat/helper-i18n.R` probed gettext with the literal msgid `"Shades of blue"`; a retired msgid would have made **every French test silently skip** instead of fail — the sentinel moved to `"the reference category (in bold)"`.

**`guaranteed_effect` carries the guarantee in the head and names the interval once.** `95%-guaranteed percentage points (risk) difference (Newcombe score interval floor): from the Total row +0; +5; +10; +20; -0; -5; -10; -20 points.` The two sides merge into one list (guarded: only where they differ by the sign alone — a measure with its own `lead` says something different on each side), the per-side `", after subtracting the margin of error (…)"` tail is gone, and the background channel repeats neither the guarantee's interval nor anything else.

**The `ref != "tot"` legend was wrong, and is fixed.** Reproduced: `tab(g, c("rincome","age"), "marital", pct = "row", ref = 1)` printed *"cells ≥ the Total row +5"*. Two row variables give one reference row per sub-table, `legend_ref_label()` returned `NA`, and `legend_ref_phrase()` fell back to the literal `"Total"` — describing a comparison the table never made. A non-total reference now always reads **"the reference category (in bold)"**, word for word as `tab_stars_legend()` already says it (maintainer's call: the label is dropped even where it resolves, so `_snaps/golden.md`'s *"the Black column"* moved too).

**The `Model:` line is now the key to the cell, not a sentence about it.** `OR: odds ratio (vs the reference category); obs%: observed proportion; adj%: adjusted predicted proportion.` — `=` became `:`, and the prose clause (*"each cell shows the effect vs the reference level and, in parentheses, the adjusted predicted probability"*) became one `<abbreviation>: <what it is>` item per (aside token × role present), the abbreviation coming from **`display_token_label(tok, col)`** — the same function the exports' unit row and the tooltip use. `REG_ASIDE_NOTE` is keyed by token **then role**, which retires the defect its own comment admitted at `R/reg-estimand.R:1096`: the old sentence claimed an adjusted prediction on the crude column, where that slot holds the counted one.

**Smaller, all measured:** `fmt_gap_lead()` names the null it measures distance from (`IRR further from no effect (1) than the observed column`), read off `EST_SCALES$neutral`; the gap reference is per position — *the observed column* in the lead (it points at the column beside it), *the observed effect* in the test note; `"%s; %s closed form on the observed column"` → `"%s; matching %s interval on the observed column"`; the `Uncoloured:`/`Unmarked:` note names the first threshold concretely (`(±5 points)`, `(×1.2)`).

⚠ **A real pre-existing bug surfaced and was fixed: the second language switch of a session silently no-opped.** `flush_gettext_cache()` bound a *throwaway* domain (`"tabxplor_reset"`), but glibc keys its cache on `(domain, msgid)` — so `"R-tabxplor"` stayed cached and one `lang = "fr"` render made **every later `lang = "en"` one French**. Measured on `en → fr → en`: the third came out French. It now rebinds the package's own domain to `tempdir()` and back to `system.file("po")`. Found only because the review artefact renders both languages in one process.

**French.** `po/R-fr.po` gained 45 entries (301 → 303 translated after the two `Model:` templates; the catalogue is regenerated by `dev/update_translations.R`). Decisions recorded in `dev/french_glossary.md` with the name grid: « rapport de cotes » kept for the odds ratio (no churn across `REG_WORDS`, the vignettes and the generated `?tab_reg` grid); « différence de proportion » preferred over « différence de risques ». ⚠ **`"%s%%-guaranteed %s"` must not be translated with a past participle** — *garanti(e)* would have to agree with the measure, which one msgid cannot do; the catalogue reorders it into the fixed masculine `"%2$s, minimum garanti à %1$s %%"`, the same device as `"the %s %s"` → `"la %2$s %1$s"`. The legend uses ordinary spaces before `;` and `:`, not non-breaking ones (what the assemblers emit and what every existing msgstr uses).

**The review artefact: `dev/review_manual/legend_review.R` → `legend_review.txt`** (~1 370 lines, `.Rbuildignore`'d). 40 cases × EN/FR × the colour palette and the three publication ones: every measure, every policy, every reference shape (`ref`, `comp = "all"`, `tab_vars`, `spread_vars`), the interval variants (method, `conf_level`, weights, a `survey` design, a column with no stored method), a custom palette, and eleven `tab_reg` shapes including `adjustment` and `between_groups`. It prints the WHOLE footer (weight line + `Model:` lines + interaction lines + colour legend + stars), which is what an export actually shows.

**Tests.** `test-color-legend.R` rewritten around the new grammar (+ a new publication-palette block); `test-i18n-fr.R`, `test-print-palette.R`, `test-tab_reg-14w.R`, `test-forest-plot.R`, `test-tab_reg-rr.R`, `test-tab_reg-display.R`, `test-adjustment-colour.R`, `test-adjustment-gap.R` repointed. `test-review-pass5.R`'s positional-`ref` test was **strengthened rather than repointed**: it asserted the per-`col_var` reference through the legend's level labels, which the phase deliberately stops printing, so it now reads the stored facts (`get_ref_type()` / `is_refcol()` / `is_refrow()`) — a check the cosmetic one never really made. `_snaps/golden.md` (8 legend lines) and `_snaps/render-html.md` (2) regenerated; verified by diff that **only** legend lines moved, and `test-color-golden.R` (per-cell hex) stayed green untouched.


**Second round — the maintainer's read-through of `dev/review_manual/legend_review.md`.** Every request implemented, plus five defects the pass turned up.

Requested: the **`guaranteed_effect` head no longer takes a colon** (it is one sentence, `95%-guaranteed … (Newcombe score interval floor) from the Total row +0; …`, and only the merged form drops it — elsewhere the head is a label and keeps its colon); the **reference is named in full once per LINE, then short** (`cell ≥ the reference category (in bold) +5; …; cell ≤ ref -5; …`), through a third form on `legend_ref_phrase(spec, form)` — `full` / `short` / `plain`, the last for the NOTE, where a phrase ending in `(in bold)` gave two brackets in a row; `MEASURES$odds_ratio$subject = "OR"` **restored**, because `color = "or"` grades a quantity the cell does not print and `cell ≥ 1.2` compared a percentage to an odds ratio; the marginal / at-reference qualifiers say **"on adjusted proportions"** rather than "on the probability scale"; `adj%: adjusted/predicted proportion`; a gap's under side admits the **sign flip** (`… closer to no effect (1) than the observed column (or inversed effect)`).

**`contrib` now states what its sign MEANS** (maintainer's choice of three): a `lead` closure, exactly like the gap measures' — `Contribution to Chi2: cell over-represented vs independence, by ×1; ×2; ×5; ×10 the mean contribution; cell under-represented, by …`. It was the one signed measure whose two sides printed identically (`×` on both), leaving the direction to the colour alone. `indep` takes `""` as its short form, so the baseline is named once. The head condition moved from "the measure writes its own lead" to "the SUBJECT is already the measure", since `contrib`'s lead states a direction, not a name.

**`at_reference` no longer says "reference level / mean" when there is no number to average.** `meta$predictor_types` already records it; `est_note_marginal()`'s closure gained an optional `has_num` that `reg_estimand_note()` supplies, and every other caller leaves NA for the honest both-ways wording.

**Five defects found in the review file and fixed.** (1) An **acronym was being capitalised as prose** — `CumOR ≥` / `Diff ≥` where the headers say `cumOR` / `diff`; only the generic subject ("cell") ever opens a sentence now. (2) On a rank family **`sup%` was glossed twice** with two different sentences (both roles print that label), so the `Model:` line read as two quantities: one item per LABEL now, `duplicated(fromLast = TRUE)` keeping the model's reading and the reading order. (3) The closed-form clause **nested brackets** — `(Wald interval…, 95% confidence (Woolf closed form))` — now `; Woolf closed form`. (4) A gap measure's baseline clause on the BACKGROUND channel did not say which channel it was about. (5) The non-collapsibility **caveat printed twice**, once on the ladder line and once on the baseline line that shows no ladder.

**Verified, not changed:** the maintainer asked whether the spread / `comp = "all"` legends describe the right cell. They do — a spread compares each column to the Total row of its own block ("the Total row"), and `comp = "all"` names the total-table cell ("the Total Ensemble row"), both matching the printed table.

**French, second round.** « risque relatif » replaces « rapport de risques » for `REG_WORDS$RR` (CLAUDE.md's own ruling, which the catalogue had never followed); « en-dessous » and no comma before it in the uncoloured note; the contrib guaranteed note says « non significatif » rather than « en deçà du seuil de significativité »; the survey line names the design once. ⚠ **The `%%-guaranteed` head became ONE msgid per measure** (`word_guar`, an optional `by_scale` closure over the confidence level): *garanti* agrees with the measure, and the previous shared template forced the gender-free « minimum garanti » the maintainer rejected. ⚠ « odds ratio » is one msgid shared by the crosstab legend and the `Model:` line, so it cannot be glossed « (odds) » in one only — maintainer chose to drop the gloss.

##### Phase 22f-ii — Excel exports review — DONE

**The workbook is the same table now, read by the same rules.** The Excel backend had grown its own answers to questions the shared framework already answered: it re-derived colour from raw slots, decided "text or number" per COLUMN, named a helper column in a different row than the console, and printed `×0.8` where every other medium says `÷1.2`. Almost every change below deletes an Excel-local answer and reads the shared one.

**A multiplicative cell keeps a real number and still reads `1/2.11`.** Excel cannot compute inside a number format, so `÷1.2` cannot be shown from a stored `0.83`. The cell holds the **reading value** instead (`fmt_excel_value`): the fold, signed by its direction — `x` at or above the neutral, `-1/x` below — printed by an unconditional two-section code, `"×"0.00;"÷"0.00` / `0.00;"1/"0.00`. Excel drops the minus in a section it was not written into, so no `[<0]` condition is needed and the workbook says exactly what the screen says. It sorts and filters in the direction it is read (`>2` = at least twice as likely, `<-2` = at least twice as unlikely) and takes the reader's own decimal separator. The glyphs come from `MEASURES$<m>$break_over` / `break_under` — the pair the text path, the legend ladder and the forest axis already print — through one shared `fmt_mult_plan()`, so a cell, its format code and its number cannot disagree. `?tab_xl` gains a section with the one formula that recovers the raw ratio (`=IF(A2<0, -1/A2, A2)`; `=ABS(A2)` for the magnitude). **`or_numeric` is retired** (2.0.0-only, never released) for `ratio_cells = c("fold", "raw", "text")`, which covers ratios as well as odds ratios.

**Everything a template writes around its number now lives in the number format, by ONE rule.** `xl_fold_literals()` folds the literals of any single-token display into the code — the aside's brackets, an `n=`, a sigma, a test label — replacing the two hand-written arms that did it one template at a time. `xl_numfmt_affix()` applies a literal to **every section** of a code, which fixed a real defect: a signed difference wore its significance stars on the negative half alone (`+3.3` bare beside `-3.3***`).

**An aside column is an aside.** `mat_aside_cols()` is Excel's `paint_split()`, so the console's three rules hold on the columns it creates: it carries **its segment** (`(n={n})`, `(σ{sd})`) rather than a bare token, it wears the secondary grey in every row (including a Total or reference row, where `ann$font` would blacken it), it is never bold, and it takes **no stars** (`set_pvalue(NA)`, with the same `resid` exception the composite renderer makes).

**Text is a property of a cell, not of a column.** One `"TEXT"`-coded cell used to turn its whole column to `"@"` — which is what put a model-fit `AIC 17 129` into a string Excel flags as a number stored as text, carrying a `.` decimal into a locale that reads `,`. A numeric column is now written with a hole at each text cell, and those few (a `{ci}` bracket, a real min-max `{n_range}`) are written individually, exactly as a row sparkline already was.

**Colour is read, never re-derived.** `tab_xl` consumed `ann$text_slot` and filtered `slot > 0L`, so an uncoloured cell got no colour at all and rendered **pure black** while html greyed it. It reads `ann$font` / `ann$back` / `ann$face_*` now — the same three fields `tab_kable` and `tab_plot` consume, where the whole rule (hex → anchor black → grey / grey2) is already folded.

**The unit row is the console's own type tag, carried into every export.** `<row%>`, `<n>`, `<mean (sd)>` — the angle brackets are pillar's, added once in `tab_units_once()` so html, markdown and Excel all get them. Italic everywhere, left-aligned in html and Excel (the tag names its column; markdown aligns a column once, for the whole column), in the chrome's `grey` and theme-aware (Excel hard-coded the light grey). In html the level-header row drops its bottom rule, so the two read as one header band closed by a single line — the horizontal rule through the middle of a header is gone.

**One definition of a column BLOCK**, `tab_col_block_ids()`: a col_var run within one sub-population, with a Total column its own and every col_var-less helper joined to whatever it was carved from. Two consumers, so what a reader sees and what they read cannot disagree — the unit line (a block restates its unit, so `Total` says `<row%>` and the count beside it `<n>`) and Excel's vertical rules (a block is boxed, so **no rule falls between a Total column and its own count**, which is what `totcols` used to draw). ⚠ Renamed from `tab_col_blocks`, which already existed in `fmt_class.R` for the *distinct* (col_var, col_group) pairs a test grid needs.

**Which row a column says its name in** — the maintainer's rule, per origin: the level header names what the TABLE has, the unit line what it HOLDS. A column the **render** carved out (a split-off aside, the base count taken out of a Total cell) has no level to name and is named by its unit alone; a helper the table already had (a regression's `n`, `col_var = "n"`) keeps both, as the console prints a name over a type tag. ⚠ Not under a **transpose**, which turns the level header into the ROW LABEL and carries no unit line to say it instead.

**Layout.** An index column's header takes both header rows (html `rowspan`, Excel a merge) and sits at the bottom, so "levels" reads on the same line as the `<row%>` beside it. A Total column's header aligns bottom like every other and left like its own cells (the total-column zone repainted the whole column, header included). Data cells align **right**, so a text-written cell no longer lands left beside them. The variable-name column is **bold** throughout (html always did; Excel gave it rotation and width and no bold at all) and reads from the left — with `vertical = "bottom"` under the 90° rotation, since rotated text runs bottom to top.

**The Excel → Word paste is fixed** (maintainer's diagnosis): a footer legend left in column A is a paragraph in one narrow cell, and Word sizes that column to the paragraph. Every line of prose — the title above the table, each footer line below it — is now one **merged, wrapped** cell, as wide as roughly an A4 portrait text width (`xl_prose_span`, capped rather than the table's own width so a wide table does not stretch the legend into one line). The title sits at the bottom of its cell, a footer line at the top. ⚠ Excel does not auto-fit a MERGED cell's height, so the row height is computed (`xl_prose_height`) or the legend is clipped to one line.

**Model-check plots in the workbook.** `tab_xl(check = "auto", data = NULL)` draws `reg_check_plots()` under each `tab_reg()` table as a picture. Nothing new was needed to find the data: `tab_reg()` already stores the *name* its `data` argument was written with (`fit_spec$data_expr`), so `tab_reg(gss, ...) |> tab_export("xl", check = "auto")` recovers it; `data =` is the escape hatch for a `%>%` pipeline or a subset. ⚠ `reg_check_plots()` draws on the current device as a side effect and returns its gtables invisibly, so the first pass runs into a null device and each grid is then drawn into its own PNG. The image is sized from the grid's own layout (the `top` title occupies one layout row and none of the panels), ~3.4 in per panel column by 2.7 in per row — measured 6.8 × 5.8 in for a 2×2 grid. The plots are rendered BEFORE the geometry so their height joins the sheet's stacking offsets and the next table does not land under a picture. Suggests-guarded (`ggplot2` + `gridExtra`); a crosstab takes none, silently.

**Four defects found and fixed on the way, all measured:**

1. **The freeze pane never froze a row.** `first_col = TRUE` and `first_active_row` are ALTERNATIVES in openxlsx2, not a pair — the shorthand won and the row split was silently dropped (the sheet XML carried an `xSplit` and no `ySplit`), so the header scrolled away while column A stayed put. Both axes are given as active-cell coordinates now, and the row is computed from the header block rather than hard-coded at 3: `ySplit="4" xSplit="2"` on the ordinary table.
2. **A thousands mark was a property of the TOKEN.** `diff` / `coef` / `gap` were absent from the mask, so one gaussian column printed its Constant row `101 002.4` (token `mean`) beside its effects `+14088.0` (token `coef`). Every rendered value takes it now — a value under 1000 is unchanged, so only the omissions moved. (The Phase 22h reported defect, fixed here because it is the same subject.)
3. **`coef` named a plain difference.** `EST_SCALES$raw_diff` (a gaussian beta, a count AME) declared `est_display = "coef"` while `mean_diff` beside it said `"diff"` — the same field, the same number, two names, and the column header already said `Model_diff`. It says `diff` now. Where `coef` survives it tells the truth: **`log(OR)` / `log(IRR)` / `log(cumOR)`**, composed the way the header is (`reg_word_logged`), from the family's own measure acronym derived through `REG_FAMILIES` (`reg_own_word()`) — so a reader never meets two words for one number. (Maintainer's report, mid-phase.)
4. **A table with no Total column lost its base count entirely.** `levels = "first"` — a battery of binary items, whose other levels were dropped after the tests — had no Total cell to fold the count into and no column either, on every medium. It takes the column shape now, the one a regression and a spread table already use. Percentages only: a column of means never had a Total column and never showed a count. This also closes the `tea` vignette note under Phase 22h.

**Tests.** `test-tab_xl.R` gains a check-plot block and two shared readers (`xl_numfmt_codes`, `xl_merges`) that assert what Excel will PRINT rather than what R wrote; `test-display-grammar.R` gains the `coef`-naming block; `test-export-parity.R` covers the reading value both ways. Repointed for the intended changes: `test-export-prep.R` (the unit line, the naming rule), `test-tab_md.R`, `test-render-html.R`, `test-transpose.R`, `test-display-extras.R`, `test-tooltips.R`, `test-export.R`, `test-adjustment-colour.R`, `test-xl-backend.R`. `test-print-palette.R`'s "a colour palette emits no italic" assertion was **scoped rather than deleted** — it now matches the NUMBER font only, because the unit row is header chrome and is italic in every theme, while what must never happen is a *cell* wearing a face. `_snaps/golden.md` and `_snaps/render-html.md` regenerated; verified by diff that only unit-row text, the two new CSS rules and the column widths they shift moved. **FAIL 0 | PASS 9712.**

**Six refinements from the maintainer's first read-through of the review workbook**, all measured:

1. **A split-off aside keeps the CELL's reading order.** `mat_aside_cols()` appended every aside AFTER its source, so a crude column exported as `Obs_OR | Obs_pct` while its template is `"({base}) {est}"`. An aside written before the primary now becomes the column before it — which puts the two ESTIMATES side by side (`obs% | OR | OR | adj%`), the whole point of printing a crude column beside its model, and exactly what the console shows.
2. **... and it keeps the role it was carved from.** `set_role("aside")` overwrote `emp` / `model`, which is what `reg_role_qualifier()` reads — so a crude aside was named `<row%>` instead of `<obs%>`. The role is `"aside:emp"` / `"aside:model"` now, and the six places that tested `role == "aside"` read one helper, `fmt_is_aside()`.
3. **The variable-name column sizes itself.** Fixed at 3.5 it cut `Constant` off in every regression table. `xl_vname_width()` computes it from the HORIZONTAL names alone (a merged run is rotated and costs one line whatever it says): `nchar × 1.05 + 1.5`, floored at 3.5 and capped at 13, the cells wrapping past that. **Deterministic, never auto-fitted** — openxlsx2 cannot auto-fit reliably, which is the whole reason it is computed.
4. **A rotated variable name centres on the block it spans** (`vertical = "center"`), while a horizontal one keeps the data zone's `top`; both read from the left.
5. **The model-check pictures are landscape and no longer clip.** 4.6 in per panel column (capped at 13) against 2.7 in per row — 9.2 × 5.8 for a 2×2 grid, ratio 1.59. The device size IS the text budget: a ggplot draws at a fixed point size, so a wider device gives every label more room rather than shrinking it. Verified by reading the rendered PNG back.
6. **The html index column keeps the rule under its header.** `:not([rowspan])` on the "no line inside the header block" rule: a `rowspan`ned header IS the bottom of the block in its column, with no unit cell beneath it to close it, so dropping its border left the levels column open onto the first data row.

**Review material — `dev/review_manual/xl_review.R`** (`.Rbuildignore`'d): 30 tables across both producers — 8 crosstab presets, 6 composite/aside cases, 6 numeric/shape cases, 4 table shapes (several `row_vars`, `tab_vars`, a spread, `levels = "first"`), 6 regressions (binomial, poisson, gaussian, `measure = "log"`, marginal RD, `color = "adjustment"`) — written to `~/xl_review.xlsx` (one sheet each), `~/xl_review.html` and `~/xl_review.md` as the twins to compare against, plus `~/xl_review_checks.xlsx` for the pictures. Its header lists what to look at, sheet by sheet.

**Still open, deliberately:** the maintainer's visual review of that set, which is what this phase's layout decisions ultimately rest on.


#### Phase 22g — Jamovi UIs manual reviews and final modifications

⚠ **The stale `R/jmvtabreg.h.R` is now BROKEN, not merely out of date** — measured in 22b-xv-2, not inferred: `tabxplor::jmvtabreg(data, outcome, predictors)` aborts with *"options$crosses does not exist"*, the option 22b-ix declared in the YAML. Two of its defaults also name values that have since been retired (`effect = "coefficient"` → `"conditional"`, `display = "value"`), so they would abort in turn once the first is fixed. The `.h.R` is never hand-edited, so `prepare()` clears all three at once — but until it runs the Regressions analysis does not work at all.

⚠ **22c-iii adds to the option surface and nothing is wired yet**: the display ComboBoxes have no `mean_sd` / `mean_cv` entry (a numeric column's default cell changed, so the idle value is right, but the two layouts cannot be chosen), and there is no `shape` control at all for `jmvtab` — the crosstab equivalent of the Regressions panel's own. Decide whether a numeric row variable's grouping is worth a picker there before running `prepare()`.

⚠ **22f-ii touches the Excel surface**: `or_numeric` is retired for `ratio_cells` (`fold` / `raw` / `text`) and `tab_xl()` gains `check` / `data`. None of the three is in a jamovi YAML today, so nothing there is inert — add them only if the Excel-export panel is ever given those controls.

⚠ **One `jmvtools::prepare()` for every jamovi-visible change of Phase 22**, batched here (**22c-v adds two `jmvtabreg.a.yaml` picker LABELS**: `ratio (RR / IRR / RoM)` and `difference (RD / diff)`, the old ones naming two words that are no longer header acronyms --- no option name or value changed, so nothing is inert meanwhile): **22c-ii's display ComboBoxes** (`jmvtab.a.yaml` gains `base_or` / `base_ratio` and names the OR layout by its preset `or_base` instead of the raw `"{or} ({pct})"` template; `jmvtabreg.a.yaml` gains `base_ratio`) --- until `prepare()` runs the new entries are inert, and the retired raw template still resolves, so nothing breaks; the `display = "num_ci"` → `"base_ci"` rename and the new preset list (22a-i), `empirical`'s four values (22a-ii), the new estimand words (22a-iii), plus whatever Phase 22b adds to the option surface (**22b-i landed**: `add_n` Bool -> an `n` ComboBox with `range` / `min` / `no`, in all four YAMLs, on both modules -- until `prepare()` runs the module silently falls back to the option default; the new display presets of 22b-ii, **22b-iii's `base_est_mdiff` / `base_est_mratio` + the `est_coef` radio, already in both YAMLs**, and any argument moved out of a signature in 22b-vi). ⚠ **22b-ii also REMOVED a control**: the `ci_print` radio pair is gone from `jmvtab.a.yaml` and `jmvtab.u.yaml`, and `jmvtab.b.R` no longer swaps the option around the render — the retired option is replaced by the `base_ci` / `base_moe` display presets, which the display ComboBox already offers. Until `prepare()` runs, the stale `.h.R` still declares `ci_print`; that is harmless (nothing reads it), but the generated JS must be regenerated so the dead radio buttons leave the UI. Until it runs, a YAML option that the stale `.h.R` does not carry is INERT, not merely undocumented — see the "Jamovi module development" section above.

#### Phase 22h — documentation reviews

From Phase 22b: one clear sentence on what "standardised" means for a numeric predictor — it is a post-hoc k-unit contrast, the predictor is neither rescaled nor centered (22b-v). The compound formula (`outcome = y ~ a * b`) is documented in `?tab_reg` and the regression vignette as the EXPERT EXIT DOOR (custom contrasts, hand-written offsets, three-way terms), never as the way to write an interaction — 22b-ix gives interactions their own argument.

From Phase 22a: the regression vignettes' PROSE for the new vocabulary (22a-iii ships only the acronym grid) — the crude/adjusted comparison read across the table, the `m` / `@ref` markers, the `{est}` / `{base}` display recipes, and `empirical = "column"` in the multinomial part (one sentence + one `eval = FALSE` chunk).

In the introduction vignette: teach levels = "first" and levels = "auto" with the FactoMineR:: tea dataset; also teach to use tab_vars + spread_vars to make a very condensed table of on the tea dataset.

**PARTLY DONE (2) — the assumptions behind a non-default `measure` are now written down.** Both regression vignettes gained `### With`effect = "coefficient"`, the measure chooses the model`, before the caveats list. The heading is scoped on purpose: only the COEFFICIENT row lets a measure change the fit — verified against the table, exactly three rows do (gaussian ratio -> `mr`, binomial ratio -> `rr`, binomial difference -> `rd`), while every `marginal` / `at_reference` row runs on the family's own model and differs only in the averaging step. The section holds: a table of every `(family, measure)` at `effect = "coefficient"` with what it fits and what the coefficient assumes, then what the literature says about the three that change the LINK (binomial ratio / difference, gaussian ratio) — and about the defaults, which are not assumption-free either. The recommendation is stated with its reasons rather than as a preference: keep the model on the family's own scale and get the reported measure through `effect = "marginal"`, because the logit always converges and cannot predict a probability outside 0–100 %, a marginal effect imposes no constant-effect assumption on the reported scale, and one fit answers every measure. With the counterweight: conditional and marginal are different ESTIMANDS, not two spellings, so the coefficient route is right when the conditional quantity is what is wanted. The three link-changing routes are graded rather than lumped — the modified Poisson is standard practice (its limits are small/sparse samples and unbounded risks), the identity link is the fragile one, PPML is consistent under a correct mean function only. Also corrected: the LPM fallback message claimed "same estimand, robust standard errors"; it targets the same risk difference but is a different ESTIMATOR, and now says so.

**PARTLY DONE — `levels` and the summed score now run on the `tea` data.** `gss_cat` has no real multiple-answer question, so every place that taught a *battery of binary items* was faking one out of unrelated variables (`married` + `black` + `income25k`, and a 0–2 "score" from `married` + `income25k`). Both moved to `FactoMineR::tea`, which carries two real six-item batteries: the intro uses *when do you drink tea?* (`breakfast` … `always`), the regression vignette *where do you drink tea?* (`home` … `pub`) — the "where" battery because it actually separates groups (men `1/1.44***` per place, `senior` `1.44**`), while "when" produced a table with nothing significant to read. `score_from_lv1()` moved with them: its section LEFT the programming vignette (where it was taught in the abstract, away from any use case) and is now three sentences inside the intro vignette's new `### Multiple-answer questions: one column per item` subsection, where the battery it sums is already on screen — the score joins the six item columns of the same `levels = "first"` table as a mean. The regression vignette just uses it now and points there. **`levels = "auto"` was undocumented**; it now has the example that shows what it actually decides — a battery of binary items beside a 3+ level factor, where it collapses the first and keeps the second (`tot = "row"` drops the Total column, which would read a misleading 100 % there until Phase 22b-i lands). `FactoMineR (>= 2.0)` added to Suggests; the shared prep chunk is inline and identical in all six files (a `tea_data_formatting()` twin of `gss_cat_data_formatting()` was considered and rejected — no new public API before CRAN).

Two details worth keeping. The prep puts the "yes" level FIRST (`fct_rev()` on the items whose level 1 starts with "Not"), because that is both what `levels = "first"` keeps and what `score_from_lv1()` counts — the one thing a reader must get right. `Sport` gets the same cleanup, since it is a predictor in the regression example. `?score_from_lv1`'s `@seealso` was repointed from the programming vignette to the intro one. ⚠ Still open from this phase: the `tab_vars` + `spread_vars` condensed table on `tea`, which waits on Phase 22b-i (the `n` / Total column) and 22c-ii (`tab_spread`).

**DONE (3) — a second, deeply pedagogical regression article: `vignettes/articles/tabxplor-all-else-equal.Rmd`** ("All else equal: reading a regression without losing the data", 611 lines, 33 tables, renders in ~13 s). Maintainer's decisions this session: it lives in `vignettes/articles/` (**pkgdown-only**, so no CRAN build-time or tarball cost, and `.Rbuildignore` already excludes that folder); it is a **pure complement** — nothing was removed from `tabxplor-reg.Rmd`, which gained one cross-link paragraph marking the pair as tutorial + reference; English only, the French twin deferred to after a manual review.

**What it is for, and why it is not the existing vignette.** `tabxplor-reg.Rmd` is organised BY FEATURE — one section per family, per argument, per colour mode, plus the estimand grid, weighting, interactions, footer stats and plots. It is a reference, and it never walks one analysis end to end. The new article is the narrative twin, aimed at the audience the package names first (literary social-science students who dislike maths): it starts from a cross-table the reader can already read and never lets go of it. It teaches the two things the roadmap listed as still-unwritten prose — the **crude/adjusted pair read left to right** across the page, and **`family` × `effect` × `measure` as a ROUND TRIP** (the observed quantity chooses the family; a measure turns it into a comparison; the model holds the rest equal; then you travel back DOWN to a quantity you can put in a sentence) — plus the `{est}` / `{base}` display recipes. Six movements: the words in plain language · start from what you can see · turn a percentage into a comparison · hold the other variables equal · come back to something you can say · what the model cannot settle.

**Four data sets, each measured before being chosen, none of them `gss_cat`** (whose adjustments barely move, which is pedagogically flat). `carData::Arrests` is the spine (Toronto Star, n = 5 226: released 86 % White vs 74 % Black, crude OR `1/2.11***` -> `1/1.48***`, marginal `-11.7 pts***` -> `-5.2***`, adjusted predictions 84 % vs 79 %). `carData::Salaries` (n = 397) carries the numeric outcome and the mediator problem. `questionr::hdv2003` (INSEE *Histoire de vie*) carries cultural practice by class. `datasets::UCBAdmissions` (base R, no dependency) carries the reversal. `DESCRIPTION`'s `Config/Needs/website` gains `carData, questionr` — **not** Suggests, since the article is outside the build — and `_pkgdown.yml` registers it in BOTH required places (the explicit navbar `articles:` menu and the `articles:` index; an incomplete index is a hard pkgdown error). Verified: `pkgdown::as_pkgdown()` sees it as `articles/tabxplor-all-else-equal` and the index builds with all 7 articles covered.

**The bridge the whole article rests on is exact, and was verified before a word was written**: the plain cross-table's own odds ratios (`1/2.11***`, `1/1.26*`, `2.99***`, `2.11***`) ARE the regression table's `Obs_OR` column, to the digit AND the star. So the observed column is not a summary of the cross-table — it IS the cross-table, printed beside the model, which is what makes the crude/adjusted distance mean adjustment and nothing else.

**One section is a genuine discovery rather than an exposition, and it is the article's best moment.** On `hdv2003`, the class gap in cinema-going APPEARS to grow under adjustment for age (OR `1/6.63***` -> `1/10.75***`, ×1.6). It is not a finding: mean age is flat across classes (46-50), so there is almost nothing for age to confound. Re-asked on collapsible scales the gap barely moves — risk ratio `÷2.9` -> `÷3.1` (×1.07), difference `-43.2` -> `-45.2` points. That is **non-collapsibility demonstrated rather than asserted** (Mood 2010), and it is the empirical justification for the package's own rule that a `color = "adjustment"` gap is coloured but never TESTED on an odds-ratio column. It also disciplined the rest of the article: the "five things adjustment can do" taxonomy (holds / shrinks / vanishes / grows / reverses) is stated entirely in **percentage points**, on one scale, and forward-references this section.

**Vocabulary: `deviation` is the umbrella, `difference` is one measure.** Maintainer's call, and it is not a new convention — it restores the package's own: `DESCRIPTION` already says "highlight **deviations** (differences from totals, ..., odds ratios, etc.)" and the intro vignette already asks "**How to measure deviation?**" before listing `diff` / `ratio` / `contrib` / `OR`. The article had drifted into using "difference" in both senses; it now reserves it for the `diff` measure, adds **deviation** to the plain-words glossary ("how far a group sits from the reference") with **effect** defined as "a deviation attributed to one predictor", and states significance in the intro vignette's own canonical form — the interval excludes the **neutral value**, 0 for a difference, 1 for a ratio or an odds ratio. ⚠ For the French twin, the natural term is ***écart***, which is also Cibois's (*les écarts à l'indépendance*) — settle it in `dev/french_glossary.md` before translating.

**A second concretisation pass followed, on the same principle: name the concrete thing, not the abstraction.** (1) **`measure` is read as the *measure of deviation***, not "the scale it is expressed on" (the reference vignette's wording) — and, on the maintainer's correction, **not a "unit" either**: a difference, a ratio and an odds ratio are not interconvertible restatements of one number, they are three different measures of it (subtract / divide / divide the odds). "unit" is now reserved for `multiplier`, where it genuinely is one (per SD, per 10, per 1). "scale" survives only where it is literally one (an ordered scale, the odds scale). **`effect` was mis-framed and is re-framed**: it is not "which comparison" (that is `ref`) but **what you do with the model once it is fitted** — read its coefficient straight off, use it to work out a figure for every real person and average, or evaluate it at one profile. So the three arguments are three non-overlapping questions: `family` = what kind of number is the outcome, `measure` = which kind of deviation, `effect` = what to do with the model to get it. ⚠ One claim was drafted and **measured false before shipping**: "`effect` never changes what was estimated" — with `measure = "ratio"` on a binomial, `effect = "coefficient"` fits `rr` while `"marginal"` fits `binomial` (checked via `reg_formulas()`). The text now states the true rule (at `marginal` / `at_reference` the fit is always the family's own; the `coefficient` row is where a measure can change the link) and defers the three combinations to `vignette("tabxplor-reg")`. (2) **A real collision was found and removed in the article's centrepiece**: "level" was used four lines apart for a factor's CATEGORY and for `{base}`, the number the estimate sits on. The reading-a-row passage now says "the two **percentages** sit at the outer edges" and names the package's own word once — *tabxplor calls that outer number the **base**, whatever it happens to be* — leaving "level" to mean a factor level everywhere. (3) **"marginal" is explained from a thing the reader already has**: it is the same *margin* as the **margins of a cross-table**, the Total row — what you get when you stop splitting people into groups and look at everybody at once, which is exactly the operation. `effect` is then posed as the question **"for whom?"** with a three-row answer table (`coefficient` = anybody, the model assumes one answer fits all; `marginal` = every real person, then averaged; `at_reference` = one imaginary profile), and the gloss closes the article's arc: climbing onto the odds scale is what made the numbers abstract, and **averaging back over real people is the step that lands on the scale the counted percentages live on** — which is why the crude and modelled figures can share a row at all. It never appears as English prose in the article, only ever as the literal `effect = "marginal"`, so one gloss suffices. (4) The glossary now names **observed (or crude)** together, since the column header says `Obs_` while the literature says *crude*. **(5) One factual error was found and fixed while linking the gloss back to §2**: the reference-profile passage claimed that adding sex and citizenship to a model containing `checks` would show a baseline group of seven people. Measured: with a CONTINUOUS predictor in the model the Constant row carries **no count at all** (22b-x's own rule — a profile at one exact value of a number is a place nobody sits), and the 7 belongs to the all-categorical `colour + sex + employed + citizen` model (verified against the raw subgroup). The corrected passage teaches the rule instead of the anecdote, and now names the call so a reader can check it.

**A third pass settled the ORDER and the framing of `effect`, both on the maintainer's correction.** (a) **The pedagogical order is `family` x `measure` x `effect`, not the signature's `family` x `effect` x `measure`** — `measure` is a question about *what you want to know* and can be answered from a cross-table with no model at all (the article's S2 does exactly that), while `effect` is a question about *how to get it* and cannot be asked until a model exists. The article already ran in that order; it now states it, and says why it differs from the help page. Corroboration from the package itself: `reg_measures()` already prints measure-major (it varies `effect` within each measure), and the reference vignette's own grid already puts measure in the ROWS and effect in the COLUMNS. (b) **`effect` is re-framed as "where does the number come from?"** — the model's own coefficients (the deviation is what the model fits, changing the model if the measure demands it), or worked out from the model's predictions for every real person then averaged, or worked out the same way at one profile. Strictly better than "what you do with the model", because it explains WHY the coefficient row can change the fit. (c) **"the family's own measure"** is the prose name adopted for `REG_ESTIMANDS$default` — *the one its arithmetic hands you without being asked twice: an odds ratio for a logistic model, a mean difference for a linear one, a rate ratio for a count*. (d) WARNING **a hazard created by the margins-of-a-cross-table gloss was caught and defused**: a Total row is both marginal AND unadjusted, so the mnemonic risks teaching that a marginal effect is a crude one, and the literature warns about exactly this conflation. The article now carries an explicit paragraph — *a marginal effect is still fully adjusted; what is averaged away is not the adjustment, it is the fact that the effect is a different size for different people*.

**On renaming `effect` (asked, researched, recommended AGAINST).** `tab_reg()` is unreleased, so a rename would cost no deprecation and the window is open until 2.0.0 ships; it should still not be taken. The three values map exactly onto the literature's own axis — conditional effect / AME / MER, and MER is the standard name for the at-a-profile case (tabxplor used that acronym itself until 22a-iii). No comparable tool offers a better single word: `marginaleffects` splits this axis across separate FUNCTIONS plus `newdata`/`by`, Stata across `margins ... atmeans / at()`, `ggeffects` across `type`. Two narrower options were weighed and rejected. Renaming `"coefficient"` to `"conditional"` would make the value list internally consistent (all three would name a quantity, instead of one naming an artefact) and literature-exact — but "coefficient" is the more teachable of the two for a non-technical reader, since it names something visible in any model printout; the article says "the conditional effect, which is what the model's own coefficient is" and keeps both. Renaming `"marginal"` (to kill the "marginal = slight" reading) must NOT be done: the whole `REG_WORDS` / `REG_CONTRASTS` vocabulary hangs off it — the `m` marker, `Model_mRD`, "marginal risk difference" — and 22a-iii built "one name per quantity" on that hook.

**Method: a numbers dossier before any prose.** Every table the article contains was run first and written to a scratchpad file; every figure in the prose was copied from it, never recalled. A final scripted pass then re-checked **21 groups of quoted figures against the rendered html** — all pass. This was cheap insurance against a known failure mode (`dev/reg_crude_adjusted_and_display_integration.md` records two stale claims in the existing reg vignette's prose; 22b-vi found three more).

**Three defects found and REPORTED, not fixed** (all reproduced; none blocks the article, which routes around them):
1. **`display = "{est} ({obs})"` and `"{est} ({gap})"` are SILENT no-ops at build time.** `obs` is populated on the model column (measured: 0.474, 2.994 — the crude ORs), and the post-hoc `set_display("{est} ({obs})")` renders it correctly (`1/1.91*** (1/2.11)`), but passing the same template to `display =` drops the bracket group and emits **no message at all** — `display_note_empty()` does not fire. Cause is stage order: `reg_apply_display()` runs inside the column builders (`R/tab_reg.R:3211/3272/3295`) while `reg_set_obs()` runs later (`R/reg-spec-build.R:204`), so the field really is void when the template is written. The article teaches the `set_display()` form and says so in one clause.
2. **`set_display()` does not refresh the footer's aside clause** — `set_display("{est} ({gap})")` still prints "in parentheses, the adjusted predicted probability". The clause is composed at build time from `display =`; the post-hoc setter rewrites cells but not `meta`.
3. **A gaussian effect column mixes two number formats within one column**: the Constant row renders `101 002.4` (thin-space separator) while the effect rows render `+14088.0` (none) — same column, same table. Also noted: `display = "base"` leaves the column headed `Model_OR` while every cell shows a percentage (coherent with the design — `display` never changes what a column IS — but a reader sees "OR" above `79%`), and `tab_reg()` has no `digits` argument to tune either.

**Still open**: the French twin (`vignettes/articles/tabxplor-all-else-equal-fr.Rmd`) and its `_pkgdown.yml` row in the "En français" group, after the maintainer's manual review. `dev/french_glossary.md` should settle the twins for the new plain-language vocabulary (outcome / predictor / adjusted / "all else equal" / composition effect -> *effet de structure*); the research for it is done — Cibois's *effet propre*, Deauvieau's *« le langage du tableau croisé »*, INSEE's *à structure constante*.

Add a direct Word export ? No, teach to go through Excel: it’s recommended to store the tables with the real numbers, not the rounded ones, and do the last formattings if needed, then to copy-paste to Word (using the app, not the web browser, not to lose formatting).


#### Phase 22x — very last features before release

⚠ **Open, found in 22e-i, reported not planned: a declared panel that cannot be drawn.** `REG_CHECKS$residuals$families` includes `"ordinal"`, so `check = "auto"` asks for a residual panel on a `polr` / `svyolr` fit — and `rd_resid()` has no ordinal arm, so it returns `NULL` and the grid silently loses a panel it declared. Two honest cures, pick one: give `rd_resid()` the ordinal randomised-quantile arm (a cumulative-probability draw between the two cut points, which is well defined), or drop `"ordinal"` from that row's `families` and from `normality`'s. ⚠ Whichever is chosen, the fact table and what can be drawn must agree — a family listed in a row is a promise `reg_panel_keys()` makes to the user.


---

### Phase 23 — documentation integration and simplification 2


#### Phase 23a — vignettes simplification and integration
- **One vocabulary rule for `deviation` / `measure`, decided in `dev/reg_estimand_api_redesign_follow_up.md` §6** (the rename to `deviation` is refused; §6.2 is the rule that replaces it): *a **deviation** is the quantity — how far a group sits from its reference; a **measure** is which of the three ways it is expressed.* Write "measure of deviation" the first time the argument appears in a document, `measure` alone thereafter. Today `vignettes/tabxplor.Rmd` teaches "How to measure deviation?" for `color`, the *All else equal* article teaches "measure of deviation" for `measure`, and `vignettes/tabxplor-reg.Rmd` teaches neither — while 6 of its 7 uses of the word are "standard deviation". French is already fixed in `dev/french_glossary.md` (*écart* / *mesure (de l'écart)*).
- Document undocumented stuff. `spread_vars` in `tab()` ?
- Vignettes should not be neverending. If some aspects, either expert, or on the contrary pedagogical and near useless to experts, need to be placed in new vignettes, make me propositions. Point to `tab_structure()` · `tab_supports()` · `reg_measures()` · `tab_columns()` · `fmt_attr()`, etc., when relevant.


#### Phase 23b — roxygen2 documentation simplication
- Carry §6.2's *deviation / measure* rule into `?tab`'s `@param color` and `?tab_reg`'s `@param measure`, and say once, in `?tab`, that an acronym names a **measure** while `display =` names a **field** (so `or` / `diff` / `ratio` are legal in both arguments with different meanings).
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
