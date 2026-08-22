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
- `reg-assumptions.R` — model checks + `shape=` cures; `REG_CHECKS`; the plot primitives.
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
| `ratio`     | dbl  | Ratio to the reference (the "×2" comparison the colour engine reads)  |
| `or`        | dbl  | Odds ratio / relative-risk ratio                                                    |
| `obs`       | dbl  | The observed value a `tab_reg` estimate is compared to (`NA` elsewhere)       |
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
| `col_group`    | chr  | The sub-population a block belongs to (a spread level or `tab_vars` group; `""` otherwise)  |
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

### The regression subsystem

`tab_reg()` gives models the same visual language: one model per column, each estimand rendered as `fmt` cells in the same `tabxplor_tab`. It reuses the 21 fields unchanged; `obs` and `gap_se` carry the regression-specific facts.

**A model column holds the crosstab's own pair.** It stores an adjusted level and its reference level, and derives both readings of that pair — additive (`diff`) and multiplicative (`ratio`); the observed column derives the same two from the counted pair. `measure` says which geometry is **promoted to the estimate** — the one carrying the interval, the stars and the colour — the others riding as asides exactly as in `tab()`. That is the round trip the package exists for: from an observed percentage out to a model and back to a percentage.

**The estimand is a cascade** (`reg-estimand.R`). **A link is a measure**: the one a model estimates directly — `difference` ↔ identity, `ratio` ↔ log, `odds_ratio` ↔ logit — so the argument naming the model takes the same words as the argument naming the report, and the statistician's vocabulary never surfaces. Four arguments, `family` → `link` → `measure` → `effect`, where `"auto"` means *follow from the left*, and one rule decides the rest: **a coefficient exists only where the reported measure IS the model's**; any other measure is applied to the model's predictions, averaged over the sample (`marginal`) or read at one constructed profile (`at_reference`, the ideal type). One clause qualifies it: `"auto"` never resolves to a *predicted* odds ratio, a specialist quantity asked for by name. Which model is fitted and which deviation is reported are two axes — `reg_formulas()` says what reached `glm()`, `reg_measures()` what an outcome can be asked.

`REG_ESTIMANDS` is **composed, not written**: `reg_compose_library()` emits one row per buildable `(link, effect, measure)` from four facts a family declares in `REG_FAMILIES` — its `level` kind (`pct` / `mean` / `count`), the `fits` it offers (the value set of `link`, first entry = its own), any header-word override and its footer qualifier — plus two shared maps: link ↔ measure, and what each kind of level can be compared by. A refusal is not a row but a derivation from the clause that failed, so a hole and its reason cannot drift apart. The family is auto-detected from the outcome (binary → logistic, unordered → multinomial, ordered → cumulative-OR ordinal) while a *number* is the user's call; one table can mix families, each column storing its own `model_family`. Hence the extension rule: **a new model is a row in a declared table, never a new argument or a word a user must learn** — a link is one map entry plus one `REG_LINK_FUNS` row (its transform and derivative — all a marginal contrast needs of a link, which is why the engine has no per-measure arm); a family is one `REG_FAMILIES` row, its footer statistics and model checks the only per-family work.

**One name per quantity** (`REG_WORDS` + `REG_CONTRASTS`). A header names the **measure**, the **contrast** is a marker on it and a log wraps the result, so the word is *composed* — `marker ∘ log-wrap ∘ acronym` gives `OR`, `mRR`, `refRD`, `log(cumOR)` — which stops two estimands sharing a header, or one estimand being named twice. The observed column and the colour legend take the measure **without** the marker — a univariable effect has no adjustment to be marginal over — so the observed/model pair stays one legend block.

**The observed companion — the distinctive feature** (`reg-empirical.R` + `reg-influence.R`). With `empirical = TRUE` each modelled effect sits beside the **observed (crude)** one: the same estimand, on the same people, with one predictor instead of all of them — so *what did adjustment change* is read across the table. One column shape built twice, and the observed shape is composed rather than declared (`REG_EMP_BY_LINK` indexes `REG_EMPIRICAL` by the measure's link), so a model row and its twin cannot state two estimands; its value is a closed form on the per-cell grid where the univariable model is saturated, otherwise a refit through the very fitter the table came from. `reg-influence.R` computes the **standard error of the gap**: both estimators are fitted on the same rows, so only the difference of their influence functions carries the covariance — and that gap SE is what makes `color = "adjustment"` a test rather than a description. On a non-collapsible measure the movement is coloured but never tested: an odds ratio moves when any strong predictor is added, which is arithmetic, not confounding.

**A parametrisation is decided while the data is prepared.** An `a*b` entry in `predictors` is *a predictor whose levels are combinations, and whose univariable model is its own saturated fit*, so it is materialised as a column before the fit and every subsystem keeps reading an ordinary predictor; `REG_CROSS_ARMS` (`reg-cross.R`) declares its two shapes — a combined factor against one common reference, or slopes nested in a moderator. `shape` recodes a continuous predictor the same way, and `ref` shifts one to its anchor so the fit's own intercept is already the baseline the Constant row shows. One rule covers the three: **the boundary defines the model's variables, then fixes their origin** — and the fit's own output is already the table.

**The boundary and the build** (`reg-resolve.R`, `tab_reg.R` + `reg-spec-build.R`). `reg_resolve_args()` is the crosstab boundary's twin, with `data` *inside* it — `family = "auto"`, `multiplier = "sd"` and `shape` are answered by the data — and one grammar per axis: the four estimand arguments per outcome, `multiplier` / `shape` / `ref` per predictor (unnamed = the fallback, named = that variable). `reg_build()` then runs over a typed `new_reg_ctx`, its per-model half a declared product (`reg_spec_build()`), the three nesting axes — `tab_vars` groups × models × outcomes — dispatching through the shared parallel seam.

**Effects and model checks.** A marginal quantity comes from tabxplor's own analytic g-computation, or from `marginaleffects` at a reference profile — derived from the contrast, never declared per row. `REG_CHECKS` catalogues the checks (linearity, dispersion, influence, proportionality, collinearity), each with the `shape =` cure that fixes what it flags — the check and its cure are one object — and each priced (`free` runs by default, `refit`-cost checks are opt-in).

### Exports and rendering

`tab_export(x, format =)` (`tab-export.R`) is the facade over four backends: HTML (the default), Markdown, Excel and plot, sharing one preparation step — `tab_export_prep()` (`tab-export-prep.R`) builds an ephemeral render model (roles, references, faces, header spans, variable-name blocks) that every backend consumes. A spread swaps the two header bands, since after a spread a **column** is identified by its sub-population and a **block** by its variable: the column header takes the `col_group`, the span takes the `col_var` and, above it, the level only where that variable gives several columns per group.

Display values reach the backends by one source of truth: `format.tabxplor_fmt()` renders the text for console, Markdown and HTML, and `tab_xl()` writes the raw value with number-format codes from that *same* `format(syntax = "excel")`, so a display change never needs mirroring. Colour is single-sourced too — every backend reads `fmt_color_channels`. HTML colour is a slot **class**, never inline hex, the theme living in a `<style>` block from the one CSS generator (`tab-css.R`), so light/dark and the publication palettes work by stylesheet — except `print_marks`, whose signal is cell text and so comes from `format()` like the stars. `tab-transpose-render.R` flips a finished render model (a transposed column is heterogeneous and cannot be an `fmt` column), and `tab-theme-detect.R` best-effort-detects the console's scheme — a subsystem that must never error, because a wrong guess only mis-tints.

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

---

#### Phase 22b — `tab_reg()` manual review (suite)

##### Phase 22b-xviii — new round of tab_reg() manual review

```r
tab_reg(gss_simple, outcome = "married", predictors = c("race", "rincome", "relig", "age"),
        family = "binomial", empirical = TRUE, link = "difference"
) 
tab_reg(gss_simple, outcome = "married", predictors = c("race", "rincome", "relig", "age"),
        family = "binomial", empirical = TRUE, measure = "difference"
)
tab_reg(gss_simple, outcome = c("married", "income25k"), predictors = c("race", "relig", "age", "tvhours"), empirical = TRUE, shape = c(age = "quadratic))
```
- Compared to `link = "ratio"` or nearly all other configurations, where observed proportion and adjusted proportions have 0 digit (percentage default, good), in a a model with `measure = "difference"` they get 1 digit, which is pure noise and make the table less readable while losing horizontal space.
- `tab_reg()` sparklines miss some kind of absolute vertical scale: currently, they may actualy amplify noise on a near flat curve, and render hard to distinguish between flat and bad shape. Would there be a way, meaningful accross different numeric variables, to get closer to an absolute vertical scale, in a reliable way ? Make web searches if needed.
- `tab_reg()` numeric predictors sparklines break the padding and alignement of the whole table in console, due to non-ascii characters`▁▅▇▇▇███▇▅` (but it prints ok in a .md script in Positron). I don’t know what to do, because I could change the console font for me but not for all users. 
- The sparkline disappear with several outcomes and "n_range", because one `n` column per model would be needed for that and that’s not the default option. 
- The solution to implement for console: make the "sparkline in n column" feature dormant for console, add a second small sparkline table below the footer table (one empty line between the two different pipe tables). It should be something like that (improve it), with the sparklines in the last column so we don’t really care anymore about broken padding :
| outcomes        | numeric predictors | <meaningful_name> |
|:----------------|:-------------------|:------------------|
| married         | age²               | ▁▅▇▇▇███▇▅        |
|                 | tvhours            | ▁▅▇▇▇███▇▅        |
| income25k       | age²               | ▁▅▇▇▇███▇▅        |
|                 | tvhours            | ▁▅▇▇▇███▇▅        |
- The solution to implement for html and Excel: keep the sparklines in the n column when there is only one outcome, it’s actually good. With several outcomes, create another small table below the main one (below it’s footer too ; with an empty line between the footer of the first and the sparkline table ; in this specific case, use two times more vertical space, and two times more horizontal space, than in the case where the sparkline is inside the n column, since there is more space.)


- On console, the thing difficult to find while reading a regression table is... the outcome. I think the only good solution may be to add the col_var name like with several outcomes, `Obs_OR [married]`, but to remove it automatically at export (since it will already be printed in the first header row). 

Model footer: 
- Pearson dispersion should always come afetr Dispersion (robust/model SE).
- "Overall association (LR)": do you think these ones are really needed as default, with all the informations there is already in the regression tables + footers ?
- Please find a logical order for the footer, based on real-world model check use cases. N is a right first row ? Then put together everything that is about assumptions checks, in their order of importance (stats that can be a no-go first, then more informative stuff) ? Then put together everything that is about (homogeneous) model comparison ? It should be thought. 
- I want to re-add Brant as a default for ordinal models, it’s too important even if it’s long (even if the rule here, would be use `reg_check_plots()`).

```r
tab_reg(gss_simple, outcome = "married", predictors = c("race", "rincome", "relig", "age"),
        family = "binomial", measure = "difference", color = "adjustment")
tab_reg(gss_simple, outcome = "married", predictors = c("race", "rincome", "relig", "age"),
        family = "binomial", measure = "difference", color = c(TRUE, "adjustment"))
```
- html tooltips "gap" still speaks in pts like "-1.9pts [-2.9 pts; -0.9pts]", which is done nowhere else in the package (or should not be). Please use the standard "diff" formatting: something like "-1.9% [-2.9;-0.9]%"
- background colors still color not only the primary display token (like text colors does, keeping the stars out of the color, etc.), but also the whole cell text with secondary display tokens. I noticed this in html. It is an html specific problem, or something more general ? 
- Also, there a strange cases where the model (and sometimes the obs too) have no stars, but the gap comparison gives background colors: is this wanted (what is the meaning and interpretation then ?), or is it a statistical error ? 
- background color keep to misalign just a bit their cells text with the other rows: it looks like a margins problem, fix it (even if the hack is to set a cell background with the overall background color so that they are hidden ; but if you think of a better solution, do it).

```r
tab_reg(gss_simple, outcome = "married", predictors = c("race", "rincome", "relig", "age"),
        family = "binomial", measure = "difference", color = c(TRUE, "adjustment"), shape = c(age = "sd_bands"))
tab(gss_simple, age, married, pct = "row", color = TRUE)
```
- I want sd_bands levels to look something like that (with french translation "moy.")
| current                | proposition (improve it) |
| age: [18,30) low       | age: [18,30) ; < mean - σ|
| [30,48) below average  | [30,48) ; < mean         |
| [48,65) above average  | [48,65) ; > mean         |
| [65,89] high           | [65,89] ; > mean + σ     |


#### Phase 22e — plots manual review

##### Phase 22e-i — assumptions plots

**DONE.** `reg_check_plots()` rewritten around three changes: **one titled grid per model**, **`data =` written once**, and a panel text a first-year student can act on. The two reported errors are fixed, and two structural defects found while reproducing them are fixed with them.

**The two reported errors.** (1) `geom_hline()` aborting on any multi-model call was reproduced on **ggplot2 4.0.3**: `reg_panel_collinearity()` passed `yintercept = c(5, 10)` with `linetype = c("dashed", "dotted")` as geom *params*, and faceting replicates a layer's data across panels but not its params — 2 models × 2 marks = 4 rows against a length-2 param. Reference lines are now a data frame read through `aes(yintercept =, linetype =)` + `scale_linetype_identity()`, which is facet-proof by construction; the rule is a `# WARNING:` on `reg_panel_mark_layers()`. (2) `"No model could be refitted from x"` on the `tea` example was `data = gss_simple` passed to a table built from `tea` — the slip the API invited. Both the cause and the message are gone: the abort now carries the first refit's own error and says to check `data`.

**`data =` is written once.** `tab_reg()` stores `deparse(substitute(data))` as `fit_spec$data_expr` (captured beside the four `enquo()`s, before any promise is forced). `reg_plot_fits()` re-resolves it **only when it is a bare name** — cheap, side-effect-free, re-runnable — in the caller's frame, with one `cli_inform()` naming what it used; a pipeline, a subset or `.` keeps the abort, which now names the expression. The safety was already built: the existing N guard aborts when a rebound name does not reproduce the model.

**One grid per model (ruling R10 revised).** Faceting by model could only ever apply `ctxs[[1]]$family` to every model — measured: a `binomial + ordinal` call drew binomial panels for the ordinal model. Every builder now takes ONE context, `reg_panel_facet()` is deleted, and each grid gets `reg_check_top()`: the outcome and the model's own formula in bold (terms through the new `reg_term_label()`, so `I((\`age\`/12.3)^2)` reads `age²`), then the family and the N in the footer's own words (`reg_family_display_name()`). Returns one `gtable`, or an invisible named list of them. Grid shape is `min(3, ceiling(sqrt(n)))` — 4 panels are 2×2, not 3+1.

**The default panel set is declared, not coded.** `REG_CHECKS` gains two columns: `panel_default` (FALSE on **dispersion** and **collinearity** — one number against one number, their footer row says it all) and `panel_marks` (the reference line(s) a panel draws). A build-time `stopifnot` now also refuses a `panel_marks` that is not the check's own `flag` — which caught a real drift: the influence panel drew `0.25` while `REG_CHECKS$influence$flag` is `1`. `reg_panels_default()` is `reg_checks_default()`'s twin for panels. `"auto"` = 4 panels (gaussian / binomial / poisson), 5 (ordinal), 2 (multinomial); `"all"` = every applicable one, at most 7. **No silent truncation**: dropping a panel the user named would be worse than a taller grid, so the "max 6" is answered by the default set and the `ncol` rule.

**Linearity now shows the shape the model fits.** `geom_smooth()` is gone entirely (which retires the `method = "auto"` hazard the file header warns about): `rd_comparator()` fits an `lm` through the observed bins — `y ~ x`, or `y ~ x + I(x^2)` for a predictor `shape = "quadratic"` has cured — so a cured predictor is no longer read against a line the model no longer fits. The facet is named by the model's own terms (`age + age²`, `log(age)` via `shape_mark()`), bold, 2 columns.

**A link is a measure, and the panel now reads it.** `rd_link_y()` branched on `reg_check_family_of()`, so an `rr` (modified Poisson) or `rd` (identity risk) fit got an *empirical logit* axis — a scale that model never uses. It now branches on the fit key first: `rr` → `log(risk)`, `rd` → `risk`, `mr` → `log(mean)`, with a new exact `"logrisk"` arm in `rd_bin()` (`Var(log p̂) = (1-p)/(np)`, which the Poisson arm overstates by `1/(1-p)`). The row sparkline reads the same producer, so it moved with it.

**Ordinal and multinomial linearity: one curve per reading** (the maintainer's own question, answered with the standard diagnostic). `rd_link_cuts()` gives one curve per cut for an ordinal (`y = 1{Y > k}` — non-parallel curves are the proportional-odds departure the factor-only Proportionality panel cannot show for a *continuous* predictor) and one per non-reference category for a multinomial (on the rows in `{ref, k}`, where `log(p/(1-p))` **is** the empirical generalised logit). Everything else keeps exactly one curve and the ordinary panel is unchanged. Verified on `rincome`: three cumulative-logit curves whose spread against age is visible by eye.

**Text and layout.** The headline is `bquote(bold(<noun>) * ": <question>")` — one bold assumption word, one plain question, one line — chosen over a `ggtext` dependency because the linearity y axis wanted real maths anyway (`empirical logit: log(p/(1−p)), p = P(married)`, built in `rd_link_expr()` beside the word, so one place produces both). The price is that `plot.title` must be `face = "plain"`, or `bold()` would spread over the whole line: that is a `# WARNING` on `reg_panel_skin()`. Subtitles are one short directive sentence, wrapped by `rd_wrap()` (`strwrap`, width 68) so a longer French translation still fits, in a new `cols$subtle` (`#555555` light, `grey2` dark/print) which also drives `axis.text` — the table's own `grey` is too light for words on white, and Phase 22f has its own claim on it. Proportionality's legend moved to the bottom (it was taking half the panel's width) with 4 facet columns.

**French.** ⚠ `potools::po_extract()` had not been run for several phases: the `.pot` was stale, so `po_update()` alone merged nothing. Re-extracted (321 msgids), 22 new strings translated, `po_compile()` rebuilt `inst/po/fr/LC_MESSAGES/R-tabxplor.mo` — the whole grid, heading included, is now French under `lang = "fr"` (verified visually). ⚠ The same extraction surfaced **~20 msgids from Phases 22a–22c that were never in the po at all** (the ordinal superiority vocabulary — `Somers' D`, `win ratio`, the sd-band words, `Cramér's V`): left untranslated for the French pass, but now visible in `po/R-fr.po`.

⚠⚠ **THE i18n TRAP, root-caused here — `po_extract()` must be followed by a `\uXXXX` normalisation pass.** The ASCII-source rule means a msgid is written `"Cram\u00e9r's V"` in R, and potools extracts it **as that literal escape** — while `gettext()` at run time looks up the *evaluated* string. So a bare `po_extract()` silently invalidates every entry with an accent or a dash: it fuzzies `Cramér's V` (`test-i18n-fr.R:85` caught it, the only failure of the first full run) and would have shipped a French build missing them. **Always, after `po_extract()`: replace `\\uXXXX` by its character in BOTH `po/R-tabxplor.pot` and `po/R-fr.po`, re-merge, then drop the `#, fuzzy` flag on every entry whose `#| msgid` is identical to its new one** — 3 entries here. This is what the Phase-18y note about "escapes normalised" meant; it is now written where it can be found.

⚠ **Two characters were removed after measurement**: `U+202F` (the narrow no-break space the tables use as `big.mark`) and `U+2014` in the heading both raise `conversion failure ... in 'mbcsToSbcs'` on a single-byte graphics device — 4 warnings under `testthat`'s reproducible output. A plot label is not a table cell: a plain space and a plain hyphen. `age²` (Latin-1) is fine.

**Verified**: every family of the review block's script (binomial + its `rr` / `rd` links, gaussian, `mr`, poisson, summed-score binomial, multinomial, ordinal, the mixed-family call, a model comparison) draws; the **full suite is FAIL 0 / WARN 0 / PASS 9549**; PNGs reviewed in English and French.

**Open, reported not fixed**: an ordinal `residuals` panel returns NULL (`rd_resid()` has no `polr` arm) although `REG_CHECKS` declares it applicable — the grid silently loses a declared panel. Either give `rd_resid()` an ordinal arm or drop `"ordinal"` from that row's `families`; noted for Phase 22x.





##### Phase 22e-ii — `forest_plot`

**DONE.** `forest_plot()` redrawn around one structural fix and one new skin. The review's complaint
("too gray, unreadable, not enough colors") was real, but reproducing it surfaced a **defect that was
losing data**, and that is what most of the phase went on.

**The defect: a mixed-unit table was plotting `log10` of a signed difference.** `forest_plot()` picked
the **modal** `scale_key` and applied its transform to the whole plot. On `family = c("binomial",
"gaussian", "poisson")` the modal transform is `log10`, so the gaussian panel plotted
`log10(<a difference in years>)`: **every negative estimate became `NaN` and was silently dropped**
(35 warnings), and the ratio ladder `÷1.5 / ×2` was drawn on an axis measured in years. That is the
reported *"age outcome relig only shows one whisker, all the others out of the range"* — a transform
bug, not a range one. The range was also computed **once for the whole table** and included the gap
band, which on the `tea` summed score stretched the axis to `÷4` while every whisker sat near 1.

**One axis per panel, in one ggplot** (the maintainer's own instruction: trick ggplot rather than
split the object). ggplot has ONE scale per aesthetic, so x is pre-transformed into each scale's own
space and the plot's scale is identity; `breaks` and `labels` are **functions**, and with free scales
ggplot calls them **once per panel**, so a lookup keyed on that panel's limits dispatches them
exactly. Verified on ggplot2 4.0.3 before a line was written. ⚠ The limits ARE the key, so they are
forced (a `geom_blank` frame + `expand = expansion(0)`) and **no layer may reach past them** — the gap
band is clamped in data space instead. ⚠ That frame is added **last**: the first layer to touch a
discrete scale fixes its level order, and it carries one level (measured: adding it first reordered
every y axis). `sec_axis()` is the one casualty — it is per scale, not per panel, so the secondary
axis survives only on a single-scale plot; the unit otherwise moves into the strip, through a
`labeller`, never by relabelling the data.

**One range per scale KEY, not per facet** — a deliberate revision of the review's wording. Panels
measuring the same thing must be directly comparable (that is what small multiples are for), while
panels on different units keep their own axis, which is the whole point of a per-panel scale.

**The ladder is trimmed, continued and thinned.** It **continues** past its last rung as
`mag[k+1] = 2 * mag[k]`, mirrored, at most 4 extra rungs — one rule for both geometries, because every
ladder already steps ≈×2 per rung in its own metric (`COLOR_SCALES`' K2 check), so `×2 → ×4 → ×8` and
`+0.3 → +0.6 → +1.2` are the same rule; a colour rung stays exact (a gridline IS a threshold) while a
continuation rung is rounded. The **first rung on each side is always in view**: a plot where every
whisker sits inside the first colour threshold must say so. Lines and labels are **decoupled** — every
in-range rung is drawn, at most 9 are labelled, thinned from the outside in in **whole pairs** (a first
attempt thinned "by roundness" and degenerated into keeping only the negative side — measured). A
scale with no ladder (a level panel) falls back to `pretty()`, the only case where the axis is not the
colour ladder.

**The skin.** `theme_minimal`; the ladder drawn as one dashed rule per rung **in that rung's own
colour** (the `formations_stat` device — the direct answer to "not enough colors"), with the theme's
own x grid off so nothing doubles; the whisker one capped `geom_segment`
(`arrow(angle = 90, ends = "both")`) wholly in the cell's colour, so significance is read off the
whisker and **the stars are gone**; level labels in the table's ink with the reference **bold**
(plotmath, per break, so it survives free facets); strips bold, no panel border, panels spaced.

**`center` replaces `labels`, `stars` and `size`.** `"n"` (default) a square whose area is the level's
base **with the value just above the whisker**; `"estimate"` the value alone; `"none"` a constant
square and no value, for a plot with many panels. The value rides above rather than on the whisker (a
square sized by the base can swallow it), over a translucent rounded halo, and it is the cell's **own
primary token** — the effect on a model column, the level for a crosstab — so the number says what the
position cannot. `format()` stays the one string producer; only the display is swapped first.

**A crosstab is plotted on its measure of deviation**, never on the level: the axis is what `color =`
grades, the reference is a dotted line, and the percentage or mean is the number above the whisker.
The observed value is offset **below** the model whisker, filled black where no band is drawn and
hollow where one is; a crude *series* gets a thin black whisker with very small caps. A band that
fills its panel is **not drawn** — it locates nothing — so a band on the page always means the
estimate is inside it or outside it.

**The layout rule was measured, and the maintainer's hypothesis (`pct`) is wrong.** Both orientations
show the same numbers — a cell's deviation is a stored field, not a property of where it is drawn — so
this is legibility alone. Two shapes, **both `pct = "row"`**, want opposite orientations:

| table                   | keep                                   | transpose                            | better    |
|-------------------------|----------------------------------------|--------------------------------------|-----------|
| 3 races × 6 marital     | 6 panels of 3 rows, strips truncated   | 2 panels of 6 rows, one profile each | transpose |
| 7 classes × 3 tea types | 3 panels of 7 rows, a readable ranking | 6 panels of 3 rows, strips truncated | keep      |

The rule that fits both: **more levels down the side, fewer panels across** — transpose iff the column
axis has more levels. `layout = "keep" (default) | "auto" | "transpose"`: the rule is **opt-in**, on
the maintainer's call — a plot whose rows are the table's rows is the one a reader can check against
the table in front of them, and silently swapping the two axes costs more than a cramped panel. A
regression table is never transposed. The **reference** falls out of it: on the panel axis it would be an all-neutral panel, so
it is dropped and the K−1 panels left are each one group's whole profile — which IS the
`formations_stat` teaching plot, at K = 2; on the reading axis it stays, as the anchor row at the
neutral, drawn as the black-filled square the table's bold reference cell deserves.

**`footer` and `legend`.** `footer = "short"` (default, the console's own `terse` streams) /
`"full"` (the exports' `prose`) / `"none"`, wrapped and flush left. One producer, one renderer, only
the style differs — so a figure and its table cannot state the method differently; and they are
identical whenever a guide carries the ladder, which neither may print twice (D6). `legend` is now a
**position** (`"auto"` → the bottom, matching `reg_check_plots()`; `TRUE`/`FALSE` still accepted).
`return_data` now returns the model **after** the layout, so it describes what is drawn.

**Two more defects found in the final visual pass, both fixed.** (1) A cross-table's figure was headed
**`NA`** — `reg_title()` names a MODEL and returns `NA` on a crosstab, and `NA` is not `NULL`, so it
went straight through `fp_caption()`'s guard (pre-existing, since z17). (2) The **axis title spoke for
a panel it did not describe**: a multinomial risk ratio beside an ordinal win ratio shares one ladder
but is two quantities, and the shared title said "Risk ratio" over the win-ratio panel. The rule is
now that a title may exist only where every panel measures the same thing AND calls it the same
thing; otherwise the word moves into the strip and the title goes. ⚠ The fix needed a second one: the
per-facet scale record was built with `c(recs[[key]], list(col = ...))`, and `c()` **appends** a
duplicate name rather than overriding it, so `$col` kept reading the scale's first column.

**Verified**: the three review calls plus a crosstab in both orientations, rendered and read; the
mixed-family table draws **every** estimate with no warning; suite **FAIL 0 / WARN 0 / SKIP 4 /
PASS 9584**. `test-tab-estimates.R` needed no change (the model is untouched); `test-forest-plot.R`
gained five tests — nothing dropped on a mixed table, per-panel breaks, shared range within a scale,
the band never setting the range, and the layout rule with its reference handling.

⚠ **Reported, not fixed (1)**: `tab(..., color = "diff")` alone stores **no interval**, so a crosstab
forest plot then has points and no whiskers. `forest_plot()` now says so once and names `ci = "ref"`,
but the honest question is whether `ci = "auto"` should give a comparison interval whenever the table
makes a comparison — a `tab()` default, not a plot one. For Phase 22x.

⚠ **Reported, not fixed (2) — the axis ladder and the legend ladder can be two ladders.** The
gridlines come from `fmt_scale_of()$breaks` (the ESTIMATE scale's `break_key`) while the legend keys
come from the colour plan (the MEASURE's scale), and on a ratio-coloured percentage column those are
`mean_ratio` and `pct_ratio` — measured on the `tea` summed score: the axis reads `÷1.2` where the
legend key reads `≤ +1.25`. Pre-existing since z17, and small, but it breaks the design's own claim
that *the gridlines ARE the colour ladder*. The cure belongs to `EST_SCALES` / `fmt_scale_of()` (one
`break_key`, read by both), not to the plot. For Phase 22x.



###### Phase 22e-ii-B — `forest_plot` round 2

**DONE.** Second manual review, twelve items. Three were latent defects rather than styling.

**The reading axis had no fixed order.** A discrete scale takes its order from the layer that trains
it FIRST, and two layers carried a single level — the limits frame and, once it existed, the coloured
row band. Measured: a six-row block came out `Total, [30,48), [48,65), [65,89], NA, [18,30) low`. The
cure is one `geom_blank` over the WHOLE model as the first layer, so every panel is trained in the
table's own order whatever else is drawn later; the note that the limits frame must come last is now
a consequence of it rather than a rule on its own.

**A row band must be a tile on the factor, never a rect on a row number.** `as.numeric(ypos)` is a
coordinate on the FULL level set, which in a panel showing four of eighteen levels is a continuous
value far outside its range: ggplot stretched the panel to reach it (a third of the height lost) and
drew the band nowhere near its row. Fixed, then removed entirely — see below.

**A rule frame must carry the ROW-facet variables.** `rule_y` had only `facet`, so `facet_grid`
replicated every block's rules into every panel, and a `yend` from a nine-level block stretched a
three-level one to reach it.

**The second colour channel is no longer drawn** (maintainer's ruling). `color = TRUE` on a crosstab
gives two measures — `difference` on the text, `ratio` on the background — and rendering the second
one as a band behind every row flooded the figure ("blue and red bands hell") without adding a
comparison: it has no interval, no neutral, and nowhere positional to go. The plot shows the main
measure. The gap under `color = "adjustment"` is not an exception: there the adjustment IS the main
measure. Undocumented on purpose — it is not a limitation to explain, it is what a forest plot is.

**The block separation is now real.** Every rule stops short of the panel edge (`0.62` to
`n_blk + 0.38`, drawn as segments rather than `geom_vline`), a grey rule sits above each block, and
the discrete expansion is tightened to `0.55` — so a predictor reads as a block instead of the grid
running through it.

**The observed row is one thing now**: a filled black point at the crude value, with the gap as a
**thin black capped whisker** at the same offset below the model's. The wide coloured band is gone —
it was the biggest ink on the page and located nothing — and the point no longer changes shape with
whether the gap was testable, which had made the same table look like two conventions.

**The guide.** One row, in the axis's own left-to-right order (deepest UNDER first, the grey where the
ladder is silent, then the OVER side deepening) — ggplot was filling by column, which put the ladder
in an order nothing on the page had. Its keys come from a **frame of their own** drawn at `alpha = 0`:
keyed off the data, a rung no cell happens to fall in got a label with no glyph, measured on both ends
of a `guaranteed_effect` ladder. Under that policy the title now names the quantity it actually
grades — *Guaranteed (95%) difference vs the Total row* — and the **cap nearest the neutral is drawn
twice the size**, because that bound IS the score the colour reads: the policy becomes visible instead
of explained.

**The reference Total row is back.** With `ref = "tot"` the baseline is the Total row, which
`totals = FALSE` was dropping — so the plot lost the one number the panel is about. It is kept
whenever it is the reference, bold, as the black square at the neutral. The `1` / `0` label above a
reference row is gone with it: the neutral line already says it, and the two glyphs landed on each
other.

**Smaller things.** Axis tick labels carry one decimal, rounded in the scale they are READ in (a
percentage-point ladder is stored as `0.05` and printed as `5`, so rounding the stored value would
turn `+5` into `+10`). The axis title absorbs the interval's name where there IS one axis to say it on
— `Percentage points (95% CI, Newcombe score interval)` — and the footer drops the method line it
would have repeated; a many-outcome regression keeps it in the footer, since no single axis can speak
for every panel there.

**One shared-producer change, on the maintainer's call:** an SD-scaled ladder now names its unit
before its numbers, in `legend_measure_word()` — `diff in SD (ref.): -0.8 -0.4 …` — so the console and
every export gain it too. Bare numbers that are not in the outcome's own units had stated their scale
only in the trailing grey clause, after the reader had already met them. ⚠ The clause is gated on the
MEASURE, not on `is_std` alone: that flag is a per-column fact and the first cut labelled a ratio
ladder `IRR in SD`. Zero golden churn (no golden covers an SD-scaled ladder); one i18n assertion moved
with the axis title, which also learned to keep quiet when a column colours without an interval (it
was printing `(95% CI, 95% confidence)`).

**Answered, not implemented.** *Colour in the footer's prose legend* is not available without a new
Suggests (`ggtext` for markup, or `ggnewscale` for a second colour guide): a ggplot caption is
single-styled. It costs nothing today — the ladder is coloured twice already, in the guide and in the
gridlines — and the prose ladder only appears at all when several ladders make a guide impossible.
*The missing whiskers* on `c(age, rincome, party3) x marital` are drawn and correct: measured at
0.16 to 21 percentage points wide (median 3) on an axis spanning 120, with n = 21 483 — a real
interval, smaller than its own marker. `center = "none"` or fewer `columns` is the cure, not code.

**Round 4 — the adjustment reads as two rows**, on the maintainer's layout. The estimate keeps its
own: a **light grey** whisker (`cols$grey`, the table's own uncoloured-cell ink) under a square **in
the arrow's colour**, so the mark and the movement it made are one statement. One `offset` below, the
arrow (linewidth 1.3, closed head), then the **very thin black** acceptance bracket over it, then the
observed point — each thinner or smaller than the last, so sharing one row costs no colour. The
**guide now describes the arrows** (`legend_guide_spec()` is asked for whichever channel carries the
gap): the gridlines still show the MAIN measure's ladder, and would otherwise be the only colour key
on a figure whose colours mean something else.

**Five arguments the viewport decides**: `offset` (how far below the estimate the observed row sits)
and `label_offset` (how far above its value is printed), both fractions of a row because ggplot has no
absolute-unit nudge; `max_size`, the area of the largest marker; `footer_width`, because a ggplot
caption does not wrap and the plot cannot measure the device at build time; and `display`, a `{}`
template passed to `set_display()` for what the label prints (`NULL` = the cell's own primary token,
unchanged). The label's halo is less translucent.

**Answered**: the `married` panel has no acceptance bracket because `gap_se` is **not computed at
all** there (measured: 0/12 finite, against 12/12 on the gaussian and poisson panels) -- an odds ratio
is non-collapsible, so the package refuses the test rather than reporting a small error.

**Round 3 — the adjustment gets its own geometry.** `color = "adjustment"` used only to repaint the
model whisker, so the whisker's POSITION said the effect and its COLOUR said something else. Now the
gap is drawn: an **arrow** from the observed value to the model's, in the gap's own colour, with a very
small closed head, over the **acceptance bracket** `[gap_lo, gap_hi]` around the observed point — the
model estimate falls outside it exactly when the gap test rejects, so the arrow head clearing the
bracket IS the verdict. The bracket is drawn **only where the gap is testable**: on a non-collapsible
measure (the binomial panel: 15/15 `gap_tested = FALSE`) the movement is real arithmetic but no test
exists, and a bracket would claim one. The arrow's length is the adjustment TO SCALE — measured, the
gap runs 0.13 to 1.04 where the effects span -5 to +5, so a small adjustment is meant to look small and
the colour is what says whether the move is real. Two consequences: the **model whisker gives up its
ink** when the adjustment is the main colour (`cols$grey2`, added to `tx_plot_colors()` — `cols$point`
is a muted blue and would read as a rung of the ladder it stands outside of), so colour still grades
exactly one thing; and when the arrows grade a different measure from the whiskers, **two ladders are
drawn**, so no guide is built and the caption carries both in prose. ⚠ Under an adjustment colour the
axis title stops absorbing the method phrase: the spec then names the GAP's test, which is not the
interval the axis draws.

**The observed row moved up**, from `0.24` of a row to `0.15`. ggplot has no absolute-unit nudge — a
position is data space, and a millimetre needs the panel height, known only at draw time — so "two
whisker linewidths" cannot be exact everywhere; measured, `0.15` is 1.3-2.4 mm across the viewports
such a table is drawn at, against the 1.8 mm two linewidths come to.

**Verified**: the review's calls rendered and read at several sizes; forest tests **FAIL 0 / PASS 112**.


#### Phase 22f — exports review

pillar abbreviation in exports: keep the "<>" formatting of the pillar version ; always *italics* for all exports ; aligned left in html and Excel (right on markdown, like the whole column).
- In html export, remove the horizontal border between the normal header row (levels names) and the pillar abbreviations row. The grey should be more grey,`tx_chrome_hex()$grey` for consistency with all themes.




#### Phase 22g — Jamovi UIs manual reviews and final modifications

⚠ **The stale `R/jmvtabreg.h.R` is now BROKEN, not merely out of date** — measured in 22b-xv-2, not inferred: `tabxplor::jmvtabreg(data, outcome, predictors)` aborts with *"options$crosses does not exist"*, the option 22b-ix declared in the YAML. Two of its defaults also name values that have since been retired (`effect = "coefficient"` → `"conditional"`, `display = "value"`), so they would abort in turn once the first is fixed. The `.h.R` is never hand-edited, so `prepare()` clears all three at once — but until it runs the Regressions analysis does not work at all.

⚠ **22c-iii adds to the option surface and nothing is wired yet**: the display ComboBoxes have no `mean_sd` / `mean_cv` entry (a numeric column's default cell changed, so the idle value is right, but the two layouts cannot be chosen), and there is no `shape` control at all for `jmvtab` — the crosstab equivalent of the Regressions panel's own. Decide whether a numeric row variable's grouping is worth a picker there before running `prepare()`.

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


⚠ **Open, found in 22c-iv, reported not planned: a UNIVARIABLE model claims a highly significant adjustment.** With one predictor the model IS its own crude twin, so there is no adjustment to test — but the gap is floating-point noise divided by a floating-point SE, and the test believes it. Measured on `tab_reg(gss_simple, outcome = "age", predictors = "race", family = "gaussian", empirical = TRUE)`, `Model_diff`:

```text
est    = -4.827654   obs    = -4.827654        (equal to the last digit, by construction)
score  = -4.44e-15   gap_se =  2.19e-16   ->   z = -20, p = 8.6e-92   ("p = <0.01%" on hover)
```

So the hover reads `gap: -0.00 [-0.00; -0.00], p = <0.01%`, and `color = "adjustment"` would paint the column at full strength. The cure belongs to the gap engine (`fmt_gap_bounds()` / `fmt_gap_p()` in `fmt_class.R`, fed by `reg-influence.R`), not to the hover, which renders faithfully what it is given: a gap whose size AND standard error are both at machine precision relative to the estimate is **not a gap**, and must yield NA rather than a z of 20. ⚠ Pick the guard against the estimate's own scale, not an absolute epsilon — and check the same clause covers the general case of a model whose predictors are all in the crude fit.

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
