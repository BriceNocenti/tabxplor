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

**The observed companion — the distinctive feature** (`reg-empirical.R` + `reg-influence.R`). It is **on by default**, and one value decides where it goes: `column` (a crude column beside the model one), `cell` (the `est_obs` layout, inside it), `tooltip` (computed, printed nowhere) or `no` — `TRUE` resolving to `column` except where that would double a table already wide (`tab_vars` groups, a per-category outcome), which take `tooltip`. In every mode but `no` the value is stored in `obs` and read by `color = "adjustment"`, by `forest_plot()` and by the hover, so the mode is a layout decision and no arithmetic branches on it. Each modelled effect sits beside the **observed (crude)** one: the same estimand, on the same people, with one predictor instead of all of them — so *what did adjustment change* is read across the table. One column shape built twice, and the observed shape is composed rather than declared (`REG_EMP_BY_LINK` indexes `REG_EMPIRICAL` by the measure's link), so a model row and its twin cannot state two estimands; its value is a closed form on the per-cell grid where the univariable model is saturated, otherwise a refit through the very fitter the table came from. `reg-influence.R` computes the **standard error of the gap**: both estimators are fitted on the same rows, so only the difference of their influence functions carries the covariance — and that gap SE is what makes `color = "adjustment"` a test rather than a description. On a non-collapsible measure the movement is coloured but never tested: an odds ratio moves when any strong predictor is added, which is arithmetic, not confounding.

**A parametrisation is decided while the data is prepared.** An `a*b` entry in `predictors` is *a predictor whose levels are combinations, and whose univariable model is its own saturated fit*, so it is materialised as a column before the fit and every subsystem keeps reading an ordinary predictor; `REG_CROSS_ARMS` (`reg-cross.R`) declares its two shapes — a combined factor against one common reference, or slopes nested in a moderator. `shape` recodes a continuous predictor the same way, and `ref` shifts one to its anchor so the fit's own intercept is already the baseline the Constant row shows. One rule covers the three: **the boundary defines the model's variables, then fixes their origin** — and the fit's own output is already the table.

**A fit is distilled, not kept** (`reg-digest.R`). Everything the table goes on to compute — a marginal effect, a baseline, an influence function, a coefficient at any confidence level — needs a model's `coef`, `vcov`, `terms` and `family`, never the fitted object; so `reg_fit()` returns a **`tabxplor_fitdigest`** beside it, and every engine reads that. Which parts a digest holds is declared, one row per fitting backend (`REG_FIT_KINDS`) and one per stored part (`REG_DIGEST_PARTS`), so **a new model backend is a row**. Nothing length-`n` is stored: the model frame is rebuilt from the live data through the *same* `reg_fit_frame()` the fitter used, and the IRLS working weights and residuals are reconstructed from the parameters. What only a fitted object can answer — the model-fit statistics, the global tests, the assumption checks, each crossed pair's test — is computed **eagerly, while it lives**, and rides on the record; what a digest genuinely cannot serve buys its fit back through `reg_digest_revive()`. Hence the record's one estimand-dependent member is `tidy`, written per `(measure, conf_level)` by `reg_tidy_finalize()` from a native-scale estimate — which is what lets the jamovi cache key on the **model alone** and serve every estimand from one fit.

**The boundary and the build** (`reg-resolve.R`, `tab_reg.R` + `reg-spec-build.R`). `reg_resolve_args()` is the crosstab boundary's twin, with `data` *inside* it — `family = "auto"`, `multiplier = "sd"` and `shape` are answered by the data — and one grammar per axis: the four estimand arguments per outcome, `multiplier` / `shape` / `ref` per predictor (unnamed = the fallback, named = that variable). `reg_build()` then runs over a typed `new_reg_ctx`, its per-model half a declared product (`reg_spec_build()`), the three nesting axes — `tab_vars` groups × models × outcomes — dispatching through the shared parallel seam. **A model comparison is a default too**: several `predictors` sets are tested against each other without being asked, sequential where every model nests in the next and against the first otherwise, decided in `reg_compare_rows()` where the fits exist. ⚠ `compare != "none"` is what makes a build serial and makes it keep its fits, so the boundary degrades the automatic one to `"none"` wherever a comparison has no meaning.

**Effects and model checks.** A marginal quantity comes from tabxplor's own analytic g-computation, or from `marginaleffects` at a reference profile — derived from the contrast, never declared per row. `REG_CHECKS` catalogues the checks (linearity, dispersion, influence, proportionality, collinearity), each with the `shape =` cure that fixes what it flags — the check and its cure are one object — each priced (`cost`) and each declaring whether it runs by default (`footer_default`), because what a table must say and what it costs are two questions. The **observed shape** of a numeric predictor is the free half of the linearity check: one curve per outcome, binned with no fit at all, drawn as a sparkline in a window floored by the data's own sampling noise and by the first colour rung — so a flat run means flat. It goes in the predictor's own `n` cell where the table has one outcome and the medium can hold it, and otherwise in a small **shape table** below the footer.

### Exports and rendering

`tab_export(x, format =)` (`tab-export.R`) is the facade over four backends: HTML (the default), Markdown, Excel and plot, sharing one preparation step — `tab_export_prep()` (`tab-export-prep.R`) builds an ephemeral render model (roles, references, faces, header spans, variable-name blocks) that every backend consumes. A spread swaps the two header bands, since after a spread a **column** is identified by its sub-population and a **block** by its variable: the column header takes the `col_group`, the span takes the `col_var` and, above it, the level only where that variable gives several columns per group.

Display values reach the backends by one source of truth: `format.tabxplor_fmt()` renders the text for console, Markdown and HTML, and `tab_xl()` writes a number with format codes from that *same* `format(syntax = "excel")`, so a display change never needs mirroring. **Excel keeps the cell a number and puts everything else in the code**: an aside becomes a column carrying its own segment (`(n={n})`), and every literal a template writes — the stars, the brackets, a sigma, a test label — folds into the numFmt, per section. A multiplicative cell holds its **reading value**, the signed fold, so `1/2.11` reaches the workbook without becoming text; text stays a property of a *cell*, not of a column. The exports' **unit row** is the console's own type tag (`<row%>`, `<n>`), written once per **block** — `tab_col_block_ids()`, the one definition of a block, which also decides where a vertical rule falls. Colour is single-sourced too — every backend reads `fmt_color_channels`. HTML colour is a slot **class**, never inline hex, the theme living in a `<style>` block from the one CSS generator (`tab-css.R`), so light/dark and the publication palettes work by stylesheet — except `print_marks`, whose signal is cell text and so comes from `format()` like the stars. `tab-transpose-render.R` flips a finished render model (a transposed column is heterogeneous and cannot be an `fmt` column), and `tab-theme-detect.R` best-effort-detects the console's scheme — a subsystem that must never error, because a wrong guess only mis-tints.

The **hover tooltip** (`tab-tooltip.R`) is that same rule read line by line: `TOOLTIP_LINES` declares one row per line — the token it renders, where its name comes from, which of the shared gates apply — and row order IS the reading order, so a line is named by its `DISPLAY_TOKENS$label`, exactly as the exports' unit row is, and one gate (non-empty · comparable · not the reference · not already shown · not already emitted) decides every one of them. It has **two rows**, declared the same way (`group`): the cell's own numbers, then the observed comparison — `obs` and the gap to it, a statement about another column — joined by a newline the stylesheet honours. It is **not translated**, deliberately: like the pillar type tags its words are the `fmt` field names, so the hover teaches the fields a user reads with `$`.

### jamovi

Two point-and-click analyses mirror the two producers: `jmvtab` (Crosstables) and `jmvtabreg` (Regressions). Each is a thin `R6` backend (`*.b.R`) over an engine-free build core (`jmvtab_build()` / `jmvtab_reg_build()`) driving `tab()` / `tab_reg()` through a content-addressed **live-UI cache** (`*-cache.R`), so an interactive tweak re-paints instead of recomputing. Each option is named after the argument it drives, so the backend is a pass-through, not a translation table. The regression store holds **distilled fit records** (kilobytes) keyed on the model alone, so every estimand change is a hit and nothing heavy crosses jamovi's `$state`. The generated `*.h.R` option headers are never hand-edited.

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


#### Phase 22g-iii — jamovi UIs manual review

##### `jmvtab`, or shared UI elements
- xl_check exports button to add tab_check_plots: I want a simple tick box, with displayed text "assumption checks plots". I want to place it after "Format" (at its right) in the first exports UI row.
- I want jamovi UIs html results and exports to be able to use two palettes/themes: "light" (default) or "print_ready". It just needs a `theme=` radio button in "Other formatting" main collapsable box (changing it should just redo the html rendering). For that I need a bit of rearranging: put `add_pct` at the right of `totaltab` (keeping the current layout so that )
- In jmvtab, the current texts of the `display` droplist are heterogeneous: I want the actual name of the display preset, to teach the jamovi user the R argument at the same time, then a very brief parenthesis if the name is not already clear by itself. The maximum number of character should be lower than today (both english and French), to let more horizontal space to the color color_signif layout column, where long text is needed.
- `jmvtab` `test` argument is a bit misleading as a way to teach R, because the "test = " label shows the two buttons behind it.
- The hidden jamovi results takes a lot of vertical space below the tables, even with height is 1 px: would there be a way to make them really hidden, taking no vertical space at all or an unseeable amount ?

"Levels and missing values" main panel, "Reorder and merge levels" UI/box
- Globally it’s very good to use it and it only needs a little polish. The main problem is that the width of the box changes at nearly each click: it changes when I expand a variable (wider) ; it changes when I click on a "merge" tick box (even wider ; the "level" column strangely widens too). I would want these widths to be fixed, and the box to always occupy all the available horizontal space. Each column, "level", "merge", "merge name", should have a fixed size and don’t change on clicks. These three columns names themselves are formatted in a grey color that does not fit the rest of the UI: use pure black for the three ones. There is a very thin vertical border between "merge" and "merge name" column, that looks bad and should be removed.
  + Replace "reorder / merge" string with "click to reorder / merge"
  + Grey-out the up/down arrow to reorder if the variable have class "ordered", but keep the possible merge (it is meaningful to merge contiguous ordinal levels).
- numeric variables currently have an arrow to expand their box but nothing in the box. This is exactly where we will put the `shape =` new argument of `tab()`: the collapsable element ilself is not needed, because the `shape =` UI only need one drop list that can fit on the current row (that same row where the variable name is already). Currently it’s written "— numeric (no levels)" : use ": numeric" only, then the `shape =` drop list. In the drop list `row_vars` and `tab_vars` should only be able to get the `sb_bands` or quantiles, that turn them to factor (no collapsed box needed, the "how many levels" can already be made with quantiles; just propose `sd_bands` default, then `quartiles`, `quintiles`, `deciles`, and whatever name is used for 2 levels and three levels, in the relevant order). In the `shape =` drop list `col_vars` can keep all the candidates that `tab()` accepts, returning either a factor or a reshaped numeric variable: the default should of course stay "linear", then the numeric to numeric reshapes ("quadratic", etc), then the same numeric to factors ones than for `row_vars` and `tab_vars`.
- Rename to "Reorder, merge and cut levels"
- Share with `jmvtabreg`

"References (points of comparison)"
- Currently, if I merge two factors levels, their name is still listed here (but it works: if I select either, the merged level is used as reference). Would there be a way to actually use the merged names/new names, and modify the content of the droplist live for that ? And add numeric variables cuts to factors by `shape =` in live ?
- numeric variables are also listed here, with useless text "numeric — compared with its Total" (remove), because the default to cut them into `sb_bands` is not yet taken into account. There should be two behaviours in `jmvtab` : 1. if the numeric variable was cut to factors levels, it’s meaningful to choose their reference level ; 2. if they are still numeric, they should not appear here since there are no references to choose in `tab()`.
- Share with `jmvtabreg`. In `jmvtabreg`, numeric variables have a `ref =` argument (they can be centered on their mean, etc.), so this choice must appear here, in the References UI.

"Confidence intervals" main panel
- `conf_level` and `stars` should be in two columns of equal widths for visual structure.
- Remove the "Confidence-interval methods (advanced)" collapsed box, and put it’s content directly in the "Confidence intervals" main panel, after an empty line and a simple label (bold like always) saying "Confidence-interval methods (advanced)". Do not re-add options title in `jmvtab.a.yaml` (it would only duplicate the options labels already in `jmvtab.u.yaml`). Like `conf_level` and `stars`, there should only be two columns of equal widths for visual structure. 


**Check that no new arguments and behaviours have broken the cache logic**, or created wrong tables by using outdated cache elements.



##### `jmvtabreg` specifics
"Model" main panel
- the `family` + `link` + `trials` selector needs to be visually improved: the column names formatting (grey, small, plain fontface) is inconsistent with the other ones (black, same size, bold). To identify this selector better, I want it inside a same box (non-collapsable) with a backgsound grey color different from the overall grey background in a material design way, look at `jmvtab` reorder and merge levels UI for an example of that (with collapsable elements we don’t need here).
- `measure` auto grey-out depending on models `family` is good, but have one flaw: with only one "binomial" model "log (un-exponentiated coefficient)" is available, but if I add a "gaussian" second outcome is becomes greyed-out. The user-friendly behaviour would be: "log" takes the un-exponentiated coefficient for OR and ratio links, but the raw coefficients that are already a "diff" for "identity" link ? Maybe rename the option in `tab_reg()` also, "raw_coeff" or "coeff" or something like that (jamovi radio-button text: "coeff ()" ; have you got better names ?) ?
- `measure` and `link` options order should be from simple to complex (same than `tab` `color =`), in `jmvtabreg()` and `tab_reg()` alike: "auto", "difference", "ratio", "odds_ratio", "log".
- `empirical` show `no`, `cell` and `column`, three options not often used in R. I want a simple TRUE/FALSE checkbox here (the only case that jamovi won’t be able to handle is then multinomial with `column`, we don’t really care it’s experts territory).
- Promote `display` to the "Model" main collapsable box (like it is in jmvtab) and make it a droplist for compactness : it should take the place of the current `empirical` , with the new `empirical` checkbox at its right on the same layout row. The two columns must be aligned with the two columns of the measure/effect layout row above them for visual consistency and structure.
- Promote `color` here too, on the last row, so that it comes after empirical, it’s main use is to choose the `adjustment`.

"Model comparison" main panel
- remove the "stats =" argument altogether from jamovi UI (the default is already good, only having "all" adds much noise, and one-by-one multi-selection would be too costly ; R only). Even remove `stats = "compare_sequential"` and `stats = "compare_baseline"` (a further phase will put baseline as the default, and auto do sequential LR when the conditions of each previous model being nested in the next one are met). It will move `na` UI at the right of the same layout row.
- "Interactions" main panel is currently a bit broken: too much horizontal space taken, so if there is a button to remove an interaction its currently unreachable, and the interaction stays forever; the menu permits to choose "race*race" useless self-interaction. It’s difficult to use, "hidden" alone in it’s own collapsable box, and impossible to build a predictor’s list with models with and without the interaction. I want to **integrate the interactions panel inside the Model comparison’s selector**: when used with only one list, it uses the live UI too ; it works with `tab_vars` ; **the only thing not working is several outcomes**, which currently errors even when there is only one predictor’s subset, and **should be made to work in that specific case** (only erroring at two predictor’s subsets, when the UI actually goes Run comparison button instead of live).
  + A design idea to test the feasability of and to build from: add an "Interaction" button after each different list of predictors ; when the user click on it, create a new row of selectors below, for him to select the two variables to interact, then confirm it with another button on this row; clicking the button remove the row, remove the two interacted variables from this particular subset of predictors, and add the chosen interaction, like "age*race", in a specific little rounded box with lighter color, no tick box at its left, but a cross at it’s right to remove it ; removing it by clicking on the cross remove the interaction, and make the individual terms reappear in the list. Would it be feasible reliably ? User-friendly ? Be ready to discard this feature if human UI tests reveal it can’t be done reliably (it that case, we won’t add interactions at all in `jmvtabreg`).
  + I also found a bug to fix: if I do model comparisons with several predictor’s lists, and then remove all the predictor’s lists (not even keeping one list), the UI goes live again, but adding a second outcome gives me a "A model comparison needs the models to share one `outcome`" error (not user-friendly: the error should state that only one outcome is possible here, and say to keep only one, or something like that). So something in the model comparison mode stays in setup, where the whole UI should go back to the full no model comparisons mode. Even one row of "predictor’s list" should work with several outcomes, only erroring when the second row is added (when the UI goes Run comparison button instead of live).

"References, level merging and predictor scaling" main panel: I should become two different main panels (collapsed boxes), "Reorder and merge levels", then "References (points of comparison)" 
- The jmvtabreg UI is visually convoluted. I want to use the two exact same UI/box than `jmvtab` here.
- The "Reorder and merge levels" UI/box should be the same than in `jmvtab`, with the same improvements (see above).
- The references selector UI should also be the same than in `jmvtab`, but with a few added features: numeric variables not reshaped to factors should continue to appear here and have their own row, since they have a `ref =` argument, can be centered on their mean, etc. The `multiplier =` should also appear here. (Not `shape =`, that it on "Reorder and merge levels")

"Display" main panel: rename it to "Other formatting" like in `jmvtab`
- Try to match the jmvtab layout a bit : `wrap_rows =` and `wrap_cols =` in the first layout row ; `n =` and `cleannames =` in the second layout row ; all with well aligned columns for visual structure and consistency.


**Check that no new regression arguments and behaviours have broken the cache logic**, or created wrong tables by using outdated cache elements.







#### Phase 22h — default-options-choice oriented full performance review — DONE

**The whole review lives in `dev/tabxplor_2.0.0_performance_review.md`** (590 lines, every number generated, none typed): the verdict on each default, where the time goes suite by suite, the 1.3.1 → 2.0.0 A/B, the student-machine projection, and the five open decisions. What follows is only what a future session needs to know about how it was produced and what it found.

**Three reusable scripts, all `.Rbuildignore`'d.** `dev/benchmarks/phase22h_perf_review.R` MEASURES — 146 cases in nine suites (the plain table by size · the marginal cost of one option · the table-of-tables serial vs parallel · exports · regressions · the jamovi live UI · the 8M standard grid · the cold session · the 1.3.1 pairs), **~4 minutes per machine profile**, `--suites=` / `--tag=` / `--cores=` / `--dt_threads=` / `--blas_threads=` / `--engine=v131`. `phase22h_report_tables.R` READS the CSVs and emits every table in the report. `phase22h_threads.R` answers the one question that needs a different experimental design (below). Runs are under `dev/benchmarks/results_2.0.0/phase22h_*.csv`; CRAN's 1.3.1 sits in its own library at `~/R/tabxplor131`, HEAD installed at `~/R/tabxplorhead` for the cold-session suite.

**The headline: the crosstab defaults are already right, and `n` is not what a table costs.** Ten times the rows (21 483 → 214 830) moves a default table from 0.040 s to 0.041 s; it takes 8 M rows before `n` appears at all (0.117 s). tabxplor's cost is **O(cells), not O(rows)** — which is why the marginal-cost matrix barely moves between fixtures, why parallelism pays (it splits *tables*), and why a student and a survey researcher wait about the same. The one 2.0.0 default that could plausibly have cost something — `ci = "auto"` resolving to a reference interval because `stars = TRUE`, where 1.3.1 defaulted `ci = "no"` — measures **+0.000 s**. `test = TRUE` is the only expensive crosstab option (×1.85) and is already off; `design_effect` is +2 %; `n` / `digits` / `cleannames` / `na` / `comp` / `color` are free.

**1.3.1 → 2.0.0 on the same data, same tables: ×1.0 to ×5.0, and ×2.2–2.7 on the exploratory workflow.** Fifteen tables merged 1.87 s → 0.84 s; a table with test and colour ×2.6–3.2; **weighted percentages at survey n ×5.0** (0.243 → 0.049 s). 1.3.1's cost grows with `n` where 2.0.0's does not (weighted col%, 21k → 215k: ×3.2 against ×1.06), so the gap widens with the survey. And 2.0.0's *default* call is faster than 1.3.1's `ci`-less one while computing an interval 1.3.1 never computed. The A/B feeds both engines byte-identical frames from the harness's own `fx_gss()` (plain dplyr/forcats — 1.3.1 has no `gss_cat_data_formatting()`), with disjoint row/column sets because 1.3.1's `tab_many(compact = FALSE)` aborts on a variable that is both.

**A real defect found and FIXED — `lvl_check_reserved()` stringified every numeric column, on every build.** `R/row-model.R`'s fallback was `unique(as.character(x))`; the pipeline's call site restricts it to `vars_not_numeric` but **the leaf's own (`R/tab-leaf.R:165`) passes its numeric col_vars too**, so a continuous column was coerced to character and hashed each time. Profiling put **67 % of an 8M numeric table on that one line**. A number can never carry a level named "Total", so the line now reads `if (is.factor(x)) levels(x) else if (is.character(x)) unique(x) else next`. Measured on the 8M fixture: one numeric col_var **8.57 s → 1.23 s**, two with `comp = "all"` **21.00 s → 0.31 s (×67)**, `tab_num(region, score + income, response)` **21.00 s → 0.50 s (×42)**. The character path still aborts on "Total" (verified). It went unseen because it only bites at a scale nobody runs interactively, `run_bench.R` had not been re-run since 8 July, and the in-suite benchmark is opt-in — a standing 8M numeric case now guards it.

**The one default set worth revisiting is `tab_reg(stats =)` on a multinomial.** A multinomial table is **6.6 s** (13-category outcome) / 2.9 s (8-category) — 20–40× a binomial, ~5× the underlying `nnet::multinom` fit — and the default footer checks are **~37 %** of it. `cost = "free"` in `REG_CHECKS` means "no refit", and for `dispersion` / `influence` that is true but not cheap: a (K−1)-block Hessian (0.81 s at 13 categories) plus an O(n·p) influence sweep with p = (K−1)×terms. On a binomial the same checks are +16 %. Three cures are laid out in the report; the recommendation is a **per-family `footer_default`**, which the fact table's existing `families` slot makes a declarative one-row change. Also measured: the category count is the driver (13 → 8 categories more than halves everything), which is one sentence for the regression vignette.

⚠ **Two measurement traps were tripped and are recorded because they produced WRONG conclusions before being caught.** (1) A **median of two runs is a mean**, so one cold run halves into the headline — the console-print case read 0.29 s instead of 0.087 s, and `tab_xl` 0.59 s instead of 0.37 s. The harness now takes at least three measured reps below its `slow_at` threshold and warms every render path, and `min_s` is the headline. (2) The **first operation of a kind in a process can cost 5–10× the second**: the default confidence interval briefly looked ×17 on 8M, and an OpenBLAS effect looked ×10 when it is ×1.2. Every case discards a warm-up.

⚠ **The data.table threading question needed its own design, and the naive ones gave the opposite answer.** `setDTthreads()` tears down and rebuilds the OpenMP pool, so flipping it inside a timing loop measures the flip (that reported the 8M numeric case ×2.2 slower with threads); and two whole harness runs are not a clean A/B either, because a late suite runs in a different heap and pool history (×2.8 slower, same case). `phase22h_threads.R` — **one thread setting per process**, all cases warmed, min of five, conditions alternated — gives the real answer: **threads help big factor tables (×1.14–1.59 at 8M) and hurt everything else (×1.08–1.29 on 21k tables, ×1.29 on the fifteen-table exploration)**, and the 8M numeric path is simply unstable. So there is no free win, only a trade; the package already pins `setDTthreads(1L)` in its mirai daemons (`R/tab-parallel.R:222`) where threads have nothing to win. Left for a later phase, size-gated if ever taken.

**Parallelism, measured on both profiles.** 12 cores: ×2.9 at 8 workers, ×2.6 already at 4. 2 cores: ×1.7 at 2 workers, then **backwards** — 8 workers on 2 cores is slower than 4 and no better than serial on the bigger fixture. `tabxplor.parallel = FALSE` is the right default and `TRUE` (physical cores − 1, capped 8) is safe from 4 cores up.

**The student machine (HP ProOne 400 G5 class: i3-9100T / i5-9500T, 8 GB, 2019).** Pinning this box to one core changed nothing (median ratio **1.00×** over 138 cases) — the serial path is genuinely single-threaded — so the projection is essentially the single-thread ratio, PassMark 3 233 vs ~2 110 = **×1.5** (×2 for the memory-bound millions-of-rows cases, single-channel DDR4-2666). Projected: a cross-table ~0.06 s, the fifteen-table exploration ~1.2 s, a jamovi interaction ~0.3–0.9 s, a binomial regression ~0.25 s, cold start to first table ~0.9 s — **everything under two seconds except a multinomial (~10 s) and `tab_plot` (~3.7 s)**. Three things make them better off than a naive scaling (Windows R ships the single-threaded reference BLAS, so §6.4's overhead does not exist there; data.table on 4 cores defaults to 2 threads; `parallel = TRUE` resolves to 3 workers) and one worse: **8 GB of RAM is the real constraint**, since each parallel worker copies the prepared population — another argument for the option staying off.

**Recorded, no action:** printing a table to the console costs **0.087 s, about twice building it** (0.040 s) — rendering, not computing, is what an interactive user waits for; `tab_plot` 2.49 s is the most expensive single call in the review (ggplot2); `tab_xl` has a ~0.37 s openxlsx2 fixed cost; html tooltips are +32 % of an export; `effect = "at_reference"` is ×2.7 `marginal` (it leaves for `marginaleffects`); the ordinal Brant refit **doubles** an ordinal table and is the one `cost = "refit"` check on by default; `empirical = TRUE` is only +33 % and the sparkline is free; at 215k rows a regression is ~1 s and `glm()` is most of it. On an OpenBLAS-pthread build BLAS threading costs `tab_reg()` ×1.2 in steady state — an environment property, absent on the students' Windows R.

**Open decisions (all in the report's §8, none blocking):** should `design_effect` default to TRUE on weighted tables now that it is known to be free · should `dispersion`/`influence` leave the multinomial footer · is the ordinal Brant refit right at survey `n` · the vignette sentence on collapsing a multinomial outcome · the size-gated `setDTthreads`.

**Verification:** full suite **FAIL 1 | PASS 9787**. The one failure is `test-jamovi-vocabulary.R:288` ("STALE generated block") and is **not from this phase** — it is the maintainer's own in-flight Phase 22g-ii hand-edit of the generated block in `jamovi/js/jmvtabreg.js` (shortened `TABX_FAMILY_LABEL` / `TABX_LINK_LABEL` strings) against what `REG_FAMILIES` / `reg_link_ui_labels()` would emit; it needs either the R fact tables changed or `dev/generate_jamovi_js.R` re-run, and touching it would undo those edits.


**FOLLOW-UP — should `parallel` be on by default, and should jamovi use it?** Measured by a fourth script, `dev/benchmarks/phase22h_parallel.R` (one worker count per PROCESS — `tab_pool_ensure()` respawns whenever the count differs, so sweeping W in one session charges every W after the first with a teardown). Full argument in the review's §7 and §8; the decisions are in its §10.

⚠ **A blocking defect had to be fixed first: a worker did not pin its BLAS.** `tab_pmap()`'s `everywhere()` set `data.table::setDTthreads(1L)` in each daemon and said nothing about BLAS — so on OpenBLAS-pthread (Debian/Ubuntu's default) the first `glm()` in a worker opened one thread PER CORE: W workers × C cores of spinning threads on C cores. `tab()` never noticed (its units are data.table-bound, and data.table WAS pinned); `tab_reg()`'s units are glm-bound. **Measured, 3 outcomes × 3 workers, 12 cores: serial 0.81 s, parallel 56.91 s unpinned, parallel 0.29 s pinned — a 70× slowdown**, reproducible, identical at every combination of `stats` / `empirical`, and independent of data size (the tell that it was contention, not work). Fixed in `R/tab-parallel.R`, guarded on `RhpcBLASctl` (already a Suggest); ⚠ the RUNTIME call is the only lever, since OpenBLAS-pthread fixes its count from the environment at process start. **`test-parallel-parity.R` passes 26/26 before AND after** — it asserts byte-identity, which a thread-thrashed worker still delivers, and that is exactly why nothing caught this.

**What parallelism is worth** (pool warm, 21k rows, tables × workers): it pays from 2 tables (×1.4–1.6), reaches ×1.7 by 4 tables at 2 workers, and **4 workers is the knee** (×2.4–2.8 from 4 tables up). 8 workers buys +38 % over 4 at 24 tables and nothing below 8 tables. **On 4 cores, 4 workers gives ×2.8 against ×1.7 for 2**; on 2 cores everything from 2 workers up is flat at ×1.75.

⚠ **The first call is the problem, and it cannot be engineered away.** `mirai::daemons()` blocks until the daemons have connected (1.16 s with dispatcher, 0.78 s without) — there is no fire-and-forget variant, so pre-warming can only be MOVED, not removed. Cold spawn is 0.87–1.98 s, so the first parallel table is always slower than serial (8 tables: 0.962 s serial vs 1.83 s parallel-including-spawn); break-even is ~2.5 calls for an 8-table build and ~17 for a 2-table one. Plus **~133 MB per idle worker** (8 workers = 1.6 GB of R before any data). Hence: **do not spawn at `.onLoad()`** — CRAN policy forbids starting processes on load, and it would tax every `library(tabxplor)` including in knitr, `R CMD check` and jamovi.

⚠ **The core-count rule is wrong today, independently of any default change.** `parallel::detectCores(logical = FALSE)` returns **12 under `taskset -c 0,1`** and **12 under `_R_CHECK_LIMIT_CORES_`**, where `parallelly::availableCores()` returns 2 for both. So `parallel = TRUE` over-spawns on every cgroup-limited container, HPC allocation and CI runner (R CMD check itself is covered — `tab_parallel_workers()` reads `_R_CHECK_LIMIT_CORES_` and caps at 2). `availableCores()` also honours `options(mc.cores)` and the SLURM/PBS/SGE/LSF variables, which is the cheapest way to satisfy "respect the user's own parallel settings".

**Recommendation: fix, then decide.** Ship the BLAS pin and the `availableCores()` swap (both are wrong today, not merely improvable); keep `FALSE` as the shipped default; add `tab_parallel_start()` as the twin of the existing `tab_parallel_stop()`, and consider `"auto"` meaning *use a pool if one is already running, never spawn one* — which can never make a first call slower and is CRAN-safe by construction. If a spawning default is wanted, raise `parallel_min` from 2 to 4. For the worker count the maintainer's own rule holds, with one change: `min(4, max(2, availableCores() %/% 2))` — the floor of 2 matters because `%/% 2` gives 1 on a 2-core machine, which is serial, yet 2 cores is exactly where 2 workers give ×1.75 with no penalty.

**jamovi — the maintainer's intuition was right for one panel and half right for the other.** `tab()` maps over **`row_vars` and only `row_vars`**, so in `jmvtab` several `tab_vars` make one unit twice as big (0.095 → 0.182 s) and add NO units (×1.01), while 3 row_vars give ×1.9. `tab_reg()` has **three** axes and all pass through the same seam: **several outcomes ×2.65** (and it holds at survey n: 9.03 s → 3.60 s), **model specs ×1.64**, and — measured — **the `tab_vars` groups axis gives no gain at all** at either size, despite dispatching. The always-serial rule is **not** structurally required by the cache: `tab_rowvar_ctxs()` already strips `cache_env` from every unit and `jmv_cache_store_tests()` runs on main after collection, from the map's own payload. What it really buys is not spawning R processes inside jamovi's Electron-managed session. And the cache does NOT make the map free: in a warm `jmvtab` interaction `tab_build_tables` is **88.7 %** of the build (`tab_transform`, the part a worker would run, **72.8 %**) while the cached tier-1 aggregate is **0.5 %** — the cache working exactly as designed. Advice: leave Crosstables serial (the realistic win is ~0.15 s and the render is the other half of the interaction); **build it for Regressions instead**, as an explicit "use several cores" checkbox defaulting OFF, since that panel's interactions are seconds and several outcomes is its best case. ⚠ Whichever is done, the BLAS fix must ship first — a regression panel spawning workers with unpinned BLAS would look like jamovi hanging.


**FOLLOW-UP 2 — the parallel fixes IMPLEMENTED, and the jamovi answer.** Parallelism **stays opt-in** (`tabxplor.parallel` is still `FALSE`); what shipped is the BLAS pin, an affinity-aware core count, and a worker rule worth saying yes to. Detail in the review's §7.5 and §8.

**`tab_available_cores()` — a cascade, one rung per case `detectCores()` gets wrong**: `_R_CHECK_LIMIT_CORES_` (CRAN's 2-core rule) → `options(mc.cores)` (base R's own convention) → `parallelly::availableCores()` (cgroups v1/v2, affinity masks, SLURM/PBS/SGE/LSF) → `nproc` (affinity-aware on Unix) → `detectCores()`. Only the MACHINE rungs are memoised; the two option rungs are re-read every call. ⚠ **`parallelly` had to become a Suggest — mirai could not supply this**: it imports `nanonext` alone and exports no core count at all (`mirai::info()` returns NULL with no daemons), so gating on mirai would have bought nothing. Zero-dependency package, and the fallback rungs mean nothing breaks without it.

**`tab_auto_workers()` — `"auto"` / `TRUE` = `if (avail <= 1) 1 else min(4, max(2, avail %/% 2))`**: 1 core → serial, 2 → 2, 4 → **2**, 6 → 3, 8+ → 4. Every clause is measured: the cap at 4 (8 workers = +38 % over 4 at 24 tables, nothing below 8 tables, for 4 processes and ~530 MB); half the cores (a build must not saturate its machine — it costs ×2.8 → ×1.7 on 4 cores and buys a usable UI); the floor of 2 (`%/% 2` gives 1 on a dual-core box, which is serial, and 2 cores is exactly where 2 workers give ×1.75 with no penalty); 1 core stays serial. Verified end to end: `taskset -c 0,1` → 2 workers, `taskset -c 0` → serial, `_R_CHECK_LIMIT_CORES_` → 2 even when 8 was asked, `mc.cores = 2` → 2, `"3"` (jamovi passes strings) → 3, a jmvtab cache → serial regardless. `?tabxplor-options` rewritten accordingly; the one line to teach is `options(tabxplor.parallel = "auto")`.

⚠ **Pinning the WORKERS alone broke the byte-identity contract, and that is the second half of the fix.** A worker at 1 BLAS thread and a main process at 12 disagree in the last bits of every coefficient. Measured on a 2-outcome `tab_reg()`: main 12 / workers 1 -> **parallel != serial**; main 1 / workers 1 -> identical; and — the row that explains it, true BEFORE any of this — a **serial** build at 12 vs at 1 thread already differed. `glm()` through a threaded BLAS is not thread-count invariant, so "byte-identical" only ever held because both branches happened to use the same count. `local_blas_threads(1L)` now pins for the duration of a build, **in `tab_pmap()` for BOTH branches** plus the one serial unit-loop that bypasses it (`reg_stage_specs()`), restoring the user's setting through a base `on.exit` in the caller's frame (⚠ `withr` is Suggests-only, so it cannot be used in package code). All three comparisons are now TRUE — **a `tab_reg()` result no longer depends on how the machine's BLAS was built**, which is strictly better than before and is what the suite already assumed (`setup.R` pins BLAS there).

**Tests:** `test-options.R` gains the worker rule (pure arithmetic, so it runs with or without mirai) and the option boundary; `test-parallel-parity.R` gains the **BLAS-pin regression guard** and a second one that **unpins the caller's BLAS to what a real session has** — the guard the other parity tests structurally cannot be, since setup.R pins BLAS suite-wide and a worker-only pin therefore looks fine there. ⚠ It had to be written with `mirai_map()` over more tasks than workers — `everywhere()` runs for side effects and returns no values — and ⚠ byte-identity was never the missing check: a thread-thrashed worker still delivers it, which is why 26/26 passed throughout the 70× regression. **FAIL 1 | PASS 9816** (the one failure is the maintainer's own stale `jmvtabreg.js` block, unrelated).

⚠ **jamovi: RECOMMENDED AGAINST, and the process model is why.** Read out of the installed flatpak, not inferred: `jamovi/server/session.py:109` builds `Scheduler(1, 3, …)`, so jamovi runs **exactly 4 `jamovi-engine` processes per session**, created once, **shared by every analysis of every module**; and `enginemanager.py::_run_analysis` sends a request to the slot where that same analysis is *currently running*, else to the **first free slot** — **no persistent affinity**. Consequences: a pool spawned in engine 1 is invisible to 2-4 and the next pass may pay the spawn again (steady state: spawned in all four); a pool tabxplor leaves behind is **inherited by other modules' analyses**; and the lifecycle cannot be managed from inside the module, because the pass that "leaves comparison mode" may run in a different engine from the one holding the pool. Memory: 4 engines × W × ~133 MB = **1.06 GB at W=2, 2.1 GB at W=4**. ⚠ **And the arithmetic kills it first**: 4 predictor lists cost **0.608 s** serial (0.421 s cached, ~0.367 s at W=2) against a **0.870 s cold spawn** — the spawn costs more than the entire build, so spawning per run is a 5× net loss and a kept pool needs ~7 comparison runs *per engine* (~28 per session) to break even. The case that motivated it is no longer slow.

**The two jamovi caches are different objects with opposite verdicts** (measured by re-running each interaction with the store withheld). ⚠ First a correction: "the tier-1 aggregate is 0.5 % of the build" is the cost of a **hit**, not the value of the cache. **Crosstabs: KEEP** — 0.08-0.19 MB buys **×18-34** on a re-applied change (0.006 s vs 0.107 s; 0.010 s vs 0.340 s), and the rebuild path it does not help costs 2-5 %, so nothing is lost by its presence. **Regressions: CUT** — ×2.3 at best on a re-apply (0.095 vs 0.196 s) and **×1.1 on a changed option** (0.282 vs 0.314 s), paid for with **6.28 MB (1 model) to 15.89 MB (4 predictor lists) serialised into jamovi's `$state` on every UI round-trip** — exactly the freeze `jmvtabreg-cache.R`'s own comment describes. Comparison mode already sets `use_cache = FALSE`; these numbers say why. The split is not taste: it is the ratio of what the store HOLDS (KB of aggregates vs MB of raw fits) to what it SAVES. Proposed: keep the regression cache only for the reference-invariant KB digest and stop persisting raw fits — or remove it, since ×1.1 on a changed option is close to free to lose. Its own phase.


#### Phase 22i — documentation reviews

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


#### Phase 22j — the fit digest: cache what a marginal effect needs, not the fit — DONE

**A fitted model is no longer the unit of anything.** `reg_fit()` now returns a **`tabxplor_fitdigest`** beside the fit (`R/reg-digest.R`), and every engine — the g-computation makers, the probability engine, the influence functions, the column builders — reads the digest. The jamovi store holds **one tier** of distilled records, keyed on the MODEL alone: a distilled record is **3.3 KB** and a whole store **29.3 KB** (binomial, 3 predictors) or **92.4 KB** (multinomial), against the **6.28 MB / 15.89 MB** Phase 22h measured. The headline win is where it hurt most — a `measure` / `effect` change on a multinomial, **14.35 s → 1.90 s (×7.6)**, because the estimand left the cache key.

**Three things jamovi silently lost, and now does not.** On the old narrow digest path `reg_checks_for(has_fit = FALSE)` returned `character(0)`, so a single-model GLM table quietly showed **no model checks**; `reg_global_rows()` and `reg_gap_se_columns()` refused the same way, so the **per-predictor global tests** and the **`color = "adjustment"` gap SE** vanished too. The cure is the **eager stage** (`reg_fit_eager()`, `R/reg-spec-build.R`): everything only a fitted object can compute — the model-fit statistics, the global tests, the assumption checks, each crossed pair's test — is computed *while it lives* and rides on the record. All four are estimand-invariant, so they survive a `measure` change; `stats` joins the key because it decides which of them exist. The `stats_checks` tick-box no longer has to turn the cache off, and `reg_checks_for()`'s `has_fit` argument is deleted.

**The reparametrisation engine is gone, and with it the codebase's one declared wrong-number site.** `reg_build_digest()` / `reg_reref_fit_res()` (~100 lines) and the **13-clause `reref` gate** whose own comment read *"THIS IS THE ONE CLAUSE WHERE A WRONG `TRUE` IS A WRONG NUMBER, NOT AN ERROR"* are deleted, along with the `reref` / `data_canon` ctx fields, `reg_build()`'s `ref` / `reref` arguments and the deferred relevel in `reg_resolve_fit_plan()`'s block U. A reference change is now an honest refit (**0.295 s** against 0.203 s for a hit) — and it is in the key **for free**, because the relevel happens before the fit and `jmv_col_fp()` fingerprints a column's levels. What is left in `reg_fit_cacheable()` is two clauses, and a wrong TRUE there costs a footer row, never a number: `method = "profile"` (its bounds are a likelihood output at one confidence level, not rebuildable from `estimate` / `std.error`) and a model comparison (a test BETWEEN the fit objects).

**The two declared tables, and the extension rule.** `REG_FIT_KINDS` is one row per fitting backend (`lm` / `glm` / `svyglm` / `multinom` / `svy_vglm` / `polr` / `svyolr`) declaring its classes in dispatch order, its `equations` shape, its influence `score` engine (`NA` = none, so the gap test is refused rather than approximated — `svy_vglm`'s standing hole, now stated as a fact) and the extra parts it carries. `REG_DIGEST_PARTS` is one row per stored part, each naming its consumer. `reg_digest()` is a loop over them, so **a new model backend is one row**; `TAB_FOREIGN_KEYS` gained three edges, so a row naming a key that does not exist fails the install.

**Nothing length-`n` is stored, and that took two discoveries.** The frame is rebuilt: `reg_fit()`'s prep is factored into `reg_fit_frame()`, which both the fitter and `reg_digest_frame()` call, so they cannot drift — and a rebuild whose row count is not the fit's is **refused**, not used. And the IRLS working weights and residuals are **reconstructed** from `(terms, coef, family)` against that frame, which is what carries `color = "adjustment"` onto the digest: `reg_coef_if_maker()`, `reg_score_multinom()` and `reg_score_polr()` now take `(model, frame)` and have no fit-based branch. ⚠ `r = (y - mu)/mu.eta(eta)` and `eta` come back **exact**; `W` differs by **~1e-8 to 2e-6 relative**, because `glm.fit` stores the weights of the *previous* IRLS iteration — a lag by construction, not a tolerance to tighten. Four assertions moved (`test-adjustment-gap.R` 1e-10 → 1e-5, `test-reg-checks.R` 1e-8 → 1e-6), each with the reason written next to it.

⚠ **A family object is not small: a fitted glm's serialises at 9.85 MB where a fresh `binomial("logit")` is 15 KB** — the closures carry the environment they were built in, and storing one would have defeated the whole phase (the first store came out empty, silently, because every record blew the tier ceiling). A family is fully determined by its NAME and its LINK, so that pair is stored and `stats` rebuilds it; anything no stats generator reproduces is kept verbatim rather than approximated. Same class of problem, same fix, one level up: `reg_digest_terms()` rebases the terms object's `.Environment` to `baseenv()`, because `reg_svyglm_env()` deliberately binds `survey::svyglm` into it.

**Three defects found on the way, all reproduced and fixed.**

1. ⚠ **A class dispatch a digest cannot satisfy is a WRONG ANSWER, not an error.** `reg_marginal_gcomp()` chose its sweep with `inherits(fit, "multinom") || inherits(fit, "polr")`, which is FALSE for a digest — so a multinomial fell through to the single-equation maker, got `NULL` (no family), and dropped to the `marginaleffects` fallback with a refit. **Measured: 123 s against 14 s for a cold build.** The cure is the declared predicate `reg_model_categorical()` reading `REG_FIT_KINDS$equations`, and every such dispatch now goes through it.
2. **The crude refits were dropping their digest** (`reg-empirical.R` kept `list(fit, data)`), so a numeric predictor's crude influence leg was built with an empty recipe and lost its **design weights** — a 16× wrong gap SE on a calibrated design. `reg_digest_base_weights()` now also reads a tabxplor frame's own `.svy_weights` where no recipe names them.
3. ⚠ **`f$tidy <- NULL` REMOVES the name, and `$` then partial-matches `tidy_native`.** `reg_fit_distil()` uses `f[c(...)] <- list(NULL)` so the names survive with NULL values.
4. ⚠ **An `@export`ed S3 method on a generic the package does not IMPORT breaks the whole S3 registration loop.** The seven new `S3method(coef|vcov|terms|family|nobs|df.residual|formula, tabxplor_fitdigest)` directives named stats generics `NAMESPACE` had never imported, so registration aborted partway and every later directive — including `vec_cast.tabxplor_fmt.tabxplor_fmt` — was silently skipped. The symptom is nowhere near the cause: **19 failures in `test-export-prep.R` and `test-tab_reg-survey.R`, reading `Can't convert `fill` <tabxplor_fmt> to <tabxplor_fmt>`**, and only under `devtools::test()` (a single file loaded fine). The cure is one `@importFrom stats <generic>` per method.

**Smaller, all deliberate.** `reg_marginal()` gained a `refit` callback: `effect = "at_reference"` (and any other route that falls through to `marginaleffects`) buys its fit back through `reg_digest_revive()`, which goes through `reg_fit()` itself so a revived fit cannot differ — verified working from a cache hit. `reg_marginal_gcomp()` refuses a digest carrying a `poly()` / `ns()` basis, because the check that a marginal effect is not silently 0 needs `predict()`. `var_y` is computed unconditionally (its `!do_exp` clause was the last estimand leak onto the record). `JMVREG_CACHE_SCHEMA` → `8L`; `entry_bytes` 24 MB → 2 MB, `store_bytes` 96 MB → 32 MB.

**Tests.** New `test-reg-digest.R` (61 assertions): the S3 surface against the fit, the frame rebuilt `identical()` and refused when it does not reproduce, the influence function and both g-computation engines off a digest, distil → rehydrate at another confidence level, `reg_digest_revive()`, and the extension rule the two tables encode. `test-jmvtabreg-cache.R`'s reparametrisation block is replaced by its opposite — *a reference change is a miss and equals a direct `tab_reg()`*, *`measure` / `effect` / `display` / `colour` / `conf_level` are hits* — plus a store assertion (< 1 MB, no fit, no frame, checks intact). Full suite **FAIL 1 | PASS 9853**; the one failure is the maintainer's in-flight `jmvtabreg.js` block (`test-jamovi-vocabulary.R:288`), unrelated to this phase.

**Still open, deliberately:** the observed (`empirical = TRUE`) block is rebuilt on every interaction, and its univariable refits are now the largest thing left in a served build — **53 %** of a marginal binomial one and **49 %** of a multinomial one, though only 27 % (0.073 s) of an ordinary binomial one. Scoped as **Phase 22k**, with the measurements and the traps written down.


#### Phase 22k — the crude block's own cache: the last refits a served table still pays

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

**Verification.** `dev/benchmarks/phase22j_digest.R` already prints the served-build timings this phase must move; add an `empirical = TRUE` arm to it and record before/after under `dev/benchmarks/results_2.0.0/`. Parity is the real gate: `test-tab_reg-numeric-crude.R`, `test-survey-crude.R`, `test-adjustment-gap.R` and `test-z10-crude-families.R` all read the crude fits, and `test-jmvtabreg-cache.R` must gain the same pair of assertions the model path has — *a predictor-set change is a miss*, *an estimand change is a hit*.

**Prior art to read first:** CLAUDE.md > Phase 22j (the whole DONE summary), `R/reg-digest.R`'s header, and `dev/tabxplor_2.0.0_performance_review.md` §8.5.


#### Phase 22x — very last features before release

⚠ **Open, deferred from 22g-i (jamovi controls the API has but the UI does not).** Both were weighed and left out of the last review phase on purpose, and neither blocks the release.

- **A `shape` picker for `jmvtab`.** `tab(shape =)` decides how a numeric `row_var` becomes levels, and today jamovi never asks: a numeric row variable is silently auto-banded (`"auto"` -> `levels` when whole-valued and short, else `sd_bands`). A picker would mirror the Regressions panel's own, but the crosstab's natural home for it — the levels box — already hosts the reorder/merge tree, and the auto rule is defensible. Decide the layout before building it.
- **`spread_vars` for `jmvtab`.** Cheap as a 5th variable box in the supplier, and the condensed tables it makes are a headline use case. But it lands in the tier-3 cache **base** key (`JMV_TAB3_REAPPLIED` is a negative set, so any new option does) and it changes which axis the reference picker filters on — two things not to touch in a review phase.

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
