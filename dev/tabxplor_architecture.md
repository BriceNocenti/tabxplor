# tabxplor architecture

This is the internal **cross-subsystem** architecture reference: the big picture, each subsystem's role, and the invariants that span many files. Per-file design lives in the R file-header comments; the file index is CLAUDE.md's Repository Map; user-facing docs are the vignettes. The code is the source of truth when this drifts.

Read it top to bottom: it goes from goals, to how a table is built, to the type system and the declarative pattern that organise everything, to each subsystem, to the invariants a future change must not break.

## What tabxplor is, and why

tabxplor builds **colour-coded cross-tables and regression tables for data exploration**. The one idea behind everything: colour lets you *read a table at a glance*. Over-represented cells turn blue, under-represented ones red, deeper colour means a stronger deviation — and a cell is only coloured when the difference is statistically solid, so structure jumps out instead of being scanned number by number.

It sits at the intersection of three things most tools keep separate, and its architecture exists to unify them:

- a **display engine** — colour and typography that encode statistics (effect size *and* significance at once), the same visual language across console, HTML, Excel, Markdown/Quarto and plots;
- a **rich cell data-model** — every cell is a `vctrs` record carrying all the numbers behind the one it shows, so tables stay ordinary `dplyr`-manipulable tibbles and the display can switch losslessly;
- a **statistical-inference layer** — exact survey/design-effect variance (reproducing the `survey` package), named CI methods (Wilson, Newcombe, Katz, Woolf, Welch), Haberman adjusted residuals.

The target user is a serious quantitative analyst — a survey researcher, a sociologist — often working with **weighted or complex-survey data**. That is why the inference layer is unusually deep for an exploration tool, and why `tab_reg()` pairs a model's adjusted effect with its **observed (crude) counterpart**, so "what did controlling for the other variables actually change" is visible in one table.

What makes it different, in one list: colour encodes **effect size and significance together**; cells are **rich values**, not strings; **one visual language** follows the table to every output; **regression tables** get the same language plus the observed-vs-modelled comparison; and the statistics are **rigorous** (design-based variance, named CI methods).

Two design principles underpin the whole package:

1. **Every cell carries all its statistical data.** A numeric cell is a `tabxplor_fmt` record (count, weighted count, percentage, difference, ratio, contribution, CI bounds, odds ratio, p-value, …). Changing what is displayed never recomputes or loses anything.
2. **Tables are tibbles.** Results inherit from `tibble` (`tabxplor_tab` / `tabxplor_grouped_tab`), so every `dplyr` verb works while table metadata and formatting survive.

**Performance:** aggregation runs on `data.table` internally; the user only ever sees tibbles of `fmt` columns. **CRAN stability:** public signatures (argument names, defaults, return types) are a stable contract; internals may change freely, soft-deprecating public arguments rather than breaking them. **Dependencies are pay-as-you-go:** table building and core inference are always available (hard Imports include `data.table`, `broom`, and the stats engines `survey`/`nnet`/`MASS`); exporters, plotting, parallelism, jamovi and advanced regression backends are all Suggests, guarded at their entry points.

## The big picture: how a table is built

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

## The declarative architecture

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

## The type system

### tabxplor_fmt — the rich cell

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

### tabxplor_tab — the table

`tabxplor_tab` is a `tibble` subclass; `tabxplor_grouped_tab` extends `grouped_df` when `tab_vars` split the table into sub-tables. Class and metadata survive `dplyr` through ~30 S3 methods, anchored by the `dplyr_row_slice()` / `dplyr_col_modify()` / `dplyr_reconstruct()` trio (a missing method silently downgrades to a plain tibble). A table carries three **optional, NULL-safe** attributes: `subtext` (legend text), `test` (a tibble of chi²/ANOVA/model-footer rows), and `meta` (one list holding `spec`, the variable model, CI settings, render intent, and any regression/assumption records). Every getter tolerates absence: a table stripped of `test` still prints, dropping only the summary it powered — cell fields and column attributes stay required, a standalone extracted `fmt` column formats and colours on its own.

### The row model

Rows describe themselves the way columns do. The `row_kind` field (from `ROW_KINDS`: `data`/`total`/`n`/`pct`/`pvalue`/`gof`/`blank`) says what kind of row a cell sits in; `is_totrow()` is the derived read. The index columns are a `tabxplor_lvl` factor subclass carrying each level's `role` (level / variable / tab-variable) and originating `var`, so variable detection and rendering read stored facts rather than guessing from labels.

## The calculation pipeline

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

## The inference layer

**The survey-design boundary** (`survey-design.R`) is one unwrap point: a `survey` design passed as `data` becomes the microdata every engine already reads, plus its sampling weights and design metadata — so the crude columns, the AME, the tests and the footer are all design-weighted, and a `svyrepdesign`/`twophase` is refused rather than approximated.

**The inference basis** is the layer's central idea: how the *estimate* is computed (`wt`) and how the *interval and test* are computed (the basis) are **orthogonal**. The basis is one of `n` / `weights` / `design` / `design_partial`, and — with `conf_level`, `degf` and `ci_method` — it is stored **on each column, not on the table**, because `dplyr` drops table attributes and a number must never depend on one. A bind reconciles these by the weakest-claim rule, so a merge can only claim the inference its weakest part carried.

**Design-based cell variance** (`survey-variance.R`) feeds the existing `n_eff` field (effective sample size), so the ordinary CI machinery becomes design-aware with no new field. A plain weight column is a survey design at `ids = ~1`, where the general formula collapses to a per-cell closed form computed from the aggregate alone (Kish is its degenerate limit); a real design uses `survey::svyrecvar`. survey owns the variance algebra throughout.

## The colour system

Colour is decomposed into three orthogonal axes: a **measure** (what to compare — `diff` / `ratio` / `contrib` / `or`), a **channel** (text and/or background), and a **significance policy** (`color_signif`: `ignore` / `grey_non_signif` / `guaranteed_effect`). The engine has three layers:

1. **Palettes** — OKLCH colour ramps, hand-tuned so intensity levels are distinguishable, in light, dark, and 8-bit (non-truecolor terminal) variants, set via `set_color_palette()`.
2. **Breaks** — per-scale thresholds (`COLOR_SCALES`), mirrored for the under side; a break value above 1 means a *ratio* comparison (the "×2" rule), so the default pct breaks encode both additive and multiplicative thresholds.
3. **Selection** — a vectorised `findInterval` engine (`fmt_color_plan` → `fmt_color_slots` → `fmt_color_channels`) that folds each cell per side and picks the strongest matching threshold.

The measure's behaviour — its raw getter, scale keys, significance source and gating — lives in the `MEASURES` row, which drives both the plan and the legend (no per-measure branches). Every backend consumes the one artifact `fmt_color_channels` produces, which is why console, HTML, Excel, Markdown and plots colour identically.

## The regression subsystem

`tab_reg()` gives models the same visual language: it fits one model, renders per-family effect measures as `fmt` cells, and returns the same `tabxplor_tab` — same colours, same accessors, same exports. It reuses the 21 `fmt` fields unchanged; `obs` and `gap_se` carry the regression-specific facts, so there is no separate field set. The subsystem is seven files, each with a fuller header.

**The estimand — effect × measure** (`reg-estimand.R`, `REG_ESTIMANDS`). The user asks two questions: which **contrast** (`effect`: coefficient / marginal / at-reference) and which **measure** (odds ratio / ratio / difference / log). One declared row per `(family, effect, measure)` answers both: it names the internal fit/link, the exponentiate flag, the header word, and the stored `scale` the column carries. The **family is auto-detected** from the outcome — a 2-level factor → logistic/OR, numeric → linear/β, a count → Poisson/IRR, 3+ unordered → multinomial, ordered → cumulative-OR ordinal — but one table can mix families (each column stores its own `model_family`). `reg_measures(data, outcome)` lists what an outcome offers; a missing `(effect, measure)` combination aborts with the list of what it does offer.

**The observed companion — the distinctive feature** (`reg-empirical.R` + `reg-influence.R`). With `empirical = TRUE`, each model effect is placed beside its **crude/observed counterpart** on the same scale — so "what did adjustment change" is read directly. `REG_EMPIRICAL` declares, per family, the shape of the crude column and its CI method; the crude value comes either from a closed form on a per-cell grid or from a univariable refit through the same fitter, so the two share estimand, link and CI rule by construction. `reg-influence.R` computes the **standard error of the gap** between the adjusted and crude estimates (their covariance, which no arithmetic on the two printed intervals could recover) via influence functions — the package's only `survey::svyrecvar` caller — and that gap SE is what lets `color_signif` colour the adjustment itself.

**The argument boundary** (`reg-resolve.R`). `reg_resolve_args()` is the `tab_reg()` analogue of the crosstab boundary: six declared stages (validate → prepare data → resolve estimands → resolve output → resolve fit plan → resolve specs) that do every check and every rewrite of `data` in one ordered place, returning a typed record the builder reads.

**The staged build** (`tab_reg.R` + `reg-spec-build.R`). `reg_build()` runs over a typed `new_reg_ctx`, one named stage per part of the table it produces; the per-model half is a declared product (`reg_spec_build()`), so "what is per-model vs between-models" has one answer. The three nesting axes — `tab_vars` groups × models × outcomes — dispatch through the shared parallel seam. The stage order is the source order and is load-bearing: every fit may emit a message, and the characterisation harness compares the message stream in order.

**Effects and model checks.** Marginal effects (AME, and MER at a reference profile) are computed by analytic g-computation (`reg-influence.R`) or `marginaleffects`, chosen per estimand. `REG_CHECKS` catalogues the model checks (linearity, dispersion, influence, proportionality, collinearity), each with the `shape =` cure that fixes what it flags — the check and its cure are one object — and each priced (`free` runs by default, `refit`-cost checks are opt-in).

## Exports and rendering

`tab_export(x, format =)` (`tab-export.R`) is the facade over four backends: HTML (the default), Markdown, Excel and plot. They share one preparation step, `tab_export_prep()` (`tab-export-prep.R`), which builds an ephemeral render model (roles, references, bold/italic, header spans, variable-name blocks) that every backend consumes.

Display values reach the backends by one source of truth: `format.tabxplor_fmt()` renders the text for console, Markdown and HTML; `tab_xl()` writes the raw value and takes its number-format codes from the *same* `format(syntax = "excel")`, so a display change never needs mirroring. Colour is single-sourced too — every backend reads `fmt_color_channels`. HTML colour is a slot **class**, never inline hex, with the theme living in a `<style>` block from the one CSS generator (`tab-css.R`), so `theme = "auto"` (light/dark) and `theme = "print"` (bold/italic, black-and-white) work by stylesheet. `tab-transpose-render.R` flips a finished render model (a transposed column is heterogeneous and cannot be an `fmt` column), and `tab-theme-detect.R` best-effort-detects the console's light/dark scheme — a subsystem that must never error, because a wrong guess only mis-tints, never breaks.

## jamovi

Two point-and-click analyses mirror the two producers: `jmvtab` (Crosstables) and `jmvtabreg` (Regressions). Each is a thin `R6` backend (`*.b.R`) over an engine-free build core (`jmvtab_build()` / `jmvtab_reg_build()`) that drives `tab()` / `tab_reg()` through a content-addressed **live-UI cache** (`*-cache.R`), so a repeated interactive tweak re-paints instead of recomputing. Each option is named after the argument it drives, so the backend is a pass-through rather than a translation table. The generated `*.h.R` option headers are never hand-edited.

## Cross-cutting invariants

Rules that span subsystems — do not undo them without reading why:

- **A number must not depend on a table attribute.** Inference facts (`conf_level`/`degf`/`basis`/`ci_method`) live on columns; `dplyr` drops table attributes.
- **A merge claims only what its weakest part carried** — the `vec_ptype2` reconcile applies the weakest-claim rule to inference attributes so a bind cannot over-claim.
- **Public API is stable; internals are free.** Soft-deprecate public arguments; the `fmt` fields users read with `$`/`mutate()` must not break.
- **Facts live in one table.** Add a row to a fact table and read it through its accessor; a foreign key checked at load keeps cross-table references honest.
- **The `fmt` record is dense.** Every column carries all fields; "not applicable" is `NA`, never an absent field.
- **`format()` is the one display source of truth** — text backends and the Excel numFmt codes both come from it.
- **Levels drop after the tests.** Non-first levels (`levels = "first"`) are removed only after chi²/CI, so tests see the full level set.
- **Theme detection must never error** — it rests on no supported API; anything unknown resolves to "light".

## Where to find detail

- **Per-file design and constraints** — the R file-header comments (and, later, `# DESIGN:` / `# WARNING:` inline tags).
- **The file index** — CLAUDE.md § Repository Map, grouped by subsystem.
- **Usage and teaching** — the vignettes (`vignette("tabxplor")`, the regression and programming vignettes).
- **Reference pages** — `?tabxplor-vctrs` (the `fmt` record), `?tabxplor-options` (generated from `TAB_OPTIONS`), `?tabxplor-data.table`.
- **Inspecting a built table** — the exported accessors: `tab_shape()`, `tab_columns()`, `reg_measures()`, `fmt_attr()`, and the `get_*` / `set_*` family.
