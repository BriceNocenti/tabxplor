# dev/jamovi/reference — vendored jamovi module examples

Real, unmodified source files from public jamovi modules, downloaded 2026-07-08 as
copyable reference for tabxplor's Jamovi module work (2.0.0 Phase 8, Phase 10).

These are **read-only study material**, not part of the package (the whole `dev/`
tree is `.Rbuildignore`'d via `^dev$`). Re-fetch from the URLs below for byte-exact
updates. The narrative that explains every file is `dev/tabxplor_2.0.0_jamovi_dev.md`.

## Why each file is here

### `jmv-logregbin/` — THE per-variable reference-level picker (feature 1)

`jamovi/jmv` binomial logistic regression. The canonical "pick a reference level for
each selected variable" widget: an `Array` of `Group{var: Variable, ref: Level}`
rendered by a `ListBox` with a `LevelSelector` column, kept in sync by JS.

- `logregbin.a.yaml` — the `refLevels` Array/Group/Level option (search `refLevels`).
- `logregbin.u.yaml` — the `ListBox` + `VariableLabel`/`LevelSelector` columns + `events`.
- `logregbin.events.js` — `updateContrasts` (row reconciliation) + `updateLevelControls`
  (binds each row's `LevelSelector` to its variable via `setPropertyValue('variable', …)`).
- `logregbin.b.R` — how `self$options$refLevels` is read in R (large file; grep `refLevels`).
- `logregbin.r.yaml` — results definition (for reference).
- Source: `https://github.com/jamovi/jmv/tree/main` (branch `main`).

### `jmv-anova/` — Variant B (static ComboBox per variable) + rich per-row templates

- `anova.a.yaml` — `contrasts` Array/Group with a static `List` (contrast *type*, not a
  level); `emMeans` Array of `Variables`; `NMXList` effect sizes; `Output` residuals.
- `anova.u.yaml` — `Supplier`/`ListBox` model-terms builder; `contrasts` ComboBox column;
  `emMeans` rich per-row `LayoutBox` template with `addButton`; `Output` Save section.
- `anova.js` — model-terms + marginal-means supplier sync (`calcModelTerms`, `updateContrasts`).

### `jmv-anovarm/` — ordered-levels Array + `RMAnovaFactorsBox` (feature 2 reality check)

- `anovarm.a.yaml` — an **ordered** list of levels modeled as `Array{template: String}`
  (element order = level order); the `rm` factors-and-levels structure.
- `anovarm.u.yaml` — the compiled `RMAnovaFactorsBox` control + a `templateName` cells grid;
  note `itemDropBehaviour: emptyspace` (append-only) on the terms box.
- `anovarm.js` — reacting to an ordered option changing (`updateFactorCells`/`filterCells`).

### `jmv-conttables/` — contingency tables (closest domain to tabxplor)

`jamovi/jmv` contingency tables (chi-square). The nearest built-in analog to tabxplor's
crosstabs — rows/cols suppliers, counts, percentages, chi-square. Good structural model.

### `gamlj/` — Variant B in the wild + conditional reveal + Action-`open` export

- `functions.js` / `gamlj.events.js` — `updateContrasts` plus `updateCustom` (reveal a
  second dependent ListBox only when a row's type == "custom", via `$el[0].style.display`).
- `Saver.R` — the version-gated `option$perform` vs `jmvReadWrite:::jmvOpn` "open as
  dataset" export idiom (jamovi ≥ 2.7.12). Source branch: `master`.

### `SummaryTables/` — THE working file-export pattern (feature 3)

`NourEdinDarwish/SummaryTables`. A module that saves to a **user-typed path** — no
file-picker control needed.

- `tblsummary.a.yaml` — `String(path)` + `Action(export)` options.
- `tblsummary.u.yaml` — `TextBox` + `ActionButton` inside a `CollapseBox`.
- `tblsummary.b.R` — click detected in R as a boolean (`if (self$options$export)`).
- `export.R` — **`resolveExportPath()`**: the copy-me-verbatim Windows path resolver
  (`USERPROFILE` not `~`, strip "Copy as path" quotes, no `sub()` backref bug) +
  `jmvcore::Notice` success report. Adapt to `openxlsx`/`tab_xl()`.

### `jamovi-client/` — compiled TypeScript controls (understand the internals)

From the `jamovi/jamovi` Electron app (`client/`). Not editable by a module — here to
explain *why* the YAML/JS behaves as it does.

- `levelselector.ts` — how `LevelSelector` fetches a variable's levels (`requestData`);
  renders levels **in received order, never reorders**.
- `gridtargetcontrol.ts` — `itemDropBehaviour` enum (`insert` = drag-reorder ON,
  `emptyspace` = append-only, `overwrite`); the drag-reorder mechanism.
- `gridactionbutton.ts` — the per-row action-button control (for up/down arrows).
- `datavarwidget.ts` — the Data-tab level editor's `_moveUp`/`_moveDown` splice idiom
  (core-only; the algorithm to replicate for arrow-button level reordering).

### `jamovi-compiler/uicompiler.js` — authoritative `.u.yaml` property list

The compiler enforcing which properties each control accepts. Grep it when the docs are
vague about a control's exact property names. Source branch: `master`.

### `jmvcore/options.R` — authoritative `.a.yaml` option-type contracts

The R6 classes (`OptionBool`, `OptionList`, `OptionArray`, `OptionVariable`, …) that each
`.a.yaml` `type:` compiles to. The real source of truth for option keys/defaults (the
`dev.jamovi.org/api/*` pages are thin summaries). Mirror of CRAN `jmvcore`.

## Branch note

`jamovi/jmv` and `jamovi/jamovi` default branch = `main`; `gamlj/gamlj` = `master`;
`NourEdinDarwish/SummaryTables` = `main`; `jamovi/jamovi-compiler` = `master`;
`jmvcore` mirrored from `cran/jmvcore` = `master`.
