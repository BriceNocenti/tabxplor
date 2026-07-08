# tabxplor 1.4.0 — Jamovi module development guide

Written 2026-07-08. This is the reference for redesigning tabxplor's Jamovi module UI in
1.4.0 (Phase 8) and integrating `tab_logit` (Phase 10). It exists because Jamovi module
development is a multi-layer stack with sparse, recently-reorganised official docs, and
past attempts (manual and AI-assisted) failed for lack of a mental model. This file gives
that model, the toolchain, the debugging workflow, and — most importantly — **verbatim,
locally-vendored working examples** for the three features we want:

1. a per-variable **reference-level** picker (choose the reference of each `row_var` under
   `pct="row"`, of one `col_var` under `pct="col"`);
2. a **level-reordering** UI for row/col factor variables;
3. a module-level **Excel export** with a user-friendly path/folder selector.

Companion material lives in `dev/jamovi/reference/` (real source files from `jmv`, `gamlj`,
`SummaryTables`, and the jamovi Electron client) — see `dev/jamovi/reference/README.md`.
Every recipe below points at the exact vendored file it was distilled from.

> How to use this file: read §1–§4 once (the mental model + toolchain + debugging), then
> jump to the feature you are building (§9 keystone, §10 ref-levels, §11 reorder, §12
> export). §14 is the Claude-Code working method. §5–§8 are the API reference tables to
> grep when writing YAML/JS.

---

## 1. Why this is hard: the layer cake and the mental model

A Jamovi module **is a normal R package** with an extra `jamovi/` folder. Jamovi itself is
an Electron desktop app embedding a Python server + an R "engine" process. One analysis is
spread across **six files** in two languages plus one generated file:

| File                                    | Language   | Role                                                     | Edit?       |
|-----------------------------------------|------------|----------------------------------------------------------|-------------|
| `jamovi/<name>.a.yaml`                  | YAML       | **Analysis definition** — the options (data model)       | ✓           |
| `jamovi/<name>.r.yaml`                  | YAML       | **Results definition** — tables/plots/html/output slots  | ✓           |
| `jamovi/<name>.u.yaml`                  | YAML       | **UI definition** — the options-panel layout (view)      | ✓           |
| `jamovi/js/<name>.js` (or `.events.js`) | JavaScript | **Custom UI events** — interactive behaviour             | ✓           |
| `R/<name>.b.R`                          | R (R6)     | **Backend** — `.init()`/`.run()`/`.plot()` analysis body | ✓           |
| `R/<name>.h.R`                          | R (R6)     | **Generated header** — options + base class              | ✗ generated |
| `jamovi/0000.yaml`                      | YAML       | Module **manifest** (analyses list, version, min app)    | ✓           |

The Model–View–Controller split is the key idea:

- `.a.yaml` = **Model** (the options; compiles to `R/<name>.h.R`).
- `.u.yaml` = **View** (the layout; property values like labels are pulled from `.a.yaml`).
- `.js` = **Controller** (reacts to user actions, rewrites option values live).
- `.b.R` = the R analysis that reads `self$options$*` and writes `self$results$*`.

Two compilers cooperate (see §3):

```
you ──▶ jmvtools (R)  ──▶  jamovi-compiler / jmc (Node)  ──▶  R/<name>.h.R  +  compiled JS bundle
                                                          ──▶  build .jmo  ──▶  install into jamovi app
```

The reason past edits failed: changing `.u.yaml`/`.js` does nothing until recompiled and
reinstalled; `.h.R` must be regenerated from `.a.yaml` (never hand-edited); and the custom
JS layer (`ui.<control>.value()`/`setValue()`, `applyToItems`, `setPropertyValue`) is
undocumented enough that you must copy a working module. This guide removes both problems.

tabxplor's module is `usesNative: true` and embedded in the R package (`R/jmvtab.b.R` +
`R/jmvtab.h.R` + `jamovi/jmvtab.*`), so it already follows this architecture.

---

## 2. The tabxplor module today (inventory + pain points)

Current files (all present, working, on CRAN as part of tabxplor 1.3.1):

| File                   | Notes                                                                                                                                                                                                                                                          |
|------------------------|----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| `jamovi/0000.yaml`     | Manifest: one analysis `jmvtab`, `menuGroup: tabxplor`, `minApp: 1.0.8`.                                                                                                                                                                                       |
| `jamovi/jmvtab.a.yaml` | ~30 options: row/col/tab vars, wt, pct, color, OR, chi2, na, lvs, ref, ref2, comp, ci, conf_level, ci_print, totaltab, wrap_*, display, add_n/pct, digits, subtext, and the Excel-export block (`exportExcel` Action + `xl_path`/`xl_filename`/`xl_replace`).  |
| `jamovi/jmvtab.r.yaml` | Results: `html_table` (Html), `export_status` (Html), an empty `plot` Image.                                                                                                                                                                                   |
| `jamovi/jmvtab.u.yaml` | `jus: '3.0'`, `compilerMode: tame`. VariableSupplier + several CollapseBoxes of RadioButton/CheckBox/TextBox; the Excel export laid out as an `ActionButton` + folder/filename `TextBox`es + a "Replace" CheckBox.                                             |
| `jamovi/js/jmvtab.js`  | Almost empty — one `exportExcel_changed` handler that resets the button after 2 s; the rest is commented-out ANOVA-example code.                                                                                                                               |
| `R/jmvtab.b.R`         | R6 `.run()`: builds `tab_many(..., compact=TRUE)`, renders via `tab_kable()` into `html_table`, handles Excel export via `tab_xl()` with a hand-rolled folder-existence check and success/error HTML in `export_status`. `.plot()` is a stub returning `TRUE`. |
| `R/jmvtab.h.R`         | Generated `jmvtabOptions` (R6, inherits `jmvcore::Options`) + `jmvtabBase`. Never edit.                                                                                                                                                                        |

Pain points visible in the code (all solved by patterns in this guide):

- **`ref`/`ref2` are free-text `TextBox`es** (the `List` version is commented out). There
  is no per-variable reference chooser → feature 1 (§10).
- **No level reordering** anywhere → feature 2 (§11).
- **Excel export is fragile**: an `ActionButton` whose JS resets it after 2 s, a plain
  `TextBox` for the folder (default `"S:/Documents"`), a hand-rolled `dir.exists()` guard,
  and repeated failed attempts in comments (`FilePicker`, `get_user_documents()`,
  `%USERPROFILE%`, `navigator.platform` defaults). This is exactly the problem
  `SummaryTables::resolveExportPath()` already solved → feature 3 (§12).
- **The R session appears to reset each run** (a comment notes `excel_message_count`
  resets), meaning cross-run state must live in Jamovi `state`/options, not R globals (§13,
  Phase 8 caching).

---

## 3. Toolchain and the dev loop

### 3.1 What to install (Windows 11)

1. **The Jamovi desktop app** (from `jamovi.org`). Mandatory: it is the install target and
   provides the bundled R engine that runs the module. Build against the same Jamovi series
   you will run (a `.jmo` is tied to OS + arch + Jamovi series).
2. **R 4.5.1** (yours) + **Rtools** matching it (needed to build the package into a `.jmo`).
3. **`jmvtools`** and the **`node`** R package, both from the Jamovi repo:

   ```r
   install.packages('node',
                     repos = 'https://repo.jamovi.org')
   install.packages('jmvtools',
                     repos = c('https://repo.jamovi.org', 'https://cran.r-project.org'))
   ```

   `jmvtools` vendors the Node `jamovi-compiler` (`jmc`) internally; the `node` package
   supplies the runtime. You do **not** need a separate system Node install for the normal
   path.

### 3.2 `jmvtools` — the developer entry point ("devtools for Jamovi")

Repo: `github.com/jamovi/jmvtools`. Exported functions:

| Function                         | Does                                                                                |
|----------------------------------|-------------------------------------------------------------------------------------|
| `create('Name')`                 | Scaffold a new module (`DESCRIPTION`, `NAMESPACE`, `R/`, `jamovi/0000.yaml`).       |
| `addAnalysis(name=, title=)`     | Add one analysis: creates `.a/.r/.u.yaml` + `R/<name>.h.R` + `R/<name>.b.R`.        |
| `prepare()`                      | **Compile only.** Regenerate `.h.R` + the JS UI bundle from YAML. Fast; no install. |
| `install()`                      | **Full build + install** the `.jmo` into Jamovi. The main dev-loop command.         |
| `check()`                        | Verify jmvtools can find Jamovi (prints the path/version). Run this first.          |
| `i18nCreate('catalog'|'<lang>')` | Create translation `.pot` / `.po` under `jamovi/i18n/`.                             |
| `i18nUpdate()`                   | Refresh catalogs after adding translatable strings.                                 |
| `version()`                      | Report the jmvtools/compiler version.                                               |

### 3.3 The Windows gotcha: pointing jmvtools at Jamovi

On Windows, jmvtools **cannot auto-find** the Jamovi install. Set the home path (pick one):

```r
options(jamovi_home = 'C:/Program Files/jamovi/bin')   # adjust to your install
# or per call:
jmvtools::check(home = 'C:/Program Files/jamovi/bin')
jmvtools::install(home = 'C:/Program Files/jamovi/bin')
# or persistently: set the JAMOVI_HOME environment variable.
```

`jmvtools::check()` succeeding (prints a version) means you are wired up. If it errors, fix
`home` before anything else. (Note: on macOS notarized builds don't work with jmvtools; not
relevant on Windows.)

### 3.4 The dev cycle

```
edit jamovi/*.yaml + jamovi/js/*.js + R/*.b.R
      │
      ├─ jmvtools::prepare()   # regenerate R/<name>.h.R + JS bundle (fast, no install)
      │
      └─ jmvtools::install()   # compile → build .jmo → install into Jamovi
                               # then RELOAD the analysis in Jamovi to see changes
```

Community-hardened variant for complex modules (ClinicoPath guide): run
`jmvtools::prepare()` then `devtools::document()` **twice** (cross-references sometimes need
a second pass), then `jmvtools::install()`. If the UI won't update: close Jamovi fully,
clear its cache, reinstall (file locks on `.jmo` are common on Windows — close the analysis
first).

### 3.5 `jamovi-compiler` (`jmc`) — the code generator underneath

Repo: `github.com/jamovi/jamovi-compiler` (Node; `bin` = `jmc`). jmvtools shells out to it.
It reads the YAML and emits `R/<name>.h.R` + a browserified JS bundle (from `.u.yaml` +
`jamovi/js/<name>.js`), then on install zips everything into a `.jmo`. You rarely call it
directly, but its flags map 1:1 to jmvtools and are useful for debugging a bad build:

| `jmc` flag                                  | Meaning                                   |
|---------------------------------------------|-------------------------------------------|
| `-p, --prepare <dir>`                       | regenerate headers/UI (= `prepare()`)     |
| `-i, --install <dir>`                       | build + install (= `install()`)           |
| `-c, --check`                               | validate the Jamovi install (= `check()`) |
| `--home <dir>`                              | Jamovi directory (= `home=`)              |
| `--debug` / `--verbose`                     | detailed compiler stack traces / logging  |
| `--i18n <dir> --create <lang>` / `--update` | catalog management                        |

`uicompiler.js` (vendored at `dev/jamovi/reference/jamovi-compiler/uicompiler.js`) is the
authoritative list of which properties each `.u.yaml` control accepts — grep it when docs
are vague.

### 3.6 Distribution

- **Sideload** a built `.jmo`: in Jamovi, library **+** (top-right) → **Side-load** tab →
  pick the `.jmo`. Appears in the ribbon like any module.
- **Public release**: email a GitHub link to `contact@jamovi.org`; the Jamovi team
  cross-compiles all OS/arch/series variants for the Jamovi library. Needs an OSI license.

---

## 4. Debugging: the dev console and how to inspect the REAL HTML

This is the capability the previous attempts were missing.

- **Open Chrome DevTools inside Jamovi: press `F10`.** You get the full Elements/DOM
  inspector, Console, Sources, Network — the *actual* rendered HTML of both the analysis
  options panel and the results. Jamovi's UI is **nested iframes**, so if `F10` doesn't
  register, **click the blue bar along the top first** (to move focus to the top window),
  then `F10`.
- **Inspect the options-panel DOM**: in Elements, drill into the iframe that renders the
  options panel to see exactly what your `.u.yaml` + custom JS produced. `ui.view.el` is the
  root DOM node (`ui.view.$el` the jQuery wrapper); `ui.<control>.el` is a control's node.
  To capture the real HTML to a file, right-click the panel's root node → **Copy → Copy
  outerHTML**, or in the Console run `copy(ui.view.el.outerHTML)` and paste into a file.
- **`console.log` from custom JS** appears in that DevTools Console (a normal Chromium
  console); you can also drive the UI from the console as a REPL, e.g.
  `ui.pct.value()`, `ui.ref.setValue('tot')`.
- **R errors / stack traces**: Jamovi's **dev mode** shows a full stack trace when an
  analysis errors (launch Jamovi from a terminal / debug entry so the engine console is
  visible). In `.b.R` you can also drop `browser()` into `.run()` to pause, and surface
  progress with `jmvcore::Notice` (see §12) or `message()` to the engine console.
- **Compiler errors**: `jmc --debug --verbose` (or reading `jmvtools::install()` output)
  gives the detailed generation error when a build fails.

Practical loop: edit → `jmvtools::install(home=)` → reload analysis in Jamovi → `F10` →
inspect DOM + Console → iterate.

---

## 5. `.a.yaml` option types (the data model)

Each `type:` compiles to an R6 class in `jmvcore` (`dev/jamovi/reference/jmvcore/options.R`
is the authoritative source; the `dev.jamovi.org/api/*` pages are thin summaries). Common
keys on every option: `name` (required; the R accessor `self$options$<name>`), `title` (UI
label; defaults to `name`), `type`, `default`, and a docs-only `description:` block.

| `type:`     | Backs (UI)                | Key type-specific keys                                                                                               | Value in R              |
|-------------|---------------------------|----------------------------------------------------------------------------------------------------------------------|-------------------------|
| `Data`      | (dataset)                 | —                                                                                                                    | data frame              |
| `Bool`      | CheckBox                  | `default`                                                                                                            | logical                 |
| `Integer`   | TextBox `format:number`   | `min`, `max`, `default`                                                                                              | integer                 |
| `Number`    | TextBox `format:number`   | `min`, `max`, `default`                                                                                              | numeric                 |
| `String`    | TextBox                   | `default`                                                                                                            | character               |
| `List`      | ComboBox / RadioButton    | `options:` (each `name`+`title`), `default`                                                                          | one `name`              |
| `NMXList`   | set of CheckBoxes         | `options:`, `default` (vector)                                                                                       | character vector        |
| `Variable`  | one VariablesListBox slot | `suggested`, `permitted`, `required`, `rejectInf`(F), `rejectMissing`, `rejectUnusedLevels`, `takeFromDataIfMissing` | column name             |
| `Variables` | multi VariablesListBox    | as `Variable` (`rejectInf` default **T**)                                                                            | character vector        |
| `Level`     | LevelSelector / ComboBox  | (variable pairing done in UI/JS)                                                                                     | one level string        |
| `Terms`     | Supplier (model terms)    | `default`                                                                                                            | list of terms           |
| `Pairs`     | two-column ListBox        | `suggested`, `permitted`                                                                                             | list of `{i1,i2}`       |
| `Sort`      | (Group)                   | fixed `sortBy`/`sortDesc`                                                                                            | `{sortBy,sortDesc}`     |
| `Group`     | fixed bundle              | **`elements:`** (fixed sub-options)                                                                                  | named list              |
| `Array`     | **templated ListBox**     | **`template:`** (usually a `Group`), `default`                                                                       | list of clones          |
| `Action`    | ActionButton              | `action` (default `'open'`)                                                                                          | logical (TRUE on click) |
| `Output`    | Output (Save section)     | a.yaml: minimal; r.yaml carries `varTitle`/`measureType`/`clearWith`/`initInRun`                                     | logical                 |

Not `.a.yaml` option types (common confusions): `Ncrementer` is a `.u.yaml` control backed
by an `Integer`/`Number` option; `clearWith` is a **results** (`.r.yaml`) key, not an option
key; there is no `Value` option type.

`suggested`/`permitted` values are measure types: `continuous`, `ordinal`, `nominal`,
`nominaltext`, `id`, `numeric`, `factor`. `suggested` = soft highlight; `permitted` = hard
filter.

---

## 6. `.u.yaml` control catalog (the view)

Root keys: `title`, `name`, `jus` (**must be `'3.0'`** for JS events), `stage`,
`compilerMode` (`aggressive` overwrites layout on `.a.yaml` change; **`tame` preserves your
hand-edits** — tabxplor uses `tame`), then a `children:` tree.

Properties available on most controls (`BaseControl`): `type`, `name`, `label`, `enable`
(boolean DSL, see below), `events` (map event→handler), `margin` (`none|small|normal|large`),
`cell` (`{row, column}` grid position), `stretchFactor`, `style` (`list|inline`),
`horizontalAlignment`, `verticalAlignment`, `min/maxWidth`, `min/maxHeight`, `stage`,
`fitToGrid`, `children`.

| Control (`type:`)             | Purpose                                       | Key properties                                                                                                                                                                                   |
|-------------------------------|-----------------------------------------------|--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| `VariableSupplier`            | left variable pool                            | `suggested`, `permitted`, `populate` (`auto|manual`),`persistentItems`,`stretchFactor`                                                                                                           |
| `Supplier`                    | term/model pool (non-var)                     | `format: term`, `higherOrders`, `persistentItems`, `label`                                                                                                                                       |
| `TargetLayoutBox`             | wraps a drop target                           | `label`, `transferAction`                                                                                                                                                                        |
| `VariablesListBox`            | variable drop target                          | `isTarget`, `maxItemCount`, `minItemCount`, `ghostText`, `valueFilter`, `height`, `columns`, `template`                                                                                          |
| `ListBox` ★                   | templated list (1 row per Array element)      | `template` **or** `columns`, `showColumnHeaders`, `fullRowSelect`, `selectable`, `isTarget`, `maxItemCount`, `valueFilter`, `itemDropBehaviour`, `addButton`, `templateName`, `height`, `events` |
| `LevelSelector`               | pick one level of a variable                  | `name`, `label`, `enable` (bound to a `Level` option; dynamic)                                                                                                                                   |
| `ComboBox`                    | dropdown (List)                               | `name`, `label`, `format`, `enable`                                                                                                                                                              |
| `RadioButton`                 | one List value                                | `optionName`, `optionPart`, `children`                                                                                                                                                           |
| `CheckBox`                    | Bool / one NMXList part                       | `optionName`, `optionPart`, `children`, `enable`                                                                                                                                                 |
| `TextBox`                     | String / Integer / Number                     | `format` (`string|number`),`suffix`,`inputPattern`,`width`,`ghostText`                                                                                                                           |
| `Label`                       | text / group header                           | `label`, `format` (`bool|number`),`style`,`children`                                                                                                                                             |
| `VariableLabel` / `TermLabel` | read-only item renderer (in a `columns` cell) | (display only)                                                                                                                                                                                   |
| `LayoutBox`                   | grid/stack container                          | `margin`, `cell`, `stretchFactor`, `style`, `fitToGrid`                                                                                                                                          |
| `CollapseBox`                 | collapsible section                           | `label`, `collapsed`, `enable`                                                                                                                                                                   |
| `ActionButton`                | button                                        | `name`, `events`, `enable`                                                                                                                                                                       |
| `Output`                      | write a column back to data                   | `name`                                                                                                                                                                                           |
| `RMAnovaFactorsBox`           | RM factors+levels editor (compiled)           | `name`, `label`                                                                                                                                                                                  |
| `CustomControl`               | JS-built DOM (escape hatch)                   | `creating`/`updated` events                                                                                                                                                                      |
| `Separator`                   | visual divider                                | —                                                                                                                                                                                                |

`enable:` DSL examples (colon = "list option has part"):

```yaml
enable: (pct:row || pct:col)          # tabxplor already uses this
enable: (OR == 'OR' || OR == 'OR_pct')
enable: (!(missing:no))
enable: ({return !!ui['groupVar'].value();})   # JS arrow form also allowed
```

`template:` vs `columns:` (the crux — §9):

- **`template:`** — one control instantiated per row (item is a scalar): e.g. a
  `VariableLabel` per selected variable, a `TermLabel` per model term.
- **`columns:`** — a list of column definitions, each with its own `template:`; use when a
  row is a *record* with several fields. Each column: `name` (maps to the array item's
  sub-field / Group `elements[].name`), `label`, `selectable` (false = display-only),
  `stretchFactor`, `maxWidth`, `template`. This is how you get a `ComboBox`/`LevelSelector`
  **per selected variable**.

---

## 7. The custom-JS events API (the controller)

File: `jamovi/js/<name>.js` (older split style used `<name>.events.js` referenced as
`./<name>.events::handler`; a plain `<name>.js` works too). It exports an object of
handlers. Requires `jus: '3.0'`.

### 7.1 Handler naming and events

`module.exports = { <handlerName>: function(ui, event) { ... }, ... }`

- **View events**: `view_creating` (before DOM), `view_loaded` (after DOM), `view_updated`
  (analysis reselected). Some modules use a bare `update:` handler for the same purpose.
- **Control events**: `<control>_changing` (before), `<control>_changed` / `<control>_change`
  (after). Some modules name them `onChange_<control>`.
- **ListBox item events**: `<listbox>_listItemAdded`, `<listbox>_listItemRemoved` (or a
  `listItemsChanged` custom event).
- Handlers always receive `(ui, event)`.

Wiring from `.u.yaml`: either the naming convention above, or an explicit `events:` block on
a control:

```yaml
events:
  change: './logregbin.events::onChange_refLevels'   # explicit module-path form
# or
events:
  listItemAdded: emMeans_listItemsChanged            # short handler-name form
```

### 7.2 The `ui` object and context helpers (verified against jmv `logregbin.events.js`)

- `ui.<name>.value()` / `ui.<name>.setValue(v)` — read/write an option value.
- `ui.<name>.applyToItems(startCol, (item, index, column) => {...})` — iterate rendered rows
  of a templated ListBox.
- `item.setPropertyValue('prop', v)` / `item.getPropertyValue('prop')` — set a per-row
  control property at runtime (e.g. bind a `LevelSelector` to a variable).
- `item.controls[i]` / `item.controls.<name>` — the row's child controls.
- `ui.<listbox>.getSelectedRowIndices()` — selected rows.
- `ui.view.el` / `ui.view.$el` — root DOM (raw / jQuery); `ui.<name>.$el[0].style.display`
  — direct DOM manipulation (gamlj hides a dependent ListBox this way).
- **Batching (avoid re-running the analysis on every set):**

  ```js
  ui.view.model.options.beginEdit();
  ui.refLevels.setValue(list);
  ui.someOther.setValue(x);
  ui.view.model.options.endEdit();
  ```

- **Context helpers on `this`** (pass `this` as `context` to sub-functions):
  `context.clone(value, default)`, `context.valuesToItems(list, FormatDef.variable)`,
  `context.itemsToValues(items)`, `context.findChanges(key, list, unique, format)` (returns
  `{added, removed}`), `context.findDifferences(prev, cur, format)`,
  `context.checkValue(control, n, values, format)`, `context.sortArraysByLength(list)`,
  `context.getCombinations(added, terms)`, `context.workspace[...]` (persistent scratch),
  and the format descriptors `FormatDef.variable` / `FormatDef.term`.

- **Limitation**: dataset metadata (e.g. number of rows) is **not** plumbed through to UI
  events — you cannot read row counts in `.js`. Level lists *are* available to
  `LevelSelector` (it requests them itself).

---

## 8. `.r.yaml` results + `clearWith` (caching backbone)

Result element types: `Table`, `Image`, `Group`, `Array`, `Preformatted`, `Html`, `Notice`,
`Output`. tabxplor uses `Html` (`html_table`, `export_status`) + a stub `Image`.

`clearWith:` (on a results element) lists **option names**; when any changes, Jamovi marks
that result stale and recomputes it, reusing results whose inputs are untouched. This is the
built-in invalidation mechanism and the natural hook for Phase 8 caching — declare each
result's true dependencies so pure-display toggles don't recompute the aggregate.

`Output` results element keys (write a column back to the spreadsheet): `varTitle`,
`varDescription`, `measureType`, `clearWith`, `initInRun`; R side:
`self$results$<name>$setValues()`, `setRowNums()`, `isFilled()`, `isNotFilled()`, `setKeys()`.

---

## 9. The keystone pattern: Array-of-Group + templated ListBox + JS row-sync

Features 1 and 2 are both instances of one pattern. Learn it once:

1. **`.a.yaml`**: an `Array` option whose `template:` is a `Group` of `elements:` — one
   element identifying the variable (`type: Variable`), one holding the per-variable choice
   (`type: Level` for a real level, or `type: List` for a fixed enum).
2. **`.u.yaml`**: a `ListBox` bound to that Array via `name:`, with `columns:` mapping to the
   Group's `elements` — a `VariableLabel` column (display) + a `LevelSelector`/`ComboBox`
   column (the picker) — plus an `events: { change: ... }` hook.
3. **`.js`**: a handler that (a) **reconciles rows** — one row per currently-selected
   variable, preserving prior choices (`updateContrasts` idiom); and (b) **binds each row's
   picker** to its variable (`updateLevelControls` idiom, only needed for the dynamic
   `LevelSelector`). Run both from `update`/`view_loaded` AND from the relevant
   `onChange_<vars>` so rows and levels never go stale.

> Critical modern-Jamovi fact: the old declarative row-sync keys **`items: (factors)`** (on
> the Array) and **`content: $key`** (on the `var` element) are **legacy no-ops** — they no
> longer bind anything. Row population is done entirely in the `.js` (the `updateContrasts`
> loop). GAMLj still carries them cosmetically; do not rely on them.

The two live examples of this pattern are vendored:

- **Level picker** (feature 1): `dev/jamovi/reference/jmv-logregbin/` — `Level` + `LevelSelector`.
- **Static enum picker** (contrast type): `dev/jamovi/reference/jmv-anova/` — `List` + `ComboBox`.

---

## 10. Feature 1 — per-variable reference-level picker

**Goal**: under `pct="row"`, let the user choose the reference row (level) of *each*
`row_var`; under `pct="col"`, the reference column of the chosen `col_var`. This maps onto
the 1.4.0 decision (§2/§4 of the decisions doc) that `ref` becomes a per-row_var named
vector. The Jamovi widget for exactly this is `jmv`'s binomial-logistic `refLevels`.

Source (vendored, byte-exact): `dev/jamovi/reference/jmv-logregbin/{logregbin.a.yaml,
logregbin.u.yaml, logregbin.events.js, logregbin.b.R}`.

### 10.1 `.a.yaml` — the Array/Group/Level option

```yaml
- name: refLevels
  title: Reference Levels
  type: Array
  default:
  template:
      type: Group
      elements:
          - name: var
            type: Variable
          - name: ref
            type: Level
```

Read in R as `self$options$refLevels` →
`list(list(var="gender", ref="female"), list(var="group", ref="control"), ...)`.

### 10.2 `.u.yaml` — the ListBox with a `LevelSelector` column

```yaml
- type: CollapseBox
  label: Reference Levels
  collapsed: true
  stretchFactor: 1
  children:
    - type: ListBox
      name: refLevels
      showColumnHeaders: true
      fullRowSelect: true
      stretchFactor: 1
      height: large
      events:
        change: './jmvtab.events::onChange_refLevels'
      columns:
        - name: var
          label: Variable
          selectable: false
          stretchFactor: 1
          maxWidth: 300
          template:
            type: VariableLabel
        - name: ref
          label: Reference Level
          selectable: false
          stretchFactor: 0.5
          template:
            type: LevelSelector
            label: ''
```

`LevelSelector` is **dynamic**: once its `variable` property is set, it fetches that
variable's own levels and renders them **in data order** (it never reorders — relevant to
feature 2). A static `ComboBox` (as in anova contrasts) cannot show a variable's real
levels — use `LevelSelector`.

### 10.3 `.js` — row reconciliation + per-row level binding (verbatim from logregbin)

```js
const events = {
    update: function(ui) {
        calcModelTerms(ui, this);      // rebuild rows from the selected variables
        updateLevelControls(ui, this); // bind each row's LevelSelector to its variable
    },
    onChange_row_vars: function(ui) { calcModelTerms(ui, this); },
    onChange_col_vars: function(ui) { calcModelTerms(ui, this); },
    onChange_refLevels: function(ui) { updateLevelControls(ui, this); },
};

// one {var, ref} row per selected variable, preserving prior choices
var calcModelTerms = function(ui, context) {
    var variableList = context.clone(ui.row_vars.value(), []);   // + col_vars as needed
    updateContrasts(ui, variableList, context);
};

var updateContrasts = function(ui, variableList, context) {
    var currentList = context.clone(ui.refLevels.value(), []);
    var list3 = [];
    for (let i = 0; i < variableList.length; i++) {
        let found = null;
        for (let j = 0; j < currentList.length; j++) {
            if (currentList[j].var === variableList[i]) { found = currentList[j]; break; }
        }
        if (found === null) list3.push({ var: variableList[i], ref: null });
        else                list3.push(found);
    }
    ui.refLevels.setValue(list3);
};

// tell each row's LevelSelector (column index 1) which variable's levels to show
var updateLevelControls = function(ui, context) {
    let dlist = ui.refLevels.value();
    ui.refLevels.applyToItems(0, (item, index, column) => {
        if (column === 1)
            item.setPropertyValue('variable', dlist[index].var);
    });
};

module.exports = events;
```

### 10.4 Reading it in `.b.R`

```r
# self$options$refLevels -> list(list(var=, ref=), ...)
ref_named <- purrr::map_chr(self$options$refLevels, "ref") |>
  rlang::set_names(purrr::map_chr(self$options$refLevels, "var"))
# pass ref_named into tab_many(ref = ...) as the per-row_var named vector (decisions §4)
```

### 10.5 Adapting to tabxplor's semantics

- Under `pct="row"`/means, `ref` is a reference **row** per `row_var` → the `refLevels`
  ListBox is populated from `row_vars`.
- Under `pct="col"`, `ref` is a reference **column** of one `col_var` → populate from
  `col_vars` (and message that only one applies, per decisions §4).
- Keep the existing free-text `ref`/`ref2` `TextBox`es as a fallback/expert path
  (`"auto"`/`"tot"`/`"first"`/regex still valid); the ListBox writes the common case.
- The empirical-OR `ref2` (a second reference for odds ratios) can reuse the same widget on
  `col_vars` (Phase 10, `tab_logit`).

---

## 11. Feature 2 — level reordering

**Reality check (important):** Jamovi has **no ready-made "drag-sortable list of factor
levels with up/down arrows"** control at module level. Confirmed against the compiler and
the app client. What actually exists:

| Capability                                                  | At module level?       | How                                                                      |
|-------------------------------------------------------------|------------------------|--------------------------------------------------------------------------|
| Drag-reorder chosen **variables/items** in a target ListBox | ✓ native, no JS        | `ListBox isTarget: true` with default `itemDropBehaviour: insert`        |
| Up/down **arrow buttons** on an ordered list                | ⚠ build it             | ordered `Array` + `GridActionButton` column + JS splice                  |
| Drag-reorder factor **levels** inside an analysis           | ✗                      | closest is compiled `RMAnovaFactorsBox` (rename/add/remove, not reorder) |
| Reorder factor **levels** with arrows                       | ✗ (core Data-tab only) | `datavarwidget.ts` `_moveUp/_moveDown` — not reachable from a module     |

### 11.1 Free drag-reorder (no JS)

A `ListBox` with `isTarget: true` and the **default** `itemDropBehaviour: insert` already
supports positional drag-reorder: dropping a row onto an existing position removes it from
its old index and re-inserts at the drop index (one undoable edit). Setting
`itemDropBehaviour: emptyspace` turns this **off** (append-only — used by model-term boxes
where order is immaterial). So: for a sortable list, use `isTarget: true` and **don't** set
`emptyspace`. (Mechanism: `dev/jamovi/reference/jamovi-client/gridtargetcontrol.ts`.)

An **ordered list of levels is modeled as** `type: Array` with `template: {type: String}` —
array element order *is* the level order (see `dev/jamovi/reference/jmv-anovarm/anovarm.a.yaml`,
the `rm` factors' `levels`).

### 11.2 Explicit up/down arrows (needs JS)

Add a `GridActionButton` column (`dev/jamovi/reference/jamovi-client/gridactionbutton.ts`)
to fire custom events, and splice in the handler using the exact Data-tab idiom
(`dev/jamovi/reference/jamovi-client/datavarwidget.ts`):

```js
onChange_moveUp: function(ui) {
    let levels = this.clone(ui.levelOrder.value(), []);
    let i = ui.levelOrder.getSelectedRowIndices();
    if (i.length === 0 || i[0] === 0) return;
    let idx = i[0];
    let item = levels.splice(idx, 1)[0];
    levels.splice(idx - 1, 0, item);     // move down: splice(idx + 1, 0, item)
    ui.levelOrder.setValue(levels);
}
```

Read `ui.<opt>.value()` → splice → `ui.<opt>.setValue(clone)` — the same `value()`/
`setValue()` surface used everywhere.

### 11.3 Recommendation for tabxplor

Reordering **rows / columns / subtables** is really reordering the chosen variables and the
retained levels:

1. For **which variables** and their order: the native drag-reorder target ListBox is free
   and idiomatic — the current `row_vars`/`col_vars`/`tab_vars` VariablesListBoxes already
   reorder by drag.
2. For **level order within a factor**: model an ordered `Array{String}` per variable
   (populate its rows from the variable's levels in JS, like feature 1), rendered as a
   drag-`insert` ListBox; optionally add up/down `GridActionButton`s. Then in `.b.R`, apply
   the order via `forcats::fct_relevel()` before `tab_many()`.
3. Do **not** promise a Jamovi-native drag-sortable levels control — it doesn't exist; build
   the Array+ListBox(+arrows) yourself.

The RM-ANOVA factors editor (`RMAnovaFactorsBox`) is a compiled control you can only invoke
as-is (text inputs + delete buttons, order = typing order); you cannot re-skin it.

---

## 12. Feature 3 — Excel export with a user-friendly path selector

**Reality check:** Jamovi has **no file/folder-picker control** exposed to an analysis
options panel (no `FilePicker`, no `action: save`, no `filters:`). The app's own
Import/Export dialogs are application-level. A module has two working export routes:

- **A. Typed path + direct write** — a `String` option shown as a `TextBox` (the user
  types/pastes a folder or full path) + an `Action` `ActionButton`; the R backend writes the
  file. This is the real, working pattern and what tabxplor already does (minus the
  robustness). **Windows `~` expansion is the main footgun** — solved below.
- **B. Action `open` handoff** — `option$perform(function(action) list(data=df, title=...))`
  opens a data.frame as a new Jamovi dataset/window; the user saves via the app's own
  File ▸ Export. Portable to cloud/server (where arbitrary-path writes are sandboxed).
  Requires Jamovi ≥ 2.7.12; guard with `is.null(option$perform)`.

For tabxplor's `tab_xl()` (writes a formatted `.xlsx`), **route A is correct**, and the
best-in-class implementation is `SummaryTables`. Source (vendored):
`dev/jamovi/reference/SummaryTables/{tblsummary.a.yaml, tblsummary.u.yaml, tblsummary.b.R,
export.R}`.

### 12.1 `.a.yaml`

```yaml
- name: path
  title: Path
  type: String
  default: ~/Desktop/Table.xlsx

- name: export
  title: Save
  type: Action           # no `action: open` -> read as a boolean click in R
```

### 12.2 `.u.yaml`

```yaml
- type: CollapseBox
  label: Export to Excel
  collapsed: true
  stretchFactor: 1
  children:
    - type: TextBox
      name: path
      format: string
      stretchFactor: 1
    - type: ActionButton
      name: export
```

No `FilePicker` — the `TextBox` bound to the `path` String **is** the picker.

### 12.3 `.b.R` — detect the click in R, no JS needed

```r
if (self$options$export) {
    path <- resolveExportPath(self$options$path)   # see 12.4
    tab_xl(tabs, path = path, sheets = "unique", open = FALSE,
           replace = self$options$xl_replace)
    notice <- jmvcore::Notice$new(options = self$options, name = "exportSuccess",
                                  type = jmvcore::NoticeType$INFO)
    notice$setContent(paste0("Saved to: ", path))
    self$results$insert(1, notice)
}
```

The current tabxplor code resets the button via JS (`exportExcel_changed`) and rolls its own
folder check — the `SummaryTables` approach (detect the boolean in R + `jmvcore::Notice`) is
simpler and more robust. The JS `setValue(false)` reset is **optional** (only needed to make
the button re-clickable without another option change); real export modules skip it.

### 12.4 `resolveExportPath()` — copy this verbatim (the Windows fixes)

The single most valuable snippet for tabxplor. It solves every problem in tabxplor's
export-path comments (`~` → Documents inside Jamovi's engine, "Copy as path" quotes,
`sub()` backslash backreference bug, bare filenames). Full source:
`dev/jamovi/reference/SummaryTables/export.R`. Core (adapt `.docx` → `.xlsx`):

```r
resolveExportPath <- function(path) {
  path <- trimws(path)
  path <- gsub("^[\"']|[\"']$", "", path)                # strip Windows "Copy as path" quotes
  if (nchar(path) == 0 || path %in% c("~", "~/")) path <- "~/Desktop/Table.xlsx"
  getHome <- function() {
    home <- Sys.getenv("USERPROFILE")                     # Windows: real profile, NOT Documents
    if (home == "") home <- Sys.getenv("HOME")            # Mac/Linux
    home
  }
  # expand leading ~ WITHOUT sub() (USERPROFILE backslashes = backreferences)
  if (grepl("^~", path)) path <- paste0(getHome(), substring(path, 2))
  if (!grepl("[/\\\\]", path)) path <- file.path(getHome(), "Desktop", path)  # bare name -> Desktop
  if (!grepl("\\.xlsx$", path, ignore.case = TRUE)) path <- paste0(path, ".xlsx")
  normalizePath(path, mustWork = FALSE)
}
```

Why it matters: inside Jamovi's bundled R engine on Windows, `path.expand("~")` resolves to
*Documents*, not the user profile — the root cause of tabxplor's `xl_path` hacks
(`%USERPROFILE%` "not working", the `"S:/Documents"` default). `Sys.getenv("USERPROFILE")`
is the fix.

### 12.5 Optional route B (cloud-portable "open as dataset")

If you also want a picker-free, sandbox-safe export, add an Action that opens the compacted
table as a new dataset (`dev/jamovi/reference/gamlj/Saver.R` shows the version-gated form):

```r
option <- self$options$option("export")
if (is.null(option$perform)) {                      # < 2.7.12
    jmvReadWrite:::jmvOpn(dtaFrm = as.data.frame(tabs), dtaTtl = "tabxplor table")
} else {                                            # >= 2.7.12
    option$perform(function(action) list(data = as.data.frame(tabs), title = "tabxplor table"))
}
```

The user then exports from Jamovi's own File menu (where the real save dialog lives).

---

## 13. File I/O, sandboxing, and state — constraints to respect

- **No native picker** in module options (§12); type-a-path is the only in-panel route.
- **Sandboxing**: on Jamovi cloud/server/Docker, file access is restricted to a mapped
  folder (Documents by default); arbitrary-path writes only work on Desktop. Route B (open
  as dataset) is the portable option. Validate `path` before writing (restrict dirs,
  controlled extension) if this ever ships publicly.
- **UI events can't see the dataset** (no row counts, no cell values in `.js`) — anything
  needing data must happen in `.b.R`.
- **R engine state resets between runs** (tabxplor's own comments confirm it): do not rely on
  R global variables to carry state across runs. Cross-interaction caching (Phase 8) must
  use Jamovi's mechanisms: results `clearWith:` for invalidation, `image$setState()`/
  `image$state` for plot data, and the analysis `state` (tutorial `tuts0203-state`). Drive
  the **same aggregate-core + per-transform subfunctions** (1.4.0 Phase 2) at
  cache-appropriate granularity instead of re-running `tab_many()` wholesale.

---

## 14. How to set up Claude Code to work with Jamovi effectively

The recurring failure mode is editing YAML/JS blind, without a working example or a way to
see the rendered result. The fix is a small, repeatable working method:

1. **Keep `dev/jamovi/reference/` as ground truth.** It holds real, byte-exact `jmv`/`gamlj`/
   `SummaryTables`/client files (see its README). When building a control or handler, open
   the matching vendored file and mirror it — never invent YAML/JS from memory (the docs are
   too thin and the JS API is undocumented). Re-fetch/extend the folder as needed (branches:
   `jmv`/`jamovi` = `main`, `gamlj`/`jamovi-compiler` = `master`).

2. **Grep the two authoritative sources** before trusting a property name: `.u.yaml`
   properties → `dev/jamovi/reference/jamovi-compiler/uicompiler.js`; `.a.yaml` option keys →
   `dev/jamovi/reference/jmvcore/options.R`. These are the compiler/runtime, not prose.

3. **Close the edit→see loop with `F10`.** After `jmvtools::install(home=)` + reloading the
   analysis, open DevTools (F10, blue-bar-focus trick), inspect the options-panel DOM, and
   run `copy(ui.view.el.outerHTML)` to dump the *real* rendered HTML into a file for review.
   This is how you (or Claude Code) verify what a `.u.yaml`/`.js` change actually produced —
   the missing feedback in past attempts. Paste that HTML back into the session when asking
   Claude to reason about layout.

4. **Prefer R over JS where possible.** Detect button clicks in `.b.R` (`if
   (self$options$export)`), not JS resets; compute in R. Reserve `.js` for the two things
   only it can do: (a) reconciling Array rows to selected variables, (b) binding per-row
   `LevelSelector`s. Both are copied from `logregbin.events.js`.

5. **Don't fight the toolchain.** `R/jmvtab.h.R` is generated — change `jamovi/jmvtab.a.yaml`
   and run `jmvtools::prepare()`; never hand-edit `.h.R`. `compilerMode: tame` (already set)
   preserves your `.u.yaml` hand-edits across `.a.yaml` changes — keep it.

6. **A dedicated skill is worth adding** (like the existing `/vctrs-field`, `/color-mode`):
   a `/jamovi-control` skill encoding "to add a per-variable picker: edit `.a.yaml` (Array/
   Group/Level) → `.u.yaml` (ListBox/columns/LevelSelector) → `.js` (updateContrasts/
   updateLevelControls) → `.b.R` (read `self$options$...`) → `prepare()` → `install(home=)` →
   F10-verify", with pointers into `dev/jamovi/reference/`. This encodes the ~5-file
   checklist the same way `/vctrs-field` encodes the record-field checklist. (Proposed, not
   yet created.)

7. **Cloning whole repos** is optional given the vendored subset, but if deeper study is
   needed, clone (ranked): `jamovi/jmv` (the canonical large reference, esp. `jamovi/js/`),
   `jamovi/walrus` (a small complete module), `sbalci/ClinicoPathJamoviModule` (best
   real-world dev guide + troubleshooting), `jamovi/jmvcore` (R6 API contract),
   `jamovi/jamovi-compiler` (compile internals). Prefer grepping the vendored files first.

---

## 15. Vendored reference files index

All under `dev/jamovi/reference/` (`.Rbuildignore`'d via `^dev$`; full annotations in that
folder's `README.md`):

| Folder                          | What it demonstrates                                                                           |
|---------------------------------|------------------------------------------------------------------------------------------------|
| `jmv-logregbin/`                | **Feature 1** — Array/Group/Level + ListBox/LevelSelector + JS row-sync.                       |
| `jmv-anova/`                    | Variant B (ComboBox per var) + rich per-row templates (`emMeans`, `addButton`) + Output/Save.  |
| `jmv-anovarm/`                  | **Feature 2** — ordered-levels Array + `RMAnovaFactorsBox` + templated cells grid.             |
| `jmv-conttables/`               | Contingency tables — closest built-in analog to tabxplor's crosstabs.                          |
| `gamlj/`                        | Variant B in the wild + conditional reveal (`$el.style.display`) + Action-`open` export.       |
| `SummaryTables/`                | **Feature 3** — typed-path Excel/file export + `resolveExportPath()` Windows fixes.            |
| `jamovi-client/`                | Compiled TS: `LevelSelector`, drag-reorder (`gridtargetcontrol`), arrows, `_moveUp/_moveDown`. |
| `jamovi-compiler/uicompiler.js` | Authoritative `.u.yaml` control-property list.                                                 |
| `jmvcore/options.R`             | Authoritative `.a.yaml` option-type R6 contracts.                                              |

---

## 16. Open questions / decisions for tabxplor Phase 8 & 10

- **Where the per-variable `ref` picker lives**: a new "References" ListBox (feature 1)
  populated from `row_vars` (row%/means) or `col_vars` (col%). Keep the free-text `ref`
  TextBox as expert fallback. Decide whether to auto-switch the populate source on `pct`.
- **Level reordering scope**: ship the free drag-reorder of variables first (zero cost);
  defer per-level Array+arrows until there's demand (it's real work with no native control).
- **Export**: replace the current ActionButton/JS-reset/hand-rolled folder check with the
  `SummaryTables` pattern (`resolveExportPath` + `jmvcore::Notice`); optionally add route B.
- **Caching (Phase 8)**: drive the aggregate-core + per-transform subfunctions at
  cache-appropriate granularity via `clearWith:` + analysis `state`, not `tab_many()` end to
  end; reuse the `.fine` aggregate across interactions. Never fork the math from the R API.
- **Do NOT** move off `usesNative`/embedded-in-package layout; it works and matches the CRAN
  build.

---

## 17. Sources

Official: `dev.jamovi.org` (Developer Hub — `/tutorial/tuts01xx`, `/api/*`, `/ui/*`;
`/ui/advanced-customisation` for JS events, `/api/option-action`); legacy mirror
`docs.jamovi.org/_pages/*`. Repos: `github.com/jamovi/{jmvtools, jamovi-compiler, jmvcore,
jmv, jamovi, walrus}`, `github.com/gamlj/gamlj`, `github.com/NourEdinDarwish/SummaryTables`,
`github.com/sbalci/ClinicoPathJamoviModule` (its `articles/module-development-jamovi.html`
and `vignettes/jamovi_*_guide.md`). Forum: `forum.jamovi.org` threads — Array options &
reference levels (`t=4129`), file I/O / no native picker (`p=13515`, `t=132`), sandboxing
(`t=3679`), debugging/F10 (`t=15`), runtime control setting (`t=440`). CRAN: `jmvcore`
(2.6.3), `jmv`. All verbatim code above is mirrored under `dev/jamovi/reference/` and was
fetched 2026-07-08.
