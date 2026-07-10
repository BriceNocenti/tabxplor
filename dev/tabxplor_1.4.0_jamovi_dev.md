# tabxplor 1.4.0 — Jamovi module development technical guide

Written 2026-07-08. The reference for redesigning tabxplor's Jamovi module (1.4.0 Phase 8)
and integrating `tab_logit` (Phase 10). Jamovi module development is a multi-layer stack in
two languages with sparse, recently-reorganised docs; past attempts (manual and AI-assisted)
failed for lack of a mental model and a way to see what the code actually produces at
runtime. This guide fixes both.

It is built on three evidence bases, in increasing order of authority:

1. **Official docs + forum** (`dev.jamovi.org`, `docs.jamovi.org`, `forum.jamovi.org`).
2. **Vendored real-module source** in `dev/jamovi/reference/` (byte-exact `jmv`, `gamlj`,
   `SummaryTables`, jamovi-client files — see that folder's `README.md`).
3. **A live dev-console capture of a running Jamovi with tabxplor 1.3.1 loaded**, in
   `dev/jamovi/dev_console_live_capture/` — the served/compiled module, the minified
   framework bundles (analysis-UI, results-view, main shell), and the rendered app HTML.
   This is the ground truth: it shows the actual runtime architecture, the compiled form of
   our own `.u.yaml`/`.js`, and exactly how our table lands in the DOM. **Sections 5–7 are
   derived from it and supersede the docs where they disagree.** See §17 for the file index.

The three target features:

1. a per-variable **reference-level** picker (the reference of each `row_var` under
   `pct="row"`, of one `col_var` under `pct="col"`) — §12;
2. a **level-reordering** UI for row/col factors — §13;
3. a module-level **Excel export** with a user-friendly path selector — §14.

> How to use this file. First time: read §1–§4 (mental model, toolchain, debugging) then
> §5–§7 (the runtime — what actually happens in the app). Building a feature: §11 (the
> keystone pattern) then §12/§13/§14. Writing YAML/JS: §8/§9/§6 are the reference tables.
> §15 covers sandboxing + Phase-8 caching; §16 is the Claude-Code working method.

---

## 1. Why this is hard: the layer cake and the mental model

A Jamovi module **is a normal R package** with an extra `jamovi/` folder. Jamovi itself is
an Electron desktop app embedding a Python server + an R "engine" process. One analysis is
spread across six files in two languages plus one generated file:

| File                   | Lang   | Role                                                      | Edit?          |
|------------------------|--------|-----------------------------------------------------------|----------------|
| `jamovi/<name>.a.yaml` | YAML   | **Analysis definition** — the options (data model)        | yes            |
| `jamovi/<name>.r.yaml` | YAML   | **Results definition** — tables/plots/html/output slots   | yes            |
| `jamovi/<name>.u.yaml` | YAML   | **UI definition** — the options-panel layout (view)       | yes            |
| `jamovi/js/<name>.js`  | JS     | **Custom UI events** — interactive behaviour (controller) | yes            |
| `R/<name>.b.R`         | R (R6) | **Backend** — `.init()`/`.run()`/`.plot()` analysis body  | yes            |
| `R/<name>.h.R`         | R (R6) | **Generated header** — options + base class               | NO (generated) |
| `jamovi/0000.yaml`     | YAML   | Module **manifest** (analyses, version, min app)          | yes            |

Model–View–Controller:

- `.a.yaml` = **Model** (options; compiles to `R/<name>.h.R`).
- `.u.yaml` = **View** (layout; labels pulled from `.a.yaml`).
- `.js` = **Controller** (reacts to user actions, rewrites option values live).
- `.b.R` = the R analysis: reads `self$options$*`, writes `self$results$*`.

```
you ─▶ jmvtools (R) ─▶ jamovi-compiler / jmc (Node) ─▶ R/<name>.h.R + compiled "uijs" blob
                                                     ─▶ build .jmo ─▶ install into jamovi app
```

Why past edits failed: `.u.yaml`/`.js` changes do nothing until recompiled and reinstalled;
`.h.R` must be regenerated from `.a.yaml` (never hand-edited); and the custom-JS layer is
undocumented enough that you must copy a working module and inspect the running DOM. This
guide supplies both the working examples (§11–§14) and the runtime map (§5–§7).

tabxplor's module is `usesNative: true`, embedded in the R package (`R/jmvtab.b.R` +
`R/jmvtab.h.R` + `jamovi/jmvtab.*`), so it already follows this architecture.

---

## 2. The tabxplor module today (inventory + pain points)

On CRAN as part of tabxplor 1.3.1:

| File                   | Notes                                                                                                                         |
|------------------------|-------------------------------------------------------------------------------------------------------------------------------|
| `jamovi/0000.yaml`     | Manifest: analysis `jmvtab`, `menuGroup: tabxplor`, `minApp: 1.0.8`.                                                          |
| `jamovi/jmvtab.a.yaml` | ~30 options incl. the Excel block (`exportExcel` Action + `xl_path`/`xl_filename`/`xl_replace`).                              |
| `jamovi/jmvtab.r.yaml` | `html_table` (Html), `export_status` (Html), stub `plot` Image.                                                               |
| `jamovi/jmvtab.u.yaml` | `jus:'3.0'`, `compilerMode: tame`; VariableSupplier + CollapseBoxes; export ActionButton + path/filename TextBoxes.           |
| `jamovi/js/jmvtab.js`  | Almost empty: one `exportExcel_changed` (resets the button after 2s); rest is commented-out ANOVA example.                    |
| `R/jmvtab.b.R`         | R6 `.run()`: `tab_many(...,compact=TRUE)` → `tab_kable()` into `html_table`; Excel via `tab_xl()` + hand-rolled folder check. |
| `R/jmvtab.h.R`         | Generated `jmvtabOptions` + `jmvtabBase`. Never edit.                                                                         |

Pain points (all addressed below):

- **`ref`/`ref2` are free-text `TextBox`es** — no per-variable chooser → §12.
- **No level reordering** → §13.
- **Excel export is fragile** — ActionButton + JS reset + hand-rolled `dir.exists()` +
  default `"S:/Documents"` + failed `FilePicker`/`%USERPROFILE%` experiments in comments →
  §14 (solved by `SummaryTables::resolveExportPath()`).
- **Two confirmed footguns from the live capture** (§5.2): the module runs in Jamovi's
  bundled R (4.4.1), not your R 4.5.1 — the root of the `~`/path quirks; and the compiler
  ships JS comments verbatim, so the 295 commented lines in `jmvtab.js` are downloaded by
  every user. Clean `jmvtab.js` before release.

---

## 3. Toolchain and the dev loop

### 3.1 Install (Windows 11)

1. **Jamovi desktop app** (from `jamovi.org`) — the install target and the R engine host.
   Build against the same Jamovi series you run (a `.jmo` is tied to OS + arch + series).
2. **R** + **Rtools** matching it (to build the `.jmo`).
3. **`node`** and **`jmvtools`** from the Jamovi repo:

   ```r
   install.packages('node', repos = 'https://repo.jamovi.org')
   install.packages('jmvtools',
                    repos = c('https://repo.jamovi.org', 'https://cran.r-project.org'))
   ```

   `jmvtools` vendors the Node `jamovi-compiler` (`jmc`); `node` supplies the runtime.

### 3.2 `jmvtools` functions

`create('Name')` scaffold · `addAnalysis(name=,title=)` add an analysis (5 files) ·
`prepare()` compile only (regenerate `.h.R` + UI blob) · `install()` build + install the
`.jmo` · `check()` verify Jamovi is found · `i18nCreate()/i18nUpdate()` catalogs ·
`version()`.

### 3.3 Windows: point jmvtools at Jamovi

Auto-detect fails on Windows — set the home path:

```r
options(jamovi_home = 'C:/Program Files/jamovi/bin')   # adjust to your install
jmvtools::check()                                       # must print a version
```

Or pass `home=` per call, or set `JAMOVI_HOME`.

### 3.4 The cycle

```
edit yaml/js/b.R ─▶ jmvtools::prepare()  (fast, regenerate .h.R + UI blob)
                 ─▶ jmvtools::install()   (build .jmo, install) ─▶ reload analysis in Jamovi
```

For complex modules: `prepare()` then `devtools::document()` twice, then `install()`. UI not
updating → close Jamovi fully, reinstall (Windows `.jmo` file locks).

### 3.5 `jamovi-compiler` (`jmc`)

Shelled out to by jmvtools; flags mirror it (`-p/--prepare`, `-i/--install`, `-c/--check`,
`--home`, `--debug`, `--verbose`, `--i18n --create/--update`). `uicompiler.js` (vendored) is
the authoritative `.u.yaml` property list. **The compiler does not strip JS comments** (§5.2)
and does not minify the module's own `.js` — keep `jamovi/js/*.js` clean.

### 3.6 Distribution

Sideload a `.jmo`: library **+** → **Side-load** → pick the file. Public release: email a
GitHub link to `contact@jamovi.org` (needs an OSI licence).


### 3.7 option names

**Option names must not collide with `jmvcore::Options` members.** The generated options
  class makes one active binding per option name; a name that shadows an inherited member
  (notably `levels`, but also `values`/`check`/`read`/`names`/`options`/`option`/`get`/`has`/
  `analysis`/`theme`/`palette`) fails at analysis creation with "symbol already has a regular
  binding". Use a safe internal name (e.g. `lvs`) and map it to the `tab()` argument in `.b.R`.



---

## 4. Debugging: the dev console + inspecting the real runtime

The capability the previous attempts lacked.

- **F10 = Chrome DevTools** inside Jamovi (Elements/DOM, Console, Sources, Network). Confirmed
  in the shell: `addKeyboardListener("F10", ()=>toggleDevTools())`. Jamovi's UI is **nested
  iframes** (§5.1); if F10 doesn't register, click the top blue bar first, then F10.
- **F9 = restart engines** (confirmed) — clears a wedged R engine.
- **Ribbon toggles**: **Syntax mode** (`id="syntaxMode"`) shows the generated R call for the
  analysis — invaluable for seeing exactly what options produced; **Dev mode**
  (`id="devMode"`) surfaces R stack traces on error.
- **Inspect the real DOM**: the options panel and each result are separate iframes (§5.1).
  In DevTools, drill into the target iframe. To dump the real rendered HTML, select the root
  node → Copy → Copy outerHTML, or in the Console run `copy(ui.view.el.outerHTML)` (options
  panel) / `copy($0.outerHTML)` (results). This is exactly how the
  `dev/jamovi/dev_console_live_capture/` files were produced — the repeatable method.
- **`console.log` from custom JS** appears in that iframe's DevTools Console; you can drive
  the panel as a REPL: `ui.pct.value()`, `ui.ref.setValue('tot')`.
- **R errors**: launch Jamovi from a terminal (engine console visible), enable Dev mode, or
  drop `browser()` into `.run()`; surface progress with `jmvcore::Notice` (§7.6).
- **Compiler errors**: `jmc --debug --verbose`, or read `jmvtools::install()` output.

Loop: edit → `jmvtools::install(home=)` → reload analysis → F10 → inspect DOM/Console → iterate.

---

## 5. Runtime architecture (verified from the live capture)

This is what actually happens when Jamovi runs the module. Evidence: the captured app HTML,
the served module file, and the minified framework bundles.

### 5.1 The iframe / origin / postMessage model

The Jamovi window is one Electron page hosting **sandboxed iframes on localhost ports**
(per-session origins). From the captured `config.js`:
`window.config = {"client":{"roots":["127.0.0.1:56680","127.0.0.1:56683","127.0.0.1:56684"]}}`
— the main instance + two engine/view ports.

- **Options panel** = one iframe:
  `<iframe id="tabxplor-jmvtab" sandbox="allow-scripts allow-same-origin"
   src="http://127.0.0.1:56683/<instanceId>/" class="silky-options-control">`. Rendered by
  the **analysis-UI framework** (`analysisui-*.js`, §6) from the module's compiled `uijs`.
- **Results panel** `#results` holds one `.jmv-results-container[data-analysis-name=...]`
  **per analysis**, each its own iframe:
  `<iframe data-id="2" src="http://127.0.0.1:56684/<instanceId>/2/" class="analysis"
   sandbox="allow-scripts allow-same-origin" scrolling="no">` (jmvtab was `data-id=2`).
  Rendered by the **results-view framework** (`resultsview-*.js`, §7).
- **Addressing**: `http://<origin>/<instanceId>/<analysisId>/` for a result iframe; image
  resources at `<instanceId>/<analysisId>/<revision>/res/<NN name>/resources/<hash>.png`
  (the captured `.../2/res/02 jmvtab/resources/*.png` were the plot placeholders).
- **Sandbox**: both panels are `allow-scripts allow-same-origin`. Scripts CAN run, but each
  iframe is isolated; the only channels are `postMessage` to the host (§5.3) and `openUrl`.
- **Sizing**: parent sets container width/height; iframe `scrolling="no"`; the iframe reports
  its content size back (`postMessage {type:"sizeChanged", data:{width: w+40, height}}`) and
  the panel resizes to it — which is why a wide table pushes the whole panel wide (§7.3).

Implication: feature UIs (§12/§13) live in the **options** iframe (analysis-UI framework);
the table (§7/§14) lives in the **results** iframe. They cannot touch each other's DOM; they
coordinate only through option values via the coms protocol (§5.3).

### 5.2 The served/compiled module format

A module served to the client is **one file** fetched from `../modules/<ns>` — YAML text
parsed by js-yaml, yielding `{options, uijs, i18n, languages}`. In the captured
`modules/tabxplor__v_1.3.1.0` (70 KB):

- Lines 1–459: the manifest — `title/name/version/jms:'1.0'/authors/description`, the
  `analyses:` list with each analysis's **full option definitions** (the `.a.yaml`), then
  `usesNative: true`, `minApp: 1.0.8`, `languages: [fr]`, **`rVersion: 4.4.1-x64`**.
- A key **`uijs:`** whose value is the entire compiled UI as a **browserified UMD JS string**
  (one ~53 KB line): the `.u.yaml` layout compiled to a JS control tree **plus** the events
  `.js`. Layout nodes look like:

  ```js
  { type: DefaultControls.RadioButton, typeName: 'RadioButton',
    name: "pct_1", optionName: "pct", optionPart: "no" }
  // enable compiles to a string:  enable: "(pct:row || pct:col)"
  // an ActionButton event compiles to:
  //   events: [ { execute: require('./jmvtab').exportExcel_changed } ]
  ```

Two load-bearing facts:

- **The module runs in Jamovi's BUNDLED R (`rVersion: 4.4.1-x64`), not your system R 4.5.1.**
  This is the root cause of `path.expand("~")` → Documents, and of package-version drift.
  Always test inside Jamovi, and use `Sys.getenv("USERPROFILE")` for paths (§14).
- **The compiler embeds `.js` comments verbatim** — the whole commented-out ANOVA example +
  failed export experiments (**295 `//` lines**) ship inside the served `uijs` blob to every
  user. Delete dead/commented code from `jamovi/js/jmvtab.js` before release.

### 5.3 The coms protocol + option round-trip + recompute model

Client ↔ engine is a WebSocket at `ws://127.0.0.1:<port>/<instanceId>/coms` carrying
protobuf `ComsMessage` envelopes. The full `.proto` is embedded in the shell bundle
(the authoritative field map). Key messages:

- **`AnalysisRequest`**: `sessionId, instanceId, analysisId, name, ns, Perform perform,
  AnalysisOptions options, repeated string changed, int32 revision, restartEngines,
  clearState, addons, index, path, part, format, i18n, ...`.
  `Perform` enum: `INIT=0, RUN=1, RENDER=4, SAVE=5, DELETE=6, DUPLICATE=7`.
- **`AnalysisResponse`**: `options, ResultsElement results, status, error, final, revision,
  references, title, ...`.
- **`AnalysisOptions`**: options are a `oneof {i, d, s, o(FALSE/TRUE/NONE), c(nested)}` with a
  parallel `names[]` — lossless round-trip of R option values.

The round-trip (client side):

1. A control edit → `analysis.setOptions(values)`. **Change detection gates everything**: an
   edit that doesn't actually change a value bumps nothing and sends nothing (built-in dedup).
2. A real change → `revision++`, then an `AnalysisRequest` with **`perform = INIT (0)`** and
   **`changed: [optionNames]`** is sent. The client does NOT distinguish re-run vs cached —
   **the R engine (jmvcore) decides** what to recompute from the `changed` list.
3. Responses are applied only if `response.revision === current revision` (stale-reply guard).

**Consequence for Phase 8 caching (§15): there is no client-side "display-only, skip the
engine" path.** Every option change — including pure display toggles — does a full INIT
round-trip to R. Any "reuse the numbers, just re-render" optimisation must live in the R
backend, keyed on the `changed` set. The results view also re-renders the whole result tree
on each update (§7), so keeping the emitted content byte-stable when only display options
changed is what avoids visible churn.

Module discovery: sideload `.jmo` via a file picker → a `ModuleRR` command; a
`moduleInstalled` broadcast hot-reloads the module's analyses without an app restart.

---

## 6. The analysis-UI framework (options panel) — authoritative API

From `analysisui-49b1a9ac.js`. This is the real contract behind `.u.yaml` + `jamovi/js/*.js`,
more authoritative than the docs. (Offsets/quotes are in the agent notes; the facts are below.)

### 6.1 The control registry (`DefaultControls`)

A type-name → constructor map. Confirmed control types (what `.u.yaml` `type:` accepts):
`VariableSupplier`, `Supplier`, `TargetLayoutBox`, `VariablesListBox`, `ListBox`, `ComboBox`,
`RadioButton`, `CheckBox`, `TextBox`, `Label`, `VariableLabel`, `TermLabel`, `LevelSelector`,
`LayoutBox`, `CollapseBox`, `ActionButton`, `Output`, `OutputSupplier`, `ModeSelector`,
`CustomControl`, `RMAnovaFactorsBox`.

- **`GridActionButton` is NOT a `.u.yaml` type** (correction to earlier guidance) — row/inline
  buttons use `ActionButton` (grid-based internally) or a `CustomControl`.
- **`TargetListBox` / `VariableTargetListBox` are deprecated** (their constructors just return
  "no longer used").
- A nested **`ListItem`** sub-registry lists the control types commonly used as a per-row
  template: `TextBox`, `ComboBox`, `TermLabel`, `VariableLabel`, `Label`. In practice column
  templates also resolve `LevelSelector` (jmv `logregbin` uses it) and `VariableLabel`.

### 6.2 The control + option-wrapper API

`ui.<controlName>` returns a control object:

- `getPropertyValue(name)` / `setPropertyValue(name, value)` — get/set a property (cannot set
  `name` or `type`). Setting fires a property-changed event.
- `setValue(value, key?, opts?)` — set the bound option value.
- `setEnabled(bool)` — sugar for `setPropertyValue("enable", bool)` (there is no
  `isDisabled`; enabled state is the `enable` property).
- `$el` (jQuery-like element) / `el` (raw DOM node); `getOption()` (the bound Option).

The per-option **wrapper** (value-facing façade) exposes: `getValue(keys)`, `setValue(value,
keys, opts)`, `getLength(keys)`, `insertValueAt(value, keys, opts)`, `removeAt(keys, opts)`,
`setProperty(...)`, `getName()`, `isValidKey(k)`, and **`beginEdit()`/`endEdit()`** (batching
lives on the Option/wrapper, not the control — controls call
`this.getOption().beginEdit()` around drag/value edits). Values are addressed by **keys**
(nested paths), so array/group options are read/written positionally.

### 6.3 Events: names, inheritance, handler signature

Valid event names by capability:

- `OptionControl` (most controls): `changed`, `changing`.
- `OptionListControl` (ListBox, VariablesListBox): `listItemAdded`, `listItemRemoved`
  (+ `preprocess`).
- `CustomControl`: `creating`, `updated`.
- `Supplier`/`VariableSupplier`: `changed`, `updated`.
- root/view (`ui.view`): `loaded`, `updated`, `remoteDataChanged`, `creating`.

`convertEventName`: `changed → change`, `updated → update`. A control's `.u.yaml` `events:`
entries and the module's `.js` handler names are resolved at compile time; at runtime the
framework carries resolved `{onEvent, execute}` arrays and `"<control>.<event>"` listener
strings. A bare `onEvent` without a dot is prefixed with the control name.

**Handler signature (authoritative): `function(ui, ...eventArgs)` with `this` = the events
context.** `ui` is the resources object — every control by `name`, plus `ui.view`. So the
`.js` idioms `view_loaded(ui)`, `factors_changed(ui)`, `onChange_refLevels(ui)` all receive
`ui` first and run with a rich `this` (§6.7). Both naming styles seen in real modules
(`<control>_<event>` and the explicit `events: { change: './name.events::handler' }`) are
compiler conventions that resolve to the same runtime binding.

### 6.4 `LevelSelector` internals (the reference-level widget)

Registers option properties **`variable`** and **`allowNone`**, plus `defaultLevelIndex`. On
update it calls `requestData("column", {columnName: <variable>, properties: ["measureType",
"levels"]})`, then renders one `<option>` per level. Confirmed facts:

- Setting the `variable` property (via `setPropertyValue('variable', name)`) re-fetches and
  repopulates — this is exactly how a per-row picker binds each row to its variable (§12).
- The stored value is a **level label string** (or `null` with `allowNone` → "- None -").
- **Levels are taken verbatim from the column and are NOT reorderable/filterable** by the
  selector; `allowNone` only prepends the none option. It is disabled for `continuous`
  columns. (This is why feature 2 — reordering — cannot be done by a LevelSelector; §13.)

### 6.5 Templated `ListBox`: columns, `applyToItems`, drop behaviour

`ListBox` (`GridOptionListControl`) properties: `columns`, `maxItemCount`,
`showColumnHeaders`, `removeAction` (`deleterow`|`clearcell`), `height`, `addButton`,
`ghostText`, `isTarget`, `stripedRows`, `valueFilter` (`none`|`unique`|`uniquePerRow`|
`uniquePerColumn`). A lone `template:` is sugar auto-wrapped into a single `columns` entry.

Iterating per-row template controls — **corrected signature**:

```js
// applyToItems(startRowIndex[, count], callback(item, rowIndex, columnIndex))
ui.refLevels.applyToItems(0, (item, rowIndex, columnIndex) => {
    if (columnIndex === 1)                       // the 2nd column's template control
        item.setPropertyValue('variable', dlist[rowIndex].var);
});
```

The first arg is a **row index**, not a column (earlier guidance said column — wrong). The
callback's `item` is the per-cell template control (full `get/setPropertyValue`).
`getSelectedRowIndices()` (no args) returns the selection.

Drop behaviour (`isTarget: true`): `itemDropBehaviour` = `insert` (default; **enables
positional drag-reorder**), `emptyspace` (append-only, reorder OFF), `overwrite` (forced when
`maxItemCount` reached). This is the free, no-JS reorder route (§13).

### 6.6 `CustomControl` — the DOM escape hatch (new for feature 2)

`ui.<name>.$el` is a jQuery-wrapped `<div class="silky-custom-control ...">`; `$el[0]` is the
raw node. A **`creating`** handler builds sub-DOM into `$el`; a `MutationObserver` auto-fires
`contentchanged` when `$el` mutates; `updated` fires on data/option change. It inherits
`RequestDataSupport`, so the handler can call
`ui.<name>.requestData("column", {columnName, properties: ["levels"]})` — the same level
fetch `LevelSelector` uses. This is enough to build a **fully custom, drag-sortable /
arrow-button level reorderer** and write the order back to an Array option (§13).

### 6.7 Helper utilities available to `.js`

- Global **`window.utils`**: `checkValue`, `clone`, `sortArraysByLength`, `getCombinations`,
  `getItemCombinations`, `valuesToItems(values, format)`, `itemsToValues(items)`,
  `findDifferences`, `listContains`, `flattenList`.
- The events **context** (`this` in a handler): `workspace` (scratch object, reset on data
  change), `requestData(request, params)`, **`requestAction(action, params)`**,
  **`setCustomVariables`/`setCustomVariable`/`removeCustomVariable`/`clearCustomVariables`**
  (create/modify dataset columns from the UI), `findChanges(name, list, ...)` (diffs vs
  `workspace[name]`), `isReady`, `getContext`.
- **`FormatDef`**: `.variable`, `.string`, `.bool`, `.number`, `.term`, `.infer(x)` — value
  format descriptors used with `valuesToItems`/`checkValue`.

Option sync under the hood: `setOptionValue`/`setPropertyValue` (keys-addressed);
`optionPart` splits one option across several controls (RadioButton/CheckBox
`checkedValue = optionPart`); the iframe→host bridge posts
`onOptionsChanged {properties:{name, key, value}}` (this feeds the coms round-trip in §5.3).

### 6.8 Field-tested gotchas (building a `CustomControl` widget)

Concrete, reusable facts learned building the level-reorder `CustomControl` (verified against the
vendored `jamovi-compiler/uicompiler.js` + `analysisui` CSS + live console). Read these BEFORE building
any custom-JS widget that edits an option.

- **A `CustomControl` NEVER "claims" its backing option.** In the compiler,
  `uiOptionControl.CustomControl.isOptionControl()` returns `false`. `insertMissingControls()` then
  AUTO-GENERATES a default control for every option not claimed by some control — for a nested
  `Array`/`Group` option that default is a `VariableSupplier`+`ListBox` that frequently crashes
  (`GridTargetContainer.getSupplierItems … reading 'isSingleItem'`). Result: your custom UI *and* a
  second broken auto-UI both appear.
- **Suppress the auto-control with `hidden: true` on the OPTION** (`.a.yaml`), not on the control.
  Compiler: `insertMissingControls` does `if (option.hidden) continue;`. `compilerMode: tame` does
  NOT prevent auto-generation (it only changes how the added controls are reported) — `hidden` is the
  reliable lever. The option stays fully functional (present in `.h.R`, readable in R).
- **A hidden, control-less option is still reachable in JS as `ui.<optionName>`** — the per-option
  wrapper (`.value()` / `.setValue()`). So the pattern is: `hidden: true` option + a `CustomControl`
  whose JS reads/writes `ui.<optionName>` and uses `ui.<controlName>.$el` / `.requestData(...)` for DOM
  + data. (`$el` is jQuery-wrapped; `$el[0]` is the raw node.)
- **A control CLAIMS an option when `ctrl.name === option.name` OR `ctrl.optionName === option.name`**
  (only for control types whose `isOptionControl()` is true). Naming a normal control after its option
  is what suppresses its auto-generation.
- **The `updated` event fires on ANY option change in the analysis — including the control's OWN
  `setValue`.** If you rebuild inside `updated`, every edit (and every unrelated toggle) triggers a
  rebuild. Gate it: skip unless the thing you care about (e.g. the selected-variable signature) changed
  AND your DOM subtree is still present (tag your root with a `data-*` marker; if it's gone, jamovi
  re-rendered `$el` and you must re-render).
- **BUT `updated` is NOT reliable for reacting to OTHER options** (Phase 7g-iii): a bare CustomControl
  (one that does not claim an option) did not re-render when a *different* option (`OR`) changed, so a
  section keyed on that option (the ref2 picker) never appeared. The robust pattern is to wire an
  explicit `events: { change: ./mod::handler }` on the controls whose value your widget reads (each
  `RadioButton` of the `pct`/`OR`/`color` groups here) → the handler re-renders the widget. `change`
  fires immediately with the fresh value; treat `updated` only as the self-`setValue` skip-gate. The
  variable boxes already work this way (`change: onChange_vars`).
- **Async `requestData` + a deferred swap RACES user input.** Building a fragment in
  `Promise.all(requestData…).then(swap into $el)` and swapping it in later will clobber a synchronous
  in-place edit the user made in between (the swapped-in snapshot was read before the edit). Symptom:
  "the 1st click shows, the 2nd does nothing, then all edits appear at once later." Fix: **cache the
  fetched column data** so re-renders are SYNCHRONOUS (placeholder + one-shot fetch that caches then
  re-renders), and do fine-grained edits in place without a full async rebuild.
- **`setValue` may store the array BY REFERENCE.** Pass a copy (`arr.slice()`), never your live working
  array — otherwise a later in-place mutation aliases the stored option value and `setValue` can miss
  the change (no `onOptionsChanged`).
- **Read a column's factor levels** with
  `ui.<ctrl>.requestData('column', {columnName, properties:['measureType','levels']})` (exactly what
  `LevelSelector` does). Returns `{measureType, levels}`; each level has `.label`; `measureType ===
  'continuous'` marks a numeric column (no levels).
- **`<details>`/`<summary>` work in the Electron/Chromium option panel** as native collapsibles. Set
  `summary { display:block; list-style:none }` to drop the native triangle and supply your own caret;
  they RESET to their default open state on every rebuild, so persist open/closed in a keyed map updated
  from the `toggle` event.
- **Match jamovi's own colors**: the list-selection blue is `#b5caef` (`.selected` in analysisui css);
  tab-selection tint `#3e6da92b`; hover/drop `#0000001a`; list header text `#555`. Reuse these so a
  custom selectable list reads as native.
- **Keyboard**: give rows/lists `tabindex=0`, handle `keydown` and `preventDefault()` on
  `ArrowUp`/`ArrowDown` (else the panel scrolls); set inner `<button>`s `tabindex=-1` so TAB focus stays
  on the row. A "select one item, then Up/Down moves the selection (it follows)" model beats per-row
  buttons for reordering long lists.
- **`GridActionButton` is not a valid `.u.yaml` `type:`** — use `ActionButton`, or DOM buttons inside a
  `CustomControl`.

### 6.9 Greying controls: declarative `enable:` vs imperative `setEnabled` (Phase 7h)

The Phase 7h consistency pass greys out every control that is a no-op given the other options,
mirroring the resolver (`tab_resolve_settings()` + the leaves). Two mechanisms, one rule of thumb:

- **Value-based greying → DECLARATIVE `enable:` in `.u.yaml`.** jamovi re-evaluates the expression on
  every option change automatically (no `.js`, no wiring). Forms: `(pct:row || pct:col)`, `(chi2)`,
  the negation `(!(color:no))`, compound `&&`/`||`, and the JS-arrow `({return ...;})`. Shipped in
  jmvtab: `color_signif` policies `(!(color:no))`, `stars` `(ci:diff || ci:auto)`, `add_n`/`add_pct`
  `(pct:row || pct:col)`.
- **What the DSL can't see → IMPERATIVE `ui.<ctrl>.setEnabled(bool)`.** The `enable:` grammar keys off
  option VALUES, not the LENGTH of a `Variables` array (an empty array is truthy) nor a column's
  `measureType`. To grey `totaltab`/`comp` when `tab_vars` is empty, `applyVarEnables(ui)` in
  `js/jmvtab.js` `setEnabled`s them on `tab_vars.value().length > 0`, re-run from the root `update`
  AND from `onChange_vars` (both fire on every variable change). `setEnabled` is sugar for
  `setPropertyValue("enable", …)`; there is no separate hide.
- **Never put BOTH on the same control** — a declarative `enable:` is re-evaluated by jamovi and would
  override an imperative `setEnabled`. Pick one per control (jmvtab: declarative for value-based,
  imperative-only for the tab_vars-length ones).
- **Disabling keeps the value** (grey ≠ unset): jamovi still sends a disabled control's value to R, and
  it returns intact when the control re-enables. So do NOT also `setValue` a neutral default — rely on
  the backend forcing the neutral behaviour internally (e.g. `tab()` forces `totaltab="no"` with no
  tab_vars). Silently changing/reverting a user's field is a UX antipattern.
- **CI coupling is a re-paint, NOT a toggle** (Phase 7h decision): `color_signif` does NOT set `ci` from
  `.js`. The backend already computes the CI the policy gates (`ci="auto"` → diff CI for factors;
  `jmvtab_build` nudges numeric means, `R/jmvtab-cache.R` ~L714-727). Reflecting it in `stars`/
  `method_diff` enables is enough; auto-toggling `ci` would be redundant and could overwrite a
  deliberate `ci="cell"`.
- **Column-type-aware greying is deliberately NOT done** (would need async `requestData`/`measureType`
  in the enable path). Consequence: `color="diff"`/`"ratio"` stay pct-greyed on a pure-means table;
  `color="auto"` (always enabled) covers colouring means, so no user is blocked. A follow-up could move
  those enables to imperative `.js` reading the cached `measureType`.
- **TextBox `width:` has no `auto` in the 2.6.44 COMPILER.** The uicompiler schema enum is only
  `small | normal | large | largest` (the runtime bundle lists `auto`/`smallest`, but they fail
  `jmvtools::prepare()`/`install()` with `<opt>.width is not one of enum values`). `largest` caps at
  200px. To make a text box fill its (stretchFactor) cell, clear the fixed-width `silky-option-<size>-text`
  cap in `.js`: widen the control root + wrappers down to the `input` to `width:100%` (helper
  `stretchTextBox(ui, name)`, re-applied in `onUpdate` because jamovi re-renders may drop inline styles).
  `ui.<textbox>.$input[0]` is the raw input; `.$el[0]` the control root.

---

## 7. How Jamovi renders RESULTS (the results iframe) — critical for exporters

From `resultsview-60a5863d.js` + `resultsview-88266f06.css`. This governs how tabxplor's
`tab_kable()` HTML actually appears, and constrains Phase 7 (exporters) and Phase 8.

The results view runs inside the per-analysis iframe (§5.1) and receives the whole results
definition from the host via `postMessage {type:"results"}`; on each update it **re-renders
the entire result tree** (no incremental diffing at this layer). It auto-sizes to its content
(`sizeChanged` = content width + 40 px).

### 7.1 HTML result injection

An `Html` result element carries `content` (HTML string), `stylesheets` (filenames), and
`scripts` (filenames). Rendering:

- **`stylesheets`** → fetched from `module/<file>` and appended to the iframe `<head>` as
  `<style class="module-asset">` — apply reliably.
- **`scripts`** → appended to `<head>` as `<script src="module/<file>">` — load and execute
  reliably. **This is the only reliable JS channel.**
- **`content`** → injected via the DOM lib's `.html()` into `.jmv-results-html .content`.
  **No iframe/srcdoc and no shadow DOM** wrap a result.

### 7.2 What runs and what does not (decisive for tab_kable)

- **Inline `<style>` inside `content` WORKS** — tabxplor's inlined CSS renders.
- **Inline `<script>` inside `content` almost certainly does NOT execute** — the DOM lib is
  jQuery-like but lacks jQuery's script-eval internals, and no Bootstrap/jQuery is present in
  the iframe. So **kableExtra JS tooltips are inert**, and **kableExtra Bootstrap classes
  silently no-op** (Bootstrap CSS isn't loaded). Only tabxplor's own inline rules bite.
- **No style isolation**: styles go into the shared iframe `<head>`. An over-broad selector
  (bare `table {}`) can restyle Jamovi's own DOM in that iframe — **scope every rule under a
  unique wrapper** (e.g. `#tabxplor-tbl table {}`).
- **`<a href>` links are hijacked** → routed to the host `openUrl` (opens the OS browser).
  Anchor-based in-page interactivity will not work.

Actionable: emit **CSS-only, self-contained, wrapper-scoped** styling; drop JS-dependent
tooltips (or convert to `title=`/`:hover`); if interactivity is essential, ship it as a module
`scripts` asset on the Html element, not inline.

### 7.3 Width and scrolling (the biggest real problem)

`.jmv-results-html { width: 500px }` is a **fixed 500 px** container, and `.content` has **no
`overflow`**. A wide table overflows and, because the iframe auto-sizes to content, widens the
whole analysis iframe → the results panel scrolls horizontally.

- tabxplor currently uses `kableExtra::scroll_box(width = "1080px")` — this forces the iframe
  ~1080 px wide and triggers panel-level horizontal scroll (ugly).
- Fix: wrap the table in tabxplor's **own `overflow-x: auto` container** sized to fit
  (`width: 100%` → resolves to the 500 px box, or an explicit `max-width`), so the table
  scrolls **inside its box** and the iframe reports a bounded width. Do not rely on the host
  to clip — `.content` doesn't.

### 7.4 Images / plots

An `Image` result is a `<div>` with `background-image: url('res/<path>')` and explicit px
`width`/`height` from the element (no `<img>`). `path` resolves relative to the iframe base
`<instanceId>/<analysisId>/<revision>/` (new revision → fresh URL). **No client-side HiDPI
scaling** — the R side decides pixel size via `renderFun`/`setSize`; emit at 2× if you want
crisp retina plots.

### 7.5 Export / copy of results (host-driven)

Per-element context menu: `Copy`, `Export...`, `Add Note` (groups add `Duplicate`).
Selections `postMessage` to the host; the actual copy/export is done by the client + engine —
**there is no module-callable export hook, no `toDataURL`/`saveAs`/clipboard in the results
bundle**, and the native context menu is disabled. App-level export formats (from the shell):
**results → PDF / PNG / HTML / LaTeX-zip only** (NOT xlsx); **the dataset** can export to xlsx
etc., but that is app-chrome-driven. "Copy" grabs the rendered DOM as-is → keep the emitted
HTML self-contained and paste-clean.

### 7.6 Notices (`jmvcore::Notice`)

Numeric `type` → class: **1 = warning-1, 2 = warning-2, 3 = info, 4 = error**. `content` gets
a light markdown-bold transform (`**x**` → `<strong>x</strong>`) then `.html()`; links are
rebound to the host. Use a Notice for the export success/error message (cleaner than the
current hand-built `export_status` HTML div).

---

## 8. `.a.yaml` option types (the data model)

Each `type:` compiles to a `jmvcore` R6 class (`dev/jamovi/reference/jmvcore/options.R` is the
source of truth). Common keys: `name` (→ `self$options$<name>`), `title`, `type`, `default`,
docs-only `description:`.

| `type:`            | UI                      | Key type-specific keys                                                  | Value in R              |
|--------------------|-------------------------|-------------------------------------------------------------------------|-------------------------|
| `Data`             | (dataset)               | —                                                                       | data frame              |
| `Bool`             | CheckBox                | `default`                                                               | logical                 |
| `Integer`/`Number` | TextBox `format:number` | `min`, `max`, `default`                                                 | int/numeric             |
| `String`           | TextBox                 | `default`                                                               | character               |
| `List`             | ComboBox/RadioButton    | `options:` (`name`+`title`), `default`                                  | one `name`              |
| `NMXList`          | CheckBox set            | `options:`, `default`                                                   | character vector        |
| `Variable`         | VariablesListBox slot   | `suggested`, `permitted`, `required`, `rejectInf`(F)                    | column name             |
| `Variables`        | VariablesListBox        | as `Variable` (`rejectInf` T)                                           | character vector        |
| `Level`            | LevelSelector/ComboBox  | (variable pairing via UI/JS)                                            | one level string        |
| `Terms`            | Supplier                | `default`                                                               | list of terms           |
| `Pairs`            | 2-col ListBox           | `suggested`, `permitted`                                                | list of `{i1,i2}`       |
| `Group`            | fixed bundle            | `elements:`                                                             | named list              |
| `Array`            | templated ListBox       | `template:` (usually a Group), `default`                                | list of clones          |
| `Action`           | ActionButton            | `action` (default `open`)                                               | logical (TRUE on click) |
| `Output`           | Output (Save)           | a.yaml minimal; r.yaml `varTitle`/`measureType`/`clearWith`/`initInRun` | logical                 |

Not option types: `Ncrementer` (a UI control backed by Integer/Number); `clearWith` (a
`.r.yaml` key); there is no `Value` type. `suggested`/`permitted` measure types:
`continuous`, `ordinal`, `nominal`, `nominaltext`, `id`, `numeric`, `factor`.

---

## 9. `.u.yaml` control catalog (the view)

Root keys: `title`, `name`, `jus` (**`'3.0'`** for JS events), `stage`, `compilerMode`
(`aggressive` regenerates layout on `.a.yaml` change; **`tame` preserves hand-edits** —
tabxplor uses `tame`), then `children:`. §6 is the authoritative runtime behaviour; this is
the authoring surface.

Common `BaseControl` properties: `type`, `name`, `label`, `enable` (boolean DSL), `events`,
`margin`, `cell` (`{row,column}`), `stretchFactor`, `style` (`list`|`inline`), alignments,
`min/maxWidth`, `min/maxHeight`, `children`.

| Control             | Purpose                      | Key properties                                                                                                                   |
|---------------------|------------------------------|----------------------------------------------------------------------------------------------------------------------------------|
| `VariableSupplier`  | variable pool                | `suggested`, `permitted`, `populate`, `persistentItems`                                                                          |
| `Supplier`          | term/model pool              | `format: term`, `higherOrders`, `persistentItems`                                                                                |
| `TargetLayoutBox`   | wraps a drop target          | `label`, `transferAction`                                                                                                        |
| `VariablesListBox`  | variable drop target         | `isTarget`, `maxItemCount`, `ghostText`, `valueFilter`, `height`                                                                 |
| `ListBox`           | templated list               | `columns`/`template`, `showColumnHeaders`, `isTarget`, `itemDropBehaviour`, `addButton`, `valueFilter`, `maxItemCount`, `events` |
| `LevelSelector`     | pick a level                 | bound to a `Level` option; dynamic (§6.4)                                                                                        |
| `ComboBox`          | dropdown (List)              | `name`, `enable`                                                                                                                 |
| `RadioButton`       | one List value               | `optionName`, `optionPart`, `children`                                                                                           |
| `CheckBox`          | Bool / NMXList part          | `optionName`, `optionPart`, `children`                                                                                           |
| `TextBox`           | String/Integer/Number        | `format`, `suffix`, `inputPattern`, `width`, `ghostText`                                                                         |
| `Label`             | text / group header          | `label`, `format`, `style`, `children`                                                                                           |
| `LayoutBox`         | grid/stack container         | `margin`, `cell`, `stretchFactor`, `style`                                                                                       |
| `CollapseBox`       | collapsible section          | `label`, `collapsed`, `enable`                                                                                                   |
| `ActionButton`      | button (incl. row buttons)   | `name`, `events`, `enable`                                                                                                       |
| `Output`            | write a column back          | `name`                                                                                                                           |
| `CustomControl`     | JS-built DOM (§6.6)          | `creating`/`updated` events                                                                                                      |
| `RMAnovaFactorsBox` | RM factors editor (compiled) | `name`, `label`                                                                                                                  |

`enable:` DSL: `(pct:row || pct:col)`, `(OR == 'OR' || OR == 'OR_pct')`, `(!(missing:no))`,
or a JS arrow `({return !!ui['x'].value();})`.

`template:` (one control per row) vs `columns:` (a record per row — each column has `name`
mapping to the array item / Group `elements[].name`, `label`, `selectable`, `stretchFactor`,
`maxWidth`, `template`). See §11.

---

## 10. `.r.yaml` results + `clearWith`

Result element types: `Table`, `Image`, `Group`, `Array`, `Preformatted`, `Html`, `Notice`,
`Output`. tabxplor uses `Html` (`html_table`, `export_status`) + a stub `Image`.

`clearWith:` (per results element) lists **option names**; when any changes, Jamovi marks that
result stale. This is the declarative invalidation hook for Phase 8 — but note (§5.3) the
engine still gets a full INIT on every change, so `clearWith` controls *result reuse*, not
whether R runs. `Output` element keys: `varTitle`, `varDescription`, `measureType`,
`clearWith`, `initInRun`; R side: `setValues()`, `setRowNums()`, `isFilled()`, `setKeys()`.

---

## 11. The keystone pattern: Array-of-Group + templated ListBox + JS row-sync

Features 1 and 2 are instances of one pattern:

1. **`.a.yaml`**: an `Array` whose `template:` is a `Group` of `elements:` — a variable
   (`type: Variable`) + a per-variable choice (`type: Level` for a real level, or `type: List`
   for a fixed enum).
2. **`.u.yaml`**: a `ListBox` bound by `name:`, with `columns:` mapping to the Group's
   `elements` — a `VariableLabel` column + a `LevelSelector`/`ComboBox` column — plus
   `events: { change: ... }`.
3. **`.js`**: (a) **reconcile rows** — one row per selected variable, preserving prior choices
   (`updateContrasts`); (b) **bind each row's picker** to its variable
   (`updateLevelControls`, only for the dynamic `LevelSelector`). Run both from
   `view`/`update` AND from the relevant `onChange_<vars>`.

> Modern-Jamovi fact: the old declarative row-sync keys `items:(factors)` and `content:$key`
> are **legacy no-ops** — row population is entirely JS-driven. GAMLj still carries them
> cosmetically; don't rely on them.

Vendored live examples: `dev/jamovi/reference/jmv-logregbin/` (Level + LevelSelector) and
`dev/jamovi/reference/jmv-anova/` (List + ComboBox).

---

## 12. Feature 1 — per-variable reference-level picker

Goal: under `pct="row"`/means, choose the reference row (level) of each `row_var`; under
`pct="col"`, the reference column of the chosen `col_var`. Maps onto the 1.4.0 decision that
`ref` becomes a per-row_var named vector. The widget is jmv's binomial-logistic `refLevels`
(vendored: `dev/jamovi/reference/jmv-logregbin/`).

> **REBUILT (Phase 7g-iii, 2026-07-10) — §12.1–12.3 below are the superseded first design.** The
> built-in `ListBox` + `LevelSelector` had no "Total" choice, showed jamovi's natural level order
> (ignoring the reorder panel), synced only `row_vars`, and read whitish (not Material). It is
> replaced by a **`CustomControl` `refPickerCtrl`** (sibling of `levelOrderCtrl`, sharing its
> `levelsCache`/`requestData`/`storedOrder`/`TABX` styles — `jamovi/js/jmvtab.js`):
> - **One compact Material LINE per active-axis** variable (row_vars under pct row/means, col_vars
>   under pct="col"): a **bold variable name + a native `<select>` drop-down** showing the current
>   reference level (`[Total, …levels in the reordered order…]`). Iteration 1 used a radio list; the
>   drop-down (Iteration 2) is far more compact and the name/level distinction is clearer. Numeric
>   col_vars show "numeric — vs its total" (no drop-down).
> - Stored **by label** in `refLevels` (`ref` element retyped `Level → String`, so it also holds
>   `"tot"`; `refLevels`/`ref`/`ref2` are `hidden: true`, the CustomControl is the sole UI). The
>   effective auto-default (Total, or the first level under OR) is highlighted when unset.
> - A **ref2 section** (the OR 2nd reference, over the OTHER axis + First/Total) renders only when
>   OR is active. `.b.R` filters `refLevels` to the active axis, then `jmvtab_ref_vector()` keys it
>   by that axis and `tab_setup()` dispatches (row ref vs per-col_var col ref).
> - Re-renders on **explicit `change` events** wired on the `pct`/`OR`/`color` radios
>   (`onChange_refopts`) and the variable boxes (`onChange_vars`) — a bare CustomControl does NOT get
>   a reliable `updated` for other options' changes (this is why the ref2 section first failed to
>   appear on `OR`; see §6.8). `refPickerCtrl_updated` is only the self-`setValue` skip-gate (a
>   reference pick is an in-place drop-down change; a level reorder re-orders the lists while the
>   by-label selection is preserved).
> - **Two jamovi-UI gotchas re-confirmed** (see §6.8): a `CustomControl` needs `hidden: true` on
>   its backing option (else a broken default control is auto-generated); and never mix celled and
>   cell-less children in one `LayoutBox`. The reference **label is matched by exact equality** in
>   `diff_index()` (not regex) so metacharacter labels work AND the stored `ref` attribute stays
>   human-readable in the colour legend (no `^…$` token). **Backend:** per-col_var col% references
>   via a `ref_vect` threaded into the factor leaf; `detect_refcol()` keeps the diff-CI reference
>   column consistent. `.h.R` regen + live-verify is the maintainer's closing step.

### 12.1 `.a.yaml`

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

`self$options$refLevels` → `list(list(var="gender", ref="female"), ...)`.

### 12.2 `.u.yaml`

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
          template: { type: VariableLabel }
        - name: ref
          label: Reference Level
          selectable: false
          stretchFactor: 0.5
          template: { type: LevelSelector, label: '' }
```

`LevelSelector` fetches its variable's levels via `requestData` and stores the chosen **level
label string** — which fits tabxplor's `ref` (a level name / regex) semantics directly (§6.4).

### 12.3 `.js` (verbatim from `logregbin.events.js`, with the corrected `applyToItems`)

> **jus 3.0 PITFALL (2026-07-10, confirmed live).** `logregbin` is `jus: '2.0'`, where the events
> `this`/`context` carries `.clone`. tabxplor is **`jus: '3.0'`**, where it does NOT — use the
> **global `utils.clone(...)`** instead (verified against `jmv-anova`, also jus 3.0, which uses
> `utils.clone` and `view_updated`). Copying `context.clone` from the jus-2.0 snippet below makes the
> root `update` handler **throw on panel load → the options panel hangs on an infinite spinner with
> no error**. Also bind the root view update as both `update` (explicit `events: update:`) and
> `view_updated` (the jus-3.0 naming-convention alias) so initial sync fires regardless. See
> `jamovi/js/jmvtab.js` for the fixed, jus-3.0 version.
>
> **LAYOUT PITFALL (same session).** A jamovi `LayoutBox`/`CollapseBox` must NOT mix children that
> set `cell: {column, row}` with children that don't — a cell-less child is auto-placed onto an
> already-claimed cell → **`Uncaught (in promise) Cell already exists`** (another silent infinite
> spinner). Dropping the `refLevels` `ListBox` (cell-less) beside the celled `ref`/`comp`/`ref2`
> boxes triggered it. Fix: put the cell-less control and the celled grid in **separate** wrapper
> LayoutBoxes. A quick validator (walk the compiled `.u.yaml`, flag any parent whose children mix
> celled/cell-less or reuse a `{column,row}`) catches this before install.

```js
const events = {
    update:               function(ui) { calcModelTerms(ui, this); updateLevelControls(ui, this); },
    onChange_row_vars:    function(ui) { calcModelTerms(ui, this); },
    onChange_col_vars:    function(ui) { calcModelTerms(ui, this); },
    onChange_refLevels:   function(ui) { updateLevelControls(ui, this); },
};
var calcModelTerms = function(ui, context) {
    var variableList = context.clone(ui.row_vars.value(), []);   // + col_vars when pct="col"
    updateContrasts(ui, variableList, context);
};
var updateContrasts = function(ui, variableList, context) {      // one {var,ref} row per variable
    var currentList = context.clone(ui.refLevels.value(), []);
    var list3 = [];
    for (let i = 0; i < variableList.length; i++) {
        let found = null;
        for (let j = 0; j < currentList.length; j++)
            if (currentList[j].var === variableList[i]) { found = currentList[j]; break; }
        list3.push(found !== null ? found : { var: variableList[i], ref: null });
    }
    ui.refLevels.setValue(list3);
};
var updateLevelControls = function(ui, context) {                // bind each row's LevelSelector
    let dlist = ui.refLevels.value();
    ui.refLevels.applyToItems(0, (item, rowIndex, columnIndex) => {   // NB: (startRow, cb(item,row,col))
        if (columnIndex === 1) item.setPropertyValue('variable', dlist[rowIndex].var);
    });
};
module.exports = events;
```

### 12.4 `.b.R`

```r
ref_named <- purrr::map_chr(self$options$refLevels, "ref") |>
  rlang::set_names(purrr::map_chr(self$options$refLevels, "var"))
# feed ref_named into tab_many(ref = ...) as the per-row_var named vector (decisions §4)
```

Keep the free-text `ref` TextBox as an expert fallback (`"auto"`/`"tot"`/`"first"`/regex).
Under `pct="col"`, populate from `col_vars` and message that only one applies. `ref2` (empirical
OR, Phase 10) can reuse the same widget.

---

## 13. Feature 2 — level reordering

**BUILT (Phase 7g-ii, 2026-07-10) — route §13.2 (`CustomControl`) + §13.4 (`fct_relevel`).** `levelOrderCtrl`
(js/jmvtab.js) is a **2-level collapsible `<details>` tree** — axis (open, left-indented) >
`"<var> : N levels - reorder"` (collapsed; one click opens the list) — Material grey tints + borders + ▸/▾
carets, in its own collapsed CollapseBox before "References". The list is a **jamovi-style selectable list**
(white box, selection = jamovi's `#b5caef`): click a level to select it (first selected by default), then an
**Up/Down button pair below the list** or the **Up/Down arrow keys** (list focused) move the selected level
(it stays selected so repeated moves walk it). It reads levels via
`requestData('column', {properties:['measureType','levels']})`, builds into a **detached fragment swapped in
atomically**, and writes the order to the `levelOrder` Array option (`{var, levels}` per reordered var). A
**variable-signature gate** makes the frequent `updated` event a no-op unless the variable set changed (keeps
focus + open sections; collapse state persists). **Two gotchas learned the hard way** (both from live test):
(1) a `CustomControl` never *claims* its option, so the compiler auto-generates a second broken default control
— set **`hidden: true`** on the `levelOrder` option (uicompiler skips hidden options) so this control is the
sole UI; (2) the option is still reachable as `ui.levelOrder` (the per-option wrapper, §6.2) even when hidden.
R side is **internal-only** (no public `tab()` arg): `jmvtab_levels_order()` → the internal
`tab(.levels_order=)` arg → **`jmv_cache_aggregate()` relevels the shaped aggregate POST-fetch** (`jmv_relevel_cols`;
stored blob stays raw) + recomputes `remove_levels` for `levels="first"`, so a reorder is a **tier-3 input**
(tiers 1-2 reused), byte-identical to `tab()` on pre-releveled microdata. The routes below are the original
design analysis (kept for context).

**Reality check (confirmed against the framework):** there is **no ready-made drag-sortable
factor-level control** at module level, and `LevelSelector` takes levels verbatim (§6.4). The
achievable routes, in preference order:

### 13.1 Free drag-reorder of the chosen variables (no JS)

A `ListBox`/`VariablesListBox` with `isTarget: true` and the default `itemDropBehaviour:
insert` already supports positional drag-reorder (§6.5). tabxplor's `row_vars`/`col_vars`/
`tab_vars` suppliers already reorder by drag — the order the user drags IS the order. Nothing
to build.

### 13.2 A per-level ordered list — `CustomControl` (recommended, the "real custom JS" route)

This is the genuinely custom-JS route (§6.6) and the best fit for "reorder the levels of a
factor". Build a sortable list in the control's `$el`:

```yaml
# .a.yaml : ordered levels per variable
- name: levelOrder
  type: Array
  default:
  template:
    type: Group
    elements:
      - { name: var,    type: Variable }
      - { name: levels, type: Array, template: { type: String } }   # element order = display order
```

```yaml
# .u.yaml
- type: CustomControl
  name: levelOrderCtrl
  events:
    creating: './jmvtab.events::levelOrderCtrl_creating'
    updated:  './jmvtab.events::levelOrderCtrl_updated'
```

```js
// .js : build a drag/arrow list in $el; requestData for levels; write order back to the option
levelOrderCtrl_creating: function(ui, event) {
    this._build = () => {
        let $el = ui.levelOrderCtrl.$el;
        let vars = ui.row_vars.value() || [];
        $el.empty();
        vars.forEach(v => {
            ui.levelOrderCtrl.requestData('column', { columnName: v, properties: ['levels'] })
              .then(col => {
                  // render col.levels as a reorderable <ul> (drag handles or ▲▼ buttons);
                  // on reorder, write back: splice + ui.levelOrder.setValue(updatedArray)
              });
        });
    };
    this._build();
},
levelOrderCtrl_updated: function(ui) { /* re-read row_vars, rebuild if changed */ },
```

The `MutationObserver` on `$el` auto-emits `contentchanged`; write the reordered array via the
option wrapper (`ui.levelOrder.setValue(...)` or `insertValueAt`/`removeAt` with keys, §6.2).
Row/column button clicks are just DOM handlers you attach in `_build` — full control over
"the behaviour of the buttons", which is what earlier attempts could not achieve.

### 13.3 Arrow buttons via `ActionButton` (not `GridActionButton`)

If you prefer declared controls over `CustomControl`, a paired ordered `ListBox` + up/down
`ActionButton`s (NOT `GridActionButton` — that's not a `.u.yaml` type, §6.1) with a JS splice
handler works:

```js
onChange_moveUp: function(ui) {
    let arr = this.clone(ui.levelOrder.value(), []);
    let sel = ui.levelOrder.getSelectedRowIndices();
    if (!sel.length || sel[0] === 0) return;
    let i = sel[0], item = arr.splice(i, 1)[0];
    arr.splice(i - 1, 0, item);                // down: splice(i + 1, 0, item)
    ui.levelOrder.setValue(arr);
}
```

### 13.4 `.b.R`

Apply the order with `forcats::fct_relevel()` per variable before `tab_many()`. Recommendation:
ship §13.1 (free) now; add §13.2 (`CustomControl`) when per-level control is actually needed.

---

## 14. Feature 3 — Excel export with a user-friendly path selector

**Confirmed against the shell + results bundles:** there is **no file/folder-picker control**
for a module, **no module-callable Save-As dialog**, and (in the captured Jamovi 2.6.44) **no
`Action`-option `perform`/open-dataset mechanism**. App-level export does xlsx for the
*dataset* only; results export is PDF/PNG/HTML/LaTeX. So a module `.xlsx` must be written by
the R engine to a path the user provides as a string. The best implementation is
`SummaryTables` (vendored: `dev/jamovi/reference/SummaryTables/`).

### 14.1 `.a.yaml` / `.u.yaml`

```yaml
# .a.yaml
- { name: path,   title: Path, type: String, default: ~/Desktop/Table.xlsx }
- { name: export, title: Save, type: Action }        # read as a boolean click in R
```

```yaml
# .u.yaml
- type: CollapseBox
  label: Export to Excel
  collapsed: true
  children:
    - { type: TextBox, name: path, format: string, stretchFactor: 1 }
    - { type: ActionButton, name: export }
```

The `TextBox` bound to `path` IS the picker. No JS reset needed (the click is a boolean read
in R; §5.3 change-detection handles re-fire).

### 14.2 `.b.R` — detect the click, write, report via Notice

```r
if (self$options$export) {
    p <- resolveExportPath(self$options$path)            # §14.3
    tab_xl(tabs, path = p, sheets = "unique", open = FALSE, replace = self$options$xl_replace)
    n <- jmvcore::Notice$new(options = self$options, name = "exportOK",
                             type = jmvcore::NoticeType$INFO)   # type 3 = info (§7.6)
    n$setContent(paste0("Saved to: ", p))
    self$results$insert(1, n)
}
```

This replaces the current ActionButton-JS-reset + hand-rolled folder check + `export_status`
HTML div.

### 14.3 `resolveExportPath()` — copy verbatim (the Windows fixes)

Full source: `dev/jamovi/reference/SummaryTables/export.R`. Core (adapt `.docx` → `.xlsx`):

```r
resolveExportPath <- function(path) {
  path <- trimws(path)
  path <- gsub("^[\"']|[\"']$", "", path)                # strip Windows "Copy as path" quotes
  if (nchar(path) == 0 || path %in% c("~", "~/")) path <- "~/Desktop/Table.xlsx"
  getHome <- function() { h <- Sys.getenv("USERPROFILE"); if (h == "") h <- Sys.getenv("HOME"); h }
  if (grepl("^~", path)) path <- paste0(getHome(), substring(path, 2))   # NOT sub() (backref bug)
  if (!grepl("[/\\\\]", path)) path <- file.path(getHome(), "Desktop", path)
  if (!grepl("\\.xlsx$", path, ignore.case = TRUE)) path <- paste0(path, ".xlsx")
  normalizePath(path, mustWork = FALSE)
}
```

Why `USERPROFILE` and not `~`: the module runs in Jamovi's bundled R (§5.2), where
`path.expand("~")` resolves to Documents — the root cause of tabxplor's `xl_path` hacks.

### 14.4 The zero-code user route, and what NOT to attempt

- **Zero-code**: the user can already get tabxplor tables out via Jamovi's **File ▸ Export →
  HTML** (whole results to HTML) or right-click a result → **Export...** (PDF/PNG/HTML). Worth
  documenting; keep the emitted HTML self-contained so "Copy" and HTML-export are clean (§7.2).
- **Do NOT** try to raise a native save dialog from the analysis (no hook exists), rely on
  `Action` `open`/`option$perform` (that is a newer Jamovi ≥ 2.7.12 feature — absent in the
  captured 2.6.44; gate with `is.null(option$perform)` if ever used), or serve the file via
  `openUrl` (backend-fragile). The typed-path + engine-write is the only robust route.

---

## 15. Sandboxing, recompute, and Phase 8 caching

Constraints to design around, now grounded in the protocol (§5.3) and results model (§7):

- **No native picker; no module save dialog; results export ≠ xlsx** (§14).
- **Sandbox**: options/results iframes are `allow-scripts allow-same-origin`; the only exits
  are `postMessage` to the host and `openUrl`. A **results element can post `setOption` back**
  to change an analysis option (a real callback channel) — but tabxplor's HTML table can't
  easily use it (inline JS is inert, §7.2), so keep interactivity in the options panel.
- **The module runs in bundled R** (§5.2); **R engine state resets between runs** — never rely
  on R globals for cross-run state.
- **Every option change is a full `perform=INIT` round-trip** carrying `changed:[names]` +
  `revision` (§5.3). There is **no client-side display-only shortcut.** Therefore Phase 8
  caching must be **R-side**:
  + In `.b.R`, branch on *what changed*. jmvcore exposes changed options
    (`self$options$changed` / the `changed` list); when only display options changed
    (`display`, `digits`, `wrap_*`, `ci_print`, colours), **reuse a cached aggregate** and only
    re-render — drive the 1.4.0 aggregate-core + per-transform subfunctions at
    cache-appropriate granularity (never fork the math).
  + Persist the cache in Jamovi `state` (`image$setState()`/`$state`, analysis `state`), keyed
    on the aggregate-defining options, not R globals.
  + Declare true dependencies with `clearWith:` in `.r.yaml` so untouched results are reused.
  + Because the results view re-renders the whole tree per update (§7), keep the emitted HTML
    byte-stable when inputs are unchanged to avoid visible flping/reflow.
- **Table HTML**: emit CSS-only, wrapper-scoped, with an own `overflow-x:auto` box sized to
  fit (not `scroll_box(1080px)`); assume no Bootstrap/jQuery; drop JS tooltips (§7.2–§7.3).

---

## 16. How to set up Claude Code to work with Jamovi

The failure mode is editing YAML/JS blind. The working method:

1. **Ground truth is local.** `dev/jamovi/reference/` holds byte-exact real-module source;
   `dev/jamovi/dev_console_live_capture/` holds the live runtime (compiled module, framework
   bundles, rendered HTML). Mirror these; never invent YAML/JS from memory.
2. **The framework bundles are searchable, not readable.** They are minified (one ~50–325 k
   char line each). Grep for **string literals** (control names, method names, event names,
   CSS classes, protocol keys) with small context; never `cat` them. §5–§7 already distilled
   the load-bearing facts; re-grep only to confirm a new detail.
3. **Close the loop with F10 + capture.** After `jmvtools::install(home=)` + reload, open
   DevTools (F10), inspect the target iframe, and `copy($0.outerHTML)` / `copy(ui.view.el
   .outerHTML)` to dump the real DOM into a file — the exact method that produced the capture
   folder. Re-capture after a UI change to verify what compiled.
4. **Prefer R over JS.** Detect clicks in `.b.R`; compute in R. Reserve `.js` for what only it
   can do: row-reconcile Array options to selected variables, bind per-row `LevelSelector`s,
   and `CustomControl` DOM (§6.6). All copied from `logregbin.events.js`.
5. **Respect the toolchain.** `.h.R` is generated (edit `.a.yaml` → `prepare()`); `compilerMode:
   tame` preserves `.u.yaml` hand-edits; delete commented `.js` (it ships, §5.2).
6. **A `/jamovi-control` skill** (like `/vctrs-field`) would encode the per-feature checklist:
   `.a.yaml` (Array/Group/Level) → `.u.yaml` (ListBox/columns/LevelSelector or CustomControl) →
   `.js` (updateContrasts/updateLevelControls or a $el builder) → `.b.R` (read `self$options`)
   →`prepare()` → `install(home=)` → F10-verify, with pointers into both `dev/jamovi/`
   folders. Proposed, not yet created.

---

## 17. Reference material index

### `dev/jamovi/reference/` — vendored real-module source (annotated in its README)

`jmv-logregbin/` (feature 1: Array/Group/Level + ListBox/LevelSelector + row-sync JS) ·
`jmv-anova/` (ComboBox-per-var + rich templates) · `jmv-anovarm/` (ordered-levels Array +
RMAnovaFactorsBox) · `jmv-conttables/` (crosstab analog) · `gamlj/` (contrasts + conditional
reveal + Action-open) · `SummaryTables/` (feature 3 export + `resolveExportPath`) ·
`jamovi-client/` (compiled TS controls) · `jamovi-compiler/uicompiler.js` (.u.yaml properties)
· `jmvcore/options.R` (.a.yaml option contracts).

### `dev/jamovi/dev_console_live_capture/` — the live runtime capture

| Path                                                                       | What it is                                                                           | Used in       |
|----------------------------------------------------------------------------|--------------------------------------------------------------------------------------|---------------|
| `Jamovi_tabxplor_1_3_1_basic_table.html`                                   | The rendered app window (outer DOM: iframes, ports, sandbox, sizing, ribbon toggles) | §5.1, §4      |
| `127.0.0.1_56680_MAIN_ELECTRON/assets/main-fd7ff1c3.js`                    | The app shell: coms protocol, module load, action system, save dialogs, F10/F9       | §5.3, §14, §4 |
| `127.0.0.1_56680_MAIN_ELECTRON/modules/tabxplor__v_1.3.1.0`                | **The served/compiled tabxplor module** (manifest + `uijs` blob)                     | §5.2          |
| `.../modules/jmv__v_2.6.44.0`, others                                      | Other served modules (compare)                                                       | —             |
| `127.0.0.1_56683_..._analysis_UI/assets/analysisui-49b1a9ac.js` (+ `.css`) | The options-panel control framework                                                  | §6            |
| `127.0.0.1_56684_results/assets/resultsview-60a5863d.js` (+ `.css`)        | The results renderer                                                                 | §7            |
| `127.0.0.1_56684_results/aa145378.../2/res/02 jmvtab/resources/*.png`      | tabxplor's plot resources (addressing example)                                       | §5.1, §7.4    |

Method to refresh the capture: run Jamovi with tabxplor, add a crosstab, F10 → DevTools →
Sources/Network → save the analysis-UI, results, and main-electron origins; save the page
HTML. (The `.zip`s in the folder are the raw exports.)

---

## 18. Open questions / decisions for Phase 8 & 10

- **Integer col_vars become factors (Phase 7e, RESOLVED in the module).** jamovi delivers a
  variable to `self$data` per its **measureType**: `Continuous` -> numeric, `Nominal`/`Ordinal`
  -> factor. An integer column (e.g. `tvhours`) usually imports as Nominal/Ordinal, so it arrives
  ALREADY factored (levels `"0".."24"`) and `tab()` would make one column per value instead of a
  mean -- diverging from plain R, where an integer/double col_var is a mean. `jmvtab_build()` fixes
  this with `jmv_coerce_numeric_cols()`: a col_var that is numeric, or a factor whose levels ALL
  parse as numbers, is coerced back to numeric -> a mean column (row/tab vars untouched). CAVEAT: a
  genuinely categorical numeric CODE (e.g. `region` 1-5) also becomes a mean -- relabel such levels
  to non-numeric text, or set the variable Continuous, to control it. The root cause is jamovi's
  measureType (it does not preserve R integer type across `.rds` import); a cleaner long-term fix
  would read the original `dataType` attribute if jmvcore exposes it per column.

- **Ref picker (§12)**: a "References" ListBox populated from `row_vars` (row%/means) or
  `col_vars` (col%); keep the free-text `ref` as expert fallback; decide whether to auto-switch
  the source on `pct`.
- **Reordering (§13)**: ship free drag-reorder of variables first; add a `CustomControl`
  per-level reorderer only when demanded.
- **Export (§14)**: adopt the `SummaryTables` typed-path + `resolveExportPath` + `Notice`
  pattern; drop the JS reset and hand-rolled folder check.
- **Table HTML (§7)**: rework the `tab_kable` output for Jamovi — CSS-only, wrapper-scoped, own
  `overflow-x:auto`, no `scroll_box(1080px)`, no JS tooltips, no Bootstrap dependence. This is
  a Phase 7 exporter item, informed here.
- **Caching (§15)**: R-side reuse keyed on `changed`; `state` for the aggregate; `clearWith:`
  for dependencies. Never fork the math from the aggregate-core.
- **Cleanup**: strip the 295 commented lines from `jamovi/js/jmvtab.js` (they ship, §5.2).
- **Do NOT** move off `usesNative`/embedded layout; it works and matches the CRAN build.

---

## 19. Sources

Official: `dev.jamovi.org` (`/tutorial/tuts01xx`, `/api/*`, `/ui/*`,
`/ui/advanced-customisation`, `/api/option-action`); legacy `docs.jamovi.org/_pages/*`. Repos:
`github.com/jamovi/{jmvtools, jamovi-compiler, jmvcore, jmv, jamovi, walrus}`,
`github.com/gamlj/gamlj`, `github.com/NourEdinDarwish/SummaryTables`,
`github.com/sbalci/ClinicoPathJamoviModule`. Forum: Array options/reference levels (`t=4129`),
file I/O / no picker (`p=13515`, `t=132`), sandboxing (`t=3679`), debugging/F10 (`t=15`),
runtime control setting (`t=440`). **Live capture: `dev/jamovi/dev_console_live_capture/`
(Jamovi 2.6.44.0, tabxplor 1.3.1, bundled R 4.4.1-x64), analysed 2026-07-08** — the authority
for §5–§7. Vendored verbatim source: `dev/jamovi/reference/`.
