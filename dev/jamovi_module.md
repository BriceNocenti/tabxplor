# The jamovi module — development guide

> PURPOSE: how tabxplor's two point-and-click analyses are built, and how the app they run inside
> actually behaves.
> ROLE: the only document here about a **foreign** system. Sections 4–7 are read out of jamovi's own
> shipped bundle and out of a dev-console capture of a running app, so they say things the docs do
> not and occasionally contradict them; the rest is the toolchain, the file formats and three worked
> recipes for the patterns that are hard to get right. It serves `R/jmvtab*.R`, `R/jmvtabreg*.R`,
> `jamovi/*.yaml` and `jamovi/js/*.js`.
> KEY CONSTRAINTS:
>   - ⚠ **Never hand-edit `R/jmvtab*.h.R`.** They are compiler output. A hand-mirrored `.h.R` was
>     carried across seven commits and was wrong in one place the compiler then fixed by itself.
>   - The runtime facts here were captured on jamovi 2.6.44 and re-read on 2.7.36. The
>     iframe / coms / results model is stable across the series; treat **version-specific details**
>     (an `rVersion` stamp, a uicompiler enum) as provisional and ask the maintainer for a fresh
>     capture when a version-specific answer is needed.
>   - A jamovi option is **named after the producer argument it drives** — exactly, or
>     `<argument>_<slot>` where several options fold into one — or it is a declared exception with
>     its reason. So the backend is a pass-through, not a translation table.
> See: `CLAUDE.md § tabxplor architecture` (jamovi) and its jamovi-module-development section (the
> installed versions, the two build paths, the environment traps) · `dev/jamovi/` (the vendored
> reference modules and the live capture) · `dev/build_jmo_windows.R` (the Windows build).

Three evidence bases, in increasing order of authority:

1. **Official docs and forum** — `dev.jamovi.org`, `docs.jamovi.org`, `forum.jamovi.org`.
2. **Vendored real-module source** in `dev/jamovi/reference/` — byte-exact `jmv`, `gamlj`,
   `SummaryTables` and jamovi-client files, annotated in that folder's `README.md`.
3. **A live dev-console capture of a running jamovi** with tabxplor loaded, in
   `dev/jamovi/dev_console_live_capture/` — the served/compiled module, the minified framework
   bundles (analysis-UI, results-view, main shell) and the rendered app HTML. This is the ground
   truth, and **§§4–7 are derived from it and supersede the docs where they disagree**. §16 indexes
   the files.

> **How to read this.** First time: §1–§3 (the mental model, the toolchain, how to see what is
> really happening), then §4–§7 (what the app does at runtime). Building a feature: §11 (the
> keystone pattern) then one of §12–§14. Writing YAML or JS: §5, §8, §9 are the reference tables.

---

## 1. The layer cake, and the mental model it needs

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
guide supplies both the working examples (§11–§14) and the runtime map (§4–§6).

tabxplor's module is `usesNative: true`, embedded in the R package (`R/jmvtab.b.R` +
`R/jmvtab.h.R` + `jamovi/jmvtab.*`), so it already follows this architecture.

---

## 2. Toolchain and the dev loop

### 2.1 Install

⚠ **A `.jmo` is tied to OS + arch + jamovi series, so there are two build paths and they are not
interchangeable.** Edit source in **one place only** — WSL — and build the Windows bundle from a
pulled copy. The installed versions, the two checkouts, the pinned `jmvtools`, the required
freedesktop SDK and the two environment traps that will otherwise waste a day
(`ELECTRON_RUN_AS_NODE`, a version-pinned `R_LIBS_USER` in `~/.Renviron`) are all stated in
**`CLAUDE.md` § Jamovi module development**, which owns them. Read that before installing anything.

⚙ **The Windows build is scripted**: `Rscript dev/build_jmo_windows.R`, run on Windows. It clones
the current branch into a **throwaway temp folder** (not the `D:\` checkout), pins `jmvtools`,
installs the dependencies, `Sys.unsetenv`s `ELECTRON_RUN_AS_NODE`, runs `jmvtools::install()` and
verifies the landed module. Branch and jamovi home are overridable by CLI argument or by
`TABXPLOR_BRANCH` / `JAMOVI_HOME`.

⚠ `jmvtools::check()` passes even when the build cannot work — it never reaches Electron — so **a
green `check()` proves nothing** about the two environment traps.

### 2.2 `jmvtools` functions

`create('Name')` scaffold · `addAnalysis(name=,title=)` add an analysis (5 files) ·
`prepare()` compile only (regenerate `.h.R` + UI blob) · `install()` build + install the
`.jmo` · `check()` verify Jamovi is found · `i18nCreate()/i18nUpdate()` catalogs ·
`version()`.

### 2.3 Point jmvtools at jamovi

**WSL / flatpak (the dev path)** — pass `home='flatpak'`; `jmc` then shells out to
`flatpak run org.jamovi.jamovi` with **no hardcoded paths** (`installer.js`), so a `--user`
install is found fine:

```r
jmvtools::check(home = 'flatpak')     # -> "jamovi 2.7.36 found at /usr/bin/flatpak"
jmvtools::install(home = 'flatpak')
```

**Windows (release builds only)** — auto-detect fails, so set the home path:

```r
options(jamovi_home = 'C:/Program Files/jamovi/bin')   # adjust to your install
jmvtools::check()                                       # must print a version
```

Or pass `home=` per call, or set `JAMOVI_HOME`.

### 2.4 The cycle

```
edit yaml/js/b.R ─▶ jmvtools::prepare()  (fast, regenerate .h.R + UI blob)
                 ─▶ jmvtools::install()   (build .jmo, install) ─▶ reload analysis in Jamovi
```

For complex modules: `prepare()` then `devtools::document()` twice, then `install()` — the
`.h.R` carries the roxygen `@param` blocks that feed `man/jmvtab.Rd`. UI not updating → close
jamovi fully, reinstall (this was a Windows `.jmo` file-lock issue; unverified on flatpak).

⚠ **Never hand-edit `R/jmvtab*.h.R`, even to keep them in sync with the YAML.** One was hand-mirrored
across seven commits; when `prepare()` was finally run the compiler reproduced **778 of its 780
lines** but corrected `exportExcel` (`type: Action`) from `NULL` → `FALSE` **and supplied a default
it lacked** — without which `tabxplor::jmvtab()` called from R throws at the pass-through. The mirror
was *nearly* right and still shipped a latent bug, on the newest option.

**Build cost, measured on the WSL flatpak: `install()` ≈ 2 min.** jamovi's bundled R serves
**binaries** for most of tabxplor's dep tree; only a few (e.g. `openxlsx2`) compile, via the SDK's
g++ 14.3.0. Verify the install by mechanism, not by the "Module installed successfully" message:

```bash
grep -E '^version:|^rVersion:' ~/.jamovi/modules/tabxplor/jamovi.yaml   # rVersion == jamovi --r-version
ls -l ~/.jamovi/modules/tabxplor/ui/jmvtab.js                           # the compiled uijs blob
```

### 2.5 `jamovi-compiler` (`jmc`)

Shelled out to by jmvtools; flags mirror it (`-p/--prepare`, `-i/--install`, `-c/--check`,
`--home`, `--debug`, `--verbose`, `--i18n --create/--update`). `uicompiler.js` (vendored) is
the authoritative `.u.yaml` property list. **The compiler does not strip JS comments** (§4.2)
and does not minify the module's own `.js` — keep `jamovi/js/*.js` clean.

### 2.6 Distribution

Sideload a `.jmo`: library **+** → **Side-load** → pick the file. Public release: email a
GitHub link to `contact@jamovi.org` (needs an OSI licence).


### 2.7 option names

**Option names must not collide with `jmvcore::Options` members.** The generated options
  class makes one active binding per option name; a name that shadows an inherited member
  (notably `levels`, but also `values`/`check`/`read`/`names`/`options`/`option`/`get`/`has`/
  `analysis`/`theme`/`palette`) fails at analysis creation with "symbol already has a regular
  binding". Use a safe internal name (e.g. `lvs`) and map it to the `tab()` argument in `.b.R`.

**One msgid carries one translation, so two panels cannot spell an option differently.** `_()` is
  keyed on the English string across the whole module, and `msgfmt` refuses a catalogue holding the
  same msgid twice --- which silently breaks the *entire* French UI, not just that label. Where two
  panels mean different things by the same word, change the ENGLISH (`ref = <i>(reference)</i>` in
  `jmvtab` vs `ref = <i>(reference profile)</i>` in `jmvtabreg`), never the `.po`.

**`ci_method = "profile"` is uncached by design and the button says so.** A profile interval is an
  output of the likelihood at one confidence level, so it is the one quantity a fit digest cannot
  rebuild (`reg_crude_cacheable()`); every option change refits every model, serially.

**`jmvtools::prepare()` DELETES `inst/i18n/fr.json` without rebuilding it** --- only a full
  `jmvtools::install()` compiles `jamovi/i18n/fr.po` into it. Restore the file (or run `install()`)
  before committing, or the module ships with no French at all.



---

## 3. Debugging: the dev console, and seeing the real runtime

The capability the previous attempts lacked.

- **F10 = Chrome DevTools** inside Jamovi (Elements/DOM, Console, Sources, Network). Confirmed
  in the shell: `addKeyboardListener("F10", ()=>toggleDevTools())`. Jamovi's UI is **nested
  iframes** (§4.1); if F10 doesn't register, click the top blue bar first, then F10.
- **F9 = restart engines** (confirmed) — clears a wedged R engine.
- **Ribbon toggles**: **Syntax mode** (`id="syntaxMode"`) shows the generated R call for the
  analysis — invaluable for seeing exactly what options produced; **Dev mode**
  (`id="devMode"`) surfaces R stack traces on error.
- **Inspect the real DOM**: the options panel and each result are separate iframes (§4.1).
  In DevTools, drill into the target iframe. To dump the real rendered HTML, select the root
  node → Copy → Copy outerHTML, or in the Console run `copy(ui.view.el.outerHTML)` (options
  panel) / `copy($0.outerHTML)` (results). This is exactly how the
  `dev/jamovi/dev_console_live_capture/` files were produced — the repeatable method.
- **`console.log` from custom JS** appears in that iframe's DevTools Console; you can drive
  the panel as a REPL: `ui.pct.value()`, `ui.ref.setValue('tot')`.
- **R errors**: launch Jamovi from a terminal (engine console visible), enable Dev mode, or
  drop `browser()` into `.run()`; surface progress with `jmvcore::Notice` (§6.6).
- **Compiler errors**: `jmc --debug --verbose`, or read `jmvtools::install()` output.

Loop: edit → `jmvtools::install(home=)` → reload analysis → F10 → inspect DOM/Console → iterate.

---

## 4. Runtime architecture, read off the live capture

This is what actually happens when Jamovi runs the module. Evidence: the captured app HTML,
the served module file, and the minified framework bundles.

### 4.1 The iframe / origin / postMessage model

The Jamovi window is one Electron page hosting **sandboxed iframes on localhost ports**
(per-session origins). From the captured `config.js`:
`window.config = {"client":{"roots":["127.0.0.1:56680","127.0.0.1:56683","127.0.0.1:56684"]}}`
— the main instance + two engine/view ports.

- **Options panel** = one iframe:
  `<iframe id="tabxplor-jmvtab" sandbox="allow-scripts allow-same-origin"
   src="http://127.0.0.1:56683/<instanceId>/" class="silky-options-control">`. Rendered by
  the **analysis-UI framework** (`analysisui-*.js`, §5) from the module's compiled `uijs`.
- **Results panel** `#results` holds one `.jmv-results-container[data-analysis-name=...]`
  **per analysis**, each its own iframe:
  `<iframe data-id="2" src="http://127.0.0.1:56684/<instanceId>/2/" class="analysis"
   sandbox="allow-scripts allow-same-origin" scrolling="no">` (jmvtab was `data-id=2`).
  Rendered by the **results-view framework** (`resultsview-*.js`, §6).
- **Addressing**: `http://<origin>/<instanceId>/<analysisId>/` for a result iframe; image
  resources at `<instanceId>/<analysisId>/<revision>/res/<NN name>/resources/<hash>.png`
  (the captured `.../2/res/02 jmvtab/resources/*.png` were the plot placeholders).
- **Sandbox**: both panels are `allow-scripts allow-same-origin`. Scripts CAN run, but each
  iframe is isolated; the only channels are `postMessage` to the host (§4.3) and `openUrl`.
- **Sizing**: parent sets container width/height; iframe `scrolling="no"`; the iframe reports
  its content size back (`postMessage {type:"sizeChanged", data:{width: w+40, height}}`) and
  the panel resizes to it — which is why a wide table pushes the whole panel wide (§6.3).

Implication: feature UIs (§12/§13) live in the **options** iframe (analysis-UI framework);
the table (§6/§14) lives in the **results** iframe. They cannot touch each other's DOM; they
coordinate only through option values via the coms protocol (§4.3).

### 4.2 The served/compiled module format

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

- **The module runs in Jamovi's BUNDLED R, not your system R.** The stamp is read from the target
  jamovi at build time, so it is self-consistent by construction — and it **differs per build path**:
  **`4.5.0-x64`** on the WSL flatpak (jamovi 2.7.36) vs **`4.4.1-x64`** in the captured Windows
  2.6.44 (below). Your system R (4.6.1 in WSL) is irrelevant to module compatibility. This is the
  root cause of `path.expand("~")` → Documents, and of package-version drift. Always test inside
  jamovi, and use `Sys.getenv("USERPROFILE")` for paths (§14).
- **The compiler embeds `.js` comments verbatim** — the whole commented-out ANOVA example +
  failed export experiments (**295 `//` lines**) ship inside the served `uijs` blob to every
  user. Delete dead/commented code from `jamovi/js/jmvtab.js` before release.

### 4.3 The coms protocol + option round-trip + recompute model

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

⚠ **Consequence for the live-UI cache: there is no client-side "display-only, skip the
engine" path.** Every option change — including pure display toggles — does a full INIT
round-trip to R. Any "reuse the numbers, just re-render" optimisation must live in the R
backend, keyed on the `changed` set. The results view also re-renders the whole result tree
on each update (§6), so keeping the emitted content byte-stable when only display options
changed is what avoids visible churn.

Module discovery: sideload `.jmo` via a file picker → a `ModuleRR` command; a
`moduleInstalled` broadcast hot-reloads the module's analyses without an app restart.

---

## 5. The analysis-UI framework (the options panel)

From `analysisui-49b1a9ac.js`. This is the real contract behind `.u.yaml` + `jamovi/js/*.js`,
more authoritative than the docs. (Offsets/quotes are in the agent notes; the facts are below.)

### 5.1 The control registry (`DefaultControls`)

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

### 5.2 The control + option-wrapper API

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

### 5.3 Events: names, inheritance, handler signature

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
`ui` first and run with a rich `this` (§5.7). Both naming styles seen in real modules
(`<control>_<event>` and the explicit `events: { change: './name.events::handler' }`) are
compiler conventions that resolve to the same runtime binding.

### 5.4 `LevelSelector` internals (the reference-level widget)

Registers option properties **`variable`** and **`allowNone`**, plus `defaultLevelIndex`. On
update it calls `requestData("column", {columnName: <variable>, properties: ["measureType",
"levels"]})`, then renders one `<option>` per level. Confirmed facts:

- Setting the `variable` property (via `setPropertyValue('variable', name)`) re-fetches and
  repopulates — this is exactly how a per-row picker binds each row to its variable (§12).
- The stored value is a **level label string** (or `null` with `allowNone` → "- None -").
- **Levels are taken verbatim from the column and are NOT reorderable/filterable** by the
  selector; `allowNone` only prepends the none option. It is disabled for `continuous`
  columns. (This is why feature 2 — reordering — cannot be done by a LevelSelector; §13.)

### 5.5 Templated `ListBox`: columns, `applyToItems`, drop behaviour

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

### 5.6 `CustomControl` — the DOM escape hatch (new for feature 2)

`ui.<name>.$el` is a jQuery-wrapped `<div class="silky-custom-control ...">`; `$el[0]` is the
raw node. A **`creating`** handler builds sub-DOM into `$el`; a `MutationObserver` auto-fires
`contentchanged` when `$el` mutates; `updated` fires on data/option change. It inherits
`RequestDataSupport`, so the handler can call
`ui.<name>.requestData("column", {columnName, properties: ["levels"]})` — the same level
fetch `LevelSelector` uses. This is enough to build a **fully custom, drag-sortable /
arrow-button level reorderer** and write the order back to an Array option (§13).

### 5.7 Helper utilities available to `.js`

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
`onOptionsChanged {properties:{name, key, value}}` (this feeds the coms round-trip in §4.3).

### 5.8 Field-tested gotchas (building a `CustomControl` widget)

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
- **BUT `updated` is NOT reliable for reacting to OTHER options**: a bare CustomControl
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

### 5.9 Greying controls: declarative `enable:` vs imperative `setEnabled`

Every control that is a no-op given the other options is greyed out,
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
- **CI coupling is a re-paint, NOT a toggle**: `color_signif` does NOT set `ci` from
  `.js`. The backend already computes the CI the policy gates (`ci="auto"` → diff CI for factors;
  `jmvtab_build` nudges numeric means, `R/jmvtab-cache.R` ~L714-727). Reflecting it in `stars`/
  `method_diff` enables is enough; auto-toggling `ci` would be redundant and could overwrite a
  deliberate `ci="cell"`.
- **Column-type-aware greying is deliberately NOT done** (would need async `requestData`/`measureType`
  in the enable path). Consequence: `color="diff"`/`"ratio"` stay pct-greyed on a pure-means table;
  `color="auto"` (always enabled) covers colouring means, so no user is blocked. A follow-up could move
  those enables to imperative `.js` reading the cached `measureType`.
- **TextBox `width:` has no `auto` in the 2.6.44 COMPILER.** ⚠ Measured against the **2.6.44**
  bundled compiler; the WSL path now builds with **jmvtools 2.7.26**, and whether its uicompiler
  widened this enum is **untested** — re-check before assuming the workaround is still needed.
  The uicompiler schema enum is only
  `small | normal | large | largest` (the runtime bundle lists `auto`/`smallest`, but they fail
  `jmvtools::prepare()`/`install()` with `<opt>.width is not one of enum values`). `largest` caps at
  200px. To make a text box fill its (stretchFactor) cell, clear the fixed-width `silky-option-<size>-text`
  cap in `.js`: widen the control root + wrappers down to the `input` to `width:100%` (helper
  `stretchTextBox(ui, name)`, re-applied in `onUpdate` because jamovi re-renders may drop inline styles).
  `ui.<textbox>.$input[0]` is the raw input; `.$el[0]` the control root.

---

## 6. How jamovi renders results — the exporters' constraint

From `resultsview-60a5863d.js` + `resultsview-88266f06.css`. This governs how tabxplor's
rendered HTML actually appears, and it constrains both exporters and the live-UI cache.

The results view runs inside the per-analysis iframe (§4.1) and receives the whole results
definition from the host via `postMessage {type:"results"}`; on each update it **re-renders
the entire result tree** (no incremental diffing at this layer). It auto-sizes to its content
(`sizeChanged` = content width + 40 px).

### 6.1 HTML result injection

An `Html` result element carries `content` (HTML string), `stylesheets` (filenames), and
`scripts` (filenames). Rendering:

- **`stylesheets`** → fetched from `module/<file>` and appended to the iframe `<head>` as
  `<style class="module-asset">` — apply reliably.
- **`scripts`** → appended to `<head>` as `<script src="module/<file>">` — load and execute
  reliably. **This is the only reliable JS channel.**
- **`content`** → injected via the DOM lib's `.html()` into `.jmv-results-html .content`.
  **No iframe/srcdoc and no shadow DOM** wrap a result.

### 6.2 What runs and what does not (decisive for tab_kable)

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

### 6.3 Width and scrolling

**Full analysis, with every rule quoted from the shipped bundle: section 7 below. Read
that before touching anything about width.** The short version:

- the results iframe reports `#results.getBoundingClientRect().width + 40`, and the app obeys it with
  a **620 px floor and no ceiling**; the iframe is `scrolling="no"`, so anything past that is clipped;
- `#results` and every `.jmv-results-item` hug their content — **except an Html result, which
  jamovi's own stylesheet pins at `.jmv-results-html{width:500px}`**, one rule after the
  `width:max-content` it gives every other item;
- a definite width contributes exactly itself and an overflowing descendant contributes nothing, so
  a table inside an `overflow-x:auto` box never reached the host: every tabxplor result was reported
  at 588 px, clamped to 620, and cut there — the scroll box's own scrollbar with it.

tabxplor therefore emits one `<style>` (from `jmv_results_content()`, `R/jmvtab-export.R`) that
un-pins `.jmv-results-html`, hugs the table (`width:max-content`, no display cap — the results panel
is `overflow:scroll` and scrolls exactly as it does for jamovi's own wide tables), and caps prose with
`tx-note` so a hint's one-line max-content cannot size the panel.

⚠ A **visible `Image`** result also sets the width (an explicit px width is definite and in-flow) —
that is why the old `plot` Image "worked" — but it costs vertical space, which is the whole reason the
state carriers stay `visible:false`. Never use one as a width mechanism.

### 6.4 Images / plots

An `Image` result is a `<div>` with `background-image: url('res/<path>')` and explicit px
`width`/`height` from the element (no `<img>`). `path` resolves relative to the iframe base
`<instanceId>/<analysisId>/<revision>/` (new revision → fresh URL). **No client-side HiDPI
scaling** — the R side decides pixel size via `renderFun`/`setSize`; emit at 2× if you want
crisp retina plots.

### 6.5 Export / copy of results (host-driven)

Per-element context menu: `Copy`, `Export...`, `Add Note` (groups add `Duplicate`).
Selections `postMessage` to the host; the actual copy/export is done by the client + engine —
**there is no module-callable export hook, no `toDataURL`/`saveAs`/clipboard in the results
bundle**, and the native context menu is disabled. App-level export formats (from the shell):
**results → PDF / PNG / HTML / LaTeX-zip only** (NOT xlsx); **the dataset** can export to xlsx
etc., but that is app-chrome-driven. "Copy" grabs the rendered DOM as-is → keep the emitted
HTML self-contained and paste-clean.

### 6.6 Notices (`jmvcore::Notice`)

Numeric `type` → class: **1 = warning-1, 2 = warning-2, 3 = info, 4 = error**. `content` gets
a light markdown-bold transform (`**x**` → `<strong>x</strong>`) then `.html()`; links are
rebound to the host. Use a Notice for the export success/error message (cleaner than the
current hand-built `export_status` HTML div).

---

## 7. How wide a result may be

Every rule below was read out of jamovi's shipped client bundle, not inferred. It matters because a
crosstab is the widest thing this app is ever asked to render, and the default chain silently clips
it.

### 7.1 The chain, in four facts

**(a) The results iframe reports its own content width to the app** — `resultsview-*.js`:

```js
_reallyNotifyResize() {
  let e = this.$results[0].getBoundingClientRect(), t = e.width + 40, n = e.height;
  this.mainWindow.postMessage({ type: "sizeChanged", data: { width: t, height: n, ... } }, "*")
}
```

`$results` is `<div id="results">`, and a resize detector re-fires this on any layout change.

**(b) The app obeys it, with a floor and NO ceiling** — `main-*.js`:

```js
case "sizeChanged":
  let u = r.height, f = r.width;
  u < 20 && (u = 20);
  f < 620 && (f = 620);            // ← the only clamp: a 620 px MINIMUM
```

The iframe is `scrolling="no"`, so whatever exceeds the reported width is **clipped, not
scrollable**. The results *panel* is `overflow:scroll`, so a wide iframe makes the panel scroll
horizontally — jamovi's own behaviour for wide content, not a failure mode.

**(c) `#results` hugs its content** — `resultsview-*.css`:

```css
#results { display:inline-block; padding-inline-start:12px; padding-inline-end:12px; box-sizing:border-box }
.jmv-results-item { display:inline-block; margin-inline-end:24px; width:max-content; ... }
```

⚠ Note `.jmv-results-item{width:max-content}` — **jamovi already intends result items to hug their
content.**

**(d) …except an Html result, pinned** — the same stylesheet, **one rule later**, so it wins on
source order at equal specificity:

```css
.jmv-results-html { width:500px }
```

### 7.2 Why that clips a table, and why an Image appears to fix it

An element with a **definite** width contributes exactly that width to its ancestors' intrinsic
sizing, and an overflowing descendant contributes nothing. So a 1 400 px table inside a scroll box
inside a 500 px Html item reports 500 + 24 (its margin) + 24 (`#results` padding) + 40 = **588 px**,
which is below the floor — and every result comes out **exactly 620 px wide, whatever the table**.
The scroll box is laid out at its true width and clipped at the iframe edge; its own horizontal
scrollbar sits at that true right edge, i.e. off-screen too. Hence the classic symptom: *cut at the
right before the scroll box appears, and the scroll box itself cut when it appears.*

⚠ **A visible `Image` result "fixes" the width by accident**, because an image is a definite-width,
in-flow element and therefore *is* counted. It is not a width mechanism worth keeping: it reserves
vertical space, since a rendering Image is never zero-height whatever `height:` says.

### 7.3 The fix: un-pin the Html element

One declaration, in the `<style>` block the results assembler injects:

```css
.jmv-results-html { width: max-content; }
```

This does not fight the framework — it **restores `.jmv-results-item`'s own `width:max-content`**,
which jamovi sets one rule earlier and then overrides for Html items only. `.tx-scrollbox` also
carries `width:max-content`, which makes min-content equal max-content, so `#results`' shrink-to-fit
lands on the true width **in one pass**, with no oscillation against the iframe-resize feedback
loop. Each analysis owns its own iframe and declares exactly one `Html` element, so an unscoped rule
cannot reach another module.

⚠ **Un-pinning means prose starts driving the width.** A normally-wrapping block's max-content is its
whole text **on one line**, so a single ~200-character hint would report ≈1 300 px with no table on
screen. Every non-table fragment must therefore carry a `max-width` — which is what `.tx-note` is
for — and a caption uses the package's own idiom, `width:0;min-width:100%`, exactly as `.tx-foot`
does.

**Past the cap, the app imposes no maximum, so it is entirely our choice — and the choice is to
hug.** `max-width` survives only as a runaway guard and `@media print` lifts even that. ⚠ The
rejected alternative is not hypothetical: a real cap gives **two nested scrollbars**, because the
results panel is typically 900–1 100 px while the cap sat at 1 600 — the user scrolls the panel
right and only then meets the box's own scrollbar. Hugging gives one scrollbar and matches what
jamovi does with its own wide tables. ⚠ There is **no way for a module to learn the panel's width**:
the iframe's width *is* what we last reported, and the only outside signal is
`@media (min-device-width:…)`, i.e. the physical screen.

### 7.4 Where it lives

⚠ **The scroll box itself is not jamovi's.** `tab_html()` wraps *every* table it renders in a
`<div class="tx-scrollbox">` (`tx_scrollbox()`, `R/tab-render-html.R`) and `tab_css()` gives it its
shape — `display:block; width:max-content; max-width:100%; overflow-x:auto`, the trailing air, and
the `@media print` that lifts the clip. That serves a document, a pkgdown site and the Viewer as
well as jamovi. What is jamovi's, and **all** that `jmv_results_style()` may restate, is the cap:
a document box stops at the space it has, and jamovi has no space to read — the panel is sized
*from* the table. Anything else repeated here would drift from the stylesheet.

`R/jmvtab-export.R`, under `# === SECTION: the jamovi results iframe` — `jmv_results_style()` (the
one `<style>`: the Html un-pin, the box's pixel cap, the prose cap), `jmv_results_note()` (the one
shape a non-table fragment takes) and `jmv_results_content()`, **the boundary every
`html_table$setContent()` call goes through**: the style once, then the fragments, empties dropped.
No backend hand-writes a `<div>`; three gates in `tests/testthat/test-jmvtab-export.R` keep it that
way.

⚠ Nothing here touches `.a.yaml` / `.u.yaml` / `.js` / `.h.R`, so a change in this area needs no
`jmvtools::prepare()` — `jmvtools::install()` ships it.

**Vertical space needs nothing further**: an inactive item is `position:absolute`
(`.jmv-results-item:not([data-active])`) and takes no space at all, and a state carrier's round-trip
is unaffected by `visible` — `ResultsElement$asProtoBuf()` serialises `state` in a branch that never
reads it.

---

## 8. `.a.yaml` option types (the data model)

Each `type:` compiles to a `jmvcore` R6 class (`dev/jamovi/reference/jmvcore/options.R` is the
source of truth). Common keys: `name` (→ `self$options$<name>`), `title`, `type`, `default`,
docs-only `description:`.

⚠ **A `description: R:` block is Rd, and jmvtools reflows it** into the generated `.h.R`
roxygen at a fixed width — so it is written like any other `@param`: `\strong{}`, `\code{}`,
`\itemize{}`. Two things it must not do. A raw HTML tag (`<b>`) becomes `\if{html}{\out{<b>}}`,
which `Rd2HTML` opens *before* the paragraph it wraps — an HTML-validation NOTE. And no wrapped
line may begin `<digit>.`, which roxygen markdown reads as an ordered list: `… between 0 and 1.
Default to 0.95` reflowed into a numbered list and broke the sentence. Since the width the reflow
lands on cannot be predicted from the YAML, verify on the regenerated `man/*.Rd`, never on the
source: `grep 'out{<\|^\\enumerate{' man/jmvtab*.Rd`.

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
tabxplor uses `tame`), then `children:`. §5 is the authoritative runtime behaviour; this is
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
| `LevelSelector`     | pick a level                 | bound to a `Level` option; dynamic (§5.4)                                                                                        |
| `ComboBox`          | dropdown (List)              | `name`, `enable`                                                                                                                 |
| `RadioButton`       | one List value               | `optionName`, `optionPart`, `children`                                                                                           |
| `CheckBox`          | Bool / NMXList part          | `optionName`, `optionPart`, `children`                                                                                           |
| `TextBox`           | String/Integer/Number        | `format`, `suffix`, `inputPattern`, `width`, `ghostText`                                                                         |
| `Label`             | text / group header          | `label`, `format`, `style`, `children`                                                                                           |
| `LayoutBox`         | grid/stack container         | `margin`, `cell`, `stretchFactor`, `style`                                                                                       |
| `CollapseBox`       | collapsible section          | `label`, `collapsed`, `enable`                                                                                                   |
| `ActionButton`      | button (incl. row buttons)   | `name`, `events`, `enable`                                                                                                       |
| `Output`            | write a column back          | `name`                                                                                                                           |
| `CustomControl`     | JS-built DOM (§5.6)          | `creating`/`updated` events                                                                                                      |
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

`clearWith:` (per results element) lists **option names**; when any changes, jamovi marks that
result stale. It is the declarative invalidation hook — but note (§4.3) that the engine still gets a
full INIT on every change, so `clearWith` controls *result reuse*, not whether R runs. `Output`
element keys: `varTitle`, `varDescription`, `measureType`, `clearWith`, `initInRun`; R side:
`setValues()`, `setRowNums()`, `isFilled()`, `setKeys()`.

### ⚠ 10.1 A `$state` carrier needs `clearWith: []`, or it loses its state on every option change

`jmvcore::Image$new()` defaults `clearWith` to `"*"`, and `ResultsElement$fromProtoBuf()` opens
with:

```r
someChanges <- length(oChanges) > 0 || length(vChanges) > 0
if (someChanges && base::identical("*", private$.clearWith)) return()
if (base::any(oChanges %in% private$.clearWith))            return()
```

So with the default, the state is **not restored the moment ANY option changes** — and an option
change is the only thing that makes an analysis re-run. A staged flow is exactly that: clicking a
Run button writes an option, the `.js` writes it back a moment later, and on that second run the
carrier's `$state` is `NULL`, so the backend takes its "nothing stored yet" branch and paints a
banner over the table the first run had just computed.

**The fix is one line per carrier**: `clearWith: []` in the `.r.yaml`. The compiler emits
`clearWith = list()`; an empty vector fails both guards, so the state is restored. ⚠ **The same
default silently degrades a live cache**, for the same reason: a store dropped whenever an option
changes can only ever hit when nothing changed. These stores are the module's own and are
invalidated by **signature** (the cache keys), never by jamovi's option diff.

⚠ **`visible: false` is not the cure**, though it is worth keeping for the vertical space.
`Image$asProtoBuf()` reports a state-holding image that wrote no file as `ANALYSIS_RENDERING`
*whatever* `visible` says — the branch never reads it:

```r
else if (status == ANALYSIS_COMPLETE && (!is.null(self$state)) && path == "")
    result$status <- ANALYSIS_RENDERING
```

---

## 11. Two routes to a per-variable widget

Anything of the shape *"one control per variable the user selected"* — a reference level, a level
order, a merge — is built one of two ways. Know both: the first is what every other module does and
what the vendored examples show; the second is what tabxplor uses, and why.

### 11.1 The declarative keystone: Array-of-Group + templated ListBox + JS row-sync

1. **`.a.yaml`**: an `Array` whose `template:` is a `Group` of `elements:` — a variable
   (`type: Variable`) plus a per-variable choice (`type: Level` for a real level, `type: List` for a
   fixed enum).
2. **`.u.yaml`**: a `ListBox` bound by `name:`, with `columns:` mapping to the Group's `elements` —
   a `VariableLabel` column plus a `LevelSelector` / `ComboBox` column — and `events: { change: … }`.
3. **`.js`**: (a) **reconcile rows** — one row per selected variable, preserving prior choices;
   (b) **bind each row's picker** to its variable. Run both from `view`/`update` **and** from the
   relevant `onChange_<vars>`.

⚠ **The old declarative row-sync keys `items:(factors)` and `content:$key` are legacy no-ops** — row
population is entirely JS-driven. GAMLj still carries them cosmetically; do not rely on them.

Vendored live examples: `dev/jamovi/reference/jmv-logregbin/` (Level + LevelSelector) and
`dev/jamovi/reference/jmv-anova/` (List + ComboBox).

### 11.2 Why tabxplor uses `CustomControl` instead

Both of tabxplor's per-variable widgets went the `CustomControl` route (§5.6), because the built-in
`ListBox` + `LevelSelector` could not do four things the tables need: offer a **"Total"** choice that
is not a level; show levels **in the order the user reordered them** rather than jamovi's natural
order; sync to the **active axis** (row_vars under row %, col_vars under col %); and look like the
rest of the panel. `LevelSelector` takes levels verbatim (§5.4), and there is **no ready-made
drag-sortable factor-level control at module level** — so a genuinely custom widget is the only
route to per-level control.

⚠ **Two gotchas apply to every `CustomControl`** and cost real debugging time each:

1. A `CustomControl` never *claims* its option, so the compiler auto-generates a second, broken
   default control beside it. Set **`hidden: true`** on the backing option — the uicompiler skips
   hidden options — so the custom control is the sole UI. The option stays reachable as
   `ui.<name>` (the per-option wrapper, §5.2) even when hidden.
2. Never mix celled and cell-less children in one `LayoutBox`.

---

## 12. The reference-level picker

Under `pct = "row"` or means it chooses the reference **row** of each `row_var`; under
`pct = "col"`, the reference **column** of the chosen `col_var`. `refPickerCtrl`
(`jamovi/js/jmvtab.js`) is a `CustomControl`, sibling of the levels control and sharing its
`levelsCache` / `requestData` / `storedOrder` / styles.

- **One compact line per active-axis variable**: a bold variable name plus a native `<select>`
  showing the current reference, offering `[Total, …levels in the reordered order…]`. A drop-down
  rather than a radio list, because it is far more compact and the name/level distinction is
  clearer. A numeric `col_var` shows "numeric — vs its total" and no drop-down.
- **Stored by label** in `refLevels` (the `ref` element is typed `String`, not `Level`, so it can
  also hold `"tot"`). `refLevels` / `ref` / `ref2` are `hidden: true`. The effective auto-default
  (Total, or the first level under an odds ratio) is highlighted when unset.
- **A `ref2` section** — the odds ratio's second reference, over the *other* axis plus First/Total —
  renders only when an odds ratio is active.
- ⚠ **It re-renders on explicit `change` events** wired on the `pct` / `OR` / `color` radios and on
  the variable boxes, **not** on `updated`: a bare `CustomControl` does not get a reliable `updated`
  when a *different* option changes (§5.8) — which is exactly why the `ref2` section first failed to
  appear. `refPickerCtrl_updated` is only the self-`setValue` skip-gate.
- ⚠ **The label is matched by exact equality, not regex.** That makes metacharacter labels work
  *and* keeps the stored `ref` attribute human-readable in the colour legend, with no `^…$` token.

```r
# .b.R -- the option is already the argument's shape
ref_named <- purrr::map_chr(self$options$refLevels, "ref") |>
  rlang::set_names(purrr::map_chr(self$options$refLevels, "var"))
```

`.b.R` filters `refLevels` to the active axis, `jmvtab_ref_vector()` keys it by that axis, and
`tab_setup()` dispatches row reference against per-`col_var` column reference. The free-text `ref`
box stays as the expert fallback (`"auto"` / `"tot"` / `"first"` / a regex).

---

## 13. The levels control: order and merge, in one widget

`levelsCtrl` (`jamovi/js/jmvtab.js`) reorders a factor's levels **and** merges runs of them.
⚠ **They are one object, not two widgets:** a merged run is a run of *consecutive* levels **in the
order the user chose**, so a separate merge widget would have had to mirror this one's order.

**The shape.** A two-level collapsible `<details>` tree — axis (open) > `"<var> : N levels —
reorder"` (collapsed) — inside its own collapsed CollapseBox. Each variable's list is a 3-column CSS
grid `[level | merge tick | merged name]`, the name box placed with `grid-row: <start> / span k` so
it spans its run; each axis gets a full-width row. Selection follows jamovi's own colour. A level is
moved with an Up/Down button pair below the list or with the arrow keys, and stays selected so
repeated presses walk it.

**How it reads and writes.** Levels come from
`requestData('column', {properties: ['measureType', 'levels']})`; the list is built into a
**detached fragment and swapped in atomically**; the order goes to the `levelOrder` option
(`{var, levels}` per reordered variable) and the merges to their own option
(`{var, label, levels}`, one entry per merged run, `var` repeated — ⚠ a jamovi option template
cannot nest three deep). A **variable-signature gate** makes the frequent `updated` event a no-op
unless the variable set actually changed, which is what preserves focus, open sections and collapse
state.

**Facts worth keeping:**

- ⚠ **A tick belongs to the LEVEL, not the position.** Moving a level re-forms the runs — and moving
  one *into* a run splits it and visibly drops that merge. Keeping a non-contiguous group behind a
  display that shows the levels apart would be worse.
- ⚠ **The list shows the SOURCE levels** — it must, or a merge could not be undone — so the JS
  writes a **raw** order while the table's levels are the merged ones. `jmv_order_after_collapse()`
  is the one place the two specs meet; without it `jmv_relevel_cols()` would drop every merged
  level's raw names and the reorder would silently revert.
- ⚠ **The name box writes on `change` / `blur`, never `input`**: jamovi recomputes the analysis on
  every option write.
- ⚠ **`var` in a `while` loop is ONE binding.** Every name box's handler must receive its own `box`
  and `levels` through an IIFE, or they all edit the last run.
- ⚠ **Guard the arrow-key handler on `e.target.tagName === "INPUT"`**, or typing a merged label
  reorders the levels underneath it.
- The whole list is a **SHARED block** copied into `jmvtabreg.js` by `dev/generate_jamovi_js.R`
  (markers `BEGIN/END SHARED`, with the same `check` mode as the generated blocks). There it hangs
  off each factor predictor's reference row, merge-only.

**The R side is internal-only** — there is no public `tab()` argument. `jmvtab_levels_order()` feeds
the internal `tab(.levels_order =)`, and **`jmv_cache_aggregate()` relevels the shaped aggregate
after the fetch** (`jmv_relevel_cols()`; the stored blob stays raw) and recomputes `remove_levels`
for `levels = "first"`. So a reorder is a **tier-3 input** — tiers 1 and 2 are reused — and is
byte-identical to `tab()` on pre-relevelled microdata. Merges apply with `forcats::fct_collapse()`.

⚠ **Reordering the variables themselves needs no code at all**: a `ListBox` / `VariablesListBox`
with `isTarget: true` and the default `itemDropBehaviour: insert` already supports positional
drag-reorder (§5.5), and the order the user drags **is** the order.

---

## 14. The Excel export, and its path selector

**Confirmed against the shell and results bundles:** there is **no file/folder-picker control**
for a module, **no module-callable Save-As dialog**, and **no
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
in R; §4.3 change-detection handles re-fire).

### 14.2 `.b.R` — detect the click, write, report via Notice

```r
if (self$options$export) {
    p <- resolveExportPath(self$options$path)            # §14.3
    tab_xl(tabs, path = p, sheets = "unique", open = FALSE, replace = self$options$xl_replace)
    n <- jmvcore::Notice$new(options = self$options, name = "exportOK",
                             type = jmvcore::NoticeType$INFO)   # type 3 = info (§6.6)
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

Why `USERPROFILE` and not `~`: the module runs in Jamovi's bundled R (§4.2), where
`path.expand("~")` resolves to Documents — the root cause of tabxplor's `xl_path` hacks.

### 14.4 The zero-code user route, and what NOT to attempt

- **Zero-code**: the user can already get tabxplor tables out via Jamovi's **File ▸ Export →
  HTML** (whole results to HTML) or right-click a result → **Export...** (PDF/PNG/HTML). Worth
  documenting; keep the emitted HTML self-contained so "Copy" and HTML-export are clean (§6.2).
- **Do NOT** try to raise a native save dialog from the analysis (no hook exists), rely on
  `Action` `open`/`option$perform` (that is a newer Jamovi ≥ 2.7.12 feature — absent in the
  captured 2.6.44; gate with `is.null(option$perform)` if ever used), or serve the file via
  `openUrl` (backend-fragile). The typed-path + engine-write is the only robust route.

---

## 15. Working method

The failure mode is editing YAML/JS blind. The working method:

1. **Ground truth is local.** `dev/jamovi/reference/` holds byte-exact real-module source;
   `dev/jamovi/dev_console_live_capture/` holds the live runtime (compiled module, framework
   bundles, rendered HTML). Mirror these; never invent YAML/JS from memory.
2. **The framework bundles are searchable, not readable.** They are minified (one ~50–325 k
   char line each). Grep for **string literals** (control names, method names, event names,
   CSS classes, protocol keys) with small context; never `cat` them. §4–§6 already distilled
   the load-bearing facts; re-grep only to confirm a new detail.
3. **Close the loop with F10 + capture.** After `jmvtools::install(home=)` + reload, open
   DevTools (F10), inspect the target iframe, and `copy($0.outerHTML)` / `copy(ui.view.el
   .outerHTML)` to dump the real DOM into a file — the exact method that produced the capture
   folder. Re-capture after a UI change to verify what compiled.
4. **Prefer R over JS.** Detect clicks in `.b.R`; compute in R. Reserve `.js` for what only it
   can do: row-reconcile Array options to selected variables, bind per-row `LevelSelector`s,
   and `CustomControl` DOM (§5.6). All copied from `logregbin.events.js`.
5. **Respect the toolchain.** `.h.R` is generated (edit `.a.yaml` → `prepare()`); `compilerMode:
   tame` preserves `.u.yaml` hand-edits; delete commented `.js` (it ships, §4.2).
6. **The per-feature checklist**, in order: `.a.yaml` (the option) → `.u.yaml` (the control) →
   `.js` (the behaviour) → `.b.R` (read `self$options`) → `prepare()` → `install(home =)` →
   verify in the app with F10.

---

## 16. Reference material index

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
| `Jamovi_tabxplor_1_3_1_basic_table.html`                                   | The rendered app window (outer DOM: iframes, ports, sandbox, sizing, ribbon toggles) | §4.1, §3      |
| `127.0.0.1_56680_MAIN_ELECTRON/assets/main-fd7ff1c3.js`                    | The app shell: coms protocol, module load, action system, save dialogs, F10/F9       | §4.3, §14, §3 |
| `127.0.0.1_56680_MAIN_ELECTRON/modules/tabxplor__v_1.3.1.0`                | **The served/compiled tabxplor module** (manifest + `uijs` blob)                     | §4.2          |
| `.../modules/jmv__v_2.6.44.0`, others                                      | Other served modules (compare)                                                       | —             |
| `127.0.0.1_56683_..._analysis_UI/assets/analysisui-49b1a9ac.js` (+ `.css`) | The options-panel control framework                                                  | §5            |
| `127.0.0.1_56684_results/assets/resultsview-60a5863d.js` (+ `.css`)        | The results renderer                                                                 | §6            |
| `127.0.0.1_56684_results/aa145378.../2/res/02 jmvtab/resources/*.png`      | tabxplor's plot resources (addressing example)                                       | §4.1, §6.4    |

Method to refresh the capture: run Jamovi with tabxplor, add a crosstab, F10 → DevTools →
Sources/Network → save the analysis-UI, results, and main-electron origins; save the page
HTML. (The `.zip`s in the folder are the raw exports.)

---

## 17. French translation

Jamovi compiler is needed to automatically create the translation files for the jamovi UI module.

```bash
NODE=$(Rscript -e 'cat(node::node())' | tr -d '"')
JMC=$(Rscript -e 'cat(jmvtools:::jmcPath())' | tr -d '"')
MOD=~/github/tabxplor
"$NODE" "$JMC" --i18n "$MOD" --update catalog
"$NODE" "$JMC" --i18n "$MOD" --update fr
```

⚠ **`inst/i18n/fr.json` is written by `install()`, not by these.** `i18nUpdate` only rewrites
`jamovi/i18n/*.po`; `prepare()` actively EMPTIES `inst/i18n/`. The catalogue reaches the app only
after `jmvtools::install()`.

### 17.1 The three surfaces a string can come from, and how each is extracted

Read out of `jamovi-compiler/i18n.js` (`scanAnalyses()`), which is the authority:

| surface | what is scanned | what is extracted |
|---|---|---|
| `jamovi/*.yaml` | `0000.yaml`, every `.a.yaml` + its `.u.yaml` / `.r.yaml` | the `label` `title` `description` `ghostText` `suffix` `menuTitle` `superTitle` `content` `notes` keys, and string `default`s |
| `jamovi/js/**/*.js` | **every module `.js`** | `_("...")`, `n_("...", "...")`, `_p("ctx", "...")` |
| `R/*.R` (not `.h.R`) | every R file | `.("...")` — jmvcore's translator |

**So a JS-rendered label IS translatable.** The analysis UI defines the lookup itself, before it
evaluates the module:

```js
this.translate = n => { … this.i18n.locale_data.messages[n.trim()] … },
window._ = this.translate.bind(this),
ui && eval(def)          // <- our module runs AFTER `_` exists
```

Three runtime constraints follow, and each is a silent failure if broken:

- ⚠ **`_()` is the only one that works.** The globals are `window._` (this module's catalogue) and
  `window.s_` (jamovi's own, for app strings). There is no `window.n_` and no `window._p`, and
  `translate` returns `msgstr[0]` only — so **no plural forms and no msgctxt**. Where a sentence
  needs a branch, write TWO FULL msgids rather than splicing a translated noun into a frame.
- ⚠ **No edge whitespace inside `_()`.** The lookup key is `n.trim()` while the extractor stores the
  literal as written, so `_(" trials")` files `" trials"` and looks up `"trials"` — a permanent miss
  that reports nothing. Put the space outside the call. (Same rule as `dev/french_glossary.md`
  § Rules already states for `gettext()`.)
- ⚠ **A fuzzy entry is DROPPED** when `createTranslationJSON()` writes `fr.json`, and an *unescaped*
  `"` inside any msgstr aborts the compiler with `Invalid key name`, shipping a module with **no
  translations at all**.

`R/*.R` uses **`jmvcore::.()`**, which resolves against this same `fr.json` keyed on jamovi's UI
language — which plain `gettext()` (the `R-tabxplor` domain) is not. ⚠ it reads `self` out of its
caller's frame, so only a function that HAS one may call it: `export_status_html()` takes its lead
words as an argument for exactly that reason.

### 17.2 What is translated, and what deliberately is not

**The argument name stays English; only the parenthetical is French** — `<b>col_vars = <i>(variables
en colonne)</i></b>`. So do argument VALUES (`2sd`, `max`, `first`, `linear`), the notation (`OR`,
`Chi2`, `R2`) and the `a*b` interaction key. That rule is what keeps a French label near its English
width, and it is why ~40 msgstrs are byte-identical to their msgid on purpose.

⚠ **Width is the real risk, and it is absolute, not relative.** The options pane is ~340px at its
narrowest and the two CustomControl tables are FIXED-px grids whose head and select cells are
`white-space:nowrap; text-overflow:ellipsis` — they TRUNCATE silently. `JMV_WIDTH_BUDGET` in
`tests/testthat/test-jamovi-i18n.R` declares a per-string character budget derived from those pixel
widths (~6px/char) and holds **both** languages to it, so a budget its own English breaks is a wrong
budget. Everything else (radio labels, hints, tooltips) wraps and is free.

The companion gate in the same file is coverage: **every literal painted into `textContent` /
`title` / `placeholder` / a host object's text slot must be inside `_()`**. That is the invariant
whose absence left ~90 strings English for several phases with no symptom anywhere.

## 18. Sources

Official: `dev.jamovi.org` (`/tutorial/tuts01xx`, `/api/*`, `/ui/*`,
`/ui/advanced-customisation`, `/api/option-action`); legacy `docs.jamovi.org/_pages/*`. Repos:
`github.com/jamovi/{jmvtools, jamovi-compiler, jmvcore, jmv, jamovi, walrus}`,
`github.com/gamlj/gamlj`, `github.com/NourEdinDarwish/SummaryTables`,
`github.com/sbalci/ClinicoPathJamoviModule`. Forum: Array options/reference levels (`t=4129`),
file I/O / no picker (`p=13515`, `t=132`), sandboxing (`t=3679`), debugging/F10 (`t=15`),
runtime control setting (`t=440`). **Live capture: `dev/jamovi/dev_console_live_capture/`** — the authority
for §4–§6. Vendored verbatim source: `dev/jamovi/reference/`.

---

