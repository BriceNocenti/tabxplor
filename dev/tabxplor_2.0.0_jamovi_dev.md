# tabxplor 2.0.0 — Jamovi module development technical guide

Written 2026-07-08. The reference for redesigning tabxplor's Jamovi module (2.0.0 Phase 8)
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

⚠ **Updated 2026-07-16 (migration Phase C3): the dev environment moved to WSL2.** The build path
is now the **flatpak jamovi 2.7.36** (bundled R **4.5.0**) in Ubuntu 26.04, driven by **jmvtools
2.7.26**; Windows jamovi survives **build-only**, for Windows `.jmo` and as the sole 2.6-solid
test target (the 2.6.44 flatpak commit has been pruned from Flathub). §3 is rewritten for this.

⚠ **The live capture in §5–§7 is from Windows jamovi 2.6.44 and has NOT been re-captured on
2.7.36.** It remains the authority on runtime *architecture* (the iframe/coms/results model is
stable across the series), but treat **version-specific details as provisional** — notably the
`rVersion` stamp (§5.2) and the uicompiler enums (§6.8). Ask the maintainer for a fresh capture
when a 2.7-specific answer is needed.

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
- **Two confirmed footguns from the live capture** (§5.2): the module runs in jamovi's
  **bundled** R, not your system R — the root of the `~`/path quirks (4.4.1 in the captured
  Windows 2.6.44; **4.5.0 on the WSL flatpak 2.7.36**; system R is now 4.6.1) — and the compiler
  ships JS comments verbatim, so the 295 commented lines in `jmvtab.js` are downloaded by
  every user. Clean `jmvtab.js` before release.

---

## 3. Toolchain and the dev loop

### 3.1 Install

⚠ **A `.jmo` is tied to OS + arch + jamovi series**, so there are **two build paths and they are not interchangeable** (migration A1/C3). Edit source in **one place only** — WSL.

| Target                            | jamovi                                                      | Checkout                                                        | Build                                                                          |
|-----------------------------------|-------------------------------------------------------------|-----------------------------------------------------------------|--------------------------------------------------------------------------------|
| **Linux `.jmo`** — the dev path   | flatpak `org.jamovi.jamovi` **2.7.36**, bundled R **4.5.0** | `~/github/tabxplor` — **authoritative for source**              | `jmvtools::install(home = 'flatpak')`                                          |
| **Windows `.jmo`** — release only | Windows jamovi (**kept forever**; the only 2.6-solid path)  | `D:\Statistiques\github\tabxplor` — **pull, build, never edit** | `options(jamovi_home='C:/Program Files/jamovi 2.6.44.0'); jmvtools::install()` |

⚙ **The Windows build is now scripted** (Phase 15a): `dev/build_jmo_windows.R` (run
`Rscript dev/build_jmo_windows.R` on Windows). It clones the current branch into a **throwaway temp
folder** (not the `D:\` checkout), pins `jmvtools` to 2.7.26, installs deps, `Sys.unsetenv`s
`ELECTRON_RUN_AS_NODE`, runs `jmvtools::install(home='C:/Program Files/jamovi 2.7.37.0')`, then
verifies the landed module. Branch/jamovi-home overridable via CLI arg / `TABXPLOR_BRANCH` /
`JAMOVI_HOME`. The manual recipe below still documents what it automates.

**Prerequisites** (WSL side, done by migration C3 — see `~/github/.WSL2_sandbox_migration/` §7):

1. **jamovi** — `flatpak --user install flathub org.jamovi.jamovi` **plus `org.freedesktop.Sdk//24.08`**, which is *required*: `flatpak run --devel` swaps Platform→SDK and that is how the compiler reaches jamovi's R.
2. **R** — any; see §5.2, the module is built by *jamovi's* bundled R, not yours. (**No Rtools** on Linux; the SDK's g++ compiles.)
3. **`node` + `jmvtools`** — ⚠ **pin jmvtools**; the obvious command installs the wrong version:

   ```r
   install.packages('node', repos = 'https://repo.jamovi.org')          # -> 1.3
   # NOT install.packages('jmvtools', repos='https://repo.jamovi.org') -- that index serves
   # 2.7.26 AND 28.0-28.3, so R takes 28.3, whose compiler can emit a `jms` 2.7.36 refuses.
   install.packages('https://repo.jamovi.org/src/contrib/jmvtools_2.7.26.tar.gz',
                    repos = NULL, type = 'source')                      # repos=NULL resolves NO deps
   packageVersion('jmvtools')                                           # MUST be 2.7.26
   ```

   `jmvtools` vendors the Node `jamovi-compiler` (`jmc`); `node` supplies the runtime.

⚠⚠ **`ELECTRON_RUN_AS_NODE` will waste your day if you don't know it.** Positron/Claude Code export it; flatpak passes it into the sandbox; jamovi's Electron then runs as **plain node** → **exit 0, no window, no error**, and `jmvtools::install()` dies `"bad option: --install"` (rc=9). `flatpak run --unset-env=` is *not* enough (zypak re-spawns children via the host). Use `env -u` on the host — the **`jamovi`** wrapper (`~/.local/bin/jamovi`) does it, plus DPI scaling. In R: `Sys.unsetenv("ELECTRON_RUN_AS_NODE")` before `install()`. ⚠ `check()` passes regardless (it never reaches Electron), so a green `check()` proves nothing.

### 3.2 `jmvtools` functions

`create('Name')` scaffold · `addAnalysis(name=,title=)` add an analysis (5 files) ·
`prepare()` compile only (regenerate `.h.R` + UI blob) · `install()` build + install the
`.jmo` · `check()` verify Jamovi is found · `i18nCreate()/i18nUpdate()` catalogs ·
`version()`.

### 3.3 Point jmvtools at jamovi

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

### 3.4 The cycle

```
edit yaml/js/b.R ─▶ jmvtools::prepare()  (fast, regenerate .h.R + UI blob)
                 ─▶ jmvtools::install()   (build .jmo, install) ─▶ reload analysis in Jamovi
```

For complex modules: `prepare()` then `devtools::document()` twice, then `install()` — the
`.h.R` carries the roxygen `@param` blocks that feed `man/jmvtab.Rd`. UI not updating → close
jamovi fully, reinstall (this was a Windows `.jmo` file-lock issue; unverified on flatpak).

⚠ **Never hand-edit `R/jmvtab.h.R`, even to keep it in sync with the YAML.** It was hand-mirrored
across ~7 phases; when C3 finally ran `prepare()`, the compiler reproduced **778 of 780 lines** but
corrected `exportExcel` (`type: Action`) from `NULL` → `FALSE` **and supplied a default it lacked** —
without which `tabxplor::jmvtab()` called from R throws at the `exportExcel = exportExcel`
pass-through. The mirror was *nearly* right and still shipped a latent bug, on the newest option.

**Build cost, measured (C3, WSL flatpak): `install()` ≈ 2 min.** jamovi's bundled R serves
**binaries** for most of tabxplor's dep tree; only a few (e.g. `openxlsx2`) compile, via the SDK's
g++ 14.3.0. Verify the install by mechanism, not by the "Module installed successfully" message:

```bash
grep -E '^version:|^rVersion:' ~/.jamovi/modules/tabxplor/jamovi.yaml   # rVersion == jamovi --r-version
ls -l ~/.jamovi/modules/tabxplor/ui/jmvtab.js                           # the compiled uijs blob
```

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

- **The module runs in Jamovi's BUNDLED R, not your system R.** The stamp is read from the target
  jamovi at build time, so it is self-consistent by construction — and it **differs per build path**:
  **`4.5.0-x64`** on the WSL flatpak (jamovi 2.7.36) vs **`4.4.1-x64`** in the captured Windows
  2.6.44 (below). Your system R (4.6.1 in WSL) is irrelevant to module compatibility. This is the
  root cause of `path.expand("~")` → Documents, and of package-version drift. Always test inside
  jamovi, and use `Sys.getenv("USERPROFILE")` for paths (§14).
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

### 7.3 Width and scrolling

**Full analysis, with every rule quoted from the shipped bundle: `dev/jamovi_results_width.md`. Read
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
`pct="col"`, the reference column of the chosen `col_var`. Maps onto the 2.0.0 decision that
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

### 13.2b Merging levels — BUILT (Phase 20g-ii), in the SAME control

The reorder control became **`levelsCtrl`** and now does both, because they are one object: a merged
run is a run of CONSECUTIVE levels *in the order the user chose*, so a separate widget would have had
to mirror this one's order. The `<ul>` list became a **3-column CSS grid** `[level | merge tick |
merged name]`, the name box placed with `grid-row: <start> / span k` so it spans its run; each axis
now gets a **full-width row** (the old `1fr 1fr` grid halved the space the name box needs).

Facts worth keeping:

- **A tick belongs to the LEVEL, not the position.** Moving a level simply re-forms the runs — and
  moving one INTO a run splits it and drops that merge, visibly. Keeping a non-contiguous group
  behind a display that shows the levels apart would be worse.
- **The option is order-INDEPENDENT groups** (`{var, label, levels}`, one entry per merged run,
  `var` repeated — a jamovi option template cannot nest three deep). R applies it with
  `forcats::fct_collapse()`; the order stays `levels_order`'s business.
- **The list shows the SOURCE levels** (it must, or a merge could not be undone), so the JS writes a
  RAW order while the table's levels are the merged ones. `jmv_order_after_collapse()` is the one
  place the two specs meet — without it `jmv_relevel_cols()` would drop every merged level's raw
  names and the reorder would silently revert.
- **The name box writes on `change`/`blur`, never `input`** (§13.2's rule, and the subtext control's):
  jamovi recomputes the analysis on every option write.
- ⚠ **`var` in a `while` loop is ONE binding.** Every name box's handler must receive its own `box`
  and `levels` through an IIFE, or they all edit the last run.
- ⚠ **Guard the arrow-key handler on `e.target.tagName === "INPUT"`**, or typing a merged label
  reorders the levels underneath it.
- The whole list is a **SHARED block** copied into `jmvtabreg.js` by `dev/generate_jamovi_js.R`
  (markers `BEGIN/END SHARED`, same `check` mode as the generated blocks). There it hangs off each
  factor predictor's reference row, merge-only.

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
    re-render — drive the 2.0.0 aggregate-core + per-transform subfunctions at
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

## 19. French translation

Jamovi compiler is needed to automatically create the translation files for the jamovi UI module.

```bash
NODE=$(Rscript -e 'cat(node::node())' | tr -d '"')
JMC=$(Rscript -e 'cat(jmvtools:::jmcPath())' | tr -d '"')
MOD=~/github/tabxplor
"$NODE" "$JMC" --i18n "$MOD" --update catalog
"$NODE" "$JMC" --i18n "$MOD" --update fr
```

## 20. Sources

Official: `dev.jamovi.org` (`/tutorial/tuts01xx`, `/api/*`, `/ui/*`,
`/ui/advanced-customisation`, `/api/option-action`); legacy `docs.jamovi.org/_pages/*`. Repos:
`github.com/jamovi/{jmvtools, jamovi-compiler, jmvcore, jmv, jamovi, walrus}`,
`github.com/gamlj/gamlj`, `github.com/NourEdinDarwish/SummaryTables`,
`github.com/sbalci/ClinicoPathJamoviModule`. Forum: Array options/reference levels (`t=4129`),
file I/O / no picker (`p=13515`, `t=132`), sandboxing (`t=3679`), debugging/F10 (`t=15`),
runtime control setting (`t=440`). **Live capture: `dev/jamovi/dev_console_live_capture/`
(Jamovi 2.6.44.0, tabxplor 1.3.1, bundled R 4.4.1-x64), analysed 2026-07-08** — the authority
for §5–§7. Vendored verbatim source: `dev/jamovi/reference/`.

---

## §15. Phase 15b — the `jmvtabreg` (Regressions) analysis

A second jamovi analysis wrapping `tab_reg()`, built from the `jmvtab` template. Files:
`jamovi/jmvtabreg.{a,u,r}.yaml`, `jamovi/js/jmvtabreg.js`, `R/jmvtabreg.b.R`,
`R/jmvtabreg-cache.R`; registered as a 2nd entry in `jamovi/0000.yaml`. `R/jmvtabreg.h.R` is the
usual generated header (a maintainer `jmvtools::prepare()` step — not created headlessly).

**Scope (15b-i, done).** Single-model UI: every family (`auto`/gaussian/binomial/poisson/
quasipoisson/multinomial/ordinal), multiple dependents, `exponentiate`/`effect`/`at`/
`estimate_display`, `empirical` (crude companion), a per-predictor **reference-level picker**
(the `refPickerCtrl` CustomControl, simplified from jmvtab: axis = the `predictors` list, factor-only,
no `ref2`), CI/`method`/`stars`, colours, `na`/`cleannames`/footer, survey `wt` + an **Advanced
survey-design** collapse (a 2nd `VariableSupplier` for `ids`/`strata`/`fpc` + `nest`, greyed by JS
when `wt` is empty), and Excel/HTML/MD export (reuses `R/jmvtab-export.R` verbatim).

**The live cache — a fit-DIGEST cache, not an aggregate cache** (rewritten in Phase 22j; what
follows is the current design, not its history). `jmvtab_reg_build()` drives
`tab_reg(..., .fit_cache = cache_env)`. The store (hidden `cache_state` Image `$state`) holds **one
tier** of DISTILLED fit records: a `tabxplor_fitdigest` (`R/reg-digest.R`) plus everything the eager
stage computed off the live fit, with the fitted object and the model frame thrown away. So the key
carries **no estimand** — `measure` / `effect` / `display` / `colour` / `conf_level` / `stars` are
all hits — and the store is kilobytes (3.3 KB per record; 29 KB for a binomial panel, 92 KB for a
multinomial one) where it used to be 6–16 MB serialised on every round-trip. What IS in the key is
the model: outcome, predictors, family/link, trials, `multiplier`, `shape`, anchors, crosses,
`stats`, the data fingerprint — and the **reference for free**, since the relevel happens before the
fit and `jmv_col_fp()` fingerprints a column's levels. Two shapes refuse the store
(`reg_fit_cacheable()`): `method = "profile"` (its bounds are a likelihood output) and a model
comparison (a test between the fit objects). Measured: a multinomial `measure` change 14.35 s →
1.90 s.

**Scope (15b-ii, done) — the model-comparison "+" builder.** A **Model comparison** CollapseBox holds
a `compare` combo (`none`/`baseline`/`sequential`), the `modelBuilderCtrl` CustomControl, and a `trials`
combo (`off`/`observed`/`fixed` + `trials_n`, declaratively greyed to binomial/auto). The builder is a
**checkbox-grid card** UI (chosen with the maintainer over multi-select chips): each card = an editable
name + one checkbox per predictor in the `predictors` pool + a delete `×`; a "+ Add model" button
appends a card defaulting to the **full pool**; a card enforces **≥1 checked var** so a card index == a
final model index. Cards are stored in the hidden `models` Array (`Group{label, vars:Array<Variable>}`),
folded by `jmvtab_reg_models()` into `tab_reg()`'s `predictors`: an **empty builder → the flat pool**
(single model, byte-identical to 15b-i); **≥1 card → a named list** = model comparison (one column per
model). **Baseline** = a **per-card radio marker** (chosen over a dropdown), shown only when
`compare=="baseline"`, writing the model's **1-based position** to the hidden `baseline` Integer (a
position is exact given the non-empty-card invariant; safer than a by-label string that `make.unique`
rewrites R-side). **`multiplicator`** (numeric-predictor scaling, OR/beta per k units) is folded into the
**numeric rows of the reference picker** (References box relabelled "References and predictor scaling"):
a numeric predictor has no reference level, so its row offers a `× k per unit` input writing the hidden
`multiplicator` Array; `jmvtab_reg_mult_vector()` folds it to `tab_reg(multiplicator=)`. `compare` is
imperatively greyed to ≥2 models (array length is invisible to `enable:`); the builder re-renders on a
`predictors` (pool) or `compare` change; its signature `[pool, compare]` **excludes** `models`/`baseline`
so a card/name/marker edit is an in-place repaint (mirrors `refSig` excluding `refLevels`).

**Cache ceiling raised (15b-ii).** Model comparison forces the **raw-fit** cache tier (the KB digest
fast-path is single-model only), and a `reg_fit` value is **~9–11 MB** on survey-scale data (21k rows) —
over the old 4 MB `JMVREG_MAX_FIT_BYTES`, so comparison fits were silently skipped (every toggle refit
every model). Bumped `JMVREG_MAX_FIT_BYTES` 4→24 MB and `JMVREG_MAX_STORE_BYTES` 16→96 MB (a contained
`R/jmvtabreg-cache.R` change; the crosstab cache is untouched), so a comparison caches: display/compare/
baseline/stars toggles reuse fits, add-a-model reuses existing + fits only the new subset, and only a
reference/predictor/family change refits (a reference change in comparison mode relevels → a new key).

**Gotchas re-confirmed.** No option named `levels` (a `jmvcore::Options` member — `method`/`family`/
`effect`/`at`/`color`/`reference`/`compare`/`baseline`/`multiplicator`/`trials_mode`/`trials_n`/`models`
are all safe, verified against `jmvcore/options.R`). `utils.clone` not `context.clone` (jus 3.0). Two
`VariableSupplier`s (main + survey) is fine. The `.b.R` R6 `inherit = jmvtabregBase` is lazy, so the file
loads / `R CMD check`s before `prepare()` generates the header (until then `check()` NOTEs `jmvtabregBase`
as an undefined global — expected). `tab_reg.R` needed **no change** — the multiplicator fit-key was
already correct (`extra` at `tab_reg.R:1815-1816`).

---

## Phase h — final UI review (2026-07-21)

All jamovi YAML/JS edits below are INERT until `jmvtools::prepare()` regenerates the `.h.R` and the
module is rebuilt; only the R-backend parts are suite-verifiable.

**Family selector (`js/jmvtabreg.js`).** No "auto (detected)" and no "quasipoisson" rows. `detectFamily(c)`
computes the family client-side (fetches `dataType` too: integer→poisson, decimal→gaussian; 2-level→
binomial, 3+ ordered→ordinal, 3+ nominal→multinomial) and PRE-SELECTS it, storing it explicitly in
`depFamily` so `jmvtab_reg_dep_family` never re-detects (and `reg_detect_family` never aborts on an
integer count). A single-option outcome greys the `<select>`. `mtRow` is full-width 3-col; the modelled-
level picker lost its "model " prefix. `applyModelEnables()` greys `effect_1/2` + `exponentiate` when
every outcome is gaussian (one `anyNonGaussian` predicate; families read from `mtCache`).

**Model-comparison Run button.** `run_compare` (Action, `.a.yaml`) + `compare_state` (hidden Image,
`.r.yaml`) persist the last comparison's `list(sig, html)`. `jmvtabreg.b.R` `.run()`: `staged` = ≥2 folded
models (`is.list(opts$predictors) && length >= 2`); when staged and neither `run_compare` nor `exportExcel`
fired, re-serve `compare_state$html` (sig match) or show `.compare_hint` (outdated banner) — NO refit.
On a trigger, build + render + persist. Single-model stays live. Pure helpers `jmvtab_reg_staged()` /
`jmvtab_reg_compare_sig()` (jmvtabreg-cache.R) are unit-tested (test-jmvtabreg-cache.R). JS `run_compare_changed`
resets the button like `exportExcel_changed`. The cache STORE shape is unchanged → no schema bump.

**Layout (`.u.yaml`).** Significance = 3 rows / 3 equal columns (Show: colour | stars ; conf_level box |
method legend | method radios ; color_signif full width). "Missing values and display" → **Display**
(estimate_display beside a single-title wrap_rows/wrap_cols/cleannames stack); subtext stretched full
width. Export `<hr>` separator above the (outside-the-collapse-hierarchy) Export block. `stars`/`cleannames`
`.a.yaml` titles bare-arg (de-dup vs the `.u.yaml` legend). `js` `injectTabxCss` (was `injectExportCss`)
adds a best-guess collapse-box bottom-margin. Same parity edits in `jmvtab.u.yaml` / `js/jmvtab.js`.

**Freeze diagnosis (mixed multinomial).** R build ≤1.5 s and correct — not the cause. The persisted
`cache_state` serializes ~41.5 MB/run for a 3-fit mixed table (model frames + qr). Mitigation: a
`private$.checkpoint()` before the heavy build in both `.b.R`. A real shrink (persist digests not raw
multi-fit stores) touches the byte-locked reref/AME path → flagged for a live-verified follow-up.

**Not fixable from tabxplor.** The `DOMNodeInserted` / `addRange()` console warnings come from jamovi's
own Electron/Chromium option-UI framework (compiled `uijs`). The `conf_level` up/down 0.01 stepper is not
a native jamovi control (plain number box kept, per the maintainer's decision).

---

## Phase o — Export-folder detection: real-world results (2026-07-22)

The throwaway `jmvtest` (menu tabxplor > Diagnostics; `R/jmvtest.b.R` + detectors in
`R/jmvtab-export.R`) was run live in jamovi on **Windows 11** (jamovi 2.7.37.0, bundled R 4.5.0) and
**WSL2 Ubuntu flatpak** (jamovi 2.7.36.0, bundled R 4.5.0). Both wrote a `.md` into every writable
candidate; the maintainer confirmed which files landed in the real Documents.

### What actually happened

- **Windows** — files landed in `D:\Documents` (the redirected real Documents), from the
  `registry Shell Folders\Personal` method and from the "detected Documents" button.
- **WSL flatpak** — the file landed in `/home/dev1/Documents` (the Linux side; the flatpak sandbox
  has no `/mnt`, no `powershell.exe`/`cmd.exe`/`wslpath`, so the Windows `D:\Documents` is
  unreachable — the Linux home is the only sane target, reachable from Windows via
  `\\wsl.localhost\...`).

### Windows detection table (jamovi 2.7.37 bundled R)

| method                                   | dir                      | exists | writable |
|------------------------------------------|--------------------------|:------:|:--------:|
| powershell GetFolderPath(MyDocuments)    | (empty)                  |   -    |    -     |
| **registry Shell Folders\Personal**      | **D:\Documents**         |  TRUE  | **TRUE** |
| registry User Shell Folders\Personal     | D:\Documents             |  TRUE  |   TRUE   |
| reg.exe query Shell Folders              | D:\Documents             |  TRUE  |   TRUE   |
| OneDrive env + \Documents                | (unset)                  |   -    |    -     |
| home/Documents (naive baseline)          | C:/Users/Brice/Documents | FALSE  |  FALSE   |
| CURRENT resolveExportPath("~/Documents") | C:/Users/Brice/Documents | FALSE  |  FALSE   |

Recommended (auto): **registry Shell Folders\Personal -> D:\Documents**. Decisive env facts:

- `which powershell.exe` = **(not found)** — PowerShell is NOT on the bundled R's PATH, so the
  "strongest" `GetFolderPath` method is UNAVAILABLE in a real jamovi. `reg.exe` and `cmd.exe` ARE
  present (`C:\WINDOWS\SYSTEM32\...`). So on Windows the winner is `utils::readRegistry` (no
  subprocess), with `reg.exe` as the subprocess fallback.
- `HOME` = `C:/Rtools/home/builder`, `path.expand("~")` = `C:/Rtools/home/builder` (!). The
  file-header claim that "bundled R's `path.expand('~')` -> Documents" is FALSE for 2.7.37 — here `~`
  is the Rtools builder home. We already sidestep this: `export_home_dir()` uses `fs::path_home()` =
  `USERPROFILE` = `C:/Users/Brice`, so `~/Documents` expands to `C:/Users/Brice/Documents` (the
  CURRENT row) — correct expansion, wrong folder (the real one is on D:).
- `getwd()` = `C:/Program Files/jamovi 2.7.37.0/bin` and is **not writable** (the one write that
  FAILED, "cannot open the connection"). `getwd()` is a useless fallback; `tempdir()` is the reliable
  safety net.

### WSL/Linux detection table (flatpak)

| method                     | dir                  | exists | writable  |
|----------------------------|----------------------|:------:|:---------:|
| xdg-user-dir DOCUMENTS     | /home/dev1           |  TRUE  |   TRUE    |
| home/Documents (baseline)  | /home/dev1/Documents | FALSE* | (created) |
| powershell / reg / wslpath | (not found)          |   -    |     -     |

`*` `/home/dev1/Documents` did not pre-exist; the write CREATED it. `xdg-user-dir` IS installed
(`/usr/bin/xdg-user-dir`) but this distro has no `~/.config/user-dirs.dirs`, so it falls back to
echoing `$HOME` (`/home/dev1`, NOT a Documents subfolder) — which is why "Recommended" wrongly points
at the home root. `getwd()` = `/app/bin` (read-only sandbox), `tempdir()` = `/var/tmp`.

### The two questions answered

- **Is `~/Documents` the normal Documents on every Ubuntu?** (The real Linux user base is normal
  **desktop or server Ubuntu**, NOT WSL — the flatpak-in-WSL run above is only the dev test box, and
  its no-`/mnt`, no-interop sandbox is the harshest case, not the typical one.) `~/Documents` is the
  freedesktop/XDG *default* (`XDG_DOCUMENTS_DIR="$HOME/Documents"`), created by the `xdg-user-dirs`
  package at first graphical login on a normal Ubuntu **desktop** — there `xdg-user-dir DOCUMENTS`
  returns `/home/<user>/Documents`, which exists, and is the winning method. But it is **not
  guaranteed**: on **server** / minimal / container / (this) WSL install `xdg-user-dirs` often never
  ran, so `user-dirs.dirs` is absent, `~/Documents` does not exist, and `xdg-user-dir` returns bare
  `$HOME`. Folder names can also be localized by `xdg-user-dirs-update` (French keeps "Documents", but
  e.g. Downloads -> "Téléchargements") — so never hardcode the name; take whatever `xdg-user-dir`
  returns. Robust rule: trust `xdg-user-dir` ONLY when it returns a real subfolder (`!= $HOME`);
  otherwise use `$HOME/Documents` and **create it** (correct on desktop, server, and WSL alike).
- **University / managed Windows robustness.** Folder Redirection / roaming profiles (GPO) push
  Documents to a UNC network share; the resolved absolute path is written into
  `Shell Folders\Personal` — exactly the value the registry method reads. So registry-first is the
  *correct* choice for managed machines too (it returns the network path). The only added risk is an
  offline share (`exists`/`writable` FALSE) -> the fix must validate and fall back.

### Recommended `export_documents_dir()` rewrite (the follow-up fix)

Mirror jamovi's own native `Dirs`, per OS, validate, and always create/fall back:

```text
Windows:
  1. readRegistry HKCU ...\Shell Folders     -> Personal   (redirect + UNC honoured)   [PROVEN]
  2. reg.exe query ...\Shell Folders /v Personal            (subprocess, if readRegistry blocked)
  3. readRegistry HKCU ...\User Shell Folders -> Personal + %VAR% expand
     -> take the first whose dir EXISTS or is creatable+writable; else
  4. USERPROFILE\Documents (create)                         [do NOT use PowerShell: absent from PATH]
macOS:
     $HOME/Documents (create)                               [always the right place]
Linux (normal desktop/server Ubuntu is the base case; WSL is just the harshest one):
  1. xdg-user-dir DOCUMENTS        -> use ONLY if != $HOME and creatable+writable   [desktop winner]
  2. ~/.config/user-dirs.dirs XDG_DOCUMENTS_DIR  (same != $HOME test)
  3. else $HOME/Documents (create)          [server/minimal/WSL: xdg falls back to $HOME, so create]
Universal safety net:
     if the chosen dir can't be created/written -> tempdir(), and SAY the resolved path in the Notice.
```

Plus the **routing fix** (the actual live bug): `resolveExportPath()` only calls
`export_documents_dir()` when the folder box is BLANK, but the default is the non-blank `"~/Documents"`,
so the resolver is skipped and the wrong `C:/Users/Brice/Documents` wins. Fix: change the `export_dir`
default to `""` (blank, show a "(your Documents)" placeholder) OR treat `"~/Documents"` / `"~"` /
`"auto"` as a sentinel routed through `export_documents_dir()`. Keep the always-`dir.create` in
`jmvtab_export()`. This lands in `R/jmvtab-export.R` (+ the `.a.yaml` default + the two JS reset
handlers) as the next step; then `jmvtest` is removed (detectors + tests stay).

## Phase o — UI bug corrections (2026-07-22)

Four defects, root-caused by three parallel search agents; two maintainer hypotheses corrected.

### Excel export crash — the older bundled openxlsx2, NOT the cache

The maintainer suspected the cache produced a subtly-different table. It does not: a jamovi-built table
is byte-equivalent to a fresh `tab()`/`tab_reg()`. The real cause is a **version drift** in openxlsx2
(same one the `xlb_na_argname()` shim already documents). `xl_coalesce()` (`R/tab-xl-backend.R`) packs
non-contiguous cells that share a style/numFmt into ONE comma-joined multi-area `dims` (e.g.
`"C7:E8,F4:F8"`) — the efficient shared-style path. A **current** openxlsx2 accepts multi-area dims; the
**older build bundled inside jamovi** has a single-range validator that rejects the comma with exactly
`"dims must be something like A1 or A1:B2."`. Comma dims only appear on richer tables (a significance
row, counts, add_n — precisely what the jamovi UI builds), so minimal plain-R tests on a newer openxlsx2
never hit it.

Fix: `xlb_dims_each(dims, f)` splits a comma dims and calls `f` per single range; `xlb_numfmt()` and the
new `xlb_set_cell_style()` (which `xl_apply_styles` + the span-row style now route through) emit one
rectangle at a time. Semantically identical (same code/style over each sub-rectangle), works on both
openxlsx2 versions, and is ONE package-level fix — no jamovi-only branch, no no-cache export path.

### Model-comparison freeze — the raw-fit store in `$state`

Confirmed cache/state. In comparison mode the store is off (a comparison is a test between the fits; historically: `tab_reg`'s `reref` needed
`compare == "none"`), so the cache holds only the raw fits (~10 MB each: model frame + qr). Once
persisted into `cache_state$state` they re-serialize on **every** UI round-trip — 4 models ≈ 40 MB →
freeze; the staged early-return never cleared them. Fix: `jmvtab_reg_build(..., use_cache = TRUE)`
fits with `.fit_cache = NULL` and returns `store = NULL` when FALSE; `jmvtabreg.b.R` `.run()` sets
`use_cache = !staged` and `if (staged) cache_state$setState(NULL)` (the one line that stops the leak).
The cache is worthless in comparison mode anyway (every Run recomputes); a single model restores it.

### UI polish (inert until `prepare()` + rebuild)

- **Export separator**: the former `<hr>` Label rendered as literal text (jamovi Labels escape
  block-level HTML). Removed from both `.u.yaml`; a real border-top is drawn by `styleExportSep()` in
  each `js/*.js` (walks from the Export button to its `margin: large` container — the same ancestor
  `bottomAlignInRow` uses).
- **Collapse-box bottom line**: `injectTabxCss()` gains `padding-bottom` on collapse-box body candidate
  selectors (a wrong one no-ops, per the existing pattern).
- **Run-comparison button**: `styleRunCompareBtn()` (mirrors `styleResetBtn`) — material grey/black
  button + a blank line below.
- No `.a.yaml` change → `.h.R` untouched, no cache-schema bump. The collapse-box-body and export-block
  ancestor selectors are best-guess and need a live-DOM confirmation on rebuild.

---

## Phase 19k — the boundary stops re-implementing R (2026-08-15)

**The rule this phase installs**: *the module states an intent; R resolves it.* Nothing between a
control and the argument it names, and nothing computed twice.

### The option vocabularies ARE tabxplor's

Both `.a.yaml` files now spell their List values exactly as the R argument does. What moved:

| analysis  | option              | before                                    | after                                                                         |
|-----------|---------------------|-------------------------------------------|-------------------------------------------------------------------------------|
| jmvtab    | `chi2`              | Bool                                      | **renamed `test`** (the test is a Chi-squared only for factors)               |
| jmvtab    | `OR`                | no / OR / OR_pct                          | **deleted** — `display` prints the odds ratio, `ref2` picks its 2×2           |
| jmvtab    | `color`             | no/auto/diff/ratio/contrib/OR             | the full-word measures (`names(MEASURES)`) + no/auto                          |
| jmvtab    | `ci`                | auto/cell/diff/ratio                      | the ANCHOR vocabulary: auto / no / cell / ref                                 |
| jmvtab    | `display`           | 13 values, **4 of them tab() refuses**    | presets that are real `tab(display =)` values, incl. `num_ci`, `{or} ({pct})` |
| jmvtab    | `method_cell`       | wilson/wald                               | + `beta` (the third `CI_METHODS$cell`)                                        |
| jmvtabreg | `exponentiate`,`at` | a checkbox + two radios                   | **deleted** — folded into `effect` × `measure`                                |
| jmvtabreg | `effect`            | coefficient/ame/ame_ratio                 | coefficient / marginal / at_reference                                         |
| jmvtabreg | `measure`           | —                                         | **new**: auto / odds_ratio / ratio / difference / log                         |
| jmvtabreg | `estimate_display`  | —                                         | **renamed `display`**                                                         |
| jmvtabreg | `color`             | Bool                                      | a MEASURE: auto / no / adjustment / between_groups (19e's D25 allow-list)     |
| jmvtabreg | `shapes`            | —                                         | **new** hidden Array: the per-numeric-predictor functional form               |

⚠ **Renaming an option loses its value in saved `.omv` files** (jamovi keys analysis options by
name). Accepted — the module carries no back-compat promise — but it is data loss, not a rename.

⚠ Quote `no` (and `n`) in the yaml. YAML 1.1 reads a bare `no` as the boolean *false*; the previous
files relied on jmvtools coping with that.

### The JS rules are GENERATED

`dev/generate_jamovi_js.R` rewrites, in place, the block between
`// --- BEGIN GENERATED … ---` / `// --- END GENERATED ---` in each `jamovi/js/*.js`. It emits the
family-detection rule + offered families (`REG_OUTCOME_KINDS`), the family labels
(`REG_FAMILIES$ui` / `$ui_binary`, via `reg_family_ui_labels()` -- Phase 19m-i; `ui = NA` IS "not
offered in the picker", which this generator used to write a second time as a hardcoded
`setdiff()`), the three-state estimand grid (`REG_ESTIMANDS`), the default measure per
(family × effect), `REG_SHAPES`, and the odds-ratio display tokens (`DISPLAY_COMPARISON`).
`Rscript dev/generate_jamovi_js.R check` fails when a block is stale, and `test-jamovi-vocabulary.R`
runs it as an assertion. A **marker block, not a second file**: whether jamovi's bundler would
resolve a `require()` of another module is not testable here.

Deleted from the JS with it: `detectFamily` / `familyOptionsFor` / the two label maps (hand-mirrors,
one already stale since 18z13), `anyNonGaussian` + `anyProbScale` (`applyModelEnables` reads the
grid instead), `applyWtEnables` (greying four options that no longer exist), and
`forceNaForCompare`'s `na = "drop_all_models"` — a value removed in z13, so every `compare` change
fired a `setValue` the List rejects.

### `.run()` is weights → build → render

`anova` was the last option travelling as a global (`options()` + `on.exit` around the build, which
also baked it into the tier-3 base key although the p-value line is materialised at DISPLAY). It is
`tab(anova =)` now, stored as display intent in `meta$render_extras` and read back by `tab_anova()`.
`ci_print` keeps its `on.exit` on purpose: it is read inside `format()`, i.e. around the *render*.

### The generated `.h.R` LAGS — design for it

Every `self$options$x` in both `.opts()` takes a `%||%` fallback. Between a `.a.yaml` edit and the
maintainer's next `jmvtools::prepare()`, a newly declared option reads back `NULL`; the module must
then run on defaults, never abort. **The new controls do nothing until that rebuild.**

### Maintainer step

`jmvtools::prepare()` + `jmvtools::install(home = "flatpak")`, then a live pass: the collapse boxes
of both analyses, the reference / model / **shape** pickers (the shape select is a best guess against
a DOM only the running app has), the `display` ComboBox in its new home beside `color`, and export.

---

## Phase 20g-i — the option NAME is the argument name (2026-08-17)

19k made the module speak tabxplor's **values**. 20g-i finishes the sentence with its **names**.

### The rule

> A jamovi option is named after the producer argument it drives — exactly, or as
> `<argument>_<slot>` when several options fold into one — or it is in a declared exception list
> with its reason.

`<argument>_<slot>` covers the three real many-to-one cases: `ci_method_cell` / `_diff` /
`_mean_diff` / `_mean_ratio` → `ci_method`; `ref` (the expert free text) + `ref_levels` (the picker
array) → `ref`; `stats_compare` / `stats_baseline` / `stats_checks` → `stats`. The declared
exceptions are `data`, the export block (`export_*`, `exportExcel`, `resetPath`, `xl_replace`),
`wrap_rows` / `wrap_cols` (renderer arguments), `models` / `run_compare` (the comparison builder),
`ci_print` (an option, read at render time) and **`lvs`** — `jmvcore::Options` already defines a
`levels()` method, so that one name can never be `levels`.

### Why it needed a gate, not a convention

`test-jamovi-vocabulary.R` compared List **values**. Every Phase 20b/20c rename moved an **argument
name**, so the file stayed green while the Regressions panel showed `dependent`, `split_var`,
`method`, `multiplicator`, `shapes` and `refLevels` for arguments that no longer existed — and
`expect_true("shapes" %in% names(o))` pinned one of them in place. Since the UI shows R argument
names *on purpose* (differentiator 4), that is the teaching path lying, silently, for six months.

Three blocks were added:

1. **option names** against `formals(tab)` / `formals(tab_reg)` + the prefix rule + the exception list;
2. **every `.u.yaml` `optionName:`** must name a declared option;
3. **every `ui.<name>` in the hand-written `.js`** must be a declared option, a control, or `view`.

Block 3 is the only test `jamovi/js/*.js` has ever had, and it caught two live misses on its first
run. ⚠ **A control naming a dead option fails silently in jamovi** — it renders inert, and every
CustomControl guards with `if (!ui.x) return;` — which is precisely why this must be mechanical.

### Two facts worth not rediscovering

- **`jmvcore::Options` has no setter.** Its public methods are `asProtoBuf`, `check`, `clone`,
  `compProtoBuf`, `eval`, `fromJSON`, `fromProtoBuf`, `get`, `has`, `initialize`, `levels`,
  `option`, `read`, `translate`, `values`. So **R cannot write a value back into a control** — any
  design that asks the backend to fill a text box (the resolved Documents folder, a detected level
  list) is not buildable. What R *can* do is put the information in the results panel.
- **PO escaping**: an *escaped* `\"` inside a **msgid** is fine (`other_if_less_than`'s title has
  carried one for phases). An *unescaped* `"` inside a **msgstr** breaks the compiler outright with
  `Error parsing PO data: Invalid key name ...`, and the module then builds with no translations at
  all. When hand-editing `jamovi/i18n/fr.po`, escape.

### The i18n cost of a rename, measured

`jmvtools::i18nUpdate("fr")` re-keys `catalog.pot` + `fr.po` from the yaml titles, so **renaming an
option's label discards its translation** (the msgid IS the key). Measured on this phase: 54
translated msgids dropped — **31 already stale** (labels of options retired in 19e / 19k / z14 and
never swept: `ids`, `strata`, `fpc`, `at`, `exponentiate`…) and **23 from these renames**. The 23
were restored by hand: the French had always kept the English argument name and translated only the
parenthetical, so carrying it across is mechanical. Compiled `inst/i18n/fr.json`: 203 → 172.

### The digest fast path hid footer rows — FIXED in Phase 22j

Recorded because it decided UI design for two phases: the old reref digest stored a **digest and no
fit**, so `reg_check_rows()` saw `reg_checks_for(has_fit = FALSE)` and every fit-based row
disappeared — **9 footer rows without the cache, 5 with it**, and a jamovi table had never shown
`global_lr` / `dispersion` / `influence` / `collinearity`. The workaround was the `stats_checks`
control turning the cache off for its build.

Phase 22j removed the cause: the **eager stage** (`reg_fit_eager()`) computes every fit-based row
while the fitted object is alive, so a distilled record carries them and `has_fit` is gone. The
`stats_checks` control keeps the cache on; `stats` is in the key instead, because it decides which
eager rows the record holds.

### Maintainer step

Already run on the dev box for this phase (`prepare()` → `i18nUpdate("fr")` → `install(home =
"flatpak")`), so the committed `.h.R` / `0000.yaml` / `inst/i18n/fr.json` are the compiler's own.
What remains is the **live click-through**: both collapse-box trees, the model / reference / shape
pickers, the new `stats = "all"` box and `add_n`, and export.

---

## Phase 22g-i — the UIs state the final API (2026-08-23)

The panels now say what `tab()` / `tab_reg()` say, and `jmvtools::prepare()` runs again. Four rules
below were learned by hitting them; they are the reason this section exists.

### Two compiler traps that abort or silently rewrite the `.u.yaml`

Both are in `jamovi-compiler/uicompiler.js` and neither produces a usable error.

- **A bare `children:` is a YAML null, and the compiler dereferences it.** `removeMissingOptions`
  guards with `ctrl.children !== undefined` (line 209), not truthiness, so it recurses into the null
  and line 205 evaluates `null.length` → `TypeError: Cannot read properties of null (reading
  'length')`, with no file or line. One empty spacer `LayoutBox` — left behind when Phase 22b-ii
  retired the `ci_print` radios — blocked **every** `prepare()` for two phases. **Remove the node.**
  ⚠ `children: []` is *not* the fix: an empty container is spliced out (line 213), which makes the
  compiler **rewrite the whole file with `yaml.dump()`**, comments and all.
- **A `optionPart` radio group must cover EVERY value of its List.** `insertMissingControls` appends
  a loose control for each missing value and rewrites the file — measured here: `jmvtabreg`'s
  `display` offered 7 of 10, and one `prepare()` cost the file all 24 of its comments. The three
  orphans (`est`, `est_coef`, `base_ratio`) had been declared by 22a-i / 22b-iii / 22c-ii and never
  given a button. Guard it the way the suite now does: walk both files and compare each
  `(optionName, optionPart)` set against the `.a.yaml`'s values.

**Checksum the `.u.yaml` files before `prepare()` and verify after.** A silent rewrite is the one
failure that costs work rather than time.

### `check` is reserved, exactly as `levels` is

`jmvcore::Options` defines a `check()` method, so an option named `check` collides with it and the
analysis cannot be instantiated at all: `makeActiveBinding: symbol already has a regular binding`,
raised from the generated `.h.R`, naming nothing. The full reserved set is
`asProtoBuf · check · clone · compProtoBuf · eval · fromJSON · fromProtoBuf · get · has ·
initialize · levels · option · read · translate · values`. The model-check control is `xl_check`,
beside `xl_replace`; `levels` is `lvs` for the same reason.

### The Model table is the cascade's left half

`family` and `link` are questions about each OUTCOME, so both are per-outcome hidden Arrays driven
by `modelTableCtrl`, which now renders a header row naming them (`outcome | family = | link =`); the
4th cell stays unheaded because it holds whichever of `outcome_level` / `trials` applies. `measure`
and `effect` — the right half — stay scalar radios. The link drop-down offers exactly
`TABX_LINKS[family]`, so an unfittable link is *unreachable* rather than greyed, which deleted
`linkOffered()` and the whole link branch of `applyModelEnables()`; `measureOffered()` now asks each
outcome with its own link. One table can carry two links (measured: `Model_RR [married]` beside
`Model_diff [tv]`).

⚠ **Moving a List into the table costs its translation.** A JS-rendered label is outside
`catalog.pot`, so the four link labels are English-only — the property `TABX_FAMILY_LABEL` has had
since Phase h. jamovi's options UI has no gettext; the alternative is keeping a scalar List.
The labels themselves are still generated, not hand-written: `reg_link_ui_labels()` composes them
from `REG_MEASURE_LINK` (the measure, then the glm spelling that map already carries).

### Three defects the wiring exposed, all reproduced

1. **`rlang::inject()` + `!!` hid every interaction.** `reg_cross_slots_quo()` fell back to
   `quo_peek_extern()`, which only reads a bare **symbol** — but `!!` splices a *literal* vector, so
   `a*b` was never seen and tidyselect tried to select a column of that name. `jmvtab_reg_build()`
   builds its call that way on purpose (an injected value cannot be hijacked by a same-named
   column), so the `crosses` fold could never have worked end to end. Fixed in `reg-cross.R`, where
   crosses are read.
2. **`empirical = "no"` was a declared value the validator refused.** `TAB_ARGS` declared
   `c("no", "cell", "column")` and `emp_on()` accepted `"no"`; only `reg_validate_args()` held a
   literal `c("cell", "column")`. It reads the fact table now.
3. **The per-outcome arrays must be folded in ONE place.** `family` / `outcome_level` / `trials`
   resolve inside `jmvtab_reg_build()`; putting `link`'s fold in `.b.R` instead meant the raw array
   reached `tab_reg()` and the pick did nothing. All four resolve together in the build core, which
   is also what keeps it testable from the raw jamovi arrays.

### The interaction picker

`crosses` had been declared and folded since 22b-ix with **no control anywhere**. `crossPickerCtrl`
(its own *Interactions* CollapseBox, after Model) is a row per pair — two drop-downs over the
predictor pool and a delete — writing the hidden array `jmvtab_reg_cross_keys()` folds into
`predictors` as `a*b`. Picking the variable already on the other side steps that side to the first
free one, because `a*a` is a refusal. Signature = the pool alone, so a pick repaints in place.

### The `.js` now has a syntax gate

`node --check` on both files, skipped where `node` is absent (`test-jamovi-vocabulary.R`). Declined
in 19n for want of an interpreter; the box has one. It is the only thing standing between a typo and
an options panel that renders inert with no R-side symptom.

### Maintainer step

`prepare()` → `i18nUpdate("catalog")` → `i18nUpdate("fr")` → `install(home = "flatpak")` all run
here, so the committed `.h.R` / `0000.yaml` / `fr.po` / `inst/i18n/fr.json` are the compiler's own,
and the 37 new French strings are filled in (199/228 translated; the rest are argument VALUES kept
English on purpose). What remains is the **live click-through** — the Model table's two headed
columns, the greying of `measure` / `effect` against a mixed-family outcome set, the Interactions
box, the crosstab `display` ComboBox, and an Excel export with `check = "auto"`.

---

## Phase 22g-iii — the panels ask what the API asks (2026-08-23)

The maintainer's click-through of the 22g-i build. Everything below is a fact that cost time to
establish; the layout itself is in the `.u.yaml` files and needs no second telling.

### `visible: false` is the way to hide a state carrier

Both analyses persist their live-UI store in a 0-size `Image` result element, because only an Image's
`$state` survives the engine reset. `height: 1` still reserved a slot, and two of them under a table
is a visible band of nothing. `visible: false` removes it, and it is **safe for the store**:
`jmvcore`'s `ResultsElement$asProtoBuf()` writes `state` in a branch of its own, independent of the
`visible` field it computes just above — so the round-trip is untouched. The compiler agrees: the
`Image` schema (`jmvtools/node_modules/jamovi-compiler/schemas/resultelementschemas.yaml`) lists
`visible: boolean | string`. ⚠ The schema also claims `height` has `min: 32`, which is not a
JSON-Schema keyword and is not enforced — `height: 1` has always compiled.

### A pre-aggregate recode MUST be in the tier-1 keys, and a rename hides its fingerprint

`shape` is the second thing jamovi can change that recodes a column **before** it is counted (the
level merge was the first). `ce$fp_map` fingerprints the **raw** columns in `jmvtab_build()`, so
without a key entry a cut would be served the un-cut aggregate. Both now travel as ONE per-variable
slot (`jmv_cache_aggregate()`'s `recode()`), in the tier-1a, tier-1b and tier-2 keys.

⚠ **The trap is the rename, and it is silent.** A numeric-keeping shape (`log` / `sqrt`) on a column
variable renames its column to `log_age`, so the tier-1b key's `msr` is `log_age` while
`fp[["log_age"]]` is `NULL` — and `list(a = 1)[["b"]]` returns **NULL, not an error**, so the source
column's fingerprint drops silently out of the key and an edit to `age` would not move it.
`shape_rename_transformed()` therefore returns its `renames` map, `tab()` puts it on the ctx, and
every fingerprint lookup in the aggregate goes through it.

### `"auto"` is not a `shape` value

`shape_value()` aborts on it: `"auto"` is the *filling rule* for a numeric row/tab variable the user
did not name, i.e. the **absence** of an entry. Both pickers lead with it and store nothing for it,
exactly as the regression picker has always stored nothing for `"linear"`.

### The two shape lists are derived, not written

`dev/generate_jamovi_js.R` emits `TABX_SHAPES_INDEX` (a row/tab variable can only be CUT) and
`TABX_SHAPES_COL` (a column variable may also keep a number) from `VAR_SHAPES$produces` — the same
fact `shape_refuse_numeric_index()` enforces R-side. Adding a shape row is still one line.

### What a widget's width moves with

The level box changed width on nearly every click, and all three causes were in one style string:
`grid-template-columns: 1fr auto minmax(96px,1fr)` sized column 2 to the tick-box (so it moved the
moment one appeared) and grew column 3 with whatever was typed in it, while `overflow-y: auto` took
width away when the list got long enough to scroll. The cure is fixed pixels on the two right
columns, `width: 100%`, and **`scrollbar-gutter: stable`** (Chromium 94+, which jamovi's Electron
is well past).

### `stats =` had to go, not be relabelled

Phase 22g-ii made the model comparison automatic, so `stats_compare = "none"` produced a comparison —
a picker naming the opposite of what it did. All three controls are deleted (`stats_compare`,
`stats_baseline`, `stats_checks`) and `jmvtab_reg_stats()` with them; the panel sends `stats = NULL`,
which is `tab_reg()`'s own default. `stats_checks` was only ever needed because the old digest hid
the fit-based footer rows, and Phase 22j's eager stage removed that reason.
⚠ **`forceNaForCompare()` had to be re-keyed, not deleted**: it watched `stats_compare`, and without
it a user comparing under `na = "drop_by_model"` silently gets a bare ΔAIC instead of an LR test. It
reads the model-card COUNT now.

### One predictor list with several outcomes is a per-outcome table

`is_comparison <- is.list(predictors)` (`R/reg-resolve.R`), and a comparison must have ONE outcome —
so a single model card beside two outcomes had to arrive as a character VECTOR, not a one-element
list. `jmvtab_reg_models(..., flatten =)` does that; the card's typed name is what it costs. Two
cards and two outcomes is still refused, and the message now names both cures.

### The `.js` gained a shared column fetch

Three near-identical `requestData("column", …)` blocks per file (the level tree, the reference
picker, the ref2 section) are one `fetchLevels(ui, ctrlName, v)`. It caches `measureType` beside the
labels, which is what an ordinal predictor's greyed ▲/▼ and a continuous one's `shape` row both read.

### A `*_OF_RADIO` map is a third place a value lives

`jmvtabreg.js` maps a radio's **control name** to the value it sets (`MEASURE_OF_RADIO`,
`EFFECT_OF_RADIO`), which is what `applyModelEnables()` greys against. Re-ordering `measure` moved
every pair, and **nothing could see it**: the `.a.yaml` still declared five values, the `.u.yaml`
still offered five buttons, so the coverage rule stayed green while the wrong buttons greyed.
`test-jamovi-vocabulary.R` now reads those object literals out of the source and compares them
against the `(name, optionPart)` pairs the `.u.yaml` declares. **Reordering a List is not a
cosmetic change** while a map keys on `<option>_<n>`.

---

## Phase 22g-iv — the per-variable table (2026-08-24)

One `varTableCtrl` replaces `levelsCtrl` + `refPickerCtrl` in BOTH panels: one row per variable, four
aligned columns, the level list opening inline. Read this before changing it — two of the three rules
below were paid for with bugs.

### The contract: a HOST descriptor, not a branch

The widget lives in the `BEGIN/END SHARED` span of `jamovi/js/jmvtab.js` and is copied verbatim into
`jmvtabreg.js` by `dev/generate_jamovi_js.R` (`check` mode fails on drift, asserted by the suite).
It has no per-panel branch. Everything a panel differs by is one declared object outside the markers:

| field | what it says |
|---|---|
| `ctrl` | the CustomControl's name — the shared code reaches it as `ui[host.ctrl]` |
| `cols` | `[{key, head, width, tip}]`; `key` is one of `name` / `levels` / `ref` / `act`. The grid template is built from `width`, so ONE grid carries the head row and every data row |
| `groups(ui)` | `[{label, vars, numericMayKeep}]`; `label: ""` prints no group head. `numericMayKeep` is the AXIS rule — only a column variable (or a predictor) may stay a number |
| `sig(ui)` | the rebuild signature — see below, this is the load-bearing one |
| `shapes(g)` / `isCut(sh)` | the `shape` drop-down's values, and which of them make the variable a FACTOR (both derived from `VAR_SHAPES`, emitted as `TABX_SHAPES*` by the generator) |
| `orderOpt` | the level-ORDER option, or `null`. ⚠ Reached only through this field: `tab_reg()` has no `levels_order`, so naming it in the shared code would claim an option one panel cannot declare |
| `canOrder` | whether the ▲/▼ bar exists at all |
| `refCell` / `unitCell` | the two genuinely divergent cells |
| `varSync(ui, v, kind)` | drop a stored option a `shape` change invalidated — see below |
| `reconcile(ui, all)` | the panel's own stale-entry sweep (`jmvtab` reconciles `ref_levels` to the ACTIVE axis, which is what stops a cross-axis reference reaching R) |

`kind` is computed once, by the shared code, so the two hosts cannot disagree about what a variable
IS: `{cached, mtype, offered, defShape, shape, loading, isNumber, isCut}`.

### ⚠ The signature rule

`host.sig()` names ONLY what the table does not itself write: the variable boxes, plus `pct` /
`color` / `display` in `jmvtab`. **`levels_order`, `levels_collapse`, `shape`, `ref_levels`, `ref2`
and `multiplier` are OUT of it**, and each repaints in place through `tabxvRefreshVar()`.

This is not an optimisation. With two controls the signatures could include each other's writes,
because a rebuild of the OTHER control clobbered nothing. With one control, a merge tick that
rebuilt the table would destroy the list the tick was made in — the "2nd click does nothing, then
all changes appear later" bug the older headers warn about. So: a tick or a ▲/▼ move calls
`renderRows()` (in place) plus `tabxvRefreshVar()`, which repaints that variable's `levels` count
and its `ref` cell, because the post-merge levels ARE the reference choices.

### ⚠ A `shape` pick can leave an option that ABORTS the build

`reg_check_continuous_names()` aborts on a `multiplier` naming a factor, and an anchor keyword cannot
name a level — so when a `shape` pick turns a number into a cut factor, `host.varSync()` drops the
stored `multiplier` and any `ref` from the wrong vocabulary. A picker must not be able to write a
combination the producer refuses.

### `ref2` has no control of its own

While an odds ratio is in force (`orIsActive()`), the second reference borrows the reference cell of
the **first variable of the other axis**, with a `title=` tooltip. Every other off-axis cell is
EMPTY: a reference the table does not use must not be offered as though it did.

### What the wiring test now checks, and what it deliberately does not

`test-jamovi-vocabulary.R` gained: *every `CustomControl` declared in a `.u.yaml` has
`<name>_creating` / `<name>_updated` exported by its `.js`, and every such export names a declared
control* — the failure a rename across two files produces, and it is silent (an empty box, no R code
run). It LOST both value-set-equality blocks: a panel chooses which values to offer and in what
order, and pinning that to the R declaration made every UI edit a test failure while catching
nothing a user meets. The value-COVERAGE assertion stays — it is not a vocabulary check but the
guard against the ui compiler appending controls and rewriting the `.u.yaml` with `yaml.dump()`.
⚠ It now spells YAML booleans back: **`optionPart: no` parses as FALSE** under YAML 1.1.

### Round 2: the layout, the greys, and three reserved-name / collapse traps

**One grid per GROUP, one header row, the group's name as its first column head.** `host.cols[0].head`
is only a fallback; `g.label` wins. ⚠ **Every name column needs a minmax FLOOR**: with
`minmax(0,1fr)` behind 510px of fixed columns the name column collapsed to zero width in a narrow
options pane and the variable names were rendered but invisible. The Model table had the same defect
in its 4th column.

**Colours**: the table `#E4E4E4`, its head `#CCCCCC`, an open level list `#F0F0F0` in an `#E4E4E4`
well. Pure white is reserved for inputs (selects, text boxes, buttons) — it is what makes them read
as inputs inside a grey pane.

**`cleannames` in the widget.** `TABXV_CLEAN` is `cleannames_condition()` transcribed, and
`tabxvClean()` is applied to every label a human reads. ⚠ Stored values stay RAW everywhere
(`data-lab`, `<option value>`, all three options). ⚠ The regex is built by `new RegExp` in a `try`:
its lookbehind is a PARSE error on an old engine, which would kill the whole file. The merged-run
default label rule (first level whole, followers cleaned) lives in R, in `new_lvl_collapse()`, so a
jamovi user and an R user get the same name.

**`jmvtabreg`'s level order is a jmvtabreg-only prep step.** The panel declares its own hidden
`levels_order`; `jmvtab_reg_build()` relevels the predictor columns before the fit, in RAW names
(the merge runs afterwards and `fct_collapse()` keeps first-appearance order). No cache entry is
needed — `jmvreg_fit_key()` fingerprints the prepared frame's levels. `host.varSync()` is the single
place the "baseline IS the first level" invariant is repaired, in both directions.

⚠ **`theme` is a reserved jamovi option name.** jamovi injects its own global `theme` (the app's
plot-styling preference) into every analysis, so a module option of that name never holds a value:
no radio ticks, a click reverts, and the backend reads the app's word. Renamed `tab_theme`. The
reserved set now known: **`levels`** (a `jmvcore::Options` method), **`check`** (likewise), and
**`theme`** (injected by the client). None of the three raises anything anywhere.

### Round 3: two ordering traps, and the gate `node --check` cannot be

⚠ **Write the option BEFORE firing the callback that reads it.** `tabxmBuildList`'s `move()` called
`commit()` (→ `onCommit`) before `onOrder(order)`, so any host deriving state from the stored order
was one move behind. A host callback fired from inside a widget must see the world the widget has
already written.

⚠ **A change made OUTSIDE the level list must rebuild it; a change made INSIDE must not.**
`tabxvRebuildList()` exists for the first (jmvtabreg's `ref =` cell reorders the levels); calling it
from `onCommit` would detach the grid that handler is about to repaint in place.

⚠ **A `.js` that parses can still be dead.** Deleting a top-level helper that is still called leaves
the jamovi options pane loading forever — no exception reaches R, no test sees it, `node --check`
passes. The suite now asserts that every identifier a `.js` calls is declared somewhere in it. It is
permissive about scope on purpose: the failure worth catching is a helper that is simply gone.

⚠ **`width: 100%` plus a horizontal margin overflows.** It cost the Model table a few millimetres
past its card. A block-level grid already fills its container; give it margins OR a width, never both.

---

## Phase 22g-vi — the yaml pass, and three facts about jmvcore (2026-08-25)

The build chain ran once for every UI item of the round. What follows is only what cost time to
establish, or would cost it again.

### ⚠ `$.Options` STOPS on an unknown option — it does not return `NULL`

```r
`$.Options` <- function(x, name) {
  if (!exists(name, envir = x)) stop("options$", name, " does not exist", call. = FALSE)
  x[[name]]
}
```

So a helper shared by both backends may never reach for an option only one panel declares.
`jmv_backend_export()` read `self$options$xl_check` behind a comment asserting that jmvtab "reads
back NULL" — and **every jmvtab export died there**. The one guarded read is `jmv_opt(self, name,
default)` (`R/jmvtab-export.R`), built on `Options$has()`; use it for any panel-specific option
reached from a `jmv_backend_*` helper. This is a third member of the reserved/asymmetric-name class
alongside `levels` → `lvs`, `check` → `xl_check` and `theme` → `tab_theme`.

### ⚠ A state-carrying Image must be `visible: false`, and the reason is not vertical space

`jmvcore::Image$asProtoBuf()` has this branch:

```r
else if (status == ANALYSIS_COMPLETE && (!is.null(self$state)) && path == "")
    result$status <- jamovi.coms.AnalysisStatus$ANALYSIS_RENDERING
```

A hidden state carrier writes no file (`.plot` returns `TRUE`), so `path` is `""` **whenever it holds
a state** — and it is therefore reported as *still rendering*. Left VISIBLE, the client then asks for
that render, and the round-trip overwrote the run's own results: the staged comparison appeared and
was replaced a moment later by the "Model comparison staged" banner. `compare_state` had its
`visible: false` commented out; `jmvtab`'s `cache_state` had too. Both are hidden again.

State itself is orthogonal to visibility — `ResultsElement$asProtoBuf()` writes `state` in a branch
that never reads `visible` — so hiding a carrier costs nothing, which is what makes this safe.

### ⚠ `$state` has a documented ceiling: 500 000 bytes, compressed

Same function: past `5e5` jmvcore prints *"state object for … is too large"* and points at
`dev.jamovi.org/tuts0203-state.html`. The staged comparison stored its whole rendered HTML there.
It now stores the signature always and the render only while it fits, with a two-entry process-local
mirror (`JMVREG_RENDERS`, `R/jmvtabreg-cache.R`) that re-serves inside a live engine either way.

### A native `<option>` renders no markup

`opt.textContent` is the only thing a `<select>` paints, so the `shape` picker's annotations are
plain text — `linear (numeric)`, `sd_bands (cut)`. They are **derived** from `VAR_SHAPES$produces`
by the generator (`TABX_SHAPE_LABEL`), not written in the `.js`.

### Two orders that must not borrow each other

`REG_FAMILIES[[f]]$fits` is ordered so its FIRST entry is the family's own link, which is what
`link = "auto"` resolves to. The Model table's drop-down is ordered like `measure`'s own radios
(`auto, difference, ratio, odds_ratio`) — a link IS a measure, and one order down the cascade is
what a reader can carry. The sort is in `dev/generate_jamovi_js.R`, on the emitted `TABX_LINKS`
alone; the suite asserts both halves.

### The outcome-level picker is gated on a declared role

`TABX_OUTCOME_LEVEL_ROLE` (from `REG_FAMILIES$<f>$outcome_level`) says which families take an
`outcome_level` and what it IS to them — `modelled` (binomial: one level against the rest) or
`baseline` (multinomial). The old gate was "the outcome has exactly 2 levels", which hid the picker
on both families that need it most.

---

## Phase 22g-viii — the interactions picker, folded into the model builder (2026-08-25)

`crossPickerCtrl` and its *Interactions* CollapseBox are gone; `modelBuilderCtrl` renders both
halves — an interaction is a PREDICTOR, so it is defined where a model says which predictors it
holds. Two facts below cost time to establish.

### ⚠ A card's `vars` cannot hold an `a*b` key

The obvious design — put the key straight into `models[].vars` — does not work. `vars` is
`type: Variable`, and `jmvtab_reg_models()` does `intersect(pool, card$vars)` against the real
column pool, so the key is dropped on the R side. Retyping `vars` to `String` would fix that at the
cost of the cards' **rename-safety**: jamovi rewrites a `Variable` option when a column is renamed,
and a card silently losing a predictor is a wrong model.

So a card carries a SECOND list, `crosses: Array of String`, holding the keys it ticked. The nested
`Array{Group{String, Array{Variable}, Array{String}}}` compiles cleanly (verified against the
generated `.h.R`). Two sweeps keep it honest, both in `reconcileModels()`: a key no longer in the
`crosses` option is dropped, and so is any parent of a key the card still holds — because
`reg_parse_crosses()` refuses a pair named beside its own parents, and a picker must never write a
combination the producer refuses.

### ⚠ The old fold made a with/without comparison IMPOSSIBLE

`jmvtab_reg_cross_fold()` replaced both parents by the key in **every** card that held them, so once
`age*race` was defined, a card holding `age` and `race` *became* the interaction model. The comment
claiming this is what made with/without expressible had it backwards. The fold now runs on the
ZERO-CARD path only (no card → nowhere to tick → every defined pair applies to the single live
model), and a card states its own interactions.

Measured on `gss_cat`, two cards over `race + age + relig`, the second ticking `race × relig`: the
footer carries **`compare_seq` χ² = 36.42, df = 14, p = 9.0e-4**, exactly equal to that model's own
`cross_lr` row. So it is a real sequential LR, not a ΔAIC fallback — `reg_cross_expand_terms()`
expands the combined factor back to its parents, which is what lets `reg_compare_chained()` see the
nesting between two models whose term labels share no name.

### ⚠ `jmvtab_reg_staged()` needs the keys too

A card holding ONLY an interaction has an empty `vars`; without `cross_keys` it reads as an empty
card, is dropped, and a two-model comparison would run LIVE instead of behind the Run button. The
predicate and `.opts()` must be handed the same keys.

### Three pairs the picker cannot express, all unreachable rather than repaired

The rule that deleted `linkOffered()` in 22g-i, applied three times. `syncCrosses()` and
`jmvtab_reg_cross_keys()` keep their own guards behind them, because a pair stored by an older build
must not reach R either.

| refused | why | how |
|---|---|---|
| `a*a` | meaningless | picking the variable already on the other side **swaps** the pair — which is what the user meant, and it keeps the flip one click away |
| `a*b` beside `b*c` | a three-way interaction, which tabxplor does not fit — the second pair simply would not apply | a variable another row uses is not offered (`crossClaimed`); `+ Add interaction` seeds the first two FREE variables and greys out below two |
| `race*age` | `reg_cross_resolve()` reads it as `age*race` and says so — the rows are about the FIRST variable, and only a continuous one has slopes within groups | `crossOrder()` puts the pair in that order here, so the two column heads stay honest and R has nothing to announce |

⚠ **`crossOrder()` must mirror R's `kind()`, which reads the column AFTER `shape`**: a number cut
into groups IS a factor there, so `crossKind()` checks `cachedLevels(v)` *and* the stored `shape`
against `TABX_SHAPES_CUT`. While a column is still being fetched the kind is unknown and nothing
moves — a wrong guess would flip a pair the user typed on purpose.

⚠ **A swap renames the key, and a card stores the key it ticked.** `syncCrosses()` therefore returns
`{keys, renames}` and `reconcileModels()` applies the renames FIRST — otherwise the card finds its
key undefined and silently drops the interaction.

### What re-renders, and what must not

`modelsSig` stays `[pool, outcome.length]`, so `models` and `crosses` are outside it. Add / delete /
a `<select>` pick change what the OTHER half shows — a card's chip labels, a row's own option list —
so those three call `renderModelBuilder()` synchronously in their handlers (a `<select>` has already
committed and lost focus when `change` fires, so this is safe under 22g-iv's rule). A tick and a
name keystroke repaint in place, or the edit that caused the rebuild is clobbered.

### `effect` became a ComboBox, and that deleted `EFFECT_OF_RADIO`

With no `effect_1..4` radios in the `.u.yaml`, the map named four controls that do not exist —
which `test-jamovi-vocabulary.R`'s `*_OF_RADIO` gate and `ui_bracket_names()` both catch.
`applyModelEnables()` now greys `measure` only; a ComboBox cannot grey one of its own items, the
same price `display` paid in 22g-iii, and an unavailable combination is refused R-side by name.

### The greyscale is one declared ladder, in literal hex

The `TABX` boxes used `rgba(0,0,0,0.0x)` overlays, so the same key rendered two shades depending on
what it sat on. They are literal hex now, the same ladder the shared per-variable table already used,
recessed → raised (material reads elevation as lightness in a light theme):

| | |
|---|---|
| `#CCCCCC` | a header row in the SHARED per-variable table, whose heads label a grid of cells |
| `#DCDCDC` | a **well**: the box holding a list of rows AND the `+ Add` button that grows it |
| `#ECECEC` | a **card** raised on that well: one interaction row, one model, one outcome |
| `#FFFFFF` | an **input**: a `<select>`, a text box, a button — reserved for them, and what makes them read as inputs inside a grey pane |

The `+ Add` button lives INSIDE its well, so a list and the control that grows it are one object,
and `TABX.sectionHead`'s top margin is the blank line between two wells.

⚠ **A head standing over CARDS takes no fill at all.** Both `mtHead` (`outcome` / `family =` /
`link =`) and `crossHead` sit directly on the well: a filled bar there reads as one more row of the
list rather than as its heading. `crossHead` is additionally in italic at the row's own type size,
because it is a plain **caption** — it describes the column, it does not name an option.

### ⚠ A `<button>` in a grid track overflows the row

A grid item defaults to `min-width:auto`, so a button's intrinsic width widens its own track past the
declared `24px` and pushes the whole row off the right edge of a narrow options pane — which is why
the delete `×` could not be reached. `TABX.xDel` sets `min-width:0; width:100%; padding:0`. The two
`<select>`s were narrowed to `minmax(48px,1fr)` with `gap:6px` for the same reason: the pane is
~340px at its narrowest, and the row must fit there.

---

## ⚠ `clearWith` — why a `$state` carrier lost its state on every option change (2026-08-25)

**The symptom.** Clicking *Run comparison* left the "Model comparison staged. Click **Run
comparison** to compute the table" banner in place, apparently forever.

**The cause, read out of jmvcore's own source.** `jmvcore::Image$new()` defaults `clearWith` to
`"*"`, and `ResultsElement$fromProtoBuf()` opens with

```r
someChanges <- length(oChanges) > 0 || length(vChanges) > 0
if (someChanges && base::identical("*", private$.clearWith)) return()
if (base::any(oChanges %in% private$.clearWith))            return()
```

so with the default the state is **not restored the moment ANY option changes** — and an option
change is the only thing that makes an analysis re-run. The staged flow is exactly that: clicking Run
writes `run_compare = TRUE`, the `.js` writes it back to `FALSE` 2 s later, and on that second run
`compare_state$state` is `NULL`, so `.run()` takes the `is.null(last)` branch and paints the banner
over the table the trigger run had just computed.

**The fix is one line per carrier**: `clearWith: []` in the `.r.yaml`, on `jmvtab`'s `cache_state`
and on `jmvtabreg`'s `cache_state` *and* `compare_state`. The compiler emits `clearWith=list()`;
an empty vector fails both guards, so the state is restored. These stores are the module's own and
are invalidated by SIGNATURE (`jmvtab_reg_compare_sig()`, the cache keys), never by jamovi's option
diff — clearing them on an option change is precisely wrong.

⚠ **It also silently degraded both live caches**, for the same reason and since they were written:
a store that is dropped whenever an option changes can only ever hit when nothing changed.

⚠ **`visible: false` was NOT the cure, and Phase 22g-vi's reasoning for it was wrong.**
`Image$asProtoBuf()` reports a state-holding image that wrote no file as `ANALYSIS_RENDERING`
*whatever* `visible` says — the branch never reads it:

```r
else if (status == ANALYSIS_COMPLETE && (!is.null(self$state)) && path == "")
    result$status <- ANALYSIS_RENDERING
```

so hiding the carrier could not have stopped a render round-trip. It is still worth keeping (no
vertical space, and `state` is serialised in a branch of `ResultsElement$asProtoBuf()` that ignores
`visible`), but the flicker it was credited with fixing was this.

---

## Phase 22g-x — what a level reorder is, and what the fit cache holds (2026-08-25)

### The ▲/▼ bar writes a DISPLAY order, and `ref` is the only half the fit sees

`reg_fit_frame()` strips the `ordered` class from every factor **predictor** before the fit — its own comment says why: polynomial contrasts that no per-level skeleton can align. So every predictor is fitted under **treatment contrasts**, in every family, and its level order decides exactly one thing: which level the others are compared to.

That splits the panel's one control into the two facts it always was:

| the widget writes | what it is | what it costs |
|---|---|---|
| `ref_levels` (= `order[0]`, kept in sync by `varSync`) | the model's baseline | a relevel of the data → an honest refit |
| `levels_order` (the rest) | the row order | `tab_reg(.levels_order =)` → a permutation of the row skeleton → **a cache hit** |

`jmvtab_reg_build()` therefore no longer calls `jmv_relevel_cols()`. It hands the order to `tab_reg()` after translating it to merged names through `jmv_order_after_collapse()` — the skeleton is built *after* `.levels_collapse` has run, so an untranslated order would name levels the table does not have.

**Nothing in the `.js` changed.** `varSync` still writes `ref := order[0]` and `regRefToFirst()` still moves a picked reference to the front, so `order[0] == ref` stays true and `boldFirst` stays right.

⚠ **One narrow consequence**: the data relevel used to reach `reg_cross_column()`, which bakes both parents' level orders into a materialised combined factor — so a ▲/▼ move also permuted an interaction block's rows. It no longer does; that block's row order is `expand.grid`'s.

⚠ **The greying of the arrows on an ordered factor was already there, in the shared block** (`tabxmBuildList(..., canOrder = mtypeCache[v] !== "ordinal", ...)`), so both panels have it. It is keyed on jamovi's own **`measureType`**, the only signal `requestData("column", …)` gives the JS — an R `ordered` factor whose jamovi measure type is *Nominal* will show live arrows, and no `.js` change can see past that. After this phase the greying states MEANING only: a reorder could not move a number even if it were allowed.

### The `"fit"` tier holds two kinds of record

The observed (crude) univariable fits now ride the same store, tier and seam as the model fit — `reg_fit_cached()` (`R/reg-digest.R`), used by `reg_spec_build_one()` and `reg_empirical_fit()` alike. They are told apart by the key alone: a crude key is a synthetic one-predictor spec whose `extra` leads with `"crude"`.

⚠ **`drop_extra` is a key member, and its columns are FINGERPRINTED.** It names the variables whose missing values narrow a fit's complete-case population without appearing in its formula — the other models' predictors under `na = "drop_all"`, and, for a crude fit, the whole predictor set minus this one. `jmvreg_fit_key()` used to hash only the outcome, this spec's predictors and the design variables, with `na_shared_vars` riding in `extra` as names: a value edit to one of those columns moved the domain and not the key.

⚠ **With a cache present, a record is distilled on the MISS too** (`jmv_store_cached()` returns the value it stored), so *every* consumer of a crude record must read `reg_model_of()`, never `$fit` — the cold path is a digest path as well. Three sites demanded a fitted object and were fixed here; the one that bites hardest is `reg_gap_se_columns()`'s numeric arm, where `$fit` silently drops `color = "adjustment"` for every numeric predictor.

### Where the drop-down latency actually is

`fetchLevels()` caches per variable and never repeats, and a `ref` pick's own handler is a bare option write — so the widget does nothing heavy. Every var-table control writes an option that is in `.opts()`, so **a pick re-runs the analysis, and that re-run is the latency**. Measured on a warm store, multinomial (`dev/benchmarks/results_2.0.0/phase22gx_crude.txt`): a re-apply 1.85 s, a reorder 1.85 s (was 14.8), a `ref` pick 14.7 s, a `multiplier` pick 14.8 s. The last of those is the one that need not be slow — see the `multiplier` item under Phase 22x.
