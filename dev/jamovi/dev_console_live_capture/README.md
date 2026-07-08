# dev/jamovi/dev_console_live_capture — live runtime capture

Dev-console (F10 DevTools) capture of a running **Jamovi 2.6.44.0** with **tabxplor 1.3.1**
loaded and a basic crosstab open. It is the ground-truth runtime: the served/compiled module,
the framework bundles, and the rendered app HTML. Captured by the maintainer 2026-07-08.

**The analysis of these files is written up in `dev/tabxplor_1.4.0_jamovi_dev.md` §5–§7**
(runtime architecture, analysis-UI framework, results renderer). Read that, not the minified
bundles — the bundles are for re-grepping specific string literals only (never `cat` them;
lines run 50k–325k chars).

## What each artifact is

| Path | What | Guide § |
|------|------|---------|
| `Jamovi_tabxplor_1_3_1_basic_table.html` | Rendered app window: outer DOM, the sandboxed options + per-result iframes, ports, sizing, ribbon Syntax/Dev toggles | §5.1, §4 |
| `127.0.0.1_56680_MAIN_ELECTRON/` | The Electron app shell. `assets/main-fd7ff1c3.js` = coms protobuf protocol, module load, action/`setOption`, save dialogs, F10/F9. `config.js` = the 3 server roots. `modules/tabxplor__v_1.3.1.0` = **the served/compiled tabxplor module** (manifest YAML + `uijs` browserified blob; note `rVersion: 4.4.1-x64`). `modules/*` = other served modules. | §5.2, §5.3, §14 |
| `127.0.0.1_56683_..._analysis_UI/` | The options-panel control framework. `assets/analysisui-49b1a9ac.js` = the `DefaultControls` registry, control/option API, events, `LevelSelector`, templated `ListBox`/`applyToItems`, `CustomControl`. `.css` = control styling. | §6 |
| `127.0.0.1_56684_results/` | The results renderer. `assets/resultsview-60a5863d.js` = Html injection (`content`/`stylesheets`/`scripts`), inline-`<style>`-yes / inline-`<script>`-no, `<a>`→openUrl, 500px width, images, Copy/Export menu, Notice. `.../aa145378.../2/res/02 jmvtab/resources/*.png` = tabxplor plot resources (addressing example). | §7 |
| `*.zip` | Raw origin exports (`MAIN_ELECTRON.zip`, `results.zip`, `tabxplor_jmvtab_analysis_UI.zip`). | — |

## Headline runtime facts (full detail in the guide)

- Options panel + each result are **separate sandboxed iframes** (`allow-scripts
  allow-same-origin`) on localhost ports; they coordinate only via `postMessage`/option values.
- A served module = manifest YAML + a `uijs:` key holding the browserified `.u.yaml` layout
  tree + events. **The compiler embeds `.js` comments verbatim** (295 commented lines shipped
  in tabxplor's blob → clean `jmvtab.js`).
- **The module runs in Jamovi's bundled R (4.4.1-x64), not the user's R** — hence
  `Sys.getenv("USERPROFILE")` over `~` for export paths.
- Every option change = a full `perform=INIT` coms round-trip with `changed:[names]` +
  `revision`; **no client display-only shortcut** → cache R-side.
- Results HTML: inline CSS works (scope it under a wrapper), inline JS does not, no Bootstrap;
  fixed 500px container → use an own `overflow-x:auto` box, not `scroll_box(1080px)`.

## Refresh method

Run Jamovi + tabxplor, add a crosstab, F10 → DevTools. Save each origin (Sources/Network) and
the page HTML; drop them here. To dump a specific DOM: select the node → Copy → Copy outerHTML,
or Console `copy($0.outerHTML)` / `copy(ui.view.el.outerHTML)`.
