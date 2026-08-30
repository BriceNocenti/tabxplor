<!--
PURPOSE: Can R and jamovi be joined into one workflow? Four candidate designs, measured.
ROLE:    A dated ONE-OFF feasibility study (2026-08-30), not a standing guide. It answers four
         questions the maintainer asked, says which are reliable and which are not, and sketches a
         design for the ones worth building. Nothing here is implemented. Once acted on, this file
         moves to `dev/archive_*` like `dev/formations_stat_migration.md`.
KEY CONSTRAINTS:
  - Every claim below is read from the jamovi actually installed here (28.2.0.0, bundled R 4.6.0),
    from its shipped Python and JavaScript, or from a run I performed. Where a fact comes from
    documentation alone it is marked as such. Where it is unverified it says so.
  - ⚠ jamovi's private protocol and file format are NOT public API. Sections 2.3 and 4 rest on
    them; treat any design that does as version-fragile and re-verify after a jamovi update.
  - This does not restate `dev/jamovi_module.md`, which owns how the MODULE is built and how the
    app renders results. This file owns the OUTSIDE of jamovi: its CLI, its HTTP and websocket
    surfaces, its file format, and what R can do with them.
See: `dev/jamovi_module.md` (the module itself) - `dev/dependencies.md` (the dependency policy and
  the CRAN Imports ceiling) - `CLAUDE.md` (the installed versions and the environment traps).
-->

# R and jamovi in one workflow: what is possible

Four workflows were asked about. The verdicts, then the evidence.

| # | Workflow                                          | Verdict                                                  |
|---|---------------------------------------------------|----------------------------------------------------------|
| 1 | `data.frame` in R -> the jamovi UI                 | **Yes, reliably** - one hole: weights                    |
| 2 | `tab_jamovi()` opens jamovi with the options set   | **Partly** - the panel fills, the table needs one click  |
| 3 | jamovi inside a Positron pane                      | **Not blocked, not worth building**                      |
| 4 | jamovi emits the `tab()` / `tab_reg()` code        | **Yes - the cheapest and the most valuable of the four** |

Read #4 first if you read only one. It costs no new dependency, no new protocol and no new file format, and it is the only one of the four whose whole mechanism is already inside this package.

---

## 1. What was measured, and against what

Measured on this desktop WSL2 box, 2026-08-30. ⚠ These differ from what `CLAUDE.md` said before this study; that section is corrected in the same pass.

| Thing                | Value                     | How it was read                                    |
|----------------------|---------------------------|----------------------------------------------------|
| jamovi               | **28.2.0.0**              | `jamovi --version`; `/app/lib/jamovi/version`      |
| jamovi's bundled R   | **4.6.0**                 | `jamovi --r-version` -> `4.6.0-x64`                |
| `jmvcore` (bundled)  | 2.7.38                    | `/app/lib/R/library/jmvcore/DESCRIPTION`           |
| `jmvcore` (system R) | 2.7.35                    | `packageVersion()`                                 |
| `jmvReadWrite`       | 0.4.12 bundled; **absent from system R**; 0.4.14 on CRAN | `DESCRIPTION`; CRAN     |
| `RProtoBuf`          | 0.4.27 bundled; **absent from system R** | `DESCRIPTION`                       |
| `jmvtools`           | 28.2                      | `packageVersion()`                                 |
| tabxplor module      | 2.0.0, `rVersion: 4.6.0-x64`, built 2026-08-29 | `~/.jamovi/modules/tabxplor/jamovi.yaml` |

⚠ `flatpak info` reports `Version: 2.7.27`. That is an upstream appstream bug, not a mask - see `CLAUDE.md § Jamovi module development`.

Three evidence bases, in increasing authority, matching `dev/jamovi_module.md`'s own convention:

- the published docs and CRAN pages;
- **the shipped source of the installed app** - jamovi's Python server at `$A/lib/python3.13/site-packages/jamovi/server/` and its Electron `main.js`, where `$A` is `~/.local/share/flatpak/app/org.jamovi.jamovi/current/active/files`;
- **runs I performed** - a headless jamovi server driven over its own protocol (§4.3).

---

## 2. The four bridges that exist

Everything in this study is built out of exactly four ways into jamovi from outside. This section is the one a future phase will re-read.

### 2.1 The desktop CLI - documented enough, and stable

`electron/app/main.js`, `marshallArgs()`: the **first positional argument is opened** (a path is resolved against the working directory; an `http(s)://` URL is passed through). Then `--title=<name>` and `--temp`. Every other `--switch` is **stripped** unless it is one of `--version`, `--r-version`, `--install <file.jmo>`, `--debug2`, `--devel`. Corroborated by the shipped desktop entry, `Exec=jamovi "%f"`.

```bash
jamovi ~/some/file.omv          # opens it
jamovi --version                # 28.2.0.0     (exits before Electron starts)
jamovi --r-version              # 4.6.0-x64
```

Two consequences worth keeping:

- The `~/.local/bin/jamovi` wrapper passes `--force-device-scale-factor=…`, which `marshallArgs` strips from its own parsing while Chromium still consumes it - so **the wrapper is safe to pass a file path through**.
- `app.requestSingleInstanceLock()` plus an `app.on('second-instance')` handler means a second `jamovi file.omv` against a running app **opens a new window inside it**. ✓ So "close the old jamovi window and open a new one" is unnecessary - just call it again.

From R that is `system2("jamovi", shQuote(path), wait = FALSE)`. ⚠ `Sys.unsetenv("ELECTRON_RUN_AS_NODE")` first, or nothing happens and nothing is reported - see `CLAUDE.md`.

### 2.2 The HTTP surface - undocumented, complete, and easy

The server is **aiohttp** (not tornado, whatever the older write-ups say - `server.py` imports `from aiohttp import web`). It opens **three consecutive ports** for origin isolation: A = the main app, B = the analysis-UI iframes, C = the results iframes. The routes on A (`server.py`, the router block):

```text
GET  /                                  -> creates an instance, 302 to /<instance_id>/
GET  /open?url=<path>&title=&temp=      -> creates an instance AND opens the file
POST /<instance_id>/open                -> multipart `file`, or options={"path": …}
POST /<instance_id>/save                -> options={"path": …, "overwrite": true}
GET  /<instance_id>/coms                -> WEBSOCKET (protobuf) -- see 2.3
GET  /api/datasets                      -> JSON list of live instances
GET  /version   POST /settings   POST /end
```

`GET /open` streams newline-delimited JSON progress and ends with `{"status":"OK","url":"<instance_id>/"}`.

**Auth** is an `access_key`, checked as a **cookie** (`server.py:118` `_auth_error`, `clientconnection.py:173-175`); `GET /` also accepts it as a query parameter and then sets the cookie. If unset it is a random `uuid4().hex`.

**Finding a running instance's port** has two routes, and neither is a contract:

- the server prints `ports: A, B, C, access_key: K` on stdout, which is exactly how the Electron client discovers them (`main.js:356` greps that line with a regex);
- the server writes an **empty `~/.jamovi/<portA>.port` marker** (`server.py:844-851`) and deletes the others. ⚠ **Nothing in the shipped app reads it** - it is a discovery hook for external tooling, i.e. for precisely this - but it is not cleaned up on an unclean exit (there was a stale one on this box from a killed session), so it is a hint, not a fact.

**Fixing the key in advance is possible**, which is what makes an R client practical: `utils/conf.py:73-76` turns **every** `JAMOVI_<X>` environment variable into config key `<x>`, so launching with `JAMOVI_ACCESS_KEY=…` makes the key known to whoever launched it. The same mechanism exposes `JAMOVI_SESSION_ID`, `JAMOVI_MODE`, `JAMOVI_SPOOL_PATH` and about thirty more.

### 2.3 The coms websocket - the private protocol

`ws://127.0.0.1:<portA>/<instanceId>/coms`, carrying protobuf `ComsMessage` envelopes whose `payloadType` names the inner message class (`clientconnection.py:55-57`). The schema ships **in R**: `jmvcore` installs `jamovi.proto` (317 lines, proto3, package `jamovi.coms`), so `RProtoBuf::readProtoFiles(system.file("jamovi.proto", package = "jmvcore"))` registers `jamovi.coms.AnalysisRequest`, `AnalysisResponse`, `AnalysisOptions`, `ResultsElement` and the rest. ✓ Verified.

The handshake is one message: `ComsMessage(payloadType = "InstanceRequest", instanceId = <id>)`; every later message is routed to that instance. An analysis is created or updated with an `AnalysisRequest`:

```text
AnalysisRequest{ instanceId, analysisId, name, ns, perform, options, changed[], revision }
  perform: INIT = 0, RUN = 1, RENDER = 4, SAVE = 5, DELETE = 6, DUPLICATE = 7
```

⚠ **`changed[]` is load-bearing.** An `AnalysisRequest` that changes nothing is acknowledged and does nothing - measured (§4.3).

**Option values are encoded by hand.** `jamovi/server/options.py::write_value_to_pb` is the contract, and its one trap is that an **array is a Python `list`**, so in R an array-valued option must be a `list()`, never a length-1 vector - a `c("marital")` silently encodes as a scalar string and the option arrives empty. ⚠ And `jmvcore::Options$asProtoBuf()` is `function() private$.pb` - **a cache of what the client sent, not an encoder** - so jmvcore gives you no help here.

### 2.4 The `.omv` file format

A zip: `meta`, `metadata.json`, `xdata.json`, `data.bin` (column-major, int 4B / double 8B), optional `strings.bin`, `index.html`, and **one entry per analysis**. jamovi writes each at `'{:02} {}/analysis'.format(analysis.id, analysis.name)` (`formatio/omv.py:221`) and reads back anything matching `^[0-9][0-9]+ .+/analysis$` (`omv.py:547`), each entry a serialized `jamovi.coms.AnalysisResponse`.

`jmvReadWrite` is the R side. `write_omv(dtaFrm, fleOut, wrtPtB = FALSE, …)` writes the data; with `wrtPtB = TRUE` it also writes `attr(dtaFrm, "protobuf")[[1]]`, **using the list element's name as the archive path**. `read_omv(…, getSyn = TRUE)` reads analyses back into `attr(df, "protobuf")` and their syntax into `attr(df, "syntax")`.

⚠ Two limits of `write_omv()`, both from its own source:

- it writes **one** protobuf, not a list of them, and its own roxygen says `wrtPtB = TRUE` "currently overwrites analyses that already exist in a data file. It is meant to be used for `describe_omv` only";
- **it does not write weights at all** - a literal `warning("Handling of weights not yet implemented.")`, with the author's own note that it "likely requires creating protobuffers". That diagnosis is right: jamovi stores the weight as a **pseudo-analysis** (`analyses/analyses.py:77-79`, `if name == 'weights' and ns == 'jmv': Ctor = Weights`).

---

## 3. Workflow 1 - a data.frame to the jamovi UI

**Verdict: yes, reliably.** This is the one that needs no private protocol.

`jmvReadWrite::write_omv()` preserves what matters: factor levels and value labels, `measureType` / `dataType` (inferrable, or forced with `attr(col, "measureType")`), variable descriptions, missing-value rules, computed-column formulas and transforms. Dates become integers, jamovi having no date type. Then `system2("jamovi", shQuote(path), wait = FALSE)`.

Sketch, and it really is this small:

```r
tab_to_jamovi <- function(data, title = NULL, open = TRUE) {
  tx_need_pkg("jmvReadWrite", fn = "tab_to_jamovi()")
  path <- file.path(tempdir(), paste0(title %||% "tabxplor", ".omv"))
  jmvReadWrite::write_omv(as.data.frame(data), path, frcWrt = TRUE)
  if (open) {
    withr::with_envvar(c(ELECTRON_RUN_AS_NODE = NA),
                       system2(jamovi_exe(), shQuote(path), wait = FALSE))
  }
  invisible(path)
}
```

Design notes:

- `jmvReadWrite` goes in **Suggests**, guarded by `tx_need_pkg()` like every other optional backend. It is a cheap dependency - `Imports: jsonlite, methods, zip`, all of which tabxplor or its tree already has.
- `jamovi_exe()` has to be found, and there is no single answer: `~/.local/bin/jamovi` here, `flatpak run org.jamovi.jamovi` on other Linux boxes, `C:/Program Files/jamovi …/bin/jamovi.exe` on Windows, `/Applications/jamovi.app` on macOS. `jmvtools:::jamoviPath()`-style probing plus an option (`tabxplor.jamovi_path`) is the honest design; **do not** guess silently.
- ⚠ **The `ELECTRON_RUN_AS_NODE` trap applies here and will bite users of Positron and of Claude Code**, which is exactly the audience this feature is for. The variable must be unset in the child. On this box `~/.local/bin/jamovi` already does it; a package cannot rely on that wrapper existing elsewhere.
- The **no-relaunch variant** is available if wanted (§2.2): if a jamovi is already running and its port and key are known, `POST /<instance>/open` puts the file into it. It buys little over a second `jamovi file.omv`, which already opens a new window in the running app, and it costs the whole port-discovery problem. Not recommended.

### ⚠ The hole: weights

`write_omv()` does not carry a weight variable, and a weight is not a detail for this package's users - it is most of the point of the inference layer. Three options, in order of honesty:

1. **Say so.** The exporter warns when the data has a weight column the user named, and tells them to set it in jamovi (Data ▸ Setup ▸ Weights). Cheap, truthful, mildly annoying.
2. **Write the weights pseudo-analysis** ourselves, as a second protobuf. Blocked twice over: `write_omv()` writes only one protobuf, and the `Weights` analysis's option shape is undocumented. Would need §4's machinery.
3. **Upstream it.** `jmvReadWrite` is actively maintained and its author has already scoped the work in a source comment. A patch or an issue is plausibly the cheapest real fix, and it would help every jamovi user, not just ours.

Recommendation: ship (1), open an issue for (3).

---

## 4. Workflow 2 - `tab_jamovi()` with the options already set

**Verdict: achievable, but not the way it first looks.** There are two routes and they behave completely differently. I built both and ran them.

### 4.1 Route A - embed the analysis in the `.omv`

Mechanically this all works. Building the analysis in R:

```r
readProtoFiles(system.file("jamovi.proto", package = "jmvcore"))
os <- <hand-encoded jamovi.coms.AnalysisOptions>          # see 2.3; an array is a list()
m  <- new(jamovi.coms.AnalysisResponse)
m$name <- "jmvtab"; m$ns <- "tabxplor"; m$analysisId <- 2L; m$options <- os
attr(d, "protobuf") <- list("02 jmvtab/analysis" = m)
jmvReadWrite::write_omv(d, "probe.omv", wrtPtB = TRUE, frcWrt = TRUE)
```

✓ The archive entry lands at `02 jmvtab/analysis`, matching jamovi's own loader regex.

**But the analysis does not run.** `analyses.create_from_serial()` (`analyses/analyses.py:125`) constructs it with `status = Analysis.Status.COMPLETE` and calls `set_results(analysis_pb, silent = True)`. Nothing schedules a run: the only path that calls `.rerun()` is the `moduleUpdated` event (`analyses.py:47`), and `stale` is a client-side display flag with no server effect.

**Measured, not inferred.** I opened the probe file in a real (headless) jamovi, then had jamovi save the instance back out and decoded it:

```text
== 02 jmvtab/analysis: tabxplor::jmvtab id=2 status=ANALYSIS_COMPLETE
   options: row_vars, col_vars, tab_vars, wt, pct, color, color_signif, test, anova, na, lvs,
            cleannames, ref_levels, levels_order, ... (all 43 declared options)
   results: - '' [None]
```

So, precisely:

- ✓ jamovi **recognises** `tabxplor::jmvtab` and loads it as a real analysis (it even created its `jmv::empty` annotation siblings at ids 3 and 5, exactly as it does for an analysis the user added).
- ✓ **The options panel is populated**: my six hand-encoded values were merged into the module's full declared set of 43.
- ✗ **`results` is empty.** The user sees a finished, blank analysis. Nothing renders.

So Route A alone delivers a pre-filled panel and no table. That is a worse first impression than no feature at all.

Could it ship pre-rendered results instead? Not easily: `jmvcore::Analysis$asProtoBuf()`, which builds exactly the message we would need, **fails outside a live engine**. Running it here aborted with `Provided character value '' cannot be cast to 32-bit integer`, from jmvcore's own reference-serialising block (`refPB$year <- as.integer(year)` on an empty year). And even if patched, we would be shipping a rendered table that the user's own jamovi did not compute - stale the moment they touch anything.

### 4.2 Route B - drive a running instance over the coms websocket

**This works completely, and it is the asked-for UX.** Measured end to end:

```text
OPEN  /open?url=probe.omv                      -> 200, instance 145538c1-…
WS    InstanceRequest                          -> InstanceResponse
WS    AnalysisRequest(perform=INIT, analysisId=2, changed=["pct"], options={pct:"col"})
      -> AnalysisResponse status=ANALYSIS_INITED   results: syntax, html_table(0 chars), cache_state
      -> AnalysisResponse status=ANALYSIS_RUNNING
      -> AnalysisResponse status=ANALYSIS_COMPLETE results: html_table = 12 810 chars
```

The 12 810 characters are a real tabxplor table - 111 `tabxplor-tab` class hits, the `tx-*` slot classes, the `tx-scrollbox`, the footer legend, `data-quarto-disable-processing`. And the response carries the `syntax` element with `tabxplor::jmvtab(data = data, row_vars = marital, col_vars = race, pct = "col", color = "difference", …)`, which is §6's mechanism observed live.

Note what this proves incidentally: **a nudge is all Route A needs.** The analysis loaded from the file was inert until an `AnalysisRequest` with a non-empty `changed[]` arrived; then it ran normally.

⚠ What Route B costs:

- **`RProtoBuf` as a dependency.** It needs a system protobuf toolchain (`SystemRequirements: ProtoBuf libraries and compiler version 3.3.0 or later`). On CRAN that is a real burden for users, and it would be Suggests-only, so the feature is unavailable to most people who install tabxplor.
- **A websocket client** (`websocket`, already a common package) plus an async read loop in R.
- **Port and key discovery**, which only works cleanly if *we* launched jamovi with `JAMOVI_ACCESS_KEY` set - i.e. the workflow only supports "tabxplor started this jamovi", not "attach to the one you already had open".
- ⚠ **It is a private protocol.** `AnalysisRequest`'s field numbers, the `Perform` enum, the options encoding and the `changed[]` semantics are all internal. They have been stable across the 2.6 → 2.7 → 28.x series, but nothing promises that, and a break is silent.

### 4.3 Recommendation for workflow 2

**Do not build the full `tab_jamovi()` yet.** The honest ranking:

1. **Ship the modest version**: `tab_jamovi(...)` writes the data, embeds the analysis via Route A, launches jamovi, and **says in one message that the table appears when you touch any option**. The panel comes up correctly filled, which is 80 % of the value, for `jmvReadWrite` + `RProtoBuf` in Suggests and no protocol client.
2. **Prototype Route B in `dev/`** - the code above is most of it - and keep it out of the package until either jamovi documents the coms protocol or a `jmvcore` helper appears.
3. **The clean upstream fix** is a jamovi feature request: a way to mark an analysis in an `.omv` as needing a run (a status other than COMPLETE honoured by `create_from_serial`, or an `.omt`-style template that runs on open). One small change upstream would turn Route A from 80 % into 100 % and delete the entire case for Route B. Worth asking on the forum before building anything.

⚠ Also note `tab_reg_jamovi()` is strictly harder than `tab_jamovi()`: `jmvtabreg`'s `models`, `crosses` and `ref_levels` are nested Array-of-Group options, so the hand encoder needs the nested `c` case, and the module's own staged model-comparison means the first run may need `run_compare` triggered too.

---

## 5. Workflow 3 - jamovi inside Positron

**Verdict: technically unblocked, and still the one to skip.**

The technical objection everyone expects does not apply. `server.py:828` sets exactly one security header, a CSP whose `frame-src` lists jamovi's own three origins plus `https://www.jamovi.org`. There is **no `frame-ancestors` directive and no `X-Frame-Options` header anywhere in the server**, so a local jamovi is not blocked from being framed by a VS Code / Positron webview. jamovi even ships an `#embed` client route (`channelId` + `authToken`, `Authorization: Bearer …`, negotiated over a `MessageChannel`) built for hosting itself inside another page - though I did not reverse the handshake.

And it runs headless without Electron at all, which I did:

```bash
flatpak run --env=JAMOVI_HOME=/app --env=JAMOVI_ACCESS_KEY=... \
  --command=/usr/bin/python3 org.jamovi.jamovi -u -m jamovi.server 41337
# -> ports: 44658, 45204, 45282, access_key: ...
```

⚠ `JAMOVI_HOME=/app` is required or the server cannot find `jamovi-engine` and every analysis fails; the other `JAMOVI_*_PATH` variables come from `$A/bin/env.conf`, which the Electron normally supplies. `--start-wb` opens a browser at port A; `--if=*` binds all interfaces.

Against building it:

- **It is not an R package.** A Positron pane means a VS Code extension in TypeScript with its own build, release and compatibility surface. Nothing tabxplor ships could deliver this, so it is a different project with a different maintenance burden.
- **Three origins.** The webview would have to reach ports A, B and C, and the client's `config.js` hard-codes them as absolute `http://127.0.0.1:<port>/` roots - so a single-port proxy is not enough without also rewriting `roots` (the server does support `hostname`/`host_a`/`host_b`/`host_c` config for `path` and `host` separation modes, which is the seam if anyone ever tries).
- **CSP collision.** jamovi's client needs `script-src 'unsafe-eval' 'unsafe-inline'`; webview hosts routinely forbid both.
- **Width.** A crosstab is the widest thing this app renders - `dev/jamovi_module.md § 7` is a whole section about a 620 px floor and clipping. A side pane is the worst possible place for it.
- **The payoff is small.** jamovi in its own window, launched by workflow 1, is already the thing.

Recommendation: **document the mechanism (done, here) and do not build it.** If the goal behind the question was "fewer window switches", the better answer is workflow 4 - stay in R, and use jamovi only when you want to point and click.

---

## 6. Workflow 4 - jamovi emits the `tab()` code

**Verdict: yes. Build this one.** It needs no new dependency, no protocol, no file format, and its mechanism is entirely inside `R/jmvtab.b.R`.

### 6.1 How jamovi's Syntax panel actually works

It is generated **R-side**, by jmvcore, on every response. `jmvcore::Analysis$asProtoBuf()` ends:

```r
syntax <- RProtoBuf_new(jamovi.coms.ResultsElement, name = "syntax",
                        preformatted = self$asSource())
prepend <- c(list(syntax), prepend)
```

and

```r
asSource = function() paste0(private$.package, "::", private$.name, "(", private$.asArgs(), ")")
```

with `.asArgs()` emitting `data = data` plus **only the options whose value differs from their declared default**. The module author writes nothing; the client keys on the element being named `"syntax"`.

**`asSource()` is a public R6 method**, and `jmvtabClass` inherits it through `jmvtabBase` -> `jmvcore::Analysis`. So it can be overridden. ✓ Verified both halves offline, with no jamovi running:

```r
o <- jmvtabOptions$new(row_vars = "marital", col_vars = "race", pct = "row", color = "diff")
jmvtabClass$new(options = o, data = d)$asSource()
#> tabxplor::jmvtab(
#>     data = data,
#>     row_vars = marital,
#>     ...
```

and a subclass defining `public = list(asSource = function() "…")` replaces it. There is precedent in the wild: `PeterC-alfaisal/jeva` overrides the sibling `.sourcifyOption()` in eight `.b.R` files.

⚠ A free fix comes with it. The current panel emits `exportExcel = FALSE, resetPath = FALSE` on every table - `Action` options whose declared default is `NULL` but whose value is `FALSE`, so `.sourcifyOption()` believes they changed. An override drops them.

### 6.2 Why the translation is nearly mechanical

Because the module already obeys the rule stated in `R/jmvtab.b.R`'s header: **an option is named after the `tab()` argument it drives**, exactly or as `<argument>_<slot>`, and `test-jamovi-vocabulary.R` enforces it. Measured against `TAB_ARGS` + `EXPORT_ARGS`:

| Module      | Options | Not a producer/export argument                                             |
|-------------|---------|-----------------------------------------------------------------------------|
| `jmvtab`    | 43      | `lvs`; 4x `ci_method_*`; the 3 level/ref widgets; `tab_theme`; 6 export     |
| `jmvtabreg` | 40      | `models`, `run_compare`, `crosses`; the 3 level/ref widgets; `tab_theme`; 7 export |

Every exception is understood, and each maps to something sayable in R:

| Module option                      | Becomes                                                        |
|------------------------------------|-----------------------------------------------------------------|
| `lvs`                              | `levels =` (renamed only because `jmvcore::Options` has a `levels()`) |
| `ci_method_cell` / `_diff` / `_mean_diff` / `_mean_ratio` | one `ci_method = c(...)` vector       |
| `ref_levels`                       | `ref = c(var = "level", ...)`                                   |
| `levels_order`                     | a prep step: `forcats::fct_relevel()`                           |
| `levels_collapse`                  | a prep step: `forcats::fct_collapse()`                          |
| `shape`                            | a prep step, or `shape =` (`shape_numeric_var()` is the 2.0.0 name) |
| `crosses` (`jmvtabreg`)            | `a*b` inside `predictors`                                       |
| `models` / `run_compare`           | a list of `predictors` sets                                     |
| `tab_theme`, export controls       | nothing - render/UI only, correctly absent from the code        |

⚠ The prep half is not guesswork: `dev/jamovi_module.md § 13` already states the invariant that a level reorder "is byte-identical to `tab()` on pre-relevelled microdata", and that merges apply with `forcats::fct_collapse()`. Internally these travel as the dot-prefixed `tab(.levels_order =, .levels_collapse =)`, which are **not public API** - which is exactly why the generator must print the `forcats` calls instead. And the option parsers already exist and already produce the right shape: `jmvtab_levels_order()`, `jmvtab_levels_collapse()`, `jmvtab_ref_vector()`, `jmvtab_shape_vector()` (`R/jmvtab-cache.R`), `jmvtab_reg_cross_keys()`, `jmvtab_reg_models()` (`R/jmvtabreg-cache.R`).

So the output is a pipeline, not a call:

```r
data <- jmvReadWrite::read_omv("your-file.omv")

data |>
  dplyr::mutate(
    partyid = forcats::fct_collapse(partyid, "Democrat" = c("Strong democrat", "Not str democrat")),
    partyid = forcats::fct_relevel(partyid, "Independent", "Democrat", "Republican")
  ) |>
  tabxplor::tab(row_vars = partyid, col_vars = race, pct = "row",
                color = "difference", ref = c(partyid = "Independent"), stars = TRUE)
```

### 6.3 Two honest limits, which the generated code must state itself

- **The data cannot be named.** `jmvcore::Analysis` never learns the dataset's path - there is no such field on it, and jamovi's own syntax mode writes the bare token `data` for the same reason. The generated code therefore opens with a `read_omv()` line naming the file the user must fill in.
- **jamovi's own data work cannot be translated.** Computed variables, transformed variables and row filters are applied by jamovi, in jamovi's formula language, **before** the module ever sees the data. There is no R equivalent to emit. `read_omv()` is what rescues this: it returns the post-computed, post-filtered frame, so the generated pipeline is correct *given that starting point*. ⚠ The generated header must say so plainly, or a user will assume their raw CSV reproduces the table.

### 6.4 Where to put it, and what is impossible

- **The Syntax panel**, by overriding `asSource()`. One text; replaces the `jmvtab()` call. Best default: it is where a jamovi user already looks, it is togglable from the ribbon, and it costs no results element.
- **A `Preformatted` results element**, as a visible extra block. `jmvcore::Preformatted` exists and `.r.yaml` supports it. Costs vertical space on every table; better behind an option if offered at all.
- ⚠ **A "copy" button is impossible.** The results iframe has no clipboard access, inline `<script>` in an `Html` element does not execute, and `<a href>` is hijacked to the OS browser (`dev/jamovi_module.md § 6.2`, § 6.5). jamovi's own per-element context-menu **Copy** is the route, and it works on a `Preformatted`.

### 6.5 How it would be tested - entirely offline

This is the part that makes workflow 4 attractive: **the whole feature is testable in `tests/testthat/` with no jamovi at all**, because `jmvtabOptions$new()` and `jmvtabClass$new()` run in plain R.

1. Build an options object, call `asSource()`, and assert the text - a snapshot, in `test-jamovi-vocabulary.R`'s neighbourhood since that file already owns the option↔argument rule.
2. `parse()` the text, to assert it is syntactically valid R.
3. The real assertion: **`eval()` it and compare the resulting table to `jmvtab_build()`'s own** on the same data and options. That is a round trip, and it is what turns "the code looks right" into "the code reproduces the table". A handful of cases - a plain crosstab, one with merged and reordered levels, one with a `ref`, one numeric with a `shape`, one regression with an interaction - would cover the whole translation table above.

⚠ One caution for the generator: jamovi column names can contain spaces and punctuation. `tab()` accepts **character** variable names through tidyselect (verified), so emit `row_vars = "odd name (x)"` rather than a bare or backticked symbol - and never round-trip a name through `as.character()` on a symbol, per `CLAUDE.md`'s existing `vars_chr()` rule.

---

## 7. What each workflow costs in dependencies

The CRAN ceiling is 20 Imports and `dev/dependencies.md` owns the policy. Nothing below would be an Import; everything is Suggests behind `tx_need_pkg()`.

| Workflow  | New Suggests               | Real cost                                                     |
|-----------|----------------------------|----------------------------------------------------------------|
| 1         | `jmvReadWrite`             | Low. Imports jsonlite/methods/zip, all already in the tree.    |
| 2 Route A | + `RProtoBuf`              | **High.** Needs a system protobuf toolchain.                   |
| 2 Route B | + `RProtoBuf`, `websocket` | **High**, plus a private protocol to track across releases.    |
| 3         | -                          | Not an R package at all.                                       |
| 4         | **none**                   | Zero. `jmvcore` is already how the module exists.              |

⚠ `RProtoBuf` is the pivot. It is what separates "write a data file" (easy, portable) from "write or drive an analysis" (needs a compiler toolchain most users of a point-and-click tool will not have). Any design that puts a jamovi *analysis* in R's hands pays it.

---

## 8. Recommendation, ranked

1. **Workflow 4 - emit the `tab()` code.** Build it. It is free in dependencies, its mechanism is proven offline, its tests need no jamovi, and it serves the package's actual teaching goal: a student clicks a table together and leaves with the R code that makes it. It also closes the loop the module's whole naming rule was designed for. ⚠ It must state its two limits (§6.3) in the generated text itself.
2. **Workflow 1 - the data hand-off.** Build it, small and honest, with the weights warning. One `jmvReadWrite` Suggests.
3. **Workflow 2 - `tab_jamovi()`.** Build only the modest Route A version, *if* workflow 1 exists and *after* asking jamovi upstream whether an analysis can be marked as needing a run on open. Keep Route B as a `dev/` prototype.
4. **Workflow 3 - Positron embedding.** Do not build. Recorded here so it is not re-derived.

An observation worth stating: **workflows 1, 2 and 4 compose into one round trip** - R prepares the data and hands it over (1), jamovi is where the table is explored by hand (2), and the user leaves with the code (4). That round trip, not any single feature, is the thing worth having, and workflow 4 is both its most valuable leg and its cheapest.

---

## 9. Open questions, each with the experiment that settles it

- **Does an `.omv`-loaded analysis really look blank in the GUI?** Measured server-side (empty `results`, §4.1) but not eyeballed in the Electron window. One launch of `jamovi probe.omv` settles the cosmetics - does the user see an empty box, a spinner, or nothing at all in the results list.
- **Can jamovi be asked to run an analysis on open?** Nothing in `analyses.py` does today. A forum question would settle whether it is wanted upstream, and it is the single change that would make Route A complete.
- **Will `jmvReadWrite` write weights?** The author has scoped it in a source comment. An issue would settle it, and it is the only blocker on workflow 1 being complete.
- **How stable is the coms protocol across 2.6 → 2.7 → 28.x?** `dev/jamovi/dev_console_live_capture/` holds a 2.6.44 capture and this study measured 28.2; the `AnalysisRequest` fields matched. A diff of the two `.proto`s would turn that impression into a fact.
- **`jmvconnect`** (CRAN 2.5.7) reads datasets out of a running jamovi from R - the reverse of workflow 1. ⚠ Last published 2024-07-06, two years stale, and untested against 28.x. If it still works it is a free second half of the round trip; if not, its mechanism (§2.2's `/api/datasets`) is reimplementable in a few lines.
- **`Rj`**, the in-jamovi R editor, can read the dataset as `data` and render output, so a user could paste generated `tab()` code back into jamovi. ⚠ But columns it creates cannot be written back to the spreadsheet or saved (jamovi staff, forum), so it is a viewer, not a bridge. Unverified against 28.x.

---

## 10. Reproducing any of this

The probe used no repo files and left nothing behind. To redo it:

- **Offline, no jamovi** (workflow 4's whole mechanism): `devtools::load_all()`, then `jmvtabOptions$new(...)` and `jmvtabClass$new(options=, data=)$asSource()`.
- **A headless jamovi**: the `flatpak run --command=/usr/bin/python3 … -m jamovi.server` line in §5, then `GET /open`, then the coms websocket. ⚠ `JAMOVI_HOME=/app` and a fixed `JAMOVI_ACCESS_KEY`, or it will not work.
- **Reading jamovi's own source**: everything cited lives under `$A/lib/python3.13/site-packages/jamovi/server/` and `$A/bin/resources/default_app.asar`, with `$A` as in §1. It is plain Python and unminified JS - far better evidence than the docs, and the reason §§2-6 can be this specific.
- ⚠ Kill a headless server by explicit PID (`ps -eo pid,cmd | grep jamovi.server`), never `pkill -f`, and remove any stale `~/.jamovi/<port>.port` it leaves.
