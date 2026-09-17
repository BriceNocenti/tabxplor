# jamovi Module Audit Report
**Module:** tabxplor — User-Friendly Tables with Color Helpers for Data Exploration
**Version:** 2.0.0
**Date:** 2026-09-16

---

## Hi, I'm Claudia

I'm an automated reviewer. I read through jamovi modules looking for security
problems, schema mismatches between the YAML definitions and the R code, and the
sort of rough edge that only shows up once real users with real data files get
hold of it. Think of me as a friendly extra pair of eyes before something ships.

I'll say up front that this is one of the most carefully built modules I've read.
The two analyses are thin orchestrators over a pure, testable build core; the
caching design is documented down to the `clearWith: []` on the hidden state
carriers and *why* jmvcore's default would break it; every `renderFun` resolves;
every option in the `.a.yaml` is either read in R or handled in the events JS;
the results definitions and the R code agree completely. The JavaScript is
genuinely careful — every user-derived string (variable names, factor levels)
goes in through `textContent`, and the only `innerHTML` writes are the module's
own literal column headings. There are 193 `tryCatch` sites and 856 `is.na`
guards; missing data is clearly something you've thought about hard. And the
export-path code sanitises the filename, auto-numbers rather than clobbering, and
HTML-escapes the status message it prints back. That is not common.

So most of what follows is polish. But two findings are genuinely serious: one is
a packaging decision that will stop features working once the module is installed
from the library rather than from your own machine, and the other silently
deletes your significance tests for a large class of perfectly ordinary datasets.
I'd fix both before release; everything below them is smaller.

## What I Found

Here's everything, ordered roughly by how much I think it matters:

---

### [HIGH] Dependencies the panel can reach are declared only in `Suggests:`

**File:** `DESCRIPTION:24-51` (the `Suggests:` block) and `DESCRIPTION:52-70` (`Imports:`)
**Pattern:** `Suggests: ... jmvcore (>= 2.4.0), ... openxlsx2 (>= 1.0.0), ... R6, ... ggplot2 (>= 3.5.0), ... svyVGAM (>= 1.2), VGAM (>= 1.1.0), marginaleffects (>= 0.20.0), gridExtra (>= 2.3.0), brant (>= 0.3.0)`

**Why this matters:** jamovi resolves a module's R dependencies from `Imports:`
and `Depends:`. `Suggests:` is not installed alongside the module. That's fine for
a package a person installs from CRAN with `dependencies = TRUE`, but a jamovi user
installs the module from the library and has no R console to run
`install.packages()` in — so the remedy your `tx_need_pkg()` message offers
(`R/utils.R:276-281`) is one they cannot act on.

Several of these are reachable straight from the panel, and one is the *default*
path:

- **`openxlsx2`** — `export_format` defaults to `excel` in both analyses
  (`jamovi/jmvtab.a.yaml`, `jamovi/jmvtabreg.a.yaml`), and
  `R/jmvtab-export.R:272` aborts with `tx_need_pkg("openxlsx2", "Excel export")`.
  The most prominent button in the panel is the first thing to fail.
- **`marginaleffects`** — `effect` offers `at_reference` as a user-selectable
  value, and `R/tab_reg.R:96-97` aborts on it.
- **`svyVGAM` / `VGAM`** — `R/tab_reg.R:99`, on the survey-weighted multinomial
  path, which the `family` control can select.
- **`ggplot2` + `gridExtra`** — `R/tab_xl.R:458`, behind the `xl_check`
  assumption-check-plots checkbox.
- **`brant`** — `R/tab_reg.R:1367`, the proportional-odds test.
- **`jmvcore` and `R6`** — these are the analysis classes themselves
  (`R/jmvtab.b.R:23`, `R/jmvtabreg.b.R:25`). Note the shape of that guard: if
  `requireNamespace('jmvcore')` is `FALSE`, the `if` returns `NULL` and
  `jmvtabClass` simply *is* `NULL` — the analysis fails to load rather than
  failing loudly with a message anyone can diagnose.

I want to be careful not to overstate this: jamovi's own bundle ships `jmvcore`,
`R6` and `ggplot2`, so those three will very likely resolve on a real install
today. The point is that nothing *pins* them — R's resolution only honours what's
in `Imports:`, so they stay available by coincidence rather than by contract.
`openxlsx2`, `marginaleffects`, `VGAM`, `svyVGAM`, `brant`, `gridExtra` and
`mirai` are not part of that bundle, and I'd expect those features to be dead on
a library install.

**What I'd suggest:** Move everything the jamovi panel can reach into `Imports:`
— at minimum `jmvcore`, `R6`, `ggplot2`, `openxlsx2`, `marginaleffects`,
`gridExtra`, and (if you want the weighted multinomial family offered in the UI)
`VGAM` and `svyVGAM`. If you'd rather keep the CRAN footprint small, the other
option is to *remove the UI controls* for the features whose packages stay in
`Suggests:` — but shipping a default `export_format: excel` that cannot work is
the one combination I'd rule out. Either way, keep the `tx_need_pkg()` guards;
they're good defence. I'd just reword the abort text so it doesn't tell a jamovi
user to run `install.packages()`.

---

### [HIGH] Variable names that aren't syntactic R names silently wipe out the survey-weighted tests

**File:** `R/survey-design.R:137`, `R/survey-design.R:195`, `R/survey-design.R:196`, `R/survey-design.R:203`
**Pattern:**
```r
stats::reformulate(as.character(x))                       # :137  the weights formula
survey::svyglm(stats::reformulate(rv, response = cv), ...) # :195
survey::regTermTest(fit, stats::reformulate(rv), ...)      # :196
survey::svychisq(stats::reformulate(c(rv, cv)), ...)       # :203
```

**Why this matters:** `rv`, `cv` and the weight name arrive here as raw column
names straight off `self$data` — whatever the user's file called them. But
`reformulate()`'s contract is that `termlabels` must be *syntactically valid
names or parseable expressions*; it does not quote them for you. I checked the
behaviour on R 4.3.2:

```
reformulate("Age group")    -> Error in str2lang(termtext): unexpected symbol
reformulate("Age (years)")  -> ~Age(years)        # no error, wrong formula
```

Both outcomes are bad, and the second is worse than the first. A name with a
space throws, gets swallowed by the `tryCatch` at `R/survey-design.R:186-189` /
`:193-200` / `:202-...`, and `svy_omnibus_one()` returns `na_row()`. A name with
parentheses parses as a *function call* and then fails downstream into the same
`na_row()`. Either way the user sees blank test columns with no error anywhere —
the Chi²/Cramér's V and ANOVA results they ticked the box for just aren't there.

This is reachable on the ordinary path: `R/tab.R:1472-1478` calls
`svy_omnibus_grid()` whenever the data is weighted and `chi2` is on — i.e. the
`test` checkbox with a weight variable, or with jamovi's own row weights. The
regression side reaches the same code through `svy_make_design()` at
`R/tab_reg.R:1416`. And jamovi's users overwhelmingly import from SPSS, Excel and
CSV, where `Age group`, `Household weight` and `Income (€)` are entirely normal
column names.

You clearly know about this — `R/tab_reg.R:1149` backticks the outcome when it
builds its formula string. The survey path just didn't get the same treatment.

**What I'd suggest:** Backtick the names before they reach `reformulate()`.
`reformulate()` normalises redundant backticks away, so you can do it
unconditionally and it costs nothing on ordinary names — I verified all three
forms on R 4.3.2:

```r
bt <- function(x) paste0("`", x, "`")
stats::reformulate(bt(as.character(x)))                        # :137
survey::svyglm(stats::reformulate(bt(rv), response = bt(cv)), design = des)
survey::regTermTest(fit, stats::reformulate(bt(rv)), method = "Wald")
survey::svychisq(stats::reformulate(bt(c(rv, cv))), design = des, statistic = "F")
```

(`reformulate(bt(x))` gives ``~`Age group` ``, and `reformulate("`x`")` gives
plain `~x`.) While you're in there, a test fixture with a column named
`"Age group"` would keep this from coming back — your `Config/testthat` setup
already runs `survey-design` and `survey-variance` first, so it's a natural place
for it.

---

### [MEDIUM] The panel is labelled in R argument names rather than in words

**File:** `jamovi/jmvtab.u.yaml:15`, `:64`, `:428`, `:441`, `:454`, `:467`, `:506` (and throughout); `jamovi/jmvtabreg.u.yaml` likewise
**Pattern:**
```yaml
label: <b>row_vars = <i>(row variables)</i></b>
label: pct = <i>(type of percentages)</i>
label: ci_method = c(mean_ratio = )
label: totaltab = <i>(with tab_vars, add a total table)</i>
```

**Why this matters:** I can see the intent, and the header comment in
`R/jmvtab.b.R:7-11` states it as a design rule: an option is named after the
`tab()` argument it drives, so the panel doubles as a way to learn the R API.
That's a defensible idea and the consistency is impressive. But it puts the cost
on the wrong person. Someone who opens jamovi rather than RStudio has, by that
choice, told you they don't want to think in function arguments — and
`ci_method = c(mean_ratio = )` is not a label they can act on. It doesn't say
what the control does, and it isn't searchable in any way that helps them.

`totaltab = <i>(with tab_vars, add a total table)</i>` is the clearest example:
the parenthetical *is* a good human label, and the `totaltab = ` in front of it
is the part that isn't.

**What I'd suggest:** Flip the two. Lead with the human label, keep the argument
name as the secondary hint — the R user still gets their mapping, the jamovi user
gets a panel they can read:

```yaml
label: "<b>Total Table</b> <i>(totaltab)</i>"
label: "Percentages <i>(pct)</i>"
label: "Mean ratio intervals <i>(ci_method)</i>"
```

Better still, drop the argument name from the label entirely and keep the mapping
in a document alongside the module — the panel isn't the place for it. This is a
`.u.yaml` change only — no option renaming, so no saved analysis breaks. jamovi's convention is sentence case on individual
controls and title case on group headings and variable boxes, which falls out
naturally once the argument prefix is gone.

While you're in `jamovi/jmvtab.u.yaml:198`: `Cramer’V` is missing its `s` —
`Cramér's V`.

---

### [MEDIUM] The total-row labels use `gettext()` where the rest of the module uses `jmvcore::.()`

**File:** `R/jmvtab.b.R:109-112`
**Pattern:**
```r
total_names  = c(row = gettext("Total",    domain = "R-tabxplor"),
                 col = gettext("Total",    domain = "R-tabxplor"),
                 tab = gettext("Ensemble", domain = "R-tabxplor"),
                 other = gettext("Others", domain = "R-tabxplor"))
```

**Why this matters:** Your own note at `R/jmvtab-export.R:330-332` states the rule
precisely — `jmvcore::.()` resolves against the module's catalogue keyed on
**jamovi's UI language**, while plain `gettext()` follows the **R engine's
locale**. `R/jmvtabreg.b.R:146-147` repeats it. These four strings are the
exception, and they're not a minor one: "Total", "Total", "Ensemble" and "Others"
are the row and column labels on every table the Crosstables analysis produces. A
French user running jamovi in French on an engine that reports a C or en_US
locale gets an English total row in an otherwise French table.

**What I'd suggest:** Switch them to `jmvcore::.()`. `.()` reads `self` out of its
caller's frame, and `.opts()` is a private method of the R6 class, so `self` is in
scope and the call is legal exactly where it stands:

```r
total_names = c(row   = jmvcore::.("Total"),
                col   = jmvcore::.("Total"),
                tab   = jmvcore::.("Ensemble"),
                other = jmvcore::.("Others"))
```

Then add the four msgids to `jamovi/i18n/catalog.pot` and `fr.po` — they're
already translated in `po/R-fr.po`, so it's a copy across rather than new
translation work.

---

### [LOW] `conf_level` in the Crosstables panel accepts 0 and 1

**File:** `jamovi/jmvtab.a.yaml:426-430`
**Pattern:**
```yaml
- name: conf_level
  type: Number
  min: 0
  max: 1
  default: 0.95
```

**Why this matters:** At `conf_level = 0` every interval collapses to zero width
and nothing is ever significant; at `conf_level = 1` every interval is infinite
and nothing is ever significant either. Neither errors — the user just gets a
table of confidently wrong conclusions. The Regressions panel already has the
right bounds (`jamovi/jmvtabreg.a.yaml:354-359`: `min: 0.5`,
`max: 0.9999999999`), so this reads as an oversight rather than a decision.

**What I'd suggest:** Copy the Regressions bounds onto the Crosstables option.

---

### [LOW] The user's `subtext` reaches the results pane as unescaped HTML

**File:** `R/tab-render-html.R:452-454`
**Pattern:**
```r
tfoot <- if (length(subtext) != 0) {
  paste0('<tfoot>...<div ...>',
         paste0(subtext, collapse = "<br>"), '</div></td></tr></tfoot>')
```

**Why this matters:** Everything else in this renderer goes through
`tx_html_escape()` — the cells, the headers, the caption, the notes at
`R/tab-render-html.R:500`. `subtext` is the exception, and I can see why: your
comment at `:75` says the colour legend is prepended to it and is markup, not
text. But `subtext` is also a user-typed `String` option in both panels, and it
lands in the Html result verbatim. Option values travel inside `.omv` files, so
opening someone else's saved analysis renders whatever markup they put in that
box. jamovi's results pane is a sandboxed Electron renderer, so the blast radius
is UI manipulation within the session rather than anything reaching the OS — which
is why this is LOW and not higher — but it's still the one hole in an otherwise
airtight escaping story.

**What I'd suggest:** Escape the *user's* half while leaving the
internally-generated legend markup alone. The cleanest seam is the jamovi
boundary, where you already know which half is which — escape
`self$options$subtext` in `.opts()` (`R/jmvtab.b.R:103`, `R/jmvtabreg.b.R:140`)
before it enters the build, and let `tab_kable()` prepend the legend afterwards.
That leaves the R-console API of `tab(subtext = )` free to pass markup
deliberately, which is presumably what you want there.

---

### [LOW] `html_table` clears on export-path options it doesn't depend on

**File:** `jamovi/jmvtab.r.yaml:12-14`, `jamovi/jmvtabreg.r.yaml:8-10`
**Pattern:**
```yaml
- name: html_table
  title: Table
  type: Html
```

**Why this matters:** With no `clearWith`, the element inherits `*` and is cleared
on *any* option change. Six of them can't affect the table at all: `export_dir`,
`export_filename`, `export_format`, `resetPath`, `xl_replace` and (in the
Regressions panel) `xl_check`. Typing a new export folder blanks the table and
then repaints it. Nothing is computed wrong — `.run()` repopulates every time —
but the user sees the flash, and on a large crosstab that flash is the whole
results pane.

You've already reasoned carefully about `clearWith` on the two hidden state
carriers, with a good comment on each; this is just the visible element not
having had the same pass.

**What I'd suggest:** Give `html_table` an explicit `clearWith` listing the
options it really depends on — everything in `.opts()` plus the render-time
`tab_theme`, `wrap_rows` and `wrap_cols`. Note that `clearWith` only governs
clearing, not whether `.run()` fires, so the Export action still triggers a run
and the status note still rides through as designed.

---

### [LOW] Nothing in the results cites the methods or the packages behind them

**File:** `jamovi/0000.yaml`, `jamovi/jmvtab.a.yaml`, `jamovi/jmvtabreg.a.yaml` (no `refs:` anywhere; no `jamovi/00refs.yaml`)

**Why this matters:** A user reporting these numbers in a paper needs to cite what
produced them, and right now the results pane gives them nothing. This module
isn't doing arithmetic in base R — it's a front end onto real statistical
machinery: `survey` does the Rao–Scott and design-effect work
(`R/survey-design.R:203`, and `R/survey-variance.R` on the variance side),
`MASS::polr` and `nnet::multinom` fit the ordinal and multinomial models,
`marginaleffects` supplies the `at_reference` estimand, and the CI methods offered
under `ci_method` are named procedures with papers behind them. None of that is
infrastructure; it's the substance.

**What I'd suggest:** Add a `jamovi/00refs.yaml` and hang `refs:` off the
analyses in the two `.a.yaml` files (and off individual result elements where a
citation belongs to one specific number). I'd start with Lumley's `survey`,
Venables & Ripley for `MASS`/`nnet`, Arel-Bundock for `marginaleffects`, and the
source for each `ci_method` choice. Each entry wants `title`, `author`, `year`
and `url`.

---

### [LOW] `import(data.table)` and `import(vctrs)` load whole packages at startup

**File:** `NAMESPACE:252-253`
**Pattern:**
```r
import(data.table)
import(vctrs)
```

**Why this matters:** A bare `import()` pulls the entire package into the
namespace when the module loads, which works against jamovi's lazy-loading model —
every user pays that cost on every session whether or not they open a tabxplor
analysis. I'll grant that data.table's own documentation recommends
`import(data.table)` because of `:=`, `.N` and `.SD`, and that vctrs carries a
lot of S3 generics you're implementing methods for; both are defensible. It's
still the one place in an otherwise disciplined NAMESPACE (the other 26 lines are
all `importFrom`) where the cost isn't narrowed.

**What I'd suggest:** If you can enumerate what you actually need, `importFrom`
both down to it. If data.table's NSE makes that impractical, leaving it is a
reasonable call — but `import(vctrs)` looks narrowable to the generics you define
methods for, and that one I'd do.

---

### [INFO] One open TODO

**File:** `R/fmt_class.R:4595`
**Pattern:** `# TODO(2.1.0): grade them instead, by building the plan once per family and picking per cell,`

**Why this matters:** Nothing today — it's scoped to a future version and it's the
only one in 2.6 MB of R source, which is remarkable discipline.

**What I'd suggest:** Leave it. I'm noting it only so it's on the record.

---

### [INFO] Six untranslated strings in the French catalog

**File:** `jamovi/i18n/fr.po`
**Pattern:** `msgstr ""` on 14 entries, six of which are real UI strings:
`"Add two or more models to compare them; leave empty to fit the full model."`,
`"Each model draws from the predictors above; untick to leave one out."`,
`"Model comparison staged. Click <b>Run comparison</b> to compute the table."`,
`"Select predictors first: they form the pool each model draws from."`, and the
two `odds ratios – the … each odds ratio is compared to (ref2 =)` hints.

**Why this matters:** A French user hits English text at exactly the point where
the model-comparison workflow needs explaining. The catalog is otherwise in
excellent shape — 285 msgids, 271 translated.

**What I'd suggest:** Fill in the six. They all come from the newer
model-comparison UI in `jamovi/js/jmvtabreg.js`, so they're likely just strings
added after the last catalog pass.

---

## What's Already Working Well

Beyond the findings above, here's what's in great shape:

- **Schema consistency is perfect.** Every option in both `.a.yaml` files is
  either read in R or handled in the events JS (`resetPath` is an `OptionAction`
  driven entirely from `jamovi/js/jmvtab.js:1084-1091` — correct, not dead). Every
  `self$options$` read in R resolves to a declared option. Every `.u.yaml` control
  maps to an option or is a container; the suffixed radio names all carry a valid
  `optionName`. All three result elements are declared and all three are used. I
  found nothing dangling in either direction.
- **`renderFun` resolves.** Both `.r.yaml` files declare `renderFun: .plot` and
  both classes define `.plot = function(image, ...)`. The hidden zero-height
  `Image` used as a state carrier is an unusual pattern, but it's documented
  thoroughly and it's correct — and because `.plot` draws nothing and touches
  neither `image$state` nor `self$data`, there's no export-path failure to worry
  about and no reason for `requiresData`.
- **The `clearWith: []` reasoning on the state carriers is right**, and the
  comments explaining why jmvcore's `"*"` default would drop the state on exactly
  the change the cache exists to survive are the best `clearWith` documentation
  I've read in a module.
- **State retrieval is properly guarded.** `self$results$cache_state$state` is
  `NULL`-safe at every read, `jmv_export_recall()` handles a `NULL` store, and
  `identical(cst$sig, cur_sig)` degrades correctly when `cst` is `NULL`. Nothing
  crashes on a cleared cache.
- **No code execution risk.** No `eval(parse(text = ...))`, no `source()`, no
  `.Call`/`.C`/`.Fortran`. The `eval()` calls I found are all data.table NSE
  (`eval(sym)`), and the four `system2()` calls (`R/jmvtab-export.R:187`, `:219`,
  `:232`, `R/tab-parallel.R:61`) invoke fixed executables located with
  `Sys.which()`, with `shQuote()` on every argument. Formula strings go through
  `stats::as.formula()` (`R/tab_reg.R:1149`, `:1466`), which jamovi validates
  against its own allowlist. The `str2lang`/`rlang::parse_expr` sites in
  `R/tab.R:799-802` and `R/tab-deprecate.R:121-130` are the R-console `filter =`
  API, which the jamovi panel never populates.
- **The JavaScript is clean.** No `eval`, no `new Function`, no prototype
  manipulation, no embedded secrets. Every `innerHTML` write is either `""` or a
  module-literal heading (and the comment at `jamovi/js/jmvtab.js:564` saying so is
  accurate — I checked). All 40-odd user-derived values, variable names and factor
  levels included, go in through `textContent`.
- **No named HTML entities anywhere.** I swept the R, YAML and JS for
  `&plusmn;`, `&times;`, `&middot;` and friends and found only the three escaping
  helpers. You use real Unicode in the YAML/JS and `\uXXXX` escapes in R
  (`R/reg-assumptions.R:654` writes the superscript-two as a backslash-u escape,
  with the U+00B2 code point named in the comment beside it). That's exactly right, and it'll survive jamovi's upcoming renderer fix
  when modules using entities won't.
- **No `library()` or `require()` in package code.** The four hits are all inside
  roxygen `@examples`.
- **Translatable strings are whole sentences.** I ran the fragment greps — no
  leading or trailing whitespace inside `.()`, no punctuation-only separators, no
  sentences spliced from `paste0()` pieces. `R/jmvtabreg.b.R:146` states the rule
  and the code follows it.
- **The export path is carefully built.** `export_sanitize_filename()`, extension
  taken from the format rather than typed, `export_number_path()` auto-numbering
  instead of overwriting unless the user opts in, `file.access(mode = 2L)` checked
  non-destructively, and `export_status_html()` escaping `&`/`<`/`>` before
  printing the path back. The WSL and Windows-registry Documents-folder detection
  is more thorough than it needed to be.
- **Error and missing-data handling is thorough** — 193 `tryCatch` sites, 220
  `cli_abort()` validation points, 856 `is.na` guards. And no `setVisible(FALSE)`
  or `setError()` on failure paths anywhere, so jamovi gets to render errors its
  own way with the results left in place.
- **`compilerMode: tame`** on both `.u.yaml` files, which is what you want with
  layout this hand-crafted — `jmvtools::install()` won't regenerate over it.
- **Version metadata is correct**: `2.0.0` in both `DESCRIPTION` and
  `jamovi/0000.yaml`, semver, past 1.0. `License: GPL (>= 3)` is OSI-approved and
  accepted. No `Remotes:` to worry about. No build artifacts checked in. A
  populated `tests/testthat/` with parallel testthat 3e configured, and an
  `jamovi/i18n/` catalog at 95% French coverage.

---

Two things to fix before this ships: get the panel-reachable packages into
`Imports:`, and backtick those `reformulate()` calls. The first will bite every
user who clicks Export; the second bites quietly, which makes it the more
dangerous of the two. Everything after those is a half-hour each and none of it
is urgent. This is a serious, well-engineered module and it shows — the comments
alone taught me two things about jmvcore's state handling that I hadn't seen
written down anywhere.

— Claudia
