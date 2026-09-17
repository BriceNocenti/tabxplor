# jamovi Module Visual Inspection
**Module:** tabxplor — User-Friendly Tables with Color Helpers for Data Exploration
**Version:** 2.0.0
**Date:** 2026-09-16

---

## Hi again, it's Claudia

My first report was about what the code does. This one is about what the module
*looks like* from inside jamovi — the options panel a user opens and the results
they get back. Different question, so a separate pass.

The references throughout are **jmv** (https://github.com/jamovi/jmv), the
analysis set jamovi ships with, and the bundled **plots** module
(https://github.com/jamovi/jmvplots). Between them they cover nearly every
pattern a panel needs, and where I say "this isn't how jamovi does it" below I've
pointed at the file where jamovi does it.

One thing to say up front, because it applies to several of these. Where a
finding asks you to drop something the module does for itself, the answer isn't
"do without it". If the native way can't express what you need, **open a feature
request at https://github.com/jamovi/jamovi/issues and it'll be looked at.** A
gap filled in jamovi serves every module and stays supported, where the same gap
worked around in a module is yours to maintain and breaks quietly when the
application moves.

The first two findings are structural — they're about how results and the panel
are built, not about details within them — and they're the ones to start with.

## What I Found

Ordered by how much I think they matter:

---

### [HIGH] The whole results model is one `Html` element

**File:** `jamovi/jmvtab.r.yaml:14`, `jamovi/jmvtabreg.r.yaml:10`
**Pattern:**
```yaml
items:
    - name: html_table
      title: Table
      type: Html
```

**Why this matters:** The comment above it is honest about the choice — tabxplor
renders one HTML table with the colour, the footer and the test summary all
inside it, so there's no `Table` element to declare. That's an accurate
description of what `R/tab-render-html.R` produces. But it means everything
jamovi does *with* a table stops at the boundary of that markup.

A `Table` is a structured object the application understands. It follows the
user's global number-format setting, so every table in a document agrees on
precision. It follows the results theme. It's navigable, annotatable, and
readable by a screen reader. Right-clicking one offers **Copy Latex** — `Html`
elements don't get that entry — and a plain **Copy** puts a real grid on the
clipboard that pastes into Excel or Word as cells rather than as markup. None of
that is recoverable once the output is markup; there's no hook a module can add
later to get it back.

**What I'd suggest:** Build the output from `Table` elements. The column set is
dynamic, which sounds like the hard part, but it's the ordinary case — declare
the fixed columns in the `.r.yaml` and add the rest in `.init()`:

```yaml
- name: table
  title: Crosstable
  type: Table
  rows: 0
  columns:
    - name: level
      title: ""
      type: text
  clearWith: [row_vars, col_vars, pct, wt]
```

```r
.init = function() {
    tbl <- self$results$table
    cv  <- self$options$col_vars[1]
    for (lv in levels(self$data[[cv]]))
        tbl$addColumn(name = lv, title = lv, type = 'number',
                      format = 'pc', superTitle = cv)
},

.run = function() {
    tbl <- self$results$table
    for (i in seq_len(nrow(tabs))) {
        tbl$addRow(rowKey = i, values = as.list(tabs[i, ]))
        if (tabs$p[i] < .05)
            tbl$addFootnote(rowNo = i, col = 'total', 'p < .05')
    }
}
```

`superTitle` gives you the grouped column headings you're currently drawing with
`<th colspan>`, and `addFootnote()` / `setNote()` give you the footer.

**On the colours.** jamovi tables are meant to be APA, and APA tables don't use
colour. So the question isn't how to get colour into a native table — it's
whether the colour is carrying information APA already has a convention for.
Deviations from the total can be a column of standardised residuals.
Significance can be footnote markers on the cells that reach it. Contributions to
variance can be their own column. If after working through it there's something
genuinely left over that can't be expressed that way, raise it as a feature
request describing what tabxplor computes per cell and what it needs to show.

---

### [HIGH] The Export block does something jamovi already does

**File:** `jamovi/jmvtab.a.yaml:714-762`, `jamovi/jmvtabreg.a.yaml:578`, `:602`;
`R/jmvtab-export.R` throughout
**Pattern:** `export_format`, `exportExcel`, `export_dir`, `export_filename`,
`resetPath`, `xl_replace` — backed by Windows-registry Documents-folder lookup
(`R/jmvtab-export.R:196`, `:207`), XDG `user-dirs.dirs` parsing (`:237`),
filename sanitisation (`:87`) and auto-numbering (`:110`).

**Why this matters:** A module may not implement an action that jamovi already
provides through its own interface, and writing files to a user-chosen path is
the clearest case. jamovi covers it: **Export…** on any results element,
whole-document export to PDF, HTML and LaTeX/BibTeX from the File menu, and
**Copy** for pasting a table straight into Excel or Word. A user who learns
jamovi's export learns it once and it works everywhere; an export button inside
one module is a second thing to learn that works in one place.

There's also a structural problem that no amount of care in the lookup code can
fix. The R engine doesn't always run on the user's machine — in jamovi Cloud it
runs server-side. `export_documents_dir()` will faithfully find a Documents
folder, read the right registry key, honour a redirected path, and write the file
somewhere the user has no way to reach. The better the detection, the more
convincingly it fails.

**What I'd suggest:** Remove the options from both `.a.yaml` files, the
corresponding block from both `.u.yaml` files, and `jmv_backend_export()` with
them. That also retires the format selector, the path and filename boxes, the
reset button, the `extCtrl` custom control that renders the file extension, and
the `setTimeout` that resets the action.

If there's something jamovi's export genuinely can't give you — a particular
workbook layout, say — that's a feature request.

---

### [MEDIUM] Five `CustomControl`s where native controls exist

**File:** `jamovi/jmvtab.u.yaml:308`, `:595`, `:665`;
`jamovi/jmvtabreg.u.yaml:56`, `:145`, `:196`, `:391`, `:464`
**Pattern:** `varTableCtrl`, `modelTableCtrl`, `modelBuilderCtrl`, `subtextCtrl`,
`extCtrl` — each an empty host whose contents `jamovi/js/jmvtab.js` and
`jamovi/js/jmvtabreg.js` build by hand.

**Why this matters:** A `CustomControl` is part of the API, but a hand-built
control has to re-implement behaviour the standard one gets for free — tracking
the variable list, repopulating when levels change, laying out correctly in the
grid — and it doesn't follow the theme.

**What I'd suggest:** Three of the five map onto existing controls.

*The per-variable reference picker* is a `ListBox` with columns. jmv's
`refLevels` in
https://github.com/jamovi/jmv/blob/master/jamovi/linreg.u.yaml does exactly this:

```yaml
- type: ListBox
  name: ref_levels
  showColumnHeaders: true
  fullRowSelect: true
  columns:
    - name: var
      label: Variable
      template: { type: VariableLabel }
    - name: ref
      label: Reference Level
      template: { type: LevelSelector, label: '' }
```

That's the whole control, with no JavaScript behind it. It's very nearly the
option you already declare at `jamovi/jmvtab.a.yaml:267` — the one change is
`ref` as `type: Level` rather than `type: String`, which is what lets
`LevelSelector` populate itself and retires the level-caching path in the events
JS.

*The model builder* is jmv's Model Builder, in the same file: a `Supplier` with
`format: term`, a `TargetLayoutBox` with `transferAction: interactions`, and a
`ListBox` with `addButton`. Blocks, drag-and-drop and an interactions button.

Worth looking at the option type while you're there. jmv's `blocks` is an `Array`
of `type: Terms`, and a term *is* a vector of variable names — so an interaction
needs no separate representation. `jamovi/jmvtabreg.a.yaml:207` splits the same
information into `vars: Array<Variable>` plus `crosses: Array<String>`, which
`jmvtab_reg_cross_keys()` and `jmvtab_reg_models()` then have to put back
together. Moving to `Terms` removes the custom builder and that reassembly in one
change.

*The level merge and reorder* has no equivalent in jmv or plots, so I have
nothing to point you at for that one.

*The subtext box* — jamovi has no multiline `TextBox`, so there's nothing
standard to reach for. But the purpose is already served: right-click any result
and **Add Note** gives editable text that sits under the element, persists in the
`.omv`, and comes through on export. Worth asking whether the option needs to
exist; if it does, a multiline `TextBox` is a feature request.

---

### [MEDIUM] The panel is labelled in R argument names

**File:** `jamovi/jmvtab.u.yaml:15`, `:23`, `:31`, `:41`, `:64`, `:428`, `:441`,
`:454`, `:467`, `:506`, `:578` and throughout; `jamovi/jmvtabreg.u.yaml` likewise
**Pattern:**
```yaml
label: <b>row_vars = <i>(row variables)</i></b>
label: pct = <i>(type of percentages)</i>
label: ci_method = c(mean_ratio = )
label: display = <i>(what numbers the cell show)</i>
label: totaltab = <i>(with tab_vars, add a total table)</i>
label: wrap_cols = <i>(nb chars for line break)</i>
```

**Why this matters:** I raised this in my first report and I'm raising it again
here, because it's the single thing that most affects how the panel reads, and a
visual inspection that left it out would be missing the point.

The intent is clear and the header comment in `R/jmvtab.b.R` states it as a
design rule: an option is named after the `tab()` argument it drives, so the
panel doubles as a way to learn the R API. But it puts the cost on the wrong
person. Someone who opens jamovi rather than RStudio has, by that choice, said
they don't want to think in function arguments. `ci_method = c(mean_ratio = )` is
not a label they can act on — it doesn't say what the control does, and it isn't
searchable in any way that helps them.

No analysis in jmv or the plots module labels a control this way. Their labels
name the decision: `Assumption Checks`, `Regression Line`, `Plot Orientation`,
`Reference Level`.

`totaltab = <i>(with tab_vars, add a total table)</i>` is the clearest case —
the parenthetical *is* a good human label, and the `totaltab = ` in front of it
is the part that isn't. That pattern holds across most of the panel: the English
is already written, it's just placed second.

**What I'd suggest:** Drop the argument names from the labels and let each one
name the decision:

```yaml
label: Total Table
label: Percentages
label: Mean ratio intervals
label: Characters before wrapping
```

This is a `.u.yaml` change only — no option is renamed, so no saved analysis
breaks. jamovi's convention is sentence case on individual controls and title
case on group headings and variable boxes, which falls out naturally once the
argument prefix is gone.

The argument mapping is worth keeping somewhere, just not in the panel. For now
that belongs in a document you give your readers alongside the module — a table
of control to argument. In-app help is on its way, and could be a better home for
it.

One label needs more than a rewrite: `<b><i>Choose reference categories, merge
and reorder levels, cut numeric variables:</i></b>` is a sentence rather than a
label, and wants three or four words.

---

### [MEDIUM] The collapse boxes group by mechanism rather than by decision

**File:** `jamovi/jmvtab.u.yaml:50`, `:211`, `:326`, `:475`;
`jamovi/jmvtabreg.u.yaml:49`, `:189`, `:239`, `:303`
**Pattern:**
```yaml
label: <b>Percentages, colors and tests</b>
label: <b>References (points of comparison), levels and missing values</b>
label: <b>Confidence intervals</b>
label: <b>Other formatting</b>
```

**Why this matters:** Four boxes for forty-four options means each box holds a
lot, and the names show it: the first bundles three unrelated decisions, and the
second bundles three more. A user looking for how missing values are handled has
no reason to open a box named for reference categories. "Other formatting" is a
remainder rather than a category.

jmv splits along the lines of what the user is deciding — `Model Fit`,
`Assumption Checks`, `Post Hoc Tests`, `Estimated Marginal Means` — and the plots
module does the same with `General Options`, `Plot & Axis Titles`, `Axes`,
`Legend`. More boxes with narrower names is easier to scan than fewer boxes with
compound ones.

**What I'd suggest:** Split the compound boxes so each has one subject.
`Percentages`, `Colours`, `Tests` as three boxes; `Reference Categories`,
`Levels`, `Missing Values` as three more. Within each, group related controls
under a `Label`, and use a two-column `cell: {column:, row:}` layout where a box
has two parallel groups — the Axes and Legend boxes in
https://github.com/jamovi/jmvplots/blob/master/jamovi/scat.u.yaml are the
pattern.

Where a box would then hold two mutually exclusive sets of controls, `ModeSelector`
with `Content` children is the native way to switch between them — the same file
uses it for title type and legend position.

---

### [MEDIUM] The events JS styles jamovi's own internals

**File:** `jamovi/js/jmvtab.js:116-126` (`injectTabxCss`), `:131-144`
(`styleExportSep`), `:150-168` (`bottomAlignInRow`); `jamovi/js/jmvtabreg.js`
likewise
**Pattern:**
```js
s.textContent =
    "input.silky-option-largest-text{min-width:260px !important; ...}" +
    "input.silky-option-large-text{min-width:0 !important; ...}" +
    ".jmv-collapse-view:not(.view-colapsed){padding-bottom:10px;}";
document.head.appendChild(s);
```
```js
if (node.classList.contains("silky-control-margin-large")) { ... }
if (node.classList.contains("silky-layout-cell")) { ... }
```

**Why this matters:** Those class names aren't API. They're jamovi's internal
markup, they carry no compatibility promise, and they change without notice — the
comment at `jamovi/js/jmvtab.js:113` records that an earlier set of guesses
(`.silky-options-collapse-box*`, `.jmv-options-collapsebox*`,
`.silky-layout-content`) matched nothing, which is why the rule never appeared.
That's the failure mode arriving once already. Note too that `.view-colapsed` is
a typo on jamovi's side; if it's ever corrected, the rule stops applying
silently.

The rules are also global rather than scoped to the tabxplor panel — appended to
`document.head` with an `#tabx-css` id guard that ensures they're never removed —
so they aren't confined to the controls they were written for.

**What I'd suggest:** Most of this exists to make hand-built DOM sit correctly in
a grid that native controls already fit, so it largely resolves itself along with
the `CustomControl` finding. For what remains, `stretchFactor` and the `width:`
property on the control are the supported levers. Where they aren't enough,
please file a feature request against the layout system rather than reaching into
its markup — a selector that works today is not a selector that will work, and we
would rather know what's missing.

---

### [LOW] Spelling and grammar in user-visible labels

**File:** as listed below
**Pattern:**

| Where | Reads | Should read |
|---|---|---|
| `jamovi/jmvtab.u.yaml`, `jmvtabreg.u.yaml` (4×) | `wrap_cols = (nb chars for line break)` | `nb` is French for *nombre* — "number of characters" |
| `jamovi/jmvtab.u.yaml:31` | `tab_vars = (subtables variables)` | "subtable variables" |
| `jamovi/jmvtabreg.u.yaml` | `display = (what numbers the cell show)` | "the cell shows" |
| `jamovi/jmvtabreg.u.yaml:189` | `Models comparison and interactions` | "Model comparison" |
| `jamovi/jmvtabreg.u.yaml` | `assumption checks plots` | "Assumption check plots" — and it's the only label in either panel that isn't capitalised |
| `jamovi/jmvtabreg.u.yaml` | `outcome = (dependent variable[s])` | "dependent variables" — the bracketed plural reads as a placeholder |

**Why this matters:** These are strings a user reads every time they open the
panel, and they're the details that make a module feel finished or not. The `nb`
abbreviation is the one I'd fix first — it's invisible to a French reader and
opaque to everyone else.

**Also, `colour` and `color` are mixed.** Every label and option name uses
`color`; thirteen option `description:` texts use `colour`
(`jamovi/jmvtab.a.yaml:132`, `:171`, `:452`, `:544`, `:546`;
`jamovi/jmvtabreg.a.yaml:386`, `:404`, `:410`, `:430`, `:431`, `:547`, `:549`).
Either is fine; pick one.

**What I'd suggest:** A pass over every `label:` and `title:` in both `.u.yaml`
files and every `description:` in both `.a.yaml` files. They're the strings most
likely to have been written once and never re-read.

---

### [LOW] `digits` restates a setting the user has already made

**File:** `jamovi/jmvtab.u.yaml:578-583`, `jamovi/jmvtabreg.u.yaml` likewise
**Pattern:** `label: digits = <i>(number of digits)</i>` with a `ComboBox`

**Why this matters:** jamovi carries a global results number format — three
significant figures and three-decimal p-values by default — which the user sets
once and expects everywhere. A per-module digits control means a tabxplor table
and the table above it in the same document disagree about precision, with
nothing on screen explaining why.

**What I'd suggest:** Drop it. Native `Table` columns follow the global setting
without the module doing anything. If a particular column genuinely needs fixed
precision — a count, say — `type: integer` says so directly.

---

### [LOW] Two weighting routes, and the panel doesn't say which won

**File:** `R/jmvtab-export.R:304-312` (`jmv_backend_weights`)
**Pattern:**
```r
if (!is.null(opt_wt) && length(opt_wt)) {
    wt <- opt_wt
} else if (!is.null(attr(data, "jmv-weights"))) {
    data[[".COUNTS"]] <- jmvcore::toNumeric(attr(data, "jmv-weights"))
    wt <- ".COUNTS"
}
```

**Why this matters:** This is correct, and I'm glad to see `weightsSupport:
'full'` honoured rather than declared and ignored. But a user who set row weights
in the spreadsheet *and* dropped a variable into the `wt` box gets the second
one, and nothing in the panel or the results says the first was set aside. It's
the kind of thing that's noticed after the analysis is written up.

**What I'd suggest:** Say so in the results. jmv does this with a `Notice` — see
the weights branch in
https://github.com/jamovi/jmv/blob/master/R/conttables.b.R:

```r
notice <- jmvcore::Notice$new(
    self$options, name = '.weights',
    type = jmvcore::NoticeType$WARNING,
    content = ..('The data is weighted by the variable {}.', wt))
self$results$insert(1, notice)
```

A `Notice` follows the results theme and sits properly in the results tree, which
also makes it the right replacement for the hand-styled banner that
`.compare_hint()` draws in `R/jmvtabreg.b.R`.

---

## What's Already Working Well

- **`compilerMode: tame`** on both `.u.yaml` files. With layout this hand-crafted
  that's exactly right — `jmvtools::install()` won't regenerate over it.
- **The variable boxes are conventional.** `VariableSupplier` with
  `TargetLayoutBox` children at the top of both panels, `permitted` and
  `suggested` set on every variable option, `maxItemCount: 1` on the weight box.
  Nothing custom where nothing custom was needed.
- **No nested collapse boxes.** All four in each panel sit at the top level, and
  flattening the interval methods out of a nested box (the comment at
  `jamovi/jmvtab.u.yaml:408`) was the right call.
- **Options are enabled rather than hidden** when they don't apply —
  `applyVarEnables()` greys `design_effect` without weights and the total-table
  controls without `tab_vars`, and `enable: (pct:row || pct:col)` does the same
  declaratively for `n`. A greyed control tells the user the feature exists and
  what it needs; a hidden one tells them nothing.
- **Every control maps to a declared option**, and the suffixed radio-button names
  all carry a valid `optionName`/`optionPart`. I found nothing orphaned.
- **The `hidden: true` reasoning is correct.** A `CustomControl` doesn't claim its
  option, so without it the compiler would generate a second, broken default
  control — the comments at `jamovi/jmvtab.a.yaml:286` and `:308` have this
  exactly right, and it's not obvious.

---

The two structural findings are the ones to start with: results built from
`Table` elements rather than markup, and the export block removed in favour of
what jamovi already does. The `CustomControl` and internal-CSS findings largely
fall out of those two, and the label and grouping findings are independent of all
of them.

One thing I've not repeated here, because my first report already covers it:
`Cramer’V` missing its `s` at `jamovi/jmvtab.u.yaml:198`. It belongs with the
spelling finding above.

— Claudia
