<!--
PURPOSE: Decide how tabxplor reaches jamovi users: rebuild the results on native `Table` elements for the
         jamovi library, or keep sideloading and solve the macOS builds.
ROLE:    Decision document written after the jamovi team's review of the 2.0.0 submission (September
         2026), and since the maintainer's decisions of 2026-09-17 the plan that follows from them
         (section "Decisions"). Nothing in it is implemented yet. Complements `dev/jamovi_library_submission.md` (what was sent) and
         `dev/jamovi_module.md` (how the module works). Not part of the package (`dev/` is .Rbuildignore'd).
KEY CONSTRAINTS:
  - Every claim about jamovi's behaviour is either MEASURED -- on the shipped jamovi 28.2 bundle (flatpak),
    on the 2.6.44 dev-console capture in `dev/jamovi/dev_console_live_capture/`, on the jamovi-compiler
    bundled in jmvtools 28.2 and 28.3 -- or cited to a URL. Web facts were read on 2026-09-17 and age
    fast (runner labels, CI free tiers, jamovi download names).
  - NOTHING here has been built or run on a Mac. The CI workflow of section 3.4 (macOS, Windows, Linux)
    is a sketch until its first green run AND a sideload test of each file.
  - The jamovi team's two reports (`dev/jamovi/2026-09-16*.md`) come from an automated reviewer reading
    `master` at 2.0.0. Section 4 checks every finding against the code and jamovi's own builds instead
    of taking it as given -- three of them are wrong, two are real bugs.
-->

# jamovi: native tables for the library, or sideloading with macOS builds

## Decisions

Taken by the maintainer on 2026-09-17, after this document's first version; nothing is implemented yet.

1. **Now, for the 2026–2027 school year: sideload, built by GitHub Actions for every platform** — macOS arm64 and Intel, Windows, Linux, each jamovi line that exists there — and published as GitHub Release assets (section 3).
2. **From September 2027: native `Table` elements by default**, dressed in an adapted `print_marks`, with **no R argument names** in the panel. Students move to it in September 2027, not before, whatever the date it becomes ready.
3. **Opt-in options restore today's behaviour** — the colour table and the argument names — and are to be negotiated with the jamovi team.
4. **Three requirements for that default**, answered in Part 4 (section 5):
   - a visually striking, permitted replacement for grey, which today marks what is not significant or below the threshold;
   - **bold reference rows and columns**, which cannot be lost;
   - how far borders can be set as tabxplor sets them.

## At a glance

**Native tables.** A jamovi `Table` cell holds a value, a bit-field of four layout flags, footnotes and a run of superscript symbols — and nothing else. The only colour is one fixed red (`Cell.NEGATIVE`), which jamovi's own Contingency Tables uses to highlight large residuals; bold, underline, grey and background cannot be expressed, and jamovi 28.2 strips `<b>` from text cells, where 2.6 still let raw HTML through. So the colour tables cannot survive, and neither can the typographic half of `print_ready`. What *does* survive is more than expected: every statistic, the observed-vs-adjusted regression columns, and **`print_marks` almost whole**, because its signal is a superscript run after the number — exactly what `Table$addSymbol()` draws. A native table is therefore not a stupid idea, but it is a different product: a publication table, not an exploration table.

**The three requirements of the September 2027 default (Part 4).**

- **Grey** has no native form and is not an APA device, so it gives way to **italic**, which APA allows with a note, which works today through text cells, and which survives Copy and LaTeX. It pairs with upright, marked cells and bold references, each voice keeping one job.
- **Bold** has no route at all in jamovi 28.2 — but jamovi is one small step away: a bold allow-list already exists, the LaTeX export already maps `<strong>`, Copy already keeps the weight, and APA allows bold with a note. The way is an upstream format bit, which degrades gracefully on older jamovis, with a lettered note on the reference as the interim.
- **Borders** are fixed by jamovi's stylesheet (booktabs-like rules only); APA's substitutes, white space and separate tables, cover what tabxplor's vertical rules do today.
- **R argument names**, taken off the labels, are already shown by jamovi's own **Syntax mode**, because tabxplor's options are named after `tab()`'s arguments.

**The hybrid is technically clean.** A native APA table by default and today's colour `Html` table behind one option is a few lines of `.r.yaml` (`visible: (tab_theme:apa)`), no recomputation (the choice is a render argument, already kept out of the cache key), and one new backend over the existing render model, the Excel exporter being its closest precedent. Whether the library accepts the opt-in `Html` view is a negotiation, not a technical question — and there are precedents: SummaryTables renders every table as `Html` and was featured on jamovi's blog, and the new submission process has a *community/experimental* tier meant for modules *"dependent on platform gaps"*.

**`print_ready` is APA in spirit, not in letter.** No colour, a note explaining every device, and even bold and italic are APA (with that note). Underline, grey ink, vertical rules, composite cells, stars at `.10/.05/.01`, p-values printed as percentages and leading zeros on bounded statistics are not.

**macOS builds are solved, for free, by a proven pattern.** The jamovi compiler builds a `.jmo` from the R inside a downloaded jamovi, without R on the machine and without opening the app. Three public modules already do it on GitHub's free macOS runners (arm64, and Intel until about August 2027), green in August–September 2026, one of them publishing per-platform files to its students as GitHub Release assets. Because jamovi's download page recommends *solid* (2.7.38, R 4.5.0) and a sideloaded module is disabled unless its R stamp matches the app's exactly, tabxplor needs both lines: four Mac files. rhub adds nothing; a Mac-less assembly on Linux is possible in principle but unnecessary.

**The two review reports sort into four piles.** The jamovi team's automated audit and visual inspection make twenty findings, two of them the same. Checked one by one against the code, jamovi 28.2 and jamovi's own library builds:

- **Two real bugs in the R package**, worth fixing whatever is decided: with the design effect on, survey tests go silently empty and weighted regressions fail on a variable name such as `Age group`; and a user's `subtext` reaches the results unescaped, where jamovi 28.2 runs scripts.
- **A handful of cheap corrections**: bounds, `clearWith`, spelling, a weights notice, citations.
- **Three findings that are wrong.** The top HIGH — that `Suggests:` packages are not installed with a module, so Excel export would be dead — is contradicted by jamovi's own library build of another module.
- **The rest are the library's conventions**: native tables, no module-level export, native controls, labels without argument names. They bind a library module, not the course build.

| Question                                  | Verdict                                                  |
|-------------------------------------------|----------------------------------------------------------|
| Reports: to fix in any case               | ✓ 2 package bugs, 5 small corrections                    |
| Reports: the `Suggests:` HIGH             | ✗ contradicted by jamovi's own library builds            |
| Colour on a native table?                 | ✗ one fixed red (`negative`), lost on Copy               |
| `print_ready` on a native table?          | ≈ `print_marks` yes (symbols); `print_emphasis` no       |
| Native APA default + colour `Html` opt-in | ✓ technically; acceptance to negotiate                   |
| Is `print_ready` APA?                     | ≈ semantics, bold, italic yes; underline, grey, rules no |
| Mac `.jmo` without a Mac                  | ✓ GitHub Actions, free, Release assets                   |
| Grey on a native table?                   | ✗ no route, not APA → italic with a note                 |
| Bold reference rows and columns?          | ✗ today → upstream format bit; a note letter meanwhile   |
| Borders set by the module?                | ✗ fixed rules → white space, separate tables             |
| R argument names outside the panel?       | ✓ jamovi's Syntax mode prints them already               |
| rhub for the `.jmo`                       | ✗ a GitHub Actions workflow built for `R CMD check`      |

---

## 1. The evidence base

Three kinds of evidence, in decreasing order of authority:

- **Jamovi's shipped code, read directly** — the jamovi 28.2 flatpak bundle (`~/.local/share/flatpak/app/org.jamovi.jamovi/current/active/files/lib/jamovi/client/assets/`: `resultsview-CUSr0Mlv.js`, `highcontrast-BWOQpgHk.js`, `main-Cq0DEaqo.js`, `quill-CVcqA7nS.js`, `resultsview-nOBIviKP.css`), its bundled `jmvcore` 2.7.38 and `jamovi.proto` (the R-side API was read on the system `jmvcore` 2.7.35, whose `jamovi.proto` is identical), the 2.6.44 capture (`dev/jamovi/dev_console_live_capture/127.0.0.1_56684_results/assets/resultsview-60a5863d.js`), and `jamovi-compiler` 0.3.5 as bundled in jmvtools 28.2 (`~/R/x86_64-pc-linux-gnu-library/4.6/jmvtools/node_modules/jamovi-compiler/`).
- **Measurements on tabxplor's own artefacts** — the installed Linux module (`~/.jamovi/modules/tabxplor/`), live `tab()` output, and macOS binaries downloaded from the snapshot the compiler uses, inspected on Linux with `lief`.
- **Web sources**, read on 2026-09-17 and listed in section 8.
- **The jamovi team's two reports**, dated 2026-09-16: `dev/jamovi/2026-09-16  jamovi team tabxplor.md` (the code audit) and `dev/jamovi/2026-09-16 jamovi team tabxplor visual inspection.md` (the panel and the results). Both are signed by an automated reviewer and read `master` at 2.0.0; section 4 goes through them finding by finding.

---

## 2. Part 1 — The library route: native `Table` elements

### 2.1 What the jamovi team asks

Damian's letter names three requests — build results from native `Table` elements, build the options panel from standard controls, drop the R argument names from the labels — and the attached reports add a fourth structural one, removing the module's own Export block (section 4.5), plus the smaller findings of section 4. On colour, the position is principled rather than technical: jamovi tables are meant to be APA, APA tables do not use colour, so the first question is whether what the colour says has an APA form already — *"deviations as a column of standardised residuals, significance as footnote markers, contributions to variance as their own column"* — and only what cannot be said that way is worth a special solution.

⚠ tabxplor already computes all three: `resid` (the adjusted standardised residual), the per-cell `pvalue` behind the stars, and `ctr` (the contribution to chi²) are `DISPLAY_TOKENS` rows today. The APA form is not missing from the package; it is missing from the *default layout*.

### 2.2 What a native table can carry — the whole data model

The protobuf message is the ceiling: whatever `jmvcore` offers in R, only these fields reach the client (`jamovi.proto` in the `jmvcore` bundled with jamovi 28.2, identical in 2.7.35):

| Level  | Field                                 | What it can express                                             |
|--------|---------------------------------------|-----------------------------------------------------------------|
| Cell   | value `i` / `d` / `s` / missing       | an integer, a double formatted by jamovi, or a string           |
| Cell   | `format` bit-field                    | 1 group start · 2 group end · 4 negative, *"red"* · 8 indent    |
| Cell   | `footnotes` (repeated string)         | a note under the table, lettered by jamovi                      |
| Cell   | `symbols` (repeated string)           | a literal superscript run after the value                       |
| Column | `type`, `format`                      | `integer`/`number`/`text`; `zto` `pvalue` `pc` `log10` `narrow` |
| Column | `title`, `superTitle`                 | one header row, and **one** spanning band above it              |
| Column | `combineBelow`, `visible`, `sortable` | blank repeats down a column; show or hide; sort button          |
| Table  | `notes` (key → text)                  | free lines under the table                                      |
| Table  | `swapRowsColumns`, `sortSelect`       | transpose; sorting                                              |

R reaches them through `Table$addFootnote()`, `$addSymbol()`, `$addFormat()` with the `Cell.BEGIN_GROUP` / `Cell.END_GROUP` / `Cell.NEGATIVE` / `Cell.INDENTED` constants, `$setNote()`, and `$addColumn()` at run time. There is no field for a colour, a background, a font weight, a font style, a border or a horizontal cell span.

### 2.3 What jamovi 28.2 draws from it

Read in `resultsview-CUSr0Mlv.js` and its stylesheet:

- **A number cell** is formatted by jamovi's own formatter (`renderMode: "trusted"` — trusted because jamovi wrote it). The user's number settings apply: significant figures or decimal places, p-value digits, decimal separator. `format: pvalue` prints `< .001` and drops the leading zero; `zto` fixes the decimals; `pc` multiplies by 100 and appends ` %`.
- **A text cell, a column title and a table title** are "rich": parsed, then rebuilt from an allow-list. The default list is `em`, `i`, `sub`, `sup`; every attribute named `on…`, `style`, `srcdoc` or `srcset` is dropped, and a tag outside the list is unwrapped to its text. **So `<b>`, `<strong>`, `<u>` and `<span style>` never render in a native table**, and italic is available only by turning a number into text.
- **`negative`** adds the class `jmv-results-table-cell-negative`, styled `td.jmv-results-table-cell-negative{color:#d00}` — one fixed red, the only colour a native table has (section 2.7 shows who uses it).
- **`symbols`** are concatenated into `<span class="jmv-results-table-sup">`, styled `position:absolute` — they do not widen the column, so a long run spills into the neighbouring cell's padding.
- **`footnotes`** are de-duplicated per table and lettered with modifier letters (`ᵃ ᵇ ᵈ ᵉ …` — the list skips *c*).
- **`notes`** each become a footer row opening with an italic *Note.* — APA's general-note form, for free.
- **Begin group** adds 8 px above the row; **indented** adds 24 px before the text.
- **A column named `x[suffix]`** is folded into sub-rows: every row of the table is repeated once per suffix. That is how jmv's Contingency Tables prints *Count / % within row / % within column* under each level (`dev/jamovi/reference/jmv-conttables/conttables.r.yaml`).

### 2.4 The direction of travel: 2.6 concatenated, 28.2 sanitises

In the 2.6.44 capture the same renderer builds each row as a string, `` `<td … class="jmv-results-table-cell ${X}">${q}<span class="jmv-results-table-sup">${D.sups}</span></td>` ``, with the cell's value `q` inserted raw — so on 2.6 a text cell could carry any markup, colour included. In 28.2 that string is gone, replaced by the sanitiser of section 2.3. The same tightening is already recorded in `dev/jamovi_library_submission.md`: on 28.x, `<b>` stopped rendering in variable-selector labels and `<i>` stopped being stripped in dropdown choices.

**Consequence:** any plan that smuggles styling into native cells through HTML is closed, and was closed on purpose. The `Html` element itself is still trusted in 28.2 — its content is parsed without an allow-list, and, unlike what `dev/jamovi_module.md` §6.2 records from the 2.6 capture, inline `<script>` elements are now re-created in the document head and therefore run.

### 2.5 What survives Copy, for both kinds of result

jamovi's *Copy* serialises the rendered DOM through one walker (`highcontrast-BWOQpgHk.js`, the `text/html` branch). It keeps the tags `table`, `thead`, `tbody`, `tfoot`, `tr`, `td`, `th`, `caption` (turned into a header row), `em`, `strong`, `b`, `u`, `s`, `sub`, `sup`, `a`, `p`, lists and headings; for `td` / `th` it inlines only the computed `text-align`, `padding`, `border`, `vertical-align` and `font-weight`; it drops `<style>` and SVG, and unwraps every other tag to its text. So:

| What a cell says     | Native table            | tabxplor `Html` today             | After *Copy*       |
|----------------------|-------------------------|-----------------------------------|--------------------|
| Colour or background | red via `negative` only | slot classes + a stylesheet       | ✗ always lost      |
| Bold                 | ✗                       | `<b>` and computed weight         | ✓                  |
| Underline            | ✗                       | `<u>`                             | ✓                  |
| Italic               | `<i>`/`<em>` in text    | `<i>` (`R/tab-render-html.R:234`) | `<em>` ✓ · `<i>` ✗ |
| Marks or stars       | `symbols`               | cell text                         | ✓                  |
| A real grid of cells | ✓                       | ✓ (it is a `<table>`)             | ✓ both             |

Three things follow. The *"copied into Excel or Word as real cells"* argument is not unique to native tables — tabxplor's table is a real `<table>` and pastes as one; what native tables uniquely get is jamovi's number settings and the LaTeX export (section 2.6). A copied colour table arrives in Word bold but colourless. And a copied `print_minimalistic` or `print_emphasis` table **loses its italics today**, because the semantic face is emitted as `<i>` and the walker keeps only `<em>` — a one-token fix in `R/tab-render-html.R`, worth doing whatever is decided here. (On 2026-09-17 jamovi's `main` branch merged a fix from the SummaryTables author, jamovi issue #1864: the walker now keeps `<br>` and the computed `font-style` of `td`, `th` and `span`. That covers a CSS italic, not an `<i>` tag, so the fix above stays useful; and until that release, a header tabxplor wraps with `<br>` loses its line breaks on Copy.)

### 2.6 The LaTeX export

*Copy LaTeX* and the LaTeX bundle export (`main-Cq0DEaqo.js`) write a document with `\documentclass[…]{apa7}`. A native table becomes a `\begin{table}` / `tabular` with its caption and label; an `Html` result goes through a rich-text flattener that keeps bold, italic, underline, sub/superscript, links and lists — **a tabxplor table becomes paragraphs of text, not a tabular**. This point in Damian's letter is fully accurate.

### 2.7 How far `print_ready` is from APA

`print_ready` is not a palette but a choice (`PRINT_READY`, `R/tab-palettes.R`): a crosstab gets `print_marks`, a regression `print_emphasis`, a caller with no table `print_minimalistic`.

APA 7 on the devices those three palettes use (sources in section 8; ⚠ quotes marked *secondary* come from university guides reproducing the Publication Manual, which could not be read directly):

| Device                         | Used by                | APA 7                              | Native table    |
|--------------------------------|------------------------|------------------------------------|-----------------|
| No colour, a note explains all | all three              | ✓ the *general note*               | ✓ `setNote()`   |
| Bold                           | emphasis, minimalistic | ✓ if the general note explains it  | ✗               |
| Italic                         | emphasis, minimalistic | ✓ same rule                        | ≈ text cells    |
| Superscript marks `⁺`/`⁻` ×1–4 | marks                  | ≈ symbols explained in a note      | ✓ `addSymbol()` |
| Underline, double underline    | all three              | ✗ not an APA device                | ✗               |
| Grey ink, non-significant cell | all three              | ✗ not APA (APA: avoid shading)     | ✗               |
| Vertical rules between blocks  | html, Excel            | ✗ "do not use vertical borders"    | ✓ none          |
| Composite cell `12% (n=5)`     | layouts                | ≈ APA prefers one index per column | ✗ → columns     |

And four number formats that are tabxplor **defaults**, not palette facts — each measured on a live `tab()`:

| tabxplor prints                    | APA 7                                 | Native table             |
|------------------------------------|---------------------------------------|--------------------------|
| stars `*` .10, `**` .05, `***` .01 | "traditionally" .05, .01, .001        | cut-offs stay tabxplor's |
| chi² p-value `<0.01%`              | `p < .001`, no leading zero           | ✓ `format: pvalue`       |
| Cramér's V `0.15`                  | `.15`: no leading zero below 1        | ≈ `zto` keeps the zero   |
| CI `[3;34]%`                       | ✓ brackets; comma separator `[3, 34]` | text cell                |

**Where `print_ready` stops being APA.** Its semantics are APA: no colour, a significance gate, and a note that says what each device means. Bold and italic are APA as long as that note exists — the APA Style blog (February 2026) recommends exactly that for highlighting significant results. What is not APA is a short, precise list: underline and double underline, grey ink, vertical rules, composite cells, and the four number formats above. None of them is essential to the idea. An APA-conformant publication palette — `print_emphasis` without its underlines, and with a non-significant cell left plain rather than grey — plus APA number defaults would be a small change for the Html, Excel and Word outputs. **On a native table it would still not render**: bold is exactly what jamovi's cells cannot carry, so there the APA-conformant subset is `print_marks` without its underline.

**jamovi's own analyses already colour native cells.** The jmv bundled with jamovi 28.2 has, in Contingency Tables, an option *"Highlight values above"* — *"highlight standardized residuals above this threshold in the post hoc tests table"* — implemented with `Cell.NEGATIVE`, the red of section 2.3 (added in 2025 by a contributor; CFA does the same, and the library module ChiSquaredTools highlights significant residuals the same way). Asked about colour coding on the forum in November 2025, jamovi's lead developer answered that it was never a priority but *"seems like something we could reasonably add"*. So "APA tables do not use colour" is jamovi's design intent, not a rule its own crosstab keeps — which is worth saying, politely, in the reply.

### 2.8 tabxplor, feature by feature, on a native table

What each part of a tabxplor table would become — ✓ kept, ≈ kept in another form, ✗ lost:

| tabxplor                             | On a native `Table`                                             |     |
|--------------------------------------|-----------------------------------------------------------------|-----|
| Weights, survey design, CI methods   | computed by R as today                                          | ✓   |
| Percentages, means, counts           | `number` columns, `pc` format; jamovi's digits replace `digits` | ✓   |
| Composite cell `12% (n=5)`           | an aside column (as `tab_xl()`) or a `[suffix]` sub-row (jmv)   | ≈   |
| Colour ladder (4 rungs × 2 sides)    | `print_marks` superscripts via `addSymbol()`, with a note       | ≈   |
| Greyed non-significant cell          | nothing: the cell is simply unmarked                            | ✗   |
| Stars                                | `addSymbol()` — the APA form                                    | ✓   |
| Residuals, contributions             | their own columns or sub-rows (`resid`, `ctr`)                  | ✓   |
| 3-band header (span / level / unit)  | 2 bands: `superTitle` + `title`, the unit folded into the title | ≈   |
| Variable names turned vertically     | a `text` column with `combineBelow`                             | ≈   |
| Total row, sub-table boundaries      | `Cell.BEGIN_GROUP` spacing — no rules                           | ≈   |
| Footer: legend, weight, CI method    | `setNote()` lines                                               | ✓   |
| Test rows (chi², V, model fit)       | a second table, as jmv's "χ² Tests"                             | ✓   |
| Regression: model beside observed    | adjacent columns under one `superTitle`                         | ✓   |
| Tooltips, data bars, sparkline notes | nothing                                                         | ✗   |
| Theme, publication palettes          | jamovi's own table look                                         | ✗   |

The number rendering deserves a word: on a native table *jamovi* formats every double. tabxplor's `format()` — the package's one display source of truth — would stop deciding digits, signs (`+3%`), multiplicative folds (`1/2.11`) and brackets. The Excel exporter already solved the neighbouring problem (one number per cell, everything else in the number format); a jamovi backend has no number-format code to hide literals in, so every literal becomes either a symbol, a sub-row or a text cell.

### 2.9 What would be left of tabxplor's spirit

The part of tabxplor that is about **numbers** survives: inference on weighted and complex-survey data, named interval methods, the observed-vs-adjusted regression layout, the tests. The part that is about **reading at a glance** does not: the colour ladder, the greying that says *this difference is noise*, the intensity that jumps out before a single figure is read. For a regression table the loss is moderate — it is already a column of numbers — and the observed column beside the model one, the feature that makes `tab_reg()` distinctive, is entirely native. For a crosstab the loss is the product itself: a native crosstab with superscript marks is a good publication table and an ordinary exploration table.

So the answer to *"would there be nothing left of the tabxplor spirit"* is: the statistics and the regression idea survive intact, `print_marks` survives nearly whole, and the colour — the reason a student reads a crosstab in five seconds — survives nowhere. A native-only module is not stupid; it is a second product, `tabxplor` for the paper rather than for the exploration.

### 2.10 The hybrid — decided: a native default, the colour table on request

**Technically it is clean.** Measured in `jmvcore` 2.7.35, a `visible:` expression accepts the `option:value` form (`Options$eval()` substitutes `name:value` with `self$has(name) && value %in% self$get(name)`), so one `.r.yaml` can declare both:

```yaml
items:
    - name: tables           # native, the default
      type: Array
      visible: (tab_theme:apa)
      template:
          type: Table
          columns: []        # added at run time with addColumn()
    - name: html_table       # today's result, on request
      type: Html
      visible: (!tab_theme:apa)
```

- **No recomputation.** The theme is a render argument, and `R/jmvtab.b.R` already keeps render arguments out of `.opts()`, the cache key's complement — switching views re-renders from the cached tables.
- **One backend, over the existing render model.** `tab_export_prep()` already decides header bands, label runs, block boundaries, faces, marks and footer members for four media; a `tab_jmv_table()` would be the fifth consumer, closest to `tab_xl()` (one value per cell, asides as columns). It fills `Table` objects instead of writing strings.
- **The regression analysis is the easier half**, and a sensible first step: its tables are already columns of numbers with a footer.

**The costs.** Two renderings to keep in step in the tests; a native table's look that tabxplor does not control (a jamovi restyle changes it); and the risk that the library still declines the `Html` view. Three facts matter for that negotiation:

- **Html tables are already in the library.** SummaryTables renders every one of its tables as `Html` (gtsummary, with options such as *"Bold significant p-values"*), and jamovi featured it on the official blog in July 2026 as *"Publication-Ready Summary Tables"*; ClinicoPathDescriptives renders its cross tables as `Html`. Most other modules use `Html` only for explanations.
- **The developer documentation discourages, but does not forbid.** The `Html` page says such elements *"should be used sparingly to maintain a consistent look and feel"*, and that *"for standard statistical results, `Table` and `Image` elements are preferred."*
- **The library now has tiers.** The submission process moved to GitHub issues in July 2026 (`jamovi/jamovi-module-submissions`), and a decision is one of *curated*, *community/experimental*, *blocked pending changes*, *rejected* or *needs expert review*. A curated module *"avoids unsupported jamovi API workarounds unless explicitly approved"*; the community/experimental tier is explicitly *"for modules that are useful, niche, early, imperfect, or dependent on platform gaps"*, published on a *"lower-trust surface"*. Damian's letter did not mention tiers, so whether tabxplor as it is today could enter at that level is a question to ask (section 7). ⚠ The documents do not say whether community modules are built by jamovi for every platform, as curated ones are; if they are, Part 2 becomes moot.

**What to call the options.** Following Damian's rule that a label names the decision, not the argument: *Table style* → *APA table* (the native default of Part 4) / *Colours (exploration)* / *Black and white (publication)*. The last two are today's `light` and `print_ready` `Html` tables, the opt-in views to negotiate. The argument names need no option of their own (section 5.7).

### 2.11 The rest of the review

The panel requests — standard controls, labels without argument names, narrower collapse boxes, no styling of jamovi's internals — and the removal of the Export block are independent of the table question. Section 4.5 treats them together, with what each costs.

---

## 3. Part 2 — Sideloading: building for macOS

### 3.1 Why a `.jmo` is platform-specific — measured on tabxplor's own build

A `.jmo` is a zip of the module directory: `jamovi.yaml` (stamped `rVersion: 4.6.0-x64` on the Linux build), the compiled UI, translations, and an `R/` library holding the module package plus every dependency jamovi does not bundle. The installed 28.2 Linux module holds 16 packages:

| Kind              | Packages                                                                       |
|-------------------|--------------------------------------------------------------------------------|
| Pure R (9)        | tabxplor, brant, clipr, DBI, insight, marginaleffects, mirai, mitools, svyVGAM |
| Compiled code (7) | fansi, nanonext, openxlsx2, parallelly, RhpcBLASctl, survey, VGAM              |

The nine pure-R packages record no platform in their `Built:` field (`R 4.6.0; ; …; unix`); the seven compiled ones record `x86_64-pc-linux-gnu`.

The compiler installs `Depends`, `Imports` **and `Suggests`** (`compilerr.js`, the `DESCRIPTION` parser), which is why `openxlsx2`, `VGAM` and the parallel stack are there. Those seven compiled packages are the only reason a build cannot be shared between systems — and the jamovi documentation states the rule plainly: a `.jmo` built on an Intel Mac works neither on Windows nor on an Apple-silicon Mac.

**What jamovi checks when a student sideloads** (jamovi server source, `server/jamovi/server/modules/modules.py`, read on GitHub on 2026-09-17):

- **The `rVersion` stamp, by exact string equality** with the app's own (`4.5.0-arm64`, `4.6.0-x64`, …). On a mismatch the module is still unpacked, but flagged *incompatible*: the library lists it as needing an update and its analyses answer *"This module is either missing or incompatible"*.
- **Nothing else.** No operating-system field, no inspection of the compiled code, and no signature: the ed25519 signature check added in jamovi 2.7.32 applies only to files downloaded from `library.jamovi.org`, never to a sideload.

⚠ So **the stamp does not name the system**. `4.6.0-x64` is the stamp of jamovi 28.x on Linux, on Windows *and* on an Intel Mac; `4.5.0-x64` that of solid on Windows and on an Intel Mac. A file built for the wrong system but the right stamp installs cleanly and fails only when R loads its first compiled package — the likely story of the students for whom the Windows build "did not work" on a Mac. The file names must carry the system, since jamovi will not.

### 3.2 How many builds a class needs

jamovi's download page (read on 2026-09-17) offers Windows and macOS users **Solid 2.7.38**, *"the tried & tested release we recommend"*, with Current one click away; Linux users get the latest release through Flathub, and Linux has no solid line at all. Since the stamp must match exactly:

| Student's computer       | Solid 2.7.38 (the default) | Current 28.2  | Built by (decided) |
|--------------------------|----------------------------|---------------|--------------------|
| Windows                  | `4.5.0-x64`                | `4.6.0-x64`   | CI, both lines     |
| Mac, Apple silicon (M1…) | `4.5.0-arm64`              | `4.6.0-arm64` | CI, both lines     |
| Mac, Intel               | `4.5.0-x64`                | `4.6.0-x64`   | CI, both lines     |
| Linux x64                | —                          | `4.6.0-x64`   | CI, current only   |

Seven files cover every student; the three solid ones (Windows, both Macs) cover the students who follow the download page's advice. Each jamovi series change (a new solid line, a 29.x) repeats the set.

### 3.3 What the compiler does on a Mac — read from its source

`jamovi-compiler` 0.3.5 (the one jmvtools 28.2 ships; the 28.3 copy differs only as noted) decides everything by `process.platform`:

- **Where jamovi is.** `--home` accepts `…/jamovi.app`; by default it looks for `/Applications/jamovi.app/Contents/MacOS/jamovi` (`installer.js`). Its R is `jamovi.app/Contents/Frameworks/R.framework/Versions/Current/Resources/bin/R` (`index.js`).
- **Which jamovi it accepts.** Majors 2 to 28 (`if (mas > 28) throw …`), so **one compiler builds both lines**. ⚠ The standalone `jamovi/jamovi-compiler` on GitHub/npm still refuses anything newer than 2.7; the compiler that accepts 28 is the one inside `jmvtools` — and the `jmvtools_28.3.tar.gz` *source* tarball on `repo.jamovi.org` ships it with its `node_modules`, ready for any `node`.
- **The stamp.** `R --version` gives the version and the platform; `aarch64` becomes `arm64`, `x86_64` becomes `x64`, hence `rVersion: 4.6.0-arm64` or `4.6.0-x64` (the same strings the library's own macOS indexes use).
- **The packages.** `install.packages(type = getOption("pkgType"), repos = …)` from the snapshot keyed by that R version: for R 4.6.0 `https://repo.jamovi.org/cran/2026-05-11` then `https://packagemanager.posit.co/cran/2026-05-11`; for R 4.5.0 the same pair at `2025-05-25`. jamovi's own repository holds only a handful of rebuilt binaries (15 for arm64, measured); everything else comes from Posit's snapshot.
- **The Mac-only patch.** After installing, it runs `/usr/bin/otool -L` on each package's `.so` and, for any dependency named `libR.dylib`, `libRlapack.dylib`, `libRblas.dylib`, `libgfortran.5.dylib`, `libquadmath.0.dylib`, `libXrender.1.dylib` or `libomp.dylib` (28.2 also listed `libc++.1.dylib` and `libc++abi.1.dylib`; 28.3 dropped them), rewrites the path with `/usr/bin/install_name_tool -change` to `@executable_path/../Frameworks/R.framework/Versions/<v>/Resources/lib/…` — jamovi's bundled copy, so the student needs no R of their own. `<v>` is read from the framework itself (`4.6` on arm64, `4.6-x86_64` on Intel, `4.5-arm64` on solid arm64). This needs the Xcode command-line tools, and the ad-hoc code signature needs no extra step: the library's own patched binaries carry a valid one (measured on `brunnermunzel.so`), while the compiler never calls `codesign` — so, by inference, Apple's `install_name_tool` renews it.
- **Build without installing.** `jmc --build <src> --home <app> --jmo <file>` writes the `.jmo` and stops; `--install` is what additionally launches jamovi.
- **Build without launching jamovi's window.** The only app call a build makes is `jamovi --version`, which Electron answers before any window exists; `--assume-app-version <x.y.z>` removes even that, the version being used for nothing but the `minApp` check. A build needs jamovi's *files* — its R — and never its GUI, which is why it runs on a CI machine.

**Binary availability, measured against those snapshots** (all 16 dependencies, at the exact versions of the Linux build):

| Snapshot (R)         | arm64 binaries                                      | Intel binaries |
|----------------------|-----------------------------------------------------|----------------|
| 2026-05-11 (R 4.6.0) | 15 of 16; `openxlsx2` 1.26 source only              | same           |
| 2025-05-25 (R 4.5.0) | the 7 compiled ones all there, `openxlsx2` included | same           |

So a build for jamovi 28.x compiles exactly one package from source (`openxlsx2`, C++ through Rcpp — no Fortran), which the Xcode command-line tools should cover; the solid build compiles nothing. jamovi ships those Posit binaries unchanged: in the library's own macOS arm64 build of SummaryTables, `bit.so` is byte-identical to Posit's `bit_4.6.0.tgz` at the same snapshot.

### 3.4 Route A — GitHub Actions (decided, for every platform)

**The runners** (GitHub documentation, 2026-09-17). Standard hosted runners are free and unlimited for public repositories:

| Label                         | Architecture | vCPU / RAM | Availability                                         |
|-------------------------------|--------------|------------|------------------------------------------------------|
| `macos-26`, `macos-latest`    | arm64 (M1)   | 3 / 7 GB   | `macos-latest` = macOS 26 since July 2026            |
| `macos-15`                    | arm64 (M1)   | 3 / 7 GB   | current                                              |
| `macos-14`                    | arm64 (M1)   | 3 / 7 GB   | ⚠ retired on 2026-11-02 — do not use                 |
| `macos-15-intel`              | x86_64       | 4 / 14 GB  | announced as the last Intel image, until August 2027 |
| `macos-26-intel`              | x86_64       | 4 / 14 GB  | GA 2026-02-26; no separate end date published        |
| `-large` / `-xlarge` variants | either       | more       | ✗ always billed, even on public repositories         |

Limits that matter here: 6 h per job, at most 5 concurrent macOS jobs on the Free plan. Rosetta 2 is installed on the arm64 images.

**Getting the file out.** Two mechanisms, and for students only one of them works:

- **A workflow artifact** (`actions/upload-artifact@v7`, and since 2026-02-26 `archive: false` uploads a single file unzipped). Downloading it requires a GitHub login; retention is 90 days. Good for the maintainer's own testing.
- **A GitHub Release asset** (`gh release upload <tag> <file> --clobber`, or `softprops/action-gh-release`). A public, permanent URL of the form `https://github.com/BriceNocenti/tabxplor/releases/download/<tag>/<file>`, no login, 2 GiB per file. **This is the student link.**

**Triggers.** `workflow_dispatch` (a *Run workflow* button) and a tag push. ⚠ A `workflow_dispatch` workflow must exist on the **default branch** (`master`) to be runnable; it can then build `dev` through the branch selector or `gh workflow run jmo.yaml --ref dev`.

**It has been done — three public precedents** (GitHub, read on 2026-09-17):

| Module                | What it builds                        | Record                                         |
|-----------------------|---------------------------------------|------------------------------------------------|
| `byurk/Randomize`     | 2.7.38: macOS arm64 + Intel, Windows  | green on a tag; Release v0.2.0 ships the files |
| `usyd-soles-edu/miso` | the same three, SHA-256 pinned        | green 2026-09-12; artifacts only               |
| `torryscott/pandion`  | macOS arm64 + Windows, **2.7 and 28** | green 2026-09-14; stamp `4.6.0-arm64`          |

Randomize and miso use the leanest recipe, and it is the one to copy: download jamovi's own installer, mount it, fetch the `jmvtools` source tarball, and run the compiler inside it with the runner's `node` — **no R, no jmvtools installation, no jamovi launch**. Two details are not in any documentation: jamovi's CDN refuses a download without a browser user-agent and a `Referer`, and `dl.jamovi.org` no longer resolves (`dl-cdn.jamovi.org` and `archives.jamovi.org` do; the latter keeps old versions). Pandion's detour for 28.x (installing jmvtools into jamovi's own R) is only needed because it uses the standalone compiler, which refuses 28.

**The workflow for tabxplor**, adapted from Randomize to both lines, to stable asset names and — for Linux — from jwellplate's flatpak route. ⚠ Untested for tabxplor; its first run is the test.

```yaml
# .github/workflows/jmo.yaml -- sideloadable .jmo files for macOS, Windows and Linux, every jamovi line.
# Must live on the default branch (master) to be runnable by hand; `--ref dev` then builds dev.
name: jmo

on:
  workflow_dispatch:
  push:
    tags: ['v*']

permissions:
  contents: read

env:
  JMVTOOLS_VERSION: "28.3"   # its compiler accepts jamovi 2.x to 28.x

jobs:
  build:
    name: jamovi ${{ matrix.line }} / ${{ matrix.platform }}
    runs-on: ${{ matrix.os }}
    timeout-minutes: 60
    strategy:
      fail-fast: false
      matrix:
        include:
          - { line: "2.7", jamovi: "2.7.38.0", os: macos-latest,   platform: macos-arm64, ext: dmg }
          - { line: "2.7", jamovi: "2.7.38.0", os: macos-15-intel, platform: macos-x64,   ext: dmg }
          - { line: "2.7", jamovi: "2.7.38.0", os: windows-latest, platform: win-x64,     ext: zip }
          - { line: "28",  jamovi: "28.2.0.0", os: macos-latest,   platform: macos-arm64, ext: dmg }
          - { line: "28",  jamovi: "28.2.0.0", os: macos-15-intel, platform: macos-x64,   ext: dmg }
          - { line: "28",  jamovi: "28.2.0.0", os: windows-latest, platform: win-x64,     ext: zip }
    defaults:
      run:
        shell: bash
    steps:
      - uses: actions/checkout@v5

      - uses: actions/cache@v4
        id: cache
        with:
          path: ${{ runner.temp }}/jamovi.${{ matrix.ext }}
          key: jamovi-${{ matrix.jamovi }}-${{ matrix.platform }}

      - name: Download jamovi
        if: steps.cache.outputs.cache-hit != 'true'
        run: |
          # the CDN turns away requests without browser-like headers
          curl -fL --retry 3 --max-time 900 \
            -A "Mozilla/5.0 (Macintosh; Intel Mac OS X 14_0) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/128.0 Safari/537.36" \
            -H "Referer: https://www.jamovi.org/" \
            -o "$RUNNER_TEMP/jamovi.${{ matrix.ext }}" \
            "https://archives.jamovi.org/jamovi-${{ matrix.jamovi }}-${{ matrix.platform }}.${{ matrix.ext }}"

      - name: Unpack jamovi
        run: |
          if [ "$RUNNER_OS" = "Windows" ]; then
            7z x -y -o"$RUNNER_TEMP/jamovi-dist" "$RUNNER_TEMP/jamovi.zip" > /dev/null
            echo "JAMOVI_HOME=$(cygpath -m "$RUNNER_TEMP")/jamovi-dist/jamovi" >> "$GITHUB_ENV"
          else
            mnt=$(hdiutil attach -nobrowse -readonly "$RUNNER_TEMP/jamovi.dmg" | grep -o '/Volumes/.*' | head -1)
            cp -R "$mnt/jamovi.app" "$RUNNER_TEMP/jamovi.app"
            hdiutil detach "$mnt" -quiet || true
            echo "JAMOVI_HOME=$RUNNER_TEMP/jamovi.app" >> "$GITHUB_ENV"
          fi

      - name: Fetch the compiler (inside the jmvtools source tarball)
        run: |
          TMP=$(cygpath -u "$RUNNER_TEMP" 2>/dev/null || echo "$RUNNER_TEMP")
          curl -fsSL --retry 3 -o "$TMP/jmvtools.tar.gz" \
            "https://repo.jamovi.org/src/contrib/jmvtools_${JMVTOOLS_VERSION}.tar.gz"
          tar xzf "$TMP/jmvtools.tar.gz" -C "$TMP"

      - name: Build
        run: |
          OUT="tabxplor_jamovi-${{ matrix.line }}_${{ matrix.platform }}.jmo"
          node "$RUNNER_TEMP/jmvtools/inst/node_modules/jamovi-compiler/index.js" \
            --build . --home "$JAMOVI_HOME" --jmo "$OUT"
          [ "$(wc -c < "$OUT")" -gt 5000000 ] || { echo "suspiciously small .jmo"; exit 1; }
          # the stamp, in the log (7z on the Windows runner, unzip on macOS)
          { unzip -p "$OUT" tabxplor/jamovi.yaml 2>/dev/null || 7z x -so "$OUT" tabxplor/jamovi.yaml; } | grep '^rVersion'
          echo "JMO=$OUT" >> "$GITHUB_ENV"

      - uses: actions/upload-artifact@v7
        with:
          path: ${{ env.JMO }}
          archive: false
          if-no-files-found: error

  # Linux has no solid line, and jamovi for Linux ships only through Flathub: build with the flatpak
  # jamovi, whose sandbox sees $HOME -- where the runner's workspace lives.
  build-linux:
    name: jamovi 28 / linux-x64
    runs-on: ubuntu-latest
    timeout-minutes: 60
    steps:
      - uses: actions/checkout@v5

      - name: Install jamovi and the Sdk its runtime needs
        run: |
          sudo apt-get update && sudo apt-get install -y flatpak
          flatpak remote-add --user --if-not-exists flathub https://dl.flathub.org/repo/flathub.flatpakrepo
          flatpak install --user -y --noninteractive flathub org.jamovi.jamovi
          # `--devel` needs the Sdk matching jamovi's runtime (25.08 for 28.2); read it, never hard-code it
          runtime=$(flatpak info --user org.jamovi.jamovi | awk '/Runtime:/ {print $2}')
          flatpak install --user -y --noninteractive flathub "${runtime/Platform/Sdk}"
          flatpak run org.jamovi.jamovi --version

      - name: Fetch the compiler (inside the jmvtools source tarball)
        run: |
          curl -fsSL --retry 3 -o "$RUNNER_TEMP/jmvtools.tar.gz" \
            "https://repo.jamovi.org/src/contrib/jmvtools_${JMVTOOLS_VERSION}.tar.gz"
          tar xzf "$RUNNER_TEMP/jmvtools.tar.gz" -C "$RUNNER_TEMP"

      - name: Build
        run: |
          OUT="tabxplor_jamovi-28_linux-x64.jmo"
          node "$RUNNER_TEMP/jmvtools/inst/node_modules/jamovi-compiler/index.js" \
            --build . --home flatpak --jmo "$OUT"
          [ "$(wc -c < "$OUT")" -gt 5000000 ] || { echo "suspiciously small .jmo"; exit 1; }
          unzip -p "$OUT" tabxplor/jamovi.yaml | grep '^rVersion'
          echo "JMO=$OUT" >> "$GITHUB_ENV"

      - uses: actions/upload-artifact@v7
        with:
          path: ${{ env.JMO }}
          archive: false
          if-no-files-found: error

  release:
    if: startsWith(github.ref, 'refs/tags/v')
    needs: [build, build-linux]
    runs-on: ubuntu-latest
    permissions:
      contents: write
    steps:
      - uses: actions/download-artifact@v8
        with:
          merge-multiple: true
      - uses: softprops/action-gh-release@v3
        with:
          files: "tabxplor_jamovi-*.jmo"
```

The asset names carry the jamovi line and the system but **not** tabxplor's version, so a link of the form `https://github.com/BriceNocenti/tabxplor/releases/latest/download/tabxplor_jamovi-2.7_macos-arm64.jmo` never goes stale — Randomize's trick; the release tag records the version. Seven jobs, four of them on macOS (the Free plan runs five at once). The Linux job follows `zindy/jwellplate`, green on 2026-09-16: jamovi and the freedesktop Sdk installed per user through flatpak, then the same compiler with `--home flatpak`, which runs jamovi's own R through `flatpak run --devel`. jamovi's `main` also has a `--home docker:` route, but not the compiler jmvtools 28.3 ships, and a Docker image is not what Linux students install.

**Known risks**, in the order a first run would show them:

- **The download.** URL scheme and header requirements are observed behaviour, not an API; a changed CDN rule breaks step 2 first. On 2026-09-17 all six files of the matrix answered on both `archives.jamovi.org` and `dl-cdn.jamovi.org` (the former keeps every version). The Windows zip is known to unpack to a top-level `jamovi/` for 2.7.38 (Randomize); for 28.2 that is unverified.
- **`openxlsx2` compiled from source on the 28 line** (section 3.3), through jamovi's R and its `Makeconf` — nobody has shown that path yet. Fallback if it fails: pre-seed the build library, which the compiler skips when a package is already there (`compilerr.js`: `depends.filter(x => installed.indexOf(x) === -1)`) — untar CRAN's current `openxlsx2` binary for R 4.6 into `build/R4.6.0-<arch>-macos/` before the Build step.
- **`macos-latest` moves.** It is macOS 26 today; the build does not depend on the host macOS, but pin `macos-26` if a future image breaks something.
- **Intel ends.** `macos-15-intel` is announced as the last Intel image, until August 2027; after that, Intel Macs need Rosetta on arm64 (untested) or no build.
- **Tags and the site.** The `v*` trigger shares the version tags of the CRAN releases (the planned `v2.0.0` included), which is the point: each package release carries its `.jmo` files. `pkgdown.yaml` also runs on `release: published`, but a release created by this workflow's `GITHUB_TOKEN` starts no other workflow (a GitHub rule), so only a release published by hand redeploys the site.
- **Linux follows Flathub.** Flathub keeps only a handful of commits, so the Linux job always builds against the *current* jamovi, and the Sdk must match its runtime — which is why the step reads the runtime instead of naming 25.08 (the trap `CLAUDE.md` records for the WSL build). The workspace must stay under `$HOME` for the flatpak sandbox to see it; on GitHub's runners it does.
- **The first real test is a student's Mac.** A green run proves the file was written, not that it loads: sideload each Mac file once (Apple silicon and Intel, solid and current) and open a Crosstables analysis on real data, before announcing the link.

### 3.5 rhub is not a separate route

rhub v2 is a GitHub Actions workflow that `rhub::rhub_setup()` adds to *your* repository and `rhub::rhub_check()` triggers; its macOS platforms are the same free runners (`macos-arm64` → `macos-latest`, `macos` → `macos-15-intel`). It runs `R CMD check --as-cran` and uploads a zipped artifact containing the check logs and the built R package, never a `.jmo`. It has a hook for a custom check script, but a plain workflow is simpler. The rhub route and route A are the same machines; only route A builds the right thing.

### 3.6 Route B — other Macs, free or cheap

| Option                          | macOS              | Status on 2026-09-17                                         |
|---------------------------------|--------------------|--------------------------------------------------------------|
| Codemagic (personal account)    | M2, no Intel       | 500 free min/month; any shell script; 60 min per build       |
| CircleCI Free plan              | M4 Pro medium      | ⚠ listed on the Free plan since 2025-11; credit cost unclear |
| MacStadium open-source program  | a donated Mac mini | waitlist only                                                |
| Scaleway Mac mini M4            | arm64              | not free: €0.22/h, 24 h minimum (~€5.30 per use)             |
| AWS EC2 Mac                     | arm64 and Intel    | not free: ~$21 (M2) to ~$26 (Intel) per 24 h minimum         |
| Cirrus CI                       | —                  | ✗ shut down on 2026-06-01                                    |
| Azure Pipelines public projects | —                  | ✗ retired; converted to private in 2027                      |
| GitLab.com macOS runners        | M1 / M2 Pro        | Premium, Ultimate or open-source programme (beta)            |
| A colleague's or student's Mac  | their own          | free; the section 3.4 recipe by hand, no R needed            |

None beats route A: it is the only one that is free, on demand, covers Intel, and hosts the result.

### 3.7 Route C — assembling the Mac build on Linux (experimental)

Because the compiler does nothing a Linux machine cannot imitate, a Mac build could in principle be assembled here: take the Linux build's module directory, keep its nine pure-R packages, replace the seven compiled ones by their macOS binaries from the same Posit snapshot, rewrite the Mach-O load commands the compiler would have rewritten, and stamp `rVersion: 4.6.0-arm64`.

**Measured on the arm64 binaries** of the six compiled dependencies available as binaries (inspected with `lief` on Linux):

| Package                                  | Links outside `/usr/lib`                                    | Signed |
|------------------------------------------|-------------------------------------------------------------|--------|
| fansi, nanonext, parallelly, RhpcBLASctl | nothing: only `/usr/lib/libSystem.B.dylib`                  | ✓      |
| survey                                   | nothing; `/usr/lib/libc++`, left alone by the 28.3 compiler | ✓      |
| VGAM                                     | `libgfortran.5`, `libquadmath.0` from CRAN's `R.framework`  | ✓      |

None of them links `libR.dylib` at all. Two comparisons made on jamovi's own library files support the idea: a pure-R module (medmod) is the same 31 files on macOS arm64, macOS Intel and Linux, differing only in the stamp, the build time and the install path recorded in the lazy-load database; and compiled dependencies are Posit's binaries, byte for byte, apart from the patched load commands. So the Mac-specific surgery comes down to **two load commands in one binary** — but that binary is signed, and on Apple silicon a modified Mach-O must be re-signed (ad hoc) or it will not load. Linux has the tools (`llvm-install-name-tool`; `rcodesign` for ad-hoc signing), and `openxlsx2` would have to come from CRAN's current binary instead of the snapshot's source.

**Why it stays an experiment:** every step imitates, rather than runs, what the compiler does; the framework directory name changes with the line and the architecture; a single wrong byte fails only on a student's Mac; no public project has done it; and it still needs a real Mac for the first validation. With route A proven, it is not worth building.

### 3.8 Getting the right file to each student

The build is the easy half; a student picking the wrong file is the likely failure, because jamovi will not say *wrong system* (section 3.1). What the students need is one page — a README section or a short pkgdown article — that asks two questions and gives one link:

1. **Which jamovi?** The number in jamovi's About box: `2.7.x` is *solid*, `28.x` is *current*. Simplest advice: install the Solid release the download page recommends, so everyone is on the same line as the course.
2. **Which computer?** Windows, Linux, or a Mac — and for a Mac, Apple menu › *About This Mac*: *Chip: Apple M…* is Apple silicon, *Processor: Intel* is Intel (jamovi's download page links the same check, *"Which Mac do I have?"*).

| Computer           | jamovi 2.7.x (solid)                  | jamovi 28.x (current)                |
|--------------------|---------------------------------------|--------------------------------------|
| Windows            | `tabxplor_jamovi-2.7_win-x64.jmo`     | `tabxplor_jamovi-28_win-x64.jmo`     |
| Mac, Apple silicon | `tabxplor_jamovi-2.7_macos-arm64.jmo` | `tabxplor_jamovi-28_macos-arm64.jmo` |
| Mac, Intel         | `tabxplor_jamovi-2.7_macos-x64.jmo`   | `tabxplor_jamovi-28_macos-x64.jmo`   |
| Linux              | —                                     | `tabxplor_jamovi-28_linux-x64.jmo`   |

Each name a `…/releases/latest/download/<name>` link; then, in jamovi, Modules (**+**) › *Sideload* › the file. Two symptoms are worth printing on the same page: *"Needs update"* or *"This module is either missing or incompatible"* means the wrong jamovi line; an R error saying a package or a shared object cannot be loaded most likely means the wrong system.

⚠ Unverified until a first Mac test: whether macOS's quarantine flag on a browser-downloaded `.jmo` reaches the files jamovi unpacks from it. A sideload is not signature-checked by jamovi, so that flag is the only macOS mechanism left that could object.

---

## 4. Part 3 — The jamovi team's two reports, finding by finding

### 4.1 What the reports are

Two files dated 2026-09-16, both signed *Claudia*, which introduces itself as an automated reviewer: an **audit** of the code (11 findings) and a **visual inspection** of the panel and the results (9 findings; the argument-name labels and `Cramer’V` appear in both). They read `master` at 2.0.0; the code they cite is unchanged on `dev`, though some line numbers have drifted (`NAMESPACE`'s two `import()` lines, for one). Their praise is specific and accurate — schema consistency, the `clearWith: []` reasoning, escaping in the JavaScript, the export path's care — and worth keeping in mind: the reviewer read the module closely. But an automated review states things with the same confidence whether it checked them or inferred them, so every finding was checked here before being counted.

### 4.2 Triage

✓ confirmed · ≈ partly right · ✗ wrong. *Course* is the sideloaded build for students; *Library* is what a library submission would need. Audit findings are `A`, visual-inspection findings `V` (V4 is A3).

| #   | Finding                                | Level  | Verified               | Course       | Library        |
|-----|----------------------------------------|--------|------------------------|--------------|----------------|
| A1  | `Suggests:` packages not installed     | HIGH   | ✗ wrong, measured      | —            | —              |
| A2  | Non-syntactic names in survey formulas | HIGH   | ✓ and wider            | fix          | fix            |
| A3  | R argument names in labels (= V4)      | MEDIUM | a design choice        | keep         | drop           |
| A4  | Total labels use `gettext()`           | MEDIUM | ≈ right, too narrow    | fix all text | fix all text   |
| A5  | Crosstables `conf_level` accepts 0, 1  | LOW    | ✓                      | fix          | fix            |
| A6  | `subtext` reaches the results raw      | LOW    | ✓ and scripts run      | fix          | fix            |
| A7  | `html_table` has no `clearWith`        | LOW    | ✓                      | fix          | moot if native |
| A8  | No `refs:` citations                   | LOW    | ✓                      | add          | add            |
| A9  | `import(data.table)`, `import(vctrs)`  | LOW    | ✗ negligible           | —            | optional       |
| A10 | One `TODO`                             | INFO   | ✓                      | —            | —              |
| A11 | Six untranslated French strings        | INFO   | ✗ false positive       | —            | —              |
| V1  | Results are one `Html` element         | HIGH   | ✓ (Part 1)             | keep         | native tables  |
| V2  | The module's own Export block          | HIGH   | ≈ rule real, reason no | keep         | remove         |
| V3  | Five `CustomControl`s                  | MEDIUM | ≈ 3 of 5 have natives  | optional     | replace 3      |
| V5  | Collapse boxes group by mechanism      | MEDIUM | ✓ a convention         | optional     | regroup        |
| V6  | Events JS styles jamovi's internals    | MEDIUM | ✓ fragile              | reduce       | remove         |
| V7  | Spelling; `colour` and `color` mixed   | LOW    | ✓ all found            | fix          | fix            |
| V8  | `digits` restates the global format    | LOW    | ≈ native tables only   | keep         | drop           |
| V9  | Two weighting routes, no notice        | LOW    | ✓                      | add `Notice` | add `Notice`   |

### 4.3 Where the reports are wrong

These deserve a short, factual answer in the reply, because the first one is presented as a blocker.

- **A1 — `Suggests:` packages *are* installed with a module.** The audit says jamovi resolves dependencies from `Imports:` and `Depends:` only, so `openxlsx2`, `marginaleffects`, `VGAM`, `svyVGAM`, `brant` and `mirai` would be missing and Excel export — the panel's default — dead on a library install. Four measurements say otherwise:
  - the compiler's `DESCRIPTION` parser concatenates `Depends`, `Imports`, **`Suggests`** and `LinkingTo`, in the copy jmvtools 28.2 and 28.3 ship and in the copy inside jamovi's own repository (`jamovi-compiler/compilerr.js`, the one beside `docker.js`);
  - tabxplor's own 28.2 build vendors exactly those packages;
  - **jamovi's library build of `vijPlots` 1.3.2** (`library.jamovi.org/linux/R4.6.0-x64`) vendors `vdiffr`, which its `DESCRIPTION` lists only under `Suggests:`;
  - `gridExtra` and `ggplot2` resolve from jamovi's bundled jmv library, which the compiler deliberately does not re-vendor.

  What survives of A1 is minor: `tx_need_pkg()`'s remedy text is R-console advice, unhelpful in jamovi if a package were ever missing. The `if (requireNamespace('jmvcore')) R6::R6Class(…)` guard the audit questions is jmvtools' generated template; jmv's own `logregbin.b.R` has the same line.
- **A11 — no French string is untranslated.** The six quoted `msgstr ""` lines are the first line of a multi-line translation (`msgstr ""` then `"Ajouter deux modèles ou plus…"`); `msgfmt --statistics` reports 284 translated messages and none untranslated, on `master` and on `dev`.
- **A9 — the `import()` cost is negligible.** Loading `data.table` and `vctrs` takes 0.05 s and tabxplor on top 0.13 s (measured twice); both namespaces load whether the directive is `import()` or `importFrom()`, which only changes how many bindings are copied.
- **Overstatements inside V1 and V2**, both already measured in Part 1:
  - *"a plain Copy puts a real grid on the clipboard"* is true of tabxplor's `Html` table too, which is a real `<table>` and pastes as cells (section 2.5); a real `<table>` with `<th>` headers is also what a screen reader reads. What only native tables get is jamovi's number settings, the LaTeX `tabular` and the uniform look.
  - *"jamovi covers it"* is not true of what tabxplor's Export does: jamovi exports results to PDF, HTML and LaTeX, never `.xlsx`, and its Copy drops every colour (section 2.5). The rule behind V2 is real; the stated reason is not.
- **V8 — `digits` is redundant only for native tables.** R sees none of the user's precision settings: `jmvcore::Options` exposes `decSymbol` (the decimal separator), `theme`, `palette` and `ppi`, and the significant figures and p-value digits are applied by jamovi's client, to native table cells only. An `Html` table cannot follow them, so as long as the results are `Html`, a `digits` control is the only precision control the user has.

### 4.4 What the reports found that is real

**A2 — a real bug in the R package, wider than reported.** `svy_design_formula()` and the three calls in `svy_omnibus_one()` (`R/survey-design.R:137`, `:195`, `:196`, `:203`) pass raw names to `stats::reformulate()`, which does not quote them. Measured on R 4.6.1 (the audit used 4.3.2; nothing changed):

- `reformulate("Age group")` and `reformulate("Income (€)")` are parse errors; `reformulate("Age (years)")` silently builds `~Age(years)`, a function call.
- `tab(…, wt = , test = TRUE)` alone is unaffected: the ordinary weighted chi² does not take that path.
- **With `design_effect = TRUE`** — the jamovi checkbox — a row variable named `Marital status` or `Marital (status)` gets a `chi2_design` row whose statistic, degrees of freedom and p-value are all `NA`, with no message; the same table with `marital` gives p = 1.0e-23.
- **`tab_reg()` with a weight variable named `Household weight`** does not degrade silently: it stops with `unexpected symbol`. A jamovi user who imported an SPSS file hits it at the first weighted regression.

The audit's fix is right — backtick the names before `reformulate()` — and so is its test: a fixture with a column called `"Age group"` and a weight called `"Household weight"`. It is a CRAN-relevant bug, not a jamovi one.

**A6 — `subtext` is an injection, and jamovi 28.2 executes it.** Measured with `tab_html()`: a `subtext` of `<script>…</script>` or `<img src=x onerror="…">` reaches the HTML verbatim, and the jamovi backends pass `self$options$subtext` straight to `tab()` (`R/jmvtab.b.R:103`, `R/jmvtabreg.b.R:140`). The audit rates it LOW because the results pane is sandboxed — but section 2.4 measured that 28.2 re-creates inline scripts in an `Html` result, so a shared `.omv` would run its author's JavaScript in the reader's results view. Escape at the jamovi boundary, as the audit suggests, with one tabxplor-specific care: since the footer became a template, `subtext` legitimately contains `<legend>`, `<weight>` and the other `FOOTER_BLOCKS` placeholders, which must survive the escaping while everything else is neutralised.

**A4 — right about four labels, too narrow about the rest.** The audit is correct that `"Total"`, `"Ensemble"` and `"Others"` go through `gettext()` while the module's own messages go through `jmvcore::.()`. But *everything* tabxplor's R core writes into a table — the legend, the test labels, the footer, the regression titles — goes through `gettext()` too, so it follows the R engine's environment, not jamovi's language setting:

- jamovi's server starts the engine with `LC_ALL=en_US.UTF-8` on Linux and never sets `LANGUAGE` (`jamovi/server/engine.py`), so on Linux the table follows the user's own `LANGUAGE` variable, and — by inference, untested — on Windows and macOS the system's language settings;
- jamovi's **results language** — a setting of its own, which defaults to the interface language — is sent by the client with every analysis request as a `.lang` option, and `jmvcore::Options` stores it in a private field that only `.()` reads.

A French jamovi on an English system therefore produces French panel messages around English tables. Switching the four labels to `.()`, as proposed, would produce a French *Total* row inside an English legend. The fix is one hand-off — read the results language once and build under `options(tabxplor.lang)`, which the footer machinery already honours (whether every other generated string does is still to check) — and it needs a supported way to read that language (section 7).

**The small ones, all confirmed:**

- **A5:** `jamovi/jmvtab.a.yaml:429-430` is `min: 0`, `max: 1`, where Regressions has `0.5` and `0.9999999999`.
- **A7:** neither `html_table` declares a `clearWith`, so an export-path edit blanks and repaints the table.
- **A8:** no `refs:` anywhere; `survey`, `MASS`, `nnet`, `marginaleffects` and the named interval methods are all citable.
- **V7:** every misspelling listed is in the files — `nb chars for line break` (four times), `subtables variables`, `what numbers the cell show`, `Models comparison`, `assumption checks plots`, `dependent variable[s]` — plus `Cramer’V` (A3) and a mix of `colour` and `color` in the option descriptions.
- **V9:** `jmv_backend_weights()` silently prefers the `wt` box over jamovi's row weights; a `jmvcore::Notice`, as jmv's Contingency Tables uses, would say so.

### 4.5 What the library requires beyond the tables

These are not defects of the course build: they are jamovi's conventions for a library module, and the price of entry.

- **V2 — the Export block.** The platform rule is stated plainly: *"a module may not implement an action that jamovi already provides"*, and the jamovi Cloud argument is sound — on Cloud the engine writes to a server disk the user cannot reach. But nothing jamovi provides writes a coloured `.xlsx`, and for a course that teaches reading colour-coded tables that export is a feature, not a convenience. So: removed in a library build, kept in the course build, and a feature request for *"export a table to .xlsx with its cell styles"* — which the reviewer explicitly invites.
- **V3 — the `CustomControl`s.** Three of five have native forms:
  - the reference picker is a `ListBox` with a `LevelSelector` column, once `ref` is declared `type: Level` rather than `String` (`dev/jamovi_module.md` §11–12 documents both routes);
  - the model builder is jmv's `Supplier` + `Terms` + `ListBox` arrangement, and moving the option to `Terms` removes the `vars` + `crosses` reassembly;
  - `subtext` could become jamovi's *Add Note*.

  The other two have none: the level merge, reorder and numeric cut (the reviewer acknowledges there is nothing to point to), and `extCtrl`, which leaves with the Export block. ⚠ Changing an option's type changes what a saved `.omv` holds, so the analyses' `version` must move and a migration be written.
- **V4 / A3 — the labels.** Dropping `argument = ` is mechanical, because by design every option is named after its argument and every label follows `argument = <i>(words)</i>`. The same regularity makes the control-to-argument table Damian suggests generable from `jamovi/*.a.yaml`, at no cost, as a page of the pkgdown site. A toggle that shows the names inside the panel is not a real option: `.u.yaml` labels are static, and rewriting them from the events JS is exactly the reaching-into-internals V6 objects to.
- **V5 — collapse boxes by decision** (*Percentages*, *Colours*, *Tests*, *Reference categories*, *Levels*, *Missing values*), with `ModeSelector` where a box holds two exclusive sets of controls.
- **V6 — no CSS on jamovi's classes.** `injectTabxCss()` and friends target `silky-*` classes and jamovi's own misspelling `view-colapsed`; most of it exists to seat the custom controls, so it shrinks with V3, and what remains is `stretchFactor` / `width:` or a layout feature request.
- **V8 — `digits`** goes with native tables (section 4.3).

**One source, two builds — mostly superseded.** The decisions of 2026-09-17 put the default and the opt-ins in one build, and section 5.7 moves the argument names to Syntax mode. The Export block is the one place a library build and the course build could still differ: if jamovi keeps refusing it, the course workflow of section 3.4 can re-add it before `--build`, keeping the source library-clean. That is a design idea, not a measured result.

---

## 5. Part 4 — The native default for September 2027

Decided on 2026-09-17: from the 2027–2028 school year, results default to native `Table` elements dressed in an adapted `print_marks`, with no R argument names in the panel; opt-in options restore today's colour table and argument names, to be negotiated with jamovi. Three requirements come with it — a striking replacement for grey, bold reference rows and columns, and an answer on borders. This part measures what jamovi allows for each and sketches the design. Nothing here is implemented.

### 5.1 What a native cell can say, medium by medium

Everything below was read in jamovi 28.2's shipped client: the results renderer and its stylesheet for the screen, the Copy walker, and the LaTeX exporter (section 1 lists the files).

| Device (how it is set)                        | Screen           | Copy        | LaTeX        | APA 7             |
|-----------------------------------------------|------------------|-------------|--------------|-------------------|
| Value formatted by jamovi (`number` cell)     | ✓                | ✓           | ✓            | ✓                 |
| Value formatted by tabxplor (string cell)     | ✓ right-aligned  | ✓ as text   | ✓            | ✓                 |
| Superscript run after the value (`addSymbol`) | ✓ in the padding | ✓           | `$^{…}$`     | ✓ with a note     |
| Lettered footnote (`addFootnote`)             | ✓ `ᵃ ᵇ ᵈ`        | ✓           | `tablenotes` | ✓ specific note   |
| General note (`setNote`)                      | ✓ *Note.*        | ✓           | ✓            | ✓ general note    |
| Italic (`<em>` in a text cell)                | ✓                | ✓ (`<i>` ✗) | ✓ (`<i>` ✗)  | ✓ with a note     |
| Space above a row (`Cell.BEGIN_GROUP`)        | ✓ 8 px           | ✓ padding   | ✗            | ✓ white space     |
| Indent (`Cell.INDENTED`)                      | ✓ 24 px          | ✓ padding   | ✗            | ✓                 |
| Red text (`Cell.NEGATIVE`)                    | ✓ `#d00`         | ✗           | ✗            | ✗ colour          |
| Bold                                          | ✗ no route       | —           | —            | ✓ with a note     |
| Grey or lighter ink                           | ✗ no route       | —           | —            | ✗ not a device    |
| A border chosen by the module                 | ✗ no route       | —           | —            | ≈ few, horizontal |

Two mechanisms not in that table matter below: a column `format` keyword, whatever it is, becomes the class `jmv-results-table-cell-format-<keyword>` on every cell of the column (`makeFormatClasses()`); and a format bit the client does not know is ignored, while `jmvcore`'s `addFormat()` ORs in any integer.

### 5.2 Grey: no native form, not an APA device — italic takes its place

**What grey does today.** In a graded row, every cell that reaches no rung on either channel is greyed — below the first threshold, or not significant under `grey_non_signif` — while totals and references are exempt, full ink and bold (`fmt_row_look()` and the greying rule in `R/fmt_class.R`). Grey is the receding voice that lets the marked cells stand out.

**Why no workaround.** A native table has no colour field, its text allow-list has no `style`, and its stylesheet is jamovi's. Grey is also not an APA device, the nearest rule being *"avoid shading"*. A workaround would be non-API *and* non-APA — two reasons, not one.

**The permitted replacement is italic.** APA allows it with a general note, and it works today through text cells: `<em>` renders on screen, survives Copy, and becomes `\textit{}` in the LaTeX export. (`<i>` renders too, but Copy drops it and the LaTeX export leaves it untranslated; always `<em>`.) Italic alone is weaker than grey, so the design gives each voice one job and lets none overlap:

- **Upright + marks** — the cells to read, `⁺` to `⁺⁺⁺⁺` or `⁻` to `⁻⁻⁻⁻`.
- **Italic, unmarked** — the cells with nothing to read.
- **Bold** — the reference, and nothing else (section 5.3).

One choice to make: grey merges *not significant* and *significant but below the first threshold*. Italic could keep that merge, or mean *not significant* only, leaving small significant differences upright and unmarked. The second is more informative, but it no longer says exactly what the colour table says.

**Considered and rejected:**

- **Parentheses or brackets.** APA reserves them for other indices (standard deviations, intervals).
- **`<sub>` to shrink the value.** It is allowed, but Copy pastes subscripts into Word, LaTeX sets them in math mode, and screen readers ignore the intent.
- **The red of `Cell.NEGATIVE`.** A colour, one direction only, and lost on Copy.
- **A stylesheet from a companion `Html` result.** It styles jamovi's classes (review finding V6), and the only per-cell class hooks are the ones owned by `negative`, `indented` and group spacing.

**Its cost today:** a text cell holds tabxplor's formatting, not jamovi's significant figures, and pastes as text. That brings review finding V8 back (keep `digits`) and argues for honouring `decSymbol`. The upstream alternative that keeps number cells is an italic format bit (section 5.3). Italic is not read aloud either: the marks, which are characters, carry significance for a screen reader, and the note says what italic means.








### 5.3 Bold for reference rows and columns: no route inside the API — an upstream change is the way

**Measured: there is no way to bold anything in a 28.2 table.**

- Number cells are formatted by jamovi.
- Text cells, column titles, the table title and notes all go through the base allow-list (`em`, `i`, `sub`, `sup`).
- Header and row-header cells are styled `font-weight:400`.
- No format bit means bold.

**But jamovi is one small step away, in three places:**

- the client already defines `richBoldOptions` (`b`, `strong`, `em`, `i`, `sub`, `sup`), used today only by the formula toolbar;
- the LaTeX exporter already maps `<strong>` to `\textbf{}`;
- the Copy walker already keeps `<b>`, `<strong>` and the computed `font-weight`.

**It is also APA.** APA 7 explicitly allows bold explained in the general note: the official factor-analysis sample and the APA Style blog of February 2026 both do it. That makes bold references exactly the *"something genuinely left over"* Damian invited.

**Routes, in order of preference:**

1. **A per-cell format bit, upstream.** For example `Cell.BOLD = 16`, and `Cell.ITALIC = 32` for section 5.2. Number cells keep their type and jamovi's formatting. The change touches `jmvcore/R/cell.R` and the proto comment, the `Format` map and the classes in `client/resultsview/table.ts`, one CSS rule, and the LaTeX row builder. **It degrades gracefully, measured:** the client tests each known bit and ignores the rest, and `addFormat()` accepts any integer, so tabxplor can set the bits now. Older jamovis show no bold, and nothing breaks. The SummaryTables author's copy fix (jamovi issue #1864) was merged the day it was opened, which suggests a well-scoped contribution is welcome.
2. **A column-level keyword, upstream — the cheapest change of all.** Because any `format` keyword already becomes a class, one CSS rule on jamovi's side (`.jmv-results-table-cell-format-bold { font-weight: bold }`, plus the LaTeX mapping) would bold a whole **reference column** with no protocol change. Rows still need route 1.
3. **Bold in text cells, upstream.** Render table text cells with `richBoldOptions` instead of the base list: a one-token change, useful only if the values are text cells (section 5.6).
4. **Inside today's API, weaker but APA-native.** A lettered footnote on the reference column's title and on the reference row's label (`ᵃ Reference`), `Cell.BEGIN_GROUP` space around total rows, and titles that say it (*Total (reference)*). It identifies the reference; it does not make it jump out.
5. **Non-API stopgap, course build only.** A hidden companion `Html` result whose stylesheet bolds a custom column class (`format: pc,txref`). It works on screen and survives Copy, but not LaTeX, and has no per-cell hook for rows. It is exactly what V6 objects to, so it is fit only for the course build, and only if nothing upstream has shipped by September 2027.

**Rejected:** mathematical bold Unicode digits (`𝟏𝟐`). Screen readers spell them letter by letter, they are not numbers once pasted, and the LaTeX export has no mapping for them.

⚠ **Timing.** Students follow jamovi's *solid* line, which trails *current* by months. An upstream bit merged in 2027 may reach solid users only after September 2027, so the September 2027 default must still read correctly without bold: the route-4 identification stays in, even once the bit exists.

### 5.4 Borders: not customisable, and mostly not needed

**Measured, 28.2 stylesheet.** The table draws:

- a 1 px rule under the title;
- a 1 px rule under every header cell, `superTitle` spans included (empty spans excepted);
- a 2 px rule under the last body row;

and nothing else. The LaTeX export draws the booktabs equivalent: `\toprule`, a `\midrule` after the header, a `\cmidrule` under each spanning title, `\bottomrule`. No field in the protocol sets a border. `Cell.BEGIN_GROUP` adds 8 px of space and no line, and `Cell.END_GROUP` — documented in `jmvcore` as spacing below — is rendered neither by 28.2 nor by jamovi's `main`.

**Against what tabxplor draws today:**

| tabxplor's html today                   | Native equivalent                     | APA 7                       |
|-----------------------------------------|---------------------------------------|-----------------------------|
| vertical rules between `col_var` blocks | `superTitle` per block, spacer column | ✓ white space instead       |
| rule above totals, between sub-tables   | `Cell.BEGIN_GROUP` space              | ≈ rule above totals allowed |
| 2 px rule above a regression's fit rows | a second table (jmv's *Model Fit*)    | ✓                           |
| rules under the header bands            | built in                              | ✓                           |

**Verdict: borders cannot be set "the tabxplor way" in a native table, and APA would not want most of them.** White space and separate tables are APA's own substitutes. The one rule worth asking jamovi for is a thin line above a totals row, which APA explicitly allows (*"You may also use a border to separate a row containing totals"*). It could travel in the same contribution as the format bits.

### 5.5 The adapted `print_marks`, sketched

From the 8-slot grid of `PRINT_PALETTES$print_marks` (`R/tab-palettes.R`) to native devices:

| Cell                             | `print_marks` in html today      | Native default                        |
|----------------------------------|----------------------------------|---------------------------------------|
| over-represented, rungs 1–2      | `⁺`, `⁺⁺`                        | the same, through `addSymbol()`       |
| over-represented, rungs 3–4      | `⁺⁺⁺`, `⁺⁺⁺⁺`, underlined        | the same marks; underline dropped     |
| under-represented, rungs 1–4     | `⁻` to `⁻⁻⁻⁻`, underlined from 3 | the same marks; underline dropped     |
| no rung, in a graded row         | grey ink                         | italic (`<em>`, or the upstream bit)  |
| reference and total cells        | bold, full ink                   | bold (upstream), plus the note letter |
| summary rows (`n`, `pct`, tests) | table ink, never bold            | plain                                 |
| significance stars               | replaced by the marks            | replaced by the marks                 |

The legend becomes one `setNote()` general note, rendered with jamovi's italic *Note.*: *"⁺ to ⁺⁺⁺⁺, over-represented …; ⁻ to ⁻⁻⁻⁻, under-represented …; italics, no difference to read; bold, the reference."* Its words come from the existing legend builder in its prose register, `FOOTER_BLOCKS` deciding which lines exist, exactly as for the other media. The underline goes because it is neither native nor APA; the mark count already carries the rung.

⚠ **To measure on a real table:** a `symbols` run is absolutely positioned inside the cell's right padding (20 px on a number column), so four marks may overflow into the neighbouring cell.

### 5.6 Text cells or number cells: a decision for spring 2027

Route A writes every value as a text cell formatted by tabxplor; route B keeps number cells formatted by jamovi and relies on the upstream bits (the route numbers in the table are those of section 5.3).

| Route                      | Italic           | Bold                 | Number format                 |
|----------------------------|------------------|----------------------|-------------------------------|
| **A**: text cells          | ✓ today (`<em>`) | route 3 or stopgap 5 | tabxplor's, `decSymbol`       |
| **B**: number cells + bits | route 1          | route 1              | jamovi's (as the review asks) |

B is the cleaner library module; A works on today's jamovi and on the solid line. Since the bits degrade gracefully, the backend can be written for B with A behind a switch, and the gate is jamovi's answer to the contribution. ⚠ A table must not mix the two in one column: number cells would follow jamovi's significant figures and text cells tabxplor's, so the digits would disagree within the column.

### 5.7 The opt-ins that restore today's behaviour

- **The colour table**: today's `Html` result behind a *Table style* option (section 2.10), the view to negotiate.
- **The argument names — already native, in jamovi's Syntax mode.** Measured in `jmvcore` 2.7.38: `Analysis$asProtoBuf()` prepends a `syntax` element built from `asSource()`, which prints `tabxplor::jmvtab(data = data, …)` with every non-default option as `name = value`. tabxplor's options are named after `tab()`'s arguments, so a student who switches on Syntax mode in jamovi's preferences already sees `row_vars = …`, `pct = "row"` above the table — copyable, per user, with no label touched. `asSource()` is a public method of the analysis class, so overriding it to print a runnable `tabxplor::tab(…)` call would make Syntax mode teach the exact R function (to confirm with jamovi, section 7). **This is the natural home for "teaching R from the buttons."**
- **Argument names inside the panel**, as an option, has no clean route: `.u.yaml` labels are static, and the alternatives are duplicated controls or events JS rewriting jamovi's DOM (V6). If it is still wanted after trying Syntax mode, it is a question for jamovi, not a design.

### 5.8 What to take upstream, and when

In order of value to the September 2027 default:

1. **Format bits `BOLD` and `ITALIC`**, and a column-level `bold` keyword (sections 5.2–5.3).
2. **A rule above a totals row** (section 5.4).
3. **A supported accessor for the results language** from R (section 4.4, A4).
4. **A styled `.xlsx` export** (section 4.5, V2).
5. **A control to merge, reorder and cut levels** (section 4.5, V3).

Issues this autumn, and pull requests for items 1–2 early in 2027, so that the change can reach a release — and, ideally, the solid line — before September.

---

## 6. Recommendation

A timeline, from the decisions of 2026-09-17.

**Autumn 2026 — the package, and the builds.**

1. **The two real bugs** (section 4.4): backtick the names in `svy_design_formula()` and the three `reformulate()` calls of `svy_omnibus_one()`, check the weight path of `tab_reg()`, and test with a column `"Age group"` and a weight `"Household weight"` under `design_effect = TRUE`; escape the jamovi user's `subtext` while keeping the `FOOTER_BLOCKS` placeholders. The first gives R users a silently empty test too: before the CRAN submission if still pending, else 2.0.1.
2. **The CI workflow of section 3.4 — all platforms**, on `master` (`.github/` already ships there): macOS arm64 and Intel, Windows, both jamovi lines, and Linux x64 through flatpak. Run it by hand against `dev` once.
3. **Test each file by sideloading it** — a Mac with Apple silicon at least, both lines ideally, and Windows once, since the workflow builds 2.7 with the 28.3 compiler rather than the Windows checkout's pinned jmvtools 2.7.26.
4. **Tag, let the release job attach the seven files**, and publish the one-page install guide of section 3.8. The local Windows and WSL builds stay as development tools, no longer as the release path.
5. **The small corrections** (section 4.4):
   - `conf_level` bounds;
   - `clearWith` on both `html_table` elements;
   - the spelling pass;
   - the weights `Notice`;
   - `jamovi/00refs.yaml`;
   - `<em>` instead of `<i>` for the semantic italic;
   - honouring `decSymbol`.

**Winter 2026–2027 — the conversation and the contributions.**

6. **Reply to Damian** with facts and questions (section 7):
   - accept what the reports got right;
   - correct what they got wrong, with the evidence (`Suggests:`, the French catalogue, Copy);
   - propose the native default of Part 4;
   - ask for the colour view as an opt-in.
7. **Open the upstream issues of section 5.8** and prepare the format-bit pull request.
8. **Decide the APA number conventions** for the publication palettes — stars at `.05/.01/.001`, `p < .001`, no leading zero on bounded statistics, a comma in a CI — since the native default will print them.

**Spring 2027 — prototypes.**

9. **A native backend for Regressions first**, where the loss is smallest (section 2.9), then Crosstables with the adapted `print_marks` of section 5.5. Settle the route A/B question of section 5.6 on jamovi's answer.
10. **The panel of section 4.5**:
    - the three native controls, with an analysis `version` bump and a migration for the `Level` and `Terms` option types;
    - collapse boxes by decision;
    - no CSS on jamovi's classes;
    - labels without argument names;
    - a generated control-to-argument page;
    - the `asSource()` override for Syntax mode, if jamovi agrees.

**Summer 2027 — the default switches**, tested on the jamovi line students will actually install in September; the Export block stays in the course build and leaves the library one (section 4.5).

**Do not** smuggle HTML into native cells (closed in 28.2, and against the platform's direction), style jamovi's classes in the library build, or build the Mac-less assembly of section 3.7.

---

## 7. Questions worth asking the jamovi team

In the submission issue or the reply, and roughly in this order:

1. **The plan.** From September 2027, native APA tables by default (section 5.5), no argument names in the panel, and today's colour table as an opt-in view labelled for exploration. Is that acceptable for the *curated* tier? If not, is the *community/experimental* tier described in `jamovi-module-submissions/docs/tiers.md` open to the module as it is today, and does jamovi build that tier for every platform?
2. **Bold and italic cells.** APA 7 allows both in a table when the general note explains them, and jmv already highlights residuals with `Cell.NEGATIVE`. Would a contribution adding `BOLD` and `ITALIC` cell format bits, and a column-level `bold` format keyword, be welcome? The client already has `richBoldOptions`, the LaTeX export already maps `<strong>`, and Copy already keeps the weight. A reference row that is not bold is the one loss tabxplor cannot accept.
3. **A rule above a totals row**, which APA explicitly allows: same contribution, or a separate one?
4. **Stability.** Is `Cell.NEGATIVE` stable API — it is documented as *"colours the value red"*?
5. **Syntax mode.** Is overriding `asSource()` in a module's analysis class acceptable, so that Syntax mode prints a runnable `tabxplor::tab(…)` call rather than `tabxplor::jmvtab(…)`? And is a module-level option that shows argument names *inside* the panel conceivable at all, or is Syntax mode the intended place?
6. **The results language.** Is there a supported way for a module's R code to read the results language that `.()` uses (the `.lang` option `jmvcore::Options` keeps private), so text produced by the package's own `gettext()` can follow it?
7. **Excel.** jamovi's export has no `.xlsx` and its Copy drops colour. Would a styled-`.xlsx` export be welcome as a feature request — and in the meantime, is a module-level export acceptable in a desktop-only build?
8. **Levels.** Merging, reordering and cutting levels has no native control. Is that a feature request jamovi would consider, or is a `CustomControl` the accepted answer for it?
9. **In-app help.** Is there a timeline, and a format module authors can target, for the argument mapping Damian suggested moving out of the panel?

---

## 8. Sources

Read on 2026-09-17 unless stated. Local measurements are described where they are used.

### jamovi — platform and API

- The jamovi team's reports (2026-09-16): `dev/jamovi/2026-09-16  jamovi team tabxplor.md`, `dev/jamovi/2026-09-16 jamovi team tabxplor visual inspection.md`
- Compiler installing `Suggests:`, jamovi's own copy: <https://github.com/jamovi/jamovi/blob/c1e67b231d1ffda46b13b4694a55ffac46cf6e8d/jamovi-compiler/compilerr.js> · library build vendoring a `Suggests:`-only package: <https://library.jamovi.org/linux/R4.6.0-x64/vijPlots-1.3.2.jmo> (its `DESCRIPTION`: <https://github.com/vjalby/vijPlots/blob/main/DESCRIPTION>)
- Engine environment (`LC_ALL`, no `LANGUAGE`): `jamovi/server/engine.py` in the 28.2 flatpak; the `.lang` option sent by the client: `main-Cq0DEaqo.js`; read by `jmvcore::Options$fromProtoBuf()`
- Native patterns the reports point to: jmv's `linreg.u.yaml` (reference levels, model builder) <https://github.com/jamovi/jmv/blob/master/jamovi/linreg.u.yaml>, the weights `Notice` in <https://github.com/jamovi/jmv/blob/master/R/conttables.b.R>, collapse-box layout in <https://github.com/jamovi/jmvplots/blob/master/jamovi/scat.u.yaml>
- Table API: <https://dev.jamovi.org/api/table> · Html element: <https://dev.jamovi.org/api/html> · Text element: <https://dev.jamovi.org/api/text>
- Rich results tutorial ("APA-formatted" tables): <https://dev.jamovi.org/tutorial/tuts0106-creating-rich-results>
- Distributing modules (platform-specific builds, the eight configurations): <https://dev.jamovi.org/tutorial/tuts0111-distributing-modules>
- Submission process and tiers: <https://github.com/jamovi/jamovi-module-submissions/blob/main/docs/process.md>, <https://github.com/jamovi/jamovi-module-submissions/blob/main/docs/tiers.md>
- Sideload checks — `rVersion` equality, library-only signatures: <https://github.com/jamovi/jamovi/blob/c1e67b231d1ffda46b13b4694a55ffac46cf6e8d/server/jamovi/server/modules/modules.py>, <https://github.com/jamovi/jamovi/blob/c1e67b231d1ffda46b13b4694a55ffac46cf6e8d/server/jamovi/server/utils/zipverify.py>
- Copy keeps `<br>` and `font-style` (merged 2026-09-17): <https://github.com/jamovi/jamovi/issues/1864>, commit `c1e67b23`
- `Cell.NEGATIVE` highlighting in jmv Contingency Tables: <https://github.com/jamovi/jmv/commit/5d0c208ba5f11b100baabc137f3920fbe8dc7165>
- Forum, colour coding in table cells (November 2025): <https://forum.jamovi.org/viewtopic.php?t=4090>
- Forum, a module built for another version or OS: <https://forum.jamovi.org/viewtopic.php?t=4095>
- In-context help, still open: <https://github.com/jamovi/jamovi/issues/642>
- Release channels: <https://www.jamovi.org/versions.json> · download page (needs a `Referer`): <https://www.jamovi.org/download.html> · release notes: <https://www.jamovi.org/releases.html>
- Library index layout: <https://library.jamovi.org/macos/R4.6.0-arm64/index>

### jamovi modules used as precedents

- SummaryTables (all tables `Html`): <https://github.com/NourEdinDarwish/SummaryTables> · blog post: <https://blog.jamovi.org/2026/07/09/summarytables.html>
- ClinicoPathDescriptives (cross tables as `Html`): <https://github.com/sbalci/ClinicoPathDescriptives>
- ChiSquaredTools (native residual highlighting): <https://github.com/gianmarcoalberti/ChiSquaredTools>
- Randomize (macOS arm64 and Intel builds, Release assets): <https://github.com/byurk/Randomize/blob/main/.github/workflows/build-modules.yml>, <https://github.com/byurk/Randomize/releases/tag/v0.2.0>
- miso (the same, with SHA-256 pins): <https://github.com/usyd-soles-edu/miso/blob/main/.github/workflows/build-jmo.yml>
- pandion (both jamovi lines): <https://github.com/torryscott/pandion/blob/main/.github/workflows/build-jmo.yml>, run <https://github.com/torryscott/pandion/actions/runs/34806339324>
- jwellplate (Linux build through flatpak in CI): <https://github.com/zindy/jwellplate/blob/main/.github/workflows/build.yml>

### APA 7

- Table setup (borders, number, title, headings): <https://apastyle.apa.org/style-grammar-guidelines/tables-figures/tables>
- Sample tables (bold loadings, probability notes, CIs in brackets): <https://apastyle.apa.org/style-grammar-guidelines/tables-figures/sample-tables>
- APA Style blog, highlighting results in tables (February 2026): <https://apastyle.apa.org/blog/methods-tables-figures>
- Colour, for figures: <https://apastyle.apa.org/style-grammar-guidelines/tables-figures/colors>
- Numbers and Statistics Guide (leading zeros, decimals): <https://apastyle.apa.org/instructional-aids/numbers-statistics-guide.pdf>
- ⚠ Secondary, reproducing Manual §7.14 and §7.17 (shading, bold and italic with a general note, specific notes): <https://tcsedsystem.libguides.com/APA7/Tables>

### Build infrastructure

- GitHub-hosted runners: <https://docs.github.com/en/actions/reference/runners/github-hosted-runners> · larger runners, always billed: <https://docs.github.com/en/billing/concepts/product-billing/github-actions> · limits: <https://docs.github.com/en/actions/reference/limits>
- macOS 26 GA: <https://github.blog/changelog/2026-02-26-macos-26-is-now-generally-available-for-github-hosted-runners/> · `macos-latest` migration: <https://github.blog/changelog/2026-05-14-github-actions-upcoming-image-migrations/>
- macOS 14 retirement: <https://github.com/actions/runner-images/issues/13518> · macOS 13 closing down: <https://github.blog/changelog/2025-09-19-github-actions-macos-13-runner-image-is-closing-down/> · `macos-15-intel`, last Intel image: <https://github.com/actions/runner-images/issues/13045>
- Non-zipped artifacts: <https://github.blog/changelog/2026-02-26-github-actions-now-supports-uploading-and-downloading-non-zipped-artifacts/> · releases: <https://docs.github.com/en/repositories/releasing-projects-on-github/about-releases>, <https://docs.github.com/en/repositories/releasing-projects-on-github/linking-to-releases> · manual runs: <https://docs.github.com/en/actions/how-tos/manage-workflow-runs/manually-run-a-workflow>
- rhub v2: <https://blog.r-hub.io/2024/04/11/rhub2/> · its platforms: <https://github.com/r-hub/actions/blob/v1/setup/platforms.json> · its check action: <https://github.com/r-hub/actions/blob/main/run-check/action.yaml>
- Cirrus CI shutdown: <https://cirruslabs.org/> · Codemagic pricing: <https://docs.codemagic.io/billing/pricing/> · Azure Pipelines public projects: <https://learn.microsoft.com/en-us/azure/devops/pipelines/licensing/concurrent-jobs?view=azure-devops> · CircleCI free macOS: <https://circleci.com/changelog/macos-m4-resource-class-now-available-for-free-customers/> · GitLab macOS runners: <https://docs.gitlab.com/ci/runners/hosted_runners/macos/> · MacStadium: <https://macstadium.com/company/opensource> · Scaleway: <https://www.scaleway.com/en/mac-mini-m4/> · AWS EC2 Mac: <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-mac-instances.html>
- CRAN macOS binaries: <https://mac.r-project.org/> · Posit Package Manager binaries: <https://docs.posit.co/rspm/admin/serving-binaries.html>
- Mach-O tools on Linux: <https://llvm.org/docs/CommandGuide/llvm-install-name-tool.html>, <https://gregoryszorc.com/docs/apple-codesign/stable/apple_codesign_rcodesign_signing.html>
- jmvtools source tarball with the compiler: <https://repo.jamovi.org/src/contrib/jmvtools_28.3.tar.gz> (SHA-256 `c34ec517…f730`, matching miso's pin)
