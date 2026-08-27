# A unified theme for pkgdown, Quarto and the editor

Design proposal. Every claim marked ✓ was verified on this machine; how, and with what versions, is in Appendix A.

## 1. What this is for

Four things display the same author's work, and today each is themed in a different language:

- the **tabxplor pkgdown site** — bslib variables, pkgdown highlight styles, `tab_css()`;
- the **statistics courses** (`~/github/formations_stat`) — `webexercises::webexercises_default2()`, i.e. `bookdown::html_document2` plus a hand-written 769-line `style.css` and a pandoc `highlight:` name;
- the **bookdown books** (`~/github/formations_stat/books/…`) — the same again, per book;
- **Positron**, where the author writes and where the students read code — the *Starless Monokai Atom* theme.

The friction is not that any one of them is hard. It is that **a colour decision has to be re-made in four places**, in four notations, and that a new output format means starting again. The goal of this document is a framework where a colour is decided **once**, and every consumer is three to six lines of YAML.

Two non-goals, stated up front so the design stays small:

- **not a design system.** No components, no utility classes, no grid. A palette, a code theme, and a handful of prose rules.
- **not a tabxplor feature.** Table colours already have a single source of truth (`tab_css()`); see §9.

## 2. What each consumer can actually read

The design is entirely determined by this table, so it is the first thing to check when a version changes.

| consumer                  | page chrome                            | code colours                                 |
|---------------------------|----------------------------------------|----------------------------------------------|
| pkgdown 2.2               | bslib variables, **or `_brand.yml`** ✓ | `theme:` / `theme-dark:`, 35 bundled `.scss` |
| Quarto 1.9                | **`brand:`, light *and* dark** ✓       | `syntax-highlighting: {light, dark}` ✓       |
| bookdown `html_document2` | `theme:` (bootswatch) or raw `css:`    | pandoc `highlight:` — **a name only**        |
| Positron / VS Code        | theme JSON                             | the same JSON                                |
| tabxplor tables           | —                                      | —                                            |

| consumer                  | extra CSS                                     | light/dark                                  |
|---------------------------|-----------------------------------------------|---------------------------------------------|
| pkgdown 2.2               | `pkgdown/extra.scss`, `template: package:` ✓  | `light-switch: true` → `data-bs-theme`      |
| Quarto 1.9                | `theme: [brand, extra.scss]`, or an extension | built in → `quarto-light` / `quarto-dark` ✓ |
| bookdown `html_document2` | `css:`                                        | **none**                                    |
| Positron / VS Code        | —                                             | editor setting                              |
| tabxplor tables           | `tab_css()`                                   | **already follows both** ✓                  |

Four consequences drive everything below:

1. **`_brand.yml` is the only palette format two of the four read natively.** pkgdown has no brand code of its own, but `template: bslib:` is splatted straight into `bslib::bs_theme()`, which takes `brand =` — so pkgdown reads a brand file today, undocumented but by construction ✓.
2. **The R side of brand.yml is single-mode.** `brand.yml::read_brand_yml()` rejects `color.background: {light:…, dark:…}` ✓ — that shape is a Quarto extension of the spec. So the dark half of the palette reaches pkgdown as `*-dark` bslib variables, generated, never hand-copied.
3. **pkgdown's `.scss` styles and pandoc's `.theme` files are the same vocabulary.** Both enumerate skylighting's 31 token types — pkgdown as two-letter classes (`co`, `st`, `fu`, …), pandoc as names (`Comment`, `String`, `Function`, …) ✓. One palette therefore generates both, mechanically and without judgement.
4. **bookdown has no dark mode and no custom code theme.** That is the reason the courses move to Quarto, not merely a preference. Until they do, they get the light half only (§8).

## 3. The four layers, and which of them need single-sourcing

| layer          | what it decides                     | single source                    | consumed as            |
|----------------|-------------------------------------|----------------------------------|------------------------|
| **A. palette** | background, text, links, borders    | `_brand.yml` + `_brand-dark.yml` | brand; `*-dark` vars   |
| **B. code**    | the 31 syntax tokens, both modes    | one token table                  | `.theme`; `.scss`      |
| **C. prose**   | bold, quotes, inline code, callouts | one `.scss` partial              | `theme:`; `extra.scss` |
| **D. tables**  | tabxplor cells                      | `tab_css()`, inside tabxplor     | automatic in both ✓    |

Layer D is the proof that this works: `tab_css(theme = "auto")` writes its cascade against `[data-bs-theme=dark]` **and** `body.quarto-dark`, so a tabxplor table already follows the reader on a pkgdown site and in a Quarto document, with no theme code of ours involved ✓. The framework's job is to make layers A–C behave the same way.

## 4. Architecture

```text
   Positron theme JSON            overrides.yml
   (izumii.starless-monokai)      (the deliberate departures:
        vendored copy              warm base text, gold bold,
        the eye's anchor           quote grey, code background)
             │                            │
             └──────────────┬─────────────┘
                            ▼
                   R/build_theme.R          ← the ONE generator
                            │
      ┌─────────────────────┼──────────────────────┬───────────────────┐
      ▼                     ▼                      ▼                   ▼
 _brand.yml           code-light.theme       code-light.scss      prose.scss
 _brand-dark.yml      code-dark.theme        code-dark.scss       (hand-written,
 (layer A)            (layer B, Quarto)      (layer B, pkgdown)    layer C)
      │                     │                      │                   │
      ├─────────────────────┴──────────────────────┼───────────────────┤
      ▼                                            ▼                   ▼
   QUARTO                                       PKGDOWN            both, verbatim
   brand: {light, dark}                  template: package: <pkg>
   syntax-highlighting: {light, dark}    theme:/theme-dark: + extra.scss
```

Read it as one rule: **nothing downstream is edited by hand.** A colour changes in the editor theme or in `overrides.yml`; everything else is regenerated. The only hand-written artefact is `prose.scss`, which is CSS no palette can express (see §7 for the one judgement call the generator makes).

## 5. Where it lives

**Recommendation: a new small repository that is both an R package and a Quarto extension.** Working name `txtheme`; the name matters only in that it will appear in every YAML file for years.

The repository is one thing with two faces, because the two mechanisms that make this frictionless are already provided by the two toolchains:

- **pkgdown reads themes from an R package.** `pkgdown:::bs_theme_rules()` looks for `inst/pkgdown/BS5/extra.scss` in the package named by `template: package:` ✓, so a site written by this author needs one line and no local CSS.
- **Quarto installs extensions from a GitHub repository.** `quarto add BriceNocenti/txtheme` copies `_extensions/txtheme/` into the project, and a custom format can carry `theme`, `syntax-highlighting`, `css`, includes and filters. A document then says `format: txtheme-html` and nothing else.

A single repository can serve both: the R package at the root, `_extensions/` beside it and `.Rbuildignore`d.

- **A new `txtheme` repository — recommended.** Independent lifecycle, serves every project, no CRAN constraints, and both install mechanisms are available to it.
- **Inside tabxplor — no.** tabxplor is a CRAN package about cross-tables. Personal branding is out of its scope, it would add a `brand.yml` dependency to the site build, and it would tie a theme's release cycle to a statistics package's.
- **Inside the webexercises fork — no.** That fork tracks an upstream repository, and the more it carries that upstream does not, the harder every rebase gets. Its scope is exercise widgets: it should keep `webex.css` / `webex.js` and nothing else.
- **No package, files copied per project — no.** That is exactly today's 769-line `style.css`, four times over.

## 6. What you write, per project

The whole point of the framework is this section being short.

**A pkgdown site** — `_pkgdown.yml`:

```yaml
template:
  bootstrap: 5
  light-switch: true
  package: txtheme          # prose.scss + the code themes, both modes
  bslib:
    brand: _brand.yml       # or the copy shipped in the package
```

**A Quarto document** — the `.qmd` front matter:

```yaml
format: txtheme-html
```

**A Quarto project or book** — `_quarto.yml`:

```yaml
format:
  txtheme-html: default
brand:
  light: _brand.yml
  dark: _brand-dark.yml
```

⚠ `brand:` must sit at **document or project level**, never inside `format: html:`. Nested there it is silently ignored: the render succeeds, the light and dark stylesheets come out byte-identical, and none of the brand colours appear ✓ — an hour lost to a misplaced two-space indent.

**A course with exercises** — the same, plus what `webexercises` needs:

```yaml
format:
  txtheme-html:
    include-after-body: include/webex.js
    css: include/webex.css
```

## 7. The generator

One R file in the package, `build_theme.R`, run by hand when the palette changes — not at render time. Its inputs and outputs:

| input                                    | why it is an input                                             |
|------------------------------------------|----------------------------------------------------------------|
| `inst/source/starless-monokai-atom.json` | a vendored copy of the Positron theme — the daily palette      |
| `inst/source/overrides.yml`              | the deliberate departures from it, each with a one-line reason |

| output                                      | consumer                                                       |
|---------------------------------------------|----------------------------------------------------------------|
| `inst/brand/_brand.yml`, `_brand-dark.yml`  | Quarto (both), pkgdown (light half)                            |
| `inst/pkgdown/BS5/extra.scss`               | pkgdown: dark bslib variables + dark code theme + `prose.scss` |
| `_extensions/txtheme/starless-html/*.theme` | Quarto's `syntax-highlighting`                                 |
| `inst/highlight/code-{light,dark}.scss`     | a pkgdown site naming the style instead of taking the package  |

**The token mapping is the generator's whole substance.** A VS Code theme is a list of TextMate scopes; skylighting has 31 token types; the mapping between them is one table, written once:

| skylighting                                                  | pkgdown class            | TextMate scope                                    |
|--------------------------------------------------------------|--------------------------|---------------------------------------------------|
| Normal                                                       | (`pre code`)             | `source` — **overridden to `#CECDC3`**, see below |
| Comment, Annotation, Documentation, CommentVar, RegionMarker | `co` `an` `do` `cv` `re` | `comment`                                         |
| String, Char, VerbatimString, SpecialString                  | `st` `ch` `vs` `ss`      | `string`                                          |
| Keyword, ControlFlow, Import, Preprocessor                   | `kw` `cf` `im` `pp`      | `keyword`                                         |
| Function, BuiltIn, Extension, Others                         | `fu` `bu` `ex` `ot`      | `entity.name.function`, `support.function`        |
| Constant, DecVal, BaseN, Float, SpecialChar                  | `cn` `dv` `bn` `fl` `sc` | `constant.numeric`, `constant.language`           |
| DataType, Attribute                                          | `dt` `at`                | `entity.name.type`, `entity.other.attribute-name` |
| Variable                                                     | `va`                     | `variable`                                        |
| Operator                                                     | `op`                     | **see the judgement call below**                  |
| Alert, Error, Warning                                        | `al` `er` `wa`           | `invalid`, `keyword`                              |
| Information                                                  | `in`                     | `constant.other.placeholder`                      |

⚠ **One judgement call, and it must be made in the generator, not per project.** downlit tags `(`, `,`, `$` **and** `<-` all as `.op` — 317 of them on one vignette page, the commonest class by far. In the editor the brackets are punctuation grey `#939293` and only `<-` is pink. Pink for all of them makes every bracket shout, so `.op` takes the punctuation grey. It is the one place where the port is *deliberately* not the editor.

The other deliberate departures, all from the author's own `settings.json`, belong in `overrides.yml`:

| override        | value                            | why                                                                        |
|-----------------|----------------------------------|----------------------------------------------------------------------------|
| base text       | `#CECDC3` instead of `#fcfcfa`   | a near-white makes the text louder than the colours it sits among          |
| bold            | `#e6ae02`                        | markdown bold should read as emphasis, not as more text                    |
| block quote     | `#B7B5AC` italic, rule `#e6ae02` | quotes recede, their rule ties to bold                                     |
| inline code     | `#fc9867`                        | the theme's own `markup.inline.raw` colour                                 |
| code background | `#1f1f1f`                        | VS Code's own dark ground, which is what "starless" inherits in the editor |

## 8. Migration, in order

Each step is independently useful and independently revertible; nothing later is a prerequisite for anything earlier.

1. **Fix the tabxplor site by hand** (Appendix B). No framework, no new repository — the YAML already drafted, corrected. Do this first: it is the visual target everything else copies.
2. **Create the repository, palette and generator.** Port the corrected tabxplor site YAML into `_brand.yml` / `overrides.yml`, regenerate, and check that the generated site matches the hand-made one. That comparison is the framework's acceptance test.
3. **Point tabxplor's site at the package** — `template: package: txtheme`. `_pkgdown.yml` loses its `bslib:` block.
4. **Pilot one course in Quarto**, the shortest one. Two things must survive the move: `webexercises` (its `.qmd` path already exists — `webexercises::add_to_quarto()`, and §9.1 is what makes it follow the switch) and the tabxplor tables (nothing to do — layer D ✓). What of `style.css` is worth keeping is already inventoried in Appendix C: of its 769 lines, only about 80 are live CSS, and a third of those are table rules `tab_css()` has since absorbed.
5. **The books.** A Quarto book is the same format plus `_quarto.yml`; the framework does not distinguish them.
6. **Retire `style.css`** and the per-project `highlight:` lines.

Until step 4 lands, the bookdown courses keep working unchanged and can borrow the light half only: `css: <path to>/prose.css` plus `highlight: arrow` — no dark mode, which is what bookdown offers.

## 9. What deliberately stays outside

- **tabxplor's table colours.** `tab_css()` is the single source for anything inside a `.tabxplor-tab`, in every medium, and it already follows both toolchains' dark hooks ✓. **Rule: the theme package never writes a selector containing `.tabxplor-tab`.** If a table looks wrong on a themed page, the fix belongs in tabxplor.
- **`webexercises`.** Exercise widgets, their CSS and their JS. The fork stays a thin fork — with the one exception §9.1 defines.
- **Fonts.** `style.css` currently pulls DejaVu from `raw.githubusercontent.com` at page load — a third party in the render path of every course page. brand.yml's `typography.fonts` takes Google or Bunny fonts, or files bundled in the package; either is better, and the choice is worth making explicitly rather than inheriting.

### 9.1 webexercises, on a page with a switch

A course page carries the switch, so the exercise widgets have to follow it — and they are the one
part of the stack that was written for a light page only. `webex.css` (217 lines) is in good shape
for this: **seven of its colours are already `:root` variables**, which the theme can redefine under
the dark hooks without touching the file at all.

| variable | light (upstream) | what it colours |
|---|---|---|
| `--correct` / `--incorrect` | `#59935B` / `#983E82` | the border of an answered field |
| `--correct_alpha` / `--incorrect_alpha` | `#c0edc2` / `#edaddd` | its fill — **pastels, made for white** |
| `--correct_text` / `--incorrect_text` | `#00D26A` / `#c60800` | the score line under a check button |
| `--highlight` | `#467AAC` | the check button, and the `.exercise` underline |

⚠ **`--highlight` is declared in `webex.css`, and the courses use it for their own `.exercise` class.**
A page with the theme but without webexercises therefore loses that underline's colour silently — it
falls back to `currentColor`. The theme must declare `--highlight` itself, from the brand's `primary`.

**Four values are literals, and no redefinition can reach them.** They are what actually breaks on a
dark page:

| `webex.css` | literal | breaks as |
|---|---|---|
| `.unchecked .webex-incorrect` / `-correct` | `background-color: white !important` | a white field on a dark page |
| `.webex-select, input.webex-solveme, .unchecked … label` | `background-color: white` | the same, for text inputs |
| `.webex-incorrect, … label.webex-incorrect` | `color: black` | black on a dark-mode fill |
| `.webex-correct, … label.webex-correct` | `color: black` | the same |

**The recommended fix is four one-word edits in the fork, not four override rules in the theme.**
Each literal becomes a variable *with the literal as its fallback* —
`background-color: var(--webex-field-bg, white)`, `color: var(--webex-answer-fg, black)` — so the
file behaves **identically** where no theme is loaded, the theme needs no `!important` war against an
`!important` rule, and the change is small enough to offer upstream. That refines §9's rule rather
than breaking it: **the fork may turn a literal into a variable that keeps the literal as its
fallback; it never gains a colour of its own.**

The dark values themselves are a design decision, not a conversion: the pastels cannot simply be
darkened, because a fill on a dark page reads as a **tint of the page**, not as a colour of its own.
The shape that works is the same one the inline-code pill uses — the semantic hue at ~20% alpha over
whatever the page is (`#59935B33`), with the text left at the page's own foreground rather than
forced to black.

## 10. Maintainer’s answers to open questions

- **Package name.** It appears in every YAML file for years; **Maintainer’s decision:** `txtheme` .
- **`brand.yml` becomes a website build dependency** (`Config/Needs/website`). It is a small pure-R package; the alternative is to have the generator emit plain bslib variables instead of a brand file, which costs nothing today but loses the shared format the day another tool reads it. **Maintainer’s decision: no problem ; it I understand well, it won’t even be a tabxplor Suggest that would need to be released on CRAN, only a website build dep ?**
- **Light theme.** The proposal keeps pkgdown's own light side (`arrow-light` plus bootstrap defaults). If the light half should also become the editor's light palette, that is one more `overrides.yml` block and no architectural change. **Maintainer’s decision: future proof for customisation of the light theme is good.**

---

## Appendix A — verified facts

Measured on 2026-08-27: pkgdown 2.2.1, bslib 0.11.0, brand.yml 0.1.0, Quarto 1.9.38 (Positron-bundled), R 4.6.1.

- **pkgdown reads `_brand.yml`** — `as_pkgdown(override = list(template = list(bslib = list(brand = …))))`, then `pkgdown:::bs_theme()`; `bs_get_variables()` returned the brand's `body-bg`, `body-color`, `link-color` and `font-family-base`.
- **pkgdown reads a theme from a package** — `pkgdown:::bs_theme_rules()` reads `path_package_pkgdown("extra.scss", package, bs_version)`.
- **The R brand side is single-mode** — `brand.yml::read_brand_yml()` on a file with `color.foreground: {light:, dark:}` fails with *"must be a single string or `NULL`, not a list"*.
- **Quarto reads a light and a dark brand** — a `.qmd` with `brand: {light:, dark:}` at document level rendered two different bootstrap stylesheets, the dark one carrying the dark brand's hexes, plus a colour-scheme toggle.
- **`brand:` under `format: html:` is ignored** — the same document with `brand:` nested produced two byte-identical stylesheets (same md5) carrying none of the brand colours.
- **Quarto takes a light/dark code theme** — `syntax-highlighting: {light:, dark:}` produced two `quarto-syntax-highlighting*.css`, linked as `quarto-color-scheme` and `quarto-color-scheme quarto-color-alternate`.
- **The two code-theme formats share one vocabulary** — a pandoc `.theme` has 31 `text-styles` (`Comment`, `String`, …); pkgdown's `.scss` files carry the same 31 as two-letter classes.
- **tabxplor already follows both** — `tx_dark_hooks` (`R/tab-css.R`) contains `[data-bs-theme=dark]` *and* `body.quarto-dark`; the rendered Quarto page is `<body class="fullcontent quarto-light">`.
- **pkgdown's defaults** — `template.theme` defaults to `arrow-light` and `template.theme-dark` to `arrow-dark` (`pkgdown:::bs_theme_rules`, `pkgdown:::bs_theme`).

## Appendix B — the immediate fixes

These apply to `_pkgdown.yml` **now**, with no framework. They are step 1 of §8.

**The white code box was the preview tool's bug, not pkgdown's.** `arrow-light` carries `pre {background-color: #f1f3f5}`; the preview scoped it with an attribute selector, which out-specified bootstrap's `[data-bs-theme="dark"] pre` and painted the box white on the dark page. On the real site the light theme is emitted unscoped at (0,0,1) and loses, so `starless` — no `pre` background at all — does work there. `dev/site_theme_preview.R` now scopes the light layer `:not([data-bs-theme="dark"])`. Naming `#1f1f1f` explicitly is still the better choice: it is what the editor shows, and it does not depend on a bootstrap default.

**The corrected YAML**, with the three colour decisions folded in:

```yaml
template:
  bootstrap: 5
  light-switch: true
  theme: arrow-light
  theme-dark: arrow-dark          # overridden by pkgdown/extra.scss
  bslib:
    body-bg-dark: "#21252b"
    body-color-dark: "#CECDC3"    # warm, not #dee2e6: the text must not outshine the colours
    body-emphasis-color-dark: "#e6e6e6"
    headings-color-dark: "#61afef"
    body-tertiary-bg-dark: "#282c34"
    border-color-dark: "#3e4451"
    link-color-dark: "#61afef"
    code-color-dark: "#fc9867"    # the theme's own inline-code orange
```

and `pkgdown/extra.scss`:

```scss
// the code theme: dev/highlight-starless-monokai-atom.scss, copied in whole
html[data-bs-theme="dark"] pre { background-color: #1f1f1f; }
html[data-bs-theme="dark"] strong,
html[data-bs-theme="dark"] b { color: #e6ae02; }
html[data-bs-theme="dark"] blockquote {
  border-left-color: #e6ae02;
  color: #B7B5AC;
  font-style: italic;
}
```

⚠ The `html` prefix on the code-theme rules is load-bearing: pkgdown adds `theme-dark` **after** `pkgdown/extra.scss`, so source order cannot win — only the extra element in the selector can.

`dev/site_theme_preview.R` regenerates the preview page and the three `dev/highlight-starless-monokai-*.scss` ports; its prose control shows the gold bold and the quote rules above, and its YAML box prints exactly this block.

---

## Appendix C — what is left of the courses' `style.css`

`~/github/formations_stat/style.css` is 769 lines, of which **about 80 are live CSS**: the rest is
commented-out experiments (three TOC layouts, an xaringanExtra clipboard button, and one whole
duplicated block at the end). `resources/tab.css` is 76 lines and **entirely** old table styling.

**Discarded without asking**, because something else now owns it:

- everything matching `.lightable-classic` and `.popover`, in both files — kableExtra's table skin
  and tabxplor's old tooltip. `tab_css()` has owned all of it since 2.0.0, and kableExtra was
  dropped from the package entirely.
- everything matching `.webex-*`, `textarea`, and the three `--show_answers` / `--try_again` /
  `--welldone` strings — webexercises' own, and §9.1 is where they are dealt with.
- every commented-out block.

⚠ **`--border-color` is used and never defined** (`resources/tab.css`, the `.lightable-classic`
border rules). A `var()` that resolves to nothing makes the whole declaration invalid, so those
borders have never been drawn. Nothing to port; worth knowing before anyone "restores" it.

**The candidates.** Nothing below is applied yet — this is the list to choose from.

| # | element | what it does | note |
|---|---|---|---|
| 1 | **DejaVu Sans / Sans Condensed** (4 `@font-face`) | the course typeface | keep the face, change the delivery |
| 2 | **heading family + bold** | `DejaVu Sans`, bold, `!important` | drop `!important`; brand `typography.headings` |
| 3 | **heading spacing ladder** | `padding-top`: h1 10, h2 **150**, h3 40, h4 20px | a scroll-anchor hack — see below |
| 4 | **heading scale** | h4 underlined, h5 16px, h6 14px italic | a real voice, cheap to keep |
| 5 | **tight paragraph rhythm** | `p` 8px/2px, `ul p` 0 | what makes a dense course page readable |
| 6 | **`.compact-list`** | tighter still, opt-in per list | keep as is |
| 7 | **`.column-display`** | a flex row of columns | Quarto has `:::{.columns}` natively |
| 8 | **`.footnote`** | 80%, tight leading | keep as is |
| 9 | **`.exercise span`** | underlined term, 3px | needs `--highlight` (§9.1) |
| 10 | **code wrapping trio** | `pre {word-break: normal}`, `white-space: inherit` on code | check the intent first |

Four of them deserve a sentence before they are chosen:

- **#1 fonts.** Worth keeping for a reason beyond taste: `tab_css()` already asks for *DejaVu Sans
  Condensed* for table text, so the course typeface and the tables' agree today by coincidence. But
  the delivery must change: `style.css` pulls the `.ttf` from `raw.githubusercontent.com` on every
  page load — a third party in the render path, serving a file GitHub does not promise to keep at
  that URL. Bundle the woff2 in `txtheme`, or take the face from Google/Bunny through
  `typography.fonts`.
- **#3 heading spacing.** `h2 { padding-top: 150px }` is not typography, it is a scroll offset for a
  fixed header — and it also pushes 150px of blank into every printed page. Quarto handles anchor
  offsets itself; if the intent survives the move, `scroll-margin-top` is the property that means it.
- **#7 columns.** Quarto's own `:::{.columns}` / `.grid` do this with layout attributes and degrade
  properly on a phone. Keeping `.column-display` as an alias costs three lines; porting the documents
  costs a search-and-replace. Either is defensible — but keeping both is not.
- **#10 code wrapping.** `pre { word-break: normal; word-wrap: normal }` stops long code lines from
  breaking mid-token, which is right, and is what pkgdown and tabxplor already do (with a horizontal
  scroll). `p code { white-space: inherit }` is the opposite bet — it lets *inline* code wrap
  anywhere, including mid-identifier. Worth confirming that this was deliberate before porting it.
