# A black-and-white "publication ready" palette — design study

Date: 2026-08-05. Status: **REPORT ONLY** (Last Phase z7, item 3). No code written.

Scope: an opt-in monochrome rendering of tabxplor's colour measures for print/publication, shared by
the HTML and Excel (and, by paste, Word) exports. The console keeps its colours.

---

## 0. Executive summary

**The feature is justified by a measurement, its ceiling is set by perception, and its shape is set by
one sentence in the Elsevier author guidelines.**

1. **The current palette is not greyscale-safe, and this is measurable.** Converting the shipped
   OKLCH palettes to CIE lightness:

   | channel | over slots 1-4 (L\*) | under slots 1-4 (L\*) |
   |---------|----------------------|-----------------------|
   | text    | 62, 57, 44, 34       | 71, **62**, 54, 45    |
   | bg      | 97, 93, 90, 82       | 97, 93, 89, 82        |

   Over-slot 1 and under-slot 2 have **identical lightness (62)**, and the background channel's two
   directions are **the same ramp to within 1 L\* at every step**. Printed in greyscale — which is how
   most journal readers see a table — a tabxplor table currently loses the over/under distinction
   entirely on the background channel and partly on the text channel. That is exactly the failure APA
   and ACM tell authors to check for. §1.
2. **Greyscale alone cannot diverge.** A grey ramp is an *ordered* variable with one end; a
   diverging scale needs a neutral in the middle, which in greyscale means shading **every** cell
   mid-grey — the "overwhelming" outcome the brief rules out. So direction must come from a **second**
   visual variable. This is Bertin's own distinction: *value* (black/white ratio) is ordered and
   quantitative, *texture/shape/weight* are selective. Use the ordered one for magnitude and the
   selective one for direction — never the reverse. §3.
3. **Elsevier's author guidelines explicitly discourage shading in table cells, and explicitly
   sanction bold face, italics, subscripts and superscripts.** APA says to limit shading and to pair
   any shading with patterns. **So the default must be typographic, not shaded** — the opposite of
   what "greyscale palette" first suggests. A shaded variant should exist, but not as the default. §2.
4. **How many steps?** Measured on the grey ramp: adjacent 5 % tint steps differ by ΔL\* ≈ 4.5 (at the
   published discrimination threshold), 10 % steps by ΔL\* ≈ 9 (comfortable), and black text keeps
   AAA contrast (≥ 7:1) down to a 40 % tint. So **shading supports up to 4 ordered levels**, 3
   comfortably. **Typography supports 2 comfortably, 3 at a push** (plain → underline → double
   underline). The brief's hope of "2 breaks over and 2 breaks under" is therefore achievable with
   room to spare; 4 + 4 is not honestly achievable typographically. §4.
5. **Underline styles are a poor gradient and a decent flag.** Excel offers only `single` and `double`;
   pandoc's markdown has no underline at all (only the `<u>` fallback / a `.underline` span). CSS has
   dotted/dashed/wavy, but they are *nominal*, not ordered, and they do not survive Excel or Word.
   **Two underline steps maximum, and treat them as ordinal-by-convention, not perceptually ordered.**
   §2, §4.
6. **There is a fully portable ordered channel the brief did not list: repeated superscript marks**
   (`⁺`, `⁺⁺`, `⁻`, `⁻⁻`). Elsevier names superscripts as acceptable; they render in HTML (`<sup>`),
   Excel (`vertAlign`), pandoc markdown (`^+^`) and plain text; they are unambiguous about direction;
   and they are the only option a screen reader can convey. Cost: cell width, and visual competition
   with significance stars. §5.
7. **Recommended: three named schemes, one default.**
   - **`print`** (default) — *typographic only*: over = **bold**, under = *italic*; second level adds an
     underline. Two levels per side. No shading, no width change, coexists with stars, survives
     HTML → Word paste. Elsevier-safe.
   - **`print_marks`** — adds `⁺`/`⁻` repetition for up to 4 ordered levels and explicit direction.
     Best when `stars = FALSE`.
   - **`print_shaded`** — grey fills (white / 15 % / 30 %) carry magnitude on the background channel,
     typography carries direction. Best on screen and in Excel; the one to avoid for Elsevier.
   §6.
8. **The architecture cost is one generalisation, and it is additive.** Today a palette is *8 hex codes
   per channel*. It becomes *8 **renderings** per channel* — hex **plus** an optional
   `list(bold, italic, underline, mark, pattern)`. Colour palettes leave the new fields `NULL`, so every
   existing output is byte-identical. Every consumer already goes through `fmt_channel_codes()` /
   `tx_slot_class()`, so there is exactly one place to widen and four backends to teach. §7.
9. **The switch should be `theme = "print"`, not a new argument.** `theme` is *already* the
   "which palette variant" axis (`light` / `dark` / `auto`), it is already threaded through
   `tab_html()`, `tab_md()`, `tab_xl()`, `tab_css()`, `resolve_export_opts()` and two options, and the
   console reads a *different* option (`tabxplor.console_theme`) so it stays coloured for free. **Zero
   new user-facing arguments.** §8.
10. **One elegant extra, nearly free**: `tab_css()` can emit the `print` rules inside an
    `@media print` block, so a coloured HTML table **prints** in the B&W scheme automatically, with no
    user action at all. §8.3.
11. **A palette must declare how many levels it can express**, and the engine must cap slots to it
    (`pmin`), so a 2-level scheme does not silently render four thresholds with two appearances. One
    field, one `pmin`, and it makes the legend honest. §4.3.

---

## 1. The measurement that justifies the feature

`get_color_style("color_code", …)` on the shipped light palettes, converted to CIE L\*:

```
text  #02a5b3(L*62) #0891c9(L*57) #0267c7(L*44) #300dfd(L*34)   <- over 1..4
      #dca331(L*71) #de7c01(L*62) #dd5301(L*54) #d60103(L*45)   <- under 1..4
bg    #dffcff(L*97) #d7efff(L*93) #cee3ff(L*90) #bbccff(L*82)   <- over 1..4
      #fff4e1(L*97) #ffe6d3(L*93) #ffd7c8(L*89) #ffbaaf(L*82)   <- under 1..4
```

- **Background channel: the two directions are the same greyscale ramp** (97/93/90/82 vs 97/93/89/82).
  In a greyscale print, a strongly over-represented cell and a strongly under-represented cell are
  *the same shade*. The information is not degraded — it is gone.
- **Text channel: over-1 and under-2 are both L\* 62**, and the whole under ramp sits inside the over
  ramp's range. A reader of a greyscale print can recover magnitude, not direction.

This is not a design flaw of the palettes — they are tuned for hue discrimination on screen, and the
24-bit blue-red variant was deliberately chosen for colour-vision-deficiency support. It is simply
the reason a **separate** monochrome palette is the right answer rather than "desaturate the existing
one": desaturation is exactly the operation measured above, and it destroys the direction.

---

## 2. What the publishing world actually says

| source                                   | what it says                                                                                                                                       |
|------------------------------------------|----------------------------------------------------------------------------------------------------------------------------------------------------|
| **Elsevier**, author guidelines          | "The use of vertical rules and shading in table cells is not recommended." **But**: bold face, italics, subscripts and superscripts *are* the sanctioned in-table emphasis. |
| **APA 7th**, table setup                 | "Limit the use of borders, shading, or lines unless needed for clarity"; and where colour is used, "use patterns along with color… ensure all users (people with color vision deficiencies **or readers printing in grayscale**) can access the content." |
| **ACM**, accessibility recommendations   | An article should remain readable when printed in greyscale; ~8 % of men have a red-green deficiency.                                               |
| **APA / general**                        | Significance is conventionally carried by **asterisks with a footnote**, used consistently for a given α throughout the paper.                       |
| **Bertin (1967/1983)**, visual variables | *Value* (black/white ratio) is ordered and quantitative. *Texture/grain* is selective and associative; hatching can carry quantity only if the pattern and direction are held constant. *Shape* and *orientation* are selective, never ordered. |

Three conclusions follow directly:

1. **A shaded default would be the one thing a major publisher asks authors not to do.** The default
   must be typographic.
2. **Superscript marks are explicitly blessed** — which makes the `print_marks` variant of §5 more
   defensible than it first looks.
3. **Significance keeps its own channel** (asterisks + footnote). Do not re-encode significance in the
   B&W palette; tabxplor already separates magnitude (`color`) from significance (`color_signif` /
   `stars`), and that separation is exactly what the conventions expect.

**Do reviewers accept it?** The honest answer for the docs: *typographic* emphasis is universally
accepted (it is in the Elsevier spec). *Shading* is journal-dependent — accepted in many sociology and
management journals, discouraged by Elsevier's generic guidance, and it is a copy-editing risk because
production may flatten it. So the package should default to the safe one and let the user opt into the
other, exactly as it defaults to a light theme and lets the user opt into dark.

---

## 3. Why greyscale alone cannot do it

A diverging scale needs three zones: below-neutral, neutral, above-neutral. In colour this is free —
two hues around a light neutral. In greyscale the "neutral" would have to be a **mid grey**, so:

- every uncoloured cell would carry a mid-grey fill (a fully shaded table — the "overwhelming" outcome
  the brief rules out, and the one Elsevier warns against);
- and the dark end would need white text, which no other tabxplor path produces.

The alternative Bertin points to — **texture for direction, value for magnitude** (solid tints one way,
hatching the other) — is genuine and is what cartographers do. Excel supports it (`lightUp`,
`darkGrid`, `lightTrellis`, … via `create_cell_style(pattern_type =)`), and CSS can do it with
`repeating-linear-gradient`. But hatching **behind digits** hurts legibility at table sizes, it does
not survive an HTML → Word paste, and it is invisible in markdown. **Recorded as available, not
recommended.**

Which leaves the working principle:

> **Ordered channel (grey value, or a repeated mark) carries MAGNITUDE.
> Selective channel (bold vs italic) carries DIRECTION.**

---

## 4. The capability matrix, and the ceiling

### 4.1 What each medium can express

Verified against the CSS specification, `openxlsx2`'s `create_font()` / `create_cell_style()`, and the
pandoc manual.

| attribute            | HTML/CSS               | Excel (openxlsx2)                  | pandoc md → Word            | HTML → Word (paste) |
|----------------------|------------------------|------------------------------------|-----------------------------|---------------------|
| **bold**             | ✓ `font-weight`        | ✓ `bold`                           | ✓ `**x**`                   | ✓                   |
| **italic**           | ✓ `font-style`         | ✓ `italic`                         | ✓ `*x*`                     | ✓                   |
| **underline**        | ✓ `text-decoration`    | ✓ `u = "single"`                   | ~ `<u>` or a `.underline` span | ✓                |
| **double underline** | ✓ `underline double`   | ✓ `u = "double"`                   | ✗ (needs `custom-style` + a reference.docx) | ✓ |
| dotted/dashed/wavy underline | ✓              | ✗                                  | ✗                           | partial             |
| **strikethrough**    | ✓                      | ✓                                  | ✓ `~~x~~`                   | ✓                   |
| **superscript mark** | ✓ `<sup>`              | ✓ `vertAlign = "superscript"`      | ✓ `^+^`                     | ✓                   |
| **grey text**        | ✓                      | ✓                                  | ✗ (span class dropped)      | ✓                   |
| **grey cell fill**   | ✓ `background-color`   | ✓ `create_fill()`                  | ✗ (a *span* background at best) | ✓ (cell shading survives) |
| **pattern fill**     | ✓ `repeating-*-gradient` | ✓ `pattern_type`                 | ✗                           | partial             |
| font size            | ✓                      | ✓                                  | ✗                           | ✓ — **but breaks table alignment; reject** |

Two readings of this table matter.

- **The portable core is bold + italic + underline + superscript.** All four survive every route,
  including plain markdown into Word.
- **Word is reached by paste, not by a writer.** tabxplor has no `.docx` exporter (`tab_export()` is
  `html` / `md` / `xl` / `plot`). Pasting the HTML table into Word preserves character formatting *and*
  cell shading; pandoc markdown → docx preserves only what markdown can say. So **investing in the
  HTML rendering is what buys Word**, and the markdown path should degrade to the portable core
  rather than being designed for.

### 4.2 How many ordered steps — measured

Grey ramp, CIE L\* and WCAG contrast against black text:

| tint | hex       | L\*  | ΔL\* vs previous | contrast with black |
|------|-----------|------|------------------|---------------------|
| 0 %  | `#FFFFFF` | 100.0| —                | 21.0                |
| 5 %  | `#F2F2F2` | 95.5 | 4.5              | 18.8                |
| 10 % | `#E6E6E6` | 91.3 | 4.2              | 16.8                |
| 15 % | `#D9D9D9` | 86.7 | 4.6              | 14.9                |
| 20 % | `#CCCCCC` | 82.0 | 4.6              | 13.1                |
| 30 % | `#B3B3B3` | 72.9 | —                | 10.0                |
| 40 % | `#999999` | 63.2 | —                | 7.4                 |
| 50 % | `#808080` | 53.6 | —                | 5.3                 |

Candidate ramps:

| ramp                     | L\* values     | min adjacent ΔL\* | min contrast w/ black |
|--------------------------|----------------|-------------------|-----------------------|
| white / 10 % / 20 %      | 100, 91, 82    | 8.7               | 13.1                  |
| **white / 15 % / 30 %**  | 100, 87, 73    | **13.3**          | 10.0                  |
| white / 12 / 25 / 40 %   | 100, 89, 77, 63| 10.8              | 7.4                   |
| white / 20 % / 40 %      | 100, 82, 63    | 18.0              | 7.4                   |

The published guidance for greyscale-distinguishable sets is a minimum lightness difference of ~5.0
(six categories) to ~3.6 (ten). Every candidate above clears it comfortably, and black text keeps
**AAA** contrast (≥ 7) even at a 40 % tint.

**So shading supports 4 ordered levels and 3 with margin.** The binding limit is not perception, it is
the publisher guidance of §2 — and the fact that a table with four fill levels reads as a heat map,
not as a table.

### 4.3 A palette must declare its resolution

Typography does **not** reach four. Honest counts:

| channel                       | comfortable | maximum |
|-------------------------------|-------------|---------|
| bold / italic (direction)     | 2 classes   | 2       |
| underline steps (magnitude)   | 2 (none, single) | 3 (+ double, HTML/Excel only) |
| repeated superscript marks    | 3           | 4       |
| grey fill                     | 3           | 4       |

The engine always produces slots 1-4 per side. A 2-level palette that maps 1,2 → *A* and 3,4 → *B*
would render four legend thresholds with two appearances — the legend would promise a distinction the
cells do not make.

**Recommendation: a palette declares `levels` (2, 3 or 4), and `fmt_color_slots()` caps the slot with
`pmin(slot, levels)` per side; the legend groups its break-words to match.** One field, one `pmin`,
one legend change. It also gives the 8-bit console palettes and any future low-resolution medium a
principled home, instead of the current implicit assumption that every medium has four shades.

---

## 5. The channel the brief did not list: repeated marks

`77 %⁺⁺` / `31 %⁻`.

| property                | verdict                                                                                          |
|-------------------------|--------------------------------------------------------------------------------------------------|
| ordered                 | **yes** — repetition is the most primitive ordered encoding there is                             |
| direction               | **yes, explicitly** — no convention to learn, no legend needed to know which way                 |
| portability             | **total** — HTML `<sup>`, Excel `vertAlign`, pandoc `^+^`, and it degrades to plain text intact  |
| accessibility           | **the only option a screen reader can read aloud**; also the only one that survives a plain-text copy |
| publisher acceptance    | Elsevier names superscripts as acceptable in-table emphasis                                       |
| **cost 1: width**       | changes cell width; tabxplor already pads for significance stars, so the machinery exists         |
| **cost 2: competition** | `77 %⁺⁺***` is busy — two trailing symbol runs meaning two different things                       |

Cost 2 is the real one, and it has a clean resolution rather than a compromise: **tabxplor already has
two ways to show significance**, and they are alternatives, not companions —

- `stars = TRUE` (trailing asterisks), or
- `color_signif = "grey_non_signif"` / `"guaranteed_effect"` (the colour itself is gated).

In a print palette the second is the better fit anyway (greyed-out text for non-significant cells is
the one thing greyscale does perfectly, and the brief explicitly wants grey kept for below-threshold).
So the documented recommendation becomes: **`print_marks` + `color_signif`, or `print` + `stars`** —
never both symbol runs at once. That is a vignette paragraph, not a code branch.

---

## 6. The three proposed schemes

Written as they would appear in the palette table. `—` = no attribute added.

### 6.1 `print` — the recommended default (typographic, Elsevier-safe, `levels = 2`)

| slot | meaning       | text channel                | background channel |
|------|---------------|-----------------------------|--------------------|
| 0    | neutral       | plain black                 | none               |
| p1   | over, break 1 | **bold**                    | none               |
| p2   | over, break 2 | **bold + underline**        | none               |
| m1   | under, break 1| *italic*                    | none               |
| m2   | under, break 2| *italic + underline*        | none               |
| —    | non-significant (`grey_non_signif`) | grey `#9f9f9f` (unchanged) | — |

Direction = bold vs italic (selective). Magnitude = underline present/absent (ordered by convention).
No width change; stars unaffected; survives every medium including a plain markdown paste into Word.

**The one honest weakness**: italic digits are less distinct than bold digits in most fonts, so the
under-represented pole is quieter than the over-represented one. Two mitigations, both cheap and both
worth putting to the maintainer (§10 Q3): swap the mnemonic (**under = underline**, over = bold, using
underline for *direction* and a second grey level or a mark for magnitude), or keep italic and accept
that over-representation is usually the reader's focus.

### 6.2 `print_marks` — explicit and portable (`levels = 3` or 4)

| slot | text channel                        |
|------|-------------------------------------|
| p1   | **bold** + `⁺`                      |
| p2   | **bold** + `⁺⁺`                     |
| p3   | **bold** + `⁺⁺⁺`                    |
| m1   | *italic* + `⁻`                      |
| m2   | *italic* + `⁻⁻`                     |
| m3   | *italic* + `⁻⁻⁻`                    |

Recommended with `stars = FALSE` and a `color_signif` policy. The most accessible variant, and the one
that still works when a reader copies the table as plain text.

### 6.3 `print_shaded` — for screen and Excel (`levels = 3`)

| slot | text channel   | background channel |
|------|----------------|--------------------|
| p1   | **bold**       | white              |
| p2   | **bold**       | 15 % grey `#D9D9D9`|
| p3   | **bold**       | 30 % grey `#B3B3B3`|
| m1   | *italic*       | white              |
| m2   | *italic*       | 15 % grey          |
| m3   | *italic*       | 30 % grey          |

Note the coupling this creates: the fill now expresses the **text** channel's magnitude, so this
variant is only coherent when a *single* measure is coloured. With `color = c("diff", "ratio")` the
background is already taken by the second measure. **The palette must therefore refuse (with a
message) or fall back to `print` when two channels are active** — a real constraint, and the reason
this variant is not the default.

---

## 7. Architecture — one generalisation, additive

### 7.1 The current shape

`tabxplor_palette_env$hex[[key]]` holds **8 hex codes**, `key ∈ {text, bg, bg_legend} × {light, dark}`.
Everything downstream reads it through two functions:

- `fmt_channel_codes(x, theme)` → `list(text = <hex>, bg = <hex>, text_slot, bg_slot)`; consumed by
  `tab_xl` (Phase 17g reads `ann$text_hex`/`ann$bg_hex` directly), `tab_plot`, and the render prep;
- `tx_slot_class(channel, slot)` → `.p1`/`.m1`/`.o1`/`.u1`; consumed by the HTML engine, `tab_md` and
  `tab_css()`.

Both already exist precisely so "the legend and the cells cannot disagree". That is the seam.

### 7.2 The change

```
tabxplor_palette_env$hex[[key]]    8 hex           (unchanged)
tabxplor_palette_env$style[[key]]  8 records, or NULL for a colour palette
                                   record = list(bold=, italic=, underline=, mark=, pattern=)
tabxplor_palette_env$levels[[key]] 2 | 3 | 4        (SS4.3)
```

- `fmt_channel_codes()` gains `text_style` / `bg_style` in its returned list. **When `style` is `NULL`
  — every existing palette — the extra elements are `NULL` and every backend's behaviour is
  unchanged.** That is the byte-identity guarantee, and it is what makes this safe to land.
- `tx_css_rules()` already loops `for (ch in c("text","bg")) for (s in 1:8)` emitting one property per
  slot. It emits two or three more (`font-weight`, `font-style`, `text-decoration`). Note the existing
  static rule that **bolds every coloured text slot** (`tab-css.R:235`) must become part of the palette
  record rather than a constant, or `print` cannot express "not bold".
- `tab_xl`'s `xl_build_styles()` already composes a per-cell font from a hex; it composes `bold`,
  `italic`, `u` and (for `print_shaded`) a fill from the same record. openxlsx2 supports all of them
  natively.
- `tab_md`'s `md_color_cell()` already wraps the value in a `[…]{.pN}` span; for `print` it also emits
  `**`/`*`/`<u>` so the markdown carries the meaning without a stylesheet. That is the one place the
  portable core of §4.1 is honoured.
- `legend_render_line()` already renders break-words per medium from the same slot; it gains the same
  attributes, so the legend shows a **bold** word for the over breaks and an *italic* one for the
  under breaks — self-describing, and impossible to desynchronise from the cells.
- `tab_plot` (frozen legacy) uses `fontface` in ggplot2, which covers bold/italic; it keeps working
  either way and needs no investment.

### 7.3 What must NOT be done

- A separate "publication renderer" beside the colour one. Two renderings of one model is the disease
  Phase 17 removed.
- Slot-to-appearance decisions inside a backend. The palette is the single source; a backend only
  translates a record into its own vocabulary.
- Deriving the B&W look by desaturating the colour palette at render time — §1 measured why that
  destroys the direction.

---

## 8. How the user turns it on

### 8.1 Recommended: `theme = "print"`

`theme` is *already* the "which palette variant" axis: `light` / `dark` / `auto`, resolved in exactly
one place (`tx_palette_theme()`), threaded through `tab_html()`, `tab_md()`, `tab_xl()`, `tab_css()`,
`resolve_export_opts()` and the options `tabxplor.export_theme` / `tabxplor.console_theme`.

Adding `"print"` as a third value gives, with **no new argument anywhere**:

```r
tab_export(t, format = "xl",   theme = "print")
tab_html(t,  theme = "print")
options(tabxplor.export_theme = "print")     # a whole document
```

and — because the console reads `tabxplor.console_theme`, a *different* option — the console stays
coloured for free, which is exactly the brief's requirement.

Two consequences to handle: `tx_chrome_hex()` needs a `print` row (it is the light one, with a black
border), and `tx_palette_theme()`'s "auto" resolution must not swallow it.

**The one objection**, stated honestly: `light`/`dark`/`auto` describe *the reader's background*, while
`print` describes *the destination medium*. They are not the same kind of thing, and a purist would
want a separate `palette =` axis. The counter-argument is that `theme` has always in practice meant
"which palette variant to render with", and that a separate axis would need to be added to six
signatures and two options for one value. **Recommend `theme = "print"`; record the objection.**

### 8.2 Naming

`"print"` reads better than `"bw"` (it says *why*, not *how*, and it leaves room for a future
`print_shaded` to be a shading choice rather than a colour choice). `"bw"` should be accepted as a
silent alias — `tx_getOption()`'s synonym resolver (Phase 17j) is the existing mechanism.

### 8.3 The nearly-free bonus: `@media print`

`tab_css()` already emits `@media (prefers-color-scheme: dark)` blocks and the light/dark toggle hooks.
Emitting the `print` palette's rules inside **`@media print`** means a coloured web page's tables
**automatically become publication-ready when printed or saved to PDF**, with no argument and no user
awareness. Same generator, one more at-rule, and it is the single most convincing demo of the feature.

Worth an opt-out (`tab_css(print_rules = FALSE)`) for a user whose colour printer is the point.

---

## 9. Caveats, tensions and things to cut

- **Bold is already taken, twice.** `tab_export_prep()`'s `tab_bold_rows()` bolds reference and total
  rows, and `tab-css.R:235` bolds every coloured text slot. In `print`, bold acquires a third meaning
  ("over-represented"). The overlap is partial (structural bolding is on labels and total rows; the
  measure's bolding is on body cells), but it is real and it must be a conscious decision, not a
  discovery at review time. If it proves confusing, the fix is the §6.1 swap (direction by underline).
- **Grey text is already taken, correctly.** `color_signif = "grey_non_signif"` greys non-significant
  cells (`g1`/`g2`). Keeping that unchanged — as the brief asks — is another argument for the
  *unshaded* default: with grey fills in play, grey text and grey fill compete for the same
  perceptual channel.
- **`print_shaded` cannot coexist with a two-channel `color =`** (§6.3). Needs a message and a
  fallback, not silent misrendering.
- **Markdown is the weak link.** `[77%]{.p1}` conveys nothing without a stylesheet; only the
  `**`/`*`/`<u>` wrapping does. So the md backend must emit *both* (class **and** markup) in `print`
  mode — which is a small but real asymmetry with the colour palettes, where the class alone suffices.
- **Deliberately cut**: pattern/hatch fills (§3 — legibility and portability), font-size gradients
  (destroy alignment), cell borders as a magnitude channel (fights the table's own rules, and Elsevier
  discourages vertical rules), and a fourth typographic level. Also cut: any attempt to *derive* the
  print palette automatically from the colour one.
- **Not investigated here, and it should be before implementation**: how the jamovi results panel
  renders it (it uses the HTML engine, so it should follow, but its `.tx-scrollbox` and the webview's
  own stylesheet are a known source of surprises), and whether `tab_plot`'s frozen status means it
  should simply ignore `theme = "print"` rather than half-support it.

---

## 10. Open questions for the maintainer

- **Q1 — the default scheme.** `print` (typographic, §6.1, recommended and Elsevier-safe),
  `print_marks` (§6.2), or `print_shaded` (§6.3)? Ship one, two or all three?
- **Q2 — the switch.** `theme = "print"` (recommended, zero new arguments, §8.1) or a separate
  `palette =` axis?
- **Q3 — the direction mnemonic.** over = **bold** / under = *italic* (recommended), or over = bold /
  under = <u>underline</u> (more legible on digits, weaker mnemonic against "underline = second
  level")? This decides §6.1's whole table.
- **Q4 — levels.** Confirm that a palette declares `levels` and the engine caps with `pmin` (§4.3),
  including the legend grouping. Without it a 2-level palette lies about its thresholds.
- **Q5 — `@media print`** (§8.3): emit the print rules automatically from `tab_css()`? Recommended —
  it is the cheapest large win in this report.
- **Q6 — stars vs marks.** Confirm the documented rule "`print` + `stars`, or `print_marks` +
  `color_signif` — never both" (§5), rather than a code-level exclusion.
- **Q7 — scope.** HTML + Excel + markdown only, leaving `tab_plot` frozen and unaffected?
- **Q8 — is it in 2.0.0 at all?** Everything above is additive and byte-identical when unused, but it
  touches the palette structure, which is the one shared object every backend reads. If the release is
  close, the safe split is: **the palette generalisation (§7.2) alone in 2.0.0** — since it is the part
  that must be right before the structure freezes — with the schemes themselves following in 2.0.1.

---

## 11. References

- **Elsevier**, *Guide for authors* / *Quick Reference Guide to the Elsevier Copyediting Specification
  for Authors* — vertical rules and cell shading discouraged; bold, italics, subscripts and
  superscripts are the sanctioned in-table emphasis.
- **APA Publication Manual, 7th ed.**, *Table setup* (apastyle.apa.org) — limit borders/shading/lines;
  pair colour with patterns; ensure greyscale-print and colour-vision accessibility.
- **ACM**, *Accessibility Recommendations for Publishing in Color* — greyscale-print readability as a
  first-class requirement.
- **Bertin J.** (1967, English ed. 1983) *Semiology of Graphics* — the visual variables and their
  perceptual properties (value = ordered/quantitative; texture and shape = selective). The basis of
  §3's "ordered channel for magnitude, selective channel for direction".
- **Zhong Y. et al.** (2020) *Black-and-White Textures for Visualization on E-ink Displays*; and
  *Design Characterization for Black-and-White Textures in Visualization* (arXiv 2307.10089) — the
  modern empirical treatment of hatching as a data channel, and why grain spacing is the ordered part.
- **WCAG 2.x** contrast ratios; CIE L\* — used for the measurements of §1 and §4.2.
- **openxlsx2**: `create_font()` (bold, italic, `u = "single"/"double"`, strikeout, `vertAlign`),
  `create_cell_style()` / `wb_add_fill()` (`pattern_type`: `lightUp`, `darkGrid`, `lightTrellis`, …).
- **pandoc** *User's Guide* — `**bold**`, `*italic*`, `~~strike~~`, `^superscript^`; no native
  underline (use `<u>` or a `.underline` Span); character styles via
  `[text]{custom-style="…"}` with a reference `.docx`.
- **finalfit**, **gtsummary**, **flextable**, **kableExtra** — the neighbouring R table packages. None
  ships a monochrome *measure* palette; they offer per-cell formatting the user drives by hand. This
  would be a genuine differentiator, in the same way the colour measures already are.

**In-repo companions**: `dev/new_colors_UI.md` (the colour framework brief),
`dev/color_blind_palettes_guide.md` (the CVD work this extends — greyscale is the limiting case of the
same problem), `dev/design_new_colors_UI_decision_process.md`, `R/tab-css.R` (the one CSS generator),
`R/tab_xl.R` §`xl_build_styles`, `R/tab_classes.R` §palettes.
