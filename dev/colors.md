# The colour system — what a break means, and what a palette must survive

> PURPOSE: the three things the colour fact tables state in one line each and cannot derive — where
> every ladder's first rung comes from, what a palette has to do to stay readable for a colour-blind
> reader, and why a page with no colour needs a different palette rather than a desaturated one.
> ROLE: what `R/tab_classes.R` (`COLOR_SCALES`), `R/tab-palettes.R` (`COLOR_RAMPS`,
> `PRINT_PALETTES`) and `R/fmt_class.R` (`MEASURES`, the selection engine) implement. Those grids are
> the live values and carry the design decisions in their own dictionaries; this carries the
> derivations and the external evidence behind them.
> KEY CONSTRAINTS:
>   - **Never quote a hex or a break value from this file.** `COLOR_RAMPS`' `oklch` column and
>     `COLOR_SCALES`' `default` are the values; anything written here is an illustration.
>   - The ladder shape rule is checked at load (`tx_check_color_scales()`), so a drifting default
>     fails the install. This file explains the rule; the code enforces it.
> See: `CLAUDE.md § tabxplor architecture` (the colour system) · `.claude/skills/color-mode`.

---

## 1. The one idea

Colour encodes two things at once — **how big a deviation is** and **whether it is statistically
solid** — and it must do so in four media, on two screen themes, for a reader who may not see red and
green apart, and on a page that may have no colour at all.

Three orthogonal axes carry it: a **measure** (which deviation to grade), a **channel** (text and/or
background), and a **significance policy**. Everything below is about the first two.

The single fact that makes the system learnable: **every ladder is the same ladder, written in
another measure.** A shade means the same size of deviation whether the table is read as points, as a
ratio, as an odds ratio or as standard deviations. §2 is where that comes from.

---

## 2. What a break means

### 2.1 Every ladder is the 5-point rung, transposed

The reference ladder is `pct_diff` — 5 / 10 / 20 / 30 percentage points. Ask what the *same cell*
would print as a ratio and as an odds ratio, at a reference of p₀:

| p₀       | as a risk ratio (over side)   | as an odds ratio (over side)  |
|----------|-------------------------------|-------------------------------|
| **10 %** | **1.50 / 2.00 / 3.00 / 4.00** | 1.59 / 2.25 / 3.86 / 6.00     |
| 20 %     | 1.25 / 1.50 / 2.00 / 2.50     | 1.33 / 1.71 / 2.67 / 4.00     |
| 33 %     | 1.15 / 1.30 / 1.60 / 1.90     | 1.24 / 1.53 / 2.29 / 3.46     |
| **50 %** | 1.10 / 1.20 / 1.40 / 1.60     | **1.22 / 1.50 / 2.33 / 4.00** |
| 65 %     | 1.08 / 1.15 / 1.31 / 1.46     | 1.26 / 1.62 / 3.05 / 10.2     |

That table is the whole grammar. Read down the p₀ = 50 % row and you get, rounded, the shipped
`odds_ratio` and `pct_ratio` defaults — which is what `COLOR_SCALES$<key>$anchor` states in prose
("×1.2 = 5 points at a 50 % reference"). ⚠ **A ladder whose anchor is not recorded cannot be kept in
step with its siblings**, which is the only reason the `anchor` field exists.

### 2.2 Why the anchor is 50 %, and nearly has to be

Three facts fall out of the same table:

- **p₀ = 50 % is the only anchor at which an odds-ratio ladder is symmetric.** The logit is
  antisymmetric about 0.5, so ±*d* points give reciprocal odds ratios there and only there. That is
  why `odds_ratio` declares `sides = "mirror"` and why the declaration is sound rather than merely
  convenient.
- **An anchor below the ladder's top rung has no under side at all.** At p₀ = 10 % a −20-point cell
  is negative and the transposition is `Inf`. So the anchor must exceed the largest break (0.30) —
  which leaves 50 % as very nearly the only choice.
- **The odds ratio is the least anchor-dependent of the three.** Its first rung moves between 1.22
  and 1.33 over p₀ ∈ [20 %, 65 %]; the risk ratio's moves between 1.08 and 1.25, a factor of two.
  **A risk-ratio ladder is intrinsically base-dependent** — a fact to document, not a defect to fix.

### 2.3 The mirror is not neutral

The engine folds a multiplicative cell to a magnitude — `ratio` above the neutral, `1/ratio` below —
and mirroring a one-sided vector onto both sides *looks* symmetric. It is not:

| u (relative deviation) |  5 % | 10 % | 20 % | 30 % | 40 % | 50 % | 60 % | 75 % | 80 % |
|------------------------|-----:|-----:|-----:|-----:|-----:|-----:|-----:|-----:|-----:|
| an over cell prints ×  | 1.05 | 1.10 | 1.20 | 1.30 | 1.40 | 1.50 | 1.60 | 1.75 | 1.80 |
| an under cell prints ÷ | 1.05 | 1.11 | 1.25 | 1.43 | 1.67 | 2.00 | 2.50 | 4.00 | 5.00 |
| the mirror's bias      | 1.00 | 1.01 | 1.04 | 1.10 | 1.19 | 1.33 | 1.56 | 2.29 | 2.78 |

For the same relative deviation the under side always prints the larger number, because
`1/(1−u) > 1+u`. The bias is **negligible at the first rung (1 %) and explosive at the top (×2.8)**.
Equivalently, a mirrored threshold *t* selects `u > t − 1` above the reference and `u > 1 − 1/t`
below it:

| threshold *t*     |   1.10 |   1.20 |   1.50 |    2.00 |    4.00 |
|-------------------|-------:|-------:|-------:|--------:|--------:|
| over selects u >  | 10.0 % | 20.0 % | 50.0 % | 100.0 % | 300.0 % |
| under selects u >  |  9.1 % | 16.7 % | 33.3 % |  50.0 % |  75.0 % |

This is arithmetic, not data. It is why `sides` is a declared field: a ladder **mirrors only where
its quantity is unbounded above**. A percentage ratio is capped at `1/base`, so `pct_ratio` is
`"asymmetric"` — its under rungs are the over rungs read as the same *relative* deviation, one rung
stricter. A mean, a count and a rate have no ceiling, so `mean_ratio` mirrors. The log-odds runs to
infinity both ways, so `odds_ratio` mirrors.

### 2.4 The shape rule was already there before it was written down

Read every ladder as *"how much bigger is each rung than the previous, in the measure's own
metric"* — the value itself for an additive scale, its logarithm for a multiplicative one:

| ladder         | step ratios in its own metric |
|----------------|-------------------------------|
| `pct_diff`     | ×2.00 ×2.00 ×1.50             |
| `contrib`      | ×2.00 ×2.50 ×2.00             |
| `adj_diff`     | ×2.50 ×2.00 ×2.00             |
| `adj_diff_std` | ×2.00 ×2.00 ×2.00             |
| `odds_ratio`   | ×2.22 ×1.71 ×2.00             |
| `zscore`       | ×1.32 ×1.51 ×1.54             |

**Each rung is about twice the previous one**, ×1.5 to ×2.5 — for every ladder except the one with a
declared external convention. `zscore` is written in confidence levels (95 / 99 / 99.99 % → 1.96 /
2.58 / 3.89 / 6) and says so in its `anchor`; it is the single member of `COLOR_SHAPE_EXEMPT`.

⚠ **A ladder whose first rung is too high fires almost never, and nothing warns you.** A mean ratio
whose first rung was 1.2 left a survey mean 7.7 years (0.45 SD) below its total completely
uncoloured, because ÷1.20 sat one hundredth below the rung — while `color = "difference"` graded the
same cell at once. That is the failure mode the shape rule and the recorded anchors exist to catch.

---

## 3. Colour vision deficiency, and the OKLCH construction

### 3.1 What a palette designer needs to know

| deficiency                             | prevalence                | what collapses                                                     |
|----------------------------------------|---------------------------|--------------------------------------------------------------------|
| **protan / deutan** (red–green)        | ~8 % of men, <1 % of women | red vs green vs brown vs orange; protan also **dims reds** in luminance |
| tritan (blue–yellow)                   | ~0.01 %                   | blue vs green, yellow vs violet, yellow vs light grey, dark blue vs black |
| achromatopsia                          | ~0.003 %                  | all hue; only luminance survives                                    |

Red–green deficiency is X-linked, which is why it is overwhelmingly male, and it is by far the
dominant case. Both protan and deutan are characterised by **confusion lines** running through the
red / orange / green / brown region: colours along such a line are indistinguishable however
different their nominal hues. So any diverging palette built on red vs green collapses into an
ambiguous ramp for a sizeable fraction of readers.

### 3.2 The rules that follow, and how the package satisfies them

1. **Never red vs green as the primary axis.** The shipped diverging ramps run **cyan → blue →
   violet** on the over side and **amber → orange → red** on the under side — a blue/orange axis,
   which is the standard colour-blind-safe diverging choice and stays distinct under all three
   simulations.
2. **Vary lightness, not only hue.** Each ramp descends in L\* as it intensifies, so a reader with no
   hue perception at all still recovers magnitude. This is what makes achromatopsia and greyscale
   printing partially served for free — *partially*: see §4.1 for exactly where it stops.
3. **Meet WCAG's non-text contrast floor (≥ 3:1) for graphical objects**, and the body-text floor
   (4.5:1) for anything the reader must actually read. The greyed-out cell is deliberately held to
   the 3:1 large-text floor instead: "greyed" means *deliberately harder to read*, and it must stay
   lighter than rung 1 so the reading ladder greyed < rung 1 < rung 2 stays monotone.
4. **Never colour alone.** Significance rides on its own channel (stars, or the greying itself), and
   the footer legend states the measure in words.
5. **Test against simulations**, not intuition — `dev/color_palette_tools.R` does protan / deutan /
   tritan simulation and APCA/WCAG contrast.

### 3.3 Why the ramps are stored as OKLCH coordinates

OKLCH is perceptually uniform: equal steps in `L` are equal steps in perceived brightness, and `C`
(chroma) is separable from `H` (hue). That is what allows a ramp to be **re-tuned along one axis
without disturbing the others** — nudge a hue for tritan safety and the lightness ladder, hence the
greyscale reading and the contrast ratios, is unchanged.

`COLOR_RAMPS` therefore stores **both** the hex and the OKLCH coordinate it was picked at, side by
side, one row per `channel × theme × direction × rung`. The hex is what ships; the coordinate is what
the next person edits. ⚠ A ladder is meant to be read **down a column** of that grid — which is
exactly the reading a colour-blindness re-tuning needs.

---

## 4. The publication palettes: a page with no colour

### 4.1 The measurement: desaturation destroys direction

Convert the shipped light ramps to CIE L\*:

```text
text  over  1..4   L* 62  57  44  34
      under 1..4   L* 71  62  54  45
bg    over  1..4   L* 97  93  90  82
      under 1..4   L* 97  93  89  82
```

- **On the background channel the two directions are the same greyscale ramp.** In a greyscale print
  a strongly over-represented cell and a strongly under-represented cell are the *same shade*. The
  information is not degraded — it is gone.
- **On the text channel over-1 and under-2 are both L\* 62**, and the whole under ramp sits inside
  the over ramp's range. A reader recovers magnitude, not direction.

This is not a flaw in the colour palettes: they are tuned for hue discrimination on screen, and the
blue/orange axis was chosen deliberately for colour-vision-deficiency support. It is precisely the
reason a **separate** monochrome palette is the right answer rather than "desaturate the existing
one" — desaturation *is* the operation measured above.

### 4.2 What the publishing world actually asks for

| source                        | what it says                                                                                                  |
|-------------------------------|----------------------------------------------------------------------------------------------------------------|
| **Elsevier** author guidelines | "The use of vertical rules and shading in table cells is not recommended." **But** bold, italics, subscripts and superscripts *are* the sanctioned in-table emphasis. |
| **APA 7th**, table setup       | "Limit the use of borders, shading, or lines unless needed for clarity"; where colour is used, "use patterns along with color… ensure all users (people with color vision deficiencies **or readers printing in grayscale**) can access the content." |
| **ACM** accessibility          | An article should remain readable printed in greyscale.                                                        |
| **APA / general**              | Significance is carried by **asterisks with a footnote**, used consistently for a given α throughout the paper. |
| **Bertin** (1967/1983)         | *Value* (black/white ratio) is ordered and quantitative. *Texture/grain* is selective and associative. *Shape* and *orientation* are selective, never ordered. |

Three consequences:

1. **A shaded default would be the one thing a major publisher asks authors not to do.** The default
   must be typographic.
2. **Superscript marks are explicitly blessed**, which is what makes a repeated-mark channel
   defensible.
3. **Significance keeps its own channel.** Do not re-encode significance in a print palette: the
   package already separates magnitude (`color`) from significance (`color_signif` / `stars`), and
   that separation is exactly what the conventions expect.

⚠ Typographic emphasis is universally accepted; **shading is journal-dependent** — accepted in many
sociology and management journals, discouraged by Elsevier's generic guidance, and a copy-editing
risk because production may flatten it. Hence: default to the safe one, let the user opt into the
other.

### 4.3 Why greyscale alone cannot do it

A diverging scale needs three zones: below-neutral, neutral, above-neutral. In colour that is free —
two hues around a light neutral. In greyscale the neutral would have to be a **mid grey**, so every
uncoloured cell would carry a mid-grey fill (a fully shaded table, the thing §4.2 rules out) and the
dark end would need white text, which no other tabxplor path produces.

Bertin's alternative — texture for direction, value for magnitude (solid tints one way, hatching the
other) — is genuine, and is what cartographers do. Excel supports it (`pattern_type`) and CSS can do
it with `repeating-linear-gradient`. But hatching **behind digits** hurts legibility at table sizes,
does not survive an HTML → Word paste, and is invisible in markdown. **Available, not recommended.**

Which leaves the working principle the publication palettes are built on:

> **An ordered channel (grey value, an emphasis ladder, or a repeated mark) carries MAGNITUDE.
> A selective channel (bold vs italic, underlined vs not) carries DIRECTION.**

⚠ And its corollary, which is why there is more than one publication palette: **a table whose cells
already print their own direction glyph** (every `tab_reg()` measure prints a `+/−` or a `×/÷`) has
direction spoken for, and can spend the whole typographic budget on magnitude. That is the difference
between `print_minimalistic` and `print_emphasis`, and it is why `theme = "print_ready"` chooses from
what the table *is* rather than from a preference.

### 4.4 What each medium can actually express

| attribute            | HTML/CSS               | Excel (openxlsx2)              | pandoc md → Word                    | HTML → Word (paste)          |
|----------------------|------------------------|--------------------------------|-------------------------------------|------------------------------|
| **bold**             | ✓                      | ✓                              | ✓ `**x**`                           | ✓                            |
| **italic**           | ✓                      | ✓                              | ✓ `*x*`                             | ✓                            |
| **underline**        | ✓                      | ✓ `u = "single"`               | ~ `<u>` or a `.underline` span      | ✓                            |
| **double underline** | ✓                      | ✓ `u = "double"`               | ✗ (needs a reference.docx style)    | ✓                            |
| **strikethrough**    | ✓                      | ✓                              | ✓ `~~x~~`                           | ✓                            |
| **superscript mark** | ✓ `<sup>`              | ✓ `vertAlign`                  | ✓ `^+^`                             | ✓                            |
| grey text            | ✓                      | ✓                              | ✗ (span class dropped)              | ✓                            |
| grey cell fill       | ✓                      | ✓                              | ✗                                   | ✓ (cell shading survives)    |
| pattern fill         | ✓                      | ✓                              | ✗                                   | partial                      |
| font size            | ✓                      | ✓                              | ✗                                   | ✓ — **breaks table alignment; rejected** |

Two readings matter:

- **The portable core is bold + italic + underline + superscript.** All four survive every route,
  including plain markdown into Word.
- **Word is reached by paste, not by a writer.** There is no `.docx` exporter. Pasting the HTML table
  into Word preserves character formatting *and* cell shading; pandoc markdown → docx preserves only
  what markdown can say. **So investing in the HTML rendering is what buys Word**, and the markdown
  path should degrade to the portable core rather than be designed for.

### 4.5 Marks and stars are alternatives, never companions

A repeated mark (`77 %⁺⁺` / `31 %⁻`) is the strongest channel available on paper: ordered by
definition, explicit about direction with no legend to learn, portable through every medium
including plain text, and **the only option a screen reader can read aloud**. Its one real cost is
competition — `77 %⁺⁺***` is two trailing symbol runs meaning two different things.

That has a clean resolution rather than a compromise, because significance already has two mutually
exclusive spellings. The documented recommendation is **`print_marks` + `color_signif`, or `print` +
`stars`** — never both symbol runs at once. In a print palette the greying is the better fit anyway:
greyed-out text for a non-significant cell is the one thing greyscale does perfectly.

---

## 5. Re-running the evidence

- `dev/color_palette_tools.R` — interactive OKLCH palette design and review, CVD simulation,
  contrast measurement, `darken_for_legend()`. The sole reason `farver` and `colorspace` are not
  Imports.
- `dev/breaks_balance_probe.R` — what the ladders actually do on real tables (share of cells at each
  intensity, per ladder). ⚠ Its header is right: **re-run it after any change to `color_breaks`.**
- `dev/make_legend_preview.R` — the legend palette as bold text on white, with APCA Lc.
- `dev/verify_color_attrs.R` — the characterization net for the colour *resolver*: `save <f.rds>`
  before a refactor, `check <f.rds>` after.
- `dev/make_color_golden.R` — regenerate the colour characterization fixtures when an output change
  is intended.

## 6. References

**Colour vision.** Brettel, Viénot & Mollon (1997), *Computerized simulation of color appearance for
dichromats*, JOSA A 14(10). · Viénot, Brettel & Mollon (1999), *Digital video colourmaps for checking
the legibility of displays by dichromats*, Color Research & Application 24(4). · Okabe & Ito, *Color
Universal Design* (the vermillion / blue / bluish-green / reddish-purple set). · W3C, *WCAG 2.2*
§1.4.11 Non-text Contrast (3:1) and §1.4.3 Contrast (Minimum) (4.5:1).

**Colour spaces and palettes.** Ottosson (2020), *A perceptual color space for image processing*
(Oklab / OKLCH). · Brewer, *ColorBrewer* and *Designing Better Maps*. · Zeileis, Hornik & Murrell
(2009), *Escaping RGBland: Selecting colors for statistical graphics*, CSDA 53.

**Typography and tables.** Bertin (1967/1983), *Sémiologie graphique* / *Semiology of Graphics* (the
visual variables, and which are ordered vs selective). · Tufte (1983), *The Visual Display of
Quantitative Information*. · APA (2020), *Publication Manual*, 7th ed., ch. 7. · Elsevier, author
guidelines for tables. · ACM, accessibility recommendations for authors.
