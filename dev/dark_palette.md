# Designing the dark palettes

How tabxplor's dark text and background ramps were designed, what method turned out to be the right
one, and what shipped. It is a record, not a brief: the exploration is closed and the ramps are in
`COLOR_RAMPS` (`R/tab-palettes.R`). Read it before changing them, and re-run the preview before
trusting any number here.

The colour system itself — what a break means, the 5-point rung, the publication palettes — is
`dev/colors.md`, and is not repeated. What this file holds is the part that could not be derived
from it: **a dark theme is not a light theme inverted**, and the reasons are all in the sRGB gamut.

## 1. What the palettes must achieve

1. **A coloured cell reads as more present than a plain one.** Colour marks a deviation worth
   noticing; a cell that recedes relative to its neighbours inverts the message.
2. **Four rungs per side, separable at a glance**, and ordered: rung 4 must read as stronger than
   rung 3, not merely different.
3. **Direction by hue, magnitude by chroma — and hue direction is not negotiable.** Cyan → blue →
   violet is over, amber → orange → red is under, in both themes. The association is cultural, and a
   palette that swapped it between themes would teach two things at once.
4. **One colour per rung.** The legend prints the rungs as words and swatches, so a rung cannot have
   one colour in the open and another inside a fill. This is what makes the dual channel (§6) hard.
5. **Colour-blind safe.** The blue/orange axis of `dev/colors.md` §3 is not negotiable.
6. **Degrade to the publication palettes.** That path exists; the dark palette only has to not make
   it worse.
7. **Survive a global chroma cap** (`dev/chroma_cap.js`), and survive a range of grounds: the table
   paints none of its own.

## 2. The methodology that worked

Six things, in the order they mattered. The first three are measurement discipline and cost nothing;
the last three are where the design actually happened.

1. ⚠ **Measure text in APCA Lc, never in WCAG ratios.** WCAG 2.x overstates contrast for dark
   colours badly enough that its own authors say it cannot guide a dark theme. The shipped-in-2.0.0
   dark ramp cleared 3:1 against its page at every rung and was still, in APCA, below the floor for
   *large* text. Bars: **90** fluent body · **75** body columns · **60** content · **45** headlines
   · **30** spot-readable · **15** the threshold of discernible.
2. ⚠ **Measure fills by lightness distance from the page, never in APCA.** APCA is a *text* model
   and clips below Lc 10, so it reads 0 for a fill that is plainly visible — including for the light
   theme's own fills. A fill is judged on `dL` from the page, with WCAG beside it for the record.
3. ⚠ **Measure against the right reference, and take every bar from the light theme.** A plain table
   figure is `tx_chrome_hex(theme)$text`, not the aside `grey2` — an early draft of this file used
   the aside and understated the fault throughout. And every threshold below is the light theme's
   own achieved number, not a standard: fills sit dL 0.029 from their page, adjacent rungs 0.030
   apart, the weakest step between text rungs is 0.075, the colour-blind gap 0.176. "No worse than
   light" is the only bar that means anything here, and for the dual channel it is a *low* one (§6).
4. **Read the gamut before choosing a colour.** Every difficulty in this file is one fact: chroma is
   bounded, and the bound depends on both L and H (§4). Design against the ceiling map, not against
   a colour picker.
5. **State chroma as a fraction of the ceiling, and read every coordinate back off the hex.** Asking
   for an absolute chroma silently gets clamped; `oklch_hex(0.90, 0.14, 270)` returns a colour of
   chroma 0.048 without complaint. A ramp must never record a coordinate it does not have.
6. **Generate widely, then judge by eye.** ~70 candidates were generated across four families and
   five lightness shapes, each scored on five checks and rendered as real tables. The generator's
   value was in *ruling things out* and in pricing each trade; **the palette that shipped was tuned
   by hand**, on impressions, and then verified against the same measurements. Neither half would
   have got there alone.

**What did not work, and is worth not repeating.** Optimising for a scalar: left to maximise chroma
a search drives lightness to where chroma lives and runs each ramp backwards, because nothing in the
objective knows what the colours *mean*. Mirroring the light palette: §4 shows why no single
transform of it exists. And optimising the hues: because the dark ceiling is flat (§4), moving rung
4 from h270 to h285 gains 5 % — effort belongs on the lightness band, not the hue path.

## 3. The tools

- **`dev/palette_preview.R` → `dev/palette_preview.html`** — the page the choice was made on. Text
  and fills are independent axes (`data-text` / `data-bg`), so any ramp can be seen under any fill,
  which is the independence the two channels have in the package. It carries a comparison strip, the
  candidate on real tables, deuteranope and protanope copies, a picker for five plausible grounds,
  the chroma cap, and a per-candidate scorecard with the chroma-vs-lightness curve for its own hues.
- **`dev/heading_ladders.R`** — the OKLCH maths, base R only: `oklch_hex()` (gamut-mapped by
  reducing chroma alone), `oklch_maxC()` (the ceiling, memoised), `oklch_cusp()` (where a hue
  peaks), `hex_oklch()` (the read-back), `oklch_ramp()` / `oklch_ladder()`, `contrast()` and
  `apca()`.
- **`dev/color_palette_tools.R`** — CVD simulation, APCA, text × background grids. ⚠ Its
  `.cg_apca()` and `heading_ladders.R`'s `apca()` are deliberate duplicates: the latter file is
  base-R only. Both are checked against the same Myndex vector (`#888` on `#fff` → 63.1).
- **`dev/chroma_cap.js`** — caps every colour on a page at once; goal 7's test.
- **`dev/make_color_golden.R`** — regenerates the 15 `_color_golden` fixtures. Any ramp edit moves
  them, and the diff must be argued.

## 4. What the gamut decides

**Max chroma for a hue sits at a lightness — its cusp — that varies enormously**, and both families'
cusps descend from their faint end to their strong end:

| cool | h196  | h210  | h230  | h250  | h270  |   | warm | h100  | h80   | h60   | h40   | h25   |
|------|-------|-------|-------|-------|-------|---|------|-------|-------|-------|-------|-------|
| L    | 0.90  | 0.84  | 0.76  | 0.66  | 0.46  |   | L    | 0.91  | 0.82  | 0.76  | 0.68  | 0.63  |
| C    | 0.153 | 0.145 | 0.152 | 0.187 | 0.304 |   | C    | 0.189 | 0.171 | 0.177 | 0.213 | 0.255 |

Three consequences, and between them they are the whole design.

1. **The light theme's hue order tracks that ridge downward, which is why it works on white.** As
   cyan → blue → violet darkens it moves into hues that hold progressively more chroma, so lightness
   and saturation intensify together and *the ceiling itself supplies the ladder*: the light ramps
   sit at 99–100 % of the ceiling at **every rung** and get ×2.7 of chroma without choosing
   anything.
2. ⚠ **On a dark page that free ladder is gone.** Once every rung must stay legible, the ceiling is
   **flat** along the whole forward path — cool 0.145–0.176, warm 0.171–0.211 — so it supplies no
   ladder at all. A dark ramp has to *manufacture* one, by holding its faint rungs below a ceiling
   they could have reached. **This is the one structural difference between the two themes, and it
   is why no single transform of the light palette produces the dark one.**
3. ⚠ **Floor and ramp are therefore in direct conflict, and the bound is exact.** Cyan tops out at C
   0.147 and only at L 0.86 — it cannot hold 0.15 at *any* lightness. Rung 4 held to Lc 42 reaches
   0.171 at h270. So `floor × ramp ≤ ~0.18`: a floor of 0.09 allows ×2.0, a floor of 0.13 allows
   ×1.37, and a floor at cyan's own ceiling allows ×1.21 with rung 1 pinned at L 0.86.

**Two floors squeeze the text ramp from both ends, and both are measured.**

- **Rung 1 needs chroma ≥ 0.060 or its direction dies — and ⚠ PROTANOPIA sets the floor, not
  deuteranopia.** At L 0.70 a cyan and an amber both at C 0.055 are 0.082 apart to a deuteranope,
  over the 0.08 bar, but only 0.076 to a protanope; at C 0.060, 0.089 and 0.083. Test both, take the
  minimum. ⚠ Light's own rung 1 manages 0.225, which at this lightness would need C 0.15 — the
  ceiling. So a dark ramp cannot have light's rung-1 separation *and* a ladder. A cost of the theme.
- **Rung 4 needs L ≥ 0.70 to clear the greyed cell — and ⚠ not because of its OKLab lightness**: a
  saturated colour is darker than a neutral at the same L, so the warm rung at L 0.68 reads Lc 41
  where a neutral there reads 44. State the depth in L; measure it in Lc.

**A hue gap only separates two colours if both carry enough chroma.** Two pale tints thirty degrees
apart are two pale tints. Measured, raising the chroma floor from 0.060 to 0.11 roughly doubles the
colour-blind gap (0.083 → 0.17) — chroma is what makes a hue readable, in both senses.

⚠ **And the light theme's hue gaps are uneven in the wrong direction**: 205/235/255/270 is
30/20/15°, smallest exactly where the chroma gaps are smallest too. The dark ramp evens them to 30°
(200/230/ 260/290) and 20° (80/60/40/25), which is worth 0.014 of the weakest step between rungs.

## 5. The result

**The dark text ramp is an arch**: rungs 1–3 climbing in lightness, rung 4 dropping back to collect
the chroma its hue only holds lower down. The light ramp descends throughout. Both rise in chroma.

| theme | over rungs                            | under rungs                           |
|-------|---------------------------------------|---------------------------------------|
| light | `#02a5b3 #0891c9 #0267c7 #300dfd`     | `#dca331 #de7c01 #dd5301 #d60103`     |
| dark  | `#2ba1a7 #37a8d7 #72a7ff #9c84ff`     | `#d6a13d #ec923e #ff885e #ff635f`     |

|                        | light                     | dark                      |
|------------------------|---------------------------|---------------------------|
| lightness, over        | 0.66 0.62 0.52 0.47 ↓     | 0.65 0.69 0.73 0.69 ⌒     |
| lightness, under       | 0.75 0.68 0.62 0.55 ↓     | 0.74 0.74 0.75 0.70 ⌒     |
| chroma, over           | 0.112 0.130 0.169 0.300   | 0.100 0.120 0.141 0.176   |
| chroma, under          | 0.140 0.160 0.187 0.225   | 0.130 0.145 0.155 0.191   |
| chroma ramp            | ×2.7 / ×1.6               | ×1.8 / ×1.5               |
| \|Lc\| against its page | 44 – 85                   | 41 – 54                   |
| the plain figure       | Lc 106                    | Lc 94                     |
| the greyed cell        | Lc 57                     | Lc 40                     |
| weakest step, rung↔rung | 0.075                     | 0.050                     |
| colour-blind gap       | 0.176                     | 0.153                     |

**How to read that comparison.** The dark ramp is *flatter in legibility and shallower in chroma*,
and both are the gamut's doing rather than a compromise that could be tuned away. Its |Lc| spans 13
points where light spans 41, because on white a ramp may run from Lc 44 to 85 while staying inside
one hue family, and on a dark page the same hues cannot: rung 4's violet does not exist at high
lightness, and rung 1's cyan does not exist at low chroma. What the dark ramp buys with that
flatness is that **nothing recedes** — every rung sits between Lc 41 and 54, comfortably clear of
the greyed cell at 40, where the palette this replaces ran 25–44 with its faintest rung *below* the
greyed one. ⚠ The one place light is genuinely better is the weakest step (0.075 against 0.050); the
dark ramp compensates with the even hue gaps, and it was judged acceptable on real tables, not on
this number.

**The dark fills are light panels, not tints — and that is the inversion that matters.**

| fills | lightness             | chroma                  | dL from page          | WCAG        |
|-------|-----------------------|-------------------------|-----------------------|-------------|
| light | 0.97 0.94 0.91 0.85   | 0.030 0.033 0.045 0.074 | 0.029 0.061 0.091 0.150 | 1.08 – 1.63 |
| dark  | 0.91 0.88 0.85 0.82   | 0.043 0.055 0.071 0.094 | 0.652 0.620 0.587 0.553 | 8.37 – 12.13 |

`#c3ecee #b4e0f6 #b3cffd #c1b9fc` over, `#f3e0c2 #f6d0b2 #fabda8 #fcaaa3` under.

⚠ **The obvious design — a tint of the dark page, mirroring light's tints of white — was tried and
is wrong.** The palette this replaces did exactly that, at L 0.25–0.35 against a page at 0.263, so
rung 1 was *darker* than the page (dL 0.012) and read as a hole rather than a mark. The dark fills
instead sit far **above** the page: an order of magnitude more distant than light's, WCAG 8–12
rather than 1.1–1.6. They are a different kind of object — panels, not tints — and they carry more
chroma than light's precisely because a fill this far from the page can afford it.

## 6. The dual channel, and the two rules it needed

A cell may carry a text colour, a fill, or both, and the two channels carry **different measures**
(`color = c("difference", "ratio")`), so they move independently: any of the 8 text colours can land
on any of the 8 fills. A fill must clear **every** text colour at once. This is the hardest
constraint in the design, and it needed two changes — one of which improved the light theme too.

**1. The loud breaks, drawn with the faint slots.** `COLOR_SCALES$bg_keep = 2` keeps two of a
ladder's four rungs on the fill channel. ⚠ **Which breaks survive and which colours draw them are
two independent choices, and only the second changed.** The breaks are still the LOUD ones — of
four, 3 and 4 — so a fill fires exactly where it always did; they are now drawn with palette slots
**1 and 3** instead of 3 and 4 (`R/fmt_class.R`, the `trim()` in `fmt_color_plan()`). On `pct_ratio`
the legend therefore still reads ×1.5 and ×2 over, ÷2 and ÷4 under, in the two quietest fills.

The reason is one thing only: drawing them in the two loudest fills put the darkest one under the
text channel's own colour. Since only two steps are ever shown, the quieter slots say exactly the
same thing — a two-step ladder either way. ⚠ **It applies to both themes**, being about slots and
not hexes, and the light theme gains more than the dark:

| worst text-on-fill | slots 3 & 4 (before) | slots 1 & 3 (now) |
|--------------------|----------------------|-------------------|
| light              | Lc 14                | **Lc 26**         |
| dark               | Lc 9                 | **Lc 16**         |

⚠ **Lc 14 is what the light theme shipped with**, which is worth sitting with: the dual channel was
already weak in what the package had, and nobody had measured it. Both themes are now above APCA's
threshold of discernible, and light is comfortably so.

**2. A fill with no text colour repaints its ink.** `tx_chrome_hex(theme)$on_fill` is the ink a cell
takes when it carries a fill and no text colour of its own. `NA` everywhere the fills are a tint of
the page — every theme but dark — and `#21252b` on dark, which is the page's own ground, so a filled
cell reads as the page showing through. ⚠ Without it the dark theme cannot ship: `#f1efe0` on a
light panel measures **APCA Lc 0**, unreadable rather than merely faint. With it, Lc 72–87, against
the light theme's 88–101 for black on its own fills.

⚠ **This is the one rule in the package that repaints a text slot**, so it is written to reach only
what has **no** text class — every coloured rung keeps its single legend colour, and goal 4 holds.

⚠ **And the footer legend needs it as much as the cells do**: a break-word nobody can read is a
break-word that names nothing. Its swatches are `<span class="o3">×1.5</span>`, not `.tx-pill`, so
the rule takes two selector families — the exclusion sits on the *ancestor* for an html cell (`<td
class="p4"><span class="tx-pill o3">`) and on the *same element* for a markdown cell (`[42%]{.p2
.o1}`) and for a legend swatch in either medium. Stated once in the chrome, and restated at the
writers that have no selectors at all:

| medium               | how a legend swatch is drawn         | what carries `on_fill`   |
|----------------------|--------------------------------------|--------------------------|
| html (stylesheet)    | `<span class="o3">`                  | `tx_css_rules()`         |
| html (no stylesheet) | inline `background-color`            | `tab_color_legend()`     |
| markdown             | `[×1.5]{.o3}` + the stylesheet       | `tx_css_rules()`         |
| console              | an ANSI **background** pair          | `tab_color_legend()`     |
| Excel, plot          | ink from `bg_legend`, no fill at all | nothing — none is needed |

The last row is why `bg_legend` exists (`dev/colors.md`): a run carries a font colour and cannot
fill, so there the break-word is already ink on the page and reads at Lc 65–88. ⚠ The console is the
one that looks safe and is not — `make_ansi_style(bg = TRUE)` paints a real fill and leaves the
terminal's own foreground on top of it. Cells reach the same ink through `fmt_channel_codes()`.

## 7. What is left open

- **A fading veil behind the digits** was built and measured — a horizontal `linear-gradient`
  lifting the fill's centre while its margins keep their chroma — and reached Lc 24–26. It was
  rejected on looks: it costs too much of the fill's own colour. ⚠ Two things it taught, if it is
  ever revisited: a *radial* gradient sized in percentages never reaches its last stop inside its
  own box (an `ellipse 74%` still carries two thirds of its alpha at the left edge, so it renders
  flat); and a gradient survives html and nothing else, where a flat alpha has a solid equivalent
  Excel and the console could be given.
- **`bg_legend_dark` fixed itself.** It aliases the dark *fill* ramp, and those hexes draw the
  legend's break-**words**. With the old near-black fills they measured Lc 0 on the dark page —
  invisible. With light panels they measure 65–88. No code changed; the bug was a consequence of the
  fills being wrong.
- **The dark ramps were tuned against `#21252b`** (`tx_chrome_hex("dark")$bg`, Atom One Dark's
  deeper shade, also the site's). ⚠ That ground is a **fallback**, not a fixture: `tab_css()` writes
  `background:transparent` for the colour themes, so in an `.Rmd`, a `.qmd` or on the site the table
  follows the page and the hex is never seen. The ramps were checked from L 0.20 to 0.30 and hold
  across it; a much lighter "dark" page is out of scope.
- **No test pins the dark ramps by hex**, but 15 `_color_golden` fixtures embed them, so any edit
  moves those and the diff must be argued. `pkgdown/index.md` carries a pasted `tab_css()` dump and
  is regenerated from `pkgdown/index.Rmd`.

## 8. References

- Ottosson, *A perceptual color space for image processing* (OKLab/OKLCH).
- Machado, Oliveira & Fernandes (2009), the CVD model behind `colorspace`.
- WCAG 2.1 §1.4.3 and §1.4.11 — and [Why APCA](https://git.apcacontrast.com/documentation/WhyAPCA)
  on why they do not carry to a dark theme, with [the Lc
  bars](https://git.apcacontrast.com/documentation/APCA_in_a_Nutshell.html).
- [Chromostereopsis](https://en.wikipedia.org/wiki/Chromostereopsis), and the accommodation cost of
  saturated short-wavelength text on a dark ground.
- [Chameleon: automated palette adaptation for dark-mode
  dataviz](https://arxiv.org/html/2512.00516v1)
- [WebAIM: contrast and colour accessibility](https://webaim.org/articles/contrast/)
