# Designing the dark over / under palette

A working brief for the dark diverging ramps of `COLOR_RAMPS` (`R/tab-palettes.R`): what the palette
has to achieve, what human perception and the sRGB gamut actually allow, which constraints bind and
which can be loosened where, and what to explore with. It deliberately proposes no palette — the
exploration is open.

The colour system itself (what a break means, the 5-point rung, the publication palettes) is
`dev/colors.md` and is not repeated here.

## 1. What the palette must achieve

Seven goals. The first is the one the shipped dark ramps fail, and it is the reason for this file.

1. **A coloured cell must read as more present than a plain one.** Colour marks a deviation worth
   noticing; a cell that recedes relative to its uncoloured neighbours inverts the message.
2. **Four rungs per side, separable at a glance**, and monotone: rung 4 must read as stronger than
   rung 3, not merely different.
3. **Direction by hue, magnitude by lightness.** Over and under are told apart by the hue family;
   how far the deviation goes must survive the loss of hue (see §3).
4. **One colour per rung.** The legend prints the rungs as words and swatches, so a rung cannot have
   one colour in the open and another inside a fill. This is a hard constraint, and it is what makes
   the two-channel problem in §5 interesting rather than trivial.
5. **Colour-blind safe.** The blue/orange axis of `dev/colors.md` §3 is not negotiable: red-vs-green
   as the primary axis is out.
6. **Degrade to the publication palettes** — greyscale and typographic. That path already exists;
   the dark palette only has to not make it worse.
7. **Survive a global chroma cap.** The site theme can desaturate a whole page (`dev/chroma_cap.js`);
   a palette that only works at full chroma is fragile.

## 2. What is wrong with the shipped ramps

Measured against each theme's own page and its own ink:

| theme | text rung 1                             | text rung 4           | fills                 |
|-------|-----------------------------------------|-----------------------|-----------------------|
| light | L .66 · 3.0:1 page · **7.0:1 from ink** | L .47 · 8.1:1 · 2.6:1 | L .97–.85 · 1.1–1.6:1 |
| dark  | L .55 · 3.3:1 page · **2.8:1 from ink** | L .66 · 4.8:1 · 2.0:1 | L .25–.34 · 1.0–1.3:1 |

Page: light `#ffffff`, dark `#21252b` (L 0.263). Ink: light `#000000`, dark `#CDCBBC` (L 0.840).

- ⚠ **Every dark rung is dimmer than the ordinary ink**, so goal 1 fails at every rung. In light mode
  the ramp moves *away* from the ink into colour; in dark mode it moves *toward* the page.
- **The dynamic range is halved**: light runs 2.3:1 → 8.1:1 against its page (a factor of 3.5), dark
  runs 3.2:1 → 5.3:1 (1.6). Less room means rungs that are harder to separate.
- **The fills are darker than the page** (L 0.25 against 0.263); rung 1 measures 1.0:1 and cannot be
  seen. They read as holes rather than as marks.

The light palette on a dark page has exactly one flaw of its own, which is worth knowing because it
looks like a solution: its lightness **descends** as the deviation grows, so on a dark ground the
faintest rung is the most visible.

## 3. How perception constrains this

- **Luminance carries legibility; hue and chroma barely do.** For text, contrast is a lightness
  question. Hue and saturation add identity, not readability.
- **The reference is the surrounding ink, not the page.** A colour can clear the page comfortably and
  still read as recessive because everything around it is brighter. This is fault 1, and it is why
  "contrast against the background ≥ 4.5:1" is necessary but not sufficient here.
- **Dark grounds exaggerate saturation.** The same hex reads as more saturated on dark than on white,
  and a highly saturated colour can vibrate against it. Dark palettes want roughly **15–25 % less
  chroma** than their light equivalents for the same felt vibrancy — which is a licence, not a
  penalty: it buys lightness back (§4).
- **A low-lightness colour on a near-black page reads muddy**, whatever its chroma. Darkness and
  saturation do not substitute for each other on this side.
- **Area changes the rules.** A filled shape needs ~3:1 where text needs 4.5:1, and a large area can
  carry a chroma difference that thin text cannot. The two channels therefore should *not* be tuned
  alike.
- **Under red–green deficiency (~8 % of men) only the lightness ladder survives.** Whatever encodes
  magnitude must be visible in lightness alone; hue may carry direction, never amount.

## 4. What the OKLCH space allows

OKLCH separates lightness, chroma and hue, but **chroma is bounded, and the bound depends on both L
and H**. That single fact drives every difficulty here.

Max sRGB chroma at the hues this palette uses:

| L    | h205  | h235  | h255  | h270  |   | h80   | h60   | h42   | h29   |
|------|-------|-------|-------|-------|---|-------|-------|-------|-------|
| 0.90 | 0.105 | 0.057 | 0.049 | 0.048 |   | 0.094 | 0.065 | 0.055 | 0.052 |
| 0.80 | 0.137 | 0.119 | 0.103 | 0.100 |   | 0.166 | 0.140 | 0.120 | 0.115 |
| 0.70 | 0.120 | 0.149 | 0.160 | 0.156 |   | 0.145 | 0.164 | 0.199 | 0.191 |
| 0.60 | 0.103 | 0.127 | 0.198 | 0.217 |   | 0.124 | 0.141 | 0.181 | 0.246 |

Three consequences worth internalising before touching a number:

1. **The rung-1 hues peak high and the rung-4 hues peak low.** Cyan and amber hold their most chroma
   around L 0.80–0.85; violet and red around L 0.55–0.60, and are nearly grey by L 0.90.
2. ⚠ **A ramp that rises in lightness therefore inverts its own chroma ladder.** Ask for equal chroma
   at every rung on a rising ramp and the gamut will silently give rung 4 half of rung 1's. This is
   arithmetic, not tuning, and it is the trap this palette falls into most easily.
3. **The ceiling has a local minimum around h 260–275** — exactly where a cyan→violet ramp ends — and
   climbs again past it: at L 0.74 it is 0.133 at h270, 0.140 at h285, 0.159 at h300. The warm side
   is flat by comparison: 0.226–0.229 anywhere from h20 to h35.

**The trade curve.** Everything else reduces to one exchange — how bright rung 4 is allowed to be
against how much chroma it can hold:

| rung 4 sinks to  | h270 | h285 | h300 | h29  |
|------------------|------|------|------|------|
| L 0.84 (the ink) | .079 | .083 | .094 | .088 |
| L 0.80           | .100 | .105 | .119 | .115 |
| L 0.76           | .122 | .128 | .145 | .143 |
| L 0.72           | .145 | .152 | .172 | .175 |
| L 0.68           | .168 | .177 | .200 | .209 |

Keeping rung 4 as bright as the ink costs about two thirds of its chroma; every 0.02 of lightness
given back buys roughly 0.011. Where to sit on that curve is the design decision, and it is a
judgement about the page, not a computation.

## 5. The two-channel constraint

A cell may carry a text colour, a fill, or both — but they live on different elements, and goal 4
says a rung has one colour. So the text ramp has to work **in the open on a dark page and on top of
whatever the fills are**, which couples the two ramps: light fills push the text ramp down into the
middle band, dark fills let it stay high.

The one inversion available is the ink of a cell that has a fill and **no** text slot, which
otherwise takes the page's own ink. It is expressible in both html and markdown, though not with the
same selector:

| medium   | markup                                          | selector for "a text slot inside a fill" |
|----------|-------------------------------------------------|------------------------------------------|
| html     | `<td class="p2">` + `<span class="tx-pill o3">` | `td.p2 > .tx-pill`                       |
| markdown | `[75%]{.p2 .o3}` — one span, both classes       | `.p2.o3`                                 |

⚠ Excel and the console have neither: `tab_xl()` writes a hex per cell and the console writes an ANSI
pair, so any rule of this kind has to be restated in each writer. A palette that needs no such rule
is cheaper to ship by a wide margin.

## 6. Where a constraint can be loosened, and what it costs

Each of these buys something real; none is free, and the sizes are measured.

- **Move one rung's hue a few degrees.** Rung 4 at h285 instead of h270 gains ~5 % chroma at the same
  lightness; h300 gains ~19 %. The cost is identity: the ramp drifts from violet toward purple, and
  the hue gap to its neighbour narrows unless the whole ramp is respaced.
- **Let a single rung leave the lightness band** instead of tilting the whole ramp. The band's job is
  to keep the ramp above the muddy zone; only the last rung needs the chroma that requires depth.
- **Lower rung 1's chroma deliberately.** A ladder is read as a *ratio*, not as absolute saturation.
  Pulling rung 1 well below its own ceiling makes rung 4 read as strong without rung 4 moving at all
  — and §3 says a dark ground needs less chroma anyway.
- **Widen or narrow the hue span.** More degrees between rungs buys separability that lightness then
  does not have to provide; too many and the two families start to meet.
- **Treat the fills as a different problem.** They need 3:1, not 4.5:1, and they may be far lighter or
  far fainter than the text rung of the same name (§3, area).
- **Out of scope but worth naming as levers**: the page (L 0.263) and the ink (L 0.840) define both
  "invisible" and "recessive". Moving either moves every threshold in this document.

## 7. The tools

- **`dev/palette_preview.R` → `dev/palette_preview.html`** — real tables on the real dark chrome:
  text channel only across four row and four column variables (so every rung of both directions
  appears), both channels together, background channel alone, and a greyed table where the ladder is
  read beside a non-significant grey. A palette menu, the whole selected scale in the sidebar as
  OKLCH plus contrast, the colour-vision copies folded under each table, and a global chroma cap.
  The candidates it carries are **probes, not proposals** — read them as points already sampled.
- **`dev/heading_ladders.R`** — `oklch_hex()` (gamut-mapped by reducing chroma only), `oklch_maxC()`
  (the ceiling at a given L and H), `in_gamut()`, `contrast()`. This is what makes an exploration
  numerical rather than a guess.
- **`dev/color_palette_tools.R`** — CVD simulation and APCA contrast, text × background grids.
- **`dev/chroma_cap.js`** — caps every colour on a page at once; goal 7's test.

**Two rules the tooling enforces, and both were learnt the hard way.** A ramp never records a chroma
it does not have: every rung is clamped to its ceiling, the achieved value is what is displayed, and
clamps are reported. And a ramp whose achieved chroma falls as the rung rises is refused, because
that inverts the ladder and is invisible in the asked-for numbers.

## 8. Traps

- ⚠ **Silent clamping.** `oklch_hex(0.90, 0.14, 270)` returns a colour of chroma 0.048 without
  complaint. Always compare against `oklch_maxC()`.
- ⚠ **CVD matrices are defined in linear RGB.** Applying them to gamma-encoded values looks plausible
  and is wrong. And in R, `pmax(0, m)` drops a matrix's dimensions where `pmax(m, 0)` keeps them.
- ⚠ **An optimiser will not respect meaning.** Left to maximise chroma, a search runs the warm ramp
  backwards (amber becomes rung 4) because the ceiling peaks at h100, and drives lightness to 0.66
  because that is where chroma lives. Hue direction and a lightness floor have to be constraints, not
  hopes.
- ⚠ **A single pair of variables is no test.** `race × party3` reaches three rungs with one cell
  apiece; judging a ladder needs a table that actually contains it.

## 9. What shipping an answer costs

- **The palette is 16 rows** of `COLOR_RAMPS` — hex plus the OKLCH coordinate it was picked at, which
  is what the next person edits.
- **A rule that repaints an ink costs more than a palette**: one selector per medium in
  `tx_css_rules()` (`R/tab-css.R`), and the same decision restated in `tab_xl()` and the console.
- ⚠ **No test pins the dark ramps.** Nothing in `tests/` names a dark hex and the goldens carry the
  light theme only. A change breaks nothing — and nothing would catch a mistake. The cheap guard is
  one golden case rendered at `theme = "dark"`.

## 10. References

- Ottosson, *A perceptual color space for image processing* (OKLab/OKLCH).
- Machado, Oliveira & Fernandes (2009), the CVD simulation model behind `colorspace`.
- WCAG 2.1 §1.4.3 and §1.4.11 (4.5:1 for text, 3:1 for graphical objects); APCA for the WCAG 3 draft.
- [Chameleon: automated palette adaptation for dark-mode dataviz](https://arxiv.org/html/2512.00516v1)
- [Why dark mode colours need more saturation than you think](https://colorarchive.org/notes/may-2026-dark-mode-saturation/)
- [WebAIM: contrast and colour accessibility](https://webaim.org/articles/contrast/)
