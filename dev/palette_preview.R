# PURPOSE: choose tabxplor's DARK palettes by eye, the way the theme was chosen.
# ROLE: dev tool, .Rbuildignore'd.  Rscript dev/palette_preview.R -> dev/palette_preview.html
#
# ✓ THE TEXT PALETTE IS SETTLED: `tuned-by-hand-3`. The exploration that reached it is recorded in
#   dev/dark_palette.md; the runners-up are kept below only so the choice can be re-read against
#   them. THIS PAGE IS NOW ABOUT THE BACKGROUND CHANNEL.
#
# ⚠ MEASURED IN APCA Lc for text, and in OKLab LIGHTNESS DISTANCE for fills -- and the second is not
#   pedantry. APCA is a text model and clips below Lc 10, so it reads 0 for a fill that is plainly
#   visible; it reads 0 for the LIGHT theme's own fills too. A fill is judged against the light
#   theme's own numbers instead: its fills sit dL 0.029-0.150 from the page, WCAG 1.08-1.59, with
#   adjacent rungs 0.030-0.059 apart.
#
# THE BACKGROUND PROBLEM, AND WHY THE DUAL CHANNEL IS THE WHOLE OF IT. Text colour and fill carry
# DIFFERENT measures (`color = c("difference", "ratio")`), so they move independently: any of the 8
# text colours can land on any of the 8 fills. A fill must therefore clear EVERY text colour at once,
# and the text ramp is fixed at L 0.649-0.751. That single band is what both options below negotiate.
#
#   A. FILLS DARKER THAN THE TEXT -- the faithful mirror of the light theme. Its fills sit 0.03-0.15
#      BELOW white; these sit 0.04-0.17 ABOVE a page at L 0.263. Text stays readable on them (worst
#      Lc 30), a bare fill keeps the page's own ink (Lc 82), and no new CSS rule is needed anywhere.
#      ⚠ The shipped fills are the same idea done backwards: L 0.25-0.35 against a page at 0.263, so
#      rung 1 is DARKER than the page. That is what reads as a hole rather than a mark.
#   B. FILLS LIGHTER THAN THE TEXT -- bold panels, WCAG 9-14 against the page, unmissable. Two costs,
#      both real: every text colour is then dark-on-light at only Lc 14-27, and the page's own ink
#      (#f1efe0) is INVISIBLE on them (Lc 0), so a cell with a fill and no text slot must repaint its
#      ink dark. That rule is expressible in html and markdown but must be restated in tab_xl() and
#      the console, which is why option A is much cheaper to ship.
#
# ⚠ AND THE TRADE INSIDE EACH: the further a fill moves from the page, the more it reads as a shape
#   and the LESS the text on it does. Bolder is not better here; it is a different bargain.

source("dev/heading_ladders.R")             # oklch_hex(), oklch_maxC(), hex_oklch(), apca(), contrast()
suppressMessages(pkgload::load_all(".", quiet = TRUE))

options(tabxplor.lang = "en", tabxplor.tab_kable_css = FALSE, tabxplor.cleannames = TRUE,
        tabxplor.tab_kable_tooltips = FALSE)
Sys.setenv(LANGUAGE = "en")

# === SECTION: the chrome, and the grounds a table can land on =====================================
# Read from the package, never retyped. The table paints no ground of its own, so the ground varies.

CH     <- tx_chrome_hex("dark")
INK    <- CH$text                                  # #f1efe0 -- a plain table figure
ASIDE  <- CH$grey2                                 # #CDCBBC -- the footer, and the page's own prose
GREYED <- CH$grey                                  # #919085 -- a non-significant cell
HEADS  <- c("#FEF1A1", "#F5E9A3", "#ECE2A4", "#E5DB9D"); PANEL_INK <- "#CDCBBC"

GROUNDS <- list(
  list(id = "atom", hex = "#21252b", nm = "Atom One Dark - the site theme"),
  list(id = "bs",   hex = "#212529", nm = "Bootstrap 5.3 default dark"),
  list(id = "tx",   hex = CH$bg,     nm = "tabxplor interactive Viewer"),
  list(id = "gh",   hex = "#0d1117", nm = "GitHub dark"),
  list(id = "deep", hex = "#181818", nm = "a deeper ground"))
REF <- GROUNDS[[1]]$hex
lift <- function(hex, by) { v <- hex_oklch(hex); oklch_hex(min(v[1] + by, 1), v[2], v[3]) }

# === SECTION: the text ramps ======================================================================
# ✓ FINAL, and the hues every fill below is built around: 200/230/260/290 cool, 80/60/40/25 warm.
#   L .65 .69 .73 .69  C .100 .120 .140 .175   |   L .74 .74 .75 .70  C .130 .145 .155 .190
TUNED3 <- c("#2ba1a7", "#37a8d7", "#72a7ff", "#9c84ff", "#d6a13d", "#ec923e", "#ff885e", "#ff635f")

# Kept only so the choice can be re-read against them. Everything else the exploration produced is
# recorded in dev/dark_palette.md and deleted from here.
TEXTS <- list(
  list(id = "tuned-3",  nm = "tuned-by-hand-3  ✓ FINAL", hex = TUNED3,
       note = "the settled dark text palette — even hue gaps, an arch in lightness on both sides, four rungs at 100% of the gamut ceiling, and the widest colour-blind margin of anything measured (0.153–0.233)"),
  list(id = "tuned-2",  nm = "tuned-by-hand-2", hex = c(TUNED3[1:4], "#c79601", "#e48e25", "#ff8950", "#fe6c66"),
       note = "the pass before: a warm side at L 0.70–0.75, whose rung 1 sits on the olive that low lightness forces on a yellow"),
  list(id = "tuned-1",  nm = "tuned-by-hand-1", hex = c(TUNED3[1:4], "#c1983a", "#d98a2e", "#ff8950", "#fe6c66"),
       note = "the first hand-tuned pass"),
  list(id = "tuned-p1", nm = "tuned-3 + p1 lift", hex = c("#38ABB0", TUNED3[2:8]),
       note = "the final palette with p1 lifted L 0.65 → 0.68, the one change that takes the faintest over rung clear of the greyed cell (Lc 41 → 46)"),
  list(id = "gen-desc", nm = "generated: desc-even", hex = c("#62ECF3", "#4AC6FB", "#6BA1FA", "#9D88FA", "#F4D16E", "#F7AC52", "#FA8C56", "#FB7570"),
       note = "the best the generator reached: chroma maximised backwards from rung 4, even hue gaps, a descending lightness"),
  list(id = "gen-flat", nm = "generated: flat-70-even", hex = c("#22B3BA", "#21ACE0", "#679DF8", "#9E8AFA", "#C19D2D", "#DB8E27", "#EF7D43", "#FB6E6B"),
       note = "the generator's flat-lightness answer — every rung at Lc 45–50"),
  list(id = "gen-arch", nm = "generated: arch-70-82-70", hex = c("#71AAB0", "#7DBADE", "#9CC7FF", "#7B98FA", "#B29B74", "#DAA478", "#FFAD8D", "#FB6958"),
       note = "the generator's arch — rungs 1–3 climbing, rung 4 dropping for its chroma"),
  list(id = "current",  nm = "current (shipped)", hex = c(tx_ramp("text","dark","over"), tx_ramp("text","dark","under")),
       note = "what tabxplor ships today — the baseline"),
  list(id = "light",    nm = "the light palette", hex = c(tx_ramp("text","light","over"), tx_ramp("text","light","under")),
       note = "the light palette on a dark page, for reference"))

# === SECTION: the fills ===========================================================================
# ⚠ CHROMA IS DELIBERATELY LOW HERE, and that is not a compromise: a filled area is large, so hue
# separates it at a chroma that would be invisible in a digit. The light theme's own fills run
# C 0.028-0.082. What a fill must NOT do is compete with the text sitting on it.

HC <- c(200, 230, 260, 290); HW <- c(80, 60, 40, 25)        # the text palette's own hues
fills <- function(L, C) { one <- function(H) vapply(seq_along(L), function(i)
    oklch_hex(L[i], min(C[i], oklch_maxC(L[i], H[i])), H[i]), character(1))
  c(one(HC), one(HW)) }

# ⚠ WHERE A LIGHT FILL'S CHROMA ACTUALLY LIVES, and it is not where the text ramp's did. Up in the
# light band the EARLY hue is the rich one and everything after it collapses: at L 0.912, h200 holds
# 0.142 while h230 holds 0.050 and h290 0.044. So RUNG 2 is the bottleneck of a light fill ramp, not
# rung 4 -- and rung 1 must be held far below its own ceiling or the ramp cannot rise at all.
# ⚠ Compressing the fill hues toward cyan buys nothing: h225 against h230 at L 0.912 is 0.007.
# ⚠ THE ONLY LEVER FOR MORE CHROMA IS LOWER LIGHTNESS, AND IT IS PRICED. Measured, every 0.010 of
# lightness given back buys ~0.006 of chroma at rung 4 and costs ~2 Lc of the worst text-on-fill:
#
#     band            rung-4 C   text-on-fill      band            rung-4 C   text-on-fill
#     .945 -> .846      0.079      15 (= B1)       .915 -> .816      0.094       9
#     .935 -> .836      0.082      13              .905 -> .806      0.100       7
#     .925 -> .826      0.088      11
#
#   Light's own worst is 14 and APCA calls Lc 15 the threshold of discernible, so the last two rows
#   put the rare cross-direction pair below anything defensible. They are carried to be seen.
# ⚠ And at B1's OWN band the chroma can be maxed for nothing: 0.033/0.043/0.056/0.079 against its
#   0.024/0.040/0.055/0.075, with the worst text-on-fill unchanged at 15. That one is free.

# Chroma maximised backwards from rung 4, `r` the minimum ratio between rungs -- the same rule the
# text ramp was built with, and what keeps the floor as high as the ramp allows.
mfill <- function(L, H, r, f = 0.95) {
  ce <- vapply(seq_along(L), function(i) oklch_maxC(L[i], H[i]), numeric(1))
  C <- numeric(4); C[4] <- f * ce[4]
  for (i in 3:1) C[i] <- min(f * ce[i], C[i + 1] / r)
  vapply(seq_len(4), function(i) oklch_hex(L[i], C[i], H[i]), character(1))
}
# `pill_ink` is the ink a text slot takes INSIDE a fill, and the only inversion the design allows:
# it reaches a cell that has a fill and NO text slot, whose ink would otherwise be the page's own.
PILL <- "#16181c"
bmax <- function(id, nm, L, r, note, remap = NULL, veil = NULL, shape = "round")
  list(id = id, nm = nm, pill = PILL, L = NULL, C = NULL, remap = remap, veil = veil, shape = shape,
       hex = c(mfill(L, HC, r), mfill(L, HW, r)), note = note)

# ⚠ TWO WORKAROUNDS FOR THE DUAL CHANNEL, and both are previews of a change that belongs in R.
#   `remap` -- COLOR_SCALES$bg_keep = 2 keeps the two LOUDEST break rungs on the fill channel, and
#     draws them with palette slots 3 and 4. The breaks are right; the slots are the problem. Drawing
#     the same two breaks with slots 1 and 3 lifts the worst text-on-fill from Lc 9 to 16, and with
#     1 and 2 to 22 -- for nothing, since the ladder still has only two steps to show either way.
#     ⚠ Here it is a CSS rule scoped to cells that HAVE a text slot, so two things are wrong that a
#     real implementation would not do: the legend swatches and the bare-fill cells keep the loud
#     slot. Read the coloured cells, not the footer.
#   `veil` -- a translucent white box behind the digits ONLY, so the fill keeps its chroma in the
#     margins while the centre gains lightness. ⚠ CSS composites alpha in gamma-encoded sRGB, so the
#     lift is not linear in alpha: 0.30 raises the fill by dL 0.056 and buys 10 Lc, 0.40 by 0.074 and
#     14 Lc. It needs one more element than tab_html() emits, added here by post-processing.
#     ⚠ `.tx-pill` ships as `padding:1px 4px` (R/tab-css.R:461), which leaves only 4px of chromatic
#     margin for a veil to sit inside. The veil palettes widen it, or there is nothing left to see.
#     `shape` -- "round", a flat alpha in a fully rounded box; or "soft", a gradient fading to
#     transparent at its left and right edges. ⚠ A fading veil needs NO rounding: the fade IS the
#     shape. ⚠ THE FADE IS HORIZONTAL, and a radial one is a trap: sized in percentages it never
#     reaches its last stop inside the box (an `ellipse 74%` still carries two thirds of its alpha at
#     the left edge), so it renders as a flat veil. There is nothing to fade into vertically anyway,
#     the pill's margin there being 1px. It is also the least portable thing on this page -- a
#     gradient survives html and nothing else, where a flat alpha has a solid equivalent Excel and
#     the console could be given.

BGS <- list(
  list(id = "b1", nm = "B1 panels (as chosen)", pill = PILL,
       L = c(.945,.912,.879,.846), C = c(.025,.040,.055,.075),
       note = "the one picked: light panels at WCAG 9–13, worst text-on-fill Lc 15 — level with the light theme's own 14"),
  bmax("b1m", "B1 band, chroma maxed", c(.945,.912,.879,.846), 1.45,
       "the same band with every rung pushed to the gamut, at the same ramp shape (×3.1) — chroma up on every rung and the worst text-on-fill unchanged at 15. This one costs nothing"),
  bmax("b1f", "B1 band, high floor", c(.945,.912,.879,.846), 1.30,
       "the same band again, the ramp flattened to ×2.4 so the floor comes up to C 0.033 — the most colour in the faint rungs that this lightness allows"),
  bmax("b2m", "one step deeper", c(.935,.902,.869,.836), 1.45,
       "the band down 0.010: rung 4 reaches C 0.082, and the worst text-on-fill falls 15 → 13"),
  bmax("b2f", "one step deeper, high floor", c(.935,.902,.869,.836), 1.30,
       "the same, ramp flattened — floor C 0.039, rung 4 C 0.082"),
  bmax("b3m", "two steps deeper", c(.925,.892,.859,.826), 1.45,
       "rung 4 at C 0.088, and the worst text-on-fill at Lc 11 — now below the light theme's own 14"),
  bmax("b4f", "the richest that holds", c(.915,.882,.849,.816), 1.30,
       "the chosen one: floor C 0.043, rung 4 C 0.094 — and a worst text-on-fill of Lc 9, which is what the two rows below exist to repair"),
  bmax("b4r13", "richest · fill slots 1+3", c(.915,.882,.849,.816), 1.30, remap = c(1L, 3L),
       "the same fills, with the two loud breaks drawn on slots 1 and 3 instead of 3 and 4 — the ladder still shows two steps, and the worst text-on-fill goes Lc 9 → 16"),
  bmax("b4r12", "richest · fill slots 1+2", c(.915,.882,.849,.816), 1.30, remap = c(1L, 2L),
       "the same again on slots 1 and 2 — Lc 22, the quietest fills the palette has, and the two steps are then close together"),
  bmax("b4v30", "richest · veil 30%", c(.915,.882,.849,.816), 1.30, veil = 0.30,
       "the loud fills kept, with a fully rounded translucent box behind the digits only: the margins hold their chroma, the centre lifts dL 0.056, and text-on-fill goes Lc 9 → 19"),
  bmax("b4v40", "richest · veil 40%", c(.915,.882,.849,.816), 1.30, veil = 0.40,
       "the same at 40% — Lc 23, and the fill starts to read as a ring around the number rather than a block"),
  bmax("b4s35", "richest · veil 35% soft", c(.915,.882,.849,.816), 1.30, veil = 0.35, shape = "soft",
       "a veil that FADES to transparent at its edges instead of stopping — no rounding needed, because the fade is the shape. The Lc below is the centre; the outermost digits sit on less of it"),
  bmax("b4s45", "richest · veil 45% soft", c(.915,.882,.849,.816), 1.30, veil = 0.45, shape = "soft",
       "the fading veil pushed harder — a gradient can carry more alpha at its centre than a hard-edged box can, because nothing about it shows an edge"),
  bmax("b4both", "richest · slots 1+3 & veil 30%", c(.915,.882,.849,.816), 1.30,
       remap = c(1L, 3L), veil = 0.30,
       "both fixes together: Lc 24, the best dual channel measured here and well past the light theme's own 14"),
  bmax("b4bs", "richest · slots 1+3 & veil 35% soft", c(.915,.882,.849,.816), 1.30,
       remap = c(1L, 3L), veil = 0.35, shape = "soft",
       "both again, with the fading veil"),
  list(id = "a1", nm = "A1 mirror (darker fills)", pill = NULL,
       L = c(.30,.34,.38,.43), C = c(.030,.045,.060,.080),
       note = "the other direction, kept for comparison: the light theme's fills transposed below the page instead of above it"),
  # ✓ WHAT SHIPS. Its `remap` and `pill` are not preview tricks any more: the package does both --
  # bg_keep now spreads its breaks into the faint slots (R/fmt_class.R), and tx_chrome_hex()$on_fill
  # repaints a bare fill's ink. Stated here so the scorecard measures the real thing.
  list(id = "cur", nm = "current (shipped) ✓", pill = tx_chrome_hex("dark")$on_fill,
       L = NULL, C = NULL, remap = c(1L, 3L),
       note = "what tabxplor ships: light panels at WCAG 8–12, the two loud breaks drawn in slots 1 and 3, and a bare fill's ink repainted to the page's own ground"))

BGS <- lapply(BGS, function(b) { if (is.null(b$hex))
    b$hex <- if (is.null(b$L)) c(tx_ramp("bg","dark","over"), tx_ramp("bg","dark","under"))
             else fills(b$L, b$C); b })

# ⚠ THE TWO CHANNELS ARE INDEPENDENT AXES, and the page keys them on two separate attributes --
# `data-text` and `data-bg` -- so any text ramp can be seen under any fill. That is not a convenience:
# it is the thing being judged, since the two channels carry different measures and move apart.
TXTP <- lapply(TEXTS, function(t) list(id = paste0("t-", t$id), name = t$nm, note = t$note,
                                       text = t$hex))
BGP  <- lapply(BGS, function(b) list(id = paste0("b-", b$id), name = b$nm, note = b$note,
                                     text = TUNED3, bg = b$hex, pill = b$pill,
                                     remap = b$remap, veil = b$veil, shape = b$shape))

# === SECTION: colour vision =======================================================================
# Machado et al. (2009) through colorspace. ⚠ The matrices are defined in LINEAR RGB, so the sRGB
# gamma comes off and goes back on. ⚠ And `pmax(0, m)` drops a matrix's dim where `pmax(m, 0)` keeps it.
cvd <- function(hex, type) {
  M   <- colorspace::interpolate_cvd_transform(
    switch(type, deutan = colorspace::deutanomaly_cvd, protan = colorspace::protanomaly_cvd), 1)
  lin <- srgb_linear(farver::decode_colour(hex, to = "rgb") / 255)
  farver::encode_colour(round(pmin(pmax(srgb_encode(as.matrix(lin) %*% t(M)), 0), 1) * 255), from = "rgb")
}
dE_ok <- function(a, b) { u <- hex_oklch(a); v <- hex_oklch(b)
  ua <- c(u[2]*cos(u[3]*pi/180), u[2]*sin(u[3]*pi/180))
  va <- c(v[2]*cos(v[3]*pi/180), v[2]*sin(v[3]*pi/180))
  sqrt((u[1]-v[1])^2 + sum((ua-va)^2)) }

# === SECTION: the checks ==========================================================================
# ⚠ The text checks do NOT include "the |Lc| rises": the light palette's own falls 43 -> 14 as its
# deviation grows. A ladder is a CHROMA ramp whose rungs must each stay legible. Only "rung 1 clears
# grey" is correctness (dev/colors.md 3.2 rule 3); the rest are the design floors to argue about.
TCHECKS <- c("rung 1 clears grey", "rung 4 stays readable", "chroma never falls",
             "direction survives CVD", "rungs separable")
BCHECKS <- c("reads as a shape (light: dL 0.029)", "fills separable (light: 0.030)",
             "text on fill no worse than light (Lc 14)", "a bare fill's ink works (Lc 45)")

tscore <- function(p, ground) {
  tx <- list(p$text[1:4], p$text[5:8])
  lc <- lapply(tx, function(v) abs(vapply(v, apca, 1, bg = ground)))
  cc <- lapply(tx, function(v) vapply(v, function(h) hex_oklch(h)[2], 1))
  gap <- vapply(1:4, function(i) min(
    dE_ok(cvd(p$text[i], "deutan"), cvd(p$text[i+4], "deutan")),
    dE_ok(cvd(p$text[i], "protan"), cvd(p$text[i+4], "protan"))), numeric(1))
  step <- unlist(lapply(tx, function(v) vapply(1:3, function(i) dE_ok(v[i], v[i+1]), numeric(1))))
  list(lc = lc, gap = gap, step = step,
       ok = c(all(vapply(lc, function(x) x[1] >= abs(apca(GREYED, ground)) + 3, TRUE)),
              all(vapply(lc, function(x) min(x) >= 35, TRUE)),
              all(vapply(cc, function(x) all(x[-1] / x[-4] > 1.02), TRUE)),
              all(gap >= 0.08), all(step >= 0.040)))
}
# ⚠ A fill is measured by LIGHTNESS DISTANCE from the page, not by APCA (see the header), and EVERY
# BAR BELOW IS THE LIGHT THEME'S OWN NUMBER -- rung 1 sits dL 0.029 from its page, adjacent rungs
# 0.030 apart, and its worst text-on-fill is Lc 14 (its rung-1 amber #dca331 on its rung-4 fill
# #ffbaaf). ⚠ That last one is worth sitting with: the dual channel is ALREADY weak in the shipped
# light theme, so "no worse than light" is a low bar and the figure itself is what to read. A bare
# fill's ink is held to Lc 45, the APCA floor for large text, because nothing else protects it.
# ⚠ CSS composites alpha in gamma-encoded sRGB, not in linear light and not in OKLab.
over_srgb <- function(top, a, bot) { t <- strtoi(substring(top, c(2,4,6), c(3,5,7)), 16L)
  b <- strtoi(substring(bot, c(2,4,6), c(3,5,7)), 16L)
  sprintf("#%02X%02X%02X", round(a*t[1]+(1-a)*b[1]), round(a*t[2]+(1-a)*b[2]), round(a*t[3]+(1-a)*b[3])) }
# What a text colour actually sits on: the fill slots the dual channel really uses, veiled if asked.
dual_fills <- function(p) { i <- if (is.null(p$remap)) c(3L, 4L) else p$remap
  f <- p$bg[c(i, i + 4L)]
  if (is.null(p$veil)) f else vapply(f, function(b) over_srgb("#ffffff", p$veil, b), character(1)) }

bscore <- function(p, ground) {
  gl <- hex_oklch(ground)[1]; fl <- vapply(p$bg, function(h) hex_oklch(h)[1], 1)
  dl <- abs(fl - gl); adj <- min(abs(diff(fl[1:4])), abs(diff(fl[5:8])))
  onf <- min(abs(outer(p$text, dual_fills(p), Vectorize(function(t, b) apca(t, b)))))
  bare <- min(abs(vapply(p$bg, function(b) apca(if (is.null(p$pill)) INK else p$pill, b), 1)))
  list(dl = dl, adj = adj, onf = onf, bare = bare,
       wcag = vapply(p$bg, contrast, 1, b = ground),
       ok = c(min(dl) >= 0.029, adj >= 0.030, onf >= 14, bare >= 45))
}

# === SECTION: the stylesheet ======================================================================
# ⚠ SPECIFICITY: tab_css()'s dark layer is `[data-bs-theme=dark] .tabxplor-tab .p1` (0,3,0). These
# rules carry the page attribute plus `html`, so (0,3,1) -- always higher, whatever the source order.
TXT_CLS <- c(paste0(".p", 1:4), paste0(".m", 1:4))
BG_CLS  <- c(paste0(".o", 1:4), paste0(".u", 1:4))

text_css <- function(p, prefix) paste0(prefix, " .tabxplor-tab ", TXT_CLS, "{color:", p$text, ";}",
                                       collapse = "\n")
bg_css <- function(p, prefix) {
  out <- paste0(prefix, " .tabxplor-tab ", BG_CLS, "{background-color:", p$bg, ";}")
  # ⚠ THE ONE INVERSION THE DESIGN ALLOWS. A rung has ONE text colour or its legend cannot be read,
  # so no rule here repaints a text slot. This reaches only a cell that has a FILL AND NO TEXT SLOT,
  # whose ink would otherwise be the page's own -- invisible once the fill is light.
  if (!is.null(p$pill))
    out <- c(out, paste0(prefix, " .tabxplor-tab td", paste0(":not(", TXT_CLS, ")", collapse = ""),
                         " .tx-pill{color:", p$pill, ";}"))
  # The remap reaches only a fill INSIDE a cell that has a text slot -- which is the dual channel.
  if (!is.null(p$remap)) {
    has_txt <- paste0("td:is(", paste(TXT_CLS, collapse = ","), ")")
    out <- c(out, paste0(prefix, " .tabxplor-tab ", has_txt, " .tx-pill", c(".o3", ".o4", ".u3", ".u4"),
                         "{background-color:", p$bg[c(p$remap, p$remap + 4L)], ";}"))
  }
  if (!is.null(p$veil)) {
    a <- format(p$veil)
    out <- c(out,
      # ⚠ WIDER, NOT ROUNDER. The fill keeps the shape tab_css() gives it (border-radius 4px, a near
      # rectangle); only its padding grows, or the veil has no chromatic margin to sit inside.
      paste0(prefix, " .tabxplor-tab .tx-pill{padding:1px 7px;margin:0 -7px;}"),
      paste0(prefix, " .tabxplor-tab .tx-lift{",
        if (identical(p$shape, "soft"))
          # ⚠ A HORIZONTAL fade, and a radial one is wrong here. The pill's margin is 7px at the
          # sides and 1px above and below, so there is nothing to fade into vertically. And a radial
          # gradient sized in percentages never reaches its last stop inside the box: at the left
          # edge of an `ellipse 74%` it still carries two thirds of its alpha, which is why it read
          # as a flat veil rather than a fading one.
          paste0("padding:0 4px;margin:0 -4px;background:linear-gradient(90deg,",
                 "rgba(255,255,255,0) 0%,rgba(255,255,255,", a, ") 20%,rgba(255,255,255,", a,
                 ") 80%,rgba(255,255,255,0) 100%);}")
        else paste0("padding:0 3px;margin:0 -3px;border-radius:999px;background:rgba(255,255,255,",
                    a, ");}")))
  }
  paste(out, collapse = "\n")
}
axis_css <- function(p, attr, fn) paste(c(
  fn(p, sprintf('html[data-%s="%s"]', attr, p$id)),
  vapply(c("deutan", "protan"), function(t) {
    q <- p; q$text <- cvd(q$text, t)
    if (!is.null(q$bg)) q$bg <- cvd(q$bg, t)
    if (!is.null(q$pill)) q$pill <- cvd(q$pill, t)
    fn(q, sprintf('html[data-%s="%s"] .cvd-%s', attr, p$id, t))
  }, character(1))), collapse = "\n")

# === SECTION: the sidebar =========================================================================

lc_spans <- function(hexes) vapply(hexes, function(h) paste0(vapply(GROUNDS, function(g)
  sprintf('<span class="lc %s">%.0f</span>', g$id, abs(apca(h, g$hex))), character(1)),
  collapse = ""), character(1))

text_card <- function(p) {
  row <- function(v, slot, bg) vapply(seq_along(v), function(i) { o <- hex_oklch(v[i])
    sprintf("<tr><td>%s%d</td><td class='sw'><span style='background:%s'></span></td><td><code>%s</code></td><td>%.2f</td><td>%.3f</td><td>%.0f</td><td class='lcc'>%s</td></tr>",
      slot, i, v[i], v[i], o[1], o[2], o[3], if (bg) sprintf("%.3f", abs(o[1] - hex_oklch(REF)[1]))
                                             else lc_spans(v[i])) }, character(1))
  ts <- tscore(p, REF)
  paste0("<table class='scale'><tr><th>slot</th><th></th><th>hex</th><th>L</th><th>C</th><th>h</th>",
         "<th>|Lc|</th></tr>",
         paste(c(row(p$text[1:4], "p", FALSE), row(p$text[5:8], "m", FALSE)), collapse = ""),
         "</table>", chk_div(TCHECKS, ts$ok),
         gamut_svg(p$text[1:4], "text over"), gamut_svg(p$text[5:8], "text under"))
}
chk_div <- function(nms, ok) sprintf("<div class='chk'>%s</div>", paste(sprintf(
  '<span class="%s">%s %s</span>', ifelse(ok, "y", "n"), ifelse(ok, "&check;", "&cross;"), nms),
  collapse = ""))
# ⚠ The fill card's text-on-fill figure is measured against the SETTLED text ramp, not against
# whichever ramp the other menu shows: the text palette is final, and a metric that moved with both
# menus would say nothing about the fill.
bg_card <- function(p) {
  row <- function(v, slot) vapply(seq_along(v), function(i) { o <- hex_oklch(v[i])
    sprintf("<tr><td>%s%d</td><td class='sw'><span style='background:%s'></span></td><td><code>%s</code></td><td>%.2f</td><td>%.3f</td><td>%.0f</td><td class='lcc'>%.3f</td></tr>",
      slot, i, v[i], v[i], o[1], o[2], o[3], abs(o[1] - hex_oklch(REF)[1])) }, character(1))
  bs <- bscore(p, REF)
  paste0("<table class='scale'><tr><th>fill</th><th></th><th>hex</th><th>L</th><th>C</th><th>h</th>",
         "<th>dL</th></tr>",
         paste(c(row(p$bg[1:4], "o"), row(p$bg[5:8], "u")), collapse = ""), "</table>",
         chk_div(BCHECKS, bs$ok),
         sprintf("<div class='num'>WCAG %s<br>text on fill <b>Lc %.0f</b>%s%s<br>a bare fill's ink <b>Lc %.0f</b>%s</div>",
                 paste(sprintf("%.2f", bs$wcag[1:4]), collapse = " "), bs$onf,
                 if (is.null(p$remap)) "" else sprintf(" &middot; slots %s", paste(p$remap, collapse = "+")),
                 if (is.null(p$veil)) "" else sprintf(" &middot; veil %.0f%% %s", 100 * p$veil, p$shape),
                 bs$bare, if (is.null(p$pill)) "" else sprintf(" (repainted %s)", p$pill)))
}

# === SECTION: the gamut curve =====================================================================
# The oklch.com reading: for each rung's hue, the chroma sRGB holds at every lightness, with the
# rung's own point on it. Headroom, the cusp it is near, and the floor it must clear, in one glance.
CURVE_L <- seq(0.46, 0.97, by = 0.01)
gamut_svg <- function(v, lab, w = 272, h = 132) {
  o <- vapply(v, hex_oklch, numeric(3))
  x <- function(l) 24 + (l - 0.46) / 0.51 * (w - 32); y <- function(c) h - 18 - c / 0.32 * (h - 30)
  paste0('<svg class="gam" viewBox="0 0 ', w, ' ', h, '" width="100%">',
    sprintf('<line x1="24" y1="%.1f" x2="%d" y2="%.1f" stroke="%s" stroke-width=".6" opacity=".25"/>',
            y(0.1), w-8, y(0.1), PANEL_INK),
    sprintf('<line x1="24" y1="%.1f" x2="%d" y2="%.1f" stroke="%s" stroke-width=".6" opacity=".25"/>',
            y(0.2), w-8, y(0.2), PANEL_INK),
    paste(vapply(seq_along(v), function(i) sprintf(
      '<polyline points="%s" fill="none" stroke="%s" stroke-width="1.1" opacity=".45"/>',
      paste(sprintf("%.1f,%.1f", x(CURVE_L),
        y(vapply(CURVE_L, oklch_maxC, numeric(1), H = o[3, i]))), collapse = " "), v[i]),
      character(1)), collapse = ""),
    paste(sprintf('<circle cx="%.1f" cy="%.1f" r="3.4" fill="%s" stroke="%s" stroke-width="1"/>',
                  x(o[1, ]), y(o[2, ]), v, PANEL_INK), collapse = ""),
    sprintf('<text x="2" y="%.1f" class="ax">.2</text><text x="2" y="%.1f" class="ax">.1</text>',
            y(0.2)+3, y(0.1)+3),
    sprintf('<text x="24" y="%d" class="ax">L .46</text><text x="%d" y="%d" class="ax">.97</text>',
            h-6, w-24, h-6),
    sprintf('<text x="80" y="12" class="ax">%s</text>', lab), '</svg>')
}

# === SECTION: the comparison strips ================================================================
# Coloured cells are BOLD, because the dark text palette is bold on all eight slots and a strip that
# was not would flatter it.
NUM <- list(over = c("+5.2", "+11.4***", "+21.7***", "+32.5***"),
            under = c("&minus;5.2", "&minus;11.4***", "&minus;21.7***", "&minus;32.5***"))

text_row <- function(p) {
  one <- function(v, lab) paste(sprintf('<span class="sc" style="color:%s">%s</span>', v, lab), collapse = "")
  fl <- vapply(GROUNDS, function(g) { ok <- tscore(p, g$hex)$ok
    sprintf('<span class="flag %s">%s</span>', g$id, if (all(ok)) "" else paste0("&cross;", sum(!ok)))
  }, character(1))
  sprintf(paste0('<div class="srow text" data-go="%s"><span class="nm">%s</span><span class="flags">%s</span>',
    '%s<span class="sep"></span>%s<span class="sep"></span>',
    '<span class="sc rf" style="color:%s">12.0</span><span class="sc rf" style="color:%s">12.0</span></div>'),
    p$id, p$name, paste(fl, collapse = ""),
    one(p$text[1:4], NUM$over), one(p$text[5:8], NUM$under), INK, GREYED)
}
# A fill is judged with a figure ON it -- both the text ramp's own colour, and the bare-cell ink.
bg_row <- function(p) {
  df <- dual_fills(p)          # what a text colour actually lands on, once the fixes are applied
  cell <- function(i) sprintf('<span class="bc" style="background:%s"><b style="color:%s">%s</b> <i style="color:%s">12.0</i></span>',
    if (i <= 4) df[min(i, 2)] else df[min(i - 4, 2) + 2], p$text[i],
    if (i <= 4) "+21.7" else "&minus;21.7", if (is.null(p$pill)) INK else p$pill)
  fl <- vapply(GROUNDS, function(g) { ok <- bscore(p, g$hex)$ok
    sprintf('<span class="flag %s">%s</span>', g$id, if (all(ok)) "" else paste0("&cross;", sum(!ok)))
  }, character(1))
  sprintf('<div class="srow bg" data-go="%s"><span class="nm">%s</span><span class="flags">%s</span>%s<span class="sep"></span>%s</div>',
          p$id, p$name, paste(fl, collapse = ""),
          paste(vapply(1:4, cell, character(1)), collapse = ""),
          paste(vapply(5:8, cell, character(1)), collapse = ""))
}

# === SECTION: the tables ==========================================================================

gss <- gss_cat_data_formatting()
tabs <- list(
  dual = list("Both channels &mdash; deviation on the text, ratio on the fill" =
                tab(gss, c(race, rincome), c(party3, relig), pct = "row",
                    color = c("difference", "ratio"), ref = 1)),
  bg   = list("The fills alone, with no text colour to help them" =
                tab(gss, c(race, rincome), c(party3, relig), pct = "row",
                    color = c("no", "difference"), ref = 1)),
  text = list("Every rung, both directions" =
                tab(gss, c(race, rincome, marital, relig), c(party3, married, income25k, black),
                    pct = "row", color = "difference"),
              "Greyed where not significant, so the ladder is read beside the grey it must out-rank" =
                tab(gss, relig, c(married, income25k), pct = "row", levels = "first",
                    color = "difference", color_signif = "grey_non_signif")))

one_table <- function(html, title) paste0('<h3>', title, '</h3>\n', html,
  '\n<details class="cvd"><summary>colour vision &mdash; deuteranopia and protanopia</summary><div class="trio">',
  '<div class="cvd-deutan"><div class="lab">deuteranopia</div>', html, '</div>',
  '<div class="cvd-protan"><div class="lab">protanopia</div>', html, '</div></div></details>')
# ⚠ tab_html() emits <td class="p4"><span class="tx-pill o3">75%</span></td> -- one box, not two. The
# veil needs an inner element, so one is added HERE, and only where a fill sits under a text colour.
# It is inert until a palette gives .tx-lift a background, so every palette shares the same markup.
add_lift <- function(h) gsub(
  '(<td class="[^"]*\\b[pm][1-4]\\b[^"]*"><span class="tx-pill [ou][1-4]">)([^<]*)(</span>)',
  '\\1<span class="tx-lift">\\2</span>\\3', h, perl = TRUE)
render <- function(l) paste(vapply(names(l), function(nm)
  one_table(add_lift(as.character(tab_html(l[[nm]]))), nm), character(1)), collapse = "\n")

# === SECTION: the page ============================================================================

menu <- function(lst, sel) paste0('<option value="', vapply(lst, function(p) p$id, character(1)),
  '"', ifelse(vapply(lst, function(p) p$id, character(1)) == sel, " selected", ""), '>',
  vapply(lst, function(p) p$name, character(1)), '</option>', collapse = "")
blocks <- function(lst, cls, fn) paste0('<div class="', cls, '" data-for="',
  vapply(lst, function(p) p$id, character(1)), '">', vapply(lst, fn, character(1)), '</div>',
  collapse = "")
SEL_T <- "t-tuned-3"; SEL_B <- "b-b4both"

gopt <- paste0('<option value="', vapply(GROUNDS, function(g) g$id, character(1)), '">',
               vapply(GROUNDS, function(g) paste0(g$hex, " ", g$nm), character(1)), '</option>', collapse = "")

ground_css <- paste(vapply(GROUNDS, function(g) sprintf(paste0(
  'html[data-ground="%1$s"] body{background:%2$s;}\nhtml[data-ground="%1$s"] .panel{background:%3$s;}\n',
  'html[data-ground="%1$s"] .lc.%1$s,html[data-ground="%1$s"] .flag.%1$s{display:inline;}'),
  g$id, g$hex, lift(g$hex, 0.03)), character(1)), collapse = "\n")

html <- paste0('<!doctype html>
<html lang="en" data-bs-theme="dark" data-text="t-tuned-3" data-bg="b-b4both" data-ground="atom">
<head>
<meta charset="utf-8"><meta name="viewport" content="width=device-width, initial-scale=1">
<title>tabxplor - dark background palettes</title>
<style>', tab_css(theme = "auto", style_tag = FALSE), '</style>
<style>', paste(c(vapply(TXTP, axis_css, character(1), attr = "text", fn = text_css),
                    vapply(BGP,  axis_css, character(1), attr = "bg",   fn = bg_css)), collapse = "\n"), '</style>
<style>
  body{color:', ASIDE, ';margin:0;line-height:1.5;
       font-family:"DejaVu Sans","Source Sans Pro",system-ui,sans-serif;}
  .layout{display:flex;align-items:flex-start;}
  .panel{flex:0 0 19rem;position:sticky;top:0;height:100vh;overflow-y:auto;
         border-right:1px solid #3e4451;padding:.9rem 1rem;font-size:.8rem;}
  .wrap{flex:1 1 auto;min-width:0;padding:1rem 1.5rem 40vh;}
  h1,h2,h3{font-weight:bold;line-height:1.25;}
  h1{color:', HEADS[1], ';font-size:1.7rem;} h2{color:', HEADS[2], ';font-size:1.35rem;margin-top:2.4rem;}
  h3{color:', HEADS[3], ';font-size:1.1rem;margin-top:2rem;}
  .trio{display:flex;gap:1.4rem;flex-wrap:wrap;align-items:flex-start;}
  .lab{font-size:.72rem;opacity:.55;margin-bottom:.3rem;}
  .tnote,.bnote,.tcard,.bcard{display:none;} .tnote,.bnote{opacity:.8;margin:.4rem 0 1.2rem;}
  html[data-text="t-tuned-3"] .tnote[data-for="t-tuned-3"],
  html[data-text="t-tuned-3"] .tcard[data-for="t-tuned-3"],
  html[data-bg="b-b4both"] .bnote[data-for="b-b4both"],
  html[data-bg="b-b4both"] .bcard[data-for="b-b4both"]{display:block;}
  .cardhd{font-size:.68rem;opacity:.45;margin:.9rem 0 .2rem;border-top:1px solid #3e4451;padding-top:.5rem;}
  .scale{border-collapse:collapse;font-size:.68rem;width:100%;margin-bottom:.4rem;}
  .scale th{opacity:.5;font-weight:normal;text-align:left;}
  .scale td{padding:0 .3rem 0 0;white-space:nowrap;}
  .scale .sw span{display:inline-block;width:1.1rem;height:.7rem;border-radius:2px;}
  .scale .lcc{text-align:right;}
  .lc,.flag{display:none;}
  .chk{font-size:.66rem;margin-top:.4rem;} .chk.sep{border-top:1px solid #3e4451;padding-top:.3rem;}
  .chk span{display:block;} .chk .y{opacity:.5;} .chk .n{color:#ff8b7d;font-weight:bold;}
  .num{font-size:.68rem;opacity:.7;margin-top:.5rem;line-height:1.6;}
  .flag{color:#ff8b7d;font-weight:bold;}
  .refbar{font-size:.7rem;opacity:.75;margin:.6rem 0 1rem;line-height:1.9;} .refbar code{opacity:.6;}
  .strip{margin:1rem 0 0;font-size:.82rem;}
  .srow{display:flex;align-items:center;gap:.1rem;padding:.14rem 0;white-space:nowrap;cursor:pointer;}
  .srow:hover{background:rgba(255,242,204,.06);}
  .srow.head{opacity:.45;font-size:.66rem;border-bottom:1px solid #3e4451;cursor:default;}
  .srow.head:hover{background:none;} .srow.head .sc{font-weight:normal;}
  .srow .nm{flex:0 0 13rem;font-size:.72rem;opacity:.85;overflow:hidden;text-overflow:ellipsis;}
  .srow .flags{flex:0 0 2rem;font-size:.7rem;}
  .sc{display:inline-block;width:5.1rem;text-align:right;font-weight:bold;font-variant-numeric:tabular-nums;}
  .sc.rf{font-weight:normal;width:4rem;} .sep{display:inline-block;width:1.4rem;}
  .bc{display:inline-block;width:7.4rem;padding:.12rem .35rem;margin-right:.15rem;border-radius:2px;
      font-size:.74rem;font-variant-numeric:tabular-nums;}
  .bc i{font-style:normal;opacity:.85;font-size:.9em;}
  details.cvd{margin:.6rem 0 0;opacity:.85;}
  details.cvd summary{font-size:.78rem;opacity:.6;cursor:pointer;}
  select,input{font:inherit;font-size:.8rem;max-width:100%;} label{display:block;margin-bottom:.7rem;}
</style>
<style>', ground_css, '</style>
</head>
<body>
<div class="layout">
<nav class="panel">
  <label>text palette<br><select id="seltext">', menu(TXTP, SEL_T), '</select></label>
  <label>background palette<br><select id="selbg">', menu(BGP, SEL_B), '</select></label>
  <label>ground<br><select id="ground">', gopt, '</select></label>
  <label>chroma cap &mdash; <span id="capout">off</span><br>
    <input id="cap" type="range" min="0.02" max="0.30" step="0.01" value="0.30" style="width:100%;"></label>
  <div class="refbar">
    <span style="color:', INK, '">&#9632;</span> plain figure <code>', INK, '</code>
    ', paste(vapply(GROUNDS, function(g) sprintf('<span class="lc %s">Lc %.0f</span>', g$id,
        abs(apca(INK, g$hex))), character(1)), collapse = ""), '<br>
    <span style="color:', GREYED, '">&#9632;</span> greyed <code>', GREYED, '</code>
    ', paste(vapply(GROUNDS, function(g) sprintf('<span class="lc %s">Lc %.0f</span>', g$id,
        abs(apca(GREYED, g$hex))), character(1)), collapse = ""), '
  </div>
  <div class="cardhd">text ramp</div>', blocks(TXTP, "tcard", text_card), '
  <div class="cardhd">fills — text-on-fill measured against the settled text ramp</div>',
  blocks(BGP, "bcard", bg_card), '
</nav>
<div class="wrap">
<h1>The dark background palette</h1>
<div class="pnote" style="display:block;opacity:.7;margin-bottom:.8rem;">The text ramp is settled
(<b>tuned-by-hand-3</b>) and fixed for every fill below. A fill has to clear <i>every</i> text colour
at once, because the two channels carry different measures and move independently.</div>

<h2>The fills</h2>
<div class="pnote" style="display:block;opacity:.7;margin-bottom:.6rem;">Each fill with the text
colour of the same rung on it, and beside it the ink a cell takes when it has a fill and no text
colour. A red mark counts the checks failing on the current ground.</div>
<div class="strip">', paste(vapply(BGP, bg_row, character(1)), collapse = "\n"), '</div>

<h2 id="selected">The selected palette, on real tables</h2>
', blocks(BGP, "bnote", function(p) p$note), blocks(TXTP, "tnote", function(p) p$note), '
', render(tabs$dual), '
', render(tabs$bg), '

<h2>The text ramps, for the record</h2>
<div class="strip">', paste(vapply(TXTP, text_row, character(1)), collapse = "\n"), '</div>
', render(tabs$text), '
</div></div>
<script>', paste(readLines("dev/chroma_cap.js", warn = FALSE), collapse = "\n"), '</script>
<script>
  const root = document.documentElement;
  const axis = (selId, attr, noteCls, cardCls) => {
    const sel = document.querySelector(selId);
    const apply = v => { root.setAttribute(attr, v);
      document.querySelectorAll("." + noteCls + ",." + cardCls).forEach(n =>
        n.style.display = n.dataset.for === v ? "block" : "none"); };
    sel.addEventListener("change", e => apply(e.target.value));
    return { sel: sel, apply: apply };
  };
  const T = axis("#seltext", "data-text", "tnote", "tcard");
  const B = axis("#selbg",   "data-bg",   "bnote", "bcard");
  document.querySelector("#ground").addEventListener("change", e =>
    root.setAttribute("data-ground", e.target.value));
  document.querySelectorAll(".srow[data-go]").forEach(r => r.addEventListener("click", () => {
    const a = r.dataset.go.startsWith("b-") ? B : T;
    a.sel.value = r.dataset.go; a.apply(r.dataset.go);
    document.querySelector("#selected").scrollIntoView({behavior:"smooth"});
  }));
  const cap = document.querySelector("#cap"), capout = document.querySelector("#capout");
  cap.addEventListener("input", () => {
    const v = +cap.value, off = v >= 0.30;
    capout.textContent = off ? "off" : "C " + v.toFixed(2);
    window.txChromaCap.set(off ? null : v);
  });
</script>
</body></html>')

writeLines(html, "dev/palette_preview.html")

# === SECTION: the self-test =======================================================================
stopifnot(apca("#888888", "#ffffff") == 63.1)                      # Myndex reference vector
old_maxC <- function(L, H) { lo <- 0; hi <- 0.4
  for (i in 1:30) { m <- (lo + hi)/2; if (in_gamut(L, m, H)) lo <- m else hi <- m }; lo }
g <- expand.grid(L = seq(0.05, 0.98, length.out = 20), H = seq(0, 350, by = 10))
stopifnot(max(abs(mapply(function(L,H) oklch_maxC(L,H) - old_maxC(L,H), g$L, g$H))) < 1/255)

message("text ramps")
for (p in TXTP) { s <- tscore(p, REF)
  message(sprintf("  %-26s |Lc| %2.0f>%2.0f / %2.0f>%2.0f  cvd %.3f  step %.3f  %s", p$name,
    s$lc[[1]][1], s$lc[[1]][4], s$lc[[2]][1], s$lc[[2]][4], min(s$gap), min(s$step),
    if (all(s$ok)) "ok" else paste("fails:", paste(TCHECKS[!s$ok], collapse = ", ")))) }
message("\nfills, under the settled text ramp")
for (p in BGP) { s <- bscore(p, REF)
  message(sprintf("  %-26s dL %s  step %.3f  WCAG %.2f-%.2f  text-on-fill Lc %2.0f  bare ink Lc %2.0f  %s",
    p$name, paste(sprintf("%.3f", s$dl[1:4]), collapse = " "), s$adj, min(s$wcag), max(s$wcag),
    s$onf, s$bare, if (all(s$ok)) "ok" else paste("fails:", paste(BCHECKS[!s$ok], collapse = ", ")))) }
message("\nwritten: dev/palette_preview.html  (", length(TXTP), " text x ", length(BGP),
        " fills x ", length(GROUNDS), " grounds x 3 vision modes)")
