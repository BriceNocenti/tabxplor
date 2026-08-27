# PURPOSE: choose tabxplor's DARK over/under palette by eye, the way the theme was chosen.
# ROLE: dev tool, .Rbuildignore'd.  Rscript dev/palette_preview.R -> dev/palette_preview.html
#   Real tabxplor tables on the real dark chrome, each rendered three times -- as seen, and as a
#   deuteranope and a protanope see it -- with a menu of candidate palettes and the chroma cap.
#
# THE DIAGNOSIS THE CANDIDATES ANSWER. Measured against each theme's own page and ink:
#
#             text rung 1        text rung 4        fills            what it means
#   light     L .66  3.0:1 page  L .47  8.1:1 page  L .97-.85       intensity = DARKER = more
#             7.0:1 from ink     2.6:1 from ink     1.1-1.6:1 page   contrast; the fill is a tint
#                                                                    and the black ink reads on it
#   dark      L .55  3.3:1 page  L .66  4.8:1 page  L .25-.34       intensity = lighter, but the
#             2.8:1 from ink     2.0:1 from ink     1.0-1.3:1 page   whole ramp sits BELOW the ink
#
# Three faults, and they are independent:
#   1. ⚠ EVERY DARK RUNG IS DIMMER THAN THE ORDINARY INK (#CDCBBC, L 0.84). A coloured cell is
#      therefore less visible than a plain one -- the exact opposite of the package's whole idea, and
#      what reads as "muddy". In light mode the ramp moves AWAY from the ink (black) into colour.
#   2. THE LADDER'S DYNAMIC RANGE IS HALVED. Light runs 2.3:1 -> 8.1:1 against its page, a factor of
#      3.5; dark runs 3.2:1 -> 5.3:1, a factor of 1.6. Rungs are hard to tell apart because there is
#      not much room between them.
#   3. THE FILLS ARE DARKER THAN THE PAGE (L 0.25 against 0.263): rung 1 measures 1.0:1 -- literally
#      invisible. They read as holes, where the light palette's fills read as clean bright shapes.
#
# ⚠ THE CONSTRAINT THAT MAKES THIS INTERESTING: the two channels are independent, so a cell can
#   carry text colour, a fill, or both -- but the fill is a `.tx-pill` span INSIDE the cell, and the
#   text colour sits on the cell. A LIGHT fill (the look that works) forces DARK text on it, while an
#   unfilled cell on a dark page needs LIGHT text. One ramp cannot do both. The `island` candidates
#   below resolve it in CSS: `td.p2 > .tx-pill` is expressible, so a text slot can take one colour
#   in the open and another inside a fill.
#
# WHAT THE LITERATURE ADDS (searched, not assumed): on a dark ground a saturated colour reads as
#   more saturated and can vibrate, so dark palettes want 15-25% LESS chroma than their light
#   equivalents; and luminance -- not hue or chroma -- is what carries legibility for text, while a
#   filled area needs only 3:1 where text needs 4.5:1. That is why a fill may be far lighter than a
#   text colour of the "same" rung, and why the lifted candidates cut chroma as they raise L.

source("dev/heading_ladders.R")             # oklch_hex(), contrast(), the OKLCH converters
suppressMessages(pkgload::load_all(".", quiet = TRUE))

options(tabxplor.lang = "en", tabxplor.tab_kable_css = FALSE, tabxplor.cleannames = TRUE,
        tabxplor.tab_kable_tooltips = FALSE)
Sys.setenv(LANGUAGE = "en")

PAGE <- "#21252b"; INK <- "#CDCBBC"; PANEL <- "#282c34"; BORDER <- "#3e4451"
HEADS <- c("#FEF1A1", "#F5E9A3", "#ECE2A4", "#E5DB9D", "#DED396", "#D6CC8F")   # warm-95-10

# === SECTION: the candidates ======================================================================
# Each is four ramps of four rungs, faint -> strong. `pill` is the ink a TEXT slot takes when it
# sits inside a fill; NULL means the text ramp is readable on the fills as it stands.

LIGHT_TEXT_OVER  <- c("#02a5b3", "#0891c9", "#0267c7", "#300dfd")
LIGHT_TEXT_UNDER <- c("#dca331", "#de7c01", "#dd5301", "#d60103")
LIGHT_BG_OVER    <- c("#dffcff", "#d7efff", "#cee3ff", "#bbccff")
LIGHT_BG_UNDER   <- c("#fff4e1", "#ffe6d3", "#ffd7c8", "#ffbaaf")

# A ramp from OKLCH. ⚠ IT NEVER RECORDS A CHROMA IT DOES NOT HAVE: every rung is clamped to what
# sRGB can actually show at that lightness and hue, the achieved value is what the sidebar prints,
# and a clamp is reported at generation time. That is not pedantry here -- it is the whole of fault 3
# below, and it went unnoticed once already.
ramp <- function(L, C, H, label = "") {
  n    <- max(length(L), length(C), length(H))
  L <- rep_len(L, n); C <- rep_len(C, n); H <- rep_len(H, n)
  cmax <- vapply(seq_len(n), function(i) oklch_maxC(L[i], H[i]), numeric(1))
  ok   <- pmin(C, cmax)
  if (any(ok < C - 1e-4) && nzchar(label))
    message("  clamped in ", label, ": asked ", paste(sprintf("%.2f", C), collapse = " "),
            " -> ", paste(sprintf("%.3f", ok), collapse = " "))
  structure(vapply(seq_len(n), function(i) oklch_hex(L[i], ok[i], H[i]), character(1)),
            L = L, C = ok, H = H)
}
s4 <- function(a, b) seq(a, b, length.out = 4)

# ⚠ THE GAMUT IS WHY `lifted` FAILS, and the numbers are worth keeping. Max sRGB chroma:
#
#            h205  h235  h255  h270  |  h80   h60   h42   h29     (the light palette's own hues)
#   L 0.90   0.105 0.057 0.049 0.048 | 0.094 0.065 0.055 0.052
#   L 0.80   0.137 0.119 0.103 0.100 | 0.166 0.140 0.120 0.115
#   L 0.70   0.120 0.149 0.160 0.156 | 0.145 0.164 0.199 0.191
#   L 0.60   0.103 0.127 0.198 0.217 | 0.124 0.141 0.181 0.246
#
# The rung-1 hues (cyan, amber) peak around L 0.80-0.85; the rung-4 hues (violet, red) peak around
# L 0.55-0.60 and are nearly grey by L 0.90. So a ramp that RISES in lightness makes rung 4 the
# LEAST saturated rung -- which is exactly the "not striking" of `lifted`, and it is a fact of sRGB,
# not a tuning mistake. Anything that wants rung 4 to be the strongest must keep it low enough to
# hold chroma: flat lightness with a rising chroma, or a descent.

pal <- function(name, note, text_over, text_under, bg_over, bg_under,
                pill_ink = NULL, chroma_may_fall = FALSE) {
  # ⚠ CHECKED, NOT TRUSTED: after clamping, chroma must still RISE with the rung. A ceiling reached
  # by rung 3 makes rung 4 no stronger, which inverts the very thing the ladder encodes -- and it is
  # invisible in the asked-for numbers.
  for (nm in c("text_over", "text_under")) {
    v <- get(nm); C <- attr(v, "C")
    if (!chroma_may_fall && !is.null(C) && any(diff(C) < -1e-4))
      warning("chroma falls in ", name, " ", nm, ": ", paste(sprintf("%.3f", C), collapse = " "),
              call. = FALSE)
  }
  list(name = name, id = gsub("[^A-Za-z0-9-]", "-", name), note = note,
       text_over = text_over, text_under = text_under, bg_over = bg_over, bg_under = bg_under,
       pill_ink = pill_ink)
}

H_OVER  <- c(205, 235, 255, 270)      # the light palette's exact hues, kept throughout
H_UNDER <- c(80, 60, 42, 29)

PALETTES <- list(
  pal("current", "what tabxplor ships today -- the baseline the others are judged against",
      c("#028282", "#0286b1", "#4687d8", "#6987ff"), c("#867002", "#b87501", "#ec6f02", "#ff626b"),
      c("#002828", "#012d3f", "#122e5d", "#202e7a"), c("#292100", "#3b2300", "#4f2100", "#720119")),

  pal("light-as-is", "the LIGHT palette, unchanged, on the dark page",
      LIGHT_TEXT_OVER, LIGHT_TEXT_UNDER, LIGHT_BG_OVER, LIGHT_BG_UNDER),

  pal("light-reversed", "the same colours, rungs reversed: the strongest deviation is the lightest",
      rev(LIGHT_TEXT_OVER), rev(LIGHT_TEXT_UNDER), LIGHT_BG_OVER, LIGHT_BG_UNDER),

  pal(chroma_may_fall = TRUE, "lifted",
      "lightness rising -- kept to show what the gamut does to rung 4: its chroma FALLS 0.11 to 0.05",
      ramp(s4(0.78, 0.90), s4(0.11, 0.14), H_OVER,  "lifted over"),
      ramp(s4(0.78, 0.90), s4(0.11, 0.14), H_UNDER, "lifted under"),
      ramp(s4(0.33, 0.45), s4(0.04, 0.10), H_OVER),
      ramp(s4(0.33, 0.45), s4(0.04, 0.10), H_UNDER)),

  # ---- chroma carries the ladder, lightness barely moves --------------------------------------
  pal("chroma-led", "one lightness band, the rungs told apart by saturation alone",
      ramp(c(0.78, 0.77, 0.75, 0.74), c(0.07, 0.10, 0.13, 0.15), H_OVER,  "chroma-led over"),
      ramp(c(0.78, 0.77, 0.75, 0.74), c(0.07, 0.11, 0.15, 0.17), H_UNDER, "chroma-led under"),
      ramp(c(0.36, 0.40, 0.44, 0.48), c(0.03, 0.05, 0.08, 0.11), H_OVER),
      ramp(c(0.36, 0.40, 0.44, 0.48), c(0.03, 0.06, 0.09, 0.12), H_UNDER)),

  pal("chroma-led-low", "the same band offset darker, where the rung-4 hues hold more chroma",
      ramp(c(0.74, 0.73, 0.71, 0.69), c(0.07, 0.11, 0.16, 0.19), H_OVER,  "chroma-led-low over"),
      ramp(c(0.74, 0.73, 0.71, 0.69), c(0.07, 0.12, 0.17, 0.21), H_UNDER, "chroma-led-low under"),
      ramp(c(0.34, 0.39, 0.44, 0.49), c(0.03, 0.06, 0.09, 0.13), H_OVER),
      ramp(c(0.34, 0.39, 0.44, 0.49), c(0.03, 0.06, 0.10, 0.14), H_UNDER)),

  # ---- the light palette's hues, its shape, lifted onto a dark page ------------------------------
  pal("light-hues-descend",
      "the light palette's own logic on a dark page: bright and pale to deep and saturated",
      ramp(c(0.86, 0.80, 0.74, 0.68), c(0.10, 0.12, 0.15, 0.18), H_OVER,  "descend over"),
      ramp(c(0.86, 0.80, 0.74, 0.68), c(0.11, 0.14, 0.17, 0.20), H_UNDER, "descend under"),
      ramp(c(0.38, 0.42, 0.46, 0.50), c(0.03, 0.06, 0.09, 0.13), H_OVER),
      ramp(c(0.38, 0.42, 0.46, 0.50), c(0.03, 0.06, 0.10, 0.14), H_UNDER)),

  pal("light-hues-tilt",
      "a gentle fall, not an arch -- the lightness only ever descends here (the old name lied)",
      ramp(c(0.84, 0.79, 0.74, 0.70), c(0.10, 0.12, 0.14, 0.155), H_OVER,  "arch over"),
      ramp(c(0.84, 0.79, 0.74, 0.70), c(0.11, 0.14, 0.17, 0.190), H_UNDER, "arch under"),
      ramp(c(0.37, 0.41, 0.45, 0.49), c(0.03, 0.06, 0.09, 0.13), H_OVER),
      ramp(c(0.37, 0.41, 0.45, 0.49), c(0.03, 0.06, 0.10, 0.14), H_UNDER)),

# ---- the trade curve, and four points on it ------------------------------------------------------
# Searching the space says two things. First, THE CEILING HAS A MINIMUM EXACTLY WHERE RUNG 4 SITS:
# at L 0.74 it is 0.128 at h205, 0.148 at h230, and only 0.133 at h270 -- the blue-violet end is the
# worst place in the cool region for chroma, and it climbs again past it (0.140 at h285, 0.159 at
# h300). Second, everything else is one trade, and it is worth reading as a curve:
#
#   what rung 4 can hold, per lightness it is allowed to sink to
#     L4     0.84  0.82  0.80  0.78  0.76  0.74  0.72  0.70
#     h270   .079  .090  .100  .111  .122  .133  .145  .156
#     h285   .083  .094  .105  .117  .128  .140  .152  .164
#     h29    .088  .101  .115  .129  .143  .159  .175  .191   (the red end)
#
# The ink is L 0.84. So "rung 4 as bright as the ink" costs it two thirds of its chroma, and every
# 0.02 of lightness given back buys about 0.011. The four below are points on that curve: the same
# ladder shape (chroma at 42/60/80/100 % of what rung 4 holds), differing only in where they stop.
  pal("flat-078", "flat at L 0.78 -- the calmest the gamut allows if the chroma must still rise",
      ramp(0.78, 0.111 * c(.42, .60, .80, 1), H_OVER,  "flat-078 over"),
      ramp(0.78, 0.129 * c(.42, .60, .80, 1), H_UNDER, "flat-078 under"),
      ramp(c(0.36, 0.40, 0.44, 0.48), c(0.03, 0.05, 0.08, 0.11), H_OVER),
      ramp(c(0.36, 0.40, 0.44, 0.48), c(0.03, 0.06, 0.09, 0.12), H_UNDER)),

  pal("tilt-076", "a gentle fall to L 0.76, which buys rung 4 a tenth more chroma",
      ramp(seq(0.82, 0.76, length.out = 4), 0.122 * c(.42, .60, .80, 1), H_OVER,  "tilt-076 over"),
      ramp(seq(0.82, 0.76, length.out = 4), 0.143 * c(.42, .60, .80, 1), H_UNDER, "tilt-076 under"),
      ramp(c(0.36, 0.40, 0.44, 0.48), c(0.03, 0.05, 0.08, 0.11), H_OVER),
      ramp(c(0.36, 0.40, 0.44, 0.48), c(0.03, 0.06, 0.09, 0.12), H_UNDER)),

  # Only the LAST rung leaves the band. Three rungs stay bright and even; rung 4 dips for the chroma
  # that makes it read as the strong one -- which is the thing `lifted` could not do.
  pal("step-4", "three rungs level and bright, and only rung 4 drops for the chroma it needs",
      ramp(c(0.82, 0.81, 0.79, 0.72), c(0.061, 0.087, 0.116, 0.145), H_OVER,  "step-4 over"),
      ramp(c(0.82, 0.81, 0.79, 0.72), c(0.073, 0.105, 0.140, 0.175), H_UNDER, "step-4 under"),
      ramp(c(0.36, 0.40, 0.44, 0.48), c(0.03, 0.05, 0.08, 0.11), H_OVER),
      ramp(c(0.36, 0.40, 0.44, 0.48), c(0.03, 0.06, 0.09, 0.12), H_UNDER)),

  # The hue offset the search found: rung 4 at 285 instead of 270 is worth about 5% more chroma at
  # the same lightness, and the family still reads cyan -> blue -> violet.
  pal("step-4-285", "the same, with rung 4 moved to hue 285 where the ceiling is higher",
      ramp(c(0.82, 0.81, 0.79, 0.72), c(0.064, 0.091, 0.122, 0.152), c(205, 240, 265, 285),
           "step-4-285 over"),
      ramp(c(0.82, 0.81, 0.79, 0.72), c(0.073, 0.105, 0.140, 0.175), c(85, 62, 42, 25),
           "step-4-285 under"),
      ramp(c(0.36, 0.40, 0.44, 0.48), c(0.03, 0.05, 0.08, 0.11), c(205, 240, 265, 285)),
      ramp(c(0.36, 0.40, 0.44, 0.48), c(0.03, 0.06, 0.09, 0.12), c(85, 62, 42, 25))),

  # ---- light boxes, ONE text ramp ---------------------------------------------------------------
  # ⚠ The hard constraint: a rung has ONE text colour, or the legend cannot be read. So the ramp has
  # to work in the open AND on a light fill, which puts it in the middle band -- bright enough
  # against the page (L 0.263), dark enough against a fill at L 0.88+. The only inversion allowed is
  # the ink of a cell that has a fill and NO text slot, which otherwise takes the page's own ink.
  pal("boxes-mid", "light fills, one mid text ramp that reads on both, dark ink on bare fills",
      ramp(c(0.72, 0.69, 0.66, 0.63), c(0.11, 0.14, 0.18, 0.21), H_OVER,  "boxes-mid over"),
      ramp(c(0.72, 0.69, 0.66, 0.63), c(0.12, 0.15, 0.19, 0.23), H_UNDER, "boxes-mid under"),
      LIGHT_BG_OVER, LIGHT_BG_UNDER, pill_ink = "#1b1f24"),

  pal("boxes-flat", "the same idea with a flat text lightness: saturation alone ranks the rungs",
      ramp(c(0.69, 0.68, 0.67, 0.66), c(0.09, 0.13, 0.16, 0.185), H_OVER,  "boxes-flat over"),
      ramp(c(0.69, 0.68, 0.67, 0.66), c(0.10, 0.14, 0.18, 0.210), H_UNDER, "boxes-flat under"),
      LIGHT_BG_OVER, LIGHT_BG_UNDER, pill_ink = "#1b1f24"),

  pal("boxes-soft", "fills a shade deeper so the text ramp can sit higher and stay readable",
      ramp(c(0.78, 0.75, 0.72, 0.69), c(0.10, 0.13, 0.17, 0.20), H_OVER,  "boxes-soft over"),
      ramp(c(0.78, 0.75, 0.72, 0.69), c(0.11, 0.14, 0.18, 0.22), H_UNDER, "boxes-soft under"),
      ramp(c(0.90, 0.88, 0.86, 0.84), c(0.04, 0.05, 0.06, 0.07), H_OVER),
      ramp(c(0.90, 0.88, 0.86, 0.84), c(0.04, 0.05, 0.06, 0.07), H_UNDER),
      pill_ink = "#1b1f24")
)

# === SECTION: colour-vision simulation ============================================================
# Machado et al. (2009), through colorspace's matrices -- the same route dev/color_palette_tools.R
# takes. Deutan and protan only: together they are ~8% of men, tritan is ~0.01%.

# ⚠ The matrix is defined in LINEAR RGB, so the sRGB gamma has to come off and go back on -- the
# same steps dev/color_palette_tools.R's simulate_cvd_farver() takes. Applying it to gamma-encoded
# values looks plausible and is wrong.
# ⚠ And `pmax(0, m)` drops a matrix's dim (the scalar is the first argument); `pmax(m, 0)` keeps it.
cvd <- function(hex, type) {
  M   <- colorspace::interpolate_cvd_transform(
    switch(type, deutan = colorspace::deutanomaly_cvd, protan = colorspace::protanomaly_cvd), 1)
  lin <- srgb_linear(farver::decode_colour(hex, to = "rgb") / 255)
  out <- srgb_encode(as.matrix(lin) %*% t(M))
  farver::encode_colour(round(pmin(pmax(out, 0), 1) * 255), from = "rgb")
}

# === SECTION: the stylesheet ======================================================================
# ⚠ SPECIFICITY: tab_css()'s dark layer is `[data-bs-theme=dark] .tabxplor-tab .p1` (0,3,0). A
# candidate's rules carry the page attribute plus `html`, so (0,3,1) -- always higher, whatever the
# source order. The CVD copies add one class more again, so they win inside their own wrapper.

slot_css <- function(p, prefix) {
  txt <- c(paste0(".p", 1:4), paste0(".m", 1:4))
  bgc <- c(paste0(".o", 1:4), paste0(".u", 1:4))
  out <- c(
    paste0(prefix, " .tabxplor-tab ", txt, "{color:", c(p$text_over, p$text_under), ";}"),
    paste0(prefix, " .tabxplor-tab ", bgc,
           "{background-color:", c(p$bg_over, p$bg_under), ";}"))
  # ⚠ THE ONLY INVERSION ALLOWED. A rung has ONE text colour or its legend cannot be read, so no rule
  # here repaints a text slot. This one reaches only a cell that has a FILL AND NO TEXT SLOT, whose
  # ink would otherwise be the page's own -- unreadable once the fill is light.
  if (!is.null(p$pill_ink))
    out <- c(out, paste0(prefix, " .tabxplor-tab td",
                         paste0(":not(", txt, ")", collapse = ""),
                         " .tx-pill{color:", p$pill_ink, ";}"))
  paste(out, collapse = "\n")
}

palette_css <- function(p) paste(c(
  slot_css(p, sprintf('html[data-pal="%s"]', p$id)),
  vapply(c("deutan", "protan"), function(t) {
    q <- p
    for (f in c("text_over", "text_under", "bg_over", "bg_under"))
      q[[f]] <- cvd(q[[f]], t)
    if (!is.null(q$pill_ink)) q$pill_ink <- cvd(q$pill_ink, t)
    slot_css(q, sprintf('html[data-pal="%s"] .cvd-%s', p$id, t))
  }, character(1))), collapse = "\n")

# What the sidebar prints: the whole selected scale, as OKLCH and as measured. `L`/`C`/`H` come off
# the ramp itself, so a clamped rung shows the chroma it HAS, never the one that was asked for.
scale_rows <- function(p) {
  one <- function(v, slot) {
    L <- attr(v, "L"); C <- attr(v, "C"); H <- attr(v, "H")
    vapply(seq_along(v), function(i) sprintf(
      "<tr><td>%s%d</td><td class='sw'><span style='background:%s'></span></td><td><code>%s</code></td>%s<td>%.1f</td></tr>",
      slot, i, v[i], v[i],
      if (is.null(L)) "<td colspan=3 class='na'>fixed</td>" else
        sprintf("<td>%.2f</td><td>%.3f</td><td>%.0f</td>", L[i], C[i], H[i]),
      contrast(v[i], if (slot %in% c("p", "m")) PAGE else INK)), character(1))
  }
  paste0("<table class='scale'><tr><th>slot</th><th></th><th>hex</th><th>L</th><th>C</th>",
         "<th>h</th><th>ratio</th></tr>",
         paste(c(one(p$text_over, "p"), one(p$text_under, "m"),
                 one(p$bg_over, "o"),   one(p$bg_under, "u")), collapse = ""), "</table>")
}

# === SECTION: the tables ==========================================================================

gss <- gss_cat_data_formatting()
tabs <- list(
  # Four row variables against four column ones, so every rung of both directions actually appears:
  # measured on this call, p1:21 p2:16 p3:4 p4:14 and m1:18 m2:19 m3:4 m4:13. A single pair reaches
  # only the first three rungs, which is no test of a ladder.
  "Text channel only -- every rung, both directions" =
    tab(gss, c(race, rincome, marital, relig), c(party3, married, income25k, black),
        pct = "row", color = "difference"),
  "Both channels -- deviation on the text, ratio on the fill" =
    tab(gss, c(race, rincome), c(party3, relig), pct = "row",
        color = c("difference", "ratio"), ref = 1),
  # The background channel ALONE: no text slot anywhere, so every filled cell shows the fill against
  # the page and the ink that sits on it -- which is the one reading the `pill_ink` rule changes.
  "Background channel only -- the fills, with no text colour to help them" =
    tab(gss, c(race, rincome), c(party3, relig), pct = "row",
        color = c("no", "difference"), ref = 1),

  "Greyed where not significant, so the ladder is read beside a grey" =
    tab(gss, relig, c(married, income25k), pct = "row", levels = "first",
        color = "difference", color_signif = "grey_non_signif"))

one_table <- function(html, title) paste0(
  '<h3>', title, '</h3>\n', html,
  '\n<details class="cvd"><summary>colour vision &mdash; deuteranopia and protanopia</summary>',
  '<div class="trio">',
  '<div class="cvd-deutan"><div class="lab">deuteranopia &mdash; ~6% of men</div>', html, '</div>',
  '<div class="cvd-protan"><div class="lab">protanopia &mdash; ~2% of men</div>', html, '</div>',
  '</div></details>')

tables_html <- paste(vapply(names(tabs), function(nm)
  one_table(as.character(tab_html(tabs[[nm]])), nm), character(1)), collapse = "\n")

# === SECTION: the page ============================================================================

opt <- paste0('<option value="', vapply(PALETTES, function(p) p$id, character(1)), '"',
              c(" selected", rep("", length(PALETTES) - 1)), '>',
              vapply(PALETTES, function(p) p$name, character(1)), '</option>', collapse = "")

notes <- paste0('<div class="pnote" data-for="', vapply(PALETTES, function(p) p$id, character(1)),
                '">', vapply(PALETTES, function(p) p$note, character(1)), '</div>', collapse = "")

scales <- paste0('<div class="pscale" data-for="', vapply(PALETTES, function(p) p$id, character(1)),
                 '">', vapply(PALETTES, scale_rows, character(1)), '</div>', collapse = "")

html <- paste0('<!doctype html>
<html lang="en" data-bs-theme="dark" data-pal="current">
<head>
<meta charset="utf-8"><meta name="viewport" content="width=device-width, initial-scale=1">
<title>tabxplor - dark over/under palettes</title>
<style>', tab_css(theme = "auto", style_tag = FALSE), '</style>
<style>', paste(vapply(PALETTES, palette_css, character(1)), collapse = "\n"), '</style>
<style>
  body{background:', PAGE, ';color:', INK, ';margin:0;line-height:1.5;
       font-family:"DejaVu Sans","Source Sans Pro",system-ui,sans-serif;}
  .layout{display:flex;align-items:flex-start;}
  .panel{flex:0 0 15rem;position:sticky;top:0;height:100vh;overflow-y:auto;background:', PANEL, ';
         border-right:1px solid ', BORDER, ';padding:.9rem 1rem;font-size:.8rem;}
  .wrap{flex:1 1 auto;min-width:0;padding:1rem 1.5rem 40vh;}
  h1,h2,h3{font-weight:bold;line-height:1.25;}
  h1{color:', HEADS[1], ';font-size:1.8rem;} h2{color:', HEADS[2], ';font-size:1.45rem;}
  h3{color:', HEADS[3], ';font-size:1.15rem;margin-top:2.2rem;}
  .trio{display:flex;gap:1.4rem;flex-wrap:wrap;align-items:flex-start;}
  .lab{font-size:.72rem;opacity:.55;margin-bottom:.3rem;}
  .pnote{display:none;opacity:.75;margin:.4rem 0 1.4rem;}
  .pscale{display:none;}
  html[data-pal="current"] .pnote[data-for="current"],
  html[data-pal="current"] .pscale[data-for="current"]{display:block;}
  .scale{border-collapse:collapse;font-size:.68rem;width:100%;}
  .scale th{opacity:.5;font-weight:normal;text-align:left;}
  .scale td{padding:0 .25rem 0 0;white-space:nowrap;}
  .scale .sw span{display:inline-block;width:1.1rem;height:.7rem;border-radius:2px;}
  .scale .na{opacity:.4;}
  details.cvd{margin:.6rem 0 0;opacity:.85;}
  details.cvd summary{font-size:.78rem;opacity:.6;cursor:pointer;}
  select,input{font:inherit;font-size:.8rem;}
  label{display:block;margin-bottom:.7rem;}
</style>
</head>
<body>
<div class="layout">
<nav class="panel">
  <label>palette<br><select id="pal">', opt, '</select></label>
  <label>chroma cap &mdash; <span id="capout">off</span><br>
    <input id="cap" type="range" min="0.02" max="0.30" step="0.01" value="0.30" style="width:100%;">
  </label>
  <div style="opacity:.55;margin-bottom:.8rem;">The colour-vision copies are folded under each
  table. The cap applies to everything on the page at once.</div>
  ', scales, '
</nav>
<div class="wrap">
<h1>The dark over / under palette</h1>
', notes, '
', tables_html, '
</div></div>
<script>', chroma_cap_js <- paste(readLines("dev/chroma_cap.js", warn = FALSE), collapse = "\n"), '</script>
<script>
  const root = document.documentElement;
  document.querySelector("#pal").addEventListener("change", e => {
    root.setAttribute("data-pal", e.target.value);
    document.querySelectorAll(".pnote, .pscale").forEach(n =>
      n.style.display = n.dataset.for === e.target.value ? "block" : "none");
  });
  const cap = document.querySelector("#cap"), capout = document.querySelector("#capout");
  cap.addEventListener("input", () => {
    const v = +cap.value, off = v >= 0.30;
    capout.textContent = off ? "off" : "C " + v.toFixed(2);
    window.txChromaCap.set(off ? null : v);
  });
</script>
</body></html>')

writeLines(html, "dev/palette_preview.html")
message("written: dev/palette_preview.html  (", length(PALETTES), " palettes x 3 vision modes)")
