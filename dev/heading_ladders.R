# PURPOSE: the heading-green ladders, and the OKLCH maths behind them -- the ONE definition.
# ROLE: sourced by both previews (dev/heading_greens_preview.R writes the specimen page,
#   dev/site_theme_preview.R offers the same ladders on the real site chrome). It writes nothing.
#
# WHY A LADDER AND NOT A COLOUR: on a dark ground prominence IS perceptual lightness, so six levels
#   want six values of L, not one green used six times. The rest of the palette fixes the room it
#   has to fit in (measured, not guessed):
#
#     body text   #CDCBBC   L 0.840  C 0.020  h 101   <- what a heading must out-rank
#     code string #a9dc76   L 0.836  C 0.142  h 131   <- the green already in every code block
#     .pertinent  #05ae30   L 0.653  C 0.204  h 145   <- the annotation green
#     .reflexivite#13c097   L 0.720  C 0.140  h 170   <- the annotation teal
#     page        #21252b   L 0.263
#
# ⚠ THE DIAGNOSIS THAT SHAPED THE FIRST TWELVE: the original #92be62 is L 0.749, C 0.130, h 130.1 --
#   the SAME hue as the code-string green, at LOWER lightness than the body text. A heading darker
#   than the prose it heads cannot lead the page, and one on the string hue reads as a dimmed code
#   token. Every proposal since starts at L >= 0.86 and most move off hue 130.
#
# ⚠ THE GAMUT IS NOT FLAT. Green chroma peaks near L 0.86 at hue 140 (max 0.277 in sRGB) and
#   collapses above L 0.92 (0.10 at hue 140). A "very light, very saturated" green does not exist:
#   ask for one and oklch_hex() reduces the chroma until it fits, as CSS Color 4 does.

# === SECTION: OKLCH <-> sRGB ======================================================================
# A port of the Ottosson matrices. Out-of-gamut colours lose CHROMA, never lightness or hue: that is
# what keeps a ladder's steps even when one rung asks for more saturation than sRGB can show.

M1  <- matrix(c(0.8189330101, 0.3618667424, -0.1288597137,
                0.0329845436, 0.9293118715,  0.0361456387,
                0.0482003018, 0.2643662691,  0.6338517070), 3, 3, byrow = TRUE)
M2  <- matrix(c(0.2104542553,  0.7936177850, -0.0040720468,
                1.9779984951, -2.4285922050,  0.4505937099,
                0.0259040371,  0.7827717662, -0.8086757660), 3, 3, byrow = TRUE)
RGB2XYZ <- matrix(c(0.4124564, 0.3575761, 0.1804375,
                    0.2126729, 0.7151522, 0.0721750,
                    0.0193339, 0.1191920, 0.9503041), 3, 3, byrow = TRUE)

srgb_encode <- function(c) ifelse(c <= 0.0031308, 12.92 * c, 1.055 * c^(1 / 2.4) - 0.055)
srgb_linear <- function(c) ifelse(c <= 0.04045, c / 12.92, ((c + 0.055) / 1.055)^2.4)

oklch_rgb <- function(L, C, H) {
  ab  <- c(L, C * cos(H * pi / 180), C * sin(H * pi / 180))
  lms <- solve(M2, ab)^3
  as.vector(solve(RGB2XYZ) %*% solve(M1, lms))
}
in_gamut <- function(L, C, H, eps = 1e-4) all(oklch_rgb(L, C, H) >= -eps & oklch_rgb(L, C, H) <= 1 + eps)

oklch_hex <- function(L, C, H) {
  if (!in_gamut(L, C, H)) {                    # binary-search the chroma back into sRGB
    lo <- 0; hi <- C
    for (i in 1:40) { mid <- (lo + hi) / 2; if (in_gamut(L, mid, H)) lo <- mid else hi <- mid }
    C <- lo
  }
  v <- pmax(0, pmin(1, oklch_rgb(L, C, H)))
  sprintf("#%02X%02X%02X", round(srgb_encode(v[1]) * 255),
          round(srgb_encode(v[2]) * 255), round(srgb_encode(v[3]) * 255))
}

rel_lum  <- function(hex) {
  v <- srgb_linear(strtoi(substring(hex, c(2, 4, 6), c(3, 5, 7)), 16L) / 255)
  sum(v * c(0.2126, 0.7152, 0.0722))
}
contrast <- function(a, b) { l <- sort(c(rel_lum(a), rel_lum(b)), decreasing = TRUE)
                             (l[1] + 0.05) / (l[2] + 0.05) }

# === SECTION: the twelve ladders ==================================================================
# Each row is one proposal: six values of L, six of C, six of hue. A scalar is recycled, so a ladder
# that moves on one axis states only that axis. `note` is what the specimen is headed with.

lad <- function(name, note, L, C, H) {
  n <- 6L
  # WARNING: `id` is not decoration. A name carrying a dot ("capped-0.18") makes `#capped-0.18 h1`
  # parse as an id PLUS a class, so the specimen silently loses its colours.
  list(name = name, id = gsub("[^A-Za-z0-9-]", "-", name), note = note,
       L = rep_len(L, n), C = rep_len(C, n), H = rep_len(H, n))
}
seq6 <- function(from, to) seq(from, to, length.out = 6)

# `round()` tags a block so the sidebar can group them, and enforces the one rule that holds across
# every proposal: chroma never RISES as the ladder descends. A lower heading that is more saturated
# than the one above it reads as more important, whatever its lightness says.
round_of <- function(group, ls) lapply(ls, function(l) {
  if (any(diff(l$C) > 1e-9))
    stop("chroma rises down the ladder in '", l$name, "': ", paste(round(l$C, 3), collapse = " "))
  l$group <- group; l
})

LADDERS <- c(round_of("first round -- the twelve", list(
  lad("lightness",     "one hue, one chroma, six lightnesses -- the plain ladder",
      L = seq6(0.90, 0.65), C = 0.16,             H = 142),
  lad("theme-hue",     "the same hue as the code strings, but properly bright at the top",
      L = seq6(0.90, 0.68), C = 0.15,             H = 130),
  lad("recede",        "lightness AND chroma both fall away: each level recedes twice over",
      L = seq6(0.90, 0.68), C = seq6(0.18, 0.08), H = 145),
  lad("chroma-only",   "one lightness for all six, saturation alone carries the rank",
      L = 0.86,             C = seq6(0.20, 0.05), H = 145),
  lad("hue-drift",     "one lightness, one chroma, the hue walking yellow-green to teal",
      L = 0.86,             C = 0.14,             H = seq6(122, 162)),
  lad("capped-0.18",   "the harmonizer method: chroma capped at 0.18, lightness does the work",
      L = seq6(0.92, 0.62), C = 0.18,             H = 145),
  lad("vivid-top",     "h1 and h2 vivid, the tail muted -- the loudness is spent at the top",
      L = c(0.89, 0.84, 0.79, 0.75, 0.71, 0.67),
      C = c(0.19, 0.17, 0.12, 0.09, 0.07, 0.05),  H = 150),
  lad("wide-sage",     "a wide span at low chroma: nearly a neutral, ranked by lightness alone",
      L = seq6(0.93, 0.58), C = 0.07,             H = 148),
  lad("teal-green",    "hue 160, as far from the string green as green still goes",
      L = seq6(0.90, 0.65), C = 0.13,             H = 160)
)), round_of("second round -- the shortlist's territory", list(
  # ---- the second round: the shortlist's own territory ------------------------------------------
  # capped-0.18 and vivid-top's first two rungs, refined. Two measured facts shape all eight:
  #   - capped-0.18 BOTTOMS OUT INSIDE the annotation green. Its h5/h6 sit 0.036 and 0.039 from
  #     .pertinent (#05ae30) in OKLab -- at that distance they are the same colour. So no rung here
  #     goes below L 0.72, which is what keeps the ladder clear of it.
  #   - vivid-top's TAIL drifts toward .reflexivite (0.070 at h4) because its chroma falls to 0.05.
  #     A desaturated green at that lightness IS the teal. So chroma stays >= 0.15 throughout.
  # What is left to choose is the hue (142 peaks in chroma, 150 is vivid-top's) and how small the
  # chroma steps are -- which is what these vary.
  lad("capped-145",    "capped-0.18's shape, stopped before it reaches the annotation green",
      L = seq6(0.90, 0.72), C = 0.18,             H = 145),
  lad("capped-150",    "the same again at vivid-top's own hue",
      L = seq6(0.90, 0.72), C = 0.18,             H = 150),
  lad("two-tier",      "h1-h2 vivid, then one chroma step down and nothing else moves but lightness",
      L = c(0.89, 0.84, 0.80, 0.77, 0.74, 0.71),
      C = c(0.19, 0.19, 0.17, 0.17, 0.17, 0.17),  H = 150),
  lad("small-steps",   "a gentle chroma decline alongside the lightness -- both move, neither much",
      L = seq6(0.90, 0.72), C = seq6(0.19, 0.16), H = 147),
  lad("tail-only",     "chroma flat for the first four, easing only on the last two",
      L = seq6(0.90, 0.72), C = c(0.18, 0.18, 0.18, 0.18, 0.16, 0.14), H = 147),
  lad("peak-chroma",   "hue 142, where green holds the most chroma sRGB can show",
      L = seq6(0.89, 0.73), C = 0.20,             H = 142),
  lad("top-heavy",     "the brightest top of all, easing by a hair at every rung",
      L = c(0.90, 0.86, 0.81, 0.78, 0.75, 0.72),
      C = c(0.20, 0.19, 0.18, 0.18, 0.17, 0.17),  H = 150)

# ---- the third round: eased ----------------------------------------------------------------------
# All at hue 150. The brief: a dense page is already vibrant -- the annotation spans sit at mid
# lightness and there are one or two per line -- so a heading should calm the page rather than add to
# it. That means LIGHTNESS HIGH AND CHROMA LOW: bright enough to lead, pale enough not to shout.
# ⚠ The gamut agrees, which is why this family is coherent rather than a compromise: at hue 150 the
#   ceiling is C 0.121 at L 0.93 and 0.083 at L 0.95. Up there a saturated green does not exist, so
#   asking for one only gets it clipped. These ask for what is actually available.
# ⚠ The cost is the lightness STEP. Six rungs inside a 0.08 span is 0.016 apart -- visible side by
#   side, marginal a page apart. That is the trade the whole round is about, so the span is varied
#   from 0.08 to 0.12 and one ladder spends its difference on chroma instead.
)), round_of("third round -- eased, hue 150", list(
  lad("airy-tight",    "the tightest: six rungs inside 0.08 of lightness, one flat chroma",
      L = seq6(0.93, 0.85), C = 0.12,             H = 150),
  lad("airy-wide",     "the same calm colour over a span half again as wide",
      L = seq6(0.93, 0.81), C = 0.12,             H = 150),
  lad("airy-ease",     "lightness and chroma both easing, neither by much",
      L = seq6(0.92, 0.82), C = seq6(0.14, 0.10), H = 150),
  lad("airy-lead",     "one clear step from h1 to h2, then a calm even ramp",
      L = c(0.93, 0.885, 0.865, 0.845, 0.825, 0.805), C = 0.12, H = 150),
  lad("airy-chroma",   "lightness nearly flat: the rank is carried by chroma alone",
      L = seq6(0.91, 0.85), C = seq6(0.16, 0.08), H = 150),
  lad("airy-soft",     "the most eased of all -- pale throughout, and the bland edge of the family",
      L = seq6(0.94, 0.84), C = 0.09,             H = 150)

# ---- landing on the annotation level -------------------------------------------------------------
# The annotation spans are the other coloured thing on a dense page, and they sit in a band: measured
# over the eleven, L runs 0.585 (.non) to 0.781 (.resultat), median 0.667. A ladder whose LAST rung
# lands in that band ends where the rest of the page's colour lives, instead of above or below it --
# so h6 stops being a heading that outranks an annotation beside it.
# ⚠ That bottom is also .pertinent's own neighbourhood (#05ae30, L 0.653, hue 145), and what keeps
#   these clear of it is CHROMA, not lightness: .pertinent is C 0.204, these are 0.09-0.12, which
#   measures 0.090 and 0.119 away in OKLab -- against the 0.036 that made capped-0.18's tail collide.
)), round_of("fourth -- h6 at the annotation level", list(
  lad("ann-floor",     "h1 bright, h6 at the annotations' own lightness, one flat calm chroma",
      L = seq6(0.93, 0.68), C = 0.12,             H = 150),
  lad("ann-floor-ease", "the same descent with the chroma easing off -- the widest clearance of all",
      L = seq6(0.92, 0.68), C = seq6(0.14, 0.09), H = 150)

# ---- between the two: airy at the top, most of the way down --------------------------------------
# airy-chroma's shape (lightness nearly flat, chroma carrying the rank) over a span that actually
# descends: L 0.91 to 0.78, which stops above the annotation band rather than landing in it. What is
# varied is only how fast the chroma goes -- from flat to a steep fall -- because that is what
# decides whether the ladder eases the page or keeps colouring it all the way down.
)), round_of("fifth -- between airy and the annotation level", list(
  lad("mid-flat",      "the span, with the chroma not moving at all",
      L = seq6(0.91, 0.78), C = 0.13,             H = 150),
  lad("mid-gentle",    "a small chroma easing over the descent",
      L = seq6(0.91, 0.78), C = seq6(0.14, 0.10), H = 150),
  lad("mid-chroma",    "airy-chroma's own fall, over the wider span",
      L = seq6(0.91, 0.78), C = seq6(0.16, 0.08), H = 150),
  lad("mid-steep",     "chroma halving and more: vivid at the top, nearly neutral at the foot",
      L = seq6(0.91, 0.78), C = seq6(0.18, 0.07), H = 150),
  lad("mid-late",      "chroma held for the first three, then let go",
      L = seq6(0.91, 0.78), C = c(0.15, 0.15, 0.15, 0.12, 0.10, 0.08), H = 150),
  lad("mid-high",      "the same fall started a rung brighter, and stopped a rung higher",
      L = seq6(0.92, 0.80), C = seq6(0.15, 0.09), H = 150)

# ---- a chroma FLOOR ------------------------------------------------------------------------------
# mid-steep's fall, stopped: the chroma drops for the first rungs and then holds. That splits the six
# into the levels that lead and the levels that only mark, which is how a document is actually
# written -- h1 and h2 carry the structure, h4 to h6 are near-equivalents.
# ⚠ AT L 0.91 THE HUE-150 CEILING IS C 0.161, so `floor-18` and `floor-16` have the SAME h1 (#8CFFA7):
#   both requests are clipped to it and the two ladders only diverge at h2. `floor-18-real` is the
#   same ladder with its top at L 0.90, where 0.18 fits (ceiling 0.182) and the first rung is the
#   colour that was actually asked for.
)), round_of("sixth -- the chroma floors", list(
  lad("floor-18",      "0.18 down to a 0.12 floor, reached at h4",
      L = seq6(0.91, 0.78), C = c(0.18, 0.16, 0.14, 0.12, 0.12, 0.12), H = 150),
  lad("floor-16",      "the same shape one notch calmer, floor 0.10",
      L = seq6(0.91, 0.78), C = c(0.16, 0.14, 0.12, 0.10, 0.10, 0.10), H = 150),
  lad("floor-18-real", "floor-18 with a top rung bright enough to hold the 0.18 it asks for",
      L = seq6(0.90, 0.78), C = c(0.18, 0.16, 0.14, 0.12, 0.12, 0.12), H = 150),
  lad("floor-early",   "the floor reached at h3: only the top two lead, the rest is one family",
      L = seq6(0.91, 0.78), C = c(0.18, 0.15, 0.12, 0.12, 0.12, 0.12), H = 150),
  lad("floor-plateau", "chroma AND lightness both settle -- big moves at the top, a tight foot",
      L = c(0.91, 0.87, 0.84, 0.815, 0.797, 0.78),
      C = c(0.17, 0.15, 0.13, 0.11, 0.11, 0.11), H = 150),
  lad("floor-break",   "a bigger drop INTO the floor: a visible seam between the leading three",
      L = seq6(0.91, 0.78), C = c(0.17, 0.15, 0.13, 0.09, 0.09, 0.09), H = 150),
  lad("floor-paired",  "the levels in pairs -- h1-h2, h3-h4, h5-h6 -- as a nested document reads",
      L = seq6(0.91, 0.78), C = c(0.18, 0.18, 0.13, 0.13, 0.10, 0.10), H = 150),
  lad("floor-deep",    "the floor idea carried down to where the annotations live",
      L = seq6(0.91, 0.72), C = c(0.16, 0.14, 0.12, 0.10, 0.10, 0.10), H = 150)

# ---- softer at the top ---------------------------------------------------------------------------
# floor-18-real read as too vibrant ON THE REAL PAGE, which is the only place that can be judged:
# a specimen page has one ladder and nothing else, a site has code blocks, tables and annotations
# competing with it. So the top comes down -- 0.16, 0.14, 0.12 instead of 0.18 -- while the floor is
# held at 0.10 or above, below which a green stops being a green and becomes a pale grey.
# Both hues are here, because 145 was set aside early and never tried at these chromas.
# ⚠ 145 is .pertinent's own hue (#05ae30, C 0.204). What keeps these clear of it is the same thing
#   as before: chroma. At 0.10-0.16 against its 0.204 they are nowhere near it.
)), round_of("seventh -- softer at the top, floor at 0.10", list(
  lad("soft-16-150",   "the shape of floor-18-real with its top brought down to 0.16",
      L = seq6(0.90, 0.78), C = c(0.16, 0.14, 0.13, 0.12, 0.12, 0.12), H = 150),
  lad("soft-14-150",   "one notch calmer again, floor 0.11",
      L = seq6(0.90, 0.78), C = c(0.14, 0.13, 0.12, 0.11, 0.11, 0.11), H = 150),
  lad("soft-12-150",   "as calm as the floor allows: 0.12 at the top, 0.10 at the foot",
      L = seq6(0.90, 0.78), C = c(0.12, 0.12, 0.11, 0.10, 0.10, 0.10), H = 150),
  lad("soft-16-145",   "the same three at hue 145, which was set aside before it was tried calm",
      L = seq6(0.90, 0.78), C = c(0.16, 0.14, 0.13, 0.12, 0.12, 0.12), H = 145),
  lad("soft-14-145",   "hue 145, top 0.14",
      L = seq6(0.90, 0.78), C = c(0.14, 0.13, 0.12, 0.11, 0.11, 0.11), H = 145),
  lad("soft-12-145",   "hue 145, the calmest of the set",
      L = seq6(0.90, 0.78), C = c(0.12, 0.12, 0.11, 0.10, 0.10, 0.10), H = 145),
  lad("soft-lead-145", "one clear drop after h1, then nothing much moves but lightness",
      L = seq6(0.90, 0.78), C = c(0.15, 0.12, 0.12, 0.11, 0.10, 0.10), H = 145),
  lad("soft-low-150",  "the whole ladder a rung darker as well as calmer",
      L = seq6(0.88, 0.76), C = c(0.14, 0.13, 0.12, 0.11, 0.11, 0.11), H = 150)

# ---- the 0.12 / 0.10 / 0.08 scale, lifted -------------------------------------------------------
# ⚠ L 0.94 CANNOT HOLD C 0.12. The sRGB ceiling there is 0.102 at hue 150 and 0.108 at hue 145, so
#   the top rung of the two `-94` ladders is clipped to that and lands within a whisker of h2's
#   0.10 -- the first two levels become nearly one colour. The `-fit` pair is the same ladder with
#   its top at L 0.93, where 0.12 is real (ceiling 0.121 / 0.129). Both are here because the clipped
#   pair is what was asked for and may well be what looks right.
)), round_of("eighth -- the 0.12/0.10/0.08 scale, lifted", list(
  lad("scale-94-150",  "as asked: L 0.94 to 0.82, hue 150 -- the top rung clipped to C 0.102",
      L = seq6(0.94, 0.82), C = c(0.12, 0.10, 0.08, 0.08, 0.08, 0.08), H = 150),
  lad("scale-93-150",  "the same, topped at L 0.93 where the 0.12 is actually shown",
      L = seq6(0.93, 0.82), C = c(0.12, 0.10, 0.08, 0.08, 0.08, 0.08), H = 150),
  lad("scale-94-145",  "as asked, at hue 145 -- clipped to C 0.108",
      L = seq6(0.94, 0.82), C = c(0.12, 0.10, 0.08, 0.08, 0.08, 0.08), H = 145),
  lad("scale-93-145",  "hue 145, topped where the 0.12 fits",
      L = seq6(0.93, 0.82), C = c(0.12, 0.10, 0.08, 0.08, 0.08, 0.08), H = 145)

# ---- warm: the body text's own hue ---------------------------------------------------------------
# The warm family on the page is already one hue, measured rather than assumed:
#     body text     #CDCBBC   L 0.846  C 0.014  h 102.2
#     its lighter   #f0efe5   L 0.950  C 0.013  h 102.1   (tabxplor's dark-theme ink)
#     the note      #F8DC05   L 0.890  C 0.184  h  99.9   -- and capped at C 0.08 it is #E7DD9F
# So a heading at hue ~100 is LITERALLY THE TEXT COLOUR, brighter and with some chroma put back:
# that is the thematic consistency, and it is why the note capped at 0.08 reads as a few lines of
# light mode inside a dark one -- it is the same hue as everything around it.
# ⚠ Below hue ~95 the gamut closes at these lightnesses (at hue 90, L 0.94 holds only C 0.076), so
#   the gold-leaning variant starts a rung lower instead of being clipped.
)), round_of("ninth -- warm, the body text's own hue", list(
  lad("warm-100",      "the 0.12/0.10/0.08 scale in warm: hue 100, beside the note's own",
      L = seq6(0.94, 0.82), C = c(0.12, 0.10, 0.08, 0.08, 0.08, 0.08), H = 100),
  lad("warm-text",     "hue 102 -- the body text's exact hue, lifted and given chroma back",
      L = seq6(0.94, 0.82), C = c(0.12, 0.10, 0.08, 0.08, 0.08, 0.08), H = 102),
  lad("warm-note",     "flat at C 0.08: the chroma the note is capped to, all the way down",
      L = seq6(0.94, 0.82), C = 0.08,             H = 100),
  lad("warm-calm",     "calmer still -- a tint rather than a colour",
      L = seq6(0.94, 0.82), C = c(0.08, 0.07, 0.06, 0.06, 0.06, 0.06), H = 100),
  lad("warm-flat-06",  "barely tinted: the text family at a brighter lightness and nothing more",
      L = seq6(0.94, 0.82), C = 0.06,             H = 102),
  lad("warm-strong",   "the loud end of the warm family, to bracket the others",
      L = seq6(0.94, 0.82), C = c(0.16, 0.13, 0.10, 0.10, 0.10, 0.10), H = 102),
  lad("warm-110",      "hue 110, a step toward olive and away from the gold",
      L = seq6(0.94, 0.82), C = c(0.12, 0.10, 0.08, 0.08, 0.08, 0.08), H = 110),
  lad("warm-gold",     "leaning to the bold gold's hue, started a rung lower so it is not clipped",
      L = seq6(0.92, 0.80), C = c(0.10, 0.09, 0.08, 0.08, 0.08, 0.08), H = 92)

# ---- warm, floored on the body text --------------------------------------------------------------
# warm-calm's chroma shape, with the descent tied to the ink: h6 lands at L 0.84, which IS the body
# text (#CDCBBC, oklch 0.840 0.020 101), and nothing goes below it. So the last heading level is the
# prose's own lightness with some colour put back, and every level above it is brighter -- the rule
# that started this whole search, now stated as a floor instead of hoped for.
# The variants trade the top rung's lightness for chroma, one for one: at hue 100 the ceiling is
# 0.086 at L 0.96, 0.107 at 0.95, 0.128 at 0.94, 0.148 at 0.93 -- so a rung of lightness buys about
# 0.02 of chroma, and each start below asks for what its own lightness can actually hold.
)), round_of("tenth -- warm, floored on the body text", list(
  lad("warm-96-08",    "from oklch(0.96 0.08 100), warm-calm's chroma shape",
      L = seq6(0.96, 0.84), C = c(0.08, 0.07, 0.06, 0.06, 0.06, 0.06), H = 100),
  lad("warm-95-10",    "one rung of lightness traded for two hundredths of chroma",
      L = seq6(0.95, 0.84), C = c(0.10, 0.09, 0.08, 0.08, 0.08, 0.08), H = 100),
  lad("warm-94-12",    "and again -- the top now as chromatic as the old warm-100",
      L = seq6(0.94, 0.84), C = c(0.12, 0.11, 0.10, 0.10, 0.10, 0.10), H = 100),
  lad("warm-93-14",    "the loud end of this family, still floored on the ink",
      L = seq6(0.93, 0.84), C = c(0.14, 0.13, 0.12, 0.12, 0.12, 0.12), H = 100),
  lad("warm-95-10-drop", "starting at 0.10 but falling to warm-calm's own floor",
      L = seq6(0.95, 0.84), C = c(0.10, 0.08, 0.06, 0.06, 0.06, 0.06), H = 100),
  lad("warm-94-12-drop", "starting at 0.12 and falling further still: the widest fall of the round",
      L = seq6(0.94, 0.84), C = c(0.12, 0.10, 0.08, 0.08, 0.08, 0.08), H = 100)
)))

for (i in seq_along(LADDERS)) {
  L <- LADDERS[[i]]
  LADDERS[[i]]$hex <- vapply(1:6, function(k) oklch_hex(L$L[k], L$C[k], L$H[k]), character(1))
}

