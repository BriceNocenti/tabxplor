# PURPOSE: preview the bg_legend palette in the medium it actually exists for.
# ROLE: design-time only, .Rbuildignore'd. Writes dev/review_manual/phase14l_legend.html.
# KEY CONSTRAINTS:
#   - preview_color_grid() (dev/color_palette_tools.R) shows text ON FILL -- the OPPOSITE case. A
#     bg_legend colour is drawn as BOLD TEXT ON WHITE (an Excel rich-text run / a ggpubr label carry a
#     font colour but no fill), and that had never been previewed: 14c designed it by the -0.2 L rule
#     plus the text-on-fill grid, and it shipped faint.
#   - APCA Lc is the acceptance bar recorded in R/tab_classes.R's design notes: >= 60 for
#     larger/heavier text (a legend break-word is bold), >= 45 large headline, < 30 decorative-only.
# See: CLAUDE.md > Phase 14l, and darken_for_legend() for why the fix needed BOTH levers.
#
# Usage:  Rscript dev/make_legend_preview.R

suppressWarnings(suppressMessages(source("dev/color_palette_tools.R")))
suppressMessages(pkgload::load_all(".", quiet = TRUE))

fills     <- c(tabxplor:::default_background_colors, tabxplor:::default_background_colors_neg)
shipped   <- c(tabxplor:::default_bg_legend_colors,  tabxplor:::default_bg_legend_colors_neg)
old_14c   <- c("#9fbbbe", "#98aebd", "#8fa3bd", "#7e8dbd",
               "#bdb3a1", "#bda694", "#bd988a", "#bc7c72")
slot_lab  <- c(paste0("over ", 1:4), paste0("under ", 1:4))
words     <- c("+5", "+10", "+20", "+30", "-5", "-10", "-20", "-30")

lc  <- function(hex) vapply(hex, function(h) .cg_apca(h, "#ffffff"), numeric(1))
oklch <- function(hex) farver::decode_colour(hex, to = "oklch")

# one row per slot: the FILL it describes, then each candidate drawn as the legend draws it
row_html <- function(i, cands) {
  cells <- vapply(names(cands), function(nm) {
    h <- cands[[nm]][i]
    sprintf('<td class="sw"><span style="color:%s;font-weight:bold">%s</span>
             <span class="lc">Lc&nbsp;%.0f</span></td>', h, words[i], lc(h))
  }, character(1))
  sprintf('<tr><td class="lab">%s</td><td class="fill" style="background:%s">%s</td>%s</tr>',
          slot_lab[i], fills[i], fills[i], paste(cells, collapse = ""))
}

cands <- list("14c: -0.2 L, 1x chroma (faint)" = old_14c,
              "14l: -0.30 L, 2x chroma (SHIPPED)" = shipped)

hdr <- paste0("<th>slot</th><th>the fill it describes</th>",
              paste0("<th>", names(cands), "</th>", collapse = ""))

o <- oklch(shipped); oo <- oklch(old_14c)
note <- sprintf(
  "<tr><td class='lab'>%s</td><td></td><td class='num'>L %.2f&ndash;%.2f &middot; C %.3f&ndash;%.3f &middot; Lc %.0f&ndash;%.0f</td><td class='num'>L %.2f&ndash;%.2f &middot; C %.3f&ndash;%.3f &middot; Lc %.0f&ndash;%.0f</td></tr>",
  "range", min(oo[,1]), max(oo[,1]), min(oo[,2]), max(oo[,2]), min(lc(old_14c)), max(lc(old_14c)),
  min(o[,1]), max(o[,1]), min(o[,2]), max(o[,2]), min(lc(shipped)), max(lc(shipped)))

html <- paste0(
  '<!doctype html><meta charset="utf-8"><title>Phase 14l - bg_legend</title><style>
   body{font-family:"DejaVu Sans",sans-serif;margin:2rem;color:#111;background:#fff}
   h1{font-size:1.2rem} p{max-width:60em;line-height:1.45;color:#333}
   table{border-collapse:collapse;margin-top:1.2rem}
   th,td{padding:6px 12px;border-bottom:1px solid #e5e5e5;text-align:left;vertical-align:middle}
   th{font-size:.8rem;text-transform:uppercase;letter-spacing:.04em;color:#666}
   .lab{font-size:.8rem;color:#666}
   .fill{font-family:monospace;font-size:.75rem;color:#555;border-radius:3px}
   .sw{font-size:1.05rem}
   .lc{font-size:.7rem;color:#999;font-weight:normal;margin-left:.5rem}
   .num{font-family:monospace;font-size:.72rem;color:#555}
   </style>
   <h1>Phase 14l &mdash; the Excel colour legend&rsquo;s background break-words</h1>
   <p>Every cell below is drawn the way an Excel rich-text run draws it: <b>bold text on white</b>,
   with no fill. That is the medium this palette exists for, and it had never been previewed.</p>
   <p><b>Why both levers.</b> APCA <span class="num">Lc</span> is driven by lightness almost alone, so
   the chroma boost you asked for fixes the <i>greyness</i> but moves Lc by ~1 point. The 14c bake sat
   at Lc 39.6&ndash;60.8 &mdash; 3 of 4 slots below the &ge;&nbsp;60 bar these palettes were designed
   against. Deepening the darkening to &minus;0.30 is what fixes the faintness; 2&times; chroma is what
   stops them looking grey. 2&times; is also the largest boost that stays in gamut on all 8 slots, so
   the chroma proportions inherited from the fills survive exactly.</p>
   <table><tr>', hdr, '</tr>',
  paste(vapply(1:8, row_html, character(1), cands = cands), collapse = ""),
  note, '</table>')

dir.create("dev/review_manual", showWarnings = FALSE, recursive = TRUE)
writeLines(html, "dev/review_manual/phase14l_legend.html")
cat("wrote dev/review_manual/phase14l_legend.html\n")
cat(sprintf("14c  Lc: %s\n", paste(sprintf("%.1f", lc(old_14c)), collapse = " ")))
cat(sprintf("14l  Lc: %s\n", paste(sprintf("%.1f", lc(shipped)), collapse = " ")))
