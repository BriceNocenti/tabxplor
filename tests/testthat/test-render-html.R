# PURPOSE: Lock the Phase 10e tab_kable() render seam -- both engines.
# ROLE: Guards (a) the kableExtra engine against drift from the legacy carve (structure snapshot),
#       (b) the home-built html engine (structure snapshot + self-contained + DOM-size win),
#       (c) cross-engine content parity (same cell text + tooltip content -> the two engines can't
#       diverge silently). The kableExtra byte-identity vs the pre-10e code was proven by a git-stash
#       A/B; these snapshots keep it locked going forward.
# KEY CONSTRAINTS:
#   - Must run via test_check("tabxplor"); skip_if_not_installed("kableExtra") on the kable branch.
#   - Snapshots strip the <style> blocks (kableExtra's tab.css / the html engine's block are large,
#     stable, and not the point) so the diff is the table STRUCTURE.

gss <- forcats::gss_cat

# --- helpers ---------------------------------------------------------------------------
rh_strip_style <- function(h) {
  h <- gsub("(?s)<style[^>]*>.*?</style>", "<!--css-->", as.character(h), perl = TRUE)
  gsub("(?s)<script[^>]*>.*?</script>", "", h, perl = TRUE)
}
rh_tbody <- function(h) {
  m <- regmatches(as.character(h), regexpr("(?s)<tbody>.*?</tbody>", as.character(h), perl = TRUE))
  if (length(m) == 0) "" else m
}
rh_cells <- function(h) {                       # tbody cell text tokens (data, not styling)
  t <- unlist(strsplit(gsub("<[^>]*>", "\x01", rh_tbody(h)), "\x01"))
  t <- trimws(t); t[nzchar(t)]
}
rh_titles <- function(h) {                       # non-empty tooltip contents
  ti <- unlist(regmatches(as.character(h), gregexpr('title="[^"]+"', as.character(h))))
  sort(unique(ti[ti != 'title=""']))
}

# === SECTION: kableExtra engine (legacy) -- version-robust structure assertions ==========
# Phase 14e made "html" the DEFAULT engine, so every call here pins engine = "kableExtra"
# explicitly -- otherwise this whole section would silently assert against the other engine.
# WARNING: do NOT snapshot this engine's bytes. Its cells come from kableExtra::cell_spec(), whose
# output is version-unstable -- 1.4.0 -> 1.4.1 moved the rgba alpha (255 -> 1), dropped leading
# padding and (text_spec) leaked a stray `class="TRUE"`. Byte-snapshotting it made CI red on all 5
# platforms for a change tabxplor did not make, and put kableExtra's release schedule in charge of
# our test suite. We do not own that HTML, so we assert the parts we DO own -- the cells, the
# colouring, the tooltips, the geometry -- which survived 1.4.1 untouched. The home-built engine's
# HTML *is* ours, so it keeps its byte snapshot below.

testthat::test_that("tab_kable kableExtra engine structure is stable", {
  testthat::skip_if_not_installed("kableExtra")
  counts   <- tab(gss, marital, race)
  row_diff <- tab(gss, marital, race, pct = "row", color = "diff")
  chi2     <- suppressWarnings(tab(gss, marital, race, pct = "row", test = TRUE))

  # geometry: exactly one <tbody>, one <tr> per row. chi2 gains ONE extra row -- the p-value row is
  # materialised at display (Phase 10i-B), so it is not in nrow(tb).
  for (tb in list(counts, row_diff)) {
    h <- rh_strip_style(tab_kable(tb, engine = "kableExtra"))
    testthat::expect_match(h, "<table")
    body <- rh_tbody(h)
    testthat::expect_length(body, 1L)
    testthat::expect_equal(lengths(regmatches(body, gregexpr("<tr", body)))[[1]], nrow(tb))
  }
  bc <- rh_tbody(rh_strip_style(suppressWarnings(tab_kable(chi2, engine = "kableExtra"))))
  testthat::expect_equal(lengths(regmatches(bc, gregexpr("<tr", bc)))[[1]], nrow(chi2) + 1L)

  # colouring reaches the cells, and only when asked for. kableExtra bakes colour INLINE (it carries
  # no stylesheet of ours), which is also why its theme shows in the markup -- both are the opposite
  # of the html engine below, and that contrast is the point of having two sections.
  hd <- rh_strip_style(tab_kable(row_diff, engine = "kableExtra"))
  hm <- rh_strip_style(tab_kable(row_diff, engine = "kableExtra", color = FALSE))
  testthat::expect_match(hd, "color:")
  testthat::expect_false(identical(hd, hm))
  # theme is honoured
  testthat::expect_false(identical(
    rh_strip_style(tab_kable(row_diff, engine = "kableExtra", theme = "dark")), hd))
  # tooltips carry the underlying fields
  testthat::expect_true(any(grepl("^title=\"diff:", rh_titles(hd))))
})

# === SECTION: home-built html engine -- structure snapshot + self-contained ==============

testthat::test_that("tab_kable html engine structure is stable", {
  counts   <- tab(gss, marital, race)
  row_diff <- tab(gss, marital, race, pct = "row", color = "diff")
  bg       <- tab(gss, marital, race, pct = "row", color = c("diff", "ratio"))
  chi2     <- suppressWarnings(tab(gss, marital, race, pct = "row", test = TRUE))

  testthat::expect_snapshot(cat(rh_strip_style(tab_kable(counts,   engine = "html"))))
  testthat::expect_snapshot(cat(rh_strip_style(tab_kable(row_diff, engine = "html"))))
  testthat::expect_snapshot(cat(rh_strip_style(tab_kable(bg,       engine = "html"))))
  testthat::expect_snapshot(cat(rh_strip_style(suppressWarnings(tab_kable(chi2, engine = "html")))))
})

testthat::test_that("html engine output is self-contained (inline <style>, no external <link>)", {
  h <- as.character(tab_kable(tab(gss, marital, race, pct = "row", color = "diff"), engine = "html"))
  testthat::expect_match(h, "<table")
  testthat::expect_match(h, "<style")
  testthat::expect_false(grepl("<link", h))
  testthat::expect_false(grepl("includeCSS|lightable|cosmo", h))
})

# === SECTION: Phase 13d -- theme lives in the CSS, not the markup =========================

testthat::test_that("cells carry slot classes, never inline colour", {
  h <- as.character(tab_kable(tab(gss, marital, race, pct = "row", color = "diff"),
                              engine = "html", tooltips = FALSE))
  b <- rh_tbody(h)
  # THE constraint of Phase 13d: an inline `style` beats every stylesheet rule short of !important, so
  # inline colour would make theme = "auto" impossible. If this fails, dark mode is silently broken.
  testthat::expect_false(grepl("color:#", b))
  # Phase 14e: the class attribute now also carries ROLE classes (align / borders / widths), so the
  # slot class is no longer first -- match it anywhere in the attribute.
  testthat::expect_match(b, 'class="[^"]*\\b(p|m)[1-4]\\b')   # a text-coloured cell
  testthat::expect_match(b, 'class="[^"]*\\bg[12]\\b')        # an uncoloured cell
  testthat::expect_match(h, '<table class="tabxplor-tab">', fixed = TRUE)   # no theme token
})

testthat::test_that("the MARKUP is theme-agnostic; only the stylesheet differs", {
  tb <- tab(gss, marital, race, pct = "row", color = "diff")
  mk <- function(th) rh_strip_style(tab_kable(tb, engine = "html", theme = th))
  cs <- function(th) as.character(tab_kable(tb, engine = "html", theme = th))
  # This is the property that makes "auto" possible at all -- one DOM, three stylesheets.
  testthat::expect_identical(mk("light"), mk("dark"))
  testthat::expect_identical(mk("light"), mk("auto"))
  testthat::expect_false(identical(cs("light"), cs("dark")))
})

testthat::test_that("theme drives the emitted CSS (light / dark / auto)", {
  tb <- tab(gss, marital, race, pct = "row", color = "diff")
  cs <- function(th) as.character(tab_kable(tb, engine = "html", theme = th))

  light <- cs("light")
  testthat::expect_false(grepl("@media", light, fixed = TRUE))
  testthat::expect_false(grepl("quarto", light, fixed = TRUE))
  testthat::expect_true(grepl(".tabxplor-tab{color:#000000;background:#ffffff;}", light, fixed = TRUE))

  dark <- cs("dark")
  testthat::expect_false(grepl("@media", dark, fixed = TRUE))
  # Phase 14e: dark is #CECDC3 on #222222 -- pure white on near-black is a glare-y contrast for body
  # text. Read the values from tx_chrome_hex() rather than re-hardcoding them here.
  dk <- tabxplor:::tx_chrome_hex("dark")
  testthat::expect_true(grepl(paste0(".tabxplor-tab{color:", dk$text, ";background:", dk$bg, ";}"),
                              dark, fixed = TRUE))
  testthat::expect_true(grepl(paste0("border-color:", dk$border), dark, fixed = TRUE))

  auto <- cs("auto")
  testthat::expect_true(grepl("@media (prefers-color-scheme: dark)", auto, fixed = TRUE))
  testthat::expect_true(grepl("body.quarto-dark",     auto, fixed = TRUE))
  testthat::expect_true(grepl("body.quarto-light",    auto, fixed = TRUE))
  testthat::expect_true(grepl("[data-bs-theme=dark]", auto, fixed = TRUE))
})

testthat::test_that("the generated CSS is syntactically valid in every mode", {
  # A single malformed rule makes the browser drop it -- and, inside @media, potentially the whole
  # block -- with no error anywhere. No selector-presence test catches that, so check the shape.
  for (chrome in c(TRUE, FALSE)) for (th in c("light", "dark", "auto")) {
    css <- tab_css(theme = th, chrome = chrome, style_tag = FALSE)
    lab <- paste0(th, if (chrome) "/chrome" else "/md")
    testthat::expect_identical(lengths(regmatches(css, gregexpr("[{]", css))),
                               lengths(regmatches(css, gregexpr("[}]", css))), label = lab)
    body <- trimws(gsub("@media \\(prefers-color-scheme: dark\\) \\{|^\\}$", "",
                        strsplit(css, "\n")[[1]]))
    body <- body[nzchar(body)]
    testthat::expect_true(all(grepl("^[^{}]+\\{([-a-z]+:[^;{}]+;)+\\}$", body)), label = lab)
  }
})

testthat::test_that("the auto cascade layers are ordered so a page toggle beats the OS", {
  # THE contract of theme = "auto". `@media (prefers-color-scheme)` only reports the OS; Quarto and
  # friends toggle a CLASS, which a media query cannot see. The hook layers must therefore come AFTER
  # the media block (they also out-specify it), and light before dark. Reordering these silently makes
  # an explicit page toggle lose to the reader's OS -- which no assertion on hex would catch.
  auto <- as.character(tab_kable(tab(gss, marital, race, pct = "row", color = "diff"),
                                 engine = "html", theme = "auto"))
  testthat::expect_lt(regexpr("@media", auto, fixed = TRUE),
                      regexpr("body.quarto-light", auto, fixed = TRUE))
  testthat::expect_lt(regexpr("body.quarto-light", auto, fixed = TRUE),
                      regexpr("body.quarto-dark", auto, fixed = TRUE))
})

testthat::test_that('theme = "auto" downgrades on the kableExtra engine, with one message', {
  testthat::skip_if_not_installed("kableExtra")
  tb <- tab(gss, marital, race, pct = "row", color = "diff")
  testthat::expect_message(tab_kable(tb, engine = "kableExtra", theme = "auto"), "auto")
  testthat::expect_identical(
    as.character(suppressMessages(tab_kable(tb, engine = "kableExtra", theme = "auto"))),
    as.character(tab_kable(tb, engine = "kableExtra", theme = "light")))
})

testthat::test_that("the theme is read from options(tabxplor.theme), explicit wins", {
  tb <- tab(gss, marital, race, pct = "row", color = "diff")
  withr::local_options(list(tabxplor.theme = "dark"))
  testthat::expect_identical(as.character(tab_kable(tb, engine = "html")),
                             as.character(tab_kable(tb, engine = "html", theme = "dark")))
  testthat::expect_identical(as.character(tab_kable(tb, engine = "html", theme = "light")),
                             as.character(withr::with_options(list(tabxplor.theme = "light"),
                                                              tab_kable(tb, engine = "html"))))
})

testthat::test_that("css = FALSE drops the <style> but keeps the classes", {
  tb <- tab(gss, marital, race, pct = "row", color = "diff")
  h  <- as.character(tab_kable(tb, engine = "html", css = FALSE))
  testthat::expect_false(grepl("<style", h, fixed = TRUE))
  testthat::expect_match(rh_tbody(h), 'class="[^"]*\\b(p|m)[1-4]\\b')
  # the once-per-document workflow: options() drives it, and tab_css() supplies the stylesheet
  withr::local_options(list(tabxplor.kable_css = FALSE))
  testthat::expect_false(grepl("<style", as.character(tab_kable(tb, engine = "html")), fixed = TRUE))
  testthat::expect_match(tab_css(theme = "auto"), "^<style>")
})

testthat::test_that("the colour legend uses classes on the html engine, hex on kableExtra", {
  tb   <- tab(gss, marital, race, pct = "row", color = "diff")
  foot <- function(h) regmatches(as.character(h),
                                 regexpr("(?s)<tfoot>.*?</tfoot>", as.character(h), perl = TRUE))
  # The legend sits in the table's own <tfoot>, so inline hex would freeze it while the cells it
  # describes follow a toggle. The discriminator is the ENGINE (does our stylesheet ship?), NOT the
  # theme: engine = "html" + theme = "light" + css = FALSE is a real case (the document supplies
  # tab_css("auto") itself), and inline hex would be wrong there too.
  for (th in c("light", "dark", "auto")) {
    f <- foot(tab_kable(tb, engine = "html", theme = th, color_legend = TRUE))
    testthat::expect_false(grepl("color:#", f, fixed = TRUE))   # no inline hex ANYWHERE in the legend
    testthat::expect_match(f, '<span class="(p|m)[1-4]"')
  }
  testthat::skip_if_not_installed("kableExtra")
  # kableExtra carries no tabxplor stylesheet -> classes would render the legend uncoloured.
  testthat::expect_match(foot(tab_kable(tb, engine = "kableExtra", color_legend = TRUE)),
                         'style="[^"]*color:#')
})

testthat::test_that("legend break-words are bold in every medium (Phase 14c)", {
  tb <- tab(gss, marital, race, pct = "row", color = c("diff", "ratio"))
  # html: inline on BOTH channels, so it holds on the kableExtra path (no stylesheet of ours) and on
  # the background classes (which are deliberately not bolded by the stylesheet, as the cells aren't).
  for (cl in c(TRUE, FALSE)) {
    spans <- unlist(regmatches(
      l <- tab_color_legend(tb, medium = "html", classes = cl),
      gregexpr("<span [^>]*>", l)))
    testthat::expect_true(length(spans) > 0)
    testthat::expect_true(all(grepl("font-weight:bold;", spans, fixed = TRUE)))
  }
  # md: `**` so the RAW markdown shows them too (the stylesheet bold only reaches a render).
  testthat::expect_match(tab_color_legend(tb, medium = "md"), "[*][*]\\[[+]5\\]\\{[.]p1\\}[*][*]")
  testthat::expect_match(tab_color_legend(tb, medium = "md"), "[*][*]\\[.2\\]\\{[.]o3\\}[*][*]")
  # runs (excel / plot)
  runs <- tab_color_legend(tb, medium = "runs")[[1]]
  coloured <- purrr::keep(runs, ~ !is.na(.$color))
  testthat::expect_true(length(coloured) > 0)
  testthat::expect_true(all(purrr::map_lgl(coloured, "bold")))
  # console: crayon bold wraps the colour style. `cli.num_colors` is the real gate (crayon defers to
  # cli::num_ansi_colors(), which reads it FIRST) -- testthat pins it to 1 for reproducible output.
  withr::with_options(list(cli.num_colors = 256, crayon.enabled = TRUE, crayon.colors = 256), {
    testthat::expect_match(tab_color_legend(tb, medium = "console"), "\033[[]1m")
  })
})

testthat::test_that("tab_css() bolds the text slot classes, not the background ones (Phase 14c)", {
  for (chrome in c(TRUE, FALSE)) {              # tab_md_css() is tab_css(chrome = FALSE)
    css <- tab_css(style_tag = FALSE, chrome = chrome)
    testthat::expect_match(css, ".p1,.p2,.p3,.p4,.m1,.m2,.m3,.m4{font-weight:bold;}", fixed = TRUE)
    testthat::expect_false(grepl("[.]o1[^{]*[{][^}]*font-weight", css))
  }
  # theme-independent -> emitted ONCE, not per cascade layer
  testthat::expect_equal(lengths(regmatches(a <- tab_css(theme = "auto", style_tag = FALSE),
                                            gregexpr(".p1,.p2,.p3,.p4,.m1,.m2,.m3,.m4{font-weight:bold;}",
                                                     a, fixed = TRUE))), 1L)
})

# === SECTION: cross-engine content parity ================================================

testthat::test_that("both engines carry the same cell text and tooltip content", {
  testthat::skip_if_not_installed("kableExtra")
  for (tb in list(
    tab(gss, marital, race, pct = "row", color = "diff"),
    tab(gss, marital, race),
    tab(gss, marital, c(race, relig), pct = "row", color = "diff"),
    tab_num(gss, race, c(age, tvhours), marital, comp = "all")
  )) {
    ke <- suppressWarnings(tab_kable(tb, engine = "kableExtra"))
    ht <- suppressWarnings(tab_kable(tb, engine = "html"))
    testthat::expect_identical(rh_cells(ke), rh_cells(ht))   # same data
    testthat::expect_identical(rh_titles(ke), rh_titles(ht)) # same tooltip content
  }
})

testthat::test_that("html engine drops the per-cell <span> wrapper (DOM-size win)", {
  testthat::skip_if_not_installed("kableExtra")
  tb <- tab(gss, marital, race, pct = "row", color = "diff")
  ke <- rh_tbody(tab_kable(tb, engine = "kableExtra"))
  ht <- rh_tbody(tab_kable(tb, engine = "html"))
  n_ke <- lengths(gregexpr("<span", ke))
  n_ht <- if (grepl("<span", ht)) lengths(gregexpr("<span", ht)) else 0L
  testthat::expect_gt(n_ke, 0L)          # kableExtra wraps every cell
  testthat::expect_lt(n_ht, n_ke)        # html has far fewer (ideally 0) tbody spans
})

# === SECTION: get_data + graceful degrade both engines ===================================

testthat::test_that("get_data returns a data.frame on both engines", {
  testthat::skip_if_not_installed("kableExtra")
  tb <- tab(gss, marital, race, pct = "row", color = "diff")
  testthat::expect_s3_class(tab_kable(tb, get_data = TRUE), "data.frame")
  testthat::expect_s3_class(tab_kable(tb, get_data = TRUE, engine = "html"), "data.frame")
})

testthat::test_that("plain data.frame degrades gracefully on both engines", {
  df <- data.frame(a = 1:3, b = letters[1:3])
  testthat::expect_no_error(suppressMessages(tab_kable(df, engine = "html")))
  h <- as.character(suppressMessages(tab_kable(df, engine = "html")))
  testthat::expect_match(h, "<table")
  testthat::skip_if_not_installed("kableExtra")
  testthat::expect_no_error(suppressMessages(tab_kable(df)))
})

testthat::test_that("tab_kable renders a non-mergeable list instead of erroring (list method)", {
  testthat::skip_if_not_installed("kableExtra")
  # different col_vars -> not mergeable (previously an error for kable)
  lst <- list(tab(gss, marital, race,  pct = "row"),
              tab(gss, marital, relig, pct = "row"))
  for (eng in c("kableExtra", "html")) {
    h <- as.character(suppressWarnings(tab_kable(lst, engine = eng)))
    testthat::expect_gte(lengths(gregexpr("<table", h)), 2L)  # both tables rendered
  }
})

testthat::test_that("n_min blanked cells render empty (no literal NA) on both engines", {
  testthat::skip_if_not_installed("kableExtra")
  tb <- suppressWarnings(tab(gss, marital, race, pct = "row", n_min = 1000))
  for (eng in c("kableExtra", "html")) {
    h <- as.character(suppressWarnings(tab_kable(tb, engine = eng)))
    testthat::expect_false(grepl(">NA<", h))       # no literal NA cell content
    testthat::expect_match(h, "<table")
  }
})

testthat::test_that("engine is resolved from the option", {
  testthat::skip_if_not_installed("kableExtra")
  tb <- tab(gss, marital, race, pct = "row", color = "diff")
  withr::local_options(tabxplor.tab_kable_engine = "html")
  h <- as.character(tab_kable(tb))
  testthat::expect_match(h, "tabxplor-tab")   # the home-built class
})


# === SECTION: Phase 14e -- the html engine as the default renderer =========================

testthat::test_that("html is the default engine and its output is Viewer-routed / knittable", {
  testthat::expect_equal(getOption("tabxplor.tab_kable_engine"), "html")
  h <- tab_kable(tab(gss, marital, race, pct = "row"))
  # the `kableExtra` class is what print.kableExtra / knit_print.kableExtra dispatch on; without it a
  # bare knitr_kable just cat()s the markup to the console instead of opening the Viewer.
  testthat::expect_s3_class(h, "kableExtra")
  testthat::expect_s3_class(h, "knitr_kable")
  testthat::expect_equal(attr(h, "format"), "html")
  testthat::expect_match(as.character(h), '<table class="tabxplor-tab">', fixed = TRUE)
})

testthat::test_that("geometry is CLASSES, not inline styles (so a user's CSS can win)", {
  h <- as.character(tab_kable(tab(gss, marital, race, pct = "row", color = "diff"),
                              engine = "html", tooltips = FALSE))
  b <- rh_tbody(h)
  # An inline style beats any stylesheet rule short of !important, so ANY inline style on a cell is a
  # thing the user cannot restyle. The engine must emit none.
  testthat::expect_false(grepl("<td[^>]*style=", b))
  testthat::expect_false(grepl("<th[^>]*style=", h))
  testthat::expect_false(grepl("<tr[^>]*style=", b))
  # ... and the roles it emits instead are all defined by the stylesheet
  css <- tab_css(style_tag = FALSE)
  for (k in c("tx-r", "tx-l", "tx-num", "tx-br", "tx-bl", "tx-tot", "tx-rv", "tx-b", "tx-pill")) {
    testthat::expect_match(css, paste0("[.]", k, "\\b"), label = k)
  }
})

testthat::test_that("a wrapped header keeps its <br>, a user's markup is still escaped", {
  # tab_wrap_text() breaks long header names on "<br>"; escaping the whole label printed a literal
  # "Some very long<br>race level name" (kableExtra never hit this -- knitr::kable(escape = FALSE)).
  d <- gss; levels(d$race)[1] <- "Some very long race level name"
  h <- as.character(tab_kable(tab(d, marital, race, pct = "row"), engine = "html", tooltips = FALSE))
  testthat::expect_match(h, "<th[^>]*>[^<]*<br>")
  testthat::expect_false(grepl("&lt;br&gt;", h, fixed = TRUE))
  # only the tag we inject ourselves is restored: a "<" in a user's own level name stays escaped
  d2 <- gss; levels(d2$race)[1] <- "a <script> b"
  h2 <- as.character(tab_kable(tab(d2, marital, race, pct = "row"), engine = "html",
                               tooltips = FALSE))
  testthat::expect_false(grepl("<script>", h2, fixed = TRUE))
  testthat::expect_match(h2, "&lt;script&gt;", fixed = TRUE)
})

testthat::test_that("a background colour is a pill hugging the text, not a full-cell flood", {
  # a low ratio break, so a background fires on this data whatever the defaults are
  tb <- tab(gss, marital, race, pct = "row", color = c("diff", "ratio"),
            color_breaks = list(pct_ratio = list(over = 1.05)))
  h  <- as.character(tab_kable(tb, engine = "html", tooltips = FALSE))
  b <- rh_tbody(h)
  # the bg slot class rides the span; the <td> keeps the text slot (so `.p*` still cascades)
  testthat::expect_match(b, '<span class="tx-pill [ou][1-4]">')
  testthat::expect_false(grepl('<td class="[^"]*\\b[ou][1-4]\\b', b))
  testthat::expect_match(tab_css(style_tag = FALSE), "[.]tx-pill[{]border-radius")
})

testthat::test_that("format() pads with FIGURE spaces for html/Excel, ASCII for console/md", {
  # U+2007 is exactly a digit wide; an ASCII space is half a digit in DejaVu Sans, and CSS collapses
  # runs of them -- so console-aligned composites arrived ragged in html.
  t <- tab(gss, marital, race, pct = "row", display = "{pct} (n={n})")
  ht <- format(t$Other, html = TRUE, na = "", stars = TRUE)
  mt <- format(t$Other, na = "", stars = TRUE)
  testthat::expect_true(any(grepl(fig_space, ht, fixed = TRUE)))
  testthat::expect_false(any(grepl(fig_space, mt, fixed = TRUE)))
  testthat::expect_true(any(grepl("  ", mt, fixed = TRUE)))          # md keeps ASCII runs
  # same visible text either way -- only the pad character differs
  testthat::expect_identical(gsub(fig_space, " ", ht, fixed = TRUE), mt)
  # and it reaches the rendered media
  testthat::expect_true(grepl(fig_space, as.character(tab_kable(t, engine = "html")), fixed = TRUE))
  testthat::expect_false(grepl(fig_space, tab_md(t, print = FALSE, color = FALSE), fixed = TRUE))
})

# === SECTION: the label column -- rowspan + vertical name (Phase 14i) ========

testthat::test_that("html engine: a merged table names each row-variable once, via rowspan", {
  h <- rh_strip_style(as.character(
    tab_kable(tab(gss, c(race, marital), relig, pct = "row"), engine = "html", css = FALSE)))
  # one cell per block, spanning it -- not one per row
  testthat::expect_match(h, '<td class="[^"]*tx-lbl[^"]*" rowspan="4">race</td>')
  testthat::expect_match(h, '<td class="[^"]*tx-lbl[^"]*" rowspan="7">marital</td>')
  testthat::expect_length(gregexpr(">race</td>", h, fixed = TRUE)[[1]], 1L)
  testthat::expect_length(gregexpr("rowspan", h)[[1]], 2L)
  # the literal "row_var" header is gone (a bug fix, not a var_names setting)
  testthat::expect_no_match(h, ">row_var<", fixed = TRUE)
  # a rowspan must not desync the column-wise assembly: every row still closes
  n_tr <- lengths(regmatches(h, gregexpr("<tr", h)))
  testthat::expect_equal(n_tr, lengths(regmatches(h, gregexpr("</tr>", h))))
})

testthat::test_that("html engine: tx-vname only where the run is longer than one row", {
  # a rotated single-row cell just makes that row tall -> it falls back to horizontal.
  one <- tab(gss, c(race, marital), relig, pct = "row") |>
    dplyr::filter(!(!!rlang::sym("row_var") == "race") | !!rlang::sym("levels") == "Total")
  h <- rh_strip_style(as.character(tab_kable(one, engine = "html", css = FALSE)))
  testthat::expect_match(h, '<td class="[^"]*tx-lbl[^"]*" rowspan="1">race</td>')
  testthat::expect_no_match(h, 'tx-vname[^"]*" rowspan="1"')
})

testthat::test_that("html engine: var_names drops the row-name column / the col_var span", {
  merged <- tab(gss, c(race, marital), relig, pct = "row")
  h_of <- function(vn) rh_strip_style(as.character(
    tab_kable(merged, engine = "html", css = FALSE, var_names = vn)))
  testthat::expect_match(h_of("rows"), ">race</td>")
  testthat::expect_no_match(h_of("rows"), "tx-span", fixed = TRUE)
  testthat::expect_no_match(h_of("cols"), ">race</td>")
  testthat::expect_match(h_of("cols"), "tx-span", fixed = TRUE)
  testthat::expect_no_match(h_of("none"), "tx-span", fixed = TRUE)
  testthat::expect_no_match(h_of("none"), "rowspan", fixed = TRUE)
})

testthat::test_that("tab_css() carries the label / vertical-name role classes", {
  css <- tab_css(chrome = TRUE)
  testthat::expect_match(css, ".tx-lbl", fixed = TRUE)
  testthat::expect_match(css, ".tx-vname", fixed = TRUE)
  # NOT `sideways-lr`: still experimental with patchy support. vertical-rl + rotate(180deg) is the
  # universally-supported equivalent (bottom-to-top, matching tab_xl's 90-degree rotation).
  testthat::expect_no_match(css, "sideways", fixed = TRUE)
  testthat::expect_match(css, "writing-mode:vertical-rl", fixed = TRUE)
})
