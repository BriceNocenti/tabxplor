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
# output is version-unstable -- 2.0.0 -> 1.4.1 moved the rgba alpha (255 -> 1), dropped leading
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
  # Last Phase m: the default "summary" test rows are p-value + effect size (2 extra; the statistic row
  # was dropped).
  bc <- rh_tbody(rh_strip_style(suppressWarnings(tab_kable(chi2, engine = "kableExtra"))))
  testthat::expect_equal(lengths(regmatches(bc, gregexpr("<tr", bc)))[[1]], nrow(chi2) + 2L)

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

testthat::test_that("legend weight: text break-words bold, background break-words plain (Phase g)", {
  tb <- tab(gss, marital, race, pct = "row", color = c("diff", "ratio"))  # text = diff, bg = ratio
  # html: TEXT break-words are bold; BACKGROUND break-words are PLAIN (they mirror filled cells, which
  # a fill alone does not bold). Holds on the class path AND the inline-hex (kableExtra) path.
  for (cl in c(TRUE, FALSE)) {
    l     <- tab_color_legend(tb, medium = "html", classes = cl)
    spans <- unlist(regmatches(l, gregexpr("<span [^>]*>", l)))
    testthat::expect_true(length(spans) > 0)
    is_bg <- grepl("background", spans) | grepl('class="[ou]', spans)
    testthat::expect_true(any(is_bg) && any(!is_bg))
    testthat::expect_true(all(grepl("font-weight:bold;", spans[!is_bg], fixed = TRUE)))
    testthat::expect_false(any(grepl("font-weight:bold;", spans[is_bg], fixed = TRUE)))
  }
  # md: text breaks carry `**`; background breaks do not (plain bracketed span).
  testthat::expect_match(tab_color_legend(tb, medium = "md"), "[*][*]\\[[+]5\\]\\{[.]p1\\}[*][*]")
  testthat::expect_match(tab_color_legend(tb, medium = "md"), "\\[.2\\]\\{[.]o3\\}")
  testthat::expect_no_match(tab_color_legend(tb, medium = "md"), "[*][*]\\[.2\\]\\{[.]o3\\}")
  # runs (excel / plot): the text channel is bold, the background channel plain.
  runs <- tab_color_legend(tb, medium = "runs")[[1]]
  coloured <- purrr::keep(runs, ~ !is.na(.$color))
  testthat::expect_true(length(coloured) > 0)
  testthat::expect_true(any(purrr::map_lgl(coloured, "bold")))
  testthat::expect_true(any(!purrr::map_lgl(coloured, "bold")))
  # console: cli::style_bold wraps the text colour style. `cli.num_colors` is the gate (read by
  # cli::num_ansi_colors()) -- testthat pins it to 1 for reproducible output, so force it here.
  withr::with_options(list(cli.num_colors = 256), {
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
  for (k in c("tx-r", "tx-l", "tx-num", "tx-br", "tx-bl", "tx-b", "tx-pill")) {
    testthat::expect_match(css, paste0("[.]", k, "\\b"), label = k)
  }
  # ... except tx-tot / tx-rv, which Phase 14j left deliberately UNSTYLED: the browser auto-sizes
  # every column, so their old min-widths could only ever be too big. They are still emitted, as the
  # hooks a user pins a width on (?tab_css) -- which is the whole point of roles over inline styles.
  for (k in c("tx-tot", "tx-rv")) {
    testthat::expect_no_match(css, paste0("[.]", k, "\\{"), label = k)
    testthat::expect_match(b, paste0("class=\"[^\"]*", k), label = k)
  }
})

testthat::test_that("numbers are monospace by default so figures stay column-aligned", {
  # Phase g: numbers are MONOSPACE by default (was: proportional unless the table showed stars).
  # Proportional digits drift out of alignment, worse under bold references / significant cells, so the
  # one `.tx-num` body rule ships the monospace stack + the size bump. Text stays Condensed; numeric
  # HEADERS (th.tx-num) keep the condensed sans (the rule is `td.tx-num`, body-only). One revert lever.
  css <- tab_css(style_tag = FALSE)
  testthat::expect_match(css, "font-family:\"DejaVu Sans Condensed\"", fixed = TRUE)   # text: Condensed
  num_rule <- regmatches(css, regexpr("[.]tabxplor-tab td[.]tx-num\\{[^}]*\\}", css))
  testthat::expect_match(num_rule, "Cascadia Mono", fixed = TRUE)
  testthat::expect_match(num_rule, "monospace")
  testthat::expect_match(num_rule, "font-size:1.1em;line-height:1;", fixed = TRUE)     # body size bump
  # the number font is one revert lever
  css2 <- withr::with_options(
    list(tabxplor.tab_kable_num_font = "\"Courier New\", monospace"), tab_css(style_tag = FALSE))
  testthat::expect_match(css2, "[.]tabxplor-tab td[.]tx-num\\{[^}]*Courier New")
})

testthat::test_that("the html engine flags a starred table with tx-has-stars, a plain one not", {
  plain   <- tab(gss, marital, race, pct = "row", color = "diff")
  d <- gss; d$married <- as.integer(d$marital == "Married")
  starred <- suppressWarnings(tab_logit(d, "married", c("race", "relig")))
  hp <- as.character(tab_kable(plain,   engine = "html", css = FALSE))
  hs <- as.character(tab_kable(starred, engine = "html", css = FALSE))
  testthat::expect_no_match(hp, "tx-has-stars")
  testthat::expect_match(hs, 'class="tabxplor-tab tx-has-stars"', fixed = TRUE)
})

testthat::test_that("no border SHORTHAND survives in the stylesheet (coloured cells, plain borders)", {
  # THE regression lock for the pass-2 defect "the text color actually change the borders colors ...
  # which is awful". `border-right:1px solid` is a shorthand: it resets border-right-color to
  # `currentColor` = the CELL's palette hex, and every border rule out-specifies the one
  # `td{border-color:...}` rule -- so the shorthand always won. Phase 14e moved the geometry off inline
  # styles and recorded the bug as fixed; that removed the INLINE half only, and three docs + NEWS
  # repeated the claim for two phases while a +20% cell kept drawing a blue border. Nothing tested it.
  # Both halves are locked here: the CSS uses longhands only, and a real cell carries both classes.
  for (th in c("light", "dark", "auto")) {
    css <- tab_css(theme = th, style_tag = FALSE)
    testthat::expect_no_match(css, "border-(top|right|bottom|left)\\s*:", label = th)
    # ... and the rule that must therefore win is present, for every theme in the file
    testthat::expect_match(css, "border-color:", label = th)
  }
  # The markup half. It needs SEVERAL col_vars: a cell is only both bordered and coloured where a
  # `tx-br` column separator meets a coloured value, which no single-col_var fixture ever produces --
  # which is exactly why this survived unseen.
  b <- rh_tbody(as.character(tab_kable(tab(gss, marital, c(race, relig), pct = "row",
                                           color = "diff"), engine = "html", tooltips = FALSE)))
  tds <- unlist(regmatches(b, gregexpr('<td class="[^"]*"', b)))
  both <- grep("\\b(p|m)[1-4]\\b", grep("tx-br|tx-bl", tds, value = TRUE), value = TRUE)
  testthat::expect_gt(length(both), 0)
})

testthat::test_that("the footnote does not SIZE the table", {
  # The real cause of "not compact enough -- levels and Total columns are very wide for nothing".
  # The legend cell spans every column and its prose is ~330 chars on one line, so it, not the data,
  # decided the table's max-content -- and a table is as wide as min(max-content, available), so it
  # took the whole pane and auto layout padded every column with the slack. `width:0` makes the cell
  # contribute 0 to max-content; `min-width:100%` refills it once the table is sized by its data.
  h <- as.character(tab_kable(tab(gss, marital, race, pct = "row", color = "diff"), engine = "html"))
  testthat::expect_match(h, '<td colspan="5"><div class="tx-foot">', fixed = TRUE)
  css <- tab_css(style_tag = FALSE)
  testthat::expect_match(css, ".tx-foot{width:0;min-width:100%;}", fixed = TRUE)
  # ... and no COLUMN carries a width floor any more: auto-sizing is the default (?tab_css shows how
  # to pin one). Match a SIZING width only -- at the start of a declaration, so `border-top-width` and
  # friends don't count. The tx-foot pair + the tooltip/popover caps are all that may remain.
  widths <- unlist(regmatches(css, gregexpr("(?<=[;{])(min-|max-)?width:[^;}]*", css, perl = TRUE)))
  testthat::expect_setequal(widths, c("width:0", "min-width:100%", "max-width:none", "max-width:none"))
})

testthat::test_that("a row reaches each role class once, and an unstyled row has no class attribute", {
  # `radd` appends; it is not a set union. The last row is normally also a totblock_bottom, so it
  # emitted class="tx-bb tx-bb".
  h   <- as.character(tab_kable(tab(gss, c(marital, relig), race, pct = "row"), engine = "html"))
  trs <- unlist(regmatches(h, gregexpr("<tr[^>]*>", h)))
  cls <- gsub('.*class="([^"]*)".*', "\\1", grep("class=", trs, value = TRUE))
  testthat::expect_false(any(vapply(strsplit(cls, " +"), function(v) anyDuplicated(v) > 0, logical(1))))
  testthat::expect_false(grepl('<tr class="">', h, fixed = TRUE))
  # a row_var block's last row is both tx-bb and tx-bb2; the 2px wins on source order (tab-css.R)
  testthat::expect_true(any(grepl("tx-bb2", cls)))
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

testthat::test_that("format() pads with FIGURE spaces for html/Excel, ASCII for the console", {
  # U+2007 is exactly a digit wide; an ASCII space is half a digit in DejaVu Sans, and CSS collapses
  # runs of them -- so console-aligned composites arrived ragged in html.
  t <- tab(gss, marital, race, pct = "row", display = "{pct} (n={n})")
  ht <- format(t$Other, html = TRUE, na = "", stars = TRUE)
  mt <- format(t$Other, na = "", stars = TRUE)                       # format() default = the console
  testthat::expect_true(any(grepl(fig_space, ht, fixed = TRUE)))
  testthat::expect_false(any(grepl(fig_space, mt, fixed = TRUE)))
  testthat::expect_true(any(grepl("  ", mt, fixed = TRUE)))          # the console keeps ASCII runs
  # same visible text either way -- only the pad character differs: a figure space inside numbers, and
  # (Phase g A6) a non-breaking space at the composite join " (n=..." so html does not wrap it.
  testthat::expect_identical(
    gsub(intToUtf8(160L), " ", gsub(fig_space, " ", ht, fixed = TRUE), fixed = TRUE), mt)
  # and it reaches the rendered media
  testthat::expect_true(grepl(fig_space, as.character(tab_kable(t, engine = "html")), fixed = TRUE))
})

testthat::test_that("tab_md pads VALUE-INTERNAL alignment with figure space, cell edges with ASCII", {
  # Phase 14m-ii, Item A: markdown sets no font of its own, so a pandoc-rendered table lands in the
  # host's PROPORTIONAL font -- where ASCII pad collapses and "100% (n=  673)" arrives ragged. The
  # figure space (a digit wide, non-collapsing) goes ONLY inside a value; cell-edge padding + spacer
  # columns stay ASCII, so pandoc still strips them and an empty cell renders `<td></td>` (`:empty`,
  # the hook Phase 14m keys on). nchar is unchanged, so the raw-markdown column layout is unmoved.
  t  <- tab(gss, marital, race, pct = "row", display = "{pct} (n={n})")
  md <- tab_md(t, print = FALSE, color = FALSE, css = FALSE)
  testthat::expect_true(grepl(fig_space, md, fixed = TRUE))          # the (n=...) padding is figure space
  testthat::expect_true(grepl("  ", md, fixed = TRUE))               # cell-edge padding stays ASCII
  # a coloured table with empty cells: no cell is written as a lone run of figure spaces (that would
  # render `<td> </td>`, breaking `:empty`). Blank/spacer cells must be ASCII-emptied.
  tc  <- tab(gss, c(race, marital), relig, pct = "row", color = "diff")
  rows <- strsplit(tab_md(tc, print = FALSE, color = TRUE, css = FALSE), "\n", fixed = TRUE)[[1]]
  testthat::expect_false(any(grepl(paste0("\\|", fig_space, "+\\|"), rows)))
})

# === SECTION: the label column -- rowspan + vertical name (Phase 14i) ========

testthat::test_that("html engine: a merged table names each row-variable once, via rowspan", {
  h <- rh_strip_style(as.character(
    tab_kable(tab(gss, c(race, marital), relig, pct = "row"), engine = "html", css = FALSE)))
  # one cell per block, spanning it -- not one per row. Last Phase m: common_totrow defaults FALSE, so
  # each block keeps its OWN Total row -> race spans 4 (3 data + Total), marital spans 7 (6 data + Total).
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
  # a rotated single-row cell just makes that row tall -> it falls back to horizontal. Keep a single
  # DATA level for race (not its Total, which the Phase 14n collapse would drop as a duplicate).
  one <- tab(gss, c(race, marital), relig, pct = "row") |>
    dplyr::filter(!(!!rlang::sym("row_var") == "race") | !!rlang::sym("levels") == "White")
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


# === SECTION: Phase 14k -- theme = "auto" resolution + the Viewer page =====================
# Everything here is PURE: tx_kable_page() takes the detector as an argument, so no test depends on
# the host IDE, and none needs interactive() (testthat never is -- which is exactly why the print
# method's own gate is asserted from the OTHER side, below).

testthat::test_that('the export theme default is "light" -- "auto" is opt-in', {
  # Phase 14k reversed the roadmap's plan to flip this default. A dark table is a deliberate choice;
  # every export (file, knit, Excel, plot) must stay light until someone asks otherwise.
  testthat::expect_identical(getOption("tabxplor.theme"), "light")          # what .onLoad() sets
  testthat::expect_identical(resolve_export_opts(NULL, allow_auto = TRUE)$theme, "light")
  # and the hard-coded fallback agrees, so an unset option can never silently mean "auto" either
  withr::local_options(list(tabxplor.theme = NULL))
  testthat::expect_identical(resolve_export_opts(NULL, allow_auto = TRUE)$theme, "light")
})

testthat::test_that("tx_page_style() paints the page from the SAME hex as the table", {
  ch_l <- tx_chrome_hex("light"); ch_d <- tx_chrome_hex("dark")
  testthat::expect_identical(tx_page_style("light"),
                             paste0("html,body{background:", ch_l$bg, ";color:", ch_l$text, ";}"))
  testthat::expect_identical(tx_page_style("dark"),
                             paste0("html,body{background:", ch_d$bg, ";color:", ch_d$text, ";}"))
  # a page we WRITE (tab_html_string) is opened elsewhere, so "auto" stays with the reader's browser
  testthat::expect_match(tx_page_style("auto"), "@media (prefers-color-scheme: dark)", fixed = TRUE)
  testthat::expect_match(tx_page_style("auto"), ch_d$bg, fixed = TRUE)
  # no !important anywhere: our <style> rides in the BODY, after save_html()'s head rule and after
  # bootstrap's, so plain source order already wins. If this ever fails, something moved to <head>.
  for (th in c("light", "dark", "auto")) {
    testthat::expect_no_match(tx_page_style(th), "!important", fixed = TRUE)
  }
})

testthat::test_that('tx_kable_page(): "auto" resolves R-side and declares an explicit toggle', {
  tbl <- '<table class="tabxplor-tab">T</table>'
  for (d in c("dark", "light")) {
    p <- tx_kable_page(tbl, theme = "auto", detected = d)
    # the toggle is symmetric: the cascade must be forced in BOTH directions, because the Viewer's
    # @media reports the OS and can therefore be wrong either way about the editor.
    testthat::expect_match(p, paste0('<div data-theme="', d, '">'), fixed = TRUE)
    testthat::expect_match(p, "</div>", fixed = TRUE)
    testthat::expect_match(p, tx_chrome_hex(d)$bg, fixed = TRUE)     # pane matches the table
    testthat::expect_match(p, tbl, fixed = TRUE)                     # table passed through verbatim
  }
})

testthat::test_that("tx_kable_page(): an explicit theme never consults the detector", {
  p <- tx_kable_page("<table>T</table>", theme = "dark", detected = "light")
  testthat::expect_match(p, tx_chrome_hex("dark")$bg, fixed = TRUE)
  # no wrapper at all: with an explicit theme the stylesheet is ONE static layer carrying no hook
  # rule, so a data-theme div would be inert markup -- and its absence proves the detector cannot leak.
  testthat::expect_no_match(p, "data-theme", fixed = TRUE)
  testthat::expect_no_match(p, tx_chrome_hex("light")$bg, fixed = TRUE)
})

testthat::test_that("the data-theme toggle actually matches a rule in the auto cascade", {
  # tx_kable_page() reuses tab_css()'s own documented hooks rather than inventing a fifth layer, so
  # this is the join between the two halves: the div is worthless if no selector names it.
  css <- tab_css(theme = "auto", style_tag = FALSE)
  testthat::expect_match(css, "[data-theme=dark] .tabxplor-tab", fixed = TRUE)
  testthat::expect_match(css, "[data-theme=light] .tabxplor-tab", fixed = TRUE)
})

testthat::test_that("the page theme rides along ONLY when our stylesheet ships with the table", {
  tb <- tab(gss, marital, race, pct = "row", color = "diff")
  # THE RULE: we paint a page only when we styled the table sitting on it.
  k <- tab_kable(tb, engine = "html")
  testthat::expect_s3_class(k, "tabxplor_kable")
  testthat::expect_identical(attr(k, "tabxplor_theme"), "light")
  testthat::expect_identical(attr(tab_kable(tb, engine = "html", theme = "auto"),
                                  "tabxplor_theme"), "auto")
  # css = FALSE: the document supplies the stylesheet (or nothing does). In the Viewer there IS no
  # document, so painting the page dark would leave an unstyled black-on-#222222 table.
  testthat::expect_null(attr(tab_kable(tb, engine = "html", css = FALSE), "tabxplor_theme"))
  withr::with_options(list(tabxplor.kable_css = FALSE),
                      testthat::expect_null(attr(tab_kable(tb, engine = "html"), "tabxplor_theme")))
})

testthat::test_that("the kableExtra engine is never page-painted", {
  testthat::skip_if_not_installed("kableExtra")
  tb <- tab(gss, marital, race, pct = "row", color = "diff")
  # It bakes its own theme (kable_material_dark paints its table #363640, which would sit two-tone on
  # our #222222 pane) and its degrade branch returns a bare kbl() with no theme at all. Same rule:
  # our stylesheet does not ship, so the page is not ours to paint.
  for (th in c("light", "dark")) {
    k <- tab_kable(tb, engine = "kableExtra", theme = th)
    testthat::expect_false(inherits(k, "tabxplor_kable"))
    testthat::expect_null(attr(k, "tabxplor_theme"))
  }
})

testthat::test_that("print() is byte-identical to today when it is not an interactive Viewer print", {
  tb <- tab(gss, marital, race, pct = "row", color = "diff")
  k  <- tab_kable(tb, engine = "html")
  testthat::expect_s3_class(k, "tabxplor_kable")
  # testthat is never interactive, so this IS the branch the suite runs: NextMethod() -> kableExtra's
  # print -> cat(). It also covers a knit (`print()` inside a chunk) and kableExtra_view_html = FALSE.
  # NOTE the assertion is deliberately on the OUTPUT, not the return value: kableExtra's print returns
  # cat()'s NULL, and "byte-identical to today" outranks the returns-its-input convention here.
  testthat::expect_identical(utils::capture.output(print(k)),
                             utils::capture.output(cat(as.character(k))))
})

testthat::test_that("the page never leaks into the returned html, whatever the theme", {
  tb <- tab(gss, marital, race, pct = "row", color = "diff")
  # The returned object is what gets written to a file or knitted into someone else's document: it
  # must carry the table and the cascade, never a page repaint and never a resolved toggle.
  for (th in c("light", "dark", "auto")) {
    h <- as.character(tab_kable(tb, engine = "html", theme = th))
    # tab_css() must never paint html,body -- that is the HOST's, and repainting it around the table
    # would recolour the whole document (Phase 13d). Asserted on the stylesheet itself.
    testthat::expect_no_match(h, "html,body{", fixed = TRUE)
    # ... and no data-theme WRAPPER. Strip the <style> first: under "auto" the cascade legitimately
    # names `[data-theme=dark]` as a SELECTOR -- that is the hook waiting for a host, not a decision.
    testthat::expect_no_match(rh_strip_style(h), "data-theme", fixed = TRUE)
  }
})

testthat::test_that("tab_html_string() paints the standalone page it builds", {
  tb <- tab(gss, marital, race, pct = "row")
  h  <- tab_html_string(tb, theme = "dark")
  testthat::expect_match(h, "html,body{background:#222222", fixed = TRUE)
  # in <head>, before the table -- and the file is opened elsewhere, so "auto" keeps the @media
  # cascade rather than resolving R-side the way the Viewer's own page does.
  testthat::expect_lt(regexpr("html,body{", h, fixed = TRUE), regexpr("<body>", h, fixed = TRUE))
  testthat::expect_match(tab_html_string(tb, theme = "auto"),
                         "@media (prefers-color-scheme: dark)", fixed = TRUE)
  # no stylesheet shipped => nothing of ours to match => no paint (the same one rule)
  testthat::expect_no_match(tab_html_string(tb, css = FALSE), "html,body{", fixed = TRUE)
})

testthat::test_that("Phase 17g: output_kable renders a two-channel colour after finalize", {
  # Regression: options(tabxplor.output_kable = TRUE) + a two-channel colour (color = TRUE ->
  # c(text = "diff", bg = "ratio")) used to error "no applicable method for 'mutate' ...
  # tabxplor_kable" -- the render ran INSIDE the build, before finalize_color_spec, which then
  # mutate()d the returned kable. The render now runs at tab()'s tail, post-finalize.
  op <- options(tabxplor.output_kable = TRUE); on.exit(options(op), add = TRUE)
  k <- tab(gss, marital, race, pct = "row", color = TRUE)   # must not error
  testthat::expect_s3_class(k, "tabxplor_kable")
  # the background channel (ratio) must be present -> the finalised two-channel colour reached render:
  # a coloured cell carries a slot class. Assert non-vacuously that some cell is coloured.
  h <- rh_strip_style(as.character(k))
  testthat::expect_match(h, "class=\"[^\"]*tx-", perl = TRUE)
  # single-channel path (which silently survived the old bug) still works
  k1 <- tab(gss, marital, race, pct = "row", color = "diff")
  testthat::expect_s3_class(k1, "tabxplor_kable")
})

testthat::test_that("Phase 17g: tabxplor_kable print degrades when kableExtra is absent", {
  km <- tabxplor:::kable_print_mode
  # non-interactive / no theme / view-off / knitting all fall through to the base method
  testthat::expect_identical(km("dark", FALSE, TRUE, FALSE, TRUE), "next")
  testthat::expect_identical(km(NULL,   TRUE,  TRUE, FALSE, TRUE), "next")
  testthat::expect_identical(km("dark", TRUE,  TRUE, TRUE,  TRUE), "next")
  # interactive themed print WITH kableExtra -> the themed Viewer page
  testthat::expect_identical(km("dark", TRUE,  TRUE, FALSE, TRUE), "viewer")
  # interactive themed print WITHOUT kableExtra -> graceful degrade (note + knitr print), never a crash
  testthat::expect_identical(km("dark", TRUE,  TRUE, FALSE, FALSE), "degrade")
})

# === SECTION: options(tabxplor.print) html routing + tooltips option =====================
# The taught value is "html" (tab_kable was renamed tab_html in Last Phase g); "kable" stays a
# working synonym. knit_print methods make a bare `tab(...)` chunk render as a real html table in
# Rmd/Quarto instead of knitr's default text capture.

testthat::test_that("tabxplor.print accepts html (taught) and kable (synonym)", {
  t1 <- tab(gss, marital, race, pct = "row")
  for (val in c("html", "kable")) {
    withr::local_options(list(tabxplor.print = val))
    txt <- utils::capture.output(res <- withVisible(print(t1)))
    testthat::expect_false(res$visible)
    testthat::expect_s3_class(res$value, "tabxplor_kable")
  }
  # multi-table list routes the same way (the "kable" spelling is locked in test-display-13c.R)
  withr::local_options(list(tabxplor.print = "html"))
  tl <- tab(gss, c(marital, relig), race, pct = "row", output_list = TRUE)
  testthat::expect_s3_class(tl, "tabxplor_tabs")
  outl <- utils::capture.output(res <- withVisible(print(tl)))
  testthat::expect_false(res$visible)
  testthat::expect_s3_class(res$value, "tabxplor_tabs")
  testthat::expect_true(any(grepl("<table", outl, fixed = TRUE)))
  # default stays the console tibble render
  withr::local_options(list(tabxplor.print = "console"))
  txt <- utils::capture.output(print(t1))
  testthat::expect_match(paste(txt, collapse = "\n"), "A tabxplor tab")
})

testthat::test_that("knit_print renders a bare tab as as-is html under tabxplor.print = html", {
  t1 <- tab(gss, marital, race, pct = "row")
  withr::local_options(list(tabxplor.print = "html"))
  k <- knitr::knit_print(t1)
  testthat::expect_s3_class(k, "knit_asis")
  testthat::expect_match(as.character(k), "<table", fixed = TRUE)
  # grouped tab (tab_vars) has its own registration (its class vector lacks "tabxplor_tab")
  tg <- tab(gss, marital, race, year, pct = "row")
  testthat::expect_s3_class(tg, "tabxplor_grouped_tab")
  kg <- knitr::knit_print(tg)
  testthat::expect_s3_class(kg, "knit_asis")
  # console mode falls through to knitr's default text capture (the fansi-hookable path)
  withr::local_options(list(tabxplor.print = "console"))
  txt <- utils::capture.output(res <- knitr::knit_print(t1))
  testthat::expect_false(inherits(res, "knit_asis"))
})

testthat::test_that("tabxplor.tab_kable_tooltips = FALSE strips tooltips document-wide", {
  t1 <- tab(gss, marital, race, pct = "row")
  h_on  <- as.character(tab_html(t1))
  testthat::expect_match(h_on, 'data-toggle="tooltip"', fixed = TRUE)
  withr::local_options(list(tabxplor.tab_kable_tooltips = FALSE))
  h_off <- as.character(tab_html(t1))
  testthat::expect_no_match(h_off, 'data-toggle="tooltip"', fixed = TRUE)
  # the per-call argument still wins over the option
  h_arg <- as.character(tab_html(t1, tooltips = TRUE))
  testthat::expect_match(h_arg, 'data-toggle="tooltip"', fixed = TRUE)
})
