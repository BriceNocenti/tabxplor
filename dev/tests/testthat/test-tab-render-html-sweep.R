
# === SECTION: the html engine =====================================================================

gss <- fx_gss()


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


testthat::test_that("the MARKUP is theme-agnostic; only the stylesheet differs", {
  tb <- tab(gss, marital, race, pct = "row", color = "diff")
  mk <- function(th) rh_strip_style(tab_kable(tb, theme = th))
  cs <- function(th) as.character(tab_kable(tb, theme = th))
  # This is the property that makes "auto" possible at all -- one DOM, three stylesheets.
  testthat::expect_identical(mk("light"), mk("dark"))
  testthat::expect_identical(mk("light"), mk("auto"))
  testthat::expect_false(identical(cs("light"), cs("dark")))
})


testthat::test_that("the html stars legend uses &#42; entities so pandoc cannot eat the stars", {
  # A knitted page's raw-html block goes THROUGH pandoc (Rmd -> md -> html on pkgdown/Quarto):
  # its markdown-in-html parsing paired the legend's `***: ... **: ... *:` runs as emphasis and the
  # stars vanished from every knitted page (Viewer/jamovi/standalone were fine -- no re-parse).
  # `&#42;` renders as `*` everywhere but is plain text to pandoc.
  t_stars <- tab(gss, marital, race, pct = "row", color = "diff", stars = TRUE)
  h <- as.character(tab_kable(t_stars, css = FALSE, tooltips = FALSE))
  testthat::expect_match(h, "&#42;&#42;&#42;:", fixed = TRUE)      # the legend's *** run, entity-encoded
  foot <- regmatches(h, regexpr("<div class=\"tx-foot\">.*", h))
  testthat::expect_no_match(foot, "\\*{2,3}:")                     # no raw pairable star runs left
})


testthat::test_that("the auto cascade layers are ordered so a page toggle beats the OS", {
  # THE contract of theme = "auto". `@media (prefers-color-scheme)` only reports the OS; Quarto and
  # friends toggle a CLASS, which a media query cannot see. The hook layers must therefore come AFTER
  # the media block (they also out-specify it), and light before dark. Reordering these silently makes
  # an explicit page toggle lose to the reader's OS -- which no assertion on hex would catch.
  auto <- as.character(tab_kable(tab(gss, marital, race, pct = "row", color = "diff"), theme = "auto"))
  testthat::expect_lt(regexpr("@media", auto, fixed = TRUE),
                      regexpr("body.quarto-light", auto, fixed = TRUE))
  testthat::expect_lt(regexpr("body.quarto-light", auto, fixed = TRUE),
                      regexpr("body.quarto-dark", auto, fixed = TRUE))
})


testthat::test_that("the theme is read from options(tabxplor.theme), explicit wins", {
  tb <- tab(gss, marital, race, pct = "row", color = "diff")
  withr::local_options(list(tabxplor.theme = "dark"))
  testthat::expect_identical(as.character(tab_kable(tb)),
                             as.character(tab_kable(tb, theme = "dark")))
  testthat::expect_identical(as.character(tab_kable(tb, theme = "light")),
                             as.character(withr::with_options(list(tabxplor.theme = "light"),
                                                              tab_kable(tb))))
})


testthat::test_that("legend weight: text break-words bold, background break-words plain (Phase g)", {
  tb <- tab(gss, marital, race, pct = "row", color = c("diff", "ratio"))  # text = diff, bg = ratio
  # html: TEXT break-words are bold; BACKGROUND break-words are PLAIN (they mirror filled cells, which
  # a fill alone does not bold). Holds on the class path AND the inline-hex one.
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
  # the fill channel keeps breaks 2 and 4, drawn with slots 1 and 3 (see fmt_color_plan).
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
  for (fmt in c("html", "md")) {
    css <- tab_css(style_tag = FALSE, format = fmt)
    testthat::expect_match(css, ".p1,.p2,.p3,.p4,.m1,.m2,.m3,.m4{font-weight:bold;}", fixed = TRUE)
    testthat::expect_false(grepl("[.]o1[^{]*[{][^}]*font-weight", css))
  }
  # theme-independent -> emitted ONCE, not per cascade layer
  testthat::expect_equal(lengths(regmatches(a <- tab_css(theme = "auto", style_tag = FALSE),
                                            gregexpr(".p1,.p2,.p3,.p4,.m1,.m2,.m3,.m4{font-weight:bold;}",
                                                     a, fixed = TRUE))), 1L)
})


# === SECTION: get_data + what a NON-tabxplor input renders ===============================

testthat::test_that("get_data returns a data.frame", {
  tb <- tab(gss, marital, race, pct = "row", color = "diff")
  testthat::expect_s3_class(tab_kable(tb, get_data = TRUE), "data.frame")
})


# Phase 19l: this is THE contract that had to survive deleting kable_tabxplor_style(), whose one
# selling point was "styles any data.frame". Three inputs, one renderer:
#   (a) a plain tibble/data.frame  -> degrades (a note + a bare <table>), never an error;
#   (b) a table that merely LOST its class, fmt columns intact -> NOT degraded, fully coloured
#       (test-degraded-attrs.R's contract -- the colour lives on the columns);
#   (c) a real tab                 -> the full render.
testthat::test_that("tab_html renders a plain data.frame, a declassed tab and a real tab", {
  df <- data.frame(a = 1:3, b = letters[1:3])
  testthat::expect_message(h_df <- as.character(tab_html(df)), "not a tabxplor table")
  testthat::expect_match(h_df, "<table")
  testthat::expect_match(as.character(suppressMessages(tab_export(df, "html"))), "<table")
  testthat::expect_match(as.character(suppressMessages(tab_html(tibble::as_tibble(df)))), "<table")

  tb <- tab(gss, marital, race, pct = "row", color = "diff")
  declassed <- tb; class(declassed) <- c("tbl_df", "tbl", "data.frame")
  h_dc <- as.character(tab_html(declassed))
  testthat::expect_match(h_dc, "<table")
  testthat::expect_match(rh_tbody(h_dc), 'class="[^"]*\\b(p|m)[1-4]\\b')   # still coloured

  testthat::expect_match(rh_tbody(as.character(tab_html(tb))), 'class="[^"]*\\b(p|m)[1-4]\\b')
})


testthat::test_that("tab_kable renders a non-mergeable list instead of erroring (list method)", {
  # different col_vars -> not mergeable (previously an error for kable)
  lst <- list(tab(gss, marital, race,  pct = "row"),
              tab(gss, marital, relig, pct = "row"))
  h <- as.character(suppressWarnings(tab_kable(lst)))
  testthat::expect_gte(lengths(gregexpr("<table", h)), 2L)  # both tables rendered
})


# Phase 19l: the retired export arguments are absorbed and reported, never acted on.
testthat::test_that("retired export arguments are inert and report once", {
  tb <- tab(gss, marital, race, pct = "row", color = "diff")
  plain <- as.character(tab_html(tb))
  for (a in list(list(engine = "kableExtra"), list(engine = "html"), list(color_type = "text"),
                 list(html_24_bit = TRUE), list(html_font = "serif"), list(full_width = TRUE))) {
    lifecycle::expect_deprecated(do.call(tab_html, c(list(tb), a)))
    h <- suppressWarnings(do.call(tab_html, c(list(tb), a)))
    testthat::expect_identical(as.character(h), plain)               # inert: byte-identical
  }
  # tab_export() reports it ONCE and does not forward it to the child exporter
  lifecycle::expect_deprecated(tab_export(tb, "html", engine = "kableExtra"))
})


# === SECTION: Phase 14e -- the html renderer's output shape ================================

testthat::test_that("the output is Viewer-routed / knittable", {
  h <- tab_kable(tab(gss, marital, race, pct = "row"))
  # the `knitr_kable` class is what the print / knit_print fall-through dispatches on; without it a
  # bare knitr_kable just cat()s the markup to the console instead of opening the Viewer.
  testthat::expect_s3_class(h, "knitr_kable")
  testthat::expect_s3_class(h, "knitr_kable")
  testthat::expect_equal(attr(h, "format"), "html")
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
  starred <- suppressWarnings(tab_reg(d, "married", c("race", "relig")))
  hp <- as.character(tab_kable(plain, css = FALSE))
  hs <- as.character(tab_kable(starred, css = FALSE))
  testthat::expect_no_match(hp, "tx-has-stars")
  testthat::expect_match(hs, 'class="tabxplor-tab tx-has-stars"', fixed = TRUE)
})


testthat::test_that("the footnote does not SIZE the table", {
  # The real cause of "not compact enough -- levels and Total columns are very wide for nothing".
  # The legend cell spans every column and its prose is ~330 chars on one line, so it, not the data,
  # decided the table's max-content -- and a table is as wide as min(max-content, available), so it
  # took the whole pane and auto layout padded every column with the slack. `width:0` makes the cell
  # contribute 0 to max-content; `min-width:100%` refills it once the table is sized by its data.
  h <- as.character(tab_kable(tab(gss, marital, race, pct = "row", color = "diff")))
  testthat::expect_match(h, '<td colspan="5"><div class="tx-foot">', fixed = TRUE)
  css <- tab_css(style_tag = FALSE)
  # ... and no COLUMN carries a width floor any more: auto-sizing is the default (?tab_css shows how
  # to pin one). Match a SIZING width only -- at the start of a declaration, so `border-top-width` and
  # friends don't count. The tx-foot pair + the tooltip/popover caps are all that may remain.
  widths <- unlist(regmatches(css, gregexpr("(?<=[;{])(min-|max-)?width:[^;}]*", css, perl = TRUE)))
  # the scrollbox pair is a SIZING width and belongs here: the box hugs the table up to the space
  # it has, which is what makes the content overflow and the bar appear.
  testthat::expect_setequal(widths, c("width:max-content", "max-width:100%",
                                      "width:0", "min-width:100%",
                                      "max-width:none", "max-width:none", "max-width:none"))
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


testthat::test_that("html engine: tx-vname only where the run is longer than one row", {
  # a rotated single-row cell just makes that row tall -> it falls back to horizontal. Keep a single
  # DATA level for race (not its Total, which the Phase 14n collapse would drop as a duplicate).
  one <- tab(gss, c(race, marital), relig, pct = "row") |>
    dplyr::filter(!(!!rlang::sym("row_var") == "race") | !!rlang::sym("levels") == "White")
  h <- rh_strip_style(as.character(tab_kable(one, css = FALSE)))
  testthat::expect_match(h, '<td class="[^"]*tx-lbl[^"]*" rowspan="1">race</td>')
  testthat::expect_no_match(h, 'tx-vname[^"]*" rowspan="1"')
})


testthat::test_that("tab_css() carries the label / vertical-name role classes", {
  css <- tab_css(format = "html")
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
  k <- tab_kable(tb)
  testthat::expect_s3_class(k, "tabxplor_kable")
  testthat::expect_identical(attr(k, "tabxplor_theme"), "light")
  testthat::expect_identical(attr(tab_kable(tb, theme = "auto"),
                                  "tabxplor_theme"), "auto")
  # css = FALSE: the document supplies the stylesheet (or nothing does). In the Viewer there IS no
  # document, so painting the page dark would leave an unstyled black-on-#222222 table.
  testthat::expect_null(attr(tab_kable(tb, css = FALSE), "tabxplor_theme"))
  withr::with_options(list(tabxplor.kable_css = FALSE),
                      testthat::expect_null(attr(tab_kable(tb), "tabxplor_theme")))
})


testthat::test_that("print() is byte-identical to today when it is not an interactive Viewer print", {
  tb <- tab(gss, marital, race, pct = "row", color = "diff")
  k  <- tab_kable(tb)
  testthat::expect_s3_class(k, "tabxplor_kable")
  # testthat is never interactive, so this IS the branch the suite runs: NextMethod() -> knitr's
  # print -> cat(). It also covers a knit (`print()` inside a chunk) and tabxplor.view_html = FALSE.
  # NOTE the assertion is deliberately on the OUTPUT, not the return value: knitr's print returns
  # cat()'s NULL, and "byte-identical to today" outranks the returns-its-input convention here.
  testthat::expect_identical(utils::capture.output(print(k)),
                             utils::capture.output(cat(as.character(k))))
})


testthat::test_that("the page never leaks into the returned html, whatever the theme", {
  tb <- tab(gss, marital, race, pct = "row", color = "diff")
  # The returned object is what gets written to a file or knitted into someone else's document: it
  # must carry the table and the cascade, never a page repaint and never a resolved toggle.
  for (th in c("light", "dark", "auto")) {
    h <- as.character(tab_kable(tb, theme = th))
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
  testthat::expect_match(h, paste0("html,body{background:", tabxplor:::tx_chrome_hex("dark")$bg),
                         fixed = TRUE)
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


testthat::test_that("tabxplor_kable print degrades when the html dependencies are absent", {
  km <- tabxplor:::kable_print_mode
  # non-interactive / no theme / view-off / knitting all fall through to the base method
  testthat::expect_identical(km("dark", FALSE, TRUE, FALSE, TRUE), "next")
  testthat::expect_identical(km(NULL,   TRUE,  TRUE, FALSE, TRUE), "next")
  testthat::expect_identical(km("dark", TRUE,  TRUE, TRUE,  TRUE), "next")
  # interactive themed print WITH the deps -> the themed Viewer page
  testthat::expect_identical(km("dark", TRUE,  TRUE, FALSE, TRUE), "viewer")
  # interactive themed print WITHOUT them -> graceful degrade (note + knitr print), never a crash
  testthat::expect_identical(km("dark", TRUE,  TRUE, FALSE, FALSE), "degrade")
})


# === SECTION: options(tabxplor.print) html routing + tooltips option =====================
# The taught value is "html" (tab_kable was renamed tab_html in Phase 18g); "kable" stays a
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


testthat::test_that("a rotated name wraps to its block's height, breaking before the operator", {
  d <- fx_gss() |>
    dplyr::mutate(married = factor(dplyr::if_else(marital == "Married", "Married", "Not married")))
  # `tvhours*marital` (15 chars over a 6-row block) is longer than the floor "Constant" sets, so it
  # rotates; two vertical lines then fit, and the break falls before the cross operator.
  t <- suppressMessages(tab_reg(d, "married", c("relig", "tvhours*marital"), family = "binomial",
                                stats = FALSE))
  h <- as.character(tab_html(t))
  testthat::expect_match(h, "tvhours<br>", fixed = TRUE)
  testthat::expect_match(h, "*marital", fixed = TRUE)
  # ... and a name no longer than that floor stays horizontal, whole: rotating it would save nothing
  t2 <- suppressMessages(tab_reg(d, "married", c("relig", "age*race"), family = "binomial",
                                 stats = FALSE))
  h2 <- as.character(tab_html(t2))
  testthat::expect_match(h2, ">age*race</td>", fixed = TRUE)
  testthat::expect_no_match(h2, "tx-vname[^\"]*\" rowspan=\"3\">age", perl = TRUE)
})


# --- Phase 22b-xviii: the background is a colour MEASURE, so it stops at the primary token --------

test_that("the pill wraps the primary token alone, and does not move it", {
  d <- suppressWarnings(fx_gss_fmt())
  t <- suppressMessages(tab_reg(d, "married", c("race", "rincome", "relig", "age"),
                                family = "binomial", measure = "difference",
                                color = c(TRUE, "adjustment"), stats = FALSE))
  h <- paste(as.character(tab_html(t)), collapse = "\n")
  tds <- regmatches(h, gregexpr("<td[^>]*>.*?</td>", h))[[1]]
  pill <- grep("tx-pill", tds, value = TRUE)
  expect_gt(length(pill), 0L)
  # the aside spans sit OUTSIDE the pill: it opens after any leading one and closes before the stars
  expect_true(all(grepl('<span class="tx-pill [a-z0-9]+">[^<]*</span>', pill)))
  expect_false(any(grepl('<span class="tx-pill [^"]*">[^<]*<span class="tx-sec"', pill)))
  # ... and the fill bleeds around the glyphs instead of shifting them (the 4px drift)
  expect_true(grepl(".tx-pill{border-radius:4px;padding:1px 4px;margin:0 -4px;}", h, fixed = TRUE))
})


test_that("the hover gap reads like every other interval in the package", {
  d <- suppressWarnings(fx_gss_fmt())
  t <- suppressMessages(tab_reg(d, "married", c("race", "rincome", "relig", "age"),
                                family = "binomial", measure = "difference",
                                color = c(TRUE, "adjustment"), stats = FALSE))
  h <- paste(as.character(tab_html(t)), collapse = "\n")
  tips <- unlist(regmatches(h, gregexpr('title="[^"]*gap:[^"]*"', h)))
  expect_gt(length(tips), 0L)
  expect_false(any(grepl(" pts", tips, fixed = TRUE)))   # a unit the cell itself never prints
  expect_true(any(grepl("gap: [-+][0-9.]+% \\[[^]]*\\]%", tips)))
})


testthat::test_that("a publication palette's marks are ink, not aside grey", {
  tc <- tab(fx_gss(), race, marital, pct = "row", color = "difference")
  h  <- as.character(tab_html(tc, theme = "print_marks"))
  testthat::expect_match(h, ".tabxplor-tab .tx-mark{color:#000000", fixed = TRUE)
  testthat::expect_match(h, ".tabxplor-tab .tx-sec{color:#444444", fixed = TRUE)
  testthat::expect_match(h, '<span class="tx-mark">', fixed = TRUE)
  # the stars stay an aside: a colour theme writes no mark span at all
  testthat::expect_no_match(sub("^.*</style>", "", as.character(tab_html(tc))),
                            "tx-mark", fixed = TRUE)
})


# === SECTION: the html engine =====================================================================

gss <- fx_gss()


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


testthat::test_that("theme drives the emitted CSS (light / dark / auto)", {
  tb <- tab(gss, marital, race, pct = "row", color = "diff")
  cs <- function(th) as.character(tab_kable(tb, theme = th))

  light <- cs("light")
  # z11: "@media" alone is no longer the marker -- every stylesheet now carries an `@media print`
  # block (the publication palette). What a STATIC theme must not emit is the AUTO cascade: the
  # prefers-color-scheme query and the page-toggle hooks.
  testthat::expect_false(grepl("@media (prefers-color-scheme", light, fixed = TRUE))
  testthat::expect_true(grepl(".tabxplor-tab{color:#000000;background:#ffffff;}", light, fixed = TRUE))

  dark <- cs("dark")
  testthat::expect_false(grepl("@media (prefers-color-scheme", dark, fixed = TRUE))
  # Phase 14e: dark is #CECDC3 on #222222 -- pure white on near-black is a glare-y contrast for body
  # text. Read the values from tx_chrome_hex() rather than re-hardcoding them here.
  dk <- tabxplor:::tx_chrome_hex("dark")
  testthat::expect_true(grepl(paste0("border-color:", dk$border), dark, fixed = TRUE))

  auto <- cs("auto")
  testthat::expect_true(grepl("@media (prefers-color-scheme: dark)", auto, fixed = TRUE))
  testthat::expect_true(grepl("body.quarto-dark",     auto, fixed = TRUE))
  testthat::expect_true(grepl("body.quarto-light",    auto, fixed = TRUE))
  testthat::expect_true(grepl("[data-bs-theme=dark]", auto, fixed = TRUE))
})


testthat::test_that("cell colour classes are emitted bare AND scoped (Bootstrap-host proofing)", {
  # pkgdown stamps class="table" on every table; Bootstrap 5's `.table>:not(caption)>*>*` (0,1,1)
  # then sets color/background-color on the SAME td our class sits on, beating a bare `.p1` (0,1,0)
  # -- every cell colour washed out on the pkgdown site while the legend spans survived. The scoped
  # twin `.tabxplor-tab .p1` (0,2,0) out-specifies it with no !important; the bare selector stays for
  # tab_md's editor contract and the legend spans outside the wrapper.
  for (fmt in c("html", "md")) {
    css <- tab_css(format = fmt, style_tag = FALSE)
    lab <- fmt
    for (cls in c("p1", "m4", "o1", "u4")) {
      testthat::expect_match(css, paste0(".", cls, ",.tabxplor-tab .", cls, "{"),
                             fixed = TRUE, label = lab)
    }
  }
  # the greys (grey_non_signif cells) are cell classes too, chrome-only
  testthat::expect_match(tab_css(style_tag = FALSE), ".g1,.tabxplor-tab .g1{", fixed = TRUE)
})


testthat::test_that("the colour legend uses slot classes, never inline hex", {
  tb   <- tab(gss, marital, race, pct = "row", color = "diff")
  foot <- function(h) regmatches(as.character(h),
                                 regexpr("(?s)<tfoot>.*?</tfoot>", as.character(h), perl = TRUE))
  # The legend sits in the table's own <tfoot>, so inline hex would freeze it while the cells it
  # describes follow a toggle. The discriminator is "does our stylesheet ship?", NOT the theme:
  # theme = "light" + css = FALSE is a real case (the document supplies tab_css("auto") itself),
  # and inline hex would be wrong there too. (The hex path survives for the media that carry no
  # stylesheet -- see `classes = FALSE` in the next test, and tab_xl.)
  for (th in c("light", "dark", "auto")) {
    f <- foot(tab_kable(tb, theme = th, color_legend = TRUE))
    testthat::expect_false(grepl("color:#", f, fixed = TRUE))   # no inline hex ANYWHERE in the legend
    testthat::expect_match(f, '<span class="(p|m)[1-4]"')
  }
})


testthat::test_that("n_min blanked cells render empty (no literal NA)", {
  tb <- suppressWarnings(tab(gss, marital, race, pct = "row", n_min = 1000))
  h <- as.character(suppressWarnings(tab_kable(tb)))
  testthat::expect_false(grepl(">NA<", h))       # no literal NA cell content
  testthat::expect_match(h, "<table")
})


testthat::test_that("a row reaches each role class once, and an unstyled row has no class attribute", {
  # `radd` appends; it is not a set union. The last row is normally also a totblock_bottom, so it
  # emitted class="tx-bb tx-bb".
  h   <- as.character(tab_kable(tab(gss, c(marital, relig), race, pct = "row")))
  trs <- unlist(regmatches(h, gregexpr("<tr[^>]*>", h)))
  cls <- gsub('.*class="([^"]*)".*', "\\1", grep("class=", trs, value = TRUE))
  testthat::expect_false(any(vapply(strsplit(cls, " +"), function(v) anyDuplicated(v) > 0, logical(1))))
  testthat::expect_false(grepl('<tr class="">', h, fixed = TRUE))
  # a row_var block's last row is both tx-bb and tx-bb2; the 2px wins on source order (tab-css.R)
  testthat::expect_true(any(grepl("tx-bb2", cls)))
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
  testthat::expect_true(grepl(fig_space, as.character(tab_kable(t)), fixed = TRUE))
})


testthat::test_that("html engine: var_names drops the row-name column / the col_var span", {
  merged <- tab(gss, c(race, marital), relig, pct = "row")
  h_of <- function(vn) rh_strip_style(as.character(
    tab_kable(merged, css = FALSE, var_names = vn)))
  testthat::expect_match(h_of("rows"), ">race</td>")
  testthat::expect_no_match(h_of("rows"), "tx-span", fixed = TRUE)
  testthat::expect_no_match(h_of("cols"), ">race</td>")
  testthat::expect_match(h_of("cols"), "tx-span", fixed = TRUE)
  testthat::expect_no_match(h_of("none"), "tx-span", fixed = TRUE)
  # no LABEL merge (there is no name column left to merge); the index column's header still takes
  # the unit row below it, which is a header decision, not a variable name
  testthat::expect_no_match(h_of("none"), "tx-lbl[^\"]*\" rowspan")
})


# ---- Phase 22b-x: what the FIRST column draws, and how a long block name is folded ---------------

testthat::test_that("a row_var separator stops at the name column; the model-fit block's does not", {
  d <- fx_gss() |>
    dplyr::mutate(married = factor(dplyr::if_else(marital == "Married", "Married", "Not married")))
  t <- suppressMessages(tab_reg(d, "married", c("race", "tvhours"), family = "binomial"))
  h <- as.character(tab_html(t))
  cells <- regmatches(h, gregexpr('<td class="[^"]*"[^>]*>[^<]*', h))[[1]]
  nm <- grep("tx-lbl", cells, value = TRUE)
  # a ONE-ROW block's name cell is a direct child of its closing row, so it used to draw a rule its
  # multi-row neighbours did not: `tx-nb` opts every name cell out of the row separator
  testthat::expect_true(any(grepl("tx-nb[^\"]*\" rowspan=\"1\"", nm)))
  testthat::expect_true(any(grepl("tx-nb", nm, fixed = TRUE) & grepl("rowspan=\"3\"", nm)))
  # "Model fit" carries a NARROW NO-BREAK space by then (tab_wrap_text), so match the first word
  testthat::expect_true(any(grepl("tx-bb2", nm[grepl(">Model", nm, fixed = TRUE)], fixed = TRUE)))
})


testthat::test_that("a col_var is named once in the span row", {
  a <- car_arrests
  t <- tab(a, colour, released, pct = "row", ref = "first") |>
    dplyr::mutate(difference = set_display(.data$Yes, "difference"),
                  odds_ratio = set_display(.data$Yes, "odds_ratio"))
  h <- as.character(tab_html(t))
  span <- regmatches(h, regexpr("<thead><tr>.*?</tr>", h))
  # the Total column carries no label of its own, so the variable used to open a SECOND labelled run
  # and print its name twice
  testthat::expect_equal(lengths(regmatches(span, gregexpr("released", span))), 1L)
})
