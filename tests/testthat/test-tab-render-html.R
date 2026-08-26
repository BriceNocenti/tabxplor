# PURPOSE: the dependency-free html engine and the stylesheet it ships with.
# ROLE: the shipped CONTRACT for R/tab-render-html.R, R/tab-css.R -- a failure here is a user-visible change.
#   The exhaustive sweep around it lives in dev/tests/testthat/.
# See: CLAUDE.md section "Testing" (what belongs in which suite).

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



# === SECTION: home-built html engine -- structure snapshot + self-contained ==============

testthat::test_that("tab_kable html engine structure is stable", {
  counts   <- tab(gss, marital, race)
  row_diff <- tab(gss, marital, race, pct = "row", color = "diff")
  bg       <- tab(gss, marital, race, pct = "row", color = c("diff", "ratio"))
  chi2     <- suppressWarnings(tab(gss, marital, race, pct = "row", test = TRUE))

  testthat::expect_snapshot(cat(rh_strip_style(tab_kable(counts))))
  testthat::expect_snapshot(cat(rh_strip_style(tab_kable(row_diff))))
  testthat::expect_snapshot(cat(rh_strip_style(tab_kable(bg))))
  testthat::expect_snapshot(cat(rh_strip_style(suppressWarnings(tab_kable(chi2)))))
})



testthat::test_that("html engine output is self-contained (inline <style>, no external <link>)", {
  h <- as.character(tab_kable(tab(gss, marital, race, pct = "row", color = "diff")))
  testthat::expect_match(h, "<table")
  testthat::expect_match(h, "<style")
  testthat::expect_false(grepl("<link", h))
  testthat::expect_false(grepl("includeCSS|lightable|cosmo", h))
})



# === SECTION: Phase 13d -- theme lives in the CSS, not the markup =========================

testthat::test_that("cells carry slot classes, never inline colour", {
  h <- as.character(tab_kable(tab(gss, marital, race, pct = "row", color = "diff"), tooltips = FALSE))
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



testthat::test_that("the generated CSS is syntactically valid in every mode", {
  # A single malformed rule makes the browser drop it -- and, inside @media, potentially the whole
  # block -- with no error anywhere. No selector-presence test catches that, so check the shape.
  # z11: "print_minimalistic" joins the list, and `@media print {` joins the at-rule opener stripped below.
  for (fmt in c("html", "md")) for (th in c("light", "dark", "auto", "print_minimalistic")) {
    css <- tab_css(theme = th, format = fmt, style_tag = FALSE)
    lab <- paste0(th, "/", fmt)
    testthat::expect_identical(lengths(regmatches(css, gregexpr("[{]", css))),
                               lengths(regmatches(css, gregexpr("[}]", css))), label = lab)
    body <- trimws(gsub("@media (print|\\(prefers-color-scheme: dark\\)) \\{|^\\}$", "",
                        strsplit(css, "\n")[[1]]))
    body <- body[nzchar(body)]
    testthat::expect_true(all(grepl("^[^{}]+\\{([-a-z]+:[^;{}]+;)+\\}$", body)), label = lab)
  }
})



testthat::test_that("css = FALSE drops the <style> but keeps the classes", {
  tb <- tab(gss, marital, race, pct = "row", color = "diff")
  h  <- as.character(tab_kable(tb, css = FALSE))
  testthat::expect_false(grepl("<style", h, fixed = TRUE))
  testthat::expect_match(rh_tbody(h), 'class="[^"]*\\b(p|m)[1-4]\\b')
  # the once-per-document workflow: options() drives it, and tab_css() supplies the stylesheet
  withr::local_options(list(tabxplor.kable_css = FALSE))
  testthat::expect_false(grepl("<style", as.character(tab_kable(tb)), fixed = TRUE))
  testthat::expect_match(tab_css(theme = "auto"), "^<style>")
})



testthat::test_that("geometry is CLASSES, not inline styles (so a user's CSS can win)", {
  h <- as.character(tab_kable(tab(gss, marital, race, pct = "row", color = "diff"), tooltips = FALSE))
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



testthat::test_that("no border SHORTHAND survives in the stylesheet (coloured cells, plain borders)", {
  # THE regression lock for the pass-2 defect "the text color actually change the borders colors ...
  # which is awful". `border-right:1px solid` is a shorthand: it resets border-right-color to
  # `currentColor` = the CELL's palette hex, and every border rule out-specifies the one
  # `td{border-color:...}` rule -- so the shorthand always won. Phase 14e moved the geometry off inline
  # styles and recorded the bug as fixed; that removed the INLINE half only, and three docs + NEWS
  # repeated the claim for two phases while a +20% cell kept drawing a blue border. Nothing tested it.
  # Both halves are locked here: the CSS uses longhands only, and a real cell carries both classes.
  for (th in c("light", "dark", "auto", "print_minimalistic")) {
    css <- tab_css(theme = th, style_tag = FALSE)
    testthat::expect_no_match(css, "border-(top|right|bottom|left)\\s*:", label = th)
    # ... and the rule that must therefore win is present, for every theme in the file
    testthat::expect_match(css, "border-color:", label = th)
  }
  # The markup half. It needs SEVERAL col_vars: a cell is only both bordered and coloured where a
  # `tx-br` column separator meets a coloured value, which no single-col_var fixture ever produces --
  # which is exactly why this survived unseen.
  b <- rh_tbody(as.character(tab_kable(tab(gss, marital, c(race, relig), pct = "row",
                                           color = "diff"), tooltips = FALSE)))
  tds <- unlist(regmatches(b, gregexpr('<td class="[^"]*"', b)))
  both <- grep("\\b(p|m)[1-4]\\b", grep("tx-br|tx-bl", tds, value = TRUE), value = TRUE)
  testthat::expect_gt(length(both), 0)
})



testthat::test_that("a wrapped header keeps its <br>, a user's markup is still escaped", {
  # tab_wrap_text() breaks long header names on "<br>"; escaping the whole label printed a literal
  # "Some very long<br>race level name" (kableExtra never hit this -- knitr::kable(escape = FALSE)).
  d <- gss; levels(d$race)[1] <- "Some very long race level name"
  h <- as.character(tab_kable(tab(d, marital, race, pct = "row"), tooltips = FALSE))
  testthat::expect_match(h, "<th[^>]*>[^<]*<br>")
  testthat::expect_false(grepl("&lt;br&gt;", h, fixed = TRUE))
  # only the tag we inject ourselves is restored: a "<" in a user's own level name stays escaped
  d2 <- gss; levels(d2$race)[1] <- "a <script> b"
  h2 <- as.character(tab_kable(tab(d2, marital, race, pct = "row"),
                               tooltips = FALSE))
  testthat::expect_false(grepl("<script>", h2, fixed = TRUE))
  testthat::expect_match(h2, "&lt;script&gt;", fixed = TRUE)
})



testthat::test_that("a background colour is a pill hugging the text, not a full-cell flood", {
  # a low ratio break, so a background fires on this data whatever the defaults are
  tb <- tab(gss, marital, race, pct = "row", color = c("diff", "ratio"),
            color_breaks = list(pct_ratio = list(over = 1.05)))
  h  <- as.character(tab_kable(tb, tooltips = FALSE))
  b <- rh_tbody(h)
  # the bg slot class rides the span; the <td> keeps the text slot (so `.p*` still cascades)
  testthat::expect_match(b, '<span class="tx-pill [ou][1-4]">')
  testthat::expect_false(grepl('<td class="[^"]*\\b[ou][1-4]\\b', b))
  testthat::expect_match(tab_css(style_tag = FALSE), "[.]tx-pill[{]border-radius")
})



# === SECTION: the label column -- rowspan + vertical name (Phase 14i) ========

testthat::test_that("html engine: a merged table names each row-variable once, via rowspan", {
  h <- rh_strip_style(as.character(
    tab_kable(tab(gss, c(race, marital), relig, pct = "row"), css = FALSE)))
  # one cell per block, spanning it -- not one per row. Phase 18m: common_totrow defaults FALSE, so
  # each block keeps its OWN Total row -> race spans 4 (3 data + Total), marital spans 7 (6 data + Total).
  testthat::expect_match(h, '<td class="[^"]*tx-lbl[^"]*" rowspan="4">race</td>')
  testthat::expect_match(h, '<td class="[^"]*tx-lbl[^"]*" rowspan="7">marital</td>')
  testthat::expect_length(gregexpr(">race</td>", h, fixed = TRUE)[[1]], 1L)
  # two label merges + the two index columns' headers over the unit row below them
  testthat::expect_length(gregexpr("rowspan", h)[[1]], 4L)
  # the literal "row_var" header is gone (a bug fix, not a var_names setting)
  testthat::expect_no_match(h, ">row_var<", fixed = TRUE)
  # a rowspan must not desync the column-wise assembly: every row still closes
  n_tr <- lengths(regmatches(h, gregexpr("<tr", h)))
  testthat::expect_equal(n_tr, lengths(regmatches(h, gregexpr("</tr>", h))))
})




# === Phase 22g-vii: the name column, the doubled span, and the publication marks ===================

testthat::test_that("a name rotates only when it saves width, and a compound name wraps", {
  names_of <- function(x) {
    b <- sub("^.*</style>", "", as.character(tab_html(x)))
    regmatches(b, gregexpr('<td class="[^"]*tx-lbl[^"]*"[^>]*>[^<]*(<br>[^<]*)*</td>', b))[[1]]
  }
  a <- car_arrests
  t <- suppressMessages(tab_reg(a, "checks", c("colour", "employed", "citizen"),
                                family = "gaussian", stats = FALSE))
  nm <- names_of(t)
  # "Constant" is a one-row block: it cannot turn, so it sets the floor and every name no longer
  # than it stays horizontal too -- rotating them would save nothing
  testthat::expect_false(any(grepl("tx-vname", nm[grepl(">employed<", nm, fixed = TRUE)])))
  testthat::expect_false(any(grepl("tx-vname", nm[grepl(">Constant<", nm, fixed = TRUE)])))
  # a name far longer than that floor is written horizontally too -- but WRAPPED, which nothing
  # could do before (stri_wrap breaks on whitespace, and a snake_case name has none)
  long <- dplyr::rename(a, shenaniganing_colorous_property_of_the_skin = "colour")
  t2 <- suppressMessages(tab_reg(long, "checks",
                                 c("shenaniganing_colorous_property_of_the_skin", "employed"),
                                 family = "gaussian", stats = FALSE))
  cell <- grep("shenaniganing", names_of(t2), value = TRUE)
  testthat::expect_length(cell, 1L)
  testthat::expect_match(cell, "<br>", fixed = TRUE)
  lines <- gsub("<[^>]*>", "", strsplit(sub("^<td[^>]*>", "", cell), "<br>", fixed = TRUE)[[1]])
  testthat::expect_gt(length(lines), 1L)
  testthat::expect_true(all(nchar(lines) <= tabxplor:::TX_VNAME_MAX + 1L))
})
