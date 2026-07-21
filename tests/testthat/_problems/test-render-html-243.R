# Extracted from test-render-html.R:243

# prequel ----------------------------------------------------------------------
gss <- forcats::gss_cat
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

# test -------------------------------------------------------------------------
tb <- tab(gss, marital, race, pct = "row", color = c("diff", "ratio"))
for (cl in c(TRUE, FALSE)) {
    spans <- unlist(regmatches(
      l <- tab_color_legend(tb, medium = "html", classes = cl),
      gregexpr("<span [^>]*>", l)))
    testthat::expect_true(length(spans) > 0)
    testthat::expect_true(all(grepl("font-weight:bold;", spans, fixed = TRUE)))
  }
testthat::expect_match(tab_color_legend(tb, medium = "md"), "[*][*]\\[[+]5\\]\\{[.]p1\\}[*][*]")
testthat::expect_match(tab_color_legend(tb, medium = "md"), "[*][*]\\[.2\\]\\{[.]o3\\}[*][*]")
runs <- tab_color_legend(tb, medium = "runs")[[1]]
coloured <- purrr::keep(runs, ~ !is.na(.$color))
testthat::expect_true(length(coloured) > 0)
testthat::expect_true(all(purrr::map_lgl(coloured, "bold")))
