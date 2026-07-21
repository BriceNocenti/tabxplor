# Extracted from test-render-html.R:86

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
counts   <- tab(gss, marital, race)
row_diff <- tab(gss, marital, race, pct = "row", color = "diff")
bg       <- tab(gss, marital, race, pct = "row", color = c("diff", "ratio"))
chi2     <- suppressWarnings(tab(gss, marital, race, pct = "row", test = TRUE))
testthat::expect_snapshot(cat(rh_strip_style(tab_kable(counts,   engine = "html"))))
testthat::expect_snapshot(cat(rh_strip_style(tab_kable(row_diff, engine = "html"))))
testthat::expect_snapshot(cat(rh_strip_style(tab_kable(bg,       engine = "html"))))
testthat::expect_snapshot(cat(rh_strip_style(suppressWarnings(tab_kable(chi2, engine = "html")))))
