# Extracted from test-render-html.R:492

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
t <- tab(gss, marital, race, pct = "row", display = "{pct} (n={n})")
ht <- format(t$Other, html = TRUE, na = "", stars = TRUE)
mt <- format(t$Other, na = "", stars = TRUE)
testthat::expect_true(any(grepl(fig_space, ht, fixed = TRUE)))
testthat::expect_false(any(grepl(fig_space, mt, fixed = TRUE)))
testthat::expect_true(any(grepl("  ", mt, fixed = TRUE)))
testthat::expect_identical(gsub(fig_space, " ", ht, fixed = TRUE), mt)
