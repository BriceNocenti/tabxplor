# Extracted from test-tab_md.R:401

# prequel ----------------------------------------------------------------------
gss <- forcats::gss_cat
tabs <- tab(gss, race, marital, pct = "row")
md <- tab_md(tabs, print = FALSE)
md_bold <- tab_md(tabs, bold_references = TRUE, print = FALSE)
md_no <- tab_md(tabs, bold_references = FALSE, print = FALSE)
gss_sub <- gss |> dplyr::filter(year %in% c(2000, 2014))
tabs_sub <- tab(gss_sub, race, marital, year, pct = "row")
tabs_col <- tab(gss, race, marital, pct = "row", color = "diff")
md_pandoc_html <- function(md) {
  f <- withr::local_tempfile(fileext = ".md")
  writeLines(md, f)
  out <- suppressWarnings(system2("pandoc", c(shQuote(f), "-t", "html"),
                                  stdout = TRUE, stderr = FALSE))
  paste(out, collapse = "\n")
}

# test -------------------------------------------------------------------------
md <- tab_md(tab(gss, marital, c(race, relig), pct = "row"), print = FALSE, color = FALSE)
sep <- strsplit(md, "\n")[[1]][2]
testthat::expect_match(sep, "|-|", fixed = TRUE)
