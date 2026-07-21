# Extracted from test-tab_md.R:535

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
md_blank_rows <- function(md) {
  lines <- strsplit(md, "\n")[[1]]
  body  <- grep("^[|]", lines, value = TRUE)              # pipe rows only (skip legend / :::)
  body[grepl("^[| ]+$", body) & !grepl("[-:]", body)]     # all-space cells, not the delimiter
}

# test -------------------------------------------------------------------------
md <- tab_md(tab(gss, marital, race, pct = "row", color = "diff"), print = FALSE)
testthat::expect_match(md, "::: {.tabxplor-tab}", fixed = TRUE)
testthat::expect_no_match(md, "<style>", fixed = TRUE)
plain <- tab_md(tab(gss, marital, race, pct = "row"), print = FALSE, color = FALSE)
testthat::expect_no_match(plain, ":::", fixed = TRUE)
