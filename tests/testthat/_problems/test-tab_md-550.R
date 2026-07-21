# Extracted from test-tab_md.R:550

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
col <- tab_md(tab(gss, marital, race, pct = "row", color = "diff"), print = FALSE)
testthat::expect_length(md_blank_rows(col), 1L)
t_tv <- tab(gss, marital, race, year, pct = "row", color = "diff") |>
    dplyr::filter(year %in% c(2000, 2006))
testthat::expect_gte(length(md_blank_rows(tab_md(t_tv, print = FALSE))), 2L)
t_tv_plain <- tab(gss, marital, race, year, pct = "row") |>
    dplyr::filter(year %in% c(2000, 2006))
plain <- strsplit(tab_md(t_tv_plain, print = FALSE, color = FALSE), "\n")[[1]]
testthat::expect_true(any(grepl("^[|] +-+", plain)))
