# Extracted from test-tab_md.R:181

# prequel ----------------------------------------------------------------------
gss <- forcats::gss_cat
tabs <- tab(gss, race, marital, pct = "row")
md <- tab_md(tabs, print = FALSE)
md_bold <- tab_md(tabs, bold_references = TRUE, print = FALSE)
md_no <- tab_md(tabs, bold_references = FALSE, print = FALSE)
gss_sub <- gss |> dplyr::filter(year %in% c(2000, 2014))
tabs_sub <- tab(gss_sub, race, marital, year, pct = "row")

# test -------------------------------------------------------------------------
md <- tab_md(tabs, print = FALSE)
lines <- strsplit(md, "\n")[[1]]
sep_idx <- which(grepl("^\\|[-:| ]+\\|$", lines))
testthat::expect_length(sep_idx, 1)
sep_line <- lines[sep_idx]
testthat::expect_true(grepl("-:", sep_line))
