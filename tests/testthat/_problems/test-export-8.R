# Extracted from test-export.R:8

# prequel ----------------------------------------------------------------------
t_row <- tab(forcats::gss_cat, race, marital, pct = "row", color = c("diff", "ratio"))

# test -------------------------------------------------------------------------
testthat::expect_no_error(as.character(tab_export(t_row, "kable")))
