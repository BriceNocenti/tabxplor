# Extracted from test-export.R:89

# prequel ----------------------------------------------------------------------
t_row <- tab(forcats::gss_cat, race, marital, pct = "row", color = c("diff", "ratio"))

# test -------------------------------------------------------------------------
merged <- tab(forcats::gss_cat, c(race, relig), marital, pct = "row")
k_both <- as.character(tab_export(merged, "kable", engine = "html", css = FALSE))
