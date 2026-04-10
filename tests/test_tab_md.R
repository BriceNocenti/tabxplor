devtools::load_all(".")

cat("\n=== Test 1: Simple row percentages ===\n")
tab(forcats::gss_cat, race, marital, pct = "row") |> tab_md()

cat("\n\n=== Test 2: Diff with ref = tot ===\n")
tab(forcats::gss_cat, race, marital, pct = "row", color = "diff") |> tab_md()

cat("\n\n=== Test 3: Diff with ref = first ===\n")
tab(forcats::gss_cat, race, marital, pct = "row", color = "diff", ref = "first") |> tab_md()

cat("\n\n=== Test 4: Multiple row_vars (compact) ===\n")
tab_many(forcats::gss_cat, c(race, year), marital, pct = "row") |> tab_md()

cat("\n\n=== Test 5: Multiple col_vars ===\n")
tab_many(forcats::gss_cat, race, c(marital, relig), pct = "row") |> tab_md()

cat("\n\n=== Test 6: Counts (no pct) ===\n")
tab(forcats::gss_cat, race, marital) |> tab_md()

cat("\n\n=== Test 7: Column percentages ===\n")
tab(forcats::gss_cat, race, marital, pct = "col") |> tab_md()

cat("\n\n=== Test 8: bold_references = FALSE ===\n")
tab(forcats::gss_cat, race, marital, pct = "row") |> tab_md(bold_references = FALSE)

cat("\n\n=== Test 9: Explicit diff display (set_display on whole tab) ===\n")
tab(forcats::gss_cat, race, marital, pct = "row", color = "diff") |>
  set_display("diff") |> tab_md()

cat("\n\n=== Test 9b: Explicit diff display (mutate/across) ===\n")
tab(forcats::gss_cat, race, marital, pct = "row", color = "diff") |>
  dplyr::mutate(dplyr::across(dplyr::where(is_fmt), ~set_display(., "diff"))) |>
  tab_md()

cat("\n\n=== Test 10: Tab with tab_vars ===\n")
tab(forcats::gss_cat, race, marital, year, pct = "row") |> tab_md()
