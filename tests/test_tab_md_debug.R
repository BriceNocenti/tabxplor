devtools::load_all(".")
t <- tab(forcats::gss_cat, race, marital, pct = "row", color = "diff") |>
  dplyr::mutate(dplyr::across(dplyr::where(is_fmt), ~set_display(., "diff")))
x <- format(t$n, special_formatting = TRUE, na = "")
cat("format output: [", paste(x, collapse = "|"), "]\n")
cat("trimmed: [", paste(stringr::str_trim(x), collapse = "|"), "]\n")
cat("nchar: ", nchar(stringr::str_trim(x)), "\n")
cat("is.na nchar: ", is.na(nchar(stringr::str_trim(x))), "\n")
