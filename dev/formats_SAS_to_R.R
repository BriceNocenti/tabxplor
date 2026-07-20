# formats_SAS_to_R() -- INSEE SAS formats to R: translate an INSEE `formats_sas.txt` into R
# forcats::fct_recode() code. Moved OUT of the package (R/utils.R) in Phase 17a: it was unexported,
# had no callers, and is a personal maintainer tool, not part of tabxplor's public surface. Kept here
# (dev/, .Rbuildignore'd) so it can be `source()`d when needed. `tabxplor:::formats_SAS_to_R(...)`
# no longer resolves -- source this file instead.

# path <- "~/Data/Enquêtes élections/Enquête Participation électorale 2017/Doc/Formats/formats_sas.txt"
# name_in = "pa17"
# name_out = "pa17"
# open = TRUE
# remove_at_end_of_var = "f"
# not_if_numeric = TRUE
# text_aposthophe = "'"
# # path_out
#
# formats_SAS_to_R("~/Data/Enquêtes élections/Enquête Participation électorale 2017/Doc/Formats/formats_sas.txt",
#                  name_in = "pa17", name_out = "pa17")

#' INSEE SAS formats to R : translate code
#'
#' @param path The path of the file with the sas formats
#' @param name_in The name of the unformatted database
#' @param name_out The name of the database to be formatted, if not the same than `name_in`.
#' @param open Should the file be opened, or just its path printed ?
#' @param remove_at_end_of_var Set to `f` or `F` the final f in variables names in the sas file.
#' @param not_if_numeric Should the code prevent numeric variables to get recoded ?
#' @param text_aposthophe How do apostrophes in labels appear ?
#' @param path_out The path, name and extension of the output file. In temporary directory
#' if not provide.
#'
#' @return A file with R code.
#' @keywords internal
formats_SAS_to_R <- function (path, name_in, name_out, open = TRUE, remove_at_end_of_var = "f",
                              not_if_numeric = TRUE, text_aposthophe = "'", path_out)  {
  f <- stringi::stri_read_raw(path)
  format <- stringi::stri_enc_detect(f)
  format <- format[[1]]$Encoding[1]
  con <- file(path, encoding = format)
  f <- readLines(con)

  f <- f |>
    stringi::stri_replace_all_regex("\t", "") |>
    stringi::stri_replace_all_regex(text_aposthophe, stringi::stri_unescape_unicode("\\u2019")) |>
    stringi::stri_replace_all_regex("\"", "'") |>
    stringi::stri_replace_all_regex(";value", "value") |>
    stringi::stri_replace_all_regex("\\s+", " ") |>
    stringi::stri_trim_both()

  f <- f[stringi::stri_detect_regex(f, "^value|=") & !stringi::stri_detect_regex(f, "^proc")] # "^value *\\$|="
  f[stringi::stri_detect_regex(f, "=")] <-
    f[stringi::stri_detect_regex(f, "=")] |>
    stringi::stri_replace_first_regex("^([^ =]+) ", "'$1' ") |>
    #stringi::stri_replace_first_regex("^([^ ]+) ", "'\\1' ") |>
    stringi::stri_replace_first_regex("''", "'") |>
    stringi::stri_replace_first_regex("' += +'", "'='") |>
    stringi::stri_replace_first_regex("'([^']+)'='([^']+)'", "'$1-$2' = '$1',") |>
    stringi::stri_replace_first_regex(";$", "")
  f_var <- stringi::stri_extract_first_regex(f[stringi::stri_detect_regex(f, "^value")],  "[^ ]+$")
  if (!is.null(remove_at_end_of_var)) f_var <- stringi::stri_replace_first_regex(f_var, paste0(remove_at_end_of_var, "$"), "")

  f[1:30]

  if (not_if_numeric) {
    f[stringi::stri_detect_regex(f, "^value")] <-
      paste0("if('",
             f_var, "' %in% names(", name_out, ") & !is.numeric(",
             name_out, "$", f_var, ")) {\n", name_out, "$", f_var,
             " <- forcats::fct_recode(", name_in, "$", f_var,
             ",")
  } else {
    f[stringi::stri_detect_regex(f, "^value")] <-
      paste0("if('",
             f_var, "' %in% names(", name_out, ")) {\n", name_out,
             "$", f_var, " <- forcats::fct_recode(as.factor(",
             name_in, "$", f_var, "),")
  }
  data <- dplyr::tibble(f = f) |>  # ???
    dplyr::mutate(group = cumsum(as.integer(stringi::stri_detect_regex(f, "^if\\(")))) |>
    dplyr::group_by(.data$group) |>
    dplyr::mutate(f = dplyr::if_else(dplyr::row_number() == dplyr::n(),
                                     paste0(f, ")\n}\n"),
                                     f
    )
    ) |>
    dplyr::ungroup()

  data <- data |>
    dplyr::mutate(var = dplyr::if_else(stringi::stri_detect_regex(f, "^if\\("),
                         true  = stringi::stri_extract_first_regex(f, "^if\\('[^']+'") |>
                           stringi::stri_replace_first_regex("if\\(", "") |> stringi::stri_replace_all_regex("'", ""),
                         false = NA_character_)
    ) |>
    tidyr::fill(tidyselect::all_of(c("var")))

  no_path_out <- missing(path_out)
  if (no_path_out) {
    path_out <- file.path(tempdir(), paste0("formats_R-", name_out, ".R"))
  }

  writeLines(data$f, path_out, useBytes = TRUE)
  if (open) {
    if (requireNamespace("rstudioapi", quietly = TRUE)) {
      rstudioapi::navigateToFile(path_out)
    } else {
      file.show(path_out)
    }

  } else if (no_path_out) {
    message(path_out)
  }
  invisible(data)
}
