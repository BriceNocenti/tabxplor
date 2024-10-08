
#' Pipe operator
#'
#' See \code{magrittr::\link[magrittr:pipe]{\%>\%}} for details.
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
#' @param lhs A value or the magrittr placeholder.
#' @param rhs A function call using the magrittr semantics.
#' @return Pipe an object forward into a function or call expression.
NULL

# Rlang .data to bind data masking variable in dplyr
#' @keywords internal
#' @importFrom rlang .data
NULL



#' fct_recode helper to recode multiple variables
#'
#' @param .data The data frame.
#' @param .cols <\link[tidyr:tidyr_tidy_select]{tidy-select}> The variables to recode.
#' @param .data_out_name The name of the output data frame, if different from the
#' input data frame.
#' @param cat By default the result is written in the console if there are less than
#' 6 variables, written in a temporary file and opened otherwise. Set to
#' false to get a data frame with a character variable instead.
#'
#' @return When the number of variables is less than 5, a text in console as a side effect.
#' With more than 5 variables, a temporary R file. A `tibble` with the recode text as a
#' character variable is returned invisibly (or as main result if `cat = TRUE`).
#' @export
fct_recode_helper <- function(.data, .cols = -where(is.numeric), .data_out_name, cat = TRUE) {
  .data_in_name <- rlang::enquo(.data) %>% rlang::as_name()
  if(missing(.data_out_name)) .data_out_name <- .data_in_name

  pos_cols <- tidyselect::eval_select(rlang::enquo(.cols), .data)
  .data <- .data[pos_cols]
  .data <- .data %>% dplyr::mutate(dplyr::across(.cols = dplyr::everything(), .fns = as.factor))

  recode <- .data %>%
    purrr::map(~ paste0("\"",
                        #stringi::stri_escape_unicode(
                        stringr::str_replace_all(
                          levels(.), "\"", "'"
                          #)
                        ),
                        "\"")) %>%
    purrr::map(
      ~ paste0(stringr::str_pad(., max(stringr::str_length(.)), "right"), " = ",
               stringr::str_pad(., max(stringr::str_length(.)), "right"), collapse = ",\n")
    ) %>%
    purrr::imap(~ paste0(.data_out_name, "$", .y, " <- fct_recode(\n",
                         .data_in_name, "$", .y, ",\n",
                         .x, "\n)\n\n"

    )) %>% purrr::flatten_chr() %>%
    tibble::tibble(recode = .)

  if (cat == FALSE) return(recode)

  if (ncol(.data) <= 5) {
    cat(recode$recode)
  } else {
    path <- tempfile("", fileext = ".R")
    writeLines(recode$recode, path, useBytes = TRUE)

    if (requireNamespace("rstudioapi", quietly = TRUE)) {
      rstudioapi::navigateToFile(path)
    } else {
      file.show(path)
    }

  }

  invisible(recode)
}




#' @keywords internal
.onLoad <- function(libname, pkgname) {
  # options "tabxplor.color_style_type" and "tabxplor.color_style_theme" :
  set_color_style()

  # option "tabxplor.color_breaks" :
  set_color_breaks(pct_breaks       = c(0.05, 0.1, 0.2, 2, 0.3),
                   #pct_ratio_breaks = 2,
                   mean_breaks      = c(1.15, 1.5, 2, 4),
                   contrib_breaks   = c(1, 2, 5, 10)  )

  options("tabxplor.print" = "console") # options("tabxplor.print" = "kable")

  options("tabxplor.kable_html_font" =
            '"DejaVu Sans", "Arial", arial, helvetica, sans-serif') # Condensed ?

  options("tabxplor.output_kable" = FALSE)

  options("tabxplor.cleannames" = FALSE)

  options("tabxplor.export_dir" = NULL)

  options("tabxplor.kable_popover" = FALSE)

  options("tabxplor.ci_print" = "ci") # or "moe"

  options("tabxplor.always_add_css_in_tab_kable" = TRUE)

  invisible()
}

# getOption("tabxplor.color_breaks")
# getOption("tabxplor.color_style_theme")
# getOption("tabxplor.color_style_type")
# get_color_breaks()
# get_color_style()




#Functions and options to work with factors and lists -------------

#' A regex pattern to clean the names of factors.
#' @keywords internal
# @export
cleannames_condition <- function()
  "^[^- ]+-(?![[:lower:]])|^[^- ]+(?<![[:lower:]])-| *\\(.+\\)"





#' Create a score variable from factors
#'
#' @param data A data.frame.
#' @param name The name of the variable to create.
#' @param vars_list The list of the factors to count
#' (only the first level is counted, as 1) ; as a character vector.
#'
#' @return The data.frame, with a new variable.
#' @export
#'
#' @examples
#' data <- tibble::tibble(group = factor(c("G1", "G1", "G2", "G2", "G3", "G3")),
#'                        a = factor(c("Oui", "Oui", "Oui", "Oui", "Non", "Oui")),
#'                        b = factor(c("Oui", "Non", "Non", "Oui", "Non", "Oui")),
#'                        c = factor(c("Oui", "Oui", "Non", "Non", "Oui", "Oui")))
#' data |>
#'   score_from_lv1("score", vars_list = c("a", "b", "c")) |>
#'   tab(group, score, digits = 1)
score_from_lv1 <- function (data, name, vars_list) {
  name <- rlang::ensym(name)
  data <- data |> dplyr::mutate(!!rlang::sym(name) := 0L)

  purrr::reduce(
    vars_list,
    .init = dplyr::mutate(data, dplyr::across(
      tidyselect::all_of(vars_list), ~ forcats::fct_na_value_to_level(., "NA"))),

    .f = ~ dplyr::mutate(.x, !!name := dplyr::if_else(
      condition = !!rlang::sym(.y) == levels(as.factor(!!rlang::sym(.y)))[1],
      true  = !!name + 1L,
      false = !!name
    )
    )
  )
}




#Use fct_relabel instead of pers functions ! -----------------------------------
#' Clean factor levels.
#'
#' @param factor A factor.
#' @param pattern A pattern.
#'
#' @return A factor.
#' @keywords internal
# @export
# @examples
fct_clean <- function(factor, pattern = cleannames_condition()) {
  forcats::fct_relabel(factor, ~ stringr::str_remove_all(.x, pattern))
}


# fct_clean <- function(factor, pattern = cleannames_condition()){
#   if(is.data.frame(factor)) {stop("must be a vector, not a data.frame")}
#   if (!is.factor(factor)) { factor <- factor %>%  as.factor() }
#   levels <- factor %>%  levels() %>%
#     magrittr::set_names(purrr::map(., ~stringr::str_remove_all(.,pattern)))
#   return(forcats::fct_recode(factor, !!!levels))
# }
# glm.data %>% dplyr::mutate_if(is.factor, ~ fct_clean(.))
# glm.data %>% dplyr::mutate_at(c(1:6,8), ~ fct_clean(., cleannames_condition()))


#' Replace Factor Levels with NA
#'
#' @param factor A factor.
#' @param patternlist A character vector of levels.
#'
#' @return A factor.
#' @keywords internal
# @export
#
# @examples
# forcats::gss_cat %>%
# dplyr::pull(race) %>%
#   fct_to_na("Other")
fct_to_na <- function(factor, patternlist){
  if (!is.factor(factor)) { factor <- factor %>% as.factor() }
  patternlist <- patternlist %>% magrittr::set_names(rep("NULL", length(.)))
  forcats::fct_recode(factor, !!!patternlist)
}


#' Recode Factor Levels using one Pattern
#' @description Recode factor levels using \code{\link[stringr]{str_replace_all}}.
#' @param factor A factor.
#' @param pattern A character of length 1.
#' @param replacement A character of length 1.
#'
#' @return A factor
#' @keywords internal
# @export
#'
# @examples
fct_replace <- function(factor, pattern, replacement){
  if (is.data.frame(factor)) {stop("must be a vector, not a data.frame")}
  if (!is.factor(factor)) { factor <- factor %>% as.factor() }
  levels <- factor %>% levels() %>%
    magrittr::set_names(purrr::map(., ~ stringr::str_replace_all(., pattern, replacement)))
  return(forcats::fct_recode(factor, !!!levels))
}



#' Recode Factor Levels using Multiple Patterns
#'
#' @param factor A factor.
#' @param pattern_replacement_named_vector A named character vector, with
#' regular expressions to find in values, replacements in names.
#'
#' @return A factor.
#' @keywords internal
# @export
#'
# @examples
fct_rename <- function (factor, pattern_replacement_named_vector){
  if(is.data.frame(factor)) {stop("must be a vector, not a data.frame")}
  if (!is.factor(factor)) { factor <- factor %>% as.factor() }
  if (!is.null(pattern_replacement_named_vector)) {
    factor <- purrr::reduce2(pattern_replacement_named_vector,
                             names(pattern_replacement_named_vector),
                             .init = factor, .f = ~ fct_replace(..1, ..2, ..3))
  }
  return(factor)
}


#' Recode Factor Levels with Detected Pattern inside
#' @description Recode factor levels using \code{\link[stringr]{str_detect}}.
#' @param factor A factor.
#' @param pattern A character vector of length 1.
#' @param replacement A character vector of length 1.
#' @param negate A factor.
#'
#' @return A factor.
#' @keywords internal
# @export
#'
# @examples
fct_detect_replace <- function(factor, pattern, replacement, negate = FALSE){
  if (is.data.frame(factor)) {stop("must be a vector, not a data.frame")}
  if (!is.factor(factor)) { factor <- factor %>% as.factor() }
  if (negate == FALSE) {
    levels <- factor %>% levels() %>%
      magrittr::set_names(purrr::map(., ~ dplyr::if_else(stringr::str_detect(., pattern), replacement, .) ))
  } else {
    levels <- factor %>% levels() %>%
      magrittr::set_names(purrr::map(., ~ dplyr::if_else(!stringr::str_detect(., pattern), replacement, .) ))
  }
  return(forcats::fct_recode(factor, !!!levels))
}




#' @keywords internal
fct_detect_rename <- function (factor, pattern_replacement_named_vector){
  if(is.data.frame(factor)) {stop("must be a vector, not a data.frame")}
  if (!is.null(pattern_replacement_named_vector)) {
    if (!is.factor(factor)) { factor <- factor %>% as.factor() }
    levels <- factor %>% levels() %>% magrittr::set_names(., .)
    new_levels_list <- purrr::map(levels, function(.lv) purrr::imap(pattern_replacement_named_vector,
                                                                    ~ dplyr::if_else(stringr::str_detect(.lv, .x), .y, .lv) ) %>% purrr::flatten_chr()  )
    new_levels <- purrr::map2(levels, new_levels_list, ~ .y[which(!.y %in% .x)] )
    new_levels <- new_levels %>% purrr::imap(~ ifelse(length(.) == 0, .y, .x))
    if ( any(purrr::map_lgl(new_levels, ~ length(.) >= 2 )) ) {
      warning_levels <- new_levels[which(purrr::map_lgl(new_levels, ~ length(.) >= 2 ))]
      warning(stringr::str_c(c(" two search patterns or more applies to the same level (only the first was kept) : ",
                               rep("", length(warning_levels) - 1)), warning_levels))
      new_levels %>% purrr::map(~ .[1])
    }
    levels <- levels %>% magrittr::set_names(new_levels)
    factor <- factor %>% forcats::fct_recode(!!!levels) %>% forcats::fct_relevel(sort)

  }
  return(factor)
}



#' Recode Factor Levels with Multiple Patterns Detection
#'
#' @param factor A factor.
#' @param pattern_replacement_named_vector A named character vector, with
#' regular expressions to find in values, replacements in names.
#' @param .else A character vector of length 1 to rename factor levels detected
#' with no pattern.
#'
#' @return A factor.
#' @keywords internal
# @export
#'
# @examples
fct_case_when_recode <- function (factor, pattern_replacement_named_vector,
                                  .else = levels(factor) ){
  if(is.data.frame(factor)) {stop("must be a vector, not a data.frame")}
  if (!is.factor(factor)) { factor <- factor %>% as.factor() }
  if (!is.null(pattern_replacement_named_vector)) {
    cases_list <-
      purrr::imap(pattern_replacement_named_vector,
                  ~ list(!! levels(factor) %>% stringr::str_detect(.x) ~ .y)
      ) %>% purrr::flatten() %>% append(!! TRUE ~ .else)

    factor <- factor %>% `levels<-`(dplyr::case_when(!!! cases_list)) %>%
      forcats::fct_recode(NULL = "NULL") %>% forcats::fct_relevel(sort)
  }
  return(factor)
}



#' Copy level of factors between dataframes
#' @description Based on the prefix numbers, otherwise don't work.
#' @param data_to Data with the variable with levels to change.
#' @param data_from Data with the variable with good levels
#' @param var The variable : must exist on both df.
#'
#' @return A factor.
#' @keywords internal
# @export
#'
# @examples
fct_levels_from_vector <- function (data_to, data_from, var) {
  var <- rlang::enquo(var)

  data_to <- data_to
  data_from <- data_from

  if (!is.factor(dplyr::pull(data_from, !!var))) {
    data_from <- data_from %>% dplyr::mutate(!!var := as.factor(!!var))
  }

  if (!all(names(data_from) %in% names(data_to))) {
    levels_recode <- data_from %>% dplyr::pull(!!var) %>% levels()
    detect_strings <- stringr::str_c("^", stringr::str_extract(levels_recode, "^[^-]+"))
    levels_recode <- detect_strings %>% magrittr::set_names(levels_recode)

    data_to <- data_to %>% dplyr::mutate(!!var := fct_detect_rename(!!var, levels_recode))
  }
  return(data_to)
}




#' Compare levels of factors in many df
#'
#' @param data Data to use.
#' @param vars Variables to compare levels.
#'
#' @return A list with results.
#' @keywords internal
# @export
#'
# @examples
compare_levels <-
  function(data, vars = c("var1", "var2")) {
    if ("character" %in% class(data)) {
      db_names <- data
      db <- data %>% purrr::map(~ eval(str2expression(.)) %>%
                                  dplyr::select(tidyselect::any_of(vars)) ) %>%
        magrittr::set_names(data)
    } else if (all(purrr::map_lgl(data, ~ "data.frame" %in% class(.)))) {
      db <- data %>% purrr::map(~ dplyr::select(., tidyselect::any_of(vars)))
      db_names <- names(db)
    }

    non_empty_db <- db %>% purrr::map(~ ncol(.)) != 0
    first_non_empty_db <- which(non_empty_db == TRUE)[1]
    non_empty_non_first_db <- non_empty_db
    non_empty_non_first_db[first_non_empty_db] <- FALSE

    if(all(non_empty_db == FALSE)) {
      stop("No variable was found.")
    }

    db_var_names <- db %>%
      purrr::map_if(non_empty_db,
                    ~ stringr::str_c("$", colnames(.)[1]),
                    .else = ~ "")

    class <- db %>%
      purrr::map_if(non_empty_db, ~ stringr::str_c(" : class = ", class(dplyr::pull(., 1))),
                    .else = ~"")

    same_name <- db %>%
      purrr::map_if(non_empty_non_first_db,
                    ~ stringr::str_c( " ; same name = ", (names(.) %in% names(db[[first_non_empty_db]])) ),
                    .else = ~ "")
    same_name[first_non_empty_db] <- " ; BASIS FOR COMPARISON"

    levelsdb <- db %>%
      purrr::map_if(non_empty_db, ~ dplyr::pull(., 1) %>% as.factor(.) %>% levels,
                    .else = NA_character_) %>%
      magrittr::set_names(stringr::str_c(db_names, db_var_names, class, same_name))
    #print(levelsdb)

    comp_true_false <- levelsdb %>%
      purrr::map_if(non_empty_db, ~ dplyr::if_else(. %in% levelsdb[[first_non_empty_db]],
                                                   "Same      : \"",
                                                   "Different : \""))
    comp_true_false[[first_non_empty_db]] <-comp_true_false[[first_non_empty_db]] %>%
      stringr::str_replace("^Same", "Base")
    #%>%
    #magrittr::set_names(stringr::str_c(names(.), " (compared to ", names(levelsdb)[first_non_empty_db], ")"))

    result <- purrr::map2(comp_true_false, levelsdb,
                          ~ stringr::str_c(.x, .y))
    result[!non_empty_db] <- "No variable with this name"
    return(result)
  }



# Adapt purrr::map_if function to pmap et map2
# (when FALSE the result is the first element of .l, or the content of .else)

#' A generalized map_if
#'
#' @param .l List of lists.
#' @param .p Predicate.
#' @param .f Function if TRUE.
#' @param .else Function if FALSE.
#' @param ... Other parameter to pass to the function.
#'
#' @return A list of same length.
#' @keywords internal
#'
# @examples
pmap_if <- function(.l, .p, .f, ..., .else = NULL) {
  .x <- .l[[1]]
  sel <- probe(.x, .p)

  out <- purrr::list_along(.x)
  out[sel] <- purrr::pmap(purrr::map(.l, ~ .[sel]), .f, ...) # .Call(pmap_impl, environment(), ".l", ".f", "list")
  if (rlang::is_null(.else)) {
    out[!sel] <- .x[!sel]
  }
  else {
    out[!sel] <- purrr::pmap(purrr::map(.l, ~ .[sel]), .else, ...)
  }
  magrittr::set_names(out, names(.x))
}


#' A 2 arguments map_if
#'
#' @param .x,.y Lists.
#' @param .p Predicate.
#' @param .f Function if TRUE.
#' @param .else Function if FALSE.
#' @param ... Other parameter to pass to the function.
#'
#' @return A list of the same length.
#' @keywords internal
#'
# @examples
map2_if <- function(.x, .y, .p, .f, ..., .else = NULL) {
  sel <- probe(.x, .p)

  out <- purrr::list_along(.x)
  out[sel] <- purrr::map2(.x[sel], .y[sel], .f, ...)
  if (rlang::is_null(.else)) {
    out[!sel] <- .x[!sel]
  }
  else {
    out[!sel] <- purrr::map2(.x[sel], .y[sel], .else, ...)
  }
  magrittr::set_names(out, names(.x))
}

# Simplifier l'alias de list2 (dans purrr) :
# ( pour data %>% list_of_maps %>% pmap(~) )
#list2 <- rlang::list2

#purrr internal functions dependencies (CRAN does'nt accept :::)

# purrr:::probe
# GNU GPL-3 Licence https://purrr.tidyverse.org/LICENSE.html
# Thanks to Hadley Wickham and Lionel Henry
#' @keywords internal
probe <- function (.x, .p, ...)
{
  if (rlang::is_logical(.p)) {
    stopifnot(length(.p) == length(.x))
    .p
  }
  else {
    .p <- as_predicate(.p, ..., .mapper = TRUE)
    purrr::map_lgl(.x, .p, ...)
  }
}

#purrr:::as_predicate
# GNU GPL-3 Licence : https://purrr.tidyverse.org/LICENSE.html
# Thanks to Hadley Wickham and Lionel Henry
#' @keywords internal
as_predicate  <- function (.fn, ..., .mapper)
{
  if (.mapper) {
    .fn <- purrr::as_mapper(.fn, ...)
  }
  function(...) { #Simplified, no purrr:::as_predicate_friendly_type_of
    out <- .fn(...)
    if (!rlang::is_bool(out)) {
      msg <- sprintf("Predicate functions must return a single `TRUE` or `FALSE`")
    }
    out
  }
}

#tidyselect:::where
# MIT + Lience : https://tidyselect.r-lib.org/LICENSE.html
# Thanks to Hadley Wickham and Lionel Henry
#' @keywords internal
where <- function (fn)
{
  predicate <- rlang::as_function(fn)
  function(x, ...) {
    out <- predicate(x, ...)
    if (!rlang::is_bool(out)) {
      rlang::abort("`where()` must be used with functions that return `TRUE` or `FALSE`.")
    }
    out
  }
}





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
#'
# @examples
formats_SAS_to_R <- function (path, name_in, name_out, open = TRUE, remove_at_end_of_var = "f",
                              not_if_numeric = TRUE, text_aposthophe = "'", path_out)  {
  f <- stringi::stri_read_raw(path)
  format <- stringi::stri_enc_detect(f)
  format <- format[[1]]$Encoding[1]
  con <- file(path, encoding = format)
  f <- readLines(con)

  f <- f |>
    stringr::str_remove_all("\t") |>
    stringr::str_replace_all(text_aposthophe, stringi::stri_unescape_unicode("\\u2019")) |>
    stringr::str_replace_all("\"", "'") |>
    stringr::str_squish()

  f <- f[stringr::str_detect(f, "^value|=") & !stringr::str_detect(f, "^proc")] # "^value *\\$|="
  f[stringr::str_detect(f, "=")] <- f[stringr::str_detect(f, "=")] |>
    stringr::str_replace("^([^ ]+) ", "'\\1' ") |>
    stringr::str_replace("''", "'") |>
    stringr::str_replace("' += +'", "'='") |>
    stringr::str_replace("'([^']+)'='([^']+)'", "'\\1-\\2' = '\\1',") |>
    stringr::str_remove(";$")
  f_var <- stringr::str_extract(f[stringr::str_detect(f, "^value")],  "[^ ]+$")
  if (!is.null(remove_at_end_of_var)) f_var <- stringr::str_remove(f_var, paste0(remove_at_end_of_var, "$"))


  if (not_if_numeric) {
    f[stringr::str_detect(f, "^value")] <-
      paste0("if('",
             f_var, "' %in% names(", name_out, ") & !is.numeric(",
             name_out, "$", f_var, ")) {\n", name_out, "$", f_var,
             " <- forcats::fct_recode(", name_in, "$", f_var,
             ",")
  } else {
    f[stringr::str_detect(f, "^value")] <-
      paste0("if('",
             f_var, "' %in% names(", name_out, ")) {\n", name_out,
             "$", f_var, " <- forcats::fct_recode(as.factor(",
             name_in, "$", f_var, "),")
  }
  data <- dplyr::tibble(f = f) |>  # ???
    dplyr::mutate(group = cumsum(as.integer(stringr::str_detect(f, "^if\\(")))) |>
    dplyr::group_by(.data$group) |>
    dplyr::mutate(f = dplyr::if_else(dplyr::row_number() == dplyr::n(),
                                     paste0(f, ")\n}\n"),
                                     f
    )
    ) |>
    dplyr::ungroup()

  data <- data |>
    dplyr::mutate(var = dplyr::if_else(stringr::str_detect(f, "^if\\("),
                         true  = stringr::str_extract(f, "^if\\('[^']+'") |>
                           stringr::str_remove("if\\(") |> stringr::str_remove_all("'"),
                         false = NA_character_                         )
    ) |>
    tidyr::fill(tidyselect::all_of(c("var")))

  no_path_out <- missing(path_out)
  if (no_path_out) {
    path_out <- file.path(tempdir(), paste0("formats_R-", name_out, ".R"))
  }

  # if (stringr::str_detect(path, "\\\\|/")) {
  #
  #
  #   path_out <- stringr::str_c(stringr::str_replace_all(path, "/", "\\\\") |>
  #                                stringr::str_remove("[^\\\\]+$"),
  #                              "formats_R-", name_out, ".R")
  # } else {
  #   path_out <- stringr::str_c("formats_R-", name_out, ".R")
  # }

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



# formats_SAS_to_R <- function (path, name_in, name_out, open = TRUE, remove_final_f = TRUE,
#                               not_if_numeric = TRUE) {
#   f <- stringi::stri_read_raw(path)
#   format <- stringi::stri_enc_detect(f)
#   format <- format[[1]]$Encoding[1]
#
#   con <- file(path, encoding = format)
#
#   f <- readLines(con)
#   f <- f |> stringr::str_remove_all("\t") |> stringr::str_replace_all("'", stringi::stri_unescape_unicode("\\u2019")) |>
#     stringr::str_replace_all("\"", "'")
#   f <- f[stringr::str_detect(f, "value *\\$|=") & ! stringr::str_detect(f, "^proc")]
#
#   f[stringr::str_detect(f, "=")] <- f[stringr::str_detect(f, "=")] |> stringr::str_squish() |>
#     stringr::str_replace("' += +'", "'='") |>
#     stringr::str_replace("'([^']+)'='([^']+)'", "'\\1-\\2' = '\\1',")
#
#   f_var <-  f[stringr::str_detect(f, "value *\\$")] |> stringr::str_extract("[^ ]+$")
#   if (remove_final_f) f_var <- f_var |> stringr::str_remove("f$")
#
#   if (not_if_numeric) {
#     f[stringr::str_detect(f, "value *\\$")] <-
#       paste0("if('", f_var, "' %in% names(", name_out,
#              ") & !is.numeric(", name_out, "$", f_var, ")) {\n",
#              name_out, "$", f_var, " <- forcats::fct_recode(", name_in, "$", f_var, ","
#       )
#   } else {
#     f[stringr::str_detect(f, "value *\\$")] <-
#       paste0("if('", f_var, "' %in% names(", name_out, ")) {\n",
#              name_out, "$", f_var, " <- forcats::fct_recode(as.factor(", name_in, "$", f_var, "),"
#       )
#   }
#   data <-
#     dplyr::tibble(f = f) |>
#     dplyr::mutate(group = stringr::str_detect(f, "^if\\(") |> as.integer() |> cumsum()) |>
#     dplyr::group_by(.data$group) |>
#     dplyr::mutate(f = dplyr::if_else(dplyr::row_number() == dplyr::n(), paste0(f, ")\n}\n"), f)) |>
#     dplyr::ungroup()
#
#   if (stringr::str_detect(path, "\\\\|/")) {
#     path_out <- stringr::str_c(stringr::str_replace_all(path,
#                                                         "/", "\\\\") %>% stringr::str_remove("[^\\\\]+$"),
#                                "formats_R-", name_out, ".R")
#   } else {
#     path_out <- stringr::str_c("formats_R-", name_out, ".R")
#   }
#
#   writeLines(data$f, path_out, useBytes = TRUE)
#
#   if(open) {
#     file.show(path_out)
#   } else {
#     message(path_out)
#   }
#
#   invisible(path_out)
# }



#' Prepare fct_recode
#'
#' @param df_in The name of the unformatted database
#' @param df_out The name of the database to be formatted.
#' @param var The name of the variable.
#' @param mode "text", "numbers" or "numbers_vector"
#' @param numbers If mode = "numbers", a character vector of length 1 with numbers.
#' @param text The character vector of length 1 with text.
#'
#' @return Code to be copied in console.
#' @keywords internal
#'
# @examples
prepare_fct_recode <- function(df_in, df_out, var,  mode = c("text", "numbers",
                                                             "numbers_vector"),
                               numbers, text){
  text <- text
  lines <- stringr::str_c(text, "\n") %>%
    stringr::str_extract_all(".*\n") %>% unlist
  lines <- lines %>% stringr::str_replace_all("\n", "") %>%
    stringr::str_replace_all("\\t+", " ") %>%
    stringr::str_replace_all("^ +", "") %>%
    stringr::str_replace_all(" +$", "")

  if (mode == "normal") {
    lines <- tibble::enframe(lines, name = "number", value = "name") %>%
      dplyr::mutate(number = as.character(.data$number))

  } else if (mode == "numbers") {
    number <- lines %>% stringr::str_match("^\\d*\\w*") %>% tibble::as_tibble()
    name <- lines %>% stringr::str_split("^\\d*[^\\s]*", n = 2, simplify = TRUE) %>%
      tibble::as_tibble() %>% dplyr::select("V2") %>%
      dplyr::mutate(V2 = stringr::str_replace_all(.data$V2, "^ *", ""))
    lines <- dplyr::bind_cols(number, name) %>%
      dplyr::rename(number = .data$V1, name = .data$V2)

  } else if (mode == "numbers_vector") {
    numb <- numbers
    numb <- stringr::str_c(numbers, "\n") %>%
      stringr::str_extract_all(".*\n") %>% unlist
    numb <- numb %>% stringr::str_replace_all("\n", "") %>%
      stringr::str_replace_all("\\t+", " ") %>%
      stringr::str_replace_all("^ +", "") %>%
      stringr::str_replace_all(" +$", "")
    numb <- tibble::enframe(numb, name = "shit", value = "number") %>%
      dplyr::select(number)

    lines <- tibble::enframe(lines, name = "number", value = "name") %>%
      dplyr::select(name)

    lines <- dplyr::bind_cols(numb, lines)
  }

  lines <- lines %>% dplyr::filter(!stringr::str_detect(.data$name,"^\\s*$")) %>%
    dplyr::mutate(first_letter = stringr::str_to_upper(stringr::str_sub(.data$name,
                                                                        1, 1)),
                  other_letters = stringr::str_sub(.data$name, 2, -1) ) %>%
    dplyr::mutate(name = stringr::str_c(.data$first_letter, .data$other_letters)) %>%
    dplyr::select(-"first_letter", -"other_letters") %>%
    dplyr::mutate(mod_line = stringr::str_c("\"", .data$number,"-", .data$name,"\" = \"",
                                            .data$number,  "\",\n"))
  first_line <-
    tibble::tibble(number = "0",
                   mod_line = stringr::str_c(df_out, "$", .data$var,
                                             " <- forcats::fct_recode(", df_in, "$",
                                             .data$var, ",\n") )
  last_line <- tibble::tibble(number = "0", mod_line = ")")
  res <- dplyr::bind_rows(first_line, lines, last_line) %>%
    dplyr::select("mod_line") %>% dplyr::pull()
  cat(res, "\n\n")
  return(invisible(res))
}










# databases <- emploi_data_list[!emploi_data_names %in% c("ee1969_74", "ee2013_18")]
# vars <- c("ANNEE", "SO", "CSE") #c("ANNEE", "SO", "EXTRI")

#' Bind dataframes for tab / tab_many
#'
#' @param data Dataframes to be bound by rows.
#' @param vars Selected variables.
#'
#' @return A tibble.
# @export
#' @keywords internal
# @examples
bind_datas_for_tab <- function(data, vars) {
  if ("character" %in% class(data)) {
    data <- data
    vars <- as.character(vars)
    data <- data %>% purrr::map(~ eval(str2expression(.))) %>%
      purrr::map(~ dplyr::select(., tidyselect::all_of(vars)))
  } else if (all(purrr::map_lgl(data, ~ "data.frame" %in% class(.)))) {
    data <- data %>% purrr::map(~ dplyr::select(., tidyselect::all_of(vars)))
  } else {stop("entry is not character vector or list of data.frames")}
  vars_factors <- #TRUE = Variable is a factor in at least one database.
    vars[purrr::map_lgl(vars, function (.vars)
      any(purrr::map_lgl(data, ~ "factor" %in% class(dplyr::pull(., .vars)))))]
  data <- data %>% purrr::map(~ dplyr::mutate_at(., vars_factors, ~ as.factor(.)))
  levels_of_all_factors <- purrr::map(vars_factors, function(.vars)
    purrr::map(data, ~ dplyr::pull(., .vars) ) %>% forcats::lvls_union()   )
  data <- data %>% purrr::map(function(.db)
    purrr::reduce2(vars_factors, levels_of_all_factors,
                   .init = .db,
                   .f = function(.result, .vars, .levels)
                     dplyr::mutate_at(.result, .vars, ~ forcats::fct_expand(., .levels))
    ) ) %>%
    dplyr::bind_rows() %>%
    dplyr::mutate_if(is.factor, ~ forcats::fct_relevel(., sort) )
  return(data)
}


# Escaped characters ----
#' @keywords internal
unbrk      <- stringi::stri_unescape_unicode("\\u202f") # unbreakable space
sigma_sign <- stringi::stri_unescape_unicode("\\u03c3") # sigma for sd
mult_sign  <- stringi::stri_unescape_unicode("\\u00d7")
cross      <- stringi::stri_unescape_unicode("\\u00d7")

# # Not working
# # Css link towards https://github.com/web-fonts/dejavu-sans-condensed
# # @export
# css_deja_vu_sans_condensed <- function() {
#
#   # "@font-face {
#   #   font-family: 'DejaVu Sans Condensed';
#   #     url('../inst/fonts/dejavu-sans-condensed-webfont.woff') format('woff'),
#   #     url('../inst/fonts/dejavu-sans-condensed-webfont.ttf') format('truetype'),
#   # }" |>
#   #   stringr::str_remove("\n")
#
#   #"@font-face{font-family:'DejaVu Sans Condensed';src:url(https://github.com/web-fonts/dejavu-sans-condensed/fonts/dejavu-sans-condensed-webfont.eot);src:url(https://github.com/web-fonts/dejavu-sans-condensed/fonts/dejavu-sans-condensed-webfont.eot?#iefix) format('embedded-opentype'),url(https://github.com/web-fonts/dejavu-sans-condensed/fonts/dejavu-sans-condensed-webfont.woff2) format('woff2'),url(https://github.com/web-fonts/dejavu-sans-condensed/fonts/dejavu-sans-condensed-webfont.woff) format('woff'),url(https://github.com/web-fonts/dejavu-sans-condensed/fonts/dejavu-sans-condensed-webfont.ttf) format('truetype'),url(https://github.com/web-fonts/dejavu-sans-condensed/fonts/dejavu-sans-condensed-webfont.svg#dejavu_sans_condensedregular) format('svg')}"
#
#   "@font-face {
#    font-family: 'DejaVu Sans Condensed';
#     src: url('https://github.com/web-fonts/dejavu-sans-condensed/fonts/dejavu-sans-condensed-webfont.eot'); /* IE9 Compat Modes */
#       src: url('https://github.com/web-fonts/dejavu-sans-condensed/fonts/dejavu-sans-condensed-webfont.eot?#iefix') format('embedded-opentype'), /* IE6-IE8 */
#       url('https://github.com/web-fonts/dejavu-sans-condensed/fonts/dejavu-sans-condensed-webfont.woff2') format('woff2'), /* Super Modern Browsers */
#       url('https://github.com/web-fonts/dejavu-sans-condensed/fonts/dejavu-sans-condensed-webfont.woff') format('woff'), /* Pretty Modern Browsers */
#       url('https://github.com/web-fonts/dejavu-sans-condensed/fonts/dejavu-sans-condensed-webfont.ttf') format('truetype'), /* Safari, Android, iOS */
#       url('https://github.com/web-fonts/dejavu-sans-condensed/fonts/dejavu-sans-condensed-webfont.svg#dejavu_sans_condensedregular') format('svg'); /* Legacy iOS */
#   }"
#
#   }





# ggpubr functions (for tab_plot() as tableGrob ) ----

# ggpubr:::is_tablegrob
#' @keywords internal
is_tablegrob <- function (tab) {
  inherits(tab, "gtable") & inherits(tab, "grob")
}

# ggpubr:::is_ggtexttable
#' @keywords internal
is_ggtexttable <- function (tab) {
  !is.null(attr(tab, "ggtexttableGrob"))
}

# ggpubr:::as_ggtexttable
#' @keywords internal
as_ggtexttable <- function (tabgrob) {
  res <- ggpubr::as_ggplot(tabgrob)
  attr(res, "ggtexttableGrob") <- tabgrob
  res
}

# ggpubr:::get_tablegrob
#' @keywords internal
get_tablegrob <- function (tab)
{
  if (is_ggtexttable(tab)) {
    tabgrob <- attr(tab, "ggtexttableGrob")
  }
  else if (is_tablegrob(tab)) {
    tabgrob <- tab
  }
  else {
    stop("tab should be an object from either ggpubr::ggtexttable() or gridExtra::tableGrob().")
  }
  tabgrob
}

# ggpubr:::tab_return_same_class_as_input
#' @keywords internal
tab_return_same_class_as_input <- function (tabgrob, input) {
  if (is_ggtexttable(input)) {
    return(as_ggtexttable(tabgrob))
  }
  else if (is_tablegrob(input)) {
    return(tabgrob)
  }
  tabgrob
}

### https://stackoverflow.com/questions/32106333/align-grob-at-fixed-top-center-position-regardless-of-size
justify_grob <- function(grob, hjust = "left", vjust = "top", pad = 5){
  w <- sum(grob$widths)
  h <- sum(grob$heights)
  xy <- list(x = switch(hjust,
                        center = 0.5 + grid::unit(pad, "points"),
                        left = 0.5*w + grid::unit(pad, "points"),
                        right = grid::unit(1,"npc") - 0.5*w - grid::unit(pad, "points")),
             y = switch(vjust,
                        center = 0.5 + grid::unit(pad, "points"),
                        bottom = 0.5*h + grid::unit(pad, "points"),
                        top = grid::unit(1,"npc") - 0.5*h - grid::unit(pad, "points") ) )
  if (is.null(grob$vp)) {
    grob$vp <- grid::viewport(x = xy[[1]], y = xy[[2]] )
  } else {
    grob$vp$x <- xy[[1]]
    grob$vp$y <- xy[[2]]
  }

  return(grob)
}




# # cowplot:::as_grob.ggplot
# as_grob.ggplot <- function (plot, device = NULL) {
#   if (is.null(device)) {
#     device <- null_dev_env$current
#   }
#   cur_dev <- grDevices::dev.cur()
#   device(width = 6, height = 6)
#   null_dev <- grDevices::dev.cur()
#   on.exit({
#     grDevices::dev.off(null_dev)
#     if (cur_dev > 1) grDevices::dev.set(cur_dev)
#   })
#   ggplot2::ggplotGrob(plot)
# }



# translation functions ----

#' @keywords internal
tr_ <- function(...) {
  enc2utf8(gettext(paste0(...), domain = "R-tabxplor"))
}

#' @keywords internal
po_to_dt <- function(file) {
  po_base <- readLines(file, encoding = "UTF-8")
  po_meta <- po_base[!dplyr::cumany(po_base == "")]

  po <- tibble::tibble(base = po_base[dplyr::cumany(po_base == "")])


  po <- po |>
    dplyr::filter(.data$base != "") |>
    dplyr::mutate(
      ok = stringr::str_detect(.data$base, "#:|msgid|msgstr"),
      ok = cumsum(as.integer(.data$ok))
    ) |>
    dplyr::group_by(!!rlang::sym("ok")) |>
    dplyr::group_split() |>
    purrr::map(
      ~ paste0(.$base, collapse = "") |>
        stringr::str_remove_all("\"")
    ) |>
    purrr::flatten_chr()

  po <- tibble::tibble(text = po) |>
    dplyr::mutate(
      type  = stringr::str_extract(.data$text, "^[^ ]+ ") |> stringr::str_trim(),
      group = cumsum(as.integer(.data$type == "#:")),
      .before = 1
    ) |>
    dplyr::mutate(
      text = stringr::str_remove(.data$text, "^[^ ]+ "),
    ) |>
    tidyr::pivot_wider(id_cols  = "group", names_from = "type", values_from = "text") |>
    dplyr::select(-"group") |>
    `attr<-`("meta", po_meta)

  return(po)
}





