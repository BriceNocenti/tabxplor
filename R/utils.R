
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
NULL

# Rlang .data to bind data masking variable in dplyr
#' @keywords internal
#' @importFrom rlang .data
NULL

#Fonctions and options to work with factors and lists -------------

#' A regex pattern to clean the names of factors.
#' @keywords internal
# @export
cleannames_condition <- function()
  "^[^- ]+-(?![[:lower:]])|^[^- ]+(?<![[:lower:]])-| *\\(.+\\)"

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

#' A generalised map_if
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
#' @param path The path of the file to be created
#' @param name_in The name of the unformatted database
#' @param name_out The name of the database to be formatted.
#'
#' @return A file with code.
#' @keywords internal
#'
# @examples
formats_SAS_to_R <- function(path, name_in, name_out) {
  f <- stringi::stri_read_raw(path)
  format <- stringi::stri_enc_detect(f)
  format <- format[[1]]$Encoding[1]
  f <- f %>% stringr::str_conv(format)

  f <- f %>% stringr::str_replace_all(c("(\\$[[^0-9 ]]+\\d+)l"="\\1")) %>%
    stringr::str_replace_all("\\\"\\\"", "\\\"") %>%
    stringr::str_replace_all("'", "'") %>%
    stringr::str_replace_all(stringr::coll("\u20AC"), "euros") %>%
    stringr::str_remove_all(" +\n") %>%
    stringr::str_replace_all("(?<=\\=) +\\\"", "\"") %>%
    stringr::str_remove_all("(?<=\") +(?=\\=)") %>%
    stringr::str_replace_all(" +(\\\"\\=)", "\\1") %>%
    stringr::str_replace_all("(?<=\") +([^ ]+\\\"\\=)", "\"\\1") %>%
    stringr::str_replace_all("\n +\\\"", "\n\\\"") %>%
    stringr::str_remove_all("(?<= )\\\"|\\\"(?= )")

  f <- f %>%
    stringr::str_replace_all(c("VALUE"="value", "Value"="value")) %>%
    stringr::str_replace_all("(\"[^;]+);[?= ]", "\\1,") %>%
    stringr::str_replace_all("(\"[^;]+);[?=\\w]", "\\1,") %>%
    stringr::str_replace_all(",alue ", "; value ") %>%
    stringr::str_replace_all("(/)(\\*).+(\\*)(/)", "") %>%
    stringr::str_replace_all(c("\n"=" ", "\t"="")) %>%
    stringr::str_replace_all("\\$ ", "$") %>%
    stringr::str_replace_all(c("^ $"="", "^$"="", "\n \n"="", "\n  \n"="", "\n\n"="")) %>%
    stringr::str_extract_all("(value [^;]+;)", "\\1")

  f <- f %>%
    #CT2013: stringr::str_replace_all("(\\\"[^=]+)=(\\\"[^\"]+\")", "\\1-\\2=\\1") %>%
    stringr::str_replace_all("\\\"(.+)\\\"=\\\"(.+)\\\"", "\\\"\\1-\\2\\\"=\\\"\\1\\\"") %>%
    #CT2013: stringr::str_replace_all(f, "(\\\"[^=]+=\\\"[^\"]+\")", "\\1,\n") %>%
    stringr::str_replace_all("(\\\"[^=]+=\\\"[^\"]+\")", "\\1,") %>%
    stringr::str_replace_all("(\\$.+)f", "\\1")

  f <- f %>%
    stringr::str_replace_all(";", ") }") %>%
    stringr::str_replace_all(",\n\\)", ")") %>%
    stringr::str_replace_all(",\n.+\\)", ")\n") %>%
    stringr::str_replace_all("value ([[a-zA-Z0-9_\\$]]+)",
                             stringr::str_c("\n",
                                            "if(\"\\1\" %in% names(", name_out,
                                            ") & !is.numeric(", name_out,"\\1", ") ) {\n",
                                            name_out,"\\1"," <- forcats::fct_recode(",
                                            name_in,"\\1",",\n")
    ) %>%
    stringr::str_replace_all("if\\(\"\\$", "if(\"")

  f <- f %>%
    stringr::str_replace_all(",[^<]*<-", " <-") %>%
    stringr::str_replace_all("'", "'") #%>%
  #stringr::str_conv("UTF-8")

  if(stringr::str_detect(path, "\\\\|/")) {
    path_out <- stringr::str_c(stringr::str_replace_all(path, "/", "\\\\") %>% stringr::str_remove("[^\\\\]+$"),
                               "formats_R-", name_out, ".R")
  } else {
    path_out <- stringr::str_c("formats_R-", name_out, ".R")
  }


  file.create(path_out)
  con <- file(path_out, open = "wt", encoding = "UTF-8")
  sink(con)
  cat(f)
  sink()
  close(con)
  # file.show(path_out) # Ouvrir le resultat.

  return(path_out)
}



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
      tibble::as_tibble() %>% dplyr::select(.data$V2) %>%
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
    dplyr::select(-.data$first_letter, -.data$other_letters) %>%
    dplyr::mutate(mod_line = stringr::str_c("\"", .data$number,"-", .data$name,"\" = \"",
                                            .data$number,  "\",\n"))
  first_line <-
    tibble::tibble(number = "0",
                   mod_line = stringr::str_c(df_out, "$", var,
                                             " <- forcats::fct_recode(", df_in, "$",
                                             var, ",\n") )
  last_line <- tibble::tibble(number = "0", mod_line = ")")
  res <- dplyr::bind_rows(first_line, lines, last_line) %>%
    dplyr::select(.data$mod_line) %>% dplyr::pull()
  cat(res, "\n\n")
  return(invisible(res))
}










# databases <- emploi_data_list[!emploi_data_names %in% c("ee1969_74", "ee2013_18")]
# vars <- c("ANNEE", "SO", "CSE") #c("ANNEE", "SO", "EXTRI")

#' Bind dataframes for tab / tab_many
#'
#' @param data Dataframes to be binded by rows.
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


