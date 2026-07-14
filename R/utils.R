# PURPOSE: Package initialization (.onLoad), pipe re-export, factor/list utilities.
# ROLE: Sets all tabxplor.* options defaults. Entry point for package configuration.
# KEY CONSTRAINTS:
#   - .onLoad() is the single source of truth for all option defaults.
#   - Changing a default here affects every user on package load.
#   - set_color_style() and set_color_breaks() are defined in tab_classes.R but called here.

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





#' @keywords internal
.onLoad <- function(libname, pkgname) {
  # options "tabxplor.color_style_type" and "tabxplor.color_style_theme" :

  # HISTORY: these OMP / data.table thread caps are commented out on purpose. data.table's
  # multithreading once triggered a CRAN thread-count flag that blocked tabxplor's acceptance,
  # until it was shown to originate in data.table itself. Left disabled; re-enable only if
  # CRAN flags threads again.
  # # CRAN OMP THREAD LIMIT
  # if (Sys.info()[['sysname']] == "Linux") {
  #  Sys.setenv("OMP_THREAD_LIMIT" = 2)
  # }

  # data.table::setDTthreads(threads = 2)
  # data.table::getDTthreads(verbose = getOption("datatable.verbose"))


  set_color_style()

  # option "tabxplor.color_breaks" : canonical Phase-5 scales (see set_color_breaks()).
  # Equivalent to the former pct_breaks = c(0.05, 0.1, 0.2, 2, 0.3), mean_breaks =
  # c(1.15, 1.5, 2, 4), contrib_breaks = c(1, 2, 5, 10); the x2 rule is now the pct_ratio scale.
  set_color_breaks(list(
    pct_diff   = c(0.05, 0.1, 0.2, 0.3),
    pct_ratio  = c(2),
    mean_diff  = NULL,
    mean_ratio = c(1.15, 1.5, 2, 4),
    contrib    = c(1, 2, 5, 10)
  ))

  options("tabxplor.print" = "console") # options("tabxplor.print" = "kable")

  options("tabxplor.kable_html_font" =
            '"DejaVu Sans", "Arial", arial, helvetica, sans-serif') # Condensed ?

  options("tabxplor.output_kable" = FALSE)

  options("tabxplor.cleannames" = FALSE)

  options("tabxplor.export_dir" = NULL)

  options("tabxplor.kable_popover" = FALSE)

  options("tabxplor.ci_print" = "ci") # or "moe"

  # Phase 3a significance stars (universal CI-inclusion). `stars` default (OPT-IN: FALSE, so a plain
  # tab() stores no per-cell pvalue and shows no stars; tab_reg() sets stars = TRUE itself), and the
  # star thresholds/labels read by get_stars(). Thresholds are nested p-value cutoffs.
  options("tabxplor.stars"         = FALSE)
  options("tabxplor.signif_levels" = c(0.10, 0.05, 0.01))
  options("tabxplor.signif_labels" = c("*", "**", "***"))

  # Weighted inference (§14): unweighted n by default; opt in to Kish n_eff = (Sum w)^2/Sum w^2
  # for the numeric (mean) CIs/tests. Factor-side Kish is deferred (open item).
  options("tabxplor.kish_neff"     = FALSE)

  # Phase 3b: which one-way ANOVA F is DISPLAYED for mean columns ("welch" = robust default,
  # matching oneway.test(var.equal=FALSE); "classic" = pooled-variance F). Both are always
  # stored in the `test` attribute; this only picks the p-value shown in the p-value row/stars.
  options("tabxplor.anova"         = "welch")

  # Phase 6: the `tabxplor.compact` option is dropped, superseded by the `output_list`
  # argument of tab() (default FALSE = merge; TRUE = list). tab_many()'s deprecated `compact`
  # argument still works (mapped onto the output shape).

  # options("tabxplor.pvalue_lines" = FALSE)

  options("tabxplor.always_add_css_in_tab_kable" = TRUE)

  # Phase 10e: tab_kable() render engine. "kableExtra" (default) or "html" (dependency-free,
  # self-contained inline-CSS <table>; faster; used by the jamovi live display). See R/tab-render-html.R.
  options("tabxplor.tab_kable_engine" = "kableExtra")

  # Phase 8: opt-in parallel build of many tables in ONE tab() call (Suggests-only {mirai}).
  # FALSE = off (default); TRUE = auto workers; an integer = that many daemons. `parallel_min` is
  # the smallest row_var count worth dispatching (fewer -> serial: setup would outweigh the gain).
  # See R/tab-parallel.R + dev/tabxplor_1.4.0_decisions.md 26.
  options("tabxplor.parallel"     = FALSE)
  options("tabxplor.parallel_min" = 2L)

  invisible()
}

# Phase 8: release the persistent mirai daemon pool when tabxplor is unloaded (a CRAN cleanliness
# backstop; tab_parallel_stop() lets users do it earlier). No pool is ever warmed at load.
#' @keywords internal
.onUnload <- function(libpath) {
  tab_parallel_stop()
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

  data <- data |> dplyr::select(-tidyselect::any_of(as.character(name)))

  new_data <- data |> dplyr::mutate(!!rlang::sym(name) := 0L)

  new_data <-
  purrr::reduce(
    vars_list,
    .init = dplyr::mutate(new_data, dplyr::across(
      tidyselect::all_of(vars_list), ~ forcats::fct_na_value_to_level(., "NA"))),

    .f = ~ dplyr::mutate(.x, !!name := dplyr::if_else(
      condition = !!rlang::sym(.y) == levels(as.factor(!!rlang::sym(.y)))[1],
      true  = !!name + 1L,
      false = !!name
    )
    )
  )

  var_final_ <- dplyr::pull(new_data, as.character(name))
    #dplyr::select(new_data, tidyselect::all_of(as.character(name)))

  data |> tibble::add_column(!!rlang::sym(name) := var_final_)
}


# data <- dplyr::select(forcats::gss_cat, -where(is.numeric))
# name_in = "data"
# name_out = "data"
# style = "base"
# reminder = TRUE
# cat = TRUE




#' fct_recode helper to recode multiple variables
#'
#' @param data The data frame.
#' @param .cols <\link[tidyr:tidyr_tidy_select]{tidy-select}> The variables to recode.
#' @param name_in The name of the input data frame. Default to the expression given in `data`.
#' @param name_out The name of the output data frame, if different from the
#' input data frame.
#' @param style Default is to use `dplyr::mutate()`. Set to `base` to use `data$var <-` style.
#' @param reminder By default, a reminder of the syntax (`"new" = "old"`) is printed.
#'  Set to `FALSE` to remove it.
#' @param freq Set to `TRUE` to print frequency and count of each level as comment.
#' Set to `FALSE` to avoid this behavior. By default, frequencies and counts are 
#' only calculated when less than 6 variables are provided.
#' @param cat By default the result is written in the console if there are less than
#' 6 variables, written in a temporary file and opened otherwise. Set to
#' false to get a data frame with a character variable instead.
#'
#' @return When the number of variables is less than 5, a text in console as a side effect.
#' With more than 5 variables, a temporary R file. A `tibble` with the recode text as a
#' character variable is returned invisibly (or as main result if `cat = TRUE`).
#' If the `labelled` package in installed, the variable label is used as title in a comment.
#' @export
fct_recode_helper <- function(data, .cols = -where(is.numeric), name_in, name_out,
                              freq = NULL, 
                              style = c("mutate", "base"), reminder = TRUE, cat = TRUE) {
  no_name_in <- missing(name_in)
  if (no_name_in) {
    name_in <- deparse(substitute(data))
    if (stringr::str_detect(name_in, "\\(")) {
      name_in <-
        stringr::str_extract(name_in, "[^\\(]+$") |>
        stringr::str_remove_all("\\).*$")
      # name_in <- "data"
    }
  }
  if (missing(name_out)) name_out <- name_in # if (missing(name_in)) {"data"} else {name_in}

  pos_cols <- tidyselect::eval_select(rlang::enquo(.cols), data)
  data <- data[pos_cols]

  with_variable_label_as_title <- requireNamespace("labelled", quietly = TRUE)
  if (with_variable_label_as_title) {
    var_labs <- labelled::get_variable_labels(data)
    var_labs <- var_labs[purrr::map_lgl(var_labs, ~ !is.null(.))]
    if (length(var_labs) == 0) with_variable_label_as_title <- FALSE

    # var_labs <- purrr::imap(var_labs, ~ paste0(.y, " with a lot of text"))
  }

  data <- data |> dplyr::mutate(dplyr::across(.cols = dplyr::everything(), .fns = as.factor))
  
  # By default, if not chosen by user, only calculate frequencies for less that 10 vars
  if (is.null(freq)) {
    freq <- ncol(data) <= 5
  }
  
  if (freq) { # With frequencies and counts helpers 
    frequencies <- names(data) |> 
      purrr::map(
        ~ tab_plain(data, !!rlang::sym(.x), pct = "col", na = "drop") |> 
          filter(!is_totrow(pct)) |> 
          rename_with(~ "lvs", .cols = 1) |> 
          mutate(lvs = paste0("\"",
                              #stringi::stri_escape_unicode(
                              stringr::str_replace_all(
                                lvs, "\"", "'"
                                #)
                              ),
                              "\""), 
                 pct = format(pct), 
                 n   = format(n), 
                 txt = paste0(str_pad(pct, max(str_length(pct)) ), 
                              " ", 
                              str_pad(n, max(str_length(n)) )
                 )
          ) |> 
          select(lvs, txt)
      ) |> 
      purrr::set_names(names(data)) 
    
    recode <- frequencies |>
      purrr::map(
        ~ paste0(stringr::str_pad(.x$lvs, max(stringr::str_length(.x$lvs)), "right"), " = ",
                 stringr::str_pad(.x$lvs, max(stringr::str_length(.x$lvs)), "right"), 
                 ", # ", 
                 .x$txt
        )
      ) |>
      purrr::map(~ paste0(., collapse = "\n"))
    
  } else { # Without frequencies and counts helpers
    recode <- data |>
      purrr::map(~ paste0("\"",
                          #stringi::stri_escape_unicode(
                          stringr::str_replace_all(
                            levels(.), "\"", "'"
                            #)
                          ),
                          "\"")) |>
      purrr::map(
        ~ paste0(stringr::str_pad(., max(stringr::str_length(.)), "right"), " = ",
                 stringr::str_pad(., max(stringr::str_length(.)), "right"), collapse = ",\n")
      )
    
  }

 
  


  if (with_variable_label_as_title) {
  titles <- purrr::map(
    names(recode),
    ~ dplyr::if_else(. %in% names(var_labs),
                     true  = paste0("# ", ., " : ", var_labs[[.]] , "\n"),
                     false = ""
    )
  )

  } else {
    titles <- purrr::map(recode, ~ "")
  }


  reminder <- if (reminder) {'   # "new" = "old" '} else {''}

  recode <-
    switch(
      style[1],
      "base"   = purrr::pmap(
        list(recode, names(recode), titles),
        ~ paste0(..3,
                 name_out, "$", ..2, " <- fct_recode(\n",
                 name_in, "$", ..2, ',', reminder, '\n',
                 ..1, "\n)\n\n"
        )) |>
        purrr::flatten_chr(),

      "mutate" =
        c(
          paste0(name_in, " |>\n", "mutate(", "\n"), # reminder
          purrr::pmap(
            list(recode, names(recode), titles),
            ~ paste0(..3,
                     ..2, " = fct_recode(", reminder, "\n",
                     ..2, ',', '\n',
                     ..1, "\n),\n\n"
            )) |> purrr::flatten_chr(),
          ")\n"
        )
    ) |>
    tibble::as_tibble() |>
    rename("recode" = "value")

  if (cat == FALSE) return(recode)

  if (ncol(data) <= 5) {
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
get_user_documents <- function() {

  # 1. Windows: query Known Folder via PowerShell
  if (.Platform$OS.type == "windows") {
    docs <- tryCatch({
      out <- system(
        'powershell -NoProfile -Command "[Environment]::GetFolderPath(\'MyDocuments\')"',
        intern = TRUE
      )
      out1 <- out[1]
      if (nzchar(out1) && dir.exists(out1)) out1 else stop()
    }, error = function(e) NULL)
    if (!is.null(docs))
      return(normalizePath(docs, winslash = "\\", mustWork = FALSE))
  }

  # 2. macOS standard
  if (Sys.info()[["sysname"]] == "Darwin") {
    docs <- file.path(path.expand("~"), "Documents")
    if (dir.exists(docs))
      return(normalizePath(docs, mustWork = FALSE))
  }

  # 3. Linux XDG user dirs
  xdg_conf <- Sys.getenv("XDG_CONFIG_HOME", unset = NA)
  if (is.na(xdg_conf))
    xdg_conf <- file.path(path.expand("~"), ".config")
  user_dirs <- file.path(xdg_conf, "user-dirs.dirs")
  if (file.exists(user_dirs)) {
    lines <- readLines(user_dirs, warn = FALSE)
    line <- grep("^XDG_DOCUMENTS_DIR=", lines, value = TRUE)
    if (length(line)) {
      path_raw <- sub('^XDG_DOCUMENTS_DIR="?', "",
                      sub('"$', "", line))
      # Expand $HOME
      path_raw <- gsub("\\$HOME", path.expand("~"), path_raw)
      if (dir.exists(path_raw))
        return(normalizePath(path_raw, mustWork = FALSE))
    }
  }

  # 4. Fallback to common defaults (only if they exist)
  candidates <- c(
    file.path(path.expand("~"), "Documents"),
    file.path(path.expand("~"), "My Documents")
  )
  for (cnd in candidates) {
    if (dir.exists(cnd))
      return(normalizePath(cnd, mustWork = FALSE))
  }

  # 5. Last resort: home directory
  normalizePath(path.expand("~"), mustWork = FALSE)


  # # fs::path_home() gives home dir on all OS
  # home <- fs::path_home()
  # # Candidate Documents folder
  # docs <- fs::path(home, "Documents")
  # # On Windows, read from registry if localized
  # if (fs::dir_exists(docs))
  #   return(docs)
  #
  # # Try Windows registry query for the personal folder
  # if (.Platform$OS.type == "windows") {
  #   reg <- tryCatch({
  #     utils::readRegistry(
  #       key = "HCU\\Software\\Microsoft\\Windows\\CurrentVersion\\Explorer\\User Shell Folders",
  #       hive = "HCU"
  #     )["Personal"]
  #   }, error = function(e) NULL)
  #   if (!is.null(reg) && nzchar(reg)) {
  #     # Registry value can contain %USERPROFILE% variable
  #     reg <- Sys.getenv("USERPROFILE", unset = reg)
  #     reg <- normalizePath(reg, winslash = "\\", mustWork = FALSE)
  #     return(reg)
  #   }
  # }
  #
  # # Fallback to ~/Documents (even if it doesn’t exist yet)
  # fs::path(home, "Documents")


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
  # Why this exists: purrr has no conditional pmap; apply .f only to elements matching .p
  # (via the vendored probe()), leaving the rest unchanged. map2_if is the 2-arg analogue.
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




# path <- "~/Data/Enquêtes élections/Enquête Participation électorale 2017/Doc/Formats/formats_sas.txt"
# name_in = "pa17"
# name_out = "pa17"
# open = TRUE
# remove_at_end_of_var = "f"
# not_if_numeric = TRUE
# text_aposthophe = "'"
# # path_out
#
# load_all()
# tabxplor:::formats_SAS_to_R("~/Data/Enquêtes élections/Enquête Participation électorale 2017/Doc/Formats/formats_sas.txt",
#                             name_in = "pa17", name_out = "pa17")


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
    stringr::str_replace_all(";value", "value") |>
    stringr::str_squish()

  f <- f[stringr::str_detect(f, "^value|=") & !stringr::str_detect(f, "^proc")] # "^value *\\$|="
  f[stringr::str_detect(f, "=")] <-
    f[stringr::str_detect(f, "=")] |>
    stringr::str_replace("^([^ =]+) ", "'\\1' ") |>
    #stringr::str_replace("^([^ ]+) ", "'\\1' ") |>
    stringr::str_replace("''", "'") |>
    stringr::str_replace("' += +'", "'='") |>
    stringr::str_replace("'([^']+)'='([^']+)'", "'\\1-\\2' = '\\1',") |>
    stringr::str_remove(";$")
  f_var <- stringr::str_extract(f[stringr::str_detect(f, "^value")],  "[^ ]+$")
  if (!is.null(remove_at_end_of_var)) f_var <- stringr::str_remove(f_var, paste0(remove_at_end_of_var, "$"))

  f[1:30]

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
  # Why this exists: before row-binding several data sources, unify each factor's levels
  # across ALL of them (lvls_union + fct_expand + fct_relevel(sort)). Otherwise a factor
  # missing a level in one source would drop that category from the pooled aggregation.
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


# Colors functions ---- 
# ---------------------------------------------------------------------------
# colour_grid_preview.R
#
# Visual review of colour combinations in the Positron Viewer pane.
#
#   preview_color_grid()    - every text x background combination of two vectors
#   preview_luminance_grid() - luminance shades of one text/background pair
#
# Both render exactly like tab_kable(): a knitr::kable() + kableExtra::kable_classic()
# table whose cells are kableExtra::cell_spec() tiles (rounded background via
# background_as_tile, the same background "shape" tab_kable() draws). Each cell
# shows the sample text plus its APCA lightness-contrast value (Lc) of
# text-on-background, so you can eyeball legibility at a glance.
#
# The table backdrop is configurable via `table_bg` (both preview functions).
# When its oklch lightness drops below `dark_threshold`, the table auto-switches
# to dark styling: white + slightly thicker borders, light text for all the
# non-tile chrome (labels, caption, footnote), and a transparent table frame so
# the dark page shows through behind the coloured tiles.
#
# Dependencies: farver (oklch + rgb maths) + knitr + kableExtra (the tab_kable
# engine). Viewer routing reuses print.kableExtra (getOption("viewer") hook),
# the same route tab_kable() output takes - no pandoc needed.
# ---------------------------------------------------------------------------

# tab_kable() default HTML font stack.
.cg_font <- '"DejaVu Sans", "Arial", arial, helvetica, sans-serif'

# --- internal: dependency guard --------------------------------------------

#' Stop early with a clear message if a rendering dependency is missing.
#' @noRd
.cg_require <- function() {
  if (!requireNamespace("farver", quietly = TRUE)) {
    stop("Package 'farver' is required for oklch handling.", call. = FALSE)
  }
  # The grids now render as tab_kable()-style tables (same engine + display).
  for (pkg in c("knitr", "kableExtra")) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop(sprintf("Package '%s' is required to render the colour grid.", pkg),
           call. = FALSE)
    }
  }
}

# --- internal: oklch / gamut maths -----------------------------------------

#' Decode one colour to its oklch (l, c, h) with clean, predictable names.
#'
#' farver may attach column names that get mangled by name-concatenation when
#' extracting single elements, so we strip names and reassign them explicitly.
#' @noRd
.cg_oklch <- function(hex) {
  m <- farver::decode_colour(hex, to = "oklch")
  stats::setNames(as.numeric(m[1, ]), c("l", "c", "h"))
}

#' Is oklch(l, c, h) inside the sRGB gamut?
#'
#' farver caps RGB output to [0, 255], so an out-of-gamut colour never returns
#' impossible RGB values - instead it loses chroma on a round-trip
#' oklch -> rgb -> oklch. We detect that chroma drop.
#' @noRd
.cg_in_gamut <- function(l, c, h, tol = 1e-3) {
  lch  <- matrix(c(l, c, h), ncol = 3)
  rgb  <- farver::convert_colour(lch, from = "oklch", to = "rgb")
  back <- farver::convert_colour(rgb, from = "rgb", to = "oklch")
  abs(back[1, 2] - c) <= tol
}

#' Largest in-gamut chroma for a given lightness/hue.
#'
#' The in-gamut chroma range is a single interval [0, cmax], so a bisection on
#' "is this chroma still in gamut?" converges on cmax.
#' @noRd
.cg_max_chroma <- function(l, h, hi = 0.4, iter = 28L) {
  lo <- 0
  for (i in seq_len(iter)) {
    mid <- (lo + hi) / 2
    if (.cg_in_gamut(l, mid, h)) lo <- mid else hi <- mid
  }
  lo
}

#' Build a hex colour at lightness `l`, keeping hue `h`; chroma set by `mode`.
#'
#' "fixed" keeps the source chroma but caps it to the gamut (so hue is never
#' distorted by RGB clipping); "max" uses the most vivid in-gamut chroma.
#' @noRd
.cg_shade <- function(l, h, base_c, mode) {
  if (base_c < 1e-4) {                    # achromatic source -> stay grey
    cc <- 0
  } else {
    cmax <- .cg_max_chroma(l, h)
    cc <- if (mode == "max") cmax else min(base_c, cmax)
  }
  farver::encode_colour(matrix(c(l, cc, h), ncol = 3), from = "oklch")
}

#' Pick black or white text for legibility over a background hex.
#' @noRd
.cg_readable_on <- function(bg_hex) {
  l <- farver::decode_colour(bg_hex, to = "oklch")[1, 1]
  if (l >= 0.6) "#000000" else "#ffffff"
}

# --- internal: theme (light / dark) ----------------------------------------

#' Resolve a light/dark theme from the table backdrop colour.
#'
#' `dark` triggers when the backdrop's oklch lightness is below `threshold`.
#' Dark mode uses white, slightly thicker borders and light "ink" for all the
#' non-tile chrome; light mode keeps the current lightable-classic appearance.
#' @noRd
.cg_theme <- function(table_bg, threshold = 0.5) {
  l    <- farver::decode_colour(table_bg, to = "oklch")[1, 1]
  dark <- isTRUE(l < threshold)
  list(
    dark          = dark,
    bg            = table_bg,
    ink           = if (dark) "#e8e8e8" else "#222222",  # non-tile text
    border        = if (dark) "#ffffff" else "#d9d9d9",  # frame + row rules
    border_w      = if (dark) "2px"     else "1px",      # a bit more linewidth
    square_border = if (dark) "#cfcfcf" else "#999999"   # row-label swatch square
  )
}

#' CSS injected AFTER the kable (so it wins over lightable's inline <style> at
#' equal specificity). Only border rules use !important; text-colour rules stay
#' non-important so inline cell_spec() tile colours still win.
#' @noRd
.cg_theme_css <- function(t) {
  css <- sprintf("body{background-color:%s;margin:0;padding:16px;}", t$bg)
  if (isTRUE(t$dark)) {
    css <- paste0(
      css,
      # let the dark page show through the frame; tiles keep their own fill
      ".lightable-classic,.lightable-classic thead,.lightable-classic tbody,",
      ".lightable-classic tr,.lightable-classic td,.lightable-classic th,",
      ".lightable-classic caption,.lightable-classic tfoot{",
      "background-color:transparent !important;}",
      # light ink for labels / corner / caption / footnote (tiles override inline)
      sprintf(".lightable-classic,.lightable-classic td,.lightable-classic th{color:%s;}",
              t$ink),
      sprintf(".lightable-classic caption{color:%s;}", t$ink),
      sprintf(".lightable-classic tfoot{color:%s;}", t$ink),
      # white, slightly thicker borders
      sprintf(".lightable-classic td,.lightable-classic th{border-color:%s !important;}",
              t$border),
      sprintf(paste0(".lightable-classic>tbody>tr>td,.lightable-classic>thead>tr>th",
                     "{border-bottom-width:%s !important;border-bottom-color:%s !important;}"),
              t$border_w, t$border)
    )
  }
  css
}

# --- internal: APCA contrast (APCA-W3 0.98G / 0.1.x constants) --------------

#' sRGB (0-255 triplet) to APCA screen luminance Y.
#' @noRd
.cg_srgb_to_y <- function(rgb) {
  lin <- (rgb / 255)^2.4                  # simple 2.4 TRC, per APCA-W3
  0.2126729 * lin[1] + 0.7151522 * lin[2] + 0.0721750 * lin[3]
}

#' APCA lightness contrast (Lc) of text-on-background, signed float.
#'
#' Positive => dark text on light background; negative => the reverse. Verified
#' against Myndex reference vectors (e.g. #888 on #fff -> ~63.06).
#' @noRd
.cg_apca <- function(text_hex, bg_hex) {
  txt <- as.numeric(farver::decode_colour(text_hex, to = "rgb"))
  bg  <- as.numeric(farver::decode_colour(bg_hex,  to = "rgb"))
  txt_y <- .cg_srgb_to_y(txt)
  bg_y  <- .cg_srgb_to_y(bg)

  soft_clamp <- function(y) if (y > 0.022) y else y + (0.022 - y)^1.414
  txt_y <- soft_clamp(txt_y)
  bg_y  <- soft_clamp(bg_y)

  if (abs(bg_y - txt_y) < 0.0005) return(0)

  if (bg_y > txt_y) {                     # BoW: dark text on light background
    sapc <- (bg_y^0.56 - txt_y^0.57) * 1.14
    if (sapc < 0.1) 0 else (sapc - 0.027) * 100
  } else {                                # WoB: light text on dark background
    sapc <- (bg_y^0.65 - txt_y^0.62) * 1.14
    if (sapc > -0.1) 0 else (sapc + 0.027) * 100
  }
}

# --- internal: HTML assembly + Viewer routing ------------------------------

#' Minimal HTML escaping for user-supplied sample text.
#' @noRd
.cg_escape <- function(x) {
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;",  x, fixed = TRUE)
  gsub(">", "&gt;", x, fixed = TRUE)
}

#' Build the swatch grid as a tab_kable()-style table and open the Viewer.
#'
#' Renders exactly like [tabxplor::tab_kable()]: each body cell is a
#' `kableExtra::cell_spec()` tile (rounded background via `background_as_tile`,
#' the same background "shape" tab_kable() draws), coloured `text` on `bg`, laid
#' out by `knitr::kable()` + `kableExtra::kable_classic()` with the tab_kable
#' DejaVu font stack. Column headers are tinted with the backdrop colour (a live
#' preview); row headers carry a small square of the text colour. `caption`
#' becomes the table caption, `subtitle` a footnote. The page/table backdrop is
#' `table_bg`; below `dark_threshold` oklch lightness the table auto-switches to
#' dark styling (white thicker borders + light chrome). Opening in the Viewer
#' reuses the `print.kableExtra` path (same dependencies + `getOption("viewer")`
#' hook tab_kable() uses), so no pandoc is needed.
#'
#' @param text_hex,bg_hex Character matrices [n_row x n_col] of cell colours.
#' @param row_swatch,col_swatch Hex used to tint the row squares / column headers.
#' @param table_bg Backdrop colour of the whole table/page.
#' @param dark_threshold oklch lightness of `table_bg` below which dark styling kicks in.
#' @noRd
.cg_kable_grid <- function(text_hex, bg_hex, row_labels, col_labels,
                           row_swatch, col_swatch, corner, sample_text,
                           show_contrast, swatch_padding, caption, subtitle,
                           font_size, table_bg = "#ffffff", dark_threshold = 0.5,
                           full_width = FALSE, browse = TRUE) {
  n_row <- nrow(text_hex)
  n_col <- ncol(text_hex)
  theme <- .cg_theme(table_bg, dark_threshold)
  sample_html <- .cg_escape(sample_text)
  tile_css <- paste0("display:inline-block;padding:", swatch_padding, ";")

  # body: one kableExtra tile per cell (the same engine + rounded-tile shape as
  # tab_kable()'s coloured-background branch, cell_spec(background = )).
  body <- outer(seq_len(n_row), seq_len(n_col), Vectorize(function(i, j) {
    lc_txt <- if (isTRUE(show_contrast)) {
      lc <- .cg_apca(text_hex[i, j], bg_hex[i, j])
      sprintf(' <span style="font-weight:400;opacity:.75;font-size:90%%;">(%d)</span>',
              as.integer(round(abs(lc))))
    } else ""
    kableExtra::cell_spec(paste0(sample_html, lc_txt), format = "html", escape = FALSE,
                          bold = TRUE, color = text_hex[i, j], background = bg_hex[i, j],
                          extra_css = tile_css)
  }))
body <- matrix(unlist(body), nrow = n_row, ncol = n_col)

  # row header: small text-colour square + label (always visible, even near-white).
  # Square border follows the theme so it stays visible on a dark backdrop too.
  rlab <- vapply(seq_len(n_row), function(i) sprintf(
    paste0('<span style="display:inline-block;width:12px;height:12px;',
           'border:1px solid %s;background-color:%s;vertical-align:middle;',
           'margin-right:6px;"></span>%s'),
    theme$square_border, row_swatch[i], row_labels[i]
  ), character(1))

  # column headers tinted with the backdrop colour (a live preview of the fill)
  col_head <- vapply(seq_len(n_col), function(j) kableExtra::cell_spec(
    col_labels[j], format = "html", escape = FALSE, bold = TRUE,
    color = .cg_readable_on(col_swatch[j]), background = col_swatch[j]
  ), character(1))

  df <- data.frame(rlab, body, check.names = FALSE, stringsAsFactors = FALSE)

  # font_size arrives as a CSS length ("16px"); kable_styling wants a bare number.
  fs <- suppressWarnings(as.numeric(gsub("[^0-9.]", "", as.character(font_size))))
  if (length(fs) != 1L || is.na(fs)) fs <- NULL

  # header underline: theme-driven (white + thicker in dark mode, else as before)
  header_rule <- if (theme$dark) {
    sprintf("border-bottom:%s solid %s;", theme$border_w, theme$border)
  } else {
    "border-bottom:1px solid;"
  }

  out <- knitr::kable(
    df, format = "html", escape = FALSE,
    align = c("l", rep("c", n_col)),
    col.names = c(corner, col_head), caption = caption
  ) |>
    kableExtra::kable_classic(
      lightable_options = "hover", full_width = full_width,
      html_font = .cg_font, font_size = fs
    ) |>
    kableExtra::row_spec(
      0, bold = TRUE,
      extra_css = paste0(header_rule, "vertical-align:bottom;text-align:center;")
    ) |>
    kableExtra::column_spec(1, extra_css = "white-space:nowrap;")

  if (!is.null(subtitle) && any(nzchar(subtitle))) {
    out <- kableExtra::add_footnote(out, subtitle, notation = "none", escape = FALSE)
  }

  # Append the theme CSS AFTER lightable's inline <style> so it wins at equal
  # specificity; restore the kableExtra attributes paste0() strips.
  attrs <- attributes(out)
  out <- paste0(out, "\n<style>", .cg_theme_css(theme), "</style>\n")
  attributes(out) <- attrs

  # Reuse print.kableExtra: opens in the Viewer in interactive sessions (the same
  # route tab_kable() output takes), cat()s the HTML otherwise.
  if (isTRUE(browse)) print(out)
  invisible(out)
}

# max in-gamut oklch chroma for a given L + hue.
# farver clamps rgb to [0,255], so an out-of-gamut chroma shrinks on a
# oklch -> rgb -> oklch round-trip; bisect on that.
#' @noRd
max_chroma <- function(l, h, hi = 0.4, iter = 28L) {
  lo <- 0
  for (i in seq_len(iter)) {
    mid  <- (lo + hi) / 2
    lch  <- matrix(c(l, mid, h), ncol = 3)
    back <- farver::convert_colour(
      farver::convert_colour(lch, "oklch", "rgb"), "rgb", "oklch"
    )
    if (abs(back[1, 2] - mid) <= 1e-3) lo <- mid else hi <- mid
  }
  lo
}

# set luminance (scalar or one-per-colour), keep hue, cap chroma to gamut
#' @noRd
set_luminance <- function(cols, l = 0.95) {
  lch <- farver::decode_colour(cols, to = "oklch")   # cols: l, c, h
  l   <- rep_len(l, nrow(lch))
  h   <- lch[, 3]; h[is.na(h)] <- 0                  # achromatic -> hue 0
  cc  <- vapply(seq_len(nrow(lch)), function(i) {
    if (lch[i, 2] < 1e-4) 0                           # keep greys grey
    else min(lch[i, 2], max_chroma(l[i], h[i]))       # keep chroma, cap to gamut
  }, numeric(1))
  farver::encode_colour(cbind(l, cc, h), from = "oklch") |>
    setNames(names(cols))
}

# set chroma (scalar or one-per-colour), keep hue + luminance, cap to gamut
#' @noRd
set_chroma <- function(cols, c = 0.1) {
  lch <- farver::decode_colour(cols, to = "oklch")   # cols: l, c, h
  c   <- rep_len(c, nrow(lch))
  l   <- lch[, 1]
  h   <- lch[, 3]; h[is.na(h)] <- 0                  # grey has no hue -> 0
  cc  <- vapply(seq_len(nrow(lch)), function(i)
    min(c[i], max_chroma(l[i], h[i])),               # requested chroma, capped
    numeric(1)
  )
  farver::encode_colour(cbind(l, cc, h), from = "oklch") |>
    setNames(names(cols))
}



## --- all text x background combinations -----------------------

#' Preview every text x background colour combination in the Viewer
#'
#' Builds a [tabxplor::tab_kable()]-style table with one row per text colour and
#' one column per background colour (cells are `kableExtra::cell_spec()` tiles,
#' the same rounded-background shape tab_kable() draws), then opens it in the
#' Positron Viewer pane.
#'
#' @param text_colors A (named) character vector of hex text colours. Names
#'   become row labels; the hex value is used when unnamed.
#' @param background_colors A (named) character vector of hex background
#'   colours, used as columns.
#' @param sample_text Text shown in each cell. Default: a random whole-number
#'   percentage (e.g. "27%"), one value per call.
#' @param show_contrast Logical; append the APCA lightness-contrast value (Lc)
#'   of text-on-background to each cell, on the same line as sample_text.
#'   Default TRUE.
#' @param table_bg Backdrop colour of the whole table/page, e.g. "#1a1a1a" to
#'   preview dark mode. Default "#ffffff".
#' @param dark_threshold oklch lightness (0-1) of `table_bg` below which the
#'   table switches to dark styling (white + slightly thicker borders, light
#'   text, transparent frame). Default 0.5.
#' @param font_size CSS font-size for the table. Default "14px".
#' @param swatch_padding CSS padding for each swatch cell. Default "1px 1px".
#' @param browse Logical; open the result in the Viewer. Default TRUE.
#'
#' @return (Invisibly) the generated HTML as a single string.
#' @examples
#' \dontrun{
#' text_colors <- c(plain = "#888888", pos3 = "#0baedb", pos5 = "#265aff")
#' background_colors <- c(plain = "#ffffff", pos3 = "#91b837", pos5 = "#05ae30")
#' preview_color_grid(text_colors, background_colors)
#' preview_color_grid(text_colors, background_colors, table_bg = "#1a1a1a")  # dark
#' }
#' @keywords internal
preview_color_grid <- function(text_colors,
                                background_colors,
                                sample_text = paste0(sample(0:100, 1L), "%"),
                                show_contrast = TRUE,
                                table_bg = "#ffffff",
                                dark_threshold = 0.5,
                                font_size = "14px",
                                swatch_padding = "2px 1px",
                                browse = TRUE) {
  .cg_require()
  stopifnot(length(text_colors) >= 1, length(background_colors) >= 1,
            length(table_bg) == 1)

  row_labels <- names(text_colors)
  if (is.null(row_labels)) row_labels <- unname(text_colors)
  col_labels <- names(background_colors)
  if (is.null(col_labels)) col_labels <- unname(background_colors)

  n_row <- length(text_colors)
  n_col <- length(background_colors)

  # row = text colour (constant across a row); col = background (constant down a column)
  text_hex <- matrix(rep(unname(text_colors), times = n_col), nrow = n_row)
  bg_hex   <- matrix(rep(unname(background_colors), each = n_row), nrow = n_row)

  .cg_kable_grid(
    text_hex, bg_hex,
    row_labels = row_labels, col_labels = col_labels,
    row_swatch = unname(text_colors),
    col_swatch = unname(background_colors),
    corner = "text \u2193 / bg \u2192",
    sample_text = sample_text, show_contrast = show_contrast,
    swatch_padding = swatch_padding,
    caption  = "Text \u00d7 background colour grid",
    subtitle = sprintf("%d text colours \u00d7 %d backgrounds \u2014 cells show APCA Lc",
                       n_row, n_col),
    font_size = font_size, table_bg = table_bg, dark_threshold = dark_threshold,
    browse = browse
  )
}

## luminance shades of one pair -----------------------------

#' Preview luminance shades of one text/background pair in the Viewer
#'
#' For a single text colour and background colour, builds a grid of luminance
#' shades: rows vary the text colour's lightness, columns vary the background's
#' lightness. Every shade keeps its source oklch hue; chroma is either held at
#' the source value (capped to gamut) or pushed to the maximum available at that
#' lightness/hue. Rendered as a [tabxplor::tab_kable()]-style table (rounded
#' `cell_spec()` background tiles) and opened in the Positron Viewer.
#'
#' @param text_color Single hex string for the text colour.
#' @param background_color Single hex string for the background colour.
#' @param l_values Numeric oklch lightness values (0-1) for the shades.
#'   Default seq(0.35, 0.95, length.out = 7).
#' @param chroma "fixed" (keep source chroma, capped to gamut) or "max"
#'   (maximum in-gamut chroma per shade). Default "fixed".
#' @param table_bg Backdrop colour of the whole table/page, e.g. "#1a1a1a" to
#'   preview dark mode. Default "#ffffff".
#' @param dark_threshold oklch lightness (0-1) of `table_bg` below which the
#'   table switches to dark styling. Default 0.5.
#' @param sample_text,show_contrast,font_size,swatch_padding,browse See
#'   [preview_color_grid()].
#'
#' @return (Invisibly) the generated HTML as a single string.
#' @examples
#' \dontrun{
#' preview_luminance_grid("#59c5bf", "#b9c653")                 # fixed chroma
#' preview_luminance_grid("#59c5bf", "#b9c653", chroma = "max") # most vivid
#' preview_luminance_grid("#59c5bf", "#b9c653", table_bg = "#1a1a1a")  # dark
#' }
#' @keywords internal
preview_luminance_grid <- function(text_color,
                                   background_color,
                                   l_values = seq(0.35, 0.95, length.out = 7),
                                   chroma = c("fixed", "max"),
                                   sample_text = paste0(sample(0:100, 1L), "%"),
                                   show_contrast = TRUE,
                                   table_bg = "#ffffff",
                                   dark_threshold = 0.5,
                                   font_size = "16px",
                                   swatch_padding = "12px 16px",
                                   browse = TRUE) {
  .cg_require()
  chroma <- match.arg(chroma)
  stopifnot(length(text_color) == 1, length(background_color) == 1,
            length(table_bg) == 1)

  txt_lch <- .cg_oklch(text_color)
  bg_lch  <- .cg_oklch(background_color)

  # shade ramps: row = text lightness, col = background lightness
  txt_shades <- vapply(l_values, function(l)
    .cg_shade(l, txt_lch[["h"]], txt_lch[["c"]], chroma), character(1))
  bg_shades  <- vapply(l_values, function(l)
    .cg_shade(l, bg_lch[["h"]],  bg_lch[["c"]],  chroma), character(1))

  n <- length(l_values)
  text_hex <- matrix(rep(txt_shades, times = n), nrow = n)  # constant per row
  bg_hex   <- matrix(rep(bg_shades,  each  = n), nrow = n)  # constant per col

  lab <- sprintf("L=%.2f", l_values)
  .cg_kable_grid(
    text_hex, bg_hex,
    row_labels = lab, col_labels = lab,
    row_swatch = txt_shades, col_swatch = bg_shades,
    corner = "text \u2193 / bg \u2192",
    sample_text = sample_text, show_contrast = show_contrast,
    swatch_padding = swatch_padding,
    caption  = sprintf("Luminance shades \u2014 chroma: %s (cells show APCA Lc)", chroma),
    subtitle = sprintf(
      "text %s (hue %.0f\u00b0) \u00d7 background %s (hue %.0f\u00b0)",
      toupper(text_color), txt_lch[["h"]],
      toupper(background_color), bg_lch[["h"]]
    ),
    font_size = font_size, table_bg = table_bg, dark_threshold = dark_threshold,
    browse = browse
  )
}

#' @keywords internal
lcd_simulate_oklch <- function(
  colours,
  chroma_scale      = 0.60,  # how much to reduce chroma (0–1)
  lightness_center  = 0.50,  # where lightness is “anchored” (0–1 scale)
  lightness_compress = 0.90  # how much to compress lightness range (0–1)
) {
  # Decode hex (or R color names) directly to OKLCH
  oklch <- farver::decode_colour(colours, to = "oklch")
  L <- oklch[, 1]
  C <- oklch[, 2]
  H <- oklch[, 3]
  
  # Reduce chroma to simulate less vivid LCD color reproduction
  C_new <- C * chroma_scale
  C_new <- pmax(C_new, 0)       # no negative chroma
  
  # Compress lightness toward a center value
  # L is on 0–1 scale inside farver’s OKLCH representation
  L_new <- lightness_center + lightness_compress * (L - lightness_center)
  
  # Reassemble modified OKLCH
  oklch_new <- cbind(L_new, C_new, H)
  
  # Convert back to sRGB and clamp to valid range for encoding
  rgb_new <- farver::convert_colour(oklch_new, from = "oklch", to = "rgb")
  rgb_new <- pmin(pmax(rgb_new, 0), 255)
  
  # Encode back to hex
  farver::encode_colour(rgb_new, from = "rgb")
}

#' Simulate color vision deficiency for hex colors using farver + colorspace
#'
#' @param col Character vector of hex colors (e.g. "#03ab86").
#' @param type Type of CVD to simulate: "deutan" (green cone defective)
#'   or "protan" (red cone defective). These are the two most common
#'   congenital red–green deficiencies.
#' @param severity Numeric in [0, 1], Machado-style severity parameter
#'   (0 = normal vision, 1 = full dichromacy). Values around 1 correspond
#'   to deuteranopia/protanopia; values in (0, 1) emulate anomalous
#'   trichromacy.
#'
#' @return Character vector of hex colors representing how a trichromatic,
#'   color-normal viewer would see your input colors if they had the
#'   specified color vision deficiency.
#'
#' @details
#' The implementation follows the physiologically-based model of
#' Machado et al. (2009), using the RGB transform matrices provided by
#' colorspace::deutanomaly_cvd and colorspace::protanomaly_cvd
#' (interpolated by severity).[web:64][web:65]
#'
#' Gamma-corrected sRGB is linearised, transformed in RGB, and then
#' re-gamma-corrected. Conversion between hex and RGB is handled by
#' farver::decode_colour() and farver::encode_colour().[web:74][web:78]
#'
#' You can always inspect or design your palette in OKLCH using
#' farver::decode_colour(col, to = "oklch") before or after simulation;[web:74][web:69]
#' the CVD model itself, however, operates in sRGB.
#'
#' @keywords internal
simulate_cvd_farver <- function(col,
                                type = c("deutan", "protan"),
                                severity = 1) {
  # Dependencies:
  # farver    >= 2.1.0  (for decode_colour / encode_colour)
  # colorspace >= 2.1.0 (for Machado CVD matrices)
  type <- match.arg(type)

  if (!requireNamespace("farver", quietly = TRUE)) {
    stop("Package 'farver' is required but not installed.")
  }
  if (!requireNamespace("colorspace", quietly = TRUE)) {
    stop("Package 'colorspace' is required but not installed.")
  }

  # 1. Pick the appropriate list of CVD transform matrices
  #    from colorspace (Machado 2009 implementation).[web:64][web:57][web:65]
  transform_list <- switch(
    type,
    deutan = colorspace::deutanomaly_cvd,
    protan = colorspace::protanomaly_cvd
  )

  # Interpolate matrix for given severity in [0, 1]
  M <- colorspace::interpolate_cvd_transform(transform_list,
                                             severity = severity)

  # 2. Decode hex to sRGB (0–255) using farver.[web:74]
  rgb_255 <- farver::decode_colour(col, to = "rgb")

  # Normalise to 0–1
  rgb <- rgb_255 / 255

  # 3. Convert sRGB to linear RGB (per IEC 61966-2-1).[web:57]
  srgb_to_linear <- function(x) {
    ifelse(x <= 0.04045,
           x / 12.92,
           ((x + 0.055) / 1.055) ^ 2.4)
  }
  rgb_lin <- srgb_to_linear(rgb)

  # 4. Apply 3×3 CVD transform matrix in linear RGB.[web:64][web:65]
  # rgb_lin is n × 3; we want n × 3 back, so multiply by t(M).
  rgb_lin_sim <- as.matrix(rgb_lin) %*% t(M)

  # 5. Convert linear RGB back to gamma-corrected sRGB.[web:57]
  linear_to_srgb <- function(x) {
    ifelse(x <= 0.0031308,
           12.92 * x,
           1.055 * (x ^ (1 / 2.4)) - 0.055)
  }
  rgb_sim <- linear_to_srgb(rgb_lin_sim)

  # Clamp to [0, 1] and scale to 0–255
  rgb_sim_clamped <- pmin(pmax(rgb_sim, 0), 1)
  rgb_sim_255 <- round(rgb_sim_clamped * 255)

  # 6. Encode back to hex using farver.[web:78]
  col_sim <- farver::encode_colour(rgb_sim_255, from = "rgb")

  col_sim
}




# Other functions ----


#' @keywords internal
# From fs:: package : thank to Gábor Csárdi
path_sanitize <- function (filename, replacement = "") {
    illegal <- "[/\\?<>\\:*|\":]"
    control <- "[[:cntrl:]]"
    reserved <- "^[.]+$"
    windows_reserved <- "^(con|prn|aux|nul|com[0-9]|lpt[0-9])([.].*)?$"
    windows_trailing <- "[. ]+$"
    filename <- gsub(illegal, replacement, filename)
    filename <- gsub(control, replacement, filename)
    filename <- gsub(reserved, replacement, filename)
    filename <- gsub(windows_reserved, replacement, filename, 
        ignore.case = TRUE)
    filename <- gsub(windows_trailing, replacement, filename)
    filename <- substr(filename, 1, 255)
    if (replacement == "") {
        return(filename)
    }
    path_sanitize(filename, "")
}

