# Special tibble class needed for printing, even if the most meaningful attributes
#  where passed to fmt class variables (only chi2 and subtext remains at tab level) :
#  the implementation relies on "grouped_df" class structure, and to manage it, it is
#  necessary to add one method for class "tabxplor_grouped_tab" for each dplyr function...
#  (Thank to Giulia Pais, Davis Vaughan and Hadley Wickham,
#   https://github.com/tidyverse/dplyr/issues/5480).

# grouped_tab class still don't handle [] ----

# Problem with methods for dplyr::filter, because it replaces base::filter,
# which cannot be detached in namespace

# #Import dplyr in NAMESPACE :
# # dplyr is imported as a "Depends" package, otherwise dplyr::filter, needed for methods,
# # cannot be found by roxygen2 because it replaces base::filter.
#
# #' Internal dplyr methods
# #' @rawNamespace import(dplyr, except = data_frame)
# #  otherwise, conflict with vctrs. Thanks to Thomas :
# #  https://stackoverflow.com/questions/51899220/import-all-the-functions-of-a-package-except-one-when-building-a-package
# #' @keywords internal
# #' @name tabxplor-dplyr
# NULL

# #' To allow dplyr::filter to be used for methods
# #' @rawNamespace import(base, except = filter)
# #' @keywords internal
# #' @name no_base_filter
# NULL


# Create class tabxplor_tab --------------------------------------------------------------
# sloop::s3_methods_class("tbl")
# sloop::s3_get_method(print.tbl)
# cli::cat_line()
# sloop::s3_get_method(format.tbl)
# tibble::trunc_mat #Gives classes :
# c("trunc_mat_single_tab", "trunc_mat_tbl_df", "trunc_mat_tbl", "trunc_mat_data.frame", "trunc_mat")
# sloop::s3_methods_class("tibble::trunc_mat")
# sloop::s3_get_method(format.tibble::trunc_mat)
# sloop::s3_get_method(print.tibble::trunc_mat)
# sloop::s3_methods_class("pillar_colonnade")
# sloop::s3_get_method(format.pillar_colonnade)
# sloop::s3_get_method(print.pillar_colonnade)
# pillar::squeeze
# sloop::s3_methods_class("single_tab")

#' A constructor for class tabxplor_tab
#'
#' @param tabs A table, stored into a \code{\link[tibble]{tibble}} data.frame.
#' It is generally made with \code{\link{tab}}, \code{\link{tab_many}}
#' or \code{\link{tab_plain}}.
#' @param subtext A character vector to print legend lines under the table.
#' @param chi2 A tibble storing information about pvalues and variances, to fill with
#' \code{\link{tab_chi2}}.
#' @param ... Needed to implement subclasses.
#' @param class Needed to implement subclasses.
#'
#' @return A \code{tibble} of class \code{tabxplor_tab}.
#' @export
#  @examples
new_tab <-
  function(tabs = tibble::tibble(), subtext = "",
           chi2 = tibble::tibble(tables   = character(),
                                 pvalue   = double()   ,
                                 df       = integer()  ,
                                 cells    = integer()  ,
                                 variance = double()   ,
                                 count    = integer()   ),
           ..., class = character()) {
    stopifnot(is.data.frame(tabs))
    #vec_assert(subtext    , character())

    tibble::new_tibble(tabs, subtext = subtext, chi2 = chi2, ...,
                       nrow = nrow(tabs), class = c(class, "tabxplor_tab"))
  }

#' @param groups The grouping data.
#' @rdname new_tab
#' @return A \code{tibble} of class \code{tabxplor_grouped_tab}.
#' @export
new_grouped_tab <-
  function(tabs = tibble::tibble(), groups,
           subtext = "",
           chi2 = tibble::tibble(tables   = character(),
                                 pvalue   = double()   ,
                                 df       = integer()  ,
                                 cells    = integer()  ,
                                 variance = double()   ,
                                 count    = integer()   ),
           ..., class = character()) {
    if (missing(groups)) groups <- attr(tabs, "groups")
    class <- c(class, c("tabxplor_grouped_tab", "grouped_df"))

    new_tab(tabs, groups = groups,
            subtext = subtext, chi2 = chi2,
            ...,
            class = class)
  }



# Functions to work with class tabxplor_tab ----------------------------------------------

# Useful test fonction :
#' @describeIn tab_many a test function for class tabxplor_tab
#' @param x A object to test with \code{\link{is_tab}}.
#' @return A single logical.
#' @export
is_tab <- function(x) {
  inherits(x, "tabxplor_tab")
}

get_subtext <- purrr::attr_getter("subtext")
get_chi2    <- purrr::attr_getter("chi2")

# # In doc exemple they do :
#  df_colour <- function(x) {
# if (inherits(x, "my_tibble")) {
#   attr(x, "colour")
# } else {
#   NULL
# }
# }


# as_tab <- function(x, ...) {
#   UseMethod("as_tab")
# }
# as_tab.default <- function(x, ...) {
#   #vctrs::vec_cast(x, tab())
# }

#' @keywords internal
untab <- function(tabs) {
  if (lv1_group_vars(tabs)) {
    `class<-`(tabs, class(tabs) %>% purrr::discard(. == "tabxplor_tab"))
  } else {
    `class<-`(tabs, class(tabs) %>%
                purrr::discard(. %in% c("tabxplor_grouped_tab", "tabxplor_tab")))
  }
}


#Methods to print class tabxplor_tab -----------------------------------------------------

#' Printing method for class tabxplor_tab
#' @param x Object to format or print.
#' @param ... Passed on to \code{tbl_format_setup()}.
#' @param n Number of rows to show.
#' @param width Width of text output to generate.
#' @param max_extra_cols Number of extra columns to print abbreviated information for,
#'   if the width is too small for the entire tibble.
#' @param max_footer_lines Maximum number of footer lines.
#' @param min_row_var Minimum number of characters for the row variable. Default to 30.
#' @param get_text Set to `TRUE` to get the text as a character vector
#' instead of a printed output.
#' @export
#' @return A printed table.
#' @method print tabxplor_tab
print.tabxplor_tab <- function(x, width = NULL, ..., n = 100, max_extra_cols = NULL,
                               max_footer_lines = NULL, min_row_var = 30, get_text = FALSE) {
  print_chi2(x, width = width)

  if (getOption("tabxplor.print") == "kable") {
    x <- tab_kable(x)
    print(x)
    return(invisible(x))
  }

  # Use pillar::char() on row_var to control truncation
  row_var   <- tab_get_vars(x)$row_var
  n_row_var <- which(names(x) == row_var)

  out <- dplyr::mutate(x, dplyr::across(
    tidyselect::all_of(row_var),
    ~ pillar::char(as.character(.), min_chars = min_row_var)
  ))

  # out <- format(out, width = NULL)
  out <- format(out, width = width, ..., n = n, max_extra_cols = max_extra_cols,
                max_footer_lines = max_footer_lines)

  # very bad workaround to retransform the <char> type into <fct>
  if (length(n_row_var) != 0) {
    regular_ex <-
      paste0("^(", paste0(rep("[^<]+<", n_row_var), collapse = ""), ")<char>") %>%
      stringr::str_replace("<\\)<", ")<")

    out[3] <- out[3] %>% stringr::str_replace(regular_ex, "\\1<fct> ")
  }


  # writeLines(format(x, width = width, ..., n = n, max_extra_cols = max_extra_cols,
  #                   max_footer_lines = max_footer_lines))
  if (get_text) {
    out
  } else {
    writeLines(out)
    invisible(x)
  }
}

#' Printing method for class tabxplor_grouped_tab
#' @param x Object to format or print.
#' @param ... Passed on to \code{tbl_format_setup()}.
#' @param n Number of rows to show.
#' @param width Width of text output to generate.
#' @param max_extra_cols Number of extra columns to print abbreviated information for,
#'   if the width is too small for the entire tibble.
#' @param max_footer_lines Maximum number of footer lines.
#' @param min_row_var Minimum number of characters for the row variable. Default to 30.
#' @param get_text Set to `TRUE` to get the text as a character vector
#' instead of a printed output.
#'
#' @export
#' @return A printed grouped table.
#' @method print tabxplor_grouped_tab
print.tabxplor_grouped_tab <- function(x, width = NULL, ..., n = 100,
                                       max_extra_cols = NULL,max_footer_lines = NULL,
                                       min_row_var = 30, get_text = FALSE) {
  print_chi2(x, width = width)

  if (getOption("tabxplor.print") == "kable") {
    x <- tab_kable(x)
    print(x)
    return(invisible(x))
  }

  # Use pillar::char() on row_var to control truncation
  row_var   <- tab_get_vars(x)$row_var
  n_row_var <- which(names(x) == row_var)

  out <- dplyr::mutate(x, dplyr::across(
    tidyselect::all_of(row_var),
    ~ pillar::char(as.character(.), min_chars = min_row_var)
  ))

  # out <- format(out, width = NULL)
  out <- format(out, width = width, ..., n = n, max_extra_cols = max_extra_cols,
                max_footer_lines = max_footer_lines)

  # very bad workaround to retransform the <char> type into <fct>
  if (length(n_row_var) != 0) {
    regular_ex <-
      paste0("^(", paste0(rep("[^<]+<", n_row_var), collapse = ""), ")<char>") %>%
      stringr::str_replace("<\\)<", ")<")

    out[4] <- out[4] %>% stringr::str_replace(regular_ex, "\\1<fct> ")
  }

  # writeLines(format(x, width = width, ..., n = n, max_extra_cols = max_extra_cols,
  #                   max_footer_lines = max_footer_lines))
  if (get_text) {
    out
  } else {
    writeLines(out)
    invisible(x)
  }

  }

#' @keywords internal
print_chi2 <- function(x, width = NULL) {
  chi2 <- get_chi2(x)
  if (is.null(chi2)) return(NULL)
  if (nrow(chi2) == 0) return(NULL)
  # if (is.na(chi2)) return(NULL)

  chi2 <- chi2 %>% # dplyr::select(-"row_var") %>%
    dplyr::filter(!.data$`chi2 stats` %in% c("cells"))

  fmt_cols <- purrr::map_lgl(chi2, is_fmt) %>% purrr::keep(. == TRUE) %>%
    names() #%>% rlang::syms()
  if (length(fmt_cols) != 0) {
    row_all_na <- chi2 %>%
      dplyr::select(where(is_fmt)) %>%
      purrr::map_df(is.na)
    row_all_na <- row_all_na %>%
      dplyr::rowwise() %>%
      dplyr::mutate(empty = all(dplyr::c_across(cols = dplyr::everything()))) %>%
      dplyr::pull(.data$empty)

    chi2 <- chi2 %>% dplyr::filter(!row_all_na)
  }

  chi2 <- chi2 %>%
    dplyr::mutate(dplyr::across(where(is_fmt),
                                ~ `class<-`(., c("tab_chi2_fmt", class(.)))  ))

  nrow_chi2 <- nrow(chi2)
  if (nrow_chi2 == 0) return(NULL)

  ind <- chi2 %>% dplyr::group_by(dplyr::across(where(is.factor))) %>%
    dplyr::group_indices()
  ind   <- c(TRUE, ind != dplyr::lead(ind, default = max(ind) + 1) )

  chi2 <- chi2 %>%
    dplyr::mutate(dplyr::across(
      where(is.factor),
      ~ dplyr::if_else(. == dplyr::lag(as.character(.), default = paste0(as.character(dplyr::first(.)), "a")),
                       #. == dplyr::lag(., default = paste0(as.character(.[1]), "a")),
                       true = stringi::stri_unescape_unicode("\\u00a0"),
                       false = as.character(.))
      %>% as.factor()
    ))

  # setup <- pillar::tbl_format_setup(chi2, width = NULL)
  setup <- pillar::tbl_format_setup(chi2, width = width, n = Inf)
  body_no_type <- tbl_format_body(chi2, setup)[-2]
  body_no_type <- body_no_type %>%
    stringr::str_replace("`chi2 stats`", "chi2 stats  ") %>%
    crayon::col_substr(stringr::str_length(nrow_chi2) + 2L, crayon::col_nchar(.))
  body_no_type[ind] <- crayon::underline(body_no_type[ind] )
  body_no_type <- body_no_type %>% `class<-`("pillar_vertical")

  cli::cat_line(body_no_type)
  cli::cat_line()
}


#' Table headers for class tab
#' @importFrom pillar tbl_sum
#' @param x An object of class tabxplor_tab
#' @param ... Other parameters.
#' @return A table header
#' @export
#' @method tbl_sum tabxplor_tab
tbl_sum.tabxplor_tab <- function(x, ...) {
  tbl_header <- NextMethod()
  names(tbl_header)[1] <- "A tabxplor tab"
  tbl_header
}
#' Table headers for class grouped tab
#' @return A table header
#' @param x An object of class tabxplor_tab
#' @param ... Other parameters.
#' @export
#' @method tbl_sum tabxplor_grouped_tab
tbl_sum.tabxplor_grouped_tab <- function(x, ...) {
  grouped_tbl_header <- NextMethod()
  names(grouped_tbl_header)[1] <- "A tabxplor tab"
  grouped_tbl_header
}


#' Table footer for class tab
#' @importFrom pillar tbl_format_footer
#' @param x An object of class tabxplor_tab
#' @param setup A setup object from the table
#' @param ... Other parameters.
#' @return A character vector.
#' @export
#' @method tbl_format_footer tabxplor_tab
tbl_format_footer.tabxplor_tab <- function(x, setup, ...) {
  default_footer <- NextMethod()

  print_colors <- suppressWarnings(tab_color_legend(x))
  subtext <- get_subtext(x) %>% purrr::discard(. == "")
  if (length(print_colors) != 0) print_colors <- paste0(
    pillar::style_subtle("# "), print_colors
  )
  if (length(subtext) != 0) subtext <- pillar::style_subtle( paste0("# ", subtext) )

  c(default_footer, print_colors, subtext)
}


#' Table body for class tab
#' @importFrom pillar tbl_format_body
#' @param x An object of class tabxplor_tab
#' @param setup A setup object from the table
#' @param ... Other parameters.
#' @return A character vector.
#' @export
#' @method tbl_format_body tabxplor_tab
tbl_format_body.tabxplor_tab <- function(x, setup, ...) {
  default_body <- NextMethod()

  body_data  <- default_body[-(1:2)]
  ind   <- dplyr::group_indices(setup$x)[1:length(body_data)]
  ind   <- tidyr::replace_na(ind != dplyr::lag(ind, default = 1L), FALSE)
  body_data <- body_data %>%
    purrr::map2(ind, function(.x, .y) if (.y) {c("", .x)} else {.x}) %>%
    purrr::flatten_chr()

  c(default_body[1:2], body_data) %>% `class<-`("pillar_vertical")
}



#' Print a tabxplor table in html
#'
#' @param tabs A table made with \code{\link{tab}} or \code{\link{tab_many}},
#'   or a `list` of tab with the same `col_vars` and no `tab_vars`.
#' @param color_type  Set to \code{"text"} to color the text, \code{"bg"} to color the
#' background. By default it takes \code{getOption("tabxplor.color_style_type")}.
#' @param theme By default, a white table with black text, Set to \code{"dark"} for a
#' black table with white text.
#' @param html_24_bit Use 24bits colors palettes for html tables : set to `"green_red"`
#' or `"blue_red"`. Only with `mode = "color_code"` (not `mode = "crayon"`) and
#' `theme = "light`. Default to \code{getOption("tabxplor.color_html_24_bit")}.
#' @param tooltips By default, html tooltips are used to display additional informations
#' at mouse hover. Set to \code{FALSE} to discard.
#' @param popover By default, takes \code{getOption("tabxplor.kable_popover")}. When
#' `FALSE`, html tooltips are of the base kind : they can't be used with floating table of
#' content in \pkg{rmarkdown} documents. Set to `TRUE` to use \pkg{kableExtra} html
#' popovers instead, which are compatible with floating toc. Remember
#' to enable the `popover` module by copying the following code into your document :
#' \code{<script>
#' $(document).ready(function(){
#'   $('[data-toggle="popover"]').popover();
#' });
#' </script>
#'}
#' @param color_legend Print colors legend below the table ?
#' You can then use a `css` chunk in rmarkdown to change popovers colors.
#' @param full_width A TRUE or FALSE variable controlling whether the HTML table
#' should have the preferable format for full_width. If not specified, a HTML
#' table will have full width by default but this option will be set to FALSE for
#' a LaTeX table.
#' @param html_font A string for HTML css font. By default, it uses
#'  `'"DejaVu Sans", "Arial", arial, helvetica, sans-serif'`. Set another
#'  default by setting `options("tabxplor.kable_html_font" = )`.
#' @param caption The table caption. For formatting, you need to use a `css`
#' with `caption{}`in rmarkdown.
#' @param wrap_rows By default, rownames are wrapped when larger than 30 characters.
#' @param wrap_cols By default, colnames are wrapped when larger than 12 characters.
#' @param whitespace_only Set to `FALSE` to wrap also on non whitespace characters.
# @param unbreakable_spaces Set to `FALSE` to keep normal spaces in text (auto-break).
#' @param get_data Get the transformed data instead of the html table.
#' @param ... Other arguments to pass to \code{\link[kableExtra:kable_styling]{kableExtra::kable_styling}}.

#' @return A html table (opened in the viewer in RStudio). Differences from totals,
#' confidence intervals, contribution to variance, and unweighted counts,
#' are available in an html tooltip at cells hover.
#' @export
#'
#' @examples
#' \donttest{
#' tabs <- tab(forcats::gss_cat, race, marital, year, pct = "row", color = "diff")
#' tab_kable(tabs, theme = "light", color_type = "text")
#' }
tab_kable <- function(tabs,
                      theme = c("light", "dark"), color_type = NULL, html_24_bit = NULL,
                      tooltips = TRUE, popover = NULL, color_legend = TRUE,
                      caption = knitr::opts_current$get("tab.cap"),
                      html_font = NULL,
                      get_data = FALSE,
                      full_width = FALSE,
                      wrap_rows = 35, wrap_cols = 15,
                      whitespace_only = TRUE, # unbreakable_spaces = TRUE,
                      ...) {
  #theme <- if (is.null(theme)) { getOption("tabxplor.color_style_theme") } else { theme }
  color_type <-
    if (is.null(color_type)) { getOption("tabxplor.color_style_type") } else {color_type}

  html_24_bit <-
    if (is.null(html_24_bit)) {getOption("tabxplor.color_html_24_bit")} else {html_24_bit}

  html_font <-
    if (is.null(html_font)) {getOption("tabxplor.kable_html_font")} else {html_font}



  popover <- if (is.null(popover)) {getOption("tabxplor.kable_popover")} else {popover}

  # with a list of tab, bind them all in a single tab if possible
  if (is.list(tabs) & !is.data.frame(tabs)) {
    same_col_vars <- purrr::map(tabs, ~ tab_get_vars(.)$col_vars)
    same_col_vars <- same_col_vars |>
      purrr::map(~ .[!. %in% c("all_col_vars", "", "no") & !is.na(.)])
    longest_col_vars <- purrr::map_int(same_col_vars, length)
    longest_col_vars <-
      dplyr::first(which(longest_col_vars == max(longest_col_vars, na.rm = TRUE)))
    longest_col_vars <- same_col_vars[[longest_col_vars]]
    same_col_vars <- same_col_vars |> purrr::map_lgl(~ all(. %in% longest_col_vars))
    if(!all(same_col_vars)) {
      stop("tab_kable() can only be used with a list of tab if they have the same col_vars")
    }

    if (any(purrr::map_lgl(tabs, ~ length(tab_get_vars(.)$tab_vars) > 0 )) ) {
      stop("tab_kable() can only be used with a list of tab if they have no tab_vars")
    }

    tabs <- tab_compact(tabs) # pvalue_lines = TRUE
  }


  #   otherwise signif stars * break the html
  if(!is.null(knitr::opts_knit$get("out.format"))) {
    tabs <- tabs |>
      dplyr::mutate(
        dplyr::across(
          dplyr::where(is.character) ,  # & tidyselect::starts_with("s")
          ~ stringr::str_replace_all(., "\\*", "\\\\*")
        ),

        # dplyr::across(
        #   dplyr::where(is.factor) ,  # & tidyselect::starts_with("s")
        #   ~ forcats::fct_relabel(., ~ stringr::str_replace_all(., "\\*", "\\\\*"))
        # )

      )
  }


  tab_vars <- tab_get_vars(tabs)$tab_vars
  subtext  <- get_subtext(tabs) %>% purrr::discard(. == "")

  new_group <- tabs %>% #dplyr::group_by(dplyr::across(where(is.factor))) %>%
    dplyr::group_indices()
  new_group <- which(new_group != dplyr::lead(new_group, default = max(new_group) + 1))


  tabs <- tabs %>% dplyr::ungroup() %>% dplyr::select(-tidyselect::all_of(tab_vars))

  tabs <- tabs |>
    tab_wrap_text(wrap_rows = wrap_rows,
                  wrap_cols = wrap_cols,
                  exdent = 2,
                  whitespace_only = whitespace_only,
                  unbreakable_spaces = TRUE,
                  brk = "<br>")


  row_var <- which(names(tabs) == tab_get_vars(tabs)$row_var)

  color_cols     <- get_color(tabs)
  fmt_no_colors  <- purrr::map_lgl(tabs, is_fmt) &
    (color_cols %in% c("", "no") | is.na(color_cols))
  fmt_no_colors  <- names(fmt_no_colors)[fmt_no_colors]
  color_cols     <- which(!color_cols %in% c("", "no") & !is.na(color_cols))
  type_cols      <- which(!color_cols %in% c("", "no") & !is.na(color_cols))
  fmt_cols <- which(purrr::map_lgl(tabs, is_fmt))
  color_cols_fmt <- names(color_cols)[names(color_cols) %in% names(fmt_cols)]

  other_cols <- which(purrr::map_lgl(tabs, ~ !is_fmt(.)))

  totcols     <- which(is_totcol(tabs))
  totrows     <- which(is_totrow(tabs))
  no_totrows  <- which(!is_totrow(tabs))

  new_col_var <- get_col_var(tabs)
  new_col_var[names(other_cols)] <- names(other_cols)
  new_col_var <- which(new_col_var != dplyr::lead(new_col_var, default = "._at_the_end"))

  text_color  <- dplyr::if_else(theme[1] == "light", "#000000", "#FFFFFF")
  grey_color  <- dplyr::if_else(theme[1] == "light", "#888888", "#BBBBBB")
  grey_color2 <- dplyr::if_else(theme[1] == "light", "#111111", "#EEEEEE")

  references <- tabs[fmt_cols] %>%
    purrr::map(~ get_reference(., mode = "all_totals") %>%
                 dplyr::if_else(true      = text_color,
                                false     = "no_color") %>%
                 list() %>% purrr::set_names(text_color)
    )

  color_selection <- references

  if (length(color_cols_fmt) != 0) {
    color_selection[color_cols_fmt] <- purrr::map(tabs[color_cols], fmt_color_selection)

    color_styles <- purrr::pmap(list(color_selection[color_cols_fmt],
                                     get_color(tabs)[color_cols_fmt],
                                     get_type(tabs)[color_cols_fmt]
    ),
    ~ select_in_color_style(
      names(..1),
      pct_diff =  ..2 %in% c("diff", "diff_ci", "after_ci") &
        !..3 %in% c("n", "mean")
    ))

    color_styles <- purrr::map(color_styles,
                               ~ get_color_style(mode = "color_code",
                                                 type = color_type[1],
                                                 theme = theme[1],
                                                 html_24_bit = html_24_bit[1])[.])

    color_selection[color_cols_fmt] <- color_selection[color_cols_fmt] %>%
      purrr::map2(color_styles, ~ purrr::set_names(.x, .y)) %>%
      purrr::map(~ purrr::imap(., ~ dplyr::if_else(condition = .x,
                                                   true      = .y,
                                                   false     = "no_color")) ) %>%
      purrr::map2(references[color_cols_fmt], ~ c(.x, .y) %>%
                    purrr::reduce(~ dplyr::if_else(.x == "no_color", .y, .x)) %>%
                    stringr::str_replace(., "no_color", grey_color) %>%
                    tidyr::replace_na(grey_color)
      )
  }

  if (length(fmt_no_colors) != 0) {
    color_selection[fmt_no_colors] <- color_selection[fmt_no_colors] %>%
      purrr::map(~ purrr::flatten_chr(.) %>%
                   stringr::str_replace(., "no_color", grey_color2) %>%
                   tidyr::replace_na(grey_color2)
      )
  }

  if (color_type == "text") {
    out <- tabs %>%
      dplyr::mutate(dplyr::across(
        where(is_fmt),
        ~ format(., html = TRUE, special_formatting = TRUE, na = "") %>%
          kableExtra::cell_spec(
            bold  = !color_selection[[dplyr::cur_column()]] %in% c(grey_color, grey_color2), #text_color
            color =  color_selection[[dplyr::cur_column()]],
            tooltip = if (!popover & tooltips) {tab_kable_print_tooltip(.)} else {NULL},
            popover = if (popover & tooltips) {tab_kable_print_tooltip(., popover = TRUE)} else {NULL}
          )
      ))

  } else {
    bg_color_selection  <- color_selection %>%
      purrr::map(~ stringr::str_replace_all(., text_color, "none") %>%
                   stringr::str_replace_all(grey_color, "none")        )

    txt_color_selection <- color_selection %>%
      purrr::map(~ dplyr::if_else(stringr::str_detect(., text_color) |
                                    stringr::str_detect(., grey_color) |
                                    stringr::str_detect(., grey_color2),
                                  true  = .,
                                  false = text_color)               )

    out <- tabs %>%
      dplyr::mutate(dplyr::across(
        where(is_fmt),
        ~ format(., special_formatting = TRUE) %>%
          kableExtra::cell_spec(
            bold  = color_selection[[dplyr::cur_column()]] %in% c(text_color), #text_color
            color      = txt_color_selection[[dplyr::cur_column()]],
            background = bg_color_selection[[dplyr::cur_column()]],
            tooltip = if (!popover & tooltips) {tab_kable_print_tooltip(.)} else {NULL},
            popover = if (popover & tooltips) {tab_kable_print_tooltip(., popover = TRUE)} else {NULL}
          )
      ))
  }

  if (get_data) return(out)

  # refs2 <- tabs[[fmt_cols[1]]] %>% get_reference(mode = "all_totals")
  #
  #   out <- out %>%
  #     dplyr::mutate(dplyr::across(
  #       where(~ !is_fmt(.)),
  #       ~ as.character(.) %>% kableExtra::cell_spec(align = "r", bold  = refs2)
  #       ))

  if (color_legend) {
    if (length(color_cols) != 0) subtext <- c(
   suppressWarnings(tab_color_legend(tabs,
                       mode = "html",
                       html_type  = color_type[1],
                       html_theme = theme[1],
                       html_24_bit = html_24_bit[1],
                       text_color = text_color,
                       grey_color = grey_color)),
      subtext)
  }


  alignement <- tabs |>
    purrr::map_chr(
      ~ dplyr::if_else(condition = is_fmt(.) | is.numeric(.),
                       true      = "r",
                       false     = "l")
    )

  out <- knitr::kable(out, escape = FALSE, format = "html", align = alignement,
                      #table.attr = "style=\"border-top: 0; border-bottom: 0; cellspacing: -10pt\"",
                      caption = caption)
  # table.attr changes css style of table_classic (no upper and lower big lines)

  if (theme[1] == "light") {
    out <- out %>% kableExtra::kable_classic(
      lightable_options = "hover", # "striped", ?
      #bootstrap_options = c("hover", "condensed", "responsive", "bordered"), #"striped",
      full_width = full_width,
      html_font = html_font, # "DejaVu Sans Condensed", # row_label_position
      #fixed_thead = TRUE,
      ...
    )

  } else {
    out <- out %>% kableExtra::kable_material_dark(
      lightable_options = "hover",
      bootstrap_options = c("hover", "condensed", "responsive"), #"striped",
      full_width = full_width,
      html_font = html_font, # "DejaVu Sans Condensed", # row_label_position
      #fixed_thead = TRUE,
      ...
    )

  }

  # # Needed to make refrows or if not totrows in bold,
  # tot_or_ref <- tabs[[fmt_cols[1]]] %>% get_reference(mode = "all_totals") %>% which()
  refref <- purrr::map_dfr(tabs[fmt_cols] , ~ get_reference(., mode = "all_totals") )
  refref <- refref |> dplyr::select(-where(all), -where(~ !any(.)))
  tot_or_ref <- which(rowSums(refref) == ncol(refref))

  tot_n_pval <- is_totrow(tabs) |
    (!is_totrow(tabs) & dplyr::pull(tabs, row_var) %in% c("n", "pvalue", "row_pct"))

  tot_rows_1 <- which(
    dplyr::if_else(tot_n_pval,
                   !dplyr::lag(tot_n_pval),
                   FALSE)
  )
  tot_rows_last <- which(
    dplyr::if_else(tot_n_pval,
                   !dplyr::lead(tot_n_pval, default = FALSE),
                   FALSE)
  )



if (length(subtext) != 0) {
  out <- out %>% kableExtra::add_footnote(subtext, notation = "none", escape = FALSE)
}


out <- out %>%
  kableExtra::row_spec(
    0, color = text_color, bold = TRUE,
    extra_css = "border-top: 0px solid ; border-bottom: 1px solid ;font-size: 90%;vertical-align: bottom;line-height: 0.9;padding: 3px;text-align: center;" #
  ) %>%
  kableExtra::row_spec(tot_or_ref, bold = TRUE) %>%
  # kableExtra::row_spec(
  #   totrows, #bold = TRUE,
  #   extra_css = "border-top: 1px solid ; border-bottom: 1px solid ;"
  # ) %>%
  kableExtra::row_spec(tot_rows_1, extra_css = "border-top: 1px solid ;") %>%
  kableExtra::row_spec(tot_rows_last, extra_css = "border-bottom: 1px solid ;") %>%
  #kableExtra::row_spec(no_totrows, extra_css = "border-top: 0px solid ;") %>%
  kableExtra::column_spec(fmt_cols, extra_css = "white-space: nowrap;") %>%
  kableExtra::column_spec(unique(c(new_col_var, ncol(tabs))), border_right = TRUE) %>%
  kableExtra::column_spec(other_cols, border_left = TRUE) %>%
  kableExtra::column_spec(totcols, border_left = TRUE, width_min = 11) %>% # bold = TRUE
  kableExtra::column_spec(row_var, width_min = 20) %>%
  kableExtra::row_spec(new_group, extra_css = "border-bottom: 2px solid;") %>%
  kableExtra::row_spec(nrow(tabs), extra_css = "border-bottom: 1px solid;") |>
  kableExtra::row_spec(1:nrow(tabs), extra_css = "vertical-align: top; line-height: 0.85;padding: 3px;")


if (getOption("tabxplor.always_add_css_in_tab_kable") | interactive()) {
  out <- paste0(

    htmltools::includeCSS(system.file("tab.css", package = "tabxplor")),
    "\n",
    # "<script type=\"text/x-mathjax-config\">MathJax.Hub.Config({tex2jax: {inlineMath: [[\"$\",\"$\"]]}})</script>",
    # "<script async src=\"https://mathjax.rstudio.com/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML\"></script>",
    # "\n",
    as.character(out) |>
      stringr::str_replace_all(">NA</span>", "></span>") #|>
    #stringr::str_replace_all("<td style", '<td class = "align-top"; style')
  ) |>
    vctrs::vec_restore(out)
}


out
}



#' Print a tabxplor table in html
#'
#' @param tabs A data.frame.
#' @param theme By default, a white table with black text, Set to \code{"dark"} for a
#' black table with white text.
#' @param total_in_bold Should rows and cols with "Total" string be set in bold ?
#' @param all_column_borders Put a vertical border around each column ?
#' @param html_font A string for HTML css font. By default, it uses
#'  `'"DejaVu Sans", "Arial", arial, helvetica, sans-serif'`. Set another
#'  default by setting `options("tabxplor.kable_html_font" = )`.
#' @param caption The table caption. For formatting, you need to use a `css`
#' with `caption{}`in rmarkdown.
#' @param full_width A TRUE or FALSE variable controlling whether the HTML table
#' should have the preferable format for full_width. If not specified, a HTML
#' table will have full width by default but this option will be set to FALSE for
#' a LaTeX table.
#' @param wrap_rows By default, rownames are wrapped when larger than 30 characters.
#' @param wrap_cols By default, colnames are wrapped when larger than 12 characters.
#' @param whitespace_only Set to `FALSE` to wrap also on non whitespace characters.
# @param unbreakable_spaces Set to `FALSE` to keep normal spaces in text (auto-break).
#' @param subtext A character vector to print rows of legend under the table.
#' @param ... Other arguments to pass to \code{\link[kableExtra:kable_styling]{kableExtra::kable_styling}}.


#' @return A html table (opened in the viewer in RStudio). Differences from totals,
#' confidence intervals, contribution to variance, and unweighted counts,
#' are available in an html tooltip at cells hover.
#' @export
#'
#' @examples
#' \donttest{
#' tabs <- tibble::tibble(nm      = c("First", "Second", "Total"),
#'                        column1 = c(1, 2, 3),
#'                        column2 = c(4, 5, 6)                    )
#' kable_tabxplor_style(tabs)
#' }
kable_tabxplor_style <- function(tabs,
                                 caption = knitr::opts_current$get("tab.cap"),
                                 theme = c("light", "dark"),
                                 total_in_bold = TRUE, all_column_borders = FALSE,
                                 html_font = NULL,
                                 full_width = FALSE,
                                 wrap_rows = 35, wrap_cols = 15,
                                 whitespace_only = TRUE, # unbreakable_spaces = TRUE,
                                 subtext = "",
                                 ...) {

  html_font <-
    if (is.null(html_font)) {getOption("tabxplor.kable_html_font")} else {html_font}


  tabs <- tabs %>% dplyr::ungroup()

  tabs <- tabs |>
    tab_wrap_text(wrap_rows = wrap_rows,
                  wrap_cols = wrap_cols,
                  exdent = 2,
                  whitespace_only = whitespace_only,
                  unbreakable_spaces = TRUE,
                  brk = "<br>")

  alignement <- tabs |>
    purrr::map_chr(
      ~ dplyr::if_else(condition = is_fmt(.) | is.numeric(.),
                       true      = "r",
                       false     = "l")
    )

  out <- tabs |> knitr::kable(escape = FALSE, format = "html", align = alignement,
                              #table.attr = "style=\"border-top: 0; border-bottom: 0; cellspacing: -10pt\"",
                              caption = caption)
  # table.attr changes css style of table_classic (no upper and lower big lines)

  if (theme[1] == "light") {
    out <- out %>% kableExtra::kable_classic(
      lightable_options = "hover", # "striped", ?
      #bootstrap_options = c("hover", "condensed", "responsive", "bordered"), #"striped",
      full_width = full_width,
      html_font = html_font, # "DejaVu Sans Condensed", # row_label_position
      #fixed_thead = TRUE,
      ...
    )

  } else {
    out <- out %>% kableExtra::kable_material_dark(
      lightable_options = "hover",
      bootstrap_options = c("hover", "condensed", "responsive"), #"striped",
      full_width = full_width,
      html_font = html_font, # "DejaVu Sans Condensed", # row_label_position
      #fixed_thead = TRUE,
      ...
    )

  }

  if (length(subtext) != 0) {
    if (subtext != "") out <- out %>% kableExtra::add_footnote(subtext, notation = "none", escape = FALSE)
  }

  totcols <- which(stringr::str_detect(names(tabs), "^Total|^Ensemble"))
  totrows <- which(stringr::str_detect(tabs[[1]], "^Total|^Ensemble"))

  out <- out %>%
    kableExtra::row_spec(
      0, bold = TRUE, # color = "black"
      extra_css = "border-top: 0px solid ; border-bottom: 1px solid ;font-size: 90%;vertical-align: bottom;line-height: 0.9;padding: 3px;text-align: center;" #
    ) %>%
    #kableExtra::row_spec(refs2, bold = TRUE) %>%
    kableExtra::row_spec(
      nrow(tabs), extra_css = "border-bottom: 1px solid ;"
    ) %>%
     #kableExtra::column_spec(fmt_cols, extra_css = "white-space: nowrap;") %>%
    #kableExtra::column_spec(unique(c(new_col_var, ncol(tabs))), border_right = TRUE) %>%
    #kableExtra::column_spec(other_cols, border_left = TRUE) %>%
    kableExtra::column_spec(1, width_min = 20, border_left = TRUE, border_right = TRUE) %>%
    kableExtra::column_spec(ncol(tabs), border_right = TRUE) %>%
     #kableExtra::row_spec(new_group, extra_css = "border-bottom: 1px solid;") %>%
    #kableExtra::row_spec(nrow(tabs), extra_css = "border-bottom: 1px solid;") |>
    kableExtra::row_spec(
      1:nrow(tabs),
      extra_css = "vertical-align: top; line-height: 0.85;padding: 3px;white-space: nowrap;"
    )

  if (total_in_bold) {
    out <- out |>
      kableExtra::row_spec(
        totrows, bold = TRUE,
        extra_css = "border-top: 1px solid ; border-bottom: 1px solid ;"
      ) |>
      kableExtra::column_spec(totcols, bold = TRUE, width_min = 11, border_left = TRUE)

    } else {
      out <- out |>
        kableExtra::row_spec(
          totrows,
          extra_css = "border-top: 1px solid ; border-bottom: 1px solid ;"
        ) |>
        kableExtra::column_spec(totcols, width_min = 11, border_left = TRUE)
    }

  if (all_column_borders) {
    out <- out |> kableExtra::column_spec(1:ncol(tabs), border_left = TRUE)
  }


  if (getOption("tabxplor.always_add_css_in_tab_kable") | interactive()) {
    out <- paste0(

      htmltools::includeCSS(system.file("tab.css", package = "tabxplor")),
      "\n",
      # "<script type=\"text/x-mathjax-config\">MathJax.Hub.Config({tex2jax: {inlineMath: [[\"$\",\"$\"]]}})</script>",
      # "<script async src=\"https://mathjax.rstudio.com/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML\"></script>",
      # "\n",
      as.character(out) #|>
      #stringr::str_replace_all("<td style", '<td class = "align-top"; style')
    ) |>
      vctrs::vec_restore(out)
  }


  out
}




#' Bind a list of tabs with the same col_vars (and no tab_vars) into a single tab
#'
#' @param tabs A `list` of `tabxplor_tab` (or a `tabxplor_tab`)
# @param pvalue_lines Set to `TRUE` to add a line with chi2 pvalues under each table.
#'
#' @returns A `tabxplor_tab`
#' @export
#'
#' @examples
#' \donttest{
#' forcats::gss_cat |>
#'   tab_many(c(race, rincome), marital, pct = "row", color = "diff") |>
#'   tab_compact()
#' }
tab_compact <- function(tabs) { # pvalue_lines = FALSE
  tabs_base <- tabs

  if (is.data.frame(tabs)) {tabs <- list(tabs) |> purrr::set_names(names(tabs)[1]) }

  if (any(purrr::map_lgl(tabs, ~ length(tab_get_vars(.)$tab_vars) > 0 )) ) {
    if (!getOption("tabxplor.compact")) {
      message("since some tab_vars were provided, tab_compact() was not used")
    }
    return(tabs_base)
    #stop("tab_compact() can't be used with tab_vars")
  }

  same_col_vars <- purrr::map(tabs, ~ tab_get_vars(.)$col_vars)
  same_col_vars <- same_col_vars |>
    purrr::map(~ .[!. %in% c("all_col_vars", "", "no") & !is.na(.)])
  longest_col_vars <- purrr::map_int(same_col_vars, length)
  longest_col_vars <-
    dplyr::first(which(longest_col_vars == max(longest_col_vars, na.rm = TRUE)))
  longest_col_vars <- same_col_vars[[longest_col_vars]]
  same_col_vars <- same_col_vars |> purrr::map_lgl(~ all(. %in% longest_col_vars))
  if(!all(same_col_vars)) {
    stop("tab_compact() can only be used with the same col_vars in each tab")
  }


  subtext <- get_subtext(tabs[[1]])

  tabs_chi2 <- purrr::map_df(tabs, ~get_chi2(.) )

  # var_type <- tabs |> map(get_type) |> first()
  # var_type <- first(unique(type[!type %in% c("", "n")]))
  #
  # color_type <- tabs |> map(get_color) |> first()
  # color_type <- first(unique(color_type[!color_type %in% c("", "no") &
  #                                         !names(color_type) %in% ("n")]))




  tabs <- tabs |> purrr::imap_dfr(
    ~ dplyr::rename_with(.x, ~"levels", .cols =  1) |>
      dplyr::mutate(row_var = as.factor(.y), .before = 1) |>
      dplyr::mutate(dplyr::across(
        dplyr::where(is_fmt),
        ~ dplyr::if_else(is_totrow(.) & !any(is_refrow(.)),
                         true  = as_refrow(.),
                         false = .
        )
      ))
  )

  # tabs$Danser |> vctrs::vec_data()
  # tabs |> tab_kable()


  # col_vars <- get_col_var(tabs)[ get_col_var(tabs) != "" &
  #                                  names(get_col_var(tabs)) != "n" &
  #                                  !str_detect(names(get_col_var(tabs)), "^Total") ]

  if (sum(stringr::str_detect(names(tabs), "^Total_")) == 1) {
    tabs <- tabs |>
      dplyr::rename_with(~ "Total", .cols = tidyselect::starts_with("Total_"))
  }

  tabs <- new_tab(tabs, subtext = subtext, chi2 = tabs_chi2) |>
    dplyr::group_by(!!rlang::sym("row_var"))

  # if (pvalue_lines) {
  #   tabs <- tabs |> tab_pvalue_lines()
  # }

  tabs
}


# tabs_bind <- function(tabs) {
#
#   if (is.data.frame(tabs)) return(tabs)
#
#   if (!(is.list(tabs) & all(purrr::map_lgl(tabs, is_tab)) )) {
#     stop("tabs must be a list of tabxplor_tab (or a single data.frame)")
#   }
#
#   col_vars <- purrr::map(tabs, ~ tab_get_vars(.)$col_vars)
#   col_vars <- col_vars |>
#     purrr::map(~ .[!. %in% c("all_col_vars", "", "no") & !is.na(.)])
#   # longest_col_vars <- purrr::map_int(col_vars, length)
#   # longest_col_vars <-
#   #   dplyr::first(which(longest_col_vars == max(longest_col_vars, na.rm = TRUE)))
#   # longest_col_vars <- col_vars[[longest_col_vars]]
#   # same_col_vars <- col_vars |> purrr::map_lgl(~ all(. %in% longest_col_vars))
#
#   same_col_vars <- purrr::map2_lgl(col_vars, dplyr::lag(col_vars),
#                                    identical
#   )
#
#   same_col_vars <- cumsum(!same_col_vars)
#
#
#
#
#
#   same_color <-
#     purrr::map2(
#       tabs, col_vars,
#       ~ dplyr::select(
#         dplyr::ungroup(.x),
#         dplyr::where(
#           function(.var) is_fmt(.var) & !is_totcol(.var) &
#             get_col_var(.var) %in% .y) & -any_of(c("n"))
#       ) |>
#         purrr::map_chr(get_color)
#     )
#
#
#   if(!all(same_col_vars)) {
#     stop("tab_kable() can only be used with a list of tab if they have the same col_vars")
#   }
#
#   if (any(purrr::map_lgl(tabs, ~ length(tab_get_vars(.)$tab_vars) > 0 )) ) {
#     stop("tab_kable() can only be used with a list of tab if they have no tab_vars")
#   }
#
#   tabs <- tab_compact(tabs, pvalue_lines = TRUE)
#
#
#
# }



# # tab_kable_multi tests
# # tabs <- tab(pc18, CRITAGE, DIPLOM, wt = POND, pct = "row", color = "diff")
# tab(pc18, CRITAGE, DIPLOM, wt = POND, pct = "row", color = "diff") |>
#   tab_kable_multi()
# tab(pc18, CRITAGE, DIPLOM, wt = POND, pct = "row", color = "after_ci", chi2 = TRUE) |>
# tab_kable_multi()
# tab(pc18, CRITAGE, DIPLOM, wt = POND, pct = "col", color = "diff", chi2 = TRUE) |>
#   tab_kable_multi()
# tab(pc18, CRITAGE, DIPLOM, wt = POND, pct = "row", color = "diff", chi2 = TRUE) |>
#   tab_kable_multi()
# tab(pc18, CRITAGE, DIPLOM, wt = POND, pct = "row", color = "diff", ref = 2, chi2 = TRUE) |>
#   tab_kable_multi()
#
#
# tab_many(pc18, c(CRITAGE, SEXE), c(DIPLOM, REVENU4), wt = POND, pct = "row", color = "diff", levels = "first", chi2 = TRUE) |>
#   tab_kable_multi()
# tab_many(pc18, c(CRITAGE, SEXE), c(DIPLOM, REVENU4), wt = POND, pct = "row", color = "diff") |>
#   tab_kable_multi()
# tab_many(pc18, c(CRITAGE, SEXE), c(DIPLOM, REVENU4), wt = POND, pct = "row", color = "after_ci", ref = c(1, "tot"), chi2 = TRUE) |>
# tab_kable_multi()
# tab_many(pc18, c(CRITAGE, SEXE), c(DIPLOM, REVENU4), wt = POND, pct = "col", color = "diff", chi2 = TRUE, ref = c(3, 2)) |>
#   tab_kable_multi()
# tab_many(pc18, c(CRITAGE, SEXE), c(DIPLOM, REVENU4), wt = POND, pct = "row", color = "diff", chi2 = TRUE) |>
#   tab_kable_multi()
# tab_many(pc18, c(CRITAGE, SEXE), c(DIPLOM, REVENU4), wt = POND, pct = "row", color = "diff", ref = "auto", chi2 = TRUE) |>
#   tab_kable_multi()



#' Transform chi2 attribute table of a tabxplor_tab into rows with pvalues.
#'
#' @param tabs A tabxplor_tab (with chi2 table as attribute).
#'
#' @return A tabxplor_tab.
# @export
#
# @examples
# \donttest{
# forcats::gss_cat |>
#   tab_many(race, marital, pct = "row", color = "diff", add_n = FALSE) |>
#   tab_chi2() |>
#   tab_pvalue_lines()
# }
tab_pvalue_lines <- function(tabs) {
  subtext   <- get_subtext(tabs)
  tabs_chi2 <- get_chi2(tabs)
  are_chi2 <- nrow(tabs_chi2) > 0

  if(!are_chi2) return(tabs)

  groups   <- dplyr::groups(tabs)
  row_var  <- tab_get_vars(tabs)$row_var
  col_vars <- tab_get_vars(tabs)$col_vars_levels |> purrr::map_chr(first)
  col_vars <- purrr::set_names(names(col_vars), col_vars)
  col_vars <- col_vars[get_type(tabs[names(col_vars)]) != "mean"]
  tab_vars <- tab_get_vars(tabs)$tab_vars


  # are_chi2 <- all(purrr::map_int(tabs, ~nrow(get_chi2(.)))) > 0

  if (!"row_var" %in% names(tabs)) {
    tabs_chi2 <- tabs_chi2 |> dplyr::select(-"row_var")
  }

  tabs_pvalue_lines <- tabs_chi2 |>
    dplyr::filter(.data$`chi2 stats` == "pvalue") |>
    dplyr::rename(tidyselect::all_of(purrr::set_names("chi2 stats", row_var))) |>
    dplyr::mutate(!!rlang::sym(row_var) := forcats::as_factor(!!rlang::sym(row_var))) |>
    dplyr::mutate(dplyr::across(
      dplyr::where(is_fmt),
      ~ set_display(., "pvalue") |>
        dplyr::mutate(n = NA_integer_,
                      ci = 0,
                      ctr = 0,
                      diff = dplyr::if_else(.$pct > 0.05, -0.5, 0),
                      #in_totrow = TRUE,
                      digits = 2L)
    )) |>
    dplyr::rename(tidyselect::any_of(col_vars))

  # tabs_pvalue_lines <- tabs_pvalue_lines |>
  #   purrr::map_df(
  #     ~get_chi2(.) |>
  #       dplyr::filter(`chi2 stats` == "pvalue") |>
  #       dplyr::rename(levels = `chi2 stats`) |>
  #       dplyr::mutate(levels = forcats::as_factor(levels)) |>
  #       dplyr::mutate(dplyr::across(
  #         dplyr::where(is_fmt),
  #         ~ set_display(., "pvalue") |>
  #           dplyr::mutate(n = NA_integer_,
  #                         ci = 0,
  #                         ctr = 0,
  #                         diff = dplyr::if_else(.$pct > 0.05, -0.5, 0),
  #                         digits = 2L)
  #       ))
  #   )

  # tabs_pvalue_lines <- tabs_pvalue_lines |> dplyr::rename(tidyselect::any_of(col_vars))

  tabs <- # keep all attributes
    purrr::map2_df(
      tabs |> dplyr::bind_rows(tabs_pvalue_lines),
      tabs,
      ~ if (is_fmt(.x)) {vctrs::vec_restore(.x, .y) } else {.x}
    )

  tabs <- tabs |>
    dplyr::group_by(!!!rlang::syms(groups)) |>
    dplyr::arrange(.by_group = TRUE) |>
    dplyr::mutate(dplyr::across(
      dplyr::where(is_fmt),   #where(is_totcol) | any_of(c("n")),
      ~ dplyr::if_else(
        !is.na(.$display),
        true  = .,
        false = fmt0(first(.$display),
                     type = get_type(.)) |>
          dplyr::mutate(n = NA_integer_) #  in_totrow = TRUE
      ) |>
        vctrs::vec_restore(.)
    ))
  # filter(levels == "pvalue") |>
  # pull(Danser) |> vctrs::vec_data()
  # pull( `Total_ART_MONTAGES`) |> vctrs::vec_data()

  new_tab(tabs, subtext = subtext) |>
    dplyr::group_by(!!!rlang::syms(groups))
}







#' Print a tabxplor table as plot
#'
#' @param tabs A table made with \code{\link{tab}} or \code{\link{tab_many}}.
#' @param color_type  Set to \code{"text"} to color the text, \code{"bg"} to color the
#' background. By default it takes \code{getOption("tabxplor.color_style_type")}.
#' @param theme By default, a white table with black text, Set to \code{"dark"} for a
#' black table with white text.
#' @param html_24_bit Use 24bits colors palettes for html tables : set to `"green_red"`
#' or `"blue_red"`. Only with `mode = "color_code"` (not `mode = "crayon"`) and
#' `theme = "light`. Default to \code{getOption("tabxplor.color_html_24_bit")}.
#' @param color_legend Print colors legend below the table ?
#' @param caption The table caption.
#' @param wrap_rows By default, rownames are wrapped when larger than 30 characters.
#' @param wrap_cols By default, colnames are wrapped when larger than 12 characters.
#' @param whitespace_only Set to `FALSE` to wrap also on non whitespace characters.
# @param unbreakable_spaces Set to `FALSE` to keep normal spaces in text (auto-break).
#' @return A \code{\link[ggplot2]{ggplot}} object to be printed in the
#' `RStudio` Plots pane or exported as image, using \code{\link[ggpubr]{ggtexttable}}.
#' @export
#'
#' @examples
#' \donttest{
#' tab(forcats::gss_cat, race, marital, pct = "row", color = "diff") |>
#'   tab_plot()
#' }
#'
tab_plot <- function(tabs,
                     theme = c("light", "dark"), color_type = NULL, html_24_bit = NULL,
                     color_legend = TRUE, caption = NULL,
                     wrap_rows = 35, wrap_cols = 14, # unbreakable_spaces = TRUE
                     whitespace_only = TRUE) {
  if (!requireNamespace("ggpubr", quietly = TRUE)) {
    stop(paste0("Package \"ggpubr\" needed for this function to work. ",
                "You can install it with : install.packages('ggpubr')"),
         call. = FALSE)
  }
  if (!requireNamespace("gtable", quietly = TRUE)) {
    stop(paste0("Package \"gtable\" needed for this function to work. ",
                "You can install it with : install.packages('gtable')"),
         call. = FALSE)
  }
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop(paste0("Package \"ggplot2\" needed for this function to work. ",
                "You can install it with : install.packages('ggplot2')"),
         call. = FALSE)
  }
  if (!requireNamespace("cowplot", quietly = TRUE)) {
    stop(paste0("Package \"cowplot\" needed for this function to work. ",
                "You can install it with : install.packages('cowplot')"),
         call. = FALSE)
  }
  #theme <- if (is.null(theme)) { getOption("tabxplor.color_style_theme") } else { theme }
  color_type <-
    if (is.null(color_type)) { getOption("tabxplor.color_style_type") } else {color_type}

  html_24_bit <-
    if (is.null(html_24_bit)) {getOption("tabxplor.color_html_24_bit")} else {html_24_bit}

  row_var  <- tab_get_vars(tabs)$row_var
  tab_vars <- tab_get_vars(tabs)$tab_vars
  subtext  <- get_subtext(tabs) %>% purrr::discard(. == "")

  new_group <- tabs %>% #dplyr::group_by(dplyr::across(where(is.factor))) %>%
    dplyr::group_indices()
  new_group <- which(new_group != dplyr::lead(new_group, default = max(new_group) + 1))


  tabs <- tabs |> dplyr::ungroup() |> dplyr::select(-tidyselect::all_of(tab_vars))

  tabs <- tabs |>
    tab_wrap_text(wrap_rows = wrap_rows, wrap_cols = wrap_cols, exdent = 1,
                  whitespace_only = whitespace_only, unbreakable_spaces = FALSE)


  color_cols     <- get_color(tabs)
  fmt_no_colors  <- purrr::map_lgl(tabs, is_fmt) &
    (color_cols %in% c("", "no") | is.na(color_cols))
  fmt_no_colors  <- names(fmt_no_colors)[fmt_no_colors]
  color_cols     <- which(!color_cols %in% c("", "no") & !is.na(color_cols))
  fmt_cols <- which(purrr::map_lgl(tabs, is_fmt))
  color_cols_fmt <- names(color_cols)[names(color_cols) %in% names(fmt_cols)]

  other_cols <- which(purrr::map_lgl(tabs, ~ !is_fmt(.)))

  totcols     <- which(is_totcol(tabs))
  totrows     <- which(is_totrow(tabs))
  no_totrows  <- which(!is_totrow(tabs))

  new_col_var <- get_col_var(tabs)
  new_col_var[names(other_cols)] <- names(other_cols)
  new_col_var <- which(new_col_var != dplyr::lead(new_col_var, default = "._at_the_end"))

  # ????
  refref <- purrr::map_dfr(tabs[fmt_cols] , ~ get_reference(., mode = "all_totals") )
  refref2 <- refref |> dplyr::select(-where(all), -where(~ !any(.)))
  refs2 <- which(rowSums(refref2) == ncol(refref2))
  #refs2 <- tabs[[fmt_cols[1]]] %>% get_reference(mode = "all_totals") %>% which()
  # refs2 <- which(rowSums(refref) == length(fmt_cols))

  refref <- purrr::map_dfr(tabs[fmt_cols] , ~ get_reference(., mode = "all_totals") )



  refs3 <- refref |> dplyr::select(dplyr::where(all)) |> names()


  text_color  <- dplyr::if_else(theme[1] == "light", "#000000", "#FFFFFF")
  grey_color  <- dplyr::if_else(theme[1] == "light", "#888888", "#BBBBBB")
  grey_color2 <- dplyr::if_else(theme[1] == "light", "#111111", "#EEEEEE")

  references <- tabs[fmt_cols] %>%
    purrr::map(~ get_reference(., mode = "all_totals") %>%
                 dplyr::if_else(true      = text_color,
                                false     = "no_color") %>%
                 list() %>% purrr::set_names(text_color)
    )

  color_selection <- references




  if (length(color_cols_fmt) != 0) {
    color_selection[color_cols_fmt] <- purrr::map(tabs[color_cols], fmt_color_selection)

    color_styles <- purrr::pmap(list(color_selection[color_cols_fmt],
                                     get_color(tabs)[color_cols_fmt],
                                     get_type(tabs)[color_cols_fmt]
    ),
    ~ select_in_color_style(
      names(..1),
      pct_diff =  ..2 %in% c("diff", "diff_ci", "after_ci") &
        !..3 %in% c("n", "mean")
    ))

    color_styles <- purrr::map(color_styles,
                               ~ get_color_style(mode = "color_code",
                                                 type = color_type[1],
                                                 theme = theme[1],
                                                 html_24_bit = html_24_bit[1])[.])

    color_selection[color_cols_fmt] <- color_selection[color_cols_fmt] %>%
      purrr::map2(color_styles, ~ purrr::set_names(.x, .y)) %>%
      purrr::map(~ purrr::imap(., ~ dplyr::if_else(condition = .x,
                                                   true      = .y,
                                                   false     = "no_color")) ) %>%
      purrr::map2(references[color_cols_fmt], ~ c(.x, .y) %>%
                    purrr::reduce(~ dplyr::if_else(.x == "no_color", .y, .x)) %>%
                    stringr::str_replace(., "no_color", grey_color) %>%
                    tidyr::replace_na(grey_color)
      )
  }

  if (length(fmt_no_colors) != 0) {
    color_selection[fmt_no_colors] <- color_selection[fmt_no_colors] %>%
      purrr::map(~ purrr::flatten_chr(.) %>%
                   stringr::str_replace(., "no_color", grey_color2) %>%
                   tidyr::replace_na(grey_color2)
      )
  }


  if (length(other_cols) != 0) {
    color_selection <-
     dplyr::bind_cols(as.list(
      dplyr::mutate(tabs[other_cols], dplyr::across(tidyselect::everything(),
                                                    ~ text_color)),
    ),
    color_selection
    )

  } else {
    color_selection <- color_selection |> dplyr::bind_cols()
  }

  face_selection <- color_selection |>
    dplyr::mutate(dplyr::across(
      dplyr::everything(),
      ~ dplyr::if_else(
        !. %in% c(text_color, grey_color, grey_color2) |
          #dplyr::cur_column() %in% names(totcols) |
          dplyr::row_number() %in% refs2 | dplyr::cur_column() %in% refs3,
        true  = "bold",
        false = "plain")
    ))

  tabs_gg <- tabs |>
    dplyr::mutate(
      dplyr::across(
        where(is_fmt),
        ~ format(., special_formatting = TRUE)
      ),
      dplyr::across( # otherwise, unbreakable spaces fail in some graphic devices
        where(is.factor),
        ~ forcats::fct_relabel(., ~ stringr::str_replace_all(., unbrk, " "))
      ),
      dplyr::across( # otherwise, unbreakable spaces fail in some graphic devices
        where(is.character),
        ~ stringr::str_replace_all(., unbrk, " ")
      ),
      # # unbreakable space at the starting of names, otherwise doesn't fit with hjust = "right"
      # dplyr::across(
      #   1,
      #   ~ forcats::fct_relabel(., ~ paste0(paste0(rep(unbrk, 4), collapse = ""),
      #                                      .))
      # )
    ) |>

    ggpubr::ggtexttable(
      rows = NULL, # base_size = 11,
      theme = ggpubr::ttheme("blank",
                             padding = grid::unit(c(4, 3), "mm"), # c(h, v)
                             # rownames.style = ggpubr::rownames_style(
                             #   color = "black", #face = "plain", #parse = TRUE,
                             #   size      = 11,
                             #   fill      = "white", #c("grey95", "grey90"),
                             #   linewidth = 0,
                             #   linecolor = "black",
                             #
                             #   hjust = 0, x = 0.95 # right ajust
                             #   ),
                              tbody.style = ggpubr::tbody_style(
                               color = "black", #face = "plain", #parse = TRUE,
                               size      = 11,
                               fill      = "white", #c("grey95", "grey90"),
                               linewidth = 0,
                               linecolor = "black",

                               hjust = 0.98, x = 0.95 # right ajust
                             )),
    )

  # tabs |>
  #   dplyr::mutate(dplyr::across(where(is_fmt), format)) |>
  #   ggpubr::ggtexttable(
  #     rows = NULL, theme = ggpubr::ttheme("blank"),
  #   )

  # c("default", "blank", "classic", "minimal", "light",
  #   "lBlack", "lBlue", "lRed", "lGreen", "lViolet", "lCyan", "lOrange", "lBlackWhite", "lBlueWhite", "lRedWhite", "lGreenWhite", "lVioletWhite", "lCyanWhite", "lOrangeWhite",
  #   "mBlack", "mBlue", "mRed", "mGreen", "mViolet", "mCyan", "mOrange", "mBlackWhite", "mBlueWhite", "mRedWhite", "mGreenWhite", "mVioletWhite", "mCyanWhite", "mOrangeWhite"
  #   )




if (color_type == "text") {

    for(j in 1:ncol(tabs)) {
      for(i in 1:nrow(tabs)) {
        tabs_gg <- tabs_gg |> ggpubr::table_cell_font(
          row    = i + 1,
          column = j,
          color  = color_selection[[j]][[i]],
          face   = face_selection[[j]][[i]]
        )
      }
    }

  } else {
    bg_color_selection  <- color_selection %>%
      purrr::map_dfc(~ stringr::str_replace_all(., text_color, "none") %>%
                       stringr::str_replace_all(grey_color, "none")        )

    txt_color_selection <- color_selection %>%
      purrr::map_dfc(~ dplyr::if_else(stringr::str_detect(., text_color) |
                                        stringr::str_detect(., grey_color) |
                                        stringr::str_detect(., grey_color2),
                                      true  = .,
                                      false = text_color)               )

    for(j in 1:ncol(tabs)) {
      for(i in 1:nrow(tabs)) {
        tabs_gg <- tabs_gg |>
          ggpubr::table_cell_font(
            row    = i + 1,
            column = j,
            color  = txt_color_selection[[j]][[i]],
            face   = face_selection[[j]][[i]]
          ) |>
          ggpubr::table_cell_bg(
            row    = i + 1,
            column = j,
            fill   = ifelse(bg_color_selection[[j]][[i]] != "none",
                            bg_color_selection[[j]][[i]],
                            dplyr::if_else(theme[1] == "light", "white", "black")


            ),
            linewidth  = 0

            )

      }
    }
    # tabs_gg

  }
# tabs_gg



  tabs_gg <- tabs_gg |>
    ggpubr::tab_add_border(from.row = 1, linetype = 1, linewidth = 2, linecolor = "black") |>
    #ggpubr::thead_add_border(linetype = 1, linewidth = 2, linecolor = "black") |>
    ggpubr::tab_add_hline(
      at.row = unique(c(1, totrows, totrows + 1, new_group)), row.side = "bottom",
      linetype = 1, linewidth = 2, linecolor = "black",
    ) |>
    # ggpubr::tab_add_hline(
    #   at.row = totrows, row.side = "top",
    #   linetype = 1, linewidth = 2, linecolor = "black",
    # ) |>
    ggpubr::tab_add_vline(
      at.column = unique(c(new_col_var, totcols - 1)), column.side = "right",
      linetype = 1, linewidth = 2, linecolor = "black",
    ) |>
    ggpubr::tab_add_vline(
      at.column = unique(c(other_cols, totcols)), column.side = "left",
      linetype = 1, linewidth = 2, linecolor = "black",
     ) #|>
    # ggpubr::tab_add_vline(
    #   at.column = totcols - 1L, column.side = "right",
    #   linetype = 1, linewidth = 2, linecolor = "black",
    # )

    ## bold
    # kableExtra::row_spec(refs2, bold = TRUE) %>%

    ## wrap
    # kableExtra::column_spec(fmt_cols, extra_css = "white-space: nowrap;") %>%



if (color_legend & length(color_cols) != 0) {

  color_legend <- suppressWarnings(tab_color_legend(tabs,
                                   mode = "html",
                                   html_type   = color_type[1],
                                   html_theme  = theme[1],
                                   html_24_bit = html_24_bit[1],
                                   text_color  = text_color,
                                   grey_color  = grey_color)) |>
    stringr::str_split("</span>|<span style=\"") |>
    purrr::imap_dfr(
      ~ tibble::tibble(n = as.integer(.y), base = .x)
    ) |>
    dplyr::mutate(
      base = stringr::str_remove_all(.data$base, "<b>|</b>") |>
        #stringr::str_remove_all("^;") |>
        stringr::str_squish(), # |>
        #stringr::str_replace(":$", " "),
      color = stringr::str_extract(.data$base, "^color: rgba.[^\\)]+") |>
        stringr::str_remove_all("color: rgba\\("),
      text  = stringr::str_remove(.data$base, '^.+!important;\\" >'),

      # base = stringr::str_remove_all(base, "</b>; "),
    ) |>
    dplyr::filter(!.data$base %in% c("", ";")) |>
    dplyr::mutate(text = stringr::str_replace_all(.data$text, "; *;", ";")) |>
    dplyr::group_by(!!rlang::sym("n")) |>
    dplyr::mutate(bold = stringr::str_detect(.data$base, "color: rgba") &
                    dplyr::row_number() != 1 ) |>
    dplyr::ungroup() |>
    tidyr::separate(col = .data$color, into = c("c1", "c2", "c3", "c4"), sep = ", ") |>
    dplyr::mutate(
      text  = dplyr::if_else(.data$bold, paste0(.data$text, " ;"), .data$text),
      dplyr::across(tidyselect::all_of(c("c1", "c2", "c3", "c4")),
             ~dplyr::if_else(!is.na(.), as.integer(.), 0L)),
      color      = grDevices::rgb(.data$c1/255, .data$c2/255, .data$c3/255),
      #bold_start = bold & !dplyr::lag(bold, default = FALSE),
      #bold_stop  = bold & !dplyr::lead(bold, default = FALSE)
    ) |>
    dplyr::select("n", "text", "color") |> # bold_start, bold_stop,
    dplyr::mutate(
      dplyr::across( # otherwise, unbreakable spaces fail in some graphic devices
        where(is.character),
        ~ stringr::str_replace_all(., unbrk, " ")
      ),
    ) |>
    dplyr::group_by(!!rlang::sym("n")) |>
    dplyr::group_split(.keep = FALSE)



  # color_legend <- color_legend |>
  #   purrr::map_dfr(
  #     ~ purrr::map_dfr(unique(.$color), function(.color)
  #       . |>
  #         dplyr::mutate(
  #           in_color = color %in% .color,
  #           group    = cumsum(in_color != dplyr::lag(in_color, default = FALSE))
  #         ) |>
  #         dplyr::group_by(group) |>
  #         dplyr::summarise(
  #           in_color = dplyr::first(in_color),
  #           text     = paste(.data$text, collapse = " "),
  #           .groups  = "drop"
  #         ) |>
  #         dplyr::mutate(text = dplyr::if_else(in_color,
  #                                             true  = paste0('"', .data$text, '"'),
  #                                             false = paste0('phantom("', .data$text, '")' ))
  #         ) |>
  #         dplyr::summarise(
  #           text = paste0("bold(", paste(.data$text, collapse = " * "),")") |>
  #             stringr::str_squish(),
  #         ) |>
  #         dplyr::mutate(
  #           color = .color,
  #           n     = dplyr::first(.$n),
  #           .before = 1
  #         )
  #
  #     )
  #   )

  # if (length(subtext) != 0) {
  #   color_legend <- list(
  #     color_legend,
  #     tibble::tibble(color = text_color,
  #                    text  = subtext, # paste0('"', subtext, '"'),
  #                    # n     = 1:length(subtext)
  #     ) |>
  #       dplyr::rowwise() |>
  #       dplyr::group_split()
  #   ) |>
  #     purrr::flatten()
  # }


# # If no color legend, just subtext
#   } else if (length(subtext) != 0) {
#     color_legend <-
#         tibble::tibble(color = text_color,
#                        text  = subtext, # paste0('"', subtext, '"'),
#                        # n     = 1:length(subtext)
#         ) |>
#       dplyr::rowwise() |>
#       dplyr::group_split()

  } else {
    color_legend <- NULL
  }

  if (length(color_legend) != 0) {
    #if (nrow(color_legend) != 0) {

      # color_legend_plot <- color_legend |>
      #   dplyr::group_by(!!rlang::sym("n")) |>
      #   dplyr::group_split() |>
      #   purrr::map(
      #     ~ dplyr::mutate(., n = max(.data$n) - .data$n) |>
      #       ggplot2::ggplot(ggplot2::aes(y     = .data$n,
      #                                    label = .data$text,
      #                                    color = .data$color)) +
      #       ggplot2::geom_text(x = 0, parse = TRUE, hjust = 0, size = 3.5) +
      #       ggplot2::scale_color_identity() +
      #       ggplot2::theme_void() #+
      #       #ggplot2::theme()
      #   )

      tab_legend <- color_legend |>
        purrr::map_dfr(
          ~ dplyr::select(., "text") |>
            dplyr::mutate(name = dplyr::row_number()) |>
            tidyr::pivot_wider( names_from = "name", values_from = "text")
        )

      tab_legend_color <- color_legend |>
        purrr::map_dfr(
          ~ dplyr::select(., "color") |>
            dplyr::mutate(name = dplyr::row_number()) |>
            tidyr::pivot_wider( names_from = "name", values_from = "color")

        )

      tab_legend_plot <- tab_legend |>
        ggpubr::ggtexttable(
          rows = NULL,
          theme = ggpubr::ttheme("blank",
                                 padding = grid::unit(c(7, 4), "mm"), # c(h, v)
                                 colnames.style = ggpubr::colnames_style(
                                   color = "white",
                                   size = 0,
                                   fill = "white",
                                   linewidth = 0
                                 ),
                                 tbody.style = ggpubr::tbody_style(
                                   color     = "black", #face = "plain", #parse = TRUE,
                                   size      = 8,
                                   fill      = "white", #c("grey95", "grey90"),
                                   linewidth = 0,
                                   linecolor = "black",

                                   hjust = 0.98, x = 0.95 # right ajust
                                 )),
        )


      for(i in 1:nrow(tab_legend)) {
        for(j in 1:ncol(tab_legend)) {
          tab_legend_plot <- tab_legend_plot |> ggpubr::table_cell_font(
            row    = i + 1,
            column = j,
            color  = tab_legend_color[[j]][[i]],
            face   = "bold"
          )
        }
      }


      cowplot::set_null_device("png") # "pdf", "png", "cairo", "agg"

      tabgrob    <- get_tablegrob(tabs_gg) |> justify_grob()
      legendgrob <- get_tablegrob(tab_legend_plot) |> justify_grob()

      tabgrob <- gtable::gtable_add_rows(
        tabgrob,
        heights = grid::grobHeight(legendgrob), #+
        #ggplot2::unit(1, "line"),
        pos = -1
      )
      tabgrob <- gtable::gtable_add_grob(tabgrob, legendgrob,
                                         t = nrow(tabgrob),
                                         b = nrow(tabgrob),
                                         l = 1,
                                         r = ncol(tabgrob))
      tabs_gg <- tab_return_same_class_as_input(tabgrob, input = tabs_gg)


      # dim_gg     <- tab_get_wrapped_dimensions(tabs)
      # dim_legend <- tab_get_wrapped_dimensions(tab_legend)
      #
      # tabgrob    <- get_tablegrob(tabs_gg) |> justify_grob()
      # legendgrob <- get_tablegrob(tab_legend_plot) |> justify_grob()
      #
      # tabs_gg    <- tab_return_same_class_as_input(tabgrob, input = tabs_gg)
      # tab_legend_plot <- tab_return_same_class_as_input(legendgrob, input = tab_legend_plot)
      #
      # tabs_gg <-
      #   ggpubr::ggarrange(
      #     tabs_gg,
      #     tab_legend_plot,
      #     ncol = 1L,
      #     #align = "v",
      #     heights = c(dim_gg[2], dim_legend[2] - 1L)
      #   )



      # for (i in 1:length(color_legend_plot)) {
      #   # ggpubr::tab_add_footnote
      #   tabgrob    <- get_tablegrob(tabs_gg)
      #   legendgrob <- cowplot::as_grob(color_legend_plot[[i]])
      # #
      #   tabgrob <- gtable::gtable_add_rows(
      #     tabgrob,
      #     heights = grid::grobHeight(legendgrob) +
      #       ggplot2::unit(1 + dplyr::if_else(i == 1, 0.5, 0), "line"),
      #     pos = -1
      #   )
      #   tabgrob <- gtable::gtable_add_grob(tabgrob, legendgrob, t = nrow(tabgrob),
      #                                      b = nrow(tabgrob), l = 1, r = ncol(tabgrob))
      #   tabs_gg <- tab_return_same_class_as_input(tabgrob, input = tabs_gg)
      # }
    #}
  }


  # Align the whole plot top left
  tabgrob <- get_tablegrob(tabs_gg)
  tabgrob <- justify_grob(tabgrob)
  tabs_gg <- tab_return_same_class_as_input(tabgrob, input = tabs_gg)

  # if (length(color_legend) != 0) {
  #   if (nrow(color_legend) != 0) {
  #     tabs_gg$color_palette <- color_palette
  #   }
  # }

  # tabs_gg$height <- grid::grobHeight(tabgrob) |> grid::convertHeight(unitTo = "points") # |> as.double()
  # tabs_gg$width  <- grid::grobWidth(tabgrob)  |> grid::convertWidth (unitTo = "points") # |> as.double()
  # # seem not ok...

  return(tabs_gg)
}










#' @keywords internal
tab_kable_print_tooltip <- function(x, popover = FALSE) {

  ref     <- get_reference(x, mode = "cells")
  totcol  <- is_totcol(x)
  totrows <- is_totrow(x)
  tottabs <- is_tottab(x)
  type    <- get_type(x)

  diff     <- get_diff(x)
  rr       <- get_rr(x)
  or       <- get_or(x)
  digits   <- get_digits(x)


  ok_diff  <- !is.na(diff) & !((totcol | totrows) & get_pct(x) == 1)
  out_diff <- dplyr::case_when(
    ref & any(ok_diff)       ~ "diff: ref",
    ok_diff & type == "mean" ~ paste0("diff: ", stringi::stri_unescape_unicode("\\u00d7"), #multiplication sign
                                      format(set_display(x, "diff")) ),
    ok_diff                  ~ paste0("diff: ",      format(set_display(x, "diff")) ),
    # ok_diff & diff >= 0      ~ paste0("diff: ", "+", format(set_display(x, "diff")) ),
    # ok_diff & diff < 0       ~ paste0("diff: ",      format(set_display(x, "diff")) ),
    TRUE                     ~ ""
  )

  ci_type  <- get_ci_type(x)
  ci_start <- switch(ci_type, "cell" = "ci: ", "")
  out_ci   <- dplyr::if_else(
    condition = !is.na(get_ci(x)),
    true      = paste0(ci_start, format(set_display(x, "ci") %>%
                                          set_digits(dplyr::if_else(digits == 0L,
                                                                    digits + 1L,
                                                                    digits))) ),
    false     = ""
  )

  out_diff <- switch(ci_type,
                     "diff" = paste0(out_diff, " ", stringr::str_remove(out_ci, "%$")),
                     out_diff)
  out_ci   <- switch(ci_type, "cell" = out_ci, "")

  out_pct <- dplyr::if_else(
    condition = type %in% c("col", "row", "all", "all_tabs") &
      !is.na(get_pct(x)) & !get_display(x) %in% c("pct", "pct_ci"),
    true      = format(set_display(x, "pct")),
    false     = ""
  )

  out_mean <- dplyr::if_else(
    condition = type == "mean" & !is.na(get_mean(x)) & !get_display(x) %in% c("mean", "mean_ci"),
    true      = format(set_display(x, "mean")),
    false     = ""
  )

  out_sd <- dplyr::if_else(
    condition = type == "mean" & !is.na(get_var(x)) & !get_display(x) == "var",
    true      = dplyr::if_else(
      x$var >= 0,
      true  = paste0("sd: ", format(set_display(set_digits(set_var(x, suppressWarnings(sqrt(x$var))), x$digits + 1L), "var"))),
      false = ""),
    false     = ""
  )

  out_rr <- dplyr::if_else(
    condition = type %in% c("col", "row") & !is.na(get_rr(x)) & !get_display(x) == "rr",
    true      = paste0("rr: ", format(set_display(x, "or")) ),
    false     = ""
  )

  out_or <- dplyr::if_else(
    condition = type %in% c("col", "row") & !is.na(get_or(x)) & !get_display(x) %in% c("or", "OR", "or_pct", "OR_pct"),
    true      = paste0("OR: ", format(set_display(x, "or")) ),
    false     = ""
  )


  mctr <- if (get_comp_all(x)) { totrows & tottabs & !totcol } else { totrows & !totcol }
  ctr_start <- dplyr::if_else(mctr,"mean_ctr: ", "contrib: ")
  out_ctr <- dplyr::if_else(condition = !is.na(get_ctr(x)) & !get_ctr(x) == Inf &
                              !((totcol | totrows) & get_pct(x) == 1 ),
                            true      = paste0(ctr_start, format(set_display(x, "ctr")) %>%
                                                 stringr::str_remove("^-")),
                            false     = "")

  out_n <- dplyr::if_else(condition = !is.na(get_n(x)) & !get_display(x) == "n",
                          true      = paste0("n: ", format(set_display(x, "n")) ),
                          false     = "")

  out <- paste(out_pct, out_mean, out_sd, out_diff, out_rr, out_or,
               out_ci, out_ctr, out_n, sep = " ; ") %>%
    stringr::str_replace_all(";  ; ", "; ") %>%
    stringr::str_replace_all(";  ; ", "; ") %>%
    stringr::str_replace_all(";  ; ", "; ") %>%
    stringr::str_remove("^ *; *") %>%
    stringr::str_remove(" *; *$") |>
    stringr::str_remove("NA *;")

  out[is.na(out) | out == "NA"] <- ""

  if (popover) out <- kableExtra::spec_popover(out, position = "left")

  out
}



#' Wrap column names and character/factor variables.
#' @param tabs A `tabxplor_tab` or a `tibble` .
#' @param wrap_rows By default, rownames are wrapped when larger than 30 characters.
#' @param wrap_cols By default, colnames are wrapped when larger than 12 characters.
#' @param exdent On the second lines or more, the number or characters to use for indentation.
#' @param whitespace_only Set to `FALSE` to wrap also on non whitespace characters.
#' @param unbreakable_spaces Set to `FALSE` to keep normal spaces in text (auto-break).
#' @param brk The string to use for linebreak : `\n` in text, but `<br>` in html.

#' @return The same `tabxplor_tab` or `tibble`.
#' @export
#'
#' @examples
#' \donttest{
#' tab(forcats::gss_cat, race, marital, pct = "row", color = "diff") |>
#'   tab_wrap_text(wrap_rows = 5L, wrap_cols = 8L)
#' }
#'
tab_wrap_text <- function(tabs, wrap_rows = 35L, wrap_cols = 15L, exdent = 1,
                          whitespace_only = TRUE, unbreakable_spaces = TRUE,
                          brk = "\n") {
  if (wrap_rows == Inf & wrap_cols == Inf) return(tabs)

  tabs <- tabs |>
    dplyr::rename_with(
      ~ stringr::str_wrap(., wrap_cols, exdent = 0, whitespace_only = whitespace_only) |>
        stringr::str_replace_all("\n", brk)
    ) |>
    dplyr::mutate(
      dplyr::across(
        where(is.factor),
        ~ forcats::fct_relabel(
          ., ~ stringr::str_wrap(.,
                                 width           = wrap_rows,
                                 exdent          = exdent,
                                 whitespace_only = whitespace_only) |>
            stringr::str_replace_all("\n", brk)
        )
      ),
      dplyr::across(
        where(is.character),
        ~ stringr::str_wrap(.,
                            width           = wrap_rows,
                            exdent          = exdent,
                            whitespace_only = whitespace_only) |>
          stringr::str_replace_all("\n", brk)
      )
    )

  if (unbreakable_spaces) {
    tabs <- tabs |>
      dplyr::rename_with(
        ~ stringr::str_replace_all(., " ", unbrk)
      ) |>
      dplyr::mutate(
        dplyr::across(
          where(is.factor),
          ~ forcats::fct_relabel(., ~ stringr::str_replace_all(., " ", unbrk) )
        ),
        dplyr::across(
        where(is.character),
        ~ stringr::str_replace_all(., " ", unbrk)
      ),

      )
  }

  return(tabs)
}



#' Get the number of actual rows and the max character length of a table after
#' being wrapped (count `\n` as a linebreak).
#' @param tabs A data.frame.
#' @param no_tab_vars For data.frame of class `tabxplor_tab`, remove `tab_vars`.
#' @param width_pad Number of characters lengths between columns.
#' @export
tab_get_wrapped_dimensions <- function(tabs, no_tab_vars = FALSE,
                                       width_pad = 4L) {

  if (no_tab_vars & is_tab(tabs)) {
    tab_vars <- tab_get_vars(tabs)$tab_vars
    tabs <- tabs |> dplyr::ungroup() |> dplyr::select(-tidyselect::all_of(tab_vars))
  }

  tabs_with_colnames <-
    dplyr::bind_rows(
      tibble::tibble(!!!purrr::set_names(names(tabs), names(tabs))),
      tabs |> # heigth depend on the number of line breaks in each column
        #dplyr::select(tidyselect::where(~ is.character(.) | is.factor(.))) |>
        dplyr::ungroup() |>
        dplyr::mutate(dplyr::across(
          tidyselect::everything(),
          format
        )),
      )

  height <- tabs_with_colnames |>
    dplyr::mutate(dplyr::across(
      tidyselect::everything(),
      ~ 1L + stringr::str_count(., "\n")
    )) |>
    dplyr::rowwise() |>
    dplyr::mutate(n = max(dplyr::c_across(cols = tidyselect::everything()))) |>
    dplyr::pull("n") |> sum()

  #length(get_subtext(tabs)) +

  #length(unique(get_color(tabs)[!get_color(tabs) %in% c("", "no")])) # color legend length

  width <- tabs_with_colnames |>
    purrr::map(
      ~ stringr::str_split(., "\n") |>
        purrr::flatten_chr() |>
        stringr::str_length() |>
        max()
    ) |>
    purrr::map_int(
      ~ max(. + width_pad)
    ) |>
    sum()

  c("width" = width, "height" = height)
}










#Methods for class tabxplor_tab ----------------------------------------------------------

# importFrom not needed when tabxplor import dplyr as a "Depends" package

#' group_by method for class tabxplor_tab
#' @importFrom dplyr group_by
#' @param .data A tibble of class \code{tabxplor_tab}.
#' @param ... Variables or computations to group by.
#' @param .add When \code{FALSE}, the default, \code{group_by()} will
#'   override existing groups. To add to the existing groups, use
#'   \code{.add = TRUE}.
#' @param .drop Drop groups formed by factor levels that don't appear in the
#'   data? The default is \code{TRUE} except when \code{.data} has been previously
#'   grouped with \code{.drop = FALSE}.

#' @method group_by tabxplor_tab
#' @return A tibble of class \code{tabxplor_grouped_tab}.
#' @export
group_by.tabxplor_tab <- function(.data,
                                  ...,
                                  .add = FALSE,
                                  .drop = dplyr::group_by_drop_default(.data)) {
  out <- NextMethod()
  groups <- dplyr::group_data(out)
  new_grouped_tab(out, groups,
                  subtext = get_subtext(.data), chi2 = get_chi2(.data))
}



#' arrange method for class tabxplor_tab
#' @importFrom dplyr arrange
#' @param .data A tibble of class tabxplor_tab.
#' @param ... <[`data-masking`][rlang::args_data_masking]> Variables, or
#'   functions of variables. Use `desc()` to sort a variable in descending
#'   order.
#' @param .by_group By default, will sort first by grouping variable.
#'   Set to `FALSE` to avoid this behaviour.
#' @param .by_totals By default, will put totals at the end of their group.
#'   Set to `FALSE` to avoid this behaviour.
#' @param .only_main_display By default, only the rows with the same display
#'   than the first row are arranged : if the first row of the group displays
#'   percentages, rows with n or pvalues are kept at the same place
#'   (typically, at the end of the group). Rows with the text `"row_pct"`, `"n"`
#'   or `"pvalue"` in the `row_var` name are also kept at the same place.
#'   Set to `FALSE` to avoid this behaviour.
#' @param .locale The locale to sort character vectors in.
#' @method arrange tabxplor_tab
#' @return A tibble of class \code{tabxplor__tab} or \code{tabxplor_grouped_tab}.
#' @export
 arrange.tabxplor_tab <-
  function(.data, ..., .by_group = TRUE, .by_totals = TRUE,
           .only_main_display = TRUE, .locale = NULL) {

    dots <- rlang::enquos(...)
    groups <- dplyr::groups(.data) #dplyr::group_data(.data)

    if (.by_totals) {
      .totrows <- is_totrow(.data)
      .data <- .data |>
        dplyr::select(-tidyselect::any_of(".totrows")) |>
        tibble::add_column(.totrows = .totrows)
      dots <- c(rlang::quo(.totrows), dots)

    }

    if (.only_main_display) {
      row_var <- tab_get_vars(.data)$row_var

      several_displays <- purrr::map_lgl(
        dplyr::select(dplyr::ungroup(.data), dplyr::where(is_fmt)),
        ~ length(unique(get_display(.))) > 1
      )
      several_displays <- names(several_displays)[several_displays]


      if (length(several_displays) > 1) {
        .secondary_display <-
          dplyr::select(.data, !!!groups, tidyselect::all_of(c(row_var)),
                        tidyselect::all_of(several_displays)) |>
          dplyr::transmute(
            secondary_display = dplyr::if_any(
              tidyselect::all_of(several_displays),
              ~ get_display(.) != dplyr::first(get_display(.))
            ) | !!rlang::sym(row_var) %in% c("row_pct", "n", "pvalue"),

            secondary_display = dplyr::if_else(.data$secondary_display,
                                               true  = dplyr::row_number(),
                                               false = 0L
            )
          ) |>
          dplyr::pull("secondary_display")

      } else {
        .secondary_display <-
          dplyr::select(.data, !!!groups, tidyselect::all_of(c(row_var)),
                        tidyselect::all_of(several_displays)) |>
          dplyr::transmute(
            secondary_display = dplyr::if_else(
              !!rlang::sym(row_var) %in% c("row_pct", "n", "pvalue"),
              true  = dplyr::row_number(),
              false = 0L
            )
          ) |>
          dplyr::pull("secondary_display")
      }

      .data <- .data |>
        dplyr::select(-tidyselect::any_of(".secondary_display")) |>
        tibble::add_column(.secondary_display = .secondary_display)
      dots <- c(rlang::quo(.secondary_display), dots)
    }

    if (.by_group) {
      dots <- c(rlang::quos(!!!dplyr::groups(.data)), dots)
    }

    out <-
      dplyr::arrange(.data = tibble::as_tibble(.data),
                     ... = !!!dots,
                     .by_group = FALSE,
                     .locale = .locale
      )

    if (.by_totals | .only_main_display) {
      out <- out |>
        dplyr::select(-tidyselect::any_of(c(".totrows", ".secondary_display")))
    }


    # out <- NextMethod()

    if (length(groups) > 0) out <- out |> dplyr::group_by(!!!groups)

    if (lv1_group_vars(out)) {
      new_tab(out, subtext = get_subtext(.data), chi2 = get_chi2(.data))

    } else {
      groups <- dplyr::group_data(out)
      new_grouped_tab(out, groups, subtext = get_subtext(.data), chi2 = get_chi2(.data))
    }

}
# tabs <- tab(forcats::gss_cat, race, marital, year, pct = "row", color = "diff")
# arrange(tabs, `Never married`)
# arrange(tabs, `Never married`, .by_group = FALSE)
# arrange(tabs, `Never married`, .by_totals = FALSE)
# arrange(tabs, `Never married`, .by_group = FALSE, .by_totals = FALSE)
# ungroup_tabs <- tab(forcats::gss_cat, race, marital, pct = "row", color = "diff")
# arrange(ungroup_tabs, `Never married`)
# arrange(ungroup_tabs, `Never married`, .by_group = FALSE)
# arrange(ungroup_tabs, `Never married`, .by_totals = FALSE)
# arrange(ungroup_tabs, `Never married`, .by_group = FALSE, .by_totals = FALSE)

#' rowwise method for class tabxplor_tab
#' @importFrom dplyr rowwise
#' @param data A tibble of class \code{tabxplor_tab}.
#' @param ... Variables to be preserved
#'   when calling \code{summarise()}. This is typically a set of variables whose
#'   combination uniquely identify each row.
#' @method rowwise tabxplor_tab
#' @return A tibble of class \code{tabxplor_grouped_tab} and \code{rowwise_df}.
#' @export
rowwise.tabxplor_tab <- function(data, ...) {
  out <- NextMethod()
  groups <- dplyr::group_data(out)
  out <- new_grouped_tab(out, groups,
                         subtext = get_subtext(data), chi2 = get_chi2(data))

  `class<-`(out, stringr::str_replace(class(out), "grouped_df", "rowwise_df"))
}




# (from vctrs documentation)
# The coercion methods for data frames operate in two steps:
# They check for compatible subclass attributes. In our case the tibble colour has to
# be the same, or be undefined.
# They call their parent methods, in this case tib_ptype2() and tib_cast() because we
# have a subclass of tibble. This eventually calls the data frame methods df_ptype2() and
# tib_ptype2() which match the columns and their types.

#' Coercion between two tab
#' @param x,y,to Subclasses of data frame.
#' @param ... For future extensions.
#' @param x_arg Argument names for x and y. These are used in error messages to inform
#' the user about the locations of incompatible types.
#' @param y_arg Argument names for x and y. These are used in error messages to inform
#' the user about the locations of incompatible types.
#' @param to_arg Argument names for x and to. These are used in error messages to inform
#' the user about the locations of incompatible types.
#'
#' @return A tibble of class \code{tabxplor_tab}.
#' @keywords internal
# @export
tab_cast <- function(x, to, ..., x_arg = "", to_arg = "") {
  out <- vctrs::tib_cast(x, to, ..., x_arg = x_arg, to_arg = to_arg)

  subtext     <- vctrs::vec_c(get_subtext(x), get_subtext(to)) %>% unique()
  if (length(subtext) > 1) subtext <- subtext[subtext != ""]
  chi2        <- vctrs::vec_rbind(get_chi2(x), get_chi2(to))

  new_tab(out, subtext = subtext, chi2 = chi2)
}

#' @rdname tab_cast
#' @return A tibble of class \code{tabxplor_tab}.
#' @keywords internal
# @export
tab_ptype2 <- function(x, y, ..., x_arg = "", y_arg = "") {
  out <- vctrs::tib_ptype2(x, y, ..., x_arg = x_arg, y_arg = y_arg)
  #colour <- df_colour(x) %||% df_colour(y)

  chi2        <- vctrs::vec_rbind(get_chi2(x), get_chi2(y))
  subtext     <- vctrs::vec_c(get_subtext(x), get_subtext(y)) %>% unique()
  if (length(subtext) > 1) subtext <- subtext[subtext != ""]

  new_tab(out, subtext = subtext, chi2 = chi2)
}


#Let's now implement the coercion methods, starting with the self-self methods.
#' @return A tibble of class \code{tabxplor_tab}.
#' @describeIn tab_cast find common ptype between tabxplor_tab and tabxplor_tab
#' @export
vec_ptype2.tabxplor_tab.tabxplor_tab <- function(x, y, ...) {
  tab_ptype2(x, y, ...)
}
#' @describeIn tab_cast convert tabxplor_tab to tabxplor_tab
#' @return A tibble of class \code{tabxplor_tab}.
#' @export
vec_cast.tabxplor_tab.tabxplor_tab <- function(x, to, ...) {
  tab_cast(x, to, ...)
}

# The methods for combining our class with tibbles follow the same pattern.
# For ptype2 we return our class in both cases because it is the richer type

#' @describeIn tab_cast find common ptype between tabxplor_tab and tbl_df
#' @export
#' @return A tibble of class \code{tabxplor_tab}.
vec_ptype2.tabxplor_tab.tbl_df <- function(x, y, ...) {
  tab_ptype2(x, y, ...)
}
#' @describeIn tab_cast find common ptype between tbl_df and tabxplor_tab
#' @return A tibble.
#' @export
vec_ptype2.tbl_df.tabxplor_tab <- function(x, y, ...) {
  tab_ptype2(x, y, ...)
}
#' @describeIn tab_cast convert tbl_df to tabxplor_tab
#' @return A tibble of class \code{tabxplor_tab}.
#' @export
vec_cast.tabxplor_tab.tbl_df <- function(x, to, ...) {
  tab_cast(x, to, ...)
}
#' @describeIn tab_cast convert tabxplor_tab to tbl_df
#' @return A tibble.
#' @export
vec_cast.tbl_df.tabxplor_tab <- function(x, to, ...) {
  vctrs::tib_cast(x, to, ...)
}

#' @describeIn tab_cast find common ptype between tabxplor_tab and data.frame
#' @return A tibble of class \code{tabxplor_tab}.
#' @export
vec_ptype2.tabxplor_tab.data.frame <- function(x, y, ...) {
  tab_ptype2(x, y, ...)
}
#' @describeIn tab_cast find common ptype between data.frame and tabxplor_tab
#' @return A data.frame.
#' @export
vec_ptype2.data.frame.tabxplor_tab <- function(x, y, ...) {
  tab_ptype2(x, y, ...)
}
#' @describeIn tab_cast convert data.frame to tabxplor_tab
#' @return A tibble of class \code{tabxplor_tab}.
#' @export
vec_cast.tabxplor_tab.data.frame <- function(x, to, ...) {
  tab_cast(x, to, ...)
}
#' @describeIn tab_cast convert tabxplor_tab to data.frame
#' @return A data.frame.
#' @export
vec_cast.data.frame.tabxplor_tab <- function(x, to, ...) {
  vctrs::df_cast(x, to, ...)
}




#Methods for class grouped_tab------------------------------------------------------------

# just modify the methodes currently used by dplyr class "grouped_df" (not relative to groups)
# .S3methods(class = "grouped_df")

# dplyr_col_modify      dplyr_reconstruct     dplyr_row_slice
# ungroup               distinct_        rename_     select_     summarise
# [                     [<-          [[<-
# cbind                 rbind  rowwise

#' ungroup method for class tabxplor_grouped_tab
#' @importFrom dplyr ungroup
#' @param x A tibble of class \code{tabxplor_grouped_tab}.
#' @param ... Variables to remove from the grouping.
#' @method ungroup tabxplor_grouped_tab
#' @return An object of class \code{tabxplor_tab} or \code{tabxplor_grouped_tab}.
#' @export
ungroup.tabxplor_grouped_tab <- function (x, ...)
{
  if (missing(...)) {
    new_tab(x, subtext = get_subtext(x), chi2 = get_chi2(x))
  }
  else {
    old_groups <- dplyr::group_vars(x)
    to_remove  <- tidyselect::vars_select(names(x), ...)
    new_groups <- setdiff(old_groups, to_remove)
    dplyr::group_by(x, !!!rlang::syms(new_groups))
  }
}

#' @keywords internal
lv1_group_vars <- function(tabs) {
  dplyr::n_groups(tabs) <= 1

  #groupvars <- dplyr::group_vars(tabs)
  # all(purrr::map_lgl(groupvars,
  #                ~ nlevels(forcats::fct_drop(dplyr::pull(tabs, .))) == 1)) |
  #   length(groupvars) == 0
}


#' dplyr_row_slice method for class tabxplor_grouped_tab
#' @importFrom dplyr dplyr_row_slice
#' @method dplyr_row_slice tabxplor_grouped_tab
#' @param data A data frame.
#' @param i A numeric or logical vector that indexes the rows of \code{.data}.
#' @param ... Future parameters.
#' @return An object of class \code{tabxplor_grouped_tab}.
#' @export
dplyr_row_slice.tabxplor_grouped_tab <- function(data, i, ...) {
  out <- NextMethod()
  if (lv1_group_vars(out)) {
    new_tab(out, subtext = get_subtext(data), chi2 = get_chi2(data))
  } else {
    groups <- dplyr::group_data(out)
    new_grouped_tab(out, groups, subtext = get_subtext(data), chi2 = get_chi2(data))
  }
}
# dplyr:::dplyr_row_slice.grouped_df

#' dplyr_col_modify method for class tabxplor_grouped_tab
#' @importFrom dplyr dplyr_col_modify
#' @method dplyr_col_modify tabxplor_grouped_tab
#' @param data A data frame.
#' @param cols A named list used modify columns. A \code{NULL} value should remove
#'   an existing column.
#' @return An object of class \code{tabxplor_grouped_tab}.
#' @export
dplyr_col_modify.tabxplor_grouped_tab <- function(data, cols) {
  out <- NextMethod()
  if (lv1_group_vars(out)) {
    new_tab(out, subtext = get_subtext(data), chi2 = get_chi2(data))
  } else {
    groups <- dplyr::group_data(out)
    new_grouped_tab(out, groups, subtext = get_subtext(data), chi2 = get_chi2(data))
  }
}
# dplyr:::dplyr_col_modify.grouped_df

#' dplyr_reconstruct method for class tabxplor_grouped_tab
#' @importFrom dplyr dplyr_reconstruct
#' @method dplyr_reconstruct tabxplor_grouped_tab
#' @param data A data frame.
#' @param template Template to use for restoring attributes
#' @return An object of class \code{tabxplor_grouped_tab}.
#' @export
dplyr_reconstruct.tabxplor_grouped_tab <- function(data, template) {
  out <- NextMethod()
  if (lv1_group_vars(out)) {
    new_tab(out, subtext = get_subtext(data), chi2 = get_chi2(data))
  } else {
    groups <- dplyr::group_data(out)
    new_grouped_tab(out, groups, subtext = get_subtext(data), chi2 = get_chi2(data))
  }
}
# dplyr:::dplyr_reconstruct.grouped_df

#' subset method for class tabxplor_grouped_tab
#' @param x A tabxplor_grouped_tab object.
#' @param i,j,... Indices
#' @param drop For matrices and arrays. If TRUE the result is coerced to the lowest
#' possible dimension (see the examples). This only works for extracting elements,
#' not for the replacement.
#' @usage "x[i]  ;  x[i, j, ... , drop = TRUE]"
#' @method `[` tabxplor_grouped_tab
#' @return An object of class \code{tabxplor_grouped_tab}.
#' @export
`[.tabxplor_grouped_tab` <- function(x, i, j, drop = FALSE) {
  out <- NextMethod()
  if (lv1_group_vars(out)) {
    new_tab(out, subtext = get_subtext(x), chi2 = get_chi2(x))
  } else {
    groups <- dplyr::group_data(out)
    new_grouped_tab(out, groups, subtext = get_subtext(x), chi2 = get_chi2(x))
  }
}
# dplyr:::`[.grouped_df`

# #' @rdname `[.tabxplor_grouped_tab`
# `[` <- `[.tabxplor_grouped_tab`


#' set subset method for class tabxplor_grouped_tab
#' @param x A tabxplor_grouped_tab object.
#' @param i,j,... Indices.
#' @param value The new value.
#' @usage "x[i] <- value  ;   x[i, j, ...] <- value"
#' @method `[<-` tabxplor_grouped_tab
#' @return An object of class \code{tabxplor_grouped_tab}.
#' @export
`[<-.tabxplor_grouped_tab` <- function(x, i, j, ..., value) {
  out <- NextMethod()
  if (lv1_group_vars(out)) {
    new_tab(out, subtext = get_subtext(x), chi2 = get_chi2(x))
  } else {
    groups <- dplyr::group_data(out)
    new_grouped_tab(out, groups, subtext = get_subtext(x), chi2 = get_chi2(x))
  }
}
# dplyr:::`[<-.grouped_df`

# #' @rdname `[<-.tabxplor_grouped_tab`
# `[<-` <- `[<-.tabxplor_grouped_tab`

#' set sub-subset method for class tabxplor_grouped_tab
#' @param x A tabxplor_grouped_tab object.
#' @param ... Indices
#' @param value The new value.
#' @usage "x[[...]] <- value"
#' @method `[[<-` tabxplor_grouped_tab
#' @return An object of class \code{tabxplor_grouped_tab}.
#' @export
`[[<-.tabxplor_grouped_tab` <- function(x, ..., value) {
  out <- NextMethod()
  if (lv1_group_vars(out)) {
    new_tab(out, subtext = get_subtext(x), chi2 = get_chi2(x))
  } else {
    groups <- dplyr::group_data(out)
    new_grouped_tab(out, groups, subtext = get_subtext(x), chi2 = get_chi2(x))
  }
}
# dplyr:::`[[<-.grouped_df`

# #' @rdname `[[<-.tabxplor_grouped_tab`
# `[[<-` <- `[[<-.tabxplor_grouped_tab`

#' rowwise method for class tabxplor_grouped_tab
#' @importFrom dplyr rowwise
#' @method rowwise tabxplor_grouped_tab
#' @param data A tibble of class \code{tabxplor_tab}.
#' @param ... Variables to be preserved
#'   when calling summarise(). This is typically a set of variables whose
#'   combination uniquely identify each row.
#' @return An object of class \code{tabxplor_grouped_tab} and \code{rowwise_df}.
#' @export
rowwise.tabxplor_grouped_tab <- function(data, ...) {
  out <- NextMethod()
  groups <- dplyr::group_data(out)

  out <- new_grouped_tab(out, groups, subtext = get_subtext(data), chi2 = get_chi2(data))
  `class<-`(out, stringr::str_replace(class(out), "grouped_df", "rowwise_df"))
}

# #' @method rbind tabxplor_grouped_tab
# #' @export
# rbind.tabxplor_grouped_tab <- function(...) {
#   out <- NextMethod()
#   groups <- dplyr::group_data(out)
#   if (lv1_group_vars(out)) {
#     new_tab(out, subtext = get_subtext(.data), chi2 = get_chi2(.data))
#   } else {
#     new_grouped_tab(out, groups, subtext = get_subtext(.data), chi2 = get_chi2(.data))
#   }
# }
# # dplyr:::rbind.grouped_df
#
# #' @method cbind tabxplor_grouped_tab
# #' @export
# cbind.tabxplor_grouped_tab <- function(...) {
#   out <- NextMethod()
#   groups <- dplyr::group_data(out)
#   if (lv1_group_vars(out)) {
#     new_tab(out, subtext = get_subtext(.data), chi2 = get_chi2(.data))
#   } else {
#     new_grouped_tab(out, groups, subtext = get_subtext(.data), chi2 = get_chi2(.data))
#   }
# }
# # dplyr:::cbind.grouped_df

#' summarise method for class tabxplor_grouped_tab
#' @importFrom dplyr summarise
#' @method summarise tabxplor_grouped_tab
#' @param .data A tibble of class \code{tabxplor_tab}.
#' @param ... Name-value pairs of summary functions. The name will be the name of the
#' variable in the result.
#' @param .groups Grouping structure of the result.
#' @return An object of class \code{tabxplor_grouped_tab}.
#' @export
summarise.tabxplor_grouped_tab <- function(.data, ..., .groups = NULL) {
  out <- NextMethod()
  groups <- dplyr::group_data(out)
  if (lv1_group_vars(out)) {
    new_tab(out, subtext = get_subtext(.data), chi2 = get_chi2(.data))
  } else {
    new_grouped_tab(out, groups, subtext = get_subtext(.data), chi2 = get_chi2(.data))
  }
}


#' select method for class tabxplor_grouped_tab
#' @importFrom dplyr select
#' @method select tabxplor_grouped_tab
#' @param .data A tibble of class \code{tabxplor_tab}.
#' @param ... One or more unquoted expressions separated by commas. Variable names can be
#' used as if they were positions in the data frame, so expressions like \code{x:y} can
#'   be used to select a range of variables.
#' @return An object of class \code{tabxplor_grouped_tab}.
#' @export
select.tabxplor_grouped_tab <- function(.data, ...) {
  out <- NextMethod()
  groups <- dplyr::group_data(out)
  if (lv1_group_vars(out)) {
    new_tab(out, subtext = get_subtext(.data), chi2 = get_chi2(.data))
  } else {
    new_grouped_tab(out, groups, subtext = get_subtext(.data), chi2 = get_chi2(.data))
  }
}

#' rename method for class tabxplor_grouped_tab
#' @importFrom dplyr rename
#' @method rename tabxplor_grouped_tab
#' @param .data A tibble of class \code{tabxplor_tab}.
#' @param ... Use \code{new_name = old_name} to rename selected variables.
#' @return An object of class \code{tabxplor_grouped_tab}.
#' @export
rename.tabxplor_grouped_tab <- function(.data, ...) {
  out <- NextMethod()
  groups <- dplyr::group_data(out)
  if (lv1_group_vars(out)) {
    new_tab(out, subtext = get_subtext(.data), chi2 = get_chi2(.data))
  } else {
    new_grouped_tab(out, groups, subtext = get_subtext(.data), chi2 = get_chi2(.data))
  }
}

#' rename_with method for class tabxplor_grouped_tab
#' @importFrom dplyr rename_with
#' @method rename_with tabxplor_grouped_tab
#' @param .data A tibble of class \code{tabxplor_tab}.
#' @param ... Additional arguments passed onto \code{.fn}.
#' @param .fn A function used to transform the selected \code{.cols}. Should
#'   return a character vector the same length as the input.
#' @param .cols Columns to rename; defaults to all columns.
#' @return An object of class \code{tabxplor_grouped_tab}.
#' @export
rename_with.tabxplor_grouped_tab <- function(.data, .fn, .cols = dplyr::everything(), ...) {
  out <- NextMethod()
  groups <- dplyr::group_data(out)
  if (lv1_group_vars(out)) {
    new_tab(out, subtext = get_subtext(.data), chi2 = get_chi2(.data))
  } else {
    new_grouped_tab(out, groups, subtext = get_subtext(.data), chi2 = get_chi2(.data))
  }
}


#' relocate method for class tabxplor_grouped_tab
#' @importFrom dplyr relocate
#' @method relocate tabxplor_grouped_tab
#' @param .data A tibble of class \code{tabxplor_tab}.
#' @param ... Columns to move.
# @param .before,.after Destination of columns selected by \code{...}. Supplying neither
#'  will move columns to the left-hand side; specifying both is an error.
#' @return An object of class \code{tabxplor_grouped_tab}.
#' @export
relocate.tabxplor_grouped_tab <- function(.data, ...) { #.before = NULL, .after = NULL
  out <- NextMethod()
  groups <- dplyr::group_data(out)
  if (lv1_group_vars(out)) {
    new_tab(out, subtext = get_subtext(.data), chi2 = get_chi2(.data))
  } else {
    new_grouped_tab(out, groups, subtext = get_subtext(.data), chi2 = get_chi2(.data))
  }
} # dplyr:::relocate.grouped_df

# #' distinct_ method for class tabxplor_grouped_tab
# #' @importFrom dplyr distinct_
# #' @method distinct_ tabxplor_grouped_tab
# #' @param .data A tibble of class \code{tabxplor_tab}.
# #' @return An object of class \code{tabxplor_grouped_tab}.
# #' @export
# distinct_.tabxplor_grouped_tab <- function(.data, ..., .dots = list(), .keep_all = FALSE) {
#   out <- NextMethod()
#   groups <- dplyr::group_data(out)
#   if (lv1_group_vars(out)) {
#     new_tab(out, subtext = get_subtext(.data), chi2 = get_chi2(.data))
#   } else {
#     new_grouped_tab(out, groups, subtext = get_subtext(.data), chi2 = get_chi2(.data))
#   }
# }
# # dplyr:::distinct_.grouped_df













#' @rdname tab_cast
#' @keywords internal
# @export
gtab_cast <- function(x, to, ..., x_arg = "", to_arg = "") {
  #based upon vctrs:::gdf_cast()
  df <- vctrs::df_cast(x, to, ..., x_arg = x_arg, to_arg = to_arg)
  vars <- dplyr::group_vars(to)
  drop <- dplyr::group_by_drop_default(to)
  gdf <- dplyr::grouped_df(df, vars, drop = drop)

  groups <- dplyr::group_data(gdf)
  new_grouped_tab(gdf, groups, subtext = get_subtext(to), chi2 = get_chi2(to))
}

#' @rdname tab_cast
#' @keywords internal
# @export
gtab_ptype2 <- function(x, y, ..., x_arg = "", y_arg = "") {
  #based upon vctrs:::gdf_ptype2
  common <- vctrs::df_ptype2(x, y, ..., x_arg = x_arg, y_arg = y_arg)
  x_vars <- dplyr::group_vars(x)
  y_vars <- dplyr::group_vars(y)
  vars <- union(x_vars, y_vars)
  drop <- dplyr::group_by_drop_default(x) && dplyr::group_by_drop_default(y)
  gdf <-  dplyr::grouped_df(common, vars, drop = drop)

  groups <- dplyr::group_data(gdf)
  new_grouped_tab(gdf, groups, subtext = get_subtext(x), chi2 = get_chi2(x))
}

#Self-self
#' @describeIn tab_cast find common ptype between tabxplor_grouped_tab and tabxplor_grouped_tab
#' @return An object of class \code{tabxplor_grouped_tab}.
#' @export
vec_ptype2.tabxplor_grouped_tab.tabxplor_grouped_tab <- function(x, y, ...) {
  gtab_ptype2(x, y, ...)
}
#' @describeIn tab_cast convert tabxplor_grouped_tab to tabxplor_grouped_tab
#' @return An object of class \code{tabxplor_grouped_tab}.
#' @export
vec_cast.tabxplor_grouped_tab.tabxplor_grouped_tab <- function(x, to, ...) {
  gtab_cast(x, to, ...)
}

#grouped_tab / grouped_df
#' @describeIn tab_cast find common ptype between tabxplor_grouped_tab and grouped_df
#' @return An object of class \code{tabxplor_grouped_tab}.
#' @export
vec_ptype2.tabxplor_grouped_tab.grouped_df <- function(x, y, ...) {
  gtab_ptype2(x, y, ...)
}
#' @describeIn tab_cast find common ptype between grouped_df and tabxplor_grouped_tab
#' @return An object of class \code{grouped_df}.
#' @export
vec_ptype2.grouped_df.tabxplor_grouped_tab <- function(x, y, ...) {
  gtab_ptype2(x, y, ...)
}
#' @describeIn tab_cast convert grouped_df to tabxplor_grouped_tab
#' @return An object of class \code{tabxplor_grouped_tab}.
#' @export
vec_cast.tabxplor_grouped_tab.grouped_df <- function(x, to, ...) {
  gtab_cast(x, to, ...)
}
#' @describeIn tab_cast convert tabxplor_grouped_tab to grouped_df
#' @return An object of class \code{grouped_df}.
#' @export
vec_cast.grouped_df.tabxplor_grouped_tab <- function(x, to, ...) {
  #vctrs:::gdf_cast
  df <- vctrs::df_cast(x, to, ...)
  vars <- dplyr::group_vars(to)
  drop <- dplyr::group_by_drop_default(to)
  dplyr::grouped_df(df, vars, drop = drop)
}

#grouped_tab / tab
#' @describeIn tab_cast find common ptype between tabxplor_grouped_tab and tabxplor_tab
#' @return An object of class \code{tabxplor_grouped_tab}.
#' @export
vec_ptype2.tabxplor_grouped_tab.tabxplor_tab <- function(x, y, ...) {
  gtab_ptype2(x, y, ...)
}
#' @describeIn tab_cast find common ptype between tabxplor_tab and tabxplor_grouped_tab
#' @return An object of class \code{tabxplor_tab}.
#' @export
vec_ptype2.tabxplor_tab.tabxplor_grouped_tab <- function(x, y, ...) {
  gtab_ptype2(x, y, ...)
}
#' @describeIn tab_cast convert tabxplor_tab to tabxplor_grouped_tab
#' @return An object of class \code{tabxplor_grouped_tab}.
#' @export
vec_cast.tabxplor_grouped_tab.tabxplor_tab <- function(x, to, ...) {
  gtab_cast(x, to, ...)
}
#' @describeIn tab_cast convert tabxplor_grouped_tab to tabxplor_tab
#' @return An object of class \code{tabxplor_tab}.
#' @export
vec_cast.tabxplor_tab.tabxplor_grouped_tab <- function(x, to, ...) {
  tab_cast(x, to, ...)
}

#grouped_tab / tbl_df
#' @describeIn tab_cast find common ptype between tabxplor_grouped_tab and tbl_df
#' @return An object of class \code{tabxplor_grouped_tab}.
#' @export
vec_ptype2.tabxplor_grouped_tab.tbl_df <- function(x, y, ...) {
  gtab_ptype2(x, y, ...)
}
#' @describeIn tab_cast find common ptype between tbl_df and tabxplor_grouped_tab
#' @return An object of class \code{tbl_df}.
#' @export
vec_ptype2.tbl_df.tabxplor_grouped_tab <- function(x, y, ...) {
  gtab_ptype2(x, y, ...)
}
#' @describeIn tab_cast convert tbl_df to tabxplor_grouped_tab
#' @return An object of class \code{tabxplor_grouped_tab}.
#' @export
vec_cast.tabxplor_grouped_tab.tbl_df <- function(x, to, ...) {
  gtab_cast(x, to, ...)
}
#' @describeIn tab_cast convert tabxplor_grouped_tab to tbl_df
#' @return An object of class \code{tbl_df}.
#' @export
vec_cast.tbl_df.tabxplor_grouped_tab <- function(x, to, ...) {
  vctrs::tib_cast(x, to, ...)
}

#grouped_tab / data.frame
#' @describeIn tab_cast find common ptype between tabxplor_grouped_tab and data.frame
#' @return An object of class \code{tabxplor_grouped_tab}.
#' @export
vec_ptype2.tabxplor_grouped_tab.data.frame <- function(x, y, ...) {
  gtab_ptype2(x, y, ...)
}
#' @describeIn tab_cast find common ptype between data.frame and tabxplor_grouped_tab
#' @return An data.frame.
#' @export
vec_ptype2.data.frame.tabxplor_grouped_tab <- function(x, y, ...) {
  gtab_ptype2(x, y, ...)
}
#' @describeIn tab_cast convert data.frame to tabxplor_grouped_tab
#' @return An object of class \code{tabxplor_grouped_tab}.
#' @export
vec_cast.tabxplor_grouped_tab.data.frame <- function(x, to, ...) {
  gtab_cast(x, to, ...)
}
#' @describeIn tab_cast convert tabxplor_grouped_tab to data.frame
#' @return An data.frame.
#' @export
vec_cast.data.frame.tabxplor_grouped_tab <- function(x, to, ...) {
  vctrs::df_cast(x, to, ...)
}


#Colors for printing fmt in tabs -------------------------------------------------------

# # Test function to see how colors print
# #' @keywords internal
# color_graph <- function(former = NULL, new = NULL, new2 = NULL, new3 = NULL) {
#   HCLformer <- tibble::as_tibble(t(round(jamba::col2hcl(former)[-4,], 0)))
#   HCLnew    <- tibble::as_tibble(t(round(jamba::col2hcl(new   )[-4,], 0)))
#   HCLnew2   <- tibble::as_tibble(t(round(jamba::col2hcl(new2  )[-4,], 0)))
#   HCLnew3   <- tibble::as_tibble(t(round(jamba::col2hcl(new3  )[-4,], 0)))
#
#   colors <- tibble::tibble(
#     color = rep(c(former, new, new2, new3), 4),
#     text = c(former, new, new2, new3,
#              HCLformer$H, HCLnew$H, HCLnew2$H, HCLnew3$H,
#              HCLformer$C, HCLnew$C, HCLnew2$C, HCLnew3$C,
#              HCLformer$L, HCLnew$L, HCLnew2$L, HCLnew3$L ),
#     x = rep(c(if(length(former) != 0){1:length(former)} else {NULL},
#               if(length(new   ) != 0){1:length(new)   } else {NULL},
#               if(length(new2  ) != 0){1:length(new2)  } else {NULL},
#               if(length(new3  ) != 0){1:length(new3)  } else {NULL} ), 4),
#     y = c(rep(1 , length(former)),
#           rep(0 , length(new   )),
#           rep(-1, length(new2  )),
#           rep(-2, length(new3  )),
#
#           rep(-4, length(former)),
#           rep(-5, length(new   )),
#           rep(-6, length(new2  )),
#           rep(-7, length(new3  )),
#
#           rep(-9, length(former)),
#           rep(-10, length(new   )),
#           rep(-11, length(new2  )),
#           rep(-12, length(new3  )),
#
#           rep(-14, length(former)),
#           rep(-15, length(new   )),
#           rep(-16, length(new2  )),
#           rep(-17, length(new3  ))
#     )
#   )
#   color_scale <- c(if(length(former) != 0){former} else {NULL},
#                    if(length(new   ) != 0){new   } else {NULL},
#                    if(length(new2  ) != 0){new2  } else {NULL},
#                    if(length(new3  ) != 0){new3  } else {NULL}
#   ) %>% purrr::set_names(.)
#   color_scale <- color_scale[!duplicated(names(color_scale))]
#
#   ggplot2::ggplot(colors, ggplot2::aes(x = x, y = y, color = color, label = text)) +
#     ggplot2::geom_text(fontface = "bold") +
#     ggplot2::scale_color_manual(values = color_scale) +
#     ggplot2::theme_minimal() +
#     ggplot2::theme(panel.grid = ggplot2::element_line(colour = "white")) +
#     ggplot2::ylim(-18, 3) +
#     ggplot2::annotate("text", x = 1, y =   2, label = "Colors :") +
#     ggplot2::annotate("text", x = 1, y =  -3, label = "Hue :") +
#     ggplot2::annotate("text", x = 1, y =  -8, label = "Chroma :") +
#     ggplot2::annotate("text", x = 1, y = -13, label = "Luminance :")
#
# }

#' @keywords internal
color_style_text_dark <-
  c(pos1 = "#CCCC33", # rgb(4, 4, 1, maxColorValue = 5),
    pos2 = "#CCFF33", # rgb(4, 5, 1, maxColorValue = 5),
    pos3 = "#99FF33", # rgb(3, 5, 1, maxColorValue = 5),
    pos4 = "#33FF33", # rgb(1, 5, 1, maxColorValue = 5),
    pos5 = "#00FF00", # rgb(0, 5, 0, maxColorValue = 5),

    neg1 = "#CC9966", # rgb(4, 3, 2, maxColorValue = 5),
    neg2 = "#FF9933", # rgb(5, 3, 1, maxColorValue = 5),
    neg3 = "#FF6633", # rgb(5, 2, 1, maxColorValue = 5),
    neg4 = "#FF3300", # rgb(5, 1, 0, maxColorValue = 5),
    neg5 = "#FF0000",  # rgb(5, 0, 0, maxColorValue = 5)

    ratio ="#3366FF"  # "#7B1FA2",

  ) # |>
#purrr::map(~ crayon::make_style(., colors = 256))

#' @keywords internal
color_style_text_light <-
  c(pos1 = "#66CCFF", # rgb(2, 4, 5, maxColorValue = 5),
    pos2 = "#33FFFF", # rgb(1, 5, 5, maxColorValue = 5),
    pos3 = "#00CCFF", # rgb(0, 4, 5, maxColorValue = 5),
    pos4 = "#0066FF", # rgb(0, 2, 5, maxColorValue = 5),
    pos5 = "#0000FF", # rgb(0, 0, 5, maxColorValue = 5),

    neg1 =  "#CC9966", # rgb(4, 3, 2, maxColorValue = 5),
    neg2 =  "#FF9933", # rgb(5, 3, 1, maxColorValue = 5),
    neg3 =  "#FF6600", # rgb(5, 2, 0, maxColorValue = 5),
    neg4 =  "#FF3333", # rgb(5, 1, 1, maxColorValue = 5),
    neg5 =  "#FF0000",  # rgb(5, 0, 0, maxColorValue = 5)

    ratio = "#6633CC" # "#6600CC"
  ) #|>
    #purrr::map(~ crayon::make_style(., colors = 256))



# # #install_github("jmw86069/jamba", upgrade = "never")
# former <-  c(white = "#111111",
#              grey = "#888888",
#              pos1 = "#66CCFF",
#              pos2 = "#33FFFF",
#              pos3 = "#00CCFF",
#              pos4 = "#0066FF",
#              pos5 = "#0000FF",
#              neg1 = "#CC9966",
#              neg2 = "#FF9933",
#              neg3 = "#FF6600",
#              neg4 = "#FF3333",
#              neg5 = "#FF0000" )
#
#
# change <- jamba::col2hcl("#4EE6B9")
# change[1,] <- 180
# change <- jamba::hcl2col(change)
# change
#
# # c(white = "#111111", grey = "#bbbbbb",
# #         "#e4e65e", "#C7D62C", "#83BB3F", "#3BA240", "#1b6e20",
# #         "#fdd835", "#ffb300", "#FF8138", "#ff3d00", "#cb0000" )
# #
# new1 <- c(white = "#111111", grey = "#bbbbbb",
#           "#7BF245", "#1de9b6", "#26c6da", "#1e88e5", "#0019ff",
#           "#fdd835", "#ffb300", "#FF8138", "#ff3d00", "#cb0000" )
#
#
# new2 <- c(white = "#111111", grey = "#bbbbbb",
#           "#93ED75", "#4EE6B9", "#00bcd4", "#1e88e5", "#0019ff",
#           "#fdd835", "#ffb300", "#FF8138", "#ff3d00", "#cb0000" )
#
# new3 <- c(white = "#111111", grey = "#bbbbbb",
#           "#93ED75", "#1AE6D6", "#00bcd4", "#1e88e5", "#0019ff",
#           "#fdd835", "#ffb300", "#FF8138", "#ff3d00", "#cb0000" )
# color_graph(former, new1, new2, new3)

#' @keywords internal
color_style_text_light_24_blue_red <-
  c(pos1 = "#93ED75",   #  c(pos1 = "#e4e65e",
    pos2 = "#1AE6D6",   #    pos2 = "#cddc39", "#4EE6B9"
    pos3 = "#00bcd4",   #    pos3 = "#8bc34a",
    pos4 = "#1e88e5",   #    pos4 = "#589E38",
    pos5 = "#0019ff",   #    pos5 = "#1b6e20",

    neg1 = "#fdd835",   #    neg1 = "#ffeb3b",
    neg2 = "#ffb300",   #    neg2 = "#ffc400",
    neg3 = "#FF8138",   #    neg3 = "#ff9100",
    neg4 = "#ff3d00",   #    neg4 = "#ff3d00",
    neg5 = "#cb0000",

    ratio = "#673AB7"
    #   "#8E24AA", "#7B1FA2" "#6A1B9A"
    # "#673AB7", "#5E35B1", "#512DA8", "#4527A0"

  )  #    neg5 = "#cb0000" )

# pct_ratio_color_style <- c(ratio = "#6A1B9A")

#' @keywords internal
color_style_text_light_24_green_red <-
  c(pos1 = "#e4e65e",   #  c(pos1 = "#e4e65e",
    pos2 = "#C7D62C",   #    pos2 = "#cddc39",
    pos3 = "#83BB3F",   #    pos3 = "#8bc34a",
    pos4 = "#3BA240",   #    pos4 = "#589E38",
    pos5 = "#1b6e20",   #    pos5 = "#1b6e20",

    neg1 = "#fdd835",   #    neg1 = "#ffeb3b",
    neg2 = "#ffb300",   #    neg2 = "#ffc400",
    neg3 = "#FF8138",   #    neg3 = "#ff9100",
    neg4 = "#ff3d00",   #    neg4 = "#ff3d00",
    neg5 = "#cb0000",

    ratio ="#1976D2"   # "#7B1FA2",

    )  #    neg5 = "#cb0000" )

#' @keywords internal
color_style_bg_light <- # also change in select_in_color_style()
  c(pos1 = "#CCFFCC", # rgb(4, 5, 4, maxColorValue = 5),
    pos2 = "#99FF99", # rgb(3, 5, 3, maxColorValue = 5),
    pos3 = "#66FF66", # rgb(2, 5, 2, maxColorValue = 5),
    pos4 = "#33FF33", # rgb(1, 5, 1, maxColorValue = 5),
    pos5 = "#00FF00", # rgb(0, 5, 0, maxColorValue = 5),

    neg1 = "#FFCCCC", # rgb(5, 4, 4, maxColorValue = 5),
    neg2 = "#FF9999", # rgb(5, 3, 3, maxColorValue = 5),
    neg3 = "#FF6666", # rgb(5, 2, 2, maxColorValue = 5),
    neg4 = "#FF3333", # rgb(5, 1, 1, maxColorValue = 5),
    neg5 = "#FF0000",  # rgb(5, 0, 0, maxColorValue = 5)

    ratio ="#6699FF" # rgb(3, 0, 5, maxColorValue = 5) #  "#9900FF" "#6600FF", "#6600CC"

    ) #%>%
#purrr::map(~ crayon::make_style(., bg = TRUE, colors = 256))

#' @keywords internal
color_style_bg_dark <- # also change in select_in_color_style()
  c(pos1 = "#000033", #rgb(0, 0, 1, maxColorValue = 5),
    pos2 = "#000066", #rgb(0, 0, 2, maxColorValue = 5),
    pos3 = "#000099", #rgb(0, 0, 3, maxColorValue = 5),
    pos4 = "#0000CC", #rgb(0, 0, 4, maxColorValue = 5),
    pos5 = "#0000FF", #rgb(0, 0, 5, maxColorValue = 5),

    neg1 = "#330000", #rgb(1, 0, 0, maxColorValue = 5),
    neg2 = "#660000", #rgb(2, 0, 0, maxColorValue = 5),
    neg3 = "#990000", #rgb(3, 0, 0, maxColorValue = 5),
    neg4 = "#CC0000", #rgb(4, 0, 0, maxColorValue = 5),
    neg5 = "#FF0000", #rgb(5, 0, 0, maxColorValue = 5)

    ratio ="#6600CC"  # "#7B1FA2",
    ) #%>%
#purrr::map(~ crayon::make_style(., bg = TRUE, colors = 256))



#' Define the color style used to print \code{\link{tab}}
#' @describeIn tab_many define the color style used to print \code{\link{tab}}.
#' @param type The style type in \code{set_color_style} and \code{get_color_style},
#'  \code{"text"} to color the text, \code{"bg"} to color the background.
#' @param theme For \code{set_color_style} and \code{get_color_style}, is your console
#' or html table background \code{"light"} or \code{"dark"} ? Default to RStudio theme.
#' @param html_24_bit Use 24bits colors palettes for html tables : set to `"green_red"`
#' or `"blue_red"`. Only with `mode = "color_code"` (not `mode = "crayon"`) and
#' `theme = "light`. Default to \code{getOption("tabxplor.color_html_24_bit")}.
#' @param custom_palette Possibility to provide a custom color styles, as a character
#' vector of 10 html color codes (the five first for over-represented numbers,
#' the five last for under-represented ones). The result is saved to
#' \code{options("tabxplor.color_style")}. To discard, relaunch the function with
#' \code{custom_palette = NULL}.
#'
#' @return Set global options \code{"tabxplor.color_style_type"} and
#' \code{"tabxplor.color_style_theme"}, used when printing \code{\link{tab}} objects.
#' @export
#'
#' @examples set_color_style(type = "bg")
set_color_style <- function(type = c("text", "bg"),
                            theme = NULL,
                            html_24_bit = c("blue_red", "green_red", "no"),
                            custom_palette = NULL) {
  stopifnot(all(type %in% c("text", "bg")))
  options("tabxplor.color_style_type" = type[1])

  stopifnot(all(html_24_bit %in% c("green_red", "blue_red", "no")))
  options("tabxplor.color_html_24_bit" = html_24_bit[1])

  if (is.null(theme)) {
    is_RStudio <- function() Sys.getenv("RSTUDIO") == "1" & rlang::is_interactive()  #.Platform$GUI == "RStudio"
    is_dark <- if (is_RStudio()) { rstudioapi::getThemeInfo()$dark } else { FALSE }
    options("tabxplor.color_style_theme" = ifelse(is_dark, "dark", "light"))
  } else {
    stopifnot(length(theme) == 1 & all(theme %in% c("dark", "light")))
    options("tabxplor.color_style_theme" = theme)
  }

  if (length(custom_palette) != 0) {
    if (length(custom_palette) != 10 | !is.character(custom_palette)) stop(
      "custom_palette should be a character vector of length 11"
    )
    options("tabxplor.color_style" = purrr::set_names(
      custom_palette,
      c("pos1","pos2","pos3","pos4","pos5", "neg1","neg2","neg3","neg4","neg5", "ratio")
    ))
    return(invisible(custom_palette))
  } else {
    options("tabxplor.color_style" = NULL)
    return(invisible())
  }

  # assign("tabxplor_color_breaks", tabxplor_color_breaks, pos = rlang::global_env() )
}

#' @describeIn tab_many get color styles as \pkg{crayon} functions or html codes.
#' @param mode By default, \code{get_color_style} returns a list of \pkg{crayon} coloring
#' functions. Set to \code{"color_code"} to return html color codes.
#' @return A vector of crayon color functions, or a vector of color html codes.
#' @export
get_color_style <- function(mode = c("crayon", "color_code"),
                            type = NULL, theme = NULL, html_24_bit = NULL) {

  type  <- if (is.null(type )) {getOption("tabxplor.color_style_type" )} else {type }
  theme <- if (is.null(theme)) {getOption("tabxplor.color_style_theme")} else {theme}
  html_24_bit <-
    if (is.null(html_24_bit)) {getOption("tabxplor.color_html_24_bit")} else {html_24_bit}

  if (mode[1] == "crayon") html_24_bit <- "no"

  custom_palette <- getOption("tabxplor.color_style")
  if (is.null(custom_palette)) {
    color_style <-
      switch(type,
             "text" = switch(theme,
                             "dark"  = color_style_text_dark,
                             "light" = switch(html_24_bit,
                                              "green_red" = color_style_text_light_24_green_red,
                                              "blue_red"  = color_style_text_light_24_blue_red,
                                              "no"        = color_style_text_light)
             ),

             "bg"   = switch(theme,
                             "light" = color_style_bg_light,
                             "dark"  = color_style_bg_dark
             )
      )

    # if (mode[1] == "color_code" & !color_bits == "24") color_style <- color_style %>%
    #   purrr::map_chr(~ attr(., "_styles", exact = TRUE) %>% names())
  } else {
    color_style <- custom_palette
  }

  if (mode[1] == "crayon") color_style <- color_style %>%
    purrr::map(~ crayon::make_style(., bg = type[1] == "bg", colors = 256))

  color_style
}




# cat_style <- function(styles = tabxplor_color_style) cat("\n",
#                                            styles$pos1("42%" ), styles$neg1("42%\n" ),
#                                            styles$pos2("42%" ), styles$neg2("42%\n" ),
#                                            styles$pos3("42%" ), styles$neg3("42%\n" ),
#                                            styles$pos4("42%" ), styles$neg4("42%\n" ),
#                                            styles$pos5("42%" ), styles$neg5("42%\n" ) )
#
# set_color_style(n = 5) %>%
#   purrr::map(~ crayon::make_style(., colors = 256)) %>% cat_style()
#
# set_color_style(console_theme = "light", n = 5) %>%
#   purrr::map(~ crayon::make_style(., colors = 256)) %>% cat_style()
#
# set_color_style(type = "bg", n = 5) %>%
#   purrr::map(~ crayon::make_style(., bg = TRUE, colors = 256)) %>% cat_style()
#
# set_color_style(type = "bg", console_theme = "light", n = 5)  %>%
#   purrr::map(~ crayon::make_style(., bg = TRUE, colors = 256)) %>% cat_style()

#crayon::show_ansi_colors()



#Color breaks for printing fmt in tabs ------------------------------------------------

#' Set the breaks used to print colors
#' @describeIn tab_many set the breaks used to print colors
#' @description Only breaks for attractions/over-representations (in green) should be
#' given, as a vector of positive doubles, with length between 1 and 5.
#' Breaks for aversions/under-representations (in orange/red) will simply be the opposite.
#' @param pct_breaks If they are to be changed, the breaks used for percentages.
#' Default to \code{c(0.05, 0.1, 0.2, 2, 0.3)} : first color used when the pct of a cell
#' is +5% superior to the pct of the related total ; second color used when
#' it is +10% superior ; third +20% superior ; fourth *2 superior ;
#' fifth +30% superior. When > 1, it does not take differences but ratio.
#' The opposite for cells inferior to the total (without the *2 rule).
#' With \code{color = "after_ci"}, the first break is subtracted from all breaks
#' (default becomes \code{c(0, 0.05, 0.15, 2, 0.25)} : +0%, +5%, +15%, *2, +25%).
#' @param mean_breaks If they are to be changed, the breaks used for means.
#' Default to \code{c(1.15, 1.5, 2, 4)} : first color used when the mean of a cell
#' is superior to 1.15 times the mean of the related total row ; second color
#' used when it is superior to 1.5 times ; etc.
#' The opposite for cells inferior to the total.
#' With \code{color = "after_ci"}, the first break is divided from all breaks
#' (default becomes \code{c(1, 1.3, 1.7, 3.5)}).
#' @param contrib_breaks If they are to be changed, the breaks used for contributions to
#' variance. Default to \code{c(1, 2, 5, 10)} : first color used when the contribution of
#' a cell is superior to the mean contribution ; second color used when it is superior to
#' 2 times the mean contribution ; etc. The global color (for example green or
#' red/orange) is given by the sign of the spread.
#'
#' @return Set the global option "tabxplor.color_breaks" as a list different double
#' vectors, and also returns it invisibly.
#' @export
#' @examples set_color_breaks(
#'   pct_breaks = c(0.05, 0.15, 0.3),
#'   mean_breaks = c(1.15, 2, 4),
#'   contrib_breaks = c(1, 2, 5)
#' )
set_color_breaks <- function(pct_breaks, mean_breaks, contrib_breaks) {
  # Defaults are set at the first use of print.tabxplor_tab method :
  #   pct_breaks = c(0.05, 0.1, 0.2, 0.3),
  #   mean_breaks = c(1.15, 1.5, 2, 4),
  #   contrib_breaks = c(1, 2, 5, 10)

  if (missing(pct_breaks) | missing(mean_breaks) | missing(contrib_breaks)   ) {
    former_breaks <- getOption("tabxplor.color_breaks")
  }

  if (!missing(pct_breaks)) {
    stopifnot(is.numeric(pct_breaks)     ,
              length(pct_breaks)     <= 5,
              sum(pct_breaks > 1)    <= 1,         # not several *2 rule
              all(pct_breaks         >= 0))

    pct_ratio_breaks <- pct_breaks[pct_breaks > 1] # *2 rule
    # if (length(pct_ratio_breaks) > 0) {
    #   pct_ratio_breaks <- c(
    #     dplyr::if_else(which(pct_breaks > 1) == 1, 0, pct_breaks[which(pct_breaks > 1) -1 ] ),
    #     pct_breaks[pct_breaks > 1]
    #   )
    #
    #   pct_ratio_brksup <- c(
    #     pct_breaks[pct_breaks > 1],
    #     dplyr::if_else(which(pct_breaks > 1) == length(pct_breaks),
    #                    true  = Inf,
    #                    false = pct_breaks[which(pct_breaks > 1) + 1 ] )
    #   )
    #
    #   pct_breaks       <- pct_breaks[pct_breaks <= 1]
    #
    # } else {
    #   pct_ratio_breaks <- NA_real_
    #   pct_ratio_brksup <- NA_real_
    # }

    pct_ci_breaks   <- pct_breaks - dplyr::if_else(pct_breaks > 1,
                                                   true  = 0,
                                                   false = pct_breaks[1])
    #pct_brksup      <- c(pct_breaks[2:length(pct_breaks)    ], Inf)
    #pct_brksup      <- pct_brksup     %>% c(., -.[. <= 1 | . == Inf])
    pct_breaks      <- pct_breaks     %>% c(., -.[. <= 1 | . == Inf])

    #pct_ci_brksup   <- c(pct_ci_breaks[2:length(pct_ci_breaks) ], Inf)
    #pct_ci_brksup   <- pct_ci_brksup  %>% c(., -.[. <= 1 | . == Inf])
    pct_ci_breaks   <- pct_ci_breaks  %>% c(., -.[. <= 1 | . == Inf])


  } else {
    pct_breaks      <- former_breaks$pct_breaks
    pct_brksup      <- former_breaks$pct_brksup
    pct_ci_breaks   <- former_breaks$pct_ci_breaks
    pct_ci_brksup   <- former_breaks$pct_ci_brksup
    # pct_ratio_breaks<- former_breaks$pct_ratio_breaks
    # pct_ratio_brksup<- former_breaks$pct_ratio_brksup
  }

  # if (!missing(pct_ratio_breaks)) {
  #   stopifnot(is.numeric(pct_ratio_breaks)     ,
  #             length(pct_ratio_breaks)     <= 1,
  #             all(pct_ratio_breaks         >= 0))
  #   pct_ratio_brksup      <- c(pct_ratio_breaks, Inf)
  #   pct_ratio_breaks      <- c(0, pct_ratio_breaks)
  #
  #   # pct_ratio_ci_brksup   <- c(pct_ratio_ci_breaks[2:length(pct_ratio_ci_breaks) ], Inf)
  #   # pct_ratio_ci_brksup   <- pct_ratio_ci_brksup  %>% c(., -.)
  #   # pct_ratio_ci_breaks   <- pct_ratio_ci_breaks  %>% c(., -.)
  #
  # } else {
  #   pct_ratio_breaks      <- former_breaks$pct_ratio_breaks
  #   pct_ratio_brksup      <- former_breaks$pct_ratio_brksup
  #   # pct_ratio_ci_breaks   <- former_breaks$pct_ratio_ci_breaks
  #   # pct_ratio_ci_brksup   <- former_breaks$pct_ratio_ci_brksup
  # }

  if (!missing(mean_breaks)) {
    stopifnot(is.numeric(mean_breaks)    ,
              length(mean_breaks)    <= 5,
              all(mean_breaks        >= 0))
    mean_ci_breaks  <- mean_breaks / mean_breaks[1]
    #mean_brksup     <- c(mean_breaks   [2:length(mean_breaks)   ], Inf)
    #mean_brksup     <- mean_brksup    %>% c(., 1/.)
    mean_breaks     <- mean_breaks    %>% c(., 1/.)

    #mean_ci_brksup  <- c(mean_ci_breaks[2:length(mean_ci_breaks)], Inf)
    #mean_ci_brksup  <- mean_ci_brksup %>% c(., -.) #then - again
    mean_ci_breaks  <- mean_ci_breaks %>% c(., -.) #then - again

  } else {
    mean_breaks     <- former_breaks$mean_breaks
    #mean_brksup     <- former_breaks$mean_brksup
    mean_ci_breaks  <- former_breaks$mean_ci_breaks
    #mean_ci_brksup  <- former_breaks$mean_ci_brksup
  }

  if (!missing(contrib_breaks)) {
    stopifnot(is.numeric(contrib_breaks) ,
              length(contrib_breaks) <= 5,
              all(contrib_breaks     >= 0))
    #contrib_brksup  <- c(contrib_breaks[2:length(contrib_breaks)], Inf)
    #contrib_brksup  <- contrib_brksup %>% c(., -.)
    contrib_breaks  <- contrib_breaks %>% c(., -.)

  } else {
    contrib_breaks  <- former_breaks$contrib_breaks
    #contrib_brksup  <- former_breaks$contrib_brksup
  }

  tabxplor_color_breaks <- list(pct_breaks       = pct_breaks      ,
                                #pct_brksup       = pct_brksup      ,
                                pct_ci_breaks    = pct_ci_breaks   ,
                                #pct_ci_brksup    = pct_ci_brksup   ,
                                # pct_ratio_breaks = pct_ratio_breaks,
                                # pct_ratio_brksup = pct_ratio_brksup,
                                mean_breaks      = mean_breaks     ,
                                #mean_brksup      = mean_brksup     ,
                                mean_ci_breaks   = mean_ci_breaks  ,
                                #mean_ci_brksup   = mean_ci_brksup  ,
                                contrib_breaks   = contrib_breaks  #,
                                # contrib_brksup   = contrib_brksup
                                )

  options("tabxplor.color_breaks" = tabxplor_color_breaks)

  # assign("tabxplor_color_breaks", tabxplor_color_breaks, pos = rlang::global_env() )

  invisible(tabxplor_color_breaks)
}





#calculate pct breaks based on the number of levels ? ----

# pct_breaks      <- c(0.05, 0.1, 0.2, 0.3)
# mean_breaks     <- c(1.15, 1.5, 2, 4)
# contrib_breaks  <- c(1, 2, 5, 10)
#
# pct_ci_breaks   <- pct_breaks - pct_breaks[1]
# mean_ci_breaks  <- mean_breaks / mean_breaks[1]
#
# pct_brksup      <- c(pct_breaks    [2:length(pct_breaks)    ], Inf)
# mean_brksup     <- c(mean_breaks   [2:length(mean_breaks)   ], Inf)
# contrib_brksup  <- c(contrib_breaks[2:length(contrib_breaks)], Inf)
# pct_ci_brksup   <- c(pct_ci_breaks [2:length(pct_ci_breaks) ], Inf)
# mean_ci_brksup  <- c(mean_ci_breaks[2:length(mean_ci_breaks)], Inf)
#
# pct_breaks         <- pct_breaks     %>% c(., -.)
# mean_breaks        <- mean_breaks    %>% c(., 1/.)
# contrib_breaks     <- contrib_breaks %>% c(., -.)
# pct_ci_breaks      <- pct_ci_breaks  %>% c(., -.)
# mean_ci_breaks     <- mean_ci_breaks %>% c(., -.) #then - again
#
# pct_brksup      <- pct_brksup     %>% c(., -.)
# mean_brksup     <- mean_brksup    %>% c(., 1/.)
# contrib_brksup  <- contrib_brksup %>% c(., -.)
# pct_ci_brksup   <- pct_ci_brksup  %>% c(., -.)
# mean_ci_brksup  <- mean_ci_brksup %>% c(., -.) #then - again


#' Get the breaks currently used to print colors
#' @describeIn tab_many get the breaks currently used to print colors
#' @param brk When missing, return all color breaks. Specify to return a given color
#' break, among \code{"pct"}, \code{"mean"}, \code{"contrib"}, \code{"pct_ci"} and
#' \code{"mean_ci"}.
#' @param type Default to \code{"positive"}, which just print breaks for positive spreads.
#' Set to \code{all} to get breaks for negative spreads as well.
#'
#' @return The color breaks as a double vector, or list of double vectors.
#' @export
get_color_breaks <- function(brk, type = c("positive", "all")) {
  tabxplor_color_breaks <- getOption("tabxplor.color_breaks")

  breaks <-
    if (missing(brk)) {
      return(
        list(pct_breaks     = tabxplor_color_breaks$pct_breaks,
             mean_breaks    = tabxplor_color_breaks$mean_breaks,
             contrib_breaks = tabxplor_color_breaks$contrib_breaks,
             pct_ci_breaks  = tabxplor_color_breaks$pct_ci_breaks,
             mean_ci_breaks = tabxplor_color_breaks$mean_ci_breaks)
      )
    } else {
      switch (brk,
              "pct"     = tabxplor_color_breaks$pct_breaks    ,
              "mean"    = tabxplor_color_breaks$mean_breaks   ,
              "contrib" = tabxplor_color_breaks$contrib_breaks,
              "pct_ci"  = tabxplor_color_breaks$pct_ci_breaks ,
              "mean_ci" = tabxplor_color_breaks$mean_ci_breaks  )
    }

  if (type[1] == "positive") breaks <- breaks[1:((length(breaks) - 1) / 2)]

  breaks
}

# get_color_breaks()
#
# set_color_breaks(pct_breaks = c(0.05, 0.10, 0.15, 0.25, 0.35))

# get_full_color_breaks()

# pct_breaks     = c(0.05, 0.10, 0.15, 0.25, 0.35)
# mean_breaks    = c(1.15, 1.25, 1.5 , 2   , 4   )
# contrib_breaks = c(0.5 , 1   , 2   , 5   , 10  )








# Tests -----
# new_tab() %>% get_chi2()
# new_tab() %>% get_total_table()
# new_tab() %>% get_subtext()

# vec_ptype2(new_tab(), new_tab()) %>% attributes()
#
# vec_rbind(red, red)
# vec_rbind(green, green)
# vec_rbind(green, red)
#
# vec_rbind(red, tibble::tibble(x = 10:12))
# vec_rbind(red, data.frame(x = 10:12))



# vctrs documentation --------------------------------------------------------------------

# howto-faq-coercion-data-frame
# FAQ - How to implement ptype2 and cast methods? (Data frames)
# Description
# This guide provides a practical recipe for implementing vec_ptype2() and vec_cast() methods
# for coercions of data frame subclasses. Related topics:
#  - For an overview of the coercion mechanism in vctrs, see ?theory-faq-coercion.
#  - For an example of implementing coercion methods for simple vectors, see ?howto-faq-coercion.
# Coercion of data frames occurs when different data frame classes are combined in some way. The
# two main methods of combination are currently row-binding with vec_rbind() and col-binding
# with vec_cbind() (which are in turn used by a number of dplyr and tidyr functions). These functions
# take multiple data frame inputs and automatically coerce them to their common type.
# vctrs is generally strict about the kind of automatic coercions that are performed when combining
# inputs. In the case of data frames we have decided to be a bit less strict for convenience. Instead of
# throwing an incompatible type error, we fall back to a base data frame or a tibble if we don't know
# how to combine two data frame subclasses. It is still a good idea to specify the proper coercion
# behaviour for your data frame subclasses as soon as possible.
# We will see two examples in this guide. The first example is about a data frame subclass that has
# no particular attributes to manage. In the second example, we implement coercion methods for a
# tibble subclass that includes potentially incompatible attributes.

# Roxygen workflow:
#   To implement methods for generics, first import the generics in your namespace and redocument:
#   #' @importFrom vctrs vec_ptype2 vec_cast
#   NULL
# Note that for each batches of methods that you add to your package, you need to export the
# methods and redocument immediately, even during development. Otherwise they won't be in
# scope when you run unit tests e.g. with testthat.
# Implementing double dispatch methods is very similar to implementing regular S3 methods. In
# these examples we are using roxygen2 tags to register the methods, but you can also register the
# methods manually in your NAMESPACE file or lazily with s3_register().

# Parent methods:
#   Most of the common type determination should be performed by the parent class. In vctrs, double
# dispatch is implemented in such a way that you need to call the methods for the parent class manually.
# For vec_ptype2() this means you need to call df_ptype2() (for data frame subclasses) or
# tib_ptype2() (for tibble subclasses). Similarly, df_cast() and tib_cast() are the workhorses
# for vec_cast() methods of subtypes of data.frame and tbl_df. These functions take the union
# of the columns in x and y, and ensure shared columns have the same type.
# These functions are much less strict than vec_ptype2() and vec_cast() as they accept any
# subclass of data frame as input. They always return a data.frame or a tbl_df. You will probably
# want to write similar functions for your subclass to avoid repetition in your code. You may want
# to export them as well if you are expecting other people to derive from your class.

# A data.tabxplor_tab le example:
# [...]

# #A tibble example:
# #  In this example we implement coercion methods for a tibble subclass that carries a colour as a
# #scalar metadata:
#
#   # User constructor
#   my_tibble <- function(colour = NULL, ...) {
#     new_my_tibble(tibble::tibble(...), colour = colour)
#   }
# # Developer constructor
# new_my_tibble <- function(x, colour = NULL) {
#   stopifnot(is.data.frame(x))
#   tibble::new_tibble(
#     x,
#     colour = colour,
#     class = "my_tibble",
#     nrow = nrow(x)
#   )
# }
# df_colour <- function(x) {
#   if (inherits(x, "my_tibble")) {
#     attr(x, "colour")
#   } else {
#     NULL
#   }
# }
# #'@export
# print.my_tibble <- function(x, ...) {
#   cat(sprintf("<%s: %s>\n", class(x)[[1]], df_colour(x)))
#   cli::cat_line(format(x)[-1])
# }
# #This subclass is very simple. All it does is modify the header.
# red <- my_tibble("red", x = 1, y = 1:2)
# red
# #> <my_tibble: red>
# #> x y
# #> <dbl> <int>
# #> 1 1 1
# #> 2 1 2
# red[2]
# #> <my_tibble: red>
# #> y
# #> <int>
# #> 1 1
# #> 2 2
# green <- my_tibble("green", z = TRUE)
# green
# #> <my_tibble: green>
# #> z
#
# #> <lgl>
# #> 1 TRUE
# #Combinations do not work properly out of the box, instead vctrs falls back to a bare tibble:
#   vec_rbind(red, tibble::tibble(x = 10:12))
# #> # A tibble: 5 x 2
# #> x y
# #> <dbl> <int>
# #> 1 1 1
# #> 2 1 2
# #> 3 10 NA
# #> 4 11 NA
# #> 5 12 NA
# # Instead of falling back to a data frame, we would like to return a <my_tibble> when combined
# # with a data frame or a tibble. Because this subclass has more metadata than normal data frames
# # (it has a colour), it is a supertype of tibble and data frame, i.e. it is the richer type. This is similar
# # to how a grouped tibble is a more general type than a tibble or a data frame. Conceptually, the
# # latter are pinned to a single constant group.
