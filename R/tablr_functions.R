
# Enhancements : it must apply on attributes if modifications are to be kept with tabxl()
bind_tabs <- function(tabs, by = c("col", "row"), change_names) {
  if (!missing(change_names)) {
    tabs %<>%
      purrr::map(~ magrittr::set_colnames(.,
                                          purrr::reduce2(
                                            change_names, names(change_names),
                                            .init = colnames(.),
                                            .f = ~ stringr::str_replace(..1, stringr::str_c("^", ..2, "$"), ..3)) )   )
  }

  if (by[1] == "col") {
    tabs[-1] %>%
      purrr::reduce2(1:length(tabs[-1]), .init = tabs[[1]],
                     .f = ~ dplyr::full_join(..1, ..2,
                                             by = colnames(..2)[1] %>% magrittr::set_names(colnames(..1)[1]),
                                             suffix = stringr::str_c(c(..3, ..3 + 1))) )
  } else if (by[1] == "row") {
    tabs %>% dplyr::bind_rows()
  }
}


#' Apply a function to each table.
#'
#' @param tabs A \code{\link{single_tabr}}, many of them gathered in
#' a \code{\link{tabr}}, or a list of \code{\link{tabr}}.
#' @param .f A \code{\link[purrr]{map}} style function or ~ formula.
#'
#' @return a table or list of tables the same length as tabs.
#' @export
#'
#' @examples
#' #To calculate row percentages, then cells deviations from columns means :
#' forcats::gss_cat %>%
#'   tabw(marital, race, perc = "row") %>%
#'   tab_map(~ dplyr::mutate_at(., -1, ~ . - dplyr::last(.)))
tab_map <- function(tabs, .f) {
  if ("single_tabr" %in% class(tabs)) {
    tabs_attr <- tabs %>% attributes()
    purrr::map_depth(tabs, 0, .f) %>% `attributes<-`(tabs_attr)
  } else if ("tabr" %in% class(tabs)) {
    tabs_attr <- tabs %>% attributes()
    purrr::map_depth(tabs, 1, .f) %>% `attributes<-`(tabs_attr)
  } else if (all(purrr::map_lgl(tabs, ~ "tabr" %in% class(.)))) {
    # vars_depth <- purrr::map(1:max(1, purrr::vec_depth(tabs) - 2), ~ purrr::map_depth(tabs, ., ~ "tabr" %in% class(.))) %>% purrr::transpose() %>%
    #   purrr::map(purrr::flatten) %>% purrr::map(purrr::flatten_lgl) %>% purrr::map_int(which)
    tabs_attr <- purrr::map(tabs, attributes)
    purrr::map_depth(tabs, 2, .f) %>% purrr::map2(tabs_attr, ~`attributes<-`(.x, .y))
  } else if (all(purrr::map_depth(tabs, 2, ~ "tabr" %in% class(.)) %>%
                 purrr::map(~purrr::flatten_lgl(.)) %>%
                 purrr::flatten_lgl())) {
    tabs_attr <- purrr::map_depth(tabs, 2, attributes)
    purrr::map_depth(tabs, 3, .f) %>% purrr::map2(tabs_attr, ~ purrr::map2(.x, .y, ~ `attributes<-`(.x, .y)))
  }  else {
    stop("some elements of the list are not of class tabr or single_tabr")
  }
}

