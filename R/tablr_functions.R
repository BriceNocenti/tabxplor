
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
#' @param .f A \code{\link[purrr]{map}} style function or ~ formula. In fact
#' \code{\link[purrr]{modify}} is used to preserve the attributes of the tables,
#' because \code{\link[purrr]{map}} remove them.
#'
#'
#' @return a table or list of tables the same length as tabs.
#' @export
#'
#' @examples
#' #To calculate row percentages, then cells deviations from columns means :
#' \dontrun{
#' forcats::gss_cat %>%
#'   tabw(marital, race, perc = "row") %>%
#'   tab_map(~ dplyr::mutate_at(., -1, ~ . - dplyr::last(.)))
#'   }
tab_map <- function(tabs, .f) {
  if ("single_tabr" %in% class(tabs) | "tabr_df" %in% class(tabs) ) {
    #tabs_attr <- tabs %>% attributes()
    purrr::modify_depth(tabs, 0, .f) #%>% `attributes<-`(tabs_attr)

  } else if ("tabr" %in% class(tabs) | all(purrr::map_lgl(tabs, ~ "tabr_df" %in% class(.)))) {
    #tabs_attr <- tabs %>% attributes()
    purrr::modify_depth(tabs, 1, .f) #%>% `attributes<-`(tabs_attr)
  } else if (all(purrr::map_lgl(tabs, ~ "tabr" %in% class(.)))) {
    # vars_depth <- purrr::map(1:max(1, purrr::vec_depth(tabs) - 2), ~ purrr::map_depth(tabs, ., ~ "tabr" %in% class(.))) %>% purrr::transpose() %>%
    #   purrr::map(purrr::flatten) %>% purrr::map(purrr::flatten_lgl) %>% purrr::map_int(which)
    #tabs_attr <- purrr::map(tabs, attributes)
    purrr::map_depth(tabs, 2, .f) #%>% purrr::map2(tabs_attr, ~`attributes<-`(.x, .y))
  } else if (all(purrr::map_depth(tabs, 2, ~ "tabr" %in% class(.)) %>%
                 purrr::map(~purrr::flatten_lgl(.)) %>%
                 purrr::flatten_lgl())) {
    #tabs_attr <- purrr::map_depth(tabs, 2, attributes)
    purrr::map_depth(tabs, 3, .f) #%>% purrr::map2(tabs_attr, ~ purrr::map2(.x, .y, ~ `attributes<-`(.x, .y)))
  }  else {
    stop("some elements of the list are not of class tabr, single_tabr or tabr_df")
  }
}


# @param confidence_intervals,conf_level,design_effect Set
# \code{confidence_intervals = TRUE} to print tables with confidence intervals
#  (for the selected kind of percentages). \code{conf_level} defines a
#  confidence level (number between 0 and 1). Set \code{design_effect = TRUE}
#  to increase the confidence intervals, depending on how the \code{wt}
#   weight variable distort the structure of data.

# tab_ci <- function(dat) {
#
#     if (confidence_intervals[1] > 0 & result[1] != "observed") {
#result <- "observed"
# warning("confidence intervals can only be shown with result = 'observed'")
# }
# stopifnot(class(conf_level) == "numeric",  conf_level > 0, conf_level < 1)
#
#
#   dat_group3  <- dat %>% dplyr::group_by(!!var3, .drop = FALSE)
#   dat_group123 <- dat_group3 %>% dplyr::group_by(!!var1, !!var2, .add = TRUE, .drop = FALSE)
#
#   wtable <- dat_group123 %>% dplyr::summarise(n =dplyr::n(), weighted_n = sum(!!wt), wn2 = sum((!!wt)^2), .groups = "drop")
#
#   wtable %<>% dplyr::group_by(!!var1, !!var2)
#   wtable %<>%
#     dplyr::bind_rows(dplyr::summarise(., .zone = factor("base"), !!var3 := factor("Total"),.TYPE = factor("factor"),
#                                       n = sum(n), weighted_n = sum(weighted_n), wn2 = sum(wn2), .groups = "drop"))
#
#   wtable %<>%
#     dplyr::mutate(deff = sum(n) * sum(wn2)/(sum(weighted_n)^2))
#
#
#   wtable %<>%
#     dplyr::group_by(!!var3, !!var1) %>%
#     dplyr::bind_rows(dplyr::summarise(., .zone = factor("base"), !!var2 := factor("Total"),
#                                       n = dplyr::first(.tot1), weighted_n = dplyr::first(.wtot1), deff = dplyr::first(deff), .TYPE = factor("factor"),
#                                       .tot1 = dplyr::first(.tot1), .wtot1 = dplyr::first(.wtot1), nbrow = dplyr::first(nbrow),
#                                       .tot2 = dplyr::first(.tot3), .wtot2 = dplyr::first(.wtot3), nbcol = dplyr::first(nbcol),
#                                       .tot3 = dplyr::first(.tot3), .wtot3 = dplyr::first(.wtot3),
#                                       .totn = dplyr::first(.totn), .wtotn = dplyr::first(.wtotn), .groups = "drop")) %>%
#     dplyr::group_by(!!var3, !!var2) %>%
#     dplyr::bind_rows(dplyr::summarise(., .zone = factor("base"), !!var1 := factor("Total"),
#                                       n = dplyr::first(.tot2), weighted_n = dplyr::first(.wtot2), deff = dplyr::first(deff), .TYPE = factor("factor"),
#                                       .tot1 = dplyr::first(.tot3), .wtot1 = dplyr::first(.wtot3), nbrow = dplyr::first(nbrow),
#                                       .tot2 = dplyr::first(.tot2), .wtot2 = dplyr::first(.wtot2), nbcol = dplyr::first(nbcol),
#                                       .tot3 = dplyr::first(.tot3), .wtot3 = dplyr::first(.wtot3),
#                                       .totn = dplyr::first(.totn), .wtotn = dplyr::first(.wtotn), .groups = "drop"))
#
#
#   wtable %<>% dplyr::group_by(!!var3, !!var1) %>%
#     dplyr::bind_rows(dplyr::summarise(., .zone = factor("base"), !!var2 := factor("Total"),
#                                       weighted_n = dplyr::first(.wtot1), deff = dplyr::first(deff), .TYPE = factor("factor"),
#                                       .wtot1 = dplyr::first(.wtot1), .wtot2 = dplyr::first(.wtot3), .wtot3 = dplyr::first(.wtot3),
#                                       .wtotn = dplyr::first(.wtotn), .groups = "drop")) %>%
#     dplyr::group_by(!!var3, !!var2) %>%
#     dplyr::bind_rows(dplyr::summarise(., .zone = factor("base"), !!var1 := factor("Total"),
#                                       weighted_n = dplyr::first(.wtot2), deff = dplyr::first(deff), .TYPE = factor("factor"),
#                                       .wtot1 = dplyr::first(.wtot3), .wtot2 = dplyr::first(.wtot2),.wtot3 = dplyr::first(.wtot3),
#                                       .wtotn = dplyr::first(.wtotn), .groups = "drop"))
#
#   #Confidence intervals :
#   if (confidence_intervals == TRUE) {
#     if (perc[1] == "row") {
#       perc_tot <- rlang::expr(.tot1)
#     } else if (perc[1] == "col")            {
#       perc_tot <- rlang::expr(.tot2)
#     } else if (perc[1] %in% c("all", "no")) {
#       perc_tot <- rlang::expr(.tot3)
#     } else if (perc[1] == "all_tabs")       {
#       perc_tot <- rlang::expr(.totn)
#     }
#     wtable %<>%
#       dplyr::mutate(moe = DescTools::BinomCI(pct*!!perc_tot, !!perc_tot, method = "wilson",
#                                              conf.level = conf_level) %>% as.data.frame() %>%
#                       dplyr::mutate(ci = (upr.ci - est)) %>% dplyr::pull(ci) %>% tidyr::replace_na(0) %>% round(7),
#                     conf_level = conf_level)
#     if (design_effect == TRUE) wtable %<>% dplyr::mutate(ci = ci * deff)
#
#     if (perc[1] == "no"){
#       wtable %<>%
#         dplyr::mutate(moe_wn = moe * .wtot3,
#                       res_min = round(pmax(weighted_n - moe_wn, 0)),  #stringr::str_replace_all(., "(100)\\.0+%", "\\1%")
#                       res_max = round(weighted_n + moe_wn), #stringr::str_replace_all(., "(100)\\.0+%", "\\1%")
#                       resCI = dplyr::if_else(res_min == res_max, stringr::str_c(res_min), stringr::str_c(res_min, "-", res_max) ))
#
#     } else {
#       wtable %<>%
#         dplyr::mutate(res_min = round(pmax((pct - moe)*100, 0), digits),  #stringr::str_replace_all(., "(100)\\.0+%", "\\1%")
#                       res_max = round(pmin((pct + moe)*100, 100), digits), #stringr::str_replace_all(., "(100)\\.0+%", "\\1%")
#                       resCI = dplyr::if_else(res_min == res_max, stringr::str_c(res_min, "%"), stringr::str_c(res_min, "-", res_max, "%") ) ) #stringr::str_remove(., "%(?=-)") %>% stringr::str_replace_all("\\.", ",")
#       #stringr::str_pad(res_min, max(stringr::str_length(res_min[res_min != res_max])))
#     }
#     wtable %<>% dplyr::select(-res_min, - res_max)
#   }
#
#   # if ( CItype[1] == "range") {
#   #   }
#   # else if ( CItype[1] == "+-" ) {
#   #   wtable %<>% purrr::map(~ adorn_pct_formatting(., digits = digits) %>%
#   #                     dplyr::mutate_at(dplyr::vars(-1), ~ stringr::str_replace_all(., "(100)\\.0+%", "\\1%")))
#   #   CI  %<>% purrr::map(~ adorn_pct_formatting(., digits = max(digits, 1)) %>%
#   #                  dplyr::mutate_at(dplyr::vars(-1), ~ stringr::str_remove_all(., "%") %>%
#   #                              stringr::str_remove_all("^-$|^0\\.0+$|^0$") ) %>%
#   #                  dplyr::mutate_at(dplyr::vars(1), ~ "") )
#   #   wtable <-
#   #     purrr::map2(wtable, CI, ~ purrr::map2_df(.x, .y, function(..x, ..y)
#   #       stringr::str_c(..x, #stringr::str_pad(..x, 3 + digits, pad = "&")  %>% stringr::str_replace_all("&", "  "),
#   #             stringr::str_pad(stringr::str_c(" (\\u00b1", ..y, ")"), 7 + max(digits, 1), pad = "&") %>%
#   #               stringr::str_replace_all("&", "  ") %>% stringr::str_replace("( *)\\(\\u00b1( *)\\)$", "\\1    \\2")
#   #       )  )) %>% magrittr::set_names(sheet_names[1:length(.)]) %>%
#   #     purrr::map(~ dplyr::mutate_at(., dplyr::vars(-1), ~ stringr::str_replace_all(., "\\.", ",") %>%
#   #                       stringr::str_replace("(100%) *", "\\1") ) %>%
#   #           dplyr::mutate_at(dplyr::vars(1), ~ stringr::str_remove_all(., " *\\(*\\u00b1 *\\)*") ) %>%
#   #           dplyr::mutate(llllllllll = 1) %>% as_tabyl() %>% dplyr::select(-llllllllll))
#   # }
#   # # Print :
#   # if (confidence_intervals[1] > 0 ) {
#   #   wtable %<>% dplyr::mutate_at(dplyr::vars(-1), ~ stringr::str_replace(., "  \\(\\u00b1", " (\\u00b1") %>%
#   #                           stringr::str_replace("  +\\(\\u00b1", "  (\\u00b1") %>%
#   #                           stringr::str_replace(" {13}$", "        ") ) %>%
#   #     dplyr::mutate_at(dplyr::vars(-1), ~ stringr::str_c(" ", .))
#   # }
#
#   # #Table de Gauss
#   # tibble::tibble(Effectifs = c(50, 100, 200, 500, 1000, 2000, 5000, 10000, 100000),
#   #        `1% (ou 99%)` = Effectifs*0.01,`2% (98%)` = Effectifs*0.02,
#   #        `5% (95%)`= Effectifs*0.05,`10% (90%)` = Effectifs*0.1,
#   #        `20% (80%)` = Effectifs*0.2,`50%` = Effectifs*0.5 ) %>%
#   #   tidyr::pivot_longer(cols = -1) %>%
#   #   dplyr::mutate(moeWi = BinomCI(value, Effectifs, method = "wilson"),
#   #          moe11 = BinomCI(value, Effectifs, method = "wald"),
#   #          moe12 = BinomCI(value, Effectifs, method = "agresti-coull"),
#   #          moe13 = BinomCI(value, Effectifs, method = "jeffreys"),
#   #          moe14 = BinomCI(value, Effectifs, method = "modified wilson"),
#   #          moe15 = BinomCI(value, Effectifs, method = "wilsoncc"),
#   #          moe16 = BinomCI(value, Effectifs, method = "modified jeffreys"),
#   #          moe17 = BinomCI(value, Effectifs, method = "clopper-pearson"),
#   #          moe18 = BinomCI(value, Effectifs, method = "logit"),
#   #          moe19 = BinomCI(value, Effectifs, method = "arcsine")) %>%
#   #   dplyr::mutate_at(dplyr::vars(starts_with("moe")),
#   #             ~ as.data.frame(.) %>% dplyr::mutate(ci = upr.ci - est) %>% dplyr::pull(ci)) %>%
#   #   dplyr::mutate_at(dplyr::vars(starts_with("moe")), ~round(. * 1 * 100, 1)) %>%
#   #   dplyr::mutate_at(dplyr::vars(starts_with("moe1")), ~ (. - moeWi)) %>%
#   #   dplyr::mutate(moe2 = moedeff_calc(pct = value/Effectifs, deff = 1, n = Effectifs) %>% round(1)) %>%
#   #   dplyr::mutate(pct = round(value/Effectifs * 100, 1)) %>%
#   #   View()
#
# }
