

# #Multicols et multirows :

# dat <- ct2013acm %>% dplyr::rename(`Nombre de contraintes (moy)` = NBCONTR)
# tabs <- dat %>%
#   tabw(EMP, var3 = PR0, sup_cols = list_of_vars, wt = pondcal, cleannames = TRUE, show_na = FALSE, perc = "row", multicols = TRUE) #%>%
#tabw(var2 = EMP, var3 = PR0, sup_rows = list_of_vars, wt = pondcal, cleannames = TRUE, show_na = FALSE, multirows = TRUE) %>%
#tabw(EMP, PR0)

# #Multirows :
# tabs <-
#   tabw(dat, var2 = EMP2, var3 = PR0, sup_rows = list_of_vars, wt = pondcal, cleannames = TRUE, show_na = FALSE, multirows = TRUE)

# tabs <-  ct2013acm %>%
#   tabw(PR0, cah, EMP, wt = pondcal, show_na = FALSE, cleannames = TRUE, perc = "row", tot = "no", sup_cols = c("RWDEMacm", "NBCONTR"), sup_rows = "CONTRADacm", another_total = TRUE) %>%
#   tabw(PE0, cah, wt = pondcal, tot = "all") %>%
#   tabw(EMP, cah, wt = pondcal, tot = "col") %>%
#   tabw(PE0, cah, wt = pondcal, tot = "row", sup_cols = c("RWDEMacm", "NBCONTR"), sup_rows = "CONTRADacm")

# tabs <-  ct2013acm %>%
#   tabw(PR0, cah, EMP, wt = pondcal, show_na = FALSE, cleannames = TRUE, perc = "col", tot = "no",
#        sup_rows = c("RWDEMacm", "NBCONTR"), sup_cols = c("RWDEMacm", "NBCONTR"), another_total = TRUE, sup_contrib = TRUE) %>%
#   tabw(PE0, cah, wt = pondcal, tot = "all", perc = "no", sup_rows = c("RWDEMacm", "NBCONTR"), sup_cols = c("RWDEMacm", "NBCONTR")) %>%
#   tabw(EMP, cah, wt = pondcal, tot = "col", sup_rows = c("RWDEMacm", "NBCONTR"), sup_cols = c("RWDEMacm", "NBCONTR")) %>%
#   tabw(PE0, cah, wt = pondcal, tot = "row", sup_cols = c("RWDEMacm", "NBCONTR"), sup_rows = c("RWDEMacm", "NBCONTR"), perc = "all")


# tabs <-  ct2013acm %>%
#   tabw(PR0, cah, EMP, wt = pondcal, show_na = FALSE, cleannames = TRUE, perc = "row",
#        sup_rows = c("RWDEMacm", "NBCONTR"), sup_cols = c("RWDEMacm", "NBCONTR"), another_total = TRUE, sup_contrib = TRUE)




# Mettre en forme la sortie Excel d'un tableau :
# BUGS :                  - If tabs are modified with tab_map, everything recalculated from tab_df is false... #####
#                         - Now test is var1 and var2 levels are se same on tabs/wtables : ambiguous
#                         - Prepare another total directly on tab_xl, without tab_draw ? Make it auto ?
#                         - Sup_cols print the Total col again.
#                         - S’il manque une colonne sup quand plusieurs tabs par page, aligner avec les bonnes colonnes.
#                         - Another total, sup_cols et insufficient headcount ne sont pas classées via sort_by...
#                         - Refaire l’articulation des différents tableaux avec left_join (avec warnings si changements)
#                         - "sheetName too long! Max length is 31 characters"
# Possibilites d'ajouts : - variance, pvalue, etc. : dans un tableau après chaque liste of tabs, compact par défaut (ou à droite et pas dessous?)
#                         - only_one_sheet ne fonctionne pas du tout avec des variables differentes en col
#                         - auto on same sheet when var2 levels are the same
#                         - (utiliser le format table de Excel ?)
#                         - Best fonts. Monospace : DejaVu Sans Mono (Source Code Pro Medium)
#                         - openxlsx::addStyle(stack = TRUE) # If TRUE the new style is merged with any existing cell styles
#                         - Plus que de sauter une petite ligne entre tableaux : une double bordure (voir si OK word)
#                         - Dans Excel, marge d’erreur sous forme de calcul (ou plusieurs colonnes) pour garder chiffre exact
#                         - Calcul des marges d’erreur dans l’espace à droite (en répétant chaque colonne pour détailler le calcul ?)
#                         - Pour calculer les MOE des variables quanti, il faut une colonne SD systématiquement (ou l’info en nested).
#                         - Proposer plusieurs calcul de couleurs (signif / moyenne, lignes deux à deux, tableau à tableau, moyenne
#                         d’ensemble....), avec possibilité d’en choisir plusieurs, générant plusieurs tableaux.
#                         - Comparaison paire par paire : le mieux c’est un autre tableau ou les paires sont regroupées, sans ligne total
#                         (avec une croisement entre la troisième variable et la variable « paires » ?).

#' Crosstabs : Excel output with conditional formatting
#'
#' @param tabs Table(s) created with \code{\link{tab}}. Strictly speaking, it
#' can be an object of class \code{\link{single_tab}}, several of them
#' gathered in a \code{\link{tab}}, or a list code{\link{tab}}.
#' @param path,replace,open The name, and possibly the path, of the Excel file
#' to create (without the .xlsx extension). Default path to working directory.
#' Use \code{replace = TRUE} to overwrite existing files. Use \code{open = TRUE}
#' if you don't want to automatically open the tables in Excel (or another
#' software associated with .xlsx files).
#' @param colnames_rotation Rotate the names of columns to an angle (in degrees).
#' @param digits_perc Number of digits to print for percentages.
#' @param digits_no_perc Number of digits to print for counts.
#' @param digits_quanti Number of digits to print for quantitative variables.
#' @param only_one_sheet When \code{TRUE}, all tables are printed on the
#'  same sheet.
#' @param color Color cells, based on their deviation from the mean, using
#' Excel conditional formatting. In rows and cols, attractions /
#' over-represented levels are colored in shades of green,
#' and aversions / under-represented levels are colored in shades of orange.
#' \itemize{
#'    \item \code{"auto"} : tabs with \code{"row"} or \code{"col"} percentages
#'    are colored based on deviations from margins,
#'    other tabs on contributions to variance
#'    \item \code{"contrib"} : for all tabs, color intensity is based on the
#'    relative contribution of cells to variance (Chi2 metric)
#'    \item \code{"no"} : no conditional formatting
#'    }
#' @param hide_near_zero By default, values printed as 0 are automatically
#' colored in light gray in Excel, to help to focus on other ones. Use doubles
#' to set the value under which numbers are colored in gray.
#' \code{hide_near_zero = Inf} if you don't want to use this feature.
#' @param compact Create the more compact table possible by removing
#' unnecessary rows (Chi2, unweighted counts, \code{sup_rows}). Useful if there
#' is \code{var3}/\code{tab_var}.
#' @param pct_breaks The values to use to color deviations from the mean,
#' when the table shows \code{"row"} or \code{"col"} percentages and
#' \code{color = "auto"}. They must be numbers between - 1 and 1
#' (i.e. -100% and +100%).
#' @param contrib_breaks The values to use to color cell
#' contributions to variance : \code{1} means above/under the average
#' contribution of cells to variance, \code{10} means 10 times the average
#' contribution. Must be positive numbers for
#' attractions (green), negative numbers for repulsions (orange).
#'
#' @return An Excel file with the tables as a side-effect. Invisibly returns
#' \code{tabs}.
#' @export
#'
#' @examples
#' \dontrun{
#' forcats::gss_cat %>%
#' tabw(marital, race, perc = "row") %>%
#'   tab_xl()
#'
#' forcats::gss_cat %>%
#'   tabw(marital, race) %>%
#'   tab_xl()
#'   }
tab_xl <-
  function(tabs, path = "Tabs\\Tab", replace = FALSE, open = rlang::is_interactive(),
           colnames_rotation = 0, remove_tab_vars = TRUE,
           colwidth = "auto",
           only_one_sheet = FALSE, min_counts = 30,
           color = "auto", #c("auto", "contrib", "diff", "diff_no_ci", "diff_after_ci", "no"), # just_row_col ?
           comp = "all",
           hide_near_zero = "auto", #c("auto", 0.0049, Inf),
           #no_contribs = FALSE, no_sup = FALSE, no_another_tot = FALSE, no_sup = FALSE, #title
           pct_breaks     = get_color_breaks("pct"),
           mean_breaks    = get_color_breaks("mean"),
           contrib_breaks = get_color_breaks("contrib") #c(1, 2, 5, -1,-2, -5)
  ) {
    stopifnot(length(pct_breaks    ) >= 1,
              length(mean_breaks   ) >= 1,
              length(contrib_breaks) >= 1 )

    if (is.data.frame(tabs)) tabs <- list(tabs)
    subtext <- purrr::map(tabs, get_subtext)
    chi2    <- purrr::map(tabs, get_chi2)
    colwidth       <- vec_recycle(colwidth,       length(tabs))
    hide_near_zero <- vec_recycle(hide_near_zero, length(tabs))
    comp           <- vec_recycle(comp,           length(tabs))
    color <- vec_recycle(color, length(tabs))

    get_vars        <- purrr::map(tabs, tab_get_vars)
    col_vars_levels_alltot <- purrr::map(get_vars, ~ purrr::map(.$col_vars_levels, rlang::syms))
    col_vars_levels <- purrr::map(col_vars_levels_alltot, ~ .[names(.) != "all_col_vars"] )
    tab_vars <- purrr::map(get_vars, ~ .$tab_vars)

    #needed args attributes : ----
    #insufficient_counts

    insc <- insufficient_counts(tabs, min_counts = min_counts)
    insuff_counts_row_var <- insc$insuff_counts_row_var
    insuff_counts_col_var <- insc$insuff_counts_col_var

    no_chi2 <- purrr::map_lgl(chi2, ~ nrow(.) == 0)

    prep_tabs_chi2 <- tabs[!no_chi2] %>%
      purrr::map(~ tibble::add_column(., totrows = is_totrow(.)) %>%
                   dplyr::mutate(`chi2 stats` = dplyr::case_when(
                     totrows                                       ~ NA_character_,
                     dplyr::lead(totrows, n = 1L, default = FALSE) ~ "count"   ,
                     dplyr::lead(totrows, n = 2L, default = FALSE) ~ "pvalue"  ,
                     dplyr::lead(totrows, n = 3L, default = FALSE) ~ "variance",
                     dplyr::lead(totrows, n = 4L, default = FALSE) ~ "cells"   ,
                     dplyr::lead(totrows, n = 5L, default = FALSE) ~ "df"      ,
                     TRUE                                          ~ NA_character_
                   )) %>% dplyr::select(-totrows, - where(is_fmt)) %>%
                   dplyr::ungroup()
      )

    tabs_chi2 <- rep(list(tibble::tibble()), length(chi2))
    tabs_chi2[!no_chi2] <-
      purrr::pmap(list(prep_tabs_chi2, chi2[!no_chi2],
                       purrr::map(tab_vars[!no_chi2], ~ append(., "chi2 stats")) ),
                  ~ dplyr::left_join(..1, ..2, by = ..3, suffix = c(" ", "")) %>%
                    dplyr::select(`chi2 stats`, where(is_fmt)) %>%
                    dplyr::mutate(dplyr::across(
                      where(is_fmt),
                      function(.var) tidyr::replace_na(.var, fmt0(type = "var"))
                    )) %>%
                    dplyr::mutate(dplyr::across(
                      where(is_fmt), get_num
                      #as_totrow(.var, FALSE) %>% as_tottab(FALSE)
                    )) %>% dplyr::mutate(`chi2 stats` =
                                           stringr::str_c(`chi2 stats`, " :"))
      )

    nbcells <- rep(list(tibble::tibble()), length(chi2))
    nbcells[!no_chi2] <-  chi2[!no_chi2] %>%
      purrr::map(~dplyr::filter(., `chi2 stats` == "cells" ) %>%
                   dplyr::select(-`chi2 stats`, -tidyselect::any_of("row_var")) %>%
                   dplyr::mutate(dplyr::across(where(is_fmt),
                                               ~ as.integer(get_num(.)))) %>%
                   dplyr::select(-where(~ is.integer(.) & all(is.na(.))))
      )
    nbcells[!no_chi2]  <-
      purrr::pmap(list(tabs[!no_chi2], nbcells[!no_chi2], tab_vars[!no_chi2]),
                  ~ dplyr::ungroup(dplyr::select(..1, -where(is_fmt))) %>%
                    dplyr::left_join(..2, by = ..3, suffix = c(".var", ""))
      )

    pvalues <- rep(list(tibble::tibble()), length(chi2))
    pvalues[!no_chi2] <-  chi2[!no_chi2] %>%
      purrr::map(~dplyr::filter(., `chi2 stats` == "pvalue" ) %>%
                   dplyr::select(-`chi2 stats`, -tidyselect::any_of("row_var")) %>%
                   dplyr::mutate(dplyr::across(where(is_fmt),
                                               ~ get_num(.) <= 0.05)) %>%
                   dplyr::select(-where(~ is.logical(.) & all(is.na(.))))
      )
    pvalues[!no_chi2]  <-
      purrr::pmap(list(tabs[!no_chi2], pvalues[!no_chi2], tab_vars[!no_chi2]),
                  ~ dplyr::ungroup(dplyr::select(..1, -where(is_fmt))) %>%
                    dplyr::left_join(..2, by = ..3, suffix = c(".var", ""))
      )

    if (remove_tab_vars == TRUE) {
      tabs <-
        purrr::map2(tabs, tab_vars, ~ dplyr::select(dplyr::ungroup(.x),
                                                    -tidyselect::all_of(.y)))
      insuff_counts_col_var <- insuff_counts_col_var %>%
        purrr::map2(tab_vars, ~ dplyr::select(dplyr::ungroup(.x),
                                              -tidyselect::all_of(.y)))
    }

    no_ci <-
      purrr::map_lgl(tabs, ~ purrr::map_lgl(dplyr::select(., where(is_fmt)),
                                            ~ all(is.na(get_ci(.)))
      ) %>% all()
      )

    tabs_ci <- rep(list(tibble::tibble()), length(tabs))
    tabs_ci[!no_ci] <-
      purrr::map(tabs[!no_ci],
                 ~ dplyr::mutate(., dplyr::across(where(is_fmt), get_ci))
      )

    if (remove_tab_vars == FALSE) {
      tabs_ci <-
        purrr::map2(tabs_ci, tab_vars, ~ dplyr::select(dplyr::ungroup(.x),
                                                       -tidyselect::all_of(.y)))
    }

    ci_types <- rep(list(character(0)), length(tabs))
    ci_types <- purrr::map(tabs[!no_ci], ~ purrr::map_chr(.x, get_ci_type) %>%
                             stringr::str_remove("_mean$") %>%
                             purrr::set_names(names(.x))
                           )

      # ci_spread <-
    #   purrr::map(ci_types, ~ which(. %in% c("spread_col", "spread_row",
    #                                         "spread_row_all")))

    #ci_types and spread not the right length without tab vars ----




    get_vars        <- purrr::map(tabs, tab_get_vars)
    row_var         <- purrr::map(get_vars, ~ rlang::sym(.$row_var))
    col_vars_alltot <- purrr::map(get_vars, ~ rlang::syms(.$col_vars))
    col_vars        <- purrr::map(col_vars_alltot,
                                  ~ .[!. %in% c("all_col_vars", "chi2_cols")] )
    col_vars_levels_alltot <- purrr::map(get_vars,
                                         ~ purrr::map(.$col_vars_levels,
                                                      rlang::syms))
    col_vars_levels <- purrr::map(col_vars_levels_alltot,
                                  ~ .[!names(.) %in% c("all_col_vars",
                                                       "chi2_cols")] )
    tab_vars <- purrr::map(get_vars, ~ .$tab_vars)

    #groups  <- purrr::map(tabs, dplyr::group_vars)

    totrowsTF <- purrr::map(tabs, is_totrow)
    totcols     <- purrr::map(tabs, is_totcol)
    all_totcols <- purrr::map(col_vars_alltot, ~ . == "all_col_vars")

    digits <-
      purrr::map(tabs, ~ purrr::map_if(., purrr::map_lgl(., is_fmt), get_digits,
                                       .else = ~ NA_integer_))



    sheet <- 1L:length(tabs)
    start <- rep(0L, length(tabs))

    #Not working for ci = "abs"
    tabs_num <-
      purrr::map(tabs, ~ dplyr::mutate(., dplyr::across(where(is_fmt), get_num)) %>%
                   tibble::as_tibble())

    totrows <- purrr::map(totrowsTF, which)
    tot_rows <- purrr::map2(totrows, start, ~ .x + .y + 1L)
    group_ind <- purrr::map(tabs, dplyr::group_indices)
    end_group   <- purrr::map(group_ind, ~ which(.[-1] != .[-length(.)] ) ) %>%
      purrr::map2(start, ~ .x + .y + 1)
    rows_nb  <- purrr::map2(tabs, start, ~ as.integer(1:nrow(.x) + .y + 1L))
    # start_group <- purrr::map(group_ind, ~ .[-length(.)] != .[-1]   )

    all_cols <- purrr::map(tabs, ~ 1:ncol(.))
    all_cols_chi2_ci <- purrr::pmap(list(tabs, tabs_chi2, tabs_ci),
                                    ~ 1:(ncol(..1) + ncol(..2) + ncol(..3)) )
    txt_cols <- purrr::map(tabs, ~ which(purrr::map_lgl(., ~ !is_fmt(.))))
    row_var_cols <- purrr::map(txt_cols, ~ .[length(.)])
    fmt_cols <- purrr::map(tabs, ~ which(purrr::map_lgl(., ~ is_fmt(.))))
    totcols <- purrr::map(totcols, which)

    col_vars_names <- purrr::map(tabs, ~ get_col_var(.))
    end_col_var <-
      purrr::map(col_vars_names,
                 ~ which(tidyr::replace_na( .[-length(.)]  != .[-1], FALSE)))
    types <- purrr::map(tabs, ~ purrr::map_if(
      ., is_fmt, .f = ~ get_type(.), .else = ~ NA_character_
    ))
    pct_cols <- purrr::map(types, ~ tidyr::replace_na(
      purrr::map_lgl(., ~ all(. == "pct")), FALSE
    ))
    pct_type <- purrr::map(tabs, get_pct_type) #%>%
    # purrr::map2(pct_cols, ~ purrr::map_if(.x, !.y, ~ NA_character_) %>%
    #               purrr::flatten_chr() %>%
    #               purrr::set_names(names(.x)))

    # cols_by_type <-
    #   purrr::map(types, ~ purrr::map_chr(., ~ dplyr::if_else(
    #     length(unique(.)) > 1, "mixed", unique(.)
    #   )  ) )

    cols_type_pct <-  purrr::map(tabs, cols_by_type_pct)
    # purrr::map2(pct_type, cols_by_type,
    #             ~ purrr::map2_chr(.x, .y, function(.pct, .type)
    #               dplyr::if_else(.type == "pct", .pct, .type)  )
    # )

    if (any(!no_ci)) {
      cols_type_pct_ci <-
        purrr::pmap(
          list(cols_type_pct, no_ci, ci_types), function(type, no, ci)
            dplyr::case_when(
              type == "row"  & !no & ci %in% c("diff_row"  , "diff_row_all"  )
              ~ "row_ci_diff"        ,

              type == "row"  & !no & ci %in% c("spread_row", "spread_row_all")
              ~ "row_ci_spread" ,

              type == "col"  & !no & ci == "diff_col"
              ~ "col_ci_diff"        ,

              type == "col"  & !no & ci == "spread_col"
              ~ "col_ci_spread" ,

              type == "mean" & !no & ci %in% c("diff_row"  , "diff_row_all"  )
              ~ "mean_ci_diff"       ,

              type == "mean" & !no & ci %in% c("spread_row", "spread_row_all")
              ~ "mean_ci_spread",

              TRUE            ~ type
            )) %>%
        purrr::map(~ purrr::set_names(., 1:length(.)) %>%
                     purrr::discard(is.na(.) | . %in% c("mixed", "pct_ci", "mean_ci",
                                                        "var", "ci", "n"))
        )
    }


    mixed_pct_cols <- purrr::map(cols_type_pct, ~ purrr::keep(., . == "mixed"))

    spreads <-
      purrr::map2(tabs, cols_type_pct, ~ dplyr::mutate(.x, dplyr::across(
        tidyselect::all_of(names(.y)[tidyr::replace_na(
          .y %in% c("row", "col", "mean", "ctr"), FALSE)]),
        ~ get_num(.))) %>%
          tibble::as_tibble()
      )

    spreads <-
      purrr::map2(spreads, totrowsTF,
                  ~ tibble::add_column(.x, totrows = .y) %>%
                    dplyr::mutate(totrows = cumsum(totrows) - totrows + 1L)
      ) %>%
      purrr::map_if(comp == "tab", ~ dplyr::group_by(., totrows))

    spreads <-
      purrr::map2(spreads, cols_type_pct, function(.tab, .type)
        dplyr::mutate(.tab, dplyr::across(
          tidyselect::all_of(names(.type)[tidyr::replace_na(.type == "row", FALSE)]),
          ~ . - dplyr::last(.)
        )) %>%
          dplyr::mutate(dplyr::across(
            tidyselect::all_of(names(.type)[tidyr::replace_na(.type == "mean", FALSE)]),
            ~ . / dplyr::last(.)
          )) %>%
          dplyr::ungroup() %>% dplyr::select(-totrows)
      )

    tot_cols_sl <- purrr::map(tabs, detect_totcols)

    spreads <-
      purrr::pmap(list(spreads, cols_type_pct, tot_cols_sl), function(.tab, .type, .tot)
        map2_if(.tab, .tot, tidyr::replace_na(.type == "col", FALSE),
                function(.var, .tot) .var - rlang::eval_tidy(.tot, data = .tab)
        ) %>% dplyr::bind_cols()
      )

    spreads[!no_chi2] <-
      purrr::pmap(
        list(spreads[!no_chi2], cols_type_pct[!no_chi2], nbcells[!no_chi2]),
        function(.tab, .type, .cells)
          dplyr::mutate(.tab, dplyr::across(
            tidyselect::all_of(names(.type)[
              tidyr::replace_na(.type %in% c("wn", "all", "all_tabs", "ctr"), FALSE)
            ] ) &
              where(function(.var) is_fmt(.var) & get_col_var(.var) %in% names(.cells)
              ),
            function(.var) get_ctr(.var) * rlang::eval_tidy(
              rlang::sym(get_col_var(.var)), data = .cells)
          ))
      )
    spreads <- spreads %>%
      purrr::map(~ dplyr::mutate(., dplyr::across(where(is_fmt), ~ NA_real_)))


    cols_type_pct <-
      purrr::map(cols_type_pct,
                 ~ purrr::set_names(., 1:length(.)) %>%
                   purrr::discard(is.na(.) | . %in% c("mixed", "pct_ci", "mean_ci",
                                                      "var", "ci", "n"))
      )



    insuff_counts_row_var <-
      purrr::map2(insuff_counts_row_var, start,
                  ~ purrr::map(.x, function(.var) which(.var) + .y + 1))
    insuff_counts_col_var <- insuff_counts_col_var %>%
      purrr::map(~ .[purrr::map_lgl(., is.logical)]) %>%
      purrr::map_depth(2, which)



    sheet_titles <-
      purrr::map2_chr(row_var, col_vars,
                      ~ ifelse(test = length(as.character(.y)) > 1,
                               yes = stringr::str_c(as.character(.x),
                                                    " by multi"),
                               no = stringr::str_c(as.character(.x), " by ",
                                                   as.character(.y)))
      ) %>%
      stringr::str_sub(., 1, 27)

    sheet_titles <- dplyr::if_else(duplicated(sheet_titles),
                                   stringr::str_c(sheet_titles, ".2"),
                                   sheet_titles)
    nb <- 2
    while (length(unique(sheet_titles)) != length(sheet_titles)) {
      nb <- nb + 1
      sheet_titles <-
        dplyr::if_else(duplicated(sheet_titles),
                       stringr::str_c(stringr::str_remove(sheet_titles, "..$"),
                                      ".", nb),
                       sheet_titles)
    }

    #Create workbook and global formatting -------------------------------------
    wb <- openxlsx::createWorkbook()
    sheet_titles %>% purrr::walk(~ openxlsx::addWorksheet(wb, .))
    purrr::pwalk(list(sheet, start, tabs_num),
                 ~ openxlsx::writeData(wb, sheet = ..1, ..3,
                                       startRow = ..2 + 1, startCol = 1,
                                       borders = "surrounding"))
    # #On a sheet, if colnames are the same, just keep the first :
    # purrr::pwalk(list(sheet[hd_remove], start[hd_remove],  tabs[hd_remove]),
    #              function(.sheet, .start, .tabs)
    #                openxlsx::deleteData(wb, sheet = .sheet, gridExpand = TRUE,
    #                                     rows = .start + 1,
    #                                     cols = 2:ncol(.tabs)))

    openxlsx::modifyBaseFont(wb, fontSize = 10, fontName = "DejaVu Sans Condensed") #"Verdana", "DejaVu Sans Condensed"
    purrr::walk(unique(sheet),
                ~ openxlsx::showGridLines(wb, sheet = .x, showGridLines = FALSE))
    purrr::walk(unique(sheet),
                ~ openxlsx::freezePane(wb, sheet = .x, firstRow = TRUE,
                                       firstCol = TRUE))
    #Fmt cells :
    #openxlsx::createStyle(halign = "right", valign = "top")

    fmt_cols_ci    <- fmt_cols
    all_cols_ci    <- all_cols
    end_col_var_ci <- end_col_var
    totcols_ci     <- totcols

    #Chi2 and variance informations --------------------------------------------
    chi2_col1 <- purrr::map(tabs, ~ ncol(.) + 1)
    chi2_cols <- purrr::map2(chi2_col1, tabs_chi2, ~ (.x + 1):(.x - 1 + ncol(.y)) )
    if (any(!no_chi2)) {
      purrr::pwalk(list(sheet[!no_chi2], start[!no_chi2],
                        chi2_col1[!no_chi2], tabs_chi2[!no_chi2]),
                   ~ openxlsx::writeData(wb, sheet = ..1, ..4,
                                         startRow = ..2 + 1, startCol = ..3))

      st_chi1 <- openxlsx::createStyle(halign = "right")
      tibble::tibble(sheet = sheet[!no_chi2], rows = rows_nb[!no_chi2],
                     cols = chi2_col1[!no_chi2]) %>%
        purrr::pwalk(openxlsx::addStyle, wb = wb, style = st_chi1,
                     gridExpand = T, stack = T)

      count_chi2 <-
        purrr::map2(tabs_chi2[!no_chi2], start[!no_chi2],
                    ~ which(
                      dplyr::pull(.x, `chi2 stats`) == "count :") + .y + 1)
      df_cells_chi2 <-
        purrr::map2(tabs_chi2[!no_chi2], start[!no_chi2],
                    ~ which(dplyr::pull(.x, `chi2 stats`) %in%
                              c("cells :", "df :")) + .y + 1)
      pvalue_chi2 <-
        purrr::map2(tabs_chi2[!no_chi2], start[!no_chi2],
                    ~ which(dplyr::pull(.x, `chi2 stats`) %in%
                              c("pvalue :")) + .y + 1)
      variance_chi2 <-
        purrr::map2(tabs_chi2[!no_chi2], start[!no_chi2],
                    ~ which(dplyr::pull(.x, `chi2 stats`) %in%
                              c("variance :")) + .y + 1)

      c_n   <- purrr::map_lgl(count_chi2, ~ length(.) != 0)
      if (any(c_n)) {
        st_n <- openxlsx::createStyle(numFmt = "#,##0", border = "bottom",
                                      borderStyle = "dashed")
        tibble::tibble(sheet = sheet[!no_chi2], rows = count_chi2,
                       cols = chi2_cols[!no_chi2]) %>%
          dplyr::filter(c_n) %>%
          purrr::pwalk(openxlsx::addStyle, wb = wb, style = st_n,
                       gridExpand = T, stack = T)
      }
      c_df  <- purrr::map_lgl(df_cells_chi2, ~ length(.) != 0)
      if (any(c_df)) {
        st_df <- openxlsx::createStyle(numFmt = "#,##0")
        tibble::tibble(sheet = sheet[!no_chi2], rows = df_cells_chi2,
                       cols = chi2_cols[!no_chi2]) %>%
          dplyr::filter(c_df) %>%
          purrr::pwalk(openxlsx::addStyle, wb = wb, style = st_df,
                       gridExpand = T, stack = T)
      }
      c_p   <- purrr::map_lgl(pvalue_chi2, ~ length(.) != 0)
      if (any(c_p)) {
        st_p     <- openxlsx::createStyle(numFmt     = "0.00%",
                                          fontColour = "forestgreen")

        tibble::tibble(sheet = sheet[!no_chi2], rows = pvalue_chi2,
                       cols = chi2_cols[!no_chi2]) %>%
          dplyr::filter(c_p) %>%
          purrr::pwalk(openxlsx::addStyle, wb = wb, style = st_p,
                       gridExpand = T, stack = T)

        st_p_inf <- openxlsx::createStyle(fontColour = "red")
        pvalue_chi2_inf <-
          tibble::tibble(tab = tabs_chi2, start) %>%
          dplyr::filter(!no_chi2) %>%
          purrr::pmap(function(tab, start, cols)
            purrr::map(2:ncol(tab), function(col)
              which(
                dplyr::pull(tab, col) >= 0.05 &
                  dplyr::pull(tab, `chi2 stats`) %in% c("pvalue :")
              ) + start + 1L
            ) %>% purrr::flatten_int() %>% unique() %>% sort()
          )

        tibble::tibble(sheet = sheet[!no_chi2], rows = pvalue_chi2_inf,
                       chi2_col1 = chi2_col1[!no_chi2],
                       cols = chi2_cols[!no_chi2]) %>%
          tidyr::unnest(tidyselect::all_of(c("chi2_col1", "rows"))) %>%
          dplyr::mutate(#row1   = purrr::map_int(rows, ~ .[1]),
            #cel1   = xl_index(purrr::map_int(cols, ~ .[1]),
            #                  row1, offset = 0L),
            rule = ">= 0.05"
          ) %>%
          purrr::pwalk(openxlsx::conditionalFormatting, wb = wb,
                       style = st_p_inf)
      }
      c_var <- purrr::map_lgl(variance_chi2, ~ length(.) != 0)
      if (any(c_var)) {
        st_var <- openxlsx::createStyle(numFmt = "#,##0.0000",
                                        textDecoration = "bold")
        tibble::tibble(sheet = sheet[!no_chi2], rows = variance_chi2[!no_chi2],
                       cols = chi2_cols[!no_chi2]) %>%
          dplyr::filter(c_var) %>%
          purrr::pwalk(openxlsx::addStyle, wb = wb, style = st_var,
                       gridExpand = T, stack = T)
      }
    }

    # Confidence intervals tables ----------------------------------------------
    if (any(!no_ci)) {
      ci_col1 <- purrr::map2(tabs, tabs_chi2, ~ ncol(.x) + ncol(.y) + 1L)

      ci_cols <- purrr::map2(ci_col1, tabs_ci, ~ (.x + 1L):(.x - 1L + ncol(.y)) ) %>%
        purrr::map2(tabs_ci, ~ purrr::set_names(.x, names(.y)[-1]))

      tibble::tibble(sheet = sheet, x = tabs_ci,
                     startRow = purrr::map(start, ~. + 1L), startCol = ci_col1) %>%
        dplyr::filter(!no_ci) %>%
        purrr::pwalk(openxlsx::writeData, wb = wb)

      tibble::tibble(sheet = sheet, cols = ci_col1,
                     rows = purrr::map2(rows_nb, start, ~ c(.y + 1, .x))) %>%
        dplyr::filter(!no_ci) %>%
        purrr::pwalk(openxlsx::deleteData, wb = wb, gridExpand = TRUE)

      offset <- purrr::map2(fmt_cols, ci_cols, ~ .y[1] - .x[1])
      fmt_cols_ci[!no_ci]    <- fmt_cols[!no_ci] %>%
        purrr::map2(offset[!no_ci] , ~ c(.x, .x + .y))

      all_cols_ci[!no_ci]    <- all_cols[!no_ci] %>%
        purrr::map2(ci_cols[!no_ci] , ~ c(.x, .y))

      end_col_var_ci[!no_ci] <- end_col_var[!no_ci] %>%
        purrr::map2(offset[!no_ci] , ~ c(.x, .x + .y))

      totcols_ci[!no_ci]     <- totcols[!no_ci] %>%
        purrr::map2(offset[!no_ci] , ~ c(.x, .x + .y))

       ci_types

      st_ci_nums <- openxlsx::createStyle(numFmt = "0.0%")

      tibble::tibble(sheet = sheet, rows = rows_nb, cols = ci_cols) %>%
        dplyr::filter(!no_ci) %>%
        purrr::pwalk(openxlsx::addStyle, wb = wb, gridExpand = TRUE, stack = T,
                     style = st_ci_nums)

      ci_mean_cols <-
        purrr::map(cols_type_pct, ~ which(. %in% c("mean", "mean_ci"))) %>%
        purrr::map2(offset, ~ .x + .y)
      st_ci_nums_mean <- openxlsx::createStyle(numFmt = "#,##0.0")

      tibble::tibble(sheet = sheet, rows = rows_nb, cols = ci_mean_cols) %>%
        dplyr::filter(!no_ci) %>%
        purrr::pwalk(openxlsx::addStyle, wb = wb, gridExpand = TRUE, stack = T,
                     style = st_ci_nums_mean)

      st_ci_borders <- openxlsx::createStyle(border = "right")

      tibble::tibble(sheet = sheet, rows = rows_nb,
                     cols = purrr::map2(ci_col1, ci_cols,
                                        ~ c(.x, .y[length(.y)]))
      ) %>%
        dplyr::filter(!no_ci) %>%
        purrr::pwalk(openxlsx::addStyle, wb = wb, gridExpand = TRUE, stack = T,
                     style = st_ci_borders)

      # ci_types
      # ci_spread

    }

    # Sep between col_vars and groups (copy for ci tabs) -----------------------
    st_end_col_var <- openxlsx::createStyle(border = "right")

    tibble::tibble(sheet, rows = purrr::map(rows_nb, ~ c(1L, .)),
                   cols = purrr::pmap(list(txt_cols, end_col_var_ci,
                                           purrr::map(totcols_ci, ~ c(. - 1, .))),
                                      c)) %>% #row_var_cols
      dplyr::filter(purrr::map_lgl(cols, ~ length(.) != 0) ) %>%
      purrr::pwalk(openxlsx::addStyle, wb = wb, gridExpand = TRUE, stack = T,
                   style = st_end_col_var)

    st_end_group <- openxlsx::createStyle(border = "bottom")

    tibble::tibble(sheet, rows = end_group, cols = all_cols_ci) %>%
      dplyr::filter(purrr::map_lgl(rows, ~ length(.) != 0) ) %>%
      purrr::pwalk(openxlsx::addStyle, wb = wb, gridExpand = TRUE, stack = T,
                   style = st_end_group)



    # Headers and totals -----------------------------------------------------
    headers <- if (colnames_rotation == 0) {
      openxlsx::createStyle(halign = "center", valign = "bottom", wrapText = TRUE,
                            textDecoration = "Bold", border = "TopBottom",
                            fontSize = 9)
    } else {
      openxlsx::createStyle(
        halign = "left", valign = "bottom", wrapText = TRUE,
        textDecoration = "Bold", textRotation = colnames_rotation,
        border = c("bottom", "top"), fontSize = 9 # "left", "right",
      )
    }

    tibble::tibble(sheet, rows = start + 1, cols = all_cols_chi2_ci) %>%
      purrr::pwalk(openxlsx::addStyle, wb = wb, gridExpand = TRUE, stack = T,
                   style = headers)

    st_totrows <-
      openxlsx::createStyle(halign = "right", valign = "top",
                            textDecoration = "Bold", border = "TopBottom",
                            borderStyle = c("thin", "double"))

    tibble::tibble(sheet, rows = tot_rows, cols = fmt_cols_ci) %>%
      dplyr::filter(purrr::map_lgl(rows, ~ length(.) != 0) ) %>%
      purrr::pwalk(openxlsx::addStyle, wb = wb, gridExpand = TRUE, stack = T,
                   style = st_totrows)

    st_totrows_text <-
      openxlsx::createStyle(halign = "left", valign = "top", wrapText = TRUE,
                            textDecoration = "Bold", border = "TopBottom",
                            borderStyle = c("thin", "double"))

    tibble::tibble(sheet, rows = tot_rows, cols = txt_cols) %>%
      dplyr::filter(purrr::map_lgl(rows, ~ length(.) != 0) ) %>%
      purrr::pwalk(openxlsx::addStyle, wb = wb, gridExpand = TRUE, stack = T,
                   style = st_totrows_text)

    st_totcols <-
      openxlsx::createStyle(halign = "left", valign = "top",
                            textDecoration = "Bold", border = "LeftRight")

    tibble::tibble(sheet, rows = rows_nb, cols = totcols_ci) %>%
      dplyr::filter(purrr::map_lgl(totcols, ~ length(.) != 0) ) %>%
      purrr::pwalk(openxlsx::addStyle, wb = wb, gridExpand = TRUE, stack = T,
                   style = st_totcols)

    st_bottomline <-
      openxlsx::createStyle(border = "bottom", borderStyle = "thin")

    tibble::tibble(sheet, rows = purrr::map2(tabs, start, ~ nrow(.) + .y + 1L),
                   cols = all_cols_ci) %>%
      purrr::pwalk(openxlsx::addStyle, wb = wb, gridExpand = TRUE, stack = T,
                   style = st_bottomline)


    # Insufficient counts ----------------------------------------------------
    st_insufficient_counts <- openxlsx::createStyle(fontColour = "#909090")

    insuff_col_cols <-
      purrr::map2(insuff_counts_col_var, row_var_cols, ~ 1:length(.x) + .y)

    insuff_col_map <-
      tibble::tibble(sheet, insuff_counts_col_var, insuff_col_cols, start) %>%
      purrr::pmap(~ purrr::map2_df(
        ..2, ..3,
        function(.x, .y) tibble::tibble(sheet = ..1,
                                        cols  = .y, rows  = list(.x + ..4 + 1)
        ) ) ) %>%
      dplyr::bind_rows() %>%
      dplyr::filter(purrr::map_lgl(rows, ~ length(.) != 0))

    if (nrow(insuff_col_map) != 0) purrr::pwalk(insuff_col_map,
                                                openxlsx::addStyle,
                                                wb = wb,
                                                gridExpand = TRUE, stack = T,
                                                style = st_insufficient_counts)

    insuff_row_rows <-
      purrr::map2(insuff_counts_row_var, col_vars_names, function(ins, cols)
        purrr::map_if(1:length(cols), cols %in% names(ins),
                      .f    = ~ ins[cols[.]] %>% purrr::map(as.integer),
                      .else = ~ list(integer())
        ) %>% purrr::set_names(names(cols)) %>% purrr::map(purrr::flatten_int)
      )

    insuff_row_map <-
      purrr::map2(sheet, insuff_row_rows, function(sh, ins) purrr::map2_df(
        ins, 1:length(ins),
        function(.x, .y) tibble::tibble(sheet = sh,
                                        cols  = .y, rows  = list(.x)
        ) ) ) %>%
      dplyr::bind_rows() %>%
      dplyr::filter(purrr::map_lgl(rows, ~ length(.) != 0))

    if (nrow(insuff_row_map) != 0) dplyr::group_by(insuff_row_map, sheet, rows) %>%
      dplyr::summarise(sheet = dplyr::last(sheet),
                       cols  = list(cols),
                       rows  = dplyr::last(rows), .groups = "drop") %>%
      purrr::pwalk(openxlsx::addStyle, wb = wb, gridExpand = TRUE, stack = T,
                   style = st_insufficient_counts)

    #Digits ----------------------------------------------------------------
    numfmt <- function(n, type) {
      ci    <- type %in% c("pct_ci", "mean_ci")
      pct   <- type %in% c("pct", "ctr", "ci")
      n_inf <- n < 0
      n_0   <- n == 0

      rep0_n <- purrr::map_chr(abs(n), ~ paste0(rep("0", .), collapse = ""))

      dplyr::case_when(
        ci          ~ "TEXT",

        pct & n_inf ~ NA_character_,
        pct & n_0   ~ "0%",
        pct         ~ paste0("0.", rep0_n, "%"),

        n_0         ~ "#,##0",
        n_inf       ~ paste0(
          "#,",
          purrr::map_chr(abs(n), ~ paste0(rep("#", 2 - . %%3 ),
                                          collapse = "")),
          purrr::map_chr(abs(n), ~ paste0(rep("0", 1 + . %%3 ),
                                          collapse = "")),
          purrr::map_chr(abs(n), ~ paste0(rep(",",    . %/%3 ),
                                          collapse = ""))            ),
        TRUE        ~ paste0("#,##0.", rep0_n)
      )
    }

    digits_map <-
      list(sheet, digits, types, start, 1:length(sheet)) %>%
      purrr::pmap(function(sheet, .n, .type, .start, .tab_nb)
        purrr::pmap(list(.n, .type, 1:length(.n)), function(n, type, .lgth)
          tibble::tibble(sheet = sheet, cols = .lgth,
                         rows = 1:length(n) + .start + 1,
                         type = type, n = n, tab_nb = .tab_nb)
        ) %>% dplyr::bind_rows()
      ) %>% dplyr::bind_rows() %>%
      dplyr::filter(!is.na(type) & !is.na(n)) %>%
      dplyr::mutate(num_format = forcats::as_factor(numfmt(n, type))) %>%
      dplyr::group_by(num_format) %>%
      dplyr::mutate(num_name = paste0("st_digits", as.integer(num_format)))

    #assign one variable for each number style
    number_styles <- digits_map %>%
      dplyr::summarise(num_name = dplyr::last(num_name), .groups = "drop") %>%
      dplyr::select(num_name, num_format) %>% tibble::deframe() %>%
      purrr::map(~ openxlsx::createStyle(fontName = "DejaVu Sans",
                                         numFmt = as.character(.)))

    for (i in 1:length(number_styles)) {
      assign(names(number_styles)[[i]], number_styles[[i]])
    }

    digits_map %>% dplyr::group_by(sheet, cols, num_name) %>%
      dplyr::summarise(rows = list(rows), .groups = "drop") %>%
      dplyr::relocate(num_name, .after = -1) %>%
      purrr::pwalk(function(sheet, cols, rows, num_name) openxlsx::addStyle(
        wb, gridExpand = TRUE, stack = T,
        sheet = sheet, cols = cols, rows = rows,
        style = rlang::eval_tidy(rlang::sym(num_name))
      ))


    #conditional formatting ###################################################
    ###########################################################################-

    # conditional formatting styles -------------------------------------------

    # Above mean : shades of green  (for example, 5% to 35%)
    st_plus1  <- openxlsx::createStyle(bgFill = "#e6f2e6")            #f2f9f2  #b3d9b5
    st_plus2  <- openxlsx::createStyle(bgFill = "#b3d9b5")            #b3d9b5  #8ec690
    st_plus3  <- openxlsx::createStyle(bgFill = "#82c083")            #68b36b  #68b36b
    st_plus4  <- openxlsx::createStyle(bgFill = "#48ad4c")            #3c903f  #43a047
    st_plus5  <- openxlsx::createStyle(bgFill = "#3a8a3d")            #2c5e2c  #3c903f   #fontColour = "#ffffff"

    # Below mean : shades of orange (for example, 5% to 35%)
    st_minus1 <- openxlsx::createStyle(bgFill = "#ffeddf")  #fff6ef   #ffeddf     #fbcaa2   #f7c3c2
    st_minus2 <- openxlsx::createStyle(bgFill = "#fbcaa2")            #fbcaa2     #f9b57d   #f4afae
    st_minus3 <- openxlsx::createStyle(bgFill = "#fcb073")            #f7a058     #f7a058   #ef8885
    st_minus4 <- openxlsx::createStyle(bgFill = "#de873f")  #b66f36   #de873f     #f79646   #ea605d
    st_minus5 <- openxlsx::createStyle(bgFill = "#c36a21")  #b66f36               #de873f   #e53935   #, fontColour = "#ffffff"

    style_pos <- c("st_plus1", "st_plus2", "st_plus3", "st_plus4", "st_plus5")
    style_neg <- c("st_minus1" , "st_minus2", "st_minus3", "st_minus4",
                   "st_minus5")

    lbrk <- max(length(pct_breaks), length(mean_breaks), length(contrib_breaks))
    pct_breaks     <- c(pct_breaks    ,
                        rep(NA_real_, lbrk - length(pct_breaks)))
    mean_breaks    <- c(mean_breaks   ,
                        rep(NA_real_, lbrk - length(mean_breaks)))
    contrib_breaks <- c(contrib_breaks,
                        rep(NA_real_, lbrk - length(contrib_breaks)))
    pct_breaks_after_ci  <- pct_breaks - pct_breaks[1]
    mean_breaks_after_ci <- mean_breaks/mean_breaks[1]

    if (lbrk >= 2) {
    pct_breakssup            <- c(pct_breaks[2:lbrk]          , Inf)
    mean_breakssup           <- c(mean_breaks[2:lbrk]         , Inf)
    coeffssup                <- c(contrib_breaks[2:lbrk]      , Inf)
    pct_breaks_after_ci_sup  <- c(pct_breaks_after_ci[2:lbrk] , Inf)
    mean_breaks_after_ci_sup <- c(mean_breaks_after_ci[2:lbrk], Inf)
    } else {
      pct_breakssup <- mean_breakssup <- coeffssup <-
        pct_breaks_after_ci_sup <- mean_breaks_after_ci_sup <- Inf
    }

    style_select <- switch(lbrk,
                          "1" = 2,
                          "2" = c(2, 4),
                          "3" = c(1, 3, 4),
                          "4" = 1:4,
                          "5" = 1:5
    )

    style_pos <- style_pos[style_select] %>% c(rep(NA_real_, lbrk - length(.)))
    style_neg <- style_neg[style_select] %>% c(rep(NA_real_, lbrk - length(.)))

    if (lbrk > 5) {
      warning("length of color breaks > 5 : only the first 5 taken")
      lbrk <- 5
      pct_breaks     <- pct_breaks    [1:5]
      mean_breaks    <- mean_breaks   [1:5]
      contrib_breaks <- contrib_breaks[1:5]
    }

    pct_breaks               <- pct_breaks               %>% c(., -.)
    mean_breaks              <- mean_breaks              %>% c(., 1/.)
    contrib_breaks           <- contrib_breaks           %>% c(., -.)
    pct_breaks_after_ci      <- pct_breaks_after_ci      %>% c(., .)
    mean_breaks_after_ci     <- mean_breaks_after_ci     %>% c(., 1/.)

    pct_breakssup            <- pct_breakssup            %>% c(., -.)
    mean_breakssup           <- mean_breakssup           %>% c(., 1/.)
    coeffssup                <- coeffssup                %>% c(., -.)
    pct_breaks_after_ci_sup  <- pct_breaks_after_ci_sup  %>% c(., .)
    mean_breaks_after_ci_sup <- mean_breaks_after_ci_sup %>% c(., 1/.)

    style <- c(style_pos, style_neg)

    sign = c(rep(">", lbrk), rep("<", lbrk))

    conditional_fmt_styles <- tibble::tibble(
      style, sign,
      pct_breaks, mean_breaks, contrib_breaks,
      pct_breaks_after_ci, mean_breaks_after_ci,
      pct_breakssup, mean_breakssup, coeffssup,
      pct_breaks_after_ci_sup,mean_breaks_after_ci_sup
    )



    # Database of parameters to map conditional formatting to excel ------------

    #kept types for conditional formatting :
    # c("row", "col", "all", "all_tabs", "wn", "ctr", "mean")
    # warning with mixed_pct_cols ? ----

    #For each table, create one line of parameters for earch column types ;
    # and in each type, one line for each continuous set of columns
    conditional_fmt_map <-
      tibble::tibble(sheet, type_ci = cols_type_pct_ci, type = cols_type_pct,
                     cols = purrr::map(cols_type_pct, ~ as.integer(names(.))),
                     totrows, totcols, tab_nb = 1:length(sheet), comp, spreads,
                     start, color) %>%
      tidyr::unnest(c(type, type_ci, cols)) %>%
      dplyr::group_by(sheet, type_ci) %>%
      dplyr::mutate(ct = cols != dplyr::lag(cols + 1, default = TRUE),
                    ct = cumsum(as.integer(ct))) %>%
      tidyr::nest(cols = cols) %>%
      dplyr::mutate(cols = purrr::map(cols, ~ dplyr::pull(., cols))) %>%
      dplyr::select(sheet, type, ct, dplyr::everything())

    #conditional_fmt_map[conditional_fmt_map$type =="col",] %>% dplyr::pull(values)

    #Remove total rows and cols when they are not relevant for type or comp ;
    #  totcols are excluded from cols, and groups with just totcols removed
    conditional_fmt_map <- conditional_fmt_map %>%
      dplyr::mutate(
        cols     = purrr::map2(.data$cols, .data$totcols,
                               ~ purrr::discard(.x, .x %in% .y)),
        totcols = dplyr::if_else(
          condition = .data$type == "col", # + ctr ?
          true      = .data$totcols,
          false     = purrr::map(.data$totcols, max)
        ),
        totrows = dplyr::if_else( # + "all" "wn" ?
          condition = .data$comp == "tab" & .data$type %in% c("row", "mean", "ctr"),
          true      = .data$totrows,
          false     = purrr::map(.data$totrows, max)
        )
      ) %>%
      dplyr::filter(purrr::map_lgl(cols, ~ length(.) != 0)) %>%
      dplyr::select(-comp)
    #comp = "all" needs a totaltab with a total row ----

    #If there are several total column, create one line of parameters for each ;
    # in "ctr", mean contrib calculated on all vars, not on each vars ----
    conditional_fmt_map <- conditional_fmt_map %>%
      tidyr::unnest(totcols) %>%
      dplyr::group_by(ct, .add = TRUE) %>%
      dplyr::mutate(
        cols = dplyr::if_else(
          condition = dplyr::n() > 1 & ! type %in% c("mean"), #See for wn ?
          true      = purrr::pmap(
            list(cols, totcols, dplyr::lag(totcols, default = 0L)),
            ~ purrr::discard(..1, ..1 >= ..2 | ..1 <= ..3)
          ),
          false     = cols),
        col1 = purrr::map_int(.data$cols, ~ .[1])
      ) %>%
      dplyr::filter(purrr::map_lgl(cols, ~ length(.) != 0))

    #When several total rows, create a line of parameters for each, if needed
    conditional_fmt_map <- conditional_fmt_map %>%
      tidyr::unnest(totrows) %>%
      dplyr::group_by(totcols, .add = TRUE) %>%
      dplyr::mutate(
        row1 = dplyr::lag(totrows, default = 0L) + 1L,
        rows = purrr::map2(row1, totrows, ~ .x:.y),
        rowsctr = dplyr::if_else(type %in% c("wn", "all", "all_tabs"),
                                 true    = rows,
                                 false   = list(NA_integer_)),
        colsctr = dplyr::if_else(type %in% c("wn", "all", "all_tabs"),
                                 true    = cols,
                                 false   = list(NA_integer_)),
      ) %>%
      dplyr::ungroup() %>%
      tidyr::unnest(colsctr) %>%
      tidyr::unnest(rowsctr) %>%
      dplyr::mutate(rows = dplyr::if_else(type %in% c("wn", "all", "all_tabs"),
                                          true    = as.list(rowsctr),
                                          false   = rows),
                    cols = dplyr::if_else(type %in% c("wn", "all", "all_tabs"),
                                          true    = as.list(colsctr),
                                          false   = cols),
      ) %>%
      dplyr::select(-colsctr, -rowsctr)


    #Select test values corresponding to kept rows, cols, and totcols
    c_ctr <- conditional_fmt_map$type %in% c("wn", "all", "all_tabs")
    conditional_fmt_map[! c_ctr,] <- conditional_fmt_map[!c_ctr,] %>%
      dplyr::mutate(
        spreads = purrr::pmap(list(spreads, cols, rows),
                              ~ ..1[..3,..2] %>%
                                tidyr::pivot_longer(cols = dplyr::everything()) %>%
                                dplyr::pull(value)
        ) )

    conditional_fmt_map[c_ctr,] <- conditional_fmt_map[c_ctr,] %>%
      dplyr::mutate(
        spreads = purrr::pmap(list(spreads, cols, rows), ~ ..1[[..3,..2]] )
      )

    conditional_fmt_map <- conditional_fmt_map %>%
      dplyr::mutate(
        cel1 = dplyr::if_else(type %in% c("wn", "all", "all_tabs"),
                              true    = xl_index(cols   , rows   , start),
                              false   = xl_index(col1   , row1   , start)),
        totc = xl_index(totcols, row1   , start, fixedcol = TRUE),
        totr = xl_index(col1   , totrows, start, fixedrow = TRUE),
        tott = xl_index(totcols, totrows, start,
                        fixedrow = TRUE, fixedcol = TRUE),
        rows = purrr::map2(rows, start, ~ .x + .y + 1),#not use for calc with df
      ) %>%
      # dplyr::group_by(tab_nb) %>%
      # dplyr::mutate(totg = xl_index(totcols, max(totrows),
      #                               fixedrow = TRUE, fixedcol = TRUE)) %>%
      # dplyr::ungroup() %>%
      dplyr::select(tab_nb, sheet, type, ct, cols, rows, totcols, totrows,
                    col1, row1, dplyr::everything())

    #Not calculate cols if not necessary by types ----

    if (any(!no_ci)) {
      conditional_fmt_map <- conditional_fmt_map %>%
        dplyr::mutate(
          col1_offset = col1 + purrr::flatten_int(offset)[.data$tab_nb],
          cel1_ci = dplyr::if_else(
            condition = .data$type_ci %in% c("row_ci_diff", "row_ci_spread",
                                             "col_ci_diff" , "col_ci_spread" ,
                                             "mean_ci_diff", "mean_ci_spread"),
            true  = xl_index(col1_offset   , row1   , start),
            false = NA_character_
          )
        )

    }


    # Calculate one conditional formating rule for each line of parameters
    rule_factory <- function(type_ci, cel1, color,
                             totc, totr, tott, ctr, cel1_ci) { #totg
      rule <- rlang::rep_along(type_ci, list())

      trow        <- type_ci == "row"
      tcol        <- type_ci == "col"
      tmean       <- type_ci == "mean"
      tctr        <- type_ci %in% c("wn", "all", "all_tabs", "ctr")

      trow_no_ci   <- color == "diff_no_ci"    & type_ci == "row_ci_diff"
      trow_no_cis  <- color == "diff_no_ci"    & type_ci == "row_ci_spread"
      tcol_no_ci   <- color == "diff_no_ci"    & type_ci == "col_ci_diff"
      tcol_no_cis  <- color == "diff_no_ci"    & type_ci == "col_ci_spread"
      tmean_no_ci  <- color == "diff_no_ci"    & type_ci == "mean_ci_diff"
      tmean_no_cis <- color == "diff_no_ci"    & type_ci == "mean_ci_spread"

      trow_af_ci   <- color == "diff_after_ci" & type_ci == "row_ci_diff"
      trow_af_cis  <- color == "diff_after_ci" & type_ci == "row_ci_spread"
      tcol_af_ci   <- color == "diff_after_ci" & type_ci == "col_ci_diff"
      tcol_af_cis  <- color == "diff_after_ci" & type_ci == "col_ci_spread"
      tmean_af_ci  <- color == "diff_after_ci" & type_ci == "mean_ci_diff"
      tmean_af_cis <- color == "diff_after_ci" & type_ci == "mean_ci_spread"
      #tctr2  <- type == "ctr"

      #comp must be the same in ci and tab_xl : make attribute ? ----

      # ci_types
      # ci_spread

      # color = c("auto", "contrib", "diff", "diff_no_ci", "diff_after_ci", "no")


      rule_row_pct <- function(cel1, totr) {      # B2 > B$T + 0.05
        function(brk, sign)
          paste0(cel1, sign, totr, ifelse(sign == ">", "+", ""), brk)
      }
      rule_col_pct <- function(cel1, totc) {      # B2 > $T2 + 0.05
        function(brk, sign)
          paste0(cel1, sign, totc, ifelse(sign == ">", "+", ""), brk)
      }
      rule_mean <- function(cel1, totr) {     # B2 > B$2 * 1.10
        function(brk, sign)
          paste0(cel1, sign, totr, "*", brk)
      }
      rule_ctr <- function(cel1, ctr) {   # B2 - B2 + ctr > 2
        function(brk, sign)
          paste0(cel1, "-", cel1, "+", ctr, sign, brk)
      }

      rule_row_pct_diff_no_ci <- function(cel1, totr, cel1_ci) {
        function(brk, sign)
          paste0("AND(", cel1, sign, totr, ifelse(sign == ">", "+", ""), brk,
                 ", ABS(", cel1, "-", totr, ") -",cel1_ci, ">0)")
      }  # B2 > B$T + 0.05 & ABS(B2 - B$T) - ci > 0
      rule_row_pct_diff_no_ci_spread <- function(cel1, totr, cel1_ci) {
        function(brk, sign)
          paste0("AND(", cel1, sign, totr, ifelse(sign == ">", "+", ""), brk,
                 ", ", cel1_ci, ">0)")
      }  # B2 > B$T + 0.05 & ci > 0
      rule_col_pct_diff_no_ci <- function(cel1, totc, cel1_ci) {
        function(brk, sign)
          paste0("AND(", cel1, sign, totc, ifelse(sign == ">", "+", ""), brk,
                 ", ABS(", cel1, "-", totc, ") -", cel1_ci, ">0)")
      } # B2 > $T2 + 0.05 & ABS(B2 - $T2) - ci > 0
      rule_col_pct_diff_no_ci_spread <- function(cel1, totc, cel1_ci) {
        function(brk, sign)
          paste0("AND(", cel1, sign, totc, ifelse(sign == ">", "+", ""), brk,
                 ", ", cel1_ci, ">0)")
      } # B2 > $T2 + 0.05 & ci > 0
      rule_mean_diff_no_ci <- function(cel1, totr, cel1_ci) {
        function(brk, sign)
          paste0("AND(", cel1, sign, totr, "*", brk,
                 ", ABS(", cel1, "-", totr, ")-", cel1_ci, ">0)")
      } # B2 > B$2 * 1.10 & B2 - B$2 - ci > 0
      rule_mean_diff_no_ci_spread <- function(cel1, totr, cel1_ci) {
        function(brk, sign)
          paste0("AND(", cel1, sign, totr, "*", brk,
                 ", ", cel1_ci, ">0)")
      } # B2 > B$2 * 1.10 & ci > 0


      rule_row_pct_diff_after_ci <- function(cel1, totr, cel1_ci) {
        function(brk, sign)
          paste0(cel1, sign, totr, ifelse(sign == ">", "+", ""), brk,
                 "-", cel1_ci)
      } # B2 > B$T + 0.05 - ci
      rule_row_pct_diff_after_ci_spread <- function(cel1, totr, cel1_ci) {
        function(brk, sign)
          paste0("AND(", cel1, "-", totr, sign, "0",
                 ", ", cel1_ci, ">", brk, ")")
      } # B2 - B$T > 0 & ci > 0.05
      rule_col_pct_diff_after_ci <- function(cel1, totc, cel1_ci) {
        function(brk, sign)
          paste0(cel1, sign, totc, ifelse(sign == ">", "+", ""), brk,
                 "-", cel1_ci)
      } # B2 > B$T + 0.05 - ci
      rule_col_pct_diff_after_ci_spread <- function(cel1, totc, cel1_ci) {
        function(brk, sign)
          paste0("AND(", cel1, "-", totc, sign, "0",
                 ", ", cel1_ci, ">", brk, ")")
      } # B2 - $B2 > 0 & ci > 0.05
      rule_mean_diff_after_ci <- function(cel1, totr, cel1_ci) {
        function(brk, sign)
          paste0(cel1, sign, "(", totr, "+", cel1_ci, ")",
                 "*", brk)
      } # B2 > (B$2 + ci) * 1.10
      # ci : + or - depending on sign ? ----
      rule_mean_diff_after_ci_spread <- function(cel1, totr, cel1_ci) {
        function(brk, sign)
          paste0("AND(", cel1, sign, totr,
                 ", ", cel1_ci, ">", brk, ")")
      } # B2 > $B2 & ci > 0.05


      # rule_ctr2 <- function(cel1, ctr) {   # B2 > 2 * nb_cells
      #   function(pct_breaks, mean_breaks, sign,  contrib_breaks)
      #     paste0(cel1, sign, contrib_breaks, "/")
      # }
      #contrib :
      # ...nbcell, "-", ...nbcell, "+",
      # ...cell_contrib, sign2, coeff, "*", .mean_contrib


      # rule[trow ] <- purrr::map2(cel1[trow ], totr[trow ], rule_row_pct )
      # rule[tcol ] <- purrr::map2(cel1[tcol ], totc[tcol ], rule_col_pct )
      # rule[tmean] <- purrr::map2(cel1[tmean], totr[tmean], rule_mean)
      # rule[tctr ] <- purrr::map2(cel1[tctr ], ctr [tctr ], rule_ctr )
      # #rule[tctr2] <- purrr::map2(cel1[tctr2], rule_ctr2)

      rule[trow        ] = purrr::pmap(list(cel1[trow        ], totr[trow        ]                       ), rule_row_pct)
      rule[trow_no_ci  ] = purrr::pmap(list(cel1[trow_no_ci  ], totr[trow_no_ci  ], cel1_ci[trow_no_ci  ]), rule_row_pct_diff_no_ci)
      rule[trow_no_cis ] = purrr::pmap(list(cel1[trow_no_cis ], totr[trow_no_cis ], cel1_ci[trow_no_cis ]), rule_row_pct_diff_no_ci_spread)
      rule[trow_af_ci  ] = purrr::pmap(list(cel1[trow_af_ci  ], totr[trow_af_ci  ], cel1_ci[trow_af_ci  ]), rule_row_pct_diff_after_ci)
      rule[trow_af_cis ] = purrr::pmap(list(cel1[trow_af_cis ], totr[trow_af_cis ], cel1_ci[trow_af_cis ]), rule_row_pct_diff_after_ci_spread)
      rule[tcol        ] = purrr::pmap(list(cel1[tcol        ], totc[tcol        ]                       ), rule_col_pct)
      rule[tcol_no_ci  ] = purrr::pmap(list(cel1[tcol_no_ci  ], totc[tcol_no_ci  ], cel1_ci[tcol_no_ci  ]), rule_col_pct_diff_no_ci)
      rule[tcol_no_cis ] = purrr::pmap(list(cel1[tcol_no_cis ], totc[tcol_no_cis ], cel1_ci[tcol_no_cis ]), rule_col_pct_diff_no_ci_spread)
      rule[tcol_af_ci  ] = purrr::pmap(list(cel1[tcol_af_ci  ], totc[tcol_af_ci  ], cel1_ci[tcol_af_ci  ]), rule_col_pct_diff_after_ci)
      rule[tcol_af_cis ] = purrr::pmap(list(cel1[tcol_af_cis ], totc[tcol_af_cis ], cel1_ci[tcol_af_cis ]), rule_col_pct_diff_after_ci_spread)
      rule[tmean       ] = purrr::pmap(list(cel1[tmean       ], totr[tmean       ]                       ), rule_mean)
      rule[tmean_no_ci ] = purrr::pmap(list(cel1[tmean_no_ci ], totr[tmean_no_ci ], cel1_ci[tmean_no_ci ]), rule_mean_diff_no_ci)
      rule[tmean_no_cis] = purrr::pmap(list(cel1[tmean_no_cis], totr[tmean_no_cis], cel1_ci[tmean_no_cis]), rule_mean_diff_no_ci_spread)
      rule[tmean_af_ci ] = purrr::pmap(list(cel1[tmean_af_ci ], totr[tmean_af_ci ], cel1_ci[tmean_af_ci ]), rule_mean_diff_after_ci)
      rule[tmean_af_cis] = purrr::pmap(list(cel1[tmean_af_cis], totr[tmean_af_cis], cel1_ci[tmean_af_cis]), rule_mean_diff_after_ci_spread)
      rule[tctr        ] = purrr::pmap(list(cel1[tctr        ], ctr [tctr        ]                       ), rule_ctr)

      rule
    }

    conditional_fmt_map <- conditional_fmt_map %>%
      dplyr::mutate(rule_gen = rule_factory(
        type_ci = type_ci, cel1 = cel1, cel1_ci = cel1_ci, color = color,
        totc = totc, totr = totr, tott = tott,
        ctr = spreads)
      ) %>%
      dplyr::filter(!is.na(rule_gen))

    tibble::tibble(
      style, sign,
      pct_breaks, mean_breaks, contrib_breaks,
      pct_breaks_after_ci, mean_breaks_after_ci,
      pct_breakssup, mean_breakssup, coeffssup,
      pct_breaks_after_ci_sup,mean_breaks_after_ci_sup
    )

    # Create conditional formatting rules for each shade of green/orange ;
    # for each one, test if realy used, remove if not
    cfmt_final <- conditional_fmt_map %>%
      tibble::add_column(styles_df = list(conditional_fmt_styles)) %>%
      tidyr::unnest(styles_df) %>%
      dplyr::mutate(
        brk = dplyr::case_when(
          color == "diff_after_ci" &
            type_ci %in% c("row_ci_diff", "col_ci_diff",
                           "row_ci_spread", "col_ci_spread")
                                                       ~ pct_breaks_after_ci,
          color == "diff_after_ci" &
            type_ci %in% c("mean_ci_spread", "mean_ci_diff")
                                                       ~ mean_breaks_after_ci,
          type %in% c("row", "col")                    ~ pct_breaks ,
          type == "mean"                               ~ mean_breaks,
          type %in% c("all", "all_tabs", "wn", "ctr")  ~ contrib_breaks,
        ),
        brksup = dplyr::case_when(
          color == "diff_after_ci" &
            type_ci %in% c("row_ci_diff", "col_ci_diff",
                           "row_ci_spread", "col_ci_spread")
                                                       ~ pct_breaks_after_ci_sup,
          color == "diff_after_ci" &
            type_ci %in% c("mean_ci_spread", "mean_ci_diff")
                                                       ~ mean_breaks_after_ci_sup,
          type %in% c("row", "col")                    ~ pct_breakssup ,
          type == "mean"                               ~ mean_breakssup,
          type %in% c("all", "all_tabs", "wn", "ctr")  ~ coeffssup
        )
      ) %>%
      dplyr::select(-pct_breaks, -mean_breaks, -contrib_breaks,
        -pct_breaks_after_ci, -mean_breaks_after_ci,
        -pct_breakssup, -mean_breakssup, -coeffssup,
        -pct_breaks_after_ci_sup, -mean_breaks_after_ci_sup    ) %>%
      dplyr::filter(! (is.na(cel1) | is.na(brk) ) # |
                    #      (type == "row"              & is.na(totr   ) ) |
                    #      (type %in% c("col", "mean") & is.na(totc   ) ) |
                    #      (type == "ctr"              & is.na(cells) )   )
      ) %>%
      dplyr::mutate(
        rule_test = purrr::pmap_lgl(
          list(spreads, brk, brksup, type_ci, sign), function(spread, brk, brksup, type_ci, sign)
            switch(type_ci,
                   "row"  = ,
                   "col"  = switch(sign,
                                   ">" = any(spread >= brk   & spread < brksup) ,
                                   "<" = any(spread <= -brk  & spread > -brksup)),
                   "mean" = switch(sign,
                                   ">" = any(spread >= brk   & spread < brksup  )     ,
                                   "<" = any(spread <= 1/brk & spread > 1/brksup)),
                   "all"  =,
                   "all_tabs" =,
                   "wn"   =,
                   "ctr"  = switch(sign,
                                   ">" = any((spread >= brk) & (spread < brksup)),
                                   "<" = any((spread <= brk) & (spread > brksup))      ),
                   "row_ci_diff"    = ,
                   "row_ci_spread"  = ,
                   "col_ci_diff"    = ,
                   "col_ci_spread"  = ,
                   "mean_ci_diff"   = ,
                   "mean_ci_spread" = TRUE,
            )
        ))

    # c("pct_breaks_after_ci",
    #   "mean_breaks_after_ci", "pct_breaks_after_ci_sup", "mean_breaks_after_ci_sup")


    #    cfmt_final$brksup[cfmt_final$type %in% c("all", "all_tabs", "wn", "ctr")]

    #
    #     cfmt_final[cfmt_final$type %in% c("all", "all_tabs", "wn", "ctr"),] %>%
    #       dplyr::filter(rule_test) %>%
    #       dplyr::mutate(broutte = purrr::map2(brk, brksup, ~ c(.x, .y))) %>%
    #       .$broutte

    cfmt_final <- cfmt_final %>%
      dplyr::filter(rule_test) %>%
      dplyr::mutate(
        rule = purrr::pmap_chr(
          list(rule_gen, brk, sign), function(fn, .brk, .sign)
            rlang::exec(fn, brk = .brk, sign = .sign)
        ) )

    #Apply all used conditional formatting to the workbook
    cfmt_final %>%
      dplyr::select(sheet, cols, rows, rule, style) %>%
      purrr::pwalk(function(sheet, cols, rows, rule, style)
        openxlsx::conditionalFormatting(
          wb, sheet = sheet, cols = cols, rows = rows,
          rule = rule, style = rlang::eval_tidy(rlang::sym(style)))
      )




    #Numbers near zero in white gray -------------------------------------------
    # enhance to put together continuous columns ----
    style_zero <- openxlsx::createStyle(fontColour = "#EAEAEA")
    near0_auto <- hide_near_zero == "auto"

    if (any(near0_auto)) {
      digits_map %>%
        dplyr::filter(tab_nb %in% (1:length(tabs))[near0_auto]) %>%
        dplyr::mutate(n = dplyr::if_else(type %in% c("pct", "ctr", "ci"), n + 2L, n ),
                      hide_near_zero = 0.49 * 10^(-n)) %>%
        dplyr::group_by(sheet, cols, hide_near_zero) %>%
        dplyr::mutate(continuous  = rows != dplyr::lag(rows + 1, default = TRUE),
                      continuous = cumsum(as.integer(continuous))) %>%
        dplyr::group_by(continuous, .add = TRUE) %>%
        dplyr::summarise(rows = list(rows), .groups = "drop") %>%
        dplyr::mutate(rule = purrr::map(hide_near_zero, ~ c(-., .)) ) %>%
        dplyr::select(sheet, cols, rows, rule) %>%
        purrr::pwalk(openxlsx::conditionalFormatting,
                     wb = wb, style = style_zero, type = "between")
    }

    if (any(!near0_auto)) {
      tibble::tibble(sheet, cols = all_cols, rows = rows_nb) %>%
        dplyr::filter(!near0_auto) %>%
        tibble::add_column(rule = purrr::map(hide_near_zero[!near0_auto],
                                             ~ c(-., .))) %>%
        purrr::pwalk(openxlsx::conditionalFormatting,
                     wb = wb, style = style_zero, type = "between")
    }


    # # Attempt to do same thing with only one complex Excel conditional fmt:
    # #  nothing works in Excel,  neither "" nor functions
    # xlc_pct    <- "CHAR(34)&CHAR(37)&CHAR(34)"
    # xlc_fmt <-
    #   "CHAR(34)&CHAR(102)&CHAR(111)&CHAR(114)&CHAR(109)&CHAR(97)&CHAR(116)&CHAR(34)"
    #     #
    # near_zero_map <- tibble::tibble(
    #   sheet,
    #   cols = fmt_cols,
    #   rows = rows_nb,
    #   cel1 = paste0(
    #     purrr::map_chr(fmt_cols, ~ paste0(LETTERS[.[1] %/% 26],
    #                                       LETTERS[.[1] %% 26]) ),
    #     purrr::map_chr(rows_nb, ~ .[1])                          ),
    #   rule = paste0(cel1, "< 0.5")
    #     #paste0("LEFT(", cel1, ";1) < 0.5")
    #   #   paste0(
    #   #   'IF(LEFT(CELL(', xlc_fmt, ';', cel1, ')) = ', xlc_pct ,';'        ,
    #   #   'ABS(', cel1, ') < 0,5 * 10^(-RIGHT(CELL(', xlc_fmt, ';', cel1, ');1) - 2);',
    #   #   'ABS(', cel1, ') < 0,5 * 10^(-RIGHT(CELL(', xlc_fmt, ';', cel1, ');1)))'
    #   # )
    # )
    #
    #
    # # if (any(!near0_auto)) {
    # #   near_zero_map[!near0_auto,] <- near_zero_map[!near0_auto,] %>%
    # #     tibble::add_column(value = as.double(hide_near_zero[!near0_auto])) %>%
    # #     dplyr::mutate(rule = paste0(
    # #       'IF(LEFT(CELL(', xlc_format, ';', cel1, ')) = ', xlc_pct ,';',
    # #       'ABS(', cel1, ') < ', value, ';',
    # #       'ABS(', cel1, ') < ', value/100
    # #     ) ) %>% select(-value)
    # # }
    #
    # near_zero_map %>%
    # purrr::pwalk(openxlsx::conditionalFormatting,
    #              wb = wb, type = "expression", style = style_zero)
    #

    # tabs_totrow_groups <-
    #   purrr::pmap(
    #     list(tabs, totrows),
    #     ~ tibble::add_column(..1, totrows = ..2) %>%
    #       dplyr::mutate(totrows = cumsum(as.integer(totrows)) - totrows + 1L) %>%
    #       dplyr::group_by(totrows)
    #   )



    #Colwidths and rowheights --------------------------------------------------
    tibble::tibble(sheet, cols = txt_cols) %>%
      purrr::pwalk(openxlsx::setColWidths, wb = wb, widths = 30)

    autocw <- purrr::map_lgl(colwidth, ~ . == "auto")

    if (any(!autocw)) {
      tibble::tibble(sheet, cols = fmt_cols, widths = colwidth) %>%
        dplyr::filter(!autocw) %>%
        dplyr::group_by(sheet) %>%
        dplyr::mutate(widths = max(as.double(widths))  ) %>%
        dplyr::ungroup() %>%
        purrr::pwalk(openxlsx::setColWidths, wb = wb)
    }

    if (any(autocw)) {
      if (colnames_rotation > 0) {
        if (colnames_rotation > 30 & colnames_rotation < 60) {
          purrr::walk2(sheet[autocw], fmt_cols[autocw],
                       ~ openxlsx::setColWidths(wb, sheet = .x, cols = .y,
                                                widths = 8))
          # purrr::pwalk(list(sheet, tabs, purrr::map(totc, ~ dplyr::if_else(., 13, 8))),
          #              ~ openxlsx::setColWidths(wb, sheet = ..1, cols = ncol(..2), widths = ..3))
        } else if (colnames_rotation > 60) {
          purrr::walk2(sheet[autocw], fmt_cols[autocw],
                       ~ openxlsx::setColWidths(
                         wb, sheet = .x, cols = .y,
                         widths = 6 + 8*cos(colnames_rotation/90*pi/2)
                       )) #Entre 6 et 14
          # purrr::pwalk(list(sheet, tabs, purrr::map(totc, ~ dplyr::if_else(., 13, 6 + 8*cos(colnames_rotation/90*pi/2)))),
          #              ~ openxlsx::setColWidths(wb, sheet = ..1, cols = ncol(..2), widths = ..3))
        }

        purrr::walk(sheet[autocw],
                    ~ openxlsx::setRowHeights(
                      wb, sheet = ., rows = 1,
                      heights = 13.8 + 105*sin(colnames_rotation/90*pi/2)
                    ))

        #Enlarge columns if there is confidence intervals
        # if (any(tab_with_CI_on_sheet)) {
        #   purrr::walk2(1:length(tab_with_CI_on_sheet)[tab_with_CI_on_sheet],
        #         purrr::map(tabs_on_same_sheet, ~ ncol(tabs[[.[1]]]))[tab_with_CI_on_sheet],
        #         ~ openxlsx::setColWidths(wb, sheet = .x, cols = 2:(.y-1), widths = 14))
        # }

      } else {
        purrr::walk2(sheet[autocw], fmt_cols[autocw],
                     ~ openxlsx::setColWidths(wb, sheet = .x, cols = .y,
                                              widths = "auto")) #13
      }
    }

    #Save to file --------------------------------------------------------------
    if (stringr::str_detect(path, "\\\\|/")) {
      dir_path <- path %>% stringr::str_remove("\\\\[^\\\\]+$|/[^/]+$")
      if (! dir.exists(dir_path))  dir.create(dir_path, recursive = TRUE)
    }
    path_name <- stringr::str_remove(path, "\\.xlsx$")
    if (! stringr::str_detect(path, "\\.xlsx$")) path <-
      stringr::str_c(path, ".xlsx")
    if (replace == FALSE) {
      i <- 0
      file_do_not_exist <- FALSE
      while (file_do_not_exist == FALSE) {
        if (file.exists(path)) {
          i = i+1
          path <- stringr::str_c(path_name, i, ".xlsx")
        } else {
          path <-
            stringr::str_c(path_name, dplyr::if_else(i == 0,
                                                     "",
                                                     stringr::str_c(i)),
                           ".xlsx")
          file_do_not_exist <- TRUE
        }
      }
    }
    print(path)
    openxlsx::saveWorkbook(wb, path, overwrite = TRUE)
    if (open == TRUE) { file.show(path) }
  }











detect_totcols <- function(tabs) {
  #detect totcols by col vars names, no position ? ----
  # get_var <- tab_get_vars(tabs)
  tot <- which(is_totcol(tabs))

  purrr::map(1:ncol(tabs), function(.i)
    tidyr::replace_na(names(tot[tot >= .i])[1], "")) %>%
    rlang::syms() %>%
    purrr::set_names(names(tabs))

}



cols_by_type_pct <- function(tabs) {
  types <-
    purrr::map_if(tabs, is_fmt, ~ get_type(.), .else = ~ NA_character_) %>%
    purrr::map_chr(~ dplyr::if_else(length(unique(.)) > 1, "mixed", unique(.)))

  purrr::map2_chr(get_pct_type(tabs), types, function(.pct, .type)
    dplyr::if_else(.type == "pct", .pct, .type)  )
}



#Calculate excel references of relevant cells
xl_index <- function(cols = "", rows = "", start_row = 0L, offset = 1L,
                     fixedcol = FALSE, fixedrow = FALSE) {

  if (class(cols) == "list") cols <- purrr::map_int(cols, ~ .[1])
  if (class(rows) == "list") rows <- purrr::map_int(rows, ~ .[1])

  fixc <- if (fixedcol) { "$" } else { "" }
  fixr <- if (fixedrow) { "$" } else { "" }

  paste0(fixc, purrr::map_chr(cols, ~ paste0(LETTERS[.[1] %/% 26],
                                             LETTERS[.[1] %%  26]) ),
         fixr, as.character(rows + start_row + offset)                       )
}












insufficient_counts <- function(tabs, min_counts = 30) {
  get_vars        <- purrr::map(tabs, tab_get_vars)
  row_var         <- purrr::map(get_vars, ~ rlang::sym(.$row_var))
  col_vars_alltot <- purrr::map(get_vars, ~ rlang::syms(.$col_vars))
  col_vars        <- purrr::map(col_vars_alltot, ~ .[. != "all_col_vars"] )
  col_vars_levels_alltot <-
    purrr::map(get_vars, ~ purrr::map(.$col_vars_levels, rlang::syms))
  col_vars_levels <-
    purrr::map(col_vars_levels_alltot, ~ .[names(.) != "all_col_vars"] )

  groups      <- purrr::map(tabs, dplyr::group_vars)

  totrows     <- purrr::map(tabs, is_totrow)
  totcols     <- purrr::map(tabs, is_totcol)
  all_totcols <- purrr::map(col_vars_alltot, ~ . == "all_col_vars" )


  #Row insufficient counts
  insuff_counts_row_var <- rlang::rep_along(tabs, tibble::tibble())

  #Take column total of any col vars if there are
  each_vars_totcol <-
    purrr::pmap(
      list(tabs, col_vars_levels, groups),
      ~ purrr::map_lgl(..2,
                       function(.levels)
                         dplyr::select(..1, tidyselect::all_of(..3),
                                       !!!.levels) %>%
                         is_totcol() %>% any()
      ))

  # var_is_mean <-
  #   purrr::map2(tabs, col_vars_levels,
  #     ~ purrr::map_lgl(.y,
  #                      function(.levels)
  #                        dplyr::select(dplyr::ungroup(.x), !!!.levels) %>%
  #                        purrr::map_lgl(~ is_mean(.) %>% all()) %>% any()
  #     ))

  any_each_vars_totcol <- purrr::map_lgl(each_vars_totcol, any)
  if(any(any_each_vars_totcol)) {

    col_vars_levels_each_tot <-
      purrr::map2(col_vars_levels[any_each_vars_totcol],
                  each_vars_totcol[any_each_vars_totcol],
                  ~ .x[.y])

    insuff_counts_row_var[any_each_vars_totcol] <-
      tibble::tibble(.tab    = tabs[any_each_vars_totcol],
                     .levels = col_vars_levels_each_tot) %>%
      purrr::pmap(function(.tab, .levels)
        purrr::map(.levels, function(..levels)
          dplyr::select(dplyr::ungroup(.tab), !!!..levels) %>%
            dplyr::select(where(is_totcol)) %>%
            dplyr::mutate(dplyr::across(everything(), ~ get_n(.) < min_counts)) %>%
            tibble::deframe()
        )
      )

    insuff_counts_any_each_vars_totcol <- insuff_counts_row_var #[any_each_vars_totcol]
  }


  #Otherwise, take the whole total column (for all vars) if there is one
  one_all_totcol <- purrr::map_lgl(all_totcols, ~ any(.))
  if( any(one_all_totcol) ) {
    insuff_counts_row_var[one_all_totcol] <-
      tibble::tibble(tabs, .levels = col_vars_levels) %>%
      dplyr::filter(one_all_totcol) %>%

      purrr::pmap(function(tabs, .levels) purrr::map(
        .levels, #! var_is_mean,
        function(..levels)
          dplyr::select(dplyr::ungroup(tabs),
                        where(~ tidyr::replace_na(
                          get_col_var(.) == "all_col_vars", FALSE)
                        )) %>%
          dplyr::mutate(dplyr::across(.fns = ~ get_n(.) < min_counts)) %>%
          tibble::deframe()
      )
      )
  }

  alltot_and_each <- purrr::map2_lgl(any_each_vars_totcol, one_all_totcol,
                                     ~ .x & .y)
  #alltot_and_each <- c(FALSE, FALSE, TRUE, FALSE, FALSE, FALSE)
  if(any(alltot_and_each)) {
    insuff_counts_row_var[alltot_and_each] <-
      tibble::tibble(.all_vars = insuff_counts_row_var,
                     .each     = insuff_counts_any_each_vars_totcol
      ) %>%
      dplyr::filter(alltot_and_each) %>%

      purrr::pmap(function(.all_vars, .each)
        map2_if(.all_vars, .each, names(.all_vars) %in% names(.each),
                .f    = ~ .y,
                .else = ~ .x )
      )
  }

  #For tabs with no totals at all, recalculate
  not_any_totcols <- purrr::map_lgl(totcols, ~ !any(.))
  if(any(not_any_totcols)) {
    insuff_counts_row_var[not_any_totcols] <-
      tibble::tibble(tabs, row_var, .col_vars = col_vars, .levels = col_vars_levels, groups) %>%
      dplyr::filter(not_any_totcols) %>%

      purrr::pmap(function(tabs, row_var, .col_vars, .levels, groups)
        purrr::map(.levels, function(..levels)
          dplyr::select(tabs, tidyselect::all_of(groups), !!!..levels) %>%
            dplyr::mutate(dplyr::across(where(is_fmt), get_n)) %>%
            dplyr::rowwise() %>%
            dplyr::transmute(insuff_counts_row_var = sum(!!!..levels, na.rm = TRUE) < min_counts) %>%
            tibble::deframe()
        )
      )
  }




  #Column insufficient counts
  #match groups and totcols ----
  insuff_counts_col_var <- rlang::rep_along(tabs, tibble::tibble())

  one_totrow <- purrr::map_lgl(totrows, ~ any(.))
  if ( any(one_totrow) ) {
    insuff_counts_col_var[one_totrow] <-
      purrr::map(tabs[one_totrow],
                 ~ dplyr::mutate(., dplyr::across(
                   where(is_fmt), ~ get_n(dplyr::last(.)) < min_counts
                 ))
      )
  }

  if (any(! one_totrow)) {
    insuff_counts_col_var[!one_totrow] <-
      purrr::map(tabs[!one_totrow],
                 ~ dplyr::mutate(., dplyr::across(
                   where(is_fmt), ~ sum(get_n(.)) < min_counts))
      )
  }

  list(insuff_counts_row_var = insuff_counts_row_var,
       insuff_counts_col_var = insuff_counts_col_var)
}

#   tab_xl(tabs)
