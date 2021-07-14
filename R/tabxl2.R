

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
#' @param perc_color_breaks The values to use to color deviations from the mean,
#' when the table shows \code{"row"} or \code{"col"} percentages and
#' \code{color = "auto"}. They must be numbers between - 1 and 1
#' (i.e. -100% and +100%).
#' @param cell_contrib_color_coeffs The values to use to color cell
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
           only_one_sheet = FALSE,
           color = c("auto", "contrib", "no"), comp = "all",
           hide_near_zero = "auto", #c("auto", 0.0049, Inf),
           #no_contribs = FALSE, no_sup = FALSE, no_another_tot = FALSE, no_sup = FALSE, #title
           perc_color_breaks = c("+0.05","+0.10","+0.15","+0.25","*2","+0.35",
                                 "-0.05","-0.10","-0.15","-0.25",     "-0.35"),
           cell_contrib_color_coeffs = c(0.5, 1, 2, 5, 10, -0.5, -1,-2, -5, -10) #c(1, 2, 5, -1,-2, -5)
  ) {
    if (is.data.frame(tabs)) tabs <- list(tabs)
    subtext <- purrr::map(tabs, get_subtext)
    chi2    <- purrr::map(tabs, get_chi2)
    colwidth       <- vec_recycle(colwidth,       length(tabs))
    hide_near_zero <- vec_recycle(hide_near_zero, length(tabs))
    comp           <- vec_recycle(comp,           length(tabs))

    #needed args attributes : ----
    #insufficient_counts
    min_counts <- 300
    insc <- insufficient_counts(tabs, min_counts = min_counts)
    insuff_counts_row_var <- insc$insuff_counts_row_var
    insuff_counts_col_var <- insc$insuff_counts_col_var

    if (remove_tab_vars == TRUE) {
      tab_vars <- purrr::map(tabs, ~ tab_get_vars(.) %>% .$tab_vars)
      tabs <-
        purrr::map2(tabs, tab_vars, ~ dplyr::select(dplyr::ungroup(.x),
                                                    -tidyselect::all_of(.y)))
      insuff_counts_col_var <- insuff_counts_col_var %>%
        purrr::map2(tab_vars, ~ dplyr::select(dplyr::ungroup(.x),
                                              -tidyselect::all_of(.y)))
    }

    get_vars        <- purrr::map(tabs, tab_get_vars)
    row_var         <- purrr::map(get_vars, ~ rlang::sym(.$row_var))
    col_vars_alltot <- purrr::map(get_vars, ~ rlang::syms(.$col_vars))
    col_vars        <- purrr::map(col_vars_alltot, ~ .[. != "all_col_vars"] )
    col_vars_levels_alltot <- purrr::map(get_vars, ~ purrr::map(.$col_vars_levels, rlang::syms))
    col_vars_levels <- purrr::map(col_vars_levels_alltot, ~ .[names(.) != "all_col_vars"] )
    tab_vars <- purrr::map(get_vars, ~ .$tab_vars)

    #groups  <- purrr::map(tabs, dplyr::group_vars)

    totrows     <- purrr::map(tabs, is_totrow)
    totcols     <- purrr::map(tabs, is_totcol)
    all_totcols <- purrr::map(col_vars_alltot, ~ . == "all_col_vars" )

    digits <-
      purrr::map(tabs, ~ purrr::map_if(., purrr::map_lgl(., is_fmt), get_digits,
                                       .else = ~ NA_integer_))

    sheet <- 1L:length(tabs)
    start <- rep(0L, length(tabs))

    tot_rows <- purrr::map2(totrows, start, ~ which(.x) + .y + 1L)
    group_ind <- purrr::map(tabs, dplyr::group_indices)
    end_group   <- purrr::map(group_ind, ~ which(.[-1] != .[-length(.)] ) ) %>%
      purrr::map2(start, ~ .x + .y + 1)
    rows_nb  <- purrr::map2(tabs, start, ~ as.integer(1:nrow(.x) + .y + 1L))
    # start_group <- purrr::map(group_ind, ~ .[-length(.)] != .[-1]   )

    all_cols <- purrr::map(tabs, ~ 1:ncol(.))
    txt_cols <- purrr::map(tabs, ~ which(purrr::map_lgl(., ~ !is_fmt(.))))
    row_var_cols <- purrr::map(txt_cols, ~ .[length(.)])
    fmt_cols <- purrr::map(tabs, ~ which(purrr::map_lgl(., ~ is_fmt(.))))
    tot_cols <- purrr::map(totcols, which)

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

    cols_by_type <-
      purrr::map(types, ~ purrr::map_chr(., ~ dplyr::if_else(
        length(unique(.)) > 1, "mixed", unique(.)
      )  ) )

    cols_by_type_pct <-
      purrr::map2(pct_type, cols_by_type,
                  ~ purrr::map2_chr(.x, .y, function(.pct, .type)
                    dplyr::if_else(.type == "pct", .pct, .type)  )
      )

    mixed_pct_cols <- purrr::map(cols_by_type_pct, ~ purrr::keep(., . == "mixed"))

    cols_by_type_pct <-
      purrr::map(cols_by_type_pct,
                 ~ purrr::set_names(., 1:length(.)) %>%
                   purrr::discard(is.na(.) | . %in% c("mixed", "pct_ci", "mean_ci",
                                                                  "var", "ci", "n"))
      )

    ctr <-
      purrr::map2(tabs,
                  purrr::map(cols_by_type_pct,
                             ~ names(.)[. %in% c("wn", "all", "all_tabs")]),
                  ~ dplyr::rename_with(.x, ~ 1:length(..1)) %>%
                    dplyr::select(as.integer(.y)) %>%
                    dplyr::select(-where(is_totcol)) %>% purrr::map(get_ctr)
      )

    values <-
      purrr::map2(tabs,
                  purrr::map(cols_by_type_pct,
                             ~ names(.)[. %in% c("row", "col", "mean")]),
                  ~ dplyr::rename_with(.x, ~ 1:length(..1)) %>%
                    dplyr::select(as.integer(.y)) %>%
                    dplyr::select(-where(is_totcol)) %>% purrr::map(get_num)
      )






    # col_pct <- purrr::map(pct_type, ~ which(. == "col"))
    # row_pct <- purrr::map(pct_type, ~ which(. == "row"))
    # all_pct <- purrr::map(pct_type, ~ which(. == "all"))
    # alt_pct <- purrr::map(pct_type, ~ which(. == "all_tabs"))




    insuff_counts_row_var <-
      purrr::map2(insuff_counts_row_var, start,
                  ~ purrr::map(.x, function(.var) which(.var) + .y + 1))
    insuff_counts_col_var <- insuff_counts_col_var %>%
      purrr::map(~ .[purrr::map_lgl(., is.logical)]) %>%
      purrr::map_depth(2, which)

    #Not working for ci = "abs"
    tabs_num <-
      purrr::map(tabs, ~ dplyr::mutate(., dplyr::across(where(is_fmt), get_num)))

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

    # Sep between col_vars and groups-----------------------------------------
    st_end_col_var <- openxlsx::createStyle(border = "right")

    tibble::tibble(sheet, rows = purrr::map(rows_nb, ~ c(1L, .)),
                   cols = purrr::pmap(list(txt_cols, end_col_var,
                                           purrr::map(tot_cols, ~ c(. - 1, .))),
                                      c)) %>% #row_var_cols
      dplyr::filter(purrr::map_lgl(cols, ~ length(.) != 0) ) %>%
      purrr::pwalk(openxlsx::addStyle, wb = wb, gridExpand = TRUE, stack = T,
                   style = st_end_col_var)

    st_end_group <- openxlsx::createStyle(border = "bottom")

    tibble::tibble(sheet, rows = end_group, cols = all_cols) %>%
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

    tibble::tibble(sheet, rows = start + 1, cols = all_cols) %>%
      purrr::pwalk(openxlsx::addStyle, wb = wb, gridExpand = TRUE, stack = T,
                   style = headers)


    st_totrows <-
      openxlsx::createStyle(halign = "right", valign = "top",
                            textDecoration = "Bold", border = "TopBottom",
                            borderStyle = c("thin", "double"))

    tibble::tibble(sheet, rows = tot_rows, cols = fmt_cols) %>%
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

    tibble::tibble(sheet, rows = rows_nb, cols = tot_cols) %>%
      dplyr::filter(purrr::map_lgl(tot_cols, ~ length(.) != 0) ) %>%
      purrr::pwalk(openxlsx::addStyle, wb = wb, gridExpand = TRUE, stack = T,
                   style = st_totcols)

    st_bottomline <-
      openxlsx::createStyle(border = "bottom", borderStyle = "thin")

    tibble::tibble(sheet, rows = purrr::map2(tabs, start, ~ nrow(.) + .y + 1L),
                   cols = all_cols) %>%
      purrr::pwalk(openxlsx::addStyle, wb = wb, gridExpand = TRUE, stack = T,
                   style = st_bottomline)


    # Insufficient counts ----------------------------------------------------
    st_insufficient_counts <- openxlsx::createStyle(fontColour = "#909090")

    insuff_col_cols <-
      purrr::map2(insuff_counts_col_var, row_var_cols, ~ 1:length(.x) + .y)

    list(sheet, insuff_counts_col_var, insuff_col_cols, start) %>%
      purrr::pmap(~ purrr::map2_df(
        ..2, ..3,
        function(.x, .y) tibble::tibble(sheet = ..1,
                                        cols  = .y, rows  = list(.x + ..4 + 1)
        ) ) ) %>%
      dplyr::bind_rows() %>%
      dplyr::filter(purrr::map_lgl(rows, ~ length(.) != 0)) %>%
      purrr::pwalk(openxlsx::addStyle, wb = wb, gridExpand = TRUE, stack = T,
                   style = st_insufficient_counts)

    insuff_row_rows <-
      purrr::map2(insuff_counts_row_var, col_vars_names, function(ins, cols)
        purrr::map_if(1:length(cols), cols %in% names(ins),
                      .f    = ~ ins[cols[.]] %>% purrr::map(as.integer),
                      .else = ~ list(integer())
        ) %>% purrr::set_names(names(cols)) %>% purrr::map(purrr::flatten_int)
      )

    purrr::map2(sheet, insuff_row_rows, function(sh, ins) purrr::map2_df(
      ins, 1:length(ins),
      function(.x, .y) tibble::tibble(sheet = sh,
                                      cols  = .y, rows  = list(.x)
      ) ) ) %>%
      dplyr::bind_rows() %>%
      dplyr::filter(purrr::map_lgl(rows, ~ length(.) != 0)) %>%
      dplyr::group_by(sheet, rows) %>%
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
      purrr::map(~ openxlsx::createStyle(numFmt = as.character(.)))

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

    mean_color_breaks <- c("*1.15","*1.25","*1.5","*2", NA_real_, "*4",
                           "/1.15","/1.25","/1.5","/2",           "/4")

    conditional_fmt_styles <- tibble::tibble(
      style   = c("st_plus1", "st_plus2", "st_plus3",
                  "st_plus4", "st_plus4", "st_plus5", # plus4 2 times (pct 25 + ×2)
                  "st_minus1" , "st_minus2", "st_minus3",
                  "st_minus4", "st_minus5"),
      pctbrk  = perc_color_breaks,
      meanbrk = mean_color_breaks,
      sign    = purrr::map_chr(perc_color_breaks, ~ dplyr::if_else(
        condition = stringr::str_detect(., "^\\+|^\\*"),
        true      = ">",
        false     = "<"                                          )),
      coeffs  = c(cell_contrib_color_coeffs[1:4], # add NA for 2e st_plus4
                  NA_real_, cell_contrib_color_coeffs[5:10]) %>% as.character()
    )

    # Database of parameters to map conditional formatting to excel ------------

    #kept types for conditional formatting :
    # c("row", "col", "all", "all_tabs", "wn", "ctr", "mean")
    # warning with mixed_pct_cols ----


    # tibble::tibble(sheet, type = cols_by_type_pct,
    #                cols = purrr::map(cols_by_type_pct, ~ as.integer(names(.))),
    #                tot_rows, tot_cols,
    #                tab_nb = 1:length(sheet), comp, ctr, values) %>%
    #   tidyr::unnest(c(type, cols)) %>%
    #   dplyr::group_by(sheet, type) %>%
    #   dplyr::mutate(ct = cols != dplyr::lag(cols + 1, default = TRUE),
    #                 ct = cumsum(as.integer(ct))) %>%
    #   #dplyr::group_by(ct, .add = TRUE) %>%
    #   #tidyr::nest(cols = cols) %>%
    #   new_tab()
    #
    #
    # tibble::tibble(sheet, cols = cols_by_type_pct, tot_rows, tot_cols,
    #                tab_nb = 1:length(sheet), comp, ctr, values) %>%
    #   purrr::pmap(function(sheet, cols, tot_rows, tot_cols,
    #                        tab_nb, comp, ctr, values)
    #     tibble::tibble(sheet, type = cols, cols = as.integer(names(cols))) %>%
    #       dplyr::group_by(type) %>%
    #       dplyr::mutate(ct = cols != dplyr::lag(cols + 1, default = TRUE),
    #                     ct = cumsum(as.integer(ct))) %>%
    #       dplyr::group_by(ct, .add = TRUE) %>%
    #       dplyr::summarise(sheet = dplyr::first(sheet), cols = list(cols),
    #                        tot_rows = list(tot_rows), tot_cols = list(tot_cols),
    #                        tab_nb, comp, ctr = list(ctr), values = list(values),
    #                        .groups = "drop")
    #   ) %>% dplyr::bind_rows()
    #

    #For each table, create one line of parameters for earch column types ;
    # and in each type, one line for each continuous set of columns
    conditional_fmt_map <-
      tibble::tibble(sheet, cols = cols_by_type_pct, tot_rows, tot_cols,
                     tab_nb = 1:length(sheet), comp, ctr, values) %>%
      purrr::pmap(function(sheet, cols, tot_rows, tot_cols,
                           tab_nb, comp, ctr, values)
        tibble::tibble(sheet, type = cols, cols = as.integer(names(cols))) %>%
          dplyr::group_by(type) %>%
          dplyr::mutate(ct = cols != dplyr::lag(cols + 1, default = TRUE),
                        ct = cumsum(as.integer(ct))) %>%
          dplyr::group_by(ct, .add = TRUE) %>%
          dplyr::summarise(sheet = dplyr::first(sheet), cols = list(cols),
                           tot_rows = list(tot_rows), tot_cols = list(tot_cols),
                           tab_nb, comp, ctr = list(ctr), values = list(values),
                           .groups = "drop")
      ) %>% dplyr::bind_rows() %>%
      dplyr::select(sheet, type, ct, dplyr::everything())

    #Remove total rows and cols when they are not relevant for type or comp ;
    #  tot_cols are excluded from cols, and groups with just tot_cols removed
    conditional_fmt_map <- conditional_fmt_map %>%
      dplyr::mutate(
        cols     = purrr::map2(.data$cols, .data$tot_cols,
                               ~ purrr::discard(.x, .x %in% .y)),
        tot_cols = dplyr::if_else(
          condition = .data$type == "col", # + ctr ?
          true      = .data$tot_cols,
          false     = purrr::map(.data$tot_cols, max)
        ),
        tot_rows = dplyr::if_else( # + "all" "wn" ?
          condition = .data$comp == "tab" & .data$type %in% c("row", "mean", "ctr"),
          true      = .data$tot_rows,
          false     = purrr::map(.data$tot_rows, max)
        )
      ) %>%
      dplyr::filter(purrr::map_lgl(cols, ~ length(.) != 0)) %>%
      dplyr::select(-comp)
    #comp = "all" needs a totaltab with a total row ----

    #If there are several total column, create one line of parameters for each ;
    # in "ctr", mean contrib calculated on all vars, not on each vars ----
    totc2 <- purrr::map_lgl(conditional_fmt_map$tot_cols, ~ length(.) >= 2)
    if ( any(totc2) ) {
      conditional_fmt_map <- conditional_fmt_map %>%
        purrr::pmap(function(sheet, type, ct, cols, tot_rows, tot_cols, tab_nb,
                             ctr, values)
          tibble::tibble(sheet, type, ct, cols = list(cols),
                         tot_rows = list(tot_rows), tot_cols, tab_nb,
                         ctr = list(ctr), values = list(values) ) %>%
            dplyr::mutate(
              cols = dplyr::if_else(
                condition = dplyr::n() > 1 & ! type %in% c("mean"), #See for wn ?
                true      = purrr::pmap(
                  list(cols, tot_cols, dplyr::lag(tot_cols, default = 0L)),
                  ~ purrr::discard(..1, ..1 >= ..2 | ..1 <= ..3)
                ),
                false     = cols
              )
            )
        ) %>% dplyr::bind_rows()
    } else {
      conditional_fmt_map <- conditional_fmt_map %>%
        dplyr::mutate(tot_cols = purrr::flatten_int(.data$tot_cols))
    }
    conditional_fmt_map <- conditional_fmt_map %>%
      dplyr::mutate(col1 = purrr::map_int(.data$cols, ~ .[1]))


    #When several total rows, create on line of parameters for each, if needed
    totr2 <- purrr::map_lgl(conditional_fmt_map$tot_rows, ~ length(.) >= 2)
    if ( any(totr2) ) {
      conditional_fmt_map <-  conditional_fmt_map %>%
        purrr::pmap(function(sheet, type, ct, cols, tot_rows, tot_cols,
                             tab_nb, ctr, values, col1)
          tibble::tibble(sheet, type, ct, cols = list(cols), tot_rows, tot_cols,
                         tab_nb, col1, ctr = list(ctr), values = list(values) ) %>%
            dplyr::mutate(row1 = dplyr::lag(tot_rows, default = 1L) + 1L)
        ) %>% dplyr::bind_rows()
    } else {
      conditional_fmt_map <- conditional_fmt_map %>%
        dplyr::mutate(tot_rows = purrr::flatten_int(.data$tot_rows),
                      row1 = 2L)
    }

    #Calculate excel references of relevant cells
    xl_index <- function(cols = "", rows = "",
                         fixedcol = FALSE, fixedrow = FALSE) {

      fixc <- if (fixedcol) { "$" } else { "" }
      fixr <- if (fixedrow) { "$" } else { "" }

      paste0(fixc, purrr::map_chr(cols, ~ paste0(LETTERS[.[1] %/% 26],
                                                 LETTERS[.[1] %%  26]) ),
             fixr, as.character(rows)                                    )
    }

    conditional_fmt_map <- conditional_fmt_map %>%
      dplyr::mutate(
        rows = purrr::map2(row1, tot_rows, ~ .x:.y),
        cel1 = xl_index(col1, row1),
        totc = xl_index(tot_cols, row1, fixedcol = TRUE),
        totr = xl_index(col1, tot_rows, fixedrow = TRUE),
        tott = xl_index(tot_cols, tot_rows,
                        fixedrow = TRUE, fixedcol = TRUE),
        meanctr = purrr::pmap_dbl(
          list(cols, tot_cols, rows, tot_rows, type), ~ dplyr::if_else(
            condition = ..5 %in% c("all", "all_tabs", "wn", "ctr"),
            true      = 1 / ( length(purrr::discard(..1, ..1 == ..2)) *
                                length(purrr::discard(..3, ..3 == ..4)) ),
            false     = NA_real_
          ) ) %>% replace(. == Inf, NA_real_)
      ) %>%
      # dplyr::group_by(tab_nb) %>%
      # dplyr::mutate(totg = xl_index(tot_cols, max(tot_rows),
      #                               fixedrow = TRUE, fixedcol = TRUE)) %>%
      # dplyr::ungroup() %>%
      dplyr::select(tab_nb, sheet, type, ct, cols, rows, tot_cols, tot_rows,
                    col1, row1, dplyr::everything())
    #mean_contrib is wrong if lines have been removed : attr ? ----

    #Not calculate cols if not necessary by types




    # Calculate one conditional formating rule for each line of parameters
    rule_factory <- function(type, cel1, totc, totr, tott, meanctr) { #totg
      rule <- rlang::rep_along(type, list())

      trow  <- type == "row"
      tcol  <- type == "col"
      tall  <- type == "all"
      tallt <- type == "all_tabs"
      twn   <- type == "wn"
      tctr  <- type == "ctr"
      tmean <- type == "mean"

      rule_row <- function(cel1, totr) {      # B2 > B$T + 0.05
        function(pctbrk, meanbrk, sign,  coeffs)
          paste0(cel1, sign, totr, pctbrk)
      }

      rule_col <- function(cel1, totc) {      # B2 > $T2 + 0.05
        function(pctbrk, meanbrk, sign,  coeffs)
          paste0(cel1, sign, totc, pctbrk)
      }

      rule_ctr <- function(cel1, meanctr) {   # B2 > 2 * meanctr
        function(pctbrk, meanbrk, sign,  coeffs)
          paste0(cel1, sign, coeffs, "*", meanctr)
      }

      rule_mean <- function(cel1, totr) {     # B2 > B$2 * 1.10
        function(pctbrk, meanbrk, sign,  coeffs)
          paste0(cel1, sign, totr, meanbrk)
      }

      #contrib :
      # ...nbcell, "-", ...nbcell, "+",
      # ...cell_contrib, sign2, coeff, "*", .mean_contrib


      rule[trow ] <- purrr::map2(cel1[trow], totr[trow], rule_row)
      rule[tcol ] <- purrr::map2(cel1[tcol], totc[tcol], rule_col)
      rule[tall ] <- list(NA_character_)
      rule[tallt] <- list(NA_character_)
      rule[twn  ] <- list(NA_character_)
      rule[tctr ] <- purrr::map2(cel1[tctr], meanctr[tctr], rule_ctr)
      rule[tmean] <- purrr::map2(cel1[tmean], totr[tmean], rule_mean)

      rule
    }

    conditional_fmt_map <- conditional_fmt_map %>%
      dplyr::mutate(rule_gen = rule_factory(
        type = type, cel1 = cel1, totc = totc, totr = totr, tott = tott,
        meanctr = meanctr)
      ) %>%
      dplyr::filter(!is.na(rule_gen))


    # Apply each conditional formatting rules for any shade of green/orange
    conditional_fmt_final <-
      conditional_fmt_map %>%
      tibble::add_column(styles_df = list(conditional_fmt_styles)) %>%
      tidyr::unnest(styles_df) %>%
      dplyr::filter(! (is.na(cel1)|is.na(coeffs)|is.na(meanbrk)|is.na(pctbrk)|
                         (type == "row"              & is.na(totr   ) ) |
                         (type %in% c("col", "mean") & is.na(totc   ) ) |
                         (type == "ctr"              & is.na(meanctr) )   )
      ) %>%
      dplyr::mutate(rule = purrr::pmap_chr(
        list(rule_gen, pctbrk, meanbrk, sign, coeffs),
        function(fn, .pctbrk, .meanbrk, .sign, .coeffs)
          rlang::exec(fn, pctbrk = .pctbrk, meanbrk = .meanbrk,
                      sign = .sign, coeffs = .coeffs)
      ) )
    # dplyr::mutate(rule = purrr::map2_chr(
    #   rule_gen,
    #   purrr::transpose(list(pctbrk, meanbrk, sign, coeffs)),
    #   function(fn, args) rlang::exec(fn, !!!args)
    # ) )
    #dplyr::select(conditional_fmt_map, -tab_nb, -ct, -(tot_cols:row1) )


    #For each conditional formatting, test if realy used ? ----

    conditional_fmt_final %>%
      dplyr::select(sheet, cols, rows, rule, style) %>%
      purrr::pwalk(function(sheet, cols, rows, rule, style)
        openxlsx::conditionalFormatting(
          wb, sheet = sheet, cols = cols, rows = rows,
          rule = rule, style = rlang::eval_tidy(rlang::sym(style)))
      )







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
