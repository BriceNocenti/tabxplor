

#' Excel output for tabxplor tables, with formatting and colors
#'
#' @param tabs A table made with \code{\link{tab}}, \code{\link{tab_many}} or
#' \code{\link{tab_core}}, or a list of such tables.
#' @param path,replace,open The name, and possibly the path, of the Excel file to
#' create (possibly without the .xlsx extension). Default path to temporary directory.
#' Set global option \code{"tabxplor.export_dir"} with \code{link[base:options]{options}}
#' to change default directory.
#' Use \code{replace = TRUE} to overwrite existing files. Use \code{open = TRUE}
#' if you don't want to automatically open the tables in Excel (or another
#' software associated with .xlsx files).
#' @param colnames_rotation Rotate the names of columns to an angle (in degrees).
#' @param remove_tab_vars By default, \code{tab_vars} columns are removed to gain space.
#' Set to \code{FALSE} to keep them.
#' @param colwidth The standard width for numeric columns, as a number.
#' Default to \code{"auto"}.
#' @param print_ci By default provided confidence intervals are printed in another table,
#' left to the base table. Set to \code{FALSE} to dismiss.
#' @param print_color_legend Should the color legends be printed with the subtexts ?
#' @param sheets The Excel sheets options :
#' \itemize{
#'   \item \code{"tabs"}: a new sheet is created for each table
#'   \item \code{"unique"}: all tables are on the same sheet
#'   \item \code{"auto"}: subsequent tables with the same columns are printed on the
#'    same sheets
#' }
#' @param min_counts The total count under which a column or row is turned pale grey
#' because there is not enough observation for it to be significant. Default to 30.
#' @param hide_near_zero By default all cells displayed as 0 (even rounded)
#' turn pale grey, to make the distribution of empty cells (and other cells) more visible.
#' Provide a number to turn grey every cell below it. Set to \code{Inf} not to use
#' this feature.
#' @param pct_breaks The breaks used to color percentages.
#' @param mean_breaks The breaks used to color means.
#' @param contrib_breaks The breaks used to color contributions of cells to variance.
#'
#' @return  The table(s) with formatting and colors in an Excel file, as a side effect.
#'  Invisibly returns \code{tabs}.
#' @export
#'
#' @examples
#' \donttest{
#' forcats::gss_cat %>%
#'   tab(marital, race, pct = "row", color = "diff") %>%
#'   tab_xl()
#'   }
tab_xl <-
  function(tabs, path = NULL, replace = FALSE, open = rlang::is_interactive(),
           colnames_rotation = 0, remove_tab_vars = TRUE,
           colwidth = "auto", print_ci = TRUE, print_color_legend = TRUE,
           sheets = "tabs", min_counts = 30,
           hide_near_zero = "auto", #c("auto", 0.0049, Inf),

           pct_breaks     = get_color_breaks("pct"),
           mean_breaks    = get_color_breaks("mean"),
           contrib_breaks = get_color_breaks("contrib") #c(1, 2, 5, -1,-2, -5)
  ) {
    if (!requireNamespace("openxlsx", quietly = TRUE)) {
      stop(paste0("Package \"openxlsx\" needed for this function to work. ",
                  "You can install it with : install.packages('openxlsx')"),
           call. = FALSE)
    }

     stopifnot(length(pct_breaks   ) >= 1,
              length(mean_breaks   ) >= 1,
              length(contrib_breaks) >= 1 )

    tabs_base <- tabs
    if (is.data.frame(tabs)) tabs <- list(tabs)
    chi2           <- purrr::map(tabs, get_chi2)
    colwidth       <- vctrs::vec_recycle(colwidth,       length(tabs))
    hide_near_zero <- vctrs::vec_recycle(hide_near_zero, length(tabs))

    get_vars        <- purrr::map(tabs, tab_get_vars)
    col_vars_levels_alltot <- purrr::map(get_vars, ~ purrr::map(.$col_vars_levels,
                                                                rlang::syms))
    col_vars_levels <- purrr::map(col_vars_levels_alltot,
                                  ~ .[names(.) != "all_col_vars"] )
    col_vars_levels_no_tot <-
      purrr::map2(col_vars_levels, purrr::map(tabs, ~ is_totcol(.) %>%
                                                purrr::keep(. == TRUE) %>%
                                                names()),
                  ~ purrr::map(.x, function(.) as.character(.) %>%
                                 purrr::discard(. %in% .y) ) %>%
                    purrr::flatten_chr()
      )
    row_vars        <- purrr::map(get_vars, ~ .$row_var)
    col_vars_plain  <- purrr::map(get_vars, ~ .$col_vars %>%
                                    purrr::discard(. == "all_col_vars"))
    tab_vars <- purrr::map(get_vars, ~ .$tab_vars)
    comp     <- dplyr::if_else(
      purrr::map_lgl(tabs, ~purrr::map_lgl(.x, get_comp_all) %>%
                       purrr::discard(is.na(.)) %>% any()),
      "all",
      "tab" )
    #add arg attribute : insufficient_counts ? ----



    stopifnot(sheets %in% c("tabs", "unique", "auto") |
                (is.integer(sheets) & length(sheets) == length(tabs)))
    sheet <-
      if (is.character(sheets)) {
        switch(sheets,
               "tabs"    = 1L:length(tabs)      ,
               "unique"  = rep(1L, length(tabs)),
               "auto"    = purrr::map2_lgl(
                 col_vars_levels_no_tot,
                 dplyr::lag(col_vars_levels_no_tot,
                            default = col_vars_levels_no_tot[1]),
                 ~ !identical(sort(.x), sort(.y))
               ) %>%
                 as.integer() %>% cumsum() + 1L
        )
      } else if (is.integer(sheets)) {
        sheets
      }


    # conditional formatting styles
    # Above mean : shades of green  (for example, 5% to 35%)
    st_plus1  <- openxlsx::createStyle(fgFill = "#e6f2e6")            #f2f9f2  #b3d9b5
    st_plus2  <- openxlsx::createStyle(fgFill = "#b3d9b5")            #b3d9b5  #8ec690
    st_plus3  <- openxlsx::createStyle(fgFill = "#82c083")            #68b36b  #68b36b
    st_plus4  <- openxlsx::createStyle(fgFill = "#48ad4c")            #3c903f  #43a047
    st_plus5  <- openxlsx::createStyle(fgFill = "#3a8a3d")            #2c5e2c  #3c903f   #fontColour = "#ffffff"

    # Below mean : shades of orange (for example, -5% to -35%)
    st_minus1 <- openxlsx::createStyle(fgFill = "#ffeddf")  #fff6ef   #ffeddf     #fbcaa2   #f7c3c2
    st_minus2 <- openxlsx::createStyle(fgFill = "#fbcaa2")            #fbcaa2     #f9b57d   #f4afae
    st_minus3 <- openxlsx::createStyle(fgFill = "#fcb073")            #f7a058     #f7a058   #ef8885
    st_minus4 <- openxlsx::createStyle(fgFill = "#de873f")  #b66f36   #de873f     #f79646   #ea605d
    st_minus5 <- openxlsx::createStyle(fgFill = "#c36a21")  #b66f36               #de873f   #e53935   #, fontColour = "#ffffff"

    st_plus1_ci  <- openxlsx::createStyle(fontColour = "#2e6e31")
    st_minus1_ci <- openxlsx::createStyle(fontColour = "#9c551a")

    style_pos <- c("st_plus1", "st_plus2", "st_plus3", "st_plus4", "st_plus5")
    style_neg <- c("st_minus1" , "st_minus2", "st_minus3", "st_minus4",
                   "st_minus5")

    lbrk <- max(length(pct_breaks), length(mean_breaks), length(contrib_breaks))

    pct_ci_breaks  <- pct_breaks - pct_breaks[1]
    mean_ci_breaks <- mean_breaks/mean_breaks[1]

     if (lbrk >= 2) {
      pct_brksup     <- c(pct_breaks    [2:lbrk], Inf)
      mean_brksup    <- c(mean_breaks   [2:lbrk], Inf)
      contrib_brksup <- c(contrib_breaks[2:lbrk], Inf)
      pct_ci_brksup  <- c(pct_ci_breaks [2:lbrk], Inf)
      mean_ci_brksup <- c(mean_ci_breaks[2:lbrk], Inf)
    } else {
      pct_brksup <- mean_brksup <- contrib_brksup <-
        pct_ci_brksup <- mean_ci_brksup <- Inf
    }

    pct_breaks     <- c(pct_breaks    ,
                        rep(NA_real_, lbrk - length(pct_breaks)))
    mean_breaks    <- c(mean_breaks   ,
                        rep(NA_real_, lbrk - length(mean_breaks)))
    contrib_breaks <- c(contrib_breaks,
                        rep(NA_real_, lbrk - length(contrib_breaks)))

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

    pct_breaks         <- pct_breaks      %>% c(., -.)
    mean_breaks        <- mean_breaks     %>% c(., 1/.)
    contrib_breaks     <- contrib_breaks  %>% c(., -.)
    pct_ci_breaks      <- pct_ci_breaks   %>% c(., -.)
    mean_ci_breaks     <- mean_ci_breaks  %>% c(., -.) #then - again

    pct_brksup     <- pct_brksup       %>% c(., -.)
    mean_brksup    <- mean_brksup      %>% c(., 1/.)
    contrib_brksup <- contrib_brksup   %>% c(., -.)
    pct_ci_brksup  <- pct_ci_brksup    %>% c(., -.)
    mean_ci_brksup <- mean_ci_brksup   %>% c(., -.) #then - again

    style <- c(style_pos, style_neg)

    sign = c(rep(">", lbrk), rep("<", lbrk))

    conditional_fmt_styles <- tibble::tibble(
      style, sign,
      pct_breaks, mean_breaks, contrib_breaks,
      pct_ci_breaks, mean_ci_breaks,
      pct_brksup, mean_brksup, contrib_brksup,
      pct_ci_brksup, mean_ci_brksup
    )

    subtext      <- purrr::map(tabs, get_subtext) #need breaks calculation first
    if (print_color_legend == TRUE) {
      color_legend <- purrr::map(tabs, ~ tab_color_legend(., colored = FALSE))
      color_legend <- color_legend %>%
        purrr::map_if(purrr::map_lgl(., ~ !is.null(.)),
                      ~ purrr::map_if(., 1:length(.) == 1,
                                      ~ paste0("Colors: ", .)) %>%
                        purrr::flatten_chr())
      subtext      <- purrr::map2(subtext, color_legend, ~ c(.y, .x))
    }

    titles <-
      purrr::pmap(list(tabs, row_vars, col_vars_plain, tab_vars),
                  ~ tab_get_titles(..1, ..2, ..3, ..4)
      )

    insc <- insufficient_counts(tabs, min_counts = min_counts)
    insuff_counts_row_var <- insc$insuff_counts_row_var
    insuff_counts_col_var <- insc$insuff_counts_col_var

    no_chi2 <- purrr::map_lgl(chi2, ~ nrow(.) == 0)
    tabs_chi2 <- rep(list(tibble::tibble()), length(chi2))

    if (any(!no_chi2)) {
      prep_tabs_chi2 <- tabs[!no_chi2] %>%
        purrr::map_if(
          comp == "tab",
          ~ tibble::add_column(., totrows = is_totrow(.)) %>%
            dplyr::mutate(`chi2 stats` = dplyr::case_when(
              .data$totrows                                       ~ NA_character_,
              dplyr::lead(.data$totrows, n = 1L, default = FALSE) ~ "count"   ,
              dplyr::lead(.data$totrows, n = 2L, default = FALSE) ~ "pvalue"  ,
              dplyr::lead(.data$totrows, n = 3L, default = FALSE) ~ "variance",
              dplyr::lead(.data$totrows, n = 4L, default = FALSE) ~ "cells"   ,
              dplyr::lead(.data$totrows, n = 5L, default = FALSE) ~ "df"      ,
              TRUE                                                ~ NA_character_
            )) %>%
            dplyr::select(-totrows, - where(is_fmt)) %>%
            dplyr::ungroup(),

          .else = ~ dplyr::ungroup(.) %>%
            dplyr::mutate(
              last = dplyr::row_number() == dplyr::n(),
              `chi2 stats` = dplyr::case_when(
                .data$last                                       ~ NA_character_,
                dplyr::lead(.data$last, n = 1L, default = FALSE) ~ "count"   ,
                dplyr::lead(.data$last, n = 2L, default = FALSE) ~ "pvalue"  ,
                dplyr::lead(.data$last, n = 3L, default = FALSE) ~ "variance",
                dplyr::lead(.data$last, n = 4L, default = FALSE) ~ "cells"   ,
                dplyr::lead(.data$last, n = 5L, default = FALSE) ~ "df"      ,
                TRUE                                             ~ NA_character_
              )) %>%
            dplyr::select(-last, -where(is_fmt))
        )

      join_vars <- purrr::map2(
        tab_vars[!no_chi2], comp[!no_chi2],
        ~ if (.y == "tab") {append(., "chi2 stats")} else {"chi2 stats"}
      )

      tabs_chi2[!no_chi2] <-
        purrr::pmap(list(prep_tabs_chi2, chi2[!no_chi2], join_vars),
                    ~ dplyr::left_join(..1, ..2, by = ..3, suffix = c(" ", "")) %>%
                      dplyr::select(.data$`chi2 stats`, where(is_fmt)) %>%
                      dplyr::mutate(dplyr::across(
                        where(is_fmt),
                        function(.var) tidyr::replace_na(.var, fmt0(type = "var"))
                      )) %>%
                      dplyr::mutate(dplyr::across(
                        where(is_fmt), get_num
                      )) %>% dplyr::mutate(`chi2 stats` =
                                             stringr::str_c(.data$`chi2 stats`, " :"))
        )
    }
    # join_vars2 <- purrr::map2(tab_vars[!no_chi2], comp[!no_chi2],
    #                           ~ if (.y == "tab") {.} else {character()} )
    # nbcells <- rep(list(tibble::tibble()), length(chi2))
    # nbcells[!no_chi2] <-  chi2[!no_chi2] %>%
    #   purrr::map(~dplyr::filter(., `chi2 stats` == "cells" ) %>%
    #                dplyr::select(-`chi2 stats`, -tidyselect::any_of("row_var")) %>%
    #                dplyr::mutate(dplyr::across(where(is_fmt),
    #                                            ~ as.integer(get_num(.)))) %>%
    #                dplyr::select(-where(~ is.integer(.) & all(is.na(.))))
    #   )
    #
    # nbcells[!no_chi2]  <-
    #   purrr::pmap(list(tabs[!no_chi2], nbcells[!no_chi2], join_vars2),
    #               ~ dplyr::ungroup(dplyr::select(..1, -where(is_fmt))) %>%
    #                 dplyr::left_join(..2, by = ..3, suffix = c(".var", ""))
    #   )
    #
    # pvalues <- rep(list(tibble::tibble()), length(chi2))
    # pvalues[!no_chi2] <-  chi2[!no_chi2] %>%
    #   purrr::map(~dplyr::filter(., `chi2 stats` == "pvalue" ) %>%
    #                dplyr::select(-`chi2 stats`, -tidyselect::any_of("row_var")) %>%
    #                dplyr::mutate(dplyr::across(where(is_fmt),
    #                                            ~ get_num(.) <= 0.05)) %>%
    #                dplyr::select(-where(~ is.logical(.) & all(is.na(.))))
    #   )
    # pvalues[!no_chi2]  <-
    #   purrr::pmap(list(tabs[!no_chi2], pvalues[!no_chi2], join_vars2),
    #               ~ dplyr::ungroup(dplyr::select(..1, -where(is_fmt))) %>%
    #                 dplyr::left_join(..2, by = ..3, suffix = c(".var", ""))
    #   )

    if (remove_tab_vars == TRUE) {
      tabs <-
        purrr::map2(tabs, tab_vars, ~ dplyr::select(dplyr::ungroup(.x),
                                                    -tidyselect::all_of(.y)))
      insuff_counts_col_var <- insuff_counts_col_var %>%
        purrr::map2(tab_vars, ~ dplyr::select(dplyr::ungroup(.x),
                                              -tidyselect::all_of(.y)))
    }

    if (print_ci == TRUE) {
      no_ci <-
        purrr::map_lgl(tabs, ~ purrr::map_lgl(dplyr::select(., where(is_fmt)),
                                              ~ all(is.na(get_ci(.)))
        ) %>% all()
        )
    } else {
      no_ci <- rep(TRUE, length(tabs))
    }


    tabs_ci <- rep(list(tibble::tibble()), length(tabs))
    tabs_ci[!no_ci] <-
      purrr::map(tabs[!no_ci],
                 ~ dplyr::mutate(., dplyr::across(where(is_fmt),
                                                  ~ set_display(., "ci") %>%
                                                    set_color("ci")))
      )

    tabs_ci_num <- rep(list(tibble::tibble()), length(tabs))
    tabs_ci_num[!no_ci] <-
      purrr::map(tabs[!no_ci],
                 ~ dplyr::mutate(., dplyr::across(where(is_fmt), get_ci))
      )

    ci_refs <-
      purrr::map(tabs[!no_ci],
                 ~ dplyr::mutate(., dplyr::across(
                   where(~ is.character(.) | is.factor(.)),
                   ~ ""
                 )) %>%
                   dplyr::mutate(dplyr::across(
                     where(is_fmt),
                     ~ dplyr::if_else(
                       condition = get_ci_type(.) == "diff" & get_reference(.),
                       true = paste0(
                         "ref x-",
                         format(
                           if (get_type(.) == "mean") {
                             set_display(., "mean")
                           } else {
                             set_display(., "pct")
                           }
                         ) ),
                       false = "")
                   ))
      )

    if (remove_tab_vars == FALSE) {
      tabs_ci <-
        purrr::map2(tabs_ci, tab_vars, ~ dplyr::select(dplyr::ungroup(.x),
                                                       -tidyselect::all_of(.y)))
      tabs_ci_num <-
        purrr::map2(tabs_ci_num, tab_vars,
                    ~ dplyr::select(dplyr::ungroup(.x),-tidyselect::all_of(.y)))

      ci_refs <-
        purrr::map2(ci_refs, tab_vars,
                    ~ dplyr::select(dplyr::ungroup(.x),-tidyselect::all_of(.y)))
    }





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

    newsheet = sheet != dplyr::lag(sheet, default = -1L)

    start <- tibble::tibble(newsheet, rows = purrr::map_int(tabs, nrow),
                            sub = purrr::map_int(subtext, length)) %>%
      dplyr::group_by(gr = cumsum(as.integer(.data$newsheet))) %>%
      dplyr::mutate(start = dplyr::lag(cumsum(.data$rows + .data$sub + 5L),
                                       default = 0L) + 1L) %>%
      dplyr::pull(.data$start)


    #Not working for ci = "cell"
    tabs_num <-
      purrr::map(tabs, ~ dplyr::mutate(., dplyr::across(where(is_fmt), get_num)) %>%
                   tibble::as_tibble())

    totrows  <- purrr::map(totrowsTF, which)
    tot_rows <- purrr::map2(totrows, start, ~ .x + .y + 1L)
    group_ind<- purrr::map(tabs, dplyr::group_indices)
    end_group<- purrr::map(group_ind, ~ which(.[-1] != .[-length(.)] ) ) %>%
      purrr::map2(start, ~ .x + .y + 1)
    rows_nb  <- purrr::map2(tabs, start, ~ as.integer(1:nrow(.x) + .y + 1L))

    all_cols <- purrr::map(tabs, ~ 1:ncol(.))
    all_cols_chi2_ci <- purrr::pmap(list(tabs, tabs_chi2, tabs_ci),
                                    ~ 1:(ncol(..1) + ncol(..2) + ncol(..3)) )
    txt_cols <- purrr::map(tabs, ~ which(purrr::map_lgl(., ~ !is_fmt(.))))
    row_var_cols <- purrr::map(txt_cols, ~ .[length(.)])
    fmt_cols <- purrr::map(tabs, ~ which(purrr::map_lgl(., ~ is_fmt(.))))
    totcols  <- purrr::map(totcols, which)

    col_vars_names <- purrr::map(tabs, ~ get_col_var(.) )
    end_col_var <-
      purrr::map(col_vars_names,
                 ~ which(. != "" & . != dplyr::lead(., default = NA_character_))
      )

    display <- purrr::map2(tabs, fmt_cols, ~ purrr::map(.x[.y], get_display))
    digits  <- purrr::map2(tabs, fmt_cols, ~ purrr::map(.x[.y], get_digits ))

    type  <- purrr::map(tabs, get_type)

    color <- purrr::map(tabs, get_color)

    color_type <- get_color_type(color, type)

    color_cols <- purrr::map(color_type, ~which(!is.na(.)))
    color_type <- color_type %>% purrr::map2(color_cols, ~ .x[.y])
    # plain_type_for_color <- type %>% purrr::map2(color_cols, ~ .x[.y])

    type       <- type  %>% purrr::map2(fmt_cols, ~ .x[.y])

    insuff_counts_row_var <-
      purrr::map2(insuff_counts_row_var, start,
                  ~ purrr::map(.x, function(.var) which(.var) + .y + 1))
    insuff_counts_col_var <- insuff_counts_col_var %>%
      purrr::map(~ .[purrr::map_lgl(., is.logical)]) %>%
      purrr::map_depth(2, which)


    sheet_titles <-
      purrr::pmap_chr(list(tabs,
                      purrr::map(row_var[newsheet], as.character),
                      purrr::map(col_vars[newsheet], as.character)),
                      ~ tab_get_titles(..1, ..2, ..3, max = 1)
      ) %>% stringr::str_sub(., 1, 27)

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
                ~ openxlsx::freezePane(wb, sheet = .x, firstActiveRow  = 3,
                                       firstCol = TRUE))

    st_titles <- openxlsx::createStyle(fontSize = 12, textDecoration = "bold")
    tibble::tibble(sheet, startRow = start + 1L - 1L, startCol = 1L, x = titles) %>%
      purrr::pwalk(openxlsx::writeData, wb = wb) %>%
      dplyr::select(.data$sheet, rows = .data$startRow, cols = .data$startCol) %>%
      purrr::pwalk(openxlsx::addStyle, wb = wb, stack = TRUE, style = st_titles)

    subtext_style <- openxlsx::createStyle(halign = "left", valign = "center",
                                           fontSize = 9)
    tibble::tibble(sheet, x = subtext, startCol = 1L,
                   startRow = purrr::map2_int(start, tabs,
                                              ~ nrow(.y) + .x + 2L)) %>%
      dplyr::filter(purrr::map_lgl(subtext, ~ length(.) != 0)) %>%
      dplyr::filter(purrr::map_lgl(subtext, ~ any(!is.na(.) & . != ""))) %>%
      purrr::pwalk(openxlsx::writeData, wb = wb) %>%
      dplyr::mutate(rows = purrr::map2(.data$startRow, .data$x,
                                       ~ .x:(.x + length(.y) - 1)),
                    cols = .data$startCol) %>%
      dplyr::select(-.data$startRow, -.data$startCol, -.data$x) %>%
      purrr::pwalk(openxlsx::addStyle, wb = wb, stack = TRUE, gridExpand = TRUE,
                   style = subtext_style)

    #    write references for diff, ci as main display, etc. ? ----

    fmt_cols_ci    <- fmt_cols
    all_cols_ci    <- all_cols
    end_col_var_ci <- end_col_var
    totcols_ci     <- totcols

    #Chi2 and variance informations --------------------------------------------
    chi2_col1 <- purrr::map(tabs, ~ ncol(.) + 1)
    chi2_cols <- rep(list(integer()), length(tabs))
    if (any(!no_chi2)) {
      chi2_cols[!no_chi2] <-
        purrr::map2(chi2_col1[!no_chi2], tabs_chi2[!no_chi2],
                    ~ (.x + 1):(.x - 1 + ncol(.y)) )

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
                  dplyr::pull(tab, .data$`chi2 stats`) %in% c("pvalue :")
              ) + start + 1L
            ) %>% purrr::flatten_int() %>% unique() %>% sort()
          )

        tibble::tibble(sheet = sheet[!no_chi2], rows = pvalue_chi2_inf,
                       chi2_col1 = chi2_col1[!no_chi2],
                       cols = chi2_cols[!no_chi2]) %>%
          tidyr::unnest(tidyselect::all_of(c("chi2_col1", "rows"))) %>%
          dplyr::mutate(rule = ">= 0.05") %>%
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

      headers_chi2 <- openxlsx::createStyle(
        halign = "center", valign = "bottom", wrapText = TRUE,
        textDecoration = "Bold", fontSize = 9)
      tibble::tibble(sheet, rows = start + 1, cols = chi2_cols) %>%
        dplyr::filter(!no_chi2) %>%
        purrr::pwalk(openxlsx::addStyle, wb = wb, gridExpand = TRUE, stack = T,
                     style = headers_chi2)

      headers_chi2_col1 <- openxlsx::createStyle(halign = "right")
      tibble::tibble(sheet, rows = start + 1, cols = chi2_col1) %>%
        dplyr::filter(!no_chi2) %>%
        purrr::pwalk(openxlsx::addStyle, wb = wb, gridExpand = TRUE, stack = T,
                     style = headers_chi2_col1)

    }

    # Confidence intervals tables ----------------------------------------------
    offset  <- rep(0L, length(tabs))
    ci_col1 <- rep(list(integer()), length(tabs))
    ci_cols <- rep(list(integer()), length(tabs))
    if (any(!no_ci)) {
      ci_col1 <- purrr::map2(tabs, tabs_chi2, ~ ncol(.x) + ncol(.y) + 1L)

      ci_cols <- purrr::map2(ci_col1, tabs_ci, ~ (.x + 1L):(.x - 1L + ncol(.y)) ) %>%
        purrr::map2(tabs_ci, ~ purrr::set_names(.x, names(.y)[-1]))

      tibble::tibble(sheet = sheet, x = tabs_ci_num,
                     startRow = purrr::map(start, ~. + 1L), startCol = ci_col1) %>%
        dplyr::filter(!no_ci) %>%
        purrr::pwalk(openxlsx::writeData, wb = wb)

      tibble::tibble(sheet = sheet, cols = ci_col1,
                     rows = purrr::map2(rows_nb, start, ~ c(.y + 1, .x))) %>%
        dplyr::filter(!no_ci) %>%
        purrr::pwalk(openxlsx::deleteData, wb = wb, gridExpand = TRUE)

      offset[!no_ci] <- purrr::map2_int(fmt_cols, ci_cols, ~ .y[1] - .x[1])
      fmt_cols_ci[!no_ci]    <- fmt_cols[!no_ci] %>%
        purrr::map2(offset[!no_ci] , ~ c(.x, .x + .y))

      all_cols_ci[!no_ci]    <- all_cols[!no_ci] %>%
        purrr::map2(ci_cols[!no_ci] , ~ c(.x, .y))

      end_col_var_ci[!no_ci] <- end_col_var[!no_ci] %>%
        purrr::map2(offset[!no_ci] , ~ c(.x, .x + .y))

      totcols_ci[!no_ci]     <- totcols[!no_ci] %>%
        purrr::map2(offset[!no_ci] , ~ c(.x, .x + .y))

      st_ci_borders <- openxlsx::createStyle(border = "right")

      tibble::tibble(sheet = sheet, rows = rows_nb,
                     cols = purrr::map2(ci_col1, ci_cols,
                                        ~ c(.x, .y[length(.y)])) ) %>%
        dplyr::filter(!no_ci) %>%
        purrr::pwalk(openxlsx::addStyle, wb = wb, gridExpand = TRUE, stack = T,
                     style = st_ci_borders)
    }

    # Sep between col_vars and groups (copy for ci tabs) -----------------------
    st_end_col_var <- openxlsx::createStyle(border = "right")

    tibble::tibble(sheet, rows = purrr::map(rows_nb, ~ c(min(. -1), .)),
                   cols = purrr::pmap(list(txt_cols, end_col_var_ci, ci_col1,
                                           purrr::map(totcols_ci, ~ c(. - 1L, .))),
                                      ~ c(..1, ..2, ..3, ..4) %>% unique())) %>%
      dplyr::filter(purrr::map_lgl(.data$cols, ~ length(.) != 0) ) %>%
      purrr::pwalk(openxlsx::addStyle, wb = wb, gridExpand = TRUE, stack = T,
                   style = st_end_col_var)

    st_end_group <- openxlsx::createStyle(border = "bottom")

    tibble::tibble(sheet, rows = end_group, cols = all_cols_ci) %>%
      dplyr::filter(purrr::map_lgl(.data$rows, ~ length(.) != 0) ) %>%
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

    tibble::tibble(sheet, rows = start + 1, cols = all_cols_ci) %>%
      purrr::pwalk(openxlsx::addStyle, wb = wb, gridExpand = TRUE, stack = T,
                   style = headers)

    st_totrows <-
      openxlsx::createStyle(halign = "right", valign = "top",
                            textDecoration = "Bold", border = "TopBottom",
                            borderStyle = c("thin", "double"))

    tibble::tibble(sheet, rows = tot_rows, cols = fmt_cols_ci) %>%
      dplyr::filter(purrr::map_lgl(.data$rows, ~ length(.) != 0) ) %>%
      purrr::pwalk(openxlsx::addStyle, wb = wb, gridExpand = TRUE, stack = T,
                   style = st_totrows)

    st_totrows_text <-
      openxlsx::createStyle(halign = "left", valign = "top", wrapText = TRUE,
                            textDecoration = "Bold", border = "TopBottom",
                            borderStyle = c("thin", "double"))

    tibble::tibble(sheet, rows = tot_rows, cols = txt_cols) %>%
      dplyr::filter(purrr::map_lgl(.data$rows, ~ length(.) != 0) ) %>%
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
      dplyr::filter(purrr::map_lgl(.data$rows, ~ length(.) != 0))

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
      dplyr::filter(purrr::map_lgl(.data$rows, ~ length(.) != 0))

    if (nrow(insuff_row_map) != 0) {
      insuff_row_map %>% dplyr::group_by(.data$sheet, .data$rows) %>%
        dplyr::summarise(sheet = dplyr::last(sheet),
                         cols  = list(.data$cols),
                         rows  = dplyr::last(.data$rows), .groups = "drop") %>%
        purrr::pwalk(openxlsx::addStyle, wb = wb, gridExpand = TRUE, stack = T,
                     style = st_insufficient_counts)

    }

    #Digits ----------------------------------------------------------------
    numfmt <- function(n, type, display) {
      display <- vctrs::vec_recycle(display, length(n))

      base_plus_ci <- display %in% c("pct_ci", "mean_ci")
      pct     <- display %in% c("pct", "ctr") |
        (display == "ci" & type %in% c("row", "col", "all", "all_tabs"))
      ci      <- display == "ci"
      n_inf   <- n < 0
      n_0     <- n == 0

      rep0_n <- purrr::map_chr(abs(n), ~ paste0(rep("0", .), collapse = ""))

      res <- dplyr::case_when(
        base_plus_ci             ~ "TEXT",

        pct & n_inf              ~ NA_character_,
        pct & n_0                ~ "0%",
        pct                      ~ paste0("0.", rep0_n, "%"),

        n_0                      ~ "#,##0",
        n_inf                    ~ paste0(
          "#,",
          purrr::map_chr(abs(n), ~ paste0(rep("#", 2 - . %%3 ),
                                          collapse = "")),
          purrr::map_chr(abs(n), ~ paste0(rep("0", 1 + . %%3 ),
                                          collapse = "")),
          purrr::map_chr(abs(n), ~ paste0(rep(",",    . %/%3 ),
                                          collapse = ""))      ),
        TRUE                     ~ paste0("#,##0.", rep0_n)
      )

      dplyr::if_else(ci, paste0(stringi::stri_unescape_unicode("\\u00b1"), res), res)
    }

    digits_map <-
      tibble::tibble(sheet, digits, cols = fmt_cols,
                     type, display, start, offset, tab_nb = 1:length(sheet)) %>%
      tidyr::unnest(c(.data$digits, .data$display, .data$type, .data$cols)) %>%
      dplyr::mutate(rows = purrr::map2(.data$digits, .data$start,
                                       ~ 1:length(.x)+ .y + 1L)) %>%
      tidyr::unnest(c(.data$digits, .data$display, .data$rows)) %>%
      dplyr::filter(!is.na(.data$display) & !is.na(.data$digits)) %>%
      dplyr::mutate(num_format =
                      forcats::as_factor(numfmt(.data$digits, .data$type,
                                                .data$display))) %>%
      dplyr::group_by(.data$num_format) %>%
      dplyr::mutate(num_name = paste0("st_digits", as.integer(.data$num_format)))

    #assign one variable for each number style
    number_styles <- digits_map %>%
      dplyr::summarise(num_name = dplyr::last(.data$num_name), .groups = "drop") %>%
      dplyr::select(.data$num_name, .data$num_format) %>% tibble::deframe() %>%
      purrr::map(~ openxlsx::createStyle(fontName = "DejaVu Sans",
                                         numFmt = as.character(.)))

    purrr::iwalk(number_styles,
                 ~ assign(.y, .x, pos = parent.env(rlang::current_env())))

    digits_map %>% dplyr::group_by(.data$sheet, .data$num_name) %>%
      dplyr::summarise(cols = list(.data$cols), rows = list(.data$rows),
                       .groups = "drop") %>%
      dplyr::relocate(.data$num_name, .after = -1) %>%
      purrr::pwalk(function(sheet, cols, rows, num_name) openxlsx::addStyle(
        wb, stack = TRUE,
        sheet = sheet, cols = cols, rows = rows,
        style = rlang::eval_tidy(rlang::sym(num_name))
      ))

    #     digits (and references) for confidence intervals tables
    if (any(!no_ci)) {
      digits_map_ci <-  digits_map %>%
        dplyr::ungroup() %>%
        dplyr::select(-.data$num_format, -.data$num_name) %>%
        dplyr::filter(.data$tab_nb %in% which(!no_ci)) %>%
        dplyr::mutate(num_format_ci =
                        forcats::as_factor(numfmt(.data$digits + 1L, .data$type, "ci")),
                      cols = .data$cols + .data$offset) %>%
        dplyr::group_by(.data$num_format_ci) %>%
        dplyr::mutate(num_name_ci = paste0("st_digits_ci",
                                           as.integer(.data$num_format_ci)))

      number_ci_styles <- digits_map_ci %>%
        dplyr::summarise(num_name_ci = dplyr::last(.data$num_name_ci), .groups = "drop") %>%
        dplyr::select(.data$num_name_ci, .data$num_format_ci) %>% tibble::deframe() %>%
        purrr::map(~ openxlsx::createStyle(fontName = "DejaVu Sans",
                                           numFmt = as.character(.),
                                           fontColour = "#b3b3b3"))

      purrr::iwalk(number_ci_styles,
                   ~ assign(.y, .x, pos = parent.env(rlang::current_env())))

      digits_map_ci %>% dplyr::group_by(.data$sheet, .data$num_name_ci) %>%
        dplyr::summarise(cols = list(.data$cols), rows = list(.data$rows),
                         .groups = "drop") %>%
        dplyr::relocate(.data$num_name_ci, .after = -1) %>%
        purrr::pwalk(function(sheet, cols, rows, num_name_ci) openxlsx::addStyle(
          wb, stack = TRUE,
          sheet = sheet, cols = cols, rows = rows,
          style = rlang::eval_tidy(rlang::sym(num_name_ci))
        ))

      ci_ref_map <-
        tibble::tibble(sheet = sheet[!no_ci], start, offset,
                       x  = purrr::map_depth(ci_refs, 2, ~ .),
                       startCol = purrr::map(ci_refs, ~ 1:ncol(.)),
                       startRow = purrr::map(ci_refs, ~ 1:nrow(.))) %>%
        tidyr::unnest(c(.data$startCol, .data$x)) %>%
        tidyr::unnest(c(.data$startRow, .data$x)) %>%
        dplyr::filter(!is.na(.data$x) & .data$x != "") %>%
        dplyr::mutate(startCol = .data$startCol + .data$offset,
                      startRow = .data$startRow + .data$start + 1L)

      ci_ref_map %>%
        dplyr::select(.data$sheet, .data$x, .data$startCol, .data$startRow) %>%
        purrr::pwalk(openxlsx::writeData, wb = wb, colNames = FALSE)
    }


    #conditional formatting (made with normal color formatting) ----------------
    color_selections <-
      purrr::map2(tabs, color_cols, ~ purrr::map(
        .x[.y],
        ~ fmt_color_selection(., force_breaks = conditional_fmt_styles) %>%
          purrr::map(which)
      ) )

    conditional_fmt_map <-
      tibble::tibble(sheet, cols = color_cols, rows = color_selections,
                     start, offset) %>%
      tidyr::unnest(c(.data$cols, .data$rows)) %>%
      tibble::add_column(style = list(style)) %>%
      tidyr::unnest(c(.data$rows, .data$style)) %>%
      dplyr::filter(purrr::map_lgl(.data$rows, ~ length(.) != 0)) %>%
      dplyr::mutate(cols  = purrr::map2(.data$cols, .data$rows,
                                        ~ rep(.x, length(.y))),
                    rows  = purrr::map2(.data$rows, .data$start, ~ .x + .y + 1L)) %>%
      dplyr::group_by(.data$sheet, .data$style) %>%
      dplyr::summarise(cols = list(.data$cols), rows = list(.data$rows),
                       offset = .data$offset[1],
                       .groups = "drop") %>%
      dplyr::mutate(dplyr::across(tidyselect::all_of(c("cols", "rows")),
                                  ~ purrr::map(., purrr::flatten_int)))

    conditional_fmt_map %>%
      dplyr::select(.data$sheet, .data$rows, .data$cols, .data$style) %>%
      purrr::pwalk(function(sheet, cols, rows, style)
        openxlsx::addStyle(
          wb = wb, stack = TRUE,
          sheet = sheet, cols = cols, rows = rows,
          style = rlang::eval_tidy(rlang::sym(style))
        ))

    if (any(!no_ci)) {
      color_selections_ci <-
        purrr::map2(tabs_ci[!no_ci], color_cols[!no_ci], ~ purrr::map(
          .x[.y],
          ~ fmt_color_selection(
            ., force_breaks =
              conditional_fmt_styles[c(1, nrow(conditional_fmt_styles)/2+1),]) %>%
            purrr::map(which)
        ) )

      conditional_fmt_map <-
        tibble::tibble(sheet = sheet[!no_ci],
                       cols = purrr::map2(color_cols[!no_ci], offset[!no_ci],
                                          ~ .x + .y),
                       rows = color_selections_ci, start) %>%
        tidyr::unnest(c(.data$cols, .data$rows)) %>%
        tibble::add_column(style = list(
          paste0(   .data$style[c(1, length(.data$style)/2 + 1)],    "_ci") #.data$ ??
        )) %>%
        tidyr::unnest(c(.data$rows, .data$style)) %>%
        dplyr::filter(purrr::map_lgl(.data$rows, ~ length(.) != 0)) %>%
        dplyr::mutate(cols  = purrr::map2(.data$cols, .data$rows,
                                          ~ rep(.x, length(.y))),
                      rows  = purrr::map2(.data$rows, .data$start, ~ .x + .y + 1L)) %>%
        dplyr::group_by(.data$sheet, .data$style) %>%
        dplyr::summarise(cols = list(.data$cols), rows = list(.data$rows),
                         offset = .data$offset[1],
                         .groups = "drop") %>%
        dplyr::mutate(dplyr::across(tidyselect::all_of(c("cols", "rows")),
                                    ~ purrr::map(., purrr::flatten_int)))

      conditional_fmt_map %>%
        dplyr::select(.data$sheet, .data$rows, .data$cols, .data$style) %>%
        purrr::pwalk(function(sheet, cols, rows, style)
          openxlsx::addStyle(
            wb = wb, stack = TRUE,
            sheet = sheet, cols = cols, rows = rows,
            style = rlang::eval_tidy(rlang::sym(style))
          ))

      st_ci_ref <- openxlsx::createStyle(fontColour = "black")

      ci_ref_map %>%
        dplyr::select(sheet, cols = .data$startCol, rows = .data$startRow) %>%
        purrr::pwalk(openxlsx::addStyle, wb = wb, stack = TRUE,
                     style = st_ci_ref)

    }

    #Numbers near zero in white gray -------------------------------------------
    style_zero <- openxlsx::createStyle(fontColour = "#EAEAEA")
    near0_auto <- hide_near_zero == "auto"

    if (any(near0_auto)) {
      digits_map %>%
        dplyr::ungroup() %>%
        dplyr::filter(.data$tab_nb %in% (1:length(tabs))[near0_auto]) %>%
        dplyr::mutate(digits = dplyr::if_else(
          condition = .data$display %in% c("pct", "diff", "ctr") |
            (.data$display == "ci" & .data$type %in% c("row", "col", "all", "all_tabs")),
          true      = .data$digits + 2L,
          false     = .data$digits                                     ),
          hide_near_zero = 0.49 * 10^(-.data$digits)) %>%
        dplyr::mutate(continuous = .data$rows != dplyr::lag(.data$rows + 1,
                                                            default = TRUE),
                      continuous = cumsum(as.integer(.data$continuous))) %>%
        tidyr::nest(rows = .data$rows) %>%
        dplyr::mutate(rows = purrr::map(.data$rows, ~ dplyr::pull(., .data$rows))) %>%
        dplyr::mutate(continuous = .data$cols != dplyr::lag(.data$cols + 1,
                                                            default = TRUE),
                      continuous = cumsum(as.integer(.data$continuous))) %>%
        tidyr::nest(cols = .data$cols) %>%
        dplyr::mutate(cols = purrr::map(.data$cols, ~ dplyr::pull(., .data$cols))) %>% # .data$ ?
        dplyr::mutate(rule = purrr::map(hide_near_zero, ~ c(-., .)) ) %>%
        dplyr::select(.data$sheet, .data$cols, .data$rows, .data$rule) %>%
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


    #Colwidths and rowheights --------------------------------------------------
    tibble::tibble(sheet, cols = txt_cols) %>%
      purrr::pwalk(openxlsx::setColWidths, wb = wb, widths = 30)

    autocw <- purrr::map_lgl(colwidth, ~ . == "auto")

    if (any(!autocw)) {
      tibble::tibble(sheet, cols = purrr::map2(fmt_cols, ci_cols, c),
                     widths = colwidth) %>%
        dplyr::filter(!autocw) %>%
        dplyr::group_by(.data$sheet) %>%
        dplyr::mutate(widths = max(as.double(.data$widths))  ) %>%
        dplyr::ungroup() %>%
        purrr::pwalk(openxlsx::setColWidths, wb = wb)
    }

    if (any(autocw)) {
      if (colnames_rotation > 0) {
        if (colnames_rotation > 30 & colnames_rotation < 60) {
          purrr::pwalk(list(sheet[autocw], fmt_cols[autocw], ci_cols[autocw]),
                       ~ openxlsx::setColWidths(wb, sheet = ..1,
                                                cols = c(..2, ..3),
                                                widths = 8))
          # purrr::pwalk(list(sheet, tabs, purrr::map(totc, ~ dplyr::if_else(., 13, 8))),
          #              ~ openxlsx::setColWidths(wb, sheet = ..1, cols = ncol(..2), widths = ..3))
        } else if (colnames_rotation > 60) {
          purrr::pwalk(list(sheet[autocw], fmt_cols[autocw], ci_cols[autocw]),
                       ~ openxlsx::setColWidths(
                         wb, sheet = ..1, cols = c(..2, ..3),
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
        purrr::pwalk(list(sheet[autocw], fmt_cols[autocw], ci_cols[autocw]),
                     ~ openxlsx::setColWidths(wb, sheet = ..1,
                                              cols = c(..2, ..3),
                                              widths = "auto")) #13
      }
    }

    #Save to file --------------------------------------------------------------
    if (is.null(path)) {
      path <- getOption("tabxplor.export_dir")
      if (is.null(path)) {
        path <- file.path(tempdir(), "Tab")
      } else {
        path <- file.path(path, "Tab")
      }
    }

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
    if (open == TRUE) { openxlsx::openXL(path) } #file.show

    invisible(tabs_base)
  }






tab_get_titles <- function(tabs, row, col, tab, max = 3) {
  res <- dplyr::case_when(
    row ==  "no_row_var" & length(col) <= max ~ paste(col, collapse = ", "),
    row ==  "no_row_var" & length(col) >  max ~ paste(col[1:max], "etc.",
                                                      collapse = ", "),
    all(col ==  "no_col_var")           ~ row,
    length(row) == 1 & length(col) <= max ~ paste(row, "by",
                                                  paste(col, collapse = ", ")),
    length(row) == 1 & length(col) >  max ~ paste(row, "by multi"),
  )
  if (!missing(tab)) {
    if (length(tab) >= 1) res <-
        if (length(tabs) >= 2) {
          paste0(res, " (tabbed by ", paste(tab, collapse = ", "), ")")
        } else {
          paste0(res, " (tabbed by ", tab, ")")
        }
  }
  res
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
            dplyr::mutate(dplyr::across(dplyr::everything(), ~ get_n(.) < min_counts)) %>%
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
      tibble::tibble(tabs, row_var, .col_vars = col_vars,
                     .levels = col_vars_levels, groups) %>%
      dplyr::filter(not_any_totcols) %>%

      purrr::pmap(function(tabs, row_var, .col_vars, .levels, groups)
        purrr::map(.levels, function(..levels)
          dplyr::select(tabs, tidyselect::all_of(groups), !!!..levels) %>%
            dplyr::mutate(dplyr::across(where(is_fmt), get_n)) %>%
            dplyr::rowwise() %>%
            dplyr::transmute(insuff_counts_row_var =
                               sum(!!!..levels, na.rm = TRUE) < min_counts) %>%
            dplyr::pull(insuff_counts_row_var)
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


# tab_xl using conditional formatting formulas (on contiguous rows and cols)
#
#
#
#    #For each table, create one line of parameters for earch column display ;
# and in each type, one line for each continuous set of columns
# conditional_fmt_map <-
#   tibble::tibble(sheet, color = color_type, cols = color_cols,
#                  rows = color_selections, start) %>%
#   tidyr::unnest(c(color, cols, rows)) %>%
#   dplyr::group_by(sheet, color) %>%
#   dplyr::mutate(ct = cols != dplyr::lag(cols + 1, default = TRUE),
#                 ct = cumsum(as.integer(ct))) %>%
#   tidyr::nest(cols = cols, rows = rows) %>%
#   dplyr::mutate(dplyr::across(
#     tidyselect::all_of(c("cols", "rows")),
#     ~ purrr::imap(.x, ~ dplyr::pull(., .y))
#   ))
#
#     #conditional_fmt_map[conditional_fmt_map$type =="col",] %>% dplyr::pull(values)
#
#     #Remove total rows and cols when they are not relevant for type or comp ;
#     #  totcols are excluded from cols, and groups with just totcols removed
#     conditional_fmt_map <- conditional_fmt_map %>%
#       dplyr::mutate(
#         cols     = purrr::map2(.data$cols, .data$totcols,
#                                ~ purrr::discard(.x, .x %in% .y)),
#         totcols = dplyr::if_else(
#           condition = .data$type == "col", # + ctr ?
#           true      = .data$totcols,
#           false     = purrr::map(.data$totcols, max)
#         ),
#         totrows = dplyr::if_else( # + "all" "wn" ?
#           condition = .data$comp == "tab" & .data$type %in% c("row", "mean", "ctr"),
#           true      = .data$totrows,
#           false     = purrr::map(.data$totrows, max)
#         )
#       ) %>%
#       dplyr::filter(purrr::map_lgl(cols, ~ length(.) != 0)) %>%
#       dplyr::select(-comp)
#     #comp = "all" needs a totaltab with a total row
#
#
#
#     #If there are several total column, create one line of parameters for each ;
#     conditional_fmt_map <- conditional_fmt_map %>%
#       tidyr::unnest(totcols) %>%
#       dplyr::group_by(ct, .add = TRUE) %>%
#       dplyr::mutate(
#         cols = dplyr::if_else(
#           condition = dplyr::n() > 1 & ! type %in% c("mean"), #See for wn ?
#           true      = purrr::pmap(
#             list(cols, totcols, dplyr::lag(totcols, default = 0L)),
#             ~ purrr::discard(..1, ..1 >= ..2 | ..1 <= ..3)
#           ),
#           false     = cols),
#         col1 = purrr::map_int(.data$cols, ~ .[1])
#       ) %>%
#       dplyr::filter(purrr::map_lgl(cols, ~ length(.) != 0))
#
#     #When several total rows, create a line of parameters for each, if needed
#     conditional_fmt_map <- conditional_fmt_map %>%
#       tidyr::unnest(totrows) %>%
#       dplyr::group_by(totcols, .add = TRUE) %>%
#       dplyr::mutate(
#         row1 = dplyr::lag(totrows, default = 0L) + 1L,
#         rows = purrr::map2(row1, totrows, ~ .x:.y),
#         rowsctr = dplyr::if_else(type %in% c("wn", "all", "all_tabs"),
#                                  true    = rows,
#                                  false   = list(NA_integer_)),
#         colsctr = dplyr::if_else(type %in% c("wn", "all", "all_tabs"),
#                                  true    = cols,
#                                  false   = list(NA_integer_)),
#       ) %>%
#       dplyr::ungroup() %>%
#       tidyr::unnest(colsctr) %>%
#       tidyr::unnest(rowsctr) %>%
#       dplyr::mutate(rows = dplyr::if_else(type %in% c("wn", "all", "all_tabs"),
#                                           true    = as.list(rowsctr),
#                                           false   = rows),
#                     cols = dplyr::if_else(type %in% c("wn", "all", "all_tabs"),
#                                           true    = as.list(colsctr),
#                                           false   = cols),
#       ) %>%
#       dplyr::select(-colsctr, -rowsctr)
#
#
#     # #Select test values corresponding to kept rows, cols, and totcols
#     # c_ctr <- conditional_fmt_map$type %in% c("wn", "all", "all_tabs")
#     # conditional_fmt_map[! c_ctr,] <- conditional_fmt_map[!c_ctr,] %>%
#     #   dplyr::mutate(
#     #     spreads = purrr::pmap(list(spreads, cols, rows),
#     #                           ~ ..1[..3,..2] %>%
#     #                             tidyr::pivot_longer(cols = dplyr::everything()) %>%
#     #                             dplyr::pull(value)
#     #     ) )
#     #
#     # conditional_fmt_map[c_ctr,] <- conditional_fmt_map[c_ctr,] %>%
#     #   dplyr::mutate(
#     #     spreads = purrr::pmap(list(spreads, cols, rows), ~ ..1[[..3,..2]] )
#     #   )
#
#     conditional_fmt_map <- conditional_fmt_map %>%
#       dplyr::mutate(
#         cel1 = dplyr::if_else(type %in% c("wn", "all", "all_tabs"),
#                               true    = xl_index(cols   , rows   , start),
#                               false   = xl_index(col1   , row1   , start)),
#         totc = xl_index(totcols, row1   , start, fixedcol = TRUE),
#         totr = xl_index(col1   , totrows, start, fixedrow = TRUE),
#         tott = xl_index(totcols, totrows, start,
#                         fixedrow = TRUE, fixedcol = TRUE),
#         rows = purrr::map2(rows, start, ~ .x + .y + 1),#not use for calc with df
#       ) %>%
#       # dplyr::group_by(tab_nb) %>%
#       # dplyr::mutate(totg = xl_index(totcols, max(totrows),
#       #                               fixedrow = TRUE, fixedcol = TRUE)) %>%
#       # dplyr::ungroup() %>%
#       dplyr::select(tab_nb, sheet, type, ct, cols, rows, totcols, totrows,
#                     col1, row1, dplyr::everything())
#
#     #Not calculate cols if not necessary by type
#     # if (any(!no_ci)) {
#     #   conditional_fmt_map <- conditional_fmt_map %>%
#     #     dplyr::mutate(
#     #       col1_offset = col1 + purrr::flatten_int(offset)[.data$tab_nb],
#     #       cel1_ci = dplyr::if_else(
#     #         condition = .data$color %in% c("row_ci_diff", "row_ci_spread",
#     #                                        "col_ci_diff" , "col_ci_spread" ,
#     #                                        "mean_ci_diff", "mean_ci_spread"),
#     #         true  = xl_index(col1_offset   , row1   , start),
#     #         false = NA_character_
#     #       )
#     #     )
#
#   }
#
#
# # Calculate one conditional formating rule for each line of parameters
# rule_factory <- function(type_ci, cel1, color,
#                          totc, totr, tott, ctr, cel1_ci) { #totg
#   rule <- rlang::rep_along(type_ci, list())
#
#   trow        <- type_ci == "row"
#   tcol        <- type_ci == "col"
#   tmean       <- type_ci == "mean"
#   tctr        <- type_ci %in% c("wn", "all", "all_tabs", "ctr")
#
#   trow_no_ci   <- color == "diff_no_ci"    & type_ci == "row_ci_diff"
#   trow_no_cis  <- color == "diff_no_ci"    & type_ci == "row_ci_spread"
#   tcol_no_ci   <- color == "diff_no_ci"    & type_ci == "col_ci_diff"
#   tcol_no_cis  <- color == "diff_no_ci"    & type_ci == "col_ci_spread"
#   tmean_no_ci  <- color == "diff_no_ci"    & type_ci == "mean_ci_diff"
#   tmean_no_cis <- color == "diff_no_ci"    & type_ci == "mean_ci_spread"
#
#   trow_af_ci   <- color == "diff_after_ci" & type_ci == "row_ci_diff"
#   trow_af_cis  <- color == "diff_after_ci" & type_ci == "row_ci_spread"
#   tcol_af_ci   <- color == "diff_after_ci" & type_ci == "col_ci_diff"
#   tcol_af_cis  <- color == "diff_after_ci" & type_ci == "col_ci_spread"
#   tmean_af_ci  <- color == "diff_after_ci" & type_ci == "mean_ci_diff"
#   tmean_af_cis <- color == "diff_after_ci" & type_ci == "mean_ci_spread"
#   #tctr2  <- type == "ctr"
#
#
#   # color = c("auto", "contrib", "diff", "diff_no_ci", "diff_after_ci", "no")
#
#
#   rule_row_pct <- function(cel1, totr) {      # B2 > B$T + 0.05
#     function(brk, sign)
#       paste0(cel1, sign, totr, ifelse(sign == ">", "+", ""), brk)
#   }
#   rule_col_pct <- function(cel1, totc) {      # B2 > $T2 + 0.05
#     function(brk, sign)
#       paste0(cel1, sign, totc, ifelse(sign == ">", "+", ""), brk)
#   }
#   rule_mean <- function(cel1, totr) {     # B2 > B$2 * 1.10
#     function(brk, sign)
#       paste0(cel1, sign, totr, "*", brk)
#   }
#   rule_ctr <- function(cel1, ctr) {   # B2 - B2 + ctr > 2
#     function(brk, sign)
#       paste0(cel1, "-", cel1, "+", ctr, sign, brk)
#   }
#
#   rule_row_pct_diff_no_ci <- function(cel1, totr, cel1_ci) {
#     function(brk, sign)
#       paste0("AND(", cel1, sign, totr, ifelse(sign == ">", "+", ""), brk,
#              ", ABS(", cel1, "-", totr, ") -",cel1_ci, ">0)")
#   }  # B2 > B$T + 0.05 & ABS(B2 - B$T) - ci > 0
#   rule_row_pct_diff_no_ci_spread <- function(cel1, totr, cel1_ci) {
#     function(brk, sign)
#       paste0("AND(", cel1, sign, totr, ifelse(sign == ">", "+", ""), brk,
#              ", ", cel1_ci, ">0)")
#   }  # B2 > B$T + 0.05 & ci > 0
#   rule_col_pct_diff_no_ci <- function(cel1, totc, cel1_ci) {
#     function(brk, sign)
#       paste0("AND(", cel1, sign, totc, ifelse(sign == ">", "+", ""), brk,
#              ", ABS(", cel1, "-", totc, ") -", cel1_ci, ">0)")
#   } # B2 > $T2 + 0.05 & ABS(B2 - $T2) - ci > 0
#   rule_col_pct_diff_no_ci_spread <- function(cel1, totc, cel1_ci) {
#     function(brk, sign)
#       paste0("AND(", cel1, sign, totc, ifelse(sign == ">", "+", ""), brk,
#              ", ", cel1_ci, ">0)")
#   } # B2 > $T2 + 0.05 & ci > 0
#   rule_mean_diff_no_ci <- function(cel1, totr, cel1_ci) {
#     function(brk, sign)
#       paste0("AND(", cel1, sign, totr, "*", brk,
#              ", ABS(", cel1, "-", totr, ")-", cel1_ci, ">0)")
#   } # B2 > B$2 * 1.10 & B2 - B$2 - ci > 0
#   rule_mean_diff_no_ci_spread <- function(cel1, totr, cel1_ci) {
#     function(brk, sign)
#       paste0("AND(", cel1, sign, totr, "*", brk,
#              ", ", cel1_ci, ">0)")
#   } # B2 > B$2 * 1.10 & ci > 0
#
#
#   rule_row_pct_diff_after_ci <- function(cel1, totr, cel1_ci) {
#     function(brk, sign)
#       paste0(cel1, sign, totr, ifelse(sign == ">", "+", ""), brk,
#              "-", cel1_ci)
#   } # B2 > B$T + 0.05 - ci
#   rule_row_pct_diff_after_ci_spread <- function(cel1, totr, cel1_ci) {
#     function(brk, sign)
#       paste0("AND(", cel1, "-", totr, sign, "0",
#              ", ", cel1_ci, ">", brk, ")")
#   } # B2 - B$T > 0 & ci > 0.05
#   rule_col_pct_diff_after_ci <- function(cel1, totc, cel1_ci) {
#     function(brk, sign)
#       paste0(cel1, sign, totc, ifelse(sign == ">", "+", ""), brk,
#              "-", cel1_ci)
#   } # B2 > B$T + 0.05 - ci
#   rule_col_pct_diff_after_ci_spread <- function(cel1, totc, cel1_ci) {
#     function(brk, sign)
#       paste0("AND(", cel1, "-", totc, sign, "0",
#              ", ", cel1_ci, ">", brk, ")")
#   } # B2 - $B2 > 0 & ci > 0.05
#   rule_mean_diff_after_ci <- function(cel1, totr, cel1_ci) {
#     function(brk, sign)
#       paste0(cel1, sign, "(", totr, "+", cel1_ci, ")",
#              "*", brk)
#   } # B2 > (B$2 + ci) * 1.10
#   # ci : + or - depending on sign ?
#   rule_mean_diff_after_ci_spread <- function(cel1, totr, cel1_ci) {
#     function(brk, sign)
#       paste0("AND(", cel1, sign, totr,
#              ", ", cel1_ci, ">", brk, ")")
#   } # B2 > $B2 & ci > 0.05
#
#
#   # rule_ctr2 <- function(cel1, ctr) {   # B2 > 2 * nb_cells
#   #   function(pct_breaks, mean_breaks, sign,  contrib_breaks)
#   #     paste0(cel1, sign, contrib_breaks, "/")
#   # }
#   #contrib :
#   # ...nbcell, "-", ...nbcell, "+",
#   # ...cell_contrib, sign2, coeff, "*", .mean_contrib
#
#
#   # rule[trow ] <- purrr::map2(cel1[trow ], totr[trow ], rule_row_pct )
#   # rule[tcol ] <- purrr::map2(cel1[tcol ], totc[tcol ], rule_col_pct )
#   # rule[tmean] <- purrr::map2(cel1[tmean], totr[tmean], rule_mean)
#   # rule[tctr ] <- purrr::map2(cel1[tctr ], ctr [tctr ], rule_ctr )
#   # #rule[tctr2] <- purrr::map2(cel1[tctr2], rule_ctr2)
#
#   rule[trow        ] = purrr::pmap(list(cel1[trow        ], totr[trow        ]                       ), rule_row_pct)
#   rule[trow_no_ci  ] = purrr::pmap(list(cel1[trow_no_ci  ], totr[trow_no_ci  ], cel1_ci[trow_no_ci  ]), rule_row_pct_diff_no_ci)
#   rule[trow_no_cis ] = purrr::pmap(list(cel1[trow_no_cis ], totr[trow_no_cis ], cel1_ci[trow_no_cis ]), rule_row_pct_diff_no_ci_spread)
#   rule[trow_af_ci  ] = purrr::pmap(list(cel1[trow_af_ci  ], totr[trow_af_ci  ], cel1_ci[trow_af_ci  ]), rule_row_pct_diff_after_ci)
#   rule[trow_af_cis ] = purrr::pmap(list(cel1[trow_af_cis ], totr[trow_af_cis ], cel1_ci[trow_af_cis ]), rule_row_pct_diff_after_ci_spread)
#   rule[tcol        ] = purrr::pmap(list(cel1[tcol        ], totc[tcol        ]                       ), rule_col_pct)
#   rule[tcol_no_ci  ] = purrr::pmap(list(cel1[tcol_no_ci  ], totc[tcol_no_ci  ], cel1_ci[tcol_no_ci  ]), rule_col_pct_diff_no_ci)
#   rule[tcol_no_cis ] = purrr::pmap(list(cel1[tcol_no_cis ], totc[tcol_no_cis ], cel1_ci[tcol_no_cis ]), rule_col_pct_diff_no_ci_spread)
#   rule[tcol_af_ci  ] = purrr::pmap(list(cel1[tcol_af_ci  ], totc[tcol_af_ci  ], cel1_ci[tcol_af_ci  ]), rule_col_pct_diff_after_ci)
#   rule[tcol_af_cis ] = purrr::pmap(list(cel1[tcol_af_cis ], totc[tcol_af_cis ], cel1_ci[tcol_af_cis ]), rule_col_pct_diff_after_ci_spread)
#   rule[tmean       ] = purrr::pmap(list(cel1[tmean       ], totr[tmean       ]                       ), rule_mean)
#   rule[tmean_no_ci ] = purrr::pmap(list(cel1[tmean_no_ci ], totr[tmean_no_ci ], cel1_ci[tmean_no_ci ]), rule_mean_diff_no_ci)
#   rule[tmean_no_cis] = purrr::pmap(list(cel1[tmean_no_cis], totr[tmean_no_cis], cel1_ci[tmean_no_cis]), rule_mean_diff_no_ci_spread)
#   rule[tmean_af_ci ] = purrr::pmap(list(cel1[tmean_af_ci ], totr[tmean_af_ci ], cel1_ci[tmean_af_ci ]), rule_mean_diff_after_ci)
#   rule[tmean_af_cis] = purrr::pmap(list(cel1[tmean_af_cis], totr[tmean_af_cis], cel1_ci[tmean_af_cis]), rule_mean_diff_after_ci_spread)
#   rule[tctr        ] = purrr::pmap(list(cel1[tctr        ], ctr [tctr        ]                       ), rule_ctr)
#
#   rule
# }
#
# conditional_fmt_map <- conditional_fmt_map %>%
#   dplyr::mutate(rule_gen = rule_factory(
#     type_ci = type_ci, cel1 = cel1, cel1_ci = cel1_ci, color = color,
#     totc = totc, totr = totr, tott = tott,
#     ctr = spreads)
#   ) %>%
#   dplyr::filter(!is.na(rule_gen))
#
# tibble::tibble(
#   style, sign,
#   pct_breaks, mean_breaks, contrib_breaks,
#   pct_ci_breaks, mean_ci_breaks,
#   pct_brksup, mean_brksup, contrib_brksup,
#   pct_ci_brksup,mean_ci_brksup
# )
#
# # Create conditional formatting rules for each shade of green/orange ;
# # for each one, test if realy used, remove if not
# cfmt_final <- conditional_fmt_map %>%
#   tibble::add_column(styles_df = list(conditional_fmt_styles)) %>%
#   tidyr::unnest(styles_df) %>%
#   dplyr::mutate(
#     brk = dplyr::case_when(
#       color == "diff_after_ci" &
#         type_ci %in% c("row_ci_diff", "col_ci_diff",
#                        "row_ci_spread", "col_ci_spread")
#       ~ pct_ci_breaks,
#       color == "diff_after_ci" &
#         type_ci %in% c("mean_ci_spread", "mean_ci_diff")
#       ~ mean_ci_breaks,
#       type %in% c("row", "col")                    ~ pct_breaks ,
#       type == "mean"                               ~ mean_breaks,
#       type %in% c("all", "all_tabs", "wn", "ctr")  ~ contrib_breaks,
#     ),
#     brksup = dplyr::case_when(
#       color == "diff_after_ci" &
#         type_ci %in% c("row_ci_diff", "col_ci_diff",
#                        "row_ci_spread", "col_ci_spread")
#       ~ pct_ci_brksup,
#       color == "diff_after_ci" &
#         type_ci %in% c("mean_ci_spread", "mean_ci_diff")
#       ~ mean_ci_brksup,
#       type %in% c("row", "col")                    ~ pct_brksup,
#       type == "mean"                               ~ mean_brksup,
#       type %in% c("all", "all_tabs", "wn", "ctr")  ~ contrib_brksup
#     )
#   ) %>%
#   dplyr::select(-pct_breaks, -mean_breaks, -contrib_breaks,
#                 -pct_ci_breaks, -mean_ci_breaks,
#                 -pct_brksup, -mean_brksup, -contrib_brksup,
#                 -pct_ci_brksup, -mean_ci_brksup    ) %>%
#   dplyr::filter(! (is.na(cel1) | is.na(brk) ) # |
#                 #      (type == "row"              & is.na(totr   ) ) |
#                 #      (type %in% c("col", "mean") & is.na(totc   ) ) |
#                 #      (type == "ctr"              & is.na(cells) )   )
#   ) %>%
#   dplyr::mutate(
#     rule_test = purrr::pmap_lgl(
#       list(spreads, brk, brksup, type_ci, sign), function(spread, brk, brksup, type_ci, sign)
#         switch(type_ci,
#                "row"  = ,
#                "col"  = switch(sign,
#                                ">" = any(spread >= brk   & spread < brksup) ,
#                                "<" = any(spread <= -brk  & spread > -brksup)),
#                "mean" = switch(sign,
#                                ">" = any(spread >= brk   & spread < brksup  )     ,
#                                "<" = any(spread <= 1/brk & spread > 1/brksup)),
#                "all"  =,
#                "all_tabs" =,
#                "wn"   =,
#                "ctr"  = switch(sign,
#                                ">" = any((spread >= brk) & (spread < brksup)),
#                                "<" = any((spread <= brk) & (spread > brksup))      ),
#                "row_ci_diff"    = ,
#                "row_ci_spread"  = ,
#                "col_ci_diff"    = ,
#                "col_ci_spread"  = ,
#                "mean_ci_diff"   = ,
#                "mean_ci_spread" = TRUE,
#         )
#     ))
#
# # c("pct_ci_breaks",
# #   "mean_ci_breaks", "pct_ci_brksup", "mean_ci_brksup")
#
#
# #    cfmt_final$brksup[cfmt_final$type %in% c("all", "all_tabs", "wn", "ctr")]
#
# #
# #     cfmt_final[cfmt_final$type %in% c("all", "all_tabs", "wn", "ctr"),] %>%
# #       dplyr::filter(rule_test) %>%
# #       dplyr::mutate(broutte = purrr::map2(brk, brksup, ~ c(.x, .y))) %>%
# #       .$broutte
#
# cfmt_final <- cfmt_final %>%
#   dplyr::filter(rule_test) %>%
#   dplyr::mutate(
#     rule = purrr::pmap_chr(
#       list(rule_gen, brk, sign), function(fn, .brk, .sign)
#         rlang::exec(fn, brk = .brk, sign = .sign)
#     ) )
#
# #Apply all used conditional formatting to the workbook
# cfmt_final %>%
#   dplyr::select(sheet, cols, rows, rule, style) %>%
#   purrr::pwalk(function(sheet, cols, rows, rule, style)
#     openxlsx::conditionalFormatting(
#       wb, sheet = sheet, cols = cols, rows = rows,
#       rule = rule, style = rlang::eval_tidy(rlang::sym(style)))
#   )



