

# Fill base fmtn elements while creating the tabs. Fill others using vector attributes for
# rows and cols (digits, type, pct). Save performance and make it easy for tabxl after.
# => Fill digits, type, pct, just before printing (less powerful ?)?

# Not working with list of tabs, but with grouped_df (add empty rows while printing ?).
# => All attributes are on the main level (digits, types, pct are int or char matrixes).

tab_totaltab <- function(tabs, totaltab = c("table", "line", "no")) {
  tabs_syms <- tab_cols_syms(tabs)
  var3_names  <- tabs_syms[[1]]
  var2_name   <- tabs_syms[[2]]
  var1_levels <- tabs_syms[[3]]
  # gr <- dplyr::group_vars(tabs)

  if (length(var3_names) == 0) return(tabs)

  # Attributes to remove totaltab ----
  totaltab <- tabs %>%
    dplyr::group_by(!!var2_name) %>%
    dplyr::summarise(dplyr::across(where(is_fmtn), sum))

  totaltab <-
    purrr::reduce(var3_names, .init = totaltab,
                  .f = ~ dplyr::mutate(.x, !!.y := factor("Ensemble"))) #%>% #English
  #dplyr::relocate(!!!var3_names, .before = 1)

  tabs %>% dplyr::bind_rows(totaltab)
}

tab_cols_syms <- function(tabs) {
  var1_levels <- names(tabs)[purrr::map_lgl(tabs, is_fmtn)] %>% rlang::syms()
  var2_name   <- names(tabs) %>%
    purrr::discard(. %in% var1_levels) %>% .[length(.)] %>% rlang::sym()
  var3_names  <- names(tabs) %>%
    purrr::discard(. %in% c(var1_levels, var2_name)) %>% rlang::syms()

  list(var3_names, var2_name, var1_levels)
}

tab_tot <- function(tabs,
                    tot = c("row", "col") #, fields = c("num", "type", "digits", "n", "wn")
                    ) {
  # stopifnot(all(fields %in% c("num", "type", "digits", "n", "wn")),
  #           length(fields) > 0)
  tabs_syms <- tab_cols_syms(tabs)
  var3_names  <- tabs_syms[[1]]
  var2_name   <- tabs_syms[[2]]
  var1_levels <- tabs_syms[[3]]
  gr <- dplyr::group_vars(tabs)

  # Test and manage grouping variables ----

  if ("row" %in% tot | is.null(tot)) {
    tabs <- tabs %>%
      dplyr::filter_at(2, ~ ! stringr::str_detect(., stringr::regex("^total",
                                                                    ignore_case = TRUE)))
  }

  if ("col" %in% tot | is.null(tot)) {
    #previous_totcol <- tabs %>% dplyr::select(1, tidyselect::starts_with("total"))
    tabs <- tabs %>% dplyr::select(- tidyselect::starts_with("total"))
  }

  if ("row" %in% tot){ #Consider as a total line any var1 beginning by "total"

    if (length(gr) != 0) {
      var3_totals <- dplyr::group_keys(tabs) %>% #dplyr::mutate(bis = PR0) %>%
        tidyr::unite(!!var2_name, sep = "/") %>%
        dplyr::mutate(!!var2_name := paste("Total", !!var2_name) %>%
                        stringr::str_to_upper() %>% as.factor())  #stringr::str_remove_all()
    } else {
      var3_totals <- tibble::tibble(!!var2_name := factor("Total"))
      var3_totals_levels <- var3_totals %>% dplyr::pull(1) %>% levels()
      }

    tabs <- tabs %>%
        dplyr::mutate(!!var2_name := forcats::fct_expand(!!var2_name, var3_totals_levels))

    var2_levels <- dplyr::pull(tabs, !!var2_name) %>% levels()

    totrows <-
      dplyr::summarise(tabs, dplyr::across(where(is_fmtn), sum)) %>%
        dplyr::bind_cols(var3_totals) %>%
        dplyr::mutate(!!var2_name := forcats::fct_expand(!!var2_name, var2_levels)) #%>%
        #dplyr::relocate(!!var2_name, .after = gr[length(gr)])

    tabs <-
      tabs %>%
      dplyr::with_groups(NULL, dplyr::bind_rows, totrows) %>%
      dplyr::arrange(.by_group = TRUE)


    # tabs_num <- tabs %>% dplyr::select_if(is_fmtn)
    # previous_totrow <- tabs %>%
    #   dplyr::filter_at(2, ~ stringr::str_detect(., stringr::regex("^total", ignore_case = TRUE)))
    # var2_levels <- levels(dplyr::pull(tabs, 2))
    # var3_levels <- levels(dplyr::pull(tabs, 1))
    # if (nrow(previous_totrow) == 0) { #Not working if more than one grouping variable.
    #   empty_nums <- rep(list(new_fmtn(NA_real_)), length(tabs) - 2) %>%
    #     purrr::set_names(names()) %>%
    #     tibble::as_tibble()
    #   #previous_totrow <-
    #   dplyr::group_keys(tabs) %>% dplyr::mutate(var2 = factor("Total", var2_levels)) %>%
    #     dplyr::bind_cols(empty_nums) %>%
    #     dplyr::group_by(!!!rlang::syms(dplyr::group_vars(tabs)))
    #
    #}
  }

  if ("col" %in% tot) {
    tabs <- tabs %>%
      dplyr::rowwise() %>%  # c_across don’t work : good workaround with quosures
      dplyr::mutate(Total = sum(!!!var1_levels))
  }

  tabs %>% dplyr::group_by(!!!var3_names)



  # #Prepare the data by extracting the needed elements of formatted numbers
  # numbers <- tabs %>% dplyr::group_split() %>%
  #   purrr::map(~ dplyr::select_if(., is_fmtn)) #%>%
  # #purrr::set_names(dplyr::group_keys(tabs) %>% tibble::deframe())
  # if ("num" %in% fields) num <-
  #   purrr::map(numbers, ~ purrr::modify(., ~ field(., "num")))
  # if ("type" %in% fields) {
  #   type <- purrr::map(numbers, ~ purrr::modify(., ~ field(., "type")))
  #   type_n    <- purrr::map(type, ~dplyr::summarise_all(., ~ . == "n"   ))
  #   type_pct  <- purrr::map(type, ~dplyr::summarise_all(., ~ . == "pct" ))
  #   type_mean <- purrr::map(type, ~dplyr::summarise_all(., ~ . == "mean"))
  # }
  # if ("digits" %in% fields) digits <-
  #   purrr::map(numbers, ~ purrr::modify(., ~ field(., "digits")))
  # if ("n"  %in% fields) ns  <- purrr::map(numbers, ~ purrr::modify(., ~ field(., "n" )))
  # if ("wn" %in% fields) wn  <- purrr::map(numbers, ~ purrr::modify(., ~ field(., "wn")))


  # #Total rows
  # if ("row" %in% tot) {
  #   if ("num" %in% fields) r_num <- purrr::map(num, ~ dplyr::summarise_all(., ~ sum(.)) )
  #
  #   if ("type" %in% fields) {
  #     r_type_n <-
  #       purrr::map(type_n, ~ dplyr::summarise_all(., ~ all(.), .groups = "drop") %>%
  #                    dplyr::mutate_all(~ dplyr::if_else(., "n", "")))
  #     r_type_pct <-
  #       purrr::map(type_pct, ~ dplyr::summarise_all(., ~ all(.), .groups = "drop") %>%
  #                    dplyr::mutate_all(~ dplyr::if_else(., "pct", "")))
  #     r_type_mean <-
  #       purrr::map(type_mean, ~ dplyr::summarise_all(., ~ all(.), .groups = "drop") %>%
  #                    dplyr::mutate_all(~ dplyr::if_else(., "mean", "")))
  #     r_type <-
  #       purrr::pmap(list(r_type_n, r_type_pct, r_type_mean),
  #                   ~ dplyr::bind_rows(..1, ..2, ..3) %>%
  #                     dplyr::summarise_all(~ paste0(., collapse = ""), .groups = "drop") )
  #     if( purrr::map(r_type, ~ . == "") %>% purrr::flatten_lgl() %>% any() ) {
  #       warning("Columns with formatted numbers of different types (n, pct, mean).") # To NA ?
  #     }
  #   }
  #
  #   if ("digits" %in% fields) r_digits <-
  #       purrr::map(digits, ~ dplyr::summarise_all(., ~ max(.), .groups = "drop") )
  #   if ("n"  %in% fields) r_ns  <- purrr::map(ns, ~ dplyr::summarise_all(., ~ sum(.)))
  #   if ("wn" %in% fields) r_wn  <- purrr::map(wn, ~ dplyr::summarise_all(., ~ sum(.)))
  #
  #
  #   # if (nrow(previous_totrow) == 0) {
  #   #   #total_row <-
  #   #   rlang::syms(paste0("r_", fields) %>% stringr::str_replace("^r_n$", "r_ns")) %>%
  #   #     purrr::set_names(fields) %>% purrr::map(rlang::eval_tidy) %>%
  #   #     purrr::pmap(
  #   #       ~ dplyr::mutate_all(..1,
  #   #                           ~ new_fmtn(num = ..1, type = ..2, digits = ..3,
  #   #                                      n = ..4, wn = ..5 ))) #%>%
  #   #
  #   #   purrr::set_names(dplyr::group_keys(tabs) %>% tibble::deframe()) %>%
  #   #     purrr::imap(
  #   #       ~ tibble::add_column(.x, !!rlang::sym(names(tabs)[2]) :=
  #   #                              factor("Total"), .before = 1) %>%
  #   #         tibble::add_column(!!rlang::sym(names(tabs)[1]) :=
  #   #                              factor(.y, var3_levels), .before = 1) %>%
  #   #         dplyr::mutate_at(2, ~ forcats::fct_expand(., var2_levels) %>%
  #   #                            forcats::fct_relevel(var2_levels))
  #   #     )
  #   #
  #   #
  #   #     rlang::syms(paste0("r_", fields) %>% stringr::str_replace("^r_n$", "r_ns")) %>%
  #   #       purrr::set_names(fields) %>% purrr::map(rlang::eval_tidy) %>%
  #   #       purrr::map_depth(3, class)
  #   #
  #   #
  #   #
  #   # } else {
  #
  #   #previous_totrow_names <- previous_totrow %>% dplyr::select(1, 2) %>% dplyr::group_split()
  #
  #   total_row <-
  #     purrr::reduce2(
  #       fields,
  #       rlang::syms(paste0("r_", fields) %>% stringr::str_replace("^r_n$", "r_ns")),
  #       .init = previous_totrow %>%
  #         purrr::map(., ~ dplyr::select(., -1, -2)),
  #       .f =
  #         ~ purrr::map2(..1, rlang::eval_tidy(..3),
  #                       function(.prev, .change)
  #                         purrr::modify2(.prev, .change, ~ `field<-`(.x, "wn", .y))
  #         )
  #     ) #%>% .[[1]] %>% dplyr::pull(1) %>% vec_data()
  #
  #   total_row <-
  #     purrr::map2(previous_totrow_names, total_row, ~ dplyr::bind_cols(.x, .y))
  #
  #   #}
  # }



  # #Total columns
  # if ("col" %in% tot) {
  #   if ("num" %in% fields) c_num <- purrr::map(num, ~ rowSums(.) %>% as.double())
  #
  #   if ("type" %in% fields) {
  #   c_type_n <- purrr::map(type_n, ~ dplyr::rowwise(.) %>%
  #                            dplyr::summarise(n    = all(dplyr::c_across()), .groups = "drop") %>%
  #                            dplyr::transmute(n    = dplyr::if_else(n, "n", "")))
  #   c_type_pct <- purrr::map(type_pct, ~ dplyr::rowwise(.) %>%
  #     dplyr::summarise(pct  = all(dplyr::c_across()), .groups = "drop")  %>%
  #     dplyr::transmute(pct  = dplyr::if_else(pct, "pct", "")))
  #   c_type_mean <- purrr::map(type_mean, ~ dplyr::rowwise(.) %>%
  #     dplyr::summarise(mean = all(dplyr::c_across()), .groups = "drop")  %>%
  #     dplyr::transmute(mean = dplyr::if_else(mean, "mean", "")))
  #   c_type <- purrr::pmap(list(c_type_n, c_type_pct, c_type_mean),
  #                         ~ dplyr::bind_cols(..1, ..2, ..3) %>% dplyr::rowwise() %>%
  #                           dplyr::summarise(type = paste0(dplyr::c_across(), collapse = ""), .groups = "drop") %>%
  #                           dplyr::pull(type))
  #   if (any(purrr::flatten_chr(c_type) == "")) {
  #     warning("Rows with formatted numbers of different types (n, pct, mean).") # To NA ?
  #   }
  #   }
  #
  #   if ("digits" %in% fields) c_digits <-
  #       purrr::map(digits, ~ dplyr::rowwise(.) %>%
  #                    dplyr::summarise(digits = max(dplyr::c_across()), .groups = "drop") %>%
  #                    dplyr::pull(digits))
  #   if ("n"  %in% fields) c_ns  <- purrr::map(ns, ~ rowSums(.) %>% as.integer())
  #   if ("wn" %in% fields) c_wn  <- purrr::map(wn, ~ rowSums(.) %>% as.double())
  #
  #   if (! "Total" %in% names(previous_totcol)) {
  #     total_col <-
  #       ls(pattern = "^c_num$|^c_type$|^c_digits$|^c_ns$|^c_wn$") %>% as.list() %>%
  #       purrr::set_names(stringr::str_remove(., "^c_") %>% stringr::str_replace("^ns$", "n")) %>%
  #       purrr::map(~ rlang::sym(.) %>% rlang::eval_tidy()) %>%   #%>% tibble::tibble(All = .) %>%
  #       tibble::as_tibble() %>%
  #       purrr::pmap(new_fmtn) %>%
  #       purrr::map(~ tibble::tibble(Total = .))
  #   } else {
  #     # previous_totcol %>% dplyr::group_split(.keep = FALSE) %>%
  #     #   purrr::map2(c_wn, ~ dplyr::mutate(.x, Total = `field<-`(Total, "wn", .y))) %>% .[[1]] %>%
  #     #   dplyr::pull(Total) %>% vec_data()
  #     total_col <-
  #       purrr::reduce2(
  #         fields,
  #         rlang::syms(paste0("c_", fields) %>% stringr::str_replace("^c_n$", "c_ns")),
  #         .init = dplyr::group_split(previous_totcol,.keep = FALSE),
  #         .f = ~ purrr::map2(..1, rlang::eval_tidy(..3),
  #                            function(.data, .col)
  #                              dplyr::mutate(.data, Total = `field<-`(Total, ..2, .col)))
  #       )
  #   }
  # }

  # tabs <- dplyr::group_split(tabs)
  #
  # if ("col" %in% tot) tabs <- tabs %>%
  #   purrr::map2(total_col, ~ dplyr::bind_cols(.x, .y))
  #
  # if ("row" %in% tot) tabs <- tabs %>%
  #   purrr::map2(total_row, ~ dplyr::bind_rows(.x, .y))
  #
  # tabs %>%
  #   dplyr::bind_rows() %>%
  #   dplyr::group_by(!!rlang::sym(names(.)[1]))
}


# tab_num_to_n <- function(tabs) {
#   tabs %>%
#     dplyr::mutate(dplyr::across(where(is_fmtn),
#                                 ~ `field<-`(., "n", as.integer(field(., "num")))))
# }
#
# tab_n_to_num <- function(tabs) {
#   tabs %>%
#     dplyr::mutate(dplyr::across(where(is_fmtn),
#                                 ~ `field<-`(., "num", as.double(field(., "n")))))
# }

pct_formula_gen <- function(pct) {
  switch(pct,
         "row"     = function(x, tot) field(x, "wn") / field(tot             , "wn"),
         "col"     = function(x, tot) field(x, "wn") / field(dplyr::last(x)  , "wn"),
         "all"     = function(x, tot) field(x, "wn") / field(dplyr::last(tot), "wn"),
         "all_tabs"= function(x, tot) field(x, "wn") / field(dplyr::last(tot), "wn"),
         "no"      = function(x, tot) field(x, "wn"))
}

tab_pct <- function(tabs, pct = c("row", "col", "all", "all_tabs", "no")) { #Add keep/change grouping ?
  #stopifnot(pct[1] %in% c("row", "col", "all", "all_tabs", "no"))

  # all_tabs : need no test if there is a Total tab ----
  # detect if there are totals, calculate otherwise ----

  if (pct[1] == "all_tabs") {
    tabs <- tabs %>% dplyr::ungroup()

  } else if (pct[1] %in% c("col", "all")) {
    var1_levels <- names(tabs)[purrr::map_lgl(tabs, is_fmtn)]
    var3_names  <- names(tabs) %>% purrr::discard(. %in% var1_levels) %>% .[-length(.)]
    gr <- dplyr::group_vars(tabs)

    if (length(gr) == 0) {
      tabs <- tabs %>% dplyr::group_by(!!!rlang::syms(var3_names))
    } else if  (gr != var3_names) {
      tabs <- tabs %>% dplyr::group_by(!!!rlang::syms(var3_names))
    }
  }

  pct_formula <- pct_formula_gen(pct[1])

  tabs %>%
    dplyr::mutate(dplyr::across(
      where(is_fmtn),
      ~ `field<-`(., "num", pct_formula(., .data$Total)) %>%
        `field<-`("type", rep(dplyr::if_else(pct[1] != "no", "pct", "n"), length(.)))
    ))
}

tab_ci <- function(tabs,
                   ci = c("row", "col", "abs", "totaltab", "r_to_r", "c_to_c", "tab_to_tab", "no"),
                   confidence_level = 0.95) {
  stopifnot(confidence_level >= 0, confidence_level <=1)
  # Test type : pct, mean ----
  # Test if there is totals, pct, totaltab ----


  # Calculate the z-score for the given confidence level :
  #  https://datascience.stackexchange.com/questions/10093/how-to-find-a-confidence-level-given-the-z-value
  # Thanks to mindcrime:
  zscore <- qnorm((1 - confidence_level)/2,lower.tail = FALSE)

  tabs %>%
    dplyr::mutate(across(where(is_fmtn),
                         ~ field(., "num")
    ))


}

is_pct <- function(fmtn) {
  stopifnot(is_fmtn(fmtn))

  field(fmtn, "type") == "pct"
}

is_n <- function(fmtn) {
  stopifnot(is_fmtn(fmtn))

  field(fmtn, "type") == "n"
}

is_mean <- function(fmtn) {
  stopifnot(is_fmtn(fmtn))

  field(fmtn, "type") == "mean"
}

