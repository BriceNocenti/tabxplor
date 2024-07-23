

# @rdname jamovi
jmvtabClass <- if (requireNamespace('jmvcore', quietly = TRUE) ) R6::R6Class(
    "jmvtabClass",
    inherit = jmvtabBase,
    #### Active bindings ----
    # active = list(
    #   weights = function() {
    #     if ( ! is.null(self$options$wt)) {
    #       return(self$options$wt)
    #     } else if ( ! is.null(attr(self$data, "jmv-weights-name"))) {
    #       return (attr(self$data, "jmv-weights-name"))
    #     }
    #     NULL
    #   }
    # ),
    private = list(
        # .init = fonction() {
        #
        #   # self$results$table$addRow('var1')
        #   # self$results$table$addRow('var2')
        #   # self$results$table$addRow('interaction')
        #
        #   # if (self$options$pcRow) {
        #   #   freqs$addColumn(
        #   #     name='.total[pcRow]',
        #   #     title=.('Total'),
        #   #     type='number',
        #   #     format='pc')
        #   # }
        #
        #
        # },
        .run = function() {

          wt <- if (! is.null(self$options$wt)) {
            rlang::sym(self$options$wt)
          # } else if (!is.null(attr(self$data, "jmv-weights-name")) ) {
          #   attr(self$data, "jmv-weights-name") |>
          #     paste0(collapse = "") |>
          #     # stringr::str_extract("POND") |>
          #     rlang::sym()
          } else {
            character()
          }

          row_var  <- self$options$row_vars[1]
          col_vars <- self$options$col_vars
          tab_vars <- self$options$tab_vars # tab_vars <- tab_get_vars(tabs)$tab_vars

          tabs <- tab_many(
            data               = self$data,
            row_vars           = all_of(self$options$row_vars),
            col_vars           = all_of(self$options$col_vars),
            tab_vars           = all_of(self$options$tab_vars),
            wt                 = !!wt,
            pct                = self$options$pct,
            color              = self$options$color,
            na                 = self$options$na,
            diff               = self$options$diff,
            comp               = self$options$comp,
            ci                 = self$options$ci,
            conf_level         = self$options$conf_level,
            chi2               = self$options$chi2,
            cleannames         = self$options$cleannames,
            levels             = self$options$lvs,
            totaltab           = self$options$totaltab,

            digits             = self$options$digits,
            rare_to_other      = self$options$other_if_less_than >= 1,
            n_min              = self$options$other_if_less_than,
            subtext            = self$options$subtext
          )

          if (self$options$display != "auto") {
            tabs <- tabs |>
              mutate(across(where(is_fmt), ~ set_display(., self$options$display)))
          }

          tabs <- tabs |>
            tab_wrap_text(wrap_rows = self$options$wrap_rows,
                          wrap_cols = self$options$wrap_cols,
                          exdent = 0)

          # Chi2 table
          chi2 <- tabs |>
            get_chi2() |>
            dplyr::mutate(
              dplyr::across(dplyr::where(is_fmt), format),
              dplyr::across(dplyr::where(is.factor), as.character),
              dplyr::across(
                all_of(col_vars),
                ~ dplyr::if_else(
                  `chi2 stats` == "pvalue",
                  true  = dplyr::if_else(
                    . >= 0.05,
                    true  = paste0('<b><p style = "color:red;margin:0;padding:0;">',
                                   .,
                                   '</p></b>'),
                    false = paste0('<b><p style = "color:green;margin:0;padding:0">',
                                   .,
                                   '</p></b>')
                  ),
                  false = .
                )
              )
            )

          # for (i in (1:ncol(chi2))[names(chi2) != "row_var"] ) {
          #   self$results$chi2_table$addColumn(
          #     name = names(chi2)[i],
          #     index = dplyr::if_else(tidyr::replace_na(names(chi2)[i] %in% tab_vars, FALSE),
          #                            true  = i,
          #                            false = Inf),
          #     combineBelow = tidyr::replace_na(names(chi2)[i] %in% tab_vars, FALSE),
          #     type = "text"
          #   )
          # }

          if (length(tab_vars) > 0 & self$options$comp == "tab") {
            for (i in 1:length(tab_vars)) {
              self$results$chi2_table$addColumn(name = tab_vars[i],
                                                index = i,
                                                combineBelow = TRUE,
                                                type = "text")
            }

          }

          # self$results$chi2_table$addColumn(name = "row_var",
          #                                   type = "text",
          #                                   content = row_var)

          self$results$chi2_table$addColumn(name = "chi2 stats")

          if (length(col_vars) > 0) {
            for (i in 1:length(col_vars)) {
              self$results$chi2_table$addColumn(name = col_vars[i],
                                                type = "text")

            }
          }

          # if (self$options$pcRow) {
          #   freqs$addColumn(
          #     name='.total[pcRow]',
          #     title=.('Total'),
          #     type='number',
          #     format='pc')
          # }


          chi2_new_group <- chi2 |>
            dplyr::group_by(!!!rlang::syms(tab_vars)) |>
            dplyr::group_indices()
          chi2_new_group <-
            which(chi2_new_group != dplyr::lead(chi2_new_group,
                                                default = max(chi2_new_group) + 1))


          for (i in 1:nrow(chi2)) {
            self$results$chi2_table$addRow(rowKey = i, values = as.list(chi2[i, ]) )

            # # formats not working, why ? rowNo = i or i-1 ?
            # if (i %in% chi2_new_group + 1L) {
            #   self$results$chi2_table$addFormat(rowNo = i, col = 1, format = "Cell.BEGIN_GROUP")
            # }
            # # "Cell.BEGIN_GROUP" "Cell.END_GROUP" "Cell.BEGIN_END_GROUP" "Cell.NEGATIVE"

          }





          # table <- self$results$chi2_table
          #
          # for (dep in dplyr::if_else(length(self$options$tab_vars) > 0,
          #                            true  = self$options$tab_vars,
          #                            false = 1L)) {
          #
          #   # formula <- jmvcore::constructFormula(dep, self$options$group)
          #   # formula <- as.formula(formula)
          #   #
          #   # results <- t.test(formula, self$data)
          #
          #   table$setRow(rowKey = dep, values=list(  # set by rowKey!
          #     t =results$statistic,
          #     df=results$parameter,
          #     p =results$p.value
          #   ))
          # }
          #



          # Calculate width and height of plotting zone
          colsnames_wrapped <-
            tibble::tibble(cols = names(tabs)[!names(tabs) %in% c(row_var, tab_vars)]) |>
            tidyr::separate_wider_delim(cols = cols, delim = "\n", names_sep = "",
                                        too_few = "align_start")

          calculated_height <- ((   #nrow(tabs) + 1L +
            (tabs |> # heigth depend on the number of line breaks in each column
               #dplyr::select(tidyselect::where(~ is.character(.) | is.factor(.))) |>
               dplyr::ungroup() |>
               dplyr::select(all_of(row_var)) |>
               dplyr::mutate(dplyr::across(
                 tidyselect::everything(),
                 ~ 1L + stringr::str_count(., "\n")
               )) |>
               colSums() |> max() ) +  #dplyr::summarise(dplyr::across(tidyselect::everything(), sum))

              ncol(colsnames_wrapped) +

              length(get_subtext(tabs)) +

              length(unique(get_color(tabs)[!get_color(tabs) %in% c("", "no")])) # color legend length
          )*20) |>
            round() |> as.integer()

          calculated_width <-
            ((
              max(c(stringr::str_length(
                c(as.character(dplyr::pull(tabs, !!rlang::sym(row_var))),
                  row_var) |>
                  stringr::str_split("\n") |>
                  purrr::flatten_chr()
              ), 5)) +

                colsnames_wrapped |>
                dplyr::mutate(dplyr::across(
                  tidyselect::everything(),
                  ~ stringr::str_length(tidyr::replace_na(., ""))
                )) |>
                dplyr::rowwise() |>
                dplyr::mutate(mx = max(dplyr::c_across(tidyselect::everything())) + 2L) |>
                dplyr::ungroup() |>
                dplyr::pull(mx) |>
                sum()
            ) * 7) |>
            round() |> as.integer()

          # https://forum.jamovi.org/viewtopic.php?t=472
          self$results$plot$setSize(calculated_width, calculated_height)


          image <- self$results$plot
          image$setState(tabs)
        },
        .plot = function(image, ...) {
          plotData <- image$state
          plot <- tab_plot(plotData, wrap_rows = Inf, wrap_cols = Inf)
          print(plot)
          TRUE
        }
        )
)







