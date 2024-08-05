

# @rdname jamovi
jmvtabClass <- if (requireNamespace('jmvcore', quietly = TRUE) ) R6::R6Class(
    "jmvtabClass",
    inherit = jmvtabBase,
    # ### Active bindings ----
    # active = list(
    #   wt = function() {
    #     if (!is.null(self$options$wt)) {
    #       return(self$options$wt)
    #     } else if ( ! is.null(attr(self$data, "jmv-weights-name"))) {
    #       return (attr(self$data, "jmv-weights-name"))
    #     }
    #     NULL
    #   }
    # ),
    private = list(
        .run = function() {

          data <- self$data

          # Note : self$data only contains the selected variables,
          #  but not wt if it was given in Jamovi with Data >>> Weights) :
          #  it needs to be added manually
          if (!is.null(self$options$wt)) {
            wt <- rlang::sym(self$options$wt)
          } else if (!is.null(attr(data, "jmv-weights"))) {
            data[['.COUNTS']] <- jmvcore::toNumeric(attr(data, "jmv-weights"))
            wt <- rlang::sym(".COUNTS")
          } else {
            wt <- character()
          }

          # weightsNotice <- jmvcore::Notice$new(
          #   self$options,
          #   name='.wt',
          #   #type=type,
          #   content = paste0(purrr::map(attributes(self$data), ~ as.character(.[1:10])) |> purrr::flatten_chr(),
          #                    collapse = "<br><br>" )
          #   #   paste0(
          #   #   #"names = " , names(self$data), collapse = ", "
          #   #   #"weight variable is ", self$wt, "", class(self$wt) #,
          #   #                  #"jmv-weights:", attr(self$data, "jmv-weights-name"),
          #   #                  #class(attr(self$data, "jmv-weights-name"))
          #   # )
          #     )
          # self$results$insert(1, weightsNotice)

          row_var  <- self$options$row_vars[1]
          col_vars <- self$options$col_vars
          tab_vars <- self$options$tab_vars # tab_vars <- tab_get_vars(tabs)$tab_vars

          tabs <- tab_many(
            data               = data,
            row_vars           = all_of(self$options$row_vars),
            col_vars           = all_of(self$options$col_vars),
            tab_vars           = all_of(self$options$tab_vars),
            wt                 = !!wt,
            pct                = self$options$pct,
            color              = self$options$color,
            na                 = self$options$na,
            ref                = self$options$ref,
            ref2               = self$options$ref2,
            comp               = self$options$comp,
            ci                 = self$options$ci,
            conf_level         = self$options$conf_level,
            chi2               = self$options$chi2,
            cleannames         = self$options$cleannames,
            levels             = self$options$lvs,
            totaltab           = self$options$totaltab,

            digits             = self$options$digits,
            other_if_less_than = self$options$other_if_less_than,
            subtext            = self$options$subtext
          )

          if (self$options$display != "auto") {
            tabs <- tabs |>
              mutate(across(where(is_fmt), ~ set_display(., self$options$display)))
          }
          if (self$options$ci == "cell" & self$options$pct %in% c("row", "col")) {
            tabs <- tabs |>
              mutate(across(where(is_fmt), ~ set_display(., "pct_ci")))
          }




          ci_print_option <- getOption('tabxplor.ci_print')

          if (self$options$ci_print == "moe") {
            options('tabxplor.ci_print' = "moe")
          } else{ # if (self$options$ci_print == "ci")x
            options('tabxplor.ci_print' = "ci")
          }

          tabs_hmtl <- tab_kable(tabs,
                                 wrap_rows = self$options$wrap_rows,
                                 wrap_cols = self$options$wrap_cols)


          # Formatting not working with kableExtra : we remove "kableExtra" class
          #   and add lightable css and custom css manually
          class(tabs_hmtl) <-  "knitr_kable"

          tabs_hmtl <-
            paste0(
              # Add css manually
              htmltools::includeCSS(# lightable css
                system.file("lightable-0.0.1/lightable.css", package = "kableExtra")
              ),

              htmltools::includeCSS(# bootstrap css
                system.file("rmd/h/bootstrap/css/cosmo.min.css", package = "rmarkdown")
              ),

              # # already done in tab_kable()
              # htmltools::includeCSS(# custom css
              #   system.file("tab.css", package = "tabxplor")
              # ),


              # "<style>\n",
              # paste0(readLines( # lightable css
              #   file.path(kableExtra::html_dependency_lightable()$src$file,
              #             kableExtra::html_dependency_lightable()$stylesheet)
              # ),
              # collapse = "\n"),
              #
              # paste0(readLines( # bootstrap css      # ADD bslib::bs_theme() ?
              #   system.file("rmd/h/bootstrap/css/cosmo.min.css", package = "rmarkdown"),
              #   # file.path(rmarkdown::html_dependency_bootstrap(theme = "cosmo")$src$file,
              #   #           rmarkdown::html_dependency_bootstrap(theme = "cosmo")$stylesheet)
              #   ),
              #   collapse = "\n"
              # ),
              #
              # paste0( # custom css
              #   readLines(system.file("tab.css", package = "tabxplor")),
              #   collapse = "\n"),
              #
              # "</style>\n\n",


              # # Add javascripts manually (n  need, all are already in Jamovi)
              #
              # # htmltools::includeScript( # jquery # no seem to need it, already in Jamovi
              # #   system.file("lib/3.6.0/jquery-3.6.0.min.js", package = "jquerylib")
              # # ),
              #
              # htmltools::includeScript(# bootstrap jv
              #   system.file("rmd/h/bootstrap/js/bootstrap.min.js", package = "rmarkdown"),
              # ),
              # # htmltools::includeScript(# bootstrap jv ; only IE 8
              # #   system.file("rmd/h/bootstrap/shim/html5shiv.min.js", package = "rmarkdown"),
              # # ),
              # # htmltools::includeScript(# bootstrap jv ; only IE 8
              # #   system.file("rmd/h/bootstrap/shim/respond.min.js", package = "rmarkdown"),
              # # ),
              #
              # htmltools::includeScript(# html_dependency_kePrint (popover)
              #   system.file("kePrint-0.0.1/kePrint.js", package = "kableExtra")
              # ),

              # "<script>\n",
              # paste0(
              #   file.path( # bootstrap jv
              #     rmarkdown::html_dependency_bootstrap(theme = "cosmo")$src$file,
              #     rmarkdown::html_dependency_bootstrap(theme = "cosmo")$script
              #   ) |>
              #     purrr::map_chr(~ paste0(readLines(., warn = FALSE), collapse = "\n")),
              #   collapse = "\n"
              # ),
              #
              # paste0(readLines(
              #   file.path( kableExtra::html_dependency_kePrint()$src$file,
              #              kableExtra::html_dependency_kePrint()$script  )
              # ),
              # collapse = "\n"),
              #
              # paste0(readLines( # jquery
              #   file.path(
              #     rmarkdown::html_dependency_jquery()$src$file,
              #             rmarkdown::html_dependency_jquery()$script)
              #   ),
              #   collapse = "\n"),
              #
              # "</script>\n\n",

              "<script type=\"text/x-mathjax-config\">MathJax.Hub.Config({tex2jax: {inlineMath: [[\"$\",\"$\"]]}})</script>",
              "<script async src=\"https://mathjax.rstudio.com/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML\"></script>",

              as.character(tabs_hmtl)
            ) |>
            vctrs::vec_restore(tabs_hmtl)
          # tabs_hmtl |> htmltools::HTML() |> htmltools::browsable()

          # what is still missing ? tabs_hmtl |> attr("kable_meta") ?





          options('tabxplor.ci_print' = ci_print_option)


          # tabs_hmtl <- tabs |>
          #   knitr::kable(format = "html") |>
          #   kableExtra::kable_classic(lightable_options = "hover") |> # bootstrap_options = c("striped", "responsive")
          #   kableExtra::add_footnote("This should be a very small footnote (font-size: 30%).",
          #                            notation = "none", escape = FALSE)
          #
          #
          # dep <- list(rmarkdown::html_dependency_jquery(),
          #             rmarkdown::html_dependency_bootstrap(theme = "cosmo"),
          #             kableExtra::html_dependency_kePrint(),
          #             kableExtra::html_dependency_lightable(),
          #
          #             htmltools::htmlDependency(
          #               name = "tab_css",
          #               version = "1.00",
          #               src = "inst",
          #               meta = NULL,
          #               script = NULL,
          #               stylesheet = "tab.css",
          #               head = NULL,
          #               attachment = NULL,
          #               package = "tabxplor",
          #               all_files = FALSE
          #             )
          #
          # )
          # tabs_hmtl <- htmltools::browsable(
          #   htmltools::HTML(
          #     as.character(tabs_hmtl),
          #     "<script type=\"text/x-mathjax-config\">MathJax.Hub.Config({tex2jax: {inlineMath: [[\"$\",\"$\"]]}})</script>
          #     <script async src=\"https://mathjax.rstudio.com/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML\"></script>"
          #   )
          # )
          #
          # htmltools::htmlDependencies(tabs_hmtl) <- dep
          # #htmltools::attachDependencies(tabs_hmtl, dep)

          # if (interactive()) tabs_hmtl <- htmltools::browsable(tabs_hmtl)

          # # Error in the jmvcore::analysis$results$html_table$setContent function,
          # #  trying to overwrite it.
          # # ERROR : "cannot change value of locked binding"
          # self$results$html_table$setContent <- function(value) {
          #
          #     knitted <- knitr::knit(text=value)
          #
          #     knitMeta <- attr(value, 'html_dependencies') # value, no knitted, got attributes
          #     if ( ! is.null(knitMeta)) {
          #       knitMeta <- knitMeta[[1]]
          #
          #       package  <- self$analysis$package
          #
          #       srcPath  <- normalizePath(knitMeta$src$file)
          #       rootPath <- normalizePath(system.file(package=package))
          #       relPath  <- substring(srcPath, nchar(rootPath)+1)
          #
          #       joinPaths <- function(path) {
          #         if (identical(relPath, ''))
          #           return(path)
          #         file.path(relPath, path)
          #       }
          #
          #       scripts <- sapply(knitMeta$script,     joinPaths, USE.NAMES=FALSE)
          #       sss     <- sapply(knitMeta$stylesheet, joinPaths, USE.NAMES=FALSE)
          #
          #       private$.scripts <- scripts
          #       private$.stylesheets <- sss
          #     }
          #
          #   attributes(knitMeta) <- NULL
          #   private$.content <- knitted
          #   private$.stale <- FALSE
          # }

          self$results$html_table$setContent(tabs_hmtl)



          # Chi2 table
          chi2_tab <- tabs |> get_chi2()
          fmtnm <- purrr::map_lgl(tabs, ~ is_fmt(.) & get_type(.) != "mean")
          col_vars_text  <- get_col_var(tabs[fmtnm]) %>% purrr::discard(is.na(.))

          if (!is.null(chi2_tab) & !length(col_vars_text) == 0) {
            if (nrow(chi2_tab) > 0) {

              chi2_tab <- chi2_tab |>
                dplyr::mutate(
                  dplyr::across(dplyr::where(is_fmt), format),
                  dplyr::across(dplyr::where(is.factor), as.character),
                  dplyr::across(
                    all_of(col_vars_text),
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

              if (length(tab_vars) > 0) {
                if(self$options$comp == "tab") {
                  for (i in 1:length(tab_vars)) {
                    self$results$chi2_table$addColumn(name = tab_vars[i],
                                                      index = i,
                                                      combineBelow = TRUE,
                                                      type = "text")
                  }

                } else { # If comp == "all"
                  cross <- paste0(" ", stringi::stri_unescape_unicode("\\u00d7"), " ") # * as cross
                  chi2_tab <- chi2_tab |>
                    dplyr::mutate(row_var = paste0(dplyr::first(.data$row_var),
                                                   cross,
                                                   paste0(tab_vars, collapse = cross)
                    )
                    )
                }
              }

              # self$results$chi2_table$addColumn(name = "row_var",
              #                                   type = "text",
              #                                   content = row_var)

              self$results$chi2_table$addColumn(name = "chi2 stats")

              if (length(col_vars_text) > 0) {
                for (i in 1:length(col_vars_text)) {
                  self$results$chi2_table$addColumn(name = col_vars_text[i],
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

              # if (length(tab_vars) == 0 | self$options$comp == "tab") {
              #   chi2_new_group <- chi2 |>
              #     dplyr::group_by(!!!rlang::syms(tab_vars)) |>
              #     dplyr::group_indices()
              #   chi2_new_group <-
              #     which(chi2_new_group != dplyr::lead(chi2_new_group,
              #                                         default = max(chi2_new_group) + 1))
              #
              # } else { # If length(tab_vars) > 0 & self$options$comp == "all"
              #   chi2_new_group <-  rep(1, nrow(chi2))
              #   chi2_new_group <-
              #     which(chi2_new_group != dplyr::lead(chi2_new_group,
              #                                         default = max(chi2_new_group) + 1))
              # }



              for (i in 1:nrow(chi2_tab)) {
                self$results$chi2_table$addRow(rowKey = i, values = as.list(chi2_tab[i, ]) )

                # # formats not working, why ? rowNo = i or i-1 ?
                # if (i %in% chi2_new_group + 1L) {
                #   self$results$chi2_table$addFormat(rowNo = i, col = 1, format = "Cell.BEGIN_GROUP")
                # }
                # # "Cell.BEGIN_GROUP" "Cell.END_GROUP" "Cell.BEGIN_END_GROUP" "Cell.NEGATIVE"

              }


            }
            #NULL
          } #else {
          #  NULL
          #}






          # Calculate size of the plotting zone
          wrapped_dims <- tab_get_wrapped_dimensions(tabs, no_tab_vars = TRUE, width_pad = 4L)

          width <- round(wrapped_dims[1]*7) |> as.integer()

          height <- ((wrapped_dims[2] #+
                      #length(get_subtext(tabs)) +
                      #1 # color legend length
          )*20) |> # 20
            round() |> as.integer()


          # # Plot size
          # # https://forum.jamovi.org/viewtopic.php?t=472
          # self$results$plot$setSize(width, height)
          if (width > 1080) {
            self$results$plot$setSize(width, 0) # empty plot
          }



          # # Plot legend

          # tab_legend <- tab_color_legend(tabs,
          #                                mode = "html",
          #                                html_type  = getOption("tabxplor.color_style_type"),
          #                                html_theme = "light",
          #                                html_24_bit = getOption("tabxplor.color_html_24_bit"),
          #                                text_color = "#000000",
          #                  grey_color = "#888888"
          # ) #|>
          #   #stringr::str_replace_all("<span", "<p") |>
          #   #stringr::str_replace_all("</span", "</p")
          #
          # #get_subtext(tabs)
          #
          #
          # self$results$plot_legend$setContent(tab_legend)

          # Plot
          #image <- self$results$plot
          #image$setState(tabs)
        },
        .plot = function(image, ...) {
          # plotData <- image$state
          # plot <- tab_plot(plotData, wrap_rows = Inf, wrap_cols = Inf, color_legend = FALSE)
          # print(plot)
          TRUE
        }
    )
)







