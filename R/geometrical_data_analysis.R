

# Geometrical data analysis AC/ACM----------------------------------------------

#Fonction : un thème ggplot2 commun pour habiller les graphes des ACM
#' A ggplot2 Theme for Geometrical Data Analysis
#'
#' @param res An object created with \code{FactoMineR::\link[FactoMineR]{MCA}},
#' \code{\link[FactoMineR]{CA}}, etc.
#' @param axes The axes to print, as a numeric vector of length 2.
#' @param legend.position c("none", "left", "right", "bottom", "top")
#' @param no_color_scale When TRUE, color_scale will be provided next.
#' @param size_scale_max Size of the points.
#' @param xlim Horizontal axe limits.
#' @param ylim Vertical axe limitss.
#'
#' @return A list of ggplot2 objects.
#'
#' @keywords internal
theme_mca <- function(res = res.mca, axes = c(1,2), legend.position = c("none", "left", "right", "bottom", "top"),
                      no_color_scale = FALSE, size_scale_max = 8, xlim, ylim) {  #no_size_scale = FALSE
  res <- res
  if (exists("axes_names", where = res)) {
    first_axe_title  <- stringr::str_c("Axe ", axes[1]," (", round(res$eig[axes[1],2], 1),
                                       "%) : ", res$axes_names[axes[1]])
    second_axe_title <- stringr::str_c("Axe ", axes[2]," (", round(res$eig[axes[2],2], 1),
                                       "%) : ", res$axes_names[axes[2]])
  } else {
    first_axe_title  <- stringr::str_c("Axe ", axes[1]," (",
                                       round(res$eig[axes[1],2], 1), "%)")
    second_axe_title <- stringr::str_c("Axe ", axes[2]," (",
                                       round(res$eig[axes[2],2], 1), "%)")
  }

  if (no_color_scale == FALSE) {
    scale_color_acm <- ggplot2::scale_color_brewer(palette = "Dark2")
    scale_fill_acm <- ggplot2::scale_fill_brewer(palette = "Dark2")
  } else {
    scale_color_acm <- NULL
    scale_fill_acm <- NULL
  }

  if (!missing(xlim) & !missing(ylim)) {coord_graph <- ggplot2::coord_fixed(xlim = xlim, ylim = ylim) }
  else if (!missing(xlim) ) { coord_graph <- ggplot2::coord_fixed(xlim = xlim ) }
  else if (!missing(ylim) ) { coord_graph <- ggplot2::coord_fixed(ylim = ylim ) }
  else { coord_graph <- ggplot2::coord_fixed() }

  #if (no_size_scale == FALSE) {
  scale_size <- ggplot2::scale_size_area(max_size = size_scale_max) #Échelle de taille des points
  #} else {
  #  scale_size <- NULL
  #}
  list(
    ggplot2::geom_hline(yintercept = 0, color="black", linetype = "dashed"), #Axe horizontal
    ggplot2::geom_vline(xintercept = 0, color="black", linetype = "dashed"), #Axe vertical
    ggplot2::labs(x = first_axe_title, y =  second_axe_title),
    scale_size,
    scale_color_acm, #Palette de couleurs
    scale_fill_acm,
    coord_graph, #Assurer que les proportions relatives des deux axes sont respectées
    ggplot2::theme_minimal(),
    ggplot2::theme(legend.position = legend.position[1],
                   panel.grid.minor = ggplot2::element_blank(), #element_line(size = 0.05, color="gray96"),
                   panel.grid.major = ggplot2::element_blank(), #element_line(size = 0.05, color="gray96"),
                   strip.text = ggplot2::element_text(face = "bold"), #Titre des facets
                   plot.title = ggplot2::element_text(hjust = 0.5, face = "bold"), #Centrer le titre du graphique
                   axis.title.x = ggplot2::element_text(size = 12, hjust = 1),
                   axis.title.y = ggplot2::element_text(size = 12, hjust = 1) )
  )
}

# theme_ac <- function(axes = c(1,2), res = res.ca) {
#   res.ca <- res.ca
#   list(
#     geom_hline(yintercept = 0, color="black", linetype = "dashed"), #Axe horizontal
#     geom_vline(xintercept = 0, color="black", linetype = "dashed"), #Axe vertical
#     labs(x = paste0("Axe ", axes[1]," (", round(res.ca$eig[axes[1],2], 1), "%)"), # Titres des axes
#          y = paste0("Axe ", axes[2]," (", round(res.ca$eig[axes[2],2], 1), "%)") ),
#     scale_size_area(max_size = 10), #Échelle de taille des points
#     scale_color_brewer(palette = "Dark2"), #Palette de couleurs
#     scale_fill_brewer(palette = "Dark2"),
#     coord_fixed(), #Assurer que les proportions relatives des deux axes sont respectées
#     theme_minimal(),
#     theme(legend.position = "none",
#           panel.grid.minor = element_blank(), #element_line(size = 0.05, color="gray96"),
#           panel.grid.major = element_blank(), #element_line(size = 0.05, color="gray96"),
#           strip.text = element_text(face = "bold"), #Titre des facets
#           plot.title = element_text(hjust = 0.5, face = "bold"), #Centrer le titre du graphique
#           axis.title.x = element_text(size = 12, hjust = 1),
#           axis.title.y = element_text(size = 12, hjust = 1) )
#   )
# }

#Fonction : à ajouter à la fin des graphes pour ouvrir en svg dans firefox (normal),
# ou pour ouvrir avec ggplotly (affichage interactif d'infos au survol des points)
#            - Si package (user-friendly) : ggi (sortie graphe interactif)
#' An Output Function to Draw Dynamic Graphs with ggplot2
#' @description Will be saved in the project directory, //2-Images
#' @param plot The plot, created with ggplot2.
#' @param type Draw interactive graph with ggiraph, or save normal graph with ggsave.
#' @param name The name of the file to be created.
#' @param xt The extension name.
#' @param dpi The resolution.
#' @param width The width.
#' @param pixel_width The width in pixels for widgetframe
#' @param scale Fixed ratio between horizontal and vertical axes.
#' @param wholescreen A big graph in 1080p ?
#' @param replace Replace file (otherwise add a number).
#' @param open Open directly ?
#'
#' @return Create a file, and open it in RStudio viewer, as a side effect.
#' @export
#'
# @examples
ggout <- function(plot = last_plot(),
                  type = c("interactive", "normal"),  #"ggplotly"
                  name = "image", xt = "png", dpi =  600,
                  width = 21, pixel_width = NULL, scale = 1, #height = width/1.418919
                  wholescreen = FALSE, replace = FALSE, open = TRUE) { #frame = FALSE
  plot <- plot
  if (wholescreen == TRUE){
    width <- 37.3333
    height <- 21
  }
  if (type[1] == "normal") {
    path <- stringr::str_c("02-Images\\", name, ".", xt)

    if (replace == FALSE) {
      i <- 0
      file_do_not_exist <- FALSE
      while (file_do_not_exist == FALSE) {
        if (file.exists(path)) {
          i = i+1
          path <- stringr::str_c("02-Images\\", name, i, ".", xt)
        } else {
          file_do_not_exist <- TRUE
        }
      }
    }
    ggplot2::ggsave(path, plot = plot, height = width * plot$heigth_width_ratio, width = width, units = "cm",
                    scale = scale, dpi = dpi)
    if (open == TRUE) { file.show(path) }
    #
    # } else if (type[1] == "ggplotly") {
    #   ggplotly(tooltip = "text") %>%
    #     style(textposition = "right", hoverlabel.align = "right")

  } else if (type[1] == "interactive") {

    if ("css_hover" %in% names(plot)) {
      css_hover <- plot$css_hover
    } else {
      css_hover <- ggiraph::girafe_css("fill:gold;stroke:orange;", text = "color:gold4;stroke:none;",
                                       point = "fill:gold;stroke:orange;", area = "fill:white")
    }

    if ("css_tooltip" %in% names(plot)) {
      css_tooltip <- plot$css_tooltip
    } else {
      css_tooltip <- "text-align:right;padding:4px;border-radius:5px;background-color:#eeeeee;"
    }


    ggiraph::girafe(ggobj = plot,
                    width_svg = width/2.563,
                    height_svg = width/2.563 * plot$heigth_width_ratio
    ) %>%
      ggiraph::girafe_options(ggiraph::opts_tooltip(use_fill = TRUE, css = css_tooltip), #use_stroke = FALSE, # = border color of the tooltip #color:white; border-color:black; opacity:1 ; background-color:transparent
                              ggiraph::opts_hover(css = css_hover)
                              # opts_zoom(max = 5) # bugue pas mal
                              # opts_hover(css = girafe_css(css = "fill:purple;stroke:black;", text = "stroke:none;fill:red;font-style:bold;")) #    point = NULL, line, area, image
                              # opts_hover_inv(css = "opacity:0.1;"),
                              # opts_sizing(rescale = TRUE, width = 0.7), #between 0 and 1
                              # opts_toolbar(saveaspng = FALSE)
      ) %>%
      widgetframe::frameWidget(width = pixel_width) #Title and name options : options = widgetframe::frameOptions(name = "Graphique")

  }
}


# res.ca <- FES2017 %>%
#   dplyr::mutate() %>%
#   tabw(CSER, PR2017ALL1, wt = w5, tot = "no",
#        rare_levels_to_other = TRUE, subtext = champ_inscrits) %>%
#   purrr::flatten_df() %>% dplyr::mutate(dplyr::across(tidyselect::where(is_decimal), as.double)) %>% tibble::column_to_rownames(colnames(.)[1]) %>%
#   FactoMineR::CA()
#
# PR1ac <- ggca(res.ca) + ggplot2::ggplot
#
# ggout(PR1ac, "ggiraph", pixel_width = 700) %>%
#   widgetframe::saveWidgetframe("widget.html", selfcontained = TRUE) ; file.show("widget.html")


# ggmca(res.mca, split_var = PE3_ord, dplyr::filter = "^.3C",
#              nb_char_for_color = 1,
#              dist_labels = 0.04, type = "ggplotly") +
#   ylim(NA, 0) + xlim (NA, 1) ; ggout("ggplotly")


# # dat <- ct2013acm
# # dat <- res.mca$call$X %>% tibble::add_column(row.w = res.mca$call$row.w)
# sup_vars = c(sup_vars_contraintes, "PE0")
# tooltip_vars_1lv = c("SEXE", "ENCADRacm")
# tooltip_vars <- c("cah")
# axes = c(1,2)
# nb_char_for_color = 1
# keep_levels = character()
# discard_levels = character()
#
# cleannames = TRUE
# names_darker = FALSE
# shift_colors = 0
# type = "text"
# profiles_tooltip_discard = "^Pas "

# type = "text"
# sup_vars =  c("PE0", "cah")
# tooltip_vars_1lv = "ENCADRacm"
# tooltip_vars = "cah"

# BUGS :             - Couleur du tooltip change inopinément avec labels : signalé david gohel ggiraph
#                    - légende des couleurs : vilain, "a" en couleur et nom en noir
# Ajouter :          - calculer taille par couleur en option (bof) ?
#                    - Option color profiles by active_vars / by cah (on ggmca_ind ?) ?
#                    - Distinguer cah des autres ; la mettre en labels dans type = "text" ? ; tj couleurs liées aux profils (non) ?
#                    - analyse des données structurées



#' Readable, Interactive and Beautiful graph for MCA
#' @description A readable, complete and beautiful graph for multiple
#' correspondence analysis made with \code{FactoMineR::\link[FactoMineR]{MCA}}.
#' Interactive tooltips, appearing when  hovering near points with mouse,
#' allow to keep in mind many important data (tables of active variables,
#' and additionnal chosen variables) while reading the graph.
#' Profiles of answers (from the graph of "individuals") are drawn in the back,
#' and can be linked to \code{FactoMineR::\link[FactoMineR]{ICPC}} classes.
#' Since it is made in the spirit of \code{\link{ggplot2}}, it is possible to
#' change theme or add another plot elements with \code{+}. Then, interactive
#' tooltips won't appear until you pass the result throught \code{\link{ggout}}.
#' @param res.mca An object created with \code{FactoMineR::\link[FactoMineR]{MCA}}.
#' @param sup_vars A character vectors of supplementary qualitative variables
#' to print. They must have been passed in \code{\link[FactoMineR]{MCA}} before.
#' @param tooltip_vars_1lv A character vectors of variables, whose first level
#' (if character/factor) or weighted_mean (if numeric) will be added
#' at the top of interactive tooltips.
#' @param tooltip_vars A character vector of variables (character/factors),
#' whose complete levels will be added at the bottom of interactive tooltips.
#' @param axes The axes to print, as a numeric vector of length 2.
#' @param xlim,ylim Horizontal and vertical axes limits,
#' as double vectors of length 2.
#' @param cleannames Set to \code{TRUE} to clean levels names, by removing
#' prefix numbers like \code{"1-"}, and text in parentheses.
#' @param text_repel When \code{TRUE} the graph is not interactive anymore,
#'  but the resulting image is better to print because points and labels don't
#'  overlaps. It uses \code{ggrepel::\link[ggrepel]{geom_text_repel}}.
#' @param out_lims_move When \code{TRUE}, the points out of \code{xlim} or
#'  \code{ylim} are not removed, but moved at the edges of the graph.
#' @param title The title of the graph.
#' @param type Determines the way \code{sup_vars} are printed.
#'    \itemize{
#'    \item \code{"text"} : colored text
#'    \item \code{"points"} : colored points with text legends
#'    \item \code{"labels"} : colored labels
#'    \item \code{"active_vars_only"} : no \code{sup_vars}
#'    \item \code{"numbers"} : colored labels of prefix numbers, with small names
#'  }
#' @param keep_levels A character vector of variables levels to keep : others
#' will be discarded.
#' @param discard_levels A character vector of variables levels to discard.
#' @param profiles When \code{TRUE}, profiles of answersx are drawn in the back
#' of the graph with light-grey points. When hovering with mouse, the answers of
#' individuals to active variables will appears. If \code{cah} is provided,
#' to hover near one point will color all the points of the same
#'  \code{\link[FactoMineR]{ICPC}} class.
#' @param profiles_tooltip_discard A regex pattern to remove useless levels
#' among interactive tooltips for profiles of answers (ex. : levels expressing
#' "no" answers).
#' @param cah A variable made with \code{\link[FactoMineR]{ICPC}}, to link
#' the answers-profiles points who share the same ICPC class (will be colored
#' together at mouse hover).
#' @param max_profiles The maximum number of profiles points to print.
#' @param nb_char_for_color If \code{sup_vars} are prefixed with numbers, the
#' number of characters to use to create classes that will be used to add
#' colors to points.
#' @param shift_colors Change colors of the \code{sup_vars} points.
#' @param colornames_recode A named character vector with
#' \code{\link[forcats]{fct_recode}} style to rename the levels of the color
#' variable if needed (levels used for colors are printed in console message
#' whenever the function is used).
#' @param text_size Size of text.
#' @param size_scale_max Size of points.
#' @param dist_labels When \code{type = points}, the distance of labels
#' from points.
#' @param right_margin A margin at the right, in cm. Useful to read tooltips
#'  over points placed at the right of the graph without formatting problems.
#'
#' @return A \code{\link[ggplot2]{ggplot}} object to be printed in the
#' RStudio Plots pane. Possibility to add other gg objects with \code{+}.
#' Sending the result through \code{\link{ggout}} will draw the
#' interactive graph in the Viewer pane using \code{\link{ggiraph}}.
#' @export
#'
# @examples
ggmca <-
  function(res.mca = res.mca, sup_vars, tooltip_vars_1lv, tooltip_vars, axes = c(1,2), xlim, ylim,
           cleannames = TRUE, text_repel = FALSE, out_lims_move = FALSE, title,
           type = c("text", "points", "labels", "active_vars_only", "numbers"),
           keep_levels, discard_levels, #function_on_dat,
           profiles = TRUE, profiles_tooltip_discard = "^Pas |Non ", cah, max_profiles,
           nb_char_for_color = rep(0, length(sup_vars)), shift_colors = 0, colornames_recode,
           text_size = 3, size_scale_max = 8, dist_labels = c("auto", 0.04),right_margin = 0
           #, tooltip_vars_text
  ) {

    dim1 <- rlang::sym(stringr::str_c("Dim ", axes[1]))
    dim2 <- rlang::sym(stringr::str_c("Dim ", axes[2]))

    if (missing(sup_vars))     sup_vars     <- character()
    if (missing(tooltip_vars_1lv))  tooltip_vars_1lv  <- character()
    if (missing(tooltip_vars)) tooltip_vars <- character()
    if (missing(keep_levels)) keep_levels <- character()
    if (missing(discard_levels)) discard_levels <- character()
    if (missing(colornames_recode)) colornames_recode <- character()
    if (missing(cah) ) {
      cah <- character()
    } else if(! cah %in% names(res.mca$call$X)[res.mca$call$quali.sup]) {
      warning(cah, " was not found among the supplementary variables of the mca")
      cah <- character()
    }


    active_vars <- stringr::str_c(colnames(res.mca$call$X)[1:length(res.mca$call$quali)])
    if (length(sup_vars)    != 0 )      sup_vars         %<>% purrr::discard(. %in% active_vars)
    if (length(tooltip_vars_1lv) != 0 ) tooltip_vars_1lv %<>% purrr::discard(. %in% active_vars) #| . %in% sup_vars
    if (length(tooltip_vars) != 0 )     tooltip_vars     %<>% purrr::discard(. %in% active_vars | . %in% tooltip_vars_1lv) #| . %in% sup_vars


    #if (names_darker == "auto") {      # if (type[1] == "points") names_darker <- TRUE
    # if (type[1] %in% c("active_vars_only", "labels", "text")) names_darker <- FALSE
    #}



    #Variables supplémentaires :
    if (length(sup_vars) != 0) {
      #if (missing(dat)) {
      data_name <- as.character(res.mca$call$call$X[[2]])
      if (! exists(data_name)) stop(stringr::str_c("database used to make the mca was not found : ", data_name, "(needed to but factor levels in the right order)"))
      #dat <- eval(res.mca$call$call$X[[2]])
      levels_sup_vars <- purrr::map(sup_vars, ~ dplyr::pull(eval(res.mca$call$call$X[[2]]), .) %>% levels())
      #pond_var <- rlang::expr(row.w)
      # } else {
      #   levels_sup_vars <- dplyr::select(dat, tidyselect::all_of(sup_vars)) %>% purrr::map(levels)
      #   pond_var <- res.mca$call$call$row.w %>% as.character() %>% .[3] %>% rlang::sym()
      # }
      levels_sup_vars <- purrr::map2(levels_sup_vars, sup_vars, ~ append(.x, stringr::str_c(.y, "_", .x)) )

      sup_vars_coord <-
        purrr::map(levels_sup_vars, function(.lvl) res.mca$quali.sup$coord %>% tibble::as_tibble(rownames = "noms") %>%
                     dplyr::filter(noms %in% .lvl) %>% dplyr::mutate(noms = forcats::fct_relevel(noms, purrr::keep(.lvl, .lvl %in% noms))) %>%
                     dplyr::arrange(noms) ) %>% magrittr::set_names(sup_vars)


      #Quand la fonction MCA() a ajouté le nom de la variable au début des noms de modalités, l’enlever
      sup_vars_coord %<>%
        purrr::imap(~ dplyr::mutate(.x, noms = fct_clean(noms, stringr::str_c("^", .y, "_")))) %>%  #More precise ?
        purrr::map(~ dplyr::select(., noms, !!dim1, !!dim2))
      sup_vars_coord %<>%
        purrr::map(~dplyr::mutate(., numbers = dplyr::case_when(
          stringr::str_detect(noms, "^[^- ]+-(?![[:lower:]])|^[^- ]+(?<![[:lower:]])-")
          ~ stringr::str_extract(noms, "^[^- ]+"),
          TRUE ~ "" )))
      if (cleannames == TRUE) sup_vars_coord %<>% purrr::map(~ dplyr::mutate(., noms = fct_clean(noms)))



      #Useful functions :
      #fct_relevel_quiet <- purrr::quietly(forcats::fct_relevel)
      bind_cols_quiet   <- purrr::quietly(dplyr::bind_cols)
      #tab_spread <- function(data) dplyr::mutate_at(data, dplyr::vars(-1, -ncol(data)), ~. - dplyr::last(.))
      # tab_spread_chr <- function(data) {
      #   dplyr::mutate_at(data, dplyr::vars(-1, -tidyselect::any_of("Total")), ~ dplyr::case_when(
      #     dplyr::row_number() == nrow(data) ~ stringr::str_c(., "%"),
      #     . - dplyr::last(.) > 0         ~ stringr::str_c("(", stringr::str_pad(stringr::str_c("+" , sign(. - dplyr::last(.)) * (. - dplyr::last(.))), 3 + get_digits(.)), "%) ", stringr::str_pad(., 2 + get_digits(.)), "%"),
      #     TRUE                    ~ stringr::str_c("(", stringr::str_pad(stringr::str_c(" -", sign(. - dplyr::last(.)) * (. - dplyr::last(.))), 4 + get_digits(.)), "%) ", stringr::str_pad(., 2 + get_digits(.)), "%") )
      #   )
      # }
      var_spread_chr <- ~ dplyr::case_when(
        . == dplyr::last(.) ~ stringr::str_c(., "%"),
        . - dplyr::last(.) > 0         ~ stringr::str_c("(", stringr::str_pad(stringr::str_c("+" , sign(. - dplyr::last(.)) * (. - dplyr::last(.))),
                                                                              3 + get_digits(.), pad = "#"),
                                                        "%) ", stringr::str_pad(., 2 + get_digits(.), pad = "#"), "%"),
        TRUE                    ~ stringr::str_c("(", stringr::str_pad(stringr::str_c(" -", sign(. - dplyr::last(.)) * (. - dplyr::last(.))),
                                                                       4 + get_digits(.), pad = "#"),
                                                 "%) ", stringr::str_pad(., 2 + get_digits(.), pad = "#"), "%") ) %>%
        stringr::str_replace_all("#", " ") #There were unbreakable space ??
      #If digits are more than 1 : add 1*sign(get_digits(.)) ?



      #Interactive tooltips
      dat <- res.mca$call$X %>% tibble::add_column(row.w = res.mca$call$row.w) %>% tibble::as_tibble()
      dat %<>% dplyr::mutate(no_var2 = "n", no_var3 = "") %>%
        dplyr::select(tidyselect::all_of(sup_vars), no_var2, no_var3, tidyselect::all_of(c(tooltip_vars_1lv, active_vars, tooltip_vars)), row.w) %>%
        dplyr::select(tidyselect::all_of(c(tooltip_vars_1lv, active_vars, tooltip_vars)), tidyselect::everything())
      all_lvl <- purrr::flatten_chr(levels_sup_vars)
      dat %<>% dplyr::mutate_if(is.factor, ~ forcats::fct_relevel(., purrr::keep(all_lvl, all_lvl %in% levels(.))) )
      if (length(tooltip_vars_1lv) != 0) {
        tooltip_vars_1lv_3levels <-
          purrr::map_lgl(dat, ~ is.factor(.) & nlevels(.) >= 3) & colnames(dat) %in% tooltip_vars_1lv
        if (any(tooltip_vars_1lv_3levels)) dat %<>%
          dplyr::mutate_if(tooltip_vars_1lv_3levels, ~ forcats::fct_other(., keep = levels(.)[1], other_level = "Autres"))
      }

      sup_list <- tabr_make_sup_list(dat, sup_cols = c(tooltip_vars_1lv, active_vars, tooltip_vars))

      # if (!missing(function_on_dat)) {
      #   if (rlang::is_function(function_on_dat) ) dat %<>% function_on_dat()
      # }

      # #Drop NA here or in the tab at the end ?
      # dat %<>% dplyr::mutate_if(function(.var) is.factor(.var) & any(stringr::str_detect(levels(.var), ".NA")),
      #                    ~ fct_clean(., ".NA"))

      wtables <-
        purrr::map(sup_vars, function(.var)
          tabr_core(dat, !!rlang::sym(.var), no_var2, no_var3, wt = row.w,
                    sup_list = purrr::map_if(sup_list, purrr::map_lgl(sup_list, ~ length(.) != 0),
                                             ~ purrr::discard(., sup_list$sup_cols == .var)), #Drop tooltip var when same than sup var
                    digits = 0L, only_first_level = FALSE, not_last_level = FALSE,
                    multicols = TRUE, accelerate = TRUE, perc = "col") %>% dplyr::filter(!stringr::str_detect(!!rlang::sym(.var), "\\.NA$")) %>%
            dplyr::group_by(.SUP_NAME, !!rlang::sym(.var)) %>%
            dplyr::filter(.zone == "base" | .SUP_NAME %in% tooltip_vars |
                            (.SUP_NAME %in% active_vars & .SUP != dplyr::last(.SUP)) |
                            (.SUP_NAME %in% tooltip_vars_1lv & .SUP == dplyr::first(.SUP)) ) %>%
            dplyr::ungroup()
        ) %>% magrittr::set_names(sup_vars)

      #Quand la fonction MCA() a ajouté le nom de la variable au début des noms de modalités, l’enlever
      wtables %<>% purrr::imap(~ dplyr::mutate_at(.x, tidyselect::all_of(.y), function(.var) fct_clean(.var, stringr::str_c("^", .y, "_")) ) %>%
                                 dplyr::mutate(.SUP = stringr::str_remove(.SUP, stringr::str_c("^", .SUP_NAME, "_"))) )

      if (cleannames == TRUE) {
        wtables %<>% purrr::imap(~ dplyr::mutate_at(.x, tidyselect::all_of(.y), function(.var) fct_clean(.var) )) %>%
          purrr::map(~ dplyr::mutate(., .SUP = fct_clean(.SUP)))
      }

      sup_vars_count <- wtables %>%
        purrr::imap(~ tabdraw(.x, weighted_n, row_var = !!.y, col_var = no_var2, tab_var = no_var3,
                              tot = "row", totaltab = "no", perc = "no") %>%
                      purrr::map(~ dplyr::mutate(., n = as.double(n)) %>% dplyr::rename(wcount = n) %>% dplyr::rename_at(1, ~"noms")) ) %>%
        purrr::flatten()

      # sup_vars_begin_text <- wtables %>%
      #   purrr::imap(~ dplyr::mutate(.x, clean_levels = fct_clean(!!rlang::sym(.y), stringr::str_c(.y, "_")) ))
      # if (cleannames == TRUE) sup_vars_begin_text %<>% purrr::map(~ dplyr::mutate(., clean_levels = fct_clean(clean_levels)))
      sup_vars_begin_text <- wtables %>%
        purrr::imap(~ dplyr::mutate(.x, begin_text = stringr::str_c("<b>", !!rlang::sym(.y), "</b>\nEffectifs : ", stringr::str_c(pct, "%")) ) %>%  #, "\n\n<b>Variables actives :</b>"
                      tabdraw(begin_text, row_var = !!.y, col_var = no_var2, tab_var = no_var3,
                              tot = "row", totaltab = "no", perc = "col") %>%
                      purrr::map(~ dplyr::rename_at(., 1, ~"noms") %>% dplyr::rename(begin_text = n) ) ) %>% purrr::flatten()

      interactive_text <- wtables %>%
        purrr::imap(~ dplyr::group_by(dplyr::filter(.x, .zone == "sup_cols"), .SUP) %>%
                      dplyr::mutate_at("pct", var_spread_chr) %>% dplyr::ungroup() %>% dplyr::mutate_at("pct", ~ stringr::str_c("\n", .SUP, " : ", .)) %>%
                      tabdraw(pct, row_var = !!.y, col_var = .SUP, tab_var = no_var3,
                              zone = "sup_cols", tot = "row", totaltab = "no", perc = "row") %>%
                      purrr::map(~ dplyr::rename_at(., 1, ~"noms") ) ) %>% purrr::flatten()
      tooltip_first_levels <- wtables[[1]] %>% dplyr::filter(.SUP_NAME %in% tooltip_vars) %>%
        dplyr::group_by(.SUP_NAME) %>% dplyr::slice(1) %>% dplyr::pull(.SUP) %>% as.character()       #purrr::map(tooltip_vars, ~ dplyr::pull(dat, !!rlang::sym(.)) %>% levels() %>% .[1])
      active_first_variable <- wtables[[1]] %>% dplyr::filter(.SUP_NAME == active_vars[1]) %>%
        dplyr::slice(1) %>% dplyr::pull(.SUP) %>% as.character()  #dplyr::pull(dat, !!rlang::sym(active_vars[1])) %>% levels() %>% .[1]
      interactive_text %<>%
        purrr::map(~ purrr::reduce2(.x = tooltip_first_levels, .y = tooltip_vars, .init = .,
                                    .f = ~ dplyr::mutate_at(..1, dplyr::vars(tidyselect::any_of(..2)),
                                                            function(.var) stringr::str_c("\n\n<b>Répartition par ", ..3, " :</b>" , .var))) ) %>%
        purrr::map(~dplyr::mutate_at(., tidyselect::all_of(active_first_variable),
                                     function(.var) stringr::str_c("\n\n<b>Variables actives :</b>", .var)))

      tooltip_text_final <- sup_vars_count %>%
        purrr::map2(sup_vars_begin_text, ~ dplyr::inner_join(.x, .y, by = "noms")) %>%
        purrr::map2(interactive_text, ~ dplyr::inner_join(.x, .y, by = "noms")) %>%
        purrr::map(~ tidyr::unite(., "interactive_text", -1:-2, sep = ""))

      mean_point_coord <-
        tooltip_text_final[[1]] %>% dplyr::filter(stringr::str_detect(noms, "^Total")) %>%
        dplyr::mutate(noms = "Point moyen", !!dim1 := 0, !!dim2 := 0, colorvar = "0", numbers = "", wcount = 1,
                      interactive_text = stringr::str_replace(interactive_text, "^<b>Total", "<b>Point moyen"))

      sup_vars_coord <-  sup_vars_coord %>%
        purrr::map2(tooltip_text_final, ~ dplyr::inner_join(.x, .y, by = "noms")) %>% magrittr::set_names(sup_vars)  #%>% purrr::imap(~ dplyr::mutate(.x, noms = fct_clean(noms, stringr::str_c(.y, "_")) ))


      if (length(keep_levels   ) >= 1L) sup_vars_coord %<>% purrr::map(~ dplyr::filter(., stringr::str_detect(noms, keep_levels)) )
      if (length(discard_levels) >= 1L) sup_vars_coord %<>% purrr::map(~ dplyr::filter(., ! stringr::str_detect(noms, discard_levels)) )

      #Se debrouiller pour que, si la cah est dans les var sup et dans les profils, les data_id soient les mêmes :
      # les deux seront colorés lorsqu’on survolera l’un ou l’autre
      #sup_vars_coord %<>% purrr::imap(~ dplyr::mutate(.x, sup_var = .y))
      if (length(cah) != 0 & cah %in% sup_vars) sup_vars_coord %<>%
        purrr::map_if(names(.) == cah,
                      ~ dplyr::mutate(., cah_id = as.integer(noms) + 10000L),
                      .else = ~ dplyr::mutate(., cah_id = NA_integer_))


      # Variable de couleur (colorvar) selon le nb de variables supplémentaires et le nb de caractères indiqués
      if (length(nb_char_for_color) == 1) {
        sup_vars_coord %<>%
          purrr::map2(sup_vars, ~ dplyr::mutate(.x, colorvar = as.factor(stringr::str_c(.y, "_", stringr::str_sub(numbers, 1, nb_char_for_color)) %>%
                                                                           stringr::str_remove("_$")) ))
      } else if (length(nb_char_for_color) == length(sup_vars)) {
        sup_vars_coord <-
          purrr::pmap(list(sup_vars_coord, sup_vars, nb_char_for_color),
                      ~ dplyr::mutate(..1, colorvar = as.factor(stringr::str_c(..2, "_", stringr::str_sub(numbers, 1, ..3)) %>%
                                                                  stringr::str_remove("_$")) ))
      }
      sup_vars_coord %<>% dplyr::bind_rows()

      if (length(colornames_recode) > 0) sup_vars_coord %<>%
        dplyr::mutate(colorvar = forcats::fct_recode(colorvar, !!!colornames_recode))
      if (shift_colors != 0) sup_vars_coord %<>% dplyr::mutate(colorvar = forcats::fct_shift(colorvar, shift_colors))
      colorvar_recode <- sup_vars_coord %>% dplyr::pull(colorvar) %>% levels()
      if (length(colorvar_recode) >= 2) {
        cat("\nColors are based on the following variables :\n")
        print(colorvar_recode)
      }

      scale_color_points <-  c(    # Material colors :
        "#7e57c2",       # Deep purple 400
        "#f57c00",       # Orange 700
        "#7cb342",       # Light green 600

        "#26a69a",       # Teal 400
        "#d32f2f",       # Red 700
        "#9e9d24",       # Lime 800

        #"#f9a825",      # Jaune 800     #reserved for hover effect
        "#607d8b",       # Blue Grey 500
        "#8d6e63",       # Brown 400
        "#ba68c8",       # Purple 300

        "#f06292",       # Pink 300
        "#388e3c",       # Vert 800
        "#42a5f5"        # Blue 400
      ) %>% magrittr::set_names(colorvar_recode[1:12])

      scale_color_names <-  c(
        "#4d2c91",       # Deep purple 400 Dark
        "#bb4d00",       # Orange 700 Dark
        "#4b830d",       # Light green 600 Dark

        "#00766c",       # Teal 400 Dark
        "#9a0007",       # Red 700 Dark
        "#6c6f00",       # Lime 800 Dark

        #"#c17900",      # Jaune 800 Dark     #reserved for hover effect
        "#34515e",       # Blue Grey 500 Dark
        "#5f4339",       # Brown 400 Dark
        "#883997",       # Purple 300 Dark

        "#ba2d65",       # Pink 300 Dark
        "#00600f",       # Green 800 Dark
        "#0077c2"        # Blue 400 Dark
      ) %>% magrittr::set_names(stringr::str_c("names_", colorvar_recode[1:12]))

      if (length(colorvar_recode[-(1:12)]) > 0) {
        scale_color_points <- scale_color_points %>%
          append(rep(.[length(.)], length(colorvar_recode[-(1:12)])) %>%
                   magrittr::set_names(colorvar_recode[-(1:12)]))
        scale_color_names <- scale_color_names %>%
          append(rep(.[length(.)], length(colorvar_recode[-(1:12)])) %>%
                   magrittr::set_names(colorvar_recode[-(1:12)]))
        warning("too much colors, all the last ones were set to blue (max 12)")
      }

      scale_color_named_vector <- scale_color_points %>% append(scale_color_names) %>% .[!is.na(names(.))]

      if (type[1] %in% c("points", "numbers"))  sup_vars_coord %<>% dplyr::mutate(colorvar_names = as.factor(stringr::str_c("names_", colorvar)))
      #} else { sup_vars_coord %<>% dplyr::mutate(colorvar_names =  colorvar) }

      if (length(cah) != 0 & cah %in% sup_vars) {
        sup_vars_coord %<>% dplyr::mutate(id = dplyr::if_else(is.na(cah_id), dplyr::row_number(), cah_id))
      } else {
        sup_vars_coord %<>% dplyr::mutate(id = dplyr::row_number())
      }


      # Enlever les caractères spéciaux dans le graph
      if (cleannames == TRUE) sup_vars_coord %<>%
        dplyr::mutate(noms = #stringr::str_remove_all(noms, cleannames_condition()) %>%
                        stringr::str_replace_all(noms, "[^[:alnum:][:punct:]]", " ") %>% stringr::str_squish() )
    }





    #Variables actives :
    levels_active_vars <- purrr::map(active_vars, ~ dplyr::pull(eval(res.mca$call$call$X[[2]]), .) %>% levels()) %>%
      magrittr::set_names(active_vars) %>% purrr::imap(~ append(.x, stringr::str_c(.y, "_", .x)))

    active_vars_wcount <-
      purrr::map(levels_active_vars, function(.lvl) res.mca$call$marge.col %>% tibble::as_tibble(rownames = "noms") %>% dplyr::rename(freq = value) %>%
                   dplyr::filter(noms %in% .lvl) %>% dplyr::mutate(noms = forcats::fct_relevel(noms, purrr::keep(.lvl, .lvl %in% noms))) %>% dplyr::arrange(noms) ) %>% magrittr::set_names(active_vars)
    # active_vars_wcount %>% purrr::imap(~ dplyr::mutate(.x, noms = fct_clean(noms, stringr::str_c("^", .y, "_"))))
    if (cleannames == TRUE) active_vars_wcount %<>% purrr::map(~ dplyr::mutate(., noms = fct_clean(noms)))
    active_vars_wcount %<>% purrr::map(~ dplyr::mutate_at(., -1, ~ as_pct(./sum(.))))

    # S'il n'y a aucune variable supplémentaire précisée calculer le tooltip du point moyen à partir des informations de res.mca
    if (length(sup_vars) == 0) {
      mean_point_interactive_text <- active_vars_wcount %>%
        purrr::map(~ dplyr::slice(., - nrow(.)) %>% dplyr::mutate(interactive_text = stringr::str_c("\n", noms, " : ", freq, "%")) %>%
                     dplyr::summarise(interactive_text = stringr::str_c(interactive_text, collapse = "") ) %>%
                     dplyr::pull(interactive_text)
        ) %>% purrr::flatten_chr() %>% stringr::str_c(collapse = "")
      mean_point_interactive_text <-
        stringr::str_c("<b>Point moyen</b>", "\nEffectifs : 100%", "\n\n<b>Variables actives :</b>", mean_point_interactive_text)
      mean_point_coord <-
        tibble::tibble(!!dim1 := 0, !!dim2 := 0, colorvar = "0", numbers = 0, wcount = 1, interactive_text = mean_point_interactive_text)
      scale_color_named_vector <- character()
      type <- "active_vars_only"
    }

    active_vars_wcount %<>% dplyr::bind_rows()

    active_var_coord <- res.mca$var$coord %>% tibble::as_tibble(rownames = "noms") %>% dplyr::select(noms, !!dim1, !!dim2)
    if (cleannames == TRUE) active_var_coord %<>% dplyr::mutate(noms = fct_clean(noms))

    active_var_coord %<>% dplyr::left_join(active_vars_wcount, by = "noms") %>%
      dplyr::bind_cols(dplyr::rename_all(dplyr::select(tibble::as_tibble(res.mca$var$contrib, rownames = NULL),  !!dim1, !!dim2), ~ c("contrib1", "contrib2"))) %>%
      dplyr::mutate(interactive_text = stringr::str_c("<b>", noms, "</b>", #"\nVariable active",
                                                      "\nEffectifs : ", stringr::str_c(freq, "%"),
                                                      "\nContrib axe 1 : ", stringr::str_pad(format(contrib1, digits = 0), 2), "%",
                                                      "\nContrib axe 2 : ", stringr::str_pad(format(contrib2, digits = 0), 2), "%"),
                    id = dplyr::row_number())

    # res.mca$var$cos2 %>% tibble::as_tibble(rownames = "noms") %>% dplyr::mutate_at(-1, ~ as_pct(.)) # %>% dplyr::rowwise() %>% dplyr::mutate(Total = sum(dplyr::c_across(`Dim 1`:`Dim 8`)))
    # res.mca$var$v.test %>% tibble::as_tibble(rownames = "noms")
    # res.mca$var$eta2 %>% tibble::as_tibble(rownames = "noms") %>% dplyr::mutate_at(-1, ~ as_pct(.))

    # #Quality of representation, calculated by % of the variance of questions (must be done with all axes in res.mca !)
    # res.mca$var$cos2 %>% tibble::as_tibble(rownames = NULL) %>%
    #   purrr::map2_dfc(res.mca$eig[1:length(.), 1], ~.x*.y) %>% dplyr::rowwise() %>%
    #   dplyr::mutate(Total = sum(dplyr::c_across(1:length(.)))) %>% dplyr::ungroup() %>%
    #   dplyr::mutate_all(~ as_pct(./Total)) %>% dplyr::bind_cols(tibble::as_tibble(res.mca$var$cos2, rownames = "noms")[1]) %>%
    #   dplyr::select(noms, tidyselect::everything())






    #Calculer les limites du graphique (argument à passer dans ggout pour regler la taille du htmlwidget)
    if (length(sup_vars) != 0) {
      min_max_lims <-
        dplyr::bind_rows(dplyr::select(active_var_coord, !!dim1, !!dim2),
                         dplyr::select(sup_vars_coord, !!dim1, !!dim2))
    } else {
      min_max_lims <- dplyr::select(active_var_coord, !!dim1, !!dim2)
    }

    if (!missing(xlim)) min_max_lims %<>%
      tibble::add_row(!!dim1 := xlim[1]) %>% tibble::add_row(!!dim1 := xlim[2])
    if (!missing(ylim)) min_max_lims %<>%
      tibble::add_row(!!dim2 := ylim[1]) %>% tibble::add_row(!!dim2 := ylim[2])
    heigth_width_ratio <- min_max_lims %>% dplyr::summarise_all(~ max(., na.rm = TRUE) - min(., na.rm = TRUE), .groups = "drop")
    min_max_lims <-
      dplyr::bind_rows(dplyr::summarise_all(min_max_lims, ~ min(., na.rm = TRUE), .groups = "drop"),
                       dplyr::summarise_all(min_max_lims, ~ max(., na.rm = TRUE), .groups = "drop"))
    width_range <- dplyr::pull(heigth_width_ratio, 1)[1]
    heigth_width_ratio %<>% dplyr::summarise(heigth_width_ratio = !!dim2/!!dim1, .groups = "drop") %>% tibble::deframe()

    if (dist_labels[1] == "auto") dist_labels <- width_range/40

    if (!missing(xlim) & !missing(ylim))  {theme_acm_with_lims <-
      theme_mca(res = res.mca, axes = axes, no_color_scale = TRUE, size_scale_max = size_scale_max,  # legend.position = "bottom",
                xlim = c(xlim[1], xlim[2]), ylim = c(ylim[1], ylim[2]))}
    else if (!missing(xlim) ) {theme_acm_with_lims <-
      theme_mca(res = res.mca, axes = axes, no_color_scale = TRUE, size_scale_max = size_scale_max,  # legend.position = "bottom",
                xlim = c(xlim[1], xlim[2]) )}
    else if (!missing(ylim) )  {theme_acm_with_lims <-
      theme_mca(res = res.mca, axes = axes, no_color_scale = TRUE, size_scale_max = size_scale_max,  # legend.position = "bottom",
                ylim = c(ylim[1], ylim[2]))}
    else {theme_acm_with_lims <-
      theme_mca(res = res.mca, axes = axes, no_color_scale = TRUE, size_scale_max = size_scale_max)} # legend.position = "bottom",

    outlims <- function(data, lim, dim) {
      dim <- rlang::enquo(dim)
      if (!is.na(lim[1])) data %<>% dplyr::filter(!!dim > lim[1])
      if (!is.na(lim[2])) data %<>% dplyr::filter(!!dim < lim[2])
      return(data)
    }

    if (text_repel == FALSE | out_lims_move == FALSE) {
      if (!missing(xlim)) {
        active_var_coord %<>% outlims(xlim, !!dim1)
        sup_vars_coord %<>% outlims(xlim, !!dim1)
      }
      if (!missing(ylim)) {
        active_var_coord %<>% outlims(ylim, !!dim2)
        sup_vars_coord %<>% outlims(ylim, !!dim2)
      }
    }


    #Calculer les profils de réponse de l'ACM ; pondérés : nb d'individus × pond
    if (profiles == TRUE) {
      dat_profils <-
        res.mca$call$X[c(res.mca$call$quali, which(names(res.mca$call$X) == cah))] %>%
        tibble::add_column(row.w = res.mca$call$row.w) %>% tibble::as_tibble()
      if (cleannames == TRUE) dat_profils %<>% dplyr::mutate_if(is.factor, fct_clean)
      if (length(cah) != 0) {
        cah_levels <- dplyr::pull(dat_profils, !!rlang::sym(cah) ) %>% levels
        dat_profils %<>% dplyr::group_by_at(dplyr::vars(-row.w, -!!rlang::sym(cah))) %>%
          dplyr::summarise(count = sum(n()), wcount = sum(row.w),
                           cah = list(!!rlang::sym(cah)) %>% purrr::map_if(purrr::map_lgl(., ~ nlevels(forcats::fct_drop(.)) == 1),
                                                                           ~ as.character(.[1]), .else = ~ NA_character_) %>%
                             purrr::flatten_chr() %>% factor(),
                           .groups = "drop") %>%
          dplyr::mutate(cah = forcats::fct_relevel(cah, cah_levels))

        dat_profils %<>% dplyr::arrange(-wcount) %>% dplyr::mutate(nb = dplyr::row_number(), cah_id = as.integer(cah)) %>%
          dplyr::group_by(cah) %>% dplyr::mutate(nb_in_cah = dplyr::row_number(), nb_tot_cah = n()) %>% dplyr::ungroup()
      } else {
        dat_profils %<>% dplyr::group_by_at(dplyr::vars(-row.w)) %>%
          dplyr::summarise(count = sum(n()), wcount = sum(row.w), .groups = "drop") %>%
          dplyr::arrange(-wcount) %>% dplyr::mutate(nb = dplyr::row_number())
      }

      res.mca.profils <-   #Refaire l'ACM avec les profils pondérés :
        FactoMineR::MCA(dat_profils[ , 1:(which(colnames(dat_profils) == "count") - 1)],
                        row.w = dat_profils$wcount, ncp = res.mca$call$ncp,
                        graph = FALSE)
      ind_coord <- dat_profils %>% dplyr::bind_cols(dplyr::select(tibble::as_tibble(res.mca.profils$ind$coord), !!dim1, !!dim2))

      #Discard the points that are out of limits
      if (!missing(xlim)) ind_coord %<>% outlims(xlim, !!dim1)
      if (!missing(ylim)) ind_coord %<>% outlims(ylim, !!dim2)

      if (!missing(max_profiles)) ind_coord %<>% dplyr::slice (1:max_profiles)

      ind_coord %<>%
        dplyr::mutate_at(res.mca$call$quali, ~ fct_detect_replace(., profiles_tooltip_discard, "#")) %>% #Enlever tout ce qui commence par pas !
        dplyr::mutate_if(is.factor, as.character) %>%
        dplyr::rowwise() %>% dplyr::mutate(interactive_text = stringr::str_c(dplyr::c_across(res.mca$call$quali), collapse = "\n")) %>% dplyr::ungroup()

      if (length(cah) != 0) {
        ind_coord %<>%
          dplyr::mutate(interactive_text = stringr::str_c("<b>Cah : ", cah, #stringr::str_to_lower(cah, locale = "fr"),
                                                          "\nProfil de réponse n°", nb_in_cah, "/", nb_tot_cah,  "</b>\n",
                                                          "Nb d’individus : ", format(count, digits = 0, trim = TRUE, big.mark = " "), "\n",
                                                          "Pondéré : ", format(wcount, digits = 0, trim = TRUE, big.mark = " "), "\n\n",
                                                          interactive_text) %>%
                          stringr::str_remove_all("\n#"))

        profiles <-
          ggiraph::geom_point_interactive(data = ind_coord,
                                          ggplot2::aes(x = !!dim1, y = !!dim2, size = wcount, tooltip = interactive_text, data_id = cah_id + 10000),
                                          color = "#eeeeee", na.rm = TRUE, inherit.aes = FALSE, show.legend = FALSE)
      } else {
        ind_coord %<>%
          dplyr::mutate(interactive_text = stringr::str_c("<b>Profil de réponse n°", nb,  "</b>\n",
                                                          "Nb d’individus : ", format(count, digits = 0, trim = TRUE, big.mark = " "), "\n",
                                                          "Pondéré : ", format(wcount, digits = 0, trim = TRUE, big.mark = " "), "\n\n",
                                                          interactive_text) %>%
                          stringr::str_remove_all("\n#"))

        profiles <-
          ggiraph::geom_point_interactive(data = ind_coord,
                                          ggplot2::aes(x = !!dim1, y = !!dim2, size = wcount, tooltip = interactive_text, data_id = nb + 10000),
                                          color = "#eeeeee", na.rm = TRUE, inherit.aes = FALSE, show.legend = FALSE)
      }
      interactive
    } else {
      profiles <- NULL
    }


    #Tracer les graphiques -----------------------------------------------------

    #Graphe des variables actives :
    if (text_repel == FALSE) {
      active_graph <-
        ggiraph::geom_text_interactive(data = active_var_coord,
                                       ggplot2::aes(x = !!dim1, y = !!dim2, label = noms, tooltip = interactive_text, data_id = id + 1000),
                                       color = "black", fontface = dplyr::if_else(type[1] == "active_vars_only", "bold", "plain"), # alpha = ifelse(type[1] == "points", 0.8, 1)
                                       size = text_size, na.rm = TRUE, inherit.aes = FALSE)
    } else {
      active_graph <-
        ggrepel::geom_text_repel(data = active_var_coord,
                                 ggplot2::aes(x = !!dim1, y = !!dim2, label = noms),
                                 color = "black", alpha = dplyr::if_else(type[1] == "points", 0.8, 1), fontface = dplyr::if_else(type[1] == "active_vars_only", "bold", "plain"), # alpha = ifelse(type[1] == "points", 0.8, 1)
                                 size = text_size, direction = "both", arrow = ggplot2::arrow(length = ggplot2::unit(0.25, "lines")), #point.padding = 0, lineheight = 0.6, segment.alpha
                                 min.segment.length = 0.4, na.rm = TRUE, inherit.aes = FALSE) #, box.padding = 0
    }

    # If type is text, put the active_vars on the same base than suplementary vars, to avoid overlapping of the two.
    if (text_repel == TRUE & type[1] == "text") {
      sup_vars_coord %<>% dplyr::mutate(face = "bold") %>%
        dplyr::bind_rows(dplyr::mutate(dplyr::select(active_var_coord, -contrib1, -contrib2, -interactive_text),
                                       colorvar = factor("active_vars"), wcount = 0, face = "plain")) #colorvar_names = factor("active_vars")
      scale_color_named_vector %<>% append(c("active_vars" = "black"))
      active_graph <- NULL
    }


    #Point moyen :
    mean_point_graph <-
      ggiraph::geom_point_interactive(data = mean_point_coord,
                                      ggplot2::aes(x = !!dim1, y = !!dim2, tooltip = interactive_text),
                                      color = "black", fill = "#eeeeee", shape = 3, size = 5, stroke = 1.5, na.rm = TRUE, inherit.aes = FALSE)

    if (!missing(title)) {
      title_graph <- ggplot2::labs(title = title) #stringr::str_c("Les variables actives de l'ACM sur les axes ",axes[1], " et ", axes[2] )
    } else {
      title_graph <- NULL
    }

    graph_theme_acm <-
      list(theme_acm_with_lims,
           ggplot2::scale_colour_manual(values = scale_color_named_vector, aesthetics = c("colour", "fill")),
           ggplot2::theme(plot.margin = ggplot2::margin(r = right_margin, unit = "cm")),
           title_graph)

    if (type[1] == "points") {
      plot_output <- ggplot2::ggplot(sup_vars_coord, ggplot2::aes(x = !!dim1, y = !!dim2, label = noms, color = colorvar, data_id = id)) +
        graph_theme_acm + profiles + active_graph +
        ggrepel::geom_text_repel(ggplot2::aes(color = colorvar_names),  #data = sup_vars_coord,
                                 size = text_size, hjust = "left",  segment.alpha = 0.2, #segment.colour = "black",
                                 direction = "y", nudge_x = dist_labels[1], point.padding = 0.25,
                                 na.rm = TRUE, fontface = "plain"  ) + # ifelse(names_darker == TRUE, "plain", "bold")
        ggiraph::geom_point_interactive(ggplot2::aes(size = wcount, fill = colorvar, tooltip = interactive_text), #data = sup_vars_coord, ggplot2::aes(x = !!dim1, y = !!dim2, color = colorvar)
                                        shape = 18, na.rm = TRUE) +
        mean_point_graph



      css_hover <- ggiraph::girafe_css("fill:gold;stroke:orange;", text = "color:gold4;stroke:none;")
      plot_output %<>% append(c("css_hover" = css_hover)) #retrieves class ggplot2::ggplot after


    } else if (type[1] == "active_vars_only") {
      plot_output <- ggplot2::ggplot() + graph_theme_acm + profiles + active_graph + mean_point_graph



    } else if (type[1] == "labels") {
      if (text_repel == FALSE) {
        graph_labels <-
          ggiraph::geom_label_interactive(ggplot2::aes(tooltip = interactive_text), #data = sup_vars_coord, ggplot2::aes(color = colorvar_names),
                                          size = text_size, fontface = "bold", na.rm = TRUE)
      } else {
        graph_labels <-                               # data = sup_vars_coord,ggplot2::aes(color = colorvar_names)
          ggrepel::geom_label_repel(size = text_size, fontface = "bold", na.rm = TRUE,
                                    direction = "both", #segment.alpha = 0.5,
                                    min.segment.length = 0.5, arrow = ggplot2::arrow(length = ggplot2::unit(0.25, "lines"))) #point.padding = 0, segment.colour = "black"
      }
      plot_output <- ggplot2::ggplot(sup_vars_coord, ggplot2::aes(x = !!dim1, y = !!dim2, label = noms, color = colorvar, data_id = id)) +
        graph_theme_acm + profiles + active_graph + graph_labels + mean_point_graph


    } else if (type[1] == "text") {
      if (text_repel == FALSE) {
        graph_text <-
          ggiraph::geom_text_interactive(ggplot2::aes(tooltip = interactive_text), #data = sup_vars_coord, colorvar_names
                                         size = text_size, fontface = "bold", na.rm = TRUE)
      } else {
        graph_text <-
          ggrepel::geom_text_repel(ggplot2::aes(fontface = face), size = text_size, na.rm = TRUE, #fontface = "bold"
                                   direction = "both", # segment.alpha = 0.5,
                                   min.segment.length = 0.4, arrow = ggplot2::arrow(length = ggplot2::unit(0.25, "lines"))) # point.padding = 0.25, segment.colour = "black",
      }
      plot_output <- ggplot2::ggplot(sup_vars_coord, ggplot2::aes(x = !!dim1, y = !!dim2, label = noms, color = colorvar, data_id = id)) +
        graph_theme_acm + profiles + active_graph + graph_text + mean_point_graph


    } else if (type[1] == "numbers") {
      plot_output <-
        ggplot2::ggplot(sup_vars_coord, ggplot2::aes(x = !!dim1, y = !!dim2, tooltip = interactive_text, data_id = id)) +
        graph_theme_acm + profiles +
        ggiraph::geom_label_interactive(data = active_var_coord,
                                        ggplot2::aes(x = !!dim1, y = !!dim2, label = noms, tooltip = interactive_text, data_id = id + 1000),
                                        size = text_size, color = "black", na.rm = TRUE, inherit.aes = FALSE) +
        ggiraph::geom_text_interactive(ggplot2::aes(label = noms, color = colorvar),  #colorvar_names
                                       size = text_size/1.2, hjust = "left", nudge_x = dist_labels[1], na.rm = TRUE) + #fontface = "bold"
        ggiraph::geom_label_interactive(ggplot2::aes(label = numbers, color = colorvar),
                                        size = text_size*1.2, fontface = "bold", na.rm = TRUE) +
        mean_point_graph


    } else { stop('unknown type of graph') }

    #Add informations in the ggplot2::ggplot object, to be used into ggout() (without losing ggplot2::ggplot class)
    plot_output %<>% append(c("heigth_width_ratio" = heigth_width_ratio)) %>%
      `attr<-`("class", c("gg", "ggplot"))

    return(plot_output)
  }

# ggmca(res.mca, sup_vars = c("NBSALAacm", "DIPLOMEacm", "TPSINFOacm"),
#              dist_labels = 0.04, names_darker = TRUE) %>% ggout()
#
# ggmca(res.mca, sup_vars = c("PE0"), nb_char_for_color = 1,
#              dist_labels = 0.04, names_darker = TRUE) %>% ggout()

# ggmca(res.mca, split_var = PE, dplyr::filter = "^62",
#              , type="normal") +
#   scale_color_discrete()  + ylim(-0.3,NA)

# ggmca(res.mca, split_var = PE0, nb_char_for_color = 1,
#              dist_labels = 0.04, type = "ggplotly") +
#   ylim(NA, 1) + xlim (NA, 1.2); ggout("ggplotly")
#
# ggmca(res.mca, split_var = PE3_ord, dplyr::filter = "^.3C",
#              nb_char_for_color = 1,
#              dist_labels = 0.04, type = "ggplotly") +
#   ylim(NA, 0) + xlim (NA, 1) ; ggout("ggplotly")





#Fonction : tracer le graphe pondéré des individus en triant selon 1 ou 2 variables
#Type "dodge" : plus lisible car les points sur la même "case" sont décalés
#Type "facet" : un graphe des individus pour chaque modalité d'une variable
#Type "profiles" : profils de réponse représentés dans des lettres (def plus haut)
#Problèmes : - Profiles need ggmca_abrev(), specific to ct2013acm
#            - Need tooltips to explore the graph of individuals (interactive by point ; hover by color)
#            - ggmca_ellipses on the same function ?
#            - Split vars and levels crossing : make standard approach for structured data analysis before coding anything
#            -

#' Graph of Individuals for MCA
#' @description To be enhanced.
#' @param res.mca An object created with \code{FactoMineR::\link[FactoMineR]{MCA}}.
# @param split_var
# @param split_var2
# @param axes
# @param type
# @param ellipses
# @param percent
# @param max_profiles
#'
# @return
#' @export
#'
# @examples
ggmca_ind <- function(res.mca = res.mca, split_var, split_var2, axes = c(1,2), # choose_letter_for_each_active_level
                      type = c("dodge", "facet", "profiles", "profiles_nb"),
                      ellipses = FALSE, percent = 0.95,
                      max_profiles = 200) {
  res.mca <- res.mca
  dim1 <- rlang::sym(stringr::str_c("Dim ", axes[1])) #rlang::expr(eval(parse(text = paste0("`Dim ", axes[1],"`"))))
  dim2 <- rlang::sym(stringr::str_c("Dim ", axes[2])) #rlang::expr(eval(parse(text = paste0("`Dim ", axes[2],"`"))))

  if (missing(split_var)){
    split_var <- NULL

  } else { #S'il y a au moins une variable précisée
    split_var <- rlang::enquo(split_var)
    levels_split_var <- levels( #Attention dans res.mca l'ordre n'est pas bon
      eval(parse(text = paste0(res.mca$call$call$X[[2]],"$",rlang::as_label(split_var)))))
  }

  if (missing(split_var2)) {
    split_var2 <- NULL
  } else { #Si la deuxième variable est précisée
    split_var2 <- rlang::enquo(split_var2)
    levels_split_var2 <- levels( #Attention dans res.mca l'ordre n'est pas bon
      eval(parse(text = paste0(res.mca$call$call$X[[2]],"$",rlang::as_label(split_var2)))))
  }






  #Calculer les profils de réponse de l'ACM ; pondérés : nb d'individus × pond
  dat_profils <- res.mca$call$X %>%
    tibble::add_column(row.w = res.mca$call$row.w)
  dat_profils <-
    dat_profils[,c(res.mca$call$quali, which(colnames(dat_profils)=="row.w"),
                   which(colnames(dat_profils)=="cah"),
                   which(colnames(dat_profils)==rlang::as_label(split_var)),
                   which(colnames(dat_profils)==rlang::as_label(split_var2))
    )] %>%
    dplyr::group_by_at(dplyr::vars(-row.w)) %>%
    dplyr::summarise(pondpr = sum(row.w)) %>%
    dplyr::arrange(-pondpr) %>% dplyr::ungroup %>%
    dplyr::mutate(row_n = dplyr::row_number(dplyr::desc(pondpr)))
  #Refaire l'ACM avec les profils pondérés :
  res.mca.profils <-
    FactoMineR::MCA(dat_profils[,1:(which(colnames(dat_profils)=="pondpr" )-1)],
                    quali.sup = (length(res.mca$call$quali)+1):(which(colnames(dat_profils)=="pondpr" )-1),
                    row.w = dat_profils$pondpr, ncp = res.mca$call$ncp,
                    graph = FALSE)

  if ((type %in% c("profiles", "profiles_nb"))){
    #Créer une variable abrev avec l'abréviation des profils
    dat_profils <- dat_profils %>%
      ggmca_abrev
    #Si l'affichage des labels de profils se fait par catégories,
    # les points eux ne sont affichés qu'une seule fois
    dat_profils_uniques <- res.mca$call$X[res.mca$call$quali] %>%
      tibble::add_column(row.w = res.mca$call$row.w) %>%
      dplyr::group_by_at(dplyr::vars(-row.w)) %>%
      dplyr::summarise(pondpr = sum(row.w)) %>%
      dplyr::arrange(-pondpr)
    res.mca.profils.uniques <-
      FactoMineR::MCA(dat_profils_uniques[,1:(which(colnames(dat_profils_uniques)=="pondpr" )-1)],
                      row.w = dat_profils_uniques$pondpr, ncp = res.mca$call$ncp,
                      graph = FALSE)
    unique_profiles_coord <- dat_profils_uniques %>%
      dplyr::bind_cols(tibble::as_tibble(res.mca.profils.uniques$ind$coord))
    # Afficher des informations de manière interactive quand on survole un point :
    quali_var_names <- stringr::str_c(colnames(
      res.mca.profils.uniques$call$X)[1:length(res.mca.profils.uniques$call$quali)])
    unique_profiles_coord %<>%
      dplyr::mutate(interactive_text = stringr::str_c("Effectifs : ", round(pondpr, 0), "\n"))
    for (i in 1:length(res.mca$call$quali)) {
      unique_profiles_coord %<>%
        dplyr::mutate(interactive_text = stringr::str_c(interactive_text, "\n",
                                                        eval(str2expression(quali_var_names[i]))) )
    }
  }







  #Choisir individus et variables nécessaires pour tracer points et ellipses :
  ind_coord <- dat_profils %>% dplyr::bind_cols(tibble::as_tibble(res.mca.profils$ind$coord))
  # Afficher des informations de manière interactive quand on survole un point :
  quali_var_names <- stringr::str_c(colnames(res.mca$call$X)[1:length(res.mca$call$quali)])
  ind_coord %<>%
    dplyr::mutate(interactive_text = stringr::str_c("Effectifs : ", round(pondpr, 0), "\n"))
  #res.mca.profils$ind$coord %>% tibble::as_tibble %>%
  #dplyr::bind_cols(dplyr::select(dat_profils, pondpr, row_n))
  for (i in 1:length(res.mca$call$quali)) {
    ind_coord %<>%
      dplyr::mutate(interactive_text = stringr::str_c(interactive_text, "\n",
                                                      eval(str2expression(quali_var_names[i]))) )
  }
  if (ellipses == TRUE) {ellipses_ind_coord <- res.mca$ind$coord %>% tibble::as_tibble}

  # if (type %in% c("profiles", "profiles_nb")){
  #   ind_coord %<>% dplyr::bind_cols(dplyr::select(dat_profils, abrev))
  # }

  if (missing(split_var2) | is.null(split_var2)) {
    if (missing(split_var) | is.null(split_var)) {
      # S'il n'y a aucune variable précisée
      split_var_coord <- tibble::tibble(`Dim 1` = 0,`Dim 2` = 0, `Dim 3` = 0,`Dim 4` = 0,
                                        `Dim 5` = 0,`Dim 6` = 0,`Dim 7` = 0,`Dim 8` = 0)

      if (type %in% c("profiles", "profiles_nb")) {
        group_var <- NULL
      } else {
        group_var <- "Ensemble"
      }
      title_var <- ""

    } else {
      #S'il n'y a qu'une variable précisée
      ind_coord %<>% #dplyr::bind_cols(dplyr::select(dat_profils, !!split_var)) %>%
        dplyr::filter(!stringr::str_detect(!!split_var,".NA")) %>%
        dplyr::mutate(!!split_var := forcats::fct_relevel(!!split_var,levels_split_var)) # Bon ordre
      if (!(rlang::as_label(split_var) %in% colnames(res.mca$call$X)[1:length(res.mca$call$quali)])) {
        ind_coord %<>% dplyr::mutate(interactive_text = stringr::str_c(interactive_text, "\n\n", !!split_var))
      }

      split_var_coord <- res.mca$quali.sup$coord %>%
        tibble::as_tibble (rownames = rlang::as_label(split_var)) %>%
        dplyr::filter(!!split_var %in% levels_split_var) %>%
        dplyr::mutate(!!split_var := forcats::fct_relevel(!!split_var,levels_split_var)) %>% #Bon ordre
        dplyr::arrange(!!split_var)

      if (ellipses == TRUE) {
        ellipses_ind_coord %<>% dplyr::bind_cols(dplyr::select(tibble::as_tibble(res.mca$call$X),
                                                               !!split_var)) %>%
          dplyr::filter(!stringr::str_detect(!!split_var,".NA")) %>%
          dplyr::mutate(!!split_var := forcats::fct_relevel(!!split_var,levels_split_var))
      }

      group_var <- split_var
      title_var <- paste0(" décomposé selon ", rlang::as_label(split_var))
    }

  } else {
    #Si l'utilisateur a précisé une 2e variable : croiser avec la 1ère
    ind_coord %<>% #dplyr::bind_cols(dplyr::select(dat_profils, !!split_var, !!split_var2)) %>%
      dplyr::filter(!stringr::str_detect(!!split_var,".NA")) %>%
      dplyr::filter(!stringr::str_detect(!!split_var2,".NA")) %>%
      dplyr::mutate(!!split_var := forcats::fct_relevel(!!split_var,levels_split_var)) %>%
      dplyr::mutate(!!split_var2 := forcats::fct_relevel(!!split_var2,levels_split_var2)) # Bon ordre
    if (!(rlang::as_label(split_var) %in% colnames(
      res.mca$call$X)[1:length(res.mca$call$quali)])) {
      ind_coord %<>% dplyr::mutate(interactive_text = stringr::str_c(interactive_text, "\n\n", !!split_var))
    }
    if (!(rlang::as_label(split_var2) %in% colnames(
      res.mca$call$X)[1:length(res.mca$call$quali)])) {
      ind_coord %<>% dplyr::mutate(interactive_text = stringr::str_c(interactive_text, "\n", !!split_var2))
    }

    if (ellipses == TRUE) {
      ellipses_ind_coord %<>% dplyr::bind_cols(dplyr::select(tibble::as_tibble(res.mca$call$X),
                                                             !!split_var, !!split_var2)) %>%
        dplyr::filter(!stringr::str_detect(!!split_var,".NA")) %>%
        dplyr::filter(!stringr::str_detect(!!split_var2,".NA")) %>%
        dplyr::mutate(!!split_var := forcats::fct_relevel(!!split_var,levels_split_var)) %>%
        dplyr::mutate(!!split_var2 := forcats::fct_relevel(!!split_var2,levels_split_var2))
    }

    if (length(levels_split_var) >=9 | length(levels_split_var2) >=9 ) {
      stop('One of the two variables has more than 8 levels.')
    }

    code_lines <- "%<>% dplyr::mutate(crossvar = dplyr::case_when("
    for (i in 1:length(levels_split_var)) {
      for (j in 1:length(levels_split_var2)) {
        new_line <- paste0("!!split_var == levels_split_var[",i,
                           "] & !!split_var2 == levels_split_var2[",j,
                           "] ~ paste0( levels_split_var[",i,
                           "], '*', levels_split_var2[",j,"]),")
        code_lines <- paste0(code_lines, new_line)
      }
    }
    code_lines %<>% stringr::str_c("))") %>% stringr::str_replace("\\)\\,\\)\\)",")))")

    eval(parse(text = paste0("ind_coord ", code_lines)))
    ind_coord %<>% dplyr::mutate(crossvar = forcats::fct_drop(crossvar))

    if (ellipses == TRUE) {
      eval(parse(text = paste0("ellipses_ind_coord ", code_lines)))
      ellipses_ind_coord %<>% dplyr::mutate(crossvar = forcats::fct_drop(crossvar))
    }

    #Calculer les points-modalités de la nouvelle variable :
    dat_profils %<>%
      dplyr::filter(!stringr::str_detect(!!split_var,".NA")) %>%
      dplyr::filter(!stringr::str_detect(!!split_var2,".NA")) %>%
      tibble::add_column(crossvar = ind_coord$crossvar, .before = "pondpr")
    vars_by_crossvar <-
      dat_profils %>%
      dplyr::group_by(!!split_var, !!split_var2, crossvar) %>%
      dplyr::summarise(n=n()) %>% dplyr::select(-n)
    dat_profils %<>% dplyr::select(-!!split_var, -!!split_var2, -cah)
    res.mca.profils <-
      FactoMineR::MCA(dat_profils[,1:(which(colnames(dat_profils)=="pondpr" )-1)],
                      quali.sup = (length(res.mca$call$quali)+1):(which(colnames(dat_profils)=="pondpr" )-1),
                      row.w = dat_profils$pondpr, ncp = res.mca$call$ncp,
                      graph = FALSE)
    split_var_coord <-
      res.mca.profils$quali.sup$coord %>%
      tibble::as_tibble (rownames = "crossvar") %>% dplyr::bind_cols(vars_by_crossvar)
    # dplyr::filter(crossvar %in% levels(ind_coord$crossvar)) %>%
    # dplyr::mutate(crossvar = forcats::fct_relevel(crossvar,levels(crossvar))) %>% #Bon ordre
    # dplyr::arrange(crossvar)
    if ( all(split_var_coord$crossvar == vars_by_crossvar$crossvar) == FALSE ) {
      stop('Points-modalités dans le mauvais ordre')
    }

    group_var <- rlang::expr(crossvar)
    title_var <- paste0(" décomposé selon ", rlang::as_label(split_var),
                        " et ", rlang::as_label(split_var2))
  }

  if (missing(split_var)|is.null(split_var)) { #S'il n'y a aucune variable
    ind_coord %<>% dplyr::mutate(pond_group = pondpr/sum(pondpr))
  } else {
    ind_coord %<>% #Prop d'individus dans chaque catégorie (pour taille des points)
      dplyr::group_by(!!group_var) %>%
      dplyr::mutate(pond_group = pondpr/sum(pondpr)) %>%
      dplyr::ungroup()
  }
  ind_profiles_coord <- ind_coord %>% dplyr::slice (1:max_profiles)







  #Préparer les ellipses et choisir leur couleur (selon le type de graphe) :
  if (ellipses == TRUE) {
    if (type == "facet") {
      ellipses <- ggplot2::stat_ellipse(data = ellipses_ind_coord,
                                        ggplot2::aes(x = !!dim1, y = !!dim2, group = !!group_var),
                                        color = "black", type = "t", level = percent, size = 1,
                                        segments = 360, alpha = 1)
    } else {
      ellipses <- ggplot2::stat_ellipse(data = ellipses_ind_coord,
                                        ggplot2::aes(x = !!dim1, y = !!dim2, group = !!group_var,
                                                     color = !!group_var),
                                        type = "t", level = percent, size = 1,
                                        segments = 360, alpha = 1)
    }
  } else {
    ellipses <- NULL
  }





  #Tracer les graphiques en fonction de leur type :
  if (type == "dodge") {
    ggplot2::ggplot() +
      theme_mca(res = res.mca, axes = axes) +
      ggplot2::labs(title = paste0("Graphe des individus (axes ", axes[1], " et ", axes[2],
                                   ")", title_var
                                   # "(ellipses à ", ellipse_percent*100,")"
      )) +
      #Coordonnées des individus :
      ggplot2::geom_point(data = ind_coord, #Taille points : proportion dans le total
                          ggplot2::aes(x = !!dim1, y = !!dim2, color = !!group_var, size = pondpr,
                                       text = interactive_text),
                          position = ggplot2::position_dodge(width = 0.2), na.rm = TRUE) +
      ellipses +
      #Points moyens des modalités :
      ggplot2::geom_label(data = split_var_coord,
                          ggplot2::aes(x = !!dim1, y = !!dim2, label = !!group_var, color = !!group_var),
                          size = 4, fontface = "bold", na.rm = TRUE) #Pour décaler légèrement : nudge_y = 0.1
  } else if (type == "facet") {

    if (missing(split_var2) | is.null(split_var2)) {
      facets <- ggplot2::facet_wrap(dplyr::vars(!!split_var), scales = "fixed")
    } else {
      facets <- ggplot2::facet_grid(dplyr::vars(!!split_var2), dplyr::vars(!!split_var), scales = "fixed")
    }

    ggplot2::ggplot() +
      theme_mca(res = res.mca, axes = axes) +
      ggplot2::labs(title = paste0("Graphe des individus (axes ", axes[1], " et ", axes[2],
                                   ")", title_var)) +
      #Coordonnées des individus :
      ggplot2::geom_point(data = ind_coord, #Taille points : proportion dans chaque groupe
                          ggplot2::aes(x = !!dim1, y = !!dim2, color = !!group_var, size = pond_group,
                                       group = !!group_var, text = interactive_text), na.rm = TRUE) +
      ellipses +
      #Points moyens des modalités :
      ggplot2::geom_point(data = split_var_coord,
                          ggplot2::aes(x = !!dim1, y = !!dim2, group = !!group_var),
                          shape = 17, size = 6, color = "black", na.rm = TRUE) +
      facets
  } else if (type == "profiles_nb") {

    ggplot2::ggplot() +
      ggplot2::theme_mca(res = res.mca, axes = axes, no_size_scale = TRUE) +
      ggplot2::labs(title = paste0("Graphe des profils de réponses (axes ",
                                   axes[1], " et ", axes[2], ")", title_var)) +
      #Coordonnées des individus :
      ggplot2::geom_point(data = unique_profiles_coord,
                          ggplot2::aes(x = !!dim1, y = !!dim2, size = pondpr, text = interactive_text),
                          shape = 21, color = "transparent", fill = "tomato", alpha = 0.8,
                          show.legend = NA, na.rm = TRUE) +
      ellipses +
      ggplot2::geom_text(data = ind_coord,
                         ggplot2::aes(x = !!dim1, y = !!dim2, label = row_n),
                         size = 4, na.rm = TRUE) +
      ggplot2::scale_size_area(max_size =  30) +  #Échelle de taille des points
      ggplot2::theme(legend.position = "right") #+
    #Points moyens des modalités :
    # geom_label(data = split_var_coord,
    #            ggplot2::aes(x = !!dim1, y = !!dim2, label = !!split_var, color = !!split_var),
    #            size = 4, fontface = "bold")
  } else if (type =="profiles") {

    ggplot2::ggplot() +
      theme_mca(res = res.mca, axes = axes, no_size_scale = TRUE) +
      ggplot2::labs(title = paste0("Graphe des profils de réponses (axes ",
                                   axes[1], " et ", axes[2], ")", title_var)) +
      #Coordonnées des individus :
      ggplot2:: geom_point(data = unique_profiles_coord,
                           ggplot2::aes(x = !!dim1, y = !!dim2, size = pondpr, text = interactive_text),
                           shape = 21, color = "transparent", fill = "black", alpha = 0.1,
                           show.legend = NA, na.rm = TRUE) +
      ellipses +
      ggplot2::geom_text(data = ind_profiles_coord,
                         ggplot2::aes(x = !!dim1, y = !!dim2, label = abrev,
                                      color = !!group_var),
                         size = 3.5, fontface = "bold", na.rm = TRUE) +
      ggplot2::scale_size_area(max_size =  30) +  #Échelle de taille des points
      ggplot2::theme(legend.position = "right")
  } else {
    stop('unknown type')
  }

}


#Exemples :
#  ggmca_ind(res.mca, PR0, type = "facet", ellipses = TRUE) &
#    ggout()
# ggmca_ind(res.mca, CSER, axes = c(1,2),
#              type = "dodge") ; ggout("ggplotly")
# ggmca_ind(res.mca, CSER, axes = c(1,2), type = "facet", ellipses = TRUE) ; ggout()
# ggmca_ind(res.mca, cah, axes = c(1,2), type = "dodge") ; ggout()
# ggmca_ind(res.mca, cah, axes = c(1,2), type = "facet", ellipses = TRUE,
#              percent = 0.95) ; ggout() # Percent : choisir le % de points dans l'ellipse
# ggmca_ind(res.mca, CSER, STATUTacm, axes = c(1,2),
#              type = "facet", ellipses = TRUE) +
#   scale_color_discrete() ; ggout() #Utile quand il y a plus de 8 couleurs à mettre
#   #Voir les profils de réponses (classés par variables actives ou selon la CAH) :
# ggmca_ind(res.mca, type = "profiles_nb") ; ggout()
# ggmca_ind(res.mca, cah, type = "profiles",
#              max_profiles = 225, ellipses = TRUE) ; ggout("ggplotly")
# ggmca_ind(res.mca, OBJVRAIacm, type = "profiles") ; ggout()
# ggmca_ind(res.mca, OBJVRAIacm, EVACRITacm, type = "profiles",
#              max_profiles = 350) ; ggout()
# ggmca_ind(res.mca, cah, type = "profiles") ; ggout() # OK avec CAH
# ggmca_ind(res.mca, cah, type = "profiles", ellipses = TRUE) ; ggout()
# # Vars non actives : les couleurs ne veulent rien dire (dernier texte superposé gagne)
# ggmca_ind(res.mca, RWDEMacm, SEXE, type = "profiles"); ggout()
#
#   #On peut ajouter d'autres éléments ggplot2 à la main avec "+" :
# ggmca_ind(res.mca, cah, axes = c(1,2), type = "dodge") +
#   scale_color_viridis(discrete = TRUE, option = "D") ; ggout() #pb = couleur hiérarchisée
#   #Zoomer sur la partie inférieure gauche (direction par objectifs) :
# ggmca_ind(res.mca, cah, type = "profiles", max_profiles = 225) +
#   ylim(NA,0) +  xlim(-0.5,1.3) + theme(legend.position = "none") ; ggout()
#   #Zoomer sur la partie supérieure gauche (travail à la chaîne) :
# ggmca_ind(res.mca, cah, type = "profiles", max_profiles = 500) +
#   ylim(0,NA) +  xlim(-0.2,1.5) + theme(legend.position = "none") ; ggout()
#   #Zoomer au milieu :
# ggmca_ind(res.mca, cah, type = "profiles", max_profiles = 500) +
#   ylim(-0.4,0.5) +  xlim(-0.4,0.6) + theme(legend.position = "none") ; ggout()









#Fonction : graphe avec centres de classes, ellipses médianes et modalités:
#Les graphe pondéré des individus apparaît en fond de trame

#' Graph of Individuals with Concentration Ellipses for MCA
#' @description To be enhanced.
# @param res.mca An object created with \code{FactoMineR::\link[FactoMineR]{MCA}}.
# @param split_var
# @param percent
# @param axes
# @param type
#
# @return
#' @export
#'
# @examples
ggmca_ellipses <- function(res.mca = res.mca, split_var = NULL,
                           percent = 0.95, axes = c(1,2), type = "normal") {
  res.mca <- res.mca
  split_var <- rlang::enquo(split_var)
  dim1 <- rlang::sym(stringr::str_c("Dim ", axes[1])) #rlang::expr(eval(parse(text = paste0("`Dim ", axes[1],"`"))))
  dim2 <- rlang::sym(stringr::str_c("Dim ", axes[2])) #rlang::expr(eval(parse(text = paste0("`Dim ", axes[2],"`"))))

  levels_split_var <- levels( #Attention dans res.mca l'ordre n'est pas bon
    eval(parse(text = paste0(res.mca$call$call$X[[2]],"$",rlang::as_label(split_var)))))
  split_var_coord <- res.mca$quali.sup$coord %>%
    tibble::as_tibble (rownames = rlang::as_label(split_var)) %>%
    dplyr::filter(!!split_var %in% levels_split_var) %>%
    dplyr::mutate(!!split_var := forcats::fct_relevel(!!split_var,levels_split_var)) %>% #Bon ordre
    dplyr::arrange(!!split_var)

  active_var_coord <- res.mca$var$coord %>%
    tibble::as_tibble(rownames = "noms")

  #Calculer les profils de réponse de l'ACM ; pondérés : nb d'individus × pond
  res.mca$call$X %<>% tibble::add_column(row.w = res.mca$call$row.w)
  dat_profils <-
    res.mca$call$X[,c(res.mca$call$quali,
                      which(colnames(res.mca$call$X)=="row.w"))] %>%
    dplyr::group_by_at(dplyr::vars(-row.w)) %>%
    dplyr::summarise(pondpr = sum(row.w)) %>%
    dplyr::arrange(-pondpr) %>% dplyr::ungroup
  res.mca.profils <-   #Refaire l'ACM avec les profils pondérés :
    FactoMineR::MCA(dat_profils[,1:(which(colnames(ct2013profils)=="pondpr" )-1)],
                    row.w = dat_profils$pondpr, ncp = res.mca$call$ncp,
                    graph = FALSE)
  ind_coord <- dat_profils %>% dplyr::select(pondpr) %>%
    dplyr::bind_cols( tibble::as_tibble(res.mca.profils$ind$coord) )


  ellipses_ind_coord <- res.mca$call$X %>% dplyr::select(!!split_var) %>%
    dplyr::bind_cols( tibble::as_tibble(res.mca$ind$coord) ) %>%
    dplyr::filter(!stringr::str_detect(!!split_var,".NA")) %>%
    dplyr::mutate(!!split_var := forcats::fct_relevel(!!split_var,levels_split_var)) #Bon ordre

  if (type == "normal") {

    ggplot2::ggplot() +
      theme_mca(res = res.mca, axes = axes) +
      ggplot2::labs(title = paste0("Les catégories de la variable ", rlang::as_label(split_var),
                                   " sur les axes ", axes[1], " et ", axes[2],
                                   " : ellipses à ",
                                   percent*100, "% et points-modalités")) +
      ggplot2::geom_point(data = ind_coord,
                          ggplot2::aes(x = !!dim1, y = !!dim2, size = pondpr),
                          alpha = 0.05, na.rm = TRUE) +
      ggplot2::stat_ellipse(data = ellipses_ind_coord,
                            ggplot2::aes(x = !!dim1, y = !!dim2, color=!!split_var),
                            type = "t", level = percent, size = 1,  segments = 360,
                            alpha = 0.5, na.rm = TRUE) +
      ggplot2::geom_label(data = active_var_coord,
                          ggplot2::aes(x = !!dim1, y = !!dim2, label = noms),
                          color = "black", na.rm = TRUE) +
      ggplot2::geom_point(data = split_var_coord, size = 3,
                          ggplot2::aes(x = !!dim1, y = !!dim2, color = !!split_var), na.rm = TRUE) +
      ggrepel::geom_label_repel(data = split_var_coord,
                                ggplot2::aes(x = !!dim1, y = !!dim2, label = !!split_var,
                                             color = !!split_var),
                                fontface = "bold", na.rm = TRUE)
  } else if (type == "multi") {

    #Variante avec trois ellipses imbriquées colorées à l'intérieur :
    ggplot2::ggplot() +
      theme_mca(res = res.mca, axes = axes) +
      ggplot2::labs(title = paste0("Les catégories de la variable ", rlang::as_label(split_var),
                                   "sur les axes ", axes[1], " et ", axes[2],
                                   " : ellipses (à 50%, 86,5% et 95%) et points-modalités")) +
      # geom_point(data = ind_coord,
      #            ggplot2::aes(x = !!dim1, y = !!dim2, size = pondpr),
      #            alpha = 0.05, na.rm = TRUE) +
      ggplot2::stat_ellipse(data = ellipses_ind_coord,
                            ggplot2::aes(x = !!dim1, y = !!dim2, fill = !!split_var),
                            geom = "polygon", type = "t", level = 0.5, size = 1,
                            segments = 360, alpha = 0.8, na.rm = TRUE) +
      ggplot2::stat_ellipse(data = ellipses_ind_coord,
                            ggplot2::aes(x = !!dim1, y = !!dim2, fill = !!split_var),
                            geom = "polygon", type = "t", level = 0.865, size = 1,
                            segments = 360, alpha = 0.1, na.rm = TRUE) +
      ggplot2::stat_ellipse(data = ellipses_ind_coord,
                            ggplot2::aes(x = !!dim1, y = !!dim2, fill = !!split_var),
                            geom = "polygon", type = "t", level = 0.95, size = 1,
                            segments = 360, alpha = 0.05, na.rm = TRUE) +
      ggplot2::geom_label(data = active_var_coord,
                          ggplot2::aes(x = !!dim1, y = !!dim2, label = noms),
                          na.rm = TRUE, color = "black") +
      # geom_point(data = split_var_coord, size = 3,
      #            ggplot2::aes(x = !!dim1, y = !!dim2, color = !!split_var)) +
      ggplot2::geom_label(data = split_var_coord,
                          ggplot2::aes(x = !!dim1, y = !!dim2, label = !!split_var,
                                       color = !!split_var), fontface = "bold", na.rm = TRUE)
  } else {
    stop('unknown type of ellipse')
  }

  # #Test de la taille des ellipses de concentration dans factominer :
  # # ce sont des 95%
  # FactoMineR::MCA(ct2013acm[,c(1:9, which(colnames(ct2013acm)=="cah"))],
  #     quali.sup = 10, row.w = ct2013acm$pondcal, ncp = 8, graph = FALSE) %>%
  #   fviz_mca_biplot(ggtheme = theme_minimal(), geom.ind = "point",
  #                   geom.var = "text", col.ind = res.mca$call$X$cah,
  #                   alpha.ind = 0.001, palette = "Dark2", col.var = "black",
  #                   select.quali.sup = "cah", addEllipses = TRUE
  #   ) + coord_fixed()
}

# #Exemple des ellipses des classes de la meilleure classification :
# ggmca_ellipses(res.mca, split_var = cah,
#                  percent = 0.5, axes=c(1,2), type="normal") ; ggout()
# #Exemple avec CSER :
# ggmca_ellipses(res.mca, split_var = CSER,
#                   percent = 0.95, axes=c(1,2), type="normal") ; ggout()
# ggmca_ellipses(res.mca, split_var = PR0, axes = c(1,2),
#                   percent = 0.95, type ="normal") ; ggout()
# ggmca_ellipses(res.mca, split_var = PR1, axes = c(1,2),
#                    percent = 0.95, type ="normal") ; ggout()








# # dat <- ct2013acm
# # dat <- res.mca$call$X %>% tibble::add_column(row.w = res.mca$call$row.w)
# sup_vars = c(sup_vars_contraintes, "PE0")
# axes = c(1,2)
# nb_char_for_color = 1
# keep_levels = character()
# discard_levels = character()
# tooltip_vars <- c("SEXE", "cah")
# cleannames = TRUE
# names_darker = FALSE
# shift_colors = 0













# axes = c(1,2)
# filter = ""
# uppercase = "col"
# show_sup = FALSE
# tooltips = "row"
# rowtips_subtitle = "% en ligne"
# coltips_subtitle = "% en colonne"
# rowcolor_numbers = 0
# colcolor_numbers = 0
# dist_labels = 0.12
# text_size = 3.5
# right_margin = 0
# size_scale_max = 8





#Améliorations :
# - Option voire les croisements (par ex employées NQ des particuliers) ?
# - Analyse de données structurées
# - Feature request ggrepel : toutes les flèches en dessous des labels/texts----
# - Améliorer la façon dont tabw permet d’amener un tableau croisé dans ggca----

#' Readable, Interactive and Beautiful Graph for CA
#' @description A readable, complete and beautiful graph for simple
#' correspondence analysis made with \code{FactoMineR::\link[FactoMineR]{CA}}.
#' Interactive tooltips, appearing when hovering on  points with mouse, allow to
#' keep in mind all the content of the table while reading the graph. Since it is
#' made in the spirit of \code{\link{ggplot2}}, it is possible to change
#' theme or add another plot elements with +. Then, interactive
#' tooltips won't appear until you pass the result throught \code{\link{ggout}}.
#'
#' @param res.ca An object created with \code{FactoMineR::\link[FactoMineR]{CA}}.
#' @param axes The axes to print, as a numeric vector of length 2.
#' @param show_sup When \code{TRUE} show supplementary rows and cols.
#' @param xlim,ylim Horizontal and vertical axes limits,
#' as double vectors of length 2.
#' @param out_lims_move When \code{TRUE}, the points out of \code{xlim} or
#'  \code{ylim} are not removed, but moved at the edges of the graph.
#' @param type Determines the way the two variables of the table are printed.
#'    \itemize{
#'    \item \code{"points"} : colored points with text legends
#'    \item \code{"text"} : colored text
#'    \item \code{"labels"} : colored labels
#'  }
#' @param text_repel When \code{TRUE} the graph is not interactive anymore,
#'  but the resulting image is better to print because points and labels don't
#'  overlaps. It uses \code{ggrepel::\link[ggrepel]{geom_text_repel}}.
#' @param uppercase Print \code{"row"} var or \code{"col"} var labels with
#' uppercase.
#' @param tooltips Choose the content of interactive tooltips at mouse hover :
#'  \code{"col"} for the table of colums percentages, \code{"row"} for line
#'  percentages, \code{c("row", "col")} for both.
#' @param rowtips_subtitle,coltips_subtitle The subtitles used before the table
#' in interactive tooltips.
#' @param rowcolor_numbers,colcolor_numbers  If row var or col var levels are
#' prefixed with numbers(ex. : \code{"1-"} ), the number of digits to use
#' to create classes that will be used to add colors to points.
#' @param cleannames Set to \code{TRUE} to clean levels names, by removing
#' prefix numbers like \code{"1-"}, and text in parentheses.
#' @param filter Regex patterns to discard levels of row or col variables.
#' @param title The title of the graph.
#' @param text_size Size of text.
#' @param dist_labels When \code{type = "points"}, the distance of text and
#' labels from points.
#' @param right_margin A margin at the right, in cm. Useful to read tooltips
#'  over points placed at the right of the graph without formatting problems.
#' @param size_scale_max Size of points.
#'
#' @return A \code{\link[ggplot2]{ggplot}} object to be printed in the
#' RStudio Plots pane. Possibility to add other gg objects with \code{+}.
#' Sending the result  through \code{\link{ggout}} will draw the
#' interactive graph in the Viewer pane using \code{\link{ggiraph}}.
#' @export
#'
# @examples
ggca <-
  function(res.ca = res.ca, axes = c(1,2), show_sup = FALSE, xlim, ylim, out_lims_move = FALSE,
           type = c("points", "text", "labels"), text_repel = FALSE, uppercase = "col",
           tooltips = "row", rowtips_subtitle = "% en ligne", coltips_subtitle = "% en colonne",
           rowcolor_numbers = 0, colcolor_numbers = 0, cleannames = TRUE, filter = "", title,
           text_size = 3.5, dist_labels = c("auto", 0.12), right_margin = 0, size_scale_max = 8) {  #, repel_max_iter = 10000

    dim1 <- rlang::sym(stringr::str_c("Dim ", axes[1])) #rlang::expr(eval(parse(text = paste0("`Dim ", axes[1],"`"))))
    dim2 <- rlang::sym(stringr::str_c("Dim ", axes[2]))  #rlang::expr(eval(parse(text = paste0("`Dim ", axes[2],"`"))))


    #Lignes :
    row_coord <- res.ca$row$coord %>% tibble::as_tibble (rownames = "noms") %>%
      dplyr::mutate(colorvar = "Active_row") %>%
      dplyr::bind_rows(res.ca$row.sup$coord %>%
                         tibble::as_tibble(rownames = "noms") %>%
                         dplyr::mutate(colorvar = "Sup_row") )
    row_coord %<>% dplyr::bind_cols(freq = rowSums(res.ca$call$Xtot) / sum(rowSums(res.ca$call$Xtot))) %>%
      dplyr::mutate(numbers = dplyr::case_when(
        stringr::str_detect(noms, "^[^- ]+-(?![[:lower:]])|^[^- ]+(?<![[:lower:]])-")
        ~ stringr::str_extract(noms, "^[^- ]+"),
        TRUE ~ "" ))

    # Enlever les mots entre parenthèses et les nombres
    if (cleannames == TRUE) row_coord %<>% dplyr::mutate(noms = stringr::str_remove_all(noms, cleannames_condition()))

    # Variable de couleur (colorvar) selon nb de caractères indiqués
    row_coord %<>% dplyr::mutate(row_colorvar = as.factor(stringr::str_sub(numbers, 1, rowcolor_numbers)))
    row_colorvar_recode <- levels(row_coord$row_colorvar)
    names(row_colorvar_recode) <- stringr::str_c(1:nlevels(row_coord$row_colorvar))
    row_coord %<>%
      dplyr::mutate(row_colorvar = forcats::fct_recode(row_colorvar, !!!row_colorvar_recode)) %>%
      dplyr::mutate(colorvar = ifelse(colorvar == "Sup_row", colorvar,
                                      stringr::str_c(colorvar, row_colorvar))) %>%
      dplyr::select(-row_colorvar) %>%
      # Afficher informations interactives au survol d'un point
      dplyr::mutate(interactive_text = stringr::str_c("<b>", noms, "</b>", "\n", "Effectifs : ",
                                                      round(freq*100, 0), "%"),
                    noms = stringr::str_replace_all(noms, "[^[:alnum:][:punct:]]", " ") %>% stringr::str_squish()  ) #[^[:alnum:]-/\\(\\)·%:.,;’?!]

    if ("row" %in% tooltips) {
      #Calculer les % par ligne (de la variable colonne)
      row_frequencies <- res.ca$call$Xtot %>% tibble::as_tibble() %>%
        tibble::add_row(!!!colSums(res.ca$call$Xtot))
      row_frequencies %<>% dplyr::mutate_all(~ ./rowSums(row_frequencies)) %>%
        dplyr::rename_all(~ stringr::str_remove_all(., cleannames_condition()))
      row_residuals <- row_frequencies %>%
        dplyr::mutate_all(~ . - .[nrow(row_frequencies)]) %>%
        dplyr::mutate_all(~ dplyr::case_when(
          round(.*100,0) >= 0 ~ stringr::str_c("+", round(.*100, 0), "%"),
          . < 0 ~ stringr::str_c(" -", round(abs(.)*100, 0), "%")
        )) %>% dplyr::slice(-nrow(row_frequencies))
      row_frequencies %<>%
        dplyr::slice(-nrow(row_frequencies)) %>%
        dplyr::mutate_all(~ stringr::str_c(round(.*100, 0), "%")) %>%
        dplyr::mutate_all(~dplyr::case_when(
          stringr::str_length(.) >= 3 ~ .,
          stringr::str_length(.) < 3 ~ stringr::str_c("  ",.),
        ))
      row_frequencies %<>%
        dplyr::bind_rows(row_residuals) %>%
        dplyr::mutate(number_of_rows = dplyr::row_number())
      row_frequencies %<>%
        dplyr::mutate_at(dplyr::vars(-number_of_rows),~dplyr::case_when(
          number_of_rows > nrow(row_frequencies)/2 ~ NA_character_,
          TRUE ~ stringr::str_c("(",.[number_of_rows + nrow(row_frequencies)/2],") ", .),
        )) %>%
        dplyr::slice(1:(nrow(row_frequencies)/2)) %>% dplyr::select(-number_of_rows)
      row_frequencies <- purrr::map_dfc(1:ncol(row_frequencies),
                                        ~dplyr::mutate_all(row_frequencies[.x],
                                                           function(.) stringr::str_c(colnames(row_frequencies)[.x], " : ", .)
                                        ))
      row_frequencies %<>% tidyr::unite(row_text, sep = "\n") %>% dplyr::pull(row_text)
      row_coord %<>%
        dplyr::mutate(interactive_text = stringr::str_c(
          interactive_text, "\n\n", rowtips_subtitle, " :\n", row_frequencies))
    }



    #Colonnes :
    col_coord <- res.ca$col$coord %>% tibble::as_tibble (rownames = "noms") %>%
      dplyr::mutate(colorvar = "Active_col") %>%
      dplyr::bind_rows(res.ca$col.sup$coord %>%
                         tibble::as_tibble(rownames = "noms") %>%
                         dplyr::mutate(colorvar = "Sup_col") ) %>%
      dplyr::bind_cols(freq = rowSums(t(res.ca$call$Xtot)) / sum(rowSums(t(res.ca$call$Xtot))))
    col_coord  %<>%
      dplyr::mutate(numbers = dplyr::case_when(
        stringr::str_detect(noms, "^[^- ]+-(?![[:lower:]])|^[^- ]+(?<![[:lower:]])-")
        ~ stringr::str_extract(noms, "^[^- ]+"),
        TRUE ~ "" ))

    # Enlever les mots entre parenthèses et les nombres
    if (cleannames == TRUE) col_coord %<>%  dplyr::mutate(noms = stringr::str_remove_all(noms, cleannames_condition()))

    # Variable de couleur (colorvar) selon nb de caractères indiqués
    col_coord %<>%  dplyr::mutate(col_colorvar = as.factor(stringr::str_sub(numbers, 1, colcolor_numbers)))
    col_colorvar_recode <- levels(col_coord$col_colorvar)
    names(col_colorvar_recode) <- stringr::str_c(1:nlevels(col_coord$col_colorvar))
    col_coord %<>%
      dplyr::mutate(col_colorvar = forcats::fct_recode(col_colorvar, !!!col_colorvar_recode)) %>%
      dplyr::mutate(colorvar = ifelse(colorvar == "Sup_col", colorvar,
                                      stringr::str_c(colorvar, col_colorvar))) %>%
      dplyr::select(-col_colorvar) %>%
      # Afficher informations interactives au survol d'un point
      dplyr::mutate(interactive_text = stringr::str_c("<b>", noms, "</b>", "\n", "Effectifs : ",
                                                      round(freq*100, 0), "%"),
                    noms = stringr::str_replace_all(noms, "[^[:alnum:][:punct:]]", " ") %>% stringr::str_squish()
      )


    if ("col" %in% tooltips) {
      # Calculer les % par colonne (de la variable en ligne)
      col_frequencies <- res.ca$call$Xtot %>% t %>% tibble::as_tibble() %>%
        tibble::add_row(!!!rowSums(res.ca$call$Xtot))
      col_frequencies %<>% dplyr::mutate_all(~ ./rowSums(col_frequencies)) %>%
        dplyr::rename_all(~ stringr::str_remove_all(., cleannames_condition()))
      col_residuals <- col_frequencies %>%
        dplyr::mutate_all(~ . - .[nrow(col_frequencies)]) %>%
        dplyr::mutate_all(~ dplyr::case_when(
          round(.*100,0) >= 0 ~ stringr::str_c("+", round(.*100, 0), "%"),
          . < 0 ~ stringr::str_c(" -", round(abs(.)*100, 0), "%")
        )) %>% dplyr::slice(-nrow(col_frequencies))
      col_frequencies %<>%
        dplyr::slice(-nrow(col_frequencies)) %>%
        dplyr::mutate_all(~ stringr::str_c(round(.*100, 0), "%")) %>%
        dplyr::mutate_all(~dplyr::case_when(
          stringr::str_length(.) >= 3 ~ .,
          stringr::str_length(.) < 3 ~ stringr::str_c("  ",.),
        ))
      col_frequencies %<>%
        dplyr::bind_rows(col_residuals) %>%
        dplyr::mutate(number_of_rows = dplyr::row_number())
      col_frequencies %<>%
        dplyr::mutate_at(dplyr::vars(-number_of_rows),~dplyr::case_when(
          number_of_rows > nrow(col_frequencies)/2 ~ NA_character_,
          TRUE ~ stringr::str_c("(",.[number_of_rows + nrow(col_frequencies)/2],") ", .),
        )) %>%
        dplyr::slice(1:(nrow(col_frequencies)/2)) %>% dplyr::select(-number_of_rows)
      col_frequencies <- purrr::map_dfc(1:ncol(col_frequencies),
                                        ~dplyr::mutate_all(col_frequencies[.x],
                                                           function(.) stringr::str_c(colnames(col_frequencies)[.x], " : ", .)
                                        ))
      col_frequencies %<>% tidyr::unite(col_text, sep = "\n") %>% dplyr::pull(col_text)
      col_coord %<>%
        dplyr::mutate(interactive_text = stringr::str_c(
          interactive_text, "\n\n", coltips_subtitle, " :\n", col_frequencies))
    }

    if (show_sup == FALSE) {
      row_coord %<>% dplyr::filter(!stringr::str_detect(colorvar, "Sup"))
      col_coord %<>% dplyr::filter(!stringr::str_detect(colorvar, "Sup"))
    }


    # Le point moyen et son texte interactif :
    col_freq_text <- rowSums(res.ca$call$Xtot) %>%
      tibble::enframe(name = "noms", value = "freq") %>%
      dplyr::mutate(freq = stringr::str_c(round(freq/sum(freq)*100, 0), "%")) %>%
      dplyr::mutate(noms = stringr::str_remove_all(noms, cleannames_condition())) %>%
      tidyr::unite(row_freq, sep = ": ") %>%   dplyr::pull(row_freq) %>%
      stringr::str_c(collapse = "\n")

    row_freq_text <- rowSums(t(res.ca$call$Xtot)) %>%
      tibble::enframe(name = "noms", value = "freq") %>%
      dplyr::mutate(freq = stringr::str_c(round(freq/sum(freq)*100, 0), "%")) %>%
      dplyr::mutate(noms = stringr::str_remove_all(noms, cleannames_condition())) %>%
      tidyr::unite(col_freq, sep = ": ") %>% dplyr::pull(col_freq) %>%
      stringr::str_c(collapse = "\n")

    mean_point_coord <- row_coord %>% dplyr::slice(1) %>%
      dplyr::mutate_at(dplyr::vars(starts_with("Dim")), ~ 0) %>%
      dplyr::mutate(noms = NA_character_, freq = 1, colorvar = "Point_moyen",
                    numbers = NA_character_) %>%
      dplyr::mutate(interactive_text = stringr::str_c(
        "<b>Point moyen</b>\nEffectifs : ", stringr::str_c(freq*100, "%")))

    #if ("row" %in% tooltips) {     }     if ("col" %in% tooltips) {        }
    mean_point_coord %<>%
      dplyr::mutate(interactive_text = stringr::str_c(interactive_text, "\n\n", rowtips_subtitle, " :\n", row_freq_text,
                                                      "\n\n", coltips_subtitle, " :\n", col_freq_text))

    # Option pour afficher les noms en majuscule (colonnes ou lignes) :
    if ("row" %in% uppercase) {
      row_coord %<>% dplyr::mutate(noms = stringr::str_to_upper(noms, locale = "fr"))
    }
    if ("col" %in% uppercase) {
      col_coord %<>% dplyr::mutate(noms = stringr::str_to_upper(noms, locale = "fr"))
    }


    all_coord <- row_coord %>%
      dplyr::bind_rows(col_coord) %>%
      dplyr::mutate(colorvar = as.factor(colorvar),
                    colorvar_names = as.factor(stringr::str_c("names_", colorvar)),
                    id = dplyr::row_number()      )



    #Calculer les limites du graphique (argument à passer dans ggout pour regler la taille du htmlwidget)
    min_max_lims <- dplyr::select(all_coord, !!dim1, !!dim2)

    if (!missing(xlim)) min_max_lims %<>%  tibble::add_row(!!dim1 := xlim[1]) %>% tibble::add_row(!!dim1 := xlim[2])
    if (!missing(ylim)) min_max_lims %<>%  tibble::add_row(!!dim2 := ylim[1]) %>% tibble::add_row(!!dim2 := ylim[2])
    heigth_width_ratio <- min_max_lims %>% dplyr::summarise_all(~ max(., na.rm = TRUE) - min(., na.rm = TRUE), .groups = "drop")
    min_max_lims <-
      dplyr::bind_rows(dplyr::summarise_all(min_max_lims, ~ min(., na.rm = TRUE), .groups = "drop"),
                       dplyr::summarise_all(min_max_lims, ~ max(., na.rm = TRUE), .groups = "drop"))
    width_range <- dplyr::pull(heigth_width_ratio, 1)[1]
    heigth_width_ratio %<>% dplyr::summarise(heigth_width_ratio = !!dim2/!!dim1, .groups = "drop") %>% tibble::deframe()
    if (dist_labels[1] == "auto") dist_labels <- width_range/50

    if (!missing(xlim) & !missing(ylim))  {theme_acm_with_lims <-
      theme_mca(res = res.ca, axes = axes, no_color_scale = TRUE, size_scale_max = size_scale_max,  # legend.position = "bottom",
                xlim = c(xlim[1], xlim[2]), ylim = c(ylim[1], ylim[2]))}
    else if (!missing(xlim) ) {theme_acm_with_lims <-
      theme_mca(res = res.ca, axes = axes, no_color_scale = TRUE, size_scale_max = size_scale_max,  # legend.position = "bottom",
                xlim = c(xlim[1], xlim[2]) )}
    else if (!missing(ylim) )  {theme_acm_with_lims <-
      theme_mca(res = res.ca, axes = axes, no_color_scale = TRUE, size_scale_max = size_scale_max,  # legend.position = "bottom",
                ylim = c(ylim[1], ylim[2]))}
    else {theme_acm_with_lims <-
      theme_mca(res = res.ca, axes = axes, no_color_scale = TRUE, size_scale_max = size_scale_max)} # legend.position = "bottom",

    outlims <- function(data, lim, dim) {
      dim <- rlang::enquo(dim)
      if (!is.na(lim[1])) data %<>% dplyr::filter(!!dim > lim[1])
      if (!is.na(lim[2])) data %<>% dplyr::filter(!!dim < lim[2])
      return(data)
    }

    if (text_repel == FALSE | out_lims_move == FALSE) {
      if (!missing(xlim)) all_coord %<>% outlims(xlim, !!dim1)
      if (!missing(ylim)) all_coord %<>% outlims(ylim, !!dim2)
    }


    scale_color_named_vector <-
      c("Point_moyen" = "black",   # Material colors :
        "Active_col1" = "#3f51b5", # Indigo 500
        "Active_col2" = "#673ab7", # Deep purple 500
        "Active_col3" = "#1976d2", # Blue 700
        "Active_col4" = "#7b1fa2", # Purple 700
        "Active_row1" = "#43a047", # Green 600
        "Active_row2" = "#f57c00", # Orange 700
        "Active_row3" = "#c0ca33", # Lime 600
        "Active_row4" = "#f4511e", # Deep orange 600
        "Active_row5" = "#7cb342", # Light green 600
        "Active_row6" = "#e53935", # Red 600
        "Active_row7" = "#fbc02d", # Jaune 700
        "Active_row8" = "#26a69a", # Teal 400

        "Sup_col"    =  "#b0bec5", # Blue grey 200
        "Sup_row"    =  "#bcaaa4", # Brown 200

        "names_Point_moyen" = "black",
        "names_Active_col1" = "#000051", # Indigo 900 Dark
        "names_Active_col2" = "#000063", # Deep purple 900 Dark
        "names_Active_col3" = "#002171", # Blue 900 Dark
        "names_Active_col4" = "#12005e", # Purple 900 Dark
        "names_Active_row1" = "#00600f", # Green 700 Dark
        "names_Active_row2" = "#bb4d00", # Orange 700 Dark
        "names_Active_row3" = "#7c8500", # Lime 700 Dark
        "names_Active_row4" = "#ac0800", # Deep orange 700 Dark
        "names_Active_row5" = "#4b830d", # Light green 600 Dark
        "names_Active_row6" = "#ab000d", # Red 600 Dark
        "names_Active_row7" = "#c49000", # Jaune 700 Dark
        "names_Active_row8" = "#00766c", # Teal 400 Dark

        "names_Sup_col" = "#808e95", # Blue grey 200 Dark
        "names_Sup_row" = "#8c7b75" # Brown 200 Dark
      )


    if (!missing(title)) {
      title_graph <- labs(title = title) #stringr::str_c("Les variables actives de l'ACM sur les axes ",axes[1], " et ", axes[2] )
    } else {
      title_graph <- NULL
    }

    graph_mean_point <-
      ggiraph::geom_point_interactive(data = mean_point_coord,
                                      ggplot2::aes(x = !!dim1, y = !!dim2, tooltip = interactive_text),
                                      color = "black", shape = 3, size = 5, stroke = 1.5, fill = "black", na.rm = TRUE)

    graph_theme_acm <-
      list(theme_acm_with_lims,
           ggplot2::scale_colour_manual(values = scale_color_named_vector, aesthetics = c("colour", "fill")),
           ggplot2::theme(plot.margin = ggplot2::margin(r = right_margin, unit = "cm")),
           title_graph)


    #Sorties :
    if (type[1] == "points") {
      plot_output <- ggplot2::ggplot() + graph_theme_acm +
        ggrepel::geom_text_repel(data = all_coord,
                                 ggplot2::aes(x = !!dim1, y = !!dim2, label = noms, color = colorvar_names),
                                 size = text_size, hjust = "left", nudge_x = dist_labels, direction = "y", segment.colour = "black",
                                 segment.alpha = 0.2, point.padding = 0.25, na.rm = TRUE) + #0.25, # min.segment.length = 0.8, max.iter = 10000 #repel_max_iter #fontface = "bold", max.iter = 50000
        ggiraph::geom_point_interactive(data = all_coord,
                                        ggplot2::aes(x = !!dim1, y = !!dim2, size = freq, color = colorvar,shape = colorvar,
                                                     tooltip = interactive_text, fill = colorvar, data_id = id),
                                        stroke = 1.5, na.rm = TRUE) +
        graph_mean_point +
        ggplot2::scale_shape_manual(values = c(
          #"Point_moyen" = 1,
          "Active_col1" = 17,
          "Active_col2" = 17,
          "Active_col3" = 17,
          "Active_col4" = 17,
          "Active_row1" = 18,
          "Active_row2" = 18,
          "Active_row3" = 18,
          "Active_row4" = 18,
          "Sup_col"    = 17,
          "Sup_row"    = 18  ))

      css_hover <- ggiraph::girafe_css("fill:gold;stroke:orange;", text = "color:gold4;stroke:none;")
      plot_output %<>% append(c("css_hover" = css_hover))

    } else if (type[1] == "text") {
      if (text_repel == FALSE) {
        graph_text <-
          ggiraph::geom_text_interactive(data = all_coord,
                                         ggplot2::aes(x = !!dim1, y = !!dim2, label = noms, color = colorvar, tooltip = interactive_text, data_id = id),
                                         size = text_size, fontface = "bold",  na.rm = TRUE)
      } else {
        graph_text <-
          ggrepel::geom_text_repel(data = all_coord,
                                   ggplot2::aes(x = !!dim1, y = !!dim2, label = noms, color = colorvar),
                                   size = text_size, na.rm = TRUE, fontface = "bold",
                                   direction = "both", # segment.alpha = 0.5,# point.padding = 0.25, segment.colour = "black",
                                   min.segment.length = 0.4, arrow = ggplot2::arrow(length = ggplot2::unit(0.25, "lines")))
      }
      plot_output <- ggplot2::ggplot() + graph_theme_acm + graph_text + graph_mean_point


    } else if (type[1] == "labels") {
      if (text_repel == FALSE) {
        graph_text <-
          ggiraph::geom_label_interactive(data = all_coord,
                                          ggplot2::aes(x = !!dim1, y = !!dim2, label = noms, color = colorvar, tooltip = interactive_text, data_id = id),
                                          size = text_size, fontface = "bold",  na.rm = TRUE)
      } else {
        graph_text <-
          ggrepel::geom_label_repel(data = all_coord,
                                    ggplot2::aes(x = !!dim1, y = !!dim2, label = noms, color = colorvar),
                                    size = text_size, na.rm = TRUE, fontface = "bold",
                                    direction = "both", # segment.alpha = 0.5,# point.padding = 0.25, segment.colour = "black",
                                    min.segment.length = 0.5, arrow = ggplot2::arrow(length = ggplot2::unit(0.25, "lines")))
      }
      plot_output <- ggplot2::ggplot() + graph_theme_acm + graph_text + graph_mean_point
    }

    #Add informations in the ggplot2::ggplot object, to be used into ggout() (without losing ggplot2::ggplot class)
    css_tooltip <- "text-align:right;padding:4px;border-radius:5px;background-color:#eeeeee;color:white;"
    plot_output %<>% append(c("css_tooltip" = css_tooltip)) %>%
      append(c("heigth_width_ratio" = heigth_width_ratio)) %>%
      `attr<-`("class", c("gg", "ggplot"))
    return(plot_output)
  }

# (ggca(res.ca, show_sup = TRUE,
#          rowcolor_numbers = 4,
#          rowtips_subtitle = "Groupe socio-pro\nil y a 5 ans")  +
#     xlim(c(-0.7,1.2)) + ylim(c(-0.7,0.5))) %>%
#   ggout("ggiraph")

# girafe_plot %>%
#   frameWidget(width = "120%")
# saveWidget("Girafe.html")

#
# FES2017 %>%
#   dplyr::mutate() %>%
#   tabw(CSER, PR2017ALL1, wt = w5, tot = "no",
#        rare_levels_to_other = TRUE, subtext = champ_inscrits) %>%
#   purrr::flatten_df() %>% dplyr::mutate(dplyr::across(tidyselect::where(rlang::is_decimal), as.double)) %>% tibble::column_to_rownames(colnames(.)[1]) %>%
#   FactoMineR::CA() %>%
#   ggca(size_scale_max = 6) %>%  #+ ggtitle("Vote au premier tour 2017 en fonction de la CSP : analyse des correspondances")) %>%
#   ggout("ggiraph")






# Un tableau automatique pour aider à interpréter les axes un par un :
#On part du tableau des contributions des modalités mais « en longeur »
#' Helper table to interpret MCA
#'
#' @param res.mca An object created with \code{FactoMineR::\link[FactoMineR]{MCA}},
#'
#' @return A tibble
#' @export
#'
# @examples
mca_interpret <- function(res.mca = res.mca) {
  res.mca <- res.mca
  contrib1 <- res.mca$var$contrib %>%
    tibble::as_tibble(rownames = "Modalités") %>%
    tidyr::pivot_longer(-Modalités, names_prefix ="Dim ", names_to = "Axe",
                        values_to = "Contrib_mod") %>%
    dplyr::select(Axe, tidyselect::everything()) %>% dplyr::arrange(Axe) %>%
    dplyr::mutate(valeur_propre = res.mca$eig[as.integer(Axe),1])

  #Matrice pour savoir les questions auxquelles correspondent les modalités
  var_names <- names(res.mca$call$X[res.mca$call$quali])
  question <- contrib1 %>%
    dplyr::select(-Contrib_mod, -valeur_propre)
  for (i in 1:length(var_names)) {
    new_var_name <- paste0(i)
    question %<>% dplyr::mutate( !!new_var_name := dplyr::if_else(
      contrib1[,2] == levels(eval(parse(text = paste0("res.mca$call$X$", var_names[i])))),
      TRUE, FALSE))
  }
  question %<>% dplyr::mutate(Modalités = paste0(Axe, "-", Modalités)) %>%
    dplyr::select (-Axe) %>%  tibble::column_to_rownames(var = "Modalités")

  #Noms des questions
  question_noms <- question %>%
    t %>% tibble::as_tibble %>%
    transmute_all(~which(. == TRUE))  %>%
    dplyr::slice(1) %>%
    t %>%
    purrr::map_chr(~var_names[.])

  #Contribution des questions
  question_contrib <- question %>%
    purrr::map_df(~ifelse(.==TRUE, contrib1$Contrib_mod, 0)) %>%
    tibble::add_column(Axe = contrib1$Axe) %>%
    dplyr::group_by(Axe) %>%
    dplyr::mutate_if(is.numeric, ~dplyr::if_else(.!=0, sum(.), 0)) %>%
    dplyr::ungroup() %>%
    dplyr::select(-Axe) %>%
    t %>% tibble::as_tibble %>%
    dplyr::summarise_all(sum) %>% t %>% as.vector

  contrib <- contrib1 %>%
    tibble::add_column(Questions = question_noms,
                       Contrib_q = question_contrib, .before = 1) %>%
    dplyr::select(Axe, tidyselect::everything())

  #Coordonnées et fréquences des modalités (pour calculer contribution des écarts)
  coord_fk <- res.mca$var$coord %>%
    tibble::as_tibble(rownames = "Modalités") %>%
    tibble::add_column(fk = res.mca$call$marge.col) %>%
    tidyr::pivot_longer(c(-Modalités, -fk), names_prefix ="Dim ", names_to = "Axe",
                        values_to = "coord") %>%
    dplyr::arrange(Axe)

  #Choisir les modalités > à la moyenne, trier par coordonnées positives/négatives
  contribsup <- contrib %>%
    dplyr::mutate(coord = coord_fk$coord,
                  fk = coord_fk$fk) %>%
    dplyr::group_by(Questions) %>%
    dplyr::filter(Contrib_mod >= mean(Contrib_mod) ) %>%
    dplyr::arrange(Axe, dplyr::desc(Contrib_q), dplyr::desc(Contrib_mod)) %>%
    #dplyr::arrange(dplyr::desc(Contrib_q)) %>%
    dplyr::mutate(Modalités_2 = Modalités, contrib_neg = Contrib_mod,
                  contrib_pos  = Contrib_mod, fneg = fk, fpos = fk,
                  coord_neg = coord, coord_pos = coord) %>%
    dplyr::select(-Contrib_mod) %>%
    dplyr::mutate_at(dplyr::vars(Modalités, contrib_neg, fneg, coord_neg),~ifelse(coord <= 0, ., NA)) %>%
    dplyr::mutate_at(dplyr::vars(Modalités_2, contrib_pos, fpos, coord_pos),~ifelse(coord > 0, ., NA)) %>%
    dplyr::ungroup

  #Ajouter les écarts par questions (en % de la contribution de la question) :
  contribsup %<>%
    dplyr::group_by(Axe, Questions) %>%
    dplyr::mutate(coord_ecart_neg = weighted.mean(coord_neg,fneg, na.rm = TRUE),
                  coord_ecart_pos = weighted.mean(coord_pos,fpos, na.rm = T),
                  poids_ecart_neg = sum(fneg, na.rm = T),
                  poids_ecart_pos = sum(fpos, na.rm = T)  ) %>%
    dplyr::mutate(poids_ecart = 1/( 1/poids_ecart_neg + 1/poids_ecart_pos) ) %>%
    dplyr::mutate(contrib_ecart = poids_ecart * 100 * (coord_ecart_pos - coord_ecart_neg)^2 / (valeur_propre*Contrib_q/100  ) ) %>%
    dplyr::select(-coord,-fk,-coord_ecart_neg, -coord_ecart_pos, -poids_ecart_neg,
                  -poids_ecart_pos, -poids_ecart) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(contrib_ecart = ifelse(is.na(contrib_ecart), NA, contrib_ecart) )

  #Contributions totales (positif/négatif sur l'axe), contrib de l'écart total :
  total1 <- contribsup %>%
    dplyr::group_by(Axe) %>%
    dplyr::summarise(contrib_neg = sum(contrib_neg, na.rm = TRUE),
                     contrib_pos = sum(contrib_pos, na.rm = TRUE),
                     coord_neg = weighted.mean(coord_neg, fneg, na.rm = TRUE),
                     coord_pos = weighted.mean(coord_pos, fpos, na.rm = TRUE),
                     poids_neg = sum(fneg, na.rm = TRUE),
                     poids_pos = sum(fpos, na.rm = TRUE),
                     poids_ecart = 1/( 1/poids_neg + 1/poids_pos), #fii' = 1/(1/fi + 1/fi').
                     contrib_ecart = poids_ecart * 100 * (coord_pos - coord_neg)^2/mean(valeur_propre) # = fii' (y l - y ')²/??l )
    ) %>% dplyr::select(-coord_neg, -coord_pos, -poids_neg, -poids_pos, - poids_ecart) %>%
    tibble::add_column(rowposition = "3") %>%
    tibble::add_column(Questions = "Total")

  #Total général (contributions sur l'axe positif + sur l'axe négatif)
  total2 <- contribsup %>%
    dplyr::group_by(Axe) %>%
    dplyr::summarise(contrib_neg = sum(contrib_neg, na.rm = TRUE) +
                       sum(contrib_pos, na.rm = TRUE),) %>%
    tibble::add_column(rowposition = "4") %>%
    tibble::add_column(Questions = "")

  #Ligne vide entre chaque axe :
  empty_lines <- total2 %>%
    dplyr::mutate(rowposition = "5") %>%
    dplyr::select(-contrib_neg)

  #Ligne de titre
  title_lines <- empty_lines %>%
    dplyr::mutate(rowposition = "0",
                  Questions = "",
                  axe_name = paste0("Interprétation de l'axe ", Axe, " : ",
                                    round(res.mca$eig[as.integer(Axe),1]*100, 1),
                                    "% de la variance totale"))
  #Répéter les noms des colonnes
  name_lines <- empty_lines %>%
    dplyr::mutate(rowposition = "1") %>%
    dplyr::mutate(axe_name = "", Questions="Questions", Contrib_q = "Contrib q",
                  Modalités = "Modalités négatives", Modalités_2 ="Modalités positives",
                  contrib_neg = "Contrib (neg)", contrib_pos = "Contrib (pos)",
                  contrib_ecart = "Contrib écart")

  #Assembler le tableau final (composants triés par ordre avec rowposition)
  Tableau_final <- contribsup %>%
    dplyr::select(-fneg, -fpos, -coord_neg, -coord_pos, -valeur_propre) %>%
    tibble::add_column(rowposition = "2") %>%
    tibble::add_column(axe_name = "", .before = 1) %>%
    dplyr::bind_rows(total1, total2, empty_lines, title_lines) %>%
    dplyr::mutate_if(is.numeric, ~round(.,1) ) %>%
    dplyr::mutate_if(is.numeric, ~ifelse(is.na(.), "", paste0(.,"%"))) %>%
    dplyr::mutate_if(rlang::is_character, ~ifelse(is.na(.), "", .)) %>%
    dplyr::mutate(Questions2 = Questions) %>%
    dplyr::group_by(Axe, Questions2) %>%
    dplyr::mutate_at(dplyr::vars(Questions, Contrib_q, contrib_ecart),
                     ~ifelse(dplyr::row_number(.) == 1, ., "")) %>%
    dplyr::ungroup() %>%
    dplyr::select(-Questions2) %>%
    dplyr::bind_rows(name_lines) %>%
    dplyr::arrange(Axe, rowposition) %>%
    dplyr::select(-rowposition, -Axe)

  remove(contrib1, contrib, coord_fk, contribsup, question, total1, total2,
         empty_lines, title_lines, name_lines)

  Tableau_final
}

# #Exemple :
# mca_interpret(res.mca)





#Fonction : fréquences des modalités actives
#' Graph for frequencies of active vars in MCA
#'
#' @param res.mca An object created with \code{FactoMineR::\link[FactoMineR]{MCA}},
#'
#' @return A graph of frequencies.
#' @export
#'
# @examples
ggmca_freq <- function(res.mca = res.mca) {
  res.mca <- res.mca
  freq <- res.mca$call$marge.col %>%
    tibble::as_tibble(rownames = "Modalités") %>%
    dplyr::mutate(fréquences = value*length(res.mca$call$quali)*100) %>% dplyr::select(-value)

  #Retrouver les questions à partir des modalités
  var_names <- names(res.mca$call$X[res.mca$call$quali])
  question <- freq %>% dplyr::select (-fréquences)
  for (i in 1:length(var_names)) {
    new_var_name <- paste0(i)
    question %<>% dplyr::mutate( !!new_var_name := dplyr::if_else(
      freq[,1] == levels(eval(parse(text = paste0("res.mca$call$X$", var_names[i])))),
      TRUE, FALSE))
  }
  question %<>%
    tibble::column_to_rownames(var = "Modalités") %>% t %>% tibble::as_tibble %>%
    transmute_all(~which(. == TRUE)) %>%  dplyr::slice(1) %>%  t %>%
    purrr::map_chr(~var_names[.])
  freq %<>%
    tibble::add_column(Questions = question_noms, .before = 1) %>%
    dplyr::mutate_if(is.character, as_factor) %>%
    dplyr::mutate(fréquences = round(fréquences, 0))
  # dplyr::mutate(fréquences = stringr::str_c(fréquences, "%"))



  ggplot2::ggplot(freq, ggplot2::aes(x = Modalités, y = fréquences, fill = Questions)) +
    # geom_hline(yintercept = 50, color="black") +
    ggplot2::geom_bar(stat = "identity") +
    ggplot2::geom_text(ggplot2::aes(label = stringr::str_c(fréquences, "%")),
                       nudge_y = 3, fontface = "bold") +
    # scale_color_brewer(palette = "Dark2") +
    # scale_fill_brewer(palette = "Dark2") +
    ggplot2::theme_minimal() +
    ggplot2::ylim(0,100) +
    ggplot2::theme(
      plot.background = ggplot2::element_rect(fill = "gray98", colour = NA),
      panel.background = ggplot2::element_rect(fill = "gray98", colour = NA),
      plot.title = ggplot2::element_text(hjust = 0.5, face = "bold"),
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_blank(),
      axis.title = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_text(color = "black", angle = 45, hjust = 1,
                                          face = "bold", size = 11),
      axis.text.y = ggplot2::element_text(color = "black", size = 11),
      axis.ticks.y = ggplot2::element_line(color = "gray"),
      axis.ticks.length = ggplot2::unit(4, "pt"),
      axis.line.y = ggplot2::element_line(color = "gray"),
    )




}





#Pas terminé/pas utile :
#wtab3_mf1 <- function(data, var1, var2, var3, wt, show_na = FALSE, ...) {
#
#
# #START
# tab <- ct2013acm %>%
#   wtab3(CSEacm, EVALOBJacm, CSER, wt = pondcal, show_na = F) %>%
#   adorn_percentages("row") %>%
#   adorn_pct_formatting(digits = 0, affix_sign = F)
#   detect_total <- tab %>%
#   dplyr::select(CSEacm) %>%
#   purrr::map(stringr::str_detect,"Total") %>% unlist %>% which(. == TRUE)
# code_lines <- "output <- tab "
# for (i in 2:ncol(tab)) {
#   new_line <- paste0(" %>% \n dplyr::mutate(", names(tab)[i], " = color_bar(\"#eeeeee\")(", names(tab)[i],"))")
#   code_lines <- paste0(code_lines, new_line)
# }
# eval(parse(text = code_lines))
#
# names(output) <- names(output) %>%
#   stringr::str_replace_all("_", " ") #Enlever _ des noms de modalités
#
# output %>%
#    purrr::map_df(stringr::str_replace_all,"_", " ") %>%  #Enveler _ des noms de modalités
#   # dplyr::mutate(CSEacm = cell_spec(output$CSEacm, "html",
#   #                            align = ifelse(!grepl("Total", output$CSEacm), "r", "l"))
#   # )  %>%
#   kable(format = "html", escape = F) %>%
#   kable_styling(
#     bootstrap_options = c("condensed"), #"striped",
#     full_width = F, position = "right",
#     fixed_thead = T
#   ) %>%
#   row_spec(1:nrow(tab), color= "black", align = "r") %>%
#   row_spec(detect_total, bold = T, hline_after = T) %>%
#   column_spec(ncol(tab), bold = T) %>%
#   column_spec(2:ncol(tab), width = "2cm")  %>%
#   footnote(general = "Champ : salariés des organisations, France (hors Mayotte).\nSource : DARES/DREES/DGAFP, Enquête Conditions de travail 2013, volet salariés.",
#            general_title = "")
# save_kable(file = "table1.html", self_contained = T)
#
#
#
# #AVEC DES COLOR BARS POUR TOUTE LA ZONE :
# tab <- ct2013acm %>%
#   wtab3(CSEacm, EVALOBJacm, CSER, wt = pondcal, show_na = F) %>%
#   adorn_percentages("row") %>%
#   adorn_pct_formatting(digits = 0, affix_sign = F) %>%
#   purrr::map_df(stringr::str_replace_all,"_", " ")
# names(tab) <- names(tab) %>%
#   stringr::str_replace_all("_", " ") #Enlever _ des noms de modalités
# detect_total <- tab %>%
#   dplyr::select(CSEacm) %>%
#   purrr::map(stringr::str_detect,"Total") %>% unlist %>% which(. == TRUE)
#
#
#
#
# #Pour faire des color_bar non pas sur une colonne mais sur plusieurs :
# #Marche aussi avec des lignes : area(row, col)
#
# tab %>% formattable(
#             align_column = c("l","r","r","r","r","r"),
#             list(
#               # `CSEacm` = formatter(
#               #   "span", style = ~ style(color = "black",font.weight = "bold")),
#               # # `Total` = formatter(
#               # #   "span", style = ~ style(color = "black",font.weight = "bold")),
#               # `<Brevet`= color_bar(customRed),
#               # `>Bac+2`= color_bar(customRed),
#               # `Bac`= color_bar(customRed),
#               # `CAP-BEP`= color_bar(customRed),
#               # `Total` = total_formatter
#               area(col=c(`Objectifs et entretien`, `Entretien sans objectifs`, `Objectifs sans entretien`, `Aucun`, `Total`)) ~ normalize_bar("#eeeeee"),
#               #area(col=c(`Objectifs et entretien`, `Entretien sans objectifs`, `Objectifs sans entretien`, `Aucun`, `Total`)) ~ formatter( "span", style = ~ style(width = "300px") ),
#               area(row=detect_total) ~ formatter( "span", style = ~ style(font.weight = "bold") )
#               #area(col=names(tab[2:ncol(tab)])~ normalize_bar("#eeeeee") )
#             )) #Need equal columns width
#
#
# #On peut créer des fonctions avec formattable et les citer après :
# total_formatter <-
#   formatter("span",
#             style = x ~ style(
#               font.weight = "bold",
#               color = ifelse(x > 0, customGreen,
#                              ifelse(x < 0, customRed, "black"))))


#}

# f <-
# "\"\";value $ SUSPENDUf
#  \"1\"=\"Le contrat a été maintenu\"
# \" 2\"=\"Le contrat a été rompu ou suspendu\"
# \"9 \"=\"Ne sait pas\"
#
# ;value $ TYPPREf
# \"1\"=\"Une préretraite d'entreprise ou \"préretraite maison\"\"
# \"2\"=\"Une allocation de préretraite à financement public (ASFNE, CATS, CAATA)\"
# \"3\"=\"Une allocation de préretraite de la fonction publique\"
# \"9\"=\"Ne sait pas\"
# ;"
# f <- read_file_raw("Enquête Emploi 2013-2018\\formats_sas-EE2018.txt")



#Modified from finalfit::or_plot WHERE ? --------------------------------------
#' Modified odd_ratios plot from finalfit
#'
#' @param .data The data
# @param dependent
# @param explanatory
# @param random_effect
# @param factorlist
# @param glmfit
# @param confint_type
# @param remove_ref
# @param break_scale
# @param column_space
# @param dependent_label
# @param prefix
# @param suffix
# @param table_text_size
# @param title_text_size
# @param plot_opts
# @param table_opts
# @param return_df
# @param ...
#
# @return
#' @export
#'
# @examples
pers_or_plot <-
  function (.data, dependent, explanatory, random_effect = NULL,
            factorlist = NULL, glmfit = NULL, confint_type = NULL, remove_ref = FALSE,
            break_scale = NULL, column_space = c(-0.5, 0, 0.2), dependent_label = NULL,
            prefix = "", suffix = ": OR (95% CI, p-value)",
            table_text_size = 5, title_text_size = 18, plot_opts = NULL,
            table_opts = NULL, return_df = FALSE, ...) {
    requireNamespace("ggplot2")
    if (!is.null(factorlist)) {
      if (is.null(factorlist$Total))
        stop("summary_factorlist function must include total_col=TRUE")
      if (is.null(factorlist$fit_id))
        stop("summary_factorlist function must include fit_id=TRUE")
    }
    if (is.null(factorlist)) {
      factorlist = finalfit::summary_factorlist(.data, dependent, explanatory,
                                                total_col = TRUE, fit_id = TRUE)
    }
    if (remove_ref) {
      factorlist = factorlist %>% dplyr::mutate(label = ifelse(label ==
                                                                        "", NA, label)) %>% tidyr::fill(label) %>%
        dplyr::group_by(label) %>% dplyr::filter(dplyr::row_number() !=
                                                   1 | dplyr::n() > 2) %>% finalfit::rm_duplicate_labels()
    }
    # if (is.null(breaks)) {
    #   breaks = scales::pretty_breaks()
    # }
    if (is.null(confint_type) && is.null(random_effect)) {
      confint_type = "profile"
    }
    else if (is.null(confint_type) && (!is.null(random_effect) |
                                       class(glmfit) == "glmerMod")) {
      confint_type = "default"
    }
    if (is.null(glmfit) && is.null(random_effect)) {
      glmfit = finalfit::glmmulti(.data, dependent, explanatory)
      glmfit_df_c = finalfit::fit2df(glmfit, condense = TRUE, estimate_suffix = " (multivariable)",
                                     confint_type = confint_type, ...)
    }
    else if (is.null(glmfit) && !is.null(random_effect)) {
      glmfit = finalfit::glmmixed(.data, dependent, explanatory, random_effect)
      glmfit_df_c = finalfit::fit2df(glmfit, condense = TRUE, estimate_suffix = " (multilevel)",
                                     confint_type = confint_type, ...)
    }
    if (!is.null(glmfit) && is.null(random_effect)) {
      glmfit_df_c = finalfit::fit2df(glmfit, condense = TRUE, estimate_suffix = " (multivariable)",
                                     confint_type = confint_type, estimate_name = "OR",
                                     exp = TRUE, ...)
    }
    else if (!is.null(glmfit) && !is.null(random_effect)) {
      glmfit_df_c = finalfit::fit2df(glmfit, condense = TRUE, estimate_suffix = " (multilevel)",
                                     confint_type = confint_type, estimate_name = "OR",
                                     exp = TRUE, ...)
    }
    glmfit_df = finalfit::fit2df(glmfit, condense = FALSE, confint_type = confint_type,
                                 estimate_name = "OR", exp = TRUE, ...)
    df.out = finalfit::finalfit_merge(factorlist, glmfit_df_c)
    df.out = finalfit::finalfit_merge(df.out, glmfit_df, ref_symbol = "1.0")
    df.out$Total = stringr::str_remove(df.out$Total, " \\(.*\\)") %>%
      as.numeric()
    df.out$Total[which(df.out$levels %in% c("Mean (SD)",
                                            "Median (IQR)"))] = dim(.data)[1]
    df.out$levels[which(df.out$levels %in% c("Mean (SD)",
                                             "Median (IQR)"))] = "-"
    if (any(is.na(df.out$label))) {
      remove_rows = which(is.na(df.out$label))
      df.out = df.out[-remove_rows, ]
    }
    else {
      df.out
    }


    #Ajouts perso???:
    if (return_df == FALSE) {
      log_range <- max(as.numeric(df.out$OR)) + max(1/as.numeric(df.out$OR))
      if (missing(break_scale)) {
        break_scale <- dplyr::case_when(
          log_range < 4/8  ~ 16,
          log_range < 4/4  ~ 8,
          log_range < 4/2  ~ 4,
          log_range < 4    ~ 2,
          log_range < 4*2  ~ 1,
          log_range < 4*4  ~ 1/2,
          log_range < 4*8  ~ 1/4,
          log_range < 4*16 ~ 1/8,
          TRUE             ~ 1/16)
      }

      inverse_breaks <-
        sort((1:max(round(1/as.numeric(df.out$OR, 0))*2*break_scale)),
             decreasing = T)/break_scale
      legend_ticks_breaks <- c(1/inverse_breaks,
                               1:(max(round(as.numeric(df.out$OR), 0)*2*break_scale))/break_scale, 1) %>%
        unique() %>% sort()
      legend_ticks_labels <- ifelse(legend_ticks_breaks < 1,
                                    yes = stringr::str_c("1/", inverse_breaks),
                                    no = stringr::str_remove_all(as.character(
                                      legend_ticks_breaks), "0+$|\\.$"))
      df.out %<>%
        dplyr::mutate(freq = (
          (as.numeric(stringr::str_remove(df.out[, 5], " \\(.*\\)"))/Total*100) %>%
            round(0) %>% stringr::str_c("%") %>% stringr::str_pad(3)
        )) %>%
        dplyr::mutate(levels = stringr::str_c(levels, " (", freq, ")")) %>%
        dplyr::mutate(`OR (multivariable)` = dplyr::case_when(
          `OR (multivariable)` == "-" ~ "Référence",
          OR >= 1 & p <  0.001 ~ stringr::str_c(      format(round(  as.numeric(OR), digits = 2), nsmall = 2), "***"),
          OR >= 1 & p <  0.005 ~ stringr::str_c(      format(round(  as.numeric(OR), digits = 2), nsmall = 2), "** "),
          OR >= 1 & p <  0.01  ~ stringr::str_c(      format(round(  as.numeric(OR), digits = 2), nsmall = 2), "*  "),
          OR >= 1 & p >= 0.01  ~ stringr::str_c(      format(round(  as.numeric(OR), digits = 2), nsmall = 2), "    "),
          OR <  1 & p <  0.001 ~ stringr::str_c("1/", format(round(1/as.numeric(OR), digits = 2), nsmall = 2), "***"),
          OR <  1 & p <  0.005 ~ stringr::str_c("1/", format(round(1/as.numeric(OR), digits = 2), nsmall = 2), "** "),
          OR <  1 & p <  0.01  ~ stringr::str_c("1/", format(round(1/as.numeric(OR), digits = 2), nsmall = 2), "*  "),
          OR <  1 & p >= 0.01  ~ stringr::str_c("1/", format(round(1/as.numeric(OR), digits = 2), nsmall = 2), "    ")
        )) %>%
        dplyr::mutate(color = as.factor(dplyr::case_when(
          `OR (multivariable)` == "Référence" ~ "Référence",
          TRUE ~ "Autre"))) %>%
        dplyr::mutate(index = index + 1) %>%
        tibble::add_row(fit_id = stringr::str_c("Title", 1:2), label = "", Total = 0, index = 0:1,
                        .before = 1) #Deux lignes vides

      #Two lines of the original function???:
      df.out$levels = as.character(df.out$levels)
      df.out$fit_id = factor(df.out$fit_id, levels = df.out$fit_id[order(-df.out$index)])

      first_row <- df.out[1,] %>%
        tibble::add_row(fit_id = "Title1", label = "Variable", Total = 0, index = 0,
                        levels = "Modalité",
                        `OR (multivariable)` = "Odds ratio",
                        .before = 1) %>%
        tibble::add_row(fit_id = "Title2", label = "", Total = 0, index = 1,
                        levels = stringr::str_c("(% ", stringr::str_to_lower(colnames(df.out)[which(
                          colnames(df.out) == "Total") - 1]), ")"),
                        .before = 2) %>%
        dplyr::slice(1:2)


      g1 = ggplot2::ggplot(df.out, ggplot2::aes(x = as.numeric(OR), xmin = as.numeric(L95),
                                                xmax = as.numeric(U95), y = fit_id)) +
        ggplot2::geom_point(ggplot2::aes(size = Total, fill = color), shape = 22) + #"darkblue"
        ggplot2::geom_vline(xintercept = 1, linetype = "longdash",
                            colour = "black") +
        ggplot2::geom_point(data = dplyr::slice(dplyr::select(df.out, 1), 1), ggplot2::aes(x = 1, y = fit_id),
                            shape = 15, color = "white", size = 16, inherit.aes = FALSE) +
        ggplot2::geom_point(ggplot2::aes(size = Total, fill = color), shape = 22) + #"darkblue"
        ggplot2::geom_errorbarh(height = 0.2) +
        #geom_point(ggplot2::aes(size = Total/2), color = "#222222", shape = 4) +
        ggplot2::annotate("text", x = 0, y = first_row$fit_id[1],
                          label = " / rapport de chances", hjust = 0,
                          size = table_text_size, fontface = "bold") +
        ggplot2::annotate("text", x = 0, y = first_row$fit_id[2],
                          label = " (IC à 95%, échelle logarithmique)", hjust = 0,
                          size = table_text_size, fontface = "bold") +
        ggplot2::scale_x_continuous(trans = "log10", breaks = legend_ticks_breaks,
                                    labels = legend_ticks_labels) +
        ggplot2::scale_fill_manual(values = c(Autre = "#333333", Référence = "#999999")) +
        #xlab("Odds ratio (95% CI, log scale)") +
        ggplot2::theme_classic(14) +
        ggplot2::theme(axis.title.x = ggplot2::element_blank(), #element_text(),
                       axis.title.y = ggplot2::element_blank(), axis.text.y = ggplot2::element_blank(),
                       axis.line.y = ggplot2::element_blank(), axis.ticks.y = ggplot2::element_blank(),
                       legend.position = "none", plot.margin = ggplot2::unit(c(0.25,0.25,0.25,-0.275), "cm"))
      t1 = ggplot2::ggplot(df.out, ggplot2::aes(x = as.numeric(OR), y = fit_id)) +
        ggplot2::annotate("text", x = column_space[1], y = df.out$fit_id,
                          label = df.out[, 2], hjust = 0, size = table_text_size) +
        ggplot2::annotate("text", x = column_space[2], y = df.out$fit_id,
                          label = df.out[, 3], hjust = 1, size = table_text_size) +
        ggplot2::annotate("text", x = column_space[3], y = df.out$fit_id,
                          label = df.out[, 8], hjust = 1, size = table_text_size) +
        ggplot2::annotate("text", x = column_space[1], y = first_row$fit_id,
                          label = first_row[, 2], hjust = 0, size = table_text_size,
                          fontface = "bold") +
        ggplot2::annotate("text", x = column_space[2], y = first_row$fit_id,
                          label = first_row[, 3], hjust = 1, size = table_text_size,
                          fontface = "bold") +
        ggplot2::annotate("text", x = column_space[3], y = first_row$fit_id,
                          label = first_row[, 8], hjust = 1, size = table_text_size,
                          fontface = "bold.italic") +
        ggplot2::theme_classic(14) +
        ggplot2::theme(axis.title.x = ggplot2::element_blank(), #element_text(colour = "white"),
                       axis.text.x = ggplot2::element_text(colour = "white"), axis.title.y = ggplot2::element_blank(),
                       axis.text.y = ggplot2::element_blank(), axis.ticks.y = ggplot2::element_blank(),
                       line = ggplot2::element_blank(), plot.margin = ggplot2::unit(c(0.25,-0.275, 0.25,0.25), "cm"))
      g1 = g1 + plot_opts
      t1 = t1 + table_opts
      # title = plot_title(.data, dependent, dependent_label = dependent_label,
      #                    prefix = prefix, suffix = suffix)
      gridExtra::grid.arrange(t1, g1, ncol = 2, widths = c(3, 2)#,
                                     # top = grid::textGrob(title, x = 0.02, y = 0.2, gp = grid::gpar(fontsize = title_text_size),
                                     #                      just = "left")
      )
    } else {
      df.out
    }
  }

# OBJ_logit_plot <- glm.data %>%
#   pers_or_plot("OBJVRAIacm", explanatory, table_text_size = 4)
