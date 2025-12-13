# library(tidyverse)
# library(tabxplor)
# library(tidymodels)


# Regressions and logit 2 --------------------------------------------

lm_plots <- function(model) {
  par(mfrow=c(2,2))
  plot(model)
  par(mfrow=c(1,1))
}



# multiplicator = c("DPO" = 10, "DPO" = 50)
# ci <- FALSE
# conf.level <- 0.95
# format_OR <- TRUE
# n = 1

readable_OR <- 
  function(model, multiplicator = double(), n = 1L, digits_with_n = 2L,
           ci = FALSE, conf.level = 0.95, format = TRUE) {
    format_OR <- format
    
    n <- vctrs::vec_cast(n, integer())
    digits_with_n <- vctrs::vec_cast(digits_with_n, integer())
    stopifnot(n >= 1, digits_with_n >= 0)
    
    OR <- broom::tidy(model, exponentiate = TRUE, conf.int = ci, conf.level = conf.level) |> 
      rename(parameter = term, Odds_ratio = estimate, z = statistic, 
             any_of(c(OR_inf = "conf.low", OR_sup = "conf.high"))) |>
      mutate(parameter = if_else(row_number() == 1, "Intercept", parameter))
    # broom.helpers::tidy_plus_plus(model, exponentiate = TRUE)
    
    
    if (length(multiplicator) != 0) {
      missing_names <-  names(multiplicator) |>
        map_lgl(~ length(which(OR$parameter == .)) == 0) |>
        set_names(names(multiplicator) )
      
      if (any(missing_names)) {
        stop(paste0("following variables not found among the model parameters: ",
                    paste0(unique(names(missing_names)[missing_names]), collapse = ", ")))
      }
      
      ref_multiplicator    <- names(multiplicator) |> map(~ which(OR$parameter == .))
      
      
      names(multiplicator) <- paste0(names(multiplicator), "_", multiplicator)
      mult <- list(names(multiplicator), ref_multiplicator, multiplicator) |> transpose()
      
      OR <- 
        reduce(mult, .init = OR, 
               ~ bind_rows(..1, 
                           summarise(..1, 
                                     parameter       = ..2[[1]], 
                                     across(any_of(c("Odds_ratio", "OR_inf", "OR_sup")), 
                                            ~ nth(., ..2[[2]])^..2[[3]]),
                                     std.error  = abs(nth(std.error , ..2[[2]])*..2[[3]]), 
                                     p.value    = nth(p.value   , ..2[[2]]),
                           ))
        )
    }
    
    OR <- OR |>
      mutate(
        signif  = case_when(p.value < 0.01 ~ "***", 
                            p.value < 0.05 ~ "** ", 
                            p.value < 0.10 ~ "*  ", 
                            TRUE           ~ "   " ),
        Odds    = if_else(row_number() != 1, Odds_ratio * first(Odds_ratio), Odds_ratio), 
        prob    = if(n <= 1) {
          fmt(0, type = "row", pct = Odds/(1 + Odds))
        } else {
          fmt(0, type = "mean", mean = Odds/(1 + Odds) * n, digits = digits_with_n)
        }, 
        
        marginal_effect = set_digits( set_num(prob, get_num(prob) - get_num(first(prob)) ), 
                                      if_else(n <= 1, 1L, digits_with_n) ), 
      )
    
    
    if (ci) {
      OR <- OR |>
        mutate(
          Odds_inf = if_else(row_number() != 1, OR_inf * first(OR_inf), OR_inf),
          Odds_sup = if_else(row_number() != 1, OR_sup * first(OR_sup), OR_sup),
          p_inf    = if (n <= 1) {
            fmt(0, type = "row", pct = Odds_inf/(1 + Odds_inf))
          } else {
            fmt(0, type = "mean", mean = Odds_inf/(1 + Odds_inf) * n, digits = digits_with_n)
          }, 
          
          p_sup    = if (n <= 1) {
            fmt(0, type = "row", pct = Odds_sup/(1 + Odds_sup))
          } else {
            fmt(0, type = "mean", mean = Odds_sup/(1 + Odds_sup) * n, digits = digits_with_n)
          }, 
          
          me_inf   = set_digits(set_num(p_inf, get_num(p_inf) - get_num(first(p_inf)) ),
                                if_else(n <= 1, 1L, digits_with_n) ), 
          me_sup   = set_digits(set_num(p_sup, get_num(p_sup) - get_num(first(p_sup)) ),
                                if_else(n <= 1, 1L, digits_with_n) ) 
        )
    }
    
    prob_column <- c("marginal_effect" = "prob", "me_inf" = "p_inf", "me_sup" = "p_sup")
    
    if (format_OR) {
      OR <- OR |>
        mutate(
          across(any_of(c("Odds_ratio", "OR_inf", "OR_sup", "Odds", "Odds_inf", "Odds_sup")),
                 ~ if_else(condition = . < 1, 
                           true      = paste0("1/", format(1/., digits = 3)), 
                           false     = paste0("   ", format(., digits = 2)))   ), 
          
          across(any_of(c("prob", "marginal_effect", "p_inf", "p_sup", "me_inf", "me_sup")), 
                 ~ str_pad(format(.), max(str_length(format(.))))
          ), 
          
          across(any_of(c("Odds_ratio", "marginal_effect")), ~ paste0(., signif)),
          
          across(any_of(c("marginal_effect", "me_inf", "me_sup")), 
                 ~ if_else(row_number() == 1, 
                           true  = paste0("Ref:", rlang::eval_tidy(sym(prob_column[cur_column()] ))) |> 
                             str_remove_all("\\*") |> str_squish(), 
                           false = .)),
          
          p.value = format(round(p.value, 3), digits = 3)
        ) |>
        select(-any_of(c("signif", "p.value")))
    }
    
    if (n > 1) {
      OR <- OR |> rename("prob × n" = "prob")
    }
    
    return(OR)
  }




## or_plot ----


#To add : - colomn with frequencies divided one by another to see if logit brings
# something more than the cross-table ?

#' Modified odd ratios plot from `finalfit`
# Licence MIT : https://finalfit.org/LICENSE-text.html
# Thanks to Ewen M Harrison.
#'
#' @param data Data frame.
#' @param dependent Character vector of length 1: name of dependent variable
#' (must have 2 levels).
#' @param predictors Character vector of any length: name(s) of predictors variables.
#' @param random_effect Character vector of length 1, name of random effect variable.
#' @param factorlist Option to provide output directly from \code{summary_factorlist()}.
#' @param glmfit 	Option to provide output directly from \code{glmmulti()} and \code{glmmixed()}.
#' @param confint_type One of \code{c("profile", "default")} for GLM models or
#' \code{c("default", "Wald", "profile", "boot")} for \code{glmer models}.
#' Note \code{"default" == "Wald"}.
#' @param remove_ref 	Logical. Remove reference level for factors.
#' @param break_scale Manually specify x-axis breaks in format \code{c(0.1, 1, 10)}.
#' @param column_space 	Adjust table column spacing.
#' @param dependent_label Main label for plot.
#' @param prefix Plots are titled by default with the dependent variable. This adds
#' text before that label.
#' @param suffix Plots are titled with the dependent variable. This adds text after
#' that label.
#' @param table_text_size Alter font size of table text.
#' @param title_text_size Alter font size of title text.
#' @param plot_opts A list of arguments to be appended to the ggplot call by \code{"+"}.
#' @param table_opts A list of arguments to be appended to the ggplot table call by
#'  \code{"+"}.
#' @param return_df To return the dataframe.
#' @param ... Other parameters.

#' @return The odd ratios plot as a \code{ggplot2} object.
#' @export
#'
# @examples
or_plot <-
  function (data, dependent, predictors, wt = NULL, 
            inverse_two_level_factors = TRUE, 
            random_effect = NULL, cleannames = NULL, 
            factorlist = NULL, glmfit = NULL, confint_type = NULL, remove_ref = FALSE,
            break_scale = NULL, column_space = c(-0.5, 0, 0.2), dependent_label = NULL,
            prefix = "", suffix = ": OR (95% CI, p-value)",
            table_text_size = 4, title_text_size = 18, plot_opts = NULL,
            table_opts = NULL, return_df = FALSE, ...) {
    # requireNamespace("finalfit", quietly = TRUE)
    
    cleannames <-
      if (is.null(cleannames)) { getOption("tabxplor.cleannames") } else {cleannames}
    
    variables_list <- c(dependent, predictors, wt)
    
    data <- dplyr::select(data, tidyselect::all_of(variables_list))
          
    tabs <- multi_logit(data, 
                        dependent = dependent, cleannames = cleannames,
                        inverse_two_level_factors = inverse_two_level_factors, 
                        predictors_sequence = list("model" = predictors),
                        add_pct = TRUE, empirical_odds_ratio = TRUE, add_n = TRUE, 
                        ci = TRUE,
                        wt = wt
    )
    tabs <- tabs[[1]]
    tabs <- tabs |> 
      dplyr::filter(!var == "Constant") |>
      dplyr::ungroup() |> 
      dplyr::mutate(
        ref = is_refrow(`OR model`), 
        index = row_number() + 1L, 
        across(c(pct, `Empirical OR`, `OR model`, n), get_num)
      ) |>
      dplyr::rename(OR = `OR model`, L95 = `OR inf_model`, U95 = `OR sup_model`, 
                    `Emp OR` = `Empirical OR`) 
    
    
    # names(df.out)
    # fit_id             # var+level
    # levels             # levels
    # freq               # pct
    # 
    # Total             # ???
    # index             # row_number
    # `OR (multivariable)` # recalculated from OR (character)
    # OR                
    # L95               #ci
    # U95               #ci
    # 
    # # # not_used
    # # lv1    
    # # lv2
    # # label              
    # # p    
    
    if (return_df == FALSE) {
     
      # Readable log breaks, in legend
       log_range <- max(tabs$OR) + max(1/tabs$OR)
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
        sort((1:max(c(round(1/as.numeric(tabs$OR, 0))*2*break_scale, 1))),
             decreasing = T)/break_scale
      legend_ticks_breaks <- c(1/inverse_breaks,
                               1:(max(round(as.numeric(tabs$OR), 0)*2*break_scale))/break_scale, 1) %>%
        unique() %>% sort()
      legend_ticks_labels <- ifelse(legend_ticks_breaks < 1,
                                    yes = stringr::str_c("1/", inverse_breaks),
                                    no = stringr::str_remove_all(as.character(
                                      legend_ticks_breaks), "0+$|\\.$"))
      
      
      unbrk <- stringi::stri_unescape_unicode("\\u202f")
      
      # write 1/OR if OR < 1 
      tabs <- tabs |>
        select(-`Emp OR`) |> 
        dplyr::mutate(
          fit_id = paste0(var, levels), 
          
          label  = as.character(var), 
          label  = dplyr::if_else(lag(label, default = "") == label, 
                                  "", 
                                  label
          ),  
          
          levels = stringr::str_c(.data$levels, " (", round(.data$pct*100), "%)"), 
          
          OR_text = dplyr::case_when(
            ref ~ "Reference",                               #There were unbreakable spaces.
            OR >= 1 & s1 == "***" ~ stringr::str_c(        format(round(  OR, digits = 2), nsmall = 2), "***"),
            OR >= 1 & s1 == "** " ~ stringr::str_c(        format(round(  OR, digits = 2), nsmall = 2), "**" , paste0(rep(unbrk, 2), collapse = "")),            #Unbreakable space
            OR >= 1 & s1 == "*  " ~ stringr::str_c(        format(round(  OR, digits = 2), nsmall = 2), "*"  , paste0(rep(unbrk, 4), collapse = "")),
            OR >= 1 & s1 == "   " ~ stringr::str_c(        format(round(  OR, digits = 2), nsmall = 2),        paste0(rep(unbrk, 6), collapse = "")),
            OR <  1 & s1 == "***" ~ stringr::str_c("1 / ", format(round(1/OR, digits = 2), nsmall = 2), "***"),
            OR <  1 & s1 == "** " ~ stringr::str_c("1 / ", format(round(1/OR, digits = 2), nsmall = 2), "**" , paste0(rep(unbrk, 2), collapse = "")),
            OR <  1 & s1 == "*  " ~ stringr::str_c("1 / ", format(round(1/OR, digits = 2), nsmall = 2), "*"  , paste0(rep(unbrk, 4), collapse = "")),
            OR <  1 & s1 == "   " ~ stringr::str_c("1 / ", format(round(1/OR, digits = 2), nsmall = 2),        paste0(rep(unbrk, 6), collapse = ""))
          ), 
          
          color = as.factor(dplyr::case_when(
            OR_text == "Reference" ~ "Reference",
            TRUE                   ~ "Autre")), 
        )
      
      
      #Add two empty lines first
      tabs <-  
      dplyr::bind_rows(
        tibble::tibble(
          fit_id = stringr::str_c("Title", 1:2), label = "",
          n = 0, index = 0:1,
          .before = 1
        ), 
        tabs
      ) |>
        dplyr::mutate(fit_id = forcats::as_factor(fit_id) |> forcats::fct_rev() )   
      
      # tabs$fit_id = factor(tabs$fit_id, levels = tabs$fit_id[order(-tabs$index)])
      
      
      
      
      
      if (inverse_two_level_factors) {
        dependent_lv <- levels(forcats::fct_drop(data[[dependent]]))[1]
        # data <- data |>
        #   dplyr::mutate(across(all_of(dependent), ~ fct_rev(as.factor(.))))
      } else {
        dependent_lv <- levels(forcats::fct_drop(data[[dependent]]))[2]
      }
      
 
      
      if (cleannames) {
        dependent_lv <- stringr::str_remove_all(dependent_lv, tabxplor:::cleannames_condition())
      }
      
      
      first_row <- tabs[1,] %>%
        tibble::add_row(fit_id = "Title1", label = "Variable", n = 0, index = 0,
                        levels = "Levels",
                        OR_text = "Odds ratio", # stringi::stri_unescape_unicode("Odds ratio (IC \\u00e0 95%, \\u00e9chelle logarithmique)")
                        .before = 1) %>%
        tibble::add_row(fit_id = "Title2", label = "", n = 0, index = 1,
                        levels =  paste0("(% ", dependent_lv, ")"), 
                        # stringr::str_c("(% ", colnames(tabs)[which( #stringr::str_to_lower(
                        # colnames(tabs) == "n") - 1], ")"),
                        .before = 2) %>%
        dplyr::mutate(fit_id = forcats::as_factor(fit_id) |> forcats::fct_rev()) |> 
        dplyr::slice(1:2)
      
      g1 = ggplot2::ggplot(tabs, ggplot2::aes(x = as.numeric(.data$OR),
                                              xmin = as.numeric(.data$L95),
                                              xmax = as.numeric(.data$U95),
                                              y = .data$fit_id)) +
        ggplot2::geom_point(ggplot2::aes(size = .data$n, fill = .data$color),
                            shape = 22, na.rm = TRUE) + #"darkblue"
        ggplot2::geom_vline(xintercept = 1, linetype = "longdash",
                            colour = "black") +
        ggplot2::geom_point(data = dplyr::slice(dplyr::select(tabs, fit_id), 1),
                            ggplot2::aes(x = 1, y = .data$fit_id),
                            shape = 15, color = "white", size = 16,
                            inherit.aes = FALSE, na.rm = TRUE) +
        ggplot2::geom_point(ggplot2::aes(size = .data$n, fill = .data$color),
                            shape = 22, na.rm = TRUE) + #"darkblue"
        ggplot2::geom_errorbarh(height = 0.2, na.rm = TRUE) +
        #geom_point(ggplot2::aes(size = n/2), color = "#222222", shape = 4) +
        ggplot2::annotate("text", x = 0, y = first_row$fit_id[1],
                          label = " (95% IC, log scale)", #" / rapport de chances",
                          hjust = 0,
                          size = table_text_size, fontface = "bold", na.rm = TRUE) +
        # ggplot2::annotate("text", x = 0, y = first_row$fit_id[2],
        #                   label = stringi::stri_unescape_unicode(" (IC \\u00e0 95%, \\u00e9chelle logarithmique)"),
        #                   hjust = 0,
        #                   size = table_text_size, fontface = "bold") +
        ggplot2::scale_x_continuous(transform = "log10", 
                                    # transform = scales::pseudo_log_trans(sigma = 0.01, base = 10)
                                    breaks = legend_ticks_breaks,
                                    labels = legend_ticks_labels) +
        ggplot2::scale_fill_manual(values = c(Autre = "#333333", Reference = "#999999")) +
        #xlab("Odds ratio (95% CI, log scale)") +
        ggplot2::theme_classic(14) +
        ggplot2::theme(axis.title.x = ggplot2::element_blank(), #element_text(),
                       axis.title.y = ggplot2::element_blank(), axis.text.y = ggplot2::element_blank(),
                       axis.line.y = ggplot2::element_blank(), axis.ticks.y = ggplot2::element_blank(),
                       legend.position = "none", plot.margin = ggplot2::unit(c(0.25,0.25,0.25,-0.275), "cm"))
      
      t1 = ggplot2::ggplot(tabs, ggplot2::aes(x = as.numeric(.data$OR),
                                              y = .data$fit_id)) +
        ggplot2::annotate("text", x = column_space[1], y = tabs$fit_id,
                          label = tabs$label, 
                          hjust = 0, size = table_text_size, na.rm = TRUE) +
        ggplot2::annotate("text", x = column_space[2], y = tabs$fit_id,
                          label = tabs$levels, 
                          hjust = 1, size = table_text_size, na.rm = TRUE) +
        ggplot2::annotate("text", x = column_space[3], y = tabs$fit_id,
                          label =tabs$OR_text, 
                          hjust = 1, size = table_text_size, na.rm = TRUE) +
        ggplot2::annotate("text", x = column_space[1], y = first_row$fit_id,
                          label = first_row$label, 
                          hjust = 0, size = table_text_size,
                          fontface = "bold", na.rm = TRUE) +
        ggplot2::annotate("text", x = column_space[2], y = first_row$fit_id,
                          label = first_row$levels, 
                          hjust = 1, size = table_text_size,
                          fontface = "bold", na.rm = TRUE) +
        ggplot2::annotate("text", x = column_space[3], y = first_row$fit_id,
                          label = first_row$OR_text, 
                          hjust = 1, size = table_text_size,
                          fontface = "bold.italic", na.rm = TRUE) +
        ggplot2::theme_classic(14) +
        ggplot2::theme(
          #text = ggplot2::element_text(family = "sans"), #if ("arial" %in% names(grDevices::windowsFonts())) { "arial" } else { "sans" }),
          axis.title.x = ggplot2::element_blank(), #element_text(colour = "white"),
          axis.text.x = ggplot2::element_text(colour = "white"), axis.title.y = ggplot2::element_blank(),
          axis.text.y = ggplot2::element_blank(), axis.ticks.y = ggplot2::element_blank(),
          line = ggplot2::element_blank(), plot.margin = ggplot2::unit(c(0.25,-0.275, 0.25,0.25), "cm"))
      
      g1 = g1 + plot_opts
      t1 = t1 + table_opts
      # title = plot_title(data, dependent, dependent_label = dependent_label,
      #                    prefix = prefix, suffix = suffix)
      
      
      #plot.out <-
      gridExtra::grid.arrange(t1, g1, ncol = 2, widths = c(3, 2)#,
                              # top = grid::textGrob(title, x = 0.02, y = 0.2, gp = grid::gpar(fontsize = title_text_size),
                              #                      just = "left")
      )
      
      # grDevices::windowsFonts(sans = windowsFont(sansF[[1]]))
      #
      # plot.out
      
    } else {
      tabs
    }
  }









# Tab logit launch ----


# Adding new engine in poisson_reg
svglm2 <- function(formula, family = gaussian(), data, weights, ...) {
  data    <- add_column(data, wt = weights)
  outcome <- as.character(formula)[2]
  
  formula <- paste0(outcome, " ~ ", 
                    paste0(names(data)[!names(data) %in% c(outcome, "wt")], 
                           collapse = " + ")
  ) |>
    as.formula()
  
  design <- survey::svydesign(ids = ~ 1, data = data, weights = ~ wt)
  
  survey::svyglm(formula = as.formula(formula), design = design, family = family, ...)
}

# #svglm2(score_intensite ~ SEXE + AGE4 + DIPLOME4 + PPP1 + ENCADR + EMP4reg + NBSALA2 + cah_ORGA, 
# #       family = quasibinomial(), data = data, weights = as.double(data$pondqaa))
# #
# #svglm2(score_intensite ~ cah_ORGA, 
# #       family = quasibinomial(), data = data, weights = as.double(data$pondqaa))
# 
# svglm2(score_intensite ~ ., 
#        family = quasibinomial(), data = select(data, -pondqaa), 
#        weights = as.double(data$pondqaa))
# 
# svglm2(score_intensite ~ ., 
#        family = quasibinomial(), data = select(data, -pondqaa), 
#        weights = as.double(data$pondqaa))



#https://rdrr.io/github/tidymodels/poissonreg/src/R/poisson_reg_data.R
parsnip::set_model_engine("poisson_reg", "regression", "svglm2")
parsnip::set_dependency("poisson_reg", "svglm2", "stats")
parsnip::set_dependency("poisson_reg", "svglm2", "survey")
parsnip::set_dependency("poisson_reg", "svglm2", "poissonreg")

parsnip::set_fit(
  model = "poisson_reg",
  eng = "svglm2",
  mode = "regression",
  value = list(
    interface = "formula",
    protect = c("formula", "data", "weights"),
    func = c(fun = "svglm2"), #c(pkg = "stats", fun = "svglm2"),
    defaults = list(family = expr(stats::quasibinomial))
  )
)

parsnip::set_encoding(
  model = "poisson_reg",
  eng = "svglm2",
  mode = "regression",
  options = list(
    predictor_indicators = "traditional",
    compute_intercept = TRUE,
    remove_intercept = TRUE,
    allow_sparse_x = FALSE
  )
)

parsnip::set_pred(
  model = "poisson_reg",
  eng = "svglm2",
  mode = "regression",
  type = "numeric",
  value = list(
    pre = NULL,
    post = NULL,
    func = c(fun = "predict"),
    args =
      list(
        object = expr(object$fit),
        newdata = expr(new_data),
        type = "response"
      )
  )
)

parsnip::set_pred(
  model = "poisson_reg",
  eng = "svglm2",
  mode = "regression",
  type = "raw",
  value = list(
    pre = NULL,
    post = NULL,
    func = c(fun = "predict"),
    args = list(object = expr(object$fit), newdata = expr(new_data))
  )
)

# https://github.com/cran/parsnip/blob/master/R/logistic_reg_data.R
parsnip::set_model_engine("logistic_reg", "classification", "svglm2")
parsnip::set_dependency("logistic_reg", "svglm2", "stats")

parsnip::set_fit(
  model = "logistic_reg",
  eng = "svglm2",
  mode = "classification",
  value = list(
    interface = "formula",
    protect = c("formula", "data", "weights"),
    func = c(fun = "svglm2"), #c(pkg = "stats", fun = "svglm2"),
    defaults = list(family = expr(stats::quasibinomial))
  )
)

parsnip::set_encoding(
  model = "logistic_reg",
  eng = "svglm2",
  mode = "classification",
  options = list(
    predictor_indicators = "traditional",
    compute_intercept = TRUE,
    remove_intercept = TRUE,
    allow_sparse_x = FALSE
  )
)

parsnip::set_pred(
  model = "logistic_reg",
  eng = "svglm2",
  mode = "classification",
  type = "class",
  value = list(
    pre = NULL,
    post = parsnip:::prob_to_class_2,
    func = c(fun = "predict"),
    args =
      list(
        object = quote(object$fit),
        newdata = quote(new_data),
        type = "response"
      )
  )
)

parsnip::set_pred(
  model = "logistic_reg",
  eng = "svglm2",
  mode = "classification",
  type = "prob",
  value = list(
    pre = NULL,
    post = function(x, object) {
      x <- tibble(v1 = 1 - x, v2 = x)
      colnames(x) <- object$lvl
      x
    },
    func = c(fun = "predict"),
    args =
      list(
        object = quote(object$fit),
        newdata = quote(new_data),
        type = "response"
      )
  )
)

parsnip::set_pred(
  model = "logistic_reg",
  eng = "svglm2",
  mode = "classification",
  type = "raw",
  value = list(
    pre = NULL,
    post = NULL,
    func = c(fun = "predict"),
    args =
      list(
        object = quote(object$fit),
        newdata = quote(new_data)
      )
  )
)

parsnip::set_pred(
  model = "logistic_reg",
  eng = "svglm2",
  mode = "classification",
  type = "conf_int",
  value = list(
    pre = NULL,
    post = parsnip:::logistic_lp_to_conf_int,
    func = c(fun = "predict"),
    args =
      list(
        object = quote(object$fit),
        newdata = quote(new_data),
        se.fit = TRUE,
        type = "link"
      )
  )
)
