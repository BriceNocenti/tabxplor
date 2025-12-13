






## multi_logit, with new parsnip engine survey weights ----



# Bug correction :
#  - Pass characters in factors

# Enhancements : 
#  - Split 3lv+ factors in binary vars (what is better, multinomial or that ?) ?

multi_logit <- function(data, dependent, predictors_sequence, split_var = NULL, wt = NULL,
                        nb_questions, 
                        odds_ratios = TRUE, marginal_effects = FALSE, signif = TRUE,
                        add_pct = FALSE, add_n = FALSE, empirical_odds_ratio = FALSE, 
                        inverse_two_level_factors = TRUE, cleannames = NULL, ci = FALSE,
                        subtext = "") {
  
  cleannames <-
    if (is.null(cleannames)) { getOption("tabxplor.cleannames") } else {cleannames}
  
  variables_list <- c(dependent, 
                      purrr::flatten_chr(predictors_sequence), 
                      split_var, wt
  )
  
  data <- dplyr::select(data, tidyselect::all_of(variables_list))
  
  dependent_class <- map_chr(data[dependent], class)
  data <- data |> # enlever levels inutilisées dans variables dependantes
    dplyr::mutate(dplyr::across(
      tidyselect::all_of(dependent) & where(is.factor), 
      forcats::fct_drop
    ))
  
  dependent_min   <- map_if(data[dependent], dependent_class %in% c("numeric", "integer"),
                            ~ min(., na.rm = TRUE), .else = ~ 0L) |> purrr::flatten_int()
  dependent_max   <- map_if(data[dependent], dependent_class %in% c("numeric", "integer"),
                            ~ max(., na.rm = TRUE), .else = ~ 1L) |> purrr::flatten_int()
  dependent_int   <- map_lgl(data[dependent], ~rlang::is_integerish(.) & !is.factor(.))
  dependent_nlv   <- map_if(data[dependent], dependent_class == "factor", 
                            ~nlevels(as.factor(.)), .else = ~ 0L) |> flatten_int()
  
  
  factors_2lv <- dependent_class %in% c("factor", "character") & dependent_nlv == 2 | 
    (dependent_int & dependent_min == 0 & dependent_max == 1)
  
  factors_3lv <- dependent_class == "factor" & dependent_nlv >= 3
  if (any(factors_3lv)) stop(paste0("some factors have more than 2 levels: ", 
                                    paste0(names(factors_3lv)[factors_3lv], 
                                           collapse = ", ")))
  
  if (any(factors_2lv & dependent_int)) {
    data <- data |> mutate(across(all_of(dependent[factors_2lv & dependent_int]), 
                                  ~ as.factor(.) |> 
                                    `levels<-`(c(paste0("Pas ", dplyr::cur_column()), 
                                                 dplyr::cur_column() ) )) )   
  }
  
  if(any(factors_2lv & !dependent_int & inverse_two_level_factors)) {
    data <- data |> mutate(across(all_of(dependent[factors_2lv & !dependent_int]), 
                                  ~ fct_rev(as.factor(.)) ))  
  }
  
  
  
  integer3 <- !factors_2lv & dependent_int
  
  if (any(integer3)) requireNamespace("poissonreg", quietly = TRUE)
  
  # double_01 <- !factors_2lv &!dependent_int & dependent_class == "numeric" & 
  #   dependent_min >= 0 & dependent_max <= 1
  
  if (missing(nb_questions)) {
    nb_questions <- if_else(integer3, as.integer(dependent_max), 1L)
    
  } else {
    nb_questions <- vctrs::vec_recycle(vctrs::vec_cast(nb_questions, integer()), 
                                       length(dependent))
  }
  nb_questions <- nb_questions |> purrr::set_names(dependent)
  
  if (any(integer3)) {
    data <- data |> 
      mutate(across(all_of(dependent[integer3]), ~ ./nb_questions[cur_column()] ))  
  }
  
  
  reference_population <- "Reference population"
  # paste0( #"Population de réference (modèle complet): ",
  #   paste0(
  #     unique(purrr::flatten_chr(predictors_sequence)) %>%
  #     purrr::keep(purrr::map_lgl(., ~ dplyr::pull(data, .) |> is.factor() )) |>
  #     purrr::map_chr(~ dplyr::pull(data, .) |> levels() |> dplyr::first()), 
  #          collapse = ", "), 
  #   " (modèle complet)"
  # )
  
  predictors <- predictors_sequence |> purrr::flatten_chr() |> unique()
  
  data <- data |> dplyr::mutate(NA_pred = if_any(.cols = all_of(predictors), is.na)) 
  message(paste0(sum(data$NA_pred), " rows with NA in at least one predictor were removed"))
  data <- data |> dplyr::filter(!NA_pred) |> dplyr::select(-NA_pred)
  
  if (cleannames) {
    data <- data |> dplyr::mutate(across(
      where(is.factor), 
      ~forcats::fct_relabel(., ~stringr::str_remove_all(., tabxplor:::cleannames_condition()))
    ))
  }
  
  
  if (!is.null(wt)) {
    data <- data |> dplyr::mutate(
      base_wt    = !!rlang::sym(wt), 
      !!rlang::sym(wt) := hardhat::importance_weights(!!rlang::sym(wt))
    )
  }
  
  
  # Specify the models
  if (is.null(split_var)) {
    models_vars <- tidyr::crossing(
      dependent    = forcats::as_factor(dependent), 
      predictors = predictors_sequence,
      data       = list(data)
    ) |> 
      dplyr::mutate(
        lv1  = purrr::map2(
          data, predictors, 
          ~ purrr::map_chr(.y, function(.pred) dplyr::first(levels(forcats::fct_drop(dplyr::pull(.x, .pred)))) )
        ), 
        split_var = ""
      )
    
  } else {
    data_split <- data |> 
      dplyr::rename(any_of(c("split_var" = split_var))) |>
      dplyr::group_by(split_var) |> group_nest() |> 
      dplyr::filter(if_any(1, ~!is.na(.))) |> 
      dplyr::rename(data = data)
    
    models_vars <- tidyr::crossing(dependent  = forcats::as_factor(dependent), 
                                   predictors = predictors_sequence,
                                   split_var  = data_split$split_var) |> 
      dplyr::left_join(data_split, by = "split_var") |> 
      dplyr::mutate(
        lv1  = purrr::map2(
          data, predictors, 
          ~ map_chr(.y, function(.pred) dplyr::first(levels(forcats::fct_drop(dplyr::pull(.x, .pred)))) )
        ), 
      )
    
  }
  
  # # WRONG NUMBER OF LEVELS SOMEWHERE ?
  # models_vars <- models_vars |> 
  #   dplyr::mutate(
  #     data = purrr::map(data, 
  #                       ~ dplyr::mutate(., across(where(is.factor), fct_drop)) )
  #     )
  
  
  if (is.null(wt)) {
    glm_model <- 
      parsnip::poisson_reg() |> # since logistic_reg() means binary dependent (2 lv factor)
      parsnip::set_engine("glm", family = stats::quasibinomial(link = "logit") )
    
    binary_model <- 
      parsnip::logistic_reg() |> 
      parsnip::set_engine("glm", family = stats::binomial(link = "logit") )
    
    
    models_vars <- models_vars |> 
      dplyr::mutate(model_type = dplyr::if_else(factors_2lv[dependent], 
                                                list("bin" = binary_model), 
                                                list("glm" = glm_model)), 
                    
                    nb_questions = nb_questions[dependent],
                    
                    # wflow_var = list(workflows::workflow_variables(
                    #   dependent    = tidyselect::matches(dependent),
                    #   predictors  = tidyselect::all_of(predictors)
                    # )),
                    
                    
                    # recipe = pmap(list(data, dependent, predictors), 
                    #               function(.dat, .dependent, .pred)
                    #               ~ recipe(.dat) |> 
                    #                 update_role(all_of(.dependent), new_role = "dependent") |>
                    #                 update_role(all_of(.pred), new_role = "predictor")
                    #               ),
                    #               
                    #wflow = purrr::map2(recipe, model_type, ~ workflows::workflow(.x, .y))
                    
                    wflow = pmap(list(data, dependent, predictors, model_type), 
                                 function(.dat, .dependent, .pred, .model)
                                   workflow() |> 
                                   add_model(.model) |> 
                                   add_variables(outcomes  = all_of(.dependent), 
                                                 predictors = all_of(.pred))
                    ), 
                    
                    
      )
    
    models_vars <- models_vars |> 
      dplyr::mutate(
        empirical_OR = purrr::pmap(
          list(data, predictors, dependent, names(model_type)), 
          ~ if (..4 == "bin" & (empirical_odds_ratio | add_pct) ) {
            tabs <- 
              withr::with_options(
                list(tabxplor.output_kable = FALSE, 
                     tabxplor.compact = FALSE, 
                     tabxplor.pvalue_lines = FALSE 
                ), 
                tabxplor::tab_many(..1, all_of(..2), all_of(..3), 
                                   pct = "row", OR = "or", na = "drop", 
                                   add_n = FALSE, cleannames = cleannames
                ))
            
            
          } else {
            NULL
          }
        ), 
      )   
    # models_vars$empirical_OR
    
    models_vars <- models_vars |> 
      dplyr::mutate(
        empirical_OR = purrr::pmap(
          list(data, predictors, dependent), 
          ~ withr::with_options(
            list(tabxplor.output_kable = FALSE, 
                 tabxplor.compact = FALSE, 
                 tabxplor.pvalue_lines = FALSE 
            ), 
            tabxplor::tab_many(..1, all_of(..2), all_of(dependent), 
                               pct = "row", OR = "or", na = "drop", add_n = FALSE
            ))
        ), 
      )
    
  } else { # with survey weights
    # dw <- survey::svydesign(ids = ~ 1, data = data, weights = ~ pondqaa)
    
    glm_model <- 
      parsnip::poisson_reg() |> # since logistic_reg() means binary dependent (2 lv factor)
      parsnip::set_engine("svglm2", family = stats::quasibinomial(link = "logit") #, 
                          #survey_weights = wt
      )
    
    binary_model <- 
      parsnip::logistic_reg() |> 
      parsnip::set_engine("svglm2", family = stats::quasibinomial(link = "logit") #, 
                          #weights = as.formula(paste0("~", wt ))
      )
    
    models_vars <- models_vars |> 
      dplyr::mutate(model_type = dplyr::if_else(factors_2lv[dependent], 
                                                list("bin" = binary_model), 
                                                list("glm" = glm_model)), 
                    nb_questions = nb_questions[dependent],
                    
                    wflow = pmap(list(data, dependent, predictors, model_type), 
                                 function(.dat, .dependent, .pred, .model)
                                   workflow() |> 
                                   add_model(.model) |> 
                                   add_variables(outcomes   = all_of(.dependent), 
                                                 predictors = all_of(.pred)    ) |>
                                   add_case_weights(!!rlang::sym(wt))
                    ), 
                    
                    
      )
    
    models_vars <- models_vars |> 
      dplyr::mutate(
        empirical_OR = purrr::pmap(
          list(data, predictors, dependent, names(model_type)), 
          ~ if (..4 == "bin" & (empirical_odds_ratio | add_pct) ) {
            tabs <- withr::with_options(
              list(tabxplor.output_kable = FALSE, 
                   tabxplor.compact = FALSE, 
                   tabxplor.pvalue_lines = FALSE 
              ), 
              tabxplor::tab_many(..1, all_of(..2), all_of(..3), 
                                 pct = "row", OR = "or", na = "drop", add_n = FALSE,
                                 wt = base_wt, cleannames = cleannames
              ))
          } else {
            NULL
          }
        ), 
      )   
    # models_vars$empirical_OR
  }
  
  
  models_vars <- models_vars |> 
    dplyr::mutate(
      empirical_OR = purrr::pmap(
        list(empirical_OR, dependent, names(model_type)), 
        ~ if (..3 == "bin" & (empirical_odds_ratio | add_pct) ) {
          tabs <- purrr::map(
            ..1, 
            ~ dplyr::mutate(.,
                            var = factor(names(.)[[1]]), 
                            .before = 1                 ) |>
              dplyr::rename_with(~"levels", .cols = 2)
          ) |> 
            dplyr::bind_rows()
          
          n_vect <- tabs |> 
            select(where(is_fmt)) |> 
            mutate(across(where(is_fmt), ~ .$n)) |> 
            rowSums() |>
            as.integer()
          
          tabs <- tabs |>
            dplyr::mutate(dplyr::across(where(is_fmt), ~ dplyr::mutate(., n = n_vect))) 

          tabs <- dplyr::filter(tabs, !is_totrow(tabs))
          
          # tabs <- dplyr::mutate(tabs, ref = tabxplor::is_refrow(tabs))
          
          tabs |> 
            dplyr::select(-3) |> 
            dplyr::rename_with(~ "Empirical OR", .cols = where(is_fmt)) # function(.var) as.character(..2)
          
        } else {
          NULL
        }
      ), 
    )   
  # models_vars$empirical_OR
  
  
  
  
  #With repice ?
  # logit_recipe <-
  #   recipe() %>%
  #   step_dummy(all_nominal_predictors())
  
  
  # Run the models
  models <- models_vars |> 
    dplyr::mutate(fit = purrr::map2(wflow, data, ~ fit(.x, data = .y)))
  
  
  
  # Retrieve coefficients, calculate Odds, probabilities and marginal effects
  models <- models |> 
    dplyr::mutate(OR_table = purrr::map2(fit, nb_questions, 
                                         ~ readable_OR(.x, n = .y, format = FALSE, 
                                                       ci = ci)))
  
  models <- models |> 
    dplyr::mutate(
      OR = purrr::map(OR_table, 
                      ~ dplyr::select(., parameter, OR = Odds_ratio, signif, 
                                      any_of(c("OR_inf", "OR_sup")))), 
      ME = purrr::map(OR_table, ~ dplyr::select(., parameter, ME = marginal_effect, 
                                                OR = Odds_ratio, 
                                                tidyselect::any_of(c("p" = "prob × n",
                                                                     "p" = "prob")), 
                                                signif)), 
      
      model_name = dplyr::if_else(split_var == "", 
                                  true  = names(predictors), 
                                  false = paste0(names(predictors), ":by:", split_var)
      )
      
      #   paste0(names(predictors), ":by:", split_var) |>
      # stringr::str_remove("\\:by\\:$")
    )
  
  
  # # Nested models comparison: drop-in-deviance test
  # models <- models |> 
  #   mutate(n = dplyr::row_number()) |> 
  #   group_by(dependent, split_var) |>
  #   mutate(
  #     nested_with = map2_int(
  #       predictors, list(predictors), 
  #       ~ map_lgl(.y, function(.pred) !identical(.x, .pred) & all(.pred  %in% .x)) |>
  #         which() |> dplyr::last() #|> tidyr::replace_na("")
  #     ), 
  #     
  #     nested_with = n[nested_with]
  #   ) |> 
  #   dplyr::ungroup() |>
  #   select(-n) |> 
  #   mutate(drop_in_dev_test = NA_real_, 
  #          fit_nested_with = fit[nested_with])
  # 
  # models[!is.na(models$nested_with),] <- models[!is.na(models$nested_with),] |> 
  #   mutate(drop_in_dev_test = purrr::map2_dbl(fit, fit_nested_with,
  #                                         ~ anova(extract_fit_engine(.x),
  #                                                 extract_fit_engine(.y), 
  #                                                 test = "Chisq") |>
  #                                           pull(`Pr(>Chi)`) |> dplyr::nth(2)
  #   ))
  # # anova(extract_fit_engine(models$fit[[1]]),  
  # #       extract_fit_engine(models$fit[[3]]), 
  # #       test = "Chisq")
  # 
  # models <- models |> 
  #   mutate(drop_in_dev_test = stringr::str_c(
  #     "/ ", stringr::str_replace(model_name[nested_with], ":by:", " "), ", ",
  #     if_else(drop_in_dev_test < 0.05, 
  #             true  = "signif", #"signif drop in deviance", 
  #             false = "not signif"), # "no signif drop in deviance"), 
  #     " (p=", round(drop_in_dev_test, digits = 3), ")" ) 
  #   ) |> 
  #   select(-nested_with, -fit_nested_with)
  # 
  # # TO DO : notes dans les noms des modèles + renvoie au subtext ? NON
  
  
  
  # Odds-ratio tables
  if (odds_ratios) {
    OR <- models |> 
      dplyr::select(dependent, model_name, OR, predictors, any_of(c("OR_inf", "OR_sup"))) |> 
      dplyr::mutate(
        OR = purrr::map2(
          OR, model_name, 
          ~ dplyr::rename(
            .x, 
            tidyselect::all_of(c(purrr::set_names("OR", paste0("OR_", .y)), 
                                 purrr::set_names("signif", paste0("s_", .y)) )), 
            tidyselect::any_of(c(purrr::set_names("OR_inf", paste0("OR_inf_", .y)), 
                                 purrr::set_names("OR_sup", paste0("OR_sup_", .y))  )) 
          )
        ) ) |> 
      dplyr::group_by(dependent) |> 
      dplyr::group_split() 
    
    OR <- OR |>
      purrr::set_names(purrr::map_chr(OR, ~ as.character(.$dependent[1]))) |> 
      purrr::map(~ purrr::reduce(.$OR, ~ dplyr::full_join(.x, .y, by = "parameter")))
    
    
    OR <- OR |> 
      purrr::map(~ dplyr::mutate(
        ., 
        var = purrr::map(parameter,
                         function(.param) purrr::map_lgl(predictors, 
                                                         function(.pred) str_detect(.param,
                                                                                    paste0("^", .pred))
                         ) ) |> 
          purrr::map(~ predictors[which(.)]) |> 
          purrr::map(~ if (length(.) == 0) {"Constant"} else {.}) |>
          purrr::flatten_chr() |> 
          forcats::as_factor() |> 
          forcats::fct_relevel(c("Constant", 
                                 predictors)), 
        
        levels = forcats::as_factor(dplyr::if_else(
          condition = stringr::str_detect(parameter, "Intercept"), 
          true      = reference_population,  
          false     = stringr::str_remove(parameter, as.character(var))
        )), 
        
        ref         = var == "Constant"
      ) |> 
        dplyr::select(var, levels, tidyselect::everything(), -parameter)
      )
    # OR[[1]]
    
    OR <- 
      purrr::pmap(list(OR, models$predictors, models$lv1, 
                       models$empirical_OR, names(models$model_type) ), 
                  ~ {
                    rename_vect <- purrr::set_names(..2, ..3)
                    
                    new_data <- dplyr::select(..1, -levels) |> 
                      dplyr::filter(var != "Constant") |>
                      dplyr::distinct(var, .keep_all = TRUE) |>
                      dplyr::mutate(
                        dplyr::across(tidyselect::starts_with("OR_"), ~ 1    ), 
                        dplyr::across(tidyselect::starts_with("OR_inf"), ~ NA_real_), 
                        dplyr::across(tidyselect::starts_with("OR_sup"), ~ NA_real_), 
                        dplyr::across(tidyselect::starts_with("s_" ), ~ "   "),
                        levels = forcats::fct_recode(var, !!!rename_vect)     ,
                        ref    = TRUE
                      )  |>
                      dplyr::select(var, levels, tidyselect::everything())
                    
                    tabs <- dplyr::bind_rows(new_data, ..1) |> dplyr::arrange(var)
                    
                    # bind empirical odds ratio (and pct) tables
                    if (..5 == "bin" & (empirical_odds_ratio | add_pct) ) {
                      tabs <- tabs |> dplyr::left_join(..4, by = c("var", "levels"))
                    } 
                    
                    tabs
                  }
      )
    
    OR <-  
      purrr::map2(OR, names(models$model_type),  # models$dependent
                  ~ (if (.y == "bin" & (empirical_odds_ratio | add_pct) ) {
                    dplyr::mutate(
                      .x, 
                      
                      var2 = var, #otherwise tabxplor internal mutate use var field (variance)
                      
                      # !!rlang::sym(..3) := 
                      pct = `Empirical OR` |> 
                        set_display("pct") |> 
                        set_color("diff") |>
                        tidyr::replace_na(tabxplor:::fmt0("pct", type = "row")) |>
                        dplyr::mutate(or = NA_real_,
                                      pct = dplyr::if_else(
                                        var2 != "Constant",
                                        true  = pct,
                                        false = NA_real_),
                                      in_refrow = ref 
                        ),
                      
                      n = set_display(pct, "n") |> set_color("") |> 
                        dplyr::mutate(or = NA_real_, pct = NA_real_, diff = NA_real_, 
                                      n = dplyr::if_else(
                                        var2 != "Constant",
                                        true  = n,
                                        false = NA_integer_),
                                      in_refrow = ref 
                        ), 
                      
                      `Empirical OR` = `Empirical OR` |> 
                        tidyr::replace_na(tabxplor:::fmt0("or", type = "row")) |> 
                        set_color("OR") |> 
                        set_display("or") |> 
                        set_col_var("Empirical OR") |>
                        dplyr::mutate(pct = NA_real_, #rep(NA_real_, length(pct)), 
                                      or = dplyr::if_else(
                                        var2 != "Constant",
                                        true  = or,
                                        false = NA_real_),
                                      in_refrow = ref 
                        ),
                      
                      dplyr::across(where(is.double) & !starts_with(c("OR_inf", "OR_sup")), 
                                    ~ fmt(
                                      n    = `Empirical OR`$n, 
                                      wn   = `Empirical OR`$wn,
                                      # pct  = dplyr::if_else(
                                      #   var2 == "Constant", 
                                      #   true  = stats::weighted.mean(`Empirical OR`$pct, 
                                      #                                w = `Empirical OR`$wn, 
                                      #                                na.rm = TRUE), 
                                      #   false = `Empirical OR`$pct),
                                      # diff = dplyr::if_else(
                                      #   var2 == "Constant", 
                                      #   true  = 0, 
                                      #   false = `Empirical OR`$diff),
                                      type      = "row", 
                                      digits    = 2L, 
                                      or        = .x, 
                                      display   = "or", 
                                      # diff      = dplyr::if_else(
                                      #   !str_detect(replace_na(eval_tidy(sym(str_replace(cur_column(),
                                      #                                                    "^OR_", "s_"))), ""),
                                      #               "/*" # in gray when not in 90% ci
                                      #   ) | var2 == "Constant", 
                                      #   true  = 0, 
                                      #   false = .), 
                                      
                                      ref       = "1", 
                                      color     = "OR", 
                                      #in_totrow = row_number() == 1,
                                      in_refrow = ref, # dplyr::row_number() == 1, 
                                      col_var = dplyr::cur_column() |> stringr::str_remove("^OR_") |>
                                        stringr::str_remove(":by:.*$"), 
                                      comp_all = FALSE, 
                                    ))
                    )
                    
                  } else {
                    dplyr::mutate(
                      .x, 
                      dplyr::across(where(is.double) & !starts_with(c("OR_inf", "OR_sup")), 
                                    ~ fmt(
                                      n = rep(0, length(.x)), 
                                      type      = "row", 
                                      digits    = 2L, 
                                      or        = .x, 
                                      display   = "or", 
                                      ref       = "1", 
                                      color     = "OR", 
                                      #in_totrow = row_number() == 1,
                                      in_refrow = ref, # dplyr::row_number() == 1, 
                                      col_var = dplyr::cur_column() |> stringr::str_remove("^OR_") |>
                                        stringr::str_remove(":by:.*$"), 
                                      comp_all = FALSE, 
                                    ))
                    )
                  }) |> 
                    dplyr::rename_with(~ stringr::str_replace(., ":by:", " ") |> 
                                         stringr::str_replace("^OR_", "OR ") ) |>
                    dplyr::select(-tidyselect::any_of(c("ref", "var2"))) |>
                    dplyr::select(var, levels, any_of(c("pct", "Empirical OR")), 
                                  tidyselect:: everything() & -any_of("n"), any_of("n")) # |>
                  #dplyr::rename_with(~ str_remove(., "^OR_")) |>
                  #dplyr::arrange(var) |>
                  #new_tab()
      )
    
    
    if (!add_pct) {
      OR <- OR |> 
        map(~ dplyr::select(., -tidyselect::any_of(c("pct"))))
    }
    
    if (!empirical_odds_ratio) {
      OR <- OR |> 
        map(~ dplyr::select(., -tidyselect::any_of(c("Empirical OR"))))
    }
    
    if (!add_n) {
      OR <- OR |> 
        map(~ dplyr::select(., -tidyselect::any_of(c("n"))))
    }
    
    if (signif) {
      OR <- OR |>purrr::map(
        ~ dplyr::rename_with(., 
                             .cols = starts_with("s_"), 
                             .fn = ~ paste0("s", seq_along(.) ))
      )
    } else {
      OR <- OR |> 
        map(~ dplyr::select(., -starts_with("s_")))
    }
  }
  
  
  # Marginal effects tables
  if (marginal_effects) {
    ME <- models |> 
      dplyr::select(dependent, model_name, ME) |> 
      dplyr::mutate(ME = purrr::map2(ME, model_name, 
                                     ~ dplyr::rename(.x, all_of(c(purrr::set_names("ME", paste0("ME_", .y)),
                                                                  purrr::set_names("OR", paste0("OR_", .y)),
                                                                  purrr::set_names("p" , paste0("p_", .y)),
                                                                  purrr::set_names("signif", paste0("s_", .y)))) ) |>
                                       dplyr::mutate(dplyr::across(starts_with(c("ME_", "p_")), get_num))
      ) ) |> 
      dplyr::group_by(dependent) |> 
      dplyr::group_split()
    
    ME <- ME |>
      purrr::set_names(purrr::map_chr(ME, ~ as.character(.$dependent[1]))) |> 
      purrr::map(~ purrr::reduce(.$ME, ~ dplyr::full_join(.x, .y, by = "parameter")))
    
    ME <- ME |> 
      purrr::map(~ dplyr::mutate(
        ., 
        var = purrr::map(
          parameter, 
          function(.param) purrr::map_lgl(predictors, 
                                          function(.pred) stringr::str_detect(.param,
                                                                              paste0("^", .pred))
          ) ) |> 
          purrr::map(~ predictors[which(.)]) |> 
          purrr::map(~ if (length(.) == 0) {"Constant"} else {.}) |>
          purrr::flatten_chr() |> 
          as_factor() |> forcats::fct_relevel(c("Constant", 
                                                predictors)), 
        
        levels = as_factor(dplyr::if_else(
          condition = stringr::str_detect(parameter, "Intercept"), 
          true      = reference_population,  
          false     = stringr::str_remove(parameter, as.character(var))
        ))
      ) |> 
        dplyr::select(var, levels, everything(), -parameter)
      )
    
    ME <- ME |>
      purrr::map(~  dplyr::mutate(., dplyr::across(
        where(is.double) & starts_with("ME_"), 
        ~ fmt(
          0, 
          type      = "OR", 
          digits    = 2L, 
          or        = dplyr::if_else(
            var == "Constant", 
            true  = replace_na(rlang::eval_tidy(sym(str_replace(cur_column(),
                                                                "^ME_", "p_"))), 0), 
            false = .), 
          
          diff      = dplyr::if_else(
            !str_detect(replace_na(rlang::eval_tidy(sym(str_replace(cur_column(),
                                                                    "^ME_", "s_"))), ""),
                        "/*" # in gray when not in 90% ci
            ) | var == "Constant", 
            true  = 0, 
            false = replace_na(rlang::eval_tidy(sym(str_replace(cur_column(),
                                                                "^ME_", "OR_"))), 1) ), 
          
          ref       = "1", 
          color     = "diff", 
          #in_totrow = row_number() == 1,
          in_refrow = dplyr::row_number() == 1, 
          col_var = dplyr::cur_column() |> str_remove("^ME_") |>
            stringr::str_remove(":by:.*$"), 
          comp_all = FALSE, 
        ))
      ) |> 
        dplyr::select(-starts_with(c("s_", "OR_", "p_"))) |>
        dplyr::rename_with(~ stringr::str_replace(., ":by:", " ") |> 
                             stringr::str_replace("^ME_", "ME ") ) #|>
      #dplyr::rename_with(~ str_remove(., "^ME_")) |>
      #dplyr::arrange(var) |>
      #new_tab()
      )
  }
  
  if (marginal_effects & odds_ratios) {
    res <- purrr::map2(OR, ME, ~dplyr::left_join(dplyr::mutate(.x, ` ` = ""), .y,
                                                 by = c("var", "levels")) )
  } else if (marginal_effects) {
    res <- ME
  } else if (odds_ratios) {
    res <- OR
  } else{
    return(models)
  }
  
  res_names <- paste0(names(res), dplyr::if_else(nb_questions > 1, 
                                                 true  = paste0(" (n=", nb_questions, ")"), 
                                                 false = ""))
  
  purrr::set_names(res, res_names) |> 
    purrr::map(~ tabxplor::new_tab(., subtext = subtext) |> dplyr::group_by(var))
}




tab_logit <- function(data, dependent, predictors, split_var = NULL, wt = NULL,
                      full_table = FALSE,
                      inverse_two_level_factors = TRUE, 
                      cleannames = NULL, 
                      subtext = "") { # nb_questions
  if(length(dependent) > 1 & full_table) {
    stop("not possible to pass several `dependent` variables with `full_table = TRUE`")
  } 
  
  stopifnot(is.character(predictors))
  
  cleannames <-
    if (is.null(cleannames)) { getOption("tabxplor.cleannames") } else {cleannames}
  
  tabs <- multi_logit(data = data,
                      dependent = dependent, 
                      predictors_sequence = list("model" = predictors),
                      split_var = split_var, 
                      wt = wt,
                      odds_ratios = TRUE, marginal_effects = FALSE, signif = TRUE,
                      ci = FALSE,
                      add_pct = full_table, add_n = full_table, empirical_odds_ratio = full_table, 
                      inverse_two_level_factors = inverse_two_level_factors, 
                      cleannames = cleannames) # , subtext = subtext
  
  dependent_lv1 <- purrr::map_chr(
    data[dependent], 
    ~levels(forcats::fct_drop(.))[if(inverse_two_level_factors) {1} else {2}]
  )
  
  if (cleannames) {
    dependent_lv1 <- 
      stringr::str_remove_all(dependent_lv1, tabxplor:::cleannames_condition()) 
  } 
  
  dependent_lv1 <- paste0(dependent_lv1, ": OR")
  
  
  if(length(dependent) >= 2) {
    
    tabs <- 
      purrr::pmap(list(tabs, dependent_lv1, seq_along(dependent)), 
                  ~ dplyr::rename(..1, 
                                  all_of(purrr::set_names("OR model", ..2)),
                                  all_of(purrr::set_names("s1", paste0("s", ..3) )),
                                  
                  )
      ) |>
      purrr::reduce(~left_join(.x, .y, by = c("var", "levels")))
    
  } else {
    tabs <- tabs[[1]] |>
      dplyr::rename(all_of(purrr::set_names("OR model", dependent_lv1)) )
  }
  
  tabs |> 
    # dplyr::mutate(dplyr::across(
    #   where(is.character) & tidyselect::starts_with("s"), 
    #   htmltools::htmlEscape # otherwise stars *** break the html doc...
    # )) |> 
    tabxplor::new_tab(subtext = subtext) |> 
    dplyr::group_by(var) 
}



