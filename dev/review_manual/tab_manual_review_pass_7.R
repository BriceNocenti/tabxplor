
# Manual reviews pass 6 ----

library(devtools)
load_all()
options(tabxplor.parallel = 8, tabxplor.cleannames = TRUE, tabxplor.print = "html") # options(tabxplor.print = "console")

gss_simple <- gss_cat_data_formatting() # gss_simple with merged levels, and first levels chosen for reference (colors, regressions)

data("tea", package = "FactoMineR")
tea_when_vars  <- c("breakfast", "tea.time", "evening", "lunch", "dinner", "always")
tea_where_vars <- c("home", "work", "tearoom", "friends", "resto", "pub")

tea <- tea |> 
  tibble::as_tibble() |> 
  dplyr::mutate(across(
    all_of(c(tea_when_vars, tea_where_vars)), 
    ~ (if (stringr::str_detect(levels(.)[1], "^Not")) {forcats::fct_rev(.)} else {.}) |> 
      forcats::fct_relabel(~ stringr::str_replace_all(., "\\.", " "))
  ))
# tea |> dplyr::select(all_of(tea_where_vars)) |> purrr::map(levels)
tea <- tea |> 
  score_from_lv1("tea_when" , vars_list = tea_when_vars) |> 
  score_from_lv1("tea_where", vars_list = tea_where_vars)







### tab_reg tests ----

#### the different main use cases ----
tab_reg(gss_simple, outcome = "married", predictors = c("race", "rincome", "relig", "age"),
        family = "binomial", empirical = TRUE
)



tab_reg(gss_simple, outcome = "married", predictors = c("race", "rincome", "relig", "age"),
        family = "binomial", effect = "marginal", measure = "ratio" , empirical = TRUE
)


# adjustment
tab_reg(gss_simple, outcome = "married", predictors = c("race", "rincome", "relig", "age"),
        family = "binomial", effect = "marginal", measure = "ratio" , empirical = TRUE, color = "adjustment"
)


# several outcomes
tab_reg(gss_simple, outcome = c("married", "tvhours"), predictors = c("race", "rincome", "relig", "age"),
        family = c("binomial", "poisson"), empirical = TRUE, 
)


# predictor’s list
tab_reg(gss_simple, outcome = "married", 
        predictors = list(race  = "race", 
                          two   = c("race", "rincome"), 
                          three = c("race", "rincome", "relig"), 
                          full  = c("race", "rincome", "relig", "age") ),
        family = "binomial", effect = "marginal", measure = "ratio" , empirical = TRUE, color ="adjustment"
)

# tab_vars 
tab_reg(gss_simple, outcome = "married", tab_vars = "race",
        predictors = c("rincome", "relig", "age", "tvhours"),
        family = "binomial", effect = "marginal", measure = "ratio" , color ="between_groups"
)


#### the different families × effects × measure columns names, displays, and footers

# binomial
## effect = "coefficient"
tab_reg(gss_simple, outcome = "married", predictors = c("race", "rincome", "relig", "age"),
        family = "binomial", empirical = TRUE #, measure = "odds_ratio"
) # Ok.
tab_reg(gss_simple, outcome = "married", predictors = c("race", "rincome", "relig", "age"),
        family = "binomial", empirical = TRUE, measure = "log"
) # The two names are homogeneous, it’s ok. Two legends blocks instead of 1.
tab_reg(gss_simple, outcome = "married", predictors = c("race", "rincome", "relig", "age"),
        family = "binomial", empirical = TRUE, measure = "ratio"
) # Ok. 
tab_reg(gss_simple, outcome = "married", predictors = c("race", "rincome", "relig", "age"),
        family = "binomial", empirical = TRUE, measure = "difference"
) # Ok.

## effect = "marginal"
tab_reg(gss_simple, outcome = "married", predictors = c("race", "rincome", "relig", "age"),
        family = "binomial", empirical = TRUE, effect = "marginal" #, measure = "difference"
)
tab_reg(gss_simple, outcome = "married", predictors = c("race", "rincome", "relig", "age"),
        family = "binomial", empirical = TRUE, effect = "marginal", measure = "ratio"
)

## effect = "at_reference"
tab_reg(gss_simple, outcome = "married", predictors = c("race", "rincome", "relig", "age"),
        family = "binomial", empirical = TRUE, effect = "at_reference" #, measure = "difference"
)
tab_reg(gss_simple, outcome = "married", predictors = c("race", "rincome", "relig", "age"),
        family = "binomial", empirical = TRUE, effect = "at_reference", measure = "ratio"
)

# gaussian
## effect = "coefficient"
tab_reg(gss_simple, outcome = "age", predictors = c("race", "rincome", "relig", "tvhours"),
        family = "gaussian", empirical = TRUE
) # Two legends blocks instead of 1.
tab_reg(gss_simple, outcome = "age", predictors = c("race", "rincome", "relig", "tvhours"),
        family = "gaussian", empirical = TRUE, measure = "ratio"
) # Two legends blocks instead of 1.

## effect = "marginal"
tab_reg(gss_simple, outcome = "age", predictors = c("race", "rincome", "relig", "tvhours"),
        family = "gaussian", empirical = TRUE, effect = "marginal"
) # Two legends blocks instead of 1.
tab_reg(gss_simple, outcome = "age", predictors = c("race", "rincome", "relig", "tvhours"),
        family = "gaussian", empirical = TRUE, effect = "marginal", measure = "ratio"
) # Seems wrong : Obs_diff and Model_mRoM. The math seems wrong : Obs_diff is +-. Two legends blocks instead of 1 (legend don’t match the diff one, which is multiplicative).

## effect = "at_reference"
tab_reg(gss_simple, outcome = "age", predictors = c("race", "rincome", "relig", "tvhours"),
        family = "gaussian", empirical = TRUE, effect = "at_reference"
) 
tab_reg(gss_simple, outcome = "age", predictors = c("race", "rincome", "relig", "tvhours"),
        family = "gaussian", empirical = TRUE, effect = "at_reference", measure = "ratio"
) # Seems wrong : Obs_diff and Model_refRoM. The math seems wrong : Obs_diff is +-. Two legends blocks instead of 1 (legend don’t match the diff one, which is multiplicative).


# poisson
## effect = "coefficient"
tab_reg(gss_simple, outcome = "tvhours", predictors = c("race", "rincome", "relig", "age"),
        family = "poisson", empirical = TRUE
) # this one is right
tab_reg(gss_simple, outcome = "tvhours", predictors = c("race", "rincome", "relig", "age"),
        family = "poisson", empirical = TRUE, measure = "log"
) # this one is right

## effect = "marginal"
tab_reg(gss_simple, outcome = "tvhours", predictors = c("race", "rincome", "relig", "age"),
        family = "poisson", empirical = TRUE, effect = "marginal"# , measure = "difference"
) # Obs-log(IRR), Model_mdiff : the log one seems wrong (it’s not a coeff but a marginal effect ?) ? Math seems ok. Two legends blocks instead of 1.
tab_reg(gss_simple, outcome = "tvhours", predictors = c("race", "rincome", "relig", "age"),
        family = "poisson", empirical = TRUE, effect = "marginal", measure = "ratio"
) # This one is right.

## effect = "at_reference"
tab_reg(gss_simple, outcome = "tvhours", predictors = c("race", "rincome", "relig", "age"),
        family = "poisson", empirical = TRUE, effect = "at_reference"# , measure = "difference"
) # Wrong name ? : Obs_log(IRR) and Model_refdiff. Math seem ok. Two legends blocks instead of 1.
tab_reg(gss_simple, outcome = "tvhours", predictors = c("race", "rincome", "relig", "age"),
        family = "poisson", empirical = TRUE, effect = "at_reference", measure = "ratio"
) # Ok. 

# summed-score binomial
## effect = "coefficient"
tab_reg(tea, outcome = "tea_where", family = "binomial", trials = length(tea_where_vars), 
        predictors = c("sex", "SPC", "Sport"),  empirical = TRUE
) # Ok for names and math. Strange col_var artifact: "tea_where:1" and "tea_where".
tab_reg(tea, outcome = "tea_where", family = "binomial", trials = length(tea_where_vars), 
        predictors = c("sex", "SPC", "Sport"), empirical = TRUE, measure = "ratio"
) # This one seems wrong : Obs_OR, Model_RR. Two legends blocks instead of 1. Strange col_var artifact: "tea_where:1" and "tea_where"
tab_reg(tea, outcome = "tea_where", family = "binomial", trials = length(tea_where_vars), 
        predictors = c("sex", "SPC", "Sport"), empirical = TRUE, measure = "difference"
) # Strange col_var artifact: "tea_where:1" and "tea_where". Two legends blocks instead of 1.

## effect = "marginal"
tab_reg(tea, outcome = "tea_where", family = "binomial", trials = length(tea_where_vars), 
        predictors = c("sex", "SPC", "Sport"), empirical = TRUE, effect = "marginal"# # measure = "difference"
) # This one is right. Two legends blocks instead of 1. Strange col_var artifact: "tea_where:1" and "tea_where".
tab_reg(tea, outcome = "tea_where", family = "binomial", trials = length(tea_where_vars), 
        predictors = c("sex", "SPC", "Sport"), empirical = TRUE, effect = "marginal", measure = "ratio"
) # This one is right. Strange col_var artifact: "tea_where:1" and "tea_where".


# summed-score defects pass 2
tab_reg(tea, outcome = "tea_where", family = "binomial", trials = length(tea_where_vars), 
        predictors = c("sex", "SPC", "Sport"), empirical = TRUE
) # ok. Both Obs and Model "base" are the mean summed-score.

## effect = "coefficient"
tab_reg(tea, outcome = "tea_where", family = "binomial", trials = length(tea_where_vars), 
        predictors = c("sex", "SPC", "Sport"), empirical = TRUE, measure = "difference"
) # Obs_ observed "base" is not the mean sum, but the mean proportion ; Model_ "base" is the sum, but formatted as a proportion not as a mean.
## effect = "marginal" (same defects with effect="at_reference")
tab_reg(tea, outcome = "tea_where", family = "binomial", trials = length(tea_where_vars), 
        predictors = c("sex", "SPC", "Sport"), empirical = TRUE, effect = "marginal"# # measure = "difference"
) # Obs_ and Model_ both show the mean proportion, not the mean summed score is formatted right, but the number is wrong (it’s the mean proportion not the mean sum)
tab_reg(tea, outcome = "tea_where", family = "binomial", trials = length(tea_where_vars), 
        predictors = c("sex", "SPC", "Sport"), empirical = TRUE, effect = "marginal", measure = "ratio"
) # Obs is ok. the Model_mRR summed score is formatted right, but the number is wrong (it’s the mean proportion not the mean sum).



## effect = "at_reference"
tab_reg(tea, outcome = "tea_where", family = "binomial", trials = length(tea_where_vars), 
        predictors = c("sex", "SPC", "Sport"), empirical = TRUE, effect = "at_reference"# # measure = "difference"
) # Two legends blocks instead of 1. Strange col_var artifact: "tea_where:1" and "tea_where".
tab_reg(tea, outcome = "tea_where", family = "binomial", trials = length(tea_where_vars), 
        predictors = c("sex", "SPC", "Sport"), empirical = TRUE, effect = "at_reference", measure = "ratio"
) # Strange col_var artifact: "tea_where:1" and "tea_where".

# multinomial: empirical="column"
## effect = "coefficient"
tab_reg(gss_simple, outcome = "party3", predictors = c("race", "rincome", "relig", "age"),
        family = "multinomial", empirical = "column"
) # Ok. 
## effect = "marginal"
tab_reg(gss_simple, outcome = "party3", predictors = c("race", "rincome", "relig", "age"),
        family = "multinomial", empirical = "column", effect = "marginal"
) # Ok.
tab_reg(gss_simple, outcome = "party3", predictors = c("race", "rincome", "relig", "age"),
        family = "multinomial", empirical = "column", effect = "marginal", measure = "ratio"
) # Names ok. But marginal risk ratio prints like an OR, I don’t know if it’s a math or a display problem.

## effect = "at_reference"
tab_reg(gss_simple, outcome = "party3", predictors = c("race", "rincome", "relig", "age"),
        family = "multinomial", empirical = "column", effect = "at_reference" #, measure = "odds_ratio"
)
tab_reg(gss_simple, outcome = "party3", predictors = c("race", "rincome", "relig", "age"),
        family = "multinomial", empirical = "column", effect = "at_reference", measure = "difference"
) # Ok. 
tab_reg(gss_simple, outcome = "party3", predictors = c("race", "rincome", "relig", "age"),
        family = "multinomial", empirical = "column", effect = "at_reference", measure = "ratio"
) # risk ratio at ref prints like an OR, I don’t know if it’s a math or a display problem.

# ordinal: empirical="column"
## effect = "coefficient"
tab_reg(gss_simple, outcome = "rincome", predictors = c("race", "marital", "relig", "age"),
        family = "ordinal", empirical = "column"
) # ok
## effect = "marginal"
tab_reg(gss_simple, outcome = "rincome", predictors = c("race", "marital", "relig", "age"),
        family = "ordinal", empirical = "column", effect = "marginal" #, measure = "difference"
) # ok.
tab_reg(gss_simple, outcome = "rincome", predictors = c("race", "marital", "relig", "age"),
        family = "ordinal", empirical = "column", effect = "marginal", measure = "ratio"
) # Here the observed percentage is missing from the "Obs_*" columns display. 
## effect = "at_reference"
tab_reg(gss_simple, outcome = "rincome", predictors = c("race", "marital", "relig", "age"),
        family = "ordinal", empirical = "column", effect = "at_reference" #, measure = "difference"
) # Here the observed percentage is missing from the "Obs_*" columns display. 
tab_reg(gss_simple, outcome = "rincome", predictors = c("race", "marital", "relig", "age"),
        family = "ordinal", empirical = "column", effect = "at_reference", measure = "ratio"
) # Here the observed percentage is missing from the "Obs_*" columns display. 
# Problem found in passing: the $ in "Lt$100000" like names destroy html formatting in footer (such ones should be escaped ?)

# multinomial: empirical="cell" are all ok
## effect = "coefficient"
tab_reg(gss_simple, outcome = "party3", predictors = c("race", "rincome", "relig", "age"),
        family = "multinomial", empirical = TRUE
)
## effect = "marginal"
tab_reg(gss_simple, outcome = "party3", predictors = c("race", "rincome", "relig", "age"),
        family = "multinomial", empirical = TRUE, effect = "marginal"
)
tab_reg(gss_simple, outcome = "party3", predictors = c("race", "rincome", "relig", "age"),
        family = "multinomial", empirical = TRUE, effect = "marginal", measure = "ratio"
)

## effect = "at_reference"
tab_reg(gss_simple, outcome = "party3", predictors = c("race", "rincome", "relig", "age"),
        family = "multinomial", empirical = TRUE, effect = "at_reference"
)
tab_reg(gss_simple, outcome = "party3", predictors = c("race", "rincome", "relig", "age"),
        family = "multinomial", empirical = TRUE, effect = "at_reference", measure = "ratio"
)

# ordinal: empirical="cell"
## effect = "coefficient"
tab_reg(gss_simple, outcome = "rincome", predictors = c("race", "marital", "relig", "age"),
        family = "ordinal", empirical = TRUE
) # ok
## effect = "marginal"
tab_reg(gss_simple, outcome = "rincome", predictors = c("race", "marital", "relig", "age"),
        family = "ordinal", empirical = TRUE, effect = "marginal" #, measure = "difference"
) # ok.
tab_reg(gss_simple, outcome = "rincome", predictors = c("race", "marital", "relig", "age"),
        family = "ordinal", empirical = TRUE, effect = "marginal", measure = "ratio"
)
## effect = "at_reference"
tab_reg(gss_simple, outcome = "rincome", predictors = c("race", "marital", "relig", "age"),
        family = "ordinal", empirical = TRUE, effect = "at_reference" #, measure = "difference"
) 
tab_reg(gss_simple, outcome = "rincome", predictors = c("race", "marital", "relig", "age"),
        family = "ordinal", empirical = TRUE, effect = "at_reference", measure = "ratio"
) 








#### review the adjustment
# binomial
## effect = "coefficient"
tab_reg(gss_simple, outcome = "married", predictors = c("race", "rincome", "relig", "age"),
        family = "binomial", color = "adjustment" #, measure = "odds_ratio"
)
tab_reg(gss_simple, outcome = "married", predictors = c("race", "rincome", "relig", "age"),
        family = "binomial", color = "adjustment", measure = "log"
)
tab_reg(gss_simple, outcome = "married", predictors = c("race", "rincome", "relig", "age"),
        family = "binomial", color = "adjustment", measure = "ratio"
)
tab_reg(gss_simple, outcome = "married", predictors = c("race", "rincome", "relig", "age"),
        family = "binomial", color = "adjustment", measure = "difference"
) 

## effect = "marginal"
tab_reg(gss_simple, outcome = "married", predictors = c("race", "rincome", "relig", "age"),
        family = "binomial", color = "adjustment", effect = "marginal" #, measure = "difference"
)
tab_reg(gss_simple, outcome = "married", predictors = c("race", "rincome", "relig", "age"),
        family = "binomial", color = "adjustment", effect = "marginal", measure = "ratio"
)

# ## effect = "at_reference"
# tab_reg(gss_simple, outcome = "married", predictors = c("race", "rincome", "relig", "age"),
#         family = "binomial", color = "adjustment", effect = "at_reference" #, measure = "difference"
# )


# gaussian
## effect = "coefficient"
tab_reg(gss_simple, outcome = "age", predictors = c("race", "rincome", "relig", "tvhours"),
        family = "gaussian", color = "adjustment"
)
tab_reg(gss_simple, outcome = "age", predictors = c("race", "rincome", "relig", "tvhours"),
        family = "gaussian", color = "adjustment", measure = "ratio"
)

## effect = "marginal"
tab_reg(gss_simple, outcome = "age", predictors = c("race", "rincome", "relig", "tvhours"),
        family = "gaussian", color = "adjustment", effect = "marginal"
)
tab_reg(gss_simple, outcome = "age", predictors = c("race", "rincome", "relig", "tvhours"),
        family = "gaussian", color = "adjustment", effect = "marginal", measure = "ratio"
)


# poisson
## effect = "coefficient"
tab_reg(gss_simple, outcome = "tvhours", predictors = c("race", "rincome", "relig", "age"),
        family = "poisson", color = "adjustment"
)
tab_reg(gss_simple, outcome = "tvhours", predictors = c("race", "rincome", "relig", "age"),
        family = "poisson", color = "adjustment", measure = "log"
)

## effect = "marginal"
# tab_reg(gss_simple, outcome = "tvhours", predictors = c("race", "rincome", "relig", "age"),
#         family = "poisson", color = "adjustment", effect = "marginal"# , measure = "difference"
# )
tab_reg(gss_simple, outcome = "tvhours", predictors = c("race", "rincome", "relig", "age"),
        family = "poisson", color = "adjustment", effect = "marginal", measure = "ratio"
)


# summed-score binomial
## effect = "coefficient"
tab_reg(tea, outcome = "tea_where", family = "binomial", trials = length(tea_where_vars), 
        predictors = c("sex", "SPC", "Sport"),  color = "adjustment"
)
tab_reg(tea, outcome = "tea_where", family = "binomial", trials = length(tea_where_vars), 
        predictors = c("sex", "SPC", "Sport"), color = "adjustment", measure = "ratio"
)
tab_reg(tea, outcome = "tea_where", family = "binomial", trials = length(tea_where_vars), 
        predictors = c("sex", "SPC", "Sport"), color = "adjustment", measure = "difference"
)

## effect = "marginal"
tab_reg(tea, outcome = "tea_where", family = "binomial", trials = length(tea_where_vars), 
        predictors = c("sex", "SPC", "Sport"), color = "adjustment", effect = "marginal"# # measure = "difference"
)
tab_reg(tea, outcome = "tea_where", family = "binomial", trials = length(tea_where_vars), 
        predictors = c("sex", "SPC", "Sport"), color = "adjustment", effect = "marginal", measure = "ratio"
)


# multinomial: empirical="cell"
## effect = "coefficient"
tab_reg(gss_simple, outcome = "party3", predictors = c("race", "rincome", "relig", "age"),
        family = "multinomial", color = "adjustment"
)
## effect = "marginal"
tab_reg(gss_simple, outcome = "party3", predictors = c("race", "rincome", "relig", "age"),
        family = "multinomial", color = "adjustment", effect = "marginal"
)
tab_reg(gss_simple, outcome = "party3", predictors = c("race", "rincome", "relig", "age"),
        family = "multinomial", color = "adjustment", effect = "marginal", measure = "ratio"
)


# ordinal: empirical="cell"
## effect = "coefficient"
tab_reg(gss_simple, outcome = "rincome", predictors = c("race", "marital", "relig", "age"),
        family = "ordinal", color = "adjustment"
)
## effect = "marginal"
tab_reg(gss_simple, outcome = "rincome", predictors = c("race", "marital", "relig", "age"),
        family = "ordinal", color = "adjustment", effect = "marginal" #, measure = "difference"
)
tab_reg(gss_simple, outcome = "rincome", predictors = c("race", "marital", "relig", "age"),
        family = "ordinal", color = "adjustment", effect = "marginal", measure = "ratio"
)









#### the different families × effects × measure: custom displays

# binomial
## effect = "coefficient"
model <- tab_reg(gss_simple, outcome = "married", predictors = c("race", "rincome", "relig", "age"),
        family = "binomial", empirical = TRUE #, measure = "odds_ratio"
) 
model |> set_display("est_ci") # Nice. But the 1/x.xx display inside the ci bracket is breaking alignement/padding. And I would prefer the "est" only to be colored and to carry the stars (primary token, the ci token carry it’s own bracket even if not seen in the display string, how to fix this reliably in a user-friendly way (for all composed CIs display ?) ?)
model |> set_display("est_base")
model |> set_display("base_est")
model |> set_display("base") # I verified model |> set_display("pct") make the numeric predictor coeff dissapear (this is why we implemented "base" at first place).
model |> set_display("{est} ({diff})")  # OR with AME displayed in parenthesis for interpretation. The Obs_OR column shows empty "()": for user-friendliness, rule should be "trim empty parenthesis, pad anyway for if some are non-empty, and if all are empty the padding will be zero". Also, would is be cheap or costly to precalculate risk ratio and risk difference for the crude column in any case where it’s meaningful (like we do for model column), please study then AskUserQuestion me ?
model |> set_display("{est} ({ratio})") # all () are empty, even in the Model column: I thought we decided to precalculate the marginal risk ratios in any case to be able to do exactly this kind of display, please check (and check for effect in performance).
model |> mutate(Obs_OR   = set_display(Obs_OR  , "({base}) {est}"), 
                Model_OR = set_display(Model_OR, "{est} ({diff})") ) # this one is good, create a "base_est_mdiff" display preset
model |> mutate(Obs_OR   = set_display(Obs_OR  , "({base}) {est}"), 
                Model_OR = set_display(Model_OR, "{est} ({ratio})") ) # this one would be good when ratio is filled, create a "base_est_mratio" display preset
model |> set_display("{est} ({coeff})") # would there be a way to print the log(OR) row coef alongside the estimate OR ("est_coeff" preset), and a reliable way to store it in any case (where ? `mean` ? `ctr` ? with a coeff alias only working for regressions ?), and display it in the tooltips with the right text (not the base text of the field since there is no coeff field ?)

#### useless combination when marginal is on the same measure than the coefficient ?
# The two following ones are, logically, the same, because more generally **it’s useless to 
#  calculate a marginal effect on the same measure that the coefficient already use** (here, "ratio"), 
#  right ? And the legend and column names are lying, because it’s not a marginal IRR ? 
#  If so, we should forbid this useless combination, and tell it to the user. And change combinations tables and documentation (in vignettes, etc.)
tab_reg(gss_simple, outcome = "tvhours", predictors = c("race", "rincome", "relig", "age"),
        family = "poisson", empirical = TRUE
) 
tab_reg(gss_simple, outcome = "tvhours", predictors = c("race", "rincome", "relig", "age"),
        family = "poisson", empirical = TRUE, effect = "marginal", measure = "ratio"
) 






#### miscellaneous
tab_reg(gss_simple, outcome = "married", predictors = c("race", "rincome", "relig", "age"),
        family = "binomial", empirical=TRUE
) # with "est_base" in the Model column, in numeric predictors cells where "base" is NA, alignment/padding is broken (not align with the other OR), and it’s the case for all measures and effects. Fix the padding when there are NAs.



# "difference" `measure` display consistency, for both mean and pct
tab_reg(gss_simple, outcome = "age", predictors = c("race", "rincome", "relig", "tvhours"),
        family = "gaussian", empirical=TRUE
) # this one is nearly ok, ref is "0", but the positive ones should have a "+" sign to signify it’s a "difference" `measure`
tab_reg(gss_simple, outcome = "married", predictors = c("race", "rincome", "relig", "age"),
        family = "binomial", empirical=TRUE, measure = "difference"
) # here positive have a "+" sign, but the ref/null is printed "+0%", I would prefer "0%".
tab_reg(gss_simple, outcome = "married", predictors = c("race", "rincome", "relig", "age"),
        family = "binomial", empirical=TRUE, effect = "marginal" #, measure = "difference"
) # same here for Obs ; and worse, Model column lose it’s reference cell content altogether (void)


# some models prints numeric predictors like age with 1 unit increment, instead of the default 1 SD.
tab_reg(gss_simple, outcome = "party3", predictors = c("race", "rincome", "relig", "age"),
        family = "multinomial"
)
tab_reg(gss_simple, outcome = "rincome", predictors = c("race", "marital", "relig", "age"),
        family = "ordinal"
)




### tab() manual review ----

# `ordered` class for rincome still causes problems in tab,
#   but only when I added a numeric col_vars `tvhours` and `na = "drop_all"` !: please fix everywhere and add a test.
tab(gss_simple, c(race, rincome, relig), c(party3, marital, tvhours), pct = "row", na = "drop_all", 
   color = TRUE, color_signif = "grey_non_signif", ref = 1
) 
# Error:
# ! Build failed on "rincome".
# Caused by error in `dplyr::full_join()`:
# ! Can't join `x$rincome` with `y$rincome` due to incompatible types.
# ℹ `x$rincome` is a <ordered<f0104>>.
# ℹ `y$rincome` is a <ordered<63988>>.

# `display` presets
# The following `display` are good, please create display presets for then, and document.
tab(gss_simple, c(race, rincome, relig), c(party3, marital), pct = "row", na = "drop_all", 
   color = TRUE, color_signif = "grey_non_signif", ref = 1, display = "{base} ({ratio})"
) # preset "base_ratio" (working for both pct and mean)

tab(gss_simple, c(race, rincome, relig), c(party3, marital), pct = "row", na = "drop_all", 
   color = TRUE, color_signif = "grey_non_signif", ref = 1, display = "{pct} ({or})"
) # preset "pct_OR" (if something like that does not already exist)
tab(gss_simple, c(race, rincome, relig), c(party3, marital), pct = "row", na = "drop_all", 
   color = "OR", color_signif = "grey_non_signif", ref = 1, display = "{or} ({pct})"
) # preset "OR_pct" (if something like that does not already exist)
# - Also, accept "{OR}" as an alias for "{or}", and "OR" as an alias for "or" (otherwise it will confuse the user).
# - Defect found : here, the 100% Total column is colored ! Two problems, OR should not be calculated if 
#   it have no meaning (or do it have a meaning ?), and even if calculated it should print with no column 
#   like a 100% total column (it may be a ref problem ?) ?
# - Defect found : with OR display as the primary display token, like it’s already done with `display = "or"`,
#   there should be no 100% column (but only the n inside the Total column. How to do this reliably. 
#   Same in other cases, like "{ratio} ({pct})" ; and here, `display = "ratio"` don’t do it yet (it keeps the 100%)

# numeric col_vars default display
tab(gss_simple, c(race, rincome, relig), c(age, tvhours), color = TRUE) 
# - I’m tired of seeing the numeric col_vars sigma sd, as pure uninterpretable noise in every "mean" column :
# I want to keep it as a display option "mean_sd" or a display token "{sd}" (computing it from `var` field at render), 
# and change the default for numeric col_vars to a coefficient of variation sd/mean
# (computed from `var` and `mean` at render), with display preset "mean_cv" and display token "{cv}", formatted as a % with no decimals
# Something like: "49 (cv 35%)". The "mean" or "{mean}" `display` should both print the bare mean (without sd or cv).
# According to literature (make web searches) : would it be more useful as a default display, is it robust, is it readable ? ;
#  Would there be another, more useful and modern default display for numeric col_vars ? Is there a symbol usable instead of "cv"?
# - In this case the "mean (sd)" column headers on exporters should be changed to "mean", since "mean (cv)" would be useless because it would repeat the acronym already on each cell.





### exports and display tests  ---- 

# "md"
tab_reg(gss_simple, outcome = c("married", "tvhours"), predictors = c("race", "rincome", "relig", "age"),
        family = c("binomial", "poisson"), empirical = TRUE, 
) |> 
  tab_export("md")
# | *relig*     |**Protestant**                       | |   **6 269**|  **3 230**| |  (52%)      **1**        |   **1    (53%)**       | |  (2.72)     **1**        |  **1    (2.61)**       |
# |             | Catholic                            | |     3 121  |    1 617  | |    (52%) 1/1.03          |  1/1.17*** (49%)       | |    (2.50) ÷1.09***       |  ×1.02    (2.65)       |
# Reference rows have a small misalignment/mispadding for the "base" field in parenthesis, compared to the other rows. @dev/review_manual/tab_md_test_5.md
# Once renderer in html @dev/review_manual/tab_md_test_5.htm : 
# - the long default cell content like "(31%) 1/2.43***" is wrapped on two lines, which makes it utterly unreadable for humans.
# - in the footer, empty cells seem to add a vertical border where there are none, and all footer internal vertical borders seems to have more linewidth than the predictors colums (constant reference population does the same) ; because because the borders are duplicated ?


# "html"
tab(gss_simple, c(race, rincome, relig), c(party3, marital), pct = "row", na = "drop_all", 
   color = TRUE, color_signif = "grey_non_signif", ref = 1
) |> tab_export()
# - We lose alignment a bit here, didn’t we resolve this for html before ? 
#   Strange thing is that it seems to be caused not by bold anymore, but by the colored background 
#   (do it adds a margin ? Would it be more reliable and more visually good if we remove the margin, 
#   or add it in cells with no specific color background ?)





# black and white publication ready tables ----
load_all()
options(tabxplor.theme = "print", tabxplor.print = "html", tabxplor.parallel = 8, tabxplor.cleannames = TRUE)

tab_reg(gss_simple, outcome = "age", predictors = c("race", "rincome", "relig", "tvhours"),
        family = "gaussian", empirical=TRUE
)
tab(gss_simple, c(race, relig), c(party3, tvhours), pct = "row", na = "drop_all", 
   color = TRUE, color_signif = "grey_non_signif",
)
# - bold only applies to the primary display token (right behaviour), but underline and italics also apply to the
#   secondary display tokens: they too, like colors, should only apply to the primary display token by default (global option). Otherwise it’s noise.
# - bold and underlines are visually very striking, but italics is subtle and not striking, specially with the current html monospace font.
#   We’ll try something else:
#   1. `tx_chrome_hex("print")$grey` identifies the greyed-out cells (the grey is much lighter than with colors)
#   2. the direction information is carried by two things: 
#     a. the over|under symbols +|- ×|÷ x|1/x  (they also carry the `measure`) ; 
#     b. the italics for the "below null"/"under" branch, subtle but which only supports the over|under symbols
#   3. the size of effect is carried by a 3 rungs ladder: `tx_chrome_hex("print")$text` pure "black" ; "black" + bold ; "black" + bold + underline
# - some tests are now failing because I tweaked the `tx_chrome_hex("print") palette. A WCAG assertion is failing for 



#   A possibility that would work for regression (were direction is clear), but not for percentages and means :
#   1. `tx_chrome_hex("print")$grey` identifies the greyed-out cells (the grey is much lighter than with colors)
#   2. the direction information is carried by two things: 
#     a. the over|under symbols +|- ×|÷ x|1/x  (they also carry the `measure`) ; 
#     b. the italics for the "below null"/"under" branch, subtle but which only supports the over|under symbols
#   3. the size of effect is carried by a 4 rungs ladder: `tx_chrome_hex("print")$grey2` ; `tx_chrome_hex("print")$text` pure "black"  ; "black" + bold ; "black" + bold + underline

#   A possibility, for tab() pct and means, that is a bit overloaded would works (ratio display carry the direction) :
#   1. `tx_chrome_hex("print")$grey` identifies the greyed-out cells (the grey is much lighter than with colors)
#   2. the direction information is carried by two things: 
#     a. a display = "{base} {ratio}" (with 1 digits ratio ×1.x or ÷1.x) ; 
#     b. the italics for the "below null"/"under" branch, subtle but which only supports the over|under symbols
#   3. the size of effect is carried by a 4 rungs ladder: `tx_chrome_hex("print")$grey2` ; `tx_chrome_hex("print")$text` pure "black"  ; "black" + bold ; "black" + bold + underline


# - In the right parameters table, change the minimum digits for the observed mean and the adjusted mean to 1 (there’s too decimals here, I want to drop one) 

# Just + ++ +++ - -- --- × ×× ××× ÷ ÷÷ ÷÷÷ for the effect size ?

# Explain empirical counterpart too in the "Model:" first tab_reg() specific legend block ?












### weights and survey-design pvalues display improvement ----
pc18 <- readRDS("~/Data/Pratiques culturelles/Pratiques culturelles 2018/pc18.rds")
musique_vars <- c("ROCK", "JAZZ", "CLASSIQUE", "VARIETE", "ELECTRO", "METAL", "CHANSON", "WORLD", "RAP", "TRADI")
pc18 <- pc18 |>
  select(-any_of(c("CHANSON", "WORLD", "TRADI", "VARIETE", "RNB", "ELECTRO", "RAP", "METAL", "ROCK", "JAZZ", "OPERA", "CLASSIQUE"))) |>
  rename(any_of(c(CHANSON   = "E1001", WORLD = "E1002", TRADI = "E1003", VARIETE   = "E1004", RNB= "E1005", ELECTRO = "E1006", 
RAP= "E1007", METAL= "E1008", ROCK= "E1009", JAZZ= "E1010", OPERA= "E1011", CLASSIQUE = "E1012"
  )))
pc18$CHANSON   <- forcats::fct_recode(pc18$CHANSON,  "1-Chanson ou variété française" = "1-Chansons ou variétés françaises", "2-Non" = "2-Non")
pc18$WORLD     <- forcats::fct_recode(pc18$WORLD,  "1-World" = "1-Musiques du monde", "2-Non" = "2-Non")
pc18$TRADI     <- forcats::fct_recode(pc18$TRADI,  "1-Tradi" = "1-Musiques traditionnelles",  "2-Non" = "2-Non")
pc18$VARIETE   <- forcats::fct_recode(pc18$VARIETE, "1-Variété inter- nationale" = "1-Variétés internationales", "2-Non" = "2-Non")
pc18$ELECTRO   <- forcats::fct_recode(pc18$ELECTRO, "1-Électro, techno" = "1-Musiques électroniques, techno", "2-Non"   = "2-Non")
pc18$RAP       <- forcats::fct_recode(pc18$RAP, "1-Rap" = "1-Hip hop, rap", "2-Non"          = "2-Non")
pc18$METAL     <- forcats::fct_recode(pc18$METAL,"1-Metal, hard rock" = "1-Metal, hard rock","2-Non"= "2-Non")
pc18$ROCK      <- forcats::fct_recode(pc18$ROCK, "1-Pop, rock" = "1-Pop, rock", "2-Non"       = "2-Non")
pc18$JAZZ      <- forcats::fct_recode(pc18$JAZZ, "1-Jazz" = "1-Jazz", "2-Non"  = "2-Non")
pc18$CLASSIQUE <- forcats::fct_recode(pc18$CLASSIQUE, "1-Classique" = "1-Musique classique", "2-Non" = "2-Non")
pc18 <- pc18 |> dplyr::select(-any_of(c("NB_MUSIQUE"))) |> score_from_lv1(name = "NB_MUSIQUES", vars_list = musique_vars)

pc18_young <- pc18 |> dplyr::filter(AGE >= 18 & AGE < 25) #  593 individuals

#   exploratory tables with several row_vars and several col_vars (nb_cine, NB_CONCERTS = numeric variable)
rows1 <- c("DIPLOM", "CRITREVENU", "CRITAGE", "CSTOTR")
cols1 <- c("CONCERTS", "JV", "NB_CONCERTS")

#   `levels = "first"`
rows2 <- c("SEXE", "DIPLOM", "CRITREVENU", "CRITAGE")
cols2 <- c(musique_vars, "NB_MUSIQUES") # plus the related score as numeric variable

# Base unweighted pvalues
options(tabxplor.kish_neff = FALSE)
tab(pc18, c(SEXE, CRITREVENU), c(CLASSIQUE, METAL, WORLD, RAP, TRADI, NB_MUSIQUES), 
wt = POND, pct = "row", levels = "first",
    na = "drop_all", color = TRUE, color_signif = "grey_non_signif", test = TRUE,
) 
# Only kish_neff
options(tabxplor.kish_neff = TRUE)
tab(pc18, c(SEXE, CRITREVENU), c(CLASSIQUE, METAL, WORLD, RAP, TRADI, NB_MUSIQUES), 
wt = POND, pct = "row", levels = "first",
    na = "drop_all", color = TRUE, color_signif = "grey_non_signif", test = TRUE,
) 
# Minimal survey design (ZAE clusters, chosen randowly)
pc18$ZAE <- substr(as.character(pc18$IDENT18), 1, 6)   # the 340 ZAE = the PSU
tab(pc18, c(SEXE, CRITREVENU), c(CLASSIQUE, METAL, WORLD, RAP, TRADI, NB_MUSIQUES),
    pct = "row", levels = "first",
    na = "drop_all", color = TRUE, color_signif = "grey_non_signif",
    wt = POND, test = "survey", ids  = ~ZAE
)









# tab(gss_simple, c(race, rincome, relig), c(party3, marital), pct = "row",  na = "drop", 
#    color = TRUE, color_signif = "grey_non_signif"
# ) |> 
#   tab_xl(open = FALSE, path = "~/github/tabxplor/dev/review_manual/Excel_test", replace = TRUE)

# tab(gss_simple, c(race, rincome, relig), c(party3, marital), pct = "row",  na = "drop", 
#    color = TRUE, color_signif = "grey_non_signif", ref = 1, stars = TRUE
# ) |> 
#   tab_xl(open = FALSE, path = "~/github/tabxplor/dev/review_manual/Excel_stars", replace = TRUE)

# tab_reg(gss_simple, "married", c("race",  "rincome"), family = "binomial") |> 
#   tab_xl(open = FALSE, path = "~/github/tabxplor/dev/review_manual/Excel_stars_OR", replace = TRUE)



# Not working
# options(tabxplor.totcol_range = "off")
# tab(gss_simple, race, c(rincome, party3, relig), pct = "row",
#    color = TRUE, color_signif = "grey_non_signif", ref = 1
# )

# options(tabxplor.totcol_range = "range")
# tab(gss_simple, race, c(rincome, party3, relig), pct = "row",
#    color = TRUE, color_signif = "grey_non_signif", ref = 1
# )

# options(tabxplor.totcol_range = "min")
# tab(gss_simple, race, c(rincome, party3, relig), pct = "row",
#    color = TRUE, color_signif = "grey_non_signif", ref = 1
# )

