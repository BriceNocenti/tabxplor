
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
## effect = "conditional"
tab_reg(gss_simple, outcome = "married", predictors = c("race", "rincome", "relig", "age"), 
        empirical = TRUE, family = "binomial" #, measure = "odds_ratio", 
)
tab_reg(gss_simple, outcome = "married", predictors = c("race", "rincome", "relig", "age"),
        family = "binomial", empirical = TRUE, measure = "log"
)
tab_reg(gss_simple, outcome = "married", predictors = c("race", "rincome", "relig", "age"),
        family = "binomial", empirical = TRUE, measure = "ratio"
)
tab_reg(gss_simple, outcome = "married", predictors = c("race", "rincome", "relig", "age"),
        family = "binomial", empirical = TRUE, measure = "difference"
)

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
## effect = "conditional"
tab_reg(gss_simple, outcome = "age", predictors = c("race", "rincome", "relig", "tvhours"),
        family = "gaussian", empirical = TRUE
)
tab_reg(gss_simple, outcome = "age", predictors = c("race", "rincome", "relig", "tvhours"),
        family = "gaussian", empirical = TRUE, measure = "ratio"
)

## effect = "marginal"
tab_reg(gss_simple, outcome = "age", predictors = c("race", "rincome", "relig", "tvhours"),
        family = "gaussian", empirical = TRUE, effect = "marginal"
)
tab_reg(gss_simple, outcome = "age", predictors = c("race", "rincome", "relig", "tvhours"),
        family = "gaussian", empirical = TRUE, effect = "marginal", measure = "ratio"
)

## effect = "at_reference"
tab_reg(gss_simple, outcome = "age", predictors = c("race", "rincome", "relig", "tvhours"),
        family = "gaussian", empirical = TRUE, effect = "at_reference"
) 
tab_reg(gss_simple, outcome = "age", predictors = c("race", "rincome", "relig", "tvhours"),
        family = "gaussian", empirical = TRUE, effect = "at_reference", measure = "ratio"
)


# poisson
## effect = "conditional"
tab_reg(gss_simple, outcome = "tvhours", predictors = c("race", "rincome", "relig", "age"),
        family = "poisson", empirical = TRUE
)
tab_reg(gss_simple, outcome = "tvhours", predictors = c("race", "rincome", "relig", "age"),
        family = "poisson", empirical = TRUE, measure = "log"
)

## effect = "marginal"
tab_reg(gss_simple, outcome = "tvhours", predictors = c("race", "rincome", "relig", "age"),
        family = "poisson", empirical = TRUE, effect = "marginal"# , measure = "difference"
)
tab_reg(gss_simple, outcome = "tvhours", predictors = c("race", "rincome", "relig", "age"),
        family = "poisson", empirical = TRUE, effect = "marginal", measure = "ratio"
)

## effect = "at_reference"
tab_reg(gss_simple, outcome = "tvhours", predictors = c("race", "rincome", "relig", "age"),
        family = "poisson", empirical = TRUE, effect = "at_reference"# , measure = "difference"
)
tab_reg(gss_simple, outcome = "tvhours", predictors = c("race", "rincome", "relig", "age"),
        family = "poisson", empirical = TRUE, effect = "at_reference", measure = "ratio"
)

# summed-score binomial
## effect = "conditional"
tab_reg(tea, outcome = "tea_where", family = "binomial", trials = length(tea_where_vars), 
        predictors = c("sex", "SPC", "Sport"),  empirical = TRUE
)
tab_reg(tea, outcome = "tea_where", family = "binomial", trials = length(tea_where_vars), 
        predictors = c("sex", "SPC", "Sport"), empirical = TRUE, measure = "ratio"
)
tab_reg(tea, outcome = "tea_where", family = "binomial", trials = length(tea_where_vars), 
        predictors = c("sex", "SPC", "Sport"), empirical = TRUE, measure = "difference"
)

## effect = "marginal"
tab_reg(tea, outcome = "tea_where", family = "binomial", trials = length(tea_where_vars), 
        predictors = c("sex", "SPC", "Sport"), empirical = TRUE, effect = "marginal"# # measure = "difference"
)
tab_reg(tea, outcome = "tea_where", family = "binomial", trials = length(tea_where_vars), 
        predictors = c("sex", "SPC", "Sport"), empirical = TRUE, effect = "marginal", measure = "ratio"
)


# summed-score defects pass 2
tab_reg(tea, outcome = "tea_where", family = "binomial", trials = length(tea_where_vars), 
        predictors = c("sex", "SPC", "Sport"), empirical = TRUE
)

## effect = "conditional"
tab_reg(tea, outcome = "tea_where", family = "binomial", trials = length(tea_where_vars), 
        predictors = c("sex", "SPC", "Sport"), empirical = TRUE, measure = "difference"
)
## effect = "marginal" (same defects with effect="at_reference")
tab_reg(tea, outcome = "tea_where", family = "binomial", trials = length(tea_where_vars), 
        predictors = c("sex", "SPC", "Sport"), empirical = TRUE, effect = "marginal"# # measure = "difference"
)
tab_reg(tea, outcome = "tea_where", family = "binomial", trials = length(tea_where_vars), 
        predictors = c("sex", "SPC", "Sport"), empirical = TRUE, effect = "marginal", measure = "ratio"
)



## effect = "at_reference"
tab_reg(tea, outcome = "tea_where", family = "binomial", trials = length(tea_where_vars), 
        predictors = c("sex", "SPC", "Sport"), empirical = TRUE, effect = "at_reference"# # measure = "difference"
)
tab_reg(tea, outcome = "tea_where", family = "binomial", trials = length(tea_where_vars), 
        predictors = c("sex", "SPC", "Sport"), empirical = TRUE, effect = "at_reference", measure = "ratio"
)

# multinomial: empirical="column"
## effect = "conditional"
tab_reg(gss_simple, outcome = "party3", predictors = c("race", "rincome", "relig", "age"),
        family = "multinomial", empirical = "column"
)
## effect = "marginal"
tab_reg(gss_simple, outcome = "party3", predictors = c("race", "rincome", "relig", "age"),
        family = "multinomial", empirical = "column", effect = "marginal"
)
tab_reg(gss_simple, outcome = "party3", predictors = c("race", "rincome", "relig", "age"),
        family = "multinomial", empirical = "column", effect = "marginal", measure = "ratio"
)

## effect = "at_reference"
tab_reg(gss_simple, outcome = "party3", predictors = c("race", "rincome", "relig", "age"),
        family = "multinomial", empirical = "column", effect = "at_reference" #, measure = "odds_ratio"
)
tab_reg(gss_simple, outcome = "party3", predictors = c("race", "rincome", "relig", "age"),
        family = "multinomial", empirical = "column", effect = "at_reference", measure = "difference"
)
tab_reg(gss_simple, outcome = "party3", predictors = c("race", "rincome", "relig", "age"),
        family = "multinomial", empirical = "column", effect = "at_reference", measure = "ratio"
)

# ordinal: empirical="column"
## effect = "conditional"
tab_reg(gss_simple, outcome = "rincome", predictors = c("race", "marital", "relig", "age"),
        family = "ordinal", empirical = "column"
)
## effect = "marginal"
tab_reg(gss_simple, outcome = "rincome", predictors = c("race", "marital", "relig", "age"),
        family = "ordinal", empirical = "column", effect = "marginal" #, measure = "difference"
)
tab_reg(gss_simple, outcome = "rincome", predictors = c("race", "marital", "relig", "age"),
        family = "ordinal", empirical = "column", effect = "marginal", measure = "ratio"
)
## effect = "at_reference"
tab_reg(gss_simple, outcome = "rincome", predictors = c("race", "marital", "relig", "age"),
        family = "ordinal", empirical = "column", effect = "at_reference" #, measure = "difference"
)
tab_reg(gss_simple, outcome = "rincome", predictors = c("race", "marital", "relig", "age"),
        family = "ordinal", empirical = "column", effect = "at_reference", measure = "ratio"
)
# Problem found in passing: the $ in "Lt$100000" like names destroy html formatting in footer (such ones should be escaped ?)

# multinomial: empirical="cell" are all ok
## effect = "conditional"
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
## effect = "conditional"
tab_reg(gss_simple, outcome = "rincome", predictors = c("race", "marital", "relig", "age"),
        family = "ordinal", empirical = TRUE
)
## effect = "marginal"
tab_reg(gss_simple, outcome = "rincome", predictors = c("race", "marital", "relig", "age"),
        family = "ordinal", empirical = TRUE, effect = "marginal" #, measure = "difference"
)
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










#### the different families × effects × measure: custom displays

# binomial
## effect = "conditional"
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




#### tab_reg() constants formatting manual review
# - Reference profile for the gaussian shows "+40.76" : but it is the mean at the reference profile,
#   so there should be no "+"" (it’s not a diff but a baseline). I wonder what would be the best solution to fix that, 
#   a custom display for that cell, or a reliable rule ensuring that for reference rows (can you see some situations were it would be wrong ?).
#   Same problem with the poisson reference profile: "×2.79", but its in fact the mean at the reference profile. It goes for "poisson" × "marginal" × "ratio" too.
#   The baseline odds formatting is ok for binomials. More details in the comments below
# - In the right parameters table, change the minimum digits for the observed mean and the adjusted mean to 1 (there’s 2 decimals by default, I want 1) ;
#   change the minimum digits for mean differences, pct differences, mean ratio and pct ratio to 1 too.
# - Having the overall N in the row "Reference profile" is meaningless and confusing, since the constant does not give the population 
#    average proportion or mean or odds at all (unless effect="marginal"). 
#    I want to remove the overall N here, and re-add N as the first stats footer row.
# binomial
tab_reg(gss_simple, outcome = "married", predictors = c("race", "rincome", "relig", "age"),
        family = "binomial", empirical = TRUE #, measure = "odds_ratio"
) # Constant "1/1.19", baseline odds, formatting is ok.
tab_reg(gss_simple, outcome = "married", predictors = c("race", "rincome", "relig", "age"),
        family = "binomial", empirical = TRUE, measure = "log"
) # Constant "-0.17", log(odds) ok. 
tab_reg(gss_simple, outcome = "married", predictors = c("race", "rincome", "relig", "age"),
        family = "binomial", empirical = TRUE, measure = "ratio"
) # Constant "÷2.30", should be baseline reference profile proportion "43%" ?
tab_reg(gss_simple, outcome = "married", predictors = c("race", "rincome", "relig", "age"),
        family = "binomial", empirical = TRUE, measure = "difference"
) # Constant "+45.7%", should be "45.7%".
tab_reg(gss_simple, outcome = "married", predictors = c("race", "rincome", "relig", "age"),
        family = "binomial", empirical = TRUE, effect = "marginal" #, measure = "difference"
)
tab_reg(gss_simple, outcome = "married", predictors = c("race", "rincome", "relig", "age"),
        family = "binomial", empirical = TRUE, effect = "marginal", measure = "ratio"
) # Constant "×2.05", should be average proportion "49%" ?
# gaussian
tab_reg(gss_simple, outcome = "age", predictors = c("race", "rincome", "relig", "tvhours"),
        family = "gaussian", empirical = TRUE
) # Constant "+40.76", should be "40.76"
tab_reg(gss_simple, outcome = "age", predictors = c("race", "rincome", "relig", "tvhours"),
        family = "gaussian", empirical = TRUE, measure = "ratio"
) # Constant "×40.47", should be "40.47". 
tab_reg(gss_simple, outcome = "age", predictors = c("race", "rincome", "relig", "tvhours"),
        family = "gaussian", empirical = TRUE, effect = "marginal", measure = "ratio"
) # Constant "×42.36", should be "42.36". 
# poisson
tab_reg(gss_simple, outcome = "tvhours", predictors = c("race", "rincome", "relig", "age"),
        family = "poisson", empirical = TRUE
) # Constant "×2.95", should be "2.95"
tab_reg(gss_simple, outcome = "tvhours", predictors = c("race", "rincome", "relig", "age"),
        family = "poisson", empirical = TRUE, measure = "log"
) # Constant "+1.08", should be "1.08"
tab_reg(gss_simple, outcome = "tvhours", predictors = c("race", "rincome", "relig", "age"),
        family = "poisson", empirical = TRUE, effect = "marginal"# , measure = "difference"
) # Constant "+2.56", should be "2.56"
# summed-score binomial
tab_reg(tea, outcome = "tea_where", family = "binomial", trials = length(tea_where_vars), 
        predictors = c("sex", "SPC", "Sport"),  empirical = TRUE
) # Constant "1/1.20", baseline odds ok.
tab_reg(tea, outcome = "tea_where", family = "binomial", trials = length(tea_where_vars), 
        predictors = c("sex", "SPC", "Sport"), empirical = TRUE, measure = "ratio"
) # Constant "1/1.20". Here all RR in the table are displayed as OR, fix this (math problem or display problem ?) !
tab_reg(tea, outcome = "tea_where", family = "binomial", trials = length(tea_where_vars), 
        predictors = c("sex", "SPC", "Sport"), empirical = TRUE, measure = "difference"
) # Constant "+2.72", should be "2.72"
tab_reg(tea, outcome = "tea_where", family = "binomial", trials = length(tea_where_vars), 
        predictors = c("sex", "SPC", "Sport"), empirical = TRUE, effect = "marginal"# # measure = "difference"
) # Constant "+2.58", should be "2.58"
tab_reg(tea, outcome = "tea_where", family = "binomial", trials = length(tea_where_vars), 
        predictors = c("sex", "SPC", "Sport"), empirical = TRUE, effect = "marginal", measure = "ratio"
)
# multinomial
tab_reg(gss_simple, outcome = "party3", predictors = c("race", "rincome", "relig", "age"),
        family = "multinomial", empirical = TRUE
) # Constants are baseline odds, ok.
tab_reg(gss_simple, outcome = "party3", predictors = c("race", "rincome", "relig", "age"),
        family = "multinomial", empirical = TRUE, effect = "marginal"
) # Constants "+45.3%" etc., should be "45.3%" etc.
tab_reg(gss_simple, outcome = "party3", predictors = c("race", "rincome", "relig", "age"),
        family = "multinomial", empirical = TRUE, effect = "marginal", measure = "ratio"
) # Constants "÷2.21" etc., should be 1/2.21 = "45%" average population proportion etc.
# ordinal: empirical="cell"
tab_reg(gss_simple, outcome = "rincome", predictors = c("race", "marital", "relig", "age"),
        family = "ordinal", empirical = TRUE
) # No constants.
tab_reg(gss_simple, outcome = "rincome", predictors = c("race", "marital", "relig", "age"),
        family = "ordinal", empirical = TRUE, effect = "marginal" #, measure = "difference"
) # Constants are "+16.5%" etc., should be "16.5%", whole average population proportions.
tab_reg(gss_simple, outcome = "rincome", predictors = c("race", "marital", "relig", "age"),
        family = "ordinal", empirical = TRUE, effect = "marginal", measure = "ratio"
) # Constants "÷6.06" etc., should be 1/6.06 = "16.5%" average population proportion etc.



tab_reg(gss_simple, outcome = "married", predictors = c("race", "rincome", "tvhours", "age", "relig"),
         family = "binomial", empirical = TRUE
)
# - With two numeric predictors, the width of the sparkline appear to differ depending on the variable. 
#   I want to standardise that so they all have the same height and the same width. Here, the width of tvhours is about right,
#   (just a bit wider could do) but age wastes horizontal space. 
#   If you think it would be better to only keep the 95% central distribution, do it. If you think its best keep all for outliers, keep them.
# - In html, there are two visual defects in the first column with the variables names : at the bottom of "tvhours", 
#   there is a horizontal border that should not be here (no horizontal borders between other variables names) ; 
#   at the top and at the bottom of "Model fit", the horizontal border (which belongs) have a smaller linewidth
#   than on every other columns, which is not visually consistent.




#### shapes
tab_reg(gss_simple, outcome = "married", predictors = c("race", "rincome", "relig", "age"),
        family = "binomial", empirical = TRUE #, measure = "odds_ratio"
)
tab_reg(gss_simple, outcome = "married", predictors = c("race", "rincome", "relig", "age"), 
        shape = c(age = 3), 
        family = "binomial", empirical = TRUE #, measure = "odds_ratio"
)
tab_reg(gss_simple, outcome = "married", predictors = c("race", "rincome", "relig", "age"), 
        shape = c(age = "quadratic"), 
        family = "binomial", empirical = TRUE #, measure = "odds_ratio"
)
tab_reg(gss_simple, outcome = "married", predictors = c("race", "rincome", "relig", "age"), 
        shape = c(age = "sqrt"),
        family = "binomial", empirical = TRUE #, measure = "odds_ratio"
)
tab_reg(gss_simple, outcome = "married", predictors = c("race", "rincome", "relig", "age"), 
        shape = c(age = "log"), 
        family = "binomial", empirical = TRUE #, measure = "odds_ratio"
)
# - shapes are written nowhere for some numeric predictors 
#   - "quadratic" and quantiles are visible on the table (age^2 row added, perfect)
#   - "sqrt" and "log" are invisible since we removed the duplicated variable name in "levels" column:
#     add "√(x)" or "log(x)" at the start of the "levels". Or if you have a reliable idea for a more modern look (mathjax in html?), say it.
#   - I also want to change a bit the normal display, with or without shape, to something like : 
#      "√(x), per 1.04 (SD), at 6.43 (mean)", or "log(x), per 10, at 0 (min)", or "per 2.08 (2SD), at 0"
# - Also, the sparkline seems to be the same for identity, quadratic, sqrt, log. How to handle this ? 
#   Should the sparkline change with the shape so that the user can verify if the new shape is more linear with the outcome ? 
#   What is the standard in regression models assumptions checks ?

#### interactions
# options(tabxplor.parallel = 8, tabxplor.cleannames = TRUE, tabxplor.print = "html") # options(tabxplor.print = "console")

tab_reg(gss_simple, outcome = "married", predictors = c("rincome", "age*race"),
        family = "binomial", empirical = TRUE #, measure = "odds_ratio"
)
# - in html, "age × race" is written vertically, and it’s long (wastes vertical space, but here there’s horizontal space remaining): 
#   plase wrap it before the `×`, so it prints vertically in two columns ?
tab_reg(gss_simple, outcome = "married", predictors = c("race", "rincome", "age*tvhours"),
        shape = c(tvhours = "quartiles"),
        family = "binomial", empirical = TRUE #, measure = "odds_ratio"
)
# - the rows are not very clear, because the user can’t be sure which variable is what. Ex: "per SD/13.4 · [0,1)"
#   In interactions only, I would want: the main numeric variable, here "age", to be written at the start of "levels" ;
#    the quantile variable to ; the separetor to be "–". Example: "age per 13.4 (SD) — [0,1)"
# - in tooltips, the observed counterparts of "age × tvhours" have many `NA`s, tooltips are very long and unreadable ; 
#    calculate the meaningful quantities to populate it, or when a field is really NA the rule should be "never shows it in the tooltips"
# - "age × tvhours" displays an adjusted proportion here, is it right ? Is it meaningful ? 
#    Adjusted married proportion of the tvhours level given it’s specific age slope (how to word it) ?


#### stats footers
# - Are there standards and good practices for the thresholds after which max VIF is too big, or max dfbetas is too big, 
#   that we could reliably use to color the cell red like when a pvalue is >=5%, with statistically soundness ?




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

# options(tabxplor.theme = "light", tabxplor.print = "html", tabxplor.parallel = 8, tabxplor.cleannames = TRUE)
# tab_reg(gss_simple, outcome = "age", predictors = c("race", "rincome", "relig", "tvhours"),
#         family = "gaussian", empirical=TRUE
# )

options(tabxplor.theme = "print_ready", tabxplor.print = "html", tabxplor.parallel = 8, tabxplor.cleannames = TRUE)
tab(gss_simple, c(race, relig), c(party3, tvhours), pct = "row", na = "drop_all", 
   color = "difference", color_signif = "grey_non_signif",
)
tab(gss_simple, c(race, relig), c(party3, tvhours), pct = "row", na = "drop_all", 
   color = TRUE, color_signif = "grey_non_signif",
)
tab_reg(gss_simple, outcome = "age", predictors = c("race", "rincome", "relig", "tvhours"),
        family = "gaussian", empirical=TRUE
)

options(tabxplor.theme = "print_minimalistic", tabxplor.print = "html", tabxplor.parallel = 8, tabxplor.cleannames = TRUE)
tab(gss_simple, c(race, relig), c(party3, tvhours), pct = "row", na = "drop_all", 
   color = "difference", color_signif = "grey_non_signif",
)
tab(gss_simple, c(race, relig), c(party3, tvhours), pct = "row", na = "drop_all", 
   color = TRUE, color_signif = "grey_non_signif",
)
tab_reg(gss_simple, outcome = "age", predictors = c("race", "rincome", "relig", "tvhours"),
        family = "gaussian", empirical=TRUE
)

options(tabxplor.theme = "print_marks", tabxplor.print = "html", tabxplor.parallel = 8, tabxplor.cleannames = TRUE)
tab(gss_simple, c(race, relig), c(party3, tvhours), pct = "row", na = "drop_all", 
   color = "difference", color_signif = "grey_non_signif",
)
tab(gss_simple, c(race, relig), c(party3, tvhours), pct = "row", na = "drop_all", 
   color = TRUE, color_signif = "grey_non_signif",
)
# tab_reg(gss_simple, outcome = "age", predictors = c("race", "rincome", "relig", "tvhours"),
#         family = "gaussian", empirical=TRUE
# ) # kills stars

options(tabxplor.theme = "print_emphasis", tabxplor.print = "html", tabxplor.parallel = 8, tabxplor.cleannames = TRUE)
tab_reg(gss_simple, outcome = "age", predictors = c("race", "rincome", "relig", "tvhours"),
        family = "gaussian", empirical=TRUE
)
tab_reg(gss_simple, outcome = "married", predictors = c("race", "rincome", "relig", "age"),
        family = "binomial", empirical=TRUE
)
tab(gss_simple, c(race, relig), c(party3, tvhours), pct = "row", na = "drop_all", 
   color = "difference", color_signif = "grey_non_signif", display = "{base} ({ratio})"
)
tab(gss_simple, c(race, relig), c(party3, tvhours), pct = "row", na = "drop_all", 
   color = TRUE, color_signif = "grey_non_signif", display = "{base} ({ratio})"
)


# WITH EXPORTS



# The legend still says "Coloured: significantly different from the Total row... Uncoloured: either not significant, or a difference under...".




# Sparkline wrong alignment due to the U+2581–U+2588 special characters not being monospace. Cascadia Mono ?




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

