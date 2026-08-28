
# Manual reviews pass 6 ----

library(devtools)
load_all()
options(tabxplor.parallel = "auto", tabxplor.cleannames = TRUE, tabxplor.print = "html") # options(tabxplor.print = "console")

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
        family = "binomial"
)



tab_reg(gss_simple, outcome = "married", predictors = c("race", "rincome", "relig", "age"),
        family = "binomial", measure = "ratio"
)


# adjustment
tab_reg(gss_simple, outcome = "married", predictors = c("race", "rincome", "relig", "age"),
        family = "binomial", measure = "ratio", color = "adjustment"
)


# several outcomes
tab_reg(gss_simple, outcome = c("married", "tvhours"), predictors = c("race", "rincome", "relig", "age"),
        family = c("binomial", "poisson"), 
)


# predictor’s list
tab_reg(gss_simple, outcome = "married", 
        predictors = list(race  = "race", 
                          two   = c("race", "rincome"), 
                          three = c("race", "rincome", "relig"), 
                          full  = c("race", "rincome", "relig", "age") ),
        family = "binomial", measure = "ratio", color = "adjustment"
)

# tab_vars 
tab_reg(gss_simple, outcome = "married", tab_vars = "race",
        predictors = c("rincome", "relig", "age", "tvhours"),
        family = "binomial", measure = "ratio" , color ="between_groups"
)


#### the different families × measure × effects

# binomial
## effect = "conditional"
tab_reg(gss_simple, outcome = "married", predictors = c("race", "rincome", "relig", "age"),
  family = "binomial" #, measure = "odds_ratio", 
)
tab_reg(gss_simple, outcome = "married", predictors = c("race", "rincome", "relig", "age"),
        family = "binomial", measure = "log"
)
tab_reg(gss_simple, outcome = "married", predictors = c("race", "rincome", "relig", "age"),
        family = "binomial", link = "ratio"
)
tab_reg(gss_simple, outcome = "married", predictors = c("race", "rincome", "relig", "age"),
        family = "binomial", link = "difference"
) 
## effect = "marginal"
tab_reg(gss_simple, outcome = "married", predictors = c("race", "rincome", "relig", "age"),
        family = "binomial", measure = "difference"
) 
tab_reg(gss_simple, outcome = "married", predictors = c("race", "rincome", "relig", "age"),
        family = "binomial", measure = "ratio"
)
## effect = "at_reference"
tab_reg(gss_simple, outcome = "married", predictors = c("race", "rincome", "relig", "age"),
        family = "binomial", measure = "difference", effect = "at_reference"
)
tab_reg(gss_simple, outcome = "married", predictors = c("race", "rincome", "relig", "age"),
        family = "binomial", measure = "ratio", effect = "at_reference"
)

# gaussian
## effect = "conditional"
tab_reg(gss_simple, outcome = "age", predictors = c("race", "rincome", "relig", "tvhours"),
        family = "gaussian"
)
tab_reg(gss_simple, outcome = "age", predictors = c("race", "rincome", "relig", "tvhours"),
        family = "gaussian", link = "ratio"
)
## effect = "marginal"
tab_reg(gss_simple, outcome = "age", predictors = c("race", "rincome", "relig", "tvhours"),
        family = "gaussian", effect = "marginal" #, measure = "difference"
)
tab_reg(gss_simple, outcome = "age", predictors = c("race", "rincome", "relig", "tvhours"),
        family = "gaussian", measure = "ratio"
)
## effect = "at_reference"
tab_reg(gss_simple, outcome = "age", predictors = c("race", "rincome", "relig", "tvhours"),
        family = "gaussian", effect = "at_reference" #, measure = "difference"
) 
tab_reg(gss_simple, outcome = "age", predictors = c("race", "rincome", "relig", "tvhours"),
        family = "gaussian", measure = "ratio", effect = "at_reference"
)


# poisson
## effect = "conditional"
tab_reg(gss_simple, outcome = "tvhours", predictors = c("race", "rincome", "relig", "age"),
        family = "poisson"
)
tab_reg(gss_simple, outcome = "tvhours", predictors = c("race", "rincome", "relig", "age"),
        family = "poisson", measure = "log"
)
## effect = "marginal"
tab_reg(gss_simple, outcome = "tvhours", predictors = c("race", "rincome", "relig", "age"),
        family = "poisson", measure = "difference"
)
tab_reg(gss_simple, outcome = "tvhours", predictors = c("race", "rincome", "relig", "age"),
        family = "poisson", effect = "marginal"
)
## effect = "at_reference"
tab_reg(gss_simple, outcome = "tvhours", predictors = c("race", "rincome", "relig", "age"),
        family = "poisson", measure = "difference", effect = "at_reference"
)
tab_reg(gss_simple, outcome = "tvhours", predictors = c("race", "rincome", "relig", "age"),
        family = "poisson", measure = "ratio", effect = "at_reference"
)

# summed-score binomial
## effect = "conditional"
tab_reg(tea, outcome = "tea_where", family = "binomial", trials = length(tea_where_vars), 
        predictors = c("sex", "SPC", "Sport")
)
tab_reg(tea, outcome = "tea_where", family = "binomial", trials = length(tea_where_vars), 
        predictors = c("sex", "SPC", "Sport"), link = "ratio"
)
tab_reg(tea, outcome = "tea_where", family = "binomial", trials = length(tea_where_vars), 
        predictors = c("sex", "SPC", "Sport"), link = "difference"
)
## effect = "marginal"
tab_reg(tea, outcome = "tea_where", family = "binomial", trials = length(tea_where_vars), 
        predictors = c("sex", "SPC", "Sport"), measure = "difference"
)
tab_reg(tea, outcome = "tea_where", family = "binomial", trials = length(tea_where_vars), 
        predictors = c("sex", "SPC", "Sport"), measure = "ratio"
)
## effect = "at_reference"
tab_reg(tea, outcome = "tea_where", family = "binomial", trials = length(tea_where_vars), 
        predictors = c("sex", "SPC", "Sport"), measure = "difference", effect = "at_reference"
)
tab_reg(tea, outcome = "tea_where", family = "binomial", trials = length(tea_where_vars), 
        predictors = c("sex", "SPC", "Sport"), measure = "ratio", effect = "at_reference"
)



# multinomial:
## effect = "conditional"
tab_reg(gss_simple, outcome = "party3", predictors = c("race", "rincome", "relig", "age"),
        family = "multinomial"
)
## effect = "marginal"
tab_reg(gss_simple, outcome = "party3", predictors = c("race", "rincome", "relig", "age"),
        family = "multinomial", measure = "ratio", color="adjustment"
)
tab_reg(gss_simple, outcome = "party3", predictors = c("race", "rincome", "relig", "age"),
        family = "multinomial", measure = "difference"
)
## effect = "at_reference"
tab_reg(gss_simple, outcome = "party3", predictors = c("race", "rincome", "relig", "age"),
        family = "multinomial", effect = "at_reference" #, measure = "odds_ratio"
)
tab_reg(gss_simple, outcome = "party3", predictors = c("race", "rincome", "relig", "age"),
        family = "multinomial", measure = "ratio", effect = "at_reference"
)
tab_reg(gss_simple, outcome = "party3", predictors = c("race", "rincome", "relig", "age"),
        family = "multinomial", measure = "difference", effect = "at_reference"
)



# ordinal: empirical="column"
## effect = "conditional"
tab_reg(gss_simple, outcome = "rincome", predictors = c("race", "marital", "relig", "age"),
        family = "ordinal" #, shape = c(age = "quadratic")
)
## effect = "marginal": 
tab_reg(gss_simple, outcome = "rincome", predictors = c("race", "marital", "relig", "age"),
        family = "ordinal", measure = "ratio"
)
tab_reg(gss_simple, outcome = "rincome", predictors = c("race", "marital", "relig", "age"),
        family = "ordinal", measure = "difference"
)
## effect = "at_reference"
tab_reg(gss_simple, outcome = "rincome", predictors = c("race", "marital", "relig", "age"),
        family = "ordinal", measure = "difference", effect = "at_reference" 
)
tab_reg(gss_simple, outcome = "rincome", predictors = c("race", "marital", "relig", "age"),
        family = "ordinal", measure = "ratio", effect = "at_reference"
)




#### adjustement review
# options(tabxplor.parallel = 8, tabxplor.cleannames = TRUE, tabxplor.print = "html") # options(tabxplor.print = "console")

# binomial
## effect = "conditional"
tab_reg(gss_simple, outcome = "married", predictors = c("race", "rincome", "relig", "age"), 
        color="adjustment", family = "binomial" #, measure = "odds_ratio", 
)
tab_reg(gss_simple, outcome = "married", predictors = c("race", "rincome", "relig", "age"),
        family = "binomial", color="adjustment", measure = "log"
)
tab_reg(gss_simple, outcome = "married", predictors = c("race", "rincome", "relig", "age"),
        family = "binomial", color="adjustment", link = "ratio"
)
tab_reg(gss_simple, outcome = "married", predictors = c("race", "rincome", "relig", "age"),
        family = "binomial", color="adjustment", link = "difference"
) 
## effect = "marginal"
tab_reg(gss_simple, outcome = "married", predictors = c("race", "rincome", "relig", "age"),
        family = "binomial", color="adjustment", measure = "difference"
) 
tab_reg(gss_simple, outcome = "married", predictors = c("race", "rincome", "relig", "age"),
        family = "binomial", color="adjustment", measure = "ratio"
)

# gaussian
## effect = "conditional"
tab_reg(gss_simple, outcome = "age", predictors = c("race", "rincome", "relig", "tvhours"),
        family = "gaussian", color="adjustment"
)
tab_reg(gss_simple, outcome = "age", predictors = c("race", "rincome", "relig", "tvhours"),
        family = "gaussian", color="adjustment", link = "ratio"
)
## effect = "marginal"
tab_reg(gss_simple, outcome = "age", predictors = c("race", "rincome", "relig", "tvhours"),
        family = "gaussian", color="adjustment", effect = "marginal" #, measure = "difference"
)
tab_reg(gss_simple, outcome = "age", predictors = c("race", "rincome", "relig", "tvhours"),
        family = "gaussian", color="adjustment", measure = "ratio"
)

# poisson
## effect = "conditional"
tab_reg(gss_simple, outcome = "tvhours", predictors = c("race", "rincome", "relig", "age"),
        family = "poisson", color="adjustment"
)
tab_reg(gss_simple, outcome = "tvhours", predictors = c("race", "rincome", "relig", "age"),
        family = "poisson", color="adjustment", measure = "log"
)
## effect = "marginal"
tab_reg(gss_simple, outcome = "tvhours", predictors = c("race", "rincome", "relig", "age"),
        family = "poisson", color="adjustment", measure = "difference"
)
tab_reg(gss_simple, outcome = "tvhours", predictors = c("race", "rincome", "relig", "age"),
        family = "poisson", color="adjustment", effect = "marginal"
)

# summed-score binomial
## effect = "conditional"
tab_reg(tea, outcome = "tea_where", family = "binomial", trials = length(tea_where_vars), 
        predictors = c("sex", "SPC", "Sport"),  color="adjustment"
)
tab_reg(tea, outcome = "tea_where", family = "binomial", trials = length(tea_where_vars), 
        predictors = c("sex", "SPC", "Sport"), color="adjustment", link = "ratio"
)
tab_reg(tea, outcome = "tea_where", family = "binomial", trials = length(tea_where_vars), 
        predictors = c("sex", "SPC", "Sport"), color="adjustment", link = "difference"
)
## effect = "marginal"
tab_reg(tea, outcome = "tea_where", family = "binomial", trials = length(tea_where_vars), 
        predictors = c("sex", "SPC", "Sport"), color="adjustment", measure = "difference"
)
tab_reg(tea, outcome = "tea_where", family = "binomial", trials = length(tea_where_vars), 
        predictors = c("sex", "SPC", "Sport"), color="adjustment", measure = "ratio"
)

# multinomial:
## effect = "conditional"
tab_reg(gss_simple, outcome = "party3", predictors = c("race", "rincome", "relig", "age"),
        family = "multinomial", color="adjustment"
)
## effect = "marginal"
tab_reg(gss_simple, outcome = "party3", predictors = c("race", "rincome", "relig", "age"),
        family = "multinomial", color="adjustment", measure = "ratio", color="adjustment"
)
tab_reg(gss_simple, outcome = "party3", predictors = c("race", "rincome", "relig", "age"),
        family = "multinomial", color="adjustment", measure = "difference"
)

# ordinal: empirical="column"
## effect = "conditional"
tab_reg(gss_simple, outcome = "rincome", predictors = c("race", "marital", "relig", "age"),
        family = "ordinal", color="adjustment" #, shape = c(age = "quadratic")
)
## effect = "marginal": 
tab_reg(gss_simple, outcome = "rincome", predictors = c("race", "marital", "relig", "age"),
        family = "ordinal", color="adjustment", measure = "ratio"
)
tab_reg(gss_simple, outcome = "rincome", predictors = c("race", "marital", "relig", "age"),
        family = "ordinal", color="adjustment", measure = "difference"
)



#### reg_check_plots()
# options(tabxplor.parallel = 8, tabxplor.cleannames = TRUE, tabxplor.print = "html") # options(tabxplor.print = "console")

# binomial
tab_reg(gss_simple, outcome = "married", predictors = c("race", "rincome", "relig", "age", "tvhours"), 
        family = "binomial") |> 
  reg_check_plots()
tab_reg(gss_simple, outcome = "married", predictors = c("race", "rincome", "relig", "age", "tvhours"),
        family = "binomial", link = "ratio") |> 
  reg_check_plots()
tab_reg(gss_simple, outcome = "married", predictors = c("race", "rincome", "relig", "age", "tvhours"),
        family = "binomial", link = "difference") |> 
  reg_check_plots()
# gaussian
tab_reg(gss_simple, outcome = "age", predictors = c("race", "rincome", "relig", "tvhours"),
        family = "gaussian") |> 
  reg_check_plots()
tab_reg(gss_simple, outcome = "age", predictors = c("race", "rincome", "relig", "tvhours"),
        family = "gaussian", link = "ratio") |> 
  reg_check_plots()
# poisson
tab_reg(gss_simple, outcome = "tvhours", predictors = c("race", "rincome", "relig", "age"),
        family = "poisson") |> 
  reg_check_plots()
tab_reg(gss_simple, outcome = "tvhours", predictors = c("race", "rincome", "relig", "age"),
        family = "poisson", measure = "log") |> 
  reg_check_plots()
# summed-score binomial
tab_reg(tea, outcome = "tea_where", family = "binomial", trials = length(tea_where_vars), 
        predictors = c("sex", "SPC", "Sport") ) |> 
  reg_check_plots()
tab_reg(tea, outcome = "tea_where", family = "binomial", trials = length(tea_where_vars), 
        predictors = c("sex", "SPC", "Sport"), link = "ratio") |> 
  reg_check_plots()
tab_reg(tea, outcome = "tea_where", family = "binomial", trials = length(tea_where_vars), 
        predictors = c("sex", "SPC", "Sport"), link = "difference") |> 
  reg_check_plots()
# multinomial:
tab_reg(gss_simple, outcome = "party3", predictors = c("race", "rincome", "relig", "age", "tvhours"),
        family = "multinomial") |> 
  reg_check_plots()
# ordinal: 
tab_reg(gss_simple, outcome = "rincome", predictors = c("race", "marital", "relig", "age", "tvhours"),
        family = "ordinal") |> 
  reg_check_plots()

# - Please give me a trick to not have to pass `data =` argument twice.
#   Maybe it’s a table-level metadata with the code that was passe to `tab_reg()` `data =` argument, maybe 
#   it’s something else, you try and if it fails you just give the current normal error message.
# - The black red formatting is good, but the text grey is to light for white background and difficult to read.
#   Make it "#555555" to keep it lighter than the titles themselves. Also, the text is cut: only the start is showns,
#   you should make it wrap on several line (max around 3 short lines + the title, 2 would be great, 1 even greater). 
#   Please synthetise the text a lot: do not try to explain everything, there’s no space for that here, 
#   just briefly say what it is, and point the student to what it should look at.
#   For example for "Linearity" : "Do the observed curve matches the straight line ?" 
#   (or something better? and another line if needed ?) Also, titles are all one-word 
#   short, good, so there is a little space to convey something more in this line, 
#   like "Linearity: observed curves ... (10 bins, ±2SE)" (complete and improve, or do better, 
#   keep very concise; second part in plain fontface, to keep "Linearity" striking alone in bold)
#   All text must stay clear, simple, pedagogical, with only few expressions for experts assessing what it is.
# - Remove the "Dispersion" and the "Collinearity" panels from the default (often useless, 
#   the table footer is often enough here). Add an expert argument to customise what is plotted 
#   (not really to choose because some models can’t draw some plots, but to choose the list of what it’s plotted
#   when it’s available to the model ; add an "all" option to restore Collinearity and Dispersion ; max 6 plots ?).
#   In Collinearity, ensure the name of the age^2 variable, which contains a math operation in it, is simplified
#   to not waste horizontal space.
# - Linearity checks for numeric predictors: if not already done, ensure the facets spread on max 2 col, 
#   then are splitted by rows. Name of the numeric variable in bold, and a bit bigger. 
#   Ensure the `shape` transformation are applied here, and reflected in the facet name (ex: "age²"). 
#   The "empirical logit: " axis label should also give the math formula of the plot, ex for logit link something like 
#   "log(married%/1(married%)) = ..." (correct this, it may be mistaken; display in beautiful mathjax, etc.)
# - Residuals: the goal is not to look if ~95% are inside the banner, but to look if ~<5% are outside 
#   (be concrete to point the student towards what is to be done) ?
# - With several outcomes or models there would be one plot result per each of them, so a global title must 
#   state what the outcome name is, the family, the link, the `reg_formula`, and "assumption checks" (compose 
#   that in a meaningful, readable, concise way).
# - The plot should be tranlated in French in French locale (matching the way the footer terms are translated).
# - In ordinal Proportionality panel, the color legend at the right lose half horizontal space (put it at the bottom of the plot)
#   Facets with maximum 4 columns, then fill the rows.
#   Is the Linearity plot ok enough for ordinal assumption check here, what is the standard 
#   one (same for multinomial) ?

# Errors
# - tab_reg(gss_simple, outcome = c("married", "rincome"), predictors = c("race", "relig", "age", "tvhours"), 
#         family = c("binomial", "ordinal"), shape = c(age = "quadratic")) |> 
#   reg_check_plots(data = gss_simple)
# Error in `ggplot2::geom_hline()`:
# ! Problem while setting up geom aesthetics.
# ℹ Error occurred in the 2nd layer.
# Caused by error in `check_aesthetics()`:
# ! Aesthetics must be either length 1 or the same as the data (4).
# ✖ Fix the following mappings: `linetype`.
# 
# - tab_reg(tea, outcome = "tea_where", family = "binomial", trials = length(tea_where_vars), 
#          predictors = c("sex", "SPC", "Sport") ) |> 
#   reg_check_plots(data = gss_simple)
# Error in `reg_check_plots()` at dev/review_manual/review.R:254:1:
# ! No model could be refitted from `x`.





#### forest_plot() 
regressions <- tab_reg(gss_simple, outcome = c("married", "age", "tvhours"), 
                       predictors = c("race", "rincome", "relig"), 
                       family = c("binomial", "gaussian", "poisson"))
regressions |> forest_plot()
summed_score <- tab_reg(tea, outcome = "tea_where", family = "binomial", trials = length(tea_where_vars), 
        predictors = c("sex", "SPC", "Sport"), measure = "ratio") 
summed_score|> forest_plot()
multinom <- tab_reg(gss_simple, outcome = "party3", predictors = c("race", "marital", "relig", "age", "tvhours"),
        family = "multinomial", measure = "ratio") 
multinom |> forest_plot()
ordinal <- tab_reg(gss_simple, outcome = "rincome", predictors = c("race", "marital", "relig", "age", "tvhours"),
        family = "ordinal", measure = "ratio") 
ordinal |> forest_plot()

# I don’t love the current default theme at all, too gray, unreadable, not enough colors, etc. Screen attached.
#  I want to redo it. I want you to improve this default, starting from two other forest plot (to not copy them entirely, 
#  just take some good elements): 
# - one old, from my package ggfacto (the only package with tabxplor in Imports), with many obvious flaws, 
#   (logistic reg only, one outcome at a time, black and white, the column width were sometimes 
#   quite unpredictable, not using the break scales we want here, etc.), 
#   but some good formatting and theme elements that may be useful here: black lines with whiskers 
#   and error bars, good dotted null line)
#  `/home/dev1/github/ggfacto/R/geometrical_data_analysis.R` `ggfacto::pers_or_plot`, who use `ggfacto::theme_facto`
gss_simple |> 
  mutate(rincome = factor(rincome, ordered = FALSE)) |> # "ordered" not working
  ggfacto::pers_or_plot(dependent = "married", explanatory = c("race", "rincome", "relig", "tvhours"))
# - one newer, with strong visual things, but that would need a bit of polish, 
#   I used to teach confidence intervals to students. 
#   It’s stronger points are: tabxplor colors used to color the whole whisker (not only the point), 
#   making it as easy as in the table ; also please, like in the second plot of the section
#   add an option to color the whole breaks with background colors (using the relevant palette), 
#   that could be used to teach tabxplor color_signif policies in a visually striking way (this one should be opt-in)
# `/home/dev1/github/formations_stat/M1S1_02.Rmd`, section "## Commenter un tableau à faibles effectifs", 
#   "### Méthode 2 : différences après marge d'erreur"

# Also : 
# - I want to keep the current minimal layout with variables names vertically at left, 
#   levels in breaks/ticks (but all pure black, not unreadable grey ; references in bold)
# - Like in the examples, start from theme_minimal and build from there
# - I want to print the actual estimate at the center of the whisker (since we don’t use a column for 
#   that like in pers_or_plot()). The point indicating the center is bad, 
# - All footer lines aligned left. Three options here : hort footer (like console ; default) ; 
#   full footer (but some lines are long and cut, they should be wrapped somehow) ; no footer 
# - outcomes names in facets in bold.
# - the displayed range should not be fixed on the breaks, but adapted to the real minimum and maximum of each facet
#   (currently age outcome "relig" only show one whisker, all the others at out of the range)
# - no signif stars in the plot at all (the goal is to read significance directly on the whiskers)
# - start from the real colors breaks to always print them, but continue the breaks (on a multiplicative scale,
#   just double the last break each time, if the last break is ×2, then ×4, ×8, etc. ; on an additive scale I don’t know, find a rule)
# - legend position in the arguments, default to where it would lose the less space.





# Round 2
regressions <- tab_reg(gss_simple, outcome = c("married", "age", "tvhours"), 
                       predictors = c("race", "rincome", "relig"), 
                       family = c("binomial", "gaussian", "poisson"))
regressions |> forest_plot()
# - Add horizontal lines between predictors, or between row_vars/tab_vars. 
# - The breaks dotted lines stop a bit to indicate separation between predictors, but it’s a bit to faint, 
#   please make the gap bigger if you can (if you need to, use a workaround, like background color rectangle
#   over it), like it should only start just above the reference line square (same for the null dotted line).
# - Strangely, for empirical counterparts, "married" have filled black points, but "age" and "tvhours" 
#   have points with black line and white fill (should be filled black too). Also "married" have no whisker,
#   but "age" and "tvhours" models have the grey band not on the point (not offsetted from the model whisker, 
#   so it’s). Just offset it, and replace it with a whisker 
#   (with smaller linewidth and error bars length than the main one; pure black.)
# - By the way, the footer of "age - mean difference" doesn’t say the unit is SD (it shoul mention it briefly, 
#   like in the first break only ! Here or in every short color legend ?
#   Here in the forest plot, use 1 digit in the breaks ticks text too (currently 2).
# - No possibility to use colors in footers color legends too, like everywhere else (without Suggests ?) ? 
# - Remove the text ("1", "0") above reference lines (unreadable with the null vline, and useless).  

tab(gss_simple, c(age, rincome, party3), married, pct = "row", color = TRUE, color_signif = "ignore") |> forest_plot()
tab(gss_simple, c(age, rincome, party3), married, pct = "row", color = TRUE, color_signif = "grey_non_signif") |> forest_plot()
tab(gss_simple, c(age, rincome, party3), married, pct = "row", color = TRUE, color_signif = "guaranteed_effect") |> forest_plot()
# - the "guaranteed_effect" one have 1/3 vertical space lost at the top, and strange red/blue bands there.
#   `tab(gss_simple, c(age, rincome, party3), married, pct = "row", color = TRUE, color_signif = "grey_non_signif", ref =1)` also does this, blue bands on Married.
# - grey_non_signif" legend/guide is ok, but displays in an unreadable order because it may fill by columns on two rows 
#   (I want the legend on only one line, with breaks on the same order than in the plot, "under" then "over" the not significant)
# - the "guaranteed_effect" legend/guide is a bit strange, the >=+20 break miss it’s dark blue dotted line (same for <= -20), 
#   the >=10 blue dotted line miss it’s whisker (same for <=-10 dotted line). Also, it prints "Difference vs the Total row", 
#   but should’nt it say something like "guaranteed (95%) difference vs the Total row" ?
# - Would there be a way to show the inward error bar of the whisker, which **IS** the guaranteed effect, 
#    bigger than the outward error bar of the whisker ?
#   The reference rows hade dissapear because `ref="tot"`, please add the right Total, it’s the reference for comparison, it’s important.

tab(gss_simple, c(age, rincome, party3), marital, pct = "row", color = TRUE, color_signif = "grey_non_signif", ref = 1) |> forest_plot()
# - Here, many points have no whiskers at all, normal (too thin for real ?), or ?
# - in color guide/legend "<-5" break have dotted line but no whisker.
# - blue and red bands hell destroys the plot.
# - Could the "Newcombe score interval, 95% confidence" stuff could appear in "Percentage points (95% CI)"
#   as "Percentage points (95% CI, Newcombe score interval)" ? It should not clutter the many outcomes/many scales
#  regression forest plot with statistical stuff, though, so it may not be a good idea.


# Round 3

# - empirical = TRUE with no adjustment : I want the empirical point and wisker to be only
#   two linewidths of the main whisker below the main whisker, would it be possible (on all viewports or close to it ?) ? 
# - (Would it work also for the adjustement case below, of would adjustement need more space to be more readable ?)

regressions_adj <- tab_reg(gss_simple, outcome = c("married", "age", "tvhours"),  predictors = c("race", "rincome", "relig"), 
                           family = c("binomial", "gaussian", "poisson"), 
                           color = "adjustment")
regressions_adj |> forest_plot()
ordinal <- tab_reg(gss_simple, outcome = "rincome", predictors = c("race", "marital", "relig", "age", "tvhours"),
        family = "ordinal", measure = "ratio", color = "adjustment") 
ordinal |> forest_plot()
# - empirical = TRUE with color = "adjustment" currently does nothing different, it just colors the main whisker and CI
#   I want a true user-friendly and readable way to color **the adjustement only**, with its own color and it’s own CI.
#   What would be the right geometrical way to do this ? Should it be an arrow, or a band between the empirical point and the model point ? 
#   How to represent the CI of the adjustment ? 
#   Please study this, and make me a well-designed proposition.

# Round 4
# - In adjustement, mode, please do the following display : 
#    - main model whiskers in "grey2" color to put them a bit less in focus.
#    - main model square colored the same color as the arrow
#    - arrow stay on the same line as now, but the point and the acceptance brackets goes on a line just below,
#      from the same y offset than used between the model whisker and the empirical point.
#    - acceptance bracket always in very thin linewidth black.
#    - arrow with a bit more linewidth to put it more in focus.
#    - There is no ggplot2 color legend/guide just adjustement right now, but one is needed because the breaks
#      and dotted-line, that act as a good legend in the normal regression case, are not what the colors are here 
#      (so keep their current color but do not add them in the legend/guide like they are for crosstables,
#      just the arrows in legend/guide ?)
# - The y offset between the model square and the empirical point should be in a forest_plot(), 
#   and the y offset between the model square and the measure label too, because the visually good result 
#   depends on the viewport. Make the measure label background a bit less tranlucent, a bit more opaque.
# - add a display argument here too, accepting {} display tokens, etc., for the user to choose what to print 
#   in the model text/label, but keep the same defaults than now.

# Round 5
# Actually, in adjustement mode, reput the empirical point with acceptance bracket on the same line than
#  the arrow, but in the foreground : since the acceptance bracket is much thinner, it won’t remove the color.
# Add a bit more linewidth for the arrow line, an a bit bigger closed arrow head.
# "grey2" is not light enough for the main model whiskers colors, put a lighter grey
# Maximum area size of the main models square 1.5 times bigger than now. Put the max size in an argument for tweakability.
# There are not acceptance brackets in the "married" plot, is it normal (very small error for binomial / odds ratio is normal here ?)
# Footer legend only takes half horizontal space, isn’t there a way to take all the horizontal text 
# available but wrap when there is not enough space in one line to avoid the text being cut
#  (if too complitaced, do nothing) ?






#### the different families × effects × measure: custom displays

# binomial
## effect = "conditional"
model <- tab_reg(gss_simple, outcome = "married", predictors = c("race", "rincome", "relig", "age"),
        family = "binomial" #, measure = "odds_ratio"
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
        family = "poisson"
) 
tab_reg(gss_simple, outcome = "tvhours", predictors = c("race", "rincome", "relig", "age"),
        family = "poisson", effect = "marginal", measure = "ratio"
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
        family = "binomial" #, measure = "odds_ratio"
) # Constant "1/1.19", baseline odds, formatting is ok.
tab_reg(gss_simple, outcome = "married", predictors = c("race", "rincome", "relig", "age"),
        family = "binomial", measure = "log"
) # Constant "-0.17", log(odds) ok. 
tab_reg(gss_simple, outcome = "married", predictors = c("race", "rincome", "relig", "age"),
        family = "binomial", measure = "ratio"
) # Constant "÷2.30", should be baseline reference profile proportion "43%" ?
tab_reg(gss_simple, outcome = "married", predictors = c("race", "rincome", "relig", "age"),
        family = "binomial", measure = "difference"
) # Constant "+45.7%", should be "45.7%".
tab_reg(gss_simple, outcome = "married", predictors = c("race", "rincome", "relig", "age"),
        family = "binomial", effect = "marginal" #, measure = "difference"
)
tab_reg(gss_simple, outcome = "married", predictors = c("race", "rincome", "relig", "age"),
        family = "binomial", effect = "marginal", measure = "ratio"
) # Constant "×2.05", should be average proportion "49%" ?
# gaussian
tab_reg(gss_simple, outcome = "age", predictors = c("race", "rincome", "relig", "tvhours"),
        family = "gaussian"
) # Constant "+40.76", should be "40.76"
tab_reg(gss_simple, outcome = "age", predictors = c("race", "rincome", "relig", "tvhours"),
        family = "gaussian", measure = "ratio"
) # Constant "×40.47", should be "40.47". 
tab_reg(gss_simple, outcome = "age", predictors = c("race", "rincome", "relig", "tvhours"),
        family = "gaussian", effect = "marginal", measure = "ratio"
) # Constant "×42.36", should be "42.36". 
# poisson
tab_reg(gss_simple, outcome = "tvhours", predictors = c("race", "rincome", "relig", "age"),
        family = "poisson"
) # Constant "×2.95", should be "2.95"
tab_reg(gss_simple, outcome = "tvhours", predictors = c("race", "rincome", "relig", "age"),
        family = "poisson", measure = "log"
) # Constant "+1.08", should be "1.08"
tab_reg(gss_simple, outcome = "tvhours", predictors = c("race", "rincome", "relig", "age"),
        family = "poisson", effect = "marginal"# , measure = "difference"
) # Constant "+2.56", should be "2.56"
# summed-score binomial
tab_reg(tea, outcome = "tea_where", family = "binomial", trials = length(tea_where_vars), 
        predictors = c("sex", "SPC", "Sport") 
) # Constant "1/1.20", baseline odds ok.
tab_reg(tea, outcome = "tea_where", family = "binomial", trials = length(tea_where_vars), 
        predictors = c("sex", "SPC", "Sport"), measure = "ratio"
) # Constant "1/1.20". Here all RR in the table are displayed as OR, fix this (math problem or display problem ?) !
tab_reg(tea, outcome = "tea_where", family = "binomial", trials = length(tea_where_vars), 
        predictors = c("sex", "SPC", "Sport"), measure = "difference"
) # Constant "+2.72", should be "2.72"
tab_reg(tea, outcome = "tea_where", family = "binomial", trials = length(tea_where_vars), 
        predictors = c("sex", "SPC", "Sport"), effect = "marginal"# # measure = "difference"
) # Constant "+2.58", should be "2.58"
tab_reg(tea, outcome = "tea_where", family = "binomial", trials = length(tea_where_vars), 
        predictors = c("sex", "SPC", "Sport"), effect = "marginal", measure = "ratio"
)
# multinomial
tab_reg(gss_simple, outcome = "party3", predictors = c("race", "rincome", "relig", "age"),
        family = "multinomial"
) # Constants are baseline odds, ok.
tab_reg(gss_simple, outcome = "party3", predictors = c("race", "rincome", "relig", "age"),
        family = "multinomial", effect = "marginal"
) # Constants "+45.3%" etc., should be "45.3%" etc.
tab_reg(gss_simple, outcome = "party3", predictors = c("race", "rincome", "relig", "age"),
        family = "multinomial", effect = "marginal", measure = "ratio"
) # Constants "÷2.21" etc., should be 1/2.21 = "45%" average population proportion etc.
# ordinal: empirical="cell"
tab_reg(gss_simple, outcome = "rincome", predictors = c("race", "marital", "relig", "age"),
        family = "ordinal"
) # No constants.
tab_reg(gss_simple, outcome = "rincome", predictors = c("race", "marital", "relig", "age"),
        family = "ordinal", effect = "marginal" #, measure = "difference"
) # Constants are "+16.5%" etc., should be "16.5%", whole average population proportions.
tab_reg(gss_simple, outcome = "rincome", predictors = c("race", "marital", "relig", "age"),
        family = "ordinal", effect = "marginal", measure = "ratio"
) # Constants "÷6.06" etc., should be 1/6.06 = "16.5%" average population proportion etc.



tab_reg(gss_simple, outcome = "married", predictors = c("race", "rincome", "tvhours", "age", "relig"),
         family = "binomial"
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
        family = "binomial" #, measure = "odds_ratio"
)
tab_reg(gss_simple, outcome = "married", predictors = c("race", "rincome", "relig", "age"), 
        shape = c(age = 3), 
        family = "binomial" #, measure = "odds_ratio"
)
tab_reg(gss_simple, outcome = "married", predictors = c("race", "rincome", "relig", "age"), 
        shape = c(age = "quadratic"), 
        family = "binomial" #, measure = "odds_ratio"
)
tab_reg(gss_simple, outcome = "married", predictors = c("race", "rincome", "relig", "age"), 
        shape = c(age = "sqrt"),
        family = "binomial" #, measure = "odds_ratio"
)
tab_reg(gss_simple, outcome = "married", predictors = c("race", "rincome", "relig", "age"), 
        shape = c(age = "log"), 
        family = "binomial" #, measure = "odds_ratio"
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
        family = "binomial" #, measure = "odds_ratio"
)
# - in html, "age × race" is written vertically, and it’s long (wastes vertical space, but here there’s horizontal space remaining): 
#   plase wrap it before the `×`, so it prints vertically in two columns ?
tab_reg(gss_simple, outcome = "married", predictors = c("race", "rincome", "age*tvhours"),
        shape = c(tvhours = "quartiles"),
        family = "binomial" #, measure = "odds_ratio"
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


# pillar abbreviations tests
tab(gss_simple, c(race, relig), c(party3, tvhours), pct = "row", na = "drop_all",
   color = TRUE, color_signif = "grey_non_signif", ref = 1
) |> tab_export()
tab_reg(gss_simple, outcome = "married", predictors = c("race", "rincome", "relig", "age"), 
        family = "binomial", display = "{diff} [{OR}] ({base})"
)

tab(gss_simple, c(race, relig), c(party3, tvhours), pct = "row", na = "drop_all",
   color = TRUE, color_signif = "grey_non_signif", ref = 1
) |> tab_export()
tab_reg(gss_simple, outcome = "married", predictors = c("race", "rincome", "relig", "age"), 
        family = "binomial", display = "{diff} [{OR}] ({base})"
) |> tab_export()

tab(gss_simple, c(race, relig), c(party3, tvhours), pct = "row", na = "drop_all",
   color = TRUE, color_signif = "grey_non_signif", ref = 1
) |> tab_export("xl", path = "~/Excel_test.xlsx")
tab_reg(gss_simple, outcome = "married", predictors = c("race", "rincome", "relig", "age"), 
        family = "binomial", display = "{diff} [{OR}] ({base})"
) |> tab_export("xl", path = "~/Excel_test.xlsx")

tab(gss_simple, c(race, relig), c(party3, tvhours), pct = "row", na = "drop_all",
   color = TRUE, color_signif = "grey_non_signif", ref = 1
) |> tab_export("md")
tab_reg(gss_simple, outcome = "married", predictors = c("race", "rincome", "relig", "age"), 
        family = "binomial", display = "{diff} [{OR}] ({base})"
) |> tab_export("md")







### exports and display tests ----
list(
  tab(gss_simple, c("rincome", "age"), "marital", pct = "row", color = TRUE),
  tab(gss_simple, c("rincome", "age"), "marital", pct = "row", color = TRUE, color_signif = "grey_non_signif"),
  tab(gss_simple, c("rincome", "age"), "marital", pct = "row", color = TRUE, color_signif = "guaranteed_effect"),
  tab(gss_simple, c("rincome", "age"), "marital", pct = "row", color = TRUE, ref = 1),
  tab(gss_simple, c("rincome", "age"), "marital", pct = "row", color = TRUE, ref = 1, color_signif = "grey_non_signif"),
  tab(gss_simple, c("rincome", "age"), "marital", pct = "row", color = TRUE, ref = 1, color_signif = "guaranteed_effect")
) |> 
  tab_export("xl", path = "~/Excel_test.xlsx", replace = TRUE)

list(
  tab_reg(gss_simple, outcome = "married", predictors = c("rincome", "race", "age"), family = "binomial"), 
  tab_reg(gss_simple, outcome = "tvhours", predictors = c("rincome", "race", "age"), family = "poisson"),
  tab_reg(gss_simple, outcome = "tvhours", predictors = c("rincome", "race", "age"), family = "poisson", color="adjustment"),
  tab_reg(gss_simple, outcome = "age", predictors = c("rincome", "race", "tvhours"), family = "gaussian")
) |> 
  tab_export("xl", path = "~/Excel_test_reg.xlsx", replace = TRUE)



# "md"
tab_reg(gss_simple, outcome = c("married", "tvhours"), predictors = c("race", "rincome", "relig", "age"),
        family = c("binomial", "poisson"), 
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







#### the different main use cases ----
# options(tabxplor.print = "console")
tab_reg(gss_simple, outcome = "married", predictors = c("race", "rincome", "relig", "age"),
        family = "binomial") |> 
  tab_export()
# adjustment
tab_reg(gss_simple, outcome = "married", predictors = c("race", "rincome", "relig", "age"),
        family = "binomial", measure = "ratio", color = "adjustment") |> 
  tab_export()
# several outcomes
tab_reg(gss_simple, outcome = c("married", "tvhours"), predictors = c("race", "rincome", "relig", "age"),
        family = c("binomial", "poisson")) |> 
  tab_export(theme = "dark")
# predictor’s list
tab_reg(gss_simple, outcome = "married", 
        predictors = list(race  = "race", 
                          two   = c("race", "rincome"), 
                          three = c("race", "rincome", "relig"), 
                          full  = c("race", "rincome", "relig", "age") ),
        family = "binomial", measure = "ratio", color = "adjustment") |> 
  tab_export()
# tab_vars 
tab_reg(gss_simple, outcome = "married", tab_vars = "race",
        predictors = c("rincome", "relig", "age", "tvhours"),
        family = "binomial", measure = "ratio" , color ="between_groups") |> 
  tab_export()



# black and white publication ready tables ----
load_all()

# options(tabxplor.theme = "light", tabxplor.print = "html", tabxplor.parallel = 8, tabxplor.cleannames = TRUE) # options(tabxplor.print = "console")
# tab_reg(gss_simple, outcome = "age", predictors = c("race", "rincome", "relig", "tvhours"),
#         family = "gaussian", empirical=TRUE
# )

options(tabxplor.theme = "print_ready", tabxplor.print = "html", tabxplor.parallel = 8, tabxplor.cleannames = TRUE)
tab(gss_simple, c(relig, age), c(party3, tvhours), pct = "row", na = "drop", 
   color = "difference", color_signif = "grey_non_signif",
)
tab(gss_simple, c(relig, age), c(party3, tvhours), pct = "row", na = "drop", 
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

