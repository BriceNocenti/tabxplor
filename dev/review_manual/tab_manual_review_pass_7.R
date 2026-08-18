
# Manual reviews pass 6 ----

library(devtools)
load_all()
options(tabxplor.parallel = TRUE, tabxplor.cleannames = TRUE, tabxplor.print = "kable")


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


#### the different families × effects × measure displays

# binomial
## effect = "coeff"
tab_reg(gss_simple, outcome = "married", predictors = c("race", "rincome", "relig", "age"),
        family = "binomial", empirical = TRUE #, measure = "odds_ratio"
)
tab_reg(gss_simple, outcome = "married", predictors = c("race", "rincome", "relig", "age"),
        family = "binomial", empirical = TRUE, display = "ame"
)
tab_reg(gss_simple, outcome = "married", predictors = c("race", "rincome", "relig", "age"),
        family = "binomial", empirical = TRUE, measure = "log"
)
tab_reg(gss_simple, outcome = "married", predictors = c("race", "rincome", "relig", "age"),
        family = "binomial", empirical = TRUE, measure = "ratio"
)
tab_reg(gss_simple, outcome = "married", predictors = c("race", "rincome", "relig", "age"),
        family = "binomial", empirical = TRUE, measure = "difference"
) # what do this one do ? Statistically sound ?

## effect = "marginal"
tab_reg(gss_simple, outcome = "married", predictors = c("race", "rincome", "relig", "age"),
        family = "binomial", empirical = TRUE, effect = "marginal" #, measure = "difference"
)
tab_reg(gss_simple, outcome = "married", predictors = c("race", "rincome", "relig", "age"),
        family = "binomial", empirical = TRUE, effect = "marginal", measure = "ratio"
)


## effect = "at_reference"
tab_reg(gss_simple, outcome = "married", predictors = c("race", "rincome", "relig", "age"),
        family = "binomial", effect = "at_reference" #, measure = "difference"
)
tab_reg(gss_simple, outcome = "married", predictors = c("race", "rincome", "relig", "age"),
        family = "binomial", effect = "at_reference", measure = "ratio"
)

# gaussian
## effect = "coeff"
tab_reg(gss_simple, outcome = "age", predictors = c("race", "rincome", "relig", "tvhours"),
        family = "gaussian", empirical = TRUE
)

## effect = "marginal"

## effect = "at_reference"


# poisson
## effect = "coeff"
tab_reg(gss_simple, outcome = "tvhours", predictors = c("race", "rincome", "relig", "age"),
        family = "poisson", empirical = TRUE
)
## effect = "marginal"

## effect = "at_reference"


# summed-score binomial
## effect = "coeff"
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
        predictors = c("sex", "SPC", "Sport"), empirical = TRUE, effect = "marginal", # measure = "difference"
)
tab_reg(tea, outcome = "tea_where", family = "binomial", trials = length(tea_where_vars), 
        predictors = c("sex", "SPC", "Sport"), empirical = TRUE, effect = "marginal", measure = "ratio"
)


## effect = "at_reference"


# multinomiale
## effect = "coeff"
tab_reg(gss_simple, outcome = "party3", predictors = c("race", "rincome", "relig", "age"),
        family = "binomial", empirical = TRUE
)
## effect = "marginal"

## effect = "at_reference"

# ordinal
## effect = "coeff"
tab_reg(gss_simple, outcome = "rincome", predictors = c("race", "marital", "relig", "age"),
        family = "binomial", empirical = TRUE
)
## effect = "marginal"

## effect = "at_reference"










# Variable name duplication in exports
tab_reg(gss_simple, outcome = c("married", "tvhours"), predictors = c("race", "rincome", "relig", "age"),
        family = c("binomial", "poisson"), empirical = TRUE, 
) |> 
  tab_export("md")
# |           | levels               |Emp. % [married]  |Emp. OR [married]  |Model OR [married]  | |Emp. rate [tvhours]  |Emp. IRR [tvhours]  |Model IRR [tvhours]  |
# |:----------|:---------------------|-----------------:|------------------:|-------------------:|-|--------------------:|-------------------:|--------------------:|
# |           |                      | *married: 01-Married*|               |                    | | *tvhours*           |
tab_reg(gss_simple, outcome = c("married", "tvhours"), predictors = c("race", "rincome", "relig", "age"),
        family = c("binomial", "poisson"), empirical = TRUE, 
) |> 
  tab_export()
# First row : married: 01-Married	tvhours
# Second row . "levels" "Obs_%\n[married]" "Obs_OR\n[married]" "Model_OR\[married]" "Obs_rate\[tvhours]" "Obs_IRR\[tvhours]" "Model_IRR\[tvhours]"
# - On exports, much space is lost for duplicated informations : the variable name is aldeady on the col_var row,
#   but it’s also repeated on each column headers / second row. We must keep the "[married]" "[tvhours]" precision in
#   console, to avoid name clashed, but it’s not needed in exports since the col_var names row already gives the information : 
#   please **remove the [] in levels / headers row in html and Excel** in a reliable way.

# No colors and wrong empirical counterpart for logistic models with exponentiate = FALSE
tab_reg(gss_simple, outcome = "married", predictors = c("race", "rincome", "relig", "age"),
        family = "binomial", empirical = TRUE, exponentiate = FALSE, 
)
# - Strange enough, when I add `exponentiate = FALSE`, all the levels colums and numbers rows, in fact all the cells, turn bold : 
#   all look mistaken with a total row or a reference row. Stanger still, the same with 
#  outcome = "tvhours" and family = "poisson" have the righ bold (only reference rows and summary stats fully in bold).




tab_reg(gss_simple, outcome = "married", predictors = c("rincome", "party3"), tab_vars = "race")
# - auto tab_spread() with a split var and just one dependent is not working for summary stats : 
#   summary stats rows are repeated three times but they are all empty. 
# - We just want a unique summary stats block below the predictors : please fix the code
#   to make the two features (tab_spread and summary tables) work with one another.




### exports and display tests  ---- 


# tab_md 
tab(gss_simple, c(race, rincome, relig), c(party3, marital), pct = "row",  na = "drop_all", 
   color = TRUE, color_signif = "grey_non_signif", ref = 1
)
tab(gss_simple, col_vars = c(race, rincome))




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


### weights and survey-design pvalues display improvement ----

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

# Display improvements : 
# - remove the "statistic" row from displayed summary stats : now that we have not 
#    just pvalue, but also effect size, the user don’t know anymore which statistic it is.
# - order of summary stats : first pvalue, then effect size
# - pvalue in-cell text waste a lot of horizontal space with "(Chi2, Kish)", "(F, Kish)" "(Chi2, Rao-Scott)", "(F, survey)", etc.
#   Since the type of pvalue, with or without Kish, with or without survey, Welch or Classic, 
#   is at the whole table level, I would want this information to be displayed 
#   in the row name only ("levels" column). So parenthesis is to be removed from the fmt cell altogether. 
#   Exemples of the display I want in the row names : 
#   - With both factors and numeric col_vars : "pvalue (Chi2, Welch F)"
#   - With only factors col_vars : "pvalue (Chi2)"
#   - With only numeric col_vars, and classic ANOVA F chosen : "pvalue (ANOVA F)"
#   - With both factors and numeric col_vars and Kish (on both) : "pvalue (Chi2, Welch F; Kish)"
#   - With both factors and numeric col_vars and survey design (Rao-Scott, etc.) : "pvalue (Chi2, Welch F; survey-design)"
#   - Say it’s Fisher when it’s fisher.
# - "effect size" is not clear either : depending on if there are only factors col_vars, only numeric, or factors and numeric both,
#   it should say which measure of the effect size is used. Ex : "Kramer’s V" (no parenthesis here, 
#   adding both an "effect size" text and a "Kramer’s V" text would be useless here), "eta2", "Kramer’s V, eta2", etc.








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

