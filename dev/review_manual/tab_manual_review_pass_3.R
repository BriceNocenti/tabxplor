
# Manual reviews pass 3 ----

library(devtools)
load_all()
options(tabxplor.parallel = TRUE, tabxplor.cleannames = TRUE, tabxplor.print = "kable")

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


# color = c("diff", "ratio"), color_signif = "ignore"
# set_color_breaks(breaks = list(pct_ratio = c(1.2, 1.5, 2, 4), mean_diff = NULL ) )
set_color_breaks(pct_ratio = list(over = c(NA, 1.5, 2, 4), under = c(NA, 1.5, 2, 4)))

#   exploratory tables with several row_vars and several col_vars (nb_cine, NB_CONCERTS = numeric variable)
rows1 <- c("DIPLOM", "CRITREVENU", "CRITAGE", "CSTOTR")
cols1 <- c("CONCERTS", "TELE", "JV", "nb_cine", "NB_CONCERTS")

#   `levels = "first"`
rows2 <- c("SEXE", "DIPLOM", "CRITREVENU", "CRITAGE")
cols2 <- c(musique_vars, "NB_MUSIQUES") # plus the related score as numeric variable



### kable and md tests ---- 


# tab(pc18, all_of(rows1), nb_cine, wt = POND, pct = "col", color = TRUE, na = "drop") |> 
#   tab_kable()

# tab(pc18, all_of(rows1), all_of(cols1), wt = POND, pct = "col", color = TRUE, na = "drop") |> 
#   tab_kable(theme = "auto")



# tab(pc18, all_of(rows2), c(ROCK, JAZZ, CLASSIQUE), wt = POND, pct = "row", color = TRUE, na = "drop", 
#     levels = "first", ref = 1
# ) |> 
#   tab_export("md", css = TRUE)









### tab_reg tests ----

# c("year", "marital", "age", "race", "rincome", "partyid", "relig", 
# "denom", "tvhours")

options(tabxplor.parallel = TRUE, tabxplor.cleannames = TRUE, tabxplor.print = "kable")

data <- forcats::gss_cat |>
dplyr::mutate(married = factor(dplyr::if_else(marital == "Married",
"01-Married",
"02-Not married")
))

# logistic (odds ratios):
tab_reg(data, dependent = "married", predictors = c("race", "rincome"), family = "binomial")     # genuinely occupy all horizontal space, too much blank            # occupy half the space, compact, good 
# - Summary stats are difficult to read because they are greyed out : 
#    a vctrs fields trick can certainly resolve this, like considering them as a total rows 


tab_reg(data, dependent = "married", predictors = c("race", "rincome"), family = "binomial")     # genuinely occupy all horizontal space, too much blank
tab(data, c("race", "rincome"), married, pct = "row", color = TRUE, na = "drop")                 # occupy all horizontal space but a tiny porton, too much blank
tab(pc18, all_of(rows1), TELE, wt = POND, pct = "col", color = TRUE, na = "drop", levels="first")# genuinely occupy all horizontal space, too much blank
tab(pc18, all_of(rows1), nb_cine, wt = POND, pct = "col", color = TRUE, na = "drop")   
# - For a reason unknown, standard tab() with few columns sometimes do not occupy all horizontal space, 
#    in html in Positron Viewer pane, sometimes don’t, which is inconsistent and bad looking (far too much blank); 
#    tab_reg() does the same : here very few column and numbers, yet all the horizontal space taken. 
#    Where does it come from ? To fix in all exports, it’s not only tab_reg.



# linear (betas):
tab_reg(data, dependent = "tvhours", predictors = c("race", "age"), family = "gaussian")


# formula escape-hatch (same model, terser):
tab_reg(data, married ~ race + rincome, family = "binomial")


# average marginal effects + adjusted predictions (needs the marginaleffects package):

tab_reg(data, dependent = "married", predictors = c("race", "rincome"), family = "binomial", effect = "ame")
# marginal effects at the reference profile (others at their reference level / mean):
tab_reg(data, dependent = "married", predictors = c("race", "rincome"), family = "binomial", 
  effect = "ame", at = "reference")

# multinomial (nominal 3+ level): one OR column per outcome category vs the reference
tab_reg(forcats::gss_cat, dependent = "partyid", predictors = c("race", "age"), family = "multinomial", 
  reference = c(partyid = "Independent"))

# ordinal (proportional-odds): one cumulative-OR column
income3 <- forcats::gss_cat |> dplyr::mutate(income = factor(rincome, ordered = TRUE))
tab_reg(income3, dependent = "income", predictors = "race", family = "ordinal")







#### comparaison with results of former tab_logit functions ----

##### startup
ct13_reg <- readRDS("dev/review_manual/ct13_reg.rgs")

vars_sociodemo  <- c("SEXE", "AGE4", "DIPLOME4")
vars_metier     <- c("PPP1ex", "FAPPPreg", "ENCADR", "PUBLIC")
vars_emp        <- c("EMP4reg", "NBSALA2")

# Modèles logits sur scores RPS (2013)
scores_RPS_predictors2 <- list(
  "employeur" = "EMP4reg",
  "orga"      = "cah_ORGA",
  "sociodemo" = vars_sociodemo,
  "metier"    = c("PPP1", "ENCADR"), #"FAPPPreg" "PUBLIC", 
  "sauf_orga" = c(vars_sociodemo, c("PPP1", "ENCADR"), vars_emp),
  #"sans sd"   = c("PPP1", "ENCADR", vars_emp, "cah_ORGA"),
  "complet"   = c(vars_sociodemo, c("PPP1", "ENCADR"), vars_emp, "cah_ORGA")
)

vars_scores_RPS13_final <- 
  c("score_risques_phy", #"score_horaires13", 
    #"score_insecurite13", 
    #"score_relations_collegues13", 
    "score_relations_chef13", "score_orga_hostile", 
    "score_emopub13", "score_charge_mentale", "score_contrad13", 
    #"score_non_reconnaissance",
    "score_conflits_ethiques13", "score_moyens" #, "score_ethique_moyens13" 
  )

nb_questions <- vars_scores_RPS13_final |>
  stringr::str_replace("score_", "vars_") |> 
  rlang::syms() |> purrr::map_int(~rlang::eval_tidy(.) |> length())

# # Scores finaux, predicteurs finaux
# scores_RPS_logits <- 
#   multi_logit(ct13_reg, vars_scores_RPS13_final,
#               predictors_sequence = scores_RPS_predictors2, marginal_effects = FALSE,
#               wt = "pondqaa", 
#               nb_questions = vars_scores_RPS13_final |>
#                 str_replace("score_", "vars_") |> 
#                 syms() |> map_int(~eval_tidy(.) |> length())
#   )


##### models 
scores_RPS_logits <- 
tab_reg(
  ct13_reg, 
  dependent = vars_scores_RPS13_final, predictors = scores_RPS_predictors2, wt = "pondqaa", 
  family = "binomial", trials = nb_questions, empirical_OR = TRUE, compare = "sequential", 
  cleannames = FALSE
)
# "Error in `tab_reg()` at dev/review_manual/tab_manual_review_pass_3.R:169:1:
# ! With a list of models in `predictors`, `dependent` must be a single name.
# ℹ A vector of dependents is for the one-model-per-outcome mode."
# - It should work with several dependent vars + predictors list (like in the former out-of-package multi_logit ; 
#    trails must then accept a vector)



score_risques_phy_logits <- 
tab_reg(
  ct13_reg, 
  dependent = vars_scores_RPS13_final[[1]], predictors = scores_RPS_predictors2, wt = "pondqaa", 
  family = "binomial", trials = nb_questions[[1]], empirical_OR = TRUE, compare = "sequential", 
  cleannames = FALSE
)

score_risques_phy