
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



### md tests and others ---- 


# How to handle significance stars in tab_md ? 
tab(pc18, all_of(rows2), c(ROCK, JAZZ, CLASSIQUE), wt = POND, pct = "row",  na = "drop", 
   color = TRUE, color_signif = "grey_non_signif", ref = 1,  # stars = TRUE
) |> 
  tab_export("md", css = TRUE)
# - Strangely enough for me, significance stars actually renders well with the md (quarto/pandoc). 
# - The rendered html of course does not respect the padding/alignement, but I wonder if there would 
#   be a trick for it too work here too ? If instead of normal spaces, we pass the right kind of  
#   special space to match the * widths, would the alignment be kept after pandoc html render ?e
#  Same thing for the "100% (n= 673)" like column in md, of course (would the 1 digit space work here ?)
# - By the way : with color_sign = "grey_non_signif", the tooltip is actually statistically false. 
#  It writes : "Grey: not significantly different from the Total row (Newcombe score interval, 95% confidence).", 
#   but that is only true with "guaranteed_effect" ; since, with "grey_non_signif", significative 
#   differences with small effects are greyed out (and some have stars !), we must change this legend. Here, 
#   the only thing we’re sure about, it that all colored cells are significantly different from ref
#   (but not all significant cells are colored). Explain it in a clear but straightforward way.



# This important exported helper is not working anomyre with `freq = TRUE`` 
#  (it’s ok with `freq = FALSE`) : try to fix it, and add testthat tests for it since it’s exported.
fct_recode_helper(forcats::gss_cat, all_of("rincome"))
# Error in `purrr::map()` at tabxplor/R/utils.R:284:5:
# ℹ In index: 1.
# Caused by error in `.f()`:
# ! object 'pct' not found




data |> tab(relig)
# The simplest table is broken, the n column from version <= 1.3.1 dissappeared ! 
data |> tab(relig, pct ="col")
# same here : no "n" column like in 1.3.1 and before (only "pct")
# when col_var column attribute is not a real name (here : "no_col_var", sometimes the special name for Total column), 
#  it should never be displayed as a column variable name (it’s noise for the user)
#
# test these behaviours, in the current state they would badly break past code from version <= 1.3.1 




### tab_reg tests ----

# c("year", "marital", "age", "race", "rincome", "partyid", "relig", 
# "denom", "tvhours")

options(tabxplor.parallel = TRUE, tabxplor.cleannames = TRUE, tabxplor.print = "kable")

data <- forcats::gss_cat |>
dplyr::mutate(
  married = factor(dplyr::if_else(marital == "Married",
  "01-Married",
  "02-Not married")
),
black = factor(dplyr::if_else(race == "Black",
  "01-Black",
  "02-Not black")
),
race = forcats::fct_relevel(race, "White", "Black", "Other"), 
marital = forcats::fct_relevel(marital, "Married", "Separated", "Divorced", "Widowed", "Never married", "No answer"),

across(where(is.factor), ~ forcats::fct_recode(., "NULL" = "No answer", "NULL" = "Refused", "NULL" = "Don't know", "NULL" = "Not applicable")),

rincome = forcats::fct_recode(   # "new" = "old" 
  rincome,
  "1-Lt $10000"      = "Lt $1000"      ,
  "1-Lt $10000"      = "$1000 to 2999" ,
  "1-Lt $10000"      = "$3000 to 3999" ,
  "1-Lt $10000"      = "$4000 to 4999" ,
  "1-Lt $10000"      = "$5000 to 5999" ,
  "1-Lt $10000"      = "$6000 to 6999" ,
  "1-Lt $10000"      = "$7000 to 7999" ,
  "1-Lt $10000"      = "$8000 to 9999" ,
  "2-10000 to 14999" = "$10000 - 14999",
  "3-15000 to 24999" = "$15000 - 19999",
  "4-15000 to 24999" = "$20000 - 24999",
  "5-25000 or more"  = "$25000 or more"
) |> 
forcats::fct_relevel(sort) |>
as.ordered(),
)



# logistic (odds ratios):
tab_reg(data, dependent = "married", predictors = c("race", "rincome"), family = "binomial", 
        empirical_OR = TRUE
)  
# - Summary stats are difficult to read because they are greyed out : 
#    a vctrs fields trick can certainly resolve this, like considering them as a total rows (study this carefully) ? 
# - For the reference rows, the reference "Emp. %" appear in grey (should be black : reference row)
# - The "n" in the tooltip is not good, it gives the N of the whole model (already in summary statistics),
#    but what is useful there is the n for the row level (put it when it exists, for ex with `empirical = TRUE` ; NA otherwise).

tab_reg(data, married ~ race + rincome, family = "binomial", effect = "ame", cleannames = FALSE) 
# - some AME do not appear and may be missing value : but among them, with OR.
#  "$20000 - 24999" is two stars significant and "$15000 - 19999" is one star significant ("$10000 - 14999" 
#  is missing too and is not significant), so these NA values are strange ! Please enquire. 
#  I checked  if it was a "cleanames = TRUE" option problem handling "-", since the three missing rows have "-" in their name,
#    but it’s not.
# - even with effect = "ame", I still want the OR in the tooltips. Any data already computed in vctrs fields
#   that can really help the user interpret and understand the model is a good candidate for tooltips.


tab_reg(data, dependent = "married", predictors = c("race", "rincome"), family = "binomial", 
        effect = "ame", empirical_OR = TRUE
) 
# "Error in `tab_reg()` at dev/review_manual/tab_manual_review_pass_3.R:95:1:
# ! `empirical_OR` is only available for a single binary logistic outcome (coefficient effect).
# ℹ It shows the descriptive crude odds ratio / percentage beside the model odds ratio."
# - Replace `empirical_OR` argument with `empirical` (no soft-deprecated : new function in 1.4.0) ;
#   for  effect = "ame", which prints both modelised difference from reference level (AME ; MER too) 
#   and modelised/adjusted percentage (predicted probability). 
#   What would be the right empirical comparison, base percentage + empirical diff ? 
# - For all types of models, **rule should be** : `empirical=TRUE` prints the empirical computation 
#   that is adapted for the comparison (which is also the modelised quantity with only one predictor). 
#   What is it for a lm() regression, just the mean per level of the predictor and difference of mean from reference ?
#   Make web searches, ensure the framework to compare "modelised" versus "empirical" is statistically sound, good practice, standard.


tab_reg(data, dependent = "marital", predictors = c("race", "rincome"), family = "multinomial")
# - With multinomial, where a same model have different columns, set all the colums to the same col_var, 
#   so that horizontal borders between the columns are automatically removed.

tab_reg(data, dependent = "marital", predictors = c("race", "rincome"), family = "multinomial", 
  effect = "ame", at = "average"
)
# here, adding many column for `empirical=TRUE` would create too much columns. But there is a way : 
#  would there be a trick to print the empirical pct and the empirical diff from ref, for the user to
#  seem if they are close or far from the modelised ones (problem : the vctrs fields only have 
#  1 pct and 1 diff fields ! Is there a hack, that would not mess with tab() tooltips or 
#  other tab_reg() tooltips ) ?


# # odds ratio of each outcome category versus the rest at reference population work
# # (but in fact, like predicted, it’s still more difficult to read than AME at average)
# tab_reg(data, dependent = "marital", predictors = c("race", "rincome"), family = "multinomial", 
#   effect = "coefficient", at = "reference"
# )

# ordinal (proportional-odds): one cumulative-OR column
tab_reg(data, dependent = "rincome", predictors = c("marital", "race"), family = "ordinal")
# - Add Brant omnibus pvalue in summary statistics for "ordinal" ?


# linear (betas):
tab_reg(data, dependent = "tvhours", predictors = c("race", "age"), family = "gaussian")
# - here too, reference it greyed out, but must be black (0)
# - Also here, for "race" predictor, I have two cells with "***" but that are greyed out (Black 1/1.30, Other 1/1.44), 
#    I don’t understand, please enquire and explain it to me.





#### comparaison with results of former tab_logit functions ----

# startup
ct13_reg <- readRDS("dev/review_manual/ct13_reg.rds") # no to use in testthat, confidential data !

vars_sociodemo  <- c("SEXE", "AGE4", "DIPLOME4")
vars_metier     <- c("PPP1ex", "FAPPPreg", "ENCADR", "PUBLIC")
vars_emp        <- c("EMP4reg", "NBSALA2")

# Modèles logits sur scores RPS (2013)
scores_RPS_predictors2 <- list(
  "employeur" = "EMP4reg",
  "orga"      = "cah_ORGA",
  "sociodemo" = vars_sociodemo,
  "metier"    = c("PPP1", "ENCADR"), 
  "sauf_orga" = c(vars_sociodemo, c("PPP1", "ENCADR"), vars_emp),
  "complet"   = c(vars_sociodemo, c("PPP1", "ENCADR"), vars_emp, "cah_ORGA")
)

vars_scores_RPS13_final <- 
  c("score_risques_phy", 
    "score_relations_chef13", "score_orga_hostile", 
    "score_emopub13", "score_charge_mentale", "score_contrad13", 
    "score_conflits_ethiques13", "score_moyens"
  )

nb_questions <- c(9, 6, 12, 6, 4, 4, 5, 7)

##### logit models 
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
#    trails must then accept a vector), but output a list of tabxplor_tab, 
#    so that with tab_export("xl") the user gets a workbook with one sheet per dependent variable.


score_risques_phy_logits <- 
tab_reg(
  ct13_reg, 
  dependent = vars_scores_RPS13_final[[1]], predictors = scores_RPS_predictors2, wt = "pondqaa", 
  family = "binomial", trials = nb_questions[[1]], empirical_OR = TRUE, compare = "baseline", 
  baseline = "complet",
  cleannames = FALSE
)
score_risques_phy_logits |> tab_export(theme="light") # theme="auto"
score_risques_phy_logits |> tab_export() # theme="auto"

# Very good. Few improvements : 
# - In row variables, the order of the variables in not totally what I would expect, but it’s a bit tricky :
#   I just want that, when a model is provided that contains all the predictors in the other models,
#   (what we can call the "complete" model) the order of predictors of this model is kept at the end ; 
#   if there’s not complete model, just keep the current behaviour.
# - When I pass `compare = "baseline", baseline = "complet"`, I have warnings "models are not nested or N differs -> showing the AIC difference vs the baseline model instead of a
#    likelihood-ratio test". What is happening ? 
#    1. Is to because the code only test if current model is
#     a subset of baseline (here it’s the opposite : "complet" is to be tested against all other models as baseline ; 
#     so maybe also test, not only if baseline is a subset of current model, but if the current model is a subset of baseline too ?)
#    2. It’s because N is not exactly the same in all tables due to NA handling ? In this case, add an opt-in
#     argument `na="drop_all"` (like in tab() ) to fit all models on the same population (remove any individual with NA to any predictor or NA to the current dependent var)
# - The name of the model appear two type, first as tabxplor col_var name, then as normal header : how
#    to resolve that without breaking tab() exports and in a simple way ? Simple rule saying that if
#    the name of a fmt column is the same than it’s col_var, for all fmt columns, silently drop the variable name additional title row ?
# - There as still padding/alignment inconsistencies. Is it because of the width of the "*" symbol in DejaVu Sans ? 
#   Is there a way to fix that, putting a kind of space of exactly the right midth in DejaVu Sans to pad the stars ? 
#   Example with small misalignement in custom html (both grey color ad plain font weight) :
#           "1.13** 	"
#           "1.02   	"
# - Would there be a reliable way to not pad the summary statistics for stars (just right align) ? 
#   A last trick would be to use DejaVu Sans Mono for all numbers/fmt cells, 
#    with fallback to other monospace fonts, anytime there are stars (but it you have a DejaVu Sans solution I would like it).
# - Remove tooltips from summary statistics, otherwise some show inconsistent "diff", etc. 
#    For example, AIC "63 785" with diff tooltip "+6378526%". 





# Pass 4 ----

data |> 
  mutate(rincome = `class<-`(rincome, "factor")) |> # temporary : remove ordered to not break model
  tab_reg(dependent = c("married", "black"), predictors = c("rincome", "tvhours", "relig"), family = "binomial", 
          # effect = "ame", empirical_OR = TRUE
  ) 

data |> 
mutate(rincome = `class<-`(rincome, "factor")) |> # temporary : remove ordered to not break model
tab_reg(dependent = c(married, black), predictors = all_of(c("rincome", "tvhours", "relig")), family = "binomial",     
) 




# tab(pc18, all_of(rows1), CONCERTS, wt = POND, pct = "row", color = TRUE,
#  color_signif = "grey_non_signif", na = "drop", ci="diff", stars = TRUE
# ) |> 
#   tab_xl(open = FALSE, path = "~/github/tabxplor/dev/review_manual/Excel_stars", replace = TRUE)

# tab_reg(pc18, "ROCK", rows1, wt = "POND", family = "binomial") |> 
#   tab_xl(open = FALSE, path = "~/github/tabxplor/dev/review_manual/Excel_stars_OR", replace = TRUE)


# HTML engine — a font-family stack (install the candidate locally first, or it falls back):
options(tabxplor.tab_kable_num_font = "DejaVu Sans") 
options(tabxplor.tab_kable_num_font = "DejaVu Sans Mono") # biggest/widest
options(tabxplor.tab_kable_num_font = '"IBM Plex Mono", monospace')
options(tabxplor.tab_kable_num_font = "Cascadia Mono")  # big/wide
options(tabxplor.tab_kable_num_font = "Cascadia Code") 
options(tabxplor.tab_kable_num_font = "Consolas")  # too small

options(tabxplor.tab_kable_num_font = "Liberation Mono") # better than DejaVu Sans Mono
# options(tabxplor.tab_kable_num_font = "JSF Mono") # 
# options(tabxplor.tab_kable_num_font = "Segoe UI Mono") # 
# options(tabxplor.tab_kable_num_font = "Menlo") # 
# ui-monospace, "SF Mono", "Cascadia Mono", "Segoe UI Mono", Menlo, Consolas, "Liberation Mono", "DejaVu Sans Mono", monospace

options(tabxplor.tab_kable_num_font = "Cascadia Mono") # big/wide
bigger_numbers <- '<style>.tabxplor-tab td.tx-num{font-size:1.1em;line-height:1;}</style>'
kab <- tab(pc18, all_of(rows1), all_of(cols1), wt = POND, pct = "row", color = TRUE, na = "drop") |> tab_export() 
kab
paste0(as.character(kab), bigger_numbers) |> vctrs::vec_restore(kab)
tab(pc18, all_of(rows1), all_of(cols1), wt = POND, pct = "row", color = TRUE, na = "drop") |> tab_export(theme="auto") 


options(tabxplor.tab_kable_num_font = "DejaVu Sans") 
tab(pc18, all_of(rows1), all_of(cols1), wt = POND, pct = "row", color = TRUE, na = "drop") 


score_risques_phy_logits |> tab_export()
tab(forcats::gss_cat, marital, race, pct = "row", color = "diff") |> tab_export() 


tab(forcats::gss_cat, marital, race, pct = "row", color = "diff") |>
  tab_export()   # then open it

# Excel (single installed name) and plot (device family):
options(tabxplor.xl_font_num  = "Cascadia Mono")
options(tabxplor.plot_num_font = "JetBrains Mono")



options(tabxplor.tab_kable_num_font = 'ui-monospace, "Cascadia Mono", Menlo, Consolas, "DejaVu Sans Mono", monospace')





