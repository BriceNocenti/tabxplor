
# Manual reviews pass 4 ----

library(devtools)
load_all()
options(tabxplor.parallel = TRUE, tabxplor.cleannames = TRUE, tabxplor.print = "kable")


# pc18 <- readRDS("~/gss_simple/Pratiques culturelles/Pratiques culturelles 2018/pc18.rds")
# musique_vars <- c("ROCK", "JAZZ", "CLASSIQUE", "VARIETE", "ELECTRO", "METAL", "CHANSON", "WORLD", "RAP", "TRADI")
# pc18 <- pc18 |>
#   select(-any_of(c("CHANSON", "WORLD", "TRADI", "VARIETE", "RNB", "ELECTRO", "RAP", "METAL", "ROCK", "JAZZ", "OPERA", "CLASSIQUE"))) |>
#   rename(any_of(c(CHANSON   = "E1001", WORLD = "E1002", TRADI = "E1003", VARIETE   = "E1004", RNB= "E1005", ELECTRO = "E1006", 
# RAP= "E1007", METAL= "E1008", ROCK= "E1009", JAZZ= "E1010", OPERA= "E1011", CLASSIQUE = "E1012"
#   )))
# pc18$CHANSON   <- forcats::fct_recode(pc18$CHANSON,  "1-Chanson ou variété française" = "1-Chansons ou variétés françaises", "2-Non" = "2-Non")
# pc18$WORLD     <- forcats::fct_recode(pc18$WORLD,  "1-World" = "1-Musiques du monde", "2-Non" = "2-Non")
# pc18$TRADI     <- forcats::fct_recode(pc18$TRADI,  "1-Tradi" = "1-Musiques traditionnelles",  "2-Non" = "2-Non")
# pc18$VARIETE   <- forcats::fct_recode(pc18$VARIETE, "1-Variété inter- nationale" = "1-Variétés internationales", "2-Non" = "2-Non")
# pc18$ELECTRO   <- forcats::fct_recode(pc18$ELECTRO, "1-Électro, techno" = "1-Musiques électroniques, techno", "2-Non"   = "2-Non")
# pc18$RAP       <- forcats::fct_recode(pc18$RAP, "1-Rap" = "1-Hip hop, rap", "2-Non"          = "2-Non")
# pc18$METAL     <- forcats::fct_recode(pc18$METAL,"1-Metal, hard rock" = "1-Metal, hard rock","2-Non"= "2-Non")
# pc18$ROCK      <- forcats::fct_recode(pc18$ROCK, "1-Pop, rock" = "1-Pop, rock", "2-Non"       = "2-Non")
# pc18$JAZZ      <- forcats::fct_recode(pc18$JAZZ, "1-Jazz" = "1-Jazz", "2-Non"  = "2-Non")
# pc18$CLASSIQUE <- forcats::fct_recode(pc18$CLASSIQUE, "1-Classique" = "1-Musique classique", "2-Non" = "2-Non")
# pc18 <- pc18 |> dplyr::select(-any_of(c("NB_MUSIQUE"))) |> score_from_lv1(name = "NB_MUSIQUES", vars_list = musique_vars)

# pc18_young <- pc18 |> dplyr::filter(AGE >= 18 & AGE < 25) #  593 individuals


# # # color = c("diff", "ratio"), color_signif = "ignore"
# # # set_color_breaks(breaks = list(pct_ratio = c(1.2, 1.5, 2, 4), mean_diff = NULL ) )
# # set_color_breaks(pct_ratio = list(over = c(NA, 1.5, 2, 4), under = c(NA, 1.5, 2, 4)))

# #   exploratory tables with several row_vars and several col_vars (nb_cine, NB_CONCERTS = numeric variable)
# rows1 <- c("DIPLOM", "CRITREVENU", "CRITAGE", "CSTOTR")
# cols1 <- c("CONCERTS", "TELE", "JV", "nb_cine", "NB_CONCERTS")

# #   `levels = "first"`
# rows2 <- c("SEXE", "DIPLOM", "CRITREVENU", "CRITAGE")
# cols2 <- c(musique_vars, "NB_MUSIQUES") # plus the related score as numeric variable



### md tests and others ---- 


# # How to handle significance stars in tab_md ? 
# tab(pc18, all_of(rows2), c(ROCK, JAZZ, CLASSIQUE), wt = POND, pct = "row",  na = "drop", 
#    color = TRUE, color_signif = "grey_non_signif", ref = 1,  # stars = TRUE
# ) |> 
#   tab_export("md", css = TRUE)
# # - Strangely enough for me, significance stars actually renders well with the md (quarto/pandoc). 
# # - The rendered html of course does not respect the padding/alignement, but I wonder if there would 
# #   be a trick for it too work here too ? If instead of normal spaces, we pass the right kind of  
# #   special space to match the * widths, would the alignment be kept after pandoc html render ?e
# #  Same thing for the "100% (n= 673)" like column in md, of course (would the 1 digit space work here ?)
# # - By the way : with color_sign = "grey_non_signif", the tooltip is actually statistically false. 
# #  It writes : "Grey: not significantly different from the Total row (Newcombe score interval, 95% confidence).", 
# #   but that is only true with "guaranteed_effect" ; since, with "grey_non_signif", significative 
# #   differences with small effects are greyed out (and some have stars !), we must change this legend. Here, 
# #   the only thing we’re sure about, it that all colored cells are significantly different from ref
# #   (but not all significant cells are colored). Explain it in a clear but straightforward way.






# # tab(pc18, all_of(rows1), CONCERTS, wt = POND, pct = "row", color = TRUE,
# #  color_signif = "grey_non_signif", na = "drop", ci="diff", stars = TRUE
# # ) |> 
# #   tab_xl(open = FALSE, path = "~/github/tabxplor/dev/review_manual/Excel_stars", replace = TRUE)

# # tab_reg(pc18, "ROCK", rows1, wt = "POND", family = "binomial") |> 
# #   tab_xl(open = FALSE, path = "~/github/tabxplor/dev/review_manual/Excel_stars_OR", replace = TRUE)









### tab_reg tests ----

# c("year", "marital", "age", "race", "rincome", "partyid", "relig", 
# "denom", "tvhours")



# library(devtools) ; load_all() ; options(tabxplor.parallel = TRUE, tabxplor.cleannames = TRUE)
# options(tabxplor.print = "kable")

gss_simple <- gss_cat_data_formatting() # gss_simple with merged levels, and first levels chosen for reference (colors, regressions)


# No colors and wrong empirical counterpart for logistic models with exponentiate = FALSE
tab_reg(gss_simple, dependent = "married", predictors = c("race", "rincome", "relig", "age"),
        family = "binomial", empirical = TRUE, exponentiate = FALSE, 
)
# - With `exponentiate = FALSE`, the base coefficient of a logistic model is all greyed out, every with *** significance. 
#   Legend says : "Model β: β (ref.): -0.8 -0.5 -0.2 +0.2 +0.5 +0.8 [grey: non-significant or under ±0.2 SD]" 
#   Is number of sd the right call here, or should the model pass a custom diff scale here ? 
#   To match the OR scale without using breaks with many decimals, here the diff scale could be : 
#     > log(c(1/4, 1/2, 1/1.5, 1/1.2, 1.2, 1.5, 2, 4)) |> round(1) |> unique()
#     [1] -1.4 -0.7 -0.4 -0.2  0.2  0.4  0.7  1.4
# - With `empirical = TRUE`, the empirical quantity do not match the base model coefficient :
#   keep `Emp. %`, log() the OR and use the diff vctrs field for that, use rename the column `Emp. log(OR)`, 
#    same color scale than raw coefficient.
#   What else would need to be done for it to work consistently, in a statistically sound way ? Log the confidence interval ?


tab_reg(gss_simple, dependent = "tvhours", predictors = c("race", "rincome", "relig", "age"),
        family = "poisson", empirical = TRUE, exponentiate = FALSE
)
# - same two problems than with binomial + `exponentiate = FALSE` here : no colors + wrong empirical counterpart.
#    With empirical = TRUE, keep `Emp. rate`, log() the IRR and use the diff vctrs field for that, 
#    use rename the column `Emp. log(IRR)`, same color scale than raw coefficient.

tab_reg(gss_simple, dependent = "party3", predictors = c("race", "rincome", "relig", "tvhours"),
        empirical = TRUE, exponentiate = FALSE
)
# - same here, and same for ordinal regression I think. 



tab_reg(gss_simple, dependent = "married", predictors = c("rincome", "party3"), split_var = "race",
        empirical = TRUE, #exponentiate = FALSE
)
# - New feature : with a `split_var`, when there is only one dependent var, and only one predictors list, 
#  and never for a multinomial model (where there are several columns for just one model), auto `tab_spread()` 
#  so that the results of the different submodels can be compared side-by-side. 
#  It must work with `empirical = TRUE`, and the `col_var` attribute should include both dependant var name 
#   and split var level, so that there are borders between the different models and it’s clear to the user 
#  that they are different models. The levels of the `split_var` must appear clearly : for example, 
#  not spread mode prints "married: Married", and after tab_spread() it must prints something like "White\nmarried: Married"
#  with an internal like break / wrapped text to that it reads as two rows in the same cell in html or Excel
#  (if difficult in console, it’s ok ; but console will have to prefix or suffix the split var level name anyway 
#   to avoid column names clashes)
# - Rename all "Emp. " to "Obs_" in `Emp. mean` `Emp. diff` `Emp. OR` etc. : `Obs_mean` `Obs_diff` `Obs_OR`, etc. 
#    is more standard / more clear for the user.




# # odds ratio of each outcome category versus the rest at reference population work
# # (but in fact, like predicted, it’s still more difficult to read than AME at average)
# tab_reg(gss_simple, dependent = "marital", predictors = c("race", "rincome"), family = "multinomial", 
#   effect = "coefficient", at = "reference"
# )







# Tests ----

# # HTML engine — a font-family stack (install the candidate locally first, or it falls back):
# options(tabxplor.tab_kable_num_font = "DejaVu Sans") 
# options(tabxplor.tab_kable_num_font = "DejaVu Sans Mono") # biggest/widest
# options(tabxplor.tab_kable_num_font = '"IBM Plex Mono", monospace')
# options(tabxplor.tab_kable_num_font = "Cascadia Mono")  # big/wide
# options(tabxplor.tab_kable_num_font = "Cascadia Code") 
# options(tabxplor.tab_kable_num_font = "Consolas")  # too small

# options(tabxplor.tab_kable_num_font = "Liberation Mono") # better than DejaVu Sans Mono
# # options(tabxplor.tab_kable_num_font = "JSF Mono") # 
# # options(tabxplor.tab_kable_num_font = "Segoe UI Mono") # 
# # options(tabxplor.tab_kable_num_font = "Menlo") # 
# # ui-monospace, "SF Mono", "Cascadia Mono", "Segoe UI Mono", Menlo, Consolas, "Liberation Mono", "DejaVu Sans Mono", monospace

# options(tabxplor.tab_kable_num_font = "Cascadia Mono") # big/wide
# bigger_numbers <- '<style>.tabxplor-tab td.tx-num{font-size:1.1em;line-height:1;}</style>'
# kab <- tab(pc18, all_of(rows1), all_of(cols1), wt = POND, pct = "row", color = TRUE, na = "drop") |> tab_export() 
# kab
# paste0(as.character(kab), bigger_numbers) |> vctrs::vec_restore(kab)
# tab(pc18, all_of(rows1), all_of(cols1), wt = POND, pct = "row", color = TRUE, na = "drop") |> tab_export(theme="auto") 


# options(tabxplor.tab_kable_num_font = "DejaVu Sans") 
# tab(pc18, all_of(rows1), all_of(cols1), wt = POND, pct = "row", color = TRUE, na = "drop") 


# score_risques_phy_logits |> tab_export()
# tab(gss_simple, marital, race, pct = "row", color = "diff") |> tab_export() 


# tab(gss_simple, marital, race, pct = "row", color = "diff") |>
#   tab_export()   # then open it

# # Excel (single installed name) and plot (device family):
# options(tabxplor.xl_font_num  = "Cascadia Mono")
# options(tabxplor.plot_num_font = "JetBrains Mono")



# options(tabxplor.tab_kable_num_font = 'ui-monospace, "Cascadia Mono", Menlo, Consolas, "DejaVu Sans Mono", monospace')





