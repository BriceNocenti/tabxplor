
# Manual reviews pass 4 ----

library(devtools)
load_all()
options(tabxplor.parallel = TRUE, tabxplor.cleannames = TRUE, tabxplor.print = "kable")

### weights and design tests ----

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
    strata = 
) 
# - remove the "statistic" row from displayed summary stats : now that we have not 
#    just pvalue, but also effect size, the user don’t know anymore which statistic it is.
# - order of summary stats : first pvalue, then effect size
# - pvalue in-cells text 



# Minimal survey design (ZAE clusters, chosen randowly)
pc18$ZAE <- substr(as.character(pc18$IDENT18), 1, 6)   # the 340 ZAE = the PSU

tab(pc18, c(SEXE, CRITREVENU), c(CLASSIQUE, METAL, WORLD, RAP, TRADI, NB_MUSIQUES),
    wt = POND, pct = "row", levels = "first",
    na = "drop_all", color = TRUE, color_signif = "grey_non_signif",
    test = "survey",          # <- design-based Rao-Scott F / svyglm-Wald F
    ids  = ~ZAE               # <- the ZAE cluster; strings or formulas, NOT bare names
)



# full survey design : calibration on calage sur marges
#  strata = declares how the sample was drawn: the population was partitioned into the 44 {ancienne 
#     région × commune type} cells, and PSUs (ZAE) were sampled independently within each cell. 
#     Declaring it lets the estimator subtract the between-strata variation it knows isn't there.
#  Sex, age, diploma, CSP were not sampling strata — the sample was not drawn independently 
#     within each sex×age×diploma cell. Passing them to strata = would tell survey a falsehood 
#     about the design and give you wrong (spuriously small) standard errors.
# "~/Data/Pratiques culturelles/Pratiques culturelles 2018/Doc/Doc_Producteur/Tableau calage sur marges.xlsx"
library(survey)
pc18$ZAE     <- substr(as.character(pc18$IDENT18), 1, 6)
EFFPOP       <- 52007885

# recode the calibration variables to the EEC margin categories (doc table)
# REFAIRE AVEC EXACTEMENT LES BONNES CATEGORIES (ATTENTION AUX NA : PVALUES NA)
pc18$sexf    <- factor(pc18$SEXE)
pc18$ageband <- cut(pc18$AGE, c(-Inf,29,39,49,59,69,79,Inf), labels = paste0("a",1:7))
pc18$dip4    <- forcats::fct_collapse(pc18$DIPLOM, d1="1-Brevet ou -", d2="2-CAP BEP",
                  d3=c("3-Bac pro","4-Bac","5-Bac+2"), d4=c("6-Licence","7-Bac+5"))

calvars <- c("sexf","ageband","dip4")                     # <- every margin variable
pc18    <- pc18[stats::complete.cases(pc18[calvars]), ]

pop <- c(`(Intercept)` = EFFPOP,
         setNames(EFFPOP*52.1/100,                        "sexf2-Femme"),
         setNames(EFFPOP*c(15.3,16.1,16.3,14.9,9.7,6.6)/100, paste0("agebanda",2:7)),
         setNames(EFFPOP*c(30.6,30.1,17.8)/100,           paste0("dip4d",2:4)))

des <- svydesign(ids = ~ZAE, weights = ~POND, data = pc18)
des <- calibrate(des, ~sexf+ageband+dip4, population = pop, calfun = "raking")

tab(des, c(SEXE, CRITREVENU), c(CLASSIQUE, METAL, WORLD, RAP, TRADI, NB_MUSIQUES),
    pct = "row", levels = "first", na = "drop_all",
    color = TRUE, color_signif = "grey_non_signif", test = TRUE) 



