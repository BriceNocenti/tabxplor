# Tableaux de régression avec tab_reg()

``` r
library(tabxplor)
library(dplyr)
#> 
#> Attaching package: 'dplyr'
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union

# Comme dans la vignette d'introduction, les tableaux sont affiches comme la sortie coloree de la
# console convertie en HTML. options(tabxplor.lang = "fr") met les legendes et notes en francais.
options(cli.num_colors = 256)
options(tabxplor.lang = "fr")
set_color_palette(theme = "light")
```

Pour les modèles de régression les plus courants,
[`tab_reg()`](https://bricenocenti.github.io/tabxplor/reference/tab_reg.md)
construit un **tableau de régression** qui ressemble et se comporte
comme un tableau croisé `tabxplor` : une ligne par modalité de
prédicteur, des étoiles de significativité, des couleurs qui grisent les
effets non significatifs, et les mêmes exports Excel, html ou markdown.
Vous lui donnez un data frame, une **variable expliquée** (`dependent`)
et quelques **prédicteurs**, et il essaie de choisir le bon type de
modèle à partir du type de la variable expliquée. Sa particularité est
`empirical = TRUE`, qui affiche l’**effet observé / brut / empirique
juste à côté de l’effet ajusté du modèle**, pour que vous voyiez ce que
« contrôler par les autres variables » a réellement changé.

Nous utilisons une version formatée des données
[`forcats::gss_cat`](https://forcats.tidyverse.org/reference/gss_cat.html),
issues de la General Social Survey américaine :

``` r
gss_simple <- gss_cat_data_formatting()
```

## Le type de la variable expliquée choisit le modèle et la quantité observée à comparer

Vous fixez rarement `family` à la main — le plus souvent,
[`tab_reg()`](https://bricenocenti.github.io/tabxplor/reference/tab_reg.md)
le détecte :

| Variable expliquée | Modèle détecté | Mesure d’effet |
|:---|:---|:---|
| facteur à 2 modalités | binomial (logistique) | rapport de cotes (OR) |
| numérique (continue) | gaussien (linéaire) | coefficient β |
| comptage | poisson | rapport de taux d’incidence (IRR) |
| facteur non ordonné à 3 modalités ou plus | multinomial | une colonne OR par catégorie vs la référence |
| facteur ordonné à 3 modalités ou plus | ordinal (cotes proportionnelles) | OR cumulé |

Avec `empirical = TRUE`, chaque colonne de modèle est accompagnée d’une
colonne compagne brute/observée montrant l’effet *empirique, non ajusté,
à un seul prédicteur* — l’effet que vous verriez sans aucun contrôle, «
toutes choses étant inégales par ailleurs », pour ce prédicteur. -
logistique à 2 modalités (binomial) → % observés, et rapports de cotes
observés (non modélisés : calculés uniquement à partir des
pourcentages) - logistique à 3 modalités ou plus (multinomial) → les OR
observés sont affichés en infobulle sur les cases du modèle dans les
exports html - gaussien (linéaire) → moyennes de groupe et leur
différence - poisson (comptage) → taux observé et rapport de taux
observé

## Régression logistique (un facteur binaire)

Quand la variable expliquée est un facteur à deux modalités, ici «
married » contre « not married »,
[`tab_reg()`](https://bricenocenti.github.io/tabxplor/reference/tab_reg.md)
choisit une famille **binomiale** pour ajuster une régression logistique
et rapporte des **rapports de cotes** (la modalité de référence de
chaque prédicteur affiche la valeur neutre `1`). - Comme dans tout
modèle de régression, l’effet d’une modalité de prédicteur sur la
variable expliquée se lit « les autres prédicteurs choisis étant égaux
». - Les couleurs se lisent comme dans n’importe quel tableau `tabxplor`
: un rapport de cotes supérieur à 1 (bleu) signifie *plus susceptible
d’être marié* que la modalité de référence, inférieur à 1 (rouge)
signifie *moins susceptible* ; étoiles et couleurs signalent toutes deux
la significativité.

``` r
tab_reg(gss_simple, "married", c("race", "age", "rincome", "relig"))
#> ℹ "married": binary outcome detected -> `family =
#> "binomial"` (logistic).
```

``` r-output
#> | predictors          | Model fit   | married |
#> |:--------------------|:------------|--------:|
#> | race, age, rincome, | N           |  12 960 |
#> | relig               | LR vs null  |  <0.01% |
#> |                     | McFadden R2 |   0.057 |
#> |                     | AIC         |  16 960 |
#> |                     | BIC         |  17 064 |
#> 
#> # A tabxplor tab: 17 × 3
#> # Groups:         var [5]
#>    var      levels                Model_OR
#>    <fct>    <fct>                <row%-or>
#>  1 Constant Reference population 1/3.07***
#> 
#>  2 race     White                     1   
#>  3 race     Black                1/2.40***
#>  4 race     Other                  1.11   
#> 
#>  5 age      age                    1.02***
#> 
#>  6 rincome  1-Lt $10000               1   
#>  7 rincome  2-$10000 to 14999      1.14*  
#>  8 rincome  3-$15000 to 24999      1.27***
#>  9 rincome  4-$25000 or more       1.86***
#> 
#> 10 relig    1-Protestant              1   
#> 11 relig    2-Catholic           1/1.17***
#> 12 relig    3-Other christian    1/1.43***
#> 13 relig    4-Jewish             1/1.29*  
#> 14 relig    5-Buddhist/Hinduist  1/1.36*  
#> 15 relig    6-Muslim               1.05   
#> 16 relig    7-Other              1/2.04***
#> 17 relig    8-None               1/1.79***
#> # Modèle : régression logistique ; rapports de cotes (par rapport à la modalité de référence).
#> # OR (réf.) : 1/4 1/2 1/1,5 1/1,2 1,2 1,5 2 4 [gris : non significatif ou sous ×1,2]
#> # *** : chiffre significativement différent de celui de la modalité de référence (en gras), au seuil de confiance de 99 % ; ** : au seuil de 95 % ; * : au seuil de 90 % ; aucune étoile : non significatif.
```

Pour une régression logistique simple, `empirical = TRUE` ajoute les
**pourcentages bruts** et les **rapports de cotes bruts non modélisés**
pour chaque prédicteur.

``` r
tab_reg(gss_simple, "married", c("race", "age", "rincome", "relig"), empirical = TRUE)
#> ℹ "married": binary outcome detected -> `family =
#> "binomial"` (logistic).
```

``` r-output
#> | predictors          | Model fit   | married |
#> |:--------------------|:------------|--------:|
#> | race, age, rincome, | N           |  12 960 |
#> | relig               | LR vs null  |  <0.01% |
#> |                     | McFadden R2 |   0.057 |
#> |                     | AIC         |  16 960 |
#> |                     | BIC         |  17 064 |
#> 
#> # A tabxplor tab: 17 × 5
#> # Groups:         var [5]
#>    var      levels               `Obs_%`    Obs_OR  Model_OR
#>    <fct>    <fct>                 <row%> <row%-or> <row%-or>
#>  1 Constant Reference population                   1/3.07***
#> 
#>  2 race     White                 52%         1         1   
#>  3 race     Black                 31%*** 1/2.43*** 1/2.40***
#>  4 race     Other                 49%*   1/1.12*     1.11   
#> 
#>  5 age      age                                      1.02***
#> 
#>  6 rincome  1-Lt $10000           37%         1         1   
#>  7 rincome  2-$10000 to 14999     41%**    1.20**    1.14*  
#>  8 rincome  3-$15000 to 24999     43%***   1.32***   1.27***
#>  9 rincome  4-$25000 or more      55%***   2.12***   1.86***
#> 
#> 10 relig    1-Protestant          52%         1         1   
#> 11 relig    2-Catholic            52%    1/1.03    1/1.17***
#> 12 relig    3-Other christian     42%*** 1/1.53*** 1/1.43***
#> 13 relig    4-Jewish              55%      1.10    1/1.29*  
#> 14 relig    5-Buddhist/Hinduist   50%    1/1.09    1/1.36*  
#> 15 relig    6-Muslim              48%    1/1.17      1.05   
#> 16 relig    7-Other               35%*** 1/2.04*** 1/2.04***
#> 17 relig    8-None                38%*** 1/1.76*** 1/1.79***
#> # Modèle : régression logistique ; rapports de cotes (par rapport à la modalité de référence).
#> # Obs_% : différence (réf.) : -30 -20 -10 -5 +5 +10 +20 +30 [gris : non significatif ou sous ±5 points]
#> # Obs_OR, Model_OR : OR (réf.) : 1/4 1/2 1/1,5 1/1,2 1,2 1,5 2 4 [gris : non significatif ou sous ×1,2]
#> # *** : chiffre significativement différent de celui de la modalité de référence (en gras), au seuil de confiance de 99 % ; ** : au seuil de 95 % ; * : au seuil de 90 % ; aucune étoile : non significatif.
```

Les pourcentages bruts `Obs_%` sont un résumé des résultats observés de
base dont tout, dans le modèle, est dérivé : « 28 % des Américains noirs
sont mariés, contre 51 % pour les Américains blancs » - La simple
différence à la référence est colorée (28 % − 51 % = −23 %). - Les cases
qui ne diffèrent pas significativement de la référence sont grisées
(d’après un intervalle de confiance de Newcombe pour les différences de
proportions).

Les **rapports de cotes modélisés** `Model_OR` sont directement comparés
aux **rapports de cotes observés** `Obs_OR` : - Comparer les deux vous
dit quel ajustement le modèle a opéré : si l’OR **du modèle** d’un
prédicteur est bien plus proche de 1 que son OR **brut**, l’association
brute était largement expliquée par les autres prédicteurs. - Ici, «
toutes choses étant inégales par ailleurs », les Américains noirs ont
des cotes d’être mariés 2,68 fois plus faibles que les Américains
blancs. « Toutes choses égales par ailleurs » (plus précisément : à
revenu, âge et religion égaux), les Américains noirs ont encore des
cotes 2,4 fois plus faibles d’être mariés que les Américains blancs. Le
résultat tient, il n’est pas expliqué par des différences de revenu ni
de religion.

Les **rapports de cotes observés** `Obs_OR` sont les mêmes que ceux que
vous pouvez calculer à partir des seuls pourcentages dans un tableau
croisé : - Les couleurs et les étoiles de significativité utilisent un
**intervalle de confiance de Woolf pour l’OR** qui correspond à ce qui
est fait dans le modèle de régression. - La population du tableau doit
correspondre à la population en cas complets du modèle, en filtrant les
individus ayant un `NA` sur l’une des variables impliquées.

``` r
gss_simple |>
  dplyr::filter(dplyr::if_all(all_of(c("race", "age", "rincome", "relig")), ~ !is.na(.) )) |>
  tab(race, married, pct = "row", na = "drop",
    OR = "OR", color = "OR", color_signif = "grey_non_signif"
   )
```

``` r-output
#> # A tabxplor tab: 4 × 3
#>   race   `01-Married` `02-Not married`
#>   <fct>     <row%-or>        <row%-or>
#> 1 White       1 (52%)          1 (48%)
#> 2 Black        1/2.43             2.43
#> 3 Other        1/1.12             1.12
#> 4 Total        1/1.14             1.14
#> # OR (réf.) : 1/4 1/2 1/1,5 1/1,2 1,2 1,5 2 4 [gris : non significatif ou sous ×1,2]
```

### Effets marginaux moyens (AME) d’une régression logistique

Une autre façon d’interpréter une régression logistique est d’utiliser
les **effets marginaux moyens** (AME) avec `effect = "ame"` : - Au lieu
de lire directement le rapport de cotes modélisé, on peut en déduire la
différence moyenne de pourcentage de chaque modalité comparée à la
référence. - Ici `Model_AME` se lit ainsi : « En comparant des
répondants noirs et blancs semblables en revenu, âge et religion, être
noir est associé à un taux de mariage inférieur de 19,8 points, en
moyenne. »

``` r
tab_reg(gss_simple, "married", c("race", "age", "rincome", "relig"), effect = "ame", empirical = TRUE)
#> ℹ "married": binary outcome detected -> `family =
#> "binomial"` (logistic).
```

``` r-output
#> | predictors          | Model fit   | married |
#> |:--------------------|:------------|--------:|
#> | race, age, rincome, | N           |  12 960 |
#> | relig               | LR vs null  |  <0.01% |
#> |                     | McFadden R2 |   0.057 |
#> |                     | AIC         |  16 960 |
#> |                     | BIC         |  17 064 |
#> 
#> # A tabxplor tab: 17 × 5
#> # Groups:         var [5]
#>    var      levels               `Obs_%`    Obs_diff `Model_AME (adjusted %)`
#>    <fct>    <fct>                 <row%> <row%-diff>             <row%-mixed>
#>  1 Constant Reference population                                             
#> 
#>  2 race     White                 52%         +0%                     (51.3%)
#>  3 race     Black                 31%***     -21%***        -19.8%*** (31.5%)
#>  4 race     Other                 49%*        -3%*           +2.4%    (53.8%)
#> 
#>  5 age      age                                                      +0.5%***
#> 
#>  6 rincome  1-Lt $10000           37%         +0%                     (39.2%)
#>  7 rincome  2-$10000 to 14999     41%**       +4%**          +3.0%*   (42.2%)
#>  8 rincome  3-$15000 to 24999     43%***      +7%***         +5.5%*** (44.7%)
#>  9 rincome  4-$25000 or more      55%***     +18%***        +14.5%*** (53.7%)
#> 
#> 10 relig    1-Protestant          52%         +0%                     (52.9%)
#> 11 relig    2-Catholic            52%         -1%            -3.7%*** (49.3%)
#> 12 relig    3-Other christian     42%***     -11%***         -8.3%*** (44.6%)
#> 13 relig    4-Jewish              55%         +2%            -6.0%*   (46.9%)
#> 14 relig    5-Buddhist/Hinduist   50%         -2%            -7.2%*   (45.7%)
#> 15 relig    6-Muslim              48%         -4%            +1.2%    (54.1%)
#> 16 relig    7-Other               35%***     -17%***        -16.4%*** (36.5%)
#> 17 relig    8-None                38%***     -14%***        -13.5%*** (39.4%)
#> # Modèle : régression logistique ; effets marginaux sur l'échelle de probabilité (points de pourcentage) (moyenne sur l'échantillon) ; chaque case indique l'effet par rapport à la modalité de référence et, entre parenthèses, la probabilité prédite ajustée.
#> # Obs_%, Obs_diff, Model_AME (adjusted %) : différence (réf.) : -30 -20 -10 -5 +5 +10 +20 +30 [gris : non significatif ou sous ±5 points]
#> # *** : chiffre significativement différent de celui de la modalité de référence (en gras), au seuil de confiance de 99 % ; ** : au seuil de 95 % ; * : au seuil de 90 % ; aucune étoile : non significatif.
```

On peut alors utiliser `empirical = TRUE` pour comparer cette nouvelle
quantité modélisée à la **différence brute, observée, de pourcentages**
: - Dans l’échantillon (`Obs_diff`), on observe un taux de mariages
inférieur de 23 points pour les Américains noirs comparés aux Américains
blancs (`28% - 51% = -23%`). - La différence modélisée `Model_AME` est
très proche, `-19,8 %`, donc très peu de cette différence entre noirs et
blancs peut être expliquée par l’âge, le revenu ou la religion.

On peut aussi comparer le pourcentage brut `Obs_%` avec la **probabilité
ajustée par le modèle** — la valeur entre parenthèses dans la colonne
`Model_AME (adjusted %)` : - Le pourcentage ajusté, qui est un résultat
du modèle, se lit ainsi : « si tout l’échantillon gardait sa
distribution de revenu/âge/religion mais que tout le monde était noir,
on estime que 31,5 % seraient mariés, contre 51,3 % si tout le monde
était blanc ». - Les noirs se marient à 30,9 % dans cet échantillon
(observé) ; standardiser leur revenu/âge/religion au mélange de la
population ne déplace cela qu’à 31,5 % — donc presque rien de l’écart
noir-blanc ne s’explique par des différences dans ces trois variables ;
il persiste après ajustement.

On peut, à nouveau, obtenir les mêmes différences empiriques et
intervalles de confiance avec un simple tableau :

``` r
dependent  <- "married"
predictors <- c("race", "age", "rincome", "relig")
factor_predictors <- c("race", "rincome", "relig")
gss_simple |>
  dplyr::filter(dplyr::if_all(all_of(c(predictors, dependent)), ~ !is.na(.) )) |>
  tab(all_of(factor_predictors), all_of(dependent), pct = "row", na = "drop",
      color = "diff", ref = 1, color_signif = "grey_non_signif", stars = TRUE,
      method_diff = "wald"
      ) |>
  dplyr::mutate(diff = set_display(`01-Married`, "diff"))
```

``` r-output
#> # A tabxplor tab: 18 × 6
#> # Groups:         row_var [3]
#>    row_var levels              `01-Married` `02-Not married`           Total
#>    <fct>   <fct>                     <row%>           <row%>          <row%>
#>  1 race    White                     52%              48%    100% (n= 9 846)
#>  2 race    Black                     31%***           69%*** 100% (n= 1 860)
#>  3 race    Other                     49%*             51%*   100% (n= 1 254)
#>  4 race    Total                     49%***           51%*** 100% (n=12 960)
#> 
#>  5 rincome 1-Lt $10000               37%              63%    100% (n= 2 142)
#>  6 rincome 2-$10000 to 14999         41%**            59%**  100% (n= 1 164)
#>  7 rincome 3-$15000 to 24999         43%***           57%*** 100% (n= 2 322)
#>  8 rincome 4-$25000 or more          55%***           45%*** 100% (n= 7 332)
#>  9 rincome Total                     49%***           51%*** 100% (n=12 960)
#> 
#> 10 relig   1-Protestant              52%              48%    100% (n= 6 269)
#> 11 relig   2-Catholic                52%              48%    100% (n= 3 121)
#> 12 relig   3-Other christian         42%***           58%*** 100% (n=   507)
#> 13 relig   4-Jewish                  55%              45%    100% (n=   222)
#> 14 relig   5-Buddhist/Hinduist       50%              50%    100% (n=   144)
#> 15 relig   6-Muslim                  48%              52%    100% (n=    56)
#> 16 relig   7-Other                   35%***           65%*** 100% (n=   267)
#> 17 relig   8-None                    38%***           62%*** 100% (n= 2 374)
#> 18 relig   Total                     49%***           51%*** 100% (n=12 960)
#> # ℹ 1 more variable: diff <row%-diff>
#> # différence (réf.) : -30 -20 -10 -5 +5 +10 +20 +30 [gris : non significatif ou sous ±5 points]
#> # *** : chiffre significativement différent de celui de la modalité de référence (en gras), au seuil de confiance de 99 % ; ** : au seuil de 95 % ; * : au seuil de 90 % ; aucune étoile : non significatif.
```

## Variables expliquées binomiales groupées (un score sommé)

Quand la variable expliquée est un **score sommé** — combien d’items
oui/non parmi plusieurs un répondant a répondu d’une certaine façon — on
modélise le nombre de « succès » sur un nombre fixe d’items avec
`trials =`.
[`tab_reg()`](https://bricenocenti.github.io/tabxplor/reference/tab_reg.md)
ajuste alors `cbind(score, trials - score)` comme un binomial, si bien
que les rapports de cotes se lisent sur la probabilité *par item*.
[`score_from_lv1()`](https://bricenocenti.github.io/tabxplor/reference/score_from_lv1.md)
(voir
[`vignette("tabxplor-programming")`](https://bricenocenti.github.io/tabxplor/articles/tabxplor-programming.md))
construit un tel score en comptant les facteurs à leur première modalité
:

``` r
gss_simple |>
  score_from_lv1("score", vars_list = c("married", "income25k")) |>   # a 0–2 score
  tab_reg("score", c("race", "age"), family = "binomial", trials = 2)
```

``` r-output
#> | predictors | Model fit   |  score |
#> |:-----------|:------------|-------:|
#> | race, age  | N           | 21 407 |
#> |            | LR vs null  | <0.01% |
#> |            | McFadden R2 |  0.012 |
#> |            | AIC         | 44 395 |
#> |            | BIC         | 44 427 |
#> |            | Dispersion  |   1.09 |
#> 
#> # A tabxplor tab: 5 × 3
#> # Groups:         var [3]
#>   var      levels                Model_OR
#>   <fct>    <fct>                <row%-or>
#> 1 Constant Reference population 1/1.10***
#> 
#> 2 race     White                     1   
#> 3 race     Black                1/1.99***
#> 4 race     Other                1/1.20***
#> 
#> 5 age      age                  1/1.00***
#> # Modèle : régression logistique ; rapports de cotes (par rapport à la modalité de référence).
#> # OR (réf.) : 1/4 1/2 1/1,5 1/1,2 1,2 1,5 2 4 [gris : non significatif ou sous ×1,2]
#> # *** : chiffre significativement différent de celui de la modalité de référence (en gras), au seuil de confiance de 99 % ; ** : au seuil de 95 % ; * : au seuil de 90 % ; aucune étoile : non significatif.
```

Les modèles binomiaux groupés (comme Poisson) rapportent un contrôle de
**dispersion** de Pearson dans le bas de tableau, signalant les
comptages surdispersés.

## Variables expliquées ordinales et nominales (un facteur à 3 modalités ou plus)

Une variable expliquée facteur **ordonné** est ajustée comme un modèle
logistique à cotes proportionnelles (cumulé) :

``` r
tab_reg(gss_simple, "rincome", c("race", "age", "relig"))
#> ℹ "rincome": ordered outcome detected -> `family = "ordinal"`
#>   (proportional-odds).
#> Warning in brant::brant(fit): 5 combinations in table(dv,ivs) do not occur.
#> Because of that, the test results might be invalid.
#> Warning: ! The proportional-odds (parallel-lines) assumption is rejected (Brant omnibus
#>   p = 0.0089).
#> ℹ Cumulative odds ratios may mislead; consider `family = "multinomial"` or a
#>   partial proportional-odds model.
#> ℹ The Brant test over-rejects at large N; inspect the per-variable tests too.
```

``` r-output
#> | predictors       | Model fit     | rincome |
#> |:-----------------|:--------------|--------:|
#> | race, age, relig | N             |  12 960 |
#> |                  | LR vs null    |  <0.01% |
#> |                  | McFadden R2   |   0.017 |
#> |                  | AIC           |  29 193 |
#> |                  | BIC           |  29 290 |
#> |                  | Brant PO test |  0.887% |
#> 
#> # A tabxplor tab: 13 × 3
#> # Groups:         var [4]
#>    var      levels                Model_OR
#>    <fct>    <fct>                <row%-or>
#>  1 Constant Reference population          
#> 
#>  2 race     White                     1   
#>  3 race     Black                1/1.40***
#>  4 race     Other                1/1.45***
#> 
#>  5 age      age                    1.02***
#> 
#>  6 relig    1-Protestant              1   
#>  7 relig    2-Catholic             1.15***
#>  8 relig    3-Other christian      1.02   
#>  9 relig    4-Jewish               2.00***
#> 10 relig    5-Buddhist/Hinduist    2.41***
#> 11 relig    6-Muslim               1.32   
#> 12 relig    7-Other                1.09   
#> 13 relig    8-None                 1.05   
#> # Modèle : régression logistique ordinale ; rapports de cotes cumulés (modèle à cotes proportionnelles).
#> # OR (réf.) : 1/4 1/2 1/1,5 1/1,2 1,2 1,5 2 4 [gris : non significatif ou sous ×1,2]
#> # *** : chiffre significativement différent de celui de la modalité de référence (en gras), au seuil de confiance de 99 % ; ** : au seuil de 95 % ; * : au seuil de 90 % ; aucune étoile : non significatif.
```

Une variable expliquée nominale à trois modalités non ordonnées ou plus
est ajustée comme un seul modèle logistique **multinomial**, donnant une
colonne de rapport de cotes par catégorie de la variable expliquée
contre la catégorie de référence (aussi appelés rapports de risques
relatifs) :

``` r
tab_reg(gss_simple, "party3", c("race", "age", "rincome", "relig"))
#> ℹ "party3": nominal outcome detected -> `family = "multinomial"` (multinomial
#>   logistic).
```

``` r-output
#> | predictors          | Model fit   | party3 |
#> |:--------------------|:------------|-------:|
#> | race, age, rincome, | N           | 12 914 |
#> | relig               | LR vs null  | <0.01% |
#> |                     | McFadden R2 |  0.083 |
#> |                     | AIC         | 24 939 |
#> |                     | BIC         | 25 148 |
#> 
#> # A tabxplor tab: 17 × 4
#> # Groups:         var [5]
#>    var      levels               2-Independent, other v…¹ 3-Republican vs 1-De…²
#>    <fct>    <fct>                               <row%-or>              <row%-or>
#>  1 Constant Reference population                    18                   1.87***
#> 
#>  2 race     White                                    1                      1   
#>  3 race     Black                               1/3.03***             1/12.89***
#>  4 race     Other                                 1.05                 1/2.37***
#> 
#>  5 age      age                                 1/1.01***              1/1.00***
#> 
#>  6 rincome  1-Lt $10000                              1                      1   
#>  7 rincome  2-$10000 to 14999                     1.01                 1/1.21** 
#>  8 rincome  3-$15000 to 24999                   1/1.00                 1/1.08   
#>  9 rincome  4-$25000 or more                    1/1.53***                1.17** 
#> 
#> 10 relig    1-Protestant                             1                      1   
#> 11 relig    2-Catholic                          1/1.11                 1/1.78***
#> 12 relig    3-Other christian                     1.13                 1/1.27** 
#> 13 relig    4-Jewish                            1/2.96***              1/4.88***
#> 14 relig    5-Buddhist/Hinduist                 1/1.33                 1/4.83***
#> 15 relig    6-Muslim                            1/2.36**               1/5.96***
#> 16 relig    7-Other                               1.10                 1/2.56***
#> 17 relig    8-None                                1.22***              1/3.54***
#> # ℹ abbreviated names: ¹​`2-Independent, other vs 1-Democrat`,
#> #   ²​`3-Republican vs 1-Democrat`
#> # Modèle : régression logistique multinomiale ; rapports de cotes (chaque modalité par rapport à la référence).
#> # OR (réf.) : 1/4 1/2 1/1,5 1/1,2 1,2 1,5 2 4 [gris : non significatif ou sous ×1,2]
#> # *** : chiffre significativement différent de celui de la modalité de référence (en gras), au seuil de confiance de 99 % ; ** : au seuil de 95 % ; * : au seuil de 90 % ; aucune étoile : non significatif.
```

Les rapports de risques relatifs peuvent être assez difficiles à lire
parce qu’ils sont relatifs à **deux** modalités de référence : non
seulement la référence choisie pour le prédicteur, mais aussi la
modalité de référence choisie pour la variable expliquée. C’est
particulièrement difficile quand il est ardu de trouver une bonne
modalité de référence correspondant à la situation la plus commune
(comme « married » pour le statut matrimonial).

La plupart du temps, les effets marginaux moyens (AME) sont plus faciles
à interpréter, parce qu’ils font disparaître la seconde modalité de
référence, et modélisent directement, pour chaque modalité de la
variable expliquée, la différence de pourcentages de chaque prédicteur
comparée à leur modalité de référence (une quantité moins abstraite que
des rapports de cotes).

``` r
tab_reg(gss_simple, "party3", c("race", "age", "rincome", "relig"), effect = "ame", empirical = TRUE) # |> tab_export()
#> ℹ "party3": nominal outcome detected -> `family = "multinomial"` (multinomial
#>   logistic).
```

``` r-output
#> | predictors          | Model fit   | party3 |
#> |:--------------------|:------------|-------:|
#> | race, age, rincome, | N           | 12 914 |
#> | relig               | LR vs null  | <0.01% |
#> |                     | McFadden R2 |  0.083 |
#> |                     | AIC         | 24 939 |
#> |                     | BIC         | 25 148 |
#> 
#> # A tabxplor tab: 17 × 5
#> # Groups:         var [5]
#>    var      levels                    `1-Democrat` `2-Independent, other`
#>    <fct>    <fct>                     <row%-mixed>           <row%-mixed>
#>  1 Constant Reference population                                         
#> 
#>  2 race     White                          (38.3%)                (20.6%)
#>  3 race     Black                +40.6%*** (78.9%)       -6.6%*** (14.0%)
#>  4 race     Other                +11.0%*** (49.3%)       +6.8%*** (27.4%)
#> 
#>  5 age      age                           +0.2%***               -0.1%***
#> 
#>  6 rincome  1-Lt $10000                    (44.0%)                (24.5%)
#>  7 rincome  2-$10000 to 14999     +2.0%    (46.0%)       +1.6%    (26.1%)
#>  8 rincome  3-$15000 to 24999     +0.9%    (44.9%)       +0.5%    (25.0%)
#>  9 rincome  4-$25000 or more      +1.8%    (45.8%)       -7.7%*** (16.8%)
#> 
#> 10 relig    1-Protestant                   (39.2%)                (17.2%)
#> 11 relig    2-Catholic            +9.2%*** (48.4%)       +2.4%*** (19.6%)
#> 12 relig    3-Other christian     +2.4%    (41.7%)       +3.7%**  (20.9%)
#> 13 relig    4-Jewish             +31.4%*** (70.6%)       -6.0%*** (11.2%)
#> 14 relig    5-Buddhist/Hinduist  +23.1%*** (62.3%)       +4.4%    (21.6%)
#> 15 relig    6-Muslim             +31.6%*** (70.9%)       -3.1%    (14.1%)
#> 16 relig    7-Other              +11.7%*** (50.9%)       +8.0%*** (25.2%)
#> 17 relig    8-None               +13.4%*** (52.7%)      +11.9%*** (29.1%)
#> # ℹ 1 more variable: `3-Republican` <row%-mixed>
#> # Modèle : régression logistique multinomiale ; effets marginaux sur l'échelle de probabilité (points de pourcentage) (moyenne sur l'échantillon) ; chaque case indique l'effet par rapport à la modalité de référence et, entre parenthèses, la probabilité prédite ajustée.
#> # AME (réf.) : -30 -20 -10 -5 +5 +10 +20 +30 [gris : non significatif ou sous ±5 points]
#> # *** : chiffre significativement différent de celui de la modalité de référence (en gras), au seuil de confiance de 99 % ; ** : au seuil de 95 % ; * : au seuil de 90 % ; aucune étoile : non significatif.
```

Avec `empirical = TRUE`, les différences brutes/observées/non ajustées
et les pourcentages bruts apparaissent dans des infobulles html,
disponibles au survol de la souris sur une case.

## Régression linéaire (une variable expliquée numérique)

Une variable expliquée continue donne de simples coefficients de
régression linéaire (ici nous fixons `family` explicitement, parce qu’un
entier comme `age` est ambigu — il pourrait aussi être modélisé comme un
comptage) :

``` r
tab_reg(gss_simple, "age", c("race", "marital", "relig", "rincome"), family = "gaussian")
```

``` r-output
#> | predictors     | Model fit   |    age |
#> |:---------------|:------------|-------:|
#> | race, marital, | N           | 12 958 |
#> | relig, rincome | F           | <0.01% |
#> |                | R2          |  0.256 |
#> |                | Adjusted R2 |  0.255 |
#> |                | Residual SD |  11.62 |
#> 
#> # A tabxplor tab: 21 × 3
#> # Groups:         var [5]
#>    var      levels                 Model_β
#>    <fct>    <fct>                   <coef>
#>  1 Constant Reference population  43.81***
#> 
#>  2 race     White                     0   
#>  3 race     Black                 -1.00***
#>  4 race     Other                 -3.32***
#> 
#>  5 marital  Married                   0   
#>  6 marital  Separated             -2.02***
#>  7 marital  Divorced               2.88***
#>  8 marital  Widowed               15.15***
#>  9 marital  Never married        -10.72***
#> 
#> 10 relig    1-Protestant              0   
#> 11 relig    2-Catholic            -1.03***
#> 12 relig    3-Other christian     -3.25***
#> 13 relig    4-Jewish               3.39***
#> 14 relig    5-Buddhist/Hinduist    0.49   
#> 15 relig    6-Muslim              -2.86*  
#> 16 relig    7-Other               -3.52***
#> 17 relig    8-None                -2.79***
#> 
#> 18 rincome  1-Lt $10000               0   
#> 19 rincome  2-$10000 to 14999      1.18***
#> 20 rincome  3-$15000 to 24999      0.96***
#> 21 rincome  4-$25000 or more       3.17***
#> # Modèle : régression linéaire ; coefficients (différence de moyenne par rapport à la modalité de référence).
#> # β (réf.) : -0,8 -0,5 -0,2 +0,2 +0,5 +0,8 [gris : non significatif ou sous ±0,2 écarts-types]
#> # *** : chiffre significativement différent de celui de la modalité de référence (en gras), au seuil de confiance de 99 % ; ** : au seuil de 95 % ; * : au seuil de 90 % ; aucune étoile : non significatif.
```

Dans le cas d’une régression linéaire, la **contrepartie
empirique/observée du coefficient du modèle** pour un prédicteur
catégoriel est simplement la **différence de moyennes** : ici, la
différence d’âge moyen, par modalité du prédicteur, comparée à la
modalité de référence.

``` r
tab_reg(gss_simple, "age", c("race", "marital", "relig", "rincome"), family = "gaussian", empirical = TRUE)
```

``` r-output
#> | predictors     | Model fit   |    age |
#> |:---------------|:------------|-------:|
#> | race, marital, | N           | 12 958 |
#> | relig, rincome | F           | <0.01% |
#> |                | R2          |  0.256 |
#> |                | Adjusted R2 |  0.255 |
#> |                | Residual SD |  11.62 |
#> 
#> # A tabxplor tab: 21 × 5
#> # Groups:         var [5]
#>    var      levels                     Obs_mean  Obs_diff   Model_β
#>    <fct>    <fct>                        <mean>    <coef>    <coef>
#>  1 Constant Reference population                           43.81***
#> 
#>  2 race     White                43.32 (σ13.58)      0         0   
#>  3 race     Black                40.31 (σ12.67)  -3.01***  -1.00***
#>  4 race     Other                38.06 (σ12.46)  -5.26***  -3.32***
#> 
#>  5 marital  Married              44.91 (σ12.20)      0         0   
#>  6 marital  Separated            41.94 (σ10.93)  -2.97***  -2.02***
#>  7 marital  Divorced             47.63 (σ11.16)   2.72***   2.88***
#>  8 marital  Widowed              59.88 (σ13.13)  14.97***  15.15***
#>  9 marital  Never married        33.03 (σ11.42) -11.88*** -10.72***
#> 
#> 10 relig    1-Protestant         44.15 (σ13.60)      0         0   
#> 11 relig    2-Catholic           41.99 (σ13.37)  -2.16***  -1.03***
#> 12 relig    3-Other christian    39.23 (σ11.47)  -4.92***  -3.25***
#> 13 relig    4-Jewish             48.04 (σ15.08)   3.89***   3.39***
#> 14 relig    5-Buddhist/Hinduist  41.45 (σ12.95)  -2.70**    0.49   
#> 15 relig    6-Muslim             38.71 (σ10.55)  -5.43***  -2.86*  
#> 16 relig    7-Other              38.91 (σ11.78)  -5.24***  -3.52***
#> 17 relig    8-None               38.92 (σ12.69)  -5.23***  -2.79***
#> 
#> 18 rincome  1-Lt $10000          38.50 (σ16.75)      0         0   
#> 19 rincome  2-$10000 to 14999    41.09 (σ15.17)   2.59***   1.18***
#> 20 rincome  3-$15000 to 24999    40.82 (σ13.34)   2.32***   0.96***
#> 21 rincome  4-$25000 or more     44.21 (σ11.69)   5.71***   3.17***
#> # Modèle : régression linéaire ; coefficients (différence de moyenne par rapport à la modalité de référence).
#> # Obs_diff, Model_β : différence standardisée (réf.) : -0,8 -0,5 -0,2 +0,2 +0,5 +0,8 [gris : non significatif ou sous ±0,2 écarts-types]
#> # *** : chiffre significativement différent de celui de la modalité de référence (en gras), au seuil de confiance de 99 % ; ** : au seuil de 95 % ; * : au seuil de 90 % ; aucune étoile : non significatif.
```

Les âges moyens empiriques, et les différences d’âge moyen à la
référence, peuvent être calculés dans un simple tableau avec :

``` r
tab(gss_simple, "race", "age", pct = "row", digits = 2, na = "drop",
    color = "diff", ref = 1,  method_mean_diff = "student"
) |>
  mutate(diff = set_display(age, "diff"))
```

``` r-output
#> # A tabxplor tab: 4 × 3
#>   race              age        diff
#>   <fct>          <mean> <mean-diff>
#> 1 White  48.72 (σ17.50)   ref:48.72
#> 2 Black  43.90 (σ16.06)       -4.83
#> 3 Other  39.48 (σ14.39)       -9.24
#> 4 Total  47.18 (σ17.29)       -1.54
#> # différence standardisée (White) : -0,8 -0,5 -0,2 +0,2 +0,5 +0,8
```

``` r
# student : intervalles de confiance a variance commune, pour correspondre a ceux calcules par la regression lineaire
```

## Régression de Poisson (une variable expliquée de comptage)

``` r
tab_reg(gss_simple, "tvhours", c("race", "marital", "relig", "rincome"), family = "poisson")
```

``` r-output
#> | predictors     | Model fit   | tvhours |
#> |:---------------|:------------|--------:|
#> | race, marital, | N           |   6 811 |
#> | relig, rincome | LR vs null  |  <0.01% |
#> |                | McFadden R2 |   0.033 |
#> |                | AIC         |  26 179 |
#> |                | BIC         |  26 295 |
#> |                | Dispersion  |    1.45 |
#> 
#> # A tabxplor tab: 21 × 3
#> # Groups:         var [5]
#>    var      levels               Model_IRR
#>    <fct>    <fct>                <row%-or>
#>  1 Constant Reference population   2.83***
#> 
#>  2 race     White                     1   
#>  3 race     Black                  1.47***
#>  4 race     Other                  1.01   
#> 
#>  5 marital  Married                   1   
#>  6 marital  Separated              1.06   
#>  7 marital  Divorced               1.05*  
#>  8 marital  Widowed                1.11** 
#>  9 marital  Never married          1.07***
#> 
#> 10 relig    1-Protestant              1   
#> 11 relig    2-Catholic             1.00   
#> 12 relig    3-Other christian    1/1.18***
#> 13 relig    4-Jewish             1/1.11   
#> 14 relig    5-Buddhist/Hinduist  1/1.26** 
#> 15 relig    6-Muslim             1/1.58***
#> 16 relig    7-Other              1/1.02   
#> 17 relig    8-None               1/1.11***
#> 
#> 18 rincome  1-Lt $10000               1   
#> 19 rincome  2-$10000 to 14999    1/1.04   
#> 20 rincome  3-$15000 to 24999    1/1.10***
#> 21 rincome  4-$25000 or more     1/1.34***
#> # Modèle : régression de Poisson ; rapports de taux d'incidence (par rapport à la modalité de référence).
#> # IRR (réf.) : 1/4 1/2 1/1,5 1/1,2 1,2 1,5 2 4 [gris : non significatif ou sous ×1,2]
#> # *** : chiffre significativement différent de celui de la modalité de référence (en gras), au seuil de confiance de 99 % ; ** : au seuil de 95 % ; * : au seuil de 90 % ; aucune étoile : non significatif.
```

Un **rapport de taux d’incidence** (IRR) de 1,5 signifie « 50 % d’heures
de télévision de plus par jour ». Les modèles de Poisson non pondérés
utilisent automatiquement des erreurs-types mises à l’échelle par la
dispersion (quasi-Poisson), si bien que les comptages surdispersés
obtiennent des intervalles honnêtes, plus larges. Concrètement : avec
une variable expliquée surdispersée, `family = "poisson"` renvoie des IC
et des p-valeurs **identiques à `family = "quasipoisson"`** et émet un
avertissement le disant (le bas de tableau rapporte la dispersion) ; à
équidispersion (≈ 1), la mise à l’échelle est sans effet et le résultat
correspond à un simple `glm(family = poisson)` — de sorte qu’une
comparaison à un `glm` de Poisson ajusté à la main ne vous surprend
jamais.

Dans le cas d’une régression de Poisson, la **contrepartie
empirique/observée du coefficient exponentié du modèle** pour un
prédicteur catégoriel est le **rapport de moyennes** : ici, le rapport
des heures moyennes de télévision comparé à la modalité de référence.

``` r
tab_reg(gss_simple, "tvhours", c("race", "marital", "relig", "rincome"), family = "poisson", empirical = TRUE)
```

``` r-output
#> | predictors     | Model fit   | tvhours |
#> |:---------------|:------------|--------:|
#> | race, marital, | N           |   6 811 |
#> | relig, rincome | LR vs null  |  <0.01% |
#> |                | McFadden R2 |   0.033 |
#> |                | AIC         |  26 179 |
#> |                | BIC         |  26 295 |
#> |                | Dispersion  |    1.45 |
#> 
#> # A tabxplor tab: 21 × 5
#> # Groups:         var [5]
#>    var      levels               Obs_rate   Obs_IRR Model_IRR
#>    <fct>    <fct>                  <mean> <row%-or> <row%-or>
#>  1 Constant Reference population                      2.83***
#> 
#>  2 race     White                 2.36         1         1   
#>  3 race     Black                 3.66***   1.55***   1.47***
#>  4 race     Other                 2.44      1.03      1.01   
#> 
#>  5 marital  Married               2.36         1         1   
#>  6 marital  Separated             2.87***   1.21***   1.06   
#>  7 marital  Divorced              2.54***   1.08***   1.05*  
#>  8 marital  Widowed               2.93***   1.24***   1.11** 
#>  9 marital  Never married         2.81***   1.19***   1.07***
#> 
#> 10 relig    1-Protestant          2.72         1         1   
#> 11 relig    2-Catholic            2.50*** 1/1.09***   1.00   
#> 12 relig    3-Other christian     2.31*** 1/1.18*** 1/1.18***
#> 13 relig    4-Jewish              2.14*** 1/1.27*** 1/1.11   
#> 14 relig    5-Buddhist/Hinduist   1.95*** 1/1.40*** 1/1.26** 
#> 15 relig    6-Muslim              1.89**  1/1.44**  1/1.58***
#> 16 relig    7-Other               2.64    1/1.03    1/1.02   
#> 17 relig    8-None                2.37*** 1/1.15*** 1/1.11***
#> 
#> 18 rincome  1-Lt $10000           3.10         1         1   
#> 19 rincome  2-$10000 to 14999     3.02    1/1.03    1/1.04   
#> 20 rincome  3-$15000 to 24999     2.84**  1/1.09**  1/1.10***
#> 21 rincome  4-$25000 or more      2.24*** 1/1.39*** 1/1.34***
#> # Modèle : régression de Poisson ; rapports de taux d'incidence (par rapport à la modalité de référence).
#> # Obs_rate : rapport (réf.) : ÷4 ÷2 ÷1,5 ÷1,2 ×1,2 ×1,5 ×2 ×4 [gris : non significatif ou sous ×1,2]
#> # Obs_IRR, Model_IRR : IRR (réf.) : 1/4 1/2 1/1,5 1/1,2 1,2 1,5 2 4 [gris : non significatif ou sous ×1,2]
#> # *** : chiffre significativement différent de celui de la modalité de référence (en gras), au seuil de confiance de 99 % ; ** : au seuil de 95 % ; * : au seuil de 90 % ; aucune étoile : non significatif.
```

Les heures moyennes empiriques de télévision par jour, et les
différences de taux de visionnage à la référence, peuvent être calculées
dans un simple tableau avec :

``` r
tab(gss_simple, "race", "tvhours", pct = "row", digits = 2, na = "drop",
    color = "ratio", ref = 1,  method_mean_ratio = "quasipoisson"
) |>
  mutate(IRR = set_display(tvhours, "ratio"))
```

``` r-output
#> # A tabxplor tab: 4 × 3
#>   race        tvhours          IRR
#>   <fct>        <mean> <mean-ratio>
#> 1 White  2.77 (σ2.31)           ×1
#> 2 Black  4.18 (σ3.51)        ×1.51
#> 3 Other  2.76 (σ2.41)           ×1
#> 4 Total  2.98 (σ2.59)        ×1.08
#> # rapport (White) : ÷4 ÷2 ÷1,5 ÷1,2 ×1,2 ×1,5 ×2 ×4
```

``` r
# la methode par defaut pour les intervalles de confiance est le rapport de moyennes robuste a variance inegale ;
#  on utilise "quasipoisson" pour correspondre a ceux calcules par la regression quasi-poisson
#  (hypothese : la variance est proportionnelle a la moyenne).
```

## Données pondérées et d’enquête

Passer une colonne de pondération avec `wt =` bascule l’estimation vers
un **plan de sondage**
([`survey::svyglm()`](https://rdrr.io/pkg/survey/man/svyglm.html)), qui
donne des erreurs-types *fondées sur le plan* correctes. Le cadre
qu’utilise `tabxplor` est simple : l’**estimation est pondérée**, et les
erreurs-types du modèle viennent du plan de sondage, si bien que des
poids inégaux élargissent honnêtement les intervalles de confiance.
(C’est la contrepartie fondée sur le plan de l’approximation moins
coûteuse `options(tabxplor.kish_neff = TRUE)` de
[`tab()`](https://bricenocenti.github.io/tabxplor/reference/tab.md) —
voir
[`vignette("tabxplor")`](https://bricenocenti.github.io/tabxplor/articles/tabxplor.md).)

``` r
# `weight` est une colonne de poids d'enquête dans vos donnees, le plus souvent un usage de base :
tab_reg(data, "dependent", c("pred1", "pred2"), wt = "weight")

# Un plan stratifie / en grappes (les strates *retrecissent* generalement les intervalles, les grappes les elargissent) :
tab_reg(data, "dependent", c("pred1", "pred2"),
        wt = "weight", ids = "psu", strata = "stratum")

# Ou passez un plan que vous avez construit vous-meme avec survey::svydesign() comme `data` (voir ?survey::svydesign
# pour la calibration, les poids de replication et les cas plus complexes).
```

Si vous n’avez que de simples poids, `wt =` suffit. Donnez `strata =`
quand vous avez une enquête stratifiée : cela gagne typiquement un peu
de précision (des intervalles de confiance plus étroits) lorsque les
variables de strate sont liées à la variable expliquée.

Les colonnes compagnes observées de `empirical = TRUE` (les effets non
ajustés `Obs_*`) sont *descriptives*, donc sur des données pondérées
leurs intervalles de confiance honorent
`options(tabxplor.kish_neff = TRUE)` exactement comme
[`tab()`](https://bricenocenti.github.io/tabxplor/reference/tab.md).
Activez-la lorsque vous utilisez
[`tab_reg()`](https://bricenocenti.github.io/tabxplor/reference/tab_reg.md)
avec des poids, pour une comparaison d’incertitude plus comparable (les
colonnes du modèle elles-mêmes restent fondées sur le plan quoi qu’il
arrive, donc il subsiste toujours un écart entre les deux façons de
calculer les intervalles de confiance).

## Comparer plusieurs modèles

Passez une **liste nommée** d’ensembles de prédicteurs au lieu d’un
vecteur pour ajuster et afficher plusieurs modèles côte à côte, et
`compare =` ajoute un test du rapport de vraisemblance de comparaison
dans le bas de tableau (`"baseline"` = chaque modèle vs le premier ;
`"sequential"` = chacun vs le précédent) :

``` r
tab_reg(gss_simple,
        "married",
        list("Race only"    = "race",
             "+ age"        = c("race", "age"),
             "+ party"      = c("race", "age", "party3")),
        compare = "sequential")
#> ℹ "married": binary outcome detected -> `family = "binomial"` (logistic).
#> ℹ Column "+ age": models are not nested or N differs -> showing the AIC
#>   difference vs the previous model instead of a likelihood-ratio test.
#> ℹ A different N is usually the per-model missing-value drop; set `na =
#>   "drop_all_models"` to fit every model on the same complete cases so the
#>   likelihood-ratio test can run.
#> ℹ Column "+ party": models are not nested or N differs -> showing the AIC
#>   difference vs the previous model instead of a likelihood-ratio test.
#> ℹ A different N is usually the per-model missing-value drop; set `na =
#>   "drop_all_models"` to fit every model on the same complete cases so the
#>   likelihood-ratio test can run.
```

``` r-output
#> | Model fit             | Race only |   |  + age |   | + party |
#> |:----------------------|----------:|:-:|-------:|:-:|--------:|
#> | N                     |    21 483 |   | 21 407 |   |  21 261 |
#> | LR vs null            |    <0.01% |   | <0.01% |   |  <0.01% |
#> | McFadden R2           |     0.019 |   |  0.023 |   |   0.032 |
#> | AIC                   |    29 139 |   | 28 933 |   |  28 489 |
#> | BIC                   |    29 163 |   | 28 965 |   |  28 537 |
#> | Delta-AIC vs previous |           |   |   -206 |   |    -444 |
#> 
#> # A tabxplor tab: 8 × 5
#> # Groups:         var [4]
#>   var      levels               `Race only`   `+ age` `+ party`
#>   <fct>    <fct>                  <row%-or> <row%-or> <row%-or>
#> 1 Constant Reference population       13*   1/1.46*** 1/1.79***
#> 
#> 2 race     White                       1         1         1   
#> 3 race     Black                  1/2.68*** 1/2.58*** 1/2.19***
#> 4 race     Other                  1/1.13*** 1/1.05      1.05   
#> 
#> 5 age      age                                1.01***   1.01***
#> 
#> 6 party3   1-Democrat                                      1   
#> 7 party3   2-Independent, other                         1.05   
#> 8 party3   3-Republican                                 1.65***
#> # Modèle : régression logistique de married ('01-Married') ; rapports de cotes (par rapport à la modalité de référence).
#> # OR (réf.) : 1/4 1/2 1/1,5 1/1,2 1,2 1,5 2 4 [gris : non significatif ou sous ×1,2]
#> # *** : chiffre significativement différent de celui de la modalité de référence (en gras), au seuil de confiance de 99 % ; ** : au seuil de 95 % ; * : au seuil de 90 % ; aucune étoile : non significatif.
```

## Le même modèle au sein de sous-populations

`split_var =` est l’analogue de régression de `tab_vars` de
[`tab()`](https://bricenocenti.github.io/tabxplor/reference/tab.md) : le
même modèle est ajusté **au sein de chaque modalité** d’une variable de
regroupement, et les tableaux par groupe sont empilés en un seul tableau
groupé. Il répond à « cet effet tient-il dans chaque sous-groupe ? ».
Avec une seule variable expliquée, il utilise
[`tab_spread()`](https://bricenocenti.github.io/tabxplor/reference/tab_spread.md)
en interne pour disposer les groupes en colonnes côte à côte, pour une
comparaison facile :

``` r
tab_reg(gss_simple, "married", c("race", "rincome"), split_var = "year")
#> ℹ "married": binary outcome detected -> `family =
#> "binomial"` (logistic).
```

``` r-output
#> | predictors    | Model fit   |   2000 |   |   2002 |   |   2004 |   |   2006 |   |   2008 |   |   2010 |   |   2012 |   |   2014 |
#> |:--------------|:------------|-------:|:-:|-------:|:-:|-------:|:-:|-------:|:-:|-------:|:-:|-------:|:-:|-------:|:-:|-------:|
#> | race, rincome | N           |  1 818 |   |  1 780 |   |  1 688 |   |  2 669 |   |  1 189 |   |  1 202 |   |  1 146 |   |  1 523 |
#> |               | LR vs null  | <0.01% |   | <0.01% |   | <0.01% |   | <0.01% |   | <0.01% |   | <0.01% |   | <0.01% |   | <0.01% |
#> |               | McFadden R2 |  0.020 |   |  0.024 |   |  0.033 |   |  0.029 |   |  0.034 |   |  0.043 |   |  0.050 |   |  0.046 |
#> |               | AIC         |  2 475 |   |  2 418 |   |  2 267 |   |  3 604 |   |  1 604 |   |  1 598 |   |  1 519 |   |  2 021 |
#> |               | BIC         |  2 508 |   |  2 451 |   |  2 299 |   |  3 640 |   |  1 635 |   |  1 628 |   |  1 549 |   |  2 053 |
#> 
#> # A tabxplor tab: 8 × 10
#> # Groups:         var [3]
#>   var     levels                  `2000`    `2002`    `2004`    `2006`    `2008`
#>   <fct>   <fct>                <row%-or> <row%-or> <row%-or> <row%-or> <row%-or>
#> 1 Consta… Reference population 1/1.54*** 1/1.45*** 1/1.23    1/1.22*   1/1.60***
#> 
#> 2 race    White                     1         1         1         1         1   
#> 3 race    Black                1/1.89*** 1/2.15*** 1/2.42*** 1/2.44*** 1/2.21***
#> 4 race    Other                1/1.10      1.12      1.04      1.01    1/1.10   
#> 
#> 5 rincome 1-Lt $10000               1         1         1         1         1   
#> 6 rincome 2-$10000 to 14999      1.44**    1.48**    1.11    1/1.20      1.13   
#> 7 rincome 3-$15000 to 24999      1.28      1.22      1.29      1.08      1.43*  
#> 8 rincome 4-$25000 or more       1.85***   1.79***   2.04***   1.70***   2.25***
#> # ℹ 3 more variables: `2010` <row%-or>, `2012` <row%-or>, `2014` <row%-or>
#> # Modèle : régression logistique ; rapports de cotes (par rapport à la modalité de référence).
#> # OR (réf.) : 1/4 1/2 1/1,5 1/1,2 1,2 1,5 2 4 [gris : non significatif ou sous ×1,2]
#> # *** : chiffre significativement différent de celui de la modalité de référence (en gras), au seuil de confiance de 99 % ; ** : au seuil de 95 % ; * : au seuil de 90 % ; aucune étoile : non significatif.
```

## Affiner l’affichage, et mettre à l’échelle un prédicteur

Quelques arguments changent *ce que chaque case affiche*, ou *comment un
prédicteur est mis à l’échelle* — sans changer le modèle lui-même.

**`estimate_display`** replie une seconde quantité dans chaque case
d’estimation. `"ci"` affiche l’intervalle de confiance à côté de chaque
estimation (toute famille) :

``` r
tab_reg(gss_simple, "married", c("race", "age"), estimate_display = "ci")
#> ℹ "married": binary outcome detected -> `family =
#> "binomial"` (logistic).
```

``` r-output
#> | predictors | Model fit   | married |
#> |:-----------|:------------|--------:|
#> | race, age  | N           |  21 407 |
#> |            | LR vs null  |  <0.01% |
#> |            | McFadden R2 |   0.023 |
#> |            | AIC         |  28 933 |
#> |            | BIC         |  28 965 |
#> 
#> # A tabxplor tab: 5 × 3
#> # Groups:         var [3]
#>   var      levels                          Model_OR
#>   <fct>    <fct>                         <row%-est>
#> 1 Constant Reference population 0.69 [0.63;0.74]***
#> 
#> 2 race     White                            1.00   
#> 3 race     Black                0.39 [0.36;0.42]***
#> 4 race     Other                0.95 [0.86;1.04]   
#> 
#> 5 age      age                  1.01 [1.01;1.01]***
#> # Modèle : régression logistique ; rapports de cotes (par rapport à la modalité de référence).
#> # OR (réf.) : 1/4 1/2 1/1,5 1/1,2 1,2 1,5 2 4 [gris : non significatif ou sous ×1,2]
#> # *** : chiffre significativement différent de celui de la modalité de référence (en gras), au seuil de confiance de 99 % ; ** : au seuil de 95 % ; * : au seuil de 90 % ; aucune étoile : non significatif.
```

Pour un modèle logistique, vous pouvez à la place replier la probabilité
prédite ajustée par le modèle (`"prob"`) ou l’effet marginal moyen
(`"ame"`) dans la case du rapport de cotes (ces deux options nécessitent
le paquet `marginaleffects`). Notez la différence :
`estimate_display = "ame"` *garde* la colonne du rapport de cotes et
ajoute l’AME à côté, tandis que `effect = "ame"` (vu plus haut)
transforme la colonne **entière** en un AME.

**`multiplier`** remet à l’échelle l’effet d’un prédicteur continu. Un
changement d’un an de `age` déplace à peine les cotes, donc son rapport
de cotes se situe près de 1 ; `multiplier = c(age = 10)` rapporte
l’effet **par tranche de 10 ans** à la place (le rapport de cotes élevé
à la puissance 10, avec son intervalle mis à l’échelle pour
correspondre) :

``` r
tab_reg(gss_simple, "married", c("race", "age"), multiplier = c(age = 10))
#> ℹ "married": binary outcome detected -> `family =
#> "binomial"` (logistic).
```

``` r-output
#> | predictors | Model fit   | married |
#> |:-----------|:------------|--------:|
#> | race, age  | N           |  21 407 |
#> |            | LR vs null  |  <0.01% |
#> |            | McFadden R2 |   0.023 |
#> |            | AIC         |  28 933 |
#> |            | BIC         |  28 965 |
#> 
#> # A tabxplor tab: 5 × 3
#> # Groups:         var [3]
#>   var      levels                Model_OR
#>   <fct>    <fct>                <row%-or>
#> 1 Constant Reference population 1/1.46***
#> 
#> 2 race     White                     1   
#> 3 race     Black                1/2.58***
#> 4 race     Other                1/1.05   
#> 
#> 5 age      age (per 10)           1.09***
#> # Modèle : régression logistique ; rapports de cotes (par rapport à la modalité de référence).
#> # OR (réf.) : 1/4 1/2 1/1,5 1/1,2 1,2 1,5 2 4 [gris : non significatif ou sous ×1,2]
#> # *** : chiffre significativement différent de celui de la modalité de référence (en gras), au seuil de confiance de 99 % ; ** : au seuil de 95 % ; * : au seuil de 90 % ; aucune étoile : non significatif.
```

**`at = "reference"`** évalue l’effet au profil de référence (la
population abstraite combinant la modalité de référence de tous les
prédicteurs) au lieu de le moyenner sur l’échantillon — un effet
marginal *à la référence* pour `effect = "ame"`, ou un rapport de cotes
« catégorie vs le reste » à ce profil pour un modèle multinomial :

``` r
tab_reg(gss_simple, "married", c("race", "age"), effect = "ame", at = "reference")
```

## Lire les statistiques de bas de tableau

Le bas de tableau résume l’ajustement du modèle ; les statistiques qui
apparaissent dépendent de la famille :

- **N** — le nombre d’observations utilisées.
- **LR vs null** — un test du rapport de vraisemblance du modèle entier
  contre le modèle à constante seule (le modèle vaut-il quelque chose
  ?).
- **McFadden R²** — un pseudo-R² pour les modèles binomiaux et de
  poisson (plus élevé = les prédicteurs expliquent davantage ; les
  valeurs sont bien plus petites qu’un R² linéaire).
- **AIC / BIC** — des critères d’information pour comparer des modèles
  sur les mêmes données (plus bas = meilleur ; le BIC pénalise davantage
  la complexité).
- Les modèles linéaires (gaussiens) ajoutent les habituels **R² / R²
  ajusté / F / σ** ; Poisson ajoute un contrôle de **dispersion**.

Pour les modèles pondérés, un ensemble réduit est rapporté (un test de
Wald contre le modèle nul, un pseudo-R² de Nagelkerke / Cox–Snell, un
AIC de Rao–Scott), parce que les quantités fondées sur la vraisemblance
ne s’appliquent pas à un ajustement fondé sur le plan.

## Graphiques

[`or_plot()`](https://bricenocenti.github.io/tabxplor/reference/or_plot.md)
trace un graphique en forêt des rapports de cotes d’un tableau terminé,
et
[`lm_plots()`](https://bricenocenti.github.io/tabxplor/reference/lm_plots.md)
trace les diagnostics standard 2×2 d’un modèle linéaire :

``` r
t <- tab_reg(gss_simple, "married", c("race", "age"))
or_plot(t)

m <- tab_reg(gss_simple, "age", c("race", "marital"), family = "gaussian")
lm_plots(m)
```

## Pour aller plus loin

- [`tab_reg()`](https://bricenocenti.github.io/tabxplor/reference/tab_reg.md)
  est aussi disponible **sans écrire de code R**, comme une analyse
  **Regressions** à cliquer dans le module
  [jamovi](https://www.jamovi.org/download) — installez *tabxplor*
  depuis la bibliothèque de modules de jamovi (voir
  [`vignette("tabxplor")`](https://bricenocenti.github.io/tabxplor/articles/tabxplor.md)).
- [`vignette("tabxplor")`](https://bricenocenti.github.io/tabxplor/articles/tabxplor.md)
  — les tableaux croisés et les aides à la couleur.
- [`?tab_reg`](https://bricenocenti.github.io/tabxplor/reference/tab_reg.md)
  pour chaque argument (groupés par usage), et
  [`?tab_reg`](https://bricenocenti.github.io/tabxplor/reference/tab_reg.md)
  → *Details* pour les choix de modélisation.
- [`survey::svydesign()`](https://rdrr.io/pkg/survey/man/svydesign.html)
  pour les plans complexes (calibration, poids de réplication,
  corrections de population finie).
