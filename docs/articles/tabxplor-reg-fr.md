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

# Comme dans la vignette d'introduction, les tableaux sont rendus comme les vrais tableaux html
# de tabxplor (le reglage recommande au quotidien) ; la feuille de style partagee est emise une
# fois par tab_css() ci-dessous, et les infobulles restent coupees.
# options(tabxplor.lang = "fr") met les legendes et notes en francais.
options(tabxplor.print = "html")
options(tabxplor.tab_kable_css = FALSE)
options(tabxplor.tab_kable_tooltips = FALSE)

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
toutes choses étant inégales par ailleurs », pour ce prédicteur.

- logistique à 2 modalités (binomial) → % observés, et rapports de cotes
  observés (non modélisés : calculés uniquement à partir des
  pourcentages)
- logistique à 3 modalités ou plus (multinomial) → les OR observés sont
  affichés en infobulle sur les cases du modèle dans les exports html
- gaussien (linéaire) → moyennes de groupe et leur différence
- poisson (comptage) → taux observé et rapport de taux observé

## Régression logistique (un facteur binaire)

Quand la variable expliquée est un facteur à deux modalités, ici «
married » contre « not married »,
[`tab_reg()`](https://bricenocenti.github.io/tabxplor/reference/tab_reg.md)
choisit une famille **binomiale** pour ajuster une régression logistique
et rapporte des **rapports de cotes** (la modalité de référence de
chaque prédicteur affiche la valeur neutre `1`).

- Comme dans tout modèle de régression, l’effet d’une modalité de
  prédicteur sur la variable expliquée se lit « les autres prédicteurs
  choisis étant égaux ».
- Les couleurs se lisent comme dans n’importe quel tableau `tabxplor` :
  un rapport de cotes supérieur à 1 (bleu) signifie *plus susceptible
  d’être marié* que la modalité de référence, inférieur à 1 (rouge)
  signifie *moins susceptible* ; étoiles et couleurs signalent toutes
  deux la significativité.

``` r
tab_reg(gss_simple, "married", c("race", "age", "rincome", "relig"))
#> ℹ "married": binary outcome detected -> `family =
#> "binomial"` (logistic).
```

Régression logistique: married selon race, age +2 more

[TABLE]

Pour une régression logistique simple, `empirical = TRUE` ajoute les
**pourcentages bruts** et les **rapports de cotes bruts non modélisés**
pour chaque prédicteur.

``` r
tab_reg(gss_simple, "married", c("race", "age", "rincome", "relig"), empirical = TRUE)
#> ℹ "married": binary outcome detected -> `family =
#> "binomial"` (logistic).
```

Régression logistique: married selon race, age +2 more

[TABLE]

Les pourcentages bruts `Obs_%` sont un résumé des résultats observés de
base dont tout, dans le modèle, est dérivé : « 28 % des Américains noirs
sont mariés, contre 51 % pour les Américains blancs »

- La simple différence à la référence est colorée (28 % − 51 % = −23 %).
- Les cases qui ne diffèrent pas significativement de la référence sont
  grisées (d’après un intervalle de confiance de Newcombe pour les
  différences de proportions).

Les **rapports de cotes modélisés** `Model_OR` sont directement comparés
aux **rapports de cotes observés** `Obs_OR` :

- Comparer les deux vous dit quel ajustement le modèle a opéré : si l’OR
  **du modèle** d’un prédicteur est bien plus proche de 1 que son OR
  **brut**, l’association brute était largement expliquée par les autres
  prédicteurs.
- Ici, « toutes choses étant inégales par ailleurs », les Américains
  noirs ont des cotes d’être mariés 2,68 fois plus faibles que les
  Américains blancs. « Toutes choses égales par ailleurs » (plus
  précisément : à revenu, âge et religion égaux), les Américains noirs
  ont encore des cotes 2,4 fois plus faibles d’être mariés que les
  Américains blancs. Le résultat tient, il n’est pas expliqué par des
  différences de revenu ni de religion.

Les **rapports de cotes observés** `Obs_OR` sont les mêmes que ceux que
vous pouvez calculer à partir des seuls pourcentages dans un tableau
croisé :

- Les couleurs et les étoiles de significativité utilisent un
  **intervalle de confiance de Woolf pour l’OR** qui correspond à ce qui
  est fait dans le modèle de régression.
- La population du tableau doit correspondre à la population en cas
  complets du modèle, en filtrant les individus ayant un `NA` sur l’une
  des variables impliquées.

``` r
gss_simple |>
  dplyr::filter(dplyr::if_all(all_of(c("race", "age", "rincome", "relig")), ~ !is.na(.) )) |>
  tab(race, married, pct = "row", na = "drop",
    OR = "OR", color = "OR", color_signif = "grey_non_signif"
   )
```

[TABLE]

### Effets marginaux moyens (AME) d’une régression logistique

Une autre façon d’interpréter une régression logistique est d’utiliser
les **effets marginaux moyens** (AME) avec `effect = "ame"` :

- Au lieu de lire directement le rapport de cotes modélisé, on peut en
  déduire la différence moyenne de pourcentage de chaque modalité
  comparée à la référence.
- Ici `Model_AME` se lit ainsi : « En comparant des répondants noirs et
  blancs semblables en revenu, âge et religion, être noir est associé à
  un taux de mariage inférieur de 19,8 points, en moyenne. »

``` r
tab_reg(gss_simple, "married", c("race", "age", "rincome", "relig"), effect = "ame", empirical = TRUE)
#> ℹ "married": binary outcome detected -> `family =
#> "binomial"` (logistic).
```

Régression logistique: married selon race, age +2 more

[TABLE]

On peut alors utiliser `empirical = TRUE` pour comparer cette nouvelle
quantité modélisée à la **différence brute, observée, de pourcentages**
:

- Dans l’échantillon (`Obs_diff`), on observe un taux de mariages
  inférieur de 23 points pour les Américains noirs comparés aux
  Américains blancs (`28% - 51% = -23%`).
- La différence modélisée `Model_AME` est très proche, `-19,8 %`, donc
  très peu de cette différence entre noirs et blancs peut être expliquée
  par l’âge, le revenu ou la religion.

On peut aussi comparer le pourcentage brut `Obs_%` avec la **probabilité
ajustée par le modèle** — la valeur entre parenthèses dans la colonne
`Model_AME (adjusted %)` :

- Le pourcentage ajusté, qui est un résultat du modèle, se lit ainsi : «
  si tout l’échantillon gardait sa distribution de revenu/âge/religion
  mais que tout le monde était noir, on estime que 31,5 % seraient
  mariés, contre 51,3 % si tout le monde était blanc ».
- Les noirs se marient à 30,9 % dans cet échantillon (observé) ;
  standardiser leur revenu/âge/religion au mélange de la population ne
  déplace cela qu’à 31,5 % — donc presque rien de l’écart noir-blanc ne
  s’explique par des différences dans ces trois variables ; il persiste
  après ajustement.

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

[TABLE]

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

Régression logistique: score selon race, age

[TABLE]

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

Régression logistique ordinale: rincome selon race, age +1 more

[TABLE]

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

Régression logistique multinomiale: party3 selon race, age +2 more

[TABLE]

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

Régression logistique multinomiale: party3 selon race, age +2 more

[TABLE]

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

Régression linéaire: age selon race, marital +2 more

[TABLE]

Dans le cas d’une régression linéaire, la **contrepartie
empirique/observée du coefficient du modèle** pour un prédicteur
catégoriel est simplement la **différence de moyennes** : ici, la
différence d’âge moyen, par modalité du prédicteur, comparée à la
modalité de référence.

``` r
tab_reg(gss_simple, "age", c("race", "marital", "relig", "rincome"), family = "gaussian", empirical = TRUE)
```

Régression linéaire: age selon race, marital +2 more

[TABLE]

Les âges moyens empiriques, et les différences d’âge moyen à la
référence, peuvent être calculés dans un simple tableau avec :

``` r
tab(gss_simple, "race", "age", pct = "row", digits = 2, na = "drop",
    color = "diff", ref = 1,  method_mean_diff = "student"
) |>
  mutate(diff = set_display(age, "diff"))
```

[TABLE]

``` r
# student : intervalles de confiance a variance commune, pour correspondre a ceux calcules par la regression lineaire
```

## Régression de Poisson (une variable expliquée de comptage)

``` r
tab_reg(gss_simple, "tvhours", c("race", "marital", "relig", "rincome"), family = "poisson")
```

Régression de Poisson: tvhours selon race, marital +2 more

[TABLE]

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

Régression de Poisson: tvhours selon race, marital +2 more

[TABLE]

Les heures moyennes empiriques de télévision par jour, et les
différences de taux de visionnage à la référence, peuvent être calculées
dans un simple tableau avec :

``` r
tab(gss_simple, "race", "tvhours", pct = "row", digits = 2, na = "drop",
    color = "ratio", ref = 1,  method_mean_ratio = "quasipoisson"
) |>
  mutate(IRR = set_display(tvhours, "ratio"))
```

[TABLE]

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

Régression logistiques (comparaison de modèles) : married, ‘01-Married’
(OR)

[TABLE]

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

Régression logistique: married selon race, rincome (ventilé par year)

[TABLE]

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

Régression logistique: married selon race, age

[TABLE]

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

Régression logistique: married selon race, age

[TABLE]

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
