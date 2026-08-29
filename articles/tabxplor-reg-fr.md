# Modèles de régression avec tab_reg() : référence

``` r

library(tabxplor)
library(dplyr)

# Comme dans la vignette d'introduction, les tableaux sont rendus comme les vrais tableaux html
# de tabxplor (le reglage recommande au quotidien) ; la feuille de style partagee est emise une
# fois par tab_css() ci-dessous, et les infobulles restent coupees.
# Fixer la langue des DEUX manieres, comme les autres documents traduits : options(tabxplor.lang)
# pilote la legende des couleurs, la note de bas de tableau et le titre de regression, mais les
# libelles du resume de tests et des lignes de bilan du modele (reg_footer_spec /
# test_pvalue_descriptor / test_es_measure) passent par gettext, que seul LANGUAGE atteint. Sans
# lui, une ligne de diagnostic s'intitule "Linearity" et non "Linearite" des que le document est
# construit sur une machine anglaise. (La notation n'est deliberement PAS traduite : "LR vs null",
# OR/IRR/beta restent tels quels.)
options(tabxplor.print = "html")
options(tabxplor.tab_kable_css = FALSE)
options(tabxplor.tab_kable_tooltips = FALSE)

# Le tableau des formes qu'un prédicteur continu dessine sous le pied de tableau fait
# l'objet d'une section ci-dessous, et n'est que du bruit ailleurs : activé là, coupé ici.
options(tabxplor.shape_table = "no")

options(cli.num_colors = 256)
options(tabxplor.lang = "fr")
Sys.setenv(LANGUAGE = "fr")
set_color_palette(theme = "light")
```

Pour les modèles de régression les plus courants,
[`tab_reg()`](https://bricenocenti.github.io/tabxplor/reference/tab_reg.md)
construit un **tableau de régression** qui ressemble à un tableau croisé
`tabxplor` et se comporte de la même manière : une ligne par modalité de
prédicteur, des étoiles de significativité, des couleurs qui grisent les
effets non significatifs, et les mêmes exports Excel, html ou markdown.
On lui donne un data frame, une **variable à expliquer** / `outcome` et
des **prédicteurs** / `predictors`, et il déduit du type de la variable
à expliquer quel modèle convient. Sa particularité est
`empirical = TRUE`, qui affiche l’**effet observé / brut à côté de
l’effet ajusté du modèle** : on voit ainsi ce que « contrôler par les
autres variables » a réellement changé.

**Il y a deux documents sur la régression, et celui-ci est la
référence.** Il est organisé par fonctionnalité : une partie par
famille, la grille complète de ce que chaque modèle peut rapporter, les
données pondérées, les interactions, les vérifications du modèle et les
graphiques. Pour *apprendre* plutôt que pour chercher un point précis,
commencer par [Interpréter un modèle de
régression](https://bricenocenti.github.io/tabxplor/articles/tabxplor-reading-a-regression-fr.md),
qui suit une seule analyse du premier tableau croisé jusqu’à une phrase
finie, et qui renvoie ici pour les détails.

**Sans écrire aucun code R**, les mêmes modèles peuvent être réalisés
dans l’interface graphique [jamovi](https://www.jamovi.org/) qui vient
avec `tabxplor`. Ses boutons portent le nom des arguments ci-dessous et
tout peut y être reproduit.

Nous utilisons une version formatée des données
[`forcats::gss_cat`](https://forcats.tidyverse.org/reference/gss_cat.html),
issues du *General Social Survey* états-unien, et de la base de donnée
`tea` du package **FactoMineR::** (merci à leurs auteur·es).

``` r

gss_simple <- gss_cat_data_formatting()
```

``` r

tea_where_vars <- c("home", "work", "tearoom", "friends", "resto", "pub")
tea <- facto_tea |> score_from_lv1("tea_where", vars_list = tea_where_vars) # score sur 6
```

## 1. Ce qu’est un tableau de régression

### Le type de la variable à expliquer choisit le modèle, et la quantité observée à comparer

Il est rare d’avoir à fixer `family` à la main —
[`tab_reg()`](https://bricenocenti.github.io/tabxplor/reference/tab_reg.md)
le détecte le plus souvent :

| Variable à expliquer | Modèle détecté | La mesure dans laquelle ce modèle travaille |
|:---|:---|:---|
| facteur à 2 modalités | binomial (logistique) | rapport de cotes (`OR`) |
| numérique (continue) | gaussien (linéaire) | différence de moyennes (`diff`) |
| comptage | poisson | rapport de taux d’incidence (`IRR`) |
| facteur non ordonné à 3 modalités ou plus | multinomial | une colonne `OR` par catégorie, contre la référence |
| facteur ordonné à 3 modalités ou plus | ordinal (cotes proportionnelles) | `OR` cumulé, ou `D` de Somers |

Cette dernière colonne est la mesure que le modèle **estime**, non un
plafond : n’importe quelle autre mesure peut être *rapportée* depuis le
même ajustement, et `measure =` est la manière de la demander. Quatre
arguments s’enchaînent — `family` dit de quel genre de nombre il s’agit,
`link` quelle mesure le modèle estime, `measure` laquelle est rapportée,
`effect` d’où ce nombre est tiré — et chacun découle du précédent, sauf
mention contraire. La grille complète est dans la partie 2.

Avec `empirical = TRUE`, chaque colonne de modèle est accompagnée d’une
colonne brute/observée montrant l’effet *observé, non ajusté (à un seul
prédicteur)* — celui qu’on verrait sans aucun contrôle, « toutes choses
*inégales* par ailleurs », pour ce prédicteur. Les deux colonnes sont la
même colonne deux fois : une même quantité estimée avec un seul
prédicteur, puis avec tous. Même échelle de couleurs, même mise en page,
une seule légende. Chaque case imprime l’effet avec, à côté, le niveau
sur lequel il porte — le pourcentage ou la moyenne observés du côté
brut, la prédiction **ajustée** du côté du modèle —, si bien que les
deux effets se retrouvent côte à côte au milieu, et que la comparaison
se lit d’un trait.

Une seule règle couvre tous les cas : **l’effet observé est l’effet de
ce prédicteur seul**. Pour un prédicteur catégoriel, c’est exactement le
contraste observé entre modalités (une différence de pourcentages, un
rapport de cotes calculé à partir des pourcentages bruts). Pour un
prédicteur *continu*, il n’existe pas de tel raccourci : c’est la pente
du modèle à un seul prédicteur — ce qui suppose que l’effet est linéaire
sur l’échelle du modèle, hypothèse qu’il vaut mieux vérifier (avec
[`cut()`](https://rdrr.io/r/base/cut.html), ou des splines) avant de s’y
fier.

- 2 modalités, logistique (binomial) → pourcentages observés, et
  rapports de cotes observés
- 3 modalités ou plus, logistique (multinomial) → les OR observés sont
  affichés en infobulle sur les cases du modèle, dans les exports html
- gaussien (linéaire) → moyennes de groupe et leur différence
- poisson (comptages) → taux observé et rapport de taux observé

Un prédicteur continu n’a pas de modalités : sa case ne montre que
l’**effet** — il n’y a pas de pourcentage ni de moyenne observés à
mettre à côté —, et sa distribution (moyenne, écart-type, et moyenne
dans chaque groupe de la variable à expliquer) s’affiche en infobulle
html à la place.

Sur des données **pondérées**, une chose mérite d’être sue dès le départ
: une colonne observée est toujours mesurée exactement comme la colonne
de modèle placée à côté d’elle, si bien que son intervalle de confiance
tient compte de la pondération (et, sous un plan de sondage `survey`, de
tout le plan). Un tableau croisé de
[`tab()`](https://bricenocenti.github.io/tabxplor/reference/tab.md) ne
le fait pas par défaut : y ajouter `design_effect = TRUE` pour que ses
pourcentages soient directement comparables à ceux-ci. Voir *Données
pondérées et plans de sondage*, en partie 5.

### Toute variable à expliquer a sa contrepartie observée

`empirical = TRUE` répond à une seule question — **« combien de cette
association survit une fois ajusté sur le reste ? »** — et il fonctionne
pour tous les types de variable à expliquer. La règle sous-jacente tient
en une phrase :

> L’effet observé, c’est **l’effet du modèle lui-même, ajusté avec un
> seul prédicteur**.

Cela mérite d’être dit clairement, parce que c’est ce qui rend la
comparaison honnête. Le nombre brut n’est pas « un pourcentage » et
celui du modèle « un coefficient » : c’est la *même quantité*, calculée
de la même façon, sur les *mêmes personnes* (les mêmes cas complets), et
affichée sur la même échelle. Seule la liste des prédicteurs change. La
distance entre les deux mesure donc l’ajustement, et rien d’autre.

Les « mêmes personnes », c’est le comportement par défaut, pas un espoir
: `na = "drop_by_outcome"` donne à tous les modèles d’une même variable
à expliquer une seule population de cas complets, si bien qu’un modèle
comparé ne peut pas être estimé sur des lignes que les colonnes
observées ne couvrent pas. Avec `na = "drop_by_model"`, un modèle
portant sur une autre population n’obtient **aucun** effet observé :
mieux vaut une case vide qu’un « écart » qui n’est en réalité qu’une
différence de valeurs manquantes.

Un mot sur la lecture. La couleur dit de combien deux nombres diffèrent,
et le grisé si cette différence dépasse le bruit. Elle ne dit pas «
voici un facteur de confusion » : le premier seuil de 10 % est une
convention, pas une règle de décision, et une partie de l’écart entre
deux rapports de cotes est arithmétique plutôt que confusionnelle (voir
*Un avertissement sur les rapports de cotes*, en partie 4). « Cet effet
a été **atténué** par l’ajustement » est la lecture sûre ; « expliqué
par » ne l’est pas.

#### Une variable ordonnée : ce que change l’ajustement

Le revenu est ici un facteur **ordonné**, donc estimé comme un modèle à
cotes proportionnelles. `Obs_cumOR` est le même modèle, avec un
prédicteur à la fois :

``` r

tab_reg(gss_simple, "rincome", c("race", "relig"),
        empirical = TRUE, color = c(TRUE, "adjustment"))
```

Ordinal logistic regression: rincome by race, relig

[TABLE]

Lire une ligne de gauche à droite : le rapport de cotes cumulé observé,
puis celui du modèle, le **fond** colorant l’écart entre les deux. Être
noir est associé à un revenu plus faible (`1/1.54` observé), et ajuster
sur la religion ne le bouge presque pas (`1/1.51`) : l’association n’est
pas expliquée par la religion. À comparer avec « bouddhiste/hindouiste
», dont l’effet *grandit* une fois la couleur de peau et les autres
groupes tenus constants : l’ajustement peut renforcer une association
aussi facilement qu’il peut en dissoudre une.

Sur cette échelle, la couleur de fond **n’est pas un test** : un rapport
de cotes bouge dès qu’on ajoute un prédicteur fort, ce qui relève de
l’arithmétique et non de la confusion. Demander `measure = "difference"`
et la même comparaison se lit sur le `D` de Somers, où elle *en est* un
— la valeur brute est alors la probabilité de supériorité du tableau
croisé lui-même, calculée sur les effectifs sans aucun modèle, si bien
que la distance à la valeur modélisée mesure l’ajustement et rien
d’autre :

``` r

tab_reg(gss_simple, "rincome", c("race", "relig"), measure = "difference",
        empirical = TRUE, display = "est_base", color = c(TRUE, "adjustment"))
```

Ordinal logistic regression: rincome by race, relig

[TABLE]

#### Une variable nominale : le nombre brut tient dans la case

Un modèle multinomial dépense déjà une colonne par catégorie de la
variable à expliquer : un second jeu de colonnes doublerait le tableau.
L’effet observé est donc imprimé **dans la case**, entre parenthèses :

``` r

tab_reg(gss_simple, "party3", c("race", "relig"), family = "multinomial",
        empirical = TRUE, color = c(TRUE, "adjustment"))
```

Multinomial logistic regression: party3 by race, relig

[TABLE]

`1/2.46 (1/2.43)` veut dire « modélisé `1/2.46`, observé `1/2.43` » :
rien à signaler. `1/12.48 (1/10.17)` veut dire que l’ajustement a un peu
éloigné l’effet de 1. La note de bas de tableau dit quel nombre est
lequel, et le survol d’une case (dans un export html) ajoute les
pourcentages bruts qui sont derrière.

#### Un score sommé

Avec `trials =`, la colonne brute donne le rapport de cotes des items
sommés, `Obs_OR`, avec à côté le **score moyen** observé — le nombre
moyen de lieux sur six. La colonne du modèle donne les deux mêmes
quantités ajustées, si bien que la paire se lit d’un trait.

``` r

tab_reg(tea, "tea_where", c("sex", "SPC", "Sport"),
        family = "binomial", trials = length(tea_where_vars),
        empirical = TRUE, color = c(TRUE, "adjustment"))
```

Logistic regression: tea_where by sex, SPC +1 more

[TABLE]

#### Comment le lire — et comment ne pas le lire

Un écart est un indice, pas un verdict : il vaut pour *ces* variables de
contrôle, mesurées *ainsi*, et les deux nombres sont des estimations —
un nombre brut reposant sur peu de personnes fluctue, et l’écart fluctue
avec lui. [Interpréter un modèle de
régression](https://bricenocenti.github.io/tabxplor/articles/tabxplor-reading-a-regression-fr.md)
est un article entier consacré à la lecture de cette distance.

Une habitude est propre à l’échelle, et elle décide de ce que le tableau
permettra de faire. Un rapport de cotes change quand on ajoute un
prédicteur **même si ce prédicteur n’a rien à voir avec l’exposition** —
la non-collapsibilité — si bien qu’une part de chaque écart, sur une
colonne de rapports de cotes, est de l’arithmétique et non de la
confusion : les couleurs y restent purement descriptives. Pour un écart
qu’on puisse *tester*, demander une mesure collapsible : des points de
pourcentage, ou un risque relatif.

``` r

tab_reg(gss_simple, "party3", c("race", "relig"), family = "multinomial",
        measure = "difference", empirical = TRUE, color = c(TRUE, "adjustment"),
        color_signif = "grey_non_signif")
```

Multinomial logistic regression: party3 by race, relig

[TABLE]

La case se lit maintenant « effet modélisé en points de pourcentage,
effet observé entre parenthèses », et un fond qui reste gris signifie
que les deux ne se distinguent pas du bruit.

## 2. Les quatre arguments : `family`, `link`, `measure`, `effect`

Quatre arguments décident de ce que contient une colonne de modèle, et
ils **s’enchaînent** — chacun découle du précédent sauf mention
contraire, ce qui est le sens de `"auto"` sur les quatre :

``` text
variable à expliquer ──auto──▶ family ──auto──▶ link ──auto──▶ measure ──auto──▶ effect
```

**Une seule règle les sous-tend, et c’est elle qui diffère de
[`tab()`](https://bricenocenti.github.io/tabxplor/reference/tab.md).**
Seul **`link`** change le modèle estimé. `measure` et `effect` changent
ce qu’on y *lit*, et `display` ne change que ce que la case **affiche**.
Dans un tableau croisé, chaque mesure vient des mêmes effectifs :
demander un rapport plutôt qu’une différence y est un choix d’affichage.
Dans une régression, c’est une étape de calcul — le modèle dérive la
mesure demandée de ses prédictions — mais c’est toujours le *même
ajustement*. Un risque relatif et un rapport de cotes peuvent donc être
deux lectures d’un même modèle logistique, et un *autre* modèle est
quelque chose qu’on demande par son nom.

### Les quatre arguments, en mots simples

| argument | la question à laquelle il répond | `"auto"` donne | qui le fixe |
|:---|:---|:---|:---|
| `family` | de quel genre de nombre il s’agit | détecté depuis la variable | soi-même, si elle est numérique |
| `link` | quelle mesure le **modèle** estime | celle propre à la famille | rarement — experts |
| `measure` | quelle mesure est **rapportée** | celle du lien | **c’est celui qu’on fixe** |
| `effect` | d’où ce nombre est tiré | un coefficient s’il en existe | rarement ; pour un idéal-type |

Les trois valeurs d’`effect` nomment trois quantités :

| `effect` | la question à laquelle il répond | son nom dans la littérature |
|----|----|----|
| `"conditional"` | deux personnes semblables sur tous les autres prédicteurs | effet conditionnel (le coef.) |
| `"marginal"` | si tout le monde en changeait, de combien cela bougerait-il ? | effet marginal moyen (AME) |
| `"at_reference"` | la même chose, pour une personne au profil de référence | effet au profil de référence |

À la main, on les obtiendrait avec `coef(glm(...))` (exponentié quand la
mesure est un rapport), avec `marginaleffects::avg_comparisons(model)`,
et avec le même appel sur un profil d’une seule ligne où chaque autre
prédicteur est à sa modalité de référence ou à sa moyenne. On n’en
tapera presque jamais aucun : `"auto"` choisit `"conditional"` chaque
fois que la mesure rapportée *est* celle du modèle — c’est-à-dire
exactement quand un coefficient existe — et `"marginal"` sinon.
`"conditional"` gagne sa place comme une assertion : le demander là où
aucun coefficient ne peut porter la mesure voulue fait que le tableau le
dit, en nommant les deux remèdes, au lieu de donner discrètement autre
chose.

⚠ **Une clause qualifie `"auto"` : il ne tombe jamais sur un rapport de
cotes *prédit*.** Un rapport de cotes marginal est une quantité de
spécialiste (Karlson & Jann 2023) : sur une voie de prédiction, `"auto"`
revient donc à la mesure propre à la variable à expliquer — « x fois
plus souvent » pour un pourcentage. Il faut nommer
`measure = "odds_ratio"` pour l’obtenir.

**`effect = "at_reference"`** évalue l’effet au profil de référence —
l’individu abstrait qui combine la modalité de référence de chaque
prédicteur — au lieu de le moyenner sur l’échantillon. Pour un modèle
multinomial, il donne en outre accès à un contraste que les deux autres
n’ont pas : le rapport de cotes de chaque catégorie de la variable à
expliquer *contre toutes les autres*, à ce profil.

``` r

tab_reg(gss_simple, "married", c("race", "age"), effect = "at_reference")
```

### Avec quel modèle une variable à expliquer peut être estimée

`link` est l’ensemble des modèles disponibles. Ses valeurs sont les mots
mêmes de `measure`, parce qu’un lien **est** une mesure — celle que le
modèle estime directement — si bien que le vocabulaire du statisticien
n’affleure jamais. † marque le lien propre à la famille, celui auquel
`link = "auto"` se résout.

| variable à expl. | `family` | une case est… | `link =` | le modèle estimé | son coef. |
|----|----|----|----|----|----|
| facteur, 2 mod. | `binomial` | un pourcent. | `"odds_ratio"` † | régression logistique | `OR` |
|  |  |  | `"ratio"` | Poisson modifiée, ET rob. | `RR` |
|  |  |  | `"difference"` | binomiale à lien identité | `RD` |
| numérique, score | `binomial`+trials | un pourcent. | les trois mêmes | les mêmes, par item | `OR`/`RR`/`RD` |
| numérique | `gaussian` | une moyenne | `"difference"` † | régression linéaire | `diff` |
|  |  |  | `"ratio"` | pseudo-Poisson à lien log | `RoM` |
| numér., comptage | `poisson` | un comptage | `"ratio"` † | Poisson, ET quasi-Poisson | `IRR` |
| facteur, 3+ n.o. | `multinomial` | un pourcent. | `"odds_ratio"` † | logit multinomial | `OR` |
| facteur ordonné | `ordinal` | un rang | `"odds_ratio"` † | modèle à cotes proport. | `cumOR` |

`family = "quasipoisson"` a exactement la ligne de `poisson` — elle
change l’hypothèse de variance, pas ce qui est estimé.

### Ce que rapporte chaque mesure, depuis n’importe lequel d’entre eux

Quelles mesures existent est décidé par **ce qu’est une case** : un
pourcentage a une identité, un log et un logit, donc les trois ; une
moyenne ou un comptage n’a pas de cotes, donc pas de rapport de cotes ;
et un **rang** — la case d’une variable ordonnée — se compare par paires
de personnes plutôt que par parts, ce qui est précisément ce qui permet
à un modèle ordinal de rapporter en une seule colonne. Chaque case
ci-dessous est l’acronyme que la combinaison place dans l’en-tête de
colonne, après le préfixe constant `Model_`.

| une case est… | `measure =` | `"conditional"` \* | `"marginal"` | `"at_reference"` |
|----|----|----|----|----|
| un pourcentage | `"odds_ratio"` | `OR` | `mOR` ¹ | `refOR` ¹ |
| un pourcentage | `"ratio"` | `RR` | `mRR` | `refRR` |
| un pourcentage | `"difference"` | `RD` | `mRD` | `refRD` |
| une moyenne | `"difference"` | `diff` | `mdiff` | `refdiff` |
| une moyenne | `"ratio"` | `RoM` | `mRoM` | `refRoM` |
| une moyenne | `"odds_ratio"` | non défini | non défini | non défini |
| un comptage | `"ratio"` | `IRR` | `mIRR` | `refIRR` |
| un comptage | `"difference"` | — | `mdiff` | `refdiff` |
| un comptage | `"odds_ratio"` | non défini | non défini | non défini |
| un rang | `"difference"` | — | `mD` | non proposé ² |
| un rang | `"ratio"` | — | `mWR` | non proposé ² |
| un rang | `"odds_ratio"` | `cumOR` | non proposé ¹ | non proposé ¹ |

La colonne `"conditional"` est celle que
[`reg_measures()`](https://bricenocenti.github.io/tabxplor/reference/reg_measures.md)
donne par **lien** ; les deux autres sont les mêmes pour tous les liens
que la famille estime, et c’est pourquoi elle ne les liste qu’une fois.

\* un coefficient n’existe **que là où cette mesure est le `link`** —
voir le tableau précédent. En demander un qui ne l’est pas fait refuser
l’appel, en nommant ses deux remèdes : retirer `effect`, ou estimer le
modèle qui l’estime.

¹ un rapport de cotes prédit a besoin d’un pourcentage **et de son
complément** : une variable à 3 catégories ou plus doit donc d’abord
répondre à « contre quoi ? ». Sur une variable multinomiale il n’existe
que comme contraste *contre le reste* avec `effect = "at_reference"`, et
sur une variable ordinale pas du tout.

² les mesures d’un rang comparent deux personnes **tirées de la
population**, et un profil n’en contient qu’une.

`reg_measures(data, outcome)` affiche cette grille pour **sa propre**
variable à expliquer, et n’affiche que ce qui est constructible : une
combinaison *non définie* n’a pas de ligne, et le message au-dessus du
tableau dit pourquoi. Ses deux refus ne sont pas le même — une quantité
n’a pas de cotes dont prendre le rapport, quoi qu’on implémente, tandis
que *non proposé* signifie que tabxplor ne le construit pas et que
l’erreur liste ce que cette variable à expliquer offre, sur ce modèle et
sur les autres.

Un état de plus n’existe qu’à l’exécution : un lien qui ne converge pas
sur les données qu’on a. L’appel le dit, et pour la différence de
proportion il bascule sur le modèle de probabilité linéaire.

`measure = "raw_coefficient"` n’est pas une quatrième colonne : c’est
**le coefficient du modèle lui-même**, la quantité estimée montrée non
transformée. Là où la mesure rapportée est multiplicative, c’est son
logarithme, et l’en-tête dit lequel il logarithme (`Model_log(OR)`,
`Model_log(IRR)`, `Model_log(RoM)`) ; là où le modèle est déjà additif,
il n’y a rien à dé-exponentier et le coefficient EST l’estimation
additive que la colonne montre déjà. Il répond donc pour toutes les
familles — ce qui permet de demander ses coefficients à un tableau
mêlant une variable à expliquer logistique et une variable linéaire.
Étant le nombre *propre* de l’ajustement, c’est toujours le coefficient
conditionnel : demandé avec `effect = "marginal"`, l’appel le dit et
nomme le remède. (`"coefficient"`, `"coef"`, `"log"`, `"log_odds"`,
`"log_risk"` et `"log_rate"` sont des orthographes acceptées ; les trois
dernières désignent laquelle.)

### Ce que veulent dire les en-têtes

Un en-tête nomme la **mesure** ; le **contraste** est une marque posée
dessus. Il y a donc un seul acronyme à chercher par quantité, et une
marque à lire par-dessus.

| acronyme | la quantité | comment l’obtenir à la main |
|----|----|----|
| `OR` | rapport de cotes | `exp(coef(glm(y ~ ., binomial)))` |
| `cumOR` | rapport de cotes cumulé | `exp(coef(MASS::polr(...)))`, le modèle à cotes proportionnelles |
| `RR` | risque relatif | Poisson modifiée, ET robuste (Zou 2004) |
| `RD` | différence de proportion, en points | `glm(y ~ ., binomial("identity"))`, ET robuste |
| `IRR` | rapport de taux d’incidence | `exp(coef(glm(y ~ ., poisson)))` |
| `RoM` | rapport de moyennes | pseudo-Poisson à lien log, ET robuste (Santos Silva & Tenreyro 2006) |
| `diff` | différence de moyennes | `coef(lm(...))` |

| marque | se lit | exemple |
|----|----|----|
| *(aucune)* | conditionnel — les autres prédicteurs tenus fixes | `Model_OR` |
| préfixe `m` | marginal — moyenné sur l’échantillon | `Model_mRR` |
| préfixe `ref` | au profil de référence | `Model_refRD` |
| `log(…)` | la même quantité estimée, non exponentiée | `Model_log(OR)` |

La contrepartie observée porte la mesure **sans** marque (`Obs_RR` à
côté de `Model_mRR`) : c’est un effet à un seul prédicteur, et là où ses
niveaux viennent des effectifs eux-mêmes, un contraste marginal et un
contraste conditionnel sont le même nombre.

### Le seul argument qui change le modèle : `link`

Tout le reste change ce qui est rapporté. `link` change ce qui est
estimé, et c’est là toute la différence — c’est donc le seul argument
qui puisse glisser dans un tableau une hypothèse qu’on n’avait pas
voulue. Chaque lien achète un coefficient directement lisible au prix
d’une hypothèse :

| `family` et `link` | ce qui est estimé | ce que le coefficient suppose |
|----|----|----|
| binomial · `"odds_ratio"` *(défaut)* | régression logistique | un rapport de cotes constant d’un profil à l’autre |
| binomial · `"ratio"` | Poisson modifiée, ET robuste | un risque relatif constant |
| binomial · `"difference"` | binomiale à lien identité, ET robuste | une différence de proportion constante |
| gaussian · `"difference"` *(défaut)* | régression linéaire | une différence de moyennes constante |
| gaussian · `"ratio"` | pseudo-MV de Poisson à lien log, ET robuste | E(y) = exp(xβ), un rapport de moyennes constant |
| poisson · `"ratio"` *(défaut)* | Poisson, ET quasi-Poisson | un rapport de taux constant |
| ordinal · `"odds_ratio"` *(défaut)* | modèle à cotes proportionnelles | un seul rapport de cotes, à chaque seuil |
| multinomial · `"odds_ratio"` *(défaut)* | logit multinomial | un rapport de cotes constant par catégorie |

Ce que dit la littérature sur les trois liens non-défaut :

- **Le risque relatif est une voie bien établie, pas un bricolage.**
  `link = "ratio"` sur une variable binaire estime la Poisson modifiée
  de Zou, qui existe précisément pour éviter les échecs de convergence
  d’un ajustement log-binomial, et qui est une pratique standard en
  épidémiologie. Ses limites propres sont d’une autre nature que celles
  de la différence de proportion : les risques ajustés ne sont pas
  bornés par 1, et sur de petits effectifs ou des données creuses
  l’estimation comme l’erreur-type sandwich sont biaisées — des
  variantes pénalisées existent pour ce cas.
- **La différence de proportion est la voie fragile.** Un lien identité
  n’est borné d’aucun côté : il peut prédire des risques impossibles et
  tout simplement ne pas converger. Le cas échéant, tabxplor estime le
  modèle de probabilité linéaire à la place et le bas de tableau le dit
  — les deux visent la même quantité, mais ce sont des estimateurs
  différents, qui ne coïncident que si le modèle est juste.
- **Le rapport de moyennes est consistant dès que la fonction de moyenne
  est juste.** La pseudo-vraisemblance de Poisson ne prétend pas que la
  variable à expliquer est un comptage : c’est un dispositif pour le
  lien log, et son erreur-type robuste n’exige pas que la variance de
  Poisson soit correcte. Rien, en revanche, ne rattrape une mauvaise
  fonction de moyenne — et la variable à expliquer doit être positive ou
  nulle.
- **Le défaut n’est pas non plus sans hypothèse.** Un coefficient
  logistique suppose un rapport de cotes constant, un coefficient
  ordinal le même rapport de cotes à chaque seuil (le test de Brant, en
  bas de tableau, le vérifie), et le rapport de cotes n’est pas
  collapsible : il bouge quand on ajoute une covariable même là où il
  n’y a rien à confondre.

**Quelle voie choisir.** Si c’est un risque relatif ou une différence de
proportion qu’on veut *rapporter*, le choix le plus sûr est de laisser
le modèle sur l’échelle propre à sa famille et de nommer la mesure avec
`measure =`. Trois raisons, dont aucune n’est affaire de goût : le logit
converge toujours et ne peut jamais prédire une probabilité hors de
0–100 %, alors que les liens log et identité peuvent faire l’un et
l’autre ; un effet marginal n’impose aucune hypothèse de constance sur
l’échelle rapportée, puisqu’il moyenne l’effet que le modèle estimé
implique réellement au profil de chaque répondant ; et tous les
`measure` tournent sur le même ajustement, si bien que changer la mesure
montrée ne change jamais ce qui a été estimé.

Cela dit, ce sont deux questions différentes plutôt que deux écritures
d’une seule. Un coefficient est un effet **conditionnel** — « deux
personnes semblables sur tous les autres prédicteurs » — et un effet
marginal une moyenne de population. Prendre la voie `link` quand c’est
la quantité conditionnelle qu’on veut : pour s’aligner sur une
estimation conditionnelle publiée, ou quand « l’effet est de la même
taille partout sur cette échelle » est bien ce qu’on entend affirmer.
[`reg_formulas()`](https://bricenocenti.github.io/tabxplor/reference/reg_formulas.md)
dit quel modèle a tourné.

### Précautions à connaître

- **Une variable binaire avec `family = "poisson"` n’est pas un modèle
  de comptage**, et l’appel est refusé plutôt que réécrit en douce.
  `family` dit de quel genre de nombre il s’agit et ne choisit jamais un
  lien à l’insu de qui appelle ; le message nomme les deux choses que
  cette écriture pouvait vouloir dire — `link = "ratio"` (le risque
  relatif *conditionnel* de la Poisson modifiée) et `measure = "ratio"`
  (le *marginal*).
- **`link = "ratio"` sur une variable numérique s’arrête sur une
  variable négative.** Un rapport de moyennes n’y est pas défini ;
  l’appel suggère de modéliser le
  [`log()`](https://rdrr.io/r/base/Log.html) d’une variable positive, ou
  de laisser `link` tranquille.
- **`Model_RD` et `Model_mRD` sont tous deux en points de pourcentage,
  et ce ne sont pas le même nombre.** `Model_RD` est une différence de
  proportion conditionnelle issue d’un ajustement à lien identité
  (`link = "difference"`), `Model_mRD` une différence marginale
  standardisée sur l’ajustement logistique (`measure = "difference"`) —
  ce que dit la marque `m`.
- **Les rapports de cotes conditionnels ne sont pas collapsibles** :
  `color = "adjustment"` colore donc l’écart sur une telle colonne mais
  ne le teste jamais (voir *Comment le lire — et comment ne pas le
  lire*, en partie 1). Demander `measure = "difference"` ou
  `measure = "ratio"` — ou, s’il faut publier des rapports de cotes,
  `measure = "odds_ratio", effect = "marginal"`, dont le rapport de
  cotes *marginal* est collapsible et dont le tableau teste bien
  l’écart.
- **Une combinaison n’a pas de contrepartie observée.** Avec
  `effect = "at_reference"`, le modèle est conditionnel à un profil
  alors que les colonnes observées restent marginales sur tout
  l’échantillon : les deux sont montrées mais pas comparées —
  `color = "adjustment"` et `{obs}` restent vides.
- **Ce qui demande un paquet en plus, et ce qui n’a pas de méthode.**
  `effect = "at_reference"` passe par **marginaleffects** ; un modèle
  multinomial *pondéré* n’a aucune méthode d’effets marginaux : il ne
  s’y lit donc que sur ses coefficients — garder la mesure propre au
  modèle et l’appel passe. Un modèle **ordinal** pondéré fait exception
  : ses mesures de rang (`measure = "difference"` / `"ratio"`) tournent
  sur la g-computation propre à tabxplor et prennent la variance fondée
  sur le plan directement dans l’ajustement, si bien qu’elles
  fonctionnent sous n’importe quel plan de sondage — seul le *test* de
  l’écart brut-ajusté y est indisponible, et `color = "adjustment"`
  colore alors sans tester. (`display`, lui, atteint toutes les
  colonnes, marginales ou non.)

### Interroger sa propre variable à expliquer

[`reg_measures()`](https://bricenocenti.github.io/tabxplor/reference/reg_measures.md)
affiche cette grille pour une variable à expliquer de ses propres
données — elle lit la table même contre laquelle
[`tab_reg()`](https://bricenocenti.github.io/tabxplor/reference/tab_reg.md)
se résout, donc ce qu’elle affiche est ce que la fonction fait :

``` r

reg_measures(gss_simple, "married")
```

``` r-output
#> # A tibble: 4 × 5
#>   link       measure    effect                header    reads_as                
#>   <chr>      <chr>      <chr>                 <chr>     <chr>                   
#> 1 odds_ratio odds_ratio conditional           Model_OR  odds ratio              
#> 2 (any)      difference marginal|at_reference Model_mRD marginal risk difference
#> 3 (any)      ratio      marginal|at_reference Model_mRR marginal risk ratio     
#> 4 (any)      odds_ratio marginal|at_reference Model_mOR marginal odds ratio
```

Elle vient en **deux blocs**. Le premier donne le modèle lui-même : son
`link`, et la mesure que portent ses coefficients. Le second donne ce
qui se lit sur les *prédictions* du modèle — les mêmes quel que soit le
lien estimé, d’où le `link = "(any)"` de ces lignes. Ce que change
`link`, c’est donc seulement quelle mesure a un **coefficient** ; toute
mesure reste rapportable depuis n’importe lequel.

Par défaut, le premier bloc ne contient que le modèle propre à chaque
famille. `link = "all"` ajoute tous les autres liens auxquels elle peut
être estimée — des choix de spécialiste — et signale le modèle de base
par la colonne `base_link` :

``` r

reg_measures(gss_simple, "married", link = "all")
```

``` r-output
#> # A tibble: 6 × 6
#>   link       base_link measure    effect                header    reads_as      
#>   <chr>      <lgl>     <chr>      <chr>                 <chr>     <chr>         
#> 1 odds_ratio TRUE      odds_ratio conditional           Model_OR  odds ratio    
#> 2 ratio      FALSE     ratio      conditional           Model_RR  risk ratio    
#> 3 difference FALSE     difference conditional           Model_RD  risk differen…
#> 4 (any)      NA        difference marginal|at_reference Model_mRD marginal risk…
#> 5 (any)      NA        ratio      marginal|at_reference Model_mRR marginal risk…
#> 6 (any)      NA        odds_ratio marginal|at_reference Model_mOR marginal odds…
```

Nommer un lien pour ne lire la table qu’à ce modèle, et une famille pour
la lire comme cette sorte de variable à expliquer :

``` r

reg_measures(gss_simple, "married", link = "ratio")
```

``` r-output
#> # A tibble: 4 × 5
#>   link  measure    effect                header    reads_as                
#>   <chr> <chr>      <chr>                 <chr>     <chr>                   
#> 1 ratio ratio      conditional           Model_RR  risk ratio              
#> 2 (any) difference marginal|at_reference Model_mRD marginal risk difference
#> 3 (any) ratio      marginal|at_reference Model_mRR marginal risk ratio     
#> 4 (any) odds_ratio marginal|at_reference Model_mOR marginal odds ratio
```

La même liste est dans
[`?tab_reg`](https://bricenocenti.github.io/tabxplor/reference/tab_reg.md),
section *« Which models each outcome offers, and which measures »*,
générée depuis la même source.

## 3. Une partie par type de variable à expliquer

### Régression logistique (un facteur binaire)

Quand la variable à expliquer est un facteur à deux modalités, ici «
married » contre « not married »,
[`tab_reg()`](https://bricenocenti.github.io/tabxplor/reference/tab_reg.md)
choisit une famille **binomiale** pour estimer une régression
logistique, et rapporte des **rapports de cotes** (la modalité de
référence de chaque prédicteur affiche la valeur neutre `1`).

- Comme dans tout modèle de régression, l’effet d’une modalité de
  prédicteur sur la variable à expliquer se lit « les autres prédicteurs
  choisis étant égaux ».
- Les couleurs se lisent comme dans n’importe quel tableau `tabxplor` :
  un rapport de cotes supérieur à 1 (bleu) signifie *plus de chances
  d’être marié·e* que la modalité de référence, inférieur à 1 (rouge)
  signifie *moins de chances* ; étoiles et couleurs signalent toutes
  deux la significativité.

``` r

tab_reg(gss_simple, "married", c("race", "age", "rincome", "relig"))
```

Logistic regression: married by race, age +2 more

[TABLE]

Pour une régression logistique simple, `empirical = TRUE` ajoute les
**rapports de cotes bruts, non modélisés**, chacun avec le **pourcentage
brut** dont il est calculé.

``` r

tab_reg(gss_simple, "married", c("race", "age", "rincome", "relig"), empirical = TRUE)
```

Logistic regression: married by race, age +2 more

[TABLE]

Les pourcentages bruts, entre parenthèses dans la colonne `Obs_OR`, sont
les résultats réellement observés dont tout, dans le modèle, est dérivé
: « 31 % des Américains noirs sont mariés, contre 52 % des Américains
blancs ».

- La simple différence à la modalité de référence est colorée (31 % − 52
  % = −21 points).
- Les cases qui ne diffèrent pas significativement de la référence sont
  grisées (d’après un intervalle de confiance de Newcombe pour les
  différences de proportions).

Les **rapports de cotes modélisés** `Model_OR` sont directement comparés
aux **rapports de cotes observés** `Obs_OR` :

- Comparer les deux montre quel ajustement le modèle a opéré : si l’OR
  **du modèle** d’un prédicteur est bien plus proche de 1 que son OR
  **brut**, l’association brute était largement expliquée par les autres
  prédicteurs.
- Ici, « toutes choses *inégales* par ailleurs », les Américains noirs
  ont `1/2.43` les cotes d’être mariés des Américains blancs. « Toutes
  les autres variables choisies étant égales » (plus précisément : à
  revenu, âge et religion égaux), ils en ont encore `1/2.40`. Le
  résultat tient : il n’est pas expliqué par des différences de revenu
  ni de religion.

Les **rapports de cotes observés** `Obs_OR` sont les mêmes que ceux
qu’on calcule à partir des seuls pourcentages d’un tableau croisé :

- Les couleurs et les étoiles de significativité utilisent un
  **intervalle de confiance de Woolf pour l’OR**, qui correspond à ce
  que fait le modèle de régression.
- La population du tableau doit correspondre à la population des cas
  complets du modèle : on filtre donc les individus ayant un `NA` sur
  l’une des variables impliquées.

``` r

gss_simple |>
  dplyr::filter(dplyr::if_all(all_of(c("race", "age", "rincome", "relig")), ~ !is.na(.) )) |>
  tab(race, married, pct = "row", na = "drop",
    display = "{or}", ref = "first", color = "odds_ratio", color_signif = "grey_non_signif"
   )
```

[TABLE]

#### Lire le même modèle dans une autre mesure

Un modèle logistique travaille en rapports de cotes, mais un rapport de
cotes est une piètre chose à dire à voix haute. Il n’est pas nécessaire
de quitter le modèle pour obtenir mieux : `measure =` demande une autre
mesure, et une seule règle couvre tous les cas :

> **Si la mesure demandée est celle dans laquelle le modèle travaille
> déjà, on lit le coefficient propre du modèle. Sinon, le modèle calcule
> la mesure demandée à partir de ses prédictions — pour chaque personne
> du fichier — et en fait la moyenne.**

Cette seconde opération est ce que les statisticiens appellent un effet
**marginal**, et l’en-tête de colonne dit lequel on a obtenu : un
`Model_OR` sans marque est un coefficient, le petit **`m`** de
`Model_mRD` signifie *calculé puis moyenné*. Comme `measure` laissé
tranquille donne le coefficient propre au modèle, toute autre mesure
demandée est un effet marginal.

##### Des points de pourcentage : `measure = "difference"`

La plus parlante des trois, parce que les points sont l’unité dans
laquelle on pense déjà. `Model_mRD` est l’**effet marginal moyen** (AME)
: la probabilité prédite par le modèle est calculée pour chaque
répondant comme s’il était à la modalité, puis à la référence, et les
deux moyennes sont soustraites.

``` r

tab_reg(gss_simple, "married", c("race", "age", "rincome", "relig"),
        measure = "difference", empirical = TRUE)
```

Logistic regression: married by race, age +2 more

[TABLE]

Lire le bloc `race` : « en comparant des répondants noirs et blancs
semblables en revenu, âge et religion, être noir est associé à un taux
de mariage inférieur de **19,8 points**, en moyenne ». La colonne brute
`Obs_RD` placée à côté est la simple différence observée de
pourcentages, `-21.2` — très proche, donc très peu de cet écart
s’explique par l’âge, le revenu ou la religion. Les **pourcentages
ajustés**, entre parenthèses, le disent dans l’autre sens : les
répondants noirs se marient à `30.9%` dans l’échantillon, et
standardiser leur revenu, leur âge et leur religion à la structure de la
population ne déplace cela qu’à `31.5%`, contre `51.3%` si tout le monde
était blanc.

##### « Combien de fois plus souvent ? » : `measure = "ratio"`

Un rapport de cotes n’est **pas** un « combien de fois plus souvent ».
Les deux ne se confondent que si le phénomène est **rare** ; au-delà
d’environ 10 %, ce qui couvre la plupart des questions d’enquête, le
rapport de cotes s’éloigne de 1 bien plus que le rapport des
probabilités. Et un rapport de cotes est *non collapsible* : il bouge
quand on ajoute une covariable même si celle-ci n’est pas un facteur de
confusion, si bien que comparer `Model_OR` entre modèles emboîtés n’est
pas valide. Les risques relatifs n’ont pas ce défaut.

``` r

tab_reg(gss_simple, "married", c("race", "age", "rincome", "relig"),
        measure = "ratio", empirical = TRUE)
```

Logistic regression: married by race, age +2 more

[TABLE]

`Model_mRR` se lit simplement : des répondants noirs et blancs
semblables en revenu, âge et religion ont environ **1,6 fois moins de
chances** d’être mariés. La case est cohérente par construction : la
probabilité de référence divisée par le risque relatif donne celle de la
modalité (`51% ÷ 1.6 ≈ 32%`), contrepartie multiplicative de l’identité
additive ci-dessus.

##### Le rapport de cotes, rendu comparable : `measure = "odds_ratio"` sur une voie de prédiction

Si la discipline dans laquelle on publie attend des rapports de cotes,
on n’est pas coincé avec le rapport conditionnel. Demander le rapport de
cotes *des deux prédictions ajustées* donne un **rapport de cotes
marginal** (`Model_mOR`), qui garde la lecture en effet relatif tout en
se comportant comme un effet marginal d’un modèle à l’autre (Karlson &
Jann 2023) :

``` r

tab_reg(gss_simple, "married", c("race", "age", "rincome", "relig"),
        measure = "odds_ratio", effect = "marginal", empirical = TRUE)
```

Logistic regression: married by race, age +2 more

[TABLE]

`1/2.29` contre le `1/2.40` conditionnel plus haut — un peu plus proche
de 1, ce qui est la non collapsibilité qui se montre. Cela compte
surtout pour une chose : comme cette mesure est collapsible,
`color = "adjustment"` **teste** l’écart brut-ajusté sur cette colonne,
là où sur un rapport de cotes conditionnel il ne peut que le colorer
(voir *Un avertissement sur les rapports de cotes*, en partie 4). Elle
est disponible sur une variable binaire et sur un score sommé ; une
variable à 3 catégories ou plus doit d’abord répondre à « contre quoi ?
», si bien qu’elle n’y existe qu’avec `effect = "at_reference"`.

⚠ Il faut la nommer : `"auto"` ne tombe jamais sur un rapport de cotes
*prédit*, parce que c’est une quantité de spécialiste, qui doit être
demandée plutôt qu’atteinte par accident.

#### L’autre risque relatif : `link = "ratio"`

Tout ce qui précède changeait ce qui est **rapporté**, jamais ce qui est
**estimé**. Un seul argument change le modèle lui-même, et il donne
l’*autre* risque relatif — celui dont parlent les épidémiologistes :

``` r

tab_reg(gss_simple, "married", c("race", "age", "rincome", "relig"),
        link = "ratio", empirical = TRUE)
```

Logistic regression: married by race, age +2 more

[TABLE]

`Model_RR` est **sans marque**, c’est donc un coefficient : il s’agit de
la régression de *Poisson modifiée* (Zou 2004), dont les coefficients
sont directement des risques relatifs. À comparer au `Model_mRR`
ci-dessus — ils concordent presque ici — et à vérifier quel modèle a
réellement tourné :

``` r

reg_formulas(tab_reg(gss_simple, "married", c("race", "age"), measure = "ratio"))
```

``` r-output
#> # A tibble: 1 × 6
#>   model           outcome family   link       fit                        formula
#>   <chr>           <chr>   <chr>    <chr>      <chr>                      <chr>  
#> 1 01-Married: mRR married binomial odds_ratio "glm(binomial(\"logit\"))" marrie…
```

``` r

reg_formulas(tab_reg(gss_simple, "married", c("race", "age"), link    = "ratio"))
```

``` r-output
#> # A tibble: 1 × 6
#>   model          outcome family   link  fit                             formula 
#>   <chr>          <chr>   <chr>    <chr> <chr>                           <chr>   
#> 1 01-Married: RR married binomial ratio "svyglm(quasipoisson(\"log\"))" married…
```

Deux colonnes répondent. `link` est la mesure que le modèle estime —
`odds_ratio` pour le premier, dont le rapport est donc calculé à partir
des prédictions ; `ratio` pour le second, qui l’estime directement — et
c’est le mot que prend `link =`. `fit` est l’appel R :
`glm(binomial("logit"))` contre `svyglm(quasipoisson("log"))`, un tout
autre modèle. Une ligne
[`svyglm()`](https://rdrr.io/pkg/survey/man/svyglm.html) signale aussi
des erreurs-types robustes (Huber-White), et c’est précisément pour cela
qu’on peut poser une vraisemblance de Poisson sur une variable en 0/1.

**Lequel utiliser.** Préférer **`measure = "ratio"`**, le marginal, dans
presque tous les cas : le modèle logistique converge toujours, ne peut
jamais prédire une probabilité supérieure à 100 %, et le nombre qu’il
donne décrit les personnes réellement interrogées. Recourir à
**`link = "ratio"`** quand c’est le risque relatif *conditionnel* qu’il
faut — celui qui suppose que le même rapport vaut pour tout le monde, ce
qu’attend un lecteur venu de l’épidémiologie, et ce qu’il faut employer
pour s’aligner sur une estimation publiée en Poisson modifiée. Ce sont
deux quantités différentes, pas deux écritures d’une seule.

Pour qui veut le détail technique : les erreurs-types sont traitées de
façon cohérente dans les deux cas. Poser une vraisemblance de Poisson
sur une variable 0/1 est une mauvaise spécification délibérée : les
erreurs-types naïves seraient trop larges, aussi tabxplor les
remplace-t-il par le **sandwich robuste de Huber-White**, via
[`survey::svyglm()`](https://rdrr.io/pkg/survey/man/svyglm.html) — ce
qui donne la variance fondée sur le plan de sondage lorsqu’on fournit
des poids d’enquête, et l’équivalent de `HC0` sinon. La contrepartie
observée suit la même quantité estimée d’un bout à l’autre : `Obs_RR`
est le risque relatif brut avec un intervalle de Katz, jamais le rapport
de cotes brut. La Poisson modifiée demande un échantillon raisonnable (n
d’au moins une centaine), et comme une quasi-vraisemblance n’a pas de
vraisemblance véritable, son bilan de bas de tableau rapporte N et un
test de Wald contre le modèle nul au lieu d’AIC/BIC.

### Régression linéaire (une variable à expliquer numérique)

Une variable à expliquer continue donne de simples coefficients de
régression linéaire (ici nous fixons `family` explicitement, parce qu’un
entier comme `age` est ambigu — il pourrait aussi être modélisé comme un
comptage) :

``` r

tab_reg(gss_simple, "age", c("race", "marital", "relig", "rincome"), family = "gaussian")
```

Linear regression: age by race, marital +2 more

[TABLE]

Dans le cas d’une régression linéaire, la **contrepartie observée du
coefficient du modèle** pour un prédicteur catégoriel est simplement la
**différence de moyennes** : ici, la différence d’âge moyen par modalité
du prédicteur, comparée à la modalité de référence.

``` r

tab_reg(gss_simple, "age", c("race", "marital", "relig", "rincome"), family = "gaussian", empirical = TRUE)
```

Linear regression: age by race, marital +2 more

[TABLE]

Les âges moyens observés, et les différences d’âge moyen à la référence,
se calculent dans un simple tableau avec :

``` r

tab(gss_simple, "race", "age", pct = "row", digits = 2, na = "drop",
    color = "difference", ref = 1,  ci_method = c(mean_diff = "ols")
) |>
  mutate(diff = set_display(age, "diff"))
```

[TABLE]

``` r

# ols : variance commune a TOUS les niveaux de la variable, si bien que les intervalles sont
#   exactement ceux que la regression lineaire donne a ses coefficients
#   ("student" ne met en commun que les deux groupes compares).
```

### Régression de Poisson (une variable de comptage)

``` r

tab_reg(gss_simple, "tvhours", c("race", "marital", "relig", "rincome"), family = "poisson")
```

Poisson regression: tvhours by race, marital +2 more

[TABLE]

Un **rapport de taux d’incidence** (IRR) de 1,5 signifie « 50 % d’heures
de télévision de plus par jour ». Les modèles de Poisson non pondérés
utilisent automatiquement des erreurs-types proportionnelles à la
dispersion observée (quasi-Poisson), si bien que les comptages
surdispersés obtiennent des intervalles honnêtes, plus larges.
Concrètement : avec une variable à expliquer surdispersée,
`family = "poisson"` renvoie des intervalles et des pvalues **identiques
à `family = "quasipoisson"`** et émet un avertissement le disant (le bas
de tableau rapporte la dispersion) ; à équidispersion (≈ 1) la mise à
l’échelle est sans effet et le résultat correspond à un simple
`glm(family = poisson)` — une comparaison avec un `glm` de Poisson
ajusté à la main ne réserve donc jamais de surprise.

Dans le cas d’une régression de Poisson, la **contrepartie observée du
coefficient exponentié du modèle** pour un prédicteur catégoriel est le
**rapport de moyennes** : ici, le rapport des heures moyennes de
télévision comparé à la modalité de référence.

``` r

tab_reg(gss_simple, "tvhours", c("race", "marital", "relig", "rincome"), family = "poisson", empirical = TRUE)
```

Poisson regression: tvhours by race, marital +2 more

[TABLE]

Les heures moyennes observées de télévision par jour, et les rapports de
taux de visionnage à la référence, se calculent dans un simple tableau
de moyennes avec :

``` r

tab(gss_simple, "race", "tvhours", pct = "row", digits = 2, na = "drop",
    color = "ratio", ref = 1,  ci_method = c(mean_ratio = "quasipoisson")
) |>
  mutate(IRR = set_display(tvhours, "ratio"))
```

[TABLE]

``` r

# la methode par defaut pour les intervalles de confiance
#   est le rapport de moyennes robuste a variance inegale ;
#  on utilise "quasipoisson" pour correspondre a ceux calcules
#   par la regression quasi-poisson -- une dispersion estimee sur tous les niveaux
#   (hypothese : la variance est proportionnelle a la moyenne).
```

### Variables binomiales groupées (un score sommé)

Quand la variable à expliquer est un **score sommé** — à combien d’items
oui/non un répondant a-t-il répondu de la même façon ? — on modélise le
nombre de « succès » sur un nombre fixe d’items avec `trials =`.
[`tab_reg()`](https://bricenocenti.github.io/tabxplor/reference/tab_reg.md)
estime alors `cbind(score, trials - score)` comme un modèle binomial, si
bien que les rapports de cotes se lisent sur la probabilité *par item*.

C’est le modèle naturel d’une **question d’enquête à réponses
multiples** — ici les données `tea` préparées plus haut, dont les six
items « où buvez-vous du thé ? » ont été sommés en un score par
[`score_from_lv1()`](https://bricenocenti.github.io/tabxplor/reference/score_from_lv1.md).

``` r

tab_reg(tea, "tea_where", c("sex", "SPC", "Sport"),
        family = "binomial", trials = length(tea_where_vars))
```

Logistic regression: tea_where by sex, SPC +1 more

[TABLE]

Chaque rapport de cotes se lit désormais *pour un lieu quelconque* :
`1/1.44` pour les hommes signifie que, pour chacun des six lieux, les
hommes ont environ 1,44 fois moins de chances que les femmes d’y boire
du thé. Le modèle traite les six items comme des tirages
interchangeables, si bien qu’un seul rapport de cotes les couvre tous —
c’est précisément l’hypothèse à garder en tête avant de s’en servir.

Les modèles binomiaux groupés, comme ceux de Poisson, rapportent un
contrôle de **dispersion** de Pearson dans le bas de tableau, qui
signale les comptages surdispersés.

### Variables ordinales et nominales (un facteur à 3 modalités ou plus)

Un facteur **ordonné** est estimé comme un modèle logistique à cotes
proportionnelles (cumulé), et il rapporte en **une seule colonne** :
toute sa prétention est qu’un seul nombre par modalité de prédicteur
suffit, il ne dépense donc jamais une colonne par catégorie de la
variable à expliquer. Par défaut ce nombre est le rapport de cotes
cumulé `cumOR` — le coefficient propre du modèle, et la seule mesure que
l’hypothèse de cotes proportionnelles rend identique à chaque seuil :

``` r

tab_reg(gss_simple, "rincome", c("race", "age", "relig"))
```

Ordinal logistic regression: rincome by race, age +1 more

[TABLE]

Un rapport de cotes cumulé est difficile à dire à voix haute : demander
une autre **mesure de l’écart** fait lire le même ajustement comme une
probabilité. `measure = "difference"` donne le `D` de Somers — *de deux
personnes, l’une de ce groupe et l’autre du groupe de référence, à
quelle fréquence celle de ce groupe se retrouve-t-elle plus haut sur
l’échelle ?* — avec cette probabilité elle-même entre crochets, 50 %
étant un pile ou face :

``` r

tab_reg(gss_simple, "rincome", c("race", "age", "relig"),
        measure = "difference", display = "est_base")
```

Ordinal logistic regression: rincome by race, age +1 more

[TABLE]

`measure = "ratio"` lit la même paire de façon multiplicative, comme un
**rapport de victoires** (*win ratio*, victoires sur défaites). Les deux
tiennent encore en une colonne, parce que les deux lisent toute la
distribution prédite plutôt qu’une seule de ses tranches — et les deux
sont robustes là où le rapport de cotes cumulé ne l’est pas : ils
bougent à peine quand l’hypothèse de cotes proportionnelles est violée,
et, à la différence d’un rapport de cotes, ils ne dérivent pas sous
l’ajustement quand il n’y a rien à ajuster. Le bas de tableau nomme
l’échelle du bas vers le haut, puisqu’un tableau à une colonne montre le
nom de la variable à expliquer et aucune de ses catégories.

Pour un nombre **par catégorie de la variable à expliquer** — un effet
en points de pourcentage sur chaque tranche de revenu — c’est une
question à laquelle l’ordre n’aide pas, et `family = "multinomial"` y
répond sans supposer les cotes proportionnelles.

Une variable à expliquer nominale, à trois modalités non ordonnées ou
plus, est estimée comme un seul modèle logistique **multinomial**,
donnant une colonne de rapport de cotes par catégorie de la variable à
expliquer contre sa catégorie de référence (aussi appelés rapports de
risques relatifs) :

``` r

tab_reg(gss_simple, "party3", c("race", "age", "rincome", "relig"))
```

Multinomial logistic regression: party3 by race, age +2 more

[TABLE]

Les rapports de risques relatifs peuvent être assez difficiles à lire,
parce qu’ils sont relatifs à **deux** modalités de référence : non
seulement celle choisie pour le prédicteur, mais aussi celle choisie
pour la variable à expliquer. C’est particulièrement difficile quand il
est ardu de trouver une bonne modalité de référence correspondant à la
situation la plus commune (comme « married » pour le statut
matrimonial).

La plupart du temps, demander des points de pourcentage est plus facile
à interpréter, parce que cela fait disparaître la seconde modalité de
référence et modélise directement, pour chaque modalité de la variable à
expliquer, la différence de pourcentages de chaque modalité de
prédicteur comparée à sa référence (une quantité moins abstraite qu’un
rapport de cotes). C’est `measure = "difference"`, exactement comme sur
une variable binaire, et les colonnes `mRD` obtenues sont des effets
marginaux moyens :

``` r

tab_reg(gss_simple, "party3", c("race", "age", "rincome", "relig"), measure = "difference", empirical = TRUE) # |> tab_export()
```

Multinomial logistic regression: party3 by race, age +2 more

[TABLE]

Une variable à 3 modalités ou plus demanderait une colonne brute par
catégorie : `empirical = TRUE` replie donc l’effet brut **dans** la case
du modèle — `+40.6% (+38.6%)`, modélisé puis observé — et les
pourcentages et différences bruts apparaissent en infobulle html, au
survol d’une case. `empirical = "column"` redemande malgré tout les
colonnes.

## 4. Lire ce que l’ajustement a fait

### Colorer ce que l’ajustement a fait

`empirical = TRUE` place l’effet brut à côté de l’effet modélisé, mais
les comparer case par case est fastidieux sur un vrai tableau.
`color = "adjustment"` colore l’**écart** entre les deux : tout un
tableau de « qu’est-ce que l’ajustement a changé ? » se lit alors d’un
coup d’œil. Placé sur le canal *fond*, le texte continue d’afficher la
taille de l’effet, de sorte qu’un seul regard répond aux deux questions
:

``` r

tab_reg(gss_simple, "married", c("race", "rincome", "relig"),
        empirical = TRUE, color = c(TRUE, "adjustment"))
```

Logistic regression: married by race, rincome +1 more

[TABLE]

Couleur du texte = la force du rapport de cotes ajusté. Fond = sa
distance à l’effet observé : un pôle signifie que l’ajustement a
**renforcé** l’effet (il s’est éloigné de « pas d’effet »), l’autre
qu’il l’a **atténué** (il s’en est rapproché). Un effet dont
l’ajustement change le *sens* — observé au-dessus de 1, modélisé en
dessous — compte comme atténué, et fortement : quoi qu’ait dit
l’association brute, le modèle dit que ce n’est pas cela. Le
renversement lui-même se voit dans la paire de cases, dont les `×` et
`÷` pointent en sens inverse. Le premier seuil est ×1,1, la convention
classique « une variation de 10 % de l’estimation mérite l’attention » ;
viennent ensuite ×1,25, ×1,5 et ×2 (modifiables avec
`set_color_breaks(adj_ratio = ...)`). Un effet *additif* reçoit une
échelle additive : des points de pourcentage pour un effet marginal sur
une probabilité (`±2 / ±5 / ±10 / ±20`), et des écarts-types de la
variable à expliquer pour une différence de moyennes linéaire
(`±0,05 / ±0,1 / ±0,2 / ±0,4`) — le même modèle se lit donc de la même
façon que la variable soit enregistrée en heures, en minutes ou en
jours. Inutile de demander `empirical` séparément : la couleur a besoin
de l’effet observé, elle l’active donc d’elle-même.

La direction se mesure **par rapport à l’absence d’effet**, et non vers
le haut ou vers le bas. Un effet protecteur (OR inférieur à 1) que
l’ajustement rapproche de 1 reçoit la même couleur qu’un effet de risque
rapproché de 1, ce qui est bien ce qu’on veut lire : « les autres
variables expliquent une partie de cette association » est un seul
énoncé, de quelque côté de 1 que se trouve l’effet.

Le même écart se lit aussi en chiffres, dans la case ou au survol.
L’argument `display =` pose le gabarit dès la construction (partie 5,
*Affiner l’affichage*) ; sur un tableau déjà construit,
[`set_display()`](https://bricenocenti.github.io/tabxplor/reference/fmt_fields.md)
fait la même chose — les colonnes sans contrepartie observée gardent
simplement leur affichage habituel :

``` r

tab_reg(gss_simple, "married", c("race", "rincome"), empirical = TRUE) |>
  set_display("{est} (obs {obs})")
```

Logistic regression: married by race, rincome

[TABLE]

#### Un avertissement sur les rapports de cotes

Un rapport de cotes change quand on l’ajuste **même lorsqu’il n’y a rien
à ajuster**. C’est ce qu’on appelle la *non-collapsibilité* : c’est une
propriété du rapport de cotes lui-même, pas le signe d’un facteur de
confusion. Dans une simulation où la variable ajoutée est indépendante
de l’exposition — donc sans aucune confusion — le rapport de cotes brut
se déplaçait tout de même d’environ **8 %** une fois ajusté, contre 0,3
% pour le risque relatif et pratiquement rien pour l’effet marginal.
Huit pour cent, c’est à peu près la taille du premier seuil de couleur :
sur l’échelle des rapports de cotes, une couleur de fond légère peut
donc relever de l’arithmétique plutôt que de la confusion. La légende
d’un tableau exporté le signale.

Les comparaisons qui *sont* propres sont celles que les sections
précédentes ont présentées : `measure = "difference"` (points de
pourcentage), `measure = "ratio"` (risques relatifs), `link = "ratio"`
(le risque relatif propre à la Poisson modifiée), la différence de
moyennes d’une régression linéaire — et, s’il faut publier des rapports
de cotes, `measure = "odds_ratio", effect = "marginal"`, dont le rapport
de cotes marginal *est* collapsible. Sur ces échelles, l’écart est bien
la confusion apportée par les variables ajoutées, et rien d’autre. Si
lire la confusion est l’objet du tableau, préférer l’une d’elles :

``` r

tab_reg(gss_simple, "married", c("race", "rincome", "relig"),
        measure = "ratio", empirical = TRUE, color = c(TRUE, "adjustment"))
```

Logistic regression: married by race, rincome +1 more

[TABLE]

#### L’écart dépasse-t-il le bruit ?

Une couleur de fond dit que le modèle a *déplacé* un effet. Elle ne dit
pas, à elle seule, que ce déplacement est réel : avec 20 000 enquêtés un
petit décalage est solide, avec 300 un grand décalage peut n’être que de
la chance. `color_signif` répond à cette question, exactement comme
partout ailleurs dans `tabxplor` — ajouter `"grey_non_signif"` et un
fond coloré signifie désormais « le modèle a vraiment changé cet effet »
:

``` r

tab_reg(gss_simple, "married", c("race", "rincome", "relig"),
        link = "ratio", empirical = TRUE,
        color = c(TRUE, "adjustment"), color_signif = "grey_non_signif")
```

Logistic regression: married by race, rincome +1 more

[TABLE]

Ici `link = "ratio"` donne les **risques relatifs** propres à la Poisson
modifiée, l’une des échelles propres de la section précédente. Couleur
du texte = la force du risque relatif ajusté. Fond = sa distance à
l’effet observé, grisée quand cette distance peut relever du hasard.
Survoler une case dans le tableau html donne les chiffres exacts :
l’effet observé, la taille de l’écart, son intervalle de confiance et sa
pvalue.

**Un exemple vaut toute l’explication.** Lire le bloc `race` de ce
tableau, qui compare chaque groupe aux enquêtés blancs, avant et après
avoir mis le revenu et la religion à égalité :

| échelle | Noirs vs Blancs | Autres vs Blancs |
|:---|---:|---:|
| rapport de cotes, brut → modélisé | `1/2.44` → `1/2.54` | `1/1.11` → `1.01` |
| risque relatif, brut → modélisé | `÷1.7` → `÷1.7` | `÷1.1` → `×1.0` |
| le risque relatif a-t-il vraiment bougé ? | **non** (p = 0,55) | **oui** (p \< 0,001) |

Deux histoires différentes, et le test les sépare. Pour les enquêtés
noirs, rien ne bouge : leur taux de mariage bien plus faible n’est *pas*
expliqué par le revenu ou la religion — il survit intact à l’ajustement.
Pour le groupe « Autres », le déficit brut de 5 % disparaît une fois le
revenu et la religion tenus égaux, et l’écart entre les deux chiffres
est bien trop grand pour être fortuit : cette différence-là *était*
expliquée par eux.

À remarquer : les rapports de cotes bougent aussi — y compris pour les
enquêtés noirs, là où le risque relatif dit que rien ne s’est passé.
C’est la non-collapsibilité de la section précédente, qui apparaît sous
la forme d’un « changement » fantôme. C’est pourquoi `color_signif`
n’est **pas appliqué aux rapports de cotes simples** :
[`tab_reg()`](https://bricenocenti.github.io/tabxplor/reference/tab_reg.md)
y laisse les couleurs descriptives et le signale une fois, en renvoyant
à `measure = "difference"`, `measure = "ratio"` ou `link = "ratio"`.

Le grisage prend toute sa valeur sur les petits échantillons. Avec 20
000 enquêtés, presque tout déplacement visible est réel ; sur 2 000, le
même tableau ne garde qu’une couleur de fond sur trois :

``` r

set.seed(1)
small <- gss_simple[sample(nrow(gss_simple), 2000), ]
tab_reg(small, "married", c("race", "rincome", "relig"),
        link = "ratio", empirical = TRUE,
        color = c(TRUE, "adjustment"), color_signif = "grey_non_signif")
```

Logistic regression: married by race, rincome +1 more

[TABLE]

#### Pour mémoire : ce qui est exactement testé

La couleur `"adjustment"` montre toujours la **taille** de l’écart.
Qu’elle le *teste* aussi dépend d’une propriété de la mesure, et le
tableau tranche de lui-même :

| ce qui a été demandé | écart testé ? |
|----|----|
| `measure = "difference"` — toute variable à expliquer | **oui** |
| `measure = "ratio"`, ou `link = "ratio"` (risques relatifs) | **oui** |
| `measure = "odds_ratio", effect = "marginal"` (binaire ou score sommé) | **oui** |
| rapports de taux d’incidence de Poisson, différence de moyennes linéaire | **oui** |
| rapports de cotes **conditionnels** — binomial, multinomial, ordinal cumulé | non (non collapsible) |
| une `formula =` composée | non |
| une variable à 3 modalités ou plus *pondérée* | elle ne se lit que sur ses coefficients |

Là où il n’y a pas de test, `color_signif` est ignoré pour ce canal et
les couleurs se lisent descriptivement : le tableau ne feint jamais une
significativité qu’il n’a pas.

L’hypothèse nulle est l’égalité de l’effet modélisé et de l’effet
observé, sur l’échelle propre de l’effet : le log du rapport pour un
risque relatif, un rapport de cotes ou un rapport de taux d’incidence ;
la différence simple pour une différence de moyennes ou un effet
marginal en points. C’est l’échelle autour de laquelle la couleur se
replie déjà, de sorte que le test et la couleur ne peuvent pas diverger.

Les deux estimations proviennent des mêmes lignes : elles sont donc
corrélées, et leur différence a une erreur-type plus petite que chacune
prise séparément. La variance correcte est celle de la différence de
leurs *fonctions d’influence* — ce que Stata appelle l’estimation «
apparemment non reliée » (Weesie 1999 ; Mize, Doan & Long 2019 en est
l’énoncé sociologique). Deux choses la rendent ici exacte plutôt
qu’approchée : tout effet observé qu’affiche `tabxplor` *est* le
coefficient d’un modèle saturé à un seul prédicteur, si bien que sa
fonction d’influence a une forme close dont l’erreur-type est
l’intervalle déjà affiché ; et avec des pondérations d’enquête ou un
plan de sondage, la variance passe par la linéarisation de `survey`, qui
respecte strates, grappes et correction de population finie. La loi de
référence est la loi normale (z), légèrement conservatrice sur petits
échantillons.

Les trois politiques lisent ce même intervalle :

| `color_signif` | ce que montre le fond |
|----|----|
| `"ignore"` (défaut dans [`tab()`](https://bricenocenti.github.io/tabxplor/reference/tab.md)) | la taille de l’écart, colorée qu’il soit significatif ou non |
| `"grey_non_signif"` (défaut dans [`tab_reg()`](https://bricenocenti.github.io/tabxplor/reference/tab_reg.md)) | la même chose, grisée si l’intervalle de l’écart contient « aucun changement » |
| `"guaranteed_effect"` | le **plancher** de l’écart : « l’ajustement a déplacé cet effet d’au moins ×1,1 » (sur une échelle additive, « d’au moins 2 points ») |

**Les étoiles et la couleur ne disent pas la même chose, et c’est la
combinaison qui est utile.** Les étoiles continuent de lire la pvalue de
chaque estimation — « cet effet diffère-t-il de 1 ? » — et la couleur
celle de l’écart. Les quatre combinaisons se rencontrent dans un même
petit tableau :

| modalité | étoiles | p (effet) | p (écart) | colorée |
|----|----|----|----|----|
| Noirs | \*\*\* | \< 0,0001 | 0,55 | non — l’écart est très en deçà du premier seuil |
| Juifs | — | 0,12 | \< 0,0001 | **oui** |
| Musulmans | — | 0,92 | 0,12 | non |
| Bouddhistes/hindouistes | \* | 0,057 | \< 0,0001 | **oui** |

Lire *étoilé et non coloré* comme un effet **robuste** — l’ajustement ne
l’a presque pas touché — et *non étoilé et coloré* comme un effet que
**seul le tableau brut montrait**. Les juifs sont la ligne instructive :
aucun effet à rapporter, mais l’ajustement l’a beaucoup déplacé, et
significativement.

Deux détails encore. La direction se mesure par rapport à la valeur
nulle : un effet protecteur ramené vers 1 se colore comme un effet
risqué ramené vers 1. Et le test exige que les deux estimations portent
sur les mêmes personnes : avec `effect = "at_reference"`, ou avec
`na = "drop_by_model"` quand un modèle comparé écarte d’autres valeurs
manquantes que les colonnes observées, la comparaison n’est pas faite du
tout. (Par défaut elle l’est toujours.)

## 5. Mettre le tableau en forme

### Affiner l’affichage, mettre un prédicteur à l’échelle

Plusieurs arguments changent *ce que chaque case affiche*, ou *comment
un prédicteur est mis à l’échelle*, sans changer le modèle lui-même.

**`display`** choisit la mise en page de la case — les mêmes
dispositions nommées que
[`tab()`](https://bricenocenti.github.io/tabxplor/reference/tab.md),
dans la même grammaire [`{}`](https://rdrr.io/r/base/Paren.html).
`"est_ci"` affiche l’intervalle de confiance à côté de chaque estimation
(quelle que soit la famille) :

``` r

tab_reg(gss_simple, "married", c("race", "age"), display = "est_ci")
```

Logistic regression: married by race, age

[TABLE]

`"est_base"` replie la **prédiction ajustée par le modèle** dans la case
de l’estimation — une probabilité ajustée pour un modèle logistique, une
moyenne ajustée pour un modèle linéaire ou de comptage, chaque colonne
répondant avec sa propre quantité. `"base_est"` inverse les deux, si
bien que le tableau se lit comme des prédictions ajustées graduées par
l’effet, et `"base"` n’affiche que les prédictions :

``` r

tab_reg(gss_simple, "married", c("race", "age"), display = "est_base")
```

Logistic regression: married by race, age

[TABLE]

Une différence à noter : `display = "est_base"` *conserve* la colonne du
rapport de cotes et ajoute la probabilité ajustée à côté, tandis que
`measure = "difference"` (vu plus haut) transforme la colonne
**entière** en un effet marginal en points.

**`multiplier`** choisit l’**unité** dans laquelle l’effet d’un
prédicteur continu est rapporté. Une unité est rarement une quantité
lisible : un changement d’un an de `age` déplace à peine les cotes, donc
son rapport de cotes reste près de 1, ne franchit jamais un seuil de
couleur, et la ligne se lit comme « aucun effet » — alors qu’un
écart-type entier d’âge multiplie les cotes par environ 0,66. Par
défaut, l’effet est donc rapporté **par deux écarts-types**, et la ligne
indique lequel (`per 34.6 (2SD)`) — la variable est nommée dans la
colonne d’à côté. Deux écarts-types, parce que c’est à peu près
l’étendue que couvre un prédicteur binaire : une ligne continue et une
ligne à deux modalités deviennent ainsi comparables, et toute une
colonne se lit d’un trait :

``` r

tab_reg(gss_simple, "married", c("race", "age"))
```

Logistic regression: married by race, age

[TABLE]

Une valeur unique la change pour tous les prédicteurs continus, un
vecteur nommé n’en modifie que certains — `"sd"`, `"2sd"` (grossièrement
du bas au haut de la distribution), ou un nombre d’unités.
`multiplier = 1` redonne l’effet brut par unité :

``` r

tab_reg(gss_simple, "married", c("race", "age"), multiplier = c(age = 10))
```

Logistic regression: married by race, age

[TABLE]

Tout est mis à l’échelle ensemble — l’estimation, son intervalle, la
colonne observée `Obs_*` et la comparaison modèle/observé — et la pvalue
ne change jamais. Deux points à connaître : comme la valeur par défaut
n’est pas 1, le `Model_OR` d’un prédicteur continu ne correspondra
**pas** à `exp(coef(glm(...)))` sans demander `multiplier = 1` ; et
l’écart-type est mesuré une seule fois, sur les cas complets des
prédicteurs, de sorte qu’une même variable garde la même unité entre
plusieurs variables à expliquer, entre modèles comparés et entre groupes
de `tab_vars`.

#### La référence depuis laquelle se mesure l’effet d’un prédicteur

`ref` nomme la **référence** — pour un facteur, la modalité à laquelle
les autres sont comparées ; pour un prédicteur continu, la valeur à
laquelle il est **ancré** :

``` r

tab_reg(gss_simple, "married", c("race", "age"), ref = c(race = "Black", age = 40))
```

Logistic regression: married by race, age

[TABLE]

Les deux moitiés sont une seule idée, et elles partagent une grammaire
avec `multiplier` et `shape` : une valeur écrite **sans nom de
variable** est la valeur par défaut pour tous les prédicteurs auxquels
elle peut s’appliquer, une valeur nommée ne concerne que cette variable.
Ainsi `shape = "quintiles"` découpe tous les prédicteurs continus,
`multiplier = c("2sd", age = 10)` se lit « par deux écarts-types, sauf
`age`, par décennie », et `ref = c("median", "last", race = "Black")`
fixe les deux valeurs par défaut d’un coup — la valeur dit elle-même à
quel type de prédicteur elle s’adresse (un nombre ou `"mean"` /
`"median"` / `"min"` / `"max"` pour un continu, `"first"` / `"last"`
pour un facteur).

Ancrer un prédicteur continu **ne change pas son propre effet** : une
pente est la même d’où qu’on la lise. La ligne du prédicteur indique où
se situe son ancrage, juste à côté de l’unité dans laquelle son effet
est rapporté — `per 34.6 (2SD), at 47.2 (mean)`, ou `at 0 (min)` — de
sorte que le profil de la ligne Constant se lit sur le tableau lui-même.
Ce que cela change, c’est la ligne **Constant**, et tout terme avec
lequel le prédicteur interagit. C’est pourquoi la valeur par défaut est
la moyenne et non zéro : personne n’a 0 an, donc l’ordonnée à l’origine
d’un ajustement brut ne décrit personne. La ligne Constant se lit comme
la ligne de base dont le reste de la colonne s’écarte — une cote de base
sur une colonne de rapports de cotes, une probabilité ou une moyenne de
base sur une colonne additive — et son intitulé dit où se situe cette
base (`Reference profile`, ou `Population average` sur une colonne
marginale). Si le zéro d’un prédicteur a bien un sens, autant le dire :
`ref = c(tvhours = 0)`.

L’ordre compte à un endroit, et il est fixé plutôt que deviné : `shape`
recode d’abord la colonne (c’est elle qui définit *ce qu’est* la
variable du modèle), puis l’ancrage s’applique au résultat. Donc `ref`
sur un prédicteur en `"log"` ancre le log, et un prédicteur découpé en
`"quartiles"` est devenu un facteur et prend un nom de modalité.

### Interactions : l’effet d’un prédicteur qui dépend d’un autre

La section sur les sous-populations, plus bas, compare des
**sous-populations** — un modèle par groupe. Une interaction pose la
même question **à l’intérieur d’un seul modèle**, où une troisième
variable peut être contrôlée sur tout l’échantillon. Elle s’écrit dans
`predictors` avec le `*` de R, nu ou entre guillemets :

``` r

tab_reg(gss_simple, "married", c(race*party3, relig), empirical = TRUE)
```

Logistic regression: married by race\*party3, relig

[TABLE]

`race*party3` se lit « l’effet de la couleur de peau, autorisé à varier
avec le parti », et c’est un **prédicteur comme un autre** : une ligne
par case du croisement, chacune comparée à une seule case de référence
commune, avec son effectif, le taux observé et le taux ajusté à côté.
Rien d’autre ne change — les couleurs, les étoiles, la colonne brute et
le bas de tableau fonctionnent comme sur n’importe quelle variable,
parce que le couple *est* devenu une variable.

Lire une ligne à la fois : « les Noirs républicains ont de fois les
cotes d’être mariés de la case de référence, et % contre % prédits ».
C’est la présentation que les épidémiologistes recommandent pour une
interaction (Knol & VanderWeele 2012) : un effet par strate contre une
référence commune unique, avec les taux réels affichés.

`a*b` est l’écriture de R et signifie ici exactement ce qu’elle signifie
dans [`glm()`](https://rdrr.io/r/stats/glm.html) : `a + b + a:b`.
L’ordre choisit la présentation, jamais le modèle — `a*b` et `b*a` sont
le même ajustement, et les lignes portent sur celle qu’on met en
premier. (Le `a:b` de R, c’est-à-dire le terme d’interaction *sans* ses
effets principaux, est un autre modèle, dont l’ajustement dépend de
l’endroit où se trouve le zéro de chaque variable :
[`tab_reg()`](https://bricenocenti.github.io/tabxplor/reference/tab_reg.md)
le refuse en le nommant plutôt que d’en faire un synonyme.)

Les deux variables d’un couple sont **fournies par l’interaction** : il
ne faut donc pas les lister à côté d’elle. Écrire
`c(race, party3, race*party3)` est une erreur, pas un modèle plus riche.
Ce que cela permet, c’est de demander naturellement si l’interaction
vaut la peine :

``` r

tab_reg(gss_simple, "married",
        list(additive = c(race, party3), crossed = c(race*party3)),
        stats = "compare_sequential")
```

Logistic regressions (models comparison): married, ‘01-Married’ (OR)

[TABLE]

Le bas de tableau y répond deux fois, avec le même nombre : la
comparaison de modèles, et la ligne **`Interaction (LR)`**, présente par
défaut sur un modèle linéaire ou logistique. C’est cette ligne qu’il
faut citer : les étoiles d’une case disent qu’elle diffère de la case de
référence, ce qui est surtout l’effet des deux variables prises
séparément ; la ligne du bas dit si la *configuration* s’écarte de ce
que ces effets séparés prédisent à eux seuls.

#### Un prédicteur continu : des pentes par groupe

Une variable continue n’a pas de cases à croiser : `age*race` donne
alors l’autre lecture honnête — la **pente de `age` à l’intérieur de
chaque modalité de `race`**, directement issue de l’ajustement, dans
l’unité que la ligne nomme :

``` r

tab_reg(gss_simple, "married", c(age*race, relig), empirical = TRUE)
```

Logistic regression: married by race, age\*race +1 more

[TABLE]

Le modérateur garde son propre bloc (le modèle le contient), et tous les
`effect` fonctionnent ici aussi : `measure = "difference"` donne l’effet
marginal moyen de chaque groupe, en points. Pour obtenir plutôt le
tableau de cases, découper la variable continue en groupes — c’est en
général la réponse la plus lisible :

``` r

tab_reg(gss_simple, "married", c(age*race, relig), shape = c(age = "quartiles"))
```

L’ordre n’a pas besoin d’être le bon : écrit `race*age`, tabxplor le lit
comme `age*race` et le dit en une ligne — `*` est symétrique dans
l’ajustement, et seule une variable continue a des pentes à montrer par
groupe : il n’existe donc qu’un seul tableau possible.

Et si les **deux** variables sont continues, il n’y a aucune case à
croiser : la seconde est alors découpée en quartiles, là encore en une
ligne :

``` r

tab_reg(gss_simple, "married", c(age*tvhours, race), empirical = TRUE)
```

Logistic regression: married by tvhours, age\*tvhours +1 more

[TABLE]

Ce découpage est un choix de modèle, pas de présentation — le test
d’interaction du bas de tableau varie avec le nombre de classes —, et
c’est pourquoi il est annoncé plutôt que silencieux. On le choisit
soi-même avec `shape` (`shape = c(tvhours = "quintiles")`), ou en
écrivant `tvhours*age` pour découper `age` à la place. Pour le
coefficient du produit sans aucun découpage, écrire le modèle sous forme
de formule : `outcome = y ~ ... + age * tvhours`.

#### L’échelle porte l’interaction

Une réserve à énoncer clairement, car ce n’est le défaut d’aucune
méthode : **une interaction dépend de l’échelle sur laquelle on la
mesure**. Un modèle sans interaction sur l’échelle des rapports de cotes
en a généralement une sur l’échelle des probabilités, et réciproquement
— c’est ce que font une fonction de lien et la non-collapsibilité (Ai &
Norton 2003). La réponse de tabxplor est de rendre l’échelle visible
plutôt que d’en choisir une : imprimer le même tableau des deux façons,
`measure = "odds_ratio"` puis `measure = "difference"`, et dire laquelle
on lit.

Pour une spécification que la surface d’arguments ne sait pas exprimer —
contrastes personnalisés, offset écrit à la main, terme à trois facteurs
— une **formule de modèle** dans `outcome` reste la porte de sortie des
expert·es (`tab_reg(d, married ~ race * age * relig)`). Elle estime
exactement ce qu’on écrit, mais elle sort du cadre : ses lignes portent
des noms bruts de coefficients, sans effectifs, sans contrepartie
observée et sans unité.

#### Trois façons de se tromper

**Une case colorée signale un écart sur *cette mesure*, pas la preuve
que la cause diffère.** Les groupes peuvent différer simplement parce
que le phénomène est plus rare chez les uns, ou plus variable — et alors
leurs effets diffèrent sur toutes les échelles, rapports de cotes,
risques relatifs et effets marginaux confondus. Aucune échelle n’y
échappe : la lecture doit donc être prudente plutôt qu’astucieuse.

**Il faut donc lire la forme d’ensemble, pas la case isolée.** Si toute
une colonne est décalée dans le même sens, les groupes diffèrent
probablement de niveau ou de variabilité — c’est une propriété du
groupe, pas du prédicteur. Si **une seule ligne** ressort quand les
autres restent sages, voilà à quoi ressemble une vraie interaction. Les
couleurs sont utiles justement parce qu’elles rendent cette forme
visible d’un coup d’œil. Ajouter `empirical = TRUE` affiche la fréquence
de base de chaque groupe (entre parenthèses dans la colonne brute), ce
qui tranche le plus souvent la question.

**Chaque case est testée pour elle-même, sans correction pour le nombre
de tests.** Dans un tableau comptant sept comparaisons, environ un
tableau sur cinq affichera une case colorée à tort. La ligne du bas de
tableau y échappe : elle pose une seule question par prédicteur.

#### Pour mémoire : ce qui est exactement testé

Les deux groupes sont des échantillons disjoints, donc les deux
estimations sont indépendantes et l’erreur-type de leur différence vaut
`sqrt(SE_A² + SE_B²)` — le test classique de la différence entre deux
estimations indépendantes (Altman & Bland 2003). Les deux erreurs-types
sont relues dans les intervalles de confiance que le tableau affiche
déjà : le test et les intervalles imprimés ne peuvent donc pas se
contredire. L’écart est mesuré sur l’échelle propre de l’effet — le log
du rapport pour un rapport de cotes, un risque relatif ou un rapport de
taux, la différence simple pour un bêta ou un effet marginal — et
comparé à un seuil normal (z), légèrement conservateur sur petits
effectifs.

Les trois politiques `color_signif` lisent ensuite ce même intervalle
exactement comme en partie 4 — ici, « aucun changement » se lit « aucune
différence entre les deux groupes ».

Deux détails utiles. Les **étoiles** de significativité dans les cases
continuent de lire la pvalue de chaque estimation — « cet effet
diffère-t-il de 1 ? » — et non celle de l’écart ; celle de l’écart est
dans l’infobulle. Et le test agrégé du bas de tableau est un test du
rapport de vraisemblance (un test F pour les modèles linéaires et quasi,
un test de Wald fondé sur le plan de sondage avec des poids — la même
règle que la comparaison de modèles ci-dessous), calculé sur un modèle
groupé supplémentaire, et portant sur les **coefficients** : sur une
colonne marginale, les cases montrent des effets marginaux tandis que le
bas de tableau teste les coefficients, deux questions proches mais
distinctes, et la ligne le précise.

### Comparer plusieurs modèles

Passer une **liste nommée** d’ensembles de prédicteurs, plutôt qu’un
vecteur, estime et affiche plusieurs modèles côte à côte. Un test de
comparaison par rapport de vraisemblance est une clé de plus dans
`stats =` : `stats = "compare_baseline"` compare chaque modèle au
premier (on en désigne un autre avec
`stats = c(compare_baseline = "M2")`), `stats = "compare_sequential"`
compare chaque modèle au précédent :

``` r

tab_reg(gss_simple,
        "married",
        list("Race only"    = "race",
             "+ age"        = c("race", "age"),
             "+ party"      = c("race", "age", "party3")),
        stats = "compare_sequential")
```

Logistic regressions (models comparison): married, ‘01-Married’ (OR)

[TABLE]

### Le même modèle au sein de sous-populations

`tab_vars =` est l’analogue du `tab_vars` de
[`tab()`](https://bricenocenti.github.io/tabxplor/reference/tab.md) pour
une régression : le même modèle est estimé **au sein de chaque
modalité** d’une variable de groupe, et les tableaux par groupe sont
empilés en un seul tableau groupé. Cela répond à la question « cet effet
tient-il dans chaque sous-groupe ? ». Avec une seule variable à
expliquer,
[`tab_spread()`](https://bricenocenti.github.io/tabxplor/reference/tab_spread.md)
est utilisé en interne pour disposer les groupes en colonnes côte à
côte, plus faciles à comparer :

``` r

tab_reg(gss_simple, "married", c("race", "rincome"), tab_vars = "year")
```

Logistic regression: married by race, rincome (tabbed by year)

[TABLE]

Lire une ligne d’un groupe à l’autre est la même corvée que comparer un
modèle à son effet observé, et la réponse est la même :
`color = "between_groups"` colore la distance entre l’effet de chaque
groupe et celui du **premier** groupe, sur la même ligne. Là où le test
de comparaison de modèles dit « ces modèles diffèrent » une fois pour
tout le modèle, ceci dit *quels effets* diffèrent selon les groupes —
une lecture prédicteur par prédicteur de ce que les statisticiens
appellent la modification d’effet.

Les groupes réunissent ici des personnes différentes : l’écart entre
deux de leurs effets peut donc être testé — et il l’est, si bien que
`color_signif` fonctionne comme partout ailleurs. Avec
`"grey_non_signif"`, une couleur de fond signifie alors « c’est vraiment
un effet différent, pas seulement du bruit » :

``` r

tab_reg(gss_simple, "married", c("race", "rincome"), tab_vars = "party3",
        color = c(TRUE, "between_groups"), color_signif = "grey_non_signif")
```

Logistic regression: married by race, rincome (tabbed by party3)

[TABLE]

Couleur du texte = la force de l’effet dans ce groupe. Couleur de fond =
la distance à l’effet du premier groupe sur la même ligne, grisée
lorsque l’écart pourrait être dû au hasard. Le premier groupe sert de
référence et reste vide (rien n’est comparé à soi-même) ;
[`forcats::fct_relevel()`](https://forcats.tidyverse.org/reference/fct_relevel.html)
réordonne les modalités pour en choisir un autre. Survoler une case du
tableau html donne les chiffres exacts : l’effet de l’autre groupe, la
taille de l’écart, son intervalle de confiance et sa pvalue.

Le bas de tableau a gagné une ligne au passage : **un test par
prédicteur**, qui demande « ce prédicteur agit-il différemment selon les
groupes ? » pour toutes ses modalités à la fois. C’est la version
agrégée de la même question, et c’est celle qu’il faut citer — parce
qu’elle est posée une fois, et non une fois par case. Elle s’obtient
aussi seule, sans les couleurs, avec
`stats = c("n", "group_interaction")`.

### Données pondérées et plans de sondage

Il n’y a que **deux façons de transmettre ses poids à tabxplor**, et
c’est délibéré. Soit on donne une variable de poids à `wt =`, soit —
lorsque le fichier porte aussi des strates, des grappes, une correction
de population finie ou un calage sur marges — on construit le plan une
fois avec le package [survey](https://CRAN.R-project.org/package=survey)
et on passe **le plan lui-même** comme `data` :

``` r

tab_reg(data, "outcome", c("pred1", "pred2"), wt = "weight")

library(survey)
d <- svydesign(ids = ~psu, strata = ~strate, weights = ~w, data = mon_enquete, nest = TRUE)
tab_reg(d, "outcome", c("pred1", "pred2"), empirical = TRUE)
```

Dans les deux cas, l’estimation bascule sur
[`survey::svyglm()`](https://rdrr.io/pkg/survey/man/svyglm.html) et tout
suit ce plan, dans un seul régime : les coefficients et leurs
intervalles, les effets moyennés sur l’échantillon, la mise à l’échelle
des prédicteurs numériques, le test de l’écart entre modèle et observé —
**et les colonnes observées `Obs_*`**, dont les intervalles sont
construits sur la variance de plan de chaque case. Ce dernier point est
ce qui rend possible la comparaison centrale de cette page : la colonne
du modèle et la colonne observée à côté d’elle sont mesurées de la même
façon, si bien qu’un écart entre elles porte sur l’*ajustement*, et non
sur deux notions différentes de l’incertitude.

Une conséquence mérite d’être énoncée, parce qu’elle diffère de
[`tab()`](https://bricenocenti.github.io/tabxplor/reference/tab.md) :
une régression est **toujours** fondée sur le plan, là où un tableau
croisé démarre sur l’effectif brut non pondéré et a besoin de
`design_effect = TRUE` pour le rejoindre. [Données pondérées et plans de
sondage](https://bricenocenti.github.io/tabxplor/articles/tabxplor-weights-fr.md)
explique cette échelle, les deux limites honnêtes d’une colonne observée
fondée sur le plan, et comment savoir si un fichier donné mérite un plan
complet.

## 6. Vérifier le modèle

### Lire le bilan du modèle

Le bas de tableau résume le bilan de santé du modèle ; les statistiques
qui apparaissent dépendent de la famille :

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
- **Association globale** — un test par prédicteur : *cette variable
  est-elle liée à la variable à expliquer, tout court ?* Un bloc
  d’étoiles face à une modalité de référence ne peut pas y répondre, et
  c’est pourquoi un facteur à plusieurs modalités a besoin d’autre chose
  que de ses cases.
- Les modèles linéaires (gaussiens) ajoutent les habituels **R² / R²
  ajusté / F / σ** ; les modèles de comptage ajoutent la **dispersion de
  Pearson (φ)**.

Pour les modèles pondérés, un ensemble réduit est rapporté (un test de
Wald contre le modèle nul, un pseudo-R² de Nagelkerke / Cox–Snell, un
AIC de Rao–Scott), car les quantités fondées sur la vraisemblance ne
s’appliquent pas à un ajustement fondé sur le plan.

### Les cinq vérifications du modèle

Le même bas de tableau porte cinq **vérifications du modèle**, et il n’y
a rien à apprendre au-delà des cinq noms : chacun désigne une hypothèse,
et la parenthèse désigne l’instrument qui l’a mesurée. Quatre
s’affichent par défaut. La **linéarité** est celle qu’on demande par son
nom, ou avec `stats = "all"` — parce qu’elle réajuste le modèle une fois
par prédicteur continu, et parce que sa moitié gratuite est de toute
façon à l’écran (voir le tableau des formes ci-dessous).

``` r

tab_reg(gss_simple, "married", c("race", "age", "rincome", "relig"), stats = c("n", "linearity", "dispersion", "influence", "collinearity"))
```

Logistic regression: married by race, age +2 more

[TABLE]

Les lire dans l’ordre où elles s’affichent, qui est l’ordre de ce que
chacune menace — d’abord ce que le nombre *signifie*, puis si son
intervalle est digne de confiance, puis sa fragilité :

- **Proportionnalité (Brant)** — pour une variable à expliquer ordinale,
  *un seul rapport de cotes cumulé suffit-il à chaque seuil ?* Affichée
  par défaut dans ce cas, et de toute façon un réajustement, parce qu’un
  rapport de cotes cumulé qui échoue au test n’est pas un nombre mais
  une fiction. Quand il échoue, `measure = "difference"` est la façon
  robuste de continuer à lire le même ajustement (le `D` de Somers bouge
  à peine quand l’hypothèse se rompt) ; `family = "multinomial"` est la
  façon de cesser tout à fait de la supposer.
- **Linéarité** — *l’effet de ce prédicteur est-il vraiment une seule
  droite ?* Une ligne par prédicteur continu, et la ligne où se voit une
  mauvaise forme est rarement la seule qu’elle abîme. Les deux sections
  suivantes portent sur sa lecture et sur son remède.
- **Dispersion (ET robuste/du modèle)** — *les erreurs-types sont-elles
  assez larges ?* Environ 1 signifie que l’hypothèse de variance de la
  famille tient ; 1,4 signifie que les intervalles devraient être
  environ 40 % plus larges qu’ils ne le sont. Pour une variable de
  comptage, `family = "quasipoisson"` corrige exactement cela, et la
  ligne revient alors à environ 1 (tandis que la dispersion de Pearson φ
  continue de signaler la surdispersion elle-même). Sous un plan de
  sondage, elle dit simplement ce que le plan a changé — les intervalles
  sont déjà ceux du plan.
- **Colinéarité (max VIF)** — *les données peuvent-elles distinguer ces
  prédicteurs ?* À partir de 5 environ, deux prédicteurs mesurent en
  grande partie la même chose et leurs intervalles sont gonflés ; le bas
  de tableau signale la case **à partir de 10**, le chiffre des manuels.
  La colinéarité ne biaise rien : c’est la seule vérification qui soit
  une mise en garde plutôt qu’un problème.
- **Influence (max dfbetas)** — *un seul enquêté porte-t-il le résultat
  ?* La quantité maximale, en erreurs-types, dont le retrait d’un seul
  enquêté déplace un coefficient. Avec des milliers d’enquêtés, c’est
  normalement rassurant de petitesse, et c’est bien le but. Attention :
  l’influence n’est pas la même chose que le fait d’être atypique — une
  réponse surprenante venant d’un enquêté par ailleurs ordinaire ne
  déplace rien.

Deux choses à savoir. Un test de courbure sur un prédicteur peut capter
la mauvaise forme d’un **autre** prédicteur lorsque les deux sont
fortement corrélés — une raison de plus de lire la ligne de colinéarité
à côté. Et avec un grand échantillon d’enquête, presque toute *p*-value
de diagnostic finit significative, ce qui explique que trois des cinq
indiquent plutôt une grandeur.

### La forme d’un prédicteur continu

La ligne de linéarité dit *si* une seule droite suffit. Le petit
**tableau des formes** imprimé sous le bas de tableau dit *quelle forme
les données ont* — et il ne coûte rien, parce qu’aucun modèle n’y
intervient : la variable à expliquer est simplement découpée en dix
tranches d’effectif égal du prédicteur, et on prend la moyenne dans
chacune.

Le tableau ci-dessous en porte un de chaque, et c’est pourquoi il vaut
la peine d’être lu de près : **`tvhours` a exactement la forme qu’un
coefficient suppose** — une chute régulière, si bien que son nombre
unique la décrit honnêtement — tandis que **`age` a une forme qu’aucun
coefficient ne peut exprimer**.

``` r

tab_reg(gss_simple, "married", c("race", "age", "tvhours"), family = "binomial")
```

Logistic regression: married by race, age +1 more

[TABLE]

| outcome | numeric predictor | observed range | observed shape (central 95%) |
|----|----|----|----|
| p = %_(Married) ; log(p/(1-p)) | age | 13-57% (OR 8.7) | ![](data:image/svg+xml;base64,PHN2ZyBjbGFzcz0idHgtc3BhcmsiIHdpZHRoPSIxOTIuNiIgaGVpZ2h0PSI0NCIgdmlld2JveD0iMCAwIDE5Mi42IDQ0IiBhcmlhLWhpZGRlbj0idHJ1ZSI+PHBvbHlsaW5lIHBvaW50cz0iMS4zLDQyLjcgMTEuMywzMC45IDIxLjMsMTMuMSAzMS4zLDcuMiA0MS4zLDEuMyA1MS4zLDEuMyA2MS4zLDEuMyA3MS4zLDEuMyA4MS4zLDEuMyA5MS4zLDEuMyAxMDEuMywxLjMgMTExLjMsMS4zIDEyMS4zLDEuMyAxMzEuMywxLjMgMTQxLjMsMS4zIDE1MS4zLDEuMyAxNjEuMyw3LjIgMTcxLjMsNy4yIDE4MS4zLDEzLjEgMTkxLjMsMTMuMSIgZmlsbD0ibm9uZSIgc3Ryb2tlPSJjdXJyZW50Q29sb3IiIHN0cm9rZS13aWR0aD0iMi42IiBzdHJva2UtbGluZWpvaW49InJvdW5kIiBzdHJva2UtbGluZWNhcD0icm91bmQiPjwvcG9seWxpbmU+PC9zdmc+) |
|  | tvhours | 31-53% (OR 2.6) | ![](data:image/svg+xml;base64,PHN2ZyBjbGFzcz0idHgtc3BhcmsiIHdpZHRoPSIxOTIuNiIgaGVpZ2h0PSI0NCIgdmlld2JveD0iMCAwIDE5Mi42IDQ0IiBhcmlhLWhpZGRlbj0idHJ1ZSI+PHBvbHlsaW5lIHBvaW50cz0iMS4zLDMwLjkgMTEuMywxMy4xIDIxLjMsMS4zIDMxLjMsMS4zIDQxLjMsMS4zIDUxLjMsMS4zIDYxLjMsNy4yIDcxLjMsMTMuMSA4MS4zLDE5LjAgOTEuMywxOS4wIDEwMS4zLDI1LjAgMTExLjMsMzAuOSAxMjEuMywzMC45IDEzMS4zLDMwLjkgMTQxLjMsMzYuOCAxNTEuMywzNi44IDE2MS4zLDM2LjggMTcxLjMsNDIuNyAxODEuMyw0Mi43IDE5MS4zLDQyLjciIGZpbGw9Im5vbmUiIHN0cm9rZT0iY3VycmVudENvbG9yIiBzdHJva2Utd2lkdGg9IjIuNiIgc3Ryb2tlLWxpbmVqb2luPSJyb3VuZCIgc3Ryb2tlLWxpbmVjYXA9InJvdW5kIj48L3BvbHlsaW5lPjwvc3ZnPg==) |

Une ligne par prédicteur continu. Elle se lit en trois temps.

**1. Lire l’étendue d’abord, pas l’image.** `13-57%` est la phrase la
plus simple de tout le tableau : *dans la tranche de `age` où le mariage
est le plus rare, 13 % sont mariés ; dans celle où il est le plus
fréquent, 57 %*. Ce sont de vrais pourcentages de vraies personnes,
comptés, sans aucun modèle dedans. Le chiffre entre parenthèses est la
même distance écrite dans la comparaison propre au modèle — `(OR 8.7)`,
un rapport de cotes de près de neuf — qu’on peut donc tenir contre
l’échelle de couleurs déjà utilisée pour les cases (`1.2`, `1.5`, `2`,
`4`). Neuf est bien au-delà du dernier cran : `age` compte énormément.

L’étendue parle toujours la langue propre de la variable à expliquer —
un pourcentage de personnes pour une variable oui/non, une moyenne pour
une variable numérique — et elle ne change pas quand on change `measure`
ou `link`, parce que c’est ce qui a été compté. Seule la parenthèse
change.

C’est le seul nombre qu’un prédicteur continu n’a jamais dans le tableau
lui-même. Une case comme `1/2.35*** (28 %)` montre un effet *et* le
pourcentage sur lequel il porte — mais `age` n’a pas de catégories, donc
pas de pourcentage unique à y mettre. La courbe en a un par tranche, et
l’étendue observée n’est que ses deux extrémités.

**2. Ensuite lire la forme, et seulement pour sa forme.** L’axe
horizontal est le prédicteur tel que le modèle le voit, et chaque point
repose sur le même nombre de personnes : ce qu’on voit est donc *où* les
choses se passent. Trois lectures comptent :

- une montée ou une chute **droite** — un seul nombre la décrit, rien à
  faire ;
- un **coude** ou un **plateau** — l’effet n’est pas le même partout, et
  les remèdes ci-dessous y répondent ;
- un **retournement**, qui monte puis descend, comme `age` ici — celui
  qu’un coefficient unique ne peut pas exprimer du tout, parce qu’il
  rapporte une seule direction pour une variable qui en a deux. `age`
  monte fortement, s’aplatit, puis retombe : le `1.45` par deux
  écarts-types imprimé sur sa ligne est la moyenne d’une montée et d’une
  descente, un nombre qui ne décrit personne. `tvhours` est le
  contraste, et le cas rassurant : il descend d’un bout à l’autre, si
  bien que son `1/1.69` est vraiment *l’*effet de regarder davantage la
  télévision, et il n’y a rien à y faire.

Les dégâts ne restent pas dans la ligne fautive. Laisser `age` se
courber au lieu d’aller tout droit déplace d’environ un quart le rapport
de cotes de la tranche de revenu la plus élevée, et fait basculer le
verdict d’une autre tranche au seuil de 5 % — dans un modèle où rien
d’autre que ce tableau des formes ne laisse deviner le problème. C’est
toute la raison de le regarder.

⚠ **Ne pas comparer deux images à l’œil.** Chaque courbe est dessinée à
sa propre échelle et remplit la hauteur quelle que soit sa taille —
c’est ce qui rend les petites formes visibles. `age` et `tvhours`
paraissent aussi spectaculaires l’une que l’autre ci-dessus ; les
étendues disent que l’une est un facteur neuf et l’autre plutôt deux et
demi. **L’image répond à *quelle forme*, l’étendue répond à *quelle
taille*.** Seuls les nombres se comparent.

**3. Vérifier le gris et le `ns` avant d’y croire.** Une courbe plus
petite que son propre bruit d’échantillonnage est grisée et marquée `ns`
: elle se lit comme une droite plate, si convaincante soit-elle. Les
deux mêmes prédicteurs, sur un échantillon de 200 lignes des mêmes
données :

``` r

set.seed(20260823)
small <- gss_simple[sample(nrow(gss_simple), 200), ]
tab_reg(small, "married", c("race", "age", "tvhours"), family = "binomial")
```

Logistic regression: married by race, age +1 more

[TABLE]

[TABLE]

Toujours des courbes, à l’œil. Elles n’en sont pas : sur si peu de
personnes, les tranches oscillent d’autant toutes seules — et les
étendues, `11-69 %` et `16-72 %`, sont plus larges que celles mesurées
sur tout l’échantillon, ce qui est le signe qui ne trompe pas.

⚠ `ns` porte sur la **forme**, non sur le prédicteur. Dix tranches
portent dix moyennes, pas deux cents enquêtés : un effet réel peut donc
être parfaitement significatif dans le tableau et rester illisible comme
forme. Les étoiles de la ligne du prédicteur jugent l’effet ; le tableau
des formes ne juge que la fiabilité de l’image.

Pour une variable à expliquer **ordinale** ou **multinomiale**, le
tableau ne dessine que le premier seuil ou la première catégorie —
utiliser
[`reg_check_plots()`](https://bricenocenti.github.io/tabxplor/reference/reg_check_plots.md)
dans ce cas, qui les dessine tous, et où un écart aux cotes
proportionnelles devient visible.

`options(tabxplor.shape_table = "console")` garde le tableau des formes
là où l’on travaille et hors des exports ; `"no"` le retire partout.

### Corriger une forme

`shape =` est la manière de corriger ce que le tableau vient de montrer,
sans sortir du tableau. La réponse la plus lisible est en général de
découper le prédicteur en groupes — il devient alors un facteur
ordinaire, donc il obtient un rapport de cotes par groupe, sa propre
contrepartie observée, des effectifs et des couleurs, et la forme
devient visible dans les nombres imprimés eux-mêmes :

``` r

tab_reg(gss_simple, "married", c("race", "age"), family = "binomial",
        shape = c(age = "quintiles"), empirical = TRUE)
```

Logistic regression: married by race, age

[TABLE]

L’alternative parcimonieuse garde une seule variable et ajoute un terme
de courbure, de sorte que `age` prend deux lignes : la pente à la
moyenne, et `age²`, qui dit si la pente s’aplatit (en dessous de 1) ou
s’accélère (au-dessus de 1) à mesure qu’on s’en éloigne.

``` r

tab_reg(gss_simple, "married", c("race", "age"), family = "binomial",
        shape = c(age = "quadratic"))
```

Logistic regression: married by race, age

[TABLE]

| outcome | numeric predictor | observed range | observed shape (central 95%) |
|----|----|----|----|
| p = %_(Married) ; log(p/(1-p)) | age | 13-57% (OR 8.7) | ![](data:image/svg+xml;base64,PHN2ZyBjbGFzcz0idHgtc3BhcmsiIHdpZHRoPSIxOTIuNiIgaGVpZ2h0PSI0NCIgdmlld2JveD0iMCAwIDE5Mi42IDQ0IiBhcmlhLWhpZGRlbj0idHJ1ZSI+PHBvbHlsaW5lIHBvaW50cz0iMS4zLDQyLjcgMTEuMywzMC45IDIxLjMsMTMuMSAzMS4zLDcuMiA0MS4zLDEuMyA1MS4zLDEuMyA2MS4zLDEuMyA3MS4zLDEuMyA4MS4zLDEuMyA5MS4zLDEuMyAxMDEuMywxLjMgMTExLjMsMS4zIDEyMS4zLDEuMyAxMzEuMywxLjMgMTQxLjMsMS4zIDE1MS4zLDEuMyAxNjEuMyw3LjIgMTcxLjMsNy4yIDE4MS4zLDEzLjEgMTkxLjMsMTMuMSIgZmlsbD0ibm9uZSIgc3Ryb2tlPSJjdXJyZW50Q29sb3IiIHN0cm9rZS13aWR0aD0iMi42IiBzdHJva2UtbGluZWpvaW49InJvdW5kIiBzdHJva2UtbGluZWNhcD0icm91bmQiPjwvcG9seWxpbmU+PC9zdmc+) |

`shape = c(x = "log")` et `"sqrt"` sont les deux autres — des rendements
décroissants, la forme qu’ont d’ordinaire les données de revenu. Tout le
reste continue de fonctionner : la contrepartie observée `Obs_*` est
estimée avec la même forme, donc la comparaison modèle-observé compare
toujours des grandeurs de même nature, et la ligne de linéarité
disparaît pour un prédicteur déjà corrigé.

(Une base [`poly()`](https://rdrr.io/r/stats/poly.html) ou de splines
n’est délibérément jamais émise : le moteur d’effets marginaux y renvoie
zéro en silence. Si l’on en atteint une via une `formula`, un
avertissement le signale.)

### Les graphiques de vérification : `reg_check_plots()`

``` r

reg_check_plots(t)                       # les panneaux par défaut, une grille titrée par modèle
reg_check_plots(t, check = "all")        # avec en plus la dispersion et la colinéarité
reg_check_plots(t, check = "linearity")  # un seul
```

Les mêmes noms que les lignes de bas de tableau, un panneau chacun, et
**une grille titrée par modèle** — chacune avec les panneaux permis par
sa propre famille, si bien qu’un tableau mêlant une variable à expliquer
binaire et une variable ordinale est vérifié correctement. Les données
sont normalement retrouvées toutes seules, à partir du nom avec lequel
le tableau a été construit ; `data =` ne sert que si le tableau vient
d’une expression plutôt que d’un objet nommé. `check = "auto"` laisse de
côté la **dispersion** et la **colinéarité**, dont la ligne de bas de
tableau dit déjà tout ; `check = "all"` les rétablit.

Ce sont des compagnons **pédagogiques**, pas un outil de décision :
chaque verdict qu’ils illustrent est déjà une ligne de bas de tableau,
dans tous les exports, sans aucun package graphique installé. Ils
existent pour montrer à quoi ressemble une violation.
`theme = "print_ready"` en donne une version en niveaux de gris pour une
annexe de mémoire ; on peut aussi passer directement un modèle ajusté au
lieu d’un tableau.

Deux panneaux méritent un second regard. La **linéarité** compare la
courbe observée à *la forme que le modèle ajuste* — une droite
d’ordinaire, une parabole dès que `shape = "quadratic"` a ajouté un
terme de courbure — et reste ainsi honnête sur un prédicteur déjà
corrigé. Avec une variable à expliquer ordinale ou nominale, elle trace
**une courbe par seuil** (par catégorie, face à la référence, pour une
multinomiale) : des courbes parallèles, c’est l’hypothèse des cotes
proportionnelles qui tient — ce que le test de Brant chiffre en bas de
tableau, mais que le panneau *Proportionnalité*, qui porte sur les
facteurs, ne peut pas montrer pour un prédicteur continu.

À noter, les contrats opposés :
[`forest_plot()`](https://bricenocenti.github.io/tabxplor/reference/forest_plot.md)
ne réajuste jamais (les résultats sont dans le tableau),
[`reg_check_plots()`](https://bricenocenti.github.io/tabxplor/reference/reg_check_plots.md)
réajuste toujours (les résidus, eux, n’y sont pas).

## 7. Graphiques

### Les résultats : `forest_plot()`

[`forest_plot()`](https://bricenocenti.github.io/tabxplor/reference/forest_plot.md)
trace le tableau fini — chaque effet avec son intervalle de confiance et
sa couleur, un panneau par colonne de modèle :

``` r

t <- tab_reg(gss_simple, "married", c("race", "rincome"), family = "binomial")
forest_plot(t)
```

Il **lit le tableau et ne réajuste jamais rien** : le graphique ne peut
donc pas contredire les nombres imprimés. Chaque moustache couvre
l’intervalle de la case, chaque ligne de grille est l’un des seuils de
couleur (déplacés avec
[`set_color_breaks()`](https://bricenocenti.github.io/tabxplor/reference/set_color_palette.md),
l’axe suit), et la moustache prend la couleur de la case en entier —
c’est pourquoi le graphique n’a pas besoin d’étoiles. La valeur est
imprimée juste au-dessus, et le carré en son centre est d’autant plus
grand qu’il y a de personnes derrière (`center = "estimate"` n’imprime
que la valeur). Un tableau qui mélange plusieurs familles reçoit un axe
par panneau, chacun dans son unité. La fonction renvoie un `ggplot`
ordinaire : `+ ggplot2::labs(...)` et `ggsave()` fonctionnent comme
d’habitude.

Les trois façons de lire la significativité deviennent trois choses qui
se voient :

| `color_signif` | dans le tableau | dans le graphique |
|----|----|----|
| `"ignore"` | couleur selon la taille de l’effet | où se place le point par rapport à la ligne nulle |
| `"grey_non_signif"` | gris si l’intervalle contient la valeur nulle | si la moustache traverse la ligne nulle |
| `"guaranteed_effect"` | couleur selon la borne la plus proche du nul | la distance entre la ligne nulle et le bout le plus proche de la moustache |

### Observé contre modélisé, tracé honnêtement

Avec `empirical = TRUE`, chaque effet modélisé porte sa contrepartie
observée (brute), et le graphique la dessine comme un **point creux muni
d’un crochet** — la marge d’erreur *de la différence entre les deux* :

``` r

t <- tab_reg(gss_simple, "married", c("race", "rincome"),
             link = "ratio", empirical = TRUE)          # risques relatifs
forest_plot(t)
```

Une seule question à se poser : **le point plein est-il en dehors du
crochet ?** S’il l’est, l’ajustement a déplacé l’effet plus que le bruit
— et c’est exactement le test que fait le tableau, au dernier chiffre
près.

Ce ne sont volontairement *pas* deux intervalles côte à côte. Juger deux
intervalles à leur recouvrement est une erreur connue (Schenker &
Gentleman 2001), et c’est pire ici : l’estimation brute et l’estimation
ajustée sont calculées sur les mêmes personnes, donc corrélées. Le bon
intervalle est celui *de la différence*, et c’est le crochet.
`observed = "ci"` redonne la figure classique à deux intervalles pour
qui y tient.

Un trait pointillé sans crochet signifie que l’écart n’a pas pu être
testé — le plus souvent un rapport de cotes conditionnel, qui bouge sous
ajustement même sans rien à ajuster (voir *Trois façons de se tromper*,
plus haut).

### Pour aller plus loin

- [Interpréter un modèle de
  régression](https://bricenocenti.github.io/tabxplor/articles/tabxplor-reading-a-regression-fr.md)
  — la même matière enseignée comme une seule analyse suivie, avec les
  habitudes de lecture qui vont avec.
- [Introduction à
  tabxplor](https://bricenocenti.github.io/tabxplor/articles/tabxplor-fr.md)
  — les tableaux croisés et les repères de couleur.
- [Données pondérées et plans de
  sondage](https://bricenocenti.github.io/tabxplor/articles/tabxplor-weights-fr.md)
  — pondération et plans de sondage, pour les deux producteurs.
- [`tab_reg()`](https://bricenocenti.github.io/tabxplor/reference/tab_reg.md)
  est aussi disponible **sans écrire de code R**, comme une analyse
  **Modèles de régression** dans le module
  [jamovi](https://www.jamovi.org/) — installer *tabxplor* depuis la
  bibliothèque de modules de jamovi (voir [Introduction à
  tabxplor](https://bricenocenti.github.io/tabxplor/articles/tabxplor-fr.md)).
- [`?tab_reg`](https://bricenocenti.github.io/tabxplor/reference/tab_reg.md)
  pour chaque argument (groupés par usage), et
  [`?tab_reg`](https://bricenocenti.github.io/tabxplor/reference/tab_reg.md)
  → *Details* pour les choix de modélisation.
- **Hors périmètre**, et à estimer avec leurs propres packages : les
  modèles de survie / de Cox, les modèles mixtes (multiniveaux) et
  l’agrégation sur des données multi-imputées.
- Sur la comparaison d’un coefficient brut et d’un coefficient ajusté,
  la référence canonique est Clogg, Petkova & Haritou (1995, *AJS*
  100(5)), avec le commentaire d’Allison dans le même numéro — c’est ce
  que calcule `color = "adjustment"`, généralisé ici aux GLM, aux plans
  de sondage et aux effets marginaux. Pour les modèles logit emboîtés,
  la décomposition KHB (Karlson, Holm & Breen 2012) sépare la part du
  changement qui relève de la confusion de celle qui relève du
  changement d’échelle.
