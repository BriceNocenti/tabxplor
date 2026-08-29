# Données pondérées et plans de sondage

``` r

library(tabxplor)

# Fixer la langue des DEUX manieres, comme les autres documents traduits : options(tabxplor.lang)
# pilote la legende des couleurs et la note de bas de tableau, tandis que les libelles du bilan du
# modele passent par gettext, que seul LANGUAGE atteint. Par defaut la langue vaut "auto" = la
# locale ambiante : la sortie ne doit pas dependre de la machine.
options(tabxplor.lang = "fr")
Sys.setenv(LANGUAGE = "fr")
library(dplyr)

# Les tableaux sont rendus comme les vrais tableaux html de tabxplor (le reglage recommande au
# quotidien) ; la feuille de style partagee est emise une fois par tab_css() ci-dessous, et les
# infobulles restent coupees ici.
options(tabxplor.print = "html")
options(tabxplor.tab_kable_css = FALSE)
options(tabxplor.tab_kable_tooltips = FALSE)

options(cli.num_colors = 256)
set_color_palette(theme = "light")
```

La plupart des fichiers d’enquête sont livrés avec une **pondération** :
un nombre qui dit combien de personnes de la population chaque répondant
représente. L’argument `wt =` l’utilise, et chaque pourcentage, chaque
moyenne du tableau devient une estimation de la *population*, et non
plus des seules personnes interrogées.

C’est la partie facile. La partie délicate, c’est la **marge d’erreur**
autour de ces pourcentages — les intervalles de confiance, les étoiles
de significativité, les couleurs et les tests. Là, tabxplor propose
**trois niveaux**, et la note de bas de tableau dit toujours à quel
niveau on se trouve.

| ce qu’on passe | niveau | ce que portent les intervalles et les tests |
|----|----|----|
| `wt = w` | 1 | l’estimation pondérée, sur le *n* brut — aucun effet de plan |
| `wt = w` + `design_effect = TRUE` | 2 | l’inégalité des poids, exactement — aveugle aux grappes et au calage |
| un plan `survey` comme `data` | 3 | le plan complet : strates, grappes, `fpc`, calage sur marges |

Ce document sert à choisir entre les trois. Les tableaux croisés
eux-mêmes sont dans [Introduction à
tabxplor](https://bricenocenti.github.io/tabxplor/articles/tabxplor-fr.md),
et le cadre de la régression dans [Tableaux de
régression](https://bricenocenti.github.io/tabxplor/articles/tabxplor-reg-fr.md).

## Niveau 1 — pourcentages pondérés, marges d’erreur simples (le défaut)

``` r

tab(mon_enquete, diplome, emploi, wt = poids, pct = "row")
#> Pondéré par poids ; les intervalles de confiance et les tests utilisent l'effectif non pondéré.
```

Les pourcentages sont pondérés ; les marges d’erreur, elles, comptent
simplement les répondants réels. C’est la convention de presque tous les
manuels et de tous les logiciels « presse-bouton », et c’est pourquoi
c’est le défaut ici. Mais quand les poids sont **inégaux**, elle est
optimiste : un échantillon où certaines personnes comptent pour quatre
autres porte moins d’information que sa taille ne le laisse croire, et
les intervalles ressortent le plus souvent un peu trop étroits.

## Niveau 2 — `design_effect = TRUE`

``` r

tab(mon_enquete, diplome, emploi, wt = poids, pct = "row", design_effect = TRUE)
options(tabxplor.design_effect = TRUE)   # ou une seule fois, pour toute la session
#> Pondéré par poids ; les intervalles de confiance et les tests tiennent compte de la pondération.
```

L’inégalité des poids est désormais prise en compte dans **chaque**
intervalle, étoile, seuil de couleur et test d’ensemble. Ce n’est pas
une règle approchée : une variable de pondération *est* un plan de
sondage — le plus simple, sans grappes ni strates — et tabxplor en
calcule la variance exactement, en reproduisant à la dernière décimale
le package de référence
[survey](https://CRAN.R-project.org/package=survey).

Voici ce que cela donne sur des données réelles. On dote `gss_simple`
d’une pondération fictive (les célibataires sont sous-représentés dans
la plupart des enquêtes : ils comptent donc davantage), et on construit
deux fois le même tableau :

``` r

gss_w <- dplyr::mutate(gss_simple, w = ifelse(marital %in% "Never married", 2.5, 0.8))

tab(gss_w, race, party3, wt = w, pct = "row", ci = "cell", na = "drop")
```

[TABLE]

``` r

tab(gss_w, race, party3, wt = w, pct = "row", ci = "cell", na = "drop",
    design_effect = TRUE)
```

[TABLE]

Mêmes pourcentages, crochets plus larges, et une note de bas de tableau
qui dit ce qui a changé. Derrière, les 16 292 répondants blancs
(`White`) n’en valent plus qu’environ 11 800 : c’est le prix de
l’inégalité des poids.

Dès lors qu’on pondère, le niveau 2 est la lecture honnête de ses
propres chiffres, et il ne coûte rien. S’il n’est pas le défaut, c’est
seulement parce que l’activer déplace tous les chiffres de tous les
tableaux déjà produits : cela doit rester un geste délibéré et visible,
pas un changement silencieux. Une raison de plus de l’activer : dans un
tableau de régression, les colonnes observées sont *toujours* au niveau
2, et c’est donc ce qui rend directement comparables un pourcentage de
[`tab()`](https://bricenocenti.github.io/tabxplor/reference/tab.md) et
un pourcentage observé de
[`tab_reg()`](https://bricenocenti.github.io/tabxplor/reference/tab_reg.md)
sur les mêmes données.

## Niveau 3 — un plan de sondage `survey`

Les vraies enquêtes ne sont pas tirées en piochant des noms au hasard
dans tout le pays. Elles sont en général **stratifiées** (tirées
séparément par région, par taille de commune…), souvent **en grappes**
(on tire d’abord quelques quartiers, puis plusieurs ménages dans
chacun), et leurs poids sont le plus souvent **calés sur marges**, pour
que l’échantillon retrouve des totaux de population connus par âge,
sexe, région. Ces trois faits ne sont pas dans la variable de poids :
ils vivent dans des variables supplémentaires. Quand le fichier les
contient, on construit un plan avec le package
[survey](https://CRAN.R-project.org/package=survey) et on le passe comme
`data` :

``` r

library(survey)
d <- svydesign(ids = ~psu, strata = ~strate, weights = ~w, data = mon_enquete, nest = TRUE)
tab(d, race, marital, pct = "row", color = TRUE, test = TRUE)
#> Estimations, intervalles et tests pondérés tiennent compte du plan d'échantillonnage (survey-design).
```

Tout suit alors le plan : les estimations, les pvalues, et chaque
intervalle de confiance, étoile et seuil de couleur. Les plans à poids
de réplication
([`svrepdesign()`](https://rdrr.io/pkg/survey/man/svrepdesign.html)) et
les plans à deux phases ne sont pas gérés : tabxplor les refuse.

## Ce que le niveau 2 peut, et ne peut pas, faire

Le niveau 2 voit les poids, et rien d’autre. C’est important, car les
trois choses qu’il ne voit pas ne tirent **pas** dans le même sens :

|  | vu au niveau 2 | demande un plan (niveau 3) |
|----|----|----|
| inégalité des poids | **oui, exactement** |  |
| strates, calage, population finie |  | **rétréciraient** les intervalles — de quelques pour cent au plus |
| **grappes** |  | peuvent les rendre **beaucoup plus larges** |

Les deux corrections qui manquent au niveau 2 sont donc très inégales.
Ignorer les strates et le calage coûte quelques pour cent, et dans le
sens *prudent* : les intervalles sont légèrement trop larges. Ignorer
les **grappes** va dans l’autre sens, et peut être énorme : sur une
véritable enquête en grappes auprès d’écoles, une moyenne que le niveau
2 annonce à ±3 points mérite en réalité ±9. Sur l’enquête de santé
américaine NHANES, la répartition par origine ressort **neuf fois** trop
précise, parce que cette enquête est justement bâtie autour de cette
variable. Ces effets ne se compensent pas : dès qu’il y a des grappes,
ce sont elles qui l’emportent.

**Quelles enquêtes sont en grappes ?** Pas celles tirées d’un registre,
ni celles menées par internet, téléphone ou courrier : là, le niveau 2
est simplement la bonne réponse, si inégaux que soient les poids. Mais
les enquêtes en **face-à-face** auprès des ménages le sont par
construction, parce qu’envoyer des enquêteurs à des adresses dispersées
coûterait trop cher. L’*Enquête Emploi* de l’Insee, par exemple, est
tirée par groupes d’environ **vingt logements voisins** ; or les voisins
se ressemblent, et vingt entretiens dans une même rue en apprennent
moins que vingt entretiens répartis dans tout le pays.

**Est-ce que cela compte pour le tableau qu’on a sous les yeux ?** En
général beaucoup moins que ces chiffres ne le suggèrent, et il existe un
test simple. L’effet de grappe s’annule en grande partie dans une
*comparaison*, dès lors que les deux groupes comparés se retrouvent dans
les mêmes quartiers. Une seule question à poser, donc, sur la variable
en ligne :

> Varie-t-elle **à l’intérieur** d’un quartier, ou **définit**-elle un
> quartier ?

Sexe, âge, diplôme, profession, revenu, opinions : tout cela varie à
l’intérieur de n’importe quel quartier, et une comparaison entre leurs
catégories est donc quasi juste au niveau 2. Région, urbain/rural,
taille de commune, type de quartier, densité d’immigration locale : là,
la variable *est* le quartier, et le niveau 2 se trompe autant que sur
un pourcentage isolé. Les mesures le confirment : sur le fichier en
grappes où une moyenne est trois fois trop précise, l’*écart* entre deux
groupes n’est trop précis que de 13 %, et un coefficient de régression
de 8 % — or ce sont bien des écarts que testent les couleurs et les
étoiles de tabxplor.

**Vérifier ce que contient réellement le fichier avant de viser le
niveau 3.** Chercher une variable de strate, de grappe ou de calage
(`strate`, `grappe`, `psu`, `nomen`…). Beaucoup de fichiers de
production et de recherche diffusent un seul poids calé et rien d’autre
— la grappe est une petite zone géographique, elle est donc retenue pour
des raisons de confidentialité — et sur un tel fichier un plan `survey`
ne porterait que l’inégalité des poids, c’est-à-dire exactement ce que
le niveau 2 donne déjà. L’*European Social Survey* est un cas
intermédiaire utile : il publie bien ses grappes et ses strates, mais
dans un fichier *séparé* qu’il faut télécharger et fusionner.

## La régression sur données pondérées

Il n’y a que **deux façons de donner ses poids à tabxplor**, et ce sont
les deux mêmes : soit on passe une variable de pondération à `wt =`,
soit on construit le plan une fois pour toutes et on passe **le plan
lui-même** comme `data`.

``` r

tab_reg(data, "outcome", c("pred1", "pred2"), wt = "weight")

library(survey)
d <- svydesign(ids = ~psu, strata = ~strate, weights = ~w, data = mon_enquete, nest = TRUE)
tab_reg(d, "outcome", c("pred1", "pred2"), empirical = TRUE)
```

Tout suit alors ce plan, dans un seul régime : les coefficients
`Model_*` et leurs intervalles
([`survey::svyglm()`](https://rdrr.io/pkg/survey/man/svyglm.html)), les
effets moyennés sur l’échantillon, la mise à l’échelle par écart-type
des prédicteurs numériques, le test de l’écart entre modèle et observé —
**et les colonnes observées `Obs_*`**, dont les intervalles sont
construits sur la variance de plan de chaque case. Ce dernier point est
ce qui rend la comparaison possible : la colonne du modèle et la colonne
observée placée à côté sont mesurées de la même façon, si bien qu’un
écart entre elles porte sur l’*ajustement*, et non sur deux notions
différentes de l’incertitude.

Deux limites, énoncées franchement. Une comparaison *entre deux cases*
(un rapport de cotes observé, une différence de proportion) ignore la
covariance de plan entre elles, et tombe donc à quelques pour cent de la
réponse exacte — contre les 15 à 25 % d’écart qu’elle atteignait lorsque
ces colonnes étaient calculées comme si le tirage était aléatoire
simple. Et une variable à expliquer *nominale* n’a pas de colonne
observée : sa valeur brute est repliée dans la case du modèle, comme un
point, sans intervalle qu’on puisse fonder sur le plan.

### Pourquoi un `tab_reg()` pondéré n’est pas un `tab()` pondéré

[`tab_reg()`](https://bricenocenti.github.io/tabxplor/reference/tab_reg.md)
n’a pas le choix : ses colonnes observées doivent être mesurées comme la
colonne de modèle placée à côté, et celle-ci est fondée sur le plan par
construction
([`survey::svyglm()`](https://rdrr.io/pkg/survey/man/svyglm.html)).
[`tab()`](https://bricenocenti.github.io/tabxplor/reference/tab.md),
lui, a le choix, et garde comme défaut la convention descriptive — une
estimation pondérée sur l’effectif brut. Autrement dit,
[`tab_reg()`](https://bricenocenti.github.io/tabxplor/reference/tab_reg.md)
est toujours au **niveau 2** (ou au niveau 3 sous un plan de sondage),
là où
[`tab()`](https://bricenocenti.github.io/tabxplor/reference/tab.md)
démarre au niveau 1 ; `tab(design_effect = TRUE)` est ce qui aligne les
deux. La note de bas de chaque tableau dit lequel a servi.

## Les petits caractères

- **Le niveau 2 rétrécit parfois un intervalle.** C’est correct, ce
  n’est pas un bug. Si les poids se trouvent aller dans le même sens que
  ce qu’on mesure, un tirage inégal peut porter *plus* d’information
  qu’un tirage égal, et la taille d’échantillon effective ressort
  au-dessus du nombre de répondants.
- **Les degrés de liberté.** Une enquête en grappes repose en général
  sur peu de grappes — 15 à 60 est courant — et le niveau 3 rapporte
  chaque intervalle à ce nombre, ce qui l’élargit encore d’environ 8 %.
  Le niveau 2 n’a pas de grappes à compter, et ne peut donc pas le faire
  : c’est une seconde raison, distincte, pour laquelle la vraie marge
  d’erreur d’une enquête en face-à-face est plus large que ce qu’affiche
  le niveau 2.
- **Quand le niveau 3 échoue.** Si la variance d’un plan ne peut pas
  être calculée pour un tableau, celui-ci revient à la correction du
  niveau 2 et le dit dans sa note de bas de tableau, plutôt que de
  revendiquer un plan que ses chiffres ne portent pas.
- **Exact pour une case, prudent pour un écart.** Le niveau 3 est exact
  pour un pourcentage isolé ; pour un **écart** case-vs-référence, il ne
  peut pas porter la covariance entre les deux cases, et l’écart ressort
  donc légèrement trop large plutôt que trop étroit. Il ne donnera
  jamais une étoile que le plan ne soutient pas.
- **Effectifs pré-agrégés et coût.** Le niveau 2 a besoin des poids
  individuels : un tableau construit avec
  [`tab_counts()`](https://bricenocenti.github.io/tabxplor/reference/tab_counts.md)
  à partir d’effectifs publiés ne peut pas l’utiliser, et le dit dans sa
  note. Enfin, un tableau de niveau 3 coûte environ trois fois un
  tableau pondéré.

## Pour aller plus loin

- [Introduction à
  tabxplor](https://bricenocenti.github.io/tabxplor/articles/tabxplor-fr.md)
  — les tableaux croisés, les couleurs et les intervalles de confiance.
- [Tableaux de
  régression](https://bricenocenti.github.io/tabxplor/articles/tabxplor-reg-fr.md)
  — le cadre de la régression.
- [Interpréter un modèle de
  régression](https://bricenocenti.github.io/tabxplor/articles/tabxplor-reading-a-regression-fr.md)
  — une analyse suivie du premier tableau croisé à une phrase finie.
- [`?tab`](https://bricenocenti.github.io/tabxplor/reference/tab.md)
  pour `wt`, `design_effect` et `ci_method` ; `?tabxplor-options` pour
  les réglages de session.
- [`survey::svydesign()`](https://rdrr.io/pkg/survey/man/svydesign.html)
  pour les plans complexes (strates, grappes, calage sur marges,
  corrections de population finie).
