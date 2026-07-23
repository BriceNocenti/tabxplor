# Introduction à tabxplor

``` r
library(tabxplor)
```

`tabxplor` aide à **explorer les données avec des tableaux croisés, en
colorant les cases pour lire un tableau d’un coup d’œil**. Les cases
sur-représentées prennent des nuances de bleu, les sous-représentées
virent au rouge/orange : les régularités sautent aux yeux sans avoir à
scruter chaque nombre.

Tout est un `tibble`, donc le résultat fonctionne avec les verbes
habituels de `dplyr`, et les tableaux s’exportent vers Excel, HTML et
Markdown avec leurs repères de couleur. Les calculs lourds sous-jacents
tournent sur `data.table`.

Tout au long de cette introduction, nous utilisons `gss_simple`, une
version nettoyée de la General Social Survey
([`forcats::gss_cat`](https://forcats.tidyverse.org/reference/gss_cat.html)),
dont les modalités des facteurs ont été fusionnées et réordonnées.

``` r
gss_simple <- gss_cat_data_formatting()
```

## Vos premiers tableaux croisés

[`tab()`](https://bricenocenti.github.io/tabxplor/reference/tab.md) a
besoin d’un data frame, d’une **variable en ligne** et d’une **variable
en colonne**. Par défaut, il affiche des effectifs :

``` r
tab(gss_simple, marital, race)
```

[TABLE]

C’est le **tableau html** de tabxplor — ce que l’on obtient dans le
panneau Viewer de RStudio ou Positron avec l’option de session
recommandée, utilisée tout au long de cette introduction :

``` r
options(tabxplor.print = "html")
```

Sans l’option, le même tableau s’imprime dans la **console**, sous forme
de `tibble` coloré — même information, affichage plus léger :

``` r
tab(gss_simple, marital, race)
```

``` r-output
#> # A tabxplor tab: 7 × 5
#>   marital        White Black Other  Total
#>   <fct>            <n>   <n>   <n>    <n>
#> 1 Married        8 316   869   932 10 117
#> 2 Separated        437   196   110    743
#> 3 Divorced       2 676   495   212  3 383
#> 4 Widowed        1 475   262    70  1 807
#> 5 Never married  3 478 1 305   633  5 416
#> 6 NA                13     2     2     17
#> 7 Total         16 395 3 129 1 959 21 483
```

Ajoutez `pct = "row"` pour des pourcentages en ligne (ou `"col"` pour
des pourcentages en colonne). Une ligne/colonne **Total** et une colonne
d’effectifs (`n`) sont ajoutées automatiquement :

``` r
tab(gss_simple, marital, race, pct = "row")
```

[TABLE]

Quand la variable en colonne est **numérique**,
[`tab()`](https://bricenocenti.github.io/tabxplor/reference/tab.md)
affiche sa **moyenne** dans chaque ligne, au lieu de pourcentages :

``` r
tab(gss_simple, marital, age)
```

|               | age       |
|---------------|-----------|
| marital       | mean (sd) |
| Married       | 49 (σ15)  |
| Separated     | 45 (σ13)  |
| Divorced      | 51 (σ13)  |
| Widowed       | 72 (σ13)  |
| Never married | 34 (σ13)  |
| NA            | 52 (σ17)  |
| Total         | 47 (σ17)  |

Vous pouvez passer **plusieurs variables en ligne et en colonne à la
fois**.

``` r
tab(gss_simple, c(race, relig), c(party3, tvhours), na = "drop", pct = "row")
```

[TABLE]

`levels = "first"` ne garde que la première modalité de chaque facteur
en colonne, ce qui est pratique pour afficher de façon compacte de
nombreux facteurs binaires, comme des questions d’enquête à réponses
multiples, tous en même temps :

``` r
tab(gss_simple, relig, c(married, black, income25k), pct = "row", levels = "first", na = "drop", cleannames = TRUE)
```

|                   | married | black | income25k       |
|-------------------|---------|-------|-----------------|
| relig             | Married | Black | \$25000 or more |
| Protestant        | 50%     | 21%   | 32%             |
| Catholic          | 50%     | 4%    | 35%             |
| Other christian   | 44%     | 18%   | 35%             |
| Jewish            | 51%     | 3%    | 43%             |
| Buddhist/Hinduist | 51%     | 5%    | 47%             |
| Muslim            | 53%     | 34%   | 32%             |
| Other             | 37%     | 13%   | 37%             |
| None              | 37%     | 11%   | 37%             |
| Total             | 47%     | 15%   | 34%             |

Quelques autres arguments du quotidien : `na = "drop"` pour retirer les
valeurs manquantes de la base, `digits =` pour le nombre de décimales,
et `cleannames = TRUE` pour retirer des préfixes comme `"1-"` des noms
de modalités. Voir
[`?tab`](https://bricenocenti.github.io/tabxplor/reference/tab.md) pour
la liste complète.

## Pondération

L’argument `wt =` ajoute une pondération d’enquête. Chaque pourcentage
et chaque moyenne est alors **pondéré**, tandis que la taille
d’échantillon derrière les intervalles de confiance reste le nombre
réel, **non pondéré**, d’observations — la base honnête, par défaut, de
l’incertitude.

``` r
data(hdv2003, package = "questionr")
tab(hdv2003, nivetud, occup, wt = poids, pct = "row", na="drop", digits = 1)
```

Sous des poids **inégaux**, cet intervalle par défaut ne porte aucun
effet de plan, donc il est un peu trop étroit. La *taille d’échantillon
efficace* de Kish corrige cela : elle compte chaque observation selon ce
qu’elle apporte réellement, `n_eff = (somme des w)² / (somme des w²)`
(toujours au plus le *n* réel), et utilise `n_eff` à la place du *n*
brut. Activez-la et chaque intervalle de confiance pondéré du tableau —
proportions comme moyennes — s’élargit honnêtement :

``` r
options(tabxplor.kish_neff = TRUE)
```

C’est une approximation simple, à un seul degré (elle a besoin des poids
individuels, donc elle n’est pas disponible pour les tableaux construits
à partir d’effectifs déjà agrégés). Un résultat **pleinement fondé sur
le plan de sondage** — grappes, strates, erreurs-types exactes — n’est
disponible que pour les p-valeurs du Chi2 (facteurs) et les p-valeurs du
F d’ANOVA (variables en colonne numériques), via `test = "survey"` et
les arguments associés (voir l’argument `test =`) : les intervalles de
confiance ne sont pas couverts. Voir le paquet
[survey](https://CRAN.R-project.org/package=survey) pour plus
d’informations sur les plans de sondage. Les tableaux de régression
[`tab_reg()`](https://bricenocenti.github.io/tabxplor/reference/tab_reg.md)
avec pondération utilisent toujours l’effet de plan pour leurs
erreurs-types
([`vignette("tabxplor-reg")`](https://bricenocenti.github.io/tabxplor/articles/tabxplor-reg.md)),
mais la version de base n’utilise que la taille d’échantillon efficace
de Kish.

## Sous-tableaux

Donnez à
[`tab()`](https://bricenocenti.github.io/tabxplor/reference/tab.md) une
troisième variable comme `tab_vars` et il construit **un sous-tableau
par groupe** (ici, un par tranche de revenu). Le résultat est *groupé* :
les opérations `dplyr` s’appliquent alors à l’intérieur de chaque
sous-tableau.

``` r
tab(gss_simple, race, party3, rincome, na = "drop", pct = "row")
```

[TABLE]

Quand vous passez plusieurs **variables en ligne** *sans* `tab_vars`,
[`tab()`](https://bricenocenti.github.io/tabxplor/reference/tab.md)
fusionne par défaut les tableaux jumeaux en un seul tableau.
`output_list = TRUE` renvoie plutôt une **liste avec un tableau par
variable en ligne** (avec `tab_vars`, le résultat est toujours une
liste) :

``` r
tab(gss_simple, c(married, income25k), race, pct = "row", output_list = TRUE)
```

[TABLE]

  

[TABLE]

## Couleurs : des repères de lecture

L’un des principaux objectifs de `tabxplor` est de fournir une palette
complète de repères colorés pour l’exploration des données.
`color = "diff"` colore chaque case selon **son écart à sa référence** —
par défaut le Total de sa ligne ou de sa colonne. Les cases nettement
au-dessus de la moyenne deviennent **bleues**, celles nettement en
dessous **rouge/orange** — plus une case s’éloigne de sa référence, plus
la nuance est forte — et une légende de couleur est imprimée en dessous.

``` r
tab(gss_simple, race, party3, pct = "row", color = "diff")
```

[TABLE]

`color = TRUE` choisit automatiquement un schéma sensé pour chaque type
de colonne (différences et rapport pour les pourcentages, seulement les
rapports pour les moyennes, …) ; vérifiez lequel dans la légende :

``` r
tab(gss_simple, rincome, c(party3, marital), pct = "row", color = TRUE)
```

[TABLE]

Les colonnes numériques sont colorées de la même façon, sur leurs
**moyennes** (ici, les heures de télévision par jour selon le revenu) :

``` r
tab(gss_simple, rincome, tvhours, color = "diff")
```

[TABLE]

**Quelle case sert de référence pour la comparaison ?** Par défaut,
chaque case est comparée au Total pertinent (ligne Total pour les
pourcentages en ligne, colonne Total pour les pourcentages en colonne),
afin de mettre en évidence sur-représentations et sous-représentations.
Deux variantes utiles :

- `ref = 1` compare chaque ligne à la **première ligne** — parfait pour
  lire une évolution dans le temps ou un facteur ordinal.
- avec des sous-tableaux, `comp = "all"` compare au Total **d’ensemble**
  plutôt qu’au Total propre à chaque sous-tableau.

``` r
tab(gss_simple, year, marital, pct = "row", color = "diff", ref = 1)
```

[TABLE]

``` r
tab(gss_simple, rincome, party3, race, na = "drop", pct = "row", color = TRUE, comp="all")
```

[TABLE]

**Une référence différente pour chaque variable.** `ref` est
réinterprété par `pct`. Sous des pourcentages en **ligne** (ou des
moyennes), il choisit une **ligne** de référence : un vecteur *nommé*
donne alors à chaque variable en ligne la sienne — ici `race` est lu par
rapport à sa première ligne, `relig` par rapport à son Total :

``` r
tab(gss_simple, c(race, relig), party3, pct = "row", color = "diff",
    ref = c(race = "first", relig = "tot"), na = "drop")
```

[TABLE]

Sous des pourcentages en **colonne**, `ref` choisit plutôt une
**colonne** de référence, vectorisé sur les variables en colonne — nommé
(`ref = c(party3 = "first", marital = "tot")`) ou positionnel, une
valeur par variable en colonne :

``` r
tab(gss_simple, race, c(party3, marital), pct = "col", color = "diff",
    ref = c("first", "tot"), na = "drop")
```

[TABLE]

Les seuils de couleur et la palette se personnalisent : réglez-les **une
fois pour toute la session** avec
[`set_color_breaks()`](https://bricenocenti.github.io/tabxplor/reference/tab_many.md)
et
[`set_color_palette()`](https://bricenocenti.github.io/tabxplor/reference/tab_many.md).

## Des couleurs qui respectent la significativité

Les couleurs ci-dessus montrent la *taille* d’un écart, mais pas s’il
est **statistiquement fiable**. Sur de petits échantillons, une
différence d’apparence marquée peut n’être que du bruit. L’argument
`color_signif` fait entrer la significativité dans la coloration :

- `"ignore"` (par défaut) : colore chaque écart selon sa taille
  observée. Grise les petites différences sous un certain seuil.
- `"grey_non_signif"` : colore selon la taille de l’effet, grise les
  petits effets sous un certain seuil, mais grise **aussi les cases dont
  l’effet est important mais non significatif**. Chaque case colorée est
  alors garantie significativement différente de sa référence, sans être
  encombrée par de très petites différences significatives.
- `"guaranteed_effect"` : ne colore que la part de l’effet dont on peut
  être sûr (sa borne de confiance), avec des couleurs plus ternes et
  prudentes. À utiliser sur de **petits échantillons** pour **mettre en
  évidence toutes les différences que l’on a le droit d’interpréter**.
  Tout ce qui est coloré est significatif ; rien de ce qui est gris ne
  l’est.

``` r
tab(gss_simple, race, party3, pct = "row", color = "diff", color_signif = "grey_non_signif")
```

[TABLE]

``` r
gss_simple |>
  dplyr::filter(year == "2012") |> # n=1 974
  tab(race, party3, pct = "row", color = "diff", color_signif = "guaranteed_effect")
```

[TABLE]

Sur de **petits échantillons**, un pourcentage d’apparence marquée peut
reposer sur une poignée de répondants. `n_min =` est un filtre purement
visuel, appliqué en dernier : il masque les cases dont la base (non
pondérée) est sous le seuil, et retire entièrement une ligne quand sa
plus grande base est trop faible. Ici, les religions les plus rares
disparaissent :

``` r
tab(gss_simple, relig, race, pct = "row", n_min = 400)
```

[TABLE]

Une alternative est de garder les lignes et colonnes rares, mais de les
regrouper toutes dans une modalité « Autres » :

``` r
tab(gss_simple, relig, race, pct = "row",  other_if_less_than = 400)
```

[TABLE]

## Intervalles de confiance, tests et contributions

Affichez les intervalles de confiance du pourcentage ou de la moyenne de
chaque case avec `ci = "cell"` :

``` r
tab(gss_simple, race, party3, pct = "row", ci = "cell") # par défaut, conf_level = 0.95
```

[TABLE]

Affichez les intervalles de confiance de la **différence** avec une
référence, utilisés pour calculer la significativité (si 0 appartient à
l’intervalle de confiance, la case n’est pas significativement
différente de la référence, ici la ligne Total) :

``` r
gss_simple |>
  dplyr::filter(year == "2012") |> # n=1 974
  tab(race, party3, pct = "row", color = TRUE, color_signif = "guaranteed_effect",
      display = "num_ci" # "{pct} {ci}"
  )
```

[TABLE]

`display = "num_ci"` en est un raccourci adaptatif au type : il affiche
chaque valeur avec l’intervalle de confiance que le tableau calcule —
`{pct} {ci}` sur les colonnes de pourcentages et `{mean} {ci}` sur les
colonnes numériques, choisi colonne par colonne — donc il fonctionne
pour un mélange de facteurs et de nombres en un seul appel :

Ajoutez des étoiles de significativité avec `stars = TRUE`. Elles
racontent la même histoire que les intervalles de confiance de la
différence avec la référence, mais pour différents seuils de confiance
(99 %, 95 %, 90 %) :

``` r
gss_simple |>
  dplyr::filter(year == "2012") |> # n=1 974
  tab(rincome, c(party3, tvhours), pct = "row", display = "num_ci", stars = TRUE)
```

[TABLE]

**Afficher deux nombres dans une même case.** `display` ne sert pas
qu’aux intervalles de confiance : il accepte un **gabarit
[`{}`](https://rdrr.io/r/base/Paren.html)** qui combine n’importe quels
champs de la case. Par exemple, `display = "{pct} ({diff})"` imprime
chaque pourcentage suivi de sa différence à la référence, et
`"{pct} (n={n})"` le fait suivre de l’effectif :

``` r
tab(gss_simple, race, party3, pct = "row", color = "diff", display = "{pct} ({diff})")
```

[TABLE]

Le premier champ du gabarit est le champ *primaire* — la valeur que
garde Excel et celle que lisent les couleurs.
[`vignette("tabxplor-programming")`](https://bricenocenti.github.io/tabxplor/articles/tabxplor-programming.md)
détaille toute la grammaire et liste tous les champs combinables.

`test = TRUE` ajoute un test statistique d’indépendance par
(sous-)tableau — **Chi2** pour les colonnes facteurs, **F d’ANOVA de
Welch** pour les variables numériques
(`options(tabxplor.anova = "classic")` bascule vers le F à variance
commune) :

``` r
tab(gss_simple, race, c(party3, tvhours), pct = "row", test = TRUE)
```

[TABLE]

`color = "contrib"` colore les cases selon leur **contribution au Chi2**
— les cases qui ressortiraient dans une analyse des correspondances :

``` r
tab(gss_simple, race, party3, color = "contrib")
```

[TABLE]

``` r
# tab(gss_simple, race, party3, pct="row", color = "contrib") # fonctionne avec pct, mais indépendant des lignes/colonnes
```

Voir plus bas le détail de la composition des intervalles de confiance
et des couleurs.

## Infobulles au survol (tableaux html)

Chaque tableau html porte des **infobulles** au survol des cases, avec
les nombres derrière la case : l’effectif non pondéré, l’écart à la
référence, le ratio, l’intervalle de confiance… Elles sont actives par
défaut dans le Viewer et dans les rapports — cette page les a seulement
coupées pour tout le document avec
`options(tabxplor.tab_kable_tooltips = FALSE)`, pour rester légère.
Survolez les cases du tableau ci-dessous, où elles sont réactivées avec
`tooltips = TRUE` :

``` r
tab(gss_simple, race, party3, pct = "row", color = "diff") |>
  tab_html(tooltips = TRUE)
```

[TABLE]

**Une note sur la pondération.** Avec un poids (`wt =`), chaque
proportion ou moyenne est pondérée, mais par défaut la taille
d’échantillon derrière les intervalles de confiance et les tests reste
le nombre réel, **non pondéré**, d’observations. Sous des poids inégaux,
elle ne porte aucun effet de plan, donc elle est un peu trop étroite :
activez la taille d’échantillon efficace de Kish avec
`options(tabxplor.kish_neff = TRUE)` (voir [Pondération](#ponderation))
pour élargir honnêtement chaque intervalle et faire passer les tests du
Chi2 d’ensemble à une correction de Rao–Scott.

## Exporter

Un tableau terminé s’exporte avec ses couleurs vers Excel, HTML ou
Markdown :

``` r
tabs <- tab(gss_simple, race, party3, pct = "row", color = "diff")
tab_export(tabs) # par défaut : tableau html (Viewer de RStudio, .Rmd/.qmd, etc.)
tab_export(tabs, format = "xl", path = "table") # export Excel
tab_export(tabs, format = "md", path = "table") # fichier markdown plat (tableaux à barres verticales)
```

Deux options valent la peine d’être connues :

- `theme = "auto"` fait **suivre le mode clair/sombre du lecteur** à un
  export HTML ou Markdown (il bascule en direct). Pour la console,
  `set_color_palette(theme = "auto")` détecte l’éditeur (RStudio,
  Positron, etc.) et choisit la palette correspondante — c’est appliqué
  automatiquement au chargement du paquet.

``` r
tab_export(tabs, theme = "auto") # HTML qui suit le mode clair/sombre du lecteur
```

- Comme les variables numériques ne peuvent être passées qu’en colonnes,
  certaines mises en page complexes avec des variables numériques en
  lignes nécessitent de transposer le tableau à l’export avec
  `transpose = TRUE` :

``` r
tab(gss_simple, party3, c(race, tvhours), pct = "row") |>
  tab_html(transpose = TRUE)
```

[TABLE]

- **Une seule feuille de style pour tout un document.** Dans un rapport
  `.Rmd`/`.qmd`,
  [`tab_css()`](https://bricenocenti.github.io/tabxplor/reference/tab_css.md)
  écrit le CSS des couleurs une seule fois, et chaque tableau suivant
  n’émet que des classes : un unique `theme` — y compris `"auto"`, qui
  suit le mode clair/sombre du lecteur — met en forme tous les tableaux
  d’un coup. Cette page fait exactement cela (avec `theme = "light"`) :

``` r
options(tabxplor.tab_kable_css = FALSE)
tab_css(theme = "auto")   # à émettre une fois, en haut du document
```

Rien n’est écrit en ligne sur une case, donc tout rendu reste surchargé
par du CSS ordinaire ensuite (largeurs de colonnes, polices…) ; voir
[`?tab_css`](https://bricenocenti.github.io/tabxplor/reference/tab_css.md)
pour les classes de rôle (`.tx-rv`, `.tx-tot`, `.tx-num`).

## Travailler avec le résultat

[`tab()`](https://bricenocenti.github.io/tabxplor/reference/tab.md)
renvoie un `tibble` (de classe `tabxplor_tab`), donc les verbes `dplyr`
fonctionnent tels quels. Utilisez le prédicat
[`is_totrow()`](https://bricenocenti.github.io/tabxplor/reference/fmt.md)
pour garder la ligne Total en place quand vous réordonnez (il repère les
lignes de total, donc trier dessus d’abord les envoie tout en bas) :

``` r
library(dplyr)
tab(gss_simple, race, marital, pct = "row") |>
  arrange(desc(Married))
```

[TABLE]

**Titrer et annoter.** `subtext =` imprime une ou plusieurs lignes de
légende sous un tableau (une source de données, une note).
[`set_caption()`](https://bricenocenti.github.io/tabxplor/reference/set_caption.md)
donne à un tableau un **titre qui survit à un pipeline dplyr**, et
chaque exportateur l’utilise comme titre du tableau :

``` r
tab(gss_simple, race, marital, pct = "row", subtext = "Source : GSS, 2000-2014") |>
  set_caption("Titre personnalisé")
```

Titre personnalisé

[TABLE]

## Composition des intervalles de confiance et des couleurs : type de variable × `color` × `color_signif`

Cette section est la référence derrière les deux sections sur les
couleurs ci-dessus. Elle montre comment les paramètres s’articulent — le
**type de variable**, la **mesure** que l’on colore (`color`) et la
**politique de significativité** (`color_signif`).

Le **type** est fixé par la variable en colonne — un **facteur** (des
pourcentages sont calculés, `pct`) ou une **numérique** (des moyennes
sont calculées, `mean`). La variable en ligne est toujours transformée
en facteur.

Tout tableau coloré répond à trois questions :

- **Comment mesurer l’écart ?** `color =` choisit ce qu’une couleur
  *signifie* : `"diff"` (distance à la référence), `"ratio"` (risque
  relatif pour les pourcentages, rapport de moyennes pour les moyennes),
  `"contrib"` (poids dans le Chi2), `"OR"` (rapport de cotes).
  `color = TRUE` en choisit un sensé par type de colonne.
- **Quelle confiance a-t-on dans cette mesure ?** chaque couleur lit un
  **intervalle de confiance** au seuil `conf_level` (95 % par défaut).
  Une case est *significative* quand cet intervalle exclut sa valeur
  neutre — **0** pour une différence, **1** pour un rapport ou un
  rapport de cotes. Le crochet imprimé, les étoiles de significativité
  et le grisage lisent tous ce même intervalle, donc ils ne peuvent
  jamais se contredire.
- **Comment montrer la significativité ?** `color_signif` — `"ignore"`,
  `"grey_non_signif"` ou `"guaranteed_effect"` ; `stars = TRUE` pour
  utiliser des étoiles de significativité au lieu des couleurs, ou
  empilées avec elles.

**L’intervalle de confiance utilisé pour les couleurs et les étoiles**
compare chaque case à sa case de référence (par défaut, la case
correspondante dans la ligne ou la colonne Total) :

| type | color | ce que la couleur mesure | intervalle de confiance (défaut) |
|----|----|----|----|
| pct | `diff` | % case − % référence (points de %) | score hybride de Newcombe |
| pct | `ratio` | % case / % référence (risque relatif) | log-risque relatif de Katz |
| pct | `OR` | rapport de cotes empirique | log-OR de Woolf |
| pct | `contrib` | contribution χ² signée (sans référence) | — (résidu standardisé) |
| mean | `diff` | moyenne case − moyenne référence (en écart-type) | *t* de Welch |
| mean | `ratio` | moyenne case / moyenne référence | rapport de moyennes robuste |

Méthodes d’intervalle alternatives (voir
[`?tab_ci`](https://bricenocenti.github.io/tabxplor/reference/tab_ci.md))
:

- `method_diff = "ac"` (Agresti-Caffo) ou `"wald"` pour une différence
  de pourcentages ;
- `method_mean_diff = "student"` (à variance commune, l’intervalle à
  deux groupes des MCO) pour une différence de moyennes ;
- `method_mean_ratio = "quasipoisson"` ou `"poisson"` pour un rapport de
  moyennes.
- Les intervalles du risque relatif (`method_ratio = "katz"`) et du
  rapport de cotes (Woolf) n’ont pas d’alternative.
- Une **différence** de moyennes est colorée **standardisée** — le Δ de
  Glass, la différence divisée par l’écart-type de la référence — donc
  les seuils de couleur `mean_diff` se lisent en unités d’écart-type,
  sauf si l’utilisateur fournit une échelle de seuils personnalisée.
- Pour un facteur à **3 modalités ou plus**, le rapport de cotes (et son
  intervalle) compare chaque modalité à la modalité de base `ref2` :
  c’est un rapport de risques relatifs (RRR) (la même quantité observée
  que celle modélisée par une régression logistique multinomiale).
  L’intervalle de l’OR n’est calculé que lorsque `color_signif` ou
  `stars` en a besoin.

**Les intervalles de confiance simples, case par case** (`ci = "cell"`)
comparent chaque case à 0 % (ou à une moyenne de 0), *et non* à une
référence :

| type | color | intervalle de confiance (défaut) | autre méthode |
|----|----|----|----|
| pct | `cell` | intervalle score de Wilson | `method_cell = "wald"` (normal) |
| mean | `cell` | *t* de Student à un échantillon (n-1) | — |

Parce qu’ils comparent à 0 et non à une référence, les intervalles de
case sont purement descriptifs : ils ne portent **aucune significativité
ni étoile**. `method_cell` choisit `"wilson"` (défaut) ou `"wald"` pour
les pourcentages ; un intervalle de case pour une moyenne est toujours
le *t* de Student à un échantillon.

**`color_signif` transforme cet intervalle en politique de coloration.**

- `"ignore"` colore **chaque** case selon la taille de son **effet
  observé**, par exemple la différence observée avec la ligne Total. Les
  cases grises ont un effet observé sous le seuil (par exemple, des
  différences de moins de ±5 points de pourcentage).
- `"grey_non_signif"` et `"guaranteed_effect"` colorent tous deux
  **seulement les cases significatives**, mais diffèrent par la *base
  d’intensité* :
  - `grey_non_signif` colore selon l’effet **observé**, comme « ignore
    », en grisant les petits écarts, mais il **grise aussi tout grand
    écart qui se révèle non significatif**. Idéal pour les grands
    échantillons.
  - `guaranteed_effect` colore selon l’effet **garanti** — la borne de
    confiance (le plancher de l’IC), le **plus petit écart assuré à un
    seuil de confiance donné** (95 % par défaut) — donc ses couleurs
    sont plus ternes et prudentes, mais **toutes les différences
    significatives sont colorées**, ce qui est idéal pour les petits
    échantillons.

| type | color | `="ignore"` | `="grey_non_signif"` | `="guaranteed_effect"` |
|----|----|----|----|----|
| pct | `diff` | diff observée | gris si l’IC de diff contient 0 | plancher IC de diff |
| pct | `ratio` | rapport observé | gris si l’IC de ratio contient 1 | plancher IC de ratio |
| pct | `OR` | OR observé | gris si l’IC de l’OR contient 1 | plancher IC de l’OR |
| pct | `contrib` | contribution χ² | gris si résidu \< 1,96 (conf. 95 %) | résidu \>= 1,96 (conf. 95 %) |
| mean | `diff` | diff observée | gris si l’IC de diff contient 0 | plancher IC de diff |
| mean | `ratio` | rapport observé | gris si l’IC de ratio contient 1 | plancher IC de ratio |

Exemples :

``` r
# --- facteurs : pourcentages ----------------------------------------------
tab(gss_simple, race, party3, pct = "row", color = "diff",  color_signif = "grey_non_signif")
tab(gss_simple, race, party3, pct = "row", color = "ratio", color_signif = "guaranteed_effect")
tab(gss_simple, rincome, married, pct = "row", color = "OR", OR = TRUE, ref2 = 1)
tab(gss_simple, rincome, party3, color = "contrib")   # fonctionne aussi avec pct = "row"/"col"

# --- numériques : moyennes ------------------------------------------------
tab(gss_simple, rincome, tvhours, color = "diff",  color_signif = "guaranteed_effect")
tab(gss_simple, rincome, tvhours, color = "ratio", color_signif = "grey_non_signif")

#    une échelle personnalisée pour les différences de moyennes, et une référence "première ligne"
tab(gss_simple, rincome, tvhours, color = "diff", color_signif = "grey_non_signif",
    color_breaks = list(mean_diff = c(0.4, 0.8, 1.6)), ref = 1)
```

## Options de session

Une poignée d’[`options()`](https://rdrr.io/r/base/options.html) fixent
vos préférences par défaut, une fois pour toute la session — placez-les
en haut d’un script, ou dans votre `.Rprofile`. Chacune a aussi un
argument par appel ; l’option ne fait que changer la valeur par défaut.
Les plus courantes :

- `options(tabxplor.print = "html")` — afficher les tableaux non pas
  dans la console, mais en html dans le panneau Viewer de RStudio ou
  Positron par défaut (recommandé)
- `options(tabxplor.cleannames = TRUE)` — retirer partout les préfixes
  de type `"1-"` des noms de modalités.
- `options(tabxplor.parallel = 8)` — paralléliser par défaut les
  tableaux à plusieurs variables sur plusieurs cœurs de processeur
  (nécessite `mirai`)
- `options(tabxplor.var_labels = TRUE)` — dans les exports, afficher
  l’étiquette d’une variable (données `haven`/`labelled`) au lieu de son
  nom brut.
- `options(tabxplor.theme = "auto")` — le thème d’export
  (`"light"`/`"dark"`/`"auto"`) ; `set_color_palette(theme = "auto")`
  fait de même pour la console.
- `options(tabxplor.stars = TRUE)` — afficher les étoiles de
  significativité dans chaque tableau (comme `stars = TRUE`).
- `options(tabxplor.conf_level = 0.9)` — le seuil de confiance des
  intervalles et des tests (défaut `0.95`).
- `options(tabxplor.ci_print = "moe")` — imprimer un intervalle de
  confiance sous forme de `pct ± marge d'erreur` plutôt qu’en crochet
  `[bas ; haut]`.
- `options(tabxplor.lang = "fr")` — la langue des légendes de couleur et
  des notes de bas de tableau (`"auto"`/`"en"`/`"fr"`).

Les seuils de couleur et les palettes ont leurs propres fonctions,
[`set_color_breaks()`](https://bricenocenti.github.io/tabxplor/reference/tab_many.md)
et
[`set_color_palette()`](https://bricenocenti.github.io/tabxplor/reference/tab_many.md).
`?tabxplor-options` documente chaque option, et
[`vignette("tabxplor-programming")`](https://bricenocenti.github.io/tabxplor/articles/tabxplor-programming.md)
couvre les plus avancées (polices d’export, calcul parallèle…).

## Une interface graphique (jamovi)

Tout ce qui précède est aussi disponible **sans écrire de code R**, via
un module [jamovi](https://www.jamovi.org/download). jamovi est un
logiciel statistique libre et gratuit : installez-le, ouvrez le menu des
modules (le **`+`** en haut à droite), choisissez **jamovi library**, et
installez *tabxplor*. Il ajoute une analyse **Crosstables** — et une
analyse **Regressions** propulsée par
[`tab_reg()`](https://bricenocenti.github.io/tabxplor/reference/tab_reg.md)
— avec les mêmes tableaux colorés et exportables, pilotés entièrement
par des menus. Pratique pour l’enseignement, ou pour des collègues qui
n’utilisent pas R.

## Pour aller plus loin

- [`vignette("tabxplor-reg")`](https://bricenocenti.github.io/tabxplor/articles/tabxplor-reg.md)
  — les tableaux de régression avec
  [`tab_reg()`](https://bricenocenti.github.io/tabxplor/reference/tab_reg.md),
  et la comparaison entre effets modélisés et observés.
- [`vignette("tabxplor-programming")`](https://bricenocenti.github.io/tabxplor/articles/tabxplor-programming.md)
  — le type de cellule `tabxplor_fmt` et comment programmer avec ses
  champs.
- [`?tab`](https://bricenocenti.github.io/tabxplor/reference/tab.md)
  pour chaque argument (groupés par usage),
  [`?tab_ci`](https://bricenocenti.github.io/tabxplor/reference/tab_ci.md)
  pour les méthodes d’intervalle de confiance, et `?tabxplor-options`
  pour les réglages par défaut du paquet.
