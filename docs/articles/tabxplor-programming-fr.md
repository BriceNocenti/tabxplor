# Programmer avec tabxplor

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

# options(tabxplor.lang = "fr") affiche les legendes et les notes de bas de tableau en francais.
options(cli.num_colors = 256)
options(tabxplor.lang = "fr")
set_color_palette(theme = "light")
```

Cette vignette s’adresse aux utilisateurs qui veulent **programmer
avec** `tabxplor` — écrire leurs propres fonctions auxiliaires, extraire
les nombres sous-jacents, ou remodeler un tableau case par case. Si vous
souhaitez seulement construire et lire des tableaux,
[`vignette("tabxplor")`](https://bricenocenti.github.io/tabxplor/articles/tabxplor.md)
est le point de départ.

Chaque colonne numérique d’un tableau `tabxplor` est un unique vecteur
de classe **`tabxplor_fmt`** (« nombre formaté »). C’est un
[enregistrement
`vctrs`](https://vctrs.r-lib.org/reference/new_rcrd.html) : derrière
l’unique valeur affichée, chaque case stocke **toutes les données
nécessaires pour calculer le nombre affiché, sa mise en forme et sa
couleur** — effectifs, pourcentages, différences, risques relatifs,
bornes d’intervalle de confiance, rapports de cotes, etc. Comme c’est un
véritable vecteur, il survit à tous les verbes `dplyr`, et vous pouvez
lire ou réécrire n’importe lequel de ses champs.

``` r
gss_simple <- gss_cat_data_formatting()
tabs <- tab(gss_simple, race, marital, pct = "row", color = "diff")
```

## Récupérer les nombres bruts

Le moyen le plus rapide de récupérer les nombres sous-jacents sous forme
de vecteurs numériques ordinaires est
[`get_num()`](https://bricenocenti.github.io/tabxplor/reference/fmt.md),
qui extrait le champ actuellement affiché :

``` r
tabs |> mutate(across(where(is_fmt), get_num))
```

``` r-output
#> # A tabxplor tab: 4 × 8
#>   race  Married Separated Divorced Widowed `Never married`     `NA` Total
#>   <fct>   <dbl>     <dbl>    <dbl>   <dbl>           <dbl>    <dbl> <dbl>
#> 1 White   0.507    0.0267    0.163  0.0900           0.212 0.000793     1
#> 2 Black   0.278    0.0626    0.158  0.0837           0.417 0.000639     1
#> 3 Other   0.476    0.0562    0.108  0.0357           0.323 0.00102      1
#> 4 Total   0.471    0.0346    0.157  0.0841           0.252 0.000791     1
```

Pour obtenir plutôt les chaînes de caractères (formatées, mais sans les
couleurs), utilisez [`format()`](https://rdrr.io/r/base/format.html) :

``` r
tabs |> mutate(across(where(is_fmt), format))
```

Un champ individuel se lit le plus simplement avec `$` sur la colonne
fmt (voir « Lire et modifier les champs » ci-dessous) :

``` r
tabs$Married$pct
```

``` r-output
#> [1] 0.5072278 0.2777245 0.4757529 0.4709305
```

## Les champs d’une case

Une case `tabxplor_fmt` porte **19 champs** (beaucoup valent `NA`
lorsque la quantité correspondante n’a pas été demandée). Les champs
destinés à l’utilisateur sont :

| Champ | Signification |
|:---|:---|
| `n` | effectif non pondéré (entier) |
| `wn` | effectif pondéré |
| `pct` | pourcentage |
| `mean` | moyenne (variables en colonne numériques) |
| `diff` | différence avec la case Total / de référence |
| `ratio` | rapport à la référence (la « règle ×2 » ; risque relatif) |
| `ci_inf`, `ci_sup` | bornes de l’intervalle de confiance |
| `pvalue` | p-valeur de significativité par case (alimente les étoiles) |
| `or` | rapport de cotes / rapport de risques relatifs |
| `ctr` | contribution au Chi2 (`color = "contrib"`) |
| `var` | variance (colonnes numériques ; variance du Chi2 avec `pct`) |
| `tot_n` | la base propre à la case — l’effectif sur lequel son pourcentage est calculé |
| `n_eff` | taille d’échantillon efficace utilisée pour l’intervalle de confiance, avec `options(tabxplor.kish_neff = TRUE)` |
| `digits` | nombre de décimales à afficher (par case) |
| `display` | quel champ est affiché (par case) |
| `in_totrow`, `in_tottab`, `in_refrow` | la case est-elle dans une ligne de total / un tableau de total / une ligne de référence (logique) |

Voyez l’ensemble du data frame sous-jacent d’une colonne avec
[`vctrs::vec_data()`](https://vctrs.r-lib.org/reference/vec_data.html) :

``` r
vctrs::vec_data(tabs$Married)
```

``` r-output
#>       n display digits wn       pct mean         diff     ratio ctr var ci_inf
#> 1  8316     pct      0 NA 0.5072278   NA  0.036297310 1.0770757  NA  NA     NA
#> 2   869     pct      0 NA 0.2777245   NA -0.193205991 0.5897357  NA  NA     NA
#> 3   932     pct      0 NA 0.4757529   NA  0.004822432 1.0102402  NA  NA     NA
#> 4 10117     pct      0 NA 0.4709305   NA  0.000000000 1.0000000  NA  NA     NA
#>   ci_sup pvalue or tot_n n_eff in_totrow in_tottab in_refrow
#> 1     NA     NA NA 16395    NA     FALSE     FALSE     FALSE
#> 2     NA     NA NA  3129    NA     FALSE     FALSE     FALSE
#> 3     NA     NA NA  1959    NA     FALSE     FALSE     FALSE
#> 4     NA     NA NA 21483    NA      TRUE     FALSE     FALSE
```

## Lire et modifier les champs

Lisez un champ avec `$` (le plus commode), ou avec
[`vctrs::field()`](https://vctrs.r-lib.org/reference/fields.html) :

``` r
tabs$Married$pct
tabs |> mutate(across(where(is_fmt), ~ .$pct))
tabs |> mutate(across(where(is_fmt), ~ vctrs::field(., "pct")))
```

Les intervalles de confiance sont stockés sous forme de leurs deux
bornes, les champs `ci_inf` et `ci_sup` :

``` r
ci_tab <- tab(gss_simple, race, marital, pct = "row", ci = "cell")
ci_tab$Married$ci_inf
```

``` r-output
#> [1] 0.4995743 0.2623114 0.4537069        NA
```

``` r
ci_tab$Married$ci_sup
```

``` r-output
#> [1] 0.5148780 0.2936827 0.4978939        NA
```

Changez le champ affiché avec
[`set_display()`](https://bricenocenti.github.io/tabxplor/reference/fmt.md)
:

``` r
tabs |> set_display("diff")
tabs |> mutate(across(where(is_fmt), ~ set_display(., "diff")))
```

Pour modifier un champ, la voie la plus simple est
[`dplyr::mutate()`](https://dplyr.tidyverse.org/reference/mutate.html)
**sur le vecteur fmt lui-même** — un `tabxplor_fmt` se comporte comme un
petit data frame de ses champs. Par exemple, arrondir chaque case à deux
décimales :

``` r
tabs |> mutate(across(where(is_fmt), ~ mutate(., digits = 2L)))
```

Un exemple plus complet : transformer la variance d’un tableau de
moyennes en écart-type et l’ajouter comme une nouvelle colonne, non
colorée, affichée à une décimale :

``` r
tab_num(gss_simple, race, c(age, tvhours), digits = 1L) |>
  mutate(across(
    c(age, tvhours),
    ~ mutate(., var = sqrt(var), display = "var", digits = 1L) |> set_color("no"),
    .names = "{.col}_sd"
  ))
```

## Affichage composite : combiner des champs

[`set_display()`](https://bricenocenti.github.io/tabxplor/reference/fmt.md)
choisit *un* champ à afficher. Pour montrer **plusieurs champs dans une
même case**, donnez-lui un **gabarit**
[`{}`](https://rdrr.io/r/base/Paren.html) plutôt qu’un simple nom de
champ — le même gabarit que vous pouvez passer à `tab(display =)` lors
de la construction du tableau. Le gabarit est du texte ordinaire avec
des jetons `{champ}` ; chaque jeton est remplacé par ce champ, formaté
comme d’habitude :

``` r
tabs |> set_display("{pct} ({diff})")
```

``` r-output
#> # A tabxplor tab: 4 × 8
#>   race      Married Separated  Divorced  Widowed `Never married`     `NA`
#>   <fct>      <row%>    <row%>    <row%>   <row%>          <row%>   <row%>
#> 1 White  51% ( +4%)  3% (-1%) 16% (+1%) 9% (+1%)      21% ( -4%) 0% (+0%)
#> 2 Black  28% (-19%)  6% (+3%) 16% (+0%) 8% (-0%)      42% (+16%) 0% (-0%)
#> 3 Other  48% ( +0%)  6% (+2%) 11% (-5%) 4% (-5%)      32% ( +7%) 0% (+0%)
#> 4 Total  47% ( +0%)  3% (+0%) 16% (+0%) 8% (+0%)      25% ( +0%) 0% (+0%)
#> # ℹ 1 more variable: Total <row%>
#> # différence (Total) : -30 -20 -10 -5 +5 +10 +20 +30
```

Les règles :

- Les **champs valides** sont `pct`, `n`, `wn`, `mean`, `diff`, `ratio`,
  `ci`, `or`, `ctr`, `var` — le même ensemble que pour les affichages à
  champ unique.
- Le **premier** jeton est le champ *primaire* : c’est ce que renvoie
  [`get_num()`](https://bricenocenti.github.io/tabxplor/reference/fmt.md),
  la valeur que garde Excel (Excel n’affiche que le primaire), et le
  champ que lisent les couleurs.
- Un **nom de champ nu** est un raccourci pour son propre gabarit, donc
  `set_display("ci")` est exactement `set_display("{ci}")`.
- C’est une **surcouche d’affichage** pour les sorties texte (la
  console,
  [`tab_kable()`](https://bricenocenti.github.io/tabxplor/reference/tab_html.md),
  [`tab_md()`](https://bricenocenti.github.io/tabxplor/reference/tab_md.md))
  : les champs stockés restent intacts, vous pouvez donc revenir en
  arrière à tout moment.
- Le champ [ci](https://github.com/GegznaV/ci) imprime déjà ses propres
  crochets `[…;…]`, écrivez donc `"{pct} {ci}"` — et **non**
  `"{pct} [{ci}]"`, qui les doublerait.

## Créer une colonne à partir d’un autre champ

Comme changer l’affichage ne recalcule rien — chaque champ est déjà
stocké dans la case — vous pouvez dériver une **nouvelle colonne qui
affiche un champ différent**. Un tableau de pourcentages a une référence
par défaut (la ligne Total), donc son champ `diff` est déjà rempli ; un
jumeau en différence de chaque colonne de pourcentages tient alors en un
seul [`mutate()`](https://dplyr.tidyverse.org/reference/mutate.html) :

``` r
tab(gss_simple, race, marital, pct = "row") |>
  mutate(across(where(is_fmt), ~ set_display(., "diff"), .names = "{.col}_diff"))
```

``` r-output
#> # A tabxplor tab: 4 × 15
#>   race   Married Separated Divorced Widowed `Never married`   `NA`  Total
#>   <fct>   <row%>    <row%>   <row%>  <row%>          <row%> <row%> <row%>
#> 1 White      51%        3%      16%      9%             21%     0%   100%
#> 2 Black      28%        6%      16%      8%             42%     0%   100%
#> 3 Other      48%        6%      11%      4%             32%     0%   100%
#> 4 Total      47%        3%      16%      8%             25%     0%   100%
#> # ℹ 7 more variables: Married_diff <row%-diff>, Separated_diff <row%-diff>,
#> #   Divorced_diff <row%-diff>, Widowed_diff <row%-diff>,
#> #   `Never married_diff` <row%-diff>, NA_diff <row%-diff>, Total_diff <row%>
```

`.names = "{.col}_diff"` conserve les colonnes de pourcentages
originales et ajoute à côté de chacune un jumeau `<nom>_diff`. (Si une
colonne `diff` ressort vide, c’est que le tableau source n’avait pas de
case de référence — construisez-la avec `ref =` / `comp =`, ou avec
`color = "diff"`.)

## Lignes de total, lignes et colonnes de référence

Des prédicats auxiliaires vous permettent d’agir sur les parties
structurelles d’un tableau :
[`is_totrow()`](https://bricenocenti.github.io/tabxplor/reference/fmt.md)
/
[`is_tottab()`](https://bricenocenti.github.io/tabxplor/reference/fmt.md)
/
[`is_refrow()`](https://bricenocenti.github.io/tabxplor/reference/fmt.md)
(au niveau de la case, vecteurs logiques), et
[`is_totcol()`](https://bricenocenti.github.io/tabxplor/reference/fmt.md)
/
[`is_refcol()`](https://bricenocenti.github.io/tabxplor/reference/fmt.md)
(au niveau de la colonne). Ce sont eux qui maintiennent les totaux en
place quand vous réordonnez, ou quand vous écrivez une mise en forme
conditionnelle :

``` r
# plus de décimales sur la ligne de total que sur le corps du tableau :
tab(gss_simple, race, marital, race, pct = "row") |>
  mutate(across(
    where(is_fmt),
    ~ if_else(is_totrow(.), mutate(., digits = 1L), mutate(., digits = 2L))
  ))
```

## Attributs de colonne

Outre ses champs par case, chaque colonne `fmt` porte quelques
**attributs de niveau colonne**, lus et fixés avec `get_*` / `set_*` (ou
`is_*` / `as_*` pour les attributs logiques) :

- `type` —
  [`get_type()`](https://bricenocenti.github.io/tabxplor/reference/fmt.md)
  /
  [`set_type()`](https://bricenocenti.github.io/tabxplor/reference/fmt.md)
  : `"n"`, `"mean"`, `"row"`, `"col"`, `"all"`, `"all_tabs"` (ou
  `"coef"` pour les bêtas de régression). Il détermine quels calculs
  effectuent les fonctions `tab_*`.
- `color` —
  [`get_color()`](https://bricenocenti.github.io/tabxplor/reference/fmt.md)
  /
  [`set_color()`](https://bricenocenti.github.io/tabxplor/reference/fmt.md)
  : la mesure de couleur de la colonne (`""`, `"no"`, `"diff"`,
  `"ratio"`, `"contrib"`, `"OR"`).
- `col_var` —
  [`get_col_var()`](https://bricenocenti.github.io/tabxplor/reference/fmt.md)
  : le nom de la variable en colonne (un tableau peut en contenir
  plusieurs).
- `comp_all` —
  [`get_comp_all()`](https://bricenocenti.github.io/tabxplor/reference/fmt.md)
  : avec des `tab_vars`, la référence de comparaison est-elle le
  sous-tableau (`FALSE`) ou le tableau entier (`TRUE`) ?
- `totcol` / `refcol` —
  [`is_totcol()`](https://bricenocenti.github.io/tabxplor/reference/fmt.md)
  /
  [`is_refcol()`](https://bricenocenti.github.io/tabxplor/reference/fmt.md)
  : est-ce une colonne de total / une colonne de référence ?

## Construire des cases de zéro

[`fmt()`](https://bricenocenti.github.io/tabxplor/reference/fmt.md)
construit un vecteur `fmt` directement à partir de nombres — pratique
pour les tests, ou pour ajouter une colonne calculée à la main. Tout ce
dont les moteurs d’affichage et de couleur ont besoin peut être fourni :

``` r
fmt(n = c(10L, 20L, 30L), pct = c(0.1, 0.2, 0.7), display = "pct", digits = 0L)
```

``` r-output
#> <fmt-n-pct[3]>
#> [1] 10% 20% 70%
```

## Tableaux à partir d’effectifs déjà agrégés

Parfois, les données arrivent déjà **croisées** — un tableau d’effectifs
issu d’un rapport, une [`table()`](https://rdrr.io/r/base/table.html),
une matrice de fréquences.
[`tab_counts()`](https://bricenocenti.github.io/tabxplor/reference/tab_counts.md)
construit le même tableau `tabxplor` coloré que
[`tab()`](https://bricenocenti.github.io/tabxplor/reference/tab.md),
mais à partir de ces effectifs plutôt que des observations brutes ;
chaque calcul tourne sur les effectifs, donc le résultat est identique à
ce que
[`tab()`](https://bricenocenti.github.io/tabxplor/reference/tab.md)
aurait produit à partir des observations individuelles.

L’entrée la plus courante est un data frame d’**effectifs au format
long** (une ligne par combinaison, l’effectif dans une colonne) :

``` r
counts <- dplyr::count(gss_simple, marital, race)
tab_counts(counts, marital, race, counts = n, pct = "row", color = TRUE)
```

``` r-output
#> # A tabxplor tab: 7 × 5
#>   marital        White  Black  Other           Total
#>   <fct>         <row%> <row%> <row%>          <row%>
#> 1 Married          82%     9%     9% 100% (n=10 117)
#> 2 Separated        59%    26%    15% 100% (n=   743)
#> 3 Divorced         79%    15%     6% 100% (n= 3 383)
#> 4 Widowed          82%    14%     4% 100% (n= 1 807)
#> 5 Never married    64%    24%    12% 100% (n= 5 416)
#> 6 NA               76%    12%    12% 100% (n=    17)
#> 7 Total            76%    15%     9% 100% (n=21 483)
#> # différence (Total) : -30 -20 -10 -5 +5 +10 +20 +30 ; fond rapport : ÷4 ÷2 ÷1,5 ×1,5 ×2 ×4
```

``` r
# identique à tab(gss_simple, marital, race, pct = "row", color = "diff")
```

Il fond aussi automatiquement une
[`table()`](https://rdrr.io/r/base/table.html) /
[`xtabs()`](https://rdrr.io/r/stats/xtabs.html) / matrice, et lit un
frame au format **large** (une colonne par modalité de la variable en
colonne) via `cols =` / `col_name =` :

``` r
tab_counts(table(gss_simple$marital, gss_simple$race), pct = "row", color = "diff")

wide <- tidyr::pivot_wider(counts, names_from = race, values_from = n)
tab_counts(wide, row_var = marital, cols = c(White, Black, Other),
           col_name = "race", pct = "row", color = "diff")
```

La pondération fonctionne comme dans
[`tab()`](https://bricenocenti.github.io/tabxplor/reference/tab.md) :
passez l’effectif non pondéré dans `counts` et l’effectif pondéré dans
`wt_counts` — les estimations utilisent l’effectif pondéré, les
intervalles de confiance et le Chi2 utilisent le N non pondéré. (La
taille d’échantillon efficace de Kish,
`options(tabxplor.kish_neff = TRUE)`, a besoin des poids individuels,
que des effectifs déjà agrégés ne portent plus, elle ne s’applique donc
pas ici — les intervalles de confiance utilisent le N non pondéré comme
indiqué.) Quand les seuls chiffres disponibles sont des nombres non
entiers (des pourcentages × une base, ou des effectifs pondérés
seulement), les intervalles de confiance et le test du Chi2 sont
désactivés avec un message.

## Passer un tableau groupé en colonnes

Un tableau **groupé** (construit avec des `tab_vars`) empile un
sous-tableau par groupe.
[`tab_spread()`](https://bricenocenti.github.io/tabxplor/reference/tab_spread.md)
bascule les modalités d’une variable de groupe en **colonnes côte à
côte** — idéal pour comparer une même mesure entre les groupes.
Sélectionnez d’abord la colonne de mesure voulue, puis basculez :

``` r
tab(gss_simple, relig, marital, year, pct = "row", totaltab = "no", tot = "row") |>
  dplyr::select(year, relig, Married) |>
  tab_spread(year)
```

``` r-output
#> # A tabxplor tab: 10 × 9
#>    relig                 `2000` `2002` `2004` `2006` `2008` `2010` `2012` `2014`
#>    <fct>                 <row%> <row%> <row%> <row%> <row%> <row%> <row%> <row%>
#>  1 "1-Protestant"           47%    48%    54%    50%    51%    47%    50%    49%
#>  2 "2-Catholic"             47%    49%    57%    53%    49%    44%    47%    47%
#>  3 "3-Other christian"      47%    41%    51%    38%    59%    39%    42%    43%
#>  4 "4-Jewish"               49%    44%    53%    51%    44%    54%    64%    50%
#>  5 "5-Buddhist/Hinduist"    40%    41%    65%    50%    59%    50%    60%    51%
#>  6 "6-Muslim"               42%    38%    69%    71%    31%    36%    62%    67%
#>  7 "7-Other"                30%    48%    43%    30%    25%    33%    42%    39%
#>  8 "8-None"                 38%    33%    40%    37%    38%    36%    32%    37%
#>  9 "NA"                     75%    37%    67%    38%    67%    31%    29%    50%
#> 10 "TOTAL "                 45%    46%    53%    48%    48%    44%    46%    46%
```

`tab(..., spread_vars = year)` fait la même chose en un seul appel. Le
`split_var` de la vignette sur la régression produit un tableau groupé
que vous pouvez basculer de la même façon — voir
[`vignette("tabxplor-reg")`](https://bricenocenti.github.io/tabxplor/articles/tabxplor-reg.md).

## Un score à partir de plusieurs facteurs

[`score_from_lv1()`](https://bricenocenti.github.io/tabxplor/reference/score_from_lv1.md)
transforme une batterie de facteurs oui/non (ou d’accord/pas d’accord)
en un unique **score sommé** : pour chaque ligne, il compte combien des
facteurs listés se trouvent à leur **première modalité**. Ici, nous
comptons combien de deux marqueurs — être marié, et se situer dans la
tranche de revenu supérieure — possède chaque répondant :

``` r
gss_simple |>
  score_from_lv1("score", vars_list = c("married", "income25k")) |>
  tab(relig, score)
```

``` r-output
#> # A tabxplor tab: 10 × 2
#>    relig                      score
#>    <fct>                     <mean>
#>  1 1-Protestant        0.82 (σ0.72)
#>  2 2-Catholic          0.85 (σ0.73)
#>  3 3-Other christian   0.79 (σ0.71)
#>  4 4-Jewish            0.94 (σ0.75)
#>  5 5-Buddhist/Hinduist 0.99 (σ0.76)
#>  6 6-Muslim            0.85 (σ0.71)
#>  7 7-Other             0.74 (σ0.69)
#>  8 8-None              0.74 (σ0.73)
#>  9 NA                  0.60 (σ0.68)
#> 10 Total               0.81 (σ0.73)
```

Le score va de 0 au nombre de facteurs (les valeurs manquantes ne
comptent jamais). Un tel score sommé est exactement l’entrée que
modélise une régression **binomiale groupée** — voir `trials` dans
[`vignette("tabxplor-reg")`](https://bricenocenti.github.io/tabxplor/articles/tabxplor-reg.md).

## Construire plusieurs tableaux d’un coup

[`tab_many()`](https://bricenocenti.github.io/tabxplor/reference/tab_many.md)
— le moteur derrière
[`tab()`](https://bricenocenti.github.io/tabxplor/reference/tab.md) —
accepte des **listes d’arguments** pour construire plusieurs tableaux de
formes différentes en un seul appel. Pour un lot entièrement piloté par
les données,
[`purrr::pmap()`](https://purrr.tidyverse.org/reference/pmap.html) sur
une petite table de spécifications est l’idiome — une ligne par tableau,
une colonne par argument :

``` r
specs <- tibble::tribble(
  ~row_var, ~col_var,  ~pct,
  "race",   "marital", "row",
  "relig",  "party3",  "row",
)
purrr::pmap(specs, \(row_var, col_var, pct)
            tab(gss_simple, all_of(row_var), all_of(col_var), pct = pct))
```

## Options avancées

Au-delà des réglages du quotidien
([`vignette("tabxplor")`](https://bricenocenti.github.io/tabxplor/articles/tabxplor.md)
→ *Options de session*), ces
[`options()`](https://rdrr.io/r/base/options.html) ajustent les
exportateurs et la construction. HTML /
[`tab_kable()`](https://bricenocenti.github.io/tabxplor/reference/tab_html.md)
:

- `tabxplor.tab_kable_engine` — `"html"` (sans dépendance, le défaut) ou
  `"kableExtra"` (ancien).
- `tabxplor.tab_kable_css` — inclure la feuille de style avec chaque
  tableau (`TRUE`) ; mettez `FALSE` dans un document à plusieurs
  tableaux et appelez
  [`tab_css()`](https://bricenocenti.github.io/tabxplor/reference/tab_css.md)
  une seule fois à la place.
- `tabxplor.tab_kable_num_font` / `tabxplor.kable_html_font` — les piles
  de polices CSS des nombres et du texte.
- `tabxplor.kable_popover` — des info-bulles au clic plutôt qu’au
  survol.

Excel / `tab_export("xl")` :

- `tabxplor.xl_font_text` / `tabxplor.xl_font_num` /
  `tabxplor.xl_font_num_stars` — les polices des étiquettes, des nombres
  et des nombres avec étoiles.
- `tabxplor.xl_or_numeric` — garder les rapports de cotes comme des
  nombres plutôt qu’un texte `1/x`.

Console, statistiques et chemins :

- `tabxplor.console_bold` — mettre en gras les cases de référence / de
  total / colorées (détecté automatiquement selon l’éditeur).
- `tabxplor.signif_levels` / `tabxplor.signif_labels` — les seuils de
  p-valeur et les étiquettes d’étoiles.
- `tabxplor.totcol_range` — comment une colonne Total s’imprime quand
  les variables en colonne ont des bases différentes.
- `tabxplor.plot_num_font` — la police des nombres de
  [`tab_plot()`](https://bricenocenti.github.io/tabxplor/reference/tab_plot.md)
  ; `tabxplor.export_dir` — le répertoire d’export par défaut.

Performance et intégration : - `tabxplor.parallel` — construire un
processus par variable en ligne sur un pool en arrière-plan (nécessite
`mirai`) ; `tabxplor.parallel_min` fixe le plus petit nombre de
variables en ligne qui vaille la peine d’être distribué. Libérez le pool
avec
[`tab_parallel_stop()`](https://bricenocenti.github.io/tabxplor/reference/tab_parallel_stop.md).

Voir `?tabxplor-options` pour la liste complète et chaque valeur par
défaut.

## Voir aussi

- [`vignette("tabxplor")`](https://bricenocenti.github.io/tabxplor/articles/tabxplor.md)
  — les tableaux croisés et les aides à la coloration.
- [`vignette("tabxplor-reg")`](https://bricenocenti.github.io/tabxplor/articles/tabxplor-reg.md)
  — les tableaux de régression.
- [`?fmt`](https://bricenocenti.github.io/tabxplor/reference/fmt.md),
  [`?tab_num`](https://bricenocenti.github.io/tabxplor/reference/tab_num.md),
  [`?tab_ci`](https://bricenocenti.github.io/tabxplor/reference/tab_ci.md)
  pour la documentation de référence du type et de ses composants.
