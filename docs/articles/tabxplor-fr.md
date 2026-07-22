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
```

Quand la variable en colonne est **numérique**,
[`tab()`](https://bricenocenti.github.io/tabxplor/reference/tab.md)
affiche sa **moyenne** dans chaque ligne, au lieu de pourcentages :

``` r
tab(gss_simple, marital, age)
```

``` r-output
#> # A tabxplor tab: 7 × 2
#>   marital            age
#>   <fct>           <mean>
#> 1 Married       49 (σ15)
#> 2 Separated     45 (σ13)
#> 3 Divorced      51 (σ13)
#> 4 Widowed       72 (σ13)
#> 5 Never married 34 (σ13)
#> 6 NA            52 (σ17)
#> 7 Total         47 (σ17)
```

Vous pouvez passer **plusieurs variables en ligne et en colonne à la
fois**.

``` r
tab(gss_simple, c(race, relig), c(party3, tvhours), na = "drop", pct = "row")
```

``` r-output
#> # A tabxplor tab: 13 × 7
#> # Groups:         row_var [2]
#>    row_var levels              `1-Democrat` `2-Independent, other`
#>    <fct>   <fct>                     <row%>                 <row%>
#>  1 race    White                        39%                    21%
#>  2 race    Black                        76%                    17%
#>  3 race    Other                        49%                    33%
#>  4 race    Total                        45%                    21%
#> 
#>  5 relig   1-Protestant                 43%                    17%
#>  6 relig   2-Catholic                   46%                    22%
#>  7 relig   3-Other christian            42%                    24%
#>  8 relig   4-Jewish                     68%                    12%
#>  9 relig   5-Buddhist/Hinduist          57%                    29%
#> 10 relig   6-Muslim                     66%                    22%
#> 11 relig   7-Other                      48%                    29%
#> 12 relig   8-None                       50%                    31%
#> 13 relig   Total                        45%                    21%
#> # ℹ 3 more variables: `3-Republican` <row%>, Total <row%>, tvhours <mean>
```

`levels = "first"` ne garde que la première modalité de chaque facteur
en colonne, ce qui est pratique pour afficher de façon compacte de
nombreux facteurs binaires, comme des questions d’enquête à réponses
multiples, tous en même temps :

``` r
tab(gss_simple, relig, c(married, black, income25k), pct = "row", levels = "first", na = "drop", cleannames = TRUE)
```

``` r-output
#> # A tabxplor tab: 9 × 4
#>   relig             Married  Black `$25000 or more`
#>   <fct>              <row%> <row%>           <row%>
#> 1 Protestant            50%    21%              32%
#> 2 Catholic              50%     4%              35%
#> 3 Other christian       44%    18%              35%
#> 4 Jewish                51%     3%              43%
#> 5 Buddhist/Hinduist     51%     5%              47%
#> 6 Muslim                53%    34%              32%
#> 7 Other                 37%    13%              37%
#> 8 None                  37%    11%              37%
#> 9 Total                 47%    15%              34%
```

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

``` r-output
#> # A tabxplor tab: 17 × 6
#> # Groups:         rincome [5]
#>    rincome           race                    `1-Democrat` `2-Independent, other`
#>    <fct>             <fct>                         <row%>                 <row%>
#>  1 1-Lt $10000       White                            38%                    26%
#>  2 1-Lt $10000       Black                            67%                    22%
#>  3 1-Lt $10000       Other                            49%                    31%
#>  4 1-Lt $10000       Total 1-Lt $10000                45%                    26%
#> 
#>  5 2-$10000 to 14999 White                            40%                    27%
#>  6 2-$10000 to 14999 Black                            76%                    14%
#>  7 2-$10000 to 14999 Other                            43%                    44%
#>  8 2-$10000 to 14999 Total 2-$10000 to 14999          47%                    26%
#> 
#>  9 3-$15000 to 24999 White                            38%                    26%
#> 10 3-$15000 to 24999 Black                            79%                    15%
#> 11 3-$15000 to 24999 Other                            45%                    39%
#> 12 3-$15000 to 24999 Total 3-$15000 to 24999          46%                    25%
#> 
#> 13 4-$25000 or more  White                            39%                    17%
#> 14 4-$25000 or more  Black                            81%                    12%
#> 15 4-$25000 or more  Other                            56%                    22%
#> 16 4-$25000 or more  Total 4-$25000 or more           45%                    16%
#> 
#> 17 Ensemble          Total Ensemble                   45%                    20%
#> # ℹ 2 more variables: `3-Republican` <row%>, Total <row%>
```

Quand vous passez plusieurs **variables en ligne** *sans* `tab_vars`,
[`tab()`](https://bricenocenti.github.io/tabxplor/reference/tab.md)
fusionne par défaut les tableaux jumeaux en un seul tableau.
`output_list = TRUE` renvoie plutôt une **liste avec un tableau par
variable en ligne** (avec `tab_vars`, le résultat est toujours une
liste) :

``` r
tab(gss_simple, c(married, income25k), race, pct = "row", output_list = TRUE)
```

``` r-output
<!-- KNITR_ASIS_OUTPUT_TOKEN --><style>.p1,.p2,.p3,.p4,.m1,.m2,.m3,.m4{font-weight:bold;}
.tabxplor-tab,.tabxplor-tab table{border-collapse:collapse;border-top-width:0;border-bottom-width:0;margin:0;font-family:"DejaVu Sans Condensed","DejaVu Sans",Arial,helvetica,sans-serif;}
.tabxplor-caption{text-align:left;font-weight:bold;font-size:110%;white-space:normal;}
.tabxplor-tab tfoot{font-size:80%;text-align:left;}
.tabxplor-tab th,.tabxplor-tab td{padding:3px 4px;vertical-align:top;line-height:1.1;}
.tabxplor-tab table td,.tabxplor-tab table th{border-width:0;}
.tabxplor-tab table tbody tr:not(:has(td:not(:empty)))>*{border-top-style:solid;border-top-width:1px;padding:0;line-height:0;}
.tabxplor-tab table td:empty,.tabxplor-tab table th:empty{padding:0;}
.tabxplor-tab table tbody tr:has(td:not(:empty)) td:empty,.tabxplor-tab table thead tr:has(th:not(:empty)) th:empty{border-left-style:solid;border-left-width:1px;}
.tabxplor-tab table > thead > tr:first-child > *{border-top-style:solid;border-top-width:1px;}
.tabxplor-tab table > tbody > tr:last-child > *{border-bottom-style:solid;border-bottom-width:1px;}
.tabxplor-tab table > tbody > tr:has(td:not(:empty)) > *:last-child,.tabxplor-tab table > thead > tr > *:last-child{border-right-style:solid;border-right-width:1px;}
.tabxplor-tab table > tbody > tr:has(td:not(:empty)) > *:first-child,.tabxplor-tab table > thead > tr > *:first-child{border-left-style:solid;border-left-width:1px;}
.tabxplor-tab p{font-size:80%;}
.tabxplor-tab thead th{font-weight:bold;font-size:90%;text-align:center;vertical-align:bottom;line-height:1;border-top-width:0;border-bottom-style:solid;border-bottom-width:1px;}
.tabxplor-tab > thead > tr:first-child > *:not(.tx-span){border-top-style:solid;border-top-width:1px;}
.tabxplor-tab .tx-span{font-weight:bold;font-size:90%;text-align:center;border-bottom-style:solid;border-bottom-width:1px;}
.tabxplor-tab .tx-r{text-align:right;}
.tabxplor-tab .tx-l{text-align:left;}
.tabxplor-tab thead .tx-r,.tabxplor-tab thead .tx-l{text-align:center;}
.tabxplor-tab .tx-num{white-space:nowrap;}
.tabxplor-tab td.tx-num{font-family:"Cascadia Mono", "Cascadia Code", Menlo, Consolas, "DejaVu Sans Mono", monospace;font-size:1.1em;line-height:1;}
.tabxplor-tab .tx-br{border-right-style:solid;border-right-width:1px;}
.tabxplor-tab .tx-bl{border-left-style:solid;border-left-width:1px;}
.tabxplor-tab .tx-lbl{vertical-align:middle;text-align:center;}
.tabxplor-tab .tx-vname{writing-mode:vertical-rl;transform:rotate(180deg);white-space:normal;padding:4px 2px;}
.tabxplor-tab .tx-b,.tabxplor-tab tr.tx-b{font-weight:bold;}
.tabxplor-tab tr.tx-bt>*{border-top-style:solid;border-top-width:1px;}
.tabxplor-tab tr.tx-bb>*,.tabxplor-tab td.tx-bb{border-bottom-style:solid;border-bottom-width:1px;}
.tabxplor-tab tr.tx-bb2>*{border-bottom-style:solid;border-bottom-width:2px;}
.tabxplor-tab .tx-foot{width:0;min-width:100%;}
.tabxplor-tab .tx-pill{border-radius:4px;padding:1px 4px;}
.tooltip-inner{max-width:none;white-space:nowrap;}
.popover{max-width:none;}
.popover-body,.popover-content{padding:6px;white-space:nowrap;}
.tabxplor-tab{color:#000000;background:#ffffff;}
.tabxplor-tab th,.tabxplor-tab td{border-color:#000000;}
.tabxplor-tab tbody tr:hover{background:#FFFCE5;}
.g1{color:#9f9f9f;}
.g2{color:#111111;}
.tabxplor-caption{color:#000000;}
.p1{color:#02A5B3;}
.p2{color:#0891C9;}
.p3{color:#0267C7;}
.p4{color:#300DFD;}
.m1{color:#DCA331;}
.m2{color:#DE7C01;}
.m3{color:#DD5301;}
.m4{color:#D60103;}
.o1{background-color:#DFFCFF;}
.o2{background-color:#D7EFFF;}
.o3{background-color:#CEE3FF;}
.o4{background-color:#BBCCFF;}
.u1{background-color:#FFF4E1;}
.u2{background-color:#FFE6D3;}
.u3{background-color:#FFD7C8;}
.u4{background-color:#FFBAAF;}</style>
<table class="tabxplor-tab"><thead><tr><th class="tx-span" colspan="1"></th><th class="tx-span" colspan="3">race</th><th class="tx-span" colspan="1"></th></tr><tr><th class="tx-l tx-br tx-bl tx-rv">married</th><th class="tx-r tx-num">White</th><th class="tx-r tx-num">Black</th><th class="tx-r tx-num">Other</th><th class="tx-r tx-num tx-br tx-bl tx-tot">Total</th></tr></thead><tbody><tr><td class="tx-l tx-br tx-bl tx-rv">01-Married</td><td class="tx-r tx-num g2" data-toggle="tooltip" data-container="body" data-placement="auto right" title="diff: +6% ; ratio: ×1.1 ; n: 8 316">82%</td><td class="tx-r tx-num g2" data-toggle="tooltip" data-container="body" data-placement="auto right" title="diff: -6% ; ratio: ÷1.7 ; n: 869">9%</td><td class="tx-r tx-num g2" data-toggle="tooltip" data-container="body" data-placement="auto right" title="diff: +0% ; ratio: ×1 ; n: 932">9%</td><td class="tx-r tx-num tx-br tx-bl tx-tot tx-b" data-toggle="tooltip" data-container="body" data-placement="auto right" title="n: 10 117">100%<span style="font-weight:normal;"> (n=10 117)</span></td></tr>
<tr><td class="tx-l tx-br tx-bl tx-rv">02-Not married</td><td class="tx-r tx-num g2" data-toggle="tooltip" data-container="body" data-placement="auto right" title="diff: -5% ; ratio: ÷1.1 ; n: 8 079">71%</td><td class="tx-r tx-num g2" data-toggle="tooltip" data-container="body" data-placement="auto right" title="diff: +5% ; ratio: ×1.4 ; n: 2 260">20%</td><td class="tx-r tx-num g2" data-toggle="tooltip" data-container="body" data-placement="auto right" title="diff: -0% ; ratio: ×1 ; n: 1 027">9%</td><td class="tx-r tx-num tx-br tx-bl tx-tot tx-b" data-toggle="tooltip" data-container="body" data-placement="auto right" title="n: 11 366">100%<span style="font-weight:normal;"> (n=11 366)</span></td></tr>
<tr class="tx-b tx-bt tx-bb tx-bb2"><td class="tx-l tx-br tx-bl tx-rv">Total</td><td class="tx-r tx-num tx-b" data-toggle="tooltip" data-container="body" data-placement="auto right" title="ref ; n: 16 395">76%</td><td class="tx-r tx-num tx-b" data-toggle="tooltip" data-container="body" data-placement="auto right" title="ref ; n: 3 129">15%</td><td class="tx-r tx-num tx-b" data-toggle="tooltip" data-container="body" data-placement="auto right" title="ref ; n: 1 959">9%</td><td class="tx-r tx-num tx-br tx-bl tx-tot tx-b" data-toggle="tooltip" data-container="body" data-placement="auto right" title="n: 21 483">100%<span style="font-weight:normal;"> (n=21 483)</span></td></tr></tbody></table>
<br>
<table class="tabxplor-tab"><thead><tr><th class="tx-span" colspan="1"></th><th class="tx-span" colspan="3">race</th><th class="tx-span" colspan="1"></th></tr><tr><th class="tx-l tx-br tx-bl tx-rv">income25k</th><th class="tx-r tx-num">White</th><th class="tx-r tx-num">Black</th><th class="tx-r tx-num">Other</th><th class="tx-r tx-num tx-br tx-bl tx-tot">Total</th></tr></thead><tbody><tr><td class="tx-l tx-br tx-bl tx-rv">01-$25000 or more</td><td class="tx-r tx-num g2" data-toggle="tooltip" data-container="body" data-placement="auto right" title="diff: +3% ; ratio: ×1 ; n: 5 856">80%</td><td class="tx-r tx-num g2" data-toggle="tooltip" data-container="body" data-placement="auto right" title="diff: -3% ; ratio: ÷1.2 ; n: 886">12%</td><td class="tx-r tx-num g2" data-toggle="tooltip" data-container="body" data-placement="auto right" title="diff: -1% ; ratio: ÷1.1 ; n: 621">8%</td><td class="tx-r tx-num tx-br tx-bl tx-tot tx-b" data-toggle="tooltip" data-container="body" data-placement="auto right" title="n: 7 363">100%<span style="font-weight:normal;"> (n= 7 363)</span></td></tr>
<tr><td class="tx-l tx-br tx-bl tx-rv">02-Less than 25k</td><td class="tx-r tx-num g2" data-toggle="tooltip" data-container="body" data-placement="auto right" title="diff: -2% ; ratio: ×1 ; n: 10 539">75%</td><td class="tx-r tx-num g2" data-toggle="tooltip" data-container="body" data-placement="auto right" title="diff: +1% ; ratio: ×1.1 ; n: 2 243">16%</td><td class="tx-r tx-num g2" data-toggle="tooltip" data-container="body" data-placement="auto right" title="diff: +0% ; ratio: ×1 ; n: 1 338">9%</td><td class="tx-r tx-num tx-br tx-bl tx-tot tx-b" data-toggle="tooltip" data-container="body" data-placement="auto right" title="n: 14 120">100%<span style="font-weight:normal;"> (n=14 120)</span></td></tr>
<tr class="tx-b tx-bt tx-bb tx-bb2"><td class="tx-l tx-br tx-bl tx-rv">Total</td><td class="tx-r tx-num tx-b" data-toggle="tooltip" data-container="body" data-placement="auto right" title="ref ; n: 16 395">76%</td><td class="tx-r tx-num tx-b" data-toggle="tooltip" data-container="body" data-placement="auto right" title="ref ; n: 3 129">15%</td><td class="tx-r tx-num tx-b" data-toggle="tooltip" data-container="body" data-placement="auto right" title="ref ; n: 1 959">9%</td><td class="tx-r tx-num tx-br tx-bl tx-tot tx-b" data-toggle="tooltip" data-container="body" data-placement="auto right" title="n: 21 483">100%<span style="font-weight:normal;"> (n=21 483)</span></td></tr></tbody></table>

<!-- KNITR_ASIS_OUTPUT_TOKEN -->
```

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

``` r-output
#> # A tabxplor tab: 4 × 6
#>   race  `1-Democrat` `2-Independent, other` `3-Republican`  `NA`           Total
#>   <cha>       <row%>                 <row%>         <row%> <row>          <row%>
#> 1 White          39%                    21%            40%    1% 100% (n=16 395)
#> 2 Black          75%                    16%             8%    1% 100% (n= 3 129)
#> 3 Other          48%                    32%            18%    1% 100% (n= 1 959)
#> 4 Total          45%                    21%            33%    1% 100% (n=21 483)
#> # différence (Total) : -30 -20 -10 -5 +5 +10 +20 +30
```

`color = TRUE` choisit automatiquement un schéma sensé pour chaque type
de colonne (différences et rapport pour les pourcentages, seulement les
rapports pour les moyennes, …) ; vérifiez lequel dans la légende :

``` r
tab(gss_simple, rincome, c(party3, marital), pct = "row", color = TRUE)
```

``` r-output
#> # A tabxplor tab: 6 × 12
#>   rincome           `1-Democrat` `2-Independent, other` `3-Republican` NA_party3
#>   <fct>                   <row%>                 <row%>         <row%>    <row%>
#> 1 1-Lt $10000                44%                    25%            30%        1%
#> 2 2-$10000 to 14999          46%                    26%            27%        1%
#> 3 3-$15000 to 24999          45%                    25%            29%        0%
#> 4 4-$25000 or more           45%                    16%            38%        0%
#> 5 NA                         45%                    22%            32%        1%
#> 6 Total                      45%                    21%            33%        1%
#> # ℹ 7 more variables: Married <row%>, Separated <row%>, Divorced <row%>,
#> #   Widowed <row%>, `Never married` <row%>, NA_marital <row%>, Total <row%>
#> # différence (Total) : -30 -20 -10 -5 +5 +10 +20 +30 ; fond rapport : ÷4 ÷2 ÷1,5 ×1,5 ×2 ×4
```

Les colonnes numériques sont colorées de la même façon, sur leurs
**moyennes** (ici, les heures de télévision par jour selon le revenu) :

``` r
tab(gss_simple, rincome, tvhours, color = "diff")
```

``` r-output
#> # A tabxplor tab: 6 × 2
#>   rincome              tvhours
#>   <fct>                 <mean>
#> 1 1-Lt $10000       3.1 (σ2.8)
#> 2 2-$10000 to 14999 3.0 (σ2.4)
#> 3 3-$15000 to 24999 2.8 (σ2.1)
#> 4 4-$25000 or more  2.2 (σ1.7)
#> 5 NA                3.6 (σ3.1)
#> 6 Total             3.0 (σ2.6)
#> # différence standardisée (Total) : -0,8 -0,5 -0,2 +0,2 +0,5 +0,8
```

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

``` r-output
#> # A tabxplor tab: 9 × 8
#>   year  Married Separated Divorced Widowed `Never married`  `NA`           Total
#>   <cha>  <row%>    <row%>   <row%>  <row%>          <row%> <row>          <row%>
#> 1 2000      45%        4%      16%     10%             25%    0% 100% (n= 2 817)
#> 2 2002      46%        3%      16%      9%             26%    0% 100% (n= 2 765)
#> 3 2004      53%        3%      15%      7%             22%    0% 100% (n= 2 812)
#> 4 2006      48%        3%      16%      8%             24%    0% 100% (n= 4 510)
#> 5 2008      48%        3%      14%      8%             26%    0% 100% (n= 2 023)
#> 6 2010      44%        3%      17%      9%             28%    0% 100% (n= 2 044)
#> 7 2012      46%        3%      16%      8%             27%    0% 100% (n= 1 974)
#> 8 2014      46%        3%      16%      8%             27%    0% 100% (n= 2 538)
#> 9 Total     47%        3%      16%      8%             25%    0% 100% (n=21 483)
#> # différence (2000) : -30 -20 -10 -5 +5 +10 +20 +30
```

``` r
tab(gss_simple, rincome, party3, race, na = "drop", pct = "row", color = TRUE, comp="all")
```

``` r-output
#> # A tabxplor tab: 16 × 6
#> # Groups:         race [4]
#>    race     rincome           `1-Democrat` `2-Independent, other` `3-Republican`
#>    <fct>    <fct>                   <row%>                 <row%>         <row%>
#>  1 White    1-Lt $10000                38%                    26%            36%
#>  2 White    2-$10000 to 14999          40%                    27%            33%
#>  3 White    3-$15000 to 24999          38%                    26%            36%
#>  4 White    4-$25000 or more           39%                    17%            45%
#>  5 White    Total White                39%                    20%            41%
#> 
#>  6 Black    1-Lt $10000                67%                    22%            11%
#>  7 Black    2-$10000 to 14999          76%                    14%            10%
#>  8 Black    3-$15000 to 24999          79%                    15%             6%
#>  9 Black    4-$25000 or more           81%                    12%             7%
#> 10 Black    Total Black                77%                    15%             8%
#> 
#> 11 Other    1-Lt $10000                49%                    31%            20%
#> 12 Other    2-$10000 to 14999          43%                    44%            13%
#> 13 Other    3-$15000 to 24999          45%                    39%            16%
#> 14 Other    4-$25000 or more           56%                    22%            22%
#> 15 Other    Total Other                51%                    30%            19%
#> 
#> 16 Ensemble Total Ensemble             45%                    20%            34%
#> # ℹ 1 more variable: Total <row%>
#> # différence (Total) : -30 -20 -10 -5 +5 +10 +20 +30 ; fond rapport : ÷4 ÷2 ÷1,5 ×1,5 ×2 ×4
```

**Une référence différente pour chaque variable.** `ref` est
réinterprété par `pct`. Sous des pourcentages en **ligne** (ou des
moyennes), il choisit une **ligne** de référence : un vecteur *nommé*
donne alors à chaque variable en ligne la sienne — ici `race` est lu par
rapport à sa première ligne, `relig` par rapport à son Total :

``` r
tab(gss_simple, c(race, relig), party3, pct = "row", color = "diff",
    ref = c(race = "first", relig = "tot"), na = "drop")
```

``` r-output
#> # A tabxplor tab: 13 × 6
#> # Groups:         row_var [2]
#>    row_var levels              `1-Democrat` `2-Independent, other`
#>    <fct>   <fct>                     <row%>                 <row%>
#>  1 race    White                        39%                    21%
#>  2 race    Black                        76%                    17%
#>  3 race    Other                        49%                    33%
#>  4 race    Total                        45%                    21%
#> 
#>  5 relig   1-Protestant                 43%                    17%
#>  6 relig   2-Catholic                   46%                    22%
#>  7 relig   3-Other christian            42%                    24%
#>  8 relig   4-Jewish                     68%                    12%
#>  9 relig   5-Buddhist/Hinduist          57%                    29%
#> 10 relig   6-Muslim                     66%                    22%
#> 11 relig   7-Other                      48%                    29%
#> 12 relig   8-None                       50%                    31%
#> 13 relig   Total                        45%                    21%
#> # ℹ 2 more variables: `3-Republican` <row%>, Total <row%>
#> # différence (réf.) : -30 -20 -10 -5 +5 +10 +20 +30
```

Sous des pourcentages en **colonne**, `ref` choisit plutôt une
**colonne** de référence, vectorisé sur les variables en colonne — nommé
(`ref = c(party3 = "first", marital = "tot")`) ou positionnel, une
valeur par variable en colonne :

``` r
tab(gss_simple, race, c(party3, marital), pct = "col", color = "diff",
    ref = c("first", "tot"), na = "drop")
```

``` r-output
#> # A tabxplor tab: 5 × 10
#>   race   `1-Democrat` `2-Independent, other` `3-Republican`    Married Separated
#>   <fct>  <col%-mixed>           <col%-mixed>   <col%-mixed> <col%-mix> <col%-mi>
#> 1 White           66%                    75%            92%        82%       59%
#> 2 Black           24%                    11%             3%         9%       26%
#> 3 Other           10%                    14%             5%         9%       15%
#> 4 Total          100%                   100%           100%       100%      100%
#> 5 n             9 679                  4 512          7 137     10 117       743
#> # ℹ 4 more variables: Divorced <col%-mixed>, Widowed <col%-mixed>,
#> #   `Never married` <col%-mixed>, Total <col%-mixed>
#> # party3 : différence (1-Democrat) : -30 -20 -10 -5 +5 +10 +20 +30
#> # marital : différence (Total) : -30 -20 -10 -5 +5 +10 +20 +30
```

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

``` r-output
#> # A tabxplor tab: 4 × 6
#>   race  `1-Democrat` `2-Independent, other` `3-Republican`  `NA`           Total
#>   <cha>       <row%>                 <row%>         <row%> <row>          <row%>
#> 1 White          39%                    21%            40%    1% 100% (n=16 395)
#> 2 Black          75%                    16%             8%    1% 100% (n= 3 129)
#> 3 Other          48%                    32%            18%    1% 100% (n= 1 959)
#> 4 Total          45%                    21%            33%    1% 100% (n=21 483)
#> # différence (Total) : -30 -20 -10 -5 +5 +10 +20 +30 [gris : non significatif ou sous ±5 points]
```

``` r
gss_simple |>
  dplyr::filter(year == "2012") |> # n=1 974
  tab(race, party3, pct = "row", color = "diff", color_signif = "guaranteed_effect")
```

``` r-output
#> # A tabxplor tab: 4 × 6
#>   race   `1-Democrat` `2-Independent, other` `3-Republican`  `NA`          Total
#>   <fct>        <row%>                 <row%>         <row%> <row>         <row%>
#> 1 White           40%                    22%            37%    1% 100% (n=1 477)
#> 2 Black           81%                    14%             5%    0% 100% (n=  301)
#> 3 Other           47%                    32%            19%    2% 100% (n=  196)
#> 4 Total           47%                    22%            30%    1% 100% (n=1 974)
#> # différence (Total) : -25 -15 -5 -0 +0 +5 +15 +25 [tout ce qui est significatif est coloré, marge d'erreur déduite]
```

Sur de **petits échantillons**, un pourcentage d’apparence marquée peut
reposer sur une poignée de répondants. `n_min =` est un filtre purement
visuel, appliqué en dernier : il masque les cases dont la base (non
pondérée) est sous le seuil, et retire entièrement une ligne quand sa
plus grande base est trop faible. Ici, les religions les plus rares
disparaissent :

``` r
tab(gss_simple, relig, race, pct = "row", n_min = 400)
```

``` r-output
#> # A tabxplor tab: 5 × 5
#>   relig              White  Black  Other           Total
#>   <fct>             <row%> <row%> <row%>          <row%>
#> 1 1-Protestant         75%    21%     4% 100% (n=10 846)
#> 2 2-Catholic           78%     4%    18% 100% (n= 5 124)
#> 3 3-Other christian    72%    18%    10% 100% (n=   784)
#> 4 8-None               80%    11%     9% 100% (n= 3 523)
#> 5 Total                76%    15%     9% 100% (n=21 483)
```

Une alternative est de garder les lignes et colonnes rares, mais de les
regrouper toutes dans une modalité « Autres » :

``` r
tab(gss_simple, relig, race, pct = "row",  other_if_less_than = 400)
```

``` r-output
#> # A tabxplor tab: 7 × 5
#>   relig              White  Black  Other           Total
#>   <fct>             <row%> <row%> <row%>          <row%>
#> 1 1-Protestant         75%    21%     4% 100% (n=10 846)
#> 2 2-Catholic           78%     4%    18% 100% (n= 5 124)
#> 3 3-Other christian    72%    18%    10% 100% (n=   784)
#> 4 8-None               80%    11%     9% 100% (n= 3 523)
#> 5 Others               68%    10%    22% 100% (n= 1 098)
#> 6 NA                   67%    18%    16% 100% (n=   108)
#> 7 Total                76%    15%     9% 100% (n=21 483)
```

## Intervalles de confiance, tests et contributions

Affichez les intervalles de confiance du pourcentage ou de la moyenne de
chaque case avec `ci = "cell"` :

``` r
tab(gss_simple, race, party3, pct = "row", ci = "cell") # par défaut, conf_level = 0.95
```

``` r-output
#> # A tabxplor tab: 4 × 6
#>   race   `1-Democrat` `2-Independent, other` `3-Republican`   `NA`
#>   <fct>        <row%>                 <row%>         <row%> <row%>
#> 1 White      [38;40]%               [20;21]%       [39;41]% [0;1]%
#> 2 Black      [73;76]%               [15;18]%         [7;9]% [1;2]%
#> 3 Other      [46;50]%               [30;34]%       [16;20]% [1;2]%
#> 4 Total           45%                    21%            33%     1%
#> # ℹ 1 more variable: Total <row%>
```

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

``` r-output
#> # A tabxplor tab: 4 × 6
#>   race    `1-Democrat` `2-Independent, other` `3-Republican`       `NA`
#>   <fct>         <row%>                 <row%>         <row%>     <row%>
#> 1 White  40% [-10;-3]%          22%   [-3;3]% 37%    [4;10]% 1% [-1;1]%
#> 2 Black  81%  [28;38]%          14% [-12;-3]%  5% [-28;-22]% 0% [-1;1]%
#> 3 Other  47%   [-7;7]%          32%   [4;17]% 19%  [-17;-5]% 2% [-0;4]%
#> 4 Total            47%                    22%            30%         1%
#> # ℹ 1 more variable: Total <row%>
#> # différence (Total) : -25 -15 -5 -0 +0 +5 +15 +25 ; fond rapport : ÷2,667 ÷1,333 ÷1 ×1 ×1,333 ×2,667 [tout ce qui est significatif est coloré, marge d'erreur déduite]
```

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

``` r-output
#> # A tabxplor tab: 6 × 7
#>   rincome              `1-Democrat` `2-Independent, other` `3-Republican`
#>   <fct>                      <row%>                 <row%>         <row%>
#> 1 1-Lt $10000       40%** [-15;-1]%        33%***  [5;18]% 27%    [-9;3]%
#> 2 2-$10000 to 14999 45%    [-12;7]%        27%    [-2;15]% 26%   [-12;5]%
#> 3 3-$15000 to 24999 50%    [-5;10]%        25%    [-2;10]% 25%   [-12;1]%
#> 4 4-$25000 or more  49%     [-2;7]%        16%*** [-9;-2]% 35%**   [0;8]%
#> 5 NA                47%     [-4;4]%        22%     [-3;4]% 30%    [-4;3]%
#> 6 Total                       47%                   22%             30%  
#> # ℹ 3 more variables: `NA` <row%>, Total <row%>, tvhours <mean>
#> # *** : chiffre significativement différent de celui de la modalité de référence (en gras), au seuil de confiance de 99 % ; ** : au seuil de 95 % ; * : au seuil de 90 % ; aucune étoile : non significatif.
```

**Afficher deux nombres dans une même case.** `display` ne sert pas
qu’aux intervalles de confiance : il accepte un **gabarit
[`{}`](https://rdrr.io/r/base/Paren.html)** qui combine n’importe quels
champs de la case. Par exemple, `display = "{pct} ({diff})"` imprime
chaque pourcentage suivi de sa différence à la référence, et
`"{pct} (n={n})"` le fait suivre de l’effectif :

``` r
tab(gss_simple, race, party3, pct = "row", color = "diff", display = "{pct} ({diff})")
```

``` r-output
#> # A tabxplor tab: 4 × 6
#>   race   `1-Democrat` `2-Independent, other` `3-Republican`     `NA`
#>   <fct>        <row%>                 <row%>         <row%>   <row%>
#> 1 White    39% ( -6%)             21% ( -0%)     40% ( +7%) 1% (-0%)
#> 2 Black    75% (+30%)             16% ( -5%)      8% (-26%) 1% (+0%)
#> 3 Other    48% ( +3%)             32% (+11%)     18% (-15%) 1% (+1%)
#> 4 Total    45% ( +0%)             21% ( +0%)     33% ( +0%) 1% (+0%)
#> # ℹ 1 more variable: Total <row%>
#> # différence (Total) : -30 -20 -10 -5 +5 +10 +20 +30
```

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

``` r-output
#> |      | Tests                  |   party3 |   |     tvhours |
#> |:-----|:-----------------------|---------:|:-:|------------:|
#> | race | N                      |   21 483 |   |      11 337 |
#> |      | pvalue (Chi2, Welch F) |   <0.01% |   |      <0.01% |
#> |      | Cramér's V, eta2       | V = 0.21 |   | eta2 = 0.04 |
#> 
#> # A tabxplor tab: 4 × 7
#>   race  `1-Democrat` `2-Independent, other` `3-Republican`  `NA`           Total
#>   <cha>       <row%>                 <row%>         <row%> <row>          <row%>
#> 1 White          39%                    21%            40%    1% 100% (n=16 395)
#> 2 Black          75%                    16%             8%    1% 100% (n= 3 129)
#> 3 Other          48%                    32%            18%    1% 100% (n= 1 959)
#> 4 Total          45%                    21%            33%    1% 100% (n=21 483)
#> # ℹ 1 more variable: tvhours <mean>
```

`color = "contrib"` colore les cases selon leur **contribution au Chi2**
— les cases qui ressortiraient dans une analyse des correspondances :

``` r
tab(gss_simple, race, party3, color = "contrib")
```

``` r-output
#> |      | Tests         |   party3 |
#> |:-----|:--------------|---------:|
#> | race | N             |   21 483 |
#> |      | pvalue (Chi2) |   <0.01% |
#> |      | Cramér's V    | V = 0.21 |
#> 
#> # A tabxplor tab: 4 × 6
#>   race   `1-Democrat` `2-Independent, other` `3-Republican` `NA`  Total
#>   <fct>           <n>                    <n>            <n>  <n>    <n>
#> 1 White         6 390                  3 365          6 546   94 16 395
#> 2 Black         2 344                    513            236   36  3 129
#> 3 Other           945                    634            355   25  1 959
#> 4 Total         9 679                  4 512          7 137  155 21 483
#> # contribution au Chi2 (p. r. à la moyenne) : ×10 ×5 ×2 ×1 ×1 ×2 ×5 ×10
```

``` r
# tab(gss_simple, race, party3, pct="row", color = "contrib") # fonctionne avec pct, mais indépendant des lignes/colonnes
```

Voir plus bas le détail de la composition des intervalles de confiance
et des couleurs.

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
tab(gss_simple, party3, c(race, relig, tvhours), pct = "row") |>
  tab_export(transpose = TRUE)
```

- **Une seule feuille de style pour tout un document.** Dans un rapport
  `.Rmd`/`.qmd`,
  [`tab_css()`](https://bricenocenti.github.io/tabxplor/reference/tab_css.md)
  écrit le CSS des couleurs une seule fois, et chaque
  [`tab_kable()`](https://bricenocenti.github.io/tabxplor/reference/tab_html.md)
  suivant n’émet que des classes : un unique `theme` — y compris
  `"auto"`, qui suit le mode clair/sombre du lecteur — met en forme tous
  les tableaux d’un coup :

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

``` r-output
#> # A tabxplor tab: 4 × 8
#>   race  Married Separated Divorced Widowed `Never married`  `NA`           Total
#>   <cha>  <row%>    <row%>   <row%>  <row%>          <row%> <row>          <row%>
#> 1 White     51%        3%      16%      9%             21%    0% 100% (n=16 395)
#> 2 Other     48%        6%      11%      4%             32%    0% 100% (n= 1 959)
#> 3 Black     28%        6%      16%      8%             42%    0% 100% (n= 3 129)
#> 4 Total     47%        3%      16%      8%             25%    0% 100% (n=21 483)
```

**Titrer et annoter.** `subtext =` imprime une ou plusieurs lignes de
légende sous un tableau (une source de données, une note).
[`set_caption()`](https://bricenocenti.github.io/tabxplor/reference/set_caption.md)
donne à un tableau un **titre qui survit à un pipeline dplyr**, et
chaque exportateur l’utilise comme titre du tableau :

``` r
tab(gss_simple, race, marital, pct = "row", subtext = "Source : GSS, 2000-2014") |>
  set_caption("Titre personnalisé")
```

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
: - `method_diff = "ac"` (Agresti-Caffo) ou `"wald"` pour une différence
de pourcentages ; - `method_mean_diff = "student"` (à variance commune,
l’intervalle à deux groupes des MCO) pour une différence de moyennes ; -
`method_mean_ratio = "quasipoisson"` ou `"poisson"` pour un rapport de
moyennes. - Les intervalles du risque relatif (`method_ratio = "katz"`)
et du rapport de cotes (Woolf) n’ont pas d’alternative. - Une
**différence** de moyennes est colorée **standardisée** — le Δ de Glass,
la différence divisée par l’écart-type de la référence — donc les seuils
de couleur `mean_diff` se lisent en unités d’écart-type, sauf si
l’utilisateur fournit une échelle de seuils personnalisée. - Pour un
facteur à **3 modalités ou plus**, le rapport de cotes (et son
intervalle) compare chaque modalité à la modalité de base `ref2` : c’est
un rapport de risques relatifs (RRR) (la même quantité observée que
celle modélisée par une régression logistique multinomiale).
L’intervalle de l’OR n’est calculé que lorsque `color_signif` ou `stars`
en a besoin.

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

**`color_signif` transforme cet intervalle en politique de
coloration.** - `"ignore"` colore **chaque** case selon la taille de son
**effet observé**, par exemple la différence observée avec la ligne
Total. Les cases grises ont un effet observé sous le seuil (par exemple,
des différences de moins de ±5 points de pourcentage). -
`"grey_non_signif"` et `"guaranteed_effect"` colorent tous deux
**seulement les cases significatives**, mais diffèrent par la *base
d’intensité* : + `grey_non_signif` colore selon l’effet **observé**,
comme « ignore », en grisant les petits écarts, mais il **grise aussi
tout grand écart qui se révèle non significatif**. Idéal pour les grands
échantillons. + `guaranteed_effect` colore selon l’effet **garanti** —
la borne de confiance (le plancher de l’IC), le **plus petit écart
assuré à un seuil de confiance donné** (95 % par défaut) — donc ses
couleurs sont plus ternes et prudentes, mais **toutes les différences
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
