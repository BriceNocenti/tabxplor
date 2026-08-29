# Histoire de vie: leisure, work and beliefs in France, 2003

A French national survey, used to show what **adjustment** does to a
relationship: going to the cinema is strongly patterned by occupation,
and also by age – and the two are entangled. See the *Reading a
regression* article for the analysis. It also carries a real sampling
weight (`poids`), so it is the data set to try `wt =` on; see
[`vignette("tabxplor-weights")`](https://bricenocenti.github.io/tabxplor/articles/tabxplor-weights.md).

## Usage

``` r
questionr_hdv
```

## Format

A tibble of 2 000 rows and 20 columns. The ones the article uses:

- cinema:

  Went to the cinema in the last 12 months? "Oui" first.

- qualif:

  Occupational qualification, 7 levels, "Cadre" (senior professional)
  first. 347 values are missing, as in the original.

- age:

  Age in years, 18 to 97.

- poids:

  The survey's own sampling weight.

The rest describe the respondent (`sexe`, `nivetud`, `occup`,
`freres.soeurs`), their views (`clso`, `relig`, `trav.imp`,
`trav.satisf`) and six more leisure activities (`sport`, `cuisine`,
`bricol`, `lecture.bd`, `peche.chasse`, `hard.rock`), plus `heures.tv`
and `id`.

## Source

The complete `hdv2003` data of the questionr package (Julien Barnier,
Francois Briatte and Joseph Larmarange), GPL (\>= 2) – with thanks. It
comes from the *Histoire de vie* survey run in 2003 by INSEE, the French
national statistics institute. tabxplor's copy changes only the level
order: every "Oui"/"Non" item reads "Oui" first, and `qualif` starts at
"Cadre".

## Examples

``` r
tab(questionr_hdv, qualif, cinema, pct = "row", na = "drop", color = "difference")
#> # A tabxplor tab: 8 × 4
#>   qualif                      Oui    Non        Total
#>                            <row%> <row%>   <row% (n)>
#> 1 Cadre                       65%    35% 100% (  260)
#> 2 Ouvrier specialise          22%    78% 100% (  203)
#> 3 Ouvrier qualifie            25%    75% 100% (  292)
#> 4 Technicien                  43%    57% 100% (   86)
#> 5 Profession intermediaire    46%    54% 100% (  160)
#> 6 Employe                     44%    56% 100% (  594)
#> 7 Autre                       47%    53% 100% (   58)
#> 8 Total                       42%    58% 100% (1 653)
#> # difference (Total): -30 -20 -10 -5 +5 +10 +20 +30
```
