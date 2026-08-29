# Tea drinkers: when, where and why they drink it

A survey of 300 tea drinkers, used here for its three **batteries of
yes/no items** – the shape a multiple-answer question ("which of these
apply to you?") arrives in. Six items say *when* people drink tea, six
say *where*, and fourteen say what they think it does for them. See
[`vignette("tabxplor")`](https://bricenocenti.github.io/tabxplor/articles/tabxplor.md)
for what a battery does in a table, and
[`score_from_lv1()`](https://bricenocenti.github.io/tabxplor/reference/score_from_lv1.md)
for turning one into a single summed score.

## Usage

``` r
facto_tea
```

## Format

A tibble of 300 rows and 36 columns. The ones the vignettes use:

- breakfast, tea.time, evening, lunch, dinner, always:

  *When* do you drink tea?

- home, work, tearoom, friends, resto, pub:

  *Where* do you drink tea?

- Sport:

  Do you play a sport?

- SPC:

  Socio-professional category, 7 levels.

- sex:

  F or M.

The other columns describe the tea itself (`Tea`, `How`, `sugar`, `how`,
`where`, `price`, `frequency`), the drinker (`age`, `age_Q`), and what
they associate tea with (`healthy`, `relaxing`, `exciting`, `slimming`,
and ten more). Every two-level item reads "yes" first.

## Source

The complete `tea` data of the FactoMineR package (Francois Husson,
Julie Josse, Sebastien Le and Jeremy Mazet), GPL (\>= 2) – with thanks.
tabxplor's copy changes only the level order: in each yes/no item the
"yes" answer comes first, and its label loses the separator dot the
original spells it with (`"Not.tea time"` becomes `"Not tea time"`).

## Examples

``` r
tab(facto_tea, SPC, c(breakfast, evening), pct = "row", levels = "first", na = "drop")
#> # A tabxplor tab: 8 × 4
#>   SPC            n breakfast_lv evening_lv
#>                <n>       <row%>     <row%>
#> 1 employee      59          49%        44%
#> 2 middle        40          60%        30%
#> 3 non-worker    64          44%        20%
#> 4 other worker  20          40%        40%
#> 5 senior        35          63%        31%
#> 6 student       70          43%        44%
#> 7 workman       12          25%        17%
#> 8 Total        300          48%        34%
```
