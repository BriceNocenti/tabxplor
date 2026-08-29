# tabxplor

`tabxplor` makes cross-tables and regression models readable at a glance
for data exploration. It builds a table with percentages, weighted
counts, confidence intervals, tests — and colors highlight the cells
that stand out from the total or reference, only when the difference is
statistically solid, to spot the structure of your data immediately.

- **Colors encode effect size and significance**: the stronger the
  difference, the deeper the color; non-significant cells are
  greyed-out.
- Html, Excel and markdown/Quarto exports are available.
- It comes with a point-and-click [jamovi](https://www.jamovi.org/)
  graphical interface: no code needed.
- A black-and-white `theme = "print_ready"` renders the same reading for
  journals.
- **Regression models** are presented with the same visual language,
  next to their observed effect.
- In R the tables **are `tibble`s you can keep working on with
  `dplyr`**. Cells are rich values, each one carries its count,
  percentage, confidence interval and reference behind the displayed
  number.
- Weighted data and survey design are supported.

## Installation

``` r

install.packages("tabxplor", dependencies = TRUE)
```

## A quick look

A simple cross-table with row percentages: shades of blue mean the cell
is over-represented compared to the total row, shades of yellow to red
mean it is under-represented.

``` r

gss <- gss_cat_data_formatting() # cleaned-up version of forcats::gss_cat

tab(gss, race, party3, pct = "row", color = "difference")
```

[TABLE]

Several column variables can be crossed at once for series of Yes/No
survey questions. With `color_signif = "grey_non_signif"`, cells that
are not significantly different from the total are greyed out, so every
colored figure is a solid one. Use `wt =` for weighted or survey data.
Example with [FactoMineR](http://factominer.free.fr/index_fr.md) tea
data :

``` r

tea_when_vars <- c("breakfast", "tea.time", "evening", "lunch", "dinner", "always")
tab(facto_tea, SPC, all_of(tea_when_vars), pct = "row", 
    levels = "first", na = "drop", 
    color = "difference", ref = "first", color_signif = "grey_non_signif")
```

[TABLE]

The same visual language extends to regression models:
[`tab_reg()`](https://bricenocenti.github.io/tabxplor/reference/tab_reg.md)
detects a binary outcome and fits a logistic regression, coloring odds
ratios by strength and greying the non-significant ones, with a default
comparison between the modelised deviations and their crude/observed
counterparts.

``` r

tab_reg(gss, outcome = "married", predictors = c("race", "age", "rincome"))
```

Logistic regression: married by race, age +1 more

[TABLE]

| outcome | numeric predictor | observed range | observed shape (central 95%) |
|----|----|----|----|
| p = %_(Married) ; log(p/(1-p)) | age | 13-57% (OR 8.7) | ![](data:image/svg+xml;base64,PHN2ZyBjbGFzcz0idHgtc3BhcmsiIHdpZHRoPSIxOTIuNiIgaGVpZ2h0PSI0NCIgdmlld2JveD0iMCAwIDE5Mi42IDQ0IiBhcmlhLWhpZGRlbj0idHJ1ZSI+PHBvbHlsaW5lIHBvaW50cz0iMS4zLDQyLjcgMTEuMywzMC45IDIxLjMsMTMuMSAzMS4zLDcuMiA0MS4zLDEuMyA1MS4zLDEuMyA2MS4zLDEuMyA3MS4zLDEuMyA4MS4zLDEuMyA5MS4zLDEuMyAxMDEuMywxLjMgMTExLjMsMS4zIDEyMS4zLDEuMyAxMzEuMywxLjMgMTQxLjMsMS4zIDE1MS4zLDEuMyAxNjEuMyw3LjIgMTcxLjMsNy4yIDE4MS4zLDEzLjEgMTkxLjMsMTMuMSIgZmlsbD0ibm9uZSIgc3Ryb2tlPSJjdXJyZW50Q29sb3IiIHN0cm9rZS13aWR0aD0iMi42IiBzdHJva2UtbGluZWpvaW49InJvdW5kIiBzdHJva2UtbGluZWNhcD0icm91bmQiPjwvcG9seWxpbmU+PC9zdmc+) |

Or as a black and white table ready for publication:

``` r

options(tabxplor.theme = "print_ready")
tab_reg(gss, outcome = "married", predictors = c("race", "age", "rincome"))
```

Logistic regression: married by race, age +1 more

[TABLE]

| outcome | numeric predictor | observed range | observed shape (central 95%) |
|----|----|----|----|
| p = %_(Married) ; log(p/(1-p)) | age | 13-57% (OR 8.7) | ![](data:image/svg+xml;base64,PHN2ZyBjbGFzcz0idHgtc3BhcmsiIHdpZHRoPSIxOTIuNiIgaGVpZ2h0PSI0NCIgdmlld2JveD0iMCAwIDE5Mi42IDQ0IiBhcmlhLWhpZGRlbj0idHJ1ZSI+PHBvbHlsaW5lIHBvaW50cz0iMS4zLDQyLjcgMTEuMywzMC45IDIxLjMsMTMuMSAzMS4zLDcuMiA0MS4zLDEuMyA1MS4zLDEuMyA2MS4zLDEuMyA3MS4zLDEuMyA4MS4zLDEuMyA5MS4zLDEuMyAxMDEuMywxLjMgMTExLjMsMS4zIDEyMS4zLDEuMyAxMzEuMywxLjMgMTQxLjMsMS4zIDE1MS4zLDEuMyAxNjEuMyw3LjIgMTcxLjMsNy4yIDE4MS4zLDEzLjEgMTkxLjMsMTMuMSIgZmlsbD0ibm9uZSIgc3Ryb2tlPSJjdXJyZW50Q29sb3IiIHN0cm9rZS13aWR0aD0iMi42IiBzdHJva2UtbGluZWpvaW49InJvdW5kIiBzdHJva2UtbGluZWNhcD0icm91bmQiPjwvcG9seWxpbmU+PC9zdmc+) |

## Export your tables

Any table exports with its colors to Excel, html or markdown (for Word,
copy-paste from Excel) :

``` r

tab(gss, marital, race, pct = "row", color = "difference") |> tab_html()
tab(gss, marital, race, pct = "row", color = "difference") |> tab_xl()
tab(gss, marital, race, pct = "row", color = "difference") |> tab_xl(theme = "print_ready")
```

## Learn more

- [Introduction to
  tabxplor](https://bricenocenti.github.io/tabxplor/articles/tabxplor.md)
  — the place to start (*aussi disponible [en
  français](https://bricenocenti.github.io/tabxplor/articles/tabxplor-fr.md)*).
- [Regression tables with
  tab_reg()](https://bricenocenti.github.io/tabxplor/articles/tabxplor-reg.md)
  (*aussi disponible [en
  français](https://bricenocenti.github.io/tabxplor/articles/tabxplor-reg-fr.md)*).
- [Reading a regression without losing sight of the
  percentages](https://bricenocenti.github.io/tabxplor/articles/tabxplor-reading-a-regression.md)
  — a single analysis walked from a first cross-table to a finished
  sentence (*aussi disponible [en
  français](https://bricenocenti.github.io/tabxplor/articles/tabxplor-reading-a-regression-fr.md)*).
- [Weighted and survey
  data](https://bricenocenti.github.io/tabxplor/articles/tabxplor-weights.md)
  — the three levels of margin of error, and which one your file
  deserves (*aussi disponible [en
  français](https://bricenocenti.github.io/tabxplor/articles/tabxplor-weights-fr.md)*).
- [Programming with
  tabxplor](https://bricenocenti.github.io/tabxplor/articles/tabxplor-programming.md)
  — many tables at once, custom workflows, options (*aussi disponible
  [en
  français](https://bricenocenti.github.io/tabxplor/articles/tabxplor-programming-fr.md)*).
