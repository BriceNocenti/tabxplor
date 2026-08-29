# fct_recode helper to recode multiple variables

**\[deprecated\]**

Printed a ready-to-paste `mutate()` call recoding a set of factor
columns via
[`forcats::fct_recode()`](https://forcats.tidyverse.org/reference/fct_recode.html)
– unrelated to cross-tabulation, and unused elsewhere in tabxplor.
Removed in 2.1.0; copy it into your own project if you rely on it.

## Usage

``` r
fct_recode_helper(
  data,
  .cols = -where(is.numeric),
  name_in,
  name_out,
  freq = NULL,
  style = c("mutate", "base"),
  reminder = TRUE,
  cat = TRUE
)
```

## Arguments

- data:

  The data frame.

- .cols:

  \<[tidy-select](https://tidyr.tidyverse.org/reference/tidyr_tidy_select.html)\>
  The variables to recode.

- name_in:

  The input data frame's name (default: the expression given as `data`).

- name_out:

  The output data frame's name, if different from `name_in`.

- freq:

  Print each level's frequency and count as a comment; defaults to
  `TRUE` when 5 or fewer variables are given.

- style:

  `"mutate"` (default) writes a
  [`dplyr::mutate()`](https://dplyr.tidyverse.org/reference/mutate.html)
  call; `"base"` writes `data$var <-`.

- reminder:

  Print a `"new" = "old"` syntax reminder. Default `TRUE`.

- cat:

  Print to console, or open a temporary file when there are more than 5
  variables; `FALSE` returns a data frame of the recode text instead.

## Value

With `cat = TRUE` (default), the text printed to console (or written to
a temp R file for more than 5 variables), returned invisibly. With
`cat = FALSE`, a `tibble` of the recode text is returned instead. A
column carrying a `label` attribute is used as its comment title.
