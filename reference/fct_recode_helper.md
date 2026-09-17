# Write the code to recode several factors

Recoding a factor with
[`forcats::fct_recode()`](https://forcats.tidyverse.org/reference/fct_recode.html)
means typing every level name exactly, and a typo is silently ignored.
`fct_recode_helper()` writes that code for you: it prints a
ready-to-paste `mutate()` call with one `fct_recode()` per variable,
each level already written as `"level" = "level"`. You then only edit
the new names on the left, and delete the lines you keep.

With a few variables, each level carries its frequency and count as a
comment, which is what tells you which small levels to merge. A column
with a `label` attribute (data imported by haven) gets that label as a
comment title.

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
  The variables to recode. Default: every non-numeric column.

- name_in:

  The input data frame's name (default: the expression given as `data`).

- name_out:

  The output data frame's name, if different from `name_in` (used by
  `style = "base"`).

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
`cat = FALSE`, a `tibble` of the recode text.

## Examples

``` r
fct_recode_helper(forcats::gss_cat, c(marital, race))
#> forcats::gss_cat |>
#> mutate(
#>  marital = fct_recode(   # "new" = "old" 
#> marital,
#> "No answer"     = "No answer"    , #  0%     17
#> "Never married" = "Never married", # 25%  5 416
#> "Separated"     = "Separated"    , #  3%    743
#> "Divorced"      = "Divorced"     , # 16%  3 383
#> "Widowed"       = "Widowed"      , #  8%  1 807
#> "Married"       = "Married"      , # 47% 10 117
#> ),
#> 
#>  race = fct_recode(   # "new" = "old" 
#> race,
#> "Other" = "Other", #  9%  1 959
#> "Black" = "Black", # 15%  3 129
#> "White" = "White", # 76% 16 395
#> ),
#> 
#>  )
```
