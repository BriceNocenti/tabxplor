# fct_recode helper to recode multiple variables

fct_recode helper to recode multiple variables

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

  The name of the input data frame. Default to the expression given in
  `data`.

- name_out:

  The name of the output data frame, if different from the input data
  frame.

- freq:

  Set to `TRUE` to print frequency and count of each level as comment.
  Set to `FALSE` to avoid this behavior. By default, frequencies and
  counts are only calculated when less than 6 variables are provided.

- style:

  Default is to use
  [`dplyr::mutate()`](https://dplyr.tidyverse.org/reference/mutate.html).
  Set to `base` to use `data$var <-` style.

- reminder:

  By default, a reminder of the syntax (`"new" = "old"`) is printed. Set
  to `FALSE` to remove it.

- cat:

  By default the result is written in the console if there are less than
  6 variables, written in a temporary file and opened otherwise. Set to
  false to get a data frame with a character variable instead.

## Value

When the number of variables is less than 5, a text in console as a side
effect. With more than 5 variables, a temporary R file. A `tibble` with
the recode text as a character variable is returned invisibly (or as
main result if `cat = TRUE`). When a column carries a variable label
(its `label` attribute), it is used as title in a comment.
