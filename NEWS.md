# tabxplor (development version)

## Bug corrections :
* `tab_kable()` : html tags in tables where no longer working and where appearing as text (`knitr::kable()` now needs a `format = "html"` argument)



# tabxplor 1.1.2

## Added : 
* `tab_kable()` : a `color_legend` argument have been added, to possibly remove the legend.

## Bug corrections :
* `tab_color_legend` had a mistake causing an error



# tabxplor 1.1.1

## Added : 
* `fct_recode_helper()` : helper function to recode multiple variables with `forcats::fct_recode`.
* `complete_partial_totals()` : complete partial total rows, total tables, and reference rows.

## Bug corrections :
* `tab_spread` : incomplete subtables led to partial total rows, total tables and reference rows.
* `tab_xl` : with `sheets = "unique"`, multiple empty sheets were created anyway
* `crayon()` error with colors in tabs printing on R 4.2.2
* color printing was not working with only one numeric `col_var`


# tabxplor 1.1.0 

## Added : 
* `tab_plain` have been separated in two functions, `tab_plain` for factors, `tab_num` for numeric variables
* `tab_plain` and `tab_num` have been rewrited in `data.table` to gain speed with big databases.


# tabxplor 1.0.3

## Added :
*  Remove rows with missing values or 0 in `wt` (weight), for them not to be added in counts (except in `tab_plain`)
*  `fmt_get_color_code()` : get the html color codes of a table as a character vector

## Bug corrections :
* `tab_many` : bug with totaltab when two numeric column variables (and a tabs_var)
* `tab_spread` not working with two `tab_vars`. Ok with a workaround, but would need to calculate one subtotal for each level of `spread_vars` in * `tab_totaltab` to fully work (and, then, to fully hierarchise total tables...). 
* `wt` argument procudes missing values with NA ; NA in weight variable are now automatically removed (excepted in `tab_plain`)
* Addition between `fmt` vectors wasn't working no more with percentages
* In `tab_plain`, `col_var` was not sorted anymore (`names_sort = TRUE` added in `pivot_wider`)
* `tab_color_legend()` was not working when some cols were colored and some not colored
* In `tab()` functions, correction was made to remove a R 4.1.2 `dplyr` warning message (data frame results in `filter()` are deprecated, use `if_any()` or `if_all()`). 


# tabxplor 1.0.2

## Added : 
* With `tab_kable`, option to use html `popover` instead of `tooltips`, to be able to use it in rmarkdown with a floating table of content.
* Two new 24 bits color styles for hmtl tables (`"blue_red"` and `"green_red"`).
* Possibility to provide a custom color palette for color styles, using `set_color_style()`. 
* `tab_core` was deprecated and renamed `tab_plain` for more clarity. Added options to render a table with normal numeric vectors instead of fmt, and to render a plain data.frame instead of a tibble. 
* Two way to print confidence intervals, using global option `"tabxplor.ci_print"` : `"moe"`, for margin of errors, prints as `12%±1.1` ; `"ci"` prints the interval `11·13%`.
* In `tab_kable`, confidence intervals of type `"cell` with print type `"moe"` appear in subscript. 
* In `tab_xl`, colors now are the same and works in the same way that `tab` and `tab_kable`.

## Bug corrections :
* With `tab` argument `color = "after_ci"`, when `diff` is negative, cells between 0 and -5% don't get colors.
* Problems in `tab_plain` with zero-rows dataframes 
* With `color = "contrib"`, no color when contribution is equal to the mean contribution (or a multiple of it).
* With `tab_kable`, white spaces are producing unwanted text wrapping (in the middle of numbers)
* In tabs and tooltips, `diff` not printing good with `type = "mean"`. 


# tabxplor 1.0.1
* Add possibility to export tables in html using `kableExtra`.
* Ensure functions do not write by default in the user's home filespace. 

## Bug corrections :
* Change color style not working in R CMD check : add possibility to change color style with global options. 
* Total rows appear even when not wanted in `tab` and `tab_many`.
* `tab_many` not working with `listed = "TRUE"` 


# tabxplor 1.0.0
* This is the first stable and published version of `tabxplor`.
