# Get HTML Color Code of a fmt vector

Get HTML Color Code of a fmt vector

## Usage

``` r
fmt_get_color_code(x, type = "text", theme = "light", ...)
```

## Arguments

- x:

  The fmt vector to get the html color codes from.

- type:

  The style type, `"text"` to color the text, `"bg"` to color the
  background.

- theme:

  Is your console or html table background `"light"` or `"dark"` ?
  Default to the current setting (RStudio theme when detectable, else
  `"light"`).

- ...:

  Absorbs deprecated arguments (e.g. `html_24_bit`); ignored.

## Value

A character vector with html color codes, of the length of the initial
vector.

## Examples

``` r
# \donttest{
tabs <- tab(forcats::gss_cat, race, marital, pct = "row", color = "diff")
dplyr::mutate(tabs, across(where(is_fmt), fmt_get_color_code))
#> # A tabxplor tab: 4 × 8
#>   race  `No answer` `Never married` Separated Divorced Widowed Married Total
#>   <fct> <chr>       <chr>           <chr>     <chr>    <chr>   <chr>   <chr>
#> 1 Other NA          #02A5B3         NA        NA       NA      NA      NA   
#> 2 Black NA          #0891C9         NA        NA       NA      #DE7C01 NA   
#> 3 White NA          NA              NA        NA       NA      NA      NA   
#> 4 Total NA          NA              NA        NA       NA      NA      NA   
# }
```
