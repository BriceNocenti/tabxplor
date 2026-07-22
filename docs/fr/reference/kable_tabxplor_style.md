# Print a tabxplor table in html

**\[deprecated\]**

Superseded by
[`tab_html()`](https://bricenocenti.github.io/tabxplor/fr/reference/tab_html.md),
which renders any table – `tabxplor_tab` or plain data.frame – through
the shared exporter prep. This function predates it and never shared its
machinery: it detects total rows/columns by matching the literal strings
`"Total"`/`"Ensemble"` against names and values, so it is hardcoded to
English and French, and it renders no colours, tooltips or spanning
headers. Nothing in the package has ever called it.

## Usage

``` r
kable_tabxplor_style(
  tabs,
  caption = knitr::opts_current$get("tab.cap"),
  theme = c("light", "dark"),
  total_in_bold = TRUE,
  all_column_borders = FALSE,
  html_font = NULL,
  full_width = FALSE,
  wrap_rows = 35,
  wrap_cols = 15,
  whitespace_only = TRUE,
  subtext = "",
  ...
)
```

## Arguments

- tabs:

  A data.frame.

- caption:

  The table caption. For formatting, you need to use a `css` with
  `caption{}`in rmarkdown.

- theme:

  By default, a white table with black text, Set to `"dark"` for a black
  table with white text.

- total_in_bold:

  Should rows and cols with "Total" string be set in bold ?

- all_column_borders:

  Put a vertical border around each column ?

- html_font:

  A string for HTML css font. By default, it uses
  `'"DejaVu Sans", "Arial", arial, helvetica, sans-serif'`. Set another
  default by setting `options("tabxplor.kable_html_font" = )`.

- full_width:

  A TRUE or FALSE variable controlling whether the HTML table should
  have the preferable format for full_width. If not specified, a HTML
  table will have full width by default but this option will be set to
  FALSE for a LaTeX table.

- wrap_rows:

  By default, rownames are wrapped when larger than 30 characters.

- wrap_cols:

  By default, colnames are wrapped when larger than 12 characters.

- whitespace_only:

  Set to `FALSE` to wrap also on non whitespace characters.

- subtext:

  A character vector to print rows of legend under the table.

- ...:

  Other arguments to pass to
  [`kableExtra::kable_styling`](https://rdrr.io/pkg/kableExtra/man/kable_styling.html).

## Value

A html table (opened in the viewer in RStudio). Differences from totals,
confidence intervals, contribution to variance, and unweighted counts,
are available in an html tooltip at cells hover.

## Examples

``` r
# \donttest{
tabs <- tibble::tibble(nm      = c("First", "Second", "Total"),
                       column1 = c(1, 2, 3),
                       column2 = c(4, 5, 6)                    )
if (requireNamespace("kableExtra", quietly = TRUE)) kable_tabxplor_style(tabs)
#> Warning: `kable_tabxplor_style()` was deprecated in tabxplor 1.4.0.
#> ℹ Please use `tab_html()` instead.
#> <style type="text/css">/* Kable tables*/
#>   .lightable-classic caption {
#>     text-align: left;
#>     font-weight: bold;
#>     font-size: 110%;
#>     color: black;
#>   }
#> 
#> .lightable-classic {
#>     border-top: 0 ;
#>     border-bottom: 0 ;
#> }
#> 
#> .lightable-classic tfoot {               /* footnotes */
#>   font-size: 80%;
#> }
#> 
#> /*textarea {
#>   -webkit-border-radius: 5px;
#>   -moz-border-radius: 5px;
#>   border-radius: 5px;
#> } */
#> 
#> 
#> /* Popover interactive tooltip for kable tables */
#> .popover {
#>   color: #ffffff;
#>     background-color: #000000;
#>     padding: 0;
#> }
#> 
#> .popover.left .arrow:after {
#>   border-left-color: black;
#> }
#> 
#> .popover-content {
#>   padding: 6px;
#> }
#> 
#> 
#> /* Reduce line spacing in kable() tables */
#> .lightable-classic table tbody tr td, /* .page-content */
#> .lightable-classic table thead tr th {
#>   padding: 2px 2px; /* 1) line spacing ; 2) line indentation*/
#>   border-bottom: 1px solid var(--border-color);
#>   border-top: none;
#>   /* text-align: left */
#> }
#> 
#> /* Break space into cell when wrapped */
#> /* table > tbody > tr > td > a {display: block;} */
#> /* td a {display: block;} */
#> /* .lightable-classic table tbody tr td a {
#>     display: block;
#>    vertical-align: top  !important;
#> } */
#> 
#> /*
#>  .lightable-classic table tbody tr td {
#>    vertical-align: top  !important;
#>    margin-bottom: -1rem !important;
#>  } */
#> 
#> /* headers, done in tab_kable inside cells (priority)
#> .lightable-classic table thead tr th {
#>    font-size: 80% !important;
#>    border-top: 0px solid ;
#>    border-bottom: 1px solid
#>   vertical-align: bottom  !important;
#> } */
#> 
#> 
#> 
#> 
#> 
#> </style>
#> <table class=" lightable-classic lightable-hover" style="font-family: &quot;DejaVu Sans&quot;, &quot;Arial&quot;, arial, helvetica, sans-serif; width: auto !important; margin-left: auto; margin-right: auto;">
#>  <thead>
#>   <tr>
#>    <th style="text-align:left;font-weight: bold;border-top: 0px solid ; border-bottom: 1px solid ;font-size: 90%;vertical-align: bottom;line-height: 0.9;padding: 3px;text-align: center;"> nm </th>
#>    <th style="text-align:right;font-weight: bold;border-top: 0px solid ; border-bottom: 1px solid ;font-size: 90%;vertical-align: bottom;line-height: 0.9;padding: 3px;text-align: center;"> column1 </th>
#>    <th style="text-align:right;font-weight: bold;border-top: 0px solid ; border-bottom: 1px solid ;font-size: 90%;vertical-align: bottom;line-height: 0.9;padding: 3px;text-align: center;"> column2 </th>
#>   </tr>
#>  </thead>
#> <tbody>
#>   <tr>
#>    <td style="text-align:left;min-width: 20; border-left:1px solid;border-right:1px solid;vertical-align: top; line-height: 0.85;padding: 3px;white-space: nowrap;"> First </td>
#>    <td style="text-align:right;vertical-align: top; line-height: 0.85;padding: 3px;white-space: nowrap;"> 1 </td>
#>    <td style="text-align:right;border-right:1px solid;vertical-align: top; line-height: 0.85;padding: 3px;white-space: nowrap;"> 4 </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;min-width: 20; border-left:1px solid;border-right:1px solid;vertical-align: top; line-height: 0.85;padding: 3px;white-space: nowrap;"> Second </td>
#>    <td style="text-align:right;vertical-align: top; line-height: 0.85;padding: 3px;white-space: nowrap;"> 2 </td>
#>    <td style="text-align:right;border-right:1px solid;vertical-align: top; line-height: 0.85;padding: 3px;white-space: nowrap;"> 5 </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;border-bottom: 1px solid ;min-width: 20; border-left:1px solid;border-right:1px solid;vertical-align: top; line-height: 0.85;padding: 3px;white-space: nowrap;font-weight: bold;border-top: 1px solid ; border-bottom: 1px solid ;"> Total </td>
#>    <td style="text-align:right;border-bottom: 1px solid ;vertical-align: top; line-height: 0.85;padding: 3px;white-space: nowrap;font-weight: bold;border-top: 1px solid ; border-bottom: 1px solid ;"> 3 </td>
#>    <td style="text-align:right;border-bottom: 1px solid ;border-right:1px solid;vertical-align: top; line-height: 0.85;padding: 3px;white-space: nowrap;font-weight: bold;border-top: 1px solid ; border-bottom: 1px solid ;"> 6 </td>
#>   </tr>
#> </tbody>
#> </table>
# }
```
