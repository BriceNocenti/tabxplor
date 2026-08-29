# Marijuana-possession arrests in Toronto, 1997-2002

5 226 people arrested for possession of a small quantity of marijuana.
The outcome is whether the person was **released with a summons** rather
than held. This is the running example of the *All else equal* article:
86 % of white arrestees were released against 74 % of black arrestees,
and the article asks what survives of that gap when people alike on
everything else are compared.

## Usage

``` r
car_arrests
```

## Format

A tibble of 5 226 rows and 8 columns.

- released:

  Released with a summons? "Yes" first – it is what the article studies.

- colour:

  The arrestee's race as the police recorded it: "White" first, then
  "Black".

- year:

  1997 to 2002.

- age:

  Age in years.

- sex:

  Female or Male.

- employed:

  Employed? "Yes" first.

- citizen:

  A Canadian citizen? "Yes" first.

- checks:

  On how many of six police databases the person's name already
  appeared, 0 to 6.

## Source

The complete `Arrests` data of the carData package (John Fox, Sanford
Weisberg and Brad Price), GPL (\>= 2) – with thanks; gathered by Michael
Friendly for a series in the *Toronto Star*. tabxplor's copy changes
only the level order: the yes/no items read "Yes" first, and `colour`
starts at "White".

## Examples

``` r
tab(car_arrests, colour, released, pct = "row", color = "difference")
#> # A tabxplor tab: 3 × 4
#>   colour    Yes     No        Total
#>          <row%> <row%>   <row% (n)>
#> 1 White     86%    14% 100% (3 938)
#> 2 Black     74%    26% 100% (1 288)
#> 3 Total     83%    17% 100% (5 226)
#> # difference (Total): -30 -20 -10 -5 +5 +10 +20 +30
```
