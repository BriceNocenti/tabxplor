# Wrap column names and long labels

Wrap column names and long labels

## Usage

``` r
tab_wrap_text(
  tabs,
  wrap_rows = 35L,
  wrap_cols = 15L,
  exdent = 1,
  whitespace_only = TRUE,
  unbreakable_spaces = TRUE,
  brk = "\n"
)
```

## Arguments

- tabs:

  A `tabxplor_tab` or a `tibble` .

- wrap_rows:

  Row labels are wrapped past this width (35 by default), as prose — on
  whitespace.

- wrap_cols:

  Column NAMES are wrapped past this width (15 by default). A name is a
  compound word, not prose, so it breaks at the seams a name is built
  from (`_`, `.`, `*`, and a camelCase boundary) as well as at spaces.

- exdent:

  On the second lines or more, the number or characters to use for
  indentation.

- whitespace_only:

  Set to `FALSE` to wrap row labels also on non whitespace characters.

- unbreakable_spaces:

  Set to `FALSE` to keep normal spaces in text (auto-break).

- brk:

  The string to use for linebreak : `\n` in text, but `<br>` in html.

## Value

The same `tabxplor_tab` or `tibble`.

## Examples

``` r
# \donttest{
tab(forcats::gss_cat, race, marital, pct = "row", color = "difference") |>
  tab_wrap_text(wrap_rows = 5L, wrap_cols = 8L)
#> # A tabxplor tab: 4 × 8
#>   race   `No\nanswer` `Never\nmarried` `Separate\nd` Divorced Widowed Married
#>                <row%>           <row%>        <row%>   <row%>  <row%>  <row%>
#> 1 Other            0%              32%            6%      11%      4%     48%
#> 2 Black            0%              42%            6%      16%      8%     28%
#> 3 White            0%              21%            3%      16%      9%     51%
#> 4 Total            0%              25%            3%      16%      8%     47%
#> # ℹ 1 more variable: Total <row% (n)>
#> # difference (Total): -30 -15 -5 +5 +15 +30
# }
```
