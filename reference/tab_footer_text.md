# Read the footer a table would print

The lines tabxplor prints under a table, resolved: the template's
placeholders built, the user's own lines as written. It is what
[`set_subtext()`](https://bricenocenti.github.io/tabxplor/reference/set_subtext.md)
edits, after the fact.

## Usage

``` r
tab_footer_text(
  x,
  medium = c("plain", "console", "html", "md"),
  style = NULL,
  lang = NULL,
  theme = NULL
)
```

## Arguments

- x:

  A `tabxplor_tab`.

- medium:

  One of `"plain"` (default), `"console"`, `"html"`, `"md"`.

- style:

  `"terse"` (the console's compact one-liner) or `"prose"` (full
  sentences, the exports' default); `NULL` follows the medium.

- lang:

  `NULL` (from `getOption("tabxplor.lang")`), `"en"` or `"fr"`.

- theme:

  Palette theme; `NULL` follows the medium's option.

## Value

A character vector, one element per footer line.

## See also

[`set_subtext()`](https://bricenocenti.github.io/tabxplor/reference/set_subtext.md)
for the template,
[`set_legend_words()`](https://bricenocenti.github.io/tabxplor/reference/set_legend_words.md)
for the words.

## Examples

``` r
t <- tab(forcats::gss_cat, race, marital, pct = "row", color = "diff")
cat(tab_footer_text(t), sep = "\n")
#> Percentage points (risk) difference: cell ≥ the Total row +5; +15; +30 points; cell ≤ the Total row -5; -15; -30 points.
```
