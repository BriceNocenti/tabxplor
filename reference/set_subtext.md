# The lines printed under a table

A table's footer text, as a **template**: everything tabxplor generates
is a `<placeholder>` and everything you write is a line, so the order of
the lines is the order of the footer. Re-order them and it re-orders;
drop `<legend>` and no colour legend is generated, in the console too.

A `subtext` naming **no** placeholder is simply appended to the template
— which is what a note has always done, so
`tab(subtext = "Field: GSS 2000")` is unchanged. Writing one placeholder
on a line of its own takes the layout over: only what you name is
printed. An unknown `<...>` is not a placeholder and passes through
verbatim (`\<` escapes a literal `<`).

The template a producer writes names only what **this** table can say:
no `<weight>` on an unweighted table, no `<model>` outside a regression.
What you read back is therefore what prints, and the way to drop a line
is to delete it.

A line opening on a short label and a colon (`"Field: GSS 2000"`) has
that label set in bold in every medium;
`options(tabxplor.subtext_bold_label = FALSE)` prints it as written.

## Usage

``` r
set_subtext(x, subtext)

get_subtext(x)
```

## Arguments

- x:

  A `tabxplor_tab`.

- subtext:

  A character vector, one element per line, or `NULL` to restore the
  default template. (There is no per-table way to print nothing at all:
  the exporters' `subtext = FALSE` is the one-off.)

## Value

`x`, with its footer template set (`set_subtext`) ; the template, as a
character vector (`get_subtext`).

## See also

[`tab_footer_text()`](https://bricenocenti.github.io/tabxplor/reference/tab_footer_text.md)
to see what the template prints,
[`set_legend_words()`](https://bricenocenti.github.io/tabxplor/reference/set_legend_words.md)
to re-word the generated legend,
[`set_footer_tabs()`](https://bricenocenti.github.io/tabxplor/reference/set_footer_tabs.md)
for a table or a note under the whole block.

## Examples

``` r
t <- tab(forcats::gss_cat, race, marital, pct = "row", color = "diff")
get_subtext(t)
#> [1] "<legend>" "<stars>" 

# your own sentence, with tabxplor's own pieces inside it
t <- set_subtext(t, c("<measure> (<ref>): <breaks>", "<stars>"))
cat(tab_footer_text(t), sep = "\n")
#> difference (Total): -30 -15 -5 +5 +15 +30
```
