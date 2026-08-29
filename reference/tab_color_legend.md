# Build the colour legend of a table

Internal. Returns one legend line per colour-signature group. For
`medium = "runs"` each line is a list of runs `list(text, color, bold)`;
otherwise a character string.

## Usage

``` r
tab_color_legend(
  x,
  medium = c("console", "html", "md", "runs", "plain"),
  style = NULL,
  lang = NULL,
  colored = TRUE,
  theme = NULL,
  classes = FALSE
)
```

## Arguments

- x:

  A `tabxplor_tab`.

- medium:

  One of "console", "html", "md", "runs", "plain". `"runs"` is for the
  media that draw the legend as coloured TEXT and cannot fill, such as
  an Excel rich-text cell
  ([`tab_xl`](https://bricenocenti.github.io/tabxplor/reference/tab_xl.md)).
  It returns the runs unrendered, and draws the background channel from
  the darker `bg_legend` palette (see
  [`set_color_palette`](https://bricenocenti.github.io/tabxplor/reference/set_color_palette.md)).

- style:

  "terse" (compact, console default) or "prose" (full sentences, export
  default).

- lang:

  NULL (auto from locale) / "en" / "fr".

- colored:

  Whether to colour the break-words.

- theme:

  Palette theme (default from options).

- classes:

  `medium = "html"` only: emit the break-words as CSS slot classes
  rather than inline hex, because a tabxplor stylesheet ships with the
  output
  ([`tab_html()`](https://bricenocenti.github.io/tabxplor/reference/tab_html.md)).
  Then the legend follows a theme toggle exactly like the cells it
  describes. `FALSE` (a table rendered without a stylesheet of ours)
  keeps inline hex.

## Value

A character vector (or, for "runs", a list of run-lists), or NULL when
nothing is coloured.
