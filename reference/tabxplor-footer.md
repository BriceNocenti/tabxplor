# The lines under a table

Everything tabxplor prints beneath a table — the weight line, the
`Model:` line, the colour legend, the significance-stars key, and your
own notes — is **one template**, and the template is the table's
`subtext`.
[`get_subtext()`](https://bricenocenti.github.io/tabxplor/reference/set_subtext.md)
shows it,
[`set_subtext()`](https://bricenocenti.github.io/tabxplor/reference/set_subtext.md)
replaces it:

    t <- tab(forcats::gss_cat, race, marital, pct = "row", color = "diff")
    get_subtext(t)
    #> "<legend>" "<stars>"

Everything generated is a `<placeholder>`; everything you write is a
line; **the order of the lines is the order of the footer**. Re-order
them and it re-orders. Delete `<legend>` and no colour legend is
generated — in the console too, which no exporter argument can reach.

**A table names only what it can say.** The template above has no
`<weight>` because the table is unweighted and no `<model>` because it
is not a regression — both are settled when the table is built and
nothing can add them afterwards. `<legend>` and `<stars>` are always
named: they are built from the columns, which
[`set_color()`](https://bricenocenti.github.io/tabxplor/reference/fmt_attributes.md)
and the dplyr verbs can still change.

## The one rule

A `subtext` naming **no** placeholder is simply appended to the default
footer, which is what a note has always done. Writing one placeholder on
a line of its own takes the layout over: only what you name is printed.
An unknown `<...>` is not a placeholder — raw html, `"n < 30"` and
`"<30 ans>"` pass through verbatim and claim nothing (`\<` escapes a
literal `<`).

## What is built when

Every placeholder is resolved **at render**, so a footer follows the
`lang =`, the `theme =` and the medium of the call that prints it, and a
table edited after it was built (a
[`select()`](https://dplyr.tidyverse.org/reference/select.html), a
[`set_display()`](https://bricenocenti.github.io/tabxplor/reference/fmt_fields.md),
a
[`set_color_breaks()`](https://bricenocenti.github.io/tabxplor/reference/set_color_palette.md))
says the truth about what it now shows. Your own lines are frozen in the
language you wrote them — the default template holds no prose, so
nothing mixes unless you write it.

A table that has lost its attributes keeps what its **columns** can
still say — the colour legend and the stars key — and drops the rest.

## The placeholders

A line that IS one of these is built by tabxplor; a line that merely
contains one of the last three keeps its own words and has that piece
substituted into it. Anything else is your text, printed as written.

- `<weight>`:

  how the table was weighted, and what its intervals and tests rest on.

- `<model>`:

  a regression's family, outcome, predictors and estimand.

- `<interaction>`:

  the aggregated effect-modification test.

- `<legend>`:

  the colour legend: what each shade means. `<legend:terse>` /
  `<legend:prose>` pin the register. Change it with
  [`set_legend_words()`](https://bricenocenti.github.io/tabxplor/reference/set_legend_words.md).

- `<stars>`:

  what each significance star means.

- `<breaks>`:

  just the coloured ladder of a measure, inside a line of your own.
  `<breaks:over>` / `<breaks:under>` take one side; `<breaks:contrib>`
  names a measure where several compete.

- `<measure>`:

  what the colours grade, in the words
  [`set_legend_words`](https://bricenocenti.github.io/tabxplor/reference/set_legend_words.md)
  gives it. Change it with
  [`set_legend_words()`](https://bricenocenti.github.io/tabxplor/reference/set_legend_words.md).

- `<ref>`:

  the baseline it is graded against, as the compact form brackets it
  (preposition included); `<ref:noun>` gives the bare noun a sentence
  points at. Change it with
  [`set_legend_words()`](https://bricenocenti.github.io/tabxplor/reference/set_legend_words.md).

- `<method>`:

  how the intervals were computed (*Wilson score interval, 95%
  confidence*).

- `<cols>`:

  the names of the columns a measure describes.

- `<conf>`:

  the confidence level, localised.

## See also

[`set_subtext()`](https://bricenocenti.github.io/tabxplor/reference/set_subtext.md)
to edit the template,
[`tab_footer_text()`](https://bricenocenti.github.io/tabxplor/reference/tab_footer_text.md)
to read what it prints,
[`set_legend_words()`](https://bricenocenti.github.io/tabxplor/reference/set_legend_words.md)
to re-state what the legend calls a measure,
[`set_footer_tabs()`](https://bricenocenti.github.io/tabxplor/reference/set_footer_tabs.md)
and
[`tab_note()`](https://bricenocenti.github.io/tabxplor/reference/tab_note.md)
for a table or a note under the whole block,
[tabxplor-options](https://bricenocenti.github.io/tabxplor/reference/tabxplor-options.md)
for the session-wide defaults.
