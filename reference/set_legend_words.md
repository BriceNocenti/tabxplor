# Re-state what the colour legend calls a measure

The generated legend names a measure with the discipline's own words —
`color = "contrib"` says *contribution to Chi2*. A table may
legitimately grade the same ladder on another quantity: a factorial axis
has no chi-squared, and its cells' contribution is to the **variance of
the axis**. `set_legend_words()` re-states the words and changes nothing
else, so the swatches, the ladder, both registers, the publication
palettes, the plot guide and every medium keep working — the console
included, which no exporter argument can reach.

Prefer it to writing a legend of your own: replacing the sentence
(through
[`set_subtext`](https://bricenocenti.github.io/tabxplor/reference/set_subtext.md))
costs you the terse/prose pair and the publication palettes' wording.

## Usage

``` r
set_legend_words(x, ...)

get_legend_words(x)
```

## Arguments

- x:

  A `tabxplor_tab`.

- ...:

  One argument per measure (`difference`, `ratio`, `odds_ratio`,
  `contrib`, `adjustment`, `between_groups`), each either a single
  string (the measure's short word) or a named list of the fields below.
  `NULL` removes an override.

## Value

`x`, with its legend words set (`set_legend_words`) ; the named list of
them, or `NULL` (`get_legend_words`).

## Details

The fields a table may re-state — naming only, never a number:

- `word` — the short word (the console, a plot guide); `word_long` — the
  same named in full, for the export footers; `word_std` /
  `word_long_std` their SD-scale twins.

- `word_guar` — the `color_signif = "guaranteed_effect"` head, a
  template taking the confidence level
  (`"%s%%-guaranteed contribution"`).

- `subject` — the noun for what is graded, when it is not the cell
  itself.

- `ref` — the baseline noun (*the mean contribution*), for a measure
  compared to a **concept** rather than to a row of the table: the terse
  form brackets it with its preposition and the prose one points at it
  bare, both from this one field. Give `ref_word` / `ref_phrase` instead
  only where those two nouns genuinely differ. Re-stating any of them on
  a measure whose reference is a category or a total is refused — there
  the legend names what the table itself shows.

- `unit_word` — the unit the thresholds are counted in.

- `lead_over` / `lead_under` — the sentence each side of the ladder
  opens with, as a template taking `%1$s` the subject, `%2$s` the
  reference and `%3$s` the null value. **Only `%1$s` is always there**:
  a line names its baseline in full on its first side and not again on
  its second, so `%2$s` comes back empty there — where the baseline is
  the measure's own (a mean contribution, an axis), write it into the
  sentence rather than interpolate it.

- `caveat` — one sentence of honesty appended to the line.

An engine fact (`raw`, `scale`, `sig_source`, `bounds`) and a ladder
glyph (`break_over`, `threshold_mult`) are refused: a table attribute
must never change a number, and a column pulled out of its table must
still colour identically.

Written in the language of the call — unlike the words tabxplor
generates, which follow `lang =` at render.

## See also

[`set_subtext()`](https://bricenocenti.github.io/tabxplor/reference/set_subtext.md)
for the footer template,
[`set_color_breaks()`](https://bricenocenti.github.io/tabxplor/reference/set_color_palette.md)
for the ladder itself.

## Examples

``` r
t <- tab(forcats::gss_cat, race, marital, pct = "row", color = "contrib")
cat(tab_footer_text(t), sep = "\n")
#> Contribution to Chi2: cell over-represented vs independence, by ×1; ×2; ×5 the mean contribution; cell under-represented, by ×1; ×2; ×5 the mean contribution.

t <- set_legend_words(t, contrib = "contribution to the axis variance")
cat(tab_footer_text(t), sep = "\n")
#> Contribution to the axis variance: cell over-represented vs independence, by ×1; ×2; ×5 the mean contribution; cell under-represented, by ×1; ×2; ×5 the mean contribution.
```
