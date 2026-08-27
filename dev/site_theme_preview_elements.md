<!-- The document elements a vignette or a course page is made of, in markdown, so the preview can
     show them as PANDOC renders them rather than as hand-written html. dev/site_theme_preview.R
     converts this file with `pandoc --mathml` and splices the result into the page.
     Edit freely: nothing here is code, and nothing depends on it. -->

## Document elements

Ordinary prose, so the body colour can be judged over several lines rather than in a caption. It
carries a [link](https://example.org), some **bold text**, some *italic*, a piece of
`inline_code()`, ~~a struck-out clause~~, a footnote[^1], and the odd 95^th^ ordinal or H~2~O.

[^1]: Footnotes land at the foot of the page, in their own section, at a smaller size.

### A third-level heading

> A block quote. In a course it usually carries a definition or a warning, so it has to read as
> set apart without shouting.

- A bullet list.
- With a second item, and `code` inside it.
  - A nested item, one level down.

1. A numbered list.
2. Whose numbers take the body colour.

Term
: A definition list — used for glossaries in the courses.

Another term
: Its definition, which can run to a second line without losing its indent.

- [ ] An unchecked task.
- [x] A checked one.

### Maths

Inline: the chi-squared statistic $\chi^2 = \sum (O - E)^2 / E$ sits in the run of text, and its
size has to match the prose around it.

Display, on its own line:

$$ OR = \frac{p_1 / (1 - p_1)}{p_0 / (1 - p_0)} \qquad \log(OR) = \beta_1 $$

A second one, with a matrix and a sum, to see how tall rows behave:

$$ \hat{\beta} = (X^{\top} X)^{-1} X^{\top} y, \qquad \bar{x} = \frac{1}{n}\sum_{i=1}^{n} x_i $$

### Code

A chunk as the vignettes set it (`collapse = TRUE`, `comment = "#>"`), so the printed output sits
in the same block as the code that made it:

```r
# a comment, at the top of the chunk
gss <- gss_cat_data_formatting()
tab(gss, race, party3, pct = "row", color = "difference") |>
  set_caption("Party by race")
#> # A tabxplor tab: 4 x 5
#>   race     Dem   Ind   Rep   Total
#>   <fct>  <row%> <row%> <row%> <row%>
#> 1 Other   38.9   35.1   26.0  100.0
```

And a second language, since a course also shows a formula or a shell line:

```bash
quarto render M1S1_02.qmd --to html
```

### A plain table

Not a tabxplor one: an ordinary markdown table, which the host's own table CSS styles.

| variable        | n     |     % | note              |
|:----------------|:------|------:|:------------------|
| Yes             | 1 204 |  40.2 | the modality      |
| No              | 1 789 |  59.8 | its complement    |
| **Total**       | 2 993 | 100.0 |                   |

### A figure

<figure>
<svg width="320" height="110" viewBox="0 0 320 110" role="img" aria-label="A small bar chart"
     style="max-width:100%;">
  <line x1="8" y1="96" x2="312" y2="96" stroke="currentColor" stroke-width="1" opacity=".5"/>
  <rect x="24"  y="34" width="46" height="62" fill="currentColor" opacity=".75"/>
  <rect x="96"  y="14" width="46" height="82" fill="currentColor" opacity=".55"/>
  <rect x="168" y="58" width="46" height="38" fill="currentColor" opacity=".35"/>
  <rect x="240" y="70" width="46" height="26" fill="currentColor" opacity=".2"/>
</svg>
<figcaption>A figure with its caption. The bars are drawn in <code>currentColor</code>, so this one
follows the page rather than fighting it.</figcaption>
</figure>

### A callout

Quarto's own `:::{.callout-note}` is styled by Quarto and does not exist on a pkgdown page. The
portable form, which both toolchains style because both ship bootstrap, is an alert:

<div class="alert alert-info" role="alert">
**Note.** An aside that must be seen, in the one shape a pkgdown page and a Quarto page agree on.
</div>

------------------------------------------------------------------------

A horizontal rule sits above this line, and this paragraph closes the section.

### Pandoc span annotations

The twelve annotation classes, as `[text]{.class}` spans. Group 1, the argument:
[the problematic]{.problematique}, [a result]{.resultat}, [a transition]{.structure}. Group 2,
theory and data: [Bourdieu (1979)]{.reference}, [a concept]{.concept}, [a case]{.terrain}. Group 3:
[a limit of the method]{.reflexivite}, [what is at stake]{.enjeu}. Group 4, the reading:
[apt]{.pertinent}, [too vague]{.preciser}, [wrong]{.non}, and [my own note]{.comment}.
