# Footer legends and side tables

The design of everything `tabxplor` prints under a table: the colour legend, the weight and `Model:` lines, the significance-stars key, the user's own notes, the regression shape table and any subordinate table. It is the guide a package building `tabxplor_tab`s reads before writing a legend of its own, and the one `R/tab-footer.R` and `R/tab-legend.R` point up to.

`.Rbuildignore`'d.

---

## 1. The one idea

**The footer's TEXT is `subtext`, and `subtext` is a template.** `tab()` writes it, `get_subtext()` shows it, `set_subtext()` replaces it:

```r
t <- tab(d, x, y, color = "diff", subtext = "Champ : PCS 2020")
get_subtext(t)
#> "<legend>" "<stars>" "Champ : PCS 2020"
```

Everything tabxplor generates is a `<placeholder>`; everything a person writes is a line. **The order of the lines is the order of the footer**: re-order them and it re-orders, delete `<legend>` and no legend is generated — in the console too, which no per-call argument can do. Write your own sentence with `<breaks>` in it and the ladder is still generated from the same plan the cells are painted with.

**And it names only what THIS table can say.** There is no `<weight>` above because the table is unweighted, no `<model>` because it is not a regression. `set_subtext()` applies the same rule, so what comes back is always what prints, and dropping a line is deleting it.

What travels **under the whole table + footer block** — a subordinate table, a shape table, a note — is not text and is not in the template. It is triggered by the metadata that holds it, exactly as the test rows are.

Three things are settable, and they answer three different questions:

| knob       | question                                 | how                                                     |
|------------|------------------------------------------|---------------------------------------------------------|
| layout     | which lines are printed, in what order   | `subtext`, the template                                 |
| vocabulary | what the generated legend *calls* things | `set_legend_words()`                                    |
| content    | what each generated piece is made of     | the table's own columns and `meta`; `set_footer_tabs()` |

---

## 2. The template

### 2.1 The one rule

> A `subtext` containing **no known placeholder** gets tabxplor's default template *plus* those lines appended. The moment it contains one known placeholder, it **owns the layout**: only what it names is printed, in the order it names it.

That rule is what keeps `new_tab(subtext = "…")`, a raw `attr(x, "subtext") <- "…"`, `tab(subtext = "Champ : …")` on a hand-built table and jamovi's free-text option working unchanged — and it is why nobody can silently lose their legend by adding a note.

Unknown `<…>` is **not** a placeholder: `"<b>bold</b>"`, `"n < 30"`, `"<30 ans>"` pass through verbatim, silently, and never trigger the switch — raw html in a subtext behaves as it always did, and a typo prints itself instead of hiding a block. Only a token declared in `FOOTER_BLOCKS` counts, and the pattern is deliberately narrow (`<` then a lowercase letter, then `>`), so ordinary prose is never re-read as markup.

`color_legend = FALSE` on an exporter drops the legend wherever the template puts it — the per-call form of deleting `<legend>`, for the one-off case. There is no per-table switch beside it: the template *is* the per-table switch, and it is the only one the console can read.

### 2.2 The placeholders

Declared in `FOOTER_BLOCKS` (`R/tab-footer.R`), one row per member of the region. A row with a `token` is **text** and the template places it; a row without one is a **block**, placed by its declared position and triggered by the metadata that holds it. Row order is the default order.

| token                            | kind   | reads                                                                                       | named when                |
|----------------------------------|--------|---------------------------------------------------------------------------------------------|---------------------------|
| `<weight>`                       | line   | `meta$spec$vars$wt` or `reg_call()$wt`; col `basis` `conf_level` `display`                  | there is a weight         |
| `<model>`                        | line   | `meta$spec$call`                                                                            | it is a regression        |
| `<interaction>`                  | line   | `attr(x, "test")`                                                                           | an interaction was tested |
| `<legend>` `:terse` `:prose`     | line   | col `color` `color_signif` `scale` `ref` `col_var`; `meta$legend_words` `meta$color_breaks` | always                    |
| `<stars>`                        | line   | col `pvalue` `conf_level`; `options(tabxplor.stars)`                                        | always                    |
| `<breaks>` `:M` `:over` `:under` | inline | col `color`; `meta$color_breaks`                                                            | —                         |
| `<measure>` `:M`                 | inline | col `color`; `meta$legend_words`                                                            | —                         |
| `<ref>` `:M` `:noun`             | inline | col `color` `ref`; `meta$legend_words`                                                      | —                         |
| `<method>`                       | inline | col `ci_method` `conf_level` `degf`                                                         | —                         |
| `<cols>` `:M`                    | inline | col `col_var` `col_group`                                                                   | —                         |
| `<conf>`                         | inline | col `conf_level`                                                                            | —                         |
| —                                | note   | `meta$assumptions`; `options(tabxplor.shape_table)`                                         | —                         |
| —                                | tab    | `meta$footer_tabs`                                                                          | —                         |

An **inline** placeholder renders inside a line rather than as one, and is never pruned: it is substitution, not layout. `<breaks>`, `<measure>` and `<ref>` take the table's colour measure when it has a single one; `:M` names it where several compete. `<legend:terse>` / `<legend:prose>` pins the register on the table, which is why `options(tabxplor.legend_style)` needs no per-call argument.

**The generated terse legend is expressible in these pieces**, which is what says they are complete:

```r
set_subtext(t, "<measure> (<ref>): <breaks>")     # identical to <legend:terse>, in English
```

(`<ref>` carries the preposition, as the bracket does; `<ref:noun>` gives the bare noun a sentence points at. French writes `" : "` round its colon, so the literal above is an English line — that is exactly the kind of thing `set_legend_words()` spares you.)

### 2.3 Two gates, two questions: `reads` and `default`

**`reads` is the RENDER gate.** It is not decoration: it is checked at load — `zzz-fact-keys.R` requires every `meta$…` it names to be a `TAB_ATTRS` row and every column attribute to be a declared `fmt` attribute — and it **generates** the user-facing help, since `TAB_ATTRS` says which setter writes each attribute, so `?tabxplor-footer`'s "to change what this says, use…" column is derived rather than restated. It is what makes a stripped table degrade correctly (§4).

**`default` is the TEMPLATE gate** — a predicate the producers ask before writing a placeholder down. It answers a different question, and the split is what the two columns are for:

> A member built from `meta` is named only where its fact exists; one built from the **columns** is always named — because `set_color()` can colour an uncoloured table afterwards, and a placeholder the producers omit is one nothing brings back.

Hence the safety rule: **a predicate may over-name (the builder then prints nothing) but must never under-name.** And hence where the template is written: at each producer's **tail**, on the finished object. A regression's model record is attached by `set_reg_call()` *after* the table is assembled, so a template computed any earlier prunes `<model>` — and `<weight>` with it, since `tab_reg()` stores the weight in that record and not in `vars`.

Two consequences worth stating, because they are the whole extension story:

- **Adding a placeholder is one row** — a token, a kind, what it reads, when it is named, its builder.
- **Changing what an existing one says is changing one of the facts its row names.**

---

## 3. What is generated at render, and what is frozen

**Everything tabxplor generates stays render-time. Everything a person writes is frozen in the language they wrote it.** The generated half must stay live, and this is not a preference — four things break otherwise:

- **`lang =`** on `tab_md()` / `tab_html()` / `tab_xl()` builds the whole footer in the language asked for, whatever the session's.
- **The theme changes the text, not only the colour.** Under a publication palette `legend_break_tokens()` drops a rung whose rendering repeats the previous one (a shorter ladder), `legend_shade_names()` adds « Underlined: » / « Italic: » and splits one sentence into two, `tx_is_print()` swaps *Uncoloured* → *Unmarked*, and `print_marks` deletes the stars line entirely.
- **terse vs prose** is a per-medium register.
- **A post-build edit would make a frozen sentence lie.** `select()`, `filter()`, `set_display()`, `set_color()`, `set_color_signif()` and `set_color_breaks()` all change what the legend must say. This is why `set_display()` needs no legend hook: the legend is derived from the columns as they are at render.

⚠ It is also why jamovi keeps `theme` and `wrap_*` **out** of the crosstab cache key: they are render arguments, so an interactive theme change is a re-paint, not a rebuild. Freezing the footer would move them into the key.

**Nothing mixes two languages by default.** The template `tab()` stores holds only placeholders — no prose at all — so a table built in a French session and rendered with `lang = "en"` comes out entirely English. Mixing requires a person to write a line, in their language, and render in another: their sentence, their language, knowingly typed. There is no language stamp and no warning, because the default is safe by construction.

---

## 4. What a stripped table still says

A `tabxplor_tab` that loses its table attributes keeps printing and colouring, because the `fmt` columns carry everything the cells need. **The footer degrades the same way, and no code makes it do so.** `legend_specs()` reaches the columns through `tab_get_vars()$col_vars_levels`, which is derived entirely from the columns' own `col_var` attribute; every other legend fact — `color`, `scale`, `ref`, `conf_level`, `ci_method`, `basis`, `pvalue` — is a column attribute too.

> A table with no stored template gets the default one — the **whole** one, `default` gates only what the producers write down — and **each row is then gated on what it `reads`**, so a row whose facts are gone produces nothing. A stripped table therefore keeps the column-derived half of its footer — the colour legend and the stars key — and silently drops the table-derived half: `<weight>`, `<model>`, `<interaction>`, the shape table and the subordinate tables.

That is the gate every row has anyway: no `tryCatch`, no `if (is.null(meta))` branch, no fallback path to maintain. What is genuinely lost is a *customised* template and the user's own lines, which are table-level by nature and unrecoverable by anything.

⚠ **Never write code that depends on the weight or `Model:` line existing.**

---

## 5. Placement: three kinds, five media

| kind          | console                                       | html / md / xl             |
|---------------|-----------------------------------------------|----------------------------|
| `line`        | under the table, `#` subtle (pillar's footer) | under the table            |
| `note`, `tab` | **above** the table                           | **below** the footer lines |

The console rule is that **the last thing printed is the R object you can go on to pipe**, so every pipe table travels above it. In an export nothing is "the result", so they read below. One declared rule, no per-medium code.

Every emitter already existed before the region was declared:

| kind   | console                         | md                     | html          | xl          |
|--------|---------------------------------|------------------------|---------------|-------------|
| `line` | `legend_render_line("console")` | `…("md")`              | `…("html")`   | `…("runs")` |
| `note` | the pipe-table renderer         | the same               | `note_html()` | `note_xl()` |
| `tab`  | `tab_pipe()`                    | the `list_method` path | the same      | the same    |

**The generated blocks belong to the HOST.** A subordinate is not a peer: it renders its own carried lines and nothing generated, so the colour legend, the weight line and the stars key are emitted exactly once. `caption =` likewise applies to the first table only.

**A subordinate inherits its host's render options, with no opt-out** — `var_names`, `theme`, `color`, `wrap_*`, and in the console the options those default to. That is what makes the five media agree by construction. ⚠ A subordinate whose readability depends on a span row must therefore name its columns in full: a host asking for `var_names = "rows"` drops the span everywhere.

---

## 6. The vocabulary

The one thing a template cannot do is **re-word** tabxplor's grammar rather than replace it. `set_legend_words()` writes `meta$legend_words[[measure]]`, folded by `measure_facts()` as a fourth layer, after `guar` (the significance policy) and `by_scale` (the ladder). A bare string is the `word`:

```r
tabs |> set_legend_words(contrib = "contribution à la variance de l'axe")

tabs |> set_legend_words(contrib = list(          # the full form
  word      = "contribution à la variance de l'axe",
  ref       = "la contribution moyenne",
  lead_over = "%1$s contribue plus que la moyenne à l'axe, de"))
```

Everything else keeps working untouched, because only the word changed: the swatches, the palette slots, the ladder, terse and prose, `lang`, the four media, the plot guide — and the console.

Two rules keep it safe:

- **A declared whitelist**, `MEASURE_WORD_FIELDS`, checked when the words are set: `word`, `word_long`, `word_std`, `word_long_std`, `word_guar`, `subject`, `ref`, `ref_word`, `ref_phrase`, `unit_word`, `lead_over`, `lead_under`, `caveat`. ⚠ **Never** an engine fact (`raw`, `scale`, `sig_source`, `bounds`, `gate_row`) nor a ladder glyph (`break_over`, `threshold_mult`, `break_scale`): a table attribute must never change a number, and an extracted `fmt` column must colour identically on its own.
- **`ref` is the baseline, once.** It feeds the two shapes a sentence needs — the terse form brackets it *with a preposition tabxplor still translates at render* (`vs la contribution moyenne` / `p. r. à la contribution moyenne`), the prose lead points at the bare noun — so stating it **deletes** the measure's own pair. `ref_word` / `ref_phrase` remain for the case where the two nouns genuinely differ, which is tabxplor's own `contrib`: terse says *vs the mean*, prose says *independence*.
- ⚠ **A baseline word only exists where the baseline is a CONCEPT.** Every other measure names the reference the table itself shows — a level label, the Total row — so `ref` / `ref_word` / `ref_phrase` are **refused** there rather than stored and ignored (which is what they used to be). Today that means `contrib`; `measure_ref_worded()` is the live list.
- ⚠ **Words are data, never closures.** A closure in `meta` captures a namespace and breaks `saveRDS()`. A sentence template carries `%1$s` the subject, `%2$s` the reference and `%3$s` the null. ⚠ Only `%1$s` is always there: a line names its baseline in full on its first side and not again on its second, so where the baseline is the measure's own, write it into the sentence rather than interpolate it.
- ⚠ The invariant is stated as what the list must **not** contain, beside `MEASURES` and not in `zzz-fact-keys.R`: the whitelist is not a subset of the fields `MEASURES` declares (`lead_over` exists only as an override, `word_std` only on a `by_scale` row), so the check is that no engine fact and no ladder glyph is in it.

It is keyed on the **measure**, not on a column: that is what the sentence is about, it survives `tab_wrap_text()` (which renames columns, so anything keyed by a column name silently stops matching), it survives a `dplyr` pipeline, and it needs no reconcile rule on a bind.

---

## 7. Side tables and notes

`set_footer_tabs(x, tabs)` carries what belongs to the table without being a row of it. A `tabxplor_tab` renders as a **table** — `fmt` cells, colours, its own Excel geometry; **any other data.frame renders as a note** — already-rendered character columns, in the aside ink. A named element becomes its caption, through `set_caption()` rather than a second titling mechanism.

`tab_note(df, headers =, align =, grey =, note =, kind =)` exists only to override what a bare data.frame cannot say: translated headers, right alignment, a greyed row, a footnote line, a per-column render kind (which is how the regression's sparkline column is declared). `reg_shape_table()` is one of its producers, and nothing about it is regression-specific any more.

⚠ A subordinate is handed down **stripped of every field `TAB_ATTRS` marks as not surviving** (`tx_strip_subordinate()`): its own `footer_tabs`, which would recurse, and its `assumptions`, which would generate a second shape note under a table that generates nothing. One declared rule instead of a recursion guard.

---

## 8. `TAB_ATTRS` — the table-attribute grid

One row per table attribute — `subtext`, `test`, and each `meta` field — with four columns, each the single source of something previously hand-kept:

| column        | what reads it                                                                    |
|---------------|----------------------------------------------------------------------------------|
| `gloss`       | `?new_tab`'s `@param meta` list, generated via `@eval`                           |
| `bind`        | `tab_meta_bind()` — the per-field merge rule                                     |
| `subordinate` | whether the field survives into a footer tab's copy                              |
| `setter`      | `?tabxplor-footer`'s "how to change this", derived through `FOOTER_BLOCKS$reads` |

`tab_attrs()`, the list the ~30 dplyr methods carry, still names its three by hand: `meta` is the *container* of the rest, so the three are not peers there. A build-time assertion refuses a row that does not declare `where` / `subordinate` / `gloss` — the `fmt_attr_rules` precedent.

---

## 9. The whole thing, as a consumer writes it

`ggfacto::mca_interpret()` is the case the framework was designed against. Here is its footer before and after, in full — every line runnable, the output below each block copied from a real render.

### 9.1 Before

```r
# --- what ggfacto writes today -------------------------------------------------------------------
gda_poles_legend <- function(complete = FALSE, color = TRUE) {
  ladder <- paste0("×", tabxplor::get_color_breaks()[["contrib"]], collapse = " ")
  ctr <- if (color)
    gettextf("contrib : contribution à la variance de l'axe, vs la contribution moyenne : %s",
             ladder)
  else gettext("contrib : contribution à la variance de l'axe")
  if (!complete) return(ctr)
  c(ctr, gettext("coord : coordonnée sur l'axe"), gettext("cos2 : qualité de représentation"))
}

gda_summary <- function(tabs, eig = NULL, legend = character(), var_names = NULL) {
  if (length(legend)) attr(tabs, "subtext") <- legend          # a raw attribute write
  if (!is.null(eig))  tabs <- tabxplor::set_footer_tabs(tabs, list(eig))
  attr(tabs, "ggfacto_render") <- list(var_names = var_names)
  class(tabs) <- unique(c("ggfacto_summary", class(tabs)))
  tabs
}

gda_render <- function(x, format, ...) {                        # and every render must suppress
  args <- utils::modifyList(list(color_legend = FALSE, var_names = "rows"), rlang::list2(...))
  if (identical(format, "md")) do.call(tabxplor::tab_md, c(list(x, css = FALSE), args))
  else                         do.call(tabxplor::tab_html, c(list(x, tooltips = FALSE), args))
}
```

Four things are paid for it: the ladder is **pasted** (`get_color_breaks()` in prose, so it drifts the day a break moves), the sentence is **one register** for every medium, it is **frozen at build** in the session's language, and in the **console** — where `color_legend` does not reach — tabxplor's own line still prints above it:

```text
Contribution to Chi2: cell over-represented vs independence, by x1; x2; x5; x10 the mean
  contribution; cell under-represented, by x1; x2; x5; x10 the mean contribution.
contrib : contribution a la variance de l'axe, vs la contribution moyenne : x1 x2 x5 x10
coord : coordonnee sur l'axe
cos2 : qualite de representation
```

### 9.2 After — the two lines that do it

Keep `<legend>` and re-state the two nouns. Nothing else changes: the swatches, the ladder, both registers, the publication palettes, the plot guide and the console all say ggfacto's words.

```r
tabs |>
  set_legend_words(contrib = list(word = "contribution à la variance de l'axe",
                                  ref  = "la contribution moyenne"))
```

```text
# console (terse)
contribution a la variance de l'axe (vs la contribution moyenne): x10 x5 x2 x1 x1 x2 x5 x10
```

That terse line **is** `gda_poles_legend()`'s, generated — coloured, and rebuilt whenever the breaks or the theme change.

### 9.3 After — the whole sentence, for the exports

The prose form has more slots, because it is a sentence rather than a label. Each slot is one field, and the fields below are written **in the order the sentence says them**:

```r
tabs |>
  set_legend_words(contrib = list(
    # read top to bottom, this IS the sentence
    word       = "contribution à la variance de l'axe",             # opens the line
    subject    = "un niveau",                                       # ...then, on each side:
    lead_over  = "%1$s contribue plus que la moyenne à l'axe, de",  #    the over side's verb
    lead_under = "%1$s contribue moins que la moyenne à l'axe, de", #    the under side's
    unit_word  = "la contribution moyenne",                         # ...and what closes each side
    ref        = "la contribution moyenne"))                        # the baseline, for the TERSE form
```

The **terse** line, which is what the console prints, uses three of them:

```text
contribution a la variance de l'axe           word
 (vs la contribution moyenne)                 ref, bracketed -- the "vs" is tabxplor's, translated
: x10 x5 x2 x1  x1 x2 x5 x10                  the ladder -- generated, both sides, never re-stated
```

The **prose** line, which is what every export prints, walks the same words through a sentence:

```text
Contribution a la variance de l'axe           word, capitalised: it opens the line
:
  un niveau                                   subject
  contribue plus que la moyenne a l'axe, de   lead_over
  x1; x2; x5; x10                             the ladder, over side
  la contribution moyenne                     unit_word
;
  un niveau                                   subject, again
  contribue moins que la moyenne a l'axe, de  lead_under
  x1; x2; x5; x10                             the ladder, under side
  la contribution moyenne                     unit_word
.
```

`ref` appears twice above because it is one noun in two positions: bracketed after the measure in the terse line, and pointed at bare by a prose lead (`%2$s`, which these leads do not use because they name the baseline themselves). State `ref_word` / `ref_phrase` separately only where the two nouns genuinely differ.

⚠ A lead template takes `%1$s` the subject, `%2$s` the reference and `%3$s` the null — but **only `%1$s` is always there**: a line names its baseline in full on its first side and not again on its second, so `%2$s` comes back empty there. Where the baseline is the measure's own, write it into the sentence, as above.

### 9.4 After — the rest of the footer

```r
gda_summary <- function(tabs, eig = NULL, glossary = character(), var_names = NULL) {
  tabs |>
    set_legend_words(contrib = list(word = "contribution à la variance de l'axe",
                                    ref  = "la contribution moyenne")) |>
    set_subtext(glossary) |>                       # "coord : ...", "cos2 : ..." -- APPENDED,
    set_footer_tabs(list("Valeurs propres" = eig)) #  since they name no placeholder
}

gda_render <- function(x, format, ...) {           # nothing to suppress any more
  args <- utils::modifyList(list(var_names = "rows"), rlang::list2(...))
  if (identical(format, "md")) do.call(tabxplor::tab_md, c(list(x, css = FALSE), args))
  else                         do.call(tabxplor::tab_html, c(list(x, tooltips = FALSE), args))
}
```

```text
# console: the eigenvalues ABOVE (the last thing printed is the object you can pipe), then the table,
# then the footer
| Axe |% variance  |
|:----|-----------:|
|     | *Variance* |
|     |  *<col%>*  |
| 1   |      9.9%  |
| 2   |      7.2%  |

: Valeurs propres

# A tabxplor tab: 5 x 6
  Axe     Variable     n contrib  coord   cos2
...
# contribution a la variance de l'axe (vs la contribution moyenne): x10 x5 x2 x1 x1 x2 x5 x10
# coord : coordonnee sur l'axe
# cos2 : qualite de representation
```

### 9.5 If the generated grammar is still wrong for you

Only then: write the sentence yourself and drop tabxplor's pieces into it. Naming one block placeholder takes the layout over, so name every line you want.

```r
tabs |> set_subtext(c(
  "contrib : <measure>, <ref> : <breaks>",
  "coord : coordonnée sur l'axe",
  "<stars>"))
```

Every piece is still generated from the plan the cells are painted with, so nothing can drift: `<breaks>` is the ladder, `<measure>` and `<ref>` are the nouns `set_legend_words()` gave them, `<method>` is the interval. What you lose against 9.2 is the terse/prose pair, the publication palettes' direction words and the plot guide. ⚠ Never paste `get_color_breaks()` into prose, and never write html into `subtext` — it reaches a `.md` file and an Excel cell as raw markup.

### 9.6 What disappears from the consumer

| gone | why |
|---|---|
| `gda_poles_legend()` | the legend is generated, in ggfacto's words |
| `color_legend = FALSE`, in every render path | nothing to suppress |
| `attr(tabs, "subtext") <- ...` | `set_subtext()` is exported |
| the pasted `get_color_breaks()` ladder | `<breaks>`, or the generated legend, builds it |
| the html `<span class="p1">` swatches ggfacto had to delete | the generated legend carries them, in every medium |

⚠ **`lang =` still has to exist in ggfacto**, for a reason tabxplor cannot fix: its axis headings and its summary-row labels are **factor levels**, built at construction. Only prose can be rendered in a language chosen later; a level is data.

---

## 10. For a package building on tabxplor

**The footer's text is `subtext`, and `subtext` is a template `tab()` writes for you** — naming only what that table can actually say. `get_subtext(x)` shows it; `tab_footer_text(x)` shows what it prints. Re-order the lines and the footer re-orders; delete `<legend>` and none is generated, console included. A `subtext` with no placeholder at all is simply appended to the template, as always, and `set_subtext()` hands the template back so the next edit starts from what prints.

- **Your measure has a different name.** Keep `color = "<measure>"` on your `fmt()` columns, keep `<legend>` in the template, and re-state the words: `set_legend_words(x, contrib = "contribution à la variance de l'axe")`, adding `ref =` for the baseline. You get tabxplor's own legend saying your nouns, in every medium and both registers. **This is the preferred route** — do not suppress the legend and write your own.
- **You need your own sentence.** Write it, and drop tabxplor's pieces into it: `set_subtext(x, c("contrib : <measure>, <ref> : <breaks>", "<stars>"))`. The pieces are listed in §2.2; every one is generated, so none can drift, and `"<measure> (<ref>): <breaks>"` **is** the generated terse line. ⚠ Never paste `get_color_breaks()` into prose.
- **You build your tables yourself** (`new_tab()`, not `tab()`), so they name no placeholder until you write one — everything still prints, through the no-template fallback of §4. `set_subtext()` is what gives you a template: pass your own lines and it hands back the placeholders plus them, which is then the document you edit.
- **A baseline word only exists where the baseline is a concept** (`contrib`). On a measure whose reference is a row of the table — a level, the Total — `ref` / `ref_word` / `ref_phrase` are refused, because there the legend names what the table itself shows.
- **A table or a note under the whole block.** `set_footer_tabs(x, list("Valeurs propres" = eig))` — a `tabxplor_tab` renders as a table, any other data.frame as a grey note. They are not in the template: carrying them is what prints them.
- **A subordinate inherits the host's render options.** Do not rely on a span row surviving a `var_names = "rows"` host; name the columns in full.
- **Your function returns ONE table**, never a list. The subordinate travels inside it, so the caller keeps something it can pipe and filter.
- **Your own prose is frozen in the language you wrote it**; every `<placeholder>` follows `lang =` at render. The default template holds no prose, so nothing mixes unless you write it.
- **If a table loses its attributes**, the footer keeps what the columns can still say — the colour legend and the stars key — and drops the rest.
- **Never write HTML into `subtext`.** It reaches a `.md` file and an Excel cell as raw markup. `<breaks>` is the supported way to get swatches.

Nothing is registered and nothing is global: it is all data on the object, so it survives a `dplyr` pipeline and a `saveRDS()`.
