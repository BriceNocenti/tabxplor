# Footer legends and side tables, as a framework — a draft

⚠ **This is a draft, not a design.** It states a problem another package ran into, inventories what
`tabxplor` offers today, and sketches two shapes a solution could take. **Nothing here is settled**,
and none of it should be built from this file as it stands: the real design has to be made inside
`tabxplor`'s own framework — the declarative tables (`TAB_OPTIONS`, `DISPLAY_TOKENS`, `EST_SCALES`,
`materialize_specs`), the one-builder/one-renderer footer, and the rule that a fact is stated once.
Written in phase 6, at the maintainer's request, after `ggfacto` hit both walls.

`.Rbuildignore`'d.

---

## 1. The use case, and its constraints

`ggfacto::mca_interpret()`, `ca_interpret()` and `pca_interpret()` summarise a geometric data
analysis as a `tabxplor` table. They are ordinary consumers — `fmt()` columns, `new_tab()`, the
exporters — with two needs the package cannot serve.

### 1.1 A foreign package cannot write the colour legend it needs

Their colour measure is `contrib`, so the generated legend says **« contribution to Chi2 (vs the
mean) »**. A factorial axis has no chi-squared: the number is a contribution to the **variance of the
axis**, and the ×1 / ×2 / ×5 / ×10 ladder is Le Roux and Rouanet's threshold, not a residual scale.
The sentence is wrong in the one place a reader looks to find out what the colour means.

What `ggfacto` needs is to **replace the measure's word**, keeping everything else: the swatches,
their palette slots, the terse/prose forms, the translation at render, the per-medium rendering.

What it can do today is **suppress the whole legend** and write plain text into `subtext`. That costs
four things at once:

| what is lost | why |
|---|---|
| the colour swatches | `subtext` is a plain token (`.lg_tok`), never a coloured break token |
| translation at render | `subtext` is fixed when the table is BUILT, so `lang =` cannot reach it |
| terse vs prose | one string, whatever the medium asks for |
| the console | `color_legend` is per-call on the exporters, and `tbl_format_footer()` never passes it |

⚠ The last one is the sharpest: **in the console there is no way to suppress the generated legend at
all**, short of having no colours — so the wrong sentence and its replacement print one after the
other.

### 1.2 A foreign package cannot attach a note, only a table

Phase 5 gave `meta$footer_tabs` — subordinate `tabxplor_tab`s rendered under the host in all four
media — and phase 6 made the console print them as pipe tables (`tab_pipe()`). That covers a
**table**: `fmt` cells, colours, an Excel sheet.

It does not cover a **note**: already-rendered character columns, grey, headers of its own, a
footnote line. That is what `reg_shape_table()` is, and it needed **one hand-written emitter per
medium** — `shape_render_console()`, `shape_html_table()`, the `tab_md()` branch, `xl_shape_cells()`
— plus a gate (`tab_wants_shape_table()`) hard-wired to `tab_is_reg(tab) && get_assumptions(tab)`.

So there are two mechanisms for one idea, one of them closed to everybody but the regression
subsystem.

---

## 2. What exists today

### 2.1 The footer: one builder, one renderer, five hard-coded streams

`tab_footer_streams()` (`R/fmt_class.R`) pushes five streams in a fixed order — weight, `Model:`,
interaction, colour legend, stars, user `subtext` — each `list(tokens =, role =)`. `render_footer()`
renders them per medium, and reads `role` for exactly one thing: the console's grey `# ` prefix.

Every medium goes through `rd_footer()` except the console, which calls the pair directly.

**The open seams a foreign package has:** `subtext` (append-only, last, unescaped, fixed at build),
`set_caption()`, `set_footer_tabs()`, `set_color_breaks()`. That is all.

**What is closed:** no S3 generic, no registry, no `meta` field for the legend, `color_legend` with
no option and no per-table equivalent, `legend_style` and `shape_table` global-only.

### 2.2 The side tables: one open, one closed

| | `meta$footer_tabs` (phase 5) | `meta$assumptions` → `reg_shape_table()` |
|---|---|---|
| what it holds | `tabxplor_tab`s | rendered character columns |
| how it renders | `tx_with_footer_tabs()` expands into the exporters' own list | four hand-written emitters |
| who may write it | anybody, `set_footer_tabs()` is exported | the regression pipeline only |
| Excel | a sheet | bespoke cells |
| console | a pipe table (`tab_pipe()`) | a pipe table (`tx_pipe_table()`) |

The two already agree on the console's shape. They disagree on everything else, for one reason: one
was designed as an extension point and the other as a feature.

---

## 3. Two shapes a solution could take

**Neither is proposed. Both are sketches**, put side by side so the real design has something to
argue against.

### Shape A — a stream registry, keyed on `role`

`role` already exists, is already a per-stream string, and is already consumed generically. The idea
is to let a producer register a stream:

```r
# NOT A PROPOSAL -- a sketch
tab_register_footer(role = "gda_legend", after = "reg", build = function(tab, lang, style, theme) ...)
```

`tab_footer_streams()` would walk the registry instead of naming five `push()` calls, and a
registered `build` would return a token stream — so a foreign legend could use `.lg_ctok()` and get
real swatches, be built at render (hence translated, hence terse/prose aware), and reach every
medium including the console.

- **For**: the smallest change to the shape of the code; `role` becomes what it looks like it already
  is; nothing about the colour engine moves.
- **Against**: a registry is session state, which this package has carefully avoided — every other
  fact is declared in a table read at build. Ordering (`after =`) is a second contract to keep. And a
  producer that forgets to unregister leaks into every table of the session.

### Shape B — the legend's WORDS as a per-table override

The wrong sentence is one word: the measure name. `legend_measure_word()` resolves it from
`MEASURES$<m>$word`, a closure. A `meta$legend` field could carry a per-table override read at the
same point:

```r
# NOT A PROPOSAL -- a sketch
set_legend_words(x, contrib = function() gettext("contribution to the variance of the axis"))
```

Everything else — swatches, slots, ladder, terse/prose, `lang`, the four media, the console —
continues to work untouched, because only the word changed.

- **For**: it rides `meta`, so it survives dplyr and needs no session state; it is one lookup, in the
  one place the word is already resolved; and it makes the console reachable, which no per-call
  argument can.
- **Against**: it solves the *word*, not the general problem. A producer wanting a legend of a shape
  the engine does not have (say, a threshold rule rather than a ladder) is no better off. And a
  closure in `meta` is a new kind of thing to carry — `meta` holds data today.

### And for the side tables — one mechanism instead of two

Whichever shape wins, `meta$footer_tabs` could take a **note** beside a table:

```r
# NOT A PROPOSAL -- a sketch
set_footer_tabs(x, list(eigenvalues = tab, shape = tx_note(df, headers =, align =, note =)))
```

with `tx_note()` producing the same record `reg_shape_table()` already returns by attribute
(`headers` / `align` / `noisy` / `note`), and the four emitters becoming one per medium for both
kinds. `tx_pipe_table()` is already generic and would be the console and markdown arm of it;
`shape_html_table()` and `xl_shape_cells()` would lose their `reg_` prefix and their gate.

That would retire `tab_wants_shape_table()`'s double hard-wiring (`tab_is_reg` **and**
`get_assumptions`) in favour of "the table carries one, or it does not".

---

## 4. What phase 6 did instead, and what it costs

Nothing above was built. `ggfacto` cuts the generated legend (`color_legend = FALSE` in its own
print/knit methods) and writes three plain-text lines into `subtext`. The price, stated so it is not
rediscovered:

- the legend is **not coloured** — no swatch beside the ×1 / ×2 / ×5 / ×10;
- it is **translated at build, not at render**, so `lang =` does not reach it and the session's
  language at the time the table was made is the one it keeps;
- in the **console**, tabxplor's own « contribution to Chi2 » still prints above it, because no
  per-table suppression exists.

The third is the one a reader meets, and it is the argument for doing this properly.
