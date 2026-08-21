# The html tooltips of `tab()` and `tab_reg()` — consistency review

## What this document is

A full review of the hover-tooltip text both producers emit, against the four questions the maintainer asked:

1. **Does the number match the table?** — formatting, measure, and whether the interval is attached to the quantity it belongs to.
2. **Does the label match what it is?** — and if not, is there a *general* rule, rather than a growing pile of exceptions?
3. **What is noise?** — what should not be shown, in which cases.
4. **What is missing?** — under the rule that the same fact should not be stated twice unless the repetition itself teaches something.

Every claim below was **reproduced against the running package** (`devtools::load_all()` at the current working tree) before being written down, and its root cause located at a line. Findings are keyed `T1`…`T21` and gathered in §3; §4–§7 hold one root cause and one capture each; §8 is the proposed design; §9 the blast radius and an implementation order.

⚠ Nothing here has been implemented. This is a review.

⚠ **Which tree this was measured on.** The captures come from `devtools::load_all()` on the **working
tree**, which carried in-flight colour-ladder work from another session (uncommitted changes to
`R/fmt_class.R`, `R/tab_classes.R`, `R/tab-display.R`, `R/tab.R`). Those hunks were checked against the
findings: they touch `COLOR_SCALES`, `fmt_color_plan()`, `normalize_color_spec()`, the legend phrases,
`log_coef$break_key` and the `ratio` token's `min_digits` — and **none of them touches
`tab_kable_print_tooltip()` or `format()`'s percentage mask**, the two places every finding below lives.
So every finding holds at `HEAD` as well; only some decimal counts in the captures reflect the newer
`min_digits`.

---

## 1. How a tooltip is built today

There is **one builder and one appender**:

| piece | home | what it produces |
| ------- | ------ | ------------------ |
| `tab_kable_print_tooltip()` | `tab_classes.R:1668` | the whole text of one **fmt column**, per row |
| `reg_append_empirical_tip()` | `tab-render-html.R:71` | appends the multinomial crude fragment |
| `reg_spec_tips_mnl()` | `reg-spec-build.R:250` | builds that fragment |
| `reg_spec_tips_num()` | `reg-spec-build.R:287` | a numeric predictor's descriptive fragment |
| `tab_tooltip_attrs()` | `tab-render-html.R:50` | wraps the text into the Bootstrap attributes |

The builder emits **15 optional fragments**, joined by `" ; "` in this fixed order:

```text
pct · mean · sd · est(+ci+p) · diff(+ci) · std diff · ratio · OR · ci · ctr · resid · obs · gap · n · note
```

Each fragment is `any()`-gated (the `format()` pass runs only where some cell carries the field), and each renders its number by **forcing a display token** onto a copy of the column: `tip_num(set_display(x, "<token>"))`.

**That forcing is where the review's two `A`-severity formatting defects surface**, though — see §8.2 — it is not where they live. A token is rendered relative to the column's `EST_SCALES` row, and asking for the token literally called `diff` is legitimate: since 22b-iii a regression column carries *every* geometry of one comparison, so a `diff:` line beside a ratio estimate is the deliberately-surfaced additive reading, not a mistake. What is wrong is that `format()`'s percentage mask, when it renders that token, asks *"is this column a mean?"* instead of *"is this quantity a percentage?"* — so on the two scales whose level is neither (`raw_diff`, `log_coef`) it multiplies a coefficient by 100 and adds a `%`.

Measured size of the surface (median / max characters per tooltip, and fragments per tooltip):

| table | n tips | chars (med / max) | fragments (med / max) |
| ------- | -------: | ------------------: | ----------------------: |
| `tab(pct = "row", color = "diff")` | 28 | 32 / 36 | 3 / 3 |
| `tab(ci = "cell", color = "contrib", guaranteed_effect)` | 28 | 90 / 99 | 7 / 7 |
| `tab()` on a numeric col_var | 4 | 67 / 68 | 5 / 5 |
| `tab_reg()` binomial, default OR | 33 | 71 / 87 | 4 / 5 |
| `tab_reg()` binomial, `measure = "difference"` | 33 | 49 / **126** | 3 / **6** |
| `tab_reg()` multinomial | 34 | 75 / 81 | 5 / 5 |

---

## 2. The three rules this review holds the tooltips to

They are not new; they are the package's own invariants applied to the hover layer.

- **R1 — one quantity, one rendering.** A number in a tooltip must be rendered exactly as the same number would be rendered in a cell: same ×100, same `%`, same `1/x` / `÷` glyph, same decimals. `format()` is the one display source of truth, and a tooltip is a `format()` consumer like any other.
- **R2 — an interval belongs to an estimate.** The bracket beside a value must be the interval *of that value*, on *that value's* scale.
- **R3 — a tooltip never states what the cell already shows**, unless the repetition adds something (an interval, a p-value, more decimals) — and a tooltip never shows a field it does not have.

R3's second half is already stated at `R/reg-spec-build.R:323` (*"A TOOLTIP NEVER SHOWS A FIELD IT DOES NOT HAVE"*); it is honoured there and not in the main builder.

---

## 3. Findings

Severity: **A** = a wrong number or a wrong claim reaches the reader · **B** = right number, misleading or
missing label · **C** = noise or polish. Each key is developed, with its root cause and its capture, in the
section named on the right.

### Severity A — the reader is told something untrue

| key | finding | § |
| ----- | --------- | --- |
| T1 | `diff:` prints a coefficient / a count difference / a log-odds **as a percentage** (`-312.07%` for `-3.1`) | 4.1 |
| T2 | In that line the **interval is right and the estimate wrong** — two scales in one fragment | 4.1 |
| T3 | The multinomial crude fragment is in **percentage points** while its own cell shows an **odds ratio** | 4.2 |
| T4 | A crude additive column's reference cell prints `diff: 0% ; ratio: 1` where its cell prints `ref` | 4.3 |

### Severity B — right number, wrong or missing label

| key | finding | § |
| ----- | --------- | --- |
| T5 | The **exact p-value appears only on multiplicative columns** — while `gap:` carries one either way | 4.4 |
| T6 | An **unlabelled percentage or mean** opens most tooltips (`14% ; 1/2.89 [...]`) | 5.1 |
| T7 | Labels name the **field**, never the estimand; `obs:` and `crude:` are two names for one thing | 5.1 |
| T8 | `mean_ctr` — an internal identifier — is a user-facing label | 5.3 |
| T9 | In French `diff` and `gap` both read **`écart`**, the package's own umbrella term | 5.3 |
| T10 | The Constant row's `n:` is a **reference-profile subgroup count**, rendered like a level's base | 7 |
| T11 | On a weighted table `n:` is the **unweighted** count beside a weighted percentage; `wn` is hidden | 7 |
| T19 | `tab()` computes a real **odds ratio on every `pct` table** and shows it only under `color = "OR"` | 7 |

### Severity C — noise and polish

| key | finding | § |
| ----- | --------- | --- |
| T12 | `OR: 1` on every reference cell, where `diff` / `ratio` correctly collapse to `ref` | 6 |
| T13 | `OR:` on a Total row whose `diff` and `ratio` were dropped as non-comparable | 6 |
| T14 | `diff:` / `ratio:` **repeat the cell** under a composite display | 6 |
| T15 | `ci:` repeats the cell's own bracket under `ci = "cell"`, differing by one decimal | 6 |
| T16 | Column **padding leaks into the tooltip** (`1/1.04␣␣␣[1/1.17;1.08]`) | 6 |
| T17 | A bare `ref` on the Constant / baseline row, which is the reference for nothing | 6 |
| T18 | `sd:` opens every mean-column tooltip, ahead of the deviation the table is about | 6 |
| T20 | Five of fifteen labels are not `gettext()`'d, and neither reg fragment is at all | 5.3 |
| T21 | `std. residual: +0` beside `-0.3` — the `min_digits = 1` floor does not survive a zero | 6 |

### Root causes, in one place

| key | root cause |
| ----- | ------------ |
| T1, T2 | `pct_or_ci` (`fmt_class.R:3111`) asks `var_kind == "mean"` where it means "is this a percentage" |
| T3 | `reg-spec-build.R:274`, a hard-coded `sprintf("%+.0f pts")` that never reaches `format()` |
| T4, T17 | the builder reads `get_reference(x, "cells")`; `format()` reads a role-aware mask |
| T5 | `cond_est` (`tab_classes.R:1740`) is gated on `!fold_ci`, i.e. on `est_field != "diff"` |
| T6, T7, T8, T20 | fifteen hand-written `paste0("<label>: ", …)`; no declared label anywhere |
| T9 | `po/R-fr.po`: `msgid "diff"` and `msgid "gap"` both `msgstr "écart"` |
| T10, T11 | `cond_n` reads the `n` field only, whatever produced it |
| T12, T13, T14, T15 | each fragment carries its own gate; the four shared conditions are applied unevenly |
| T16 | `tip_num()` trims the outside only; `format()` pads *per token* inside a composite |
| T19 | `cond_or` requires `get_scale(x) == "odds_ratio"` |
| T21 | `print_num()` |

---

## 4. Part A — does the number match the table?

### 4.1 T1/T2 — the reported defect, and it is wider than reported

Reported on a gaussian table; it is **a property of the scale, not of the family**, and it hits every scale whose additive estimate is *not* the `diff` token.

```r
tab_reg(gss_simple, outcome = "age", predictors = c("race","rincome","relig","tvhours"),
        family = "gaussian", empirical = TRUE)
```

```text
cell = -3.6 (39.7)     tip = diff: -364.1% [-4.6;-2.7] ; obs: -3.1 ; gap: +0.52 [...]
cell = +6.7            tip = diff: +666.2% [5.8;7.5]                     (Obs_diff)
```

Reproduced on the other two reachable scales:

```text
measure = "log"      cell = -0.89   tip = diff: -88.80% [-0.99;-0.78]     (log_coef)
poisson, difference  cell = +1.3    tip = diff: +129.4% [1.1;1.5]         (raw_diff)
```

**Root cause.** The builder asks for the token `"diff"`:

```r
ok_diff ~ paste0(gettext("diff"), ": ", tip_num(set_display(x, "diff")))   # tab_classes.R:1699
```

and `format()` puts that token in the percentage mask for everything that is not a mean:

```r
pct_or_ci <- ok & (display %in% c("pct", "diff", "ctr") & !(display == "diff" & is_mean) | …)
```

`raw_diff` and `log_coef` declare `var_kind = "coef"`, so `is_mean` is FALSE and the value is multiplied by 100 and given a `%`. The cell escapes because those scales declare `est_display = "coef"`, a token outside the mask.

**Which scales are affected.** `est_display` explains why the *cell* is right; `var_kind` is what decides
whether the *tooltip* is (§8.2):

| scale | `var_kind` | `est_display` | cell | tooltip's `diff:` | |
| ------- | ------------ | --------------- | ------ | ------------------- | --- |
| `points` | `pct` | `diff` | `-19.8%` | `-19.8%` | ✓ |
| `mean_diff` | `mean` | `diff` | `+1.5` | `+1.5` | ✓ |
| `odds_ratio` · `pct_ratio` | `pct` | `or` / `ratio` | `1/2.40` · `÷1.63` | `-23%` · `-20%` | ✓ derived |
| `mean_ratio` · the two `score_*` | `mean` | `or` / `ratio` | `×1.5` | `+1.2` | ✓ derived |
| `raw_diff` | **`coef`** | `coef` | `-3.6` | **`-364.1%`** | ✗ |
| `log_coef` | **`coef`** | `coef` | `-0.89` | **`-88.80%`** | ✗ |

⚠ The two `derived` rows are what makes the fix delicate: on a **ratio** column the `diff:` line is *not* the estimate — since 22b-iii every model and crude column carries **all** geometries of one comparison, and `diff:` there is the deliberately-surfaced additive reading. So the cure is **not** "always use `est_display`" — that would silence the additive reading of every ratio column. It is "**render each geometry on the scale that geometry belongs to**", which §8.2 shows is one condition in one mask.

**T2 is the sharpest form of it.** `fmt_ci_bracket()` takes `is_pct` from the column's scale and is therefore right; the estimate takes it from the forced token and is wrong. One fragment, two scales, and the bracket is what makes the error visible: `-312.1%` cannot have `[-4.0;-2.2]` as its interval.

### 4.2 T3 — the multinomial crude fragment is on another measure than its own cell

```r
tab_reg(gss_simple, "party3", c("race","rincome"), family = "multinomial", empirical = TRUE)
```

```text
cell = 1/2.89 (1/2.76)
tip  = 14% ; 1/2.89 [1/3.33;1/2.51], p = <0.01% ; diff: -6% ; ratio: ÷1.44 ; n: 1 862
       ; crude: 15% (-5 pts [-7; -4])
```

The cell's bracket `(1/2.76)` **is** the crude odds ratio (22a-ii's in-cell fold). The tooltip's `crude:` fragment reports `-5 pts` — the crude **risk difference**, with its own interval. Two different estimands for the same cell, in the same hover, neither saying which is which. The fragment is `sprintf`'d at `R/reg-spec-build.R:274`, never reaches `format()`, and therefore cannot follow the column's measure, its digits, or `options(tabxplor.ratio_print =)`.

It also states `15%` — the crude *level* — beside the tooltip's own leading `14%`, which is the **adjusted** level (T6). Two percentages, three characters apart, both unlabelled.

### 4.3 T4 — the crude column's reference cell disagrees with its own cell

```text
Obs_RD    cell = (52.0%)  0%    tip = diff: 0% ; ratio: 1 ; n: 9 846
Model_mRD cell = 0% (51.3%)     tip = ref ; obs: 0% ; n: 9 846
```

Same table, same scale, same row — one says `ref`, the other spells the neutral out.

**Root cause.** `format()` resolves the baseline through a role-aware rule (`R/fmt_class.R`, `ref_base()`: `is_refrow(x)` where `role` is non-empty). The tooltip resolves it through `get_reference(x, "cells")`, which is the **crosstab** rule. Measured:

```text
Obs_RD    ref_type = "tot"  refrow = 1,2,5   get_reference(cells) = (empty)
Model_mRD ref_type = ""     refrow = 1,2,5   get_reference(cells) = 1,2,5
```

`ref = "tot"` sends `get_reference_base()` down the `totrows & !totcol` branch, and a regression table has no total rows. So the difference is not conceptual at all — it is that the two builders stamp `ref` differently and the tooltip is the last consumer still reading the crosstab rule.

### 4.4 T5 — the p-value asymmetry, visible inside one tooltip

```text
diff: -19.8% [-22.1;-17.5] ; ratio: ÷1.6 ; OR: 1/2.4 ; obs: -21.2%
    ; gap: -1.4 pts [-2.1 pts; -0.6 pts], p = 0.021% ; n: 1 860
```

The **adjustment gap** carries an exact p-value; the **estimate** the whole column is about does not. On a multiplicative column it does (`1/2.40 [...], p = <0.01%`). The cell carries stars on both, so the exact value is available on both — `cond_est` simply never fires where the estimate lives in `diff`, because that branch was written to fill the hole the folded-CI line already covered, and the fold carries no p.

The same asymmetry reaches plain `tab()`: `tab(ci = "ref")` cells are starred and their tooltip is `diff: +4% [2.6;4.6]`, with no p anywhere.

### 4.5 What is correct, and must not be "fixed"

Verified sound, listed so a later pass does not disturb them:

- The `{ci}` bracket on every scale (`fmt_ci_bracket()`): `%`/×100 from `EST_SCALES$is_pct`, the `1/x` / `÷` glyph per bound from the measure, bounds never reordered.
- The `gap:` fragment: score, bounds and p all come from `fmt_adjustment_score()` / `fmt_gap_bounds()` / `fmt_gap_p()` — the very helpers the colour engine reads — and are rendered in the cell's own units through `fmt_gap_render()` (`-1.4 pts`, `×0.97`).
- The `est_ci`-driven fragment on multiplicative columns: `1/2.40 [1/2.68;1/2.15], p = <0.01%` matches the cell exactly.
- The `n` column's own tooltip is empty (`shows("n")` folds `n_range` onto `n`), and the `.note` carries the per-block breakdown (`marital: 16 395 ; tvhours: 8 610`).
- gof / blank footer cells get no tooltip at all.
- The transposed path pre-builds and flips the same text.

---

## 5. Part B — do the labels match, and is there a general rule?

### 5.1 The current labels

| fragment | label | translated | names |
| ---------- | ------- | :----------: | ------- |
| `pct` | *(none)* | — | the level, or on a regression the **adjusted** level |
| `mean` | *(none)* | — | idem |
| `sd` | `sd:` | ✗ | the cell's own SD |
| `est_ci` | *(none)* | — | the estimate, its interval, its p |
| `diff` | `diff:` | ✓ | a difference **in whatever units** |
| `std diff` | `std diff:` | ✓ | Glass's Δ |
| `ratio` | `ratio:` | ✗ | a relative risk / a ratio of means |
| `or` | `OR:` | ✗ | an odds ratio |
| `ci` | `ci:` | ✗ | a **level** interval |
| `ctr` | `contrib:` / `mean_ctr:` | ✓ | chi² contribution |
| `resid` | `std. residual:` | ✓ | adjusted standardized residual |
| `obs` | `obs:` / `ref. group:` | ✓ | the crude effect / the reference group's |
| `gap` | `gap:` | ✓ | the adjustment |
| `n` | `n:` | ✗ | the unweighted base |
| crude fragment | `crude:` | ✗ | the crude level and effect |
| numeric fragment | *(none)* | ✗ | the predictor's distribution |

Three things are wrong with this list as a *system*, independently of any single row:

- **The labels name fields, not quantities.** `diff:` is the same word for a percentage-point difference, a mean difference in years, a regression coefficient and a log-odds. `OR:` on a crosstab is an empirical odds ratio and on a regression column the model's own — the reader has no way to tell.
- **Two names for one thing.** `obs:` in the builder, `crude:` in the appender, in the same tooltip of the same table. The package settled on *observed (crude)* as a pair in Phase 22h; the tooltips predate that.
- **The adjusted/observed distinction — the thing `tab_reg()` exists for — is invisible.** `14% ; … ; crude: 15%` gives the reader two percentages and no word.

### 5.2 The general rule — and it needs no new vocabulary

**A tooltip line is named by the display token it renders, resolved through the column's scale.**

That is exactly what the cell already does, and the fact is already declared: `DISPLAY_TOKENS` carries a `doc` field, one user-facing phrase per token (*"the difference from the reference"*, *"the odds ratio"*, *"the level the estimate sits on"*). Those phrases are too long for a hover line, so the rule is **one new column beside `doc`** — a short `label` — and the builder stops hand-writing `paste0("<word>: ", …)`.

Three properties make this the right shape rather than another table:

1. **It is the same relation the cell obeys.** `fmt_resolve_scale_tokens()` already maps `{est}` / `{base}` onto the column's own token; running the label through the same resolver means the label and the number can never describe different quantities. It is also the second line of defence behind §8.2's mask fix: a line labelled *coefficient* that rendered a percentage would be visibly self-contradicting, where today's unqualified `diff:` hides it.
2. **It has no per-family branch.** `raw_diff` → `coef` → *"coefficient"*; `points` → `diff` → *"difference"*; `odds_ratio` → `or` → *"OR"*. A new scale is a row in `EST_SCALES` naming an existing token, which is already required.
3. **It absorbs the reg/crosstab distinction with one qualifier, not a second table.** `role` is already a column attribute with three values. *"adjusted"* on `role == "model"`, *"observed"* on `role == "emp"`, nothing on a crosstab, prefixed to the token's own label. `obs:` and `crude:` then collapse into one word, and `14% ; … ; crude: 15%` becomes `adjusted 14% ; … ; observed 15%`.

**What it would look like** (illustrative; the exact wording is a maintainer call):

```text
now  14% ; 1/2.89 [1/3.33;1/2.51], p = <0.01% ; diff: -6% ; ratio: ÷1.44 ; n: 1 862
                                                   ; crude: 15% (-5 pts [-7; -4])
then adjusted 14% ; OR 1/2.89 [1/3.33;1/2.51], p = <0.01% ; as points -6% ; as RR ÷1.44
                                                   ; observed 15%, OR 1/2.76 ; n: 1 862
```

**Two labels cannot come from the token and need their own fact, and both already have one:**

- The **derived geometries** (T7's *"as points"* / *"as RR"*): what a `diff:` line means beside a ratio estimate is *the same comparison read additively*. `EST_SCALES$unit` already distinguishes `points` / `units` / `or` / `ratio` / `rate_ratio` / `log`, and `MEASURES$<m>$word()` already yields the translated noun. The label is `unit` of the geometry, not of the column.
- The **Constant row's base count** (T10): a reference-profile subgroup count is not a level's base. `reg_constant_count()` already knows which of the three arms produced it; the label follows from the arm.

**What NOT to do**, and the reason each was rejected:

- *A `label` per `EST_SCALES` row.* Thirteen scales × the tokens each can render — the same word repeated, and `diff` on a ratio column would have to be declared per scale. The token is the finer key and the correct one.
- *A `switch(family, …)` in the builder.* This is precisely the per-family dispatch the declarative architecture exists to delete; a new family would need an edit here.
- *A free-text label argument on `tab_html()`.* Pushes the problem to the user and cannot be translated.

### 5.3 T8, T9, T20 — three defects that stand on their own

`mean_ctr` is a code identifier printed to a reader (French is already fine: *contrib. moy.*). And `"diff"` and `"gap"` both translate to **`écart`**, so the French tooltip reads `écart: -19.8% … écart: -1.4 pts` — two different quantities under one word, and under the word Phase 22h made the *umbrella* term. `dev/french_glossary.md` should settle the pair before the catalogue sweep of Phase 23f.

And **T20**: five of the fifteen labels were never wrapped in `gettext()` — `sd:`, `ratio:`, `OR:`, `n:` and
`, p =`. `OR` is an acronym and can stay; the other four are words. Neither reg fragment is translatable at
all: `"crude: %.0f%% (%+.0f pts …)"` and `"%s: mean %s (SD %s); mean if yes %s, if no %s"` are `sprintf`
formats with English embedded in them, and the second also hard-codes the binary *yes / no* wording.

---

## 6. Part C — what is noise

Applying R3 (never state what the cell shows; never show a field you do not have):

**Always removable**

- **T12** `OR: 1` on a reference cell. `diff` and `ratio` already collapse to `ref` there; `cond_or` simply lacks `ref_grp`.
- **T13** `OR:` on a Total row whose `diff` and `ratio` were dropped as non-comparable. `cond_or` lacks `comparable`. Measured: `tab(pct = "col", color = "OR")` prints `tip = OR: 1.40 ; n: 743` on a `100%` cell.
- **T17** the bare `ref` on a Constant / baseline row. That row is not the reference *for* anything — it is the quantity the column's effects operate on. On a population-average baseline the tooltip currently reads `ref ; OR: 1/1.2 ; n: 12 960`, which is wrong twice over (`ref`, and an odds *ratio* label on the baseline odds).

**Removable when the cell already says it** (this is the `shows()` gate, applied consistently)

- **T14** `diff:` and `ratio:` under a composite display. `out_diff` has no `shows()` gate; `ok_rr` tests only `display_primary()`, so an aside is not seen. Measured: `display = "{pct} ({ratio})"` → `cell = 51% (×1.08)`, `tip = diff: +4% ; ratio: ×1.08 ; n: 8 316`.
- **T15** `ci:` under `ci = "cell"`. The tooltip adds exactly one decimal (`[50.0;51.5]%` vs `[50;51]%`). Either keep it and say so, or drop it — the current state is an accident, not a choice.
- The `ratio:` line on a **regression** column when the column's own estimate is already a ratio of the same kind — `Obs_RR` correctly omits it, `Model_OR` shows both `or` and `ratio`, which on a logistic column are two genuinely different quantities and should stay. This one is a judgement call; §8.4 proposes the test.

**Order** (T18): `sd:` opens every mean-column tooltip, ahead of the deviation the table is about. The reading order should follow the table's own: what this cell IS, then how far it sits from its reference, then the base it rests on. Proposed: `level · estimate(+ci+p) · other geometries · comparison detail (std diff, contrib, resid) · observed · gap · base · note`.

**Rendering polish**

- **T16** — the column's alignment padding survives into the tooltip, because `tip_num()` trims the outside
  only while `format()` pads *per token* inside a composite: `1/1.04␣␣␣[1/1.17;1.08], p = 49% ; …`. A tooltip
  is prose; padding means nothing there and should be collapsed, not merely trimmed at the ends.
- **T21** — `std. residual: +0` sits beside `-0.3` in the same column: the `min_digits = 1` floor does not
  survive a value of exactly zero through `print_num()`. Cosmetic, and shared with the cell.

**Not noise, keep**: the `gap:` fragment (it is the only place the adjustment's interval and p can be read at all), the derived geometries on a regression column (22b-iii built them for exactly this), the numeric predictor's descriptive fragment.

---

## 7. Part D — what is missing

- **T19 — the odds ratio on a plain `tab()` percentage table.** The maintainer's point, and it is measured: on `tab(gss_simple, race, marital, pct = "row", color = "diff")` the `or` field is fully populated and meaningful (column *Separated*: `0.72 · 3.07 · 1.61 · 1`, the last being the reference row), and the tooltip never shows it because `cond_or` requires `get_scale(x) == "odds_ratio"`. Under `color = "OR"` the same table shows `OR: …`, so the number is trusted enough to colour by but not to hover. It should be shown on every `pct = "row"` / `"col"` column, exactly as `ratio:` is, subject to T12/T13's gates.
  ⚠ **and the label must stay `OR`, not `or`** — verified: the code writes `paste0("OR: ", …)` and the rendered attribute reads `OR:`. The lower-case `or` is the field name and never surfaces.
  ⚠ one caveat to state in the fix: under `color = "diff"` the `or` field is the 2×2 against *(reference row × reference column)*, under `color = "OR"` against *(reference row × first column)* — the reference differs with the measure, so the label may need the same reference phrase the legend uses.
- **T5 — the exact p-value on additive columns.** Available (the cell is starred), missing from the hover.
- **T11 — the weighted base.** On a weighted table `n: 8 316` sits beside a percentage computed on `wn = 10 418`. Measured: `wn` is populated, `n_eff` is `NA` under `design_effect = FALSE`. At minimum the label must distinguish the two; showing both where they differ is the honest form, and `n_eff` should appear wherever it is finite, since it is what the interval was computed on.
- **T10 — the Constant row's base.** 715 people for a reference profile, rendered like a level's base count.
- **A regression column never states what it is adjusted for.** The footer does, once per table. Debatable; probably right to leave to the footer.

**Deliberately not added**: the confidence level and the CI method (both in the legend, and per-column heterogeneity is already a legend problem); the column's `pct_type`; anything the legend states once for the whole table.

---

## 8. The proposed design

### 8.1 One principle

> **A tooltip is a rendering of the cell's own record, and every line of it goes through `format()` on the column's own scale, labelled by the token it renders.**

Everything in §3 is a departure from that sentence.

### 8.2 The mechanical fix for T1/T2 — the `%` mask must ask the right question

The forced-token idiom itself is cheap and vectorised and is worth keeping. What is wrong sits one level down, in `format()`:

```r
pct_or_ci <- ok & (display %in% c("pct", "diff", "ctr") & !(display == "diff" & is_mean) | …)
```

The `diff` arm asks **"is this column a mean?"** and treats everything else as a percentage. The question it means to ask is **"is the quantity this token renders a percentage?"**, and for the `diff` token that is *"is this column's LEVEL a percentage"* — i.e. `EST_SCALES$var_kind == "pct"`, a fact every scale already declares.

⚠ **It is `var_kind`, not `is_pct`** — and the difference is not academic. `is_pct` says *"is this column's own ESTIMATE a percentage"*, which is TRUE on only three scales. Measured on a logistic model column:

```text
Model_OR   get_diff = -0.2295   get_pct = 0.5072 / 0.2777   rendered `diff` token = -23%
```

The derived additive reading of an odds-ratio column is a **risk difference in percentage points** (22b-iii derives it from the two adjusted predictions), so it must keep its `%` — while `odds_ratio$is_pct` is FALSE. Switching the mask to `is_pct` would therefore fix `raw_diff` and `log_coef` and simultaneously break `odds_ratio` and `pct_ratio`, i.e. the default column of every logistic table.

`var_kind` is right on all thirteen rows, which is the check that chose it:

| scale | `var_kind` | today | with `var_kind == "pct"` | |
| ------- | ------------ | ------- | -------------------------- | --- |
| `level_pct` · `points` · `mixed` | `pct` | `%` | `%` | unchanged |
| `odds_ratio` · `pct_ratio` | `pct` | `%` | `%` | unchanged — the derived risk difference |
| `mean_diff` · `level_mean` · `mean_ratio` · `score_ratio` · `score_odds_ratio` | `mean` | bare | bare | unchanged |
| `raw_diff` · `log_coef` | `coef` | **`%`** | bare | **fixed (T1, T2)** |
| `level_n` | `count` | `%` | bare | unreachable — `diff` is `NA` there (verified) |

So the change is **one condition in one mask**, it moves exactly the two scales that are wrong, and it fixes T1 and T2 in `format()` — hence for every backend, not only the hover — while leaving the tooltip's literal `"diff"` harmless.

**Blast radius, checked**: `pct_or_ci` also feeds the Excel `%` numFmt (`excel_pct`) and `pct_no_ci`, so both follow the same correction — which is what a user wants, since a coefficient exported to Excel as a percentage is the same defect. The two moving scales are `tab_reg()`-only, and **no golden case builds a `tab_reg()` table** (verified: zero `tab_reg` occurrences in `tests/testthat/helper-golden.R`), so no `_golden/*.rds` should move. `tests/testthat/_snaps/render-html.md` will.

**An independent, optional readability improvement**: where a line *is* the column's own estimate, resolve the token through the scale (`fmt_resolve_scale_tokens("est", scl)`) instead of writing `"diff"`, so a gaussian column's line is labelled and rendered as a **coefficient**. That is a §8.3 (labelling) decision, not a correctness one — once the mask is right, the number is right either way.

### 8.3 The label — one new declared column

`DISPLAY_TOKENS` gains a short `label` beside `doc`, `NA` where the token opens a tooltip line unlabelled today and should stay so (the estimate itself). `role` supplies the *adjusted* / *observed* qualifier. `EST_SCALES$unit` + `MEASURES$<m>$word()` supply the derived-geometry phrasing. No new table, no per-family branch, one foreign key to check at load.

### 8.4 The gates, stated once instead of per fragment

Today each of the fifteen fragments carries its own hand-written condition, and the four that matter (`comparable`, `ref_grp`, `shows()`, the role-aware reference) are applied to different subsets of them. The proposal is **one gate applied to every value-bearing fragment**:

```text
show(token) :=  the field is finite
            &   the cell is comparable            (not its own 100% base)
            &   the cell is not the reference     (else the line collapses to "ref")
            &   the cell does not already show it (shows(), over the WHOLE template)
```

That single rule closes T12, T13, T14, T15 and — once the reference mask is the role-aware one — T4 and T17.

### 8.5 The two reg fragments

`reg_spec_tips_mnl()`'s `sprintf` should be replaced by the same `format()` path the cells use, so the crude value in the tooltip and the crude value in the cell are one rendering (T3). The natural shape is the one 22a-ii already built: the crude value is an ordinary `fmt` cell — build it, render it, read it — rather than a string assembled from grid columns. Both fragments also need `gettext()` (T20).

---

## 9. Blast radius and a suggested order

| step | touches | risk | closes |
| ------ | --------- | ------ | -------- |
| 1 | `pct_or_ci` mask: `var_kind == "pct"` instead of `!is_mean` | **medium** — display engine + Excel numFmt | T1, T2 |
| 2 | the reference mask in the builder → the role-aware one | low | T4, and half of T17 |
| 3 | one shared `show(token)` gate | low | T12, T13, T14, T15 |
| 4 | `cond_or` widened to every `pct = "row"` / `"col"` column | low | T19 |
| 5 | `cond_est` un-gated from `!fold_ci` | low | T5 |
| 6 | `DISPLAY_TOKENS$label` + the `role` qualifier; drop `mean_ctr`; settle `écart` | medium — wording, new msgids | T6–T11, T19 |
| 7 | the two reg fragments through `format()`; `gettext()` both | medium | T3, T20 |
| 8 | strip inner padding in `tip_num()`; fragment order | trivial | T16, T18, T21 |

⚠ Step 1 is the only one with reach outside the hover layer, and it is the one that matters most: it fixes T1/T2 in `format()`, so print, Markdown and Excel are corrected with it. **Goldens verified unaffected** — no golden case builds a `tab_reg()` table, and the two moving scales are `tab_reg()`-only.

Steps 1–5 are **defect fixes** and are worth doing before release; step 6 is the *design* the maintainer asked for and is where the user-facing wording is decided; steps 7–8 are polish.

⚠ **Snapshot note**: `tests/testthat/_snaps/render-html.md` carries tooltip text, so steps 1, 3, 4, 5 and 6 will move it. Every diff there is a tooltip line and should be reviewed as a list, not accepted wholesale.

---

## 10. Appendix — the captures

All produced with `gss_simple <- gss_cat_data_formatting()` on the current working tree.

**A1 — the reported gaussian defect** (`family = "gaussian"`, `empirical = TRUE`)

```text
Obs_diff    cell = +6.7          tip = diff: +666.2% [5.8;7.5] ; n: 3 833
Model_diff  cell = -3.6 (39.7)   tip = diff: -364.1% [-4.6;-2.7] ; obs: -3.1
                                       ; gap: +0.52 [+0.15; +0.89], p = 0.612% ; n: 1 006
```

**A2 — the same defect on the other two scales**

```text
measure = "log"                Obs_log(OR)  cell = -0.89  tip = diff: -88.80% [-0.99;-0.78]
poisson, measure = "difference" Obs_diff    cell = +1.3   tip = diff: +129.4% [1.1;1.5]
```

**A3 — the p-value asymmetry inside one tooltip** (`family = "binomial", measure = "difference"`)

```text
diff: -19.8% [-22.1;-17.5] ; ratio: ÷1.6 ; OR: 1/2.4 ; obs: -21.2%
    ; gap: -1.4 pts [-2.1 pts; -0.6 pts], p = 0.021% ; n: 1 860
```

**A4 — the multinomial crude fragment on another measure than its cell**

```text
cell = 1/2.89 (1/2.76)
tip  = 14% ; 1/2.89 [1/3.33;1/2.51], p = <0.01% ; diff: -6% ; ratio: ÷1.44 ; n: 1 862
       ; crude: 15% (-5 pts [-7; -4])
```

**A5 — the reference cell disagreeing with its own cell**

```text
Obs_RD     cell = (52.0%)  0%   tip = diff: 0% ; ratio: 1 ; n: 9 846
Model_mRD  cell = 0% (51.3%)    tip = ref ; obs: 0% ; n: 9 846
```

**A6 — `OR:` where `diff` and `ratio` were suppressed** (`tab(pct = "col", color = "OR")`)

```text
cell = 82%    tip = ref ; OR: 1 ; n: 8 316           (reference cell)
cell = 100%   tip = OR: 1.40 ; n: 743                (total row, non-comparable)
```

**A7 — the odds ratio computed and hidden** (`tab(pct = "row", color = "diff")`)

```text
column "Separated"   or = 0.72  3.07  1.61  1        tip = diff: +3% ; ratio: ×1.81 ; n: 196
```

**A8 — repetition under a composite display** (`display = "{pct} ({ratio})"`)

```text
cell = 51% (×1.08)   tip = diff: +4% ; ratio: ×1.08 ; n: 8 316
```

**A9 — padding leaking into the tooltip**

```text
|1/1.04␣␣␣[1/1.17;1.08], p = 49% ; diff: -1% ; ratio: ÷1.02 ; obs: 1/1.11 ; n: 1 263|
```

**A10 — the weighted base**

```text
tab(wt = w, pct = "row")   n = 8 316   wn = 10 418   n_eff = NA
cell = 51%                 tip = diff: +4% [2.5;4.6] ; ratio: ×1.08 ; n: 8 316
```

**A11 — the baseline row**

```text
binomial, effect = "conditional"  cell = 1/1.19 (46%)  tip = 1/1.19 [...], p = 0.0766% ; ref
binomial, measure = "difference"  cell = 48.7%         tip = ref ; OR: 1/1.2 ; n: 12 960
poisson,  measure = "difference"  cell = 2.6           tip = n: 6 803
poisson,  default (IRR)           cell = 2.9           tip = ref
```

⚠ the last four lines are the same row of four tables of two families, and no two of them are labelled alike.
