# Phase 22a — the crude/adjusted comparison, `family × effect × measure`, and `display`

Research and design study. Status: **design proposal, nothing implemented.** Read this before touching
`R/reg-empirical.R`, `R/reg-estimand.R`, `R/tab_reg.R`'s display block, or the legend builder in
`R/fmt_class.R`.

Everything below was checked against the running package (`devtools::load_all()`, `forcats::gss_cat`
through `gss_cat_data_formatting()`, 21 483 rows). Every number, string and legend line quoted is a
real capture, not a reconstruction. File:line references are HEAD at `544e926`.

---

## 1. What this document is for

The maintainer's Phase 22a review asks three questions that turn out to be **one** question:

1. why are the observed and modelled columns coloured on different ladders, and could there be one
   observed column and one model column instead;
2. why does the `family × effect × measure` grid name the difference path three ways (`β` / `RD` /
   `AME` / `MER`) and the ratio path one way (`RR` everywhere), and are adjusted percentages a
   `measure`;
3. why does `display` behave differently on every path, and what should the regression presets and
   tokens be.

They are one question because all three are symptoms of the same thing: **`tab_reg()` builds the
observed column, the model column and the display as three separate systems that happen to sit next
to each other.** `tab()` does not work that way — there, one column carries every geometry of the
same comparison and `display` picks which one to print. This document works out what the regression
side looks like when it obeys the same rule, and what breaks if it does.

The conclusion in one paragraph:

> The observed column and the model column are the **same estimand computed twice** — once with one
> predictor, once with all of them. So they must be one column *shape* built twice, not two shapes:
> same stored scale, same colour measure, same display template, same legend block, and the crude
> level (`%` / mean) folded into the crude cell exactly as the adjusted level is already folded into
> the model cell. `display` then applies to both, every preset works on every family, and the
> crude/adjusted comparison is read **across** the table rather than needing a display of its own.
> Adjusted percentages are not a `measure` — they are the second slot of the display, which is
> precisely how `tab()` already prints a percentage and colours it by its difference.

---

## 2. Method: what was measured

```r
gss_simple <- gss_cat_data_formatting()
p <- c("race", "rincome", "relig", "age")
tab_reg(gss_simple, "married", p, family = "binomial", measure = "ratio", empirical = TRUE)
```

captured for: `binomial` × {`odds_ratio`, `ratio`, `difference`} × {`coefficient`, `marginal`},
`gaussian`, `poisson`, `ordinal`, `multinomial`; each with `empirical = TRUE`, with
`color = "adjustment"` and `color = c(TRUE, "adjustment")`; every `display` preset and a dozen `{}`
templates; console, `tab_color_legend(style = "terse" | "prose")` and `tab_html()` tooltips.

Field population was read straight off the records with `vctrs::field()`; timings are the median of
three warm runs, single-threaded.

---

## 3. Part I — the diagnosis

Five structural inconsistencies, in decreasing order of how much they cost the reader.

### 3.1 Two crude columns, two ladders, two legend blocks

The default binomial table:

```text
  var      levels                 n `Obs_%`    Obs_OR  Model_OR
1 Constant Reference population 12 960                  1.22***
2 race     White                 9 846  52%         1     1.00
3 race     Black                 1 860  31%*** 1/2.67*** 1/2.88***
4 race     Other                 1 254  49%*   1/1.13*** 1/1.10*

# Obs_%: difference (ref.): -30 -20 -10 -5 +5 +10 +20 +30 [grey: ... under +/-5 points]
# Obs_OR, Model_OR: OR (ref.): 1/4 1/2 1/1.5 1/1.2 1.2 1.5 2 4 [grey: ... under x1.2]
```

Three columns, **three renderings of one comparison**: `Obs_%` is the crude risk, `Obs_OR` the crude
odds ratio, `Model_OR` the adjusted one. `Obs_%` and `Obs_OR` are computed from the same 2x2 and test
the same null against the same reference level — verified: `Obs_%`'s stars come from the Wald risk
difference against the reference *level*, not against the total. So the two columns disagree about
nothing except **which ladder grades them**: `points` (`+/-5 +/-10 +/-20 +/-30`) versus `odds_ratio`
(`x1.2 x1.5 x2 x4`).

That is the maintainer's diagnosis, and it is exactly right. The declared cause is one column of
`REG_EMPIRICAL` (`R/reg-empirical.R:576-650`): every shape row carries **its own `color`**.

| shape row       | `nm`       | `scale`      | `color`   | ladder       |
|-----------------|------------|--------------|-----------|--------------|
| `binomial$base` | `Obs_%`    | `points`     | `"diff"`  | `pct_diff`   |
| `binomial$or`   | `Obs_OR`   | `odds_ratio` | `"OR"`    | `odds_ratio` |
| `rr$rr`         | `Obs_RR`   | `odds_ratio` | `"OR"`    | `odds_ratio` |
| `mr$base`       | `Obs_mean` | `level_mean` | `""`      | none         |
| `mr$mr`         | `Obs_RoM`  | `mean_ratio` | `"ratio"` | `mean_ratio` |
| `poisson$base`  | `Obs_rate` | `mean_ratio` | `"ratio"` | `mean_ratio` |
| `poisson$irr`   | `Obs_IRR`  | `odds_ratio` | `"OR"`    | `odds_ratio` |
| `gaussian$diff` | `Obs_diff` | `raw_diff`   | `"diff"`  | `mean_diff`  |

Read the poisson pair: `Obs_rate` is graded on `mean_ratio` with the `÷ ×` glyphs while `Obs_IRR` is
graded on `odds_ratio` with the `1/` glyph — **two multiplicative ladders with two notations, in the
same table, for the same association.** Measured legend:

```text
# Obs_rate: ratio (ref.): /4 /2 /1.5 /1.2 x1.2 x1.5 x2 x4 [grey: ... under x1.2]
# Obs_IRR, Model_IRR: IRR (ref.): 1/4 1/2 1/1.5 1/1.2 1.2 1.5 2 4 [grey: ... under x1.2]
```

Scope, measured precisely — this is worth knowing because it says how much of the codebase the fix
touches:

- `gaussian` and `grouped_binomial` are **already fine**: their base column is `Obs_mean`, declared
  `color = ""`, so it produces no legend spec at all and the effect column merges with the model one.
- `binomial × difference` (`RD` / `AME`) is fine too: base and effect are both `points` / `"diff"`,
  and the legend really does print one line for all three columns — verified.
- **The broken set is exactly `binomial × {odds_ratio, ratio}` and `poisson × ratio`**: the families
  where a *coloured* base column sits on a different ladder from the effect column.
- `ordinal` has no base column at all, and `multinomial` has no crude columns at all.

So **the two-ladder problem is not a family quirk, it is the `base` column's existence.** Where the
base column happens to share the effect column's geometry (the additive families) there is no problem;
where it does not, there are two ladders. Removing the base column as a *column* removes the problem
for every family at once, with no per-family branch.

`legend_group_by_body()` (`R/fmt_class.R:4803`) groups columns by their **rendered legend sentence**:
two columns share a block if and only if their sentence is byte-identical. That is why `Obs_OR` and
`Model_OR` already merge (their bodies coincide, even though one interval is Woolf and the other
Wald-on-log — both render as *"Wald interval on the log odds-ratio"*), and why `Obs_%` cannot.

### 3.2 `display` reaches one column out of three, and is silently ignored on the marginal path

Measured, all with `empirical = TRUE`:

| path                            | `display = "value"` | `display = "ci"` | `display = "prob"`       | `{}` template  |
|---------------------------------|---------------------|------------------|--------------------------|----------------|
| coefficient, binomial           | model col only      | model col only   | model col only           | model col only |
| coefficient, gaussian / poisson | model col only      | model col only   | **degraded to `est_ci`** | degraded       |
| marginal, any family            | **ignored**         | **ignored**      | **ignored**              | **ignored**    |
| the `Obs_*` columns             | never               | never            | never                    | never          |

Three separate rules:

- `reg_resolve_output()` (`R/reg-resolve.R:435-439`) resets any non-`"value"` display to `"value"`
  when the builder is not `"coef"`, with a message. So on the whole marginal path `display` does
  nothing at all. Confirmed: `display = "est_ci"`, `display = "{diff}"` and no `display` produce
  byte-identical tables.
- `reg_apply_display()` (`R/tab_reg.R:1660`) degrades a *folding* template to `est_ci` on any
  non-binomial column. So `display = "prob"` — "show me the adjusted percentage" — is binomial-only;
  the adjusted **mean** of a linear model is unreachable, which is the maintainer's point 3.4.
- `reg_empirical_columns()` never consults `display`; the `Obs_*` templates are the shape row's own
  `display` literal.

And the token the presets are aliases of does not exist as a token:

```r
tab_reg(..., display = "{value}")
#> Error: Unknown field "value" in `display` template.
#>   i Valid fields: "pct", "n", "wn", "mean", "diff", "ratio", "ci", "or", "ctr", "var",
#>     "resid", and "obs".
```

`"value"` is only a preset **name** meaning "leave every cell's token alone" (`R/tab_reg.R:1635`,
whitelisted as a non-token in `R/zzz-fact-keys.R:168-170`). There is therefore no way to write a
template that says "the estimate, whatever this column estimates" — which is what makes every preset
family-specific and why the shorthand table has to hard-code `or`:

```r
REG_DISPLAY_SHORTHANDS <- c(value = "value", ci = "est_ci",
                            prob = "{or} ({pct})", ame = "{or} ({diff})")
```

`{or}` is void on `Model_β` (a gaussian coefficient lives in `diff`) and on `Model_RoM` (which lives
in `ratio`). The presets work on exactly the families whose estimate happens to land in `or`.

### 3.3 The multiplicative inverse is rendered three different ways

`format.tabxplor_fmt()` has three separate multiplicative renderings:

| rendering         | tokens                                     | gate                                                         | example  |
|-------------------|--------------------------------------------|--------------------------------------------------------------|----------|
| `1/x.xx`          | `or`, `OR`                                 | only when `special_formatting = TRUE` (`R/fmt_class.R:3036`) | `1/2.67` |
| `÷x.xx` / `×x.xx` | `ratio`                                    | always (`R/fmt_class.R:2884-2903`)                           | `÷2.67`  |
| raw `0.xx`        | `est_ci`, **and every composite template** | `R/fmt_class.R:3055`, `:3116`                                | `0.37`   |

`special_formatting = FALSE` is exactly what the composite expander passes to its recursive
`format()` call (`R/fmt_class.R:3116`). So **any template with more than one token loses the
inversion**, which is the whole of the maintainer's Phase 22b observation:

```text
display = "value"            1/2.67***
display = "{or} ({obs})"     0.37*** (0.37)     <- both halves raw
display = "ci"               0.37 [0.32;0.44]   <- deliberate (forest-plot convention)
effect = "marginal"          0.53*** (26.7%)    <- composite, so raw
```

The scale already declares whether it is multiplicative (`EST_SCALES$<key>$mult`) and the `ratio`
token already reads that flag. The `or`/`ratio` split is historical, not designed: `or` and `ratio`
are two fields of the *same* geometry, and one of them prints `1/x` while the other prints `÷x`.

### 3.4 The header words encode two different facts depending on the measure

The measured grid (`reg_measures(gss_simple, "married")` and the vignette's own table):

| measure      | `coefficient` | `marginal`  | `at_reference` |
|--------------|---------------|-------------|----------------|
| `odds_ratio` | `Model_OR`    | not offered | not offered    |
| `ratio`      | `Model_RR`    | `Model_RR`  | `Model_RR`     |
| `difference` | `Model_RD`    | `Model_AME` | `Model_MER`    |
| `log`        | `Model_β`     | not offered | not offered    |

The `difference` row names the **contrast** and drops the measure; the `ratio` row names the
**measure** and drops the contrast. Same header, three different estimands on one row; three
different headers, one measure on the other. The maintainer read this as a vocabulary problem. It is
worse than that: on the `ratio` row a reader cannot tell a conditional risk ratio from a marginal one
without reading the footer, and on the `difference` row `Model_RD` and `Model_AME` are both risk
differences in percentage points that the header presents as different quantities.

Two more collisions, both measured:

- **`Model_β` names five distinct quantities**: the gaussian plain coefficient, plus the `log` path of
  gaussian (log ratio of means), binomial (log odds), poisson (log rate) and multinomial/ordinal
  (log odds). All five are `word = "β"` in `REG_ESTIMANDS`. Meanwhile the crude side already names
  them correctly — `Obs_log(OR)`, `Obs_log(RR)`, `Obs_log(IRR)`, `Obs_log(RoM)`, `Obs_log(cumOR)`.
  The model side is the only one that lost the information.
- **ordinal prints `Model_OR` beside `Obs_cumOR`.** The crude column says "cumulative"; the model
  column does not, though the model column is the proportional-odds one.

Why `AME` cannot simply be extended to the ratio path — the finding that settles the naming question.
`REG_ESTIMANDS`' ratio rows ask `marginaleffects` for `comparison = "lnratioavg"`, which is
`log( mean(Ŷ | treated) / mean(Ŷ | control) )`: **the ratio of the averages**. The difference rows use
the default contrast, `mean(Ŷ₁) − mean(Ŷ₀)`, which for an additive contrast equals the average of the
individual differences. So:

- averaging **commutes** with the additive contrast — "average marginal effect" is literally what the
  number is;
- averaging **does not commute** with the multiplicative contrast — the marginal risk ratio is not an
  average of anything, it is a contrast of two standardised means.

Any name of the form "average marginal effect ratio" (or `AMR`, or an `ME` prefix) is therefore
technically wrong, and re-imports the very confusion the maintainer wants removed. The literature's
axis is **marginal vs conditional**, not "effect vs average effect": in the non-collapsibility
literature these are the *marginal* and *conditional* odds/risk ratios, obtained by g-computation
(model-based standardisation). In the Stata/Long-Freese vocabulary tabxplor's audience knows, the two
axes are (effects | predictions) x (averaged | at representative values) — `AME`, `MER`, `AAP`, `APM`.
`MER` is used correctly today; `AME` is correct for the difference path only.

### 3.5 The tooltips show the least useful half

Measured HTML `title=` strings, binomial, `empirical = TRUE`:

| column            | tooltip                                                                                       |
|-------------------|-----------------------------------------------------------------------------------------------|
| `n`               | `n: 3 115`                                                                                    |
| `Obs_%`           | `28% ; diff: -23% [-24.7;-21.2] ; n: 3 115`                                                   |
| `Obs_OR`          | `n: 3 115`                                                                                    |
| `Model_OR`        | `obs: 0.37`                                                                                   |
| `Model_AME`       | `diff: -22.1% [-23.9;-20.3] ; OR: 0.39 ; obs: -23.0% ; gap: -0.01 [-0.01; -0.01], p = <0.01%` |
| numeric predictor | `age: mean 47.2 (SD 17.3); mean if yes 48.7, if no 45.8`                                      |

The two ratio columns have **no interval and no p-value in their tooltip at all** — the one thing a
hovering reader wants from an odds ratio. The cause is structural, not an oversight:
`tab_kable_print_tooltip()` emits its CI only for `kind == "level"` columns (`R/tab_classes.R:1648`)
and its `out_diff` interval fold requires `est_field != "or"` (`:1665`). Every ratio column of every
regression is excluded by both. So the design intent ("the tooltip shows what the cell does not
print") is inverted on precisely the default column.

Also visible above: the gap line prints `-0.01` on a column whose `obs` prints `-23.0%`. The score of
a `points` column is stored as a proportion and the gap formatter is
`sprintf("%+.2f", v)` (`R/tab_classes.R:1743`) — no `x100`, no unit word. The bounds then round to
`[-0.01; -0.01]`, i.e. a visibly degenerate interval.

### 3.6 Smaller defects found on the way

Reported, not fixed — each is a one-liner but each is a maintainer-facing decision.

1. **`needs = "marginaleffects"` aborts on a path that does not use it.** Every `marginal` row of
   `REG_ESTIMANDS` declares `needs = "marginaleffects"` (`R/reg-estimand.R:241-286`), and
   `reg_check_deps()` (`R/tab_reg.R:86-93`) aborts up front when the package is absent. But
   `reg_marginal_engine()` (`R/reg-estimand.R:185-189`) resolves `effect = "marginal"` to the
   **dependency-free** `gcomp` engine; `marginaleffects` is genuinely required only for
   `at_reference`, and as a fallback when `gcomp` returns `NULL`. The guard should move to that
   fallback, or `needs` should be declared only on the `at_reference` rows. This matters to §4.4
   below: it is the difference between "always populate the marginal quantities" being free and being
   a new hard dependency.
2. **`color = "adjustment"` has no test on the default binomial path, by design, but the field is
   silently absent rather than explained.** Measured: `Model_OR` at `effect = "coefficient"` carries
   `obs` but **no `gap_se`** (0/17), so `fmt_gap_force_policy()` demotes it to `ignore`. That is
   ruling Q1(b) working correctly (non-collapsibility). It is only worth noting because §4.6's
   proposal must not appear to change it.
3. **The `[outcome]` suffix on `Obs_*` columns.** `reg_add_emp_cols()` appends `" [outcome]"` to the
   crude columns exactly as to the model columns in a multi-outcome table. Phase 22b already asks for
   the exporter to stop repeating it; the crude columns inherit the same fix.
4. **The multinomial `col_var` already carries the measure** (`"relig: OR"`), while the binomial one
   does not (`"married: 01-Married"`). §4.5 turns that accident into the rule.
5. **The vignette claims a parity the gaussian crude interval does not have.** *"student :
   confidence intervals with pooled variance, to match those computed by linear regression"* is true
   for a 2-level predictor (identical to 6e-15) and false for a k-level one (up to 8.9 % apart),
   because the closed form pools pairwise while `lm` pools globally. Measurements and both fixes in
   §4.2 and Q7.
6. **`{gap}` is not a token, and tooltips are HTML-only.** So a print, Excel or Markdown reader has no
   access to the gap interval or its p-value at all — the information exists only on hover. `dev/
   model_vs_observed_gap_test.md` ruling Q6 put the gap in the tooltip deliberately, and a `{gap_p}`
   token was refused; `dev/reg_comparison_framework_stress_test.md` then re-opened a narrow version
   for print/B&W/Excel and it was never shipped. Phase 22d (black-and-white publication print) makes
   this a live hole. See §4.7.

---

## 4. Part II — the keys

Six ideas. Together they make the three questions one answer.

### 4.1 K1 — a prediction is not a measure; it is the display's second slot

The maintainer asks whether adjusted percentages should get a `measure` — `"value"`, `"base"`,
`"identity"` — and suspects the answer is no. The answer is no, and the reason is structural rather
than a matter of taste.

`measure` is declared to be the **geometry of a contrast** (`ratio` / `difference` / `log` /
`odds_ratio`), and `REG_ESTIMANDS`' job is to turn `(family, effect, measure)` into a fit and an
estimand. An adjusted predicted probability is not a contrast: it has no null, no reference, and
therefore no ladder. `EST_SCALES` states this as a fact about the three `kind = "level"` scales:

```r
# the three LEVEL scales: a cell percentage / a mean / a count. No null to draw (the reference is a
# per-column value), and no ladder of their own -- a level column's colour ladder grades its
# DIFFERENCE, so putting it on the level axis would be a lie.
```

A `measure = "identity"` row would therefore be a row that cannot colour, cannot be tested, and
cannot take the marginal/conditional marker — the definition of a white elephant. Worse, it would
break the one invariant that keeps `measure` meaningful: *`measure` changes what is fitted, `display`
changes what is shown*. An adjusted prediction changes neither the fit nor the estimand.

But the user need behind the question is real, and the framework already answers it — **in `tab()`**.
A `tab()` percentage column shows the percentage and is coloured and starred by its *difference from
the reference*. That is a level printed, graded by an effect. Transposed to `tab_reg()`:

```text
display = "{est} ({base})"      1/1.63*** (31.5%)     the effect, with the adjusted % beside it
display = "{base} ({est})"      31.5%*** (1/1.63)     the adjusted %, graded by the effect
display = "{base}"              31.5%***              adjusted percentages, tout court
```

The third line is the "table of adjusted percentages" the question is really about, and it needs no
new estimand: the cell prints `pct`, the stars and the colour come from the column's stored effect and
its interval, exactly as in a crosstab. **The `measure` argument is untouched; one display token does
the work.**

Two sub-questions from the review, answered:

- *Is it useless because the comparison would be relative to each predictor's reference level?* No —
  that **is** the right comparison, and it is the one already stored. The adjusted percentage of
  `race = Black` is meaningful next to the adjusted percentage of `race = White`, which is the
  reference; the effect that grades it is the same contrast the column already reports.
- *Is it only available on the `marginal` / `at_reference` paths?* No. Measured: `display = "prob"` on
  `effect = "coefficient"` already works and prints `(50.87%)`, because `reg_apply_display()` calls
  `reg_marginal(at = "average", want_se = FALSE)` on the coefficient fit. An adjusted prediction is a
  property of the **fit**, not of the contrast — so it is available on every path that has a fit,
  which is every path. What differs is only whether the *reported effect* was standardised.

### 4.2 K2 — the crude column is the model column's mirror

The governing ruling already exists (`dev/model_vs_observed_gap_test.md:954-957`):

> The observed effect is the model's own effect, fitted with one predictor. When that univariable
> model is *saturated*, it has a closed form and tabxplor uses it.

If that is true — and it is verified to 1e-10 across five families — then the crude column and the
model column are **the same column shape, built from two fits**. Everything that describes the shape
must therefore be shared, and only what describes the *estimation* may differ:

| fact                           | shared?                | why                                                          |
|--------------------------------|------------------------|--------------------------------------------------------------|
| stored `scale`                 | **shared**             | same estimand, so same `EST_SCALES` row (already true today) |
| colour measure                 | **must become shared** | this is §3.1's bug                                           |
| colour breaks / ladder         | follows the scale      | automatic once the measure is shared                         |
| `display` template             | **must become shared** | this is §3.2's bug                                           |
| digits                         | **must become shared** | measured mismatch: `Obs_%` prints `31%`, the model `31.5%`   |
| reference (`ref`, `in_refrow`) | shared                 | the model's reference level, already true                    |
| `color_signif` policy          | shared                 | already true                                                 |
| `ci_method` (the key)          | stays per column       | it names the closed form evaluated; see the measurements below |
| the interval *arithmetic*      | **the same** except on gaussian / poisson | measured identical on every other family |
| `conf_level`, `degf`, `basis`  | shared                 | already true                                                 |
| the interval itself            | different              | that is the point of the comparison                          |

`ci_method` is the one row that is **not** simply shared — but the reason is narrower than it looks,
and it decides how much the legend has to change, so it is worth measuring rather than asserting.
**The maintainer's precision is correct and my first reading of this was wrong:** the closed forms were
built to reproduce the univariable model's interval, and for most families they do so exactly.

Measured against hand-run univariable fits (`gss_cat`, n = 6 803 complete cases, `race` as the
predictor; largest relative difference over both bounds):

| crude column | declared engine | compared with | max rel. difference |
|---|---|---|---|
| `Obs_OR` | `woolf` | `glm(binomial)`, Wald on the log OR | **3e-13 — identical** |
| `Obs_RR` | `katz` | modified Poisson, HC0 sandwich | **8e-09 — identical** |
| `Obs_diff`, binary predictor | `student` | `lm()`, t with residual df | **6e-15 — identical** |
| `Obs_diff`, 3-level predictor | `student` | `lm()`, t with residual df | **8.9e-02 — differs** |
| `Obs_diff`, 3-level predictor | `student` | two-sample pooled t, per pair | **2e-16 — identical** |
| `Obs_IRR`, binary predictor | `katz` | `glm(quasipoisson)` | 4e-06 |
| `Obs_IRR`, binary predictor | `katz` | `glm(poisson)` | 1.2e-02 |
| `Obs_IRR`, 3-level predictor | `katz` | `glm(quasipoisson)` | 2.0e-03 |

What that establishes:

- **On the probability families the crude interval IS the univariable model's Wald interval on the
  link scale, to machine precision.** Woolf is not a rival engine — it is the closed form of the
  saturated logistic model's Wald-on-log interval, which is what `CI_METHOD_LABELS` already asserts
  by rendering `woolf` as *"Wald interval on the log odds-ratio"*. Katz stands in the same relation to
  the modified Poisson: for a saturated two-group fit the robust sandwich SE of the log risk ratio
  reduces algebraically to Katz's, and it does, to eight decimal places. Same for the multinomial
  Woolf ORs, and **by construction** for every `from = "fit"` shape (ordinal, numeric predictors, the
  marginal shapes), which are literal univariable `reg_fit()` calls.
- **So `katz` versus `wald_log` in the legend is a labelling difference over identical arithmetic**,
  not the honest divergence I first took it for. §5.5's recommendation changes accordingly, and
  shrinks: the two columns can share one method phrase, and no change to the legend's grouping key is
  needed.
- **The two moment families are the real exception, and the cause is not the engine but the variance
  pooling scope.** `Obs_diff` is the *pairwise* pooled-variance t interval — only the two levels being
  compared, df = n1 + n2 - 2. For a 2-level predictor that IS the univariable `lm` coefficient
  interval, exactly. For a k-level predictor `lm` pools one residual variance across all k levels, so
  the two diverge: measured **8.9 %** on the `Other` contrast, because the levels are strongly
  heteroscedastic (SD of `tvhours`: White 2.32, Black 3.51, Other 2.41). `Obs_IRR` has the same shape
  one family over — a per-pair Katz variance against one global dispersion — agreeing with
  quasi-Poisson to five significant digits rather than exactly.

⚠ **A documentation defect falls out of this.** The regression vignette says of the gaussian crude
column: *"student : confidence intervals with pooled variance, to match those computed by linear
regression"*. True for a binary predictor, false for a k-level one by up to 8.9 % — the column pools
pairwise, not globally. Either the claim or the engine has to change; see Q7 in §8.

So the shared/not-shared line moves: **the measure, the ladder, the digits, the display, the legend
block and — on every family but the two moment ones — the interval arithmetic itself are all one.**
What stays per-column is only the declared `ci_method` key, because it names the closed form actually
evaluated, and the residual pooling-scope difference on `gaussian` / `poisson`.

### 4.3 K3 — `{est}` already exists internally; exposing it unlocks family-agnostic display

The `est_ci` token is the only field-agnostic token in the grammar. It reads
`fmt_center_field(x) <- fmt_scale_row(x)$est_field` (`R/fmt_class.R:1726`), i.e. the field the
column's own scale declares:

| scale                                  | `est_field`          | which estimands                            |
|----------------------------------------|----------------------|--------------------------------------------|
| `odds_ratio`                           | `or`                 | OR, RR, IRR, cumOR, multinomial vs-rest OR |
| `mean_ratio` / `pct_ratio`             | `ratio`              | RoM                                        |
| `raw_diff` / `mean_diff`               | `diff`               | gaussian coefficient, count AME            |
| `points`                               | `diff`               | RD, probability-scale AME/MER              |
| `log_coef`                             | `diff`               | every `measure = "log"`                    |
| `level_pct` / `level_mean` / `level_n` | `pct` / `mean` / `n` | crosstab levels                            |

So `{est}` is a `DISPLAY_TOKENS` row with `field = NA` and a `center = TRUE` flag, resolved by the
function that already exists. `get_num()` / `set_num()` already have the matching arms (`set_num()`
switches on `fmt_center_field()`). This is a genuinely small addition with a large payoff: every
preset stops being family-specific, and — because the crude column's `scale` is the model's scale —
`{est}` on the crude column means, exactly as the maintainer put it, *the same field the model
estimates, in its crude version*, with no extra rule.

The second slot needs a token too. `{pct}` and `{mean}` exist but are family-specific, which is what
makes `display = "prob"` binomial-only. `{base}` resolves to `pct` or `mean` by the column's
`var_kind` — the same one-line dispatch as `{est}`, on the other axis of `EST_SCALES`.

**Why the token is `{base}` and not `{num}`, and what it drags with it.** `num` was the first
candidate, but the exported `get_num()` / `set_num()` mean "the currently *displayed* field" and are
**published** — they are in `master`'s NAMESPACE and taught in the README with that exact wording — so
`{num}` would ship two meanings of "num" in one user surface. `base` is instead the word the code
already uses for this concept: `REG_EMPIRICAL$*$base` is the descriptive column being merged away, and
`R/fmt_class.R:830` already spells the `num_ci` composite as `"{base} {ci}"`. Promoting it costs
nothing and removes a synonym.

Two renames follow, and both are free because neither name is published:

- **`display = "num_ci"` -> `"base_ci"`** — 28 sites across 9 files, including the generated
  `R/jmvtab.h.R` (so one `jmvtools::prepare()`) and two `man/*.Rd`. Verified: `num_ci` is **absent from
  `master`**, so it is 2.0.0-only, and it is the **only** `num`-flavoured display preset — the other
  literal `"num"` strings in `R/` are `MEASURES$applies_to` values meaning "a numeric column", a
  different axis, left alone.
- **the `pct_base` column attribute must be renamed**, or the surface ships `{base}` (the percentage
  itself) beside `pct_base` (which axis the percentage is *of*). Also free: **`pct_base` does not exist
  on `master` at all** — 1.3.1 called this attribute `type`, and 2.0.0 split `type` into `scale` +
  `pct_base`. 205 sites, plus the exported `get_pct_base()` / `set_pct_base()` and the `PCT_BASES`
  constant; all mechanical.

The new name is **`pct_type`** — "which type of percentage": `row` / `col` / `all` / `all_tabs` /
`none`. The qualifier is what makes it safe: it cannot be read as 1.x's bare `type`, which conflated
"what kind of number" (`"n"`, `"mean"`) with "a percentage of what" (`"row"`, `"col"`), and which
2.0.0 split into `scale` + this attribute. One consequence to handle while renaming: the live
deprecation abort at `R/fmt_class.R:457` currently reads *"`type` is now `scale` + `pct_base`"*, and
mechanically substituting the new name would make it read *"`type` is now `scale` + `pct_type`"* —
which a 1.x user could mistake for a no-op. Reword it so it names the split rather than the tokens,
e.g. *"`type` split in two: `scale` (what the column estimates) + `pct_type` (which kind of
percentage: row / col / all)"*. Same for the matching `NEWS.md` bullet.

### 4.4 K4 — the marginal quantities are cheap and dependency-free: populate always

**First, the review's reading of the two paths is exactly right, and it is worth writing down because
the whole proposal rests on it.** Verified in the code:

|                                  | `effect = "coefficient"`, `display` folds an AME | `effect = "marginal"`       |
|----------------------------------|--------------------------------------------------|-----------------------------|
| what is fitted                   | the family's own model (logit)                   | the same model              |
| the reported estimate            | the odds ratio                                   | the average marginal effect |
| interval, p-value, stars, colour | the **odds ratio's**                             | recomputed **for the AME**  |
| the folded number                | the AME point estimate, `want_se = FALSE`        | the adjusted prediction     |
| cost                             | one g-computation sweep, no delta method         | sweep + influence functions |

So on the coefficient path the AME is an *interpretation aid* riding a column whose inference belongs
to the odds ratio; on the marginal path the AME *is* the estimand and carries its own inference. Both
are legitimate and they answer different questions — the first says "here is the OR, and here is what
it means in percentage points", the second says "the percentage-point effect is the result".

The consequence the review draws is also right: `display = "ame"` is a badly named preset, because it
sounds like it changes the estimand when it only changes what is printed. Which field to print is
`{est}` versus `{base}`; which estimand to report is `effect`.

The review's hypothesis — "since it does not need its own SE/CI/stars it's very cheap to compute,
right?" — is correct, and measurably so. Median of three warm runs, 21 483 rows, 4 predictors,
logistic:

| call                                                           | time   |
|----------------------------------------------------------------|--------|
| `empirical = TRUE`, `display = "value"` (no fold)              | 0.32 s |
| `empirical = TRUE`, `display = "prob"` (adjusted predictions)  | 0.36 s |
| `empirical = TRUE`, `display = "ame"` (AME point estimates)    | 0.28 s |
| `empirical = TRUE`, `display = "{or} ({diff}) ({pct})"` (both) | 0.31 s |
| no `empirical`, `display = "value"`                            | 0.12 s |
| no `empirical`, `display = "ame"`                              | 0.23 s |

The fold costs about **0.1 s** on the bare path and is **inside the noise** once `empirical = TRUE`.
The reason is that `reg_apply_display()` passes `want_se = FALSE`: a point-estimate g-computation
sweep, no delta method, no influence functions. And the engine is `gcomp`, not `marginaleffects`
(`reg_marginal_engine()`), so it adds no dependency — **provided defect §3.6.1 is fixed**, otherwise
always-populating would drag the `marginaleffects` abort onto every table.

So the review's conclusion holds: populate `pct`/`mean` (the adjusted prediction) and `diff` (the
additive marginal effect) on **every** model column, always, exactly as `tab()` always fills both
`diff` and `ratio`. Consequences:

- `display` becomes a **pure post-hoc property**: `set_display()` on a built table works, jamovi's
  tier-3 repaint stops needing a refit, and `display` never changes a number (the invariant `tab_reg`
  already claims but cannot currently keep, since asking for a fold triggers a computation).
- the tooltips can print the mirror quantities unconditionally (§4.7);
- `display = "ame"` stops being a meaningful preset name, exactly as the review says: the choice is
  not "marginal or not" (that is `effect`), it is "which field" — `{est}` or `{base}`.

⚠ Two limits to state, because "always" is not quite always:

- **The ratio-scale marginal effect is not free.** `pct` (a prediction) and `diff` (an additive
  contrast) come from one sweep. A *marginal ratio* is a third quantity, and on a column whose
  estimate is already a ratio it is the estimate itself. There is no case where a fourth field is
  needed: the ratio of two adjusted predictions is recoverable from `pct` at display time if anyone
  ever wants it in a tooltip.
- **Numeric predictors have no adjusted prediction.** `marginaleffects` / `gcomp` return predictions
  per factor level only. So `{base}` is void on a numeric-predictor row, and the per-row-kind display
  fold must fall back to `{est}` there — which is precisely what the marginal path already does today
  (its stored display is `blank / ({pct}) / {diff} ({pct}) / diff`, the fourth variant being the
  numeric rows).

### 4.5 K5 — the header names the measure; the contrast is a marker on the measure

Settled with the maintainer this session. The header word is the **measure**, carrying a marker for
the contrast:

- `effect = "coefficient"` — unmarked. Conditional is the default reading, as in the literature
  (nobody writes `cOR` unless contrasting).
- `effect = "marginal"` — a lower-case `m` prefix on the measure: `mRR`, `mRD`, `mRoM`. This is
  standard epidemiological notation for the marginal (standardised, g-computed) contrast.
- `effect = "at_reference"` — an `@ref` suffix: `RR@ref`, `RD@ref`.
- `measure = "log"` — `log(X)`, naming what is logged, mirroring the crude side's existing
  `Obs_log(OR)`.

The marker rides the **measure**, not the word `Model`, so `Model_` stays a constant, visually
ignorable prefix and the varying part is the acronym a reader can look up. The full grid is in §5.4.

One exception, which the codebase already handles: when the outcome has several categories the header
slot is taken by the category contrast (`"2-Catholic vs 1-Protestant"`), and the measure lives in the
`col_var` span — measured today as `"relig: OR"`. So for per-category tables the marker goes in the
span: `"relig: mRR"`, `"relig: OR@ref"`. §5.4 makes this the uniform rule rather than a multinomial
accident, which also gives the exporters a place to name the measure once above a `Obs` / `Model`
column pair.

### 4.6 K6 — under a gap measure the crude column *is* the reference column

The review's Phase 22b observation — *"with `color = "adjustment"`, the colour of the `Obs_RR` column
is misleading, since it's the reference for comparison and should be all bold / no colours. Use the
reference detection and reference management subsystem"* — has an exact and almost free
implementation, and it is worth stating here because it is a consequence of K2 rather than a separate
feature.

If the crude column takes the **model column's** colour measure (K2), then under
`color = "adjustment"` the crude column's measure is `adjustment` too. Its `obs` field is empty (a
crude column has no crude counterpart), so `fmt_adjustment_score()` returns `NA` everywhere and the
column is **uncoloured by construction** — no gate, no exception. And the legend already has a word
for that state: `legend_gap_baseline()` (`R/fmt_class.R:4065`), built for the `between_groups`
reference group.

Verified by experiment — writing `"adjustment"` into the crude columns' `color` attribute of a built
table and re-rendering the legend:

```text
before:
# Obs_%: difference (ref.): -30 -20 -10 -5 +5 +10 +20 +30 [grey: ... under +/-5 points]
# Obs_RR: RR (ref.): 1/4 1/2 1/1.5 1/1.2 1.2 1.5 2 4 [grey: ... under x1.2]
# Model_RR (adjusted %): adjustment: /2 /1.5 /1.25 /1.1 x1.1 x1.25 x1.5 x2 [grey: ...]

after:
# Obs_%, Obs_RR: adjustment: no observed effect
# Model_RR (adjusted %): adjustment: /2 /1.5 /1.25 /1.1 x1.1 x1.25 x1.5 x2 [grey: ...]
```

Three ladders became one ladder plus one baseline note, with a one-attribute change and no new
machinery. Two refinements remain:

- the baseline word is `"no observed effect"`, which is right for the `between_groups` reference group
  but wrong here — on the crude column it should read *"the observed effect (the reference for the
  adjustment)"*. One `role == "emp"` arm in `legend_gap_baseline_word()`.
- **bold.** `get_reference()` (`R/fmt_class.R:5135-5180`) is the anchor mask that bolds reference
  cells. Its first branch tests `measure_key(color) == "odds_ratio"`; a gap measure falls through to
  the `ref`-based branches, which know about `refcol` in some arms only. The `refcol` **attribute
  already exists** on every `fmt` column (`R/fmt_class.R:2023`) and has no consumer outside the
  legacy step API. So: set `refcol = TRUE` on the crude column, and give `get_reference()` a declared
  arm for gap measures returning `rep(refcol, n)`. That is the review's "use the reference management
  subsystem" taken literally — one declared arm, not a special case.

---

## 5. Part III — the target design

### 5.1 The column model

Decided this session: **two columns by default, with an opt-in fold to one.**

```text
empirical = TRUE                       (default when asked for)
  var      levels     Obs_RR             Model_RR
  race     White      1        (52%)     1        (51.3%)
  race     Black      1/1.69*** (31%)    1/1.63*** (31.5%)

empirical = "cell"                     (folds the crude into the model cell)
  var      levels     Model_RR
  race     White      1        (51.3%)
  race     Black      1/1.63*** (1/1.69)
```

The fold mode is **not new**: it is exactly what multinomial does today
(`visible = FALSE` + `{or} ({obs})`, ruling Q4 of `dev/model_vs_observed_gap_test.md`). Making it a
value of `empirical` rather than a per-family flag **deletes** that special case: multinomial and
ordinal-marginal tables simply *default* to `"cell"` because they would otherwise draw one crude
column per outcome category. Same for `predictors = list(...)` with many models, where every model
column shares one crude effect.

So `empirical` becomes: `FALSE` (default) | `TRUE` (a crude column) | `"cell"` (folded in-cell), with
`TRUE` silently resolving to `"cell"` where a per-category column set would otherwise multiply. One
message says so.

⚠ A redundancy to keep deliberately. In two-column mode the crude effect is stored **twice**: as the
crude column's own estimate field, and as the model column's `obs`. That is not waste — the colour
engine reads one column and never the table
(`dev/model_vs_observed_effect_colour.md`, rejected option C), so `color = "adjustment"` needs the
crude value *inside* the model column. Worth a `# DESIGN:` tag so nobody "optimises" it away.

### 5.2 `REG_EMPIRICAL` after the merge

Today: 8 families x up to 4 shape rows, each with `nm`, `scale`, `display`, `digits`, `ref`,
`pct_base`, `ci_method`, **`color`**, `link`, and optionally `visible` / `per_category` / `from`.
The `base` rows exist only to draw the descriptive column.

After: the `base` rows **disappear as rows** and become two facts on the effect row — which field
holds the crude level, and with which interval. Sketch (binomial; the other families follow):

```r
binomial = list(
  method_diff = "wald", coef = "or", coef_log = "or_log",
  or     = list(nm = "Obs_OR",      scale = "odds_ratio", digits = 2L, ref = "1",
                pct_base = "row", ci_method = "woolf",
                base_field = "pct", base_ci_method = "wald", base_digits = 1L,
                link = "logit"),
  rd     = list(nm = "Obs_RD",      scale = "points",     digits = 1L, ref = "tot",
                pct_base = "row", ci_method = "wald",
                base_field = "pct", base_ci_method = "wald", base_digits = 1L,
                link = "identity"),
  or_log = list(nm = "Obs_log(OR)", scale = "log_coef",   digits = 2L, ref = NA_character_,
                pct_base = "none", ci_method = "woolf",
                base_field = "pct", base_ci_method = "wald", base_digits = 1L,
                link = "logit"))
```

What changes, and why each is a simplification rather than a move:

- **`color` is deleted from every shape row.** The crude column takes the model column's measure
  (K2). Fourteen declared colour values disappear, and with them the two-ladder problem, for every
  family at once. `zzz-fact-keys.R` loses the `REG_EMPIRICAL$color -> MEASURES` foreign-key edge.
- **`display` is deleted from every shape row.** The template comes from the user's `display`,
  resolved once for the whole table and applied to both columns.
- **`base_field` replaces the `base` row.** It says which level quantity rides in the crude cell:
  `pct` for probability families, `mean` for gaussian / poisson / grouped-binomial. `NA` for ordinal,
  which genuinely has no single share beside a cumulative odds ratio — and that is now expressed as
  one `NA` instead of "this family has no `base` row".
- **`digits` splits into `digits` / `base_digits`,** which is what fixes the measured `31%` versus
  `31.5%` mismatch: both columns take the same pair.
- **`reg_empirical_columns()`'s `emit()` emits one column, not two.** It already computes both halves
  of the grid (`emp_prop` / `emp_mean` and `emp_diff` / `emp_ratio`); it currently splits them across
  two `fmt()` calls with disjoint field sets. One call with the union is strictly less code.
- **`shape_visible()` disappears**; visibility is now `empirical`'s value, table-wide.

The crude column's field set becomes the mirror of the model column's:

| field                                  | crude column                 | model column                 |
|----------------------------------------|------------------------------|------------------------------|
| the estimate (`or` / `ratio` / `diff`) | crude effect                 | adjusted effect              |
| `pct` or `mean`                        | observed level               | adjusted prediction          |
| `ci_inf` / `ci_sup` / `pvalue`         | crude interval               | model interval               |
| `n`                                    | count behind the level       | (see Phase 22b-ii)           |
| `obs`                                  | empty (it *is* the observed) | the crude effect             |
| `gap_se`                               | empty                        | the gap SE where collapsible |

### 5.3 The display grammar for `tab_reg()`

**Two new tokens**, both one-line dispatches on the column's own scale:

| token   | resolves to                     | on a model column       | on a crude column  |
|---------|---------------------------------|-------------------------|--------------------|
| `{est}` | `EST_SCALES[[scale]]$est_field` | the adjusted effect     | the crude effect   |
| `{base}`| `pct` or `mean` per `var_kind`  | the adjusted prediction | the observed level |

`{obs}`, `{ci}`, `{n}` keep their meaning. `{est}` supersedes the preset name `"value"`, which stops
being a magic non-token (and the `allow = "value"` exemption in `R/zzz-fact-keys.R:168` goes away).

**Presets** — the full set, all of which now work on every family and both columns:

| preset              | template         | reads as                                                 |
|---------------------|------------------|----------------------------------------------------------|
| `"est"` *(default)* | `{est}`          | the effect                                               |
| `"est_ci"`          | `{est} ({ci})` ? | the effect with a visible interval (verify template)     |
| `"est_base"`        | `{est} ({base})` | the effect, with the adjusted / observed level beside it |
| `"base_est"`        | `{base} ({est})` | the level, coloured and starred by the effect            |
| `"base"`            | `{base}`         | adjusted percentages / adjusted means, tout court        |

`"value"`, `"prob"` and `"ame"` are deleted (no back-compatibility is owed on `tab_reg()`), and the
jamovi ComboBox (`R/jmvtabreg.h.R:298-306`) regenerates from the same list.

**Duplicating a column with a second display**, which the review asks to keep possible, becomes a
one-liner once every field is populated (K4) and `{est}` / `{base}` are family-agnostic — no rebuild,
no second fit, and the copy keeps its colour because the measure lives on the column. No color, no legend. 
Teach it in regression vignette :

```r
t <- tab_reg(gss_simple, "married", c("race", "rincome", "age"), family = "binomial", empirical = TRUE)
t |> dplyr::mutate(Model_pct = set_display(Model_OR, "{base}") |> set_color(""), .after = Model_OR)
```
**Maintainer’s request: we removed the color, we should find a direct and clear way to remove the stars too.**

Verified today with `"{pct}"` in place of the not-yet-existing `"{base}"` (the fold had to be requested
at build time, which K4 removes):

```text
  var      levels                 n `Obs_%`    Obs_OR         Model_OR Model_pct
2 race     White             16 323  51%         1    1.00    (50.87%) 50.87%
3 race     Black              3 110  28%*** 1/2.67*** 0.35*** (26.72%) 26.72%***
4 race     Other              1 942  48%*** 1/1.13*** 0.91*   (48.56%) 48.56%*

# Obs_OR, Model_OR, Model_pct: OR (ref.): 1/4 1/2 1/1.5 1/1.2 1.2 1.5 2 4 [grey: ...]
```

This is the same recipe `?tabxplor-vctrs` already teaches for crosstabs, and it is the reason
`display` must never be the thing that triggers a computation.

Naming alternatives for the two compound presets, if `est_base` / `base_est` read too cryptically:
`"effect"` / `"level"`; `"with_level"` / `"level_first"`; `"est+num"` / `"num+est"`. My preference is
`est_base` / `base_est` because the order of the words *is* the order in the cell, which is the one
thing a user needs to predict.

**The per-row-kind fold stays**, and generalises. The stored `display` field is already per cell, and
the marginal path already writes four variants (`blank`, `({pct})`, `{diff} ({pct})`, `diff`). The
rule, stated once instead of per builder:

| row kind                  | template                                                          |
|---------------------------|-------------------------------------------------------------------|
| `Constant` / out-of-model | `blank`                                                           |
| reference level           | the template with `{est}` dropped — there is no effect to show    |
| a factor level            | the template as written                                           |
| a numeric predictor       | the template with `{base}` dropped — no adjusted prediction exists |

**And the multiplicative rendering unifies.** One rule, replacing three:

> A cell whose scale declares `mult = TRUE` prints the inverse form (`1/x.xx`) below the neutral,
> in every rendering path — bare token, composite, and `est_ci` — unless the global option
> `tabxplor.inverse_ratios = FALSE`, in which case it prints `0.xx`.

That requires the composite expander to stop passing `special_formatting = FALSE`
(`R/fmt_class.R:3116`) for the inversion specifically, and the `ratio` token's `÷ ×` glyphs to become
the *legend's* notation rather than the *cell's* — or the two notations to be reconciled explicitly.
This is Phase 22b work, listed here because §5.3's presets are unreadable without it: `"est_base"` on
a ratio column is exactly the composite that loses the inverse today.

### 5.4 The header vocabulary

Full grid. `Obs_` and `Model_` prefixes shown once; the marker rides the measure.

| outcome kind         | `family`            | `measure`             | `coefficient` | `marginal` | `at_reference`         |
|----------------------|---------------------|-----------------------|---------------|------------|------------------------|
| numeric              | gaussian            | `difference` *(def.)* | `diff`        | `mdiff`    | `diff@ref`             |
| numeric              | gaussian            | `ratio`               | `RoM`         | `mRoM`     | `RoM@ref`              |
| numeric              | gaussian            | `log`                 | `log(RoM)`    | —          | —                      |
| numeric              | gaussian            | `odds_ratio`          | not defined   | —          | —                      |
| numeric (count)      | poisson             | `ratio` *(def.)*      | `IRR`         | `mRoM`     | `RoM@ref`              |
| numeric (count)      | poisson             | `difference`          | —             | `mdiff`    | `diff@ref`             |
| numeric (count)      | poisson             | `log`                 | `log(IRR)`    | —          | —                      |
| numeric (0..k score) | binomial + `trials` | `odds_ratio` *(def.)* | `OR`          | —          | —                      |
| numeric (0..k score) | binomial + `trials` | `ratio`               | `RR`          | `mRR`      | `RR@ref`               |
| numeric (0..k score) | binomial + `trials` | `difference`          | `RD`          | `mRD`      | `RD@ref`               |
| factor, 2 levels     | binomial            | `odds_ratio` *(def.)* | `OR`          | —          | —                      |
| factor, 2 levels     | binomial            | `ratio`               | `RR`          | `mRR`      | `RR@ref`               |
| factor, 2 levels     | binomial            | `difference`          | `RD`          | `mRD`      | `RD@ref`               |
| factor, 2 levels     | binomial            | `log`                 | `log(OR)`     | —          | —                      |
| factor, 3+ unordered | multinomial         | `odds_ratio` *(def.)* | `OR`          | —          | `OR@ref` (vs the rest) |
| factor, 3+ unordered | multinomial         | `ratio`               | —             | `mRR`      | `RR@ref`               |
| factor, 3+ unordered | multinomial         | `difference`          | —             | `mRD`      | `RD@ref`               |
| factor, 3+ unordered | multinomial         | `log`                 | `log(OR)`     | —          | —                      |
| factor, ordered      | ordinal             | `odds_ratio` *(def.)* | `cumOR`       | —          | —                      |
| factor, ordered      | ordinal             | `ratio`               | —             | `mRR`      | `RR@ref`               |
| factor, ordered      | ordinal             | `difference`          | —             | `mRD`      | `RD@ref`               |
| factor, ordered      | ordinal             | `log`                 | `log(cumOR)`  | —          | —                      |

Notes on the grid, answering the review's questions:

- **`poisson` is numeric only.** A binary outcome with `family = "poisson"` is rewritten to
  `binomial` × `ratio` (the modified Poisson) with a message — verified. So the first column is not
  decorative: it tells the reader which outcomes reach which row, and it is where the summed-score
  syntax (`trials =`, the `grouped_binomial` internal family) finally gets a place in the table. It
  has none today.
- **ordinal is `cumOR`, not `OR`** — as the maintainer noted mid-session, and as the crude side has
  always said (`Obs_cumOR`). Confirmed by capture: today the table really does print `Obs_cumOR`
  beside `Model_OR`.
- **`measure = "log"` gets four proper rows** instead of a footnote, and stops colliding: `log(OR)`,
  `log(IRR)`, `log(RoM)`, `log(cumOR)` replace the single overloaded `β`.
- **gaussian × `difference` × `coefficient`.** The review asks whether mirroring the crude side is
  meaningful here. It is, and there is a decisive argument for `diff` over `coeff`: **the header word
  must be able to take the marginal marker.** `mRR` and `mRD` work because `RR` and `RD` name
  *measures*; `mcoeff` is nonsense because `coeff` names an *estimator*, and a marginal effect is not
  a coefficient. So the choice is between `diff` / `mdiff` (plain, mirrors `Obs_diff`, no acronym to
  learn) and `MD` / `mMD` (completes the acronym family — MD = mean difference, standard in
  meta-analysis and epidemiology — but adds a fifth acronym and reads awkwardly with the prefix).
  I recommend **`diff` / `mdiff`**: it reuses vocabulary tabxplor already teaches everywhere
  (`{diff}`, `color = "diff"`, "difference" in the legend), and the footer still says "coefficients
  (mean difference vs the reference category)" for anyone looking for the word. Note this supersedes
  Phase 22b's `Model_coeff` request — the reason is the marker, not preference, so it needs a
  maintainer decision.

**For the vignette table**, as requested: drop the `Model_` prefix and give the meaning beside the
acronym, since the prefix is constant and the acronym is the thing being taught.

| outcome kind     | `coefficient`                 | `marginal`                       | `at_reference`                             |
|------------------|-------------------------------|----------------------------------|--------------------------------------------|
| factor, 2 levels | `OR` odds ratio               | —                                | —                                          |
| factor, 2 levels | `RR` risk ratio               | `mRR` marginal risk ratio        | `RR@ref` risk ratio at the ref. profile    |
| factor, 2 levels | `RD` risk difference          | `mRD` marginal risk difference   | `RD@ref` risk diff. at the ref. profile    |
| factor, ordered  | `cumOR` cumulative odds ratio | —                                | —                                          |
| numeric          | `diff` mean difference        | `mdiff` marginal mean difference | `diff@ref` mean diff. at the ref. profile  |
| numeric          | `RoM` ratio of means          | `mRoM` marginal ratio of means   | `RoM@ref` ratio of means at the ref. prof. |
| numeric (count)  | `IRR` incidence-rate ratio    | —                                | —                                          |

Read the outcome-kind column as "which outcomes reach this row": a 2-level factor and a summed score
(`trials =`) both reach the binomial rows; an ordered factor only the ordinal ones; a numeric outcome
the gaussian and poisson ones. A **binary outcome with `family = "poisson"` never reaches the count
rows** — it is rewritten to `binomial` × `ratio` with a message, so `IRR` is unreachable there.

**And the `reg_measures()` / abort message.** Today:

```text
i A "binomial" outcome offers:
i `effect = "marginal", measure = "difference"` -> "AME"
i `effect = "marginal", measure = "ratio"` -> "RR"
```

which is the review's complaint: `AME` has no expansion and `RR` does not say it is marginal. With
the new headers the acronym already carries the contrast, so the message only has to expand it:

```text
i A "binomial" outcome offers, with `effect = "marginal"`:
i   measure = "difference"  ->  mRD, the marginal risk difference
i                               (average marginal effect on the probability scale, in points)
i   measure = "ratio"       ->  mRR, the marginal risk ratio
i                               (ratio of the adjusted predicted probabilities, sample-averaged)
```

The expansion is a new `long` column on the `REG_ESTIMANDS` row — one declared string per row, read by
the abort message, by `reg_measures()`, by the generated `?tab_reg` section and by the footer. Today
the footer's `note` closure carries this prose and the message carries only `word`; they should be the
same fact.

### 5.5 Colour and legend

The colour model after the merge, stated as three rules:

1. **One measure per table region.** The crude column takes the model column's `color` attribute,
   both channels. `REG_EMPIRICAL$*$color` is deleted.
2. **A gap measure makes the crude column the baseline.** Its `obs` is empty, so it is uncoloured by
   construction; it is marked `refcol = TRUE` and bolded through `get_reference()`'s new declared arm
   (K6).
3. **`color = FALSE` still switches both off** — unchanged, and now automatic rather than the
   `emp_off` special case in `reg_empirical_columns()`.

What the legend needs. The measured behaviour is the good news: **the terse legend already merges**
`Obs_OR` with `Model_OR` — the grouping key is the rendered sentence, and once the two columns share
measure, ladder, policy and reference they render identically. So for the default case (`color = TRUE`)
the answer to the maintainer's mid-turn question is: **no legend change is needed**; deleting the
`base` column is what produces the single block.

Two cases still split the block, and after the §4.2 measurements only **one** of them is a real
difference:

- **The CI-method phrase, in `prose` style — a labelling defect, not a divergence.** Measured today,
  `Obs_RR` renders *"Katz interval on the log risk-ratio"* and `Model_RR` *"Wald interval on the log
  risk-ratio"*, so the bodies differ and the block splits. But §4.2 shows the two intervals are the
  **same arithmetic to 8 decimal places**: Katz *is* the Wald interval on the log risk ratio of the
  saturated fit. `CI_METHOD_LABELS` already encodes this insight for `woolf` — which is why `Obs_OR`
  and `Model_OR` merge even in prose. So the fix is the same one, one row over: **label the crude
  effect column's interval from the estimand rather than from the internal engine key**, i.e. let
  `katz` render as *"Wald interval on the log risk-ratio"*. `"Katz"` must be retained parenthetically, with something like "(equal to Katz interval for the observed column)". The bodies then coincide and the block merges with **no change to
  `legend_group_by_body()` at all** — strictly smaller than the grouping-key change I first proposed.

  The exception is `gaussian` / `poisson`, where the arithmetic genuinely differs (pairwise pooling
  versus global). There the two phrases are honest and the block should split — or Q7 removes the
  difference at its source by switching those two crude effect columns to `from = "fit"`.

- **The two-channel case `color = c(TRUE, "adjustment")`.** The model column gains a background clause
  the crude column cannot have (it is the baseline), so the bodies differ. This one is not a labelling
  artefact. Two honest options: emit the background clause once inside a shared block, naming the
  column it applies to and the baseline —

  ```text
  Obs_RR, Model_RR -- Shades of blue: RR >= ... [the shared text ladder]
    Background colour (adjustment), on Model_RR only: RR >= the observed (crude) effect
    x1.1; x1.25; x1.5; x2. ... Obs_RR is the observed effect the background compares to.
  ```

  — which does need the ladder-identity grouping key; or keep two blocks with the baseline note, which
  ruling D8 of `dev/reg_comparison_framework_stress_test.md` ("honest legend wording only") already
  points at and which needs no code change. Worth deciding on real output rather than in the abstract
  (Q3).


One more wording fix falls out: `legend_gap_baseline_word()` needs a `role == "emp"` arm so the crude
column reads *"the observed effect (the reference for the adjustment)"* rather than *"no observed
effect"*.

**Stars versus colour.** The review asks whether it is acceptable that, under
`color = "adjustment"`, a cell's stars test the effect while its colour grades the gap — the measured
`Obs_RR 1.18***` / `Model_RR 1.18***` in grey. This was already ruled on and should not be reopened:
`fmt_stars_applicable` deliberately excludes only `contrib`, stars belong to the quantity the cell
*prints*, and the legend is required to name what the significance is *of*
(`dev/model_vs_observed_gap_test.md`, and the canonical four-row teaching table in
`dev/reg_comparison_framework_stress_test.md` §5). Two things this redesign adds in its favour:

- with **one** crude column and **one** model column the reader has two numbers and one ladder rather
  than three numbers and three ladders, so "the colour grades the move from left to right" becomes
  visually obvious in a way it cannot be today;
- with the crude column bolded as the baseline (K6), the table *shows* which column the colour
  compares to, instead of asserting it in the footer.

So: keep stars on the estimate, document it with the four-row table in the main vignette text, and let
the layout carry the explanation. No third significance channel.

### 5.6 Tooltips

The contract, stated once: **a tooltip shows what its own cell does not print, and mirrors its
counterpart.** Applied to the two columns:

| line                        | crude column                                          | model column                                           |
|-----------------------------|-------------------------------------------------------|--------------------------------------------------------|
| the estimate + interval + p | crude effect, its own interval and p                  | adjusted effect, its own interval and p                |
| the level                   | observed `%` / mean (+ its interval, + SD for a mean) | adjusted prediction                                    |
| the counterpart             | — (it *is* the observed)                              | `obs`: the crude effect                                |
| the gap                     | —                                                     | gap, its interval and p, where a `gap_se` exists       |
| `n`                         | the count behind the level                            | the same count (see Phase 22b-ii)                      |
| the additive twin           | —                                                     | the marginal `diff` when the cell prints a ratio, etc. |

Each line suppressed where the cell already prints it — the existing `shows()` mechanism.

What must be **fixed** to get there, in order of importance:

1. **Ratio columns must get their interval and p-value in the tooltip.** Today they get neither
   (§3.5): `out_ci` is level-columns-only and `out_diff`'s fold excludes `est_field == "or"`. This is
   the single largest tooltip gap and it hits the default column of every logistic table. The fix is a
   generic "estimate + interval + p" fragment driven by `fmt_center_field()` — the same dispatch as
   `{est}` — replacing the `out_diff` / `out_or` / `out_rr` / `out_ci` overlap.
2. **The gap line must use the scale's units.** `sprintf("%+.2f", score)` on a `points` column prints
   `-0.01` where the cell prints `-23.0%`. Read the unit from
   `measure_facts()`'s `unit_kind` / the `by_scale` override, which already declares "points" / "SD" /
   none for the three additive gap scales.
3. **Drop what is now redundant.** With `{base}` in the display, the `out_pct` line is suppressed by
   `shows()` automatically. The `OR:` rider on a marginal column (`or_tip`) becomes the "additive
   twin" line above, and is now available in both directions rather than only ratio-on-additive.
4. **Keep the two build-time fragments.** `reg_empirical_tips()`'s numeric-predictor line
   (`age: mean 47.2 (SD 17.3); mean if yes 48.7, if no 45.8`) is genuinely useful and has no other
   home, and it is the *only* crude information a numeric row can carry. The multinomial `crude: 28%
   (+5 pts [...])` fragment becomes partly redundant once the fold prints `{est} ({obs})`, but its
   *interval* is not in the cell, so keep it and drop only its point estimate.

What should **not** go in: the model formula, the fit family, anything already in the footer, and the
crude column's `n` on the model column if Phase 22b-ii gives `n` its own column semantics.

### 5.7 What `measure` does not gain

For the record, so it is not re-proposed: **no `measure = "value"` / `"base"` / `"identity"`.**
Reasons, in order:

1. it would be a row that cannot colour, cannot be tested and cannot take the marginal marker (K1);
2. it would break the invariant that `measure` selects a fit — a prediction selects nothing;
3. the need it answers is already met by `display = "{base}"`, which is *more* capable (it keeps the
   effect's colour and stars, which a level-only estimand would have to invent);
4. it would put a level scale (`neutral = NA`, `break_key = NA`) in a slot every consumer reads as an
   effect scale.

Equally, **`effect` does not gain a `"prediction"` value.** It would be the same white elephant one
argument over.

---

## 6. Part IV — what disappears

The redesign is a net deletion. Inventory, so the implementation phase can be judged by it:

| deleted                                          | where                       | replaced by                                                 |
|--------------------------------------------------|-----------------------------|-------------------------------------------------------------|
| 8 `base` shape rows                              | `REG_EMPIRICAL`             | `base_field` + `base_ci_method` on the effect row             |
| 14 `color` declarations                          | `REG_EMPIRICAL`             | the model column's measure (K2)                             |
| 14 `display` declarations                        | `REG_EMPIRICAL`             | the user's `display`, applied to both columns               |
| `shape_visible()`, `visible = FALSE`             | `R/reg-empirical.R:658`     | `empirical = "cell"`, table-wide                            |
| the multinomial in-cell exception                | ruling Q4's per-family rule | the same `"cell"` mode                                      |
| `emp_off`                                        | `R/reg-empirical.R:739`     | `color = FALSE` propagating like any other measure          |
| `REG_DISPLAY_SHORTHANDS`' family-specific `{or}` | `R/tab_reg.R:1627`          | `{est}` / `{base}`                                           |
| `reg_display_folds()` + the binomial-only guard  | `R/tab_reg.R:1645`, `:1660` | fields always populated (K4)                                |
| the marginal-path `display` reset                | `R/reg-resolve.R:435-439`   | `display` applies everywhere                                |
| `"value"`'s foreign-key exemption                | `R/zzz-fact-keys.R:168`     | `{est}` is a real token                                     |
| `Model_β`'s five-way overload                    | `REG_ESTIMANDS$*$word`      | `diff` / `log(OR)` / `log(IRR)` / `log(RoM)` / `log(cumOR)` |
| the `(adjusted %)` header suffix                 | `reg_eff_word()`            | `{base}` in the display                                      |
| 2 of 3 multiplicative renderings                 | `format.tabxplor_fmt()`     | one `mult`-driven rule + one option                         |
| 4 overlapping tooltip fragments                  | `tab_kable_print_tooltip()` | one `fmt_center_field()`-driven fragment                    |

Two additions only: two `DISPLAY_TOKENS` rows, and one `long` column on `REG_ESTIMANDS`. Plus the
`get_reference()` gap arm, and — only for the two-channel legend case (Q3) — the
`legend_group_by_body()` grouping key.

And two mechanical renames, both on unpublished names (D7):

| renamed | sites | note |
|---------|-------|------|
| `display = "num_ci"` -> `"base_ci"` | 28, in 9 files | incl. the generated `jmvtab.h.R` (one `jmvtools::prepare()`) and 2 `man/*.Rd`; the only `num` preset |
| `pct_base` -> `pct_type` | 205 | plus `get_pct_base()` / `set_pct_base()` / `PCT_BASES`; absent from `master`, which called it `type` |

---

## 7. Part V — caveats, risks, and what I could not settle

Honest list. Some of these are arguments against parts of the proposal.

1. **The crude column loses its own descriptive stars.** Today `Obs_%` prints `31%***`, testing the
   crude risk difference with a Wald interval. After the merge the primary token is the effect, so the
   stars test the crude odds ratio with a Woolf interval. The two agree on the same 2x2 null and will
   differ only in borderline cells — but they *will* differ, and the goldens will move. This is
   arguably an improvement (the crude stars now test the same thing the model stars do), but it is a
   behaviour change to accept knowingly.
2. **`{base}` on a crude gaussian cell has nowhere to put the SD.** `Obs_mean` currently prints
   `2.36 (σ1.83)`; under `{est} ({base})` the parenthesis is taken by the mean, so the SD moves to the
   tooltip. Fine, but it is a real loss on the console for a user who was reading the dispersion.
3. **Two columns is not always narrower than three.** `{est} ({base})` makes each column wider. For a
   binomial table the net is clearly better (3 columns -> 2, and one ladder instead of two). For
   gaussian, where there is no two-ladder problem today, the merge saves a column but widens the
   remaining one — roughly neutral. Worth checking on a wide `tab_vars` table before committing.
4. **The `@ref` suffix in a column name.** `Model_RR@ref` contains a character that Excel sheet names,
   some Markdown pipelines and `make.names()` treat specially. `tab_xl()` derives sheet names from
   `reg_family_short()`, not from column names, so this is probably safe — but it must be verified on
   all four exporters before the name is locked.
5. **`mRD` versus `RD` in black-and-white print.** A single lower-case letter is the whole marker.
   Phase 22d's print theme drops colour entirely, so the header carries more weight there. If it
   proves too subtle, the fallback is the spelled-out `marg.` form, which was the third option
   considered.
6. **`empirical = "cell"` mixes two estimators in one cell with one interval.** The cell prints
   `1/1.63*** (1/1.69)` where the stars and the CI belong to the first number only. The multinomial
   path already does exactly this under ruling Q4, so it is precedent, not novelty — but generalising
   it makes it visible on the families where a two-column layout was available. The legend must say
   which number is tested.
7. **Ruling Q1(b) is untouched and must stay untouched.** The gap test is enabled only on collapsible
   scales; a conditional odds ratio gets descriptive colour and no test, because part of an OR gap is
   non-collapsibility rather than confounding (measured at +7.9 % with zero confounding by
   construction — the size of the first colour break). Nothing in this proposal changes that, and the
   merge must not accidentally give the crude column a `gap_se`.
8. **What I could not settle: whether the fold or the pair should be the default.** The decision taken
   is "pair by default, fold on request, fold automatically where a per-category column set would
   multiply". The automatic switch is a heuristic ("would this draw more than N crude columns?") and
   heuristics age badly. An explicit `empirical = "cell"` with a message on the wide families may be
   better than silence. Needs a look at real multinomial and multi-outcome tables.
9. **Phase interaction.** §5.3's multiplicative-rendering rule and §5.6's `n` line both overlap Phase
   22b / 22b-ii. The display grammar cannot be finished without the inverse-rendering fix, and the
   tooltip contract cannot be finished without knowing what the `n` column becomes. Suggested order:
   22b's rendering rule first (small, self-contained), then 22a's column merge, then 22b-ii's `n`.

---

## 8. Part VI — decisions and open questions

### Decided with the maintainer in this session

- **D1 — two columns by default, plus an opt-in in-cell fold.** The fold generalises today's
  multinomial rule and deletes it as a per-family special case.
- **D2 — the header names the measure, with the contrast as a marker on the measure.** Unmarked =
  conditional; an `m` prefix = marginal; an `@ref` suffix = at the reference profile. The marker rides
  the measure, not the word `Model`.
- **D3 — `measure = "log"` names what it logs**, mirroring the crude side: `log(OR)`, `log(IRR)`,
  `log(RoM)`, `log(cumOR)`. The five-way `Model_β` collision disappears.
- **D4 — ordinal is `cumOR`** on the model side as well as the crude one.
- **D5 — the display tokens are `{est}` and `{base}`.** `{est}` works on the crude column too, where it
  means the same field the model estimates, in its crude version. `{base}` was chosen over `{num}`
  because `get_num()` is published with a different meaning, and because `base` is already the code's
  word for this quantity — so it must work identically in `tab()` and in `tab_reg()`.
- **D7 — two free renames follow from D5**, neither name being published: `display = "num_ci"` becomes
  **`"base_ci"`** (28 sites, incl. the generated `jmvtab.h.R`; it is the only `num`-flavoured preset),
  and the `pct_base` attribute is renamed so it stops colliding with the `{base}` token (205 sites,
  plus `get_pct_base()` / `set_pct_base()` and `PCT_BASES`; `pct_base` is absent from `master`, which
  called this attribute `type`).
- **D8 — the new attribute name is `pct_type`** ("which type of percentage": row / col / all /
  all_tabs / none). The qualifier is what keeps it clear of 1.x's bare `type`, which conflated the
  kind of number with the percentage base and which 2.0.0 split into `scale` + this attribute. One
  follow-through: reword the `type` deprecation abort (`R/fmt_class.R:457`) and its `NEWS.md` bullet to
  name the split rather than the tokens, so the message does not read as a no-op after the rename.
- **D6 — the vignette grid prints the acronym with its meaning and drops the `Model_` prefix**, and
  gains a first column for the outcome kind — which is where the summed-score syntax finally appears.

### Maintainer’s answers to open questions

- **Q1 — gaussian × `difference` × `coefficient`:** `diff` / `mdiff`, or `MD` / `mMD`, or keep Phase
  22b's `coeff`? *Recommendation:* `diff` / `mdiff`. `coeff` names an estimator, not a measure, and so
  cannot take the marginal marker (`mcoeff` is nonsense). This supersedes 22b's `Model_coeff` request.
  **Maintainer’s decision: `diff` / `mdiff`**
- **Q2 — preset names:** `est_base` / `base_est`, or `effect` / `level`, or `with_level` /
  `level_first`? *Recommendation:* `est_base` / `base_est`, because the word order is the cell order —
  and they line up with `base_ci` (D7), giving one `base` family of presets.
  **Maintainer’s decision: `est_base` / `base_est`**
- **Q3 — the two-channel legend (`color = c(TRUE, "adjustment")`):** one shared block with the
  background clause scoped to the model column, or two blocks with a baseline note? *Recommendation:*
  decide on real output. Only this case needs the ladder-identity grouping key; the plain
  `color = TRUE` case merges once the `base` column is gone, and the `katz`/`wald_log` split is fixed
  by labelling from the estimand (§5.5) with no grouping change at all.
  **Maintainer’s decision: decide on real output, but one shared block seems *a priori* preferable if it’s achievable.**
- **Q7 — the gaussian and poisson crude intervals: pairwise closed form, or `from = "fit"`?** §4.2
  measured that these are the only two families where the crude interval is *not* the univariable
  model's — the closed form pools variance pairwise, `lm` / quasi-Poisson pool globally, and they
  diverge by up to **8.9 %** on a heteroscedastic 3-level predictor. Two honest resolutions:
  *(a)* switch those two crude **effect** columns to `from = "fit"` (a univariable `lm` / `glm`, which
  the architecture already supports and already uses for ordinal and numeric predictors) — then the
  ruling "the observed effect is the model's own effect fitted with one predictor" holds for the
  interval as well as the estimate, every family reports one method name, and §5.5's remaining split
  disappears; cost is one extra fit per predictor.
  *(b)* keep the pairwise interval — it makes no homoscedasticity assumption across levels, which is
  arguably the better descriptive choice, and the 8.9 % gap appeared precisely *because* that
  assumption fails — and correct the vignette's claim of parity with `lm`.
  *Recommendation:* **(a)**, because the crude column's whole promise is to be the model's own effect
  with one predictor, and the measurement shows it currently breaks that promise on exactly the
  families where a reader is most likely to compare the two numbers by eye. If the cost is unwelcome,
  (b) is defensible — but then the vignette sentence must go.
  **Maintainer’s decision: first try (c) find closed-forms matching the univariate model’s one on 3lv+ predictors too (make web searches, test things on temp scripts), and add them to the ci methods ; if it fails go (a)**
- **Q4 — `empirical = "cell"` on wide families:** automatic, or explicit? *Recommendation:* automatic
  with a message naming the switch, so a narrow multinomial can opt back to two columns.
  **Maintainer’s decision: automatic with no message, but add an expert exit door, `empirical=TRUE` can’t do it, so something like `empirical="column"` would be needed, to document in the regression vignettes multinomial part (quick sentence and code only, echo=TRUE, eval=FALSE)**
- **Q5 — add a `{gap}` token now** (§3.6.6), so print, Excel and Markdown readers can see the gap at
  all? *Recommendation:* yes — the display grammar is being reworked anyway, and Phase 22d makes the
  hole real.
  **Maintainer’s decision: yes, good idea.**
- **Q6 — fix `needs = "marginaleffects"`** (§3.6.1) in this phase or separately? *Recommendation:* in
  this phase, because K4's "always populate" depends on it.
  **Maintainer’s decision: fix it in this phase.**

### Explicitly out of scope, and why

- The `color = "adjustment"` sign convention on a **sign-flipping** effect (crude `1/1.06`, model
  `1.05`, scored as "attenuated"). The away-from/toward-the-null rule is ruling Q4 of
  `dev/model_vs_observed_effect_colour.md` and is correct for protective effects; the flip case is a
  genuine edge the rule does not describe well, but it is a Phase 22b question about the *score*, not
  about the column layout. Noted so it is not lost.
- Ordering of the "Overall association" footer rows, the `[outcome]` repetition in exports, the `n`
  column, `Model_β -> Model_coeff` as a pure rename, tidyselect in `tab_reg()`, the stored formulas
  accessor: all Phase 22b.
- Anything on the list of already-rejected alternatives in `dev/model_vs_observed_effect_colour.md`
  and `dev/model_vs_observed_gap_test.md` — in particular a materialised gap column, CI-overlap as the
  gap test, Hausman subtraction, a table-level colour hook, and reusing an existing field for the gap
  SE. None of this proposal touches them.

---

## 9. Appendix — captures

### A. Field population, `empirical = TRUE`, binomial (17 rows)

| column                             | populated fields (non-NA count / 17)                                           |
|------------------------------------|--------------------------------------------------------------------------------|
| `n`                                | `n` 16                                                                         |
| `Obs_%`                            | `n` 15, `pct` 15, `tot_n` 15, `diff` 15, `ci_inf`/`ci_sup`/`pvalue` 12         |
| `Obs_OR`                           | `n` 15, `or` 16, `ci_inf`/`ci_sup`/`pvalue` 13                                 |
| `Model_OR` (coefficient)           | `or` 17, `obs` 16, `ci_inf`/`ci_sup`/`pvalue` 14, **`gap_se` 0**               |
| `Model_AME` (marginal, difference) | `pct` 15, `diff` 13, `or` 14, `obs` 16, `gap_se` 13, `ci`/`pvalue` 13          |
| `Model_RR` (marginal, ratio)       | `pct` 15, `or` 16, `obs` 16, `gap_se` 13, `ci`/`pvalue` 13                     |
| `Model_β` (gaussian)               | `diff` 17, `obs` 16, `gap_se` 13, `var` 17, `ci`/`pvalue` 14                   |
| `Obs_mean` (gaussian)              | `n` 15, `mean` 15, `tot_n` 15, `var` 15, `ci_inf`/`ci_sup` 15, **no `pvalue`** |
| `Obs_rate` (poisson)               | `n` 15, `mean` 15, `tot_n` 15, `ratio` 15, `ci`/`pvalue` 12                    |

`Model_OR`'s empty `gap_se` is ruling Q1(b) at work: no test on a non-collapsible conditional OR.

### B. `display` behaviour, measured

```text
display = "value"        Model_OR: 1/2.67***
display = "ci"           Model_OR: 0.37 [0.32;0.44]***
display = "prob"         Model_OR: 0.37*** (26.72%)
display = "ame"          Model_OR: 0.37*** (-24.16%)
display = "{or} ({pct})" Model_OR: 0.37*** (26.72%)      identical to "prob"
display = "{value}"      Error: Unknown field "value"
effect = "marginal", display = <anything>   ignored, silently reset to "value"
gaussian / poisson, display = "prob"        degraded to est_ci
Obs_% / Obs_OR           unaffected by every display above

# post-hoc, on a table built with display = "prob":
t |> mutate(Model_pct = set_display(Model_OR, "{pct}"), .after = Model_OR)
#>   Model_OR         Model_pct
#>   1.00    (50.87%) 50.87%
#>   0.35*** (26.72%) 26.72%***      <- the adjusted %, starred/coloured by the OR's interval
#> legend: "Obs_OR, Model_OR, Model_pct: OR (ref.): ..."   <- one block, unchanged
```

### C. Legend blocks, measured

```text
binomial, coefficient, empirical, color = TRUE                       terse: 2 blocks
  Obs_%                                    difference ladder
  Obs_OR, Model_OR                         OR ladder                 <- already merged

binomial, marginal, difference, empirical                            terse: 1 block
  Obs_%, Obs_diff, Model_AME (adjusted %)  difference ladder         <- already one

binomial, marginal, ratio, empirical, color = c(TRUE, "adjustment")  terse: 3 blocks
  Obs_%                                    difference ladder
  Obs_RR                                   RR ladder
  Model_RR (adjusted %)                    RR ladder + bg adjustment ladder

poisson, coefficient, empirical                                      terse: 2 blocks
  Obs_rate                                 mean_ratio ladder, "/ x" glyphs
  Obs_IRR, Model_IRR                       odds_ratio ladder, "1/" glyph
```

### D. Timings

21 483 rows, 4 predictors (3 factors + 1 numeric), logistic, median of 3 warm runs, 1 thread.

```text
empirical = TRUE,  display = "value"                    0.32 s
empirical = TRUE,  display = "prob"                     0.36 s
empirical = TRUE,  display = "ame"                      0.28 s
empirical = TRUE,  display = "{or} ({diff}) ({pct})"    0.31 s
empirical = FALSE, display = "value"                    0.12 s
empirical = FALSE, display = "ame"                      0.23 s
```

### E. Sources consulted for the naming question

- Marginal vs conditional estimands and non-collapsibility: the g-computation / model-based
  standardisation literature (Sjölander, *Marginal causal effects*; the indirect-comparison
  marginalisation papers). The axis word is **marginal vs conditional**; a marginal odds or risk ratio
  is obtained by standardising the fitted model over the covariate distribution.
- `marginaleffects`' own vocabulary: `comparison = "lnratioavg"` is documented as the **log of the
  ratio of average** estimated potential outcomes — a contrast of standardised means, not an average
  of contrasts. This is what makes "average marginal effect ratio" wrong and `mRR` right.
- Stata / Long & Freese / Williams' framework: the two axes are (effects | predictions) x (averaged |
  at representative values), giving AME, MER, AAP, APM. `MER` is tabxplor's existing usage and it is
  correct; `AME` is correct for the additive path only; **AAP** ("average adjusted predictions") is
  the standard name for what `{base}` prints on a model column.
