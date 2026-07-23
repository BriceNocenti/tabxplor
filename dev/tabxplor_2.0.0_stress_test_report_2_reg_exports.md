# tabxplor 2.0.0 — stress-test report 2: tab_reg() + export stack

Campaign run 2026-07-23 on commit `80bdfd2` (branch `v2.0.0`), R 4.6.1, WSL2 ext4, via `devtools::load_all()`. Scope chosen by the maintainer: **tab_reg() (all families, engines, weights, options) + the export stack (xl/html/md/plot/export/transpose)**, with **deep statistical verification** — every estimate/CI/p cross-checked against independent refits (`stats::`, `survey::`, `svyVGAM`, `nnet`, `MASS::polr`, `marginaleffects`, closed-form crude CIs). 229 logged cases across 9 harness scripts (176 ok, the rest triaged below); every §2 finding was re-reproduced in a minimal cold `Rscript` outside the harness. This complements report 1 (`dev/tabxplor_2.0.0_stress_test_report.md`, build core); nothing from report 1's known-issues or the already-fixed Phase m/q/r items is re-reported.

## 1. Executive summary

The statistical core of `tab_reg()` is in excellent shape: **every parity check against an independent refit passed to numerical tolerance** — all 6 families on all engines (lm, glm, svyglm with wt/full design/prebuilt design, nnet::multinom, svyVGAM::svy_vglm, MASS::polr, svyolr), grouped-binomial `trials`, profile CIs, AME/MER vs `marginaleffects`, empirical crude companions vs hand computation, model comparison vs `anova()` (incl. the weighted Wald path), GOF footers vs `glance`-equivalents, `multiplier`, `reference=` releveling, and the Kish `n_eff` isolation contract. The findings are concentrated at two edges:

- **Input hygiene of exported HTML/markdown**: factor-level text is not escaped in `tab_html` body cells and in the md column-header row (§2.1, §2.2). Real French survey levels like `"<25 ans"` or `"P&O"` corrupt the output.
- **Argument-boundary validation of `tab_reg()`**: a family vector not sliced in one recursion (§2.3), a silent all-empty table for a 2-level multinomial (§2.4), unvalidated `conf_level` (§2.5), plus a series of obscure error messages for common user mistakes (§4).

## 2. Confirmed bugs

### 2.1 MAJOR — `tab_html()` does not HTML-escape factor-level text in body cells

Both engines ("html" and "kableExtra") write row-label text raw into `<td>`. A level shaped like markup becomes a real HTML element and its text disappears; a level containing `&` emits invalid HTML; entity-shaped text (a literal `"&lt;"` in data) would render as `<`. Header cells are escaped (`C&D` → `C&amp;D` in `<th>`), so the body path is the gap. `"<25 ans"` only renders today through HTML5 parser leniency (`<` before a digit is treated as text) — it is still invalid HTML and any `<letter…>` level breaks for real.

```r
library(tabxplor)
d <- data.frame(g = factor(rep(c("<b>injected</b>", "Q&A", "safe"), 20)),
                h = factor(rep(c("A", "B"), 30)))
k <- tab_kable(tab(d, g, h, pct = "row"))
grepl("<b>injected</b>", paste(as.character(k), collapse = ""), fixed = TRUE)  # TRUE: real tag
# xml2 parse-back: the level text is gone, a real <b> element sits inside the td
```

- **Observed**: `<td class="tx-l ...">amp & lt ` (truncated at the raw `<`); `<b>injected</b>` becomes a bold element; no `&lt;` anywhere.
- **Expected**: body text escaped exactly like the header path, so any level renders as its literal text.
- **Root cause pointer**: the body-cell writers in `R/tab-render-html.R` (home engine) interpolate `format()` strings without an escape pass; the kableExtra engine passes pre-composed cells with `escape = FALSE`, so it inherits the same gap. Note the escape must be applied to the *data text only* — the colour `<span>`s and tooltips the engine itself adds must stay raw.
- Repro: `r06_html_level_escape.R` (campaign scratchpad).

### 2.2 MAJOR/MINOR — md export: `|` in a col_var level breaks the pipe table (header row only)

Body rows escape pipes correctly (`pipe \| pipe`); the column-header row does not, so the header has one more `|` than every other row and the GFM table no longer parses in pandoc/quarto.

```r
d <- data.frame(g = factor(rep(c("u", "v"), 30)),
                h = factor(rep(c("A|B", "C"), 30)))
tab_md(tab(d, g, h), color = FALSE, css = FALSE)
# | g       | A|B  |   C  |Total  |   <- 6 pipes; every other row has 5
```

- **Expected**: `A\|B` in the header, as in body cells.
- **Root cause pointer**: the header-row builder in `R/tab_md.R` (the `tab_header_runs()` consumption added in Phase 17g) skips the pipe-escape applied to body cells.
- Repro: `r04_md_header_pipe.R`.

### 2.3 MINOR — multi-dependent + model list: unnamed `family` vector is not sliced per dependent

The multi-dependent comparison recursion carefully slices `trials` per dependent but forwards the **whole** `family` vector, so the 2nd dependent is fitted with the 1st family and dies in `reg_prep_binary()` with a confusing message (it dumps the numeric outcome's 1500 "levels").

```r
tab_reg(d, c("y_bin", "y_gauss"),
        predictors = list(m1 = "x1", m2 = c("x1", "f1")),
        family = c("binomial", "gaussian"), compare = "sequential")
# Error: The dependent variable "y_gauss" must be binary (2 levels). It has 1500 levels: ...
```

- The same unnamed vector works fine without a predictors list; a **named** vector (`c(y_bin = "binomial", ...)`) works in both modes (workaround).
- **Root cause**: `R/tab_reg.R:2601` — `family = family` in the per-dependent recursion; slice it like `tri` is sliced 4 lines above.
- Repro: `r01_multidep_compare_family.R`.

### 2.4 MINOR — `family = "multinomial"` with a 2-level dependent builds an all-empty table silently

The fit succeeds (GOF footer is correct: N, LR, McFadden all match the binomial fit) but every estimate cell is empty/NA — `nnet::multinom` returns a coefficient *vector* for 2 classes where the extraction expects a *matrix*. No error, no warning: a plausible-looking table with no numbers.

```r
d$y <- factor(ifelse(runif(300) < plogis(d$x1), "yes", "no"))
tr <- tab_reg(d, "y", "x1", family = "multinomial")
all(is.na(get_num(tr[[ncol(tr)]])))   # TRUE — silent
```

- **Expected**: either coerce to the (numerically equivalent) binomial path with a message, or extract the vector-shaped coefficients correctly.
- **Root cause pointer**: `reg_fit_multinom` / its `coef()` matrix indexing in `R/tab_reg.R`.
- Repro: `r02_mnl_2level_allNA.R`.

### 2.5 MINOR — `conf_level` is never validated (tab_reg **and** tab)

`conf_level = 1.5` builds a table with NA CI bounds plus a leaked base-R warning (`NaNs produced` from `qnorm`); `1` gives `[0, Inf]`; `0` gives a zero-width interval. `tab(..., ci = "cell", conf_level = 1.5)` accepts it too, so the gap is shared, not reg-specific.

```r
tab_reg(d, "y", "x1", family = "binomial", conf_level = 1.5)  # NA CIs + "NaNs produced"
```

- **Expected**: one boundary check `0 < conf_level < 1` with a cli abort, at both entry points (`tab_resolve_settings()` / the `tab_reg` arg block).
- Repro: `r03_conf_level_unvalidated.R`.

## 3. Design / statistical concerns (defensible, but worth a decision or a doc line)

### 3.1 Perfect separation is not flagged, and absurd ORs print with full digits

A separated binomial fits "successfully" and renders `OR = 344 744 301 440` — and on the inverse side the literal string `1/118848597673118416240640.00`. The p≈1 and CI `[0, Inf]` are self-indicating to a statistician, but nothing tells the user the fit is degenerate, and no scientific-notation cap tames the display. Suggest: a footer note when any |coef| or SE exceeds a sanity threshold (the classic separation signature), and a display cap (`>999` → scientific) for the `or` token.

### 3.2 Empirical mean-diff CI uses population-denominator (÷n) variances

The `empirical = TRUE` gaussian companion's Welch CI matches `t.test()` only after substituting `var*(n-1)/n` — the documented "weighted means/variances, unweighted n" convention (Σw denominator). At n≈450 the interval is ~0.1 % narrower than Welch-on-sample-variance; harmless at survey scale, mildly anti-conservative at very small group n (~10 % at n=5). Worth one doc line at `ci_mean_diff2()` (`R/tab-agg.R:408`); no code change needed.

### 3.3 Degenerate weights produce misleading errors; NA weights drop silently

- All-zero weights: `Error in svyglm.survey.design(): weights must not contain NA values` — the weights were 0, not NA (a normalisation turns them into NaN first). `tab()` by contrast aborts naming the weight column (Phase p). Same misleading message for an `Inf` weight.
- NA weights: rows are dropped silently (complete-case) with no note, while the same situation in `tab()` is messaged.

### 3.4 `baseline = "nope"` warns and silently falls back to the first model

`compare = "baseline", baseline = "nope"` emits a warning ("matches no model; using the first") and proceeds. Defensible, but an abort (like `reference =` gives: *level "ZZZ" not found*, with the valid set listed) would be more consistent with the package's own style.

### 3.5 `dependent` also listed in `predictors` builds a corrupt-looking table

glm drops the RHS response with its own warnings, and the output keeps a spurious skeleton block for the dependent (est 1 / empty). `split_var` gets a clean guard for the same class of mistake (*"cannot also be the outcome or a predictor"*) — mirroring that guard for `predictors` would close the inconsistency.

## 4. Minor issues and rough edges (error-message quality)

Each of these is a *working* guard whose message doesn't name the actual problem — the pattern report 1 called "obscure internal error":

- **Missing columns**: `predictors = c("x1", "nope")`, `ids = "nocol"`, `strata = "nocol"` → bare `object 'nocol' not found` with no hint which argument it came from. A pre-flight `setdiff(vars, names(data))` check with a cli message would cover all three.
- **`trials = "tri"`** (a column name where a count is expected — the natural reading of the API) → `NAs introduced by coercion` + `contrasts can be applied only to factors with 2 or more levels`. One `is.numeric(trials)` check away from a clean message.
- **Single-level factor predictor** → the same bare `contrasts` error, without the variable name.
- **`predictors = character(0)`** → `str2lang: attempt to use zero-length variable name`, while `predictors = NULL` gets the clean *"`predictors` is required"* message. Route empty to the same message.
- **Duplicate model names** (`list(m1 = ..., m1 = ...)`) are accepted silently; one of the two wins.
- **`transpose = TRUE` on a reg table** aborts with *"does not support tables with `tab_vars`"* — true internally (the `var` block is a group) but confusing for a user who passed no tab_vars; say "regression tables cannot be transposed" when `reg_meta` is present.
- **`format()` on an NA fmt cell** returns the string `"NA"`, while print/pillar and every exporter blank it. Only visible to programmatic `format()` users; note it in `?format.tabxplor_fmt`.

## 5. Verified-clean areas (coverage map)

Everything below ran and matched its independent reference (tolerances: 1e-8 coefficients, 1e-6 Wald CIs, 1e-4 profile CIs / crude CIs).

| Area | Verified against | Result |
|------|------------------|--------|
| gaussian lm, weighted svyglm | `lm`/`svyglm` coef, t-CIs, p | exact |
| binomial glm; svyglm via wt=, ids+strata+nest, prebuilt design | refits (first-level modelled) | exact |
| exponentiate=FALSE; profile CIs | `exp(coef)` identity; `confint.glm` | exact |
| poisson (equidispersed) / quasipoisson | Pearson-dispersion-scaled quasi SEs, t | exact¹ |
| grouped binomial `trials=30` | `glm(cbind, quasibinomial)` + t CIs | exact |
| multinomial nnet; weighted svyVGAM | `multinom` / `svy_vglm` per-outcome ORs | exact |
| ordinal polr; weighted svyolr | Wald ORs from `summary()` | exact |
| family="auto" detection (bin/gauss/mnl); mixed 3-family table | `model_family` attrs | ✓ |
| AME (gauss≡coef, binom, pois, weighted); MER at="reference" | `marginaleffects` avg/`datagrid` | exact |
| estimate_display prob/ame/ci folds | predicted probs in [0,1]; AME parity; bracket renders | ✓ |
| empirical: Obs_%/OR (Woolf), Obs_mean/diff, Obs_rate/IRR; log twins | hand-computed crude values + CIs | exact² |
| kish_neff isolation | emp CIs widen, model CIs byte-identical, off-kish identical | ✓ |
| compare sequential/baseline (gauss F, binom LRT); weighted Wald | `anova()`; `anova.svyglm(method="Wald")` | exact |
| non-nested compare | clean message + dAIC row (no fake p) | ✓ |
| GOF footers (lm r2/f/sigma; glm N/LR/McFadden/AIC/BIC; svyglm wald/Nagelkerke) | direct computation | exact |
| multiplier c(x1=2)/c(x1=.5) | OR^k; factor/unknown names cleanly refused | exact |
| conf_level 0.5/0.999 CI math; stars vs p thresholds | z-quantile recompute | exact |
| reference= relevel; bad level; inverse_two_level_factors | manual `relevel()` refit; 1/OR flip | exact |
| na= drop_by_model vs drop_all_models (staggered NAs) | complete-case counts in footer | ✓ |
| split_var: spread parity per group, stacked mode, empty level, AME/empirical per group | per-subset refits | exact |
| tab_logit ≡ tab_reg; formula-in-dependent (interactions) | identical estimates | ✓ |
| Degenerate probes with clean guards | family validation, split_var/multiplier/trials guards, fpc, bad reference | ✓ |
| Exports: 13 stressed tables × md/html/kableExtra/xl (+2 plots) | file produced, non-empty | 54/54 |
| Export options: transpose, var_names, themes, lang=fr, captions, popover, dispatcher, list | build + read-back | ✓ |
| Export parity: `format()` ≡ md ≡ html text; xl numeric ≡ `get_num()`³ | read-back (xml2 / wb_to_df) | ✓ |
| fr: numbers identical, labels translated; engines value-identical; transpose | cell-set comparison | ✓ |
| unicode/RTL/200-char levels; body `\|` escape; `&` in headers | all formats | ✓ except §2.1/§2.2 |

- ¹ `tab_reg(family="poisson")` always applies the dispersion scaling (documented + warned); at dispersion ≈ 1.0 the CI differs from a naive `glm(poisson)` z-CI by ~0.1 % — consistent with its own documentation, not a defect.
- ² Gaussian diff CI is Welch on population-denominator variances — see §3.2.
- ³ Excel writes reg OR columns as formatted text by default (`tabxplor.xl_or_numeric` FALSE); with `or_numeric = TRUE` all raw ORs round-trip exactly.

## 6. Dismissed leads (harness artefacts — do **not** chase)

- **"Binomial ORs are inverted vs glm"** — deliberate convention: the modelled level of a 2-level factor dependent is its **first** level (`inverse_two_level_factors`, the maintainer's `"1-Oui"`-first convention); `glm` models the second. Every refit matches after flipping. Same for the "mismatched" svyglm/profile CIs.
- **"Stars missing from `format()`"** — `format.tabxplor_fmt(stars = FALSE)` is the default; the pillar shaft passes `stars = TRUE`. Intended layering.
- **"`est_ci` bracket missing"** — the bracket needs `special_formatting = TRUE` (which print and all exporters pass); bare `format()` omits it by design.
- **"σ cell missing from md"** — the md pads numbers with U+2007 figure spaces; byte-level comparison must normalise them. Value is present and correct.
- **"Excel loses reg ORs"** — text-OR default, see §5 note ³.
- **"Transpose drops the test summary"** — the labels move from `<td>` to `<th>` and the Total composite splits n into its own line (the documented xl-style materialisation); all values present.
- **"Weighted-compare p wrong"** — matches `anova.svyglm(method = "Wald")` exactly (the documented choice); the default `anova.svyglm` LRT is a different test, not the reference.
- **quasi p-values at rel 1e-4** — t-vs-z quantile differences at df ≈ 1500; both defensible, no user impact.

## 7. Recommended actions before release

1. Escape body-cell text in both `tab_html` engines (§2.1) and the md header row (§2.2) — the only findings that corrupt real-world output (`"<25 ans"`, `"P&O"`, `"A|B"`).
2. Slice `family` in the multi-dependent recursion (`R/tab_reg.R:2601`, one line, §2.3).
3. Guard the 2-level-multinomial extraction (§2.4) and validate `conf_level` at both entry points (§2.5).
4. Sweep the §4 error-message list — each is a one-line pre-flight check; together they close every "obscure error" found in this campaign.
5. Optional (§3): separation footer note + scientific cap for absurd ORs; align the zero/Inf-weight message with `tab()`'s; a doc line on the ÷n variance convention.

Repro scripts (`r01`–`r06`) and the full JSONL evidence trail live in the session scratchpad (`scratchpad/repro/`, `scratchpad/logs/`); they are throwaway harness artefacts, not repo files.
