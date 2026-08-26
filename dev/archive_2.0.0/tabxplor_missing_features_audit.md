# tabxplor — missing and non-standard features: an honest adoption audit

Date: 2026-07-21. Author: assistant audit for the maintainer.

This document answers one question honestly: **what would make a quantitative social scientist
try tabxplor and then not stick with it?** It covers the crosstabs side, the regressions side,
and the non-standard design choices that create friction. Every "missing" claim below was
verified against the source (file:line pointers) before being written — nothing is asserted as
absent that actually exists under another name. Benchmarks are the incumbents these users
actually leave (SPSS, Stata, SAS) and the R packages they land on (expss, gtsummary, pollster,
questionr, srvyr/survey, modelsummary, sjPlot, ggeffects).

The tone is deliberately critical. tabxplor already does several things better than the
competition — those are credited inline so the gaps are seen in proportion, not inflated.

---

## 1. Executive summary

Five things, in order, are most likely to make a target user bounce:

1. **No effect size on a crosstab (Cramér's V / phi).** APA 7 and most journals now require an
   effect size alongside every chi-square. A social scientist runs one cross, looks for the V,
   doesn't find it, and reaches for `questionr`/`vcd`/SPSS. This is the single cheapest gap to
   close and the most visible one.
2. **The weighting/significance inconsistency.** tabxplor's crosstab significance uses the
   **weighted estimate but the unweighted n** by default (Kish `n_eff` is opt-in and numeric-only),
   while `tab_reg()` is fully design-based. A survey methodologist who notices this will not trust
   the crosstab p-values on complex-survey data — the field standard is Rao-Scott / `svychisq`.
3. **No LaTeX / Word export.** Journal and Overleaf workflows expect texreg/huxtable/modelsummary
   output. Excel/HTML/Markdown/ggplot do not cover "paste this regression table into my paper."
4. **No mixed / multilevel models and no cluster-robust SEs.** Two of the most common regression
   needs in sociology, education, political science and economics are simply out of scope
   (lmer/glmer absent; only survey-design SEs, no `sandwich`/`vcovCL`).
5. **No multiple-response tables and no SPSS-style column-comparison letters.** The two table
   shapes that define survey/market-research tabulation (multi-select questions summing to >100%,
   and pairwise column significance letters) are the reason expss and SPSS Custom Tables exist.

Everything else is either nice-to-have, deliberate niche, or already handled. The prioritized
verdict is in §6.

---

## 2. Who the user is and what "standard" means

The person most likely to reach tabxplor is a **survey / social-science analyst** — sociology,
political science, education, public health, market research — who imports labelled data (haven),
wants weighted crosstabs with significance, and writes regression tables for a paper or report.
They are leaving, or supplementing, SPSS/Stata. "Standard" for them means, roughly, what SPSS
Crosstabs + Custom Tables and Stata `svy: tabulate` / `margins` produce without effort, plus what
the R packages they'd otherwise use (expss, gtsummary, modelsummary, sjPlot) give out of the box.

tabxplor's actual sweet spot is narrower and genuinely strong: **colour-coded, reference-relative
crosstabs with per-cell CI-based significance, and a unified regression-table builder covering
survey designs.** The gaps below are the distance between that sweet spot and the full "standard".

---

## 3. Crosstabs — what's missing

### 3.1 Inference and effect sizes

- **No association effect size.** No Cramér's V, phi, Tschuprow's T, contingency coefficient, or
  Cohen's w/h anywhere (confirmed: zero matches for `cramer`/`fisher.test`/`phi` in `R/`; the only
  "Cohen" scale is for *colouring* mean-differences and betas, `fmt_class.R:2614`, not an
  association statistic). `tab_chi2()` computes per-cell contributions and a table p-value
  (`tab.R:5332`) but never converts to a standardized effect size. **Every comparator has this:**
  SPSS Crosstabs (Phi/Cramér's V checkbox), Stata, `questionr::cramer.v`, `vcd::assocstats`,
  `rstatix`, `effectsize`. Journals require it; its absence is conspicuous.
- **No Fisher's exact test.** The small-cell path is Yates-corrected chi-square only. SPSS and
  Stata offer Fisher's exact automatically for sparse 2×2 (and R×C via `fisher.test`). Reviewers
  ask for it when expected counts < 5.
- **Cell significance is proportion-difference, not adjusted residuals.** This is a design choice
  worth stating plainly. tabxplor colours a cell by its **proportion/percentage difference from a
  reference** (the total row or the first cell) and gates significance by **CI inversion**
  (Wilson single proportion, Newcombe hybrid for a difference, or Wald) — `fmt_class.R:171,367`,
  `tab-agg.R:243-259`. It does **not** use chi-square **adjusted/standardized (Haberman)
  residuals**, the convention most textbooks and SPSS use to say "which cell drives the
  association" (the ±1.96 rule). The nearest thing is the opt-in `color = "contrib"` mode
  (correspondence-analysis-style signed contribution to variance, gated by an independence-test
  p-value — `fmt_class.R:1485-1491`), which is powerful but unfamiliar and off by default.
  The reference-relative CI approach is defensible and arguably more interpretable, but it is
  **not what a user trained on SPSS expects to see**, and there is no way to ask for classic
  adjusted residuals.
- **No column-proportion significance letters / pairwise column comparison.** Significance is
  always cell-vs-reference (total or first column), never all-pairs-of-columns with a
  multiple-comparison correction. SPSS Custom Tables' column-proportion z-test with APA subscript
  letters (Bonferroni or Benjamini-Hochberg) is *the* market-research idiom; expss reproduces it.
  A user comparing several groups column-by-column has no way to get "column A differs from column
  C" with FWER/FDR control. No `bonferroni`/`holm`/`pairwise`/`marascuilo` anywhere.

### 3.2 Table shapes

- **No multiple-response / multiple-choice tables.** A respondent who "select all that apply"
  produces percentages that sum to >100% over a common base. tabxplor has no `mrset`/`mdset`
  equivalent; every table is a single categorical cross summing to 100% within a base. This is a
  core survey/market-research shape (expss `mrset`, SPSS "Multiple Response Sets", Stata). For many
  survey datasets it is not optional. **Maintainer answer : they *are* present, its `levels = "first"`**. 
- **Banner / nested tables ARE present — credit it.** `tab()`/`tab_many()` accept several
  `col_vars` and render a genuine banner header with column-group separators
  (`fmt_class.R:1662`, `tab_md.R:249-257`). This matches expss `nest` and SPSS banners and is a
  real strength; it should be advertised more, because users assume it's missing.

### 3.3 Design-based inference (the headline honesty point)

tabxplor's crosstab estimates are weighted, but the **significance test uses the unweighted n by
default**. Kish's effective sample size `n_eff = (Σw)²/Σw²` exists only as an opt-in option
(`options(tabxplor.kish_neff = TRUE)`), applies to numeric/mean CIs only, and is explicitly
**deferred on the factor side** (`tab-agg.R:123,165-175`; `utils.R:111-113`). The **design effect
(deff) is not exposed at all** — only the raw Σw² sufficient statistic is retained.

For complex surveys (stratification, clustering, unequal weights) the field standard is
**design-based inference**: Rao-Scott second-order corrected chi-square (`survey::svychisq`),
which uses cell-level design effects to adjust the reference distribution. Using a weighted
proportion with an unweighted n produces **over-confident** crosstab p-values under clustering —
exactly the failure the survey literature warns about.

The sharp version: **`tab_reg()` is design-based (svyglm, Taylor linearization) but the crosstab
significance engine is not.** A methodologist will see the inconsistency immediately. This is the
gap most likely to lose the *sophisticated* survey user, as opposed to the effect-size gap that
loses the *ordinary* one.

### 3.4 Other tests

- **No trend test** (Cochran-Armitage) for ordered categories × binary — common in
  epidemiology/education dose-response tables.
- **No paired test** (McNemar) for before/after or matched designs.
- **Missing-data handling is solid — credit it.** `na = "keep"/"drop"/"drop_all"/"common_base"`
  (`tab.R:533,555`), NA printed as an explicit level by default, per-scope listwise via
  `drop_all`, and the valid-vs-total-percent distinction falls out of `drop` vs `keep`. This is on
  par with SPSS valid-percent and better than several R competitors.

---

## 4. Regressions — what's missing

`tab_reg()` is a genuinely ambitious unified builder (gaussian β / binomial OR / poisson IRR /
multinomial OR / ordinal cumulative OR over lm/glm/svyglm/svyolr/svy_vglm/nnet/polr), with
survey designs as a first-class citizen. The gaps are about breadth of model families, the
standard-error menu, and effect visualization.

### 4.1 Model families

- **No mixed / multilevel models.** lmer/glmer/lme4/random effects are entirely absent (0 matches).
  Multilevel modelling is arguably *the* dominant modern method in quantitative sociology,
  education and political science. gtsummary + broom.mixed and sjPlot both handle it; tabxplor
  cannot. For a large user segment this alone is disqualifying.
- **No fixed-effects panel models.** No plm/fixest/feols/felm. Panel/longitudinal econometrics
  (two-way FE, `feols(y ~ x | id + year)`) is out of scope.
- **No extra GLM families.** Negative binomial (`glm.nb`), zero-inflated (`zeroinfl`), Tobit,
  Heckman selection, GEE (`geeglm`), and Cox / parametric survival (`coxph`/`survreg`) are all
  absent. Count data with overdispersion (very common) can only be handled via quasipoisson, not
  a proper negative binomial; event-history analysis is impossible.

### 4.2 Standard errors

- **No general robust / cluster-robust SEs.** There is no `sandwich`/`clubSandwich`/`vcovCL`/
  `coeftest` path (the "sandwich" mentions in the source are prose). The **only** cluster-robust
  route is a survey design (svyglm Taylor linearization via `ids`/`strata`/`fpc`/`nest`) — which
  is excellent for survey data but is not what an econometrician means by
  `vcov = "cluster", cluster = ~firm`. There is a robust-Poisson/GEE-sandwich variance path, but
  only inside the *crosstab* mean-ratio engine (`tab-agg.R:425`), not exposed for regression.
  modelsummary/estimatr make HC0-3 and clustered SEs a one-argument choice; tabxplor users who
  need them for a non-survey design are stuck.

### 4.3 Effects and visualization

- **No ggeffects / emmeans-style effect plot.** Predicted probabilities and AME are folded into
  table cells via marginaleffects (`effect="ame"`, `estimate_display="prob"/"ame"`,
  `at=c("average","reference")`) — a real strength — but there is no adjusted-prediction **plot**
  across a predictor's range, the sjPlot/ggeffects idiom that social scientists now expect for
  communicating interactions and non-linearities. The only plots are `or_plot()` (OR forest) and
  `lm_plots()` (2×2 diagnostic panel) — `tab_reg_plots.R`.
- **Interactions are best-effort term rows.** They render (`man/tab_reg.Rd:272`) but there is no
  dedicated marginal-effects-at-interaction grid (e.g. AME of X at each level of Z).

### 4.4 Coefficients and diagnostics

- **No fully standardized (beta) coefficients displayed.** The gaussian coefficient is
  standardized only as `beta/SD(Y)` to drive the effect-size *colour* (`fmt_class.R:519-520`);
  the predictor is not standardized and no standardized beta is shown. `multiplier` (OR^k) is
  user-supplied rescaling, not auto-standardization. Users who report standardized betas for
  cross-predictor comparison must compute them elsewhere.
- **No multicollinearity diagnostic (VIF).** Absent. A near-universal check in applied regression.
- **No classification diagnostics.** No ROC/AUC, no Hosmer-Lemeshow, no Brier score, no confusion
  matrix for logistic models.
- **Diagnostics that ARE present — credit it.** Pseudo-R² trio (McFadden, Nagelkerke, Cox-Snell),
  LR-vs-null, AIC/BIC, dispersion, F/R²/adj-R², and the **Brant proportional-odds test** for
  ordinal models (`tab-test-display.R:124-131`, `tab_reg.R:665,1505`). This is a respectable
  footer, better than gtsummary's defaults for pseudo-R².

### 4.5 Model comparison — present, credit it

Side-by-side model columns exist (a named list of predictor sets fits one model per column;
`multi_logit()`; `compare = c("none","baseline","sequential")` — `tab_reg.R:2543-2546`), plus
`tab_spread(split_var)` to pivot subgroups into columns. It is home-grown (no modelsummary/
stargazer dependency). It works, but it lacks the polish and format coverage of modelsummary
(fit-statistic rows, flexible star schemes, and — see §5 — LaTeX/Word output).

---

## 5. Non-standard choices that create adoption friction

- **The custom `tabxplor_fmt` vctrs record.** It is the source of tabxplor's power (lossless
  display switching, per-cell metadata, colour). But it also means a tabxplor column is **not a
  plain number** — users who `mutate()`, join, or hand a table to another package can be surprised.
  The retro-compat contract (users read fields with `$`) mitigates this, but the mental model
  ("my percentage is a record vector") is a learning cost no competitor imposes. This is a
  deliberate, defensible trade — but it is friction, and it should be acknowledged in onboarding.
- **No LaTeX / Word export.** Exporters cover Excel (coloured), HTML/kable, Markdown, and ggplot —
  but **not LaTeX or Word**. The journal/Overleaf workflow is served by texreg, huxtable,
  modelsummary, flextable, gt. A user who needs "put this in my paper" cannot, without manually
  bridging to another package (and the custom type makes that bridge harder). For the academic
  audience this is a first-order gap, not cosmetic.
- **Labelled-data (haven/labelled) interop.** Social scientists import SPSS/Stata files with
  variable and value labels (`haven_labelled`). How cleanly those labels flow into tabxplor row/
  column titles determines whether the tool fits the real workflow. sjPlot/expss lean on labels
  heavily; if tabxplor requires the user to pre-convert labels to factors, that's friction worth
  documenting and, ideally, smoothing.
- **Colour-first display vs plain publication tables.** The colour coding is the headline feature
  and a real differentiator for *exploration*. But a plain, black-and-white, APA-styled table is
  what goes in a manuscript. The HTML/Excel exports address this partly; the point is that the
  default aesthetic optimizes for screen exploration, not print submission, and some users want the
  opposite by default.
- **The weighting/significance rule (§3.3)** is itself a non-standard choice, not just a missing
  feature — restated here because it is as much an "expectations" problem as a "capability" one.

---

## 6. Prioritization — dealbreaker / high-value / nice-to-have / niche

| Gap | Side | Verdict | Why |
|-----|------|---------|-----|
| Effect size (Cramér's V / phi) | Cross | **Dealbreaker** | APA/journal requirement; cheapest to add; most visible absence |
| Design-based crosstab significance (Rao-Scott / n_eff by default) | Cross | **Dealbreaker** (survey users) | Over-confident p-values on complex surveys; inconsistent with `tab_reg` |
| LaTeX / Word export | Both | **Dealbreaker** (academics) | "Put it in my paper" is unserved |
| Mixed / multilevel models | Reg | **Dealbreaker** (sociology/edu/poli-sci) | Dominant modern method, fully absent |
| Cluster-robust / HC standard errors | Reg | **High-value** | One-arg elsewhere; needed off-survey |
| Multiple-response tables | Cross | **High-value** (survey/market) | Defines survey tabulation; expss/SPSS core |
| Column-proportion letters (pairwise + correction) | Cross | **High-value** (market research) | The SPSS Custom Tables idiom |
| ggeffects-style effect plots | Reg | **High-value** | Modern standard for communicating effects |
| Fisher's exact | Cross | **High-value** | Expected on sparse tables; small effort |
| Adjusted (Haberman) residuals option | Cross | **High-value** | Familiarity; the SPSS "which cell" convention |
| Standardized beta display | Reg | **Nice-to-have** | Common but easily computed elsewhere |
| VIF / multicollinearity | Reg | **Nice-to-have** | Expected check; small |
| ROC/AUC, Hosmer-Lemeshow, Brier | Reg | **Nice-to-have** | Logistic diagnostics; audience-dependent |
| Cochran-Armitage trend, McNemar | Cross | **Nice-to-have** | Field-specific |
| Fixed-effects panel (plm/fixest) | Reg | **Niche** | Econometrics; different tool ecosystem |
| Neg-binomial / zero-inflated / Tobit / Cox / GEE | Reg | **Niche** | Specialist; large surface for the return |
| Labelled-data smoothing | Both | **High-value (quiet)** | Workflow fit; determines whether import "just works" |

Reading the table: the four dealbreakers are what convert "tried it once" into "didn't stick" for
whole user segments. The two cheapest dealbreakers to close — **Cramér's V** and **defaulting
crosstab significance to a design-aware n** — would remove the two most common early bounces at
low cost. LaTeX/Word export and mixed models are larger investments but each unlocks a distinct
audience (paper-writers; multilevel modellers).

What tabxplor should *not* chase: the niche model families. That surface is huge, specialist, and
better served by the packages those users already run; matching it would dilute the crosstab-first
identity that is tabxplor's actual advantage.

---

## 7. What tabxplor already does better (so the gaps stay in proportion)

- Reference-relative, colour-coded crosstabs with per-cell CI significance — no competitor makes
  this the default, and it is genuinely more informative for exploration than a bare percentage.
- Banner / nested column tables (§3.2), often assumed missing.
- Survey-design regression as a first-class citizen (svyglm/svyolr/svy_vglm; Rao-Scott Wald;
  Nagelkerke for weighted models).
- A unified effect-shape model across families (β / OR / IRR / cumulative OR) with
  marginaleffects-based predicted-prob/AME folded into cells.
- Pseudo-R² trio + Brant PO test in the footer.
- Robust NA handling with an explicit NA level and valid-vs-total percent.
- `tab_counts()` to build tables from already-aggregated count/percentage input.
- A jamovi module (jmvtab / jmvtabreg) — a real answer to the SPSS "menu-driven" objection that
  no other package on this list offers.

The honest framing for the maintainer: tabxplor is **not** trying to be modelsummary or lme4, and
shouldn't. Its risk is losing the *ordinary survey analyst* over a handful of table-standard
expectations (effect size, design-aware significance, multiple response, column letters,
LaTeX/Word), not over exotic models. Close the four dealbreakers and the "didn't stick" rate for
its actual target audience drops sharply.

---

## Sources

Web research (competitor landscape and methodological standards):

- [expss — SPSS-style tables, nested banners, multiple response, weights, significance](https://gdemin.github.io/expss/)
- [pollster — weighted crosstab/topline for survey data](https://cran.r-project.org/web/packages/pollster/readme/README.html)
- [gtsummary — tbl_cross / tbl_svysummary / tbl_regression](https://www.danieldsjoberg.com/gtsummary/reference/tbl_cross.html)
- [modelsummary — side-by-side models, robust/clustered vcov](https://modelsummary.com/)
- [sjPlot — plot_model marginal effects](https://strengejacke.github.io/sjPlot/articles/plot_marginal_effects.html)
- [ggeffects — adjusted predictions / marginal effects](https://strengejacke.github.io/ggeffects/)
- [survey::svychisq — Rao-Scott corrected contingency tests](https://rdrr.io/rforge/survey/man/svychisq.html)
- [Exploring Complex Survey Data Analysis Using R — design effects, statistical testing](https://tidy-survey-r.github.io/tidy-survey-book/c06-statistical-testing.html)
- [srvyr — design effects (deff argument)](https://github.com/gergness/srvyr/issues/39)
- [IBM SPSS Custom Tables — column-proportion letters, multiple response](https://www.ibm.com/docs/en/SSLVMB_28.0.0/pdf/IBM_SPSS_Custom_Tables.pdf)
- [SPSS chi-square with pairwise z-tests (column letters, Bonferroni/BH)](https://www.spss-tutorials.com/spss-chi-square-test-with-pairwise-z-tests/)
- [Cramér's V / effect size for chi-square, APA reporting](https://statisticseasily.com/effect-size-for-chi-square/)
- [texreg / huxtable — LaTeX / Word / HTML regression tables](https://cran.r-project.org/web/packages/texreg/vignettes/texreg.pdf)
- [haven / labelled — SPSS/Stata value+variable labels in R](https://larmarange.github.io/labelled/articles/labelled.html)
- [Why social scientists stay on SPSS/Stata (institutional, reproducibility, specialization)](https://www.theanalysisfactor.com/choosing-statistical-software/)

Codebase evidence (verified this session): absence confirmed by `rg` for
`cramer|fisher.test|glmer|lmer|vif|clubSandwich|vcovCL|zeroinfl|glm.nb|coxph|feols|plm` (no
matches in `R/`); Kish `n_eff` opt-in at `R/tab-agg.R:123,165-175` and `R/utils.R:111-113`;
`color="contrib"` CA-style residuals at `R/fmt_class.R:1485-1491`; banner support at
`R/fmt_class.R:1662` / `R/tab_md.R:249-257`; model comparison at `R/tab_reg.R:2543-2546`;
pseudo-R² and Brant at `R/tab-test-display.R:124-131` / `R/tab_reg.R:665`.
