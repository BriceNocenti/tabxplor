# tabxplor — French terminology glossary

The canonical French term for each recurring tabxplor concept, so the `.po` catalogue, the French
vignettes and the pkgdown site stay consistent. `.Rbuildignore`'d.

**Phase w-ii** translated the three vignettes into French, as web-only articles in
`vignettes/articles/*-fr.Rmd` (they render French legends via `options(tabxplor.lang = "fr")`). They
follow the terms below; new recurring choices settled while translating: *variable en ligne / en
colonne* (row / column variable), *variable expliquée* (outcome / dependent), *prédicteur* (predictor),
*champ* (fmt field), *case* (cell), *modélisé / observé* (modelised / empirical), *gabarit* (display
template). Column labels (`Obs_%`, `Model_OR`, …) and argument names stay English. The three rough
spots below are runtime-string issues (not vignette prose) and still await maintainer review.

## Rules

- **Argument names stay English** (`pct`, `ref`, `color`, `tab_vars`, …): the jamovi package teaches R
  progressively to French-speaking students, so only the *legend / help text* of an argument is French,
  never the argument itself. This glossary is for the *rendered* strings (legends, footers, summaries).
- **Notation and international abbreviations stay as-is**: `OR`, `IRR`, `β`, `AME`, `MER`, `AIC`, `BIC`,
  `Chi2`, `phi`, `eta2`, `R2`, `%`, `N`. A French statistics reader uses these directly.
- **French typography** (handled in code by `legend_num()` + the `lang == "fr"` branches): thin space
  before `; : ! ?`, decimal **comma** (`×1,5`). Keep this in every translation.
- **No edge whitespace inside `gettext()`** — `xgettext` strips it, so `"Model: "` would never match at
  runtime. Punctuation/spacing lives in the `gettextf()` template or is added outside the call.

## Core terms

| English                        | French                              |
|--------------------------------|-------------------------------------|
| cross-table / cross-tabulation | tableau croisé                      |
| cell(s)                        | case(s)                             |
| row / column                   | ligne / colonne                     |
| Total (row)                    | Total (kept)                        |
| whole-table total ("Ensemble") | Ensemble (kept)                     |
| Others (lumped level)          | Autres                              |
| reference category             | modalité de référence               |
| ref. (short)                   | réf.                                |
| weighted / weighted by         | pondéré / pondéré par               |
| significant / non-significant  | significatif / non significatif     |
| significance                   | significativité                     |
| confidence interval            | intervalle de confiance             |
| confidence level               | seuil de confiance                  |
| margin of error                | marge d'erreur                      |
| over- / under-represented      | sur-représenté / sous-représenté    |
| difference                     | différence                          |
| standardized difference        | différence standardisée             |
| ratio                          | rapport                             |
| contribution to Chi2           | contribution au Chi2                |
| effect size                    | taille d'effet                      |
| p-value                        | pvalue                              |
| shades of blue / yellow to red | nuances de bleu / du jaune au rouge |
| vs the mean                    | /la moyenne (par rapport à)         |

## Weights and survey-design terms

Settled in Phase 18z16-iiiiii, when the Weights section of both intro vignettes was rewritten.
The three earlier renderings of *effective sample size* (`taille d'échantillon efficace`,
`n effectif`, none at all) are now one.

| English                      | French                                                   |
|------------------------------|----------------------------------------------------------|
| weight / weighting           | pondération                                              |
| survey design                | plan de sondage (`survey-design` where it names a class) |
| the three weighting levels   | les trois niveaux de pondération                         |
| design effect                | effet de plan / *design effect*                          |
| effective sample size        | taille d'échantillon effective (short: *n* effectif)     |
| cluster / clustered          | grappe / en grappes                                      |
| stratum / stratified         | strate / stratifié                                       |
| calibration                  | calage sur marges                                        |
| finite population correction | correction de population finie (notation `fpc` kept)     |
| PSU (primary sampling unit)  | unité primaire (notation `psu` kept)                     |
| degrees of freedom           | degrés de liberté                                        |
| margin of error              | marge d'erreur                                           |
| face-to-face survey          | enquête en face-à-face                                   |
| replicate weights            | poids de réplication                                     |

## Regression terms

| English                            | French                                               |
|------------------------------------|------------------------------------------------------|
| linear regression                  | régression linéaire                                  |
| logistic regression                | régression logistique                                |
| Poisson / quasi-Poisson regression | régression de Poisson / quasi-Poisson                |
| multinomial logistic regression    | régression logistique multinomiale                   |
| ordinal logistic regression        | régression logistique ordinale                       |
| odds ratio(s)                      | rapport(s) de cotes (notation `OR` kept)             |
| incidence-rate ratio(s)            | rapport(s) de taux d'incidence (notation `IRR` kept) |
| log-odds / log-rate coefficients   | coefficients log-cotes / log-taux                    |
| cumulative odds ratios             | rapports de cotes cumulés                            |
| proportional-odds model            | modèle à cotes proportionnelles                      |
| marginal effects (AME / MER)       | effets marginaux (notation `AME`/`MER` kept)         |
| response / probability scale       | échelle de réponse / de probabilité                  |
| sample-averaged                    | moyenne sur l'échantillon                            |
| reference profile                  | profil de référence                                  |
| models comparison                  | comparaison de modèles                               |
| tabbed by (split_var)              | ventilé par                                          |
| Residual SD                        | Écart-type résiduel                                  |
| vs null / baseline / previous      | vs nul / vs référence / vs précédent                 |

### The estimand cascade (`family` → `link` → `measure` → `effect`)

The argument names themselves are code and stay English; what is translated is the prose around them.

| English                                | French                                     |
|----------------------------------------|--------------------------------------------|
| deviation (the umbrella)               | écart                                      |
| measure (of deviation)                 | mesure (de l'écart)                        |
| link (the measure the model estimates) | lien (la mesure que le modèle estime)      |
| effect (where the number comes from)   | effet (d'où le nombre est tiré)            |
| conditional effect                     | effet conditionnel                         |
| marginal effect / sample-averaged      | effet marginal / moyenné sur l'échantillon |
| at the reference profile               | au profil de référence                     |
| risk ratio / risk difference           | rapport de risques / différence de risques |
| ratio of means                         | rapport de moyennes                        |
| marginal odds ratio                    | rapport de cotes marginal                  |
| modified Poisson (regression)          | (régression de) Poisson modifiée           |
| collapsible / non-collapsible          | collapsible / non collapsible              |
| adjusted prediction                    | prédiction ajustée                         |
| observed / crude  (companion) column   | colonne observée / brute                   |

⚠ A **quoted cell value** keeps the decimal point the table actually prints (`1/2.43`, `÷1.7`), never
a French comma — it is a quotation of the output. Ordinary prose numbers stay French (`19,8 points`).

### The footer legend's measure names

The colour legend names its measure in words, per (measure × ladder scale). These are the **legend's**
names — a crosstab has no acronym header to lean on — while `REG_WORDS` keeps the discipline's own
short names for the regression headers (`RD`, `RR`, `mRR`, …) and their expansions.

| measure × scale        | English                             | French                                           |
|------------------------|-------------------------------------|--------------------------------------------------|
| difference × pct_diff  | percentage points (risk) difference | différence de points de pourcentage (de risques) |
| difference × mean_diff | mean difference                     | différence de moyennes                           |
| … standardized         | standardized mean difference        | différence de moyennes standardisée              |
| difference × log_odds  | log-odds difference                 | différence de log-cotes                          |
| ratio × pct_ratio      | relative risk (ratio)               | risque relatif (ratio)                           |
| ratio × mean_ratio     | ratio of means                      | rapport de moyennes                              |
| odds_ratio             | odds ratio                          | rapport de cotes                                 |
| contrib                | contribution to Chi2                | contribution au Chi2                             |
| contrib (guaranteed)   | standardized residual               | résidu standardisé                               |

Each name carries **both** the discipline's term and the base measure, so a reader meeting either word
lands on the same quantity. « rapport de cotes » is the settled term for the odds ratio (not « rapport
de chances »); « différence de risques » is avoided in favour of « différence de proportion ».

⚠ **The `guaranteed_effect` head is ONE msgid PER MEASURE**, not a shared `"%s-guaranteed %s"`
template: *garanti* agrees with the measure (*différence … garantie* vs *rapport … garanti* vs
*risque relatif … garanti*), which no single format string can do. `MEASURES$<m>$by_scale$<scale>$word_guar`
declares each one; French then writes the agreement out in full.

⚠ **The legend uses ordinary spaces before `;` and `:`**, not non-breaking ones: that is what the
assemblers emit (`colon <- " : "`) and what every existing msgstr uses, and the tests match on it.

### Settled by the Phase 22f-i review

| English                                                  | French                                                                                                |
|----------------------------------------------------------|-------------------------------------------------------------------------------------------------------|
| risk ratio (`REG_WORDS$RR`)                              | risque relatif *(not « rapport de risques »)*                                                         |
| risk difference (`REG_WORDS$RD`)                         | différence de risque                                                                                  |
| over-/under-represented                                  | sur-représentée / sous-représentée *(agrees with « case »)*                                           |
| ref (the short reference, 2nd use on)                    | réf                                                                                                   |
| adjusted/predicted proportion                            | proportion prédite/ajustée                                                                            |
| on adjusted proportions                                  | sur les proportions ajustées                                                                          |
| other predictors held at their reference level (or mean) | les autres prédicteurs fixés à leur modalité de référence (ou à leur moyenne)                         |
| or under the first colour threshold                      | ou en-dessous du premier seuil de couleur                                                             |
| Design-based (survey): …                                 | Estimations, intervalles et tests pondérés tiennent compte du plan d'échantillonnage (survey-design). |

⚠ « odds ratio » is **one msgid** shared by the crosstab legend and the regression `Model:` line, so
it cannot be glossed « rapport de cotes (odds) » in one and not the other: it stays
**« rapport de cotes »** everywhere. The acronym `OR` is the legend's subject anyway.

## Known first-draft rough spots (for maintainer review)

- **Reg caption** (`reg_title`): the `": "` after the family name keeps an English colon
  ("Régression logistique: y selon race") — not templated to avoid a too-generic `"%s: %s"` msgid.
  The footer "Modèle : …" line has full French typography. Refine if desired.
- **Comparison title plural**: `"%s (models comparison): %s"` receives the family name with an English
  `"s"` suffix appended (`Fam + "s"`), so French shows "Régression logistiques (…)". Byte-identity with
  English forbids restructuring; adjust in the catalogue/source if a cleaner French plural is wanted.
- **Tooltips** (`diff`/`std diff`/`mean_ctr`/`contrib`): translated as abbreviations; they follow the
  **ambient locale** (the tooltip builder is not under `with_legend_lang`, so a per-call `lang="fr"` on
  an English-locale machine reaches the footer but not the hover text). Pure notation (`ci`/`ratio`/
  `OR`/`n`/`sd`) is intentionally left English.
