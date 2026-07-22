# tabxplor — French terminology glossary

The canonical French term for each recurring tabxplor concept, so the `.po` catalogue (and, later, the
French vignettes + pkgdown site) stay consistent. `.Rbuildignore`'d. Written for Last Phase w.

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

| English                        | French                                             |
|--------------------------------|----------------------------------------------------|
| cross-table / cross-tabulation | tableau croisé                                     |
| cell(s)                        | case(s)                                            |
| row / column                   | ligne / colonne                                    |
| Total (row)                    | Total (kept)                                        |
| whole-table total ("Ensemble") | Ensemble (kept)                                     |
| Others (lumped level)          | Autres                                             |
| reference category             | modalité de référence                              |
| ref. (short)                   | réf.                                               |
| weighted / weighted by         | pondéré / pondéré par                              |
| significant / non-significant  | significatif / non significatif                    |
| significance                   | significativité                                    |
| confidence interval            | intervalle de confiance                            |
| confidence level               | seuil de confiance                                 |
| margin of error                | marge d'erreur                                     |
| over- / under-represented      | sur-représenté / sous-représenté                   |
| difference                     | différence                                         |
| standardized difference        | différence standardisée                            |
| ratio                          | rapport                                            |
| contribution to Chi2           | contribution au Chi2                               |
| effect size                    | taille d'effet                                     |
| p-value                        | p-valeur                                           |
| shades of blue / yellow to red | nuances de bleu / du jaune au rouge                |
| vs the mean                    | p. r. à la moyenne (par rapport à)                 |

## Regression terms

| English                                | French                                                |
|----------------------------------------|-------------------------------------------------------|
| linear regression                      | régression linéaire                                   |
| logistic regression                    | régression logistique                                 |
| Poisson / quasi-Poisson regression     | régression de Poisson / quasi-Poisson                 |
| multinomial logistic regression        | régression logistique multinomiale                    |
| ordinal logistic regression            | régression logistique ordinale                        |
| odds ratio(s)                          | rapport(s) de cotes (notation `OR` kept)              |
| incidence-rate ratio(s)                | rapport(s) de taux d'incidence (notation `IRR` kept)  |
| log-odds / log-rate coefficients       | coefficients log-cotes / log-taux                     |
| cumulative odds ratios                 | rapports de cotes cumulés                             |
| proportional-odds model                | modèle à cotes proportionnelles                       |
| marginal effects (AME / MER)           | effets marginaux (notation `AME`/`MER` kept)          |
| response / probability scale           | échelle de réponse / de probabilité                   |
| sample-averaged                        | moyenne sur l'échantillon                             |
| reference profile                      | profil de référence                                   |
| models comparison                      | comparaison de modèles                                |
| tabbed by (split_var)                  | ventilé par                                           |
| Residual SD                            | Écart-type résiduel                                   |
| vs null / baseline / previous          | vs nul / vs référence / vs précédent                  |

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
