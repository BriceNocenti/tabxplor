# tabxplor — French terminology glossary

The canonical French term for each recurring tabxplor concept, so the `.po` catalogue, the French
vignettes and the pkgdown site stay consistent. `.Rbuildignore`'d.

The French vignettes are web-only articles (`vignettes/articles/*-fr.Rmd`, rendering French legends
via `options(tabxplor.lang = "fr")`) and follow the tables below. Column labels (`Obs_%`,
`Model_OR`, …) and argument names stay English.

⚠ **The whole French set is written in one register: `on` for the generic rule, `nous` for the shared
analytic move, and *zero* `vous` / `votre` / `vos` — headings included.** Settled in Phase 23f-i for
the article and extended to the four reference pages in 23f-ii, so a reader moving between them meets
one voice. The single exception is a **quoted questionnaire item**, which addresses a respondent and
therefore keeps `vous` (« parmi ces situations, lesquelles vous concernent ? », « où buvez-vous du
thé ? »). An imperative addressed to the reader is rewritten, not merely softened: « Donnez une valeur
unique » → « Une valeur unique la change » ; « Vérifiez ce que contient votre fichier » → « Vérifier ce
que contient le fichier ».

⚠ **§ *Le vocabulaire d'enseignement* is the section to read before writing any French teaching
prose about regression.** Its primary source is the maintainer's own logit séance
(`~/github/formations_stat/M2_06_07.Rmd`) rather than a dictionary: these are the words the target
readership already has. It records, deliberately, the two places where one concept keeps **several**
French words on purpose — crude/adjusted, and the reference.

## Rules

- **Argument names stay English** (`pct`, `ref`, `color`, `tab_vars`, …): the jamovi package teaches R
  progressively to French-speaking students, so only the *legend / help text* of an argument is French,
  never the argument itself. This glossary is for the *rendered* strings (legends, footers, summaries).
- ⚠ **In teaching prose, a French term that names an argument is given WITH its English code name**
  (« la **variable à expliquer** — `outcome` »), at the point where the French word is coined and
  nowhere else. The reader thinks in French and types in English, and an argument that is only ever
  named in French cannot be used. The same names serve jamovi, whose options mirror the arguments.
  Settled in Phase 23f-i; applied to both *Reading a regression* twins.
- **Notation and international abbreviations stay as-is**: `OR`, `IRR`, `β`, `AME`, `MER`, `AIC`, `BIC`,
  `Chi2`, `phi`, `eta2`, `R2`, `%`, `N`. A French statistics reader uses these directly.
- **French typography** (handled in code by `legend_num()` + the `lang == "fr"` branches): thin space
  before `; : ! ?`, decimal **comma** (`×1,5`). Keep this in every translation.
- **No edge whitespace inside `gettext()`** — `xgettext` strips it, so `"Model: "` would never match at
  runtime. Punctuation/spacing lives in the `gettextf()` template or is added outside the call.
  ⚠ **The same rule covers jamovi's `_()` and `jmvcore::.()`**, for a different reason: jamovi looks a
  msgid up TRIMMED while the compiler files it as written, so `_(" trials")` can never match. Put the
  space outside the call.
- ⚠ **jamovi has no plural and no context at runtime.** Only `window._` exists (not `n_`, not `_p`),
  and it returns the first msgstr — so a sentence that needs a branch is written as TWO FULL msgids,
  never as a translated noun spliced into an English frame. See
  `dev/tabxplor_2.0.0_jamovi_dev.md` § 19.1.

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

## Le vocabulaire d'enseignement (article « Toutes choses égales par ailleurs »)

Settled in Phase 23f-i, for the French twin of `vignettes/tabxplor-reading-a-regression.Rmd`. The
readership is "literary" social-science students, so the primary source is **the maintainer's own
logit séance** (`~/github/formations_stat/M2_06_07.Rmd:310-2100`) — those are the words these
students already have — checked against Cibois (*Les méthodes d'analyse d'enquêtes*, ch. V),
Deauvieau (*BMS* 2010), Larmarange's *guide-R*, INSEE and the OQLF.

**The register**: « on » for the generic rule, « nous » for the shared analytic move, no *vous* and no
*tu*. That is the séance's own register (measured: on ≈ 73, nous ≈ 36, vous ≈ 18, tu = 0) minus its
classroom half — an article gives no TD instructions, so *vous* drops out on its own.

**Four devices taken from the séance**: « **Lecture : …** » (the INSEE reading-key, which is what the
English article's blockquotes are) · « Rappel : » / « Attention : » / « Note : » as callout registers ·
reading a table in order (source, champ, variables, Total, l'intérieur) · median-dot inclusive forms
(« enquêté·es »).

### Les mots du raisonnement

| English                  | French                                                        | note                                              |
|--------------------------|---------------------------------------------------------------|---------------------------------------------------|
| outcome                  | variable à expliquer *(aussi : variable dépendante)*          | ⚠ the printed label stays « Variable expliquée »  |
| predictor                | prédicteur / variable explicative                             | his own gloss: « on s'en sert pour prédire »      |
| the modelled level       | la modalité étudiée                                           | « placée en premier »                             |
| reference                | modalité de référence / catégorie de référence / la référence | a family, per context                             |
| reference profile        | profil de référence                                           | printed                                           |
| anchor value             | valeur d'ancrage                                              |                                                   |
| deviation                | écart                                                         | continuous with his « écart à la moyenne »        |
| measure of deviation     | mesure de l'écart                                             |                                                   |
| crude / observed         | effet brut · effet observé · effet empirique                  | family; the column is « la colonne observée »     |
| adjusted                 | effet ajusté · effet modélisé                                 | family; « la colonne du modèle »                  |
| *(effet net)*            | mentioned once only                                           | canonical in French sociology, too abstract       |
| adjustment (the move)    | l'ajustement                                                  | printed (`ajustement : …`)                        |
| all else equal           | « toutes choses égales par ailleurs », always in guillemets   | see the pair below                                |
| all else *un*equal       | « toutes choses *inégales* par ailleurs », *inégales* italic  | his own; names the observed column                |
| the honest long form     | « toutes les autres variables explicatives choisies étant égales » | his own, `M2_06_07:517`                      |
| composition effect       | effet de structure                                            | INSEE                                             |
| holding composition      | à structure constante                                         | INSEE                                             |
| the base                 | la base                                                       | (`base_ci` → « base + IC ») |
| the round trip           | l'aller-retour · « redescendre au pourcentage »               |                                                   |
| untangling correlations  | démêler les corrélations cachées                              | his own, `M2_06_07:452`                           |
| significance-as-permission | une autorisation · « le droit de généraliser »              | *mot d'excuse* does not travel                    |
| confounding              | facteur de confusion                                          | preferred over *confondant* (Inserm)              |
| mediator                 | variable médiatrice                                           |                                                   |
| collider                 | facteur de collision                                          | by analogy; *collisionneur* reads as physics      |
| overcontrol bias         | le surajustement                                              | French epidemiology's one-word term               |
| Table 2 fallacy          | le « piège du tableau 2 »                                     | English name once, it is the searchable term      |
| ideal type               | l'idéal-type                                                  | Weber; first-year vocabulary in France            |

**The pair that names the two columns.** `M2_06_07:1830-1834` already uses it as a column legend, and
it is the French name of this whole article's subject: an **observed** effect is read *toutes choses
inégales par ailleurs*, a **modelled** one *toutes les autres variables choisies étant égales*.

### Les mots du modèle

| English                | French                                                      | note                                            |
|------------------------|-------------------------------------------------------------|-------------------------------------------------|
| to fit a model         | ajuster · estimer · réaliser · calculer · faire tourner      | rotate; see the rule below                      |
| a fitted model         | le modèle ajusté / le modèle réalisé                        |                                                 |
| model fit (the block)  | bilan du modèle *(métaphore : bilan de santé)*              | ⚠ NOT « ajustement du modèle »                  |
| to hold equal          | maintenir constant · raisonner à variables constantes       | his own, `M2_06_07:452`                         |
| held equal (short)     | « à emploi égal », « à âge et CSP égales »                  | his own refrain                                 |
| "controlling for X"    | comparer des personnes qui se ressemblent sur X             | ⚠ « contrôler » also means *vérifier*           |
| predicting (refused)   | pronostiquer                                                | ⚠ the footer prints « proportion prédite »      |
| marginal effect        | effet marginal · effet moyenné · effet moyenné sur l'échantillon | alternate; see faux amis                   |
| conditional effect     | effet conditionnel                                          |                                                 |
| risk difference        | différence de proportion · différence de points de pourcentage | « différence de risque » is medical-flavoured |
| effect persists        | son effet persiste                                          | his own, `M2_06_07:1858`                        |
| explained away         | en partie / totalement expliquée par d'autres variables     | his own, idem                                   |
| effect grows           | son effet avait été dilué dans le tableau croisé            | his own, idem                                   |
| bundles                | elles vont par paquets                                      | ⚠ never *par grappes* (cluster sampling)        |
| different bookkeeping  | les mêmes personnes, une autre manière de les compter       |                                                 |

⚠ **« ajusté » carries two senses and both are kept** — a fitted *model* and an adjusted *percentage*.
The single guard: **never both senses in one sentence.** The séance itself avoids the problem by using
« réaliser un modèle » / « faire tourner un modèle » and reserving « calculer » for the odds
arithmetic; « réaliser » is therefore the safest of the five when a sentence is already about
adjustment.

### L'odds ratio : deux routes vers la même distinction

The hard case, and the reason this section exists. In ordinary French « **n fois moins de chances** »
reads as a *risk ratio*, so an odds ratio needs disambiguating. There are two routes and the article
teaches both:

- **La clause** — « 1,48 fois moins de chances d'avoir été relâché *plutôt que de ne pas l'avoir
  été* ». Sans la clause finale, la même phrase énonce un risque relatif.
- **Le nom** — « sa *cote* est 1,48 fois plus faible ». Le mot *cote* porte à lui seul la distinction,
  et la phrase se raccourcit.
- **Le risque relatif**, lui, s'énonce sans rien : « 1,1 fois moins de chances d'être relâché ».

**The clause is primary** — it is the séance's own template (`M2_06_07:515`), and the more striking of
the two. **The noun is the fallback and the disambiguator.** The rule: *the clause may be dropped only
when « cote » carries the distinction instead* — never neither, never both at once.

**« cote » is taught once, from the racetrack**, as the séance already does (`:682-717`, quoting
Cibois on the etymology of *odd*): « tel cheval est coté à 3 contre 1 » = sa probabilité de gagner est
3 fois plus grande que sa probabilité de perdre. And the inversion rule (`:717`): below 1 one takes
the inverse *and* inverts the wording — « X fois **moins** de chances ».

⚠ **Terminology stays « rapport de cotes »**, the OQLF's preferred term and one shared msgid (see the
warning above). The séance prefers « **rapport de chances** » (`:821`) and the article says once that
this is the commoner spoken form — but the table prints « rapport de cotes », so that is what the
prose works with.

### Faux amis et mots déjà pris

1. ⚠ **« effet marginal » is a false friend for exactly this readership.** Cibois uses it for *an
   effect expressed in percentage points*; tabxplor's `effect = "marginal"` means *averaged over the
   sample*. Defuse at first use. The compensation: the etymology (« la même *marge* que les marges
   d'un tableau croisé ») is **stronger in French**, since « marge » is literally a Total row's margin.
2. ⚠ **« écart » is locked as *deviation*** — so Deauvieau's « écart pur / écart net / écart
   expérimental » are **cited, not adopted**: using them would make *écart* mean the adjusted effect.
3. ⚠ **« significatif » means *important* in ordinary French.** Say so out loud — English has the same
   trap and no French teacher has ever had to name it.
4. ⚠ **« grappe » is taken** by cluster sampling (see the weights section) → « paquets ».
5. ⚠ **« contrôler » is worse in French than in English** — it also means *vérifier*. The article says
   so; it is a point the English version cannot make.
6. ⚠ **A quoted cell value keeps the printed decimal point** (`1/1.48`, `par 1.54 (SD)`); prose numbers
   take the comma (« 5,2 points »). Same rule as the warning after the estimand cascade.

## Le vocabulaire des pages de référence

Settled in Phase 23f-ii, when the four reference pages were re-authored in the article's voice. Only
what the article did not already fix.

| English                        | French                                          | note                                                     |
|--------------------------------|-------------------------------------------------|----------------------------------------------------------|
| outcome (in prose)             | variable à expliquer                            | ⚠ NOT « variable expliquée », which is the *printed* label |
| footer statistics (the block)  | le bilan du modèle                              | ⚠ NOT « le résumé statistique »; see *bilan de santé*    |
| to fit (a model)               | estimer · ajuster · réaliser                    | rotate; « estimé » where the sentence is about adjustment |
| model checks                   | les vérifications du modèle                     |                                                          |
| effect modification            | la modification d'effet                         | not « l'interaction », which names the *term*            |
| the shape table                | le tableau des formes                           | printed under the footer                                 |
| observed range                 | l'étendue observée                              |                                                          |
| win ratio                      | rapport de victoires (*win ratio*)              | English name once — it is the searchable term            |
| superiority probability        | probabilité de supériorité                      | Somers' `D` read as a probability                        |
| display grammar / template     | la grammaire `display` / le gabarit             |                                                          |
| named layout                   | la mise en page nommée                          |                                                          |
| primary token                  | le jeton primaire                               | « le premier jeton **hors crochets** »                   |
| aside (a bracketed field)      | une mention secondaire                          |                                                          |
| the base count                 | l'effectif de base                              |                                                          |
| three levels (of margin of error) | les trois niveaux                            | the weights vignette's spine                             |

⚠ **`RR` is « risque relatif », never « rapport de risques »** (22f-i) — a rule the pre-23f-ii twin
broke in a heading. The distinct `or` field is glossed « rapport de cotes / rapport de **risques
relatifs** » (the multinomial RRR), which is a different quantity and keeps its own name.

⚠ **A section reporting `measure = "ratio"` says « combien de fois plus *souvent* ? », not « plus de
*chances* ? »** — that section reports a risk ratio, and « chances » is exactly the word the odds-ratio
rule reserves.

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
