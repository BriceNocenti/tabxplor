# PURPOSE: the four example data sets, and their credits.
# ROLE: every vignette and man-page example runs on one of these. They are COMPLETE copies of data
#   published in other packages, saved with tabxplor's own level order, so example code shows
#   tabxplor rather than data preparation while a reader still finds every variable of the survey.
# KEY CONSTRAINTS:
#   - THE NAME CARRIES THE CREDIT: facto_ / questionr_ / car_ says where each came from, and means
#     attaching FactoMineR, questionr or carData beside tabxplor masks nothing.
#   - ONE editorial change, applied to all four: in a two-level yes/no factor the "yes" answer goes
#     FIRST, because tab() and tab_reg() model and show the first level. A few ordinary factors are
#     re-levelled by name on top of that; each @source says which.
#   - Each @source names the original package, its authors and its licence. All four are GPL (>= 2),
#     which tabxplor's GPL (>= 3) may redistribute. Say thank you, and keep saying it.
#   - Rebuilt by data-raw/DATASETS.R, the only place the source packages are needed.

#' Tea drinkers: when, where and why they drink it
#'
#' A survey of 300 tea drinkers, used here for its three **batteries of yes/no items** -- the shape a
#' multiple-answer question ("which of these apply to you?") arrives in. Six items say *when* people
#' drink tea, six say *where*, and fourteen say what they think it does for them. See
#' `vignette("tabxplor")` for what a battery does in a table, and [score_from_lv1()] for turning one
#' into a single summed score.
#'
#' @format A tibble of 300 rows and 36 columns. The ones the vignettes use:
#' \describe{
#'   \item{breakfast, tea.time, evening, lunch, dinner, always}{*When* do you drink tea?}
#'   \item{home, work, tearoom, friends, resto, pub}{*Where* do you drink tea?}
#'   \item{Sport}{Do you play a sport?}
#'   \item{SPC}{Socio-professional category, 7 levels.}
#'   \item{sex}{F or M.}
#' }
#' The other columns describe the tea itself (`Tea`, `How`, `sugar`, `how`, `where`, `price`,
#' `frequency`), the drinker (`age`, `age_Q`), and what they associate tea with (`healthy`,
#' `relaxing`, `exciting`, `slimming`, and ten more). Every two-level item reads "yes" first.
#'
#' @source The complete `tea` data of the \pkg{FactoMineR} package (Francois Husson, Julie Josse,
#'   Sebastien Le and Jeremy Mazet), GPL (>= 2) -- with thanks. tabxplor's copy changes only the
#'   level order: in each yes/no item the "yes" answer comes first, and its label loses the
#'   separator dot the original spells it with (`"Not.tea time"` becomes `"Not tea time"`).
#' @examples
#' tab(facto_tea, SPC, c(breakfast, evening), pct = "row", levels = "first", na = "drop")
"facto_tea"

#' Histoire de vie: leisure, work and beliefs in France, 2003
#'
#' A French national survey, used to show what **adjustment** does to a relationship: going to the
#' cinema is strongly patterned by occupation, and also by age -- and the two are entangled. See the
#' *All else equal* article for the analysis. It also carries a real sampling weight (`poids`), so it
#' is the data set to try `wt =` on; see `vignette("tabxplor-weights")`.
#'
#' @format A tibble of 2 000 rows and 20 columns. The ones the article uses:
#' \describe{
#'   \item{cinema}{Went to the cinema in the last 12 months? "Oui" first.}
#'   \item{qualif}{Occupational qualification, 7 levels, "Cadre" (senior professional) first.
#'     347 values are missing, as in the original.}
#'   \item{age}{Age in years, 18 to 97.}
#'   \item{poids}{The survey's own sampling weight.}
#' }
#' The rest describe the respondent (`sexe`, `nivetud`, `occup`, `freres.soeurs`), their views
#' (`clso`, `relig`, `trav.imp`, `trav.satisf`) and six more leisure activities (`sport`, `cuisine`,
#' `bricol`, `lecture.bd`, `peche.chasse`, `hard.rock`), plus `heures.tv` and `id`.
#'
#' @source The complete `hdv2003` data of the \pkg{questionr} package (Julien Barnier, Francois
#'   Briatte and Joseph Larmarange), GPL (>= 2) -- with thanks. It comes from the *Histoire de vie*
#'   survey run in 2003 by INSEE, the French national statistics institute. tabxplor's copy changes
#'   only the level order: every "Oui"/"Non" item reads "Oui" first, and `qualif` starts at "Cadre".
#' @examples
#' tab(questionr_hdv, qualif, cinema, pct = "row", na = "drop", color = "difference")
"questionr_hdv"

#' Marijuana-possession arrests in Toronto, 1997-2002
#'
#' 5 226 people arrested for possession of a small quantity of marijuana. The outcome is whether the
#' person was **released with a summons** rather than held. This is the running example of the *All
#' else equal* article: 86 % of white arrestees were released against 74 % of black arrestees, and
#' the article asks what survives of that gap when people alike on everything else are compared.
#'
#' @format A tibble of 5 226 rows and 8 columns.
#' \describe{
#'   \item{released}{Released with a summons? "Yes" first -- it is what the article studies.}
#'   \item{colour}{The arrestee's race as the police recorded it: "White" first, then "Black".}
#'   \item{year}{1997 to 2002.}
#'   \item{age}{Age in years.}
#'   \item{sex}{Female or Male.}
#'   \item{employed}{Employed? "Yes" first.}
#'   \item{citizen}{A Canadian citizen? "Yes" first.}
#'   \item{checks}{On how many of six police databases the person's name already appeared, 0 to 6.}
#' }
#' @source The complete `Arrests` data of the \pkg{carData} package (John Fox, Sanford Weisberg and
#'   Brad Price), GPL (>= 2) -- with thanks; gathered by Michael Friendly for a series in the
#'   *Toronto Star*. tabxplor's copy changes only the level order: the yes/no items read "Yes"
#'   first, and `colour` starts at "White".
#' @examples
#' tab(car_arrests, colour, released, pct = "row", color = "difference")
"car_arrests"

#' Salaries of US college professors, 2008-09
#'
#' Nine months' salary for 397 professors at one US college, collected by the institution to monitor
#' a pay gap. Used in the *All else equal* article to show a gap that **grows** under adjustment, and
#' then a **mediator**: rank explains the gap away, but rank is itself part of what is unequal.
#'
#' @format A tibble of 397 rows and 7 columns.
#' \describe{
#'   \item{rank}{AsstProf, AssocProf, then Prof.}
#'   \item{discipline}{"A" (theoretical) or "B" (applied).}
#'   \item{yrs.since.phd}{Years since the PhD.}
#'   \item{yrs.service}{Years of service.}
#'   \item{sex}{Female or Male.}
#'   \item{salary}{Nine-month salary, in US dollars.}
#'   \item{is_prof}{Full professor or not: the same information as `rank`, asked as the yes/no
#'     question a percentage can answer. Added by tabxplor.}
#' }
#' @source The complete `Salaries` data of the \pkg{carData} package (John Fox, Sanford Weisberg and
#'   Brad Price), GPL (>= 2) -- with thanks. tabxplor's copy adds `is_prof` and orders `rank` from
#'   assistant to full professor.
#' @examples
#' tab_reg(car_salaries, "salary", c("sex", "discipline"))
"car_salaries"
