# PURPOSE: build tabxplor's four example data sets from their original packages.
# ROLE: run by hand, never at build time. Each data set is the COMPLETE original, saved with one
#   editorial change -- tabxplor's own level order -- so example code shows tabxplor rather than
#   data preparation, while a curious reader still finds every variable of the survey.
# USAGE: Rscript data-raw/DATASETS.R      (needs FactoMineR, questionr, carData installed)
#
# THE ONE RULE, applied to all four: in a two-level yes/no factor, the "yes" answer goes FIRST,
#   because tab() and tab_reg() keep the first level as the one they model and show. Its label also
#   loses the separator dot the original spells it with ("Not.tea time" -> "Not tea time").
#   A handful of ordinary factors are re-levelled by name on top of that, where the examples want a
#   particular reference; each is spelt out below.
#
# WARNING: every shipped name carries its SOURCE PACKAGE as a prefix -- facto_ / questionr_ / car_.
#   That credits the origin wherever the data is used, and it means attaching FactoMineR, questionr
#   or carData beside tabxplor masks nothing.
#
# See: R/data.R for each data set's documentation and its full source credit.

stopifnot(requireNamespace("FactoMineR", quietly = TRUE),
          requireNamespace("questionr", quietly = TRUE),
          requireNamespace("carData",   quietly = TRUE))

# The rule, as one function: a two-level factor with exactly one NEGATIVE level is a yes/no item.
yes_first <- function(f) {
  if (!is.factor(f)) return(f)
  lv <- levels(f)
  if (length(lv) != 2L) return(f)
  neg <- lv %in% c("No", "Non") | grepl("^(Not|No)[.[:space:]]", lv)
  if (sum(neg) != 1L) return(f)
  forcats::fct_relabel(forcats::fct_relevel(f, lv[!neg]), ~ gsub(".", " ", ., fixed = TRUE))
}
grab <- function(nm, pkg) { e <- new.env(); utils::data(list = nm, package = pkg, envir = e); e[[nm]] }

# --- facto_tea: 300 tea drinkers, three batteries of yes/no items --------------------------------
# FactoMineR (Francois Husson, Julie Josse, Sebastien Le, Jeremy Mazet), GPL (>= 2).
facto_tea <- tibble::as_tibble(grab("tea", "FactoMineR"))
facto_tea <- dplyr::mutate(facto_tea, dplyr::across(dplyr::everything(), yes_first))

# --- questionr_hdv: the French INSEE "Histoire de vie" survey, 2003 ------------------------------
# questionr (Julien Barnier, Francois Briatte, Joseph Larmarange), GPL (>= 2).
questionr_hdv <- tibble::as_tibble(grab("hdv2003", "questionr"))
questionr_hdv <- dplyr::mutate(questionr_hdv, dplyr::across(dplyr::everything(), yes_first),
                     # "Cadre" (senior professional) is the reference the examples compare with
                     qualif = forcats::fct_relevel(qualif, "Cadre"))

# --- car_arrests: Toronto marijuana-possession arrests, 1997-2002 --------------------------------
# carData (John Fox, Sanford Weisberg, Brad Price), GPL (>= 2); gathered by Michael Friendly.
car_arrests <- tibble::as_tibble(grab("Arrests", "carData"))
car_arrests <- dplyr::mutate(car_arrests, dplyr::across(dplyr::everything(), yes_first),
                         colour = forcats::fct_relevel(colour, "White"))

# --- car_salaries: US college salaries, 2008-09 --------------------------------------------------
# carData (John Fox, Sanford Weisberg, Brad Price), GPL (>= 2).
car_salaries <- tibble::as_tibble(grab("Salaries", "carData"))
car_salaries <- dplyr::mutate(car_salaries,
  rank    = forcats::fct_relevel(rank, "AsstProf", "AssocProf", "Prof"),
  # the same information as `rank`, asked as the yes/no question a percentage can answer
  is_prof = factor(ifelse(rank == "Prof", "Full professor", "Not yet"),
                   levels = c("Full professor", "Not yet")))

# Plain save(), not usethis::use_data(): this script must run with nothing installed beyond the
# three source packages.
dir.create("data", showWarnings = FALSE)
for (nm in c("facto_tea", "questionr_hdv", "car_arrests", "car_salaries")) {
  save(list = nm, file = file.path("data", paste0(nm, ".rda")), compress = "xz", version = 3)
  cat(sprintf("%-13s %4d x %2d  %6.1f KB\n", nm, nrow(get(nm)), ncol(get(nm)),
              file.size(file.path("data", paste0(nm, ".rda"))) / 1024))
}
