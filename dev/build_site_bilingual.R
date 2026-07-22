# dev/build_site_bilingual.R -- build the bilingual pkgdown site (Last Phase w). .Rbuildignore'd.
#
#   Rscript dev/build_site_bilingual.R
#
# English site  -> docs/       (from _pkgdown.yml,     with a "FR" navbar link to fr/)
# French site   -> docs/fr/    (from _pkgdown.fr.yml,  with an "EN" navbar link back to ../)
#
# SCAFFOLD STATUS: pkgdown has no first-class bilingual support, so this builds the English site, then
# builds a French-localised site (pkgdown UI in French via `lang: fr`, translated reference-group titles
# / navbar) into a subdirectory via `override=`. The function REFERENCE pages stay English (R ships no
# bilingual .Rd -- help-page translation was declined). The French ARTICLES are the translated vignettes,
# added in the deferred vignette phase; consider `babeldown` there to translate article content in bulk.
# Until then the French site's narrative is English under a French shell.

stopifnot(requireNamespace("pkgdown", quietly = TRUE), requireNamespace("yaml", quietly = TRUE))

message("== English site -> docs/ ==")
pkgdown::build_site(".", devel = FALSE, preview = FALSE)

message("== French site  -> docs/fr/ ==")
fr <- yaml::read_yaml("_pkgdown.fr.yml")
# override merges over _pkgdown.yml: fr supplies destination/url/template(lang)/navbar/reference/articles.
pkgdown::build_site(".", override = fr, devel = FALSE, preview = FALSE, new_process = TRUE)

message("done. English: docs/index.html  |  French: docs/fr/index.html")
