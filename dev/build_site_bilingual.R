# dev/build_site_bilingual.R -- build the bilingual pkgdown site (Last Phase w). .Rbuildignore'd.
#
#   Rscript dev/build_site_bilingual.R
#
# English site  -> docs/       (from _pkgdown.yml,     with a "FR" navbar link to fr/)
# French site   -> docs/fr/    (from _pkgdown.fr.yml,  with an "EN" navbar link back to ../)
#
# STATUS: pkgdown has no first-class bilingual support and reads its config ONLY from `_pkgdown.yml`
# (there is no config-file argument). The earlier `build_site(override = read_yaml("_pkgdown.fr.yml"))`
# approach applied SOME French settings (destination, template lang, articles) but NOT the translated
# `reference:` group titles -- the French reference index came out with English headings. So the French
# site is now built by TEMPORARILY swapping `_pkgdown.fr.yml` in as `_pkgdown.yml` (backup + guaranteed
# restore via on.exit), so pkgdown reads the full French config directly. `_pkgdown.fr.yml` supplies
# `destination: docs/fr` and `url`, so the French site lands in docs/fr/ with a French UI, French
# reference-group titles / navbar, and the French `articles:` order.
#
# The function REFERENCE PAGES themselves stay English (R ships no bilingual .Rd -- help-page translation
# was declined); only the index group titles/descriptions are French. The French ARTICLES are the
# translated vignettes in `vignettes/articles/*-fr.Rmd` (.Rbuildignore'd, so web-only, never on CRAN);
# they render French legends via `options(tabxplor.lang = "fr")` in their setup chunk. NOTE: pkgdown
# builds every .Rmd under vignettes/ into BOTH trees, so each site also carries the other language's
# article pages unlinked (harmless -- the navbar surfaces each site's own language).

stopifnot(requireNamespace("pkgdown", quietly = TRUE))

message("== English site -> docs/ ==")
pkgdown::build_site(".", devel = FALSE, preview = FALSE)

message("== French site  -> docs/fr/ ==")
# Build with _pkgdown.fr.yml swapped in as _pkgdown.yml. A function scope + on.exit guarantees the
# English config is restored whether the build succeeds or errors (a hard kill mid-build leaves the
# backup at the path printed below for manual recovery).
build_french_site <- function() {
  cfg    <- "_pkgdown.yml"
  cfg_fr <- "_pkgdown.fr.yml"
  backup <- tempfile("pkgdown_en_config_", fileext = ".yml")
  stopifnot(file.copy(cfg, backup, overwrite = TRUE))
  on.exit(
    {
      if (!file.copy(backup, cfg, overwrite = TRUE)) {
        warning("Could not restore ", cfg, " -- English config backed up at: ", backup)
      }
    },
    add = TRUE
  )
  stopifnot(file.copy(cfg_fr, cfg, overwrite = TRUE))
  # new_process = TRUE: a fresh R subprocess re-reads _pkgdown.yml from disk (now the French config).
  pkgdown::build_site(".", devel = FALSE, preview = FALSE, new_process = TRUE)
}
build_french_site()

message("done. English: docs/index.html  |  French: docs/fr/index.html")
