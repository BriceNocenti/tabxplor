# dev/build_site.R -- build the pkgdown site into docs/. .Rbuildignore'd.
#
#   Rscript dev/build_site.R
#
# The home page comes from pkgdown/index.md (README.Rmd in colour, knitted the same way).
# One English site (from _pkgdown.yml). The French translations of the vignettes are ordinary
# pkgdown ARTICLES (`vignettes/articles/*-fr.Rmd`, .Rbuildignore'd so web-only, never on CRAN),
# listed in the "En français" articles group -- there is no second French site build any more
# (the bilingual config-swap machinery of the former dev/build_site_bilingual.R was removed:
# the language toggle was dropped, and the reference pages were always English anyway, since R
# ships no bilingual .Rd). The FR articles render French legends via options(tabxplor.lang = "fr")
# in their setup chunk.

stopifnot(requireNamespace("pkgdown", quietly = TRUE))
# Wipe first: docs/ is git-ignored and never cleaned by a build, so pages from an earlier
# layout (a renamed article, a dropped topic) linger and make a local check lie.
pkgdown::clean_site(".")
pkgdown::build_site(".", devel = FALSE, preview = FALSE)
source("dev/site_prune.R")   # CLAUDE.md is not a site page -- see that file
message("done. Site: docs/index.html")
