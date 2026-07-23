# dev/build_site.R -- build the pkgdown site into docs/. .Rbuildignore'd.
#
#   Rscript dev/build_site.R
#
# One English site (from _pkgdown.yml). The French translations of the vignettes are ordinary
# pkgdown ARTICLES (`vignettes/articles/*-fr.Rmd`, .Rbuildignore'd so web-only, never on CRAN),
# listed in the "En français" articles group -- there is no second French site build any more
# (the bilingual config-swap machinery of the former dev/build_site_bilingual.R was removed:
# the language toggle was dropped, and the reference pages were always English anyway, since R
# ships no bilingual .Rd). The FR articles render French legends via options(tabxplor.lang = "fr")
# in their setup chunk.

stopifnot(requireNamespace("pkgdown", quietly = TRUE))
pkgdown::build_site(".", devel = FALSE, preview = FALSE)
message("done. Site: docs/index.html")
