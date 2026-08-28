# dev/build_site.R -- build the pkgdown site into docs/. .Rbuildignore'd.
#
#   Rscript dev/build_site.R
#
# The home page comes from pkgdown/index.md, rebuilt below from pkgdown/index.Rmd -- README.Rmd in
# colour -- so it can never be stale against the working tree.
# ONE English site (from _pkgdown.yml). The French translations of the vignettes are ordinary
# pkgdown ARTICLES (`vignettes/articles/*-fr.Rmd`, .Rbuildignore'd so web-only, never on CRAN),
# listed in the "En français" articles group; they render French legends via
# options(tabxplor.lang = "fr") in their setup chunk.
# DESIGN: no second, French site build -- the reference pages would be English either way, since R
#   ships no bilingual .Rd, so a language toggle would swap the articles alone.

stopifnot(requireNamespace("pkgdown", quietly = TRUE))

# WARNING: a SUBPROCESS, not source() -- index.Rmd's setup chunk pins tabxplor options and
# LANGUAGE for reproducibility, and those pins would otherwise leak into every example and
# article rendered after it. See dev/build_readmes.R.
if (system2(file.path(R.home("bin"), "Rscript"), c("dev/build_readmes.R", "index")) != 0L)
  stop("pkgdown/index.md could not be rebuilt")

# Wipe first: docs/ is git-ignored and never cleaned by a build, so pages from an earlier
# layout (a renamed article, a dropped topic) linger and make a local check lie.
pkgdown::clean_site(".")
pkgdown::build_site(".", devel = FALSE, preview = FALSE)
source("dev/site_prune.R")   # CLAUDE.md is not a site page -- see that file
message("done. Site: docs/index.html")
