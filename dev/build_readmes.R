# PURPOSE: knit the two hand-written home pages from their .Rmd sources. .Rbuildignore'd.
#
#   Rscript dev/build_readmes.R           # both
#   Rscript dev/build_readmes.R index     # pkgdown/index.md only -- what dev/build_site.R runs
#   Rscript dev/build_readmes.R readme    # README.md only
#
# ROLE: README.md is GitHub's home, rendered in the black-and-white publication palette because
#   GitHub's sanitizer strips `class` and `style` from raw html; pkgdown/index.md is the site's home
#   -- the same prose and the same examples, in real colour. Edit the .Rmd, never the .md.
# WARNING: knit the WORKING TREE, not the installed package -- `library(tabxplor)` in a setup chunk
#   would otherwise silently document whatever version happens to be in the library. load_all() with
#   export_all = FALSE attaches the same surface a user gets.
# WARNING: each source pins tabxplor options and `LANGUAGE` so the page cannot depend on who builds
#   it. Run this in a FRESH session (Rscript), never source it into a build session, or those pins
#   leak into everything rendered afterwards -- which is why dev/build_site.R shells out to it.

root <- normalizePath(".")
if (!file.exists(file.path(root, "DESCRIPTION"))) stop("run from the package root")

PAGES <- list(
  readme = list(src = "README.Rmd",        out = "README.md"),
  index  = list(src = "pkgdown/index.Rmd", out = "pkgdown/index.md")
)

which <- commandArgs(trailingOnly = TRUE)
if (!length(which)) which <- names(PAGES)
bad <- setdiff(which, names(PAGES))
if (length(bad)) stop("unknown page(s): ", paste(bad, collapse = ", "))

stopifnot(requireNamespace("rmarkdown", quietly = TRUE),
          requireNamespace("pkgload",   quietly = TRUE))
pkgload::load_all(root, quiet = TRUE, export_all = FALSE, helpers = FALSE)

for (p in PAGES[which]) {
  message("knitting ", p$out, " from ", p$src)
  rmarkdown::render(
    file.path(root, p$src),
    output_file   = basename(p$out),
    # DESIGN: the root, not the source's own folder -- so pkgdown/index.Rmd resolves `man/figures/`
    # and any project path exactly as README.Rmd does, one page being the other in colour.
    knit_root_dir = root,
    quiet         = TRUE
  )
}
message("done: ", paste(vapply(PAGES[which], `[[`, "", "out"), collapse = ", "))
