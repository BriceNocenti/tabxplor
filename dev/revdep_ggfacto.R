# PURPOSE: check tabxplor's only reverse dependency, `ggfacto`, against the working tree.
#   .Rbuildignore'd.
#
#   Rscript dev/revdep_ggfacto.R           # both versions
#   Rscript dev/revdep_ggfacto.R cran      # CRAN ggfacto only
#   Rscript dev/revdep_ggfacto.R dev       # the local ~/github/ggfacto checkout only
#
# ROLE: the release gate `dev/release_checklist.md` step 1 calls. Two versions matter and they ask
#   different questions: **CRAN ggfacto** is what a user has installed today, so it exercises the
#   superseded surface (it still calls `set_type()`, which is exactly what broke at 2.0.0 and is why
#   that function was re-exported); the **local checkout** is what ggfacto will release next, so it
#   exercises the current one. A release may not break either.
# WARNING: `revdepcheck` is deliberately not used. For one reverse dependency it adds a `revdep/`
#   directory to git- and build-ignore and an opaque cache, where two `R CMD check` runs against a
#   throwaway library say the same thing and can be read.
# WARNING: the library below is THROWAWAY and is rebuilt from scratch; never point it at the user
#   library, or the checked ggfacto would resolve whatever tabxplor happens to be installed.
# See: CLAUDE.md > tabxplor github repo, and dev/release_checklist.md step 1.

root <- normalizePath(".")
if (!file.exists(file.path(root, "DESCRIPTION"))) stop("run from the package root")

LIB      <- path.expand("~/R/revdep-ggfacto")   # throwaway
# WARNING: an explicit CRAN mirror, not the session's repos: this box resolves to a BINARY repo, and
# `download.packages(type = "source")` against it hands back a binary that R CMD check refuses with
# "Only *source* packages can be checked".
REPOS    <- "https://cloud.r-project.org"
GGFACTO  <- path.expand("~/github/ggfacto")
OUT      <- file.path(root, "dev", "revdep-ggfacto-log")

which <- commandArgs(trailingOnly = TRUE)
if (!length(which)) which <- c("cran", "dev")
stopifnot(all(which %in% c("cran", "dev")))

unlink(LIB, recursive = TRUE); dir.create(LIB, recursive = TRUE)
dir.create(OUT, showWarnings = FALSE, recursive = TRUE)
tmp <- tempfile("revdep"); dir.create(tmp)

message("-- building tabxplor from the working tree")
tar_tabxplor <- pkgbuild::build(root, dest_path = tmp, vignettes = FALSE, quiet = TRUE)

message("-- installing tabxplor and ggfacto's dependencies into ", LIB)
# ggfacto's own Imports/Suggests, plus tabxplor's, resolved once into the throwaway library
utils::install.packages(tar_tabxplor, lib = LIB, repos = REPOS, type = "source",
                        dependencies = c("Depends", "Imports"), INSTALL_opts = "--no-manual")
.libPaths(c(LIB, .libPaths()))

sources <- list()
if ("cran" %in% which) {
  message("-- fetching CRAN ggfacto")
  # download.packages takes whatever CRAN currently serves; the version is recorded in the log
  got <- utils::download.packages("ggfacto", destdir = tmp, type = "source", repos = REPOS)
  sources$cran <- got[1, 2]
}
if ("dev" %in% which) {
  if (!dir.exists(GGFACTO)) stop("no ggfacto checkout at ", GGFACTO)
  message("-- building ggfacto from ", GGFACTO)
  sources$dev <- pkgbuild::build(GGFACTO, dest_path = tmp, vignettes = FALSE, quiet = TRUE)
}

results <- list()
for (nm in names(sources)) {
  message("-- R CMD check ggfacto (", nm, ")")
  utils::install.packages(sources[[nm]], lib = LIB, repos = REPOS,
                          dependencies = c("Depends", "Imports", "Suggests"),
                          INSTALL_opts = "--no-manual")
  # NOT --as-cran: we are checking that ggfacto still WORKS, not submitting it, and its incoming
  # checks only add noise about ggfacto's own version and timestamp.
  res <- rcmdcheck::rcmdcheck(sources[[nm]], libpath = c(LIB, .libPaths()),
                              args = "--no-manual",
                              error_on = "never", check_dir = file.path(tmp, nm))
  results[[nm]] <- res
  writeLines(c(sprintf("ggfacto (%s): %s", nm, basename(sources[[nm]])),
               sprintf("tabxplor: %s", basename(tar_tabxplor)),
               "", format(res)),
             file.path(OUT, paste0("ggfacto-", nm, ".log")))
}

cat("\n== reverse dependency check ==\n")
bad <- FALSE
for (nm in names(results)) {
  r <- results[[nm]]
  n <- c(length(r$errors), length(r$warnings), length(r$notes))
  cat(sprintf("  ggfacto (%-4s): %d error(s), %d warning(s), %d note(s)   %s\n",
              nm, n[1], n[2], n[3], file.path(OUT, paste0("ggfacto-", nm, ".log"))))
  if (any(n > 0)) bad <- TRUE
}
if (bad) {
  cat("\nRead the logs above before releasing.\n")
  quit(status = 1L)
}
cat("\nBoth clean.\n")
