# PURPOSE: price tabxplor's dependency tree for the three audiences that actually pay -- CRAN's
#   count of non-base Imports, the jamovi .jmo's megabytes, and a plain user's install.
# ROLE: the measurement behind dev/dependencies.md. Re-run it after ANY DESCRIPTION change; the
#   numbers in that document are its output.
# USAGE: Rscript dev/dep_footprint.R
#   Sizes are read from the LOCAL library, so a package that is not installed counts as 0 -- the
#   script says which those are. jamovi's bundled list is read from the flatpak when present, and
#   falls back to a recorded snapshot otherwise.

base <- tools:::.get_standard_package_names()$base

# jamovi bundles ~124 packages that a module never has to carry. The flatpak is the authority.
jmv <- tryCatch(suppressWarnings(system2(
  "flatpak", c("run", "--devel", "--command=sh", "org.jamovi.jamovi",
               "-c", shQuote("ls /app/lib/R/library")), stdout = TRUE, stderr = FALSE)),
  error = function(e) character())
if (!length(jmv) || !"jmvcore" %in% jmv) {
  message("! flatpak jamovi not reachable -- using the recorded 2.7.36 snapshot")
  jmv <- c(base, "askpass","backports","base64enc","boot","brio","broom","bslib","cachem","callr",
    "class","cli","cluster","codetools","cpp11","crayon","curl","data.table","desc","devEMF",
    "diffobj","digest","dplyr","evaluate","export","fansi","farver","fastmap","flextable",
    "fontawesome","fontBitstreamVera","fontLiberation","fontquiver","foreign","fs","gdtools",
    "generics","ggplot2","glue","gtable","highr","htmltools","isoband","jmvcore","jmvReadWrite",
    "jquerylib","jsonlite","KernSmooth","knitr","labeling","lattice","lifecycle","magrittr","MASS",
    "Matrix","memoise","mgcv","mime","nlme","nnet","officer","openssl","openxlsx","pillar",
    "pkgbuild","pkgconfig","pkgload","praise","processx","ps","purrr","R6","ragg","rappdirs",
    "RColorBrewer","Rcpp","remotes","RInside","rlang","rmarkdown","rpart","rprojroot","RProtoBuf",
    "rvg","sass","scales","spatial","stargazer","stringi","stringr","survival","sys","systemfonts",
    "testthat","textshaping","tibble","tidyr","tidyselect","tinytex","translations","utf8","uuid",
    "vctrs","viridisLite","waldo","withr","xfun","xml2","xtable","yaml","zip")
}

deps <- function(p) {                       # RUNTIME deps: Depends + Imports, never LinkingTo
  d <- tryCatch(packageDescription(p), error = function(e) NULL)
  if (is.null(d) || identical(d, NA)) return(character())
  x <- paste(stats::na.omit(c(d$Depends, d$Imports)), collapse = ",")
  setdiff(trimws(gsub("\\s*\\([^)]*\\)", "", unlist(strsplit(x, ",")))), c("R", "", base))
}
closure <- function(seed) {
  seen <- character(); todo <- setdiff(seed, base)
  while (length(todo)) { p <- todo[1]; todo <- todo[-1]
    if (p %in% seen) next
    seen <- c(seen, p); todo <- c(todo, setdiff(deps(p), seen)) }
  sort(unique(seen))
}
sz <- function(p) {
  d <- find.package(p, quiet = TRUE)
  if (!length(d)) return(NA_real_)
  sum(file.info(list.files(d, recursive = TRUE, full.names = TRUE))$size, na.rm = TRUE) / 1048576
}
mb <- function(v) sum(vapply(v, sz, 0), na.rm = TRUE)

f   <- function(x) trimws(gsub("\\s*\\([^)]*\\)", "", unlist(strsplit(x, ","))))
d   <- read.dcf("DESCRIPTION")
imp <- setdiff(f(d[, "Imports"]),  base)
sug <- setdiff(f(d[, "Suggests"]), base)

cat(sprintf("Imports (non-base): %d / 20 CRAN limit\nSuggests: %d\n\n", length(imp), length(sug)))
cat(sprintf("hard install, recursive        : %3d packages\n", length(closure(imp))))
cat(sprintf("dependencies = TRUE, recursive : %3d packages\n\n", length(closure(c(imp, sug)))))

payload <- setdiff(closure(c(imp, sug)), jmv)
floor_  <- setdiff(closure(imp), jmv)
cat(sprintf("jamovi .jmo payload : %d packages, %.1f MB\n", length(payload), mb(payload)))
cat(sprintf("  of which forced by Imports alone: %d packages, %.1f MB\n",
            length(floor_), mb(floor_)))
missing <- payload[is.na(vapply(payload, sz, 0))]
if (length(missing)) cat("  ! not installed here, counted as 0 MB:", paste(missing, collapse=", "), "\n")

cat("\nWHAT DROPPING EACH SUGGEST WOULD SAVE FROM THE .jmo (its closure, not itself)\n")
groups <- list("DescTools (test-only)" = "DescTools", "bench (test-only)" = "bench",
               "VGAM + svyVGAM" = c("VGAM", "svyVGAM"), "marginaleffects" = "marginaleffects",
               "openxlsx2" = "openxlsx2", "kableExtra" = "kableExtra",
               "parallel seam" = c("mirai", "parallelly", "RhpcBLASctl", "pkgload"),
               "gridExtra" = "gridExtra", "brant" = "brant",
               "survey (an Import)" = "survey", "forcats (an Import)" = "forcats")
# ⚠ a saving of 0 does NOT mean the package is free: it means something ELSE in the tree still
# pulls it, so it cannot be dropped alone. Name that puller rather than printing a bare zero.
pullers <- local({
  par <- list(); todo <- c(imp, sug); seen <- character()
  while (length(todo)) { q <- todo[1]; todo <- todo[-1]
    if (q %in% seen) next
    seen <- c(seen, q)
    for (k in deps(q)) { par[[k]] <- unique(c(par[[k]], q)); todo <- c(todo, k) } }
  par
})
res <- vapply(groups, function(g)
  mb(setdiff(payload, setdiff(closure(setdiff(c(imp, sug), g)), jmv))), 0)
for (i in order(-res)) {
  g <- groups[[i]]
  why <- setdiff(unlist(lapply(g, function(x) pullers[[x]])), g)
  cat(sprintf("  %-24s %6.1f MB%s\n", names(res)[i], res[i],
              if (res[i] < 0.05 && length(why))
                sprintf("   (still pulled by %s)", paste(why, collapse = ", ")) else ""))
}
