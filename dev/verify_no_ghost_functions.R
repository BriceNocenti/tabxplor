# verify_no_ghost_functions.R -- find comments that name a function which no longer exists.
#
# WHY THIS FILE EXISTS. Phase 19l pass 1 found `reg_fam_logscale()` carrying a WARNING that named
# "fmt_class.R's colour engine AND its legend" as its consumers -- neither had read it since 19b. A
# comment that describes CURRENT behaviour through a function nobody defines any more is not clutter;
# it is a false statement about the code, and the next reader believes it. Pass 2 swept for the class
# and found ~80 sites (tab_apply_tests x6, is_reg_footer x4, or_plot x4, reg_empirical_tips x3,
# tab_assemble x3, measure_stage, reg_model_note, reg_spread_models, est_scale_key, ci_scale_of, ...).
#
# WHAT IT REPORTS. Every `foo()` written inside a comment in R/ whose name is defined NOWHERE in R/
# (at any indentation, including list entries and factory assignments) and belongs to no package the
# repo depends on. Auto-generated files (*.h.R) are skipped.
#
# HOW TO READ IT -- there are TWO classes, and only the second is a bug:
#   (a) a deliberate historical note: "X is DELETED because ...", "the X() that stood here", "was X()".
#       KEEP those. They are the phase record, and deleting them re-opens settled questions.
#   (b) a LIVE claim that runs through a dead function: "read by X()", "X() then does ...", "see X()".
#       FIX those -- they are the reg_fam_logscale class.
# The script cannot tell them apart (that is a reading judgement), so it prints the comment line and
# lets you decide. It is a REPORT, not a gate.
#
# KNOWN FALSE POSITIVES, both unavoidable: a LOCAL closure defined inside another function (`emp_col`,
# `two`, `one_side`, `mk_side`, ...) is not in the namespace, and a METHOD of somebody else's object
# (jmvtools' `prepare()`, an R6 `$run()`) is not a function of ours -- `foreign_methods` below lists
# the ones met so far. Neither is worth machinery: read the line, it is obvious which it is.
#
#   Rscript dev/verify_no_ghost_functions.R            # every ghost, grouped by name
#   Rscript dev/verify_no_ghost_functions.R <name>     # one name, with its lines

files <- list.files("R", pattern = "[.]R$", full.names = TRUE)
files <- files[!grepl("[.]h[.]R$", files)]            # jamovi .h.R are generated -- never hand-read

# Every name the package DEFINES -- taken from the loaded namespace, which is the only source that
# is right by construction. A regex on `<- function` misses the multi-line form (`tab_xl <-` /
# newline / `  function(...)`), the factory idiom (`get_n <- fmt_field_factory("n")`) and plain
# aliases (`tab_kable <- tab_html`) -- ~19 false ghosts on the first attempt.
suppressMessages(devtools::load_all(".", quiet = TRUE))
defined <- ls(asNamespace("tabxplor"), all.names = TRUE)

# packages whose functions a comment may legitimately name
pkgs <- c("base", "stats", "utils", "methods", "graphics", "grDevices", "tools", "grid",
          "dplyr", "tibble", "vctrs", "purrr", "rlang", "tidyr", "tidyselect", "stringi",
          "pillar", "cli", "data.table", "survey", "broom", "forcats", "knitr", "lifecycle",
          "htmltools", "ggplot2", "openxlsx2", "kableExtra", "marginaleffects", "nnet", "MASS",
          "withr", "fs", "jmvcore", "mirai", "svyVGAM", "car", "DescTools", "potools", "scales",
          "testthat", "devtools", "bench", "clipr", "ggpubr", "gtable", "cowplot", "brant")
# Method names that belong to another package's OBJECT, not to a function of ours: jmvtools' R6
# generator API and the analysis backends' own methods.
foreign_methods <- c("prepare", "install", "check", "run", "init", "setContent", "setState",
                     "options", "results", "setVisible", "setNote")
in_pkg <- function(nm) nm %in% foreign_methods || any(vapply(pkgs, function(p) {
  ns <- tryCatch(asNamespace(p), error = function(e) NULL)
  !is.null(ns) && exists(nm, envir = ns, inherits = FALSE)
}, logical(1)))

hits <- list()
for (f in files) {
  L <- readLines(f, warn = FALSE)
  for (i in seq_along(L)) {
    if (!grepl("#", L[i], fixed = TRUE)) next
    cm <- sub("^[^#]*#", "#", L[i])                      # the comment part only
    # a leading dot is part of the name (`.opts()`, `.onLoad()`) -- \\b would drop it and report a
    # ghost `opts` / `onLoad` that the package does define.
    nms <- unique(regmatches(cm, gregexpr("[.]?[a-zA-Z][a-zA-Z0-9_.]*(?=\\(\\))", cm,
                                          perl = TRUE))[[1]])
    for (nm in nms) if (!nm %in% defined)
      hits[[length(hits) + 1L]] <- data.frame(file = f, line = i, name = nm, text = trimws(cm))
  }
}
g <- if (length(hits)) do.call(rbind, hits) else
  data.frame(file = character(), line = integer(), name = character(), text = character())
g <- g[!vapply(g$name, in_pkg, logical(1)), , drop = FALSE]

arg <- commandArgs(trailingOnly = TRUE)
if (length(arg)) g <- g[g$name == arg[[1]], , drop = FALSE]

if (!nrow(g)) {
  cat("No ghost function names in R/ comments.\n")
} else {
  tb <- sort(table(g$name), decreasing = TRUE)
  cat(nrow(g), "comment sites naming", length(tb), "function(s) defined nowhere in R/.\n")
  cat("Read each one: a historical note KEEPS, a live claim FIXES (see the header).\n\n")
  for (nm in names(tb)) {
    rows <- g[g$name == nm, , drop = FALSE]
    cat(sprintf("%s()  x%d\n", nm, nrow(rows)))
    for (k in seq_len(nrow(rows)))
      cat(sprintf("    %s:%d  %s\n", sub("^R/", "", rows$file[k]), rows$line[k],
                  substr(rows$text[k], 1L, 96L)))
    cat("\n")
  }
}
