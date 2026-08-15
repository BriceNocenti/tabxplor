# PURPOSE: GENERATE the jamovi `.js` rule blocks from the R fact tables (Phase 19k).
# ROLE: The two `jamovi/js/*.js` files hold real UI code (DOM building, pickers, styling) that stays
#   hand-written. What must NOT be hand-written is the RULES they encode -- which family an outcome
#   gets, which families it offers, which (effect x measure) estimands exist for it -- because those
#   are R facts, and a hand-mirror in a language with no test harness here is exactly the class of
#   duplication Phase 19 exists to delete. This script rewrites, IN PLACE, the block between
#     // --- BEGIN GENERATED (dev/generate_jamovi_js.R) -- do not edit ---
#     // --- END GENERATED ---
#   in each file. Everything outside the markers is untouched.
#
# USAGE (from the package root, unsandboxed):
#   Rscript dev/generate_jamovi_js.R          # rewrite the blocks
#   Rscript dev/generate_jamovi_js.R check    # verify they are up to date (exit 1 if not)
#
# The suite runs the `check` mode as an assertion (tests/testthat/test-jamovi-vocabulary.R), so a
# drifted block fails like any other test. A marker block (rather than a separate generated .js file)
# is deliberate: jamovi bundles `jamovi/js/*.js` itself, and whether it would resolve a `require()`
# of a second module is not something this repo can test.
#
# See: CLAUDE.md > Phase 19k ; dev/tabxplor_2.0.0_jamovi_dev.md.

# Locate the package root from THIS script's own path, so it runs identically from the root, from
# tests/testthat (the drift check) and from anywhere else.
.root <- local({
  a <- commandArgs(trailingOnly = FALSE)
  f <- sub("^--file=", "", a[grepl("^--file=", a)])
  if (length(f)) normalizePath(file.path(dirname(f[[1]]), "..")) else normalizePath(".")
})
suppressMessages(devtools::load_all(.root))

BEGIN <- "// --- BEGIN GENERATED (dev/generate_jamovi_js.R) -- do not edit ---"
END   <- "// --- END GENERATED ---"

# --- tiny JS literal writers (no jsonlite dependency for a dev script) -----------------------
js_str  <- function(x) paste0('"', gsub('"', '\\\\"', x), '"')
js_arr  <- function(x) paste0("[", paste(js_str(x), collapse = ", "), "]")
js_obj  <- function(x, val = js_str) {                    # named vector/list -> { k: v, ... }
  paste0("{ ", paste0(js_str(names(x)), ": ",
                      vapply(x, val, character(1)), collapse = ", "), " }")
}


# =============================================================================================
# jmvtabreg.js -- the family rules + the three-state estimand grid
# =============================================================================================
reg_block <- function() {
  kinds <- tabxplor:::REG_OUTCOME_KINDS

  # (1) the outcome-kind rule: detected family + offered families, per kind.
  detect <- vapply(kinds, function(k) k$detect, character(1))
  offers <- lapply(kinds, function(k) k$offers)

  # (2) the estimand grid: per family, per effect, the measures that BUILD. `reg_estimand()` is the
  #     authority (it also knows the "impossible" and the "not offered" states); the UI only needs
  #     to know what to enable, so the grid stores the available set.
  # Phase 19m-i: "which families the picker offers" is a DECLARED fact -- REG_FAMILIES$ui is NA on
  # the ones it does not (quasipoisson, and the internal link keys). It used to be a hardcoded
  # setdiff() here AND an omission from REG_FAMILY_UI_LABEL: one fact, two encodings.
  fams  <- names(tabxplor:::reg_family_ui_labels())
  effs  <- tabxplor:::REG_EFFECTS_VALUES
  meas  <- setdiff(tabxplor:::REG_MEASURES_VALUES, "auto")
  grid  <- lapply(fams, function(f) {
    lapply(stats::setNames(effs, effs), function(e) {
      ok <- vapply(meas, function(m) {
        r <- tabxplor:::reg_estimand(f, e, m)
        identical(r$status, "ok")
      }, logical(1))
      c("auto", meas[ok])                       # "auto" always resolves (it IS the family default)
    })
  })
  names(grid) <- fams

  # (3) what "auto" resolves to, so the picker can SHOW the default it will get.
  defaults <- lapply(stats::setNames(fams, fams), function(f)
    vapply(stats::setNames(effs, effs), function(e) tabxplor:::reg_default_measure(f, e),
           character(1)))

  c(
    BEGIN,
    "// Generated from R/tab_reg.R (REG_OUTCOME_KINDS), R/reg-estimand.R (REG_FAMILIES,",
    "// REG_ESTIMANDS) and R/reg-assumptions.R (REG_SHAPES). Re-run dev/generate_jamovi_js.R after",
    "// changing any of them; the suite checks this block (test-jamovi-vocabulary.R).",
    paste0("var TABX_FAMILY_LABEL = ", js_obj(tabxplor:::reg_family_ui_labels()), ";"),
    paste0("var TABX_FAMILY_LABEL_BINARY = ", js_obj(tabxplor:::reg_family_ui_labels(binary = TRUE)), ";"),
    paste0("var TABX_OUTCOME_DETECT = ", js_obj(detect), ";"),
    paste0("var TABX_OUTCOME_OFFERS = ", js_obj(offers, js_arr), ";"),
    paste0("var TABX_ESTIMANDS = ",
           paste0("{ ", paste0(js_str(fams), ": ",
                               vapply(grid, function(g) js_obj(g, js_arr), character(1)),
                               collapse = ", "), " }"), ";"),
    paste0("var TABX_DEFAULT_MEASURE = ",
           paste0("{ ", paste0(js_str(fams), ": ",
                               vapply(defaults, js_obj, character(1)), collapse = ", "), " }"), ";"),
    # the per-predictor functional forms (REG_SHAPES, R/reg-assumptions.R)
    paste0("var TABX_SHAPES = ", js_arr(tabxplor:::REG_SHAPES), ";"),
    END
  )
}


# =============================================================================================
# jmvtab.js -- the crosstab vocabularies the UI branches on
# =============================================================================================
tab_block <- function() {
  # The odds ratio is "in force" when the COLOUR names it or the DISPLAY prints it -- which is what
  # switches the reference picker to a first-level default and shows its ref2 section. Both halves
  # are R facts: the measure key, and DISPLAY_COMPARISON's mapping from a display token.
  or_displays <- names(Filter(function(x) identical(x, "odds_ratio"),
                              as.list(tabxplor:::DISPLAY_COMPARISON)))
  c(
    BEGIN,
    "// Generated from R/fmt_class.R (MEASURES) and R/tab-resolve.R (DISPLAY_COMPARISON).",
    "// Re-run dev/generate_jamovi_js.R after changing them; the suite checks this block",
    "// (test-jamovi-vocabulary.R).",
    paste0("var TABX_MEASURE_ODDS_RATIO = ", js_str("odds_ratio"), ";"),
    paste0("var TABX_DISPLAY_ODDS_RATIO_FIELDS = ", js_arr(or_displays), ";"),
    END
  )
}


# =============================================================================================
splice <- function(path, block) {
  lines <- readLines(path, warn = FALSE)
  i <- which(lines == BEGIN)
  j <- which(lines == END)
  if (length(i) != 1L || length(j) != 1L || j <= i)
    stop("missing or malformed generated-block markers in ", path, call. = FALSE)
  c(lines[seq_len(i - 1L)], block, lines[seq(j + 1L, length(lines))])
}

targets <- list(
  reg_block(),
  tab_block()
)
names(targets) <- file.path(.root, "jamovi", "js", c("jmvtabreg.js", "jmvtab.js"))

mode <- if (length(commandArgs(TRUE))) commandArgs(TRUE)[[1]] else "write"
stale <- character()
for (path in names(targets)) {
  new  <- splice(path, targets[[path]])
  cur  <- readLines(path, warn = FALSE)
  if (identical(new, cur)) next
  if (identical(mode, "check")) stale <- c(stale, path)
  else { writeLines(new, path); message("rewrote generated block: ", path) }
}
if (identical(mode, "check")) {
  if (length(stale)) {
    message("STALE generated block(s): ", paste(stale, collapse = ", "))
    quit(status = 1L)
  }
  message("generated blocks are up to date")
}
