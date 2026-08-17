# PURPOSE: GENERATE the jamovi `.js` rule blocks from the R fact tables (Phase 19k).
# ROLE: The two `jamovi/js/*.js` files hold real UI code (DOM building, pickers, styling) that stays
#   hand-written. What must NOT be hand-written is the RULES they encode -- which family an outcome
#   gets, which families it offers, which (effect x measure) estimands exist for it -- because those
#   are R facts, and a hand-mirror in a language with no test harness here is exactly the class of
#   duplication Phase 19 exists to delete. This script rewrites, IN PLACE, the block between
#     // --- BEGIN GENERATED (dev/generate_jamovi_js.R) -- do not edit ---
#     // --- END GENERATED ---
#   in each file. Everything outside the markers is untouched.
#   Phase 20g-ii adds a SECOND pair, `BEGIN/END SHARED`, which is a verbatim COPY of a block of real
#   UI code from jmvtab.js into jmvtabreg.js (the level list with the merge tick-boxes) -- the two
#   analyses show the same widget, and ~120 lines of export/subtext/CSS helpers already sit in both
#   files kept in step by a comment, which is exactly what this avoids repeating.
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
# Phase 20g-ii: the second pair -- a block COPIED from jmvtab.js into jmvtabreg.js (shared_block()).
BEGIN_SHARED <- "// --- BEGIN SHARED (dev/generate_jamovi_js.R: copied from jamovi/js/jmvtab.js) -- do not edit ---"
END_SHARED   <- "// --- END SHARED ---"

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
    "// Generated from R/fmt_class.R (MEASURES) and R/tab-display.R (DISPLAY_TOKENS).",
    "// Re-run dev/generate_jamovi_js.R after changing them; the suite checks this block",
    "// (test-jamovi-vocabulary.R).",
    paste0("var TABX_MEASURE_ODDS_RATIO = ", js_str("odds_ratio"), ";"),
    paste0("var TABX_DISPLAY_ODDS_RATIO_FIELDS = ", js_arr(or_displays), ";"),
    END
  )
}


# =============================================================================================
# Phase 20g-ii: TWO marker pairs now, so `splice()` takes the pair it is replacing. The second one
# is a COPY, not a generation -- see shared_block() below.
splice <- function(lines, block, begin = BEGIN, end = END) {
  i <- which(lines == begin)
  j <- which(lines == end)
  if (length(i) != 1L || length(j) != 1L || j <= i)
    stop("missing or malformed markers (", begin, ")", call. = FALSE)
  c(lines[seq_len(i - 1L)], block, lines[seq(j + 1L, length(lines))])
}

# The SHARED block: the level list with the merge tick-boxes, written ONCE in jmvtab.js and copied
# verbatim into jmvtabreg.js. It is a copy rather than a generation because it is real UI code, not
# a rule table -- but it rides the same mechanism (markers + `check`) for the same reason the
# generated blocks do: jamovi bundles jamovi/js/*.js itself, and whether it would resolve a
# require() of a third module is not something this repo can test. Everything outside the markers in
# either file stays hand-written.
shared_block <- function() {
  src <- readLines(file.path(.root, "jamovi", "js", "jmvtab.js"), warn = FALSE)
  i <- which(src == BEGIN_SHARED)
  j <- which(src == END_SHARED)
  if (length(i) != 1L || length(j) != 1L || j <= i)
    stop("missing or malformed SHARED markers in jmvtab.js", call. = FALSE)
  src[seq(i, j)]                        # markers included: they become jmvtabreg.js's own
}

targets <- list(
  jmvtabreg.js = function(lines) splice(splice(lines, reg_block()),
                                        shared_block(), BEGIN_SHARED, END_SHARED),
  jmvtab.js    = function(lines) splice(lines, tab_block())
)

mode <- if (length(commandArgs(TRUE))) commandArgs(TRUE)[[1]] else "write"
stale <- character()
for (nm in names(targets)) {
  path <- file.path(.root, "jamovi", "js", nm)
  cur  <- readLines(path, warn = FALSE)
  new  <- targets[[nm]](cur)
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
