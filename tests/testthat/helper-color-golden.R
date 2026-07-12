# PURPOSE: Shared matrix for the COLOR characterization golden (Phase 5 safety net).
# ROLE: Used by BOTH dev/make_color_golden.R (writes tests/testthat/_color_golden/*.rds) and
#        test-color-golden.R (checks live color output against them), so generator and test
#        can never drift. This is the byte-identity net for the color/breaks refactor: it
#        captures the RENDERED per-cell colors (fmt_get_color_code hex), which every current
#        exporter and the console derive from the same selection path.
# KEY CONSTRAINTS:
#   - Every case DETERMINISTIC (fixed data / fixed synthetic fields; no unseeded random).
#   - fmt_get_color_code() is the per-cell color signal (returns one hex/crayon code per cell,
#     NA where uncolored); pillar_shaft() and all exporters route through the SAME
#     fmt_color_selection -> select_in_color_style -> palette path, so this one capture locks
#     them all. It reads the current getOption("tabxplor.color_breaks"); the new breaks list
#     (Step 1) is defined so the DEFAULTS are equivalent, hence locked modes reproduce.
#   - The synthetic factor-"diff" case exercises cells sitting EXACTLY on breaks (0.05/0.1/0.2/
#     0.3 and the x2 ratio) -- the tie behaviour that risk R1 (fold + findInterval, left.open)
#     must reproduce byte-for-byte. Factor "diff" is the LOCKED tripwire Phase 5 must not move.
# See: CLAUDE.md > 1.4.0 roadmap > Phase 5 ; dev/new_colors_UI.md §13 (Step 0).

# The render combinations that cover all 6 palettes (text light/dark 8-bit, text-light 24-bit
# blue_red/green_red, background light/dark). type/theme/html_24_bit are passed explicitly to
# fmt_get_color_code so the capture does not depend on the color_style options being set.
color_golden_render_matrix <- function() {
  tibble::tribble(
    ~type,  ~theme,  ~html_24_bit,
    "text", "light", "no",
    "text", "dark",  "no",
    "text", "light", "blue_red",
    "text", "light", "green_red",
    "bg",   "light", "no",
    "bg",   "dark",  "no",
  )
}

# Capture fmt_get_color_code() for one fmt column across the whole render matrix. Wrapped in
# tryCatch so a mode that ERRORS on the current source (the pct CI-gated modes have a known
# break-indexing bug -- odd-length ci breaks suppress the negative direction, so keep_last_break
# gets an empty slice) is captured as an "<error: ...>" sentinel instead of crashing generation.
# Those modes are slated for conscious regeneration at Step 3; the sentinel documents the
# before-state ("errors today"), which the fix legitimately replaces with real colors.
color_golden_capture_col <- function(col) {
  rm <- color_golden_render_matrix()
  purrr::pmap(rm, function(type, theme, html_24_bit) {
    tryCatch(
      fmt_get_color_code(col, type = type, theme = theme, html_24_bit = html_24_bit),
      error = function(e) paste0("<error: ", conditionMessage(e), ">")
    )
  }) |>
    rlang::set_names(paste(rm$type, rm$theme, rm$html_24_bit, sep = "/"))
}

# Capture every colored fmt column of a whole table (named by column).
color_golden_capture_tab <- function(tab) {
  fmt_cols <- names(tab)[purrr::map_lgl(tab, is_fmt)]
  colored  <- fmt_cols[purrr::map_lgl(tab[fmt_cols], function(col) {
    cl <- get_color(col)
    length(cl) >= 1L && !is.na(cl[1]) && !cl[1] %in% c("", "no")
  })]
  purrr::map(rlang::set_names(colored), function(cn) color_golden_capture_col(tab[[cn]]))
}

# THE tie lock: a synthetic factor (type = "row") "diff" column whose `diff` values sit on and
# around every default break (0.05, 0.1, 0.2, 0.3, both signs), plus two cells whose ratio
# (the x2 rule) exceeds 2 -- one where the x2 should win, one where a top-break diff (>0.3)
# should beat it. The x2 ratio is written to BOTH the legacy `mean` overload (read by today's
# engine) and the `ratio` field (read by the Phase-5 engine after the field repoint), so the
# same input column drives an identical capture before and after Step 4.
color_golden_syn_diff_fmt <- function() {
  diff <- c( 0, 0.03, 0.05, 0.07, 0.10, 0.15, 0.20, 0.25, 0.30, 0.40,
            -0.03, -0.05, -0.07, -0.10, -0.20, -0.30, -0.40,
             0.12, 0.35)
  ratio <- rep(1, length(diff))
  ratio[c(length(diff) - 1L, length(diff))] <- 2.5  # cells 18 (x2 wins) & 19 (top diff wins)
  pct <- pmin(pmax(0.40 + diff, 0.01), 0.99)
  n   <- rep(50L, length(diff))
  # append a reference total row so get_ref_pct() (called in diff mode when a x2 break exists)
  # resolves to a real base without erroring.
  fmt(
    n         = c(n, 200L),
    type      = "row",
    pct       = c(pct, 0.40),
    diff      = c(diff, 0),
    mean      = c(ratio, 1),   # legacy x2 overload (current engine reads get_mean)
    ratio     = c(ratio, 1),   # Phase-5 engine reads get_ratio for the x2
    ci_type   = "",
    ref       = "tot",
    comp_all  = FALSE,
    color     = "diff",
    in_totrow = c(rep(FALSE, length(diff)), TRUE)
  )
}

# Named list of zero-arg thunks, each returning a CAPTURE (nested list of hex vectors). Names
# are the fixture basenames written to _color_golden/<name>.rds.
color_golden_cases <- function() {
  gss <- forcats::gss_cat

  list(
    # --- synthetic tie lock (LOCKED: factor "diff" must stay byte-identical) ---
    c_syn_diff       = function() color_golden_capture_col(color_golden_syn_diff_fmt()),

    # --- factor / % modes on real data ---
    c_diff           = function() color_golden_capture_tab(
      tab(gss, marital, race, pct = "row", color = "diff")),
    # c_diff_ci / c_after_ci / c_ci lock the soft-deprecated combined color strings; wrap the
    # build in suppressWarnings() so the deprecation nudge stays out of the captured output.
    c_diff_ci        = function() color_golden_capture_tab(suppressWarnings(
      tab(gss, marital, race, pct = "row", ci = "diff", color = "diff_ci"))),
    c_after_ci       = function() color_golden_capture_tab(suppressWarnings(
      tab(gss, marital, race, pct = "row", ci = "diff", color = "after_ci"))),
    c_ci             = function() color_golden_capture_tab(suppressWarnings(
      tab(gss, marital, race, pct = "row", ci = "diff", color = "ci"))),
    c_contrib        = function() color_golden_capture_tab(
      tab(gss, marital, race, pct = "row", color = "contrib")),
    # contrib + comp = "all": the whole-table mean-contribution colour. WITH tab_vars the seed lives
    # on the total table's total row; WITHOUT tab_vars grand_totrow() degrades to the plain total row
    # (both previously crashed the colour engine -- get_mean_contrib size 0 / mis-stored seed).
    c_contrib_all    = function() color_golden_capture_tab(suppressWarnings(
      tab(gss, marital, race, tab_vars = year, pct = "row", color = "contrib", comp = "all"))),
    c_contrib_all_notab = function() color_golden_capture_tab(suppressWarnings(
      tab(gss, marital, race, pct = "row", color = "contrib", comp = "all"))),
    c_or             = function() color_golden_capture_tab(
      tab(gss, marital, race, pct = "col", OR = "OR", color = "OR")),

    # --- numeric / mean modes (CONSCIOUSLY REGENERATED at Step 3: diff -> Glass's delta) ---
    c_mean_diff      = function() color_golden_capture_tab(
      tab_num(gss, race, c(age, tvhours), comp = "all", color = "diff", digits = 1L)),
    c_mean_diff_ci   = function() color_golden_capture_tab(suppressWarnings(
      tab_num(gss, race, c(age, tvhours), comp = "all", ci = "cell",
              color = "diff_ci", digits = 1L))),
    c_mean_after_ci  = function() color_golden_capture_tab(suppressWarnings(
      tab_num(gss, race, c(age, tvhours), comp = "all", ci = "cell",
              color = "after_ci", digits = 1L)))
  )
}

# Directory holding the color-golden .rds fixtures (relative to tests/testthat/).
color_golden_dir <- function() testthat::test_path("_color_golden")
