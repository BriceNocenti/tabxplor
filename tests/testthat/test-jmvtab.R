# PURPOSE: the crosstab live-UI cache and the jamovi export helpers.
# ROLE: the shipped CONTRACT for R/jmvtab-cache.R, R/jmvtab-export.R -- a failure here is a user-visible change.
#   The exhaustive sweep around it lives in dev/tests/testthat/.
# See: CLAUDE.md section "Testing" (what belongs in which suite).

# === SECTION: the crosstab live-UI cache ==========================================================

withr::local_options(lifecycle_verbosity = "quiet", .local_envir = testthat::teardown_env())




jmv_opts <- function(...) {
  # Phase 19k: the option names + values are tab()'s own -- `chi2` is `test`, the retired `OR` is a
  # `display` + `ref2` route, `ci` speaks the anchor vocabulary, `color` the full measure words.
  o <- list(row_vars = character(), col_vars = character(), tab_vars = character(), wt = character(),
            pct = "no", color = "no", color_signif = "ignore", test = FALSE,
            na = "keep", levels = "all", ref = "auto", ref2 = "first", comp = "tab", ci = "auto",
            conf_level = 0.95, stars = TRUE,   # design_effect: absent -> the global option decides
            # the jamovi UI keeps one ComboBox per interval kind; jmv_ci_method() folds them
            ci_method_cell = "wilson", ci_method_diff = "newcombe",
            ci_method_mean_diff = "welch", ci_method_mean_ratio = "robust",
            totaltab = "line", digits = 0, add_pct = FALSE,
            # 20g-ii: `n_min` was MISSING here although .opts() sets it and it is in the tier-3
            # `reapplied` set -- the fixture must carry every key that vector names, or the D12
            # assertion below cannot see the real invariant.
            n_min = 0,
            subtext = "", output_list = FALSE, cleannames = FALSE, display = "auto",
            anova = "welch",
            # 22g-iii: the per-numeric-variable cut. Like `levels_collapse` it is a PRE-aggregate
            # recode and therefore sits in the tier-1 keys, so the oracle must pass it too.
            shape = NULL,
            # 20g-i: ONE key of the option's own shape (it was three constants mirroring three
            # arguments that no longer exist). `.opts()` fills it with the module's translations.
            total_names = c(row = "Total", col = "Total", tab = "Ensemble", other = "Others"))
  # WARNING: `modifyList()` keeps the FIRST of two same-named entries, and every `o0(...)`/`mk(...)`
  # wrapper below splices its own defaults beside the caller's `...` -- so a later override was
  # silently swallowed. Measured on the pre-fix tree: `o0(color = "ratio")` at L318 built with
  # `color = "difference"`, and the trailing `ref = "1"` at L381 was dead, making case `b` identical to
  # case `a`. Keep the LAST, which is R's ordinary override semantic.
  args <- list(...)
  if (length(args)) args <- args[!duplicated(names(args), fromLast = TRUE)]
  utils::modifyList(o, args)
}



# The no-cache oracle: tab() with jmvtab_build()'s exact arg mapping (dummy vars, color, ci forcing).
jmv_oracle <- function(opts, data) {
  if (length(opts$row_vars) == 0L) { data$no_row_var <- factor("no_row_var"); opts$row_vars <- "no_row_var" }
  if (length(opts$col_vars) == 0L) { data$no_col_var <- factor("n");          opts$col_vars <- "no_col_var" }
  color <- switch(opts$color, "no" = FALSE, "auto" = TRUE, opts$color)
  # The RESOLVED anchor, from the same shared resolver jmvtab_build() calls -- not a hand-mirrored
  # `if (a policy) ci <- "diff"`, which was one more copy of a rule that has moved twice since it was
  # written.
  ci <- resolve_leaf_ci(opts$ci, jmv_tab3_measure(color), opts$color_signif, opts$stars,
                        if (length(opts$ref)) opts$ref else "auto")$ci
  wt_sym <- if (length(opts$wt)) rlang::sym(opts$wt) else NULL
  # Phase 19k: no translation left -- every option is passed under the name tab() gives it.
  # 20b/20g-i: the four synthetic labels are an option, installed exactly as jmv_tab3_build_armed()
  # installs them.
  if (length(opts$total_names)) {
    .old <- options(tabxplor.total_names = tabxplor:::tab_total_names_merge(opts$total_names))
    on.exit(options(.old), add = TRUE)
  }
  rlang::inject(tab(
    data, row_vars = tidyselect::all_of(opts$row_vars), col_vars = tidyselect::all_of(opts$col_vars),
    tab_vars = tidyselect::all_of(opts$tab_vars), wt = !!wt_sym, pct = opts$pct, color = color,
    color_signif = opts$color_signif, display = opts$display, test = opts$test, na = opts$na,
    levels = opts$levels, ref = opts$ref, ref2 = opts$ref2, comp = opts$comp, ci = ci,
    conf_level = opts$conf_level, stars = opts$stars, anova = opts$anova,
    ci_method = c(cell = opts$ci_method_cell, diff = opts$ci_method_diff),
    cleannames = FALSE, totaltab = opts$totaltab, digits = opts$digits,
    n = opts[["n"]], add_pct = opts$add_pct,
    subtext = opts$subtext, output_list = isTRUE(opts$output_list), shape = opts$shape
  ))
}



gss <- fx_gss()


gssw <- dplyr::mutate(gss, w = as.numeric(1 + (as.integer(marital) %% 3)))


# Phase 19m-i: a BINARY col_var. It is the shape on which "which column is the total" changes the
# answer rather than merely the bookkeeping -- two levels + a total is `binary` (each level's odds
# ratio against the OTHER level), three columns counted as levels is not.
gssb <- dplyr::mutate(gss, white = forcats::fct_other(race, keep = "White",
                                                      other_level = "Non-white"))




# --- store primitives ---------------------------------------------------------------------
test_that("store lifecycle: new / migrate / schema mismatch / round-trip", {
  s <- jmv_cache_new()
  expect_identical(s$schema, JMVTAB_CACHE_SCHEMA)
  expect_length(s$agg, 0L)
  expect_identical(jmv_cache_migrate(NULL)$schema, JMVTAB_CACHE_SCHEMA)  # NULL -> fresh
  bad <- s; bad$schema <- 999L
  expect_length(jmv_cache_migrate(bad)$agg, 0L)                          # mismatch -> discarded

  s <- jmv_cache_put(s, "agg", "k1", list(cols = list(n = 1:3), keys = "g"))
  got <- jmv_cache_fetch(s, "agg", "k1")
  expect_true(got$hit)
  expect_identical(got$value$cols$n, 1:3)
  expect_false(jmv_cache_fetch(s, "agg", "nope")$hit)
  # gzip-RDS round-trip preserves the store
  back <- unserialize(serialize(got$store, connection = NULL))
  expect_true(jmv_cache_fetch(back, "agg", "k1")$hit)
})




# --- byte-identity to tab() ---------------------------------------------------------------
test_that("cold build == tab(cleannames = FALSE); warm == cold", {
  cases <- list(
    jmv_opts(row_vars = "marital", col_vars = "race", pct = "row", test = TRUE),
    jmv_opts(row_vars = "marital", col_vars = "tvhours", test = TRUE),
    jmv_opts(row_vars = "marital", col_vars = c("race", "tvhours"), pct = "row", test = TRUE),
    jmv_opts(row_vars = "relig", col_vars = "race", tab_vars = "marital", pct = "row"),
    jmv_opts(row_vars = "marital", col_vars = "race", pct = "row", na = "drop"),
    jmv_opts()  # bare table
  )
  for (o in cases) {
    cold <- jmvtab_build(gss, o, NULL)
    warm <- jmvtab_build(gss, o, cold$store)
    expect_equal(cold$tabs, jmv_oracle(o, gss))
    expect_equal(warm$tabs, cold$tabs)
  }
})




# --- Phase 7f: tier-3 built-table cache (display / colour re-paint) ------------------------
# A change that touches only display / colour reuses the cached ARMED table (pre-finalize fmt cells)
# and re-paints -- hits$tab3 == TRUE, no O(cells) rebuild -- while staying byte-identical to a fresh
# tab(). A base change (pct / na / levels) or a reference change rebuilds (hits$tab3 == FALSE).
test_that("Phase 7f: display / colour toggles re-use the armed table (warm == cold, tier-3 hit)", {
  # warm (re-paint from a base-warmed store) must equal a cold full-pipeline build; a tier-3 hit
  # means no O(cells) rebuild. (display / cleannames are modelled by the full jmvtab pipeline, not by
  # jmv_oracle = tab(), so they are locked warm==cold; the fresh-tab() lock is the next test.)
  o0   <- function(...) jmv_opts(row_vars = "marital", col_vars = "race", pct = "row",
                                 color = "difference", test = TRUE, ...)
  st   <- jmvtab_build(gss, o0(), NULL)$store
  # Phase 19d-tail: `ratio` is not an exact RE-PAINT -- since 19d the stored interval follows the
  # comparison (percentage-POINT bounds vs Katz log-RR), so painting a ratio over a difference
  # carrier would show the wrong bracket and the wrong stars. Phase 19k: it is a tier-3 HIT again,
  # by the other route -- the RE-REF recomputes the bounds on the other scale (leaf_ci_plain takes
  # `ci_scale`) and restamps the column's `scale` / `ci_method` with them. The gate is the same one
  # every other case has: warm == cold.
  reference <- c(same = TRUE, digits = TRUE, display = TRUE, ratio = TRUE, auto = TRUE,
                 grey = TRUE, color_all = TRUE, cleannames = TRUE)
  toggles <- list(
    "same"       = o0(),
    "digits"     = o0(digits = 2),
    "display"    = o0(display = "n"),
    "ratio"      = o0(color = "ratio"),
    "auto"       = o0(color = "auto"),
    "grey"       = o0(color_signif = "grey_non_signif"),
    "color_all"  = o0(color_signif = "guaranteed_effect"),
    "cleannames" = o0(cleannames = TRUE)
  )
  for (nm in names(toggles)) {
    r    <- suppressMessages(jmvtab_build(gss, toggles[[nm]], st))
    cold <- suppressMessages(jmvtab_build(gss, toggles[[nm]], NULL)$tabs)
    expect_equal(isTRUE(r$hits$tab3), unname(reference[[nm]]), info = paste("tier-3 reuse:", nm))
    expect_equal(r$tabs, cold, info = paste("warm == cold:", nm))
  }
})




# --- Phase 7g-ii: level reordering (post-aggregate, tier-3 input) -------------------------
# The oracle is a plain tab() on PRE-RELEVELED microdata: the jmvtab reorder relevels the shaped
# aggregate in memory (stored blob stays raw), which must be byte-identical to having fct_relevel'd
# the data before tab(). A reorder reuses tiers 1-2 (aggregate + test) and only rebuilds the fmt.
mar_ord  <- c("Married", "Divorced", "Widowed", "Separated", "Never married", "No answer")


race_ord <- c("White", "Black", "Other", "Not applicable")   # raw first = "Other"


re_relevel <- function(d, spec) {
  for (v in names(spec)) d[[v]] <- forcats::fct_relevel(d[[v]], spec[[v]])
  d
}




# --- Phase 20g-ii: level MERGING (pre-aggregate; the reorder's opposite in every cache respect) ---
# Same free oracle as the reorder -- a plain tab() on microdata the user collapsed themselves -- but
# the tier contract is INVERTED: a merge changes the counts, so it must MISS tier 1.
mar_merge <- list(marital = list("Not married" = c("Never married", "Divorced", "Separated")))


re_collapse <- function(d, spec) {
  for (v in names(spec)) d[[v]] <- forcats::fct_collapse(d[[v]], !!!spec[[v]])
  d
}



test_that("every `reapplied` name is a key of the opts list (the D12 invariant)", {
  # jmv_tab3_base_key()'s `structural` is the NEGATIVE set, so a name in JMV_TAB3_REAPPLIED that is
  # not an opts key is silently ineffective and its option quietly rebuilds the whole table. That IS
  # D12 (the four `method_*` keys); the ⚠ beside the vector has said so since 19k with nothing
  # checking it. 20g-ii made it a constant so this one line can.
  expect_identical(setdiff(JMV_TAB3_REAPPLIED, names(jmv_opts())), character(0))
})




# === SECTION: the jamovi export helpers ===========================================================

gss <- fx_gss()


tabs <- tab(gss, marital, race, pct = "row")



testthat::test_that("resolveExportPath(dir, filename, ext): folder + bare name + format extension", {
  # folder + bare filename -> folder/filename.ext (extension from the format, not typed)
  p1 <- resolveExportPath("/tmp/reports", "My Table", ext = "xlsx")
  testthat::expect_match(p1, "My Table\\.xlsx$", ignore.case = TRUE)
  testthat::expect_match(p1, "reports", fixed = TRUE)

  # blank folder -> Documents; blank filename -> "Table"
  p2 <- resolveExportPath("", "", ext = "html")
  testthat::expect_match(p2, "Documents", fixed = TRUE)
  testthat::expect_match(p2, "Table\\.html$", ignore.case = TRUE)

  # a typed extension (even a WRONG one) is dropped; the format's extension wins
  p3 <- resolveExportPath("/tmp", "report.csv", ext = "md")
  testthat::expect_match(p3, "report\\.md$", ignore.case = TRUE)
  testthat::expect_false(grepl("csv", p3))

  # surrounding quotes / brackets are stripped from BOTH parts
  p4 <- resolveExportPath('"/tmp/out"', "<Report>", ext = "xlsx")
  testthat::expect_false(grepl('["<>]', p4))
  testthat::expect_match(p4, "Report\\.xlsx$")

  # OS-illegal filename characters are removed (fs::path_sanitize or the base-R fallback)
  p5 <- resolveExportPath("/tmp", 'a/b:c*d?e', ext = "md")
  testthat::expect_false(grepl('[/:*?]', basename(p5)))
  testthat::expect_match(p5, "\\.md$")

  # ~ in the folder expands via the OS home (NOT R's Documents-remapped path.expand)
  p6 <- resolveExportPath("~/Desktop", "t", ext = "md")
  testthat::expect_false(grepl("^~", p6))
  testthat::expect_match(p6, "t\\.md$")

  # a directory pasted into the FILENAME box is reduced to its bare base name
  p7 <- resolveExportPath("/tmp", "sub/dir/Name", ext = "xlsx")
  testthat::expect_match(basename(p7), "^Name\\.xlsx$")
})



testthat::test_that("tab_html_string produces self-contained HTML (table + inlined CSS)", {
  h <- tab_html_string(tabs)
  testthat::expect_true(grepl("<table", h))
  testthat::expect_true(grepl("<style", h))           # CSS inlined, not linked
  testthat::expect_false(grepl("<link", h))           # no external stylesheet
})



testthat::test_that("jmvtab_export writes a valid Excel workbook", {
  testthat::skip_if_not_installed("openxlsx2")
  tmp <- withr::local_tempdir()
  p   <- file.path(tmp, "t.xlsx")
  jmvtab_export(tabs, "excel", p, replace = TRUE)
  testthat::expect_true(file.exists(p))
  wb <- openxlsx2::wb_load(p)                          # opens without error
  testthat::expect_true(length(openxlsx2::wb_get_sheet_names(wb)) >= 1)
})




# === SECTION: the cache-key skeleton ==============================================================

testthat::test_that("tab_cache_keys emits the tier 0-2 skeleton", {
  keys <- tabxplor:::tab_cache_keys(
    na = "keep", wt_name = "w", other_if_less_than = 5, comp = "tab",
    tab_vars = c("region", "year"), row_vars = "marital", col_vars = c("race", "partyid")
  )
  testthat::expect_named(keys, c("tier0", "tier1_common", "tier2"))
  testthat::expect_named(keys$tier0, c("na", "wt", "filter", "population"))
  testthat::expect_named(keys$tier1_common, c("grain", "wt", "other_if_less_than", "population"))
  testthat::expect_named(keys$tier2, "comp")

  # grain = sorted tab_vars; wt carried on both persisted tiers.
  testthat::expect_identical(keys$tier1_common$grain, c("region", "year"))
  testthat::expect_identical(keys$tier0$wt, "w")
  testthat::expect_identical(keys$tier1_common$wt, "w")
  testthat::expect_identical(keys$tier1_common$other_if_less_than, 5)
  testthat::expect_identical(keys$tier2$comp, "tab")
})
