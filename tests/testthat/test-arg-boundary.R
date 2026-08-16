# PURPOSE: Phase 19i -- lock THE argument boundary. tab() / tab_plain() / tab_num() / tab_counts()
#          run one `tab_resolve_common_args()` (R/tab-resolve.R) instead of re-implementing the same
#          rules; this file pins the five defects that re-implementation had produced, and the new
#          validation the shared boundary makes possible.
# See: R/tab-resolve.R (TAB_ARG_VALUES / tab_validate_args / tab_resolve_common_args); CLAUDE.md
#      > 2.0.0 roadmap > Phase 19i.

# Phase 20a: this file calls functions deprecated in 2.0.0 on purpose -- what it asserts is their
# arithmetic, which the leaf shares with them and which does NOT go away in 2.1.0.
# ⚠ This quiets the TOP-LEVEL calls only: testthat 3e runs local_reproducible_output() inside
# every test_that(), which forces lifecycle_verbosity = "warning" again, so the in-block calls
# still warn. Migrating them to tab() is the corpus sweep routed to Phase 20h.
withr::local_options(lifecycle_verbosity = "quiet", .local_envir = testthat::teardown_env())


ab_gss <- function() {
  gss <- forcats::gss_cat
  gss$w <- ((as.integer(gss$marital) * 3L + as.integer(gss$race)) %% 5L) + 1
  gss
}

# --- validation: three arguments that were checked NOWHERE -------------------------------------
# `tab(totaltab = "tabel")` used to mean, silently, "no total table": nothing validated the value,
# and every consumer tests `totaltab %in% c("line","table")`.
testthat::test_that("an unknown argument value aborts, naming the valid set", {
  gss <- ab_gss()
  testthat::expect_error(tab(gss, marital, race, totaltab = "tabel"), "Unknown .*totaltab")
  testthat::expect_error(tab(gss, marital, race, totaltab = "tabel"), "line")
  testthat::expect_error(tab(gss, marital, race, na = "dropp"),       "Unknown .*na")
  testthat::expect_error(tab(gss, marital, race, pct = "rows"),       "Unknown .*pct")
  testthat::expect_error(tab(gss, marital, race, tot = "rows"),       "Unknown .*tot")
  testthat::expect_error(tab(gss, marital, race, levels = "firsts"),  "Unknown .*levels")
  # the leaves and tab_counts get the restricted `na` vocabulary, from the same table
  testthat::expect_error(tab_plain(gss, marital, race, na = "drop_all"), "Unknown .*na")
})

testthat::test_that("conf_level and n_min are validated, and conf_level guesses the typo", {
  gss <- ab_gss()
  testthat::expect_error(tab(gss, marital, race, conf_level = 95), "0\\.95")
  testthat::expect_error(tab(gss, marital, race, conf_level = 0),  "conf_level")
  testthat::expect_error(tab(gss, marital, race, n_min = -1),      "n_min")
  testthat::expect_error(tab_num(gss, race, tvhours, conf_level = 95), "0\\.95")
})

# A per-row_var LIST `pct` is a SHAPE refusal with its own message (Phase 19h); the shared validator
# must step aside for it rather than deparsing the list into a bogus "unknown value".
testthat::test_that("a per-row_var `pct` list still gets its own message", {
  gss <- ab_gss()
  testthat::expect_error(
    tab(gss, c(marital, relig), race, pct = list(marital = "row", relig = "col")),
    "must be a character vector")
})

# --- defect 1: tab_counts() stored a significance gate it never applied -------------------------
# It builds a color_spec and finalises it, but never ran ci_disable_signif() -- so with an anchor
# that leaves nothing to test, every column carried a `color_signif` the resolver ignored.
testthat::test_that("tab_counts() informs and disables a policy `ci` cannot anchor (D28)", {
  cu <- dplyr::count(ab_gss(), marital, race)
  t <- testthat::expect_message(
    tab_counts(cu, marital, race, counts = n, pct = "row",
               ci = "cell", color = "diff", color_signif = "grey_non_signif"),
    "nothing to test")
  testthat::expect_true(all(vapply(t[vapply(t, tabxplor::is_fmt, logical(1))],
                                   function(x) identical(attr(x, "color_signif"), "ignore"),
                                   logical(1))))
  # ... exactly as tab() has done since 19d
  tt <- suppressMessages(tab(ab_gss(), marital, race, pct = "row",
                             ci = "cell", color = "diff", color_signif = "grey_non_signif"))
  testthat::expect_equal(unique(vapply(tt[vapply(tt, tabxplor::is_fmt, logical(1))],
                                       function(x) attr(x, "color_signif"), "")),
                         "ignore")
})

# --- defect 2: the `stars` option reached tab() and not tab_num() -------------------------------
# tab_num() handed a possibly-NULL `stars` to resolve_leaf_ci(), which tests isTRUE(stars), and
# resolved it against the option only much later, inside num_core(). Measured: the same call built
# a reference interval through tab() and none through tab_num().
testthat::test_that("options(tabxplor.stars) reaches tab_num() as it reaches tab()", {
  gss  <- ab_gss()
  inf  <- function(t) vctrs::field(t$tvhours, "ci_inf")
  withr::with_options(list(tabxplor.stars = TRUE), {
    testthat::expect_equal(inf(tab_num(gss, race, tvhours)), inf(tab(gss, race, tvhours)))
    testthat::expect_true(any(!is.na(inf(tab_num(gss, race, tvhours)))))
  })
  # and with the option off, neither builds one
  withr::with_options(list(tabxplor.stars = FALSE), {
    testthat::expect_true(all(is.na(inf(tab_num(gss, race, tvhours)))))
    testthat::expect_true(all(is.na(inf(tab(gss, race, tvhours)))))
  })
})

# --- defect 3: a direct tab_num() carried no `meta` at all -------------------------------------
testthat::test_that("both leaves record the table's identity (shared leaf_finish())", {
  gss <- ab_gss()
  for (leaf in list(tab_num(gss, race, tvhours, wt = w),
                    tab_plain(gss, marital, race, pct = "row", wt = w))) {
    testthat::expect_equal(tab_kind(leaf), "crosstab")
    testthat::expect_equal(tabxplor:::get_vars_attr(leaf)$wt, "w")
  }
  # the weight footer therefore has something to read on the numeric leaf too
  testthat::expect_match(paste(tab_md(tab_num(gss, race, tvhours, wt = w), print = FALSE),
                               collapse = " "),
                         "Weighted by w")
})

# --- defect 4: tab_num() dropped the policy half of a composite colour --------------------------
# resolve_leaf_ci() was handed the RAW `color_signif` argument instead of the DECODED
# `color_spec$signif`, and its `if (signif_on) ... else "ignore"` overwrote the policy the composite
# carried -- so `tab_num(color = "after_ci")` stored "ignore" where tab() stored "grey_non_signif".
# (19c's standing warning: decode the alias FIRST, normalise second.)
testthat::test_that("a composite colour keeps its policy on the numeric leaf, as on tab()", {
  gss <- ab_gss()
  pol <- function(t) unique(vapply(t[vapply(t, tabxplor::is_fmt, logical(1))],
                                   function(x) attr(x, "color_signif"), ""))
  a <- suppressWarnings(tab_num(gss, race, c(age, tvhours), comp = "all", ci = "ref",
                                color = "after_ci"))
  b <- suppressWarnings(tab(gss, race, c(age, tvhours), comp = "all", ci = "ref",
                            color = "after_ci"))
  testthat::expect_equal(pol(a), "guaranteed_effect")   # what "after_ci" decodes to
  testthat::expect_equal(pol(a), pol(b))
})

# --- tab_counts()'s half-gated limits become real -----------------------------------------------
testthat::test_that("tab_counts() refuses the ci_method slots it cannot honour", {
  cu <- dplyr::count(ab_gss(), marital, race)
  testthat::expect_error(
    tab_counts(cu, marital, race, counts = n, pct = "row",
               ci_method = c(mean_diff = "student")),
    "no mean columns")
  # the two slots that DO apply are accepted, and reach the build
  testthat::expect_no_error(
    tab_counts(cu, marital, race, counts = n, pct = "row", ci = "cell",
               ci_method = c(cell = "wald")))
})

# --- tab_ci()'s vocabulary is declared, and stays the STEP one ----------------------------------
testthat::test_that("tab_ci() aborts on an unknown `ci`, but `diff` is its own native word", {
  t <- tab_plain(ab_gss(), marital, race, pct = "row")
  testthat::expect_error(tab_ci(t, ci = "bogus"), "Unknown .*ci")
  testthat::expect_error(tab_ci(t, ci = "bogus"), "cell")
  # Phase 20a: tab_ci() ITSELF is deprecated now, so the call warns -- but not about its `ci` VALUE.
  # "diff" is this step's own native word (the pipeline called it that way), and only the public
  # anchor vocabulary soft-deprecates it. Collect every warning and check what they are about.
  seen <- character(0)
  withCallingHandlers(tab_ci(t, ci = "diff"),
                      warning = function(w) { seen <<- c(seen, conditionMessage(w))
                                              invokeRestart("muffleWarning") })
  testthat::expect_true(any(grepl("tab_ci()", seen, fixed = TRUE)))
  testthat::expect_false(any(grepl('ci = "diff"', seen, fixed = TRUE)))
})


# =================================================================================================
# Phase 20b -- KEY 1: the argument surface as data
# =================================================================================================

testthat::test_that("20b: everything past the variable roles must be NAMED", {
  # `...` sits right after `wt`, so R itself refuses to bind a 6th positional argument to a formal.
  # Before 20b, position 6 was `sup_cols` and a stray value landed there silently.
  testthat::expect_error(tab(forcats::gss_cat, marital, race, NULL, NULL, "row"),
                         "not named", class = "rlang_error")
  # and a NAMED one at the same place still works
  testthat::expect_s3_class(tab(forcats::gss_cat, marital, race, pct = "row"), "tabxplor_tab")
})

testthat::test_that("20b: an unknown argument gets a suggestion, not 'unused argument'", {
  testthat::expect_error(tab(forcats::gss_cat, marital, race, colour = TRUE),
                         "Did you mean", class = "rlang_error")
  testthat::expect_error(tab_plain(forcats::gss_cat, marital, race, nosuchargument = 1),
                         "Unknown argument", class = "rlang_error")
  # ⚠ formals AFTER `...` are matched EXACTLY, so a partial spelling now gets the suggestion too
  # (it used to partial-match silently).
  testthat::expect_error(tab(forcats::gss_cat, marital, race, color_br = list()),
                         "color_breaks")
})

testthat::test_that("20b: the superseded producers still take every shared argument by name", {
  g <- forcats::gss_cat
  testthat::expect_s3_class(
    suppressWarnings(                                  # "a total column was added" -- not our point
      tab_plain(g, marital, race, pct = "row", ci = "ref", tot = "row", digits = 1,
                color = "difference", color_signif = "grey_non_signif", conf_level = 0.9)),
    "tabxplor_tab")
  testthat::expect_s3_class(
    tab_num(g, marital, c(age, tvhours), ci = "ref", comp = "tab", digits = 2), "tabxplor_tab")
  cnt <- as.data.frame(table(g$marital, g$race), stringsAsFactors = TRUE)
  testthat::expect_s3_class(
    tab_counts(cnt, Var1, Var2, counts = Freq, pct = "row", tot = "row"), "tabxplor_tab")
})

testthat::test_that("20b: a leaf's OWN default survived the move into `...`", {
  # tab_num() starts from color = "auto", ref = "tot", comp = c("tab","all"), na = c("keep","drop")
  # and BOTH leaves from tot = NULL -- declared as `default_for` entries, not inherited from tab().
  d <- tabxplor:::tab_dots_expand(list(), "tab_num")
  testthat::expect_identical(d$color, "auto")
  testthat::expect_identical(d$ref,   "tot")
  testthat::expect_identical(d$comp,  c("tab", "all"))
  testthat::expect_identical(d$na,    c("keep", "drop"))
  testthat::expect_null(d$tot)
  testthat::expect_identical(tabxplor:::tab_dots_expand(list(), "tab_plain")$color, "no")
})

testthat::test_that("20b: the four synthetic labels are ONE option", {
  g <- forcats::gss_cat
  withr::local_options(tabxplor.total_names = c(row = "Ens.", col = "Tot.", other = "Autres"))
  t1 <- tab(g, marital, race, pct = "row")
  testthat::expect_true("Ens." %in% as.character(t1[[1]]))
  testthat::expect_true("Tot." %in% names(t1))
  # a PARTIAL vector leaves the untouched slots at their declared default
  testthat::expect_identical(unname(tabxplor:::tab_total_names()[["tab"]]), "Ensemble")
  # an unknown slot is refused by name
  testthat::expect_error(tabxplor:::tab_total_names_merge(c(rows = "x")), "Unknown")
})

testthat::test_that("20b: the three retired label arguments still work, and say where they went", {
  g <- forcats::gss_cat
  withr::local_options(lifecycle_verbosity = "warning")
  seen <- character(0)
  t1 <- withCallingHandlers(
    tab(g, marital, race, pct = "row", total_names = c("Ens.", "Tot.")),
    warning = function(w) { seen <<- c(seen, conditionMessage(w)); invokeRestart("muffleWarning") })
  testthat::expect_true(any(grepl("tabxplor.total_names", seen, fixed = TRUE)))
  testthat::expect_true("Ens." %in% as.character(t1[[1]]))   # LOSSLESS, not merely deprecated
})

testthat::test_that("20b: tab(row_var =) still nudges, through R's partial matching", {
  # ⚠ `row_var` is a PREFIX of the live `row_vars`, so it never reaches `...`; the deprecation is
  # read off the call the user wrote. This fixture is the one that fails if that is forgotten.
  withr::local_options(lifecycle_verbosity = "warning")
  seen <- character(0)
  withCallingHandlers(invisible(tab(forcats::gss_cat, row_var = marital, col_var = race)),
                      warning = function(w) { seen <<- c(seen, conditionMessage(w))
                                              invokeRestart("muffleWarning") })
  testthat::expect_true(any(grepl("row_var", seen, fixed = TRUE)))
  testthat::expect_true(any(grepl("col_var", seen, fixed = TRUE)))
})

testthat::test_that("20b: tabxplor.stars carries the ladder as well as the switch", {
  g  <- forcats::gss_cat
  t1 <- tab(g, marital, race, pct = "row", ci = "ref", stars = TRUE)
  base <- unique(get_stars(t1[[2]]))
  withr::local_options(tabxplor.stars = c("*" = 0.05, "**" = 0.001))
  testthat::expect_false(identical(base, unique(get_stars(t1[[2]]))))   # a RENDER-time reading
  testthat::expect_identical(names(tabxplor:::tx_stars_ladder()), c("*", "**"))
  # a numeric ladder in the ARGUMENT is refused, naming the option
  testthat::expect_error(tab(g, marital, race, stars = c("*" = 0.05)), "tabxplor.stars")
})

testthat::test_that("20b: the retired signif_levels/signif_labels pair still wins if set", {
  testthat::expect_null(getOption("tabxplor.signif_levels"))    # no longer seeded
  withr::local_options(tabxplor.signif_levels = 0.5, tabxplor.signif_labels = "#")
  testthat::expect_identical(names(tabxplor:::tx_stars_ladder()), "#")
})

testthat::test_that("20b: TAB_ARG_VALUES is DERIVED and unchanged", {
  # a frozen copy of the 19i literal: the derived view must reproduce it exactly, key by key.
  ref <- list(
    pct      = list(values = c("no","row","col","all","all_tabs"), leaf = NULL, size = NA, na_ok = TRUE),
    na       = list(values = c("keep","drop","drop_all","common_base"), leaf = c("keep","drop"),
                    size = 1L, na_ok = FALSE),
    levels   = list(values = c("all","first","auto"), leaf = NULL, size = NA, na_ok = FALSE),
    comp     = list(values = c("tab","all",""), leaf = NULL, size = 1L, na_ok = TRUE),
    tot      = list(values = c("row","col","both","no",""), leaf = NULL, size = NA, na_ok = FALSE),
    totaltab = list(values = c("line","table","no",""), leaf = NULL, size = 1L, na_ok = FALSE),
    totcol   = list(values = c("last","each","all_col_vars","no",""), leaf = NULL, size = 1L,
                    na_ok = FALSE),
    output   = list(values = c("single","list"), leaf = NULL, size = 1L, na_ok = FALSE),
    anova    = list(values = c("welch","classic"), leaf = NULL, size = 1L, na_ok = FALSE))
  testthat::expect_setequal(names(tabxplor:::TAB_ARG_VALUES), names(ref))
  for (k in names(ref)) testthat::expect_identical(tabxplor:::TAB_ARG_VALUES[[k]], ref[[k]], info = k)
})

testthat::test_that("20b: the anti-drift check refuses a formal with no row, or a drifted default", {
  chk <- tabxplor:::tx_check_tab_args
  ns  <- asNamespace("tabxplor")
  with_tab <- function(f, ...) {
    old <- ns$tab
    unlockBinding("tab", ns); on.exit({assign("tab", old, envir = ns); lockBinding("tab", ns)})
    assign("tab", f, envir = ns)
    chk("tab")
  }
  testthat::expect_error(with_tab(function(data, row_vars, nosuchformal = 1) NULL),
                         "no TAB_ARGS row")
  drifted <- ns$tab; formals(drifted)$pct <- "col"
  testthat::expect_error(with_tab(drifted), "declared TAB_ARGS default")
  testthat::expect_true(chk())                                  # the real tree passes
})

testthat::test_that("20b: every option is seeded FROM the declared table", {
  for (k in names(tabxplor:::TAB_OPTIONS)) {
    r <- tabxplor:::TAB_OPTIONS[[k]]
    if (!identical(r$seed, "always")) next
    testthat::expect_identical(getOption(tabxplor:::tx_option_name(k)),
                               tabxplor:::tx_option_default(k), info = k)
  }
})

testthat::test_that("20b: tab_args_rd() documents exactly the formals, in signature order", {
  rd <- tabxplor:::tab_args_rd("tab")
  tags <- sub("^@param ([^ ]+) .*$", "\\1", grep("^@param ", rd, value = TRUE))
  documented <- unlist(strsplit(tags, ",", fixed = TRUE))
  testthat::expect_setequal(documented, setdiff(names(formals(tab)), "..."))
})
