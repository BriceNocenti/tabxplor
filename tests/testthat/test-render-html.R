# PURPOSE: Lock the Phase 10e tab_kable() render seam -- both engines.
# ROLE: Guards (a) the kableExtra engine against drift from the legacy carve (structure snapshot),
#       (b) the home-built html engine (structure snapshot + self-contained + DOM-size win),
#       (c) cross-engine content parity (same cell text + tooltip content -> the two engines can't
#       diverge silently). The kableExtra byte-identity vs the pre-10e code was proven by a git-stash
#       A/B; these snapshots keep it locked going forward.
# KEY CONSTRAINTS:
#   - Must run via test_check("tabxplor"); skip_if_not_installed("kableExtra") on the kable branch.
#   - Snapshots strip the <style> blocks (kableExtra's tab.css / the html engine's block are large,
#     stable, and not the point) so the diff is the table STRUCTURE.

gss <- forcats::gss_cat

# --- helpers ---------------------------------------------------------------------------
rh_strip_style <- function(h) {
  h <- gsub("(?s)<style[^>]*>.*?</style>", "<!--css-->", as.character(h), perl = TRUE)
  gsub("(?s)<script[^>]*>.*?</script>", "", h, perl = TRUE)
}
rh_tbody <- function(h) {
  m <- regmatches(as.character(h), regexpr("(?s)<tbody>.*?</tbody>", as.character(h), perl = TRUE))
  if (length(m) == 0) "" else m
}
rh_cells <- function(h) {                       # tbody cell text tokens (data, not styling)
  t <- unlist(strsplit(gsub("<[^>]*>", "\x01", rh_tbody(h)), "\x01"))
  t <- trimws(t); t[nzchar(t)]
}
rh_titles <- function(h) {                       # non-empty tooltip contents
  ti <- unlist(regmatches(as.character(h), gregexpr('title="[^"]+"', as.character(h))))
  sort(unique(ti[ti != 'title=""']))
}

# === SECTION: kableExtra engine (default) -- version-robust structure assertions ==========
# WARNING: do NOT snapshot this engine's bytes. Its cells come from kableExtra::cell_spec(), whose
# output is version-unstable -- 1.4.0 -> 1.4.1 moved the rgba alpha (255 -> 1), dropped leading
# padding and (text_spec) leaked a stray `class="TRUE"`. Byte-snapshotting it made CI red on all 5
# platforms for a change tabxplor did not make, and put kableExtra's release schedule in charge of
# our test suite. We do not own that HTML, so we assert the parts we DO own -- the cells, the
# colouring, the tooltips, the geometry -- which survived 1.4.1 untouched. The home-built engine's
# HTML *is* ours, so it keeps its byte snapshot below.

testthat::test_that("tab_kable kableExtra engine structure is stable", {
  testthat::skip_if_not_installed("kableExtra")
  counts   <- tab(gss, marital, race)
  row_diff <- tab(gss, marital, race, pct = "row", color = "diff")
  chi2     <- suppressWarnings(tab(gss, marital, race, pct = "row", chi2 = TRUE))

  # geometry: exactly one <tbody>, one <tr> per row. chi2 gains ONE extra row -- the p-value row is
  # materialised at display (Phase 10i-B), so it is not in nrow(tb).
  for (tb in list(counts, row_diff)) {
    h <- rh_strip_style(tab_kable(tb))
    testthat::expect_match(h, "<table")
    body <- rh_tbody(h)
    testthat::expect_length(body, 1L)
    testthat::expect_equal(lengths(regmatches(body, gregexpr("<tr", body)))[[1]], nrow(tb))
  }
  bc <- rh_tbody(rh_strip_style(suppressWarnings(tab_kable(chi2))))
  testthat::expect_equal(lengths(regmatches(bc, gregexpr("<tr", bc)))[[1]], nrow(chi2) + 1L)

  # colouring reaches the cells, and only when asked for
  hd <- rh_strip_style(tab_kable(row_diff))
  hm <- rh_strip_style(tab_kable(row_diff, color = FALSE))
  testthat::expect_match(hd, "color:")
  testthat::expect_false(identical(hd, hm))
  # theme is honoured
  testthat::expect_false(identical(rh_strip_style(tab_kable(row_diff, theme = "dark")), hd))
  # tooltips carry the underlying fields
  testthat::expect_true(any(grepl("^title=\"diff:", rh_titles(hd))))
})

# === SECTION: home-built html engine -- structure snapshot + self-contained ==============

testthat::test_that("tab_kable html engine structure is stable", {
  counts   <- tab(gss, marital, race)
  row_diff <- tab(gss, marital, race, pct = "row", color = "diff")
  bg       <- tab(gss, marital, race, pct = "row", color = c("diff", "ratio"))
  chi2     <- suppressWarnings(tab(gss, marital, race, pct = "row", chi2 = TRUE))

  testthat::expect_snapshot(cat(rh_strip_style(tab_kable(counts,   engine = "html"))))
  testthat::expect_snapshot(cat(rh_strip_style(tab_kable(row_diff, engine = "html"))))
  testthat::expect_snapshot(cat(rh_strip_style(tab_kable(row_diff, engine = "html", theme = "dark"))))
  testthat::expect_snapshot(cat(rh_strip_style(tab_kable(bg,       engine = "html"))))
  testthat::expect_snapshot(cat(rh_strip_style(suppressWarnings(tab_kable(chi2, engine = "html")))))
})

testthat::test_that("html engine output is self-contained (inline <style>, no external <link>)", {
  h <- as.character(tab_kable(tab(gss, marital, race, pct = "row", color = "diff"), engine = "html"))
  testthat::expect_match(h, "<table")
  testthat::expect_match(h, "<style")
  testthat::expect_false(grepl("<link", h))
  testthat::expect_false(grepl("includeCSS|lightable|cosmo", h))
})

# === SECTION: cross-engine content parity ================================================

testthat::test_that("both engines carry the same cell text and tooltip content", {
  testthat::skip_if_not_installed("kableExtra")
  for (tb in list(
    tab(gss, marital, race, pct = "row", color = "diff"),
    tab(gss, marital, race),
    tab(gss, marital, c(race, relig), pct = "row", color = "diff"),
    tab_num(gss, race, c(age, tvhours), marital, comp = "all")
  )) {
    ke <- suppressWarnings(tab_kable(tb, engine = "kableExtra"))
    ht <- suppressWarnings(tab_kable(tb, engine = "html"))
    testthat::expect_identical(rh_cells(ke), rh_cells(ht))   # same data
    testthat::expect_identical(rh_titles(ke), rh_titles(ht)) # same tooltip content
  }
})

testthat::test_that("html engine drops the per-cell <span> wrapper (DOM-size win)", {
  testthat::skip_if_not_installed("kableExtra")
  tb <- tab(gss, marital, race, pct = "row", color = "diff")
  ke <- rh_tbody(tab_kable(tb, engine = "kableExtra"))
  ht <- rh_tbody(tab_kable(tb, engine = "html"))
  n_ke <- lengths(gregexpr("<span", ke))
  n_ht <- if (grepl("<span", ht)) lengths(gregexpr("<span", ht)) else 0L
  testthat::expect_gt(n_ke, 0L)          # kableExtra wraps every cell
  testthat::expect_lt(n_ht, n_ke)        # html has far fewer (ideally 0) tbody spans
})

# === SECTION: get_data + graceful degrade both engines ===================================

testthat::test_that("get_data returns a data.frame on both engines", {
  testthat::skip_if_not_installed("kableExtra")
  tb <- tab(gss, marital, race, pct = "row", color = "diff")
  testthat::expect_s3_class(tab_kable(tb, get_data = TRUE), "data.frame")
  testthat::expect_s3_class(tab_kable(tb, get_data = TRUE, engine = "html"), "data.frame")
})

testthat::test_that("plain data.frame degrades gracefully on both engines", {
  df <- data.frame(a = 1:3, b = letters[1:3])
  testthat::expect_no_error(suppressMessages(tab_kable(df, engine = "html")))
  h <- as.character(suppressMessages(tab_kable(df, engine = "html")))
  testthat::expect_match(h, "<table")
  testthat::skip_if_not_installed("kableExtra")
  testthat::expect_no_error(suppressMessages(tab_kable(df)))
})

testthat::test_that("tab_kable renders a non-mergeable list instead of erroring (list method)", {
  testthat::skip_if_not_installed("kableExtra")
  # different col_vars -> not mergeable (previously an error for kable)
  lst <- list(tab(gss, marital, race,  pct = "row"),
              tab(gss, marital, relig, pct = "row"))
  for (eng in c("kableExtra", "html")) {
    h <- as.character(suppressWarnings(tab_kable(lst, engine = eng)))
    testthat::expect_gte(lengths(gregexpr("<table", h)), 2L)  # both tables rendered
  }
})

testthat::test_that("n_min blanked cells render empty (no literal NA) on both engines", {
  testthat::skip_if_not_installed("kableExtra")
  tb <- suppressWarnings(tab(gss, marital, race, pct = "row", n_min = 1000))
  for (eng in c("kableExtra", "html")) {
    h <- as.character(suppressWarnings(tab_kable(tb, engine = eng)))
    testthat::expect_false(grepl(">NA<", h))       # no literal NA cell content
    testthat::expect_match(h, "<table")
  }
})

testthat::test_that("engine is resolved from the option", {
  testthat::skip_if_not_installed("kableExtra")
  tb <- tab(gss, marital, race, pct = "row", color = "diff")
  withr::local_options(tabxplor.tab_kable_engine = "html")
  h <- as.character(tab_kable(tb))
  testthat::expect_match(h, "tabxplor-tab")   # the home-built class
})
