
# === SECTION: the render-level transpose ==========================================================

gss <- fx_gss()


# a small wrapper: the object-level tab_transpose() is soft-deprecated (use transpose = TRUE), but still
# supported for the single-row_var round-trip -- silence the deprecation where we test it on purpose.
xpose <- function(...) {
  withr::local_options(lifecycle_verbosity = "quiet")
  tab_transpose(...)
}


testthat::test_that("transpose(transpose(x)) restores the original", {
  orig <- tab(gss, marital, race, pct = "row", color = "diff")
  tr2  <- xpose(xpose(orig))
  testthat::expect_identical(names(tr2), names(orig))
  testthat::expect_equal(get_pct(tr2[["White"]]), get_pct(orig[["White"]]))
  testthat::expect_identical(get_scale(tr2[["White"]]),    get_scale(orig[["White"]]))
  testthat::expect_identical(is_totcol(tr2[["Total"]]), is_totcol(orig[["Total"]]))
})


testthat::test_that("tab_transpose() emits a soft-deprecation, and errors truthfully", {
  withr::local_options(lifecycle_verbosity = "warning")
  testthat::expect_warning(tab_transpose(tab(gss, marital, race, pct = "row")),
                           class = "lifecycle_warning_deprecated")
  testthat::expect_error(xpose(42), "tabxplor")
})


# === SECTION: RENDER-LEVEL transpose = TRUE (Phase 14o -- finding 8) ==========

# the transposed render model for one table (post-materialise, post-flip)
tx_prep <- function(t, backend = "kable", color = TRUE) {
  compute <- if (color) c("refs", "colors", "bold") else c("refs", "bold")
  tab_export_prep(t, backend = backend, transpose = TRUE, compute = compute)$tables[[1]]
}

# the untransposed reference must materialise the SAME way the transposed flip does (xl-style: `n` a
# column), so the slot grids line up cell-for-cell. backend = "xl" does that -- but it ALSO splits
# every composite's aside into a column of its own, which a transpose does not (the flipped cell
# keeps its bracket), so those columns are dropped below by their declared role.
plain_prep <- function(t) {
  tab_export_prep(t, backend = "xl", compute = c("refs", "colors", "bold"))$tables[[1]]
}

drop_asides <- function(rdu, i) i[!vapply(rdu$tab[i], function(c)
  identical(tabxplor:::get_role(c), "aside"), logical(1))]


testthat::test_that("ONE Total column, no Total_<var> suffix (finding 8 + common_totrow collapse)", {
  # Phase 18m: the single-Total-column transpose use case opts into the shared Total (common_totrow).
  t  <- tab(gss, c(marital, race), c(relig, tvhours), pct = "row", color = TRUE, na = "drop",
            common_totrow = TRUE)
  md <- tab_md(t, transpose = TRUE, print = FALSE, color = FALSE)
  testthat::expect_false(grepl("Total_", md, fixed = TRUE))
  rd <- tx_prep(t, color = FALSE)
  testthat::expect_length(rd$roles$totcols, 1L)                 # exactly one Total column
})


testthat::test_that("n sits right after Total; numeric means last (finding 8)", {
  t  <- tab(gss, c(marital, race), c(relig, tvhours), pct = "row", na = "drop")
  rd <- tx_prep(t, color = FALSE)
  lv <- as.character(rd$tab[["levels"]])
  i_tot <- which(lv == "Total"); i_n <- which(lv == "n"); i_mean <- which(lv == "mean")
  testthat::expect_identical(i_n, i_tot + 1L)                   # n immediately after Total
  testthat::expect_true(i_mean > i_n)                          # the numeric row after both
})


testthat::test_that("leading [variable-name, levels] label columns mirror (row_var, levels)", {
  t  <- tab(gss, c(marital, race), c(relig, tvhours), pct = "row", na = "drop")
  rd <- tx_prep(t, color = FALSE)
  testthat::expect_identical(names(rd$tab)[1:2], c("row_var", "levels"))
  testthat::expect_true(all(c("relig", "tvhours") %in% rd$tab[["row_var"]]))
  # the old row_vars span the new columns (a col_var header naming marital + race)
  testthat::expect_true(all(c("marital", "race") %in% rd$col_var_header$label))
})


testthat::test_that("a single-row_var transpose still renders exactly like a native col% table", {
  transposed <- tab_md(tab(gss, marital, race, pct = "row"), transpose = TRUE,
                       print = FALSE, color = FALSE)
  native     <- tab_md(tab(gss, race, marital, pct = "col"), print = FALSE, color = FALSE)
  testthat::expect_identical(transposed, native)
})


testthat::test_that("transpose = TRUE keeps both colour channels + the numeric mean's sd", {
  t <- tab(gss, marital, race, pct = "row", color = c("diff", "ratio"))
  h <- as.character(tab_kable(t, transpose = TRUE))
  testthat::expect_match(h, "tx-pill")                          # background-channel colour survives
  testthat::expect_match(h, 'class="[^"]*p[0-9]')               # text-channel slot survives
  tn <- set_display(tab_num(gss, race, c(age, tvhours), color = "diff"), "mean_sd")
  md <- tab_md(tn, transpose = TRUE, print = FALSE)
  testthat::expect_true(grepl(intToUtf8(0x03c3), md))           # inline sigma sd survives the flip
})


# Phase 19h (D1): rd2 is a MODIFICATION of rd, not a 39-slot literal. The literal had already lost two
# slots and was losing `ann$keep_black` -- the "do not grey this cell" anchor set -- which the html
# engine reads behind a length-check fallback, so a transposed regression's GOF footer cells rendered
# GREY where the native render keeps them black, with no error and no test.
testthat::test_that("the transposed model keeps every slot the flip does not touch (D1)", {
  t  <- tab(gss, c(marital, race), relig, pct = "row", na = "drop")
  rd  <- tabxplor:::tab_export_prep(t, backend = "kable",
                                    compute = c("refs", "colors", "bold"))$tables[[1]]
  rd2 <- tabxplor:::tab_export_prep(t, backend = "kable", transpose = TRUE,
                                    compute = c("refs", "colors", "bold"))$tables[[1]]
  # no top-level slot is dropped by the flip (it may be added: cells / tooltips / color_src ...)
  testthat::expect_true(all(names(rd) %in% names(rd2)))
  # every per-cell ann field survives too -- this is where keep_black went missing
  testthat::expect_true(all(names(rd$ann[[1]]) %in% names(rd2$ann[[1]])))
  testthat::expect_length(rd2$ann[[1]]$keep_black, nrow(rd2$tab))
})


# Phase 20i: TAB_OPS gained a `kind` predicate. The DEPRECATED object-level tab_transpose() refuses a
# regression with a kind-specific reason (pointing at transpose = TRUE, which DOES support reg -- the
# tests below) instead of the misleading crosstab "exactly one row variable" (a reg reads as `merged`
# via its var-role predictor column).
testthat::test_that("Phase 20i: tab_transpose() on a regression aborts with a kind-specific message", {
  r <- suppressMessages(tab_reg(fx_gss_fmt(), "married", c("relig", "age"),
                                family = "binomial", cleannames = FALSE))
  testthat::expect_false(tab_supports(r, "transpose_object"))
  testthat::expect_error(xpose(r), "transpose = TRUE", fixed = TRUE)
})


testthat::test_that("a transposed regression's footer cells stay black in HTML (D1)", {
  d <- fx_gss_fmt()
  r <- suppressMessages(tab_reg(d, "married", c("relig", "age"), family = "binomial",
                                cleannames = FALSE))
  h <- as.character(tab_html(r, transpose = TRUE))
  # the GOF footer values (N, McFadden R2 ...) are reading anchors: they must NOT carry a grey class
  greyed <- grepl('class="[^"]*\\bg[12]\\b[^"]*"[^>]*>[^<]*McFadden', h)
  testthat::expect_false(greyed)
  # non-vacuous: the untransposed render has the same footer, also un-greyed
  testthat::expect_match(as.character(tab_html(r)), "McFadden")
})


# === SECTION: the render-level transpose ==========================================================

gss <- fx_gss()


# a small wrapper: the object-level tab_transpose() is soft-deprecated (use transpose = TRUE), but still
# supported for the single-row_var round-trip -- silence the deprecation where we test it on purpose.
xpose <- function(...) {
  withr::local_options(lifecycle_verbosity = "quiet")
  tab_transpose(...)
}


testthat::test_that("cell values transpose: orig[i, j] == transposed[j, i]", {
  orig <- tab(gss, marital, race, pct = "row", color = "diff")
  tr   <- xpose(orig)
  white_row <- which(as.character(dplyr::pull(tr, 1)) == "White")
  marital_lv <- "Never married"
  testthat::expect_equal(get_pct(tr[[marital_lv]])[white_row],
                         get_pct(orig[["White"]])[which(as.character(dplyr::pull(orig, 1)) == marital_lv)])
  testthat::expect_equal(get_diff(tr[[marital_lv]])[white_row],
                         get_diff(orig[["White"]])[which(as.character(dplyr::pull(orig, 1)) == marital_lv)])
})


# === SECTION: RENDER-LEVEL transpose = TRUE (Phase 14o -- finding 8) ==========

# the transposed render model for one table (post-materialise, post-flip)
tx_prep <- function(t, backend = "kable", color = TRUE) {
  compute <- if (color) c("refs", "colors", "bold") else c("refs", "bold")
  tab_export_prep(t, backend = backend, transpose = TRUE, compute = compute)$tables[[1]]
}


# the untransposed reference must materialise the SAME way the transposed flip does (xl-style: `n` a
# column), so the slot grids line up cell-for-cell. backend = "xl" does that -- but it ALSO splits
# every composite's aside into a column of its own, which a transpose does not (the flipped cell
# keeps its bracket), so those columns are dropped below by their declared role.
plain_prep <- function(t) {
  tab_export_prep(t, backend = "xl", compute = c("refs", "colors", "bold"))$tables[[1]]
}


drop_asides <- function(rdu, i) i[!vapply(rdu$tab[i], function(c)
  identical(tabxplor:::get_role(c), "aside"), logical(1))]


testthat::test_that("Phase 17g: transpose carries the caption/title through the flip (drift fix)", {
  # rd2 used to drop reg_title / caption / empirical_tips, so a transposed table lost its title. The
  # stored set_caption() must survive the flip into md's and html's shared rd_caption() fallback.
  tc <- tab(gss, c(marital, race), relig, pct = "row", na = "drop") |>
    set_caption("A transposed caption")
  testthat::expect_match(tab_md(tc, transpose = TRUE, print = FALSE), "A transposed caption", fixed = TRUE)
  testthat::expect_match(as.character(tab_kable(tc, transpose = TRUE)), "A transposed caption", fixed = TRUE)
  # non-vacuous: the caption is present WITHOUT transpose too (so the assertion is not testing nothing)
  testthat::expect_match(tab_md(tc, print = FALSE), "A transposed caption", fixed = TRUE)
})
