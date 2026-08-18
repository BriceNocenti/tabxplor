# PURPOSE: significance stars are OPT-IN (default off), storage-driven, and right-padded.
# ROLE: locks the bug-fix that made stars opt-in in format()/tab() (default FALSE), kept them the
#        default for tab_reg(), removed the tooltip leakage, and right-pads them so numbers align.
# See: CLAUDE.md > Phase 18a (Bug corrections).

gss <- forcats::gss_cat

# a synthetic diff column with three significance levels stored in `pvalue`
star_col <- function() {
  fmt(n = rep(100L, 3), scale = "points", pct_type = "row", pct = c(0.4, 0.5, 0.6), diff = c(0.1, 0, -0.1),
      ci_inf = c(0.05, -0.10, -0.20), ci_sup = c(0.15, 0.10, -0.05),
      pvalue = c(0.0005, 0.5, 0.07), display = "pct")
}

testthat::test_that("format(): stars are opt-in (default off), appended only with stars = TRUE", {
  x <- star_col()
  testthat::expect_false(any(grepl("\\*", format(x))))          # default: none
  out <- format(x, stars = TRUE)
  testthat::expect_equal(stringi::stri_count_regex(out, "\\*"), c(3L, 0L, 1L))  # ***, none, *
})

testthat::test_that("stars are right-padded so numbers stay aligned (monospace)", {
  out <- format(star_col(), stars = TRUE)
  testthat::expect_equal(length(unique(nchar(out))), 1L)        # every value cell equal width
  # a cell with fewer stars is space-padded to the column-max star width
  testthat::expect_true(grepl("\\*  $", out[3]))                # one star + two pad spaces
})

testthat::test_that("secondary-field re-render (tooltip path) shows no stars by default", {
  x <- star_col()
  # tooltips re-render alternate fields via format(set_display(x, ...)) at the DEFAULT stars = FALSE
  testthat::expect_false(any(grepl("\\*", format(set_display(x, "n")))))
  testthat::expect_false(any(grepl("\\*", format(set_display(x, "diff")))))
})

testthat::test_that("tab() stores no pvalue and shows no stars by default; stars = TRUE does", {
  t0 <- tab(gss, marital, race, pct = "row", ci = "ref")
  testthat::expect_true(all(is.na(get_pvalue(t0$White))))
  testthat::expect_false(any(grepl("\\*", format(t0$White, stars = TRUE))))  # no pvalue -> none

  t1 <- tab(gss, marital, race, pct = "row", ci = "ref", stars = TRUE)
  testthat::expect_true(any(!is.na(get_pvalue(t1$White))))
  testthat::expect_true(any(grepl("\\*", format(t1$White, stars = TRUE))))
})

testthat::test_that("stars = TRUE forces ci = 'diff' when ci is unset (Phase 16f)", {
  # The reported bug: `stars = TRUE` with no `ci` silently showed nothing, because no difference CI
  # (hence no pvalue) was computed. stars must now surface on its own, on both factor and mean columns.
  tf <- tab(gss, marital, race, pct = "row", stars = TRUE)             # factor, NO ci set
  testthat::expect_true(any(!is.na(get_pvalue(tf$White))))
  testthat::expect_true(any(grepl("\\*", format(tf$White, stars = TRUE))))

  tn <- tab(gss, marital, tvhours, stars = TRUE)                       # numeric mean, NO ci set
  ncol <- reg_fmt_cols(tn)[[1]]
  testthat::expect_true(any(!is.na(get_pvalue(tn[[ncol]]))))

  # byte-safety: the default (no stars) still stores no pvalue, so ordinary tables are unchanged
  t0 <- tab(gss, marital, race, pct = "row")
  testthat::expect_true(all(is.na(get_pvalue(t0$White))))
  # and it also folds with a {ci} display: the bracket AND the stars appear (stars opt-in in format())
  tb <- tab(gss, marital, race, pct = "row", display = "{ci}", stars = TRUE)
  out <- format(tb$White, stars = TRUE)
  testthat::expect_true(any(grepl("\\[", out)))                        # the [ci] bracket renders
  testthat::expect_true(any(grepl("\\*", out)))                        # and stars ride the cell
})

testthat::test_that("star presence is the dual of the CI excluding neutral (no contradiction)", {
  col <- tab(gss, marital, race, pct = "row", ci = "ref", stars = TRUE)$White
  st  <- get_stars(col)
  sig <- get_ci_inf(col) > 0 | get_ci_sup(col) < 0
  val <- !is.na(get_pvalue(col))
  testthat::expect_equal(nzchar(st)[val], sig[val])            # starred <=> CI excludes 0
})

testthat::test_that("tab_reg() shows stars by default; stars = FALSE strips the pvalue", {
  testthat::skip_if_not_installed("broom")
  d <- gss |> dplyr::mutate(m = factor(dplyr::if_else(marital == "Married", "Married", "No")))
  r1  <- tab_reg(d, "m", c("race", "rincome"), family = "binomial")
  orc <- reg_fmt_cols(r1)[[1]]
  testthat::expect_true(any(!is.na(get_pvalue(r1[[orc]]))))
  testthat::expect_true(any(grepl("\\*", format(r1[[orc]], stars = TRUE))))

  r0 <- tab_reg(d, "m", c("race", "rincome"), family = "binomial", stars = FALSE)
  testthat::expect_true(all(is.na(get_pvalue(r0[[orc]]))))
})

testthat::test_that("tab_kable main cells carry stars (opt-in) but tooltips do not leak them", {
  testthat::skip_if_not_installed("kableExtra")
  t1  <- tab(gss, marital, race, pct = "row", ci = "ref", stars = TRUE)
  html <- as.character(tab_kable(t1, tooltip = TRUE))
  testthat::expect_true(grepl("\\*", html))                    # main cells show stars
  # every data-toggle tooltip title is star-free (secondary fields do not leak stars)
  titles <- unlist(regmatches(html, gregexpr('title="[^"]*"', html)))
  testthat::expect_false(any(grepl("\\*", titles)))
})
