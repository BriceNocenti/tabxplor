# PURPOSE: Lock the tabxplor_fmt vctrs record contract (fields + attributes) and its
#          serialization stability, so any change to the record shape fails loudly.
# ROLE: Retro-compatibility guardrail for the 1.4.0 internal refactors. Locks the Phase 1a
#       18-field baseline (was 15: +pvalue +tot_n +ci_inf +ci_sup, rr->ratio, dropped ci).
# KEY CONSTRAINTS:
#   - This test is DELIBERATELY BRITTLE. Update it ONLY when intentionally adding, removing,
#     or renaming a field/attribute of tabxplor_fmt -- follow the `/vctrs-field` skill.
#   - The hardcoded vectors below ARE the contract (the 18-field 1.4.0 baseline). `ci` is no
#     longer a stored field: it is derived from the `ci_inf`/`ci_sup` bounds by get_ci()
#     (bounds-shim), and the public `fmt(ci=)` arg maps a symmetric half-width onto them.
# See: CLAUDE.md > 1.4.0 roadmap (Phase 1) and Design Decisions > Type System.

# The 18 per-cell fields, in construction order (new_fmt() -> vctrs::new_rcrd()).
fmt_contract_fields <- c(
  "n", "display", "digits", "wn", "pct", "mean", "diff", "ratio", "ctr", "var",
  "ci_inf", "ci_sup", "pvalue", "or", "tot_n", "in_totrow", "in_tottab", "in_refrow"
)

# Storage type of each field (typeof), as guaranteed by the vec_cast lines in fmt().
fmt_contract_field_types <- c(
  n = "integer", display = "character", digits = "integer", wn = "double",
  pct = "double", mean = "double", diff = "double", ratio = "double", ctr = "double",
  var = "double", ci_inf = "double", ci_sup = "double", pvalue = "double", or = "double",
  tot_n = "double", in_totrow = "logical", in_tottab = "logical", in_refrow = "logical"
)

# The 9 per-column attributes and their constructor defaults. Phase 5 added `color_signif`
# (the significance policy: "ignore" / "grey_non_signif" / "guaranteed_effect") -- it cannot fold
# into `color` (which is measure x channel) and pillar_shaft renders columns standalone, so the
# policy must live on the column. The `color` attribute is now length 1 (text) or 2 (text, bg).
# Phase 10i-A DROPPED the Phase-10c `display_spec` attribute (10 -> 9): the opt-in composite display
# is now a per-cell `display`-FIELD {} template ("{pct} (n={n})"), not a column attribute.
fmt_contract_attr_defaults <- list(
  type = "n", comp_all = NA, ref = "", ci_type = "",
  col_var = "", totcol = FALSE, refcol = FALSE, color = "", color_signif = "ignore"
)

testthat::test_that("fmt has exactly the contracted fields, in order", {
  x <- fmt(1)
  testthat::expect_identical(vctrs::fields(x), fmt_contract_fields)
  testthat::expect_length(vctrs::fields(x), 18L)
})

testthat::test_that("each fmt field has the contracted storage type", {
  x <- fmt(1)
  for (f in fmt_contract_fields) {
    testthat::expect_identical(
      typeof(vctrs::field(x, f)), fmt_contract_field_types[[f]],
      info = paste0("field '", f, "'")
    )
  }
})

testthat::test_that("fmt carries exactly the contracted column attributes with right defaults", {
  x <- fmt(1)
  # Presence + default value of every contracted attribute (read via attr(), the documented access).
  for (a in names(fmt_contract_attr_defaults)) {
    testthat::expect_identical(
      attr(x, a, exact = TRUE), fmt_contract_attr_defaults[[a]],
      info = paste0("attribute '", a, "'")
    )
  }
  # No UNCONTRACTED column attribute has crept in (structural attrs excluded).
  structural <- c("names", "class", "row.names")
  col_attrs <- setdiff(names(attributes(x)), structural)
  testthat::expect_setequal(col_attrs, names(fmt_contract_attr_defaults))
})

testthat::test_that("fmt survives saveRDS/readRDS round-trip with all fields and attributes", {
  x <- fmt(
    n = c(10L, 20L), type = "row", digits = 1L, display = c("n", "pct"),
    wn = c(9.5, 19.4), pct = c(NA, 0.5), mean = c(NA, NA), diff = c(NA, 0.1),
    ctr = c(NA, 0.3), var = c(NA, NA), ci = c(NA, 0.02),
    in_totrow = c(FALSE, TRUE), in_refrow = c(TRUE, FALSE),
    comp_all = TRUE, ref = "tot", ci_type = "cell", col_var = "sex",
    totcol = FALSE, color = "diff"
  )

  tmp <- tempfile(fileext = ".rds")
  on.exit(unlink(tmp), add = TRUE)
  saveRDS(x, tmp)
  y <- readRDS(tmp)

  testthat::expect_identical(y, x)                       # whole object identical
  testthat::expect_identical(vctrs::fields(y), vctrs::fields(x))
  for (f in fmt_contract_fields) {
    testthat::expect_identical(vctrs::field(y, f), vctrs::field(x, f),
                               info = paste0("field '", f, "'"))
  }
  for (a in names(fmt_contract_attr_defaults)) {
    testthat::expect_identical(attr(y, a, exact = TRUE), attr(x, a, exact = TRUE),
                               info = paste0("attribute '", a, "'"))
  }
})

# The `ci` bounds-shim (Phase 3a): the public fmt(ci=) half-width is stored as ABSOLUTE
# ci_inf/ci_sup bounds around the estimate the interval is centred on (here the proportion
# pct), and get_ci() / $ci read the half-width back as ci_sup - centre.
testthat::test_that("fmt(ci=) stores absolute bounds and get_ci() reads the half-width back", {
  x <- fmt(n = c(10L, 20L), type = "row", pct = c(0.4, 0.5), ci = c(NA, 0.02))
  testthat::expect_identical(vctrs::field(x, "ci_sup"), c(NA_real_, 0.52))  # pct + ci
  testthat::expect_identical(vctrs::field(x, "ci_inf"), c(NA_real_, 0.48))  # pct - ci
  testthat::expect_equal(get_ci(x), c(NA_real_, 0.02))       # half-width read back
  testthat::expect_identical(x$ci,   get_ci(x))              # $ci still works
})

# Human-readable second signal. Skipped on CRAN by default. Regenerate consciously with
# testthat::snapshot_accept("fmt-contract") only when the contract intentionally changes.
testthat::test_that("fmt record shape snapshot", {
  testthat::expect_snapshot({
    x <- fmt(1)
    cat("fields:\n")
    print(vctrs::fields(x))
    cat("\nfield types:\n")
    print(vapply(vctrs::fields(x), function(f) typeof(vctrs::field(x, f)), character(1)))
    cat("\ncolumn attributes:\n")
    print(sort(setdiff(names(attributes(x)), c("names", "class", "row.names"))))
  })
})
