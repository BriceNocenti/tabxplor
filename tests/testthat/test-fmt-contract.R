# PURPOSE: Lock the tabxplor_fmt vctrs record contract (fields + attributes) and its
#          serialization stability, so any change to the record shape fails loudly.
# ROLE: Retro-compatibility guardrail for the 1.4.0 internal refactors (esp. the Phase 1a
#       15 -> 18 field pass: +pvalue +tot_n +ci_inf +ci_sup, rr->ratio, drop ci).
# KEY CONSTRAINTS:
#   - This test is DELIBERATELY BRITTLE. Update it ONLY when intentionally adding, removing,
#     or renaming a field/attribute of tabxplor_fmt -- follow the `/vctrs-field` skill.
#   - The hardcoded vectors below ARE the contract (currently the 15-field baseline). Phase 1a
#     flips them to 18: add `pvalue`, `tot_n`, `ci_inf`, `ci_sup`; rename `rr`->`ratio`; drop `ci`.
# See: CLAUDE.md > 1.4.0 roadmap (Phase 1) and Design Decisions > Type System.

# The 15 per-cell fields, in construction order (new_fmt() -> vctrs::new_rcrd()).
fmt_contract_fields <- c(
  "n", "display", "digits", "wn", "pct", "mean", "diff", "ctr", "var", "ci",
  "rr", "or", "in_totrow", "in_tottab", "in_refrow"
)

# Storage type of each field (typeof), as guaranteed by the vec_cast lines in fmt().
fmt_contract_field_types <- c(
  n = "integer", display = "character", digits = "integer", wn = "double",
  pct = "double", mean = "double", diff = "double", ctr = "double",
  var = "double", ci = "double", rr = "double", or = "double",
  in_totrow = "logical", in_tottab = "logical", in_refrow = "logical"
)

# The 8 per-column attributes and their constructor defaults.
fmt_contract_attr_defaults <- list(
  type = "n", comp_all = NA, ref = "", ci_type = "",
  col_var = "", totcol = FALSE, refcol = FALSE, color = ""
)

testthat::test_that("fmt has exactly the contracted fields, in order", {
  x <- fmt(1)
  testthat::expect_identical(vctrs::fields(x), fmt_contract_fields)
  testthat::expect_length(vctrs::fields(x), 15L)
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
