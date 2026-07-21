# Extracted from test-tab_reg.R:216

# prequel ----------------------------------------------------------------------
reg_data <- function() {
  forcats::gss_cat |>
    dplyr::mutate(
      married = factor(dplyr::if_else(marital == "Married", "Married", "Not married"))
    )
}

# test -------------------------------------------------------------------------
skip_if_not_installed("broom")
m <- suppressWarnings(tab_reg(reg_data(), "married", c("age", "race"),
                                family = "binomial", empirical = TRUE, cleannames = FALSE))
role <- tabxplor:::get_role(m)
fmt_cols <- names(m)[purrr::map_lgl(m, is_fmt)]
emp   <- fmt_cols[startsWith(fmt_cols, "Emp.")]
model <- setdiff(fmt_cols, emp)
expect_true(length(emp) >= 1L && length(model) >= 1L)
expect_true(all(role[emp]   == "emp"))
expect_true(all(role[model] == "model"))
names(m)[match(emp[1], names(m))] <- "Crude"
expect_identical(tabxplor:::get_role(m[["Crude"]]), "emp")
