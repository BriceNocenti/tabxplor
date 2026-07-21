# Extracted from test-tab_reg-display.R:93

# prequel ----------------------------------------------------------------------
reg_data <- function() {
  forcats::gss_cat |>
    dplyr::mutate(married = factor(dplyr::if_else(marital == "Married", "Married", "Not married")))
}
first_fmt <- function(t) t[[names(t)[vapply(t, is_fmt, logical(1))][1]]]

# test -------------------------------------------------------------------------
skip_if_not_installed("broom")
d <- reg_data()
t_split <- tab_logit(d, "married", "age", split_var = "race")
md_s <- gsub(intToUtf8(160L), " ", tab_md(t_split, print = FALSE), fixed = TRUE)
expect_true(grepl("Model fit", md_s))
n_groups <- nlevels(forcats::fct_drop(as.factor(d$race)))
expect_equal(length(gregexpr("McFadden R2", md_s)[[1]]), n_groups)
