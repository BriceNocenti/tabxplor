# Extracted from test-tab_reg.R:652

# prequel ----------------------------------------------------------------------
reg_data <- function() {
  forcats::gss_cat |>
    dplyr::mutate(
      married = factor(dplyr::if_else(marital == "Married", "Married", "Not married"))
    )
}
gb_data <- function() {
  reg_data() |>
    dplyr::mutate(score = pmin(as.integer(tvhours), 10L))   # a summed score 0..10 ("yes" out of 10)
}
mnl_data <- function() {                                    # nominal 3-level party, Ind = reference
  forcats::gss_cat |>
    dplyr::mutate(party3 = factor(dplyr::case_when(
        grepl("democrat", partyid)   ~ "Dem",
        grepl("republican", partyid) ~ "Rep",
        partyid %in% c("Independent", "Ind,near rep", "Ind,near dem") ~ "Ind"),
      levels = c("Ind", "Dem", "Rep")))
}
ord_data <- function() {                                    # ordered spectrum Rep < Ind < Dem
  mnl_data() |>
    dplyr::mutate(spectrum = factor(as.character(party3),
                                    levels = c("Rep", "Ind", "Dem"), ordered = TRUE))
}
ord_income_data <- function() {                             # ordered income, known to violate PO
  forcats::gss_cat |>
    dplyr::mutate(income3 = factor(dplyr::case_when(
        rincome %in% c("Lt $1000", "$1000 to 2999", "$3000 to 3999", "$4000 to 4999",
                       "$5000 to 5999") ~ "1-low",
        rincome %in% c("$6000 to 6999", "$7000 to 7999", "$8000 to 9999",
                       "$10000 - 14999") ~ "2-mid",
        rincome %in% c("$15000 - 19999", "$20000 - 24999", "$25000 or more") ~ "3-high"),
      levels = c("1-low", "2-mid", "3-high"), ordered = TRUE))
}

# test -------------------------------------------------------------------------
skip_if_not_installed("broom")
skip_if_not_installed("marginaleffects")
d   <- reg_data()
col <- tab_reg(d, "tvhours", c("age", "race"), family = "gaussian", effect = "ame",
                 cleannames = FALSE)[["Model AME"]]
expect_identical(get_type(col), "coef")
expect_identical(get_ci_type(col), "diff")
dm <- d |> dplyr::filter(!is.na(tvhours), !is.na(age), !is.na(race))
dm$race <- forcats::fct_drop(dm$race)
m   <- stats::lm(tvhours ~ age + race, data = dm)
ac  <- rbind(as.data.frame(marginaleffects::avg_comparisons(m, variables = "age",  newdata = dm)),
               as.data.frame(marginaleffects::avg_comparisons(m, variables = "race", newdata = dm)))
keep <- !is.na(get_diff(col)) & !is_refrow(col)
