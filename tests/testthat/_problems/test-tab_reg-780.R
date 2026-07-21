# Extracted from test-tab_reg.R:780

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
t1  <- tab_reg(d, "married", c("race", "age"), family = "binomial", effect = "ame",
                 at = "reference", cleannames = FALSE)
col <- t1[["Model MER"]]
expect_identical(get_type(col), "row")
dm <- d |> dplyr::filter(!is.na(married), !is.na(race), !is.na(age))
dm$race    <- forcats::fct_drop(dm$race)
dm$married <- forcats::fct_rev(forcats::fct_drop(factor(dm$married)))
g    <- stats::glm(married ~ race + age, data = dm, family = stats::binomial())
grid <- marginaleffects::datagrid(model = g, race = levels(dm$race)[1], age = mean(dm$age))
acr  <- as.data.frame(marginaleffects::comparisons(g, variables = "race", newdata = grid))
aca  <- as.data.frame(marginaleffects::comparisons(g, variables = "age",  newdata = grid))
pg   <- marginaleffects::datagrid(model = g, race = levels(dm$race), age = mean(dm$age))
ap   <- as.data.frame(marginaleffects::predictions(g, newdata = pg))
keep <- !is.na(get_diff(col))
