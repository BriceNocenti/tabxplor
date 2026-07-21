# Extracted from test-tab_reg-empirical.R:252

# prequel ----------------------------------------------------------------------
emp_data <- function() {
  d <- forcats::gss_cat
  d$race    <- forcats::fct_drop(d$race)
  d$married <- factor(as.integer(d$marital == "Married"), labels = c("no", "yes"))
  d$party3  <- factor(dplyr::case_when(grepl("dem", d$partyid, ignore.case = TRUE) ~ "Dem",
                                       grepl("rep", d$partyid, ignore.case = TRUE) ~ "Rep",
                                       TRUE ~ "Ind"),
                      levels = c("Ind", "Dem", "Rep"))
  d$inc3    <- factor(dplyr::case_when(d$rincome %in% c("$25000 or more") ~ "hi",
                                       d$rincome %in% c("Not applicable", "No answer",
                                                        "Don't know", "Refused") ~ NA_character_,
                                       TRUE ~ "lo"),
                      levels = c("lo", "hi"), ordered = FALSE)
  d$spectrum <- factor(d$party3, levels = c("Ind", "Dem", "Rep"), ordered = TRUE)
  d <- d[!is.na(d$tvhours) & !is.na(d$race) & !is.na(d$party3), , drop = FALSE]
  tibble::as_tibble(d)
}
emp_positive_level <- function(t, d, levcol) {
  r1 <- levels(d$race)[1]
  e1 <- unname(get_pct(t[["Obs_%"]])[match(r1, as.character(t$levels))])   # P(positive | race == r1)
  p_first <- mean(d[[levcol]][d$race == r1] == levels(d[[levcol]])[1])
  if (isTRUE(all.equal(e1, p_first, tolerance = 1e-6))) levels(d[[levcol]])[1]
  else                                                  levels(d[[levcol]])[2]
}
emp_2lvl <- function() {
  d <- emp_data()
  d <- d[d$race %in% c("Black", "White"), , drop = FALSE]
  d$race <- forcats::fct_drop(d$race)
  d
}

# test -------------------------------------------------------------------------
d  <- emp_data()
d$black <- factor(as.integer(d$race == "Black"), labels = c("no", "yes"))
expect_no_message(
    t <- tab_reg(d, c("married", "black"), "inc3", family = "binomial", empirical = TRUE,
                 cleannames = FALSE),
    message = "not available")
expect_true(any(grepl("Obs_% \\(married\\)", names(t))))
