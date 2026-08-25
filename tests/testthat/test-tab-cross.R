# Phase 22h -- `a*b` in `tab(col_vars =)` (R/tab-cross.R), the crosstab half of the interaction
# subsystem. What a cross IS lives in R/reg-cross.R and is tested in test-reg-cross.R; here is what
# each arm MEANS on a column axis, and the pipeline facts that were easy to get wrong.

quiet <- function(x) suppressMessages(x)

cx_data <- function() {
  d <- forcats::gss_cat
  d$relig3 <- forcats::fct_lump_n(d$relig, 2)
  d
}

test_that("both parents categorical: one column per observed CELL, both parents absorbed", {
  d <- cx_data()
  t <- quiet(tab(d, marital, race*relig3, pct = "row"))
  cells <- levels(tabxplor:::reg_cross_column(d$race, d$relig3))
  expect_true(all(cells %in% names(t)))
  # ... and neither parent has a block of its own
  expect_false(any(c("race", "relig3") %in% names(t)))
  # the counts are the pair's own
  n <- get_n(t[[cells[[2]]]])[t$marital == "Married"]
  expect_equal(n, sum(d$race == "Black" & d$relig3 == "Catholic" & d$marital == "Married"))
})

test_that("a numeric crossed with a factor: one MEAN column per level, beside the factor's block", {
  d <- cx_data()
  t <- quiet(tab(d, marital, age*race, pct = "row"))
  # the moderator keeps its own block (REG_CROSS_ARMS$nested$keeps), then the means
  cv <- purrr::map_chr(t, ~ if (is_fmt(.)) get_col_var(.) else NA_character_)
  expect_identical(unique(cv[!is.na(cv)]), c("race", "age*race"))
  # and each mean is the mean of that group -- the same table `spread_vars` builds
  s <- quiet(tab(d, marital, age, spread_vars = race))
  m <- get_mean(t[[paste0("White_age*race")]])
  expect_equal(m[t$marital == "Married"],
               get_mean(s$age_White)[s$marital == "Married"])
  # its base count is that group's own, NAs excluded from both mean and n
  expect_equal(get_n(t[["White_age*race"]])[t$marital == "Married"],
               sum(d$race == "White" & d$marital == "Married" & !is.na(d$age)))
})

test_that("the block reads as ONE block, named by the key, its columns by the level", {
  d <- cx_data()
  t <- quiet(tab(d, marital, age*race, pct = "row"))
  p <- tab_export_prep(t, backend = "kable")$tables[[1]]
  j <- which(p$col_var_header$label == "age*race")
  expect_length(unique(p$roles$col_blocks[j]), 1L)              # one block
  lv <- levels(forcats::fct_drop(d$race))
  expect_identical(p$col_var_header$clean[j], lv)               # headed by the LEVEL
  expect_equal(sum(nzchar(p$col_var_header$unit[j])), 1L)       # the unit written once
  # a cells block too
  t2 <- quiet(tab(d, marital, race*relig3, pct = "row"))
  p2 <- tab_export_prep(t2, backend = "kable")$tables[[1]]
  j2 <- which(p2$col_var_header$label == "race*relig3")
  expect_length(unique(p2$roles$col_blocks[j2]), 1L)
})

test_that("two continuous parents: the moderator is cut, never silently", {
  d <- cx_data()
  expect_message(t <- tab(d, marital, age*tvhours, pct = "row"), "cut")
  expect_equal(sum(purrr::map_lgl(t, ~ is_fmt(.) && identical(get_col_var(.), "age*tvhours"))), 4L)
})

test_that("a cross is refused on every axis but col_vars, and by tab_counts()", {
  d <- cx_data()
  expect_error(tab(d, race*relig3, marital),                     "row_vars")
  expect_error(tab(d, marital, race, tab_vars = race*relig3),    "tab_vars")
  expect_error(tab(d, marital, age, spread_vars = race*relig3),  "spread_vars")
  expect_error(tab(d, marital, race:relig3),                     "col_vars")
  expect_error(tab(d, marital, race*race),                       "with itself")
  expect_error(tab(d, marital, c(race, race*relig3)),            "beside an interaction")
  expect_error(tab(d, marital, age*race, shape = c(age = "log")), "cannot keep a number")
  cnt <- dplyr::count(d, race, relig3, name = "n")
  expect_error(tab_counts(cnt, race, race*relig3, counts = n), "takes no interaction")
})

test_that("the na policy acts on the PAIR, never on the columns it became", {
  d <- cx_data()
  # ⚠ every row is NA in all but one nested column by construction: `drop_all` applied to them
  # would empty the table. It is applied to the placeholder -- one variable.
  t <- quiet(tab(d, marital, age*race, pct = "row", na = "drop_all"))
  expect_gt(nrow(t), 1L)
  expect_gt(sum(get_n(t[["White_age*race"]]), na.rm = TRUE), 0)
})

test_that("a shaped parent is cut BEFORE the cells are combined", {
  d <- cx_data()
  t <- quiet(tab(d, marital, age*race, pct = "row", shape = c(age = "quartiles")))
  # a cut modified parent makes it a `cells` cross: the columns are combinations of GROUPS
  cells <- names(t)[purrr::map_lgl(t, ~ is_fmt(.) && identical(get_col_var(.), "age*race"))]
  cells <- setdiff(cells, c("NA", "Total"))
  expect_true(all(grepl(tabxplor:::reg_cross_sep(), cells, fixed = TRUE)))
  # a GROUP on each side, never a raw age -- the cut ran before the pair was combined
  expect_true(all(grepl("^[0-9]+ to [0-9]+ ", cells)))
  # ... and a cut modified parent makes it a `cells` cross, so race has no block of its own
  expect_false("Other" %in% names(t))
})

# Phase 22h: found while building the above. `as.character()` on a LIST of symbols deparses, so a
# non-syntactic name came back backticked and every later selection missed it.
test_that("tab() takes a column whose name is not syntactic", {
  d <- forcats::gss_cat
  d[["my race"]] <- d$race
  d[["my age"]]  <- d$age
  expect_no_error(quiet(tab(d, marital, tidyselect::all_of("my race"), pct = "row")))
  expect_no_error(quiet(tab(d, tidyselect::all_of("my race"), marital, pct = "row")))
  expect_no_error(quiet(tab(d, marital, tidyselect::all_of("my age"))))
  expect_no_error(quiet(tab(d, marital, race, tab_vars = tidyselect::all_of("my race"))))
  # ... and it is the same table the syntactic twin builds
  a <- quiet(tab(d, marital, tidyselect::all_of("my race"), pct = "row"))
  b <- quiet(tab(d, marital, race, pct = "row"))
  expect_equal(get_pct(a[[2]]), get_pct(b[[2]]))
})
