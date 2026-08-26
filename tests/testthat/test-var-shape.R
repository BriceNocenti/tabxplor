# PURPOSE: Phase 22c-iii -- the shape subsystem, shared by both producers.
# ROLE: locks what `shape` IS (one declared vocabulary, one cutter, one label rule), the automatic
#       answer a numeric row/tab variable gets, and the two derived display tokens the numeric
#       column's default layout rests on.
# See: CLAUDE.md > Phase 22c-iii; R/var-shape.R.

gsh <- suppressWarnings(fx_gss_fmt())
gsh$age     <- fx_gss()$age
gsh$tvhours <- fx_gss()$tvhours
quiet <- function(e) suppressMessages(e)

# === the declared vocabulary =====================================================================

testthat::test_that("the two producers read ONE table, and a refusal is derived from it", {
  # every shape is offered to whoever declares it; nothing is listed twice
  quant <- c("median", "terciles", "quartiles", "quintiles", "deciles")
  testthat::expect_setequal(tabxplor:::shape_vocab("tab"),
                            c("linear", "log", "sqrt", "sd_bands", quant, "values_to_levels"))
  testthat::expect_setequal(tabxplor:::shape_vocab("tab_reg"),
                            c("linear", quant, "sd_bands", "log", "sqrt", "quadratic"))
  # ...and a named quantile cut and its integer twin cannot mean two different things: `k` is read
  # off the row, once (Phase 22g-iii added the three the jamovi drop-down needed to be able to name).
  testthat::expect_identical(vapply(quant, tabxplor:::shape_k, integer(1)),
                             c(median = 2L, terciles = 3L, quartiles = 4L, quintiles = 5L,
                               deciles = 10L))
  # the refusal names the producer that DOES have it, and the cure -- read off the row, not written
  testthat::expect_error(shape_numeric_var(1:10, "quadratic"), "tab_reg")
  testthat::expect_error(shape_numeric_var(1:10, "quadratic"), "quintiles")
  testthat::expect_error(quiet(tab(gsh, race, age, shape = c(age = "quadratic"))), "tab_reg")
  # `values_to_levels` is tab()'s alone: a model fits a slope or a set of contrasts, not one per value
  testthat::expect_error(tab_reg(gsh, "married", "age", family = "binomial",
                                 shape = c(age = "values_to_levels")), "tab_reg|one of")
  # a shape naming a variable that is already a factor says so, in the caller's own noun
  testthat::expect_error(quiet(tab(gsh, race, age, shape = c(race = "quintiles"))),
                         "numeric variables")
  testthat::expect_error(tab_reg(gsh, "married", c("race", "age"), family = "binomial",
                                 shape = c(race = "quintiles")), "continuous predictors")
})

# === the cutters =================================================================================

testthat::test_that("quantile groups are balanced, weighted when asked, and frozen", {
  q <- shape_numeric_var(gsh$age, "quartiles")
  testthat::expect_length(levels(q), 4L)
  testthat::expect_true(is.ordered(q))                       # tab()'s answer
  testthat::expect_false(is.ordered(shape_numeric_var(gsh$age, "quartiles", ordered = FALSE)))
  # balanced: no group is more than ~25 % off an equal share
  sh <- as.numeric(table(q)) / sum(!is.na(q))
  testthat::expect_true(all(abs(sh - 0.25) < 0.06))
  # a WEIGHTED cut is equal shares of the POPULATION, so it moves when the weights do
  w  <- ifelse(gsh$age > 50, 5, 1)
  qw <- shape_numeric_var(gsh$age, "quartiles", w = w)
  testthat::expect_false(identical(levels(q), levels(qw)))
  # the breaks are FROZEN into the spec, so a replay re-cuts at the same places
  spec <- tabxplor:::shape_value("quartiles", "age", "tab")
  a1 <- tabxplor:::shape_apply(data.frame(age = gsh$age), list(age = spec), ordered = TRUE)
  a2 <- tabxplor:::shape_apply(data.frame(age = gsh$age[1:500]), a1$shapes, ordered = TRUE)
  testthat::expect_identical(levels(a2$data$age), levels(a1$data$age))
})

testthat::test_that("sd_bands sit at the mean +/- 1 SD, in words, and degrade rather than abort", {
  sg <- "\u03c3"
  b <- shape_numeric_var(gsh$age, "sd_bands")
  testthat::expect_length(levels(b), 4L)
  # the band says its OWN cut, so the label can be checked against the interval beside it
  testthat::expect_match(levels(b)[[1]], paste0("; < -1", sg, "$"))
  testthat::expect_match(levels(b)[[2]], "; below mean$")
  testthat::expect_match(levels(b)[[3]], "; above mean$")
  testthat::expect_match(levels(b)[[4]], paste0("; > \\+1", sg, "$"))
  # every label carries the real cut points too -- and `age` is whole-numbered, so it names its
  # VALUES ("18 to 29") rather than the interval they sit in (Phase 22g-v)
  testthat::expect_true(all(grepl("^[0-9]+( (to|or) [0-9]+)? ;", levels(b))))
  testthat::expect_true(all(grepl("^\\[", levels(shape_numeric_var(gsh$age + 0.5, "sd_bands")))))
  # ⚠ a skewed variable loses a landmark rather than asking cut() for an empty band
  set.seed(1)
  testthat::expect_length(levels(shape_numeric_var(stats::rexp(5000, 1 / 3e4), "sd_bands")), 3L)
  # ... and one that does not vary at all is refused, naming the balanced cure
  testthat::expect_error(shape_numeric_var(rep(1, 100), "sd_bands"), "to vary")
})

testthat::test_that("a whole-numbered variable is cut at whole numbers -- the SAME cut, read", {
  b  <- shape_numeric_var(gsh$age, "sd_bands")
  br <- attr(tabxplor:::shape_cut_bands(gsh$age), "tabxplor_breaks")
  testthat::expect_identical(br, round(br))                  # no 29.89 in a label
  # exact, not approximate: with `right = FALSE` a break and its ceiling admit the same integers
  m <- mean(gsh$age, na.rm = TRUE); s <- stats::sd(gsh$age, na.rm = TRUE)
  raw <- cut(gsh$age, c(min(gsh$age, na.rm = TRUE), m - s, m, m + s, max(gsh$age, na.rm = TRUE)),
             include.lowest = TRUE, right = FALSE)
  testthat::expect_identical(as.integer(table(b)), as.integer(table(raw)))
})

testthat::test_that("the variable's name is written on the FIRST level only", {
  b <- shape_numeric_var(gsh$age, "sd_bands", name = "age")
  testthat::expect_match(levels(b)[[1]], "^age: ")
  testthat::expect_false(any(grepl("age:", levels(b)[-1], fixed = TRUE)))
  testthat::expect_false(any(grepl("age:", levels(shape_numeric_var(gsh$age, "sd_bands")),
                                   fixed = TRUE)))
  # ... but never on `levels`: a raw value names itself, and prefixing it broke the tab_counts()
  # parity a numeric key has always had (measured).
  d <- dplyr::mutate(fx_gss(), yr = as.integer(year))
  a <- quiet(tab(d, yr, race, pct = "row"))
  b <- quiet(tab_counts(dplyr::count(d, yr, race, name = "n"), yr, race, counts = n, pct = "row"))
  testthat::expect_equal(a, b)
  testthat::expect_false(any(grepl("yr", levels(a$yr), fixed = TRUE)))
})

testthat::test_that("shape_numeric_var() builds exactly the column tab(shape =) builds", {
  t <- quiet(tab(gsh, age, party3, pct = "row", na = "drop", shape = "quintiles"))
  testthat::expect_identical(
    levels(shape_numeric_var(gsh$age[!is.na(gsh$age) & !is.na(gsh$party3)], "quintiles")),
    setdiff(levels(t$age), "Total"))
})

# === `auto`: what a NUMBER on the row axis becomes ================================================

testthat::test_that("`auto` keeps one row per value for a short scale, bands a continuous one", {
  d <- gsh
  d$nkids <- as.integer(pmin(5, abs(round(stats::rnorm(nrow(d), 2, 1.5)))))
  testthat::expect_identical(tabxplor:::shape_auto(d$nkids), "values_to_levels")
  testthat::expect_identical(tabxplor:::shape_auto(d$age),   "sd_bands")
  # ... and it SAYS which it chose, only where the user did not
  testthat::expect_message(tab(d, age, party3, pct = "row", na = "drop"), "four bands")
  testthat::expect_message(tab(d, nkids, party3, pct = "row", na = "drop"), "one row per value")
  testthat::expect_silent(tab(d, age, party3, pct = "row", na = "drop", shape = "quintiles"))
  # a numeric COLUMN variable keeps its means -- there a number already has a reading
  testthat::expect_silent(tab(d, race, age, na = "drop"))
  testthat::expect_identical(fmt_var_kind(tab(d, race, age, na = "drop")[[2]]), "mean")
})

testthat::test_that("a shaped row variable is an ordinary factor from the prepare stage on", {
  t <- quiet(tab(gsh, age, party3, pct = "row", na = "drop", shape = "quartiles"))
  testthat::expect_s3_class(t$age, "factor")
  testthat::expect_true(is.ordered(t$age))                   # a band has a real order
  testthat::expect_true("Total" %in% levels(t$age))
  # `levels` reproduces what a numeric row variable has always become: a PLAIN factor, so a table
  # built this way stays identical to one built before `shape` existed (and to tab_counts()').
  d <- gsh; d$yr <- as.integer(fx_gss()$year)
  testthat::expect_false(is.ordered(quiet(tab(d, yr, party3, pct = "row", na = "drop"))$yr))
  # the cut runs ONCE, on the whole population: every sub-table shares the breaks, so there are
  # exactly four cut labels however many sub-tables there are
  tv <- quiet(tab(gsh, age, party3, tab_vars = race, pct = "row", na = "drop",
                  shape = "quartiles"))
  testthat::expect_length(grep("^Total", levels(tv$age), invert = TRUE, value = TRUE), 4L)
})

testthat::test_that("a numeric col_var may be cut, and the spine knows before the data does", {
  t <- quiet(tab(gsh, race, age, pct = "row", na = "drop", shape = c(age = "quartiles")))
  # cut -> percentage columns, not a mean column
  testthat::expect_gt(sum(purrr::map_lgl(t, is_fmt)), 2L)
  testthat::expect_identical(fmt_var_kind(t[[2]]), "pct")
})

testthat::test_that("a transform renames its column, and is refused on the row axis", {
  t <- quiet(tab(gsh, race, age, na = "drop", shape = c(age = "log")))
  testthat::expect_true("log_age" %in% names(t))              # the mean of a log is not a log of a mean
  testthat::expect_false("age" %in% names(t))
  testthat::expect_equal(get_mean(t$log_age)[[1]], mean(log(gsh$age[gsh$race == "White"]),
                                                        na.rm = TRUE), tolerance = 1e-8)
  testthat::expect_error(quiet(tab(gsh, age, party3, shape = c(age = "log"))), "has no rows")
  testthat::expect_error(shape_numeric_var(c(-1, 2, 3), "log"), "strictly positive")
  testthat::expect_error(shape_numeric_var(c(-1, 2, 3), "sqrt"), "negative")
})

# === the two derived display tokens ==============================================================

testthat::test_that("{sd} and {cv} are DERIVED from `var` (and `mean`), and never stored", {
  x <- fmt(mean = c(4, 10), var = c(1, 25), n = c(9L, 9L), scale = "level_mean", digits = 1L)
  testthat::expect_identical(get_num(set_display(x, "{sd}")), c(1, 5))
  testthat::expect_identical(get_num(set_display(x, "{cv}")), c(0.25, 0.5))
  # both are read-only: there is nothing to write a derived number back into
  testthat::expect_false("sd" %in% tabxplor:::DISPLAY_SETTABLE)
  testthat::expect_false("cv" %in% tabxplor:::DISPLAY_SETTABLE)
  # a cv against a level at or below zero is not a share of anything -> declared void
  y <- fmt(mean = c(-4, 0), var = c(1, 1), n = c(9L, 9L), scale = "level_mean", digits = 1L)
  testthat::expect_true(all(is.na(get_num(set_display(y, "{cv}")))))
  # it prints as a percentage with no decimals, wherever it sits
  testthat::expect_identical(format(set_display(x, "{cv}")), c("25%", "50%"))
})

testthat::test_that("the numeric default is mean_cv, GUARDED per column", {
  t <- quiet(tab(gsh, race, c(age, tvhours), na = "drop"))
  testthat::expect_identical(unique(get_display(t$age)), DISPLAY_PRESETS$mean_cv$template)
  testthat::expect_match(format(t$age)[[1]], "cv")
  # a column that holds a non-positive mean falls back to the bare mean -- ONE layout per column,
  # never a wild figure in some rows and a sensible one in others
  d <- gsh; d$gap <- fx_gss()$age - 47
  t2 <- quiet(tab(d, race, c(age, gap), na = "drop"))
  testthat::expect_identical(unique(get_display(t2$gap)), "mean")
  testthat::expect_identical(unique(get_display(t2$age)), DISPLAY_PRESETS$mean_cv$template)
})

testthat::test_that("a self-naming aside is named once: in the cell, not again in the header", {
  hdr <- function(t) {
    cvh <- tabxplor:::tab_export_prep(t, backend = "kable", wrap = NULL)$tables[[1]]$col_var_header
    c(clean = cvh$clean[cvh$label == "age"], unit = cvh$unit[cvh$label == "age"])
  }
  testthat::expect_identical(unname(hdr(quiet(tab(gsh, race, age, na = "drop")))["clean"]), "mean")
  testthat::expect_identical(unname(hdr(quiet(tab(gsh, race, age, na = "drop",
                                                  display = "mean_sd")))["clean"]), "mean (sd)")
})

# --- Phase 22g-v: a quantile cut gives k groups, and a whole number names its values -------------

testthat::test_that("a tied variable still gets k quantile groups, and says so when it cannot", {
  # `checks`-like: 7 distinct values, very uneven. Two quantiles landing on one value used to drop a
  # break silently, so `quartiles` gave 3 groups where `quintiles` gave 4 -- on the same column.
  x <- c(rep(0, 1800), rep(1, 1600), rep(2, 950), rep(3, 500), rep(4, 250), rep(5, 90), rep(6, 36))
  testthat::expect_length(levels(shape_numeric_var(x, "quartiles")), 4L)
  testthat::expect_length(levels(shape_numeric_var(x, "quintiles")), 5L)
  # ...and a genuine shortfall is a fact about the data, so it is stated -- once, naming both counts
  testthat::expect_message(shape_numeric_var(x, "deciles"), "rather than 10")
  testthat::expect_no_message(shape_numeric_var(x, "quartiles"))
})

testthat::test_that("a whole-numbered cut names its VALUES, a fractional one its interval", {
  x <- c(rep(0, 100), rep(1, 60), rep(2, 40), rep(3, 20), 4:6)
  lv <- levels(shape_numeric_var(x, "quartiles"))
  testthat::expect_true(all(grepl("^([0-9]+|[0-9]+ (or|to) [0-9]+)$", lv)))
  testthat::expect_identical(lv[[1]], "0")              # [0,1) holds exactly the value 0
  # NO rank tag: the bounds already say where the group sits, read as values or as an interval
  testthat::expect_false(any(grepl("Q[0-9]", c(lv, levels(shape_numeric_var(x + 0.5, "quartiles"))))))
  # the same column shifted off the integers keeps cut()'s own interval literal
  testthat::expect_true(all(grepl("^\\[", levels(shape_numeric_var(x + 0.5, "quartiles")))))
  # a cut is still a cut: the groups hold the same rows either way
  testthat::expect_identical(as.integer(shape_numeric_var(x, "quartiles")),
                             as.integer(shape_numeric_var(x + 0.5, "quartiles")))
})
