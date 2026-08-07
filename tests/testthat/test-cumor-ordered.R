# Last Phase z10, Steps 1-2: `ordered` factors survive the whole pipeline, and `OR = "cumOR"` prints
# the per-cut-point cumulative odds ratio of an ordered col_var (the descriptive analogue of a
# proportional-odds model's cumulative OR -- one number per cut, no PO assumption).

# A controlled fixture with exact cell counts, so every cumulative 2x2 can be hand-built.
ord_data <- function() {
  n <- c(a = 40, b = 30, c = 20, d = 10,      # group "ref"
         a = 10, b = 20, c = 30, d = 40)      # group "hi"
  data.frame(
    g = factor(rep(c("ref", "hi"), each = 100), levels = c("ref", "hi")),
    y = factor(rep(rep(c("a", "b", "c", "d"), 2), times = n),
               levels = c("a", "b", "c", "d"), ordered = TRUE)
  )
}

woolf_or <- function(a, b, cc, dd) (a * dd) / (b * cc)


# --- Step 1: the ordered class survives -------------------------------------------------------

test_that("an ordered tab_var no longer breaks the totals machinery, and keeps its class", {
  d <- ord_data()
  d$tv <- d$y                                   # an ORDERED tab_var: the case that used to abort

  t <- tab(d, g, y, tab_vars = tv)              # used to error in leaf_rename_totals()
  expect_s3_class(t, "tabxplor_tab")
  expect_true(is.ordered(t$tv))
  expect_true("Ensemble" %in% levels(t$tv))

  d$num <- as.numeric(d$y)
  tn <- tab(d, g, num, tab_vars = tv)           # used to error in num_rollup()'s vec_rbind
  expect_s3_class(tn, "tabxplor_tab")
  expect_true(is.ordered(tn$tv))
})

test_that("un-blocking `ordered` leaves an unordered table byte-identical", {
  d <- ord_data()
  d$tv <- d$y
  plain <- d
  plain$y  <- factor(plain$y,  levels = levels(plain$y),  ordered = FALSE)
  plain$tv <- factor(plain$tv, levels = levels(plain$tv), ordered = FALSE)

  a <- tab(d,     g, y, tab_vars = tv, pct = "row")
  b <- tab(plain, g, y, tab_vars = tv, pct = "row")
  # same values and same level ORDER; only the class of the grouping column differs
  expect_identical(levels(a$tv), levels(b$tv))
  expect_identical(lapply(a[-(1:2)], unclass), lapply(b[-(1:2)], unclass))
  expect_true(is.ordered(a$tv))
  expect_false(is.ordered(b$tv))
})


test_that("stacking an ordered row_var with a plain one drops the incomparable order", {
  d <- ord_data()
  d$g2 <- d$g
  # the merged `levels` column holds DIFFERENT variables' levels, so an order across them would be a
  # claim that does not exist -- and vctrs refuses to combine an ordered factor with a plain one.
  t <- tab(d, c(g2, y), g, pct = "row", na = "drop")
  expect_s3_class(t, "tabxplor_tab")
  expect_false(is.ordered(t$levels))
  expect_true(all(c(levels(d$g2), levels(d$y)) %in% levels(t$levels)))
  # two ordered row_vars have different level sets -- also incomparable
  d$y2 <- d$y
  expect_s3_class(tab(d, c(y, y2), g, pct = "row", na = "drop"), "tabxplor_tab")
  # a SINGLE ordered row_var keeps its class
  expect_true(is.ordered(tab(d, y, g, pct = "row", na = "drop")[[1]]))
})


# --- Step 2: OR = "cumOR" ----------------------------------------------------------------------

test_that("cumOR is the per-cut Woolf odds ratio of the cumulated counts", {
  t <- tab(ord_data(), g, y, pct = "row", OR = "cumOR", na = "drop")
  lv <- c("a", "b", "c", "d")

  cum_ref <- cumsum(c(40, 30, 20, 10))
  cum_hi  <- cumsum(c(10, 20, 30, 40))
  hand    <- vapply(seq_len(3), function(j)                       # k-1 = 3 real cut points
    woolf_or(cum_hi[j], 100 - cum_hi[j], cum_ref[j], 100 - cum_ref[j]), numeric(1))

  got <- vapply(lv[1:3], function(l) get_or(t[[l]])[t$g == "hi"], numeric(1))
  expect_equal(unname(got), hand, tolerance = 1e-10)
  expect_true(all(hand < 1))                                      # "hi" really is shifted upward

  # the reference row is 1 on every real cut
  expect_equal(unname(vapply(lv[1:3], function(l) get_or(t[[l]])[t$g == "ref"], numeric(1))),
               rep(1, 3))
})

test_that("the last cut is degenerate, so its column is empty and carries no reference '1'", {
  t <- tab(ord_data(), g, y, pct = "row", OR = "cumOR", na = "drop")
  expect_true(all(is.na(get_or(t[["d"]]))))
  # and it must not print the raw "NA" beside the reference percentage
  expect_false(any(grepl("NA", format(t[["d"]]), fixed = TRUE)))
})

test_that("cumOR has no reference COLUMN (every column is its own cut point)", {
  t <- tab(ord_data(), g, y, pct = "row", OR = "cumOR", na = "drop")
  expect_false(any(vapply(c("a", "b", "c", "d"), function(l) any(is_refcol(t[[l]])), logical(1))))
})

test_that("cumOR carries a Woolf interval and stars when a policy asks for one", {
  t <- tab(ord_data(), g, y, pct = "row", OR = "cumOR", na = "drop", stars = TRUE)
  cum_ref <- cumsum(c(40, 30, 20, 10)); cum_hi <- cumsum(c(10, 20, 30, 40))
  j  <- 1
  ex <- ci_or(cum_hi[j], 100 - cum_hi[j], cum_ref[j], 100 - cum_ref[j], want_p = TRUE)
  hi <- t$g == "hi"
  expect_equal(get_ci_inf(t[["a"]])[hi], ex$inf, tolerance = 1e-10)
  expect_equal(get_ci_sup(t[["a"]])[hi], ex$sup, tolerance = 1e-10)
  expect_equal(get_pvalue(t[["a"]])[hi], ex$pvalue, tolerance = 1e-10)
  expect_identical(as.character(get_ci_type(t[["a"]])), "or")
})

test_that("the `na = 'keep'` column never becomes a cut point", {
  d <- ord_data()
  d$y[1:10] <- NA
  t <- tab(d, g, y, pct = "row", OR = "cumOR")            # na = "keep" -> an "NA" column
  expect_true("NA" %in% names(t))
  expect_true(all(is.na(get_or(t[["NA"]]))))
  # the last REAL level is then the degenerate cut, not the NA column
  expect_true(all(is.na(get_or(t[["d"]]))))
  expect_false(all(is.na(get_or(t[["c"]]))))
})

test_that("an ineligible col_var degrades to no OR, with one message naming the fix", {
  d <- ord_data()
  d$nominal <- factor(rep(c("p", "q", "r"), length.out = nrow(d)))

  expect_message(t <- tab(d, g, c(y, nominal), pct = "row", OR = "cumOR", na = "drop"),
                 "ordered")
  expect_false(all(is.na(get_or(t[["a"]]))))               # the ordered col_var still gets cumOR
  expect_true(is.na(get_or(t[["p"]])[[1]]))                # the nominal one falls back to plain %
  expect_identical(as.character(get_display(t[["p"]]))[1], "pct")
})

test_that("cumOR needs row percentages, and says so instead of computing nonsense", {
  expect_message(t <- tab(ord_data(), g, y, pct = "col", OR = "cumOR", na = "drop"),
                 "pct")
  expect_true(all(is.na(get_or(t[["a"]]))))
})


# --- the recycle bug the per-pair OR resolution deleted ----------------------------------------

test_that("color = 'auto' resolves to the OR measure with several factor col_vars", {
  d <- ord_data()
  d$y2 <- factor(rep(c("no", "yes"), length.out = nrow(d)), levels = c("no", "yes"))
  # `auto_or` used to index the per-row_var SCALAR OR with a logical over col_vars, so with >= 2
  # factor col_vars it read c("OR", NA) -> FALSE -> the table silently coloured on the difference.
  t <- tab(d, g, c(y2, y), pct = "row", OR = "OR", color = TRUE, ref2 = 1, na = "drop")
  expect_identical(as.character(get_color(t[["yes"]])), "OR")
})
