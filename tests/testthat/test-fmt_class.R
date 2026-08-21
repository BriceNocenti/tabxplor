
# Phase 20a: this file calls functions deprecated in 2.0.0 on purpose -- what it asserts is their
# arithmetic, which the leaf shares with them and which does NOT go away in 2.1.0.
# ⚠ This quiets the TOP-LEVEL calls only: testthat 3e runs local_reproducible_output() inside
# every test_that(), which forces lifecycle_verbosity = "warning" again, so the in-block calls
# still warn. Migrating them to tab() is the corpus sweep routed to Phase 20h.
withr::local_options(lifecycle_verbosity = "quiet", .local_envir = testthat::teardown_env())


testthat::test_that("class is right", {
  testthat::expect_s3_class(fmt(1), "tabxplor_fmt")
})

testthat::test_that("fmt prints without error", {
  testthat::expect_output(
    print(fmt(n = c(5, 10, 15), scale = "level_n", display = c("n", "row", "mean"),
              wn = c(4.7, 12.1, 13.9), digits = 1, pct = c(NA, 0.63, NA),
              mean = c(NA, NA, 27.3)))
  )
  testthat::expect_output(print(tibble::tibble(
    fmt(n = c(15, 10, 5), scale = "level_pct", pct_type = "row", display = c("n", "row", "mean"),
        wn = c(13.9, 12.1, 4.7), digits = 0, pct = c(NA, 0.22, NA),
        mean = c(NA, NA, 21))
  )))
})

#test of common type :
# vec_ptype_show(fmt(1, "level_pct", pct_type = "row", pct = 0.255), fmt(2, "level_pct", pct_type = "row", pct = 0.987))
# vec_ptype_show(fmt(), double(), fmt())
# vec_ptype_common(fmt(1, "level_pct", pct_type = "row", pct = 0.255), fmt(2, "level_pct", pct_type = "row", pct = 0.987))
# vec_ptype2(fmt(1, "level_pct", pct_type = "row", pct = 0.255), fmt(2, "level_pct", pct_type = "row", pct = 0.987))
# vec_ptype2(fmt(1, "level_pct", pct_type = "row", pct = 0.255), fmt(2, "level_pct", pct_type = "col", pct = 0.987))

testthat::test_that("class is right after conversion", {
  testthat::expect_s3_class(vec_cast(5, fmt()), "tabxplor_fmt")
  testthat::expect_s3_class(vec_cast(5L, fmt()), "tabxplor_fmt")
  testthat::expect_type(vec_cast(fmt(6), double()), "double")
  testthat::expect_type(vec_cast(fmt(6), integer()), "integer")
  testthat::expect_type(vec_cast(fmt(1, "level_pct", pct_type = "row", pct = 0.6005), character()), "character")
  testthat::expect_s3_class(vec_cast(NA, fmt()), "tabxplor_fmt")
})
# vec_cast(fmt(1, "level_pct", pct_type = "row", pct = 0.255), fmt(2, "level_pct", pct_type = "row", pct = 0.987))

testthat::test_that("combinations with c() work", {
  testthat::expect_s3_class(vec_c(fmt(1, "level_pct", pct_type = "row", pct = 0.255),
                                  fmt(2, "level_pct", pct_type = "row", pct = 0.987)), "tabxplor_fmt")
  testthat::expect_s3_class(c(fmt(1), fmt(2))                , "tabxplor_fmt")
  testthat::expect_s3_class(vec_c(fmt(3), 1)                 , "tabxplor_fmt")
  testthat::expect_s3_class(vec_c(fmt(3), 1L)                , "tabxplor_fmt")
  testthat::expect_s3_class(vec_c(NA, fmt(4))                , "tabxplor_fmt")
})

testthat::test_that("comparisons and sorting work", {
  testthat::expect_true(fmt(1) == 1)
  testthat::expect_s3_class(sort(c(fmt(2), fmt(1))), "tabxplor_fmt")
})

testthat::test_that("model_family column attribute round-trips and reconciles (Phase 15e)", {
  f <- fmt(c(7, 19), "level_pct", pct_type = "row", or = c(1.2, 0.8), model_family = "binomial")
  testthat::expect_identical(get_model_family(f), "binomial")
  testthat::expect_identical(get_model_family(set_model_family(f, "poisson")), "poisson")
  testthat::expect_identical(get_model_family(fmt(1, "level_pct", pct_type = "row", pct = 0.3)), "")   # inert default

  # vec_c of two different families collapses to "" (like col_var -> "several_vars"); same survives
  testthat::expect_identical(
    get_model_family(vec_c(f, set_model_family(f, "gaussian"))), "")
  testthat::expect_identical(
    get_model_family(vec_c(f, fmt(3, "level_pct", pct_type = "row", or = 2, model_family = "binomial"))), "binomial")

  # arithmetic carries x's family; cast copies model_family from `to`
  testthat::expect_identical(get_model_family(f + f), "binomial")
  testthat::expect_identical(get_model_family(vec_cast(2.5, f)), "binomial")

  # data.frame getter -> one value per column
  df <- tibble::tibble(a = f, b = fmt(1, "level_pct", pct_type = "row", pct = 0.3))
  testthat::expect_identical(unname(get_model_family(df)), c("binomial", ""))
})

testthat::test_that("arithmetic between fmt and fmt works", {
  a <- fmt(5, "level_n"  , 0, wn = 5.1)
  b <- fmt(1, "level_n"   , 0, pct  = 0.25000001, wn =  1.5)
  testthat::expect_equal(get_n(a + b), 6)
  testthat::expect_equal(get_wn(a + b), 5.1 + 1.5)

  testthat::expect_warning((fmt(15L, "level_pct", pct_type = "row" , 1, pct =  0.55, wn = 15.1) -
                              fmt(  2L, "level_mean", 0, mean = 0.25000001, wn =  2.5 )))

  a <- fmt(25, "level_pct", pct_type = "row" , 2, pct =  0.55      , wn = 25.1)
  b <- fmt(3 , "level_pct", pct_type = "row" , 3, pct  = 0.25000001, wn =  3.5)
  testthat::expect_equal(get_pct(a - b), 0.55 - 0.25000001)

  a <- fmt(25, "level_pct", pct_type = "row" , 2, pct =  0.55      , wn = 25.1)
  b <- fmt(3 , "level_pct", pct_type = "row" , 3, pct  = 0.25000001, wn =  3.5 )
  testthat::expect_equal(get_pct(a / b), 0.55 / 0.25000001)

  a <- fmt(35, "level_mean" , 3, mean = 3.55, wn = 35.1)
  b <- fmt(4 , "level_mean" , 0, mean = 1.60, wn =  4.5)
  testthat::expect_equal(get_mean(a + b), (3.55 * 35.1 + 1.60 * 4.5)/(35.1 + 4.5))
})

testthat::test_that("arithmetic between fmt and numeric works", {
  (fmt(45, "level_pct", pct_type = "row" , 4, pct =  0.55, wn = 5.1) + 0.7)|> testthat::expect_s3_class("tabxplor_fmt")
  (fmt(55, "level_mean", 3, mean = 2.55, wn = 55.1) - 1) |> testthat::expect_s3_class("tabxplor_fmt")
  (fmt(65, "level_pct", pct_type = "row", 2, pct =  0.55, wn = 65.1) / 2)  |> testthat::expect_s3_class("tabxplor_fmt")
  (fmt(75, "level_n" ,-1, pct =  0.55, wn = 75.1) * 3)   |> testthat::expect_s3_class("tabxplor_fmt")
  (fmt(1) + 1)                                     |> testthat::expect_s3_class("tabxplor_fmt")
  (1 + fmt(1, "level_pct", pct_type = "row", pct = 0.12))                  |> testthat::expect_s3_class("tabxplor_fmt")
  (1 - fmt(1, "level_pct", pct_type = "row", pct = 0.12))                  |> testthat::expect_s3_class("tabxplor_fmt")
  (2 / fmt(3, "level_pct", pct_type = "row", pct = 0.12))                  |> testthat::expect_s3_class("tabxplor_fmt")
  (5 * fmt(1, "level_n", 2)           )                  |> testthat::expect_s3_class("tabxplor_fmt")
  (-fmt(1, "level_pct", pct_type = "row", pct = 0.12)   )                  |> testthat::expect_s3_class("tabxplor_fmt")
})

testthat::test_that("math (sum and mean) between fmt and fmt works", {
  testthat::expect_equal(get_n(sum(fmt(1), fmt(1))), 2)
  testthat::expect_equal(get_n(mean(fmt(1, "level_n", 2), fmt(1, "level_n", 2))), 1)
})

# Phase 20h: the prepared starwars fixture, built ONCE at top level -- where the file-level
# lifecycle line above actually bites (testthat re-enables the warning inside every
# test_that()). It was written verbatim in each block below.
sw_prepared <- dplyr::starwars |>
  tab_prepare("sex", "hair_color", "eye_color", "mass", "gender",
              other_if_less_than = 5)

testthat::test_that("fmt vectors works with mutate", {

  data <- sw_prepared

  tab_num(data, sex, c(height, birth_year), gender, comp = "all") |>
    dplyr::mutate(dplyr::across(
      c(height, birth_year),
      ~ dplyr::mutate(., var = sqrt(var), display = "var", digits = 2L) |> set_color("no"),
      .names = "{.col}_sd"
    )) |>
    dplyr::pull(height_sd) |>
    testthat::expect_s3_class("tabxplor_fmt")

})

testthat::test_that("fmt work with $", { #and [[
  fmt_vect <- fmt(n = c(1, 2), scale = "level_n")
  testthat::expect_equal(fmt_vect$n, c(1, 2))
  testthat::expect_equal(fmt_vect$digits, c(0, 0))
  #testthat::expect_equal(fmt_vect[["n"]], c(1, 2))
  #testthat::expect_equal(fmt_vect[[2, "n"]], 2)

  testthat::expect_true(any(!is.na(fmt_vect$wn))) # gives n when wn not provided
  })

# === SECTION: Phase 10i-A -- display {} grammar composite display ===============
# The composite display is a per-cell `display`-FIELD {} template; see test-display-grammar.R for the
# shared helpers (display_primary / parse_display_template / validate_display_template) + edge cases.

testthat::test_that("composite {} template renders 'primary (secondary)' on value cells", {
  x <- set_display(fmt(n = c(10L, 20L), scale = "level_pct", pct_type = "row", pct = c(0.4, 0.6), display = "pct"),
                   "{pct} ({n})")
  testthat::expect_identical(format(x), c("40% (10)", "60% (20)"))   # byte-identical to Phase 10c
  y <- set_display(x, "{n} ({pct})")
  testthat::expect_identical(format(y), c("10 (40%)", "20 (60%)"))
  z <- set_display(x, "{pct} (n={n})")
  testthat::expect_identical(format(z), c("40% (n=10)", "60% (n=20)"))
})

testthat::test_that("a composite cell resolves to its PRIMARY (get_num / Excel / tibble header)", {
  x0 <- fmt(n = c(10L, 20L), scale = "level_pct", pct_type = "row", pct = c(0.4, 0.6), display = "pct")
  xs <- set_display(x0, "{pct} ({n})")
  # get_num() and the Excel bypass show the primary field -- byte-identical to the plain column.
  testthat::expect_identical(get_num(xs), get_num(x0))
  testthat::expect_identical(format(xs, syntax = "excel"), format(x0, syntax = "excel"))
  # the tibble header NAMES the layout: the primary's type, then each aside in its own brackets
  # (Phase 22c-ii -- an aside was shown in every cell and named nowhere).
  testthat::expect_identical(vctrs::vec_ptype_abbr(x0), "row%")
  testthat::expect_identical(vctrs::vec_ptype_abbr(xs), "row% (n)")
})

testthat::test_that("format() is byte-identical when no cell is a composite", {
  x0 <- fmt(n = c(10L, 20L), scale = "level_pct", pct_type = "row", pct = c(0.4, 0.6), display = "pct")
  testthat::expect_identical(format(x0), c("40%", "60%"))
})

testthat::test_that("tab(display = ) writes the {} template into the display FIELD of value cells", {
  t0 <- tab(forcats::gss_cat, marital, race, pct = "row")
  t1 <- tab(forcats::gss_cat, marital, race, pct = "row", display = "{pct} ({n})")
  t3 <- tab(forcats::gss_cat, marital, race, pct = "row", display = "{pct} (n={n})")
  fcol <- function(t) t[[which(purrr::map_lgl(t, is_fmt))[1]]]
  # value cells carry the {} template; the plain table's field is untouched.
  testthat::expect_true(any(grepl("{", get_display(fcol(t1)), fixed = TRUE)))
  testthat::expect_false(any(grepl("{", get_display(fcol(t0)), fixed = TRUE)))
  testthat::expect_match(format(fcol(t1))[1], "\\([0-9 ]+\\)$")     # "{pct} ({n})" -> "...(n)"
  testthat::expect_match(format(fcol(t3))[1], "\\(n=[0-9 ]+\\)$")   # "{pct} (n={n})" -> "...(n=..)"
  testthat::expect_identical(get_num(fcol(t1)), get_num(fcol(t0)))  # primary == the plain numbers
  # No curated sugar any more: the old recipe strings error, {} is required.
  testthat::expect_error(tab(forcats::gss_cat, marital, race, display = "pct (n)"))
  testthat::expect_error(tab(forcats::gss_cat, marital, race, display = "wibble"))
})






# x <- fmt(n = c(2, 1), scale = "level_pct", pct_type = "row", pct = c(0.5, 1.5)) #wn = c(0.7, 2.4)
# y <- fmt(n = c(3, 9), scale = "level_n"  , pct = c(0.5, 1.5)) #wn = c(0.7, 2.4)
# z <- c(x, y)
#
# x ; y ; z
# sum(x)
# sum(y)
# sum(z)
#
# sum(x) |> vec_data()
# sum(y) |> vec_data()
# sum(z) |> vec_data()
#
# (get_pct(x)[1]*get_n(x)[1] + get_pct(x)[2]*get_n(x)[2]) / (get_n(x)[1] + get_n(x)[2])
# get_n(y)[1] + get_n(y)[2]
# (get_pct(z)[1]*get_n(z)[1] + get_pct(z)[2]*get_n(z)[2] + get_pct(z)[3]*get_n(z)[3] + get_pct(z)[4]*get_n(z)[4]) / (get_n(z)[1] + get_n(z)[2] + get_n(z)[3] + get_n(z)[4])

# get_type(x)
# get_display(x)
# get_n(x)
# get_wn(x)
# get_pct(x)
# get_digits(x)
# get_ctr(x)
# get_mean(x)
# get_var(x)
# get_ci(x)


# ---- Phase 17a janitorial fixes: failing-first fixtures ----

test_that("model_family is carried through the fmt carrier round-trip (Defect 1, Phase 17a)", {
  # A regression column carries its own model_family (Phase 15e). fmt_col_attrs -- the per-column
  # attribute list used by every carry/round-trip -- must include it, else it is silently dropped.
  # Pre-17a fmt_col_attrs was hand-written with 9 names and omitted model_family.
  expect_true("model_family" %in% fmt_col_attrs)
  expect_true("role" %in% fmt_col_attrs)                   # Phase 17c: the 11th column attribute
  expect_true("conf_level" %in% fmt_col_attrs)             # Phase 18z13: the 12th
  expect_true("degf"  %in% fmt_col_attrs)                  # Phase 18z16-iiiii: the 13th
  expect_true("basis" %in% fmt_col_attrs)                  #                       and the 14th
  expect_true("ci_method" %in% fmt_col_attrs)              # Phase 19b: the 15th
  expect_true("col_group" %in% fmt_col_attrs)              # Phase 19n: the 16th
  expect_length(fmt_col_attrs, 16L)

  tb <- tab(forcats::gss_cat, marital, race)
  tb[["Black"]] <- set_model_family(tb[["Black"]], "binomial")
  expect_identical(get_model_family(tb[["Black"]]), "binomial")

  round <- fmt_wrap(fmt_unwrap(tb))                       # the carrier round-trip (jmvtab / stacking)
  expect_identical(get_model_family(round[["Black"]]), "binomial")
})

test_that("vec_math sum/mean keep both colour channels + signif + model_family (Defect 2, Phase 17a)", {
  # color = TRUE gives a two-channel colour c(diff, ratio); the sum/mean arms of vec_math used to
  # rebuild with get_color() (first channel only) and drop color_signif / model_family.
  x <- tab(forcats::gss_cat, marital, race, pct = "row", color = TRUE)[["Black"]]
  x <- set_color_signif(x, "grey_non_signif")
  x <- set_model_family(x, "binomial")
  expect_length(fmt_color_attr(x), 2L)

  for (s in list(sum(x), mean(x))) {
    expect_identical(fmt_color_attr(s), fmt_color_attr(x))   # both channels, not just the first
    expect_identical(get_color_signif(s), "grey_non_signif")
    expect_identical(get_model_family(s), "binomial")
  }
})


# ---- Phase 19a / E1: the DECLARED attribute rules drive the four reconstructors ----

test_that("every fmt column attribute is DECLARED, and a bind yields its neutral (E1)", {
  # WHY THIS IS THE E1 FIXTURE. Before 19a the 14 attributes were enumerated by hand in SEVEN
  # reconstructor blocks, so a 15th one meant eight edits and the 10th (model_family) was silently
  # dropped for two phases. The loop below is driven by `fmt_attr_rules` itself, so a new attribute
  # is covered the day its row is added -- there is nothing to remember.
  R <- tabxplor:::fmt_attr_rules

  # 1. completeness + ORDER. Mirrors the build-time stopifnot in fmt_class.R, which a cached binary
  #    install would not re-run.
  expect_identical(names(R), fmt_col_attrs)
  # 2. the reader's default IS new_fmt()'s own formal default -- derived, so the two cannot drift.
  expect_identical(tabxplor:::fmt_attr_default,
                   lapply(formals(tabxplor:::new_fmt)[fmt_col_attrs], eval, envir = baseenv()))
  # 3. the shared zero-length ptype is never mutated by the splice (it is a namespace binding).
  expect_identical(attributes(tabxplor:::fmt_ptype_empty)[fmt_col_attrs],
                   attributes(tabxplor:::new_fmt())[fmt_col_attrs])

  a <- fmt(1:2, "level_pct", pct_type = "row", pct = c(.1, .2), ref = "tot", col_var = "v1",
           col_group = "g1", totcol = TRUE,  refcol = TRUE,  color = c("diff", "ratio"),
           color_signif = "grey_non_signif", model_family = "binomial", role = "model",
           conf_level = 0.99, degf = 30, basis = "design", ci_method = "newcombe",
           comp_all = TRUE)
  b <- fmt(1:2, "points", pct_type = "col", pct = c(.3, .4), ref = "first", col_var = "v2",
           col_group = "g2", totcol = FALSE, refcol = FALSE, color = c("ratio", "difference"),
           color_signif = "ignore", model_family = "poisson", role = "emp",
           conf_level = 0.90, degf = 12, basis = "weights", ci_method = "wilson",
           comp_all = FALSE)
  # NON-VACUOUS: all 16 must really differ, else every assertion below proves nothing.
  expect_true(all(!mapply(identical, tabxplor:::fmt_attrs_of(a), tabxplor:::fmt_attrs_of(b))))

  got <- tabxplor:::fmt_attrs_of(suppressWarnings(vctrs::vec_c(a, b)))
  for (nm in fmt_col_attrs) {
    rule <- R[[nm]]
    expected <- switch(
      rule$merge,
      same = , comp3 = , elementwise = rule$neutral,
      min     = 12,                       # the widest critical value wins -> the smallest degf
      weakest = "weights",                # a merge claims only what its weakest part carried
      stop("unhandled merge rule: ", rule$merge))
    expect_identical(got[[nm]][[1]], expected, label = paste0("neutral of `", nm, "`"))
  }
})

# Phase 22c-v: fmt() is the LAST writer of the `color` attribute that did not validate, so a stored
# colour was not guaranteed to be a MEASURES key. Now it is, by construction.
test_that("fmt() validates and canonicalises `color` / `color_signif` (22c-v)", {
  expect_identical(get_color(fmt(1L, color = "OR")),   "odds_ratio")
  expect_identical(get_color(fmt(1L, color = "rr")),   "ratio")
  expect_identical(get_color(fmt(1L, color = "RoM")),  "ratio")
  expect_identical(get_color(fmt(1L, color = "diff")), "difference")
  # a legacy combined string keeps its MEASURE half; the policy half belongs to color_signif
  expect_identical(get_color(fmt(1L, color = "after_ci")), "difference")
  # ...and an unknown one is an error, where it used to colour nothing in silence
  expect_error(fmt(1L, color = "banana"), "Unknown color measure")
  expect_error(fmt(1L, color = "IRR", color_signif = "banana"), "color_signif")
  # a regression-only acronym is named, not called unknown
  expect_error(fmt(1L, color = "cumOR"), "tab_reg")
  # the two gap measures ARE storable on a hand-built column (it may fill `obs` itself)
  expect_identical(get_color(fmt(1L, color = "adjustment")), "adjustment")
})

test_that("fmt arithmetic reconciles the INFERENCE claim instead of taking x's (E1)", {
  # THE one deliberate behaviour change of E1. vec_ptype2 has applied the weakest-claim rule since
  # z16-iiiii, but vec_arith took x's conf_level/degf/basis blindly -- so `x - y` kept x's account
  # of how ITS interval was computed and stapled it onto a number that is half y's.
  mk <- function(cl, dg, bs) tabxplor:::set_basis(
    tabxplor:::set_degf(tabxplor:::set_conf_level(fmt(1:2, "level_pct", pct_type = "row", pct = c(.1, .2)), cl), dg), bs)
  a <- mk(0.99, 30, "design")
  b <- mk(0.90, 12, "weights")

  ab <- suppressWarnings(a + b)
  expect_identical(tabxplor:::get_basis(ab), "weights")     # weakest claim
  expect_identical(tabxplor:::fmt_degf_attr(ab), 12)        # widest critical value
  expect_true(is.na(tabxplor:::fmt_conf_level_attr(ab)))    # two levels -> "unknown", not x's
  # ... and the arith policies that did NOT change: display facts follow x, position is destroyed
  a2 <- set_color_signif(as_totcol(a, TRUE), "grey_non_signif")
  b2 <- set_color_signif(as_totcol(b, TRUE), "ignore")
  ab2 <- suppressWarnings(a2 * b2)
  expect_identical(get_color_signif(ab2), "grey_non_signif")
  expect_false(is_totcol(ab2))
})

test_that("adding a count column to a percentage one WARNS instead of erroring (E1)", {
  # `comp_all` is NA on a count column, so `same_comp` is three-valued and the guard `if (!same_comp)`
  # aborted with "missing value where TRUE/FALSE needed". The reconcile itself was always NA-safe.
  expect_warning(out <- fmt(5L) + fmt(5L, "level_pct", pct_type = "row", pct = .5, comp_all = FALSE))
  expect_true(is.na(get_comp_all(out, replace_na = FALSE)))   # NA-vs-set stays NA (rule "comp3")
})
