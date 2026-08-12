
testthat::test_that("class is right", {
  testthat::expect_s3_class(fmt(1), "tabxplor_fmt")
})

testthat::test_that("fmt prints without error", {
  testthat::expect_output(
    print(fmt(n = c(5, 10, 15), type = "n", display = c("n", "row", "mean"),
              wn = c(4.7, 12.1, 13.9), digits = 1, pct = c(NA, 0.63, NA),
              mean = c(NA, NA, 27.3)))
  )
  testthat::expect_output(print(tibble::tibble(
    fmt(n = c(15, 10, 5), type = "row", display = c("n", "row", "mean"),
        wn = c(13.9, 12.1, 4.7), digits = 0, pct = c(NA, 0.22, NA),
        mean = c(NA, NA, 21))
  )))
})

#test of common type :
# vec_ptype_show(fmt(1, "row", pct = 0.255), fmt(2, "row", pct = 0.987))
# vec_ptype_show(fmt(), double(), fmt())
# vec_ptype_common(fmt(1, "row", pct = 0.255), fmt(2, "row", pct = 0.987))
# vec_ptype2(fmt(1, "row", pct = 0.255), fmt(2, "row", pct = 0.987))
# vec_ptype2(fmt(1, "row", pct = 0.255), fmt(2, "col", pct = 0.987))

testthat::test_that("class is right after conversion", {
  testthat::expect_s3_class(vec_cast(5, fmt()), "tabxplor_fmt")
  testthat::expect_s3_class(vec_cast(5L, fmt()), "tabxplor_fmt")
  testthat::expect_type(vec_cast(fmt(6), double()), "double")
  testthat::expect_type(vec_cast(fmt(6), integer()), "integer")
  testthat::expect_type(vec_cast(fmt(1, "row", pct = 0.6005), character()), "character")
  testthat::expect_s3_class(vec_cast(NA, fmt()), "tabxplor_fmt")
})
# vec_cast(fmt(1, "row", pct = 0.255), fmt(2, "row", pct = 0.987))

testthat::test_that("combinations with c() work", {
  testthat::expect_s3_class(vec_c(fmt(1, "row", pct = 0.255),
                                  fmt(2, "row", pct = 0.987)), "tabxplor_fmt")
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
  f <- fmt(c(7, 19), "row", or = c(1.2, 0.8), model_family = "binomial")
  testthat::expect_identical(get_model_family(f), "binomial")
  testthat::expect_identical(get_model_family(set_model_family(f, "poisson")), "poisson")
  testthat::expect_identical(get_model_family(fmt(1, "row", pct = 0.3)), "")   # inert default

  # vec_c of two different families collapses to "" (like col_var -> "several_vars"); same survives
  testthat::expect_identical(
    get_model_family(vec_c(f, set_model_family(f, "gaussian"))), "")
  testthat::expect_identical(
    get_model_family(vec_c(f, fmt(3, "row", or = 2, model_family = "binomial"))), "binomial")

  # arithmetic carries x's family; cast copies model_family from `to`
  testthat::expect_identical(get_model_family(f + f), "binomial")
  testthat::expect_identical(get_model_family(vec_cast(2.5, f)), "binomial")

  # data.frame getter -> one value per column
  df <- tibble::tibble(a = f, b = fmt(1, "row", pct = 0.3))
  testthat::expect_identical(unname(get_model_family(df)), c("binomial", ""))
})

testthat::test_that("arithmetic between fmt and fmt works", {
  a <- fmt(5, "n"  , 0, wn = 5.1)
  b <- fmt(1, "n"   , 0, pct  = 0.25000001, wn =  1.5)
  testthat::expect_equal(get_n(a + b), 6)
  testthat::expect_equal(get_wn(a + b), 5.1 + 1.5)

  testthat::expect_warning((fmt(15L, "row" , 1, pct =  0.55, wn = 15.1) -
                              fmt(  2L, "mean", 0, mean = 0.25000001, wn =  2.5 )))

  a <- fmt(25, "row" , 2, pct =  0.55      , wn = 25.1)
  b <- fmt(3 , "row" , 3, pct  = 0.25000001, wn =  3.5)
  testthat::expect_equal(get_pct(a - b), 0.55 - 0.25000001)

  a <- fmt(25, "row" , 2, pct =  0.55      , wn = 25.1)
  b <- fmt(3 , "row" , 3, pct  = 0.25000001, wn =  3.5 )
  testthat::expect_equal(get_pct(a / b), 0.55 / 0.25000001)

  a <- fmt(35, "mean" , 3, mean = 3.55, wn = 35.1)
  b <- fmt(4 , "mean" , 0, mean = 1.60, wn =  4.5)
  testthat::expect_equal(get_mean(a + b), (3.55 * 35.1 + 1.60 * 4.5)/(35.1 + 4.5))
})

testthat::test_that("arithmetic between fmt and numeric works", {
  (fmt(45, "row" , 4, pct =  0.55, wn = 5.1) + 0.7)|> testthat::expect_s3_class("tabxplor_fmt")
  (fmt(55, "mean", 3, mean = 2.55, wn = 55.1) - 1) |> testthat::expect_s3_class("tabxplor_fmt")
  (fmt(65, "row", 2, pct =  0.55, wn = 65.1) / 2)  |> testthat::expect_s3_class("tabxplor_fmt")
  (fmt(75, "n" ,-1, pct =  0.55, wn = 75.1) * 3)   |> testthat::expect_s3_class("tabxplor_fmt")
  (fmt(1) + 1)                                     |> testthat::expect_s3_class("tabxplor_fmt")
  (1 + fmt(1, "row", pct = 0.12))                  |> testthat::expect_s3_class("tabxplor_fmt")
  (1 - fmt(1, "row", pct = 0.12))                  |> testthat::expect_s3_class("tabxplor_fmt")
  (2 / fmt(3, "row", pct = 0.12))                  |> testthat::expect_s3_class("tabxplor_fmt")
  (5 * fmt(1, "n", 2)           )                  |> testthat::expect_s3_class("tabxplor_fmt")
  (-fmt(1, "row", pct = 0.12)   )                  |> testthat::expect_s3_class("tabxplor_fmt")
})

testthat::test_that("math (sum and mean) between fmt and fmt works", {
  testthat::expect_equal(get_n(sum(fmt(1), fmt(1))), 2)
  testthat::expect_equal(get_n(mean(fmt(1, "n", 2), fmt(1, "n", 2))), 1)
})

testthat::test_that("fmt vectors works with mutate", {

  data <- dplyr::starwars |>
    tab_prepare("sex", "hair_color", "eye_color", "mass", "gender",
                other_if_less_than = 5)

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
  fmt_vect <- fmt(n = c(1, 2), type = "n")
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
  x <- set_display(fmt(n = c(10L, 20L), type = "row", pct = c(0.4, 0.6), display = "pct"),
                   "{pct} ({n})")
  testthat::expect_identical(format(x), c("40% (10)", "60% (20)"))   # byte-identical to Phase 10c
  y <- set_display(x, "{n} ({pct})")
  testthat::expect_identical(format(y), c("10 (40%)", "20 (60%)"))
  z <- set_display(x, "{pct} (n={n})")
  testthat::expect_identical(format(z), c("40% (n=10)", "60% (n=20)"))
})

testthat::test_that("a composite cell resolves to its PRIMARY (get_num / Excel / tibble header)", {
  x0 <- fmt(n = c(10L, 20L), type = "row", pct = c(0.4, 0.6), display = "pct")
  xs <- set_display(x0, "{pct} ({n})")
  # get_num() and the Excel bypass show the primary field -- byte-identical to the plain column.
  testthat::expect_identical(get_num(xs), get_num(x0))
  testthat::expect_identical(format(xs, syntax = "excel"), format(x0, syntax = "excel"))
  # the tibble header abbreviates to the primary type, not the raw template.
  testthat::expect_identical(vctrs::vec_ptype_abbr(xs), vctrs::vec_ptype_abbr(x0))
})

testthat::test_that("format() is byte-identical when no cell is a composite", {
  x0 <- fmt(n = c(10L, 20L), type = "row", pct = c(0.4, 0.6), display = "pct")
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






# x <- fmt(n = c(2, 1), type = "row", pct = c(0.5, 1.5)) #wn = c(0.7, 2.4)
# y <- fmt(n = c(3, 9), type = "n"  , pct = c(0.5, 1.5)) #wn = c(0.7, 2.4)
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
  expect_true("conf_level" %in% fmt_col_attrs)             # Last Phase z13: the 12th
  expect_true("degf"  %in% fmt_col_attrs)                  # Last Phase z16-iiiii: the 13th
  expect_true("basis" %in% fmt_col_attrs)                  #                       and the 14th
  expect_length(fmt_col_attrs, 14L)

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
