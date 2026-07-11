
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
  (fmt(45, "row" , 4, pct =  0.55, wn = 5.1) + 0.7)%>% testthat::expect_s3_class("tabxplor_fmt")
  (fmt(55, "mean", 3, mean = 2.55, wn = 55.1) - 1) %>% testthat::expect_s3_class("tabxplor_fmt")
  (fmt(65, "row", 2, pct =  0.55, wn = 65.1) / 2)  %>% testthat::expect_s3_class("tabxplor_fmt")
  (fmt(75, "n" ,-1, pct =  0.55, wn = 75.1) * 3)   %>% testthat::expect_s3_class("tabxplor_fmt")
  (fmt(1) + 1)                                     %>% testthat::expect_s3_class("tabxplor_fmt")
  (1 + fmt(1, "row", pct = 0.12))                  %>% testthat::expect_s3_class("tabxplor_fmt")
  (1 - fmt(1, "row", pct = 0.12))                  %>% testthat::expect_s3_class("tabxplor_fmt")
  (2 / fmt(3, "row", pct = 0.12))                  %>% testthat::expect_s3_class("tabxplor_fmt")
  (5 * fmt(1, "n", 2)           )                  %>% testthat::expect_s3_class("tabxplor_fmt")
  (-fmt(1, "row", pct = 0.12)   )                  %>% testthat::expect_s3_class("tabxplor_fmt")
})

testthat::test_that("math (sum and mean) between fmt and fmt works", {
  testthat::expect_equal(get_n(sum(fmt(1), fmt(1))), 2)
  testthat::expect_equal(get_n(mean(fmt(1, "n", 2), fmt(1, "n", 2))), 1)
})

testthat::test_that("fmt vectors works with mutate", {

  data <- dplyr::starwars %>%
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

# === SECTION: Phase 10c -- display_spec composite display =======================

testthat::test_that("display_spec attribute round-trips and validates", {
  x <- fmt(n = c(10L, 20L), type = "row", pct = c(0.4, 0.6))
  testthat::expect_true(is.na(get_display_spec(x)))              # default NA
  x2 <- set_display_spec(x, "pct (n)")
  testthat::expect_identical(get_display_spec(x2), "pct (n)")
  testthat::expect_true(is.na(get_display_spec(set_display_spec(x2, NA))))
  testthat::expect_error(set_display_spec(x, "diff"), "composite")  # not a whitelisted recipe
})

testthat::test_that("composite display renders 'primary (secondary)' on value cells", {
  x <- set_display_spec(fmt(n = c(10L, 20L), type = "row", pct = c(0.4, 0.6),
                            display = "pct"), "pct (n)")
  testthat::expect_identical(format(x), c("40% (10)", "60% (20)"))
  y <- set_display_spec(x, "n (pct)")
  testthat::expect_identical(format(y), c("10 (40%)", "20 (60%)"))
})

testthat::test_that("display_spec is byte-identical when unused, and Excel-bypass ignores it", {
  x0 <- fmt(n = c(10L, 20L), type = "row", pct = c(0.4, 0.6), display = "pct")
  xs <- set_display_spec(x0, "pct (n)")
  testthat::expect_identical(format(x0), format(set_display_spec(x0, NA)))  # NA == default
  # the Excel bypass reads get_num()/get_display(), which the attribute does NOT touch.
  testthat::expect_identical(get_num(xs), get_num(x0))
  testthat::expect_identical(get_display(xs), get_display(x0))
})

testthat::test_that("tab(display = ) sets the composite recipe", {
  tb <- tab(forcats::gss_cat, marital, race, pct = "row", display = "pct (n)")
  fmt_col <- tb[[which(purrr::map_lgl(tb, is_fmt))[1]]]
  testthat::expect_identical(get_display_spec(fmt_col), "pct (n)")
  testthat::expect_match(format(fmt_col)[1], "\\([0-9 ]+\\)$")   # "...(n)"
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
# sum(x) %>% vec_data()
# sum(y) %>% vec_data()
# sum(z) %>% vec_data()
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
