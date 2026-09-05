# PURPOSE: the tabxplor_fmt record -- its fields, its column attributes, and the arithmetic and casts that carry them.
# ROLE: the shipped CONTRACT for R/fmt_class.R -- a failure here is a user-visible change.
#   The exhaustive sweep around it lives in dev/tests/testthat/.
# See: CLAUDE.md section "Testing" (what belongs in which suite).

# === SECTION: the record: fields, arithmetic, casts, vec_math =====================================

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
  t0 <- tab(fx_gss(), marital, race, pct = "row")
  t1 <- tab(fx_gss(), marital, race, pct = "row", display = "{pct} ({n})")
  t3 <- tab(fx_gss(), marital, race, pct = "row", display = "{pct} (n={n})")
  fcol <- function(t) t[[which(purrr::map_lgl(t, is_fmt))[1]]]
  # value cells carry the {} template; the plain table's field is untouched.
  testthat::expect_true(any(grepl("{", get_display(fcol(t1)), fixed = TRUE)))
  testthat::expect_false(any(grepl("{", get_display(fcol(t0)), fixed = TRUE)))
  testthat::expect_match(format(fcol(t1))[1], "\\([0-9 ]+\\)$")     # "{pct} ({n})" -> "...(n)"
  testthat::expect_match(format(fcol(t3))[1], "\\(n=[0-9 ]+\\)$")   # "{pct} (n={n})" -> "...(n=..)"
  testthat::expect_identical(get_num(fcol(t1)), get_num(fcol(t0)))  # primary == the plain numbers
  # No curated sugar any more: the old recipe strings error, {} is required.
  testthat::expect_error(tab(fx_gss(), marital, race, display = "pct (n)"))
  testthat::expect_error(tab(fx_gss(), marital, race, display = "wibble"))
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

  tb <- tab(fx_gss(), marital, race)
  tb[["Black"]] <- set_model_family(tb[["Black"]], "binomial")
  expect_identical(get_model_family(tb[["Black"]]), "binomial")

  round <- fmt_wrap(fmt_unwrap(tb))                       # the carrier round-trip (jmvtab / stacking)
  expect_identical(get_model_family(round[["Black"]]), "binomial")
})

test_that("vec_math sum/mean keep both colour channels + signif + model_family (Defect 2, Phase 17a)", {
  # color = TRUE gives a two-channel colour c(diff, ratio); the sum/mean arms of vec_math used to
  # rebuild with get_color() (first channel only) and drop color_signif / model_family.
  x <- tab(fx_gss(), marital, race, pct = "row", color = TRUE)[["Black"]]
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


# `comp = "all"` puts the reference in the TOTAL TABLE, which holds one reference row per row_var
# BLOCK -- several row_vars stack several of them, and a row must read its OWN block's.
testthat::test_that("comp = \"all\" reads the total table's reference of its own row_var block", {
  g <- fx_gss()
  # `age` carries missing values rincome does not, so under na = "drop" the two blocks really are
  # two populations -- which is what makes the per-block reference more than cosmetic.
  t <- tab(g, c(rincome, age), tvhours, tab_vars = race, pct = "row", shape = c(age = "quartiles"),
           color = "difference", na = "drop", comp = "all")
  col <- t[["tvhours"]]
  refs <- tabxplor:::get_ref_field(col, get_mean)
  testthat::expect_length(refs, nrow(t))              # was length(x) * n_row_vars -- a hard error

  tot <- which(is_totrow(col) & is_tottab(col))
  testthat::expect_length(tot, 2L)
  testthat::expect_false(isTRUE(all.equal(get_mean(col)[tot[1]], get_mean(col)[tot[2]])))
  rv <- as.character(t$row_var)
  testthat::expect_equal(unique(refs[rv == rv[[1]]]), get_mean(col)[tot[[1]]])
  testthat::expect_equal(unique(refs[rv == rv[[nrow(t)]]]), get_mean(col)[tot[[2]]])

  testthat::expect_length(tabxplor:::get_mean_contrib(col), nrow(t))
  testthat::expect_silent(invisible(tab_html(t)))
})


# === SECTION: fmt_attr(): the programmatic attribute surface ======================================

testthat::test_that("fmt_attr() reaches every declared attribute", {
  x <- fmt(n = c(10, 20), pct = c(0.3, 0.7), scale = "level_pct", pct_type = "row")
  for (a in tabxplor:::fmt_col_attrs)
    testthat::expect_no_error(fmt_attr(x, a), message = paste("attribute", a))
  # ...and each one agrees with its own named accessor where there is one
  testthat::expect_identical(fmt_attr(x, "scale"),    get_scale(x))
  testthat::expect_identical(fmt_attr(x, "pct_type"), get_pct_type(x))
  testthat::expect_identical(fmt_attr(x, "col_var"),  get_col_var(x))
  testthat::expect_identical(fmt_attr(x, "totcol"),   is_totcol(x))
})

testthat::test_that("an unset attribute reads its declared neutral", {
  x <- fmt(n = 1)
  testthat::expect_identical(fmt_attr(x, "col_group"), "")
  testthat::expect_identical(fmt_attr(x, "conf_level"), NA_real_)
  testthat::expect_identical(fmt_attr(x, "role"), "")
})

testthat::test_that("fmt_attr<-() writes through the attribute's own setter", {
  x <- fmt(n = 1)
  fmt_attr(x, "col_var") <- "region"
  testthat::expect_identical(get_col_var(x), "region")
  # the validation is the setter's, not a second one: set_scale() checks EST_SCALE_KEYS
  testthat::expect_error(`fmt_attr<-`(x, "scale", "not_a_scale"))
  # every declared attribute has a writer -- the build-time assert beside fmt_attr_rules
  testthat::expect_true(all(vapply(tabxplor:::fmt_attr_rules,
                                   function(r) is.function(r$write), logical(1))))
})

testthat::test_that("an unknown attribute name aborts naming the set", {
  x <- fmt(n = 1)
  testthat::expect_error(fmt_attr(x, "colour"), "Unknown")
  testthat::expect_error(fmt_attr(x, "colour"), "col_var")
})

testthat::test_that("fmt_attr() on a data.frame reads every fmt column", {
  t <- tab(fx_gss(), marital, race, pct = "row")
  cv <- fmt_attr(t, "col_var")
  testthat::expect_true(all(cv == "race"))
  testthat::expect_length(cv, sum(vapply(t, is_fmt, logical(1))))
  # ...and refuses to write there, pointing at the verb that does it
  testthat::expect_error(`fmt_attr<-`(t, "col_var", "x"), "one")
})

testthat::test_that("set_ref_type() replaces set_diff_type(), which still works", {
  x <- fmt(n = 1)
  x <- set_ref_type(x, "first")
  testthat::expect_identical(get_ref_type(x), "first")
  lifecycle::expect_deprecated(y <- set_diff_type(fmt(n = 1), "tot"))
  testthat::expect_identical(get_ref_type(y), "tot")
})

testthat::test_that("tab_columns() reports one row per fmt column with its stored facts", {
  t  <- tab(fx_gss(), marital, race, pct = "row", ci = "ref", color = "diff")
  tc <- tab_columns(t)
  testthat::expect_s3_class(tc, "tbl_df")
  testthat::expect_equal(nrow(tc), sum(vapply(t, is_fmt, logical(1))))
  testthat::expect_true(all(c("column", "col_var", "scale", "conf_level", "degf", "basis",
                              "ci_method", "color", "color_bg") %in% names(tc)))
  testthat::expect_true(all(tc$col_var == "race"))
  testthat::expect_true(all(tc$color == "difference"))
  # the four inference facts, side by side -- the user story the getters could not answer
  testthat::expect_true(all(tc$conf_level == 0.95))
  testthat::expect_true(all(tc$basis == "n"))
  testthat::expect_true(any(tc$totcol))
  # a table with no fmt column is an empty answer, not an error
  testthat::expect_equal(nrow(tab_columns(data.frame(a = 1))), 0L)
  testthat::expect_error(tab_columns("not a table"))
})


# === SECTION: the field and attribute contract ====================================================

fmt_contract_fields <- c(
  "n", "display", "digits", "wn", "pct", "mean", "diff", "ratio", "ctr", "var",
  "ci_inf", "ci_sup", "pvalue", "or", "tot_n", "n_eff", "obs", "gap_se",
  "row_kind", "in_tottab", "in_refrow"
)

# Storage type of each field (typeof), as guaranteed by the vec_cast lines in fmt().
fmt_contract_field_types <- c(
  n = "integer", display = "character", digits = "integer", wn = "double",
  pct = "double", mean = "double", diff = "double", ratio = "double", ctr = "double",
  var = "double", ci_inf = "double", ci_sup = "double", pvalue = "double", or = "double",
  tot_n = "double", n_eff = "double", obs = "double", gap_se = "double",
  row_kind = "character", in_tottab = "logical", in_refrow = "logical"
)

# The 10 per-column attributes and their constructor defaults. Phase 5 added `color_signif`
# (the significance policy: "ignore" / "grey_non_signif" / "guaranteed_effect") -- it cannot fold
# into `color` (which is measure x channel) and pillar_shaft renders columns standalone, so the
# policy must live on the column. The `color` attribute is now length 1 (text) or 2 (text, bg).
# Phase 10i-A DROPPED the Phase-10c `display_spec` attribute (10 -> 9): the opt-in composite display
# is now a per-cell `display`-FIELD {} template ("{pct} (n={n})"), not a column attribute.
# Phase 15e ADDED `model_family` (9 -> 10): the per-column regression family ("" on cross-tables), so
# one table can mix several dependents of different families and each column keeps its effect wording.
# Phase 17c ADDED `role` (10 -> 11): a reg column's role ("model"/"emp", "" on cross-tables), read by
# the colour legend to name each column's effect without matching its rendered "Emp." label.
# Phase 19b REPLACED `type` (8 values, two jobs) with `scale` + `pct_type`, and DELETED `ci_type`
# (the stored interval is always on the estimate's own scale, and "is there one" is a data fact).
# Net 14 -> 15 attributes.
fmt_contract_attr_defaults <- list(
  scale = "level_n", comp_all = NA, ref = "", pct_type = "none",
  col_var = "", col_group = "", totcol = FALSE, refcol = FALSE, color = "", color_signif = "ignore",
  model_family = "", role = "",
  # Phase 18z13 (D3): the 12th. NA = "this column never recorded a level" -> every threshold in the
  # colour engine falls back to options(tabxplor.conf_level), i.e. the pre-z13 behaviour.
  conf_level = NA_real_,
  # Phase 18z16-iiiii: the 13th and 14th -- HOW this column's interval was computed. They were
  # meta$inference, a TABLE attribute, until two rebuild sites were found dropping the whole of `meta`.
  # NA / "n" = an unweighted or weights-only table: refer to z, claim no design effect.
  degf = NA_real_, basis = "n",
  # Phase 19b: the 15th -- WHICH interval engine built this column's bounds ("" = none). It was
  # meta$ci_settings, a table-wide vector the legend indexed BY MEASURE (D8).
  ci_method = ""
  # Phase 19n added the 16th, `col_group` (declared beside `col_var` above): WHICH SUB-POPULATION
  # this column's block belongs to. It was WELDED into `col_var` as "{level}<br>{col_var}", so three
  # backends sniffed an html tag out of a variable name to recover it.
)

testthat::test_that("fmt has exactly the contracted fields, in order", {
  x <- fmt(1)
  testthat::expect_identical(vctrs::fields(x), fmt_contract_fields)
  testthat::expect_length(vctrs::fields(x), 21L)
})

testthat::test_that("each fmt field has the contracted storage type", {
  x <- fmt(1)
  for (f in fmt_contract_fields) {
    testthat::expect_identical(
      typeof(vctrs::field(x, f)), fmt_contract_field_types[[f]],
      info = paste0("field '", f, "'")
    )
  }
})

testthat::test_that("fmt carries exactly the contracted column attributes with right defaults", {
  x <- fmt(1)
  # Presence + default value of every contracted attribute (read via attr(), the documented access).
  for (a in names(fmt_contract_attr_defaults)) {
    testthat::expect_identical(
      attr(x, a, exact = TRUE), fmt_contract_attr_defaults[[a]],
      info = paste0("attribute '", a, "'")
    )
  }
  # No UNCONTRACTED column attribute has crept in (structural attrs excluded).
  structural <- c("names", "class", "row.names")
  col_attrs <- setdiff(names(attributes(x)), structural)
  testthat::expect_setequal(col_attrs, names(fmt_contract_attr_defaults))
})

testthat::test_that("fmt survives saveRDS/readRDS round-trip with all fields and attributes", {
  x <- fmt(
    n = c(10L, 20L), scale = "level_pct", pct_type = "row", digits = 1L, display = c("n", "pct"),
    wn = c(9.5, 19.4), pct = c(NA, 0.5), mean = c(NA, NA), diff = c(NA, 0.1),
    ctr = c(NA, 0.3), var = c(NA, NA), ci = c(NA, 0.02),
    row_kind = c("data", "total"), in_refrow = c(TRUE, FALSE),
    comp_all = TRUE, ref = "tot", col_var = "sex",
    totcol = FALSE, color = "diff"
  )

  tmp <- tempfile(fileext = ".rds")
  on.exit(unlink(tmp), add = TRUE)
  saveRDS(x, tmp)
  y <- readRDS(tmp)

  testthat::expect_identical(y, x)                       # whole object identical
  testthat::expect_identical(vctrs::fields(y), vctrs::fields(x))
  for (f in fmt_contract_fields) {
    testthat::expect_identical(vctrs::field(y, f), vctrs::field(x, f),
                               info = paste0("field '", f, "'"))
  }
  for (a in names(fmt_contract_attr_defaults)) {
    testthat::expect_identical(attr(y, a, exact = TRUE), attr(x, a, exact = TRUE),
                               info = paste0("attribute '", a, "'"))
  }
})

# The `ci` bounds-shim (Phase 3a): the public fmt(ci=) half-width is stored as ABSOLUTE
# ci_inf/ci_sup bounds around the estimate the interval is centred on (here the proportion
# pct), and get_ci() / $ci read the half-width back as ci_sup - centre.
testthat::test_that("fmt(ci=) stores absolute bounds and get_ci() reads the half-width back", {
  x <- fmt(n = c(10L, 20L), scale = "level_pct", pct_type = "row", pct = c(0.4, 0.5), ci = c(NA, 0.02))
  testthat::expect_identical(vctrs::field(x, "ci_sup"), c(NA_real_, 0.52))  # pct + ci
  testthat::expect_identical(vctrs::field(x, "ci_inf"), c(NA_real_, 0.48))  # pct - ci
  testthat::expect_equal(get_ci(x), c(NA_real_, 0.02))       # half-width read back
  testthat::expect_identical(x$ci,   get_ci(x))              # $ci still works
})

# Human-readable second signal. Skipped on CRAN by default. Regenerate consciously with
# testthat::snapshot_accept("fmt-contract") only when the contract intentionally changes.
testthat::test_that("fmt record shape snapshot", {
  testthat::expect_snapshot({
    x <- fmt(1)
    cat("fields:\n")
    print(vctrs::fields(x))
    cat("\nfield types:\n")
    print(vapply(vctrs::fields(x), function(f) typeof(vctrs::field(x, f)), character(1)))
    cat("\ncolumn attributes:\n")
    print(sort(setdiff(names(attributes(x)), c("names", "class", "row.names"))))
  })
})


# === SECTION: NA cells (bind_rows fills a column absent from one input) ===========================
# dplyr::bind_rows() NA-fills a fmt column the other table has not got, and EVERY field of those
# cells comes back NA -- `display` and `row_kind` included. That is an ordinary user action, so a
# cell that shows nothing must render blank in every medium, never abort.

testthat::test_that("a NA-filled fmt cell renders in every medium", {
  gss <- fx_gss()
  t1  <- tab(gss, race, marital, pct = "row", color = "difference", test = TRUE)
  t2  <- tab(gss, race, partyid, pct = "row", color = "difference", test = TRUE)
  z   <- dplyr::bind_rows(t1, t2)

  na_cells <- is.na(tabxplor:::get_display(z[["Married"]]))
  testthat::expect_true(any(na_cells))

  testthat::expect_true(all(is.na(format(z[["Married"]])[na_cells])))
  testthat::expect_identical(fmt_color_channels(z[["Married"]])$text_slot[na_cells],
                             rep(0L, sum(na_cells)))
  testthat::expect_no_error(utils::capture.output(print(z)))
  testthat::expect_no_error(tab_html(z))
  testthat::expect_no_error(tab_html(z, tooltips = TRUE))
  testthat::expect_no_error(tab_md(z, print = FALSE))
})

testthat::test_that("the three row predicates never return NA", {
  x <- fmt(n = c(1L, 1L))
  x <- vctrs::`field<-`(x, "row_kind" , c("total", NA_character_))
  x <- vctrs::`field<-`(x, "in_refrow", c(TRUE   , NA))
  x <- vctrs::`field<-`(x, "in_tottab", c(TRUE   , NA))
  testthat::expect_identical(is_totrow(x), c(TRUE, FALSE))
  testthat::expect_identical(is_refrow(x), c(TRUE, FALSE))
  testthat::expect_identical(is_tottab(x), c(TRUE, FALSE))
})


# === SECTION: the read aliases of the renamed 1.x fields ==========================================

testthat::test_that("$rr reads $ratio, as the method's own comment promises", {
  t <- tab(fx_gss(), race, marital, pct = "row")
  f <- t[["Married"]]
  testthat::expect_identical(f$rr, f$ratio)
  testthat::expect_identical(f$rr, get_ratio(f))
})


# === SECTION: the console reads the shared anchor rule ============================================
# Phase 11. pillar_shaft() used to spell the anchor rule a second time, so a summary row greyed out
# in the console while the exports were being taught not to. Both now read fmt_row_look().

testthat::test_that("the console does not set back a summary row", {
  testthat::local_reproducible_output(crayon = TRUE)   # testthat's own switch; withr's loses
  withr::local_options(cli.num_colors = 256)          # ...which caps at 8, where the greys go flat
  m <- tabxplor:::tab_materialize_extras(
    tab(fx_gss(), race, marital, pct = "col", color = "diff", add_pct = TRUE),
    backend = "text", pvalue = TRUE)
  kinds <- tabxplor:::fmt_row_kind(m)
  # a column that actually GRADES something: pillar::style_subtle alone can be a no-op here, so a
  # wholly uncoloured column would prove nothing either way.
  fmts  <- names(which(vapply(m, tabxplor::is_fmt, logical(1))))
  graded_col <- Find(function(nm) any(tabxplor:::fmt_color_channels(m[[nm]])$text_slot > 0L), fmts)
  txt   <- as.character(format(pillar::pillar_shaft(m[[graded_col]]), width = 30))
  ansi  <- grepl("\033[", txt, fixed = TRUE)
  testthat::skip_if_not(any(ansi), "no ANSI in this console")
  # an ungraded row is painted with nothing: not greyed, not bolded
  testthat::expect_false(any(ansi[!tabxplor:::row_kind_graded(kinds)]))
  testthat::expect_true(any(ansi[kinds == "data"]))
})
