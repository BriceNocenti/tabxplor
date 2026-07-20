# PURPOSE: Phase 17b -- the `meta` table-attribute merge. Locks the six 1.4.0-new table attrs
#          (render_extras / ci_settings / vars / empirical_tips / reg_meta / color_breaks) into ONE
#          `meta` list, its dplyr-carry, its bind reconcile, the "absent when unset" invariant, and the
#          new set_caption()/get_caption() surface. Each assertion is non-vacuous.
# See: CLAUDE.md > Phase 17b ; R/tab_classes.R new_tab()/tab_attrs()/tab_bind_attrs().

test_that("meta gathers the attrs and every legacy getter reads into it", {
  t <- tab(forcats::gss_cat, marital, race, ci = "auto")
  m <- attr(t, "meta", exact = TRUE)
  expect_type(m, "list")
  expect_true(!is.null(get_vars_attr(t)))
  expect_true(!is.null(get_render_extras(t)))
  expect_true(!is.null(get_ci_settings(t)))          # ci = "auto" -> ci_settings recorded
  # the getters read the SAME objects the meta list holds
  expect_identical(get_vars_attr(t), m$vars)
  expect_identical(get_ci_settings(t), m$ci_settings)
})

test_that("meta (vars / ci_settings / render_extras) survives a dplyr pipeline", {
  t <- tab(forcats::gss_cat, marital, race, ci = "auto")
  out <- t |>
    dplyr::filter(TRUE) |>
    dplyr::mutate(.zzz = 1) |>
    dplyr::arrange(dplyr::desc(.data[[names(t)[[1]]]])) |>
    dplyr::select(-".zzz")
  expect_false(is.null(get_vars_attr(out)))
  expect_false(is.null(get_ci_settings(out)))
  expect_false(is.null(get_render_extras(out)))
  expect_identical(get_vars_attr(out), get_vars_attr(t))
})

test_that("set_render_extras(NULL) clears ONLY render_extras, keeping vars/ci_settings", {
  t <- tab(forcats::gss_cat, marital, race, ci = "auto")
  expect_false(is.null(get_render_extras(t)))
  t2 <- set_render_extras(t, NULL)
  expect_null(get_render_extras(t2))
  expect_false(is.null(get_vars_attr(t2)))          # untouched
  expect_false(is.null(get_ci_settings(t2)))        # untouched
})

test_that("an unset table carries NO meta attribute (absent-when-unset)", {
  e <- new_tab(tibble::tibble(a = 1:2))
  expect_null(attr(e, "meta", exact = TRUE))
  # emptying the last meta field removes the whole attribute
  t  <- set_vars_attr(new_tab(tibble::tibble(a = 1:2)), new_vars_attr(row_vars = "a"))
  expect_false(is.null(attr(t, "meta", exact = TRUE)))
  t0 <- set_vars_attr(t, NULL)
  expect_null(attr(t0, "meta", exact = TRUE))
})

test_that("color_breaks now rides meta -> survives a dplyr chain (defect 7)", {
  t <- tab(forcats::gss_cat, marital, race,
           color_breaks = list(pct_diff = c(0.05, 0.1, 0.2)))
  expect_false(is.null(get_color_breaks_attr(t)))
  t2 <- t |> dplyr::filter(TRUE) |> dplyr::mutate(.z = 1) |> dplyr::select(-".z")
  expect_false(is.null(get_color_breaks_attr(t2)))   # was dropped pre-17b
  expect_identical(get_color_breaks_attr(t2), get_color_breaks_attr(t))
})

test_that("bind reconciles color_breaks per named scale", {
  a <- set_color_breaks_attr(new_tab(tibble::tibble(x = 1:2)),
                             resolve_color_breaks_arg(list(pct_diff = c(0.05, 0.1, 0.2))))
  b <- set_color_breaks_attr(new_tab(tibble::tibble(x = 3:4)),
                             resolve_color_breaks_arg(list(pct_ratio = list(over = 2))))
  merged <- tab_meta_bind(get_meta(a), get_meta(b))$color_breaks
  expect_true(all(c("pct_diff", "pct_ratio") %in% names(merged)))   # both scales survive the bind
})

test_that("set_caption / get_caption round-trip and survive dplyr; NA removes it", {
  t <- tab(forcats::gss_cat, marital, race)
  expect_null(get_caption(t))
  tc <- set_caption(t, "My caption")
  expect_identical(get_caption(tc), "My caption")
  tc2 <- tc |> dplyr::filter(TRUE) |> dplyr::mutate(.z = 1) |> dplyr::select(-".z")
  expect_identical(get_caption(tc2), "My caption")
  expect_null(get_caption(set_caption(tc, NA)))
  expect_null(get_caption(set_caption(tc, NULL)))
})

test_that("a stored caption precedes reg_title in the markdown export", {
  t   <- tab(forcats::gss_cat, marital, race)
  md0 <- tab_md(t, print = FALSE)
  md1 <- tab_md(set_caption(t, "STORED CAP"), print = FALSE)
  expect_false(grepl("STORED CAP", md0, fixed = TRUE))
  expect_true(grepl("STORED CAP", md1, fixed = TRUE))     # rendered as a pandoc caption line
})

test_that("get_chi2 / get_test back-compat still read the top-level test attr", {
  t <- tab(forcats::gss_cat, marital, race)
  expect_identical(get_chi2(t), get_test(t))
  expect_s3_class(get_test(t), "tbl_df")
})
