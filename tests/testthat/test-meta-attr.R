# PURPOSE: Phase 17b -- the `meta` table-attribute merge. Locks the six 2.0.0-new table attrs
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

# === SECTION: meta must SURVIVE every table rebuild (Last Phase z16-iv, W-A) ======================
# THE guard is field-AGNOSTIC on purpose: it stamps a sub-field that no constructor, no getter and no
# bind rule knows about. Any re-enumeration of `meta` (a fresh `meta = list(a, b, c)` literal in a
# rebuilder) drops it and fails here -- which is exactly how meta$inference was lost in tab_compact(),
# where it inverted the footer sentence and cost the exported step path its `degf`.

test_that("every table rebuild carries an UNKNOWN meta sub-field (no re-enumeration)", {
  withr::local_options(list(lifecycle_verbosity = "quiet"))   # tab_transpose() is soft-deprecated
  probe <- function(x) tabxplor:::set_meta_field(x, "zz_probe", list(kept = TRUE))
  kept  <- function(x) tabxplor:::get_meta(x)[["zz_probe"]]
  tl <- tab(forcats::gss_cat, c(marital, relig), race, pct = "row", output_list = TRUE)
  expect_null(kept(tl[[1]]))                                     # non-vacuous: absent before
  tl <- purrr::map(tl, probe)
  expect_identical(kept(tab_compact(tl)),                    list(kept = TRUE))  # the >=2 row_var merge
  expect_identical(kept(dplyr::bind_rows(tl[[1]], tl[[1]])), list(kept = TRUE))  # the vctrs reconcile
  expect_identical(kept(tab_transpose(tl[[1]])),             list(kept = TRUE))  # rewrites vars only
  expect_identical(kept(dplyr::filter(tl[[1]], TRUE)),       list(kept = TRUE))  # a dplyr verb
})

test_that("a >=2 row_var table keeps meta$inference (the footer cannot invert)", {
  skip_if_no_gettext()
  d <- forcats::gss_cat[!is.na(forcats::gss_cat$tvhours) & forcats::gss_cat$tvhours > 0, ]
  withr::local_options(list(tabxplor.design_effect = TRUE, tabxplor.lang = "en"))
  one <- tab(d, marital, race, wt = tvhours, pct = "row")
  two <- tab(d, c(marital, relig), race, wt = tvhours, pct = "row")
  expect_identical(tabxplor:::tab_inference_basis(one), "weights")   # non-vacuous
  expect_identical(tabxplor:::tab_inference_basis(two), "weights")
  # the sentence the merged table prints must be the one the numbers earned
  expect_identical(tab_weight_line(two), tab_weight_line(one))
  expect_match(tab_weight_line(two), "account for the weighting")
})

test_that("an UNWEIGHTED merge still carries no inference (absent-when-unset)", {
  m <- tab(forcats::gss_cat, c(marital, relig), race, pct = "row")
  expect_false("inference" %in% names(tabxplor:::get_meta(m)))
})

test_that("the inference bind rule is min over n < weights < design_partial < design", {
  mk <- function(b, d = NA_real_) list(inference = tabxplor:::new_inference_attr(b, degf = d))
  bs <- function(x, y) tabxplor:::tab_meta_bind(x, y)$inference$basis
  expect_identical(bs(mk("design"),         mk("weights")), "weights")
  expect_identical(bs(mk("weights"),        mk("design")),  "weights")   # symmetric
  expect_identical(bs(mk("n"),              mk("design")),  "n")
  expect_identical(bs(mk("design_partial"), mk("design")),  "design_partial")
  expect_identical(bs(mk("design"),         mk("design")),  "design")
  expect_identical(tabxplor:::tab_meta_bind(mk("design", 30), mk("design", 12))$inference$degf, 12)
  expect_null(tabxplor:::tab_meta_bind(mk("design"), mk("design"))$inference$degf)  # NA stays ABSENT
  # a one-sided bind is a pass-through, not a downgrade
  expect_identical(tabxplor:::tab_meta_bind(mk("design"), NULL)$inference$basis, "design")
  expect_identical(tabxplor:::tab_meta_bind(NULL, mk("design"))$inference$basis, "design")
})

test_that("tab_weight_line() reads the STORED basis, never the .svy_weights column name", {
  skip_if_no_gettext()
  withr::local_options(list(tabxplor.lang = "en"))
  g <- forcats::gss_cat[!is.na(forcats::gss_cat$tvhours) & forcats::gss_cat$tvhours > 0, ]
  t <- tab(g, marital, race, wt = tvhours)
  expect_match(tab_weight_line(t), "unweighted sample size")           # basis "n" = the default
  # forge the internal design weight name with NO stored inference: the line is DROPPED, the internal
  # name is never printed, and no claim about the intervals is invented.
  v <- get_vars_attr(t); v$wt <- ".svy_weights"
  t2 <- tabxplor:::set_meta_field(tabxplor:::set_meta_field(t, "vars", v), "inference", NULL)
  expect_null(tab_weight_line(t2))
})
