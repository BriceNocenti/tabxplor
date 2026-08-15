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
  # the getters read the SAME objects the meta list holds
  expect_identical(get_vars_attr(t), m$spec$vars)      # Phase 19g: `vars` is a slot of meta$spec
  expect_identical(tab_kind(t), "crosstab")            # ...beside the STORED table kind
  # Phase 19b: which interval METHOD was used is a per-COLUMN fact, not a meta sub-field. A count
  # column carries no interval, so it names none -- which is the point: the method describes THIS
  # column's bounds, not a table-wide setting the legend then indexes by measure (D8).
  expect_true(all(get_ci_method(t)[purrr::map_lgl(t, is_fmt)] == ""))
  tp <- tab(forcats::gss_cat, marital, race, pct = "row", ci = "ref")
  expect_true(all(get_ci_method(tp)[purrr::map_lgl(tp, is_fmt)] == "newcombe"))
})

test_that("meta (vars / render_extras) survives a dplyr pipeline", {
  t <- tab(forcats::gss_cat, marital, race, ci = "auto")
  out <- t |>
    dplyr::filter(TRUE) |>
    dplyr::mutate(.zzz = 1) |>
    dplyr::arrange(dplyr::desc(.data[[names(t)[[1]]]])) |>
    dplyr::select(-".zzz")
  expect_false(is.null(get_vars_attr(out)))
  expect_false(is.null(get_render_extras(out)))
  expect_identical(get_vars_attr(out), get_vars_attr(t))
})

test_that("set_render_extras(NULL) clears ONLY render_extras, keeping vars", {
  t <- tab(forcats::gss_cat, marital, race, ci = "auto")
  expect_false(is.null(get_render_extras(t)))
  t2 <- set_render_extras(t, NULL)
  expect_null(get_render_extras(t2))
  expect_false(is.null(get_vars_attr(t2)))          # untouched
})

test_that("an unset table carries NO meta attribute (absent-when-unset)", {
  e <- new_tab(tibble::tibble(a = 1:2))
  expect_null(attr(e, "meta", exact = TRUE))
  # emptying the last meta field removes the whole attribute
  t  <- set_vars_attr(new_tab(tibble::tibble(a = 1:2)), new_vars_attr(wt = "a"))
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

test_that("get_test reads the top-level test attr", {
  t <- tab(forcats::gss_cat, marital, race)
  expect_s3_class(get_test(t), "tbl_df")
})

# === SECTION: meta must SURVIVE every table rebuild (Phase 18z16-iv, W-A) ======================
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
  # Phase 18z16-iiiii (defect 1): tab_spread() -- exported, AND what tab(spread_vars =) calls --
  # ended in a bare new_tab(tabs, subtext =, test =) literal, so EVERY spread table silently lost its
  # whole meta. It was the SECOND rebuild-from-a-literal site, which z16-iv's record said did not exist.
  ts <- probe(tab(forcats::gss_cat, marital, race, relig, pct = "row"))
  expect_identical(kept(tab_spread(ts, relig)), list(kept = TRUE))
})

test_that("bind_rows() on two GROUPED tabs keeps subtext / test / meta (Phase 19a, D16)", {
  # THE FIFTH instance of "a rebuild site drops table-level facts", and the one that took the
  # 15-verb carrier score from 14/15 to 15/15. dplyr's dplyr_reconstruct generic runs `data` through
  # dplyr_new_data_frame() BEFORE dispatch, so the method received a payload with no attributes at
  # all; restoring from it gave back a correctly-classed grouped tab carrying NOTHING -- no weight
  # footer, no CI legend, no inference basis, no test summary. Fixed by restoring from `template`.
  # It is ALSO the only carrier on that path: dplyr's own vec_ptype2.grouped_df.grouped_df wins over
  # vec_ptype2.tabxplor_grouped_tab.tabxplor_grouped_tab, which a bind therefore never reaches.
  g <- tab(forcats::gss_cat, marital, race, relig, pct = "row", test = TRUE)
  expect_s3_class(g, "tabxplor_grouped_tab")                       # non-vacuous: really grouped
  expect_false(is.null(attr(g, "subtext", exact = TRUE)))          # ... and really populated
  expect_false(is.null(tabxplor:::get_test(g)))
  expect_false(is.null(tabxplor:::get_meta(g)))

  b <- dplyr::bind_rows(g, g)
  expect_s3_class(b, "tabxplor_grouped_tab")
  expect_identical(attr(b, "subtext", exact = TRUE), attr(g, "subtext", exact = TRUE))
  expect_identical(tabxplor:::get_test(b), tabxplor:::get_test(g))
  expect_identical(tabxplor:::get_vars_attr(b), tabxplor:::get_vars_attr(g))

  # and the field-AGNOSTIC probe, so a future `meta` sub-field is covered without an edit here
  p <- tabxplor:::set_meta_field(g, "zz_probe", list(kept = TRUE))
  expect_identical(tabxplor:::get_meta(dplyr::bind_rows(p, p))[["zz_probe"]], list(kept = TRUE))
})

test_that("tab_spread() keeps the weight footer, and narrows only tab_vars", {
  skip_if_no_gettext()
  d <- forcats::gss_cat[!is.na(forcats::gss_cat$tvhours) & forcats::gss_cat$tvhours > 0, ]
  withr::local_options(list(tabxplor.design_effect = TRUE, tabxplor.lang = "en"))
  flat <- tab(d, marital, race, relig, wt = tvhours, pct = "row")
  wide <- tab(d, marital, race, relig, wt = tvhours, pct = "row", spread_vars = relig)
  expect_identical(tabxplor:::tab_inference_basis(flat), "weights")   # non-vacuous
  expect_identical(tabxplor:::tab_inference_basis(wide), "weights")
  expect_identical(tab_weight_line(wide), tab_weight_line(flat))
  # Phase 19f/19g: the variable MODEL is derived from the columns; `vars` keeps only what none can
  # carry, and the pivot leaves all of it alone.
  v_flat <- tabxplor:::get_vars_attr(flat); v_wide <- tabxplor:::get_vars_attr(wide)
  expect_identical(v_wide$wt,       v_flat$wt)
  expect_identical(tabxplor:::tab_render_vars(wide)$tab_vars, character(0))
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

test_that("the weakest-claim rule lives on the COLUMN reconcile now", {
  # Phase 18z16-iiiii: `inference` left `meta` for two per-column attributes, so the bind algebra
  # left tab_inference_bind() for vec_ptype2.tabxplor_fmt.tabxplor_fmt() -- where it fires on every
  # c() / bind / group without anyone having to call it.
  mk <- function(b, d = NA_real_) tabxplor:::set_degf(tabxplor:::set_basis(fmt(1:2), b), d)
  bs <- function(x, y) tabxplor:::get_basis(c(x, y))
  expect_identical(bs(mk("design"),         mk("weights")), "weights")
  expect_identical(bs(mk("weights"),        mk("design")),  "weights")   # symmetric
  expect_identical(bs(mk("n"),              mk("design")),  "n")
  expect_identical(bs(mk("design_partial"), mk("design")),  "design_partial")
  expect_identical(bs(mk("design"),         mk("design")),  "design")
  # the widest critical value wins: the SMALLEST design df survives a bind
  expect_identical(tabxplor:::fmt_degf_attr(c(mk("design", 30), mk("design", 12))), 12)
  expect_identical(tabxplor:::fmt_degf_attr(c(mk("design", 30), mk("design"))), 30)  # NA is not a claim
  expect_true(is.na(tabxplor:::fmt_degf_attr(c(mk("design"), mk("design")))))
  # and it reaches a whole TABLE through the derived reader
  t <- tab(forcats::gss_cat, marital, race, pct = "row")
  expect_identical(tabxplor:::tab_inference_basis(t), "n")
  expect_identical(tabxplor:::tab_inference_degf(t), Inf)
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
  t2 <- tabxplor:::set_meta_field(tabxplor:::set_vars_attr(t, v), "inference", NULL)
  expect_null(tab_weight_line(t2))
})

# === SECTION: the regression rebuild sites (Phase 18z16-iiiii) =================================

test_that("a weighted tab_reg(split_var=) keeps its inference, spread or stacked", {
  skip_if_no_gettext()
  skip_if_not_installed("survey")
  withr::local_options(list(tabxplor.lang = "en"))
  set.seed(11)
  n <- 400L
  d <- tibble::tibble(
    g = factor(sample(c("a", "b"), n, TRUE)),
    s = factor(sample(c("north", "south"), n, TRUE)),
    y = factor(sample(c("no", "yes"), n, TRUE)),
    w = stats::rgamma(n, 2, 2)
  )
  mk <- function(...) suppressMessages(
    tab_reg(d, dependent = "y", predictors = "g", family = "binomial", wt = "w", ...))
  flat <- mk()
  # The auto-spread is the shape that lost everything: it routes through tab_spread(), whose bare
  # new_tab() literal dropped the whole meta, so the table asserted "intervals use the unweighted
  # sample size" while its models came from svyglm. The stacked shape (several models per group) is
  # checked beside it.
  wide <- mk(split_var = "s")
  tall <- suppressMessages(tab_reg(d, dependent = "y", predictors = list(m1 = "g", m2 = "g"),
                                   family = "binomial", wt = "w", split_var = "s"))
  expect_identical(tabxplor:::tab_inference_basis(flat), "weights")   # non-vacuous
  expect_identical(tabxplor:::tab_inference_basis(wide), "weights")
  expect_identical(tabxplor:::tab_inference_basis(tall), "weights")
  expect_match(tab_weight_line(wide), "account for the weighting")
  expect_identical(tab_weight_line(wide), tab_weight_line(flat))
})

test_that("a split tab_reg()'s columns name the same interval methods as an unsplit one", {
  set.seed(12)
  n <- 300L
  d <- tibble::tibble(
    g = factor(sample(c("a", "b"), n, TRUE)),
    s = factor(sample(c("north", "south"), n, TRUE)),
    y = stats::rnorm(n)
  )
  mk <- function(...) suppressMessages(
    tab_reg(d, dependent = "y", predictors = "g", family = "gaussian", ...))
  # the split branch used to write a THREE-key reduction of the six the unsplit branch writes, so a
  # split gaussian/poisson table's legend could not name the interval its Obs_* columns print.
  # Phase 19b: the methods ride the COLUMNS, so a rebuild site cannot lose them at all.
  meth <- function(t) sort(unique(get_ci_method(t)[purrr::map_lgl(t, is_fmt)]))
  expect_identical(meth(mk(split_var = "s")), meth(mk()))
})

test_that("tab_reg() on a survey design keeps the design's degrees of freedom", {
  skip_if_not_installed("survey")
  set.seed(13)
  n <- 400L
  dd <- tibble::tibble(
    psu   = rep(1:10, each = n / 10L),
    strat = rep(1:2,  each = n / 2L),
    w     = stats::rgamma(n, 2, 2),
    g     = factor(sample(c("a", "b", "c"), n, TRUE)),
    y     = factor(sample(c("no", "yes"), n, TRUE))
  )
  des <- survey::svydesign(ids = ~psu, strata = ~strat, weights = ~w, data = dd, nest = TRUE)
  # tab_reg() rebuilt its design_spec from a literal AFTER the boundary had computed degf, so it was
  # the one design consumer that never saw it: its model columns were on t(degf) (df.residual() of an
  # svyglm IS the design df) while its crude Obs_* columns stayed on z.
  tr <- suppressMessages(
    tab_reg(des, dependent = "y", predictors = "g", family = "binomial", empirical = TRUE))
  tt <- suppressMessages(tab(des, g, y, pct = "row", ci = "cell"))
  # (svy_degf() stores it as a double; survey::degf() returns an integer)
  expect_identical(tabxplor:::tab_inference_degf(tr), as.double(survey::degf(des)))
  expect_identical(tabxplor:::tab_inference_degf(tr), tabxplor:::tab_inference_degf(tt))
})
