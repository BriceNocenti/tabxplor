# Phase 19k: the jamovi module speaks tabxplor's OWN vocabularies -- there is no translation layer
# left between a control and the argument it names. Nothing MECHANICAL enforces that: the `.a.yaml`
# option values are hand-written (they carry titles, HTML and the translation keys), and the `.js`
# rule blocks are generated but could be edited by hand. So this file is the enforcement:
#
#   1. every List option's value set EQUALS the R vocabulary it names;
#   2. every generated `.js` block is what dev/generate_jamovi_js.R would write today.
#
# It is why "keep the UI in sync" is a checked property here and not a convention. When a vocabulary
# legitimately moves, the yaml moves with it in the same commit -- that is the whole point.

# ⚠ `jamovi/` is .Rbuildignore'd, so NONE of these files exists inside a built package -- this whole
# file is a source-tree check. Guard on the FILE, exactly as the generated-block test below guards on
# `dev/`: without it `R CMD check` errors on the tarball (found by Phase 19n's check(), the first one
# run since 19b, i.e. since this file was written).
yaml_opts <- function(file) {
  skip_if_not_installed("yaml")
  path <- testthat::test_path("..", "..", "jamovi", file)
  skip_if_not(file.exists(path), "jamovi/ is not shipped in a built package")
  y <- yaml::read_yaml(path)
  stats::setNames(y$options, vapply(y$options, function(o) o$name, character(1)))
}
# The declared value set of one List option, in declaration order.
opt_values <- function(opts, name) {
  o <- opts[[name]]
  expect_false(is.null(o), info = paste("option missing from the .a.yaml:", name))
  vapply(o$options, function(e) as.character(e$name), character(1))
}


test_that("jmvtab.a.yaml speaks tab()'s vocabularies", {
  o <- yaml_opts("jmvtab.a.yaml")

  # `color` = the crosstab colour MEASURES: what tab() accepts on the text channel, in MEASURES order,
  # with the two sentinels the UI adds in front.
  tab_measures <- names(MEASURES)[vapply(
    MEASURES, function(m) "tab" %in% m$producers && "text" %in% m$channels, logical(1))]
  expect_identical(opt_values(o, "color"), c("no", "auto", tab_measures))

  # `ci` = the ANCHOR question's four answers (resolve_ci_value()'s valid set).
  expect_identical(opt_values(o, "ci"), c("auto", "no", "cell", "ref"))

  # the four interval-method ComboBoxes = the four CI_METHODS slots, each in its declared order
  # (first = the default, which must also be the yaml default).
  for (slot in names(CI_METHODS)) {
    nm <- if (slot == "cell") "method_cell" else paste0("method_", slot)
    expect_identical(opt_values(o, nm), CI_METHODS[[slot]], info = nm)
    expect_identical(o[[nm]]$default, CI_METHODS[[slot]][[1]], info = nm)
  }

  # the vocabularies TAB_ARG_VALUES declares (the UI offers no ""/NA spellings)
  expect_identical(opt_values(o, "pct"),      TAB_ARG_VALUES$pct$values)
  expect_identical(opt_values(o, "na"),       TAB_ARG_VALUES$na$values)
  expect_identical(opt_values(o, "lvs"),      TAB_ARG_VALUES$levels$values)   # `levels` renamed: jmvcore clash
  expect_identical(opt_values(o, "comp"),     setdiff(TAB_ARG_VALUES$comp$values, ""))
  expect_identical(opt_values(o, "totaltab"), setdiff(TAB_ARG_VALUES$totaltab$values, ""))
  expect_identical(opt_values(o, "anova"),    TAB_ARG_VALUES$anova$values)

  # `color_signif` = the policy vocabulary normalize_color_spec() accepts
  expect_identical(opt_values(o, "color_signif"),
                   c("ignore", "grey_non_signif", "guaranteed_effect"))

  # `display`: EVERY offered value must be one tab(display =) accepts -- that is what defect D11 was
  # (four values the writer refuses), and what makes jmv_apply_display() deletable.
  tb <- tab(forcats::gss_cat, marital, race, pct = "row", ci = "ref", test = TRUE)
  for (d in opt_values(o, "display")) {
    # (messages muffled: D22's "renders void" note is legitimate here for the fields this small
    # table does not carry -- `mean` / `var` / `ctr`. What is under test is that no value ABORTS.)
    expect_no_error(suppressMessages(tab_apply_display(tb, d)),
                    message = paste("display value refused:", d))
  }

  # the retired options are GONE (19d retired `OR` onto display/ref2; `chi2` is `test`)
  expect_false("OR"   %in% names(o))
  expect_false("chi2" %in% names(o))
  expect_true("test"  %in% names(o))
})


test_that("jmvtabreg.a.yaml speaks tab_reg()'s vocabularies", {
  o <- yaml_opts("jmvtabreg.a.yaml")

  expect_identical(opt_values(o, "effect"),  REG_EFFECTS_VALUES)
  expect_identical(opt_values(o, "measure"), REG_MEASURES_VALUES)
  expect_identical(opt_values(o, "na"),      eval(formals(tab_reg)$na))
  expect_identical(opt_values(o, "method"),  eval(formals(tab_reg)$method))

  # `color` on a reg table: off / the column's own geometry / the own-reference measures. The last
  # two are DERIVED (measure_own_ref), so the yaml cannot offer a measure D25 refuses.
  own_ref <- names(MEASURES)[vapply(names(MEASURES), measure_own_ref, logical(1))]
  expect_identical(opt_values(o, "color"), c("auto", "no", own_ref))

  # the significance policy is spelled the SAME way, and in the same order, as in jmvtab
  expect_identical(opt_values(o, "color_signif"),
                   opt_values(yaml_opts("jmvtab.a.yaml"), "color_signif"))

  # the retired estimand options are GONE (19e); `display` replaced `estimate_display`
  expect_false("exponentiate"     %in% names(o))
  expect_false("at"               %in% names(o))
  expect_false("estimate_display" %in% names(o))
  expect_true("display"           %in% names(o))
  expect_true("shapes"            %in% names(o))   # the per-predictor functional-form picker
})


test_that("the generated .js rule blocks are up to date", {
  skip_on_cran()
  gen <- testthat::test_path("..", "..", "dev", "generate_jamovi_js.R")
  skip_if_not(file.exists(gen), "dev/ is not shipped in a built package")
  # `check` mode exits 1 when a block differs from what the R tables would write today.
  st <- system2("Rscript", c(shQuote(gen), "check"),
                stdout = TRUE, stderr = TRUE, env = "OMP_NUM_THREADS=1")
  expect_identical(attr(st, "status") %||% 0L, 0L,
                   info = paste(st, collapse = "\n"))
})


test_that("the .u.yaml controls name values their option declares", {
  skip_if_not_installed("yaml")
  for (an in c("jmvtab", "jmvtabreg")) {
    opts <- yaml_opts(paste0(an, ".a.yaml"))
    ui   <- yaml::read_yaml(testthat::test_path("..", "..", "jamovi", paste0(an, ".u.yaml")))
    # walk the control tree, collecting every (optionName, optionPart) pair a RadioButton declares
    pairs <- list()
    walk <- function(node) {
      if (!is.list(node)) return(invisible(NULL))
      if (!is.null(node$optionName) && !is.null(node$optionPart))
        pairs[[length(pairs) + 1L]] <<- c(as.character(node$optionName),
                                          as.character(node$optionPart))
      for (el in node) if (is.list(el)) walk(el)
      invisible(NULL)
    }
    walk(ui)
    for (p in pairs) {
      vals <- opt_values(opts, p[[1]])
      expect_true(p[[2]] %in% vals,
                  info = paste0(an, ".u.yaml: ", p[[1]], " = ", p[[2]],
                                " is not a declared value (", paste(vals, collapse = "/"), ")"))
    }
  }
})
