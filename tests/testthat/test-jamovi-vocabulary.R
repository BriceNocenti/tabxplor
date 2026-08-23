# Phase 19k: the jamovi module speaks tabxplor's OWN vocabularies -- there is no translation layer
# left between a control and the argument it names. Nothing MECHANICAL enforces that: the `.a.yaml`
# option values are hand-written (they carry titles, HTML and the translation keys), and the `.js`
# rule blocks are generated but could be edited by hand. So this file is the enforcement:
#
#   1. every List option's value set EQUALS the R vocabulary it names;
#   2. every generated `.js` block is what dev/generate_jamovi_js.R would write today;
#   3. (Phase 20g-i) every option NAME is the producer argument it drives;
#   4. every `.u.yaml` control and every `ui.<name>` in the hand-written `.js` names something the
#      `.a.yaml` or the `.u.yaml` declares.
#
# ⚠ 3 and 4 exist because 1 could NOT see the Phase 20b/20c renames: it compares VALUES, and what
# moved was ARGUMENT NAMES -- so this file stayed green through six months of the reg panel showing
# `dependent` / `split_var` / `method` for arguments called `outcome` / `tab_vars` / `ci_method`.
# The jamovi UI shows R argument names ON PURPOSE (a user learns the API by clicking), so a stale
# name is the teaching path lying, and that has to be a checked property, not a convention.

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
# ui_bracket_names() -- Phase 20h: the names the .js reaches through `ui[...]` rather than `ui.<name>`.
# ⚠ THE LIMIT, stated: this is a regex over the SOURCE. There is no `node` and no `V8` on the dev box,
# so the .js is never parsed or executed (declined in 19n, recorded so it is not re-proposed) -- what
# is gated is the naming, not the behaviour. Three forms, each the sources' own convention:
#   (1) a string literal passed right after `ui`   bottomAlignInRow(ui, "xl_replace")
#                                                  arrGet(ui, "family", v, "family")
#   (2) an ARRAY literal whose .forEach indexes it ["totaltab_1", ...].forEach(... ui[nm] ...)
#   (3) Object.keys(OBJ).forEach with ui[...]      -> OBJ's declared keys (MEASURE_OF_RADIO ...)
ui_bracket_names <- function(js) {
  s <- paste(js, collapse = "\n")
  out <- character(0)
  h <- regmatches(s, gregexpr("\\w+\\s*\\(\\s*ui\\s*,\\s*[\"'][^\"']+[\"']", s, perl = TRUE))[[1]]
  out <- c(out, gsub("[\"']", "", sub("^.*,\\s*", "", h)))
  for (m in regmatches(s, gregexpr("\\[[^][]*\\][[:space:]]*\\.forEach", s, perl = TRUE))[[1]]) {
    i <- regexpr(m, s, fixed = TRUE)
    if (!grepl("ui\\[", substr(s, i, i + nchar(m) + 200L), perl = TRUE)) next
    out <- c(out, gsub("[\"']", "",
                       regmatches(m, gregexpr("[\"'][^\"']+[\"']", m, perl = TRUE))[[1]]))
  }
  obs <- unique(gsub("[^A-Za-z0-9_]", "", sub("^Object\\.keys\\(", "", regmatches(s,
    gregexpr("Object\\.keys\\(\\s*\\w+\\s*\\)", s, perl = TRUE))[[1]])))
  for (ob in obs) {
    i <- regexpr(paste0("Object\\.keys\\(\\s*", ob, "\\s*\\)"), s, perl = TRUE)
    if (i < 0 || !grepl("ui\\[", substr(s, i, i + 400L), perl = TRUE)) next
    d <- regmatches(s, regexpr(paste0("var\\s+", ob, "\\s*=\\s*\\{[^}]*\\}"), s, perl = TRUE))
    if (!length(d)) next
    out <- c(out, sub("\\s*:$", "", gsub("[\"']", "", regmatches(d,
      gregexpr("[\"']?[A-Za-z_][A-Za-z0-9_]*[\"']?\\s*:", d, perl = TRUE))[[1]])))
  }
  sort(unique(trimws(out)))
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

  # the interval-method ComboBoxes = the CI_METHODS slots THIS PRODUCER offers, each in its declared
  # order (first = the default, which must also be the yaml default). Phase 20c: CI_METHODS gained a
  # `model` slot for tab_reg(), and a crosstab has no model interval -- CI_SLOT_PRODUCER declares
  # which slots belong where, so the loop asks instead of enumerating.
  for (slot in ci_slots_of("tab")) {
    nm <- paste0("ci_method_", slot)     # `<argument>_<slot>`: four boxes, one `ci_method =` vector
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
})


test_that("jmvtabreg.a.yaml speaks tab_reg()'s vocabularies", {
  o <- yaml_opts("jmvtabreg.a.yaml")

  # The cascade's RIGHT half is scalar, and each option's value set IS the R vocabulary it names.
  expect_identical(opt_values(o, "effect"),  REG_EFFECTS_VALUES)
  expect_identical(opt_values(o, "measure"), REG_MEASURES_VALUES)

  # Its LEFT half -- `family` and `link` -- is a question about each OUTCOME, so both are hidden
  # per-outcome Arrays driven by the Model table. There is no value set to compare here: what the
  # link drop-down offers is TABX_LINKS, emitted from REG_FAMILIES$fits by dev/generate_jamovi_js.R
  # and checked by the generated-block test below. What IS checkable here is the shape the backend
  # folder reads (jmvtab_reg_link_vector() indexes `var` and `link`).
  for (nm in c("family", "link")) {
    expect_true(isTRUE(o[[nm]]$hidden), info = nm)
    expect_identical(o[[nm]]$type, "Array", info = nm)
    expect_identical(vapply(o[[nm]]$template$elements, function(e) e$name, character(1)),
                     c("var", nm), info = nm)
  }
  expect_identical(opt_values(o, "na"),      eval(formals(tab_reg)$na))
  # Phase 20c: `method` became `ci_method`'s `model` slot, so the vocabulary is CI_METHODS' -- a
  # stricter single source than the formal's own default vector was. Phase 20g-i renamed the option.
  expect_identical(opt_values(o, "ci_method"), CI_METHODS$model)

  # ⚠ There is NO `stats` control: Phase 22g-iii deleted all three, because tab_reg()'s own default
  # already compares several predictor subsets (22g-ii) and a picker offering "none" named the
  # opposite of what it did.
  for (nm in c("stats_compare", "stats_baseline", "stats_checks"))
    expect_false(nm %in% names(o), info = nm)

  # `color` on a reg table: off / the column's own geometry / the own-reference measures. The last
  # two are DERIVED (measure_own_ref), so the yaml cannot offer a measure D25 refuses.
  own_ref <- names(MEASURES)[vapply(names(MEASURES), measure_own_ref, logical(1))]
  expect_identical(opt_values(o, "color"), c("auto", "no", own_ref))

  # the significance policy is spelled the SAME way, and in the same order, as in jmvtab
  expect_identical(opt_values(o, "color_signif"),
                   opt_values(yaml_opts("jmvtab.a.yaml"), "color_signif"))
})


# --- 3. THE NAME RULE (Phase 20g-i) --------------------------------------------------------
# An option is named after the producer ARGUMENT it drives: exactly, or as `<argument>_<slot>` when
# several options fold into one (`ci_method_cell` ... -> `ci_method`; `stats_compare` /
# `stats_baseline` / `stats_checks` -> `stats`; `ref` + `ref_levels` -> `ref`). Anything else must be
# declared HERE with its reason -- which is the whole list of things the panel asks that `tab()` /
# `tab_reg()` do not.
JMV_UI_ONLY <- c(
  data            = "the jamovi dataset, not an argument",
  wrap_rows       = "tab_html() / the renderer, not the producer",
  wrap_cols       = "tab_html() / the renderer, not the producer",
  theme           = "tab_html() / tab_xl() -- the renderer, not the producer",
  export_format   = "the export block (R/jmvtab-export.R)",
  export_dir      = "the export block",
  export_filename = "the export block",
  exportExcel     = "the export block: an Action button",
  resetPath       = "the export block: an Action button",
  xl_replace      = "the export block: number the file instead of overwriting"
)
JMV_UI_ONLY_EXTRA <- list(
  jmvtab = c(
    lvs = "`levels`, renamed: jmvcore::Options already defines a levels() method"
  ),
  jmvtabreg = c(
    models      = "the model-comparison builder; folded into `predictors` by jmvtab_reg_models()",
    crosses     = "the interaction picker; folded into `predictors` as `a*b` keys (22b-ix)",
    run_compare = "an Action button: the staged-comparison trigger",
    xl_check    = paste("tab_xl(check =): the model-check plots, an EXPORT argument -- and it",
                        "cannot be named `check`, a jmvcore::Options method (as `lvs` cannot be",
                        "`levels`)")
  )
)

test_that("every jamovi option is named after the argument it drives", {
  for (an in c("jmvtab", "jmvtabreg")) {
    o        <- yaml_opts(paste0(an, ".a.yaml"))
    pname    <- if (an == "jmvtab") "tab" else "tab_reg"
    producer <- get(pname, envir = asNamespace("tabxplor"))
    # ⚠ Phase 20g-ii: the INTERNAL dot-arguments count too, de-dotted. They are declared in TAB_ARGS
    # and ride `...`, so they are not formals -- `levels_order` passed this rule only because tab()
    # happens to have a `levels` formal, i.e. by coincidence, and `levels_collapse` on tab_reg()
    # (which has no such formal) is what exposed it. Reading the declaration makes both pass by
    # INTENT, and makes a rename of `tab(levels =)` unable to silently remove the justification.
    # ⚠ ...and so do the PUBLIC arguments that ride `...`, which the same table declares
    # (`dots = <producer>`): 22g-ii moved `ci_method` off tab_reg()'s signature onto its dots, and it
    # is no less reachable for it.
    declared <- tabxplor:::tab_args_for(pname)
    on_dots  <- declared[vapply(declared, function(k)
      pname %in% (tabxplor:::TAB_ARGS[[k]][["dots"]] %||% character()), logical(1))]
    args     <- unique(c(setdiff(names(formals(producer)), c("...", "")), on_dots,
                         sub("^\\.", "", grep("^\\.", declared, value = TRUE))))
    allowed  <- c(names(JMV_UI_ONLY), names(JMV_UI_ONLY_EXTRA[[an]]))
    for (nm in names(o)) {
      ok <- nm %in% args || any(startsWith(nm, paste0(args, "_"))) || nm %in% allowed
      expect_true(ok, info = paste0(
        an, ".a.yaml: option `", nm, "` is neither an argument of ",
        if (an == "jmvtab") "tab()" else "tab_reg()",
        ", nor `<argument>_<slot>`, nor a declared UI-only control (JMV_UI_ONLY)."))
    }
  }
})


# --- 4. NOTHING NAMES SOMETHING UNDECLARED --------------------------------------------------
# A half-done rename leaves a control or a `ui.<name>` pointing at an option that no longer exists,
# and jamovi fails SILENTLY there (a control with no option renders inert; `ui.gone` is undefined,
# and every CustomControl guards with `if (!ui.x) return;`). Both halves are checked here.
test_that("the .u.yaml controls and the .js name declared options", {
  skip_if_not_installed("yaml")
  for (an in c("jmvtab", "jmvtabreg")) {
    o   <- yaml_opts(paste0(an, ".a.yaml"))
    ui  <- yaml::read_yaml(testthat::test_path("..", "..", "jamovi", paste0(an, ".u.yaml")))

    # every `name:` / `optionName:` in the control tree
    ctrl_names <- character(0); bound <- character(0)
    walk <- function(node) {
      if (!is.list(node)) return(invisible(NULL))
      if (!is.null(node$name))       ctrl_names <<- c(ctrl_names, as.character(node$name))
      if (!is.null(node$optionName)) bound      <<- c(bound, as.character(node$optionName))
      for (el in node) if (is.list(el)) walk(el)
      invisible(NULL)
    }
    walk(ui)
    # an `optionName:` ALWAYS names an option (that is what it is for)
    for (nm in unique(bound))
      expect_true(nm %in% names(o),
                  info = paste0(an, ".u.yaml: optionName `", nm, "` is not a declared option"))

    # the hand-written .js may only reach for a declared option, a control, or the root view
    js  <- readLines(testthat::test_path("..", "..", "jamovi", "js", paste0(an, ".js")), warn = FALSE)
    hit <- unique(unlist(regmatches(js, gregexpr("(?<=\\bui\\.)[A-Za-z_][A-Za-z0-9_]*", js,
                                                 perl = TRUE))))
    # Phase 20h: ...and by BRACKET access, which the `ui.<name>` regex above cannot see. A rename of
    # `totaltab_*` / `comp` / `xl_replace` / `family` / `trials` would have no-op'd in SILENCE, since
    # every CustomControl guards with `if (!ui.x) return;`. The three forms are derived from the
    # sources' own convention, never from a hand-kept list.
    hit <- unique(c(hit, ui_bracket_names(js)))
    known <- c(names(o), ctrl_names, "view")
    for (nm in hit)
      expect_true(nm %in% known,
                  info = paste0("jamovi/js/", an, ".js: `", nm,
                                "` names neither an option nor a control (a stale rename?)"))
  }
})


# ⚠ A `.js` map from a RADIO NAME to the value it sets must agree with the `.u.yaml` that names those
# radios -- and nothing else can see it. Phase 22g-iii re-ordered `measure` AND renamed one of its
# values, which moved every pair in `MEASURE_OF_RADIO`; the value-coverage test above stayed green
# (the yaml still declares five values and offers five buttons) while `applyModelEnables()` greyed
# the wrong button. The maps are the JS's own object literals, read here rather than listed.
test_that("the .js radio maps agree with the .u.yaml that names those radios", {
  skip_if_not_installed("yaml")
  for (an in c("jmvtab", "jmvtabreg")) {
    ui <- yaml::read_yaml(testthat::test_path("..", "..", "jamovi", paste0(an, ".u.yaml")))
    js <- paste(readLines(testthat::test_path("..", "..", "jamovi", "js", paste0(an, ".js")),
                          warn = FALSE), collapse = "\n")
    # every RadioButton, as control-name -> the value it sets
    declared <- list()
    walk <- function(node) {
      if (!is.list(node)) return(invisible(NULL))
      if (identical(node$type, "RadioButton") && !is.null(node$name) && !is.null(node$optionPart))
        declared[[as.character(node$name)]] <<- as.character(node$optionPart)
      for (el in node) if (is.list(el)) walk(el)
      invisible(NULL)
    }
    walk(ui)
    # `var NAME_OF_RADIO = { ctrl: "value", ... };` -- the convention both files follow
    for (m in regmatches(js, gregexpr("var\\s+\\w+_OF_RADIO\\s*=\\s*\\{[^}]*\\}", js, perl = TRUE))[[1]]) {
      nm <- sub("^var\\s+(\\w+)\\s*=.*$", "\\1", m)
      kv <- regmatches(m, gregexpr("(\\w+)\\s*:\\s*\"([^\"]*)\"", m, perl = TRUE))[[1]]
      for (p in kv) {
        ctrl <- trimws(sub(":.*$", "", p))
        val  <- sub("^.*\"([^\"]*)\"$", "\\1", p)
        expect_identical(declared[[ctrl]], val, info = paste0(
          "jamovi/js/", an, ".js: ", nm, "$", ctrl, " = \"", val,
          "\", but ", an, ".u.yaml gives that radio optionPart \"",
          declared[[ctrl]] %||% "<no such control>", "\""))
      }
    }
  }
})


# The hand-written .js PARSES. It ships verbatim to every user and a syntax error makes the whole
# options panel inert with no R-side symptom, so this is worth the 40 ms -- and it is now possible:
# the box has node (it was declined in 19n only because there was none). Skipped where there is not.
test_that("the jamovi .js files are syntactically valid", {
  skip_on_cran()
  node <- Sys.which("node")
  skip_if(!nzchar(node), "node is not on the PATH")
  for (an in c("jmvtab", "jmvtabreg")) {
    f <- testthat::test_path("..", "..", "jamovi", "js", paste0(an, ".js"))
    skip_if_not(file.exists(f), "jamovi/ is not shipped in a built package")
    out <- suppressWarnings(system2(node, c("--check", shQuote(f)), stdout = TRUE, stderr = TRUE))
    expect_identical(attr(out, "status") %||% 0L, 0L, info = paste(c(an, out), collapse = "\n"))
  }
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

    # ...AND covers every one of them. ⚠ this is not tidiness: the ui compiler APPENDS a control for
    # each value a radio group leaves out and then REWRITES the .u.yaml with yaml.dump(), which
    # deletes every comment in it. Measured on `jmvtabreg`'s `display` (7 of 10 offered), whose three
    # orphans had been declared three phases earlier and never given a button.
    for (nm in unique(vapply(pairs, `[[`, character(1), 1L))) {
      offered <- unlist(lapply(pairs[vapply(pairs, function(p) p[[1]] == nm, logical(1))],
                               `[[`, 2L))
      expect_setequal(offered, opt_values(opts, nm))
    }
  }
})
