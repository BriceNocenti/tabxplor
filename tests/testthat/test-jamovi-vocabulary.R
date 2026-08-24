
# The jamovi module speaks tabxplor's OWN vocabulary -- there is no translation layer left between a
# control and the argument it names. This file checks the STRUCTURE of that wiring, i.e. the things
# that break SILENTLY: a control bound to an option that no longer exists renders inert, a `ui.gone`
# is undefined, a renamed CustomControl never runs any code, a syntax error kills the whole panel,
# and a radio group missing one value makes the ui compiler REWRITE the .u.yaml and delete every
# comment in it. None of that raises anything an R-side test would see.
#
#   1. every option NAME is the producer argument it drives (Phase 20g-i);
#   2. every `.u.yaml` control and every `ui.<name>` in the hand-written `.js` names something the
#      `.a.yaml` or the `.u.yaml` declares;
#   3. the `.js` radio maps agree with the `.u.yaml` that names those radios;
#   4. both `.js` files parse, and their generated blocks are what the R tables would write today;
#   5. a radio group covers every value of its option (see its own ⚠);
#   6. every CustomControl is wired to handlers its `.js` exports (Phase 22g-iv).
#
# ⚠ What is deliberately NOT here (removed in Phase 22g-iv): any assertion that a List option's VALUE
# SET equals an R vocabulary, in content or in order. A panel chooses which values to offer and how
# to order them for a reader -- `no` last, simple before complex -- and pinning that to the R
# declaration made every ordinary UI edit a test failure while catching nothing a user would meet.
# Name checks stay; value checks are gone.

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
  tab_theme       = paste("tab_html()/tab_xl()'s `theme`, renamed: jamovi injects its own",
                          "global `theme` option into every analysis"),
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
    levels_order = paste("jmvtabreg-only: `tab_reg()` has no such argument -- it relevels the",
                         "predictor columns in jmvtab_reg_build(), before the fit"),
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


# ⚠ NOT a vocabulary check: the ui COMPILER appends a RadioButton for every value a radio group
# leaves out and then REWRITES the .u.yaml with yaml.dump(), which deletes every comment in it.
# Measured on `jmvtabreg`'s `display` (7 of 10 offered), whose three orphans had been declared three
# phases earlier and never given a button. So this asserts COVERAGE only -- never which values a
# panel offers, nor in what order.
test_that("a radio group covers every value of the option it writes", {
  skip_if_not_installed("yaml")
  # ⚠ YAML 1.1 reads a bare `no` / `yes` / `on` / `off` as a BOOLEAN, so an optionPart spelled
  # `no` arrives as FALSE. Spell it back rather than demanding the .u.yaml quote it.
  spell <- function(x) if (is.logical(x)) c("no", "yes")[x + 1L] else as.character(x)
  for (an in c("jmvtab", "jmvtabreg")) {
    opts <- yaml_opts(paste0(an, ".a.yaml"))
    ui   <- yaml::read_yaml(testthat::test_path("..", "..", "jamovi", paste0(an, ".u.yaml")))
    pairs <- list()
    walk <- function(node) {
      if (!is.list(node)) return(invisible(NULL))
      if (!is.null(node$optionName) && !is.null(node$optionPart))
        pairs[[length(pairs) + 1L]] <<- c(as.character(node$optionName), spell(node$optionPart))
      for (el in node) if (is.list(el)) walk(el)
      invisible(NULL)
    }
    walk(ui)
    for (nm in unique(vapply(pairs, `[[`, character(1), 1L))) {
      offered <- unlist(lapply(pairs[vapply(pairs, function(p) p[[1]] == nm, logical(1))], `[[`, 2L))
      expect_setequal(offered, opt_values(opts, nm))
    }
  }
})


# Phase 22g-iv: a CustomControl is wired in TWO files -- the `.u.yaml` declares it and names its
# `creating` / `updated` handlers, the `.js` exports them. A rename done in one and not the other is
# SILENT: jamovi renders an empty box, and no R code runs at all. So the two halves are checked
# against each other, in both directions, derived from the files rather than from a list of names.
test_that("every CustomControl is wired to handlers its .js exports", {
  skip_if_not_installed("yaml")
  for (an in c("jmvtab", "jmvtabreg")) {
    p <- testthat::test_path("..", "..", "jamovi", paste0(an, ".u.yaml"))
    f <- testthat::test_path("..", "..", "jamovi", "js", paste0(an, ".js"))
    skip_if_not(file.exists(p) && file.exists(f), "jamovi/ is not shipped in a built package")
    ui <- yaml::read_yaml(p)

    ctrls <- character(0)
    walk <- function(node) {
      if (!is.list(node)) return(invisible(NULL))
      if (identical(node$type, "CustomControl") && !is.null(node$name))
        ctrls <<- c(ctrls, as.character(node$name))
      for (el in node) if (is.list(el)) walk(el)
      invisible(NULL)
    }
    walk(ui)
    ctrls <- unique(ctrls)
    expect_true(length(ctrls) > 0L, info = an)

    js <- paste(readLines(f, warn = FALSE), collapse = "\n")
    # the handler names the .js exports, i.e. `    <name>_creating:` in module.exports
    exported <- gsub("[^A-Za-z0-9_]", "",
                     regmatches(js, gregexpr("\\n\\s{4}\\w+_(creating|updated)\\s*:", js,
                                             perl = TRUE))[[1]])
    for (nm in ctrls) for (ev in c("creating", "updated"))
      expect_true(paste0(nm, "_", ev) %in% exported,
                  info = paste0(an, ": ", nm, "_", ev, " is declared in the .u.yaml but not",
                                " exported by the .js"))
    # `view_updated` is the ROOT view's jus-3.0 alias, not a control handler.
    for (h in setdiff(exported, "view_updated"))
      expect_true(sub("_(creating|updated)$", "", h) %in% ctrls,
                  info = paste0(an, ": the .js exports ", h,
                                " but no CustomControl of that name is declared"))
  }
})
