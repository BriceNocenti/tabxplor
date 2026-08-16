# verify_tab_args.R -- prove that a change to the ARGUMENT SURFACE changed no resolved value and no
# message a user reads.
#
# WHY THIS FILE EXISTS. Phase 20b moves nine deprecated formals of `tab()` and four internal dot-args
# into `...`, gives the three superseded producers a `...` of their own, and turns ~15 argument
# vocabularies into generated documentation. Every one of those edits is invisible to the goldens:
# they lock CELLS, and an argument boundary can resolve a different value, or say a different thing
# while refusing one, without a single cell moving. `dev/verify_reg_specs.R` learned the same lesson
# on the regression side, which is why it dumps the MESSAGES IN ORDER as well as the specs.
#
# THREE CAPTURES, and the third is the one 20b needs most:
#   resolve   tab_resolve_common_args() (R/tab-resolve.R) over a grid -- the ONE point where every
#             crosstab producer's arguments are validated and derived, so its return IS the boundary.
#   columns   what a BUILT table stores: per fmt column the 16 attributes (fmt_attrs_of()), the
#             distinct `display` and `row_kind` values, and the table-level schema (meta sub-fields,
#             the `test` tibble's columns). An argument that survives resolution but stops reaching a
#             column shows up here and nowhere else.
#   messages  the abort / deprecation text of deliberately invalid and deliberately legacy calls,
#             captured IN ORDER. Moving a formal into `...` changes "unused argument" into a
#             lifecycle warning (or the reverse) with no other visible effect -- that is the class of
#             regression this capture exists to catch.
#
# HOW TO USE IT:
#   Rscript dev/verify_tab_args.R save  <file.rds>   # on the pre-change tree
#   Rscript dev/verify_tab_args.R check <file.rds>   # after -- must print "IDENTICAL"
#   Rscript dev/verify_tab_args.R probe              # a readable dump of today's boundary
#
# WHAT IT DOES NOT CAPTURE: rendering (that is _snaps/), per-cell values (test-golden.R +
# dev/verify_golden_field_delta.R) and the colour resolver (dev/verify_color_attrs.R). Run those too.

suppressMessages(devtools::load_all("~/github/tabxplor", quiet = TRUE))

# DETERMINISM, and both halves were learned the hard way:
#  * lifecycle's default verbosity warns only ONCE per session per call site, so whether a deprecation
#    appears would depend on what ran before it. Pinned to "warning": every deprecated path speaks.
#  * cli renders its bullets as UNICODE or ASCII depending on the locale ("i" under LC_ALL=C, "ℹ"
#    otherwise), so a baseline saved in one shell and checked in another reported every message as
#    CHANGED. Pinned to ASCII + no colour, so the capture is a function of the CODE alone.
options(lifecycle_verbosity = "warning", cli.unicode = FALSE, cli.num_colors = 1)

`%||%` <- function(x, y) if (is.null(x)) y else x

# =================================================================================================
# 1. the resolver grid
# =================================================================================================
# A baseline call plus ONE override per (argument, value) -- the shape that makes a diff readable:
# a changed cell names the argument and the value that moved. Then the interactions that are known
# to be load-bearing (the colour spec's decode-then-normalise order, D28's ci/stars/signif gate, the
# retired `OR` route, `tot`'s expansion).
resolver_cases <- function() {
  base <- list(fn = "tab", test = FALSE, color = TRUE, color_signif = "ignore", ci = "no",
               stars = FALSE, conf_level = 0.95, cleannames = FALSE, ref = "auto", ref2 = "first",
               tot = "both", total_names = "Total", na = "keep", levels = "all", pct = "row",
               comp = "tab", totaltab = "line", totcol = "last", output = "single", n_min = 0)

  cases <- list(base = base)
  add <- function(nm, ...) cases[[nm]] <<- utils::modifyList(base, list(...))

  # one override per declared vocabulary value
  for (a in names(TAB_ARG_VALUES)) for (v in TAB_ARG_VALUES[[a]]$values) {
    if (is.na(v)) next
    cases[[paste0(a, ".", v)]] <- utils::modifyList(base, stats::setNames(list(v), a))
  }
  # the leaf vocabularies are narrower -- the same overrides, seen through a leaf `fn`
  for (fn in c("tab_plain", "tab_num", "tab_counts")) {
    cases[[paste0("fn.", fn)]] <- utils::modifyList(base, list(fn = fn, na = "keep"))
  }

  # the axes with no TAB_ARG_VALUES row (validated elsewhere, or not at all before 19i)
  for (v in c(0.9, 0.99)) add(paste0("conf_level.", v), conf_level = v)
  for (v in c(0, 30))     add(paste0("n_min.", v), n_min = v)
  for (v in c(TRUE, FALSE)) add(paste0("test.", v), test = v)
  for (v in c(TRUE, FALSE)) add(paste0("cleannames.", v), cleannames = v)
  for (v in c(TRUE, FALSE)) add(paste0("stars.", v), stars = v)
  add("total_names.2", total_names = c("Total", "Ensemble"))

  # the colour spec: measure x policy x the ci gate (D28) x stars
  for (co in list(TRUE, FALSE, "difference", "ratio", "odds_ratio", "contrib", "no",
                  c("difference", "ratio"), c("", "difference"), c(pct = "ratio"),
                  "diff_ci", "after_ci", "ci")) {
    for (sg in c("ignore", "grey_non_signif", "guaranteed_effect")) {
      for (ci in c("no", "cell", "ref")) {
        nm <- paste("color", paste(co, collapse = "+"), sg, ci, sep = ".")
        cases[[nm]] <- utils::modifyList(base, list(color = co, color_signif = sg, ci = ci,
                                                    stars = TRUE))
      }
    }
  }
  # the retired `OR` route: display / ref2 / ref
  for (o in c("no", "OR", "OR_pct", "cumOR")) add(paste0("OR.", o), OR = o)
  add("OR.with_display", OR = "OR", display = "{pct}")
  add("OR.with_ref2",    OR = "OR", ref2 = "cumulative")
  # the renamed argument
  add("chi2.TRUE",  chi2 = TRUE)
  add("chi2.FALSE", chi2 = FALSE)
  # ci_method, in every accepted shape
  add("ci_method.scalar", ci_method = "wald")
  add("ci_method.named",  ci_method = c(cell = "wilson", diff = "wald"))
  add("ci_method.legacy", method_cell = "wilson", method_diff = "wald")
  cases
}

run_resolver <- function() {
  cases <- resolver_cases()
  lapply(cases, function(a) {
    out <- withCallingHandlers(
      tryCatch(do.call(tab_resolve_common_args, a), error = function(e) paste0("<ERROR: ", conditionMessage(e), ">")),
      warning = function(w) invokeRestart("muffleWarning"),
      message = function(m) invokeRestart("muffleMessage"))
    # the color spec holds closures in no case, but sort the names so a list re-order is not a diff
    if (is.list(out)) out <- out[order(names(out))]
    out
  })
}

# =================================================================================================
# 2. the built tables
# =================================================================================================
tab_cases <- function() {
  gss <- forcats::gss_cat
  gss$tv <- gss$tvhours
  cnt <- as.data.frame(table(gss$marital, gss$race), stringsAsFactors = TRUE)
  out <- list()
  add <- function(nm, f) out[[nm]] <<- f

  for (p in c("no", "row", "col", "all")) local({
    pp <- p; add(paste0("tab.pct.", pp), function() tab(gss, marital, race, pct = pp))
  })
  for (v in c("keep", "drop", "drop_all", "common_base")) local({
    vv <- v; add(paste0("tab.na.", vv), function() tab(gss, marital, denom, pct = "row", na = vv))
  })
  for (v in c("row", "col", "both", "no")) local({
    vv <- v; add(paste0("tab.tot.", vv), function() tab(gss, marital, race, pct = "row", tot = vv))
  })
  for (v in c("line", "table", "no")) local({
    vv <- v; add(paste0("tab.totaltab.", vv),
                 function() tab(gss, marital, race, year, pct = "row", totaltab = vv))
  })
  # `totcol` lives on tab_many() only -- tab() and the leaves say `tot`
  for (v in c("last", "each", "all_col_vars", "no")) local({
    vv <- v; add(paste0("many.totcol.", vv),
                 function() tab_many(gss, marital, race, pct = "row", totcol = vv))
  })
  for (v in c("tab", "all")) local({
    vv <- v; add(paste0("tab.comp.", vv),
                 function() tab(gss, marital, race, year, pct = "row", comp = vv))
  })
  for (v in c("no", "cell", "ref")) local({
    vv <- v; add(paste0("tab.ci.", vv), function() tab(gss, marital, race, pct = "row", ci = vv))
  })
  for (v in c("all", "first")) local({
    vv <- v; add(paste0("tab.levels.", vv),
                 function() tab(gss, marital, race, pct = "row", levels = vv))
  })
  for (v in c(0.9, 0.95, 0.99)) local({
    vv <- v; add(paste0("tab.conf_level.", vv),
                 function() tab(gss, marital, race, pct = "row", ci = "ref", conf_level = vv))
  })
  add("tab.ci_method", function()
    tab(gss, marital, race, pct = "row", ci = "ref", ci_method = c(cell = "wilson", diff = "wald")))
  add("tab.stars",      function() tab(gss, marital, race, pct = "row", stars = TRUE, test = TRUE))
  add("tab.test",       function() tab(gss, marital, race, pct = "row", test = TRUE))
  add("tab.anova",      function() tab(gss, marital, c(age, tv), test = TRUE, anova = "classic"))
  add("tab.n_min",      function() tab(gss, marital, race, pct = "row", n_min = 400))
  add("tab.cleannames", function() tab(gss, marital, race, pct = "row", cleannames = TRUE))
  add("tab.total_names", function()
    tab(gss, marital, race, pct = "row", total_names = c("Ens.", "Tot.")))
  add("tab.display",    function() tab(gss, marital, race, pct = "row", display = "{pct} ({n})"))
  add("tab.ref_first",  function() tab(gss, marital, race, pct = "row", ref = "first"))
  add("tab.ref2",       function() tab(gss, marital, race, pct = "row", ref2 = "cumulative"))
  add("tab.wt",         function() tab(gss, marital, race, wt = tvhours, pct = "row"))
  add("tab.output_list", function()
    tab(gss, c(marital, relig), race, pct = "row", output_list = TRUE))
  add("tab.spread",     function() tab(gss, marital, race, year, pct = "row", spread_vars = year))
  add("tab.add_n_pct",  function() tab(gss, marital, race, pct = "row", add_n = TRUE, add_pct = TRUE))

  add("plain.base",  function() tab_plain(gss, marital, race, pct = "row"))
  add("plain.ci",    function() tab_plain(gss, marital, race, pct = "row", ci = "ref"))
  add("plain.tot",   function() tab_plain(gss, marital, race, pct = "row", tot = "row"))
  add("num.base",    function() tab_num(gss, marital, c(age, tv)))
  add("num.comp",    function() tab_num(gss, marital, c(age, tv), comp = "all"))
  add("num.ci",      function() tab_num(gss, marital, c(age, tv), ci = "ref"))
  add("counts.base", function() tab_counts(cnt, Var1, Var2, counts = Freq, pct = "row"))
  add("counts.tot",  function() tab_counts(cnt, Var1, Var2, counts = Freq, pct = "row", tot = "row"))
  add("many.two",    function() tab_many(gss, c(marital, relig), race, pct = "row"))
  out
}

capture_tab <- function(x) {
  if (is.list(x) && !is.data.frame(x)) return(lapply(x, capture_tab))
  fmt <- names(x)[vapply(x, is_fmt, logical(1))]
  cols <- lapply(rlang::set_names(fmt), function(cn) {
    col <- x[[cn]]
    c(fmt_attrs_of(col),
      list(display = sort(unique(as.character(get_display(col)))),
           row_kind = sort(unique(as.character(get_row_kind(col))))))
  })
  tt <- attr(x, "test")
  list(columns  = cols,
       index    = setdiff(names(x), fmt),
       meta     = sort(names(attr(x, "meta") %||% list())),
       spec     = sort(names((attr(x, "meta") %||% list())$spec %||% list())),
       test     = if (is.null(tt)) NULL else list(cols = names(tt), n = nrow(tt),
                                                  kinds = sort(unique(as.character(tt$test)))),
       subtext  = attr(x, "subtext"),
       class    = class(x))
}

run_tabs <- function() {
  cases <- tab_cases()
  lapply(cases, function(f)
    tryCatch(suppressWarnings(suppressMessages(capture_tab(f()))),
             error = function(e) paste0("<ERROR: ", conditionMessage(e), ">")))
}

# =================================================================================================
# 3. the messages
# =================================================================================================
# Every condition a call raises, in the order it raises it: the class, then the text. This is the
# capture a signature change reorders silently.
message_cases <- function() {
  gss <- forcats::gss_cat
  list(
    bad_pct        = function() tab(gss, marital, race, pct = "rows"),
    bad_na         = function() tab(gss, marital, race, na = "dropp"),
    bad_totaltab   = function() tab(gss, marital, race, totaltab = "tabel"),
    # `totcol` is a tab_many() argument -- tab() and the leaves have no such formal (they say `tot`)
    bad_totcol     = function() tab_many(gss, marital, race, totcol = "every"),
    bad_comp       = function() tab(gss, marital, race, comp = "table"),
    bad_levels     = function() tab(gss, marital, race, levels = "firsts"),
    bad_tot        = function() tab(gss, marital, race, tot = "rows"),
    bad_conf_pct   = function() tab(gss, marital, race, conf_level = 95),
    bad_conf_neg   = function() tab(gss, marital, race, conf_level = -1),
    bad_n_min      = function() tab(gss, marital, race, n_min = -3),
    bad_anova      = function() tab(gss, marital, c(age), anova = "welsh"),
    bad_ci         = function() tab(gss, marital, race, ci = "difference"),
    bad_color      = function() tab(gss, marital, race, color = "rainbow"),
    bad_signif     = function() tab(gss, marital, race, color = TRUE, color_signif = "grey"),
    bad_ci_method  = function() tab(gss, marital, race, ci = "ref", ci_method = c(cell = "clopper")),
    bad_display    = function() tab(gss, marital, race, display = "{nope}"),
    dep_chi2       = function() tab(gss, marital, race, chi2 = TRUE),
    dep_OR         = function() tab(gss, marital, race, pct = "row", OR = "OR"),
    dep_ci_diff    = function() tab(gss, marital, race, pct = "row", ci = "diff"),
    dep_ci_ratio   = function() tab(gss, marital, race, pct = "row", ci = "ratio"),
    dep_sup_cols   = function() tab(gss, marital, race, sup_cols = denom),
    dep_row_var    = function() tab(gss, row_var = marital, col_var = race),
    # a NON-default value: resolve_ci_method() skips the deprecation when the legacy argument
    # happens to name the default ("wilson"), so testing it with the default proves nothing
    dep_methods    = function() tab(gss, marital, race, ci = "ref", method_cell = "beta"),
    dep_many_chi2  = function() tab_many(gss, marital, race, chi2 = TRUE),
    dep_many_pos6  = function() tab_many(gss, marital, race, NULL, NULL, "row"),
    dep_transpose  = function() tab_transpose(tab(gss, marital, race, pct = "row")),
    leaf_bad_na    = function() tab_plain(gss, marital, race, na = "drop_all"),
    leaf_bad_ci    = function() tab_num(gss, marital, age, ci = "ratio"),
    counts_design  = function() tab_counts(data.frame(a = 1, b = 2, n = 3), a, b, counts = n,
                                           ci_method = c(mean_diff = "welch")),
    unused_arg     = function() tab(gss, marital, race, nosuchargument = 1)
  )
}

run_messages <- function() {
  lapply(message_cases(), function(f) {
    seen <- character(0)
    res <- withCallingHandlers(
      tryCatch({ invisible(f()); "<ok>" },
               error = function(e) paste0("ERROR|", class(e)[[1]], "|", conditionMessage(e))),
      warning = function(w) {
        seen <<- c(seen, paste0("WARN|", class(w)[[1]], "|", conditionMessage(w)))
        invokeRestart("muffleWarning")
      },
      message = function(m) {
        seen <<- c(seen, paste0("MSG|", class(m)[[1]], "|", trimws(conditionMessage(m))))
        invokeRestart("muffleMessage")
      })
    c(seen, res)                     # IN ORDER: every condition, then the outcome
  })
}

# =================================================================================================
# main
# =================================================================================================
args <- commandArgs(trailingOnly = TRUE)
mode <- if (length(args) >= 1) args[[1]] else "check"
path <- if (length(args) >= 2) args[[2]] else "/tmp/tab_args.rds"

got <- list(resolve = run_resolver(), columns = run_tabs(), messages = run_messages())
errs <- names(got$columns)[vapply(got$columns, is.character, logical(1))]
cat("resolver:", length(got$resolve), " tables:", length(got$columns),
    " (errors:", length(errs), ") messages:", length(got$messages), "\n")
if (length(errs)) for (e in errs) cat("   - ", e, ": ", got$columns[[e]], "\n", sep = "")

if (identical(mode, "probe")) {
  cat("\n-- resolver, base case --\n"); utils::str(got$resolve$base, max.level = 2)
  cat("\n-- messages --\n")
  for (nm in names(got$messages))
    cat(sprintf("%-14s %s\n", nm, paste(substr(got$messages[[nm]], 1, 110), collapse = " || ")))
  quit(save = "no")
}

if (identical(mode, "save")) {
  saveRDS(got, path, version = 2)
  cat("saved to", path, "\n")
} else {
  ref <- readRDS(path)
  bad <- 0L
  for (part in c("resolve", "columns", "messages")) {
    a <- ref[[part]]; z <- got[[part]]
    gone <- setdiff(names(a), names(z)); new <- setdiff(names(z), names(a))
    common <- intersect(names(a), names(z))
    ch <- common[!vapply(common, function(k) identical(a[[k]], z[[k]]), logical(1))]
    if (length(gone)) { cat("[", part, "] MISSING: ", paste(gone, collapse = ", "), "\n", sep = "") }
    if (length(new))  { cat("[", part, "] NEW    : ", paste(new,  collapse = ", "), "\n", sep = "") }
    bad <- bad + length(gone) + length(new) + length(ch)
    for (k in utils::head(ch, 25L)) {
      cat("[", part, "] CHANGED ", k, "\n", sep = "")
      if (part == "messages") {
        cat("      was: ", paste(substr(a[[k]], 1, 100), collapse = " || "), "\n",
            "      now: ", paste(substr(z[[k]], 1, 100), collapse = " || "), "\n", sep = "")
      } else {
        fa <- a[[k]]; fz <- z[[k]]
        if (!is.list(fa) || !is.list(fz)) {
          cat("      ", paste(utils::head(unlist(fa), 8), collapse = ","), " -> ",
              paste(utils::head(unlist(fz), 8), collapse = ","), "\n", sep = "")
        } else for (f in union(names(fa), names(fz))) {
          if (identical(fa[[f]], fz[[f]])) next
          cat("      $", f, ": ", paste(utils::head(unlist(fa[[f]]), 8), collapse = ","),
              "  ->  ", paste(utils::head(unlist(fz[[f]]), 8), collapse = ","), "\n", sep = "")
        }
      }
    }
  }
  if (bad == 0L) cat("IDENTICAL -- every resolved argument, stored attribute and message matches.\n")
  else cat("CHANGED in", bad, "case(s).\n")
}
