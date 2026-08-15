# verify_color_attrs.R -- prove that a refactor of the COLOUR RESOLVER changed nothing a reader sees.
#
# WHY THIS FILE EXISTS. `tab_resolve_settings()`'s four sub-passes (color_diff_OR / color_ctr /
# color_ci / color_num) are asserted by NO test: `color_ctr`, `color_ci` and `color_num` appear
# nowhere in tests/, and `color_diff_OR` only as a NAME in one ctx-field list
# (test-carve-parity.R). The `_color_golden` fixtures lock the per-cell HEX of 15 cases, which is a
# narrow slice of the argument space. So the only real guard on Phase 19c (KEY 4) is a
# characterization dump: build a wide matrix of colour arguments and record, per built column, every
# stored fact the colour engine and the legend read.
#
# HOW TO USE IT, around a colour-resolver refactor:
#   Rscript dev/verify_color_attrs.R save <file.rds>      # on the pre-refactor tree
#   Rscript dev/verify_color_attrs.R check <file.rds>     # after -- must print "IDENTICAL"
#   Rscript dev/verify_color_attrs.R probe                # what the resolver returns, per case
#
# WHAT IT CAPTURES, per (case x fmt column):
#   the four colour attributes (color / color_bg / color_signif) + the two 19b ones the resolver can
#   move (scale / ci_method) + the RESOLVED per-cell slot integers of both channels
#   (fmt_color_channels) -- the engine's actual output, one step before the palette. Plus, per case,
#   the resolver's own returned settings, so a change in the cascade shows up even when it happens
#   not to reach a column.
#
# WHAT IT DOES NOT CAPTURE: rendering (that is _snaps/golden.md + _snaps/render-html.md) and the
# per-cell hex (that is _color_golden). Those two run as usual from the test suite.

suppressMessages(devtools::load_all("~/github/tabxplor", quiet = TRUE))

# --- the matrix -------------------------------------------------------------------------------
# Kept deterministic and small-N: forcats::gss_cat, the same fixture every other golden uses.
color_attr_cases <- function() {
  gss <- forcats::gss_cat
  # one small numeric column so the mean arm is exercised without dragging tvhours' NAs everywhere
  gss$tv <- gss$tvhours

  colors <- list(
    lgl_true   = TRUE,
    lgl_false  = FALSE,
    diff       = "diff",
    ratio      = "ratio",
    contrib    = "contrib",
    OR         = "OR",
    or_lower   = "or",
    diff_ci    = "diff_ci",
    after_ci   = "after_ci",
    ci         = "ci",
    two_chan   = c("diff", "ratio"),
    bg_only    = c("", "diff"),
    by_type_v  = c(pct = "ratio"),
    by_type_l  = list(pct = c("diff", "ratio"), mean = "ratio")
  )
  signifs <- c("ignore", "grey_non_signif", "guaranteed_effect")

  out <- list()
  add <- function(nm, expr) out[[nm]] <<- expr

  # --- factor tables: color x color_signif x pct -------------------------------------------
  for (cn in names(colors)) for (sg in signifs) for (pc in c("row", "col", "all", "no")) {
    force(cn); force(sg); force(pc)
    local({
      cc <- colors[[cn]]; ss <- sg; pp <- pc
      add(paste("fct", cn, ss, pp, sep = "."),
          function() tab(gss, marital, race, pct = pp, color = cc, color_signif = ss))
    })
  }

  # --- factor tables: the ci axis (the forcing the policy and the measure drive) -----------
  for (cn in c("lgl_true", "diff", "ratio", "contrib", "OR")) for (ci in c("no", "cell", "diff", "ratio")) {
    force(cn); force(ci)
    local({
      cc <- colors[[cn]]; ii <- ci
      add(paste("ci", cn, ii, sep = "."),
          function() tab(gss, marital, race, pct = "row", color = cc, ci = ii))
    })
  }

  # --- the OR axis (ref2 / cumOR) + stars --------------------------------------------------
  for (orv in c("no", "OR", "OR_pct", "cumOR")) for (cn in c("lgl_true", "OR", "diff")) {
    force(orv); force(cn)
    local({
      oo <- orv; cc <- colors[[cn]]
      add(paste("or", oo, cn, sep = "."),
          function() tab(gss, marital, race, pct = "row", OR = oo, color = cc))
    })
  }
  for (st in c(TRUE, FALSE)) for (cn in c("lgl_true", "diff", "OR")) {
    force(st); force(cn)
    local({
      s <- st; cc <- colors[[cn]]
      add(paste("stars", s, cn, sep = "."),
          function() tab(gss, marital, race, pct = "row", color = cc, stars = s))
    })
  }

  # --- ref / comp / totrow ------------------------------------------------------------------
  for (rf in c("auto", "tot", "first", "last")) for (cn in c("lgl_true", "diff", "OR")) {
    force(rf); force(cn)
    local({
      r <- rf; cc <- colors[[cn]]
      add(paste("ref", r, cn, sep = "."),
          function() tab(gss, marital, race, pct = "row", color = cc, ref = r))
    })
  }
  add("comp.all.contrib", function()
    tab(gss, marital, race, tab_vars = year, pct = "row", color = "contrib", comp = "all"))
  add("comp.all.diff", function()
    tab(gss, marital, race, tab_vars = year, pct = "row", color = "diff", comp = "all"))
  add("totrow.off.diff", function()
    tab(gss, marital, race, pct = "row", color = "diff", tot = "col"))
  add("totrow.off.contrib", function()
    tab(gss, marital, race, pct = "row", color = "contrib", tot = "col"))

  # --- numeric-only and mixed tables --------------------------------------------------------
  for (cn in names(colors)) for (sg in signifs) {
    force(cn); force(sg)
    local({
      cc <- colors[[cn]]; ss <- sg
      add(paste("num", cn, ss, sep = "."),
          function() tab(gss, race, c(age, tv), color = cc, color_signif = ss))
    })
  }
  for (cn in c("lgl_true", "diff", "ratio", "by_type_l")) for (ci in c("no", "cell", "diff", "ratio")) {
    force(cn); force(ci)
    local({
      cc <- colors[[cn]]; ii <- ci
      add(paste("numci", cn, ii, sep = "."),
          function() tab(gss, race, c(age, tv), color = cc, ci = ii))
    })
  }
  for (cn in c("lgl_true", "diff", "contrib", "by_type_l")) {
    force(cn)
    local({
      cc <- colors[[cn]]
      add(paste("mixed", cn, sep = "."),
          function() tab(gss, marital, c(race, age), pct = "row", color = cc))
    })
  }
  # the direct leaf entry points (they resolve colour on their own path)
  add("tab_num.diff.grey", function()
    tab_num(gss, race, c(age, tv), comp = "all", color = "diff", color_signif = "grey_non_signif"))
  add("tab_num.auto", function() tab_num(gss, race, c(age, tv), comp = "all"))
  # Phase 19c defect fixtures -- both FAIL on the pre-phase tree (see resolve_color_auto_num()):
  # the first stored the composite "after_ci" and coloured NOTHING; the next three ABORTED with
  # "Unknown color measure" because the unresolved "auto" sentinel reached set_color().
  add("tab_num.auto.ci_diff",   function() tab_num(gss, race, c(age, tv), ci = "diff"))
  add("tab_num.auto.signif",    function()
    tab_num(gss, race, c(age, tv), ci = "diff", color_signif = "grey_non_signif"))
  add("tab.auto_str.signif",    function()
    tab(gss, marital, race, pct = "row", color = "auto", color_signif = "grey_non_signif"))
  add("tab.auto_str.plain",     function()
    tab(gss, marital, race, pct = "row", color = "auto"))
  add("tab_counts.true", function()
    tab_counts(as.data.frame(table(gss$marital, gss$race)), Var1, Var2, counts = Freq,
               pct = "row", color = TRUE))
  add("tab_counts.contrib", function()
    tab_counts(as.data.frame(table(gss$marital, gss$race)), Var1, Var2, counts = Freq,
               pct = "row", color = "contrib"))
  add("tab_many.two_rowvars", function()
    tab_many(gss, c(marital, partyid), race, pct = "row", color = TRUE))

  out
}

# --- capture ----------------------------------------------------------------------------------
capture_tab <- function(tab) {
  if (is.list(tab) && !is.data.frame(tab))
    return(purrr::map(tab, capture_tab))
  fmt_cols <- names(tab)[purrr::map_lgl(tab, is_fmt)]
  purrr::map(rlang::set_names(fmt_cols), function(cn) {
    col <- tab[[cn]]
    ch  <- tryCatch(fmt_color_channels(col), error = function(e) paste0("<err ", conditionMessage(e), ">"))
    list(color        = get_color(col),
         color_bg     = get_color_bg(col),
         color_signif = get_color_signif(col),
         scale        = get_scale(col),
         ci_method    = get_ci_method(col),
         slots        = ch)
  })
}

run_cases <- function() {
  cases <- color_attr_cases()
  purrr::imap(cases, function(f, nm) {
    tryCatch(suppressWarnings(suppressMessages(capture_tab(f()))),
             error = function(e) paste0("<ERROR: ", conditionMessage(e), ">"))
  })
}

# --- the resolver's own output, per case (the cascade, independent of any column) ------------
probe_resolver <- function() {
  # ONLY the values a public entry point can actually deliver. Every caller hands tab_build() a
  # `color_spec$legacy`, and legacy_union() emits exactly these five (17d decoded the combined
  # strings at the boundary, so "diff_ci"/"after_ci"/"ci" can no longer arrive from outside).
  # Feeding the resolver anything else measures dplyr::recode()'s pass-through, not the cascade.
  grid <- tidyr::expand_grid(
    color  = c("auto", "diff", "contrib", "OR", "no"),
    signif = c("ignore", "grey_non_signif", "guaranteed_effect"),
    pct    = c("row", "col", "all", "no"),
    ci     = c("no", "cell", "diff"),
    OR     = c("no", "OR"),
    stars  = c(FALSE, TRUE),
    txt    = c(TRUE, FALSE)          # a factor col_var vs a numeric-only table
  )
  purrr::pmap_dfr(grid, function(color, signif, pct, ci, OR, stars, txt) {
    s <- tryCatch(
      tab_resolve_settings(color = color, OR = OR, ci = ci, chi2 = FALSE, ref = "tot",
                           pct_vect = list(pct), col_vars_text = txt, totrow = TRUE,
                           color_signif = signif, stars = stars),
      error = function(e) NULL)
    if (is.null(s)) return(tibble::tibble())
    tibble::tibble(in_color = color, in_signif = signif, in_pct = pct, in_ci = ci, in_OR = OR,
                   in_stars = stars, in_txt = txt,
                   out_color = s$color, out_chi2 = s$chi2, out_ci = s$ci,
                   out_ci_scale = s$ci_scale, out_totrow = s$totrow,
                   # Phase 19c: the four per-step sub-passes are gone; what each consumer derives
                   # from the ONE resolved measure is recorded instead, so the probe still shows the
                   # routing (and a pre-19c run of this script can be diffed against it).
                   out_builds = measure_builds(s$color),
                   out_num_ok = identical(s$color, "auto") || measure_applies(s$color, "num"))
  })
}

# --- main -------------------------------------------------------------------------------------
`%||%` <- function(x, y) if (is.null(x)) y else x
args <- commandArgs(trailingOnly = TRUE)
mode <- if (length(args) >= 1) args[[1]] else "check"
path <- if (length(args) >= 2) args[[2]] else "/tmp/color_attrs.rds"

if (identical(mode, "probe")) {
  p <- probe_resolver()
  cat("resolver probe:", nrow(p), "rows\n")
  cat("resolved color:", paste(sort(unique(p$out_color)), collapse = " | "), "\n")
  cat("build class   :", paste(sort(unique(p$out_builds)), collapse = " | "), "\n")
  saveRDS(p, sub("[.]rds$", "_probe.rds", path), version = 2)
  quit(save = "no")
}

got <- run_cases()
errs <- names(got)[purrr::map_lgl(got, is.character)]
cat("cases:", length(got), " errors:", length(errs), "\n")
if (length(errs)) { cat("  ERROR cases:\n"); for (e in errs) cat("   -", e, ":", got[[e]], "\n") }

if (identical(mode, "save")) {
  saveRDS(got, path, version = 2)
  # the resolver probe rides along, in the same file's sibling
  saveRDS(probe_resolver(), sub("[.]rds$", "_probe.rds", path), version = 2)
  cat("saved to", path, "\n")
} else {
  ref <- readRDS(path)
  common <- intersect(names(ref), names(got))
  gone   <- setdiff(names(ref), names(got)); new <- setdiff(names(got), names(ref))
  bad <- common[!purrr::map_lgl(common, ~ identical(ref[[.x]], got[[.x]]))]
  if (length(gone)) cat("MISSING cases:", paste(gone, collapse = ", "), "\n")
  if (length(new))  cat("NEW cases:", paste(new, collapse = ", "), "\n")
  if (length(bad) == 0 && !length(gone) && !length(new)) {
    cat("IDENTICAL -- ", length(common), " cases, every column attribute and slot vector matches.\n")
  } else {
    cat("CHANGED in", length(bad), "case(s):\n")
    for (b in utils::head(bad, 40L)) {
      cat(" *", b, "\n")
      a <- ref[[b]]; z <- got[[b]]
      if (is.character(a) || is.character(z)) { cat("    (error vs result)\n"); next }
      for (cn in union(names(a), names(z))) {
        if (identical(a[[cn]], z[[cn]])) next
        for (f in union(names(a[[cn]]), names(z[[cn]]))) {
          if (identical(a[[cn]][[f]], z[[cn]][[f]])) next
          cat("    ", cn, "$", f, ": ", paste(utils::head(unlist(a[[cn]][[f]]), 6), collapse = ","),
              "  ->  ", paste(utils::head(unlist(z[[cn]][[f]]), 6), collapse = ","), "\n", sep = "")
        }
      }
    }
  }
}
