# PURPOSE: Jamovi module backend for the `jmvtabreg` analysis (Regressions).
# ROLE: A thin orchestrator (Phase 15b), the sibling of R/jmvtab.b.R. `.run()` reads the options from
#       jamovi/jmvtabreg.a.yaml, restores the live fit cache from the hidden `cache_state` result
#       element's $state, calls the pure jmvtab_reg_build() (R/jmvtabreg-cache.R -- which drives
#       tab_reg() with the cache injected via `.fit_cache`), persists the updated store, and renders
#       the table as HTML via tab_kable().
# KEY CONSTRAINTS:
#   - jmvtabreg.h.R is GENERATED from jmvtabreg.a.yaml (jmvtools::prepare()); never hand-edit it. The
#     R6Class `inherit = jmvtabregBase` is evaluated LAZILY (at instantiation, in the running app), so
#     this file loads / checks fine before the .h.R exists.
#   - The module runs in Jamovi's bundled R -- keep dependencies to what the package Imports/Suggests.
#   - The cache lives ONLY in $state (survives the engine reset); never rely on R globals.
#   - Export (Excel / HTML / Markdown) reuses R/jmvtab-export.R (resolveExportPath / jmvtab_export).
# See: dev/tabxplor_1.4.0_jamovi_dev.md ; CLAUDE.md > 1.4.0 roadmap > Phase 15b.

# @rdname jamovi
jmvtabregClass <- if (requireNamespace('jmvcore', quietly = TRUE)) R6::R6Class(
  "jmvtabregClass",
  inherit = jmvtabregBase,
  private = list(

    .run = function() {

      data <- self$data

      # Weights (shared helper: adds a Data-level weight back as .COUNTS).
      wr   <- jmv_backend_weights(data, self$options$wt)
      data <- wr$data
      wt   <- wr$wt

      opts  <- private$.opts(wt)

      # DESIGN (Phase h): a model COMPARISON (>=2 folded models) is heavy -- refitting every model on
      # each live predictor edit is what froze the panel. In that "staged" mode the table computes ONLY
      # on the Run button (or an Export, which needs the result): between clicks a changed signature
      # marks the shown table outdated; an unchanged one (incl. the run_compare auto-reset run) re-serves
      # the last render. Single-model use stays fully live. compare_state persists sig + HTML across resets.
      staged  <- is.list(opts$predictors) && length(opts$predictors) >= 2L
      trigger <- isTRUE(self$options$run_compare) || isTRUE(self$options$exportExcel)
      cur_sig <- jmvtab_reg_compare_sig(opts)
      cst     <- self$results$compare_state$state       # list(sig=, html=) or NULL

      # Phase o: the fit cache is only useful for a SINGLE model (the reref digest fast-path). In staged
      # comparison mode it just holds raw fits (~10 MB each) that re-serialize into $state on every UI
      # round-trip -> the freeze. Drop it entirely here (the single most important line) so it stops
      # persisting; the trigger path below then builds without a cache. Reverting to one model starts a
      # fresh cache on the next run (the digest fast-path re-engages).
      if (staged) self$results$cache_state$setState(NULL)

      if (staged && !trigger) {
        if (!is.null(cst) && identical(cst$sig, cur_sig)) {
          self$results$html_table$setContent(cst$html)  # unchanged / just-computed -> re-serve
        } else {
          self$results$html_table$setContent(private$.compare_hint(cst))
        }
        return(invisible())
      }

      store <- if (staged) NULL else self$results$cache_state$state   # NULL on the first / staged run
      # Flush queued option changes BEFORE the (potentially heavy) fit so a newer edit supersedes this
      # run instead of piling up -- the jmvcore remedy for UI stutter. Guarded for the non-jamovi harness.
      try(private$.checkpoint(), silent = TRUE)
      built <- jmvtab_reg_build(data, opts, store, use_cache = !staged)
      self$results$cache_state$setState(if (staged) NULL else built$store)  # persist the fit digests (single model only)
      tabs  <- built$tabs

      if (is.null(tabs)) {
        self$results$html_table$setContent(private$.hint())
        return(invisible())
      }

      # Export (Excel / HTML / Markdown) + HTML render -- the shared jmv_backend_* helpers. The export
      # returns a styled status line (bold green with the path REALLY written / bold red on failure)
      # prepended above the table; compare_state stores the PURE render so a re-serve stays clean.
      status <- jmv_backend_export(self, tabs)
      html <- jmv_backend_render_html(self, tabs)
      self$results$html_table$setContent(paste0(status, html))
      # Remember the computed comparison so a later live edit can re-serve / flag it (Phase h).
      if (staged) self$results$compare_state$setState(list(sig = cur_sig, html = html))
    },

    # Collect the jamovi options into the plain list jmvtab_reg_build() consumes, ALREADY in tab_reg()
    # vocabulary (so the build core stays engine-free / testable without a live jamovi session).
    .opts = function(wt) {
      list(
        dependent    = self$options$dependent,
        # the model-builder (`models` Array) folds into `predictors`: empty -> the flat pool (one
        # model); >=1 card -> a named list of predictor subsets (model comparison).
        predictors   = jmvtab_reg_models(self$options$models, self$options$predictors),
        # model-comparison test (footer): needs >=2 models; baseline = the chosen model's position.
        compare      = self$options$compare,
        baseline     = self$options$baseline,
        # Phase 15d: the per-dependent Model table drives family / modelled level / trials. The raw
        # arrays are passed through; jmvtab_reg_build() resolves each outcome's family (auto-detect for
        # a blank pick), groups the outcomes by family, and calls tab_reg() once per family group.
        depFamily     = self$options$depFamily,
        depModelLevel = self$options$depModelLevel,
        depTrials     = self$options$depTrials,
        # numeric-predictor scaling (raw array; the build core drops it for multinomial / ordinal
        # groups so a family switch never aborts tab_reg()). NB the jamovi option key is still
        # `multiplicator` (unchanged so no prepare() regen); only the R-facing arg is renamed.
        multiplicator = self$options$multiplicator,
        wt           = wt,
        ids          = self$options$ids,
        strata       = self$options$strata,
        fpc          = self$options$fpc,
        nest         = self$options$nest,
        split_var    = self$options$split_var,
        # exponentiate / color are now logical checkboxes (TRUE = ratios-when-sensible / colour on).
        exponentiate = isTRUE(self$options$exponentiate),
        effect       = self$options$effect,
        at           = self$options$at,
        estimate_display = self$options$estimate_display,
        empirical    = self$options$empirical,
        # the reference-level picker (refLevels) -> tab_reg's `reference` named vector (NULL = default)
        reference    = jmvtab_reg_ref_vector(self$options$refLevels),
        conf_level   = self$options$conf_level,
        method       = self$options$method,
        stars        = self$options$stars,
        color        = isTRUE(self$options$color),
        color_signif = self$options$color_signif,
        na           = self$options$na,
        cleannames   = self$options$cleannames,
        subtext      = self$options$subtext
        # footer removed (Phase 15d): the model-summary footer is always shown (tab_reg stats = NULL).
      )
    },

    # .render_html / export / weights are the shared jmv_backend_* helpers in
    # R/jmvtab-export.R (Phase 17i) -- called directly from .run() above.

    # A friendly placeholder when the outcome / predictors are not both selected yet (or a model
    # comparison was requested with several dependents, which tab_reg() does not allow).
    .hint = function() {
      paste0("<div style='padding:12px;opacity:0.7;font-style:italic;'>",
             "Select a <b>dependent</b> (outcome) variable and one or more <b>predictors</b> ",
             "to fit a regression. For a model comparison (predictor subsets), choose a single ",
             "dependent.</div>")
    },

    # Phase h: shown in staged comparison mode when the model set / options changed but the user has not
    # clicked Run. Any previous render (cst$html) stays below the banner so the outdated table is visible.
    .compare_hint = function(cst = NULL) {
      banner <- paste0(
        "<div style='padding:10px 12px;margin-bottom:6px;border:1px solid #d0a; ",
        "border-radius:4px;background:rgba(204,0,170,0.06);'>",
        if (is.null(cst))
          "Model comparison staged. Click <b>Run comparison</b> to compute the table."
        else
          "The model set or options changed. Click <b>Run comparison</b> to refresh (the table below is outdated).",
        "</div>")
      paste0(banner, if (is.null(cst)) "" else cst$html)
    },

    .plot = function(image, ...) {
      TRUE
    }
  )
)
