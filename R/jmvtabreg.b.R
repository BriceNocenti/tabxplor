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

      # A Data-level weight (Data >>> Weights) is carried as an attribute; add it back as a column.
      wt <- character()
      if (!is.null(self$options$wt) && length(self$options$wt)) {
        wt <- self$options$wt
      } else if (!is.null(attr(data, "jmv-weights"))) {
        data[['.COUNTS']] <- jmvcore::toNumeric(attr(data, "jmv-weights"))
        wt <- ".COUNTS"
      }

      opts  <- private$.opts(wt)
      store <- self$results$cache_state$state          # NULL on the first run
      built <- jmvtab_reg_build(data, opts, store)
      self$results$cache_state$setState(built$store)   # persist the fit digests / raw fits
      tabs  <- built$tabs

      if (is.null(tabs)) {
        self$results$html_table$setContent(private$.hint())
        return(invisible())
      }

      # Export (Excel / HTML / Markdown): the `exportExcel` Action is a boolean click. The format picks
      # the extension; the user-typed `path` is resolved (Documents default) and reported via a Notice.
      if (isTRUE(self$options$exportExcel)) {
        fmt <- self$options$export_format
        ext <- switch(fmt, "excel" = "xlsx", "html" = "html", "md" = "md", "xlsx")
        p   <- resolveExportPath(self$options$path, ext)
        tryCatch({
          jmvtab_export(tabs, format = fmt, path = p, replace = self$options$xl_replace)
          private$.notice(paste0("Saved to: ", p), ok = TRUE)
        }, error = function(err) {
          private$.notice(paste0("Export failed: ", err$message), ok = FALSE)
        })
      }

      self$results$html_table$setContent(private$.render_html(tabs))
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
        # multiplicator (numeric-predictor scaling): not for multinomial / ordinal -> NULL there so a
        # family switch never aborts tab_reg().
        multiplicator = if (self$options$family %in% c("multinomial", "ordinal")) NULL
                        else jmvtab_reg_mult_vector(self$options$multiplicator),
        # trials (grouped / summed-score binomial): binomial only -> NULL for other families.
        trials       = if (self$options$family %in% c("binomial", "auto"))
                         switch(self$options$trials_mode,
                                "observed" = TRUE,
                                "fixed"    = { n <- self$options$trials_n
                                               if (is.null(n) || is.na(n) || n < 1) NULL
                                               else as.integer(n) },
                                NULL)
                       else NULL,
        wt           = wt,
        ids          = self$options$ids,
        strata       = self$options$strata,
        fpc          = self$options$fpc,
        nest         = self$options$nest,
        split_var    = self$options$split_var,
        family       = self$options$family,
        # exponentiate: the List gives "nongaussian" / "yes" / "no" -> tab_reg's "nongaussian"/TRUE/FALSE
        exponentiate = switch(self$options$exponentiate, "yes" = TRUE, "no" = FALSE, "nongaussian"),
        effect       = self$options$effect,
        at           = self$options$at,
        estimate_display = self$options$estimate_display,
        inverse_two_level_factors = self$options$inverse_two_level_factors,
        empirical    = self$options$empirical,
        # the reference-level picker (refLevels) -> tab_reg's `reference` named vector (NULL = default)
        reference    = jmvtab_reg_ref_vector(self$options$refLevels),
        conf_level   = self$options$conf_level,
        method       = self$options$method,
        stars        = self$options$stars,
        # color: "default" -> NULL (per-family default); else the chosen measure / "no"
        color        = if (identical(self$options$color, "default")) NULL else self$options$color,
        color_signif = self$options$color_signif,
        na           = self$options$na,
        cleannames   = self$options$cleannames,
        # footer: TRUE -> the default GOF set (NULL); FALSE -> no footer ("none")
        stats        = if (isTRUE(self$options$footer)) NULL else "none",
        subtext      = self$options$subtext
      )
    },

    # Render a tab (or list of tabs) to standalone HTML for the Jamovi results iframe -- the SAME
    # dependency-free, self-contained html engine + scroll box the crosstab module uses.
    .render_html = function(tabs) {
      tab_kable(
        tabs, engine = "html",
        wrap_rows = self$options$wrap_rows,
        wrap_cols = self$options$wrap_cols,
        tooltips = FALSE
      ) |>
        tab_render_scrollbox()
    },

    # A friendly placeholder when the outcome / predictors are not both selected yet (or a model
    # comparison was requested with several dependents, which tab_reg() does not allow).
    .hint = function() {
      paste0("<div style='padding:12px;opacity:0.7;font-style:italic;'>",
             "Select a <b>dependent</b> (outcome) variable and one or more <b>predictors</b> ",
             "to fit a regression. For a model comparison (predictor subsets), choose a single ",
             "dependent.</div>")
    },

    # Report an export result via a native jmvcore::Notice (info / error), inserted at the top.
    .notice = function(text, ok = TRUE) {
      notice <- jmvcore::Notice$new(
        options = self$options, name = "exportNotice",
        type = if (ok) jmvcore::NoticeType$INFO else jmvcore::NoticeType$ERROR
      )
      notice$setContent(text)
      self$results$insert(1, notice)
    },

    .plot = function(image, ...) {
      TRUE
    }
  )
)
