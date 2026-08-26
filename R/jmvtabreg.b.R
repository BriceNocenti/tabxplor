# PURPOSE: The jamovi backend for the `jmvtabreg` analysis (Regressions).
# ROLE: A thin orchestrator, the sibling of R/jmvtab.b.R. `.run()` reads the panel's options, restores
#       the fit cache from the hidden `cache_state` element's $state, calls the pure jmvtab_reg_build()
#       (R/jmvtabreg-cache.R, which drives tab_reg() with the cache injected), persists the updated
#       store, and renders through the shared jmv_backend_* helpers (R/jmvtab-export.R).
# KEY CONSTRAINTS:
#   - AN OPTION IS NAMED AFTER THE tab_reg() ARGUMENT IT DRIVES (or `<argument>_<slot>` where several
#     fold into one), so `.opts()` speaks tab_reg()'s own vocabulary end to end and no translator sits
#     between a control and its argument; test-jamovi-vocabulary.R checks the rule. ⚠ renaming an
#     option DISCARDS its value in already-saved .omv files -- accepted: this module carries no
#     back-compat promise.
#   - jmvtabreg.h.R is GENERATED from jamovi/jmvtabreg.a.yaml by jmvtools::prepare(); never hand-edit
#     it. `inherit = jmvtabregBase` is resolved LAZILY (at instantiation, in the running app), so this
#     file loads and checks fine before the .h.R exists -- and the .h.R LAGS, hence the `%||%` on
#     every read: in that window the module runs on defaults instead of aborting.
#   - A MODEL COMPARISON IS STAGED, not live. Refitting every model on each predictor edit is what
#     froze the panel, so with >=2 folded models the table computes only on Run (or on an Export,
#     which needs the result) and the fit cache is dropped outright -- see `.run()`.
#   - `stats =` has no control: tab_reg()'s own default already compares several predictor subsets, so
#     the panel asks nothing and sends NULL.
#   - The cache lives ONLY in $state -- it alone survives jamovi's engine reset. Never in an R global.
#   - The module runs in jamovi's bundled R: keep to what the package Imports / Suggests.
# See: CLAUDE.md § tabxplor architecture (jamovi) ; dev/tabxplor_2.0.0_jamovi_dev.md.

jmvtabregClass <- if (requireNamespace('jmvcore', quietly = TRUE)) R6::R6Class(
  "jmvtabregClass",
  inherit = jmvtabregBase,
  private = list(

    .run = function() {

      data <- self$data

      wr   <- jmv_backend_weights(data, self$options$wt)
      data <- wr$data
      wt   <- wr$wt

      opts  <- private$.opts(wt)

      # Between Run clicks a CHANGED signature marks the table outdated and an unchanged one
      # re-serves the last render; compare_state carries both across an engine reset.
      staged  <- jmvtab_reg_staged(
        self$options$models, self$options$predictors,
        jmvtab_reg_cross_keys(self$options$crosses, self$options$predictors))
      trigger <- isTRUE(self$options$run_compare) || isTRUE(self$options$exportExcel)
      cur_sig <- jmvtab_reg_compare_sig(opts)
      cst     <- self$results$compare_state$state       # list(sig=, html=) or NULL

      # WARNING: a comparison is a test BETWEEN the fits, so it KEEPS them -- and they would then 
      # re-serialize into $state on every UI round-trip. Dropping the cache here is what stops that.
      if (staged) self$results$cache_state$setState(NULL)

      if (staged && !trigger) {
        # ⚠ read the render through jmvtab_reg_render_fetch(), never off `cst$html`: one too big for
        # jmvcore's state ceiling never went in there, and only the process-local mirror has it.
        last <- jmvtab_reg_render_fetch(cst)
        if (!is.null(last) && identical(cst$sig, cur_sig)) {
          self$results$html_table$setContent(jmv_results_content(last))   # unchanged -> re-serve
        } else {
          self$results$html_table$setContent(jmv_results_content(private$.compare_hint(last)))
        }
        return(invisible())
      }

      store <- if (staged) NULL else self$results$cache_state$state   # NULL on the first / staged run
      # Flush queued option changes BEFORE the (heavy) fit, so a newer edit supersedes this run.
      try(private$.checkpoint(), silent = TRUE)
      built <- jmvtab_reg_build(data, opts, store, use_cache = !staged)
      self$results$cache_state$setState(if (staged) NULL else built$store)
      tabs  <- built$tabs

      if (is.null(tabs)) {
        self$results$html_table$setContent(jmv_results_content(private$.hint()))
        return(invisible())
      }

      status <- jmv_backend_export(self, tabs)
      html <- jmv_backend_render_html(self, tabs)
      self$results$html_table$setContent(jmv_results_content(status, html))
      # Store the PURE render, so a later re-serve stays clean of the export status line.
      if (staged) self$results$compare_state$setState(jmvtab_reg_render_store(cur_sig, html))
    },

    .opts = function(wt) {
      list(
        outcome      = self$options$outcome,
        # The model builder and the interaction picker fold into the ONE `predictors` argument
        # tab_reg() takes -- an interaction IS a predictor.
        # ⚠ `flatten` is the several-outcomes rule: ONE predictor subset with several outcomes is a
        # per-outcome table, not a comparison, and `is.list(predictors)` is what tells them apart, so
        # the card must not make `predictors` a LIST there.
        predictors   = jmvtab_reg_models(
          self$options$models, self$options$predictors,
          jmvtab_reg_cross_keys(self$options$crosses, self$options$predictors),
          flatten = length(self$options$outcome) > 1L),
        # The estimand cascade's LEFT half is a question about each OUTCOME: all four pass through raw
        # and resolve together in jmvtab_reg_build(), so mixed families render as one table.
        family        = self$options$family,
        link          = self$options$link,
        outcome_level = self$options$outcome_level,
        trials        = self$options$trials,
        # Raw: the build core drops it for multinomial / ordinal groups, so a family switch cannot
        # abort tab_reg().
        multiplier   = self$options$multiplier,
        shape        = self$options$shape,
        wt           = wt,
        tab_vars     = self$options$tab_vars,
        # ...and the cascade's RIGHT half stays scalar: which measure is REPORTED, and from where.
        measure      = self$options$measure %||% "auto",
        effect       = self$options$effect  %||% "auto",
        display      = self$options$display %||% "auto",
        digits       = as.integer(jmv_opt(self, "digits", "0")),  # a List -> a "0".."6" string
        empirical    = self$options$empirical,   # a Bool; NULL -> tab_reg()'s TRUE
        ref          = jmvtab_reg_ref_vector(self$options$ref_levels),
        levels_collapse = self$options$levels_collapse,
        # A DISPLAY order: `tab_reg(.levels_order =)` permutes the row skeleton and never the data, so
        # a reorder is a cache HIT. "The baseline IS the first level" is carried by `ref` alone.
        levels_order    = self$options$levels_order,
        conf_level   = self$options$conf_level,
        ci_method    = self$options$ci_method,
        stars        = self$options$stars,
        # `color` is a MEASURE, not a checkbox: the words pass through as tab_reg() takes them, and
        # only "no" needs translating, colour being the one argument spelled FALSE.
        color        = switch(self$options$color %||% "measure", "no" = FALSE, self$options$color),
        color_signif = self$options$color_signif,
        na           = self$options$na,
        n            = self$options$n,
        cleannames   = self$options$cleannames,
        subtext      = self$options$subtext
      )
    },

    # Render / export / weights are the shared jmv_backend_* helpers in R/jmvtab-export.R.

    # ⚠ ONE msgid, not concatenated pieces: a translator owns the whole sentence. And jmvcore's `.()`,
    # not gettext(): it reads the module's own catalogue, keyed on jamovi's UI language.
    .hint = function() {
      jmv_results_note(
        jmvcore::.("Select an <b>outcome</b> variable and one or more <b>predictors</b> to fit a regression. For a model comparison (predictor subsets), choose a single outcome."),
        style = "padding:12px;opacity:0.7;font-style:italic;")
    },

    # Any previous render stays below the banner, so the outdated table remains visible.
    .compare_hint = function(last = NULL) {
      banner <- jmv_results_note(
        if (is.null(last))
          jmvcore::.("Model comparison staged. Click <b>Run comparison</b> to compute the table.")
        else
          jmvcore::.("Model options changed, table below is outdated. Click <b>Run comparison</b> to refresh."),
        style = paste0("padding:10px 12px;margin-bottom:6px;border:1px solid #d0a;",
                       "border-radius:4px;background:rgba(204,0,170,0.06);"))
      paste0(banner, last %||% "")
    },

    .plot = function(image, ...) {
      TRUE
    }
  )
)
