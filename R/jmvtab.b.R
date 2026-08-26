# PURPOSE: The jamovi backend for the `jmvtab` analysis (Crosstables).
# ROLE: A thin orchestrator. `.run()` is weights -> build -> render: it reads the panel's options,
#       restores the crosstab cache from the hidden `cache_state` element's $state, calls the pure
#       jmvtab_build() (R/jmvtab-cache.R), persists the updated store, and renders through the shared
#       jmv_backend_* helpers (R/jmvtab-export.R). No option travels as a global around the build.
# KEY CONSTRAINTS:
#   - AN OPTION IS NAMED AFTER THE tab() ARGUMENT IT DRIVES -- exactly, or `<argument>_<slot>` where
#     several fold into one (the four `ci_method_*`; `ref` + `ref_levels`). `.opts()` is therefore a
#     pass-through, not a translation table, and test-jamovi-vocabulary.R checks the rule. The
#     declared exceptions are `lvs` (jmvcore::Options already defines a levels() method) and the
#     UI-only controls (export, wrap, theme).
#   - jmvtab.h.R is GENERATED from jamovi/jmvtab.a.yaml by jmvtools::prepare(); never hand-edit it --
#     and it LAGS: between a .a.yaml edit and the next prepare() a newly declared option reads back
#     NULL. Every read below therefore carries a `%||%` fallback, so the module runs on defaults in
#     that window instead of aborting.
#   - An argument applied at RENDER (`tab_theme`, `wrap_*`) is read STRAIGHT off self$options and kept
#     OUT of `.opts()`, which is the tier-3 cache key's complement: putting it there would make a
#     palette flip rebuild the whole table.
#   - The cache lives ONLY in $state -- it alone survives jamovi's engine reset. Never in an R global.
#   - The module runs in jamovi's bundled R: keep to what the package Imports / Suggests.
# See: CLAUDE.md § tabxplor architecture (jamovi) ; dev/tabxplor_2.0.0_jamovi_dev.md.

jmvtabClass <- if (requireNamespace('jmvcore', quietly = TRUE)) R6::R6Class(
  "jmvtabClass",
  inherit = jmvtabBase,
  private = list(

    .run = function() {

      data <- self$data

      wr   <- jmv_backend_weights(data, self$options$wt)
      data <- wr$data
      wt   <- wr$wt

      opts  <- private$.opts(wt)

      store <- self$results$cache_state$state          # NULL on the first run
      # DESIGN: flush queued option changes first, so a newer edit supersedes this run rather than
      # queueing behind it (the jmvcore remedy for UI stutter). Guarded for the non-jamovi harness.
      try(private$.checkpoint(), silent = TRUE)
      built <- jmvtab_build(data, opts, store)
      tabs  <- built$tabs

      # The status box sits UNDER the table (jamovi's Notice has no success type) and OUTLIVES the
      # click: the Export action resets itself after ~2 s, so the note rides in the state carrier.
      status <- jmv_backend_export(self, tabs)
      note   <- if (nzchar(status)) status else jmv_export_recall(store)
      self$results$cache_state$setState(jmv_export_remember(built$store, note))
      self$results$html_table$setContent(
        jmv_results_content(jmv_backend_render_html(self, tabs), note))
    },

    # Kept separate so the build core stays engine-free, i.e. testable without a live jamovi session.
    # NULL flows through: length-0 means "inject a placeholder", so NULL == character() here.
    .opts = function(wt) {
      # Filter the reference picker to the ACTIVE axis, or a stale cross-axis entry leaks into `ref`:
      # tab_setup() dispatches by `pct` -- a reference ROW under row%/means, a COLUMN under col%.
      active_vars <- as.character(
        if (identical(self$options$pct, "col")) self$options$col_vars else self$options$row_vars
      )
      ref_levels_active <- Filter(
        function(e) { v <- e[["var"]]; !is.null(v) && as.character(v) %in% active_vars },
        self$options$ref_levels
      )
      # The tick list shows the SOURCE levels (it must, or a merge could not be undone) and so writes
      # a RAW order, while the table's are merged: jmv_order_after_collapse() is where the two meet.
      lvl_collapse <- jmvtab_levels_collapse(self$options$levels_collapse)
      list(
        row_vars = self$options$row_vars,
        col_vars = self$options$col_vars,
        tab_vars = self$options$tab_vars,
        wt       = wt,
        pct          = self$options$pct,
        color        = self$options$color,          # "no"/"auto"/measure -> mapped in jmvtab_build
        color_signif = self$options$color_signif,
        test         = self$options$test %||% FALSE,
        anova        = self$options$anova %||% "welch",
        design_effect = isTRUE(self$options$design_effect),
        na           = self$options$na,
        levels       = self$options$lvs,
        ref          = jmvtab_ref_vector(ref_levels_active, self$options$ref),
        ref2         = self$options$ref2,
        # Applied post-aggregate: a tier-3 rebuild, tiers 1-2 reused.
        levels_order = jmv_order_after_collapse(
          jmvtab_levels_order(self$options$levels_order), lvl_collapse),
        # A merge and a cut change what is COUNTED, so both are in the tier-1 keys and miss them.
        levels_collapse = lvl_collapse,
        shape        = jmvtab_shape_vector(self$options$shape),
        comp         = self$options$comp,
        ci           = self$options$ci %||% "auto",
        conf_level   = self$options$conf_level,
        stars        = self$options$stars,
        ci_method_cell       = self$options$ci_method_cell,   # folded into ONE ci_method vector by
        ci_method_diff       = self$options$ci_method_diff,   # jmv_ci_method(): the UI keeps one
        ci_method_mean_diff  = self$options$ci_method_mean_diff,  # ComboBox per interval kind
        ci_method_mean_ratio = self$options$ci_method_mean_ratio,
        cleannames   = self$options$cleannames,
        totaltab     = self$options$totaltab,
        digits       = as.integer(self$options$digits),  # `digits` is a List -> a "0".."6" string
        n            = self$options$n,
        add_pct      = self$options$add_pct,
        subtext      = self$options$subtext,
        n_min        = self$options$n_min,
        display      = self$options$display %||% "auto",
        output_list  = FALSE,
        # Not a user choice but the module translating its own defaults (the R option is seeded in
        # English), which is why these are produced here rather than read off self$options.
        total_names  = c(row = gettext("Total",    domain = "R-tabxplor"),
                         col = gettext("Total",    domain = "R-tabxplor"),
                         tab = gettext("Ensemble", domain = "R-tabxplor"),
                         other = gettext("Others", domain = "R-tabxplor"))
      )
    },

    # Render / export / weights are the shared jmv_backend_* helpers in R/jmvtab-export.R.

    .plot = function(image, ...) {
      TRUE
    }
  )
)
