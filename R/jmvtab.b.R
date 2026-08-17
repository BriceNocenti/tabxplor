# PURPOSE: Jamovi module backend for the `jmvtab` analysis (Crosstables).
# ROLE: A thin orchestrator (Phase 7e). `.run()` reads the options from jamovi/jmvtab.a.yaml, restores
#       the multi-tier cache from the hidden `cache_state` result element's $state, calls the pure
#       jmvtab_build() (R/jmvtab-cache.R -- which drives the SAME tab() pipeline with the cache
#       injected), persists the updated store, and renders the table as HTML via tab_kable().
# KEY CONSTRAINTS:
#   - jmvtab.h.R is GENERATED from jmvtab.a.yaml (jmvtools::prepare()); never hand-edit it -- and it
#     LAGS: a newly declared option reads back NULL until the next prepare(), so `.opts()` gives each
#     one an explicit `%||%` fallback (the module runs on defaults in that window, never aborts).
#   - Phase 19k: `.run()` is weights -> build -> render. NO option travels as a global around the
#     build any more (`anova` was the last; it is tab()'s own argument now). `ci_print` keeps its
#     options()/on.exit, deliberately: it is read inside format(), i.e. around the RENDER.
#   - Phase 20g-i: AN OPTION IS NAMED AFTER THE tab() ARGUMENT IT DRIVES -- exactly, or as
#     `<argument>_<slot>` where several options fold into one (`ci_method_cell` ... -> `ci_method`,
#     `ref` + `ref_levels` -> `ref`). `.opts()` is therefore a pass-through, not a translation
#     table, and test-jamovi-vocabulary.R checks the rule (names, control names and `ui.<name>` in
#     the .js alike). The declared exceptions are `lvs` (jmvcore::Options already has a levels()
#     method) and the UI-only controls (export, wrap, models/run_compare, ci_print).
#   - The module runs in Jamovi's bundled R -- keep dependencies to what the package Imports/Suggests.
#   - The cache lives ONLY in $state (survives the engine reset); never rely on R globals (§5.2).
#   - Export (Excel / HTML / Markdown; Phase 7g) resolves a typed path (Documents default) and
#     reports via a jmvcore::Notice -- the export dispatch lives in R/jmvtab-export.R.
# See: dev/tabxplor_jmvtab_cache_design.md ; CLAUDE.md > 2.0.0 roadmap > Phase 7e.

# @rdname jamovi
jmvtabClass <- if (requireNamespace('jmvcore', quietly = TRUE)) R6::R6Class(
  "jmvtabClass",
  inherit = jmvtabBase,
  private = list(

    .run = function() {

      data <- self$data

      # --- Weights (shared helper: adds a Data-level weight back as .COUNTS) ------------------
      wr   <- jmv_backend_weights(data, self$options$wt)
      data <- wr$data
      wt   <- wr$wt

      # --- Build the table through the cached pipeline ---------------------------------------
      opts  <- private$.opts(wt)

      # Phase 18z16-iii (W11): ONE honest checkbox. It was labelled "Type of p-value" but has moved
      # every confidence interval, star and colour threshold in the table since Phase 18s.
      # z16-iiiii: it rides `opts$design_effect` into tab()'s own argument -- no global option, no
      # on.exit dance (it is in the tier-3 base key, so a toggle rebuilds with it).
      # Phase 19k: `anova` rides `opts$anova` the same way -- it was the LAST option travelling as a
      # global here (options() + on.exit around the build), which also made it a stale-cache hazard.
      # It is tab()'s own argument now, stored as display intent and read back at render, so a
      # toggle is a tier-4 re-derive rather than a rebuild.

      store <- self$results$cache_state$state          # NULL on the first run
      # DESIGN (Phase h): flush queued option changes before building so a newer edit supersedes this
      # run rather than queuing (the jmvcore remedy for UI stutter). Guarded for the non-jamovi harness.
      try(private$.checkpoint(), silent = TRUE)
      built <- jmvtab_build(data, opts, store)
      self$results$cache_state$setState(built$store)   # persist tiers 1-2 for the next interaction
      tabs  <- built$tabs

      # ci_print controls the [inf;sup] vs pct +- moe display; it is a global option read at
      # format time, so set it around the render and restore it afterwards.
      ci_print_option <- getOption("tabxplor.ci_print")
      options("tabxplor.ci_print" = if (self$options$ci_print == "moe") "moe" else "ci")
      on.exit(options("tabxplor.ci_print" = ci_print_option), add = TRUE)

      # --- Export (Excel / HTML / Markdown; Phase 7g) + HTML render (shared helpers) ----------
      # The export returns a styled status line (bold green with the path REALLY written / bold red on
      # failure); prepend it above the rendered table (jamovi's Notice has no green success type).
      status <- jmv_backend_export(self, tabs)
      self$results$html_table$setContent(paste0(status, jmv_backend_render_html(self, tabs)))
    },

    # Collect the jamovi options into the plain list jmvtab_build() consumes. Kept separate so the
    # build core stays engine-free (testable without a live jamovi session).
    .opts = function(wt) {
      # NULL (an empty variable slot) flows through: jmvtab_build() treats length-0 as "inject a
      # placeholder", so NULL and character() are equivalent here.
      # WARNING (Phase 19k): every option this list reads must tolerate NULL. `R/jmvtab.h.R` is a
      # GENERATED artefact that only a maintainer `jmvtools::prepare()` can rebuild, so between a
      # `.a.yaml` edit and that step `self$options$<new option>` is NULL -- the module must then run
      # on defaults, never abort. Hence the `%||%` fallbacks below (`%||%` is defined by the package,
      # R/tab-test-display.R, so it does not need base R >= 4.4).
      # Phase 7g-iii: filter the reference picker to the active axis (see `ref` below).
      active_vars <- as.character(
        if (identical(self$options$pct, "col")) self$options$col_vars else self$options$row_vars
      )
      ref_levels_active <- Filter(
        function(e) { v <- e[["var"]]; !is.null(v) && as.character(v) %in% active_vars },
        self$options$ref_levels
      )
      # Phase 20g-ii: the two level specs are folded TOGETHER, because the tick list shows the SOURCE
      # levels (it must, or a merge could not be undone) and therefore writes a raw order, while the
      # table's levels are the merged ones. jmv_order_after_collapse() is the one place they meet.
      lvl_collapse <- jmvtab_levels_collapse(self$options$levels_collapse)
      list(
        row_vars = self$options$row_vars,
        col_vars = self$options$col_vars,
        tab_vars = self$options$tab_vars,
        wt       = wt,
        pct          = self$options$pct,
        color        = self$options$color,          # "no"/"auto"/measure -> mapped in jmvtab_build
        color_signif = self$options$color_signif,
        # Phase 19k: `test`, not `chi2` -- the option is renamed after tab()'s own argument (the test
        # is a Chi-squared only for factors; a numeric col_var gets an F). The retired `OR` option is
        # gone: what prints an odds ratio is `display`, and which 2x2 it uses is `ref2`.
        test         = self$options$test %||% FALSE,
        # `anova` selects the displayed F (welch/classic). Phase 19k: it is tab()'s own argument,
        # stored as display intent -- so it sits in the tier-3 `reapplied` set and a toggle is a
        # cheap re-derive (the p-value line is materialised at DISPLAY, from the `test` attribute,
        # which holds BOTH F rows).
        anova        = self$options$anova %||% "welch",
        # Phase 18j / z16-iii: the inference basis checkbox. It lands in the tier-3 base key
        # (structural, not `reapplied`) -> a toggle rebuilds; the robust overlay recomputes the
        # omnibus p on the flat design, and every cell interval moves with it.
        design_effect = isTRUE(self$options$design_effect),
        na           = self$options$na,
        levels       = self$options$lvs,             # option named `lvs` (jmvcore has a levels() method)
        # Phase 7g-iii: the reference-level picker (ref_levels) drives `ref`, keyed by the ACTIVE axis
        # (col_vars under pct="col", else row_vars -- filtered above so a stale cross-axis entry can't
        # leak). tab_setup() dispatches by pct: a row reference (row%/means) vs a per-col_var column
        # reference (col%). A chosen level label is matched by exact equality in diff_index(). Falls
        # back to the (hidden) expert free-text `ref`. ref2 = the OR 2nd reference (a level / first / tot).
        ref          = jmvtab_ref_vector(ref_levels_active, self$options$ref),
        ref2         = self$options$ref2,
        # Phase 7g-ii: per-variable level reordering (levels_order picker) -> a named list of ordered
        # levels; applied post-aggregate in jmv_cache_aggregate() (tier-3 rebuild, tiers 1-2 reused).
        levels_order = jmv_order_after_collapse(
          jmvtab_levels_order(self$options$levels_order), lvl_collapse),
        # Phase 20g-ii: the level-MERGE tick-boxes, in the same control -> tab()'s internal
        # `.levels_collapse`; applied PRE-aggregate in tab_prepare(), so it changes the cells, the
        # bases and the test (a tier-1 cache miss, by design).
        levels_collapse = lvl_collapse,
        comp         = self$options$comp,
        ci           = self$options$ci %||% "auto",
        conf_level   = self$options$conf_level,
        stars        = self$options$stars,
        ci_method_cell       = self$options$ci_method_cell,   # folded into ONE ci_method vector by
        ci_method_diff       = self$options$ci_method_diff,   # jmv_ci_method() -- the UI keeps one
        ci_method_mean_diff  = self$options$ci_method_mean_diff,  # ComboBox per interval kind
        ci_method_mean_ratio = self$options$ci_method_mean_ratio,
        cleannames   = self$options$cleannames,      # applied at DISPLAY (Phase 7e)
        totaltab     = self$options$totaltab,
        digits       = as.integer(self$options$digits),  # `digits` is a List -> a "0".."6" string
        other_if_less_than = self$options$other_if_less_than,
        add_n        = self$options$add_n,
        add_pct      = self$options$add_pct,
        subtext      = self$options$subtext,
        n_min        = self$options$n_min,           # Phase 7g: small-base display filter (tier 4)
        display      = self$options$display %||% "auto",
        output_list  = FALSE,
        # Phase 20b: the four synthetic labels are ONE option (`tabxplor.total_names`), and 20g-i made
        # them ONE key here -- they were three, mirroring three arguments that no longer exist and
        # that no control ever offered. They are not a user choice; they are the module TRANSLATING
        # its own defaults (the R option is seeded in English), which is why they are produced here
        # rather than read from `self$options`. jmv_tab3_build_armed() installs them for one build.
        total_names  = c(row = gettext("Total",    domain = "R-tabxplor"),
                         col = gettext("Total",    domain = "R-tabxplor"),
                         tab = gettext("Ensemble", domain = "R-tabxplor"),
                         other = gettext("Others", domain = "R-tabxplor"))
      )
    },

    # .render_html / export / weights are the shared jmv_backend_* helpers in
    # R/jmvtab-export.R (Phase 17i) -- called directly from .run() above.

    .plot = function(image, ...) {
      TRUE
    }
  )
)
