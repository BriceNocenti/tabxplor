# PURPOSE: Jamovi module backend for the `jmvtab` analysis (Crosstables).
# ROLE: A thin orchestrator (Phase 7e). `.run()` reads the options from jamovi/jmvtab.a.yaml, restores
#       the multi-tier cache from the hidden `cache_state` result element's $state, calls the pure
#       jmvtab_build() (R/jmvtab-cache.R -- which drives the SAME tab() pipeline with the cache
#       injected), persists the updated store, and renders the table as HTML via tab_kable().
# KEY CONSTRAINTS:
#   - jmvtab.h.R is GENERATED from jmvtab.a.yaml (jmvtools::prepare()); never hand-edit it.
#   - The module runs in Jamovi's bundled R -- keep dependencies to what the package Imports/Suggests.
#   - The cache lives ONLY in $state (survives the engine reset); never rely on R globals (§5.2).
#   - Export (Excel / HTML / Markdown; Phase 7g) resolves a typed path (Documents default) and
#     reports via a jmvcore::Notice -- the export dispatch lives in R/jmvtab-export.R.
# See: dev/tabxplor_jmvtab_cache_design.md ; CLAUDE.md > 1.4.0 roadmap > Phase 7e.

# @rdname jamovi
jmvtabClass <- if (requireNamespace('jmvcore', quietly = TRUE)) R6::R6Class(
  "jmvtabClass",
  inherit = jmvtabBase,
  private = list(

    .run = function() {

      data <- self$data

      # --- Weights ---------------------------------------------------------------------------
      # self$data only holds the selected variables; a Data-level weight (Data >>> Weights) is
      # carried as an attribute and must be added back by hand. Result: the weight VARIABLE NAME
      # (a character), or character() when unweighted.
      wt <- character()
      if (!is.null(self$options$wt)) {
        wt <- self$options$wt
      } else if (!is.null(attr(data, "jmv-weights"))) {
        data[['.COUNTS']] <- jmvcore::toNumeric(attr(data, "jmv-weights"))
        wt <- ".COUNTS"
      }

      # --- Build the table through the cached pipeline ---------------------------------------
      opts  <- private$.opts(wt)

      # Phase 7g: the ANOVA F displayed for numeric col_vars (Welch vs classic) is a global option
      # read while BUILDING the p-value line, so set it before the build and restore it afterwards.
      anova_option <- getOption("tabxplor.anova")
      options("tabxplor.anova" = if (identical(self$options$anova, "classic")) "classic" else "welch")
      on.exit(options("tabxplor.anova" = anova_option), add = TRUE)

      store <- self$results$cache_state$state          # NULL on the first run
      built <- jmvtab_build(data, opts, store)
      self$results$cache_state$setState(built$store)   # persist tiers 1-2 for the next interaction
      tabs  <- built$tabs

      # ci_print controls the [inf;sup] vs pct +- moe display; it is a global option read at
      # format time, so set it around the render and restore it afterwards.
      ci_print_option <- getOption("tabxplor.ci_print")
      options("tabxplor.ci_print" = if (self$options$ci_print == "moe") "moe" else "ci")
      on.exit(options("tabxplor.ci_print" = ci_print_option), add = TRUE)

      # --- Export (Excel / HTML / Markdown; Phase 7g) ----------------------------------------
      # The `exportExcel` Action is a boolean click (§5.3). The format chooses the extension; the
      # user-typed `path` is resolved (Documents default, ~ -> USERPROFILE) and the result reported
      # via a jmvcore::Notice (info / error). See R/jmvtab-export.R.
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

      # --- HTML table ------------------------------------------------------------------------
      self$results$html_table$setContent(private$.render_html(tabs))
    },

    # Collect the jamovi options into the plain list jmvtab_build() consumes. Kept separate so the
    # build core stays engine-free (testable without a live jamovi session).
    .opts = function(wt) {
      # NULL (an empty variable slot) flows through: jmvtab_build() treats length-0 as "inject a
      # placeholder", so NULL and character() are equivalent here (avoids base-R-4.4-only `%||%`).
      # Phase 7g-iii: filter the reference picker to the active axis (see `ref` below).
      active_vars <- as.character(
        if (identical(self$options$pct, "col")) self$options$col_vars else self$options$row_vars
      )
      refLevels_active <- Filter(
        function(e) { v <- e[["var"]]; !is.null(v) && as.character(v) %in% active_vars },
        self$options$refLevels
      )
      list(
        row_vars = self$options$row_vars,
        col_vars = self$options$col_vars,
        tab_vars = self$options$tab_vars,
        wt       = wt,
        pct          = self$options$pct,
        color        = self$options$color,          # "no"/"auto"/measure -> mapped in jmvtab_build
        color_signif = self$options$color_signif,
        OR           = self$options$OR,
        chi2         = self$options$chi2,
        # Phase 7g: `anova` selects the displayed F (welch/classic). It is baked into the p-value
        # line at build time, so it must sit in the tier-3 base-key (not `reapplied`) -> a toggle
        # rebuilds. The global option is set from it around the build in .run().
        anova        = self$options$anova,
        na           = self$options$na,
        levels       = self$options$lvs,             # option named `lvs` (jmvcore has a levels() method)
        # Phase 7g-iii: the reference-level picker (refLevels) drives `ref`, keyed by the ACTIVE axis
        # (col_vars under pct="col", else row_vars -- filtered above so a stale cross-axis entry can't
        # leak). tab_setup() dispatches by pct: a row reference (row%/means) vs a per-col_var column
        # reference (col%). A chosen level label is matched by exact equality in diff_index(). Falls
        # back to the (hidden) expert free-text `ref`. ref2 = the OR 2nd reference (a level / first / tot).
        ref          = jmvtab_ref_vector(refLevels_active, self$options$ref),
        ref2         = self$options$ref2,
        # Phase 7g-ii: per-variable level reordering (levelOrder picker) -> a named list of ordered
        # levels; applied post-aggregate in jmv_cache_aggregate() (tier-3 rebuild, tiers 1-2 reused).
        levels_order = jmvtab_levels_order(self$options$levelOrder),
        comp         = self$options$comp,
        ci           = self$options$ci,
        conf_level   = self$options$conf_level,
        stars        = self$options$stars,
        method_cell  = self$options$method_cell,
        method_diff  = self$options$method_diff,
        cleannames   = self$options$cleannames,      # applied at DISPLAY (Phase 7e)
        totaltab     = self$options$totaltab,
        digits       = as.integer(self$options$digits),  # `digits` is a List -> a "0".."6" string
        other_if_less_than = self$options$other_if_less_than,
        add_n        = self$options$add_n,
        add_pct      = self$options$add_pct,
        subtext      = self$options$subtext,
        n_min        = self$options$n_min,           # Phase 7g: small-base display filter (tier 4)
        display      = self$options$display,
        output_list  = FALSE,
        totaltab_name = gettext("Ensemble", domain = "R-tabxplor"),
        total_names   = gettext("Total",    domain = "R-tabxplor"),
        other_level   = gettext("Others",   domain = "R-tabxplor")
      )
    },

    # Render a tab (or list of tabs) to standalone HTML for the Jamovi results iframe.
    # Formatting does not survive kableExtra's classes in Jamovi, so the lightable + bootstrap
    # CSS is injected manually and the "kableExtra" class dropped. (The CSS-only rework is Phase 8.)
    .render_html = function(tabs) {
      tabs_html <- tab_kable(
        tabs,
        wrap_rows = self$options$wrap_rows,
        wrap_cols = self$options$wrap_cols,
        fixed_thead = FALSE,  # not working in Jamovi
        # Phase 7e perf: drop the per-cell hover tooltips -- they roughly DOUBLE the render time
        # (~570 -> ~250 ms on a 21k-row 1x3 table) and the interactive JS does not fire in Jamovi
        # anyway (§7). The full CSS-only render rewrite is Phase 8.
        tooltips = FALSE,
        position = "left"
      ) |>
        kableExtra::scroll_box(
          width = "1080px",
          fixed_thead = FALSE,
          box_css = "border: none; padding: 0; overflow-x: auto !important; display: block; table-layout: auto;",
          extra_css = "margin-left: 0; width: 100%;"
        )

      class(tabs_html) <- "knitr_kable"
      paste0(
        htmltools::includeCSS(system.file("lightable-0.0.1/lightable.css", package = "kableExtra")),
        htmltools::includeCSS(system.file("rmd/h/bootstrap/css/cosmo.min.css", package = "rmarkdown")),
        as.character(tabs_html)
      ) |>
        vctrs::vec_restore(tabs_html)
    },

    # Report an export result via a native jmvcore::Notice (info / error), inserted at the top of
    # the results (dev guide §7.6 / §14). Replaces the old hand-built HTML status box.
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
