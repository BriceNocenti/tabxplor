# PURPOSE: Jamovi module backend for the `jmvtab` analysis (Crosstables).
# ROLE: A thin orchestrator (Phase 7e). `.run()` reads the options from jamovi/jmvtab.a.yaml, restores
#       the multi-tier cache from the hidden `cache_state` result element's $state, calls the pure
#       jmvtab_build() (R/jmvtab-cache.R -- which drives the SAME tab() pipeline with the cache
#       injected), persists the updated store, and renders the table as HTML via tab_kable().
# KEY CONSTRAINTS:
#   - jmvtab.h.R is GENERATED from jmvtab.a.yaml (jmvtools::prepare()); never hand-edit it.
#   - The module runs in Jamovi's bundled R -- keep dependencies to what the package Imports/Suggests.
#   - The cache lives ONLY in $state (survives the engine reset); never rely on R globals (§5.2).
#   - Excel export is the historical typed-path implementation (redesign is roadmap Phase 7f).
# See: dev/tabxplor_jmvtab_cache_design.md ; CLAUDE.md > 1.4.0 roadmap > Phase 7e.

# @rdname jamovi
jmvtabClass <- if (requireNamespace('jmvcore', quietly = TRUE)) R6::R6Class(
  "jmvtabClass",
  inherit = jmvtabBase,
  private = list(

    .run = function() {

      data <- self$data

      # --- Excel folder pre-check (before building the table, to fail fast) ------------------
      folder_path <- NULL
      if (isTRUE(self$options$exportExcel)) {
        folder_path <- path.expand(stringr::str_remove_all(self$options$xl_path, "\"|'"))
        if (!dir.exists(folder_path)) {
          self$results$export_status$setContent(private$.status_box(
            paste0("Error: the specified folder does not exist: <strong>", folder_path, "</strong>"),
            ok = FALSE
          ))
          return(invisible(NULL))
        }
      }

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
      store <- self$results$cache_state$state          # NULL on the first run
      built <- jmvtab_build(data, opts, store)
      self$results$cache_state$setState(built$store)   # persist tiers 1-2 for the next interaction
      tabs  <- built$tabs

      # ci_print controls the [inf;sup] vs pct +- moe display; it is a global option read at
      # format time, so set it around the render and restore it afterwards.
      ci_print_option <- getOption("tabxplor.ci_print")
      options("tabxplor.ci_print" = if (self$options$ci_print == "moe") "moe" else "ci")
      on.exit(options("tabxplor.ci_print" = ci_print_option), add = TRUE)

      # --- Excel export (historical typed-path implementation; redesign is Phase 7f) ----------
      if (isTRUE(self$options$exportExcel)) {
        file_path <- path_sanitize(self$options$xl_filename)
        if (is.null(file_path) || file_path == "") file_path <- "Table.xlsx"
        file_path <- file.path(folder_path, file_path)
        if (!grepl("\\.xlsx$", file_path, ignore.case = TRUE)) file_path <- paste0(file_path, ".xlsx")

        tryCatch({
          xl_result_path <- tab_xl(
            tabs, path = file_path, sheets = "unique",
            open = FALSE, replace = self$options$xl_replace
          ) |>
            capture.output() |>
            stringr::str_remove("^\\[1\\] ") |>
            stringr::str_remove_all("\"") |>
            normalizePath(winslash = "\\")
          self$results$export_status$setContent(private$.status_box(
            paste0("Successfully exported to Excel: <strong>", xl_result_path, "</strong>"), ok = TRUE
          ))
        }, error = function(err) {
          self$results$export_status$setContent(private$.status_box(
            paste0("Excel export failed: <strong>", err$message, "</strong>"), ok = FALSE
          ))
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
        na           = self$options$na,
        levels       = self$options$lvs,             # option named `lvs` (jmvcore has a levels() method)
        ref          = self$options$ref,
        ref2         = self$options$ref2,
        comp         = self$options$comp,
        ci           = self$options$ci,
        conf_level   = self$options$conf_level,
        stars        = self$options$stars,
        method_cell  = self$options$method_cell,
        method_diff  = self$options$method_diff,
        cleannames   = self$options$cleannames,      # applied at DISPLAY (Phase 7e)
        totaltab     = self$options$totaltab,
        digits       = self$options$digits,
        other_if_less_than = self$options$other_if_less_than,
        add_n        = self$options$add_n,
        add_pct      = self$options$add_pct,
        subtext      = self$options$subtext,
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

    # Small colored status box for the Excel-export message.
    .status_box = function(html, ok = TRUE) {
      col <- if (ok) c(bg = "#ecfdf5", border = "#a7f3d0", fg = "#065f46")
             else    c(bg = "#fee2e2", border = "#fecaca", fg = "#7f1d1d")
      paste0(
        "<div style=\"background-color:", col[["bg"]], ";border:1px solid ", col[["border"]],
        ";color:", col[["fg"]], ";padding:10px 12px;border-radius:4px;font-size:0.95em;\">",
        html, "</div>"
      )
    },

    .plot = function(image, ...) {
      TRUE
    }
  )
)
