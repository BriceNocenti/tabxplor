# PURPOSE: Jamovi module backend for the `jmvtab` analysis (Crosstables).
# ROLE: R6 `.run()` reads the options defined in jamovi/jmvtab.a.yaml, builds a colored
#       cross-table with the unified tab() (1.4.0), and renders it as HTML via tab_kable().
# KEY CONSTRAINTS:
#   - jmvtab.h.R is GENERATED from jmvtab.a.yaml (jmvtools::prepare()); never hand-edit it.
#   - The module runs in Jamovi's bundled R -- keep dependencies to what the package Imports/Suggests.
#   - Excel export is the historical typed-path implementation (redesign is roadmap Phase 7f).
# See: dev/tabxplor_1.4.0_jamovi_dev.md ; CLAUDE.md > 1.4.0 roadmap > Phase 7.

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
      # carried as an attribute and must be added back by hand.
      if (!is.null(self$options$wt)) {
        wt <- rlang::sym(self$options$wt)
      } else if (!is.null(attr(data, "jmv-weights"))) {
        data[['.COUNTS']] <- jmvcore::toNumeric(attr(data, "jmv-weights"))
        wt <- rlang::sym(".COUNTS")
      } else {
        wt <- character()
      }

      # --- Variables (inject a dummy when a slot is empty, so a bare table still renders) -----
      row_vars <- if (is.null(self$options$row_vars)) {
        data <- data |> dplyr::mutate(no_row_var = factor("no_row_var"))
        "no_row_var"
      } else self$options$row_vars

      col_vars <- if (is.null(self$options$col_vars)) {
        data <- data |> dplyr::mutate(no_col_var = factor("n"))
        "no_col_var"
      } else self$options$col_vars

      tab_vars <- self$options$tab_vars

      # --- Colors: map the two UI controls onto tab()'s color / color_signif arguments --------
      # "no" -> FALSE (no colors) ; "auto" -> TRUE (smart per-column-type default) ; otherwise the
      # measure string ("diff"/"ratio"/"contrib"/"OR") on the text channel.
      color        <- switch(self$options$color, "no" = FALSE, "auto" = TRUE, self$options$color)
      color_signif <- self$options$color_signif

      # A significance policy needs a difference confidence interval. An explicit color = "diff"/
      # "ratio" forces it inside tab(), but color = TRUE (auto) does not -- so ensure ci = "diff"
      # when a policy is set and the user left ci on "auto".
      ci <- self$options$ci
      if (!isFALSE(color) && color_signif != "ignore" && ci == "auto") ci <- "diff"

      # --- Build the table -------------------------------------------------------------------
      tabs <- tab(
        data,
        row_vars     = tidyselect::all_of(row_vars),
        col_vars     = tidyselect::all_of(col_vars),
        tab_vars     = tidyselect::all_of(tab_vars),
        wt           = !!wt,
        pct          = self$options$pct,
        color        = color,
        color_signif = color_signif,
        OR           = self$options$OR,
        chi2         = self$options$chi2,
        na           = self$options$na,
        levels       = self$options$lvs,  # option named `lvs` (jmvcore::Options has a levels() method)
        ref          = self$options$ref,
        ref2         = self$options$ref2,
        comp         = self$options$comp,
        ci           = ci,
        conf_level   = self$options$conf_level,
        stars        = self$options$stars,
        method_cell  = self$options$method_cell,
        method_diff  = self$options$method_diff,
        cleannames   = self$options$cleannames,
        totaltab           = self$options$totaltab,
        digits             = self$options$digits,
        other_if_less_than = self$options$other_if_less_than,
        add_n              = self$options$add_n,
        add_pct            = self$options$add_pct,
        subtext            = self$options$subtext,
        totaltab_name      = gettext("Ensemble", domain = "R-tabxplor"),
        total_names        = gettext("Total",    domain = "R-tabxplor"),
        other_level        = gettext("Others",   domain = "R-tabxplor")
      )

      # --- Display overrides (work on a single tab or a list of tabs) -------------------------
      tabs <- private$.apply_display(tabs)

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

    # Display-field overrides shared by single tabs and lists of tabs.
    .apply_display = function(tabs) {
      one <- function(tb) {
        if (self$options$display != "auto") {
          tb <- tb |> dplyr::mutate(dplyr::across(
            dplyr::where(is_fmt), ~ set_display(., self$options$display)
          ))
        }
        if (self$options$ci == "cell" && self$options$pct %in% c("row", "col")) {
          tb <- tb |> dplyr::mutate(dplyr::across(
            dplyr::where(is_fmt) &
              -(tidyselect::any_of(c("n", "wn")) & dplyr::where(~ get_type(.) == "n")),
            ~ set_display(., "pct_ci")
          ))
        }
        tb
      }
      if (is.list(tabs) && !is.data.frame(tabs)) purrr::map(tabs, one) else one(tabs)
    },

    # Render a tab (or list of tabs) to standalone HTML for the Jamovi results iframe.
    # Formatting does not survive kableExtra's classes in Jamovi, so the lightable + bootstrap
    # CSS is injected manually and the "kableExtra" class dropped.
    .render_html = function(tabs) {
      tabs_html <- tab_kable(
        tabs,
        wrap_rows = self$options$wrap_rows,
        wrap_cols = self$options$wrap_cols,
        fixed_thead = FALSE,  # not working in Jamovi
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
