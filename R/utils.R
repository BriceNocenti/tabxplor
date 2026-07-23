# PURPOSE: Package initialization (.onLoad), factor/list/string utilities (incl. stringi-based
#          tx_str_wrap/tx_str_trunc, the two str_wrap/str_trunc replacements after stringr was dropped).
# ROLE: Sets all tabxplor.* options defaults. Entry point for package configuration.
# KEY CONSTRAINTS:
#   - .onLoad() is the single source of truth for all option defaults.
#   - Changing a default here affects every user on package load.
#   - set_color_style() and set_color_breaks() are defined in tab_classes.R but called here.

# Rlang .data to bind data masking variable in dplyr
#' @keywords internal
#' @importFrom rlang .data
NULL





# Internal stringi-based replacements for the two stringr functions with no direct stringi
# equivalent (Last Phase b-ii: stringr dropped as a dependency). Signatures mirror the stringr
# originals (arg names + order), so every call site is a plain name swap.

# str_wrap(): wrap each element to `width`; stri_wrap returns a list of lines, join with "\n".
tx_str_wrap <- function(string, width = 80, exdent = 0, whitespace_only = TRUE) {
  wrapped <- stringi::stri_wrap(string, width = width, exdent = exdent,
                                whitespace_only = whitespace_only, simplify = FALSE)
  vapply(wrapped, function(lines) stringi::stri_c(lines, collapse = "\n"), character(1))
}

# str_trunc(): truncate to `width` with a trailing ellipsis (right side only, the sole use).
tx_str_trunc <- function(string, width, ellipsis = "...") {
  too_long <- !is.na(string) & stringi::stri_length(string) > width
  string[too_long] <- stringi::stri_c(
    stringi::stri_sub(string[too_long], 1L, width - stringi::stri_length(ellipsis)),
    ellipsis
  )
  string
}

# Read a tabxplor option that accepts synonym names (a renamed option's old name, or a
# convenience alias); the FIRST name that is set (non-NULL) wins, then `default`. Pass the
# SEEDED/canonical name LAST: the seeded default is always present, so a user's explicit
# legacy/alias value must be checked before it to win. One resolver for every option synonym --
# see ?tabxplor-options. (Phase 17j.)
tx_getOption <- function(names, default = NULL) {
  for (nm in names) {
    v <- getOption(nm)
    if (!is.null(v)) return(v)
  }
  default
}

#' @keywords internal
.onLoad <- function(libname, pkgname) {
  # option "tabxplor.color_style_theme" is seeded by set_color_palette() below.

  # HISTORY: these OMP / data.table thread caps are commented out on purpose. data.table's
  # multithreading once triggered a CRAN thread-count flag that blocked tabxplor's acceptance,
  # until it was shown to originate in data.table itself. Left disabled; re-enable only if
  # CRAN flags threads again.
  # # CRAN OMP THREAD LIMIT
  # if (Sys.info()[['sysname']] == "Linux") {
  #  Sys.setenv("OMP_THREAD_LIMIT" = 2)
  # }

  # data.table::setDTthreads(threads = 2)
  # data.table::getDTthreads(verbose = getOption("datatable.verbose"))


  set_color_palette()

  # Phase 16f: bold the reference/total (+ coloured) cells in the CONSOLE, but only on a front-end that
  # renders ANSI bold at fixed glyph width (Positron / VS Code; RStudio draws it wider and shears table
  # alignment -- rstudio#1721). IDE-detected default; guarded with is.null so a user's .Rprofile choice
  # survives (like tabxplor.color_style_theme). Override any time with options(tabxplor.console_bold = ).
  if (is.null(getOption("tabxplor.console_bold")))
    options("tabxplor.console_bold" = console_bold_default())

  # option "tabxplor.color_breaks" : canonical Phase-13a scales (see set_color_breaks()).
  # pct_ratio is the "only x2" rule (over side only); mean_ratio is asymmetric (4 over / 3 under);
  # mean_diff = NULL restores the standardized (Glass's delta) default.
  options("tabxplor.color_breaks" = default_color_scales())

  options("tabxplor.print" = "console") # options("tabxplor.print" = "kable")

  options("tabxplor.kable_html_font" =
            '"DejaVu Sans", "Arial", arial, helvetica, sans-serif') # Condensed ?

  options("tabxplor.output_kable" = FALSE)

  options("tabxplor.cleannames" = FALSE)

  options("tabxplor.export_dir" = NULL)

  options("tabxplor.kable_popover" = FALSE)

  options("tabxplor.tab_kable_tooltips" = TRUE)

  options("tabxplor.ci_print" = "ci") # or "moe"

  # Phase 10: how a Total column's in-cell base is shown when a table's col_vars have DIFFERING bases
  # (e.g. na = "drop"). "off" (default) = each row's own base, uniform "{pct} (n={n})"; "range" =
  # the per-row "[min;max]" across col_vars; "min" = the smallest (safest) base. Read by both the
  # console (tab_fold_addn_incell) and the exporter prep (tab_totcol_range), so set once here.
  options("tabxplor.totcol_range" = "off")

  # Phase 3a significance stars (universal CI-inclusion). `stars` default (OPT-IN: FALSE, so a plain
  # tab() stores no per-cell pvalue and shows no stars; tab_reg() sets stars = TRUE itself), and the
  # star thresholds/labels read by get_stars(). Thresholds are nested p-value cutoffs.
  options("tabxplor.stars"         = FALSE)
  options("tabxplor.signif_levels" = c(0.10, 0.05, 0.01))
  options("tabxplor.signif_labels" = c("*", "**", "***"))

  # Weighted inference (§14): unweighted n by default; opt in to Kish n_eff = (Sum w)^2/Sum w^2 for the
  # weighted CIs/tests. Last Phase s made it uniform: n_eff now backs EVERY weighted descriptive CI --
  # factor proportions (tab_ci) AND means (num_core) AND the color="OR" interval AND tab_reg's empirical
  # companions -- plus the whole-table chi2/F tests (Last Phase j, first-order Rao-Scott). It needs the
  # microdata weights (tab_counts on pre-aggregated counts cannot apply it). Full design test: `test="survey"`.
  options("tabxplor.kish_neff"     = FALSE)

  # Phase 3b: which one-way ANOVA F is DISPLAYED for mean columns ("welch" = robust default,
  # matching oneway.test(var.equal=FALSE); "classic" = pooled-variance F). Both are always
  # stored in the `test` attribute; this only picks the p-value shown in the p-value row/stars.
  options("tabxplor.anova"         = "welch")

  # Phase 16a / Last Phase j: how many crosstab test rows the EXPORTERS append (md/html/Excel).
  # "summary" (the new default) = statistic + effect size + p-value (the console's full block, minus N,
  # already shown by add_n); "stat" = statistic + p-value; "pvalue" = the single p-value row; "all" =
  # summary. Console always shows the full N/statistic/effect-size/p-value block, so this is export-only.
  options("tabxplor.test_lines"    = "summary")

  # Phase 16e: the colour-legend style in EXPORTS (md/html/Excel). "prose" (default) = the full
  # sentences; "terse" = the compact one-line console form. The console itself is always terse.
  options("tabxplor.legend_style"  = "prose")

  # Default confidence level for the intervals and significance tests. The per-call `conf_level`
  # argument of tab() / tab_num() / tab_ci() / tab_reg() (and its wrappers) overrides it; it is also
  # the fallback alpha of the `contrib` colour-significance gate. Single source of truth (Last Phase c).
  options("tabxplor.conf_level"    = 0.95)

  # Phase 6: the `tabxplor.compact` option is dropped, superseded by the `output_list`
  # argument of tab() (default FALSE = merge; TRUE = list). tab_many()'s deprecated `compact`
  # argument still works (mapped onto the output shape).

  options("tabxplor.always_add_css_in_tab_kable" = TRUE)

  # tab_kable() render engine. Phase 14e makes "html" the DEFAULT: the home-built engine is
  # dependency-free, self-contained (<table> + one stylesheet), ~3x faster, restyleable (its geometry
  # is CSS classes, not inline styles) and the only engine that can follow a theme = "auto" toggle.
  # "kableExtra" keeps the legacy renderer (its own themes, baked at render time). R/tab-render-html.R.
  options("tabxplor.tab_kable_engine" = "html")

  # The NUMBER font of each font-bearing export. Text (row labels, headers) always stays Condensed.
  # Phase g: html/md numbers are MONOSPACE by default -- one lever `tab_kable_num_font` (was: a
  # proportional font + a `_stars` monospace variant switched per table). Excel/plot keep the per-stars
  # split (their alignment complaint is stars-specific, and the review did not touch them):
  #   - html/md    -> ONE CSS font-family stack (tab_css()'s `.tx-num`), monospace.
  #   - Excel      -> two single font names, no-stars/stars (xlsx has no fallback list, so the option IS
  #     the fallback).
  #   - tab_plot   -> ONE graphics family, applied to the whole plot body only when the table has stars
  #     (ggpubr has no per-column font); "" keeps the ggpubr default. tab_plot() is superseded.
  options("tabxplor.tab_kable_num_font"       = tx_num_font_html_stars)   # monospace
  options("tabxplor.xl_font_num"        = "DejaVu Sans")                  # no stars (proportional)
  options("tabxplor.xl_font_num_stars"  = "Cascadia Mono")               # stars (monospace)
  options("tabxplor.xl_font_text"       = "DejaVu Sans Condensed")
  # keep odds ratios as real numbers in Excel instead of "1/x" text; per-call tab_xl(or_numeric =).
  options("tabxplor.xl_or_numeric"      = FALSE)
  options("tabxplor.plot_num_font"      = "Cascadia Mono")                # applied only when stars

  # Phase 13d: the EXPORT theme -- "light" (default), "dark", or "auto" (follow the reader's colour
  # scheme: their OS, plus any dark-mode toggle of the host page). "auto" needs a stylesheet, so only
  # tab_kable(engine = "html") / tab_md() / tab_css() honour it; static backends (tab_xl, tab_plot, the
  # kableExtra engine) resolve it to "light". See R/tab-css.R.
  # WARNING: NOT `tabxplor.color_style_theme`, which is a different axis -- that one is the CONSOLE
  # palette theme, set by set_color_palette() (which auto-detects the editor's theme, Phase 14g).
  # DESIGN (Phase 14k): "light" STAYS the default and "auto" is opt-in -- this reverses the roadmap's
  # plan to flip it. Unlike the console (a pane we can measure), an export is read who-knows-where, so
  # a dark table must be asked for, not inferred. tab_kable()'s Viewer print is the one place "auto" is
  # resolved in R rather than by the browser: only R can see the editor around the pane.
  options("tabxplor.theme" = "light")

  # Phase 13d: whether tab_kable(engine = "html") inlines the stylesheet with each table (TRUE =
  # self-contained: Viewer, jamovi, standalone .html). Set FALSE in a many-table .Rmd/.qmd that emits
  # tab_css() once at the top -- the CSS is table-independent, so one copy styles every table.
  # Phase 17j: renamed tabxplor.kable_css -> tabxplor.tab_kable_css (aligns with the tab_kable_* family).
  # The old name still works (read via tx_getOption()); only the new one is seeded here.
  options("tabxplor.tab_kable_css" = TRUE)

  # Phase 14i: which variable NAMES the exporters annotate a table with. "both" (default) = today's
  # behaviour; "rows" = only the row-variable names (a merged table's name column); "cols" = only the
  # col_var spanning-name row; "none" = neither. It never touches a level column's HEADER (`marital`
  # on a single-row_var table, `year` on a kept tab_var): that header identifies the column, costs no
  # width, and is the col-side rule's mirror (which removes the span row, never the level names).
  # Per-call `var_names =` on tab_kable/tab_md/tab_xl/tab_plot/tab_export overrides. R/tab-export-prep.R.
  options("tabxplor.var_names" = "both")

  # Phase k: opt-in display-swap of variable NAMES for variable LABELS (the haven/labelled `label`
  # attribute, captured at build into meta$vars$var_labels). FALSE (default) shows names; TRUE shows
  # the label where a variable has one (else its name). EXPORTS only (md/html/xl/plot) -- the console
  # keeps canonical names, which disambiguate. Structure is unchanged, so name-based select()/reference
  # still work. is.null-guarded so an Rprofile opt-in survives load. R/tab-export-prep.R var_label_map().
  if (is.null(getOption("tabxplor.var_labels"))) options("tabxplor.var_labels" = FALSE)

  # Phase 8: opt-in parallel build of many tables in ONE tab() call (Suggests-only {mirai}).
  # FALSE = off (default); TRUE = auto workers; an integer = that many daemons. `parallel_min` is
  # the smallest row_var count worth dispatching (fewer -> serial: setup would outweigh the gain).
  # See R/tab-parallel.R + dev/tabxplor_2.0.0_decisions.md 26.
  options("tabxplor.parallel"     = FALSE)
  options("tabxplor.parallel_min" = 2L)

  # Phase 17i: the jamovi live-UI caches fingerprint each column by class / factor levels / NA-count
  # (cheap, per-column). Blind spot: a same-shape VALUE edit (values changed, structure unchanged) is
  # not caught -> a stale cache HIT (self-heals on the next structural change). TRUE forces a full-value
  # column hash (slower, exact) in BOTH modules -- the escape hatch for the paranoid. is.null-guarded so
  # an Rprofile opt-in survives load. See ?tabxplor-options ; R/jmvtab-cache.R jmv_col_fp().
  if (is.null(getOption("tabxplor.jmv_full_hash"))) options("tabxplor.jmv_full_hash" = FALSE)

  # Phase 13b: the colour-legend language. "auto" follows the R/OS locale (English fallback); "en"/"fr"
  # force it. Per-call `lang =` on the exporters overrides. Bind the R-tabxplor gettext catalog to the
  # package's compiled .mo (found under system.file("po"); harmless if absent -> English msgids).
  options("tabxplor.lang" = "auto")
  po <- system.file("po", package = pkgname)
  if (nzchar(po)) try(bindtextdomain("R-tabxplor", po), silent = TRUE)

  invisible()
}

# Phase 8: release the persistent mirai daemon pool when tabxplor is unloaded (a CRAN cleanliness
# backstop; tab_parallel_stop() lets users do it earlier). No pool is ever warmed at load.
#' @keywords internal
.onUnload <- function(libpath) {
  tab_parallel_stop()
}

# getOption("tabxplor.color_breaks")
# getOption("tabxplor.color_style_theme")
# get_color_breaks()
# get_color_style()




#Functions and options to work with factors and lists -------------

#' A regex pattern to clean the names of factors.
#' @keywords internal
# @export
cleannames_condition <- function()
  "^[^- ]+-(?![[:lower:]])|^[^- ]+(?<![[:lower:]])-| *\\(.+\\)"





#' Create a score variable by counting factors at their first level
#'
#' Builds an integer score column that counts, for each row, how many of the
#' listed factors sit at their **first level**. Each factor contributes 1 when
#' it equals its first level and 0 otherwise, so the score ranges from 0 to
#' `length(vars_list)`. This is the natural way to turn a battery of yes/no
#' (or agree/disagree) survey items into a single summed score, and it feeds
#' the grouped-binomial outcome of [tab_reg()] (its `trials` argument).
#'
#' @param data A data.frame.
#' @param name The name of the score variable to create (unquoted or a string);
#'   an existing column of that name is replaced.
#' @param vars_list The factors to count, as a character vector. For each one
#'   only its **first level** counts (as 1); every other level, including
#'   missing values, counts as 0.
#'
#' @return `data` with the integer score column `name` added (or replaced).
#'
#' @details
#'   The "first level" is `levels(as.factor(x))[1]` -- the reference level of the
#'   factor. Non-factor columns are coerced with [as.factor()]. Missing values
#'   are folded into an explicit `"NA"` level before counting (via
#'   [forcats::fct_na_value_to_level()]), so an `NA` never matches the first
#'   level and contributes 0.
#'
#' @seealso [tab_reg()] and its `trials` argument for modelling a summed score
#'   as a grouped binomial; `vignette("tabxplor-programming")` for a worked
#'   example.
#'
#' @export
#'
#' @examples
#' data <- tibble::tibble(group = factor(c("G1", "G1", "G2", "G2", "G3", "G3")),
#'                        a = factor(c("Oui", "Oui", "Oui", "Oui", "Non", "Oui")),
#'                        b = factor(c("Oui", "Non", "Non", "Oui", "Non", "Oui")),
#'                        c = factor(c("Oui", "Oui", "Non", "Non", "Oui", "Oui")))
#' data |>
#'   score_from_lv1("score", vars_list = c("a", "b", "c")) |>
#'   tab(group, score, digits = 1)
score_from_lv1 <- function (data, name, vars_list) {
  name <- rlang::ensym(name)

  data <- data |> dplyr::select(-tidyselect::any_of(as.character(name)))

  new_data <- data |> dplyr::mutate(!!rlang::sym(name) := 0L)

  new_data <-
  purrr::reduce(
    vars_list,
    .init = dplyr::mutate(new_data, dplyr::across(
      tidyselect::all_of(vars_list), ~ forcats::fct_na_value_to_level(., "NA"))),

    .f = ~ dplyr::mutate(.x, !!name := dplyr::if_else(
      condition = !!rlang::sym(.y) == levels(as.factor(!!rlang::sym(.y)))[1],
      true  = !!name + 1L,
      false = !!name
    )
    )
  )

  var_final_ <- dplyr::pull(new_data, as.character(name))
    #dplyr::select(new_data, tidyselect::all_of(as.character(name)))

  data |> tibble::add_column(!!rlang::sym(name) := var_final_)
}


# data <- dplyr::select(forcats::gss_cat, -where(is.numeric))
# name_in = "data"
# name_out = "data"
# style = "base"
# reminder = TRUE
# cat = TRUE




#' fct_recode helper to recode multiple variables
#'
#' @param data The data frame.
#' @param .cols <\link[tidyr:tidyr_tidy_select]{tidy-select}> The variables to recode.
#' @param name_in The name of the input data frame. Default to the expression given in `data`.
#' @param name_out The name of the output data frame, if different from the
#' input data frame.
#' @param style Default is to use `dplyr::mutate()`. Set to `base` to use `data$var <-` style.
#' @param reminder By default, a reminder of the syntax (`"new" = "old"`) is printed.
#'  Set to `FALSE` to remove it.
#' @param freq Set to `TRUE` to print frequency and count of each level as comment.
#' Set to `FALSE` to avoid this behavior. By default, frequencies and counts are 
#' only calculated when less than 6 variables are provided.
#' @param cat By default the result is written in the console if there are less than
#' 6 variables, written in a temporary file and opened otherwise. Set to
#' false to get a data frame with a character variable instead.
#'
#' @return When the number of variables is less than 5, a text in console as a side effect.
#' With more than 5 variables, a temporary R file. A `tibble` with the recode text as a
#' character variable is returned invisibly (or as main result if `cat = TRUE`).
#' When a column carries a variable label (its `label` attribute), it is used as title in a comment.
#' @export
fct_recode_helper <- function(data, .cols = -where(is.numeric), name_in, name_out,
                              freq = NULL, 
                              style = c("mutate", "base"), reminder = TRUE, cat = TRUE) {
  no_name_in <- missing(name_in)
  if (no_name_in) {
    name_in <- deparse(substitute(data))
    if (stringi::stri_detect_regex(name_in, "\\(")) {
      name_in <-
        stringi::stri_extract_first_regex(name_in, "[^\\(]+$") |>
        stringi::stri_replace_all_regex("\\).*$", "")
      # name_in <- "data"
    }
  }
  if (missing(name_out)) name_out <- name_in # if (missing(name_in)) {"data"} else {name_in}

  pos_cols <- tidyselect::eval_select(rlang::enquo(.cols), data)
  data <- data[pos_cols]

  # Variable labels as titles: the `label` attribute (e.g. from haven / labelled-imported data).
  # get_variable_labels() returned exactly this per-column named list, so read it with base attr()
  # and drop the `labelled` dependency (Last Phase b-ii).
  var_labs <- purrr::map(data, \(col) attr(col, "label", exact = TRUE))
  var_labs <- var_labs[purrr::map_lgl(var_labs, ~ !is.null(.))]
  with_variable_label_as_title <- length(var_labs) > 0

  # var_labs <- purrr::imap(var_labs, ~ paste0(.y, " with a lot of text"))

  data <- data |> dplyr::mutate(dplyr::across(.cols = dplyr::everything(), .fns = as.factor))
  
  # By default, if not chosen by user, only calculate frequencies for less that 10 vars
  if (is.null(freq)) {
    freq <- ncol(data) <= 5
  }
  
  if (freq) { # With frequencies and counts helpers 
    frequencies <- names(data) |> 
      purrr::map(
        # Phase 14p: fully qualify the non-base calls. `filter` is NOT imported, so the bare form
        # resolved to stats::filter(), which evaluated `!is_totrow(pct)` OUTSIDE the data mask ->
        # "object 'pct' not found" (the reported freq = TRUE crash). A no-col_var pct = "col" table
        # is `<row_var> | pct | n`, so both columns exist and are read straight.
        ~ tab_plain(data, !!rlang::sym(.x), pct = "col", na = "drop") |>
          dplyr::filter(!is_totrow(.data$pct)) |>
          dplyr::rename_with(~ "lvs", .cols = 1) |>
          dplyr::mutate(lvs = paste0("\"",
                              stringi::stri_replace_all_regex(lvs, "\"", "'"),
                              "\""),
                 pct = format(.data$pct),
                 n   = format(n),
                 txt = paste0(stringi::stri_pad(pct, max(stringi::stri_length(pct))),
                              " ",
                              stringi::stri_pad(n, max(stringi::stri_length(n)))
                 )
          ) |>
          dplyr::select(lvs, txt)
      ) |>
      purrr::set_names(names(data)) 
    
    recode <- frequencies |>
      purrr::map(
        ~ paste0(stringi::stri_pad(.x$lvs, max(stringi::stri_length(.x$lvs)), "right"), " = ",
                 stringi::stri_pad(.x$lvs, max(stringi::stri_length(.x$lvs)), "right"), 
                 ", # ", 
                 .x$txt
        )
      ) |>
      purrr::map(~ paste0(., collapse = "\n"))
    
  } else { # Without frequencies and counts helpers
    recode <- data |>
      purrr::map(~ paste0("\"",
                          #stringi::stri_escape_unicode(
                          stringi::stri_replace_all_regex(
                            levels(.), "\"", "'"
                            #)
                          ),
                          "\"")) |>
      purrr::map(
        ~ paste0(stringi::stri_pad(., max(stringi::stri_length(.)), "right"), " = ",
                 stringi::stri_pad(., max(stringi::stri_length(.)), "right"), collapse = ",\n")
      )
    
  }

 
  


  if (with_variable_label_as_title) {
  titles <- purrr::map(
    names(recode),
    ~ dplyr::if_else(. %in% names(var_labs),
                     true  = paste0("# ", ., " : ", var_labs[[.]] , "\n"),
                     false = ""
    )
  )

  } else {
    titles <- purrr::map(recode, ~ "")
  }


  reminder <- if (reminder) {'   # "new" = "old" '} else {''}

  recode <-
    switch(
      style[1],
      "base"   = purrr::pmap(
        list(recode, names(recode), titles),
        ~ paste0(..3,
                 name_out, "$", ..2, " <- fct_recode(\n",
                 name_in, "$", ..2, ',', reminder, '\n',
                 ..1, "\n)\n\n"
        )) |>
        purrr::flatten_chr(),

      "mutate" =
        c(
          paste0(name_in, " |>\n", "mutate(", "\n"), # reminder
          purrr::pmap(
            list(recode, names(recode), titles),
            ~ paste0(..3,
                     ..2, " = fct_recode(", reminder, "\n",
                     ..2, ',', '\n',
                     ..1, "\n),\n\n"
            )) |> purrr::flatten_chr(),
          ")\n"
        )
    ) |>
    tibble::as_tibble() |>
    rename("recode" = "value")

  if (cat == FALSE) return(recode)

  if (ncol(data) <= 5) {
    cat(recode$recode)
  } else {
    path <- tempfile("", fileext = ".R")
    writeLines(recode$recode, path, useBytes = TRUE)

    if (requireNamespace("rstudioapi", quietly = TRUE)) {
      rstudioapi::navigateToFile(path)
    } else {
      file.show(path)
    }

  }

  invisible(recode)
}

#' `forcats::gss_cat` test dataframe, from US General Social Survey, 
#'   but formatted with merged levels for cleaner tables,
#'   and first levels chosen to be used as references (for color helpers, regressions, etc.)
#' @export
gss_cat_data_formatting <- function() {
forcats::gss_cat |>
dplyr::mutate(
  married = factor(dplyr::if_else(marital == "Married",
  "01-Married",
  "02-Not married")
),
black = factor(dplyr::if_else(race == "Black",
  "01-Black",
  "02-Not black")
),
income25k = factor(dplyr::if_else(rincome == "$25000 or more",
"01-$25000 or more",
"02-Less than 25k")
),
race = forcats::fct_relevel(race, "White", "Black", "Other"), 
marital = forcats::fct_relevel(marital, "Married", "Separated", "Divorced", "Widowed", "Never married", "No answer"),
year = as.factor(year),
  
dplyr::across(dplyr::where(is.factor), ~ forcats::fct_recode(., "NULL" = "No answer", "NULL" = "Refused", "NULL" = "Don't know", "NULL" = "Not applicable")),

rincome = forcats::fct_recode(   # "new" = "old" 
  rincome,
  "1-Lt $10000"      = "Lt $1000"       , #  1%   286
  "1-Lt $10000"      = "$1000 to 2999"  , #  2%   395
  "1-Lt $10000"      = "$3000 to 3999"  , #  1%   276
  "1-Lt $10000"      = "$4000 to 4999"  , #  1%   226
  "1-Lt $10000"      = "$5000 to 5999"  , #  1%   227
  "1-Lt $10000"      = "$6000 to 6999"  , #  1%   215
  "1-Lt $10000"      = "$7000 to 7999"  , #  1%   188
  "1-Lt $10000"      = "$8000 to 9999"  , #  2%   340
  "2-$10000 to 14999" = "$10000 - 14999", #  5% 1 168
  "3-$15000 to 24999" = "$15000 - 19999", #  5% 1 048
  "3-$15000 to 24999" = "$20000 - 24999", #  6% 1 283
  "4-$25000 or more"  = "$25000 or more"  # 34% 7 363
) |> 
forcats::fct_relevel(sort) |>
as.ordered(),

  
party3 = forcats::fct_recode(   # "new" = "old" 
  partyid,
  "NULL"                 = "No answer"         , #  1%   154
  "NULL"                 = "Don't know"        , #  0%     1
  "3-Republican"         = "Strong republican" , # 11% 2 314
  "3-Republican"         = "Not str republican", # 14% 3 032
  "3-Republican"         = "Ind,near rep"      , #  8% 1 791
  "2-Independent, other" = "Independent"       , # 19% 4 119
  "2-Independent, other" = "Other party"       , #  2%   393
  "1-Democrat"           = "Ind,near dem"      , # 12% 2 499
  "1-Democrat"           = "Not str democrat"  , # 17% 3 690
  "1-Democrat"           = "Strong democrat"   , # 16% 3 490
  ) |> forcats::fct_relevel(sort),
  

relig = forcats::fct_recode( 
  relig,
  "1-Protestant"        = "Protestant"             , # 50% 10 846
  "2-Catholic"          = "Catholic"               , # 24%  5 124
  "3-Other christian"   = "Christian"              , #  3%    689
  "3-Other christian"   = "Orthodox-christian"     , #  0%     95
  "4-Jewish"            = "Jewish"                 , #  2%    388
  "5-Buddhist/Hinduist" = "Hinduism"               , #  0%     71
  "5-Buddhist/Hinduist" = "Buddhism"               , #  1%    147
  "6-Muslim"            = "Moslem/islam"           , #  0%    104
  "7-Other"             = "Inter-nondenominational", #  1%    109
  "7-Other"             = "Native american"        , #  0%     23
  "7-Other"             = "Other eastern"          , #  0%     32
  "7-Other"             = "Other"                  , #  1%    224
  "8-None"              = "None"                   , # 16%  3 523
  "NULL"                = "No answer"              , #  0%     93
  "NULL"                = "Don't know"             , #  0%     15
) |> forcats::fct_relevel(sort),

)
}






# Adapt purrr::map_if function to pmap et map2
# (when FALSE the result is the first element of .l, or the content of .else)


#tidyselect:::where
# MIT + Lience : https://tidyselect.r-lib.org/LICENSE.html
# Thanks to Hadley Wickham and Lionel Henry
#' @keywords internal
where <- function (fn)
{
  predicate <- rlang::as_function(fn)
  function(x, ...) {
    out <- predicate(x, ...)
    if (!rlang::is_bool(out)) {
      rlang::abort("`where()` must be used with functions that return `TRUE` or `FALSE`.")
    }
    out
  }
}




# formats_SAS_to_R() (INSEE SAS formats -> R fct_recode code) moved to dev/formats_SAS_to_R.R in
# Phase 17a: unexported, no callers, a personal maintainer tool rather than package surface.















# Escaped characters ----
#' @keywords internal
unbrk      <- stringi::stri_unescape_unicode("\\u202f") # unbreakable space
sigma_sign <- stringi::stri_unescape_unicode("\\u03c3") # sigma for sd
mult_sign  <- stringi::stri_unescape_unicode("\\u00d7") # multiply sign (ratio >= 1)
div_sign   <- stringi::stri_unescape_unicode("\\u00f7") # divide sign (ratio < 1, shows 1/ratio)
# Phase 14d/14e: FIGURE SPACE -- defined by Unicode to be exactly as wide as a digit in fonts with
# tabular figures, which is what format()'s alignment padding assumes. An ASCII space is only HALF a
# digit in DejaVu Sans (measured: 651 vs 1303/2048 em), so "100% (n=  849)" aligned in the console
# collapsed into a ragged mess in html/Excel -- and CSS additionally collapses runs of ASCII spaces.
# Used where the output is rendered in a PROPORTIONAL font (html, Excel); console/markdown, which are
# read in a monospace font, keep the ASCII space (see format.tabxplor_fmt(pad =)).
fig_space  <- stringi::stri_unescape_unicode("\\u2007")

# Phase 14m-ii (reworked): the number font is CONDITIONAL on whether the table shows significance
# stars. A plain table keeps the proportional DejaVu Sans it always had (compact, better-looking); only
# a STARRED table switches to a MONOSPACE stack, because that is the one case where alignment breaks -- a
# proportional "*" is narrower than a digit, so a starred cell slides out of its column. In a monospace
# font every glyph (digits, "%", brackets, "*", the figure-space pad) is one width, so stars and
# "(n=...)" composites line up. The TEXT channel (row labels, headers) always stays DejaVu Sans Condensed.
#   - tx_num_font_html_stars : the html/md `.tx-num` font -- MONOSPACE (Phase g: numbers are monospace
#     by default so figures stay column-aligned, worse under the bold references / significant cells the
#     html render adds). Cascadia Mono target, then Cascadia Code (same metrics, far more widely
#     installed), then per-OS monos, then the generic `monospace`. `ui-monospace` is deliberately absent:
#     it resolves to the OS's OWN mono (SF Mono, ...), which would override Cascadia; at the tail it is
#     never reached (Menlo/Consolas/DejaVu already cover every OS), so it would only add noise.
# The one option is tabxplor.tab_kable_num_font (default = this monospace stack); tab_css() stays
# table-independent (one `.tx-num` rule, no per-table class needed).
tx_num_font_html_stars <-
  '"Cascadia Mono", "Cascadia Code", Menlo, Consolas, "DejaVu Sans Mono", monospace'

# # Not working
# # Css link towards https://github.com/web-fonts/dejavu-sans-condensed
# # @export
# css_deja_vu_sans_condensed <- function() {
#
#   # "@font-face {
#   #   font-family: 'DejaVu Sans Condensed';
#   #     url('../inst/fonts/dejavu-sans-condensed-webfont.woff') format('woff'),
#   #     url('../inst/fonts/dejavu-sans-condensed-webfont.ttf') format('truetype'),
#   # }" |>
#   #   stringi::stri_replace_first_regex("\n", "")
#
#   #"@font-face{font-family:'DejaVu Sans Condensed';src:url(https://github.com/web-fonts/dejavu-sans-condensed/fonts/dejavu-sans-condensed-webfont.eot);src:url(https://github.com/web-fonts/dejavu-sans-condensed/fonts/dejavu-sans-condensed-webfont.eot?#iefix) format('embedded-opentype'),url(https://github.com/web-fonts/dejavu-sans-condensed/fonts/dejavu-sans-condensed-webfont.woff2) format('woff2'),url(https://github.com/web-fonts/dejavu-sans-condensed/fonts/dejavu-sans-condensed-webfont.woff) format('woff'),url(https://github.com/web-fonts/dejavu-sans-condensed/fonts/dejavu-sans-condensed-webfont.ttf) format('truetype'),url(https://github.com/web-fonts/dejavu-sans-condensed/fonts/dejavu-sans-condensed-webfont.svg#dejavu_sans_condensedregular) format('svg')}"
#
#   "@font-face {
#    font-family: 'DejaVu Sans Condensed';
#     src: url('https://github.com/web-fonts/dejavu-sans-condensed/fonts/dejavu-sans-condensed-webfont.eot'); /* IE9 Compat Modes */
#       src: url('https://github.com/web-fonts/dejavu-sans-condensed/fonts/dejavu-sans-condensed-webfont.eot?#iefix') format('embedded-opentype'), /* IE6-IE8 */
#       url('https://github.com/web-fonts/dejavu-sans-condensed/fonts/dejavu-sans-condensed-webfont.woff2') format('woff2'), /* Super Modern Browsers */
#       url('https://github.com/web-fonts/dejavu-sans-condensed/fonts/dejavu-sans-condensed-webfont.woff') format('woff'), /* Pretty Modern Browsers */
#       url('https://github.com/web-fonts/dejavu-sans-condensed/fonts/dejavu-sans-condensed-webfont.ttf') format('truetype'), /* Safari, Android, iOS */
#       url('https://github.com/web-fonts/dejavu-sans-condensed/fonts/dejavu-sans-condensed-webfont.svg#dejavu_sans_condensedregular') format('svg'); /* Legacy iOS */
#   }"
#
#   }





# ggpubr functions (for tab_plot() as tableGrob ) ----

# ggpubr:::is_tablegrob
#' @keywords internal
is_tablegrob <- function (tab) {
  inherits(tab, "gtable") & inherits(tab, "grob")
}

# ggpubr:::is_ggtexttable
#' @keywords internal
is_ggtexttable <- function (tab) {
  !is.null(attr(tab, "ggtexttableGrob"))
}

# ggpubr:::as_ggtexttable
#' @keywords internal
as_ggtexttable <- function (tabgrob) {
  res <- ggpubr::as_ggplot(tabgrob)
  attr(res, "ggtexttableGrob") <- tabgrob
  res
}

# ggpubr:::get_tablegrob
#' @keywords internal
get_tablegrob <- function (tab)
{
  if (is_ggtexttable(tab)) {
    tabgrob <- attr(tab, "ggtexttableGrob")
  }
  else if (is_tablegrob(tab)) {
    tabgrob <- tab
  }
  else {
    stop("tab should be an object from either ggpubr::ggtexttable() or gridExtra::tableGrob().")
  }
  tabgrob
}

# ggpubr:::tab_return_same_class_as_input
#' @keywords internal
tab_return_same_class_as_input <- function (tabgrob, input) {
  if (is_ggtexttable(input)) {
    return(as_ggtexttable(tabgrob))
  }
  else if (is_tablegrob(input)) {
    return(tabgrob)
  }
  tabgrob
}

### https://stackoverflow.com/questions/32106333/align-grob-at-fixed-top-center-position-regardless-of-size
justify_grob <- function(grob, hjust = "left", vjust = "top", pad = 5){
  w <- sum(grob$widths)
  h <- sum(grob$heights)
  xy <- list(x = switch(hjust,
                        center = 0.5 + grid::unit(pad, "points"),
                        left = 0.5*w + grid::unit(pad, "points"),
                        right = grid::unit(1,"npc") - 0.5*w - grid::unit(pad, "points")),
             y = switch(vjust,
                        center = 0.5 + grid::unit(pad, "points"),
                        bottom = 0.5*h + grid::unit(pad, "points"),
                        top = grid::unit(1,"npc") - 0.5*h - grid::unit(pad, "points") ) )
  if (is.null(grob$vp)) {
    grob$vp <- grid::viewport(x = xy[[1]], y = xy[[2]] )
  } else {
    grob$vp$x <- xy[[1]]
    grob$vp$y <- xy[[2]]
  }

  return(grob)
}



# translation functions ----

#' @keywords internal
tr_ <- function(...) {
  enc2utf8(gettext(paste0(...), domain = "R-tabxplor"))
}

#' @keywords internal
po_to_dt <- function(file) {
  po_base <- readLines(file, encoding = "UTF-8")
  po_meta <- po_base[!dplyr::cumany(po_base == "")]

  po <- tibble::tibble(base = po_base[dplyr::cumany(po_base == "")])


  po <- po |>
    dplyr::filter(.data$base != "") |>
    dplyr::mutate(
      ok = stringi::stri_detect_regex(.data$base, "#:|msgid|msgstr"),
      ok = cumsum(as.integer(.data$ok))
    ) |>
    dplyr::group_by(!!rlang::sym("ok")) |>
    dplyr::group_split() |>
    purrr::map(
      ~ paste0(.$base, collapse = "") |>
        stringi::stri_replace_all_regex("\"", "")
    ) |>
    purrr::flatten_chr()

  po <- tibble::tibble(text = po) |>
    dplyr::mutate(
      type  = stringi::stri_extract_first_regex(.data$text, "^[^ ]+ ") |> stringi::stri_trim(),
      group = cumsum(as.integer(.data$type == "#:")),
      .before = 1
    ) |>
    dplyr::mutate(
      text = stringi::stri_replace_first_regex(.data$text, "^[^ ]+ ", ""),
    ) |>
    tidyr::pivot_wider(id_cols  = "group", names_from = "type", values_from = "text") |>
    dplyr::select(-"group") |>
    `attr<-`("meta", po_meta)

  return(po)
}


# path_sanitize() (a vendored copy of fs::path_sanitize) was removed in Phase 17a: it had no callers.
# jmvtab-export.R is self-contained -- it uses fs::path_sanitize() with its own base-R fallback.

