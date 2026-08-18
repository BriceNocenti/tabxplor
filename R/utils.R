# PURPOSE: Package initialization (.onLoad), factor/list/string utilities (incl. stringi-based
#          tx_str_wrap/tx_str_trunc, the two str_wrap/str_trunc replacements after stringr was dropped).
# ROLE: Entry point for package configuration: .onLoad() seeds the options from their declared table.
# KEY CONSTRAINTS:
#   - TAB_OPTIONS (R/tab-options.R) is the single source of truth for option names and defaults;
#     .onLoad() only SEEDS them, through tx_seed_options().
#   - set_color_style() and set_color_breaks() are defined in tab_classes.R but called here.

# Rlang .data to bind data masking variable in dplyr
#' @keywords internal
#' @importFrom rlang .data
NULL





# Internal stringi-based replacements for the two stringr functions with no direct stringi
# equivalent (Phase 18b-ii: stringr dropped as a dependency). Signatures mirror the stringr
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

# tx_getOption() MOVED to R/tab-options.R in Phase 20b, with the rest of the option subsystem it
# belongs to -- and it had to: tab.R's top-level globalVariables() tail reaches conf_level_default()
# -> tx_option() -> tx_getOption() while the namespace is still being SOURCED, and utils.R sorts
# last of all.

# THE retired-argument catcher for the export backends (Phase 19l).
#
# WHY IT EXISTS. `color_type` and `html_24_bit` were inert 1.3.1 arguments carried as real formals by
# SIX exporters and threaded down whole call chains just to be dropped (~40 sites); `engine`,
# `html_font` and `full_width` joined them when 19l deleted the kableExtra engine. A formal per
# retired argument per backend is the shape this phase exists to delete -- and it is also what made
# `tab_export()` warn once and its child warn a second time for one user mistake.
#
# THE RULE: a retired export argument is absorbed by `...`, named HERE, warned about ONCE per call,
# and never forwarded. Anything else in `...` is passed on untouched, so a real typo still reaches
# R's own "unused argument" error at the leaf. `fn` names the function in the message, so the user
# is told where they wrote it.
#' @keywords internal
TX_INERT_EXPORT_ARGS <- c(
  color_type  = "the text channel always uses the text palette; the CHANNEL is chosen by color = c(text, background)",
  html_24_bit = "exports are always 24-bit",
  engine      = "there is one HTML engine; restyle it with tab_css()",
  html_font   = "the font is a CSS rule -- set it with tab_css() or your own stylesheet",
  full_width  = "table width is a CSS rule -- set it with tab_css() or your own stylesheet"
)

#' @keywords internal
tx_deprecate_inert <- function(dots, fn) {
  hit <- intersect(names(dots), names(TX_INERT_EXPORT_ARGS))
  for (nm in hit) {
    lifecycle::deprecate_soft(
      "2.0.0", I(paste0(fn, "(", nm, " = )")),
      details = c("i" = paste0("Inert since 2.0.0: ", TX_INERT_EXPORT_ARGS[[nm]], "."))
    )
  }
  dots[setdiff(names(dots), names(TX_INERT_EXPORT_ARGS))]
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


  set_color_palette()   # seeds tabxplor.color_style_theme (declared seed = "elsewhere")

  # Phase 20b (KEY 1): EVERY default comes from the declared table -- name, value, doc page and the
  # "seed only if unset" rule alike. R/tabxplor-options.R.
  tx_seed_options()

  # Bind the R-tabxplor gettext catalog to the package's compiled .mo (found under system.file("po");
  # harmless if absent -> English msgids).
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
#'   as a grouped binomial; `vignette("tabxplor")`, section "Multiple-answer
#'   questions", for a worked example.
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





# tx_user_call() -- was this deprecated function called by a USER, or by tabxplor itself?
#
# WHY IT EXISTS (Phase 20a). Two functions being un-exported in 2.1.0 -- tab_prepare() and
# complete_partial_totals() -- still have exactly one caller each, and it is the package's own build.
# `lifecycle::deprecate_soft()` is meant to handle that ("silent for same-package callers"), but it
# treats a testthat run as a direct user call whatever `user_env` says, so under the suite EVERY
# tab() would emit the nudge for a call the user never made. This asks the question the message is
# actually about: whose code called it.
#' @keywords internal
#' @noRd
tx_user_call <- function(env = parent.frame(2)) !identical(topenv(env), asNamespace("tabxplor"))


#' fct_recode helper to recode multiple variables
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' A code-writing convenience for [forcats::fct_recode()], unrelated to cross-tabulation: it
#' prints a ready-to-paste `mutate()` call for a set of factor columns. Nothing in tabxplor uses it.
#' It will be removed in 2.1.0 — copy it into your own project if you rely on it.
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
#' @keywords internal
#' @export
fct_recode_helper <- function(data, .cols = -where(is.numeric), name_in, name_out,
                              freq = NULL, 
                              style = c("mutate", "base"), reminder = TRUE, cat = TRUE) {
  lifecycle::deprecate_soft("2.0.0", "fct_recode_helper()",
                            details = "It writes forcats code and has nothing to do with tables.")
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
  # and drop the `labelled` dependency (Phase 18b-ii).
  var_labs <- purrr::map(data, \(col) attr(col, "label", exact = TRUE))
  var_labs <- var_labs[purrr::map_lgl(var_labs, ~ !is.null(.))]
  with_variable_label_as_title <- length(var_labs) > 0


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



# Phase 19l: `tr_()` (a gettext wrapper) and `po_to_dt()` (a 40-line .po parser) are DELETED.
# They were kept for "the upcoming French translation phase"; that phase shipped using potools
# and gettext() directly, and neither function ever acquired a caller.

# path_sanitize() (a vendored copy of fs::path_sanitize) was removed in Phase 17a: it had no callers.
# jmvtab-export.R is self-contained -- it uses fs::path_sanitize() with its own base-R fallback.

