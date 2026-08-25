# PURPOSE: package initialization plus the shared factor / list / string utilities.
# ROLE: .onLoad() SEEDS the package options and the colour palette; everything else here is a small
#   helper with no home of its own -- the two stringi-based replacements for str_wrap/str_trunc, the
#   NAME wrapper beside them, the retired-export-argument catcher, and the three exported user helpers
#   (score_from_lv1(), gss_cat_data_formatting(), and the deprecated fct_recode_helper()).
# KEY CONSTRAINTS:
#   - TAB_OPTIONS (R/tab-options.R) is the single source of truth for option names and defaults;
#     .onLoad() only seeds them, through tx_seed_options().
#   - set_color_style() and set_color_breaks() are defined in tab_classes.R but called from here.
#   - A LEVEL LABEL IS PROSE, A VARIABLE NAME IS A COMPOUND WORD: tx_str_wrap() breaks the first on
#     whitespace, tx_wrap_name() breaks the second at the seams a name is actually built from
#     (`_`, `.`, `*`, camelCase). One of them was missing, so a snake_case name met no break
#     opportunity and no width could ever hold it.
#   - This file sorts second-to-last in C collation (only zzz-fact-keys.R follows), so nothing in
#     the package may depend on it at SOURCE time.
# See: CLAUDE.md § tabxplor architecture.

# Rlang .data to bind data masking variable in dplyr
#' @keywords internal
#' @importFrom rlang .data
NULL


# Internal stringi replacements for two stringr functions with no direct stringi equivalent.
# Signatures mirror the stringr originals (arg names + order), so every call site is a name swap.

# str_wrap(): wrap each element to `width`; stri_wrap returns a list of lines, join with "\n".
tx_str_wrap <- function(string, width = 80, exdent = 0, whitespace_only = TRUE) {
  wrapped <- stringi::stri_wrap(string, width = width, exdent = exdent,
                                whitespace_only = whitespace_only, simplify = FALSE)
  vapply(wrapped, function(lines) stringi::stri_c(lines, collapse = "\n"), character(1))
}

# === SECTION: wrapping a NAME, as opposed to prose =================================================
#
# tx_str_wrap() above breaks on whitespace, which is right for a level LABEL ("Never married") and
# useless for a variable NAME: `shenaniganing_colorous_property_of_the_skin` holds no whitespace, so
# no `wrap_rows`, no column cap and no rotation could ever hold it -- the defect the export review
# reported. A name is a COMPOUND WORD, and its parts are separated by declared characters.
#
# THE BREAK OPPORTUNITIES, stated once and read by every medium:
#   after   a space (consumed at the break), `_` and `.` -- the separator stays at the end of its
#           line, where it reads as "this continues below";
#   before  `*` (the interaction operator: `age` / `*tvhours` says which side the operator belongs
#           to) and a lowercase -> uppercase camelCase seam.
# `-` and `/` are deliberately NOT opportunities: they are far more often a range ("25-34") or a date
# than a compound-name seam, and breaking those reads as a typo.
#' @keywords internal
tx_name_atoms <- function(s) {
  s <- gsub("([_. ])", "\\1\u0001", s, perl = TRUE)
  s <- gsub("(?=[*])|(?<=[a-z0-9])(?=[A-Z])", "\u0001", s, perl = TRUE)
  a <- strsplit(s, "\u0001", fixed = TRUE)[[1L]]
  a[nzchar(a)]
}

# Wrap a NAME to `width`, greedily, at those opportunities. `exdent` indents every line after the
# first, so a reader sees at a glance that it is one name and not two.
# `hard = TRUE` also splits a run with NO opportunity inside it, so the cap is ALWAYS honoured -- what
# a fixed-width column needs, and what prose does not.
#' @keywords internal
tx_wrap_name <- function(string, width = 12L, exdent = 1L, hard = TRUE, brk = "\n") {
  width <- max(1L, as.integer(width))
  ex    <- max(0L, as.integer(exdent))
  avail <- max(1L, width - ex)
  one <- function(s) {
    if (is.na(s) || !nzchar(s) || nchar(s) <= width) return(s)
    lines <- character(0)
    cur   <- ""
    push  <- function(x) lines <<- c(lines, sub(" +$", "", x))
    for (a in tx_name_atoms(s)) {
      r <- if (length(lines)) avail else width          # ⚠ read ONCE: `lines` grows below
      if (nzchar(cur) && nchar(cur) + nchar(a) > r) {
        push(cur); cur <- ""
        r <- avail
        a <- sub("^ +", "", a)                          # a space that became a break is consumed
      }
      while (hard && !nzchar(cur) && nchar(a) > r) {
        push(substr(a, 1L, r))
        a <- substr(a, r + 1L, nchar(a))
        r <- avail
      }
      cur <- paste0(cur, a)
    }
    if (nzchar(cur)) push(cur)
    if (length(lines) < 2L) return(s)
    # DESIGN: the indent is a NO-BREAK space (U+00A0), not an ordinary one. The html path rewrites
    # every remaining space into U+202F so the browser cannot re-break what we already wrapped; an
    # indent written with a plain space would be caught by that blanket rule and shrink to a fifth of
    # its width. U+00A0 renders at full width in every medium and tx_unwrap_text() already undoes it.
    paste0(lines[[1L]], brk,
           paste(paste0(strrep("\u00a0", ex), lines[-1L]), collapse = brk))
  }
  vapply(string, one, character(1), USE.NAMES = FALSE)
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

# A retired export argument is absorbed by `...`, named here, warned about ONCE per call and never
# forwarded; anything else in `...` passes through untouched, so a real typo still errors at the leaf.
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
  # These OMP / data.table thread caps are commented out on purpose (data.table's multithreading
  # once triggered a CRAN thread-count flag traced to data.table itself); re-enable only if needed.
  # # CRAN OMP THREAD LIMIT
  # if (Sys.info()[['sysname']] == "Linux") {
  #  Sys.setenv("OMP_THREAD_LIMIT" = 2)
  # }

  # data.table::setDTthreads(threads = 2)
  # data.table::getDTthreads(verbose = getOption("datatable.verbose"))

  set_color_palette()   # seeds tabxplor.color_style_theme (declared seed = "elsewhere")

  tx_seed_options()

  # Bind the R-tabxplor gettext catalog to the package's compiled .mo (harmless if absent -> English).
  po <- system.file("po", package = pkgname)
  if (nzchar(po)) try(bindtextdomain("R-tabxplor", po), silent = TRUE)

  invisible()
}

# Releases the persistent mirai daemon pool as a CRAN-cleanliness backstop; no pool is ever warmed
# at load (tab_parallel_stop() lets users release it earlier).
#' @keywords internal
.onUnload <- function(libpath) {
  tab_parallel_stop()
}


# Functions and options to work with factors and lists --------------------------------------------

#' A regex pattern to clean the names of factors.
#' @keywords internal
cleannames_condition <- function()
  "^[^- ]+-(?![[:lower:]])|^[^- ]+(?<![[:lower:]])-| *\\(.+\\)"


#' Create a score variable by counting factors at their first level
#'
#' Builds an integer score column counting, for each row, how many of the listed factors sit at
#' their **first level** (1 if so, 0 otherwise) -- the score ranges 0 to `length(vars_list)`. The
#' natural way to sum a battery of yes/no survey items into one score, feeding the grouped-binomial
#' outcome of [tab_reg()] (its `trials` argument).
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
#'   The "first level" is `levels(as.factor(x))[1]`. Non-factor columns are coerced with
#'   [as.factor()]; missing values are folded into an explicit `"NA"` level first (via
#'   [forcats::fct_na_value_to_level()]), so `NA` never counts as the first level.
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

  data |> tibble::add_column(!!rlang::sym(name) := var_final_)
}


# lifecycle::deprecate_soft()'s "silent for same-package callers" rule is fooled by a testthat run
# (it treats the suite as a direct user call), so this asks the real question: whose code called it.
#' @keywords internal
#' @noRd
tx_user_call <- function(env = parent.frame(2)) !identical(topenv(env), asNamespace("tabxplor"))


#' fct_recode helper to recode multiple variables
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' Printed a ready-to-paste `mutate()` call recoding a set of factor columns via
#' [forcats::fct_recode()] -- unrelated to cross-tabulation, and unused elsewhere in tabxplor.
#' Removed in 2.1.0; copy it into your own project if you rely on it.
#'
#' @param data The data frame.
#' @param .cols <\link[tidyr:tidyr_tidy_select]{tidy-select}> The variables to recode.
#' @param name_in The input data frame's name (default: the expression given as `data`).
#' @param name_out The output data frame's name, if different from `name_in`.
#' @param style `"mutate"` (default) writes a `dplyr::mutate()` call; `"base"` writes `data$var <-`.
#' @param reminder Print a `"new" = "old"` syntax reminder. Default `TRUE`.
#' @param freq Print each level's frequency and count as a comment; defaults to `TRUE` when 5 or
#'   fewer variables are given.
#' @param cat Print to console, or open a temporary file when there are more than 5 variables;
#'   `FALSE` returns a data frame of the recode text instead.
#'
#' @return With `cat = TRUE` (default), the text printed to console (or written to a temp R file for
#'   more than 5 variables), returned invisibly. With `cat = FALSE`, a `tibble` of the recode text is
#'   returned instead. A column carrying a `label` attribute is used as its comment title.
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
    }
  }
  if (missing(name_out)) name_out <- name_in

  pos_cols <- tidyselect::eval_select(rlang::enquo(.cols), data)
  data <- data[pos_cols]

  # Variable labels come from the `label` attribute (haven / labelled-imported data), read directly
  # with base attr() rather than depending on the labelled package.
  var_labs <- purrr::map(data, \(col) attr(col, "label", exact = TRUE))
  var_labs <- var_labs[purrr::map_lgl(var_labs, ~ !is.null(.))]
  with_variable_label_as_title <- length(var_labs) > 0


  data <- data |> dplyr::mutate(dplyr::across(.cols = dplyr::everything(), .fns = as.factor))

  if (is.null(freq)) {
    freq <- ncol(data) <= 5
  }

  if (freq) { # With frequencies and counts helpers
    frequencies <- names(data) |>
      purrr::map(
        # `filter` is NOT imported: the bare call resolves to stats::filter(), which evaluates
        # outside the data mask and crashes. dplyr::filter() must stay fully qualified here.
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

  } else {
    recode <- data |>
      purrr::map(~ paste0("\"",
                          stringi::stri_replace_all_regex(
                            levels(.), "\"", "'"
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


# Vendored from tidyselect:::where (MIT licence: https://tidyselect.r-lib.org/LICENSE.html).
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


# ggpubr functions (vendored, for tab_plot() as a tableGrob) ---------------------------------------

#' @keywords internal
is_tablegrob <- function (tab) {
  inherits(tab, "gtable") & inherits(tab, "grob")
}

#' @keywords internal
is_ggtexttable <- function (tab) {
  !is.null(attr(tab, "ggtexttableGrob"))
}

#' @keywords internal
as_ggtexttable <- function (tabgrob) {
  res <- ggpubr::as_ggplot(tabgrob)
  attr(res, "ggtexttableGrob") <- tabgrob
  res
}

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


# Escaped characters ------------------------------------------------------------------------------
#' @keywords internal
unbrk      <- stringi::stri_unescape_unicode("\\u202f") # unbreakable space
sigma_sign <- stringi::stri_unescape_unicode("\\u03c3") # sigma for sd
mult_sign  <- stringi::stri_unescape_unicode("\\u00d7") # multiply sign (ratio >= 1)
div_sign   <- stringi::stri_unescape_unicode("\\u00f7") # divide sign (ratio < 1, shows 1/ratio)
# U+2007 FIGURE SPACE is exactly digit-width in tabular fonts, where an ASCII space is not (and CSS
# collapses space runs) -- used for proportional-font exports (html/Excel) only; console and
# markdown keep the ASCII space.
fig_space  <- stringi::stri_unescape_unicode("\\u2007")


# Only a STARRED table needs the monospace stack below: a proportional "*" is narrower than a digit
# and slides a starred cell out of column alignment. `ui-monospace` is deliberately excluded -- it
# resolves to the OS's own mono and would override the pinned target.
tx_num_font_html_stars <-
  '"Cascadia Mono", "Cascadia Code", Menlo, Consolas, "DejaVu Sans Mono", monospace'


# --- weighted moments -----------------------------------------------------------------------------
# The ML weighted variance (/ sum w), which is what tab()'s numeric leaf computes too, so a band cut
# by shape_cut_bands() and a mean printed in a cell rest on the same definition. Unweighted, the SD
# is the ordinary sample one.
#' @keywords internal
wtd_mean <- function(x, w = NULL) {
  x <- as.numeric(x)
  ok <- is.finite(x)
  if (!is.null(w)) { w <- as.numeric(w); ok <- ok & is.finite(w) & w > 0 }
  if (!any(ok)) return(NA_real_)
  if (is.null(w)) mean(x[ok]) else sum(w[ok] * x[ok]) / sum(w[ok])
}

#' @keywords internal
wtd_sd <- function(x, w = NULL) {
  x <- as.numeric(x)
  ok <- is.finite(x)
  if (!is.null(w)) { w <- as.numeric(w); ok <- ok & is.finite(w) & w > 0 }
  if (sum(ok) < 2L) return(NA_real_)
  if (is.null(w)) return(stats::sd(x[ok]))
  xw <- x[ok]; ww <- w[ok]
  m  <- sum(ww * xw) / sum(ww)
  sqrt(sum(ww * (xw - m)^2) / sum(ww))          # the ML weighted variance, as tab()'s numeric side uses
}
