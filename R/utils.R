# PURPOSE: package initialization plus the shared factor / list / string utilities.
# ROLE: .onLoad() SEEDS the package options and the colour palette; everything else here is a small
#   helper with no home of its own -- the base-R wrapping / padding / truncating primitives, the
#   NAME wrapper beside them, the retired-export-argument catcher, the two message helpers
#   (tx_inform_once() / tx_need_pkg()), and the three exported user helpers (score_from_lv1(),
#   gss_cat_data_formatting(), and the deprecated fct_recode_helper()).
# KEY CONSTRAINTS:
#   - TAB_OPTIONS (R/tab-options.R) is the single source of truth for option names and defaults;
#     .onLoad() only seeds them, through tx_seed_options().
#   - set_color_style() and set_color_breaks() are defined in tab_classes.R but called from here.
#   - A LEVEL LABEL IS PROSE, A VARIABLE NAME IS A COMPOUND WORD: tx_str_wrap() breaks the first on
#     whitespace, tx_wrap_name() breaks the second at the seams a name is actually built from
#     (`_`, `.`, `*`, camelCase). One of them was missing, so a snake_case name met no break
#     opportunity and no width could ever hold it.
#   - A VARIABLE LIST IS A LIST OF SYMBOLS: vars_chr(), never as.character(), which deparses a
#     non-syntactic name back into backticks.
#   - This file sorts second-to-last in C collation (only zzz-fact-keys.R follows), so nothing in
#     the package may depend on it at SOURCE time.
# See: CLAUDE.md § tabxplor architecture.

# Rlang .data to bind data masking variable in dplyr
#' @keywords internal
#' @importFrom rlang .data
NULL


# THE variable-list -> character conversion, and the reason it needs a name of its own.
# `row_vars` / `col_vars` / `tab_vars` travel as LISTS of symbols, and `as.character()` on a list
# DEPARSES each element -- so a non-syntactic name comes back wrapped in backticks
# (`as.character(rlang::syms("my age"))` is "`my age`") and every later tidyselect misses it. A bare
# symbol is fine, which is why this went unseen: `tab(d, marital, `my age`)` aborted on a column
# that plainly exists. rlang::as_name() reads the symbol instead of printing it.
# ⚠ the same trap the shape subsystem already documents (shape_colname(), R/var-shape.R).
#' @keywords internal
#' @noRd
vars_chr <- function(x) {
  if (is.null(x)) return(character(0))
  if (is.character(x)) return(unname(x))
  if (rlang::is_symbol(x)) return(rlang::as_name(x))
  vapply(x, function(v) if (rlang::is_symbol(v)) rlang::as_name(v) else as.character(v),
         character(1), USE.NAMES = FALSE)
}


# tx_pad(): pad each element to `width`, on DISPLAY width -- not on character count, because the
# tables are aligned by eye and a wide glyph occupies two columns. `pad` is often a figure space or
# a non-breaking space rather than an ASCII one, so formatC() cannot do this.
tx_pad <- function(str, width, side = c("left", "right", "both"), pad = " ") {
  side <- match.arg(side)
  n   <- pmax(0L, width - nchar(str, type = "width"))
  out <- switch(side,
    left  = paste0(strrep(pad, n), str),
    right = paste0(str, strrep(pad, n)),
    both  = paste0(strrep(pad, n %/% 2L), str, strrep(pad, n - n %/% 2L)))
  out[is.na(str)] <- NA_character_
  out
}

# tx_str_wrap(): wrap each element to `width`, lines joined by "\n". A LABEL, not prose.
#
# WARNING: NOT base::strwrap(), and NOT a greedy fill. strwrap formats a PARAGRAPH -- it normalises
# whitespace runs, double-spaces after a full stop and re-flows across elements. And a greedy fill
# (take words until the line is full) gives a visibly worse table: it leaves one long line beside a
# nearly empty one.
#
# THE ALGORITHM IS MINIMUM RAGGEDNESS, by dynamic programming: over all ways of breaking the words
# into lines, take the one minimising the sum of (cap - line width)^2 over every line BUT THE LAST.
# Squaring is what makes two medium lines beat one full and one nearly empty. `exdent` shortens the
# cap of every line after the first, and is written back in as leading spaces.
# `whitespace_only` is kept for the signature's sake: a label breaks at spaces, never inside a word
# (a compound NAME with no spaces is tx_wrap_name()'s job, in the next section).
# ⚠ NA becomes the literal "NA": the wrapped value goes on to be a factor level or a cell label,
# and a missing one still has to print.
tx_str_wrap <- function(string, width = 80, exdent = 0, whitespace_only = TRUE) {
  ind    <- strrep(" ", exdent)
  string <- as.character(string)
  string[is.na(string)] <- "NA"
  vapply(string, function(x) {
    if (!nzchar(x)) return(x)
    w <- strsplit(x, "[[:space:]]+", perl = TRUE)[[1]]
    w <- w[nzchar(w)]
    if (length(w) <= 1L) return(paste0(w, collapse = ""))
    n    <- length(w)
    L    <- nchar(w, type = "width")
    ends <- cumsum(L) + seq_len(n) - 1L               # width of words 1..j on one line
    span <- function(i, j) ends[j] - (if (i > 1L) ends[i - 1L] + 1L else 0L)
    cap1 <- width
    cap2 <- max(1L, width - exdent)
    # best[i] = least cost of laying out words i..n, line i being a NON-first line.
    best <- c(rep(Inf, n), 0)
    brk  <- integer(n)
    for (i in n:1) for (j in i:n) {
      len <- span(i, j)
      if (len > cap2 && j > i) break                  # a lone over-long word still gets its line
      cost <- (if (j == n) 0 else (cap2 - len)^2) + best[j + 1L]
      if (cost < best[i]) { best[i] <- cost; brk[i] <- j }
    }
    first <- 1L; fbest <- Inf
    for (j in 1:n) {
      len <- span(1L, j)
      if (len > cap1 && j > 1L) break
      cost <- (if (j == n) 0 else (cap1 - len)^2) + best[j + 1L]
      if (cost < fbest) { fbest <- cost; first <- j }
    }
    out <- character(0); i <- 1L; j <- first
    repeat {
      out <- c(out, paste(w[i:j], collapse = " "))
      if (j >= n) break
      i <- j + 1L; j <- brk[i]
    }
    if (length(out) > 1L) out[-1L] <- paste0(ind, out[-1L])
    paste0(out, collapse = "\n")
  }, character(1), USE.NAMES = FALSE)
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
  too_long <- !is.na(string) & nchar(string, type = "chars") > width
  string[too_long] <- paste0(substr(string[too_long], 1L, width - nchar(ellipsis, type = "chars")), ellipsis)
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


# === SECTION: user messages =======================================================================
# A message is addressed to the person writing the call: what is wrong, or what was decided for them,
# and the argument that changes it. See CLAUDE.md § Cross-cutting invariants.

# An automatic recode changes what the table IS, so it is never silent -- but it must not repeat on
# every call of a loop either.
# ⚠ THE ID CARRIES THE SUBJECT, not just the kind of message: `paste0("shape_auto_", var)`, never
# "shape_auto". A fixed id would silence the note for the NEXT variable of the same session.
#' @keywords internal
#' @noRd
tx_inform_once <- function(id, ..., .envir = parent.frame()) {
  id <- paste0("tabxplor_", id)
  tx_said[[id]] <- TRUE
  cli::cli_inform(c(...), .envir = .envir, .frequency = "once", .frequency_id = id)
  invisible(NULL)
}

# The ids said so far, so a session can be put back to its first-call state. Used by the tests that
# assert on a once-per-session message, which would otherwise see it only in whichever ran first.
tx_said <- new.env(parent = emptyenv())

#' @keywords internal
#' @noRd
tx_reset_messages <- function() {
  for (id in ls(tx_said)) rlang::reset_message_verbosity(id)
  rm(list = ls(tx_said), envir = tx_said)
  invisible(NULL)
}

# THE Suggests gate: one message for every missing package of one request, never one per package.
# `what` is plain prose ("Excel export", "the model-check plots"): it is substituted as a VALUE, so cli markup
# written into it would reach the user raw.
# Deliberately the only message allowed three bullets -- it is rare, it is shown once, and it is
# aimed at a reader for whom installing a package is the hard part.
#' @keywords internal
#' @noRd
tx_need_pkg <- function(pkgs, what, severity = c("abort", "inform"), call = NULL) {
  miss <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
  if (length(miss) == 0L) return(invisible(TRUE))
  code <- if (length(miss) == 1L) paste0('install.packages("', miss, '")')
          else paste0('install.packages(c(', paste0('"', miss, '"', collapse = ", "), '))')
  msg <- c(
    "{what} needs the {.pkg {miss}} package{?s}.",
    "i" = "{.code {code}}",
    "i" = 'Everything tabxplor can use: {.code install.packages("tabxplor", dependencies = TRUE)}')
  if (identical(match.arg(severity), "inform")) {
    tx_inform_once(paste0("need_pkg_", paste(miss, collapse = "_")), msg,
                   .envir = environment())
    return(invisible(FALSE))
  }
  cli::cli_abort(msg, call = call)
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
      tidyselect::all_of(vars_list), ~ forcats::fct_na_value_to_level(., TAB_NA_LEVEL))),

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
    if (grepl("\\(", name_in, perl = TRUE)) {
      name_in <- regmatches(name_in, regexpr("[^\\(]+$", name_in, perl = TRUE))
      name_in <- sub("\\).*$", "", name_in, perl = TRUE)
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
                              gsub("\"", "'", lvs, perl = TRUE),
                              "\""),
                 pct = format(.data$pct),
                 n   = format(n),
                 txt = paste0(tx_pad(pct, max(nchar(pct, type = "chars"))),
                              " ",
                              tx_pad(n, max(nchar(n, type = "chars")))
                 )
          ) |>
          dplyr::select(lvs, txt)
      ) |>
      purrr::set_names(names(data))

    recode <- frequencies |>
      purrr::map(
        ~ paste0(tx_pad(.x$lvs, max(nchar(.x$lvs, type = "chars")), "right"), " = ",
                 tx_pad(.x$lvs, max(nchar(.x$lvs, type = "chars")), "right"),
                 ", # ",
                 .x$txt
        )
      ) |>
      purrr::map(~ paste0(., collapse = "\n"))

  } else {
    recode <- data |>
      purrr::map(~ paste0("\"",
                          gsub("\"", "'", levels(.), perl = TRUE),
                          "\"")) |>
      purrr::map(
        ~ paste0(tx_pad(., max(nchar(., type = "chars")), "right"), " = ",
                 tx_pad(., max(nchar(., type = "chars")), "right"), collapse = ",\n")
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



# === SECTION: knitr chunk options ================================================================
# knitr is Suggests: the three chunk options tabxplor reads are only ever set DURING a render, and a
# render is exactly when knitr is loaded. `knitr.in.progress` is knitr's own flag for that, so the
# gate answers "am I being knitted?" and the requireNamespace() below can never be the slow path.
#' @keywords internal
tx_knitr_opt <- function(name, which = c("current", "knit")) {
  if (!isTRUE(getOption("knitr.in.progress"))) return(NULL)
  if (!requireNamespace("knitr", quietly = TRUE)) return(NULL)
  switch(match.arg(which),
         current = knitr::opts_current$get(name),
         knit    = knitr::opts_knit$get(name))
}


# === SECTION: HTML escaping ======================================================================
# Vendored from htmltools::htmlEscape (htmltools 0.5.9, RStudio/Posit, GPL (>= 2), redistributed
# here under tabxplor's GPL (>= 3) as that licence's "or later" clause permits). Thank you.
#
# htmltools was a one-function Import: this, plus base64enc / digest / fastmap and a compile, for a
# vector of gsub()s. The early return is what makes it cheap on a table of numbers, where nothing
# ever matches -- and `useBytes = TRUE` on both the test and the substitutions is what keeps it so
# in a non-UTF-8 locale.
#
# WARNING: `attribute = TRUE` is not decoration. Inside an attribute value a bare quote or a raw
# newline ENDS the attribute, so the extra four are a correctness requirement, not a nicety.
tx_html_specials <- list("&" = "&amp;", "<" = "&lt;", ">" = "&gt;")
tx_html_specials_attrib <- c(
  tx_html_specials,
  list("'" = "&#39;", "\"" = "&quot;", "\r" = "&#13;", "\n" = "&#10;")
)

#' @keywords internal
tx_html_escape <- function(text, attribute = FALSE) {
  specials <- if (attribute) tx_html_specials_attrib else tx_html_specials
  pattern  <- if (attribute) "[&<>'\"\r\n]" else "[&<>]"
  text <- enc2utf8(as.character(text))
  if (!any(grepl(pattern, text, useBytes = TRUE))) return(text)
  for (chr in names(specials))
    text <- gsub(chr, specials[[chr]], text, fixed = TRUE, useBytes = TRUE)
  Encoding(text) <- "UTF-8"
  text
}


# Escaped characters ------------------------------------------------------------------------------
#' @keywords internal
unbrk      <- "\u202f" # unbreakable space
sigma_sign <- "\u03c3" # sigma for sd
mult_sign  <- "\u00d7" # multiply sign (ratio >= 1)
div_sign   <- "\u00f7" # divide sign (ratio < 1, shows 1/ratio)
# U+2007 FIGURE SPACE is exactly digit-width in tabular fonts, where an ASCII space is not (and CSS
# collapses space runs) -- used for proportional-font exports (html/Excel) only; console and
# markdown keep the ASCII space.
fig_space  <- "\u2007"


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
