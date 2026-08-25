# The jamovi module's own translation catalogue (`jamovi/i18n/*.po` -> `inst/i18n/*.json`), which is
# a DIFFERENT domain from the package's R messages (`po/R-*.po`, dev/update_translations.R).
#
# Phase 21g-xii established that jamovi DOES expose gettext to a module's own UI JavaScript -- the
# compiler extracts `_()` / `n_()` / `_p()` from `jamovi/js/**/*.js` (jamovi-compiler/i18n.js) and the
# analysis UI defines `window._` before it evaluates the module. Before that, ~90 user-visible strings
# in the two `.js` files shipped English in every locale. These gates keep that from coming back, and
# keep a translation from silently overflowing the options pane.

po_path <- function() testthat::test_path("..", "..", "jamovi", "i18n", "fr.po")
js_path <- function(an) testthat::test_path("..", "..", "jamovi", "js", paste0(an, ".js"))

# Read a .po into a msgid -> msgstr character vector. Handles the multi-line continuation form.
po_read <- function(f) {
  blk <- strsplit(paste(readLines(f, warn = FALSE, encoding = "UTF-8"), collapse = "\n"), "\n\n")[[1]]
  grab <- function(b, kw) {
    ln <- grep(paste0("^", kw, " "), strsplit(b, "\n")[[1]])
    if (!length(ln)) return(NA_character_)
    parts <- strsplit(b, "\n")[[1]][ln[[1]]:length(strsplit(b, "\n")[[1]])]
    parts[[1]] <- sub(paste0("^", kw, " "), "", parts[[1]])
    keep <- character(0)
    for (p in parts) { if (!grepl('^\\s*"', p)) break; keep <- c(keep, p) }
    paste0(gsub('^\\s*"|"\\s*$', "", keep), collapse = "")
  }
  ids <- vapply(blk, grab, character(1), "msgid")
  st  <- vapply(blk, grab, character(1), "msgstr")
  ok  <- !is.na(ids) & nzchar(ids)
  stats::setNames(st[ok], ids[ok])
}

# What a user actually SEES: the markup is not painted, and `\"` is one character.
vis_nchar <- function(x) nchar(gsub("<[^>]*>", "", gsub('\\\\"', '"', x)), type = "chars")


test_that("the jamovi catalogue is complete, unfuzzy and compiler-safe", {
  f <- po_path(); skip_if_not(file.exists(f), "jamovi/ is not shipped in a built package")
  src <- readLines(f, warn = FALSE, encoding = "UTF-8")
  tr  <- po_read(f)

  expect_true(length(tr) > 250)
  # A blank msgstr ships the English; the whole point of the catalogue is that none do.
  expect_identical(names(tr)[!nzchar(tr)], character(0))
  # ⚠ jamovi's compiler SKIPS a fuzzy entry when it writes inst/i18n/fr.json -- a fuzzy flag is a
  # silently untranslated string, not a warning.
  expect_false(any(grepl("^#,.*fuzzy", src)))
  # ⚠ an UNESCAPED `"` inside a msgstr aborts the compiler with "Invalid key name" and the module
  # then builds with NO translations at all. Every quote in the file must be escaped.
  bad <- grep('^msgstr\\s+"(.*[^\\\\])?"[^"]*"', src, value = TRUE)
  expect_identical(bad, character(0))
})


# ⚠ THE WIDTH RULE, and why it is absolute rather than relative to the English. A jamovi options pane
# is ~340px at its narrowest, and the two CustomControl tables are FIXED-px grids whose head and
# select cells are `white-space:nowrap; text-overflow:ellipsis` -- they TRUNCATE, silently, with no
# R-side symptom. So the budget is the cell's own width divided by ~6px per character, taken from the
# `grid-template-columns` in the .js, and both languages are held to it: a budget the English breaks
# is a wrong budget. Everything not listed here WRAPS (radio labels, hints, tooltips) and is free.
JMV_WIDTH_BUDGET <- c(
  # the per-variable table  (jmvtab.js / jmvtabreg.js VAR_TABLE_HOST `cols`)
  # ⚠ the name column is minmax(90px,1fr) -- 15 chars at its floor. The budget is 16 because the
  # English "Column variables" is 16 and ships: a budget its own English breaks is a wrong budget.
  "variable" = 16, "predictor" = 16,
  "Row variables" = 16, "Column variables" = 16, "Table variables" = 16, "Predictors" = 16,
  "levels / shape =" = 27, "ref = <i>(reference)</i>" = 30, "multiplier =" = 14,   # 165 / 180 / 85px
  "ref2 = <i>(reference)</i>" = 30,        # the same 180px column, off the percentage axis
  "merge" = 12,                                            # TABXM.grid's 72px tick column
  # the `shape =` select, in the 165px levels cell
  "linear (numeric)" = 27, "log (numeric)" = 27, "sqrt (numeric)" = 27, "quadratic (num.)" = 27,
  "sd_bands (cut)" = 27, "median (cut)" = 27, "terciles (cut)" = 27, "quartiles (cut)" = 27,
  "quintiles (cut)" = 27, "deciles (cut)" = 27, "values_to_levels" = 27,
  # the Model table's two headed selects (TABX.mtRow: 150px family, 145px link, 105px level/trials)
  "gaussian (linear)" = 25, "binomial (logistic)" = 25, "poisson (counts)" = 25,
  "multinomial" = 25, "ordinal" = 25,
  "auto (family based)" = 24, "difference" = 24, "ratio" = 24, "odds_ratio" = 24,
  "trials" = 10,
  # the reference cell's own selects, in its 180px column
  "Total" = 30, "First group" = 30, "Last group" = 30, "First" = 30,
  "first group (lowest)" = 30, "last group (highest)" = 30, "mean (default)" = 30
)

test_that("no translation overflows the fixed-width cell it is painted in", {
  f <- po_path(); skip_if_not(file.exists(f), "jamovi/ is not shipped in a built package")
  tr <- po_read(f)
  for (en in names(JMV_WIDTH_BUDGET)) {
    b <- JMV_WIDTH_BUDGET[[en]]
    expect_true(en %in% names(tr), info = paste0("budgeted msgid is not in the catalogue: ", en))
    # the budget must be honest: the English it was measured from has to fit it too
    expect_lte(vis_nchar(en), b)
    expect_lte(vis_nchar(tr[[en]]), b)
  }
})


# ⚠ THE COVERAGE RULE. A user-visible string that is NOT inside `_()` never reaches catalog.pot, so it
# ships English in every locale -- and nothing anywhere reports it. This walks the assignments that
# paint text and demands the call. Argument names and VALUES stay English on purpose (the glossary's
# own rule), so they are listed rather than wrapped.
# ⚠ the glyphs are \uXXXX-escaped, per the package's ASCII-source rule -- R evaluates them to the
# very characters the .js holds. `\\u2014` is different: it is the LITERAL backslash sequence the .js
# source spells, and this gate reads that source as text.
JS_TEXT_EXEMPT <- c("2sd", "max", "\u2026", "family =", "link =",
                    "\u00d7", "*", "\u2502", "\u25b2", "\u25bc", "\u25b8", "\u25be",
                    " \\u2014 ")   # the dash that JOINS two already-translated tips

test_that("every user-visible string in the jamovi .js goes through _()", {
  for (an in c("jmvtab", "jmvtabreg")) {
    f <- js_path(an); skip_if_not(file.exists(f), "jamovi/ is not shipped in a built package")
    src <- paste(readLines(f, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
    code <- gsub("//[^\n]*", "", src)                       # comments quote strings freely

    # `X.textContent = "..."` / `.title = "..."` / `.placeholder = "..."`, and the host object's
    # own text slots. ⚠ `head:` is also a CSS key in the TABX/TABXV/TABXM style objects, so a
    # literal that IS a css declaration list is not a label.
    pat <- paste0("(?:textContent|\\.title|placeholder|\\bhead|\\btip|\\blabel|emptyHint|mergeTip",
                  "|closeTip)\\s*[:=]\\s*(\"(?:[^\"\\\\]|\\\\.)*\"|'(?:[^'\\\\]|\\\\.)*')")
    hits <- regmatches(code, gregexpr(pat, code, perl = TRUE))[[1]]
    lit  <- sub("^[^\"']*", "", hits)
    lit  <- substr(lit, 2L, nchar(lit) - 1L)
    lit  <- lit[!grepl("[a-z-]+\\s*:\\s*[^;]+;", lit)]      # a CSS declaration list, not a label
    lit  <- lit[nzchar(trimws(lit)) & !lit %in% JS_TEXT_EXEMPT]   # a bare separator is not a label
    expect_identical(unique(lit), character(0),
                     info = paste0(an, ".js paints an unwrapped literal: ",
                                   paste(unique(lit), collapse = " | ")))
  }
})
