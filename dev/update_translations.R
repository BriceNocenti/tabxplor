# dev/update_translations.R -- the sanctioned tabxplor R-message translation workflow (Phase 18w).
# .Rbuildignore'd. Run from the package root after adding/changing any gettext()/gettextf() string.
#
#   Rscript dev/update_translations.R          # extract -> normalise -> merge -> compile
#
# Domain "R-tabxplor". Sources of truth:
#   po/R-tabxplor.pot   extraction template (regenerated here)
#   po/R-fr.po          French catalogue (translators fill the msgstr; NEVER auto-overwrite by hand)
#   inst/po/fr/LC_MESSAGES/R-tabxplor.mo   compiled catalogue (committed; what the package loads)
#
# WHY the norm_u() step: potools extracts R string tokens verbatim, so a non-ASCII msgid written as a
# \uXXXX escape in R source (required by the ASCII-source rule) lands in the catalogue as a LITERAL
# backslash-u sequence -- which R's runtime gettext (which passes the *evaluated* character) never
# matches. norm_u() rewrites those escapes to the real UTF-8 character so the .mo key == the runtime
# lookup. Idempotent; safe to re-run.
#
# NOTE on dynamically-gettext'd strings: potools cannot see gettext(variable). Most of the package
# avoids the problem by making the string a CLOSURE (MEASURES$word, CI_METHOD_WORDED, REG_ESTIMANDS'
# notes: potools reads the literal inside the closure body statically), which is why the anchor that
# used to sit beside legend_measure_word() in R/fmt_class.R was deleted in Phase 19l. ONE anchor
# survives and must not be removed: reg_check_msgid_anchor() in R/reg-assumptions.R, holding the 17
# REG_CHECKS nouns and instruments -- those are BARE strings gettext()'d dynamically at render, so
# nothing else makes them visible to the extractor. Keep it in sync with REG_CHECKS.

stopifnot(requireNamespace("potools", quietly = TRUE))
dir <- normalizePath(".")

norm_u <- function(f) {
  if (!file.exists(f)) return(invisible())
  x <- readLines(f, encoding = "UTF-8", warn = FALSE)
  m <- gregexpr("\\\\\\\\u[0-9a-fA-F]{4}", x, perl = TRUE)     # "\\uXXXX" (two backslash bytes)
  regmatches(x, m) <- lapply(regmatches(x, m), function(h)
    vapply(h, function(one) intToUtf8(strtoi(sub("^\\\\\\\\u", "", one), 16L)), character(1)))
  writeLines(enc2utf8(x), f, useBytes = TRUE)
}

message("1/5  extract  -> po/R-tabxplor.pot")
potools::po_extract(dir)

message("2/5  normalise \\uXXXX escapes -> real UTF-8 (msgids must match runtime gettext)")
norm_u(file.path(dir, "po", "R-tabxplor.pot"))

message("3/5  merge     -> po/*.po (preserves existing msgstr, flags new/obsolete)")
potools::po_update(dir)
for (po in list.files(file.path(dir, "po"), pattern = "\\.po$", full.names = TRUE)) norm_u(po)

message("4/5  compile   -> inst/po/*/LC_MESSAGES/R-tabxplor.mo")
potools::po_compile(dir)

# The `en@quot` PSEUDO-locale (English with directional quotes) has no translator catalogue: R
# derives it mechanically from the .pot. potools does not, so it was generated once and then sat
# stale -- 136 msgids against the catalogue's 235 by Phase 19n, i.e. a user running
# LANGUAGE=en@quot got quoted English for the old messages and plain English for every message
# added since. It is DERIVED here on every run instead, and its .po is deliberately NOT kept in
# po/ (po_update() would then treat it as a translation to merge).
message("5/5  derive    -> inst/po/en@quot/LC_MESSAGES/R-tabxplor.mo")
en_quote <- tryCatch(get("en_quote", envir = asNamespace("tools")), error = function(e) NULL)
if (is.null(en_quote)) {
  message("     skipped: tools:::en_quote() is unavailable in this R.")
} else {
  tmp <- tempfile(fileext = ".po")
  en_quote(file.path(dir, "po", "R-tabxplor.pot"), tmp)
  out <- file.path(dir, "inst", "po", "en@quot", "LC_MESSAGES")
  dir.create(out, recursive = TRUE, showWarnings = FALSE)
  system2("msgfmt", c("-c", "-o", shQuote(file.path(out, "R-tabxplor.mo")), shQuote(tmp)))
}

message("done. Review po/R-fr.po (fill any blank msgstr, resolve fuzzies), then re-run to compile.")
