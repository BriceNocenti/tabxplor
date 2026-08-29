# PURPOSE: the ONE guard for "can this environment translate at all?", shared by every test that
#          asserts French output (test-i18n-fr.R, test-color-legend.R).
#
# Whether a mid-session LANGUAGE switch can translate is a property of the ENVIRONMENT, not of
# tabxplor: R must be built with NLS, and GNU gettext ignores LANGUAGE entirely when the message
# locale is "C"/"POSIX". That is exactly the state under R CMD check on Linux (check.R forces
# LANGUAGE=en, and testthat's local_reproducible_output() sets LANG/LANGUAGE to "C" for every
# test_that() block) -- and on the CRAN farm. It is why the French fixtures failed on the 3 Linux
# jobs while passing on macOS/Windows, whose libintl honours LANGUAGE regardless, and on the
# maintainer's fr_FR.UTF-8 box.
#
# So PROBE the capability with a raw gettext() call rather than guessing from LANG: a plain LANG
# check would skip on Windows/macOS too, throwing away the only platforms that can actually
# exercise the translation. The probe deliberately uses the SAME cache-flush production uses, so
# it fails only where translation is genuinely impossible.
skip_if_no_gettext <- function() {
  # 1. the compiled catalog is installed (a fresh checkout before dev/update_translations.R has none)
  po <- system.file("po", "fr", "LC_MESSAGES", "R-tabxplor.mo", package = "tabxplor")
  testthat::skip_if(!nzchar(po) || !file.exists(po), "R-tabxplor fr catalog not compiled")

  # 2. R itself can do native language support
  testthat::skip_if_not(capabilities("NLS"), "R built without NLS")

  # 3. a LANGUAGE switch actually reaches the catalog here
  can_translate <- function() {
    old <- Sys.getenv("LANGUAGE", unset = NA_character_)
    on.exit({
      if (is.na(old)) Sys.unsetenv("LANGUAGE") else Sys.setenv(LANGUAGE = old)
      tabxplor:::flush_gettext_cache()
    }, add = TRUE)
    tabxplor:::flush_gettext_cache()
    Sys.setenv(LANGUAGE = "fr")
    tabxplor:::flush_gettext_cache()
    # ⚠ the probe msgid must be one the package actually still emits: a retired msgid would make
    # every French test SKIP instead of fail. "the reference category" is named by the stars line too.
    sentinel <- "the reference category (in bold)"
    !identical(gettext(sentinel, domain = "R-tabxplor"), sentinel)
  }
  testthat::skip_if_not(can_translate(), "gettext cannot honour LANGUAGE here (locale is C)")
}
