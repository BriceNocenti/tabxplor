# PURPOSE: Lock the Phase 14g console colour-scheme detection.
# ROLE: tx_detect_theme() feeds set_color_palette(theme = "auto"). Detection is best-effort and rests
#       on no supported API, so what these tests pin is the CONTRACT: it never errors, it never guesses
#       (unknown -> "light"), and the name -> uiTheme resolution is exact.
# KEY CONSTRAINTS:
#   - Must NOT depend on the host IDE: every probe is driven from injected fixtures.

# --- fixtures ----------------------------------------------------------------------------
# A History folder shaped like VS Code's: entries.json naming the resource + the snapshots beside it.
mk_history <- function(dir, settings, resource = "vscode-userdata:/c%3A/U/Positron/User/settings.json") {
  d <- file.path(dir, "abc123"); dir.create(d, recursive = TRUE, showWarnings = FALSE)
  writeLines(paste0('{"version":1,"resource":"', resource, '","entries":[{"id":"old.json",',
                    '"timestamp":1},{"id":"new.json","timestamp":2}]}'),
             file.path(d, "entries.json"))
  writeLines('{"workbench.colorTheme": "Some Old Theme"}', file.path(d, "old.json"))
  writeLines(settings, file.path(d, "new.json"))
  Sys.setFileTime(file.path(d, "old.json"), Sys.time() - 600)
  dir
}

mk_ext <- function(dir, label, uiTheme) {
  d <- file.path(dir, "pub.ext-1.0.0"); dir.create(d, recursive = TRUE, showWarnings = FALSE)
  writeLines(paste0('{"name":"ext","contributes":{"themes":[',
                    '{"label":"Other Theme","uiTheme":"vs","path":"./a.json"},',
                    '{"label":"', label, '","uiTheme":"', uiTheme, '","path":"./b.json"}]}}'),
             file.path(d, "package.json"))
  dir
}

# === SECTION: the settings probe =========================================================

testthat::test_that("the newest settings snapshot is found, and only two keys are read", {
  root <- mk_history(withr::local_tempdir(),
                     '{"workbench.colorTheme": "Starless Monokai Atom",
                       "claudeQuota.sessionKey": "sk-SECRET-do-not-read",
                       "editor.fontSize": 13}')
  f <- tabxplor:::tx_positron_settings_file(root)
  testthat::expect_true(!is.null(f))
  testthat::expect_match(f, "new[.]json$")            # newest, not the first found

  s <- tabxplor:::tx_positron_settings(f)
  testthat::expect_named(s, c("theme", "auto_detect"))
  testthat::expect_equal(s$theme, "Starless Monokai Atom")
  testthat::expect_false(s$auto_detect)
  # PRIVACY: that file also holds credentials. Nothing but the two keys may come back.
  testthat::expect_false(any(grepl("SECRET", unlist(s), fixed = TRUE)))
})

testthat::test_that("a History folder for another resource is ignored", {
  root <- mk_history(withr::local_tempdir(), '{"workbench.colorTheme": "X"}',
                     resource = "file:///home/u/notes.md")
  testthat::expect_null(tabxplor:::tx_positron_settings_file(root))
})

testthat::test_that("settings.json is JSONC-tolerant (comments, trailing commas)", {
  root <- mk_history(withr::local_tempdir(),
                     '{\n // my theme\n "workbench.colorTheme": "Quiet Light",\n}')
  testthat::expect_equal(tabxplor:::tx_positron_settings(
    tabxplor:::tx_positron_settings_file(root))$theme, "Quiet Light")
})

testthat::test_that("every probe degrades to NULL -- silently: no error AND no warning", {
  # "silently" is the contract: a warning from a colour probe is noise the user can neither act on
  # nor switch off. readLines() warns before it errors, so tryCatch(error=) alone is not enough.
  testthat::expect_silent(testthat::expect_null(tabxplor:::tx_positron_settings_file("/no/such/dir")))
  testthat::expect_silent(testthat::expect_null(tabxplor:::tx_positron_settings(NULL)))
  testthat::expect_silent(testthat::expect_null(tabxplor:::tx_positron_settings("/no/such/f.json")))
  testthat::expect_silent(testthat::expect_null(tabxplor:::tx_theme_kind(NULL)))
  testthat::expect_silent(testthat::expect_null(tabxplor:::tx_theme_kind("", ext_dir = "/no/such")))
  testthat::expect_silent(tabxplor:::tx_detect_theme())
})

# === SECTION: name -> uiTheme ============================================================

testthat::test_that("a theme resolves by EXACT name through its extension's uiTheme", {
  ext <- mk_ext(withr::local_tempdir(), "Starless Monokai Atom", "vs-dark")
  testthat::expect_equal(tabxplor:::tx_theme_kind("Starless Monokai Atom", ext), "dark")
  testthat::expect_equal(tabxplor:::tx_theme_kind("Other Theme", ext), "light")
  testthat::expect_null(tabxplor:::tx_theme_kind("Not Installed", ext))
})

testthat::test_that("the theme NAME is never a signal -- only its uiTheme is", {
  # THE trap: "Starless Monokai Atom" contains neither "dark" nor "light" and is vs-dark; a name
  # regex would have to guess. And a name CAN lie:
  ext <- mk_ext(withr::local_tempdir(), "Bright Sunny Day", "vs-dark")
  testthat::expect_equal(tabxplor:::tx_theme_kind("Bright Sunny Day", ext), "dark")
  ext2 <- mk_ext(withr::local_tempdir(), "Midnight Black", "vs")
  testthat::expect_equal(tabxplor:::tx_theme_kind("Midnight Black", ext2), "light")
})

testthat::test_that("builtin themes resolve without an extension (they ship no package.json)", {
  testthat::expect_equal(tabxplor:::tx_theme_kind("Default Dark+", "/no/such/dir"), "dark")
  testthat::expect_equal(tabxplor:::tx_theme_kind("Quiet Light",  "/no/such/dir"), "light")
  testthat::expect_equal(tabxplor:::tx_theme_kind("Positron Dark", "/no/such/dir"), "dark")
  testthat::expect_equal(tabxplor:::tx_theme_kind("Dark High Contrast", "/no/such/dir"), "dark")
})

# === SECTION: the bail-outs ==============================================================

testthat::test_that("autoDetectColorScheme = true bails: colorTheme is then STALE", {
  # with it on, the live theme comes from window.preferredDark/LightColorTheme following the OS, so
  # workbench.colorTheme is not what is showing -- guessing from it would be actively wrong.
  s <- list(theme = "Default Dark+", auto_detect = TRUE)
  testthat::expect_null(tabxplor:::tx_positron_theme(s))
  s$auto_detect <- FALSE
  testthat::expect_equal(tabxplor:::tx_positron_theme(s), "dark")
  testthat::expect_null(tabxplor:::tx_positron_theme(NULL))
})

testthat::test_that("tx_detect_theme() always returns light/dark and never errors", {
  th <- tabxplor:::tx_detect_theme()
  testthat::expect_true(th %in% c("light", "dark"))
  # with every signal removed it must say "light", not guess
  withr::with_envvar(
    list(RSTUDIO = "", POSITRON = "", VSCODE_PID = "", VSCODE_CWD = "", TERM_PROGRAM = "",
         COLORFGBG = ""),
    testthat::expect_equal(tabxplor:::tx_detect_theme(), "light")
  )
})

testthat::test_that("COLORFGBG is read as the terminal fallback", {
  no_ide <- list(RSTUDIO = "", POSITRON = "", VSCODE_PID = "", VSCODE_CWD = "", TERM_PROGRAM = "")
  withr::with_envvar(c(no_ide, list(COLORFGBG = "15;0")),
                     testthat::expect_equal(tabxplor:::tx_detect_theme(), "dark"))
  withr::with_envvar(c(no_ide, list(COLORFGBG = "0;15")),
                     testthat::expect_equal(tabxplor:::tx_detect_theme(), "light"))
  withr::with_envvar(c(no_ide, list(COLORFGBG = "15;;0")),   # the 3-field variant
                     testthat::expect_equal(tabxplor:::tx_detect_theme(), "dark"))
  withr::with_envvar(c(no_ide, list(COLORFGBG = "nonsense")),
                     testthat::expect_equal(tabxplor:::tx_detect_theme(), "light"))
})

testthat::test_that("tx_ide() names the host", {
  withr::with_envvar(list(RSTUDIO = "1"), testthat::expect_equal(tabxplor:::tx_ide(), "rstudio"))
  withr::with_envvar(list(RSTUDIO = "", POSITRON = "1"),
                     testthat::expect_equal(tabxplor:::tx_ide(), "positron"))
  withr::with_envvar(list(RSTUDIO = "", POSITRON = "", VSCODE_PID = "42", TERM_PROGRAM = ""),
                     testthat::expect_equal(tabxplor:::tx_ide(), "vscode"))
  withr::with_envvar(list(RSTUDIO = "", POSITRON = "", VSCODE_PID = "", VSCODE_CWD = "",
                          TERM_PROGRAM = ""),
                     testthat::expect_equal(tabxplor:::tx_ide(), "terminal"))
})

testthat::test_that("rstudioapi is never called outside RStudio (its isAvailable() lies in ark)", {
  withr::with_envvar(list(RSTUDIO = ""), testthat::expect_null(tabxplor:::tx_rstudio_dark()))
})

# === SECTION: the set_color_palette() seam ===============================================

testthat::test_that("set_color_palette(theme = 'auto') resolves to a real palette theme", {
  old <- getOption("tabxplor.color_style_theme")
  withr::defer(options("tabxplor.color_style_theme" = old))
  set_color_palette(theme = "auto")
  testthat::expect_true(getOption("tabxplor.color_style_theme") %in% c("light", "dark"))
  # "auto" must never be STORED: a palette is always one or the other (get_color_style would build
  # the key "text_auto", find nothing, and error on a length-0 vector).
  testthat::expect_false(identical(getOption("tabxplor.color_style_theme"), "auto"))
  testthat::expect_length(get_color_style("color_code"), 8L)
  set_color_palette(theme = "dark")
  testthat::expect_equal(getOption("tabxplor.color_style_theme"), "dark")
  testthat::expect_error(set_color_palette(theme = "nonsense"))
})
