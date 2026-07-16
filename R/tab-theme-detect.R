# PURPOSE: Best-effort detection of the CONSOLE's colour scheme, light or dark (Phase 14g).
# ROLE: Feeds set_color_palette(theme = "auto") / .onLoad, so tab()'s console colours suit the
#   editor's background without the user setting an option. EXPORT is not concerned: there,
#   theme = "auto" delegates to the browser (Phase 13d), the only layer that can truly know.
# KEY CONSTRAINTS:
#   - NEVER errors, never warns, never asks: every probe is wrapped, and anything unknown is "light".
#     A wrong guess makes a table unreadable, so silence + the safe default beats cleverness.
#   - PRIVACY: the Positron probe reads a settings file that also holds secrets. It extracts TWO keys
#     by regex and never parses, stores, logs or errors with anything else. Do not widen it.
#   - Detection is best-effort and unsupported upstream (see below); it must stay easy to delete.
# See: CLAUDE.md Phase 14g.

# === SECTION: which IDE are we in ==================================================================

# `.Platform$GUI == "Positron"` holds in the Positron CONSOLE (ark rebinds .Platform in baseenv()).
# MEASURED 2026-07-17 in this distro's Positron integrated TERMINAL: GUI = "X11", but POSITRON = "1"
# and TERM_PROGRAM = "vscode" ARE set -- so detection works there too, which matters because that is
# where R is actually run here. (An earlier note in the roadmap recorded POSITRON as empty in the
# terminal and concluded terminal-side detection was impossible; that is no longer true.) Treating the
# integrated terminal as Positron is right on the merits as well: its background IS the editor theme's.
#' @keywords internal
tx_ide <- function() {
  if (identical(Sys.getenv("RSTUDIO"), "1"))                        return("rstudio")
  if (identical(.Platform$GUI, "Positron") ||
      identical(Sys.getenv("POSITRON"), "1"))                       return("positron")
  if (nzchar(Sys.getenv("VSCODE_PID")) || nzchar(Sys.getenv("VSCODE_CWD")) ||
      identical(Sys.getenv("TERM_PROGRAM"), "vscode"))              return("vscode")
  "terminal"
}

# === SECTION: RStudio =============================================================================

# TRAPS, each source-verified -- this is not the usual idiom, deliberately:
#   - `rstudioapi::isAvailable()` is a LIE in Positron: ark fakes it TRUE, so verifyAvailable() passes
#     and findFun() then stop()s. Gate on hasFun() + the RSTUDIO env var instead.
#   - `$dark` can be NA even in real RStudio (tidyverse#88, rstudio#4850) -> isTRUE(), never `if (x)`.
#   - `readRStudioPreference()` is NOT usable: ark's shim is `function(name, default) default`, so it
#     ships, hasFun() sees it, and it always returns your default.
tx_rstudio_dark <- function() {
  if (!identical(Sys.getenv("RSTUDIO"), "1")) return(NULL)
  if (!requireNamespace("rstudioapi", quietly = TRUE)) return(NULL)
  if (!isTRUE(tryCatch(rstudioapi::hasFun("getThemeInfo"), error = function(e) FALSE))) return(NULL)
  dark <- tryCatch(rstudioapi::getThemeInfo()$dark, error = function(e) NULL)
  if (is.null(dark) || is.na(dark)) return(NULL)
  isTRUE(dark)
}

# === SECTION: Positron ============================================================================

# There is NO supported API: positron#2986 ("Support rstudioapi::getThemeInfo()") has been open since
# 2024-05, milestone "Future", and no R package detects it (thematic assumes light and warns, cli
# returns FALSE, the rest do not try). What IS reachable: Positron is a VS Code server, and VS Code
# caches the CLIENT's settings.json server-side under ~/.positron-server/data/User/History/<hash>/,
# updating it on live writes -- which is the same path the theme picker itself uses. So the chain
#   workbench.colorTheme -> the declaring extension's package.json -> uiTheme: vs-dark
# resolves the ACTUAL theme. Fragile by construction (a client-only theme extension has no
# server-side package.json; the Positron console is separately themable) -- hence NULL, not a guess,
# whenever any link is missing.

# The newest cached snapshot of the client's User/settings.json, or NULL. VS Code keys each History
# folder by an `entries.json` naming the original `resource`; the snapshots sit beside it.
# NOTE: no JSON parser -- one regex for `resource`, then newest-by-mtime. That keeps tabxplor free of a
# jsonlite dependency for a best-effort probe, and (with tx_positron_settings' regex) means no file
# here is ever fully parsed into R.
tx_positron_settings_file <- function(
    root = file.path(path.expand("~"), ".positron-server", "data", "User", "History")) {
  tryCatch({
    if (!dir.exists(root)) return(NULL)
    for (d in list.dirs(root, recursive = FALSE)) {
      ent <- file.path(d, "entries.json")
      if (!file.exists(ent)) next
      res <- regmatches(txt <- paste(readLines(ent, warn = FALSE), collapse = ""),
                        regexpr('"resource"\\s*:\\s*"[^"]*"', txt))
      if (length(res) == 0 || !grepl("/User/settings[.]json\"$", res)) next
      snaps <- setdiff(list.files(d, pattern = "[.]json$", full.names = TRUE), ent)
      if (length(snaps) == 0) next
      return(snaps[which.max(file.mtime(snaps))])
    }
    NULL
  }, error = function(e) NULL)
}

# PRIVACY: read the file, pull the two keys by regex, drop everything else. settings.json is JSONC
# (comments allowed), so it is not parsed as JSON -- which also keeps every other key, including any
# credential, out of R entirely. Never widen this to a full parse, and never put `txt` in a message.
tx_positron_settings <- function(file = tx_positron_settings_file()) {
  tryCatch({
    # file.exists() first: readLines() WARNS before it errors, and tryCatch(error=) would let that
    # through -- this must be silent, not merely non-fatal.
    if (is.null(file) || !file.exists(file)) return(NULL)
    txt  <- paste(readLines(file, warn = FALSE), collapse = "\n")
    get1 <- function(key, pat) {
      m <- regmatches(txt, regexpr(paste0('"', key, '"\\s*:\\s*', pat), txt))
      if (length(m) == 0) NULL else sub(paste0('^"', key, '"\\s*:\\s*'), "", m)
    }
    theme <- get1("workbench.colorTheme", '"[^"]*"')
    auto  <- get1("window.autoDetectColorScheme", "(true|false)")
    list(theme = if (is.null(theme)) NULL else gsub('^"|"$', "", theme),
         auto_detect = identical(auto, "true"))
  }, error = function(e) NULL)
}

# The VS Code / Positron builtins, which have no server-side package.json to read a uiTheme from.
# Anything else must resolve through an extension: a theme NAME is not a signal (this maintainer's
# "Starless Monokai Atom" contains neither "dark" nor "light" and is vs-dark), so never regex it.
tx_builtin_themes <- c(
  "Default Dark+" = "dark", "Default Dark Modern" = "dark", "Dark+ (default dark)" = "dark",
  "Visual Studio Dark" = "dark", "Dark (Visual Studio)" = "dark", "Abyss" = "dark",
  "Kimbie Dark" = "dark", "Monokai" = "dark", "Monokai Dimmed" = "dark", "Red" = "dark",
  "Solarized Dark" = "dark", "Tomorrow Night Blue" = "dark", "Dark High Contrast" = "dark",
  "Default High Contrast" = "dark", "Positron Dark" = "dark",
  "Default Light+" = "light", "Default Light Modern" = "light", "Light+ (default light)" = "light",
  "Visual Studio Light" = "light", "Light (Visual Studio)" = "light", "Quiet Light" = "light",
  "Solarized Light" = "light", "Light High Contrast" = "light",
  "Default High Contrast Light" = "light", "Positron Light" = "light"
)

# theme LABEL -> "light"/"dark", via the declaring extension's contributes.themes[].uiTheme.
tx_theme_kind <- function(name,
                          ext_dir = file.path(path.expand("~"), ".positron-server", "extensions")) {
  tryCatch({
    if (is.null(name) || !nzchar(name)) return(NULL)
    if (name %in% names(tx_builtin_themes)) return(unname(tx_builtin_themes[[name]]))
    if (!dir.exists(ext_dir)) return(NULL)
    # ONE level deep -- an extension is `<ext_dir>/<publisher.name-version>/package.json`. A recursive
    # list.files() over that tree walks every extension's whole source and cost 70 ms, which is not a
    # thing to spend at .onLoad (and this runs there, via set_color_palette).
    cands <- file.path(list.dirs(ext_dir, recursive = FALSE), "package.json")
    # A contributes.themes[] entry is a FLAT object, so `\{[^{}]*\}` isolates each one exactly -- no
    # JSON parser needed for two keys whose order we cannot assume.
    for (p in cands[file.exists(cands)]) {
      txt <- tryCatch(paste(readLines(p, warn = FALSE), collapse = ""), error = function(e) "")
      if (!grepl(name, txt, fixed = TRUE)) next          # cheap reject before the object scan
      for (obj in regmatches(txt, gregexpr("\\{[^{}]*\"uiTheme\"[^{}]*\\}", txt))[[1]]) {
        lab <- regmatches(obj, regexpr('"(label|id)"\\s*:\\s*"[^"]*"', obj))
        if (length(lab) == 0 || !identical(sub('^"[^"]*"\\s*:\\s*"(.*)"$', "\\1", lab), name)) next
        ui <- regmatches(obj, regexpr('"uiTheme"\\s*:\\s*"[^"]*"', obj))
        if (length(ui) == 0) next
        ui <- sub('^"uiTheme"\\s*:\\s*"(.*)"$', "\\1", ui)
        return(if (ui %in% c("vs-dark", "hc-black")) "dark" else "light")
      }
    }
    NULL
  }, error = function(e) NULL)
}

tx_positron_theme <- function(settings = tx_positron_settings()) {
  if (is.null(settings)) return(NULL)
  # BAIL: with autoDetectColorScheme the live theme comes from window.preferredDark/LightColorTheme
  # following the OS, so workbench.colorTheme is stale and would be actively wrong.
  if (isTRUE(settings$auto_detect)) return(NULL)
  tx_theme_kind(settings$theme)
}

# === SECTION: the resolver ========================================================================

# Layered, in decreasing confidence; anything unresolved is "light". Shape borrowed from
# cli:::detect_dark_theme() (RSTUDIO -> getThemeInfo()$dark; iTerm; Emacs; else FALSE), extended with
# the Positron branch cli has not got.
#' @keywords internal
tx_detect_theme <- function() {
  ide <- tryCatch(tx_ide(), error = function(e) "terminal")

  if (identical(ide, "rstudio")) {
    d <- tx_rstudio_dark()
    if (!is.null(d)) return(if (d) "dark" else "light")
  }
  if (identical(ide, "positron")) {
    k <- tx_positron_theme()
    if (!is.null(k)) return(k)
  }
  # COLORFGBG is "fg;bg" (some terminals "fg;;bg"); bg 0-6 and 8 are dark.
  fgbg <- Sys.getenv("COLORFGBG", "")
  if (nzchar(fgbg)) {
    parts <- strsplit(fgbg, ";", fixed = TRUE)[[1]]
    bg    <- suppressWarnings(as.integer(utils::tail(parts, 1L)))
    if (!is.na(bg)) return(if (bg %in% c(0:6, 8)) "dark" else "light")
  }
  "light"
}
