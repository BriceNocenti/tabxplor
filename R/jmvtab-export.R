# PURPOSE: Jamovi module export helpers (Phase 7g; Phase 15c robustness pass) -- write a built table
#          to Excel / HTML / Markdown, and resolve a user-typed FOLDER + FILENAME robustly inside
#          Jamovi's Electron-locked engine. Phase 17i also parks here the SHARED R6 backend helpers
#          (jmv_backend_*) that both module orchestrators (jmvtab.b.R / jmvtabreg.b.R) delegate to.
#          Last Phase o adds the Documents-folder DETECTORS (doc_* / fb_* / export_writable /
#          export_write_test / export_*_candidates / export_env_probe) driving the throwaway `jmvtest`
#          diagnostic (R/jmvtest.b.R) -- the detectors are the seed of the eventual
#          export_documents_dir() rewrite and stay; the panel/HTML glue goes with jmvtest.
# ROLE: Engine-free, session-free helpers so the export logic is unit-testable without a live
#       jamovi session. jmvtab.b.R detects the export click, resolves the path, and calls
#       jmvtab_export(); the click is a boolean read (§5.3) and the result is reported via a Notice.
# KEY CONSTRAINTS:
#   - No native file/folder picker exists for a module (dev guide §14) -- typed strings are the only
#     route. Phase 15c splits the old single `path` into a FOLDER box + a bare FILENAME box (the
#     format's extension is authoritative, never typed); resolveExportPath() composes + sanitises them.
#   - The module runs in Jamovi's BUNDLED R where path.expand("~") -> Documents, so we expand ~ via
#     the OS home (fs::path_home() / USERPROFILE / HOME), NOT path.expand()/sub() (§5.2 / §14.3 bug).
#   - `fs` (Suggests) makes path_home / path_sanitize / dir_create cross-platform-robust; every use is
#     guarded with a base-R fallback so export never HARD-depends on it.
# See: dev/tabxplor_1.4.0_jamovi_dev.md §14 ; CLAUDE.md > 1.4.0 roadmap > Phase 7g / Phase 15c.

# The OS home folder. fs::path_home() reads USERPROFILE (Windows) / HOME (Unix) via libuv -- more
# robust than either env var alone; fall back to the env vars when `fs` is absent.
#' @keywords internal
#' @noRd
export_home_dir <- function() {
  if (requireNamespace("fs", quietly = TRUE)) {
    h <- tryCatch(as.character(fs::path_home()), error = function(e) "")
    if (length(h) && nzchar(h[1])) return(h[1])
  }
  h <- Sys.getenv("USERPROFILE")
  if (!nzchar(h)) h <- Sys.getenv("HOME")
  h
}

# Expand Windows %VAR% tokens (a `User Shell Folders` value may hold %USERPROFILE%\Documents).
#' @keywords internal
#' @noRd
export_expand_winenv <- function(p) {
  toks <- regmatches(p, gregexpr("%[^%]+%", p))[[1]]
  for (t in unique(toks)) {
    v <- Sys.getenv(gsub("%", "", t, fixed = TRUE))
    if (nzchar(v)) p <- gsub(t, v, p, fixed = TRUE)
  }
  p
}

# The default export folder: the user's Documents.
# DESIGN: on Windows a REDIRECTED Documents (e.g. D:\Documents) is invisible to <home>/Documents --
# the locked jamovi Electron R only sees USERPROFILE=C:\Users\<x>, so it wrongly created
# C:\Users\<x>\Documents. Read the resolved known-folder path from the registry instead: the
# `Shell Folders\Personal` value holds the already-expanded absolute path (redirects honoured), with
# `User Shell Folders\Personal` (env-token form) then <home>/Documents as fallbacks. Off Windows,
# <home>/Documents is correct (jamovi's ~ already IS Documents, so we build from home, not
# path.expand(), to avoid a Documents/Documents double -- §14.3).
#' @keywords internal
#' @noRd
export_documents_dir <- function() {
  fallback <- file.path(export_home_dir(), "Documents")
  if (.Platform$OS.type != "windows") return(fallback)
  read_personal <- function(subkey) tryCatch({
    reg <- utils::readRegistry(
      file.path("Software", "Microsoft", "Windows", "CurrentVersion", "Explorer", subkey,
                fsep = "\\"),
      hive = "HCU", maxdepth = 1L)
    p <- reg[["Personal"]]
    if (is.character(p) && length(p) && nzchar(p[1])) export_expand_winenv(p[1]) else NA_character_
  }, error = function(e) NA_character_)
  doc <- read_personal("Shell Folders")
  if (is.na(doc)) doc <- read_personal("User Shell Folders")
  if (is.na(doc) || !nzchar(doc)) fallback else doc
}

# Expand a leading ~ to the OS home with substring() (NOT sub(): USERPROFILE holds backslashes sub()
# would read as backreferences, the §14.3 bug).
#' @keywords internal
#' @noRd
export_expand_home <- function(p) if (grepl("^~", p)) paste0(export_home_dir(), substring(p, 2)) else p

# Strip surrounding quotes / brackets a user may paste around a path or name ("Copy as path", <..>, ..).
#' @keywords internal
#' @noRd
export_unwrap <- function(s) {
  s <- trimws(as.character(if (length(s)) s[1] else ""))
  # The set of wrapping chars to strip from either end. `]` is FIRST so the POSIX bracket class reads
  # it literally (backslash does NOT escape inside a POSIX class), and `[` mid-class is literal too.
  wrap <- "[]'\"<>[(){}]"
  s <- sub(paste0("^", wrap, "+"), "", s)
  s <- sub(paste0(wrap, "+$"), "", s)
  trimws(s)
}

# Turn a user-typed name into a safe, bare file name (no directory, no extension, no OS-illegal
# chars / reserved names). fs::path_sanitize() is the robust route (removes control/reserved chars,
# Windows reserved names, trailing dots); a base-R fallback strips the same illegal set.
#' @keywords internal
#' @noRd
export_sanitize_filename <- function(name) {
  name <- basename(export_unwrap(name))                    # drop any directory pasted into the name box
  if (requireNamespace("fs", quietly = TRUE)) {
    name <- tryCatch(as.character(fs::path_sanitize(name)), error = function(e) name)
  } else {
    name <- gsub('[/\\\\?<>:*|":[:cntrl:]]', "", name)     # OS-illegal characters
    name <- sub("[. ]+$", "", name)                        # trailing dots / spaces (invalid on Windows)
  }
  sub("\\.[A-Za-z0-9]{1,5}$", "", trimws(name))            # drop any extension the user typed
}

# Resolve a user-typed FOLDER + bare FILENAME + the format's extension into one absolute file path.
# Blank folder -> Documents; blank / all-illegal filename -> "Table"; the `ext` (from the chosen
# format) is always authoritative. Never touches the filesystem.
#' @keywords internal
#' @noRd
resolveExportPath <- function(dir, filename, ext = "xlsx") {
  dir  <- export_unwrap(dir)
  base <- export_sanitize_filename(filename)

  if (!nzchar(dir))  dir  <- export_documents_dir()
  dir <- export_expand_home(dir)
  if (!nzchar(base)) base <- "Table"

  normalizePath(file.path(dir, paste0(base, ".", ext)), mustWork = FALSE)
}


# === Export-folder detection & diagnostics (Last Phase o) ==================================
# A throwaway jamovi analysis (`jmvtest`, R/jmvtest.b.R) probes MANY ways to find the user's real
# Documents folder and writes a plain .md test file to each, so the maintainer can report -- from
# real Windows / WSL / macOS machines -- which method lands where (the default "~/Documents" resolves
# to C:\Users\<x>\Documents, missing a D:\Documents redirect; and to a non-existent ~/Documents on a
# fresh WSL distro). jamovi never resolves paths in R -- its native `Dirs` does (SHGetKnownFolderPath
# on Windows, xdg-user-dir DOCUMENTS on Linux); tabxplor writes files itself, so these mirror `Dirs`.
# DESIGN: the doc_* / fb_* detectors + export_writable() + export_write_test() + export_*_candidates()
# are the SEED of the eventual export_documents_dir() rewrite and STAY when jmvtest is removed;
# export_env_probe() / export_probe_html() are panel glue that goes with it. Every detector is guarded
# (tryCatch) and returns a single clean path or NA -- none error, whatever the OS.

# Running under WSL (a Linux binary on the Windows kernel)? Gates the Windows-interop detectors.
#' @keywords internal
#' @noRd
export_is_wsl <- function() {
  if (nzchar(Sys.getenv("WSL_DISTRO_NAME"))) return(TRUE)
  pv <- tryCatch(readLines("/proc/version", n = 1L, warn = FALSE), error = function(e) character())
  length(pv) && grepl("microsoft|WSL", pv[1], ignore.case = TRUE)
}

# First non-blank element, trimmed, else NA -- the single clean value every detector returns.
#' @keywords internal
#' @noRd
export_norm1 <- function(x) {
  x <- tryCatch(as.character(x), error = function(e) character())
  x <- x[!is.na(x) & nzchar(trimws(x))]
  if (length(x)) trimws(x[1]) else NA_character_
}

# On WSL, convert a Windows path (X:\...) to its /mnt/... form via `wslpath -u`; elsewhere a no-op.
#' @keywords internal
#' @noRd
export_wsl_to_unix <- function(p) {
  if (is.na(p) || !export_is_wsl() || !grepl("^[A-Za-z]:[\\\\/]", p)) return(p)
  wp <- Sys.which("wslpath"); if (!nzchar(wp)) return(p)
  q <- tryCatch(export_norm1(suppressWarnings(
    system2(wp, c("-u", shQuote(p)), stdout = TRUE, stderr = FALSE))), error = function(e) NA_character_)
  if (is.na(q)) p else q
}

# --- Documents detectors (>= 5; the strongest first) --------------------------------------

# PowerShell [Environment]::GetFolderPath('MyDocuments') -- the redirection-aware known-folder API
# (a D:\Documents move is honoured). Reachable from bundled Windows R AND from WSL (powershell.exe is
# on the WSL PATH); the WSL result (X:\...) is wslpath-converted. Empty when the folder is absent.
#' @keywords internal
#' @noRd
doc_win_powershell <- function() tryCatch({
  ps <- Sys.which("powershell.exe"); if (!nzchar(ps)) ps <- Sys.which("pwsh")
  if (!nzchar(ps)) return(NA_character_)
  out <- suppressWarnings(system2(
    ps, c("-NoProfile", "-NonInteractive", "-Command",
          shQuote("[Environment]::GetFolderPath('MyDocuments')")),
    stdout = TRUE, stderr = FALSE))
  export_wsl_to_unix(export_norm1(out))
}, error = function(e) NA_character_)

# Registry HCU ...\Explorer\Shell Folders -> Personal (the already-expanded absolute path, redirects
# honoured). What export_documents_dir() uses today; Windows only.
#' @keywords internal
#' @noRd
doc_win_reg_shell <- function() {
  if (.Platform$OS.type != "windows") return(NA_character_)
  tryCatch({
    reg <- utils::readRegistry(
      file.path("Software", "Microsoft", "Windows", "CurrentVersion", "Explorer", "Shell Folders",
                fsep = "\\"), hive = "HCU", maxdepth = 1L)
    export_norm1(reg[["Personal"]])
  }, error = function(e) NA_character_)
}

# Registry HCU ...\Explorer\User Shell Folders -> Personal (the authoritative env-token form) + expand.
#' @keywords internal
#' @noRd
doc_win_reg_usershell <- function() {
  if (.Platform$OS.type != "windows") return(NA_character_)
  tryCatch({
    reg <- utils::readRegistry(
      file.path("Software", "Microsoft", "Windows", "CurrentVersion", "Explorer", "User Shell Folders",
                fsep = "\\"), hive = "HCU", maxdepth = 1L)
    p <- export_norm1(reg[["Personal"]])
    if (is.na(p)) NA_character_ else export_norm1(export_expand_winenv(p))
  }, error = function(e) NA_character_)
}

# reg.exe query -- a subprocess route in case utils::readRegistry is blocked in the locked engine.
# Native on Windows; reachable on the WSL PATH (the returned X:\... is wslpath-converted).
#' @keywords internal
#' @noRd
doc_win_regexe <- function() tryCatch({
  rg <- Sys.which("reg.exe"); if (!nzchar(rg)) rg <- Sys.which("reg")
  if (!nzchar(rg)) return(NA_character_)
  key <- "HKCU\\Software\\Microsoft\\Windows\\CurrentVersion\\Explorer\\Shell Folders"
  out <- suppressWarnings(system2(rg, c("query", shQuote(key), "/v", "Personal"),
                                  stdout = TRUE, stderr = FALSE))
  line <- grep("Personal", out, value = TRUE, fixed = TRUE)
  if (!length(line)) return(NA_character_)
  m <- regmatches(line[1], regexpr("REG_[A-Z_]+[ \t]+.*$", line[1]))
  if (!length(m)) return(NA_character_)
  val <- trimws(sub("^REG_[A-Z_]+[ \t]+", "", m))
  export_wsl_to_unix(export_norm1(export_expand_winenv(val)))
}, error = function(e) NA_character_)

# OneDrive Known-Folder-Move: the OneDrive* env root + \Documents (common on Windows 11).
#' @keywords internal
#' @noRd
doc_win_onedrive <- function() {
  for (v in c("OneDrive", "OneDriveConsumer", "OneDriveCommercial")) {
    od <- Sys.getenv(v)
    if (nzchar(od)) return(export_wsl_to_unix(export_norm1(file.path(od, "Documents"))))
  }
  NA_character_
}

# xdg-user-dir DOCUMENTS -- the freedesktop way; what jamovi's own native `Dirs` uses on Linux.
#' @keywords internal
#' @noRd
doc_xdg <- function() tryCatch({
  x <- Sys.which("xdg-user-dir"); if (!nzchar(x)) return(NA_character_)
  export_norm1(suppressWarnings(system2(x, "DOCUMENTS", stdout = TRUE, stderr = FALSE)))
}, error = function(e) NA_character_)

# Parse ~/.config/user-dirs.dirs for XDG_DOCUMENTS_DIR -- the fallback when the xdg-user-dir binary is
# off a sandboxed (flatpak) PATH.
#' @keywords internal
#' @noRd
doc_xdg_file <- function() tryCatch({
  f <- file.path(export_home_dir(), ".config", "user-dirs.dirs")
  if (!file.exists(f)) return(NA_character_)
  ln <- grep("^[ \t]*XDG_DOCUMENTS_DIR", readLines(f, warn = FALSE), value = TRUE)
  if (!length(ln)) return(NA_character_)
  val <- gsub("\"", "", trimws(sub("^[^=]*=", "", ln[1])))
  export_norm1(gsub("$HOME", export_home_dir(), val, fixed = TRUE))
}, error = function(e) NA_character_)

# WSL -> the Windows USERPROFILE via cmd.exe, wslpath-converted, + \Documents (only when /mnt is
# visible inside the distro / flatpak).
#' @keywords internal
#' @noRd
doc_wsl_mnt <- function() {
  if (!export_is_wsl()) return(NA_character_)
  tryCatch({
    cm <- Sys.which("cmd.exe"); if (!nzchar(cm)) return(NA_character_)
    up <- export_norm1(suppressWarnings(
      system2(cm, c("/c", "echo", "%USERPROFILE%"), stdout = TRUE, stderr = FALSE)))
    if (is.na(up) || !grepl("^[A-Za-z]:", up)) return(NA_character_)
    up <- export_wsl_to_unix(up)
    if (is.na(up)) NA_character_ else export_norm1(file.path(up, "Documents"))
  }, error = function(e) NA_character_)
}

# home/Documents -- the naive baseline (= today's default behaviour, blind to any redirect).
#' @keywords internal
#' @noRd
doc_home_documents <- function() export_norm1(file.path(export_home_dir(), "Documents"))

# --- Fallback save locations (>= 5; tempdir is the universal safety net) -------------------
#' @keywords internal
#' @noRd
fb_home      <- function() export_norm1(export_home_dir())
#' @keywords internal
#' @noRd
fb_desktop   <- function() export_norm1(file.path(export_home_dir(), "Desktop"))
#' @keywords internal
#' @noRd
fb_downloads <- function() export_norm1(file.path(export_home_dir(), "Downloads"))
#' @keywords internal
#' @noRd
fb_cwd       <- function() export_norm1(getwd())
#' @keywords internal
#' @noRd
fb_tempdir   <- function() export_norm1(tempdir())

# --- Probes, writes, tables ---------------------------------------------------------------

# TRUE when `dir` exists and is writable, checked WITHOUT creating anything (file.access mode 2) -- so
# the read-only panels never litter. The buttons use export_write_test() for real, persisting writes.
#' @keywords internal
#' @noRd
export_writable <- function(dir) {
  if (length(dir) != 1L || is.na(dir) || !nzchar(dir)) return(FALSE)
  isTRUE(dir.exists(dir) && file.access(dir, mode = 2L) == 0L)
}

# Actually write a plain .md test file into `dir` (creating the folder if needed). Returns
# list(ok, path, error). PERSISTS the file on purpose -- the maintainer finds it in the file manager
# to learn which candidate mapped to their real Documents.
#' @keywords internal
#' @noRd
export_write_test <- function(dir, name, note = NULL) {
  path <- NA_character_
  tryCatch({
    if (length(dir) != 1L || is.na(dir) || !nzchar(dir)) stop("no folder", call. = FALSE)
    if (!dir.exists(dir)) dir.create(dir, recursive = TRUE, showWarnings = FALSE)
    base <- export_sanitize_filename(name); if (!nzchar(base)) base <- "tabxplor_export_test"
    path <- normalizePath(file.path(dir, paste0(base, ".md")), mustWork = FALSE)
    body <- c("# tabxplor export folder test", "",
              paste0("Written: ", format(Sys.time())),
              paste0("Target folder: ", dir),
              paste0("Resolved path: ", path))
    if (!is.null(note)) body <- c(body, "", note)
    # suppressWarnings: an unwritable target makes file() WARN ("cannot open file") then error -- the
    # error is what we want (caught below); the warning is incidental noise the backend must not leak.
    suppressWarnings(writeLines(enc2utf8(body), path, useBytes = TRUE))
    list(ok = TRUE, path = path, error = NA_character_)
  }, error = function(e) list(ok = FALSE, path = path, error = conditionMessage(e)))
}

# One row per method: method label, resolved dir, exists?, writable?. `builders` is a named list of
# 0-arg detectors; append the `current` row (what resolveExportPath() produces for today's default).
#' @keywords internal
#' @noRd
export_candidate_df <- function(builders, add_current = FALSE) {
  rows <- lapply(names(builders), function(nm) {
    d <- tryCatch(builders[[nm]](), error = function(e) NA_character_)
    ok_dir <- !is.na(d) && nzchar(d)
    data.frame(method = nm, dir = if (ok_dir) d else "",
               exists = ok_dir && dir.exists(d), writable = export_writable(d),
               stringsAsFactors = FALSE)
  })
  if (add_current) {
    cur <- tryCatch(resolveExportPath("~/Documents", "tabxplor_export_test", "md"),
                    error = function(e) NA_character_)
    cur_dir <- if (is.na(cur)) "" else dirname(cur)
    rows[[length(rows) + 1L]] <- data.frame(
      method = "CURRENT resolveExportPath(\"~/Documents\")", dir = cur_dir,
      exists = nzchar(cur_dir) && dir.exists(cur_dir), writable = export_writable(cur_dir),
      stringsAsFactors = FALSE)
  }
  do.call(rbind, rows)
}

# The Documents-detection candidate table (>= 5 methods + the current-behaviour row).
#' @keywords internal
#' @noRd
export_doc_candidates <- function() export_candidate_df(list(
  "powershell GetFolderPath(MyDocuments)" = doc_win_powershell,
  "registry Shell Folders\\Personal"      = doc_win_reg_shell,
  "registry User Shell Folders\\Personal" = doc_win_reg_usershell,
  "reg.exe query Shell Folders"           = doc_win_regexe,
  "OneDrive env + \\Documents"            = doc_win_onedrive,
  "xdg-user-dir DOCUMENTS"                = doc_xdg,
  "user-dirs.dirs XDG_DOCUMENTS_DIR"      = doc_xdg_file,
  "WSL cmd.exe USERPROFILE + wslpath"     = doc_wsl_mnt,
  "home/Documents (naive baseline)"       = doc_home_documents
), add_current = TRUE)

# The fallback save-location candidate table (>= 5).
#' @keywords internal
#' @noRd
export_fallback_candidates <- function() export_candidate_df(list(
  "home"           = fb_home,
  "home/Desktop"   = fb_desktop,
  "home/Downloads" = fb_downloads,
  "getwd()"        = fb_cwd,
  "tempdir()"      = fb_tempdir
))

# The environment facts for the diagnostic's first panel (a named character vector).
#' @keywords internal
#' @noRd
export_env_probe <- function() {
  si    <- tryCatch(Sys.info(), error = function(e) character())
  sig   <- function(k) if (length(si) && k %in% names(si)) unname(si[[k]]) else ""
  getv  <- function(v) { x <- Sys.getenv(v); if (nzchar(x)) x else "(unset)" }
  which1 <- function(cmd) { x <- Sys.which(cmd); if (nzchar(x)) unname(x) else "(not found)" }
  ph <- if (requireNamespace("fs", quietly = TRUE))
    tryCatch(as.character(fs::path_home()), error = function(e) "(error)") else "(fs absent)"
  c("R version"            = R.version.string,
    "OS.type"              = .Platform$OS.type,
    "sysname"              = sig("sysname"),
    "release"              = sig("release"),
    "user"                 = sig("user"),
    "nodename"             = sig("nodename"),
    "WSL"                  = if (export_is_wsl()) "yes" else "no",
    "WSL_DISTRO_NAME"      = getv("WSL_DISTRO_NAME"),
    "USERPROFILE"          = getv("USERPROFILE"),
    "HOME"                 = getv("HOME"),
    "HOMEDRIVE"            = getv("HOMEDRIVE"),
    "HOMEPATH"             = getv("HOMEPATH"),
    "OneDrive"             = getv("OneDrive"),
    "XDG_DOCUMENTS_DIR"    = getv("XDG_DOCUMENTS_DIR"),
    "TEMP"                 = getv("TEMP"),
    "TMP"                  = getv("TMP"),
    "fs::path_home()"      = ph,
    "path.expand(\"~\")"   = path.expand("~"),
    "export_home_dir()"    = export_home_dir(),
    "getwd()"              = getwd(),
    "tempdir()"            = tempdir(),
    "which powershell.exe" = which1("powershell.exe"),
    "which cmd.exe"        = which1("cmd.exe"),
    "which wslpath"        = which1("wslpath"),
    "which xdg-user-dir"   = which1("xdg-user-dir"),
    "which reg.exe"        = which1("reg.exe"))
}

# Render an env-probe named vector OR a candidate data.frame to a simple HTML block for a jmvtest panel.
#' @keywords internal
#' @noRd
export_probe_html <- function(x, title = NULL) {
  esc <- function(s) { s <- as.character(s); s[is.na(s)] <- ""
    s <- gsub("&", "&amp;", s, fixed = TRUE); s <- gsub("<", "&lt;", s, fixed = TRUE)
    gsub(">", "&gt;", s, fixed = TRUE) }
  head <- if (is.null(title)) "" else paste0("<h3 style='margin:8px 0 2px'>", esc(title), "</h3>")
  if (is.data.frame(x)) {
    th <- paste0("<th style='text-align:left;padding:2px 10px;border-bottom:1px solid #ccc'>",
                 esc(names(x)), "</th>", collapse = "")
    tr <- vapply(seq_len(nrow(x)), function(i) paste0("<tr>",
      paste0("<td style='padding:2px 10px;font-family:monospace;white-space:nowrap'>",
             esc(unlist(x[i, ], use.names = FALSE)), "</td>", collapse = ""), "</tr>"), character(1))
    body <- paste0("<table style='border-collapse:collapse'><tr>", th, "</tr>",
                   paste0(tr, collapse = ""), "</table>")
  } else {
    tr <- paste0("<tr><td style='padding:1px 10px'>", esc(names(x)),
                 "</td><td style='padding:1px 10px;font-family:monospace'>", esc(unname(x)),
                 "</td></tr>")
    body <- paste0("<table style='border-collapse:collapse'>", paste0(tr, collapse = ""), "</table>")
  }
  paste0(head, body)
}


# Render a built tab (or list of tabs) to a self-contained HTML string via the Phase 10e home-built
# html engine (inline CSS in a <style> block), so the file opens in any browser with no external
# assets, webshot, pandoc -- or even kableExtra (which the html engine does not use).
#' @keywords internal
#' @noRd
tab_html_string <- function(tabs, wrap_rows = 35, wrap_cols = 15, standalone = TRUE, ...) {
  k    <- tab_html(tabs, engine = "html", wrap_rows = wrap_rows,
                   wrap_cols = wrap_cols, tooltips = FALSE, ...)
  body <- as.character(k)
  if (!standalone) return(body)
  # Phase 14k: this is the OTHER page tabxplor builds (print.tabxplor_kable()'s Viewer page is the
  # first), so the same rule applies -- a page we write paints itself, or a dark table lands on a white
  # <body>. The theme rides on the result, so there is no second option resolve to drift from: NULL
  # means no stylesheet shipped, hence nothing of ours to match. "auto" keeps the @media cascade here,
  # unlike the Viewer: this file is opened elsewhere, so only the reader's browser can know.
  theme <- attr(k, "tabxplor_theme")
  page  <- if (is.null(theme)) "" else paste0("<style>\n", tx_page_style(theme), "\n</style>\n")
  paste0("<!DOCTYPE html>\n<html>\n<head>\n<meta charset=\"utf-8\"/>\n", page,
         "</head>\n<body>\n", body, "\n</body>\n</html>\n")
}

# Write a built tab (or list of tabs) to a file in the chosen format. Returns the path invisibly.
# The single dispatch point shared by the jamovi backend and its tests.
#' @keywords internal
#' @noRd
jmvtab_export <- function(tabs, format = c("excel", "html", "md"), path, replace = FALSE, ...) {
  format <- match.arg(format)

  # Pre-flight, friendly checks (concise, non-expert). These run BEFORE the writer so the common
  # failures surface as a clear "what to do" instead of a deep internal error.
  if (format == "excel" && !requireNamespace("openxlsx2", quietly = TRUE)) {
    stop("Excel export needs the 'openxlsx2' package. Install it with ",
         'install.packages("openxlsx2"), or choose HTML or Markdown instead.', call. = FALSE)
  }

  # Ensure the target folder exists (create it if we can); a permission failure is a friendly stop,
  # not a deep file-connection error.
  dir <- dirname(path)
  if (nzchar(dir) && !dir.exists(dir)) {
    created <- tryCatch({
      if (requireNamespace("fs", quietly = TRUE)) fs::dir_create(dir)
      else dir.create(dir, recursive = TRUE, showWarnings = FALSE)
      dir.exists(dir)
    }, error = function(e) FALSE, warning = function(w) FALSE)
    if (!created) {
      stop("Can't create the folder:\n  ", dir,
           "\nChoose a folder that exists, or check you're allowed to write there.", call. = FALSE)
    }
  }

  # The writer itself is left UNwrapped: a low-level failure keeps its full rlang cause chain, which
  # the backend surfaces via conditionMessage() (Phase 15c un-masking) -- not the bare "In index: 1."
  switch(
    format,
    excel = tab_xl(tabs, path = path, sheets = "unique", open = FALSE, replace = replace),
    # Phase 10e: html export uses the self-contained home-built engine -> no kableExtra needed.
    html  = writeLines(tab_html_string(tabs, ...), path),
    md    = tab_md(tabs, file = path, print = FALSE)
  )
  invisible(path)
}


# === Shared jamovi backend helpers (Phase 17i) =============================================
# The two module backends (R/jmvtab.b.R, R/jmvtabreg.b.R) are thin sibling orchestrators; these four
# helpers factor out the blocks that were byte-identical across their .run() / private methods, so a
# fix lands once. Each takes the live R6 `self` (or the data). They run ONLY inside a live jamovi
# session, where jmvcore (Suggests) is guaranteed present -- same context as the code they replace.

# Resolve the weight VARIABLE NAME for a build: the explicit `wt` option, else a Data-level weight
# (Data >>> Weights) carried as the "jmv-weights" attribute (added back as a `.COUNTS` column, since
# self$data holds only the selected variables). Returns list(data = <possibly with .COUNTS>, wt =
# <name or character()>).
#' @keywords internal
#' @noRd
jmv_backend_weights <- function(data, opt_wt) {
  wt <- character()
  if (!is.null(opt_wt) && length(opt_wt)) {
    wt <- opt_wt
  } else if (!is.null(attr(data, "jmv-weights"))) {
    data[[".COUNTS"]] <- jmvcore::toNumeric(attr(data, "jmv-weights"))
    wt <- ".COUNTS"
  }
  list(data = data, wt = wt)
}

# Report an export result via a native jmvcore::Notice (info / error), inserted at the top of the
# results (dev guide §7.6 / §14). Replaces the old hand-built HTML status box.
#' @keywords internal
#' @noRd
jmv_backend_notice <- function(self, text, ok = TRUE) {
  notice <- jmvcore::Notice$new(
    options = self$options, name = "exportNotice",
    type = if (ok) jmvcore::NoticeType$INFO else jmvcore::NoticeType$ERROR
  )
  notice$setContent(text)
  self$results$insert(1, notice)
}

# Handle the `exportExcel` boolean-click Action (§5.3): resolve the typed FOLDER + FILENAME + the
# format's extension into a path, write via jmvtab_export(), and report success / failure via a Notice.
# conditionMessage() (not err$message) surfaces the FULL rlang cause chain (Phase 15c un-masking) --
# the bare err$message is only the top "In index: 1." wrapper.
#' @keywords internal
#' @noRd
jmv_backend_export <- function(self, tabs) {
  if (!isTRUE(self$options$exportExcel)) return(invisible())
  fmt <- self$options$export_format
  ext <- switch(fmt, "excel" = "xlsx", "html" = "html", "md" = "md", "xlsx")
  p   <- resolveExportPath(self$options$export_dir, self$options$export_filename, ext)
  tryCatch({
    jmvtab_export(tabs, format = fmt, path = p, replace = self$options$xl_replace)
    jmv_backend_notice(self, paste0("Saved to: ", p), ok = TRUE)
  }, error = function(err) {
    jmv_backend_notice(self, paste0("Export failed: ", conditionMessage(err)), ok = FALSE)
  })
}

# Render a built tab (or list of tabs) to standalone HTML for the jamovi results iframe: the Phase 10e
# dependency-free home-built html engine (inline CSS, no kableExtra) wrapped in a scroll box. tooltips
# stay OFF here for now (the engine emits the SAME bootstrap tooltip attrs, so they can be turned on
# once verified live).
#' @keywords internal
#' @noRd
jmv_backend_render_html <- function(self, tabs) {
  tab_html(
    tabs, engine = "html",
    wrap_rows = self$options$wrap_rows,
    wrap_cols = self$options$wrap_cols,
    tooltips = FALSE
  ) |>
    tab_render_scrollbox()
}
