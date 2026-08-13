# PURPOSE: Jamovi module export helpers (Phase 7g; Phase 15c robustness pass) -- write a built table
#          to Excel / HTML / Markdown, and resolve a user-typed FOLDER + FILENAME robustly inside
#          Jamovi's Electron-locked engine. Phase 17i also parks here the SHARED R6 backend helpers
#          (jmv_backend_*) that both module orchestrators (jmvtab.b.R / jmvtabreg.b.R) delegate to.
#          Phase 18o: export_documents_dir() is now a robust per-OS known-folder resolver (registry
#          on Windows, xdg-or-$HOME/Documents on Linux, $HOME/Documents on macOS), backed by the doc_*
#          detectors below, and the "~/Documents"/"~"/"auto" default routes THROUGH it. The wider
#          jmvtest diagnostic toolkit that drove the experiment is archived in dev/jamovi/jmvtest.b.R.
# ROLE: Engine-free, session-free helpers so the export logic is unit-testable without a live
#       jamovi session. jmvtab.b.R detects the export click, resolves the path, and calls
#       jmvtab_export(); the click is a boolean read (§5.3) and the result (the path REALLY written) is
#       reported as a bold green / red status line prepended above the results table.
# KEY CONSTRAINTS:
#   - No native file/folder picker exists for a module (dev guide §14) -- typed strings are the only
#     route. Phase 15c splits the old single `path` into a FOLDER box + a bare FILENAME box (the
#     format's extension is authoritative, never typed); resolveExportPath() composes + sanitises them.
#   - The module runs in Jamovi's BUNDLED R where path.expand("~") -> Documents, so we expand ~ via
#     the OS home (fs::path_home() / USERPROFILE / HOME), NOT path.expand()/sub() (§5.2 / §14.3 bug).
#   - `fs` (Suggests) makes path_home / path_sanitize / dir_create cross-platform-robust; every use is
#     guarded with a base-R fallback so export never HARD-depends on it.
# See: dev/tabxplor_2.0.0_jamovi_dev.md §14 ; CLAUDE.md > 2.0.0 roadmap > Phase 7g / Phase 15c.

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

# The default export folder: the user's real Documents, resolved robustly per-OS. jamovi writes files
# from R (bypassing jamovi's native `Dirs`), so we mirror `Dirs` here. Returns a directory that is
# EXISTS+writable, else one whose PARENT is writable (jmvtab_export() creates it), else tempdir() --
# never errors, never returns NA. Proven live 2026-07-22 (see dev/tabxplor_2.0.0_jamovi_dev.md § Phase o).
# DESIGN:
#   Windows -- readRegistry `Shell Folders\Personal` = the resolved known-folder path: honours a
#     D:\Documents redirect AND a university GPO folder-redirection UNC path (the redirected absolute
#     path is exactly what that value holds). Fallbacks: `reg.exe query` (if readRegistry is blocked in
#     the locked engine), then `User Shell Folders\Personal`, then USERPROFILE\Documents. NOT PowerShell
#     -- powershell.exe is absent from the bundled R's PATH (the live test proved GetFolderPath empty).
#   macOS   -- $HOME/Documents (always the right place, non-redirectable).
#   Linux   -- xdg-user-dir DOCUMENTS / ~/.config/user-dirs.dirs, but ONLY when it names a real
#     SUBfolder (!= $HOME): on a normal Ubuntu DESKTOP it returns ~/Documents (the winner); on
#     server / minimal / container / WSL it echoes bare $HOME (xdg-user-dirs never ran) -- there we use
#     $HOME/Documents and CREATE it. Folder names may be localized, so we never hardcode "Documents".
#' @keywords internal
#' @noRd
export_documents_dir <- function() {
  tryCatch({
    home      <- export_home_dir()
    home_docs <- file.path(home, "Documents")
    # An xdg result counts only when it is a genuine subfolder, not a bare $HOME echo (unconfigured distro).
    xdg_sub <- function(f) {
      d <- f()
      if (!is.na(d) && !identical(normalizePath(d,    mustWork = FALSE),
                                  normalizePath(home, mustWork = FALSE))) d else NA_character_
    }
    cands <- if (.Platform$OS.type == "windows") {
      c(doc_win_reg_shell(), doc_win_regexe(), doc_win_reg_usershell(), home_docs)
    } else if (identical(Sys.info()[["sysname"]], "Darwin")) {
      home_docs
    } else {
      c(xdg_sub(doc_xdg), xdg_sub(doc_xdg_file), home_docs)
    }
    cands <- unique(cands[!is.na(cands) & nzchar(cands)])

    for (d in cands) if (export_writable(d))          return(d)  # 1) exists + writable
    for (d in cands) if (export_writable(dirname(d))) return(d)  # 2) creatable (parent writable)
    if (export_writable(dirname(home_docs))) home_docs else tempdir()   # 3) universal safety net
  }, error = function(e) tempdir())
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
# The default option value "~/Documents" (and blank, "~", "auto") is a SENTINEL meaning "my Documents"
# -> the redirect-aware export_documents_dir() (else it would naively expand to <home>/Documents and
# miss a D:\Documents redirect -- the live bug). Any OTHER typed path is respected; a leading ~ on a
# real path (e.g. ~/Desktop) still expands to the OS home. blank / all-illegal filename -> "Table";
# the `ext` (from the chosen format) is always authoritative. Never touches the filesystem.
#' @keywords internal
#' @noRd
resolveExportPath <- function(dir, filename, ext = "xlsx") {
  dir  <- export_unwrap(dir)
  base <- export_sanitize_filename(filename)

  if (!nzchar(dir) || tolower(dir) %in% c("~", "~/documents", "auto"))
    dir <- export_documents_dir()
  else
    dir <- export_expand_home(dir)
  if (!nzchar(base)) base <- "Table"

  normalizePath(file.path(dir, paste0(base, ".", ext)), mustWork = FALSE)
}

# Auto-number a target file when NOT replacing and it already exists: "Table.xlsx" -> "Table1.xlsx" ->
# "Table2.xlsx" ... (the historical tab_xl scheme, now format-agnostic via tools::file_ext). replace =
# TRUE, or a free path, returns `path` unchanged. THE single "replace" rule -- shared by tab_xl() (direct
# R use) AND every jamovi export format, so Excel / HTML / Markdown auto-number identically and the caller
# can report the path that was REALLY written (jmvtab_export() returns this, not the requested path).
#' @keywords internal
#' @noRd
export_number_path <- function(path, replace = FALSE) {
  if (isTRUE(replace) || !file.exists(path)) return(path)
  stem <- tools::file_path_sans_ext(path)
  ext  <- tools::file_ext(path)
  dot  <- if (nzchar(ext)) paste0(".", ext) else ""
  i <- 0L
  repeat { i <- i + 1L; cand <- paste0(stem, i, dot); if (!file.exists(cand)) return(cand) }
}

# The export status line shown above the results table: BOLD green on success (with the file's REAL
# path), BOLD red on failure. jamovi's Notice has no green/success type, so this is inline-styled HTML
# prepended to the html_table content (the one results Html element that always renders).
#' @keywords internal
#' @noRd
export_status_html <- function(text, ok = TRUE) {
  esc <- function(s) {
    s <- gsub("&", "&amp;", s, fixed = TRUE)
    s <- gsub("<", "&lt;",  s, fixed = TRUE)
    gsub(">", "&gt;", s, fixed = TRUE)
  }
  color <- if (isTRUE(ok)) "#1a7f37" else "#c62828"   # green / red
  lead  <- if (isTRUE(ok)) "Saved to: " else "Export failed: "
  paste0("<div style=\"margin:8px 2px;font-weight:bold;color:", color, ";\">",
         esc(lead), esc(as.character(text)[1]), "</div>")
}


# === Documents-folder detectors (support export_documents_dir(); Phase 18o) =============
# The per-OS known-folder detectors export_documents_dir() composes, chosen from the live jmvtest
# experiment (see dev/tabxplor_2.0.0_jamovi_dev.md § Phase o). jamovi never resolves paths in R -- its
# native `Dirs` does (SHGetKnownFolderPath on Windows, xdg-user-dir DOCUMENTS on Linux) -- and tabxplor
# writes files itself, so these mirror `Dirs`. Every detector is guarded (tryCatch) and returns a
# single clean path or NA -- none error, whatever the OS. (The wider diagnostic toolkit that also drove
# jmvtest -- powershell/onedrive/wsl detectors, fallback probes, HTML panels -- is archived with the
# retired analysis in dev/jamovi/jmvtest.b.R.)

# Running under WSL (a Linux binary on the Windows kernel)? Gates the Windows-interop detectors.
#' @keywords internal
#' @noRd
export_is_wsl <- function() {
  if (nzchar(Sys.getenv("WSL_DISTRO_NAME"))) return(TRUE)
  # WARNING: gate on file.exists FIRST (the doc_xdg_file pattern below). readLines(warn = FALSE)
  # only silences the incomplete-final-line warning: when /proc/version is absent, file() signals a
  # WARNING and *then* an error, so an error-only tryCatch lets the warning escape to the caller.
  # That leaked one warning per Documents-resolution on Windows (9 in the test file alone).
  if (!file.exists("/proc/version")) return(FALSE)
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

# --- Documents detectors used by export_documents_dir() -----------------------------------

# Registry HCU ...\Explorer\Shell Folders -> Personal (the already-expanded absolute path, redirects
# honoured). The Windows primary; Windows only.
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

# home/Documents -- the last-resort candidate (built from the OS home, blind to any redirect; used
# only when every OS-native detector above yields nothing).
#' @keywords internal
#' @noRd
doc_home_documents <- function() export_norm1(file.path(export_home_dir(), "Documents"))

# --- Writability probe --------------------------------------------------------------------

# TRUE when `dir` exists and is writable, checked WITHOUT creating anything (file.access mode 2).
#' @keywords internal
#' @noRd
export_writable <- function(dir) {
  if (length(dir) != 1L || is.na(dir) || !nzchar(dir)) return(FALSE)
  isTRUE(dir.exists(dir) && file.access(dir, mode = 2L) == 0L)
}

# Render a built tab (or list of tabs) to a self-contained HTML string via the Phase 10e home-built
# html engine (inline CSS in a <style> block), so the file opens in any browser with no external
# assets, webshot, pandoc -- or even kableExtra (which the html engine does not use).
#' @keywords internal
#' @noRd
tab_html_string <- function(tabs, wrap_rows = 35, wrap_cols = 15, standalone = TRUE, ...) {
  # tooltips follow the option default (tabxplor.tab_kable_tooltips, seeded TRUE): the native
  # `title=` attrs work in any browser with no JS; `...` can still pass tooltips = FALSE.
  k    <- tab_html(tabs, engine = "html", wrap_rows = wrap_rows,
                   wrap_cols = wrap_cols, ...)
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

  # Apply the "replace" rule ONCE, here, for EVERY format (Excel / HTML / Markdown alike) -- so the
  # auto-numbering and the reported path are identical across formats. tab_xl() is then handed the
  # already-final path with replace = TRUE so it does not number a second time.
  path <- export_number_path(path, replace)

  # The writer itself is left UNwrapped: a low-level failure keeps its full rlang cause chain, which
  # the backend surfaces via conditionMessage() (Phase 15c un-masking) -- not the bare "In index: 1."
  switch(
    format,
    excel = tab_xl(tabs, path = path, sheets = "unique", open = FALSE, replace = TRUE),
    # Phase 10e: html export uses the self-contained home-built engine -> no kableExtra needed.
    html  = writeLines(tab_html_string(tabs, ...), path),
    md    = tab_md(tabs, file = path, print = FALSE)
  )
  invisible(path)          # the path REALLY written (auto-numbered), for the caller's status message
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

# Handle the `exportExcel` boolean-click Action (§5.3): resolve the typed FOLDER + FILENAME + the
# format's extension into a path, write via jmvtab_export(), and RETURN a styled status line (bold green
# with the path REALLY written, bold red on failure) for the caller to prepend above the results table.
# Returns "" when no export was requested. conditionMessage() (not err$message) surfaces the FULL rlang
# cause chain (Phase 15c un-masking) -- the bare err$message is only the top "In index: 1." wrapper.
#' @keywords internal
#' @noRd
jmv_backend_export <- function(self, tabs) {
  if (!isTRUE(self$options$exportExcel)) return("")
  fmt <- self$options$export_format
  ext <- switch(fmt, "excel" = "xlsx", "html" = "html", "md" = "md", "xlsx")
  p   <- resolveExportPath(self$options$export_dir, self$options$export_filename, ext)
  tryCatch({
    actual <- jmvtab_export(tabs, format = fmt, path = p, replace = self$options$xl_replace)
    export_status_html(actual, ok = TRUE)
  }, error = function(err) {
    export_status_html(conditionMessage(err), ok = FALSE)
  })
}

# Render a built tab (or list of tabs) to standalone HTML for the jamovi results iframe: the Phase 10e
# dependency-free home-built html engine (inline CSS, no kableExtra) wrapped in a scroll box.
# DESIGN: tooltips are ON by default (the option tabxplor.tab_kable_tooltips, seeded TRUE): the
# non-popover attrs carry the content in the native `title=` attribute, which needs NO bootstrap JS
# and works in jamovi's results webview (the multinomial empirical_tips become reachable). The
# Phase-7e perf cost of building them (~+15% render time, ~+44% DOM bytes) is an accepted trade.
# Opt-out = options(tabxplor.tab_kable_tooltips = FALSE) -- note that inside jamovi's bundled R a
# user has no practical .Rprofile, so this is effectively the fixed default there. Popovers
# (tabxplor.kable_popover) stay off: their content lives in data-content, dead without bootstrap JS.
#' @keywords internal
#' @noRd
jmv_backend_render_html <- function(self, tabs) {
  tab_html(
    tabs, engine = "html",
    wrap_rows = self$options$wrap_rows,
    wrap_cols = self$options$wrap_cols
  ) |>
    tab_render_scrollbox()
}
