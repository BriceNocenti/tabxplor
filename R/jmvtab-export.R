# PURPOSE: Jamovi module export helpers (Phase 7g; Phase 15c robustness pass) -- write a built table
#          to Excel / HTML / Markdown, and resolve a user-typed FOLDER + FILENAME robustly inside
#          Jamovi's Electron-locked engine. Phase 17i also parks here the SHARED R6 backend helpers
#          (jmv_backend_*) that both module orchestrators (jmvtab.b.R / jmvtabreg.b.R) delegate to.
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

# The default export folder: the user's Documents. <home>/Documents is correct on all three platforms
# (in jamovi's bundled R path.expand("~") already IS Documents, so we build it from the home instead --
# NOT path.expand() -- to avoid a Documents/Documents double, §14.3).
#' @keywords internal
#' @noRd
export_documents_dir <- function() file.path(export_home_dir(), "Documents")

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
