# PURPOSE: Jamovi module export helpers (Phase 7g) -- write a built table to Excel / HTML /
#          Markdown, and resolve a user-typed path robustly inside Jamovi's Electron-locked engine.
# ROLE: Engine-free, session-free helpers so the export logic is unit-testable without a live
#       jamovi session. jmvtab.b.R detects the export click, resolves the path, and calls
#       jmvtab_export(); the click is a boolean read (§5.3) and the result is reported via a Notice.
# KEY CONSTRAINTS:
#   - No native file/folder picker exists for a module (dev guide §14) -- a typed path string is
#     the only route; resolveExportPath() makes it robust (Windows "Copy as path", ~, extension).
#   - The module runs in Jamovi's BUNDLED R where path.expand("~") -> Documents, so we expand ~
#     via Sys.getenv("USERPROFILE") (dev guide §5.2 / §14.3), NOT path.expand()/sub().
# See: dev/tabxplor_1.4.0_jamovi_dev.md §14 ; CLAUDE.md > 1.4.0 roadmap > Phase 7g.

# Resolve a user-typed export path to an absolute path with the right extension. Adapted from
# SummaryTables::resolveExportPath (dev/jamovi/reference/SummaryTables/export.R), generalised so
# the caller's `ext` (from the chosen format) decides the extension.
#' @keywords internal
#' @noRd
resolveExportPath <- function(path, ext = "xlsx") {
  path <- trimws(path)
  path <- gsub("^[\"']|[\"']$", "", path)                       # strip Windows "Copy as path" quotes

  getHome <- function() {                                        # USERPROFILE on Windows, HOME elsewhere
    h <- Sys.getenv("USERPROFILE")
    if (h == "") h <- Sys.getenv("HOME")
    h
  }

  # Blank or directory-only input -> a friendly default in the real Documents folder.
  if (nchar(path) == 0 || path %in% c("~", "~/")) path <- paste0("~/Documents/Table.", ext)

  # Expand a leading ~ with paste0(substring()), NOT sub() -- USERPROFILE holds backslashes that
  # sub() would read as backreferences (the §14.3 bug).
  if (grepl("^~", path)) path <- paste0(getHome(), substring(path, 2))

  # A bare filename (no separator) lands in Documents.
  if (!grepl("[/\\\\]", path)) path <- file.path(getHome(), "Documents", path)

  # Ensure the extension AFTER path assembly (so ~ and dirs stay intact).
  if (!grepl(paste0("\\.", ext, "$"), path, ignore.case = TRUE)) path <- paste0(path, ".", ext)

  normalizePath(path, mustWork = FALSE)
}

# Render a built tab (or list of tabs) to a self-contained HTML string. Reuses tab_kable() +
# inlines the lightable/bootstrap CSS (the same route jmvtab.b.R's live render uses), so the file
# opens correctly in any browser with no external assets, webshot or pandoc.
#' @keywords internal
#' @noRd
tab_html_string <- function(tabs, wrap_rows = 35, wrap_cols = 15, standalone = TRUE, ...) {
  k <- tab_kable(tabs, wrap_rows = wrap_rows, wrap_cols = wrap_cols,
                 fixed_thead = FALSE, tooltips = FALSE, position = "left", ...)

  css_of <- function(pkg, file) {
    p <- system.file(file, package = pkg)
    if (nzchar(p)) as.character(htmltools::includeCSS(p)) else ""
  }
  css  <- paste0(css_of("kableExtra", "lightable-0.0.1/lightable.css"),
                 css_of("rmarkdown", "rmd/h/bootstrap/css/cosmo.min.css"))
  body <- as.character(k)

  if (!standalone) return(paste0(css, body))
  paste0("<!DOCTYPE html>\n<html>\n<head>\n<meta charset=\"utf-8\"/>\n",
         css, "\n</head>\n<body>\n", body, "\n</body>\n</html>\n")
}

# Write a built tab (or list of tabs) to a file in the chosen format. Returns the path invisibly.
# The single dispatch point shared by the jamovi backend and its tests.
#' @keywords internal
#' @noRd
jmvtab_export <- function(tabs, format = c("excel", "html", "md"), path, replace = FALSE, ...) {
  format <- match.arg(format)
  dir <- dirname(path)                                          # writeLines/tab_md need it to exist
  if (nzchar(dir) && !dir.exists(dir)) dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  switch(
    format,
    excel = {
      if (!requireNamespace("openxlsx", quietly = TRUE)) {
        stop("Package 'openxlsx' is required for Excel export.", call. = FALSE)
      }
      tab_xl(tabs, path = path, sheets = "unique", open = FALSE, replace = replace)
    },
    html = {
      if (!requireNamespace("kableExtra", quietly = TRUE)) {
        stop("Package 'kableExtra' is required for HTML export.", call. = FALSE)
      }
      writeLines(tab_html_string(tabs, ...), path)
    },
    md = tab_md(tabs, file = path, print = FALSE)
  )
  invisible(path)
}
