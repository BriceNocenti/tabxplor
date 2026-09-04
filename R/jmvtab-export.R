# PURPOSE: The jamovi modules' export path (Excel / HTML / Markdown), the folder + filename resolver
#          it needs inside jamovi's Electron-locked engine, and the R6 backend helpers both module
#          orchestrators share.
# ROLE: Engine-free and session-free, so the logic is testable without a live jamovi session. A
#       backend detects the export click, resolves the path and calls jmvtab_export(); what comes
#       back -- the path REALLY written, or the failure -- is a status box under the table. The shared
#       jmv_backend_* helpers (weights, export, theme, render) hold the blocks that were identical
#       across the two backends, so a fix lands once.
# KEY CONSTRAINTS:
#   - A module gets NO native file picker, so a typed FOLDER box plus a bare FILENAME box are the
#     only route (the format's extension is authoritative, never typed); resolveExportPath()
#     composes and sanitises them, and export_documents_dir() resolves the per-OS default folder.
#   - ⚠ In jamovi's bundled R, path.expand("~") resolves to Documents rather than the real home --
#     expand through the OS home (USERPROFILE / HOME) instead.
#   - `fs` is Suggests: every use carries a base-R fallback, so export never hard-depends on it.
#   - ⚠ A SHARED HELPER MAY ONLY READ AN OPTION BOTH PANELS DECLARE, and the failure is not a NULL:
#     jmvcore's `$.Options` STOPS on an unknown name. Anything panel-specific goes through jmv_opt().
#   - ⚠ THE RESULTS PANEL SIZES ITSELF FROM THE Html ELEMENT, which jamovi pins at width:500px -- so
#     everything the two backends show goes through ONE boundary, jmv_results_content(), which
#     un-pins it and decides what may drive that width (the table, never the prose).
#   - ⚠ THE EXPORT STATUS OUTLIVES THE CLICK. jamovi's own JS resets the Export action ~2 s after it
#     fires so a second click can re-fire, and that reset is a real option change: the run it
#     triggers has `exportExcel = FALSE`. The note therefore rides in the `$state` carrier each
#     analysis already has (jmv_export_remember / jmv_export_recall) and is re-emitted until the next
#     export replaces it.
# See: CLAUDE.md § tabxplor architecture (jamovi) ; dev/jamovi_module.md, whose section 7 quotes
#      the whole CSS chain out of jamovi's own bundle.

#' @noRd
export_home_dir <- function() {
  h <- Sys.getenv("USERPROFILE")                           # Windows, where jamovi mostly runs
  if (!nzchar(h)) h <- Sys.getenv("HOME")
  if (!nzchar(h)) h <- path.expand("~")
  h
}

#' @noRd
export_expand_winenv <- function(p) {
  toks <- regmatches(p, gregexpr("%[^%]+%", p))[[1]]
  for (t in unique(toks)) {
    v <- Sys.getenv(gsub("%", "", t, fixed = TRUE))
    if (nzchar(v)) p <- gsub(t, v, p, fixed = TRUE)
  }
  p
}

# The user's real Documents folder, per-OS and redirect-aware; falls back to a writable parent, then tempdir().
#' @noRd
export_documents_dir <- function() {
  tryCatch({
    home      <- export_home_dir()
    home_docs <- file.path(home, "Documents")
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

# WARNING: substring(), not sub() -- a Windows USERPROFILE's backslashes would be read as regex
# backreferences by sub().
#' @noRd
export_expand_home <- function(p) if (grepl("^~", p)) paste0(export_home_dir(), substring(p, 2)) else p

#' @noRd
export_unwrap <- function(s) {
  s <- trimws(as.character(if (length(s)) s[1] else ""))
  wrap <- "[]'\"<>[(){}]"
  s <- sub(paste0("^", wrap, "+"), "", s)
  s <- sub(paste0(wrap, "+$"), "", s)
  trimws(s)
}

#' @noRd
export_sanitize_filename <- function(name) {
  name <- basename(export_unwrap(name))                    # drop any directory pasted into the name box
  name <- gsub('[/\\\\?<>:*|":[:cntrl:]]', "", name)       # OS-illegal characters
  name <- sub("[. ]+$", "", name)                          # trailing dots / spaces (invalid on Windows)
  sub("\\.[A-Za-z0-9]{1,5}$", "", trimws(name))            # drop any extension the user typed
}

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

#' @noRd
export_number_path <- function(path, replace = FALSE) {
  if (isTRUE(replace) || !file.exists(path)) return(path)
  stem <- tools::file_path_sans_ext(path)
  ext  <- tools::file_ext(path)
  dot  <- if (nzchar(ext)) paste0(".", ext) else ""
  i <- 0L
  repeat { i <- i + 1L; cand <- paste0(stem, i, dot); if (!file.exists(cand)) return(cand) }
}

# The export status, as a rounded box under the table. ONE hue per state, stated the way every other
# palette in the package is -- the hex beside the OKLCH coordinate it was picked at:
#   ok      ink oklch(0.52 0.14 148) #1a7f37 | ground L 0.96 C 0.025 #E7F7E9 | edge L 0.88 #BEE3C2
#   failed  ink oklch(0.54 0.19  27) #c62828 | ground L 0.96 C 0.025 #FFECE9 | edge L 0.88 #FDC9C3
# The ink is the one jamovi's own flat styling uses; the ground is the SAME hue lifted to a tint, so
# the pair carries the state without a second colour. Contrast ok/failed = 4.6 / 4.9, both over AA.
# Italic and selectable: the path is meant to be copied.
#' @noRd
export_status_html <- function(text, ok = TRUE, lead = if (isTRUE(ok)) "Saved to:" else "Export failed:") {
  esc <- function(s) {
    s <- gsub("&", "&amp;", s, fixed = TRUE)
    s <- gsub("<", "&lt;",  s, fixed = TRUE)
    gsub(">", "&gt;", s, fixed = TRUE)
  }
  ink    <- if (isTRUE(ok)) "#1a7f37" else "#c62828"
  ground <- if (isTRUE(ok)) "#E7F7E9" else "#FFECE9"
  edge   <- if (isTRUE(ok)) "#BEE3C2" else "#FDC9C3"
  jmv_results_note(
    paste0("<b>", esc(lead), "</b> <i>", esc(as.character(text)[1]), "</i>"),
    style = paste0("margin:10px 2px;padding:6px 10px;border-radius:6px;",
                   "border:1px solid ", edge, ";background-color:", ground, ";color:", ink, ";",
                   "font-style:normal;overflow-wrap:anywhere;user-select:text;"))
}

# THE EXPORT STATUS OUTLIVES THE CLICK. jamovi's own JS resets the Export action about 2 s after it
# fires, so a second click can re-fire; that reset is a real option change, and the run it triggers
# used to erase the line the moment it had been read. It is therefore kept in the `$state` carrier
# each analysis already has, and re-emitted on every run until the next export replaces it.
# ⚠ `store` may be NULL (a first run, or a staged regression), and then there is simply nothing to say.
#' @noRd
jmv_export_remember <- function(store, note) {
  if (is.null(store)) store <- list()
  store$export_note <- note
  store
}
#' @noRd
jmv_export_recall <- function(store) {
  n <- store$export_note
  if (is.null(n) || is.na(n[1]) || !nzchar(n[1])) "" else n[1]
}


# === Documents-folder detectors ============================================================
# Tried in order of reliability; each is guarded (tryCatch) and returns a clean path or NA when it
# does not apply.

#' @noRd
export_is_wsl <- function() {
  if (nzchar(Sys.getenv("WSL_DISTRO_NAME"))) return(TRUE)
  # WARNING: gate on file.exists() FIRST -- an error-only tryCatch around readLines() would let its
  # incomplete-final-line warning leak to the caller when the file is absent.
  if (!file.exists("/proc/version")) return(FALSE)
  pv <- tryCatch(readLines("/proc/version", n = 1L, warn = FALSE), error = function(e) character())
  length(pv) && grepl("microsoft|WSL", pv[1], ignore.case = TRUE)
}

#' @noRd
export_norm1 <- function(x) {
  x <- tryCatch(as.character(x), error = function(e) character())
  x <- x[!is.na(x) & nzchar(trimws(x))]
  if (length(x)) trimws(x[1]) else NA_character_
}

#' @noRd
export_wsl_to_unix <- function(p) {
  if (is.na(p) || !export_is_wsl() || !grepl("^[A-Za-z]:[\\\\/]", p)) return(p)
  wp <- Sys.which("wslpath"); if (!nzchar(wp)) return(p)
  q <- tryCatch(export_norm1(suppressWarnings(
    system2(wp, c("-u", shQuote(p)), stdout = TRUE, stderr = FALSE))), error = function(e) NA_character_)
  if (is.na(q)) p else q
}

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

#' @noRd
doc_xdg <- function() tryCatch({
  x <- Sys.which("xdg-user-dir"); if (!nzchar(x)) return(NA_character_)
  export_norm1(suppressWarnings(system2(x, "DOCUMENTS", stdout = TRUE, stderr = FALSE)))
}, error = function(e) NA_character_)

#' @noRd
doc_xdg_file <- function() tryCatch({
  f <- file.path(export_home_dir(), ".config", "user-dirs.dirs")
  if (!file.exists(f)) return(NA_character_)
  ln <- grep("^[ \t]*XDG_DOCUMENTS_DIR", readLines(f, warn = FALSE), value = TRUE)
  if (!length(ln)) return(NA_character_)
  val <- gsub("\"", "", trimws(sub("^[^=]*=", "", ln[1])))
  export_norm1(gsub("$HOME", export_home_dir(), val, fixed = TRUE))
}, error = function(e) NA_character_)

#' @noRd
doc_home_documents <- function() export_norm1(file.path(export_home_dir(), "Documents"))

# Deliberately non-destructive: checks writability without creating anything (file.access mode 2).
#' @noRd
export_writable <- function(dir) {
  if (length(dir) != 1L || is.na(dir) || !nzchar(dir)) return(FALSE)
  isTRUE(dir.exists(dir) && file.access(dir, mode = 2L) == 0L)
}

#' @noRd
tab_html_string <- function(tabs, wrap_rows = 35, wrap_cols = 15, standalone = TRUE, ...) {
  k    <- tab_html(tabs, wrap_rows = wrap_rows,
                   wrap_cols = wrap_cols, ...)
  body <- as.character(k)
  if (!standalone) return(body)
  theme <- attr(k, "tabxplor_theme")
  page  <- if (is.null(theme)) "" else paste0("<style>\n", tx_page_style(theme), "\n</style>\n")
  paste0("<!DOCTYPE html>\n<html>\n<head>\n<meta charset=\"utf-8\"/>\n", page,
         "</head>\n<body>\n", body, "\n</body>\n</html>\n")
}

#' @noRd
jmvtab_export <- function(tabs, format = c("excel", "html", "md"), path, replace = FALSE,
                          check = FALSE, data = NULL, theme = NULL, ...) {
  format <- match.arg(format)

  if (format == "excel") tx_need_pkg("openxlsx2", "Excel export")

  dir <- dirname(path)
  if (nzchar(dir) && !dir.exists(dir)) {
    created <- tryCatch({
      dir.create(dir, recursive = TRUE, showWarnings = FALSE)
      dir.exists(dir)
    }, error = function(e) FALSE, warning = function(w) FALSE)
    if (!created) {
      cli::cli_abort(c("Cannot create the folder {.file {dir}}.",
                       "i" = "Choose a folder that exists, or one you are allowed to write in."))
    }
  }

  path <- export_number_path(path, replace)

  # WARNING: left unwrapped -- a low-level failure keeps its full rlang cause chain, which the caller
  # surfaces via conditionMessage(), not the bare `err$message` top wrapper.
  switch(
    format,
    excel = tab_xl(tabs, path = path, sheets = "unique", open = FALSE, replace = TRUE,
                   check = check, data = data, theme = theme),
    html  = writeLines(tab_html_string(tabs, theme = theme, ...), path),
    md    = tab_md(tabs, file = path, print = FALSE, theme = theme)
  )
  invisible(path)          # the path REALLY written (auto-numbered), for the caller's status message
}


# === Shared jamovi backend helpers ==========================================================

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

# This is the route: every panel-specific option read from a shared jmv_backend_* helper goes
# through here, never through `$` directly.
#' @noRd
jmv_opt <- function(self, name, default = NULL) {
  has <- tryCatch(isTRUE(self$options$has(name)), error = function(e) FALSE)
  if (has) self$options[[name]] else default
}

#' @noRd
jmv_backend_export <- function(self, tabs) {
  if (!isTRUE(self$options$exportExcel)) return("")
  fmt <- self$options$export_format
  ext <- switch(fmt, "excel" = "xlsx", "html" = "html", "md" = "md", "xlsx")
  p   <- resolveExportPath(self$options$export_dir, self$options$export_filename, ext)
  chk <- if (isTRUE(jmv_opt(self, "xl_check"))) "auto" else FALSE
  # WARNING: a string the jamovi USER reads goes through jmvcore's `.()`, which resolves against the
  # module's own catalogue keyed on jamovi's UI language -- plain gettext() follows the R engine's
  # locale instead. `.()` reads `self` out of its caller's frame, so only a function with one may call it.
  ok_lead <- jmvcore::.("Saved to:")
  ko_lead <- jmvcore::.("Export failed:")
  tryCatch({
    actual <- jmvtab_export(tabs, format = fmt, path = p, replace = self$options$xl_replace,
                            check = chk, data = if (isFALSE(chk)) NULL else self$data,
                            theme = jmv_backend_theme(self))
    export_status_html(actual, ok = TRUE, lead = ok_lead)
  }, error = function(err) {
    export_status_html(conditionMessage(err), ok = FALSE, lead = ko_lead)
  })
}

# WARNING: read HERE, not in `.opts()` -- a theme applies at RENDER, and `.opts()` is the cache
# key's complement, so putting it there would rebuild the whole table on a palette flip. Named
# `tab_theme`, not `theme`, because jamovi injects its own global `theme` option that would shadow it.
#' @noRd
jmv_backend_theme <- function(self) self$options$tab_theme %||% "light"

# DESIGN: tooltips are on by default -- they ride the native `title=` attribute, which needs no
# bootstrap JS and so works in jamovi's results webview. Popovers stay off: their content lives in
# `data-content`, dead without that JS.
#' @noRd
jmv_backend_render_html <- function(self, tabs) {
  tab_html(
    tabs,
    wrap_rows = self$options$wrap_rows,
    wrap_cols = self$options$wrap_cols,
    theme     = jmv_backend_theme(self)
  )
}


# === SECTION: the jamovi results iframe ===========================================================
# WARNING: jamovi sizes an analysis from its results iframe's reported width, but jamovi's own
# stylesheet pins an Html result at `width:500px`, so the table's real width never reached the host.
# Un-pinning to `width:max-content` restores that intent and lets the box hug the table in one pass;
# and prose must NOT drive the width -- hence `tx-note` on every non-table fragment. Full CSS chain:
# dev/jamovi_module.md s7.
#
# THE SCROLL BOX ITSELF IS NOT JAMOVI'S. tab_html() wraps every table in a `.tx-scrollbox` and
# tab_css() gives it its shape, for jamovi as for a document, a pkgdown site and the Viewer. What is
# jamovi's, and all that is left here, is the CAP: a document box stops at the space it has
# (`max-width:100%`), and jamovi has no such space to read -- the panel is sized FROM the table.
# Nothing else may be restated below, or the two would drift.

# Runaway guard only: no table is meant to reach it.
JMV_RESULTS_MAX_WIDTH <- 4000L

#' @noRd
jmv_results_style <- function(max_width = JMV_RESULTS_MAX_WIDTH) {
  paste0(
    "<style>",
    ".jmv-results-html{width:max-content;}",
    ".tx-scrollbox{max-width:", max_width, "px;}",
    ".tx-note{max-width:520px;}",
    "</style>"
  )
}

# The shape every non-table fragment takes: `tx-note` keeps its prose from sizing the panel.
#' @noRd
jmv_results_note <- function(inner, style = NULL) {
  paste0('<div class="tx-note"',
         if (!is.null(style) && nzchar(style)) paste0(' style="', style, '"') else "",
         '>', inner, '</div>')
}

# THE content boundary: emits the style once, then the fragments in order; NULL / "" fragments drop out.
#' @noRd
jmv_results_content <- function(...) {
  parts <- as.character(unlist(list(...), use.names = FALSE))
  parts <- parts[!is.na(parts) & nzchar(parts)]
  paste0(jmv_results_style(), paste0(parts, collapse = ""))
}
