# PURPOSE: ARCHIVED backend for the retired `jmvtest` diagnostic analysis (Export folder test; Last
#          Phase o). It ran live in jamovi on Windows / WSL / macOS to find which Documents-folder
#          detection method lands where; those results drove the export_documents_dir() rewrite (in
#          R/jmvtab-export.R). The analysis is no longer built into the module -- it lives here for
#          reference / resurrection only.
# ROLE: A THROWAWAY probe UI: four HTML panels (Environment / Documents-detection / Fallbacks / Write
#       results) + two buttons that PERSIST a plain .md into every candidate folder so the maintainer
#       can see which one is the real Documents. Writes .md only (no Excel) to isolate the folder problem.
# TO RESURRECT: move this file + jmvtest.a.yaml/.r.yaml/.u.yaml/.js (and drop jmvtest.h.R -- it is
#       regenerated) back to R/ and jamovi/ (js -> jamovi/js/), re-add the analysis to jamovi/0000.yaml,
#       then `jmvtools::prepare()` + rebuild. The R6 class references the Documents detectors that STAY
#       in R/jmvtab-export.R (doc_win_reg_shell / doc_xdg / doc_home_documents / export_writable /
#       export_home_dir / export_norm1 / export_wsl_to_unix / export_is_wsl / export_expand_home /
#       export_unwrap / export_sanitize_filename / resolveExportPath / jmv_backend_notice); the
#       diagnostic-only helpers it also needs are defined below (they were removed from the package when
#       the analysis was retired, so they travel with it).
# See: CLAUDE.md > 1.4.0 roadmap > Last Phase o ; dev/tabxplor_1.4.0_jamovi_dev.md § Phase o.

# === Diagnostic-only helpers (removed from R/jmvtab-export.R with the analysis) ============

# PowerShell [Environment]::GetFolderPath('MyDocuments') -- the redirection-aware known-folder API.
# The live test proved powershell.exe is NOT on the bundled R's PATH, so the FIX uses the registry
# instead; kept here only so the diagnostic still shows the method as "(empty / not found)".
doc_win_powershell <- function() tryCatch({
  ps <- Sys.which("powershell.exe"); if (!nzchar(ps)) ps <- Sys.which("pwsh")
  if (!nzchar(ps)) return(NA_character_)
  out <- suppressWarnings(system2(
    ps, c("-NoProfile", "-NonInteractive", "-Command",
          shQuote("[Environment]::GetFolderPath('MyDocuments')")),
    stdout = TRUE, stderr = FALSE))
  export_wsl_to_unix(export_norm1(out))
}, error = function(e) NA_character_)

# OneDrive Known-Folder-Move: the OneDrive* env root + \Documents (common on Windows 11).
doc_win_onedrive <- function() {
  for (v in c("OneDrive", "OneDriveConsumer", "OneDriveCommercial")) {
    od <- Sys.getenv(v)
    if (nzchar(od)) return(export_wsl_to_unix(export_norm1(file.path(od, "Documents"))))
  }
  NA_character_
}

# WSL -> the Windows USERPROFILE via cmd.exe, wslpath-converted, + \Documents (only when /mnt is
# visible inside the distro / flatpak).
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

# Fallback save locations (tempdir is the universal safety net).
fb_home      <- function() export_norm1(export_home_dir())
fb_desktop   <- function() export_norm1(file.path(export_home_dir(), "Desktop"))
fb_downloads <- function() export_norm1(file.path(export_home_dir(), "Downloads"))
fb_cwd       <- function() export_norm1(getwd())
fb_tempdir   <- function() export_norm1(tempdir())

# Actually write a plain .md test file into `dir` (creating the folder if needed). Returns
# list(ok, path, error). PERSISTS the file on purpose -- the maintainer finds it in the file manager.
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
    suppressWarnings(writeLines(enc2utf8(body), path, useBytes = TRUE))
    list(ok = TRUE, path = path, error = NA_character_)
  }, error = function(e) list(ok = FALSE, path = path, error = conditionMessage(e)))
}

# One row per method: method label, resolved dir, exists?, writable?. `builders` is a named list of
# 0-arg detectors; append the `current` row (what resolveExportPath() produces for the default).
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

# The Documents-detection candidate table (all methods + the current-behaviour row).
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

# The fallback save-location candidate table.
export_fallback_candidates <- function() export_candidate_df(list(
  "home"           = fb_home,
  "home/Desktop"   = fb_desktop,
  "home/Downloads" = fb_downloads,
  "getwd()"        = fb_cwd,
  "tempdir()"      = fb_tempdir
))

# The environment facts for the diagnostic's first panel (a named character vector).
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

# Render an env-probe named vector OR a candidate data.frame to a simple HTML block for a panel.
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

# === The R6 backend =======================================================================

# @rdname jamovi
jmvtestClass <- if (requireNamespace('jmvcore', quietly = TRUE)) R6::R6Class(
  "jmvtestClass",
  inherit = jmvtestBase,
  private = list(

    .run = function() {
      # Read-only panels (never litter -- export_writable() probes with file.access, not a write).
      self$results$environment$setContent(export_probe_html(export_env_probe(), "Environment"))
      docs <- export_doc_candidates()
      self$results$documents$setContent(paste0(
        private$.recommend_html(docs),
        export_probe_html(docs, "Documents-detection methods")))
      self$results$fallbacks$setContent(
        export_probe_html(export_fallback_candidates(), "Fallback save locations"))

      # The buttons are the experiment: they PERSIST files so the maintainer can find them.
      if (isTRUE(self$options$write_detected)) private$.write_detected(docs)
      else if (isTRUE(self$options$write_all)) private$.write_all(docs)
    },

    # A banner naming the first existing+writable Documents candidate (the fix recommendation).
    .recommend_html = function(docs) {
      ok <- which(docs$writable & !startsWith(docs$method, "CURRENT"))
      msg <- if (length(ok))
        paste0("<b>Recommended:</b> ", docs$method[ok[1]], " &rarr; ", docs$dir[ok[1]])
      else
        "<b>Recommended:</b> no Documents method yielded a writable folder &mdash; use a fallback."
      paste0("<p style='margin:4px 0'>", msg, "</p>")
    },

    # First writable Documents candidate, else first writable fallback, else tempdir().
    .best_dir = function(docs) {
      ok <- which(docs$writable & !startsWith(docs$method, "CURRENT"))
      if (length(ok)) return(list(dir = docs$dir[ok[1]], label = docs$method[ok[1]]))
      fb  <- export_fallback_candidates(); okf <- which(fb$writable)
      if (length(okf)) return(list(dir = fb$dir[okf[1]], label = fb$method[okf[1]]))
      list(dir = tempdir(), label = "tempdir()")
    },

    # Write ONE file to the best-detected Documents folder; report the resolved path.
    .write_detected = function(docs) {
      pick <- private$.best_dir(docs)
      r <- export_write_test(pick$dir, self$options$test_name, note = paste0("method: ", pick$label))
      df <- data.frame(method = pick$label,
                       path = if (r$ok) r$path else "",
                       result = if (r$ok) "OK" else paste0("FAILED: ", r$error),
                       stringsAsFactors = FALSE)
      self$results$write_results$setContent(export_probe_html(df, "Write result"))
      jmv_backend_notice(self,
        if (r$ok) paste0("Saved to: ", r$path) else paste0("Export failed: ", r$error), ok = r$ok)
    },

    # Write a distinct file into EVERY candidate folder (detection + fallback + optional custom),
    # de-duplicated by resolved dir. The maintainer then reports which file is in their real Documents.
    .write_all = function(docs) {
      cand <- rbind(
        docs[!startsWith(docs$method, "CURRENT"), c("method", "dir")],
        export_fallback_candidates()[, c("method", "dir")]
      )
      custom <- export_expand_home(export_unwrap(self$options$test_dir))
      if (nzchar(custom))
        cand <- rbind(data.frame(method = "custom test_dir", dir = custom,
                                 stringsAsFactors = FALSE), cand)
      cand <- cand[nzchar(cand$dir) & !duplicated(cand$dir), , drop = FALSE]

      rows <- lapply(seq_len(nrow(cand)), function(i) {
        tag <- gsub("(^_|_$)", "", gsub("[^A-Za-z0-9]+", "_", cand$method[i]))
        r <- export_write_test(cand$dir[i], paste0(self$options$test_name, "_", tag),
                               note = paste0("method: ", cand$method[i]))
        data.frame(method = cand$method[i],
                   path = if (r$ok) r$path else "",
                   result = if (r$ok) "OK" else paste0("FAILED: ", r$error),
                   stringsAsFactors = FALSE)
      })
      df <- do.call(rbind, rows)
      self$results$write_results$setContent(
        export_probe_html(df, "Write results (open these in your file manager)"))
      n_ok <- sum(df$result == "OK")
      jmv_backend_notice(self, paste0(
        "Wrote ", n_ok, " of ", nrow(df), " test file(s). Open your file manager and tell us which ",
        "one is in your real Documents folder (each file names the method that wrote it)."),
        ok = n_ok > 0)
    }
  )
)
