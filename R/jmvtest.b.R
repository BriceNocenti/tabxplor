# PURPOSE: Jamovi diagnostic backend for the `jmvtest` analysis (Export folder test; Last Phase o).
# ROLE: A THROWAWAY probe UI. It reports, for real Windows / WSL / macOS machines, which of many
#       Documents-folder detection methods yields a writable path, and (on a button) writes a plain
#       .md test file into each candidate so the maintainer can see -- in the file manager -- which
#       one landed in their real Documents. The winning method then folds into export_documents_dir()
#       and this whole analysis is removed.
# KEY CONSTRAINTS:
#   - jmvtest.h.R is GENERATED from jmvtest.a.yaml (jmvtools::prepare()); never hand-edit it.
#   - The detector helpers live in R/jmvtab-export.R (they STAY -- they are the seed of the fix);
#     this backend is pure display glue and gets removed with jmvtest.
#   - Writes ONLY plain .md via export_write_test() (no Excel / tab_xl) -- isolates the folder problem
#     from Phase n's Excel-serialization bug.
# See: CLAUDE.md > 1.4.0 roadmap > Last Phase o.

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
