# PURPOSE: Phase 12 (roadmap "Manual reviews") — build two MIRROR Excel workbooks, one from CRAN
#          tabxplor 1.3.1 and one from the 2.0.0 dev source, with IDENTICAL sheet names, so the
#          maintainer can eyeball statistical results side-by-side and confirm 2.0.0 reproduces the
#          published 1.3.1 numbers (and see the *intended* changes: numeric diff -> real difference,
#          the new `ratio` mode, add_n in-cell, display-only p-value rows, ...).
#
# ROLE: dev-only review harness (NOT a package test). Uses the maintainer's real weighted survey
#       `pc18`; per the maintainer, pc18 must NEVER enter tests/. Run it TWICE (see USAGE).
#
# USAGE (two isolated processes — 1.3.1 is installed, 2.0.0 is the uninstalled dev source):
#   Rscript dev/manual_review_131_vs_140.R installed   # -> dev/review_manual/review_tabxplor_1.3.1.xlsx
#   Rscript dev/manual_review_131_vs_140.R dev          # -> dev/review_manual/review_tabxplor_2.0.0-dev.xlsx
#
# KEY CONSTRAINTS / cross-version mechanics (all probed, 2026-07-13):
#   - 1.3.1 `tab()` takes ONE col_var (ensym); the merged factor+numeric table there needs
#     `tab_many(..., compact = TRUE)`. 2.0.0 `tab()` merges several col_vars natively. -> `build()`.
#   - Version is detected by FEATURE (`color_signif` in formals(tab)), because the dev DESCRIPTION
#     still reads 1.3.1(.9000).
#   - `color_signif` (2.0.0) folds into the old `color` string for 1.3.1:
#         diff + ignore           -> "diff"
#         diff + grey_non_signif  -> "diff_ci"
#         diff + color_all_signif -> "after_ci"
#         ratio                   -> "diff"   (1.3.1 numeric "diff" IS the multiplicative ratio)
#   - 1.3.1 default `method_diff = "ac"` (Agresti-Caffo); 2.0.0 default is "newcombe" -> the greying
#     cases pass method_diff = "ac" so both sides invert the SAME interval.
#   - The "field" table (2nd per sheet) = the SAME table with `set_display()` switched to the relevant
#     vctrs field (diff / ratio / ctr / ci). set_display + integer `sheets=` (one case = one sheet,
#     two stacked tables) both work in BOTH versions.
#   - pc18 weight-grouping rule (maintainer): each variable group is valid ONLY with its own weight
#     (POND / POND5 / POND2). We pre-filter to that weight's non-NA rows to isolate the group; only
#     the POND2 group holds a numeric col_var (nb_livres), so the roadmap's "col_vars = c(factor,
#     numeric)" tables all live there.

suppressWarnings(suppressMessages({
  # --- pick the package source -------------------------------------------------------------------
  MODE <- commandArgs(TRUE)[1]
  if (is.na(MODE) || !nzchar(MODE)) {
    stop("Specify the source: `Rscript dev/manual_review_131_vs_140.R installed|dev`", call. = FALSE)
  }
  MODE <- match.arg(MODE, c("installed", "dev"))
  PKG_ROOT <- "d:/Statistiques/github/tabxplor"
  if (MODE == "dev") devtools::load_all(PKG_ROOT, quiet = TRUE) else library(tabxplor)
}))

# reliable 2.0.0 discriminator (dev version string is still 1.3.1.9000)
IS_DEV  <- "color_signif" %in% names(formals(tab))
VER_LAB <- if (IS_DEV) "2.0.0-dev" else as.character(utils::packageVersion("tabxplor"))
cat(sprintf("\n== tabxplor %s  (mode=%s, is_dev=%s) ==\n", VER_LAB, MODE, IS_DEV))

is_fmt      <- getFromNamespace("is_fmt", "tabxplor")
set_display <- getFromNamespace("set_display", "tabxplor")

# =================================================================================================
# 1. Data: rebuild pc18 exactly per the maintainer's recipe, then isolate each weight-group.
# =================================================================================================
PC18_DIR <- "~/github/formations_stat"   # ~ resolves to D:/Statistiques on this machine
f1 <- path.expand(file.path(PC18_DIR, "M1S1_04_pc18.rds"))
f2 <- path.expand(file.path(PC18_DIR, "pc18_M1S2_03.rds"))
if (!file.exists(f1) || !file.exists(f2)) {
  stop("pc18 source files not found. Edit PC18_DIR. Looked for:\n  ", f1, "\n  ", f2, call. = FALSE)
}
pc18 <- dplyr::bind_rows(readRDS(f1), dplyr::rename(readRDS(f2), POND2 = POND))
cat(sprintf("pc18 rebuilt: %d rows x %d cols\n", nrow(pc18), ncol(pc18)))

# Variable configs. `wt` is BOTH the group's weight AND the column we filter non-NA on to isolate
# the group's respondents (each weight is non-NA only on its own file's rows). Edit freely.
CFG <- list(
  # POND2: the ONLY group with a numeric col_var -> factor THEATRE + numeric nb_livres
  num   = list(rv = "NATIOM",     cv = c("THEATRE", "nb_livres"), tv = "JV",     wt = "POND2"),
  # POND: rich factor-only exploratory table
  fac   = list(rv = "DIPLOM",     cv = c("CONCERTS", "THEATRE4"), tv = "CSTOTR", wt = "POND"),
  # POND: the maintainer's literal tab_vars group (factor-only)
  tabv  = list(rv = "CRITAGE",    cv = "RESEAUX",                 tv = "CSTOTR", wt = "POND"),
  # POND5: many col_vars with levels = "first"
  music = list(rv = "CRITREVENU2",
               cv = c("ROCK","JAZZ","CLASSIQUE","VARIETE","ELECTRO","METAL","CHANSON","WORLD","RAP","TRADI"),
               tv = NULL, wt = "POND5")
)

# =================================================================================================
# 2. Version-aware builders.
# =================================================================================================
# Fold the 2.0.0 (color, color_signif) pair into the 1.3.1 `color` string.
color_131 <- function(color, color_signif) {
  if (is.null(color)) return("no")
  if (color == "ratio") return("diff")
  if (color == "diff")
    return(switch(color_signif %||% "ignore",
                  ignore = "diff", grey_non_signif = "diff_ci", color_all_signif = "after_ci"))
  color                                  # "no", "contrib", (unused) ci-modes
}

# Build one merged table. `sc` = scalar tab() args in 2.0.0 vocabulary; translated for 1.3.1.
build <- function(cfg, sc, use_tab = FALSE, use_wt = FALSE) {
  data <- dplyr::filter(pc18, !is.na(.data[[cfg$wt]]))     # isolate this weight-group's rows
  s <- sc
  if (!IS_DEV) {                                           # 1.3.1: no color_signif / stars
    s$color        <- color_131(sc$color, sc$color_signif)
    s$color_signif <- NULL
    s$stars        <- NULL
  }
  s <- s[!vapply(s, is.null, logical(1))]
  a <- list(data = data,
            row_vars = rlang::expr(dplyr::all_of(!!cfg$rv)),
            col_vars = rlang::expr(dplyr::all_of(!!cfg$cv)))
  if (use_tab && !is.null(cfg$tv)) a$tab_vars <- rlang::expr(dplyr::all_of(!!cfg$tv))
  if (use_wt) a$wt <- rlang::sym(cfg$wt)
  a <- c(a, s)
  if (IS_DEV) rlang::inject(tab(!!!a)) else rlang::inject(tab_many(!!!a, compact = TRUE))
}

# Second table: same numbers, displaying the relevant vctrs field.
show_field <- function(tabs, token) {
  suppressWarnings(
    dplyr::mutate(tabs, dplyr::across(tidyselect::where(is_fmt), ~ set_display(.x, token)))
  )
}

# =================================================================================================
# 3. Review cases. `field` may be a single token (both versions) or c(dev=, ref=) when they differ.
#    sc uses 2.0.0 names; `cfg` names a CFG entry.
# =================================================================================================
mk <- function(key, cfg, sc, tab = FALSE, wt = FALSE, field = "diff", desc = key)
  list(key = key, cfg = cfg, sc = sc, tab = tab, wt = wt, field = field, desc = desc)

CASES <- list(
  mk("01 base_diff",    "num",  list(pct = "row", color = "diff"),
     field = "diff", desc = "pct=row, color=diff (numeric diff CHANGED in 2.0.0)"),
  mk("02 tabv_diff",    "num",  list(pct = "row", color = "diff"), tab = TRUE, wt = TRUE,
     field = "diff", desc = "tab_vars, wt, pct=row, color=diff"),
  mk("03 tabv_ratio",   "num",  list(pct = "row", color = "ratio"), tab = TRUE, wt = TRUE,
     field = c(dev = "ratio", ref = "diff"),
     desc = "tab_vars, wt, pct=row, color=ratio (2.0.0) vs former diff (1.3.1) — numeric col should MATCH"),
  mk("04 tabv_coldiff", "num",  list(pct = "col", color = "diff"), tab = TRUE, wt = TRUE,
     field = "diff", desc = "tab_vars, wt, pct=col, color=diff"),
  mk("05 tabv_contrib", "num",  list(pct = "row", color = "contrib", comp = "all", chi2 = TRUE),
     tab = TRUE, wt = TRUE, field = "ctr", desc = "tab_vars, wt, color=contrib, comp=all, chi2"),
  mk("06 grey_row",     "num",  list(pct = "row", color = "diff",
                                     color_signif = "grey_non_signif", method_diff = "ac"),
     field = "ci", desc = "pct=row, color=diff, grey_non_signif (CI method Agresti-Caffo)"),
  mk("07 grey_row_wt",  "num",  list(pct = "row", color = "diff",
                                     color_signif = "grey_non_signif", method_diff = "ac"),
     wt = TRUE, field = "ci", desc = "wt, pct=row, color=diff, grey_non_signif (AC)"),
  mk("08 grey_col",     "num",  list(pct = "col", color = "diff",
                                     color_signif = "grey_non_signif", method_diff = "ac"),
     field = "ci", desc = "pct=col, color=diff, grey_non_signif (AC)"),
  mk("09 grey_col_wt",  "num",  list(pct = "col", color = "diff",
                                     color_signif = "grey_non_signif", method_diff = "ac"),
     wt = TRUE, field = "ci", desc = "wt, pct=col, color=diff, grey_non_signif (AC)"),
  mk("10 ci_cell",      "num",  list(pct = "row", ci = "cell", method_cell = "wilson"),
     field = "ci", desc = "pct=row, ci=cell (Wilson)"),
  mk("11 ci_cell_wt",   "num",  list(pct = "row", ci = "cell", method_cell = "wilson"),
     wt = TRUE, field = "ci", desc = "wt, pct=row, ci=cell (Wilson, weighted est + unweighted n)"),
  mk("12 allsig_row",   "num",  list(pct = "row", color = "diff",
                                     color_signif = "color_all_signif", method_diff = "ac"),
     field = "ci", desc = "pct=row, color=diff, color_all_signif (1.3.1 after_ci, AC)"),
  # --- supplementary: other weight-groups / shapes ---
  mk("13 fac_expl",     "fac",  list(pct = "row", color = "diff", chi2 = TRUE), wt = TRUE,
     field = "diff", desc = "POND exploratory: DIPLOM x (CONCERTS, THEATRE4), pct=row, diff, chi2"),
  mk("14 music_first",  "music", list(pct = "row", color = "diff", levels = "first"), wt = TRUE,
     field = "diff", desc = "POND5: CRITREVENU2 x 10 music genres, levels=first, pct=row, diff"),
  mk("15 tabv_user",    "tabv", list(pct = "row", color = "diff"), tab = TRUE, wt = TRUE,
     field = "diff", desc = "POND (maintainer's literal group): CRITAGE x RESEAUX by CSTOTR")
)

# =================================================================================================
# 4. Build every case -> a flat list of (standard, field) tabs, one integer sheet per case.
# =================================================================================================
field_for <- function(field) if (length(field) == 1) unname(field) else
  field[[if (IS_DEV) "dev" else "ref"]]

# A guaranteed-valid tiny placeholder keeps the two files' sheets ALIGNED if a case errors in one
# version only (the failure is surfaced in the sheet title, and the file stays fully formatted —
# tab_xl would otherwise downgrade the WHOLE workbook to plain if any list member is a bare frame).
placeholder <- build(CFG$num, list(pct = "row"))

tabs_out <- list(); titles <- character(); sheet_idx <- integer(); status <- character()
for (i in seq_along(CASES)) {
  cs  <- CASES[[i]]
  cfg <- CFG[[cs$cfg]]
  tok <- field_for(cs$field)
  res <- tryCatch({
    std <- build(cfg, cs$sc, use_tab = cs$tab, use_wt = cs$wt)
    fld <- show_field(std, tok)
    list(std = std, fld = fld, err = NA_character_)
  }, error = function(e) list(std = placeholder, fld = show_field(placeholder, "pct"),
                              err = conditionMessage(e)))
  ok  <- is.na(res$err)
  status[i] <- if (ok) "ok" else paste("FAILED:", res$err)
  cat(sprintf("  [%s] %-16s %s\n", if (ok) "ok " else "ERR", cs$key,
              if (ok) paste0("(", cs$cfg, ", field=", tok, ")") else res$err))
  suffix <- if (ok) "" else paste0("  <ERR: ", substr(res$err, 1, 40), ">")
  tabs_out <- c(tabs_out, list(res$std, res$fld))
  titles   <- c(titles, paste0(cs$key, " | std", suffix),
                        paste0(cs$key, " | field:", tok))
  sheet_idx <- c(sheet_idx, i, i)
}

# =================================================================================================
# 5. One workbook, one sheet per case (two stacked tables), matching sheet names across versions.
# =================================================================================================
out_dir <- file.path(PKG_ROOT, "dev", "review_manual")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
out_file <- file.path(out_dir, sprintf("review_tabxplor_%s", VER_LAB))

tab_xl(tabs_out, path = out_file, replace = TRUE, open = FALSE,
       sheets = as.integer(sheet_idx), titles = titles)

n_bad <- sum(!startsWith(status, "ok"))
cat(sprintf("\nWROTE %s.xlsx  (%d sheets; %d case(s) failed)\n",
            out_file, length(unique(sheet_idx)), n_bad))
if (n_bad) {
  bad <- which(!startsWith(status, "ok"))
  cat("Failed cases:\n",
      paste0("  - ", vapply(bad, function(j) CASES[[j]]$key, ""), ": ",
             sub("^FAILED: ", "", status[bad]), collapse = "\n"), "\n")
}
cat("Run the OTHER mode, then open both files in dev/review_manual/ side-by-side.\n")
