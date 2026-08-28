# PURPOSE: one workbook of Excel edge cases, for a MANUAL width review (Phase 24c, section E).
# ROLE: the sizer (xl_col_widths) is measured arithmetic -- what it cannot check is whether a column
#   reads well. This writes the cases that broke it, so the maintainer can judge them in Excel.
# KEY CONSTRAINTS:
#   - dev-only (.Rbuildignore'd). Never sourced by the package or by tests.
#   - one SHEET per case, named for what it stresses; the console report states, per fmt column,
#     the widest string, whether it is bold, and the slack the width leaves it.
# See: CLAUDE.md § Phase 24c > E.

suppressMessages(devtools::load_all("~/github/tabxplor", quiet = TRUE))

out <- "~/tabxplor_xl_width_review.xlsx"

g <- forcats::gss_cat
g$income25k <- forcats::fct_lump_n(g$rincome, 3)
g$party3    <- forcats::fct_lump_n(g$partyid, 3)
g$married   <- forcats::fct_lump_n(g$marital, 3)
d_reg <- dplyr::slice_sample(g, n = 3000)

cases <- list(
  # the NA / Total / n trio, bold totals, two row_vars + tab_vars (and the section F borders)
  "na_total_n"   = tab(g, c(income25k, married), party3, race, pct = "row", na = "keep"),
  # the same, in the publication palette: marks switch the figures to the monospace stack
  "print_marks"  = tab(g, income25k, party3, race, pct = "row", color = TRUE,
                       color_signif = "grey_non_signif"),
  # six-figure levels (the digits rule) + the Model fit footer
  "big_means"    = tab_reg(car_salaries, salary, c(sex, rank, yrs.service), color = FALSE),
  # the <sup%> observed column + the shape table
  "ordinal_sup"  = suppressWarnings(tab_reg(d_reg, rincome, c(age, race),
                                            family = "ordinal", measure = "ratio", color = FALSE)),
  # a mean under 1 at two decimals, in a column whose name is one character
  "means_tiny"   = tab(dplyr::mutate(g, x = tvhours / 1000), income25k, x, digits = 2),
  # the other end: a very long column name over short figures
  "long_names"   = tab(dplyr::rename(g, a_very_long_column_name_indeed = party3),
                       income25k, a_very_long_column_name_indeed, pct = "row"),
  # stars on / off: the ratio gate that was deleted
  "stars_on"     = tab(g, income25k, party3, pct = "row", color = TRUE),
  "stars_off"    = tab(g, income25k, party3, pct = "row", color = FALSE, stars = FALSE)
)

# THE REPORT: per figure column, the widest string Excel will show against the width written for it.
# ⚠ MEASURED ON THE MATERIALIZED TABLE, never on the source: a composite cell becomes SEVERAL Excel
# columns (mat_aside_cols), so "(101 002)  0" is two columns and measuring it as one says nothing.
# A base digit is ~7px in the workbook's own width unit and ~8px in the number font (XL_NUM_RATIO),
# and a bold cell ~12 % more, so `slack` under 0 is a column Excel will print as "#####".
report <- function(tab_obj, nm) {        # imap() passes (value, name), in that order
  f <- withr::local_tempfile(fileext = ".xlsx")
  suppressMessages(tab_xl(tab_obj, path = f, replace = TRUE, open = FALSE))
  wb  <- openxlsx2::wb_load(f)
  # ⚠ `cols_attr` is one entry per RANGE (min..max), not per column: expand it before indexing.
  ca  <- wb$worksheets[[1]]$cols_attr
  num <- function(a, x) as.numeric(sub(paste0('.*', a, '="([0-9.]+)".*'), "\\1", x))
  w   <- rep(NA_real_, max(0, num("max", ca), na.rm = TRUE))
  for (k in seq_along(ca)) w[num("min", ca[[k]]):num("max", ca[[k]])] <- num("width", ca[[k]])
  # `tables` is one render model per table stacked on the sheet; the first is enough for a report.
  rd   <- tabxplor:::tab_export_prep(tab_obj, backend = "xl")$tables[[1]]
  mt   <- rd$tab
  bold <- seq_len(nrow(mt)) %in% (rd$bold_rows %||% integer(0))
  rows <- purrr::map(which(purrr::map_lgl(mt, is_fmt)), function(j) {
    body <- format(mt[[j]], special_formatting = FALSE, na = "", stars = TRUE)
    px   <- nchar(body) * 8 * ifelse(bold[seq_along(body)], 1.12, 1)
    k    <- which.max(px)
    if (!length(k)) return(NULL)
    wj <- if (j <= length(w)) w[[j]] else NA_real_
    tibble::tibble(case = nm, col = names(mt)[[j]], widest = body[[k]],
                   bold = bold[[k]], width = round(wj, 2),
                   need_px = round(px[[k]]), have_px = round(wj * 7),
                   slack = round(wj * 7 - px[[k]]))
  })
  purrr::list_rbind(purrr::compact(rows))
}

rep_all <- purrr::list_rbind(purrr::imap(cases, report))
print(rep_all, n = Inf)
cat("\ntightest columns:\n"); print(head(dplyr::arrange(rep_all, slack), 10))

# one workbook, one sheet per case
tab_xl(unname(cases), path = out, replace = TRUE, open = FALSE, sheets = "tabs",
       titles = names(cases))
cat("\nwritten: ", path.expand(out), "\n")
