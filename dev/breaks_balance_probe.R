# PURPOSE: measure whether the diff / ratio / odds-ratio colour ladders are BALANCED --
#   i.e. whether the same real cell lands on the same intensity slot whichever measure is read.
# ROLE: evidence for dev/reg_family_measure_effect.md (S3, the ladders). Re-run after any
#   change to `color_breaks`; it needs no fixtures and prints a self-contained report.
# USAGE: OMP_NUM_THREADS=1 Rscript dev/breaks_balance_probe.R
suppressMessages(devtools::load_all(".", quiet = TRUE))
suppressMessages({library(dplyr); library(carData); library(questionr)})
options(width = 150, tabxplor.print = "console", tabxplor.lang = "en")

slot_of <- function(x, breaks, center) {            # 0 = neutral, 1..4 = intensity, sign = side
  s <- rep(0L, length(x)); ok <- is.finite(x)
  dev <- if (center == 1) ifelse(x[ok] >= 1, x[ok], 1 / x[ok]) else abs(x[ok] - center)
  s[ok] <- findInterval(dev, breaks) * ifelse(
    if (center == 1) x[ok] >= 1 else x[ok] >= center, 1L, -1L)
  s
}
B <- tx_option_default("color_breaks")

## ---------- the tables to probe -------------------------------------------------
gss <- gss_cat_data_formatting()
arr <- carData::Arrests |> as_tibble() |>
  mutate(released = forcats::fct_rev(released), colour = forcats::fct_relevel(colour, "White"))
data("hdv2003", package = "questionr")
hdv <- hdv2003 |> as_tibble() |> mutate(cinema = forcats::fct_rev(cinema))

tabs <- list(
  `gss race x party`   = tab(gss, race, party3, pct = "row", na = "drop"),
  `gss relig x married`= tab(gss, relig, married, pct = "row", na = "drop"),
  `gss rincome x party`= tab(gss, rincome, party3, pct = "row", na = "drop"),
  `arrests x released` = tab(arr, c(colour, sex, employed, citizen), released, pct = "row"),
  `hdv qualif x cinema`= tab(hdv, qualif, cinema, pct = "row", na = "drop")
)

cell <- purrr::imap_dfr(tabs, function(t, nm) {
  fc <- t[vapply(t, is_fmt, TRUE)]
  purrr::map_dfr(fc, function(col) tibble(
    tab = nm, diff = get_diff(col), ratio = get_ratio(col), or = get_or(col),
    tot = is_totrow(col)))
}) |> filter(!tot, is.finite(diff), is.finite(ratio), is.finite(or), ratio > 0)

cat("\n================ CROSSTABS:", nrow(cell), "data cells ================\n")
cell <- cell |> mutate(
  s_diff  = slot_of(diff,  B$pct_diff$over$breaks,   0),
  s_ratio = slot_of(ratio, B$pct_ratio$over$breaks,  1),
  s_or    = slot_of(or,    B$odds_ratio$over$breaks, 1))
show <- function(v, lbl) cat(sprintf("  %-22s %s\n", lbl,
  paste(sprintf("|%s|=%d: %4.1f%%", c(0:4), 0:4,
        100 * vapply(0:4, function(k) mean(abs(v) == k), 0)), collapse = "   ")))
cat("Share of cells at each intensity (0 = uncoloured):\n")
show(cell$s_diff,  "pct_diff  5/10/20/30");  show(cell$s_ratio, "pct_ratio 1.5/2/4")
show(cell$s_or,    "odds_ratio 1.2/1.5/2/4")
cat(sprintf("\n  cells coloured by diff but NOT by ratio : %4.1f%%\n",
            100*mean(cell$s_diff != 0 & cell$s_ratio == 0)))
cat(sprintf("  cells coloured by diff but NOT by OR    : %4.1f%%\n",
            100*mean(cell$s_diff != 0 & cell$s_or == 0)))
cat(sprintf("  mean |slot| -- diff %.2f | ratio %.2f | OR %.2f\n",
            mean(abs(cell$s_diff)), mean(abs(cell$s_ratio)), mean(abs(cell$s_or))))

cat("\n---- the SAME cells under a calibrated ratio ladder (1.1/1.2/1.4/1.6) ----\n")
show(slot_of(cell$ratio, c(1.1, 1.2, 1.4, 1.6), 1), "pct_ratio 1.1/1.2/1.4/1.6")

## ---------- regressions ---------------------------------------------------------
cat("\n================ REGRESSIONS ================\n")
regs <- list(
  `binomial OR`   = tab_reg(arr, "released", c("colour","sex","employed","citizen","checks")),
  `binomial mRR`  = tab_reg(arr, "released", c("colour","sex","employed","citizen","checks"),
                            effect = "marginal", measure = "ratio"),
  `hdv OR`        = tab_reg(hdv, "cinema", c("qualif","age")),
  `hdv mRR`       = tab_reg(hdv, "cinema", c("qualif","age"), effect = "marginal", measure = "ratio")
)
for (nm in names(regs)) {
  t  <- regs[[nm]]; fc <- t[vapply(t, is_fmt, TRUE)]
  v  <- unlist(lapply(fc, get_num)); v <- v[is.finite(v) & v > 0]
  v  <- v[abs(v - 1) > 1e-9]                       # drop the reference cells
  brk <- if (grepl("OR", nm) && !grepl("mRR", nm)) B$odds_ratio$over$breaks else B$pct_ratio$over$breaks
  cat(sprintf("\n  %-14s n=%3d  range %.2f - %.2f  ladder %s\n", nm, length(v), min(v), max(v),
              paste(brk, collapse = "/")))
  show(slot_of(v, brk, 1), "    as shipped")
  if (grepl("mRR", nm)) show(slot_of(v, c(1.1,1.2,1.4,1.6), 1), "    calibrated 1.1/1.2/1.4/1.6")
}

## ---------- candidate ladders, side by side -------------------------------------
cat("\n\n================ CANDIDATE RATIO LADDERS ================\n")
cands <- list(
  `shipped   1.5/2/4`       = c(1.5, 2, 4),
  `calibrated 1.1/1.2/1.4/1.6` = c(1.1, 1.2, 1.4, 1.6),
  `adj_ratio 1.1/1.25/1.5/2`   = c(1.1, 1.25, 1.5, 2),
  `mid       1.15/1.35/1.75/2.5` = c(1.15, 1.35, 1.75, 2.5),
  `odds-like 1.2/1.5/2/4`      = c(1.2, 1.5, 2, 4)
)
prof <- function(v, brk) {
  s <- slot_of(v, brk, 1)
  c(vapply(0:4, function(k) 100*mean(abs(s) == k), 0))
}
report <- function(v, lbl) {
  cat(sprintf("\n  -- %s (n = %d) --\n", lbl, length(v)))
  cat(sprintf("     %-30s %s\n", "ladder", paste(sprintf("|%d|", 0:4), collapse = "     ")))
  for (nm in names(cands))
    cat(sprintf("     %-30s %s\n", nm,
        paste(sprintf("%5.1f%%", prof(v, cands[[nm]])), collapse = " ")))
}
report(cell$ratio, "crosstab cells, ALL")
report(cell$ratio[cell$ratio >= 1], "crosstab cells, OVER side")
report(cell$ratio[cell$ratio <  1], "crosstab cells, UNDER side")
rr <- unlist(lapply(regs[["binomial mRR"]][vapply(regs[["binomial mRR"]], is_fmt, TRUE)], get_num))
rr <- rr[is.finite(rr) & rr > 0 & abs(rr - 1) > 1e-9]
report(rr, "regression marginal RR, common outcome (83%)")
rr2 <- unlist(lapply(regs[["hdv mRR"]][vapply(regs[["hdv mRR"]], is_fmt, TRUE)], get_num))
rr2 <- rr2[is.finite(rr2) & rr2 > 0 & abs(rr2 - 1) > 1e-9]
report(rr2, "regression marginal RR, strong class effects")
cat("\n  reference -- what pct_diff itself does on the same crosstab cells:\n")
cat(sprintf("     %-30s %s\n", "pct_diff 5/10/20/30",
    paste(sprintf("%5.1f%%", vapply(0:4, function(k) 100*mean(abs(cell$s_diff)==k), 0)), collapse=" ")))
