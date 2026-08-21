# PURPOSE: measure what the colour LADDERS actually do on real tables -- how much colour each grades,
#   how the two sides balance, and whether the same cell lands on the same intensity whichever measure
#   it is read on.
# ROLE: the standing evidence for dev/color_ladders_balance.md. Re-run after ANY change to
#   `color_breaks`; it needs no fixtures and prints a self-contained report.
# USAGE: OMP_NUM_THREADS=1 Rscript dev/breaks_balance_probe.R
suppressMessages(devtools::load_all(".", quiet = TRUE))
suppressMessages({library(dplyr); library(carData); library(questionr)})
options(width = 150, tabxplor.print = "console", tabxplor.lang = "en")

B <- tx_option_default("color_breaks")

# 0 = neutral, 1..4 = intensity, sign = side. The same fold + findInterval fmt_color_slots() does.
slot_of <- function(x, over, under, center) {
  s <- rep(0L, length(x)); ok <- is.finite(x)
  up  <- if (center == 1) x[ok] >= 1 else x[ok] >= center
  mag <- if (center == 1) ifelse(up, x[ok], 1 / x[ok]) else abs(x[ok] - center)
  s[ok] <- ifelse(up, findInterval(mag, over), -findInterval(mag, under))
  s
}
prof <- function(s, lbl) cat(sprintf("  %-34s %s   coloured %5.1f%%\n", lbl,
  paste(sprintf("|%d| %5.1f%%", 0:4, 100 * vapply(0:4, function(k) mean(abs(s) == k), 0)),
        collapse = "  "), 100 * mean(s != 0)))
sides <- function(s, lbl) cat(sprintf("  %-34s over  grey %5.1f%% top %5.1f%%   |   under  grey %5.1f%% top %5.1f%%\n",
  lbl, 100*mean(s[s >= 0] == 0), 100*mean(s[s >= 0] == 4),
       100*mean(s[s <= 0] == 0), 100*mean(s[s <= 0] == -4)))

## ---------- the four corpora ----------------------------------------------------
gss <- gss_cat_data_formatting()
arr <- carData::Arrests |> as_tibble() |>
  mutate(released = forcats::fct_rev(released), colour = forcats::fct_relevel(colour, "White"))
data("hdv2003", package = "questionr")
hdv <- hdv2003 |> as_tibble() |> mutate(cinema = forcats::fct_rev(cinema))
sal <- carData::Salaries |> as_tibble()

cells_of <- function(t) {                      # data cells only: no total row, no total column
  fc <- as.list(t)[vapply(as.list(t), is_fmt, TRUE)]
  fc <- fc[!vapply(fc, function(cl) isTRUE(is_totcol(cl)), TRUE)]
  purrr::map_dfr(fc, function(cl) tibble(
    diff = get_diff(cl), ratio = get_ratio(cl), or = get_or(cl), pct = get_pct(cl),
    var = get_ref_var(cl), tot = is_totrow(cl))) |>
    filter(!tot) |> mutate(base = pct - diff)
}

pct_tabs <- list(
  tab(gss, race, party3, pct = "row", na = "drop"),
  tab(gss, relig, married, pct = "row", na = "drop"),
  tab(gss, rincome, party3, pct = "row", na = "drop"),
  tab(gss, marital, relig, pct = "row", na = "drop"),
  tab(arr, c(colour, sex, employed, citizen), released, pct = "row"),
  tab(hdv, qualif, cinema, pct = "row", na = "drop"),
  tab(hdv, qualif, sport, pct = "row", na = "drop")
)
mean_tabs <- list(
  tab(gss, c(race, relig), c(age, tvhours), na = "drop"),
  tab(gss, c(rincome, marital), c(age, tvhours), na = "drop"),
  tab(hdv, c(qualif, sexe), c(age, heures.tv, freres.soeurs), na = "drop"),
  tab(sal, c(rank, discipline, sex), c(salary, yrs.since.phd), na = "drop"),
  tab(arr, colour, checks)
)
cell <- purrr::map_dfr(pct_tabs, cells_of)  |> filter(is.finite(diff), is.finite(ratio), ratio > 0)
mcel <- purrr::map_dfr(mean_tabs, cells_of) |> filter(is.finite(diff), is.finite(var), var > 0) |>
  mutate(glass = diff / sqrt(var))

reg_cols <- function(t) {
  fc <- as.list(t)[vapply(as.list(t), is_fmt, TRUE)]
  v  <- unlist(lapply(fc[grepl("^Model", names(fc))], get_num))
  v[is.finite(v) & !is.na(v)]
}
regs <- list(
  `binomial OR`  = tab_reg(arr, "released", c("colour","sex","employed","citizen","checks")),
  `binomial mRR` = tab_reg(arr, "released", c("colour","sex","employed","citizen","checks"),
                           measure = "ratio"),
  `hdv OR`       = tab_reg(hdv, "cinema", c("qualif","age")),
  `hdv mRR`      = tab_reg(hdv, "cinema", c("qualif","age"), measure = "ratio"),
  `poisson IRR`  = tab_reg(gss, "tvhours", c("race","rincome"), family = "poisson"),
  `gaussian beta`= tab_reg(gss, "age", c("race","rincome"), family = "gaussian")
)

## ---------- 1. the three readings of ONE percentage cell -------------------------
cat("\n=========== CROSSTAB PERCENTAGE CELLS:", nrow(cell), "===========\n")
cat("The same cells, graded on each ladder a percentage column can be read on:\n")
prof(slot_of(cell$diff,  B$pct_diff$over$breaks,   B$pct_diff$under$breaks,   0), "pct_diff   (the reference)")
prof(slot_of(cell$ratio, B$pct_ratio$over$breaks,  B$pct_ratio$under$breaks,  1), "pct_ratio")
prof(slot_of(cell$or,    B$odds_ratio$over$breaks, B$odds_ratio$under$breaks, 1), "odds_ratio")
sd <- slot_of(cell$diff, B$pct_diff$over$breaks, B$pct_diff$under$breaks, 0)
sr <- slot_of(cell$ratio, B$pct_ratio$over$breaks, B$pct_ratio$under$breaks, 1)
so <- slot_of(cell$or, B$odds_ratio$over$breaks, B$odds_ratio$under$breaks, 1)
cat(sprintf("\n  agreement with pct_diff, within one intensity:  ratio %4.1f%%   OR %4.1f%%\n",
            100*mean(abs(abs(sr) - abs(sd)) <= 1), 100*mean(abs(abs(so) - abs(sd)) <= 1)))

## ---------- 2. the two SIDES, and the ceiling that makes them differ -------------
cat("\n--- the two sides (a percentage ratio is capped at 1/base, so the over side has a ceiling) ---\n")
sides(sr, "pct_ratio, as shipped")
sides(slot_of(cell$ratio, B$pct_ratio$over$breaks, B$pct_ratio$over$breaks, 1), "  ... mirrored, for comparison")
cat(sprintf("  reach: largest ratio ABOVE x%.2f   largest BELOW /%.2f\n",
            max(cell$ratio[cell$ratio >= 1]), 1 / min(cell$ratio[cell$ratio > 0 & cell$ratio < 1])))
by_base <- cell |> mutate(band = cut(base, c(0, .05, .1, .2, .35, .5, 1))) |> group_by(band) |>
  summarise(n = n(), max_over = max(ratio[ratio >= 1], -Inf),
            max_under = 1 / min(c(ratio[ratio < 1 & ratio > 0], Inf)), ceiling = 1 / mean(base))
print(as.data.frame(by_base), digits = 3)

## ---------- 3. the BACKGROUND channel (the ladder's loud rungs only) -------------
cat("\n--- the background channel: COLOR_SCALES$pct_ratio$bg_keep =",
    COLOR_SCALES$pct_ratio$bg_keep, "---\n")
k <- COLOR_SCALES$pct_ratio$bg_keep
bo <- utils::tail(B$pct_ratio$over$breaks, k); bu <- utils::tail(B$pct_ratio$under$breaks, k)
cat(sprintf("  text  over %-22s under %s\n", paste(B$pct_ratio$over$breaks, collapse = "/"),
            paste(B$pct_ratio$under$breaks, collapse = "/")))
cat(sprintf("  bg    over %-22s under %s\n", paste(bo, collapse = "/"), paste(bu, collapse = "/")))
prof(sr, "text channel")
prof(slot_of(cell$ratio, bo, bu, 1), "background channel")

## ---------- 4. crosstab MEAN cells ----------------------------------------------
cat("\n=========== CROSSTAB MEAN CELLS:", nrow(mcel), "===========\n")
prof(slot_of(mcel$glass, B$mean_diff$over$breaks, B$mean_diff$under$breaks, 0), "mean_diff (Glass delta, SD)")
prof(slot_of(mcel$ratio, B$mean_ratio$over$breaks, B$mean_ratio$under$breaks, 1), "mean_ratio")
cat(sprintf("  |Glass delta| quantiles: median %.2f  75%% %.2f  90%% %.2f  max %.2f\n",
            median(abs(mcel$glass)), quantile(abs(mcel$glass), .75),
            quantile(abs(mcel$glass), .9), max(abs(mcel$glass))))
cat(sprintf("  mean ratio reach: above x%.2f  below /%.2f  (no ceiling -- a mean is unbounded)\n",
            max(mcel$ratio, na.rm = TRUE), 1 / min(mcel$ratio[mcel$ratio > 0], na.rm = TRUE)))

## ---------- 5. regression columns -----------------------------------------------
cat("\n=========== REGRESSION COLUMNS ===========\n")
for (nm in names(regs)) {
  t  <- regs[[nm]]
  cl <- as.list(t)[vapply(as.list(t), is_fmt, TRUE)]
  cl <- cl[grepl("^Model", names(cl))][[1]]
  sc <- EST_SCALES[[get_scale(cl)[1]]]
  b  <- color_scale_resolve(sc$break_key, color_scales())
  v  <- get_num(cl)
  # a `std` ladder is read in SD units, exactly as fmt_color_plan() does before findInterval()
  if (isTRUE(b$std)) {
    sdv <- sqrt(if (identical(sc$sd_from, "var")) get_var(cl) else get_ref_var(cl))
    v   <- v / sdv
  }
  v  <- v[is.finite(v)]
  v  <- v[abs(v - sc$neutral) > 1e-9]
  cat(sprintf("\n  %-14s scale %-12s ladder %s\n", nm, get_scale(cl)[1],
              paste(b$over$breaks, collapse = "/")))
  prof(slot_of(v, b$over$breaks, b$under$breaks, b$center), paste0("    ", nm))
}

## ---------- 6. the guaranteed_effect ladder --------------------------------------
cat("\n=========== guaranteed_effect: one rung down, no arithmetic ===========\n")
for (nm in c("pct_diff", "pct_ratio", "odds_ratio", "mean_diff")) {
  ov <- B[[nm]]$over$breaks
  cat(sprintf("  %-11s ignore %-22s guaranteed %s\n", nm, paste(ov, collapse = "/"),
              paste(guaranteed_breaks(ov, B[[nm]]$center), collapse = "/")))
}
