# =====================================================================================
# PERSONAL manual-review script -- Phase 18s (Kish n_eff on ALL weighted descriptive CIs).
# CONFIDENTIAL DATA (pc18): NOT a package/testthat test. Lives in dev/review_manual/ (.Rbuildignore'd).
#
# Goal: on data you know, compare the WEIGHTED confidence intervals with
#   options(tabxplor.kish_neff = FALSE)  vs  options(tabxplor.kish_neff = TRUE)
# and check the intervals WIDEN (n_eff < n under unequal weights => design effect > 1).
#
# Run in Positron for the pretty kable tables; run with Rscript for the numeric comparison below.
# =====================================================================================

library(devtools)
load_all("~/github/tabxplor")
suppressMessages({ library(dplyr); library(purrr) })

# For this scripted run: serial (no mirai orphans) + text print. In Positron use your own options.
options(tabxplor.parallel = FALSE, tabxplor.cleannames = TRUE)

# ---- data prep (verbatim from the past review script) -------------------------------
pc18 <- readRDS("~/Data/Pratiques culturelles/Pratiques culturelles 2018/pc18.rds")
musique_vars <- c("ROCK","JAZZ","CLASSIQUE","VARIETE","ELECTRO","METAL","CHANSON","WORLD","RAP","TRADI")
pc18 <- pc18 |>
  select(-any_of(c("CHANSON","WORLD","TRADI","VARIETE","RNB","ELECTRO","RAP","METAL","ROCK","JAZZ","OPERA","CLASSIQUE"))) |>
  rename(any_of(c(CHANSON="E1001", WORLD="E1002", TRADI="E1003", VARIETE="E1004", RNB="E1005", ELECTRO="E1006",
                  RAP="E1007", METAL="E1008", ROCK="E1009", JAZZ="E1010", OPERA="E1011", CLASSIQUE="E1012")))
pc18$CHANSON   <- forcats::fct_recode(pc18$CHANSON,  "1-Chanson ou variete francaise"="1-Chansons ou variétés françaises","2-Non"="2-Non")
pc18$WORLD     <- forcats::fct_recode(pc18$WORLD,    "1-World"="1-Musiques du monde","2-Non"="2-Non")
pc18$TRADI     <- forcats::fct_recode(pc18$TRADI,    "1-Tradi"="1-Musiques traditionnelles","2-Non"="2-Non")
pc18$VARIETE   <- forcats::fct_recode(pc18$VARIETE,  "1-Variete internationale"="1-Variétés internationales","2-Non"="2-Non")
pc18$ELECTRO   <- forcats::fct_recode(pc18$ELECTRO,  "1-Electro, techno"="1-Musiques électroniques, techno","2-Non"="2-Non")
pc18$RAP       <- forcats::fct_recode(pc18$RAP,      "1-Rap"="1-Hip hop, rap","2-Non"="2-Non")
pc18$METAL     <- forcats::fct_recode(pc18$METAL,    "1-Metal, hard rock"="1-Metal, hard rock","2-Non"="2-Non")
pc18$ROCK      <- forcats::fct_recode(pc18$ROCK,     "1-Pop, rock"="1-Pop, rock","2-Non"="2-Non")
pc18$JAZZ      <- forcats::fct_recode(pc18$JAZZ,     "1-Jazz"="1-Jazz","2-Non"="2-Non")
pc18$CLASSIQUE <- forcats::fct_recode(pc18$CLASSIQUE,"1-Classique"="1-Musique classique","2-Non"="2-Non")
pc18 <- pc18 |> select(-any_of("NB_MUSIQUE")) |> score_from_lv1(name="NB_MUSIQUES", vars_list=musique_vars)

# ---- helpers ------------------------------------------------------------------------
ci_hw  <- function(col) (get_ci_sup(col) - get_ci_inf(col)) / 2          # CI half-width
lab_of <- function(tb) tb[[ names(tb)[!map_lgl(tb, is_fmt)][1] ]]         # first (row) label col
fmt_cols <- function(tb) setdiff(names(tb)[map_lgl(tb, is_fmt)], "Total")  # data cols (skip the n-base)

# Build the SAME table twice (kish off/on); return a per-cell CI comparison for one fmt column.
compare_col <- function(build, col) {
  off <- withr::with_options(list(tabxplor.kish_neff = FALSE), build())
  on  <- withr::with_options(list(tabxplor.kish_neff = TRUE ), build())
  co <- off[[col]]; cn <- on[[col]]; is_mean <- get_type(cn) == "mean"
  base_n <- if (is_mean) get_n(cn) else get_tot_n(cn)
  tibble(
    row     = as.character(lab_of(off)),
    est     = round(if (is_mean) get_mean(cn) else 100 * get_pct(cn), 1),
    n       = base_n,
    n_eff   = round(get_n_eff(cn), 0),
    deff    = round(base_n / get_n_eff(cn), 2),                 # design effect n / n_eff
    hw_off  = round(if (is_mean) ci_hw(co) else 100 * ci_hw(co), 2),
    hw_on   = round(if (is_mean) ci_hw(cn) else 100 * ci_hw(cn), 2),
    widen_x = round(ci_hw(cn) / ci_hw(co), 2)
  ) |> filter(!(is.na(hw_off) & is.na(hw_on)))
}
compare_all <- function(build) {
  cols <- fmt_cols(build())
  imap(set_names(cols), function(cl, nm) { cat("\n--", nm, "--\n"); print(as.data.frame(compare_col(build, cl))) }) |> invisible()
}
sep <- function(t) cat("\n\n================= ", t, " =================\n")

# =====================================================================================
# CASE 1 -- weighted FACTOR proportions, cell CI (row%), several binary genres by SEXE
# =====================================================================================
sep("CASE 1  factor cell CI  (row%, weighted by POND)  SEXE x genres")
b1 <- function() tab(pc18, SEXE, c(CLASSIQUE, METAL, WORLD, RAP),
                     wt = POND, pct = "row", levels = "first", ci = "cell", na = "drop_all")
compare_all(b1)

# =====================================================================================
# CASE 2 -- weighted FACTOR proportions, DIFF CI (vs Total row)  CRITREVENU x CLASSIQUE
# =====================================================================================
sep("CASE 2  factor diff CI  (row%, weighted)  CRITREVENU x CLASSIQUE")
b2 <- function() tab(pc18, CRITREVENU, CLASSIQUE, wt = POND, pct = "row",
                     levels = "first", ci = "diff", na = "drop_all")
compare_all(b2)

# =====================================================================================
# CASE 3 -- weighted MEAN CI, numeric score NB_MUSIQUES by SEXE and by CRITREVENU
# =====================================================================================
sep("CASE 3  mean cell CI  (weighted)  NB_MUSIQUES by SEXE")
b3 <- function() tab_num(pc18, SEXE, NB_MUSIQUES, wt = POND, ci = "cell", na = "drop")
compare_all(b3)

sep("CASE 3b mean cell CI  (weighted)  NB_MUSIQUES by CRITREVENU")
b3b <- function() tab_num(pc18, CRITREVENU, NB_MUSIQUES, wt = POND, ci = "cell", na = "drop")
compare_all(b3b)

# =====================================================================================
# CASE 4 -- OVERALL design effect on this weight (POND): how much does kish move things?
# =====================================================================================
sep("CASE 4  overall design effect summary (POND weight)")
allcmp <- bind_rows(
  compare_col(b1, fmt_cols(b1())[1]),
  compare_col(b2, fmt_cols(b2())[1]),
  compare_col(b3, fmt_cols(b3())[1])
)
cat("median design effect (n / n_eff):", round(median(allcmp$deff, na.rm = TRUE), 2), "\n")
cat("median CI widening factor       :", round(median(allcmp$widen_x, na.rm = TRUE), 2), "\n")
cat("range  CI widening factor       :", paste(round(range(allcmp$widen_x, na.rm = TRUE), 2), collapse = " - "), "\n")

# =====================================================================================
# CASE 5 -- printed tabxplor tables (off vs on). ci_print="moe" shows the +- margin of error.
# =====================================================================================
sep("CASE 5  printed tables (pct +- moe): kish OFF then ON")
options(tabxplor.ci_print = "moe")
cat("\n----- kish OFF -----\n")
print(withr::with_options(list(tabxplor.kish_neff = FALSE), b2()))
cat("\n----- kish ON  -----\n")
print(withr::with_options(list(tabxplor.kish_neff = TRUE ), b2()))

cat("\n\nDONE.\n")
