# PURPOSE: reproduce every measurement in dev/weights_framework_stress_test_2_post_z16.md
#   (round 2 of the weights audit, run against Last Phase z16). Round 1 -- the PRE-z16 audit that
#   produced findings W1-W13 and led to the redesign -- is dev/weights_framework_stress_test.md,
#   whose own reproducer is inlined in its Appendix A.
# ROLE: an audit script, not a test file -- it PRINTS, it does not assert. The assertions that must
#   hold forever live in tests/testthat/ (test-flat-design-parity.R for the maths,
#   test-design-effect.R for the option's behaviour, test-survey-*.R for the design paths).
# USAGE:  OMP_NUM_THREADS=1 Rscript dev/weights_stress_test.R
# Blocks are labelled with the finding they supported (W-A .. W-G) or "OK" where they record a
# verified-sound property. SINCE Last Phase z16-iiii EVERY W-* FINDING IS FIXED: the blocks are kept as
# the REGRESSION probe for each one, and each now prints the corrected behaviour (see S9 of the
# companion .md for what each fix turned out to be).
# Companion documents: dev/weights_framework_redesign.md (the z16 design),
# dev/full_survey_design_scope.md (z14), dev/survey_design_measurements.R (z14's own reproducer).

Sys.setenv(LANGUAGE = "en")
suppressMessages(devtools::load_all("~/github/tabxplor", quiet = TRUE))
options(tabxplor.lang = "en")
stopifnot(requireNamespace("survey", quietly = TRUE))

ON <- function(e) withr::with_options(list(tabxplor.design_effect = TRUE), e)
R  <- function() tabxplor:::svy_degrade_reset()          # the build-scoped degrade flag
svse <- function(o) as.numeric(unlist(survey::SE(o)))
hd <- function(x) cat("\n\n===== ", x, " =====\n", sep = "")
fm <- function(x, d = 6) paste(format(x, digits = d), collapse = "  ")

# --- the fixture: weights that carry information about the outcome ----------------------------------
# (the assumption Kish makes and unequal weights break -- so a regression to (Sum w)^2/Sum(w^2)
#  shows up as a wrong number here, not as a lost digit)
fx <- function(n = 4000, seed = 7) {
  set.seed(seed)
  d <- data.frame(grp = factor(sample(c("A", "B", "C", "D"), n, TRUE, prob = c(.4, .3, .2, .1))),
                  g2  = factor(sample(c("u", "v", "z"), n, TRUE)))
  d$w   <- exp(stats::rnorm(n, 0, .55)) * c(A = .6, B = 1, C = 1.6, D = 2.4)[as.character(d$grp)]
  d$w   <- d$w / mean(d$w)
  lin   <- -0.3 + 0.5 * scale(log(d$w))[, 1] + c(A = -.4, B = 0, C = .3, D = .6)[as.character(d$grp)]
  d$col <- factor(ifelse(stats::rbinom(n, 1, stats::plogis(lin)) == 1, "yes", "no"),
                  levels = c("no", "yes"))
  d$x   <- round(stats::rnorm(n, 50, 12) + 6 * log(d$w), 4)
  d$yes01 <- as.numeric(d$col == "yes")   # svymean returns ONE column for a numeric indicator
  d$psu <- factor(paste0("p", rep(seq_len(n / 20), each = 20)))
  d$str <- factor(rep(c("s1", "s2"), each = n / 2))
  d
}
d    <- fx()
desf <- survey::svydesign(ids = ~1, weights = ~w, data = d)
desc <- survey::svydesign(ids = ~psu, strata = ~str, weights = ~w, data = d, nest = TRUE)


# ====================================================================================================
hd("OK 1. the closed form IS survey -- proportions, all bases")
# WHY the full-sample n/(n-1) is right for a DOMAIN: `[.survey.design2` subsets fpc$sampsize
# row-wise, which for ids = ~1 holds the ORIGINAL n in every row, and onestrat() pads to nPSU with
# zeros. So survey's own domain factor is the full-sample one.
vp <- function(col) { p <- get_pct(col); p * (1 - p) / get_n_eff(col) }
row <- ON(tab(d, grp, col, wt = w, pct = "row", ci = "cell"))
o   <- survey::svyby(~yes01, ~grp, desf, survey::svymean)
cat("row%  implied var:", fm(vp(row[["yes"]])[1:4], 9), "\n")
cat("row%  survey  var:", fm(svse(o)^2, 9), "\n")
cat("row%  ratio      :", fm(vp(row[["yes"]])[1:4] / svse(o)^2, 10), "\n")

hd("OK 2. ... and on a STRATIFIED + CLUSTERED design (svyrecvar path)")
R(); tc <- suppressMessages(tab(desc, grp, col, pct = "row", ci = "cell"))
oc <- survey::svyby(~yes01, ~grp, desc, survey::svymean)
cat("degf stored:", get_inference(tc)$degf, " (survey::degf:", survey::degf(desc), ")\n")
cat("ratio      :", fm(vp(tc[["yes"]])[1:4] / svse(oc)^2, 10), "\n")

hd("OK 3. ... and for MEANS")
tn <- ON(tab(d, grp, x, wt = w))
om <- survey::svyby(~x, ~grp, desf, survey::svymean)
cat("ratio      :", fm((get_var(tn[["x"]]) / get_n_eff(tn[["x"]]))[1:4] /
                       svse(om)^2, 10), "\n")

hd("OK 4. the three positions actually separate (CI width, pct = row)")
wd <- function(t) (get_ci_sup(t[["yes"]]) - get_ci_inf(t[["yes"]]))[1:4]
R()
cat("1 wt only    :", fm(wd(tab(d, grp, col, wt = w, pct = "row", ci = "cell")), 4), "\n")
cat("2 wt+option  :", fm(wd(ON(tab(d, grp, col, wt = w, pct = "row", ci = "cell"))), 4), "\n")
cat("3 design     :", fm(wd(suppressMessages(tab(desc, grp, col, pct = "row", ci = "cell"))), 4), "\n")


# ====================================================================================================
hd("W-A (FIXED). meta$inference survives the >=2 row_var merge")
# WAS: tab_compact() rebuilt `meta` from a hand-enumerated 3-field list, dropping everything else.
# NOW: tab_meta_merge() reduces the inputs' metas and overwrites only what the merge recomputes.
f <- function(t) tab_weight_line(t) %||% "(none)"
cat("1 row_var , ON :", f(ON(tab(d, grp, col, wt = w, pct = "row"))), "\n")
cat("2 row_vars, ON :", f(ON(tab(d, c(grp, g2), col, wt = w, pct = "row"))), "  <-- was INVERTED\n")
cat("inference stored, 2 row_vars:",
    if (is.null(get_inference(ON(tab(d, c(grp, g2), col, wt = w, pct = "row"))))) "NULL" else "present", "\n")
t1 <- ON(tab(d, grp, col, wt = w, pct = "row", ci = "cell"))
t2 <- ON(tab(d, c(grp, g2), col, wt = w, pct = "row", ci = "cell"))
cat("...yet the CELLS are corrected in both:\n")
cat("   1 row_var  widths:", fm(wd(t1), 4), "\n")
cat("   2 row_vars widths:", fm(wd(t2), 4), "\n")
R(); cat("design, 2 row_vars:", f(suppressMessages(tab(desf, c(grp, g2), col, pct = "row"))), "\n")
cat("   ^ now from the STORED basis; the `.svy_weights` name-sniff (W5) is deleted\n")

hd("W-A (numbers, FIXED). the degf survives; losing it would cost 9 % of interval width")
dd  <- fx(560, seed = 9); dd$psu <- factor(rep(seq_len(14), each = 40))
des2 <- survey::svydesign(ids = ~psu, weights = ~w, data = dd)
R(); s1 <- suppressMessages(tab_plain(des2, grp, col, pct = "row"))
cat("design degf:", survey::degf(des2), " | stored:", get_inference(s1)$degf, "\n")
w_with <- (function(x) get_ci_sup(x) - get_ci_inf(x))(tab_ci(s1)[["yes"]])[1:3]
s1b <- s1; attr(s1b, "meta")$inference <- NULL            # simulate the merge's loss
w_without <- (function(x) get_ci_sup(x) - get_ci_inf(x))(tab_ci(s1b)[["yes"]])[1:3]
cat("with    stored degf:", fm(w_with, 6), "\n")
cat("without stored degf:", fm(w_without, 6), "\n")
cat("anti-conservative by:", fm(100 * (1 - w_without / w_with), 3), "%\n")


# ====================================================================================================
hd("W-B (FIXED). color = 'contrib' significance follows the design")
# WAS: the residual's base was the total column's GRAND CELL, whose p is 1 -> Var 0 -> the degenerate
# fallback svy_flat_base_neff(B, S) = B^2/S, the weights-only number, at EVERY basis.
# NOW: the raw n over Rao-Scott's delta-bar, the same one the omnibus row reports. The grand cell's
# own n_eff is STILL B^2/S below -- that is the field, correctly; what changed is that the residual
# no longer reads it, which is why the two bases' p-values now differ.
gb <- function(t) { v <- get_n_eff(t[["Total"]]); v[length(v)] }
R(); a <- ON(tab(d, grp, col, wt = w, pct = "row", color = "contrib"))
R(); b <- suppressMessages(tab(desc, grp, col, pct = "row", color = "contrib"))
cat("grand-cell n_eff, weights basis :", fm(gb(a), 10), "\n")
cat("grand-cell n_eff, DESIGN basis  :", fm(gb(b), 10), "\n")
cat("B^2/S (flat, by hand)           :", fm(sum(d$w)^2 / sum(d$w^2), 10), "\n")
cat("contrib p identical across the two bases:",
    isTRUE(all.equal(get_pvalue(a[["yes"]]), get_pvalue(b[["yes"]]))), "\n")

hd("W-B (magnitude). the bad case: a CLUSTER-LEVEL row_var")
# clustering inflates a marginal proportion far more than an association -- so an individual-level
# row_var is nearly harmless, and a geography / school / establishment is not.
set.seed(5); nc <- 120; m <- 30; N <- nc * m
cl  <- rep(seq_len(nc), each = m); gcl <- sample(c("A", "B", "C"), nc, TRUE)
e <- data.frame(psu = factor(cl), grp = factor(gcl[cl]))
e$w   <- exp(stats::rnorm(N, 0, .35)); e$w <- e$w / mean(e$w)
e$yes01 <- NA_real_
e$col <- factor(ifelse(stats::rbinom(N, 1, stats::plogis(
  -0.2 + stats::rnorm(nc, 0, 1.1)[cl] + c(A = -.4, B = 0, C = .5)[as.character(e$grp)])) == 1,
  "yes", "no"), levels = c("no", "yes"))
dese <- survey::svydesign(ids = ~psu, weights = ~w, data = e)
R(); te <- suppressMessages(tab(dese, grp, col, pct = "row", color = "contrib", test = TRUE))
st <- get_test(te); base_used <- gb(te)
cat("n                          :", N, "\n")
cat("residual base USED (B^2/S) :", fm(base_used, 8), "\n")
cat("Rao-Scott mean deff        :", fm(st$deff, 6), "\n")
cat("design-honest base n/deff  :", fm(N / st$deff, 8), "\n")
cat("|z| overstated by          : x", fm(sqrt(base_used / (N / st$deff)), 5), "\n")
z <- fmt_resid(te[["yes"]])
cat("contrib p, as shipped      :", fm(get_pvalue(te[["yes"]])[1:3], 4), "\n")
cat("contrib p, design-honest   :", fm(2 * stats::pnorm(-abs(z[1:3] / sqrt(st$deff))), 4), "\n")

hd("W-B (contrast). an INDIVIDUAL-level row_var is nearly harmless")
set.seed(21); nc2 <- 120; m2 <- 25; N2 <- nc2 * m2
cl2 <- rep(seq_len(nc2), each = m2)
h <- data.frame(psu = factor(cl2), grp = factor(sample(c("A", "B", "C"), N2, TRUE)))
h$w   <- exp(stats::rnorm(N2, 0, .4)); h$w <- h$w / mean(h$w)
h$col <- factor(ifelse(stats::rbinom(N2, 1, stats::plogis(
  -0.2 + stats::rnorm(nc2, 0, 1.2)[cl2] + c(A = -.4, B = 0, C = .5)[as.character(h$grp)])) == 1,
  "yes", "no"), levels = c("no", "yes"))
desh <- survey::svydesign(ids = ~psu, weights = ~w, data = h)
R(); th <- suppressMessages(tab(desh, grp, col, pct = "row", color = "contrib", test = TRUE))
sh <- get_test(th)
cat("cell n_eff (design) :", fm(get_n_eff(th[["yes"]])[1:3], 6), "  <-- a real cell deff\n")
cat("Rao-Scott mean deff :", fm(sh$deff, 6), "  <-- but the ASSOCIATION is barely clustered\n")
cat("residual base used  :", fm(gb(th), 8), " | design-honest n/deff:", fm(N2 / sh$deff, 8), "\n")
cat("|z| overstated by   : x", fm(sqrt(gb(th) / (N2 / sh$deff)), 5), "\n")

hd("W-B (secondary). two effective sizes for one table, even at position 2")
cat("residual base B^2/S            :", fm(sum(d$w)^2 / sum(d$w^2), 8), "\n")
chf <- survey::svychisq(~grp + col, desf, statistic = "F")
x2f <- sum((chf$observed - chf$expected)^2 / chf$expected)
cat("omnibus implied n/deff         :", fm(4000 / (x2f / (as.numeric(chf$statistic) * 3)), 8), "\n")


# ====================================================================================================
hd("W-C (FIXED). the degrade flag no longer leaks across calls into tab_reg()")
# tab() resets it in tab_transform() and both leaf wrappers; tab_reg() now does too.
R(); cat("clean flag  -> basis:",
         tab_inference_basis(suppressMessages(
           tab_reg(desf, dependent = "col", predictors = "grp", family = "binomial"))), "\n")
R(); suppressMessages(tabxplor:::svy_var_degraded("size"))      # a degrade in an EARLIER call
cat("stale flag  -> basis:",
    tab_inference_basis(suppressMessages(
      tab_reg(desf, dependent = "col", predictors = "grp", family = "binomial"))),
    "  <-- was wrong: the footer then denied a variance that was computed\n")
R()


# ====================================================================================================
hd("W-D (FIXED). the crude Obs_* columns STORE the effective base they used")
g <- tabxplor:::reg_empirical(d, fac_preds = "grp", dependent = "col", crude_key = "binomial",
                              positive_level = "no", wt = "w", conf_level = 0.95)
k1 <- g$category == g$category[[1]]        # the MODELLED category, one row per level
cat("reg_empirical emp_n_draw:", fm(g$emp_n_draw[k1], 8), "\n")
cat("tab()          n_eff    :", fm(get_n_eff(ON(tab(d, grp, col, wt = w, pct = "row"))[["no"]])[1:4], 8),
    "  <-- identical\n")
R(); rr <- suppressMessages(tab_reg(d, dependent = "col", predictors = "grp", wt = "w",
                                    family = "binomial", empirical = TRUE))
cat("Obs_%  n_eff FIELD      :", fm(get_n_eff(rr[["Obs_%"]])), "  <-- NA on the Constant only\n")

hd("W-E (RULED WANTED across tables; FIXED inside tab_reg). two interval methods")
pk <- g$emp_prop[k1]; nk <- g$emp_n_draw[k1]
hand <- tabxplor:::ci_prop_diff(pk[2:4], nk[2:4], rep(pk[[1]], 3), rep(nk[[1]], 3),
                                conf_level = 0.95, method = "newcombe", want_p = FALSE)
td <- ON(tab(d, grp, col, wt = w, pct = "row", ci = "diff", ref = 1))
cat("hand ci_prop_diff(newcombe):", fm(hand$sup - hand$inf, 8), "\n")
cat("tab()   (newcombe):", fm((get_ci_sup(td[["no"]]) - get_ci_inf(td[["no"]]))[2:4], 8), "\n")
cat("tab_reg (wald)    :", fm((get_ci_sup(rr[["Obs_%"]]) - get_ci_inf(rr[["Obs_%"]]))[3:5], 8), "\n")
cat("ci_settings method_diff -- tab():", get_ci_settings(td)$method_diff,
    "| tab_reg():", get_ci_settings(rr)$method_diff, "\n")

hd("W-F (RULED: keep + document). the two footers a session prints for one weight")
R(); cat("tab()    :", f(tab(d, grp, col, wt = w, pct = "row")), "\n")
cat("tab_reg():", f(rr), "\n")


# ====================================================================================================
hd("W-G.1 (FIXED). inference_basis is declared (findGlobals still lists it -- that is what")
cat("       globalVariables() answers; the point is that R CMD check no longer NOTEs it)\n")
for (fn in c("tab_transform", "tab_assemble_tables")) {
  v <- codetools::findGlobals(get(fn, envir = asNamespace("tabxplor")), merge = FALSE)$variables
  cat(sprintf("%-20s undefined: %s\n", fn,
              paste(intersect(v, c("inference_basis", "inference_mode", "design_spec", "wt", "data")),
                    collapse = ", ")))
}
cat("declared in globalVariables()?",
    all(c("inference_basis", "robust_tests") %in%
          get(".__global__", envir = asNamespace("tabxplor"))), "\n")


# ====================================================================================================
hd("OK 5. every family's crude column moves with the position")
fam <- list(
  binomial = list(dependent = "col",  family = "binomial"),
  gaussian = list(dependent = "x",    family = "gaussian"),
  ame      = list(dependent = "col",  family = "binomial", effect = "ame"))
for (nm in names(fam)) for (W in list(NULL, "w")) {
  R(); r <- suppressWarnings(suppressMessages(do.call(
    tab_reg, c(list(d, predictors = "grp", empirical = TRUE, wt = W), fam[[nm]]))))
  for (cn in grep("^Obs_", names(r), value = TRUE)) {
    v <- get_ci_sup(r[[cn]]) - get_ci_inf(r[[cn]]); v <- v[is.finite(v)]
    cat(sprintf("%-9s %-9s %-10s %s\n", nm, if (is.null(W)) "[unwt]" else "[wt]", cn, fm(v[1:2], 5)))
  }
}

hd("OK 6. tab_counts() states only what it can carry")
agg <- as.data.frame(dplyr::count(d, grp, col, wt = w, name = "wn"))
tcn <- ON(tab_counts(agg, grp, col, counts = wn, pct = "row"))
cat("weighted counts only -> basis:", tab_inference_basis(tcn), "| line:", f(tcn), "\n")
cnt <- as.data.frame(dplyr::count(d, grp, col, name = "n")); cnt$wn <- agg$wn
tc2 <- ON(tab_counts(cnt, grp, col, counts = n, wt_counts = wn, pct = "row"))
cat("with wt_counts       -> basis:", tab_inference_basis(tc2), "| line:", f(tc2), "\n")
cat("   ^ the weighted basis was asked for and cannot be served (no per-obs Sum w^2):\n",
    "     svy_degrade_unserved() downgrades to 'n' and the footer says so.\n")

hd("OK 7. the option is read in exactly one DECISION site")
cat(paste(system("grep -rn 'getOption(\"tabxplor.design_effect' ~/github/tabxplor/R/",
                 intern = TRUE), collapse = "\n"), "\n")
cat("   ^ survey-design.R:139 is the decision; jmvtab.b.R:41 only saves it for on.exit().\n")

cat("\n\ndone.\n")
