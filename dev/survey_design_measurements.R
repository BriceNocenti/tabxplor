# dev/survey_design_measurements.R -- the reproducer for dev/full_survey_design_scope.md (2026-08-11).
#
# Every number quoted in that study comes from one of the blocks below. Run the whole file, or source
# it and call the blocks individually. Needs `survey` and `marginaleffects` (both already Suggests/
# Imports). ~1 minute total; block 6 is the slow one (60k rows).
#
#   1. how far the current cell interval is from the design-based one, by design feature
#   2. the decisive one: a SEGREGATED vs a spread row_var, cell AND difference
#   3. calibration -- where Kish points the wrong way
#   4. the influence-function route: exact vs survey, and its cost
#   5. Route A: Wilson / Newcombe / Woolf on a design-based effective n (5b odds ratio, 5c gains)
#   6. cost + correctness under calibration at survey scale
#   7. the nine defects (D1-D9), each reproduced
#   8. what tab(design) does get right today
#
# Usage:  OMP_NUM_THREADS=1 Rscript dev/survey_design_measurements.R

suppressMessages(devtools::load_all(".", quiet = TRUE))
suppressMessages(library(survey))

hdr <- function(x) cat("\n\n########## ", x, " ##########\n")

# --- a synthetic survey where each design feature really bites --------------------------------------
# strata explain the outcome, PSUs carry a random effect (real ICC), weights are unequal, and `grp`
# can be either spread across PSUs or segregated into them (region / race / school / quartier).
make_survey <- function(seed, S = 8, P = 50, M = 50, segregated = FALSE, sigma_u = 0.55) {
  set.seed(seed)
  d <- expand.grid(m = seq_len(M), p = seq_len(P), s = seq_len(S))
  n <- nrow(d)
  d$strat <- factor(d$s); d$psu <- factor(paste0(d$s, "-", d$p))
  u  <- stats::rnorm(nlevels(d$psu), 0, sigma_u)
  pg <- sample(c("A", "B", "C", "D"), nlevels(d$psu), TRUE)
  d$grp <- factor(if (segregated)
    ifelse(stats::runif(n) < 0.85, pg[as.integer(d$psu)], sample(c("A","B","C","D"), n, TRUE))
    else sample(c("A", "B", "C", "D"), n, TRUE))
  lin <- u[as.integer(d$psu)] + seq(-0.8, 0.8, length.out = S)[d$s] +
    c(A = -0.4, B = 0, C = 0.3, D = 0.6)[as.character(d$grp)]
  d$y   <- stats::rbinom(n, 1, stats::plogis(lin))
  d$col <- factor(ifelse(d$y == 1, "yes", "no"))
  d$num <- round(stats::rnorm(n, 50, 12) + 8 * u[as.integer(d$psu)])
  w     <- c(.5,.7,1,1,1.2,1.5,2,2.6)[d$s] * exp(stats::rnorm(n, 0, .3))
  d$w   <- w / mean(w)
  d
}
tabx_se <- function(p, nbase) sqrt(p * (1 - p) / nbase)   # the SE tabxplor's interval implies
neff_kish <- function(w) sum(w)^2 / sum(w^2)

# ====================================================================================================
hdr("1. SE(design) / SE(tabxplor), by design feature")
d <- make_survey(11); n <- nrow(d)
cat(sprintf("n = %d, %d PSUs, weight CV = %.2f, Kish deff = %.3f\n",
            n, nlevels(d$psu), sd(d$w)/mean(d$w), n * sum(d$w^2)/sum(d$w)^2))
des <- list(
  weights_only  = svydesign(~1,   weights = ~w, data = d),
  strat         = svydesign(~1,   strata = ~strat, weights = ~w, data = d),
  cluster       = svydesign(~psu, weights = ~w, data = d),
  strat_cluster = svydesign(~psu, strata = ~strat, weights = ~w, data = d, nest = TRUE))
popx <- c(`(Intercept)` = sum(d$w),
          stats::setNames(colSums(stats::model.matrix(~grp, d)[, -1, drop = FALSE] * d$w),
                          paste0("grp", c("B","C","D"))))
des$calibrated <- calibrate(des$strat_cluster, ~grp, popx, calfun = "linear")
for (dn in names(des)) {
  r <- vapply(levels(d$grp), function(g) {
    m  <- svymean(~y, subset(des[[dn]], grp == g)); p <- coef(m)[[1]]
    ix <- d$grp == g
    c(SE(m)[[1]]/tabx_se(p, sum(ix)), SE(m)[[1]]/tabx_se(p, neff_kish(d$w[ix])))
  }, numeric(2))
  cat(sprintf("  %-14s vs raw n: x%.2f   vs Kish n_eff: x%.2f\n", dn, mean(r[1,]), mean(r[2,])))
}

# ====================================================================================================
hdr("2. THE DECISIVE ONE -- segregated vs spread row_var, cell AND difference")
for (seg in c(FALSE, TRUE)) {
  d   <- make_survey(7, segregated = seg)
  des <- svydesign(~psu, strata = ~strat, weights = ~w, data = d, nest = TRUE)
  by  <- svyby(~y, ~grp, des, svymean, covmat = TRUE); V <- vcov(by); p <- coef(by)
  ne  <- function(g) neff_kish(d$w[d$grp == g])
  nr  <- function(g) sum(d$grp == g)
  cell <- c(SE(by)[1]/tabx_se(p[["A"]], nr("A")), SE(by)[1]/tabx_se(p[["A"]], ne("A")))
  ct   <- svycontrast(by, stats::setNames(c(1, -1), c("B", "A")))
  dr   <- sqrt(p[["A"]]*(1-p[["A"]])/nr("A") + p[["B"]]*(1-p[["B"]])/nr("B"))
  dk   <- sqrt(p[["A"]]*(1-p[["A"]])/ne("A") + p[["B"]]*(1-p[["B"]])/ne("B"))
  cat(sprintf("  %-11s CELL design/raw x%.2f  design/Kish x%.2f  |  DIFF design/raw x%.2f  design/Kish x%.2f\n",
              if (seg) "SEGREGATED" else "spread", cell[1], cell[2],
              SE(ct)[[1]]/dr, SE(ct)[[1]]/dk))
}
cat("  -> when the row_var is segregated into PSUs, today's stars are ~2.4x too generous, Kish included.\n")

# ====================================================================================================
hdr("3. Calibration -- Kish cannot see it")
set.seed(3); N <- 3e5
pop <- data.frame(x = factor(sample(1:4, N, TRUE, prob = c(.4,.3,.2,.1))))
pop$y <- stats::rbinom(N, 1, stats::plogis(-1 + c(0,.9,1.7,2.4)[as.integer(pop$x)]))
pop$stratum <- factor(sample(1:6, N, TRUE))
fr  <- c(.004,.006,.008,.010,.014,.020)[as.integer(pop$stratum)]
sel <- stats::runif(N) < fr
smp <- pop[sel, ]; smp$w <- 1/fr[sel]
d0  <- svydesign(~1, strata = ~stratum, weights = ~w, data = smp)
dc  <- calibrate(d0, ~x, c(`(Intercept)` = N,
                           colSums(stats::model.matrix(~x, pop)[, -1, drop = FALSE])), calfun = "linear")
p <- coef(svymean(~y, dc))[[1]]
ne0 <- neff_kish(smp$w); nec <- neff_kish(weights(dc))
cat(sprintf("  uncalibrated : SE_design %.5f | Kish n_eff %.0f -> %.5f  (design/Kish %.2f)\n",
            SE(svymean(~y, d0))[[1]], ne0, tabx_se(p, ne0), SE(svymean(~y, d0))[[1]]/tabx_se(p, ne0)))
cat(sprintf("  CALIBRATED   : SE_design %.5f | Kish n_eff %.0f -> %.5f  (design/Kish %.2f)\n",
            SE(svymean(~y, dc))[[1]], nec, tabx_se(p, nec), SE(svymean(~y, dc))[[1]]/tabx_se(p, nec)))
cat(sprintf("  -> calibration moved the DESIGN SE by x%.3f and the Kish SE by x%.3f\n",
            SE(svymean(~y, dc))[[1]]/SE(svymean(~y, d0))[[1]], tabx_se(p, nec)/tabx_se(p, ne0)))

# ====================================================================================================
hdr("4. The influence-function route -- exact vs survey, and its cost")
d <- make_survey(5); n <- nrow(d)
des <- svydesign(~psu, strata = ~strat, weights = ~w, data = d, nest = TRUE)
RL <- levels(d$grp)
# z_ij(k) = 1{row_k = i} * (1{col_k = j} - p_ij) / W_i ; Var = svyrecvar(w * z, ...)
if_cell <- function(i, lev = "yes") {
  m <- d$grp == RL[i]; Wi <- sum(d$w[m])
  p <- sum(d$w[m & d$col == lev]) / Wi
  list(p = p, z = as.numeric(m) * ((d$col == lev) - p) / Wi)
}
Z  <- vapply(seq_along(RL), function(i) if_cell(i)$z, numeric(n))
Vj <- svyrecvar(d$w * Z, des$cluster, des$strata, des$fpc, postStrata = des$postStrata)
by <- svyby(~col, ~grp, des, svymean, covmat = TRUE); V <- vcov(by); nm <- names(coef(by))
ix <- function(i) which(nm == paste0(RL[i], ":colyes"))
cat(sprintf("  cell SE, IF vs survey : max relative error = %.2e\n",
            max(abs(sqrt(diag(Vj)) / vapply(seq_along(RL), function(i) sqrt(V[ix(i), ix(i)]), 0) - 1))))
cat(sprintf("  diff SE, IF vs survey : ratio = %.9f\n",
            sqrt(Vj[1,1] + Vj[2,2] - 2*Vj[1,2]) /
              sqrt(V[ix(1),ix(1)] + V[ix(2),ix(2)] - 2*V[ix(1),ix(2)])))
t_if  <- system.time(svyrecvar(d$w * Z, des$cluster, des$strata, des$fpc,
                               postStrata = des$postStrata))[["elapsed"]]
t_svy <- system.time(svyby(~col, ~grp, des, svymean, covmat = TRUE))[["elapsed"]]
cat(sprintf("  one svyrecvar block %.2f s   vs   svyby(covmat = TRUE) %.2f s\n", t_if, t_svy))

# ====================================================================================================
hdr("5. ROUTE A -- every existing CI engine on a design-based effective n")
for (seg in c(FALSE, TRUE)) {
  d   <- make_survey(5, segregated = seg); n <- nrow(d)
  des <- svydesign(~psu, strata = ~strat, weights = ~w, data = d, nest = TRUE)
  by  <- svyby(~col, ~grp, des, svymean, covmat = TRUE); V <- vcov(by); cf <- coef(by)
  nm  <- names(cf); RL <- levels(d$grp)
  ix  <- function(r) which(nm == paste0(r, ":colyes"))
  ne  <- vapply(RL, function(r) cf[ix(r)]*(1-cf[ix(r)])/V[ix(r), ix(r)], 0)   # n_eff(design)
  cat(sprintf("\n  --- row_var %s ---\n", if (seg) "SEGREGATED" else "spread"))
  for (r in RL[1:2]) {
    p <- cf[ix(r)]; wl <- ci_wilson(p, ne[[r]], conf_level = .95)
    cat(sprintf("  %-3s cell : design [%.4f;%.4f]   Wilson(n_eff) [%.4f;%.4f]\n",
                r, p - 1.96*sqrt(V[ix(r),ix(r)]), p + 1.96*sqrt(V[ix(r),ix(r)]), wl$inf, wl$sup))
  }
  for (r in RL[-1]) {
    a <- ix(RL[1]); b <- ix(r); pa <- cf[a]; pb <- cf[b]
    se_des <- sqrt(V[a,a] + V[b,b] - 2*V[a,b])
    se_ne  <- sqrt(pa*(1-pa)/ne[[RL[1]]] + pb*(1-pb)/ne[[r]])
    se_raw <- sqrt(pa*(1-pa)/sum(d$grp == RL[1]) + pb*(1-pb)/sum(d$grp == r))
    cat(sprintf("  %s-%s diff : design %.5f | on n_eff(design) x%.2f | today (raw n) x%.2f\n",
                r, RL[1], se_des, se_ne/se_des, se_raw/se_des))
  }
}
cat("\n  -> exact where clustering bites, conservative (never anti-conservative) where it does not.\n")

# --- 5b. the same lever on the ODDS RATIO (Woolf on effective counts) --------------------------------
hdr("5b. ROUTE A on the odds ratio -- Woolf on effective counts vs a saturated svyglm")
for (seg in c(FALSE, TRUE)) {
  d   <- make_survey(6, segregated = seg)
  des <- svydesign(~psu, strata = ~strat, weights = ~w, data = d, nest = TRUE)
  by  <- svyby(~y, ~grp, des, svymean, covmat = TRUE); V <- vcov(by); p <- coef(by)
  RL  <- levels(d$grp)
  ne  <- vapply(RL, function(r) p[[r]]*(1-p[[r]])/V[r, r], 0)
  nk  <- vapply(RL, function(r) neff_kish(d$w[d$grp == r]), 0)
  nr  <- vapply(RL, function(r) sum(d$grp == r), 0)
  f   <- svyglm(y ~ grp, design = des, family = quasibinomial())
  ci  <- exp(stats::confint(f)); wd <- function(l) log(ci[paste0("grp",l),2]) - log(ci[paste0("grp",l),1])
  woolf <- function(l, base) ci_or(p[[l]]*base[[l]], (1-p[[l]])*base[[l]],
                                   p[[RL[1]]]*base[[RL[1]]], (1-p[[RL[1]]])*base[[RL[1]]],
                                   conf_level = .95)
  cat(sprintf("\n  --- predictor %s ---\n", if (seg) "SEGREGATED" else "spread"))
  for (l in RL[-1]) {
    r_ne <- woolf(l, ne); r_nk <- woolf(l, nk); r_nr <- woolf(l, nr)
    cat(sprintf("  %s vs %s : design [%.4f;%.4f] | Woolf on raw n x%.2f | on Kish x%.2f | on n_eff(design) x%.2f\n",
                l, RL[1], ci[paste0("grp",l),1], ci[paste0("grp",l),2],
                (log(r_nr$sup)-log(r_nr$inf))/wd(l), (log(r_nk$sup)-log(r_nk$inf))/wd(l),
                (log(r_ne$sup)-log(r_ne$inf))/wd(l)))
  }
}

# --- 5c. Route A is NOT a one-way widener: it carries the design's PRECISION GAINS too ---------------
hdr("5c. Route A under a design that GAINS precision (stratification / calibration)")
set.seed(21); S <- 10; nper <- 400; nn <- S*nper
q <- data.frame(strat = factor(rep(1:S, each = nper)))
q$y <- stats::rbinom(nn, 1, stats::plogis(seq(-2.2, 2.2, length.out = S)[as.integer(q$strat)]))
q$w <- 1                                                   # EQUAL weights: Kish deff is exactly 1
qs <- svydesign(~1, strata = ~strat, weights = ~w, data = q)
pq <- coef(svymean(~y, qs))[[1]]; neq <- pq*(1-pq)/SE(svymean(~y, qs))[[1]]^2
w_of <- function(base) { ci <- ci_wilson(pq, base, conf_level = .95); ci$sup - ci$inf }
cat(sprintf("  (a) stratified, equal weights, predictive strata\n"))
cat(sprintf("      raw n %d | Kish n_eff %.0f | design n_eff %.0f -> deff %.3f\n",
            nn, neff_kish(q$w), neq, nn/neq))
cat(sprintf("      interval width: today %.5f -> Route A %.5f  (x%.2f, NARROWER)\n",
            w_of(nn), w_of(neq), w_of(neq)/w_of(nn)))

set.seed(22); NN <- 2e5
pp <- data.frame(x = factor(sample(1:5, NN, TRUE)))
pp$y <- stats::rbinom(NN, 1, stats::plogis(seq(-2, 2, length.out = 5)[as.integer(pp$x)]))
sm <- pp[sample(NN, 4000), ]; sm$w <- NN/4000
c0 <- svydesign(~1, weights = ~w, data = sm)
cc <- calibrate(c0, ~x, c(`(Intercept)` = NN,
                          colSums(stats::model.matrix(~x, pp)[, -1, drop = FALSE])), calfun = "linear")
pc <- coef(svymean(~y, cc))[[1]]; nec2 <- pc*(1-pc)/SE(svymean(~y, cc))[[1]]^2
w2 <- function(base) { ci <- ci_wilson(pc, base, conf_level = .95); ci$sup - ci$inf }
cat(sprintf("  (b) calibrated on a predictive auxiliary, equal weights\n"))
cat(sprintf("      raw n %d | Kish n_eff %.0f | design n_eff %.0f -> deff %.3f\n",
            nrow(sm), neff_kish(sm$w), nec2, nrow(sm)/nec2))
cat(sprintf("      interval width: today %.5f -> Route A %.5f  (x%.2f) | Kish x%.2f (cannot move)\n",
            w2(nrow(sm)), w2(nec2), w2(nec2)/w2(nrow(sm)), w2(neff_kish(sm$w))/w2(nrow(sm))))
cat("  -> a design-based n_eff can EXCEED the raw n. Route A carries gains as well as losses.\n")

# ====================================================================================================
hdr("6. Cost + correctness under calibration, at survey scale (60k rows, 1000 PSUs)")
set.seed(2); n <- 60000; nps <- 1000
d <- data.frame(strat = factor(rep(1:10, each = n/10)))
d$psu <- factor(paste0(d$strat, "-", rep(rep(1:(nps/10), each = n/nps), 10)))
u <- stats::rnorm(nlevels(d$psu), 0, .5)
d$row <- factor(sample(paste0("r", 1:15), n, TRUE))
d$aux <- factor(sample(c("a","b","c"), n, TRUE, prob = c(.5,.3,.2)))
lin <- u[as.integer(d$psu)] + seq(-.6,.6,length.out = 15)[as.integer(d$row)] +
  c(a=0,b=.4,c=.9)[as.character(d$aux)]
d$col <- factor(paste0("L", 1 + stats::rbinom(n, 7, stats::plogis(lin))))
d$w   <- {w <- c(.5,.7,.9,1,1,1.1,1.3,1.6,2,2.6)[as.integer(d$strat)]*exp(stats::rnorm(n,0,.3)); w/mean(w)}
des <- svydesign(~psu, strata = ~strat, weights = ~w, data = d, nest = TRUE)
cal <- calibrate(des, ~aux, c(`(Intercept)` = sum(d$w),
                              auxb = sum(d$w[d$aux=="b"])*1.08,
                              auxc = sum(d$w[d$aux=="c"])*0.93), calfun = "linear")
RL <- levels(d$row); CL <- levels(d$col)
block <- function(dsg, j) {
  W <- as.numeric(1/dsg$prob)
  Z <- vapply(seq_along(RL), function(i) {
    m <- d$row == RL[i]; Wi <- sum(W[m])
    m * ((d$col == CL[j]) - sum(W[m & d$col == CL[j]])/Wi) / Wi }, numeric(n))
  svyrecvar(W * Z, dsg$cluster, dsg$strata, dsg$fpc, postStrata = dsg$postStrata)
}
t_pass <- system.time(lapply(seq_along(CL), function(j) block(des, j)))[["elapsed"]]
t_cal  <- system.time(VC <- lapply(seq_along(CL), function(j) block(cal, j)))[["elapsed"]]
t_tab  <- system.time(tab(d, row, col, wt = w, pct = "row", ci = "diff", test = TRUE))[["elapsed"]]
cat(sprintf("  design-variance pass %.2f s | calibrated %.2f s | tab() itself %.2f s  -> x%.2f\n",
            t_pass, t_cal, t_tab, t_pass/t_tab))
by <- svyby(~col, ~row, cal, svymean, covmat = TRUE); Vb <- vcov(by); nb <- names(coef(by))
cat(sprintf("  CALIBRATED cell SE, IF vs svyby : max relative error = %.2e\n",
            max(abs(vapply(seq_along(RL), function(i)
              sqrt(VC[[2]][i,i]) / sqrt(Vb[which(nb == paste0(RL[i], ":col", CL[2])),
                                          which(nb == paste0(RL[i], ":col", CL[2]))]) - 1, 0)))))

# ====================================================================================================
hdr("7. The nine defects")
set.seed(4); n <- 6000
b <- data.frame(psu = factor(rep(1:120, each = 50)))
b$hidden <- stats::rnorm(n); b$w <- exp(0.9*b$hidden); b$w <- b$w/mean(b$w)
b$x <- factor(sample(c("low","mid","high"), n, TRUE), levels = c("low","mid","high"))
b$z <- factor(sample(c("u","v"), n, TRUE))
b$c2 <- stats::rnorm(n) + 0.8*b$hidden
b$y <- factor(ifelse(stats::rbinom(n,1,stats::plogis(-.3 + .8*(b$x=="mid") + 1.4*(b$x=="high") +
                                                       .5*(b$z=="v") + 1.1*b$hidden)) == 1, "yes","no"),
              levels = c("no","yes"))
b$num <- round(stats::rnorm(n, 50, 12) + 8*b$hidden)
bdes <- svydesign(~psu, weights = ~w, data = b)

cat("D1  (FIXED in z14-i) the crude block under a prebuilt design is now WEIGHTED\n")
pcell <- function(t, pat, f) { cl <- t[[grep(pat, names(t))[1]]]
  f(cl)[which(as.character(t$levels) == "mid")] }
t_des <- tab_reg(bdes, outcome = "y", predictors = c("x","z"), family = "binomial", empirical = TRUE)
t_wt  <- tab_reg(b, outcome = "y", predictors = c("x","z"), family = "binomial", empirical = TRUE,
                 wt = "w")
t_un  <- tab_reg(b, outcome = "y", predictors = c("x","z"), family = "binomial", empirical = TRUE)
cat(sprintf("      Obs_%%  design %.5f | wt= %.5f | unweighted %.5f  <- z14-i: design == wt\n",
            pcell(t_des, "^Obs_%", get_pct), pcell(t_wt, "^Obs_%", get_pct), pcell(t_un, "^Obs_%", get_pct)))
cat(sprintf("      Obs_OR design %.5f | wt= %.5f | unweighted %.5f\n",
            pcell(t_des, "^Obs_OR", get_or), pcell(t_wt, "^Obs_OR", get_or), pcell(t_un, "^Obs_OR", get_or)))
cat(sprintf("      Model_OR design %.5f | wt= %.5f  <- the model column IS design-weighted\n",
            pcell(t_des, "^Model_OR", get_or), pcell(t_wt, "^Model_OR", get_or)))

cat("D2  (FIXED in z14-i) effect = 'ame' under a design is now the POPULATION-average\n")
g <- function(t) { cl <- t[[grep("^Model_AME", names(t))[1]]]
  unname(get_diff(cl)[which(as.character(t$levels) == "high")]) }
cat(sprintf("      design as data %+.6f | wt = 'w' %+.6f\n",
            g(tab_reg(bdes, outcome="y", predictors=c("x","c2"), family="binomial", effect="ame")),
            g(tab_reg(b, outcome="y", predictors=c("x","c2"), family="binomial", effect="ame",
                      wt="w"))))

rp <- as.svrepdesign(bdes, type = "bootstrap", replicates = 20)
cat("D3  tab_reg(svrepdesign) : ",
    tryCatch({tab_reg(rp, outcome="y", predictors="x", family="binomial"); "OK"},
             error = function(e) paste("ERROR -", sub("\n.*", "", conditionMessage(e)))), "\n")
cat("D4  tab(svrepdesign)     : ",
    tryCatch({tab(rp, x, y, pct = "row"); "OK"},
             error = function(e) paste("ERROR -", sub("\n.*", "", conditionMessage(e)))), "\n")
cat("      weights(rep) dim", paste(dim(as.matrix(weights(rp))), collapse = "x"),
    "| weights(rep, 'sampling') length", length(weights(rp, type = "sampling")), "\n")

for (fn in c("tab_num", "tab_plain", "tab_many")) {
  r <- tryCatch({do.call(fn, list(bdes, quote(x), quote(y))); "OK"},
                error = function(e) paste("ERROR -", sub("\n.*", "", conditionMessage(e))))
  cat(sprintf("D5  %-10s(design) : %s\n", fn, r))
}

cat("D6  (FIXED in z14-i) the effect size follows the weights\n")
tt <- tab(bdes, x, y, pct = "row", test = TRUE)
cv <- function(M) { N <- sum(M); E <- outer(rowSums(M), colSums(M))/N
  sqrt(sum((M-E)^2/E)/(N*(min(dim(M))-1))) }
cat(sprintf("      reported %.5f | unweighted %.5f | weighted (population) %.5f\n",
            get_test(tt)$effect_size, cv(table(b$x, b$y)), cv(as.matrix(stats::xtabs(w ~ x + y, b)))))

cat("D9  (z14-i) a design reaches the p-value; a bare wt= gives a weighted chi2 at the raw n\n")
ta <- tab(b, x, y, wt = w, pct = "row", ci = "cell")
tb <- tab(svydesign(~psu, weights = ~w, data = b), x, y, pct = "row", ci = "cell", test = TRUE)
cat(sprintf("      CIs identical: %s | classic p = %.3g -> survey p = %.3g\n",
            isTRUE(all.equal(get_ci_inf(ta[["yes"]]), get_ci_inf(tb[["yes"]]))),
            get_test(tab(b, x, y, wt = w, pct = "row", test = TRUE))$pvalue, get_test(tb)$pvalue))

# ====================================================================================================
hdr("8. What tab(design) DOES get right today, and by how much the rest is off")
t1 <- tab(bdes, x, y, pct = "row", ci = "cell", test = TRUE)
cl <- t1[["yes"]]
sub <- subset(bdes, x == "low"); m <- svymean(~y, sub)
pw <- coef(m)[["yyes"]]; sw <- SE(m)[["yyes"]]
cat(sprintf("  point estimate : tabxplor %.5f | weighted %.5f | unweighted %.5f  -> WEIGHTED %s\n",
            get_pct(cl)[1], pw, mean(b$y[b$x == "low"] == "yes"),
            isTRUE(all.equal(unname(get_pct(cl)[1]), unname(pw), tolerance = 1e-8))))
cat(sprintf("  whole-table p  : tabxplor %.6g | svychisq %.6g  -> DESIGN-BASED %s\n",
            get_test(t1)$pvalue, svychisq(~x + y, bdes, statistic = "F")$p.value,
            isTRUE(all.equal(unname(get_test(t1)$pvalue),
                             unname(svychisq(~x + y, bdes, statistic = "F")$p.value), tolerance = 1e-8))))
cat(sprintf("  proportion cell CI width : design / tabxplor = x%.2f\n",
            (2*1.96*sw) / (get_ci_sup(cl)[1] - get_ci_inf(cl)[1])))
t2 <- tab(bdes, x, num, ci = "cell"); c2 <- t2[["num"]]
mm <- svyby(~num, ~x, bdes, svymean)
cat(sprintf("  mean cell CI width       : design / tabxplor = x%.2f\n",
            (2*1.96*mm$se[1]) / (get_ci_sup(c2)[1] - get_ci_inf(c2)[1])))
cat(sprintf("  D7: the footer line reads: %s\n", tabxplor:::tab_weight_line(t1, lang = "en")))
