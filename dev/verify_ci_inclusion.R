# PURPOSE: Phase 3a verification. Validate the closed-form CI primitives against DescTools,
#          and quantify the "universal CI-inclusion" star strategy:
#            (a) proportion diff  -- Newcombe-10 inclusion at .90/.95/.99  vs  score-test stars
#            (b) means            -- Welch-t inclusion  vs  t.test(var.equal = FALSE)
#          Read before wiring the primitives into R/tab-agg.R. Not a testthat test; run manually:
#            Rscript dev/verify_ci_inclusion.R
# See: CLAUDE.md > 2.0.0 roadmap > Phase 3; dev/tabxplor_2.0.0_decisions.md §20.

options(width = 130)
z_of  <- function(cl) stats::qnorm(1 - (1 - cl) / 2)

# --- closed-form primitives (candidates for R/tab-agg.R) -----------------------------------
wilson <- function(x, n, cl) {
  z <- z_of(cl); p <- x / n; d <- 1 + z^2 / n
  ctr  <- (p + z^2 / (2 * n)) / d
  half <- (z / d) * sqrt(p * (1 - p) / n + z^2 / (4 * n^2))
  list(inf = ctr - half, sup = ctr + half)
}

ci_ac <- function(x1, n1, x2, n2, cl) {              # Agresti-Caffo (symmetric pivot)
  z <- z_of(cl)
  p1 <- (x1 + 1) / (n1 + 2); p2 <- (x2 + 1) / (n2 + 2)
  d  <- p1 - p2
  se <- sqrt(p1 * (1 - p1) / (n1 + 2) + p2 * (1 - p2) / (n2 + 2))
  list(inf = d - z * se, sup = d + z * se, se = se, est = d)
}

ci_newcombe <- function(x1, n1, x2, n2, cl) {         # Newcombe method 10 (score/hybrid)
  p1 <- x1 / n1; p2 <- x2 / n2; d <- p1 - p2
  w1 <- wilson(x1, n1, cl); w2 <- wilson(x2, n2, cl)
  list(inf = d - sqrt((p1 - w1$inf)^2 + (w2$sup - p2)^2),
       sup = d + sqrt((w1$sup - p1)^2 + (p2 - w2$inf)^2))
}

score_test_p <- function(x1, n1, x2, n2) {            # uncorrected two-proportion score test
  p1 <- x1 / n1; p2 <- x2 / n2
  pbar <- (x1 + x2) / (n1 + n2)
  se <- sqrt(pbar * (1 - pbar) * (1 / n1 + 1 / n2))
  if (se == 0) return(NA_real_)
  2 * stats::pnorm(-abs((p1 - p2) / se))
}

stars_of_p   <- function(p, lv = c(.10, .05, .01))
  if (is.na(p)) 0L else sum(p < lv)                    # 0..3
stars_of_incl <- function(inf_sup_fun) {              # count levels whose interval excludes 0
  sum(vapply(c(.90, .95, .99), function(cl) {
    ci <- inf_sup_fun(cl); (ci$inf > 0) || (ci$sup < 0)
  }, logical(1)))
}

has_desc <- requireNamespace("DescTools", quietly = TRUE)
cat("DescTools available:", has_desc, "\n\n")

# === 1. Wilson closed form vs DescTools::BinomCI(method='wilson') ==========================
if (has_desc) {
  err <- 0
  for (n in c(20, 50, 100, 400)) for (x in unique(round(c(0, .1, .3, .5, .8, 1) * n))) {
    my <- wilson(x, n, .95)
    dt <- DescTools::BinomCI(x, n, conf.level = .95, method = "wilson")
    err <- max(err, abs(my$inf - dt[, "lwr.ci"]), abs(my$sup - dt[, "upr.ci"]))
  }
  cat(sprintf("[1] Wilson cell   vs BinomCI(wilson):     max abs err = %.2e  %s\n",
              err, if (err < 1e-10) "OK" else "**MISMATCH**"))
}

# === 2. AC / Newcombe closed forms vs DescTools::BinomDiffCI ===============================
if (has_desc) {
  find_method <- function(fun, methods) {
    grid <- expand.grid(x1 = c(5, 20, 45), n1 = 50, x2 = c(5, 25, 48), n2 = 60)
    res <- sapply(methods, function(m) {
      e <- 0
      for (i in seq_len(nrow(grid))) {
        g <- grid[i, ]
        my <- fun(g$x1, g$n1, g$x2, g$n2, .95)
        dt <- try(DescTools::BinomDiffCI(g$x1, g$n1, g$x2, g$n2, conf.level = .95, method = m),
                  silent = TRUE)
        if (inherits(dt, "try-error")) { e <- NA; break }
        e <- max(e, abs(my$inf - dt[, "lwr.ci"]), abs(my$sup - dt[, "upr.ci"]))
      }
      e
    })
    res
  }
  ac_methods  <- c("ac")
  nc_methods  <- c("score", "scorecc", "mn", "mee", "wald", "waldcc", "hal", "jp", "blj", "ha")
  cat("\n[2] AC closed form vs BinomDiffCI methods (max abs err):\n")
  print(round(find_method(ci_ac, ac_methods), 12))
  cat("\n[2] Newcombe closed form vs BinomDiffCI methods (which string matches?):\n")
  print(sort(round(find_method(ci_newcombe, nc_methods), 12)))
}

# === 3. score_test_p vs prop.test(correct = FALSE) ========================================
{
  e <- 0
  for (g in list(c(10, 50, 20, 60), c(5, 40, 30, 45), c(25, 100, 25, 100), c(1, 30, 15, 30))) {
    p_my <- score_test_p(g[1], g[2], g[3], g[4])
    p_pt <- suppressWarnings(
      stats::prop.test(c(g[1], g[3]), c(g[2], g[4]), correct = FALSE)$p.value)
    e <- max(e, abs(p_my - p_pt))
  }
  cat(sprintf("\n[3] score test    vs prop.test(correct=FALSE): max abs err = %.2e  %s\n",
              e, if (e < 1e-10) "OK" else "**MISMATCH**"))
}

# === 4. Newcombe-inclusion stars vs score-test stars (the headline question) ================
grid <- expand.grid(n1 = c(20, 30, 50, 100, 200, 500),
                    n2 = c(20, 30, 50, 100, 200, 500),
                    p1 = seq(.05, .95, .05),
                    p2 = seq(.05, .95, .05))
grid$x1 <- round(grid$p1 * grid$n1); grid$x2 <- round(grid$p2 * grid$n2)
grid <- grid[grid$x1 > 0 & grid$x1 < grid$n1 & grid$x2 > 0 & grid$x2 < grid$n2, ]

s_test <- integer(nrow(grid)); s_nc <- integer(nrow(grid)); s_ac <- integer(nrow(grid))
for (i in seq_len(nrow(grid))) {
  g <- grid[i, ]
  s_test[i] <- stars_of_p(score_test_p(g$x1, g$n1, g$x2, g$n2))
  s_nc[i]   <- stars_of_incl(function(cl) ci_newcombe(g$x1, g$n1, g$x2, g$n2, cl))
  s_ac[i]   <- stars_of_incl(function(cl) ci_ac(g$x1, g$n1, g$x2, g$n2, cl))
}
cat(sprintf("\n[4] Proportion diff, %d configurations:\n", nrow(grid)))
cat(sprintf("    Newcombe-inclusion == score-test stars : %.2f%%  (|diff|>=2 bins: %.3f%%)\n",
            100 * mean(s_nc == s_test), 100 * mean(abs(s_nc - s_test) >= 2)))
cat(sprintf("    AC-inclusion       == score-test stars : %.2f%%  (|diff|>=2 bins: %.3f%%)\n",
            100 * mean(s_ac == s_test), 100 * mean(abs(s_ac - s_test) >= 2)))
cat("    Newcombe vs score disagreement direction (neg = NC more conservative):\n")
print(table(sign(s_nc - s_test)))

# === 5. Means: Welch-t pivot inclusion vs t.test(var.equal = FALSE) ========================
ci_pivot <- function(est, se, df, cl) {              # candidate primitive
  q <- if (is.infinite(df)) z_of(cl) else stats::qt(1 - (1 - cl) / 2, df)
  list(inf = est - q * se, sup = est + q * se, p = 2 * stats::pt(-abs(est / se), df))
}
set.seed(1)
e_ci <- 0; e_p <- 0
for (rep in 1:200) {
  n1 <- sample(5:60, 1); n2 <- sample(5:60, 1)
  a <- stats::rnorm(n1, 10, 3); b <- stats::rnorm(n2, 12, 5)
  m1 <- mean(a); m2 <- mean(b); v1 <- stats::var(a); v2 <- stats::var(b)
  se <- sqrt(v1 / n1 + v2 / n2); d <- m1 - m2
  df <- se^4 / ((v1 / n1)^2 / (n1 - 1) + (v2 / n2)^2 / (n2 - 1))
  my <- ci_pivot(d, se, df, .95)
  tt <- stats::t.test(a, b, var.equal = FALSE)
  e_ci <- max(e_ci, abs(my$inf - tt$conf.int[1]), abs(my$sup - tt$conf.int[2]))
  e_p  <- max(e_p, abs(my$p - tt$p.value))
}
cat(sprintf("\n[5] Welch-t pivot vs t.test(var.equal=FALSE): CI err = %.2e, p err = %.2e  %s\n",
            e_ci, e_p, if (max(e_ci, e_p) < 1e-9) "OK" else "**MISMATCH**"))

cat("\nDone.\n")
