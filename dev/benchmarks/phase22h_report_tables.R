# PURPOSE: Turn the phase22h_*.csv runs into the markdown tables of
#          dev/tabxplor_2.0.0_performance_review.md. Emits text; it never edits the report.
# ROLE: The second half of the Phase 22h harness. phase22h_perf_review.R MEASURES, this one READS --
#       so a number in the report is never typed by hand, and re-running both after a change
#       regenerates the whole grid.
# KEY CONSTRAINTS:
#   - `min_s` is the headline for a per-case comparison (least-noise estimate); `median_s` is kept
#     beside it so a case with a wide spread is visible rather than hidden.
#   - A missing profile is skipped, not faked: the tables show whichever runs exist.
# USAGE (from the package root):
#   Rscript dev/benchmarks/phase22h_report_tables.R > /tmp/tables.md

DIR <- "dev/benchmarks/results_2.0.0"

rd <- function(tag, engine = "current") {
  f <- file.path(DIR, sprintf("phase22h_%s_%s.csv", tag, engine))
  if (!file.exists(f)) return(NULL)
  utils::read.csv(f, stringsAsFactors = FALSE)
}

fmt <- function(x, d = 3) ifelse(is.na(x), "--", formatC(x, format = "f", digits = d))
ratio <- function(a, b) ifelse(is.na(a) | is.na(b) | b <= 0, "--",
                               paste0(formatC(a / b, format = "f", digits = 2), "x"))

md_table <- function(df) {
  hdr <- names(df)
  w   <- vapply(seq_along(df), function(i)
    max(nchar(c(hdr[i], as.character(df[[i]]))), na.rm = TRUE), integer(1))
  pad <- function(v, i) formatC(as.character(v), width = w[i], flag = "-")
  cat("| ", paste(vapply(seq_along(hdr), function(i) pad(hdr[i], i), character(1)), collapse = " | "),
      " |\n", sep = "")
  cat("|", paste(vapply(w, function(k) strrep("-", k + 2L), character(1)), collapse = "|"), "|\n", sep = "")
  for (r in seq_len(nrow(df)))
    cat("| ", paste(vapply(seq_along(hdr), function(i) pad(df[r, i], i), character(1)),
                    collapse = " | "), " |\n", sep = "")
  cat("\n")
}

D  <- rd("desktop"); C1 <- rd("1core"); C2 <- rd("2core")
MT <- rd("desktopMT"); V  <- rd("desktop", "v131"); DZ <- rd("desktopZ")
DC <- rd("desktopC")
stopifnot(!is.null(D))

# --- 1. the profile grid: one row per case, one column per machine profile -----------------
cat("\n### The full grid (min seconds; `--` = the profile did not run that case)\n\n")
base <- D[, c("suite", "case", "min_s", "median_s")]
names(base)[3:4] <- c("desktop_min", "desktop_med")
add <- function(b, x, nm) {
  if (is.null(x)) { b[[nm]] <- NA_real_; return(b) }
  b[[nm]] <- x$min_s[match(b$case, x$case)]
  b
}
base <- add(base, C2, "c2_min"); base <- add(base, C1, "c1_min")
out <- data.frame(
  suite = base$suite, case = base$case,
  `12 cores` = fmt(base$desktop_min), `2 cores` = fmt(base$c2_min),
  `1 core` = fmt(base$c1_min), `1c/12c` = ratio(base$c1_min, base$desktop_min),
  check.names = FALSE, stringsAsFactors = FALSE)
for (s in sort(unique(out$suite))) {
  cat("**Suite ", s, "**\n\n", sep = "")
  md_table(out[out$suite == s, -1, drop = FALSE])
}

# --- 2. suite B read as marginal cost against its own baseline -----------------------------
cat("\n### Suite B as marginal cost (against the `baseline pct=row` row of the same fixture)\n\n")
b <- D[D$suite == "B", ]
for (fxn in unique(b$fixture)) {
  s   <- b[b$fixture == fxn, ]
  ref <- s$min_s[grepl("^baseline", s$case)][1]
  s   <- s[!grepl("^baseline", s$case), ]
  tb  <- data.frame(
    option = sub(" \\[.*\\]$", "", s$case), group = s$group,
    seconds = fmt(s$min_s), `vs baseline` = paste0(fmt(s$min_s - ref), " s"),
    `x baseline` = ratio(s$min_s, ref), check.names = FALSE, stringsAsFactors = FALSE)
  cat("**", fxn, "** (baseline = ", fmt(ref), " s)\n\n", sep = "")
  md_table(tb)
}

# --- 3. data.table threads, from phase22h_threads.R -----------------------------------------
# WARNING: NOT from the desktop/desktopMT pair. Two whole harness runs differ in heap and OpenMP
# pool history by the time a late suite runs, and that difference is larger than the effect being
# measured -- it reported the 8M numeric case 2.8x slower with threads, where the one-condition-
# per-process design reports it unstable. phase22h_threads.R is the design that answers this.
TH <- file.path(DIR, "phase22h_threads.csv")
if (file.exists(TH)) {
  cat("\n### data.table threads: 1 vs the package default\n\n")
  r <- utils::read.csv(TH, stringsAsFactors = FALSE)
  ag <- function(a) stats::aggregate(min_s ~ case, data = r[r$asked == a, ], FUN = min)
  cm <- merge(ag(1L), ag(0L), by = "case", suffixes = c("_1", "_auto"))
  cm <- cm[match(unique(r$case), cm$case), , drop = FALSE]
  md_table(data.frame(
    case = cm$case, `1 thread` = fmt(cm$min_s_1, 4), `auto` = fmt(cm$min_s_auto, 4),
    verdict = ifelse(cm$min_s_auto < cm$min_s_1,
                     paste0(formatC(cm$min_s_1 / cm$min_s_auto, format = "f", digits = 2), "x faster"),
                     paste0(formatC(cm$min_s_auto / cm$min_s_1, format = "f", digits = 2), "x slower")),
    check.names = FALSE, stringsAsFactors = FALSE))
}

# --- 4. the parallel scaling, both profiles ------------------------------------------------
cpar <- rbind(if (!is.null(DC)) cbind(DC[DC$suite == "C", ], prof = "12 cores"),
              if (!is.null(C2)) cbind(C2[C2$suite == "C", ], prof = "2 cores"))
if (!is.null(cpar) && nrow(cpar)) {
  cat("\n### Suite C: parallel scaling\n\n")
  cpar$fx  <- sub(".*\\[(.*)\\]$", "\\1", cpar$case)
  cpar$how <- cpar$group
  for (pr in unique(cpar$prof)) {
    s <- cpar[cpar$prof == pr, ]
    for (fxn in unique(s$fx)) {
      k   <- s[s$fx == fxn, ]
      ser <- k$min_s[k$how == "serial"][1]
      cat("**", pr, ", ", fxn, "**\n\n", sep = "")
      md_table(data.frame(mode = k$how, seconds = fmt(k$min_s),
                          `vs serial` = ratio(ser, k$min_s),
                          check.names = FALSE, stringsAsFactors = FALSE))
    }
  }
}

# --- 5. the 1.3.1 A/B ----------------------------------------------------------------------
if (!is.null(V) && !is.null(DZ)) {
  cat("\n### 1.3.1 (CRAN) vs 2.0.0 (this source), same data, same table\n\n")
  m <- merge(V[, c("case", "min_s")], DZ[, c("case", "min_s")], by = "case",
             suffixes = c("_131", "_200"))
  m <- m[order(m$case), ]
  md_table(data.frame(case = m$case, `1.3.1` = fmt(m$min_s_131), `2.0.0` = fmt(m$min_s_200),
                      speedup = ratio(m$min_s_131, m$min_s_200),
                      check.names = FALSE, stringsAsFactors = FALSE))
  d <- DZ[grepl("defaults", DZ$case), c("case", "min_s")]
  if (nrow(d)) {
    cat("**The 2.0.0 default of the same call** (interval included, which 1.3.1 never computed)\n\n")
    md_table(data.frame(case = d$case, seconds = fmt(d$min_s),
                        check.names = FALSE, stringsAsFactors = FALSE))
  }
}

# --- 6. the 8M grid against the frozen dev references --------------------------------------
cat("\n### Suite G against the frozen 8M references in dev/benchmarks/\n\n")
refs <- list(`07-01 ref` = "dev/benchmarks/baseline.csv",
             `07-08 pre` = "dev/benchmarks/results_2.0.0/before_phase2_8M.csv",
             `07-08 post` = "dev/benchmarks/results_2.0.0/after_rollup_8M.csv")
g <- D[D$suite == "G", c("case", "min_s")]
tb <- data.frame(operation = g$case, stringsAsFactors = FALSE)
for (nm in names(refs)) {
  f <- refs[[nm]]
  v <- rep(NA_real_, nrow(g))
  if (file.exists(f)) {
    r <- utils::read.csv(f, stringsAsFactors = FALSE)
    if ("dataset" %in% names(r)) r <- r[r$dataset == "big_8M", , drop = FALSE]
    v <- r$median_s[match(g$case, r$operation)]
  }
  tb[[nm]] <- fmt(v)
}
tb[["now 1thr"]] <- fmt(g$min_s)
md_table(tb)

# --- 7. the parallel evidence, from phase22h_parallel.R -------------------------------------
prd <- function(tag) { f <- file.path(DIR, sprintf("phase22h_parallel_%s.csv", tag))
                       if (file.exists(f)) utils::read.csv(f, stringsAsFactors = FALSE) }
PD <- prd("desktop"); P4 <- prd("4core"); P2 <- prd("2core")

par_grid <- function(x, label) {                        # speedup vs serial, N (rows) x W (cols)
  if (is.null(x)) return(invisible(NULL))
  g <- x[x$kind == "grid", ]
  if (!nrow(g)) return(invisible(NULL))
  g$W <- as.integer(g$a); g$N <- as.integer(g$b); g$s <- as.numeric(g$c)
  ser <- stats::setNames(g$s[g$W == 0L], g$N[g$W == 0L])
  ws  <- sort(setdiff(unique(g$W), 0L))
  tb  <- data.frame(tables = sort(unique(g$N)), check.names = FALSE)
  for (w in ws) {
    v <- vapply(tb$tables, function(n) { k <- g$s[g$W == w & g$N == n]
      if (length(k)) round(ser[[as.character(n)]] / k[1], 2) else NA_real_ }, 0)
    tb[[paste0(w, " workers")]] <- ifelse(is.na(v), "--", paste0(formatC(v, format = "f", digits = 2), "x"))
  }
  cat("\n### Parallel speedup vs serial -- ", label, "\n\n", sep = "")
  md_table(tb)
}
par_grid(PD, "12 cores"); par_grid(P4, "4 cores"); par_grid(P2, "2 cores")

if (!is.null(PD)) {
  sp <- PD[PD$kind == "spawn", ]
  if (nrow(sp)) { cat("\n### Cold pool spawn (seconds the session is blocked)\n\n")
    tb <- data.frame(workers = sp$a, `12 cores` = fmt(as.numeric(sp$b)), check.names = FALSE)
    for (nm in c("4core", "2core")) { x <- prd(nm)
      tb[[paste0(sub("core", "", nm), " cores")]] <-
        if (is.null(x)) "--" else fmt(as.numeric(x$b[x$kind == "spawn"])[match(sp$a, x$a[x$kind == "spawn"])]) }
    md_table(tb) }

  me <- PD[PD$kind == "mem", ]
  if (nrow(me)) { cat("\n### Pool memory: total RSS of every R process (MB)\n\n")
    md_table(data.frame(workers = me$a, `data MB` = me$b, `pool idle` = me$c,
                        `after a build` = me$d, check.names = FALSE)) }

  sh <- PD[PD$kind == "ship", ]
  if (nrow(sh)) { cat("\n### Per-call cost of shipping the population (4 tables)\n\n")
    w <- unique(sh$a); ser <- sh[sh$a == "0", ]; par <- sh[sh$a != "0", ]
    md_table(data.frame(rows = ser$b, `data MB` = ser$c, serial = fmt(as.numeric(ser$d)),
                        `4 workers` = fmt(as.numeric(par$d)),
                        speedup = ratio(as.numeric(ser$d), as.numeric(par$d)),
                        check.names = FALSE)) }

  sha <- PD[PD$kind == "shape", ]
  if (nrow(sha)) { cat("\n### Which `jmvtab` shape parallelises (12 cores)\n\n")
    s0 <- sha[sha$a == "0", ]; s3 <- sha[sha$a != "0", ]
    md_table(data.frame(shape = s0$b, serial = fmt(as.numeric(s0$c)),
                        `3 workers` = fmt(as.numeric(s3$c[match(s0$b, s3$b)])),
                        speedup = ratio(as.numeric(s0$c), as.numeric(s3$c[match(s0$b, s3$b)])),
                        check.names = FALSE)) }

  ra <- PD[PD$kind == "regax", ]
  if (nrow(ra)) { cat("\n### The three `tab_reg()` parallel axes (12 cores)\n\n")
    r0 <- ra[ra$a == "0", ]
    tb <- data.frame(case = r0$b, serial = fmt(as.numeric(r0$c)), check.names = FALSE)
    for (w in setdiff(unique(ra$a), "0")) { k <- ra[ra$a == w, ]
      v <- as.numeric(k$c[match(r0$b, k$b)])
      tb[[paste0("W=", w)]] <- fmt(v)
      tb[[paste0("W=", w, " gain")]] <- ratio(as.numeric(r0$c), v) }
    md_table(tb) }
}

cat("\n(", nrow(D), " desktop cases; profiles present: ",
    paste(c("desktop", if (!is.null(C2)) "2core", if (!is.null(C1)) "1core",
            if (!is.null(MT)) "desktopMT", if (!is.null(V)) "v131"), collapse = ", "), ")\n", sep = "")
