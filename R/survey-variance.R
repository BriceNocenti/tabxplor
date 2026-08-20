# PURPOSE: the DESIGN-BASED variance of a tab() table's cells.
# ROLE: two producers for the two leaves -- plain_core() asks for Var(p_hat) per (row, column level),
#   num_core() for Var(x_bar) per (row, numeric variable). Each becomes an EFFECTIVE SAMPLE SIZE,
#   written into the existing `n_eff` fmt field:
#
#       proportion :  n_eff = p (1 - p) / Var_design(p_hat)
#       mean       :  n_eff = s^2       / Var_design(x_bar)
#
#   That is Korn & Graubard's own device, and it is why this module changes NOTHING downstream: the
#   CI engines, the colour engine, the fmt record and the four exporters already read `n_eff` as "the
#   base for this cell's interval". No new field, no new column attribute.
# DESIGN: ONE influence function, four domain pairs -- not four formulas. Every quantity here is a
#   ratio of two weighted sums, p = A/B with A = sum(u*w) and B = sum(v*w), whose linearized
#   contribution is
#
#       z_k = ( u_k - p * v_k ) / B     Var = svyrecvar(w * z, cluster, strata, fpc, postStrata)
#
#   so `pct` chooses (u, v) -- svy_uv_v() -- it does not choose a formula.
# DESIGN: the row DOMAINS come from the wide table's own key columns, with "Total" read as "every
#   level of this variable". One rule therefore serves a data row, a subtable total row and a
#   total-table row, so total rows get a design-based base with no special case. Matching goes
#   through a group code, so only the influence matrix is ever n-long.
# KEY CONSTRAINTS:
#   - EVERY function answers "no answer" plus a REASON rather than a wrong number. The leaf reads
#     that, falls back to the flat closed form (then to the raw n) and stamps the basis
#     "design_partial", so a fallen-back table can never be reported as design-based, in any export.
#   - `survey` owns the variance algebra: this module builds influence vectors and calls
#     svyrecvar(); it never re-implements strata / clusters / fpc / calibration.
#   - Weights are read as 1/prob, never from a data column -- the calibrated-design-safe form, and it
#     is 0 on the rows survey's domain `[` excluded rather than dropped.
#   - WARNING: "Total" as a key value means "all levels" here, exactly as leaf_totrow_tottab() and
#     build_total_rows() already assume. A user level literally named "Total" is ambiguous throughout
#     the leaf, not only here.
# See: CLAUDE.md § tabxplor architecture (the inference layer); R/survey-design.R (the boundary and
#   the domain helper); R/reg-influence.R (the same algebra for tab_reg()'s gap test).

# === SECTION: the design side =======================================================================

# DESIGN: deliberately NOT through svy_domain_design() -- that helper swaps `$variables` for
#   svychisq() / svyglm(), which svyrecvar() (the only consumer here) never reads. Its warning still
#   applies, and `at` is the answer: the design row each PREPARED row occupies.
svy_var_prep <- function(design, des_rows) {
  if (is.null(design) || is.null(des_rows) || !length(des_rows))              return(NULL)
  des_rows <- as.integer(des_rows)
  if (anyNA(des_rows) || any(des_rows < 1L))                                  return(NULL)
  dd <- tryCatch(design[des_rows, ], error = function(e) NULL)
  if (is.null(dd) || is.null(dd$prob) || !length(dd$prob))                    return(NULL)
  n_dd <- length(dd$prob)
  at   <- svy_row_at(n_dd, des_rows)
  if (is.null(at))                                                            return(NULL)
  w <- 1 / as.numeric(dd$prob)          # Inf prob (survey's domain exclusion) -> weight 0
  w[!is.finite(w)] <- 0
  list(dd = dd, at = at, n = n_dd, w = w)
}

# THE row-space rule, stated once: the design SHRANK -> row i; it did NOT (calibrated / PPS keep all n
# at prob = Inf) -> des_rows[i]. reg_gap_se_columns() scatters its crude influence leg by the same
# rule. NULL = no rule applies, which every caller reads as "no design answer here".
svy_row_at <- function(n_design, des_rows) {
  if (!length(n_design) || !length(des_rows) || anyNA(des_rows)) return(NULL)
  if (n_design == length(des_rows)) return(seq_along(des_rows))
  if (n_design >= max(des_rows))    return(des_rows)
  NULL
}

# THE svyrecvar call, and the ONE place the lonely-PSU policy is answered -- svy_omnibus_one() must
# answer it the same way, or the package says "adjust" for the test and something else for the CIs.
svy_var_recvar <- function(Z, dd) {
  old <- options(survey.lonely.psu = "adjust"); on.exit(options(old), add = TRUE)
  tryCatch(survey::svyrecvar(Z, dd$cluster, dd$strata, dd$fpc, postStrata = dd$postStrata),
           error = function(e) NULL)
}

# The off-diagonal is the cell-to-cell design COVARIANCE, deliberately discarded: it is produced for
# free here, so storing it later would cost nothing that is not already spent.
svy_var_block <- function(Zf, prep) {
  Z <- matrix(0, prep$n, ncol(Zf))
  Z[prep$at, ] <- Zf
  V <- svy_var_recvar(Z, prep$dd)
  if (is.null(V)) return(NULL)
  v <- diag(as.matrix(V))
  if (length(v) != ncol(Zf)) return(NULL)
  v[!is.finite(v) | v < 0] <- NA_real_
  v
}

svy_var_out <- function(v = NULL, reason = NULL) list(v = v, reason = reason)

svy_var_setup <- function(prep, keys, n_tab, mkeys, nfr, K) {
  if (is.null(prep) || !length(keys) || !K || !nfr)              return(svy_var_out())
  R <- length(keys[[1]])
  if (R == 0L)                                                   return(svy_var_out())
  if (!all(vapply(mkeys, length, integer(1)) == nfr))            return(svy_var_out())
  if (!is.finite(R * nfr) || R * nfr > 5e7) return(svy_var_out(reason = "size")) # hard bail: ~400 MB
  if (length(prep$w) != prep$n || max(prep$at) > prep$n)         return(svy_var_out())
  gm <- svy_group_map(keys, n_tab, mkeys)
  if (is.null(gm))                                               return(svy_var_out())
  list(gm = gm, R = R, K = K, nfr = nfr)
}

# The degrade REASON is a build event, so it travels with the answer as a LOCAL of that build (which
# the leaf resolves into basis "design_partial"), never as a process-global a later build could read.
svy_var_degraded <- function(reason = NULL) {
  cli::cli_inform(c(
    "!" = if (identical(reason, "size"))
      "This table is too large for the sample design's variance."
    else "The sample design's variance could not be computed for this table.",
    "i" = "Its confidence intervals fall back to the weighting alone."))
  invisible(TRUE)
}

# === SECTION: the flat closed form (ids = ~1) =======================================================

# A WEIGHT COLUMN IS A SURVEY DESIGN -- the flat one -- and at ids = ~1 svyrecvar reduces to per-cell
# sums the aggregate core accumulates: no microdata, and additive, so total rows need no special case.
#
# ⚠ THE BOUNDARY THAT MUST NEVER BE CROSSED: ids = ~1 and NOTHING else -- there is no svyrecvar to
#   re-implement here (no lonely-PSU policy, no multistage fpc, no calibration, no strata). A design
#   with any of those goes through the svyrecvar path instead.
#
# KISH IS THIS FORMULA WITH ONE INPUT DISCARDED: assume A ~ p*S and n_eff collapses to B^2/S -- which
# unequal weights violate, and which cannot move with the outcome. It survives only as its limit.

# NA below 2 observations (no variance is defined).
svy_flat_fac <- function(n_obs) {
  n <- suppressWarnings(as.double(n_obs)[1])
  if (!isTRUE(is.finite(n)) || n < 2) return(NA_real_)
  n / (n - 1)
}

svy_flat_base_neff <- function(B, S) {
  out <- B^2 / S
  out[!is.finite(out) | out <= 0] <- NA_real_
  out
}

# `P` the DISPLAYED proportions, `A` each cell's own Sum(w^2), `S`/`B` its percentage base's sums.
svy_flat_neff_prop <- function(P, A, S, B, n_obs) {
  fac <- svy_flat_fac(n_obs)
  V   <- fac * (A * (1 - P)^2 + (S - A) * P^2) / B^2
  ne  <- P * (1 - P) / V
  fb  <- svy_flat_base_neff(B, S)
  ne[!is.finite(ne) | ne <= 0] <- NA_real_
  ne[is.na(ne)] <- fb[is.na(ne)]
  ne[!is.finite(ne) | ne <= 0] <- NA_real_
  ne
}

svy_flat_neff_mean <- function(M, s2, W2, W2X, W2X2, B, n_obs) {
  fac <- svy_flat_fac(n_obs)
  V   <- fac * (W2X2 - 2 * M * W2X + M^2 * W2) / B^2
  ne  <- s2 / V
  fb  <- svy_flat_base_neff(B, W2)
  ne[!is.finite(ne) | ne <= 0] <- NA_real_
  ne[is.na(ne)] <- fb[is.na(ne)]
  ne[!is.finite(ne) | ne <= 0] <- NA_real_
  ne
}

# The general per-row form: `num` is the numerator of the effective n, p(1-p) for a share.
svy_flat_neff_rows <- function(w, u, v, n_obs, num = NULL) {
  B <- sum(w * v)
  if (!isTRUE(is.finite(B)) || B <= 0) return(NA_real_)
  p   <- sum(w * u) / B
  fac <- svy_flat_fac(n_obs)
  V   <- fac * sum((w * (u - p * v))^2) / B^2
  ne  <- (num %||% (p * (1 - p))) / V
  if (isTRUE(is.finite(ne) && ne > 0)) return(ne)
  svy_flat_base_neff(B, sum(w^2))
}

svy_design_is_flat <- function(design) {
  if (is.null(design) || !inherits(design, "survey.design")) return(FALSE)
  if (!is.null(design$postStrata) && length(design$postStrata))  return(FALSE)
  if (!is.null(design$fpc$popsize))                              return(FALSE)
  st <- design$strata
  if (!is.null(st) && length(st) && length(unique(st[[1]])) > 1L) return(FALSE)
  cl <- design$cluster
  if (is.null(cl) || !length(cl)) return(FALSE)
  if (ncol(cl) != 1L) return(FALSE)
  !anyDuplicated(cl[[1]])          # ids = ~1 <=> one "cluster" per row
}

# === SECTION: the row domains =======================================================================

# Key values as the wide table and the microdata BOTH spell them, NA read as the leaf's "NA" level.
svy_key_chr <- function(x) {
  x <- as.character(x)
  x[is.na(x)] <- "NA"
  x
}

# Returns a group code per microdata row, `in_dom` / `in_sub` (the wide row's own domain, then its
# subtable) as R x L masks, and `any_dom`, the groups reaching a wide row (pct = "all_tabs" totals).
# ⚠ "Total" here is the LEAF's internal pre-rename key, minted by build_total_rows() and only swapped
#   for the user's `total_names` much later (leaf_rename_totals) -- these producers run long before
#   that rename, so substituting total_names[1] here would be a BUG, not a fix.
svy_group_map <- function(keys, n_tab, mkeys) {
  nk <- length(keys)
  if (!nk || nk != length(mkeys)) return(NULL)
  R <- length(keys[[1]])
  mk    <- do.call(paste, c(mkeys, list(sep = "\r")))
  first <- !duplicated(mk)
  lev   <- mk[first]
  gcode <- match(mk, lev)
  U     <- lapply(mkeys, function(v) v[first])
  one   <- function(idx) {
    if (!length(idx)) return(matrix(TRUE, R, length(lev)))
    Reduce(`&`, lapply(idx, function(k)
      outer(keys[[k]], U[[k]], function(a, b) a == "Total" | a == b)))
  }
  in_dom <- one(seq_len(nk))
  list(gcode = gcode, in_dom = in_dom, in_sub = one(seq_len(n_tab)),
       any_dom = apply(in_dom, 2L, any), L = length(lev))
}

# The denominator domain of each base -- the ONLY place `pct` enters this module, and the mirror of
# leaf_wide_pct()'s Dmat() selector.
svy_uv_v <- function(base, d, s, uj, valid) {
  switch(base,
         "row"      = d & valid,
         "col"      = s & valid & uj,
         "all"      = s & valid,
         "all_tabs" = valid,
         NULL)
}

# === SECTION: the two producers =====================================================================

svy_var_prop <- function(prep, keys, n_tab, mkeys, mcol, col_names, base) {
  s <- svy_var_setup(prep, keys, n_tab, mkeys, nfr = length(mcol), K = length(col_names))
  if (is.null(s$gm)) return(s)
  gm <- s$gm; R <- s$R; K <- s$K; nfr <- s$nfr

  wf     <- prep$w[prep$at]
  if (length(wf) != nfr || anyNA(wf))                           return(svy_var_out())
  is_tot <- col_names == "Total"
  # under na = "drop" the NA column leaves the table, so its rows must leave the totals too.
  valid  <- gm$any_dom[gm$gcode] & (mcol %in% col_names[!is_tot])
  gsum   <- function(x) as.vector(rowsum(x, gm$gcode, reorder = TRUE))
  gw     <- gsum(wf * valid)
  gwj    <- vapply(seq_len(K), function(j)
    gsum(wf * valid * if (is_tot[[j]]) 1 else (mcol == col_names[[j]])), numeric(gm$L))
  gwj    <- matrix(gwj, nrow = gm$L, ncol = K)

  num <- gm$in_dom %*% gwj
  den <- switch(base,
                "row"      = matrix(as.vector(gm$in_dom %*% gw), R, K),
                "col"      = gm$in_sub %*% gwj,
                "all"      = matrix(as.vector(gm$in_sub %*% gw), R, K),
                "all_tabs" = matrix(sum(gw), R, K),
                NULL)
  if (is.null(den)) return(svy_var_out())
  P <- num / den

  out <- matrix(NA_real_, R, K)
  for (j in seq_len(K)) {
    uj <- if (is_tot[[j]]) rep(TRUE, nfr) else (mcol == col_names[[j]])
    Zf <- vapply(seq_len(R), function(i) {
      B <- den[i, j]
      if (!isTRUE(is.finite(B)) || B <= 0) return(rep(0, nfr))
      d <- gm$in_dom[i, gm$gcode]
      v <- svy_uv_v(base, d, gm$in_sub[i, gm$gcode], uj, valid)
      wf * (as.numeric(d & valid & uj) - P[i, j] * as.numeric(v)) / B
    }, numeric(nfr))
    v <- svy_var_block(matrix(Zf, nrow = nfr, ncol = R), prep)
    if (is.null(v)) return(svy_var_out())
    out[, j] <- v
  }
  svy_var_out(out)
}

# The same influence function at (u, v) = (x, 1), per variable's own non-missing rows. `wmult`
# multiplies the design weight, so the SAME producer serves tab_reg()'s crude grid: wmult = trials
# with x = succ/trials IS (u, v) = (succ, trials), the general ratio form, not a second formula.
svy_var_mean <- function(prep, keys, n_tab, mkeys, xs, wmult = NULL) {
  K <- length(xs)
  s <- svy_var_setup(prep, keys, n_tab, mkeys, nfr = if (K) length(xs[[1]]) else 0L, K = K)
  if (is.null(s$gm)) return(s)
  gm <- s$gm; R <- s$R; nfr <- s$nfr

  wf <- prep$w[prep$at]
  if (!is.null(wmult)) {
    if (length(wmult) != length(wf) || anyNA(wmult))            return(svy_var_out())
    wf <- wf * as.numeric(wmult)
  }
  if (length(wf) != nfr || anyNA(wf))                           return(svy_var_out())
  base_ok <- gm$any_dom[gm$gcode]
  gsum    <- function(x) as.vector(rowsum(x, gm$gcode, reorder = TRUE))

  out <- matrix(NA_real_, R, K)
  for (j in seq_len(K)) {
    x  <- as.numeric(xs[[j]])
    if (length(x) != nfr) return(svy_var_out())
    ok <- base_ok & !is.na(x)
    xz <- ifelse(ok, x, 0)
    B  <- as.vector(gm$in_dom %*% gsum(wf * ok))
    M  <- as.vector(gm$in_dom %*% gsum(wf * xz)) / B
    Zf <- vapply(seq_len(R), function(i) {
      if (!isTRUE(is.finite(B[[i]])) || B[[i]] <= 0) return(rep(0, nfr))
      d <- gm$in_dom[i, gm$gcode] & ok
      wf * d * (x - M[[i]]) / B[[i]]
    }, numeric(nfr))
    v <- svy_var_block(matrix(Zf, nrow = nfr, ncol = R), prep)
    if (is.null(v)) return(svy_var_out())
    out[, j] <- v
  }
  svy_var_out(out)
}
