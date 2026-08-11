# PURPOSE: the DESIGN-BASED variance of a tab() table's cells -- the one quantity Last Phase z14-ii
#   needs, and the only new statistics in the whole of z14.
# ROLE: producers for the two leaves. plain_core() asks for Var(p_hat) per (row, column level),
#   num_core() for Var(x_bar) per (row, numeric variable); each divides its own displayed estimate by
#   that variance to get an EFFECTIVE SAMPLE SIZE, and writes it into the existing `n_eff` fmt field:
#
#       proportion :  n_eff = p (1 - p) / Var_design(p_hat)
#       mean       :  n_eff = s^2       / Var_design(x_bar)
#
#   That is Korn & Graubard's (1998) own device -- survey::svyciprop(method = "beta") is defined as
#   binom.test "with an effective sample size based on the estimated variance of the proportion" -- and
#   it is why this module changes NOTHING downstream: tab_ci(), the nine ci_* engines, the colour
#   engine, the fmt record and the four exporters already read `n_eff` as "the base for this cell's
#   interval" (Last Phase s). No new fmt field, no new column attribute. See dev/full_survey_design_scope.md S4.
# DESIGN: ONE influence function, four domain pairs -- not four influence functions. Every quantity
#   here is a ratio of two weighted sums, p = A/B with A = sum(u*w) and B = sum(v*w), whose linearized
#   influence contribution is
#
#       z_k = ( u_k - p * v_k ) / B          Var = svyrecvar(w * z, cluster, strata, fpc, postStrata)
#
#   so `pct` chooses (u, v), it does not choose a formula (svy_uv_v()). Verified against survey itself
#   in dev/survey_design_measurements.R blocks 4 and 6: max relative error 0 uncalibrated, 2.22e-16
#   CALIBRATED, and the difference SE to ratio 1.000000000.
# DESIGN: the row DOMAINS come from the wide table's own key columns, with "Total" read as "every level
#   of this variable". One rule therefore serves a data row, a subtable total row and a total-table row
#   -- so total rows get a design-based base with no special case. Matching goes through a group code
#   (the distinct key tuples, a few hundred at most), so the per-cell weighted sums are small matrix
#   products and only the influence matrix is ever n-long.
# KEY CONSTRAINTS:
#   - EVERY function returns NULL rather than a wrong number (R/reg-influence.R's discipline). The leaf
#     reads a NULL as "no design base here" and falls back to Kish / the raw n, which is what the
#     package did before z14-ii -- a degraded design is never silently reported as design-based.
#   - `survey` owns the variance algebra. This module builds influence vectors and calls svyrecvar();
#     it never re-implements strata / clusters / fpc / calibration (Route C, rejected, S4.8).
#   - Weights are read as 1/prob, never from a data column: that is the calibrated-design-safe form,
#     and it is 0 on the rows survey's domain `[` excluded rather than dropped (see svy_var_prep()).
#   - WARNING: "Total" as a key value means "all levels" here, exactly as leaf_totrow_tottab() and
#     build_total_rows() already assume. A user level literally named "Total" is ambiguous throughout
#     the leaf, not only here.
# See: dev/full_survey_design_scope.md S4 (Route A, the measurements) and S10 (the roadmap);
#   R/survey-design.R (the boundary and the domain helper); R/reg-influence.R (the same algebra for
#   tab_reg()'s gap test, and the package's other svyrecvar caller).

# === SECTION: the design side =======================================================================

# Restrict a design to the rows the leaf actually holds and return everything the influence machinery
# needs: the domain design, the design weights, and `at` -- the position each PREPARED row occupies in
# that design. `des_rows` is `.svy_row`, the position of each prepared row in the ORIGINAL design.
# DESIGN: this deliberately does NOT go through svy_domain_design(). That helper swaps `$variables`
#   because svychisq() / svyglm() read their data off the design; svyrecvar() never does, and it is the
#   only consumer here. Its WARNING still applies, which is what `at` exists for: `[` does NOT drop
#   rows on a CALIBRATED or PPS design -- it keeps all n and sets prob = Inf (weight 0). So `at` is
#   seq_along(des_rows) when the design shrank, and des_rows itself when it did not; either way the
#   influence matrix is scattered into the design's own row space and the excluded rows stay zero.
svy_var_prep <- function(design, des_rows) {
  if (is.null(design) || is.null(des_rows) || !length(des_rows))              return(NULL)
  if (inherits(design, c("svyrep.design", "twophase", "twophase2")))          return(NULL)
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

# THE row-space rule, extracted in Last Phase z14-iii so it is stated once: where does the i-th row of
# a frame sit in a design of `n_design` rows, given `des_rows` (its `.svy_row` positions)?
#   the design SHRANK   -> it holds exactly these rows, in order      -> i
#   it did NOT shrink   -> it kept all n and set prob = Inf outside   -> des_rows[i]
# Second consumer: reg_gap_se_columns() scatters its closed-form crude influence leg with the same
# rule, because svyglm keeps a padded design's zero-weight rows in model.matrix() while the crude leg
# is built on the complete-case frame. NULL = no rule applies (the design is smaller than the rows
# asked for), which every caller reads as "no design answer here".
svy_row_at <- function(n_design, des_rows) {
  if (!length(n_design) || !length(des_rows) || anyNA(des_rows)) return(NULL)
  if (n_design == length(des_rows)) return(seq_along(des_rows))
  if (n_design >= max(des_rows))    return(des_rows)
  NULL
}

# THE svyrecvar call -- the one place the package's crosstab variance is computed, and the one place
# the lonely-PSU policy is answered (svy_omnibus_one() answers it the same way; the package must not
# say "adjust" for the omnibus test and something else for the intervals).
svy_var_recvar <- function(Z, dd) {
  old <- options(survey.lonely.psu = "adjust"); on.exit(options(old), add = TRUE)
  tryCatch(survey::svyrecvar(Z, dd$cluster, dd$strata, dd$fpc, postStrata = dd$postStrata),
           error = function(e) NULL)
}

# Scatter a prepared-rows x R influence matrix into the design's row space, call svyrecvar, and keep
# the diagonal (the per-cell variances). The off-diagonal is the cell-to-cell design COVARIANCE, which
# Route A deliberately discards (ruling Q3, S4.5): it is produced for free here, so storing it later
# costs nothing that is being spent now.
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

# The one message a fallback owes the user. The footer sentence is blanket -- any table built from a
# design says it is design-based (ruling Q7) -- so a table whose variance could NOT be computed has to
# say so somewhere, or the sentence is silently untrue. Not throttled, like every other tabxplor
# per-render notice (CLAUDE.md Last Phase k2: a once-per-session throttle was tried and reverted).
svy_var_degraded <- function() {
  cli::cli_inform(c(
    "!" = "The sample design's variance could not be computed for this table.",
    "i" = "Its confidence intervals fall back to the unweighted sample size."))
}

# === SECTION: the row domains =======================================================================

# Key values as the wide table and the microdata BOTH spell them: character, with NA read as the "NA"
# level the leaf gives it (forcats::fct_na_value_to_level under na = "keep"). Under na = "drop" the
# wide table has no NA row, so an NA microdata row then matches no domain -- which is correct, and is
# also why svy_group_map() reports `any_dom`.
svy_key_chr <- function(x) {
  x <- as.character(x)
  x[is.na(x)] <- "NA"
  x
}

# Map the microdata onto the wide table's rows. `keys` is the wide table's key columns (tab_vars
# first, then row_var), `mkeys` the same columns of the microdata, `n_tab` how many leading keys are
# tab_vars. Returns the group code of each microdata row plus two small R x L membership matrices:
#   in_dom -- the wide row's own domain (all keys, "Total" = every level)
#   in_sub -- its SUBTABLE (the tab_vars keys only), the denominator domain of pct = "col" / "all"
# plus `any_dom`, the groups that belong to at least one wide row -- i.e. the rows the displayed table
# is actually built on, which is what pct = "all_tabs" totals over.
svy_group_map <- function(keys, n_tab, mkeys, tot = "Total") {
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
      outer(keys[[k]], U[[k]], function(a, b) a == tot | a == b)))
  }
  in_dom <- one(seq_len(nk))
  list(gcode = gcode, in_dom = in_dom, in_sub = one(seq_len(n_tab)),
       any_dom = apply(in_dom, 2L, any), L = length(lev))
}

# The denominator domain of each base -- the ONLY place `pct` enters this module, and the mirror of
# leaf_wide_pct()'s Dmat() selector (row -> the row's Total, col -> the subtable's total row, all ->
# the subtable's grand cell, all_tabs -> the whole table's). `d`/`s` are the domain / subtable masks of
# one wide row, `uj` the column-level mask, `valid` the rows the displayed table is built on.
svy_uv_v <- function(base, d, s, uj, valid) {
  switch(base,
         "row"      = d & valid,
         "col"      = s & valid & uj,
         "all"      = s & valid,
         "all_tabs" = valid,
         NULL)
}

# === SECTION: the two producers =====================================================================

# Var_design of every cell PERCENTAGE of a wide factor table: an R x K matrix aligned to `keys` (rows)
# and `col_names` (columns), or NULL. `mcol` is the col_var of each microdata row; a column named like
# `tot_lab` is the table's Total column, i.e. "every level" rather than one. One svyrecvar call per
# column level, each on a prepared-rows x R influence matrix (7 MB at 60 000 x 15).
svy_var_prop <- function(prep, keys, n_tab, mkeys, mcol, col_names, base, tot_lab = "Total") {
  if (is.null(prep) || !length(keys) || !length(col_names))     return(NULL)
  R <- length(keys[[1]]); K <- length(col_names); nfr <- length(mcol)
  if (R == 0L || nfr == 0L)                                     return(NULL)
  if (!all(vapply(mkeys, length, integer(1)) == nfr))           return(NULL)
  if (!is.finite(R * nfr) || R * nfr > 5e7)                     return(NULL)   # ~400 MB of influence
  if (length(prep$w) != prep$n || max(prep$at) > prep$n)        return(NULL)
  gm <- svy_group_map(keys, n_tab, mkeys); if (is.null(gm)) return(NULL)

  wf     <- prep$w[prep$at]
  if (length(wf) != nfr || anyNA(wf))                           return(NULL)
  is_tot <- col_names == tot_lab
  # rows the displayed table is built on: a key tuple that reaches some wide row, and a col_var level
  # that reaches some wide column (the NA column is dropped from the table under na = "drop", so its
  # observations must leave the row and column totals too).
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
  if (is.null(den)) return(NULL)
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
    if (is.null(v)) return(NULL)
    out[, j] <- v
  }
  out
}

# Var_design of every cell MEAN of a numeric table: an R x K matrix aligned to `keys` and `xs` (a named
# list of numeric microdata columns), or NULL. Same influence function with (u, v) = (x, 1) over the
# row's domain, restricted per variable to its own non-missing rows -- which is the n the leaf's own
# per-cell variance is computed on.
# Last Phase z14-iii: `wmult` is a per-row multiplier on the design weight, so the SAME producer also
# serves tab_reg()'s crude grid, where a grouped-binomial row is a cluster of `trials` Bernoulli draws
# (p = Sum(w*succ) / Sum(w*trials)). With wmult = trials and x = succ/trials the expression below is
# (u - p*v)/B for (u, v) = (succ, trials) -- the general ratio form, not a second formula.
svy_var_mean <- function(prep, keys, n_tab, mkeys, xs, wmult = NULL) {
  if (is.null(prep) || !length(keys) || !length(xs))            return(NULL)
  R <- length(keys[[1]]); K <- length(xs); nfr <- length(xs[[1]])
  if (R == 0L || nfr == 0L)                                     return(NULL)
  if (!all(vapply(mkeys, length, integer(1)) == nfr))           return(NULL)
  if (!is.finite(R * nfr) || R * nfr > 5e7)                     return(NULL)
  if (length(prep$w) != prep$n || max(prep$at) > prep$n)        return(NULL)
  gm <- svy_group_map(keys, n_tab, mkeys); if (is.null(gm)) return(NULL)

  wf <- prep$w[prep$at]
  if (!is.null(wmult)) {
    if (length(wmult) != length(wf) || anyNA(wmult))            return(NULL)
    wf <- wf * as.numeric(wmult)
  }
  if (length(wf) != nfr || anyNA(wf))                           return(NULL)
  base_ok <- gm$any_dom[gm$gcode]
  gsum    <- function(x) as.vector(rowsum(x, gm$gcode, reorder = TRUE))

  out <- matrix(NA_real_, R, K)
  for (j in seq_len(K)) {
    x  <- as.numeric(xs[[j]])
    if (length(x) != nfr) return(NULL)
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
    if (is.null(v)) return(NULL)
    out[, j] <- v
  }
  out
}
