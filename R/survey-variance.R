# PURPOSE: the DESIGN-BASED variance of a tab() table's cells -- the one quantity Phase 18z14-ii
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
#   interval" (Phase 18s). No new fmt field, no new column attribute. See dev/full_survey_design_scope.md S4.
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
#   - EVERY function answers "no answer" rather than a wrong number (R/reg-influence.R's discipline).
#     The two producers say so through svy_var_out(): `v` NULL, plus the REASON. The leaf reads that as
#     "no design base here", falls back to the flat closed form (and, failing that, the raw n) and
#     records the step in a LOCAL, which becomes the basis "design_partial" it stamps on its own
#     columns -- so a fallen-back table can never be reported as design-based, in any export.
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

# THE row-space rule, extracted in Phase 18z14-iii so it is stated once: where does the i-th row of
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

# THE producers' return type (Phase 18z16-iiiii). They answer with a VALUE or with a REASON, never
# with a bare NULL the caller has to interpret: `list(v = <matrix>, reason = NULL)` on success,
# `list(v = NULL, reason = "size" | NULL)` when there is no answer.
# DESIGN: this return type is what let the process-global degrade environment go. The reason now
#   travels WITH the answer, up the call stack, to the one caller that knows whether it matters (a
#   leaf under a real design) -- instead of being assigned into a mutable env that every later build
#   in the session could read, which is the stale-flag hazard W-C had to patch with a reset in four
#   entry points. A build's own degrade is now a LOCAL of that build, and cannot outlive it.
svy_var_out <- function(v = NULL, reason = NULL) list(v = v, reason = reason)

# The guards both producers open with, plus the group map they both need -- extractable only once the
# return type carries the bail. `list(gm =, R =, K =, nfr =)` on success; a bail (no `gm`) otherwise,
# which the caller returns verbatim as its own answer.
svy_var_setup <- function(prep, keys, n_tab, mkeys, nfr, K) {
  if (is.null(prep) || !length(keys) || !K || !nfr)              return(svy_var_out())
  R <- length(keys[[1]])
  if (R == 0L)                                                   return(svy_var_out())
  if (!all(vapply(mkeys, length, integer(1)) == nfr))            return(svy_var_out())
  if (!is.finite(R * nfr) || R * nfr > 5e7) return(svy_var_out(reason = "size")) # ~400 MB of influence
  if (length(prep$w) != prep$n || max(prep$at) > prep$n)         return(svy_var_out())
  gm <- svy_group_map(keys, n_tab, mkeys)
  if (is.null(gm))                                               return(svy_var_out())
  list(gm = gm, R = R, K = K, nfr = nfr)
}

# THE degrade message (Phase 18z16-i, W4). A console message is not a property of the table:
# suppressMessages(), an Rmd chunk, tab_export(), jamovi's backend all drop it, and what survived was
# a table whose footer asserted, permanently and in every export, something untrue of its numbers. So
# the fall-back also records itself -- as a LOCAL of the build that fell back (z16-iiiii), which the
# leaf resolves into basis "design_partial" and stamps on its own columns. The CLAIM rides the
# numbers; the REASON is a build event, and belongs where the user is when it happens and where it is
# actionable ("too large" says to reduce the table, which an exported footer read months later could
# not act on anyway).
# Returns TRUE, so a caller writes `degraded <- svy_var_degraded(res$reason)`.
# Not throttled, like every other tabxplor per-render notice (CLAUDE.md Phase 18k2: a
# once-per-session throttle was tried and reverted).
svy_var_degraded <- function(reason = NULL) {
  cli::cli_inform(c(
    "!" = if (identical(reason, "size"))
      "This table is too large for the sample design's variance."
    else "The sample design's variance could not be computed for this table.",
    "i" = "Its confidence intervals fall back to the weighting alone."))
  invisible(TRUE)
}

# === SECTION: the flat closed form (ids = ~1) =======================================================
#
# Phase 18z16-ii. A WEIGHT COLUMN IS A SURVEY DESIGN -- the flat one -- and at `ids = ~1`, with no
# strata, no fpc and no calibration, svyrecvar reduces to a plain sum of squares of `w_k z_k` with
# survey's finite-sample factor n/(n-1). Because Sum(w_k z_k) = (A - p B)/B is EXACTLY zero for every
# base, onestrat()'s centering is a no-op, so the whole variance collapses to per-cell sums the
# aggregate core can accumulate in the same pass as Sum(w):
#
#     Var(p_hat) = n/(n-1) * [ A (1-p)^2 + (S - A) p^2 ] / B^2
#     Var(x_bar) = n/(n-1) * [ Sum(w^2 x^2) - 2 x_bar Sum(w^2 x) + x_bar^2 Sum(w^2) ] / B^2
#
#   A = the CELL's own Sum(w^2)   S = Sum(w^2) over the base's domain   B = Sum(w) over that domain
#   n = the design's nPSU, i.e. the number of observations the table is built from
#
# so the weighted basis stops needing the microdata at all: O(cells), composing with the wide-table
# rollup (Sum(w^2) is additive across a partition, so total rows and the Total column get the right
# A / S / B by summation, with no special case), with no size ceiling and nothing that can degrade.
#
# WARNING -- THE BOUNDARY THAT MUST NEVER BE CROSSED: this is the ids = ~1 case and NOTHING else. The
#   rejection of a hand-rolled aggregate variance for real designs (Route C,
#   dev/full_survey_design_scope.md S4.8) stands and does not apply here, precisely because at
#   ids = ~1 there is no svyrecvar to re-implement: no lonely-PSU policy, no multistage fpc, no
#   calibration, no strata. A design with ANY of those goes through the svyrecvar section below.
#
# KISH IS THIS FORMULA WITH ONE INPUT DISCARDED. Write A ~ p*S ("the cell's Sum(w^2) is its
# proportional share of the base's") and the bracket collapses to S*p(1-p), giving n_eff -> B^2/S,
# which is Kish up to the finite-sample factor. That assumption is exactly what unequal weights
# violate: measured, Kish is up to 17 % wrong in EITHER direction once the outcome follows the weight,
# and it cannot move with the outcome at all (it is a property of the weights alone). It survives here
# only as the DEGENERATE-CASE LIMIT (svy_flat_base_neff), which is what it always was.
# See dev/weights_framework_redesign.md S1 and its Appendix A.

# n/(n-1), survey's own finite-sample factor. NA below 2 observations (no variance is defined).
svy_flat_fac <- function(n_obs) {
  n <- suppressWarnings(as.double(n_obs)[1])
  if (!isTRUE(is.finite(n)) || n < 2) return(NA_real_)
  n / (n - 1)
}

# The base domain's own effective n, B^2/S -- the exact effective size of a quantity that carries no
# information about the weights. It is the limit the cell formula tends to when the cell carries no
# information at all (p = 0 or p = 1 give Var = 0, so p(1-p)/Var is 0/0), which is where it is used.
svy_flat_base_neff <- function(B, S) {
  out <- B^2 / S
  out[!is.finite(out) | out <= 0] <- NA_real_
  out
}

# n_eff of every cell PERCENTAGE of a wide factor table, from four matrices aligned cell by cell:
# `P` the DISPLAYED proportions, `A` each cell's own Sum(w^2), `S` / `B` the Sum(w^2) / Sum(w) of its
# percentage base (the SAME broadcast the leaf applies to build `tot_n`). Degenerate cells fall back
# to the base's B^2/S rather than to the raw n.
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

# n_eff of every cell MEAN, the twin: `s2` the ML weighted variance the cell's interval uses, and the
# three per-cell moment sums Sum(w^2), Sum(w^2 x), Sum(w^2 x^2) over the cell's own rows (B = Sum(w)).
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

# The general per-row form, for the callers that hold observations rather than a wide table
# (tab_reg()'s crude grid): p = Sum(w u) / Sum(w v), whose linearized contribution is (u - p v)/B.
# Returns the effective n directly -- p(1-p)/Var for a share, s^2/Var for a mean -- via `num`.
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

# Is this design the flat one the closed form covers? A `survey::svydesign(ids = ~1, weights = ~w)` --
# the shape most users build -- has an algebraic answer, so it never needs an influence matrix, a
# 400 MB ceiling or a microdata pass. Anything with strata, fpc, calibration or real clusters does.
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
# Phase 19m-iii: `tot = "Total"` WAS a parameter here, and `tot_lab = "Total"` in svy_var_prop() --
# neither had a caller that passed anything, so they promised a configurability that did not exist and
# invited the reading that this is a user-facing LABEL leaking into the engine. It is not: "Total" is
# the LEAF's internal pre-rename key (the fourth of the internal names listed in the round-trip DESIGN
# note of R/tab-leaf.R, beside "col_var" / "_colvarbis" / the "n_"-"wn_" value prefixes), minted by
# build_total_rows() / the leaf's total column and only swapped for the user's `total_names` much
# later, in leaf_rename_totals(). Substituting `total_names[1]` here would therefore be a BUG, not a
# fix -- these producers run long before that rename. Stated once, as a literal, like its siblings.
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
# and `col_names` (columns), or NULL. `mcol` is the col_var of each microdata row; the column named
# "Total" is the table's Total column, i.e. "every level" rather than one -- the leaf's internal
# pre-rename key, see svy_group_map() above. One svyrecvar call per column level, each on a
# prepared-rows x R influence matrix (7 MB at 60 000 x 15).
svy_var_prop <- function(prep, keys, n_tab, mkeys, mcol, col_names, base) {
  s <- svy_var_setup(prep, keys, n_tab, mkeys, nfr = length(mcol), K = length(col_names))
  if (is.null(s$gm)) return(s)
  gm <- s$gm; R <- s$R; K <- s$K; nfr <- s$nfr

  wf     <- prep$w[prep$at]
  if (length(wf) != nfr || anyNA(wf))                           return(svy_var_out())
  is_tot <- col_names == "Total"
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

# Var_design of every cell MEAN of a numeric table: an R x K matrix aligned to `keys` and `xs` (a named
# list of numeric microdata columns), or NULL. Same influence function with (u, v) = (x, 1) over the
# row's domain, restricted per variable to its own non-missing rows -- which is the n the leaf's own
# per-cell variance is computed on.
# Phase 18z14-iii: `wmult` is a per-row multiplier on the design weight, so the SAME producer also
# serves tab_reg()'s crude grid, where a grouped-binomial row is a cluster of `trials` Bernoulli draws
# (p = Sum(w*succ) / Sum(w*trials)). With wmult = trials and x = succ/trials the expression below is
# (u - p*v)/B for (u, v) = (succ, trials) -- the general ratio form, not a second formula.
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
