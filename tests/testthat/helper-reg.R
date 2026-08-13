# Shared selectors for REGRESSION tables (Phase 18z13).
#
# `tab_reg(add_n = TRUE)` -- the default since z13 -- puts a per-predictor-level `n` column right after
# the labels, where STROBE reads it and where gtsummary / finalfit put theirs. It is a row descriptor,
# not an estimate, so a test that means "the model's columns" must say so: select by the STORED role,
# never by position. `t[[3]]` and "the first fmt column" both used to mean "the first estimate", and
# they no longer do.
reg_fmt_cols <- function(t) {
  nm <- names(t)[vapply(t, is_fmt, logical(1))]
  nm[vapply(nm, function(n) !identical(get_role(t[[n]]), "n"), logical(1))]
}

reg_first_fmt <- function(t) t[[reg_fmt_cols(t)[[1]]]]

# The MODEL column of one `split_var` group in a spread table. With add_n the spread carries two fmt
# columns per group (`n_<g>` and `<measure>_<g>`), so tab_spread suffixes both and the model column is
# no longer named by the bare group level.
reg_group_col <- function(t, g) {
  nm <- reg_fmt_cols(t)
  hit <- nm[nm == g | endsWith(nm, paste0("_", g))]
  hit[[length(hit)]]
}
