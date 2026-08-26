# PURPOSE: THE SHAPE OF A NUMERIC VARIABLE -- how a number enters a table or a model when it is not
#   wanted as one raw slope or one raw mean.
# ROLE: one vocabulary for both producers. `tab(shape =)` and `tab_reg(shape =)` are the same
#   argument reading the same table: cut a number into quantile groups or mean/SD bands (it becomes
#   an ordinary factor, and every subsystem downstream keeps reading a factor), or transform it and
#   keep it a number. What a fit adds on top -- the quadratic TERM -- stays in R/reg-assumptions.R,
#   beside the check whose cure it is.
# KEY CONSTRAINTS:
#   - VAR_SHAPES is the vocabulary. A shape's `producers` is what makes a refusal DERIVED: `tab()`
#     fits no model, so it cannot take `"quadratic"`, and the message says so from the row rather
#     than from a hand-written clause.
#   - A CUT IS APPLIED ONCE, on the whole population, before any split. Both producers do it at
#     their argument boundary, so every sub-table / sub-model is cut at the SAME places; a per-group
#     quantile would silently compare different things.
#   - THE BREAKS RIDE OUT WITH THE FACTOR (`tabxplor_breaks` / `tabxplor_labels`) and are frozen
#     back into the spec. A refit or a replay re-cuts at those exact places -- a weighted quantile
#     of a different frame would not land there.
#   - `ordered` DIFFERS BY PRODUCER, and it is not taste: `tab()` wants an ordered index (bands have
#     a real order), a fit wants treatment contrasts (an ordered factor gets polynomial ones). It is
#     an argument, defaulting to the fit's answer.
#   - THE LABEL IS THE ONLY THING A READER GETS, and it says NOTHING THE BOUNDS ALREADY SAY. It
#     carries the real cut points, and for a BAND the landmark it sits at ("[30,48) ; < mean"),
#     which no interval states; a quantile group's rank is readable off its own interval, so it
#     carries no tag. On a WHOLE-NUMBERED column it names the VALUES instead of the interval holding
#     them ("0", "1 or 2", "3 to 6"): the breaks are already snapped to integers, so the two say the
#     same thing and one of them is readable. The variable's own name goes on the FIRST level only
#     when `tab(shape_name = TRUE)` asks -- the one case that earns it is a table whose leading text
#     columns are stripped.
#   - A QUANTILE CUT GIVES k GROUPS WHENEVER THE VALUES ALLOW IT. Ties make two quantiles land on
#     one value, and deduplicating the breaks used to lose a group silently -- `quartiles` giving 3
#     where `quintiles` gave 4, on one column. shape_fill_breaks() fills back up at the distinct
#     values the quantiles missed; a genuine shortfall is stated, once.
#   - `cut()` is always called `right = FALSE, include.lowest = TRUE`, so every label is the
#     paren-free `[a,b)` form: cleannames_condition() (R/utils.R) strips any balanced ` (...)` group.
#   - THE DECLARATION ORDER IS THE OFFER ORDER, numeric first then the cuts coarse-to-fine, and
#     `values_to_levels` last because it is the one that explodes the level count. Both jamovi
#     pickers are emitted from it verbatim (dev/generate_jamovi_js.R), so reordering here reorders
#     them -- and the FIRST entry of a list is what that picker treats as "nothing stored".
#   - A TRANSFORM RENAMES ITS COLUMN, and shape_rename_transformed() returns that map (`renames`)
#     BECAUSE a fingerprint keyed on a column name would otherwise lose the source it came from --
#     which is exactly what the jamovi cache keys do (R/jmvtab-cache.R, via ctx$shape_renames).
# See: CLAUDE.md § tabxplor architecture; R/reg-assumptions.R (the quadratic term and the checks
#   whose cure `shape` is).


# === SECTION: the declared vocabulary ==============================================================

# ONE row per shape. `kind` is what the applier dispatches on, `produces` what the column IS
# afterwards (which is what a producer's variable classification must read), `producers` who may ask
# for it, `k` how many groups a quantile cut makes (NA where the shape does not cut), `mark` how a
# still-numeric transform names itself in a header, `doc` the user-facing prose ?shape_numeric_var
# renders.
#' @keywords internal
#' @noRd
VAR_SHAPES <- tx_grid(tibble::tribble(
  ~key,               ~kind,       ~produces, ~producers,          ~k,          ~mark, ~doc,
  "linear",           "none",      "numeric", c("tab", "tab_reg"), NA_integer_, NA_character_,
      paste("the number as it is --- one slope in a model, one mean in a crosstab. The default for a column",
            "variable, and what `shape` is spelled out as when nothing is done."),
  "log",              "log",       "numeric", c("tab", "tab_reg"), NA_integer_, "log(x)",
      "replace the variable by its logarithm --- diminishing returns. Needs strictly positive values.",
  "sqrt",             "sqrt",      "numeric", c("tab", "tab_reg"), NA_integer_, "\u221a(x)",
      "replace the variable by its square root. Needs non-negative values.",
  "quadratic",        "quadratic", "term",    "tab_reg",           NA_integer_, NA_character_,
      paste("add a curvature term, so the predictor takes two rows --- the slope at the mean, and the squared",
            "term saying whether the slope flattens or accelerates away from it. A model term, so [tab()]",
            "cannot take it."),
  "sd_bands",         "bands",     "factor",  c("tab", "tab_reg"), NA_integer_, NA_character_,
      paste("four bands cut at the mean and one standard deviation either side. Each level names its own cut",
            "(`[30,48) ; < mean`), so the label can be checked against the interval beside it. The cut points",
            "mean the same thing across sub-samples of one variable, where quantile breaks move with each one;",
            "but the bands are NOT balanced, and on a skewed variable a landmark falling outside the data is",
            "dropped (an exponential variable gets three bands, not four)."),
  "median",           "quantiles", "factor",  c("tab", "tab_reg"), 2L,          NA_character_,
      "two groups of equal size, cut at the median --- the coarsest reading of a number.",
  "terciles",         "quantiles", "factor",  c("tab", "tab_reg"), 3L,          NA_character_,
      "three groups of equal size.",
  "quartiles",        "quantiles", "factor",  c("tab", "tab_reg"), 4L,          NA_character_,
      "four groups of equal size. The counts are balanced, so every group answers on a comparable base.",
  "quintiles",        "quantiles", "factor",  c("tab", "tab_reg"), 5L,          NA_character_,
      "five groups of equal size.",
  "deciles",          "quantiles", "factor",  c("tab", "tab_reg"), 10L,         NA_character_,
      paste("ten groups of equal size. Reads a gradient, but ten rows need a large sample to keep each base",
            "usable."),
  # LAST because it is the one that explodes the level count: a level per distinct value.
  "values_to_levels", "levels",    "factor",  "tab",               NA_integer_, NA_character_,
      paste("one level per distinct value, in numeric order. Right for a counted number or a 1-7 scale;",
            "unreadable for a continuous one, which is what `\"auto\"` decides."),
))

# The value set one producer accepts, in the order it is offered. THE one list; there is no second.
#' @keywords internal
#' @noRd
shape_vocab <- function(producer = "tab")
  names(VAR_SHAPES)[vapply(VAR_SHAPES, function(r) producer %in% r$producers, logical(1))]

# What a resolved spec turns the column into. Read by a producer's variable classification, so a
# column variable cut into groups is known to be a factor BEFORE it is one.
#' @keywords internal
#' @noRd
shape_produces <- function(spec) {
  if (is.null(spec)) return("numeric")
  kinds <- vapply(VAR_SHAPES, `[[`, character(1), "kind")
  VAR_SHAPES[[names(kinds)[match(spec$kind, kinds)]]]$produces
}

#' @keywords internal
#' @noRd
shape_is_factor <- function(spec) identical(shape_produces(spec), "factor")

# How a still-numeric transform names itself in a header. `tab()` writes the variable's own name in
# ("log(tvhours)"), because the mean of the logarithm is not the logarithm of the mean and a column
# headed by the bare name would say the wrong thing; `tab_reg()` writes the literal "x", because the
# `var` column beside the row already names the variable.
#' @keywords internal
#' @noRd
SHAPE_MARKS <- vapply(VAR_SHAPES, `[[`, character(1), "mark")

# Keyed on the KIND, which for the two marked shapes is their own name. `var` writes the variable in.
#' @keywords internal
#' @noRd
shape_mark <- function(kind, var = "x") {
  if (length(kind) != 1L || is.na(kind) || !kind %in% names(SHAPE_MARKS)) return(NA_character_)
  m <- SHAPE_MARKS[[kind]]
  if (is.na(m)) return(NA_character_)
  sub("x", var, m, fixed = TRUE)
}

# The same fact as a COLUMN NAME -- `log_age`, not `log(age)`. WARNING: it must be SYNTACTIC.
# tab()'s pipeline reads its variable names back with `as.character()` over a list of symbols, which
# deparses, and a deparsed non-syntactic name comes back wrapped in backticks -- so `log(age)` would
# stop matching its own column. A row LABEL has no such constraint, which is why the regression side
# keeps the `log(x)` form.
#' @keywords internal
#' @noRd
shape_colname <- function(kind, var) {
  if (length(kind) != 1L || is.na(kind) || !kind %in% names(SHAPE_MARKS)) return(NA_character_)
  if (is.na(SHAPE_MARKS[[kind]])) return(NA_character_)
  paste0(kind, "_", var)
}


# === SECTION: the per-variable argument grammar ====================================================

# THE shared grammar of every per-variable argument (`shape`, and tab_reg()'s `multiplier` / `ref`):
# an unnamed value -- or one named `default` -- applies to every eligible variable; a named element
# overrides it for that one. The fallbacks are read FIRST, so a named override always wins whatever
# order they were written in.
#
# `kinds` / `fallback_kind` let one argument carry one default PER KIND of variable (`ref`'s factor
# and numeric answers are different words). `also` is addressable BY NAME ONLY: a fallback meant for
# the variables must never silently move a grouping variable's reference.
#' @keywords internal
#' @noRd
per_variable <- function(x, eligible, arg, kinds = NULL, fallback_kind = NULL,
                         also = character(0), vocab = NULL, example = NULL,
                         what = "variable") {
  if (is.null(x) || length(x) == 0L) return(list())
  nm <- names(x) %||% rep("", length(x))
  nm[is.na(nm)] <- ""
  is_fb <- !nzchar(nm) | nm == "default"
  whats <- paste0(what, "s")
  out   <- list()

  seen <- character(0)
  for (i in which(is_fb)) {
    v <- x[[i]]
    k <- if (is.null(fallback_kind)) "" else fallback_kind(v)
    if (is.na(k))
      cli::cli_abort(c(
        "{.arg {arg}} cannot use {.val {as.character(v)[[1]]}} as a default for every {what}.",
        stats::setNames(c(vocab, if (!is.null(example))
          paste0("A value for one variable must name it: {.code ", example, "}.")),
          rep("i", length(vocab) + !is.null(example)))), call = NULL)
    if (k %in% seen)
      cli::cli_abort(c("{.arg {arg}} has two defaults for the same kind of {what}.",
                       "i" = "Give one unnamed value per kind, or name the variable."), call = NULL)
    seen    <- c(seen, k)
    targets <- if (!nzchar(k)) eligible else eligible[kinds[eligible] == k]
    if (length(targets) == 0L)
      cli::cli_abort(c("{.arg {arg}}: no {what} for the default {.val {as.character(v)[[1]]}}.",
                       "i" = "Eligible: {.val {eligible}}."), call = NULL)
    for (v2 in targets) out[[v2]] <- v
  }

  named <- nm[!is_fb]
  bad   <- setdiff(named, c(eligible, also))
  if (length(bad) > 0L)
    cli::cli_abort(c("{.arg {arg}} must name {whats} it applies to.",
                     "x" = "Not {whats} it applies to: {.val {bad}}.",
                     "i" = "Eligible: {.val {c(eligible, also)}}."), call = NULL)
  for (i in which(!is_fb)) out[[nm[[i]]]] <- x[[i]]
  out
}

# The one message the generic abort cannot give: a per-variable argument naming a variable that is
# already a factor.
#' @keywords internal
#' @noRd
shape_check_numeric_names <- function(x, data, vars, arg, whats = "numeric variables") {
  nm  <- setdiff(names(x) %||% character(0), c("", "default"))
  fac <- intersect(nm, vars[!vapply(vars, function(v) shape_is_numeric(data[[v]]), logical(1))])
  if (length(fac) > 0L)
    cli::cli_abort(c("{.arg {arg}} applies to {whats} only.",
                     "x" = "Not {whats}: {.val {fac}}.",
                     "i" = "{.val {fac}} {?is/are} already {?a factor/factors}."), call = NULL)
  invisible(TRUE)
}

# ⚠ a LOGICAL is a two-level factor, not a number (a fit names its coefficient `<var>TRUE`);
# Date / POSIXct stay numeric, where they already worked.
#' @keywords internal
#' @noRd
shape_is_numeric <- function(x) !(is.factor(x) || is.character(x) || is.logical(x))


# === SECTION: the parser ===========================================================================

# The number of quantile groups a value asks for (NA = it is not a cut request). A named shape's
# count comes from its VAR_SHAPES row, so "quartiles" and 4 cannot mean two different things.
#' @keywords internal
#' @noRd
shape_k <- function(value) {
  if (is.character(value) && length(value) == 1L && value %in% names(VAR_SHAPES)) {
    k <- VAR_SHAPES[[value]]$k
    if (!is.na(k)) return(k)
  }
  k <- suppressWarnings(as.integer(value))
  if (!is.na(k) && k >= 2L && k <= 20L && identical(trimws(as.character(value)), as.character(k)))
    k else NA_integer_
}

# One value -> the spec it names, or NULL for "nothing to do". The whole vocabulary is here: a
# quantile count, or one of this producer's VAR_SHAPES rows.
#' @keywords internal
#' @noRd
shape_value <- function(val, var, producer = "tab") {
  kind <- if (is.character(val)) trimws(tolower(val)) else val
  k    <- shape_k(kind)
  vocab <- shape_vocab(producer)
  if (is.character(kind) && length(kind) == 1L && kind %in% names(VAR_SHAPES) &&
      !kind %in% vocab) {
    r <- VAR_SHAPES[[kind]]
    cli::cli_abort(c(
      '{.arg shape} for {.val {var}}: {.val {kind}} is not something {.fn {producer}} can do.',
      "x" = "It {cli::qty(length(r$producers))}{?is/are} for {.or {.fn {r$producers}}}.",
      "i" = if (identical(r$produces, "term"))
        '{.val quintiles} shows the same curve as groups.'
      else 'Use {.or {.val {vocab}}}, or a number of groups.'), call = NULL)
  }
  if (!is.na(k)) return(list(kind = "quantiles", k = k))
  if (!is.character(kind) || length(kind) != 1L || !kind %in% vocab)
    cli::cli_abort(c(
      "{.arg shape} for {.val {var}} must be one of {.or {.val {vocab}}}, or a number of groups.",
      "x" = "Got {.val {as.character(val)[[1]]}}."), call = NULL)
  if (identical(VAR_SHAPES[[kind]]$kind, "none")) return(NULL)   # the default, spelled out
  list(kind = VAR_SHAPES[[kind]]$kind, k = NA_integer_)
}

# The whole `shape` argument -> a named list of specs, on the shared per-variable grammar. Validated
# against the data, so every refusal names the variable and the value the user wrote.
#' @keywords internal
#' @noRd
shape_resolve <- function(shape, data, vars, producer = "tab") {
  if (is.null(shape) || length(shape) == 0L) return(list())
  # ONE noun per producer: a crosstab shapes VARIABLES, a model shapes PREDICTORS, and every message
  # below says whichever the caller is -- the grammar and the checks stay single.
  what  <- if (identical(producer, "tab_reg")) "predictor" else "variable"
  whats <- if (identical(producer, "tab_reg")) "continuous predictors" else "numeric variables"
  vars <- intersect(vars, names(data))
  shape_check_numeric_names(shape, data, vars, "shape", whats)
  num  <- vars[vapply(vars, function(v) shape_is_numeric(data[[v]]), logical(1))]
  vals <- per_variable(shape, num, "shape", what = what)
  purrr::compact(purrr::imap(vals, function(v, nm) shape_value(v, nm, producer)))
}


# === SECTION: the cutters ==========================================================================

# Weighted quantiles (the midpoint / Hmisc definition). ONE producer for the cut groups, the
# diagnostic bins and the row sparkline, so a group and its curve can never disagree about where a
# break is.
#' @keywords internal
#' @noRd
shape_wquantile <- function(x, probs, w = NULL) {
  x <- as.numeric(x)
  ok <- is.finite(x)
  w  <- if (is.null(w)) rep(1, length(x)) else as.numeric(w)
  ok <- ok & is.finite(w) & w > 0
  if (!any(ok)) return(rep(NA_real_, length(probs)))
  x <- x[ok]; w <- w[ok]
  o <- order(x); x <- x[o]; w <- w[o]
  if (length(x) == 1L) return(rep(x, length(probs)))
  cw <- (cumsum(w) - 0.5 * w) / sum(w)
  stats::approx(cw, x, xout = probs, rule = 2, ties = "ordered")$y
}

# A break on a WHOLE-numbered variable is snapped up to the whole number. With `right = FALSE` an
# interval is [a, b), so a break at 29.89 and one at 30 admit exactly the same integers -- the cut is
# identical and the label stops reading "[29.89,47.18)" where "[30,48)" says the same thing.
#' @keywords internal
#' @noRd
shape_snap_breaks <- function(x, br) {
  u <- x[is.finite(x)]
  if (!length(u) || !all(abs(u - round(u)) < 1e-8)) return(br)
  br[-c(1L, length(br))] <- ceiling(br[-c(1L, length(br))] - 1e-8)
  br[c(1L, length(br))]  <- round(br[c(1L, length(br))])
  br
}

# The interval a cut() level names. A LITERAL, not a rebuild: the label is what a replay re-cuts
# against, and re-formatting it would drift -- with ONE exception, below.
#
# DESIGN: A WHOLE-NUMBERED VARIABLE NAMES ITS VALUES, not its interval. `[0,1)` holds exactly the
# value 0 and `[1,3)` exactly 1 and 2, so "0" and "1 or 2" say the same thing in the reader's own
# words. It is safe precisely where it applies: the breaks are already snapped to whole numbers
# (shape_snap_breaks), so the interval and the value set are the same statement.
# ⚠ the labels become factor LEVELS -- unique, no trailing parenthetical (cleannames_condition()
# strips one), and they are frozen into the spec, which the jamovi cache key hashes.
#' @keywords internal
#' @noRd
shape_bounds <- function(x, breaks) {
  f <- cut(x, breaks = breaks, include.lowest = TRUE, right = FALSE, dig.lab = 4L)
  list(idx = as.integer(f), bounds = shape_bound_labels(x, breaks, levels(f)))
}

# The value-set spelling of each [a, b) interval, on a whole-numbered column only.
#' @keywords internal
#' @noRd
shape_bound_labels <- function(x, breaks, bounds) {
  u <- x[is.finite(x)]
  if (!length(u) || !all(abs(u - round(u)) < 1e-8)) return(bounds)
  b <- round(breaks)
  if (length(b) != length(bounds) + 1L || any(abs(breaks - b) > 1e-8)) return(bounds)
  lo <- b[-length(b)]
  # every interval is [a, b) but the LAST, which cut(include.lowest =) closes on its upper bound
  hi <- c(b[-c(1L, length(b))] - 1L, b[[length(b)]])
  # ⚠ ONE STRING LITERAL PER gettextf() CALL -- potools extracts what it sees, gettext() looks up
  # what was evaluated, so a message pasted together inside the call can never be found again.
  labs <- ifelse(lo == hi, as.character(lo),
                 ifelse(hi == lo + 1L, gettextf("%s or %s", lo, hi),
                        gettextf("%s to %s", lo, hi)))
  if (anyDuplicated(labs)) bounds else labs
}

# THE label rule, one function for every cut. Bounds first (the real cut points), then in words where
# the group sits, then -- on the FIRST level only, and only when ASKED (`tab(shape_name = TRUE)`,
# off by default) -- the variable's own name, for the one case that earns it: a table whose leading
# text columns are stripped and which would then name the variable nowhere.
# ⚠ A `side` is written only where the bounds do not already say it: a quantile group's rank is
# readable off the interval it names, so a cut carries NO side; a band's landmark is not, so
# sd_bands does. `sep`: a PHRASE is separated ("[18,30) ; < mean - sigma"), because a side made of
# several words run against the bounds reads as one long string.
#' @keywords internal
#' @noRd
shape_labels <- function(bounds, side, name = NULL, sep = " ") {
  labs <- if (is.null(side)) bounds else paste0(bounds, sep, side)
  if (!is.null(name) && nzchar(name) && length(labs) > 0L)
    labs[[1L]] <- paste0(name, ": ", labs[[1L]])
  labs
}

# The word a band gets: where it sits relative to the mean and the SD landmarks it was cut at. Read
# off the surviving landmarks, so a skewed variable that lost one still names its bands correctly.
# The mean is always inside the range of a variable that varies, so the mean is always the pivot.
# The band SAYS ITS OWN CUT ("< mean - sigma") instead of grading it ("low"): the reader can then
# check the label against the interval beside it, and the words survive a skewed variable that lost
# a landmark without lying about which one it lost.
# WARNING: the sigma NEVER enters a string literal -- it is a %s, so the source stays ASCII AND the
# msgid stays ASCII. potools extracts a "\uXXXX" escape verbatim while gettext() looks the EVALUATED
# string up, which silently fuzzies every accented entry.
#' @keywords internal
#' @noRd
shape_band_words <- function(tag) {
  sg <- "\u03c3"
  low <- gettextf("< -1%s", sg); below <- gettext("below mean")
  above <- gettext("above mean"); high <- gettextf("> +1%s", sg)
  # one word per BAND: n_tag landmarks cut the range into n_tag + 1 bands, and each band is named by
  # the landmark it starts at (the first by the one it ends at).
  starts <- c(NA_character_, tag)
  vapply(seq_along(starts), function(i) {
    s <- starts[[i]]
    if (is.na(s)) return(if (identical(tag[[1L]], "m-sd")) low else below)
    switch(s, "m-sd" = below, "m" = above, "m+sd" = high, "")
  }, character(1))
}

# Fill a collapsed quantile cut back up to k groups, at the distinct values the quantiles missed.
# Greedy and weight-aware: each pass takes the WIDEST group by population and splits it at the
# distinct value that leaves its two halves closest to equal -- which is what an equal-frequency cut
# was asking for in the first place. Returns as many breaks as the values allow, never more than k+1.
#' @keywords internal
#' @noRd
shape_fill_breaks <- function(x, br, k, w = NULL) {
  x  <- as.numeric(x); ok <- is.finite(x)
  w  <- if (is.null(w)) rep(1, length(x)) else as.numeric(w)
  ok <- ok & is.finite(w) & w > 0
  if (!any(ok)) return(br)
  x <- x[ok]; w <- w[ok]
  u <- sort(unique(x))
  while (length(br) < k + 1L) {
    # candidate cut points: every value that opens a new group, i.e. every one not already a break
    cand <- setdiff(u[-1L], br)
    if (!length(cand)) break
    g  <- findInterval(x, br, rightmost.closed = TRUE)
    sw <- vapply(seq_len(length(br) - 1L), function(i) sum(w[g == i]), numeric(1))
    pick <- NA_real_
    # heaviest group first; a group holding one distinct value cannot be split, so try the next
    for (i in order(sw, decreasing = TRUE)) {
      in_i <- cand > br[[i]] & cand < br[[i + 1L]]
      if (!any(in_i)) next
      # the split of THIS group that leaves its two sides most even
      pick <- cand[in_i][which.min(vapply(cand[in_i], function(cp)
        abs(sum(w[g == i & x >= cp]) - sum(w[g == i & x < cp])), numeric(1)))]
      break
    }
    if (is.na(pick)) break
    br <- sort(c(br, pick))
  }
  br
}

# k quantile groups of a numeric column, as a factor. Breaks are WEIGHTED quantiles when the call
# carries weights (equal share of the POPULATION, not of the sample), with the extremes forced to the
# observed range so no value falls out.
#' @keywords internal
#' @noRd
shape_cut_quantiles <- function(x, k, w = NULL, var = "x", breaks = NULL, labels = NULL,
                                ordered = FALSE, name = NULL) {
  x <- as.numeric(x)
  if (!is.null(breaks) && !is.null(labels)) {
    b <- shape_bounds(x, breaks)
    return(factor(labels[b$idx], levels = labels, ordered = ordered))
  }
  br <- shape_wquantile(x, seq(0, 1, length.out = k + 1L), w)
  if (all(is.finite(x[!is.na(x)]))) {
    br[[1L]]     <- min(x, na.rm = TRUE)
    br[[k + 1L]] <- max(x, na.rm = TRUE)
  }
  br <- unique(shape_snap_breaks(x, br[is.finite(br)]))
  if (length(br) < 3L)
    cli::cli_abort(c("{.arg shape} cannot cut {.val {var}} into {k} groups.",
                     "x" = "Its distribution has too few distinct values.",
                     "i" = 'Use fewer groups, or {.val values_to_levels} to keep one level per value.'),
                   call = NULL)
  # THE ARGUMENT MEANS WHAT IT SAYS. On a tied variable two quantiles land on the same value and the
  # `unique()` above silently drops one, so `quartiles` gave 3 groups where `quintiles` gave 4 -- on
  # the same column, which reads as a bug because it is one. Fill back up to k at the distinct values
  # the quantiles missed, taking each time the split that leaves the two sides most even, and stop
  # when the values run out: a genuine shortfall is a fact about the data, an uneven one was not.
  br <- shape_fill_breaks(x, br, k, w)
  # A SHORTFALL IS NOW A FACT ABOUT THE DATA, so it is said -- and only then. ⚠ one string literal
  # per gettextf() call (see shape_band_words).
  if (length(br) - 1L < k)
    tx_inform_once(paste0("shape_short_", var), c("i" = gettextf(
      "%s: cut into %s groups rather than %s, having too few distinct values.",
      var, length(br) - 1L, k)))
  b    <- shape_bounds(x, br)
  # NO rank tag: the bounds already state where the group sits, whether they are read as an interval
  # ("[29,38)") or, on a whole-numbered column, as the values themselves ("4 to 24").
  labs <- shape_labels(b$bounds, NULL, name)
  # ⚠ the BREAKS AND LABELS ride out with the factor: a replay must cut a refit's frame at exactly
  # the same places, and a weighted quantile of a different frame would not land there.
  structure(factor(labs[b$idx], levels = labs, ordered = ordered),
            tabxplor_breaks = br, tabxplor_labels = labs)
}

# THE OTHER CUT: bands at the mean and one SD either side -- the landmarks moderated regression
# already names low / average / high (Aiken & West's evaluation points, used here as the BOUNDARIES).
# FOUR bands, not three: measured on a normal variable the cuts (m-sd, m, m+sd) give 16/34/34/16,
# where dropping the middle cut leaves 64-68 % of the sample in one undifferentiated row.
#
# ⚠ UNLIKE QUANTILES, THE BANDS ARE NOT BALANCED, and on a skewed variable a landmark can fall
# outside the data entirely -- `m - sd` is below the minimum for a lognormal or an exponential, which
# would ask cut() for an empty band. Such a landmark is DROPPED, so an exponential gets three bands
# rather than an empty one, and the words say which landmarks survived. Where BALANCE matters more
# than the landmarks, quantiles are the other cut: that asymmetry is the whole reason both exist.
#' @keywords internal
#' @noRd
shape_cut_bands <- function(x, w = NULL, var = "x", breaks = NULL, labels = NULL,
                            ordered = FALSE, name = NULL) {
  x <- as.numeric(x)
  if (!is.null(breaks) && !is.null(labels)) {
    b <- shape_bounds(x, breaks)
    return(factor(labels[b$idx], levels = labels, ordered = ordered))
  }
  m  <- wtd_mean(x, w)
  s  <- wtd_sd(x, w)
  rg <- range(x, na.rm = TRUE)
  land <- c(m - s, m, m + s)
  tag  <- c("m-sd", "m", "m+sd")
  keep <- is.finite(land) & land > rg[[1L]] & land < rg[[2L]]
  land <- land[keep]; tag <- tag[keep]
  # ⚠ the mean is always strictly inside the range of a variable that varies, so the only reachable
  # refusal is one that does not: a skewed variable DEGRADES to fewer bands rather than aborting.
  if (!is.finite(s) || s <= 0 || length(land) == 0L)
    cli::cli_abort(c(
      '{.code shape = "sd_bands"} needs {.val {var}} to vary.',
      "x" = "Its standard deviation is zero, so there are no bands to cut.",
      "i" = 'Use {.val values_to_levels} to keep one level per value, or pass it as a factor.'), call = NULL)
  br   <- unique(shape_snap_breaks(x, c(rg[[1L]], land, rg[[2L]])))
  b    <- shape_bounds(x, br)
  labs <- shape_labels(b$bounds, shape_band_words(tag), name, sep = " ; ")
  structure(factor(labs[b$idx], levels = labs, ordered = ordered),
            tabxplor_breaks = br, tabxplor_labels = labs)
}

# One level per distinct value, in NUMERIC order -- what a counted number or a short scale wants, and
# what a continuous variable must never get.
#
# DESIGN: PLAIN, never ordered, and NEVER PREFIXED by the variable's name, whatever the caller asks.
# This is exactly what a numeric row variable has always become (forcats::as_factor on the
# aggregate); naming it does not change it, so a table built this way stays identical to one built
# before `shape` existed -- and to tab_counts()' on the same keys, which is a declared parity
# (measured: the prefix alone broke it). The prefix exists for a level whose TEXT cannot say what it
# is a level of -- a bracketed interval, a band word -- and a raw value is not one. The two real CUTS
# are ordered for the same kind of reason: their order is a fact they add, and they are new, so
# nothing can differ from before.
#' @keywords internal
#' @noRd
shape_cut_levels <- function(x) {
  lv   <- sort(unique(x[!is.na(x)]))
  labs <- format(lv, trim = TRUE, scientific = FALSE)
  structure(factor(labs[match(x, lv)], levels = labs),
            tabxplor_breaks = lv, tabxplor_labels = labs)
}


# === SECTION: the applier ==========================================================================

# Apply every column-RECODING shape once, and return the shapes with their breaks and labels frozen
# in. `quadratic` is not a recode -- it emits a model term -- so it passes through untouched.
# The column keeps its NAME: every subsystem downstream reads an ordinary variable, and there is no
# bookkeeping to get wrong. (The raw column is therefore unrecoverable from the result, which is why
# a replay is given the user's own data and re-cuts at the frozen breaks.)
#' @keywords internal
#' @noRd
shape_apply <- function(data, shapes, w = NULL, ordered = FALSE, var_names = character(0)) {
  wv <- if (!is.null(w) && is.character(w) && length(w) == 1L && w %in% names(data)) data[[w]] else NULL
  for (v in names(shapes)) {
    sp <- shapes[[v]]
    nm <- if (v %in% var_names) v else NULL
    f  <- shape_one(data[[v]], sp, w = wv, var = v, ordered = ordered, name = nm)
    if (is.null(f)) next
    shapes[[v]]$breaks <- attr(f, "tabxplor_breaks") %||% shapes[[v]]$breaks
    shapes[[v]]$labels <- attr(f, "tabxplor_labels") %||% shapes[[v]]$labels
    attributes(f)[c("tabxplor_breaks", "tabxplor_labels")] <- NULL
    data[[v]] <- f
  }
  list(data = data, shapes = shapes)
}

# One column, one spec. THE dispatcher every path goes through -- the applier, the replay and the
# exported primitive -- so a cut can only ever be built one way. NULL = nothing to recode.
#' @keywords internal
#' @noRd
shape_one <- function(x, spec, w = NULL, var = "x", ordered = FALSE, name = NULL) {
  if (is.null(spec) || identical(spec$kind, "quadratic")) return(NULL)
  xn <- as.numeric(x)
  switch(
    spec$kind,
    log = {
      if (any(xn <= 0, na.rm = TRUE))
        cli::cli_abort(c('{.code shape = "log"} needs strictly positive values.',
                         "x" = "{.val {var}} has values <= 0.",
                         "i" = 'Use {.val sqrt}, {.val quintiles}, or shift the variable first.'),
                       call = NULL)
      log(xn)
    },
    sqrt = {
      if (any(xn < 0, na.rm = TRUE))
        cli::cli_abort(c('{.code shape = "sqrt"} needs non-negative values.',
                         "x" = "{.val {var}} has negative values."), call = NULL)
      sqrt(xn)
    },
    quantiles = shape_cut_quantiles(xn, spec$k, w, var = var, breaks = spec$breaks,
                                    labels = spec$labels, ordered = ordered, name = name),
    bands     = shape_cut_bands(xn, w, var = var, breaks = spec$breaks,
                                labels = spec$labels, ordered = ordered, name = name),
    levels    = shape_cut_levels(xn),
    NULL)
}


# === SECTION: the exported primitive ===============================================================

#' Shape a numeric variable
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Turn a number into the form you want to read it in: cut it into quantile groups or into bands at
#' the mean and one standard deviation either side (it becomes an ordinary factor), or transform it
#' and keep it a number. This is the same operation `tab(shape =)` and [tab_reg()]`(shape =)`
#' perform, exposed on its own vector, so a column you cut by hand is identical to one they cut.
#'
#' @param x A numeric vector.
#' @param shape A single string, or a number of groups --- see \emph{The shapes} below.
#' @param w Optional weights, the same length as `x`. Quantile breaks and the mean/SD landmarks are
#'   then weighted --- equal shares of the *population*, not of the sample.
#' @param name The variable's name, written onto the **first level only** (`"age: [18,35) low"`), so
#'   a table that names the variable nowhere else still says what the levels are levels *of*. `NULL`
#'   (the default here) writes nothing. It applies to the two CUTS, whose levels are intervals and
#'   band words; `"values_to_levels"` keeps the raw values, which name themselves.
#' @param ordered Whether the resulting factor is `ordered`. Bands and quantile groups do have a
#'   real order; a model fit does not want one (an ordered factor takes polynomial contrasts instead
#'   of contrasts against a reference), which is why [tab_reg()] cuts unordered.
#'
#' @return For a cut: a factor whose levels carry the real cut points and, in words, where each group
#'   sits. For `"log"` / `"sqrt"`: a numeric vector.
#'
#' @section The mean of a transform is not the transform of the mean:
#' `"log"` and `"sqrt"` replace the values, so a crosstab column then shows the mean *of the
#' logarithm* --- a different quantity from the logarithm of the mean, and smaller. [tab()] renames
#' the column for that reason (`age` becomes `log_age`), so no header promises the wrong quantity;
#' do the same if you shape a column by hand.
#'
#' @eval shape_values_rd()
#'
#' @seealso [tab()] and [tab_reg()], whose `shape` argument is this function applied once to the
#'   whole population, before any sub-table or sub-model is split off.
#' @export
#'
#' @examples
#' \donttest{
#' age <- forcats::gss_cat$age
#' table(shape_numeric_var(age, "sd_bands", name = "age"))
#' table(shape_numeric_var(age, "quartiles"))
#' }
shape_numeric_var <- function(x, shape, w = NULL, name = NULL, ordered = TRUE) {
  if (!shape_is_numeric(x))
    cli::cli_abort(c("{.arg x} must be a numeric vector.",
                     "x" = "Got a {.cls {class(x)}} vector."), call = NULL)
  spec <- shape_value(shape, name %||% "x", "tab")
  out  <- shape_one(x, spec, w = w, var = name %||% "x", ordered = ordered, name = name)
  if (is.null(out)) return(x)
  attributes(out)[c("tabxplor_breaks", "tabxplor_labels")] <- NULL
  out
}

# The generated value list, so ?shape_numeric_var, ?tab and ?tab_reg cannot drift from VAR_SHAPES.
#' @keywords internal
#' @noRd
shape_values_rd <- function(producer = "tab", section = TRUE) {
  keys <- shape_vocab(producer)
  body <- c(" \\itemize{",
            vapply(keys, function(k)
              paste0("  \\item \\code{\"", k, "\"}: ", VAR_SHAPES[[k]]$doc), character(1)),
            "  \\item an integer \\code{k} (2 to 20): \\code{k} quantile groups of equal size.",
            " }")
  if (!section) return(body)
  c("@section The shapes:", body)
}


# === SECTION: the automatic answer for a row or tab variable =======================================

# A number on the ROW axis has no useful raw form -- one line per distinct value is a table nobody
# reads. "auto" is the answer, and it is one threshold: few distinct WHOLE values is a counted
# number or a short scale, where every value is a category; anything else is continuous and is
# banded. The threshold is an option, so it is tunable without an argument.
#' @keywords internal
#' @noRd
shape_auto <- function(x) {
  u <- unique(x[!is.na(x)])
  if (length(u) == 0L) return("values_to_levels")
  whole <- all(is.finite(u)) && all(abs(u - round(u)) < 1e-8)
  if (whole && length(u) <= as.integer(tx_option("shape_auto_max"))) "values_to_levels" else "sd_bands"
}

# The row and tab axes have no raw form worth printing, so every numeric one there gets a shape --
# the user's if they named it, "auto" otherwise. A numeric COLUMN variable is left alone: there a
# number already has a reading (its mean), which is the whole point of the numeric leaf.
#' @keywords internal
#' @noRd
shape_fill_auto <- function(shapes, data, index_vars) {
  auto <- character(0)
  for (v in setdiff(intersect(index_vars, names(data)), names(shapes))) {
    if (!shape_is_numeric(data[[v]])) next
    shapes[[v]] <- shape_value(shape_auto(data[[v]]), v, "tab")
    auto <- c(auto, v)
  }
  list(shapes = shapes, auto = auto)
}

# Say what was decided, once per call and only for what the user did NOT ask for -- an automatic
# recode changes what the table IS, so it is never silent, and the message names the argument that
# overrides it.
#' @keywords internal
#' @noRd
shape_report <- function(shapes, auto) {
  if (length(auto) == 0L) return(invisible(NULL))
  said <- vapply(auto, function(v) {
    if (identical(shapes[[v]]$kind, "levels"))
      gettextf("%s: one row per value.", v)
    else
      gettextf("%s: cut into four bands, at its mean and one standard deviation either side.", v)
  }, character(1))
  tx_inform_once(paste0("shape_auto_", paste(auto, collapse = "_")), c(
    stats::setNames(said, rep("i", length(said))),
    "i" = paste0('Choose otherwise with {.code shape = c(', auto[[1]], ' = "quintiles")}.')))
  invisible(NULL)
}


# A transform CHANGES THE NUMBER, so the column must stop being headed by the bare variable name:
# the mean of a logarithm is not the logarithm of a mean. A cut needs nothing -- its levels say it.
#' @keywords internal
#' @noRd
shape_var_labels <- function(var_labels, shapes) {
  for (v in names(shapes)) {
    # the column has already been renamed `log_age`, so the LABEL -- which is display-only and never
    # matched against a column -- may say it the readable way.
    mk <- shape_mark(shapes[[v]]$kind, sub("^(log|sqrt)_", "", v))
    if (!is.na(mk)) var_labels[[v]] <- mk
  }
  var_labels
}

# A TRANSFORM RENAMES ITS COLUMN, at the boundary where the table's variables are decided. The mean
# of a logarithm is not the logarithm of a mean, so a column still headed `age` would state the wrong
# quantity -- in the console, in the returned tibble and in every export alike, which is why this is
# a rename and not a display label. A CUT needs none: its levels say what it is.
# Runs on the user's own frame, before any role is fixed, so only `data` and the selected names have
# to move; everything downstream simply sees a variable called `log(age)`.
#' @keywords internal
#' @noRd
shape_rename_transformed <- function(data, vars, shape) {
  none <- list(data = data, vars = vars, shape = shape, renames = character(0))
  if (is.null(shape) || length(shape) == 0L || length(vars) == 0L) return(none)
  specs <- tryCatch(shape_resolve(shape, data, vars, "tab"), error = function(e) list())
  ren   <- character(0)
  for (v in intersect(names(specs), vars)) {
    mk <- shape_colname(specs[[v]]$kind, v)
    if (!is.na(mk) && !mk %in% names(data)) ren[[v]] <- mk
  }
  if (length(ren) == 0L) return(none)
  names(data)[match(names(ren), names(data))] <- unname(ren)
  vars[match(names(ren), vars)] <- unname(ren)
  nm <- names(shape)
  if (!is.null(nm)) names(shape)[nm %in% names(ren)] <- unname(ren[nm[nm %in% names(ren)]])
  # WARNING: the NEW name -> the source column it came from. Anything that fingerprints a column by
  # name (the jamovi cache keys) must look the source up through this, or `log_age` would carry no
  # fingerprint at all and an edit to `age` would not move the key.
  list(data = data, vars = vars, shape = shape,
       renames = stats::setNames(names(ren), unname(ren)))
}

# `log` / `sqrt` keep the column a NUMBER, and a number on the row or tab axis has no reading -- it
# would fall straight back to one line per distinct value. Refused by name, pointing at the cuts.
#' @keywords internal
#' @noRd
shape_refuse_numeric_index <- function(shapes, index_vars) {
  bad <- intersect(names(shapes), index_vars)
  bad <- bad[vapply(bad, function(v) identical(shape_produces(shapes[[v]]), "numeric"), logical(1))]
  if (length(bad) == 0L) return(invisible(NULL))
  cure <- paste0('shape = c(', bad[[1]], ' = "quintiles")')
  cli::cli_abort(c(
    "{.arg shape}: {.val {bad}} {?is/are} on the row or tab axis, where a number has no rows.",
    "x" = "{.val log} and {.val sqrt} leave it a number.",
    "i" = "Cut it into groups instead: {.code {cure}}.",
    "i" = "A transform is for a {.arg col_vars}, where the column shows a mean."), call = NULL)
}
