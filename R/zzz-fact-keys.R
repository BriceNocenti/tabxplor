# PURPOSE: REFERENTIAL INTEGRITY between the declared fact tables -- checked when the namespace
#   loads, so a broken reference fails the install rather than a user's table.
# ROLE: a key written by hand in one table and read by name in another is a FOREIGN KEY.
#   TAB_FOREIGN_KEYS declares every such edge and tx_check_foreign_keys() walks them at load. Two
#   sibling checkers ride along: tx_check_tab_args() (the argument surface against the real
#   signatures) and tx_check_reg_ctx() (the two regression records against each other).
#   THE RULE: a dangling key is a LOAD failure, not a runtime one. Adding an edge is one row.
# DESIGN -- what is here, and what is not:
#   here     every edge from one declared table's VALUES to another's KEYS, including a table's
#            references to itself (COLOR_SCALES$derive, DISPLAY_TOKENS$alias).
#   not here a table's OWN self-consistency -- "does it cover its own key set", "does it agree with
#            the switch bodies". Those stay beside the table, where their operands are in scope:
#            fmt_class.R (FMT_FIELD_DOC, fmt_attr_rules, MEASURES' members), tab-agg.R (CI_GEOMS),
#            tab-display.R (DISPLAY_TOKENS vs the get_num()/set_num() bodies), reg-estimand.R
#            (per-family defaults).
# KEY CONSTRAINTS:
#   - ⚠ WHY THIS FILE SORTS LAST. There is no `Collate:` field, so R sources R/ in C collation and
#     the tables are spread over seven files. `zzz-` is the only prefix that is last BY
#     CONSTRUCTION, whatever the other files are called -- and it is the only reason
#     tx_check_tab_args() can exist at all: formals(tab) is not available while tab-args.R is
#     being sourced.
#   - ⚠ READ EVERY TABLE WITH `[[`, NEVER `$`. MEASURES$adjustment has `scale_from` and no `scale`,
#     so `$scale` PARTIAL-MATCHES to "gap" and a generic checker would silently validate the wrong
#     string. The two readers below are the only way rows are read here.
#   - The checkers use stop(), not cli::cli_abort(): they run while the namespace is still being
#     built and must not depend on the package's own machinery.
# See: CLAUDE.md § tabxplor architecture (the declarative architecture).


# --- reading a table's rows ---------------------------------------------------------------------

# ONE value per row (the row's own default when the member is absent).
#' @keywords internal
#' @noRd
tx_fk_scalar <- function(tbl, field) {
  vapply(tbl, function(r) {
    v <- r[[field]]
    if (is.null(v) || !length(v)) NA_character_ else as.character(v)[[1]]
  }, character(1), USE.NAMES = FALSE)
}

# EVERY value of a member, over every row (a member may itself be a vector: MEASURES$applies_to,
# REG_CHECKS$families).
#' @keywords internal
#' @noRd
tx_fk_all <- function(tbl, field) {
  unlist(lapply(tbl, function(r) {
    v <- r[[field]]
    if (is.null(v)) character(0) else as.character(v)
  }), use.names = FALSE)
}

# REG_ESTIMANDS is a list BY FAMILY of `list(default =, rows =)`, so its rows are two levels down.
#' @keywords internal
#' @noRd
tx_fk_reg_rows <- function() unlist(lapply(REG_ESTIMANDS, `[[`, "rows"), recursive = FALSE)

# REG_EMPIRICAL mixes per-family SCALARS (method_diff, coef) with SHAPE rows; only the
# latter have members.
#' @keywords internal
#' @noRd
tx_fk_emp_shapes <- function() {
  out <- unlist(lapply(REG_EMPIRICAL, function(fam) fam[vapply(fam, is.list, logical(1))]),
                recursive = FALSE, use.names = FALSE)
  out
}
#' @keywords internal
#' @noRd
tx_fk_emp_shape_names <- function()
  unique(unlist(lapply(REG_EMPIRICAL, function(fam) names(fam)[vapply(fam, is.list, logical(1))])))

# The shape keys QUALIFIED by their block ("binomial.or_log"). A bare name is ambiguous -- `or` is
# declared by four blocks, `rr_log` by two -- so only the qualified form can say "this shape exists
# in the block that will be asked for it".
#' @keywords internal
#' @noRd
tx_fk_emp_shape_keys <- function()
  unlist(lapply(names(REG_EMPIRICAL), function(f) {
    fam <- REG_EMPIRICAL[[f]]
    paste0(f, ".", names(fam)[vapply(fam, is.list, logical(1))])
  }), use.names = FALSE)

# Every crude shape a REACHABLE estimand asks for, resolved exactly as reg_crude_shape() resolves it.
# Two things the bare-name edge cannot see, and both have drawn another estimand's column: the BLOCK
# is chosen by `crude_fam` / `crude_key` (a borrow crosses families, and a summed score overrides
# both), and a `measure = "log_*"` request composes the `_log` twin by string concatenation, so a
# shape no one declared silently resolved to its block's coefficient shape.
#' @keywords internal
#' @noRd
tx_fk_emp_reachable <- function() {
  out <- character(0)
  for (f in names(REG_ESTIMANDS)) for (r in REG_ESTIMANDS[[f]]$rows) {
    # `trials = ` is the one caller-supplied fact that moves a block, so both states are enumerated.
    for (tr in list(NULL, 1)) {
      key <- reg_crude_key(r$fit, tr)
      if (is.na(key)) next
      blk <- reg_emp_block_of(key, r)          # the resolver's own rule, never a second copy
      if (is.na(blk)) next
      base <- if (is.na(r$crude_shape)) REG_EMPIRICAL[[blk]]$coef else r$crude_shape
      out  <- c(out, paste0(blk, ".", base))
    }
  }
  unique(out)
}


# --- the declared edges --------------------------------------------------------------------------
# Each row: `from` (what the message names) - `get`/`to` (closures for the values / the legal key
# set) - `allow` (legal non-key values) - `orphan` (TRUE: also REPORT target keys no edge references
# -- dead weight, a fact to report, never a failure).
#' @keywords internal
#' @noRd
tx_fk <- function(from, get, to, allow = character(0), orphan = FALSE)
  list(from = from, get = get, to = to, allow = allow, orphan = orphan)

#' @keywords internal
#' @noRd
TAB_FOREIGN_KEYS <- list(

  # --- into MEASURES (the colour measures) ---------------------------------------------------
  tx_fk("EST_SCALES$label_meas",   function() tx_fk_scalar(EST_SCALES, "label_meas"),
        function() names(MEASURES)),
  tx_fk("DISPLAY_TOKENS$comparison", function() tx_fk_scalar(DISPLAY_TOKENS, "comparison"),
        function() names(MEASURES)),
  tx_fk("COLOR_LEGACY_ALIASES$measure",   function() tx_fk_scalar(COLOR_LEGACY_ALIASES, "measure"),
        function() names(MEASURES)),
  tx_fk("MEASURE_ACRONYMS",        function() unname(MEASURE_ACRONYMS),
        function() names(MEASURES)),
  tx_fk("MEASURE_ACRONYMS_REG",    function() unname(MEASURE_ACRONYMS_REG),
        function() names(MEASURES)),
  # --- the FOOTER: the region, its vocabulary and what each part reads -------------------------
  # a placeholder's `reads` names the facts it is built from: `meta$<field>` must be a declared table
  # attribute, and every other name a declared `fmt` column attribute. That edge is what lets
  # ?tabxplor-footer say "to change what this says, use ..." without restating a single setter.
  tx_fk("FOOTER_BLOCKS$reads (meta)",
        function() sub("^meta[$]", "", grep("^meta[$]", tx_fk_all(FOOTER_BLOCKS, "reads"), value = TRUE)),
        function() c(names(TAB_ATTRS), "spec$vars$wt", "spec$call")),
  tx_fk("FOOTER_BLOCKS$reads (column)",
        function() grep("^meta[$]", tx_fk_all(FOOTER_BLOCKS, "reads"), value = TRUE, invert = TRUE),
        function() c(fmt_col_attrs, fmt_field_names, "subtext", "test")),
  tx_fk("FOOTER_BLOCKS$kind",      function() tx_fk_scalar(FOOTER_BLOCKS, "kind"),
        function() c("line", "note", "tab", "inline")),

  # --- into COLOR_SCALES (the break ladders) -------------------------------------------------
  tx_fk("EST_SCALES$break_key",    function() tx_fk_scalar(EST_SCALES, "break_key"),
        function() names(COLOR_SCALES), orphan = TRUE),
  tx_fk("EST_SCALES$gap_key",      function() tx_fk_scalar(EST_SCALES, "gap_key"),
        function() names(COLOR_SCALES), orphan = TRUE),
  tx_fk("MEASURES$scale",          function() tx_fk_all(MEASURES, "scale"),
        function() names(COLOR_SCALES), orphan = TRUE),
  tx_fk("MEASURES$guar$scale",
        function() unlist(lapply(MEASURES, function(m) as.character(m[["guar"]][["scale"]]))),
        function() names(COLOR_SCALES), orphan = TRUE),
  tx_fk("names(MEASURES$by_scale)",
        function() unlist(lapply(MEASURES, function(m) names(m[["by_scale"]]))),
        function() names(COLOR_SCALES), orphan = TRUE),
  tx_fk("COLOR_SCALES$derive$from",
        function() unlist(lapply(COLOR_SCALES, function(s) as.character(s[["derive"]][["from"]]))),
        function() names(COLOR_SCALES)),

  # --- into EST_SCALES (what a column ESTIMATES) ---------------------------------------------
  tx_fk("CI_GEOMS$scale_key",      function() tx_fk_scalar(CI_GEOMS, "scale_key"),
        function() names(EST_SCALES)),
  tx_fk("REG_ESTIMANDS$rows$scale", function() tx_fk_scalar(tx_fk_reg_rows(), "scale"),
        function() names(EST_SCALES)),
  tx_fk("REG_EMPIRICAL$*$scale",   function() tx_fk_scalar(tx_fk_emp_shapes(), "scale"),
        function() names(EST_SCALES)),
  tx_fk("EST_SCALES$ladder",       function() tx_fk_scalar(EST_SCALES, "ladder"),
        function() unique(unlist(lapply(MEASURES, function(m) names(m[["scale"]]))))),

  # --- into the interval vocabulary ----------------------------------------------------------
  tx_fk("CI_GEOMS$method_slot",    function() tx_fk_scalar(CI_GEOMS, "method_slot"),
        function() names(CI_METHODS)),
  # ⚠ "katz" is deliberately outside CI_METHODS: a proportion RATIO has exactly one interval, so
  #   declaring it a ci_method choice would offer an empty menu.
  tx_fk("CI_GEOMS$method_fixed",   function() tx_fk_scalar(CI_GEOMS, "method_fixed"),
        function() unlist(CI_METHODS, use.names = FALSE), allow = "katz"),
  # ⚠ woolf/katz/wald_log: each the ONLY interval of its shape, not a `ci_method` a user picks --
  #   named in the legend instead.
  tx_fk("REG_EMPIRICAL$*$ci_method", function() tx_fk_scalar(tx_fk_emp_shapes(), "ci_method"),
        function() unlist(CI_METHODS, use.names = FALSE), allow = c("woolf", "katz", "wald_log")),
  # the crude leg's own link -- read by reg_crude_if_maker()'s delta factor and by the estimand
  # library's composition of every crude companion. An undeclared one would silently drop the gap
  # test rather than fail, which is why this edge exists.
  tx_fk("REG_EMPIRICAL$*$link", function() tx_fk_scalar(tx_fk_emp_shapes(), "link"),
        function() names(REG_LINK_FUNS)),
  tx_fk("REG_EMPIRICAL$*$ci_method_design",
        function() tx_fk_scalar(tx_fk_emp_shapes(), "ci_method_design"),
        function() unlist(CI_METHODS, use.names = FALSE)),
  tx_fk("CI_POOLED",               function() unlist(CI_POOLED, use.names = FALSE),
        function() unlist(CI_METHODS, use.names = FALSE)),

  # --- into DISPLAY_TOKENS (what a cell shows) -----------------------------------------------
  tx_fk("EST_SCALES$default_display", function() tx_fk_scalar(EST_SCALES, "default_display"),
        function() names(DISPLAY_TOKENS)),
  tx_fk("EST_SCALES$est_display",  function() tx_fk_scalar(EST_SCALES, "est_display"),
        function() names(DISPLAY_TOKENS)),
  tx_fk("EST_SCALES$base_display", function() tx_fk_scalar(EST_SCALES, "base_display"),
        function() names(DISPLAY_TOKENS)),
  tx_fk("EST_SCALES$const_display", function() tx_fk_scalar(EST_SCALES, "const_display"),
        function() names(DISPLAY_TOKENS)),
  tx_fk("REG_CELL_DIGITS", function() names(REG_CELL_DIGITS), function() names(EST_SCALES)),
  tx_fk("DISPLAY_TOKENS$alias",    function() tx_fk_scalar(DISPLAY_TOKENS, "alias"),
        function() names(DISPLAY_TOKENS)),
  # every {token} a named layout is spelt with, so a preset can never name a token that went away
  tx_fk("DISPLAY_PRESETS",
        function() {
          v <- unlist(lapply(DISPLAY_PRESETS, function(r) r$template), use.names = FALSE)
          v <- v[!is.na(v)]
          unique(trimws(gsub("[{}]", "", unlist(regmatches(v, gregexpr("\\{[^{}]+\\}", v))))))
        },
        function() names(DISPLAY_TOKENS)),
  tx_fk("DISPLAY_PRESETS$alias", function() unname(DISPLAY_PRESET_ALIASES),
        function() names(DISPLAY_PRESETS)),
  # every hover LINE renders a declared token, so a tooltip cannot name one that went away
  tx_fk("TOOLTIP_LINES$token",     function() tx_fk_scalar(TOOLTIP_LINES, "token"),
        function() names(DISPLAY_TOKENS)),

  # --- into fmt_field_names (the record) -----------------------------------------------------
  tx_fk("DISPLAY_TOKENS$field",    function() tx_fk_scalar(DISPLAY_TOKENS, "field"),
        function() fmt_field_names, allow = "ci"),
  tx_fk("EST_SCALES$est_field",    function() tx_fk_scalar(EST_SCALES, "est_field"),
        function() fmt_field_names),

  # --- into the small declared enums ---------------------------------------------------------
  tx_fk("COLOR_LEGACY_ALIASES$policy",    function() tx_fk_scalar(COLOR_LEGACY_ALIASES, "policy"),
        function() COLOR_SIGNIF_VALUES),
  tx_fk("MEASURES$applies_to",     function() tx_fk_all(MEASURES, "applies_to"),
        function() COLOR_COL_KINDS),
  # who can BUILD a measure, and whose `color =` may NAME it -- two questions, one value set.
  tx_fk("MEASURES$producers",      function() tx_fk_all(MEASURES, "producers"),
        function() MEASURE_PRODUCERS),
  tx_fk("MEASURES$color_arg",      function() tx_fk_scalar(MEASURES, "color_arg"),
        function() MEASURE_PRODUCERS),
  tx_fk("names(MEASURE_PRODUCER_FN)", function() names(MEASURE_PRODUCER_FN),
        function() MEASURE_PRODUCERS),
  tx_fk("REG_CHECKS$kind",         function() tx_fk_scalar(REG_CHECKS, "kind"),
        function() names(ROW_KINDS)),

  # --- into TEST_ROWS: what kind of statistical row this is -----------------------------------
  tx_fk("TEST_ROWS$kind",          function() tx_fk_scalar(TEST_ROWS, "kind"),
        function() names(ROW_KINDS)),
  tx_fk("TEST_ROWS$stat",          function() tx_fk_scalar(TEST_ROWS, "stat"),
        function() reg_stat_keys()),
  tx_fk("TEST_ROWS$var_kind",      function() tx_fk_scalar(TEST_ROWS, "var_kind"),
        function() unique(tx_fk_scalar(EST_SCALES, "var_kind"))),
  # TAB_ARGS owns the argument, TEST_ROWS owns which test each of its two values selects.
  tx_fk("TAB_ARGS$anova$values",   function() TAB_ARGS[["anova"]][["values"]],
        function() unique(tx_fk_scalar(TEST_ROWS, "anova"))),

  # --- into the regression family vocabulary -------------------------------------------------
  tx_fk("names(REG_ESTIMANDS)",    function() names(REG_ESTIMANDS),
        function() names(REG_FAMILIES)),
  tx_fk("REG_ESTIMANDS$rows$fit",  function() tx_fk_scalar(tx_fk_reg_rows(), "fit"),
        function() names(REG_FAMILIES)),
  tx_fk("REG_ESTIMANDS$rows$fit -> checks", function() tx_fk_scalar(tx_fk_reg_rows(), "fit"),
        function() REG_CHECK_FAMILIES),
  tx_fk("REG_CHECKS$families",     function() tx_fk_all(REG_CHECKS, "families"),
        function() REG_CHECK_FAMILIES),
  tx_fk("REG_OUTCOME_KINDS$detect", function() tx_fk_all(REG_OUTCOME_KINDS, "detect"),
        function() names(REG_FAMILIES)),
  tx_fk("REG_OUTCOME_KINDS$offers", function() tx_fk_all(REG_OUTCOME_KINDS, "offers"),
        function() names(REG_FAMILIES)),

  # --- the fit digest: a new model backend is one REG_FIT_KINDS row, and its edges are these ---
  tx_fk("REG_FIT_KINDS$score", function() unlist(lapply(REG_FIT_KINDS, function(k) k$score)),
        function() REG_SCORE_ENGINES, allow = NA_character_),
  tx_fk("REG_FIT_KINDS$parts", function() unlist(lapply(REG_FIT_KINDS, function(k) k$parts)),
        function() names(REG_DIGEST_PARTS)),
  tx_fk("REG_DIGEST_PARTS$kinds",
        function() unlist(lapply(REG_DIGEST_PARTS, function(p)
          if (identical(p$kinds, "all")) character(0) else p$kinds)),
        function() names(REG_FIT_KINDS)),

  # --- into the vocabulary reg_build() dispatches on ------------------------------------------
  tx_fk("REG_ESTIMANDS$rows$builder", function() tx_fk_scalar(tx_fk_reg_rows(), "builder"),
        function() REG_BUILDERS),

  # --- into REG_EMPIRICAL (which crude column pairs with which estimand) ----------------------
  tx_fk("REG_ESTIMANDS$rows$crude_fam", function() tx_fk_scalar(tx_fk_reg_rows(), "crude_fam"),
        function() names(REG_EMPIRICAL), allow = "auto"),
  tx_fk("REG_ESTIMANDS$rows$crude_shape", function() tx_fk_scalar(tx_fk_reg_rows(), "crude_shape"),
        function() tx_fk_emp_shape_names()),
  # ...and the same keys QUALIFIED by the block that will be asked for them, log twins included.
  tx_fk("the crude shape every reachable estimand resolves to", tx_fk_emp_reachable,
        tx_fk_emp_shape_keys),

  # --- into the header vocabulary (REG_WORDS / REG_CONTRASTS) ---------------------------------
  tx_fk("REG_ESTIMANDS$rows$word", function() tx_fk_scalar(tx_fk_reg_rows(), "word"),
        function() names(REG_WORDS)),
  tx_fk("REG_EMPIRICAL$*$word",    function() tx_fk_scalar(tx_fk_emp_shapes(), "word"),
        function() names(REG_WORDS)),
  tx_fk("names(REG_WORDS)",        function() names(REG_WORDS),
        function() names(REG_MEASURE_SPELLINGS)),
  tx_fk("names(REG_CONTRASTS)",    function() names(REG_CONTRASTS),
        function() REG_EFFECTS_VALUES),

  # --- the ACRONYM vocabulary is ONE table, checked in both directions ------------------------
  # What a header can print is what an argument can be typed. The pairs are encoded as
  # "OR->odds_ratio" strings because tx_fk() compares value SETS, one direction at a time (the
  # tx_fk_emp_shape_keys idiom), and an acronym naming the WRONG measure must fail too.
  tx_fk("the acronym of every estimand the library composes", reg_word_measures,
        function() {
          a <- c(MEASURE_ACRONYMS, MEASURE_ACRONYMS_REG); paste0(names(a), "->", unname(a))
        }),
  tx_fk("MEASURE_ACRONYMS + MEASURE_ACRONYMS_REG",
        function() {
          a <- c(MEASURE_ACRONYMS, MEASURE_ACRONYMS_REG); paste0(names(a), "->", unname(a))
        }, reg_word_measures),
  # ...and WHICH HALF: a word only a rank level names is one a crosstab can never print.
  tx_fk("the acronyms only a model can print", reg_model_only_words,
        function() names(MEASURE_ACRONYMS_REG)),
  tx_fk("names(MEASURE_ACRONYMS_REG)", function() names(MEASURE_ACRONYMS_REG),
        reg_model_only_words),

  # --- into the estimand vocabulary ----------------------------------------------------------
  tx_fk("REG_LOG_BASE",            function() as.character(REG_LOG_BASE),
        function() REG_MEASURES_VALUES),
  tx_fk("names(REG_LOG_BASE)",     function() names(REG_LOG_BASE),
        function() names(REG_MEASURE_SPELLINGS)),
  tx_fk("REG_MEASURE_SPELLINGS",   function() as.character(REG_MEASURE_SPELLINGS),
        function() REG_MEASURES_VALUES),
  # what reg_formulas() prints in `fit` can be typed straight back into `link`.
  tx_fk("REG_FIT_ONLY_FAMILIES",   function() REG_FIT_ONLY_FAMILIES,
        function() names(REG_FIT_SPELLINGS)),
  tx_fk("REG_FIT_SPELLINGS",       function() unname(REG_FIT_SPELLINGS),
        function() REG_MEASURES_VALUES),

  # --- into the ARGUMENT surface ---------------------------------------------------------------
  tx_fk("TAB_ARGS$values_from",    function() tx_fk_all(TAB_ARGS, "values_from"),
        function() c("MEASURES", "COLOR_SIGNIF_VALUES", "CI_METHODS", "COLOR_SCALES",
                     "DISPLAY_TOKENS",
                     "REG_FAMILIES", "REG_ESTIMANDS", "REG_CHECKS", "TEST_ROWS",
                     "VAR_SHAPES")),
  tx_fk("TAB_ARGS$values_rd",      function() tx_fk_all(TAB_ARGS, "values_rd"),
        function() ls(asNamespace("tabxplor"), pattern = "_rd$")),
  tx_fk("TAB_ARGS$option",         function() tx_fk_all(TAB_ARGS, "option"),
        function() names(TAB_OPTIONS)),
  tx_fk("TAB_ARGS$doc_with",       function() tx_fk_all(TAB_ARGS, "doc_with"),
        function() names(TAB_ARGS)),
  tx_fk("TAB_ARGS$pct$stored",     function() TAB_ARGS[["pct"]][["stored"]],
        function() PCT_TYPES),
  # Every option's per-call twin has a declared row (crosstab in TAB_ARGS, render in EXPORT_ARGS),
  # so this edge is checked outright, with no exception list.
  tx_fk("TAB_OPTIONS$arg",         function() tx_fk_scalar(TAB_OPTIONS, "arg"),
        function() unique(c(names(TAB_ARGS), names(EXPORT_ARGS),
                            tx_fk_all(TAB_OPTIONS, "arg_extra"))))
)


# --- the checker ----------------------------------------------------------------------------------

# Runs at load, below. `keys` lets the test suite hand a deliberately broken edge and see the
# failure. Returns the ORPHAN report invisibly (not thrown -- dead weight, not a wrong number).
#' @keywords internal
#' @noRd
tx_check_foreign_keys <- function(keys = TAB_FOREIGN_KEYS) {
  seen <- list()                                   # target signature -> values referencing it
  for (k in keys) {
    ok  <- unique(c(k$to(), k$allow))
    got <- k$get()
    got <- unique(got[!is.na(got) & nzchar(got)])
    bad <- setdiff(got, ok)
    if (length(bad))
      stop("tabxplor: dangling key in ", k$from, ": ",
           paste(sQuote(bad), collapse = ", "),
           "\n  legal: ", paste(ok, collapse = ", "), call. = FALSE)
    if (isTRUE(k$orphan)) {
      sig <- paste(sort(k$to()), collapse = "\r")
      seen[[sig]] <- unique(c(seen[[sig]], got))
    }
  }
  orphans <- list()
  for (k in keys) {
    if (!isTRUE(k$orphan)) next
    tgt <- k$to()
    sig <- paste(sort(tgt), collapse = "\r")
    left <- setdiff(tgt, seen[[sig]])
    if (length(left)) orphans[[k$from]] <- left
  }
  invisible(orphans)
}

# --- the argument surface's own anti-drift check -----------------------------------------------
# Every covered producer's FORMALS and its declared TAB_ARGS rows are the same set, and every
# surviving formal's default is the declared one -- what makes the generated `@param` blocks safe.
#' @keywords internal
#' @noRd
tx_check_tab_args <- function(producers = c("tab", "tab_plain", "tab_num", "tab_counts",
                                            "tab_many", "tab_build",
                                            EXPORT_PRODUCERS,
                                            "tab_reg")) {
  for (p in producers) {
    fn <- get(p, envir = asNamespace("tabxplor"))
    f  <- setdiff(names(formals(fn)), "...")
    d  <- tab_args_for(p)
    # SCOPED producers: the table owns SOME of the signature, so only the declared rows are checked
    # (tab_build's is just `output`); the other direction is still checked for every producer.
    if (p == "tab_build" || p %in% EXPORT_PRODUCERS) f <- intersect(f, d)
    if (length(setdiff(f, d)))
      stop("tabxplor: ", p, "() has formals with no TAB_ARGS row: ",
           paste(setdiff(f, d), collapse = ", "), call. = FALSE)
    if (length(setdiff(d, f)) && p != "tab_build") {
      # a declared argument that is no longer a formal must ride `...`, or it is unreachable.
      if (!"..." %in% names(formals(fn)))
        stop("tabxplor: ", p, "() declares ", paste(setdiff(d, f), collapse = ", "),
             " but takes neither the formal nor `...`.", call. = FALSE)
    }
    tb <- arg_table_of(p)
    for (k in intersect(f, d)) {
      r  <- tb[[k]]
      if (is.null(r[["default"]]) && is.null(r[["default_for"]])) next
      ov <- r[["default_for"]]
      dd <- if (!is.null(ov) && p %in% names(ov)) ov[[p]] else r[["default"]]
      got <- formals(fn)[[k]]
      # ⚠ rlang::is_missing(), never is.symbol(): a formal with NO default IS the empty symbol, and
      # merely touching it raises "argument is missing".
      if (rlang::is_missing(got)) next
      if (!identical(eval(got, envir = asNamespace("tabxplor")), dd))
        stop("tabxplor: ", p, "(", k, " = ) does not match its declared TAB_ARGS default.",
             call. = FALSE)
    }
  }
  invisible(TRUE)
}

# --- the regression context's own anti-shadow check --------------------------------------------
# new_reg_ctx() and new_reg_shared() are unpacked into ONE scope by every reg_stage_*(), so a name
# declared in both would silently shadow -- list2env() lets the LAST one win.
#' @keywords internal
#' @noRd
tx_check_reg_ctx <- function() {
  dup <- intersect(names(formals(new_reg_ctx)), names(formals(new_reg_shared)))
  if (length(dup))
    stop("tabxplor: new_reg_ctx() and new_reg_shared() both declare: ",
         paste(dup, collapse = ", "), call. = FALSE)
  invisible(TRUE)
}

# The observed comparison must be the LAST tooltip row, because reg_append_empirical_tip() appends
# the multinomial crude level to an already-joined string with " ; " and therefore lands on whatever
# row came last. A new group beyond it would silently move that fragment onto the wrong line.
#' @keywords internal
#' @noRd
tx_check_tooltip_groups <- function() {
  g <- vapply(TOOLTIP_LINES, function(l) l$group %||% 1L, integer(1))
  if (max(g) != TOOLTIP_GROUP_OBS)
    stop("tabxplor: TOOLTIP_GROUP_OBS must be the last TOOLTIP_LINES group; got ", max(g),
         call. = FALSE)
  invisible(TRUE)
}

# Runs at namespace load (R CMD INSTALL / pkgload::load_all()), so a broken reference fails the
# build, at the moment it is made.
tx_check_foreign_keys()
tx_check_tab_args()
tx_check_reg_ctx()
tx_check_tooltip_groups()
