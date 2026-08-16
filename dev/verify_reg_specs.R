# verify_reg_specs.R -- prove that a refactor of tab_reg()'s ARGUMENT BOUNDARY changed nothing.
#
# WHY THIS FILE EXISTS. There is no regression golden and no regression snapshot: `_golden/` is 36
# crosstab cases and `grep -c Model` on `_snaps/golden.md` / `_snaps/render-html.md` is 0. So
# `tab_reg()`'s whole argument surface -- 738 lines resolving 28 arguments, and 30 of the package's
# ~190 user messages -- is asserted only by the `expect_*` calls in the 15 test-*reg*.R files. That
# is exactly the situation dev/verify_color_attrs.R was written for in Phase 19c, where it caught
# that phase's one real regression. This is its regression-side twin.
#
# HOW TO USE IT, around a boundary refactor:
#   Rscript dev/verify_reg_specs.R save  <file.rds>     # on the pre-refactor tree
#   Rscript dev/verify_reg_specs.R check <file.rds>     # after -- must print "IDENTICAL"
#   Rscript dev/verify_reg_specs.R list                 # just enumerate the cases (no fitting)
#
# WHAT IT CAPTURES, per case:
#   messages -- every cli_inform / cli_warn / lifecycle nudge / abort, IN ORDER, class-tagged. This
#     is the field verify_color_attrs.R does not have and this phase most needs: 30 messages live in
#     the region being moved, and several deliberately change. Text AND order are part of IDENTICAL.
#   specs    -- reg_call(x)$fit_spec$specs, i.e. THE resolver's central output. It is stored on every
#     built table (R/tab_reg.R, reg_call_record$fit_spec), so the capture needs no new API and works
#     unchanged on both trees.
#   call     -- the whole reg_call(): families / measures / effects / positive_level /
#     predictor_types / multiplier / crude_keys / split_var / comparison / wt + fit_spec's scalars.
#   cols     -- per fmt column, the stored facts the colour engine, the legend and the plots read.
#   labels   -- as.character() of every NON-fmt column. This is the only cheap window on the four
#     `data` rewrites the boundary performs (design unwrap / labelled / shape / relevel): a shape
#     recode shows up as quantile-group levels, the relevel as reference-row order, the multiplier
#     as the relabelled unit, cleannames and the positive level as the header text.
#   test     -- the `test` tibble's names, nrow and its key tuples.
#
# WHAT IT DOES NOT CAPTURE: the numbers. Coefficients are the reg test files' job; this file is
# about WHAT WAS RESOLVED and WHAT THE USER WAS TOLD.
#
# ⚠ Everything is run through scrub() before comparison. identical() on a closure or a formula
# compares ENVIRONMENTS, and a fresh load_all() makes new ones -- so an un-scrubbed dump would
# report every case as CHANGED. Functions become "<fn>", language becomes its deparse.

suppressMessages(devtools::load_all("~/github/tabxplor", quiet = TRUE))

# deterministic message text: no ANSI, no locale-dependent glyphs
options(cli.num_colors = 1, cli.unicode = FALSE, crayon.enabled = FALSE, cli.width = 200)

# --- the fixture ------------------------------------------------------------------------------
# forcats::gss_cat, thinned deterministically (every 6th row, ~3.6k). The boundary is what is under
# test, not the arithmetic, so N only has to be big enough that every family actually fits.
reg_fx <- local({
  cached <- NULL
  function() {
    if (!is.null(cached)) return(cached)
    g <- forcats::gss_cat
    g <- g[seq(1L, nrow(g), by = 6L), , drop = FALSE]
    g$married <- factor(ifelse(g$marital == "Married", "Married", "Not married"))
    g$party3  <- forcats::fct_lump_n(g$partyid, 2)                       # 3-level nominal
    g$inc3    <- factor(forcats::fct_lump_n(g$rincome, 2), ordered = TRUE)  # ordered
    g$score   <- pmin(g$tvhours, 6L)                                     # 0..6 grouped-binomial
    cached <<- as.data.frame(g)
    cached
  }
})

# --- the cases --------------------------------------------------------------------------------
# Named axes crossed 2-3 at a time inside case FAMILIES -- verify_color_attrs.R's own shape, not a
# full cross (family x effect x measure x trials x multiplier x color x split x compare x shape is
# ~1e5 cells, >90 % of them no-ops).
reg_spec_cases <- function() {
  out <- list()
  add <- function(nm, f) out[[nm]] <<- f

  P <- c("race", "age")                        # a 3-level factor + a numeric, on every case

  # --- estimand.* : family x effect x measure. A FULL cross IS right here -- most cells are
  # enumerated REFUSALS (cheap, no fit) and those messages are exactly what must not move.
  dep_of <- c(auto = "married", binomial = "married", gaussian = "tvhours",
              poisson = "tvhours", quasipoisson = "tvhours",
              multinomial = "party3", ordinal = "inc3")
  for (fm in names(dep_of)) for (ef in c("coefficient", "marginal", "at_reference"))
    for (ms in c("auto", "odds_ratio", "ratio", "difference", "log")) {
      local({
        f <- fm; e <- ef; m <- ms; d <- unname(dep_of[[fm]])
        add(sprintf("estimand.%s.%s.%s", f, e, m), function()
          tab_reg(reg_fx(), d, P, family = f, effect = e, measure = m))
      })
    }

  # --- perdep.* : the per-dependent slicer. `family` / `inverse` / `trials` as scalar, positional,
  # named-complete and named-PARTIAL. The partial named vectors are defect 1's fixtures.
  D2 <- c("married", "party3")
  add("perdep.family.scalar",     function() tab_reg(reg_fx(), D2, P, family = "auto"))
  add("perdep.family.positional", function() tab_reg(reg_fx(), D2, P, family = c("binomial", "multinomial")))
  add("perdep.family.named_full", function() tab_reg(reg_fx(), D2, P,
                                                     family = c(married = "binomial", party3 = "multinomial")))
  add("perdep.family.named_part", function() tab_reg(reg_fx(), D2, P, family = c(married = "binomial")))
  add("perdep.family.short_pos",  function() tab_reg(reg_fx(), D2, P, family = c("binomial")))
  # Phase 20c: `inverse_two_level_factors` (a logical toggling level ORDER) is `outcome_level`
  # (which NAMES the level). The scalar case has no equivalent -- a level belongs to one outcome --
  # so it becomes the partial-named one on the other dependent, keeping the case count.
  add("perdep.inverse.named_part",function() tab_reg(reg_fx(), D2, P,
                                                     outcome_level = c(married = "Not married")))
  add("perdep.inverse.scalar",    function() tab_reg(reg_fx(), D2, P,
                                                     outcome_level = c(party3 = "Other")))
  add("perdep.effect.named_part", function() tab_reg(reg_fx(), D2, P, effect = c(married = "marginal")))
  add("perdep.measure.named_part",function() tab_reg(reg_fx(), D2, P, measure = c(married = "log")))

  # --- trials.* : block M's six aborts + one warn + both auto paths
  for (tv in list(NULL, TRUE, FALSE, 4, c(score = 4), c(score = NA_real_), c(nope = 4), "score", c(1, 2)))
    local({
      t <- tv
      add(paste0("trials.score.", gsub("[^A-Za-z0-9]+", "_",
                                       paste(names(t) %||% "", paste(t, collapse = "-"), collapse = "_"))),
          function() tab_reg(reg_fx(), "score", P, family = "binomial", trials = t))
    })
  add("trials.on_factor",   function() tab_reg(reg_fx(), "married", P, family = "binomial", trials = TRUE))
  add("trials.on_gaussian", function() tab_reg(reg_fx(), "tvhours", P, family = "gaussian", trials = TRUE))
  add("trials.two_deps",    function() tab_reg(reg_fx(), c("score", "married"), P,
                                               family = "binomial", trials = c(score = 4)))

  # --- mult.* : the frozen-frame unit
  for (mu in list("sd", "2sd", 1, 10, c(age = 10), c(age = "2sd"), c(race = 2), c(nope = 2), NULL))
    local({
      m <- mu
      add(paste0("mult.", gsub("[^A-Za-z0-9]+", "_",
                               paste(c(names(m), m), collapse = "_")) , if (is.null(m)) "NULL" else ""),
          function() tab_reg(reg_fx(), "married", P, family = "binomial", multiplier = m))
    })
  add("mult.multinomial_only", function() tab_reg(reg_fx(), "party3", P, family = "multinomial",
                                                  multiplier = "sd"))
  add("mult.mixed", function() tab_reg(reg_fx(), c("married", "party3"), P,
                                       family = c("binomial", "multinomial"), multiplier = "sd"))

  # --- color.* : S4's whole surface (defects 6 and 7 live here)
  cols <- list(t = TRUE, f = FALSE, adj = "adjustment", betw = "between_groups",
               two = c(TRUE, "adjustment"), two_rev = c("adjustment", TRUE), bad = "typo",
               no = "no", auto = "auto")
  for (cn in names(cols)) for (sg in list(NULL, "ignore", "grey_non_signif", "guaranteed_effect", "grey"))
    local({
      cc <- cols[[cn]]; ss <- sg; k <- cn
      add(sprintf("color.%s.%s", k, ss %||% "NULL"), function()
        tab_reg(reg_fx(), "married", P, family = "binomial", color = cc, color_signif = ss))
    })
  add("color.adj.split",  function() tab_reg(reg_fx(), "married", P, family = "binomial",
                                             color = "adjustment", tab_vars = "race"))
  add("color.betw.split", function() tab_reg(reg_fx(), "married", P, family = "binomial",
                                             color = "between_groups", tab_vars = "race"))

  # --- empirical.* : H20/H22, the eff_word x empirical timing
  for (em in c(TRUE, FALSE)) for (fm in c("binomial", "ordinal", "gaussian"))
    for (ef in c("coefficient", "marginal")) local({
      e <- em; f <- fm; ee <- ef
      d <- if (f == "ordinal") "inc3" else if (f == "gaussian") "tvhours" else "married"
      add(sprintf("empirical.%s.%s.%s", e, f, ee), function()
        tab_reg(reg_fx(), d, P, family = f, effect = ee, empirical = e))
    })
  add("empirical.forced_by_color", function() tab_reg(reg_fx(), "married", P, family = "binomial",
                                                      color = "adjustment", empirical = FALSE))

  # --- display.*
  for (dv in c("value", "ci", "prob", "ame", "{or} ({pct})", "{diff}", "{or} ({obs})", "{bad", "n"))
    local({
      d <- dv
      add(paste0("display.", gsub("[^A-Za-z0-9]+", "_", d)), function()
        tab_reg(reg_fx(), "married", P, family = "binomial", display = d))
    })
  add("display.on_marginal", function() tab_reg(reg_fx(), "married", P, family = "binomial",
                                                effect = "marginal", display = "prob"))
  add("display.on_mnl",      function() tab_reg(reg_fx(), "party3", P, family = "multinomial",
                                                display = "prob"))

  # --- shape.* : H12 / H18 / H19
  for (sh in list(NULL, c(age = "quadratic"), c(age = "log"), c(age = "quintiles"), c(age = "4"),
                  c(race = "log"), c(nope = "log"), c(age = "sqrt")))
    local({
      s <- sh
      add(paste0("shape.", gsub("[^A-Za-z0-9]+", "_", paste(c(names(s), s), collapse = "_")),
                 if (is.null(s)) "NULL" else ""),
          function() tab_reg(reg_fx(), "married", P, family = "binomial", shape = s))
    })
  add("shape.with_split", function() tab_reg(reg_fx(), "married", P, family = "binomial",
                                             shape = c(age = "quadratic"), tab_vars = "race"))

  # --- split.* : block W's five aborts
  for (sv in list(NULL, "race", "nope", "married", "age", c("race", "marital")))
    local({
      s <- sv
      add(paste0("split.", paste(s %||% "NULL", collapse = "_")), function()
        tab_reg(reg_fx(), "married", P, family = "binomial", tab_vars = s))
    })

  # --- compare.* / stats.* / baseline
  M3 <- list(m1 = "race", m2 = c("race", "age"), m3 = c("race", "age", "marital"))
  # Phase 20c: `compare` + `baseline` are two `stats =` keys, the baseline riding as the key's value.
  # The case NAMES are unchanged so the pre-rename baseline still lines up, one for one.
  for (cp in c("none", "baseline", "sequential")) for (bl in list(NULL, "m1", "typo", 2L, 99L))
    local({
      c1 <- cp; b <- bl
      st <- if (c1 == "none") NULL
            else if (c1 == "sequential") "compare_sequential"
            else if (is.null(b)) "compare_baseline"
            else stats::setNames(as.character(b), "compare_baseline")
      add(sprintf("compare.%s.%s", c1, if (is.null(b)) "NULL" else as.character(b)), function()
        tab_reg(reg_fx(), "married", M3, family = "binomial", stats = st))
    })
  for (st in list(NULL, FALSE, "none", "all", c("n", "aic"), c("n", "typo"), "linearity", "global"))
    local({
      s <- st
      add(paste0("stats.", paste(as.character(s) %||% "NULL", collapse = "_")), function()
        tab_reg(reg_fx(), "married", P, family = "binomial", stats = s))
    })

  # --- na.* / conf_level / cleannames
  for (nv in c("drop_by_outcome", "drop_by_model", "drop_all")) local({
    n <- nv
    add(paste0("na.", n), function() tab_reg(reg_fx(), c("married", "tvhours"), P,
                                             family = c("binomial", "gaussian"), na = n))
  })
  add("na.default_unset", function() tab_reg(reg_fx(), c("married", "tvhours"), P,
                                             family = c("binomial", "gaussian")))
  for (cl in list(0.95, 0.99, 95, -1, c(0.9, 0.95))) local({
    c1 <- cl
    add(paste0("conf.", paste(c1, collapse = "_")), function()
      tab_reg(reg_fx(), "married", P, family = "binomial", conf_level = c1))
  })
  add("cleannames.true",  function() tab_reg(reg_fx(), "married", P, family = "binomial", cleannames = TRUE))
  add("cleannames.false", function() tab_reg(reg_fx(), "married", P, family = "binomial", cleannames = FALSE))

  # --- formula.* : defect 8 and D's aborts
  add("formula.simple",     function() tab_reg(reg_fx(), married ~ race + age))
  add("formula.compound",   function() tab_reg(reg_fx(), married ~ race + poly(age, 2)))
  add("formula.transformed",function() tab_reg(reg_fx(), log(tvhours + 1) ~ race + age))
  add("formula.both",       function() tab_reg(reg_fx(), married ~ race, P))
  add("formula.plus_models",function() tab_reg(reg_fx(), married ~ race, list(m1 = "race")))
  add("formula.no_preds",   function() tab_reg(reg_fx(), "married"))
  add("predictors.bad_type",function() tab_reg(reg_fx(), "married", 42))
  add("predictors.list_2dep",function() tab_reg(reg_fx(), c("married", "party3"), list(m1 = "race")))

  # --- recursion.* : block B's slicer (defect 1 again, one layer up)
  add("recursion.plain",   function() tab_reg(reg_fx(), c("married", "party3"), M3,
                                              family = c("binomial", "multinomial")))
  add("recursion.perdep",  function() tab_reg(reg_fx(), c("married", "score"), M3,
                                              family = c(married = "binomial", score = "binomial"),
                                              trials = c(score = 4)))
  add("recursion.effect",  function() tab_reg(reg_fx(), c("married", "tvhours"), M3,
                                              family = c("binomial", "gaussian"),
                                              effect = c(married = "marginal")))

  # --- svy.* : S2's design unwrap
  add("svy.design", function() {
    d <- reg_fx(); d$w <- 1 + (seq_len(nrow(d)) %% 5)
    des <- survey::svydesign(ids = ~1, weights = ~w, data = d)
    tab_reg(des, "married", P, family = "binomial")
  })
  add("svy.design_plus_wt", function() {
    d <- reg_fx(); d$w <- 1 + (seq_len(nrow(d)) %% 5)
    des <- survey::svydesign(ids = ~1, weights = ~w, data = d)
    tab_reg(des, "married", P, family = "binomial", wt = "w")
  })
  add("svy.wt_only", function() {
    d <- reg_fx(); d$w <- 1 + (seq_len(nrow(d)) %% 5)
    tab_reg(d, "married", P, family = "binomial", wt = "w")
  })
  add("svy.mnl_marginal", function() {
    d <- reg_fx(); d$w <- 1 + (seq_len(nrow(d)) %% 5)
    tab_reg(d, "party3", P, family = "multinomial", wt = "w", effect = "marginal")
  })

  # --- reref.* : T's 13-clause conjunction, ONE clause off at a time. Nothing else covers this
  # axis, and a wrongly-TRUE `reref` returns a table built from a stale digest -- a WRONG NUMBER,
  # not an error. Each case reports whether the digest path was taken (via fit_spec + the columns).
  base_reref <- list(family = "binomial", ci_method = "wald")
  reref_off <- list(
    on            = list(),
    off_marginal  = list(effect = "marginal"),
    off_profile   = list(ci_method = "profile"),
    off_split     = list(tab_vars = "race"),
    off_trials    = list(outcome = "score", trials = 4),
    off_compare   = list(predictors = M3, stats = "compare_sequential"),
    off_models    = list(predictors = M3),
    off_color_adj = list(color = "adjustment"),
    off_shape     = list(shape = c(age = "quadratic")),
    off_display   = list(display = "prob"),
    off_mnl       = list(outcome = "party3", family = "multinomial")
  )
  for (rn in names(reref_off)) local({
    ov <- reref_off[[rn]]; k <- rn
    add(paste0("reref.", k), function() {
      a <- utils::modifyList(list(data = reg_fx(), outcome = "married", predictors = P,
                                  .fit_cache = new.env(parent = emptyenv())),
                             utils::modifyList(base_reref, ov))
      do.call(tab_reg, a)
    })
  })

  # --- reference.* : block U
  add("reference.factor", function() tab_reg(reg_fx(), "married", P, family = "binomial",
                                             ref = c(race = "Black")))
  # Phase 20c: a multinomial's BASELINE CATEGORY left `reference` (which names the level a predictor
  # is compared against) for `outcome_level` (which names the level of the OUTCOME singled out).
  # Both halves are covered: the capability, and the refusal that now points at the right argument.
  add("reference.outcome",function() tab_reg(reg_fx(), "party3", P, family = "multinomial",
                                             outcome_level = c(party3 = "Other")))
  add("reference.outcome_refused", function() tab_reg(reg_fx(), "party3", P, family = "multinomial",
                                             ref = c(party3 = "Other")))
  add("reference.split",  function() tab_reg(reg_fx(), "married", P, family = "binomial",
                                             tab_vars = "race", ref = c(race = "Black")))
  add("reference.bad",    function() tab_reg(reg_fx(), "married", P, family = "binomial",
                                             ref = c(race = "Nope")))

  # --- add_n / stars / subtext
  # (Phase 20a deleted tab_logit() / multi_logit(); their two cases went with them. The binary
  # outcome is `family = "binomial"` everywhere above, which is what the wrappers forwarded.)
  add("addn.false", function() tab_reg(reg_fx(), "married", P, family = "binomial", add_n = FALSE))
  add("stars.false",function() tab_reg(reg_fx(), "married", P, family = "binomial", stars = FALSE))

  out
}

# --- scrub: make a dump comparable across two load_all()s ---------------------------------------
# ⚠ identical() on a function or a formula compares ENVIRONMENTS. A package closure's environment is
# the namespace, which is recreated by every load_all(), and a formula carries its caller's frame --
# so an un-scrubbed dump reports 100 % CHANGED. Language becomes its deparse; closures become a tag.
scrub <- function(x) {
  if (is.null(x)) return(NULL)
  if (is.function(x)) return("<fn>")
  if (is.environment(x)) return("<env>")
  if (rlang::is_formula(x) || is.call(x) || is.name(x) || is.expression(x))
    return(paste0("<lang> ", paste(deparse(x), collapse = " ")))
  if (is.factor(x)) return(list(kind = "factor", levels = levels(x), values = as.character(x)))
  if (is.data.frame(x)) return(lapply(as.list(x), scrub))
  if (is.list(x)) return(lapply(x, scrub))
  if (is.atomic(x)) return(x)
  paste0("<", class(x)[[1]], ">")
}

# --- capture ------------------------------------------------------------------------------------
# ⚠ cli embeds a SOURCE REFERENCE in a rethrown error ("Caused by error in `f()` at
# tabxplor/R/tab_reg.R:1247:9"). Adding or removing a line anywhere above the failing call therefore
# rewrites the message, and every refactor step would report dozens of false CHANGEs. The reference
# names the same call either way, so it is normalised out -- at COMPARISON time on both sides, so a
# baseline saved before this rule was written still compares cleanly.
# --- DECLARED renames -------------------------------------------------------------------------
# A phase that RENAMES a spec field would otherwise report every case as CHANGED, which hides the
# cases that changed for a real reason. Declaring the rename here is the twin of
# dev/verify_golden_field_delta.R's RENAMED_TEST_COLS: the harness proves the MAPPING, then compares
# everything else -- so the gate keeps biting through the rename instead of being re-baselined blind.
# `to = NULL` means the field is DELETED and its value is not carried anywhere.
#
# Phase 20c: `inverse` (a logical: "model the FIRST level") became `outcome_level` (the level's
# NAME, NA = the family's default). TRUE is the default in both spellings; FALSE had no name to map
# to, so only the default maps and any non-default case is compared on its other fields.
SPEC_RENAMES <- list(          # per-model, reg_call()$fit_spec$specs[[i]]
  list(from = "dependent", to = "outcome", map = identity),
  list(from = "inverse", to = "outcome_level",
       map = function(v) if (isTRUE(v)) NA_character_ else v)
)
FIT_SPEC_RENAMES <- list(      # table-wide, reg_call()$fit_spec
  list(from = "inverse_two_level_factors", to = "outcome_level",
       map = function(v) if (isTRUE(v)) NULL else v)
)
CALL_RENAMES <- list(          # reg_call() itself -- KEY 4's four cross-producer renames
  list(from = "dependent", to = "outcome",   map = identity),
  list(from = "split_var", to = "tab_vars",  map = identity)
)

# Phase 20d: the third declared shape of delta, beside a rename and a deletion -- an ADDITION. A new
# member on a REG_ESTIMANDS row rides `est` into every spec and into reg_call(), so it would report
# every non-aborting case as CHANGED and drown the signal. Declaring it here drops it from BOTH sides,
# which keeps IDENTICAL meaningful: what the run then proves is that NOTHING ELSE moved.
# ⚠ each entry is a member of one estimand row (`$est` / `$ests[[i]]`), named for the phase that added
# it. Remove an entry once its baseline has been re-saved past it.
# Phase 20e: EMPTY -- 20d's `engine` is in the baseline now (re-saved on the pre-20e tree), so it is
# COMPARED rather than dropped. Add a member here only for the duration of the phase that adds it.
EST_ADDITIONS <- character(0)

apply_renames <- function(c1) {
  if (is.null(c1$out)) return(c1)
  # ⚠ rename IN PLACE. identical() on two lists compares names in ORDER, so deleting the old field
  # and appending the new one reports every case as CHANGED for a reason that is not the rename.
  rename <- function(x, rules) {
    for (r in rules) {
      i <- match(r$from, names(x))
      if (is.na(i)) next
      v <- r$map(x[[i]])
      if (is.null(r$to)) { x[[i]] <- NULL; next }
      x[i] <- list(v)            # `[<-` with a list keeps a NULL value as a real element
      names(x)[i] <- r$to
    }
    x
  }
  drop_added <- function(e) { if (is.list(e)) e[EST_ADDITIONS] <- NULL; e }
  fix_one <- function(o) {
    if (is.null(o) || is.null(o$call) || is.null(o$call$fit_spec)) return(o)
    o$call$fit_spec$specs <- lapply(o$call$fit_spec$specs, function(s) {
      s <- rename(s, SPEC_RENAMES); s$est <- drop_added(s$est); s })
    o$call$fit_spec       <- rename(o$call$fit_spec, FIT_SPEC_RENAMES)
    o$call                <- rename(o$call, CALL_RENAMES)
    o$call$est            <- drop_added(o$call$est)
    if (!is.null(o$call$ests)) o$call$ests <- lapply(o$call$ests, drop_added)
    o
  }
  # a tabxplor_tabs case dumps a LIST of tables
  c1$out <- if (!is.null(c1$out$call)) fix_one(c1$out) else lapply(c1$out, fix_one)
  c1
}

strip_srcref <- function(m) gsub("(at [^ ]+\\.R):[0-9]+:[0-9]+", "\\1:#", m)

capture_case <- function(f) {
  msgs <- character()
  note <- function(kind, cnd)
    msgs <<- c(msgs, paste0("[", kind, "] ", gsub("[\r\n]+", " ", conditionMessage(cnd))))
  res <- withCallingHandlers(
    tryCatch(f(), error = function(e) { note("error", e); NULL }),
    message = function(m) { note("i", m); invokeRestart("muffleMessage") },
    warning = function(w) { note("!", w); invokeRestart("muffleWarning") })
  list(messages = msgs, table = res)
}

dump_table <- function(x) {
  if (is.null(x)) return(NULL)
  if (is.list(x) && !is.data.frame(x))                       # a tabxplor_tabs
    return(lapply(x, dump_table))
  fmt <- vapply(x, is_fmt, logical(1))
  cl  <- reg_call(x)
  tt  <- attr(x, "test", exact = TRUE)
  list(
    call = scrub(cl),
    # per fmt column: every stored fact the colour engine, the legend and the plots read
    cols = lapply(as.list(names(x)[fmt]), function(cn) {
      col <- x[[cn]]
      list(name = cn, color = get_color(col), color_bg = get_color_bg(col),
           color_signif = get_color_signif(col), scale = get_scale(col),
           display = unique(get_display(col)), ci_method = get_ci_method(col),
           col_var = get_col_var(col), role = get_role(col),
           model_family = get_model_family(col), conf_level = get_conf_level(col),
           n = length(col))
    }),
    # the only cheap window on the four `data` rewrites (shape recode / relevel / multiplier
    # relabel / cleannames + positive level all surface as label text or row order)
    labels = lapply(as.list(names(x)[!fmt]), function(cn) list(name = cn, v = scrub(x[[cn]]))),
    test = if (is.null(tt)) NULL else
      list(names = names(tt), nrow = nrow(tt),
           keys = if (nrow(tt)) scrub(tt[intersect(c("var", "col", "test", "outcome"), names(tt))]) else NULL),
    subtext = get_subtext(x),
    grouped = inherits(x, "tabxplor_grouped_tab"),
    kind    = tryCatch(tab_kind(x), error = function(e) "<err>")
  )
}

run_cases <- function(only = NULL) {
  cases <- reg_spec_cases()
  if (!is.null(only)) cases <- cases[grepl(only, names(cases))]
  n <- length(cases); i <- 0L
  purrr::imap(cases, function(f, nm) {
    i <<- i + 1L
    if (i %% 25L == 0L) cat("  ...", i, "/", n, "\n")
    cap <- capture_case(f)
    list(messages = cap$messages, out = dump_table(cap$table))
  })
}

# --- main ---------------------------------------------------------------------------------------
`%||%` <- function(x, y) if (is.null(x)) y else x
args <- commandArgs(trailingOnly = TRUE)
mode <- if (length(args) >= 1) args[[1]] else "check"
path <- if (length(args) >= 2) args[[2]] else "/tmp/reg_specs.rds"
only <- if (length(args) >= 3) args[[3]] else NULL

if (identical(mode, "list")) {
  nm <- names(reg_spec_cases())
  cat(length(nm), "cases:\n"); cat(paste0("  ", nm, collapse = "\n"), "\n")
  quit(save = "no")
}

t0  <- Sys.time()
got <- run_cases(only)
cat("cases:", length(got), "  elapsed:",
    round(as.numeric(difftime(Sys.time(), t0, units = "secs")), 1), "s\n")
nul <- names(got)[vapply(got, function(g) is.null(g$out), logical(1))]
cat("cases returning no table (an abort, expected for the refusal axes):", length(nul), "\n")

if (identical(mode, "save")) {
  saveRDS(got, path, version = 2)
  cat("saved to", path, "\n")
} else {
  ref    <- readRDS(path)
  norm   <- function(d) lapply(d, function(c1) {
    c1$messages <- strip_srcref(c1$messages)
    apply_renames(c1)
  })
  ref    <- norm(ref); got <- norm(got)
  common <- intersect(names(ref), names(got))
  gone   <- setdiff(names(ref), names(got)); new <- setdiff(names(got), names(ref))
  bad    <- common[!vapply(common, function(k) identical(ref[[k]], got[[k]]), logical(1))]
  if (length(gone)) cat("MISSING cases:", paste(gone, collapse = ", "), "\n")
  if (length(new))  cat("NEW cases:",     paste(new,  collapse = ", "), "\n")
  if (!length(bad) && !length(gone) && !length(new)) {
    cat("IDENTICAL -- ", length(common),
        " cases: every message, spec, column attribute, label and test key matches.\n")
  } else {
    cat("CHANGED in", length(bad), "case(s):\n")
    for (b in utils::head(bad, 60L)) {
      a <- ref[[b]]; z <- got[[b]]
      cat(" *", b, "\n")
      if (!identical(a$messages, z$messages)) {
        cat("    messages:\n")
        for (m in setdiff(a$messages, z$messages)) cat("      - ", m, "\n", sep = "")
        for (m in setdiff(z$messages, a$messages)) cat("      + ", m, "\n", sep = "")
        if (setequal(a$messages, z$messages) && !identical(a$messages, z$messages))
          cat("      (same set, different ORDER)\n")
      }
      if (!identical(a$out, z$out)) {
        if (is.null(a$out) || is.null(z$out)) { cat("    table: ",
          if (is.null(a$out)) "none -> built" else "built -> none", "\n", sep = ""); next }
        for (f in union(names(a$out), names(z$out))) {
          if (identical(a$out[[f]], z$out[[f]])) next
          cat("    $", f, ":\n", sep = "")
          # ⚠ print the DIFFERING elements, not the first 12: `cols` is one nested list per column, so
          # head(unlist(.), 12) showed column 1 whatever moved -- twice identical lines, and a real
          # delta invisible (measured in Phase 20d, on a column-11 attribute).
          av <- unlist(a$out[[f]]); zv <- unlist(z$out[[f]])
          if (length(av) == length(zv) && !is.null(names(av)) && identical(names(av), names(zv))) {
            k <- which(av != zv | xor(is.na(av), is.na(zv)))
            for (kk in utils::head(k, 12L))
              cat("      [", names(av)[kk], "] - ", substr(av[[kk]], 1, 100),
                  "\n                 + ", substr(zv[[kk]], 1, 100), "\n", sep = "")
            if (length(k)) next
          }
          cat("      - ", substr(paste(utils::head(av, 12), collapse = " | "), 1, 220),
              "\n      + ", substr(paste(utils::head(zv, 12), collapse = " | "), 1, 220), "\n", sep = "")
        }
      }
    }
  }
}
