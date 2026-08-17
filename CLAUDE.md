# tabxplor — AI Assistant Guide

## Package Purpose

`tabxplor` is a public CRAN R package (v1.3.1) maintained by me for creating, manipulating, and formatting color-coded cross-tabulations. It uses `data.table` for fast aggregation internally and exposes a `tidyverse`-compatible API via custom `tibble` subclasses. Tables can be exported to Excel (with colors), HTML, Markdown, and ggplot. All numeric cells are stored as `vctrs` record vectors (`tabxplor_fmt`) carrying full statistical metadata, enabling lossless display switching.

---

## Repository Map

```
R/
├── fmt_class.R     (~6425 L) Core type: tabxplor_fmt vctrs record, getters/setters, new_fmt() +
│                              fmt_field_names (the 21 fields; s +n_eff, z5 +obs, z8 +gap_se) + FMT_FIELD_DOC
│                              (one gloss each, build-time-exhaustive, behind `?fmt`'s @eval'd roll-call --
│                              the hand-written one still named `in_totrow`, deleted in 19f)
│                              + DERIVED fmt_col_attrs (16 attrs; 19n +col_group = WHICH SUB-POPULATION a
│                              column's block belongs to -- a spread level or a split_var group, "" otherwise.
│                              Both producers WELDED it into `col_var` as "{level}<br>{col_var}", which three
│                              backends recovered by sniffing for an html tag `tab_wrap_text(brk="<br>")` also
│                              emits; stored, it composes only where two lines are wanted. fmt_col_block() /
│                              tab_col_blocks() = THE (col_var, col_group) identity, key + one-line label)
│                              + 19m-i's **TAB_PLACEHOLDER_COL_VARS** = the six `col_var` values that
│                              are NOT a variable name (all_col_vars / no_col_var / no_row_var / "" /
│                              "no" / NA), behind TWO deliberately distinct predicates:
│                              **is_real_col_var()** of a STORED attribute and **is_placeholder_var()**
│                              of a build-time variable NAME (⚠ as.character(): the build passes
│                              symbols, and `sym == "x"` coerces while `sym %in% "x"` errors). Eight
│                              hand-written filters spelled between two and six of them, exactly one
│                              spelled all six, and the exported tab_shape() spelled two -- so it
│                              reported "no_col_var" as a column variable. NOT folded in:
│                              detect_totcols()'s `== "no_col_var"` (that asks "is this the TOTAL
│                              column") and quo_miss_na_null_empty_no() (a deparsed user expression)
│                              + 19a's **fmt_attr_rules** = HOW each attribute is carried (neutral/merge/arith/scalar
│                              + 20a's `write` = the attribute's own SETTER, so `fmt_attr<-()` validates
│                              exactly as set_scale() does and a build-time stopifnot refuses an
│                              attribute with no writer) driving all 4 reconstructors through
│                              fmt_attrs_of/_merge/_arith + fmt_ptype_attrs -- the 7 hand-written 14-attribute
│                              lists are GONE, adding an attribute is 2 lines, a build-time stopifnot enforces it,
│                              and vec_ptype2 got ~2x faster; +fmt_base (the n_eff->tot_n->n coalesce, 5 sites);
│                              z13 +conf_level = the level THIS column was built at, so the engine's four
│                              thresholds stop reading the global option -- TWO accessors, and the split is
│                              load-bearing: fmt_conf_level_attr (RAW, the 6 reconcilers, so a bind carries
│                              "unknown" forward) vs get_conf_level (resolved, option fallback); stamped by
│                              ONE tab_stamp_conf_level() sweep per build tail) (17a: moved here
│                              from tab.R, = new_fmt formals minus the fields, so it can't miss an attr);
│                              format/pillar methods, vctrs arithmetic/casting;
│                              **fmt_attr(x, name)** / **`fmt_attr<-`** (20a, KEY 3) = THE generic
│                              column-attribute accessor -- the RAW stored value with the declared
│                              `neutral` default, writing through fmt_attr_rules$write. The named
│                              accessors are the TAUGHT surface, this is the PROGRAMMATIC one, and it
│                              is what stops the ~23-function family growing with the table: a 17th
│                              attribute needs no accessor at all. ⚠ the HOT PATH stays hand-written
│                              (get_col_var / is_totrow / get_scale). ⚠ get_conf_level / get_degf /
│                              get_basis are RESOLVERS (option fallback / NA->Inf / ""->"n"), not raw
│                              reads, which is why they stay internal; set_diff_type ->
│                              **set_ref_type** (old name soft-deprecated: the pair did not share a
│                              stem and its validation was commented out);
│                              get_num()/set_num() (the read + write display maps -- 19m-iii: their
│                              vocabulary is R/tab-display.R's DISPLAY_TOKENS, which asserts at BUILD
│                              time that the two agree with it and with each other, after set_num
│                              was found to be MISSING pct_ci/mean_ci/pvalue -> vec_arith wrote
│                              nothing at all on those columns; tabxplor_display_fields and
│                              tabxplor_display_aliases moved there too),
│                              color engine (measure_facts = THE MEASURES accessor, folds a row's `guar`
│                              per-policy override [z4: contrib only] + defaults its `bounds` [z8];
│                              per-policy `guar` override + (z13) its per-SCALE `by_scale` one, folded from
│                              the plan's new `scale_key` -- so the legend's glyphs/unit follow the scale
│                              ACTUALLY used (D4), byte-identical for every pre-z13 measure by construction;
│                              fmt_gap_scale_key = D2's dispatch, and its ORDER is the contract (a poisson
│                              count AME and a raw poisson coef are identical in type/ci_type/model_family,
│                              only `var` separates them); legend_gap_baseline = "this column IS the
│                              baseline" (keys on the stored `obs` being empty, never on the plan's gate);
│                              measure_policy(measure, policy, x) = its twin, applies a row's
│                              `force_policy` -- z8-B: a PREDICATE ON THE COLUMN for both gap measures
│                              (fmt_gap_force_policy: an all-NA `gap_se` = no test here -> `ignore`),
│                              which is how ruling Q1(b) lands with no 12th column attr and no
│                              display-string match, and which also un-greyed between_groups under
│                              method="profile"; ONE call site, fmt_color_plan, the legend inherits it;
│                              measure_own_ref = the z5 predicate "this
│                              measure's baseline is ANOTHER COLUMN" (it names itself in the legend +
│                              resolves its ref phrase AND its interval NAME per channel [z8]); fmt_resid =
│                              the adjusted std. residual DERIVED from pvalue+sign(ctr), no field, backs
│                              the `resid` display token + tooltip; fmt_est_field/fmt_est_of [z9] = the
│                              ONE ci_type -> estimate-field rule (or/ratio/diff), shared by
│                              fmt_gap_parts, reg_write_group_gap and the crude numeric overlay;
│                              fmt_gap_parts = the ONE
│                              estimate-vs-`obs` decomposition (mult/est/obs/ok/null-sign) behind
│                              fmt_adjustment_score (the z5 score, folded around 1 [ci_type or/ratio] or 0
│                              [diff], signed AWAY-FROM/TOWARD THE NULL so a protective effect reads like a
│                              risky one) + z8's fmt_gap_raw/_bounds/_p; fmt_gap_bounds = the interval OF
│                              THE SCORE (|gap| re-signed by the null direction), so every plan branch works
│                              on it unchanged; z17: **EST_SCALES + est_scale_key + fmt_scale_of** =
│                              what a column ESTIMATES (neutral / transform / axis unit key / estimate
│                              field / the ladder the ESTIMATE lives on / the adj_* one its GAP reads)
│                              -- ONE dispatch whose ORDER is the contract, of which fmt_gap_scale_key
│                              and ci_center are now LOOKUPS (fmt_center_field = its estimate-field
│                              half, called WITHOUT a display because it answers about the INTERVAL);
│                              legend_guide_spec = the colour legend as a real ggplot GUIDE (NULL when
│                              the plotted columns form >1 legend_group_by_body group -- one ggplot
│                              scale, one ladder); fmt_point_palette = the 8 slot colours to paint a
│                              MARK with (print's text slots are all black, so a point borrows its
│                              grey ramp: in a forest plot DIRECTION is the position, not the hue);
│                              fmt_color_plan/fmt_color_slots/fmt_color_channels;
│                              per-side fold + findInterval; slots 1-4 over / 5-8 under; 17d: fmt_color_plan
│                              reads MEASURES for raw/scale/sig_source/gate_row + z5's std_when="additive"
│                              (the scale keys off the ESTIMATE's ci_type, since Model_OR and Model_AME
│                              are both type "row") -- no switch arms; legacy
│                              strings decoded once at the boundary [color_decode_legacy], color_measure_policy
│                              + single0 GONE, stored color always clean; broadcast helpers get_ref_field);
│                              colour legend + footer (16e ONE model): MEASURES table = per-measure facts
│                              (word/glyph/ref_kind/unit/has_ref_lead + 17d engine facts, one row not ~5 arms) ->
│                              legend_specs -> legend_reg_adapter (reg emp+model fold: share ref-label,
│                              neutralise the additive AME/beta subject) -> legend_resolve_spec (every
│                              per-channel fact into the spec ONCE) -> legend_tokens_terse/_prose = DUMB
│                              templates (no switch/is_reg) -> legend_group_by_body (group by RENDERED body,
│                              not a sig string -> can't drift) -> legend_render_line. legend_name_list =
│                              prefix normalise [<br>/U+202F -> U+00A0, cap 6 +N vars]. tab_footer_streams =
│                              THE ordered footer as typed token-streams (weight -> Model: -> legend -> stars
│                              -> subtext), render_footer = per-medium render+join (role-aware console
│                              subtle); every backend calls these two (was 5x re-ordered + 2x prep fields).
│                              Plain one-liners tab_weight_line/reg_model_line/tab_stars_legend wrap as
│                              1-token streams. legend_export_style() = options(tabxplor.legend_style)
│                              terse-in-exports. contrib legend = x N BOTH sides "vs the mean".
│                              19c (KEY 4): MEASURES gains its VOCABULARY beside its arithmetic, so
│                              names(MEASURES) IS the allow-list and adding a measure is genuinely
│                              ONE row -- `channels` (text/bg eligibility: 5 allow-lists -> 1, closing
│                              D4), `producers` (tab/reg: what refuses a reg-only measure in tab(),
│                              and GENERATES the "that is a tab_reg measure" hint), `applies_to`
│                              (pct/num), `builds` (diff/or/contrib = which per-cell fields the
│                              pipeline must compute; its "contrib" value IS "the CONTRIBUTION pass
│                              stamps this one" -- 19l deleted the measure_stage() wrapper, which
│                              said that in the vocabulary of a step 19j had removed -- and it
│                              absorbed jmv_tab3_arming), `requires` (always/gated, keys ref/ci/
│                              chi2/totrow/empirical/interaction -- 5 copies of "a comparison colour
│                              needs a reference and its interval" -> 1), `ref_auto`, `auto_for`
│                              (per channel, per CONTEXT pct/num/counts/or_table/reg_diff/reg_ratio:
│                              the THREE `color = TRUE` cascades that could disagree -> 1), `method`
│                              (legend_method_name's 3 leading arms), `subject`, `caveat`. Read ONLY
│                              through measure_key/_stored/_builds/_stage/_forces/_requires/_applies/
│                              _kind_keyed/_auto/_validate (THE validator, shared by the argument
│                              boundary normalize_color_spec(producer="tab") and the storage one
│                              resolve_color_channels) + COLOR_ALIASES = the declared spelling table
│                              behind color_decode_legacy ("ci" a row, not a 3rd switch arm).
│                              WARNING at that boundary: DECODE first, normalise second -- measure_key
│                              resolves an alias to its MEASURE, so normalising first drops the policy
│                              half of diff_ci/after_ci/ci. `word` is a CLOSURE (gettext at render AND
│                              statically extractable) -> `word_i18n` + the hand-kept potools anchor
│                              DELETED. Also folded: fmt_stars_applicable (= sig_source=="pvalue"),
│                              partial_test + the plot gap channel (= measure_own_ref), the
│                              contrib-needs-totrows warning (= requires), get_reference (= measure_key)
├── tab.R           (~3915 L) Main API. 19l SPLIT it (7918 L -> 3915: four unrelated subsystems left
│                              for tab-leaf.R / tab-chi2.R / tab-display.R / tab-deprecate.R, and the
│                              six helpers only tab-steps-legacy.R calls went there. Whole functions,
│                              no behaviour change; tab.R sorts AFTER every tab-*.R in R's C
│                              collation, so a new file may read tab.R's top-level objects, not the
│                              reverse -- and the DERIVED globalVariables() tail must stay last).
│                              What remains: tab(), the tab_build() stages, the ctx, tab_prepare(),
│                              tab_spread(), tab_transpose(), the variable-model readers and the
│                              colour-spec normaliser. tab_many() (19h: a translating SHIM over tab(), 10
│                              formals not 42 -- tab_deprecate_many() maps chi2->test, totrow/totcol
│                              ->tot, compact->output_list, and tab_deprecate_na_drop_all() maps
│                              na_drop_all=c(a,b) -> filter=!is.na(a)&!is.na(b), which is EXACT: both
│                              apply immediately before tab_prepare(). Only the 5 leading positional
│                              slots are taken -- the two functions' 6th formals differ, so an unnamed
│                              6th is REFUSED, not silently mis-assigned. It keeps its historical
│                              shape by unwrapping a length-1 result ITSELF, so `output = "legacy"` is
│                              DELETED and tab()'s return is a function of `output_list` alone;
│                              `tabxplor.output_kable` renders and no longer decides a class),
│                              tab_plain(), tab_num(),
│                              tab_apply_reference() (Phase 7f carve; Phase 9d: matrix-sweep internals;
│                              14z: also the empirical-OR Woolf CI [ci_or on the {level j, ref2 level} x
│                              {row i, ref row} 2x2, gated by tabs_totn!=NULL = a color_signif/stars ask;
│                              tab_plain threads conf_level/stars/color_signif] so color_signif works on OR),
│                              leaf_wide_pct() + build_total_rows()/finalize_total_rows() (Phase 9d:
│                              base-R/matrix leaf math for tab_plain pct/tot_n + total rows).
│                              17f: the leaves are WRAPPER/CORE splits -- public tab_plain()/tab_num()
│                              (NSE defuse + validate + normalize colour) -> shared resolver
│                              plain_resolve()/num_resolve() -> resolved-args core plain_core()/num_core()
│                              (pure fmt build, returns PRE-FINALISE). tab_transform calls the CORES
│                              directly, so forcing runs ONCE + colour finalises ONCE downstream (no
│                              double finalize, no .color_deprecate). df=/num= build normally then pull
│                              get_num() per cell via leaf_extract_raw(); shared tails leaf_totrow_tottab()
│                              + leaf_rename_totals(). Both public leaves take a real `display =` (19d
│                              tail) and run the SAME tab_apply_display() the pipeline runs, so the
│                              `OR` retirement route is lossless on them too -- the leaf and the
│                              wrapper speak ONE display grammar; tab_apply_display() also takes a BARE
│                              field name ("n" == "{n}") and "auto" as a no-op, which is what let the
│                              jamovi writer delegate to it instead of stamping a literal "{or}".
│                              tab_apply_reference() = the ONE reference executor
│                              (tab_num's diff_index_mean twin + inline calculate_refrows copy DELETED);
│                              its `dichotomise` = "this col_var is SHOWN as first-level-vs-the-rest"
│                              (carried from `lv1`), so the odds ratio is the TRUE binary one on both
│                              merge paths -- tab() pre-merges before the leaf, jmvtab defers it and
│                              the surviving level is also ref2, which made `or` come out 1 everywhere.
│                              display_write_col() = THE per-column display-template writer, shared by
│                              build-time tab(display =) and post-hoc set_display(col, "num_ci")
│                              (fmt_apply_num_ci DELETED: the two copies disagreed on every total row).
│                              D22 is PER-CELL there -- a template is written only where every one of
│                              its fields exists -- and D23 (display_refuse_mismatch) refuses two
│                              EFFECT geometries, never a LEVEL beside a comparison interval
│                              ("48% [-3;+4]" IS the flagship cell). WARNING: its `across()` callback
│                              must stay a NAMED function -- dplyr inlines an anonymous one into the
│                              data mask, `r$col` yields NULL, and across() DROPS the column.
│                              19j (KEY 5) — **ONE AGGREGATE CORE**: the leaf computes the cells, THEIR
│                              INTERVAL and the whole-table TEST, because that is where the plan is.
│                              **leaf_ci_plain()** (beside tab_apply_reference, sharing its matrices and
│                              its `ra`/refcols) = tab_ci()'s per-cell arithmetic with the plan
│                              RECONSTRUCTION removed: the 8-branch case_when collapses to 5 scalar
│                              lines (in a factor leaf pct_base/var_kind are column-invariant, so
│                              `ci_able` IS `pct != "no"` and the direction IS `pct`). ⚠ it reproduces
│                              group_last_pos()'s LAST-in-group reference row (tab_apply_reference's own
│                              ref_abs takes the FIRST -- they coincide, but the class of risk is gone),
│                              tab_ci()'s ungroup-only-under-diff_row asymmetry, and diff_col's
│                              `ref_n` read at the group's TOTAL row (invisible unweighted, wrong on
│                              every design-based col-% table). Shared verbatim with jmv_tab3_reref, so
│                              the two cannot fork. **leaf_chi2()/leaf_chi2_num()/leaf_test_view()** call
│                              the SAME chi2_compute_test()/chi2_write_contrib() the step calls -- what
│                              moved is not the arithmetic but the QUESTION. ⚠ leaf_test_view() applies
│                              `comp = "all"` as a LOCAL ungrouping: tab_chi2() ungrouped the table it
│                              RETURNED, so whether a comp="all" table came back grouped depended on
│                              whether a test ran (and the tier-2 test cache, which skips the step,
│                              returned a different CLASS). plain_core gains ci/ci_scale/test/deff,
│                              leaf_finish a `test`; tab_apply_tests() is DELETED and the ordering
│                              invariant is STRUCTURAL. tab_plain() gains a public `ci`/`ci_method`.
│                              tab_prepare() (20g-ii: + **tab_collapse_levels()**, the applier of
│                              row-model.R's collapse spec, called there one line BEFORE
│                              tab_lump_others() so a merged level's COMBINED count faces
│                              `other_if_less_than` and the spec keys on the RAW labels. Reached by
│                              the internal `.levels_collapse` on tab() AND tab_reg(); pre-aggregate,
│                              so pct bases / tot_n / n_eff / the tests all follow with no code, and
│                              it is provably tab() on a frame the user collapsed themselves),
│                              tab_spread(), tab_get_vars(),
│                              tab_render_vars() (Phase 10c: robust group_vars-based role detection +
│                              graceful degrade, used by print + exporters),
│                              tab_add_n_pct() (shared add_n/add_pct, used by tab_many + tab_counts).
│                              tab_build() = staged pipeline over a TYPED ctx (17e: new_ctx(), one
│                              defaults list; 19i: it declares every STAGE PRODUCT too -- 54 declared
│                              vs ~81 live had left 27 undeclared, and an undeclared field is ABSENT,
│                              so list2env() creates no binding and its own is.null() guard ERRORS
│                              [19a's D7 class]; utils::globalVariables() for those bindings is now
│                              DERIVED from new_ctx() + CTX_SETTINGS_LOCALS at the END of tab.R --
│                              ⚠ it must stay there, new_ctx()'s defaults call conf_level_default(),
│                              defined further down -- replacing the ~70-name hand mirror in
│                              fmt_class.R that had outlived a field it named):
│                              tab_setup (builds the SETTINGS SPINE ctx$settings = rows/cols/pairs star
│                              schema; pairs REPLACES pct_vect/ref_vect -- the axes meet only there) /
│                              tab_prepare_pop / tab_aggregate / tab_build_tables (Phase 9a: the OUTER
│                              row_var map -> tab_build_one, + tab_rowvar_ctxs, which 17e slices by KEY
│                              -- length heuristic GONE, and 19i STOPS THERE: it used to slice the
│                              spine only to re-flatten every column into the same bare names the ctx
│                              already carried) ; tab_transform / tab_assemble_tables are SCALAR
│                              over one row_var ; tab_assemble_output (merge/pvalue/unwrap);
│                              tab_lump_others/tab_cleannames_relabel (extracted from tab_prepare).
│                              19i: **ctx_settings_locals()** = the spine is the ONLY carrier. Each
│                              stage opens `list2env(ctx_settings_locals(ctx), environment())`, which
│                              projects settings$rows/cols/pairs into the bare names the resolution
│                              blocks read (pre-slice a VECTOR over row_vars, post-slice the scalar --
│                              the same property the flat duplicates had, which is why they existed);
│                              tab_setup writes NEITHER the 15 resolved duplicates NOR the raw inputs
│                              the spine owns (`SPINE_OWNED_INPUTS` are DELETED from the ctx), so a
│                              bare-name read cannot find a pre-resolution value. `na` joins the spine
│                              at its two real grains (pairs$na / rows$na_num), `lvs`/`lv1` at theirs.
│                              CTX_SETTINGS_LOCALS declares the projected names (build-time assert).
│                              **leaf_finish()** = the two leaves' shared result tail (row-index
│                              declaration -> group-or-not -> new_tab/new_grouped_tab WITH the table's
│                              own `spec` -> tab_stamp_inference -> leaf_extract_raw): num_core passed
│                              NO meta at all, so a direct tab_num() had no spec$kind and no vars$wt
│                              (nothing for the weight footer to read). **leaf_inference_setup()** =
│                              the 6 preamble statements they share (basis/design_on/design_flat/
│                              want_neff/use_raw); the divergent halves stay local, with the reason.
│                              **num_total_postprocess()** = num_core's two identical post-rollup
│                              blocks. ⚠ build_total_rows() and num_rollup() are NOT merged: base::sum
│                              over split() vs data.table gforce is a 1-ULP contract on both sides.
├── tab-agg.R        (~965 L) Aggregate-core (Phase 2-3) + 19j's **CI_GEOMS + ci_dispatch()** = THE
│                              interval GEOMETRY vocabulary beside the method one: one row per
│                              (kind cell|diff x var_kind pct|mean x scale diff|ratio) carrying the
│                              `engine`, the `method_slot` that names it, the `scale_key` it makes
│                              the column ESTIMATE and (19m-i) **`ref_cell`** = does the cell that IS
│                              the reference keep its OWN interval. Read only through ci_geom/_scale/
│                              _method/_ref_cell/ci_dispatch. Its 3 consumers (the factor leaf,
│                              num_core, the superseded tab_ci step) held SIX copies of that rule --
│                              which is how D8 happened (a chain that could name a method the bounds
│                              were never built with).
│                              ⚠ the engine call is written per ROW, never one do.call over a shared
│                              arg list: the proportion engines take `df=`, the mean ones `df_design=`.
│                              19m-i: the reference-cell MECHANISM stays the caller's (tab_ci NAs the
│                              base, num_core the results -- they genuinely differ on a mean cell) but
│                              the DECISION is `ref_cell`: a CELL interval compares each cell to 0 %,
│                              not to a reference, so every cell keeps it (the total row included); a
│                              CONTRAST interval blanks the row it would compare to itself. Two of
│                              the three consumers had it wrong, so a factor `ci = "cell"` table's
│                              total row showed no bracket while a numeric one's did.
│                              20c (KEY 4): CI_METHODS gains a 5th slot **`model`** (wald/profile) --
│                              `tab_reg(method =)` had its own argument, name and vocabulary purely
│                              because it belongs to the other producer; `ci_method` is now ONE
│                              grammar for both, and a bare "profile" means that slot. +
│                              **CI_SLOT_PRODUCER** / ci_slots_of(): which producer offers each slot
│                              (a crosstab has no model interval), declared as a named vector rather
│                              than by restructuring CI_METHODS -- that would touch
│                              default_ci_method(), the validation loop, CI_GEOMS$method_slot and
│                              CI_METHOD_LABELS for one fact.
│                              + z16-iiiii's **CI_METHODS** = THE interval-method
│                              vocabulary (4 kinds x their legal values, first = default), from which
│                              default_ci_method() derives and resolve_ci_method() validates -- so the ONE
│                              public `ci_method = c(cell=, diff=, mean_diff=, mean_ratio=)` named vector
│                              (partial, like `ref`) cannot mean different things in tab/tab_many/tab_num/
│                              tab_counts/tab_ci. + df_clean() (the df sanitiser, was inlined 4x).
│                              num_derive_stats/num_rollup, num_moment_scan
│                              + tab_aggregate_num (numeric tier-1 producer, Phase 7d-i),
│                              CI engine (ci_pivot/ci_wilson/ci_newcombe/ci_katz_rr/ci_mean_diff2/
│                              …: 14b's Katz log-RR + 14v-ii's ci_mean_ratio [robust/quasipoisson/
│                              poisson] are the RATIO-scale intervals ci_type="ratio"; ci_mean_diff2
│                              gains method welch/student; ci_or = Woolf log-OR for the empirical crude
│                              OR, used by tab_reg(empirical) AND (14z) tab()'s OR colour via
│                              tab_apply_reference; RULE B [§48]: numeric CIs are t where a variance is estimated, z
│                              otherwise -- NOT stars-gated; ci_pivot guards df<=0 -> NA; zscore_formula (+ z4's exported conf_level_to_z wrapper) =
│                              the normal quantile, 17a: moved here from tab.R), agg_chi2/agg_anova
│                              (Phase 18j: both also emit the whole-table EFFECT SIZE -- Cramer's V/phi
│                              from agg_chi2's uncorrected chi2, eta^2 = SSB/SST from agg_anova) +
│                              agg_fisher (exact test on small weak factor tables, size/N-guarded ->
│                              simulate fallback)
├── survey-design.R  (~425 L) z14-i: THE survey-design BOUNDARY + the constructors + the robust overlay.
│                              svy_is_design() = the ONE class list (4 entry points accept, tab_counts
│                              REFUSES); svy_unwrap_data() = the ONE unwrap, called by tab/tab_many/
│                              tab_plain/tab_num/tab_reg -- returns NULL for a plain frame (so the
│                              ordinary path is one inherits(), byte-identical), else $variables +
│                              `.svy_weights` (weights(type="sampling") -- the bare weights() is the
│                              n x R REPLICATE MATRIX for a svyrep.design) + `.svy_row` (position in the
│                              ORIGINAL design). svrepdesign/twophase are REFUSED, never approximated.
│                              svy_check_test() (test is TRUE/FALSE only) + svy_inference_basis() = the
│                              DERIVED fact, resolved in tab_setup() where the weight AND design_spec are
│                              both known (before, only tab() had the rule -> tab_many() was always classic).
│                              svy_domain_design(design, rows, frame) = the ONE domain helper shared with
│                              tab_reg (restrict by INTEGER rows + swap in the prepared/recoded frame --
│                              both halves needed, svychisq/svyglm read variables OFF the design;
│                              WARNING `[` does not drop rows on a CALIBRATED design, it sets prob=Inf,
│                              so the frame is padded back to full length). svy_omnibus_grid() (the
│                              PRODUCER, run in tab_transform) + svy_omnibus_one() work on the PREPARED
│                              microdata (so the p describes the table SHOWN -- lumping/filter/relabel
│                              included); tab_robust_overlay() (the thin JOINER, in tab_assemble_tables,
│                              which is where the numeric ANOVA rows are bound) REPLACES the chi2/F rows'
│                              statistic/df/p/n and carries effect_size/min_e through.
│                              The ONE architectural exception to "test from the aggregate" (opt-in, per-table).
│                              z14-ii: it governs the cell INTERVALS too, so the leaves stopped re-reading
│                              the option. z16-i: **svy_inference_basis(design_spec, wt, force=, can_serve=,
│                              design_effect=)** = THE BASIS -- "n"/"weights"/"design"(/"design_partial"),
│                              the ONLY option-or-design read (`design_effect` is the per-call argument,
│                              NULL = the option). `wt` says how the
│                              ESTIMATE is computed; the basis says how the INTERVAL is -- two orthogonal
│                              facts, which is why one kept needing four encodings. `force` is tab_reg's
│                              ruling-1 rule (its crude Obs_* are ALWAYS weighted-basis, so they match the
│                              Model_* beside them; the option is tab()-scoped). + svy_degf() (the design's
│                              #PSU-#strata, captured at the boundary -> every interval's critical value),
│                              svy_abort_wt_design() (W10: wt beside a design ABORTS, all 5 entry points),
│                              svy_weighted() (the ONE "is anything weighted" predicate, was 3 spellings).
│                              z16-iiiii: **new_inference(wt, design_spec, conf_level, method, agg_only,
│                              force, design_effect)** = THE build-time object `ctx$inference`
│                              (wt/design/basis/degf/conf_level/method/agg_only), resolved once in
│                              tab_setup() and carried whole by plain_core/num_core/tab_apply_tests --
│                              it replaced ~10 flat formals that had to be threaded through five layers
│                              by hand. What SURVIVES the build is the per-column conf_level/degf/basis
│                              attributes tab_stamp_inference() projects from it.
│                              z16-iii: svy_omnibus_one() is ONE estimator, two ways in -- the "weights"
│                              basis SYNTHESISES the flat svydesign and runs the SAME survey::svychisq /
│                              svyglm+regTermTest the "design" basis runs, so the discriminators are two
│                              (chi2/chi2_design, F_welch/F_classic/F_design) and not four. ~35 lines of
│                              hand-rolled first-order Rao-Scott + weighted ANOVA DELETED, not replaced:
│                              "survey owns the variance algebra" is this subsystem's standing rule.
│                              z16-iv: the overlay SPLIT into producer + joiner because TWO consumers
│                              need the grid at two times -- the `color = "contrib"` residual's base
│                              (tab_transform, via svy_deff_lookup + tab_chi2(.deff=)) and the test
│                              overlay (assemble). Producing it once is what makes a table's p and its
│                              cell colours describe ONE design effect. The producer also carries the
│                              TOTAL-TABLE group (the overlay used to drop the Ensemble test row) and is
│                              skipped for an input that cannot SERVE the basis (pre-aggregated counts:
│                              their footer says "n", so their test must not say "design").
│                              the weakest-first basis order is declared inline in basis_rank()
│                              (fmt_class.R) -- there is no `inference_basis_order` symbol
├── survey-variance.R (~405 L) z14-ii Route A: the DESIGN variance of a table's cells -> the existing
│                              `n_eff` field (n_eff = p(1-p)/Var_design, or s2/Var_design for a mean =
│                              Korn-Graubard's device), so tab_ci + the color="OR" interval + contrib all
│                              become design-based through the ONE field they already read. No new fmt
│                              field, no column attr, no colour-engine change. ONE influence function
│                              `z = (u - p*v)/B`, four (u,v) DOMAIN PAIRS (svy_uv_v: row/col/all/all_tabs
│                              + the mean), NOT four formulas; row domains come from the wide table's own
│                              keys with "Total" = every level, so total rows need no special case.
│                              svy_group_map = the distinct-key-tuple codes (small R x L matrices; only
│                              the influence matrix is n-long, one svyrecvar per column level).
│                              svy_var_prep does NOT reuse svy_domain_design (svyrecvar never reads
│                              $variables) but keeps its calibrated/PPS warning: scatter index + w=1/prob.
│                              z16-iiiii: the producers return **svy_var_out() = list(v=, reason=)** --
│                              "no answer" plus WHY, never a bare NULL the caller must interpret, and
│                              svy_var_setup() extracts the 6-guard prologue they share. That return type
│                              is what let the process-global degrade env GO: the reason travels with the
│                              answer to the leaf, which keeps its own `degraded` / `unserved` LOCALS and
│                              passes them to leaf_inference() -> basis "design_partial" / "n" on ITS OWN
│                              columns (W4/W9). svy_degrade_env + its 5 helpers + svy_var_bail DELETED
│                              (6 fns, 12 sites), and with them the stale-flag hazard class that had
│                              needed a reset in four entry points. svy_var_degraded() is now just the
│                              message, naming the reason where it is actionable.
│                              **z16-ii: the FLAT CLOSED FORM** -- a weight column IS a survey design, and
│                              at ids=~1 svyrecvar collapses (Sum(w*z) = 0 exactly, so the centering is a
│                              no-op) to per-cell sums the aggregate already has:
│                              Var(p) = n/(n-1)*[A(1-p)^2+(S-A)p^2]/B^2, A = the CELL's Sigma w^2, S/B =
│                              the base domain's. svy_flat_neff_prop/_mean/_rows + svy_design_is_flat()
│                              (a flat svydesign routes here too: same answer, no influence matrix, no
│                              400 MB ceiling). So the weighted basis needs NO microdata: O(cells), and
│                              Kish is this formula with each cell's own Sigma w^2 discarded -- it
│                              survives ONLY as the degenerate-cell limit B^2/S. Exact vs survey
│                              (test-flat-design-parity.R, 50 assertions, ratio 1.0000000000)
│                              z14-iii: svy_row_at() = THE row-space rule extracted out of svy_var_prep
│                              (shrank -> i, did not -> des_rows[i]), also read by reg_if_align();
│                              svy_var_mean(wmult=) = a per-row weight multiplier, which is what lets
│                              tab_reg()'s crude grid share this producer (a grouped-binomial row is a
│                              cluster of `trials` draws -> the general ratio form, not a 2nd formula)
├── reg-empirical.R (~1190 L) Phase 20e: THE OBSERVED (crude) COMPANION of a model effect, and the SE
│                              of the gap between the two -- carved WHOLE out of tab_reg.R (5630 ->
│                              4734), the 19l precedent applied to the package's biggest file. Its
│                              STAGE is reg_stage_empirical() in tab_reg.R (the tab-leaf.R / tab.R
│                              relationship): **REG_EMPIRICAL** = per family, the SHAPE of each crude
│                              column (name / stored scale / display / digits / ref / pct base / CI
│                              method / colour measure / link) + which of them is the effect twin of
│                              the model's coefficient -- a family is ONE row, foreign-key checked in
│                              zzz-fact-keys.R. TWO sources, one shape: `from = "grid"` = a CLOSED
│                              FORM off reg_empirical()'s per-(var, level, category) grid (the
│                              univariable model being saturated for a factor predictor, the crude OR
│                              IS the Woolf 2x2 ratio); `from = "fit"` = a univariable reg_fit()
│                              through the very fitter the table came from (ordinal, every numeric
│                              predictor, any marginal shape), so ruling Q6 -- same estimand, link,
│                              CI rule, multiplier -- holds by construction. Also reg_crude_y/_yw,
│                              reg_level_counts (add_n's column), reg_empirical_fit, reg_fit_overlay,
│                              reg_empirical_columns, reg_same_estimand/_frame (the two predicates
│                              that WITHHOLD `obs` rather than lie) and reg_gap_se_columns.
│                              ⚠ it sorts BEFORE tab_reg.R, so its only top-level code (the
│                              REG_EMPIRICAL literal) may not read anything defined there
├── reg-spec-build.R (~435 L) Phase 20f-iii: WHAT ONE MODEL CONTRIBUTES TO A `tab_reg()` TABLE, as
│                              ONE declared product -- "20e one grain finer". Six stages carried
│                              their own `map(specs, ...)`, so "which parts of the table are
│                              per-model and which are between-models" needed four files to answer.
│                              **`reg_spec_build(i, ctx, emp_shared)`** does, in today's relative
│                              order for one spec: fit -> columns -> gof/global/check rows -> the
│                              `add_n` count -> (a per-outcome crude block) -> `reg_set_obs()` -> the
│                              multinomial tooltip; **`new_reg_spec_product()`** is the 4th record
│                              constructor (formals ARE the contract; ⚠ no dot-prefixed key).
│                              **THE PAYLOAD RULE**: no fit and nothing referencing one, so a unit
│                              can cross a process boundary -- ONE declared exception since 20f-iiii,
│                              `fit` (only consumer `reg_compare_rows()`, on a path that is serial
│                              anyway). A crude block leaves as its COLUMNS and nothing else
│                              (`reg_emp_slim()`); its 60-100 MB `$frame`/`$fits` never travel.
│                              TWO placeholders, because a
│                              worker cannot know post-`make.unique()` facts: the footer rows' `col`
│                              (rewritten wholesale per product -- every row of one model shares one)
│                              and the tooltips' (col_idx, skeleton row) pair. ⚠ the MULTINOMIAL tip
│                              is the SPEC's (it keys that model's category columns); the NUMERIC one
│                              is the BLOCK's, built once with it -- letting each spec build it
│                              re-emits identical rows for one column (20f-ii's deleted duplication).
│                              **`reg_specs_independent(ctx)`** = NULL or THE REASON, and since
│                              20f-iiii there are TWO, each a fact about the statistics AND measured
│                              (perf study §8): a comparison is a test BETWEEN fits (returning them
│                              instead = 162 MB per fit at n = 200 000) · an all-coefficient compound
│                              formula takes its skeleton from the first fit (`skeleton_deferred` --
│                              ⚠ UNREACHABLE from tab_reg(), a compound formula forces exactly one
│                              spec, kept as the invariant for a direct reg_build() caller). The
│                              third ("compared models share one crude block") is GONE: the block is
│                              the OUTCOME's, built by reg_stage_crude(). + the other two
│                              parallel axes' workers, `reg_build_group()` (G) and
│                              `reg_build_outcome()` (R). ⚠ sorts BEFORE tab_reg.R: no top-level code
│                              reading a tab_reg.R object
├── reg-resolve.R    (~980 L) Phase 19m-ii: THE argument boundary of `tab_reg()` -- 19i's
│                              `tab_resolve_common_args()` medicine for the producer that never got
│                              one. **`reg_resolve_args()`** is the ONE entry point (tab_reg() calls
│                              it once and gets **`new_reg_args()`**, new_reg_shared()'s idiom), and
│                              it is six declared stages: **S1 `reg_validate_args()`** (the checks
│                              that are PURE -- and four of them are new: `conf_level` had NEVER been
│                              validated here, `stats` was SILENTLY FILTERED so a typo lost a footer
│                              row, `color_signif` was unvalidated so an unknown policy was STORED on
│                              every column, `baseline`'s shape) · **S2 `reg_prepare_data()`** (the
│                              design unwrap / formula / predictors dispatch / labelled / the level
│                              MERGE (20g-ii: block G0, tab()'s own tab_collapse_levels(), and H25 --
│                              it cannot interact with `shape`, which refuses a factor predictor, so
│                              one acts on the numeric predictors and the other on the factor ones;
│                              like `shape` it must also recode `design_obj$variables`) / `shape` /
│                              predictor union / the five `split_var` refusals -- i.e. EVERY rewrite
│                              of `data`) · **S3 `reg_resolve_estimands()`** (the per-dependent
│                              TABLE: dep/family/rr_promoted/est/fit_family/trials/inverse/crude_key,
│                              + `reg_resolve_trials()`) · **S4 `reg_resolve_output()`**
│                              (display/colour/`empirical`, THE NOTES LAST) · **S5
│                              `reg_resolve_fit_plan()`** (na / the `reref` gate / the reference
│                              relevel / multiplier / shape terms) · **S6 `reg_resolve_specs()`**
│                              (labels, positive levels, the ONE `new_reg_spec()` call site).
│                              ⚠ `data` is INSIDE the boundary and a declared field of the record
│                              (new_ctx()'s precedent), because a pure resolver is impossible without
│                              a cycle: `family = "auto"`, `trials = TRUE` and `multiplier = "sd"` are
│                              all ANSWERED by the data, `shape` recodes it, `reference` relevels it,
│                              and the relevel needs the families S3 resolves. Lifting them out would
│                              put the ORDERING in the caller.
│                              ⚠ there is deliberately NO `REG_ARG_VALUES` table: TAB_ARG_VALUES
│                              collapsed FIVE drifted producers, `tab_reg()` is ONE whose vocabularies
│                              are already declared once each, and TAB_ARG_VALUES' own exclusion rule
│                              ("validating it means REWRITING it") disqualifies 11 of 15 candidates.
│                              THE ORDER IS THE DESIGN, and the 24 constraints are written out as
│                              `H1`..`H24` where they bind (20f-ii's H24 = a between-model test
│                              needs the models to share an OUTCOME -- `compare` was gated nowhere,
│                              so several outcomes + a comparison key reached reg_compare_rows()
│                              with two responses, `anova.glmlist` silently dropped one and the row
│                              was labelled with the wrong outcome) -- three of which were VIOLATED: the
│                              `empirical` forcing/degrade straddled the notes and the specs (so a
│                              stored effect word could contradict its own column), the
│                              `color_signif` default landed 22 lines after the note that reads it,
│                              and the frozen frame was built TWICE under a comment demanding it be
│                              one. ⚠ the `reref` clause is the one place a wrong TRUE is a wrong
│                              NUMBER (a stale digest), and it reads 13 resolved values across eight
│                              blocks -- its reasoning is spelled out per clause.
│                              Characterisation harness: `dev/verify_reg_specs.R` (291 cases,
│                              save/check, dumping the MESSAGES in order as well as the specs /
│                              reg_call / column attributes / labels / test keys).
├── reg-estimand.R   (~790 L) Phase 19e (KEY 8b): WHAT A REGRESSION COLUMN ESTIMATES. The user's two
│                              questions -- `effect` (which CONTRAST: coefficient / marginal /
│                              at_reference) x `measure` (which MEASURE: odds_ratio / ratio /
│                              difference / log) -- resolved through ONE declared library,
│                              **REG_ESTIMANDS**: a row per (family, effect, measure) carrying
│                              `builder` (which of reg_build's three column builders runs -- the
│                              table-scalar `if` is GONE), `fit` (the internal family key: "rr" =
│                              modified Poisson, "rd" = identity link, "mr" = log-link pseudo-ML,
│                              each a LINK chosen to reach a measure and never a distribution the
│                              user should name), `exp`, `word` (the header), `scale` (the
│                              EST_SCALES key stamped on the column), `display`, `crude_fam` /
│                              `crude_shape` (which REG_EMPIRICAL row pairs with it),
│                              `comparison` (the marginal contrast), 20d's `engine` (WHICH engine
│                              computes this row's marginal quantities: "gcomp" = tabxplor's own
│                              analytic g-computation | "marginaleffects" | "auto" = the rule, stated
│                              ONCE in reg_marginal_engine(): everything but `at_reference`, whose
│                              one-row profile grid g-computation does not build. It is a PERMISSION,
│                              not a promise -- the producer returns NULL and reg_marginal() falls back
│                              for the WHOLE call, never a per-contrast mix), `status` + the `why` /
│                              `note` closures (gettext at render, statically extractable). It
│                              replaced a FOUR-argument product (family x effect x at x
│                              exponentiate = 36 cells for 9 estimands, 3 degrade blocks, 2 aborts,
│                              ~19 silently-ignored cells) and DELETED reg_effect_word() (a
│                              4-argument nested switch = the `word` column), reg_model_note()
│                              (6 arms x do_exp = the `note` closures), reg_crude_shape()'s dispatch
│                              incl. its cross-family borrow (= 2 columns), and do_exp_for /
│                              effect_shape_for / eff_word_for (views of one row).
│                              THE VOCABULARY IS tab()'s: `measure`'s values ARE EST_SCALES$geometry
│                              (19b), so the argument that asks, the attribute that stores, the
│                              legend that names and the plot axis that draws are one vocabulary --
│                              *the argument names the geometry, the attribute names the row*.
│                              THREE STATES: a row (`ok`) builds; `impossible` aborts with its own
│                              reason; NO ROW = "not offered", the message ENUMERATING what the
│                              outcome does offer, generated from the table (+ a fourth at run time:
│                              a link that did not converge). Read ONLY through reg_measure_key /
│                              reg_estimand / reg_estimands_for / reg_estimand_abort /
│                              reg_default_measure / reg_estimand_note, with a build-time stopifnot.
│                              Also: reg_normalize_color (D25 -- a reg colour cannot contradict the
│                              column: the ladder comes from its stored `scale`, so only the
│                              own-ref measures remain, a DERIVED allow-list; `TRUE` in the text
│                              slot = "the column's own geometry", so c(TRUE, "adjustment") replaces
│                              c("OR", "adjustment")), reg_retired_args / reg_effect_key (the
│                              retired spellings abort with their mapping -- 19b's fmt(type=) idiom),
│                              reg_per_dep (THE per-dependent slicer shared by family / effect /
│                              measure and the multi-dependent recursion, D6),
│                              20c: REG_FAMILIES gains **`outcome_level`** = WHAT
│                              `outcome_level = c(<outcome> = "<level>")` MEANS FOR THIS FAMILY:
│                              "modelled" (binomial -- with two levels, singling one out IS choosing
│                              what is estimated, and it becomes the column header), "baseline"
│                              (multinomial -- with k > 2 you can only choose the PIVOT, the opposite
│                              role), NA + a `why` closure = the refusal (ordinal must keep its
│                              order). The one non-uniformity is forced by arithmetic, so it is
│                              DECLARED once and read by the resolver, the abort and the `@param`
│                              alike. Readers: reg_outcome_level_role / _abort /
│                              reg_resolve_outcome_level / reg_outcome_levels (⚠ which accepts BOTH
│                              spellings of a 0/1 numeric outcome's synthesised labels, and the raw
│                              "0"/"1" -- that path is where the retired logical was a SILENT NO-OP)
│                              / **reg_outcome_level_of()** = the ONE NA <-> NULL boundary, because a
│                              tibble column and a typed record field cannot hold NULL.
│                              19m-i's **REG_FAMILIES** = WHAT EACH FAMILY IS CALLED and where it
│                              may be named (one row per family: `display` closure = the footer
│                              sentence, `short` = the Excel filename tag, `ui` / `ui_binary` = the
│                              jamovi picker labels where **`NA` IS the fact "not offered in the
│                              picker"**, `outcome` = the OUTCOME family of an internal link key).
│                              FOUR name tables and a switch before, in two files, already
│                              disagreeing -- and the "not offered" fact was ALSO a hardcoded
│                              setdiff() in dev/generate_jamovi_js.R. Readers:
│                              reg_family_display_name / reg_family_short / reg_family_ui_labels,
│                              each keeping its own default ("regression" / "reg").
│                              DERIVED from it: REG_FIT_FAMILY (the `outcome` column) and
│                              **REG_FAMILY_MULT_WORD** / reg_family_mult_word() = the
│                              multiplicative effect word of a FIT key (OR / RR / IRR / RoM), read
│                              off REG_ESTIMANDS' own exponentiated coefficient row with a
│                              build-time singleton assert -- it replaced the last hand-written
│                              `switch(fam, ...)` in legend_reg_eff_word(), whose default answered
│                              "OR" for every family it did not list. ⚠ keyed on `fit`, NOT on the
│                              family bucket (a binomial outcome holds both the logit row, word OR,
│                              and the modified-Poisson one, word RR), and the fit's word wins only
│                              where the LINK makes one other than an odds ratio -- a logistic fit
│                              asked for a MARGINAL ratio keeps its crude RR.
│                              REG_FIT_FAMILY, and
│                              the exported **reg_measures(data, dependent)** lister +
│                              reg_measures_rd() (the roxygen `@eval` that GENERATES ?tab_reg's
│                              estimand section) -- four consumers, one table.
├── table-spec.R     (~125 L) Phase 19g (KEY 6): THE TABLE IDENTITY -- ONE `meta$spec` with three
│                              slots for BOTH producers. `kind` ("crosstab"/"regression"), stated by
│                              the producer, read through tab_kind()/tab_is_reg() (is_reg_footer(),
│                              which sniffed the `test` tibble for a reg discriminator, is DELETED;
│                              that sniff survives only as tab_kind()'s degraded fallback). `vars` =
│                              what NO column can carry (wt / caption / var_labels) -- the rest of
│                              the variable model is derived (rows from the declared index columns,
│                              cols from the fmt columns' own `col_var`), so it is uniform across
│                              producers by construction. `call` = the producer's recipe: a
│                              regression's model record, read through reg_call() (`meta$reg_meta`
│                              is gone), incl. z15's `fit_spec`; a crosstab records none yet.
│                              new_spec/get_spec/set_spec_field (which never invents a `kind` --
│                              materialising the degraded guess would break absent-when-unset) +
│                              spec_bind (the declared meta_bind_rules entry, slot by slot).
├── row-model.R      (~370 L) 20h: **lvl_reserved_labels(what = "merge"|"data")** = the labels a
│                              level may not take, for the TWO questions that ask, and
│                              **lvl_check_reserved()** = the refusal. A SOURCE level named "Total"
│                              collides with the leaf's internal pre-rename sentinel and was read back
│                              as a total ROW (measured: row_kind "total", is_totrow TRUE, bold, out
│                              of the pct base, and the table printed TWO "Total" rows) -- so it
│                              ABORTS, at tab_prepare()'s tail (post-recode, on the levels that reach
│                              the leaf) AND in leaf_defuse_vars() (a direct leaf call never reaches
│                              tab_prepare). ⚠ NOT "NA"/"Others": measured, an "NA" level renders
│                              correctly unless the column also holds real NAs, so refusing it would
│                              be a false positive on an ordinary survey label.
│                              20g-ii: it also owns the declared LEVEL OPERATIONS -- **new_lvl_collapse()**
│                              = the validated "merge these levels into one" spec, in
│                              forcats::fct_collapse()'s own shape (var -> merged label -> the levels
│                              it swallows). The SPEC is here, the APPLIER is tab_collapse_levels()
│                              at the prepare stage: `tabxplor_lvl` exists only on a BUILT table's
│                              index columns, while a collapse must change COUNTS. Two refusals, each
│                              because the alternative is silently wrong -- one level in two groups
│                              (fct_collapse gives it to the LAST, no message) and a merged label
│                              colliding with one tab() mints itself (Total/Ensemble/Others/NA, read
│                              from the OPTION so it is true in every locale). An empty label
│                              defaults to the joined levels HERE, once, which is why the jamovi text
│                              box can show that string as a mere placeholder.
│                              Phase 19f (KEY 1): THE ROW MODEL -- what a row IS, given the same treatment
│                              a column already had. TWO facts, TWO carriers. (1) **ROW_KINDS** +
│                              the `row_kind` FIELD (data/total/n/pct/pvalue/gof/blank) replacing
│                              the logical `in_totrow`: it cannot leave the record, because
│                              fmt_color_plan() asks is_totrow() of a LONE column. (2) **tabxplor_lvl**,
│                              a factor SUBCLASS on the index columns carrying `role`
│                              (level/var/tab_var), `var` and `ordered` (a named logical, ONE ENTRY
│                              PER VARIABLE -- how a merged `levels` column keeps which of its
│                              stacked variables were ordinal once the factor itself must go plain).
│                              It IS a factor, so is.factor/levels/as.character/arrange/filter/
│                              group_by/print need NO method; only vec_c+bind_rows (ptype2/cast),
│                              droplevels() and `[` do. ONE stamping idiom, tab_stamp_index(), called
│                              by both leaves + tab_counts + tab_compact + tab_reg + the transpose;
│                              ONE read, tab_declared_vars() -> row_var / tab_vars / var_col /
│                              row_vars / compacted, which tab_render_vars()/tab_get_vars() call
│                              first (the last-factor heuristic survives as the DEGRADED path only).
├── tab-shape.R      (~220 L) Phase 19h (KEY 7): WHAT SHAPE IS THIS TABLE, AND WHICH OPERATIONS TAKE
│                              IT. `tab_shape(x)` (exported) reads container / kind / merged / grouped
│                              / the three variable axes off the DECLARED model (19f's index columns +
│                              19g's `meta$spec$kind`), never a column name; for a list it adds
│                              `same_col_vars` (nested-in-the-widest, tab_compact()'s own rule) and
│                              `same_tab_vars`. `TAB_OPS` = one row per operation (compact /
│                              transpose_object / transpose_render) declaring the facts it needs, its
│                              `severity` ("abort", or "bail" = tab_compact()'s message-and-return
│                              contract) and a `why` closure (gettext at render, potools-extractable).
│                              Read ONLY through tab_supports() (exported) + tab_check_shape() -- the
│                              five scattered aborts are one call each now. rd_shape(rd) builds the
│                              same record from a finished RENDER model (the transpose has no table).
│                              Refusals that are NOT shape facts (duplicated row keys, >1 total
│                              row/column) stay local to tab_transpose(), with a comment saying so.
│                              20a: **tab_columns(x)** (exported) = the COLUMN-axis mirror -- one row
│                              per fmt column x its 19 stored facts, off fmt_attrs_of(). The only
│                              place conf_level / degf / basis / ci_method are readable side by side,
│                              which is what z13/z16 stored them per column FOR. Reports the STORED
│                              values, so `conf_level = NA` honestly means "no interval was stamped".
│                              20i: `TAB_OPS$transpose_object` gained a `kind` check (FIRST, so a
│                              regression gets its own reason -- use tab_export(transpose = TRUE) --
│                              not the crosstab-shaped "one row variable" message it fails as `merged`).
├── zzz-fact-keys.R  (~250 L) Phase 20a (KEY 2): REFERENTIAL INTEGRITY between the declared fact
│                              tables. **TAB_FOREIGN_KEYS** = 51 declared edges (from / get / to /
│                              allow / orphan), read only through tx_check_foreign_keys(), which runs
│                              at LOAD -- a key written by hand in one table and read by name in
│                              another is a FOREIGN KEY, and a dangling one breaks the build at the
│                              moment it is made (19d renamed the colour measures and did not reach
│                              EST_SCALES$label_meas; the fix shipped with a WARNING comment, which
│                              is hard rule 4 one level up). ⚠ IT MUST SORT LAST: COLOR_SCALES lives
│                              in tab_classes.R and REG_EMPIRICAL in tab_reg.R, so reg-estimand.R --
│                              19o's proposed home -- sees neither; `zzz-` is last by construction.
│                              ⚠ its two readers (tx_fk_scalar / tx_fk_all) use `[[`, never `$`:
│                              MEASURES$adjustment has `scale_from` and no `scale`, so `$scale`
│                              partial-matches to "gap". `allow` entries are STATED FACTS, never a
│                              way to silence a real dangling key (DISPLAY_TOKENS' `ci` names a
│                              DERIVED quantity; woolf/katz/wald_log are the only interval of their
│                              geometry, so none is a `ci_method` a user picks). Holds every
│                              cross-TABLE edge; a table's SELF-consistency stays beside it (the
│                              seven intra-table stopifnot blocks are listed in the header).
│                              20b: +6 edges into the ARGUMENT surface (TAB_ARGS' `values_from` /
│                              `values_rd` / `option` / `doc_with` / `pct$stored`, and TAB_OPTIONS'
│                              `arg`) + **tx_check_tab_args()** = the anti-drift check the generated
│                              `@param` blocks rest on: every covered producer's FORMALS and its
│                              declared TAB_ARGS rows are the same set, and every surviving formal's
│                              default IS the declared one. ⚠ it must live HERE and not beside
│                              TAB_ARGS -- `formals(tab)` does not exist while R/tab-args.R is being
│                              sourced. ⚠ rlang::is_missing(), never is.symbol(): merely touching a
│                              no-default formal raises "argument is missing".
│                              20d: +2 edges on the two vocabularies reg_build() dispatches on --
│                              REG_ESTIMANDS' `builder` -> REG_BUILDERS and `engine` ->
│                              REG_MARGINAL_ENGINES (allow "auto"). The other direction of `builder`
│                              is reg_build()'s own `switch()`, whose arms are all NAMED and whose
│                              default now ABORTS -- it used to fall through to the coefficient
│                              builder, so a typo built the wrong column in silence.
│                              20e: +**tx_check_reg_ctx()** = new_reg_ctx() and new_reg_shared()
│                              declare TWO record types that every reg_stage_*() binds into ONE scope
│                              (reg_ctx_locals() = the ctx plus its `shared` element), so a name in
│                              both would SILENTLY SHADOW -- `c()` keeps both and list2env() lets the
│                              LAST win. ⚠ same reason as tx_check_tab_args(): neither constructor
│                              exists while the other's file is being sourced.
├── tab-counts.R     (~430 L) tab_counts() from-the-middle constructor (Phase 4): reshape any
│                              input shape → count-aggregate → tab_plain(.fine) + shared finalize.
│                              19i: its ~15 copy-pasted boundary lines are ONE
│                              tab_resolve_common_args() call, which also gave it two rules it never
│                              had (D28 on its OWN color_spec -- it stored a gate it never applied --
│                              and `stars` resolved from the option). What stays local is what is
│                              true of THIS producer: the design refusal, the microdata-only `na`
│                              refusal (it says WHY) and counts_refuse_mean_methods() (the ci_method
│                              mean slots are inert here -- accepted-and-ignored before)
├── tab-resolve.R    (~680 L) THE argument boundary + the arg-overwrite cascade.
│                              19i: **tab_resolve_common_args()** = what every crosstab producer
│                              must do to its arguments, run once, by tab() / tab_plain() / tab_num()
│                              / tab_counts() (5 hand-written copies that had drifted -> 1): the
│                              chi2->test rename, TAB_ARG_VALUES (DERIVED from TAB_ARGS since 20b)
│                              + tab_validate_args() (the
│                              vocabulary AS DATA: `values` / `leaf` / `size` / `na_ok` per argument,
│                              so `totaltab`/`n_min`/`conf_level`, validated NOWHERE before, abort
│                              naming the valid set), resolve_cleannames/_stars/_ci_method, the `OR`
│                              route, normalize_color_spec + ci_disable_signif ON THE SPEC, tot ->
│                              (totrow, totcol), total_names. ⚠ `ci` is deliberately NOT in
│                              TAB_ARG_VALUES: its vocabulary carries a soft-deprecation, so
│                              validating it means REWRITING it -- resolve_ci_value()'s job.
                              19k: TAB_ARG_VALUES gains `anova` (welch/classic), and
                              tab_cache_keys() finally receives a REAL `filter_expr` (D13: it was a
                              hardcoded NA_character_, so a filter change never invalidated a key).
│                              TAB_CI_STEP_VALUES = tab_ci()'s own STEP vocabulary, declared beside
│                              it, in which "diff" is native (the pipeline calls the step that way)
│                              and carries no deprecation. Then tab_resolve_settings() (Phase 7b): the ONE pure arg-overwrite
│                              cascade shared by tab_build+tab_counts -- color="auto" -> a MEASURE
│                              (via MEASURES' declared auto_for contexts), then that measure's
│                              declared `requires` applied to chi2/totrow/ci/ref;
│                              resolve_color_auto_num() (numeric arm); emits ci_scale (14b: "ratio"
│                              = the Katz interval). The jmvtab .js / cache boundary.
│                              19c: it returns ONE resolved measure. The 4-way split
│                              color_diff_OR/color_ctr/color_ci/color_num is GONE -- a fossil of the
│                              pre-2.0.0 four-step pipeline (4 hand recodes over measure literals,
│                              routing WHICH step stamped the colour), one of which (color_ci)
│                              existed only to receive a legacy composite the cascade MANUFACTURED
│                              one step after 17d decoded such strings away at the boundary. Each
│                              consumer asks the measure instead: measure_builds() (does the
│                              contribution pass stamp it), measure_applies() (can it colour a
│                              mean), measure_forces().
│                              19d: THE comparison chain (`color` -> `display` -> the difference) via
│                              display_comparison()/tab_leaf_comparison(); resolve_ci_value() (the
│                              c("auto","no","cell","ref") anchor + its soft-deprecations);
│                              resolve_leaf_ci() = the SAME rules for a leaf called directly AND for
│                              the jamovi boundary (jmvtab_build's 2 hand-mirrored ci rules are gone);
│                              display_comparison() (19m-iii: the MAPPING it reads is DISPLAY_TOKENS'
│                              `comparison` column now -- DISPLAY_COMPARISON was the only one of the
│                              eight display vocabularies living in a third file);
│                              measure_geometry() = which of the 3 geometries owns the stored interval
│                              ("or"/"ratio"/"diff"), shared with the jmvtab tier-3 cache TUPLE so the
│                              cache and the pipeline cannot disagree (a diff<->ratio toggle changes
│                              the interval, so it can never be a re-paint); ci_disable_signif() =
│                              D28's ONE rule -- the CI_NO_INTERVAL_TO_TEST values ("cell" AND, since
│                              the 19d tail, "no") inform and disable stars/color_signif, because `ci`
│                              is the ANCHOR question and those two read what it anchors. ONLY "auto"
│                              resolves in either resolver now: tab_resolve_settings() used to
│                              silently upgrade an explicit "no" to "ref" while resolve_leaf_ci() did
│                              not, so tab() built an interval tab_num() did not and the jamovi tuple
│                              recorded a `ci` its carrier contradicted. It is
│                              called by both resolvers AND by tab()'s argument boundary -- the last
│                              because the STORED policy attribute is written from the colour spec,
│                              not from what the resolver decided.
├── tab-export.R      (~100 L) Phase 10j-A: the tab_export(format=) facade over the four exporters +
│                              forest_plot. 19h: its `theme` / `color_legend` blocks stopped naming
│                              `format = "kable"`, a value `match.arg()` rejects.
├── tab-leaf.R      (~2700 L) 20h: **tab_plain() runs finalize_color_tail()** -- it was the ONE
│                              crosstab producer of four that skipped the shared colour tail, so the
│                              `color_spec` its own argument boundary had resolved was computed and
│                              DROPPED, and three documented behaviours silently did not happen:
│                              `color_signif` stored "ignore" whatever was asked (measured against
│                              tab()/tab_num(), which were right) · a legacy composite kept its
│                              MEASURE and lost its POLICY (`color = "diff_ci"` coloured and tested
│                              nothing) · a two-channel `color = c(text, bg)` ABORTED inside
│                              plain_resolve() on a length-2 `if`. Byte-identical on all 7
│                              pre-existing shapes; the df/num escape hatch keeps num_core's early
│                              return (the tail would stamp color_breaks on a plain frame).
│                              + **plain_core()'s 20 PHASES are declared** (one numbered head per
│                              sequential block, naming WHAT IT PRODUCES -- 652 source lines ran
│                              with 2 of them marked). Comments only, PROVED: 2 of 390 deparsed
│                              lines differ and both are 20h's own dead-formal drops. ⚠ the
│                              EXTRACTION into leaf_reshape()/_compare()/_infer()/_totals() is NOT
│                              done and the header says why: the ~14 optional `tabs_*` tables cross
│                              most boundaries, so each helper would take and return most of them.
│                              The orphan `Region B`/`C`/`E` lettering is GONE (there was never an
│                              A or a D anywhere in R/).
│                              Phase 19l: THE AGGREGATE CORE, carved out of tab.R (whole functions,
│                              no behaviour change). tab_plain()/tab_num() (the public superseded
│                              leaves) + plain_resolve/plain_core + num_resolve/num_core + every
│                              leaf_* + tab_apply_reference + leaf_ci_plain + calculate_refrows +
│                              build_total_rows/finalize_total_rows. **leaf_defuse_vars()** = the
│                              shared NSE preamble (enquo -> quo_miss_na_null_empty_no -> ensym /
│                              eval_select + the svy_abort_wt_design tail), written THREE times
│                              before (plain_core / num_core / tab_aggregate_num), differing only in
│                              `plural` -- one col_var sym vs a tidyselect of several + pos_col_vars.
│                              The quosures are captured BY THE CALLER, so it is an ordinary function.
├── tab-chi2.R       (~465 L) Phase 19l: chi2_compute_test() (READ-ONLY: builds the tidy `test`
│                              tibble) + chi2_write_contrib() (the ONE mutate(across()) that writes
│                              ctr/var/pvalue) + the plain-vector contribution helpers. TWO callers,
│                              ONE implementation: the leaf (leaf_chi2/leaf_chi2_num) and the
│                              superseded tab_chi2() step -- so a step and a build cannot differ.
├── tab-display.R    (~810 L) Phase 19l: THE DISPLAY GRAMMAR -- tab_apply_display(),
│                              display_write_col() (THE per-column template writer, shared by
│                              build-time tab(display=) and post-hoc set_display()), D22's per-cell
│                              void + D23's geometry refusal, and the add_n/add_pct materialisation
│                              (tab_add_n_pct, tab_fold_addn_incell, tab_or_total_col,
│                              tab_apply_n_min).
│                              19m-iii: **DISPLAY_TOKENS** = THE per-token relation, 23 rows (22
│                              tokens + the `rr` alias) x 12 declared columns, absorbing the EIGHT
│                              vocabularies that stated one fact each in four files -- get_num()'s
│                              read map + set_num()'s write map + tabxplor_display_fields (12) +
│                              tabxplor_display_aliases (both OUT of fmt_class.R) + DISPLAY_BARE_TOKENS
│                              + DISPLAY_FIELD_SOURCE + DISPLAY_TOKEN_GEOMETRY + DISPLAY_COMPARISON
│                              (out of tab-resolve.R) + the inline value-cell gate + the footer gate
│                              (written TWICE, with two near-miss variants). Every one of those names
│                              SURVIVES, DERIVED from a column and keeping its contents AND ORDER
│                              (rows 1-12 are the user fields in the order the "Valid fields" abort
│                              prints them; rows 1-8 are additionally the bare tokens), so no consumer
│                              moved. `footer` and `colour` are TWO columns, not one "numberless":
│                              `pvalue` is a footer statistic that IS coloured (a significance
│                              warning), which is exactly the disagreement the four hand-written
│                              copies encoded three different ways.
│                              ⚠ `OR`/`OR_pct` are ROWS, not aliases of `or`/`or_pct` -- they render
│                              identically, but display_primary() returns a display VERBATIM and
│                              fmt_display_shows() compares against that raw value.
│                              ⚠ THE HOT PATH STAYS HAND-WRITTEN (the fmt_attr_rules precedent):
│                              get_num()/set_num() are vectorised mask writes, format() is ~15
│                              rendering-class masks crossed with the stored `scale`. Instead, a
│                              build-time **stopifnot() at the file's TAIL** (where fmt_class.R's two
│                              switches and this table are all in scope -- fmt_class.R sorts FIRST)
│                              walks display_switch_tokens(get_num/set_num) for their string
│                              constants and ties the three together BOTH ways. That is what caught
│                              the phase's defect (get_num 22 arms vs set_num 17 -> vec_arith wrote
│                              NOTHING on a pct_ci/mean_ci/pvalue column) and what makes it
│                              unrepresentable. `resid`/`blank` are the only honest settable=FALSE.
│                              **display_tokens_rd(user_only=)** = the `#' @eval` generator (the
│                              reg_measures_rd() precedent) behind ?tab's "Display fields" and ?fmt's
│                              "Every display token" -- ?fmt hand-listed ELEVEN of the 22 and had
│                              drifted; ?tab hand-copied a vector from a file 1400 lines away.
├── tab-deprecate.R  (~310 L) Phase 19l: tab()'s 1.3.1 -> 2.0.0 translation layer -- tab_many() and
│                              tab_deprecate_or/_many/_sup_cols/_na_drop_all, grouped so the live
│                              build path never meets them. Each shim is LOSSLESS or it aborts.
├── tab-test-display.R (~1005 L) Phase 20c (KEY 5): **TEST_ROWS** = WHAT KIND OF STATISTICAL ROW THIS
│                              IS -- one row per `test` discriminator (39), for BOTH producers,
│                              where only the reg half had a declaring table (reg_footer_spec, 31 of
│                              them) and the crosstab half lived as literals in four consumers.
│                              13 columns: `block` (WHICH PRODUCER WRITES IT -- glance/compare/
│                              global/check/interaction/omnibus; REG_GOF_KEYS derives from it, and
│                              `stat` cannot serve because a single-instrument row's `stat` IS its
│                              own name) · `producer` · `kind` (FK ROW_KINDS) · `digits` (⚠ ABSENT
│                              on a pvalue row, so reg_footer_plan()'s `%||% 0L` gives every one of
│                              them the same value) · `render` (grid = a footer ROW / line = a
│                              table-wide SENTENCE) · `noun`+`instrument` (BARE MSGIDS -- the label
│                              IS reg_check_label(noun, instrument) for all 34 reg rows, which is how
│                              21 hand-written labels and a 3-arm switch collapse into one rule) ·
│                              `stat` (WHICH `stats =` KEY REQUESTS IT: the many-to-one that keeps
│                              the user's vocabulary smaller than the storage's) · `method`
│                              (lr/f/wald/aic) · `design` · `var_kind` (FK EST_SCALES) · `anova` ·
│                              `cell_label` · `word`. Read through .trow_chr / .trow_keys (⚠ which(),
│                              never `[keep]`: most members are NA on the rows they do not apply to,
│                              and `NA == "lr"` is a phantom element) + **test_row_key(stat, method)**
│                              / **test_row_types(stat)**, which replaced the four paste0()-BUILT
│                              `compare_*` discriminators (hand-enumerated a second time in the
│                              footer spec) and the three `types = c(wald=, f=, lr=)` maps. The seven
│                              CHECK rows stay GENERATED from REG_CHECKS (test_rows_from_checks):
│                              that table owns `families` / `weighted_ok` / `panel` and the two
│                              taught-but-never-scored checks, which have a panel and NO row here.
│                              8 build-time stopifnot at the file TAIL, incl. ⭐ "exactly one crosstab
│                              row per (var_kind x anova x design)" -- the invariant
│                              test_grid_crosstab() stated only in a comment, and what lets a third
│                              ANOVA F be added as ONE row with no code change.
│                              ⚠ reg_footer_spec() MUST stay a FUNCTION (a top-level list freezes
│                              gettext at load). DERIVED with contents AND order intact, so no
│                              consumer moved: reg_footer_spec / reg_footer_test_types /
│                              REG_GOF_KEYS / reg_global_types / reg_interaction_types /
│                              reg_stat_keys (⚠ a UNION with names(REG_CHECKS) -- deriving from
│                              `stat` alone silently deletes `residuals` and `normality`) /
│                              test_display_rows' filter / test_cell_label / test_pvalue_descriptor /
│                              tab_kind()'s degraded fallback.
│                              Phase 16a: THE shared framework rendering the `test` attribute as an
│                              (19n: test_grid_crosstab() keys its value columns on the (col,
│                              col_group) BLOCK, via tab_col_blocks() -- `col` alone identified a
│                              block only while the spread level was welded into `col_var`, so two
│                              spread blocks of one variable would collapse into a single p-value
│                              column; `col_group` is a DECLARED new_test_tibble() column, since
│                              test_group_cols() reads every undeclared one as a grouping variable)
│                              aligned summary -- the console GFM block AND the inline export rows,
│                              which were four ad-hoc renderers split by (crosstab vs reg) x (console
│                              vs export). Three layers: CONTENT (test_display_rows / the formatters /
│                              reg_footer_spec) -> CONSOLE (test_summary_grid + test_render_console)
│                              -> EXPORT (tab_append_footer, the ONE fmt-frame append engine behind
│                              both inline appenders). The crosstab-vs-reg arm keys off the STORED
│                              kind (tab_is_reg).
├── tab-theme-detect.R (~200 L) Phase 14g: best-effort detection of the CONSOLE's colour scheme, for
│                              set_color_palette(theme = "auto"). NEVER errors, warns or asks --
│                              anything unknown is "light", because a wrong guess makes a table
│                              unreadable. EXPORT is not concerned (there "auto" delegates to the
│                              browser). ⚠ the Positron probe reads a settings file that also holds
│                              secrets: it extracts TWO keys by regex and never parses or logs
│                              anything else. Do not widen it.
├── tab-parallel.R   (~300 L) THE dispatch seam of BOTH producers. 20f-iiii: **THE NESTING RULE is
│                              stated and ENFORCED here, once** -- the axes nest (`row_var`s;
│                              `tab_vars` groups x models x outcomes) and only the OUTERMOST
│                              dispatches, so the everywhere() snapshot forces `tabxplor.parallel`
│                              off in every daemon (the `^tabxplor\.` regex used to SHIP the user's
│                              value in, and the three `parallel = FALSE` unit sites masked it only
│                              because an argument beats an option). + **the worker ERROR relay**:
│                              `[.stop]` re-threw mirai's own wrapper BEFORE the condition replay, so
│                              a failure discarded every message the successful units had produced --
│                              the diagnostics that explain it. The trampoline catches its unit's
│                              error and returns it on the payload, `[]` collects, the replay runs up
│                              to and INCLUDING the failing unit (serially the ones after it never
│                              ran) and `tab_unit_abort()` names it -- ⚠ de-duplicated by NAME, not
│                              by class, because the axes nest and an inner failure legitimately
│                              gains an outer name. `tab_cnd_strip()` makes a condition safe to send
│                              back (⚠ non-optional: reg_fit()'s survey call holds the DESIGN in its
│                              own `call`). `tab_pmap(.names =)` = what to call each unit; the SERIAL
│                              branch names it too, so purrr's `i In index: N` is gone and both
│                              branches say one sentence. 20f-iii added three callers and
│                              changed NOTHING here -- tab_pmap() is generic (`.l` per-unit args,
│                              `.f_name` a namespaced worker, `.const` small, `.ship` big), so
│                              `tab_reg(parallel =)` reuses the same option, worker-count rule, pool
│                              and tab_parallel_stop(). Its units are MODELS (reg_stage_specs), the
│                              `tab_vars` GROUPS (reg_build_group) and the OUTCOMES of a
│                              multi-outcome recursion (reg_build_outcome).
│                              Phase 8/9a row-axis dispatch (Suggests-only mirai): tab_pmap() + trampoline,
│                              named "tabxplor" pool (tab_pool_ensure/tab_parallel_workers/
│                              tab_parallel_stop), tab_build_one() (the per-row_var worker, serial OR mirai).
│                              20f: **the trampoline RELAYS the worker's conditions** -- it collects
│                              message/warning under withCallingHandlers and returns them with the
│                              value, tab_pmap() replays them in UNIT ORDER via rlang::cnd_signal().
│                              A daemon's console is not the user's, so before that every cli_inform /
│                              cli_warn raised inside tab_build_one() was silently LOST (measured on
│                              tab_transform()'s several-numeric-col_vars notice: 2 messages serial,
│                              0 parallel). The everywhere() options snapshot gained `cli.*` /
│                              `crayon.*` / `width` for the same reason -- cli renders its text AT
│                              SIGNAL TIME, so a daemon would otherwise format with its own glyphs
│                              and wrap width. ⚠ the replay is necessarily AFTER collection, so
│                              worker conditions land after anything the caller signalled around
│                              tab_pmap() rather than interleaved with it.
├── tab-steps-legacy.R (~1425 L) The superseded dplyr-era step API, quarantined OUT of tab.R's live
│                              pipeline: tab_pct()/tab_tot()/tab_totaltab() + pct_formula()/
│                              diff_formula() (17f), and **tab_ci()/tab_chi2() (19j)**. With 19j the
│                              WHOLE pre-2.0.0 chain is here -- nothing in the build calls a step.
│                              All exported, superseded badge, NO lifecycle warning. WHAT A WRAPPER IS:
│                              it RECONSTRUCTS a plan from fmt markers, because it runs on a table it
│                              did not build (tab_get_vars / detect_totcols / detect_refcol /
│                              detect_firstcol, the 8-branch ci case_when, the 2nd `ci = "ratio"` fold,
│                              the stars-from-the-option + degf-from-the-columns fallbacks, and the
│                              four tab_match_*/tab_add_* passes that MUTATE the table to make the
│                              step's preconditions true). That is their PURPOSE, which is why 19j did
│                              not delete them -- but the ARITHMETIC is shared with the leaves
│                              (ci_dispatch()/CI_GEOMS; chi2_compute_test()/chi2_write_contrib()), so
│                              a step and a build cannot compute two different answers.
├── tab_classes.R   (~3915 L) tabxplor_tab/grouped_tab classes, 30+ dplyr S3 methods,
│                              print methods, tab_kable(), tab_plot(), tab_compact(),
│                              OKLCH color palettes, set_color_palette()/get_color_style(),
│                              set_color_breaks() (over/under scales), color_breaks table attr;
│                              19c: **COLOR_SCALES** = the break-scale fact table (center/strict/std/
│                              settable/default/null_default [mean_diff's standardized arm]/derive/
│                              legacy/alias), from which mk_color_scale()'s validation,
│                              default_color_scales() and both breaks accessors' name maps all derive
│                              -- 7 edit sites per added scale -> 1 row; and the two DERIVED scales
│                              (log_odds, adj_diff_log) declare their parent instead of owning a
│                              switch arm in fmt_color_plan (-> color_scale_resolve);
│                              Phase 13c-iv tabxplor_tabs (multi-table LIST class: print/[/c/knit_print,
│                              auto-print + Viewer routing); 17g: tab_materialize_extras -> tab_materialize()
│                              over materialize_specs() (DECLARED list(kind,when,apply): add_n_pct/or_total/
│                              sd_twin/footer/collapse_totals; mat_add_n_pct/mat_sd_twin applies; add_n `n`
│                              COLUMN built xl-ONLY, text folds direct -- no throwaway; collapse = display slice);
│                              tx_print_html = THE options(tabxplor.print) predicate ("html" taught,
│                              "kable" synonym) -> print + knit_print.tabxplor_tab/_grouped_tab render
│                              tab_html() (bare tab() chunks knit as live html tables; tooltips option
│                              tabxplor.tab_kable_tooltips). pkgdown = ONE English site (_pkgdown.fr.yml
│                              + docs/fr + the toggle removed; FR vignette-articles stay in Articles;
│                              19n regrouped its reference index -- a real `Colours` section in
│                              Everyday use instead of five helpers hidden on the superseded
│                              `tab_many()` page, an `Inspect a table` one, and check_pkgdown() green)
├── tab_xl.R        (~1015 L)  Excel export via openxlsx2 (Suggests-only; Phase 10h). Single-tab-first
│                              + list. tab_xl() orchestrator -> tab_xl_plan_one() (pure per-table plan:
│                              raw values + numFmt codes w/ stars + a precomposed per-cell STYLE grid
│                              via xl_build_styles) -> xl_write_table() (writes values, then
│                              xl_apply_styles = register deduped fonts/fills/borders + composed xf,
│                              apply by id with set_cell_style, then the numFmt merging pass). Consumes
│                              tab-export-prep (roles/refs/bold) + format(syntax="excel"); transpose
│                              arg. 17g: consumes ann$text_hex/$bg_hex DIRECTLY (private text_pal/bg_pal
│                              palette GONE, slot->hex single-sourced via fmt_channel_codes); the inert
│                              n_min/hide_near_zero/conditional_format args DROPPED.
│                              Phase 13c-v: xl_materialize_data (ci-cell/OR text columns; or_numeric
│                              arg), +/x/sigma numFmt, mean/_sd twin col, col_var span header + geometry
├── tab-xl-backend.R (~200 L) Phase 10h openxlsx2 backend: plumbing xlb_* engine wrappers (in-place R6
│                              $, +xlb_merge) + the pure range coalescer (xl_runs/xl_coalesce -> fewest
│                              multi-area dims). Styling-model notes (precompose + set_cell_style path).
│                              Phase o: xlb_dims_each splits a comma multi-area dims to single ranges at
│                              the emit (xlb_numfmt/xlb_set_cell_style) -- the OLDER jamovi-bundled
│                              openxlsx2 rejects multi-area dims (the Excel-export crash).
├── tab_md.R         (~760 L) Markdown export: plain padded pipe table + (Phase 10f) pandoc colour
│                              spans [<num>]{.p3} (aligned) via tab_export_prep; md_span_attr/
│                              md_color_cell (13d: slot classes via tx_slot_class, no `.n` neutral --
│                              an uncoloured cell is bracket-free, padded to the same offset);
│                              tab_css(format = "md") = the colour classes only (20a: the argument
│                              names the OUTPUT now, and the tab_md_css() wrapper is deleted).
│                              14m-iii: md_has_color() = the ONE "is this table coloured" predicate
│                              (shared: tab_md()'s ::: div gate + md_render_one's styled = do_color||css);
│                              a STYLED table (coloured OR css) draws sub-table + name-underline
│                              separators as md_blank_row() (fully-ASCII, :empty) that tab_css() collapses
│                              to 1px rules; plain keeps dash rows (byte-clean GFM). The ::: div ships for
│                              ANY styled table (not only css=TRUE) so a doc-level tab_css() reaches it
├── tab-css.R        (~660 L) Phase 13d: THE one CSS generator, shared by tab_md + tab_kable("html").
│                              z11: the rule table carries THREE theme columns (light/dark/print) plus
│                              FACE rows (font-weight/style/text-decoration) emitted only where a theme
│                              DIVERGES from the static bold_slots baseline (tx_face_decls) -- so
│                              light/dark stay byte-identical and print can say "not bold";
│                              tx_resolve_theme() = the theme VALUE vocabulary + the "bw"->"print"
│                              alias (2 callers); tx_chrome_hex() gains a print arm (grey #595959, the
│                              only one readable on the fills); tx_print_block() = the @media print
│                              emission (default on) -- under theme="auto" it MUST also emit a
│                              hook-prefixed layer (layers 3/4 are (0,3,1) and out-specify it) and
│                              carry print-color-adjust:exact (browsers drop fills when printing).
│                              WARNING: tx_css_layer() subsets its VALUE by `keep` -- was latent.
│                              x2: tx_cell_sel = every CELL colour class emitted bare + scoped
│                              (".p1,.tabxplor-tab .p1") so (0,2,0) beats Bootstrap hosts'
│                              .table>:not(caption)>*>* (0,1,1) cell rules (pkgdown/Quarto wash-out).
│                              tab_css() (exported; takes NO table -- the stylesheet is a pure function
│                              of palette+color_type+theme, so one copy styles every table in a doc and
│                              class collisions are impossible); tx_slot_class (slot -> .p/.m/.o/.u),
│                              tx_palette_theme ("auto"->"light"; the ONLY place auto is resolved),
│                              tx_chrome_hex (single source, also read by tab_export_prep's theme_cols),
│                              tx_css_rules/tx_css_render + the tx_dark_hooks/tx_light_hooks page-toggle
│                              selectors. "auto" = 4 cascade layers; their ORDER is the contract.
│                              14j: NO border SHORTHAND (it resets border-*-color to the CELL's hex and
│                              out-specifies the one border-color rule -- the 2-phase bug) and NO column
│                              width (.tx-rv/.tx-tot emitted UNSTYLED = the user's fixed-width hooks,
│                              ?tab_css); .tx-foot keeps the footnote out of the table's max-content.
│                              14k: tx_page_style() = the chrome (html,body) of a page WE build -- the
│                              2 callers are print.tabxplor_kable + tab_html_string; NEVER tab_css (a
│                              host page is not ours to repaint). No vscode-* hooks (webview iframe).
│                              14m-iii: three `.tabxplor-tab table ...` rules (MD-ONLY by selector -- a
│                              `table` DESCENDANT exists only in the pandoc div>table, never the html
│                              engine): reset the host's per-cell border-width to 0 (BEFORE thead th),
│                              redraw our blank-row rules (tr with all-:empty cells) as 1px border-top,
│                              collapse :empty spacer cells. Tames finding 9/10 (host draws black per-row
│                              borders + ugly spacers). format = "html" only; format = "md" omits them.
├── tab-export-prep.R (~940 L) Phase 10d shared exporter prep: tab_export_prep() -> tabxplor_render
│                              (19n: tab_col_var_header() returns `group` beside `label` and
│                              tab_header_runs() RLEs the PAIR -- RLE-ing the label alone merges two
│                              adjacent blocks of ONE variable into a single span);
│                              model (roles/ann/bold/range), consumed by kable/md/plot/xl;
│                              17g: rd_footer()/rd_caption() (the ONE footer sandwich + caption fallback
│                              every backend shares) + roles_totblock_edges() (border formula shared w/
│                              transpose);
│                              resolve_export_opts() (13d: theme=NULL -> options("tabxplor.theme"),
│                              gains "auto" gated by allow_auto; static backends get "light");
│                              Phase 13c-iii col_var header model tab_col_var_header()/tab_header_runs()
│                              (spanning names + suffix-stripped level labels);
│                              Phase 14i variable-NAME model: tab_label_runs() -> roles$label_cols/
│                              label_runs (name each block ONCE: md blanks / html rowspans / xl merges)
│                              + roles$var_name_col (the merged `row_var` col: droppable, vertical,
│                              italic, never bold) + the shared `var_names` arg, whose BOTH drops live
│                              in the prep (the col side = blank col_var_header$label, which every
│                              backend already gates its span row on -> zero backend code). 14j: that
│                              col-side drop moved INTO tab_col_var_header(name_cols=) -- one rule with
│                              the level labels, since a level header may say "mean (sd)" only while the
│                              span says the variable; tab_export_labels()/the `labels` slot DELETED (it
│                              ran on every export and nothing read it). 16e: the plain footer fields
│                              (reg_line/weight_line/stars_legend) DELETED too -- every backend now builds
│                              its whole footer via tab_footer_streams(); only reg_title (the caption) stays
├── tab-transpose-render.R (~315 L) Phase 14o: THE render-level transpose. tx_transpose_render(rd,
│                              backend) flips a FINISHED prep_one_table() model (a transposed column is
│                              heterogeneous -> not an fmt column, cannot be format()ted; so colours +
│                              strings are computed per source column then swapped as plain data). The
│                              result is a SYNTHETIC model: $tab plain-character, $transposed=TRUE,
│                              $cells pre-formatted, $color_src the fmt table for the legend; roles/ann/
│                              col_var_header/label_runs flipped. md/plot need NO branch (char fallback +
│                              ann); html injects $cells + flipped $tooltips; xl writes coloured TEXT
│                              (numbers deferred). Materialise is xl-style when transposing (n col -> n
│                              row); one Total col (14n); leading [var-name, levels] cols; real tab_vars
│                              aborts. 17g: rd2 carries reg_title/caption/empirical_tips through the flip
│                              (drift fix -> transposed reg tables keep title/caption/tooltips); shares
│                              roles_totblock_edges() w/ prep. Object-level tab_transpose() soft-deprecated. §46
├── tab-render-html.R (~595 L) Phase 10e tab_html render seam: render_kable_html() -> ONE engine
│                              (19l DELETED the legacy kableExtra one: it baked its own theme so it
│                              could not do theme = "auto", could not render a transposed model at
│                              all, and was reachable only by naming it. ⚠ kableExtra stays a
│                              Suggests and the `kableExtra` CLASS tab_kable_join() stamps is
│                              LOAD-BEARING -- its print/knit_print route the fragment to the Viewer
│                              and bind the bootstrap tooltips. Do not "clean up" that class.) + tab_kable_join(css=)/scrollbox. 13d: the html
│                              engine is THEME-AGNOSTIC -- colour is a slot CLASS, never inline hex
│                              (inline would beat any @media rule); the theme lives only in the <style>
│                              tab_kable() builds. html_style_block() deleted. 14b: tab_tooltip_attrs()
│                              = the ONE bootstrap tooltip/popover attr builder both engines use
│                              (placement "auto right"; kableExtra takes it pre-classed ke_tooltip --
│                              its spec_tooltip() match.arg CANNOT emit the two-token form). 14e made
│                              "html" the DEFAULT; 14j puts the footnote in a .tx-foot div (width:0 =>
│                              it stops deciding the table's width) + dedups the row classes.
│                              14k: tx_kable_page() (pure; the probe is a DEFAULT ARG = the only way to
│                              test it, testthat is never interactive()) + print.tabxplor_kable = the
│                              ONE place a theme is resolved in R not the browser (a Viewer webview's
│                              @media reports the OS, never the editor). It delegates to kableExtra's
│                              print (its 2 UNEXPORTED html deps bind the tooltips); knit_print is NOT
│                              overridden (a host document is not ours). tab_kable_join(theme=) carries
│                              the intent, but ONLY when our stylesheet ships (engine html + nzchar(css))
│                              -- painting a page we did not style = an unreadable table.
│                              15c: tab_render_scrollbox() (jamovi results only) = scoped <style> +
│                              .tx-scrollbox class (width:max-content; max-width base; overflow-x:auto),
│                              NOT an inline max-width (would out-specify @media). 15c-ii: OS-scaling-
│                              aware cap via @media (device-width) tiers (CSS px = already scaled; screen
│                              not iframe-viewport = no feedback loop); base cap stands if unsupported.
├── utils.R         (~600 L)  .onLoad() (20b: it SEEDS FROM TAB_OPTIONS -- ~35 hand-written
│                              options() calls gone; tx_getOption() moved to R/tab-options.R with
│                              the rest of the subsystem, because tab.R's top-level tail reaches it
│                              while the namespace is still being SOURCED), factor/list utilities,
│                              tx_deprecate_inert/tx_user_call, tx_str_wrap/tx_str_trunc
│                              NOT the colour-palette DESIGN tools (preview_color_grid /
│                              simulate_cvd_farver / plot_oklch_hue_strip_cvd / set_luminance...):
│                              they live in dev/color_palette_tools.R and must stay there -- they
│                              are the sole reason the package would depend on farver + colorspace.
├── tab-options.R    (~430 L) Phase 20b (KEY 1): THE option subsystem -- **TAB_OPTIONS**, one row
│                              per option (`default` · `section` · `arg` = its per-call twin ·
│                              `alias` = the tx_getOption synonym chain · `seed` =
│                              always/if_unset/elsewhere/no · `doc`), plus tx_getOption /
│                              tx_option / tx_option_names / tx_option_default, tx_seed_options()
│                              (what .onLoad calls) and tab_options_rd() (the `@eval` that
│                              GENERATES `?tabxplor-options`). One option was three hand-written
│                              places -- an options() call, an \item{}, and a default restated in
│                              the prose -- kept in step by a comment saying "keep this in sync";
│                              the DEFAULT and the "Per-call `x =`" sentence are rendered from the
│                              row now, so neither can drift. ~25 call sites that respelled a
│                              default (`getOption("tabxplor.anova", "welch")`) read tx_option().
│                              +`tabxplor.total_names` (the four synthetic labels, ONE partial
│                              named vector) and `tabxplor.stars` absorbing signif_levels +
│                              signif_labels (tx_stars_ladder = the ONE reader; the retired pair
│                              still wins if a user set it). ⚠ THE FILE NAME IS LOAD-BEARING: it
│                              must sort before `tab.R` ('-' < '.'), because tab.R's DERIVED
│                              globalVariables() tail calls new_ctx() -> conf_level_default() ->
│                              tx_option() AT SOURCE TIME -- which is also why every computed
│                              `default` is a CLOSURE.
├── tab-args.R      (~1165 L) Phase 20b (KEY 1 + KEY 8): THE argument surface as data.
│                              20h: +**EXPORT_ARGS**, the RENDER surface -- the exporters' half, same
│                              shape, same readers via **arg_table_of()** (DERIVED from
│                              EXPORT_PRODUCERS). A SECOND table because 3 names mean something else
│                              there (`color` a logical not a measure, `subtext`, `stars`) and a named
│                              list cannot hold two rows per key. Its scope rule is NARROWER: a row
│                              for an argument shared by >=2 exporters OR an option's per-call twin;
│                              a single-backend geometry one (`sheets`/`titles`/`colwidth`/the fonts)
│                              stays home, so tx_check_tab_args() checks the exporters SCOPED (the
│                              `tab_build` idiom already in its body). ⚠ only 9 of 24 rows carry
│                              PROSE, by the table's own test: `@param theme` is written 7 times but
│                              the ACCEPTED VALUES differ per backend (`allow_auto = TRUE` only in
│                              tab_html/tab_md/tab_css, so only they take "auto") -- 7 texts for 5
│                              value sets are not one duplicate, so it and `caption`/`css`/`format`/
│                              `file`/`path`/`subtext` + the single-backend twins are DECLARED with
│                              `doc_in_producer = TRUE` (which is what empties the FK) and documented
│                              where they are true. ⚠ the reward is ANTI-DRIFT, not man/ lines: the
│                              exporters keep every formal, so man/ GREW 23 lines -- what it bought is
│                              26 blocks -> 9 declarations, 5 corrected texts, and TAB_OPTIONS$arg's
│                              11-name `allow` list -> EMPTY. ⚠ tab_check_dots()/tab_dots_expand()
│                              stay CROSSTAB-only: an exporter's `...` is a backend pass-through
│                              **TAB_ARGS** = one row per public argument of the crosstab producers
│                              (`producers` · `status`, which may be NAMED when an argument is
│                              deprecated on ONE producer -- `row_var` is a deprecated alias on
│                              tab() and the REAL formal of the leaves · `default` + `default_for`
│                              · `values`/`leaf`/`size`/`na_ok` · `values_from` = the fact table
│                              that OWNS the vocabulary · `values_rd` = its renderer · `option` ·
│                              `check` · `doc`, moved VERBATIM · `doc_with` · `validate`).
│                              THE RULE: *the fact table owns the VOCABULARY, TAB_ARGS owns the
│                              ARGUMENT*. **TAB_ARG_VALUES is DERIVED from it**, contents and order
│                              intact (the DISPLAY_TOKENS precedent), so its four readers did not
│                              move; `validate = FALSE` is what keeps `ci` and `input` out of it
│                              (their own resolvers rewrite/partial-match them).
│                              **tab_args_rd(producer)** = the `@eval` generator behind every
│                              producer's `@param` block -- ORDER from formals(), SET asserted
│                              equal to the declared one at load. **tab_check_dots()** = the
│                              validator that makes `...` a net gain (an unnamed argument refused
│                              by position, an unknown one refused with a suggestion -- and the
│                              suggester must match PREFIXES too, since a formal after `...` loses
│                              R's partial matching). **tab_dots_expand()** fills an unsupplied
│                              argument from its declared default, which is why the leaves kept
│                              their own (`tab_num` starts at color="auto", ref="tot").
│                              20c (KEY 4): **`tab_reg()` joins it** -- all 25 formals declared, the
│                              14 shared with `tab()` gaining it in `producers` (which is what makes
│                              "the two producers ask the SAME question" a checked fact:
│                              tx_check_tab_args() polices that signature like a crosstab one), the
│                              11 reg-only ones carrying `doc_in_producer = TRUE`. ⚠ `tab_reg()` does
│                              NOT get `@eval tab_args_rd()`, and the header says why it was MEASURED
│                              rather than assumed: the two producers share the NAME and the GRAMMAR
│                              of `wt`/`ref`/`na`/`display`/`color`/`ci_method`/`tab_vars`, not the
│                              PROSE -- every one reads differently on a model, so emitting the
│                              crosstab text into ?tab_reg would be WRONG documentation, not
│                              deduplicated documentation. THE TEST for moving prose here is §4's
│                              bundle test: it must remove a DUPLICATE. +`doc_for` (one prose per
│                              producer, `default_for`'s idiom).
│                              +color_measures_rd (from MEASURES' new `doc` member, filtered by
│                              `producers`) and color_signif_rd; `{VALUES}` in a `doc` is where the
│                              generated list is spliced. ⚠ read the rows with `[[`, never `$`:
│                              `r$values` partial-matches `values_from`.
├── tab_reg.R       (~4680 L)  Phase 20f-iiii: **`reg_stage_crude()`** -- THE OBSERVED (CRUDE) BLOCK
│                              BELONGS TO THE OUTCOME, so a one-outcome table builds it ONCE, before
│                              any model, and a several-outcome one builds each with its spec (which
│                              IS its outcome, so the work stays on the parallel axis). It is
│                              FIT-FREE, which is what makes it liftable: the two facts it used to
│                              read off a fit have exact producers -- `reg_positive_level()` (the
│                              function reg_prep_binary() itself calls) and the outcome's first level
│                              (reg_crude_yw() already collapses a foreign `ref_category` to it).
│                              `reg_crude_block()` = the shared arithmetic, so the two paths cannot
│                              fork. It DELETED `share_crude`, the `emp_shared` hand-down, the loop's
│                              last carried state, one refusal and one whole redundant producer.
│                              ⚠ TWO build-time stopifnot in reg_stage_setup() carry it: with one
│                              outcome every spec is built from deps[1,] (so reading specs[[1]] is
│                              legal -- 20f-ii's "true today, stated nowhere"), and
│                              `skeleton_deferred` implies `!empirical`.
│                              + `reg_fit_formula()` = the model formula of the 3+ level engines,
│                              and a MEASURED defect: reg_fit_multinom()/_ordinal() BUILT it from the
│                              bare predictors and never saw the user's, so `party3 ~ race * age`
│                              silently fitted `race + age` -- the interaction left the MODEL, not
│                              just the table. ⚠ they need `environment(fml) <- environment()`: both
│                              store their call and re-evaluate it. And `reg_skeleton_from_fit()`
│                              takes its coefficient names off the MODEL MATRIX (the vector `assign`
│                              indexes), never coef(): a matrix for nnet::multinom (names() NULL) and
│                              one short for MASS::polr (no intercept).
│                              Phase 20f-iii: **the STAGES are cross-spec ASSEMBLERS** and the
│                              per-MODEL half is R/reg-spec-build.R's declared product. EIGHT stages
│                              now: _split (the tab_vars recursion, at the TOP, returning a finished
│                              TABLE -- and axis G's tab_pmap()) / **_setup** (was _fit's tail: the
│                              shape facts, the reref relevel of `data`+`data_canon`, the FIT-FREE
│                              skeleton or `skeleton_deferred`, and the per-spec PLAN -- `want_n` /
│                              `n_names` / `want_emp` / `want_crude`, the de-duplications that used
│                              to be loop-carried `break`/`next` a worker cannot reproduce) /
│                              **_crude** (above) / **_specs**
│                              (the loop: serial, or tab_pmap() when reg_specs_independent() says
│                              nothing -- plus the column LAYOUT the products imply, `built` being
│                              their flattened view) / _footer (⚠ SLOT-MAJOR: every product's gof
│                              rows, then reg_compare_rows() -- UNPORTED, a test between fit OBJECTS
│                              -- then the globals, then the checks; each product's rows are re-keyed
│                              from a pre-make.unique placeholder to `fit_first_col[[i]]`) / _rows
│                              (labels, sparklines, `tab`; the add_n columns are SPLICED from the
│                              products) / _assemble (the crude blocks + the model columns, which
│                              already carry `obs`/`gap_se`) / _tips (resolves the products'
│                              (col_idx, row) placeholders against `labels` / `disp_levels`, which is
│                              what freed the tooltips from needing _rows first) / _finalize.
│                              ⚠ THE ORDER IS STILL THE SOURCE ORDER, but the message stream is
│                              SPEC-major now (one model's diagnostics arrive together) -- 20f-iii's
│                              one declared delta, and measured at ZERO reordered cases across
│                              verify_reg_specs.R's 290. + `reg_gof_tibble()` -> per-spec
│                              **`reg_gof_rows()`** and `reg_global_rows()` per-spec (one caller
│                              each, each loop body already a pure function of `i`).
│                              Phase 20e (KEY 6): **reg_build() IS A STAGED BUILD** -- 20 deparsed
│                              lines over named stages (was 726 source lines, 39 top-level
│                              locals, 7 closures, eleven unnamed phases: the package's largest
│                              function), each named after THE PART OF THE TABLE IT PRODUCES.
│                              **`new_reg_ctx()`** is new_ctx()'s idiom, fourth use -- the
│                              FORMALS are the contract, `as.list(environment())` the body, the
│                              globalVariables() mirror DERIVED -- and 19i's lesson: a stage product
│                              is DECLARED, never left to appear (an undeclared key is ABSENT, so its
│                              own is.null() guard ERRORS instead of firing). ⚠ NO ctx key may start
│                              with a dot: `as.list(environment())` defaults to all.names = FALSE, so
│                              `.fit_cache` would be SILENTLY DROPPED and the jamovi cache would just
│                              stop being threaded -- the key is `fit_cache`, reg_build()'s formal
│                              keeps its dot. **`reg_ctx_locals(ctx)` = ctx_settings_locals()'s twin**
│                              (`c(ctx, ctx$shared)`), so `shared` stays ONE nested record PROJECTED
│                              at each stage head, never flattened into a second carrier (19i's
│                              finding), and tx_check_reg_ctx() keeps the two name sets disjoint at
│                              load. ⚠ **THE STAGE ORDER IS THE SOURCE ORDER AND IS LOAD-BEARING**:
│                              every fit -- the reported ones, the linearity refits, the crude
│                              univariable ones -- may inform or warn, so a reorder moves the MESSAGE
│                              STREAM, which verify_reg_specs.R compares in order. ⚠ that also
│                              corrects the plan of plans' §5.4 "the ONE place a fit happens = the
│                              parallel seam": on a 5-predictor `empirical = TRUE` table the model
│                              fits are a MINORITY, so 20f measured all four. The 7 closures became 4
│                              top-level functions (reg_cols_coef/_ame/_vsrest, dispatched by
│                              REG_BUILDERS; reg_emp_frame, shared by TWO stages; reg_set_obs --
│                              20f-iii takes `(bi, e, f, sp, ctx)`, never `fits[[fi]]`;
│                              reg_add_emp_cols) + one local.
│                              Phase 20d (KEY 7): **reg_marginal() is a DISPATCHER** between two
│                              engines, reading the estimand row's declared `engine`. The fast one is
│                              **reg_marginal_gcomp()** -- one counterfactual sweep per (predictor,
│                              level) over R/reg-influence.R's reg_gcomp_maker() /
│                              reg_gcomp_cat_maker(), printing est +- crit * reg_delta_se(G, vcov(fit))
│                              through the existing reg_wald_finalize(), and taking the adjusted
│                              predictions from the SAME sweep (so the whole avg_predictions() pass
│                              disappears). The slow one is **reg_marginal_me()**, today's body
│                              verbatim. It returns the SAME shape, so reg_marginal_column() /
│                              cols_ame() / cols_vsrest() / reg_apply_display() / reg_empirical_fit()
│                              are untouched; a NULL from the fast route falls back for the WHOLE call
│                              (never a per-contrast mix -- one column, one convention). Measured
│                              10.0 s -> 1.2 s (binomial) and 45.2 s -> 5.2 s (multinomial).
│                              + `want_se = FALSE` and `vcov = FALSE` wherever the interval is
│                              DISCARDED (reg_apply_display's fold keeps the column's own CI; the
│                              prediction pass only ever reads $estimate) -- byte-identical estimates,
│                              4-7x on those calls, and it helps the routes that stay numeric.
│                              + reg_marginal_basis_warn() = 18z15's poly()/ns() guard, HOISTED out of
│                              the per-predictor loop so it runs once whichever engine answered (both
│                              build the counterfactual by re-evaluating the formula, so both can be
│                              silently wrong on a basis expansion).
│                              ⚠ reg_empirical_fit()'s marginal branch assigned its per-predictor
│                              estimates to `est` -- ALSO its estimand-row argument. Latent while
│                              nothing read that argument twice; the moment the engine is read off it
│                              per predictor, every predictor after the first aborted inside a
│                              tryCatch and lost its `obs` in silence.
│                              Phase 20c (KEY 4 + KEY 5): the SURFACE is one word per question --
│                              `dependent`->**`outcome`** (package-wide: the formal, `deps$outcome`,
│                              `n_outcomes`, `reg_per_outcome()`, `reg_measures(data, outcome)` and
│                              the `test` tibble's DECLARED column), `split_var`->**`tab_vars`**,
│                              `reference`->**`ref`**, `method`->**`ci_method`**,
│                              `inverse_two_level_factors`->**`outcome_level`**,
│                              `stats`+`compare`+`baseline`->**`stats`**, `.fit_cache`->`...`.
│                              25 named formals + `...` (was 29 + `...`); every retired spelling
│                              ABORTS naming its replacement (REG_RETIRED_ARGS, the 19e idiom -- no
│                              permanent aliases, `tab_reg()` being unreleased).
│                              **`reg_resolve_stats()`** is `stats =`'s grammar: a `stats` element is
│                              always a KEY, carried in the NAME when it has a parameter and in the
│                              value when it does not (`ref = c(var = "level")`'s grammar) --
│                              `c("n","aic","compare_sequential")` / `c("n", compare_baseline = "M1")`
│                              / `c(compare_baseline = 2)`. It returns the plain (stats, compare,
│                              baseline) triple every producer below already speaks, so nothing
│                              downstream changed. ⚠ a comparison key ADDS a row and RESTRICTS
│                              NOTHING (naming only it keeps the per-family default set); ⚠ one
│                              declared behaviour change -- `stats = FALSE`/"none" now hides the
│                              comparison too, which `compare` (applied unconditionally) did not.
│                              `reg_prep_binary()`/`reg_positive_level()` take the LEVEL, not a
│                              direction; `conf_level` is `NULL` on this producer too (20b's idiom),
│                              resolved at the boundary -- so `fit_spec` must read `a$shared$conf_level`,
│                              never tab_reg()'s own local.
│                              Phase 12c–12h: unified regression tables. 19m-ii moved the ARGUMENT
│                              BOUNDARY out to R/reg-resolve.R, so `tab_reg()` itself is 147 lines
│                              (was 821) holding ONE user message (was 30): the retired-args guard,
│                              three match.arg, the multi-dependent x model-list RECURSION (a
│                              dispatch over the call SHAPE, not resolution -- moving it would make
│                              the return type a union), one `reg_resolve_args()` call, `reg_build()`,
│                              and a tail that READS the returned record instead of recomputing from
│                              four closures. **`new_reg_spec()`** (beside `new_reg_shared()`) is the
│                              typed per-model record its two hand-written 14-field literals became:
│                              11 fields, `fit_family` renamed from `family` (it is `est$fit`, the
│                              internal LINK key, one word from reg_call's `families` = the OUTCOME
│                              ones), and `effect_shape` (0 readers) / `do_exp` (= `isTRUE(est$exp)`)
│                              / `eff_word` (= `reg_eff_word(est, empirical)`, derived in reg_build
│                              where `empirical` is FINAL) DELETED. `reg_test_row()` gained **`dep`**
│                              -- which dependent a footer row is about, a DECLARED `new_test_tibble()`
│                              column (an undeclared one would be read as a grouping variable), NA on
│                              a crosstab row; it replaced the length coincidence `test_grid_reg()`
│                              paired `meta$dependent` against `unique(test$col)` with.
│                              tab_reg() over ONE engine
│                              (stats::lm/glm, survey::svyglm/svyolr, svyVGAM::svy_vglm, nnet::multinom,
│                              MASS::polr; broom::tidy). Phase 15e: `family` is resolved PER DEPENDENT
│                              (scalar / vector / named vector; `family_for`/`do_exp_for`/`effect_shape_for`/
│                              `eff_word_for`/`color_for` -> per spec, read as `sp$*` in reg_build), so one
│                              table mixes outcomes of different families -- each column carries its own
│                              `model_family` fmt attr; reg_model_lines() = one "Model:" line per family;
│                              reg_gof_tibble takes a per-fit family vector.
│                              with family dispatch and exponentiate-driven fmt shape: gaussian beta
│                              (additive -> `diff` field, type="coef", display="coef", ci_type="diff",
│                              color="diff", `var`=var(Y) for the beta/SD(Y) effect-size colour) |
│                              binomial OR / poisson IRR / multinomial OR / ordinal cumulative OR
│                              (multiplicative -> `or`, type="row", display="or", ci_type="or",
│                              color="OR"). A binary outcome is tab_reg(family = "binomial").
│                              reg_* helpers (fit/skeleton/column/build + reg_wald_from_tidy shared Wald
│                              CI<->p dual); `predictors` char-vec (one model, dependent may be a vector)
│                              vs named list (model comparison); per-variable reference= relevel. 12c-ii:
│                              `trials` grouped-binomial + formula escape-hatch; reg_build fits-all then
│                              columns-all. 12d: reg_fit_multinom (one OR col per y.level category "j vs
│                              ref", reference= keyed on the outcome sets baseline) / reg_fit_ordinal
│                              (polr cumulative OR, Constant NA; reg_ordinal_diagnostic = Brant PO test,
│                              self-heals fit$call for brant); weighted MNL/ordinal deferred (guard).
│                              12e-i: orthogonal effect="ame" (marginaleffects Suggests) -- reg_marginal
│                              (avg_comparisons/avg_predictions, newdata REQUIRED, wts=wt population-avg)
│                              + reg_marginal_column composing AME-first "{diff} ({pct})" (prob-scale
│                              type="row"; gaussian/poisson type="coef"; MNL/ordinal = one AME col per
│                              outcome category). No new fmt fields/tokens; coefficient path byte-identical.
│                              12e-ii: `at="reference"` profile axis (reg_reference_grid_values +
│                              datagrid -> comparisons/predictions): MER-at-reference (effect="ame",
│                              label AME->MER) + MNL "j vs rest" OR at profile (coefficient, lnor->exp,
│                              reg_marginal_column shape="or"); at no-ops on ordinary coefficients.
│                              12f: model-summary FOOTER (reg_glance/reg_gof_tibble/reg_footer_stats: N/
│                              LR-null/McFadden/AIC/BIC, lm R2/adjR2/F/sigma, poisson dispersion; svyglm
│                              degraded) + compare= (reg_compare_rows/reg_compare_guard, LR/F/dAIC)
│                              stored in the `test` attr with DISJOINT discriminators; DISPLAY-ONLY
│                              (rendered by R/tab_classes.R print_reg_footer/reg_footer_lines). One new
│                              fmt token "gof" (uncoloured). Crosstab p-value cells gain in-cell test
│                              labels ("{pvalue} (Chi2)"). stats=/compare=/baseline= args. 12g: SURVEY
│                              designs (wt=/ids=/strata=/fpc=/nest= -> reg_make_design per model; a prebuilt
│                              design as `data` -> reg_subset_design/reg_resolve_design; reg_svyglm_env
│                              binds svyglm for AIC/anova when survey unattached; reduced weighted glance
│                              n/wald_null/nagelkerke[/cox_snell]/Rao-Scott-AIC via reg_aic_value; weighted
│                              compare = anova.svyglm Wald). Weighted 3+ level: svyolr / svy_vglm.
│                              split_var = tab_vars analogue (reg_build recurses per group on shared
│                              skeleton_data, stacks grouped_tab (split_var,var); tab_spread works,
│                              group-aware print_reg_footer). multiplier (OR^k) + 14v `empirical`
│                              (renamed from empirical_OR; cross-family crude companion: reg_empirical /
│                              reg_empirical_columns per family -- binomial %/OR|%/diff [16d: the
│                              risk-diff companion CI = two-proportion WALD, matching method_diff="wald"
│                              + the model AME's Wald so the merged legend names ONE method], gaussian
│                              mean/diff [diff/SD(Y), type=coef], poisson rate/IRR; multinomial =
│                              tooltip via reg_empirical_tips -> `empirical_tips` table attr; z5:
│                              reg_empirical_columns RETURNS list(cols, effect) -- the crude effect
│                              vector reg_build writes into each model column's `obs` field [one crude
│                              block serves every model column when n_dep==1, hence model COMPARISON
│                              works; per fit when several dependents]; reg_write_group_obs (called at
│                              THE one point the split groups are parallel tibbles, before vec_rbind)
│                              fills `obs` with the FIRST group's estimate for color="between_groups",
│                              keyed by reg_skel_key not position [the compound-formula path builds a
│                              per-group skeleton]; per-spec
│                              for a dependents vector). No new fmt fields. 16d: reg_meta$wt (weight
│                              name for the "Weighted by" footer).
│                              12h (display): estimate_display= arg -> est_ci token (estimate + visible
│                              [ci_inf;ci_sup] bracket, no 1/x; fmt_class.R only) | "prob"/"ame" fold
│                              predicted prob / AME into the OR cell via {} grammar (binomial coef only,
│                              reg_apply_estimate_display + reg_marginal). No new fmt fields.
│                              17h (integration, all internal + byte-identical): reg_build takes
│                              (data, specs, shared, split_var, .fit_cache...) -- the 5 scalar family/
│                              do_exp/effect_shape/eff_word/color formals + sp_get() are GONE (specs are
│                              the truth, read as sp$*; the ~18 remaining args ride ONE `shared` list, so
│                              the split recursion no longer re-lists them). reg_wald_finalize() = the ONE
│                              est+/-crit.se -> p-dual -> exp assembly (backs reg_wald_from_tidy + the
│                              reg_fit Wald branch + reg_reref_fit_res); reg_skel_key/reg_skel_match = the
│                              "\r" skeleton-align idiom; reg_cleanup() = the 8 cleannames sites;
│                              reg_complete_frame() = the ONE model complete-case frame (reg_fit + the
│                              empirical/tips emp_frame_of share it). The empirical arms fold into the
│                              REG_EMPIRICAL fact table (per family: base+effect column SHAPE + CI method)
│                              driving one emp_col() builder; ci_settings' method_mean_diff/_ratio read
│                              REG_EMPIRICAL, so "empirical CI == what the legend names" is data. Multinomial
│                              tips stay a separate arm. predicted_unadjusted CUT (the Emp.%==unadjusted
│                              identity stays a test-only assertion, test-tab_reg-empirical.R).
│                              z8-B: reg_gap_se_columns() = the gate + per-term loop writing `gap_se`
│                              for `color = "adjustment"` (six facts, all pre-existing: sp$color,
│                              the crude SHAPE row [REG_EMPIRICAL gained `link`; two() now returns it],
│                              a live f$fit, reg_same_estimand() [shape ci_type == column's -- also
│                              closes a z5 defect: poisson + effect="ame" paired an additive AME with
│                              the crude RATIO], equal nrow = PROVEN same rows, and
│                              reg_estimand_collapsible() = ruling Q1(b), no test on a conditional OR).
│                              set_obs_if() writes obs + gap_se together; reg_crude_y() = the ONE
│                              outcome recode shared with reg_empirical().
│                              z9: NUMERIC predictors get a crude column too -- reg_empirical_numeric()
│                              re-calls reg_fit() with ONE predictor + the model's own family/design/
│                              method/inverse/multiplier (so ruling Q6 is structural), on the model's
│                              population via the new internal reg_fit(drop_extra=) (vars joining
│                              drop_vars but NOT the formula; the pre-filtered frame would break a
│                              PREBUILT design's keep_mask). Native-scale, so ONE fit serves the exp'd
│                              column, its log twin and the gap test. reg_num_overlay() splices those
│                              rows in at exactly TWO single sites -- emp_col()'s twin two() (the only
│                              place the effect shape is known; earlier would write the AME into the
│                              SHARED rd_fields and colour the blank Obs_% cell) -- and the base cell
│                              stays NA (SS4.1: the fit's only base-scale output is the MARGINAL rate for
│                              every numeric predictor), its distribution going to empirical_tips.
│                              reg_gap_se_columns() gained the numeric arm (crude leg =
│                              reg_coef_if_maker on the univariable fit, kept only for `adjustment`,
│                              build-time local, NEVER cached) + the |k| rescale. `multiplier` = the
│                              UNIT a numeric effect is reported per, DEFAULT "sd": scalar applies to
│                              all, named vector overrides, 1 = per unit; resolved ONCE in tab_reg()
│                              (reg_resolve_multiplier/reg_predictor_sd/reg_weighted_mean) on the
│                              PREDICTOR complete-case frame so one predictor keeps one unit across
│                              outcomes/models/split groups; consumed by reg_fit + reg_marginal
│                              (variables=list(v=k)) + reg_reref_fit_res (est*k, se*|k| in reg_fit's
│                              own order -> byte-identical, and multiplier LEFT the reref gate, so a
│                              scaling change is a cache HIT) + the row label. reg_is_factor_var()
│                              (factor/character/LOGICAL) = the ONE predictor-kind predicate replacing
│                              5 disagreeing sites (fixes a logical predictor rendering blank);
│                              reg_meta gains predictor_types + the resolved multiplier
│                              19a: the family PREDICATE table gains 5 rows beside z3's three --
│                              reg_fam_glm / _overdispersed / _disp_known / _disp_estimated /
│                              _svy_fitted (the last = "this fit comes from svyglm", the ONE fact behind
│                              both `use_svy` and `use_wald`: an svyglm has no ordinary likelihood).
│                              21 hard-coded whitelists absorbed, incl. 2 in reg-assumptions.R.
│                              z13: `na` is a THREE-value family (drop_by_outcome default / drop_by_model /
│                              drop_all) implemented through reg_fit(drop_extra=) -- no pre-pass on `data`
│                              (which breaks a PREBUILT design's keep_mask), so the "ignored for a survey
│                              design" caveat is gone; reg_same_frame() = the twin of reg_same_estimand,
│                              gating `obs` as well as its gap SE (a model on other rows had kept the
│                              descriptive colour); reg_color_notes() = THE producer of "the colour you
│                              asked for cannot be computed here" (4 hand-written blocks + 2 silent cases
│                              -> 1 table, two kinds: no-colour / no-test); reg_term_tests() +
│                              reg_term_test_line() = the shared core of the interaction test and the new
│                              per-predictor GLOBAL one (`stats = "global"`, IN THE DEFAULT SET, no extra
│                              fit, terms with 2+ coefficients only) -- a footer LINE, so its
│                              discriminators must be registered in is_reg_footer + reg_footer_lines'
│                              carve-out + tab_footer_streams; reg_level_counts() + `add_n = TRUE` = the N
│                              per predictor level, a BUILT column (role "n", read by forest_plot's model pick,
│                              reg_spread_models' GOF key and the [dep] strip); reg_detect_family: any
│                              numeric -> gaussian (integers included), matching the jamovi selector
│                              z14-iii: reg_empirical(design_spec=) -> the crude bases `emp_n_ci` /
│                              `emp_n_draw` climb the SAME rung ladder as tab()'s cells
│                              (svy_inference_mode; the local getOption read is gone), design values
│                              from svy_var_mean() per predictor level, per-level fallback. `emp_n_draw`
│                              is per (level, CATEGORY) -- the multinomial html tooltip prints its
│                              intervals. reg_resolve_design() maps its complete-case mask through
│                              `.svy_row`, and the split branch NO LONGER subsets the design nor passes
│                              it through utils::modifyList() (which merges a survey.design's $variables
│                              COLUMN BY COLUMN -> an error on unequal groups, wrong rows when calibrated)
├── reg-assumptions.R (~950 L) Phase 18z15: THE model checks of a tab_reg() table, their CURE
│                              (`shape =`) and the primitives its plots draw.
│                              20f-iii: **reg_check_rows() is PER SPEC** -- `(data, f, sp, shared,
│                              stats, col_var, grouped)` returning that model's rows or NULL, its
│                              accumulator argument gone. Its two siblings (reg_gof_rows /
│                              reg_global_rows) moved the same way; each had one caller and each loop
│                              body was already a pure function of `i`.
│                              Phase 20f: **EACH CHECK COSTS WHAT IT SAYS.** `REG_CHECKS` gains
│                              **`cost`** ("free" = arithmetic on the fit in hand -> the DEFAULT
│                              `stats` set | "refit" = it fits a model -> asked for by name), because
│                              the two fit-based ones were 87 % of a default binomial table at
│                              n = 200 000 and 80 % of an ordinal one. Readers: reg_checks_default()
│                              (ONE caller, reg_footer_stats()'s default composition -- reg_check_rows()
│                              still asks reg_checks_for(), so a named check is computed and shown) and
│                              reg_checks_costly(). ⚠ INDEPENDENT of `panel`: a panel is always free,
│                              so reg_check_plots() never filters on it. `stats = "all"` MEANS ALL now
│                              (it was a synonym of the default set -- a name that already lied).
│                              + the three de-duplications the 20f measurement found:
│                              **reg_nested_test(base, aug, use_f)** = THE test between two nested fits
│                              ALREADY IN HAND, the route Linearity takes instead of drop1() (which
│                              refits `base_fit`, 1.02 s vs 0.028 s at n = 200 000). ⚠ it IS what drop1
│                              returns, BIT FOR BIT on both arms -- the F arm divides by
│                              deviance/df.residual of the AUGMENTED fit, which is drop1.glm's own
│                              dispersion at scale = 0 and is NEITHER the Pearson one summary() reports
│                              NOR what anova(base, aug, test = "F") uses (14.25 vs 12.47 on a
│                              quasipoisson fit), so neither may be substituted; pinned with
│                              expect_identical in test-reg-checks.R. reg_term_tests() survives as the
│                              DESIGN arm (regTermTest refits nothing anyway).
│                              **reg_check_influence_pass(fit, want, V)** = Dispersion and Influence as
│                              ONE decomposition read two ways (one vcov, one influence closure, one
│                              sweep of the p unit contrasts; reg_if_se(d) vs max|d|). They were FOUR
│                              vcov() calls per fit -- on a multinomial one, four multinomHess
│                              re-derivations at 0.757 s each. The two footer ROWS stay two declared
│                              rows. + reg_fit_vcov() (the per-fit constant) and the `V =` argument on
│                              reg_check_model_se / reg_coef_if_maker / reg_score_multinom /
│                              reg_score_polr. ⚠ reg_check_model_se keeps its own svy_vglm `fit$var`
│                              degradation locally: that is a SANDWICH, and handing it to
│                              reg_score_polr as the bread would double-count the design.
│                              **the Brant test runs where its row is built** (reg_check_rows'
│                              proportionality branch), not at fit time: it used to run on EVERY polr
│                              fit -- the reported one, both Linearity refits, every crude univariable
│                              one -- and be read once, and its "assumption is rejected" warning fired
│                              from each (the crude fits are suppressMessages'd, not suppressWarnings'd).
│                              `attr(fit, "brant_po")` is GONE.
│                              `REG_CHECKS` = the fact
│                              table (one row per check: `noun` + `types` = discriminator -> the
│                              INSTRUMENT, both BARE MSGIDS -- a top-level gettext() freezes the build
│                              locale, so reg_check_label() translates at render + a dead-code anchor
│                              keeps potools able to extract; `kind`/`digits`/`families`/`weighted_ok`/
│                              `per_predictor`/`cost`/`panel`), read by reg_checks_for(what=) = THE
│                              selection rule (the reg_crude_shape pattern), reg_check_spec_entries()
│                              (-> reg_footer_spec) and reg_check_expand() (a user's KEY -> the `test`
│                              discriminators). names(REG_CHECKS) IS the `stats =` AND `check =`
│                              vocabulary, so label, argument and panel title cannot drift; z15-iii
│                              added two TAUGHT-BUT-NEVER-SCORED rows (residuals/normality) whose EMPTY
│                              `types` IS "a panel, no footer row". NO new statistic engine: Linearity =
│                              reg_fit(add_terms=) + reg_nested_test() (20f), its squared term from
│                              reg_shape_term() -- the SAME builder `shape = "quadratic"` emits, so the
│                              check and its cure are one object;
│                              Dispersion + Influence = reg_check_influence_pass() (20f: ONE sweep read
│                              twice) over reg_coef_if_maker() + reg_if_se() (max SE_robust/SE_model,
│                              and max|IF_i(e_j)|/SE_j == stats::dfbetas() to cor 0.999999, but working
│                              for polr/multinom and design-aware);
│                              Proportionality = reg_ordinal_diagnostic(), run at its own row; Collinearity =
│                              car::vif() (the ONE new Suggest; absent -> no row, never a hand-roll).
│                              z15-ii `shape`: a shape either RECODES THE COLUMN (log/sqrt/quantile
│                              groups -- reg_resolve_shape + reg_shape_apply + reg_cut_quantiles, at ONE
│                              boundary in tab_reg(), so a cut predictor genuinely IS a factor and
│                              inherits the whole factor machinery with no code) or ADDS ONE TERM
│                              (quadratic -> reg_shape_terms/reg_shape_add -> reg_skeleton(shape_terms=)
│                              on the COEFFICIENT path only + reg_fit/reg_empirical_fit(add_terms=)).
│                              WARNING reg_shape_term() returns the DEPARSED string (deparse drops the
│                              spaces around `/` a pasted one keeps -> the curvature row rendered EMPTY).
│                              z15-iii primitives (base R, no dependency): rd_wquantile (ONE producer for
│                              the bins, the panels and `shape="quintiles"`), rd_link_y, rd_bin (the
│                              THEORETICAL band, not arm's empirical one), rd_spark/rd_spark_glyphs/
│                              tx_spark_strip, rd_resid (ONE randomised quantile residual for 5 families;
│                              multinomial refused), rd_qq (the analytic Beta band), rd_thin/rd_with_seed,
│                              reg_curves (-> meta$assumptions, drawn on skeleton_data at the MODELLED
│                              level, NULL with several outcomes)
├── reg-influence.R  (~620 L) Phase 18z8-B: influence functions + the SE of the gap between two
│                              estimators on the SAME rows (the covariance no arithmetic on the two
│                              printed intervals recovers). Pure matrix math; the package's ONLY
│                              survey::svyrecvar() caller; every fn returns NULL, never a wrong number.
│                              reg_if_from_parts(X,W,r) = ONE formula for lm/glm/svyglm, returned as a
│                              CLOSURE over the contrast (U = X*(W*r) is a row scaling, so no second
│                              n x p is ever built); reg_coef_if_maker (its fit adapter, == survey's
│                              own influence attr to 5e-17); reg_crude_if_maker (closed form, no fit,
│                              == the Woolf SE the Obs_OR column prints); reg_ame_if_maker (the
│                              two-term marginal IF, == marginaleffects' SE); reg_if_se (svyrecvar
│                              with a design == SE(svyglm) exactly, else sum of squares).
│                              20d (KEY 7): **THE G-COMPUTATION PRODUCERS**, because an average
│                              marginal effect and BOTH its variances are ONE counterfactual sweep read
│                              three ways. **reg_gcomp_maker(fit, data, wt, ratio)** (lm/glm/svyglm) and
│                              **reg_gcomp_cat_maker()** (multinom/polr, answering for EVERY outcome
│                              category at once -- the two probability matrices serve them all) return
│                              a closure (var, level, ref) -> list(est, G [ANALYTIC], emp, mean1,
│                              mean0); the two IF makers are now their four-line wrappers, the
│                              single-equation one BYTE-IDENTICAL by construction. The 3+ level jacobian
│                              stops being central differences -- reg_prob_engine() gained **dmean()**,
│                              the derivative of its own probs() (softmax: p_j(1{j=c}-p_c)x; cumulative
│                              logit: the two densities at the cuts), so that predictor now has THREE
│                              consumers, not two: 2.4 s -> 6.6 ms per contrast, ~1e-9 on gap_se.
│                              + **reg_delta_se(G, V)** = the standard error a marginal effect PRINTS,
│                              sqrt(G' vcov(fit) G). ⚠ IT IS NOT reg_if_se(): the influence-function SE
│                              is a SANDWICH variance PLUS the empirical-averaging term (measured up to
│                              3.6 % away on a rare level) and answers "is this effect different from
│                              its crude twin"; reg_delta_se is marginaleffects' own quantity (1e-8 on
│                              glm and weighted svyglm alike) and answers "what interval does this
│                              print". Two questions, two variances, never swapped.
│                              + reg_counterfactual(data, var, lv) = the ONE "sample with var set to
│                              this level" rule both makers share. ⚠ it assigns through `[<-`, never
│                              factor(): a fresh factor() DROPS `ordered`, so an ordered predictor got
│                              TREATMENT contrasts where the fit used polynomial ones (measured on gss
│                              rincome: AME 0.1038 instead of 0.0302). It cannot bite through tab_reg()
│                              -- 14r's reg_fit() de-orders predictors -- but the argument is "a level
│                              label" and must be right for one.
│                              z9: reg_ame_if_maker's counterfactual gained a NUMERIC arm -- (level, ref)
│                              are SHIFTS on the observed x, so (k, 0) is the k-unit forward difference
│                              the AME columns show (it used to coerce the column to character).
│                              z10: the 3+ LEVEL core -- reg_if_from_score(S, bread) (the general
│                              M-estimator form; NOT merged with reg_if_from_parts, which exists to
│                              avoid materialising U and has no analogue here), reg_score_multinom /
│                              reg_score_polr (columns NAMED against vcov, so the category-major trap
│                              returns NULL not a wrong number; bread is ALWAYS vcov(), never
│                              solve(polr$Hessian) -- measured 99% off), reg_prob_engine (the local
│                              softmax / cumulative logit, one producer for the score AND the jacobian)
│                              + reg_ame_if_cat_maker (the per-category marginal IF; pinned to
│                              marginaleffects to 10 decimals).
│                              z14-iii: reg_if_align(v, n, des_rows) over svy_row_at() = the ONE
│                              row-space alignment. `[` does not drop rows on a CALIBRATED/PPS design,
│                              so svy_domain_design pads the fit's and svyglm keeps the zero-weight
│                              rows in model.matrix(): a leg built on the complete-case frame was
│                              SHORTER (measured 380 vs 400 -> the gap test silently skipped, and
│                              reg_ame_if_maker's `emp + delta` RECYCLED = a wrong number). Padded rows
│                              carry weight 0, so a zero scatter is exact, not an approximation.
├── plots.R         (~1190 L) z17 (was tab_reg_plots.R): the package's data CHARTS + the ONE model they
│                              read. **tab_estimates()** = one long tibble, one row per (table row x
│                              plotted column), computing NOTHING -- estimate/interval/p from the
│                              accessors the printed table used, scale from fmt_scale_of(), colour from
│                              resolve_color_channel_plans() + fmt_col_ann() (the EXPORTERS' resolver, so
│                              a point is the cell's colour down to the greys), plus `obs` and the gap's
│                              interval. Its three axes are STORED facts: columns = role + col_var +
│                              is_totcol (est_plot_columns / est_crude_of, paired by ci_type -- never the
│                              "Obs_" prefix), rows = tab_render_vars() + tab_row_roles() over the four
│                              label-block shapes (est_row_axis), facet key DERIVED once (est_facet_keys,
│                              ruling D7: col_var, unless a col_var holds several columns of ONE role ->
│                              one panel per column; a crude block serving several models is replicated).
│                              **forest_plot()** = a renderer with no statistics in it: the gridlines are
│                              the column's own break ladder (fp_axis_breaks + legend_break_label, so the
│                              axis and the footer print the same glyph), the two secondary axes come
│                              from the scale record, the GAP BAND is obs (+-|x/) z*gap_se so containment
│                              IS fmt_gap_p() < alpha, the guides come from legend_guide_spec() and the
│                              caption from rd_footer(want_legend = FALSE) + fp_method_line (the ladder
│                              is never printed twice). ONE ggplot out, so `+ theme()` / ggsave() work.
│                              + z15-iii's reg_check_plots() (TEACHING ONLY; the panel set IS REG_CHECKS,
│                              one dispatch reg_panel_build(); refits through reg_fit() from
│                              reg_meta$fit_spec, ABORTS on an N mismatch) -- the OPPOSITE contract, said
│                              in both help pages. tx_plot_deps/_colors/_theme = the shared seam (was
│                              reg_plot_*). or_plot() + lm_plots() DELETED. ggplot2 (>= 3.5.0) guarded.
├── jmvtab-cache.R  (~1250 L)  20g-ii: **jmvtab_levels_collapse()** (the merge tick-boxes' Array ->
│                             new_lvl_collapse()'s shape) and **jmv_order_after_collapse()** = THE ONE
│                             PLACE THE TWO LEVEL SPECS MEET. The tick list shows the SOURCE levels --
│                             it must, or a merge could not be undone -- so the JS writes a RAW order,
│                             while the table's levels are the merged ones; this maps one onto the
│                             other, and without it jmv_relevel_cols()'s `ord %in% levels(f)` would
│                             drop every merged level's raw names and the reorder would silently
│                             revert. ⚠ the merge is PRE-aggregate (tab_prepare), the reorder POST --
│                             so the tier-1 keys and the tier-2 test key must NAME the merge spec
│                             (`ce$fp_map` is fingerprinted before tab() runs, so nothing else moves),
│                             which makes a merge a declared tier-1 MISS where a reorder is a hit.
│                             Tier 3 needs no code: `structural` is a NEGATIVE set. +
│                             **JMV_TAB3_REAPPLIED** = that set's complement, hoisted out of
│                             jmv_tab3_base_key() to a constant so one fixture can assert D12's own
│                             invariant (every name in it IS an `opts` key) -- the ⚠ had said it since
│                             19k with nothing checking it. Schema 18 -> 19.
│                             17i: the SHARED cache kernel at the top (jmv_cache_config +
│                             jmv_store_new/migrate/env/fetch/put/evict/cached, ONE byte-bounded LRU
│                             O(n log n), canonical entry list(value,bytes,seq); jmv_hash/jmv_col_fp),
│                             consumed by BOTH stores as config -- JMVTAB_CFG (3 tiers agg/test/tab3,
│                             schema 6) + thin jmv_cache_* wrappers. Then jmvtab live multi-tier cache:
│                             content-addressed store + jmv_cache_aggregate (tier 1-2, tab_aggregate hook) + the Phase 7f
│                             tier-3 CARRIER cache (Phase 9b-7: jmv_carrier_unwrap/wrap store, not a
│                             live tab; jmv_tab3_base_key/tuple, jmv_reapply_digits re-paint +
│                             jmv_tab3_reref/rerefable instant reference re-ref (19j: it rebuilds the
│                             INTERVAL in the same per-col_var sweep, via leaf_ci_plain(), where it used
│                             to fmt_wrap -> tab_ci -> fmt_unwrap a whole record -- the study's own
│                             example of a cache path shaped by a pipeline defect. ⚠ it must pass
│                             `degf` explicitly: tab_ci() derived it from the columns because this
│                             caller passed none. 19k: **jmv_tab3_rerefable() is now stated as
│                             "everything the re-ref RECOMPUTES may differ; everything it copies must
│                             not"** -- so `ref`/`ref2`/`geom`/`ci_method` LEFT the identity set and a
│                             diff<->ratio or a CI-method toggle is a re-ref, not a rebuild. Both
│                             restrictions were vestigial (19e's engine reason died with tab_ci();
│                             D12's four method_* keys never reached the tuple at all, being misnamed
│                             `"ci_method"` in `reapplied`). The re-ref restamps `meta$scale` and
│                             `meta$ci_method` from the SAME ci_res the bounds come from, which is
│                             what makes a geometry change safe. jmv_reref_shape_ok()'s
│                             `color=="auto" && ci=="diff"` exclusion is GONE too)
│                             -- ONE SWEEP PER
│                             col_var there, as the build runs one leaf per col_var: `or`'s 2x2 is
│                             (this level) x (the ref2 level OF THE SAME VARIABLE), so a pooled sweep
│                             compared a partyid level against a race one. The TUPLE keys
│                             `comparison = display_comparison(display)`, not the raw display string:
│                             .return_armed returns before tab_apply_display, so the only way display
│                             reaches the carrier is by naming the comparison -- and that absorbed the
│                             separate `or` flag) + jmvtab_build
│                             (engine-free core; reuses tab() via .cache) + jmvtab_ref_vector (ref-picker)
│                             + jmvtab_levels_order/jmv_relevel_cols (7g-ii level-reorder,
│                             post-aggregate; .levels_order arg on tab()).
│                             19k: **NO rule is mirrored here any more** -- jmv_population_descriptor
│                             (a line-for-line copy of tab_cache_keys(), in the file that also read
│                             the real one) and jmv_apply_display (D11: its `ci == "cell"` block wrote
│                             `pct_ci` onto MEAN columns, whose `pct` is NA -> an EMPTY cell, and it
│                             ran after the ComboBox so it overrode the user; since 19j the leaf
│                             stamps that display itself) are DELETED, the digits floor is
│                             num_digits_floor() and the display writer tab_apply_display(). The
│                             option NAMES are tab()'s (`test` not `chi2`; `OR` retired onto
│                             display/ref2, so tab_deprecate_or() has no caller here). New
│                             jmv_reapply_anova = the tier-4 stamp that makes `anova` a re-derive
├── jmvtab-export.R  (~440 L)  jmvtab export helpers (Phase 7g; 15c robustness): resolveExportPath now
│                             takes (dir, filename, ext) -- fs::path_home Documents default + fs::
│                             path_sanitize filename + quote/bracket strip + format-driven extension
│                             (export_home_dir/_documents_dir/_expand_home/_unwrap/_sanitize_filename
│                             helpers, all fs-guarded w/ base-R fallback); tab_html_string (self-
│                             contained HTML); jmvtab_export (Excel/HTML/MD dispatch) w/ friendly
│                             pre-flight (openxlsx2 / dir-create) + UNwrapped writer so the .b.R
│                             conditionMessage() surfaces the real cause (not "In index: 1."). 17i: also
│                             the SHARED R6 backend helpers jmv_backend_weights/_notice/_export/
│                             _render_html (the 4 verbatim blocks both .b.R files now delegate to)
├── jmvtab.b.R       (~150 L)  Jamovi module backend (R6): thin orchestrator over jmvtab_build + $state;
│                             17i: weights/export/notice/render delegate to jmv_backend_* (export
│                             helpers). 19k: `.run()` is weights -> build -> render -- NO option
│                             travels as a global around the BUILD (`anova` was the last; `ci_print`
│                             keeps its on.exit around the RENDER, where it is read). ⚠ every
│                             `self$options$x` read in `.opts()` takes a `%||%` fallback: the .h.R is
│                             GENERATED and LAGS a .a.yaml edit, so a new option reads NULL until the
│                             maintainer's next prepare(). 20g-i: **AN OPTION IS NAMED AFTER THE
│                             ARGUMENT IT DRIVES** -- exactly, or `<argument>_<slot>` when several
│                             fold into one (`ci_method_cell`... -> `ci_method`; `ref` + `ref_levels`
│                             -> `ref`; `levelOrder` -> `levels_order`) -- so `.opts()` is a
│                             PASS-THROUGH, not a translation table, and the rule is CHECKED
│                             (test-jamovi-vocabulary.R, incl. every `.u.yaml` control and every
│                             `ui.<name>` in the hand-written .js). The two declared exceptions are
│                             `lvs` (jmvcore::Options already defines levels()) and the UI-only
│                             controls. The three synthetic-label keys became ONE `total_names` of
│                             the option's own shape -- the module TRANSLATES them (the R option is
│                             seeded in English), it does not ask. 20g-ii: `.opts()` folds the TWO
│                             level specs TOGETHER (jmv_order_after_collapse), because the tick list
│                             shows the SOURCE levels and therefore writes a raw order while the
│                             table's levels are the merged ones -- one local, then the list literal
├── jmvtab.h.R       (605 L)  Jamovi module UI (auto-generated, do not edit)
├── jmvtabreg-cache.R (~340 L) Phase 15b: the jmvtabreg (Regressions) live-UI fit cache +
│                              jmvtab_reg_build() engine-free core (drives tab_reg(.fit_cache=)). 17i:
│                              rides the SHARED kernel (JMVREG_CFG: 2 tiers digest/fit, schema 3) -- the
│                              duplicated + O(n^2)-evicting store lifecycle is gone, only thin jmvreg_*
│                              wrappers stay; jmvreg_fit_key (ref-INDEPENDENT digest key -> a reference
│                              change is a HIT) + the picker folders jmvtab_reg_ref_vector (reference),
│                              jmvtab_reg_models (15b-ii "+" builder -> `predictors` list / flat pool),
│                              jmvtab_reg_mult_vector (numeric scaling -> `multiplier`),
│                              19k's jmvtab_reg_shape_vector (functional form -> `shape`). 15b-ii raised
│                              the raw-fit ceilings (fit 4->24MB, store 16->96MB) so comparison fits (a
│                              raw reg_fit ~9-11MB) cache instead of graceful-skipping. 15d: the
│                              per-outcome Model table (the `family`/`outcome_level`/`trials` arrays) ->
│                              jmvtab_reg_dep_family/_dep_level/_dep_trials. 15e: jmvtab_reg_build
│                              calls tab_reg() ONCE with per-outcome family/outcome_level/trials VECTORS
│                              -> one mixed-family table (no group-by-family / tabxplor_tabs stacking).
│                              19k: jmv_reg_estimand_opts() (19e's translator for the retired
│                              exponentiate/at/estimate_display) is DELETED -- the UI sends
│                              effect/measure/display straight through; `stats = opts$stats` (a key
│                              .opts() never set) is dropped for tab_reg()'s own default GOF set;
│                              `trials` sends the typed count or NA = "take the observed maximum",
│                              which is tab_reg()'s OWN rule, instead of taking max() here silently
│                              for any integer outcome (one rule, two semantics).
│                              20g-i: every option is named after its tab_reg() argument (schema 6 --
│                              jmvreg_fit_key's member is `outcome_level`, the retired `inverse`
│                              spelling was part of the hash). **jmvtab_reg_stats(compare, baseline,
│                              checks)** = THE `stats =` folder, three controls -> one argument;
│                              `checks` sends `"all"`, which COMPOSES with a comparison key
│                              (reg_resolve_stats strips the comparison and hands the rest on).
│                              ⚠ it ALSO turns the live fit cache off, because the digest fast path
│                              distils the fit away -- reg_check_rows() asks
│                              reg_checks_for(has_fit =), so with a cache a single-model table shows
│                              only the reference-invariant glance rows and never a check or the
│                              per-predictor global test. Without that, the tick-box would be inert
│                              on exactly the path it is for
├── jmvtabreg.b.R   (~170 L)  Phase 15b: jmvtabreg R6 backend (thin orchestrator, sibling of jmvtab.b.R;
│                              .h.R generated by prepare() -- inherit is lazy so it loads before then;
│                              17i: weights/export/notice/render delegate to jmv_backend_*, keeps .hint;
│                              19k: the staged-comparison gate calls jmvtab_reg_staged() -- which
│                              existed for exactly that and whose own caller inlined the predicate
│                              instead -- and `.opts()` speaks tab_reg()'s vocabulary, `%||%`-guarded;
│                              20g-i: it speaks its NAMES too -- `outcome` / `tab_vars` / `ci_method` /
│                              `multiplier` / `shape` / `ref_levels` / `family` / `outcome_level` /
│                              `trials` / `stats_*`, so the six 20c translation lines are gone. ⚠ a
│                              rename DISCARDS that option's value in already-saved .omv files;
│                              20g-ii: +`levels_collapse`, folded by the SAME jmvtab_levels_collapse()
│                              the crosstab uses, and needing no reg cache entry -- jmvreg_fit_key()
│                              fingerprints the PREPARED frame's levels and the merge runs before any
│                              fit, so it invalidates by construction)
└── jmvtabreg.h.R   (~670 L)  Jamovi Regressions UI header (auto-generated by prepare(), do not edit)
```

The `tab_reg()` fit-cache seam (Phase 15b): an internal `.fit_cache` arg threads a cache env into
`reg_build`. On the single-equation GLM coefficient path (wald / value-ci / no split-mult-trials-compound
-ame-mnl), the model is fit ONCE at the canonical reference and distilled into a KB-sized digest
(`reg_build_digest`: coef + vcov + reference-invariant glance); any factor-predictor reference is then
recomputed live via coefficient contrasts (`reg_reref_fit_res`, sharing `reg_wald_crit` with `reg_fit`)
— NO refit, byte-identical to a real refit (`test-jmvtabreg-cache.R`). Heavy paths cache the raw fit
(refit on a reference change). `.fit_cache = NULL` (every ordinary call) is byte-unchanged.

**Other directories:**

| Directory         | Purpose                                                                                      |
|-------------------|----------------------------------------------------------------------------------------------|
| `vignettes/`      | User intro (`tabxplor.Rmd`)                                                                  |
| `tests/testthat/` | testthat v3 tests                                                                            |
| `man/`            | Auto-generated by roxygen2 (never edit by hand)                                              |
| `inst/i18n/`      | Internationalization resources                                                               |
| `jamovi/`         | Jamovi module definition files                                                               |
| `po/`             | Translation files                                                                            |
| `dev/`            | architecture guide + dev scripts + perf harness + `color_palette_tools.R`, `.Rbuildignore`'d |

**Cross-cutting dependencies** (be careful when modifying):

- `fmt_class.R` — used by everything; the `tabxplor_fmt` class is the foundation
- `tab_get_vars()` in `tab.R` — used by all export functions (tab_xl, tab_kable, tab_md, tab_plot)
- `get_color_style()`/`set_color_breaks()` in `tab_classes.R` — shared between `fmt_class.R` (console) and `tab_xl.R` (Excel)
- `.onLoad()` in `utils.R` — sets all default options; changing defaults affects every user

---

## Global Architecture

```
tab() [user-friendly wrapper]
  └── tab_many() [full-featured, vectorised over row_vars and col_vars]
        └── per row_var:
              tab_prepare()  →  tab_plain() / tab_num()  →  tab_pct()
                →  tab_ci()  →  tab_chi2()  →  tab_totaltab()
                      →  tab_spread() / tab_compact()

Export:  tab_xl()  |  tab_kable()  |  tab_md()  |  tab_plot()
```

> **This is the *current* pipeline. 2.0.0 rewrites it around a single aggregate-core** (see roadmap § Keystone + `dev/tabxplor_2.0.0_decisions.md`): the step chain `tab_pct → tab_ci → tab_chi2 → …` collapses into one core, and `tab_pct`/`tab_tot`/`tab_ci`/`tab_chi2` become superseded thin wrappers.

**Ordering invariant** (in `tab_many()`, `tab.R` ~L1146): `tab_chi2()` and `tab_ci()` are independent (either order), but non-first levels (`levels="first"`) must be dropped **after both**, so chi2/ci are computed on the full set of levels. Do not move the level-drop above chi2/ci.

### Key Constraints

| Constraint               | Detail                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               |
|--------------------------|------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| CRAN stability           | Public function arguments must NOT change without deprecation. Internals can change freely.                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          |
| vctrs record contract    | Adding a field to `tabxplor_fmt` requires updating `new_fmt()`, `fmt()`, `format.tabxplor_fmt()`, `pillar_shaft.tabxplor_fmt()`, `vec_arith` methods, and possibly `tab_pct()`/`tab_ci()`/`tab_chi2()`. ~8 functions across 3 files.                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 |
| NAMESPACE                | Auto-generated by roxygen2. Never edit `NAMESPACE` by hand. Run `devtools::document()` after changing `@export`/`@import`.                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           |
| data.table internals     | `tab_plain()`/`tab_num()` rename `col_var` to internal names to avoid data.table conflicts. The user's column names are restored afterward.                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          |
| dplyr class preservation | 30+ S3 methods on `tabxplor_tab`/`tabxplor_grouped_tab` ensure class + attributes survive all dplyr verbs. Missing a method = silent class downgrade to `tbl_df`.                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    |
| Options as config        | All defaults set in `.onLoad()` in `utils.R`. Users override via `options()`. Functions read with `getOption()`.                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     |
| Suggests-only guards     | `openxlsx2`, `ggplot2`, `jmvcore`, `ggpubr`, `cowplot`, `mirai`, `kableExtra` are in Suggests. Every call must be guarded with `requireNamespace()` or equivalent (tab_xl's ONE guard is in `tab_xl()`; `R/tab-xl-backend.R` wrappers are unguarded; `kableExtra`'s two entry points — `render_kable_html()` engine dispatch + `kable_tabxplor_style()` — are guarded, the default `html` engine never touches it).                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  |
| Color break mirroring    | `set_color_breaks()` takes positive-only thresholds. Negative breaks are auto-mirrored internally. Any `pct_breaks` value > 1 triggers ratio comparison instead of difference (the "*2 rule").                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       |
| Mean-diff asymmetry      | For `type="mean"` columns, the `diff` field stores a **ratio** (cell_mean / ref_mean), NOT a difference. Thresholds like 1.15 mean "+15% above reference". This asymmetry propagates into `color_formula()` and `format.tabxplor_fmt()`. **(2.0.0 §3: numeric `diff` becomes a real difference; the ratio moves to the `ratio` field — the never-used `rr` field renamed, placed after `diff`.)**                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    |
| tab_reg                  | Phase 12c–12g LIVE: unified regression tables (gaussian beta / binomial OR / poisson IRR / multinomial OR / ordinal cumulative OR) over lm/glm/svyglm/svyolr/svy_vglm/nnet::multinom/MASS::polr + broom (no parsnip). A binary outcome is `tab_reg(family = "binomial")` (20a deleted the tab_logit/multi_logit wrappers). **The estimand is `effect` x `measure`** (19e, R/reg-estimand.R): the row it resolves to declares the fit, the `exp` flag, the header word and the stored `scale` -- additive beta -> the `diff` field + scale "raw_diff"/"log_coef"; multiplicative OR/IRR/cumOR -> `or` + scale "odds_ratio"; a ratio of means -> `ratio` + "mean_ratio". `exponentiate` / `at` / `estimate_display` are DELETED (`measure = "log"`, `effect = "at_reference"`, a real `display =`); `type`/`ci_type` are gone (19b). The `var` field carries var(Y). 12d: MNL = one OR col per outcome category vs ref; ordinal polr + Brant PO diagnostic. 12f: model-summary footer + compare= in the `test` attr. 12g / z14-i: SURVEY designs — `wt=` (a flat ids=~1 design), or a prebuilt `survey::svydesign` as `data` for anything richer (clusters / strata / fpc / CALIBRATION); `ids=`/`strata=`/`fpc=`/`nest=` are REMOVED (they reached only the omnibus p) and a svrepdesign/twophase is refused. A design's own weights become `.svy_weights` at the shared boundary, so the crude `Obs_*` columns, the AME, the frozen SD, the gap-test influence weights and the footer are all design-weighted (they silently were not); reduced weighted glance (Wald/Nagelkerke/Cox-Snell/Rao-Scott-AIC) + weighted compare (anova.svyglm Wald); weighted 3+ level (svyolr / svyVGAM); `split_var` (tab_vars analogue, tab_spread-able); `multiplier` (the UNIT a continuous predictor's effect is reported per -- **default `"sd"`** since z9, so `Model_*` on a numeric row is per-1-SD, NOT `exp(coef(glm))`, unless `multiplier = 1`); `empirical_OR` (crude %/OR beside model OR, binary; z9: continuous predictors too, from their univariable fit). No new fmt fields; new Suggests svyVGAM. |


---

## Design Decisions

### Type System

- **`tabxplor_fmt`**: vctrs record (`new_rcrd()`) with **21 per-cell fields** (was 15 before v2.0.0 Phase 1a, 18 through Phase 18s which added **`n_eff`** = the effective sample size used for a cell's CI, `p(1-p)/Var_design` (Korn-Graubard): the closed-form flat-design variance under `options(tabxplor.design_effect=TRUE)` on weighted data, `svyrecvar` under a real design, else NA → the CI falls back to the raw unweighted base; non-displayed, carried like `tot_n`, reset to NA on arithmetic; Phase 18z5 added the 20th, **`obs`** = the value a `tab_reg` cell's estimate is COMPARED TO on its own scale -- the observed/crude effect, or under `split_var` the reference group's -- NA everywhere else, so the measures reading it leave those cells uncoloured; displayable as `{obs}`; Phase 18z8 added the 21st, **`gap_se`** = the standard error of the GAP between the estimate and `obs`, on the estimate's own test scale -- written where the two are independent (`split_var` groups), which is what lets `color_signif` apply to `color = "between_groups"`; NA elsewhere, non-displayed) and **14 per-column attributes** (Phase 10i-A dropped `display_spec` → 9; Phase 15e added `model_family` → 10; Phase 17c added `role` → 11; Phase 18z13 added `conf_level` → 12; Phase 18z16-iiiii added **`degf`** + **`basis`** → 14 = "how was THIS column's interval computed", moved off the table because `meta` proved droppable). The critical distinction: fields vary per cell (accessed via `vctrs::field()`), attributes are scalar describing the whole column (accessed via `attr()`). Constructor chain: `fmt()` (public, validates + coerces) -> `new_fmt()` (internal, calls `vctrs::new_rcrd()`). *(Phase 1a reshaped 15→18 in one combined pass — decisions doc §9; `ci` is now derived from the `ci_inf`/`ci_sup` bounds by `get_ci()`, a bounds-shim.)* The 10th attribute **`model_family`** (Phase 15e; `get/set_model_family`, `""` on cross-tables) is a regression column's own family. The 11th, **`role`** (Phase 17c; internal `get_role`, `"model"`/`"emp"`/`""`), is a reg column's role, read by the colour legend to name each column's effect (OR / IRR / β / AME) without matching its rendered `"Emp."` label. The 13th and 14th, **`degf`** (the design's #PSU-#strata, NA = refer to z) and **`basis`** (`"n"`/`"weights"`/`"design"`/`"design_partial"`), are the twins of `conf_level`: the level an interval was built AT, the df it is referred to, and HOW it was computed. All three are written by ONE sweep per build tail, `tab_stamp_inference()` (was `tab_stamp_conf_level`), and the ptype2 reconcile applies the weakest-claim rule (`basis_rank`/`basis_weakest`, min non-NA `degf`) so a bind cannot over-claim. All are picked up automatically by the DERIVED `fmt_col_attrs` (17a) and carried by every cast/ptype2/vec_math reconstructor.
- **`mean` field is mean-only** (the old overload is GONE — Phase 5 landed): `mean` now carries an actual mean only on `type=="mean"` columns; for **pct-type** columns it is `NA` and the cell/reference **ratio** (the "*2 rule") lives in the dedicated **`ratio` field** (Phase 1a renamed the never-used `rr`→`ratio`). The build writes `mean = NA_reals, ratio = <ref-relative ratio>` for pct columns (`tab.R` ~L3608) and the colour engine reads `get_ratio(x)` (`fmt_class.R` ~L2688). *(c-iii audit 2026-07-19 confirmed no field/attribute consolidation is both safe and worthwhile — the fields are all user-contract and none vestigial; the column attributes — 9 then 10 with Phase 15e's `model_family`, now 11 with Phase 17c's `role` — are exported getters (except the internal `role`) AND required per-column so `format()`/colour work on a standalone extracted column.)*
- **`tabxplor_tab`**: tibble subclass via `tibble::new_tibble()` with **3 top-level table attributes** (Phase 17b merged the six 2.0.0-new attrs into one `meta` list): `subtext` (legend text, CRAN-public), `test` (chi2/ANOVA-F results tibble; §16 hard-rename of the old `chi2` attribute; row-bound → `vec_rbind` on bind; Phase 20c renamed its declared `dep` column **`outcome`**, with the argument, and its 39 discriminators are declared in `TEST_ROWS`; Phase 18j added `effect_size`/`es_type`/`pvalue_exact` columns, Phase 18z16-i `deff` = the design effect this row's test corrected by, and the robust discriminators are `chi2_design`/`F_design` -- TWO, not four, because the flat and the full design run the same estimator; `n` is now ALWAYS the raw count), and **`meta`** — ONE named list holding `spec` (Phase 19g, KEY 6: the table IDENTITY —
`list(kind, vars, call)`; it absorbed `vars` and `reg_meta`, see `R/table-spec.R`), `render_extras` (Phase 10i-B, the `list(add_n=, add_pct=)` display intent), `ci_settings` (Phase 13b, CI method/confidence level the colour legend names), `vars` (Phase 14d, variable roles + `wt` + the `caption` + Phase 17c's `row_roles` + Phase k's `var_labels` = the haven/labelled variable-label map for the opt-in `tabxplor.var_labels` export name-swap), `empirical_tips` (Phase 14v, multinomial crude-companion tooltips), `reg_meta` (Phase 14w, a reg table's model record driving its title/"Model:" legend/colour wording, + z15's `fit_spec` = the ~4 KB recipe `reg_check_plots()` refits from), `assumptions` (Phase 18z15, the observed curve of each continuous predictor: the sparkline's data + the linearity panel's), and `color_breaks` (Phase 13a per-table break override, now carried so it survives a pipeline). All three are carried through dplyr verbs by the S3 methods + vctrs reconcilers (`tab_attrs()` returns exactly these three; `tab_bind_attrs()` unions `subtext`, `vec_rbind`s `test`, and reconciles `meta` element-wise through the DECLARED `meta_bind_rules` table — default first-non-NULL, `color_breaks` per named scale). Phase 18z16-iiiii DELETED the `inference` sub-field: "how were these numbers computed" is a per-COLUMN fact now (`degf`/`basis`), read back through the DERIVED `tab_inference_basis()`/`tab_inference_degf()`, and its bind rule moved into the fmt ptype2 reconcile where it fires without being called. A table rebuilt from SEVERAL inputs (`tab_compact()`, `tab_transpose()`) goes through **`tab_meta_merge(metas, ...)`** — reduce, then overwrite only what it recomputes — NEVER a fresh `meta = list(...)` literal: that is how z16-iv found `tab_compact()` dropping `inference` on every ≥2-`row_var` table, and how z16-iiiii found **two more** such sites -- `tab_spread()` (which is also what `tab(spread_vars =)` calls) and `reg_build()`'s `split_var` branch, both losing the WHOLE of `meta`. Their numbers are safe now (the inference facts ride the columns), but `vars` / `ci_settings` / `render_extras` still needed the merge. Guarded by a field-AGNOSTIC probe in `test-meta-attr.R`. Every existing getter (`get_vars_attr`/`get_ci_settings`/`get_render_extras`/`get_empirical_tips`/`get_reg_meta`/`get_color_breaks_attr`) is a thin accessor into `meta`; `set_meta_field()` writes one sub-field (NULL removes it; an emptied `meta` drops the attribute → "absent when unset"). New exported `set_caption()`/`get_caption()` store a caption at `meta$vars$caption`, read by every exporter ahead of `reg_title`. `tab_plain()` now records `vars` at build. **Adding/removing a `meta` sub-field is one getter + one line — never a constructor formal.** **Phase k missing-metadata contract:** all three table-level attrs are OPTIONAL and NULL-safe (getters return `NULL`, consumers treat absent as absent) — a table that loses one, or is downgraded to a plain tibble in a pipeline (fmt columns intact), still prints/exports fully coloured, dropping only what that metadata powered (missing `test` → the summary; `subtext` → the note; reg `meta` → title/legend wording), never erroring. Cell FIELDS + column ATTRIBUTES stay required (a standalone extracted `tabxplor_fmt` column formats/colours on its own). The only loss on a *dropped class* is the console auto-print footer (a bare `print()` on a `tbl_df` runs dplyr's printer, not our S3). Locked by `test-degraded-attrs.R`; `tab_degrade_inform` was deliberately left per-render (not throttled once-per-session — conflicts with the `test-edge-cases.R` degrade-message loops).
- **`tabxplor_grouped_tab`**: extends `grouped_df` for subtabled results (when `tab_vars` are present). Requires separate S3 method for every dplyr verb.

### Export Parity

Cell display values reach exporters by two **non-unified** paths — keep them in sync:

- **`format.tabxplor_fmt()`** (`fmt_class.R`) is the single source of truth for markdown (`tab_md()`), knitr/HTML (`tab_kable()`), and the console (`pillar_shaft`).
- **`tab_xl()`** (Excel) writes the raw `get_num()` value and delegates numeric formatting to Excel's engine, but it now sources the per-cell Excel number-format codes from `format(x, syntax = "excel")` (Phase 10g) — the SAME `format()` masks the text backends use — so a display/digits change no longer needs manual mirroring in `tab_xl.R` (the old `numfmt()` desync is gone). Colours come from `fmt_color_channels()`; roles/refs/bold from `tab_export_prep()`.
- Color is safe: all exporters call the same `fmt_color_channels()` / `fmt_channel_codes()`.

When adding or changing a `tabxplor_fmt` field, follow the `/vctrs-field` skill — it encodes the full ~11-step checklist across `fmt_class.R`, `tab.R`, and the exporters.

### Reference System

The `ref` argument controls which row serves as the comparison baseline for differences/colors:
- `"auto"`: defaults to `"first"` when OR requested, `"tot"` otherwise
- `"tot"`: total row is the reference
- `"first"`: first non-total row
- integer: specific row index
- regex string: matched against row labels
- `comp="tab"` compares within each subtable; `comp="all"` compares against the total table

Note: `ref` is **reinterpreted by `pct`** — a reference **row** under `pct="row"`/means, a reference **column** under `pct="col"`. 2.0.0 makes `ref` a per-row_var named vector (row%/means only) and stores each cell's own base as `tot_n` — see decisions doc §2, §4.

### Color System (3-layer)

1. **Palettes** (`tab_classes.R` ~L2892): 6 named color vectors (dark/light text, 24-bit blue-red/green-red, dark/light background), each with 11 hex codes: `pos1`-`pos5` (over-represented), `neg1`-`neg5` (under-represented), `ratio`. Hues are hand-tuned so intensity levels are eye-distinguishable on real tables; 8-bit variants target non-truecolor terminals; the 24-bit blue-red variant is more colorblind-friendly than green-red (fuller colorblind support is a future goal).
2. **Breaks** (`set_color_breaks()` in `tab_classes.R`): stored in `options("tabxplor.color_breaks")`. Default pct: `c(0.05, 0.1, 0.2, 2, 0.3)` — the `2` means "twice the reference" (ratio mode). Mirrored for negative. Mean breaks: `c(1.15, 1.5, 2, 4)` — always ratios. *(2.0.0 §18 adds `mean_diff_breaks` `c(0.2, 0.5, 0.8, 1.2)` — sd-standardized differences for the numeric diff mode, Phase 5.)*
3. **Selection** (the Phase-5 `findInterval` engine in `fmt_class.R`: `fmt_color_plan` → `fmt_color_slots` → `fmt_color_channels`/`fmt_channel_codes`, the shared artifact every backend consumes; the old `fmt_color_selection`/`keep_last_break` are gone): per-side fold + `findInterval` over the break scale picks the strongest matching threshold per cell. The 4 measures (`diff`/`ratio`/`or`/`contrib`) each carry their engine facts (raw getter, scale keys, `sig_source`, `gate_row`) in the ONE `MEASURES` fact table (Phase 17d — it now drives BOTH `fmt_color_plan` and the legend; the per-measure switch arms are gone, only the diff↔ratio bound rescale + guaranteed-effect offset stay as policy code). Phase 18z4: `MEASURES` is read ONLY through **`measure_facts(measure, policy)`** (1 plan + 5 legend sites), which folds in a row's optional **`guar`** override — `contrib` alone has one, being the one measure whose reading changes with the policy: the relative contribution under `ignore`/`grey_non_signif`, the ABSOLUTE adjusted standardized residual on the 7th break scale `residual` under `guaranteed_effect`. The legacy combined strings (`diff_ci`/`after_ci`/`ci`) are decoded ONCE at the boundary (`color_decode_legacy`) into a clean `(measure, color_signif)` pair — the stored `color` attribute is always a clean measure and the engine never re-parses; `color_measure_policy`/`single0` are deleted (`"ci"` == `after_ci` now).

### dplyr Integration

The `dplyr_row_slice()` / `dplyr_col_modify()` / `dplyr_reconstruct()` trio in `tab_classes.R` is the core mechanism preserving `tabxplor_grouped_tab` class through dplyr operations. When the table has only one grouping level left, `lv1_group_vars()` detects this and downgrades to plain `tabxplor_tab`. Every new dplyr verb needs a corresponding S3 method — check `NAMESPACE` for the full list.

### Deprecation and retro-compatibility

#### For main user-facing functions and arguments
- This package have a small but existing users base : **soft deprecate main user-facing functions and arguments carefully** to ensure retro-compatibility.
- Some user code rely on `tabxplor_fmt` vctrs fields extracted with `$` or calculated with `mutate()` method for `tabxplor_fmt` (see readme), so **the vctrs fields should not break**.

#### For internal code and internal functions
- **Do not hesitate to propose radical redesign of internal code and internal workflows** for quality, simplicity, structure, performance and future-proofing, specially when they are too convoluted or have grown organically.
- **Always try to simplify, integrate and create smart shared subfunctions** instead of adding a new layer of confusion and ad-hoc solutions inside the code: your main aim is to simplify, to remove traces of old implementations altogether when they have become useless, to clarify, to help me make relevant architectural choices instead of piling up ad-hoc solutions, to integrate the new features in the current code seamlessly.

---

## Key Dependency APIs to read up on

Before working on the `tabxplor_fmt` type system, arithmetic, or display, fetch the help pages for these via the `r-btw` MCP **docs** tools (or `?`) — the model's recall of their exact current contracts is the weakest link:

- `vctrs::new_rcrd`, `vctrs::field` — record type and per-cell field access
- `vctrs::vec_arith`, `vctrs::vec_cast`, `vctrs::vec_ptype2` — arithmetic and casting S3 contracts
- `pillar::pillar_shaft` — console display method
- `data.table` reference semantics (`:=`, `.SD`, `.N`) — internal aggregation
- `DescTools::BinomCI`, `DescTools::BinomDiffCI` — **now Suggests-only** (test parity only). Since Phase 3a the CI math is the closed-form engine in `R/tab-agg.R` (`ci_pivot`/`ci_wilson`/`ci_newcombe`); read it, not DescTools, before touching CI.

---

## Testing

### How to run the suite (the ONLY sanctioned recipe — 2026-07-16)

```bash
# In a temp .R file (outside tests/), then run it EXACTLY like this, unsandboxed:
#   OMP_NUM_THREADS=1 Rscript that_file.R
# The .R file:  Sys.setenv(TESTTHAT_CPUS = "8", NOT_CRAN = "true"); devtools::test("~/github/tabxplor")
```

✅ **Since 2026-07-23 the suite SELF-PINS its threads** — `tests/testthat/setup.R` pins data.table
(`setDTthreads(1L)`) AND BLAS/OpenMP (`RhpcBLASctl::blas_set_num_threads(1L)`, Suggests-guarded — the
runtime call is the ONLY thing that can pin an already-running worker, since OpenBLAS-pthread fixes its
count from the env at process startup), and `tests/testthat.R` sets `OMP_NUM_THREADS=1` + a non-CRAN
`TESTTHAT_CPUS` fallback before workers spawn. So `devtools::test()`, `devtools::check()`, GH Actions
and CRAN all get 1 thread/worker with no manual env. Keep the `OMP_NUM_THREADS=1` prefix anyway
(harmless belt for grandchild processes and RhpcBLASctl-less setups).

⚠ **The trap this guards against** (root-caused 2026-07-16, second session lost to it; hit again by
`devtools::check()` 2026-07-23 before the self-pin): `Config/testthat/parallel: true` runs each test
file in its own PROCESS, and **each process then multi-threads on its own**:

| thread source                                | per worker | x 8 workers | lever                                              |
|----------------------------------------------|------------|-------------|----------------------------------------------------|
| data.table (defaults to 50 % of cores)       | 6          | 48          | `setDTthreads(1L)` — in `tests/testthat/setup.R`   |
| OpenBLAS *pthread* build (`lm`/`glm`/ggplot) | ~10        | ~80         | `RhpcBLASctl` pin in setup.R + `OMP_NUM_THREADS=1` |

**Measured: 165 threads on 12 cores (~14x oversubscribed) -> the suite ran >26 min instead of ~50 s**,
two workers pegged at ~485 % CPU while the rest starved and the log went silent for 10 min. With both
levers: **47 threads, 48.9 s, FAIL 0.**

**Never run anything else while the suite runs.** A single `Rscript` repro uses ~4 cores here; racing
it against 8 workers is what turns "slow" into "apparently hung". Iterate with `filter =`, and run the
full suite once, alone.

⛔ **Before blaming the code for a slow run, check whether YOU are the cause** — this is the companion
to the orphan rule below. In order: (1) is another R of mine running? (2) `ps -eLo pid,args | grep -c
"[-]-no-readline --slave"` — is the THREAD count >> 12? (3) only then look for orphans. A worker at
485 % CPU is oversubscription, not a hang.

```r
# One/few files while iterating (cheap, safe to repeat):
devtools::test("~/github/tabxplor", filter = "tab")  # regex on test-<name>.R
```

⚠ **A green local suite does NOT mean a green CI — this box is `fr_FR.UTF-8`.** GNU gettext ignores
`LANGUAGE` entirely when `LC_MESSAGES` is `C`/`POSIX`, which is the state under `R CMD check` on
Linux (check.R forces `LANGUAGE=en`, and testthat's `local_reproducible_output()` pins `LANG`/
`LANGUAGE` to `"C"` per block) **and on the CRAN farm**. So every French assertion passes here and
fails there. That is why French output is guarded by `skip_if_no_gettext()`
(`tests/testthat/helper-i18n.R`) and why each i18n feature is tested twice — an UNGUARDED English
block (the guard-rail that keeps the goldens from moving; must run everywhere) plus a GUARDED French
one.

**Never simulate CI, even before committing something really locale-touching and heavily do translation: only do it when the user call for it, when you explicitely now the user will push (not for every commit), at release, etc.**

```bash
LC_ALL=C.UTF-8 LANGUAGE=en OMP_NUM_THREADS=1 Rscript <runner>.R   # the CI locale
```

Use `C.UTF-8`, not `C`: plain `C` is *harsher* than any CI runner (non-UTF-8 native encoding), and
makes `test-non-ascii.R`'s own fixtures fail for reasons no CI job will ever hit.

⚠ **Two test/tooling steps need `dangerouslyDisableSandbox` here — root-caused 2026-07-16 from the bwrap
command line, do not re-diagnose:**

- **`test-parallel-parity.R` fails sandboxed** (`fail=1 err=7`, ~0.7 s) with
  `nanonext::.dispatcher_start: 16 | Permission denied`. Cause: bwrap runs **`--unshare-net`**, and
  mirai's dispatcher needs sockets. **Not a regression** — it passes 11/11 unsandboxed. Any full-suite
  run inside the sandbox reports these 8 as failures; ignore them or run that file unsandboxed.
- **`devtools::document()` fails sandboxed** with *"cannot open file 'NAMESPACE': Read-only file
  system"*. Cause: bwrap `--ro-bind`s `NAMESPACE` and `man/` specifically (the rest of the repo is
  writable, which is why snapshot writes succeed). Run it unsandboxed.

⚠ Dev now runs **inside WSL2 Ubuntu 26.04** (`~/github/tabxplor` on ext4), not Windows. The old `d:/Statistiques/github/tabxplor` paths are dead — the Windows checkout survives **build-only** for Windows `.jmo` (see *Jamovi module development*). The `~46s` / `225s -> 56s` suite timings recorded here were measured on Windows/NTFS and have **not** been re-measured on ext4 — treat them as order-of-magnitude only.

**Measured on ext4 / WSL2, 2026-07-16 (per-file, serial): total `359 s`, 2357 passing; slowest
`test-tab_reg.R` `33.6 s`, then `counts-parity` / `calculations` / `color-legend` ~23-25 s, most files
1-13 s.** Under `Config/testthat/parallel: true` the wall clock is roughly the SLOWEST FILE, so the
recorded `56 s` is consistent and still right. **A multi-minute run means something else is wrong — look
for orphans (below) before blaming the code.** Pass `TESTTHAT_CPUS=8`: `parallel: true` alone picks only
~2 processes here.

⛔ **NEVER kill a test run by killing its parent — you orphan the workers, and they do NOT stop.**
Measured 2026-07-16: two `TaskStop`'d suites left 6 R processes (2 `--file=…` parents + 4
`--no-readline --slave` testthat/mirai workers) alive for **52 minutes at ~860 % CPU** (one had burned
174 min of CPU time). They silently starve every later run — a suite that "takes 15 minutes" is usually
this, not the code. Symptoms + rules:

- **Diagnose AND kill unsandboxed — bwrap runs `--unshare-pid --proc /proc`**, so each Bash tool call
  gets its OWN PID namespace (`ps` shows the shell as PID 1). Two consequences: `ps aux` **cannot see
  the orphans**, and a *sandboxed* `kill <host-pid>` cannot kill them — worse, a low PID like `34`
  usually DOES exist inside the namespace, so it would kill **the wrong process**. Both `ps` and `kill`
  must run unsandboxed. Identify yours by the parent's
  `--file=/tmp/claude-…/<session-id>/scratchpad/…` — never by name alone (Positron runs its own R, and
  killing that is destructive).
- **Never `pkill -f <pattern>`.** Measured: `pkill -f testthat` matched and killed the calling shell,
  and `pkill -f t9.R` is what orphaned the workers (parent SIGKILLed -> exit 137, children reparented
  and kept running). Read `ps` first, then `kill` explicit PIDs.
- **Prefer not to create them**: run the suite in the foreground with a long timeout, or
  `filter =` to the files you touched. `setsid nohup … &` is ALSO killed when the tool's shell exits.
- **Never pipe a long run through `tail`/`head`** — they buffer until EOF, so the output file stays
  empty and the run looks hung. Write the incremental log to a file and read that.
- ⚠ Killing PIDs needs the maintainer: the auto-mode classifier denies it (rightly — this is a shared
  dev box). Surface the `ps` evidence and hand over the exact `kill -9 <pids>`.

**Test files:**

| File                      | Coverage                                                                                        |
|---------------------------|-------------------------------------------------------------------------------------------------|
| `test-fmt_class.R`        | fmt creation, printing, type conversion, c(), arithmetic                                        |
| `test-tab.R`              | Core: plain tables, pct, totals, NA, CI, chi2, references, wrapping                             |
| `test-tab_classes.R`      | Class preservation through dplyr verbs                                                          |
| `test-tab_xl.R`           | Basic Excel export                                                                              |
| `test-tab_reg-binomial.R` | Binary outcomes: OR/CI/p parity vs glm/svyglm, 1/OR (was test-tab_logit.R)                      |
| `test-tab_reg.R`          | Phase 12c/12d/12e: beta/OR/IRR/MNL/ordinal + AME parity vs lm/glm/multinom/polr/marginaleffects |
| `test-tab_reg-display.R`  | Phase 12h: estimate_display (est_ci bracket / prob / ame folds), Excel test label, split footer |
| `test-tab_reg-plots.R`    | Phase 12h / z15: reg_check_plots() smoke tests (build a gtable without error)                   |
| `test-tab-estimates.R`    | Phase 18z17: the estimate model + fmt_scale_of() -- no graphics device                          |
| `test-forest-plot.R`      | Phase 18z17: forest_plot() -- ladder == gridlines, cell colour == point, gap band == test       |
| `test-reg-shape.R`        | Phase 18z15: `shape =`, the plot primitives, the stored curves and the row sparkline            |

---

## Jamovi module development

tabxplor currently use jamovi `2.6.44.0` (solid). Version 2.0.0 will also be tested on jamovi current "solid" version `2.7.37` afterwards (Phase 7i confirmed 2.7.37 ✓).

✅ **jamovi IS installed on BOTH dev machines** — flatpak `org.jamovi.jamovi` **2.7.36**, bundled R **4.5.0**: the desktop WSL2 (migration Phase C3, 2026-07-16) and the **laptop WSL2 (Ubuntu 26.04, 2026-08-13)**. Launch it with **`jamovi`** (the `~/.local/bin/jamovi` wrapper — never bare `flatpak run`, see below). The module builds with `jmvtools::install(home = "flatpak")` in ~2 min (~33 s once jamovi's R has the dep tree), and Crosstables is verified running on real data.

⚠ **2.7.36 is PINNED and MASKED, and that is now load-bearing.** Flathub has moved jamovi to a **new version scheme**: the current stable is **28.x** (28.2 as of 2026-08-13) and only **one** 2.7.x commit is still retained — `56eb8de3d468e093ac25cf0bb6236c51e0828fb1b5e8e5bce7b3df110cf49240` = 2.7.36 (2026-06-28); 2.7.32 is the other. `flatpak install` has **no `--commit` option** (only `flatpak update` does), so the recipe is install → downgrade → mask:

```bash
flatpak --user install -y flathub org.jamovi.jamovi
flatpak --user update  -y --commit=56eb8de3d468e093ac25cf0bb6236c51e0828fb1b5e8e5bce7b3df110cf49240 org.jamovi.jamovi
flatpak --user mask org.jamovi.jamovi     # else a routine `flatpak update` silently jumps to 28.x
```

The mask matters because 2.7.36 is the "solid" teaching target **and** because a 28.x jamovi would pair with a newer `jmvtools` whose compiler can emit a `jms` that 2.7.36 refuses (next note). Verify by mechanism, never by the version field — `flatpak info` reports a stale appstream `Version: 2.7.27`; `jamovi --version` reports the truth (`2.7.36.0`), and `--r-version` must equal the module's `rVersion` (`4.5.0-x64`).

✅ **The six "OPEN — maintainer step: regenerate `jmvtab.h.R`" items (Phases 7a, 7e, 7g-i, 7g-ii, 7g-iii, 7h) are CLOSED** — one `jmvtools::prepare()` covered all of them, and the compiled **`uijs` blob** means those UI changes are live in a running app for the first time.

✅ **A second `prepare()` ran on 2026-08-13** (as part of `jmvtools::install()` on the laptop) and closed every `.h.R` item accumulated since — z13's `jmvtabreg.a.yaml` (`na`'s three values), z16's `jmvtab.a.yaml` (`test_robust` → the `design_effect` checkbox) and z16-iiiii's (`method_ratio` removed). **Measured against HEAD**: `design_effect` went **0 → 11** occurrences in `jmvtab.h.R` (the checkbox was declared in the YAML but absent from the stale `.h.R`, so `isTRUE(NULL)` made it **inert** — every claim in its help text was untrue in the running module), and the dead options went to zero (`test_robust` 10→0, `method_ratio` 10→0, `na = "drop_all_models"` 1→0, `ids` 13→0, `strata` 13→0, `fpc` 12→0). `inst/i18n/fr.json` is regenerated from `jamovi/i18n/fr.po` at the same time: translated strings **72 → 159**; the ~21 that disappear are stale msgids for labels renamed across phases (`chi2 = <i>(Chi2 test)</i>`, `after_ci <i>(…)</i>`), and most of the 44 still untranslated are argument **values** (`all`, `auto`, `ci`, `at`) that stay English on purpose.

⚠ **`prepare()` proved the hand-edited `.h.R` had a latent bug**, so do not hand-edit it again. `R/jmvtab.h.R` was hand-mirrored to the YAML across ~7 commits; the compiler reproduced 778 of its 780 lines but corrected `exportExcel` (`type: Action`) from `NULL` → `FALSE` **and gave it a default it lacked** — without which `tabxplor::jmvtab()` called from R throws. The never-edit rule earned its keep.

⚠⚠ **`ELECTRON_RUN_AS_NODE` — do not debug jamovi without knowing this.** Claude Code/Positron export `ELECTRON_RUN_AS_NODE=1`; flatpak passes it into the sandbox and jamovi's Electron runs as **plain node** → **exit 0, no window, no error**, and `jmvtools::install()` dies `"bad option: --install"` (rc=9). `flatpak run --unset-env=` is NOT enough (zypak re-spawns children via the host); only `env -u` on the host works — which is what the `jamovi` wrapper does. In R: `Sys.unsetenv("ELECTRON_RUN_AS_NODE")` before `jmvtools::install()`. ⚠ `jmvtools::check()` passes regardless — it never reaches Electron — so a green `check()` proves nothing here.

⚠⚠ **`R_LIBS_USER` in `~/.Renviron` — the second environment trap, found on the laptop 2026-08-13.** jamovi's flatpak bundles **its own R** (4.5.0 for 2.7.36) and, having `filesystems=home`, it **reads your `~/.Renviron`**. A hard-coded library path there —

```sh
R_LIBS_USER=~/R/x86_64-pc-linux-gnu-library/4.6      # WRONG: pins one R version
```

— puts your **system R 4.6** packages on jamovi's R 4.5.0 `.libPaths()`, and `jmvtools::install(home = "flatpak")` dies at lazy-load with `data_table.so: undefined symbol: R_duplicateAsResizable` (a 4.6 symbol absent from 4.5.0). The assignment is **unconditional**, so **no env var passed to the child can override it** — `withr::with_envvar()` does not help; the file itself must be version-generic:

```sh
R_LIBS_USER=~/R/%p-library/%v      # R's own default: %p = platform, %v = major.minor
```

Same resolved path for system R (verify with `.libPaths()` before/after), while jamovi's R falls back to its bundled `/app/lib/R/library`. Diagnose in one line: `flatpak run --devel --command=sh org.jamovi.jamovi -c '/app/bin/R --vanilla --no-echo -e ".libPaths()"'` — anything outside `/app` is contamination. ⚠ This bites on **any** second R version, not just jamovi's.

⚠ **WSLg is in COPY MODE** (known WSL 2.7.x bug [microsoft/WSL#40618](https://github.com/microsoft/WSL/issues/40618)): windows can be slow or render blank (taskbar entry + penguin icon, `[WARN:COPY MODE]` in the title). **Not a jamovi problem** — plain `xmessage` fails identically. One-time fix, persists across reboots: `sudo mkdir -p /mnt/shared_memory && sudo mount -t tmpfs tmpfs /mnt/shared_memory`. ⚠ The bug is *unstable* — it sometimes renders fine without the mount, then regresses; a working window is not evidence the mount is unneeded.

⚠ **There are now TWO build paths, and they are not interchangeable — `.jmo` bundles are platform-specific** (migration Phase A1):

| Target                               | jamovi                                                  | Checkout                                                                    | Recipe                                                                                                                                                         |
|--------------------------------------|---------------------------------------------------------|-----------------------------------------------------------------------------|----------------------------------------------------------------------------------------------------------------------------------------------------------------|
| **Linux `.jmo`** (WSL, the dev path) | flatpak `org.jamovi.jamovi` **2.7.36 ✅ installed (C3)** | `~/github/tabxplor` — **authoritative for source**                          | `jmvtools::install(home = 'flatpak')` (setup doc §7.4; the SDK `org.freedesktop.Sdk//24.08` is REQUIRED — `flatpak run --devel` is how the compiler reaches R) |
| **Windows `.jmo`** (release only)    | Windows jamovi, **kept forever**                        | `D:\Statistiques\github\tabxplor` — **build-only: pull, build, never edit** | `options(jamovi_home='C:/Program Files/jamovi 2.6.44.0'); devtools::load_all(); jmvtools::install(); devtools::load_all()`                                     |

**A Linux jamovi cannot produce a Windows bundle**, so the Windows checkout survives *even if C3 fully succeeds* — this is not a C3-failure fallback. The rule that matters: **never edit tabxplor in both places.** Edit in WSL, pull on Windows, build there.

✅ **`jmvtools` is pinned to 2.7.26** (C3). ⚠ Never `install.packages("jmvtools", repos="https://repo.jamovi.org")` — that index serves 2.7.26 **and** 28.0-28.3, so R takes **28.3**, whose newer compiler can emit a `jms` version 2.7.36 refuses. Reinstall with the explicit tarball: `install.packages("https://repo.jamovi.org/src/contrib/jmvtools_2.7.26.tar.gz", repos = NULL, type = "source")` (install `node` from that repo first — `repos = NULL` resolves no deps).

⛔ **The 2.6.44 flatpak is GONE** (C3): Flathub retains only ~5 commits; 2.6.44 was built 2025-03-06 and is long pruned. **2.6-solid compatibility is verified on Windows only** — via the build-only Windows checkout, which is kept forever regardless. ⚠ **The retention window is now the constraint on 2.7.x itself**: as of 2026-08-13 the log holds 28.2 / 28.1 / 28 / **2.7.36** / 2.7.32, so 2.7.36 is *two commits from being pruned*. Once it goes, a fresh machine can no longer install it from Flathub — check `flatpak remote-info --log flathub org.jamovi.jamovi` before assuming a reinstall is possible, and keep the installed copy masked.

To know the real structure of the final .html and .js, check at this live capture done from dev console (for a basic table) :
- `dev/jamovi/dev_console_live_capture/Jamovi_tabxplor_1_3_1_basic_table.html` : the live html from tabxplor 1.3.1 jamovi module
- `dev/jamovi/dev_console_live_capture/127.0.0.1_56680_MAIN_ELECTRON/` : the exported main election scripts
- `dev/jamovi/dev_console_live_capture/127.0.0.1_56683_tabxplor_jmvtab_analysis_UI/` : the exported tabxplor jmvtab analysis UI scripts
- `dev/jamovi/dev_console_live_capture/127.0.0.1_56684_results/` : the exported jamovi "results" panel scripts (where the actual table appears)

To **capture new html** in the dev console, **ask the maintainer whenever you need**.

Look at `dev/tabxplor_2.0.0_jamovi_dev.md` and `@dev/jamovi/` for detailed informations.


---

## Common tabxplor package Development Issues

| Issue                                   | Solution                                                                              |
|-----------------------------------------|---------------------------------------------------------------------------------------|
| R CMD check NOTE about global variables | Add to `globalVariables()` call in `fmt_class.R` (for data.table's `:=`, `.SD`, `.N`) |
| magrittr `%>%` vs base R pipe           | Prefer base R pipe for new code, examples, etc. Package re-exports `%>%` for users.   |
| New vctrs type combination doesn't work | Need both `vec_ptype2.*` and `vec_cast.*` S3 methods for every type pair              |
| dplyr verb silently drops class         | Missing S3 method for `tabxplor_grouped_tab` — add one in `tab_classes.R`             |

---

## Architecture Technical Guide

For the full detailed technical reference, see `dev/tabxplor_architecture.md`, which documents every subsystem in depth. Read it whenever needed and keep it up-to-date.

---

## tabxplor github repo

Branches :
- `dev` is the branch where development happens
- `release/<version number>` is the version stripped of dev only files
- `master` is the public user-facing branch

Commits :
- **The maintainer makes the commits.** Do not commit unless explicitly asked.
- **Never add a `Co-Authored-By` trailer** (nor any "Generated with …" line) to a commit message.
  This overrides the default. The maintainer authors and signs every commit and is solely
  responsible for it; the assistant does not co-sign.
- The release procedure is `dev/release_checklist.md` — read it before touching a release branch.

---







## tabxplor version 2.0.0 roadmap : the current goal

Currently implementing tabxplor 2.0.0 (2.0.0 only if breaking changes land). **Update the sections below at the end of every work session.**

Phases already implemented can be found in `dev/tabxplor_2.0.0_roadmap_DONE_PHASES.md` Only phases not yet finished appear below.

### The aim of 2.0.0 — read first, it governs every decision

This version exists to **refactor and simplify `tab()`/`tab_many()`** — the two functions that matter — by **stripping the white-elephant flexibility that real-world data analysis never uses**, and **redesigning the underlying `tabxplor_fmt` vctrs-field architecture** (one combined field pass) to fit the simpler, faster model. The governing rule, non-negotiable:

- **Public API stays retro-compatible.** User-facing functions, their arguments (soft-deprecate, never hard-break), and the `tabxplor_fmt` fields users read with `$`/`mutate()` keep working.
- **Internals are redesigned as radically as needed** for consistency, simplicity, and performance. Do **not** preserve internal structure, dead code, or the old step-by-step (`tab_pct`→`tab_ci`→…) paths for their own sake — remove them, fuse them, route everything through the one aggregate-core. Whenever a choice trades never-used internal flexibility for a single well-defined faster path, take it.

Every phase and decision below serves that aim: fewer knobs, one computation core, a field set shaped to the real use cases.

### Start here

This roadmap is the **plan of plans**: the phased implementation order plus every open question. A fresh session asked for a *part* of the work should read, in order:

1. **This roadmap** — the phase your task belongs to, its bullets, and its pointers
2. **`dev/tabxplor_2.0.0_roadmap_DONE_PHASES.md`** – the detailed report of all the **already implemented phases of the roadmap**.  
3. **`dev/tabxplor_architecture.md`** — architecture guide (type system, pipeline, compaction loss, exporters). It describes the **current** architecture. Read the section matching the file you touch.
4. **Top of this CLAUDE.md** — Repository Map, Global Architecture, Key Constraints, Design Decisions.

**Other long-form 2.0.0 docs live in `dev/` (all `.Rbuildignore`'d), never inline here — read the matching ones before you start:**
- `dev/benchmarks/` — performance harness + saved results (documented under *Reference > Benchmarks*). Read/run when a phase touches perf (Phases 2, 3, 6, 8).
- `dev/benchmarks/tab_many_performance_profile.md` — the full 2026-07 profile. Read before optimizing `tab_many` / `tab_chi2` / `tab_num`.



#### Verification (every phase)

- **Byte-identity**: `devtools::test("~/github/tabxplor")` after each phase; `test-golden.R` + `test-export-parity.R` + `test-fmt-contract.R` + `test-fuse-parity.R` stay green. Intentional output changes → rerun `dev/make_golden.R`, review the `_golden/`/`_snaps/` diff consciously, `testthat::snapshot_accept()`.
- **Release gate**: `devtools::check()` (~3 min, run manually by maintainer) before CRAN.

---



### Phase 20 — the surface: how the package is asked, and what it exposes

**The plan of plans is `dev/tabxplor_phase20_surface_integration.md`** — goals, design and
architecture decisions, then the seven phases in full. **Read it at the start of every Phase 20
session**, together with the two assessments it is built on (`dev/tabxplor_phase19_assessment.md`
= 19o, `dev/tabxplor_phase19p_api_review.md` = 19p). The section below is the big picture only, so
it can never be lost.

---

#### The mission — read this first, it governs every phase

Phase 19 gave the package a complete, explicit data model and ~15 declared fact tables. It also
grew `R/` by 11.9 % and never touched what makes the package feel large to a user:

> **Phase 19 unified how facts are STORED and how rules are DECLARED. It did not unify how the
> package is ASKED** — and every remaining duplication in the public surface is one shape:
> **a fact is declared once in an R table, and re-typed by hand in the place a user meets it**
> (a formal, a `@param` block, an option name, an accessor).

The package has already solved that four times (`fmt_fields_rd`, `display_tokens_rd` ×2,
`reg_measures_rd`), each because the hand-written copy had drifted. **Phase 20 applies the solution
to the surface.** After it:

- an **argument** is declared once — producers, legal values, option twin, one-line gloss — and the
  signature, the reference page and the jamovi vocabulary all read that declaration;
- a **producer variant** stops re-declaring the entry point's arguments (**83 of the 149 crosstab
  formals are the same argument written a 2nd–4th time**);
- the **two producers ask the same question with the same word**, everywhere they ask the same one;
- an **exported name** exists because a user story reads it, not because a fact became stored;
- a **fact table's cross-references are checked at load**, not remembered in a comment;
- `tab_reg()` gains a staged build and stops paying for a variance it already computes
  (**15.3 s → ~2 s**, measured).

**Not a feature phase either**, with two deliberate exceptions: the jamovi level-collapse UI (20g)
and — only if the measurement justifies it — `tab_reg()` parallelisation (20f).

**The hard rules are Phase 19's nine, unchanged**: simplify and integrate, never add another ad hoc
layer · never guess what something is · one resolver, taken to completion · **facts live in ONE
table — and Phase 20 extends that to documentation: a value list written in roxygen beside the
table that declares it is the same offence one level up** · never leave a representation
half-migrated · internals redesigned as radically as needed (`tab_reg()`'s back-compat waived,
`tab()`'s CRAN-released surface shimmed) · a claimed fix ships with the fixture that fails without
it · golden discipline · the phase **"DONE" summary goes in CLAUDE.md and ONLY there**.

⚠ **"Released" means CRAN 1.3.1** (commit `86320287`, **63 exports**) — not the only git tag
`v1.2.0` (59), not the dev head. Check every removal against it.

⚠ **Do not count lines as the simplification metric.** Phase 19 grew 11.9 % and got substantially
better; Phase 20 will grow `R/` too (`TAB_ARGS`, `TEST_ROWS`, `outcome_level`, three generators, two
harnesses). The metrics that track reality: *formals per producer · duplicated `@param` blocks ·
`man/` lines · exports with zero external callers · cross-table keys unchecked · aborts vs informs*.
`man/` is the one surface that shrinks unambiguously: **8 930 → ~7 300**.

⚠ **Differentiator 4 is the one at risk**: the jamovi UI shows R argument names *on purpose*, so
every rename must reach `jamovi/*.a.yaml` in 20g or the teaching path starts lying.
`test-jamovi-vocabulary.R` goes red on any renamed value and stays red until then — that is the
gate working, and it is why 20f cannot be skipped.

---

#### The nine keys

19o and 19p each lettered their own (α–η and A–D); two schemes for one body of work is the disease
this phase cures, so they are stated as one set.

| key       | the missing fact / unstated rule                                                                                                                                           | lands in  |
|-----------|----------------------------------------------------------------------------------------------------------------------------------------------------------------------------|-----------|
| **KEY 1** | *which producer takes which argument, what it means, what it may be, which option is its default* → `TAB_ARGS`, generated `@param`s and value lists, `...` on the variants | **20b**   |
| **KEY 2** | *a key written in one declared table and read in another is a foreign key* → ~14 checks, at load time                                                                      | **20a**   |
| **KEY 3** | *which accessors exist* → one generic `fmt_attr()` pair + a measured keep-list + `tab_columns()`                                                                           | **20a**   |
| **KEY 4** | *if two producers ask the same question, they ask it with the same word* → `tab_vars`, `ref`, `ci_method`, `footer`                                                        | **20c**   |
| **KEY 5** | *what kind of statistical row this is* → `TEST_ROWS`; the crosstab half of the footer finally declared                                                                     | **20c**   |
| **KEY 6** | *which stage of a regression build produced which part of the table* → `new_reg_ctx()` + five named stages                                                                 | **20e**   |
| **KEY 7** | *which estimands tabxplor can differentiate analytically* → a declared `se` column; the AME stops being computed twice                                                     | **20d**   |
| **KEY 8** | *the export surface re-declares seven arguments five times* → `TAB_ARGS` covers the exporters too — **not** a `tab_style()` bundle                                         | **20b**   |
| **KEY 9** | *a package whose whole value is a data model states it in one place* → `?tabxplor-model`                                                                                   | Phase 22b |

**KEY 1 is this phase's keystone**, as KEY 5 was of Phase 19. Everything else is a prerequisite for
it (2, 3), a second instance of it in another subsystem (4, 5) — or, in KEY 8's case, **the same
instance**: the export surface's duplication turned out to be KEY 1's, one subsystem further out.
Only 6 and 7 are independent of it.

---

#### Settled decisions — do not re-open

Thirty rulings; full table + rationale in the plan of plans §4. The ones that change what
gets built:

| decision                                                                                                                                                                                                                                     | ruling                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  |
|----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| **both proposed bundles**                                                                                                                                                                                                                    | **REJECTED.** No `tab_inference()`, no `tab_style()`. `ci_method` / `design_effect` / `anova` stay flat with their option twins (the only change is `tab_reg(method =)` → **`ci_method`** with a declared `model` slot); the exporters keep every formal. *A bundle must make the common call shorter, not only the signature* — the general test for every future one. And the lesson generalises: **a mirrored formal is not automatically a problem** — 7 defaulted arguments × 5 exporters cost a user nothing, the duplication was 35 hand-written `@param` blocks, which is KEY 1 |
| `tab()`'s 9 deprecated formals                                                                                                                                                                                                               | **into `...`**, caught by name, with an **abort on an unnamed 6th argument**                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            |
| the legacy step API                                                                                                                                                                                                                          | **hard-deprecate now**, defunct in 2.1.0 — the exported *chaining API*, never the computations (those moved into the leaf in 19j). ⚠ removes nothing this cycle: `tab-steps-legacy.R`'s 1 433 lines stay                                                                                                                                                                                                                                                                                                                                                                                |
| `tab_many()`                                                                                                                                                                                                                                 | **stays soft-deprecated**; only its 448-line `.Rd` is fixed (`@inheritDotParams` → plain `@param`, −390)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                |
| `tab_logit()` / `multi_logit()`                                                                                                                                                                                                              | **deleted** (genuinely unreleased) — ⚠ **59 test call sites** to migrate, not "nothing references them"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 |
| `tab_reg()` renames                                                                                                                                                                                                                          | `split_var` → **`tab_vars`** · `dependent` → **`outcome`** (package-wide) · `reference` → **`ref`** (`c(var = "level")`, **predictors only**) · `method` → **`ci_method`** · `stats`+`compare`+`baseline` → **`footer`** · `.fit_cache` → `...`                                                                                                                                                                                                                                                                                                                                         |
| **`outcome_level`** (new)                                                                                                                                                                                                                    | `inverse_two_level_factors` is **deleted** for `outcome_level = c(outcome = "level")`. ⚠ NOT absorbed into `ref`: **`ref` names the level you compare AGAINST, `outcome_level` the level you MODEL** — opposite roles, so one argument would carry two meanings. binomial → the modelled level (the column header); multinomial → the baseline (taking over what `reference` does today); ordinal → **refused**. Precedent: SAS `PROC LOGISTIC` has exactly this pair, `EVENT=` beside `REF=`                                                                                           |
| `tab(ref / ref2)` · `na`'s two vocabularies · the `color` default asymmetry · `pct = "no"`                                                                                                                                                   | **unchanged** — the last two deliberate; *state* `pct`'s default in `?tab` rather than change it                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        |
| `TEST_ROWS` · reg parallelisation · the jamovi level-collapse UI                                                                                                                                                                             | **all inside Phase 20**, pre-release — parallelisation as its own phase (20f), gated on 20d and 20e and free to conclude "no"                                                                                                                                                                                                                                                                                                                                                                                                                                                           |
| accessors                                                                                                                                                                                                                                    | **generic mostly, a few named ones kept — the most used.** The keep-list is measured at plan time and must include `get_col_var()` and `set_row_kind()`                                                                                                                                                                                                                                                                                                                                                                                                                                 |
| `new_lvl()`/`is_lvl()` **stay exported** (a user meets the class) · `tab_prepare()` + `complete_partial_totals()` **off the public surface** · `tab_get_wrapped_dimensions()` → `@keywords internal` · `tabxplor.color_style_type` deleted   | ⚠ `tab_prepare` and `complete_partial_totals` **are CRAN 1.3.1**, so both take the deprecate-now / un-export-in-2.1.0 route — never a silent drop                                                                                                                                                                                                                                                                                                                                                                                                                                       |
| `tabxplor.stars` absorbs `signif_levels`+`signif_labels` **and becomes a per-call ladder** · new `options(tabxplor.total_names = c(row=, col=, tab=, other=))`, **and `total_names` / `totaltab_name` / `other_level` leave the signatures** | ⚠ all three are **CRAN 1.3.1 formals** of `tab()` (and of `tab_many()`), so they go through `...` with a deprecation, not out                                                                                                                                                                                                                                                                                                                                                                                                                                                           |
| `@inheritDotParams`                                                                                                                                                                                                                          | **never** — it *inlines*; `tab_many.Rd` is the 448-line proof                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           |
| `...`                                                                                                                                                                                                                                        | on **wrappers and superseded producers only**; `tab()` and `tab_reg()` keep every live formal                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           |
| the `tab_kable_*` / `xl_font_*` option renames · a JS syntax gate · column-axis `ordered`                                                                                                                                                    | **stay dropped** — do not re-propose                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    |

---

#### Verification discipline

- **Per phase, targeted** (`filter =`) plus the sentinels the phase entry names. **Not the full
  suite after every edit.**
- **Full suite** at three checkpoints: end of **20b**, end of **20e**, end of **20i** — plus one
  **`devtools::check()` at the end of 20i**, so Phase 22 does not inherit a broken tree (19n found
  three `check()`-only failures invisible to the suite). The CI-locale run is the release phase's.
- **Six harnesses.** Four exist (`verify_golden_field_delta.R` · `verify_color_attrs.R` ·
  `verify_reg_specs.R` · `verify_no_ghost_functions.R`); **two are new in 20a and gate 20b/20c**:
  `dev/verify_tab_args.R` (every crosstab producer's *resolved* settings over a call grid) and the
  export-usage census.
- ⚠ Run every census under **`LC_ALL=C`** (fr collation does not group identifiers containing
  `_`/`.`) and **never `grep -w` on a pattern ending in `(`** — both produced a wrong census while
  19p was written.

---

#### The phases

Nine. Each is *plan-then-implement*, starting in plan mode, in its own fresh session. The
maintainer commits between phases and pushes at the end of Phase 20.

**There is deliberately no documentation phase** — Phase 22 below already owns the architecture
document, the vignettes, the roxygen sweep, the comment rewrite, `NEWS.md`, the tests and `dev/`.
§10 of the plan of plans maps every documentation item to its 22a–22g home and the gate set to the
release phase (⚠ flagging one gap: **i18n has no home in 22a–22g** — recommended as 22h). What each
Phase 20 phase still owes is the standing rule 9: update the docstrings you changed, in the phase
that changed them — and after KEY 1 most `@param` blocks are *generated*, so a rename documents
itself.

| phase        | title                                                                       | one line                                                                                                                                                                                                                                                                                                                                                                                                                                                        |
|--------------|-----------------------------------------------------------------------------|-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| **20a**      | The floor: referential integrity, the exposed surface, dead weight          | KEY 2's foreign keys · KEY 3's `fmt_attr()`/`tab_columns()` · the deletions and demotions · `tab_many.Rd` −390 · the two harnesses · the 2 live colour-engine `FIXME`s                                                                                                                                                                                                                                                                                          |
| **20b**      | KEY 1 + KEY 8 — the argument surface as data, producers and exporters alike | `TAB_ARGS` + generated `@param`s and ~15 value lists, **the five exporters included** (part 1, byte-identical) → `...` + `tab_check_dots()`, `tab()` 52 → ~37, the `stars` ladder, `total_names` (part 2)                                                                                                                                                                                                                                                       |
| **20c**      | KEY 4 + KEY 5 — one word per question, and the footer's model               | the `tab_reg()` renames incl. `dependent` → `outcome` and the new `outcome_level` · `footer =` with `TEST_ROWS` as its vocabulary                                                                                                                                                                                                                                                                                                                               |
| **20d**      | KEY 7 — marginal effects, computed once and computed fast                   | 85 % of a 15.3 s call is an avoidable *numerical* jacobian, and tabxplor already owns the analytic SE. Then the research half: can `marginaleffects` leave the hot path entirely? ⚠ **web searches expected**                                                                                                                                                                                                                                                   |
| **20e**      | KEY 6 — `reg_build()` becomes a staged build                                | the package's largest function (534 lines, 7 local closures, 11 unnamed phases) gets `new_reg_ctx()` + five named stages. **Pure refactor**: `verify_reg_specs.R` must print IDENTICAL                                                                                                                                                                                                                                                                          |
| **20f**      | `tab_reg()` parallelisation: measure, then decide                           | ✅ **measured "no"** — the remaining cost was the model-check footer (81–94 % of a call), most of it computed several times and read once. No pool; three de-duplications + a declared `REG_CHECKS$cost` instead (2.6–6.0× on a default call). Study: `dev/tabxplor_reg_performance.md`                                                                                                                                                                          |
| **20f-ii**   | the same question at the MODEL level: measure the three axes                | ✅ **measured** — the three axes are not the same shape. `tab_vars` (G) and outcomes × a models list (R) already return finished tables and are dispatchable today, but clear ≥2× only on an *even* axis at survey scale; the **S** axis (several outcomes in one table · a models list) holds the 2×+ shapes (2.86× at four outcomes) and cannot be dispatched as written. Shipped the crude-block de-duplication + the cross-outcome `compare` guard. Study §6 |
| **20f-iii**  | the S axis: `reg_spec_build()`, and the parallelism it unlocks              | ✅ **done** — the six per-spec loops are ONE declared product (`R/reg-spec-build.R`), and `parallel` becomes a shared argument over all three model axes. ⚠ the message-order price it budgeted for was measured at **zero** reordered cases; what changed instead is 9 abort messages losing purrr's `In index:` wrapper                                                                                                                                        |
| **20f-iiii** | the reg framework: finished, and CLEAN under parallelisation                | ✅ **done** — the crude block is the OUTCOME's (`reg_stage_crude()`), so one refusal is GONE; the other two are declared keeps **with their measurement** (a fit is 162 MB; the deferred skeleton is unreachable from `tab_reg()`). + the worker-error relay, the nesting rule enforced once, and three silent defects in the compound-formula path                                                                                                              |
| **20g-i**    | jamovi: the boundary speaks the 2.0.0 vocabulary                            | ✅ **done** — an option is NAMED after the argument it drives, and the gate checks it (it only checked *values*, so it stayed green through six months of the reg panel showing `dependent` / `split_var` / `method`) · the two owed controls · `prepare()` + `install()` ran HERE, so nothing is left inert                                                                                                                                                     |
| **20g-ii**   | jamovi: the level-collapse UI (★)                                           | ✅ **done** — the tick-box merge widget, in ONE control with the reorder (a run is consecutive *in the chosen order*), one full-width row per axis, merge-only in `jmvtabreg`. Pre-aggregate behind an internal `.levels_collapse` on BOTH producers, so it is provably `tab()` on a pre-collapsed frame; the tier-1/2 keys name it, which is a declared MISS. + the shared-JS COPY mechanism and two gate gaps                                                  |
| **20h**      | Harvest 1: the deletion pass                                                | re-run the censuses, delete what the new declarations made unnecessary, and **report what did not shrink** — that report is the product                                                                                                                                                                                                                                                                                                                         |
| **20i**      | Harvest 2: open integration                                                 | ⚠ creative, own session: what does the finished surface make *possible*? Look and propose first — **ask before building**                                                                                                                                                                                                                                                                                                                                       |

**Dependencies**: 20a first · 20b and 20c need 20a's harnesses · 20d needs 20c · 20e needs 20d ·
20f needs 20d+20e · 20f-ii needs 20f · 20f-iii needs 20f-ii · 20f-iiii needs 20f-iii ·
20g-i needs 20b/20c/20d · 20g-ii needs 20g-i · 20h then 20i last.

⚠ **The `tab_reg()` phases are deliberately separate sessions** — one story, several frames of
mind: 20d is **numerical parity** (research, closed forms, tolerance fixtures), 20e is a **pure
structural refactor** proved by one harness printing IDENTICAL, 20f and 20f-ii are **measurements**
that may conclude "no" (and both did, in part), 20f-iii is a refactor that budgeted for trading
the message-order half of that proof for a declared per-spec product -- and, measured, did not have
to spend it -- and 20f-iiii is three different questions again (a numerical-parity one, a plumbing
fix in a shared file, a census), which is why it too gets its own.
Interleaving them is how a refactor and a numeric change land in one diff and
neither can be verified.

**Mapping from the old draft** (nothing lost): old 20d (jamovi UI) → **20g** · old 20e (the
marginal-effects freeze) → **20d**, root-caused as KEY 7 · old 20f (parallelisation) → **20f**,
unchanged in content but now gated on 20d and 20e.

**At the end of each Phase,** add a `#### Phase 20{x} — <title>` header **here, in CLAUDE.md**, and
write the **"DONE" summary** under it. Write it in **this file and nowhere else**. Update the
Repository Map above in the same pass, yourself.


#### Phase 20a — The floor: referential integrity, the exposed surface, the dead weight

**DONE (2026-08-16), both halves.** Full suite **FAIL 0, WARN 58, SKIP 4, PASS 6626** — and every
one of the 57 deprecation warnings is this phase's own, named below. Exports **93 → 94** (−`tab_logit`, −`multi_logit`, −`tab_md_css`;
+`fmt_attr`, +`fmt_attr<-`, +`tab_columns`, +`set_ref_type`) and **`man/` 8 930 → 7 318 (−18 %)** —
the plan's estimate to within 20 lines. The three harnesses print the declared result:
`dev/verify_color_attrs.R` **IDENTICAL** (293 cases), `dev/verify_tab_args.R` **IDENTICAL** on its
own re-saved baseline, `dev/verify_golden_field_delta.R` **empty declaration set** over 1 788 cells ×
36 goldens. `dev/census_exports.R` reports exactly the −3/+4.

**The two harnesses, built and baselined FIRST — and they earned their keep before the phase's own
work started.**

- **`dev/verify_tab_args.R`** — three captures: `tab_resolve_common_args()` over a 167-case grid ·
  52 built tables' per-column attributes + `meta`/`test` schema · and **the messages, in order**, of
  30 deliberately invalid or legacy calls. That third capture is what 20b needs: moving nine formals
  into `...` changes *what the package says* while every cell stays put.
- **`dev/census_exports.R`** — per export: `released_in` (v1.2.0 / **CRAN 1.3.1** / dev) · callers in
  `R/`, `tests/`, `vignettes/`, `README`, `dev/` · Rd lines · pkgdown section. Both traps are encoded
  in the script rather than remembered: it does every set operation in R (so `LC_ALL` cannot reach
  it) and matches `(^|[^a-zA-Z0-9._])name\(`, never `grep -w`.

**Four defects, all found by the harnesses, all fixed with the fixture that fails without them.**

1. ⚠ **Three deprecations had never fired.** `tab(OR = )`, `tab(ci = "diff"/"ratio")` and
   `method_cell`/`method_diff` warn through `deprecate_soft(user_env = )`, and 19i's move of those
   calls **into the shared boundary added a stack frame**: `caller_env(2)` now lands on `tab()`'s own
   execution env, so lifecycle read a tabxplor frame as "the user" and stayed silent. `user_env` is
   threaded explicitly now. ⚠ **`ci` is said at the boundary and rewritten downstream** — the first
   attempt resolved it there, and `verify_color_attrs.R` caught that `ci = "ratio"` has a *second*
   effect (it pins the Katz ratio scale) which the per-row_var resolvers read off the RAW word. Five
   cases moved `mean_ratio → mean_diff`; the harness is the only thing that would have seen it.
2. **`tab(total_names = )` was broken outright** — *any* non-default value aborted
   (`forcats::fct_recode()` takes its pairs as NAMED `...` arguments, and the call spliced with `!!`
   instead of `!!!`). It is a CRAN 1.3.1 formal whose only untested branch that was, and 20b is about
   to move it onto an option.
3. **`tab_many()`'s unnamed-argument guard aborted with a cli internal error** — cli takes a `{?s}`
   quantity from the *last substitution before it*, so a message opening on `"Argument{?s}"` died
   with *"Cannot pluralize without a quantity"*. The guard refused the call and said nothing
   actionable. `cli::qty()` fixes it; 19h's safety net for positional calls now works.
4. **`REG_EMPIRICAL$*$ci_method` carried `"wald_log"`, outside every declared vocabulary** — found by
   KEY 2 on its first run, together with a wrong edge of mine.

**KEY 2 — the foreign keys (`R/zzz-fact-keys.R`, new).** **`TAB_FOREIGN_KEYS`** declares **34 edges**
(19o listed 12), read only through `tx_check_foreign_keys()`, which runs at **load**: a dangling key
breaks the build at the moment it is made. ⚠ The file **must sort last** — `COLOR_SCALES` lives in
`tab_classes.R` and `REG_EMPIRICAL` in `tab_reg.R`, both *after* `reg-estimand.R`, which is where 19o
proposed putting it; `zzz-` is the only prefix that is last by construction. Its two readers
(`tx_fk_scalar`/`tx_fk_all`) use `[[` exclusively — `MEASURES$adjustment` has `scale_from` and no
`scale`, so `$scale` partial-matches to `"gap"`. Three `allow` entries, each a stated fact
(`DISPLAY_TOKENS$field`'s `"ci"` is DERIVED, not a 22nd record field; `"katz"` / `"woolf"` /
`"wald_log"` are the only interval of their geometry, so none is a `ci_method` a user picks). The two
cross-FILE asserts moved in from `reg-estimand.R`, one of them **strengthened**: "every estimand's
fit has model checks" is stated on `REG_ESTIMANDS$rows$fit` now, not on the three-entry
`REG_FIT_FAMILY` subset it was written against. `tab.R` also gained the `SPINE_OWNED_INPUTS` ↔
`CTX_SETTINGS_LOCALS` assert its own comment had promised since 19i and never had.

**KEY 3 — the accessor surface stops growing with the attribute table.**

- **`fmt_attr(x, name)` / `` `fmt_attr<-` ``** (exported), validated against `fmt_col_attrs`,
  dispatching on an fmt column or a data.frame. **`fmt_attr_rules` gains a declared `write` column**
  — the attribute's own setter — so the generic validates exactly as `set_scale()` does and a build-
  time `stopifnot` refuses an attribute with no writer. **Adding a 17th attribute now needs no
  accessor at all.** ⚠ The hot path stays hand-written (the `DISPLAY_TOKENS` precedent):
  `get_col_var()` (33 sites), `is_totrow()` (44), `get_scale()` are untouched. ⚠ `fmt_attr()` is the
  **raw** read; `get_conf_level()`/`get_degf()`/`get_basis()` are *resolvers* (option fallback, NA →
  Inf, "" → "n") and stay internal — **the three named getters 19p asked for were NOT added**, per
  the ruling: `fmt_attr()` + `tab_columns()` answer that user story with two names instead of three.
- **`tab_columns(x)`** (exported) — one row per fmt column × its 19 stored facts, on `tab_shape()`'s
  model and `fmt_attrs_of()`'s reader. The only place `conf_level` / `degf` / `basis` / `ci_method`
  can be read side by side, which is what z13/z16 stored them per column *for*.
- **`set_diff_type()` → `set_ref_type()`** (old name a soft-deprecated alias; it is in 1.3.1). The
  pair did not share a stem, and the new name gets the validation the old one had **commented out**.
- ⚠ **The planned `@keywords internal` sweep had no target, and that is reported rather than
  faked**: the ~23 accessors are `@describeIn fmt`, i.e. **one** page already, so they occupy no
  index line to demote. What the measurement *did* find is **53 S3-method Rd stubs** (12 accessor
  generics × 3 arms), each a page restating its generic. Registered by `S3method()`, so `@noRd` beside
  `@export` removes the page and keeps the registration: **36 pages, 803 `man/` lines, gone.**

**The last unsurfaced Phase 19 fact.** ⚠ Correction to 19p §2.4: `basis` **is** already said —
`tab_weight_line()` switches on all four values. Only **`degf`** was invisible, and it is exactly
what makes a design-based interval differ from a flat one. `legend_method_phrase()` appends *"…, 42
design df"* **only** when the basis is `design`/`design_partial` and `degf` is finite, so an ordinary
table is byte-identical and only a real survey design gains the clause.

**The two live `FIXME`s are answered, not restated.** They asked one question twice, and the answer is
that the two arms of `vec_arith` ask *different* questions: `+`/`-` take two **symmetric** operands,
so `row_kind` / `in_refrow` / `in_tottab` survive only where they AGREE (a sum of a total-row cell and
a data cell sits in no row kind); `*`//` are **asymmetric** — "x per y" — so the metadata follows `x`,
and`mean` is dropped because a ratio of two means is not a mean. No behaviour change; two `# DESIGN:`
notes. `R/` now has **zero open FIXMEs**.

**The deletions and demotions** (each checked against CRAN 1.3.1, commit `86320287`):

- ⚠ **`tab_md_css()` is UNRELEASED** — absent from v1.2.0 *and* 1.3.1, which 19p §3.4 had wrong — so
  it is **deleted**, not deprecated. And, per the maintainer's ruling, the argument that made it
  necessary is fixed: **`tab_css(chrome = TRUE/FALSE)` → `tab_css(format = c("html", "md"))`**,
  borrowing `tab_export(format =)`'s existing vocabulary, so `tab_css(format = "md")` reads like the
  function it replaces. ⚠ `chrome` in `...` **aborts with the mapping** instead of being swallowed by
  `tx_deprecate_inert()` — a silently ignored `chrome = FALSE` emits the wrong stylesheet. The
  internal `chrome` word stays: it names a real CSS concept, and the translation happens once.
- **`tab_logit()` / `multi_logit()` deleted** (unreleased): 523 Rd lines, 0 vignette uses, thin
  forwarders that mirrored ~20 of `tab_reg()`'s formals and therefore **hid** `effect = "marginal"`,
  `measure`, `compare`, `baseline`, `reference` and `color`. **62 test call sites migrated**;
  `test-tab_logit.R` → `test-tab_reg-binomial.R`, coverage kept. ⚠ One assertion changed meaning
  rather than moving: *"a 3+ level dependent errors cleanly"* was asserting the WRAPPER's forced
  family — `tab_reg()` detects a 6-level nominal outcome and builds a multinomial table, which is the
  capability the wrapper hid. It now pins both halves.
- **`auto_or` and the `"or_table"` context deleted together** (rule 1): a `case_when` arm on a
  constant `FALSE` and the allow-list value it was the only producer of.
- **Soft-deprecated, released, never silently dropped**: `tab_prepare()` (+ `@keywords internal`, and
  out of pkgdown's *"Superseded entry points and steps"*, which read as a verdict it had not been
  given), `complete_partial_totals()`, `fct_recode_helper()`. `tab_get_wrapped_dimensions()` keeps its
  export and gains `@keywords internal`. ⚠ **`tx_user_call()`** (new, `R/utils.R`) is why the first
  two are silent from tabxplor's own build: `deprecate_soft()`'s "silent for same-package callers"
  does **not** hold under testthat, which treats every call as direct — so without it every `tab()`
  in the suite nudged about a call the user never made.
- **The five legacy steps are HARD-deprecated** (`deprecate_warn`, defunct 2.1.0). ⚠ The message
  distinguishes the deprecated **chaining API** from the **computations**, which moved into the leaf
  in 19j and are shared — "tab_ci() is going away" would otherwise read as "the confidence interval
  is". `test-steps-legacy.R` becomes the deprecation test. `R/tab-steps-legacy.R`'s 1 433 lines
  **stay** this cycle.
- **`tabxplor.color_style_type` deleted** — documented, never seeded, read at one place only to warn
  about itself; `?tabxplor-options`' own "keep in sync with `.onLoad()`" promise is true now.
  **`tabxplor.jmv_full_hash` → the internal constant `JMV_FULL_HASH`** (one seed, one read, no test,
  both sites internal).
- **`man/tab_many.Rd` 448 → 97**: `@inheritDotParams tab` does not link, it *inlines*.

**Documentation truths**: `FMT_FIELD_DOC$var` states the **rule** ("which one is given by its
`scale`") instead of the enumeration that drifts; `display_tokens_rd(user_only = FALSE)` stops
re-glossing the eleven tokens named after a field (`?fmt` glossed them **twice**, 30 lines apart, in
already-drifted wording); `?tab`'s `color_breaks` is one line; both reg vignettes' claim that
*"`tab_reg()` has no `display` argument"* — contradicted by each file's own `display = "ci"` section
430 lines later — is gone.

**HONEST CONCERNS.**

- ⚠ **Do not prefix a test run with `LC_ALL=C`.** Four `_snaps/render-html.md` failures appeared
  under it and reproduced on a pristine `git worktree` of HEAD, which read as pre-existing drift;
  the same tree is **FAIL 0** in the normal locale. `C` is a non-UTF-8 native encoding, harsher than
  any CI runner — CLAUDE.md § Testing already says to use `C.UTF-8` for the CI-locale run, and this
  is the second session to lose time to it. The `LC_ALL=C` rule belongs to the **censuses**
  (`sort` / `uniq` / `comm` collation), never to the suite.
- ⚠ **The planned "one `withr::local_options(lifecycle_verbosity = "quiet")` per test file" does not
  work**, and the plan's cost estimate for the step hard-deprecation rested on it: testthat 3e runs
  `local_reproducible_output()` inside **every** `test_that()`, which forces
  `lifecycle_verbosity = "warning"` again. The file-level line covers **top-level** calls only (which
  is real — `test-tab.R` builds its fixture with `tab_prepare()` there), and the comment in all 16
  files says exactly that. So the deprecated calls inside test blocks **do** warn — measured: **57
  warnings**, `tab_chi2` 14 · `tab_ci` 11 · `tab_tot` 9 · `tab_totaltab` 7 · `tab_pct` 7 ·
  `tab_prepare` 6 · `fct_recode_helper` 3, and nothing else. Migrating those calls to `tab()` is the
  corpus sweep, routed to **20h**.
- **`tab_columns()` reports the STORED attributes**, so `conf_level = NA` honestly means "no interval
  was stamped here" rather than "0.95 by default". One semantic, stated in `?tab_columns`.
- **`fmt_attr()` beside 23 named accessors is two ways to say one thing** — legitimate only because
  the header states the split (named = taught, generic = programmatic) *and* because the named family
  stops growing. If that line is not held in 20b–20i it becomes duplication.

**Routed to later phases** (measured here, not done here): `materialize_specs()$kind` has **no
reader** and none of its five values is a `ROW_KINDS` value → **20h** · `REG_ESTIMANDS$builder` has no
declared vocabulary (a bare `switch()` at `tab_reg.R:4144`) → **20e** · `TAB_ARG_VALUES$pct` says
`"no"` where `PCT_BASES` says `"none"` → **20b** · `tab_totcol_range()` is an orphan producer kept
alive only by its own test → **20h** · the deprecated-call corpus migration → **20h**.

No `.a.yaml` / `.u.yaml` was touched, so **no `jmvtools::prepare()` is needed** — 20g still owns the
outstanding rebuild.

**FOLLOW-UPS.** 20b and 20c can both start on this commit: `dev/verify_tab_args.R` and
`dev/census_exports.R` are committed with baselines, and `TAB_FOREIGN_KEYS` protects every table edit
they make.

---

#### Phase 20b — KEY 1 + KEY 8: the argument surface as data

**DONE (2026-08-16), both parts.** Full suite **FAIL 0, WARN 58, SKIP 4, PASS 6696**, against the
inherited FAIL 0 / WARN 58 / PASS 6626 — same warning count, +70 assertions, nothing red.
`dev/verify_tab_args.R` reports the **declared delta and nothing else**: two new keys on every
resolver case (`totaltab_name`, `other_level`) and one changed message (`unused_arg`, which is the
new suggestion) — ⚠ **`columns` is UNCHANGED across all 52 built tables**, which is the gate that
matters: an argument that survives resolution but stops reaching a column shows up there and nowhere
else. `dev/verify_color_attrs.R` **IDENTICAL** (293 cases), goldens untouched, `_snaps/` untouched,
`document()` idempotent, `tools::checkDocFiles()` **silent**.

**Formals: 149 → 80** across the five crosstab producers (`tab` 52 → 36 + `...`, `tab_plain` 29 → 9
- `...`, `tab_num` 28 → 9 + `...`, `tab_counts` 40 → 10 + `...`, `tab_many` unchanged).
**`man/` 7 318 → 6 801** (`tab_plain.Rd` 279 → 78, `tab_num.Rd` 208 → 70, `tab_counts.Rd` 137 → 92,
`tab.Rd` 693 → 636). `R/` grows **+533 lines**, which is expected and is not the metric (§2).

**The declaration (part 1, byte-identical to behaviour).**

- **`R/tab-args.R`** — **`TAB_ARGS`**, 67 rows, one per public argument of the five producers. The
  rule that keeps it from swallowing the fact tables is stated in its header: ***the fact table owns
  the VOCABULARY, TAB_ARGS owns the ARGUMENT*** — `MEASURES` knows what `difference` is; `TAB_ARGS`
  knows that `color` is an argument of four producers, which table names its values, and how to say
  so in a help page.
- **`TAB_ARG_VALUES` is DERIVED from it**, contents *and order* intact (the `DISPLAY_TOKENS`
  precedent), so `tab_validate_args()`, `tab_deprecate_many()`, `tab_ci()`'s totcol guard and
  `test-jamovi-vocabulary.R` did not move — and a frozen copy of the 19i literal is now a fixture.
  `validate = FALSE` is what keeps `ci` and `input` out of it: both are DECLARED here (so
  `resolve_ci_value()` stops spelling `c("auto","no","cell","ref")` twice in its own body) but
  validated by their own resolvers, because one rewrites its values and the other partial-matches.
- **`tab_args_rd(producer)`** — the fourth use of the `reg_measures_rd()` pattern, and the first to
  emit `@param` tags rather than an `@section` (spiked first; roxygen2 8.0.0 processes markdown,
  links and `\itemize{}` through `@eval` unchanged). The **order is `formals()`** — better than the
  proposed `group` column, because it matches `\usage{}` *and* is self-checking. ⚠ Which is why
  **there is deliberately no `group` column**: it was left with no reader, and a column with no
  reader is weight, not a fact (19b's admission test), so it is not there.
- **`color_measures_rd()` / `color_signif_rd()`** read `MEASURES` (which gains a `doc` member, in
  its existing exhaustiveness `stopifnot`) and `COLOR_SIGNIF_VALUES`. A `"{VALUES}"` element in a
  `doc` is *where* the generated list is spliced — an argument's value list sits mid-paragraph, with
  the grammar explained after it, so appending would have been wrong. The `(default)` marker is
  derived from the vocabulary's first entry, the convention `CI_METHODS` already uses.
- **`R/tabxplor-options.R` → `R/tab-options.R`, and it stops being doc-only**: **`TAB_OPTIONS`**,
  34 rows × (`default` · `section` · `arg` · `alias` · `seed` · `doc`). `.onLoad()` **seeds from
  it** (−35 hand-written `options()` calls), `?tabxplor-options` is **`@eval`-generated** from it,
  and the DEFAULT and the "Per-call `x =`" sentence are *rendered* rather than typed — which is what
  the deleted *"keep this in sync with .onLoad()"* comment used to ask a reader to do by hand.
  ~25 call sites that respelled a default (`getOption("tabxplor.anova", "welch")`) read
  `tx_option()` now, and the three `tx_getOption()` alias chains read `tx_option_names()`.
- ⚠ **The file name is load-bearing, and this cost two failed loads to find**: `tab.R` sorts
  *before* `tabxplor-options.R` but *after* `tab-options.R`, and `tab.R`'s DERIVED
  `globalVariables()` tail calls `new_ctx()` → `conf_level_default()` → `tx_option()` **at source
  time**. That is also why every computed `default` is a **closure**, and why `tx_getOption()` moved
  out of `utils.R` (which sorts last of all) into the option subsystem it belongs to.

**The signatures (part 2).**

- **`...` sits right after `wt`**, so **R itself** enforces "everything past the variable roles is
  named" — §7.1's unnamed-6th-argument guard, with no hand-written guard. Nothing is lost: position
  6 was `sup_cols`, so no *live* argument was ever reachable positionally past `wt`.
- ⚠ **The plan-of-plans' leaf sketch was wrong and would have broken released positional calls** —
  it moved `num`/`df` to positions 6/7 of `tab_plain()` (they are 20/21) and dropped
  `row_var`/`col_var`/`tab_vars` from `tab_counts()`' first four slots. Every leading positional slot
  is kept instead, and `...` begins exactly where the *shared* arguments begin.
- **`tab_check_dots()`** is what makes `...` a net gain: an unnamed argument refused **by position**,
  an unknown one refused **with a suggestion**. ⚠ The suggester must match **prefixes** as well as
  edit distance — a formal sitting after `...` loses R's partial matching, so an abbreviation that
  used to bind silently (`color_br =`) now arrives here and must be *named*, not merely refused.
- **`tab_dots_expand()`** fills an unsupplied argument from its **declared** default. That column
  exists because the mirrors documented themselves as "same meaning as in `tab()`" while their
  DEFAULTS were not all the same and nothing said which: `tab_num()` alone starts from
  `color = "auto"`, `ref = "tot"`, `comp = c("tab","all")`, `na = c("keep","drop")`, and both leaves
  from `tot = NULL`. Moving the formals into `...` would have thrown that away silently.
- **`options(tabxplor.total_names = c(row=, col=, tab=, other=))`** replaces three released formals
  (caught by name, deprecated with a message that names the **option**, lossless). A partial vector
  is completed from the declared default. The jamovi bridge installs it for the duration of one
  build instead of passing three arguments — the module speaks `tab()`'s current vocabulary, which
  is the point of the teaching path.
- **`options(tabxplor.stars)` absorbs `signif_levels` + `signif_labels`** (3 → 1). `tx_stars_ladder()`
  is the one reader; the retired pair is no longer seeded and still wins if a user set it.
  ⚠ `stars = <numeric>` **aborts naming the option** rather than being ignored: the ladder is a
  render-time reading of the stored p-value, so a per-call one would be a per-column stored fact —
  deliberately not built (maintainer's ruling: re-reading every table you already have is the better
  contract).
- **`conf_level` is one idiom now**: every public producer says `NULL` and
  `tab_resolve_common_args()` resolves it. `conf_level_default()` survives as the *internal* default.
- **`?tab` states `pct`'s `"no"` default** and why (a bare `tab()` is a table of counts).

**KEY 2 grows with the phase, which is the discipline it exists for**: 6 new edges into the argument
surface (`values_from` / `values_rd` / `option` / `doc_with` / `pct$stored`, and `TAB_OPTIONS$arg`)
plus **`tx_check_tab_args()`** — every covered producer's FORMALS and its declared rows are the same
set, and every surviving formal's default **is** the declared one. ⚠ It lives in `zzz-fact-keys.R`
and not beside `TAB_ARGS` because `formals(tab)` does not exist while `R/tab-args.R` is being
sourced. Both halves are proved to bite by fixtures.

**Three defects found and fixed, each with the fixture that fails without it.**

1. ⚠ **`tab(row_var = )`'s deprecation silently stopped firing** — `row_var` is a **prefix** of the
   live `row_vars`, and R matches a partial name against the formals **before** `...`, so it never
   reached `.dots`: the argument still did exactly the right thing and only the nudge was lost. It is
   read off `names(sys.call())` now. **This is 20a's first defect one level up** (three deprecations
   that had stopped firing), and it is the reason the harness dumps messages at all.
2. ⚠ **`as.character()` strips names**, so `tab_total_names_merge(c(other = "Autres"))` filled the
   slots *positionally* and silently renamed the total **row**. Caught by its own fixture before it
   could ship.
3. **The `$` partial-matching trap the `zzz-fact-keys.R` header warns about, hit again**:
   `r$values` matches `values_from`, so three rows leaked into the derived `TAB_ARG_VALUES`. Every
   read of a `TAB_ARGS` row uses `[[` now, and the header says so.

⚠ **And one guard fired that nobody had thought about**: `test-non-ascii.R` exempts comments and
checks *string literals*, so moving 360 lines of `@param` prose out of roxygen and into a `doc =`
vector moved an em-dash across that line. It is a real property of this migration — any prose that
becomes data becomes ASCII-checked — and it is worth knowing before 20h moves the exporters'.

**HONEST CONCERNS.**

- ⚠ **KEY 8 is only half landed, and that is a coverage gap, not a half-migration.** `TAB_ARGS`
  covers the five crosstab producers; the **seven exporters and `tab_reg()` have not joined**, so
  their `@param` blocks are still hand-written — 35 blocks for 7 concepts on the export side, and
  `?tab_reg`'s 101-line colour block. There is no *duplicate* encoding (an exporter's `color` is a
  logical, a different argument that happens to share a name), so hard rule 5 is not breached; but
  the plan's `?tab_reg` −90 and the exporters' −125 Rd lines are **not delivered**. `color_measures_rd()`
  exists and is filtered by `producers`, so `?tab_reg` is one `@eval` away. **Routed to 20h**, which
  is the pass that re-runs the censuses anyway.
- ⚠ **`var_labels` did not get its per-call argument** (§2.6). Measured: 5 new formals + ~12
  threading edits through `prep_one_table()` / `tab_export_prep()` / the transpose, for an argument
  with **0 corpus uses** — surface growth in a phase about shrinking it. **Routed to 20h**, where it
  lands with the exporters' `TAB_ARGS` rows and costs one row instead of five formals.
- **`tab.Rd` is 636, not the plan's ~490.** Prose moved verbatim (ruling 3), so the only saving on
  that page is the generated value lists. The rest is 22d's editorial pass — which will now edit
  **one table** instead of five files.
- **A user's abbreviation of an argument after `...` is a behaviour change.** `tab(pct = "row",
  col = "difference")` used to partial-match `color`; it now aborts with the full name in the
  message. Deliberate (silent partial matching is how a value reaches the wrong argument) and in
  `NEWS.md`, but it is the one thing here that can break a working script.
- **`test-jamovi-vocabulary.R` stays GREEN** — 20b renamed no *value*, only re-homed the table, so
  the 20g gate the plan warned about was not triggered. No `.a.yaml` / `.u.yaml` was touched, so **no
  `jmvtools::prepare()` is needed**; 20g still owns the outstanding rebuild.
- **`tab_dots_expand()` re-derives defaults on every leaf call** (a `tab_args_for()` scan + a
  `formals()` call). Measured as noise against a build, but it is on the leaf's hot path if someone
  ever calls `tab_plain()` in a tight loop — memoise it there if a profile ever says so.

**FOLLOW-UPS.** 20c can start on this commit: `TAB_ARGS`' idiom exists, and its `status` /
`default_for` / `doc_with` columns are exactly what a rename needs. 20h: the exporters' and
`tab_reg()`'s `TAB_ARGS` rows, `var_labels`, and the deprecated-call corpus sweep (still 58).

---

#### Phase 20c — KEY 4 + KEY 5: one word per question, and the footer's model

**DONE (2026-08-16), both keys.** Full suite **FAIL 0, WARN 58, SKIP 4, PASS 6777**, against the
inherited FAIL 0 / WARN 58 / PASS 6696 — same warning count, +81 assertions, nothing red. All four
harnesses print the declared delta and nothing else: `dev/verify_reg_specs.R` **IDENTICAL** on the
mid-phase checkpoints and, at the end, **only** the `test` tibble's renamed column across 201 of 290
cases (no `$call`, no `$cols`, no `$labels`, no message) · `dev/verify_color_attrs.R` **IDENTICAL**
(293 cases) · `dev/verify_tab_args.R` changes **2** of 249 captures, both `ci_method`'s new slot, and
⚠ **`columns` is UNCHANGED across all 52 built tables** · `dev/verify_golden_field_delta.R` reports
*"Only the declared addition differs"* over **1 788 cells × 36 goldens**. The 36 goldens were
regenerated for that one column rename, declared through the harness's own `RENAMED_TEST_COLS` slot
so the mapping is **proved**, not tolerated.

**`tab_reg()` 29 named formals + `...` → 25 + `...`. Six cross-producer name collisions → zero.**

**KEY 5 — `TEST_ROWS`** (`R/tab-test-display.R`). The `test` attribute carried **39 kinds of row**
under one discriminator and only the regression half had a declaring table (`reg_footer_spec()`, 31
of them). Details in the Repository Map. What it deleted, beyond the four crosstab literal lists: the
four **`paste0()`-generated** `compare_*` discriminators (hand-enumerated a second time in the footer
spec — the package's last generated keys), three `types = c(wald=, f=, lr=)` maps, the `global_*`
instrument switch, the `interact_*` label map, 21 hand-written labels, and `tab_kind()`'s need to
know that a regression's interaction rows live outside the footer spec.

⚠ **The one column the design pass and I both had to add back is `block`** ("which producer writes
this row"). `stat` cannot serve: a single-instrument row's `stat` **is its own name**
(`dispersion`, `compare_baseline`), so "the rows `reg_glance()` emits" is not expressible in it —
`REG_GOF_KEYS` derived from `stat == name` silently picked up five extra rows.

**KEY 4 — the renames.** `dependent` → **`outcome`** (package-wide, 200 R/ sites), `split_var` →
**`tab_vars`**, `reference` → **`ref`**, `method` → **`ci_method`** (with `CI_METHODS`' 5th slot
`model` and the new `CI_SLOT_PRODUCER`), `inverse_two_level_factors` → **`outcome_level`**,
`stats`+`compare`+`baseline` → **`stats`**, `.fit_cache` → `...`. Every retired spelling **aborts
naming its replacement**; there are no permanent aliases.

⚠ **`stats =` was kept, `footer =` was not** — a maintainer ruling taken this session that
*re-decides* the plan of plans (§4, §9 20c). `footer` is already the package's word for the whole
bottom region (`tab_footer_streams()` / `rd_footer()` / `reg_footer_lines()`), while the argument
governs only the model-summary block; `stats` is the narrow, correct word and already had a declared
vocabulary. Recorded so the ledger stops carrying two answers.

**Three defects found and fixed, each with the fixture that fails without it.**

1. ⚠ **A design-based numeric test called itself a Welch F.** `test_pvalue_descriptor()` read
   `if (any(num == "F_classic")) "ANOVA F" else "Welch F"`, and after the survey overlay a design
   table carries **only** `F_design` — so it printed `pvalue (Chi2, Welch F; survey-design)` for a
   test that is `svyglm` + `regTermTest(method = "Wald")`. Each row declares its own `word` now.
   One-string golden delta; no existing test asserted it.
2. ⚠ **`inverse_two_level_factors` was a SILENT NO-OP on a 0/1 numeric outcome** — that branch of
   `reg_prep_binary()` (and of `reg_positive_level()`) returns before ever reaching the level
   reversal. `outcome_level` works there, accepting either the synthesised label or the raw `"0"` /
   `"1"`.
3. ⚠ **The jamovi `depModelLevel` picker threw its answer away.** It asked for a LEVEL and folded it
   into a logical "did the user pick anything?", so **any** pick modelled the SECOND level — choosing
   the first one in the UI silently modelled the other. `tab_reg()` takes a level now, so the bridge
   passes it through and got *simpler*.

**And one design gap closed while migrating the harness**: `stats = "compare_baseline"` first meant
"a footer with only the comparison in it", because the comparison keys were removed from `stats`
before it reached `reg_footer_stats()`. A comparison key **restricts nothing** now — naming only it
keeps the per-family default set, which is what `compare = "baseline"` always did. Caught because the
10 migrated `compare.*` harness cases went red; nothing else would have seen it.

**`tab_robust_overlay()`'s suspected defect is DENIED, and that is the phase's most useful negative
result.** `chi2_compute_test()` emits `F_welch` **and** `F_classic` from the same table with the same
`effect_size` / `es_type` / `min_e`, so `anova = "classic"` loses nothing: the `c("chi2", "F_welch")`
literal is a **de-duplication device**, not a display choice. It is stated as one now (every classic
crosstab row, `distinct()`-ed on the join key), which removes the latent hazard its `semi_join` had —
a producer emitting `F_classic` without `F_welch` would have dropped the whole design row, p-value
included, in silence.

**HONEST CONCERNS.**

- ⚠ **`tab_reg()` did NOT get `@eval tab_args_rd()`, and `man/tab_reg.Rd` GREW 721 → 739** where the
  plan estimated ~550. All 25 formals are declared in `TAB_ARGS` — which is what lets
  `tx_check_tab_args()` police that signature and makes "the two producers ask the same question" a
  *checked* fact — but the prose stayed put, because the phase **measured** the thing that would have
  justified moving it and it was not there: *the two producers share the NAME and the GRAMMAR of
  `wt` / `ref` / `na` / `display` / `color` / `ci_method` / `tab_vars`, not the PROSE*. Every one of
  those reads differently on a model, so emitting the crosstab text into `?tab_reg` would be **wrong**
  documentation, not deduplicated documentation. The growth is `outcome_level`'s new block and
  `stats`' comparison paragraph. Trimming `family`/`effect`/`measure` against
  `@eval reg_measures_rd()` is a *prose* edit and belongs to **22d**.
- ⚠ **The jamovi UI speaks the old vocabulary until 20g.** `jmvtabreg.b.R`'s `.opts()` translates
  five yaml option names (`dependent`, `split_var`, `refLevels`, `method`, `compare`+`baseline`) into
  `tab_reg()`'s; each carries a `# ⚠ 20g:` marker. Deferred **on purpose**: renaming the `.a.yaml`
  now leaves the module inert (a stale generated `.h.R`) across 20d/20e/20f. No `.a.yaml` / `.u.yaml`
  was touched, so **no `jmvtools::prepare()` is needed** — 20g still owns the outstanding rebuild.
- ⚠ **`test-jamovi-vocabulary.R`'s two reg assertions changed shape in the same commit as the change
  they gate** — `method` now asserts `CI_METHODS$model` instead of `formals(tab_reg)$method`, and the
  ComboBox loop asks `ci_slots_of("tab")` instead of enumerating every slot. Both are *stricter*
  single sources (the crosstab module must NOT have a `method_model` box), but a gate rewritten
  beside its subject is worth saying out loud.
- **`reg_resolve_args()` keeps its internal `method` formal** (the resolved scalar) while the public
  argument is `ci_method` (the named vector). Deliberate — the internal one is one slot's value, and
  `reg_fit(method =)` speaks the same word — but it is two names for related things, one layer apart.
- **`R/tab-args.R`'s 25 `tab_reg` rows are read by exactly one consumer** (`tx_check_tab_args()`);
  `tab_check_dots()` / `tab_dots_expand()` are not on this producer's path, which still uses
  `reg_retired_args()`. Folding those two together is a candidate for **20h**.
- ⚠ **`reg_retired_args()` now ignores every dot-prefixed name**, which is how `.fit_cache` rides
  `...`. That is the package's own convention (`tab()`'s `.cache` / `.return_armed`), but it means a
  typo'd `.fit_cach` is silently accepted rather than refused.
- The 58 warnings are the pre-existing step-API deprecations; the corpus sweep is still **20h**'s.

**FOLLOW-UPS.** 20d can start on this commit — the surface has settled, and
`dev/verify_reg_specs.R`'s baseline is re-saved with the phase's declared renames encoded in its new
`SPEC_RENAMES` / `FIT_SPEC_RENAMES` / `CALL_RENAMES` maps (the twin of the golden harness's
`RENAMED_TEST_COLS`, so a rename stays *provable* rather than re-baselined blind). 20g: the five
yaml option renames and the owed `prepare()`. 20h: `tab_reg()` onto `tab_check_dots()`, and the
`?tab_reg` prose trim.

---

#### Phase 20d — KEY 7: marginal effects, computed once and computed fast

**DONE (2026-08-16), both halves.** Full suite **FAIL 0, WARN 58, SKIP 4, PASS 6894**, against the
inherited FAIL 0 / WARN 58 / PASS 6777 — same warning count, +117 assertions, nothing red.
`dev/verify_reg_specs.R` prints **IDENTICAL over all 290 cases** (every message, spec, column
attribute, label and test key), against a baseline saved on the untouched tree.

**The measured result.** `marginaleffects` leaves the hot path entirely on the `at = "average"` route:

| case (gss, 21 483 rows; warm, min of 2)                | before        | after         |       |
|--------------------------------------------------------|---------------|---------------|-------|
| `effect = "marginal"` (binomial, 4 predictors)         | 8.69 s        | **0.59 s**    | 14.7x |
| `effect = "marginal", measure = "ratio"`               | 8.46 s        | **0.42 s**    | 20.3x |
| `effect = "marginal", empirical = TRUE`                | 9.21 s        | **0.66 s**    | 14.0x |
| `display = "ame"` (the fold onto a coefficient column) | 6.61 s        | **0.40 s**    | 16.4x |
| `effect = "marginal"` (3-level **multinomial**)        | 36.39 s       | **3.43 s**    | 10.6x |
| `effect = "coefficient"` / `at_reference` (untouched)  | 0.27 / 1.23 s | 0.28 / 1.26 s | 1.0x  |

**The brief's own route would have been a defect, and that is the phase's most important finding.**
19o and the plan of plans both said "supply the SE from the influence function". Measured, that
substitution moves printed standard errors by **up to 3.6 %** on a rare level — `reg_coef_if_maker()`
is a *sandwich* variance (empirical information, not `vcov(fit)`) **and** the full IF adds the
empirical-averaging term. The phase's own ruling is "a change in the last printed decimal is
acceptable, a change anywhere else is a defect", so the printed interval takes the **analytic delta**,
`sqrt(G' vcov(fit) G)`, which reproduces `marginaleffects` to **1e-8** on estimate, standard error,
both bounds *and* the p-value, on `glm` and weighted `svyglm` alike. The full IF stays exactly where
it was, in the adjustment-gap test. **Two different questions — *how uncertain is this AME* vs *is it
different from its crude twin* — correctly answered by two different variances**, and that is now
said in the code (`reg_delta_se()` beside `reg_if_se()`, each pointing at the other).

**The producers (`R/reg-influence.R`).** An AME, its adjusted predictions and both variances are ONE
counterfactual sweep read three ways, so the sweep became its own producer: **`reg_gcomp_maker()`**
(lm/glm/svyglm) and **`reg_gcomp_cat_maker()`** (multinom/polr). The two influence makers are now
their four-line wrappers — the single-equation one **byte-identical by construction** (same
arithmetic, same order), locked by the existing 1e-10/1e-12 pins. Details in the Repository Map.

**The 3+ level half was NOT in the brief.** A 3-level multinomial AME was measured at **45.2 s**
(against 4.4 s for coefficients) — worse than the binomial case that motivated the phase — and its
existing central-difference jacobian cost **2.4 s per contrast**, i.e. it was never a fast path. The
analytic softmax / cumulative-logit jacobian is the derivative of the function `reg_prob_engine()`
already implements, so it was validated against **both** oracles before being written into the plan:
marginaleffects (multinomial SE 1.06e-09; ordinal 5.3e-09 … 3.0e-08, every category; estimates exact)
and the package's own central-difference jacobian. `reg_prob_engine()` gained `dmean()` and now has
three consumers instead of two.

**`vcov = FALSE` wherever the interval is discarded** — byte-identical estimates, and it is what
helps the routes that stay numeric: the prediction pass only ever reads `$estimate`, and
`reg_apply_display()`'s fold pokes `pct`/`diff` into a column that keeps its own CI (a new
`want_se = FALSE`).

**Two defects found, each with the fixture that fails without it.**

1. ⚠ **`reg_empirical_fit()` clobbered its own `est` argument.** Its marginal branch assigned the
   per-predictor estimates to `est` — which is *also* the estimand-row argument. Harmless while
   nothing read that argument twice; the moment the engine is read off it per predictor, **every
   predictor after the first aborted inside a `tryCatch` and lost its `obs` in silence**. `obs` is
   what `color = "adjustment"` scores, so nothing in the printed values showed it. Caught by
   `verify_reg_specs.R` (one changed case), which is exactly what that harness is for.
2. **`reg_build()`'s builder dispatch fell through to the coefficient builder** — the 20a-routed
   item. Every arm is named now and the default aborts, with `REG_BUILDERS` + a foreign key
   (⚠ `TAB_FOREIGN_KEYS` is 51 edges, +2).

**And one latent trap hardened**: the counterfactual wrote `factor(lv, levels = levels(x))`, which
**drops `ordered`** — an ordered predictor would then get treatment contrasts where the fit used
polynomial ones (measured on gss `rincome`: AME 0.1038 instead of 0.0302). It cannot bite through
`tab_reg()` (14r's `reg_fit()` de-orders predictors first), so this is a *contract* fix on a shared
helper, with its own unit fixture rather than an end-to-end one.

**HONEST CONCERNS.**

- ⚠ **Three existing assertions were relaxed from `tolerance = 1e-10` to `1e-7`**
  (`test-tab_reg-rr.R`, `test-tab_reg-numeric-crude.R` ×2), and a relaxed assertion always deserves
  suspicion. The justification is measured, not asserted: on the same fit, **marginaleffects' own
  step-size choice (`fdforward` vs `fdcenter`) moves that bound by 3.99e-9, while our analytic value
  sits 2.74e-9 from its default** — our residual is *smaller than the oracle's own noise*, because
  ours is the exact number and its is the approximation. Only the **bounds** were relaxed; every
  ESTIMATE assertion stays at 1e-10 and passes with a relative difference of **0**.
- ⚠ **`at_reference` and the MNL `vsrest` row stay on `marginaleffects`**, declared, not forgotten:
  a one-row profile grid is not something g-computation builds, and the route costs 2.4 s, not 45.
  `effect = "at_reference"` improved anyway (2.4 s → ~1.5 s) purely from the prediction pass.
- **`engine` is `"auto"` on every row**, resolved by one rule in `reg_marginal_engine()` rather than
  written out 36 times — the `crude_fam = "auto"` idiom from the same table. That is a *rule* wearing
  a column's clothes; it is honest only because any row can override it and a fixture asserts that.
  If a future family needs a per-row value, write it there rather than widening the rule.
- **The fallback is per CALL, not per contrast** — deliberate (one column, one convention), but it
  means a single refusing contrast pays the full numerical route for the whole table.
- **`dev/verify_reg_specs.R` gained a third declared-delta shape**, `EST_ADDITIONS`, beside its two
  rename tables: a new member on a `REG_ESTIMANDS` row rides `est` into every spec and into
  `reg_call()`, so without it the run reports every non-aborting case as CHANGED. Its diff printer
  was also fixed — it unlisted the first 12 elements, which for `cols` is always *column 1*, so it
  printed two identical lines while a column-11 attribute had really moved.
- **The crosstab harnesses were not run**: the diff touches `R/reg-*.R` and `R/tab_reg.R` only, no
  `tab()` code path, and `test-golden.R` covers the goldens inside the suite anyway.
- ⚠ **The timings are ext4/WSL2** and are not comparable to the committed Windows baselines.
  Recorded in `dev/benchmarks/results_2.0.0/`.

No `.a.yaml` / `.u.yaml` was touched, so **no `jmvtools::prepare()` is needed** — 20g still owns the
outstanding rebuild, and the jamovi Regressions panel's marginal option is no longer the one that
freezes it.

**FOLLOW-UPS.** 20e can start on this commit — the marginal path has settled, which is what a
provably-pure `reg_build()` refactor needs, and `dev/verify_reg_specs.R`'s baseline should be re-saved
past `EST_ADDITIONS` first. ⚠ **20f must re-measure**: the case for a process pool was built on a
15.3 s call that is now ~1.2 s, and on a 45 s multinomial that is now 5.2 s — of which **4.4 s is the
`multinom` fit itself**, so whatever remains to parallelise is the fitting, not the marginal pass.
Routed to **20h**: the model AME interval uses `z` while its crude companion beside it uses
`t(degf)` (a live asymmetry noted at `R/reg-resolve.R:843-851`; closing it changes printed numbers on
a clustered design, which §6 forbids in this phase), and `REG_ESTIMANDS$obs` still has no reader.

---



#### Phase 20e — KEY 6: `reg_build()` becomes a staged build

**DONE (2026-08-16).** Full suite **FAIL 0, WARN 58, SKIP 4, PASS 6894** — byte-for-byte the
inherited numbers. `dev/verify_reg_specs.R` prints **IDENTICAL over all 290 cases** (every message
*in order*, every spec, every column attribute, every label, every test key) against a baseline
saved on the untouched tree, and it printed IDENTICAL at every one of the phase's four checkpoints.
Zero golden churn, zero `_snaps/` churn, `document()` a no-op (everything new is internal).

**`reg_build()` is 726 source lines → 20 deparsed lines over eight named stages.** It was the
largest function in the package — 39 top-level locals, 7 local closures, eleven unnamed phases —
while `tab_build()` has had a typed ctx and named stages since 17e/19i. The largest function is now
`plain_core()` (482 deparsed); the largest stage is `reg_stage_tips()` at 103.

- **`new_reg_ctx()`** — 32 declared keys, `new_ctx()`'s idiom on its fourth use (the FORMALS are the
  contract, `as.list(environment())` the body, the `globalVariables()` mirror DERIVED), with 19i's
  rule applied: **a stage product is DECLARED, never left to appear** — an undeclared key is
  *absent*, so its own `is.null()` guard errors instead of firing. The keys are grouped and commented
  **by the stage that writes them**, so "what may a stage find" is readable without running a build.
- **`shared` stays ONE nested record and is PROJECTED**, never flattened. `reg_ctx_locals(ctx)` is
  `ctx_settings_locals()`'s twin (`c(ctx, ctx$shared)`), so the ~200 bare-name reads inside the moved
  bodies read exactly as before, and 19i's two-carriers problem is not recreated. Stages close with
  the existing `ctx_update()`.
- **The eight stages, each named after the part of the table it produces**: `_split` (the `tab_vars`
  recursion, at the TOP, returning a finished TABLE — `tab_build_tables()`'s shape) · `_fit` ·
  `_columns` · `_footer` · `_rows` · `_empirical` · `_assemble` · `_tips` · `reg_stage_finalize`.
  The seven closures became **four top-level functions** — `reg_cols_coef` / `reg_cols_ame` /
  `reg_cols_vsrest` (the three arms `REG_BUILDERS` declares), `reg_emp_frame` (needed by TWO
  stages), `reg_set_obs`, `reg_add_emp_cols` — plus one one-line local.
- **`R/reg-empirical.R`** (~1190 L), carved whole out of `tab_reg.R` (5630 → **4734**): the observed/
  crude subsystem the brief calls the package's third biggest, until now spelled as an `if` block
  plus two 200+-line functions in the middle of the largest file. The producers live there, the
  stage that drives them stays in `tab_reg.R` — the `tab-leaf.R` / `tab.R` relationship.

**⚠ Two corrections to the plan of plans, both measured rather than assumed.**

1. **The stage ORDER is the source order, and `_footer` runs BEFORE `_empirical`** (the §9 sketch has
   them the other way round). Reordering is not a refactor here: **FOUR sites fit models** — `_fit`,
   `_footer`'s linearity refits, `_empirical`'s univariable fits, and `_split` →
   `reg_interaction_rows()` — and every fit may inform or warn, so the stage order IS the message
   order, which `verify_reg_specs.R` compares.
2. **§5.4's "the ONE place a fit happens = the parallel seam" is false.** On a 5-predictor
   `empirical = TRUE` table the model fits are a *minority* of the fits. Written into
   `reg_stage_fit()`'s own header: **20f must measure all four sites** before parallelising one.

**One defect, found the moment the ctx was introduced and fixed with the rename.** ⚠
`as.list(environment())` — the idiom all four record constructors use — defaults to
**`all.names = FALSE`**, so **a dot-prefixed key is silently dropped from the record**. Declaring
`.fit_cache` as a ctx key therefore made the jamovi fit cache simply stop being threaded, with no
error until a stage read it. The ctx key is `fit_cache`; `reg_build()`'s formal keeps its dot
(it is the entry point's internal argument, `tab()`'s `.cache` convention). Stated in `new_reg_ctx()`
and in the Repository Map, because it applies to `new_reg_shared()` / `new_reg_spec()` /
`new_reg_args()` equally.

**The three findings from the plan, all fixed:**

1. **`mnl_vsrest` was assigned and never read** — dead local, deleted. (`compound`'s inline
   re-derivation in the empirical block is `any(compound)` now.)
2. **Two hand-written mini-records named `shared`.** `reg_global_rows()` and `reg_check_rows()` each
   took a `list(weighted =, design_spec =, …)` literal whose every field is a **subset** of the real
   `new_reg_shared()` record — two look-alikes of the one typed record, in the file that declares it.
   Verified field by field (incl. `reg_check_linearity_rows()`'s five reads); both take the real
   record now, as `reg_interaction_rows()` already did.
3. ⚠ **`skeleton_data = data`'s lazy default was forced too late.** The promise was first forced
   *after* `data` is releveled on the jamovi reref path, so on that one path it silently meant the
   **post**-relevel frame rather than the full pre-relevel data its own comment describes. Forced at
   the head now (`new_reg_ctx()` forces it). Behaviour-identical and it is stated why: a factor
   relevel moves no predictor between `reg_numeric_preds()`/`reg_factor_preds()`, and `reg_curves()`
   reads only the numeric predictors and the outcome. `test-jmvtabreg-cache.R` (the reref
   byte-identity contract) is green.

**Also landed**: `tx_check_reg_ctx()` in `R/zzz-fact-keys.R` — `new_reg_ctx()` and
`new_reg_shared()` declare two record types that every stage binds into ONE scope, so a shared name
would silently shadow (`c()` keeps both, `list2env()` lets the last win). It lives there for
`tx_check_tab_args()`'s reason: neither constructor exists while the other's file is being sourced.

**Already done, reported not repeated**: `REG_ESTIMANDS$builder`'s missing vocabulary — routed here
by 20a — **landed in 20d** (`REG_BUILDERS`, named `switch()` arms, an aborting default, and a
two-directional foreign key). The stage split only inherits it.

**HONEST CONCERNS.**

- **`R/` grew.** `tab_reg.R` −896, `reg-empirical.R` +1190, so the reg subsystem is ~+290 lines: the
  ctx declaration, ten stage headers, four hoisted helpers with their own docstrings. That is the
  phase's expected shape (§2: *do not count lines as the simplification metric*), and the metric that
  did move is the one KEY 6 is about — the largest function in the package went from 534 deparsed
  lines to 20, and "which stage produced this part of the table" is now answerable by reading one
  screen.
- ⚠ **`reg_stage_tips()` is 103 deparsed lines and does two unrelated things** (the multinomial
  crude tooltip and the numeric-predictor distribution tooltip). They share only their carrier
  (`meta$empirical_tips`) and the fact that neither number can honestly take a column. Splitting them
  would have been a second, unproved edit inside a phase whose contract is IDENTICAL; **routed to
  20h**, where the census runs anyway.
- **`reg_set_obs()` re-projects the whole ctx per column** (`list2env(reg_ctx_locals(ctx), …)` inside
  a loop over columns). Measured as noise against a build that fits models, but it is the one place
  the projection idiom sits on a per-column path rather than a per-stage one.
- **The three `reg_cols_*` builders take `(f, sp, ctx)`** and read their ~13 settings by projection
  rather than by named argument. That is deliberate (it is what made the hoist byte-identical and it
  matches the stage idiom), but it means their signatures do not state what they read.
- **Not run**: `verify_color_attrs.R` / `verify_golden_field_delta.R` / `verify_tab_args.R`. The diff
  touches `R/tab_reg.R`, `R/reg-empirical.R`, `R/reg-assumptions.R` and `R/zzz-fact-keys.R` only —
  no `tab()` code path — and `test-golden.R` covers the goldens inside the suite, which is green.
- **`dev/verify_reg_specs.R`'s `EST_ADDITIONS` is now empty** and its baseline re-saved past 20d's
  `engine`, per 20d's own follow-up: that member is COMPARED again rather than dropped on both sides.
- The 58 warnings are the pre-existing step-API deprecations; the corpus sweep is still **20h**'s.

No `.a.yaml` / `.u.yaml` was touched, so **no `jmvtools::prepare()` is needed** — 20g still owns the
outstanding rebuild.

**FOLLOW-UPS.** 20f can start on this commit and **must re-measure first**: `reg_stage_fit()` is the
seam, but it is one of four fitting sites and 20d already took the marginal path from 15.3 s to
~1.2 s. 20h: `reg_stage_tips()`'s two halves, and `reg_build()`'s remaining `.fit_cache` threading
(the ruling was *keep as is*, and it was kept).

---

#### Phase 20f — `tab_reg()` parallelisation: measure, then decide

**DONE (2026-08-16). The verdict is NO POOL, and the phase's product is the measurement that says
why — plus the four things the measurement found instead.** Full study:
`dev/tabxplor_reg_performance.md`; re-runnable harness `dev/benchmarks/phase20f_reg_profile.R`,
before/after in `dev/benchmarks/results_2.0.0/phase20f_reg_profile{_before,}.{txt,csv}`.

**THE MEASUREMENT.** Post-20d/20e, **81–94 % of every default `tab_reg()` call is the model-check
footer**, at every data size (binomial 4 preds: 0.65 s → 0.16 s with `stats = FALSE`; ordinal
8.75 → 1.73; binomial 6 preds at n = 200 000 **12.27 → 1.29**). And a large part of that was the
20d pathology one level down — *work computed several times and read once*, each traced by
instrumenting the call, not inferred:

1. **Linearity fitted the model TWICE per numeric predictor.** The augmented fit is the test; the
   second was `drop1()` refitting the reduced model, which is `base_fit`, already in hand (1.02 s
   against 0.028 s at n = 200 000).
2. **`vcov()` recomputed FOUR times on one multinomial fit** — `nnet:::multinomHess` ran **7×** per
   default table (1× at `stats = FALSE`) at **0.757 s each**, because `reg_check_model_se()` and
   `reg_coef_if_maker()` each called it and `dispersion` and `influence` each called both.
3. **The Brant test ran THREE times per ordinal table and was read once** (~1.1 s each):
   `reg_fit_ordinal()` ran it on every polr fit — the reported one, both Linearity refits, every
   crude univariable fit — and `attr(fit, "brant_po")` had exactly one reader.

**WHY NO POOL**, five measured reasons: the work was redundant, not distributable · `tab_pmap()`
**dropped worker conditions entirely** while `verify_reg_specs.R` compares messages *in order*, and
three of the four fitting sites are unsuppressed or only message-suppressed — so parallelising them
was a correctness regression · **jamovi can never use a pool** (`cache_env` forces serial,
`.fit_cache` is an environment) and at teaching scale the checks were 94 % of the call, so the
de-duplication is the only lever that reaches the interactive path · the common call was already
0.16 s without the footer · and the one qualifying payload is now ~1 s of a 3.4 s call at
n = 200 000. Also measured and declined, with the numbers, in the study: the Rao score test (z15's
ruling holds — design-blind), `glm(start = )` (4 IRLS iterations either way), and
`anova(base, aug, test = "F")` as the F engine (see the trap below).

**WHAT LANDED INSTEAD** — no new option, no new formal, no new fact table, no concurrency.

- **D1 — `reg_nested_test(base, aug, use_f)`** (`reg_nested_lr()` grown an F arm and promoted from
  fallback to first choice): the Linearity check compares the two fits it holds. ⚠ It IS what
  `drop1()` returns, **bit for bit on both arms** — and the F arm is the trap: `drop1.glm` at its
  default `scale = 0` estimates the dispersion as `deviance/df.residual` of the AUGMENTED fit, which
  is neither the Pearson dispersion `summary()` reports nor what `anova()` uses (12.47 against 14.25
  on a quasipoisson fit). Pinned with `expect_identical()` on lm / gaussian glm / quasipoisson /
  binomial, plus two refusal fixtures (not nested, different rows). `reg_term_tests()` stays the
  design arm, where `regTermTest()` refits nothing anyway.
- **D2 — `reg_check_influence_pass()`**: Dispersion and Influence are ONE decomposition read two
  ways (one `vcov`, one influence closure, one sweep of the `p` unit contrasts). The two footer rows
  stay two declared rows; only the arithmetic merged. `vcov()` per fit **4 → 1**, via a new
  `reg_fit_vcov()` and a `V =` argument on `reg_check_model_se` / `reg_coef_if_maker` /
  `reg_score_multinom` / `reg_score_polr`. ⚠ `reg_check_model_se` keeps its svy_vglm `fit$var`
  degradation LOCAL: that is a sandwich, and handing it to `reg_score_polr` as the bread would
  double-count the design.
- **D3 — the Brant test moved to the row that reads it** (`reg_check_rows()`'s `proportionality`
  branch); `attr(fit, "brant_po")` is deleted. One producer, one consumer, one warning.
- **D4 — `REG_CHECKS$cost`** (`"free"` / `"refit"`) + `reg_checks_default()` / `reg_checks_costly()`
  + two build-time `stopifnot`. The two fit-based checks leave the default `stats` set and are asked
  for by name. ⚠ Default-set vs vocabulary, not vocabulary vs nothing: `reg_check_rows()` still asks
  `reg_checks_for()`, and `reg_check_plots()` is untouched — a panel is always free, so `cost` is
  independent of `panel`. **And `stats = "all"` starts meaning all**: it was a synonym of `NULL`,
  i.e. of the default set, so it already lied and D4 would have made it worse. It is now the one
  value to remember for the whole diagnostic footer (the maintainer's question, answered by fixing
  the name rather than adding a `"checks"` group key).
- **D5 — `tab_pmap()` relays worker conditions**, in unit order. Measured defect, not a
  precaution: on `tab(parallel = 2)` with two numeric col_vars at different references, serial says
  2 messages and parallel said **0**. The `everywhere()` options snapshot gained `cli.*` / `crayon.*`
  / `width` for the same reason — cli renders its text AT SIGNAL TIME, so a daemon formatted with its
  own glyphs and wrap width and the relayed message did not match the serial one (caught by the
  fixture on its first run).

**THE RESULT.** `stats = "all"` is the honest column — it computes strictly MORE than today's
default did:

| shape (gss_cat unless stated)       | before (default) | after (default) | after (`stats = "all"`) |
|-------------------------------------|------------------|-----------------|-------------------------|
| binomial, 4 predictors, n = 2 000   | 0.14 s           | **0.08 s**      | 0.12 s                  |
| binomial, 4 predictors              | 0.65 s           | **0.32 s**      | 0.67 s                  |
| binomial, 6 predictors, n = 200 000 | 12.27 s          | **3.44 s**      | 7.90 s                  |
| multinomial (3 levels)              | 5.89 s           | **1.78 s**      | 3.95 s                  |
| ordinal (16 levels)                 | 8.75 s           | **1.45 s**      | 4.13 s                  |
| 3-model comparison, n = 200 000     | 13.55 s          | **4.61 s**      | 9.41 s                  |
| `tab_vars` (4 groups), n = 200 000  | 4.93 s           | **1.73 s**      | 3.75 s                  |

2.6–6.0× on the default call, and **1.3–2.1× even with every check asked for** — that second column
is the pure de-duplication. Counts, before → after: `multinomHess` 7 → 2, `brant` 3 → 0 (1 when the
check is asked for), the `drop1` refit 3 → 1 on a binomial table and 2 → 0 on a multinomial one.

**VERIFICATION.** Full suite **FAIL 0, WARN 58, SKIP 4, PASS 6920**, against the inherited FAIL 0 /
WARN 58 / PASS 6894 — same warning count, +26 assertions, nothing red.
`dev/verify_reg_specs.R`: **CHANGED in 188 of 290 cases, in exactly the two declared places** — the
`test` tibble's rows (186 cases, D4) and the Brant warning (13 cases, D3). Every spec, every
`reg_call()`, every column attribute and every label IDENTICAL, and every OTHER message identical
**in order**. ⚠ The baseline must be re-saved past those two.
`test-parallel-parity.R` green **unsandboxed** with its new fixture. No golden churn (no `tab()` code
path is touched but `tab-parallel.R`, whose serial branch is unchanged), so
`verify_golden_field_delta.R` / `verify_color_attrs.R` / `verify_tab_args.R` were not run.

**HONEST CONCERNS.**

- ⚠ **A default ordinal table no longer warns that the proportional-odds assumption is rejected.**
  The warning is the Brant check's output, so it follows the check out of the default set. Declared,
  ruled by the maintainer with that consequence stated, and in `NEWS.md` — but it is the one thing
  here a user could miss rather than merely wait less for. Its own text says it over-rejects at
  survey N, and `stats = "all"` / `stats = "proportionality"` brings it back.
- ⚠ **`reg_nested_test()`'s ordinal arm is not bit-identical to `drop1.polr`** (which re-optimises
  through `optim`), so an ordinal Linearity p can move in ~1e-6 — invisible at printed precision, and
  the new number is the more accurate one. Not separately measured: no fixture pins an ordinal
  Linearity p, and `verify_reg_specs.R` compares test KEYS, not values. Worth a look if 20h touches
  the ordinal footer.
- **D5's replay is necessarily late**: worker conditions land after anything the caller signalled
  around `tab_pmap()`, rather than interleaved. Stated in the code and in the fixture, which asserts
  the same set and the same relative order among worker conditions.
- **The three `stats` migrations in the test corpus are a behaviour change made visible**: 6 blocks
  now pass `stats = c("n", "linearity")` or `"proportionality"`. That is the argument working, but it
  means a reader of those tests must know the default moved.
- **`reg_global_rows()`'s `drop1` still refits** one reduced model per multi-coefficient term (~2 s
  at n = 200 000). Left in the default set deliberately — it is a *test*, not a diagnostic, and the
  only cheaper route (a Wald test) is a different number. Recorded in the study, routed to **20h**.
- ⚠ The timings are ext4/WSL2 and are not comparable to the committed Windows baselines.

No `.a.yaml` / `.u.yaml` was touched, so **no `jmvtools::prepare()` is needed** — but ⚠ **20g owes
one UI item**: the Regressions panel's `stats` control must offer the two now-opt-in checks and an
"all" entry, or the module silently loses them.

**FOLLOW-UPS.** 20g can start on this commit (⚠ the `stats` control, above). 20h: the study's own
routed item (`reg_global_rows()`'s refits), `reg_stage_tips()`'s two halves (from 20e), and the
deprecated-call corpus sweep.

---

#### Phase 20f-ii — `tab_reg()` parallelisation at the model level

**PARTLY DONE (2026-08-16): the measurement is complete and two fixes shipped; the S-axis
restructure is NOT built and is a fresh-session piece.** Study: `dev/tabxplor_reg_performance.md`
**§6** (extended, never a second file — it is *the* durable answer to "why is `tab_reg()` not
parallel", and a second document answering it one axis out is the duplication this phase exists to
remove). Re-runnable harness `dev/benchmarks/phase20f2_reg_model_axis.R`, before/after in
`dev/benchmarks/results_2.0.0/phase20f2_*`.

**THE STRUCTURAL FINDING, which decides more than the timings do: the three model axes are not the
same shape.** `tab_vars` groups (**G**) and several outcomes × a models list (**R**) are the two
places `tab_reg()` already recurses, and each unit returns a **finished table** — fit-free, KB-sized,
with the cross-unit work already after the loop and the message stream already unit-major, so a
`tab_pmap()` relay would preserve order exactly. Several outcomes in ONE table, or a models list
(**S**), share one table: the unit returns the raw fit, and `emp_by_fit[[i]]` carries `$frame` +
`$fits` — **60–100 MB at n = 200 000**, six to ten times the payload 20f-i's constraint was written
about.

**THE MEASUREMENT.** Ceilings (a perfect pool, zero overhead; clamped at the longest unit, since no
number of cores shortens it): G 4 uneven race groups **1.23×** · G 8 even survey waves **2.28×** ·
G 4 groups at n = 21 483 1.53× (0.29 s total) · S 2 outcomes 1.87× · S 4 outcomes **2.86×** ·
S 3-model comparison unbalanced 1.32× / balanced **2.33×** · R 2 outcomes × a models list 1.89×.
⚠ **Balance, not unit count, is the variable**: the same axis at the same size gives 1.23× over four
uneven groups and 2.28× over eight even ones. And **transport is not the obstacle**, which had been
assumed: shipping the 16 MB fixture is **0.05 s** and a warm 4-task round-trip **0.003 s** — the
1.67 s is the first dispatch's connection setup, once per pool. So the axis is bounded by Amdahl and
by balance, not by serialisation. (The decisions doc's "transfer is the killer", 6.8 s, is a 161 MB
fixture.)

**WHAT SHIPPED.** Both verified `IDENTICAL` by `dev/verify_reg_specs.R` over all 290 cases.

- **The crude-block de-duplication — 20f-i's finding repeating one axis out.** In COMPARISON mode
  every input to `reg_stage_empirical()`'s loop is table-wide or per-*outcome*, and a models list is
  refused unless it has exactly one outcome — so specs 2..S recomputed spec 1 **exactly**, and only
  spec 1 was read (`reg_stage_assemble()` takes `emp_by_fit[[1]]` as every column's `obs` *and* as
  its gap-test crude leg). Its one other reader, `reg_stage_tips()`'s numeric block, emitted
  duplicate rows for a column name every spec resolves identically, which `tab_export_prep()`'s
  `match()` discarded first-wins. Fixed with the idiom the `add_n` loop 70 lines up already uses,
  `if (i > 1L && n_outcomes <= 1L) break`. Measured on a 3-model comparison: **`reg_empirical`
  3 → 1, `reg_empirical_fit` 3 → 1, `reg_fit` 9 → 5** — and **unchanged at 2 / 2 / 6** for two
  outcomes, which is the half of the contract that says it did not over-reach (two outcomes are two
  genuinely different crude blocks). Worth more than the pool would have been on that same shape.
- **A latent defect beside it**: `compare` was gated **nowhere**, so
  `outcome = c("a","b"), stats = "compare_baseline"` reached `reg_compare_rows()` with two different
  responses — `anova.glmlist`'s own `sameresp` filter silently dropped a model and the surviving row
  was labelled with `specs[[1]]`'s outcome. One `cli_abort` at H24 in `reg_resolve_args()`, the first
  point `compare` (S1) and the resolved `prep$outcome` (S2) are both known. A models list already
  refused several outcomes; this is the same fact for the one-model-per-outcome shape.

**WHAT IS NOT BUILT, and what it would cost** (§6.5, each constraint read in the code): the S axis
needs the six per-spec loop *bodies* lifted out of the table-scalar stages into one
`reg_spec_build()` returning a declared product — "20e one grain finer", ~580 lines over 6 files.
⚠ `reg_compare_rows()` **cannot be ported**: it needs two fit *objects*
(`stats::anova(m_lo, m_hi)`, the `method = "Wald"` → `regTermTest` arm on a survey fit), and
re-implementing survey's Wald arithmetic would make tabxplor a second producer of a survey quantity
— the same class as 20f-i's measured `drop1` vs `anova` divergence (12.47 against 14.25). It stays
and **forces the serial path**, which is a fact about the statistic, not a limitation: a
between-model test needs the models together. It returns early on `compare == "none"` (the default),
so that excludes much less than it sounds. Also serial: comparison mode *with* a crude block (spec
1's block is every column's `obs` and carries the 60–100 MB frame) and a compound formula (the
shared skeleton comes from `fits[[1]]`). ⚠ And any per-spec design turns the message stream
**stage-major → spec-major**, so `verify_reg_specs.R` stops printing IDENTICAL for multi-spec cases
and prints *"(same set, different ORDER)"* — detectable, not silent, but the one irreducible price.

**HONEST CONCERNS.**

- ⚠ **The phase's headline question got two answers, and the second is the one to act on.** Measured
  against the ≥2× bar, G and R clear it only for an *even* axis at survey scale where the whole
  saving is ~1 s, while **S — the axis that cannot be dispatched as written — is where the 2×+
  shapes are** (2.86× at four outcomes). That is the opposite of the a-priori ranking, which had G
  as the promising one because it was the easy one.
- ⚠ **Every ceiling is an upper bound that no implementation reaches**, and the `sum units` term is
  itself an over-estimate (a unit built alone re-runs the argument boundary the real call runs once —
  it exceeds `whole` on five of the eight rows, which is why the ceiling is clamped at `max unit`).
- **The harness cost three self-inflicted re-runs** and the reasons are in its header: `tab_vars`
  needs a factor (`year` is integer), the S fixtures needed a `grepl("dem", ...)` that gss_cat's
  lowercase levels actually match, and ⚠ **`Rscript` reads a file incrementally, so editing the
  harness mid-run corrupts the run** — a whole measurement was discarded to that.
- **No golden churn** (no `tab()` path is touched) and the jamovi path is untouched, so no
  `jmvtools::prepare()` is needed — 20g still owns the outstanding rebuild.

**FOLLOW-UPS.** The S-axis restructure is its own session, on 20e's precedent (a pure structural
refactor proved by one harness) — and ⚠ it must re-save `dev/verify_reg_specs.R`'s baseline first and
declare the message-order delta. 20h: `reg_global_rows()`'s `drop1` refits (still routed from 20f-i),
`reg_stage_tips()`'s two halves, and the deprecated-call corpus sweep.

---

#### Phase 20f-iii — the S axis: `reg_spec_build()`, and the parallelism it unlocks

**DONE (2026-08-16), both halves.** Full suite **FAIL 0, WARN 58, SKIP 4, PASS 6959** — the
inherited warning count exactly, and +39 assertions from this phase's own two fixtures.
`dev/verify_reg_specs.R` reports **9 of 290 cases changed, all in ONE declared way** (below), and
⚠ **ZERO cases with a message-ORDER change** — the price the plan of plans and the study both said
was irreducible was not paid at all, and that is the phase's most useful negative result.
`test-parallel-parity.R` green **unsandboxed** (PASS 20) with a new case per axis.

**`reg_build()`'s per-MODEL half is ONE declared product.** Six stages carried their own
`map(specs, …)`, plus `_assemble`'s per-column `reg_set_obs()` reading `fits[[fi]]`, so *"which
parts of the table are per-model and which are between-models"* took four files to answer. Now
**`R/reg-spec-build.R`**: `reg_spec_build(i, ctx, emp_shared)` → `new_reg_spec_product()` (the
fourth record constructor, same idiom), doing in one place what one model contributes — fit,
columns, GOF / global / check rows, its `add_n` count, its observed block, its `obs`/`gap_se`, its
two tooltip fragments. `tab_reg.R` **4762 → 4573**; the stages above it are cross-spec **assemblers**,
and `reg_stage_columns()` / `reg_stage_empirical()` are gone into it. Details in the Repository Map.

**THE PAYLOAD RULE, and why it is what made the S axis dispatchable**: the product carries no fit
and nothing referencing one. **Two exceptions, each declared and each identical to a reason that
shape is serial anyway** — `fit` (sole consumer `reg_compare_rows()`) and the crude block's
60–100 MB `$frame`/`$fits`, kept only for the block SHARED with the compared models
(`reg_emp_slim()` drops them elsewhere). **`reg_specs_independent(ctx)`** is the one predicate —
`NULL`, or the *reason*, reported only when `parallel` was explicitly asked for — and its three
reasons are facts about the statistics: a comparison is a test BETWEEN fits · compared models share
spec 1's observed block · an all-coefficient compound formula reads its skeleton off the first fit.

**`parallel` is now a shared argument of both producers (KEY 4), over all three axes** — the models
of one table (S), the `tab_vars` groups (G) and the outcomes of a multi-outcome recursion (R) — and
`R/tab-parallel.R` **needed no change**: `tab_pmap()` was already generic, so the option, the worker
count rule, the pool, `tab_parallel_stop()` and 20f's condition relay are the same ones. jamovi is
serial by construction (`tab_parallel_workers(cache_env =)`), with no new rule.

**Three producers became per-spec** — `reg_gof_tibble()` → **`reg_gof_rows()`**, `reg_global_rows()`,
`reg_check_rows()`. Each had exactly ONE caller and each loop body was already a pure function of
`i`; leaving them vectorised and calling them with singleton lists would have been a half-migrated
representation.

**Two loop-carried de-duplications became a DECLARED per-spec plan** (`reg_stage_setup()`'s
`spec_plan`): `add_n`'s "one count column per distinct outcome" `break`/`next` pair, and 20f-ii's
crude-block `break`. A worker cannot reproduce a loop-carried skip, and a reader had to simulate one.

**Two placeholders**, because a worker cannot know post-`make.unique()` facts: the footer rows' `col`
(rewritten wholesale per product — every row of one model shares one) and the tooltips'
`(column index, skeleton row)` pair. The second also **freed the tooltips from needing
`reg_stage_rows()` to have run**, which is what let the row axis move after the loop.

**THE DECLARED DELTA, and it is not the one that was expected.** 9 of 290 harness cases changed, all
in the same way: an ABORT inside the fit loop no longer wears purrr's `i In index: N. Caused by
error in ...` wrapper, because that loop is no longer a `purrr::map()`. The message is strictly more
direct (`"score" scores must lie in 0..4` instead of two frames of context), and all nine are
self-identifying (each names the variable). ⚠ what is LOST is `i With name: m1.` on a models list —
an error in the 2nd model of a comparison no longer says which model. Recorded rather than papered
over. **No message ORDER moved**: the only per-spec emitters are the fit itself,
`reg_marginal_basis_warn()` and the Brant test, and no harness case combines two of them across two
specs.

**MEASURED** (`dev/benchmarks/results_2.0.0/phase20f2_*_20fiii.*`, harness gained a section 1d
"THE ACHIEVED SPEEDUP" beside 20f-ii's ceilings): the S axis delivers **2.93×** on four
outcomes, **2.74×** on three balanced models, **2.20×** on the R axis (2 outcomes × a models list)
and **2.03×** on two outcomes; the G axis **1.93×** over eight EVEN survey waves and **1.08×** over
four uneven race groups — its balance figure, confirmed. ⚠ five of the eight rows come out ABOVE
§6.2's ceiling, which is that ceiling's own conservatism (a unit built alone re-runs the argument
boundary), not a contradiction. ⚠ at teaching scale the question is moot: 0.25 s → 0.23 s, which is
why the doc sentence names the shape that pays.

**HONEST CONCERNS.**

- ⚠ **The product's `fit` slot is conditional** (populated only when `compare != "none"`). Declared
  with its single consumer and single reason, and unreachable otherwise because the same condition
  forces the serial path — but a conditional slot is the kind of thing that bites, and it is the one
  place the record is not uniform.
- ⚠ **A worker's ERRORS are not relayed like its messages.** `tab_pmap_trampoline()` catches
  conditions deliberately and not errors (mirai's `[.stop]` re-throws the first). So under
  `parallel`, a failing model surfaces with a different call stack than serially, and with several
  units it may be a *different* model's error that is reported. Pre-existing on `tab()`, inherited
  here.
- ⚠ **`reg_stage_specs()` ships the WHOLE ctx** as `tab_pmap()`'s one `.ship` element. That is the
  right split (everything big — `data`, `skeleton_data`, a prebuilt design — is inside it, sent once
  per dispatch) but it means a future ctx key holding something enormous would travel silently.
- **The parallel payoff stays narrow, and the doc sentence says so**: the pool costs ~1.6 s on its
  first dispatch, so `parallel` is a loss below roughly 5 s of work, and two uneven units cannot
  reach 2× at all. `parallel_min` stays `2L` — `tab_reg()` obeys the argument as `tab()` does
  (maintainer's ruling) — so it is the doc, not a heuristic, that carries the caveat.
- **`R/` grew ~+250 net** (+433 for the new file, −189 from `tab_reg.R`, ~+6 elsewhere).
  Expected, and not the metric: what moved is that the per-model half of a regression table is one
  declared object, and the file that used to hold it lost the six loops.
- **The multinomial-comparison grid rebuild was left alone.** In a multinomial model comparison with
  `empirical`, specs 2..S have no crude block of their own, so each tooltip block re-runs
  `reg_empirical()` — 20f-ii's §6.4 redundancy, one shape further in. Reusing spec 1's grid would be
  byte-identical only if every spec resolves the same `y_ref`, which holds today but is not stated
  anywhere; **routed to 20f-iiii** rather than assumed -- where lifting the crude block out of the
  loop entirely (it is the OUTCOME's, not spec 1's) would delete it for free.
- The 58 warnings are the pre-existing step-API deprecations; the corpus sweep is still **20h**'s.

No `.a.yaml` / `.u.yaml` was touched, so **no `jmvtools::prepare()` is needed** — but ⚠ **20g owes
one UI item**: nothing exposes `parallel` in the jamovi panels, and nothing should (the live cache
forces serial), so what 20g must NOT do is add a checkbox for it.

**FOLLOW-UPS.** 20g can start on this commit. **20f-iiii** (new, added this session) owns everything
this phase routed and everything it left honest: the three refusals of `reg_specs_independent()`,
the worker-error relay, the multinomial-comparison grid rebuild, `reg_global_rows()`'s `drop1`
refits (from 20f), and the product-slot census. `reg_stage_tips()`'s two halves (routed here by 20e)
are CLOSED — 20f-iii split them into `reg_spec_tips_mnl()` / `reg_spec_tips_num()`. 20h keeps the
deprecated-call corpus sweep.

---



#### Phase 20f-iiii — the reg framework: finished, and CLEAN under parallelisation

**DONE (2026-08-16), all three parts.** Full suite **FAIL 0, WARN 58, SKIP 4, PASS 6987** — the
inherited warning count exactly, +28 assertions. `dev/verify_reg_specs.R`: **3 of 290 cases changed,
all in the ONE declared way** (below); every spec, `reg_call()`, column attribute, label and test key
IDENTICAL, and every other message identical **in order**. `test-parallel-parity.R` green
**unsandboxed** (PASS 26) with three new cases.

**THE THREE REFUSALS, each MEASURED before it was kept or removed** — 20f's own rule that *"keep it,
with the number written down" is a complete answer*. Study: `dev/tabxplor_reg_performance.md` **§8**.

1. **`compare != "none"` — DECLARED KEEP, with the number.** 20f-ii measured *transport* at 0.05 s
   for a 16 MB frame, which made the refusal look like caution. Measured on the fits themselves it
   is not: **one `reg_fit()` result serialises to 162.4 MB at n = 200 000** — `$fit$model` 94.7 MB,
   and `$fit$family` / `$formula` / `$terms` **87.8 MB each**, because a family object, a formula and
   a terms object each close over the frame they were built in. Three fits ≈ half a gigabyte. The
   `reg_compare_digest()` alternative would re-implement `anova.svyglm` → `regTermTest` (hard rule
   5), so it was not attempted.
2. **`skeleton_deferred` — KEPT, and UNREACHABLE from `tab_reg()`.** `compound` is only ever
   `formula_mode`, which refuses `predictors` and takes one bare LHS ⇒ **exactly one spec** ⇒
   `reg_specs_independent()` returns at its `length(specs) < 2L` guard first. And
   `reg_crude_key(compound = TRUE)` is `NA` ⇒ `empirical` is turned off at the boundary, so a
   deferred skeleton and a crude block **cannot co-exist** (now a build-time `stopifnot`). It stays
   as the invariant for a direct `reg_build()` caller. A fit-free twin was measured and refused: it
   diverges per fitter and would need a second producer of `reg_fit()`'s frame preparation.
3. **the crude block — REMOVED**, and it was a *modelling* mistake, not a payload one.

**A2 — the observed (crude) block belongs to the OUTCOME.** `reg_stage_crude()` builds it once,
before any model, for every one-outcome table; a several-outcome table keeps its blocks per spec,
where each spec IS an outcome and the work stays on the parallel axis instead of serialising into a
pre-pass. It is **fit-free**, which is what makes it liftable — the two facts it read off the model
have exact producers of their own (`reg_positive_level()`, the function `reg_prep_binary()` itself
calls to order the levels; and the outcome's first level, which `reg_crude_yw()` already collapses
any foreign `ref_category` to, so **`y_ref` moves no number, provably**). Details in the Repository
Map. What it deleted: `share_crude` · the `emp_shared` argument and hand-down · the loop's last piece
of carried state · `reg_emp_slim()`'s three-slot allow-list (a block leaves as its COLUMNS, so the
payload rule has **one** exception left) · refusal 2 · and **§7.4's first routed redundancy**,
`reg_spec_tips_mnl()`'s second `reg_empirical()` producer, whose "byte-identical only if every spec
resolves the same `y_ref` — true today, stated nowhere" caveat is a `stopifnot` now.

⚠ **Two traps the design pass caught before they shipped**: the NUMERIC tooltip must stay on the
block (reading `own %||% crude` would make specs 2..S re-emit identical `Obs_*` rows — exactly what
20f-ii deleted), and `want_crude` must **not** be gated on `!at_profile` (under
`effect = "at_reference"` the block is still built; only `obs` is withheld).

**THREE SILENT DEFECTS, found by measuring refusal 3** — all in the compound-formula path, all with
fixtures. Measured through `tab_reg()`: `party3 ~ race * age` built **4 rows instead of 7** and
`inc3 ~ race * age` **5**, both losing `age` and both interactions.

- ⚠ **`reg_fit_multinom()` / `reg_fit_ordinal()` never saw the user's formula.** They BUILT one from
  the bare predictors, so a compound formula was silently reduced to main effects — the interaction
  left the **model**, not merely the table. `reg_fit_formula()` is the one rule now (the glm arm's,
  which fitted `formula` verbatim all along). ⚠ both then need `environment(fml) <- environment()`:
  they store their call and re-evaluate it (`model.frame.multinom`, which `reg_skeleton_from_fit()`
  reads), so a formula carrying the user's environment resolves `fml` nowhere.
- ⚠ **`reg_skeleton_from_fit()` indexed `names(coef(fit))` with the model matrix's `assign`.**
  `coef()` is a **matrix** for `nnet::multinom` (`names()` NULL → every non-pure-factor term produced
  zero rows) and **one short** for `MASS::polr` (no intercept, while `model.matrix()` has one). It
  reads `colnames(model.matrix(fit))` now — the vector `assign` indexes by construction, identical
  for lm/glm/svyglm, which is why this went unseen.

**B — the worker error relay** (`R/tab-parallel.R`, both producers). `mirai_map(...)[.stop]` re-threw
mirai's own wrapper *before* the condition replay, so a failure **discarded every message the
successful units had already produced** — the diagnostics that explain it — and what surfaced was a
`miraiError`, not the worker's condition. The trampoline catches its unit's error and returns it on
the payload; `[]` collects; the replay runs up to and **including** the failing unit (serially the
ones after it never ran, so replaying them would show output the serial branch cannot produce).
`tab_cnd_strip()` makes a condition safe to send back — ⚠ non-optional, `reg_fit()`'s
`do.call(survey::svyglm, list(fml, design = ...))` puts the whole design in the error's own `call`.
`reg_spec_build()` wraps its body and names the **model's label**; `tab_pmap(.names =)` names the
unit, **in the serial branch too**, so purrr's `i In index: N` is gone and both branches say one
sentence. ⚠ the de-duplication is by NAME, not by class: the axes nest, so an inner failure
legitimately gains an outer name (`Build failed on "score". Caused by: Model "m1" could not be
built.`).

**C — the declarations.** The **nesting rule** is stated and enforced once, in `tab_pmap()`'s
`everywhere()` block (the option snapshot's `^tabxplor\.` regex was **shipping the user's
`tabxplor.parallel` into every daemon**; the three `parallel = FALSE` unit sites masked it only
because an argument beats an option — they stay as defence in depth, each a one-line pointer).
⚠ A helper function was rejected: the three sites assign three different shapes, so it would be
three overloads of one word. Two **declared keeps** where they sit: `reg_global_rows()`'s `drop1`
refits (the only cheaper route is a Wald test, a *different number* — 20f's `drop1` 12.47 vs `anova`
14.25 precedent) and `reg_interaction_rows()`, the fourth fitting site, which lives after the split
barrier and needs the POOLED data, so it is a different question rather than a missed axis. **Census
deletions**: product slots `nobs` / `y_ref`, ctx keys `fit_ncol` / `fit_of_col` (zero readers
package-wide), `emp$crude_key` (write-only), and a stale comment claiming two consumers read
`fit_of_col` — both use `fit_first_idx`. ⚠ `emp$fac_preds` was a census candidate and is **alive**
(`reg_set_obs()` reads it); said so in one line.

**A real jamovi hole, closed**: in **staged** mode `jmvtab_reg_build(use_cache = FALSE)` passes
`.fit_cache = NULL`, so `parallel` fell through to the option — a user who had set
`tabxplor.parallel` would have jamovi spawning daemons inside its own R process, for a UI that
repaints on every click. One word, plus a fixture pinning both cache modes.

**THE DECLARED DELTA: 3 of 290 harness cases, all multi-spec aborts**, each now naming the model
(`Model "m2" could not be built. Caused by: …`) instead of an anonymous error or purrr's
`i In index: 2.`. All three are strictly more informative, and the nested case now names **both**
the outcome and the model.

**MEASURED** on the shape A2 un-refuses (three models on one outcome with `color = "adjustment"`,
n = 200 000): **9.06 s serial → 6.08 s with `parallel = 3`, 1.49×**. ⚠ Lower than the 2.74× the same
three models reach *without* a crude block, and the reason is Amdahl, not the dispatch: the block is
now a serial pre-pass, so the univariable crude fits do not ride the pool. Building it per spec
instead would parallelise three identical copies of one answer — which is what 20f-ii deleted. The
honest reading is *refused → 1.5×*, with the remaining serial share the correct answer to "how many
times should this be computed".

**HONEST CONCERNS.**

- ⚠ **The compound-formula fix CHANGES NUMBERS** on multinomial and ordinal tables that use the
  escape hatch: they now fit the model the user wrote. Nothing pinned the old behaviour (it would
  have been pinning a bug) and no harness case covers that combination, so the fixture in
  `test-tab_reg.R` is the only guard — it asserts the row axis matches the glm arm's on the same RHS.
  This was **not in the phase brief**; it was found by measuring refusal 3 and fixed because leaving
  it would have made the skeleton fix "work" only by matching a wrong model.
- ⚠ **`reg_stage_crude()`'s block rides the shipped ctx** to every daemon (`$frame` + `$fits`,
  60-100 MB at survey scale) whenever a models list dispatches with `empirical`. Shipped whole and
  measured rather than pre-emptively slimmed — the 1.49× above is *with* that cost. A
  table-scalar slimming rule is written down in the plan if a future measurement wants it.
- **`reg_specs_independent()` still names two reasons**, not one. The phase's metric asked for
  "NULL for every shape a user actually builds, or ONE reason with a measurement" — refusal 2 is
  provably unreachable from the public API, so a *user* meets exactly one; the second survives as an
  internal invariant, which is a different thing from a user-facing exception.
- **A worker error has no backtrace** (`tab_cnd_strip()` removes it, and it must). The unit's name
  replaces it. Stated where the stripping happens.
- The 58 warnings are the pre-existing step-API deprecations; the corpus sweep is still **20h**'s.

No `.a.yaml` / `.u.yaml` was touched, so **no `jmvtools::prepare()` is needed** — 20g still owns the
outstanding rebuild, and ⚠ still must NOT add a `parallel` control (the module is serial by
construction, now in code rather than in a comment).

**FOLLOW-UPS.** 20g can start on this commit. 20h: the deprecated-call corpus sweep, and the two
items this phase re-declared rather than removed (`reg_global_rows()`'s refits, and whether the
shipped crude block ever needs slimming).

---

#### Phase 20g-ii — jamovi: the level-collapse UI (★)

**DONE (2026-08-17).** Full suite **FAIL 0, WARN 58, SKIP 4, PASS 7182**, against the inherited
FAIL 0 / WARN 58 / PASS 7138 — same warning count, +44 assertions, nothing red. All four harnesses
print the declared result: `dev/verify_reg_specs.R` **IDENTICAL** over 290 cases (against a baseline
saved from a pristine worktree of HEAD) · `dev/verify_color_attrs.R` **IDENTICAL** (293) ·
`dev/verify_tab_args.R` **IDENTICAL** (167 resolver cases + 52 tables + 30 messages) ·
`dev/verify_golden_field_delta.R` *"only the declared addition differs"* over 1 788 cells × 36
goldens — i.e. **zero golden churn**. `Rscript dev/generate_jamovi_js.R check` clean.
**And the module is not left inert**: `jmvtools::prepare()`, `i18nUpdate("fr")` and
`install(home = "flatpak")` all ran on this box, so the `.h.R`, `0000.yaml` and `inst/i18n/fr.json`
in this commit are the compiler's own. Only the live click-through is the maintainer's.

**THE FEATURE**: a tick-box beside each level meaning *"merge this level into the one above"*,
chained ticks collapsing a run, and one text box spanning the run to name the merged level — the
last ★ of Phase 20, and one of the two things every real crosstab user does (the other, lumping by
count, has been `other_if_less_than` since 1.x).

**THE R HALF IS ONE OPERATION, PRE-AGGREGATE** (the maintainer's ruling, and it earns its keep):
`R/row-model.R` owns the declared spec (`new_lvl_collapse()`, in `forcats::fct_collapse()`'s own
shape), `R/tab.R` the applier (`tab_collapse_levels()`), called from `tab_prepare()` one line
**before** `tab_lump_others()`. Details in the Repository Map. Two consequences worth stating:

- **it is provably `tab()` on a frame the user collapsed themselves** — the fixture is an
  `expect_equal` against exactly that, on both producers — so pct bases / `tot_n` / `n_eff` / the χ²
  all follow with no code of their own;
- **it costs a tier-1 jamovi cache MISS per merge edit**, and that is asserted, not tolerated. The
  alternative (collapse the aggregate keys *and* the microdata) is two implementations of one
  operation, and the design-based `n_eff` path reads microdata anyway.

⚠ **The cache hazard, and it would have been silent.** `ce$fp_map` is fingerprinted in
`jmvtab_build()` **before `tab()` runs**, on the raw columns, so a pre-aggregate merge moves no
fingerprint: without naming the spec in the tier-1 keys the module would have served the un-merged
aggregate. The tier-2 test key names it too, explicitly — it would ride the tier-1 keys in every
ordinary shape but not when every col_var is a self-crosstab, and **a merge changes the χ²'s df and
p where a reorder does not**. Tier 3 needed nothing (`structural` is a negative set), which is the
same property that made `levels_order` free in 7g-ii.

**THE UI IS ONE CONTROL, because reorder and merge are one object**: a merged run is a run of
CONSECUTIVE levels *in the order the user chose*, so a separate widget would have had to mirror this
one's order. `levelOrderCtrl` → **`levelsCtrl`** (a control that merges cannot go on being called
"levelOrder"), its list is now a 3-column CSS grid `[level | tick | merged name]` with the name box
placed by `grid-row: … / span k`, and **`renderTree()` gives each axis a FULL-WIDTH row** instead of
the two-column grid — the "whole-panel change" the brief flagged, and what gives the name box room.
A tick belongs to the **LEVEL**, not the position, so a move simply re-forms the runs.

**THE SHARED BLOCK.** `dev/generate_jamovi_js.R` gains a second marker pair, `BEGIN/END SHARED`, and
`splice()` is generalised to named blocks: the merge list is written once in `jmvtab.js` and
**copied verbatim** into `jmvtabreg.js`, where it hangs off each factor predictor's reference row
(merge only — `ref` picks the baseline explicitly). It is the first *copied* rather than *generated*
block, and it rides the same `check` mode, which is why ~120 lines of export/subtext/CSS helpers
already duplicated between the two files did not become ~230.

**FIVE DEFECTS / GAPS, each with the fixture that fails without it.**

1. ⚠ **The name rule passed `levels_collapse` only by COINCIDENCE**, and the gate said so on its
   first run: `startsWith(nm, "levels_")` matches because `tab()` has a `levels` formal, not because
   `.levels_order` exists — and `tab_reg()`, which has no such formal, made it fail. The rule reads
   the **declaration** now (`TAB_ARGS`' internal dot-args, de-dotted), so both pass by intent and a
   rename of `tab(levels =)` cannot silently remove the justification.
2. ⚠ **`JMV_TAB3_REAPPLIED ⊆ names(opts)` was asserted nowhere**, although the ⚠ beside that vector
   has said since 19k that a misspelt name silently sends its option into the base key — which *is*
   D12. Hoisted to a constant, one fixture — **and it immediately found `n_min` missing from the
   `jmv_opts()` test fixture**, i.e. the fixture's tier-3 key was not the real one's.
3. ⚠ **A `var` capture in a `while` loop**: every merged-name box's `change` handler would have read
   the LAST box created, so typing in one run's name would have renamed another's. Caught on review
   (there is no JS engine on this box); both `box` and `levels` ride the IIFE now.
4. **`man/jmvtab.Rd` / `man/jmvtabreg.Rd` were stale since 20g-i** — that phase ran `prepare()` but
   not `document()`, so the reference pages still listed `refLevels` / `levelOrder` / `method_*`.
   Fixed by this phase's own `document()`.
5. **`jmv_order_after_collapse()`** — not a defect but the design's sharpest point, and it would have
   been one if missed: the tick list shows the **SOURCE** levels (it must, or a merge could not be
   undone), so the JS writes a RAW order, while the table's levels are the merged ones. Without the
   mapping, `jmv_relevel_cols()`'s `ord[ord %in% levels(f)]` would drop every merged level's raw
   names and a reorder would silently revert to the collapse's own positioning.

**HONEST CONCERNS.**

- ⚠ **The JS is unverified by any engine.** No `node`, no `V8` on this box (the standing "there is no
  JS syntax gate and there cannot be one here"). What was done instead: a bracket-balance check, a
  full re-read, and the two `var`-scope traps fixed by inspection. **The live click-through is the
  real gate**, and it is the maintainer's — specifically: tick a level, chain two, type a merged
  name, move a level into a run, and check the reference drop-down offers the merged label.
- ⚠ **Moving a level INTO a merged run splits it and drops the merge.** Deliberate: a run must be
  contiguous in the displayed order, and keeping a non-contiguous group behind a display that shows
  the levels apart is worse. The ticks disappear where the user can see them. Stated in the code.
- ⚠ **`grid-row: … / span k`** is the one genuinely new DOM technique here. If it misbehaves in the
  Electron panel the fallback is the name box on the run's first row with a bracket down its side —
  decide by looking, not by guessing.
- **`.levels_collapse` is INTERNAL**, on both producers (the maintainer's ruling, confirmed this
  session against the differentiator-4 argument). The R route is `forcats::fct_collapse()` before
  the call, which the pre-aggregate placement makes *exactly* equivalent — and `?tab`'s `.cache`
  block now says so, as `.levels_order`'s sentence already did for `fct_relevel()`.
- **Merging every level of a variable** leaves a 1-level factor (χ² df 0). Allowed, not aborted.
- **`JMVTAB_CACHE_SCHEMA` 18 → 19**: the stored VALUES are still readable, which is precisely why
  stale entries must be discarded rather than evicted — a pre-20g-ii entry would be served for a
  merged table.
- The 58 warnings are the pre-existing step-API deprecations; the corpus sweep is still **20h**'s.

**FOLLOW-UPS.** 20h can start on this commit. Owed to it: the live click-through's findings, and
`test-jamovi-vocabulary.R`'s remaining blind spot — `ui[nm]` bracket access (`applyVarEnables`,
`bottomAlignInRow`, `arrGet`/`arrWrite`, `applyModelEnables`) is invisible to the `ui.<name>` regex,
so a rename of `totaltab_*` / `comp` / `xl_replace` would still no-op in silence.

---

#### Phase 20g-i — jamovi: the boundary speaks the 2.0.0 vocabulary

**DONE (2026-08-17).** Full suite **FAIL 0, WARN 58, SKIP 4, PASS 7138**, against the inherited
FAIL 0 / WARN 58 / PASS 6987 — same warning count, +151 assertions, nothing red.
`Rscript dev/generate_jamovi_js.R check` clean. **And the module is not left inert**:
`jmvtools::prepare()`, `i18nUpdate("fr")` and `install(home = "flatpak")` all ran on this box, so
the `.h.R`, `0000.yaml` and `inst/i18n/fr.json` in this commit are the compiler's own and the
renames are LIVE. Only the live click-through is the maintainer's.

**THE RULE THIS PHASE INSTALLS**: *a jamovi option is named after the producer argument it drives* —
exactly, or as `<argument>_<slot>` when several options fold into one — or it is in a declared
exception list with its reason. That is KEY 1 one layer out, and it is now **checked**.

⚠ **Why it had to be: the gate could not see the Phase 20b/20c renames.** `test-jamovi-vocabulary.R`
compares List **values**; what moved was **argument names** — so it stayed green (166 assertions)
through the reg panel showing `dependent` / `split_var` / `method` / `multiplicator` / `shapes` /
`refLevels` for arguments called `outcome` / `tab_vars` / `ci_method` / `multiplier` / `shape` /
`ref`, and `expect_true("shapes" %in% names(o))` actively PINNED one of them. CLAUDE.md's "goes red
on any rename" was true of renamed *values* only. The differentiator-4 promise — *the UI shows R
argument names on purpose, so a user learns the API by clicking* — was quietly false for six months.
The gate is 304 assertions now and grew three blocks: option NAMES · every `.u.yaml` `optionName:` ·
**every `ui.<name>` in the hand-written `.js`** (the only test that file has ever had).

**It caught two of my own misses on its first run** — `ui.refLevels` / `ui.levelOrder` in
`jamovi/js/jmvtab.js`, which my rename regex had excluded by construction (a negative lookbehind on
`.`). A control naming a dead option fails SILENTLY in jamovi: it renders inert, and every
CustomControl guards with `if (!ui.x) return;`.

**THE RENAMES.** `jmvtabreg`: `dependent`→**`outcome`** · `split_var`→**`tab_vars`** ·
`method`→**`ci_method`** · `multiplicator`→**`multiplier`** · `shapes`→**`shape`** ·
`refLevels`→**`ref_levels`** · `depFamily`/`depModelLevel`/`depTrials`→**`family`** /
**`outcome_level`** / **`trials`** · `compare`+`baseline`→**`stats_compare`** + **`stats_baseline`**,
whose ComboBox values are now the R keys themselves (`compare_baseline`, `compare_sequential`).
`jmvtab`: the four `method_*`→**`ci_method_<slot>`** (⚠ they are named in `jmv_tab3_base_key()`'s
`reapplied` vector, which is D12's exact trap — `structural` is the NEGATIVE set, so a name that is
not an `opts` key silently sends its option into the base key) · `refLevels`→**`ref_levels`** ·
`levelOrder`→**`levels_order`** (the internal argument it drives). `lvs` stays, declared: `jmvcore::
Options` already defines a `levels()` method. ⚠ **A rename discards that option's value in
already-saved `.omv` files** — 19k's accepted precedent, in `NEWS.md`.

**THE TWO OWED CONTROLS.** `stats_checks` (20f's explicit debt: the two checks that refit the model
have been opt-in since 20f and nothing offered them) and `add_n`. `jmvtab_reg_stats()` grew a third
argument and stays the one folder — measured, not assumed: `reg_resolve_stats(c("all",
"compare_baseline"))` already returns `list(stats = "all", compare = "baseline")`, so `"all"`
composes with a comparison key and **no R change was needed**.

⚠ **But the tick-box would have been INERT, and that is this phase's real finding.** The digest fast
path *distils the fit away*, so `reg_check_rows()` asks `reg_checks_for(has_fit = FALSE)` and drops
every fit-based row. Measured on the same model: **9 footer rows without the cache, 5 with it** — so
a single-model jamovi table has never shown `global_lr` / `dispersion` / `influence` /
`collinearity`, and `stats = "all"` would have added nothing. `stats_checks = TRUE` now turns the
live fit cache off for that build (the `use_cache` lever staged mode already uses): the control
means what it says, at the price of a refit per edit, and default-off is byte-unchanged.

**THE SWEEP.** The four `# ⚠ 20g` translation lines and their shims · the retired `"OR"` inside
`reapplied` · `jmvreg_fit_key()`'s `inverse =` member (the retired `inverse_two_level_factors`
spelling — a member NAME is part of the hash, so **`JMVREG_CACHE_SCHEMA` 5 → 6**) · the three
constant label keys, which became ONE `total_names` of the option's own shape (the module
*translates* them — the R option is seeded in English — it does not ask) · two dead commented-out
option blocks and a `# filter` section header sitting over the display+export block · five
commented-out result items in `jmvtab.r.yaml` · two interleaved roxygen blocks in
`jmvtabreg-cache.R` (`jmvtab_reg_mult_vector()` had none at all; half of `jmvtab_reg_staged()`'s sat
above a different function) · the hint reading *"Select a **outcome** (outcome) variable"* · stale
prose naming `reference` / `exponentiate` / `tab_reg(multiplicator =)`.

**THE STALE-NOTE VERDICTS**, both measured rather than carried:

- **The `ci = "cell"` + mixed col_vars divergence does NOT reproduce.** `tab()` and `jmvtab_build()`
  both give `pct_ci pct_ci pct_ci pct_ci mean_ci`. 19j/19k closed it; the note (plan of plans §7.3)
  is deleted and a fixture pins it.
- **`jmvcore::Options` has no setter** (`get`/`has`/`values`/`levels`/`option`/`read`/`eval`, and no
  `set`), so the brief's *"resolve the Documents folder at load and write the actual path into the
  text box"* is **not buildable** from R. Maintainer's ruling: leave the export path alone — the
  Phase-o routing fix already resolves a redirected/UNC Documents and the status line already prints
  the path actually written.

**THE REBUILD, and what it closed.** `jmvtab.a.yaml` declared **13** `display` values while the
generated `.h.R` carried **9**: `{or} ({pct})`, `ctr`, `mean` and `var` were added in 19k *after* the
2026-08-13 `prepare()` and were unreachable in the running module. All 13 are live now.
**i18n, measured rather than waved at**: `i18nUpdate("fr")` pruned **54** translated msgids — **31
already stale** (options retired in 19e/19k/z14 and never swept: `ids`, `strata`, `fpc`, `at`,
`exponentiate`, `AME`…) and **23 from this phase's label renames**, of which I restored all 23 by
carrying the old French across (the translation always kept the English argument name and translated
only the parenthetical, so it is mechanical). Compiled `fr.json`: 203 → **172** translated strings,
the delta being exactly the 31 stale ones. ⚠ One landmine found doing it: an **unescaped** `"` in a
`msgstr` breaks the compiler's PO parser outright (*"Invalid key name"*) — an escaped one in a
`msgid` has always been fine (`other_if_less_than`'s title proves it), so the fix is escaping, not
avoiding the character.

**HONEST CONCERNS.**

- ⚠ **`tests/testthat/helper-benchmark.R` had drifted since 19k** — it still built its jmvtab opts
  with `OR` and `chi2`, options retired then, so the benchmark was silently measuring a *different*
  table. Fixed here, and it is the same disease one layer out: the new gate polices the module's own
  files, not a test helper that hand-mirrors them.
- ⚠ **`test-jmvtabreg-cache.R`'s `reg_opts()` helper needed restructuring**, because an ARRAY option
  and the helper's scalar convenience field now share a name (`family`, `trials`, `multiplier`). The
  convenience ones are `..`-prefixed. A helper rewritten in the same commit as its subject is worth
  saying out loud — the parity assertions themselves are untouched.
- **Nothing was added to `jmvtab`.** Measured: no argument added in 19d–19m lacks a control there.
  The ones still absent — `tot`, `common_totrow`, `spread_vars`, `color_breaks` — are *teaching*
  decisions, not mechanical gaps, so they are reported here for the maintainer rather than added on
  my judgement. `parallel` must **not** get one (the live cache forces serial).
- **The live click-through is still owed** (the maintainer's): both collapse-box trees, the model /
  reference / shape pickers, the new `stats = "all"` box and `add_n`, and export.

**FOLLOW-UPS.** **20g-ii** owns the ★ level-collapse UI, and its design is settled — recorded here
so the planning is not lost:

- **R half — a pre-aggregate microdata recode** (maintainer's ruling). One `fct_collapse()` in
  `tab_prepare()` beside `tab_lump_others()`, i.e. the mechanism `levels = "first"` already uses,
  and **before** the lump so a merged level's combined count faces `other_if_less_than`. ONE
  implementation, provably identical to `tab()` on pre-collapsed data, and pct bases / `tot_n` /
  `n_eff` / the tests all follow. The carrier is an internal `.levels_collapse` beside
  `.levels_order` (in R you write `forcats::fct_collapse()` — the reorder's own precedent).
  ⚠ The brief's *"it is a `tabxplor_lvl` operation"* is not literally right: `tabxplor_lvl` exists
  only on a BUILT table's index columns, while a collapse must change counts. `R/row-model.R` owns
  the SPEC (a declared level operation); the applier belongs to the prepare stage.
- **Cache**: tier 3 is free (`structural` is a negative set, so a new key lands there automatically —
  and the collapse must NOT join `reapplied`); the tier-1 keys and the **tier-2 test key** must gain
  it, the last being the correctness hazard (a collapse changes the χ², a reorder does not).
- **Caveats to state**: a *total* level is synthetic and minted after the collapse (safe under this
  seam); `"Others"` is created by `tab_lump_others()`, so collapse-then-lump ≠ lump-then-collapse
  and the order must be declared; under `levels = "first"` the kept level may become a merged run; a
  stored spec keys on RAW labels (jamovi cleans names at display, `tab()` pre-aggregate).
- **UI**: fold into `levelOrderCtrl` — reorder and merge are ONE object, because the chain follows
  the chosen order — extending `buildVarBody()`, and re-laying `renderTree()` as **one full-width
  row per axis** instead of today's 2-column grid (that is the "whole-panel change" the brief
  flags). `jmvtabreg` gets **merge only**, off the per-predictor `refPickerCtrl` row: reorder is
  meaningless there because `ref` picks the reference explicitly. The shared half is synced between
  the two `.js` by `dev/generate_jamovi_js.R` (a marked block copied from one source of truth, gated
  by its existing `check` mode), never written twice.

20h also inherits: the `tot` / `spread_vars` teaching decisions above, and the deprecated-call
corpus sweep (58 warnings, unchanged).

---

#### Phase 20h — Harvest 1: the deletion pass

**DONE (2026-08-17), all eight steps.** Full suite **FAIL 0, WARN 1, SKIP 4, PASS 7279**, against the
inherited FAIL 0 / WARN **58** / SKIP 4 / PASS 7182 — and the surviving warning is not deprecation
noise but a real over-dispersion notice on a poisson fit, which is exactly what a suite should
surface. **All four harnesses print IDENTICAL** — `verify_golden_field_delta.R` clean over 1 788
cells × 36 goldens **with every declaration block EMPTY**, `verify_color_attrs.R` 293 cases,
`verify_tab_args.R` 167 resolver cases + 52 tables + 30 messages, `verify_reg_specs.R` 290 cases —
so the phase met its own contract: *a deletion pass that moves a value has stopped being a deletion
pass*. `document()` idempotent, `tools::checkDocFiles()` silent, `generate_jamovi_js.R check` clean.

**THE CENSUS, including what did not shrink.** That report is the phase's product, so it leads.

|                                      | before | after         |                                             |
|--------------------------------------|--------|---------------|---------------------------------------------|
| suite warnings                       | **58** | **1**         | the 1 is a real statistical warning         |
| `TAB_OPTIONS$arg` FK `allow` entries | **11** | **0**         | the checkable KEY 8 reward                  |
| inert public arguments               | **3**  | **0**         | all three found by this phase's sweeps      |
| dead formals                         | **15** | **0**         | proved unread, every call site fixed        |
| exports                              | 94     | **94**        | nothing added, nothing removed              |
| `man/`                               | 6 863  | **6 886**     | **+23 — it GREW**, see the KEY 8 correction |
| `R/`                                 | 47 629 | 47 829        | +200, expected (§2: not the metric)         |
| formals per producer                 | —      | **unchanged** | no signature moved this phase               |
| ghost-comment sites                  | 164    | 164           | **routed to 22c**, not this phase's work    |

⚠ **The KEY 8 correction, measured rather than assumed.** The plan's "exporters −125 Rd lines" was
the rejected `tab_style()` bundle's saving and died with it: the exporters keep every formal by
ruling, so each page still documents each argument, and replacing the drifted *short* texts with the
canonical *fuller* ones made `man/` grow. **KEY 8's reward is anti-drift, not size** — 26 hand-written
blocks became 9 declarations, five texts that were wrong or incomplete were corrected, and the
foreign key's eleven-name exception list is empty.

**THE PHASE'S REAL FIND: three inert public arguments** — documented behaviour that did not happen.
Two of them were sitting in the "dead formal, delete it" pile, so the brief's own deletion list would
have buried them. This is the strongest argument for doing the sweeps before the deletions.

1. ⚠ **`tab_plain()` was the ONE crosstab producer of four that never finalised colour.** Measured:
   `tab_plain(color = "difference", color_signif = "grey_non_signif")` stored `"ignore"`, while
   `tab()` and `tab_num()` stored the real value. `finalize_color_tail()` is run by `tab()`,
   `tab_counts()` and `tab_num()`; `tab_plain()` ended at `tab_apply_display()`, so the `color_spec`
   its own argument boundary had already resolved was computed and dropped — and `plain_core()`
   hard-coded `color_signif = "ignore"` while carrying the resolved value in an unread formal, which
   is *why* that formal looked dead. Three declared behaviours silently did not happen: the policy ·
   a legacy composite's POLICY half (`color = "diff_ci"` coloured by the difference and tested
   nothing — 20b's decode-then-normalise warning one layer down) · a two-channel
   `color = c(text, bg)`, which **aborted** inside `plain_resolve()` on a length-2 `if`. Byte-identical
   on all seven pre-existing shapes, and the `df`/`num` escape hatch keeps `num_core`'s early return
   (the tail would otherwise stamp `color_breaks` on a plain frame). The stale comment claiming
   *"tab_plain never finalises colour — the outer wrapper is the sole finaliser"* documented the defect.
2. ⚠ **`lang` was inert on `tab_md()` and `tab_xl()`.** `rd_footer(lang =)` exists and only
   `tab_html()`, `tab_plot()` and `forest_plot()` passed it; `tab_md()` handed it to
   `md_render_one()`, which dropped it under a comment saying so, and `tab_xl()` never read it at
   all. Both now thread it — it fires only when supplied, and nothing in the corpus supplies it, so
   zero churn.
3. ⚠ **A source level literally named `"Total"` was silently turned into a total ROW.** `"Total"` is
   the leaf's declared internal pre-rename sentinel, and `leaf_totrow_tottab()` derives its role
   vectors by matching it — measured, the data row came back with `row_kind = "total"`,
   `is_totrow() == TRUE`, bold, out of the percentage base, and the table printed **two** identically
   labelled "Total" rows. There is no right reading of that table, so it aborts. ⚠ Two homes, both
   needed: `tab_prepare()`'s tail (post-recode, so a collision a recode *created* is caught) and
   `leaf_defuse_vars()` (a direct `tab_plain()` / `tab_num()` / `tab_counts()` call never reaches
   `tab_prepare()` — the gate's own fixture caught that). ⚠ **NOT `"NA"` or `"Others"`**: measured, an
   `"NA"` level renders correctly unless the column *also* holds real NAs, so refusing it would be a
   false positive on an ordinary survey label ("NA" = "not applicable").

**THE DELETIONS**, each re-proved before it was touched and done call-site-by-call-site (19l dropped
one and the i18n tests caught it):

- **the whole DORMANT total-column-range block** — `tab_totcol_range()`, the `range_totcol` model
  slot, both commented-out call sites, four breadcrumbs, the `TAB_OPTIONS` note and the test section
  that existed only to keep the helper alive. With it, `tab_fold_addn_incell()`'s `rng` branch
  collapses to two lines.
- **`materialize_specs()`'s `kind`** — zero readers, and its header claimed it "matched the stored
  row-role vocabulary" when no `ROW_KINDS` value matched. The list is NAMED instead: a name costs
  nothing and cannot make that claim.
- **`tab_transpose()`'s unreachable duplicate abort** — `tab_check_shape()` one call earlier already
  refuses it, and the declared `row_var`/`row_vars` move together, so the "degraded table" case it
  guarded is caught there too.
- **15 dead formals** across 14 functions. ⚠ **One cascade, and it is the hazard of this work**:
  deleting `reg_checks_for(grouped)` made `reg_checks_default(grouped)` dead too, and its call site
  passed three positional arguments — so a naive deletion would have silently bound `grouped`'s old
  value to `has_fit`. **Three KEPT with their reason**: `color_signif_rd(producer)` (the `values_rd`
  calling convention — the formal is the interface), `set_color_style(html_24_bit)` (deprecated-inert,
  documented), `.onLoad/.onUnload(libname, libpath)` (R's contract). ⚠ `jmv_store_fetch()` losing its
  `cfg` makes it the one kernel function without that first argument, so the asymmetry is now
  **declared**: `cfg` is the schema and the byte budget, and only the functions that *decide*
  something read it — a read does not.
- **`REG_ESTIMANDS$obs` was NOT deleted; it got its reader.** It was `FALSE` on exactly the 13
  `at_reference` rows of 43 and the rule was re-derived from the string downstream, so
  `reg_stage_setup()`'s `at_profile` reads the declared column now, with the proven equality asserted
  beside the table. Byte-identical; it turns 19b's "a column with no reader is weight" into a fact
  and removes a re-derivation.

**KEY 8 — `EXPORT_ARGS`** (Repository Map for the design). Two facts worth keeping: a **second table**
is forced, not chosen (three names mean something else on the render side, and a named list cannot
hold two rows per key), and only **9 of 24 rows carry prose** because the table's own admission test
was applied honestly — `@param theme` is written seven times but `allow_auto = TRUE` is passed by only
three backends, so seven texts describe five value sets and are not one duplicate. ⚠ The scope gate
**caught my own error on its first run**: I gave `print` a row on the strength of a `tab_css` formal
that is actually `print_rules`.

**Also landed**: `fmt_materialize_wn()` names the `set_wn(col, get_wn(col))` round-trip, with the rule
(`get_wn()` is the only getter with a fallback, so the write *fixes* it into the record) stated once
instead of twice — 19o §7.4's "state the rule or drop the write", answered without dropping a write
the goldens pin. And **the jamovi gate's bracket blind spot is closed**: `test-jamovi-vocabulary.R`
saw `ui.<name>` but not `ui[...]`, so a rename of `totaltab_*` / `comp` / `xl_replace` / `family` /
`trials` would have no-op'd in silence. The three forms are DERIVED from the sources' own convention
(a literal after `ui`, an array literal whose `.forEach` indexes `ui[...]`, `Object.keys(OBJ)` with
`ui[...]`), never a hand list — 304 → 323 assertions, and every name resolves.

**THE SWEEP, and what it de-duplicated.** 58 → 1. `test-steps-legacy.R`'s 31 are quieted per block
(its subject *is* the deprecated call, and the two blocks that assert the warning keep it); the nine
`tab_chi2` sites in `test-calculations.R` are migrated to `tab(test = TRUE)` after measuring the chi2
row, the ANOVA rows and the contribution sums identical — so those assertions now cover the LIVE
path; and the identical `tab_prepare()` starwars fixture, **written six times in four files**, is
hoisted to each file's top level, where the existing file-level lifecycle line actually bites. Four
blocks keep an in-block quiet with a one-line reason (the step is half the subject).

**HONEST CONCERNS.**

- ⚠ **`man/` grew.** Stated above and in the architecture doc, because the plan predicted a −125 and
  the measurement is +23. The pages that grew are the ones whose documentation was worst
  (`tab_plot` +10: its `color_legend` was "Print colors legend below the table ?" and its `theme` had
  a comma splice and denied `"print"`).
- ⚠ **Three of the four surviving `EXPORT_ARGS` prose decisions are judgement calls**, not
  measurements: `caption` genuinely renders five ways, but `css`, `format`, `file` and `path` are
  *two* texts each and I read them as different rather than drifted. If 22d disagrees, moving them is
  one `doc` each — the row already exists.
- ⚠ **The `"Total"` abort is a behaviour change on data that previously "worked"** — it produced a
  wrong table, but a script that fed such a level now stops. It is in `NEWS.md`. It also fires for a
  pre-aggregated `tab_counts()` input carrying a Total row, which is correct (it would inflate every
  base) but is the one shape a user might not expect.
- ⚠ **The jamovi gate is a regex over source, not a parse.** No `node`, no `V8` on this box (declined
  in 19n); the limit is stated in the test. It gates the NAMING, not the behaviour.
- **`tab_plain()` still has no `color_breaks` formal** (it is declared for `tab`/`tab_num`/
  `tab_counts`), so its new tail passes `NULL`. Deliberate — adding it is surface growth — but the
  four producers are not yet uniform on that one argument.
- **`plain_core()` is still 426 deparsed lines.** The phases are declared, not extracted; the header
  says what the extraction would cost and why it needs its own session.
- **`var_labels` per-call: DECLINED with the number** — 11 functions to thread, 0 corpus uses, and
  the option already works. `pct`'s `"no"`/`"none"` item needed **nothing**: 20b's `stored = "none"`
  field and its foreign key already declare that mapping.

**ROUTED TO 20i** (which is allowed to move a value), all from the guess sweep: `tab-leaf.R`'s
last-level-guessed-as-total fallback when nothing carries the flag · `tab.R:1491`'s
`names() |> last()` picking the total column · the transposed "representative column" chosen by
position, twice · `reg-resolve.R`'s `deps$est[[1]]` choosing the estimand for a whole table · and
**per-row `obs`** (today one `at_reference` spec suppresses the crude value for the *entire* table,
which is over-broad). **Routed to 22c**: 164 ghost-comment sites naming 107 dead functions, almost
all legitimate historical notes. Declared keeps, with their measurements, from 20f/20f-iiii:
`reg_global_rows()`'s `drop1` refits and the shipped crude block.

No `.a.yaml` / `.u.yaml` was touched, so **no `jmvtools::prepare()` is owed**.

**FOLLOW-UPS.** 20i can start on this commit; every harness baseline is current
(`verify_reg_specs.R`'s is re-saved, and `verify_golden_field_delta.R`'s declaration blocks are empty
and should stay so unless 20i adds something). 20i also owns the full-suite + `devtools::check()`
checkpoint that closes Phase 20.

---

#### Phase 20i — Harvest 2: open integration

**DONE (2026-08-17). The honest headline is the phase's product: Phases 19–20 already spent the
large integration refactors — this IS the final 2.0.0 architecture.** Three parallel Explore agents
plus direct reads converged on it: raw-attr discipline is total (zero fmt-attr `attr()` reads outside
the getters), the exported inspectors are legitimate user-views over already-dogfooded machinery
(`tab_supports` twins `tab_check_shape`, `tab_columns` twins `fmt_attrs_of`, `reg_measures` delegates
to `reg_estimand`), the row model is fully exploited, the parallel-rule copies were collapsed in
17i/19i/19k, and the export/plot pipeline is producer-uniform. **There is no missing key to a further
big simplification.** So 20i CLEANS and reaps the framework's simplification rewards. All four
harnesses print the declared result: `verify_color_attrs` **IDENTICAL** (293) · `verify_tab_args`
**IDENTICAL** (167 resolver + 52 tables + 30 messages) · `verify_golden_field_delta` "no new field"
over 1 788 cells × 36 goldens (**zero golden churn**) · `verify_reg_specs` IDENTICAL over 290 cases
(the 3 flagged were a package-dir substring artifact of the baseline method, not a behaviour change).

**Site 5 — per-column `obs` (the one real reachable bug).** `reg_set_obs()` gated on the table-scalar
`at_profile <- any(!... s$est$obs)`, so a multi-outcome table with a MIXED per-outcome `effect`
(`effect = c(a = "at_reference", b = "coefficient")` — `effect` IS per-outcome,
`reg-resolve.R:337`) blanked `{obs}` and `color = "adjustment"` on **b's coefficient columns too**.
Now the gate reads the column's OWN spec (`sp = specs[[i]]`, `reg-spec-build.R:253`):
`if (is.null(e) || !isTRUE(sp$est$obs)) return(col)`. The `at_profile` ctx field is **removed
entirely** (producer / assignment / default — the reward), and `reg_color_notes()` needs no change
(its at_reference note describes the at_reference columns, still accurate). Measured: on a two-outcome
`married`(at_reference) × `widowed`(coefficient) table, `Model_OR [widowed]` keeps `obs`,
`Model_MER…[married]` withholds it; before, both were blanked. Fixture in `test-tab_reg.R`.

**The load-bearing cleanups.** ⚠ **Site 3b** (`tab-export-prep.R:266`): the `emp_tips` var column
already reads the declared `rv$var_col` (19l) — the `[[1]]` extracts the single reg-only column;
comment now states the invariant so it is not a bare positional guess. **Site 1**
(`tab-leaf.R:1111`): `leaf_chi2`'s `tot_cols` was built from a last-column fallback when NO total
column exists, though read only under `if (do_ctr && any(is_tot))` — now built only when a total
exists (the inert guess is gone). **Site 4** (`reg-resolve.R:916`): `deps$est[[1]]` is documented as
the table's REPRESENTATIVE estimand (the per-outcome facts live in `ests`/the specs, so columns and
legend are already per-outcome-correct; only the `reg_call` one-line summary is the first outcome's)
— no code change, columns were never wrong. **Site 2** (`tab.R:1491`) skipped: a resolve-time
decision that PRODUCES the total-column identity, with no earlier declared fact to substitute.

**Two byte-identical DRYs (the reward).** **`fmt_has_role(x, roles)`** (`fmt_class.R`) — the ONE
"is this fmt column one of these roles?" predicate; `get_role` was compared three inconsistent ways
(`%in%` here, `as.character()[1]` in plots, bare `identical()` in tab.R). `fmt_is_helper_col` is now
`fmt_has_role(x, c("n","pct"))`; the `plots.R` count-column scan and the `tab.R` footer-key skip both
route through it. **`tab_last_factor_row_var(fct_names, groups)`** (`tab.R`) — the ONE degraded
"which factor is the row variable?" heuristic, shared by `tab_get_vars()` (no groups → bare last
factor) and `tab_render_vars()` (group-aware); byte-identical to both prior copies, and it fires only
for a table with no declared index.

**TAB_OPS gains a `kind` predicate.** `transpose_object` now refuses a regression FIRST, with a
kind-specific reason pointing at `tab_export(transpose = TRUE)` (which DOES support reg), instead of
the misleading crosstab "needs exactly one row variable" (a reg reads as `merged` via its var-role
predictor column). Message-only on the deprecated object-level `tab_transpose()`; fixture in
`test-transpose.R`. This is the one change that extends 19h/20a's "read it, don't discover it" to the
`kind` axis.

**DEFERRED / NOT DONE — stated clearly:** a further big refactor (**none exists**) · actually
transposing a regression object (a product decision, not integration) · `plain_core()` extraction
(its ~13 `tabs_*` intermediates are a wide data-flow bus all consumed at the Phase-16 carrier build,
not a chain — the declared model gives no leverage; needs a carried struct, a separate measured
session) · the reader-naming convention (doc/cosmetic → **22c**) · the `tab_export()` ↔
`jmvtab_export()` twin switches (cross a deliberately thin boundary) · the `tab_kind()` sniff and
last-factor heuristics (intended degraded contracts, unreachable by the package's own producers).

**THE CLOSE-OUT `check()` EARNED ITS KEEP** — it surfaced **two pre-existing Phase 20b defects
invisible to the test suite** (exactly the 19n lesson: examples and static analysis are not run by
`devtools::test()`), both fixed here:
- ⚠ a **stale `@example` in `?tab_reg`** used `reference = c(party3 = "3-Republican")`, retired in
  20c → for an OUTCOME's baseline that is **`outcome_level`** (the multinomial pivot), so
  `--run-donttest` **ERROR**ed. Fixed (and the one stale usage-prose spot at `tab_reg.R:4397`,
  `reference` → `ref`; the retired-name LIST at `:4578` is correct as-is). `man/tab_reg.Rd`
  regenerated.
- ⚠ a **"no visible binding for global variable" NOTE** (`OR` / `tot` / `color_breaks` in
  `tab_counts`/`tab_num`/`tab_plain`): 20b's `...`-args are bound at runtime by
  `list2env(tab_dots_expand(), environment())`, invisible to R's static checker — declared via
  `utils::globalVariables()` beside the existing `list2env`-pattern declarations.

**HONEST CONCERNS.**
- ⚠ **Site 5 changes numbers on a rare shape** (a mixed per-outcome `effect` multi-outcome table) —
  the coefficient columns now correctly carry `obs`/`color = "adjustment"`. Nothing pinned the old
  (buggy) behaviour and no golden/harness case uses that shape, so the fixture is the only guard.
- **Sites 3b/1/4 are hygiene, not fixes** — 3b was already reading the declared identity, 1 was
  inert, 4 was never wrong. Reported as such rather than dressed up.
- **`man/tab_reg.Rd` moved** (the example fix); exports unchanged — no new `@export`
  (`fmt_has_role`/`tab_last_factor_row_var` are internal).

**VERIFICATION.** Targeted (`transpose|tab_reg|forest-plot|tab-estimates|calculations|fmt_class|
adjustment-gap|between-groups-gap`) **FAIL 0, WARN 0, PASS 1550**. Full suite **FAIL 0, WARN 1,
SKIP 4, PASS 7284** (vs inherited 20h PASS 7279; the 1 warning is the pre-existing real poisson
over-dispersion notice, +5 from the two new fixtures) and **`devtools::check()` Status OK — 0 errors,
0 warnings, 0 notes** on the final tree (after the two 20b fixes above), the close-out that means
Phase 22 does not inherit a broken tree. `document()` regenerated only `man/tab_reg.Rd` (the example
fix). No `.a.yaml` / `.u.yaml` touched → no `jmvtools::prepare()` owed.

**FOLLOW-UPS.** Phase 20 is closed. To Phase 22: reader-naming convention (22c), and the `?tab_reg`
prose trim / `family × effect × measure` generated table (22b/22d) inherited from 20c.

---


### Phase 22 — documentation integration and simplification

#### Phase 22a — Architecture document simplification
- The document must be presented around the real design goals and real-world usage of tabxplor.

#### Phase 22b — simplification and integration
- Document undocumented stuff. `spread_vars` in `tab()` ?
- Vignettes should not be neverending. If some aspects, either expert, or on the contrary pedagogical and near useless to experts, need to be placed in new vignettes, make me propositions. Point to `tab_shape()` · `tab_supports()` · `reg_measures()` · `tab_columns()` · `fmt_attr()`, etc., when relevant.
- Document the family x effect x measure stuff in regression vignettes, in an expert section, adding a clear, very concise and user-friendly markdown table (like for color x type x color_signif in the introduction vignette) stating what combination does what in terms broadly understandable by experts/in glm() terms. It should also be usable for teaching the framework. Look at `REG_ESTIMANDS` and `reg_measures_rd()`.

#### Phase 22c — R scripts comments simplification

I want you to  **drastically** simplify comments, **dividing their global length by at least 4 (measure it)**. Remove most of the archeological stuff, only keep what is actually useful for **future** developement (tabxplor next versions) : what explains design decisions, architecture choices, caveats ; the "why" of the code, the way it integrates in the global functions ecosystem of the package, the way to use this ecosystem to avoid re-adding evergrowing exceptions and white elephants in the future. The past must dissapear : the present ecosystem and usage must be the reference point. Remove all references to tabxplor 2.0.0 dev history (with only a few exceptions if needed) : rewrite everything based on the final design, architecture and real-world usage, for future development to never lose focus on them. Comments must be clear, understandable by both machine and human.
- Dive deep inside the current architecture and framework ; read dev history in `CLAUDE.md` and `dev/tabxplor_2.0.0_roadmap_DONE_PHASES.md` focusing on the last phases, specially the ecosystem integration phases ; study relevant documentation in `dev/` ; make tests on temporary scripts if needed ; study vignettes and dev history in details to understand the real-world use cases of the package, the how is it supposed to be used, and the "why" it’s different from other existing package.

#### Phase 22d — roxygen2 documentation simplication
- Point to the right vignette for more details and pedagogy. Point to the introduction vignette in `?tab` description and the regression vignette in `?tab_reg` description. Start the english vignettes with a link to the French vignette to say it exists, if not already done.

#### Phase 22e — drastic `NEWS.md` simplification
`NEWS.md` `# tabxplor 2.0.0 (in development)` was already drastically simplified in Phase 18y, but have since Phase z2 accumulated all dev history again. Most of it is really not user-facing and irrevelant here (and already in other dev documentation). A **drastic** reduction is needed here, no dev details **at all**, straight to the point, please **reduce around 400 lines to maximum 150 lines** :
- "## New features" should only list the most important things. New exported functions, like tab_counts() and others, should be presented in one quick sentence. New arguments in one quick sentence, rarely more. Everything about `tab_reg()` should be near-zero-words : in spirit, "possibility to do regressions added, see the vignette".
- Drastically reduce "## Changes that may affect existing code", only keep what is really important
- Drastically reduce bug fixes (same thing really), to only speak about very very few bugs that could have been hit by real user. Remove everything about any new argument or implementation. Make it small.
- Keep deprecation, reduce it’s size, list elements quickly, but keep differenciate what is soft deprecated and what is hard deprecated.

#### Phase 22f — Tests simplification
- testthat tests have grown organically, it was right for development, but would slow future dev for no real benefits: I want you to select the tests that are really necessary , and to move the others to a unique script not run with `test`. **The full suite must go below 20 seconds** (parallelised, on this computer).

#### Phase 22g — `dev/` folder and `CLAUDE.md` simplification
Files inside the `dev/` folder have grown organically, with many now useless files and outdated ones, which is very messy for future development : I want you to clean and reorganise the folder and main files.
- Put all files related to v 2.0.0 dev history and of no real use for future dev in an 2.0.0 archive subfolder. That should be most of them.
- Only keep at `dev/` root level a few selected .md files that explain in detail the architecture or functioning or use cases of some subsystems, and will be really useful for future dev : clean these files, simplify them by removing useless dev history and focusing on current architecture and usage, ensure they are up-to-date compared to the current design and code ;  organise them internally in such a way that goals, design and architecture decisions, usage, and everything giving the big picture come first, and details come next ; reference them in the architecture document.

Also simplify CLAUDE.md.

#### Phase 22h — french translation


### Phase 2{x} — release

Procedure: **`dev/release_checklist.md`** (branch mechanics — ⚠ merge commit, never squash; the
strip list; `.Rbuildignore` identical on both branches; tag *after* CRAN acceptance — ⚠ the only
existing tag is `v1.2.0`, 1.3.0 and 1.3.1 were never tagged).
Gate set + the three owed maintainer items (README hero screenshot, `cran-comments.md`, the jamovi
live pass): **§10.2 of `dev/tabxplor_phase20_surface_integration.md`**.

---







### Reference — bugs, benchmarks, perf

Fixed bugs recorded in `dev/tabxplor_2.0.0_roadmap_DONE_PHASES.md`


#### Benchmarks (`dev/benchmarks/`)

The performance harness lives in `dev/benchmarks/` (`.Rbuildignore`'d). Per the scope decision, save every phase's before/after runs under `dev/benchmarks/results_2.0.0/`.

⚠ **Every committed baseline below was measured on WINDOWS/NTFS. Dev is now WSL2 Ubuntu on ext4 — do NOT diff a WSL2 run against them.** Affected: `dev/benchmarks/baseline.csv`, `tests/testthat/benchmark_baseline.csv`, `jmvtab_benchmark_baseline.csv`, `jmvtab_big_benchmark_baseline.csv`, plus every absolute timing quoted in the roadmap phases above (`~46s` suite, `225s -> 56s` parallel, the §26 parallel PoC, the Phase 5/7f/9b/10 speedups). The *ratios* within a single run stay meaningful; the absolutes do not cross the platform boundary. Nothing fails — benchmarks are opt-in (`TABXPLOR_BENCH=true`) and `test-benchmark.R` never fails — so this is a **silently misleading comparison**, not a broken test. Re-baseline consciously on ext4 before drawing any conclusion, and note the platform in the file when you do.

⚠ **The 8M fixtures are NOT in WSL2.** Migration Phase A1 ruled `big_df.rds` (161 MB) and `big_pc18_full_15M.rds` (572 MB) *reproducible* and deliberately did not copy them (`.gitignore`: *"Generated benchmark fixtures: large, regenerable, never commit"*; `gen_big_df.R` is tracked). The first `run_bench.R` therefore **regenerates the fixture first** — expect a long, one-off build, not a hang. The 13 loose `dev/benchmarks/results_*.csv` WERE copied; `results_2.0.0/` is tracked.

- `run_bench.R` — heavy 8M-row `tab()` harness: `source("dev/benchmarks/run_bench.R")`. Compares to `dev/benchmarks/baseline.csv`; writes `results_<stamp>.csv` (git-ignored).
- `run_fused_vs_bytable.R` — fused vs table-by-table arbiter on a 15M fixture (the `.by_table` flag). *(OBSOLETE since Phase 9c removed the tab()-level factor fusion — `.fine` now only reaches `tab_plain` via jmvtab / `tab_counts()`.)*
- `gen_big_df.R` — deterministic 8M fixture builder (cached to `big_df.rds`, git-ignored).
- `baseline.csv` — committed 8M baseline; reset consciously after a deliberate perf change.
- `tab_many_performance_profile.md` — the full 2026-07 profile (read before optimizing).
- In-suite counterpart: `tests/testthat/test-benchmark.R` (small `gss_cat`, informational, never fails, vs committed `tests/testthat/benchmark_baseline.csv`; regen via `dev/make_benchmark_baseline.R`). `bench` is Suggests-only (falls back to `system.time`).

#### Perf findings (condensed — full profile in `dev/benchmarks/tab_many_performance_profile.md`)

- **`tab_chi2` is the #1 cost** (84% of a small 9-tab call; N-independent, scales with *cells*) → the reason CI/chi2 move onto the aggregate in Phase 3.
- Per-table fixed fmt/vctrs overhead (~0.19 s/table) dominates over the scan; `tab_num` double-scans N and weighted `tab_num` allocates ~7.8 GB (`weighted.var` recomputes the mean) → Phases 1-3.
- Scan-fusion — the tab()-level opt-in (`options(tabxplor.fuse_min_rows=)` + the fused block in `tab_aggregate`) was **removed in Phase 9c** (§30): a NET NEGATIVE (+1–7 %) once the build is O(cells) / N-independent, so fusing the O(N) scan buys nothing at survey scale. The `.fine`/`.by_table`/`fine_for_pair()`/`use_raw` seam **remains** as the jmvtab-cache aggregate-injection seam (+ `tab_counts()` + numeric `fine_num`); `test-fuse-parity.R` now drives `tab_plain(.fine=)` directly.

---

## The last step of every implementation, during the final test suite : Update instructions and relevant development files

The final test suite is now quite long, **so you must always start updating documentation and writing "DONE" summary while you wait for the final test suite to finish** (if some tests fails and you modify stuff, briefly correct the documentation that needs to be corrected after verification passes) :

1. Ensure the file-header docstring/comment of any modified module is still accurate. Update or add `# DESIGN:` / `# WARNING:` tags next to changed logic.
2. Update `dev/tabxplor_architecture.md` whenever you modify the package structure *for real* (add modules, rename functions, change config fields). Do not add clutter and useless details. When there is nothing to change, skip it. Update other `dev/*md` file when relevant.
3. **Edit `CLAUDE.md` yourself** — never hand the maintainer "update lines" to paste. Two things go in, both minimalistic, concise, no bullshit, nothing that would clutter the prompt (the details are already in the docstring/comments):
   + the **Repository Map** / *Key Constraints* / *Design Decisions* entries of anything you really changed (a new module, a renamed function, a new config field). When there is nothing to change, *skip it*.
   + the phase **"DONE" summary**, under its own `#### Phase <x> — <title>` header in the roadmap section. **CLAUDE.md is the ONLY place it goes**. The maintainer moves done phases to `dev/tabxplor_2.0.0_roadmap_DONE_PHASES.md` himself.
4. (`NEWS.md`: user-facing and CRAN-facing, tracking new functions, new arguments and arguments changes, deprecations, and really important user-facing bugs fixes. Keep it *fully* minimalistic and *radically* no bullshit. Do not edit it when it’s not necessary. Most of the time, it’s not necessary.)
5. (`README.Rmd` : user manual. Only update before release of new version to CRAN, never before.)
