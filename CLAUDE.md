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
│                              + 19a's **fmt_attr_rules** = HOW each attribute is carried (neutral/merge/arith/scalar,
│                              one row each, meta_bind_rules-shaped) driving all 4 reconstructors through
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
│                              tab_prepare(), tab_spread(), tab_get_vars(),
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
│                              inference_basis_order = the declared weakest-first enum (tab_inference_bind)
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
├── reg-resolve.R    (~980 L) Phase 19m-ii: THE argument boundary of `tab_reg()` -- 19i's
│                              `tab_resolve_common_args()` medicine for the producer that never got
│                              one. **`reg_resolve_args()`** is the ONE entry point (tab_reg() calls
│                              it once and gets **`new_reg_args()`**, new_reg_shared()'s idiom), and
│                              it is six declared stages: **S1 `reg_validate_args()`** (the checks
│                              that are PURE -- and four of them are new: `conf_level` had NEVER been
│                              validated here, `stats` was SILENTLY FILTERED so a typo lost a footer
│                              row, `color_signif` was unvalidated so an unknown policy was STORED on
│                              every column, `baseline`'s shape) · **S2 `reg_prepare_data()`** (the
│                              design unwrap / formula / predictors dispatch / labelled / `shape` /
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
│                              THE ORDER IS THE DESIGN, and the 23 constraints are written out as
│                              `H1`..`H23` where they bind -- three of which were VIOLATED: the
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
│                              `comparison` (the marginaleffects contrast), `status` + the `why` /
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
├── row-model.R      (~255 L) Phase 19f (KEY 1): THE ROW MODEL -- what a row IS, given the same treatment
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
│                              chi2->test rename, **TAB_ARG_VALUES** + tab_validate_args() (the
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
├── tab-leaf.R      (~2595 L) Phase 19l: THE AGGREGATE CORE, carved out of tab.R (whole functions,
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
├── tab-test-display.R (~685 L) Phase 16a: THE shared framework rendering the `test` attribute as an
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
├── tab-parallel.R   (~215 L) Phase 8/9a row-axis dispatch (Suggests-only mirai): tab_pmap() + trampoline,
│                              named "tabxplor" pool (tab_pool_ensure/tab_parallel_workers/
│                              tab_parallel_stop), tab_build_one() (the per-row_var worker, serial OR mirai).
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
│                              tab_md_css() = thin wrapper on tab_css(chrome = FALSE).
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
│                              borders + ugly spacers). chrome-only; tab_md_css() omits them.
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
├── utils.R         (~755 L)  .onLoad() options setup + tx_getOption() (17j: the ONE option-synonym
│                              resolver -- first name set wins, seeded/canonical LAST; backs the
│                              tab_kable_css [was kable_css] rename + the console_theme/export_theme
│                              silent aliases), factor/list utilities, tx_str_wrap/tx_str_trunc
│                              NOT the colour-palette DESIGN tools (preview_color_grid /
│                              simulate_cvd_farver / plot_oklch_hue_strip_cvd / set_luminance...):
│                              they live in dev/color_palette_tools.R and must stay there -- they
│                              are the sole reason the package would depend on farver + colorspace.
├── tabxplor-options.R (~170 L) Doc-only page `?tabxplor-options`: every tabxplor.* global option
│                              (defaults live in .onLoad; keep in sync). Cross-linked from ?tab.
├── tab_reg.R       (~5460 L)  Phase 12c–12h: unified regression tables. 19m-ii moved the ARGUMENT
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
│                              color="OR"). tab_logit()/multi_logit() = thin binomial-family wrappers.
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
├── reg-assumptions.R (~895 L) Phase 18z15: THE model checks of a tab_reg() table, their CURE
│                              (`shape =`) and the primitives its plots draw. `REG_CHECKS` = the fact
│                              table (one row per check: `noun` + `types` = discriminator -> the
│                              INSTRUMENT, both BARE MSGIDS -- a top-level gettext() freezes the build
│                              locale, so reg_check_label() translates at render + a dead-code anchor
│                              keeps potools able to extract; `kind`/`digits`/`families`/`weighted_ok`/
│                              `per_predictor`/`panel`), read by reg_checks_for(what=) = THE selection
│                              rule (the reg_crude_shape pattern), reg_check_spec_entries()
│                              (-> reg_footer_spec) and reg_check_expand() (a user's KEY -> the `test`
│                              discriminators). names(REG_CHECKS) IS the `stats =` AND `check =`
│                              vocabulary, so label, argument and panel title cannot drift; z15-iii
│                              added two TAUGHT-BUT-NEVER-SCORED rows (residuals/normality) whose EMPTY
│                              `types` IS "a panel, no footer row". NO new statistic engine: Linearity =
│                              reg_fit(add_terms=) + reg_term_tests() (the dispatcher global/interaction
│                              already use), its squared term from reg_shape_term() -- the SAME builder
│                              `shape = "quadratic"` emits, so the check and its cure are one object;
│                              Dispersion + Influence = reg_coef_if_maker() + reg_if_se() (max
│                              SE_robust/SE_model, and max|IF_i(e_j)|/SE_j == stats::dfbetas() to
│                              cor 0.999999, but working for polr/multinom and design-aware);
│                              Proportionality = the Brant p already stashed on the fit; Collinearity =
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
├── reg-influence.R  (~495 L) Phase 18z8-B: influence functions + the SE of the gap between two
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
│                              + reg_ame_if_cat_maker (the per-category marginal IF, jacobian by
│                              central differences; pinned to marginaleffects to 10 decimals).
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
├── jmvtab-cache.R  (~1185 L)  17i: the SHARED cache kernel at the top (jmv_cache_config +
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
│                             maintainer's next prepare()
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
│                              per-dependent Model table (depFamily/depModelLevel/depTrials) ->
│                              jmvtab_reg_dep_family/_dep_modelled_first/_dep_trials. 15e: jmvtab_reg_build
│                              calls tab_reg() ONCE with per-dependent family/inverse/trials VECTORS -> one
│                              mixed-family table (no more group-by-family / tabxplor_tabs stacking).
│                              19k: jmv_reg_estimand_opts() (19e's translator for the retired
│                              exponentiate/at/estimate_display) is DELETED -- the UI sends
│                              effect/measure/display straight through; `stats = opts$stats` (a key
│                              .opts() never set) is dropped for tab_reg()'s own default GOF set;
│                              `trials` sends the typed count or NA = "take the observed maximum",
│                              which is tab_reg()'s OWN rule, instead of taking max() here silently
│                              for any integer outcome (one rule, two semantics)
├── jmvtabreg.b.R   (~170 L)  Phase 15b: jmvtabreg R6 backend (thin orchestrator, sibling of jmvtab.b.R;
│                              .h.R generated by prepare() -- inherit is lazy so it loads before then;
│                              17i: weights/export/notice/render delegate to jmv_backend_*, keeps .hint;
│                              19k: the staged-comparison gate calls jmvtab_reg_staged() -- which
│                              existed for exactly that and whose own caller inlined the predicate
│                              instead -- and `.opts()` speaks tab_reg()'s vocabulary, `%||%`-guarded)
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

| Constraint               | Detail                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                |
|--------------------------|---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| CRAN stability           | Public function arguments must NOT change without deprecation. Internals can change freely.                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           |
| vctrs record contract    | Adding a field to `tabxplor_fmt` requires updating `new_fmt()`, `fmt()`, `format.tabxplor_fmt()`, `pillar_shaft.tabxplor_fmt()`, `vec_arith` methods, and possibly `tab_pct()`/`tab_ci()`/`tab_chi2()`. ~8 functions across 3 files.                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  |
| NAMESPACE                | Auto-generated by roxygen2. Never edit `NAMESPACE` by hand. Run `devtools::document()` after changing `@export`/`@import`.                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            |
| data.table internals     | `tab_plain()`/`tab_num()` rename `col_var` to internal names to avoid data.table conflicts. The user's column names are restored afterward.                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           |
| dplyr class preservation | 30+ S3 methods on `tabxplor_tab`/`tabxplor_grouped_tab` ensure class + attributes survive all dplyr verbs. Missing a method = silent class downgrade to `tbl_df`.                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     |
| Options as config        | All defaults set in `.onLoad()` in `utils.R`. Users override via `options()`. Functions read with `getOption()`.                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      |
| Suggests-only guards     | `openxlsx2`, `ggplot2`, `jmvcore`, `ggpubr`, `cowplot`, `mirai`, `kableExtra` are in Suggests. Every call must be guarded with `requireNamespace()` or equivalent (tab_xl's ONE guard is in `tab_xl()`; `R/tab-xl-backend.R` wrappers are unguarded; `kableExtra`'s two entry points — `render_kable_html()` engine dispatch + `kable_tabxplor_style()` — are guarded, the default `html` engine never touches it).                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   |
| Color break mirroring    | `set_color_breaks()` takes positive-only thresholds. Negative breaks are auto-mirrored internally. Any `pct_breaks` value > 1 triggers ratio comparison instead of difference (the "*2 rule").                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        |
| Mean-diff asymmetry      | For `type="mean"` columns, the `diff` field stores a **ratio** (cell_mean / ref_mean), NOT a difference. Thresholds like 1.15 mean "+15% above reference". This asymmetry propagates into `color_formula()` and `format.tabxplor_fmt()`. **(2.0.0 §3: numeric `diff` becomes a real difference; the ratio moves to the `ratio` field — the never-used `rr` field renamed, placed after `diff`.)**                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     |
| tab_reg                  | Phase 12c–12g LIVE: unified regression tables (gaussian beta / binomial OR / poisson IRR / multinomial OR / ordinal cumulative OR) over lm/glm/svyglm/svyolr/svy_vglm/nnet::multinom/MASS::polr + broom (no parsnip). tab_logit/multi_logit are binomial wrappers. **The estimand is `effect` x `measure`** (19e, R/reg-estimand.R): the row it resolves to declares the fit, the `exp` flag, the header word and the stored `scale` -- additive beta -> the `diff` field + scale "raw_diff"/"log_coef"; multiplicative OR/IRR/cumOR -> `or` + scale "odds_ratio"; a ratio of means -> `ratio` + "mean_ratio". `exponentiate` / `at` / `estimate_display` are DELETED (`measure = "log"`, `effect = "at_reference"`, a real `display =`); `type`/`ci_type` are gone (19b). The `var` field carries var(Y). 12d: MNL = one OR col per outcome category vs ref; ordinal polr + Brant PO diagnostic. 12f: model-summary footer + compare= in the `test` attr. 12g / z14-i: SURVEY designs — `wt=` (a flat ids=~1 design), or a prebuilt `survey::svydesign` as `data` for anything richer (clusters / strata / fpc / CALIBRATION); `ids=`/`strata=`/`fpc=`/`nest=` are REMOVED (they reached only the omnibus p) and a svrepdesign/twophase is refused. A design's own weights become `.svy_weights` at the shared boundary, so the crude `Obs_*` columns, the AME, the frozen SD, the gap-test influence weights and the footer are all design-weighted (they silently were not); reduced weighted glance (Wald/Nagelkerke/Cox-Snell/Rao-Scott-AIC) + weighted compare (anova.svyglm Wald); weighted 3+ level (svyolr / svyVGAM); `split_var` (tab_vars analogue, tab_spread-able); `multiplier` (the UNIT a continuous predictor's effect is reported per -- **default `"sd"`** since z9, so `Model_*` on a numeric row is per-1-SD, NOT `exp(coef(glm))`, unless `multiplier = 1`); `empirical_OR` (crude %/OR beside model OR, binary; z9: continuous predictors too, from their univariable fit). No new fmt fields; new Suggests svyVGAM. |


---

## Design Decisions

### Type System

- **`tabxplor_fmt`**: vctrs record (`new_rcrd()`) with **21 per-cell fields** (was 15 before v2.0.0 Phase 1a, 18 through Phase 18s which added **`n_eff`** = the effective sample size used for a cell's CI, `p(1-p)/Var_design` (Korn-Graubard): the closed-form flat-design variance under `options(tabxplor.design_effect=TRUE)` on weighted data, `svyrecvar` under a real design, else NA → the CI falls back to the raw unweighted base; non-displayed, carried like `tot_n`, reset to NA on arithmetic; Phase 18z5 added the 20th, **`obs`** = the value a `tab_reg` cell's estimate is COMPARED TO on its own scale -- the observed/crude effect, or under `split_var` the reference group's -- NA everywhere else, so the measures reading it leave those cells uncoloured; displayable as `{obs}`; Phase 18z8 added the 21st, **`gap_se`** = the standard error of the GAP between the estimate and `obs`, on the estimate's own test scale -- written where the two are independent (`split_var` groups), which is what lets `color_signif` apply to `color = "between_groups"`; NA elsewhere, non-displayed) and **14 per-column attributes** (Phase 10i-A dropped `display_spec` → 9; Phase 15e added `model_family` → 10; Phase 17c added `role` → 11; Phase 18z13 added `conf_level` → 12; Phase 18z16-iiiii added **`degf`** + **`basis`** → 14 = "how was THIS column's interval computed", moved off the table because `meta` proved droppable). The critical distinction: fields vary per cell (accessed via `vctrs::field()`), attributes are scalar describing the whole column (accessed via `attr()`). Constructor chain: `fmt()` (public, validates + coerces) -> `new_fmt()` (internal, calls `vctrs::new_rcrd()`). *(Phase 1a reshaped 15→18 in one combined pass — decisions doc §9; `ci` is now derived from the `ci_inf`/`ci_sup` bounds by `get_ci()`, a bounds-shim.)* The 10th attribute **`model_family`** (Phase 15e; `get/set_model_family`, `""` on cross-tables) is a regression column's own family. The 11th, **`role`** (Phase 17c; internal `get_role`, `"model"`/`"emp"`/`""`), is a reg column's role, read by the colour legend to name each column's effect (OR / IRR / β / AME) without matching its rendered `"Emp."` label. The 13th and 14th, **`degf`** (the design's #PSU-#strata, NA = refer to z) and **`basis`** (`"n"`/`"weights"`/`"design"`/`"design_partial"`), are the twins of `conf_level`: the level an interval was built AT, the df it is referred to, and HOW it was computed. All three are written by ONE sweep per build tail, `tab_stamp_inference()` (was `tab_stamp_conf_level`), and the ptype2 reconcile applies the weakest-claim rule (`basis_rank`/`basis_weakest`, min non-NA `degf`) so a bind cannot over-claim. All are picked up automatically by the DERIVED `fmt_col_attrs` (17a) and carried by every cast/ptype2/vec_math reconstructor.
- **`mean` field is mean-only** (the old overload is GONE — Phase 5 landed): `mean` now carries an actual mean only on `type=="mean"` columns; for **pct-type** columns it is `NA` and the cell/reference **ratio** (the "*2 rule") lives in the dedicated **`ratio` field** (Phase 1a renamed the never-used `rr`→`ratio`). The build writes `mean = NA_reals, ratio = <ref-relative ratio>` for pct columns (`tab.R` ~L3608) and the colour engine reads `get_ratio(x)` (`fmt_class.R` ~L2688). *(c-iii audit 2026-07-19 confirmed no field/attribute consolidation is both safe and worthwhile — the fields are all user-contract and none vestigial; the column attributes — 9 then 10 with Phase 15e's `model_family`, now 11 with Phase 17c's `role` — are exported getters (except the internal `role`) AND required per-column so `format()`/colour work on a standalone extracted column.)*
- **`tabxplor_tab`**: tibble subclass via `tibble::new_tibble()` with **3 top-level table attributes** (Phase 17b merged the six 2.0.0-new attrs into one `meta` list): `subtext` (legend text, CRAN-public), `test` (chi2/ANOVA-F results tibble; §16 hard-rename of the old `chi2` attribute; row-bound → `vec_rbind` on bind; Phase 18j added `effect_size`/`es_type`/`pvalue_exact` columns, Phase 18z16-i `deff` = the design effect this row's test corrected by, and the robust discriminators are `chi2_design`/`F_design` -- TWO, not four, because the flat and the full design run the same estimator; `n` is now ALWAYS the raw count), and **`meta`** — ONE named list holding `spec` (Phase 19g, KEY 6: the table IDENTITY —
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

| File                     | Coverage                                                                                        |
|--------------------------|-------------------------------------------------------------------------------------------------|
| `test-fmt_class.R`       | fmt creation, printing, type conversion, c(), arithmetic                                        |
| `test-tab.R`             | Core: plain tables, pct, totals, NA, CI, chi2, references, wrapping                             |
| `test-tab_classes.R`     | Class preservation through dplyr verbs                                                          |
| `test-tab_xl.R`          | Basic Excel export                                                                              |
| `test-tab_logit.R`       | Phase 12a: binomial-wrapper OR/CI/p parity vs glm/svyglm, 1/OR                                  |
| `test-tab_reg.R`         | Phase 12c/12d/12e: beta/OR/IRR/MNL/ordinal + AME parity vs lm/glm/multinom/polr/marginaleffects |
| `test-tab_reg-display.R` | Phase 12h: estimate_display (est_ci bracket / prob / ame folds), Excel test label, split footer |
| `test-tab_reg-plots.R`   | Phase 12h / z15: reg_check_plots() smoke tests (build a gtable without error)                  |
| `test-tab-estimates.R`   | Phase 18z17: the estimate model + fmt_scale_of() -- no graphics device                      |
| `test-forest-plot.R`     | Phase 18z17: forest_plot() -- ladder == gridlines, cell colour == point, gap band == test  |
| `test-reg-shape.R`       | Phase 18z15: `shape =`, the plot primitives, the stored curves and the row sparkline        |

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

### Phase 19 — ecosystem integration round 2 roadmap

**The plan of plans is `dev/tabxplor_phase19_ecosystem_integration.md`** — goals, design and
architecture decisions, then the fourteen phases in full. **Read it at the start of every Phase 19
session**, together with the study it is built on (`dev/ecosystem_keys_2.md`: the measurements, the
eight keys, the defect ledger). The section below is the big picture only, so it can never be lost.

---

#### The mission — read this first, it governs every phase

Phase 17 was round 1. Since then **+8 000 lines** landed and the shape of the remaining complexity
moved, so a second study asked one question: *what are the missing keys — the small number of stored
facts or stated rules that would each collapse many scattered special cases at once?* Phase 19
implements the eight answers. **It is not a feature phase.** Its whole content is:

- a **row** describes itself, the way a **column** already does;
- a **column** says what it estimates, instead of six switches re-deriving it;
- a **measure** declares what it needs, instead of four allow-lists that disagree;
- an **argument** is a choice, not a consequence with a message attached;
- a **table** says what kind it is, once, for both producers;
- the two producers **share one vocabulary end to end** — the argument that asks, the attribute that
  stores, the legend that names and the plot axis that draws use the same words.

**The hard rules** (they override convenience, every phase):

1. **Simplify and integrate — never add another ad hoc layer.** Delete the old implementation's traces
   in the same phase: no commented-out corpses, no "kept just in case" branch.
2. **Never guess what something is.** No behaviour may depend on a rendered English label, a name
   prefix, a positional vector or a magic field value. If the fact is not stored, **storing it is the
   task**.
3. **One resolver, one model, taken to completion.** Re-deriving downstream is the disease.
4. **Facts live in ONE table.** Two encodings "kept in sync by comment" is forbidden.
5. **Never leave a representation half-migrated.** KEY 1's value is entirely in *deleting* the four
   label-block shapes; a fifth added beside them is worse than doing nothing. Split the *session*,
   never the migration.
6. **Internals and outputs are redesigned as radically as needed.** `tab_reg()`'s back-compat is
   **waived entirely** (user API included). `tab()`'s CRAN-released surface gets soft-deprecation
   shims, never silent breakage.
7. **A claimed fix ships with the fixture that fails without it.**
8. **Golden discipline** — each phase declares which goldens may move and proves the delta with
   `dev/verify_golden_field_delta.R`.
9. **End-of-phase documentation discipline** (§ The last step of every implementation).

**What must survive**: the five differentiators (per-cell metadata → lossless display switching ·
colour that reads significance · crude-vs-model comparison · the jamovi teaching path · dplyr
citizenship). Differentiator 1 is the one at risk here: it *means* every geometry is present in every
cell and the user selects afterwards — **no phase may make the user choose a geometry at build time.**

---

#### The eight keys

| key       | the missing fact                                                 | what it stores / states                                                | phase    |
|-----------|------------------------------------------------------------------|------------------------------------------------------------------------|----------|
| **KEY 1** | *what a row is*                                                  | a typed factor label column (role/var/ordered) + a `row_kind` field    | 19f      |
| **KEY 2** | *which field holds the estimate, on which scale*                 | column attrs `scale` + `pct_base` + `ci_method`; **`ci_type` deleted** | 19b      |
| **KEY 3** | *the derivation graph between arguments*                         | the graph as data — the reg collapse + the forcings in MEASURES        | 19c, 19e |
| **KEY 4** | *what a colour measure requires and is called*                   | MEASURES gains `requires`/`channels`/`auto_for`/`method`/`subject`     | 19c      |
| **KEY 5** | *2.0.0's own keystone — one aggregate core*                      | CI + test computed in the leaf, from the plan                          | 19j      |
| **KEY 6** | *what kind of table this is, and which variables it has*         | one `meta$spec` with `kind` + a uniform variable model                 | 19g      |
| **KEY 7** | *what `tab()` returns*                                           | one entry point, a predictable class, one capability predicate         | 19h      |
| **KEY 8** | *where the comparison is named* — **and it differs by producer** | `tab()`: `color` names it · `tab_reg()`: `measure` names it            | 19d, 19e |

**KEY 8's principled divergence is the intellectual core of the phase and must never be
re-collapsed**: on a crosstab every geometry is a function of the *same* sufficient statistics, so
asking for one is a **selection** over facts already computed; on a regression a geometry is a
*different fit or estimator*, so it is a **modelling decision** and must live in an argument.
*Changing `display` must never change the model.*

---

#### Settled decisions — do not re-open

Maintainer rulings (study §10 + those taken 2026-08-13, marked ★). Full table + rationale in the plan
of plans §4.

| decision                                         | ruling                                                                                                                                                        |
|--------------------------------------------------|---------------------------------------------------------------------------------------------------------------------------------------------------------------|
| ★ KEY 1 carrier                                  | **Option C** — a typed **factor subclass** label column carrying column attributes (15/15 verb survival, ~4 short methods, `is.factor()` stays TRUE)          |
| ★ KEY 1 naming                                   | friendly single-variable names **stay** (`tab$marital`) — C decouples naming from robustness                                                                  |
| ★ `ordered`                                      | stored **per variable in the declared column attributes, both axes**; a merged `levels` stays plain                                                           |
| ★ KEY 2 naming                                   | **`scale` + `pct_base`**, `ci_type` **deleted**; `get_type()`/`get_ci_type()` become **derived, soft-deprecated** accessors                                   |
| ★ `ci` anchor values                             | **`ci = c("auto","no","cell","ref")`** — `"ref"`, not `"comparison"` (reads as a sibling of `comp =`); **`"cell"` does not move**                             |
| ★ `spread`                                       | one implementation; `tab_spread()` keeps its name and absorbs `reg_spread_models()`; one argument name on both producers                                      |
| ★ KEY 5                                          | **in Phase 19**, late, after KEY 1, gated on the jamovi cold+warm+reref lock                                                                                  |
| ★ release                                        | **all of Phase 19 lands before the 2.0.0 CRAN release** — one set of shims, introduced once                                                                   |
| entry points                                     | `tab_many()` → a one-line deprecated shim; `tab_plain`/`tab_num` superseded, stop mirroring formals                                                           |
| `.fit_cache` / reref                             | keep **as is** — do not "improve" it in this phase                                                                                                            |
| jamovi boundary                                  | a shared resolver both boundaries call + a **generated** table for the JS eligibility rules                                                                   |
| `tab(OR =)`                                      | **deleted** (soft-deprecated); the `or` field becomes **unconditional** on row/col-% columns; `ref2` alone picks the dichotomisation (`"cumulative"` = cumOR) |
| `exponentiate`, `at`, `estimate_display`         | **deleted / folded** → `measure = "log"`, `effect = "at_reference"`, a real `display =`                                                                       |
| `color` canonical values                         | migrate to the **full words**, short ones kept as aliases both ways                                                                                           |
| a mismatched `{ci}` bracket                      | **refused**, never converted; an empty `display` token renders **void** + a one-time note                                                                     |
| `ci = "cell"` + `stars`/`color_signif`           | **inform and disable**, from ONE rule                                                                                                                         |
| `color` alone triggering the comparison interval | **no** — measured +38 % on a build                                                                                                                            |
| capability gaps                                  | **closed** (gaussian ratio-of-means, identity-link RD); the legality table is three-state and ships as a **runtime object**                                   |
| `filter`                                         | **keep** on `tab()`; remove from the jamovi UI                                                                                                                |

**Anti-propositions** (all still binding): do not route regression columns through the aggregate core ·
do not go sparse on the record · do not merge fmt fields · do not replace the S3-per-verb model · do
not force `pillar_shaft` through the render model · do not re-open the settled perf verdicts · do not
add a fifth label-block shape · do not delete `tab_ci()`/`tab_chi2()` as exported functions
(supersede them, move the computation) · do not move the jamovi JS rules into R (**generate** them).

---

#### Verification discipline — deliberately light

- **Per phase the default is targeted**: the test files your change touches (`filter =`) plus the
  sentinel the phase entry names. **Do not run the full suite after every edit.**
- **Full suite** (CLAUDE.md § Testing recipe) at four checkpoints: **end of 19d, 19f, 19j, and 19n**.
- **The CI-locale run** (`LC_ALL=C.UTF-8 LANGUAGE=en`) and `devtools::check()`: **once, in 19n.**
- Byte-identical phases (19a, 19c, 19i) tolerate **zero** golden churn — investigate any diff.

---

#### The phases

Each is *plan-then-implement*, starting in plan mode, in its own session. Maintainer commits between
phases, pushes at the end. Dependencies: 19a unblocks 19b/19c/19e; 19b+19c unblock 19d; 19f unblocks
19g/19h/19i/19j.

| phase   | title                                                              | one line                                                                                                                                                        |
|---------|--------------------------------------------------------------------|-----------------------------------------------------------------------------------------------------------------------------------------------------------------|
| **19a** | The floor: enabling moves, dead weight, cheap defects              | **E1** (drive the 4 reconstructors from `fmt_col_attrs` + declared reconcile rules) · D16 · D27 · the §5 cuts · the free single-sourcing · 3 family predicates  |
| **19b** | KEY 2 — what a column estimates                                    | `scale`/`pct_base`/`ci_method` stored, `ci_type` deleted, `EST_SCALES` becomes the stored library, 7 derived predicates + the `var`-sniff die                   |
| **19c** | KEY 4 — what a measure declares it needs                           | MEASURES gains its vocabulary; 4 allow-lists → 1; the `color_diff_OR`/`color_ctr`/`color_ci`/`color_num` fossil and the internal legacy vocabulary die          |
| **19d** | KEY 8a — the `tab()` comparison surface                            | `OR` retired + the odds ratio unconditional (gated on a re-measure), `ci = "ref"`, `ci_scale` cut, full-word colour values, D20–D23/D26/D28                     |
| **19e** | KEY 8b — the `tab_reg()` estimand surface                          | `effect` × `measure`; `exponentiate`/`at`/`ame_ratio`/`family="rr"` deleted; real `display =`; the three-state capability table as a runtime object             |
| **19f** | KEY 1 — the row model (Option C)                                   | `row_kind` field + the typed label column; every producer and **every consumer** migrated; `meta$vars` derived; `tab_vars` × several `row_vars` finally compose |
| **19g** | KEY 6 — one table identity, and `reg_build`'s assemblers           | `meta$spec` (kind + uniform vars); the 4 parallel assemblers → 1; `shared` becomes typed; the `test` tibble stops overloading `row_var`                         |
| **19h** | KEY 7 — one entry point, one return shape, one render model        | `tab_many()` shim · predictable class · `tab_shape()` · spread unified · the export stack's ten items + D1/D2                                                   |
| **19i** | The build pipeline and the `tab_counts` boundary                   | the settings spine becomes the **only** interface; `tab_resolve_common_args()`; the ctx declares what it carries                                                |
| **19j** | KEY 5 — one aggregate core                                         | CI + test move into the leaf; `tab_ci()`/`tab_chi2()` become superseded wrappers. **Abandon rather than force if the jamovi lock goes red.**                    |
| **19k** | The jamovi boundary                                                | the 7 hand-mirrored rules collapse onto the shared resolver; the JS rules are generated; `anova` becomes an argument; D11/D12/D13                               |
| **19l** | **Harvest 1 — the deletion pass**                                  | re-run §2's censuses, hunt the shapes the new facts made unnecessary, delete them; **report what did not shrink**                                               |
| **19m** | **Harvest 2 — open integration** *(creative, ask before building)* | what becomes possible now that rows and columns both self-describe and one vocabulary runs end to end                                                           |
| **19n** | Documentation, i18n, release readiness                             | `?help` · the six vignettes (EN+FR mirrored) · `po`/`.mo` once · NEWS · README · the CI-locale run and `check()`                                                |

**Two things to carry into every session.** (i) ✅ **The `prepare()` prerequisite is DONE** (2026-08-13,
see § Jamovi module development): the generated `.h.R` was stale and shipping *inert controls*, which
is what made **D9** and **D10** user-visible; both are now closed, so 19a inherits a clean generated
layer and only **19k** still needs a `prepare()` + rebuild. Any phase that edits a `.a.yaml`/`.u.yaml`
leaves it **inert until then** — say so in the DONE summary rather than claiming the UI changed.
(ii) The study found **no statistical soundness problem anywhere**: every issue in Phase 19 is
structural, so do not "improve" a statistic while passing through.

**At the end of each Phase,** add a `#### Phase 19{x} — <title>` markdown header **here, in CLAUDE.md**, and write the **"DONE" summary** of what was implemented in the session under it. Write it in **this file and nowhere else** — not in `dev/tabxplor_phase19_ecosystem_integration.md`, not in the chat response. Update the Repository Map above in the same pass, yourself.

---

#### Phase 19a — The floor: enabling moves, dead weight, and the cheap defects

**DONE (2026-08-13).** Targeted suite green: **FAIL 0, WARN 0, SKIP 1, PASS 4091** across every file
the phase touches. **Zero golden churn** (`dev/verify_golden_field_delta.R`: 1787 cells, 36 cases, no
delta) and zero snapshot churn — the only behaviour that moved is the four defect fixtures.

**E1 — the enabling move.** The four reconstructor families enumerated the 14 per-column attributes by
hand in **seven** blocks, so a 15th attribute meant eight edits (and `model_family` was silently
dropped for two phases because one list was forgotten). They are driven by **`fmt_attr_rules`** now —
one row per attribute, four declared columns (`neutral` / `merge` / `arith` / `scalar`), in the shape
`meta_bind_rules` + `tab_meta_bind()` already used for the table-level `meta`. The reader's default is
DERIVED from `new_fmt()`'s own formals, so "the reader's default is the constructor's default" is true
by construction; a build-time `stopifnot(setequal(names(fmt_attr_rules), fmt_col_attrs))` makes the
table exhaustive (it must be build-time — the index vectors derive at the same moment, and a missing
row would make the loops silently *skip* an attribute). **~210 lines → 1 table + 4 helpers**
(`fmt_attrs_of` / `fmt_attrs_merge` / `fmt_attrs_arith` / `fmt_ptype_attrs`), and adding an attribute
is genuinely two lines — which is what 19b, 19c and 19g were waiting for.

- **It got faster, not slower.** The 14-attribute enumeration was never the cost: 28 getter calls (12
  of them `UseMethod`) plus a full 21-field `new_fmt()` were. `vec_ptype2` **234 µs → 125 µs**,
  `vec_ptype_common` (the compact merge's reduce, the hottest fmt path) **717 µs → 378 µs**,
  `c()` 577 → 417 µs, `vec_cast` 139 → 113 µs. The end-to-end merge guard shows no regression.
  `dev/benchmarks/e1_fmt_ptype2.R` + `results_2.0.0/e1_{before,after}.txt`.
- **One deliberate behaviour change** (maintainer-approved): `vec_arith` reconciles
  `conf_level`/`degf`/`basis` with the weakest-claim rule `vec_ptype2` has applied since z16-iiiii.
  It took `x`'s blindly, so `design_col + n_col` claimed `"design"` — x's account of how ITS interval
  was computed, stapled onto a number that is half y's.
- **Found while implementing**: `vec_arith`'s `if (!same_comp)` was evaluated on a THREE-valued
  `same_comp`, so `count_column + pct_column` **errored** ("missing value where TRUE/FALSE needed")
  where a warning was intended. One token (`isFALSE`), kept out of E1 so the refactor stayed
  behaviour-free.

**D16** — `bind_rows()` on two *grouped* tabs dropped `subtext`, `test` and the whole `meta`. Root
cause: dplyr's generic runs `data` through `dplyr_new_data_frame()` **before** dispatch, so
`dplyr_reconstruct.tabxplor_grouped_tab` restored from a payload with no attributes at all; it now
restores from `template`, per dplyr's contract. Verified that this method is the **only** carrier on
that path — dplyr registers its own `vec_ptype2.grouped_df.grouped_df` into vctrs' table and it wins
unconditionally, so `vec_ptype2/vec_cast.tabxplor_grouped_tab.*` are dead code for a bind and no extra
registration could reach them. Fifth instance of "a rebuild site drops table-level facts"; takes the
carrier score from 14/15 to 15/15.

**D27** — `ref`/`ref2 = "last"` did not resolve (it fell through to the regex matcher → index 0 → a
"no columns were found as reference" warning and an all-NA `or`). **Prerequisite for 19d**, where the
odds ratio becomes unconditional and `ref2` is therefore always in force. `"last"` is now a sentinel
with **one meaning on both axes — the last LEVEL** (a total is not a level; `"tot"` names it), even
though the two axes express it differently: the column axis excludes the total column and returns a
real index, the row axis returns `-1L`, which revives a previously *dead* branch in
`calculate_refrows()` as "the last non-total row of each sub-table". Documented in all four mirrored
`?ref` blocks.

**The rest**: D7 (`pct_vect`/`ref_vect` declared in `new_ctx()` — their guards could not fire, they
*errored*) · §7.10 (`settings$cols$lvs` refreshed when `tab_prepare_pop()` resolves `"auto"`, and
`lv1` stored beside it — dormant, but it is the stale copy shipped to every parallel worker) ·
`tab_assemble()`, `set_tot_n`, `set_n_eff`, `reg_meta$shape`, `reg_meta$model_labels` deleted ·
`resolve_cleannames()` (5 sites, one of which had drifted to a different fallback),
`conf_level_default()` (10 formal defaults), `fmt_base()` (the `n_eff → tot_n → n` coalesce, 5 sites),
`inference` made a **required** argument on `plain_core`/`num_core`/`tab_apply_tests` (a lazy default
could only fire on a caller that forgot, and would then silently re-read the global option), and
`tab_ci()`/`tab_chi2()`'s tails replaced by `tab_restore()` — they were literally its body, minus
`meta`, which the exported step path therefore dropped · five family predicates
(`reg_fam_glm` / `_overdispersed` / `_disp_known` / `_disp_estimated` / `_svy_fitted`) absorbing **21**
hard-coded whitelists, extending the three z18z3 already had. The fifth is the one worth its name: the
same expression appeared as `use_svy` and as `use_wald` because **an `svyglm` has no ordinary
likelihood** — one fact, now stated · D5, D15, D18 and the `tot`-block's wrong orientation word.

**Four of the study's "cut" verdicts were wrong and were NOT applied** — reported so the ledger stops
carrying them: `complete_partial_totals` and `set_ci_type` each have one live caller (the latter dies
with `ci_type` in 19b); `set_model_family` is exported with test callers; `get_ref_means`/`get_ref_pct`
are read by `plots.R`; **D14 was already fixed**. Two more scope corrections: `plain_resolve`'s `tot`
forcing block is **not** dead — it is unreachable from `tab()`/`tab_counts()` but live through the
exported `tab_plain(tot =)`, so it is tagged and handed to 19h (its wrong message word fixed in
passing); and `ctx$levels_order` **stays in the ctx** — its one reader is `jmv_cache_aggregate(ctx)`,
reached through a hook that passes nothing but the ctx, so there is no "directly" to pass it (19k).

⚠ `dev/verify_golden_field_delta.R` gained a **reset warning at the top**: its four declarations
describe the CURRENT phase's intended delta, and z16-iiiii's leftover `ci_settings` reshape rule was
reporting its own already-landed change as a PROBLEM on four cases.

No `.a.yaml`/`.u.yaml` was touched, so **no `jmvtools::prepare()` is needed** — 19k still owns that.

#### Phase 19b — KEY 2: what a column estimates

**DONE (2026-08-13).** Full suite green: **FAIL 0, WARN 0, SKIP 4, PASS 5787**. The delta is *proved*,
not asserted: `dev/verify_golden_field_delta.R` checks, on all **1787 cells of the 36 structural
goldens**, that each stored `scale` is exactly what the deleted dispatch derived from that column's own
`(type, ci_type, var)` — and that every field and every other attribute is bit-identical.
`_snaps/golden.md` and `_snaps/render-html.md` did **not** move: no rendered output changed.

**The library became the stored fact.** `EST_SCALES` gained the `level_n` row it lacked (`type = "n"`
borrowed `level_pct`, whose `est_field` is `pct` — the code documented the fudge), a `mixed` row (the
bind neutral, content-identical to what the old dispatch answered for `type = "mixed"`), and four
declared columns: **`ladder`** (`pct`/`std`/`log`), **`var_kind`** (`pct`/`mean`/`count`/`coef`),
**`geometry`** (the word 19d/19e's arguments will resolve into) and `sd_from` extended to the level
rows. The `or` row is **`odds_ratio`**, so the row and the geometry word agree. `ladder` is the
collapse that paid best: `MEASURES$scale` is a three-entry map `c(pct=, std=, log=)` the COLUMN indexes,
so `std_when`'s four values, `is_mean`, `is_std_diff`, `use_std`, `is_logcoef` and the
`is_logcoef && measure == "diff"` special case are **one lookup**; `std_when` survives only as
`scale_from = "gap"` on the two gap measures.

**Three attributes in, one vocabulary and a `meta` sub-field out.** `scale` + `pct_base` + `ci_method`
(15 attributes); `type` and `ci_type` **deleted**, `meta$ci_settings` **deleted** with
`get/set_ci_settings`, `default_ci_settings`, `ci_method_of` and `reg_ci_settings`. Deleted by
construction: `fmt_est_field()` and its copies (**D17** — two rules that disagreed on 178 of 190 golden
columns are one), `est_scale_key()`'s order-dependent dispatch **and its `var` sniff** (the "the ORDER
of the branches is the contract" warning is gone with it), `fmt_scale_key()`'s `display` fallback,
`fmt_color_plan()`'s seven predicates, `legend_specs()`'s six, and `legend_method_name()`'s
eight-branch chain — an `est_scale_key()` dispatch written a second time in a third vocabulary.

**D19 closed**: an OR table's reference column carries `odds_ratio` like its siblings (its all-NA bounds
are the data fact saying "no interval here"), where it used to stamp `""` and z17 had to patch the axis
back by reading the rendered `display`. **D8 closed and made unrepresentable**: the method is stamped
where the interval is computed and named through the declared `CI_METHOD_LABELS`, so a `ci = "cell"`
mean now says *Student t* (it said *Welch t*) and a poisson crude IRR says *Katz on the log rate-ratio*
(it said *Wald*). **D18** finished: `has_ci` is the scale's declared `kind`, so `ci = "cell"`'s
deliberate exclusion from the significance gate is a property of the scale instead of a value silently
missing from a five-element vector.

⚠ **Maintainer ruling, superseding §4 ★ and the study's naming option 3: a clean break, not derived
accessors.** `get_type()` / `set_type()` / `get_ci_type()` / `set_ci_type()` are **removed**, and
`fmt()` lost `type =` / `ci_type =` (it gains a `...` whose only job is to abort with the mapping —
the error is the documentation, delivered where the mistake is made). So the ~40 internal readers
migrated *in this phase* rather than keeping the old vocabulary alive internally, and nothing derived
survives to be re-derived. `NEWS.md` announces it under *Removed / defunct*; both programming
vignettes' taught line is updated (one line each — the rest of the vignette work stays 19n's).

**Two roadmap instructions were NOT followed, and why.** (i) *"fold `raw_diff`/`mean_diff` into one row,
they differ only in `sd_from`"* — they also differ in `gap_key` (`adj_diff_std` vs `adj_diff`), so
folding them would re-derive both from `model_family`, i.e. re-introduce a dispatch. Two rows kept;
every stamping site knows which it is building. (ii) *"the `gof` special case becomes a declared
`geometry = "none"`"* — `gof` is a per-cell **`display`** token (a footer cell sits in the same column
as coefficients), so it cannot become a column attribute; `fmt_color_slots()`'s mask stays, with a
`# WARNING:` saying why. Recorded for 19l.

**`ordered` was deferred to 19f** (maintainer's call): measured, it has **no reader on a built table**
today — it is read once from the raw data in `tab_setup()` for `OR = "cumOR"` and discarded — so §5.1's
own admission test ("does a reader exist?") fails. 19f lands it with its row-axis half.

**Also found in passing**: `ci_type` could literally hold `"no"` (`num_core` recorded its `ci` ARGUMENT
rather than the fact) — one more instance of the disease. `verify_golden_field_delta.R` learned two
modes: `REMOVED_ATTRS`, and an `EXPECTED_ATTR` entry that may be a **predicate**
`function(old_attrs, new_value, col)` — which is what turns this phase's central claim into a
per-column proof. jamovi cache schema **12 → 13** (a tier-3 carrier's per-column `meta` list carries the
new names). No `.a.yaml` / `.u.yaml` was touched, so **no `jmvtools::prepare()` is needed** — 19k still
owns that.

#### Phase 19c — KEY 4: what a measure declares it needs

**DONE (2026-08-13).** Targeted suite green: **FAIL 0, WARN 0, SKIP 1, PASS 3792** over every file the
phase touches. **Zero golden churn** (`dev/verify_golden_field_delta.R` with an EMPTY declaration set —
1787 cells, 36 cases — which is this phase's own contract: it moves vocabulary, not facts) and zero
snapshot churn. The only behaviour that moved is three defect fixtures.

**The measurement that made the phase safe, and that had to be built first.** `color_ctr`, `color_ci`
and `color_num` are asserted by **no test anywhere**; `color_diff_OR` only as a NAME in one ctx-field
list. So the phase opens with **`dev/verify_color_attrs.R`** (committed): ~290 tables over the
`color` × `color_signif` × `pct` × `ci` × `OR` × factor/numeric/mixed space, dumping per COLUMN
`(color, color_bg, color_signif, scale, ci_method)` **plus the resolved per-cell slot vectors**, and
per case the resolver's own return. `save` before, `check` after, "IDENTICAL" is the gate. It is the
only thing standing between this refactor and a silent mis-stamping, and it caught the one real
regression on the way (see the decode-order WARNING below).

**MEASURES gained its VOCABULARY** — nine declared fields beside the arithmetic 17d put there, each
deleting a hand-written list: `channels` · `producers` · `applies_to` · `builds` · `requires` ·
`ref_auto` · `auto_for` · `method`/`subject`/`caveat`. Details in the Repository Map above. Two
build-time `stopifnot`s keep the table exhaustive (every row carries the four structural fields;
`COLOR_BUILD_ORDER` covers every declared `builds`). Counted honestly, it collapses **5 allow-lists →
1**, **5 copies of "a comparison colour needs a reference and its interval" → 1**, **3 `color = TRUE`
cascades → 1**, and the jamovi arming class → a lookup. `names(MEASURES)` is now the allow-list, which
is what the `/color-mode` skill has always (wrongly) claimed; its checklist was rewritten to match.

**`word` became a closure** (`function() gettext("difference")`). That deleted the `word_i18n` flag AND
the hand-maintained `if (FALSE) c(gettext(...))` potools anchor — verified with
`potools::get_message_data()` that all six msgids still extract statically from the closure bodies
before deleting it, because the anchor's whole purpose was that they would not.

**The break scales too**: **`COLOR_SCALES`** replaces four name-keyed lists inside `mk_color_scale()`,
a second enumeration in `default_color_scales()` and two more name maps in `set_color_breaks()` /
`get_color_breaks()` — and lets the two DERIVED scales be *declared* (`log_odds`, `adj_diff_log` name
their parent) instead of living as a `switch` arm inside `fmt_color_plan()`.

**Both fossils are dead.** (i) The 4-way split `color_diff_OR`/`color_ctr`/`color_ci`/`color_num` — 4
ctx fields, 4 spine columns, 4 recodes, 4 globals entries — is gone; the resolver returns ONE measure
and each consumer asks `measure_stage()` / `measure_applies()` / `measure_forces()`. (ii) **The
resolver was still MANUFACTURING the legacy vocabulary it had been told to stop speaking**: its
`case_when` produced `"after_ci"` one step after 17d decoded such strings away at the boundary, purely
so the CI step rather than the leaf would stamp the colour — and `color_ci` existed to receive it. Its
net effect was nil (the per-column repaint overwrites both), which is why deleting it is
byte-identical.

**Three defects, all PRE-EXISTING on HEAD, all caused by that manufactured composite** — each measured
on the pre-phase tree first, each shipping with a fixture that fails without the fix:

- `tab_num(color = "auto", ci = "diff")` stored the composite `"after_ci"` in the `color` ATTRIBUTE.
  `fmt_color_plan()` cannot match that against `names(MEASURES)` → it returned NULL and the table came
  out **entirely uncoloured** (measured: every slot 0). `tab_num()` now agrees with `tab()` cell for
  cell on that request.
- **Any** `color = "auto"` beside a `color_signif` policy **aborted** ("Unknown color measure") — on
  factor and mean tables alike. `"auto"` is the documented STRING spelling of `color = TRUE`, and only
  the logical took `mode = "auto"`, so the unresolved sentinel reached `set_color()`. The two spellings
  now agree wherever a policy is set; making them agree unconditionally moves goldens and is handed to
  19d.
- `tab-resolve.R`'s `case_when` rebuilt the **whole** `color` vector whenever any entry was `"auto"`,
  re-deriving an explicit per-row_var measure from its `pct`. Unreachable from any public entry point
  today (every caller hands `tab_build()` a scalar `color_spec$legacy`) — reported as latent, fixed
  because it is wrong on its own terms.

⚠ **One WARNING earned the hard way, now in the code and in the skill**: at the argument boundary,
**decode the alias FIRST and normalise SECOND**. `measure_key()` resolves a policy-carrying alias to
its MEASURE, so normalising first silently discards the policy half of `diff_ci`/`after_ci`/`ci` —
measured as 18 cases losing their `color_signif` and their forced CI, caught only by the
characterization dump.

**Two things deliberately NOT done, both logged in the roadmap.** `jmv_tab3_rerefable()`'s exclusion of
`color = "auto"` + `ci = "diff"` is now **vestigial** (it existed because that pair resolved to
`"after_ci"`); lifting it changes which cache PATH a live jamovi toggle takes, so it goes to **19k**
with the cold+warm+reref lock. And applying `requires["ci"] == "gated"` on the DIRECT `tab_num()` leaf
path would fix a real gap (a policy with no explicit `ci` greys every cell — 14a fixed that inside the
resolver only), but it is a behaviour change on `ci`'s surface → **19d, as D29**.

#### Phase 19d — KEY 8a: the `tab()` comparison surface

**PHASE 19d: BLOCKED (partial).** The design landed in full and the package loads and builds correctly,
but the session ran out of budget with **FAIL 48 / PASS 5773** — the remaining failures are the
*mechanical* tail of the vocabulary migration (assertions and snapshots still spelling the old values,
the `cumOR` fixtures, the jamovi tier-3 cache tuple), not a design problem. **Do not start 19e on this
commit**: the tree is red. What follows is what is really in it.

**What landed.**

- **The odds ratio is unconditional** on `type in {row, col}` percentage columns — `tab_apply_reference()`
  computes `or`/`rr` in the same sweep that produces `diff` and `ratio` (measured +16 ms on a 216 ms
  3x2 build, ~7 %: more than the study's "free" but well inside the ruling). `ref2` alone picks the
  2x2, and `ref2 = "cumulative"` replaces `OR = "cumOR"` (ruling b) — `or_resolve_cum()` became
  `ref2_resolve_cum()`, `pairs$OR` became `pairs$ref2`, and `rows$OR` is gone from the settings spine.
- **`OR` is retired**, soft-deprecated through ONE shim shared by all four entry points
  (`tab_deprecate_or()`: `"OR"` -> `display = "{or}"`, `"OR_pct"` -> `"{or} ({pct})"`,
  `"cumOR"` -> `ref2 = "cumulative"`, plus `ref = "first"` so the route is lossless). The jamovi
  boundary routes the option **silently**, at `jmv_tab3_build_armed()`, so a UI toggle never emits a
  lifecycle warning into the results panel.
- **THE comparison is resolved once**, in `tab_resolve_settings()`, as a declared **chain**:
  `color`'s text channel -> `display`'s primary token -> the difference (study §8.6 caveat 3;
  `display_comparison()` / `tab_leaf_comparison()`). Everything that used to ask the question
  separately reads that one answer, which is what makes **D26 unrepresentable** — `stars` and
  `color_signif` are no longer asked, so they cannot disagree about what an odds-ratio table compares.
  `odds_ratio` gained `requires = c(ref = "always", ci = "gated")`, which it could not have before
  ("gated" used to mean *a difference interval*); the resolver now returns `or_ci` = "the LEAF owns
  this table's interval (the Woolf log-OR one)" beside `ci`/`ci_scale`.
- **`ci` is the anchor question and nothing else**: `c("auto", "no", "cell", "ref")`, `"auto"` the new
  default (= today's hidden forcing cascade, promoted to a documented value). `"diff"`/`"ratio"`
  soft-deprecate onto `"ref"` via `resolve_ci_value()`; `"ratio"` stays lossless (it still pins the
  Katz scale) while the message teaches `color = "ratio"`. `tab_num(ci_scale =)` is **cut**.
- **D28** — `ci = "cell"` beside `stars`/`color_signif`: **inform and disable**, from one rule, on
  both paths (`resolve_ci_value` in the pipeline, `resolve_leaf_ci()` in the leaves). It used to
  abort for one consumer and silently drop the stars for the other.
- **D29** — `resolve_leaf_ci()` applies the gated forcing on the DIRECT `tab_num()`/`tab_plain()` path
  too, so `tab_num(color = "diff", color_signif = "grey_non_signif")` stops greying every cell.
- **D22** — a `display` token whose field is empty renders **void**, with a one-time note naming the
  argument that would fill it (`DISPLAY_FIELD_SOURCE`). It used to silently substitute the column's
  own primary field. **D23** — a `{ci}` bracket beside an estimate of another geometry is **refused**
  (`display_refuse_mismatch()`, reading KEY 2's stored `scale` against `DISPLAY_TOKEN_GEOMETRY`).
- **A one-field template is not a composite**: `tab_apply_display()` writes the BARE pipeline token
  (`DISPLAY_BARE_TOKENS`), so `display = "{or}"` renders exactly as the retired `OR = "OR"` did
  (1/x form, reference-cell annotation) instead of going through the composite renderer's
  `special_formatting = FALSE` path.
- **`color`'s canonical values are the full words** (ruling c): the MEASURES keys ARE `difference` /
  `ratio` / `odds_ratio` / `contrib`, the acronyms are permanent (never-deprecated) `COLOR_ALIASES`
  rows, and **`measure_stored()` is deleted** — the value typed, the value stored and the word the
  legend names are one string.
- **Two build-time OR special cases deleted**, both of which keyed on an ARGUMENT to decide a purely
  DISPLAY question: `tot_cols_type <- "no_delete"` for a row-% OR table, and the col-% total-row drop.
  The display-keyed rules that say the same thing already exist and already run
  (`tab_fold_addn_incell` / `tab_or_total_col` on `tab_is_or_display()`). Visible consequence: an
  odds-ratio table keeps its Total column, reading `n=<base>` — which is what `?tab` has always
  promised and did not deliver.
- The two BASELINE markers stay gated on the comparison (`refcols_vector` on the row path, `refrows`
  on the col one): a marker means "this is the reference of the comparison in force", never "some
  comparison could use it" — which is why the unconditional odds ratio does not dress the first level
  of every ordinary difference table as a baseline.

**HONEST CONCERNS.**

- **The 48 failures.** Categories, all seen but unverified-after-fix: `test-cumor-ordered.R` (4),
  `test-jmvtab-cache.R` (7 — the tier-3 tuple still keys on `opts$OR`, and the re-ref now has to
  refresh `or`, which it does, but `jmv_tab3_rerefable` was not revisited), `test-color-config.R`,
  `test-tooltips-14b.R` / `test-render-html.R` (the declared tooltip `OR:` line, snapshots not
  regenerated), `test-tab_reg.R` / `test-forest-plot.R` / `test-tab-estimates.R` (the full-word
  colour spelling), `test-golden.R` (2 remaining after the regen), `test-i18n-fr.R`.
  **`_golden/` and `_color_golden/` WERE regenerated** (36 + 15 fixtures) but the diff was NOT
  reviewed cell by cell, and `dev/verify_golden_field_delta.R` was NOT run — so the declared delta
  (a populated `or`, the new `color` spellings, `ci = "auto"`) is asserted, not proved. That review is
  the first thing the next session must do.
- **`dev/verify_color_attrs.R` was not run** before/after. It is the characterisation net 19c built
  for exactly this kind of migration, and skipping it is the biggest hole in this phase.
- **The +7 % odds-ratio cost** is real (216 -> 232 ms on a 3 row_var x 2 col_var build), not the
  "within noise" the study measured. It is a fair price for deleting an argument, but it should be
  re-measured on a wide table before the release.
- **Documentation is NOT done**: `?tab`'s `OR` / `ci` / `color` blocks still describe the old surface
  (four mirrored copies), `NEWS.md` says nothing, and `dev/tabxplor_architecture.md` was not touched.
- `tab_plain()` has no `display` formal, so its `OR` route reaches only `ref2`/`ref`; the odds ratio
  is computed anyway, but the old display is not restored. Decide in 19h (the entry-point phase)
  whether the superseded leaf gets a `display` or loses `OR` outright.
- The jamovi `.a.yaml` was NOT touched, so no `jmvtools::prepare()` is needed; 19k still owns
  carrying the new `color` / `ci` vocabulary into the UI.

**FOLLOW-UPS.** Finish the test tail and the golden review (immediately); `?tab` + `NEWS.md` +
the architecture guide (immediately, they belong to this phase); re-measure the odds-ratio cost on a
wide table (19l); `jmv_tab3_rerefable`'s now-vestigial `color = "auto"` + `ci` exclusion (19k).

#### Phase 19d — KEY 8a: the `tab()` comparison surface (session 2 — the tail)

**PHASE 19e: BLOCKED — NOT STARTED.** This session was asked for 19e and found the tree red exactly
as 19d's own summary warned. Closing that tail is a hard prerequisite (19e's declared sentinels
`test-tab_reg*.R` were themselves among the failures), and it consumed the whole session:
**FAIL 48 → 8, PASS 5773 → 5822**, with every remaining failure confined to ONE subsystem, the
jamovi tier-3 cache. **Nothing of 19e's own content was implemented** — no `effect` × `measure`, no
`exponentiate`/`at`/`ame_ratio` deletion, no `display =` on `tab_reg()`, no capability table, no
D25/D6. Start it on this commit, which is green everywhere except `test-jmvtab-cache.R`.

**Nine defects, all of them 19d's own, each with the fixture that fails without the fix.**

- **The odds-ratio tooltip leaked onto every percentage table** (`OR: 1.00` on a plain `tab()` hover).
  Root cause is the phase's own rule broken: the gate asked whether the `or` FIELD is populated, and
  19d made it populated everywhere. It reads the column's **declared `scale`** now (`odds_ratio` =
  this table compares on it) — *or* a non-empty `role`, because on a **regression** column the odds
  ratio is not a by-product but the model's own estimate, deliberately attached beside an AME.
- **`display` was refusing its own flagship cell.** D23 compared the template's estimate geometry to
  the column's interval geometry and aborted on `{pct} {ci}` — i.e. on `48% [-3;+4]`, which is what
  `display = "num_ci"` literally expands to. A **level names no comparison**, so it constrains the
  bracket not at all; the class D23 closes is two EFFECT geometries disagreeing.
- **`display = "num_ci"` and its documented equivalent `"{pct} {ci}"` disagreed on every total row**,
  because they were two implementations. Folded into ONE writer, **`display_write_col()`**, shared by
  the build-time `tab(display =)` and the post-hoc `set_display(col, "num_ci")`; `fmt_apply_num_ci()`
  is DELETED. D22 became **per-cell** in the fold (a total row is the reference, so it has no
  difference interval and keeps a bare `pct`), and the note still fires only where a field is empty in
  the whole column.
- ⚠ **`across()` + an inline anonymous `.fns` = silent column loss.** dplyr INLINES an anonymous
  function body into the mutate expression, so `r <- f(col)` then `r$col` resolves against the data
  mask and yields NULL — and NULL from `across()` **drops the column**. Measured: every `<fmt>` column
  vanished, `tab(display = ...)` returned the label column alone. The writer is a NAMED function now,
  with the warning next to it.
- **`ci = "cell"` + a policy was informed, disabled — and then STORED anyway.** The resolvers
  disabled it locally while `finalize_color_spec()` wrote the original `color_signif` onto every
  column, so the table claimed a gate it did not apply. The rule is ONE function,
  **`ci_disable_signif()`**, called by both resolvers and by `tab()`'s argument boundary (idempotent,
  so exactly one message).
- **A numeric `sup_cols` column lost its interval and greyed itself out.** `can_compare` asked one
  per-TABLE question ("are the factor columns on row/col %"), but a MEAN needs no percentage base —
  it compares to its reference row always. It is per-column-kind now (`pct_rowcol | has_num`).
- **`ci_scale` stopped being per-row_var** when 19d made `geom` follow the scalar `color`, so a
  vector `ci` collapsed to one entry. Recycled, and pinned to entries that actually build a reference
  interval.
- **19d's full-word colour rename had not reached `EST_SCALES$label_meas`** (still `"or"`/`"diff"`),
  which is a MEASURES **key**: the forest plot's axis lost its `1/2` glyphs and errored on lookup.
  Two more stale keys in `legend_measure_word()` / `legend_reg_adapter()` (the French legend printed
  `diff` for *différence*).
- `tab_deprecate_or()` refuses a **vector** `OR` (the row_var axis is globalised and `display` is
  scalar, so there is nowhere for it to land) instead of silently keeping the first entry.

**The gap 19d flagged and handed forward is closed here instead: `tab_plain()` and `tab_num()` gain a
real `display =`.** 19d's summary called the `OR` route "lossy on the leaves, decide in 19h"; but the
leaf and the wrapper speaking two grammars is the disease, not a scheduling question. Both leaves now
run the SAME `tab_apply_display()` the pipeline runs, so `tab_plain(OR = "OR")` is lossless and
`tab_num(display =)` exists at all. `tab_num` also resolves the comparison chain (`color` →
`display`) for its interval scale.

**The jamovi boundary got its correctness half** (its consolidation stays 19k's). `jmvtab_build()`'s
**two hand-mirrored `ci` rules are deleted** for one `resolve_leaf_ci()` call — they had fallen behind
19d, so a `stars = FALSE` factor table let the re-ref compute an interval the fresh rebuild leaves NA:
a cached table that disagreed with a rebuilt one. The tier-3 tuple gained the **interval geometry**
(`measure_geometry()`, extracted so the cache and the pipeline cannot disagree about it) — a
diff↔ratio toggle used to be an exact tuple HIT and re-painted a ratio over the difference interval —
and is keyed on the **resolved** `OR` route (display/ref/ref2), not the retired option. Cache schema
**13 → 14**.

**HONEST CONCERNS.**

- **`test-jmvtab-cache.R` is the one red file: 8 failures.** 7 pre-existed on the 19d commit; **1 is
  new**, from my tuple rework, which I could not finish verifying. They are all the tier-3 armed
  CARRIER, and they share one cause I identified but did not fix: **19d made `or` a
  reference-dependent field on every table, and the tier-3 re-ref / level-relevel paths do not
  recompute it** (`or_compare = TRUE` in `jmv_tab3_reref()` is a first step; `jmv_relevel_cols()`
  reorders columns, which changes which level is `ref2`, and recomputes nothing). Two assertions also
  expect a re-ref HIT where the stricter tuple now rebuilds. **This is 19k's subsystem and it should
  be finished there, with the cold+warm+re-ref lock** — but it is a genuine correctness hole in the
  live jamovi module today, not a cosmetic one, so it must not be deferred past 19k.
- **The golden review 19d owed is still not done cell by cell.** `_golden/` was regenerated in 19d and
  `verify_golden_field_delta.R` was not run then and is not run here. What IS now true: `test-golden.R`
  and every `_snaps/` file pass unchanged, and this session's only golden edit was migrating
  `helper-golden.R` off the deprecated `ci = "diff"` (lossless — it maps to `ci = "ref"`), which is
  what was polluting two snapshots with a lifecycle warning. Two stray `_snaps/*.new.md` artifacts
  committed by 19d are deleted.
- **`dev/verify_color_attrs.R` was still not run** before/after. It is the characterisation net 19c
  built for this migration and it remains 19d's biggest unclosed hole.
- **124 deprecation WARNINGs remain in the suite** — the test corpus still calls `ci = "diff"` /
  `OR = TRUE` / `color = "OR"` widely. Harmless (the shims work, that is what they assert), but it
  hides new warnings. A mechanical corpus migration belongs to 19l.
- `?tab`'s `OR`/`ci`/`color` blocks and `NEWS.md` still describe the pre-19d surface (19d's own
  follow-up, still open); `dev/tabxplor_architecture.md` untouched.
- No `.a.yaml` / `.u.yaml` was touched, so **no `jmvtools::prepare()` is needed**; 19k still owns
  carrying the new `color` / `ci` vocabulary into the UI.

**FOLLOW-UPS.** 19e, in full, on this commit (nothing of it exists). Then: the tier-3 `or` recompute
+ the two re-ref hit expectations (19k, at the latest); `?tab` + `NEWS.md` + the architecture guide
(19d's debt); `dev/verify_color_attrs.R` and the golden cell review (19l); the deprecation-warning
corpus migration (19l).

#### Phase 19f — KEY 1: the row model (Option C)

**DONE (2026-08-14).** Full-suite checkpoint: **FAIL 8, PASS 5823, SKIP 4** — and the 8 are *exactly*
the pre-existing `test-jmvtab-cache.R` failures 19d's summary flagged (verified by re-running that
file on the 19d commit: same 8 line numbers, same count). **No rendered output moved**: not one
`_snaps/*.md` changed except `fmt-contract.md`'s field list. The structural goldens moved and the
delta is **proved, not asserted** — `dev/verify_golden_field_delta.R`, taught two new modes, checks on
all **1795 cells of the 36 goldens** that `row_kind` is exactly `ifelse(in_totrow, "total", "data")`,
that every other field and column attribute is bit-identical, that each declared index column's
VALUES are unchanged, and that `meta$vars` lost only the facts that are now derived.

**Two facts, two carriers — and the split is load-bearing.** (i) `row_kind`, a **field**
(`data`/`total`/`n`/`pct`/`pvalue`/`gof`/`blank`), replacing the logical `in_totrow` — the record stays
at 21 fields. It cannot live anywhere else: `fmt_color_plan()` calls `is_totrow()` on a LONE extracted
column with no table in scope. (ii) **`tabxplor_lvl`**, a factor **subclass** on the index columns
carrying `role` / `var` / `ordered` as ordinary column attributes. Measured, and it is why the
migration was affordable: `[`, filter, arrange, mutate, slice, group_by, as.data.frame, vec_slice and
forcats' fct_drop/fct_rev/fct_relevel keep class **and** attributes with **zero code**; only
`vec_c`/`bind_rows`, `droplevels()` and `[` needed one. `is.factor()` stays TRUE, so the 39 `is.factor`
sites did not move, and `tab$marital` keeps its friendly name.

**Every producer declares, every consumer reads.** ONE stamping call, `tab_stamp_index()`, in both
leaves, `tab_compact()`, `tab_reg()` and the transpose; ONE read, `tab_declared_vars()`.
`tab_vars_recorded()` is deleted. What went with them:

- **`meta$vars` lost the whole variable model.** `row_vars` / `tab_vars` / `compacted` are the declared
  columns, `col_vars` always was the fmt columns' own attribute, and `row_roles` is the field. `vars`
  keeps only `wt` / `caption` / `var_labels` — what no column can carry. `new_vars_attr()` went from
  six formals to two.
- **`meta$vars$row_roles` is gone**, with `set_row_roles`/`get_row_roles_raw` and the seed/extend/slice
  bookkeeping in three files. It was a *positional* vector created at RENDER and living one render
  pass, so every consumer outside that pass fell back to matching English row labels — the i18n hazard
  17c closed for the exporters was still open for everything else, **by design**. Now the rows carry
  their kind through every slice.
- **`tab_reg()` stops punning.** A predictor is `role = "var"`, not `tab_vars = "var"` — a fake
  sub-table variable it was reported as because that was the only slot the grouped-tab machinery
  offered.
- **`tab_collapse_total_rows()` compares a KEY** (`n`/`wn`/`pct`/`mean`) instead of running a full
  `format()` pass over every fmt column of every block. It is also stricter in the right direction:
  two blocks with genuinely different bases that happened to *round* to the same printed cell used to
  be collapsed into one Total whose N was only one of theirs.
- **The export prep's variable-name column is `rv$var_col`** — one rule where a merged crosstab tested
  for a column literally NAMED `"row_var"` and the regression needed a second, different clause
  (which also sniffed the grouping). Same in `tab_estimates()`.

**The composition limit is lifted.** `tab(d, c(marital, relig), race, tab_vars = year)` returns a
**table**, not a silent list: `can_merge <- length(tab_vars) == 0` is deleted, and `tab_compact()`
groups by `(tab_vars, row_var)` with the sub-table axis outer (a stable re-order, so each variable's
own row order survives). A documented product limitation disappears. Found while implementing:
`tab_compact()` renamed **column 1** to `"levels"`, which with `tab_vars` is a sub-table column — so
the composition could not have worked even with the grouping slot freed. It renames the *declared*
level column now.

**`ordered` landed, as 19b deferred it.** A merged `levels` column must stay a plain factor (vctrs
rightly refuses to combine two ordered factors with different level sets), but the FACT now survives:
each piece's declared `ordered` map is carried through the flattening and **unioned** by the vec_c
reconcile, so a merged table knows which of its stacked variables were ordinal. It used to lose that
outright.

**Retro-compat kept where it is CRAN-public**: `fmt(in_totrow =)` is a soft-deprecated spelling of
`row_kind = "total"`, `$in_totrow` is a read alias (the README teaches `$` field access), and
`is_totrow()` / `as_totrow()` are unchanged derived reads.

**HONEST CONCERNS.**

- **The 8 `test-jmvtab-cache.R` failures are still red**, unchanged and untouched by this phase. They
  are 19d's tier-3 carrier hole (`or` is reference-dependent on every table now and the re-ref /
  relevel paths do not recompute it). Still a genuine correctness hole in the live jamovi module;
  19k owns it and it must not slip past 19k.
- **The reg `var` column now renders as the variable-NAME column** (`var_name_col`: rotated vertical
  in html/xl, italic, droppable by `var_names = "none"`), where it used to render as a plain kept
  tab_var. No snapshot moved, so nothing in the test corpus exercises that path visually — the change
  is *asserted* by the uniform rule, not *seen*. Worth one eyeball at the 19n documentation pass.
- **`ordered` on the COLUMN axis was not done.** The §4 ★ ruling says "both axes"; the row axis had a
  real defect (a merged table losing it) and now has a real carrier, but a col-axis `ordered` would be
  a 16th fmt attribute with **no reader anywhere** — 19b's own admission test — plus stamping in four
  producers and a golden move. Deferred with that reason stated, not forgotten.
- `dev/verify_color_attrs.R` was not run (19d/19c's standing debt); nothing in this phase touches the
  colour vocabulary, and the golden delta proof covers the stored colour attributes cell by cell.
- `?tab` / `NEWS.md` / the vignettes still describe the pre-19d surface (19d's debt, still open).

**FOLLOW-UPS.** 19g (`meta$spec`) can now derive half its `vars` and key the `test` tibble on
`(scope, var, level, col)`; 19h's `tab_shape()` capability predicate replaces the five scattered
aborts (`tab_compact` / `tab_transpose` / `tx_transpose_render`) that this phase left in place; the
column-axis `ordered` when a reader exists (19m or later); the reg `var`-column rendering eyeball
(19n).


---





#### Phase 19g — KEY 6: one table identity, and `reg_build`'s assemblers

**DONE (2026-08-14).** Full suite: **FAIL 8, PASS 6001, SKIP 4** — and the 8 are *exactly* the
pre-existing `test-jmvtab-cache.R` failures 19d flagged and 19f re-verified (same file, same count,
untouched here). The golden delta is **proved, not asserted**: `dev/verify_golden_field_delta.R`,
taught two new modes, checks on all **1795 cells of the 36 goldens** that the new `meta$spec$vars`
is bit-identical to the old `meta$vars`, that `spec$call` is the old `reg_meta`, that a `kind` is
stated, that every other `meta` sub-field is untouched — and that the `test` tibble's re-key is the
declared MAPPING (`row_var` -> `var`, `col_var` -> `col`, `term` absorbed) with every other column
bit-identical. No per-cell field and no per-column attribute moved. No `_snaps/*.md` moved.

**One `meta$spec`, three slots, both producers.** A crosstab recorded its variables in `meta$vars`
and a regression recorded **none of them**, carrying a parallel 20-field `meta$reg_meta` instead; and
the *kind* of table was not stored at all — `is_reg_footer()` decided "is this a regression" by
asking whether the `test` tibble happened to contain a reg-flavoured discriminator, in the same file
whose header comment said a reg table carries `reg_meta`. Now: `kind` is **stated** by the producer
and read through `tab_is_reg()`; `vars` keeps only what no column can carry, which after 19f is the
*whole* uniform variable model (everything else is derived from the columns, so the two producers
agree by construction rather than by two code paths); `call` is the producer's recipe, so
"a table remembers how it was made" generalises past `reg_check_plots()`'s `fit_spec`. `is_reg_footer`
is deleted — the sniff survives ONLY inside `tab_kind()`, as the documented fallback for a table that
lost its metadata. `reg_meta$conf_level` went with it: a stale table-wide duplicate of a per-COLUMN
attribute (`tab_stamp_inference` stamps the level on every column), so it could only ever disagree
with what it described.

**The `test` tibble stops overloading `row_var` — and the two arms end up on ONE key.** `row_var`
meant the row VARIABLE on a crosstab row and the SPLIT-GROUP LEVEL on a reg row, which is why z15 had
to add a 13th column (`term`) rather than use it. Now: **`var`** = which variable the row is about
(a crosstab's row variable, a regression's predictor, `""` = the whole table/model — `term` is
**deleted**, folded into it), **`col`** = which column it keys under, and the sub-population rides a
column **named after the grouping variable** — the tab_vars for a crosstab, the `split_var` for a
regression. That last move is the integration: one rule (`test_group_cols()`) reads both arms, and
it cost a column rather than adding one. 14 columns → 13.

**`reg_build`'s four parallel assemblers → one.** The split branch carried a **complete duplicate**
of the assembly tail (its own `tab_stamp_inference` / `new_tab` / `meta` literal / `group_by`) which
had already drifted once — both are `reg_finalize()` now. The three column-builder blocks
(AME, MNL-vs-rest, coefficient) were three `purrr::map2(fits, specs, ...)` chosen by a **table-scalar**
`if`, even though 15e made the family per SPEC — so a mixed table had to be degraded upstream before
the scalar could be trusted. They are three named builders behind ONE map with a **per-spec** choice,
which picks exactly what the scalar picked on a homogeneous table. The four hand-written copies of the
`test`-row tibble literal (GOF / comparison / interaction+global / checks) are `reg_test_row()`, and
`reg_term_tests()` lost the `row =` parameter it only ever received one value for.

**The `shared` bag is a typed record.** `new_reg_shared()`: 24 keys documented as 20, partially
re-listed twice, with two fields declared nowhere and a hand-kept mirror in `fmt_class.R`'s
`globalVariables()` — the constructor's **formals** are the contract now, the mirror is DERIVED from
them (and moved beside the record), and `reg_build()` normalises whatever it is handed through the
constructor, so a direct caller cannot be missing a field.

**One `stats` / `check` vocabulary.** `REG_GOF_KEYS` + `reg_stat_keys()` + `reg_validate_stat_keys()`
— `tab_reg(stats =)` and `reg_check_plots(check =)` had two hand-written lists and two validators for
one vocabulary, so a check could be addable in one and not the other.

**Two defects found while implementing, both shipping with the fixture that fails without them.**
(i) `test_group_cols()`'s "not in the schema" rule read the renderers' own scratch keys (`.grp`,
`.term`) as grouping variables and split a plain regression footer into one block **per predictor**;
dot-prefixed names are render scratch, never data. (ii) `reg_footer_lines()` used the dropped `test`
tibble as its own idempotence guard — with the KIND stored, a second call no longer no-ops by
accident, so it carries an explicit emptiness guard.

**HONEST CONCERNS.**

- **The 8 `test-jmvtab-cache.R` failures are still red**, unchanged and untouched. They are 19d's
  tier-3 carrier hole (`or` is reference-dependent on every table now and the re-ref / relevel paths
  do not recompute it). Still a genuine correctness hole in the live jamovi module; **19k owns it and
  it must not slip past 19k.** The tier-3 cache schema is bumped **14 → 15** here (a carrier stores a
  built table, whose `meta` and `test` shapes both moved), so stale stores are discarded rather than
  deserialized into the new code.
- **`spec$call` is EMPTY on a crosstab**, deliberately. The plan asks that `fit_spec` "generalise";
  measured, everything a crosstab would record already rides its columns or its settings spine, so
  filling the slot today would create the duplicate this key exists to delete. The slot and its
  accessor (`tab_call()`) exist and are read; **19i**, which makes the settings spine the only
  interface, is where a crosstab recipe can be written without inventing a second encoding.
- **The three extracted column builders keep their old inner indentation** (one level too deep). The
  bodies are byte-identical to what they replaced, which is what made the extraction reviewable
  against the golden proof; re-indenting ~110 lines would have made the diff unreadable for no
  behaviour. Worth a mechanical pass in **19l**.
- `?tab`'s `OR`/`ci`/`color` blocks and `NEWS.md` still describe the pre-19d surface (19d's standing
  debt); `dev/verify_color_attrs.R` still not run (19c/19d's).
- No `.a.yaml` / `.u.yaml` was touched, so **no `jmvtools::prepare()` is needed**; 19k still owns that.

**FOLLOW-UPS.** 19h's `tab_shape()` can key on `tab_kind()` (the two facts together are the
capability predicate); a crosstab `spec$call` in 19i; the tier-3 `or` recompute in 19k; the
column-builder re-indent + `?tab`/`NEWS.md` in 19l/19n.

#### Phase 19e — KEY 8b + KEY 3a: the `tab_reg()` estimand surface

**DONE (2026-08-14).** Full suite: **FAIL 8, WARN 131, SKIP 4, PASS 5997**, against a same-session
baseline of the 19g commit measured by stashing the whole diff and re-running: **FAIL 9, WARN 131,
SKIP 4, PASS 5871**. So +126 assertions, one failure fixed, and the remaining 8 are *exactly* the
pre-existing `test-jmvtab-cache.R` set (verified the same way: same file, same 8 line numbers). No
`_snaps/` and no `_golden/` fixture moved: 19e touches no crosstab path, and every retired spelling
has an **exact** new equivalent, which is what `test-reg-estimand.R` asserts cell by cell.

**The four-argument product is gone.** `family` × `effect` × `at` × `exponentiate` was 36
combinations for 9 distinct estimands, with three degrade blocks, two aborts and ~19 cells in which
an argument was silently ignored (`exponentiate` was a no-op on the whole marginal path; `at` was
degraded away in three separate places). The surface is now the minimal non-redundant
parameterisation — **(which contrast) × (which measure)**:

```r
effect  = c("coefficient", "marginal", "at_reference")            # absorbs `at`
measure = c("auto", "odds_ratio", "ratio", "difference", "log")   # absorbs `exponentiate`
```

both resolved **per dependent** exactly where `family` is. `measure` takes the full word (taught) or
the discipline's acronym (`"OR"` / `"RR"` / `"IRR"` / `"RD"`, permanent aliases), and the column
header keeps the acronym — **so the table prints the mapping between the two every time it renders**.
`"log"` is not a peer value: it is the family's default estimand un-exponentiated (which is what
`exponentiate = FALSE` meant), with `log_odds` / `log_risk` / `log_rate` pinning *which* base.

**`R/reg-estimand.R` — the declared library.** One row per (family, effect, measure) the package can
answer, plus rows that state why one cannot be. Details in the Repository Map. What it **deleted**,
counted honestly: `reg_effect_word()` (a four-argument nested switch) IS the `word` column;
`reg_model_note()` (six family arms × `do_exp`) IS the `note` closures; `reg_crude_shape()`'s
dispatch — *including* its cross-family borrow (a binary marginal ratio reusing `REG_EMPIRICAL$rr$rr`)
— IS two declared columns; `do_exp_for` / `effect_shape_for` / `eff_word_for` are views of one row;
and reg_build's **table-scalar `if`** choosing between the three column builders is the row's own
`builder`, so the choice is per spec where 19g made the builders per spec. `reg_column()` now writes
the estimate into the field its SCALE declares (`or` / `ratio` / `diff`) instead of choosing between
two hard-coded `fmt()` calls — which is precisely what made a third shape unrepresentable.

**The vocabulary is `tab()`'s, end to end.** `measure`'s values ARE `EST_SCALES$geometry` (19b), so
the argument that asks, the attribute that stores, the legend that names and the forest-plot axis that
draws are one vocabulary: *the argument names the geometry, the attribute names the row.*

**The two capability gaps are closed** (maintainer ruling), both mirroring the existing modified-Poisson
route one link over — same fitter (`svyglm`, whose design-based variance IS the Huber–White sandwich),
same crude-companion rule, one `reg_fit` arm each:

- **`measure = "ratio"` on a binary outcome** = the modified Poisson, reachable **by name** at last.
  It used to require typing `family = "poisson"` on a binary outcome — naming the wrong distribution
  to get a measure. That route still works, unchanged and byte-identical (asserted), and its message
  now names the front door.
- **`measure = "difference"` on a binary outcome** (new internal fit `"rd"`) = the identity-link
  additive-risk model, started from OLS and falling back to the **linear probability model** with a
  message if it does not converge — the runtime third state made real. Its crude twin needed **no new
  `REG_EMPIRICAL` rows**: `binomial$base` + `binomial$ame` already are the risk pair.
- **`measure = "ratio"` on a continuous outcome** (new internal fit `"mr"`) = the ratio of adjusted
  means by Poisson pseudo-maximum-likelihood, on the `mean_ratio` scale tabxplor already owned and
  `tab()` has used for years, with a new `REG_EMPIRICAL$mr` crude block. Guarded on a non-negative
  outcome.
- **The marginal ratio opens to every family** — the "needs a probability-scale outcome" abort is
  deleted; a gaussian/poisson `effect = "marginal", measure = "ratio"` is `lnratioavg` on
  `mean_ratio` (new `reg_marginal_column()` shape `"raw_ratio"`).

Both new fits are checked against hand-fitted `glm(binomial("identity"))` / `glm(quasipoisson("log"))`
in `test-reg-estimand.R` (agreement to 1e-6).

**The capability table ships as a runtime object with four consumers**, as the ruling required: the
boundary resolver, the **enumerated** error message (it says which of the three states it is, and
lists what the outcome *does* offer, generated from the table), the new exported
**`reg_measures(data, dependent)`** lister, and `?tab_reg`'s section — a roxygen `@eval` of
`reg_measures_rd()`, so the documentation is rendered *from* the resolver. Phase 19k adds the jamovi
eligibility rule as the fifth reader of the same table.

**`estimate_display` → a real `display =`** on `tab_reg()` / `tab_logit()` / `multi_logit()`, mirroring
`tab()`'s grammar, with the four old values kept as documented shorthands over it — deleting a preset
layer rather than adding machinery, since `"prob"` already *was* `"{or} ({pct})"`. The rule is stated
in the code: **a display template may ask for an auxiliary quantity of the SAME fit; it may never
change the fit or the estimand.** That is the anti-proposition at its true grain, and it is what keeps
`measure` the only estimand argument.

**D25 closed and made unrepresentable.** `tab_reg(color = "difference")` on an odds-ratio column used
to be *accepted*, storing a measure that contradicted what the column estimates. The ladder comes from
the column's stored `scale` now, so what is left to choose is what to compare it **to** — the measures
for which `measure_own_ref()` is TRUE, a **derived** allow-list, not a new one. `TRUE` in the text slot
means "the column's own geometry", so the documented headline `c("OR", "adjustment")` becomes
`c(TRUE, "adjustment")`. ⚠ `c(TRUE, "adjustment")` is coerced by `c()` to `c("TRUE", "adjustment")`, so
the STRING spellings are the ones the normaliser must accept — stated in the code.

**D6 closed**: the multi-dependent × model-list recursion forwarded neither `spread_models` nor
`.fit_cache` (so a user's `spread_models = FALSE` silently reverted, and the jamovi cache never
filled), and passed a **positional** `family` vector whole to each recursion, where its first entry
became every outcome's family. `reg_per_dep()` is the one slicer, shared by `family` / `effect` /
`measure` and the recursion. **D5** was already fixed in-tree (verified, not re-done).

**19g's corrective pass, reported as asked.** `spec$call` did *not* record enough to reproduce the
estimand: `at` and `estimate_display` were absent from `fit_spec`, and `effect` was stored twice. It
records the estimand per dependent now (`measures` / `effects` beside `families`), read back through
the new `reg_meta_estimand()`, and `exponentiate` / `do_exp` / `at` left with the arguments they
mirrored. `spec$vars` needed no change. **Found in passing**: `test-reg-checks.R:175` was already
failing on the 19g commit — 19g renamed the `test` tibble's `col_var` to `col` and this assertion was
missed (its summary reports 8 failures, all in `test-jmvtab-cache.R`; measured here, the baseline is
**9**). Fixed here.

**The jamovi module keeps working, with stale labels.** Its generated `.h.R` can only be rebuilt by a
maintainer `jmvtools::prepare()`, which **19k** owns together with the `.a.yaml` / `.u.yaml` / `.js`
vocabulary — so the retired options are **translated at the bridge** (`jmv_reg_estimand_opts()` in
`jmvtabreg-cache.R`, the same silent routing 19d used for the retired `tab(OR =)`), one function that
dies in one edit when 19k lands. `JMVREG_CACHE_SCHEMA` **3 → 4** (the raw-fit key's `extra` carries
`(effect, measure, display)` instead of `(effect, at, estimate_display)`). **No `.a.yaml` / `.u.yaml`
was touched.**

**The corpus and the call sites migrated in the same phase** (rule 5): ~70 call sites across 19 test
files, both reg vignettes (EN + FR), `?tab_reg`'s examples and prose, and `NEWS.md`. The **marginal
risk ratio keeps its full teaching** — prose, worked example and `Model_RR` header — under the new
spelling, as instructed. `tab_reg()` has never been released, so the retired names are **removed**,
not deprecated; a `...` catches them and the mapping IS the message (19b's `fmt(type =)` idiom).

**HONEST CONCERNS.**

- **The 8 `test-jmvtab-cache.R` failures are still red**, unchanged and untouched — 19d's tier-3 `or`
  hole. Still a genuine correctness hole in the live jamovi module; **19k owns it.**
- **`Model_MR`** is a header this package invents: there is no settled acronym for `exp(coef)` of a
  log-link mean model ("ratio of means" has no standard one). Flagged for the maintainer to veto.
- **The `rd` fallback means two different estimators can produce one column.** The footer says which
  ran (the family display name differs), and the fallback informs — but a user who does not read the
  message will not know from the numbers.
- **The new footer phrases are untranslated.** `reg_family_display_name()` gained two arms and the
  estimand notes gained several msgids; `po/R-fr.po` is 19n's single pass, as planned. The pre-existing
  French phrases are untouched and still resolve (verified).
- **`REG_EMPIRICAL`'s `coef` / `coef_log` per-family fields were NOT deleted**, contrary to the plan.
  They name a family's own coefficient shape and its logged twin, and the binary arm builds *both* at
  once — they are family facts, not an estimand dispatch. The estimand row is the authority for which
  shape the current estimand pairs with; these two are the fallback and the twin lookup.
- **`dev/verify_color_attrs.R` was still not run** (19c/19d's standing debt), and the golden cell
  review 19d owed is still open. Nothing here touches the crosstab colour vocabulary, and
  `test-golden.R` + every `_snaps/` file pass unchanged.
- `?tab`'s `OR` / `ci` / `color` blocks still describe the pre-19d surface (19d's debt, still open).

**FOLLOW-UPS.** 19k: the tier-3 `or` recompute, the `.a.yaml`/`.u.yaml`/`.js` estimand vocabulary + a
`prepare()`, and deleting `jmv_reg_estimand_opts()`. 19l: the `Model_MR` naming call, and re-checking
whether `reg_fam_binary()`/`reg_fam_logscale()` still earn their keep now that `REG_FIT_FAMILY` exists.
19n: `po/R-fr.po` + the vignette prose pass.

---

#### Verifying phases 19d–19g and closing the red tail

**DONE (2026-08-14).** The tree is **GREEN for the first time since 19d**: full suite
**FAIL 0, WARN 127, SKIP 4, PASS 6005**, against the inherited **FAIL 8, WARN 131, PASS 5997**. The
delta is *proved*: `dev/verify_golden_field_delta.R` with an **empty** declaration set reports no
change on any of the **1795 cells of the 36 goldens** — no field, no column attribute, no `test`
column, no `meta` sub-field — and `dev/verify_color_attrs.R` prints **IDENTICAL** over its 293 cases
(every stored colour attribute and both resolved slot vectors). No `_snaps/*.md` and no `_golden/`
fixture moved.

**19d, 19e, 19f and 19g are verified landed**, by mechanism rather than by re-reading their summaries:
the `OR` and `ci = "diff"` shims are `all.equal`-lossless; every stored `color` is a `names(MEASURES)`
full word and every stored `scale` an `EST_SCALES` key across 293 argument combinations;
`reg_measures()` returns its three-state table; `tab(c(marital, relig), race, tab_vars = year)` returns
a grouped table; `tab_kind()` answers. **The three standing debts are closed**: the colour
characterisation now has a real before/after, the golden review is superseded by two per-cell proofs,
and `NEWS.md` is written (`?tab`'s three mirrored blocks are parked in 19h, which deletes two of them).

**The 8 failures were four independent problems, none of them what the summaries said.** The `or`
recompute inside `jmv_tab3_reref()` was already there and correct; what was actually broken:

- **The `ci` anchor rule was written twice and the two copies disagreed** — the pipeline resolver
  silently UPGRADED an explicit `ci = "no"` to `"ref"` whenever `stars`/`color_signif` wanted an
  interval, the leaf resolver upgraded only `"auto"`. So `tab(ci = "no", stars = TRUE)` built an
  interval that `tab_num()` did not, and the jamovi tuple recorded a `ci` its own carrier contradicted
  (hence a re-ref that refreshed everything except the bounds). **Maintainer ruling: extend D28's
  "inform and disable" from `"cell"` to `"no"`** — `ci` is the anchor question, `stars` and
  `color_signif` READ what it anchors, so the two values with nothing to read now disable them from
  ONE place (`ci_disable_signif()`, already the single statement with three consumers, gains
  `CI_NO_INTERVAL_TO_TEST`). Overruling what the user typed was the root of it. The disagreement is
  unrepresentable now rather than reconciled.
- **`or` under `levels = "first"`** — a *leaf* divergence reproducing on a cold build, so it was live
  in the module. The table shows one level against the merged rest, so its odds ratio is the **true
  binary one** (that level against everything else — which is what makes showing a single column
  meaningful). `tab()` merges before the leaf and gets it right; the jamovi path DEFERS the merge (the
  aggregate and the whole-table test must see every level) and the surviving level is also `ref2`, so
  every column referenced itself and `or` came out **1 everywhere**. The leaf is now TOLD the col_var
  is shown dichotomised (`dichotomise`, carried from `lv1` — the fact travels instead of being
  re-derived from a level count) and rebuilds the complement, which within a row base is just `1 - p`.
  Both paths are byte-identical on `pct = "row"` and `pct = "col"`.
- **Two test-harness slips, one of which was hiding a real bug.** `jmv_opts()` is `modifyList`, which
  keeps the FIRST of two same-named entries — so every `o0(...)` wrapper silently swallowed the
  caller's override: `o0(color = "ratio")` built with `color = "diff"`, and the multi-`col_var` case
  built a **one**-col_var table. It keeps the LAST now (R's ordinary override semantic), and that
  exposed **`jmv_tab3_reref()` pooling every col_var's levels into ONE sweep** — so a partyid level's
  odds ratio was computed against a race level (measured: ORs in the tens against a rebuild's 1.00).
  It runs one sweep per `col_var` now, exactly as the build runs one leaf per `col_var`. `diff` and
  `ratio` are column-wise and were unaffected: **`or` is the only per-cell field whose value depends
  on which OTHER columns are present** — the same fact as the dichotomise fix, found twice.
- **`display` was applied by two writers.** `jmv_apply_display()` stamped the literal `"{or}"` where
  `tab_apply_display()` normalises a one-field template back to the bare `or` token (1/x form and
  reference annotation included). It delegates now — so it also stopped writing a display onto p-value
  and blank rows — and `tab_apply_display()` gained the two tokens that kept the vocabularies apart:
  **a bare field name** (`display = "n"` ≡ `"{n}"`, which is the better spelling anyway and is what
  the jamovi ComboBox has always sent) and **`"auto"`** as a no-op beside `NULL` / `""` / `"no"`.

**One optimisation taken, one deliberately refused.** The tier-3 tuple keyed the RAW `display` string,
which made every display toggle — the second most frequent jamovi interaction — rebuild the whole
table; `.return_armed = TRUE` returns before `tab_apply_display()`, so the only way `display` reaches
the carrier is by NAMING the comparison. The tuple carries `comparison = display_comparison(display)`
instead, which also absorbed the `or` flag (that same fact tested for one of its values): two keys →
one, and the toggle is a re-paint again. **Refused**: recovering the `diff ↔ ratio` toggle, which since
19d genuinely changes the stored interval (percentage points vs Katz log-RR) — the re-ref could
recompute it on the other scale, an exact re-paint never can, and that is 19k's seam with its
cold/warm/re-ref lock. Four assertions state the rebuild explicitly, with the reason. Cache schema
**15 → 16**.

**HONEST CONCERNS.**

- **`tab(ci = "no", stars = TRUE)` changed behaviour** — it informs and drops the stars where it used
  to build an interval silently. Nothing in the corpus or the goldens moved, but it is a real change
  on a CRAN-released argument, and it is in `NEWS.md` rather than merely in the code.
- **`jmv_apply_display()` no longer writes a display onto p-value / blank / total-marker rows.** That
  is correct (a p-value cell has no `n`) and no test moved, so it is *asserted* by the shared writer's
  rule rather than *seen*. Worth one eyeball in a live jamovi pass, which 19k schedules anyway.
- **The `dichotomise` fix assumes the kept level is the FIRST**, which is what `levels = "first"`
  means. A user combining it with an explicit `ref2` naming a level that gets dropped would see the
  Total column's odds ratio differ between the two merge paths — pathological, untested, and stated in
  the code rather than guarded.
- **`?tab`'s `OR` / `ci` / `color` blocks are still pre-19d**, now consciously parked in 19h (three
  mirrored copies, two of which that phase deletes) rather than left as an open debt.
- The three phases' own *HONEST CONCERNS* above are left as written — they are the historical record;
  what this pass closed is stated here.

No `.a.yaml` / `.u.yaml` was touched, so **no `jmvtools::prepare()` is needed** — 19k still owns that.

**FOLLOW-UPS.** 19h can start on this commit. 19k: the `diff ↔ ratio` re-ref, the remaining four
non-field ComboBox display values, and the vocabulary/`prepare()` items already listed. 19l: the
deprecation-warning corpus migration (127 remain, all `ci = "diff"` / `OR = TRUE` / short colour names
in the test corpus — harmless, but they hide new warnings).

---

#### Phase 19j — KEY 5: one aggregate core

**DONE (2026-08-15), both halves.** 2.0.0's own keystone is honoured: **`tab_apply_tests()` is
deleted**, and with it the second pass. The leaf computes the cells, **their interval** and **their
whole-table test** — because that is where the plan is. `tab_ci()` and `tab_chi2()` join
`tab_pct()`/`tab_tot()`/`tab_totaltab()` in `R/tab-steps-legacy.R` as superseded public wrappers, so
the whole pre-2.0.0 chain now lives in one quarantined file and nothing in the build calls a step.

**Verified, and the delta is PROVED rather than asserted.** Full suite **FAIL 0, WARN 133, SKIP 4,
PASS 6100**. The baseline was measured *in the same session, on the same reporter*, by stashing the
whole diff and re-running: HEAD gives **6073**, this tree gave **6073** before the new fixture file —
identical assertion count, identical result — and 6100 is 6073 + this phase's 27 new assertions.
(CLAUDE.md's recorded 6042 for 19i is a parallel-reporter count; the +31 is a reporter artefact, not a
change.) `dev/verify_golden_field_delta.R` with an **empty** declaration set reports **no delta** on
all **1788 cells of the 36 goldens** — no field, no column attribute, no `test` column, no `meta`
sub-field — and no `_snaps/*.md` or `_color_golden` fixture moved. So the *declared* golden delta of
this phase is EMPTY, on both halves.

**The interval.** **`CI_GEOMS` + `ci_dispatch()`** (`R/tab-agg.R`, beside `CI_METHODS`): one row per
(kind × var_kind × scale), carrying the engine, the `CI_METHODS` slot that names it, and the
`EST_SCALES` key it makes the column *estimate*. Its three consumers held **six** encodings of that
rule between them — `tab_ci()`'s engine `switch` + `ci_scale_of()` + `ci_method_of()`, and
`num_core()`'s `if/else` + `scale_num` + `method_num` — which is exactly how D8 happened (a chain that
could name a method the bounds were never built with). **`leaf_ci_plain()`** is `tab_ci()`'s per-cell
arithmetic with the plan *reconstruction* removed, on the matrices `tab_apply_reference()` already
holds; `plain_core()` gains `ci`/`ci_scale` off the settings spine, and stamps the display, the scale
and the method from the same lookup. **One slot, one interval**: the Woolf log-OR bounds when the odds
ratio IS the comparison, this producer's otherwise — the resolver already guaranteed they are mutually
exclusive, which is why the two could finally share the field.

**The test.** `chi2_compute_test()` and `chi2_write_contrib()` are **not rewritten** — the leaf calls
them, on its own single-`col_var` table, through **`leaf_chi2()`** / **`leaf_chi2_num()`** /
**`leaf_test_view()`**. That was the design decision worth taking: a matrix port of 180 lines carrying
an explicit byte-identity lock would have been a second implementation. What moves is not the
arithmetic but the *question* — the step had to reconstruct its metadata from markers
(`tab_get_vars`, `detect_totcols`, `tab_validate_comp`) **and mutate the table to make its own
preconditions true** (`tab_match_groups_and_totrows` / `tab_add_totcol_if_no` /
`tab_match_comp_and_tottab`, five warning branches between them); the leaf simply knows all of it, and
built the totals itself. The numeric ANOVA folds in the same way (`leaf_chi2_num`), so
`tab_chi2()` has no caller left.

**Two real defects, both found by the migration, both fixed.**

- ⚠ **A computation step decided the table's SHAPE.** `tab_chi2()` ungrouped the table it *returned*,
  so whether a `comp = "all"` table came back GROUPED depended on whether a test happened to run — and
  the jamovi **tier-2 test cache, which skips the step, therefore returned a different CLASS from a
  fresh build**. It was invisible only because `tab_ci()` ungrouped too; removing that half turned it
  into a red assertion mid-flight, and removing the other half closed it. `comp = "all"` is a LOCAL
  ungrouping now (`leaf_test_view`), and all four `comp = "all"` combinations agree.
  Fixture: `test-aggregate-core.R`.
- **The jamovi re-reference passed no `degf`.** It got away with it because `tab_ci()` derived one off
  the columns; calling the producer directly would have silently fallen back to *z* (the 9 %-too-narrow
  defect `test-degraded-attrs.R` records). Stated in the code where it now must be passed.

**`tab_plain()` gains a public `ci =` / `ci_method =`** — it had none, so the step chain was the only
route to a factor cell interval. It resolves the same `(or_ci, ci, ci_scale)` triple
`tab_resolve_settings()` derives, so `tab_plain(ci = "cell")` and `tab(ci = "cell")` agree **by
construction**, not by mirroring. Default `"auto"` is byte-identical to the previous hard-passed NULL.

**What actually died, and what did NOT — the roadmap's "What dies" list is wrong on three items and
the correction is the honest part of this report.** A wrapper's *entire job* is to reconstruct a plan
from markers on a table it did not build (`test-steps-legacy.R` calls `tab_ci()` on a chain that never
saw a settings spine), so:

| item | roadmap | reality |
|---|---|---|
| `detect_totcols` / `detect_refcol` / `detect_firstcol` | dies | **survives** in the wrapper + the exporters; stops running on the `tab()` path — that is the honest win |
| the 8-branch `case_when` | dies | **survives** in the wrapper; **collapses to 5 scalar lines** in the leaf. Two encodings of *different questions* ("reconstruct" vs "state") |
| the 2nd `ci = "ratio"` fold, the 3rd `stars`, the `degf` re-derivation | die | **survive** in the wrapper (that is what makes it self-contained); stop running on the pipeline |
| the engine `switch`, `ci_scale_of`, `ci_method_of`, the four `method_*` scalar unpacks | — | **die** → `CI_GEOMS` |
| `tab_apply_tests()`, the `spread_col` token | — | **die** |
| the jamovi `fmt_wrap` → `tab_ci` → `fmt_unwrap` round trip | — | **dies** |

**HONEST CONCERNS.**

- **`measure_stage()` was NOT deleted**, contrary to the plan. Its two values are still a real
  distinction — the contribution is a *different computation* from a plain colour stamp — so it now
  answers "which of the leaf's two passes stamps this measure" rather than "which step". Its `"chi2"`
  value is therefore a misnomer; renaming it would churn `test-color-config.R` for no behaviour, so it
  is flagged for **19l** instead of half-done here.
- **`tab_ci()`'s `set_wn(col, get_wn(col))` quirk did not travel to the leaf** (the maintainer's
  ruling), but **no golden surfaced it** — none is *grouped + factor + ci*, and `chi2_write_contrib()`
  still runs the same write, so `f_color_contrib` is unchanged. The declared `MATERIALISED_FIELDS`
  mode was therefore never needed. The behaviour change is real but unobserved: a grouped unweighted
  factor table with a difference interval now stores `wn = NA` where it stored `n`. `get_wn()`
  coalesces, so nothing rendered moves.
- **The whole-table chi2 is now one `agg_chi2()` call per col_var** instead of one batched call for
  all of them. The values are identical (`table_id` already partitioned by col_var); the cost is not
  measured — `test-benchmark.R` was not re-run. Worth a look in **19l** on a wide table.
- **`dev/verify_golden_field_delta.R` gained an order-insensitivity fix**: it compared the table
  attributes as an *ordered* list, and reported all 36 cases as CHANGED because the leaf sets `test`
  before `meta` where the post-assembly step set it after. Attribute order is a by-product, never a
  contract — but it means that check was previously stricter than intended, and any earlier phase that
  reordered an attribute would have been reported as a regression.
- **`jmv_tab3_rerefable()`'s `geom == "diff"` restriction was NOT lifted.** It is now only a *path*
  decision (the producer takes `ci_scale`), but lifting it flips four assertions that state the rebuild
  explicitly and changes which cache path a live toggle takes — **19k's**, with its cold/warm/re-ref
  lock and the live pass. The comment there says so.
- The two items the plan refused stay refused: `if (!all(is.na(a[[11]]))) "woolf"` (a magic-value test
  that should die, but moves the stamp on a degenerate all-NA OR table) and unifying `tab_ci()`'s
  NA-**base** device with `num_core()`'s NA-**results** one (they genuinely disagree on a mean *cell*
  reference row — a behaviour change wearing a refactor's clothes). Both → **19l**, and both are
  stated in the code where they live.
- No `.a.yaml` / `.u.yaml` was touched, so **no `jmvtools::prepare()` is needed** — 19k still owns that.

**FOLLOW-UPS.** 19k can start on this commit. 19l: `measure_stage()`'s naming, the per-col_var
`agg_chi2` cost, and the two refused items above.

---

#### Phase 19k — The jamovi boundary

**DONE (2026-08-15).** Full suite **FAIL 0, WARN 133, SKIP 4, PASS 6292**, against the inherited
FAIL 0 / WARN 133 / PASS 6042 — same warning count, +250 assertions, nothing red. The delta is
*proved*: `dev/verify_golden_field_delta.R` reports **no change** on any of the **1788 cells of the
36 goldens** (no field, no column attribute, no `test` column, no `meta` sub-field), and
`dev/verify_color_attrs.R` prints **IDENTICAL** over its 293 cases — checked against a baseline
saved from a `git worktree` of the pre-phase HEAD, so the "before" is the real one. No `_snaps/*.md`
and no `_golden/` fixture moved.

**The rule this phase installs: the module states an intent, R resolves it.** Nothing between a
control and the argument it names, and no rule computed twice.

**The seven hand-mirrored rules are gone.** `jmv_population_descriptor()` was a line-for-line copy of
`tab_cache_keys()` — *in the file that also reads the real one* — and is now that call. The digits
magnitude floor is **`num_digits_floor()`** (`R/tab.R`), shared by `num_core()` (where the column is
built) and the tier-4 re-paint (which must reproduce it exactly). The multiplier keywords are
**`REG_MULTIPLIER_KEYWORDS`**. The staged-comparison predicate is `jmvtab_reg_staged()` — which
existed for exactly that and whose own caller inlined it instead, so only the tests reached it.
And **`jmv_apply_display()` is deleted**: it was `tab_apply_display()` plus one block, and that block
was **D11**.

**The vocabularies are tabxplor's, both ways.** Both `.a.yaml` files spell every List value the way
the R argument does: `chi2` → **`test`**; `OR` **deleted** (`display` prints the odds ratio, `ref2`
picks its 2×2); `color` → the full measure words; `ci` → the four anchor values; `display` → presets
that are all legal `tab(display =)` values (`num_ci` collapses `pct_ci` + `mean_ci`; `{or} ({pct})`
replaces `OR_pct` and teaches the `{}` grammar); `method_cell` gains `beta`. On the reg side
`exponentiate` / `at` / `estimate_display` are **deleted** for `effect` × `measure` × `display`, and
**`jmv_reg_estimand_opts()` — 19e's translator, written to be deleted here — is gone**. `color`
becomes a MEASURE (D25's derived allow-list: `auto` / `no` / `adjustment` / `between_groups`), which
makes differentiator #3, the crude-vs-model comparison, reachable from the UI at all. New per-numeric
-predictor **`shapes`** picker → `tab_reg(shape =)`. `stats = opts$stats` — a key `.opts()` never set
— is dropped for `tab_reg()`'s own default GOF set.

**The JS rules are GENERATED.** `dev/generate_jamovi_js.R` rewrites a marker block in each
`jamovi/js/*.js` from `REG_OUTCOME_KINDS` / `REG_FAMILY_UI_LABEL` / `REG_ESTIMANDS` / `REG_SHAPES` /
`DISPLAY_COMPARISON`; `check` mode fails on a stale block and `test-jamovi-vocabulary.R` runs it as
an assertion. `reg_detect_family()` now READS `REG_OUTCOME_KINDS`, so the JS is generated from the
same rule rather than claiming in a comment to match it. That deleted `detectFamily` /
`familyOptionsFor` / the two label maps, and **`anyProbScale()`**, whose whole content — "a marginal
ratio needs a probability scale" — 19e made false. A marker block, not a second `.js` file:
whether jamovi's bundler resolves a `require()` is not testable here.

**`.run()` is weights → build → render.** `anova` was the last option travelling as a global
(`options()` + `on.exit`), which also baked it into the tier-3 base key although the p-value line is
materialised at DISPLAY. It is **`tab(anova =)`** now — a real argument on `tab()` / `tab_num()`,
stored in `meta$render_extras` only when stated (so no golden moves) and read back by `tab_anova()`,
which both `test_display_rows()` callers pass. A welch↔classic toggle became a tier-4 re-derive.

**The tier-3 cache: `jmv_tab3_rerefable()` is now a stated rule, not a list.** *Everything the re-ref
RECOMPUTES may differ; everything it copies must not.* So `ref` / `ref2` / `geom` / `ci_method` left
the identity set: a **diff ↔ ratio** toggle and a **CI-method** toggle are re-refs, not rebuilds.
Both restrictions were vestigial — 19e's because the re-ref went through `tab_ci()` (the DIFFERENCE
engine) until 19j replaced it with `leaf_ci_plain()`, which takes `ci_scale`; and **D12**'s because
the four `method_*` keys never reached the tuple at all, `reapplied` naming a `"ci_method"` that is
not a key of `opts`. The re-ref restamps `meta$scale` and `meta$ci_method` from the same `ci_res` the
bounds come from, which is what makes a geometry change safe (19b's D8/D19 class). The
`color == "auto" && ci == "diff"` exclusion in `jmv_reref_shape_ok()` is gone with them.

**D13** — `tab_cache_keys()` gets a real `filter_expr`. It was a hardcoded `NA_character_`, so two
calls differing only by their filter shared every tier-0/tier-1 key. The ctx carries `filter_expr`
(NA = none) and `with_filter` is **derived** from it — one fact, one carrier.

**`trials`: one rule, R's.** The module took `max()` **itself** for any integer outcome — the same
rule as `trials = TRUE`, but silently and on a different trigger, so the jamovi behaviour was not
reproducible from the R API. `trials` accepts **`NA` per dependent = "take the observed maximum"**,
applied only where there IS one (a factor / 0-1 outcome stays an ordinary binary logit, where
`trials = TRUE` used to run `max()` on a factor and error). Explicit and automatic counts can now
mix; a name matching NO dependent aborts, because that is a typo, not a mixing request.
⚠ Found by the fixture: the reref gate read the RAW `trials`, so a table of ordinary binary logits
carrying `c(dep = NA)` looked grouped-binomial and lost the digest fast path entirely. It reads the
RESOLVED `trials_for(d)` now.

**Three measured live JS bugs, fixed**: `forceNaForCompare()` wrote `na = "drop_all_models"`, a value
removed in z13, on every `compare` change (it pushes back to `drop_by_outcome`, which is what makes a
comparison valid); `applyWtEnables()` greyed `ids`/`strata`/`fpc`/`nest`, four options deleted in
z14-i; `resetPath_changed` disagreed with the `.a.yaml` about the default filename.

**New `test-jamovi-vocabulary.R`** is the enforcement, not a convention: every List option's value
set must EQUAL the R vocabulary it names (`names(MEASURES)` filtered by `producers`/`channels`,
`CI_METHODS` slot by slot with its default, `TAB_ARG_VALUES`, `REG_EFFECTS_VALUES`,
`REG_MEASURES_VALUES`, the `measure_own_ref()` allow-list), every `display` value must be one
`tab_apply_display()` accepts, every `.u.yaml` `optionPart` must be a value its option declares, and
the generated JS must be what the R tables would write today.

**HONEST CONCERNS.**

- **The module is INERT until the maintainer runs `jmvtools::prepare()` + rebuild.** `measure`,
  `shapes` and the renamed `test` do not exist in the generated `.h.R`, so `self$options$…` reads
  NULL. Every read in both `.opts()` carries a `%||%` fallback, so the module *runs on defaults*
  rather than aborting — but the new controls do nothing until the rebuild, and the live pass
  (collapse boxes, the shape select, the moved `display` ComboBox, export) is the maintainer's. Do
  not read this summary as "the UI changed".
- **Renaming `chi2` → `test` and deleting `OR` lose those settings in saved `.omv` files** — jamovi
  keys analysis options by name. Accepted per the standing no-back-compat ruling for the module;
  recorded because it is data loss, not a rename. Two guards soften the window: a retired `ci`
  spelling resolves silently (no lifecycle warning into the results panel) and a retired `display`
  value **degrades to "the display the table was built with"** instead of aborting the render — a
  `tryCatch` on a GENERATED-artefact-lags hazard, the same discipline as the `%||%` defaults. It
  translates nothing; the value is dropped.
- **The `shape` select is a best guess against a DOM only the running app has.** Same class as the
  existing pickers (it reuses their get/write/reconcile idiom on the same numeric row), but it is
  asserted by construction, not seen.
- **`jamovi/js/*.js` has no syntax check here** — no node/V8 on this box (the `node` R package ships
  a Windows binary). The suite balance-checks brackets and the generator diff; that is all. → 19l.
- **The digest fast path is now unreachable from the UI for `color = "adjustment"` and for any
  `shape`.** Both correct (they need the fitted object / a different model) and both were previously
  unreachable *because the options did not exist*, so this is a real new refit cost on those two
  paths. Unmeasured. → 19l.
- **D22's "renders void" note is per COLUMN but reads as per TABLE**: `display = "num_ci"` on a table
  that does have intervals still notes it, because the `add_n` total column carries none. 19d's own
  rule, not a regression — recorded in the 19l hand-over.
- The 133 warnings are unchanged deprecation nudges from the test corpus (`ci = "diff"` / `OR = TRUE`
  / short colour names). The corpus migration is still 19l's.

**FOLLOW-UPS.** 19l can start on this commit. Maintainer: `jmvtools::prepare()` +
`jmvtools::install(home = "flatpak")` + the live pass. 19l: a real JS syntax/lint gate, the two
refit-cost measurements, D22's note scope, and the deprecation-warning corpus migration.

---


#### Phase 19l — Harvest 1: the deletion pass

**DONE (2026-08-15).** Full suite **FAIL 0, WARN 133, SKIP 4, PASS 6301**, against the inherited
FAIL 0 / WARN 133 / PASS 6292 — same warning count, +9 assertions, nothing red. Both proofs pass, run
before AND after the comment sweep: `dev/verify_golden_field_delta.R` with an **empty** declaration
set reports no delta on any of the **1788 cells of the 36 goldens**, and `dev/verify_color_attrs.R`
prints **IDENTICAL** over its 293 cases against a baseline saved from the pre-phase tree. No
`_snaps/*.md` and no `_golden/` fixture moved. That is the phase's whole claim: nothing moved.

**THE CENSUS — the honest headline first.** Measured against the study's 2026-08-13 baseline
(pre-19a), the package **grew**:

| | before | after 19a–19k | after 19l |
|---|---|---|---|
| R/ total lines | 38 784 | 43 667 | **43 488** |
| code | 19 853 | 21 691 | **21 650** |
| comment (share) | 15 909 (41 %) | 18 700 (42.8 %) | **18 567 (42.7 %)** |
| top-level functions | 900 | 1036 | **1032** |
| median function length | 17 L | 17 L | **17 L** |
| user messages (`cli_*`) | 163 | 200 | **197** |
| … in `tab.R` | 50 | 29 | **29** |
| … in `tab_reg.R` | 67 | 65 | **62** |
| **share at the two boundaries** | **72 %** | 47 % | **46 %** |
| `reg_build` / `tab_reg` / `plain_core` | 1307 / 763 / 616 | 1352 / 849 / 654 | all still bigger |
| `num_core` | 700 | 561 | 561 |

**What did not shrink, and why** — the report §19l asks for, not hidden. Phase 19 traded scattered
implicit rules for **declared fact tables plus the prose that explains them**, and added four modules
(`row-model.R`, `table-spec.R`, `tab-shape.R`, `reg-estimand.R`) plus a **1272-line quarantine**
(`tab-steps-legacy.R`) that is dead on the build path by design. A key that stores a fact costs a
table and a comment; what it saves is scattered *decisions*, which are cheap in lines and expensive
in correctness. So the line count is the wrong scoreboard, and the study said so when it named the
diagnostic: **the share of everything the package says to a user that is said while negotiating
arguments fell 72 % → 46 %**, and `tab.R`'s own message count nearly halved. Ten phases did not touch
the two worst functions (`reg_build`, `tab_reg`) and both are bigger; that is the honest gap, and it
is a decomposition problem, not a fact-storage one.

**TWO REAL DEFECTS**, both verified against `DESCRIPTION` before touching anything:

- **`withr::with_options()` called unguarded on a Suggests-only package** (`jmvtab-cache.R`) — a hard
  failure in the live jamovi module on any machine without `withr`. `reg-assumptions.R` states that
  exact rule and hand-rolls base R to obey it. Now `options()` + `tryCatch(finally=)`.
- **Three `requireNamespace()` guards on packages in `Imports:`** (`nnet`, `MASS` ×2, `tab_reg.R`) —
  the guard can never be FALSE, so three `cli_abort`s were unreachable.

**WHAT WAS DELETED.** ~500 lines net, all of it byte-identical:

- **7 dead functions**, each verified by a repo-wide `grep -rnw` whose only hit is the definition:
  `set_empirical_tips()` / `set_assumptions()` (write-only accessors, never written),
  `reg_footer_labels()` / `reg_footer_per_term()`, `tr_()` / `po_to_dt()` (a 40-line `.po` parser
  kept for an i18n phase that shipped using potools instead), `shape_from_fit()`. Plus a dead
  `row_var <- tab_get_vars(.data)$row_var` in `arrange.tabxplor_tab()`.
- **`measure_stage()` — deleted, not renamed.** 19j flagged its `"chi2"` value as stale. It was worse:
  the body *is* `identical(measure_builds(m), "contrib")`, all three callers asked only `== "chi2"`,
  and the `"leaf"` return was compared to nowhere — a two-valued predicate wearing a string's clothes
  whose second value named a step 19j had removed. The callers ask `builds` directly.
- **`reg_fam_logscale()` — deleted, and with it a WARNING that had become false.** It claimed to be
  "read by fmt_class.R's colour engine AND its legend — the single source that replaced their
  sync-by-comment pair". Measured: neither reads it, and has not since 19b — both reach the fact
  through the column's **stored `scale`**. Its one caller picked `"log_coef"`, which `REG_ESTIMANDS`
  declares per row (`est$scale`), so the `%||%` fallback beside it was unreachable too. A WARNING
  naming consumers that no longer exist is the sync-by-comment disease it claimed to have cured.
- **~200 lines of commented-out code**, 26 blocks, each verified comment-only before deletion: a dead
  `tab_vars` resolver, a dead `group_vars_totals` builder, `ci_formula_factory`, `format.pillar_shaft_fmt`,
  the pre-13a break tables, palette and `arrange()` REPL scratch, an 18-line `vec_assert` block, and
  the duplicate `pct_formula` / `diff_formula` copies in the legacy file. ⚠ **Two blocks stay**: the
  `totcol_range` dormant feature (`tab.R`, `tab-export-prep.R`), which the maintainer ruled *keep,
  dormant* and which carries its own explanation. Reported as a standing tension with rule 1 rather
  than resolved unilaterally.

**THE 29 `exists()` LOCAL-BINDING GUARDS IN `plain_core` — the flagship.** The factor leaf created
~14 optional data.tables (`tabs_wn`/`_pct`/`_diff`/`_mean`/`_rr`/`_or`/`_or_ci_inf`/`_sup`/`_pvalue`/
`_totn`/`_neff`/`_w2`, `refcols_vector`, `refrows`) as bare locals, then asked the **environment**
whether each existed — 29 times, in four different spellings. They are **declared once** now, with the
list as the documentation of what the leaf may or may not compute; every guard is `!is.null()`. Same
medicine 19i applied to the ctx, and the same reason: an undeclared name is indistinguishable from a
mistyped one, and a typo reads as "absent" instead of erroring. Two more went with them: `or_refrows`
joined the declaration block `18z16-iv` had already built for its siblings in `tab_apply_reference()`,
and `tab_assemble_tables()`'s `var_labels` guard **could never be FALSE** — it is a declared `new_ctx()`
field, exactly the class 19i's declaration was meant to retire. Only the two legitimate `exists()`
calls remain (`.Random.seed`, the `svyglm` namespace probe).

**WHAT STOPPED GUESSING** — each a read of a fact already in scope, and each deletes the guess:

- `"var" %in% names(tab)` (`tab-export-prep.R`) → `rv$var_col`. The **last** consumer sniffing for a
  column literally named `var`, with the declared answer already in scope and used 26 lines later.
- `tabs[["row_var"]]` ×2 (`tab_transpose`) → the declared `var_col`, from the
  `tab_declared_vars()` call already on the line above.
- **`stri_detect_regex(names(tabs), "^Total_")`** in `tab_compact()` → `is_totcol()` + the column's own
  `col_var`. It hardcoded the **English** default, so a table built with `total_names = "Ensemble"`
  silently kept the qualified name — while `tab.R`'s sibling site does the same job through
  `total_names[2]`.
- **`"^Total|^Ensemble"`** in `kable_tabxplor_style()` → `is_totcol()` / `is_totrow()`. The last place
  in the package where a total was identified by a *word*, and its row half read column 1
  positionally. ⚠ The function is exported and deprecated, so it was **fixed, not deleted** (1.3.1
  public surface); whether it should go at all is a 19n release-review question.
- The **`_sd` name suffix** (2 sites) → a declared `role = "sd"` on the Excel twin, stamped by
  `mat_sd_twin()` where it is built. `set_role()` is new (the attribute had a getter and no setter,
  so a column built by COPYING another could not restate it).

**THE FAMILY WHITELISTS.** 19a absorbed 14 of 21; **11 sites in 4 sets survived**, none covered.
Three predicates absorb them: **`reg_fam_percategory()`** (4 copies of `c("multinomial","ordinal")`),
**`reg_fam_count()`** (3 copies of `c("poisson","quasipoisson")` — neither
`reg_fam_overdispersed` nor `reg_fam_disp_estimated` is that set), and **`REG_USER_FAMILIES`**, the
*public* vocabulary promoted out of a local in `tab_reg()` and stated as
`setdiff(names(REG_ESTIMANDS), REG_FIT_ONLY_FAMILIES)` — so the two cannot drift.
`REG_FIT_ONLY_FAMILIES` was **defined and never used** while its literal was written twice; it is used
now. `reg_fam_binary()`'s body is **restated as a derivation** from `REG_FIT_FAMILY` (13 call sites
keep the function; what goes is a third copy of a list declared one file over).

⚠ **"Is this a grouped binomial" was written three times and one copy disagreed** — and the
disagreement was **dead code**: `reg_crude_key()`'s `c("binomial", "rd")` can never see `"rd"`,
because the line above returns for it. One predicate (`reg_is_grouped_binomial`), and the
compound-formula clause — part of the fact, since a compound formula controls its own LHS — is stated
once instead of being present in two copies and absent from the third.

**`"all_col_vars"` — the helper columns declare themselves** *(maintainer-requested)*. The tag's name
**lies**: it means "belongs to no col_var", not "to all of them", and the legacy `tab_tot()`
grand-total column uses the same string for the opposite meaning. The `add_n` `n` column and the
`add_pct` `col_pct` column now carry `col_var = ""` (which every "not a real col_var" filter already
excluded, identically) plus a stored **`role`** — `"n"` / `"pct"`, the values a `tab_reg()` count
column already carries — behind one predicate, `fmt_is_helper_col()`. The legacy grand total **keeps**
`"all_col_vars"`, so the string ends the phase with exactly one sense.

⚠ **19h's cost estimate was wrong, and the correction cuts both ways.** It said this "regenerates
every `add_n` golden": **no golden fixture uses `add_n` at all** (36 files, none), so the migration
moved **zero** goldens and was far cheaper than recorded. But that also means those columns had **no
structural coverage whatsoever**, so per rule 7 it ships with a new fixture in
`test-display-extras.R` asserting the stored `(col_var, role)` pair on both helpers, the predicate's
selectivity, and the xl-only/text-folded split.

**`Model_MR` → `Model_RoM`** (maintainer's call, adopted): `MR` collides with several established
meanings and must be looked up; `RoM` reads as *"Ratio of Means"* on sight, which is what a header
this package invented should do when there is no discipline convention to inherit. The mixed case is
the signal that it is a phrase, not an acronym. Five `REG_ESTIMANDS` rows; the three readers (the
column name, `reg_measures()`, the generated `?tab_reg` section) follow automatically. `"MR"` stays an
accepted `measure` spelling. Also: the **19g re-indent**, 98 lines of pure whitespace across the three
column builders.

**HONEST CONCERNS.**

- **The two worst functions are untouched and still growing.** `reg_build` 1307 → 1352, `tab_reg` 763
  → 849, `plain_core` 616 → 654. Nothing in Phase 19 was aimed at them, and no key collapses them —
  they are long because they *assemble*, which is sequential work. Naming it as the largest remaining
  structural item rather than pretending the harvest covered it.
- **One 19j hand-over was DECLINED, with its reason** (filed in the roadmap, do not re-issue as
  written): making `plain_core`'s `woolf` stamp read the plan (`or_ci`) instead of
  `!all(is.na(ci_inf))` would be **wrong**. `ci_method` is a column-scalar, and the reference column,
  the total column and any degenerate 2×2 carry all-NA bounds *by construction* — so reading the plan
  would stamp `"woolf"` on columns whose bounds were never computed and make the legend name a method
  for them: the exact **D8** failure the surrounding comment cites as its reason to exist.
- **Two corrections to the record, both overstating what exists.** (i) There is **no committed JS
  bracket-balance check** — CLAUDE.md and the roadmap both claim one; `tests/` opens no `.js` file,
  and `test-jamovi-vocabulary.R` verifies only the *generated blocks*, a few dozen lines of 1610.
  (ii) The deprecation corpus is **~136 sites, not 385**: 177 of the raw hits are **permanent silent
  aliases** (`color = "diff"` 156, `color = "OR"` 21) that `COLOR_ALIASES` never deprecated by design.
- **Three items the plan listed were NOT built**, per the session's agreed scope (pure deletion): the
  test-corpus deprecation migration, the three cost measurements 19j/19k asked for, and the JS syntax
  gate (no node/V8 on this box). All filed into the roadmap with what they need.
- **`sd_cols` changed discriminator**, from a name suffix to a stored role. Provably equivalent for
  every column `mat_sd_twin()` builds — but a user who hand-set `display = "var"` on a mean column
  would previously not have been treated as an sd twin and now still is not (the role is what is
  read, not the display). Stated because the intermediate design *would* have changed that.
- `NEWS.md`: untouched. This phase has no user-facing change.

No `.a.yaml` / `.u.yaml` was touched, so **no `jmvtools::prepare()` is needed** — 19k's maintainer
rebuild + live pass is still the outstanding one.

**FOLLOW-UPS.** 19m can start on this commit; the roadmap's 19l entry now carries everything filed
(the two behaviour decisions, the five newly-found structural items, the two record corrections, the
three owed measurements). 19n: the deprecation-corpus migration, `po/R-fr.po` (the estimand notes and
two family display names are still untranslated), and the vignettes.

---




#### Phase 19l — Harvest 1, pass 2: the deletion pass continued

**DONE (2026-08-15).** Full suite **FAIL 0, WARN 1, SKIP 4, PASS 6295**, against the inherited
FAIL 0 / WARN 133 / PASS 6301. Both proofs pass with an EMPTY declaration set:
`dev/verify_golden_field_delta.R` reports no delta on any of the **1788 cells of the 36 goldens**
(no field, no column attribute, no `test` column, no `meta` sub-field) and `dev/verify_color_attrs.R`
prints **IDENTICAL** over its 293 cases against a baseline saved from the pre-phase tree. The only
`_snaps/` churn is four snapshot CODE lines in `render-html.md` (the calls lost a retired argument);
the rendered bytes are unchanged.

**The headline number: deprecation warnings 133 → 1**, and the one left is a genuine statistical
notice (a Poisson over-dispersion advisory), not a deprecation. The suite can surface a NEW warning
again, which it could not before. PASS is −6 because the phase deleted ~28 assertions that existed
only to compare two render engines and added 22 new ones.

**Three mechanical sweeps ran first**, all under `LC_ALL=C` — ⚠ the box is `fr_FR.UTF-8` and fr
collation does NOT group identifiers containing `_`/`.`, so any `sort | uniq` token census silently
under-counts (pass 1's zero-caller list may hold both false positives and misses). A zero-caller
sweep over `R/ tests/ vignettes/ dev/ man/ NAMESPACE jamovi/ _pkgdown.yml` with `S3method()`/`export()`
resolved; a "what still guesses" sweep (rendered labels, name prefixes, positional picks, in-band
separators, silent length fallbacks); and a **ghost sweep** — every `foo()` named in a comment that is
defined nowhere. The third is committed as **`dev/verify_no_ghost_functions.R`**, because that class
is what pass 1 found in `reg_fam_logscale()`: a comment naming consumers that had not existed for two
phases. Its definition list comes from the loaded NAMESPACE, not a regex (`tab_xl <-` on its own line,
the `fmt_field_factory` idiom and plain aliases all defeat the regex — 19 false ghosts on the first
try). It is a REPORT, not a gate: a historical *"X is DELETED because…"* note KEEPS, a live claim
running through a dead function FIXES. 149 sites remain, all read, the class-(b) ones fixed.

**THE CENSUS.** R/ **43 488 → 42 997 lines**, comment 18 567 → 18 431, `cli_*` messages 195 → 192
(`tab.R` 29 → **23**, `tab_reg.R` 62 — unchanged, see the honest concern below), options 41 → **39**,
exports 92 (one export became a defunct stub). `tab.R` **7918 → 3915**.

**THE KABLEEXTRA ENGINE IS DELETED** *(maintainer's ruling)*. `render_kableExtra_engine()` (164 L),
the zero-caller deprecated `kable_tabxplor_style()` (192 L incl. docs, whose own body carried
`# -- unreachable so far only because nothing calls this`), and the two options no other path read.
`engine =` is accepted and ignored. ⚠ **kableExtra stays a Suggests and the CLASS is load-bearing**:
`tab_kable_join()` stamps `kableExtra` so its print/knit_print route the fragment to the Viewer and
bind the bootstrap tooltips — stated in the file header so nobody "cleans it up". The maintainer's
condition — that `tab_export("html")` still answer a plain frame — needed no new code but is now
ASSERTED, on three inputs: a plain tibble (degrades to a bare `<table>` with a note), a table that
merely LOST its class with its fmt columns intact (**not** degraded — renders fully coloured, which is
`test-degraded-attrs.R`'s contract), and a real tab.

**`tab.R` IS SPLIT** *(maintainer's ruling)*, whole functions only, no behaviour change:
**`R/tab-leaf.R`** (2595 L — the aggregate core), **`R/tab-chi2.R`** (465), **`R/tab-display.R`**
(550), **`R/tab-deprecate.R`** (310). ⚠ The one constraint is collation: `tab.R` sorts AFTER every
`tab-*.R` in the C order R uses, so a new file may read tab.R's top-level objects but not the
reverse, and the DERIVED `globalVariables()` tail must stay last. Before that, **the quarantine was
finished**: the six helpers with no caller outside `R/tab-steps-legacy.R` moved into it — the four
that MUTATE a table to make a step's preconditions true (`tab_match_groups_and_totrows` /
`tab_add_totcol_if_no` / `tab_validate_comp` / `tab_match_comp_and_tottab`, out of `tab.R`) and the two
that RECONSTRUCT which column a step compares against (`detect_refcol` / `detect_firstcol`, out of
`fmt_class.R`). `detect_totcols()` did NOT go: one live caller, `tab_add_n_pct()` on the exporter path.

**`leaf_defuse_vars()`** collapses the largest verbatim duplication left in the package: the
`enquo → quo_miss_na_null_empty_no → ensym`/`eval_select` cascade plus the `svy_abort_wt_design` tail,
written THREE times (`plain_core`, `num_core`, `tab_aggregate_num`) and differing in exactly one
thing — whether `col_var` is one symbol or a tidyselect of several. The quosures are captured BY THE
CALLER, so it is an ordinary function: no NSE forwarding, no `caller_env()`.

**TWO LIVE DEFECTS, each with the fixture that fails without it** (`test-19l-defects.R`, 22 assertions):

- **19e's two new estimands got NO model checks at all.** `reg_checks_for()` filters on `sp$family`,
  which is the estimand's `fit` — an internal LINK key. `REG_CHECK_FAMILIES` named `rr` but not `rd`
  or `mr`, so `tab_reg(family = "binomial", measure = "difference")` and
  `tab_reg(family = "gaussian", measure = "ratio")` reported no linearity / dispersion / influence /
  collinearity row and drew no panel — **silently**. Measured before the fix: 4 checks vs **0**. It
  cannot be derived in place (`R/reg-assumptions.R` loads before `R/reg-estimand.R` and consumes the
  vector at build time), so the exhaustiveness is a **build-time `stopifnot()` at the end of
  reg-estimand.R** — adding a link key now fails to load rather than silently losing its diagnostics.
  ⚠ Fixing it EXPOSED two latent arms: `rd_link_y()` and `rd_resid()` dispatch on the family in order
  and `"mr"` matched none, so it would have fallen to the ordinal branch and to `pbinom`. Both read
  `reg_check_family_of()` now — the distribution behind a link.
- **`tab_html(tab(data, marital), transpose = TRUE)` aborted** "subscript out of bounds":
  `compacted2 <- length(real_col_vars) > 1` sends length **0** down the `else`, which indexes `[[1]]`,
  and a no-col_var table's sentinel is filtered out of `real_col_vars` entirely.

**FOUR NEAR-MISSES**, each wrong the moment a precondition moves: the lone-total rename built a regex
from the USER's `total_names[2]`, unescaped (it reads the stored `totcol` flag now — the same job
`tab_compact()` already did that way); `legend_specs()` asked `!is.null(reg_call(x))` where the
STORED kind is the question (they diverge on a reg table whose `spec$call` was never attached, which
`spec_bind()`'s `%||%` makes reachable); `reg_strip_model_prefix()` matched `"^Model .+ \\((.+)\\)$"`
— an English word plus a space that NO producer has emitted since Phase g, so it silently returned
its input, and is deleted; and **`Obs_MR` survived pass 1's own `Model_MR → Model_RoM` rename**.

**DELETED.** `kable_tabxplor_style()` + the engine (~360 L) · `LVL_ROLES` (declared, never read) ·
`get_chi2()` (a one-line alias whose comment claimed it kept pre-2.0.0 user code running — it was
never in NAMESPACE and has no man page, so no user could call it) · the `if (FALSE) c(gettext(...))`
potools anchor in `fmt_class.R` (⚠ verified with `potools::get_message_data()` that **all 14 msgids
still extract** from the `CI_METHOD_LABELS` closures without it — and the twin in
`R/reg-assumptions.R` is NOT deletable, its nouns are bare strings `gettext()`ed dynamically) ·
~200 lines of commented-out code in 30 sites, incl. `css_deja_vu_sans_condensed()` (whose own header
said *"Not working"*) and the commented `as_fmt()` generic · **the ~100-line palette-review recipe
moved to `dev/color_palette_tools.R`**, which is where CLAUDE.md says those tools belong · ~14 dead
formals with their call sites, of which the `lang` chain is the real one: `with_legend_lang()` sets
the render locale in the calling ENVIRONMENT, so threading it through four legend signatures changed
nothing. ⚠ `legend_break_tokens()` KEEPS its `lang` — it passes it down to `legend_num()` for the
French decimal comma. I removed it, the i18n tests caught it, and the restore is commented.

**THE CORPUS AND THE TAUGHT SURFACE MIGRATED** *(maintainer's ruling)*: ~120 sites —
`ci = "diff"` → `ci = "ref"` (74, ⚠ NOT on `tab_ci()`, whose step vocabulary owns that word natively),
`OR = TRUE`/`"OR"` → `display = "{or}"` + `ref = "first"` (⚠ only where no explicit `ref` was given —
that is `tab_deprecate_or()`'s actual rule), `fmt(in_totrow =)` → `row_kind =`, two incidental
`pmap(.f = tab_many)` batches → `tab()`. What STAYS is what is deprecated ON PURPOSE: the tests whose
SUBJECT is the deprecation. Plus the six public sites — `forest_plot()`'s and `tab_compact()`'s own
roxygen examples, and the four EN/FR vignette chunks; the programming vignette stopped teaching
`tab_many()` as "the engine behind `tab()`" (it is a shim), and both option lists dropped the three
deleted options.

**HONEST CONCERNS.**

- **`tab_reg()` is untouched and is now the single biggest structural item left.** It has NO argument
  boundary: of its 821 lines ~550 are argument resolution before one `reg_build()` call, and **62 of
  the package's 192 user messages live there** — the number that did not move. Inside sit ten ad-hoc
  local closures (`family_for`, `est_for`, `do_exp_for`, `trials_for`, `color_for`, …) and two
  near-identical `specs <- purrr::map2()` literals; the code already calls its own `do_exp` /
  `effect_shape` / `eff_word` spec fields *"views of `est`, kept as fields because ~15 build sites
  read them by those names"*. The key is `reg_resolve_args()` + `new_reg_spec()` — 19i's and 19g's
  medicine, one layer over. It is a resolver redesign, not a deletion; filed to 19m.
- **~20 dead formals were NOT removed** (`build_total_rows(totvars)`, `agg_anova(group_id)`, five in
  `tab_reg.R`, …). They sit at POSITIONAL argument slots in long calls, and I made exactly that
  mistake once in this session (dropping `legend_break_tokens(lang)`, caught by the i18n tests). The
  remaining ones need a call-site-by-call-site read; the safe subset (13) is done.
- **Tracks 5 and 6 of the plan were not reached**: the declared-vocabulary single-sourcing
  (`REG_FAMILIES` — three per-family name tables that already disagree, `TAB_PLACEHOLDER_COL_VARS` —
  the sentinel set filtered by hand at 8 sites with 4 different contents, `EST_SCALES$default_display`
  — `fmt()` and `new_fmt()` are two copies of one rule that DISAGREE on `scale = "mixed"`), and the
  12 silent length-fallback guards (the class that hid D1's greyed footer for two phases). All are
  measured and filed to 19m, none is a correctness bug today.
- **`_pkgdown.yml` still lists `kable_tabxplor_style`.** It exists (as a defunct stub), so the site
  builds; whether a defunct function belongs in the reference index is a 19n release-review call.
- **The `totcol_range` dormant feature is untouched**, per your ruling — including the three now-
  unreachable `tmpl` branches in `tab_fold_addn_incell()` that follow from its hardcoded `rng <- NULL`.
- No `.a.yaml` / `.u.yaml` was touched, so **no `jmvtools::prepare()` is needed** — 19k's maintainer
  rebuild + live pass is still the outstanding one.

**FOLLOW-UPS.** 19m can start on this commit and now carries: `reg_resolve_args()`/`new_reg_spec()`,
Tracks 5 and 6 above, the ~20 positional dead formals, and pass 1's own filed items. 19n: `po/R-fr.po`
(the estimand notes and two family display names are still untranslated), the vignette prose pass, and
the `_pkgdown.yml` question.

---

#### Phase 19m-i — Harvest 2: open integration 1

**DONE (2026-08-15).** Full suite **FAIL 0, WARN 1, SKIP 4, PASS 6402**, against the inherited
FAIL 0 / WARN 1 / PASS 6295 — same warning count, +107 assertions, nothing red. Both proofs pass:
`dev/verify_color_attrs.R` prints **IDENTICAL** over its 293 cases against a baseline saved from the
pre-phase tree, and `dev/verify_golden_field_delta.R` reports **only the declared delta** on all
**1788 cells of the 36 goldens**. One golden family moves (`f_ci_cell`) plus 11 lines of
`_snaps/golden.md`; everything else is bit-identical.

**Scope (maintainer's rulings at plan time)**: the theme is **hard rules 2 and 4 taken to
completion** — nothing may depend on a rendered label, a name prefix, a positional vector or an
in-band separator; a fact lives in ONE table. The display-grammar table, the options cluster and
`tab_reg()`'s argument boundary go to **19m-ii**, filled in with everything measured here.

**THREE LIVE DEFECTS, each with the fixture that fails without it** (`test-19m-defects.R`, new).

- **`tab_collapse_total_rows()` keyed on `group_vars()[1]`**, but `tab_compact()` groups by
  `c(merge_tab_vars, "row_var")` — so with tab_vars it keyed on the **tab_var**. The declared answer
  (`tab_declared_vars()$var_col`) was already read on the function's first line. ⚠ Fixing the key
  was not enough: the collapse also compared every total block in the WHOLE table, so with tab_vars
  it reported *"the variables have different total rows"* (blaming `na = "drop"`) on any table whose
  sub-tables merely differ from each other — i.e. `common_totrow` was **inert** on the shape 19f
  made possible. It compares and collapses **within a tab_vars key** now: "the shared population" is
  the SUB-population when there are tab_vars. Without tab_vars it is byte-identical.
- **`tab_apply_reference()` re-derived the total COLUMN from the literal `"Total"`** while taking
  the row totals as declared vectors. Its second caller, `jmv_tab3_reref()`, passes **post-rename**
  names, so with `total_names = c("Total", "Ensemble")` nothing matched: measured, the re-referenced
  odds ratio came back **1 everywhere** against a rebuild's real values. Masked only because
  `po/R-fr.po` translates `"Total"` → `"Total"`. It takes a `totcol_vector` now — the same
  expression `leaf_ci_plain()` is handed 20 lines below.
- **`tab_shape()`, the EXPORTED shape reader, reported `col_vars = "no_col_var"`** for a table with
  no column variable. Consequences taken, not guarded (ruling): `tab_supports(list, "compact")` and
  `tab_check_same_col_vars()` now accept a list mixing a no-col_var table with a col_var one, and
  `tab_transpose()` names its label column `"variables"` instead of the sentinel.

**Found while implementing, pre-existing, and worse than the leak that surfaced it**:
`tab_stack_tables()` bound on the FIRST table's column names, so `TAB_OPS$compact`'s declared
NESTING rule ("every table's set a subset of the widest") **depended on list ORDER** — narrow-first
silently DROPPED the wider table's extra columns, wide-first ERRORED. It binds on the UNION now,
padding a table that lacks a column with NA cells from the merged ptype.

**RULE 4 — the vocabularies written twice.**

- **`TAB_PLACEHOLDER_COL_VARS`** + `is_real_col_var()` / `is_placeholder_var()`: eight set filters
  spelling between two and six of the six sentinels (exactly one spelled all six) and seven
  single-column tests, in seven files. Two predicates, deliberately distinct — a STORED attribute vs
  a build-time variable NAME. ⚠ `is_placeholder_var()` must `as.character()`: the build passes
  symbols, and `sym == "x"` coerces while `sym %in% "x"` errors. Two questions were NOT folded in,
  with the reason next to each (`detect_totcols()` asks "is this the total column";
  `quo_miss_na_null_empty_no()` tests a deparsed user expression).
- **`REG_FAMILIES`** (`R/reg-estimand.R`): four per-family name tables and a fifth switch, in two
  files, already disagreeing. `ui = NA` IS the fact "not offered in the picker" — which
  `dev/generate_jamovi_js.R` wrote a second time as a hardcoded `setdiff(…, "quasipoisson")`.
  `REG_FIT_FAMILY` is now the `outcome` column; `REG_OUTCOME_KINDS` gained `said`.
  **The generated `jamovi/js/jmvtabreg.js` came out byte-identical** except the provenance comment,
  and `dev/generate_jamovi_js.R check` exits clean. No `.a.yaml` / `.u.yaml` touched.
- **`REG_FAMILY_MULT_WORD`** — DERIVED from `REG_ESTIMANDS`' own exponentiated coefficient row, with
  a build-time singleton assert, replacing the last hand-written `switch(fam, …)` in
  `legend_reg_eff_word()` (whose default answered `"OR"` for every family it did not list, including
  `rd` and `mr`, added one phase after it was written). ⚠ **the assert did its job twice**: it is
  keyed on the row's `fit`, not on the family bucket (a binomial outcome holds BOTH the logit row,
  word OR, and the modified-Poisson one, word RR); and the fit's word may win only where the LINK
  makes one other than an odds ratio — a logistic fit asked for a **marginal** ratio keeps its crude
  RR, which the corpus caught and which is now its own fixture.
- **`CI_METHOD_WORDED`** — `katz`'s label msgid was written TWICE (a `CI_METHOD_LABELS` row that was
  intercepted before it could ever be read, plus the switch default) and `wald_log` had no row at
  all. One table, same shape, same lookup; `potools::get_message_data()` verified every msgid still
  extracts.
- **`EST_SCALES$default_display`** — `fmt()` and `new_fmt()` were two copies of one rule and
  **disagreed** for the bind neutral (`"pct"` vs `"n"`); `new_fmt()`'s deliberate `"n"` is declared.
  **`TAB_ARG_VALUES$totcol`** — `tab-deprecate.R` had lost `""`, `tab-steps-legacy.R`
  `"all_col_vars"`. **`fmt_blank_fields()`** — one chain written 4× in two shapes and five
  wrappings. **`reg_glance()`'s `regTermTest` block** — byte-identical twice, ten lines apart, in
  one function.

**RULE 2 — the silent degradations.** The eight `if (length(v) == n) v else <neutral>` guards that
are dead BY CONSTRUCTION became `stopifnot()` (`tab-render-html.R` ×4, `tab-transpose-render.R` ×3,
and `tab_md.R`, which had only the `is.null` half its two html siblings carry and degraded to
silently-uncoloured cells). The `is.null` half stays everywhere: an ABSENT annotation is a real
state, a SHORT one is a producer bug — and the silent substitution is what hid D1's grey footer for
two phases. ⚠ **The two GENUINE ones were deliberately NOT promoted** (`tab-test-display.R`,
`plots.R`): each is a length-equality standing in for a **missing join key**, and a `stopifnot()`
there would abort on a legitimately degraded table. Each carries a comment naming the missing key.

**`is_reg` — two questions, two messages.** `reg_plot_fits()` *stated* the conflated claim out loud
("`x` is not a `tab_reg()` table") on a table that IS one but has lost its recipe; it is two aborts
now. `reg_eff_word_of()` gates on the stored kind and passes a possibly-NULL call through, so a
meta-stripped table keeps its plot-axis word. `reg_model_lines()` **keeps** its guard — it genuinely
asks "is there a recipe to describe" — and says so; reported as a rename, not a fix.

**G5 — `ci = "cell"` keeps the reference row's interval** (maintainer ruling; the only user-visible
change). The rule is stated once, as `CI_GEOMS$ref_cell`: *a CELL interval compares each cell to
0 %, not to a reference, so every cell keeps it; a CONTRAST interval blanks the row it would compare
to itself.* It was written in all three consumers and two were wrong, so `tab(…, ci = "cell")`'s
Total row showed no bracket while `tab_num(…, ci = "cell", tot = "row")`'s showed one — and the
rule the vignette teaches is the numeric one. `dev/verify_golden_field_delta.R` gained the
**"populated field on a declared row subset"** mode for it (these cells were NA and are now finite,
every other cell bit-identical, both directions checked) — and it was verified to FIRE, not to pass
silently, by disabling the declaration and watching it report the change.

**HONEST CONCERNS.**

- **`tab_compact()` accepts more than it did**, by two independent routes: the sentinel fix (a
  no-col_var table now nests) and the union bind (a genuinely narrower table keeps the wider one's
  columns instead of truncating it). Both are `TAB_OPS`' own declared rule finally applied to the
  truth — but the merged table's *layout* on those shapes is asserted by fixtures, not eyeballed.
  Worth one look at 19n.
- **The `ci = "cell"` change touches ~126 call sites across 25 test files.** None asserted the
  blank; only `f_ci_cell` moved. But it is a real change on a CRAN-released argument, and it is in
  `NEWS.md` and both vignettes rather than only in the code.
- **`REG_FAMILY_MULT_WORD`'s "the fit wins unless its word is `OR`"** is the honest statement of
  what the old switch did, and it is a statement about LINKS — but it reads as a magic test until
  you have the comment beside it. The genuinely principled fact would be the crude block's own word,
  which `REG_EMPIRICAL` does not carry (it lives in the column NAME, `Obs_RR` / `Obs_IRR`, which is
  the guess this phase exists to stop making). Filed as a smell, not a defect.
- **Measured and explained, needing no fix**: on a **`meta`-stripped** reg table the colour legend
  loses the effect word and names the wrong interval ("Katz interval on the log **risk**-ratio" for
  a Poisson crude column). Root cause: `tab_materialize_extras()` CONSUMES the `test` attribute and
  `tab_kind()`'s degraded fallback sniffs exactly that, so the materialised table reports
  `kind = "crosstab"`. That is the documented degraded contract (`test-degraded-attrs.R`: *"a
  regression losing `meta` drops its title/effect wording"*); a full table is unaffected.
- **`tab_reg()`'s argument boundary is untouched** and remains the single biggest structural item.
  So are the display-grammar table (designed in full, filed) and the options cluster (censused, one
  of three items taken).
- No `.a.yaml` / `.u.yaml` was touched, so **no `jmvtools::prepare()` is needed** — 19k's maintainer
  rebuild + live pass is still the outstanding one.

**FOLLOW-UPS.** 19m-ii can start on this commit; the roadmap's 19m-ii entry now carries the full
`DISPLAY_TOKENS` design, the two options folds, `reg_resolve_args()`/`new_reg_spec()`, the three
carrier migrations (with `emp_tips` measured **not reachable**), the two join-key guards, and the
four still-owed measurements.

---

#### Phase 19m-ii — Harvest 2: `tab_reg()`'s argument boundary

**DONE (2026-08-15).** THE structural item 19l pass 2 and 19m-i both handed forward, and the last one
that moves the study's headline diagnostic. **`tab_reg()`: 821 lines → 147, and 30 of the package's
~190 user messages → 1.** Phase 19i gave the four crosstab producers one argument boundary; the
regression producer never got one, so 738 of its 821 lines resolved 28 arguments before a single
`reg_build()` call, and inside them sat **twelve ad-hoc local closures** and **two near-identical
14-field spec literals** — all there for one reason: the per-dependent facts were never materialised.

**Scope (maintainer's rulings at plan time)**: this session is the reg boundary **only**. The
`DISPLAY_TOKENS` grammar, the carrier migrations and the owed measurements go to **19m-iii**. The
options cluster is **dropped** — no tooltips tri-state, `output_kable` left alone.

**Verified.** Full suite **FAIL 0, WARN 1, SKIP 4, PASS 6461** against the inherited FAIL 0 / WARN 1
/ SKIP 4 / PASS 6402 — same warning count (the pre-existing Poisson over-dispersion advisory), and
the +59 is exactly `test-reg-resolve.R`, this phase's own fixture file.
`dev/verify_golden_field_delta.R` reports **only the declared addition**
across all **1788 cells of the 36 goldens** — the `test` tibble's new `dep` column, all-NA on every
crosstab case, and no per-cell field, no column attribute, no other `test` column and no `meta`
sub-field moving. No `_snaps/*.md` moved.

**THE ENABLING FACT, and why step 1 was a dev script with zero source change.** There is **no
regression golden and no regression snapshot**: `_golden/` is 36 crosstab cases and `grep -c Model`
on `_snaps/golden.md` / `_snaps/render-html.md` is 0. The reg producer's whole argument surface was
asserted only by `expect_*`. So the phase opens with **`dev/verify_reg_specs.R`** (committed, on
`dev/verify_color_attrs.R`'s model): 291 cases over 20 named axes, dumping per case the **messages in
order** (the field `verify_color_attrs.R` lacks and this phase most needed — 30 messages live in the
region and several deliberately move), the specs, the whole `reg_call()`, every fmt column's stored
attributes, every non-fmt column's labels (the only cheap window on the four `data` rewrites) and the
`test` keys. It captures through `tab_reg()` alone, because `reg_call(x)$fit_spec$specs` already
stores the resolver's central output — so it works unchanged on both trees with no new API. ⚠ It
`scrub()`s language and closures (`identical()` on either compares ENVIRONMENTS, and a fresh
`load_all()` makes new ones) and normalises cli's embedded source references at COMPARISON time
(adding a line anywhere rewrites `"Caused by error in f() at tab_reg.R:1247:9"`). It was proved
deterministic — `check` against its own baseline on the unchanged tree printed IDENTICAL — before
being trusted as a gate.

**The eight steps, each gated on it.** Steps **5, 6b and 6c were required to be exactly IDENTICAL**
and were (0 differing paths); the rest declared their delta and matched it exactly, verified by a
path-level differ rather than by eyeballing case names.

**THE SHAPE**: one entry point, **`reg_resolve_args()`**, six declared stages in a new
**`R/reg-resolve.R`**, returning **`new_reg_args()`** — `new_reg_shared()`'s idiom (the FORMALS are
the contract, the body is `as.list(environment())`, the derived `globalVariables()` mirror beneath).
Details in the Repository Map.

**⚠ `data` is INSIDE the boundary, as a declared field.** A pure resolver is impossible here without
a cycle: `family = "auto"`, `trials = TRUE` and `multiplier = "sd"` are all ANSWERED by the data,
`shape` recodes it, `reference` relevels it — and the relevel needs the families S3 resolves. A
separate `reg_prepare_data()` that `tab_reg()` called itself would put the ORDERING in the caller: a
second place it can be got wrong, i.e. the ad-hoc layer rule 1 forbids. `new_ctx()`'s `data = NULL`
is the exact precedent.

**⚠ There is deliberately NO `REG_ARG_VALUES` table** (maintainer-confirmed after measurement).
`TAB_ARG_VALUES` exists because FIVE producers had each re-implemented the boundary and drifted
(`tot`'s expansion four times, `na`'s allow-list three times *with three contents*). `tab_reg()` is
ONE producer whose vocabularies are already declared once each — and `TAB_ARG_VALUES`' own exclusion
rule (*"validating it means RESOLVING it, so it lives with its resolver"*) disqualifies **eleven of
the fifteen** candidates. A table would have had ~4 rows, one duplicating a list that already existed
twice. `reg_validate_args()` instead does five checks, each **calling an existing single source**.
The one genuine table-move: **`COLOR_SIGNIF_VALUES`** extracted (it was written twice, in `tab.R` and
`fmt_class.R`; three readers now).

**THE PER-DEPENDENT TABLE is the key.** Nine of the twelve closures existed because family /
estimand / trials / inverse / crude key were re-derived on demand from a frame later blocks kept
mutating — `est_for` even carried its own `local()` memo cache, and `trials_for` was **defined
twice**, an off default and an on-path redefinition nested two `if`s deep. `reg_resolve_estimands()`
computes the rows once; the survivors are four one-line LOOKUPS, and the cache is unnecessary by
construction. The other three became **pure package functions** — `reg_eff_word(est, empirical)`,
`reg_trials_observed_max(x)`, `reg_color_auto_measure(est)` / `reg_color_for(color, est)`. That last
pair also deleted `color_auto` / `color_slot_auto` / `color_spec_arg`: the body filled `color` in
place one line after computing the sentinel from it, so three extra locals existed to remember what
`is.na()` had meant.

**THE TWO SPEC LITERALS → ONE `new_reg_spec()` CALL SITE**, with the collapse *proved*, not assumed:
`formula_mode` is set only inside the `is_formula(dependent)` branch, which aborts if `predictors` is
non-NULL and then assigns it a CHARACTER vector — so `is_comparison` cannot be TRUE alongside it, and
the branch's hardcoded `compound = FALSE, formula = NULL` were the general expressions. A
`stopifnot()` records it. Three fields left the record (`effect_shape` had **zero** readers;
`do_exp` is one token; `eff_word` is now derived inside `reg_build()`, where `empirical` is FINAL —
strictly better than storing it).

**NINE DEFECTS, each shipping with the fixture that fails without it** (`test-reg-resolve.R`, 59
assertions). Four were on the plan; five were found while implementing:

- **`reg_per_dep()` is THE declared slicer, and the cascade was open-coded three more times with
  DIFFERING semantics.** `family[[d]]` **errors** ("subscript out of bounds") when a named vector
  omits a dependent, `family[[i]]` when a positional one is short, and
  `inverse_two_level_factors[[d]]` does both — a *positional* `inverse_two_level_factors` was
  unusable entirely, since the length>1 branch assumed names. Measured: `tab_reg(d, c("a","b"),
  family = c(a = "binomial"))` died; it now detects `b`.
- **`stats` was never validated.** `reg_validate_stat_keys(x, arg = "stats")` has carried that
  default since 19g and had ONE caller, passing `arg = "check"`. `stats` was silently FILTERED, so a
  typo produced a missing footer row with no message.
- **`color_signif` was unvalidated on the reg path.** It went straight to `fmt()`, which casts
  without validating, so `color_signif = "grey"` was **stored on every column**.
- **`conf_level` was never validated here** — `conf_level = 95` produced `NaN` bounds and a table.
- **`baseline`** was validated conditionally, late, and as a warning, so a bogus one under
  `compare = "none"` was dropped in silence.
- **A formula `dependent` entered the multi-dependent recursion.** `length(y ~ x)` is **3**, so every
  two-sided formula passed `length(dependent) > 1L`; each child died on an internal `stopifnot` while
  the teachable message written for exactly that mistake sat unreachable.
- **`reg_color_notes()`'s `crude_keys` formal was DEAD** — the name appeared only in the signature,
  and the caller ran a per-dependent `vapply` purely to fill it: dead work *and* a fourth encoding of
  the crude-key cascade.
- **The `color_signif` default landed 22 lines after the note that reads it** (H21), so
  `tab_reg(color = "adjustment")` was silent while the identical explicit state emitted the note.
- **A table's own record could contradict its own column header** (H22). `empirical` is written by
  two blocks (the `adjustment` forcing turns it ON, the no-crude-companion degrade turns it OFF) and
  read by three later ones, and the notes ran BETWEEN them. Measured on the pre-phase tree:
  `reg_call$eff_word` said `"AME"` while the column it describes was `"Model_AME (adjusted %)"`.

**THE ORDER IS THE DESIGN, and it is now written down.** Twenty-three constraints (`H1`..`H23`)
stated where they bind rather than implied by 738 lines of sequence. Three were violated (H20/H21/H22
above); one more was silent waste — **the frozen frame was built TWICE, verbatim, ten lines apart,
under a comment demanding the multiplier's SD and the quadratic terms' centre come from the SAME
measurement** (H19). And **H23**: the five `split_var` refusals ran ~500 lines late, so *"`split_var`
is not a column of `data`"* arrived after up to eight informs about families and colours the call was
never going to produce.

**⚠ The `reref` clause is the one place a wrong `TRUE` is a wrong NUMBER, not an error** (a table
built from a stale digest). It reads **13 resolved values spanning eight blocks**, which is the
strongest argument that the stage order is the design; its reasoning is now spelled out per clause,
and the harness has a `reref.*` axis toggling each one — an axis nothing covered before.

**THE `test` TIBBLE'S `dep` KEY** (19m-i's "missing join key", filed here). `reg_test_row()` gains
`dep`; **`new_test_tibble()` declares it** — it MUST be in the schema, since `test_group_cols()` is
`setdiff(names(tt), names(new_test_tibble()))` and an undeclared column would be read as a GROUPING
variable and split the reg footer into one block per outcome (19g's own defect). Crosstab rows carry
`NA`, written explicitly in `tab-chi2.R`'s three `transmute()`s — NA, not `""`, because `var = ""`
already means "the whole table". `test_grid_reg()` now states a RULE: *a dependent names a column
only when it IDENTIFIES it — one model per outcome; a model COMPARISON gives every column the same
outcome, so the column key is the header.* Strictly better in the one case the length coincidence got
wrong (a single-model comparison used to be headed by the outcome).

**`sp$family` → `fit_family`** (32 sites, maintainer-approved, landed last and alone). It IS
`est$fit` — the internal LINK key, `rr`/`rd`/`mr` included — sitting one word from `reg_call$families`
and `sp$est$family`, which both mean the OUTCOME family. A name that invited a guess about which of
the two it was, in a phase whose rule 2 is "never guess".

**HONEST CONCERNS.**

- **`R/tab_reg.R` shrank 6087 → 5470 while `R/reg-resolve.R` adds 981: net +364 lines.** That is
  the same trade every Phase 19 key made — scattered implicit rules for declared stages plus the
  prose that explains them — and the line count is the wrong scoreboard. What moved is the
  diagnostic: **`tab_reg()`'s body 821 → 147 lines and 30 messages → 1**, and 33 of `tab_reg.R`'s 62
  messages are now at a boundary that says so in its name.
- **The one message left in `tab_reg()`** is the `trials`-length abort inside the multi-dependent
  recursion, which stays because that block is a dispatch over the call SHAPE, not resolution —
  moving it would make `reg_resolve_args()`'s return type a union.
- **The estimand-refusal errors lost purrr's `In index: 1. With name: … Caused by error in …`
  wrapper** (36 harness cases), because the loop moved from `purrr::map` to `lapply`. The message
  bodies are character-identical and already name the dependent, so the wrapper was pure noise — but
  step 5 was declared IDENTICAL in the plan and this is the one respect in which it was not.
- **H20's own path produced no change in the sweep.** The forcing and the degrade never both fire on
  the 291 fixtures (the degrade needs a compound formula, where the estimand is a coefficient and the
  parenthetical never applies). H22 IS reachable and is measured and fixtured; H20's reorder is a
  correctness fix whose failure mode I could not construct. Said plainly rather than claimed.
- **The `empirical` degrade now asks the SPEC's own stored `crude_key`** instead of re-deriving one
  from the OUTCOME family — a third encoding, and one that read a different family from the one the
  spec pairs its crude block with. Verified equivalent on the only question the degrade asks (every
  fit key and every outcome family yields a non-NA key; only a compound formula gives NA), so the
  sweep shows no change. It is a *unification*, not a behaviour claim.
- **One harness run took 546 s instead of 93 s.** No orphans (`ps` checked, 0 workers); the next run
  was 92.5 s. Transient machine contention, not a regression — recorded because the number is in the
  logs.
- **The `.a.yaml` / `.u.yaml` were not touched**, so **no `jmvtools::prepare()` is needed** — 19k's
  maintainer rebuild + live pass is still the outstanding one.
- `dev/verify_color_attrs.R` was not re-run: nothing here touches the crosstab colour vocabulary, and
  the golden delta proof covers the stored colour attributes cell by cell.

**FOLLOW-UPS.** **19m-iii** carries what this session did not take: the `DISPLAY_TOKENS` grammar
(designed in full in the roadmap), the `spread_relabel()` `<br>` carrier, the `"Total"` sentinel
defaults in `survey-variance.R`, the two genuine length guards (one of which — `tab-test-display.R`'s
— **this phase closed**, so only `plots.R`'s remains, and it cannot be fixed by tabxplor alone), the
four owed measurements and the JS syntax gate. 19n: `po/R-fr.po` (the four new aborts are
untranslated), the vignettes, and `?tab_reg`'s argument prose.

---

#### Phase 19m-iii — Harvest 2: the display grammar

**DONE (2026-08-15).** Full suite **FAIL 0, WARN 1, SKIP 4, PASS 6528**, against the inherited
FAIL 0 / WARN 1 / SKIP 4 / PASS 6461 — the +67 is exactly this phase's own fixture file, and the one
warning is the same pre-existing Poisson over-dispersion advisory. Both proofs are clean with **EMPTY
declaration sets**, which is this phase's whole contract: `dev/verify_color_attrs.R` prints
**IDENTICAL** over its 293 cases against a baseline captured from the pre-phase tree, and
`dev/verify_golden_field_delta.R` reports **no delta** on any of the **1788 cells of the 36 goldens**
— no field, no column attribute, no `test` column, no `meta` sub-field. No `_snaps/*.md` and no
`_golden/` fixture moved; the only `man/` churn is the two generated `display` sections.

**THE LAST SCATTERED VOCABULARY.** The display grammar stated ONE per-token relation as **eight
separate vocabularies in four files**, none aware of the others: `get_num()`'s read map (22 arms),
`set_num()`'s write map (**17**), `tabxplor_display_fields` (12), `tabxplor_display_aliases`,
`DISPLAY_BARE_TOKENS` (8), `DISPLAY_FIELD_SOURCE` (9), `DISPLAY_TOKEN_GEOMETRY` (7),
`DISPLAY_COMPARISON` (3, in a third file), plus an inline value-cell gate and a footer gate written
**twice with two near-miss variants**. **`DISPLAY_TOKENS`** (`R/tab-display.R`) is that relation: 23
rows × 12 columns, details in the Repository Map. Every old name SURVIVES, derived from a column,
keeping its contents *and its order* — so not one consumer moved, which is what made an empty
declared delta possible.

⚠ **And the split was already costing correctness.** `get_num()` had 22 arms where `set_num()` had
17, and `vec_arith` writes through `set_num()` — so **arithmetic on a column displaying `pct_ci`,
`mean_ci` or `pvalue` silently returned it unchanged** (`x * 2` == `x`, no warning), on a `pct_ci`
that `?fmt` *documents* and with the README teaching `mutate()` over fmt columns. Measured on HEAD
before touching anything. Declaring `settable` is what made two switches 50 lines apart comparable;
the three arms are added, and `resid` (derived from p-value + `sign(ctr)`) and `blank` are now the
only `settable = FALSE` rows — a stated fact rather than an omission indistinguishable from one.

**The guard is what keeps them honest.** A build-time `stopifnot()` at the **tail of
`R/tab-display.R`** — the first file where `DISPLAY_TOKENS` and both switches are in scope, since
`fmt_class.R` sorts first — walks `body(get_num)` / `body(set_num)` for their string constants and
ties all three together **both ways**: an undeclared arm, an unhandled row and a `settable` token
with no write arm each fail the install. It was verified to FIRE, not merely to pass. ⚠ Scoped to
those two only: they are pure per-token maps, so every character constant in them IS a token, which
is what makes the check two-directional; `format()` is excluded (its body is full of rendering-class
and unicode constants) with the reason written down. The hot path stays hand-written throughout, the
`fmt_attr_rules` precedent — `display_primary()`'s in-suite micro-benchmark is unmoved (0.93 s for
20× on 1e6 cells).

**Why `footer` and `colour` are two columns and not the roadmap's one `numberless`.** The gate was
written four times with *three* different contents. That is not sloppy copying: `pvalue` never
carries a star but **is** coloured, deliberately, as a significance warning. Two facts, declared
separately; the family reads as a rule instead of three exceptions.

**THE DOCUMENTATION IS GENERATED**, on the `reg_measures_rd()` model (`#' @eval`, the package's only
other one): `?tab` gains *Display fields* and `?fmt` *Every display token*. `?fmt` hand-listed
**eleven of the twenty-two** and had drifted; `?tab` hand-copied `tabxplor_display_fields` verbatim
from a file 1400 lines away. A `doc` column carries each token's phrase, so the prose lives with the
fact.

**THREE RULE-2 REPAIRS.**

- **`R/plots.R`'s dispersion panel** joined `se` to a SECOND, independent read
  (`names(coef(fit))`) by length coincidence. ⚠ The fix 19m-ii filed (read both from
  `summary(fit)$coefficients`) would have been **wrong twice**: it drops aliased rows, so `se` would
  stop indexing the influence closure, and on a quasipoisson its SEs are not `vcov()`'s — the very
  reason `reg_check_model_se()` reads `vcov()`. The real fix is smaller: `sqrt(diag(vcov(fit)))`
  **already carries vcov's dimnames**, so `names(se)` is the join key, same provenance, same length
  by construction. Strictly better on `multinom`, where `coef()` is a matrix (names `NULL`) and the
  old code fell back to `"1","2",…` while `vcov()` is properly named.
- **The `"Total"` sentinel.** The roadmap's framing was wrong here too, and the correction is the
  honest part: `"Total"` is the **leaf's internal pre-rename key**, not a user label — the fourth of
  the internal names in `tab-leaf.R`'s round-trip DESIGN note, beside `"col_var"` / `"_colvarbis"` /
  the `"n_"`-`"wn_"` prefixes — and `total_names` is applied only much later, at
  `leaf_rename_totals()`, so substituting `total_names[1]` in the variance producers would have been
  a **bug**. The package's own precedent for this class is a literal plus one comment naming them
  all, so that is what it got; what genuinely went are the `tot = "Total"` / `tot_lab = "Total"`
  **parameters no caller ever set** — a false promise of configurability, which is what the roadmap
  actually complained about.
- **`emp_tips`' rekey** yielded `NA` names silently for a key the wrap rename cannot follow. 19m-i
  measured the miss unreachable; it now keeps the old name rather than blanking a tooltip.

**THE THREE OWED MEASUREMENTS ARE TAKEN** — `dev/benchmarks/phase19m3_measurements.R`, results at
`dev/benchmarks/results_2.0.0/phase19m3.txt`. (i) **19j's per-`col_var` `agg_chi2()` costs ~10 ms per
extra col_var, and it is pure per-call FIXED overhead** — independent of cell count (16 col_vars: 140
ms at 480 cells, 134 ms at 2400), ~9 % of an 8-col_var build. That is the **price of the
one-aggregate-core design, quantified**: the leaf runs one `plain_core()` per col_var by
construction, so re-batching would need the cross-leaf step 19j deleted. (ii) **19k's fit cache**: a
reference change is 45 ms on the digest path, **396 ms under `color = "adjustment"` (×8.8)** and 108
ms under `shape` (×2.4) — a real new live-UI cost, since neither was reachable before 19k. (iii)
**19d's unconditional odds ratio does NOT worsen with width**: `tab_apply_reference()`'s profile
share over 1/2/4/8 col_vars is 12.5 / 20.0 / 23.1 / 17.5 % — no trend, inside sampling noise — and
`ci_or` never rises above the floor. 19d's "re-measure wide before release" is answered: nothing to
do.

**HONEST CONCERNS.**

- **Three of the plan's items were dropped by maintainer ruling and the roadmap is amended to say
  so**, rather than left proposing them: `tabxplor.output_kable` is **not** to be folded (it keeps
  its build-time render), the other options folds are dropped, and the `spread_relabel()` `<br>`
  carrier migration is deferred. The `<br>` design notes are kept, collapsed, in the roadmap.
- **`set_num()` is still a silent no-op on `resid` and `blank`.** Correct — neither has a field to
  write — but it is the same *shape* as the defect just fixed. A warning was rejected: `blank` cells
  are routine (`n_min` masking), so it would fire on ordinary tables. Declared, and filed.
- **The Rprof shares in measurement (iii) are noisy** — the platform clamps the interval to 10 ms on
  an 80-420 ms build, and `K = 1` moved 25 % → 12.5 % between two runs. The *trend* is the claim, not
  the digits; the file says so.
- **The JS syntax gate was not attempted and cannot be here**: no `V8`, no `node`. ⚠ While filing it
  I corrected the record — CLAUDE.md and the roadmap both claimed a committed JS bracket check and
  **there is none**; `tests/` opens no `.js` file, and `test-jamovi-vocabulary.R:100` checks content
  drift only (and is itself double-skipped). Decision filed to 19n.
- `jamovi/js/jmvtab.js` regenerated: the **provenance comment only** (`DISPLAY_COMPARISON` moved
  file); the emitted values are byte-identical and `dev/generate_jamovi_js.R check` is clean. No
  `.a.yaml` / `.u.yaml` touched, so **no `jmvtools::prepare()` is needed** — 19k's maintainer rebuild
  + live pass is still the outstanding one.
- `po/R-fr.po` untouched; nothing here adds a translatable string (the `doc` column is Rd-only, and
  Rd is English by design in this package). 19n still owns the i18n pass.

**FOLLOW-UPS.** 19n: the `<br>` migration if it is taken at all, the JS-gate decision, `po/R-fr.po`,
the vignettes, and the one remaining `?fmt` double-gloss (`ctr` / `obs` are now described both in
their own `@param` and in the generated section).

#### Phase 19n — Documentation, i18n, and release readiness

**DONE (2026-08-15).** The last phase before the 2.0.0 release: *the taught surface matches the
shipped one, in both languages, and the package passes its release gates.* Full suite
**FAIL 0, WARN 1, SKIP 4, PASS 6560** against the inherited FAIL 0 / WARN 1 / SKIP 4 / PASS 6528 --
same warning count (the pre-existing Poisson over-dispersion advisory), and the +32 is exactly this
phase's own fixture file. `dev/verify_golden_field_delta.R` reports **only the declared addition**
across all **1788 cells of the 36 goldens**, and the only `_snaps/` churn is `fmt-contract.md`'s
attribute list, one line.

**The gates, all green, all run on the final tree**: full suite (normal locale) · the CI-locale run
(`LC_ALL=C.UTF-8 LANGUAGE=en`) **FAIL 0 / SKIP 17** -- the French blocks *skipping* as designed, not
failing, which is the CRAN-farm property that run exists to prove · `verify_golden_field_delta.R`
(only the declared addition, 1788 cells) · `verify_color_attrs.R` (**IDENTICAL**, 293 cases) ·
`document()` **idempotent** · **`devtools::check()` Status OK, 0 errors / 0 warnings / 0 notes** ·
`pkgdown::build_site()` (the only gate on the three FR articles).

⚠ **Two release gates were RED on arrival, which means neither `devtools::check()` nor
`pkgdown::check_pkgdown()` had run since 19b.** `?fmt`'s `@examples` called `fmt(n = …, type = "row")`
and `set_type(f, "col")` -- an argument and a function both **deleted in 19b** -- so the first is an
abort and the second does not exist: an **R CMD check example failure**, i.e. a hard blocker, sitting
in the package's flagship type documentation. And `check_pkgdown()` errored on three exported topics
missing from the index (`reg_measures`, `tab_shape`, `tab_supports`) while still publishing the
defunct `kable_tabxplor_style`. Both are closed; `tools::checkDocFiles()` and `check_pkgdown()` are
silent.

**THE `<br>` CARRIER MIGRATION** *(maintainer's ruling: take it, in full)*. The last welded fact in
the package. `tab(spread_vars =)` / `tab_spread()` and `tab_reg(split_var =)` both folded the
sub-population into the column's `col_var` as `"{level}<br>{col_var}"`; three backends recovered it
by **sniffing for that html tag** (Excel's two-line span and its wrap flag, the legend's name
normaliser) and a fourth un-escaped it after `htmlEscape()` -- while `tab_wrap_text(brk = "<br>")`
emits the same tag for an unrelated reason, which none of them could tell apart. It is the 16th
column attribute **`col_group`** now (`get_col_group()` exported, the setter internal: writing is the
pipeline's job), composed only where two lines are actually wanted -- html a `<br>`, Excel a newline
+ wrap, markdown the one-line form it can draw. `<br>` in a header means exactly one thing.

⚠ **The roadmap's brief ("2 lines + move the span composition") understated it: the weld had THREE
carriers, and the third is the one that bites.** The `test` tibble keys its rows on `col`, and
`test_grid_crosstab()` matches that against `unique(get_col_var(...))` -- so with the level removed
from `col_var`, a spread table's two blocks collapse to one key and the grid emits **one p-value
column for a table that has two**. `test` carries a **declared** `col_group` column too (declared,
because `test_group_cols()` reads every undeclared column as a *grouping* variable -- 19g's own
defect), and both the grid and the span header key on the pair through the one
`fmt_col_block()` / `tab_col_blocks()` rule. `tab_header_runs()` RLEs the **pair** for the same
reason: on the label alone, two adjacent blocks of one variable merge into a single span.
New fixture `test-col-group.R` (11 tests) is the migration's proof -- the stored pair, the two-line
span in html and md, the legend prefix, the header runs, and the p-value column count; the two
assertions that tested the weld (`test-test-display.R`, `test-tab_reg-survey.R`) are migrated with
their reason. jamovi cache schema **17 -> 18**.

**ONE COLOURS PAGE** *(maintainer's ruling)*. `?set_color_breaks` opened a page titled *"Many
cross-tables as one, with color helpers"* whose first line was a **superseded badge for
`tab_many()`** -- five live, everyday functions documented on a shim's page, which `_pkgdown.yml`
pointed at twice. They are `?set_color_palette` now, retitled *"Colours: palettes, styles and
breaks"* with a real description; `is_tab` and `tab_get_vars` (a different concern) got their own
pages, and `tab_many.Rd` keeps only itself.

⚠ **That page was silently shadowing two `@param`s, and the fix needed two passes.** `theme` was
documented twice and `type` **three** times; roxygen keeps one, so `set_color_palette(theme =)` and
`set_color_style(type =)` were documented with *another function's* definition. One
per-function-disambiguated tag each -- and the first attempt still lost, because the third `type`
lived on `get_color_breaks`'s own block, 400 lines away. `checkDocFiles()` cannot see this class
(the param IS documented, just wrongly), so the only thing that caught it was reading the generated
`\arguments{}` by eye, which is why the plan required it.

**THE TAUGHT SURFACE.** The colour values are the full words everywhere *(maintainer's ruling:
migrate everything)* -- 16 roxygen sites, 52 vignette/README sites -- so what a user types matches
what the table stores and what its legend prints, with the acronyms noted as permanent shorthands.
**18 roxygen cross-references stopped naming `tab_many()` as a way to build a table** (`grep -l
tab_many man/*.Rd`: 20 files -> 2, the second being the deliberate `na_drop_all` history), and the
two claims that were outright FALSE since 19h went: *"`tab()` is a friendly wrapper around the more
powerful `tab_many()`"* at the top of `?tab`, and its `@seealso` twin. Other repairs: `ci = "diff"`
-> `"ref"` where the page speaks `tab()`'s vocabulary (⚠ **not** in `tab_ci()`, whose step
vocabulary owns that word natively -- instead the `@section Significance stars` those two pages
SHARE through `@inheritSection` stopped naming a value at all, since it means different things on
each); `tab_ci(ci_scale =)` stopped documenting storage as `ci_type = "ratio"`, an attribute deleted
in 19b; `effect = "ame"` -> `"marginal"` in `?forest_plot` (a value that now **aborts**);
`OR = "cumOR"` -> `ref2 = "cumulative"`; `tab_plain()` got the `@description` that was commented
out; the four soft-deprecated composite-colour examples were rewritten so `check()` runs clean; and
`?tab` now documents `pct`'s per-`col_var` and `ref`'s per-`row_var` vector forms, neither of which
appeared anywhere. ⚠ Found in passing and fixed: `?tab`'s `display` prose asserted *"`tab_reg` has
no `display` argument of its own"* -- **19e gave it one**.

**`?fmt`'s field roll-call is GENERATED** (`FMT_FIELD_DOC` + `fmt_fields_rd()`, a fourth `@eval` on
the `display_tokens_rd()` / `reg_measures_rd()` model, exhaustive by build-time `stopifnot`): the
hand-written list still named `in_totrow`, **deleted in 19f**, and omitted its replacement
`row_kind`. The same list in both programming vignettes said *"19 fields"* for 21 and contradicted
the `vec_data()` output printed two lines below it.

**i18n.** `po/R-fr.po` was 22 entries behind: **235 translated, 0 fuzzy, 0 untranslated** now.
⚠ `po_update()` carried six near-matches over as FUZZY and several were **wrong** -- "Wilson score
interval" had inherited *"intervalle de Newcombe"* -- so every one was rewritten rather than
accepted. ⚠ **`inst/po/en@quot` had rotted to 136 of 235 msgids** and nothing in the repo
regenerated it: it has no translator catalogue, and potools only compiles `po/*.po`. It is
**DERIVED** now, step 5 of `dev/update_translations.R` (`tools:::en_quote()` on the `.pot`), with its
`.po` deliberately not kept in `po/` -- `po_update()` would otherwise merge it as a translation.
That script's NOTE also named an extraction anchor `19l` deleted; it names the one that survives
(`reg_check_msgid_anchor()`) and says why it cannot go.

**Also**: the FR regression article was the only one of the seven documents missing
`Sys.setenv(LANGUAGE = "fr")` beside `options(tabxplor.lang = "fr")`, so its GOF / model-fit /
test-summary rows knit in the *builder's* language; both `ame_ratio` capability rows taught a
spelling that **aborts**; and the same row said `measure = "ratio"` in EN and `family = "poisson"`
in FR -- one table, two claims, which is what editing file-by-file does.

**HONEST CONCERNS.**

- **`man/figures/README-hero.jpg` is handed over** (maintainer's ruling: flag it). It is a console
  screenshot dated Jul 27, before the 2.0.0 OKLCH palettes; I cannot re-shoot one. The re-knit
  refreshed everything *around* it -- including real 2.0.0 features the Aug 10 render predates (the
  `n` column, the variable-name column, the sparkline, the five model-check footer rows) -- which
  sharpens the mismatch rather than hiding it. Reproduce it with the first `tab()` call in
  `README.Rmd` under `set_color_palette(theme = "light")`.
- ⚠ **`devtools::build_readme()` is NOT the right tool here** and its output must not be committed:
  it renders `github_document`, which strips the YAML header and hard-wraps every paragraph
  (+1329 lines of pure churn). The committed README is `knitr::knit("README.Rmd", "README.md")`,
  which needs the package *loaded* first. Recorded because I made that mistake once.
- **The JS gate is DECLINED** (maintainer's ruling), and the record corrected: there is no `node`
  and no `V8` on this box, so nothing added here could be *run*. ⚠ CLAUDE.md's 19k summary still
  claimed *"The suite balance-checks brackets and the generator diff"* -- there is **no** such
  check; `tests/` opens no `.js` file, and `test-jamovi-vocabulary.R` compares only the generated
  marker blocks (itself double-skipped). 19l corrected this in two places and missed the third.
- **`jamovi/jmvtab.a.yaml`'s prose is fixed but the shipped `man/jmvtab.Rd` stays stale** until the
  maintainer runs `jmvtools::prepare()` -- which **19k already owes before release**. ⚠ Note
  `R/jmvtab.h.R` is NOT `.Rbuildignore`d, so its roxygen ships to CRAN; the yaml is the source and
  must never be worked around by hand-editing the `.h.R`.
- **The FR articles are covered only by the pkgdown build**, never by `check()`
  (`^vignettes/articles$` is `.Rbuildignore`d). The build ran here and they render French
  ("Linéarité", "rapports de cotes"), which is also the end-to-end proof the recompiled catalogue
  landed.
- ⚠ **`check()` found THREE more failures of its own, all pre-existing and all invisible until it
  ran.** (i) **`test-jamovi-vocabulary.R` ERRORED inside the tarball**: it reads `jamovi/*.a.yaml`,
  and `jamovi/` is `.Rbuildignore`d -- so those files do not exist in a built package. The
  generated-block test in the SAME file already had the right guard for `dev/`; `yaml_opts()` now
  has it too. (ii) `yaml` was used via `::` in tests without being declared -- it is a Suggest now.
  (iii) `w2`, the per-cell sum of squared weights the flat-design variance reads, was a data.table
  NSE symbol never declared beside its siblings `n` / `wn`. Plus a stray `Rplots.pdf` (a README-knit
  artefact, git-ignored but not build-ignored) which was the third NOTE.
- ⚠ **The README's own language-pin comment cited a stale example** -- *"Without it, `LR vs null`
  knits as `RV vs nul`"*. The catalogue deliberately keeps `LR` as **notation** (like OR/IRR/β), so
  that string is translated to itself; the comment's *reason* is right and the built FR article
  proves it ("Linéarité"), only its example was wrong. Fixed in both mirrored copies.
- **`_pkgdown.fr.yml` is still in `.Rbuildignore` and does not exist**, a leftover of the bilingual
  site the maintainer collapsed to one. Harmless (an ignore entry for a missing file), left alone
  because deleting it is the kind of change that looks like a mistake in a release diff.
- The one WARN in the suite is the pre-existing Poisson over-dispersion advisory, unrelated.

**FOLLOW-UPS.** Maintainer, before the release: `jmvtools::prepare()` + `jmvtools::install(home =
"flatpak")` + the live jamovi pass (19k's standing debt, and the only thing that un-stales
`man/jmvtab.Rd`); the README hero screenshot; `cran-comments.md` / `CRAN-SUBMISSION`; then
`dev/release_checklist.md`'s branch mechanics. Phase 19o (the Phase 19 assessment) can start on this
commit.

#### Phase 19o — assesment of what have been done in Phase 19

Please, review what have been done in Phase 19 : have the code really been simplified ? Is there more simplifications possible ? Write your findings in a new file in `dev/`.

---




### Phase 20 — reviews and further simplification before release

#### Phase 20a — Review of vctrs fields, column attributes and table attributes
- Giving the new framework, is there room for further simplifications and integration ? 
- Now that conf_level has become a column-level attribute, is there additional ways to simplify and integrate the code around it ?

#### Phase 20b — Review of exported functions
- Among the new exported functions, which one are not really necessary and should not be exported (the package would be clearer and more direct without them than with them) ?
- Among the old exported programming functions, which ones are not really necessary and should be deprecated ?

#### Phase 20c — Review of all main user-facing functions arguments and global options
- What should stay an argument, and what would be more user-friendly as a global option ? What is a global option, and would be more user-friendly as an argument (or both, an argument that default to a global option) ? What arguments should be better integrated between `tab`, `tab_reg`, etc. for clarity and consistency ? 
- There are too many global options to teach to users, and many are new : if want to clean this before release. What could we remove ?
- `tab` and `tab_reg` have too many arguments, specially in their roxygen documentation. Are there good candidates for removal ? Are there arguments that we could `...` on main user-facing functions so that their documentation goes to an exported subfunction, to reduce the number of arguments in their documentation ? Are there arguments that we could group together, with a concise text refering to a specific documentation file  (or to a specific part of a vignette) for some subsystems ? 
- Give special attention to new arguments on 2.0.0, specially the new user-facing functions like `tab_reg()`, were change is free until release (and costly afterwards). 

#### Phase 20d — Jamovi UIs updates
Jamovi UIs have not been updated for a long time : add new arguments that are not in the current version of `jmvtab` and `jmvtabreg`, remove deprecated ones, etc. AskUserQuestion me when you have a doubt about what to add and what to remove.
- Levels reordering in jamovi UI : add a tick box (nothing ticked by default ; no tick box for the first level of each variable) to collapse a level with the former level (possibly in chain to collapse many levels together ; the chain must respect the order of levels chosen by the user), in which case a text box should appear taking all the vertical width of the levels that would be collapsed together to choose the new name. To get enough horizontal space for this, in the general case, row_vars, col_vars, tab_vars, etc. should each be on it’s own row of the layout (not two panes side-by-side). Are there caveats ? Are there better ways to do it ?
- `jmvtabreg` : add the same level reordering UI than `jmvtab` for predictors here too. They should be the same for UI consistency and ease-of-learning. If you can avoid code duplication, it’s better.
- For export folder, the ~ syntax is ok for expert users, but difficult for normal users and literary students. Is there a possibility to do the opposite : jamovi search the Documents folder at load like now (we fixed that), it writes the actual documents folder path in the folder text box, so the user sees it and can figure where the exports will go (and the default path button redo exactly that). ~ should continue to work and to mean home. When there are network disks, it would be preferable to refer to them in the readable way (`S:/...`) rather than the unreadable network path.

On the jamovi cache path a table built with `ci = "cell"` and MIXED col_vars renders its numeric column with the `pct_ci` display token where plain `tab()` renders `mean_ci`. A display-resolution divergence in `jmvtab_build`, not a weightsissue.

#### Phase 20e — Resolve jamovi freeze problems
- Even going marginal effects for a logit regression (one outcome) is neverending, but if I check "coefficient" again it’s working.


#### Phase 20f — `tab_reg()` parallelisation

`tab()` has had a parallel row-axis since Phase 8/9a (`R/tab-parallel.R`: `tab_pmap()` + trampoline,
the named `"tabxplor"` mirai pool, `tab_build_one()` as the per-row_var worker, Suggests-only).
`tab_reg()` has nothing, and the work it does is increasingly fit-bound. Research and design it **as a
whole** — pick the right level(s) of parallelisation after real measurement, rather than bolting a pool onto
whichever producer happened to get slow. Write the study in a new `dev/*.md`, pause ; then only plan and implement.

**Candidate payloads** (measure each; they have very different granularity and shipping cost):
- **Per-predictor crude fits.** z9's numeric `Obs_*` (univariable `glm` ~10.4 ms each, but
  `marginaleffects` AME **229 ms** each) and z10's ordinal `Obs_cumOR` (univariable `polr`: **794 ms**
  for 4 predictors, against 323 ms for the full model — 2.5x the model's own cost, on every
  interactive jamovi round-trip). Embarrassingly parallel, independent, small inputs.
- **Per-fit**: model comparison (`predictors = list(...)`), several dependents, `split_var` groups.
- **Per-contrast**: AME / `ame_ratio` calls, and z10's AME influence-function jacobians.
- The z8 `stats = "interaction"` pooled fits.

- **Shipping cost is the known hazard, already measured.** Phase o root-caused the jamovi
  model-comparison freeze to ~10 MB per raw fit and ~41.5 MB serialized per round-trip. A worker that
  returns a *fit* repeats that; one that returns only the tidy/digest does not. Design the worker
  boundary around what crosses it.
- **The `.fit_cache` seam.** `jmvtab_reg_build()` threads a cache **env**, which cannot cross a process
  boundary. Decide how parallel and cached interact (and note Phase o already disables the cache in
  comparison mode). `jmvreg_fit_key`'s reference-independence and `reg_reref_fit_res`' byte-identity
  (locked by `test-jmvtabreg-cache.R`) must survive untouched.
- **Byte-identity.** Every reg path is value-asserted, not snapshotted; results must be identical
  serially and in parallel, and stable in ORDER (`vec_rbind` of split parts, `fit_first_idx`/`fit_ncol`
  column mapping).
- **jamovi.** mirai's dispatcher needs sockets — `test-parallel-parity.R` already fails under the bwrap
  sandbox (`--unshare-net`) for this reason. Confirm a pool is viable inside flatpak Electron at all
  before assuming it; if not, the feature is R-session-only and jamovi keeps the serial path.
- **When NOT to parallelise.** `tab()`'s own answer (Phase 9c) was that scan fusion was a net negative
  once the build went O(cells); the honest outcome here may be "only above N fits / N ms", or a
  `tabxplor.reg_parallel_min` threshold. Do not ship a pool that costs more than it saves on the
  common one-model call.

**Reuse, don't duplicate**: the pool lifecycle (`tab_pool_ensure`/`tab_parallel_workers`/
`tab_parallel_stop`) and the `tab_pmap()` trampoline are the existing infrastructure; a second pool or
a second Suggests-guard idiom would be exactly the ad-hoc layer Phase 17 removed.





### Phase 2{x} — release



---







#### Phase 19h — KEY 7: one entry point, one return shape, one render model

**DONE (2026-08-14), both halves.** Full suite **FAIL 0, WARN 133, SKIP 4, PASS 6031**, against the
inherited FAIL 0 / WARN 127 / PASS 6005. The delta is *proved*: `dev/verify_golden_field_delta.R`
reports **one** changed case — the declared `f_totcol_each` — and no field, no column attribute, no
`test` column and no `meta` sub-field moving on the other 35 (**1655 cells**); `dev/verify_color_attrs.R`
prints **IDENTICAL** over its 293 cases (every stored colour attribute and both resolved slot vectors),
which closes 19c/19d's standing debt at last. No `_snaps/*.md` moved.

**The shape half.**

- **`tab_many()` is a translating shim**, 10 formals instead of 42 — it was the last home of the
  pre-2.0.0 vocabulary, which is why four public functions documented four spellings of one table.
  Every legacy name has an exact `tab()` equivalent, so it translates rather than degrades
  (`tab_deprecate_many()`, on the `tab_deprecate_or()` model): `chi2`→`test`, `totrow`/`totcol`→`tot`,
  `compact`→`output_list`, and `na_drop_all = c(a,b)` → `filter = !is.na(a) & !is.na(b)` — **exact**,
  not an approximation: `filter` is materialised on the unselected data and applied immediately before
  `tab_prepare()`, which is where `na_drop_all`'s own `na.omit()` ran (its only other effect was a
  "keep" speed shortcut that changes timing, not results). ⚠ Only the **five leading** positional slots
  are accepted: the two functions' 6th formals differ (`pct` vs `sup_cols`), so an unnamed 6th argument
  is **refused**, never forwarded into the wrong one.
- **`output = "legacy"` is deleted** with its only producer, and `getOption("tabxplor.output_kable")`
  is out of `merge_now` — a *display* option, read inside a *build* stage, that changed the **class** of
  the returned object. What `tab()` returns is now a function of `output_list` alone. The legacy
  irregularity (a list for ≥2 row_vars, a bare table for one) survives **inside the shim**, which
  unwraps a length-1 result itself — the deprecated function keeps its shape and it dies with it.
- **`tab_shape()` / `tab_supports()` / `TAB_OPS`** (`R/tab-shape.R`, both exported): the support matrix
  was written down nowhere and enforced by five aborts in three files. Details in the Repository Map.
- **The spread is ONE implementation.** `reg_spread_models()` is deleted; its two post-spread repairs
  were generic all along and are `spread_relabel()` inside `tab_spread()` — fold the level into each
  new column's `col_var` ("{level}<br>{col_var}", which is what makes the two-line span and the block
  borders), and re-key the `test` tibble. ⚠ The re-key needs a discriminator because **`test$col` holds
  two kinds of entity** — a crosstab row names a COL_VAR, a regression row names a COLUMN — so it is
  one rule ("follow `col` through the spread") with two lookups; unifying that overload is 19g's, and
  is left to a later phase. `tab_reg(spread_models =)` is **removed from the user surface** (maintainer
  ruling): the groups go side by side whenever that is unambiguous (one spec, non-multinomial), stay
  stacked otherwise, and `tab_spread()` is the public way to control the layout.
- **`totcol` collapses 5 states to 3.** ⚠ The classifier compared a **character** (`"last"`) against a
  **list of symbols** with `identical()`, so **both arms were dead**: `"all_col_vars"` was unreachable
  as a `tot_cols_type` and every `tab()` call fell through to the catch-all `"some"`, which was doing
  the work. `"one"` / `"no_delete"` / `"no_no_create"` is all there ever was. `"each"` and
  `"all_col_vars"` are accepted **spellings** of one total column now (never an error), and an unknown
  value aborts instead of silently meaning "col".
- `sup_cols` stops mirroring (one `tab_deprecate_sup_cols()` returning the `(col_vars, levels, pct)`
  triple — it was written into three arguments of the `tab_build()` call, with a fourth commented out);
  `names_prefix`/`names_sort` badged deprecated (verified: consumed at exactly one place, the spread
  path); `tab_md_css()` drops the argument documented as ignored; `?tabxplor-options` gains the live
  but undocumented `tabxplor.color_style_type`.
- **The `?tab` prose 19d parked here**: `OR` / `ci` / `color` rewritten **once**, and the mirrors
  attacked structurally — `@inheritParams tab` on `tab_plain()` / `tab_num()`, whose local copies were
  deleted (`tab_plain`'s `color` block still described the 1.x three-value string; `tab_num` still
  documented `ci_scale`, an argument 19d cut).

**The render half.**

- **D1 — `rd2` MODIFIES `rd`.** The literal it replaces enumerated ~39 slots, had already lost two
  silently, and was losing `ann$keep_black` — the "do not grey this cell" anchor set — behind a
  length-check fallback in the html engine, so a transposed regression's model-fit footer rendered
  **grey** where the native render keeps it black, with no error and no test. `keep_black` is flipped
  like every other per-cell logical now, and every slot the flip does not touch survives by
  construction. Ships with two fixtures.
- **D2** — `theme = "print"` on the kableExtra engine rendered `kable_material_dark`: a **black** table
  for the black-and-white publication palette. The branch tested `== "light"` against everything else;
  only `"dark"` gets the dark theme now.
- **One `"auto"` downgrade, one theme-option reader.** Measured, two of the five sites genuinely
  *honour* `"auto"` (browser-side and R-side — a deliberate divergence that stays); the real duplication
  was three downgrades and four option-chain spellings. `tx_theme_resolve()` + `tx_theme_option(scope)`
  (`R/tab-css.R`). That closes the live drift: `render_footer()`'s NULL-theme default reached for the
  **console** pair although `rd_footer()` calls it on the export path — the scope is derived from the
  medium now.
- **One `has_stars` font rule**, `tx_num_font(medium, has_stars)`. The three options **stay**, and the
  measurement is reported rather than the merge forced: a CSS font stack, an xlsx font *name* and a
  graphics family are not interchangeable values, and html/md has been unconditionally monospace since
  Phase g. What was duplicated is the switch, not the knob.
- **The backends read the model**: `rd_caption()` gained a `fallback` closure and absorbed `tab_xl`'s
  own copy (xl's two extra fallbacks are its policy, not a second caption *rule*); xl reads
  `rd$subtext`; `roles_col_var_edges()` states the three col_var-boundary conventions side by side
  (right edge / left edge / real-col_vars-only) where prep, xl and md each derived them from the same
  seed; `tx_strip_dep_suffix()` replaces the `" [dep]"` regex that was written twice, each copy
  commenting that the other existed.
- **"Is this coloured" is two questions, so it has two names**: `roles$color_cols` stays DECLARED (the
  legend describes the scheme, so it prints even if no cell reaches a break) and the new
  `roles$has_color` is REALISED (the span/CSS gate). Both from one `roles_color_flags()` called by the
  prep **and** the transpose — which is what stops the transpose defining them a third way.
  `md_has_color()` is deleted.
- **`tab_plot()` translates the footer typography** instead of overriding it. The `"runs"` medium
  already carries bold/italic/underline per token and `tab_xl` reads them; this backend dropped them,
  grouped by colour alone and forced `face = "bold"` — so under `theme = "print"`, whose palette encodes
  direction as bold-vs-italic on black text, the legend collapsed into one uniform run and said nothing.
- **Dead slots gone**: `vars$col_vars_levels` (no reader of the SLOT), `roles$no_totrows` (bound and
  never used again in `tab_plot`), `ann$anchor` (a prep-internal intermediate that was nevertheless
  shipped — and silently dropped by the transpose; a local now). `range_totcol` **stays**, declared
  dormant.

**Two defects found in passing, both fixed with their fixtures.**

- ⚠ **`tab(filter = )` accepted only a character string.** `tab()` forwarded
  `if (missing(filter)) NULL else {{ filter }}`, and `{{ }}` inside an `if` defuses **the whole `if`
  call** — so a bare `filter = !is.na(g)` was evaluated as `if (missing(filter)) NULL else !is.na(g)`
  inside the data mask and aborted, although `?tab` documents a dplyr::filter expression. `filter`
  reaches the internal engine already defused now (a quosure / a string / NULL), and an expression may
  reference the caller's own variables. ⚠ Second trap, hit twice: rlang gives a **constant** quosure the
  **empty environment**, so re-quoting a parsed string with `quo_get_env()` leaves it unable to find
  even `%in%`.
- **`tab_spread()` left the `test` tibble pointing at pre-spread columns**, so `test_grid_crosstab()`'s
  `intersect()` came back empty and a spread crosstab lost its **whole** test summary — the same defect
  `reg_spread_models()` had been fixing for the regression side alone since Phase 18m.

**HONEST CONCERNS.**

- **`pct` became per-`col_var` on `tab()`** (dropping a size-1 assert; the engine has always recycled
  it) so the shim could be lossless. That is a real surface addition in a phase that exists to shrink
  the surface — justified because `levels` and `digits` were already per-col_var and `pct` was the odd
  one out, but it is an addition. The per-**row_var** list form `tab_many()` also accepted is now
  **refused**, with a message: Phase 6 globalised that axis on purpose.
- **`tab_reg(spread_models = FALSE)` has no replacement.** Ten test call sites migrated to a models
  list (which stays stacked); a user who wanted the stacked shape for a single model no longer can.
  That is the maintainer's ruling, recorded here because it is a capability removal, not a rename.
- **The crosstab `col_var` fold is a rendering change** for `tab(spread_vars =)`: spread columns now
  report `"2000<br>marital"` rather than `"marital"`. Nothing in the corpus or the goldens covers a
  spread crosstab's render, so it is *asserted* by the uniform rule rather than *seen*. Worth one
  eyeball at 19n.
- **The `"all_col_vars"` string still carries two unrelated meanings** (the total-column tag and the
  `add_n`/`add_pct` helper tag). The right fix — `col_var = ""` plus a stored `role` on the helpers — is
  a stored-attribute change touching ~25 sites and every golden with `add_n`; it is harmless today
  because `is_totcol` separates them, so it goes to **19l** with that reason written down rather than
  half-migrated here.
- **`tab_ci()` / `tab_chi2()` did NOT get `@inheritParams`**: their `color` argument is a different
  axis (`"diff_ci"`/`"after_ci"`; `"all"`/`"all_pct"`), so inheriting would have documented the wrong
  vocabulary. Only their `ci` prose was updated.
- **`md`'s header blanking is NOT the prep's** — I folded it, the goldens caught it, and I reverted it
  with a WARNING in the code: the prep blanks only the literal `"row_var"` header and keeps a real
  variable name, md blanks them all (it renders the name as a body row). Two rules, deliberately.
- The 133 warnings are deprecation nudges (+6 from this phase's own new fixtures). The corpus-wide
  migration is still 19l's.

No `.a.yaml` / `.u.yaml` was touched, so **no `jmvtools::prepare()` is needed** — 19k still owns that.

**FOLLOW-UPS.** 19i can start on this commit. 19l: the `"all_col_vars"` disambiguation, the
deprecation-warning corpus migration, and re-checking whether `roles$new_col_var` and `has_color`
have grown other consumers. 19n: the spread-crosstab render eyeball, and the vignettes.

---

#### Phase 19i — The build pipeline and the `tab_counts` boundary

**DONE (2026-08-14).** Full suite **FAIL 0, WARN 133, SKIP 4, PASS 6042**, against the inherited
FAIL 0 / WARN 133 / PASS 6031 — same warning count, +11 assertions, nothing red. The delta is
*proved*: `dev/verify_golden_field_delta.R` reports **only the declared addition** across all
**1788 cells of the 36 goldens** (no field, no other column attribute, no `test` column, no other
`meta` sub-field), `dev/verify_color_attrs.R` prints **IDENTICAL** over its 293 cases, and
`test-parallel-parity.R` is green unsandboxed. Two fixtures moved, both consciously and both
*because of* a defect fixed here: 8 `tab_num()` goldens gain `meta$spec` (Defect 3) with one
snapshot line, and one `_color_golden` case (Defect 4).

**A — the settings spine becomes the ONLY carrier.** Measured on HEAD, `ctx$settings` had exactly
**one functional reader**: `tab_rowvar_ctxs()`, whose whole job was to slice it and then
**re-flatten** every column into the same ~15 bare names `tab_setup()` *also* wrote flat into the
ctx. Two carriers for one fact, with the flat one the interface. Now:

- **`ctx_settings_locals()`** projects the spine into those bare names at each stage head
  (`list2env(ctx_settings_locals(ctx), environment())`). Pre-slice a spine column projects to a
  VECTOR over row_vars, post-slice to the scalar the per-row_var stages expect — the same property
  the flat duplicates had, which is exactly why they existed. That is what made the migration ~6
  stage-head edits instead of ~200 rewritten reads. (Maintainer's ruling: *one carrier, projected
  locals*.)
- `tab_setup()` writes **neither** the 15 resolved duplicates **nor** the raw inputs the spine owns:
  **`SPINE_OWNED_INPUTS`** are DELETED from the ctx once consumed. So a bare-name read can no longer
  find the user's raw, unrecycled, pre-resolution value beside the resolved one — the two-carriers
  problem one step earlier.
- **`na` joins the spine at its two real grains** (the promise `settings`' own comment has carried
  since 17e): `pairs$na` per (row_var × col_var) — a text col_var whose row/col/tab vars are all in
  `na_drop_all` genuinely keeps its NAs, so the policy varies by PAIR — and `rows$na_num` per
  row_var. `lvs`/`lv1` join at theirs. The flat `na_text`/`na_num`/`lvs`/`lv1` are gone.
- `tab_rowvar_ctxs()` slices and stops; `per_rv` is four names. `tab_counts()` writes the spine where
  it hand-wrote the flat fields — and it wrote **neither `lvs` nor `settings$cols$lv1`** before, a
  latent gap closed on the way.
- **The line is stated**: the spine carries SETTINGS (values a user chose or a resolver derived) at
  one of three grains; never built OBJECTS (`fine_num`, `remove_levels`, the stage products).

**`new_ctx()` declares every live key** — 54 declared against ~81 live left 27 undeclared, and an
undeclared field is *absent*, so `list2env()` creates no binding and its own `is.null()` guard
**errors** instead of firing (19a's D7, generalised). The roadmap's alternative ("17 fields become
locals") was **measured and not taken**: every one is either a public input or a product that
genuinely crosses a stage boundary, so converting them removes nothing. A free win fell out:
`utils::globalVariables()` for those bindings is **derived** from `new_ctx()` + `CTX_SETTINGS_LOCALS`
(the 19g move for `reg_build`'s `shared`), deleting a ~70-name hand-kept mirror in `fmt_class.R` that
had already outlived a field it named. ⚠ It must sit at the **end** of `tab.R` — `new_ctx()`'s
defaults call `conf_level_default()`, defined further down, and top-level code runs in source order.

**The two leaves share their head and their tail.** **`leaf_finish()`** (row-index declaration →
group-or-not → `new_tab`/`new_grouped_tab` → `tab_stamp_inference` → `leaf_extract_raw`) replaces two
~30-line blocks that differed in ONE thing — and that one thing was a defect (below).
**`leaf_inference_setup()`** is the 6 statements the preambles genuinely share; the ~45 lines of
divergent comment around them stay put, because they are divergent. **`num_total_postprocess()`**
folds `num_core`'s two identical post-rollup blocks.

**B — one argument boundary.** **`tab_resolve_common_args()`** (`R/tab-resolve.R`) is what every
crosstab producer does to its arguments, run once: the `chi2` → `test` rename, validation,
`resolve_cleannames`/`_stars`/`_ci_method`, the `OR` route, `normalize_color_spec` + D28 **on the
spec**, `tot` → `(totrow, totcol)`, `total_names`. Five hand-written copies collapse — copies that
had drifted in ways a reader could not distinguish from intent (`tot`'s "both" expansion written 4×,
one of them differently; `na`'s allow-list 3× with 3 contents; `pct`'s vocabulary 3×, one of them
checking the SIZE only). **`TAB_ARG_VALUES`** is the vocabulary as data (`values` / `leaf` / `size` /
`na_ok`), read by `tab_validate_args()`; the ruling was **abort on every unknown value**, so
`totaltab` / `n_min` / `conf_level` — validated nowhere at all — now abort naming the valid set, and
`conf_level = 95` suggests `0.95`. `tab_counts()`'s inert `ci_method` mean slots became a real
refusal.

**Five defects, each with the fixture that fails without it** (`test-arg-boundary.R`):

1. **`tab_counts()` stored a significance gate it never applied.** It builds a `color_spec` and
   finalises it but never ran `ci_disable_signif()`, so with `ci = "cell"`/`"no"` every column
   carried a `color_signif` the resolver ignored — the exact situation D28's own comment says the
   rule exists to prevent.
2. **`options(tabxplor.stars = TRUE)` reached `tab()` and not `tab_num()`.** Measured: the same call
   built a reference interval through one and none through the other. `tab_num()` handed a possibly-
   `NULL` `stars` to `resolve_leaf_ci()` — which tests `isTRUE(stars)` — and resolved it against the
   option only much later, inside `num_core()`.
3. **A direct `tab_num()` carried no `meta` at all**: no `spec$kind` (19g's `tab_kind()` fell back to
   its degraded guess) and no `vars$wt`, so a weighted one printed no "Weighted by …" footer. `tab()`
   masked it by setting the meta itself at assemble. Fixed by `leaf_finish()`, which is why the 8
   numeric goldens moved.
4. **`tab_num(color = "after_ci")` dropped the policy half of the composite.** `resolve_leaf_ci()`
   was handed the RAW `color_signif` argument instead of the DECODED `color_spec$signif`, and its
   `if (signif_on) … else "ignore"` overwrote it — so the leaf stored `"ignore"` where `tab()` stored
   `"guaranteed_effect"` for the same request. **19c's standing warning, hit again**: decode the
   alias FIRST, normalise second.
5. **`totaltab = "tabel"` silently meant "no total table"** (and `conf_level = 95` reached the
   interval engine as a probability). Closed by B1.

**Three roadmap items were measured and NOT done**, with the measurement recorded rather than the
item silently dropped:

- **Folding `num_core`'s totals into `build_total_rows()`/`finalize_total_rows()`** — the two use
  *deliberately different* accumulators (`base::sum()` over `split()` because data.table gforce
  drifts 1 ULP, which `build_total_rows()`'s own header says), and both carry byte-parity locks.
  Maintainer's ruling: skip the merge, dedupe inside `num_core`. Written into both headers.
- **"17 ctx fields become locals"** — see above.
- **Routing `tab_ci(ci =)` through `resolve_ci_value()`.** Tried, and it was wrong: this superseded
  STEP speaks the COMPUTATIONAL vocabulary, in which `"diff"` is its own native word — the pipeline
  itself calls it that way (`tab_apply_tests` hands it the resolved step value), so the public
  resolver fired a deprecation on tabxplor's own build. What it really lacked was a *declared*
  vocabulary, so it got one: **`TAB_CI_STEP_VALUES`**, beside `resolve_ci_value()`, with the
  difference stated. A smaller win than planned.

**HONEST CONCERNS.**

- **`ctx[SPINE_OWNED_INPUTS] <- NULL` is a deliberate sharp edge.** A future stage that reads a raw
  `pct` / `color` / `ci` after `tab_setup()` fails loudly ("object not found") rather than getting a
  stale value. That is the intent, and the three `tab_transform()` NULL-fallbacks are the documented
  no-spine path — but it is a rule a reader must know, so it is stated in the code, in the header and
  here.
- **The jamovi boundary was NOT consolidated** — 19k owns it, and it already reuses
  `resolve_leaf_ci()` / `normalize_color_spec()`. What this phase did there is the minimum the spine
  change required: its three pre-slice ctx reads now read `ctx$settings`. Its own hand-mirrored rules
  (the digits floor, the population descriptor, family detection) are untouched. ⚠ Note it passes the
  RAW `color_signif` to `resolve_leaf_ci()`, the shape of Defect 4 — harmless today because the UI
  never sends a composite, and left for 19k with the rest of that boundary.
- **`test-carve-parity.R`'s ctx assertions were rewritten**, which the plan flagged as the signal
  that the design moved — it did, by design: they now assert that each fact is ON the spine and NOT
  flat beside it, which is the phase's contract rather than a description of the old shape.
- **`tab_ci()`'s `ci_scale` and `comp` keep their own `stopifnot`** (they are step-internal, not
  public-surface vocabularies).
- `?tab`'s prose was rewritten in 19h; only `NEWS.md` needed the new aborts.

No `.a.yaml` / `.u.yaml` was touched, so **no `jmvtools::prepare()` is needed** — 19k still owns that.

**FOLLOW-UPS.** 19j can start on this commit — the leaf now owns its whole head and tail, which is
what KEY 5 needs. 19k: the jamovi boundary's own mirrors, including the `color_signif` note above.
19l: the deprecation-warning corpus migration (133 remain).

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
   - the **Repository Map** / *Key Constraints* / *Design Decisions* entries of anything you really changed (a new module, a renamed function, a new config field). When there is nothing to change, *skip it*.
   - the phase **"DONE" summary**, under its own `#### Phase <x> — <title>` header in the roadmap section. **CLAUDE.md is the ONLY place it goes**. The maintainer moves done phases to `dev/tabxplor_2.0.0_roadmap_DONE_PHASES.md` himself.
4. (`NEWS.md`: user-facing and CRAN-facing, tracking new functions, new arguments and arguments changes, deprecations, and really important user-facing bugs fixes. Keep it *fully* minimalistic and *radically* no bullshit. Do not edit it when it’s not necessary. Most of the time, it’s not necessary.)
5. (`README.Rmd` : user manual. Only update before release of new version to CRAN, never before.)
