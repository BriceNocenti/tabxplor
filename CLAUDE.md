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
│                              20a: **tab_columns(x)** (exported) = the COLUMN-axis mirror -- one row
│                              per fmt column x its 19 stored facts, off fmt_attrs_of(). The only
│                              place conf_level / degf / basis / ci_method are readable side by side,
│                              which is what z13/z16 stored them per column FOR. Reports the STORED
│                              values, so `conf_level = NA` honestly means "no interval was stamped".
├── zzz-fact-keys.R  (~250 L) Phase 20a (KEY 2): REFERENTIAL INTEGRITY between the declared fact
│                              tables. **TAB_FOREIGN_KEYS** = 34 declared edges (from / get / to /
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
├── tab-args.R      (~1000 L) Phase 20b (KEY 1 + KEY 8): THE argument surface as data.
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
│                              +color_measures_rd (from MEASURES' new `doc` member, filtered by
│                              `producers`) and color_signif_rd; `{VALUES}` in a `doc` is where the
│                              generated list is spliced. ⚠ read the rows with `[[`, never `$`:
│                              `r$values` partial-matches `values_from`.
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
| tab_reg                  | Phase 12c–12g LIVE: unified regression tables (gaussian beta / binomial OR / poisson IRR / multinomial OR / ordinal cumulative OR) over lm/glm/svyglm/svyolr/svy_vglm/nnet::multinom/MASS::polr + broom (no parsnip). A binary outcome is `tab_reg(family = "binomial")` (20a deleted the tab_logit/multi_logit wrappers). **The estimand is `effect` x `measure`** (19e, R/reg-estimand.R): the row it resolves to declares the fit, the `exp` flag, the header word and the stored `scale` -- additive beta -> the `diff` field + scale "raw_diff"/"log_coef"; multiplicative OR/IRR/cumOR -> `or` + scale "odds_ratio"; a ratio of means -> `ratio` + "mean_ratio". `exponentiate` / `at` / `estimate_display` are DELETED (`measure = "log"`, `effect = "at_reference"`, a real `display =`); `type`/`ci_type` are gone (19b). The `var` field carries var(Y). 12d: MNL = one OR col per outcome category vs ref; ordinal polr + Brant PO diagnostic. 12f: model-summary footer + compare= in the `test` attr. 12g / z14-i: SURVEY designs — `wt=` (a flat ids=~1 design), or a prebuilt `survey::svydesign` as `data` for anything richer (clusters / strata / fpc / CALIBRATION); `ids=`/`strata=`/`fpc=`/`nest=` are REMOVED (they reached only the omnibus p) and a svrepdesign/twophase is refused. A design's own weights become `.svy_weights` at the shared boundary, so the crude `Obs_*` columns, the AME, the frozen SD, the gap-test influence weights and the footer are all design-weighted (they silently were not); reduced weighted glance (Wald/Nagelkerke/Cox-Snell/Rao-Scott-AIC) + weighted compare (anova.svyglm Wald); weighted 3+ level (svyolr / svyVGAM); `split_var` (tab_vars analogue, tab_spread-able); `multiplier` (the UNIT a continuous predictor's effect is reported per -- **default `"sd"`** since z9, so `Model_*` on a numeric row is per-1-SD, NOT `exp(coef(glm))`, unless `multiplier = 1`); `empirical_OR` (crude %/OR beside model OR, binary; z9: continuous predictors too, from their univariable fit). No new fmt fields; new Suggests svyVGAM. |


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
| `test-tab_reg-binomial.R`| Binary outcomes: OR/CI/p parity vs glm/svyglm, 1/OR (was test-tab_logit.R)                      |
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

| key | the missing fact / unstated rule | lands in |
|-----------|------------------------------------------------------------------------------------------------------------------------------------|----------|
| **KEY 1** | *which producer takes which argument, what it means, what it may be, which option is its default* → `TAB_ARGS`, generated `@param`s and value lists, `...` on the variants | **20b** |
| **KEY 2** | *a key written in one declared table and read in another is a foreign key* → ~14 checks, at load time | **20a** |
| **KEY 3** | *which accessors exist* → one generic `fmt_attr()` pair + a measured keep-list + `tab_columns()` | **20a** |
| **KEY 4** | *if two producers ask the same question, they ask it with the same word* → `tab_vars`, `ref`, `ci_method`, `footer` | **20c** |
| **KEY 5** | *what kind of statistical row this is* → `TEST_ROWS`; the crosstab half of the footer finally declared | **20c** |
| **KEY 6** | *which stage of a regression build produced which part of the table* → `new_reg_ctx()` + five named stages | **20e** |
| **KEY 7** | *which estimands tabxplor can differentiate analytically* → a declared `se` column; the AME stops being computed twice | **20d** |
| **KEY 8** | *the export surface re-declares seven arguments five times* → `TAB_ARGS` covers the exporters too — **not** a `tab_style()` bundle | **20b** |
| **KEY 9** | *a package whose whole value is a data model states it in one place* → `?tabxplor-model` | Phase 22b |

**KEY 1 is this phase's keystone**, as KEY 5 was of Phase 19. Everything else is a prerequisite for
it (2, 3), a second instance of it in another subsystem (4, 5) — or, in KEY 8's case, **the same
instance**: the export surface's duplication turned out to be KEY 1's, one subsystem further out.
Only 6 and 7 are independent of it.

---

#### Settled decisions — do not re-open

Thirty rulings; full table + rationale in the plan of plans §4. The ones that change what
gets built:

| decision | ruling |
|---|---|
| **both proposed bundles** | **REJECTED.** No `tab_inference()`, no `tab_style()`. `ci_method` / `design_effect` / `anova` stay flat with their option twins (the only change is `tab_reg(method =)` → **`ci_method`** with a declared `model` slot); the exporters keep every formal. *A bundle must make the common call shorter, not only the signature* — the general test for every future one. And the lesson generalises: **a mirrored formal is not automatically a problem** — 7 defaulted arguments × 5 exporters cost a user nothing, the duplication was 35 hand-written `@param` blocks, which is KEY 1 |
| `tab()`'s 9 deprecated formals | **into `...`**, caught by name, with an **abort on an unnamed 6th argument** |
| the legacy step API | **hard-deprecate now**, defunct in 2.1.0 — the exported *chaining API*, never the computations (those moved into the leaf in 19j). ⚠ removes nothing this cycle: `tab-steps-legacy.R`'s 1 433 lines stay |
| `tab_many()` | **stays soft-deprecated**; only its 448-line `.Rd` is fixed (`@inheritDotParams` → plain `@param`, −390) |
| `tab_logit()` / `multi_logit()` | **deleted** (genuinely unreleased) — ⚠ **59 test call sites** to migrate, not "nothing references them" |
| `tab_reg()` renames | `split_var` → **`tab_vars`** · `dependent` → **`outcome`** (package-wide) · `reference` → **`ref`** (`c(var = "level")`, **predictors only**) · `method` → **`ci_method`** · `stats`+`compare`+`baseline` → **`footer`** · `.fit_cache` → `...` |
| **`outcome_level`** (new) | `inverse_two_level_factors` is **deleted** for `outcome_level = c(outcome = "level")`. ⚠ NOT absorbed into `ref`: **`ref` names the level you compare AGAINST, `outcome_level` the level you MODEL** — opposite roles, so one argument would carry two meanings. binomial → the modelled level (the column header); multinomial → the baseline (taking over what `reference` does today); ordinal → **refused**. Precedent: SAS `PROC LOGISTIC` has exactly this pair, `EVENT=` beside `REF=` |
| `tab(ref / ref2)` · `na`'s two vocabularies · the `color` default asymmetry · `pct = "no"` | **unchanged** — the last two deliberate; *state* `pct`'s default in `?tab` rather than change it |
| `TEST_ROWS` · reg parallelisation · the jamovi level-collapse UI | **all inside Phase 20**, pre-release — parallelisation as its own phase (20f), gated on 20d and 20e and free to conclude "no" |
| accessors | **generic mostly, a few named ones kept — the most used.** The keep-list is measured at plan time and must include `get_col_var()` and `set_row_kind()` |
| `new_lvl()`/`is_lvl()` **stay exported** (a user meets the class) · `tab_prepare()` + `complete_partial_totals()` **off the public surface** · `tab_get_wrapped_dimensions()` → `@keywords internal` · `tabxplor.color_style_type` deleted | ⚠ `tab_prepare` and `complete_partial_totals` **are CRAN 1.3.1**, so both take the deprecate-now / un-export-in-2.1.0 route — never a silent drop |
| `tabxplor.stars` absorbs `signif_levels`+`signif_labels` **and becomes a per-call ladder** · new `options(tabxplor.total_names = c(row=, col=, tab=, other=))`, **and `total_names` / `totaltab_name` / `other_level` leave the signatures** | ⚠ all three are **CRAN 1.3.1 formals** of `tab()` (and of `tab_many()`), so they go through `...` with a deprecation, not out |
| `@inheritDotParams` | **never** — it *inlines*; `tab_many.Rd` is the 448-line proof |
| `...` | on **wrappers and superseded producers only**; `tab()` and `tab_reg()` keep every live formal |
| the `tab_kable_*` / `xl_font_*` option renames · a JS syntax gate · column-axis `ordered` | **stay dropped** — do not re-propose |

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

| phase | title | one line |
|---------|--------------------------------------------------------------------|------------------------------------------------------------------------------------------------------------------|
| **20a** | The floor: referential integrity, the exposed surface, dead weight | KEY 2's foreign keys · KEY 3's `fmt_attr()`/`tab_columns()` · the deletions and demotions · `tab_many.Rd` −390 · the two harnesses · the 2 live colour-engine `FIXME`s |
| **20b** | KEY 1 + KEY 8 — the argument surface as data, producers and exporters alike | `TAB_ARGS` + generated `@param`s and ~15 value lists, **the five exporters included** (part 1, byte-identical) → `...` + `tab_check_dots()`, `tab()` 52 → ~37, the `stars` ladder, `total_names` (part 2) |
| **20c** | KEY 4 + KEY 5 — one word per question, and the footer's model      | the `tab_reg()` renames incl. `dependent` → `outcome` and the new `outcome_level` · `footer =` with `TEST_ROWS` as its vocabulary |
| **20d** | KEY 7 — marginal effects, computed once and computed fast          | 85 % of a 15.3 s call is an avoidable *numerical* jacobian, and tabxplor already owns the analytic SE. Then the research half: can `marginaleffects` leave the hot path entirely? ⚠ **web searches expected** |
| **20e** | KEY 6 — `reg_build()` becomes a staged build                       | the package's largest function (534 lines, 7 local closures, 11 unnamed phases) gets `new_reg_ctx()` + five named stages. **Pure refactor**: `verify_reg_specs.R` must print IDENTICAL |
| **20f** | `tab_reg()` parallelisation: measure, then decide                  | ⚠ **re-measure first** — if 20d got the call to ~2 s the case may have evaporated. Study in a `dev/*.md`, **pause**, then implement only what the measurement justifies. **A measured "no" is a complete phase** |
| **20g** | jamovi: the level-collapse UI, the boundary, the rebuild           | every new vocabulary into the `.a.yaml`s (generated) · the collapse as a real `tabxplor_lvl` R operation emitted into both modules · the readable export path · the owed `prepare()` + live pass |
| **20h** | Harvest 1: the deletion pass                                       | re-run the censuses, delete what the new declarations made unnecessary, and **report what did not shrink** — that report is the product |
| **20i** | Harvest 2: open integration                                        | ⚠ creative, own session: what does the finished surface make *possible*? Look and propose first — **ask before building** |

**Dependencies**: 20a first · 20b and 20c need 20a's harnesses · 20d needs 20c · 20e needs 20d ·
20f needs 20d+20e · 20g needs 20b/20c/20d · 20h then 20i last.

⚠ **The three `tab_reg()` phases are deliberately separate sessions** — one story, three frames of
mind: 20d is **numerical parity** (research, closed forms, tolerance fixtures), 20e is a **pure
structural refactor** proved by one harness printing IDENTICAL, 20f is a **measurement** that may
conclude "no". Interleaving them is how a refactor and a numeric change land in one diff and
neither can be verified.

**Mapping from the old draft** (nothing lost): old 20d (jamovi UI) → **20g** · old 20e (the
marginal-effects freeze) → **20d**, root-caused as KEY 7 · old 20f (parallelisation) → **20f**,
unchanged in content but now gated on 20d and 20e.

**At the end of each Phase,** add a `#### Phase 20{x} — <title>` header **here, in CLAUDE.md**, and
write the **"DONE" summary** under it. Write it in **this file and nowhere else**. Update the
Repository Map above in the same pass, yourself.



### Phase 22 — documentation integration and simplification

#### Phase 22a — Architecture document simplification. 
- The document must be presented around the real design goals and real-world usage of tabxplor.
 
#### Phase 22b — simplification and integration
- Document undocumented stuff. `spread_vars` in `tab()` ?
- Vignettes should not be neverending. If some aspects, either expert, or on the contrary pedagogical and near useless to experts, need to be placed in new vignettes, make me propositions.
- Document the family x effect x measure stuff in regression vignettes, in an expert section, adding a clear, very concise and user-friendly markdown table (like for color x type x color_signif in the introduction vignette) stating what combination does what in terms broadly understandable by experts/in glm() terms. It should also be usable for teaching the framework.

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

### Phase 2{x} — release

Procedure: **`dev/release_checklist.md`** (branch mechanics — ⚠ merge commit, never squash; the
strip list; `.Rbuildignore` identical on both branches; tag *after* CRAN acceptance — ⚠ the only
existing tag is `v1.2.0`, 1.3.0 and 1.3.1 were never tagged).
Gate set + the three owed maintainer items (README hero screenshot, `cran-comments.md`, the jamovi
live pass): **§10.2 of `dev/tabxplor_phase20_surface_integration.md`**.

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
+ `...`, `tab_num` 28 → 9 + `...`, `tab_counts` 40 → 10 + `...`, `tab_many` unchanged).
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
and `mean` is dropped because a ratio of two means is not a mean. No behaviour change; two `# DESIGN:`
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
