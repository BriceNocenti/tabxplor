# tabxplor — AI Assistant Guide

## Package Purpose

`tabxplor` is a public CRAN R package (v1.3.1) maintained by me for creating, manipulating, and formatting color-coded cross-tabulations. It uses `data.table` for fast aggregation internally and exposes a `tidyverse`-compatible API via custom `tibble` subclasses. Tables can be exported to Excel (with colors), HTML, Markdown, and ggplot. All numeric cells are stored as `vctrs` record vectors (`tabxplor_fmt`) carrying full statistical metadata, enabling lossless display switching.

---

## Repository Map

```
R/
├── fmt_class.R     (~4400 L) Core type: tabxplor_fmt vctrs record, getters/setters, new_fmt() +
│                              fmt_field_names (the 21 fields; s +n_eff, z5 +obs, z8 +gap_se) + DERIVED fmt_col_attrs (14 attrs)
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
│                              format/pillar methods, vctrs arithmetic/casting,
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
│                              pipeline must compute; measure_stage() derives WHICH step stamps, and
│                              it absorbed jmv_tab3_arming), `requires` (always/gated, keys ref/ci/
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
├── tab.R           (~6640 L) Main API: tab(), tab_many(), tab_plain(), tab_num(),
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
│                              wrapper speak ONE display grammar. tab_apply_reference() = the ONE
│                              reference executor
│                              (tab_num's diff_index_mean twin + inline calculate_refrows copy DELETED).
│                              display_write_col() = THE per-column display-template writer, shared by
│                              build-time tab(display =) and post-hoc set_display(col, "num_ci")
│                              (fmt_apply_num_ci DELETED: the two copies disagreed on every total row).
│                              D22 is PER-CELL there -- a template is written only where every one of
│                              its fields exists -- and D23 (display_refuse_mismatch) refuses two
│                              EFFECT geometries, never a LEVEL beside a comparison interval
│                              ("48% [-3;+4]" IS the flagship cell). WARNING: its `across()` callback
│                              must stay a NAMED function -- dplyr inlines an anonymous one into the
│                              data mask, `r$col` yields NULL, and across() DROPS the column.
│                              tab_prepare(), tab_ci(), tab_chi2(), tab_spread(), tab_get_vars(),
│                              tab_render_vars() (Phase 10c: robust group_vars-based role detection +
│                              graceful degrade, used by print + exporters),
│                              tab_add_n_pct() (shared add_n/add_pct, used by tab_many + tab_counts).
│                              tab_build() = staged pipeline over a TYPED ctx (17e: new_ctx(), one
│                              defaults list, kills the exists() guards + both hand-written ctx literals):
│                              tab_setup (builds the SETTINGS SPINE ctx$settings = rows/cols/pairs star
│                              schema; pairs REPLACES pct_vect/ref_vect -- the axes meet only there) /
│                              tab_prepare_pop / tab_aggregate / tab_build_tables (Phase 9a: the OUTER
│                              row_var map -> tab_build_one, + tab_rowvar_ctxs, which 17e slices by KEY --
│                              length heuristic GONE) ; tab_transform / tab_assemble_tables are SCALAR
│                              over one row_var ; tab_assemble_output (merge/pvalue/unwrap);
│                              tab_lump_others/tab_cleannames_relabel (extracted from tab_prepare)
├── tab-agg.R        (~500 L) Aggregate-core (Phase 2-3) + z16-iiiii's **CI_METHODS** = THE interval-method
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
├── survey-design.R  (~335 L) z14-i: THE survey-design BOUNDARY + the constructors + the robust overlay.
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
├── survey-variance.R (~410 L) z14-ii Route A: the DESIGN variance of a table's cells -> the existing
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
├── row-model.R      (~245 L) Phase 19f (KEY 1): THE ROW MODEL -- what a row IS, given the same treatment
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
├── tab-counts.R     (~360 L) tab_counts() from-the-middle constructor (Phase 4): reshape any
│                              input shape → count-aggregate → tab_plain(.fine) + shared finalize
├── tab-resolve.R    (~230 L) tab_resolve_settings() (Phase 7b): the ONE pure arg-overwrite
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
│                              consumer asks the measure instead: measure_stage() (leaf vs test
│                              step), measure_applies() (can it colour a mean), measure_forces().
│                              19d: THE comparison chain (`color` -> `display` -> the difference) via
│                              display_comparison()/tab_leaf_comparison(); resolve_ci_value() (the
│                              c("auto","no","cell","ref") anchor + its soft-deprecations);
│                              resolve_leaf_ci() = the SAME rules for a leaf called directly AND for
│                              the jamovi boundary (jmvtab_build's 2 hand-mirrored ci rules are gone);
│                              measure_geometry() = which of the 3 geometries owns the stored interval
│                              ("or"/"ratio"/"diff"), shared with the jmvtab tier-3 cache TUPLE so the
│                              cache and the pipeline cannot disagree (a diff<->ratio toggle changes
│                              the interval, so it can never be a re-paint); ci_disable_signif() =
│                              D28's ONE rule (`ci = "cell"` informs and disables stars/color_signif),
│                              called by both resolvers AND by tab()'s argument boundary -- the last
│                              because the STORED policy attribute is written from the colour spec,
│                              not from what the resolver decided.
├── tab-parallel.R   (~200 L) Phase 8/9a row-axis dispatch (Suggests-only mirai): tab_pmap() + trampoline,
│                              named "tabxplor" pool (tab_pool_ensure/tab_parallel_workers/
│                              tab_parallel_stop), tab_build_one() (the per-row_var worker, serial OR mirai).
├── tab-steps-legacy.R (~700 L) Phase 17f quarantine: the superseded dplyr-era step functions
│                              tab_pct()/tab_tot()/tab_totaltab() (exported, superseded badge) + their
│                              trio-exclusive helpers pct_formula()/diff_formula(), moved OUT of the live
│                              tab.R pipeline. They call INTO shared helpers that stay in tab.R
│                              (tab_match_groups_and_totrows/tab_add_totcol_if_no/tab_validate_comp/
│                              tab_match_comp_and_tottab) + live tab_ci()/tab_chi2(); nothing here is
│                              called BY the aggregate core.
├── tab_classes.R   (~3700 L) tabxplor_tab/grouped_tab classes, 30+ dplyr S3 methods,
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
│                              + docs/fr + the toggle removed; FR vignette-articles stay in Articles)
├── tab_xl.R        (~595 L)  Excel export via openxlsx2 (Suggests-only; Phase 10h). Single-tab-first
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
├── tab-xl-backend.R (~110 L) Phase 10h openxlsx2 backend: plumbing xlb_* engine wrappers (in-place R6
│                              $, +xlb_merge) + the pure range coalescer (xl_runs/xl_coalesce -> fewest
│                              multi-area dims). Styling-model notes (precompose + set_cell_style path).
│                              Phase o: xlb_dims_each splits a comma multi-area dims to single ranges at
│                              the emit (xlb_numfmt/xlb_set_cell_style) -- the OLDER jamovi-bundled
│                              openxlsx2 rejects multi-area dims (the Excel-export crash).
├── tab_md.R         (~530 L) Markdown export: plain padded pipe table + (Phase 10f) pandoc colour
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
├── tab-css.R        (~290 L) Phase 13d: THE one CSS generator, shared by tab_md + tab_kable("html").
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
├── tab-export-prep.R (~570 L) Phase 10d shared exporter prep: tab_export_prep() -> tabxplor_render
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
├── tab-transpose-render.R (~230 L) Phase 14o: THE render-level transpose. tx_transpose_render(rd,
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
├── tab-render-html.R (~370 L) Phase 10e tab_kable render seam: render_kable_html() (kableExtra +
│                              home-built html engines) + tab_kable_join(css=)/scrollbox. 13d: the html
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
├── utils.R         (~945 L)  .onLoad() options setup + tx_getOption() (17j: the ONE option-synonym
│                              resolver -- first name set wins, seeded/canonical LAST; backs the
│                              tab_kable_css [was kable_css] rename + the console_theme/export_theme
│                              silent aliases), factor/list utilities, tx_str_wrap/tx_str_trunc
│                              NOT the colour-palette DESIGN tools (preview_color_grid /
│                              simulate_cvd_farver / plot_oklch_hue_strip_cvd / set_luminance...):
│                              they live in dev/color_palette_tools.R and must stay there -- they
│                              are the sole reason the package would depend on farver + colorspace.
├── tabxplor-options.R (~110L) Doc-only page `?tabxplor-options`: every tabxplor.* global option
│                              (defaults live in .onLoad; keep in sync). Cross-linked from ?tab.
├── tab_reg.R       (~1780L)  Phase 12c–12h: unified regression tables. tab_reg() over ONE engine
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
├── reg-assumptions.R (~730 L) Phase 18z15: THE model checks of a tab_reg() table, their CURE
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
├── reg-influence.R  (~450 L) Phase 18z8-B: influence functions + the SE of the gap between two
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
├── plots.R         (~1010 L) z17 (was tab_reg_plots.R): the package's data CHARTS + the ONE model they
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
├── jmvtab-cache.R  (~910 L)  17i: the SHARED cache kernel at the top (jmv_cache_config +
│                             jmv_store_new/migrate/env/fetch/put/evict/cached, ONE byte-bounded LRU
│                             O(n log n), canonical entry list(value,bytes,seq); jmv_hash/jmv_col_fp),
│                             consumed by BOTH stores as config -- JMVTAB_CFG (3 tiers agg/test/tab3,
│                             schema 6) + thin jmv_cache_* wrappers. Then jmvtab live multi-tier cache:
│                             content-addressed store + jmv_cache_aggregate (tier 1-2, tab_aggregate hook) + the Phase 7f
│                             tier-3 CARRIER cache (Phase 9b-7: jmv_carrier_unwrap/wrap store, not a
│                             live tab; jmv_tab3_base_key/tuple, jmv_reapply_digits re-paint +
│                             jmv_tab3_reref/rerefable instant reference re-ref) + jmvtab_build
│                             (engine-free core; reuses tab() via .cache) + jmvtab_ref_vector (ref-picker)
│                             + jmvtab_levels_order/jmv_relevel_cols (7g-ii level-reorder,
│                             post-aggregate; .levels_order arg on tab())
├── jmvtab-export.R  (~160 L)  jmvtab export helpers (Phase 7g; 15c robustness): resolveExportPath now
│                             takes (dir, filename, ext) -- fs::path_home Documents default + fs::
│                             path_sanitize filename + quote/bracket strip + format-driven extension
│                             (export_home_dir/_documents_dir/_expand_home/_unwrap/_sanitize_filename
│                             helpers, all fs-guarded w/ base-R fallback); tab_html_string (self-
│                             contained HTML); jmvtab_export (Excel/HTML/MD dispatch) w/ friendly
│                             pre-flight (openxlsx2 / dir-create) + UNwrapped writer so the .b.R
│                             conditionMessage() surfaces the real cause (not "In index: 1."). 17i: also
│                             the SHARED R6 backend helpers jmv_backend_weights/_notice/_export/
│                             _render_html (the 4 verbatim blocks both .b.R files now delegate to)
├── jmvtab.b.R       (~140 L)  Jamovi module backend (R6): thin orchestrator over jmvtab_build + $state;
│                             17i: weights/export/notice/render delegate to jmv_backend_* (export helpers)
├── jmvtab.h.R       (605 L)  Jamovi module UI (auto-generated, do not edit)
├── jmvtabreg-cache.R (~270 L) Phase 15b: the jmvtabreg (Regressions) live-UI fit cache +
│                              jmvtab_reg_build() engine-free core (drives tab_reg(.fit_cache=)). 17i:
│                              rides the SHARED kernel (JMVREG_CFG: 2 tiers digest/fit, schema 3) -- the
│                              duplicated + O(n^2)-evicting store lifecycle is gone, only thin jmvreg_*
│                              wrappers stay; jmvreg_fit_key (ref-INDEPENDENT digest key -> a reference
│                              change is a HIT) + the picker folders jmvtab_reg_ref_vector (reference),
│                              jmvtab_reg_models (15b-ii "+" builder -> `predictors` list / flat pool),
│                              jmvtab_reg_mult_vector (numeric scaling -> `multiplier`). 15b-ii raised
│                              the raw-fit ceilings (fit 4->24MB, store 16->96MB) so comparison fits (a
│                              raw reg_fit ~9-11MB) cache instead of graceful-skipping. 15d: the
│                              per-dependent Model table (depFamily/depModelLevel/depTrials) ->
│                              jmvtab_reg_dep_family/_dep_modelled_first/_dep_trials. 15e: jmvtab_reg_build
│                              calls tab_reg() ONCE with per-dependent family/inverse/trials VECTORS -> one
│                              mixed-family table (no more group-by-family / tabxplor_tabs stacking)
├── jmvtabreg.b.R   (~110 L)  Phase 15b: jmvtabreg R6 backend (thin orchestrator, sibling of jmvtab.b.R;
│                              .h.R generated by prepare() -- inherit is lazy so it loads before then;
│                              17i: weights/export/notice/render delegate to jmv_backend_*, keeps .hint)
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
| tab_reg                  | Phase 12c–12g LIVE: unified regression tables (gaussian beta / binomial OR / poisson IRR / multinomial OR / ordinal cumulative OR) over lm/glm/svyglm/svyolr/svy_vglm/nnet::multinom/MASS::polr + broom (no parsnip). tab_logit/multi_logit are binomial wrappers. Effect shape is exponentiate-driven: additive beta -> `diff`+type="coef"+display="coef"+ci_type="diff"; multiplicative OR/IRR/cumOR -> `or`+type="row"+ci_type="or". No new fmt fields/attributes: `type` gains value "coef", `display` gains token "coef", the `var` field carries var(Y). 12d: MNL = one OR col per outcome category vs ref; ordinal polr + Brant PO diagnostic. 12e: orthogonal `effect="ame"` (marginaleffects) + `at="reference"` profile axis. 12f: model-summary footer + compare= in the `test` attr. 12g / z14-i: SURVEY designs — `wt=` (a flat ids=~1 design), or a prebuilt `survey::svydesign` as `data` for anything richer (clusters / strata / fpc / CALIBRATION); `ids=`/`strata=`/`fpc=`/`nest=` are REMOVED (they reached only the omnibus p) and a svrepdesign/twophase is refused. A design's own weights become `.svy_weights` at the shared boundary, so the crude `Obs_*` columns, the AME, the frozen SD, the gap-test influence weights and the footer are all design-weighted (they silently were not); reduced weighted glance (Wald/Nagelkerke/Cox-Snell/Rao-Scott-AIC) + weighted compare (anova.svyglm Wald); weighted 3+ level (svyolr / svyVGAM); `split_var` (tab_vars analogue, tab_spread-able); `multiplier` (the UNIT a continuous predictor's effect is reported per -- **default `"sd"`** since z9, so `Model_*` on a numeric row is per-1-SD, NOT `exp(coef(glm))`, unless `multiplier = 1`); `empirical_OR` (crude %/OR beside model OR, binary; z9: continuous predictors too, from their univariable fit). No new fmt fields; new Suggests svyVGAM. |


---

## Design Decisions

### Type System

- **`tabxplor_fmt`**: vctrs record (`new_rcrd()`) with **21 per-cell fields** (was 15 before v2.0.0 Phase 1a, 18 through Phase 18s which added **`n_eff`** = the effective sample size used for a cell's CI, `p(1-p)/Var_design` (Korn-Graubard): the closed-form flat-design variance under `options(tabxplor.design_effect=TRUE)` on weighted data, `svyrecvar` under a real design, else NA → the CI falls back to the raw unweighted base; non-displayed, carried like `tot_n`, reset to NA on arithmetic; Phase 18z5 added the 20th, **`obs`** = the value a `tab_reg` cell's estimate is COMPARED TO on its own scale -- the observed/crude effect, or under `split_var` the reference group's -- NA everywhere else, so the measures reading it leave those cells uncoloured; displayable as `{obs}`; Phase 18z8 added the 21st, **`gap_se`** = the standard error of the GAP between the estimate and `obs`, on the estimate's own test scale -- written where the two are independent (`split_var` groups), which is what lets `color_signif` apply to `color = "between_groups"`; NA elsewhere, non-displayed) and **14 per-column attributes** (Phase 10i-A dropped `display_spec` → 9; Phase 15e added `model_family` → 10; Phase 17c added `role` → 11; Phase 18z13 added `conf_level` → 12; Phase 18z16-iiiii added **`degf`** + **`basis`** → 14 = "how was THIS column's interval computed", moved off the table because `meta` proved droppable). The critical distinction: fields vary per cell (accessed via `vctrs::field()`), attributes are scalar describing the whole column (accessed via `attr()`). Constructor chain: `fmt()` (public, validates + coerces) -> `new_fmt()` (internal, calls `vctrs::new_rcrd()`). *(Phase 1a reshaped 15→18 in one combined pass — decisions doc §9; `ci` is now derived from the `ci_inf`/`ci_sup` bounds by `get_ci()`, a bounds-shim.)* The 10th attribute **`model_family`** (Phase 15e; `get/set_model_family`, `""` on cross-tables) is a regression column's own family. The 11th, **`role`** (Phase 17c; internal `get_role`, `"model"`/`"emp"`/`""`), is a reg column's role, read by the colour legend to name each column's effect (OR / IRR / β / AME) without matching its rendered `"Emp."` label. The 13th and 14th, **`degf`** (the design's #PSU-#strata, NA = refer to z) and **`basis`** (`"n"`/`"weights"`/`"design"`/`"design_partial"`), are the twins of `conf_level`: the level an interval was built AT, the df it is referred to, and HOW it was computed. All three are written by ONE sweep per build tail, `tab_stamp_inference()` (was `tab_stamp_conf_level`), and the ptype2 reconcile applies the weakest-claim rule (`basis_rank`/`basis_weakest`, min non-NA `degf`) so a bind cannot over-claim. All are picked up automatically by the DERIVED `fmt_col_attrs` (17a) and carried by every cast/ptype2/vec_math reconstructor.
- **`mean` field is mean-only** (the old overload is GONE — Phase 5 landed): `mean` now carries an actual mean only on `type=="mean"` columns; for **pct-type** columns it is `NA` and the cell/reference **ratio** (the "*2 rule") lives in the dedicated **`ratio` field** (Phase 1a renamed the never-used `rr`→`ratio`). The build writes `mean = NA_reals, ratio = <ref-relative ratio>` for pct columns (`tab.R` ~L3608) and the colour engine reads `get_ratio(x)` (`fmt_class.R` ~L2688). *(c-iii audit 2026-07-19 confirmed no field/attribute consolidation is both safe and worthwhile — the fields are all user-contract and none vestigial; the column attributes — 9 then 10 with Phase 15e's `model_family`, now 11 with Phase 17c's `role` — are exported getters (except the internal `role`) AND required per-column so `format()`/colour work on a standalone extracted column.)*
- **`tabxplor_tab`**: tibble subclass via `tibble::new_tibble()` with **3 top-level table attributes** (Phase 17b merged the six 2.0.0-new attrs into one `meta` list): `subtext` (legend text, CRAN-public), `test` (chi2/ANOVA-F results tibble; §16 hard-rename of the old `chi2` attribute; row-bound → `vec_rbind` on bind; Phase 18j added `effect_size`/`es_type`/`pvalue_exact` columns, Phase 18z16-i `deff` = the design effect this row's test corrected by, and the robust discriminators are `chi2_design`/`F_design` -- TWO, not four, because the flat and the full design run the same estimator; `n` is now ALWAYS the raw count), and **`meta`** — ONE named list holding `render_extras` (Phase 10i-B, the `list(add_n=, add_pct=)` display intent), `ci_settings` (Phase 13b, CI method/confidence level the colour legend names), `vars` (Phase 14d, variable roles + `wt` + the `caption` + Phase 17c's `row_roles` + Phase k's `var_labels` = the haven/labelled variable-label map for the opt-in `tabxplor.var_labels` export name-swap), `empirical_tips` (Phase 14v, multinomial crude-companion tooltips), `reg_meta` (Phase 14w, a reg table's model record driving its title/"Model:" legend/colour wording, + z15's `fit_spec` = the ~4 KB recipe `reg_check_plots()` refits from), `assumptions` (Phase 18z15, the observed curve of each continuous predictor: the sparkline's data + the linearity panel's), and `color_breaks` (Phase 13a per-table break override, now carried so it survives a pipeline). All three are carried through dplyr verbs by the S3 methods + vctrs reconcilers (`tab_attrs()` returns exactly these three; `tab_bind_attrs()` unions `subtext`, `vec_rbind`s `test`, and reconciles `meta` element-wise through the DECLARED `meta_bind_rules` table — default first-non-NULL, `color_breaks` per named scale). Phase 18z16-iiiii DELETED the `inference` sub-field: "how were these numbers computed" is a per-COLUMN fact now (`degf`/`basis`), read back through the DERIVED `tab_inference_basis()`/`tab_inference_degf()`, and its bind rule moved into the fmt ptype2 reconcile where it fires without being called. A table rebuilt from SEVERAL inputs (`tab_compact()`, `tab_transpose()`) goes through **`tab_meta_merge(metas, ...)`** — reduce, then overwrite only what it recomputes — NEVER a fresh `meta = list(...)` literal: that is how z16-iv found `tab_compact()` dropping `inference` on every ≥2-`row_var` table, and how z16-iiiii found **two more** such sites -- `tab_spread()` (which is also what `tab(spread_vars =)` calls) and `reg_build()`'s `split_var` branch, both losing the WHOLE of `meta`. Their numbers are safe now (the inference facts ride the columns), but `vars` / `ci_settings` / `render_extras` still needed the merge. Guarded by a field-AGNOSTIC probe in `test-meta-attr.R`. Every existing getter (`get_vars_attr`/`get_ci_settings`/`get_render_extras`/`get_empirical_tips`/`get_reg_meta`/`get_color_breaks_attr`) is a thin accessor into `meta`; `set_meta_field()` writes one sub-field (NULL removes it; an emptied `meta` drops the attribute → "absent when unset"). New exported `set_caption()`/`get_caption()` store a caption at `meta$vars$caption`, read by every exporter ahead of `reg_title`. `tab_plain()` now records `vars` at build. **Adding/removing a `meta` sub-field is one getter + one line — never a constructor formal.** **Phase k missing-metadata contract:** all three table-level attrs are OPTIONAL and NULL-safe (getters return `NULL`, consumers treat absent as absent) — a table that loses one, or is downgraded to a plain tibble in a pipeline (fmt columns intact), still prints/exports fully coloured, dropping only what that metadata powered (missing `test` → the summary; `subtext` → the note; reg `meta` → title/legend wording), never erroring. Cell FIELDS + column ATTRIBUTES stay required (a standalone extracted `tabxplor_fmt` column formats/colours on its own). The only loss on a *dropped class* is the console auto-print footer (a bare `print()` on a `tbl_df` runs dplyr's printer, not our S3). Locked by `test-degraded-attrs.R`; `tab_degrade_inform` was deliberately left per-render (not throttled once-per-session — conflicts with the `test-edge-cases.R` degrade-message loops).
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
- No `.a.yaml` / `.u.yaml` was touched, so **no `jmvtools::prepare()` is needed**; 19k still owns that.

**FOLLOW-UPS.** 19g (`meta$spec`) can now derive half its `vars` and key the `test` tibble on
`(scope, var, level, col)`; 19h's `tab_shape()` capability predicate replaces the five scattered
aborts (`tab_compact` / `tab_transpose` / `tx_transpose_render`) that this phase left in place; the
column-axis `ordered` when a reader exists (19m or later); the reg `var`-column rendering eyeball
(19n).


---





### Phase 20 — last features before release

#### Phase 20{x} — `tab_reg()` parallelisation

`tab()` has had a parallel row-axis since Phase 8/9a (`R/tab-parallel.R`: `tab_pmap()` + trampoline,
the named `"tabxplor"` mirai pool, `tab_build_one()` as the per-row_var worker, Suggests-only).
`tab_reg()` has nothing, and the work it does is increasingly fit-bound. Research and design it **as a
whole** — pick the level of parallelisation after real measurement, rather than bolting a pool onto
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

---







### Phase 21 — release



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

## The last step of every implementation : Update instructions and relevant development files

After verification passes, always :

1. Ensure the file-header docstring/comment of any modified module is still accurate. Update or add `# DESIGN:` / `# WARNING:` tags next to changed logic.
2. Update `dev/tabxplor_architecture.md` whenever you modify the package structure for real (add modules, rename functions, change config fields). Do not add clutter and useless details. When there is nothing to change, skip it. Update other `dev/*md` file when relevant.
3. **Edit `CLAUDE.md` yourself** — never hand the maintainer "update lines" to paste. Two things go in, both minimalistic, concise, no bullshit, nothing that would clutter the prompt (the details are already in `dev/tabxplor_architecture.md`):
   - the **Repository Map** / *Key Constraints* / *Design Decisions* entries of anything you really changed (a new module, a renamed function, a new config field). When there is nothing to change, skip it.
   - the phase **"DONE" summary**, under its own `#### Phase <x> — <title>` header in the roadmap section. **CLAUDE.md is the ONLY place it goes**: not in `dev/*.md`, not in your chat response (there, give a short readable account, not the summary text). The maintainer moves done phases to `dev/tabxplor_2.0.0_roadmap_DONE_PHASES.md` himself.
4. `NEWS.md`: user-facing and CRAN-facing, tracking new functions, new arguments and arguments changes, deprecations, and important bugs fixes. Keep it minimalistic and no bullshit. Do not edit it when it’s not necessary (most of the time, it’s not necessary).
5. (`README.Rmd` : user manual. Only update before release of new version to CRAN, never before.)
