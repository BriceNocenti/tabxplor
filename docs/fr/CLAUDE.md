# tabxplor — AI Assistant Guide

## Package Purpose

`tabxplor` is a public CRAN R package (v1.3.1) maintained by me for
creating, manipulating, and formatting color-coded cross-tabulations. It
uses `data.table` for fast aggregation internally and exposes a
`tidyverse`-compatible API via custom `tibble` subclasses. Tables can be
exported to Excel (with colors), HTML, Markdown, and ggplot. All numeric
cells are stored as `vctrs` record vectors (`tabxplor_fmt`) carrying
full statistical metadata, enabling lossless display switching.

------------------------------------------------------------------------

## Repository Map

    R/
    ├── fmt_class.R     (~4400 L) Core type: tabxplor_fmt vctrs record, getters/setters, new_fmt() +
    │                              fmt_field_names (the 19 fields; Last Phase s +n_eff) + DERIVED fmt_col_attrs (17a: moved here
    │                              from tab.R, = new_fmt formals minus the fields, so it can't miss an attr);
    │                              format/pillar methods, vctrs arithmetic/casting,
    │                              color engine (fmt_color_plan/fmt_color_slots/fmt_color_channels;
    │                              per-side fold + findInterval; slots 1-4 over / 5-8 under; 17d: fmt_color_plan
    │                              reads MEASURES for raw/scale/sig_source/gate_row -- no switch arms; legacy
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
    │                              terse-in-exports. contrib legend = x N BOTH sides "vs the mean"
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
    │                              + leaf_rename_totals(). tab_apply_reference() = the ONE reference executor
    │                              (tab_num's diff_index_mean twin + inline calculate_refrows copy DELETED).
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
    ├── tab-agg.R        (~500 L) Aggregate-core (Phase 2-3): num_derive_stats/num_rollup, num_moment_scan
    │                              + tab_aggregate_num (numeric tier-1 producer, Phase 7d-i),
    │                              CI engine (ci_pivot/ci_wilson/ci_newcombe/ci_katz_rr/ci_mean_diff2/
    │                              …: 14b's Katz log-RR + 14v-ii's ci_mean_ratio [robust/quasipoisson/
    │                              poisson] are the RATIO-scale intervals ci_type="ratio"; ci_mean_diff2
    │                              gains method welch/student; ci_or = Woolf log-OR for the empirical crude
    │                              OR, used by tab_reg(empirical) AND (14z) tab()'s OR colour via
    │                              tab_apply_reference; RULE B [§48]: numeric CIs are t where a variance is estimated, z
    │                              otherwise -- NOT stars-gated; ci_pivot guards df<=0 -> NA; zscore_formula =
    │                              the normal quantile, 17a: moved here from tab.R), agg_chi2/agg_anova
    │                              (Last Phase j: both also emit the whole-table EFFECT SIZE -- Cramer's V/phi
    │                              from agg_chi2's uncorrected chi2, eta^2 = SSB/SST from agg_anova) +
    │                              agg_fisher (exact test on small weak factor tables, size/N-guarded ->
    │                              simulate fallback)
    ├── survey-design.R  (~200 L) Last Phase j: the shared survey-design constructors (svy_design_formula/
    │                              _vars/_make_design, lifted from tab_reg's reg_* which now delegate) + the
    │                              OPT-IN robust omnibus overlay tab_robust_overlay()/svy_omnibus_one(): run
    │                              in tab_assemble_tables (ctx$data in scope), recompute each (subtable x
    │                              col_var) whole-table p from the microdata under "kish" (first-order
    │                              Rao-Scott n_eff rescale) or "survey" (svychisq / svyglm+regTermTest),
    │                              REPLACE the chi2/F rows' statistic/df/p/n, carry effect_size/min_e through.
    │                              The ONE architectural exception to "test from the aggregate" (opt-in, per-table)
    ├── tab-counts.R     (~360 L) tab_counts() from-the-middle constructor (Phase 4): reshape any
    │                              input shape → count-aggregate → tab_plain(.fine) + shared finalize
    ├── tab-resolve.R    (~200 L) tab_resolve_settings() (Phase 7b): the ONE pure arg-overwrite
    │                              cascade (color="auto"/forcing/split) shared by tab_build+tab_counts;
    │                              resolve_color_auto_num() (numeric arm); emits ci_scale (14b: "ratio"
    │                              = the Katz interval). The jmvtab .js / cache boundary.
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
    │                              Phase 13c-iv tabxplor_tabs (multi-table LIST class: print/[/c/knit_print,
    │                              auto-print + Viewer routing); 17g: tab_materialize_extras -> tab_materialize()
    │                              over materialize_specs() (DECLARED list(kind,when,apply): add_n_pct/or_total/
    │                              sd_twin/footer/collapse_totals; mat_add_n_pct/mat_sd_twin applies; add_n `n`
    │                              COLUMN built xl-ONLY, text folds direct -- no throwaway; collapse = display slice)
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
    ├── tab-css.R        (~230 L) Phase 13d: THE one CSS generator, shared by tab_md + tab_kable("html").
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
    │                              tooltip via reg_empirical_tips -> `empirical_tips` table attr; per-spec
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
    ├── tab_reg_plots.R  (~230 L) Phase 12h display: or_plot() (finalfit-style OR forest plot ON a
    │                              tabxplor_tab -- reads fmt fields, NO refit; gridExtra 2-panel) + lm_plots()
    │                              (ggplot2 2x2 glm/lm diagnostics). ggplot2+gridExtra guarded (Suggests).
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

The
[`tab_reg()`](https://bricenocenti.github.io/tabxplor/fr/reference/tab_reg.md)
fit-cache seam (Phase 15b): an internal `.fit_cache` arg threads a cache
env into `reg_build`. On the single-equation GLM coefficient path (wald
/ value-ci / no split-mult-trials-compound -ame-mnl), the model is fit
ONCE at the canonical reference and distilled into a KB-sized digest
(`reg_build_digest`: coef + vcov + reference-invariant glance); any
factor-predictor reference is then recomputed live via coefficient
contrasts (`reg_reref_fit_res`, sharing `reg_wald_crit` with `reg_fit`)
— NO refit, byte-identical to a real refit (`test-jmvtabreg-cache.R`).
Heavy paths cache the raw fit (refit on a reference change).
`.fit_cache = NULL` (every ordinary call) is byte-unchanged.

**Other directories:**

| Directory | Purpose |
|----|----|
| `vignettes/` | User intro (`tabxplor.Rmd`) |
| `tests/testthat/` | testthat v3 tests |
| `man/` | Auto-generated by roxygen2 (never edit by hand) |
| `inst/i18n/` | Internationalization resources |
| `jamovi/` | Jamovi module definition files |
| `po/` | Translation files |
| `dev/` | architecture guide + dev scripts + perf harness + `color_palette_tools.R`, `.Rbuildignore`’d |

**Cross-cutting dependencies** (be careful when modifying):

- `fmt_class.R` — used by everything; the `tabxplor_fmt` class is the
  foundation
- [`tab_get_vars()`](https://bricenocenti.github.io/tabxplor/fr/reference/tab_many.md)
  in `tab.R` — used by all export functions (tab_xl, tab_kable, tab_md,
  tab_plot)
- [`get_color_style()`](https://bricenocenti.github.io/tabxplor/fr/reference/tab_many.md)/[`set_color_breaks()`](https://bricenocenti.github.io/tabxplor/fr/reference/tab_many.md)
  in `tab_classes.R` — shared between `fmt_class.R` (console) and
  `tab_xl.R` (Excel)
- `.onLoad()` in `utils.R` — sets all default options; changing defaults
  affects every user

------------------------------------------------------------------------

## Global Architecture

    tab() [user-friendly wrapper]
      └── tab_many() [full-featured, vectorised over row_vars and col_vars]
            └── per row_var:
                  tab_prepare()  →  tab_plain() / tab_num()  →  tab_pct()
                    →  tab_ci()  →  tab_chi2()  →  tab_totaltab()
                          →  tab_spread() / tab_compact()

    Export:  tab_xl()  |  tab_kable()  |  tab_md()  |  tab_plot()

> **This is the *current* pipeline. 2.0.0 rewrites it around a single
> aggregate-core** (see roadmap § Keystone +
> `dev/tabxplor_2.0.0_decisions.md`): the step chain
> `tab_pct → tab_ci → tab_chi2 → …` collapses into one core, and
> `tab_pct`/`tab_tot`/`tab_ci`/`tab_chi2` become superseded thin
> wrappers.

**Ordering invariant** (in
[`tab_many()`](https://bricenocenti.github.io/tabxplor/fr/reference/tab_many.md),
`tab.R` ~L1146):
[`tab_chi2()`](https://bricenocenti.github.io/tabxplor/fr/reference/tab_chi2.md)
and
[`tab_ci()`](https://bricenocenti.github.io/tabxplor/fr/reference/tab_ci.md)
are independent (either order), but non-first levels (`levels="first"`)
must be dropped **after both**, so chi2/ci are computed on the full set
of levels. Do not move the level-drop above chi2/ci.

### Key Constraints

| Constraint | Detail |
|----|----|
| CRAN stability | Public function arguments must NOT change without deprecation. Internals can change freely. |
| vctrs record contract | Adding a field to `tabxplor_fmt` requires updating [`new_fmt()`](https://bricenocenti.github.io/tabxplor/fr/reference/new_fmt.md), [`fmt()`](https://bricenocenti.github.io/tabxplor/fr/reference/fmt.md), [`format.tabxplor_fmt()`](https://bricenocenti.github.io/tabxplor/fr/reference/format.tabxplor_fmt.md), [`pillar_shaft.tabxplor_fmt()`](https://bricenocenti.github.io/tabxplor/fr/reference/pillar_shaft.tabxplor_fmt.md), `vec_arith` methods, and possibly [`tab_pct()`](https://bricenocenti.github.io/tabxplor/fr/reference/tab_pct.md)/[`tab_ci()`](https://bricenocenti.github.io/tabxplor/fr/reference/tab_ci.md)/[`tab_chi2()`](https://bricenocenti.github.io/tabxplor/fr/reference/tab_chi2.md). ~8 functions across 3 files. |
| NAMESPACE | Auto-generated by roxygen2. Never edit `NAMESPACE` by hand. Run [`devtools::document()`](https://devtools.r-lib.org/reference/document.html) after changing `@export`/`@import`. |
| data.table internals | [`tab_plain()`](https://bricenocenti.github.io/tabxplor/fr/reference/tab_plain.md)/[`tab_num()`](https://bricenocenti.github.io/tabxplor/fr/reference/tab_num.md) rename `col_var` to internal names to avoid data.table conflicts. The user’s column names are restored afterward. |
| dplyr class preservation | 30+ S3 methods on `tabxplor_tab`/`tabxplor_grouped_tab` ensure class + attributes survive all dplyr verbs. Missing a method = silent class downgrade to `tbl_df`. |
| Options as config | All defaults set in `.onLoad()` in `utils.R`. Users override via [`options()`](https://rdrr.io/r/base/options.html). Functions read with [`getOption()`](https://rdrr.io/r/base/options.html). |
| Suggests-only guards | `openxlsx2`, `ggplot2`, `jmvcore`, `ggpubr`, `cowplot`, `mirai`, `kableExtra` are in Suggests. Every call must be guarded with [`requireNamespace()`](https://rdrr.io/r/base/ns-load.html) or equivalent (tab_xl’s ONE guard is in [`tab_xl()`](https://bricenocenti.github.io/tabxplor/fr/reference/tab_xl.md); `R/tab-xl-backend.R` wrappers are unguarded; `kableExtra`’s two entry points — `render_kable_html()` engine dispatch + [`kable_tabxplor_style()`](https://bricenocenti.github.io/tabxplor/fr/reference/kable_tabxplor_style.md) — are guarded, the default `html` engine never touches it). |
| Color break mirroring | [`set_color_breaks()`](https://bricenocenti.github.io/tabxplor/fr/reference/tab_many.md) takes positive-only thresholds. Negative breaks are auto-mirrored internally. Any `pct_breaks` value \> 1 triggers ratio comparison instead of difference (the “\*2 rule”). |
| Mean-diff asymmetry | For `type="mean"` columns, the `diff` field stores a **ratio** (cell_mean / ref_mean), NOT a difference. Thresholds like 1.15 mean “+15% above reference”. This asymmetry propagates into `color_formula()` and [`format.tabxplor_fmt()`](https://bricenocenti.github.io/tabxplor/fr/reference/format.tabxplor_fmt.md). **(2.0.0 §3: numeric `diff` becomes a real difference; the ratio moves to the `ratio` field — the never-used `rr` field renamed, placed after `diff`.)** |
| tab_reg | Phase 12c–12g LIVE: unified regression tables (gaussian beta / binomial OR / poisson IRR / multinomial OR / ordinal cumulative OR) over lm/glm/svyglm/svyolr/svy_vglm/nnet::multinom/MASS::polr + broom (no parsnip). tab_logit/multi_logit are binomial wrappers. Effect shape is exponentiate-driven: additive beta -\> `diff`+type=“coef”+display=“coef”+ci_type=“diff”; multiplicative OR/IRR/cumOR -\> `or`+type=“row”+ci_type=“or”. No new fmt fields/attributes: `type` gains value “coef”, `display` gains token “coef”, the `var` field carries var(Y). 12d: MNL = one OR col per outcome category vs ref; ordinal polr + Brant PO diagnostic. 12e: orthogonal `effect="ame"` (marginaleffects) + `at="reference"` profile axis. 12f: model-summary footer + compare= in the `test` attr. 12g: SURVEY designs — `wt=`/`ids=`/`strata=`/`fpc=`/`nest=` + a prebuilt survey.design/svyrep.design as `data`; reduced weighted glance (Wald/Nagelkerke/Cox-Snell/Rao-Scott-AIC) + weighted compare (anova.svyglm Wald); weighted 3+ level (svyolr / svyVGAM); `split_var` (tab_vars analogue, tab_spread-able); `multiplier` (OR^k); `empirical_OR` (crude %/OR beside model OR, binary). No new fmt fields; new Suggests svyVGAM. |

------------------------------------------------------------------------

## Design Decisions

### Type System

- **`tabxplor_fmt`**: vctrs record (`new_rcrd()`) with **19 per-cell
  fields** (was 15 before v2.0.0 Phase 1a, 18 through Last Phase s which
  added **`n_eff`** = the effective sample size used for a cell’s CI:
  Kish `n_eff` when `options(tabxplor.kish_neff=TRUE)` on weighted data,
  else NA → the CI falls back to the raw unweighted base; non-displayed,
  carried like `tot_n`, reset to NA on arithmetic) and **11 per-column
  attributes** (Phase 10i-A dropped `display_spec` → 9; Phase 15e added
  `model_family` → 10; Phase 17c added `role` → 11). The critical
  distinction: fields vary per cell (accessed via
  [`vctrs::field()`](https://vctrs.r-lib.org/reference/fields.html)),
  attributes are scalar describing the whole column (accessed via
  [`attr()`](https://rdrr.io/r/base/attr.html)). Constructor chain:
  [`fmt()`](https://bricenocenti.github.io/tabxplor/fr/reference/fmt.md)
  (public, validates + coerces) -\>
  [`new_fmt()`](https://bricenocenti.github.io/tabxplor/fr/reference/new_fmt.md)
  (internal, calls
  [`vctrs::new_rcrd()`](https://vctrs.r-lib.org/reference/new_rcrd.html)).
  *(Phase 1a reshaped 15→18 in one combined pass — decisions doc §9;
  `ci` is now derived from the `ci_inf`/`ci_sup` bounds by `get_ci()`, a
  bounds-shim.)* The 10th attribute **`model_family`** (Phase 15e;
  `get/set_model_family`, `""` on cross-tables) is a regression column’s
  own family. The 11th, **`role`** (Phase 17c; internal `get_role`,
  `"model"`/`"emp"`/`""`), is a reg column’s role, read by the colour
  legend to name each column’s effect (OR / IRR / β / AME) without
  matching its rendered `"Emp."` label. Both are picked up automatically
  by the DERIVED `fmt_col_attrs` (17a) and carried by every
  cast/ptype2/vec_math reconstructor.
- **`mean` field is mean-only** (the old overload is GONE — Phase 5
  landed): `mean` now carries an actual mean only on `type=="mean"`
  columns; for **pct-type** columns it is `NA` and the cell/reference
  **ratio** (the “*2 rule”) lives in the dedicated **`ratio` field**
  (Phase 1a renamed the never-used `rr`→`ratio`). The build writes
  `mean = NA_reals, ratio = <ref-relative ratio>` for pct columns
  (`tab.R` ~L3608) and the colour engine reads `get_ratio(x)`
  (`fmt_class.R` ~L2688).* (c-iii audit 2026-07-19 confirmed no
  field/attribute consolidation is both safe and worthwhile — the fields
  are all user-contract and none vestigial; the column attributes — 9
  then 10 with Phase 15e’s `model_family`, now 11 with Phase 17c’s
  `role` — are exported getters (except the internal `role`) AND
  required per-column so
  [`format()`](https://rdrr.io/r/base/format.html)/colour work on a
  standalone extracted column.)\*
- **`tabxplor_tab`**: tibble subclass via
  [`tibble::new_tibble()`](https://tibble.tidyverse.org/reference/new_tibble.html)
  with **3 top-level table attributes** (Phase 17b merged the six
  2.0.0-new attrs into one `meta` list): `subtext` (legend text,
  CRAN-public), `test` (chi2/ANOVA-F results tibble; §16 hard-rename of
  the old `chi2` attribute; row-bound → `vec_rbind` on bind; Last Phase
  j added `effect_size`/`es_type`/`pvalue_exact` columns + the
  `chi2_kish`/`chi2_svy`/`F_kish`/`F_svy` robust discriminators), and
  **`meta`** — ONE named list holding `render_extras` (Phase 10i-B, the
  `list(add_n=, add_pct=)` display intent), `ci_settings` (Phase 13b, CI
  method/confidence level the colour legend names), `vars` (Phase 14d,
  variable roles + `wt` + the `caption` + Phase 17c’s `row_roles` +
  Phase k’s `var_labels` = the haven/labelled variable-label map for the
  opt-in `tabxplor.var_labels` export name-swap), `empirical_tips`
  (Phase 14v, multinomial crude-companion tooltips), `reg_meta` (Phase
  14w, a reg table’s model record driving its title/“Model:”
  legend/colour wording), and `color_breaks` (Phase 13a per-table break
  override, now carried so it survives a pipeline). All three are
  carried through dplyr verbs by the S3 methods + vctrs reconcilers
  (`tab_attrs()` returns exactly these three; `tab_bind_attrs()` unions
  `subtext`, `vec_rbind`s `test`, and reconciles `meta` element-wise —
  `color_breaks` per named scale). Every existing getter
  (`get_vars_attr`/`get_ci_settings`/`get_render_extras`/`get_empirical_tips`/`get_reg_meta`/`get_color_breaks_attr`)
  is a thin accessor into `meta`; `set_meta_field()` writes one
  sub-field (NULL removes it; an emptied `meta` drops the attribute →
  “absent when unset”). New exported
  [`set_caption()`](https://bricenocenti.github.io/tabxplor/fr/reference/set_caption.md)/[`get_caption()`](https://bricenocenti.github.io/tabxplor/fr/reference/set_caption.md)
  store a caption at `meta$vars$caption`, read by every exporter ahead
  of `reg_title`.
  [`tab_plain()`](https://bricenocenti.github.io/tabxplor/fr/reference/tab_plain.md)
  now records `vars` at build. **Adding/removing a `meta` sub-field is
  one getter + one line — never a constructor formal.** **Phase k
  missing-metadata contract:** all three table-level attrs are OPTIONAL
  and NULL-safe (getters return `NULL`, consumers treat absent as
  absent) — a table that loses one, or is downgraded to a plain tibble
  in a pipeline (fmt columns intact), still prints/exports fully
  coloured, dropping only what that metadata powered (missing `test` →
  the summary; `subtext` → the note; reg `meta` → title/legend wording),
  never erroring. Cell FIELDS + column ATTRIBUTES stay required (a
  standalone extracted `tabxplor_fmt` column formats/colours on its
  own). The only loss on a *dropped class* is the console auto-print
  footer (a bare [`print()`](https://rdrr.io/r/base/print.html) on a
  `tbl_df` runs dplyr’s printer, not our S3). Locked by
  `test-degraded-attrs.R`; `tab_degrade_inform` was deliberately left
  per-render (not throttled once-per-session — conflicts with the
  `test-edge-cases.R` degrade-message loops).
- **`tabxplor_grouped_tab`**: extends `grouped_df` for subtabled results
  (when `tab_vars` are present). Requires separate S3 method for every
  dplyr verb.

### Export Parity

Cell display values reach exporters by two **non-unified** paths — keep
them in sync:

- **[`format.tabxplor_fmt()`](https://bricenocenti.github.io/tabxplor/fr/reference/format.tabxplor_fmt.md)**
  (`fmt_class.R`) is the single source of truth for markdown
  ([`tab_md()`](https://bricenocenti.github.io/tabxplor/fr/reference/tab_md.md)),
  knitr/HTML
  ([`tab_kable()`](https://bricenocenti.github.io/tabxplor/fr/reference/tab_html.md)),
  and the console (`pillar_shaft`).
- **[`tab_xl()`](https://bricenocenti.github.io/tabxplor/fr/reference/tab_xl.md)**
  (Excel) writes the raw
  [`get_num()`](https://bricenocenti.github.io/tabxplor/fr/reference/fmt.md)
  value and delegates numeric formatting to Excel’s engine, but it now
  sources the per-cell Excel number-format codes from
  `format(x, syntax = "excel")` (Phase 10g) — the SAME
  [`format()`](https://rdrr.io/r/base/format.html) masks the text
  backends use — so a display/digits change no longer needs manual
  mirroring in `tab_xl.R` (the old `numfmt()` desync is gone). Colours
  come from `fmt_color_channels()`; roles/refs/bold from
  `tab_export_prep()`.
- Color is safe: all exporters call the same `fmt_color_channels()` /
  `fmt_channel_codes()`.

When adding or changing a `tabxplor_fmt` field, follow the
`/vctrs-field` skill — it encodes the full ~11-step checklist across
`fmt_class.R`, `tab.R`, and the exporters.

### Reference System

The `ref` argument controls which row serves as the comparison baseline
for differences/colors: - `"auto"`: defaults to `"first"` when OR
requested, `"tot"` otherwise - `"tot"`: total row is the reference -
`"first"`: first non-total row - integer: specific row index - regex
string: matched against row labels - `comp="tab"` compares within each
subtable; `comp="all"` compares against the total table

Note: `ref` is **reinterpreted by `pct`** — a reference **row** under
`pct="row"`/means, a reference **column** under `pct="col"`. 2.0.0 makes
`ref` a per-row_var named vector (row%/means only) and stores each
cell’s own base as `tot_n` — see decisions doc §2, §4.

### Color System (3-layer)

1.  **Palettes** (`tab_classes.R` ~L2892): 6 named color vectors
    (dark/light text, 24-bit blue-red/green-red, dark/light background),
    each with 11 hex codes: `pos1`-`pos5` (over-represented),
    `neg1`-`neg5` (under-represented), `ratio`. Hues are hand-tuned so
    intensity levels are eye-distinguishable on real tables; 8-bit
    variants target non-truecolor terminals; the 24-bit blue-red variant
    is more colorblind-friendly than green-red (fuller colorblind
    support is a future goal).
2.  **Breaks**
    ([`set_color_breaks()`](https://bricenocenti.github.io/tabxplor/fr/reference/tab_many.md)
    in `tab_classes.R`): stored in `options("tabxplor.color_breaks")`.
    Default pct: `c(0.05, 0.1, 0.2, 2, 0.3)` — the `2` means “twice the
    reference” (ratio mode). Mirrored for negative. Mean breaks:
    `c(1.15, 1.5, 2, 4)` — always ratios. *(2.0.0 §18 adds
    `mean_diff_breaks` `c(0.2, 0.5, 0.8, 1.2)` — sd-standardized
    differences for the numeric diff mode, Phase 5.)*
3.  **Selection** (the Phase-5 `findInterval` engine in `fmt_class.R`:
    `fmt_color_plan` → `fmt_color_slots` →
    `fmt_color_channels`/`fmt_channel_codes`, the shared artifact every
    backend consumes; the old `fmt_color_selection`/`keep_last_break`
    are gone): per-side fold + `findInterval` over the break scale picks
    the strongest matching threshold per cell. The 4 measures
    (`diff`/`ratio`/`or`/`contrib`) each carry their engine facts (raw
    getter, scale keys, `sig_source`, `gate_row`) in the ONE `MEASURES`
    fact table (Phase 17d — it now drives BOTH `fmt_color_plan` and the
    legend; the per-measure switch arms are gone, only the diff↔︎ratio
    bound rescale + guaranteed-effect offset stay as policy code). The
    legacy combined strings (`diff_ci`/`after_ci`/`ci`) are decoded ONCE
    at the boundary (`color_decode_legacy`) into a clean
    `(measure, color_signif)` pair — the stored `color` attribute is
    always a clean measure and the engine never re-parses;
    `color_measure_policy`/`single0` are deleted (`"ci"` == `after_ci`
    now).

### dplyr Integration

The
[`dplyr_row_slice()`](https://dplyr.tidyverse.org/reference/dplyr_extending.html)
/
[`dplyr_col_modify()`](https://dplyr.tidyverse.org/reference/dplyr_extending.html)
/
[`dplyr_reconstruct()`](https://dplyr.tidyverse.org/reference/dplyr_extending.html)
trio in `tab_classes.R` is the core mechanism preserving
`tabxplor_grouped_tab` class through dplyr operations. When the table
has only one grouping level left, `lv1_group_vars()` detects this and
downgrades to plain `tabxplor_tab`. Every new dplyr verb needs a
corresponding S3 method — check `NAMESPACE` for the full list.

### Deprecation and retro-compatibility

#### For main user-facing functions and arguments

- This package have a small but existing users base : **soft deprecate
  main user-facing functions and arguments carefully** to ensure
  retro-compatibility.
- Some user code rely on `tabxplor_fmt` vctrs fields extracted with `$`
  or calculated with
  [`mutate()`](https://dplyr.tidyverse.org/reference/mutate.html) method
  for `tabxplor_fmt` (see readme), so **the vctrs fields should not
  break**.

#### For internal code and internal functions

- **Do not hesitate to propose radical redesign of internal code and
  internal workflows** for quality, simplicity, structure, performance
  and future-proofing, specially when they are too convoluted or have
  grown organically.
- **Always try to simplify, integrate and create smart shared
  subfunctions** instead of adding a new layer of confusion and ad-hoc
  solutions inside the code: your main aim is to simplify, to remove
  traces of old implementations altogether when they have become
  useless, to clarify, to help me make relevant architectural choices
  instead of piling up ad-hoc solutions, to integrate the new features
  in the current code seamlessly.

------------------------------------------------------------------------

## Key Dependency APIs to read up on

Before working on the `tabxplor_fmt` type system, arithmetic, or
display, fetch the help pages for these via the `r-btw` MCP **docs**
tools (or `?`) — the model’s recall of their exact current contracts is
the weakest link:

- [`vctrs::new_rcrd`](https://vctrs.r-lib.org/reference/new_rcrd.html),
  [`vctrs::field`](https://vctrs.r-lib.org/reference/fields.html) —
  record type and per-cell field access
- [`vctrs::vec_arith`](https://vctrs.r-lib.org/reference/vec_arith.html),
  [`vctrs::vec_cast`](https://vctrs.r-lib.org/reference/vec_cast.html),
  [`vctrs::vec_ptype2`](https://vctrs.r-lib.org/reference/vec_ptype2.html)
  — arithmetic and casting S3 contracts
- [`pillar::pillar_shaft`](https://pillar.r-lib.org/reference/pillar_shaft.html)
  — console display method
- `data.table` reference semantics (`:=`, `.SD`, `.N`) — internal
  aggregation
- [`DescTools::BinomCI`](https://andrisignorell.github.io/DescTools/reference/BinomCI.html),
  [`DescTools::BinomDiffCI`](https://andrisignorell.github.io/DescTools/reference/BinomDiffCI.html)
  — **now Suggests-only** (test parity only). Since Phase 3a the CI math
  is the closed-form engine in `R/tab-agg.R`
  (`ci_pivot`/`ci_wilson`/`ci_newcombe`); read it, not DescTools, before
  touching CI.

------------------------------------------------------------------------

## Testing

### How to run the suite (the ONLY sanctioned recipe — 2026-07-16)

``` bash
# In a temp .R file (outside tests/), then run it EXACTLY like this, unsandboxed:
#   OMP_NUM_THREADS=1 Rscript that_file.R
# The .R file:  Sys.setenv(TESTTHAT_CPUS = "8", NOT_CRAN = "true"); devtools::test("~/github/tabxplor")
```

⚠ **`OMP_NUM_THREADS=1` is NOT optional, and `TESTTHAT_CPUS=8` alone is
a trap.** Root-caused 2026-07-16 (second session lost to it).
`Config/testthat/parallel: true` runs each test file in its own PROCESS,
and **each process then multi-threads on its own**:

| thread source | per worker | x 8 workers | lever |
|----|----|----|----|
| data.table (defaults to 50 % of cores) | 6 | 48 | `setDTthreads(1L)` — now in `tests/testthat/setup.R` |
| OpenBLAS *pthread* build (`lm`/`glm`/ggplot) | ~10 | ~80 | `OMP_NUM_THREADS=1` **in the env before R starts** |

**Measured: 165 threads on 12 cores (~14x oversubscribed) -\> the suite
ran \>26 min instead of ~50 s**, two workers pegged at ~485 % CPU while
the rest starved and the log went silent for 10 min. With both levers:
**47 threads, 48.9 s, FAIL 0.** OpenBLAS fixes its thread count at
**library init**, so `setup.R` is too late for it — it MUST be an env
var on the `Rscript` command (workers inherit it).

**Never run anything else while the suite runs.** A single `Rscript`
repro uses ~4 cores here; racing it against 8 workers is what turns
“slow” into “apparently hung”. Iterate with `filter =`, and run the full
suite once, alone.

⛔ **Before blaming the code for a slow run, check whether YOU are the
cause** — this is the companion to the orphan rule below. In order: (1)
is another R of mine running? (2)
`ps -eLo pid,args | grep -c "[-]-no-readline --slave"` — is the THREAD
count \>\> 12? (3) only then look for orphans. A worker at 485 % CPU is
oversubscription, not a hang.

``` r
# One/few files while iterating (cheap, safe to repeat):
devtools::test("~/github/tabxplor", filter = "tab")  # regex on test-<name>.R
```

⚠ **Two test/tooling steps need `dangerouslyDisableSandbox` here —
root-caused 2026-07-16 from the bwrap command line, do not
re-diagnose:**

- **`test-parallel-parity.R` fails sandboxed** (`fail=1 err=7`, ~0.7 s)
  with `nanonext::.dispatcher_start: 16 | Permission denied`. Cause:
  bwrap runs **`--unshare-net`**, and mirai’s dispatcher needs sockets.
  **Not a regression** — it passes 11/11 unsandboxed. Any full-suite run
  inside the sandbox reports these 8 as failures; ignore them or run
  that file unsandboxed.
- **[`devtools::document()`](https://devtools.r-lib.org/reference/document.html)
  fails sandboxed** with *“cannot open file ‘NAMESPACE’: Read-only file
  system”*. Cause: bwrap `--ro-bind`s `NAMESPACE` and `man/`
  specifically (the rest of the repo is writable, which is why snapshot
  writes succeed). Run it unsandboxed.

⚠ Dev now runs **inside WSL2 Ubuntu 26.04** (`~/github/tabxplor` on
ext4), not Windows. The old `d:/Statistiques/github/tabxplor` paths are
dead — the Windows checkout survives **build-only** for Windows `.jmo`
(see *Jamovi module development*). The `~46s` / `225s -> 56s` suite
timings recorded here were measured on Windows/NTFS and have **not**
been re-measured on ext4 — treat them as order-of-magnitude only.

**Measured on ext4 / WSL2, 2026-07-16 (per-file, serial): total `359 s`,
2357 passing; slowest `test-tab_reg.R` `33.6 s`, then `counts-parity` /
`calculations` / `color-legend` ~23-25 s, most files 1-13 s.** Under
`Config/testthat/parallel: true` the wall clock is roughly the SLOWEST
FILE, so the recorded `56 s` is consistent and still right. **A
multi-minute run means something else is wrong — look for orphans
(below) before blaming the code.** Pass `TESTTHAT_CPUS=8`:
`parallel: true` alone picks only ~2 processes here.

⛔ **NEVER kill a test run by killing its parent — you orphan the
workers, and they do NOT stop.** Measured 2026-07-16: two `TaskStop`’d
suites left 6 R processes (2 `--file=…` parents + 4
`--no-readline --slave` testthat/mirai workers) alive for **52 minutes
at ~860 % CPU** (one had burned 174 min of CPU time). They silently
starve every later run — a suite that “takes 15 minutes” is usually
this, not the code. Symptoms + rules:

- **Diagnose AND kill unsandboxed — bwrap runs
  `--unshare-pid --proc /proc`**, so each Bash tool call gets its OWN
  PID namespace (`ps` shows the shell as PID 1). Two consequences:
  `ps aux` **cannot see the orphans**, and a *sandboxed*
  `kill <host-pid>` cannot kill them — worse, a low PID like `34`
  usually DOES exist inside the namespace, so it would kill **the wrong
  process**. Both `ps` and `kill` must run unsandboxed. Identify yours
  by the parent’s `--file=/tmp/claude-…/<session-id>/scratchpad/…` —
  never by name alone (Positron runs its own R, and killing that is
  destructive).
- **Never `pkill -f <pattern>`.** Measured: `pkill -f testthat` matched
  and killed the calling shell, and `pkill -f t9.R` is what orphaned the
  workers (parent SIGKILLed -\> exit 137, children reparented and kept
  running). Read `ps` first, then `kill` explicit PIDs.
- **Prefer not to create them**: run the suite in the foreground with a
  long timeout, or `filter =` to the files you touched.
  `setsid nohup … &` is ALSO killed when the tool’s shell exits.
- **Never pipe a long run through `tail`/`head`** — they buffer until
  EOF, so the output file stays empty and the run looks hung. Write the
  incremental log to a file and read that.
- ⚠ Killing PIDs needs the maintainer: the auto-mode classifier denies
  it (rightly — this is a shared dev box). Surface the `ps` evidence and
  hand over the exact `kill -9 <pids>`.

**Test files:**

| File | Coverage |
|----|----|
| `test-fmt_class.R` | fmt creation, printing, type conversion, c(), arithmetic |
| `test-tab.R` | Core: plain tables, pct, totals, NA, CI, chi2, references, wrapping |
| `test-tab_classes.R` | Class preservation through dplyr verbs |
| `test-tab_xl.R` | Basic Excel export |
| `test-tab_logit.R` | Phase 12a: binomial-wrapper OR/CI/p parity vs glm/svyglm, 1/OR |
| `test-tab_reg.R` | Phase 12c/12d/12e: beta/OR/IRR/MNL/ordinal + AME parity vs lm/glm/multinom/polr/marginaleffects |
| `test-tab_reg-display.R` | Phase 12h: estimate_display (est_ci bracket / prob / ame folds), Excel test label, split footer |
| `test-tab_reg-plots.R` | Phase 12h: or_plot() / lm_plots() smoke tests (build a gtable without error) |

------------------------------------------------------------------------

## Jamovi module development

tabxplor currently use jamovi `2.6.44.0` (solid). Version 2.0.0 will
also be tested on jamovi current “solid” version `2.7.37` afterwards
(Phase 7i confirmed 2.7.37 ✓).

✅ **jamovi IS installed in this WSL2 distro (migration Phase C3,
2026-07-16): flatpak `org.jamovi.jamovi` 2.7.36, bundled R 4.5.0.**
Launch it with **`jamovi`** (the `~/.local/bin/jamovi` wrapper — never
bare `flatpak run`, see below). The module builds with
`jmvtools::install(home = "flatpak")` in ~2 min, and Crosstables is
verified running on real data.

✅ **The six “OPEN — maintainer step: regenerate `jmvtab.h.R`” items
(Phases 7a, 7e, 7g-i, 7g-ii, 7g-iii, 7h) are CLOSED** — one
[`jmvtools::prepare()`](https://rdrr.io/pkg/jmvtools/man/prepare.html)
covered all of them, and the compiled **`uijs` blob** means those UI
changes are live in a running app for the first time.

⚠ **`prepare()` proved the hand-edited `.h.R` had a latent bug**, so do
not hand-edit it again. `R/jmvtab.h.R` was hand-mirrored to the YAML
across ~7 commits; the compiler reproduced 778 of its 780 lines but
corrected `exportExcel` (`type: Action`) from `NULL` → `FALSE` **and
gave it a default it lacked** — without which
[`tabxplor::jmvtab()`](https://bricenocenti.github.io/tabxplor/fr/reference/jmvtab.md)
called from R throws. The never-edit rule earned its keep.

⚠⚠ **`ELECTRON_RUN_AS_NODE` — do not debug jamovi without knowing
this.** Claude Code/Positron export `ELECTRON_RUN_AS_NODE=1`; flatpak
passes it into the sandbox and jamovi’s Electron runs as **plain node**
→ **exit 0, no window, no error**, and
[`jmvtools::install()`](https://rdrr.io/pkg/jmvtools/man/install.html)
dies `"bad option: --install"` (rc=9). `flatpak run --unset-env=` is NOT
enough (zypak re-spawns children via the host); only `env -u` on the
host works — which is what the `jamovi` wrapper does. In R:
`Sys.unsetenv("ELECTRON_RUN_AS_NODE")` before
[`jmvtools::install()`](https://rdrr.io/pkg/jmvtools/man/install.html).
⚠ [`jmvtools::check()`](https://rdrr.io/pkg/jmvtools/man/check.html)
passes regardless — it never reaches Electron — so a green `check()`
proves nothing here.

⚠ **WSLg is in COPY MODE** (known WSL 2.7.x bug
[microsoft/WSL#40618](https://github.com/microsoft/WSL/issues/40618)):
windows can be slow or render blank (taskbar entry + penguin icon,
`[WARN:COPY MODE]` in the title). **Not a jamovi problem** — plain
`xmessage` fails identically. One-time fix, persists across reboots:
`sudo mkdir -p /mnt/shared_memory && sudo mount -t tmpfs tmpfs /mnt/shared_memory`.
⚠ The bug is *unstable* — it sometimes renders fine without the mount,
then regresses; a working window is not evidence the mount is unneeded.

⚠ **There are now TWO build paths, and they are not interchangeable —
`.jmo` bundles are platform-specific** (migration Phase A1):

| Target | jamovi | Checkout | Recipe |
|----|----|----|----|
| **Linux `.jmo`** (WSL, the dev path) | flatpak `org.jamovi.jamovi` **2.7.36 ✅ installed (C3)** | `~/github/tabxplor` — **authoritative for source** | `jmvtools::install(home = 'flatpak')` (setup doc §7.4; the SDK `org.freedesktop.Sdk//24.08` is REQUIRED — `flatpak run --devel` is how the compiler reaches R) |
| **Windows `.jmo`** (release only) | Windows jamovi, **kept forever** | `D:\Statistiques\github\tabxplor` — **build-only: pull, build, never edit** | `options(jamovi_home='C:/Program Files/jamovi 2.6.44.0'); devtools::load_all(); jmvtools::install(); devtools::load_all()` |

**A Linux jamovi cannot produce a Windows bundle**, so the Windows
checkout survives *even if C3 fully succeeds* — this is not a C3-failure
fallback. The rule that matters: **never edit tabxplor in both places.**
Edit in WSL, pull on Windows, build there.

✅ **`jmvtools` is pinned to 2.7.26** (C3). ⚠ Never
`install.packages("jmvtools", repos="https://repo.jamovi.org")` — that
index serves 2.7.26 **and** 28.0-28.3, so R takes **28.3**, whose newer
compiler can emit a `jms` version 2.7.36 refuses. Reinstall with the
explicit tarball:
`install.packages("https://repo.jamovi.org/src/contrib/jmvtools_2.7.26.tar.gz", repos = NULL, type = "source")`
(install `node` from that repo first — `repos = NULL` resolves no deps).

⛔ **The 2.6.44 flatpak is GONE** (C3): Flathub retains only 4 commits,
back to 2.7.29 (2026-05-12); 2.6.44 was built 2025-03-06 and is pruned.
**2.6-solid compatibility is verified on Windows only** — via the
build-only Windows checkout, which is kept forever regardless.

To know the real structure of the final .html and .js, check at this
live capture done from dev console (for a basic table) : -
`dev/jamovi/dev_console_live_capture/Jamovi_tabxplor_1_3_1_basic_table.html`
: the live html from tabxplor 1.3.1 jamovi module -
`dev/jamovi/dev_console_live_capture/127.0.0.1_56680_MAIN_ELECTRON/` :
the exported main election scripts -
`dev/jamovi/dev_console_live_capture/127.0.0.1_56683_tabxplor_jmvtab_analysis_UI/`
: the exported tabxplor jmvtab analysis UI scripts -
`dev/jamovi/dev_console_live_capture/127.0.0.1_56684_results/` : the
exported jamovi “results” panel scripts (where the actual table appears)

To **capture new html** in the dev console, **ask the maintainer
whenever you need**.

Look at `dev/tabxplor_2.0.0_jamovi_dev.md` and `@dev/jamovi/` for
detailed informations.

------------------------------------------------------------------------

## Common tabxplor package Development Issues

| Issue | Solution |
|----|----|
| R CMD check NOTE about global variables | Add to [`globalVariables()`](https://rdrr.io/r/utils/globalVariables.html) call in `fmt_class.R` (for data.table’s `:=`, `.SD`, `.N`) |
| magrittr `%>%` vs base R pipe | Prefer base R pipe for new code, examples, etc. Package re-exports `%>%` for users. |
| New vctrs type combination doesn’t work | Need both `vec_ptype2.*` and `vec_cast.*` S3 methods for every type pair |
| dplyr verb silently drops class | Missing S3 method for `tabxplor_grouped_tab` — add one in `tab_classes.R` |

------------------------------------------------------------------------

## Architecture Technical Guide

For the full detailed technical reference, see
`dev/tabxplor_architecture.md`, which documents every subsystem in
depth. Read it whenever needed and keep it up-to-date.

------------------------------------------------------------------------

## tabxplor version 2.0.0 roadmap : the current goal

Currently implementing tabxplor 2.0.0 (2.0.0 only if breaking changes
land). **Update the sections below at the end of every work session.**

Phases already implemented can be found in
`dev/tabxplor_2.0.0_roadmap_DONE_PHASES.md` Only phases not yet finished
appear below.

### The aim of 2.0.0 — read first, it governs every decision

This version exists to **refactor and simplify
[`tab()`](https://bricenocenti.github.io/tabxplor/fr/reference/tab.md)/[`tab_many()`](https://bricenocenti.github.io/tabxplor/fr/reference/tab_many.md)**
— the two functions that matter — by **stripping the white-elephant
flexibility that real-world data analysis never uses**, and
**redesigning the underlying `tabxplor_fmt` vctrs-field architecture**
(one combined field pass) to fit the simpler, faster model. The
governing rule, non-negotiable:

- **Public API stays retro-compatible.** User-facing functions, their
  arguments (soft-deprecate, never hard-break), and the `tabxplor_fmt`
  fields users read with
  `$`/[`mutate()`](https://dplyr.tidyverse.org/reference/mutate.html)
  keep working.
- **Internals are redesigned as radically as needed** for consistency,
  simplicity, and performance. Do **not** preserve internal structure,
  dead code, or the old step-by-step (`tab_pct`→`tab_ci`→…) paths for
  their own sake — remove them, fuse them, route everything through the
  one aggregate-core. Whenever a choice trades never-used internal
  flexibility for a single well-defined faster path, take it.

Every phase and decision below serves that aim: fewer knobs, one
computation core, a field set shaped to the real use cases.

### Start here

This roadmap is the **plan of plans**: the phased implementation order
plus every open question. A fresh session asked for a *part* of the work
should read, in order:

1.  **This roadmap** — the phase your task belongs to, its bullets, and
    its pointers
2.  **`dev/tabxplor_2.0.0_roadmap_DONE_PHASES.md`** – the detailed
    report of all the **already implemented phases of the roadmap**.  
3.  **`dev/tabxplor_2.0.0_decisions.md`** – the **new architecture
    decisions** taken for version 2.0.0. Some parts of the file may be
    outdated :
4.  **`dev/tabxplor_architecture.md`** — architecture guide (type
    system, pipeline, compaction loss, exporters). It describes the
    **current** architecture. Read the section matching the file you
    touch.
5.  **Top of this CLAUDE.md** — Repository Map, Global Architecture, Key
    Constraints, Design Decisions.

**Other long-form 2.0.0 docs live in `dev/` (all `.Rbuildignore`’d),
never inline here — read the matching ones before you start:** -
`dev/benchmarks/` — performance harness + saved results (documented
under *Reference \> Benchmarks*). Read/run when a phase touches perf
(Phases 2, 3, 6, 8). - `dev/benchmarks/tab_many_performance_profile.md`
— the full 2026-07 profile. Read before optimizing `tab_many` /
`tab_chi2` / `tab_num`.

#### Verification (every phase)

- **Byte-identity**: `devtools::test("~/github/tabxplor")` after each
  phase; `test-golden.R` + `test-export-parity.R` +
  `test-fmt-contract.R` + `test-fuse-parity.R` stay green. Intentional
  output changes → rerun `dev/make_golden.R`, review the
  `_golden/`/`_snaps/` diff consciously,
  [`testthat::snapshot_accept()`](https://testthat.r-lib.org/reference/snapshot_accept.html).
- **Release gate**:
  [`devtools::check()`](https://devtools.r-lib.org/reference/check.html)
  (~3 min, run manually by maintainer) before CRAN.

------------------------------------------------------------------------

### tabxplor Phase 17 — ecosystem integration roadmap (end of v2.0.0)

This is the plan of plans for the last development stretch of v2.0.0,
implementing `dev/tabxplor_ecosystem_simplification.md` (the six-audit
design analysis, reviewed and decided by the maintainer on 2026-07-20).
Phases group the tasks that need the same systemic understanding of the
same code region, so a session builds that understanding once (with
search agents) and spends it fully. Respect its order.

**Precedence rule for the analysis doc**: where §5/§9 of
`dev/tabxplor_ecosystem_simplification.md` contradicts its §6 table or
its “Maintainer choices” (both edited by the maintainer), **the §6 table
and Maintainer choices win**. The reconciled rulings are §Settled
decisions below — implement those, not the stale §5/§9 lines.

The release freezes every surface this roadmap touched — anything in
§Settled decisions marked “now” that has not landed by then converts
into a permanent deprecation project, which is the one outcome this plan
exists to avoid.

------------------------------------------------------------------------

#### The mission — read this first, it governs every phase

Phase 17 exists to cure five diagnosed disease patterns (analysis §2),
not to add features. Every session must hold these as hard rules:

1.  **Simplify and integrate — never add another ad hoc layer.** When a
    task needs a new behaviour, extend the relevant shared model or fact
    table; never bolt a special case onto a call site. Remove traces of
    old implementations entirely when they become useless — no
    commented-out corpses, no “kept just in case” branches.
2.  **Roles are stored, never guessed.** No code may identify a
    row/column/cell by matching its rendered English label, its name
    prefix, or a magic field value. If you need to know what something
    is *for*, read its stored role; if the role is not stored yet,
    storing it is part of your task.
3.  **One resolver, one model, taken to completion.** A setting is
    resolved ONCE (in the settings frame / the render model / the fact
    table) and consumed everywhere. If you find yourself re-deriving
    “what kind of column is this” downstream, you are patching the
    disease, not the symptom.
4.  **The axes never meet in a vectorised expression.** Anything indexed
    per row_var and anything indexed per col_var may only combine
    through the settings frame (one row per pair). No `length(x) == n`
    guessing, no cross-axis `&`.
5.  **Facts live in ONE table.** Never maintain two encodings of the
    same rule “kept in sync” by comment — derive both consumers from one
    source, or group by the rendered output itself (the 16e lesson).
6.  **Public API stays retro-compatible; internals are free.** The
    2.0.0-new, never-released surface (constructor formals, new args,
    new options) is still free to change — **that freedom ends at the
    CRAN release**, which is why Phase 17 runs now.
7.  **A claimed fix ships with the fixture that fails without it.**
    Assert non-zero counts; never let a test pass vacuously.
8.  **Byte-identity discipline.** Each phase declares which parts are
    byte-identical targets (goldens must not move) and which are one
    conscious snapshot regen. Run the suite exactly as CLAUDE.md §
    Testing prescribes (`OMP_NUM_THREADS=1`, `TESTTHAT_CPUS=8`, temp
    runner outside `tests/`).
9.  **End-of-phase documentation discipline** (CLAUDE.md § The last step
    of every implementation): file headers, `# DESIGN:`/`# WARNING:`
    tags, CLAUDE.md § Key Design Decisions line,
    `dev/tabxplor_architecture.md` when structure changes, NEWS.md when
    user-facing. Line refs in this roadmap are anchors from the
    2026-07-20 audit — **re-grep before editing**, they drift as phases
    land.

------------------------------------------------------------------------

#### Settled decisions — maintainer rulings, do not re-open

| Decision | Ruling |
|----|----|
| `meta` merge of the five 2.0.0-new table attrs | **Yes, merge now** (Phase 17b) |
| Role model (row/col kinds, honest pvalue cells, reg column role) | **Yes, now**, before the French phase (17c) |
| `tabxplor.output_kable` | **Keep** (used in .Rmd/.qmd); **fix** its KNOWN-BUG instead of retiring (17g) |
| kableExtra engine + `kable_tabxplor_style` + `always_add_css_in_tab_kable` | **Keep as legacy** — no kill, no deletion; fix stale comments, degrade gracefully without kableExtra (17g) |
| `mnl_vsrest` (MNL “j vs rest” at profile) | **Keep** (maintainer removed it from the cut list) |
| `method = "profile"` | **Keep as-is** (no shrink) |
| `tab_plot` | **Freeze as legacy**: keeps working, zero new investment, redesigns only preserve its compatibility |
| `predicted_unadjusted` | **Cut now**; keep the Emp.%==unadjusted identity as a test assertion |
| `tab_num(df=, num=)` escape hatch | **Cut now** (soft-deprecation shim if it turns out 1.3.1-public — verify at implementation) |
| `totcol` 5-grammar parser | **Cut 3 of 5 grammars now** (names / numeric indices / “col”-“no” vector); keep “last”/“all_col_vars” + “each” |
| `.by_table` on [`tab_many()`](https://bricenocenti.github.io/tabxplor/fr/reference/tab_many.md) | **Make internal now** (parity-test plumbing, not a public arg) |
| `conditional_format`, `n_min`, `hide_near_zero` on [`tab_xl()`](https://bricenocenti.github.io/tabxplor/fr/reference/tab_xl.md) | **Drop now**, before release |
| `filter=` string arg on [`tab()`](https://bricenocenti.github.io/tabxplor/fr/reference/tab.md) | **Doc-deprecate** (keep working) |
| `score_from_lv1` | **Keep** + add test + document + vignette mention (17j) |
| `tab_get_wrapped_dimensions` | **Keep** (personal use), no action |
| `fct_clean`, `compare_levels`, `formats_SAS_to_R` | Delete if unexported; lifecycle-deprecate if 1.3.1-exported; `formats_SAS_to_R` may move to `dev/` |
| `quasipoisson` arm, compound-formula escape hatch | **Keep** (cheap / contained) |
| jamovi JS helper duplication, tier-3 reref sub-path | **Keep as-is** (maintainer removed both work items) |
| Dead weight (§2.5 + §6 “delete now” rows) | **Delete now** |

**Anti-propositions (analysis §7, all confirmed):** no reg columns
through the aggregate core; no fmt field merges or column-attr drops
(c-iii stands); keep the S3-per-verb registrations; keep the
test-display two-rail split (console grid vs export rows); no re-opening
of settled perf verdicts (scan fusion, chi2 marshalling, `.fine` seam);
no `pct="col"` parity work as a side effect.

------------------------------------------------------------------------

#### Target architecture — the global image after Phase 17

**Metadata model.** The 18 fmt fields are untouched (user contract).
Column attributes go 10 → **11** with `role = "model" | "emp" | ""`, and
`fmt_col_attrs` is **derived from one source** (the
[`new_fmt()`](https://bricenocenti.github.io/tabxplor/fr/reference/new_fmt.md)
formals minus the field names) so an attribute can never again be
forgotten at a rebuild site. The table constructor becomes
`new_tab(tabs, subtext, test, meta)` (+ deprecated `chi2` alias):
`subtext` (CRAN-public) and `test` (data, needs `vec_rbind`) stay
top-level; **`meta` is ONE list** holding `vars` (roles incl. the new
`row_roles`/`col_roles` kinds, `wt`, the new `caption`), `ci_settings`,
`render_extras`, `empirical_tips`, `reg_meta`, `color_breaks`. One
`tab_attrs()` line per top-level attr; `meta` reconciles element-wise on
bind; every existing getter keeps working as an accessor into `meta`.

**Resolution spine.**
[`tab()`](https://bricenocenti.github.io/tabxplor/fr/reference/tab.md)/[`tab_many()`](https://bricenocenti.github.io/tabxplor/fr/reference/tab_many.md)
normalize arguments ONCE at the boundary into a **settings frame** — one
row per (row_var × col_var) pair carrying every resolved per-pair
setting (pct, or, ci, colour spec, digits, levels, na, ref rule…). A
**typed ctx** (constructor with defaults, no
[`exists()`](https://rdrr.io/r/base/exists.html) guards) carries it;
`tab_rowvar_ctxs` slices frame rows. The leaves (`tab_plain`/`tab_num`)
split into public wrapper (parses user args) + **core that consumes
resolved settings only** — no re-forcing, no double
`finalize_color_spec`, no legacy-string re-decoding. A **reference
plan** (per leaf: ref-row rule per comp group, `ref_col_idx` per column,
ref2) is computed once and executed by `tab_apply_reference` (signature
preserved — the jmvtab reref consumes it).

**Fact tables.** ONE `MEASURES` table drives both the colour plan and
the legend (word, glyph, raw field, scale key,
`sig_source ∈ {bounds, pvalue, none}`, totrow/refrow gates); the reg
**empirical fact table** (per family × effect: column names, fmt shape,
CI function + method, colour measure) drives the crude-companion
builders AND derives `ci_settings` — the “empirical CI matches the model
CI” rule becomes data.

**Render path.** `tab_export_prep()`’s model carries roles **including
the stored kinds** (no English whitelists, no rendered-string equality);
a **staged materializer** declares synthetic rows/cols as specs with
per-backend fold policies (no create-then-delete cycles); transpose is a
flipped call into a shared `roles_from()` builder (no second model);
[`format()`](https://rdrr.io/r/base/format.html) remains the ONLY string
producer (export-parity contract); footer =
`tab_footer_streams`/`render_footer` behind one `rd_footer()` helper.

**jamovi.** One cache **kernel** (store lifecycle, byte-bounded LRU,
fetch-or-compute, array folder) with per-module key configs (jmvtab
3-tier, jmvtabreg 2-tier); shared R6 helpers; schema bumps ride the
designed invalidation.

------------------------------------------------------------------------

#### Cross-phase protocol

- **Start of session**: read this roadmap’s phase entry, the analysis
  sections it points to, and the listed code regions (use parallel
  search agents for the audit refresh — line refs below WILL have
  drifted). Read `dev/tabxplor_2.0.0_decisions.md` for any §-referenced
  settled decision you touch.
- **Verification**: full suite green after each phase (the CLAUDE.md §
  Testing recipe). Byte-identical phases: zero golden/snapshot churn
  tolerated — investigate any diff. Conscious-regen phases: regenerate
  ONLY the listed families, review the diff deliberately, record it.
- **jamovi schema**: any phase that changes what the caches store or key
  on bumps `JMVTAB_CACHE_SCHEMA`/`JMVREG_CACHE_SCHEMA` (the designed
  invalidation path). Never hand-edit `.h.R`; UI-file edits stay inert
  until the maintainer’s `prepare()`.
- **End of session**: the § last-step documentation discipline; append
  the phase’s DONE summary under its entry (the maintainer archives to
  `dev/tabxplor_2.0.0_roadmap_DONE_PHASES.md`); accumulate NEWS.md
  entries for user-facing changes (Phase g trims later).
- **If a phase runs long**: split at its marked seam into `-i`/`-ii`
  sessions rather than rushing the tail.

------------------------------------------------------------------------

#### Phase 17a — defects, drift and dead weight (janitorial)

**Goal**: fix every verified defect that needs no redesign, delete all
verified-dead code, and single-source the small sync-by-comment pairs —
so later phases work on a clean floor. Everything here is byte-identical
except the fixed bugs (each gets its failing-first fixture, rule 7).

Read first: analysis §2.4, §2.5, §3; the audit refs below.

1.  **Defect 1**: add `model_family` to the column-attr carry — and fix
    it structurally: derive `fmt_col_attrs` (tab.R:2949) from one source
    ([`new_fmt()`](https://bricenocenti.github.io/tabxplor/fr/reference/new_fmt.md)
    formals minus the 18 field names) so the list can never drift again.
    Fixture: a mixed-family `tab_reg(empirical=)` export keeps
    per-column families through footer materialisation (legend names OR
    and IRR correctly).
2.  **Defect 2**: `vec_math.tabxplor_fmt` sum/mean arms use
    `fmt_color_attr(x)` (as `+`/`-` do) and pass `color_signif` +
    `model_family`. Fixture: [`sum()`](https://rdrr.io/r/base/sum.html)
    over a two-channel column keeps both channels + policy.
3.  **Defect 3**: port the exact-match-first rule into `diff_index_mean`
    (tab.R:4604) — interim fix; Phase 17f deletes the function entirely.
    Fixture: mean table with `ref = "$25000 or more"`-style label.
4.  **Defect 4**: `gtab_cast`/`gtab_ptype2` (tab_classes.R:2846,2862)
    reconcile via `tab_bind_attrs` like the plain path. Fixture: bind
    two grouped tabs, both `test` blocks present.
5.  **Defect 9**: doc corrections — CLAUDE.md colour-engine claim
    (`fmt_color_selection` is gone; the shared artifact is
    `fmt_color_channels`/`fmt_channel_codes`), repo-map line counts
    (fmt_class ~4550, tab_classes ~3999), stale `tab-render-html.R:536`
    “kableExtra is an Import” comment.
6.  **Dead weight, delete**: `var_contrib()`,
    `tab_num(na="drop_fct"/"drop_num")` signature values, `tab_last`
    relic, `ci_html_subscript`, `pillar_shaft.tab_chi2_fmt` (+ NAMESPACE
    line), dead vendored `path_sanitize` (utils.R:964 — or wire
    jmvtab-export’s inline fallback to it, one of the two), ~780
    commented-out lines across tab.R / fmt_class.R / tab_classes.R
    (inventoried in the audits: old tab_ci :6860-6997, pillar relics
    :2399-2466, color_graph, vctrs-FAQ transcription, vec_arith
    relics…). `fct_clean`/`compare_levels`/`formats_SAS_to_R` per the
    ruling (check NAMESPACE first). Move `zscore_formula` to tab-agg.R.
7.  **Small single-sourcing**: adopt `tab_restore()` in the 6
    hand-rolled restore blocks
    (select/rename/rename_with/relocate/summarise/arrange tails); merge
    the twin console print methods
    (`out[3 + inherits(x, "grouped_df")]`); merge
    `vec_ptype_abbr`/`vec_ptype_full`; single-source the `get_wn` NA→n
    fallback (4 copies: fmt_class.R:1345/2620, tab_classes.R:1091,
    tab-test-display.R:490); make `default_ci_settings()` derive from
    [`tab()`](https://bricenocenti.github.io/tabxplor/fr/reference/tab.md)’s
    formals instead of hand-mirroring them.

Verification: full suite, zero golden churn; the new fixtures are the
only new tests.

**DONE (2026-07-20).** Full suite green (FAIL 0, PASS 3794, SKIP 4 = the
usual Suggests/benchmark opt-ins), zero golden/snapshot churn
(byte-identity held everywhere except the four new defect fixtures). -
**Defects.** (1) `fmt_col_attrs` is now DERIVED in `fmt_class.R` —
`setdiff(names(formals(new_fmt)), c(fmt_field_names, "...", "class"))`
off the new single-source `fmt_field_names` (the 18 fields) — so it can
never again miss an attribute; it now carries `model_family` (10 attrs).
(2) `vec_math.tabxplor_fmt` sum/mean arms now use `fmt_color_attr` +
pass `color_signif`/`model_family`. (3) `diff_index_mean` (nested in
`tab_num`) tries an exact label match first. (4)
`gtab_cast`/`gtab_ptype2` reconcile via `tab_bind_attrs(x, ...)` like
the plain path. Each ships a failing-first fixture (test-fmt_class.R ×2,
test-tab.R, test-tab_classes.R). - **Dead weight deleted.**
`var_contrib()`, the `tab_num(na=)` `drop_fct`/`drop_num` values, the
`tab_last` relic, `ci_html_subscript` (inlined at its one caller),
`pillar_shaft.tab_chi2_fmt` (unreachable — NAMESPACE regenerated), the
vendored `path_sanitize`, `fct_clean`, `compare_levels`;
`formats_SAS_to_R` MOVED to `dev/formats_SAS_to_R.R`; `zscore_formula`
MOVED to `tab-agg.R` (beside the CI engine); ~500 lines of commented-out
dead code (old `tab_ci`, pillar/vec_arith relics, `color_graph`,
vctrs-FAQ transcription, old total-recalc + totcol-neutralising
blocks). - **Single-sourced.** `tab_restore()` adopted at the 6 dplyr
restore tails; the two console `print` methods merged into one (grouped
is an alias; header index via `inherits(x, "grouped_df")`);
`vec_ptype_abbr`/`vec_ptype_full` share `fmt_ptype_label()`; the 3
get_wn materialise sites use `fmt_data_wn()`; `default_ci_settings()`
DERIVES from `formals(tab)`. - **Docs.** Defect 9 corrections (CLAUDE.md
colour-engine claim + repo-map line counts, `tab-render-html.R`
kableExtra-Import comment); the stale “9 fmt_col_attrs” comments updated
to “the fmt_col_attrs”.

------------------------------------------------------------------------

#### Phase 17b — table metadata: the `meta` merge

**Goal**: finalize the public constructor surface before it freezes at
release. `new_tab(tabs, subtext, test, meta)` with ONE `meta` list
replacing the five 2.0.0-new scalar formals; `color_breaks` joins it;
`caption` and build-time `vars` complete the metadata.

Read first: analysis §5.6.4 (+ maintainer ruling “merge now”), §8;
tab_classes.R attr threading (`tab_attrs`, `tab_bind_attrs`, the
reconcilers), the ~80 real write/read sites (grep
`render_extras|ci_settings|empirical_tips|reg_meta|new_vars_attr`).

1.  Design: `meta` = named list `vars`, `ci_settings`, `render_extras`,
    `empirical_tips`, `reg_meta`, `color_breaks`. `subtext`
    (CRAN-public) and `test` (needs `vec_rbind`) stay top-level formals;
    `chi2` stays as the deprecated alias formal. `tab_attrs()` returns
    three entries; bind reconcile: subtext union, test `vec_rbind`, meta
    element-wise first-non-NULL (color_breaks: per-scale merge as
    `push_color_breaks` does).
2.  Mechanical pass over the write sites
    ([`tab()`](https://bricenocenti.github.io/tabxplor/fr/reference/tab.md)
    tail, tab_reg tail, tab_counts, tab_compact, the two footer
    appenders’ `attrs=` lists) and read sites (exported getters become
    accessors into `meta` — **every exported getter keeps its signature
    and behaviour**).
3.  `color_breaks` thereby joins the carried attrs (fixes defect 7) —
    `tab(color_breaks=) |> filter()` keeps the per-table breaks;
    document in
    [`?tab`](https://bricenocenti.github.io/tabxplor/fr/reference/tab.md).
4.  Add `caption` as a `meta$vars` sub-field: written by a new
    `tab(caption=)`? NO — no new public arg without need; written by
    `tab_kable(caption=)`-style setters? Decision recorded in analysis
    §8: a stored caption so it survives pipelines; implement as
    `vars$caption`, settable via a small exported setter
    ([`set_caption()`](https://bricenocenti.github.io/tabxplor/fr/reference/set_caption.md))
    and read by every exporter’s caption fallback (before `reg_title`).
5.  [`tab_plain()`](https://bricenocenti.github.io/tabxplor/fr/reference/tab_plain.md)
    writes `vars` at build (it is free) so `tab_render_vars` stops
    guessing on step-built tables.
6.  Bump both jamovi cache schemas (the tier-3 carrier stores unwrapped
    attrs).

Verification: full suite; byte-identical rendering (attribute plumbing
only). Sentinels: test-tab_classes (verb survival), test-jmvtab-cache /
test-jmvtabreg-cache cold+warm, export snapshots unchanged.

**DONE (2026-07-20).** Full suite green (FAIL 0, PASS 3824, SKIP 4 = the
usual benchmark/Suggests opt-ins). Zero display/export snapshot churn
(rendering byte-identical); the structural `_golden/*.rds` were
consciously regenerated — a script proved for all 36 cases that the ONLY
delta is the reshape (body/subtext/test byte-identical AND the new
`meta` == the old separate attrs). - **Constructor.**
`new_tab(tabs, subtext, test, meta)` (+ deprecated `chi2` alias) — the
five 2.0.0-new formals collapsed to ONE `meta` list;
drop-NULL-then-attach keeps “absent when unset” (all-NULL meta → no
attribute). `new_grouped_tab` mirrors it. Roxygen folded to one
`@param meta`. - **Accessors.** `get_meta()`/`set_meta_field()` (NULL
value removes a sub-field; emptied meta drops the attribute — the
load-bearing path for `set_render_extras(NULL)`). Every legacy
getter/setter (`get/set_render_extras`, `_ci_settings`, `_vars_attr`,
`_empirical_tips`, `_reg_meta`, + new `get_color_breaks_attr`) is a thin
accessor into `meta`, names/signatures unchanged. - **Carry/bind.**
`tab_attrs()` returns three entries (`subtext`/`test`/`meta`);
`tab_bind_attrs()` unions subtext, `vec_rbind`s test, and
`tab_meta_bind()` reconciles meta element-wise (x wins, other fills
NULL) with `color_breaks` merged per named scale. The vctrs reconcilers
were untouched (already route through `tab_bind_attrs`). -
**color_breaks joined meta** → survives a dplyr pipeline (defect 7
fixed; still set last). **caption**: exported
[`set_caption()`](https://bricenocenti.github.io/tabxplor/fr/reference/set_caption.md)/[`get_caption()`](https://bricenocenti.github.io/tabxplor/fr/reference/set_caption.md)
at `meta$vars$caption`, read by md/kable/xl/plot ahead of `reg_title`
(threaded as `rd$caption` in the prep).
**[`tab_plain()`](https://bricenocenti.github.io/tabxplor/fr/reference/tab_plain.md)**
now records `vars` at build (render-parity verified — matches the old
last-factor heuristic; the 1-level branch records
`tab_vars=character(0)` for the dropped columns). Both jamovi cache
schemas bumped (JMVTAB 3→4, JMVREG 1→2). - Producer tails updated (build
tail, compact rebuild, transpose `attrs$meta$vars`, the two `tab_reg`
[`new_tab()`](https://bricenocenti.github.io/tabxplor/fr/reference/new_tab.md)
calls, both footer-append `attrs=` lists). New sentinel file
`test-meta-attr.R` (carry, per-scale bind merge, NULL-clear isolation,
absent-when-unset, caption round-trip + md precedence, get_chi2
back-compat). Four conscious test updates (storage moved into `meta`):
`test-export-prep` (rd gains `caption`),
`test-counts-parity`/`test-color-legend`/`test-color-config` (strip/read
via the getters).

------------------------------------------------------------------------

#### Phase 17c — the role model (keystone)

**Goal**: everything knows what it is. Stored kinds for synthetic
rows/columns, honest `pvalue` cells, a reg column `role` attribute —
retiring every render-then-match-by-English heuristic. **This phase
unblocks the French translation phase.**

Read first: analysis §4 (all), §2.1; tab-export-prep.R (tot_block
detection), tab_classes.R (`tab_collapse_total_rows`,
`tab_materialize_extras`), tab-transpose-render.R (absorb heuristics),
tab-test-display.R (cell builders), fmt_class.R (legend adapters,
`fmt_color_plan` significance gate).

1.  **Row/col kinds**
    (`"data" | "total" | "n" | "row_pct" | "pvalue" | "gof" | "sd"`)
    stored in `meta$vars$row_roles`/`col_roles`, written by every
    materializer at creation (`tab_add_n_pct`, `tab_append_footer`, the
    xl sd-twin, `tab_or_total_col`, total-row builders). Consumers
    switched: export-prep’s tot_block detection (the English whitelist
    at tab-export-prep.R:410-416), `tab_collapse_total_rows`
    (rendered-string equality at tab_classes.R:1360-1362 → role + key
    comparison), the transpose absorb heuristics
    (tab-transpose-render.R:181,187). Keep a graceful fallback for
    hand-built tables without roles (the old heuristic, clearly marked
    as fallback-only).
2.  **Honest p-value cells** (fixes defect 5): the p lives in the
    `pvalue` field; the colour plan gains the explicit
    `sig_source = "pvalue"` gate for these cells (the mechanism contrib
    already uses); delete the `diff = -0.5` magic, the `pct`/`var`
    double-write, and the write-only `col_var = "chi2_cols"` marker.
    Conscious regen: export snapshots containing p-value/GOF rows
    (values identical, storage honest); fixture: p ≥ 0.05 row turns red
    under `color_signif = "grey_non_signif"`.
3.  **Reg column `role` attribute** (`"model" | "emp" | ""`, the 11th
    column attr — safe now that `fmt_col_attrs` is derived, 17a.1):
    written by `reg_build`/`reg_empirical_columns`, read by
    `legend_reg_adapter`/`legend_reg_eff_word`/`legend_specs` instead of
    `startsWith("Emp.")`; `legend_ref_label` uses
    [`is_totcol()`](https://bricenocenti.github.io/tabxplor/fr/reference/fmt.md)
    instead of `startsWith("Total")`. One `/vctrs-field` checklist pass.
4.  Re-grep at the end: **zero** remaining sites matching rendered
    labels or name prefixes to decide behaviour
    (`rg 'startsWith.*(Emp|Total)|"pvalue"|"row_pct"' R/` reviewed line
    by line).

Verification: full suite; conscious regen limited to p-value/GOF-row
snapshots + the fmt-contract record-shape snapshot (11th attr).
Everything else byte-identical.

**DONE (2026-07-21).** Full suite green (FAIL 0, PASS 3848, SKIP 4 = the
usual Suggests/benchmark opt-ins). Conscious regens exactly as planned,
nothing else moved: the 36 structural `_golden/*.rds` +
`fmt-contract.md` (a script proved the ONLY delta is the added `role=""`
attr — p-value rows are display-only, never in a built golden), and
`render-html.md` (the p-value cell lost its bogus
`diff: +0% ; contrib: 0%` tooltip; the value `<0.01% (Chi2 !)` is
byte-identical). - **(C) the 11th column attr `role`**
(`"model"`/`"emp"`/`""`, internal `get_role`) added to
`new_fmt`/`fmt`/`new_rcrd` and every reconstructor (`vec_cast`×3,
`vec_ptype2`, `vec_arith` ±/×÷, `vec_math` sum/mean) beside
`model_family`; written by
`reg_column`/`reg_marginal_column`/`reg_unadj_column` (`"model"`) and
all 7 `reg_empirical_columns` sites (`"emp"`); read by `legend_specs` +
`legend_reg_eff_word` (which dropped its now-dead `cn` arg).
`legend_ref_label`’s `startsWith(nm,"Total")` →
[`is_totcol()`](https://bricenocenti.github.io/tabxplor/fr/reference/fmt.md).
Zero `startsWith.*(Emp|Total)` behavioural sites remain. - **(B) honest
p-value cells** — `pvalue_line_fmt()` writes the p to the `pvalue` FIELD
(dropped `pct`/`var` double-write, the `diff = -0.5` magic, the
write-only `col_var = "chi2_cols"`);
[`format()`](https://rdrr.io/r/base/format.html) +
[`get_num()`](https://bricenocenti.github.io/tabxplor/fr/reference/fmt.md)
read `get_pvalue`; `get_stars()` gained a
`display %in% c("gof","pvalue","blank") → ""` gate (closes the
`has_stars`/xl-star-pad leak); `fmt_color_slots()` colours a
non-significant test row (`p > alpha`) with the deepest under-slot on
the `diff` channel — byte-identical under `ignore`, and now firing under
`grey_non_signif`/`guaranteed_effect` too (defect 5). - **(A) the
row-role model** — `meta$vars$row_roles` (positional, display-time; seed
in `tab_materialize_extras`, thread through
`tab_append_pctcol_rows`(`role=`)/`tab_append_footer`(`row_role=`),
slice in `tab_collapse_total_rows`); resolver `tab_row_roles()`
(stored-or-fallback) retired the three English whitelists (export-prep
tot-block, collapse sweep, + the 2 secondary-display sites) and the
transpose absorb heuristic (fixed structurally). **No `col_roles`**
(col-side detections were already structural). See
`dev/tabxplor_architecture.md` § The row-role model.

------------------------------------------------------------------------

#### Phase 17d — colour, legend and display facts

**Goal**: one fact table for measures end-to-end; the colour-spec maze
decoded once at the boundary; the display token system canonicalised.

Read first: analysis §5.2, §2.2; fmt_class.R colour pipeline
(`color_scales` → `color_measure_policy` → `fmt_color_plan` →
`fmt_color_slots` → `resolve_color_channel_plans` →
`fmt_color_channels`), the legend `MEASURES` table +
`legend_resolve_spec`, tab.R/tab-resolve.R normalizers
(`normalize_color_spec`, `finalize_color_spec`, `legacy_union`), the
`/color-mode` skill.

1.  **`get_ref_field(x, field)`** — one base-R helper replacing the four
    broadcast clones
    `get_ref_pct`/`get_ref_means`/`get_ref_var`/`get_mean_contrib` (~70
    L, colour-hot-path speedup per the `fmt_row_flag` precedent).
    Byte-identical.
2.  **Unified `MEASURES`**: extend the legend’s fact table with the plan
    columns (raw field, scale key per column kind, `sig_source`,
    totrow/refrow gates) and make `fmt_color_plan` read it — 11 measure
    switch arms → ~3 (only the diff↔︎ratio bound rescale and the
    guaranteed-effect offset stay as policy code). Adding a measure
    becomes one row end-to-end; update the `/color-mode` skill checklist
    accordingly. Byte-identical target (plan is golden-locked).
3.  **Finish Step 4d**: decode legacy colour strings
    (`diff_ci`/`after_ci`/`ci`) ONCE at the argument boundary; thread
    only the decoded `(color, color_signif)` pair (through the settings
    frame if 17e landed first — see §Order); delete
    `color_measure_policy`’s re-decoding, `legacy_union`’s string
    manufacture, and the `single0` legacy slot table’s plumbing (keep
    the user-facing soft-deprecated strings working at the boundary).
    Bump the jmvtab cache schema (the tuple carried the legacy string).
4.  **Canonicalise `rr` → `ratio`** as the internal token (read-side
    alias only) — deletes the ~8 dual matches (`c("ratio","rr")`) across
    get_num/set_num/format/tooltips; fix the stale
    [`fmt()`](https://bricenocenti.github.io/tabxplor/fr/reference/fmt.md)
    roxygen for `display` while there.
5.  **Optional, only if the byte-harness stays green**: the
    [`format()`](https://rdrr.io/r/base/format.html) token registry (per
    token: source field, ×100, signed, big.mark, min-digits, excel-code
    class). Stop at the first non-identical golden — this item is
    expendable, the phase is complete without it.

Verification: full suite; byte-identical (items 1-4); item 3
additionally cold+warm jamovi cache tests after the schema bump.

**DONE (2026-07-21).** Full suite green (FAIL 0, PASS 3854, SKIP 4 = the
usual Suggests/benchmark opt-ins). Conscious regen limited to exactly
two fixtures (a script confirmed only these two moved):
`_color_golden/c_ci.rds` (single0 retired -\> `color="ci"` renders as
`after_ci`) and `_golden/f_color_afterci.rds` (its stored `color` attr
is now the clean `diff`/`guaranteed_effect` pair instead of the
composite `"after_ci"` – **rendered body byte-identical**). Everything
else byte-identical. - **Item 1 (was uncommitted):**
`get_ref_field(x, getter)` + `fmt_broadcast_last()` replace the 4
broadcast clones. **Item 4 (was uncommitted, finished here):** `rr` -\>
`ratio` canonical internal token (read-side alias `c(rr = "ratio")`);
the two stale `test-display-grammar.R` expectations updated to
`"ratio"`. - **Item 2 (unified MEASURES):** each of the 4 `MEASURES`
rows gained the engine facts (`raw` getter closure,
`scale = c(std=, pct=)` keys, `std_when`,
`sig_source ∈ {bounds,pvalue}`, `gate_row ∈ {refrow,totrow}`);
`fmt_color_plan()` reads them, so the raw/scale switches + the contrib
sig branch + the two gates collapse to MEASURES lookups. Only the
diff↔︎ratio bound rescale + guaranteed-effect offset stay as policy code.
Byte-identical. - **Item 3 (Step 4d):** new `color_decode_legacy()`
(R/tab.R) decodes `diff_ci`/`after_ci`/`ci` ONCE -\>
`(measure="diff", policy)`, called in `normalize_color_spec()`
(`parse_channels` sets the scalar `signif`; `legacy_union()` returns a
clean measure, no more manufacture) and in
[`tab_ci()`](https://bricenocenti.github.io/tabxplor/fr/reference/tab_ci.md)
(stores clean color + color_signif; covers the deprecated step path).
`fmt_color_plan()` reads the clean stored `color` (no re-parse);
`color_measure_policy()` + the `single0` block DELETED;
`resolve_color_channels()`’s `ok` set + the `resolve_col_measures` dead
`color_measure_policy` call dropped. `JMVTAB_CACHE_SCHEMA` 4-\>5 (the
carrier now stores clean colour attrs). **Behaviour change (NEWS):**
`color="ci"` == `after_ci`; `color="after_ci"`/`"diff_ci"` + `ci="cell"`
now errors (use `ci="diff"`, which they always gated on) instead of
silently upgrading – 4 test/helper inputs moved `ci="cell"`-\>`"diff"`
(value-identical for the factor path via the old line-155 upgrade). The
resolve cascade’s INTERNAL `after_ci` vocabulary
(`color_auto_text`/`color_ci`, decoded by `tab_ci`) is left as-is –
byte-identical, and its clean rethread belongs with Phase 17e’s settings
frame (per the roadmap’s own §Order note). - **Item 5 (DEFERRED):**
[`format.tabxplor_fmt()`](https://bricenocenti.github.io/tabxplor/fr/reference/format.tabxplor_fmt.md)’s
display behaviour is driven by compound `(display × type × ci_type)`
predicates with entangled CI-bracket construction, NOT independent
per-token facts (the `n_wn` mask already groups the simple big.mark
tokens). A per-token registry would not capture the interactions and
would risk golden churn for negligible simplification – so, per the
item’s own “expendable / stop at the first golden move” rule, it is
deferred, not forced.

------------------------------------------------------------------------

#### Phase 17e — the settings spine (boundary)

**Goal**: arguments are normalized ONCE into a per-(row_var × col_var)
settings frame; the ctx is typed; the recycle-bug class becomes
unrepresentable.

Read first: analysis §5.1.2/7, §2.3; tab.R boundary
([`tab()`](https://bricenocenti.github.io/tabxplor/fr/reference/tab.md)
pre-recycles, `tab_setup`’s 9+2 recycles, the 5-branch `pct_vect`,
`ref_vect`, `tab_rowvar_ctxs`), tab-parallel.R (`tab_pmap`),
tab-counts.R’s parallel ctx literal, the settled decisions (§5 row-axis
globalisation; Q7 tab_many list guarantee; the ordering invariant).

1.  **The settings frame**: one tibble, one row per (row_var × col_var),
    columns = every per-pair resolved setting (pct, or, ci, colour spec,
    digits, levels, na, totcol-type, ref rule…). All input grammars
    (scalar, per-col_var vector, tab_many list-of-lists, `sup_cols`
    shim) become boundary parsers filling the frame. After `tab_setup`,
    **no code recycles anything** — consumers index the frame.
2.  **`tab_rowvar_ctxs` slices frame rows** — the `length(x) == n`
    heuristic dies.
3.  **Typed ctx**: a constructor giving every field a default (kills the
    39 [`exists()`](https://rdrr.io/r/base/exists.html) guards);
    `ctx_update`’s NULL-preservation rule enforced by the helper, not
    comments. `tab_counts`’s hand-built parallel ctx uses the same
    constructor (kills the ctx-literal duplication).
4.  While there: collapse the triple `stars`-option read and the
    duplicated `comp` forcing into the frame’s resolution (leaf-side
    removal completes in 17f).
5.  **Argument-surface cuts that live in this same boundary code**: the
    `totcol` grammar cut (3 of 5), `.by_table` made internal, `filter=`
    doc-deprecation.

Verification: full suite, **byte-identical** — this is a pure
re-plumbing. Sentinels: test-parallel-parity, test-cache-keys,
test-fuse-parity, the multi×multi shapes (the past bug fixtures must all
stay green). Split seam if long: frame + slicing (17e-i) / typed ctx +
cuts (17e-ii).

**DONE (2026-07-21).** Full suite green (FAIL 0, PASS 3855, SKIP 4),
**zero golden/snapshot churn** — pure re-plumbing. Done in one session,
star schema (maintainer choices). - **Typed ctx `new_ctx()`** (R/tab.R,
body `ctx_update(defaults, list(...))` so an explicit `totcol = NULL` is
a present-but-NULL key — the NULL rule now in the helper). Replaced BOTH
hand-written ctx literals (tab_build + tab_counts → the duplication is
gone; test-carve-parity’s `carve_ctx` too). Deleted the 6 Cluster-A
lean-ctx [`exists()`](https://rdrr.io/r/base/exists.html) guards
(`defer_level_merge`/`cached_tests`/`method_ratio`/`method_mean_diff`/`method_mean_ratio`/`n_min`
→ new_ctx defaults) and converted the `ref_vect` guard to `is.null`.
(The ~29 Cluster-B/C [`exists()`](https://rdrr.io/r/base/exists.html)
are inline data.table leaf locals, out of scope.) - **The settings spine
`ctx$settings` = list(rows, cols, pairs)** built ONCE in `tab_setup`
(the star schema). `pairs` (row-major `expand_grid`) carries `pct`/`ref`
and **REPLACED** the `pct_vect` (5-branch) + `ref_vect` (2-branch) ctx
fields — the two axes now meet only in `pairs`. `tab_rowvar_ctxs` slices
by explicit KEY (`rows[i,]` + `pairs[row_var==rv]` + `fine_num[[rv]]` by
name) — the `length(x) == n` heuristic + the `per_rv` vector are GONE.
`na_text`/`na_num` (population-prep) and `fine_num` (aggregate) stay
per-row_var objects sliced by index/name, NOT settings; the flat per-row
scalar fields remain alongside `rows` for the pre-slice stages + jmvtab
that still read them (17f retires that).
`tab_transform`/`tab_assemble`/jmvtab-cache unchanged (unit projection
is byte-identical) — no schema bump. - **DRY helpers** `resolve_stars()`
(3 sites: tab_setup/tab_num/tab_ci) + `force_comp()` (2 leaf sites);
full leaf-side removal is 17f. - **Arg-surface cuts**: `totcol` keeps
only scalar `"last"`/`"all_col_vars"`/`"each"`/`"no"` (the 3 vector
grammars — names / `"col"`-`"no"` / numeric — cut;
`tot_cols_type == "some"` KEPT, still reached by `each` + mixed
factor/numeric col_vars). `.by_table` removed from `tab_many`’s public
formals (kept on tab_build/leaves). `filter=` doc-superseded (badge,
still works). man/ regenerated (`document()`), NAMESPACE unchanged.

------------------------------------------------------------------------

#### Phase 17f — leaves, reference plan and legacy quarantine

**Goal**: the leaves consume resolved settings only; the reference
system becomes one plan + one executor; the superseded dplyr-era steps
leave tab.R.

Read first: analysis §5.1.3/4/5/6/8, §2.4; tab.R leaves (`tab_plain`,
`tab_num`), `tab_apply_reference` +
`resolve_ref_vector`/`diff_index`/`calculate_refrows` + tab_num’s inline
copies, `tab_ci`’s re-derivation head, the step wrappers,
jmvtab-cache.R’s reref (consumer of `tab_apply_reference` — signature
must hold).

1.  **Leaf wrapper/core split** (decisions §29 Finding 3, endorsed):
    public
    [`tab_plain()`](https://bricenocenti.github.io/tabxplor/fr/reference/tab_plain.md)/[`tab_num()`](https://bricenocenti.github.io/tabxplor/fr/reference/tab_num.md)
    = arg-parsing wrappers; the pipeline calls cores that consume the
    settings frame. Removes the double `finalize_color_spec`, the
    `.color_deprecate` flag, the leaves’ duplicated `ref="auto"`/`comp`
    forcing.
2.  **The reference plan**: per leaf, computed once — ref-row rule per
    comp group, per-column `ref_col_idx` (16c binary-OR encoding
    generalised), ref2. `tab_apply_reference` stays the executor with
    its signature (jmvtab reref untouched); `diff_index_mean` and
    tab_num’s inline `calculate_refrows` copy are **deleted**;
    `tab_ci`’s built-table re-derivation chain
    (`detect_totcols`/`detect_refcol`/8-branch case_when) consumes the
    plan when driven by the pipeline (standalone step-path keeps a
    fallback). Must preserve: `ref` reinterpreted by `pct`, per-row_var
    named refs, the col% collapse message (settled §4).
3.  **Shared leaf tails**: totals renaming, `tab_var_1lv` wrap,
    totrow/tottab derivation, the six-copy placeholder-injection idiom —
    extracted once (~150 L).
4.  **Cut `tab_num(df=, num=)`** per the ruling (deletes the three
    `weighted.mean` N-scan copies, ~90 L); soft-deprecation shim only if
    1.3.1-public (verify).
5.  **Quarantine the superseded trio**:
    `tab_pct`/`tab_tot`/`tab_totaltab` + `pct_formula`/`diff_formula` +
    their repair machinery (~650 L) move to `R/tab-steps-legacy.R`
    (exports unchanged); retire the internal `chi2 =` constructor alias
    and `get_chi2()` reads (10 sites — the public deprecated alias
    formal stays).

Verification: full suite, byte-identical target throughout (item 2’s
`diff_index_mean` deletion is covered by 17a’s ported fix + fixture).
Split seam: leaves + plan (17f-i) / tails + cuts + quarantine (17f-ii).

**DONE (2026-07-21).** Full suite green after every commit (FAIL 0, PASS
3855, SKIP 4 = the usual Suggests/benchmark opt-ins); byte-identical
(zero golden/snapshot churn) except the one intended df/num semantics
change (below). Landed in the two-session seam. - **17f-i (leaves +
reference plan).** Both leaves are now WRAPPER/CORE splits: public
[`tab_plain()`](https://bricenocenti.github.io/tabxplor/fr/reference/tab_plain.md)/[`tab_num()`](https://bricenocenti.github.io/tabxplor/fr/reference/tab_num.md)
(NSE defuse + validate + normalize colour) -\> shared resolver
`plain_resolve()`/`num_resolve()` -\> resolved-args core
`plain_core()`/`num_core()` (pure fmt build, returns PRE-FINALISE).
`tab_transform` calls the CORES directly, so the argument forcing runs
ONCE and colour is finalised ONCE downstream by
[`tab()`](https://bricenocenti.github.io/tabxplor/fr/reference/tab.md)/[`tab_many()`](https://bricenocenti.github.io/tabxplor/fr/reference/tab_many.md)
– killing the numeric **double `finalize_color_spec`** and the
`.color_deprecate` flag (deleted; deprecation now lives only in the
public `tab_num` wrapper). `num_resolve` is forcing-only, so
`tab_transform`’s numeric branch replicates the wrapper’s
digits-cast/total_names-recycle validate; `plain_resolve` does the full
validate+forcing. **Reference plan**: deleted `tab_num`’s inline
`diff_index_mean()` twin + its inline `calculate_refrows` copy;
`tab_num` routes ref-row derivation through the shared
`calculate_refrows()`/`diff_index()`. `tab_apply_reference()` signature
unchanged (the jmvtab tier-3 reref pins it); `tab_ci`’s marker-based
re-derivation left as the single reader (the plan already materialises
into fmt markers). - **17f-ii (part 1: quarantine + chi2).** Moved
[`tab_pct()`](https://bricenocenti.github.io/tabxplor/fr/reference/tab_pct.md)/[`tab_tot()`](https://bricenocenti.github.io/tabxplor/fr/reference/tab_tot.md)/[`tab_totaltab()`](https://bricenocenti.github.io/tabxplor/fr/reference/tab_totaltab.md) +
`pct_formula()`/`diff_formula()` to `R/tab-steps-legacy.R` (exports
unchanged; the shared repair helpers used by live `tab_ci`/`tab_chi2`
stay in tab.R). Retired the INTERNAL `chi2=` constructor alias in the
live `tab_spread`/`tab_ci` (`get_test`/`test=`); the PUBLIC alias
(`new_tab`/`new_grouped_tab` `chi2=` formal + `get_chi2()`) is kept. -
**17f-ii (part 2: df/num + shared tails).** `df=`/`num=` now build the
normal table and pull
[`get_num()`](https://bricenocenti.github.io/tabxplor/fr/reference/fmt.md)
per cell at the very end (shared `leaf_extract_raw`), deleting the 3
pre-2.0.0 `weighted.mean` N-scans + the count-only dcast + both early
returns (~90 L). **Intended semantics change** (tests only assert class;
undocumented details): a FACTOR table with `pct = "row"` + df/num now
returns the displayed percentages, not counts (`df=TRUE` still defaults
to `pct = "no"` -\> counts for FactoMineR); unweighted counts are
`double`; `num=TRUE` without tab_vars is ungrouped. Extracted the
byte-identical shared tails `leaf_totrow_tottab()` +
`leaf_rename_totals()` (the `tab_var_1lv` wrap + fmt
placeholder-injection genuinely differ per leaf -\> left separate).

------------------------------------------------------------------------

#### Phase 17g — export stack integration

**Goal**: the render model becomes the one intermediate representation
it set out to be — shared headers, single-sourced hex, a staged
materializer on stored roles, transpose without a second model — and the
print-path bugs die.

Read first: analysis §5.3, §2.2; tab-export-prep.R (the model +
`tab_header_runs`/`tab_label_runs`), tab_md.R, tab_xl.R (+
tab-xl-backend.R), tab-transpose-render.R, tab_classes.R
print/kable/materialize sections, tab-render-html.R; the export-parity
contract (format() = only string producer).

1.  **md onto the shared models**: `tab_header_runs()` + prep’s
    `new_col_var` replace md’s hand-rolled separator/span loops
    (tab_md.R:257-268, 473-505). Conscious md-snapshot regen.
2.  **xl ann-hex completion** (the stale 10j-A-ii TODO): xl consumes the
    theme-resolved hex already in `ann`; its own
    [`get_color_style()`](https://bricenocenti.github.io/tabxplor/fr/reference/tab_many.md)
    lookups die; slot→hex is single-sourced (CSS side reads the same
    source).
3.  **`rd_footer(rd, medium, theme)`**: folds the 4× footer-invocation
    boilerplate + the 4× caption fallback (now reading
    `meta$vars$caption` first, then `reg_title`).
4.  **Staged materializer** (requires 17c roles): synthetic rows/cols
    declared as specs (kind + payload) with per-backend fold policies —
    replaces the 6-8 sequential passes and both create-then-delete
    cycles (n column built-then-folded; total rows
    built-then-collapsed); `xl_materialize_data` becomes a backend
    policy. [`format()`](https://rdrr.io/r/base/format.html) stays the
    only string producer. One conscious cross-backend regen.
5.  **Transpose via `roles_from()`**: extract `prep_one_table()`’s role
    assembly into a builder both orientations call; keep
    `tx_format_source_cols` (physical constraint). Fixes the audited
    drift (transposed tables currently lose `reg_title` +
    `empirical_tips`).
6.  **kableExtra legacy containment** (per ruling — keep, don’t kill):
    fix the stale Import comment, make the html engine’s Viewer print
    degrade gracefully when kableExtra is absent (tooltips off +
    message, no broken dispatch), leave `kable_tabxplor_style` +
    `inst/tab.css` untouched.
7.  **Fix the `output_kable` KNOWN-BUG** (per ruling — the option
    stays): the two-channel-colour crash at the `tab.R:2219` internal
    switch (`mutate` on a `tabxplor_kable`); root-cause the
    finalize/kable ordering divergence; fixture:
    `options(tabxplor.output_kable=TRUE)` + `color = TRUE` auto-prints.
8.  **Drop `conditional_format`, `n_min`, `hide_near_zero` from
    [`tab_xl()`](https://bricenocenti.github.io/tabxplor/fr/reference/tab_xl.md)**
    per the ruling (inert shells).
9.  `tab_plot`: frozen — verify it still renders after 4/5 (it consumes
    the prep + footer streams), change nothing else.

Verification: full suite; conscious regens limited to md snapshots (1),
xl workbook assertions (2/4), transpose locks (5). The transpose≡native
and export-parity tests are the sentinels. Split seam: 1-3+6-9 (17g-i,
mostly mechanical) / 4-5 (17g-ii, the materializer).

**DONE (2026-07-21).** Full suite green (FAIL 0, PASS 3864, SKIP 4 = the
usual Suggests/benchmark opt-ins), **zero golden/snapshot churn** —
every item landed byte-identical except the one intended `output_kable`
fix + additive fixtures. All 9 items done in one session (no conscious
regen needed). - **(7) output_kable crash fixed**: the render moved OUT
of `tab_assemble_output()` to
[`tab()`](https://bricenocenti.github.io/tabxplor/fr/reference/tab.md)’s
tail
(post-`finalize_color_spec`/`tab_apply_display`/`set_color_breaks_attr`),
so a two-channel colour no longer feeds a `tabxplor_kable` into
`finalize_color_spec`’s
[`mutate()`](https://dplyr.tidyverse.org/reference/mutate.html), and the
background channel now renders. Fixture in `test-render-html.R`. **(8)
tab_xl arg drop**: `n_min`/`hide_near_zero`/`conditional_format`
formals + guards + roxygen removed (man/ regenerated). **(6) kableExtra
degrade**: `print.tabxplor_kable` routes through the pure
`kable_print_mode()` predicate → when kableExtra is absent the
interactive Viewer path emits a one-time note + knitr print (no broken
dispatch); stale `:536` Import comment fixed. - **(2) xl ann-hex**:
`tab_xl` consumes `ann$text_hex`/`ann$bg_hex` directly; its private
`text_pal`/`bg_pal` (the two
[`get_color_style()`](https://bricenocenti.github.io/tabxplor/fr/reference/tab_many.md)
calls) deleted — slot→hex single-sourced through `fmt_channel_codes()`
(the CSS side’s source). **(3) footer/caption helpers**:
`rd_footer(src, medium, theme, want_legend, subtext, lang, classes)` +
`rd_caption(rd, user_caption)` (in `tab-export-prep.R`) fold the 4×
footer sandwich + the md/html/plot caption fallback (xl keeps its
named-tabs/`tab_get_titles` tail). **(1) md header**: the spanning-name
row groups by the shared `tab_header_runs()` RLE (width-padded
per-column blanks stay md-local — pandoc can’t colspan). - **(5)
transpose**: `tx_transpose_render()`’s `rd2` now carries
`reg_title`/`caption`/`empirical_tips` through the flip (a transposed
reg table keeps its title/caption/tooltips — the audited drift);
`roles_totblock_edges()` single-sources the total-block border formula
shared with `prep_one_table()` (the rest of the two role models are
genuinely different computations — fmt-based vs flipped-positional — so
a full `roles_from()` merge would rewrite the golden-locked transpose
for marginal gain, not done; documented). **(4) declarative
materializer**: `tab_materialize_extras()` →
`tab_materialize(tab, backend, ctx)` over `materialize_specs()` (a
DECLARED `list(kind, when, apply)` inventory: add_n_pct / or_total /
sd_twin / footer / collapse_totals). The two build-then-undo cycles are
gone: the add_n `n` COLUMN is built for xl ONLY
(`tab_add_n_pct(..., backend=)`; text folds from the Total cell’s own
`n` field, no throwaway); collapse_totals is a declared display slice on
the stored roles. `mat_add_n_pct`/`mat_sd_twin` are the extracted
applies. **(9) tab_plot** verified rendering unchanged.

------------------------------------------------------------------------

#### Phase 17h — tab_reg integration

**Goal**: one Wald finalize, one skeleton aligner, specs as the unit of
truth, the empirical system as one fact-driven framework whose CI rule
derives `ci_settings`.

Read first: analysis §5.4, §2.4; tab_reg.R (`reg_build`, `reg_fit`,
`reg_column`/`reg_marginal_column`/`reg_empirical_columns`/`reg_empirical_tips`,
the `.fit_cache` seam — its byte-identity contract is load-bearing),
tab-agg.R CI engines, test-jmvtabreg-cache.R.

1.  **`reg_wald_finalize()`** replacing the 3 est±crit·se→p-dual→exp
    copies; **`align_to_skeleton()`** replacing the 5 `"\r"`-key mask
    blocks; **`reg_cleanup()`** for the 8× inlined cleannames regex.
    Byte-identical.
2.  **Spec as the unit of truth**: drop the scalar
    family/do_exp/effect_shape/eff_word/color formals from `reg_build`
    (15e populates specs fully); collapse the 30-formal signature
    re-listed at 3 call sites into `(data, specs, shared)`; the 19
    `sp_get()` fallbacks die. Internal-only (no external caller —
    verified).
3.  **Empirical fact table**: per (family, effect) — column names, fmt
    shape fields, CI function + method, colour measure — one builder
    loop replaces the four isomorphic arms; **`ci_settings` derives from
    the same rows** (the 16d rule becomes data). Multinomial tips stay a
    separate arm (different medium). The `role = "emp"` attr (17c) is
    written here.
4.  **Model frame once**: store the complete-case frame (or row mask)
    per fit and thread it to the empirical/tips blocks — the three
    textually-identical `drop_na()` recomputes die; document the
    digest-path fallback in one place.
5.  **Cut `predicted_unadjusted`** per the ruling (~80 L); keep the
    Emp.% == unadjusted-prediction identity as a test-only assertion.
6.  Untouched per rulings: `mnl_vsrest`, `method="profile"`,
    `quasipoisson`, the compound-formula escape hatch, the `.fit_cache`
    digest/reref math.

Verification: full suite; byte-identical (reg tables are not
snapshotted; test-tab_reg\* value assertions + the jmvtabreg cache
byte-identity lock are the sentinels).

**DONE (2026-07-21).** Full suite green (FAIL 0, WARN 0, SKIP 4 = the
usual Suggests/benchmark opt-ins, PASS 3864), **zero golden/snapshot
churn** — every task landed byte-identical except the intended
`predicted_unadjusted` cut (which touches no golden) + its rewritten
fixture. All 5 tasks in one session. - **(1) three shared helpers.**
`reg_wald_finalize(est, do_exp, se/crit | lo/hi, p, disp_known, df)` =
the ONE est±crit·se → p-dual → exp assembly, now behind
`reg_wald_from_tidy` + the `reg_fit` Wald else-branch (the profile
branch supplies `lo/hi/p`, finalize does the exp) + `reg_reref_fit_res`
(the `.fit_cache` reref — byte-identity re-locked by
`test-jmvtabreg-cache.R`). `reg_skel_key()`/`reg_skel_match()` = the
`"\r"` skeleton-align idiom (5 sites, incl. the 3-part tips key via
`extra=`). `reg_cleanup(x, cleannames)` = the 8 inlined
[`cleannames_condition()`](https://bricenocenti.github.io/tabxplor/fr/reference/cleannames_condition.md)
strips. - **(2) spec as truth.** Dropped the 5 scalar
family/do_exp/effect_shape/eff_word/color formals + the `sp_get()`
closure (→ `sp$*`); the residual scalar `family` (mnl_vsrest +
reg_compare_rows) derives from `specs[[1]]$family`. Collapsed the
signature to
`reg_build(data, specs, shared, split_var=NULL, .fit_cache, reference, reref, skeleton_data)`
— `shared` (17 settings) is built once in `tab_reg`, unpacked via
`list2env`, and the split recursion passes
`modifyList(shared, list(design_spec=ds_g))` (split_var stays a formal —
a NULL value cannot survive `modifyList`). No external caller
(verified), so internal-only. - **(3) empirical fact table.**
`REG_EMPIRICAL` (per binomial/gaussian/poisson: base + effect column
SHAPE + CI method literal) + one `emp_col()` builder replace the four
isomorphic
[`fmt()`](https://bricenocenti.github.io/tabxplor/fr/reference/fmt.md)
arms; `ci_settings`’ `method_mean_diff`/`method_mean_ratio` read
`REG_EMPIRICAL` (the 16d “empirical CI == model CI” rule is now data).
`role = "emp"` written once in `emp_col`. Multinomial tips stay a
separate arm. - **(4) model frame once.**
`reg_complete_frame(data, vars)` = the ONE
`drop_na(intersect(unique(vars), names(data)))`; `reg_fit` uses it, the
empirical + tips blocks share it via the `emp_frame_of(dep)` closure
(the reref/digest fit’s `$data` is NULL, so they recompute — from ONE
helper now). - **(5) `predicted_unadjusted` cut** (arg +
`reg_unadj_column` + `reg_marginal`’s `want_unadj`/`pred_unadj` +
tooltip rider); the Emp.% == unadjusted-prediction identity survives as
a direct-refit assertion in `test-tab_reg-empirical.R`. man/tab_reg.Rd
regenerated; NAMESPACE unchanged. - **Regression caught + fixed:** the
`reg_skel_match` refactor first dropped the `if (nrow(prd))` guard
around `prd$pred` (an empty column-less tibble → “Unknown column: pred”
warning); restored at both `pred` sites. `reg_build` line count 3096 →
3025.

------------------------------------------------------------------------

#### Phase 17i — jamovi integration

**Goal**: one cache kernel, two module configs; shared R6 helpers; the
fingerprint blind spot documented and escapable in both modules.

Read first: analysis §5.5, defect 6; jmvtab-cache.R + jmvtabreg-cache.R
(the two store lifecycles, the two LRUs — one O(n²), the three array
folders), jmvtab.b.R + jmvtabreg.b.R (the 4 verbatim blocks), the
schema-bump invalidation design.

1.  **Cache kernel**: extract store lifecycle + byte-bounded LRU +
    fetch-or-compute + generic `jmv_fold_array(arr, key, val, coerce)`
    into one internal module; jmvtab keeps its 3-tier key logic and
    carrier/reref untouched, jmvtabreg its 2-tier digest/fit — as
    configs on the kernel. Fix the O(n²) eviction in passing. Bump both
    schemas.
2.  **Shared R6 helpers**: `.notice()`, `.render_html()`, the
    export-click block, the `jmv-weights` fold — one package-level
    helper set called by both `.b.R` files.
3.  **Defect 6**: document the `jmv_col_fp` value-edit blind spot in
    jmvtabreg’s header (it can serve a stale FIT); thread the
    `tabxplor.jmv_full_hash` escape hatch to both modules; seed +
    document the option in `.onLoad`/`?tabxplor-options` (it is
    currently unseeded).
4.  Untouched per rulings: the JS helper duplication (uijs is
    per-module), the tier-3 reref sub-path.
5.  Preserve absolutely: `jmvreg_fit_key`’s reference-independence,
    `reg_reref_fit_res` byte-identity, the `.h.R` never-hand-edit rule.

Verification: full suite; test-jmvtab-cache / test-jmvtabreg-cache
cold+warm+reref green; byte-identical rendering.

**DONE (2026-07-21).** Full suite green (FAIL 0, WARN 0, PASS 3864, SKIP
4 = the usual survey-Brant + 3 benchmark opt-ins), **zero
golden/snapshot churn** — pure internal re-plumbing + two doc/seed
fixes + the designed schema-bump invalidation. - **(1) shared cache
kernel** at the top of `R/jmvtab-cache.R`: `jmv_cache_config()` +
`jmv_store_new/migrate/env/fetch/put/evict/cached` (ONE byte-bounded
LRU, O(n log n), canonical entry `list(value,bytes,seq)`;
`jmv_hash`/`jmv_col_fp` left in place just below). Both stores consume
it as CONFIG — `JMVTAB_CFG` (3 tiers agg/test/tab3, **schema 5→6**) +
`JMVREG_CFG` (2 tiers digest/fit, **schema 2→3**) — keeping their
one-line `jmv_cache_*` / `jmvreg_*` wrappers so no call site or test
moves. The per-tier byte ceiling folds into the config (the `max_bytes=`
put arg + the `if(tier=="fit")` switch are gone); the reg store’s
**O(n²) `jmvreg_cache_evict`** and its duplicated lifecycle are DELETED.
Two access patterns kept deliberately distinct (functional bump-always
`fetch`/`put` for crosstab; env-mutating bump-on-hit/store `cached` for
reg — the reg tallies/eviction are byte-locked). The canonical entry
rename (`payload`→`value`) touched 3 crosstab tests + 2 ceiling refs
(now read `JMVTAB_CFG$entry_bytes[[...]]`). `jmv_fold_array` NOT added —
the one keyed-array-append (jmvtab-cache.R:215) stays inline (a
one-caller helper is an ad-hoc layer, per the maintainer ruling). -
**(2) shared R6 backend helpers** in `R/jmvtab-export.R`:
`jmv_backend_weights/_notice/_export/_render_html` (take the live
`self`); the 4 verbatim blocks + the now-redundant
`.notice`/`.render_html` private methods are DELETED from both `.b.R`
files (each `.run()` delegates in one line; jmvtabreg keeps its unique
`.hint`). - **(3) defect 6**: `tabxplor.jmv_full_hash` seeded in
`.onLoad` (is.null-guarded → an Rprofile opt-in survives) + documented
in `?tabxplor-options` (new “jamovi live cache” section) + the
blind-spot bullet added to `jmvtabreg-cache.R`’s header. No code
threading needed — both modules already fingerprint through the shared
`jmv_col_fp`.

------------------------------------------------------------------------

#### Phase 17j — options and internal-docs alignment (DONE)

**Goal**: the options namespace is coherent, and the dev docs describe
the post-17 architecture with no trace of the removed machinery.

Read first: analysis §5.6.5, §8; `?tabxplor-options`, `.onLoad`,
`dev/tabxplor_architecture.md`.

1.  **Options pass (2.0.0-new names only)**: `kable_css` →
    `tab_kable_css` (alias kept); `console_theme`/`export_theme` aliases
    for the two non-parallel theme options (old names keep working);
    `jmv_full_hash` seeded + documented (done in 17i — verify);
    `output_kable` + `always_add_css_in_tab_kable` stay per rulings.
    Every option in `.onLoad` AND `?tabxplor-options`, in sync.
2.  **Architecture docs**: rewrite the affected sections of
    `dev/tabxplor_architecture.md` (metadata model, resolution spine,
    fact tables, materializer, cache kernel) and the CLAUDE.md repo
    map + Key Design Decisions to describe the POST-17 state; delete
    descriptions of removed machinery entirely (rule 1 — no traces).
3.  NEWS.md: consolidate the Phase 17 user-facing entries (arg cuts, new
    `set_caption`, option aliases) — Phase g does the final trim.

Verification:
[`pkgdown::check_pkgdown()`](https://pkgdown.r-lib.org/reference/check_pkgdown.html)
still clean; full suite green.

------------------------------------------------------------------------

#### Phase 17k — vignette enrichment: teach the good features

**Goal**: close the gap between the shipped surface and the taught
surface. The audit found a large *cold-but-good* list —
differentiator-grade features no vignette teaches (analysis §1, §6) — so
users literally cannot discover them through the learning path. This
phase adds them where they pedagogically belong, in the same
beginner-first voice as the existing vignettes, on `gss_simple`, with
Suggests-guarded chunks where needed.

Read first: analysis §1 (hot/cold surface), §6 closing note; the three
vignettes + README.Rmd (voice + structure); the roxygen of each feature
below.

Feature-by-vignette map (a paragraph or short subsection each — an
example the reader can run, one sentence on when to reach for it, no
internals):

1.  **Intro vignette (`tabxplor.Rmd`)**:
    - `n_min=` — hiding cells with too-small bases (the small-sample
      companion to `guaranteed_effect`).
    - `subtext=` and the new
      [`set_caption()`](https://bricenocenti.github.io/tabxplor/fr/reference/set_caption.md)
      (17b) — titling and annotating a table that survives the pipeline
      into every export.
    - `transpose=` at export — the sanctioned answer to “col% with
      several row_vars” (settled §7), shown on `tab_kable`/`tab_xl`.
    - [`tab_css()`](https://bricenocenti.github.io/tabxplor/fr/reference/tab_css.md)
      — one stylesheet for a whole document, dark-mode `theme = "auto"`,
      the fixed-width escape hatches
      ([`?tab_css`](https://bricenocenti.github.io/tabxplor/fr/reference/tab_css.md)).
    - `output_list=` — when you want separate tables instead of one
      merged table.
    - One honest sentence on
      [`tab()`](https://bricenocenti.github.io/tabxplor/fr/reference/tab.md)’s
      weighting rule (weighted estimate + unweighted n; Kish `n_eff`
      opt-in;
      [`tab_reg()`](https://bricenocenti.github.io/tabxplor/fr/reference/tab_reg.md)
      is fully design-based) — the vignette layer currently doesn’t
      state it (analysis, Tensions).
2.  **Programming vignette (`tabxplor-programming.Rmd`)**:
    - [`tab_counts()`](https://bricenocenti.github.io/tabxplor/fr/reference/tab_counts.md)
      — a real section: building tabxplor tables from pre-aggregated
      counts (long/wide/freq+N), what CI/chi2 can and cannot do on
      frequency-only input. A whole Phase-4 feature with zero doc
      presence today.
    - [`tab_spread()`](https://bricenocenti.github.io/tabxplor/fr/reference/tab_spread.md)
      / `spread_vars=` — pivoting tab_vars into columns, with the reg
      `split_var` cross-reference.
    - [`score_from_lv1()`](https://bricenocenti.github.io/tabxplor/fr/reference/score_from_lv1.md)
      — per the ruling: test + roxygen refresh land here too, with a
      worked example.
    - A pointer paragraph:
      [`tab_many()`](https://bricenocenti.github.io/tabxplor/fr/reference/tab_many.md)’s
      list mode +
      [`purrr::pmap`](https://purrr.tidyverse.org/reference/pmap.html)
      batch workflow (already in README) linked from here.
3.  **Regression vignette (`tabxplor-reg.Rmd`)**:
    - `split_var=` — a real section: one model per subpopulation, side
      by side, `tab_spread`-able; how it appears in exports (the merged
      vertical first column).
    - `trials=` — grouped-binomial outcomes (the jamovi Model table
      exposes it; R users currently have no example).
    - [`tab_logit()`](https://bricenocenti.github.io/tabxplor/fr/reference/tab_logit.md)
      /
      [`multi_logit()`](https://bricenocenti.github.io/tabxplor/fr/reference/multi_logit.md)
      — one paragraph naming the curated wrappers and when they suffice.
4.  **Placement sanity**: every example must use only exported functions
    (the Last Phase e-iiii lesson — vignettes build against the
    installed namespace); keep each addition short — these are discovery
    paragraphs, not reference docs (the reference lives in
    [`?help`](https://rdrr.io/r/utils/help.html)).

Verification: all three vignettes render with colours (the fansi hook);
[`devtools::build_vignettes()`](https://devtools.r-lib.org/reference/build_vignettes.html)
clean; no new unexported-function calls (grep the chunks); full suite
untouched.

**DONE (2026-07-21).** All three vignettes render clean (each chunk
evaluated – verified via
[`rmarkdown::render`](https://pkgs.rstudio.com/rmarkdown/reference/render.html);
`build_vignettes()` is deprecated, needs `remotes`); the new
`test-score-from-lv1.R` is green (PASS 10); the rest of the suite is
byte-unchanged by this phase. - **Intro (`tabxplor.Rmd`)**: `n_min=`
(relig×race, drops sub-200 rows) appended to the significance section as
the small-base companion to `guaranteed_effect`; the weighting sentence
(weighted estimate / unweighted n / Kish opt-in) in the CI section;
`output_list=` at the end of Sub-tables; `transpose=` +
[`tab_css()`](https://bricenocenti.github.io/tabxplor/fr/reference/tab_css.md)
(one stylesheet, `theme="auto"`, role classes) in Exporting;
`subtext=` +
[`set_caption()`](https://bricenocenti.github.io/tabxplor/fr/reference/set_caption.md)/[`get_caption()`](https://bricenocenti.github.io/tabxplor/fr/reference/set_caption.md)
in Working-with-the-result; a “point-and-click interface (jamovi)”
section (link + module-library install). - **Programming
(`tabxplor-programming.Rmd`)**: new sections
`## Tables from pre-aggregated counts`
([`tab_counts()`](https://bricenocenti.github.io/tabxplor/fr/reference/tab_counts.md)
tidy/table/wide + the counts/wt_counts rule),
`## Pivoting a grouped table into columns`
([`tab_spread()`](https://bricenocenti.github.io/tabxplor/fr/reference/tab_spread.md)
/ `spread_vars=`), `## A score from several factors`
([`score_from_lv1()`](https://bricenocenti.github.io/tabxplor/fr/reference/score_from_lv1.md)),
`## Building many tables at once`
([`tab_many()`](https://bricenocenti.github.io/tabxplor/fr/reference/tab_many.md)
list mode +
[`purrr::pmap`](https://purrr.tidyverse.org/reference/pmap.html)). -
**Reg (`tabxplor-reg.Rmd`)**: the three MASS/nnet `requireNamespace`
guards stripped (now Imports); `## Grouped-binomial outcomes`
(`trials=`, pairs with `score_from_lv1`);
`## The same model within sub-populations` (`split_var=` +
`tab_spread`); a jamovi bullet in Where-to-go-next.
[`tab_logit()`](https://bricenocenti.github.io/tabxplor/fr/reference/tab_logit.md)/[`multi_logit()`](https://bricenocenti.github.io/tabxplor/fr/reference/multi_logit.md)
deliberately NOT taught (legacy wrappers – the full
[`tab_reg()`](https://bricenocenti.github.io/tabxplor/fr/reference/tab_reg.md)
path + the existing comparison section cover it). - **Code**:
[`score_from_lv1()`](https://bricenocenti.github.io/tabxplor/fr/reference/score_from_lv1.md)
roxygen refreshed (description/details on the first-level + NA rule,
`@seealso tab_reg` `trials`) + `man/` regenerated; new
`test-score-from-lv1.R`. - **Pre-existing, NOT this phase**:
`test-tab_reg-survey.R:264` (`empirical=TRUE` expected to throw
`defunctError`) fails at HEAD (8b3333d) – `empirical` is the live
headline arg since 14v (renamed from `empirical_OR`, which is now simply
gone, not lifecycle-defunct), so the test is stale; `git diff HEAD` is
empty for `R/tab_reg.R` and this test. Also
[`devtools::document()`](https://devtools.r-lib.org/reference/document.html)
corrected a stale `man/tab_reg.Rd` (formal order drifted from source).
Both handed to the maintainer.

------------------------------------------------------------------------

### Last Phase – lasts steps and release

#### Last Phase g — tab_reg() improvements

Carefully study the manual review made by the maintainer at
`dev/review_manual/tab_manual_review_pass_4.R`. The problems to resolve,
decisions taken by the maintainer and new features to implement are all
inside R `#` comments.

Other improvements to implement : - Add “html” argument in `tab_export`,
remove “kable” option name altogether (kable can be choosed as an
engine, but the type is really html ; hard deprecation of the option
name : tab_export is new, it was not in the former public version
1.3.1). Rename
[`tab_kable()`](https://bricenocenti.github.io/tabxplor/fr/reference/tab_html.md)
[`tab_html()`](https://bricenocenti.github.io/tabxplor/fr/reference/tab_html.md),
while keeping the
alias[`tab_kable()`](https://bricenocenti.github.io/tabxplor/fr/reference/tab_html.md)
too (not deprecated at all, keep it as normal exported function). - In
legends and table footers, on all kind of exports : 1. Put variable
names in bold ; 2. For background colors legend, breaks text in plain
font weight (keep bold for text colors breaks/legend only).

**DONE (2026-07-21).** Full suite green (FAIL 0, PASS 3902, SKIP 4 = the
usual benchmark/Suggests opt-ins). Seven workstreams; conscious snapshot
regen limited to `_snaps/golden.md` + `_snaps/render-html.md` (md
css-default + monospace numbers + footer/nbsp + bold refs + escaped
stars + bg-plain legend); everything else byte-identical. - **Export
rename.**
[`tab_kable()`](https://bricenocenti.github.io/tabxplor/fr/reference/tab_html.md)
→
[`tab_html()`](https://bricenocenti.github.io/tabxplor/fr/reference/tab_html.md)
(full body + roxygen), `tab_kable <- tab_html` a permanent exported
alias (`@rdname`); `tab_export(format = c("html","md","xl","plot"))`
(`"kable"` hard-removed, new fn); internal callers +
`kable_tabxplor_style` deprecation point at `tab_html`. S3 class
`tabxplor_kable` kept (internal). - **Legend/footer weight (fmt_class.R
`legend_render_line`).** `.lg_tok(bold=, esc=)`: variable names bold
every medium; the bold decision drops for the **background** channel
(text breaks bold, bg breaks plain); the stars token is `esc`-flagged so
the md renderer backslash-escapes `*` (pandoc no longer reads `***`/`*`
as emphasis). User subtext left raw. - **md/html render.** `md_bold`
keeps alignment pad OUTSIDE the `**` (valid `**77%**`, no star
placeholders on references); `td.tx-num` monospace by default (one
`tab_kable_num_font` lever, `_stars` retired); md footer font-size via
`.tabxplor-tab p`; best-effort col_var vertical borders (`:has()` on the
md spacer column); composite `" (n="` join + styled-md level labels use
U+00A0 (no wrap); `tab_md(css = TRUE)` default. - **tab_reg naming.**
`Obs_%`/`Obs_OR`/`Obs_mean`/`Obs_diff`/`Obs_rate`/`Obs_IRR` (was
`Emp.`) + `Model_OR`/`Model_IRR`/`Model_β`; multi-dependent
disambiguated by a `[dep]` bracket the console shows and
`tab_col_var_header()` strips in exports (role-driven). -
**exponentiate=FALSE colour + empirical.** New `log_odds_scale()`
(fmt_class.R) — a non-gaussian coef (`type=="coef"` +
`model_family ∈ binomial/poisson/…`) colours on the LOGGED odds_ratio
breaks (center 0, std=FALSE → SD-division skips), so it matches its OR
twin; gaussian β keeps SD-standardization. Legend `is_std` false for
log-coef (no “SD” unit). `REG_EMPIRICAL` gains `or_log`/`irr_log` twins
→ `reg_empirical_columns(do_exp=)` builds `Obs_log(OR)`/`Obs_log(IRR)`
(logged effect + logged CI). - **split_var auto-spread.**
`tab_reg(spread_models = TRUE)` (+ `tab_logit`): a single
non-multinomial model with a split_var
auto-[`tab_spread()`](https://bricenocenti.github.io/tabxplor/fr/reference/tab_spread.md)s
to side-by-side columns; `reg_spread_models()` folds the split level
into each column’s col_var as `"{level}<br>{outcome}"` (borders +
two-line span; xl converts `<br>`→newline+wrap). `FALSE` keeps the
stacked grouped_tab.

#### Last phase h — final Jamovi UI maintainer’s review

Jamovi `jmvtabreg` improvements.

For the family selector : - Don’t show “auto (detected)”, just auto
chose the auto detected object in the drop list (ex : “binomial
(logistic)” for binary factors). When only one choice is possible, just
grey out the dropdown since there’s not choice left. When the auto
detection fails, for integers or doubles, please autoselect “poisson”
over “gaussian” (more annoying if the models don’t fit in jamovi live UI
than in R session). - The model “modelised level” selector in the third
column is not wide enough to be readable, and the whole family selector
does not take all horizontal available : make sure the whole family
selector is a 3 columns layout taking all horizontal space available
right ; give more space for levels names in the drop list ; third column
“level” text wastes horizontal space and is not necessary (if it shows
the levels of the binomial var, the user see it’s a level picker). -
Only show “poisson (counts)”, not quasi-poisson, but do a quasi-poisson
anyway, like in the current code (with simple poisson dispersion says
1.46 so I guess it’s that) (would also reduce the width of the drop list
since it’s the longest item) ? By the way : with “quasipoisson”
selected, `empirical=TRUE` does nothing. - In mixed model with one
binomial + one poisson, working well, adding a 3 level multinomial
freezed jamovi (restart necessary). Same happened the other way round :
3 level multinomial was working well, adding one binomial + 1 poisson
made it freeze

Rest of the `Model` pane : - Grey out `effect` when there are no
binomial/multinomial/ordinal selected, since “AME” il only meaningful in
these cases (not for gaussian and poisson, right ?) - Grey out
`exponentiate` where there are no binomial/multinomial/ordinal/poisson
selected ?

`Model comparison / predictors subset` : - The menu is great, but it
still freezes very often : I can’t reproduce the pattern for freeze, but
I think just selecting or selecting out predictors too fast may make it
freeze (sometimes it doesn’t even feel fast : like it click “+” to add
three models, they add, then I select out a level, waiting for 5 sec and
not more loading between each action, and it still freezes ; sometime
it’s total freeze that require jamovi restart, sometimes it’s still
possible to remove the analysis and redo, but this one feels random).
Please do thorought web searches about jamovi freeze problems, and help
me diagnose the cause and find a solution. Maybe the model comparison
panel needs a kind of Ok button : since it’s an heavy operation that
have no meaning to be redone every second, maybe the right UI is maybe
“the user pick its models, then click the button to start analysis”,
actually bypassing jamovi UI live display for this one. Once the models
to compare are picked, changes in other buttons in the UI keeps them,
removing a variable in variable selection remove it in all models then
relaunch (maybe guard this one in another way since it had been a source
of freeze with model comparison in the past ?), etc. What other guards
against jamovi freeze ? - There’s a R side problem too : for a row
variable/predictor selection in all models the reference catogory is in
bold in the firt “levels” column, with is the right behaviour ; but when
the predictor have been selected out in any model, the bold dissapear ;
I think it’s just because empty parts of the table doesn’t properly keep
the `in_refrow` field, and mess with reference row detection.

`References and predictor scaling` : - I have a doubt if the reference
selector drop list display the factors levels in the right order, or
maybe mess with the order, please check.

`Significance` pane : - Starts the menu with the two ways of visualising
significance, on the same row, in a clear 3 equal-sized columns layout :
first column label just says “**Show:**” ; second column have the
`color` tick box ; third column have the `stars` tick box. - Second row
have : first column “conf_level =” and the number box (use a number box
with up and down arrows to increment by 0.01) ; second column “method =
*(conf. interval)*” ; third column with the radio buttons (no duplicate
title). - Third row have color_signif, taking all the horizontal space
in the row.

In general for jamovi UI (jmvtab + jmtvtabreg) : - Add an empty line at
the end of each main UI collapsable box, to more clearly separate each
menu from the next when the menu is collapsed (when not collapsed, this
additional line should not show, compact is good).

I still have these messages in jamovi devtools console :
“\[Deprecation\] Listener added for a ‘DOMNodeInserted’ mutation event.
This event type is deprecated, and will be removed from this browser
VERY soon. Usage of this event listener will cause performance issues
today, and represents a large risk of imminent site breakage. Consider
using MutationObserver instead. See for more information.” “addRange():
The given range isn’t in document.”

`Missing values and display` : rename `Display` (missing values are not
here anymore) - Layout for the first row : first column, half the h
space, `estimate_display` ; second column, half the h space : a common
bold title + “wrap_rows =” + “wrap cols =” + a label for cleannames +
“cleannames =”, verticaly stacked (5 rows, matching the 5 rows of
`estimate_display` including it’s label) (to not duplicate titles in
both label and title) - `subtext` auto height growth text box is good,
but it’s very thin : make it take all the horizontal space available at
its right please (same for `jmvtab` subtext box).

Whenever you can, **keep the “real_R_argument = ” syntax** (like :
“color = *(color helpers)*”), since I use the jamovi package as a
progressive approach to teach R / tabxplor on R to literary students
(it’s also why we do not want to translate the argument in French, only
their legend).

In general, **do not repeat the same legend twice in the argument title
(.a.yaml), and in it’s UI label (.u.yaml)**.

Export menu (`jmvtab` + `jmvtabreg`) : - jmvtab Excel export still
fails, windows-side, with default parameters : “Export failed: ℹ In
index: 1. Caused by error in `wb$add_data()`: ! argument 6 matches
multiple formal arguments” - html table export working, but on my
Windows 11 computer it totally fails to find my real `Documents` folder
: it creates a new “C:” folder, but my Windows have a different official
location to “D:” with a pointer towards it in the normal “C:" and all
`Documents` normal shortcuts. How to find the real folder from inside
the locked electron R session ? - Above the Export block, always add an
empty line, or a clear horizontal rule that inserts well in the current
jamovi options pane styling, since it’s not in the collapsable hierarchy
and separation must be distinguished easily.

**DONE (2026-07-21), partial — R-side verified green (FAIL 0, PASS 3915,
SKIP 4); every jamovi YAML/JS/`.h.R` change is INERT until the
maintainer runs
[`jmvtools::prepare()`](https://rdrr.io/pkg/jmvtools/man/prepare.html) +
rebuilds, so those parts need a live-app pass.** - **R backend
(verified, suite-green).** Excel export crash fixed structurally:
`tab-xl-backend.R` `xlb_add_data()` resolves the openxlsx2 NA-arg name
(`na` vs older `na_strings`) from the method’s own formals and passes it
via `do.call(list(NULL))` — no more “argument matches multiple formal
arguments”. `export_documents_dir()` (`jmvtab-export.R`) reads the
resolved Windows known-folder from the registry
(`Shell Folders\Personal`, base-R
[`utils::readRegistry`](https://rdrr.io/r/utils/readRegistry.html),
env-token expansion) so a redirected Documents (D:) is honoured,
`<home>/Documents` fallback off-Windows. `empirical = TRUE` now works
for `family = "quasipoisson"` (rides the poisson crude shapes via
`fam_key`; `REG_EMPIRICAL` unchanged, 3 gate sites generalised).
Comparison **reference-row bold** fixed at the source: `in_refrow` in
`reg_column`/`reg_marginal_column` is now the union-skeleton row fact
(dropped the `& in_model` gate on the FLAG only; value-zeroing stays
gated) so a predictor absent from one model keeps its bold. Fixtures:
test-tab_reg.R (bold), test-tab_reg-empirical.R (quasipoisson),
test-tab_xl.R (NA-arg). **Reference-selector level order**: verified no
sort in R (`jmvtab_reg_ref_vector`) or JS — order is jamovi’s
`col.levels` = factor order; no change. - **Mixed-family + multinomial
freeze**: R is fast (≤1.5 s) and correct — NOT the cause. Measured the
real suspect: the persisted `cache_state` **serializes ~41.5 MB every
run** for a mixed multinomial table (three fits carry their model
frames/qr). Safe mitigation shipped: `private$.checkpoint()` before the
heavy build in both `.b.R` (flushes queued edits so a newer change
supersedes rather than piling up). **Flagged for the maintainer**: a
proper shrink (persist digests, not raw multi-fit stores) touches the
byte-locked reref/AME paths and needs live-jamovi confirmation —
deferred, not hacked. - **Model-comparison “Run button, no live”
(maintainer’s decision).** New `run_compare` Action + hidden
`compare_state` Image (persists the last comparison’s sig + HTML). In
`jmvtabreg.b.R` `.run()`, a ≥2-model comparison (`jmvtab_reg_staged`)
computes ONLY on Run/Export; between clicks it re-serves the last render
or shows an “outdated → click Run” banner (`.compare_hint`).
Single-model use stays live. Pure helpers
`jmvtab_reg_staged()`/`jmvtab_reg_compare_sig()` (jmvtabreg-cache.R)
unit-tested. JS resets the button like the export one. (The cache STORE
shape is unchanged → no schema bump.) - **jmvtabreg JS family selector**
(jmvtabreg.js): “auto”/“quasipoisson” dropped; the family is detected
client-side (`detectFamily`, fetches `dataType` for integer→poisson) and
pre-selected + stored explicitly (so the backend never
re-detects/aborts); single-option outcomes grey the select; full-width
3-col row, wider levels, the “model” prefix dropped.
`effect`/`exponentiate` grey out when all outcomes are gaussian
(`applyModelEnables`). - **jmvtabreg YAML**: Significance pane →
3-row/3-column layout (Show: colour/stars; conf_level; method label +
radios; color_signif full width); “Missing values and display” →
**Display** with estimate_display beside a single-title wrap/cleannames
stack; subtext stretched full width; export `<hr>` separator;
`stars`/`cleannames` `.a.yaml` titles de-duplicated to bare arg names. -
**jmvtab parity**: same collapse-box CSS spacer (`injectTabxCss`),
export `<hr>` separator, full-width subtext. - **Not fixable from
tabxplor / flagged**: the `DOMNodeInserted` + `addRange()` console
warnings are jamovi’s own Electron/Chromium option-UI framework
(compiled `uijs`), not our YAML/JS. The conf_level up/down stepper isn’t
a native jamovi control (kept a plain number box, per decision). The
collapse-box “spacer” + `<hr>` selectors are best-guess against the live
DOM — worth a visual check on rebuild.

#### Last Phase j — last new features 1, effect size statistics and survey-design Chi2 test

In
[`tab()`](https://bricenocenti.github.io/tabxplor/fr/reference/tab.md),
I want to change the way Chi2 et Welch pvalue are calculated for
**weighted** crosstables / mean tables. It should reduce the gap with
[`tab_reg()`](https://bricenocenti.github.io/tabxplor/fr/reference/tab_reg.md)
in that matter. Please, **design a sound infrastructure for a minimal
opt-in survey design pvalues**, for chi2, and if possible it’s
equivalent for ANOVA F / numeric variables. Do not hesisate to do web
searches. Write your design in `dev/tabxplor_2.0.0_decisions.md`. The
AskUserQuestions, plan and implement. - I don’t want to go full survey
design for all tabxplor calculations including all types of ci, etc.,
but I would at least want to have **a opt-in more robust pvalue with
survey weights**. - I’m thinking about simplified survey design with
minimal features like in
[`tab_reg()`](https://bricenocenti.github.io/tabxplor/fr/reference/tab_reg.md),
but I wonder what would be a **good balance between “minimal acceptable
survey weights robust pvalue for users who like it” and added complexity
?** What part of this all could be done withouh changign everything ?
What part of it would be too complicated in the current framework ? -
What to use, Rao-Scott second-order corrected chi-square
([`survey::svychisq`](https://rdrr.io/pkg/survey/man/svychisq.html)) ?
What informations does it need, anything new not yet in fields ? -
Implement Kish’s effective sample size to factors Chi2 pvalue too, with
the opt-in option `options(tabxplor.kish_neff = TRUE)`, since for now
it’s only implemented for numeric variables. Implement the possibility
to add a strata for stratified surveys to regain a bit ? - **What would
be the equivalent for Welch / classic Anova F with numeric variables /
tables of means ?** - Should I accept the possibility to pass a design
object instead of data, while saying clearly to the user that it’s only
for pvalues and won’t be used for confidence intervals etc. (so most of
the pipeline will just extract the normal df from the design object), or
is it too complicated ?

In
[`tab()`](https://bricenocenti.github.io/tabxplor/fr/reference/tab.md),
I also want to add a few new per table summary statistic along Chi2 and
Welch pvalues, all triggered by the same `test=TRUE` : - Cramér’s V /
phi to measure effect size of each crosstable. Is there an equivalent
for numeric column variables / tables of means here ? - Fisher’s exact
on very small crosstables. - Make a default of the current opt-in
behaviour to keep the whole summary table for
[`tab()`](https://bricenocenti.github.io/tabxplor/fr/reference/tab.md)
too (current default is pvalue line only).

Then, we should also think what to add, minimally, in jmvtab, UI for
these new features.

**DONE (2026-07-21).** Full suite green (FAIL 0, PASS 3939 — +24 from
the new `test-effect-size-survey.R` parity file, SKIP 5). The classic
path is BYTE-IDENTICAL: a script proved the 36 structural goldens differ
ONLY by the 3 new `test` columns
(`effect_size`/`es_type`/`pvalue_exact`, body untouched); the only
conscious snapshot moves are `render-html.md` (the summary gained a
statistic + effect-size row) + 3 hardcoded display assertions. Design in
`dev/tabxplor_2.0.0_decisions.md` §51. - **Effect sizes** ride each
omnibus row as two columns: `agg_chi2` emits Cramér’s V (uncorrected
chi2) / phi (2×2), `agg_anova` emits η² = SSB/SST. Rendered as an
“effect size” line (console grid + export summary; `test_fmt_es`).
**Fisher** (`agg_fisher`, size/N-guarded) on small weak factor tables
(`min_e < 5`), stored as `pvalue_exact` ON the chi2 row (no row-count
change) and shown only when the EXACT test ran (a large table’s
simulated fallback is dropped → keeps the chi2 + `!` flag). - **Robust
p-value ladder** (opt-in, all on the `test` attribute):
`options(tabxplor.kish_neff = TRUE)` → `chi2_kish`/`F_kish` (first-order
Rao-Scott n_eff rescale); `test = "survey"` (+ new
`ids`/`strata`/`fpc`/`nest` args, or a
[`survey::svydesign`](https://rdrr.io/pkg/survey/man/svydesign.html) as
`data`) → `chi2_svy`/`F_svy`
([`survey::svychisq`](https://rdrr.io/pkg/survey/man/svychisq.html) /
`svyglm`+`regTermTest`, matches the survey package to 1e-6). New
`R/survey-design.R` = the shared `svy_*` design helpers (tab_reg’s
`reg_*` now delegate, byte-identical) + `tab_robust_overlay()` (runs in
`tab_assemble_tables` where `ctx$data` lives; the ONE test path reading
the microdata, per-table, documented complete-case caveat). - **Export
default** `tabxplor.test_lines` `"pvalue"` → `"summary"` (statistic +
effect size + p-value). **jamovi** gained a `test_robust` selector +
`strata`/`ids` (`.a.yaml`/`.u.yaml`/`.b.R`),

#### Last Phase k — last new features 2, labelled-data

Add full support for **labelled-data (haven/labelled) interop** : - Full
use of labelled:: value labels for factors when they exists. Throught
fast shared functions that recode all factors levels using value labels
attributes, and then work normally on the new levels (so value labels
are, obviously, hardcoded as true levels in the output tibble). When a
factor have no value labels, the result should still be exactly the same
as now. (Do not add additional numbering like “1-Non”, if the user wants
it he can code it in the labels or levels. But remove them from the
value label if `cleannames = TRUE`.) - Opt-in option to replace variable
names with variable labels : what would be the best way ? Store them in
col_var, or a row_var column for tables with multiple row_vars, then
they aren used in all exports anyway ? Are there caveats, or
complexities to it ? - All this **without adding any dependency to the
labelled package** : working with attr() and `attr<-`() must be enough.

**DONE (2026-07-21).** Full suite green (FAIL 0, PASS 3969, SKIP 5 = the
usual Suggests/benchmark opt-ins), **zero golden/snapshot churn** —
non-labelled data is byte-identical (the shared converter no-ops without
a `labels` attr). No new dependency (base
[`attr()`](https://rdrr.io/r/base/attr.html) only). - **Value labels →
factor levels (always on).** `val_labels_to_factor()` (R/tab.R) = the
shared base-R converter: a variable with a `labels` attr whose value
labels are COMPLETE (every observed value labelled) → factor with the
label text as levels, in labels-vector order; INCOMPLETE (maintainer’s
ruling) → stripped to its underlying numeric/character type (a coded
numeric keeps its `tab_num` means path); no `labels` attr → unchanged.
`tab_apply_val_labels()` applies it by name-`[[` (NOT `data[vars]`,
which row-subsets a data.table — the regression the first suite run
caught). Runs in `tab_setup()` (before the numeric/text classification
at ~L1571),
[`tab_prepare()`](https://bricenocenti.github.io/tabxplor/fr/reference/tab_prepare.md),
`plain_core`/`num_core`, `tab_counts_normalize()`, and
[`tab_reg()`](https://bricenocenti.github.io/tabxplor/fr/reference/tab_reg.md)
(before family detection / skeleton). `cleannames = TRUE` then strips a
`"1-"` prefix off the derived levels for free. - **Variable labels →
export names (opt-in, display-only).** `capture_var_labels()` reads each
var’s `label` attr BEFORE conversion strips it; the map rides
`ctx`/`shared` into **`meta$vars$var_labels`** (`new_vars_attr()` gains
the field, stored only when non-empty → absent-when-unset, unioned
across a
[`tab_compact()`](https://bricenocenti.github.io/tabxplor/fr/reference/tab_compact.md)
merge). New option `tabxplor.var_labels` (default FALSE) →
`var_label_display()` (R/tab-export-prep.R) swaps the col-var span, the
single-row_var header, and the merged `row_var` column values (+ the
transpose mirror). Structure keeps canonical names →
[`select()`](https://dplyr.tidyverse.org/reference/select.html)/references
by name still work; the console always shows names. Covers
[`tab()`](https://bricenocenti.github.io/tabxplor/fr/reference/tab.md)/`tab_num`/`tab_counts`/`tab_reg`. -
New `tests/testthat/test-labelled.R` (fixtures built with base
`structure(codes, labels=, label=)`, no haven). man/tabxplor-options.Rd
regenerated; NAMESPACE unchanged (helpers internal).

#### Last Phase k2 — last new features 3, handling of missing table-level attributes

Would it be possible to ensure the tables does not error when
table-level attributes are missing, but only remove the behaviours that
can’t be computed (all tabxplor_fmt fields or column attributes stay
required, since they are more solid) ? Would it be possible to ensure
nothing will error if a tabxplor_tab is converted to a normal tibble,
still doing what can be done with tabxplor_fmt columns metadata and
fields data in a somewhat degraded mode ? What would the user really
lost (summary stats only in tab(), much more in tab_reg() ? ) ? Maybe
just a friendly message in that case, for the user to know it may have
remove table attributes or table class in his pipeline ?

**DONE (2026-07-22).** Full suite green (FAIL 0, WARN 0, PASS 4051, SKIP
5), **zero golden/snapshot churn**. Empirical finding: the
graceful-degradation the maintainer asked for was **already achieved by
design** (Phases 10c + 17b) — a broad probe (crosstab / mean / reg /
grouped, each with `test`/`meta`/`subtext` stripped, the class dropped,
and
[`as_tibble()`](https://tibble.tidyverse.org/reference/as_tibble.html)d;
plus standalone fmt columns and degenerate frames) hit **zero errors**
across print + all four exports. So this phase is a **guarantee-lock +
doc**, not a fix: no production code path changed. - **Confirmed
contract:** the three table-level attrs (`subtext`/`test`/`meta`) are
optional & NULL-safe (getters return `NULL`; consumers treat absent as
absent); losing one drops only its behaviour (missing `test` → the
statistic/effect-size/p-value summary; `subtext` → the note; reg `meta`
→ the caption/title + effect-specific legend wording, falling back to
the generic crosstab legend). Cell FIELDS + column ATTRIBUTES stay
required — a standalone extracted `tabxplor_fmt` column formats &
colours on its own. Exporters are class-agnostic (`is_fmt` detection),
so a class-dropped tibble (or an
[`as_tibble()`](https://tibble.tidyverse.org/reference/as_tibble.html),
which keeps the attrs) exports **byte-identically, fully coloured**. -
**New `tests/testthat/test-degraded-attrs.R`** (10 tests) locks it:
no-error on every strip/downgrade × print/export; class-dropped md ==
classed md; the precise loss per stripped attr (summary / note / reg
title); standalone-column format+colour equals in-table; degenerate
frames degrade without error; bind tolerates a missing `test`. - **The
“friendly message” the note floated was declined** (maintainer chose
silent degrade). Its one honest limitation is documented, not worked
around: a bare [`print()`](https://rdrr.io/r/base/print.html) on a
*fully class-stripped* `tbl_df` runs dplyr’s own printer, which our S3
methods can’t intercept — the fmt columns still render via `pillar`, but
the footer/summary only reappears once the object next passes through a
tabxplor function/export. The once-per-session throttle for
`tab_degrade_inform` (the existing “not a tabxplor table” note) was
tried then **reverted** — it broke the `test-edge-cases.R`
degrade-message loops that assert the note fires each render. `R/tab.R`
change is comment-only; docs updated (NEWS, architecture § Render-time
degrade, this file).

#### Last Phase m — another maintainer’s manual review

Carefully study the manual review made by the maintainer at
`dev/review_manual/tab_manual_review_pass_5.R`. The problems to resolve,
decisions taken by the maintainer and new features to implement are all
inside the maintainer’s R `#` comments. Do not forget **any** of them.

**DONE (2026-07-22).** Full suite green (FAIL 0, PASS 4074, SKIP 4 = the
usual Suggests/benchmark opt-ins). Eight items; the classic path is
byte-identical (built goldens unchanged — `common_totrow`’s
`render_extras` fields are stored ONLY when opted in), the conscious
snapshot regens are `_snaps/golden.md` (md interior-spacer verticals +
the 3 new CSS edge rules) and `_snaps/render-html.md` (Item 7 summary
rows). New fixtures in `tests/testthat/test-review-pass5.R` (one per
fix, failing-first). - **`common_totrow` (new
[`tab()`](https://bricenocenti.github.io/tabxplor/fr/reference/tab.md)/[`tab_many()`](https://bricenocenti.github.io/tabxplor/fr/reference/tab_many.md)
arg, default FALSE).** Default now shows **one Total row per row_var**;
`TRUE` collapses to a single shared Total in its **own group** (a blank
`row_var` sentinel → the group-separator machinery detaches it; level
stays “Total”), **bold** when any row_var used `ref = "tot"`. Stored in
`render_extras$common_totrow`/`_ref` (only when TRUE → zero golden
churn); the `collapse_totals` materialize spec (`tab_classes.R`) is now
gated on it and `tab_collapse_total_rows(ref_bold=)` does the
group-reassign + `in_refrow` set. The old always-collapse default is
gone (a display-only change; `test-display-extras.R` opts in). - **`ref`
positional over col_vars under `pct = "col"`** (`tab_setup`): an unnamed
vector of length \#col_vars maps per col_var (factor→ref column via
`ref_by_colvar`/`ref_vect`, numeric→ref row via `ref_vect[col_vars_num]`
into `num_core`); byte-identical when unset (broadcast). The `n` (count)
row under pct=“col” renders plain (a role-“n”-aware bold override in
`prep_one_table`; `totcol` is a column attr, not clearable per-cell). -
**Summary display (Item 7).** Crosstab `test=TRUE` rows are now
**p-value then effect size** (statistic dropped from the default); the
test type moves into the p-value row NAME (`test_pvalue_descriptor`:
“pvalue (Chi2, Welch F; Kish)”, “Fisher”/” !” flags) and the measure
into the effect-size row NAME (`test_es_measure`: “Cramér’s V, eta2”) —
both shared by the console grid (`tab-test-display.R`) and the export
rows (`tab_pvalue_lines`); the cell is now the bare p (no in-cell
“(Chi2)”). `tabxplor.test_lines` gains “all” (adds statistic back). -
**tab_reg fixes.** `tab_bold_rows` keys on `ref_alltot | is_refrow` (new
`ann$anchor`) and returns `integer(0)` on zero discriminating columns —
killing the binomial `exponentiate=FALSE`+`empirical` all-bold edge
(crosstabs byte-identical, `is_refrow ⊆ ref_alltot`). The colour legend
strips the multi-dependent `[dep]` bracket for reg groups
(`legend_streams`, `fmt_class.R`). `reg_spread_models` re-keys the
`test` tibble onto the spread columns + clears `row_var` → one non-empty
GOF block (was tripled/empty). - **md→HTML borders (`tab_md.R` +
`tab-css.R`, keep the pipe table).** Styled md fills blanked label /
span-row / header cells with U+00A0 so ONLY the real spacer columns stay
`:empty` (kills the span-row stray borders + the ragged left edge).
`tab-css.R` gains div-aware top/bottom/right edge rules. The spacer set
`sep_after` (was `new_col_var`) adds interior boundaries in styled mode
(levels\|numbers, numbers\|Total) — the span row now routes through
`md_insert_col_sep` like the body, so every vertical lines up.

#### Last phase n — Jamovi UI default export folder tests

Default export path still can’t detect my real Windows Documents folder,
an creates “USER/Documents”. Same on WSL : it creates “~/Documents” (is
this folder absolutely standard but just not present in my WSL ?) I
think the R in Electron session is locked, can’t read Windows registry,
etc.

Please think about how, from inside jamovi, we can find a reliable
solution, or a good fallback. How does
`SummaryTable::resolveExportPath()` do at the first place, where do it
writes exports ? Then, create a new jmvtest analysis, and experiment
with at least 5 different solutions to make it work, and 5 fallback
solutions of where to save it if it’s not possible to reliable find the
real documents folder. Use a simple text saved as a .md file, not Excel
or table needed here. Also add buttons to test intermediary results and
features, and I’ll give you the real world results back. I can test live
on Windows + Ubuntu in WSL2, but it shall work on Mac OS too.

**DONE (2026-07-22), diagnostic ran live + the fix landed + jmvtest
archived.** Full R suite green (FAIL 0, WARN 0, PASS 4099, SKIP 4), zero
golden/snapshot churn. - **Real-world results (Windows 11 jamovi
2.7.37 + WSL flatpak 2.7.36; full tables in
`dev/tabxplor_2.0.0_jamovi_dev.md` § Phase o).** Windows winner =
**`registry Shell Folders\Personal`** -\> `D:\Documents` (redirect
honoured; **PowerShell is NOT on the bundled R’s PATH** so
`GetFolderPath` is unavailable; the same registry value carries a
university GPO folder-redirection UNC path -\> robust for managed
machines). Linux base = normal desktop/server Ubuntu (not WSL):
`xdg-user-dir DOCUMENTS` when it returns a real subfolder (`!= $HOME`,
the desktop case), else `$HOME/Documents` created
(server/minimal/WSL). - **The fix (R/jmvtab-export.R):**
`export_documents_dir()` is now a robust per-OS known-folder resolver —
Windows `readRegistry Shell Folders\Personal` -\> `reg.exe query` -\>
`User Shell Folders` -\> `USERPROFILE\Documents`; macOS
`$HOME/Documents`; Linux `xdg-user-dir`/`user-dirs.dirs` (real-subfolder
only) -\> `$HOME/Documents` — validated (exists+writable, else
parent-writable/creatable, else
[`tempdir()`](https://rdrr.io/r/base/tempfile.html)), never errors.
`resolveExportPath()` routes the `"~/Documents"`/`"~"`/`"auto"`/blank
sentinel THROUGH it (a real typed path, incl. `~/Desktop`, is respected)
— fixing the live bug where the non-blank `"~/Documents"` default
skipped the resolver and the wrong `C:/Users/<x>/Documents` won. The
`.a.yaml` export_dir help text updated to match. - **jmvtest retired:**
the throwaway analysis (5 jamovi files + `.b.R`/`.h.R`) moved to
`dev/jamovi/` (de-registered from `0000.yaml`); the diagnostic-only
helpers (powershell/onedrive/wsl detectors, fallback probes, candidate
tables, env-probe, HTML panels) travel with it in the self-contained
`dev/jamovi/jmvtest.b.R`. Only the detectors the fix uses stay in the
package (+ their tests in `test-jmvtab-export.R`). - **Premise
corrections (both wrong in the ask):** there is NO `SummaryTable`
package anywhere — the only `resolveExportPath()` is tabxplor’s own.
jamovi never resolves paths in R; a normal module returns result objects
and the app saves them, resolving `{{Documents}}` once in a native C++
`Dirs` class (`SHGetKnownFolderPath` on Windows,
`xdg-user-dir DOCUMENTS` on Linux). tabxplor writes files ITSELF,
bypassing `Dirs`, hence the R reimplementation. Also root-caused:
`export_documents_dir()`’s registry resolver is DEAD in the default case
— the `"~/Documents"` default is non-blank, so `resolveExportPath` skips
it and `~` expands to `C:\Users\<x>\Documents` (blind to a
D:redirect). - **The diagnostic** (`jmvtest`, menu tabxplor ▸
Diagnostics; the 5 hand files + `R/jmvtest.b.R`, registered in
`0000.yaml`): four Html panels (Environment / Documents-detection
methods / Fallback save locations / Write results) + two Action buttons
that PERSIST a plain `.md` per candidate so the maintainer finds which
one lands in the real Documents. Read-only panels probe with
`file.access` (never litter); writes are `.md`-only via
`export_write_test()` (no Excel — isolates the Phase-o serialization
bug). - **Detectors** (all in `R/jmvtab-export.R`, guarded, never error,
the seed of the eventual `export_documents_dir()` rewrite): 9 Documents
methods (powershell `GetFolderPath('MyDocuments')` \[+wslpath\],
registry Shell / User Shell Folders, reg.exe, OneDrive, xdg-user-dir,
user-dirs.dirs, WSL cmd.exe+wslpath, home/Documents baseline) + a
CURRENT-behaviour row; 5 fallbacks (home / Desktop / Downloads / getwd /
tempdir). Permanent tests in `test-jmvtab-export.R`. -
**Diagnostic-only** (maintainer decision): the live resolver is
UNTOUCHED — the panel shows today’s output beside every candidate.
**Temporary** (maintainer decision): once the winning method is reported
it folds into `export_documents_dir()` and `jmvtest` (+ its generated
`.h.R`) is removed; the detectors + tests stay. - **Maintainer step**:
`Sys.unsetenv("ELECTRON_RUN_AS_NODE")` →
[`jmvtools::prepare()`](https://rdrr.io/pkg/jmvtools/man/prepare.html)
(generates `R/jmvtest.h.R`) → `document()` → `install(home='flatpak')`;
add Diagnostics, click “Write to every candidate”, report which `.md`
reached the real Documents on Windows + WSL (+ mac).

#### Last phase o — Jamovi UI bug corrections

Export to Excel with default parameters in Jamovi still fails (html and
md works), Windows-side **and** Linux-side (WSL): “Export failed: ℹ In
index: 1. Caused by error: ! Invalid input: dims must be something like
A1 or A1:B2.” - Excel exports work well with tab() and tab_reg(), so it
looks like a jamovi problem : maybe due to cache system, the data is
somehow different than a regular tab() and tab_reg() table ? Would it be
a good idea to call
[`tab()`](https://bricenocenti.github.io/tabxplor/fr/reference/tab.md)
and
[`tab_reg()`](https://bricenocenti.github.io/tabxplor/fr/reference/tab_reg.md)
directly (no cache system used) for Excel export only (mardkdown too ?
html not necessarily since it’s also the base jamovi result already
computed ?), but on the df modified by jamovi UI (like : new ref) ?
Would it be sound and reliable ? If not , can you think about others
ways to fix the Excel export in Jamovi ?

Horizontal rule before Export appear as raw html in the UI, it’s written
: “

------------------------------------------------------------------------

” Fix it, use empty line if needed : one empty line before subset, one
empty line before Export block.

Add an empty line at the bottom of each collapsable box elements that
form the main outline of the jamovi options UI.

`Model comparison / predictors subset` : - The “Run comparaison” changes
nothing for the freeze problem (see above). Sometimes it works,
sometimes it freezes, see R code below. So it may definitely be a cache
problem, which is difficult to reproduce in R jmvtabreg since each
button click build cache. Diagnose thoroughly. How to resolve this one ?
Maybe not using the cache system when the user enters the “model
comparison” mode, since it become useless (all models calculated at Run
button click) ? In any cases, the moment the user go back to just one
model, it should reverse to the normal cache system (it’s ok if it’s a
new cache and the old cache is not here anymore). A difficult question
is what to do if the user have ran the comparison between 4 models, and
change options elsewhere in the UI (references, display, ame, empirical,
etc. ) : if it’s a cache problem it will still crash. So any change
should drop the cache and print the “Model comparison staged. Click Run
comparison to compute the table” message, and if the user want cache
system back it can just remove all models in comparison ? - I want the
“Run comparison” button to be with black text and grey background (the
right grey for a good material design depending on jamovi options UI
background colors ; it should be visible yet integrated with other
elements). An empty line is needed after it (like at the end of each
main outline collapsable boxes).

``` r
# Working on jamovi live UI
tab_reg(gss_simple, dependent = "married", 
predictors = list(
  model1 = c("race"), 
  model2 = c("race", "rincome"), 
  model3 = c("race", "rincome", "relig")#, 
),
family = "binomial", # empirical = TRUE, 
) 

# Always freezing on jamovi live UI
tab_reg(gss_simple, dependent = "married", 
predictors = list(
  model1 = c("race"), 
  model2 = c("race", "rincome"), 
  model3 = c("race", "rincome", "relig"), 
  model4 = c("race", "rincome", "relig", "age")#, 
),
family = "binomial", # empirical = TRUE, 
) 
```

**DONE (2026-07-22).** Full suite green (FAIL 0, WARN 0, PASS 4109, SKIP
4 = the usual Suggests/benchmark opt-ins), **zero golden/snapshot
churn** — the R fixes are backend plumbing + one new arg; the jamovi
YAML/JS edits are inert until the maintainer’s `prepare()` + rebuild.
Three Explore agents root-caused each item; two maintainer hypotheses
were corrected (below). - **Excel export crash — NOT the cache.** A
jamovi-built table is byte-equivalent to a fresh
[`tab()`](https://bricenocenti.github.io/tabxplor/fr/reference/tab.md)/[`tab_reg()`](https://bricenocenti.github.io/tabxplor/fr/reference/tab_reg.md)
(verified). Real cause: `xl_coalesce()` packs non-contiguous same-style
cells into a comma-joined MULTI-area `dims` (e.g. `"C7:E8,F4:F8"`) that
the OLDER openxlsx2 bundled in jamovi rejects with exactly that message
(a current openxlsx2 accepts it — why plain R “worked”). Fix: new
`xlb_dims_each(dims, f)` splits a comma dims into single ranges at the
emit boundary; `xlb_numfmt()` + the new `xlb_set_cell_style()` (which
`xl_apply_styles` + the span-row style now route through) apply one
rectangle at a time — semantically identical, works on both openxlsx2
versions, ONE package fix covering jmvtab + jmvtabreg + plain
`tab(...,chi2=TRUE)`. A no-cache export path was **rejected** (fixes
nothing, adds an ad hoc branch). Fixtures in `test-xl-backend.R`
(split + a stub-wb reproducing the old single-range validator). -
**Model-comparison freeze — IS the cache/state.** The raw fits (~10
MB/model) were persisted into `cache_state$state` and re-serialized by
jamovi on EVERY UI round-trip (4 models ≈ 40 MB → freeze; the staged
early-return never cleared it). In comparison mode the cache gives zero
benefit (the reref digest fast-path is off for comparisons; every Run
recomputes). Fix: `jmvtab_reg_build(..., use_cache = TRUE)` — when FALSE
it fits with `.fit_cache = NULL` and returns `store = NULL`; `.run()`
sets `use_cache = !staged`, and `if (staged) cache_state$setState(NULL)`
drops the leak on every staged pass. Reverting to a single model starts
a fresh cache (digest fast-path re-engages). The “staged / changed →
click Run” banners are unchanged. Fixture in `test-jmvtabreg-cache.R`
(identical table, `store = NULL`). - **jamovi UI (inert until
rebuild).** The raw `<hr>` before Export (which jamovi rendered as
literal text — Labels escape block-level HTML) is replaced by a real
border-top drawn in `js/*.js` `styleExportSep()` (walks to the export
block’s `margin: large` container); the two `<hr>` Labels removed from
the `.u.yaml`. `injectTabxCss()` gains a `padding-bottom` on
collapse-box body candidates (empty line at the bottom of each expanded
box). `styleRunCompareBtn()` (mirrors `styleResetBtn`) gives *Run
comparison* a material grey/black button + a blank line below. No
`.a.yaml` change → `.h.R` untouched, no schema bump. **Needs the
maintainer’s live-DOM pass** (the collapse-box body + export-block
ancestor selectors are best-guess; wrong ones no-op).

#### Last Phase p – bug corrections

- ~~**OPEN (found Last Phase e, low impact):**
  `options(tabxplor.output_kable = TRUE)` + a **two-channel colour**
  errors on the auto-print with *“no applicable method for ‘mutate’ …
  tabxplor_kable”*.~~ **FIXED in Phase 17g**: the render ran INSIDE the
  build (`tab_assemble_output`), before `finalize_color_spec`, which
  then [`mutate()`](https://dplyr.tidyverse.org/reference/mutate.html)d
  the returned kable. The render moved to
  [`tab()`](https://bricenocenti.github.io/tabxplor/fr/reference/tab.md)’s
  tail (post-finalize), so it also shows the background channel.
  Fixture: `test-render-html.R`.
- ~~**A pre-existing golden drift.** `n_ci_tabvars.rds` /
  `n_ci_tabvars_all.rds` had a `ci_sup` `NaN` where a clean run wants
  `NA`.~~ **FIXED in 14v-ii**: the cause was `n <= 1` cells
  (`df = n - 1 <= 0` feeding `qt`); `ci_pivot()` now coerces `df <= 0`
  to `NA` (clean NA, no NaN, no warning). The two goldens were
  regenerated with the rule-B mean CIs and no longer carry the NaN.
- **Bad named-`ref` name → cli internal error.** A
  `ref = c(badname = "x")` on `tab_many` surfaces *“Multiple quantities
  for pluralization”* (a raw `cli` pluralisation failure) instead of a
  message about the unknown variable name.
- **`row_var` also listed in `tab_vars` → obscure `tidyselect` error**
  (“Element `marital` doesn’t exist”) rather than “a variable cannot be
  both a row and a tab variable” (the weight-collision case *does* get a
  clean message — mirror it).
- **All-zero / all-`NA`-weight tables → generic** *“data is of length 0
  (possibly after filter or na = ‘drop_all’)”*. Correct outcome (nothing
  to tabulate) but the message never mentions weights; a user who passed
  `wt` with all zeros won’t connect it.
- **Leaked base-R warning on an all-`NA` numeric column**:
  `tab(..., <all-NA numeric>)` emits *“no non-missing arguments to max;
  returning -Inf”* from base R instead of a handled message.

Add a quick word in documentation (more readable than the following
paragraphs to beginners/more quick when its for experts), about two
aspects in vignettes : - A weighted cell CI is exactly
`Wilson(weighted p, unweighted n = tot_n)`. This treats the weighted
proportion as if it came from `tot_n` independent Bernoulli trials, so
under unequal weights the interval is **too narrow** (no design effect).
Also add a quick note to
[`?tab`](https://bricenocenti.github.io/tabxplor/fr/reference/tab.md)
near the weighting paragraph. - With an overdispersed count outcome
(Pearson dispersion 2.04), a `family = "poisson"` fit returns
CIs/p-values **identical to `family = "quasipoisson"`** (SEs scaled by
`√dispersion`), and it **emits a warning saying exactly that**. At
equidispersion (≈1.0) it matches a standard `glm(poisson)` z-CI. Make se
sure the R-side
[`?tab_reg`](https://bricenocenti.github.io/tabxplor/fr/reference/tab_reg.md)
and regression vignette documents it (the jamovi side already intends it
per Last-Phase-h notes), so a user comparing to a hand-fit `glm` isn’t
surprised.

##### 2.1 MAJOR — a factor with a real `NA` *level* crashes print/format/every export

A table built from a factor that carries `NA` as an actual level (not
merely `NA` values) **builds successfully** but then **throws on
[`print()`](https://rdrr.io/r/base/print.html),
[`format()`](https://rdrr.io/r/base/format.html), and consequently every
exporter**.

``` r
library(tabxplor); library(dplyr)
d <- tibble(r = factor(c("a","b",NA), exclude = NULL), c = factor(c("x","y","x")))
t <- tab(d, r, c)          # builds fine
format(t)                  # Error: NAs are not allowed in subscripted assignments
print(t)                   # same
tab_md(t); tab_kable(t)    # same (all go through format)
```

- **Observed**:
  `Error in out[ok & tot] <- ... : NAs are not allowed in subscripted assignments`.
- **Expected**: either drop/relabel the `NA` level at build (as
  `na = "keep"` does for `NA` *values*, which works fine — see §5), or
  render it. A validly-built table must be printable.
- **Root cause**:
  [`pillar_shaft.tabxplor_fmt()`](https://bricenocenti.github.io/tabxplor/fr/reference/pillar_shaft.tabxplor_fmt.md)
  at `R/fmt_class.R:2486` —
  `out[ok & tot] <- cli::style_bold(out[ok & tot])`. When a row label is
  `NA`, the total-row detection mask `tot` contains `NA`, so `ok & tot`
  is `NA` and the subscripted assignment aborts.
- **Fix direction**: coerce the total-row mask with `tot & !is.na(tot)`
  (or `%in% TRUE`) before indexing; or normalise an `NA` factor level to
  a visible label (e.g. `"NA"`/the `na` text) during
  [`tab_prepare()`](https://bricenocenti.github.io/tabxplor/fr/reference/tab_prepare.md).
  Note `exclude = NULL` factors are the common way `haven`/imported data
  arrives, so this is reachable from real data, not only synthetic.

##### 2.2 MINOR/MAJOR — logical and Date `col_var` produce an obscure internal error

``` r
tab(tibble(r = factor(rep(c("a","b"),50)), lg = rep(c(TRUE,FALSE),50)), r, lg)
# Error in UseMethod(): no applicable method for 'n_groups' applied to an object of class "NULL"
tab(tibble(r = factor(rep(c("a","b"),50)), dt = rep(as.Date("2020-01-01")+0:1,50)), r, dt)
# same obscure error
```

- **Observed**: a cryptic `n_groups`/`NULL` error deep in the pipeline.
- **Expected**: an informative “`col_var` must be a factor, character or
  numeric — got `logical`/`Date`” message, **or** support them (a
  logical is a perfectly natural 2-level cross-tab variable, and
  [`tab_plain()`](https://bricenocenti.github.io/tabxplor/fr/reference/tab_plain.md)
  called directly *does* accept a logical `col_var` — see §6 — so
  [`tab()`](https://bricenocenti.github.io/tabxplor/fr/reference/tab.md)
  is inconsistent with its own leaf).
- **Impact**: low frequency, but the error gives the user no idea what
  to fix.

**DONE (2026-07-22).** Full suite green (FAIL 0, WARN 0, PASS 4126, SKIP
4 = the usual Suggests/benchmark opt-ins), **zero golden/snapshot
churn** — every fix fires only on the degenerate input it repairs, so
non-degenerate tables are byte-identical. New fixtures in
`tests/testthat/test-edge-cases.R` (one per fix, failing-first). All six
defects live in the shared
[`tab()`](https://bricenocenti.github.io/tabxplor/fr/reference/tab.md)/[`tab_many()`](https://bricenocenti.github.io/tabxplor/fr/reference/tab_many.md)
path (both funnel through `tab_build` → `tab_setup`). - **Bug A (NA
factor *level*)** — routed through `na=` at ONE boundary: `tab_setup()`
maps
[`forcats::fct_na_level_to_value()`](https://forcats.tidyverse.org/reference/fct_na_value_to_level.html)
over the selected factor columns (an NA *level* becomes an NA *value*,
so `na="drop"` drops it / `na="keep"` relabels it `"NA"` via the
existing machinery; a factor with no NA level is untouched →
byte-identical). Defense-in-depth: `leaf_totrow_tottab()` uses
`%in% "Total"` not `== "Total"` so `in_totrow`/`in_tottab` are always
pure logical (no NA to poison `is_totrow`/`get_reference`/`is_refrow`
and crash the `out[mask] <-` assignments in pillar_shaft/format). -
**Bug B (logical/Date col_var)** — `tab_setup()` coerces a logical
col_var to a factor before the numeric-vs-text classification (routes
through `plain_core`, matching `tab_plain`), and aborts cleanly for any
col_var that is still neither numeric nor factor/character
(Date/POSIXct/list/…). - **Clearer messages** — `resolve_ref_vector()`’s
unknown-named-`ref` warning now pins every cli `{?}` marker to
`length(unknown)` via
[`cli::qty()`](https://cli.r-lib.org/reference/pluralization-helpers.html)
(no more “Multiple quantities for pluralization”); a new guard in
`tab_setup()` mirrors the weight-collision abort for a variable used as
both a tab_var and a row/col var; `tab_prepare_pop()` aborts naming the
weight when every row is zero/NA-weighted. - **Warning leak** —
`num_core()` wraps the
digits-[`max()`](https://rdrr.io/r/base/Extremes.html) in
[`suppressWarnings()`](https://rdrr.io/r/base/warning.html) + coerces a
non-finite result to 0 (all-NA numeric col_var). - **Docs** —
[`?tab`](https://bricenocenti.github.io/tabxplor/fr/reference/tab.md) +
intro vignette: a weighted cell CI is
`Wilson(weighted p, unweighted n)`, too narrow under unequal weights.
[`?tab_reg`](https://bricenocenti.github.io/tabxplor/fr/reference/tab_reg.md) +
reg vignette: over-dispersed `family="poisson"` == quasi (warns), ==
`glm(poisson)` only at equidispersion.

#### Last Phase q – jamovi Excel export still fails

On jamovi, html and md exports work. But Excel still fails, with a new
error message this time (tell me if and how I shall give you debug
feedback, or if needed createa debug jmvexceltest analysis to test
things and I can paste you back the results) : “Export failed: ℹ In
index: 1. Caused by error in `pmap()`: ℹ In index: 1. Caused by error: !
xml import unsuccessful”

**DONE (2026-07-22), Excel export only (per maintainer: the `<hr>` is
resolved on the rebuilt version, the Run-button styling moves to Phase
r, the model-comparison freeze is out of scope).** Full suite green
(FAIL 0, PASS 4139, SKIP 4), **zero golden/snapshot churn** — the fix
only changes `syntax="excel"` numFmt literals (Excel workbooks are not
textually snapshotted; rendering is identical). - **Root cause
(reproduced locally on the jamovi-bundled openxlsx2 1.15).** The failing
call is `wb$add_numfmt()` via the numFmt `pwalk` at `tab_xl.R:883`.
tabxplor folded stars / in-cell test labels / the sd sigma / the ratio
multiply-sign into the numFmt `formatCode` wrapped in RAW DOUBLE-QUOTES
(`0.0%"***"`, `"×"#,##0.0`). openxlsx2 writes that verbatim into a
`<numFmt formatCode="…"/>` XML ATTRIBUTE; the older bundled build does
not escape the embedded `"`, so its own `read_xml` round-trip rejects
the malformed fragment (`xml import unsuccessful`). Windows-only because
the current WSL openxlsx2 (1.28) escapes it. **Not the cache** — the
same in-memory `tabs` feeds HTML/MD/Excel (byte-identical carrier), so
calling
[`tab()`](https://bricenocenti.github.io/tabxplor/fr/reference/tab.md)
without the cache would emit the same code. - **Fix.** New
`xl_numfmt_literal()` (fmt_class.R, beside `excel_numfmt_code`)
backslash-escapes each character of a literal (`\*\*\*`, `\×`, `\σ`) —
XML-safe on EVERY openxlsx2 version (no `"` in the attribute), renders
identically in Excel. Replaced the 4 double-quote-wrapping sites
(stars/label/sigma in `tab_xl.R`, multiply-sign in `fmt_class.R`); the
bare `±` was already unquoted. Fixtures: `test-xl-backend.R` (helper +
ratio-code no-quote), `test-tab_xl.R` (source codes carry no `"`),
`test-export-parity.R` (ratio code is `\×#,##0.0`). - **Follow-up (same
phase): empty summary-row cells no longer export as Excel `#N/A`.** The
older bundled openxlsx2’s `add_data` NA formal is `na.strings` (dot),
which `xlb_na_argname` did not detect (it only knew `na`/`na_strings`)
-\> our `NULL` was an unused arg -\> the default wrote `#N/A` for NA
cells on the p-value / Cramér’s V rows. `xlb_na_argname` now reads the
exact formal off the method (`na` / `na_strings` / `na.strings`). Also
`xl_materialize_data` coerces `NaN -> NA` so a NaN cell blanks instead
of `#VALUE!` (the na arg only covers NA). Reproduced + verified fixed on
the bundled openxlsx2 1.15. Fixtures in `test-xl-backend.R` (argname
stub + NaN blanking). - **Follow-up (same phase): the jamovi export
message now shows the path REALLY written, styled.** The old message
reported the requested path even when `xl_replace = FALSE` auto-numbered
the file (`Tableau.xlsx` -\> `Tableau1.xlsx`), and HTML/MD ignored
`replace` entirely (always overwrote). New shared `export_number_path()`
(R/jmvtab-export.R) is THE replace/auto-number rule — used by
`jmvtab_export()` (once, for every format) AND `tab_xl_resolve_path()`
(single-sourced). `jmvtab_export()` returns the actual (numbered) path;
`jmv_backend_export()` returns a bold green (real path) / bold red
(error) HTML status via new `export_status_html()`, prepended above
`html_table` by both `.b.R` backends (jamovi’s `Notice` has no
green/success type). Removed the now-unused `jmv_backend_notice`.
Fixtures in `test-jmvtab-export.R` (numbering, per-format replace +
returned path, status styling/escaping). The `.b.R`/`.r.yaml` are inert
until the maintainer’s rebuild; the R helpers are suite-verified.

#### Last Phase r – last display fixes

Custom html table export still have little details to fix : - With
several row_vars, the result print the row_vars names in the leftmost
column vertically : but this new column lacks a bottom border so the
whole table looks not-well-closed. This bottom border should be the same
linewidth that the rest of the table bottom border. - Remove the upper
border above variable names in all situations. With several col_vars,
even in tab_reg with empirical = TRUE and several dependent vars, ensure
there are never left and right borders between col_vars names (since
without top border here, they would look very bad).

markdown export still have a few problems on their own pandoc/quarto
html rendering : - (look at `dev/review_manual/tab_md_test_4.htm` ; code
was
`tab(gss_simple, c(race, rincome, relig), c(party3, marital), pct = "row", na = "drop_all", color = TRUE, color_signif = "grey_non_signif", ref = 1) |> tab_export("md")`) -
The first row, with variables names (here : “partx3”, “marital”), have
right and left borders in each cells, but should be like the rest of the
table (vertical borders between different col_vars only, and at
start/end) - On rows with a row variable name (here : “race”, “rincome”,
“relig”), the leftmost border dissapears just for this cell, which makes
the whole table bad looking not-closed. How to fix it ? If style code
simplification is needed here for reliability, do it.

`jmvtabreg` UI : - “Model comparison” : currently the model boxes
created with “+” (to set model name and choose predictors of each model)
do not take all the horizontal space available at their right on jamovi
option pane. It would really be better if they did, specially when there
are many predictors. - “Run comparison” button should be more visually
striking : let’s get it back to the same look than the Export button,
with white text in bold over blue background.

`jmvtab` and `jmvtabreg` UI : - Add an empty line at the bottom of each
collapsable box elements from the main outline of the jamovi options UI
(“Percentages, colors and tests”, “Levels and missing values”, “Model”,
etc. ; if it was attempted in the last improvements, it dit not appear
in Jamovi)

**DONE (2026-07-22)** `R/tab-css.R` — Last Phase r: explicit md table
LEFT edge (symmetric to the right edge; the Phase-m nbsp fill had
removed the accidental one); the html top edge is
`> thead > tr:first-child > *:not(.tx-span)` so a col_var names row
floats (no top border); `tx-bb` now also matches `td.tx-bb` (cell-scoped
bottom to close the rowspanned var-name column). `R/tab-render-html.R` —
Last Phase r: the bottom-reaching rowspanned label cell is tagged
`tx-bb` (closes the vertical var-name column’s bottom-left corner).

#### Last Phase s – Kish neff for all CI

The current documentation say contradictory things about kish_neff, and
I can’t remember exactly what was done : - In
[`tab()`](https://bricenocenti.github.io/tabxplor/fr/reference/tab.md),
with `wt =` survey weights provided (but no full survey design), is
`options(tabxplor.kish_neff = TRUE)` actually used in the calculation of
**all** confidence intervals (for factors, for means, and all of them)
? - In
[`tab_reg()`](https://bricenocenti.github.io/tabxplor/fr/reference/tab_reg.md),
is `options(tabxplor.kish_neff = TRUE)` used not only for weighted
models, but also for their observed counterpart’s confidence intervals
using `empirical = TRUE` ? - In
[`tab_reg()`](https://bricenocenti.github.io/tabxplor/fr/reference/tab_reg.md),
are **all** the selected kind of models handling well
`options(tabxplor.kish_neff = TRUE)` ? A real full survey design ?

If not, would it be easy to use kish_neff in all weighted confidence
intervals when `options(tabxplor.kish_neff = TRUE)`, or would it require
to build a complete framework for it from scratch (how much is already
given in survey::, if not using the full survey design thing, and no
design objects)?

Please enquire, then modify documentation and architecture documents to
state it clearly, then state it in a concise way in introduction
vignette
([`tab()`](https://bricenocenti.github.io/tabxplor/fr/reference/tab.md))
and regression vignette in a “Weights” section. It should start very
clearly and understandably for beginners, explaining base wt, then only
kish neff (explaining clearly what is it / what it does for beginners),
then very rapidly full survey design (refering to survey:: for more).

**DONE (2026-07-22).** Full suite green (FAIL 0, WARN 0, SKIP 4, PASS
4193). **Verification found the option was asymmetric** — kish_neff
backed only the MEAN cell CIs + the whole-table chi2/F p-value; NOT
factor/proportion CIs (a deferred open item), NOT
[`tab_reg()`](https://bricenocenti.github.io/tabxplor/fr/reference/tab_reg.md)
at all. Maintainer chose to **extend it to all descriptive CIs**. Now
uniform: `kish_neff` replaces the raw n with `n_eff = (Σw)²/Σw²` in
EVERY weighted descriptive interval — factor proportions AND means
(cell/diff/ratio + the `color="OR"` significance) in
[`tab()`](https://bricenocenti.github.io/tabxplor/fr/reference/tab.md)/[`tab_num()`](https://bricenocenti.github.io/tabxplor/fr/reference/tab_num.md),
and
[`tab_reg()`](https://bricenocenti.github.io/tabxplor/fr/reference/tab_reg.md)’s
`empirical=` companions — while the reg MODEL CIs stay design-based
(`svyglm`, untouched). Off-kish is byte-identical. - **New 19th fmt
field `n_eff`** (double, NA default, non-displayed, carried like
`tot_n`, reset to NA on arithmetic): the effective sample size used for
a cell’s CI. Full `/vctrs-field` pass in `fmt_class.R`; `get/set_n_eff`
internal. - **Factor side** (`plain_core`): a `w2` (Σw²) dcast added to
the microdata `use_raw` scan **only when opted in** (`kish && has_w2` —
the empty-scratch `w2` column that leaks as an id var is dropped, like
`wn`); `leaf_wide_pct()` broadcasts `(Σw_base)²/(Σw²_base)` into
`n_eff`;
[`tab_ci()`](https://bricenocenti.github.io/tabxplor/fr/reference/tab_ci.md)
uses `coalesce(get_n_eff, tot_n/n)` as the cell + diff base; the
`color="OR"` interval (`tab_apply_reference`/`ci_or`) swaps in the
effective base too. **Numeric side** (`num_core`): its existing `_en` is
surfaced into the same field (kish-only). The `.fine`/`tab_counts` path
has no per-obs weights → `n_eff` stays NA → raw base (documented,
correct). -
**[`tab_reg()`](https://bricenocenti.github.io/tabxplor/fr/reference/tab_reg.md)
empirical** (`reg_empirical`/`_columns`/`_tips`): a separate effective-n
(`emp_n_ci`/`emp_ref_n_ci` = `neff` when kish+weighted, else raw) feeds
the `ci_*` engines; the displayed `n` stays the raw count. No fmt
field. - **Byte-identity**: adding the field regenerated all 36
`_golden/*.rds` + the fmt-contract snapshot (verified: the ONLY per-cell
delta is the added all-NA `n_eff` column); display/export snapshots
unmoved. New `test-kish-descriptive.R` (failing-first: factor/mean/OR +
reg empirical CIs widen on-kish; displayed n + model CI unchanged;
off-kish identical; counts-data NA). - **Docs**: `?tabxplor-options`
(fixed the `FALSE (default): use Kish` wording bug + scope),
[`?tab`](https://bricenocenti.github.io/tabxplor/fr/reference/tab.md)
(test para + Weighted-CIs details),
[`?tab_reg`](https://bricenocenti.github.io/tabxplor/fr/reference/tab_reg.md)
(`empirical` honours kish), `.onLoad`/architecture/decisions §14
(factor-side open item CLOSED), NEWS. **Vignettes**: intro `## Weights`
rewritten as the wt → kish → survey ladder + fixed the
self-contradicting L217 note (“not applied to CIs” was false); reg +
programming Weights notes.

#### Last Phase w-i – tabxplor R french translation

I wonder about the possible scope of this package French translation
(the public is actually mostly French for now). Help me choose, then
make a first version of all translations : I’ll review and modify them
manually. It All legends should be carefully translated to French. What
other strings should be translated in French ? - Could the package
documentation (?`tab`, ?`tab_reg`, etc.) be translated for French users
? - Could the whole pkgdown easily have a french version, with the
possibility to choose on the webpage ? - Could the vignettes be fully
translated to french ?

**DONE (2026-07-22), Part A (runtime strings) complete + Part B
(bilingual site) scaffolded.** Full suite green (FAIL 0, PASS 4214, SKIP
4 = the usual opt-ins), **zero golden/snapshot churn** — English is
byte-identical everywhere (`gettext("X")` returns the msgid under the en
locale); French activates only under `lang="fr"` / a French locale.
**Scope decided with maintainer:** translate everything printed on a
table (legends + all display labels), NOT
[`?help`](https://rdrr.io/r/utils/help.html) pages (declined — R has no
bilingual `.Rd`), errors stay English; vignettes/README French drafts
DEFERRED (become the site’s articles); bilingual site YES. - **Runtime
translation.** The colour-legend/footer i18n machine already existed
(gettext domain `R-tabxplor`, `lang=` threaded through every exporter,
`with_legend_lang()`, FR typography); this phase (a) **filled
`po/R-fr.po`** (124 strings, careful FR terminology,
thin-space/decimal-comma typography) and (b) **extended gettext** to the
rest of the below-table surface: regression wording
(`reg_family_display_name`/`reg_model_note`/`reg_model_line[s]`/`reg_title`,
`R/tab_reg.R` — full `gettextf` templates, `reg_model_lines(x, lang)`
under `with_legend_lang`; notation OR/IRR/β kept), the `test=TRUE`
summary + GOF labels
(`test_pvalue_descriptor`/`test_es_measure`/`reg_footer_spec`,
`R/tab-test-display.R`, ambient locale) and HTML tooltips (word labels
in `tab_kable_print_tooltip`, ambient locale; pure notation left
English). The `fmt_class.R:3775` footer call passes `lg`. - **Two i18n
gotchas fixed** (both in `dev/update_translations.R`, the sanctioned
extract→normalise→ merge→compile workflow): the **dynamically**
gettext’d MEASURES words (“difference”/“ratio”/ “contribution to Chi2”)
are kept extractable by a dead-code `if (FALSE) c(gettext(...))` anchor
beside `legend_measure_word()`; and potools’ `\uXXXX` escapes (from the
ASCII-source rule) are normalised to real UTF-8 so the `.mo` key matches
R’s runtime `gettext`. New `tests/testthat/test-i18n-fr.R` locks FR
rendering + the English-untouched guard; `dev/french_glossary.md`
records the terminology. - **Bilingual pkgdown scaffold** (Part B):
`_pkgdown.fr.yml` (`lang: fr`, translated navbar + reference group
titles/desc, EN↔︎FR switcher) + `dev/build_site_bilingual.R`. Reference
PAGES stay English (help not translated); **French articles = the
deferred vignettes** (consider `babeldown` there), so the FR site’s
narrative is English-under-a-French-shell until the vignette phase
lands. - **Deferred to a follow-up (w-ii):** French vignettes + README,
and the polished French site content. Known first-draft rough spots (reg
caption English colon, comparison-title FR plural) documented in
`dev/french_glossary.md` for maintainer review.

#### Last Phase w-ii – vignettes french translation

French vignettes + README (they become the site’s French articles), then
a real dev/build_site_bilingual.R run

Add in vignettes : - How to use ref with several variables (depending on
“row” or “col” pct) ? - Present the base options in the vignettes ? THe
really standard ones in introduction vignette, the more complex and
expert ones in programming vignette (some in regression vignette if
appropriate).

**DONE (2026-07-22).** The three vignettes are now shipped in French as
**web-only pkgdown articles** (`vignettes/articles/*-fr.Rmd`,
`.Rbuildignore`’d via `^vignettes/articles$` → never on CRAN), and the
real `dev/build_site_bilingual.R` runs green: `docs/` (EN) + `docs/fr/`
(FR) both build, each article renders in its own language (FR articles
set `options(tabxplor.lang = "fr")` in setup → French legends/ footers
verified in the built HTML; code chunks byte-identical to the English
source, argument names + column labels kept English per the glossary).
**README skipped** (maintainer choice — the FR site home keeps the
English README; the three FR articles carry the French narrative).
`docs/` was built to verify and left **uncommitted** (untracked, not
`.gitignore`’d → Phase z publishes it). - **New English content**
(mirrored in French): the intro’s colour-reference section gains a
“different reference per variable” subsection (`pct = "row"` → a
per-row_var **named** `ref` picks a reference **row**; `pct = "col"` →
`ref` vectorised over col_vars, **named or positional**, picks a
reference **column**); a “Session options” section (everyday
[`options()`](https://rdrr.io/r/base/options.html)) in the intro; an
“Advanced options” section (export fonts / parallel / jamovi) in the
programming vignette; a `tabxplor.anova` note in the intro test section.
Also corrected the reg vignette’s **stale column labels**
(`Emp. %`→`Obs_%`, `Model OR`→`Model_OR`, `Emp. OR`→`Obs_OR`,
`Model AME`→`Model_AME`, `Emp. diff`→`Obs_diff`, and the `adjusted %`
prose → the parenthesised value in `Model_AME (adjusted %)`; Last Phase
g renamed them), and removed a dead hidden chunk referencing the
Phase-17h-cut `predicted_unadjusted`. - **Wiring**: `_pkgdown.fr.yml`
`articles:` points at the `articles/*-fr` slugs (French leads, English
in an “In English” group); `_pkgdown.yml` mirrors it (English leads, “En
français” group); `.Rbuildignore` + the build-script header updated. -
**Three pre-existing `_pkgdown.yml` bugs fixed** (surfaced by the
FIRST-EVER site build): the dead `- "%>%"` reference entry (magrittr is
gone — base `|>` only); an **incomplete `articles:` index**, which
pkgdown 2.2.1 treats as a HARD ERROR (not the benign warning the roadmap
assumed — it builds every article into BOTH trees, so each config must
index all six); and `set_caption` (exported in Phase 17b) missing from
the reference index. - **Flag for maintainer**: a pre-existing
`\Documents` unknown-Rd-macro warning in `man/jmvtab.Rd:258` +
`man/jmvtabreg.Rd:159` (should be `\\Documents` or escaped in the
roxygen source — harmless, not fixed here). The three
`dev/french_glossary.md` runtime-string rough-spots (reg-caption colon,
comparison-title plural, ambient-locale tooltips) still await review —
they are NOT vignette prose. Translations are **first drafts** for the
maintainer’s hand review.

#### Last Phase x — Jamovi UI French translation (DONE)

#### Last Phase y – NEWS.md simplification

`NEWS.md` `# tabxplor 2.0.0 (in development)` section have accumulated
all dev history of the new version, must most of it is really not
user-facing and irrevelant (and already in other dev documentation). A
**drastic** reduction is needed here, no dev details **at all**,
straight to the point, please **reduce 1150 lines to maximum 100
lines**, divide it by 10 : - “## New features” should only list the most
important things. New exported functions, like tab_counts() and others,
should be presented in one quick sentence. New arguments in one quick
sentence, rarely more. Everything about
[`tab_reg()`](https://bricenocenti.github.io/tabxplor/fr/reference/tab_reg.md)
should be near-zero-words : in spirit, “possibility to do regressions
added, see the vignette”. - Remove “## Internal” and “##
Documentation” - Drastically reduce “## Changes that may affect existing
code”, only keep what is really important - Drastically reduce bug
corrections and bug fixes (same thing really), to only speak about very
very few bugs that could have been hit by real user. Remove everything
about any new argument or implementation. Make it small. - Keep
deprecation, reduce it’s size, list elements quickly, but differenciate
what is soft deprecated and what is hard deprecated (two different
sections).

#### Last Phase z – github PR and CRAN release

Help me do the github PR.

I want the master github branch to get rid of `dev/` and other not
user-facing files, while still keeping them in a branch for development
and future bug fixes (the branch I want to use in Positron, since master
is more user-facing). What would be the best wax to proceed ? Should I
just keep two branches in parallel, master and dev, and PR to master
before releases ? What are the good practices in that matter, for
simplicity and reliability ?

### Reference — bugs, benchmarks, perf

Fixed bugs recorded in `dev/tabxplor_2.0.0_roadmap_DONE_PHASES.md`

#### Benchmarks (`dev/benchmarks/`)

The performance harness lives in `dev/benchmarks/` (`.Rbuildignore`’d).
Per the scope decision, save every phase’s before/after runs under
`dev/benchmarks/results_2.0.0/`.

⚠ **Every committed baseline below was measured on WINDOWS/NTFS. Dev is
now WSL2 Ubuntu on ext4 — do NOT diff a WSL2 run against them.**
Affected: `dev/benchmarks/baseline.csv`,
`tests/testthat/benchmark_baseline.csv`,
`jmvtab_benchmark_baseline.csv`, `jmvtab_big_benchmark_baseline.csv`,
plus every absolute timing quoted in the roadmap phases above (`~46s`
suite, `225s -> 56s` parallel, the §26 parallel PoC, the Phase
5/7f/9b/10 speedups). The *ratios* within a single run stay meaningful;
the absolutes do not cross the platform boundary. Nothing fails —
benchmarks are opt-in (`TABXPLOR_BENCH=true`) and `test-benchmark.R`
never fails — so this is a **silently misleading comparison**, not a
broken test. Re-baseline consciously on ext4 before drawing any
conclusion, and note the platform in the file when you do.

⚠ **The 8M fixtures are NOT in WSL2.** Migration Phase A1 ruled
`big_df.rds` (161 MB) and `big_pc18_full_15M.rds` (572 MB)
*reproducible* and deliberately did not copy them (`.gitignore`:
*“Generated benchmark fixtures: large, regenerable, never commit”*;
`gen_big_df.R` is tracked). The first `run_bench.R` therefore
**regenerates the fixture first** — expect a long, one-off build, not a
hang. The 13 loose `dev/benchmarks/results_*.csv` WERE copied;
`results_2.0.0/` is tracked.

- `run_bench.R` — heavy 8M-row
  [`tab()`](https://bricenocenti.github.io/tabxplor/fr/reference/tab.md)
  harness: `source("dev/benchmarks/run_bench.R")`. Compares to
  `dev/benchmarks/baseline.csv`; writes `results_<stamp>.csv`
  (git-ignored).
- `run_fused_vs_bytable.R` — fused vs table-by-table arbiter on a 15M
  fixture (the `.by_table` flag). *(OBSOLETE since Phase 9c removed the
  tab()-level factor fusion — `.fine` now only reaches `tab_plain` via
  jmvtab /
  [`tab_counts()`](https://bricenocenti.github.io/tabxplor/fr/reference/tab_counts.md).)*
- `gen_big_df.R` — deterministic 8M fixture builder (cached to
  `big_df.rds`, git-ignored).
- `baseline.csv` — committed 8M baseline; reset consciously after a
  deliberate perf change.
- `tab_many_performance_profile.md` — the full 2026-07 profile (read
  before optimizing).
- In-suite counterpart: `tests/testthat/test-benchmark.R` (small
  `gss_cat`, informational, never fails, vs committed
  `tests/testthat/benchmark_baseline.csv`; regen via
  `dev/make_benchmark_baseline.R`). `bench` is Suggests-only (falls back
  to `system.time`).

#### Perf findings (condensed — full profile in `dev/benchmarks/tab_many_performance_profile.md`)

- **`tab_chi2` is the \#1 cost** (84% of a small 9-tab call;
  N-independent, scales with *cells*) → the reason CI/chi2 move onto the
  aggregate in Phase 3.
- Per-table fixed fmt/vctrs overhead (~0.19 s/table) dominates over the
  scan; `tab_num` double-scans N and weighted `tab_num` allocates ~7.8
  GB (`weighted.var` recomputes the mean) → Phases 1-3.
- Scan-fusion — the tab()-level opt-in
  (`options(tabxplor.fuse_min_rows=)` + the fused block in
  `tab_aggregate`) was **removed in Phase 9c** (§30): a NET NEGATIVE
  (+1–7 %) once the build is O(cells) / N-independent, so fusing the
  O(N) scan buys nothing at survey scale. The
  `.fine`/`.by_table`/`fine_for_pair()`/`use_raw` seam **remains** as
  the jmvtab-cache aggregate-injection seam (+
  [`tab_counts()`](https://bricenocenti.github.io/tabxplor/fr/reference/tab_counts.md) +
  numeric `fine_num`); `test-fuse-parity.R` now drives
  `tab_plain(.fine=)` directly.

------------------------------------------------------------------------

## The last step of every implementation : Update instructions and relevant development files

After verification passes, always :

1.  Ensure the file-header docstring/comment of any modified module is
    still accurate. Update or add `# DESIGN:` / `# WARNING:` tags next
    to changed logic.
2.  Keep the tabxplor version 2.0.0 roadmap in CLAUDE.md and
    `dev/tabxplor_2.0.0_decisions.md` up-to-date as you build it or
    implement it.
3.  Update `dev/tabxplor_architecture.md` whenever you modify the
    package structure for real (add modules, rename functions, change
    config fields). Do not add clutter and useless details. When there
    is nothing to change, skip it. Update other `dev/*md` file when
    relevant.
4.  For package structure and architecture, also add the relevant
    CLAUDE.md update lines in your response : it should be minimalistic,
    concice, no bullshit, with nothing useless that would clutter the
    prompt, since the details are already in
    `dev/tabxplor_architecture.md`. When there is nothing to change,
    skip it. Maintainer will move done phases to
    `dev/tabxplor_2.0.0_roadmap_DONE_PHASES.md` himself.
5.  `NEWS.md`: user-facing and CRAN-facing, tracking new functions, new
    arguments and arguments changes, deprecations, and important bugs
    fixes. Keep it minimalistic and no bullshit. Do not edit it when
    it’s not necessary.
6.  (`README.Rmd` : user manual. Only update before release of new
    version to CRAN, never before.)
