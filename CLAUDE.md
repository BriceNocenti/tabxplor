# tabxplor — AI Assistant Guide

## Package Purpose

`tabxplor` is a public CRAN R package (v1.3.1) maintained by me for creating, manipulating, and formatting color-coded cross-tabulations. It uses `data.table` for fast aggregation internally and exposes a `tidyverse`-compatible API via custom `tibble` subclasses. Tables can be exported to Excel (with colors), HTML, Markdown, and ggplot. All numeric cells are stored as `vctrs` record vectors (`tabxplor_fmt`) carrying full statistical metadata, enabling lossless display switching.

---

## Repository Map

```
R/
├── fmt_class.R     (~4400 L) Core type: tabxplor_fmt vctrs record, getters/setters, new_fmt() +
│                              fmt_field_names (the 18 fields) + DERIVED fmt_col_attrs (17a: moved here
│                              from tab.R, = new_fmt formals minus the fields, so it can't miss an attr);
│                              format/pillar methods, vctrs arithmetic/casting,
│                              color engine (fmt_color_plan/fmt_color_slots/fmt_color_channels;
│                              per-side fold + findInterval; slots 1-4 over / 5-8 under);
│                              colour legend + footer (16e ONE model): MEASURES table = per-measure facts
│                              (word/glyph/ref_kind/unit/has_ref_lead, one row not ~5 switch arms) ->
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
├── tab.R           (~7150 L) Main API: tab(), tab_many(), tab_plain(), tab_num(),
│                              tab_apply_reference() (Phase 7f carve; Phase 9d: matrix-sweep internals;
│                              14z: also the empirical-OR Woolf CI [ci_or on the {level j, ref2 level} x
│                              {row i, ref row} 2x2, gated by tabs_totn!=NULL = a color_signif/stars ask;
│                              tab_plain threads conf_level/stars/color_signif] so color_signif works on OR),
│                              leaf_wide_pct() + build_total_rows()/finalize_total_rows() (Phase 9d:
│                              base-R/matrix leaf math for tab_plain pct/tot_n + total rows),
│                              tab_prepare(), tab_pct(), tab_ci(), tab_chi2(),
│                              tab_tot(), tab_totaltab(), tab_spread(), tab_get_vars(),
│                              tab_render_vars() (Phase 10c: robust group_vars-based role detection +
│                              graceful degrade, used by print + exporters),
│                              tab_add_n_pct() (shared add_n/add_pct, used by tab_many + tab_counts).
│                              tab_build() = staged pipeline: tab_setup / tab_prepare_pop / tab_aggregate
│                              / tab_build_tables (Phase 9a: the OUTER row_var map -> tab_build_one, +
│                              tab_rowvar_ctxs) ; tab_transform / tab_assemble_tables are SCALAR over one
│                              row_var ; tab_assemble_output (merge/pvalue/unwrap);
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
├── tab-counts.R     (~360 L) tab_counts() from-the-middle constructor (Phase 4): reshape any
│                              input shape → count-aggregate → tab_plain(.fine) + shared finalize
├── tab-resolve.R    (~200 L) tab_resolve_settings() (Phase 7b): the ONE pure arg-overwrite
│                              cascade (color="auto"/forcing/split) shared by tab_build+tab_counts;
│                              resolve_color_auto_num() (numeric arm); emits ci_scale (14b: "ratio"
│                              = the Katz interval). The jmvtab .js / cache boundary.
├── tab-parallel.R   (~200 L) Phase 8/9a row-axis dispatch (Suggests-only mirai): tab_pmap() + trampoline,
│                              named "tabxplor" pool (tab_pool_ensure/tab_parallel_workers/
│                              tab_parallel_stop), tab_build_one() (the per-row_var worker, serial OR mirai).
├── tab_classes.R   (~3700 L) tabxplor_tab/grouped_tab classes, 30+ dplyr S3 methods,
│                              print methods, tab_kable(), tab_plot(), tab_compact(),
│                              OKLCH color palettes, set_color_palette()/get_color_style(),
│                              set_color_breaks() (over/under scales), color_breaks table attr;
│                              Phase 13c-iv tabxplor_tabs (multi-table LIST class: print/[/c/knit_print,
│                              auto-print + Viewer routing); tab_materialize_extras (+ xl mean/_sd col)
├── tab_xl.R        (~595 L)  Excel export via openxlsx2 (Suggests-only; Phase 10h). Single-tab-first
│                              + list. tab_xl() orchestrator -> tab_xl_plan_one() (pure per-table plan:
│                              raw values + numFmt codes w/ stars + a precomposed per-cell STYLE grid
│                              via xl_build_styles) -> xl_write_table() (writes values, then
│                              xl_apply_styles = register deduped fonts/fills/borders + composed xf,
│                              apply by id with set_cell_style, then the numFmt merging pass). Consumes
│                              tab-export-prep (roles/refs/bold) + format(syntax="excel"); transpose
│                              arg; conditional_format experimental; n_min/hide_near_zero inert.
│                              Phase 13c-v: xl_materialize_data (ci-cell/OR text columns; or_numeric
│                              arg), +/x/sigma numFmt, mean/_sd twin col, col_var span header + geometry
├── tab-xl-backend.R (~110 L) Phase 10h openxlsx2 backend: plumbing xlb_* engine wrappers (in-place R6
│                              $, +xlb_merge) + the pure range coalescer (xl_runs/xl_coalesce -> fewest
│                              multi-area dims). Styling-model notes (precompose + set_cell_style path).
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
│                              aborts. Object-level tab_transpose() soft-deprecated. See decisions §46
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
├── utils.R         (~945 L)  .onLoad() options setup, factor/list utilities, tx_str_wrap/tx_str_trunc
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
├── tab_reg_plots.R  (~230 L) Phase 12h display: or_plot() (finalfit-style OR forest plot ON a
│                              tabxplor_tab -- reads fmt fields, NO refit; gridExtra 2-panel) + lm_plots()
│                              (ggplot2 2x2 glm/lm diagnostics). ggplot2+gridExtra guarded (Suggests).
├── jmvtab-cache.R  (~800 L)  jmvtab live multi-tier cache: content-addressed store + hashing +
│                             jmv_cache_aggregate (tier 1-2, tab_aggregate hook) + the Phase 7f
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
│                             conditionMessage() surfaces the real cause (not "In index: 1.")
├── jmvtab.b.R       (~200 L)  Jamovi module backend (R6): thin orchestrator over jmvtab_build + $state
├── jmvtab.h.R       (605 L)  Jamovi module UI (auto-generated, do not edit)
├── jmvtabreg-cache.R (~290 L) Phase 15b: the jmvtabreg (Regressions) live-UI fit cache +
│                              jmvtab_reg_build() engine-free core (drives tab_reg(.fit_cache=)). Own
│                              2-tier store (digest / fit), byte-bounded LRU, reuses jmv_hash/jmv_col_fp;
│                              jmvreg_cached / jmvreg_fit_key (ref-INDEPENDENT digest key -> a reference
│                              change is a HIT) + the picker folders jmvtab_reg_ref_vector (reference),
│                              jmvtab_reg_models (15b-ii "+" builder -> `predictors` list / flat pool),
│                              jmvtab_reg_mult_vector (numeric scaling -> `multiplier`). 15b-ii raised
│                              the raw-fit ceilings (fit 4->24MB, store 16->96MB) so comparison fits (a
│                              raw reg_fit ~9-11MB) cache instead of graceful-skipping. 15d: the
│                              per-dependent Model table (depFamily/depModelLevel/depTrials) ->
│                              jmvtab_reg_dep_family/_dep_modelled_first/_dep_trials. 15e: jmvtab_reg_build
│                              calls tab_reg() ONCE with per-dependent family/inverse/trials VECTORS -> one
│                              mixed-family table (no more group-by-family / tabxplor_tabs stacking)
├── jmvtabreg.b.R   (~140 L)  Phase 15b: jmvtabreg R6 backend (thin orchestrator, sibling of jmvtab.b.R;
│                              .h.R generated by prepare() -- inherit is lazy so it loads before then)
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

> **This is the *current* pipeline. 1.4.0 rewrites it around a single aggregate-core** (see roadmap § Keystone + `dev/tabxplor_1.4.0_decisions.md`): the step chain `tab_pct → tab_ci → tab_chi2 → …` collapses into one core, and `tab_pct`/`tab_tot`/`tab_ci`/`tab_chi2` become superseded thin wrappers.

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
| Mean-diff asymmetry      | For `type="mean"` columns, the `diff` field stores a **ratio** (cell_mean / ref_mean), NOT a difference. Thresholds like 1.15 mean "+15% above reference". This asymmetry propagates into `color_formula()` and `format.tabxplor_fmt()`. **(1.4.0 §3: numeric `diff` becomes a real difference; the ratio moves to the `ratio` field — the never-used `rr` field renamed, placed after `diff`.)**                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     |
| tab_reg                  | Phase 12c–12g LIVE: unified regression tables (gaussian beta / binomial OR / poisson IRR / multinomial OR / ordinal cumulative OR) over lm/glm/svyglm/svyolr/svy_vglm/nnet::multinom/MASS::polr + broom (no parsnip). tab_logit/multi_logit are binomial wrappers. Effect shape is exponentiate-driven: additive beta -> `diff`+type="coef"+display="coef"+ci_type="diff"; multiplicative OR/IRR/cumOR -> `or`+type="row"+ci_type="or". No new fmt fields/attributes: `type` gains value "coef", `display` gains token "coef", the `var` field carries var(Y). 12d: MNL = one OR col per outcome category vs ref; ordinal polr + Brant PO diagnostic. 12e: orthogonal `effect="ame"` (marginaleffects) + `at="reference"` profile axis. 12f: model-summary footer + compare= in the `test` attr. 12g: SURVEY designs — `wt=`/`ids=`/`strata=`/`fpc=`/`nest=` + a prebuilt survey.design/svyrep.design as `data`; reduced weighted glance (Wald/Nagelkerke/Cox-Snell/Rao-Scott-AIC) + weighted compare (anova.svyglm Wald); weighted 3+ level (svyolr / svyVGAM); `split_var` (tab_vars analogue, tab_spread-able); `multiplier` (OR^k); `empirical_OR` (crude %/OR beside model OR, binary). No new fmt fields; new Suggests svyVGAM. |


---

## Design Decisions

### Type System

- **`tabxplor_fmt`**: vctrs record (`new_rcrd()`) with **18 per-cell fields** (was 15 before v1.4.0 Phase 1a) and **10 per-column attributes** (Phase 10i-A dropped `display_spec` → 9; Phase 15e added `model_family` → 10). The critical distinction: fields vary per cell (accessed via `vctrs::field()`), attributes are scalar describing the whole column (accessed via `attr()`). Constructor chain: `fmt()` (public, validates + coerces) -> `new_fmt()` (internal, calls `vctrs::new_rcrd()`). *(Phase 1a reshaped 15→18 in one combined pass — decisions doc §9; `ci` is now derived from the `ci_inf`/`ci_sup` bounds by `get_ci()`, a bounds-shim.)* The 10th attribute **`model_family`** (Phase 15e; `get/set_model_family`, `""` on cross-tables) is a regression column's own family, so one `tab_reg()` table can mix several dependents of different families and the colour legend names each column's effect (OR / IRR / β / AME) from the column itself, not a scalar `reg_meta`.
- **`mean` field is mean-only** (the old overload is GONE — Phase 5 landed): `mean` now carries an actual mean only on `type=="mean"` columns; for **pct-type** columns it is `NA` and the cell/reference **ratio** (the "*2 rule") lives in the dedicated **`ratio` field** (Phase 1a renamed the never-used `rr`→`ratio`). The build writes `mean = NA_reals, ratio = <ref-relative ratio>` for pct columns (`tab.R` ~L3608) and the colour engine reads `get_ratio(x)` (`fmt_class.R` ~L2688). *(c-iii audit 2026-07-19 confirmed no field/attribute consolidation is both safe and worthwhile — the 18 fields are all user-contract and none vestigial; the column attributes — 9 then, now 10 with Phase 15e's `model_family` — are exported getters AND required per-column so `format()`/colour work on a standalone extracted column.)*
- **`tabxplor_tab`**: tibble subclass via `tibble::new_tibble()` with **7 table attributes**: `subtext` (legend text), `test` (chi2/ANOVA-F results tibble; §16 hard-rename of the old `chi2` attribute), `render_extras` (Phase 10i-B, the `list(add_n=, add_pct=)` display intent), `ci_settings` (Phase 13b, the CI method/confidence level metadata the colour legend names; kept distinct so it survives footer materialisation), `vars` (Phase 14d, variable roles), `empirical_tips` (Phase 14v, the multinomial crude-companion tooltip data) and `reg_meta` (Phase 14w, a reg table's model record: family/effect/dependent/reference/predictors, driving its title + "Model:" legend line + colour-legend wording), all carried through dplyr verbs by the S3 methods + vctrs reconcilers (one line each in `tab_attrs()`).
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

Note: `ref` is **reinterpreted by `pct`** — a reference **row** under `pct="row"`/means, a reference **column** under `pct="col"`. 1.4.0 makes `ref` a per-row_var named vector (row%/means only) and stores each cell's own base as `tot_n` — see decisions doc §2, §4.

### Color System (3-layer)

1. **Palettes** (`tab_classes.R` ~L2892): 6 named color vectors (dark/light text, 24-bit blue-red/green-red, dark/light background), each with 11 hex codes: `pos1`-`pos5` (over-represented), `neg1`-`neg5` (under-represented), `ratio`. Hues are hand-tuned so intensity levels are eye-distinguishable on real tables; 8-bit variants target non-truecolor terminals; the 24-bit blue-red variant is more colorblind-friendly than green-red (fuller colorblind support is a future goal).
2. **Breaks** (`set_color_breaks()` in `tab_classes.R`): stored in `options("tabxplor.color_breaks")`. Default pct: `c(0.05, 0.1, 0.2, 2, 0.3)` — the `2` means "twice the reference" (ratio mode). Mirrored for negative. Mean breaks: `c(1.15, 1.5, 2, 4)` — always ratios. *(1.4.0 §18 adds `mean_diff_breaks` `c(0.2, 0.5, 0.8, 1.2)` — sd-standardized differences for the numeric diff mode, Phase 5.)*
3. **Selection** (the Phase-5 `findInterval` engine in `fmt_class.R`: `fmt_color_plan` → `fmt_color_slots` → `fmt_color_channels`/`fmt_channel_codes`, the shared artifact every backend consumes; the old `fmt_color_selection`/`keep_last_break` are gone): per-side fold + `findInterval` over the break scale picks the strongest matching threshold per cell. Different measures per color mode: `diff`, `diff_ci`, `ci`, `after_ci`, `contrib`, `OR` (+ the 1.4.0 additions `ratio`/`diff_ratio`, Phase 5).

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

⚠ **`OMP_NUM_THREADS=1` is NOT optional, and `TESTTHAT_CPUS=8` alone is a trap.** Root-caused
2026-07-16 (second session lost to it). `Config/testthat/parallel: true` runs each test file in its own
PROCESS, and **each process then multi-threads on its own**:

| thread source                                | per worker | x 8 workers | lever                                                |
|----------------------------------------------|------------|-------------|------------------------------------------------------|
| data.table (defaults to 50 % of cores)       | 6          | 48          | `setDTthreads(1L)` — now in `tests/testthat/setup.R` |
| OpenBLAS *pthread* build (`lm`/`glm`/ggplot) | ~10        | ~80         | `OMP_NUM_THREADS=1` **in the env before R starts**   |

**Measured: 165 threads on 12 cores (~14x oversubscribed) -> the suite ran >26 min instead of ~50 s**,
two workers pegged at ~485 % CPU while the rest starved and the log went silent for 10 min. With both
levers: **47 threads, 48.9 s, FAIL 0.** OpenBLAS fixes its thread count at **library init**, so
`setup.R` is too late for it — it MUST be an env var on the `Rscript` command (workers inherit it).

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
| `test-tab_reg-plots.R`   | Phase 12h: or_plot() / lm_plots() smoke tests (build a gtable without error)                    |

---

## Jamovi module development

tabxplor currently use jamovi `2.6.44.0` (solid). Version 1.4.0 will also be tested on jamovi current "solid" version `2.7.37` afterwards (Phase 7i confirmed 2.7.37 ✓).

✅ **jamovi IS installed in this WSL2 distro (migration Phase C3, 2026-07-16): flatpak `org.jamovi.jamovi` 2.7.36, bundled R 4.5.0.** Launch it with **`jamovi`** (the `~/.local/bin/jamovi` wrapper — never bare `flatpak run`, see below). The module builds with `jmvtools::install(home = "flatpak")` in ~2 min, and Crosstables is verified running on real data.

✅ **The six "OPEN — maintainer step: regenerate `jmvtab.h.R`" items (Phases 7a, 7e, 7g-i, 7g-ii, 7g-iii, 7h) are CLOSED** — one `jmvtools::prepare()` covered all of them, and the compiled **`uijs` blob** means those UI changes are live in a running app for the first time.

⚠ **`prepare()` proved the hand-edited `.h.R` had a latent bug**, so do not hand-edit it again. `R/jmvtab.h.R` was hand-mirrored to the YAML across ~7 commits; the compiler reproduced 778 of its 780 lines but corrected `exportExcel` (`type: Action`) from `NULL` → `FALSE` **and gave it a default it lacked** — without which `tabxplor::jmvtab()` called from R throws. The never-edit rule earned its keep.

⚠⚠ **`ELECTRON_RUN_AS_NODE` — do not debug jamovi without knowing this.** Claude Code/Positron export `ELECTRON_RUN_AS_NODE=1`; flatpak passes it into the sandbox and jamovi's Electron runs as **plain node** → **exit 0, no window, no error**, and `jmvtools::install()` dies `"bad option: --install"` (rc=9). `flatpak run --unset-env=` is NOT enough (zypak re-spawns children via the host); only `env -u` on the host works — which is what the `jamovi` wrapper does. In R: `Sys.unsetenv("ELECTRON_RUN_AS_NODE")` before `jmvtools::install()`. ⚠ `jmvtools::check()` passes regardless — it never reaches Electron — so a green `check()` proves nothing here.

⚠ **WSLg is in COPY MODE** (known WSL 2.7.x bug [microsoft/WSL#40618](https://github.com/microsoft/WSL/issues/40618)): windows can be slow or render blank (taskbar entry + penguin icon, `[WARN:COPY MODE]` in the title). **Not a jamovi problem** — plain `xmessage` fails identically. One-time fix, persists across reboots: `sudo mkdir -p /mnt/shared_memory && sudo mount -t tmpfs tmpfs /mnt/shared_memory`. ⚠ The bug is *unstable* — it sometimes renders fine without the mount, then regresses; a working window is not evidence the mount is unneeded.

⚠ **There are now TWO build paths, and they are not interchangeable — `.jmo` bundles are platform-specific** (migration Phase A1):

| Target                               | jamovi                                                  | Checkout                                                                    | Recipe                                                                                                                                                         |
|--------------------------------------|---------------------------------------------------------|-----------------------------------------------------------------------------|----------------------------------------------------------------------------------------------------------------------------------------------------------------|
| **Linux `.jmo`** (WSL, the dev path) | flatpak `org.jamovi.jamovi` **2.7.36 ✅ installed (C3)** | `~/github/tabxplor` — **authoritative for source**                          | `jmvtools::install(home = 'flatpak')` (setup doc §7.4; the SDK `org.freedesktop.Sdk//24.08` is REQUIRED — `flatpak run --devel` is how the compiler reaches R) |
| **Windows `.jmo`** (release only)    | Windows jamovi, **kept forever**                        | `D:\Statistiques\github\tabxplor` — **build-only: pull, build, never edit** | `options(jamovi_home='C:/Program Files/jamovi 2.6.44.0'); devtools::load_all(); jmvtools::install(); devtools::load_all()`                                     |

**A Linux jamovi cannot produce a Windows bundle**, so the Windows checkout survives *even if C3 fully succeeds* — this is not a C3-failure fallback. The rule that matters: **never edit tabxplor in both places.** Edit in WSL, pull on Windows, build there.

✅ **`jmvtools` is pinned to 2.7.26** (C3). ⚠ Never `install.packages("jmvtools", repos="https://repo.jamovi.org")` — that index serves 2.7.26 **and** 28.0-28.3, so R takes **28.3**, whose newer compiler can emit a `jms` version 2.7.36 refuses. Reinstall with the explicit tarball: `install.packages("https://repo.jamovi.org/src/contrib/jmvtools_2.7.26.tar.gz", repos = NULL, type = "source")` (install `node` from that repo first — `repos = NULL` resolves no deps).

⛔ **The 2.6.44 flatpak is GONE** (C3): Flathub retains only 4 commits, back to 2.7.29 (2026-05-12); 2.6.44 was built 2025-03-06 and is pruned. **2.6-solid compatibility is verified on Windows only** — via the build-only Windows checkout, which is kept forever regardless.

To know the real structure of the final .html and .js, check at this live capture done from dev console (for a basic table) :
- `dev/jamovi/dev_console_live_capture/Jamovi_tabxplor_1_3_1_basic_table.html` : the live html from tabxplor 1.3.1 jamovi module
- `dev/jamovi/dev_console_live_capture/127.0.0.1_56680_MAIN_ELECTRON/` : the exported main election scripts
- `dev/jamovi/dev_console_live_capture/127.0.0.1_56683_tabxplor_jmvtab_analysis_UI/` : the exported tabxplor jmvtab analysis UI scripts
- `dev/jamovi/dev_console_live_capture/127.0.0.1_56684_results/` : the exported jamovi "results" panel scripts (where the actual table appears)

To **capture new html** in the dev console, **ask the maintainer whenever you need**.

Look at `dev/tabxplor_1.4.0_jamovi_dev.md` and `@dev/jamovi/` for detailed informations.


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

## tabxplor version 1.4.0 roadmap : the current goal

Currently implementing tabxplor 1.4.0 (2.0.0 only if breaking changes land). **Update the sections below at the end of every work session.**

Phases already implemented can be found in `dev/tabxplor_1.4.0_roadmap_DONE_PHASES.md` Only phases not yet finished appear below.

### The aim of 1.4.0 — read first, it governs every decision

This version exists to **refactor and simplify `tab()`/`tab_many()`** — the two functions that matter — by **stripping the white-elephant flexibility that real-world data analysis never uses**, and **redesigning the underlying `tabxplor_fmt` vctrs-field architecture** (one combined field pass) to fit the simpler, faster model. The governing rule, non-negotiable:

- **Public API stays retro-compatible.** User-facing functions, their arguments (soft-deprecate, never hard-break), and the `tabxplor_fmt` fields users read with `$`/`mutate()` keep working.
- **Internals are redesigned as radically as needed** for consistency, simplicity, and performance. Do **not** preserve internal structure, dead code, or the old step-by-step (`tab_pct`→`tab_ci`→…) paths for their own sake — remove them, fuse them, route everything through the one aggregate-core. Whenever a choice trades never-used internal flexibility for a single well-defined faster path, take it.

Every phase and decision below serves that aim: fewer knobs, one computation core, a field set shaped to the real use cases.

### Start here

This roadmap is the **plan of plans**: the phased implementation order plus every open question. A fresh session asked for a *part* of the work should read, in order:

1. **This roadmap** — the phase your task belongs to, its bullets, and its pointers
2. **`dev/tabxplor_1.4.0_roadmap_DONE_PHASES.md`** – the detailed report of all the **already implemented phases of the roadmap**.  
3. **`dev/tabxplor_1.4.0_decisions.md`** – the **new architecture decisions** taken for version 1.4.0. Some parts of the file may be outdated :
4. **`dev/tabxplor_architecture.md`** — architecture guide (type system, pipeline, compaction loss, exporters). It describes the **current** architecture. Read the section matching the file you touch.
4. **Top of this CLAUDE.md** — Repository Map, Global Architecture, Key Constraints, Design Decisions.

**Other long-form 1.4.0 docs live in `dev/` (all `.Rbuildignore`'d), never inline here — read the matching ones before you start:**
- `dev/benchmarks/` — performance harness + saved results (documented under *Reference > Benchmarks*). Read/run when a phase touches perf (Phases 2, 3, 6, 8).
- `dev/benchmarks/tab_many_performance_profile.md` — the full 2026-07 profile. Read before optimizing `tab_many` / `tab_chi2` / `tab_num`.


#### Verification (every phase)

- **Byte-identity**: `devtools::test("~/github/tabxplor")` after each phase; `test-golden.R` + `test-export-parity.R` + `test-fmt-contract.R` + `test-fuse-parity.R` stay green. Intentional output changes → rerun `dev/make_golden.R`, review the `_golden/`/`_snaps/` diff consciously, `testthat::snapshot_accept()`.
- **Release gate**: `devtools::check()` (~3 min, run manually by maintainer) before CRAN.

---


### tabxplor Phase 17 — ecosystem integration roadmap (end of v1.4.0)

This is the plan of plans for the last development stretch of v1.4.0, implementing `dev/tabxplor_ecosystem_simplification.md` (the six-audit design analysis, reviewed and decided by the maintainer on 2026-07-20). Phases group the tasks that need the same systemic understanding of the same code region, so a session builds that understanding once (with search agents) and spends it fully. Respect its order.

**Precedence rule for the analysis doc**: where §5/§9 of `dev/tabxplor_ecosystem_simplification.md` contradicts its §6 table or its "Maintainer choices" (both edited by the maintainer), **the §6 table and Maintainer choices win**. The reconciled rulings are §Settled decisions below — implement those, not the stale §5/§9 lines.

The release freezes every surface this roadmap touched — anything in §Settled decisions marked "now" that has not landed by then converts into a permanent deprecation project, which is the one outcome this plan exists to avoid.

---

#### The mission — read this first, it governs every phase

Phase 17 exists to cure five diagnosed disease patterns (analysis §2), not to add features. Every session must hold these as hard rules:

1. **Simplify and integrate — never add another ad hoc layer.** When a task needs a new behaviour, extend the relevant shared model or fact table; never bolt a special case onto a call site. Remove traces of old implementations entirely when they become useless — no commented-out corpses, no "kept just in case" branches.
2. **Roles are stored, never guessed.** No code may identify a row/column/cell by matching its rendered English label, its name prefix, or a magic field value. If you need to know what something is *for*, read its stored role; if the role is not stored yet, storing it is part of your task.
3. **One resolver, one model, taken to completion.** A setting is resolved ONCE (in the settings frame / the render model / the fact table) and consumed everywhere. If you find yourself re-deriving "what kind of column is this" downstream, you are patching the disease, not the symptom.
4. **The axes never meet in a vectorised expression.** Anything indexed per row_var and anything indexed per col_var may only combine through the settings frame (one row per pair). No `length(x) == n` guessing, no cross-axis `&`.
5. **Facts live in ONE table.** Never maintain two encodings of the same rule "kept in sync" by comment — derive both consumers from one source, or group by the rendered output itself (the 16e lesson).
6. **Public API stays retro-compatible; internals are free.** The 1.4.0-new, never-released surface (constructor formals, new args, new options) is still free to change — **that freedom ends at the CRAN release**, which is why Phase 17 runs now.
7. **A claimed fix ships with the fixture that fails without it.** Assert non-zero counts; never let a test pass vacuously.
8. **Byte-identity discipline.** Each phase declares which parts are byte-identical targets (goldens must not move) and which are one conscious snapshot regen. Run the suite exactly as CLAUDE.md § Testing prescribes (`OMP_NUM_THREADS=1`, `TESTTHAT_CPUS=8`, temp runner outside `tests/`).
9. **End-of-phase documentation discipline** (CLAUDE.md § The last step of every implementation): file headers, `# DESIGN:`/`# WARNING:` tags, CLAUDE.md § Key Design Decisions line, `dev/tabxplor_architecture.md` when structure changes, NEWS.md when user-facing. Line refs in this roadmap are anchors from the 2026-07-20 audit — **re-grep before editing**, they drift as phases land.

---

#### Settled decisions — maintainer rulings, do not re-open

| Decision                                                                   | Ruling                                                                                                         |
|----------------------------------------------------------------------------|----------------------------------------------------------------------------------------------------------------|
| `meta` merge of the five 1.4.0-new table attrs                             | **Yes, merge now** (Phase 17b)                                                                                 |
| Role model (row/col kinds, honest pvalue cells, reg column role)           | **Yes, now**, before the French phase (17c)                                                                    |
| `tabxplor.output_kable`                                                    | **Keep** (used in .Rmd/.qmd); **fix** its KNOWN-BUG instead of retiring (17g)                                  |
| kableExtra engine + `kable_tabxplor_style` + `always_add_css_in_tab_kable` | **Keep as legacy** — no kill, no deletion; fix stale comments, degrade gracefully without kableExtra (17g)     |
| `mnl_vsrest` (MNL "j vs rest" at profile)                                  | **Keep** (maintainer removed it from the cut list)                                                             |
| `method = "profile"`                                                       | **Keep as-is** (no shrink)                                                                                     |
| `tab_plot`                                                                 | **Freeze as legacy**: keeps working, zero new investment, redesigns only preserve its compatibility            |
| `predicted_unadjusted`                                                     | **Cut now**; keep the Emp.%==unadjusted identity as a test assertion                                           |
| `tab_num(df=, num=)` escape hatch                                          | **Cut now** (soft-deprecation shim if it turns out 1.3.1-public — verify at implementation)                    |
| `totcol` 5-grammar parser                                                  | **Cut 3 of 5 grammars now** (names / numeric indices / "col"-"no" vector); keep "last"/"all_col_vars" + "each" |
| `.by_table` on `tab_many()`                                                | **Make internal now** (parity-test plumbing, not a public arg)                                                 |
| `conditional_format`, `n_min`, `hide_near_zero` on `tab_xl()`              | **Drop now**, before release                                                                                   |
| `filter=` string arg on `tab()`                                            | **Doc-deprecate** (keep working)                                                                               |
| `score_from_lv1`                                                           | **Keep** + add test + document + vignette mention (17j)                                                        |
| `tab_get_wrapped_dimensions`                                               | **Keep** (personal use), no action                                                                             |
| `fct_clean`, `compare_levels`, `formats_SAS_to_R`                          | Delete if unexported; lifecycle-deprecate if 1.3.1-exported; `formats_SAS_to_R` may move to `dev/`             |
| `quasipoisson` arm, compound-formula escape hatch                          | **Keep** (cheap / contained)                                                                                   |
| jamovi JS helper duplication, tier-3 reref sub-path                        | **Keep as-is** (maintainer removed both work items)                                                            |
| Dead weight (§2.5 + §6 "delete now" rows)                                  | **Delete now**                                                                                                 |

**Anti-propositions (analysis §7, all confirmed):** no reg columns through the aggregate core; no fmt field merges or column-attr drops (c-iii stands); keep the S3-per-verb registrations; keep the test-display two-rail split (console grid vs export rows); no re-opening of settled perf verdicts (scan fusion, chi2 marshalling, `.fine` seam); no `pct="col"` parity work as a side effect.

---

#### Target architecture — the global image after Phase 17

**Metadata model.** The 18 fmt fields are untouched (user contract). Column attributes go 10 → **11** with `role = "model" | "emp" | ""`, and `fmt_col_attrs` is **derived from one source** (the `new_fmt()` formals minus the field names) so an attribute can never again be forgotten at a rebuild site. The table constructor becomes `new_tab(tabs, subtext, test, meta)` (+ deprecated `chi2` alias): `subtext` (CRAN-public) and `test` (data, needs `vec_rbind`) stay top-level; **`meta` is ONE list** holding `vars` (roles incl. the new `row_roles`/`col_roles` kinds, `wt`, the new `caption`), `ci_settings`, `render_extras`, `empirical_tips`, `reg_meta`, `color_breaks`. One `tab_attrs()` line per top-level attr; `meta` reconciles element-wise on bind; every existing getter keeps working as an accessor into `meta`.

**Resolution spine.** `tab()`/`tab_many()` normalize arguments ONCE at the boundary into a **settings frame** — one row per (row_var × col_var) pair carrying every resolved per-pair setting (pct, or, ci, colour spec, digits, levels, na, ref rule…). A **typed ctx** (constructor with defaults, no `exists()` guards) carries it; `tab_rowvar_ctxs` slices frame rows. The leaves (`tab_plain`/`tab_num`) split into public wrapper (parses user args) + **core that consumes resolved settings only** — no re-forcing, no double `finalize_color_spec`, no legacy-string re-decoding. A **reference plan** (per leaf: ref-row rule per comp group, `ref_col_idx` per column, ref2) is computed once and executed by `tab_apply_reference` (signature preserved — the jmvtab reref consumes it).

**Fact tables.** ONE `MEASURES` table drives both the colour plan and the legend (word, glyph, raw field, scale key, `sig_source ∈ {bounds, pvalue, none}`, totrow/refrow gates); the reg **empirical fact table** (per family × effect: column names, fmt shape, CI function + method, colour measure) drives the crude-companion builders AND derives `ci_settings` — the "empirical CI matches the model CI" rule becomes data.

**Render path.** `tab_export_prep()`'s model carries roles **including the stored kinds** (no English whitelists, no rendered-string equality); a **staged materializer** declares synthetic rows/cols as specs with per-backend fold policies (no create-then-delete cycles); transpose is a flipped call into a shared `roles_from()` builder (no second model); `format()` remains the ONLY string producer (export-parity contract); footer = `tab_footer_streams`/`render_footer` behind one `rd_footer()` helper.

**jamovi.** One cache **kernel** (store lifecycle, byte-bounded LRU, fetch-or-compute, array folder) with per-module key configs (jmvtab 3-tier, jmvtabreg 2-tier); shared R6 helpers; schema bumps ride the designed invalidation.

---

#### Cross-phase protocol

- **Start of session**: read this roadmap's phase entry, the analysis sections it points to, and the listed code regions (use parallel search agents for the audit refresh — line refs below WILL have drifted). Read `dev/tabxplor_1.4.0_decisions.md` for any §-referenced settled decision you touch.
- **Verification**: full suite green after each phase (the CLAUDE.md § Testing recipe). Byte-identical phases: zero golden/snapshot churn tolerated — investigate any diff. Conscious-regen phases: regenerate ONLY the listed families, review the diff deliberately, record it.
- **jamovi schema**: any phase that changes what the caches store or key on bumps `JMVTAB_CACHE_SCHEMA`/`JMVREG_CACHE_SCHEMA` (the designed invalidation path). Never hand-edit `.h.R`; UI-file edits stay inert until the maintainer's `prepare()`.
- **End of session**: the § last-step documentation discipline; append the phase's DONE summary under its entry (the maintainer archives to `dev/tabxplor_1.4.0_roadmap_DONE_PHASES.md`); accumulate NEWS.md entries for user-facing changes (Phase g trims later).
- **If a phase runs long**: split at its marked seam into `-i`/`-ii` sessions rather than rushing the tail.

---

#### Phase 17a — defects, drift and dead weight (janitorial)

**Goal**: fix every verified defect that needs no redesign, delete all verified-dead code, and single-source the small sync-by-comment pairs — so later phases work on a clean floor. Everything here is byte-identical except the fixed bugs (each gets its failing-first fixture, rule 7).

Read first: analysis §2.4, §2.5, §3; the audit refs below.

1. **Defect 1**: add `model_family` to the column-attr carry — and fix it structurally: derive `fmt_col_attrs` (tab.R:2949) from one source (`new_fmt()` formals minus the 18 field names) so the list can never drift again. Fixture: a mixed-family `tab_reg(empirical=)` export keeps per-column families through footer materialisation (legend names OR and IRR correctly).
2. **Defect 2**: `vec_math.tabxplor_fmt` sum/mean arms use `fmt_color_attr(x)` (as `+`/`-` do) and pass `color_signif` + `model_family`. Fixture: `sum()` over a two-channel column keeps both channels + policy.
3. **Defect 3**: port the exact-match-first rule into `diff_index_mean` (tab.R:4604) — interim fix; Phase 17f deletes the function entirely. Fixture: mean table with `ref = "$25000 or more"`-style label.
4. **Defect 4**: `gtab_cast`/`gtab_ptype2` (tab_classes.R:2846,2862) reconcile via `tab_bind_attrs` like the plain path. Fixture: bind two grouped tabs, both `test` blocks present.
5. **Defect 9**: doc corrections — CLAUDE.md colour-engine claim (`fmt_color_selection` is gone; the shared artifact is `fmt_color_channels`/`fmt_channel_codes`), repo-map line counts (fmt_class ~4550, tab_classes ~3999), stale `tab-render-html.R:536` "kableExtra is an Import" comment.
6. **Dead weight, delete**: `var_contrib()`, `tab_num(na="drop_fct"/"drop_num")` signature values, `tab_last` relic, `ci_html_subscript`, `pillar_shaft.tab_chi2_fmt` (+ NAMESPACE line), dead vendored `path_sanitize` (utils.R:964 — or wire jmvtab-export's inline fallback to it, one of the two), ~780 commented-out lines across tab.R / fmt_class.R / tab_classes.R (inventoried in the audits: old tab_ci :6860-6997, pillar relics :2399-2466, color_graph, vctrs-FAQ transcription, vec_arith relics…). `fct_clean`/`compare_levels`/`formats_SAS_to_R` per the ruling (check NAMESPACE first). Move `zscore_formula` to tab-agg.R.
7. **Small single-sourcing**: adopt `tab_restore()` in the 6 hand-rolled restore blocks (select/rename/rename_with/relocate/summarise/arrange tails); merge the twin console print methods (`out[3 + inherits(x, "grouped_df")]`); merge `vec_ptype_abbr`/`vec_ptype_full`; single-source the `get_wn` NA→n fallback (4 copies: fmt_class.R:1345/2620, tab_classes.R:1091, tab-test-display.R:490); make `default_ci_settings()` derive from `tab()`'s formals instead of hand-mirroring them.

Verification: full suite, zero golden churn; the new fixtures are the only new tests.

**DONE (2026-07-20).** Full suite green (FAIL 0, PASS 3794, SKIP 4 = the usual Suggests/benchmark opt-ins), zero golden/snapshot churn (byte-identity held everywhere except the four new defect fixtures).
- **Defects.** (1) `fmt_col_attrs` is now DERIVED in `fmt_class.R` — `setdiff(names(formals(new_fmt)), c(fmt_field_names, "...", "class"))` off the new single-source `fmt_field_names` (the 18 fields) — so it can never again miss an attribute; it now carries `model_family` (10 attrs). (2) `vec_math.tabxplor_fmt` sum/mean arms now use `fmt_color_attr` + pass `color_signif`/`model_family`. (3) `diff_index_mean` (nested in `tab_num`) tries an exact label match first. (4) `gtab_cast`/`gtab_ptype2` reconcile via `tab_bind_attrs(x, ...)` like the plain path. Each ships a failing-first fixture (test-fmt_class.R ×2, test-tab.R, test-tab_classes.R).
- **Dead weight deleted.** `var_contrib()`, the `tab_num(na=)` `drop_fct`/`drop_num` values, the `tab_last` relic, `ci_html_subscript` (inlined at its one caller), `pillar_shaft.tab_chi2_fmt` (unreachable — NAMESPACE regenerated), the vendored `path_sanitize`, `fct_clean`, `compare_levels`; `formats_SAS_to_R` MOVED to `dev/formats_SAS_to_R.R`; `zscore_formula` MOVED to `tab-agg.R` (beside the CI engine); ~500 lines of commented-out dead code (old `tab_ci`, pillar/vec_arith relics, `color_graph`, vctrs-FAQ transcription, old total-recalc + totcol-neutralising blocks).
- **Single-sourced.** `tab_restore()` adopted at the 6 dplyr restore tails; the two console `print` methods merged into one (grouped is an alias; header index via `inherits(x, "grouped_df")`); `vec_ptype_abbr`/`vec_ptype_full` share `fmt_ptype_label()`; the 3 get_wn materialise sites use `fmt_data_wn()`; `default_ci_settings()` DERIVES from `formals(tab)`.
- **Docs.** Defect 9 corrections (CLAUDE.md colour-engine claim + repo-map line counts, `tab-render-html.R` kableExtra-Import comment); the stale "9 fmt_col_attrs" comments updated to "the fmt_col_attrs".

---

#### Phase 17b — table metadata: the `meta` merge

**Goal**: finalize the public constructor surface before it freezes at release. `new_tab(tabs, subtext, test, meta)` with ONE `meta` list replacing the five 1.4.0-new scalar formals; `color_breaks` joins it; `caption` and build-time `vars` complete the metadata.

Read first: analysis §5.6.4 (+ maintainer ruling "merge now"), §8; tab_classes.R attr threading (`tab_attrs`, `tab_bind_attrs`, the reconcilers), the ~80 real write/read sites (grep `render_extras|ci_settings|empirical_tips|reg_meta|new_vars_attr`).

1. Design: `meta` = named list `vars`, `ci_settings`, `render_extras`, `empirical_tips`, `reg_meta`, `color_breaks`. `subtext` (CRAN-public) and `test` (needs `vec_rbind`) stay top-level formals; `chi2` stays as the deprecated alias formal. `tab_attrs()` returns three entries; bind reconcile: subtext union, test `vec_rbind`, meta element-wise first-non-NULL (color_breaks: per-scale merge as `push_color_breaks` does).
2. Mechanical pass over the write sites (`tab()` tail, tab_reg tail, tab_counts, tab_compact, the two footer appenders' `attrs=` lists) and read sites (exported getters become accessors into `meta` — **every exported getter keeps its signature and behaviour**).
3. `color_breaks` thereby joins the carried attrs (fixes defect 7) — `tab(color_breaks=) |> filter()` keeps the per-table breaks; document in `?tab`.
4. Add `caption` as a `meta$vars` sub-field: written by a new `tab(caption=)`? NO — no new public arg without need; written by `tab_kable(caption=)`-style setters? Decision recorded in analysis §8: a stored caption so it survives pipelines; implement as `vars$caption`, settable via a small exported setter (`set_caption()`) and read by every exporter's caption fallback (before `reg_title`).
5. `tab_plain()` writes `vars` at build (it is free) so `tab_render_vars` stops guessing on step-built tables.
6. Bump both jamovi cache schemas (the tier-3 carrier stores unwrapped attrs).

Verification: full suite; byte-identical rendering (attribute plumbing only). Sentinels: test-tab_classes (verb survival), test-jmvtab-cache / test-jmvtabreg-cache cold+warm, export snapshots unchanged.

---

#### Phase 17c — the role model (keystone)

**Goal**: everything knows what it is. Stored kinds for synthetic rows/columns, honest `pvalue` cells, a reg column `role` attribute — retiring every render-then-match-by-English heuristic. **This phase unblocks the French translation phase.**

Read first: analysis §4 (all), §2.1; tab-export-prep.R (tot_block detection), tab_classes.R (`tab_collapse_total_rows`, `tab_materialize_extras`), tab-transpose-render.R (absorb heuristics), tab-test-display.R (cell builders), fmt_class.R (legend adapters, `fmt_color_plan` significance gate).

1. **Row/col kinds** (`"data" | "total" | "n" | "row_pct" | "pvalue" | "gof" | "sd"`) stored in `meta$vars$row_roles`/`col_roles`, written by every materializer at creation (`tab_add_n_pct`, `tab_append_footer`, the xl sd-twin, `tab_or_total_col`, total-row builders). Consumers switched: export-prep's tot_block detection (the English whitelist at tab-export-prep.R:410-416), `tab_collapse_total_rows` (rendered-string equality at tab_classes.R:1360-1362 → role + key comparison), the transpose absorb heuristics (tab-transpose-render.R:181,187). Keep a graceful fallback for hand-built tables without roles (the old heuristic, clearly marked as fallback-only).
2. **Honest p-value cells** (fixes defect 5): the p lives in the `pvalue` field; the colour plan gains the explicit `sig_source = "pvalue"` gate for these cells (the mechanism contrib already uses); delete the `diff = -0.5` magic, the `pct`/`var` double-write, and the write-only `col_var = "chi2_cols"` marker. Conscious regen: export snapshots containing p-value/GOF rows (values identical, storage honest); fixture: p ≥ 0.05 row turns red under `color_signif = "grey_non_signif"`.
3. **Reg column `role` attribute** (`"model" | "emp" | ""`, the 11th column attr — safe now that `fmt_col_attrs` is derived, 17a.1): written by `reg_build`/`reg_empirical_columns`, read by `legend_reg_adapter`/`legend_reg_eff_word`/`legend_specs` instead of `startsWith("Emp.")`; `legend_ref_label` uses `is_totcol()` instead of `startsWith("Total")`. One `/vctrs-field` checklist pass.
4. Re-grep at the end: **zero** remaining sites matching rendered labels or name prefixes to decide behaviour (`rg 'startsWith.*(Emp|Total)|"pvalue"|"row_pct"' R/` reviewed line by line).

Verification: full suite; conscious regen limited to p-value/GOF-row snapshots + the fmt-contract record-shape snapshot (11th attr). Everything else byte-identical.

---

#### Phase 17d — colour, legend and display facts

**Goal**: one fact table for measures end-to-end; the colour-spec maze decoded once at the boundary; the display token system canonicalised.

Read first: analysis §5.2, §2.2; fmt_class.R colour pipeline (`color_scales` → `color_measure_policy` → `fmt_color_plan` → `fmt_color_slots` → `resolve_color_channel_plans` → `fmt_color_channels`), the legend `MEASURES` table + `legend_resolve_spec`, tab.R/tab-resolve.R normalizers (`normalize_color_spec`, `finalize_color_spec`, `legacy_union`), the `/color-mode` skill.

1. **`get_ref_field(x, field)`** — one base-R helper replacing the four broadcast clones `get_ref_pct`/`get_ref_means`/`get_ref_var`/`get_mean_contrib` (~70 L, colour-hot-path speedup per the `fmt_row_flag` precedent). Byte-identical.
2. **Unified `MEASURES`**: extend the legend's fact table with the plan columns (raw field, scale key per column kind, `sig_source`, totrow/refrow gates) and make `fmt_color_plan` read it — 11 measure switch arms → ~3 (only the diff↔ratio bound rescale and the guaranteed-effect offset stay as policy code). Adding a measure becomes one row end-to-end; update the `/color-mode` skill checklist accordingly. Byte-identical target (plan is golden-locked).
3. **Finish Step 4d**: decode legacy colour strings (`diff_ci`/`after_ci`/`ci`) ONCE at the argument boundary; thread only the decoded `(color, color_signif)` pair (through the settings frame if 17e landed first — see §Order); delete `color_measure_policy`'s re-decoding, `legacy_union`'s string manufacture, and the `single0` legacy slot table's plumbing (keep the user-facing soft-deprecated strings working at the boundary). Bump the jmvtab cache schema (the tuple carried the legacy string).
4. **Canonicalise `rr` → `ratio`** as the internal token (read-side alias only) — deletes the ~8 dual matches (`c("ratio","rr")`) across get_num/set_num/format/tooltips; fix the stale `fmt()` roxygen for `display` while there.
5. **Optional, only if the byte-harness stays green**: the `format()` token registry (per token: source field, ×100, signed, big.mark, min-digits, excel-code class). Stop at the first non-identical golden — this item is expendable, the phase is complete without it.

Verification: full suite; byte-identical (items 1-4); item 3 additionally cold+warm jamovi cache tests after the schema bump.

---

#### Phase 17e — the settings spine (boundary)

**Goal**: arguments are normalized ONCE into a per-(row_var × col_var) settings frame; the ctx is typed; the recycle-bug class becomes unrepresentable.

Read first: analysis §5.1.2/7, §2.3; tab.R boundary (`tab()` pre-recycles, `tab_setup`'s 9+2 recycles, the 5-branch `pct_vect`, `ref_vect`, `tab_rowvar_ctxs`), tab-parallel.R (`tab_pmap`), tab-counts.R's parallel ctx literal, the settled decisions (§5 row-axis globalisation; Q7 tab_many list guarantee; the ordering invariant).

1. **The settings frame**: one tibble, one row per (row_var × col_var), columns = every per-pair resolved setting (pct, or, ci, colour spec, digits, levels, na, totcol-type, ref rule…). All input grammars (scalar, per-col_var vector, tab_many list-of-lists, `sup_cols` shim) become boundary parsers filling the frame. After `tab_setup`, **no code recycles anything** — consumers index the frame.
2. **`tab_rowvar_ctxs` slices frame rows** — the `length(x) == n` heuristic dies.
3. **Typed ctx**: a constructor giving every field a default (kills the 39 `exists()` guards); `ctx_update`'s NULL-preservation rule enforced by the helper, not comments. `tab_counts`'s hand-built parallel ctx uses the same constructor (kills the ctx-literal duplication).
4. While there: collapse the triple `stars`-option read and the duplicated `comp` forcing into the frame's resolution (leaf-side removal completes in 17f).
5. **Argument-surface cuts that live in this same boundary code**: the `totcol` grammar cut (3 of 5), `.by_table` made internal, `filter=` doc-deprecation.

Verification: full suite, **byte-identical** — this is a pure re-plumbing. Sentinels: test-parallel-parity, test-cache-keys, test-fuse-parity, the multi×multi shapes (the past bug fixtures must all stay green). Split seam if long: frame + slicing (17e-i) / typed ctx + cuts (17e-ii).

---

#### Phase 17f — leaves, reference plan and legacy quarantine

**Goal**: the leaves consume resolved settings only; the reference system becomes one plan + one executor; the superseded dplyr-era steps leave tab.R.

Read first: analysis §5.1.3/4/5/6/8, §2.4; tab.R leaves (`tab_plain`, `tab_num`), `tab_apply_reference` + `resolve_ref_vector`/`diff_index`/`calculate_refrows` + tab_num's inline copies, `tab_ci`'s re-derivation head, the step wrappers, jmvtab-cache.R's reref (consumer of `tab_apply_reference` — signature must hold).

1. **Leaf wrapper/core split** (decisions §29 Finding 3, endorsed): public `tab_plain()`/`tab_num()` = arg-parsing wrappers; the pipeline calls cores that consume the settings frame. Removes the double `finalize_color_spec`, the `.color_deprecate` flag, the leaves' duplicated `ref="auto"`/`comp` forcing.
2. **The reference plan**: per leaf, computed once — ref-row rule per comp group, per-column `ref_col_idx` (16c binary-OR encoding generalised), ref2. `tab_apply_reference` stays the executor with its signature (jmvtab reref untouched); `diff_index_mean` and tab_num's inline `calculate_refrows` copy are **deleted**; `tab_ci`'s built-table re-derivation chain (`detect_totcols`/`detect_refcol`/8-branch case_when) consumes the plan when driven by the pipeline (standalone step-path keeps a fallback). Must preserve: `ref` reinterpreted by `pct`, per-row_var named refs, the col% collapse message (settled §4).
3. **Shared leaf tails**: totals renaming, `tab_var_1lv` wrap, totrow/tottab derivation, the six-copy placeholder-injection idiom — extracted once (~150 L).
4. **Cut `tab_num(df=, num=)`** per the ruling (deletes the three `weighted.mean` N-scan copies, ~90 L); soft-deprecation shim only if 1.3.1-public (verify).
5. **Quarantine the superseded trio**: `tab_pct`/`tab_tot`/`tab_totaltab` + `pct_formula`/`diff_formula` + their repair machinery (~650 L) move to `R/tab-steps-legacy.R` (exports unchanged); retire the internal `chi2 =` constructor alias and `get_chi2()` reads (10 sites — the public deprecated alias formal stays).

Verification: full suite, byte-identical target throughout (item 2's `diff_index_mean` deletion is covered by 17a's ported fix + fixture). Split seam: leaves + plan (17f-i) / tails + cuts + quarantine (17f-ii).

---

#### Phase 17g — export stack integration

**Goal**: the render model becomes the one intermediate representation it set out to be — shared headers, single-sourced hex, a staged materializer on stored roles, transpose without a second model — and the print-path bugs die.

Read first: analysis §5.3, §2.2; tab-export-prep.R (the model + `tab_header_runs`/`tab_label_runs`), tab_md.R, tab_xl.R (+ tab-xl-backend.R), tab-transpose-render.R, tab_classes.R print/kable/materialize sections, tab-render-html.R; the export-parity contract (format() = only string producer).

1. **md onto the shared models**: `tab_header_runs()` + prep's `new_col_var` replace md's hand-rolled separator/span loops (tab_md.R:257-268, 473-505). Conscious md-snapshot regen.
2. **xl ann-hex completion** (the stale 10j-A-ii TODO): xl consumes the theme-resolved hex already in `ann`; its own `get_color_style()` lookups die; slot→hex is single-sourced (CSS side reads the same source).
3. **`rd_footer(rd, medium, theme)`**: folds the 4× footer-invocation boilerplate + the 4× caption fallback (now reading `meta$vars$caption` first, then `reg_title`).
4. **Staged materializer** (requires 17c roles): synthetic rows/cols declared as specs (kind + payload) with per-backend fold policies — replaces the 6-8 sequential passes and both create-then-delete cycles (n column built-then-folded; total rows built-then-collapsed); `xl_materialize_data` becomes a backend policy. `format()` stays the only string producer. One conscious cross-backend regen.
5. **Transpose via `roles_from()`**: extract `prep_one_table()`'s role assembly into a builder both orientations call; keep `tx_format_source_cols` (physical constraint). Fixes the audited drift (transposed tables currently lose `reg_title` + `empirical_tips`).
6. **kableExtra legacy containment** (per ruling — keep, don't kill): fix the stale Import comment, make the html engine's Viewer print degrade gracefully when kableExtra is absent (tooltips off + message, no broken dispatch), leave `kable_tabxplor_style` + `inst/tab.css` untouched.
7. **Fix the `output_kable` KNOWN-BUG** (per ruling — the option stays): the two-channel-colour crash at the `tab.R:2219` internal switch (`mutate` on a `tabxplor_kable`); root-cause the finalize/kable ordering divergence; fixture: `options(tabxplor.output_kable=TRUE)` + `color = TRUE` auto-prints.
8. **Drop `conditional_format`, `n_min`, `hide_near_zero` from `tab_xl()`** per the ruling (inert shells).
9. `tab_plot`: frozen — verify it still renders after 4/5 (it consumes the prep + footer streams), change nothing else.

Verification: full suite; conscious regens limited to md snapshots (1), xl workbook assertions (2/4), transpose locks (5). The transpose≡native and export-parity tests are the sentinels. Split seam: 1-3+6-9 (17g-i, mostly mechanical) / 4-5 (17g-ii, the materializer).

---

#### Phase 17h — tab_reg integration

**Goal**: one Wald finalize, one skeleton aligner, specs as the unit of truth, the empirical system as one fact-driven framework whose CI rule derives `ci_settings`.

Read first: analysis §5.4, §2.4; tab_reg.R (`reg_build`, `reg_fit`, `reg_column`/`reg_marginal_column`/`reg_empirical_columns`/`reg_empirical_tips`, the `.fit_cache` seam — its byte-identity contract is load-bearing), tab-agg.R CI engines, test-jmvtabreg-cache.R.

1. **`reg_wald_finalize()`** replacing the 3 est±crit·se→p-dual→exp copies; **`align_to_skeleton()`** replacing the 5 `"\r"`-key mask blocks; **`reg_cleanup()`** for the 8× inlined cleannames regex. Byte-identical.
2. **Spec as the unit of truth**: drop the scalar family/do_exp/effect_shape/eff_word/color formals from `reg_build` (15e populates specs fully); collapse the 30-formal signature re-listed at 3 call sites into `(data, specs, shared)`; the 19 `sp_get()` fallbacks die. Internal-only (no external caller — verified).
3. **Empirical fact table**: per (family, effect) — column names, fmt shape fields, CI function + method, colour measure — one builder loop replaces the four isomorphic arms; **`ci_settings` derives from the same rows** (the 16d rule becomes data). Multinomial tips stay a separate arm (different medium). The `role = "emp"` attr (17c) is written here.
4. **Model frame once**: store the complete-case frame (or row mask) per fit and thread it to the empirical/tips blocks — the three textually-identical `drop_na()` recomputes die; document the digest-path fallback in one place.
5. **Cut `predicted_unadjusted`** per the ruling (~80 L); keep the Emp.% == unadjusted-prediction identity as a test-only assertion.
6. Untouched per rulings: `mnl_vsrest`, `method="profile"`, `quasipoisson`, the compound-formula escape hatch, the `.fit_cache` digest/reref math.

Verification: full suite; byte-identical (reg tables are not snapshotted; test-tab_reg* value assertions + the jmvtabreg cache byte-identity lock are the sentinels).

---

#### Phase 17i — jamovi integration

**Goal**: one cache kernel, two module configs; shared R6 helpers; the fingerprint blind spot documented and escapable in both modules.

Read first: analysis §5.5, defect 6; jmvtab-cache.R + jmvtabreg-cache.R (the two store lifecycles, the two LRUs — one O(n²), the three array folders), jmvtab.b.R + jmvtabreg.b.R (the 4 verbatim blocks), the schema-bump invalidation design.

1. **Cache kernel**: extract store lifecycle + byte-bounded LRU + fetch-or-compute + generic `jmv_fold_array(arr, key, val, coerce)` into one internal module; jmvtab keeps its 3-tier key logic and carrier/reref untouched, jmvtabreg its 2-tier digest/fit — as configs on the kernel. Fix the O(n²) eviction in passing. Bump both schemas.
2. **Shared R6 helpers**: `.notice()`, `.render_html()`, the export-click block, the `jmv-weights` fold — one package-level helper set called by both `.b.R` files.
3. **Defect 6**: document the `jmv_col_fp` value-edit blind spot in jmvtabreg's header (it can serve a stale FIT); thread the `tabxplor.jmv_full_hash` escape hatch to both modules; seed + document the option in `.onLoad`/`?tabxplor-options` (it is currently unseeded).
4. Untouched per rulings: the JS helper duplication (uijs is per-module), the tier-3 reref sub-path.
5. Preserve absolutely: `jmvreg_fit_key`'s reference-independence, `reg_reref_fit_res` byte-identity, the `.h.R` never-hand-edit rule.

Verification: full suite; test-jmvtab-cache / test-jmvtabreg-cache cold+warm+reref green; byte-identical rendering.

---

#### Phase 17j — options and internal-docs alignment

**Goal**: the options namespace is coherent, and the dev docs describe the post-17 architecture with no trace of the removed machinery.

Read first: analysis §5.6.5, §8; `?tabxplor-options`, `.onLoad`, `dev/tabxplor_architecture.md`.

1. **Options pass (1.4.0-new names only)**: `kable_css` → `tab_kable_css` (alias kept); `console_theme`/`export_theme` aliases for the two non-parallel theme options (old names keep working); `jmv_full_hash` seeded + documented (done in 17i — verify); `output_kable` + `always_add_css_in_tab_kable` stay per rulings. Every option in `.onLoad` AND `?tabxplor-options`, in sync.
2. **Architecture docs**: rewrite the affected sections of `dev/tabxplor_architecture.md` (metadata model, resolution spine, fact tables, materializer, cache kernel) and the CLAUDE.md repo map + Key Design Decisions to describe the POST-17 state; delete descriptions of removed machinery entirely (rule 1 — no traces).
3. NEWS.md: consolidate the Phase 17 user-facing entries (arg cuts, new `set_caption`, option aliases) — Phase g does the final trim.

Verification: `pkgdown::check_pkgdown()` still clean; full suite green.

---

#### Phase 17k — vignette enrichment: teach the good features

**Goal**: close the gap between the shipped surface and the taught surface. The audit found a large *cold-but-good* list — differentiator-grade features no vignette teaches (analysis §1, §6) — so users literally cannot discover them through the learning path. This phase adds them where they pedagogically belong, in the same beginner-first voice as the existing vignettes, on `gss_simple`, with Suggests-guarded chunks where needed.

Read first: analysis §1 (hot/cold surface), §6 closing note; the three vignettes + README.Rmd (voice + structure); the roxygen of each feature below.

Feature-by-vignette map (a paragraph or short subsection each — an example the reader can run, one sentence on when to reach for it, no internals):

1. **Intro vignette (`tabxplor.Rmd`)**:
   + `n_min=` — hiding cells with too-small bases (the small-sample companion to `guaranteed_effect`).
   + `subtext=` and the new `set_caption()` (17b) — titling and annotating a table that survives the pipeline into every export.
   + `transpose=` at export — the sanctioned answer to "col% with several row_vars" (settled §7), shown on `tab_kable`/`tab_xl`.
   + `tab_css()` — one stylesheet for a whole document, dark-mode `theme = "auto"`, the fixed-width escape hatches (`?tab_css`).
   + `output_list=` — when you want separate tables instead of one merged table.
   + One honest sentence on `tab()`'s weighting rule (weighted estimate + unweighted n; Kish `n_eff` opt-in; `tab_reg()` is fully design-based) — the vignette layer currently doesn't state it (analysis, Tensions).
2. **Programming vignette (`tabxplor-programming.Rmd`)**:
   + `tab_counts()` — a real section: building tabxplor tables from pre-aggregated counts (long/wide/freq+N), what CI/chi2 can and cannot do on frequency-only input. A whole Phase-4 feature with zero doc presence today.
   + `tab_spread()` / `spread_vars=` — pivoting tab_vars into columns, with the reg `split_var` cross-reference.
   + `score_from_lv1()` — per the ruling: test + roxygen refresh land here too, with a worked example.
   + A pointer paragraph: `tab_many()`'s list mode + `purrr::pmap` batch workflow (already in README) linked from here.
3. **Regression vignette (`tabxplor-reg.Rmd`)**:
   + `split_var=` — a real section: one model per subpopulation, side by side, `tab_spread`-able; how it appears in exports (the merged vertical first column).
   + `trials=` — grouped-binomial outcomes (the jamovi Model table exposes it; R users currently have no example).
   + `tab_logit()` / `multi_logit()` — one paragraph naming the curated wrappers and when they suffice.
4. **Placement sanity**: every example must use only exported functions (the Last Phase e-iiii lesson — vignettes build against the installed namespace); guard nnet/MASS/survey chunks with `requireNamespace`; keep each addition short — these are discovery paragraphs, not reference docs (the reference lives in `?help`).

Verification: all three vignettes render with colours (the fansi hook); `devtools::build_vignettes()` clean; no new unexported-function calls (grep the chunks); full suite untouched.

---



### Last Phase – lasts steps and release

#### Phase 15f — Jamovi UI French translation


#### Last Phase g – NEWS.md simplification


#### Last Phase h – tabxplor R french translation

All legends should be carefully translated to French.
Could the package documentation be translated for French users ? Could the whole pkgdown easily have a french version, with the possibility to choose on the webpage ?  
What other strings should be translated in French ?

#### Last Phase i – github PR and CRAN release











### Reference — bugs, benchmarks, perf

Fixed bugs recorded in `dev/tabxplor_1.4.0_roadmap_DONE_PHASES.md`

#### Open bugs

- **OPEN (found Last Phase e, low impact):** `options(tabxplor.output_kable = TRUE)` + a **two-channel
  colour** (a background channel, e.g. the `color = TRUE` auto scheme = `c("diff","ratio")` = diff text
  + ratio background) errors on the auto-print with *"no applicable method for 'mutate' applied to ...
  tabxplor_kable"*. The failing site is `tab.R:2219` (`tabs %>% tab_kable()` inside `tab()`), reached
  ONLY through the `output_kable` internal switch: `tab_kable(tab(..., color = TRUE))` and the console
  print BOTH work, and single-channel `color = "auto"`/`"diff"` work under `output_kable` too. So the
  finalize/kable ordering diverges only on that one path. Narrow (an internal switch); the new
  `vignette("tabxplor")` sidesteps it by rendering tables as coloured console output (fansi), the way a
  console user sees them. Fix when the `output_kable` / print path is next touched.
- ~~**A pre-existing golden drift.** `n_ci_tabvars.rds` / `n_ci_tabvars_all.rds` had a `ci_sup` `NaN`
   where a clean run wants `NA`.~~ **FIXED in 14v-ii**: the cause was `n <= 1` cells (`df = n - 1 <= 0`
   feeding `qt`); `ci_pivot()` now coerces `df <= 0` to `NA` (clean NA, no NaN, no warning). The two
   goldens were regenerated with the rule-B mean CIs and no longer carry the NaN.



#### Benchmarks (`dev/benchmarks/`)

The performance harness lives in `dev/benchmarks/` (`.Rbuildignore`'d). Per the scope decision, save every phase's before/after runs under `dev/benchmarks/results_1.4.0/`.

⚠ **Every committed baseline below was measured on WINDOWS/NTFS. Dev is now WSL2 Ubuntu on ext4 — do NOT diff a WSL2 run against them.** Affected: `dev/benchmarks/baseline.csv`, `tests/testthat/benchmark_baseline.csv`, `jmvtab_benchmark_baseline.csv`, `jmvtab_big_benchmark_baseline.csv`, plus every absolute timing quoted in the roadmap phases above (`~46s` suite, `225s -> 56s` parallel, the §26 parallel PoC, the Phase 5/7f/9b/10 speedups). The *ratios* within a single run stay meaningful; the absolutes do not cross the platform boundary. Nothing fails — benchmarks are opt-in (`TABXPLOR_BENCH=true`) and `test-benchmark.R` never fails — so this is a **silently misleading comparison**, not a broken test. Re-baseline consciously on ext4 before drawing any conclusion, and note the platform in the file when you do.

⚠ **The 8M fixtures are NOT in WSL2.** Migration Phase A1 ruled `big_df.rds` (161 MB) and `big_pc18_full_15M.rds` (572 MB) *reproducible* and deliberately did not copy them (`.gitignore`: *"Generated benchmark fixtures: large, regenerable, never commit"*; `gen_big_df.R` is tracked). The first `run_bench.R` therefore **regenerates the fixture first** — expect a long, one-off build, not a hang. The 13 loose `dev/benchmarks/results_*.csv` WERE copied; `results_1.4.0/` is tracked.

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
2. Keep the tabxplor version 1.4.0 roadmap in CLAUDE.md and `dev/tabxplor_1.4.0_decisions.md` up-to-date as you build it or implement it.
3. Update `dev/tabxplor_architecture.md` whenever you modify the package structure for real (add modules, rename functions, change config fields). Do not add clutter and useless details. When there is nothing to change, skip it. Update other `dev/*md` file when relevant.
4. For package structure and architecture, also add the relevant CLAUDE.md update lines in your response : it should be minimalistic, concice, no bullshit, with nothing useless that would clutter the prompt, since the details are already in `dev/tabxplor_architecture.md`. When there is nothing to change, skip it. Maintainer will move done phases to `dev/tabxplor_1.4.0_roadmap_DONE_PHASES.md` himself.
5. `NEWS.md`: user-facing and CRAN-facing, tracking new functions, new arguments and arguments changes, deprecations, and important bugs fixes. Keep it minimalistic and no bullshit. Do not edit it when it’s not necessary.
6. (`README.Rmd` : user manual. Only update before release of new version to CRAN, never before.)


