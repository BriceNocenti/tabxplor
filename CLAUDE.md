# tabxplor — AI Assistant Guide

## Package Purpose

`tabxplor` is a public CRAN R package (v1.3.1) maintained by me for creating, manipulating, and formatting color-coded cross-tabulations. It uses `data.table` for fast aggregation internally and exposes a `tidyverse`-compatible API via custom `tibble` subclasses. Tables can be exported to Excel (with colors), HTML, Markdown, and ggplot. All numeric cells are stored as `vctrs` record vectors (`tabxplor_fmt`) carrying full statistical metadata, enabling lossless display switching.

---

## Repository Map

```
R/
├── fmt_class.R     (3341 L)  Core type: tabxplor_fmt vctrs record, getters/setters,
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
├── tab.R           (~6200 L) Main API: tab(), tab_many(), tab_plain(), tab_num(),
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
│                              otherwise -- NOT stars-gated; ci_pivot guards df<=0 -> NA), agg_chi2/agg_anova
├── tab-counts.R     (~360 L) tab_counts() from-the-middle constructor (Phase 4): reshape any
│                              input shape → count-aggregate → tab_plain(.fine) + shared finalize
├── tab-resolve.R    (~200 L) tab_resolve_settings() (Phase 7b): the ONE pure arg-overwrite
│                              cascade (color="auto"/forcing/split) shared by tab_build+tab_counts;
│                              resolve_color_auto_num() (numeric arm); emits ci_scale (14b: "ratio"
│                              = the Katz interval). The jmvtab .js / cache boundary.
├── tab-parallel.R   (~200 L) Phase 8/9a row-axis dispatch (Suggests-only mirai): tab_pmap() + trampoline,
│                              named "tabxplor" pool (tab_pool_ensure/tab_parallel_workers/
│                              tab_parallel_stop), tab_build_one() (the per-row_var worker, serial OR mirai).
├── tab_classes.R   (3554 L)  tabxplor_tab/grouped_tab classes, 30+ dplyr S3 methods,
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
├── utils.R         (~940 L)  Pipe re-export, .onLoad() options setup, factor utilities.
│                              NOT the colour-palette DESIGN tools (preview_color_grid /
│                              simulate_cvd_farver / plot_oklch_hue_strip_cvd / set_luminance...):
│                              they live in dev/color_palette_tools.R and must stay there -- they
│                              are the sole reason the package would depend on farver + colorspace.
├── tabxplor-options.R (~110L) Doc-only page `?tabxplor-options`: every tabxplor.* global option
│                              (defaults live in .onLoad; keep in sync). Cross-linked from ?tab.
├── tab_reg.R       (~1780L)  Phase 12c–12h: unified regression tables. tab_reg() over ONE engine
│                              (stats::lm/glm, survey::svyglm/svyolr, svyVGAM::svy_vglm, nnet::multinom,
│                              MASS::polr; broom::tidy)
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
├── jmvtab-export.R  (~120 L)  jmvtab export helpers (Phase 7g): resolveExportPath (typed path →
│                             Documents/USERPROFILE), tab_html_string (self-contained HTML),
│                             jmvtab_export (Excel/HTML/MD dispatch)
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
│                              raw reg_fit ~9-11MB) cache instead of graceful-skipping
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

- **`tabxplor_fmt`**: vctrs record (`new_rcrd()`) with **18 per-cell fields** (was 15 before v1.4.0 Phase 1a) and 9 per-column attributes (Phase 10i-A dropped `display_spec`). The critical distinction: fields vary per cell (accessed via `vctrs::field()`), attributes are scalar describing the whole column (accessed via `attr()`). Constructor chain: `fmt()` (public, validates + coerces) -> `new_fmt()` (internal, calls `vctrs::new_rcrd()`). *(Phase 1a reshaped 15→18 in one combined pass — decisions doc §9; `ci` is now derived from the `ci_inf`/`ci_sup` bounds by `get_ci()`, a bounds-shim.)*
- **`mean` field is mean-only** (the old overload is GONE — Phase 5 landed): `mean` now carries an actual mean only on `type=="mean"` columns; for **pct-type** columns it is `NA` and the cell/reference **ratio** (the "*2 rule") lives in the dedicated **`ratio` field** (Phase 1a renamed the never-used `rr`→`ratio`). The build writes `mean = NA_reals, ratio = <ref-relative ratio>` for pct columns (`tab.R` ~L3608) and the colour engine reads `get_ratio(x)` (`fmt_class.R` ~L2688). *(c-iii audit 2026-07-19 confirmed no field/attribute consolidation is both safe and worthwhile — the 18 fields are all user-contract and none vestigial; the 9 column attributes are exported getters AND required per-column so `format()`/colour work on a standalone extracted column.)*
- **`tabxplor_tab`**: tibble subclass via `tibble::new_tibble()` with **7 table attributes**: `subtext` (legend text), `test` (chi2/ANOVA-F results tibble; §16 hard-rename of the old `chi2` attribute), `render_extras` (Phase 10i-B, the `list(add_n=, add_pct=)` display intent), `ci_settings` (Phase 13b, the CI method/confidence level metadata the colour legend names; kept distinct so it survives footer materialisation), `vars` (Phase 14d, variable roles), `empirical_tips` (Phase 14v, the multinomial crude-companion tooltip data) and `reg_meta` (Phase 14w, a reg table's model record: family/effect/dependent/reference/predictors, driving its title + "Model:" legend line + colour-legend wording), all carried through dplyr verbs by the S3 methods + vctrs reconcilers (one line each in `tab_attrs()`).
- **`tabxplor_grouped_tab`**: extends `grouped_df` for subtabled results (when `tab_vars` are present). Requires separate S3 method for every dplyr verb.

### Export Parity

Cell display values reach exporters by two **non-unified** paths — keep them in sync:

- **`format.tabxplor_fmt()`** (`fmt_class.R`) is the single source of truth for markdown (`tab_md()`), knitr/HTML (`tab_kable()`), and the console (`pillar_shaft`).
- **`tab_xl()`** (Excel) writes the raw `get_num()` value and delegates numeric formatting to Excel's engine, but it now sources the per-cell Excel number-format codes from `format(x, syntax = "excel")` (Phase 10g) — the SAME `format()` masks the text backends use — so a display/digits change no longer needs manual mirroring in `tab_xl.R` (the old `numfmt()` desync is gone). Colours come from `fmt_color_channels()`; roles/refs/bold from `tab_export_prep()`.
- Color is safe: all exporters call the same `fmt_color_selection()`.

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
3. **Selection** (`fmt_color_selection()` in `fmt_class.R`): iterates breaks, applies `color_formula()` per break level, `keep_last_break()` picks the strongest matching threshold per cell. Different boolean formulas for each color mode: `diff`, `diff_ci`, `ci`, `after_ci`, `contrib`, `OR` (+ the 1.4.0 additions `ratio`/`diff_ratio`, Phase 5).

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

### Start here (reading order + where docs live)

This roadmap is the **plan of plans**: the phased implementation order plus every open question. A fresh session asked for a *part* of the work should read, in order:

1. **This roadmap** — the phase your task belongs to, its bullets, and its pointers; the full 1.4.0 analysis (grounding, keystone, decisions, verification) is right below.
2. **`dev/tabxplor_1.4.0_decisions.md`** – the **new architecture decisions** taken for version 1.4.0. **Always read carefully**.
3. **`dev/tabxplor_architecture.md`** — architecture guide (type system, pipeline, compaction loss, exporters). It describes the **current** architecture. Read the section matching the file you touch.
4. **Top of this CLAUDE.md** — Repository Map, Global Architecture, Key Constraints, Design Decisions.

**Other long-form 1.4.0 docs live in `dev/` (all `.Rbuildignore`'d), never inline here — read the matching ones before you start:**
- `dev/benchmarks/` — performance harness + saved results (documented under *Reference > Benchmarks*). Read/run when a phase touches perf (Phases 2, 3, 6, 8).
- `dev/benchmarks/tab_many_performance_profile.md` — the full 2026-07 profile. Read before optimizing `tab_many` / `tab_chi2` / `tab_num`.

### Settled architecture decisions (2026-07 planning session)

#### Why — current-state grounding

- **Two math paths, duplicated**: `tab_plain`/`tab_num` compute pct/diff/OR/totals inline with data.table (`tab.R` ~L2491-2678); the legacy `tab_pct`/`tab_tot`/`tab_totaltab` recompute the same math via dplyr and are **not called by `tab_many()`** — the percentage/total logic exists twice.
- **CI/chi2 outside the fast path**: `tab_ci` (proportions) uses `dplyr::across` + per-cell `DescTools::BinomCI` (`tab.R` ~L4934); `tab_chi2` uses `group_split` + per-column `chisq.test` (~L5274). `tab_num` already folds *mean*-CI into data.table via closed-form `ci_mean = zs*sqrt(var/n)` (~L3771) — the template to copy.
- **No from-the-middle entry**: only the low-level `fmt()` builds cells from numbers; abusing `wt=count` leaves `n=1` per cell and silently breaks CI/chi2.
- **Output type inconsistency**: `tab.R:1540` unwraps a length-1 list to a bare tab (bare tab if 1 row_var *or* `compact`; a list only if ≥ 2 row_vars *and* not compact).
- **Exporters**: no shared prep — `tab_kable`+`tab_md` duplicate a "canonical col_vars → validate → compact" preamble; `tab_xl` keeps a list-of-sheets; `tab_plot` needs a pre-compacted tab.

#### Keystone — the aggregate-core

One internal canonical representation — a keyed count-aggregate (`n`, `wn` per `tab_vars × row_var × col_var-cell`, NA kept; **for numeric col_vars this must be a sufficient-statistics aggregate carrying moment-sums `Σwt·x`, `Σwt·x²`, NOT counts — else means/var/CI/t can't be recovered and the `weighted.var` double-scan survives; plus `Σwt²` on both branches for Kish `n_eff` (§14 weighted inference); unweighted moment-sums dropped (review 4 — §14 uses weighted dispersion only); open item G1**) — and one pure core turning `(aggregate, settings)` → fmt columns. Both entry points converge on it:

```
microdata ─ tab_prepare ─┐
                         ├─► count-aggregate ─► [pct | diff | OR | CI | chi2 | totals] ─► fmt cols ─► tab
counts (long/wide/freq) ─┘   via as_tab_counts()   (one vectorised impl each)
```

Why it is the keystone — it simultaneously (a) kills the duplicated pct/total math; (b) makes from-the-middle reliable (validate once at the boundary, then the identical core runs); (c) lets CI/chi2 join the fast path (aggregate-based, `tab_num` mean-CI template); (d) gives `tot_n` (each cell's own % base) almost for free (a property of a proper aggregate, not "the last `col_var` total column"); (e) defines the clean Jamovi cache boundaries (aggregate | per-transform | display).

**Conceptual vs physical**: the core is always aggregate-based (conceptual). The physical shared finest-grain `.fine` aggregate (fusing per-table scans) is Jamovi-reuse + `tab_counts()`-injection only. *(Phase 9c: the tab()-level opt-in scan-fusion switch — `options(tabxplor.fuse_min_rows)` — was REMOVED as a net-negative (§30); the `.fine`/`fine_for_pair()`/`use_raw` seam in `tab_plain()`/`tab_num()` remains for jmvtab, `tab_counts()`, and the numeric `fine_num`.)*

**Retro-compat guardrails**: `tabxplor_fmt` fields are the user contract (extracted via `$`/`mutate`) — must not break. Public args must not change without deprecation. `tab_pct`/`tab_tot`/`tab_ci`/`tab_chi2` stay exported but become superseded thin wrappers over the core (`lifecycle::signal_stage`), so old user code keeps working.

#### The decisions

- **Output shape**: `output_list` (default `FALSE`) replaces `compact`; `compact` deprecated (arg), `tabxplor.compact` option removed. Compact-loss analysis persisted in `dev/tabxplor_architecture.md` ("Compaction: what is lost when tables are bound"). Verdict: single-table default only gives up per-row_var flexibility real analysis never uses (divergent color/ref/ci-type on the *same column*); each-variable-vs-own-total is preserved. When `tab_vars` present, compaction can't merge → keep multi-table regardless.
- **Field surgery = one combined pass** (before the core rewrite) → **18 fields**: add `pvalue`, `tot_n`, `ci_inf`, `ci_sup`; **rename the unused `rr`→`ratio`** (placed after `diff`); **drop `ci`** (recomputed on `$`/`get_ci()` from the bounds; `fmt(ci=)` arg kept); numeric `diff` becomes a difference; `mean`-overload removed. CI is stored as asymmetric **bounds** (the single upper-half-width + symmetric bracket is wrong for Wilson/AC proportions; means exact); OR CIs move off their sidecar into the fields. **Per-cell significance is a stored `pvalue`** (Q2 — three star levels can't come from one CI level, and are undefined from bounds for asymmetric proportions/OR; decisions §12): factor `ci="diff"` = two-proportion score test, numeric `ci="diff"` = Welch t, empirical `OR` = log-OR Wald, logit = model p. Do NOT pre-add se/z/coef (tab_logit never displays them). After this pass tab_logit needs no further field surgery. Detail: `dev/tabxplor_1.4.0_decisions.md` §1-3, §12.
- **From-the-middle constructor** (`as_tab_counts()`): support long tidy counts, wide count matrix, frequencies+base N. Validate once at the boundary → same core. Require real unweighted `n`; warn/disable CI/chi2 on frequency-only input.
- **Order**: 0 finish safety net → 1 combined field pass → 2 aggregate core + math unification → 3 CI/chi2 onto aggregate (headline perf) → 4 counts constructor → 5 color diff/ratio split → 6 tab()→tab_many() merge + output_list → 7 unified exporter prep (on openxlsx v1) → 8 Jamovi caching → 9 Excel engine swap openxlsx→openxlsx2 (isolated; may slip to a 1.4.x follow-up). Each phase: golden/parity green + **save before/after benchmarks** (`dev/benchmarks/results_1.4.0/`).

#### Resolved architecture decisions (2026-07)

Grounding (code refs + statistics + caveats) in `dev/tabxplor_1.4.0_decisions.md`. Summary:

1. **fmt fields** (Phase 1, §1-3, §12) — one combined pass → **18 fields**: add `pvalue`, `tot_n`, `ci_inf`, `ci_sup`; rename unused `rr`→`ratio` (after `diff`); drop `ci` (recomputed from bounds on `$`/`get_ci()`; `fmt(ci=)` arg kept); numeric `diff` = difference; `mean`-overload removed.
2. **CI = bounds + `pvalue`** (Phase 3, §1, §12) — store asymmetric `ci_inf`/`ci_sup`; the current upper-half-width + symmetric bracket mis-draws Wilson/AC proportion CIs (means exact). **Per-cell significance reads the stored `pvalue`** (three star levels need a real p, undefined from one CI level for asymmetric proportions), not the bounds; compact `± moe` shows the larger arm; tab_logit OR-CIs move into the fields (sidecar retired).
3. **`tot_n`** (Phase 1-2, §2 — renamed from the roadmap's `ref_n`) — each cell's OWN unweighted % base (its row/col total, *not* the diff-reference's n). Stored; the weighted base `tot_wn` is recovered as `wn/pct` (not a field). Retires `detect_totcols` on built tables. Only load-bearing for standalone `tab_ci`/`tab_pct` + post-processing (not the aggregate-core / Jamovi, which hold the aggregate); `tot_n` is a stable cache quantity (changes only with the base), vs the reference base which is re-read on `ref` change.
4. **Row_var-axis globalised** (Phase 6, §5) — `OR/pct/color/comp/ci/chi2` and `ref2` are no longer vectorised over row_vars (mirror tables share them). Still per-row_var: `totaltab` and `ref` (named vector = one reference row per row_var; row%/means only, collapses under col% + message). col_var axis stays flexible (`pct/levels/digits` per col_var). Different tables → `list()` → export sequentially.
5. **Totals** (Phase 6, §6) — deprecate `totrow` (always a total row) and **soft-deprecate `totcol`** (Q1: default = exactly one total column, after factor / before numeric cols; old values `each`/`no`/names kept behind `deprecate_soft`, now purely cosmetic — never a calc base); `tab_plain()` = the no-total escape hatch; move/drop via dplyr. The total column shows each row's base as a **display-time `[min;max]` range** across col_vars (scalar when equal; no field overload — §10).
6. **col% + several row_vars** (Phase 7, §7) — manual invert (row_vars↔col_vars, row%) + **opt-in transpose at export** (`tab_kable`/`tab_md`/`tab_xl`); console never transposes; warn on `pct="col"` with several row_vars. `tab_transpose()` integrated/exported here.
7. **Exporters** (Phase 7, §8) — every exporter gets a base method (single tab) **and** a list method (several tabs rendered one-after-another, not merged), plus one shared prep helper preserving export parity. Phase 7 stays on **openxlsx v1**; the **openxlsx2** engine swap is isolated to **Phase 9** (decisions §8).
8. **Deprecations** (Phase 6) — soft-deprecate singular `row_var`/`col_var` (only `row_vars`/`col_vars` remain); drop the `tabxplor.compact` option.
9. **Class model** — keep the `tabxplor_tab`/`tabxplor_grouped_tab` split; `output_list = TRUE` container is a plain list for now. `/dplyr-method` if verbs change.

**Review session 2 (2026-07-07)** — four consistency decisions from the roadmap review (detail: `dev/tabxplor_1.4.0_decisions.md` §14-17):

10. **Weighted inference (Q5, §14)** — one rule for every CI/test: **weighted estimate + unweighted `n`** (for a 0/1 var, weighted-var + unweighted-n ≡ weighted-% + unweighted-n → proportions and means unified). Fixes the §12 self-contradiction. Caveat: anti-conservative under variable weights (`deff→1`); Kish `n_eff=(Σw)²/Σw²` a cheap opt-in (needs `Σw²`, G1). NOT full survey design.
11. **CI ⇄ stars duality (Q6, §15)** — the bracket and the stars must be duals. Significance stars are opt-in; **when on**, `pvalue` = two-proportion **score test** and the stored diff interval switches **AC→Newcombe** (its score dual); `ci="cell"` already Wilson, means Welch-t, OR log-Wald (all duals). AC stays the no-stars default (less golden churn).
12. **`tab_many()` return type (Q7, §13)** — **preserve the list-default** for the soft-deprecated `tab_many` alias; only the unified `tab()` merges by default. No silent return-type break.
13. **Test-result placement (Q8, §16)** — whole-**table** test → table attribute (generalise `chi2`→`test` to also hold ANOVA/F); whole-**column** test → rows of the same `test` tibble keyed by col_var (Q15, review 4 — was: column attribute); per-**cell** significance → the `pvalue` field. Display: a p-value *row* for now; a future `!`-per-cell "weak-test" warning documented.

**Review session 3 (2026-07-07)** — closures from the consistency review (detail: `dev/tabxplor_1.4.0_decisions.md` §15-18 + *Status*):

14. **Numeric diff-color scale (Q9, §18)** — `color="diff"` on numeric columns colors the **sd-standardized** difference (Glass's Δ = `diff/sd_ref`, derived at color time from `diff` + the reference `var` — no new field); default breaks `c(0.2, 0.5, 0.8, 1.2)` as new `mean_diff_breaks`. `$diff` stays raw; `ratio` mode keeps `mean_breaks`; `diff_ci`/`after_ci` unaffected (diff vs its own CI is already unit-free).
15. **Whole-table test slot (Q11, §16-17)** — **hard rename** of the `chi2` table attribute → `test` (constructor arg follows; one tibble holding chi2 + ANOVA/F with a discriminator column); `attr(x, "chi2")` → NULL is an accepted §17 break. Lands in Phase 3 with the chi2-leftovers cleanup.
16. **Stars vs explicit method (Q12, §15)** — the AC→Newcombe switch is **default-sensitive**: only when `method_diff` was left default; an explicit method is respected + one-time message that bracket ⇄ stars are no longer exact duals.
17. **G2 closed + serialization non-issue (§ *Status*, §17)** — vectorised chi2 must match `chisq.test()` defaults **exactly, incl. Yates on 2×2** (today's path calls it with defaults, `tab.R` ~L5290; golden locks it). Old serialized tabs are a non-issue (tabs are exported or re-created from code, never saved as `.rds`) — documented unsupported, no upgrade shim.

**Review session 4 (2026-07-07)** — inference pins + precision closures from the deep review (detail: `dev/tabxplor_1.4.0_decisions.md` §14-16, §19 + *Status*):

18. **Omnibus F weighting (Q13, §14)** — the mean-table Welch F follows the §14 rule (weighted means/variances + unweighted `n`), testing the numbers the table displays; **chi2 stays fully unweighted** (G2 parity) — a documented asymmetry on weighted tables.
19. **Mean CI quantile (Q14, §15)** — a second swap-under-stars pair: mean intervals keep today's `z` (`qnorm`, verified `tab.R` ~L5591) when stars are off, switch to **Welch-t** when stars are on — the dual of the Welch-t `pvalue`.
20. **Per-column tests (Q15, §16)** — per-col_var chi2/F results are **rows of the table-level `test` tibble** (today's chi2 mechanism), NOT a new fmt column attribute — the 8-attribute contract holds.
21. **Empirical-OR reference (Q16, §19)** — keep `ref2="first"` (the maintainer's data puts the positive level first); glm-convention alignment decided at tab_logit integration. Precision closures: the score test is **uncorrected** (Newcombe-10 dual — never `prop.test()`'s Yates default, §15); G1 drops the unweighted moment-sums; **D3** interim — Phase 2 flips numeric `diff` field+display but numeric *color* keeps reading `ratio` until Phase 5; the §10 `[min;max]` range is a **table-level display pre-pass** (`format()` is per-column; Excel may fall back to `min`); `totrow=FALSE` stays cosmetic during deprecation (§6).

#### Verification (every phase)

- **Byte-identity**: `devtools::test("~/github/tabxplor")` after each phase; `test-golden.R` + `test-export-parity.R` + `test-fmt-contract.R` + `test-fuse-parity.R` stay green. Intentional output changes → rerun `dev/make_golden.R`, review the `_golden/`/`_snaps/` diff consciously, `testthat::snapshot_accept()`.
- **Performance**: run the harness (see *Reference > Benchmarks*) before/after Phases 2, 3, 6; save to `dev/benchmarks/results_1.4.0/`; confirm the Phase 3 CI/chi2 win. When past benchmarks on the former tabxplor version are missing, use installed **tabxplor 1.3.1** version.
- **From-the-middle**: feed the same data as microdata / long counts / wide / freq+N → identical fmt tables where `n` is real; CI/chi2 warn+skip on freq-only.
- **Release gate**: `devtools::check()` (~3 min, run manually) before CRAN.

---


### Phase 15 – finalise jamovi module


#### Phase 15a – create Windows-side script to build and test .jmo files (DONE)

Implemented as `dev/build_jmo_windows.R` — a single self-contained `Rscript` (run `Rscript dev/build_jmo_windows.R` on Windows 11 / R 4.6.1). It clones the current branch (default `v1.4.0`, overridable) into a throwaway temp folder, pins `jmvtools` to 2.7.26 + installs deps, `Sys.unsetenv`s `ELECTRON_RUN_AS_NODE`, runs `jmvtools::install(home='C:/Program Files/jamovi 2.7.37.0')` (auto-detected/overridable via `JAMOVI_HOME`), then verifies the landed module (version/rVersion/UI blob) and reports PASS/FAIL.

```r
# To run the script and build the .jmo module out of WSL2, Windows-side
source("//wsl.localhost/dev/home/dev1/github/tabxplor/dev/build_jmo_windows.R", encoding = "UTF-8")
```


#### Phase 15b – jamovi UI `jmvtabreg`

One user-friendly, fast, clear and simple regression analysis, starting from jmvtab template and adapting it to the regression functions and use cases.
- Reuse patterns, UI elements and good ideas from `jmvtab` primarily. Customise .js to grey out options that are not possible with the other selected arguments or outcomes types. When relevant, reuse patterns from known regression jamovi modules.
- Like `jmvtab`, use a consistent cache system to fit with jamovi live UI, where any UI input relaunch the script.
- Fully use the possibility, specific to tabxplor, to compare regression models estimates with their relative empirical/observed quantities.
- A "+" to add predictor subsets to create predictor’s lists for models comparison, selecting, or selecting out, among the already chosen predictors.
- For tests etc., use the dataset `gss_simple <- gss_cat_data_formatting()`, which is classic `forcats::gss_cat` formatted with merged levels for cleaner tables, and first levels chosen to be used as references (for color helpers, regressions, etc.) : I’ll use the same inside Jamovi to test and review the UI.

##### Phase 15b-i (DONE)

The single-model UI is built: 6 files (`jamovi/jmvtabreg.{a,u,r}.yaml`, `jamovi/js/jmvtabreg.js`,
`R/jmvtabreg.b.R`, `R/jmvtabreg-cache.R`) + `0000.yaml` registration + a `test-jmvtabreg-cache.R`.
Covers every family, multi-dependent, `empirical=TRUE`, a per-predictor reference picker, survey weights
(+ an advanced ids/strata/fpc/nest collapse), and Excel/HTML/MD export (reuses `R/jmvtab-export.R`). The
**full fit-level cache** is the `tab_reg(.fit_cache=)` seam above: KB-sized digests, references
reparametrized live with no refit (chosen with the maintainer over serializing raw fit objects). Full
suite green (FAIL 0). **Maintainer step (headless-impossible):** `jmvtools::prepare()` to generate
`R/jmvtabreg.h.R` + the uijs blob, then `install(home='flatpak')` / `dev/build_jmo_windows.R`, then
live-review with `gss_simple`.

##### Phase 15b-ii (DONE)

The model-comparison "+" builder is built. A **Model comparison** CollapseBox holds `compare`
(none/baseline/sequential), a `modelBuilderCtrl` CustomControl (checkbox-grid **cards**: name + a
checkbox per pool predictor + delete + "+ Add model", each card ≥1 var), and `trials`
(off/observed/fixed). Cards store to the hidden `models` Array; `jmvtab_reg_models()` folds them into
`tab_reg(predictors=)` (empty builder → the flat pool = single model; ≥1 card → a named list = model
comparison). **Baseline** = a per-card radio marker → the hidden `baseline` position. **`multiplicator`**
folds into the numeric rows of the reference picker (`× k per unit`; References box → "References and
predictor scaling"), via `jmvtab_reg_mult_vector()`. `tab_reg.R` needed **no change** (feature-complete;
the multiplicator fit-key was already correct). One cache change: the raw-fit ceilings were raised (fit
4→24MB, store 16→96MB) so comparison fits (~9–11MB each) cache instead of graceful-skipping — decided
with the maintainer. Full suite green (FAIL 0). **Maintainer step:** `jmvtools::prepare()` to generate
`R/jmvtabreg.h.R` + the uijs blob, then `install(home='flatpak')` / `dev/build_jmo_windows.R`, then
live-review the builder with `gss_simple`.

##### Phase 15b-iii — remaining polish (deferred)

The per-dependent named `trials` vector (only off/observed/fixed-integer is exposed) is an expert-only
`tab_reg` feature, deferred.



#### Phase 15c — Jamovi UI French translation


### Phase 16 — final maintainer’s review

#### Phase 16a — common framework for summary statistics (DONE)

Design a reliable, readable and user-friendly shared framework to display the "test" attribute, both in a console display of its own as markdown text (displayed above the tibble in console), and integrated in the tables with html, Excel and markdown exports, working consistently accross both `tab()` and `tab_reg()`.
- If some metadata are missing to implement that, let’s think about how to add them in the current framework.
- If the "test" attribute itself must be changed (hard deprecation : it’s a new attribute, never published), and can be changed reliably, we can think about it. It the "test" attribute is used in many other places of the code and changing it would imply a difficult code refactor, we must judge if it’s worthwhile or not.

This is a simplification task : think about what the "test" attribute should be, and what the other table metadata should be, for the whole summary statistics console display + exports to be the more simple, direct, straightforward possible, simplifying the code, while making the result standard, readable and user-friendly.

`tab(gss_simple, c(race, relig), c(party3, tvhours), pct = "row", test = TRUE)`
"party3: Chi2=1.91e+03 (df=6) p=   0%
 tvhours: F= 127 (df=2,2029.3) p=7.99e-51%
 party3: Chi2=2.34e+03 (df=24) p=   0%
 tvhours: F=9.78 (df=8,486) p=1.26e-10%"
- summary table printing in console is really bad, number are unformatted, nothing is padded/align for human readability, several row variables give meaningless results (user don’t know which row it is).
- I want summary statistics, in the "test" table attribute, to have a special method to print in console above the table itself : a user friendly markdown monospace-font-aligned structured table. It should’nt print the whole "test" attribute table (not user-facing, but keep it as it), but a readable simplified table, created at display time before the table.
- It should for example use pivot_wider or a fast equivalent to produce a table matching the structure of the real crosstables (col_vars in columns, row_vars in rows ; tab_vars only where there are real tab_vars with `comp="tab"` and a pvalue per subtable, replaced with a row telling "<row_var>×<tab_vars>" when `comp="all"` and only one pvalue is calculated for the whole table), with clean formatted numbers.
- There is just one red color helper needed when p>=0.05 : for the console it should use only cli colors.
- Make it the fastest possible, since it will be recomputed at every console display.
- It must look a lot like the summary statistics of tab_reg exports, specially with tab_md : start from the "pvalue lines at export" and "summary tables at export" implementations when useful, and **extend it to find a reliable shared framework for the test attribute printing accross `tab()` and `tab_reg()`**.
- It should look like this, with minor variations needed to ensure consistency of the whole "test" table display framework (and one more column with `tab_vars` and `comp="tab"`) :
|       | Tests     |        party3 |   |           tvhours |
|:------|:----------|--------------:|---|------------------:|
| race  | N         |        21 483 |   |            11 337 |
|       | statistic |   1911 (df 6) |   |  127 (df 2; 2029) |
|       | pvalue    | <0.01% (Chi2) |   | <0.01% (F, Welch) |
| ----- | --------- | ------------- |   | ----------------- |
| relig | N         |        21 483 |   |            11 337 |
|       | statistic |  2337 (df 24) |   |  9.78 (df 8; 486) |
|       | pvalue    | <0.01% (Chi2) |   | <0.01% (F, Welch) |
- For exports, keep a single pvalue line like the current implementation by default, but also add a global option to add the possibility to print the three lines in tab() Excel, html or md.

`tab_reg(gss_simple, c("married", "income25k"), c("race", "age"))`
"Model OR (married): N=21 407  LR vs null p=<0.01%  McFadden R2=0.023  AIC=28 933  BIC=28 965
 Model OR (income25k): N=21 407  LR vs null p=<0.01%  McFadden R2=0.017  AIC=27 082  BIC=27 114"

`tab_reg(gss_simple, c("married", "income25k"), c("relig", "age"), split_var = "black") |> tab_export()`
"Model OR (married) | 01-Black: N=3 097  LR vs null p=<0.01%  McFadden R2=0.014  AIC=3 631  BIC=3 686
 Model OR (income25k) | 01-Black: N=3 097  LR vs null p=1.18%  McFadden R2=0.005  AIC=3 695  BIC=3 749
 Model OR (married) | 02-Not black: N=18 210  LR vs null p=<0.01%  McFadden R2=0.012  AIC=24 963  BIC=25 033
 Model OR (income25k) | 02-Not black: N=18 210  LR vs null p=<0.01%  McFadden R2=0.016  AIC=23 308  BIC=23 378"
- Same exact problem here, it’s unreadable, and it should print in console in a structured table highly readable when there are several `row_vars` and several `col_vars` (and possibly `split_var`, which are like `tab_vars` for regressions).
- `split_var` do not appear in exports, so the user basically don’t have the most important information which is that different models where made for different populations / different levels of `split_var`. They should appear in html and Excel the same way `row_vars` name appear with several `row_vars` : in merged cells, with vertical text, in the first column (for `tab()` with `tab_vars`, the only reason they do not appear is because the levels of the tab_vars in written in the subtotals / Total rows clearly.) Ensure the framework is consistent and avoid to create an ad hoc solution just to handle this case if possible.
- It should look like this, with minor variations needed to ensure consistency of the whole "test" table display framework :
|                | predictors   | Model fit         |   married |   |   income25k |
|:---------------|:-------------|:------------------|----------:|---|------------:|
| 01-Black       | relig, age   | N                 |     3 097 |   |       3 097 |
|                |              | LR vs null        |    <0.01% |   |       1.18% |
|                |              | McFadden R2       |     0.014 |   |       0.005 |
|                |              | AIC               |     3 631 |   |       3 695 |
|                |              | BIC               |     3 686 |   |       3 749 |
| -------------- | ------------ | ----------------- | --------- | - | ----------- |
| 02-Not black   | relig, age   | N                 |    18 210 |   |      18 210 |
|                |              | LR vs null        |    <0.01% |   |      <0.01% |
|                |              | McFadden R2       |     0.012 |   |       0.016 |
|                |              | AIC               |    24 963 |   |      23 308 |
|                |              | BIC               |    25 033 |   |      23 378 |

With more predictors, the display difficulty would be to wrap the more predictors names possible in the available space without wasting horizontal space (adding … after the 6th variable if 7 or more) (do the same in html and Excel by merging and wrapping a cell) :

| predictors         | Model fit   |
|:-------------------|:------------|
| relig, age,        | N           |
| rincome, party3,   | LR vs null  |
| long_variable_name | McFadden R2 |
| variable6… +3 vars | AIC         |
|                    | BIC         |


The `test` attribute (chi2/ANOVA for `tab()`, GOF footer for `tab_reg()`) got ONE shared display
framework in new `R/tab-test-display.R`: `test_summary_grid()` (crosstab + reg -> a backend-independent
grid) + `test_render_console()` (a GFM-aligned markdown table printed above the tibble, replacing the
ugly `print_chi2`/`print_reg_footer` lines — both deleted) + shared formatters (`test_fmt_pvalue`/
`_stat`/`_num`) & a `test_cell_label_weak` reused by the inline export appenders. Console mirrors the
crosstable (col_vars in columns; row_vars / tab_vars / `split_var` as row groups; comp="all" collapses
the group to "row_var × tab_vars"); p >= 5% shown red (cli); a chi2 with min expected count < 5 flagged
`!` (console + exports). New `options(tabxplor.test_lines = "stat")` adds a statistic export row above
the p-value row (N omitted — `add_n` shows it); default `"pvalue"` byte-identical. `test` schema dropped
the vestigial `variance` column (10->9; goldens regenerated, variance-only). A reg `split_var` now
renders in HTML/Excel as a merged, VERTICAL first column (`tab-export-prep` keeps it when other tab_vars
are dropped) — previously lost in exports (only `tab_md` kept it). Suite green (0 fail); new
`test-test-display.R`. `var`-column drop in reg html/xl is pre-existing and left as-is.

**Further simplification (same phase):** the two inline-row export appenders (`tab_pvalue_lines` +
`reg_footer_lines`, ~190 L of duplicated fmt-frame surgery) now run on ONE shared engine
`tab_append_footer()` (in `R/tab-test-display.R`) — each is a thin arm-specific config (its `grp_of` /
per-cell builder / non-fmt labels); a `footer_groups` arg lets a crosstab skip subtables with no
computable test. All `test`-display CONTENT helpers moved into that one module (test_display_rows /
pvalue_line_fmt / test_cell_label / reg_footer_spec+siblings / the fmt-cell builders); dead
`chi2`-attribute fallback dropped from `get_test()` (§17: 1.4.0 tabs are re-created, never
deserialized). Byte-identical — full suite green, NO golden/snapshot regen. NOT done: making the
display grid physically drive the export appender — assessed as a net complexity ADD (it would push
export-placement plumbing into the console display model; the CONTENT is already shared via the helpers).


#### Phase 16b — adjusted percentages (DONE)

The maintainer's ruling: `adjusted %` must **always** be the real adjusted percentage, and every empirical
companion must be computed on **exactly the same complete-case population as the model**, by design. Four
changes, all in `R/tab_reg.R` (no fmt-field change, no cache-schema bump; the digest never stores
predictions or empirical columns, so byte-identity holds there):

- **A — adjusted %.** `reg_marginal()`'s `at="average"` prediction switched from `avg_predictions(by=v)`
  to **`avg_predictions(variables=v)`** (marginal standardization). The parenthetical is now the
  covariate-standardized prediction that coheres with the AME (verified: adjusted-%(White) 0.5132 +
  AME(Black) −0.198 = adjusted-%(Black) 0.3152). Also standardizes the multinomial AME `pct` and the
  `estimate_display="prob"` fold.
- **B — empirical on the model frame.** The `reg_build()` empirical loop + multinomial-tip block recompute
  the per-spec **complete-case frame** (`drop_na(data, c(dependent, union_predictors, design_vars))`,
  mirroring `reg_fit()`'s `mdata`) and feed it to `reg_empirical()` / `reg_empirical_tips()` / `var_y`.
  Recomputed from `data`, **not** `fits[[i]]$data` (which is `NULL` on the reref/digest path). For a **model
  comparison** (one crude block, N model frames) the union-predictor complete-case frame is used — the
  shared population where all compared models overlap (and, under `na="drop_all"`, the models' own frame).
  Verified: `Emp. %` cell counts now sum to the model N (12 960), not full-data N (21 483).
- **C — rename.** The header token `(model %)` → **`(adjusted %)`** (one behavioral site + comments).
- **D — `predicted_unadjusted` (new opt-in arg, binomial AME only).** Adds a `Model % (unadj.)` control
  column + an HTML tooltip on the adjusted-% cell showing `avg_predictions(by=v)` (the observed-group
  average). By the logit score-equation identity this **equals the same-frame `Emp. %` exactly** (verified
  to 2e-13) — a pure cross-check that the crude companion sits on the model's population. Column + tooltip
  reuse the existing `empirical_tips` pipeline (no new attribute/field). jamovi exposure deferred (no
  `.a.yaml` option, no `prepare()` regen). One-time `cli_inform` + no-op outside binomial AME.

Tests: the AME-prediction oracle in `test-tab_reg.R` flipped to `variables=`; the empirical header
assertion to `"adjusted %"`; new `test-tab_reg-empirical.R` cases lock B (Emp. N == model N < full N), A
(adjusted%(ref)+AME==adjusted%(level)) and D (Emp.% == unadjusted %). **No golden/snapshot regen** (reg
tables are not snapshotted). Interpretation guidance for the docs is the "Do adjusted % mean something?"
section above (standardization / comparison, never manipulation; Table-2 fallacy).


#### Phase 16c — tab() binary OR calculations, breaks improvements (DONE)

`tab(gss_simple, rincome, married, pct = "row", color = "OR", OR = TRUE)`
- By default, with `OR = TRUE`, `ref2` is 1, so the first level with is often the interesting one for a binary factor, just says "1". I want another default : in reality, for binary factors, odds-ratio do not need a second reference ref2, since the OR of each level is calculated against the other level (none have to show "1", it’s more sound statistically, and as a bonus it shows the beginner user that the OR of the two levels are the inverse of one another) ; keep the `ref2` argument for 3+ levels factors only, where we necessary need to chose a second reference (keep `ref2=1` as default). Also ensure Woolf CI are right for both levels of a binary factor.
- The Total "100%" column (or row with pct="col") is misleading with OR or RRR (they do not add up to 1) : keep the column, but only display the "n= ... " part in console (so the "100%" and the parenthesis are not printed), and only export the n column with no 100% column (or even nothing at all if `add_n = FALSE`)

If have changed `default_color_scales()` to add a specific odds_ratio breaks scale, with default : `odds_ratio = mk_color_scale("or",  list(over = c(1.2, 1.5, 2, 4), under = c(1.2, 1.5, 2, 4)) )` For now they are not wired to anything in the code. Please **modify the code to implement them and integrate them in the current framework completely**.
- Reason : otherwise, if OR use the pct_ratio scale, the user can’t set an asymmetrical pct_ratio scale, often useful to not highlight very small deviations (like : only keep the x2 rule), it also renders the OR scale useless (it should be symmetrical).

`tab(gss_simple, race, party3, pct = "row", color = TRUE, color_signif = "guaranteed_effect", color_breaks = list(pct_ratio = c(NA, 2) ))`
- Here all cells with positive guaranteed effect are colored with the supposedly `x2` background color : "bg ratio: ×1 [significant, error-adjusted]" This is a local failure of the rule applied on "guaranteed_effect" breaks, "substract or divide all breaks by the first break to have 0 or 1 as bound" ; useless information, because they are already cells with text color and the x1 rule tell nothing about effect size ; it’s even worse, here, because x2 is asymmetrical have have no /2, so only positive ones have background.
- **Rule should be** : when both text and background channels are used, if a channel only have one break in "over" (same for "under"), and the resulting "guaranteed_effect" breaks scale is useless (+0, -0, ×1, ÷1), just disable this particular one and remove it’s legend too (here, only pct_ratio have just one break and must be disabled). If both text and background channels are this way, only keep the first channel (text).

##### DONE

Four changes, all landed (full suite green, 3697 pass; only `_color_golden/c_or.rds` + `_snaps/render-html.md`
regenerated + a few value assertions updated):

- **Binary-factor OR** (`R/tab.R` `tab_apply_reference`): the single `refcols` ref2 column became a PER-COLUMN
  `ref_col_idx` — a BINARY col_var (exactly 2 non-Total levels) references the COMPLEMENT level, so both
  levels show reciprocal ORs (neither forced to `1`, ref2 ignored) with a Woolf CI each; 3+ levels stay
  byte-identical (`rep(ridx0, k)`). The shared Woolf block's gate was rewritten (it keyed on a
  self-referencing `refcols_vector` column, which for binary is empty → it silently skipped both CIs). The
  bare-`1` display follows automatically via `get_reference()` (no fmt_class change). pct="col" binary
  mirror DEFERRED (row axis needs a per-comp-group complement; noted).
- **odds_ratio colour scale** (`R/tab_classes.R`, `R/fmt_class.R`): `mk_color_scale()` accepts the new
  `odds_ratio` (multiplicative, center 1); `default_color_scales()` wires it; `fmt_color_plan()`'s `or`
  measure reads `sc$odds_ratio` (was `sc$mean_ratio`). The maintainer's symmetric `pct_ratio` /
  `mean_ratio` WIP defaults are KEPT — OR no longer borrows a ratio scale, so `pct_ratio` is free to be
  asymmetric. `set_color_breaks(odds_ratio=)` / `tab(color_breaks=list(odds_ratio=))` work.
- **OR total column** (`R/tab.R` `tab_is_or_display` / OR-aware `tab_fold_addn_incell` / `tab_or_total_col`
  wired into `tab_materialize_extras`): an OR table (displayed `or`/`or_pct`) drops the meaningless
  "100%" — console shows only `n={n}`, Excel exports only the base-`n` column, nothing when `add_n=FALSE`.
  Scoped to pct="row" `OR = TRUE`; pct="col" total-ROW deferred with the binary mirror; the string forms
  `OR="OR"`/`"OR_pct"` build no total column at all (pre-existing `tot`-resolution quirk).
- **Degenerate guaranteed_effect channel** (`R/fmt_class.R`): `fmt_color_plan()` returns a `degenerate`
  flag (guaranteed_effect + single-break-per-side scale, pre-offset, excluding `color="ci"`); the new
  shared `resolve_color_channel_plans()` (used by BOTH `fmt_color_channels` + `legend_specs`) drops a
  degenerate channel and its legend line, but never the last one (a lone/both-degenerate table keeps the
  text channel). `fmt_get_color_code()` (single-channel golden) is left un-arbitrated.


#### Phase 16d — color legends and table footers improvements (DONE)

`tab_reg(gss_simple, "married", c("race", "age", "rincome", "relig"), empirical = TRUE)`
"Emp. OR: OR (ref.): 1/4 1/2 1/1.5 1.15 1.5 2 4 [grey: non-significant or under ×1.15]
 Model OR: OR (ref.): 1/4 1/2 1/1.5 1.15 1.5 2 4 [grey: non-significant or under ×1.15]"
- Here the legend is repeated, either though by construction the colors reads the same for empirical OR and modelised OR : the main modelised quantity and the related crude/empirical quantity should have a unified legend.
- It’s even worse in the full legend (html, Excel), where the 5 lines block is duplicated with the only difference being the leading "Emp. OR —" or "Model OR —".
- I want you to **redesign the shared functions for color legend**, with this simple rule : **if different columns have the same color measure**, they should share their legend block, starting with the related list of variables, for example "Emp. OR, Model OR — Shades of blue:..." Display the name of the first six variables that have this legend, then "… +2 vars". It’s very rare that different columns of the same table have the same color measure bet not the same color_signif, so in this case duplication is ok.
- Note : tab already mostly have the right no duplication behaviour, for example `tab(gss_simple, race, c(married, income25k), pct = "row", na = "drop",color = "ratio", color_signif = "grey_non_signif")` only have one legend block for both col_var. But adding `color = "OR"` duplicates the legend : `tab(gss_simple, race, c(married, income25k), pct = "row", na = "drop", OR = "OR", color = "OR", color_signif = "grey_non_signif")`. Result :
    "01-Married, 01-$25000 or
    more — Shades of blue: OR ≥ 1.2; 1.5; 2; 4. Shades of yellow to red: OR ≤ 1/1.2; 1/1.5; 1/2; 1/4. Coloured: significantly different from the reference category (White) (Wald interval on the log odds-ratio, 95% confidence), by at least the first colour threshold. Uncoloured: either not significant, or a difference under ×1.2.
    02-Not married, 02-Less than
    25k — Shades of blue: OR ≥ 1.2; 1.5; 2; 4. Shades of yellow to red: OR ≤ 1/1.2; 1/1.5; 1/2; 1/4. Coloured: significantly different from the reference category (White) (Wald interval on the log odds-ratio, 95% confidence), by at least the first colour threshold. Uncoloured: either not significant, or a difference under ×1.2."
- Also, you can see in the above legend that there a strange line breaks appearing where they should not, in the middle of the levels names. The same happens to the one below, after "Model AME".
- `tab_reg(gss_simple, "married", c("race", "age", "rincome", "relig"), effect = "ame", empirical = TRUE)` Here a verify small difference, "AME ≥ +5..." on one side, "cells ≥ +5..." on the other, create a useless duplication. Please find a way to integrate this color legend framework better to avoid such duplications on small irrelevant details (here : what is specific to the logistic regression model and AME must live in the "Model:" part of the legend ; everything common must be shared with the empirical counterpart ; if the confidence interval is not the same, well this is a statistically problem we must resolve, since the rule is : for each empirical counterpart of the modelised quantity, we find the ci calculation that matches the one in the model best when the model only have one predictor). (Same problem with `family = "gaussian" + empirical = TRUE`, and `family = "poisson" + empirical = TRUE` ; poisson is even worse, it duplicates the same legend *three times*.) Here the full legend is :
    "Model: logistic regression; marginal effects on the probability scale (percentage points) (sample-averaged); each cell shows the effect vs the reference level and, in parentheses, the adjusted predicted probability.
    Model AME
    (adjusted %) — Shades of blue: AME ≥ +5; +10; +20; +30 points. Shades of yellow to red: AME ≤ -5; -10; -20; -30 points. Coloured:    significantly different from the reference category (Wald interval, 95% confidence), by at least the first colour threshold.    Uncoloured: either not significant, or a difference under ±5 points.
    Emp. %, Emp. diff — Shades of blue: cells ≥ +5; +10; +20; +30 points. Shades of yellow to red: cells ≤ -5; -10; -20; -30 points. Coloured: significantly different from the reference category (Wald interval, 95% confidence), by at least the first colour threshold. Uncoloured: either not significant, or a difference under ±5 points."
- More generally, **I want you to make a structured and thorough inspections of the color legends**, visually reviewing the resulting tables of the rendered tables of the introduction vignette and regression model vignette, and maybe in other relevant tests, **to find possible inconsistencies, statistically absurd things, confidence intervals no applying to the right quantities, useless duplications, possible improvements of clarify and precision, and the like.**.
- For every duplication or near-duplication, ask yourself : how to remove it without creating inconsistencies on near identical cases with a few different details ? Then ask yourself : what too detailed informations should we remove in order to be able to merge the legend in a consistent way ?

`tab_reg(gss_simple, "married", c("race", "age", "rincome", "relig"))`
"Model: logistic regression; odds ratios (vs the reference category)."
- Here, the "vs the reference category" is misleading : binary/standard odds-ratios are always calculated against `1-p` / the other category. For 2-level only, replace with "odds ratios (vs the second category)".

`tab(gss_simple, race, party3, color = "contrib")`
- The simple legend in console says : "contribution to Chi2 (indep.): ÷10 ÷5 ÷2 ÷1 ×1 ×2 ×5 ×10" That’s not clear, the user must know it’s compared to the **mean contribution**. And the underrepresented part is false : "negative" colors are also the mean contribution ×1 ×2 ×5 ×10, but with another sign ! Verify if only the legends are wrong, or if the code have been messed up (CRAN tabxplor 1.3.1 was ok, but we may have broken it).
- When a weight variable is provided, always start legend/table footer with "Weighted by {wt}." ("Pondéré par {wt}." in French translation).

`tab(gss_simple, rincome, tvhours, color = "diff" , color_signif = "grey_non_signif", color_breaks = list(mean_diff = c(0.4, 0.8, 1.6)), ref = 1)`
- with a custom scale for mean differences, the console legend still says "standardized difference (1-Lt $10000) ... [grey: non-significant or under ±0.4 SD]". It is a legend error, or a code error (custom scales for mean diff not working anymore), or do we chose to never implement the "user provides custom scale means it’s not standardised anymore", or is it implemented but recalculated in "number of sd" for the legend only  ? Also check the full legend of exports.


`tab(gss_simple, race, party3, pct = "row", display = "{ci}", stars = TRUE)`
- A legend is needed for significance stars. Here is a French version, to keep for French but to translate in English : "*** : chiffre significativement différent de celui de la modalité de référence (en gras), au seuil de confiance de 99  % ; ** : au seuil de 95  % ; * : au seuil de 90  % ; aucune étoile : non significatif."

##### DONE

Legend de-duplication (issues 1–3). legend_specs() now reconciles a tab_reg(empirical=TRUE) table's crude + modelised columns per col_var (shared reference label; the additive AME/β subject neutralised to "cells" only when an empirical sibling exists, so a lone AME/β table keeps its word) and drops role from the grouping key, so companions fold into one line prefixed by the columns they cover ("Emp. OR, Model OR — …", first 6 then "… +N vars"):

binom OR + empirical: 3 → 2 lines · AME + empirical: 2 → 1 · gaussian: 2 → 1 · poisson: 3 → 2
Issue 3 (OR crosstab) already folded on gss_simple — so I skipped the belt-and-suspenders change that would have degraded the correct "(White)" label.
Wrapping (issue 2). legend_name_list() normalises prefix names (strips the html-path <br>/U+202F, protects intra-name spaces with U+00A0) — "Model AME (adjusted %)" no longer breaks mid-word.

contrib (issue 5). Verified the colour computation was correct (over/under split by sign — not broken vs 1.3.1); only the legend was wrong. Now "×N the mean contribution" on both sides (no misleading ÷), and it no longer prints stars it never opted into (fmt_stars_applicable).

mean_diff (issue 7, your call: raw). Custom mean_diff breaks now read as a plain "difference" (no "SD"), driven by the scale's std flag — a 3-way pct / SD / raw mode kept consistent with how the cells colour.

New: stars legend (issue 8) + "Weighted by <wt>." footer (issue 6) — wired through console, markdown, HTML and Excel; the weight name is persisted on the table (vars attr / reg_meta).

Empirical CI (your scope decision). The binomial risk-difference companion now uses the two-proportion Wald interval, matching the model AME's Wald, so the merged legend names one honest method.


#### Phase 16e — further simplify and integrate the legend/footer system (DONE)

After 16a + 16d, a table is wrapped by three separate explanatory-text subsystems, each with its own per-medium rendering and its own threading into the backends:
- Colour legend — tab_color_legend() → token stream → 5 media.
- Test / GOF grid (16a) — test_summary_grid() + test_render_console() / export appenders, rendered in a different position (above the table on console, inline rows in exports).
- Three ad-hoc one-liners — weight, reg Model: line, stars — threaded by hand at every footer site.
The real cost isn't any one of these — each is clean. It's the orchestration layer: ~16 helper calls across ~5 backends (tbl_format_footer, md_render_one, tab_kable, tab_xl, tab_plot), each re-specifying what goes below the table and in what order. That ordering is duplicated 5×.

How to further simplify and integrate the whole color legend framework at package level (tab() + tab_reg() ) ?

1. One footer model + one per-medium footer renderer (highest value, heaviest)
Define the below-table footer once as an ordered list of typed blocks — {kind:"plain", text} for weight/model/stars/subtext, {kind:"legend", tokens} for the colour legend — and let a single render_footer(blocks, medium) dispatch per kind. Each backend calls it once.

Gain: the 5-site ordering dup collapses to one definition; a new footer element becomes one block, not five edits; weight/stars/model stop being special-cased.
Give up: nothing functional — no backend needs independent ordering. But it touches all 5 backends → real regression surface (snapshots). The test grid can't fully join (different position) but could share the plain-block renderer + the gettext/lang plumbing.

2. Replace the hand-picked sig with body-text grouping (clean, low-risk win) legend_canonicalise_reg() (16d) now makes "same rendered body ⟺ mergeable" actually true, which is exactly the precondition the earlier design lacked. So group columns by their rendered prose body (minus prefix) instead of the 10-field sig string.

Gain: removes a whole bug class — the model had to extend sig with is_pct in 16d; forget that and the grouping silently drifts from what renders. Body-grouping can't drift.
Give up: a negligible double-render. This is the one I'd do first.

3. A per-measure descriptor table (medium value)
Measure facts are scattered across five functions: word (legend_measure_word), break glyph (legend_break_label), unit (one_side), reference concept (legend_ref_info), CI-method family (legend_method_name). Collapse to one MEASURES[[m]] = list(word, glyph, unit, ref_kind, …).
Gain: the contrib-vs-ratio divergence just fixed (÷ vs ×) becomes a data field, not a switch arm you can forget; adding a measure is one row.
Give up: a little indirection.

4. Consolidate the reg-specific legend logic — is_reg branches live in four functions. A single "reg legend adapter" that normalizes a reg column into a plain spec would let the core assemblers stop knowing regressions exist, so tab() and tab_reg() truly share one core.

5. Keep the terse console form for the legend, and add the possibility to use it in exports using a global option.

##### DONE

**Body-text grouping** (legend_group_by_body) replaces the hand-maintained 10-field `sig` string — two columns share a legend line iff they render identically, so a line can never drift from what it describes (the 16d `is_pct`-in-sig patch is now moot).
**`MEASURES` fact table + resolve-into-spec** (legend_resolve_spec, legend_reg_adapter) — every per-measure/per-channel fact (word, glyph, reference, unit, method) is resolved into the spec once, so legend_tokens_terse/_prose are now **dumb templates with zero `switch(measure)` and zero `is_reg` branches** (verified). tab() and tab_reg() truly share one legend core.
**Zero-kind footer streams** — tab_footer_streams() + render_footer() are the one definition of what goes below a table and in what order. Every footer line is a token stream (a plain one-liner is just a 1-token stream — `legend_render_line()` already renders uncoloured tokens, so no plain-vs-legend dispatch). This replaced the **5× re-ordering** across console/md/html/Excel/plot and the 2× field pre-compute in export-prep (reg_line/weight_line/stars_legend deleted).
**Plot parity + terse option** — `tab_plot()` now draws the full footer (weight/`Model:`/stars/subtext) and its `caption`, both previously silently dropped; `options(tabxplor.legend_style = "terse")` switches exports to the compact console legend.

A few honest notes for you :

**Where I diverged from the roadmap**: I used zero-kind streams instead of its `{plain}/{legend}` two-kind dispatch (you approved this) — it reuses the existing renderer rather than adding a parallel plain-text one.
**One latent bug fixed in passing**: the md backend used to call `tab_weight_line(rd$tab)`, which is stripped for transposed tables — the unified builder standardizes on the fmt source (`rd$color_src`), so a transposed weighted table now keeps its weight line.
**The test/GOF grid deliberately stays on its own rail** (console = above the table, exports = body rows via fmt-frame surgery) — that position/mechanism split is load-bearing and I did not force-merge it, as flagged during planning.
**Out of scope (16d wording, untouched)**: the reg legend still says "odds ratios (vs the reference category)" for binary factors — the "vs the second category" refinement is a Phase 16d item, not 16e.

#### Phase 16f — Dark mode colors in positron console, ci and stars improvements

Finally, is there a reliable way to detect Dark mode in Positron, in order to use Dark mode colors in it’s R Console automatically ? Look at dev history in `dev/`, I remember we found a Positron way for html at a point, then implement the most reliable solution.

`tab(gss_simple, race, party3, pct = "row", ci = "diff", display = "ci")`
    'Error in `validate_display_template()` at tabxplor/R/tab.R:671:3:
    ! Invalid `display` value "ci".
    ℹ Composite display uses a {} template listing the fields to combine, e.g. `{pct} (n={n})` or `{diff}
      [{ci}]`.'
- `display = "ci"` should still work to display the confidence interval, internally mapping to the right custom display.

`tab(gss_simple, race, party3, pct = "row", display = "{ci}", stars = TRUE)`
- No stars appear, since color_signif is "ignore", but with no message : if user forces to `stars = TRUE` with or without colors, ci should be overriden to `"diff"` if not set, for the stars to appear. 
- works well : `tab(gss_simple, race, party3, pct = "row", ci="diff", display = "{ci}", stars = TRUE)`

##### DONE

Three fixes; full suite green (PASS 3711, +2 tests), only `man/tab.Rd` regenerated (no golden/snapshot churn).

- **Positron console dark mode** (`R/tab-theme-detect.R`): the detector already existed (14g) but
  `tx_ide()` gated Positron on `POSITRON`/`.Platform$GUI`, which this WSL2 remote leaves empty (only
  `VSCODE_CWD` set) -> misclassified `"vscode"` -> `"light"` while the real theme was dark. Now Positron =
  a VS Code fork WITH the server cache: a `VSCODE_*` var AND `dir.exists(~/.positron-server)`. New
  `tx_positron_server_dir()` (one root, injectable `positron_dir` arg for tests). The ark console keeps
  working via `GUI=="Positron"`; the new clause rescues the terminal/extension-host where the env vars
  are unset (verified live here: ide=positron, theme=dark). One-shot at load (maintainer confirmed a
  restart fixes it); `set_color_palette(theme="auto")` still refreshes mid-session.
- **`display = "ci"`** (`R/fmt_class.R` `validate_display_template`): a bare KNOWN field (no braces) is now
  wrapped to its `{}` template, so `display = "ci"` == `"{ci}"` (and `"diff"`/`"pct"`/...). One general
  rule; unknown bare values (`"foo"`) still abort.
- **`stars = TRUE` with unset `ci`** (`R/tab-resolve.R` gains a `stars` arg + one forcing line, wired from
  `tab.R:1639`): stars are cut from a stored `pvalue` that only exists alongside a difference CI, so
  `stars` now forces `ci="diff"` on pct row/col + mean columns (NOT OR -- its own pvalue via the OR path).
  Runs AFTER colour resolution (never flips a plain `diff` colour to the gated `after_ci`). NB: OR reaches
  the resolver as a LOGICAL (stringified only in the leaf), so the exclusion uses a robust `or_on`, not the
  string-testing `auto_or`. Byte-safe (stars default FALSE). Because tab()'s `ci` default IS `"no"`, an
  EXPLICIT `ci="no"` is indistinguishable from unset and is also forced (stars win).
- **jmvtab-cache consistency** (`R/jmvtab-cache.R`): the resolved `ci` (drives the tuple + armed build +
  tier-3 reref) now mirrors the stars forcing, else an explicit `ci="no"`+`stars` armed a pvalue the reref
  never refreshed (reref != rebuild). One line beside the existing `auto->diff` numeric nudge.
- **Console bold** (`R/fmt_class.R` `pillar_shaft`, follow-up): the console can now embolden cells, gated
  to front-ends that render ANSI bold at FIXED glyph width (verified: Positron + VS Code's xterm.js; NOT
  RStudio, which draws bold wider -- rstudio#1721). New option `tabxplor.console_bold`, seeded at `.onLoad`
  via `console_bold_default()` = `tx_ide() %in% c("positron","vscode")` (guarded by is.null, so a
  `.Rprofile` choice survives; read fresh at print so a mid-session toggle applies). The bold SET is
  export-parity: coloured branch bolds `totals | text_slot>0` (anchors + text-coloured cells, matching
  `fmt_col_ann()`'s `bold = !is.na(text_hex) | keep_black`); the else branch (uncoloured cols, incl. the
  Total col) bolds `totals` (anchors) only. pillar measures ANSI-stripped width so bold adds none. Tests
  pin `console_bold=FALSE` in `setup.R` (IDE-independent suite; ANSI is off under testthat anyway) and
  force `cli.num_colors` on to assert the emboldening. Maintainer confirmed alignment holds in Positron
  with a scattered per-cell bold+colour grid.



### Last Phase — final simplifications and package user-friendly documentation


#### Last Phase a – Bug corrections

##### Last Phase a – Bug corrections (round 1) (DONE)



#### Last Phase b – rethink package dependencies

#### Laste Phase b-i – package dependencies pass 1 (DONE)

Package dependencies : are there Imports or Suggests that are used very little ? Imports and Suggests that in general could be easily replaced with custom functions, or by copying a hand of opensource functions (thanking authors in the code) ?

Are there Suggests that we should better add to Imports, since they are important for many functions ? Adding `broom::` in Imports to be able to use `tab_reg()` natively in all cases, and only Suggests the packages necessary for more specific models ? Adding what else ? How many packages is it recommended to have at maximum and, particularly, after which threshold is CRAN currently giving a R CMD CHECK Note (do web searches) ?

Among the new global options created in 1.4.0, are they all useful and clearly named and documentated ?

Done: `broom` Suggests→Imports (common `tab_reg()` models native; model-specific back-ends stay
Suggests). `htmltools` + `knitr` Suggests→Imports (core render paths) so `kableExtra` Imports→Suggests
(default `html` engine is dependency-free; legacy `engine="kableExtra"` + `kable_tabxplor_style()`
now guarded). `crayon` dropped entirely → console colours built with `cli` (already a dep; internal
palette slot `e$crayon`→`e$ansi`, public `get_color_style(mode="crayon")` frozen for back-compat).
Dead `grDevices` removed. Non-default Imports = 18 (CRAN NOTEs at ≥20). New `?tabxplor-options` help
page documents every `tabxplor.*` option. Fixed 2 option-default inconsistencies (`totcol_range` set
in `.onLoad` "off", read one place; `cleannames` fallback FALSE everywhere). Suite green (PASS 3609),
no snapshot churn. NB: `document()` also materialised the pending Phase-15b `export(jmvtabreg)` +
`man/jmvtabreg.Rd`.

#### Laste Phase b-ii – package dependencies pass 2

Study if it would be possible to replace all `stringr::` calls to `stringi::` calls, since `stringi::` is used anyway but mostly for unescape unicodes and encoding (if there’s a non stringi way to do that without adding other dependency, I’m intereste.

Study if it would be possible to pass knitr:: as Suggests, instead of import, since kable is now opt-in the the default html tables are custom.

Is lifecycle really needed in Imports, if it mostly helps to generate documentation at dev / roxygen time ? 

Remove `magrittr::` from dependencies altogether, replace all `%>%` pipes with native R `|>` pipes. 
- You must look for all `%>%` that are still used in a way `|>` can’t directly replace, for example passing the piped argument at different places using the `.` syntax, like `%>% purrr::discard(., .)`.

Remove labelled:: form Suggests, since it’s possible to read and write variable labels with `attr()`/`attr<-`() with the package. There is only one use in the current code, in `R/utils.R` : replace `labelled::get_variable_labels()` with simple attributes reading, giving exaclty the same kind of resulting object than `labelled::get_variable_labels()`. 

Is VGAM really needed in Suggests, since we only use svyVGAM and it’s already there ?

In the case we manage to reduce the Imports number, to still pass the CRAN R CMD CHECK of less than 20 imports, the Suggest packages I would want to add to Imports are, in this order (we just move the first ones until we get to 19 ; the first three are specially important to me) : survey, marginaleffects, nnet, svyVGAM, openxlsx2, MASS, brant.




#### Last Phase c – code and framework simplifications (DONE)

How to further simplify tabxplor package framework ? Do four round of simplification, each on a fresh Claude Code session.
- How to further integrate the internal functions into a reliable and simple ecosystem aimed at global code simplification ?
- What features and ad hoc parts of the code are white elephants, that could be removed and integrated in a common global framework without meaningful losses for the user ? What should we give up or modify to enable a global simplification of some functions and code ?
- What are the missing attributes, at table-level, column-level or fmt_cell-level, that would be necessary for a more reliable and straighforward architecture, or that would be necessary for further simplifications of the code/of the arguments ? At the contrary, what are the attributes that seem ad hoc, unnecessary, adding useless complexity to the code, and how to remove or modify them for simplification ?
- What new arguments of v 1.4.0 could be merged or redesigned for simplicty of use, consistency and clarify ?

##### Last Phase c-i: internal-function ecosystem simplification (round 1) (DONE)

Remove verified-dead internal code so the internal surface reads as one
reliable ecosystem instead of accreted dev leftovers. Every removed function
is non-exported, non-S3, and has zero live callers (checked across
R/, tests/, jamovi/, inst/).

R/utils.R (1481 -> 938):
- dead factor-helper cluster: fct_to_na / fct_replace / fct_rename /
    fct_detect_replace / fct_detect_rename / fct_case_when_recode /
    fct_levels_from_vector (self-contained, superseded by fct_clean +
    the exported fct_recode_helper)
- dead vendored map cluster: pmap_if / map2_if / probe / as_predicate
- dead singletons: get_user_documents (superseded by resolveExportPath's
    getHome), prepare_fct_recode, bind_datas_for_tab
- dead commented-out blocks: old fct_clean, formats_SAS_to_R
  Kept: tr_/ po_to_dt (upcoming Phase h French translation may reuse them).

R/tab_classes.R: drop dead `untab` + ~90 lines of half-commented dead code
  in tab_plot()'s legend block (flagged in the 14c dev notes).
R/fmt_class.R: drop dead commented switch() in fmt0().

##### Last Phase c-ii: option single-source + honour tabxplor.conf_level (round 2) (DONE)

The white-elephant fruit was already cleared in earlier phases (no dead
option remained), so this round tightens config consistency instead.

- .onLoad is now the single source of truth for two stray defaults that lived
  only at their read sites: seed `tabxplor.conf_level` (0.95) and
  `tabxplor.xl_or_numeric` (FALSE), matching the stated architecture rule.
- `tabxplor.conf_level` now does what its doc claims. It used to be read in
  exactly ONE place (the contrib colour-significance alpha) while tab()'s
  interval CIs used a hard-coded 0.95 arg default. The public entry points
  tab() / tab_many() / tab_num() / tab_ci() / tab_reg() / tab_logit() /
  multi_logit() now default `conf_level = getOption("tabxplor.conf_level",
  0.95)`, so the option is the global default and the per-call argument still
  overrides it. Default value unchanged (0.95) -> byte-identical goldens.
  New lock-in test in test-calculations.R (option widens the CI monotonically;
  arg overrides the option).
- Retire the dead `tabxplor.pvalue_lines` option: its .onLoad seed was already
  commented out and its only reads were dead commented lines in tab.R.
- Doc drift: correct the CLAUDE.md repo map (removed tab_logit*.R; jmvtabreg.h.R
  now exists) and the conf_level option help.

Deliberately NOT touched (agent-confirmed, retro-compat-constrained): the
experimental `conditional_format` arg (maintainer may still build it) and the
`totcol` legacy-value parser (needs a deliberate consolidation, not a sweep).

##### Last Phase c-iii: attribute audit -> correct stale docs (round 3) (DONE)

Full audit of the 18 fmt fields, 9 column attributes and 7 table attributes
(usage mapped by grep across R/, NAMESPACE, tests/). Honest outcome: the
1.4.0 combined field surgery already left the attribute set lean and correctly
placed -- there is NO safe, high-value structural consolidation left:
- all 18 fmt fields are user contract ($/mutate) and none is vestigial;
- all 9 column attributes have EXPORTED getters AND are required per-column
    so format()/the colour engine work on a standalone extracted fmt column
    (the apparent redundancies -- refcol/in_refrow, totcol/in_totrow -- are
    orthogonal column-vs-row encodings, not duplicates);
- the 7 table attributes are already threaded through one shared tab_attrs()
    line each, so merging the 5 scalar metadata lists would be high churn for
    little gain (and touches the exported new_tab() formals).

So the round's real deliverable is fixing stale documentation the audit
surfaced (which would otherwise mislead future attribute work):
- the `mean`-field overload is GONE (Phase 5 landed): mean is now mean-only
    on type=="mean" columns, the pct "*2 rule" ratio lives in the `ratio`
    field, and the colour engine reads get_ratio(); CLAUDE.md + the
    architecture doc still described this as a not-yet-done Phase 5 item, and
    the architecture doc contradicted itself (line 302 vs 33/304).
- add the missing 7th table attribute `ci_settings` to the CLAUDE.md list.

##### Last Phase c-iiii: rename multiplicator -> multiplier; new-arg review (round 4) (DONE)

Fourth simplification round: review the NEW v1.4.0 arguments for merge/rename
BEFORE the CRAN freeze (they're never-released, so still free to change).

The one outright naming defect: `multiplicator` is non-idiomatic English for
what every stats audience calls a **multiplier**. Renamed the R-facing
argument on tab_reg() / tab_logit() / multi_logit() + all internal plumbing
- tests. The jamovi module is deliberately untouched: the internal jamovi
option KEY stays `multiplicator` and jmvtabreg.b.R bridges it to the renamed
`multiplier` arg, so NO `jmvtools::prepare()` regeneration (which recompiles
the uijs blob) is needed and the module keeps working as-is.

Reviewed but deliberately NOT changed:
- The five `method_*` args: merging into one named `method` vector would lose
  autocomplete discoverability + per-slot validation for rarely-touched expert
  knobs -- a net regression. Kept.
- `output_list` / `color`+`color_signif` / `var_names` / `stars` / the
  `stats`/`compare`/`baseline` group: already well-designed and consistent.
- `estimate_display` value collision: its "ame"/"prob" values are also jamovi
  option values, so renaming them ("with_ame"/"with_prob") would need a jamovi
  bridge or a maintainer prepare() -- net complexity for a subtle, documented
  clash. Deferred; instead the roxygen now explicitly distinguishes
  `estimate_display = "ame"` (adds an AME beside the OR) from `effect = "ame"`
  (the whole column IS the AME), which is the actual confusion.


#### Last Phase d – make tab() / tab_reg() docs approachable for beginners (DONE)

Simplify `tab()` and `tab_reg()` and other main functions documentation, to make it more easily understandable and more helpful to students that are not statistical experts and may be true beginners with programming. And less terrifying – because the length of the current documentation may be terrifying for newcomers in R (specially my literary sociology students).
- Would there be possibilities to nest some of the more complex argument in other functions ? For example, all the complex customisation things about ci refer to tab_ci(), with a link for the user to go further if he wants to ? All the complex things about color customisation somewhere else ? All the helpers set / get etc. somewhere else too, but with a ling to them somewhere in tab() page. What else could be grouped and put out of the main user-facing functions documentation ?
- The order of the arguments matters, what comes first is / must be what really matters for base users/beginners (like variables, percentages, colors, etc.)

Can you think about remaining possible simplifications of the arguments themselves, specially the new arguments introduced in v 1.4.0, since once they become public it will be difficult to modify them in next versions ? How could the main user-facing functions be more user-friendly ?

The two flagship functions have huge argument lists (tab() alone documents 42
params) that read as a terrifying wall to newcomers. Add a beginner on-ramp
without touching any signature (doc-only, zero behaviour/test risk):

tab():
- Warmer @description that says what the function does in one breath and tells
  a newcomer the four arguments to start with (data/row_vars/col_vars/pct),
  plus a pointer to vignette("tabxplor").
- New @details "which arguments to learn first" MAP: the args grouped by
  purpose (the table / what each cell shows / colors / comparisons / statistics
  / totals & missing / advanced), so a beginner can navigate instead of reading
  42 params top to bottom, and the complex CI-method knobs are pointed to
  tab_ci() where they are fully documented.
- @seealso rebuilt into a helper map (tab_many, tab_reg, tab_ci, the color
  setters, tab_chi2/tab_pct/tab_tot, the four exporters, tabxplor-options).

tab_reg():
- @details opens with the three-argument first model + how the family is
  auto-detected + the empirical crude-vs-adjusted idea + a vignette pointer,
  then the same purpose-grouped argument map.

Deliberately NOT done: physically reordering the @param blocks. tab()/tab_many()
/tab_num() share near-identical @param text, so string-moving a block risks
editing the wrong function's docs; the signature/usage already lists
pct/color early and the new @details map gives the "essentials first" guidance
the reorder was meant to provide.



#### Last Phase e – Create meaningful and user-friendly vignettes (DONE)

Each vignette must be user-friendly, understandable by novices for the base crosstables one and regression models one, while still having just enough technical detail for the experts to known exactly what important technical choices were done internally.
- For each vignette, carefully study the dev history in `dev/tabxplor_1.4.0_decisions.md`, `dev/tabxplor_1.4.0_roadmap_DONE_PHASES.md`, or other `dev/` .md when relevant : the aim is of course not to give the user any information about how the package was implement (would be useless to him), but to retrieve the more data possible about what were the intended real world use cases of each option, then **select** which part is **really** important for the user.
- For real-world examples, use `gss_simple <- gss_cat_data_formatting()` (exported), which is classic `forcats::gss_cat` formatted with merged levels for cleaner tables, and first levels chosen to be used as references (for color helpers, regressions, etc.).

##### Last Phase e-i – rewrite the introductory vignette for beginners (DONE)

The current vignette should be the simple and useful basis for non-expert users, a light and direct introduction to what tabxplor do better than other packages (but with more humility than that !) : color helpers, references and confidence intervals for crosstables (factors and means), with exports, etc. It shall also permit expert users to understand what this package is really interesting for, by giving only the really necessary technical details. Maybe first a simple explanation about what do with color helpers, without significance ; then a concrete explanation of color_signif, for exemple "guaranteed_effect" to highligh all significant on tables from small samples ; and add, somewhere, the measure×color_signif summary table for experts, and other, to know exactly what are the possibilities.

Something very close is what’s to be used for `README.Rmd` (never edit `README.md` manually). Or maybe do a much more concise introduction in the `README.Rmd`, presenting only the really interesting features of tabxplor for exploratory analysis (mostly colors helpers for crosstables, possibly taking significance into account, with at the end a last example of logistic regression with a meaningful comparaison of modelised quantities versus empirical/observed quantities) ?

Rewrite vignettes/tabxplor.Rmd around the current 1.4.0 API and a beginner
path. It used deprecated forms (sup_cols, chi2 =, color = "diff_ci"/"after_ci");
now it uses col_vars + levels = "first", test =, and the color / color_signif
split, on the shipped gss_simple = gss_cat_data_formatting() dataset (tidy
merged levels; first level = reference).

Structure: first crosstables (counts / pct / means / several col_vars) ->
sub-tables -> COLOUR HELPERS without significance (color = "diff" / TRUE, and
references ref/comp) -> then colours that RESPECT SIGNIFICANCE (color_signif =
grey_non_signif / guaranteed_effect, the latter for small samples) ->
confidence intervals, tests, contributions -> exporting -> dplyr -> an EXPERT
reference table of color x color_signif -> where to go next.

Rendering: the vignette shows tables as coloured console output turned to HTML
(cli + fansi), the way a console user sees them; a report would use tab_kable()
/ tab_xl() (shown in the Exporting section). Verified: rmarkdown::render()
produces the coloured tables (blue/red/grey spans + legends), no errors.

Also records a bug found while writing it (CLAUDE.md discovered-bugs + an
in-code KNOWN-BUG tag at tab.R:2219): options(tabxplor.output_kable = TRUE) +
a two-channel colour errors on auto-print; the real export tab_kable() and the
console path both work, so the vignette sidesteps it.

##### Last Phase e-ii – add the tab_reg() regression vignette (DONE)
tab_reg should come with it’s own very detailed vignette
- A section for each kind of regression model : binomial, gaussian, poisson, etc. Explain how to use weighted models,  xplaining clearly and simply for beginners what is the chosen framework for weights (see dev history) and how to use simple survey weights (referto survey:: documentation for more complex cases, stating cleardy that stratified surveys can gain a bit of precision an narrow a bit confidence intervals if the strata variables are given).
- Meaningful examples in each section, that should help the novice remember in what situation and what kind of variable he should use each kind of model, and briefly inform the expert about the exact underlying methodological choices.
- Since tabxplor differenciates from other packages by the possibility to compare regression models estimates with their relative empirical/observed quantity, each section vignette should include a full detailed explanation with meaningful examples of what the `empirical = TRUE` framework does in this case (how to use and what to compare to what, which ci are calculated and why, what tab() code with ci compares to what tab_reg() one dependent/one predictor model, etc.).
- Explain, in a simple way, what the different summary statistics for each case are for.

New vignette vignettes/tabxplor-reg.Rmd (the vignette("tabxplor-reg") linked
from ?tab_reg and the intro vignette). Covers, on gss_simple:

- a first three-argument model, and how the outcome's type picks the family
  (binomial OR / gaussian beta / poisson IRR / multinomial / ordinal), with a
  worked example of each (nnet / MASS chunks guarded with requireNamespace so
  the vignette still builds without the Suggests);
- the distinctive `empirical = TRUE` framework, spelled out: the crude
  companion column is the SAME quantity as a cross-table, shown next to
  tab(race, married, OR = "OR") so the reader sees crude == empirical, plus
  what each family's crude measure is and how to read model-vs-crude;
- weighted / survey data: the weighted-estimate + design-based-SE framework in
  plain words, the wt / ids / strata syntax, and a pointer to survey::svydesign
  for the complex cases;
- model comparison (a named predictor list + compare=);
- how to read each footer statistic; and the or_plot() / lm_plots() plots.

##### Last Phase e-iii – add the "Programming with tabxplor" vignette (DONE)
All the part about "programming with tabxplor" and its vctrs fields should come in their own vignette, and it must be updaded and extended, with user-friendly example stating the possibilities.

New vignette vignettes/tabxplor-programming.Rmd (the vignette("tabxplor-
programming") linked from the intro vignette), moving the vctrs-field material
out of the README into its own page and updating + extending it for 1.4.0:

- what a tabxplor_fmt cell is (a vctrs record) and how it survives dplyr;
- getting plain numbers out (get_num / format / the per-field getters);
- the CURRENT 18-field table -- the README list was stale (`rr` is now
  `ratio`, the single `ci` is now the `ci_inf`/`ci_sup` bounds read by
  get_ci(), and `pvalue` / `tot_n` were missing);
- reading/writing fields ($ / vctrs::field / vec_data / set_display / mutate on
  an fmt vector), with the sd-from-variance worked example;
- the structural predicates (is_totrow/tottab/refrow, is_totcol/refcol);
- the column attributes (type/color/col_var/comp_all/totcol/refcol) with their
  current allowed values;
- building cells with fmt(); and the tab_prepare -> tab_plain/num -> tab_pct ->
  tab_ci -> tab_chi2 step-by-step pipeline.


##### Last Phase e-iiii: programming vignette uses exported field access only (DONE)

R CMD check builds vignettes against the INSTALLED namespace, not load_all, so
a vignette may only call EXPORTED functions. The programming vignette reached
for the internal field getters get_pct() / get_ci() / get_ci_inf() /
get_ci_sup() / get_diff() / get_mean() / get_n() / get_or(), which would fail
the check (they render fine under load_all, masking it). Switch to the
package's public field-access idioms -- `$field` on the fmt column,
vctrs::field(), get_num() -- exactly as the README's programming section does
(no public-surface expansion). Re-audited all three vignettes: clean.

##### Last Phase e-iiii: NEWS.md elements in vignettes ? 

`NEWS.md` is too long so we’ll trim it badly at the very end of development. But I wonder what would be useful, in it, to put in vignettes to explain how to use important new features.
- What should go in introduction vignette ?
- What should go in programming vignette ?
- What should go in regression vignette ?
- What new vignette should we if needed create for specific features ?

In the tabxplor introduction vignette as a quick tip, and in `vignettes/tabxplor-programming.Rmd` in details, please also explain the way the display = `"{pct} ({diff})"` syntax works to customise the display. In `vignettes/tabxplor-programming.Rmd`, also explain how to create a new column displaying diff from a column displaying percentages, or the like.
- By the way : there is an error in documentation for ci, the way to customise it is `"{pct} {ci}"`, not `"{pct} [{ci}]"` (which in reality doubles the []).


#### Last Phase f – pkgdown site + coverage CI (DONE)

Full pkgdown framework + a test-coverage GitHub Action.

pkgdown:
- _pkgdown.yml (validated: pkgdown::check_pkgdown() = "No problems found"):
  bootstrap 5, the site URL, a reference organised into purpose groups
  (cross-tables / build steps / regression / reshape / export / the fmt type /
  options+data / jamovi / helpers) with an `internal` catch-all for the S3
  methods + keyword-internal helpers, and the three vignettes as articles.
- .github/workflows/pkgdown.yaml: build + deploy to GitHub Pages (gh-pages),
  the standard r-lib/actions v2 recipe.
- DESCRIPTION URL gains the site (<https://bricenocenti.github.io/tabxplor/>);
  Config/Needs/website: pkgdown. _pkgdown.yml / docs / pkgdown .Rbuildignore'd.

Two Rd fixes pkgdown surfaced (both harmless to R CMD check, fatal to pkgdown):
- the `[` / `[<-` / `[[<-` methods for tabxplor_grouped_tab had a manual
  `@usage "x[i] ; ..."` STRING (invalid Rd usage). Dropped the manual @usage
  AND the redundant backtick `@method` tags so roxygen auto-generates the
  standard \method{...} usage; NAMESPACE S3 registrations are byte-equivalent
  (just re-quoted), suite green (3611).
- tab_pvalue_lines (internal, unexported) lacked @keywords internal.

test coverage:
- .github/workflows/test-coverage.yaml: covr -> Codecov (r-lib/actions v2);
  Config/Needs/coverage: covr; codecov.yml with informational (non-blocking)
  status.



#### Last Phase g – NEWS.md simplification

#### Last Phase h – tabxplor R french translation

All legends should be carefully translated to French.
Could the package documentation be translated for French users ? Could the whole pkgdown easily have a french version, with the possibility to choose on the webpage ?  
What other strings should be translated in French ?

#### Last Phase i – github PR and CRAN release








## Deferred / needs the maintainer

- **README.Rmd rewrite** — deferred per the roadmap's own note (*"only update before release"*); it depends on 8 hand-made colored-table screenshots that must be regenerated, which isn't scriptable here. The pkgdown site now renders the vignettes with real colors as the online showcase.
- **Known bug, recorded** (`# KNOWN-BUG` at `tab.R:2219` + CLAUDE.md): `options(tabxplor.output_kable=TRUE)` + a two-channel colour (`color=TRUE`) errors on auto-print — narrow (internal switch); `tab_kable()` and console both work.
- **`estimate_display` value collision** (`"ame"` means two things) — deferred because renaming would need a jamovi bridge/`prepare()`; instead the roxygen now explicitly distinguishes it from `effect="ame"`.
- **Maintainer steps**: run `devtools::check()` (the CRAN gate — I did not run it to avoid the orphaned-worker risk the CLAUDE.md warns about); enable GitHub Pages (gh-pages) + optional `CODECOV_TOKEN`; `jmvtools::prepare()` (already pending). One out-of-band deletion (`dev/review_manual/~$…xlsx`, an Office lock file) is left unstaged for you to decide.

- Maintainer steps (can't be done headless): enable GitHub Pages (gh-pages) in the repo settings; add a CODECOV_TOKEN secret if the repo is private.


### Reference — bugs, benchmarks, perf

#### Discovered bugs

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

- FIXED (Last Phase a): the two live-`jmvtab` degrade defects (2026-07-16). (1) The misleading 3×
  *"formatting and colors skipped: no tabxplor_fmt columns"* message: `tab_export_prep()` now decides
  the degrade notice ONCE per render batch and suppresses it when the batch still holds a real fmt
  table (`vars$notify`, gated at the 5 exporter emit sites); a lone non-tabxplor input still informs.
  (2) The 0-row hard **ERROR** (`"data is of length 0"`): `jmvtab_build()` now guards `nrow(data)==0`
  and returns a graceful empty frame the exporters render plainly (the core `tab()` `stop()` is kept —
  a public `tab()` on empty data still errors helpfully). Regression cases in `test-edge-cases.R`.

In-code these are tagged for grep: `# KNOWN-BUG:` (bugs below), `# FIXME:` / `# FIXME(clarify):` / `# FIXME(future):` (suspect logic or future work, several tied to the Phase 5 color work), `# OBSOLETE:` (dead-code banners, e.g. the stale `tab_xl` duplicate). Fix each bug inside the phase that rewrites the relevant code, not as a separate pass.

- FIXED (Phase 1a): `fmt()` public constructor cast `totcol` into `refcol` (the `refcol` argument was silently ignored). Now casts `refcol`. Low impact (refcol is normally set internally).
- FIXED (Phase 7g-iii, golden-locked): two latent `ref` bugs surfaced by the reference picker. (1) `diff_index()` matched a level label as a REGEX, so a metacharacter label (e.g. `"$25000 or more"`) silently mismatched (the reported "picking the 2nd row_var does nothing" — `rincome` has `$` levels) and a substring label multi-matched — now EXACT-match-first, then regex. (2) `resolve_ref_vector()`'s `length(ref)==1` early return recycled even a NAMED length-1 ref, so `c(race = "Black")` leaked to every col_var — now only an UNNAMED length-1 recycles; a named one is name-matched. Both byte-identical on existing goldens (the goldens' refs are `first`/`tot`/non-substring labels).
- FIXED (Phase 6e, golden-locked; hardened Phase 7d-i): `tab_num(..., <tab_vars>, ci="cell")` used to error ("some columns don't belong to the data.table: [tab_var]") in the `tot="no"` grand-total-only grouping-set / `na="keep"` reorder path. 6e made the grand total a length-1 list so `num_rollup()` keeps every tab_var present; 7d-i added a defensive `intersect(tab_vars, names(tabs_tot))` guard at the reorder + an `expect_no_error` regression in `test-num-fuse-parity.R`. Locked by golden `n_ci_tabvars` / `n_ci_tabvars_all`, both `comp` modes.
- FIXED (Phase 14b): `tab_kable(engine = "html", popover = TRUE)` rendered its own escaped ATTRIBUTE STRING as the popover content (`data-content="data-toggle=&quot;popover&quot;..."`). `tab_kable_print_tooltip(popover = TRUE)` returned `kableExtra::spec_popover()`'s attributes from a *text* builder, and the html engine wrapped them again. Attributes now live only in `tab_tooltip_attrs()`; the arg is deleted. The same builder also ends a second drift: the html popover omitted `data-trigger`, so it needed a CLICK where kableExtra's opened on HOVER.
- FIXED (Phase 14v-ii): `empirical = TRUE` with a **0/1 numeric** binary outcome silently produced a crude base of 0 (every `Emp. %`/`Emp. OR`/diff column blank). `reg_prep_binary()` recodes a 0/1 outcome to the labelled factor `c("Not <dep>", "<dep>")` with `positive_level = "<dep>"`, but `reg_empirical()` saw the RAW 0/1 data, so `as.character(0/1) == "<dep>"` never matched. `reg_empirical()` now mirrors the recode. Pre-existing (the crude columns were always 0 for a numeric 0/1 outcome), surfaced by adding CIs to those columns.
- FIXED (Phase 14v-ii): a mean cell CI at `n = 1` (`df = n - 1 = 0`) made `qt(0.975, 0)` emit `NaN` + a "NaNs produced" warning (rule B put means on `t`). `ci_pivot()` now coerces `df <= 0` to `NA` -> a clean `NA` interval (an undefined-variance cell is left blank/uncoloured). Also retires the pre-existing `n_ci_tabvars` NaN drift.
- FIXED (Phase 14b): the tooltip fragment join left a dangling `"f1: 5 ;"` / leading `"; f10: 5"` past 4 adjacent empty fragments — `str_replace_all(";  ; ", "; ")` matches non-overlapping, so the 3 repeats could not collapse a longer run. Latent (no cell reached 5 empties) until the 10th fragment made 9-empty runs reachable. Now an exact per-cell non-empty join.
- FIXED (2026-07-15, CI green-up): `tab_color_legend()`'s `lang` argument silently did nothing on **Linux** (`lang="fr"` returned English) — `Sys.setenv(LANGUAGE=)` alone can't switch gettext once glibc has cached a lookup. Now flushed via `flush_gettext_cache()` before/after/on-exit. Caught only because the snapshot tests SHIP and run on CI's Linux jobs. Cannot work under `LANG=C` (gettext ignores `LANGUAGE` there) — a documented gettext rule, not a package bug.
- FIXED (2026-07-15, CI green-up): 6 unqualified `globalVariables()` calls in `R/fmt_class.R` with `utils` declared nowhere — `pkgload::load_all()` crashed ("could not find function globalVariables") in any process without `utils` attached, e.g. a testthat parallel worker. Now `utils::globalVariables()` + `utils` in Imports. Latent since forever; surfaced by turning on `Config/testthat/parallel`.
- FIXED (2026-07-15, CI green-up): `test-tab_logit.R` "colour_signif='ignore'" asserted a symmetric OR break (`mag > 1.16`) against the **asymmetric** `mean_ratio` scale (`under` starts at 1.5 since Phase 13a) — wrong test; failed in isolation everywhere and on macOS CI, passing elsewhere only via a leaked global scale. Now derives the threshold per direction from the scale in force and pins it.
- **NOT a bug — confirmed deliberate (Last Phase a)**: row labels render with **U+202F narrow no-break spaces** in the HTML/kable path ONLY (both engines, via `tab_wrap_text(unbreakable_spaces=TRUE)`), a no-wrap choice with an opt-out (`unbreakable_spaces=FALSE`). md / plot / console keep ASCII. The only side-effect is HTML copy-paste yielding NBSPs; kept as-is.
- **FIXED (Phase 7e)**: `tab(data, >=2 row_vars, >=2 col_vars)` used to error "pct can't be recycled" for ANY `pct` (the multi×multi tables jmvtab drives). `tab()` recycles `pct` to a per-col_var vector (`pct = c(rep(pct, length(col_var)), ...)`), but `pct_vect` only broadcasts a per-col_var vector when there is exactly ONE row_var (branch B); with ≥2 row_vars it falls to the `else` stop. Fix: add a branch `is.character(pct) & length(pct) == length(col_vars)` → `rep(list(pct), length(row_vars))`. Pre-existing (reproduces pre-7d-ii on `git stash`); low impact (multi×multi + output_list); fix with the recycling code.
- FIXED (Last Phase a): `tab()` errored on a `data.table` **input**. Root cause: `tab_setup()` did `data[pos_col_vars]` to classify col_vars, which is COLUMN-subsetting on a data.frame/tibble but ROW-subsetting on a data.table → NA col_var → `tab_num()` "Selections can't have missing values". Now `purrr::map_lgl(pos_col_vars, ~ is.numeric(data[[.x]]))` (engine-agnostic `[[`-by-position).
- FIXED (this session): `set_num()` wrote `display=="diff"` via `set_pct()` (should be `set_diff()`), so setting the displayed value of a diff cell went to the wrong field. Now uses `set_diff()`.
- FIXED (workstream 5): `relabel_levels_in_varnames()` (`tab.R` ~L5592) made big weighted tables ~60× slower. Its `across(where(...))` predicate ran on **every** column with vectorised `&`/`|`, so the character branch `any(. %in% names(data))` coerced whole 8M-row numeric/factor columns to strings (~15s × 2 calls). Rewrote it to examine **only the `col_vars` targets** with short-circuit `&&`/`||` (numeric targets cost ~0); output byte-identical. 8M `tab(wt=)`: ~30s → ~0.2s; unweighted tables also faster + ~90% less memory.


##### mirai parallel crash under load_all + `pct`/`OR` recycle warning (FIXED 2026-07-13)

Two byte-identical fixes (full suite green FAIL 0 / PASS 2070, NO golden regen).
1. **`tab(parallel=)` crashed under `devtools::load_all()`** with `object 'tab_build_one' not found`
   whenever the call had **≥ 2 row_vars** (1 row_var stays serial below `parallel_min = 2`). Root cause:
   the mirai daemons bind the *installed* (stale) tabxplor namespace, which lacks `tab_build_one`; an
   installed 1.4.0 works, but dev sessions don't. Fix ([R/tab-parallel.R](R/tab-parallel.R)): new
   `tab_dev_pkg_path()` (dev detected via the loaded namespace path + an `R/` source check) + a
   `tab_pool_ensure()` branch that `pkgload::load_all()`s the dev source on each freshly spawned daemon
   (once per pool, before dispatch). Inert once installed (`tab_dev_pkg_path()` → NULL). No manual pre-warm
   needed anymore. New `test-parallel-parity.R` case locks the auto-load (parallel without `warm_pool()`).
2. **Spurious recycle warning** `In pct == "row" & OR %in% c(...) : longer object length is not a
   multiple of shorter object length` on multi-row_var × multi-col_var tables whose counts don't divide
   (e.g. 3 × 4), independent of OR/parallel/`levels`. Root cause: [tab.R:1341](R/tab.R#L1341) combined the
   per-col_var `pct` (length ncolvars) with the per-row_var `OR` (length nrowvars) via vectorised `&` —
   the twin of the Phase 9a L1859 fix, missed. Fix: `all(pct == "row") && all(OR %in% c(...))`
   (byte-identical: `all(A & B) ≡ all(A) && all(B)` for any lengths, minus the recycle).

##### colour `color_all_signif` ratio channel + significance-stars UX (FIXED 2026-07-13)

Interrupted Phase 12 to fix two colour/significance defects + redesign stars. Full suite green
(FAIL 0 / PASS 2068); goldens byte-identical (RDS reverted via stars-pinned CI fixtures; one conscious
display-snapshot regen for the new star padding).

1. **`color_all_signif` mis-coloured the `ratio` channel** ([R/fmt_class.R](R/fmt_class.R)
   `fmt_color_plan()`). The "guaranteed effect" branch set `score` = the raw **difference** CI bound
   (centre 0, ~0.05); the ratio channel then folded it around centre 1 (`1/0.05 ≈ 20`) → nearly every
   significant cell, INCLUDING over-represented ones, got the strongest *under-represented* colour.
   Fix: compute the guaranteed magnitude on the measure's OWN scale — `ratio` (no native CI) converts
   the shared diff floor to a guaranteed ratio `1 + (get_ratio − 1)·(guar_diff/get_diff)` (centre 1);
   `diff`/`or` unchanged. Consistency now provable: 0 direction-mismatches across the reported shapes
   (a `test-color-engine.R` slot-lock encodes it). The reported "scalar `color="diff"` colours nothing"
   was NOT a separate bug — the two-channel case merely looked coloured because of the flooded ratio
   background; the diff text channel was always correct, and the two cases are now consistent.
2. **Significance stars → opt-in, default off, right-padded, no tooltip leak.** Stars were a global
   option (default TRUE) appended by `format()` to *every* field (so `tab_kable` tooltips leaked stars
   onto pct/n/rr/…), unaligned. New design (STORAGE-driven; `pvalue` feeds ONLY stars, colour reads the
   bounds): `options(tabxplor.stars)` default → **FALSE** ([R/utils.R](R/utils.R)) so a plain `tab()`
   stores no `pvalue`; `format(x, stars = FALSE)` default — the MAIN sites (`pillar_shaft`, `tab_kable`,
   `tab_md`, `tab_xl` numFmt fold) pass `stars = TRUE`, tooltips keep the default → **leak fixed for
   free**; `format()` **right-pads** the star field to the column-max width so numbers stay aligned
   (`str_trim(side="left")` in `tab_md`). `tab_reg()`/`tab_logit()`/`multi_logit()` gained
   `stars = TRUE` (strip the `pvalue` post-build when `FALSE`) so regression tables keep stars by
   default. `test-stars.R` (16) locks it. The `*** but no colour` complaint was a symptom of always-on
   stars: under `color_all_signif` a significant cell whose GUARANTEED effect is below the first break
   is correctly starred-but-uncoloured — legitimate, and now off by default.

FIXED (Last Phase a): weight column literally named `"wt"` — the real cause was data.table `j`
SHADOWING: a column named `"wt"` (the weight OR a col_var) masked the `wt` ARGUMENT inside the scan's
`as.character(wt)` naming, leaking a garbage column + warnings (numeric means only; factor counts were
fine). `num_moment_scan()` + the mean-direct branches now capture `wt_name` outside `j` and read the
column via `get(wt_name)` (shadow-proof, byte-identical for ordinary names); `tab_setup()` also errors
early if the weight is ALSO a selected variable (the nonsensical double-role that used to crash cryptically).

FIXED (Last Phase a): `contrib` + a significance policy (`color_all_signif`/`grey_non_signif`) coloured
nothing — contrib has no CI to gate on. Now `chi2_write_contrib()` computes each cell's standardized
(Pearson) residual p-value at chi2-time (`N` in hand) and stores it in the `pvalue` field;
`fmt_color_plan()` gates contrib on it. Both policies now colour significant contributions (exact vs
`chisq.test` on unweighted tables; approximate under weights per the §10/§18 framework). Conscious
golden: `f_color_contrib.rds` gained the `pvalue` field (contrib `ignore` colouring byte-identical).

(The multi-row_var `pct`/`OR` length-mismatch warning + the mirai load_all crash were FIXED 2026-07-13, above.)

##### contrib rendering crashes (Phase 10j-B) (FIXED 2026-07-12)
Fixing the flagged `color="contrib"` + `comp="all"` colour crash surfaced THREE distinct render bugs (all now fixed, golden-locked, byte-identical  to every working path):
  1. **Colour engine** — `get_mean_contrib()` returned length 0 under `comp="all"` when there is NO total
     table (no tab_vars), so `fmt_color_plan()`'s `get_ctr(x) / get_mean_contrib(x)` errored
     `false must have size N, not size 0` (both `tab_kable`/`tab_xl`). Fix: new shared `grand_totrow()`
     ([R/fmt_class.R](R/fmt_class.R)) = `is_totrow & is_tottab`, **degrading to `is_totrow` when there is
     no total-table axis** so a single table is its own total table; used by BOTH `get_mean_contrib()`
     (read) and `chi2_write_contrib()`'s seed protection ([R/tab.R](R/tab.R)) so the mean-contribution seed
     is stored where it is read. `get_mean_contrib()` also never returns length 0 now (graceful → NA).
  2. **Kable tooltip** — `cond_ctr` ([R/tab_classes.R](R/tab_classes.R)) did `get_pct(x) == 1` on the Total
     column (whose `pct` is NA while `ctr` is written), yielding NA → `if (any(cond_ctr))` crashed **any**
     contrib table via `tab_kable(tooltip=TRUE)`, incl. the default `comp="tab"`. Fix: NA-safe guard
     (mirrors the sibling `cond_pct`).
  3. **Markdown** — `tab_md()`'s tab_var-blanking loop ([R/tab_md.R](R/tab_md.R)) did `vals[i]==vals[i-1]`
     without NA-safety, crashing on the NA tab_var of a **materialised p-value row** → **any**
     `chi2=TRUE` + tab_vars table via `tab_md`. Fix: blank NA/repeat cells NA-safely (kable already tolerated).
  **Semantics confirmed (the maintainer's note):** the code DOES implement the wanted behaviour — `comp="all"` ungroups the table ([tab.R:5557](R/tab.R#L5557)) so chi2 + contributions are computed on the WHOLE table  (all row_var × tab_var level combinations, referenced to the grand total); `comp="tab"` keeps per-subtable  grouping so a chi2 + contributions are computed PER subtable (each vs its own total row). Coverage added: `c_contrib_all` / `c_contrib_all_notab` colour goldens + an exporter render-no-crash test (`test-export.R`).

##### CI green-up (2026-07-15) — 3 causes, none R-version-related

First GitHub Actions run of the 1.4.0 branch: **all 5 jobs red**. Diagnosis (each reproduced locally,
NOT guessed): devel/release/oldrel-1 fail **identically**, so R version is not a variable — the
variables are a dependency version, a libc, and two wrong tests. Suite now green **in parallel, 225s
-> 56s**.

1. **kableExtra 1.4.0 (local) vs 1.4.1 (CRAN/CI)** — 7 `test-render-html` snapshot fails on ALL
   platforms. `text_spec`/`cell_spec` HTML changed (rgba alpha `255`->`1`, leading padding dropped,
   tile `border-radius` dropped, and `text_spec` leaks a stray `class="TRUE"` — an upstream
   regression, its `background_as_tile` default; **worth reporting to kableExtra**). Fix = **decouple,
   not regenerate**: the legend `<span>` is now emitted INLINE in `legend_render_line()`
   (R/fmt_class.R) instead of via `kableExtra::text_spec()` — which was ALSO the last kableExtra call
   on the "self-contained" html engine's path (its test claimed self-containment; it was false). The
   kableExtra-engine byte snapshot was **replaced by version-robust assertions** (geometry / colour
   on-off / theme / tooltips): we do not own that HTML, so we must not lock its bytes. Proven: html
   engine output is now **byte-identical under 1.4.0 and 1.4.1**, so its snapshots regenerate safely
   on either. `_snaps/render-html.md` regenerated (legend line only; all data rows unchanged).
2. **glibc gettext cache — a REAL user-facing bug** (3 `test-color-legend` fails, **Linux only**;
   macOS/Windows passed). `tab_color_legend()` set `LANGUAGE` without flushing, so on Linux
   `lang = "fr"` silently returned **English** for every exporter. glibc caches translated strings and
   only invalidates on `setlocale`/`bindtextdomain`/`textdomain`. Fix: new `flush_gettext_cache()`
   (`bindtextdomain("reset", tempdir())` — the portable lever; the older `Sys.setlocale` trick fails
   on musl, withr#213) called **before and after** the switch + on exit, mirroring
   `withr::local_language()` (Suggests-only, so inlined). **Constraint that cannot be fixed**: gettext
   IGNORES `LANGUAGE` when the locale is `C`, and `R CMD check` forces `LANGUAGE=en` while testthat's
   `local_reproducible_output()` sets `LANG`/`LANGUAGE=C` — so the test **probes the capability with a
   raw `gettext()` call** and skips only where translation is genuinely impossible (keeps coverage on
   macOS/Windows; a blunt `LANG=C` skip would have killed it everywhere).
3. **`test-tab_logit.R:192` was simply wrong** (macOS only in-suite; **failed in isolation
   everywhere** — the "colour-breaks-leak" note above it). It asserted every non-sig OR with
   `mag > 1.16` is coloured, but OR colouring reads the **`mean_ratio`** scale, which Phase 13a made
   **asymmetric** (`over = 1.15,1.5,2,4` / `under = 1.5,2,4`): an OR of `1/1.34` is legitimately
   uncoloured. It only ever passed by inheriting a symmetric scale from an earlier file. Now derives
   each side's threshold from the scale in force + pins it with `withr::local_options()`.

**Test-suite policy changes** (grounded in testthat/r-pkgs/CRAN primary sources):
- **`Config/testthat/parallel: true`** + `Config/testthat/start-first` (slowest files first).
  **225s -> 56s.** Prerequisite was #3: parallel workers run disjoint file subsets, so any test that
  passes only via another file's leaked state starts failing. Enabling it immediately exposed a
  **latent load bug**: 6 unqualified `globalVariables()` calls in `R/fmt_class.R` + `utils` declared
  nowhere — `load_all()` crashed in the (reduced-default) subprocess. Now `utils::globalVariables()`,
  `utils` added to Imports.
- **Benchmarks are opt-in** (`skip_unless_benchmarks()` in helper-benchmark.R, gate
  `TABXPLOR_BENCH=true`). `skip_on_cran()` did NOT hold them back: `NOT_CRAN="true"` is set by
  `devtools::test()`, by `devtools::check()` (its literal default) AND by r-lib/actions — so ~46s
  (21% of the suite) ran on every local run and every CI job to print numbers nobody reads, asserting
  nothing. Also required: under parallel, **stdout from test files is discarded**, so their printed
  comparison would silently vanish, and parallel timings are meaningless anyway.
- **Snapshots stay shipped.** `expect_snapshot()` defaults to `cran = FALSE`, so testthat skips them
  on CRAN — shipping costs nothing at submission and can never fail a CRAN check. `.Rbuildignore`ing
  them would also remove them from CI (which checks the built tarball), i.e. would have hidden bug #2.
  The rule to hold: **snapshot only output we own**; assert invariants on anyone else's.
- **No CRAN 10-minute test limit exists** (the folklore "10" is `_R_CHECK_TIMINGS_`, a 10-**second**
  *reporting* threshold; the real `_R_CHECK_*_TIMEOUT_` vars default to `0` = no limit). Policy says
  only "as little CPU time as possible". The actionable target is r-pkgs': **tests under ~1 min**.

**Flagged, not fixed** (pre-existing, unrelated to CI): (a) row labels render with **U+202F narrow
no-break spaces** instead of ASCII spaces in BOTH html engines ("No answer" -> `No<U+202F>answer`), so
`rh_cells()`-vs-`levels()` comparisons silently under-test and copy-paste from HTML yields NBSPs —
looks deliberate (no-wrap labels), worth confirming; (b) ~~**dependency drift**: the dev library was
behind CRAN on 11/13 key packages incl. `vctrs 0.6.5 -> 0.7.3` and `dplyr 1.1.4 -> 1.2.1` — CI tests
the package against dependencies the dev machine has never run. Maintainer is installing R 4.6.1 +
a fresh library; **re-run the suite after that**~~ — ✅ **CLOSED 2026-07-15 by the WSL2 migration (Phase C2).**

##### ✅ The dev machine now MATCHES CI — the drift is gone, and the re-run is done

Measured on the new WSL2 Ubuntu 26.04 library (R 4.6.1, 484 packages from P3M `resolute`):
**`vctrs 0.7.3` · `dplyr 1.2.1` · `kableExtra 1.4.1` · `tibble 3.3.1` · `tidyr 1.3.2` · `pillar 1.11.1`**
— i.e. **exactly the versions CI had and Windows never ran**. `devtools::check("~/github/tabxplor")` on
that library: **0 errors / 0 warnings / 0 notes** on R 4.6.1 **and** on R-devel 4.7.0. `check()` sets
`NOT_CRAN=true`, so the snapshots fired — vctrs 0.7 / dplyr 1.2 are now **exercised**, not assumed.

Two of this section's own findings are settled by that, and both should be read as *retired*:

- **The kableExtra 1.4.0-vs-1.4.1 split no longer exists locally.** The 7 snapshot fails came from the dev
  box being on 1.4.0 while CI shipped 1.4.1; the dev box **is** on 1.4.1 now, and the decoupling fix (html
  engine emits its legend `<span>` inline; kableExtra output asserted on invariants, not bytes) is
  validated on it. ⚠ The upstream `text_spec` `class="TRUE"` regression is still worth reporting.
- **The Linux-only gettext bug class is now reproducible on the dev machine.** This section records the
  3 `test-color-legend` fails as *"Linux only; macOS/Windows passed"* — i.e. Windows could not reproduce
  it and only CI caught it. Verified on WSL2: the `.mo` is installed, `gettext("Shades of blue")` returns
  **"Nuances de bleu"**, and the file runs **43 pass / 0 skip** — the FR tests actually exercise here
  rather than passing vacuously. **Linux-only defects now surface before CI.** (The `LANG=C` capability
  probe still governs: under `R CMD check`, which forces `LANGUAGE=en`, they skip by design.)



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
4. For package structure and architecture, also add the relevant CLAUDE.md update lines in your response : it should be minimalistic, concice, no bullshit, with nothing useless that would clutter the prompt, since the details are already in `dev/tabxplor_architecture.md`. When there is nothing to change, skip it. Maintainer while move done phases to `dev/tabxplor_1.4.0_roadmap_DONE_PHASES.md` himself.
5. `NEWS.md`: user-facing and CRAN-facing, tracking new functions, new arguments and arguments changes, deprecations, and important bugs fixes. Keep it minimalistic and no bullshit. Do not edit it when it’s not necessary.
6. (`README.Rmd` : user manual. Only update before release of new version to CRAN, never before.)


