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
│                              per-side fold + findInterval; slots 1-4 over / 5-8 under)
├── tab.R           (~6200 L) Main API: tab(), tab_many(), tab_plain(), tab_num(),
│                              tab_apply_reference() (Phase 7f carve; Phase 9d: matrix-sweep internals),
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
│                              CI engine (ci_pivot/ci_wilson/ci_newcombe/ci_katz_rr/…: 14b's Katz
│                              log-RR is the RATIO-scale interval, ci_type="ratio"), agg_chi2/agg_anova
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
│                              ran on every export and nothing read it)
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
├── utils.R         (1364 L)  Pipe re-export, .onLoad() options setup, factor utilities.
│                              NOT the colour-palette DESIGN tools (preview_color_grid /
│                              simulate_cvd_farver / plot_oklch_hue_strip_cvd / set_luminance...):
│                              they live in dev/color_palette_tools.R and must stay there -- they
│                              are the sole reason the package would depend on farver + colorspace.
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
│                              group-aware print_reg_footer). multiplicator (OR^k) + empirical_OR
│                              (reg_empirical_or crude %/OR beside model OR, binary). No new fmt fields.
│                              12h (display): estimate_display= arg -> est_ci token (estimate + visible
│                              [ci_inf;ci_sup] bracket, no 1/x; fmt_class.R only) | "prob"/"ame" fold
│                              predicted prob / AME into the OR cell via {} grammar (binomial coef only,
│                              reg_apply_estimate_display + reg_marginal). No new fmt fields.
├── tab_reg_plots.R  (~230 L) Phase 12h display: or_plot() (finalfit-style OR forest plot ON a
│                              tabxplor_tab -- reads fmt fields, NO refit; gridExtra 2-panel) + lm_plots()
│                              (ggplot2 2x2 glm/lm diagnostics). ggplot2+gridExtra guarded (Suggests).
├── tab_logit.R      (~5 L)   Emptied in Phase 12c (renamed -> tab_reg.R; git rm pending).
├── tab_logit_2.R    (~8 L)   Emptied in Phase 12a (git rm pending). or_plot/lm_plots -> tab_reg_plots.R.
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
└── jmvtab.h.R       (605 L)  Jamovi module UI (auto-generated, do not edit)
```

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

| Constraint               | Detail                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   |
|--------------------------|------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| CRAN stability           | Public function arguments must NOT change without deprecation. Internals can change freely.                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              |
| vctrs record contract    | Adding a field to `tabxplor_fmt` requires updating `new_fmt()`, `fmt()`, `format.tabxplor_fmt()`, `pillar_shaft.tabxplor_fmt()`, `vec_arith` methods, and possibly `tab_pct()`/`tab_ci()`/`tab_chi2()`. ~8 functions across 3 files.                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     |
| NAMESPACE                | Auto-generated by roxygen2. Never edit `NAMESPACE` by hand. Run `devtools::document()` after changing `@export`/`@import`.                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               |
| data.table internals     | `tab_plain()`/`tab_num()` rename `col_var` to internal names to avoid data.table conflicts. The user's column names are restored afterward.                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              |
| dplyr class preservation | 30+ S3 methods on `tabxplor_tab`/`tabxplor_grouped_tab` ensure class + attributes survive all dplyr verbs. Missing a method = silent class downgrade to `tbl_df`.                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        |
| Options as config        | All defaults set in `.onLoad()` in `utils.R`. Users override via `options()`. Functions read with `getOption()`.                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         |
| Suggests-only guards     | `openxlsx2`, `ggplot2`, `jmvcore`, `ggpubr`, `cowplot`, `mirai` are in Suggests. Every call must be guarded with `requireNamespace()` or equivalent (tab_xl's ONE guard is in `tab_xl()`; `R/tab-xl-backend.R` wrappers are unguarded).                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  |
| Color break mirroring    | `set_color_breaks()` takes positive-only thresholds. Negative breaks are auto-mirrored internally. Any `pct_breaks` value > 1 triggers ratio comparison instead of difference (the "*2 rule").                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           |
| Mean-diff asymmetry      | For `type="mean"` columns, the `diff` field stores a **ratio** (cell_mean / ref_mean), NOT a difference. Thresholds like 1.15 mean "+15% above reference". This asymmetry propagates into `color_formula()` and `format.tabxplor_fmt()`. **(1.4.0 §3: numeric `diff` becomes a real difference; the ratio moves to the `ratio` field — the never-used `rr` field renamed, placed after `diff`.)**                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        |
| tab_reg                  | Phase 12c–12g LIVE: unified regression tables (gaussian beta / binomial OR / poisson IRR / multinomial OR / ordinal cumulative OR) over lm/glm/svyglm/svyolr/svy_vglm/nnet::multinom/MASS::polr + broom (no parsnip). tab_logit/multi_logit are binomial wrappers. Effect shape is exponentiate-driven: additive beta -> `diff`+type="coef"+display="coef"+ci_type="diff"; multiplicative OR/IRR/cumOR -> `or`+type="row"+ci_type="or". No new fmt fields/attributes: `type` gains value "coef", `display` gains token "coef", the `var` field carries var(Y). 12d: MNL = one OR col per outcome category vs ref; ordinal polr + Brant PO diagnostic. 12e: orthogonal `effect="ame"` (marginaleffects) + `at="reference"` profile axis. 12f: model-summary footer + compare= in the `test` attr. 12g: SURVEY designs — `wt=`/`ids=`/`strata=`/`fpc=`/`nest=` + a prebuilt survey.design/svyrep.design as `data`; reduced weighted glance (Wald/Nagelkerke/Cox-Snell/Rao-Scott-AIC) + weighted compare (anova.svyglm Wald); weighted 3+ level (svyolr / svyVGAM); `split_var` (tab_vars analogue, tab_spread-able); `multiplicator` (OR^k); `empirical_OR` (crude %/OR beside model OR, binary). No new fmt fields; new Suggests svyVGAM. |


---

## Design Decisions

### Type System

- **`tabxplor_fmt`**: vctrs record (`new_rcrd()`) with **18 per-cell fields** (was 15 before v1.4.0 Phase 1a) and 9 per-column attributes (Phase 10i-A dropped `display_spec`). The critical distinction: fields vary per cell (accessed via `vctrs::field()`), attributes are scalar describing the whole column (accessed via `attr()`). Constructor chain: `fmt()` (public, validates + coerces) -> `new_fmt()` (internal, calls `vctrs::new_rcrd()`). *(Phase 1a reshaped 15→18 in one combined pass — decisions doc §9; `ci` is now derived from the `ci_inf`/`ci_sup` bounds by `get_ci()`, a bounds-shim.)*
- **`mean` field overload** (cross-cutting): for **pct-type** columns the `mean` field carries the cell/reference **ratio** for the "*2 rule" (not an actual mean). Written by `tab_pct()`, read by `fmt_color_selection()`. The **`ratio` field** now exists (Phase 1a renamed the never-used `rr`→`ratio`; decisions doc §3); the overload removal + moving the ratio to `ratio` lands in **Phase 5** (color diff/ratio split), not yet done.
- **`tabxplor_tab`**: tibble subclass via `tibble::new_tibble()` with `subtext` (legend text), `test` (chi2/ANOVA-F results tibble; §16 hard-rename of the old `chi2` attribute) and — Phase 10i-B — `render_extras` (the `list(add_n=, add_pct=)` display intent) attributes, all carried through dplyr verbs by the S3 methods + vctrs reconcilers.
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



### Phase 14 – manual review by maintainer and next improvements

#### Context : 14a to 14g

`dev/review_manual/tab_manual_review_pass_1.R` is the maintainer's first hands-on review of tabxplor
1.4.0 on real survey data (`pc18`). Its `#` comments are the spec. This plan turns them into phases.

Nine defects were **reproduced and root-caused** during planning (not guessed) — several have causes
neither the maintainer nor I had named, and three change the shape of the fix:

| # | Symptom (maintainer)                             | Verified root cause                                                                                                                                                                                                                                                                                              |
|---|--------------------------------------------------|------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| 1 | `color_signif` greys out **every** cell          | `legacy_union()` ([tab.R:677-688](R/tab.R#L677)) returns early for `"auto"`/`"contrib"`/`"OR"` **before** the `signif` switch, and `color = TRUE` never calls it at all → `legacy = "auto"` → `tab_resolve_settings()` never forces `ci = "diff"` → no CI → all gates FALSE. Affects the DEFAULT `color = TRUE`. |
| 2 | mirai crash `rep()` invalid 'times'              | **Not a parallel bug.** `chi2_compute_test()` ([tab.R:5763](R/tab.R#L5763)): `vapply(lv_cols, ..., double(n_rows2))` returns a **vector, not a matrix**, when `n_rows2 == 1` → `ncol(M)` is `NULL` → `rep(times = NULL)`. Guard is `n_rows2 > 0`, must be `> 1`. mirai only reframes the error.                  |
| 3 | `n` row disappears with 2+ row_vars, `pct="col"` | `tab_add_n_pct()` ([tab.R:6721](R/tab.R#L6721)) does `dplyr::slice(..1, last_totrow)` with a **global** index on a **grouped** tab → slice is group-aware → 0 rows → `bind_rows` silently drops it. Verified: `nrow(slice(t2, 11)) == 0`.                                                                        |
| 4 | `diff: ××1.3`                                    | Double `×`: the tooltip prepends it ([tab_classes.R:2037](R/tab_classes.R#L2037)) **and** `format()` already does ([fmt_class.R:2009](R/fmt_class.R#L2009)).                                                                                                                                                     |
| 5 | "is it diff or ratio?"                           | It IS the diff — but a **multiplication sign on an additive difference**. The numeric-diff *display* flip was deferred at Phase 2 and never landed ([fmt_class.R:1876-1878](R/fmt_class.R#L1876) admits it).                                                                                                     |
| 6 | `ratio: ×1` on Total cols / ref rows             | `cond_rr` ([tab_classes.R:2088](R/tab_classes.R#L2088)) lacks the totcol / totrow / ref gates its sibling `ok_diff` has. Also `type=="mean"` is excluded → means show no ratio line at all, though ratio is what colours them.                                                                                   |
| 7 | kable padding `100% (n= 849)`                    | **Not (only) the font.** Both engines set `white-space: nowrap` on fmt columns, and CSS **collapses runs of ASCII spaces** — the pad dies before the font matters.                                                                                                                                               |
| 8 | dark mode loses all light formatting             | `theme="dark"` swaps kableExtra's **whole theme** → `kable_material_dark` (56px headers, `#FFFFFF60` th, its own padding).                                                                                                                                                                                       |
| 9 | col_var header grey + too big                    | `add_header_above()` runs **before** `kable_classic()` → `lightable_class` is NULL → the `#ddd` grey fallback fires; and `row_spec(0)` only targets the **last** `<thead>` row, so the spanning row is never styled.                                                                                             |

Two systemic findings worth naming once:

- **`tab_compact()`'s synthetic `levels` / `row_var` columns are a recurring root cause.** They make
  `tab_get_vars()` report `tab_vars = "row_var"` on a table with no tab_vars, which is why
  `tab_transpose()` aborts with a misleading message and why `tab_xl()`'s title reads
  *"levels by multi (tabbed by row_var)"*.
  + **This needs to be fixed at framework level, reliably**, by finding what solid and reliable property differenciate a table with a real tab_vars and several row_vars merged (the column currents names with several row_vars merged are one, but maybe not the most reliable).
- **Positron has no supported theme API — but a workaround exists and is verified on this machine.**
  Two research passes (§14g) agree there is no supported route and none scheduled
  ([positron#2986](https://github.com/posit-dev/positron/issues/2986), open 2 years, milestone
  "Future"), and that **no R package detects it** — thematic assumes light + warns, cli returns FALSE,
  the rest don't try. But your client `settings.json` **is** reachable from WSL (VS Code caches it
  server-side under `~/.positron-server/data/User/History/`, and that cache updates on live writes), and
  the chain `workbench.colorTheme` → extension `package.json` → `uiTheme: vs-dark` resolves your actual
  theme correctly today. §14g ships it best-effort, silent, degrading to today's behaviour.

##### Decisions taken with the maintainer

1. **Padding** → swap the pad char to **U+2007 FIGURE SPACE** for HTML + Excel only; console/md keep
   ASCII. Measured in DejaVu Sans: `U+2007` = 1303/2048 em = **exactly** the digit width (1303), while
   `U+0020` = 651 = exactly half — so the "2 spaces" intuition was numerically right for this font, but
   U+2007 is right *by definition* in any font with tabular figures, and never collapses. Goes next to
   `unbrk`/`sigma_sign`/`mult_sign` at [utils.R:1180-1183](R/utils.R#L1180) as a `\uXXXX` escape
   (R sources stay ASCII).
2. **kable engine** → `engine = "html"` becomes the default, but it needs **serious design work**
   → its own phase (§14e) with the maintainer's feedback list as the brief.
3. **md col_var names** → a **first BODY row**, lower-contrast, visually marked, with an option to
   remove it. Long names: whole name in the **first cell only**, deliberately **not** pipe-aligned for
   that row (parses fine; only the maintainer's own markdownlint flags it), followed by a separator row.
4. **XL title** → full names, elided past ~3 with a count. Sheet name likewise, cut to 25.
5. **List at export** → **never merge**. Delete the export-time compaction branch.
6. **XL legend** → precompute a **9th palette** (`-0.2` OKLCH lightness of the bg palette) in
   `dev/color_palette_tools.R`, baked as constants like the other 8.
7. **Ratio CI** → **Katz log-RR**, stored in the existing `ci_inf`/`ci_sup` with **`ci_type = "ratio"`**
   (the Phase-12a `ci_type = "or"` precedent). **No new fmt fields.** Trigger rule: the CI follows the
   **text channel**. `color = "ratio"` or `c("ratio","diff")` → Katz. `color = "diff"` / `TRUE` /
   `c("diff","ratio")` → exactly today's behaviour, unchanged.
8. **`tab_xl()`** → keeps returning `invisible(tabs_base)` **and** `cat()`s the path.
9. **Tests** → never use `pc18` (confidential). `forcats::gss_cat` and the like only.

---

#### Phase 14a — correctness bugs + the `test=` rename (DONE — 2026-07-16)

All five landed. Conscious golden regen: **only** `_color_golden/c_after_ci.rds` +
`c_mean_after_ci.rds` (the two `guaranteed_effect` fixtures; every changed cell has a CI excluding
the neutral, no cell LOST colour, direction always matches the sign of `diff` — verified cell by
cell). The other 10 colour goldens were re-written by `make_color_golden.R` but are **semantically
identical** (gzip mtime churn); restore them with
`git checkout -- tests/testthat/_color_golden/{c_ci,c_contrib,c_contrib_all,c_contrib_all_notab,c_diff,c_diff_ci,c_mean_diff,c_mean_diff_ci,c_or,c_syn_diff}.rds`.

- **`color_signif` forces its CI** — the policy now reaches `tab_resolve_settings()` as a real
  argument (`tab()`/`tab_many()` pass `color_spec$signif` -> `tab_build(color_signif=)` -> `ctx` ->
  `tab_setup`; `tab_counts()` passes `"ignore"`, it only takes legacy colour strings). The forcing sits
  in the ONE cascade (§7b), **before** the `color = "auto"` resolution, so the implicit form is
  byte-identical to the explicit `ci = "diff"` one (locked for factor / numeric-only / mixed).
  `pct_rowcol` was hoisted out of the auto `case_when` and is shared by both. Gated == an explicit
  diff, or an "auto" resolving to row/col pct or to the numeric arm; `contrib`/`OR` are never forced.
  Explicit `ci = "cell"` + a policy -> error.
- **`guaranteed_effect` break offset** — new `offset_guaranteed_breaks()` next to `fmt_color_plan()`
  (`R/fmt_class.R`), applied to `over`/`under` independently (asymmetric scales) inside the plan, so
  `legend_specs()` follows for free. The legend now prints `+0; +5; +15; +25`, as the Phase 13b spec
  always claimed.
- **`chi2_compute_test()` single-row crash** — `ncM <- length(lv_cols)`, not `ncol(M)`: `vapply()`
  returns a matrix only when `FUN.VALUE` has length > 1, so `n_rows2 == 1` made `M` a vector and
  `ncol(M)` NULL. NOT parallel-specific — mirai only reframed it.
- **`tab_add_n_pct()` pct="col"** — new shared `tab_append_pctcol_rows()` (used by BOTH the add_n and
  add_pct branches): slice on the UNGROUPED tab (the old global index vs `dplyr::slice()`'s
  group-relative one returned 0 rows -> `bind_rows` silently dropped the row), one row per sub-table,
  spliced after each sub-table (anchoring on the group END, not the total row, is what preserves the
  historical `Total | row_pct | n` order).
- **`chi2` -> `test`** on `tab()` + `tab_counts()` (`lifecycle::deprecated()` sentinel). `tab_build()`
  keeps the internal `chi2` name (it drives `tab_chi2()`; the ANOVA arm branches in `tab_transform()`)
  — only the PUBLIC surface is renamed. `tab_many()` keeps `chi2` (itself deprecated).

Tests: `test-color-engine.R` (offset + the "significant => coloured" invariant + strict/neutral edge +
the multiplicative arm), `test-color-config.R` (CI forcing, implicit==explicit, ci="cell" error,
contrib/OR not forced), `test-calculations.R` (single-row test + chisq.test parity + the rename),
`test-display-extras.R` (the n row per sub-table, order, base).
**Suite: FAIL 0 | WARN 0 | SKIP 4 | PASS 2426 in 48.9 s.**

**Landmines hit while doing it — read before the next rename:**

- **`auto_or` / `pct_rowcol` are `all()` over the FACTOR col_vars, so on a numeric-only table they are
  `all(logical(0))` == TRUE, vacuously.** A `!auto_or` guard therefore silently excludes the numeric
  arm. Cost me a regression that the filtered run did not catch (the probe predated the guard) and only
  the full suite did.
- **`chi2` is THREE different names.** Renaming it needs classification, not `sed`: `tab()`/
  `tab_counts()` = the deprecated public arg (-> `test`); `tab_build()`/`tab_resolve_settings()` = the
  INTERNAL arg (keeps `chi2`, it drives `tab_chi2()`); `jmv_opts()`/`mk()`/`jmvtab_build()` = the jamovi
  OPTION (keeps `chi2` -- its `.a.yaml`/`.h.R` surface is compiled, and `jmvtab_build` reads
  `opts$chi2`); plus `tab_many()` (kept), list names, sprintf labels and test titles. A line-scoped
  regex over-reached into all of the last three. `jmvtab_build()` also called `tab(chi2 = )` itself, so
  the package tripped its own deprecation (-> now `test = opts$chi2`).
- **`tests/testthat/setup.R`'s `lifecycle_verbosity = "quiet"` has never worked** --
  `testthat::local_reproducible_output()` re-sets it to `"warning"` inside every `test_that()`. See the
  corrected comment there. Quiet comes from not calling the deprecated surface, or `suppressWarnings()`.
- **A new `ctx` field must be added in FOUR places**, or something breaks quietly:
  (1) `tab_build()`'s ctx; (2) `tab_counts()`'s ctx; (3) `test-carve-parity.R`'s hand-built ctx (a THIRD
  builder mirroring `tab_build`'s -- otherwise `tab_setup()`'s `list2env(ctx)` leaves the local
  undefined and every stage-composition test errors); (4) **`utils::globalVariables()` in
  `R/fmt_class.R`** -- `list2env(ctx, environment())` is invisible to codetools, so R CMD check NOTEs
  `no visible binding for global variable`. Only `devtools::check()` catches (4); the suite is green
  without it.


#### Phase 14b — tooltips + the numeric-diff display (DONE — 2026-07-17)

All seven bullets landed. **Full suite FAIL 0 | WARN 0 | PASS 2725; `check()` 0/0/0; NO golden
regeneration** — every `_golden/*.rds` and `_color_golden/*.rds` is byte-identical, only
`_snaps/render-html.md` moved (tooltip text + placement, reviewed). New: `test-tooltips-14b.R`,
`test-ci-ratio-katz.R`. Maintainer forks this session: do all 7 at once; mean diff = **raw signed
difference + a `std diff:` tooltip line** (NOT sd units in the cell — the number must stay `$diff`,
Excel writes the raw field, and `scale$std` belongs to the *colour* scale, which `color = TRUE` does
not even consult for a mean); placement = **`"auto right"`**, not a last-N-columns rule.

- **Numeric-diff display (the Phase-2 D3 leftover)** — `format()` now signs EVERY diff (`diff_signed`
  = `ok & display == "diff"`); the mean branch's `mult_sign` is gone (`+1.2` / `-0.22`, the variable's
  own units). The Excel `signed` mask widened to `display %in% c("ctr", "diff")` — excluding means is
  what would desync the bypass now. `×`/`÷` belong to `rr` alone.
- **Tooltip** (`tab_kable_print_tooltip`, now TEXT-only): shared `comparable` gate (the base-cell
  exclusion the diff line had, NA-safe, now also gating ratio + reused by `cond_ctr`); ONE `ref` token
  for the diff+ratio group (`ref_grp`); `type == "mean"` added to the ratio gate; `tip_num()` trims the
  column padding off every interpolated value; new `std diff:` line (Glass's Δ, mean columns, where
  `sd_ref` resolves). The `ref & any(ok_diff)` tautology is gone (it sat *inside* `if (any(ok_diff))`).
- **Fragment join rewritten** — the old chain pasted all fragments with a fixed `" ; "` then rewrote
  the result (`str_replace_all(";  ; ", "; ")` ×3 + trims + an `"NA ;"` scrub). Non-overlapping
  matching means one pass cannot collapse adjacent empties, so it silently assumed <5 in a row; the
  10th fragment makes 9-empty runs reachable (a Total cell is `n:` only). **A/B proved the OLD side
  wrong** (`"f1: 5 ;"` / `"; f10: 5"`). Now an exact per-cell non-empty join.
- **Placement** — ONE builder `tab_tooltip_attrs()` (`R/tab-render-html.R`) for both engines: the
  kableExtra path passes it pre-classed (`cell_spec()` honours a `ke_tooltip`/`ke_popover` verbatim),
  the html path pastes it into the `<td>`. `data-placement="auto right"` = Bootstrap's auto token
  (prefer right, reorient on overflow) — measured at render time, so it also covers a scrolled table
  or a narrow pane. ⚠ **`kableExtra::spec_tooltip()` cannot emit it**: its `match.arg()` takes ONE
  token from `c("right","bottom","top","left","auto")`, so `"auto right"` errors and `c("auto","right")`
  silently yields a length-2 attribute. Hence the hand-built string. `.tooltip-inner{max-width:none;
  white-space:nowrap;}` added to `tab_css(chrome = TRUE)` — ⚠ NOT scopable (bootstrap moves the
  tooltip to `<body>`, which is what stops the table clipping it), documented in place.
- **Two pre-existing bugs fixed in passing**: the html engine's popover rendered its own escaped
  ATTRIBUTE STRING as its content (`tab_kable_print_tooltip(popover=)` returned `spec_popover()`
  attributes from a *text* builder, and the engine wrapped them again) — the arg is deleted, attrs
  live only in `tab_tooltip_attrs()`; and the html popover omitted `data-trigger`, so it needed a
  CLICK where kableExtra's opened on HOVER (the shared builder ends the drift).
- **Katz ratio CI** — `ci_katz_rr()` (`R/tab-agg.R`), `ci_type = "ratio"` (the 4-site Phase-12a "or"
  pattern: enum, `ci_center()`, the colour gate, `format()`'s bracket + a 2-digit bump so the bounds
  do not round equal and collapse to a point). Trigger = `color_pct_text_is_ratio(spec)` (R/tab.R)
  -> `tab_build(color_ratio_ci=)` -> ctx -> `tab_resolve_settings()`, which emits the new per-row_var
  **`ci_scale`** ("diff"/"ratio", only where `ci == "diff"`) -> `tab_apply_tests()` -> `tab_ci(ci_scale=)`.
  Threaded exactly like 14a's `color_signif`, and for the same reason: `legacy_union()` maps every
  ratio onto a diff-family string, so the legacy `color` cannot carry it. **Proportions only** (a mean
  ratio needs Fieller) — which is also what keeps `color = TRUE` untouched, since a mean's *text*
  channel already IS the ratio.
- **The significance gate is now CI-driven, not measure-driven** (`fmt_color_plan`): an interval is
  significant when it excludes ITS OWN neutral (0 additive / 1 multiplicative). Keying it on the
  measure only held while each measure had exactly one possible ci_type. It also fixes a latent
  mismatch: measure `"or"` + a difference ci_type tested the diff bounds against 1 -> never
  significant (the hazard 14a's cascade works around with `& !auto_or`).
- **`rescale_bound()`** replaces the ad-hoc diff->ratio conversion: `diff` and `ratio` are both affine
  in the cell proportion with the reference at its point estimate (`ratio - 1 = diff / p_ref`), so ONE
  helper maps a bound either way by a ratio of offsets from the neutrals. The diff->ratio direction is
  byte-identical to the expression it replaces; ratio->diff is the new mirror (the derived bg channel).
- Legend names Katz off the STORED `ci_type` (not `method_diff`, which never built it) + FR
  translation (`po/R-fr.po`, `.mo` recompiled — **`gettext` had to be apt-installed on this box**;
  `tools::update_pkg_po()` needs `msgfmt`/`msgmerge`/`msginit`).

#### Phase 14c — colour legends (DONE — 2026-07-17)

All four bullets landed, plus **two defects the item-4 re-verification turned up** and **one the
`tab_plot` legend had been carrying silently**. Full suite **FAIL 0 | WARN 0 | PASS 2751**; `document()`
clean. Golden `_golden/*.rds` + `_color_golden/*.rds` **all byte-identical — no colour regeneration**;
only the two legend-bearing display snapshots moved (`render-html.md`: 17 spans gain
`font-weight:bold;`; `golden.md`: 8 md legend lines gain `**`), each diffed token-by-token first.

- **Bold break-words, every medium.** Runs already did; console composes `crayon::bold`, html emits
  `font-weight:bold` **inline**, md wraps `**[+5]{.p1}**`. Inline/markup rather than left to the
  stylesheet, because it must hold on the **background** channel (whose `.o*`/`.u*` stay unbolded —
  they mirror the cells, where a fill alone does not bold) and on the **kableExtra** path (no
  stylesheet of ours ships there).
- **`tab_css()`/`tab_md_css()` bold the text slots** (`.p1..m4{font-weight:bold;}`, emitted once,
  `chrome`-independent — it is theme-independent so it must not sit in the 4×-emitted rule table).
  This is the maintainer's separate "like in kable" note, and it IS kable: `tab_export_prep()`'s
  `bold = !is.na(text_hex) | ref_alltot` already bolds every text-coloured cell in kableExtra AND the
  html engine, so the rule is a **no-op there** and exists for the one medium with no other way to say
  it — `tab_md()`'s bare `[42%]{.p2}` spans.
- **Excel bg-legend readability** (decision 6). A rich-text run carries a font colour but **no fill**,
  so a background break-word is drawn as text — and the background palette (L 0.85–0.97) is invisible
  on white. New 9th palette `default_bg_legend_colors`/`_neg` = the same hues at **−0.2 OKLCH
  lightness** (chroma kept, gamut-capped), baked from new `dev/color_palette_tools.R::darken_for_legend()`,
  reachable as `get_color_style(type = "bg_legend")` (color_code-only: it substitutes for a fill, and a
  console has one → crayon aborts). **Light only, deliberately**: the legend cell's page is white
  whatever the `theme`, the dark fills (L 0.20–0.35) already read there, and −0.2 collapses them to
  black (measured: `#001b1b` → `#000000`, slots 3/4 both → L 0.10) — so `bg_legend_dark` is the dark bg
  palette unchanged. `set_color_palette(bg_legend_colors=, bg_legend_colors_neg=)` added; setting
  `background_colors` without them makes them follow the fills verbatim (a custom green fill must never
  keep the default blue legend word).
- **Console `theme` divergence** fixed: it read `options(tabxplor.color_style_theme)` while `slot_hex()`
  right above used the resolved `pal` — the two could disagree.
- **`medium = "excel"` → `"runs"`** (internal fn, 2 call sites): the concept is "draws TEXT, cannot
  fill", which is Excel **and** `tab_plot`. Both now take the bg_legend palette.
- **BUG FOUND + FIXED — `tab_plot()`'s legend was raw HTML in black.** It scraped the legend back out of
  the *html* rendering with regexes (`^color: rgba...`) that stopped matching when Phase 13b replaced
  kableExtra's `text_spec` spans with inline hex; every token rendered as e.g.
  `color:#02A5B3 !important;">+5` in uniform black. Rewritten onto `medium = "runs"` (the structure it
  always wanted: text + hex per token) — **~45 lines of regex deleted**, adjacent same-colour runs
  folded into one ggtexttable cell.
- **BUG FOUND + FIXED — two `tab_reg` legend wordings** (item 4 asked to re-verify β/IRR; β/SD and
  IRR-vs-OR from 13b hold, but): (1) a β legend said *"not significantly different from **the Total
  row**"* — a reg table has no total row; `legend_ref_info()` read ref_type "tot" like any fmt column.
  `is_coef` now takes the same "reference category" branch as OR/IRR (imprecise for a numeric
  predictor's per-unit β, whose null is 0 — the same approximation the OR arm always made). (2) a
  Poisson **IRR** was described as a *"Wald interval on the log **odds-ratio**"*: `ci_type = "or"` is
  the multiplicative **shape**, shared by OR / IRR / cumulative OR, so the name now comes from the
  effect word (+ 2 new FR strings, `.mo` recompiled).

**Flagged for the maintainer** (not fixed here — judgment calls, see the questions block after 14g):
the darkened light legend hues are faint (L≈0.65–0.77 at C≈0.03), and `tab_plot()`'s legend block
still holds ~60 lines of half-commented dead code.


#### Phase 14d — transpose, list container, `tab_xl` (DONE — 2026-07-17)

Every bullet landed. Full suite **FAIL 0 | WARN 0 | PASS 2782**. Conscious golden regen: **the `vars`
attribute only** — 28 of 36 `_golden/*.rds` gained it and are otherwise `identical()` (proven by
stripping the attr and comparing); the 8 that did not are raw `tab_num()` leaves, which never reach the
stage that records roles (the documented heuristic-fallback case). No `_snaps/` moved.

- **The framework fix — `vars` recorded, not inferred.** New table attribute
  `list(row_vars, col_vars, tab_vars, compacted)`, written in `tab_assemble_tables()` / `tab_compact()`
  and re-keyed by `tab_transpose()`; read via new **`tab_vars_recorded()`**, which **validates it
  against the real columns** (a dplyr chain can rename/drop them) → NULL → the old heuristic, so
  hand-built tables still work. ⚠ **CONTRACT**: `tab_get_vars()`'s `row_var`/`tab_vars` stay **column**
  names (what every consumer indexes with); `row_vars` carries the **source** names. They differ only
  on a merged table — conflating them would have broken every `x[[row_var]]`.
- **PREREQUISITE (done first, byte-identical): `tab_attrs()` / `tab_restore()` / `tab_bind_attrs()`.**
  The ~34 dplyr S3 methods + vctrs reconcilers each named every attribute by hand, so `subtext` / `test`
  / `render_extras` / `ci_settings` had each paid the same ~34-site edit. A 5th attribute would have
  paid it a 5th time. Now: one `new_tab()` formal + a getter/setter + **one line in `tab_attrs()`**.
- **`tab_compact()` re-merge guard.** The heuristic used to catch an already-merged table *by accident*
  (reading its synthetic `row_var` column as a tab_var → the bail). Truthful roles remove that accident,
  so the guard is now explicit (`compacted` → no-op) — otherwise it would have merged a second time.
- **`tab_transpose()` with several row_vars.** Folds the `(row_var, levels)` pair into one key column so
  the existing single-row_var pivot runs unchanged; each old row_var becomes a **col_var** with its own
  total/reference column (exporters span its name over its levels for free). Levels are suffixed
  `_<var>` only where two row_vars share one (tab()'s own `Other_race` convention, which
  `tab_col_var_header()` already strips). The total-row guard is now per sub-table.
- **BUG FOUND — `dplyr::pull(tabs, all_of(row_var))` read the DATA MASK.** tidyselect resolves
  `row_var` against the columns first, and a merged table has a column literally *named* `row_var` — so
  it silently pulled that column instead of the local variable. Latent (a merged table never got past
  the old guard); now `tabs[[row_var]]`.
- **Never merge a list at export** (decision 5). Deleted the branch **and `tab_list_mergeable()`** —
  which re-ran `tab_get_vars()` over every tab immediately before `tab_compact()` re-ran the identical
  scan. `tab_resolve_tables()`'s `compact` arg is gone (dead; nothing read `meta$compact`).
- **`tab_xl`**: new shared `xl_finish()` → `cat`s the resolved path (decision 8) and **fixes the
  double-resolve** (`tab_xl_resolve_path()` is NOT pure — with `replace = FALSE` it auto-numbers past
  the file it just wrote, so the two degrade paths opened a file that never existed). `tab_get_titles()`
  rewritten per decision 4 (real names via `vars`, elide past 3 with "+N more", no NA fall-through);
  mean/sd headers → `mean` / `sd` under the col_var span, **gated on the split existing** so the text
  backends (sd inline) are untouched — their wording is 14e's.
- **`transpose` now runs BEFORE materialise**: the extras are ORIENTED (add_n is a column under row%, a
  row under col%), so materialising first baked the pre-transpose orientation in. `tab_md(transpose =
  TRUE)` of a row% table is now **byte-identical** to the native col% table (test-locked).

**Flagged for the maintainer** (see the questions block after 14g): a pre-existing golden drift
(`n_ci_tabvars*`'s `ci_sup` `NA`→`NaN`, invisible to `expect_equal`'s tolerance, reproduces on
unmodified HEAD) is now baked in; and the Excel mean/sd column WIDTH was not narrowed.


#### Phase 14e — the html engine becomes the default, and is designed properly (DONE — 2026-07-17)

`options(tabxplor.tab_kable_engine)` is now **`"html"`**. Full suite **FAIL 0 | PASS 2812**;
`check()` 0 errors / 0 warnings / 0 notes. Only `_snaps/render-html.md`'s 4 html-engine snapshots moved
(reviewed); no `_golden/*.rds`, no `_color_golden/*.rds`. A browser-checkable sample is written to
**`dev/review_manual/phase14e_html_engine.html`** (theme = "auto" + a composite-display table).

**The governing decision: the engine emits NO inline styles.** Every look — geometry included — is a
**role class** resolved by `tab_css()` (`tx-r`/`tx-l`, `tx-num`, `tx-br`/`tx-bl`, `tx-tot`/`tx-rv`,
`tx-b`, `tx-bt`/`tx-bb`/`tx-bb2`, `tx-span`, `tx-pill`). Three reasons, in order of weight: (1) **an
inline style cannot be overridden by a user's CSS**, so the maintainer's own rule — *"must continue to
work with common css customisation, as kableExtra does... a good, compact, readable default that can be
overwritten"* — was **impossible** while the engine wrote its own borders/widths; (2) it removes the
INLINE half of the coloured-border bug (`border-right:1px solid` is a shorthand → resets `border-color`
to `currentColor` = the cell's palette hex; inline it also beat the stylesheet's rule) — ⚠ **14e claimed
this fixed the bug and it did not**: a class still out-specifies `td{border-color:…}`, so the shorthand
kept winning until **Phase 14j** replaced it with longhands (§40); (3) the markup
shrinks. This extends 13d's colour rule to everything. **Consequence**: `css = FALSE` + no `tab_css()`
now renders *unstyled*, not merely uncoloured.

- **Viewer/knit routing**: `tab_kable_join()` claims the **`kableExtra` class** for the html output (it
  IS an html fragment with `format = "html"`) rather than duplicating `print.kableExtra` /
  `knit_print.kableExtra`. Ends the maintainer's hand `class<-` workaround. kableExtra is a Suggests →
  absent, the class is inert and it falls back to today's `cat()`.
- **BUG — a wrapped header rendered its `<br>` literally.** `tab_wrap_text()` wraps long header names
  on `<br>`, and the engine html-escaped the whole label. kableExtra never hit it (`kable(escape =
  FALSE)`). New `html_escape_br()`: escape, then restore **only the tag we inject** — a `<` in a user's
  own level name stays escaped (test-locked both ways).
- **Fonts** DejaVu Sans Condensed (text) / DejaVu Sans (numbers), mirroring `tab_xl`'s
  `font_text`/`font_num` — kableExtra used DejaVu Sans throughout. **Geometry**: `padding:3px 4px`
  (~1mm sides, was touching the border) + `line-height:1.1` (was 0.85, crammed). **Hover** →
  kableExtra's lightable yellow. **Dark** → `#CECDC3` on `#222222` (pure white on near-black glares).
- **Background = a PILL** (`<span class="tx-pill o3">`) hugging the text, rounded — a full-cell flood
  reads as a blocky grid **and** swallows the row hover (a child's background always paints over its
  row's, whatever the specificity; kableExtra escaped this only because it fills a `<span>`).
- **U+2007 figure space** (decision 1): new `format(pad =)`, defaulting to `fig_space` when
  `html = TRUE` and a plain space otherwise, threaded through all 6 alignment sites + the composite
  recursion; `tab_xl` passes it explicitly (⚠ `html = TRUE` is NOT the lever there — it would also
  switch on the html-only `<sub>` markup). Console/md keep ASCII, so their goldens are byte-identical.
- **Test-suite trap found**: the `kableExtra engine (default)` section relied on the DEFAULT, so
  flipping it made the whole section silently assert against the *other* engine. Every call there now
  pins `engine = "kableExtra"`.
- **Bug caught by our own CSS well-formedness test**: a rule accidentally split across two `c()`
  elements became two broken lines. Worth keeping that test.

**DEFERRED (flagged for the maintainer, see the questions block after 14g):** the **VS Code/Positron
webview hooks** (`body.vscode-dark` / `data-vscode-theme-kind`) — the roadmap itself demands a live DOM
check FIRST (R html usually lands in an *iframe*, and the class sits on the OUTER webview body, so the
hook may never match); `pct="col"` compactness and the `min-width:10em`/`5.5em` review (needs a visual
judgment); tooltip dark styling; `inst/tab.css` is now dead for the default engine (all
`.lightable-classic`-scoped) — kableExtra-only, left alone.

#### Phase 14f — `tab_md` (DONE — 2026-07-17)

Full suite **FAIL 0 | PASS 2850**. Conscious golden regen: `_snaps/golden.md` only (the md layout
changed on purpose — see below); no `.rds`, no `render-html.md`.

- ⛔ **THE FIND: `tab_md()`'s output was NOT VALID PANDOC — every normal table.** The 13c-iii col_var
  name row sat ABOVE the level header, i.e. a **two-row header**, which pipe tables do not have:
  pandoc gives up and renders the whole thing as a line-block + a paragraph of pipes (reproduced on
  pandoc 3.7 with tabxplor's own output; 0 `<td>` emitted). It had been shipping since 13c-iii because
  **nothing ever rendered the md** — every test asserted on the markdown string. Fixed by moving the
  name to the **first BODY row** (decision 3): italic, in the FIRST cell of its group, one cell per
  column (never merged — a pipe row must keep the cell count or pandoc shifts the data), that row
  deliberately not pipe-*aligned* (a long name overflows rather than widening every column below).
  New **`tab_md(col_var_names = FALSE)`** drops it. **New test renders through pandoc** across 6
  shapes — the test that was missing.
- **Two more invalidities**: the spacer column's delimiter cell was `| |` (not a legal delimiter →
  `md_insert_col_sep(fill = "-")`, since one helper builds all 4 row types); and a `|` in a level label
  opened a spurious cell (now escaped `\|`, label columns only — fmt cells are package-formatted).
- **Padding model rebuilt around the VISIBLE end.** The bold rows' `+4` entered `num_width`, which
  pads INSIDE the bracket → `[    38%]{.p2}`: four spaces pandoc discards, and which push the number
  *out* of line with the bold cell in the raw file. Now each cell pads by its own visible-end width
  (`md_extra()`: markup that PRECEDES the last visible character — 0 plain, 2 whole-bold since its
  closing `**` follows the value, **4 composite-bold** whose closing `**` sits mid-cell before the
  `(n=…)` tail), so the markup grows leftwards into the pad and every number shares a raw column. The
  attr is padded to `attr_width` so `}` lines up (verified: pandoc reads `{.m2   }` == `{.m2}`).
- **`css = TRUE` now wraps the table in a pandoc fenced div** `::: {.tabxplor-tab}` → pandoc emits
  `<div class="tabxplor-tab">`, the hook every `tab_css()` rule already matches (pandoc emits a BARE
  `<table>`, which none could reach) — so `chrome = TRUE` is meaningful for md for the first time and a
  rendered md table gets the layout, not just the colours. `.tabxplor-tab table` added to the
  border-collapse rule (the class is the table itself in html, a wrapping div in md).
- **The existing test suite earned its keep twice**: the pipe-grid test caught my first name-row draft
  merging cells, and the numbers-aligned test caught the composite-bold case. Both metrics needed
  fixing too (they measured the RAW end, so a bold cell could only agree by accident).
- `tab_export("html_md")` — **declined** (the maintainer's own note): `tab_kable(engine = "html")` IS
  "markdown rendered to a styled html table", and the real ask (md renders well in Quarto) is what the
  validity + fenced-div fixes deliver.

#### Phase 14g — console theme / IDE detection (DONE — 2026-07-17)

**It works, end to end, on your machine.** New **`R/tab-theme-detect.R`**: `tx_detect_theme()` →
`"light"`/`"dark"`, wired into `set_color_palette(theme = "auto")` (new value) and `.onLoad`. Verified
live here: `workbench.colorTheme = "Starless Monokai Atom"` → `izumii.starless-monokai/package.json` →
`uiTheme: vs-dark` → **dark** ✓. Full suite **FAIL 0 | PASS 2897**; `test-theme-detect.R` (41) drives
every probe from **injected fixtures**, so it never depends on the host IDE.

- **Your live test is NOT needed — don't bother running it.** The roadmap flagged
  `.ps.ui.evaluateWhenClause` as a confirmation step you'd have to try by hand. The History-cache chain
  resolves the theme on its own, so the private ark RPC (`# TODO: Unexport these methods`) is not used
  at all. One fewer thing depending on an unexported API.
- **A roadmap measurement was stale, in our favour**: `POSITRON=1` and `TERM_PROGRAM=vscode` ARE set in
  the Positron **integrated terminal** (recorded as empty → *"terminal-side detection is dead here"*).
  Since that is where you actually run R, detection works there — and it is right on the merits, the
  terminal's background being the editor theme's.
- All five traps encoded: `isAvailable()` lies in ark (gate on `hasFun()` + `RSTUDIO=="1"`); `$dark` can
  be NA (`isTRUE`); `readRStudioPreference()` always returns your default (unused); the theme NAME is
  never a signal (exact-name → `uiTheme`); `autoDetectColorScheme` → bail (colorTheme is then stale).
- **PRIVACY honoured**: two keys pulled by regex, the file never parsed (it is JSONC anyway), so the
  `claudeQuota.sessionKey` beside them never enters R. Test-locked with a fake secret in the fixture.
- **Never warns** (not just never errors): `readLines()` warns *before* it errors, so `tryCatch(error=)`
  let it through — `file.exists()` first. `expect_silent`-locked.
- **Cost**: the extension scan is one level deep — recursive cost **70 ms at every load**, now **9 ms**,
  and only inside Positron.
- ⚠ **`setup.R` now pins `tabxplor.color_style_theme = "light"`**: detection makes the default
  machine-dependent, which is exactly the CI-passes/local-fails divergence the 2026-07-15 green-up
  spent a day on. Two colour-legend tests that read the option were pinned too.

- **Not done** (deliberate): re-detecting at PRINT. The resolved value is stored, so switching your
  editor theme mid-session needs another `set_color_palette(theme = "auto")` — per-print detection
  would mean an rstudioapi RPC / a file scan on every table.

##### Original research (historical intent)

**The research paid off — there IS a workaround, and it works on your actual setup.** Upstream is a
dead end ([posit-dev/positron#2986](https://github.com/posit-dev/positron/issues/2986), *"Support
rstudioapi::getThemeInfo()"*, OPEN since 2024-05, motivated by `thematic`, bounced `Future` → `RC` →
`Post-RC` → back to `Future` in 2025-12, one maintainer reply in two years). Neither `cli` nor `crayon`
detects a background — verified in their installed sources; cli knows *how many* colours, never *which*
background. But two local oracles exist:

**Verified working on your machine** — your client `settings.json` IS reachable from WSL despite
`C:\Users` being unmounted: VS Code caches it server-side under
`~/.positron-server/data/User/History/<hash>/`, and that cache **updates on live writes** (snapshots
grew 582→585 lines as extensions called `configurationService.updateValue()` — the same path the theme
picker uses, so it is not a stale manual-save snapshot). The full chain resolves in R today:
`workbench.colorTheme = "Starless Monokai Atom"` → `izumii.starless-monokai/package.json` →
`uiTheme: vs-dark` → **DARK** ✓ (correct for you). `window.autoDetectColorScheme` is not set for you, so
`workbench.colorTheme` is authoritative.

**A second, live oracle** — `.ps.ui.evaluateWhenClause("config.workbench.colorTheme == '<name>'")`. The
mechanism is proven (ark itself ships `config.git.enabled && gitOpenRepositoryCount > 0` through this
exact RPC), and jennybc's own note on #2986 points at it. It can only *test equality*, never read the
value — so it is a **confirmation** step for a name the History cache already supplied, not a probe.
(Two research passes disagreed here and the conflict resolves cleanly: VS Code exposes **no theme-KIND
context key** — you cannot ask "is it dark?" — but it *does* expose `config.<setting>` keys, so you can
ask "is the theme named X?". Both statements are true; only the name-equality question is answerable.)

Design — `tx_console_theme()`, layered, every step `tryCatch`-wrapped, defaulting to `"light"`:

1. explicit `options("tabxplor.color_style_theme")` always wins;
2. **RStudio** → `rstudioapi::getThemeInfo()$dark`, re-checked at **print** (today it is one-shot at
   `.onLoad`, [tab_classes.R:3428-3437](R/tab_classes.R#L3428), so a mid-session switch is missed);
3. **Positron** → History-cache `workbench.colorTheme` → `uiTheme` (extension `package.json`, plus a
   small hardcoded table for builtins, which have **no** server-side `package.json` — 62 builtin
   extensions, zero with `uiTheme`); optionally confirmed via `evaluateWhenClause`;
4. terminal → `COLORFGBG`; else `"light"`.

Copy **`cli:::detect_dark_theme()`**'s shape ([cli/R/themes.R:326](https://github.com/r-lib/cli/blob/main/R/themes.R))
— `RSTUDIO` env → `getThemeInfo()$dark`; iTerm → AppleScript; Emacs → `ESS_BACKGROUND_MODE`; else FALSE —
and extend it with the Positron branch. That is the best-in-class prior art: **no R package detects
Positron's theme** (thematic assumes light + warns; cli returns FALSE; crayon/gt/reactable/colorspace/
ggthemes/unikn don't try — several don't even depend on rstudioapi).

Five traps the implementation MUST encode (each source-verified):

- **`getThemeInfo()` errors, it does not degrade.** ark fakes `isAvailable() → TRUE`
  ([ark init.R:103](https://github.com/posit-dev/ark/blob/main/crates/ark/src/modules/positron/init.R)),
  so `verifyAvailable()` passes and `findFun()` then `stop()`s. The usual
  `if (rstudioapi::isAvailable()) getThemeInfo()` idiom **breaks in Positron**. Gate on
  `rstudioapi::hasFun("getThemeInfo")` (thematic's guard) *and* `Sys.getenv("RSTUDIO") == "1"`, never on
  `isAvailable()`.
- **`$dark` can be `NA` even in RStudio** — [tidyverse#88](https://github.com/tidyverse/tidyverse/issues/88),
  [rstudio#4850](https://github.com/rstudio/rstudio/issues/4850); cli's NEWS records a crash from exactly
  this. Use `isTRUE()`, never `if (info$dark)`.
- **`readRStudioPreference()` lies silently.** Its ark shim is literally `function(name, default)
  default` — it *shipped*, so `hasFun()` returns TRUE and it always returns your default. Never use it.
- **Name regex fails on your own theme.** `"Starless Monokai Atom"` contains neither "dark" nor
  "light" yet is `vs-dark`. Exact-name → `uiTheme` resolution is mandatory; no substring guessing.
- **Detect Positron by `.Platform$GUI == "Positron"`** (ark force-rebinds `.Platform` in `baseenv()`,
  [ark positron.R](https://github.com/posit-dev/ark/blob/main/crates/ark/src/modules/positron/positron.R))
  or `Sys.getenv("POSITRON") == "1"` — but **only in the console**. Measured in your WSL2 integrated
  terminal: `.Platform$GUI = X11`, `POSITRON` empty, `RSTUDIO` empty, `TERM_PROGRAM` empty — only
  `VSCODE_*` is present, despite [positron#3842](https://github.com/posit-dev/positron/issues/3842)
  being closed. **Terminal-side detection is dead here**; don't build on it.

Bail to `"light"` (never guess) when: `window.autoDetectColorScheme` is TRUE (the active theme then comes
from `workbench.preferredDark/LightColorTheme` following the OS, so `workbench.colorTheme` is **stale and
wrong** — it is not set for you, but must be guarded); the theme name resolves to no `uiTheme`; or the
History cache is absent.

⚠ **Two things only the maintainer can settle** — surface both before implementing:

- **One live test.** `.ps.*` exists only inside ark, so it could not be executed from `Rscript`. Run in
  the Positron console: `as.environment("tools:positron")$.ps.ui.evaluateWhenClause(
  "config.workbench.colorTheme == 'Starless Monokai Atom'")` → expect `TRUE`.
- **Privacy.** That History `settings.json` also holds a live `claudeQuota.sessionKey`. The parser must
  read **only** `workbench.colorTheme` / `window.autoDetectColorScheme` and never echo, log or error
  with file contents.

Honest fragility (the researcher recommends against shipping it; I lean *ship it, gated and silent*
since it is best-effort and degrades to today's behaviour): `.ps.*` is private and carries
`# TODO: Unexport these methods` in ark's source; a client-only theme extension has no server-side
`package.json`; and the Positron console is *independently* themable (`positronConsole.background`), so
a correct global answer need not match the console. Note the **export** side is already correct — Phase
13d's `theme = "auto"` delegates to the browser via `prefers-color-scheme` + toggle hooks, the only
layer that can truly know. This phase closes the **console** gap only.

Also here: `tx_ide()` (rstudio/positron/vscode/terminal/jamovi), used to re-check the
`bit8 <- Sys.getenv("RSTUDIO") == "1"` 24-bit fallback; `set_color_palette(theme=)` must accept
`"auto"` ([tab_classes.R:3435](R/tab_classes.R#L3435) currently `stopifnot(theme %in% c("dark","light"))`).
Tests must not depend on the host IDE: unit-test the name→uiTheme resolver and the layering with
injected fixtures.

---

#### Context : Phases 14h to 14o

`dev/review_manual/tab_manual_review_pass_2.R` (+ the mid-session `tab_md_test_2.md`/`.htm`) is the
maintainer's second hands-on review of 1.4.0 on real survey data. Its `#` comments are the spec. Phases
14a–14g are committed; this plan turns pass 2 into phases 14h–14o, each a **fresh Claude Code session**.
The three hard ones (14m, 14n, 14o) **start with a design step, not with code**.

Every defect below was **reproduced and root-caused** during planning, not guessed. Five have causes
neither the review nor the roadmap had named, and three of those change the shape of the fix:

| #  | Symptom (maintainer)                                  | Verified root cause                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  |
|----|-------------------------------------------------------|----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| 1  | `row_var` name repeats on every row (html, md, Excel) | **A Phase 14d regression.** `tab_md()`'s blanking loop ([tab_md.R:271](R/tab_md.R#L271)) is gated on `tab_vars`; 14d made `tab_compact()` correctly record `tab_vars = character(0)` ([tab_classes.R:1243](R/tab_classes.R#L1243)), so the loop went silent. The html engines never had blanking at all — they sidestep real tab_vars with `drop_tab_vars = TRUE`, which a compacted table never triggers. **md does not "already do it" — it stopped.**                                                                                                             |
| 2  | Excel title `levels by ROCK, JAZZ, CLASSIQUE +8 more` | `tab_render_vars()` DOES return `row_vars` (the source names, [tab.R:2653](R/tab.R#L2653)), but `prep_one_table()` rebuilds `vars` without it ([tab-export-prep.R:313](R/tab-export-prep.R#L313)) → `tab_xl`'s `.$vars$row_vars %||% .$vars$row_var` ([tab_xl.R:196](R/tab_xl.R#L196)) falls back to the literal `"levels"`. **One line.**                                                                                                                                                                                                                           |
| 3  | `(n=1 811)` padding wrong                             | The thousands separator is a **plain ASCII space** ([fmt_class.R:1992](R/fmt_class.R#L1992) `prettyNum(big.mark = " ")`) — half a digit wide in DejaVu Sans, and collapsed by CSS. `big.mark` never consulted `pad`, so the figure space that 14a-decision-1 introduced fixed the padding and the separator kept breaking it.                                                                                                                                                                                                                                        |
| 4  | Borders take the text colour                          | ✅ **FIXED in 14j.** **Not fixed in 13d/14e, only narrowed** — 3 docs + NEWS recorded it as fixed and nothing tested it. `.tabxplor-tab .tx-br{border-right:1px solid;}` (0,2,0 — [tab-css.R:199](R/tab-css.R#L199)) is a SHORTHAND: it resets `border-right-color` to `currentColor`, and it out-specifies `.tabxplor-tab td{border-color:…}` (0,1,1 — [tab-css.R:107](R/tab-css.R#L107)). Live on `tab(gss_cat, marital, c(race, relig), pct="row", color="diff")`: 6 of 140 `<td>`s carry both a border class and a colour class (incl. `.g1` grey → grey border). |
| 5  | "levels and Total columns very wide for nothing"      | ✅ **FIXED in 14j — but the roadmap's diagnosis below was WRONG.** The min-widths were a sideshow: the real cause was the colour legend in `<tfoot><td colspan>` (**327 chars on one line** vs a widest data cell of 23) deciding the table's max-content, so the table took the whole pane and auto layout padded every column. Both are fixed (`.tx-foot` + the min-widths deleted). Original note: the **only** widths in the stylesheet are `.tx-rv{min-width:10em}` and `.tx-tot{min-width:5.5em}` — exactly the two columns named.                              |
| 6  | Excel numbers in Condensed                            | The XML **already says** `DejaVu Sans` (verified: `Excel_test.xlsx` fonts 1–10). But `openxlsx2::create_font()` defaults **`scheme = "minor"`** and we never override it ([tab_xl.R:640](R/tab_xl.R#L640)), tagging every font as "the theme's minor font" — which `xlb_base_font()` sets to `font_text` = **DejaVu Sans Condensed** (verified in the file's `theme1.xml`).                                                                                                                                                                                          |
| 7  | `theme="auto"` always dark in the Viewer              | The dark layer is `@media (prefers-color-scheme: dark)` ([tab-css.R:222](R/tab-css.R#L222)), which in an Electron webview follows the **OS**, not Positron's colour theme — so toggling Positron cannot move it.                                                                                                                                                                                                                                                                                                                                                     |
| 8  | Transpose colours numerics wrongly                    | `tab_transpose()` copies **ONE representative column's** `fmt_col_attrs` onto every transposed column ([tab.R:2445-2456](R/tab.R#L2445)). A transposed column mixes variables; one fmt column cannot carry two `type`/`digits`/`color` values. **Unfixable at the object level.**                                                                                                                                                                                                                                                                                    |
| 9  | md spacers/separators bad in rendered html            | Verified against pandoc 3.7 on `tab_md_test_2.md`: spacers become real empty `<th></th>`/`<td></td>` columns with `padding:3px 4px`; the sub-table separators ([tab_md.R:490-511](R/tab_md.R#L490)) become literal rows of dashes.                                                                                                                                                                                                                                                                                                                                   |
| 10 | md rendered html: a border under **every** row        | **Not our CSS — the host's.** Quarto tags the table `class="caption-top table"`, and Bootstrap's `.table > :not(caption) > * > *` sets `border-bottom-width` on every cell (its `.table-borderless` sibling is in the file, so Bootstrap is confirmed present). `tab_css()` has no such rule: its border rules are all class-gated, and md-rendered html carries no classes — it only sets `border-color`, which is why the host's borders come out **black on every row**.                                                                                          |

##### Answering the review's two questions about the DOM capture

- **`dev/review_manual/Positron_Inspect_kable_theme_auto.html` is the OUTER workbench DOM** — our markup
  appears in it only as escaped console text (`&lt;table class="tabxplor-tab"&gt;`), so it cannot show the
  table's own `<body>`. What it *does* prove is decisive: the Viewer is a **cross-origin webview iframe**
  (`vscode-webview://…/index-external.html?…extensionId=positron.positron-r`). VS Code's
  `body.vscode-dark` sits on the outer host; it cannot reach our document. **Do not ship the `vscode-*`
  hooks the roadmap contemplated** — they would never fire.
- **Ask the maintainer for one live check** (14k): with `theme = "auto"` open in the Viewer, toggle
  **Windows** dark mode. If it flips, the webview follows the OS (expected); if it stays dark, something
  forces it and the diagnosis needs one more pass.

##### Settled this session (maintainer)

1. **One Total row**: collapse only when the total rows are identical **as displayed** — same rendered
   strings at the chosen digits. 17.22% and 17.31% both printing "17%" still collapse; the diffs/CI were
   computed per block beforehand and stay right.
2. **Transpose**: soft-deprecate `tab_transpose()`; flip only at export, on the render model.
3. **`theme="auto"`**: resolved R-side for the interactive Viewer, browser-side for files. ~~Option default
   `"light"` → `"auto"`.~~ **AMENDED 2026-07-17 (14k): the option default STAYS `"light"`; `"auto"` is
   opt-in only** — unlike the console, an export is read who-knows-where, so a dark table must be asked
   for, never inferred.
4. **Excel legend**: keep the lighter L ladder, boost chroma proportionally (tunable + preview).
5. **`var_names = c("both","rows","cols","none")`** — one shared arg; `tab_md(col_var_names=)` deprecated onto it.
6. **Title**: dependent first, decided by `pct`; max 2 names + "+N more".
7. **Figure space**: html + Excel only; console and md keep ASCII (monospace — an ASCII space is already
   exactly one digit wide there).
8. **`color_type`**: deprecate (confirmed vestigial).

---

#### Phase 14h — one digit-width space, everywhere it must align

Mechanical, cheap, no design. Do it first: 14i/14j/14m all read the padded output.

**Why** — finding 3, plus four siblings found with it:

- `big.mark = " "` ([fmt_class.R:1992](R/fmt_class.R#L1992)) and the same in `print_reg_footer`'s
  `fmt_val` ([tab_classes.R:714](R/tab_classes.R#L714)).
- The Excel star pad is ASCII: `formatC(st, width = -w)` ([tab_xl.R:425](R/tab_xl.R#L425)) — inconsistent
  with [:418](R/tab_xl.R#L418), which already passes `pad = fig_space`. Its width mask
  (`st[val & nzchar(st)]`) also differs from `format()`'s (`st[val]`, [fmt_class.R:2205](R/fmt_class.R#L2205));
  they agree only because `nchar("") == 0`.
- The mean/sd joiner is `unbrk` (U+202F, [fmt_class.R:2091](R/fmt_class.R#L2091)) — a narrow no-break space
  inside a cell whose digits must align. This is the review's "replace all unbreakable spaces with this
  good 1 digit sep, everywhere padding have to be aligned well".
- **sd-less mean cells are not padded**: `format()` right-pads the sd *text* ([:2089](R/fmt_class.R#L2089)),
  but a cell with no sd gets nothing → `1.0` and `1.7 (σ2.1)` do not align.
- **`bold_split` misses the mean/sd cell**: it covers `{}` templates only; the `disp_mean_sd` branch sets
  no `primary_nchar`, so a bold Total row bolds `4 (σ11)` whole — and bold DejaVu Sans is wider than plain,
  which is the review's "bold cells are not perfectly aligned with plain font weight cells".

**What**

1. `big.mark = pad` in `format.tabxplor_fmt()`. `pad` already resolves per backend
   ([fmt_class.R:1828](R/fmt_class.R#L1828): `" "` text / `fig_space` html; `tab_xl.R:418` passes it
   explicitly) → **console + md unchanged, html + Excel aligned, one line**. Same in `print_reg_footer`.
2. `pad`-ify the mean/sd joiner and the Excel star pad; unify the two star-width masks.
3. Pad an sd-less mean cell to the column's `unbrk (σ…)` width ([fmt_class.R:2085-2092](R/fmt_class.R#L2085)).
4. Extend `primary_nchar` to the mean/sd branch → `bold_split` bolds only the mean; `(σ…)` stays plain in
   md + both html engines. Excel needs nothing (mean and sd are separate columns there).
5. Delete `cross` ([utils.R:1185](R/utils.R#L1185)) — a dead byte-identical duplicate of `mult_sign`, zero
   call sites.

**Verify** — `test-display-grammar.R` / `test-fmt_class.R`: `format(html = TRUE)` of a 4-digit-`n` column
contains no ASCII space; a mean column with a mixed-NA sd has constant `nchar()`; `bold_split` splits a
mean cell at the mean. `test-tab_xl.R`: the star pad char. **Expected regen**: `_snaps/render-html.md`
only — `_snaps/golden.md` (console, `pad = " "`) must NOT move; if it does, the change leaked.

**Do not touch**: the U+202F in row LABELS ([tab_classes.R:2299-2315](R/tab_classes.R#L2299), the
`unbreakable_spaces` option). It is not padding; it is the separately-flagged "is this deliberate?" item.

##### Done (2026-07-17)

Full suite **FAIL 0 | WARN 0 | PASS 2923**; no `_golden/*.rds` and no `_color_golden/*.rds` touched.
Conscious regen of the two DISPLAY snapshots only (see below). New `test-digit-space.R` (26).

- **`big.mark = pad`** ([fmt_class.R](R/fmt_class.R)) — the one-line fix. `pad` already resolved per
  medium, so the console/markdown keep the ASCII space (already exactly one digit wide there) and
  html/Excel get the figure space. Proof: the whole `_snaps/render-html.md` diff is **28 ASCII spaces
  becoming U+2007** — mapping U+2007 to a space on both sides makes new and old byte-identical.
- **Excel star pad** ([tab_xl.R](R/tab_xl.R)) — `formatC(width = -w)` (ASCII) -> `str_pad(pad = fig_space)`,
  and the two star-width masks unified on `st[val]` ("" is width 0, so it IS the column max).
- **sd-less mean cells padded** to the `(sigma sd)` tail, so the MEANS align, not the cell edges.
  ⚠ Exact in the console/markdown (monospace) only: in html/Excel it lands within ~1 digit-width,
  because `(`, sigma and `)` are not digit-wide — **no run of spaces can match them**. An exact fix
  needs markup (a hidden tail), which belongs to 14j, not to `format()`.
- **`bold_split` reaches the mean/sd cell** — a bold row now renders `**47.2** (sigma17.3)`, the tail
  plain, exactly as a composite `{pct} (n={n})` cell does. `prim_nchar` moved above the
  `special_formatting` block (two branches write it now) and is attached only when something actually
  split, so the output stays attribute-free otherwise.
- **`cross` deleted** (a byte-identical duplicate of `mult_sign`, zero call sites). Two stale
  `fmt_class.R` header lines fixed in passing (`cross`; and "for type=mean, diff stores a RATIO" —
  false since Phase 2).

**BUG FOUND AND FIXED while building, which the suite did NOT catch**: the sd-less mask keyed on
`is.na(get_var(x))`, which is **also true of an EMPTY cell** — so padding pasted onto the NA and
produced the literal string `"NA       "`. Only the `na` argument (kable/md pass `""`) hid it; the
console, which keeps NA, printed it. Fixed with `!na_out`, regression-tested. The lesson for 14i/14j:
`is.na(var)` is not "has no sd", it is "has no sd **or is empty**".

**Deviation from the plan, deliberate**: the plan's "pad-ify the mean/sd joiner" was **not done**, and
`print_reg_footer`'s `big.mark` needed **no change**. The joiner (`unbrk`, U+202F) is a non-breaking
SEPARATOR, identical in every cell of the column, so it cannot misalign anything; making it `pad` would
lose the no-break property in md, and making it `fig_space` would move the console snapshot AND require
teaching the plot backend's three `unbrk`-strip sites ([tab_classes.R:1740](R/tab_classes.R#L1740),
[:1744](R/tab_classes.R#L1744), [:1871](R/tab_classes.R#L1871)) about a second exotic space — all for a
sub-glyph gain inside an approximation that is inherent (above). `print_reg_footer` is console-only, so
its ASCII `big.mark` was already the right glyph; the EXPORT footer renders through `format()`'s `gof`
token and got the fix for free.

**Snapshots regenerated (conscious)**: `_snaps/render-html.md` (the space swap, proven above) and
`_snaps/golden.md`. The latter was NOT expected to move; it did, for three reasons, each verified by
normalising every padding difference away and re-diffing — no number and no content changed:
(1) sd-less means are now padded; (2) bold mean cells split; (3) md's column budget grows by 2 on those
columns, because `md_extra()` correctly charges 4 markup columns for a partial-bold cell instead of 2.

---

#### Phase 14i — the variable-name model (one shared label column) (DONE — 2026-07-17)

Both findings fixed. Full suite **FAIL 0 | WARN 0 | PASS 3023** (+100); `document()` clean. Every
`_golden/*.rds` and `_color_golden/*.rds` **byte-identical**, `_snaps/render-html.md` unchanged; the
ONLY churn is one conscious `_snaps/golden.md` line (a tab_var label cell de-bolded — see below).
Browser/Excel sample: `dev/review_manual/phase14i_var_names.{html,md,xlsx}`.

**The shape: two roles, and both `var_names` drops live in the prep.** The insight that shrank the
phase — all four backends ALREADY gate the col_var span on `any(nzchar(cvh$label))` (md, kableExtra,
the html engine, and tab_xl's `has_span`, which also drives its geometry offset). So blanking
`col_var_header$label` in `prep_one_table()` drops the span row **everywhere with zero backend code**;
the row-side drop is the twin (drop the column before the role detection, and even `tab_plot` — which
reads no header model — inherits it). Two roles, deliberately distinct (conflating them would rotate
"Male"/"Female"), **mutually exclusive by construction** since `tab_compact()` bails on tab_vars:
- `roles$label_cols` + `roles$label_runs` — the leading factor cols whose value repeats down a block
  (the synthetic `row_var` col when `compacted`, else the kept `tab_vars`). ONE run model, four
  consumers: md blanks, the html engine `rowspan`s, Excel merges, tab_plot blanks.
- `roles$var_name_col` — the name-VALUED subset only: `var_names` drops it, its header always blanks,
  html/Excel rotate it, md italicises it, and it is never bold. A tab_var's values are LEVELS: merged
  and blanked, never dropped, never rotated.

- **Finding 2 (one line)**: `prep_one_table()`'s `vars` now carries `row_vars` + `compacted` (which
  `tab_render_vars()` has returned since 14d). The Excel title reads **"race, marital by relig"**, was
  "levels by relig". Unblocks 14l.
- **New shared `tab_label_runs()`** (`R/tab-export-prep.R`): per column `list(show, span)`. Runs come
  from the VALUES, not the grouping (`new_group` marks the full group COMBINATION for >= 2 tab_vars, so
  the outer tab_var's run would be cut; values also survive an ungrouping dplyr chain). NA = a
  continuation (md's rule verbatim: a materialised p-value row belongs to the block above). Nested
  outer -> inner, which md's naive per-column scan was not.
- **`var_names = c("both","rows","cols","none")`** + `options("tabxplor.var_names")`, on
  `tab_kable`/`tab_md`/`tab_xl`/`tab_plot`/`tab_export` via `resolve_export_opts()` (the formal sits
  **after `caption`** — every call site passes the ones above it positionally). It never touches a
  LEVEL column's header (`marital` on a single-row_var table, `year` on a kept tab_var): that header
  identifies the column, costs no width, and is the mirror of the col-side rule (which removes the span
  row, never the level names). **Maintainer's call this session.** `tab_md(col_var_names)` →
  `deprecate_soft` onto it (FALSE drops the col side of whatever `var_names` asks, so they compose);
  its use site and the `md_render_one()` formal are deleted — the prep's blank `label` is the gate now.
- **The literal `"row_var"` header is always dropped** (a bug fix, not a setting): one blank in
  `tab_col_var_header()`, whose suffix loop only ever visited LABELLED columns. md / kableExtra / html
  / xl all follow.
- **md**: name once, **italic** (the maintainer's call — it mirrors the `*ROCK*` col_var row and marks
  a NAME in a column that otherwise holds level labels; tab_var cells stay plain), never bold. ⚠ The
  bold exclusion had to reach the WIDTH pass too (`bold_rows_of()`): `md_extra()` and the `+4` charge
  markup width per column, so charging `**` the body no longer writes over-pads the column and the
  pipes stop lining up. **The one golden line**: a tab_var's `**Ensemble**` label cell is now
  `Ensemble` — exactly "bold not needed for row_vars names (or tab_vars names)"; the LEVEL
  (`**Total Ensemble**`) still bolds, and the width is unchanged.
- **html**: the roadmap's "watch out" was **free** — `td_html` is a list of per-column vectors joined by
  `do.call(paste0, ...)`, so a continuation row just contributes `""`. `rowspan` per run; `tx-vname`
  only where `span > 1` (a rotated 1-row cell just makes the row tall).
- **Excel**: `xlb_merge()` per run (`text_rotation` was already a per-cell matrix in the style dedup
  key, only `colnames_rotation` drove it) + 90 degrees + a narrow (3.5) name column. The label repeats
  are **blanked in the written data**: Excel keeps only a merged range's top-left value, so a repeat
  below it is an invisible ghost the user finds again on unmerging.

**Two deviations from the roadmap's letter, both deliberate:**
- **point 5's `writing-mode: sideways-lr` → `vertical-rl` + `rotate(180deg)`.** MDN still flags
  `sideways-*` experimental with patchy support. The replacement reads the same way (bottom-to-top,
  matching Excel's 90 degrees) and is supported since Chrome 8 / Safari 5.1. Test-locked.
- **point 6's md dash separator row → deferred to 14m** (maintainer's call): reusing `dash_line` today
  renders as a literal dash row in html, and 14m makes every separator row invisible at once.

**Found and fixed in passing**: `%||%` at `tab_xl.R:196` and `tab_classes.R:1244` is **base R >= 4.4
only** — DESCRIPTION says `R (>= 4.1)` and neither `data.table` nor `vctrs` (the only `import()`s)
exports it, so both errored on R 4.1-4.3. The package knows (three other sites carry the *"use explicit
is.null()"* comment); these two missed it. Step 1 deleted the `tab_xl` one outright.

**Flagged for the maintainer** (not fixed here): `prep$labels` and `prep$range_totcol` are both **dead**
— nothing reads either, and each costs a `compute` token on every kable/plot export. 14j item 5 already
schedules `tab_export_labels()`; `range_totcol` is scheduled nowhere.

##### Original plan (historical intent)

**Why** — findings 1 and 2. Today, in all three backends, a compacted table renders a column with the
literal header `row_var` and its value on every row.

**What**

1. **Pass `row_vars` + `compacted` through** `prep_one_table()`'s `vars`
   ([tab-export-prep.R:313](R/tab-export-prep.R#L313)). `tab_render_vars()` already returns both. This
   alone fixes the Excel title's `"levels"` and unblocks 14l.
2. **New shared role `roles$label_cols`** in `prep_one_table()` — the leading factor columns whose value
   repeats down a block: the synthetic `row_var` column when `compacted`, the `tab_vars` when kept. One
   definition, four consumers. This is the "shared function, be consistent between export types" the
   review asks for.
3. **New shared arg `var_names = c("both","rows","cols","none")`** (+ `options("tabxplor.var_names")`),
   resolved in `resolve_export_opts()`, on `tab_kable`/`tab_md`/`tab_xl`/`tab_plot`/`tab_export`.
   `tab_md(col_var_names=)` ([tab_md.R:82](R/tab_md.R#L82)) → `lifecycle::deprecate_soft` onto it.
   `"cols"` drops the row_var label column entirely; `"rows"` drops the col_var spanning row.
   The literal `"row_var"` **header** is always dropped (an internal name, never informative) — that is a
   bug fix, not a `var_names` setting.
4. **Render the name once**: md extends the existing blanking loop from `tab_vars` to `label_cols` (and
   blanks its header); html gives the label column a `rowspan` over the block; Excel merges the block's
   cells (`xlb_merge`).
5. **Vertical label** (html + Excel), so a long name costs no horizontal space and wraps into several
   vertical lines: html `writing-mode: sideways-lr` on a new class in `tab_css(chrome = TRUE)`; Excel
   reuses the **existing** `create_cell_style(text_rotation=)` machinery
   ([tab_xl.R:677](R/tab_xl.R#L677) — today only driven by `colnames_rotation`). The maintainer verified
   the Excel 90° result is good.
6. **md**: no bold on the label cell (exclude `label_cols` from the bold-row markup — the *level* stays
   bold when it is the reference row, which is wanted); keep the col_var name italic; add the **dash
   separator row under the col_var name row**, reusing `dash_line` from Step 12
   ([tab_md.R:490-511](R/tab_md.R#L490)).

**Watch out** — html `rowspan` breaks the engine's column-wise `paste0` assembly
([tab-render-html.R:319-359](R/tab-render-html.R#L319)): the label column must be built separately and
its repeat rows omitted. A 1-row block must fall back to horizontal text (a rotated cell in a 1-row block
is clipped in Excel and forces a tall row).

**Verify** — `test-export-prep.R` (`roles$label_cols` for compacted / tab_vars / plain; `vars$row_vars`
present); `test-tab_md.R` (name once, header blank, not bold, separator row); `test-render-html.R`
(rowspan, no repeat); `test-tab_xl.R` (merge + rotation); `test-export.R` (`var_names` on all four
exporters). gss_cat only.

---

#### Phase 14j — the html engine, pass 2 (borders + compactness) (DONE — 2026-07-17)

Both blocking defects fixed, and both had been **misdiagnosed in the records**. Full suite **FAIL 0 |
PASS 3046**; **no `_golden/*.rds` and no `_color_golden/*.rds` moved**. Browser sample:
`dev/review_manual/phase14j_html_engine.html`. Full record + the corrected history: decisions **§40**.

- **THE BORDER BUG WAS NEVER FIXED — 14e announced it, `NEWS.md` shipped the claim, and nothing tested
  it.** `.tx-br{border-right:1px solid}` is a SHORTHAND: it resets `border-right-color` to
  `currentColor` = the cell's palette hex, and at (0,2,0) it out-specifies `td{border-color:…}` (0,1,1).
  14e moved the geometry off inline styles, which removed the INLINE half only — a class still
  out-specifies the colour rule. The comment beside the code stated the mechanism correctly and drew the
  wrong conclusion. **Fix**: no border shorthand anywhere — `border-*-style` + `border-*-width` only, so
  the ONE `border-color` rule is the only thing that names a border colour. **Locked two ways**, since
  either alone missed it: `expect_no_match(css, "border-(top|right|bottom|left):")` per theme, AND a
  **multi-col_var** fixture asserting a `<td>` carries both a border class and a colour slot class (a
  single-col_var fixture never produces one — which is why two phases of tests saw nothing). Five stale
  records corrected: NEWS.md, CLAUDE.md ×2, architecture, decisions §38, + the code comments.
- **THE COMPACTNESS CAUSE WAS THE LEGEND, NOT THE MIN-WIDTHS.** Measured: the legend in
  `<tfoot><td colspan="7">` is **327 chars on one line** vs a widest data cell of 23, so IT decided the
  table's max-content; a table is `min(max-content, available)` wide, so it took the whole pane and auto
  layout spread the slack over every column ("a tvhours cell half numbers half blank"; pass-3's
  "genuinely occupy all horizontal space"). The 14e sample was already the experiment — its Table 1 has
  a legend and was called not compact, Table 2 has none and was called compact. Every pass-3 full-width
  example has `color = TRUE`, which is also the "inconsistent". **Fix** (maintainer's pick): keep the
  `<tfoot>`, wrap in `<div class="tx-foot">` + `width:0;min-width:100%` — `width:0` is definite so the
  cell contributes 0 to max-content; `min-width:100%` refills it once the table is sized by its data.
  The two `min-width`s are deleted too (the browser already content-sizes every column).
- **No `col_width` argument** (maintainer): `.tx-rv`/`.tx-tot`/`.tx-num` stay emitted, deliberately
  UNSTYLED — `.tx-rv{min-width:10em}` in the user's own CSS is the escape hatch, documented in a new
  `?tab_css` "Restyling a table" section. That is what 14e's no-inline-styles contract buys; a
  per-COLUMN width could not be a class and would break 13d's table-independent stylesheet.
- **`inst/tab.css` KEPT** (maintainer; the roadmap's "dead" holds only for the DEFAULT engine — it still
  styles `engine = "kableExtra"`). Only `.popover` ported to `tab_css(chrome = TRUE)`, **geometry only**
  (`max-width:none` + padding + nowrap; `.popover-body` BS4/5 + `.popover-content` BS3): bootstrap moves
  popovers to `<body>`, so the selector is as unscopable as `.tooltip-inner` — "one line, not 276px" is
  what every bootstrap popover wants, but tab.css's white-on-black is our taste and would repaint the
  HOST page's popovers. Unstyled, a popover inherits the host's theme. The html engine's popovers had
  never been styled at all.
- **`mean (sd)` header**: a numeric col_var's column is named after the variable → the name was said
  twice under its own span (three times in Excel, which splits a `_sd` sibling). The level header now
  names the STATISTIC: `mean (sd)` text / `mean`+`sd` Excel / `mean` when no sd shows (`ci = "cell"`),
  via `format()`'s OWN `disp_mean_sd` predicate so header and cells cannot drift. The `var_names`
  col-side drop MOVED into `tab_col_var_header(name_cols=)`, because it is one rule: *a level header may
  name the statistic only while the span names the variable*. Blanking the span afterwards (14i) left
  `var_names = "none"` + Excel headed `mean` with the variable named NOWHERE — latent bug, fixed. Both
  drops still live in the prep, so 14i's "no backend knows the argument exists" holds.
- **`tab_export_labels()` DELETED** + the `labels` slot (render model = `list(tables, meta)`): it walked
  every column of every table on 100% of exports and nothing read the result — `NULL` in practice anyway,
  the source `label` not surviving `tab()`. **`kable_tabxplor_style()`** soft-deprecated (exported, zero
  callers/tests, regex role detection hardcoded to "Total"/"Ensemble") + its latent `if (subtext != "")`
  length>1 error fixed. Cleanups: the duplicate `tx-bb` on the last row (`radd` appends, it is not a set
  union), `<tr class="">` → bare `<tr>`, the stale "kableExtra is the DEFAULT" header/doc/fallbacks.
- **NOT changed, deliberately**: padding (already `3px 4px`; the pass-2 padding complaint was the
  thousands separator, fixed in 14h) and hover (already kableExtra's `#FFFCE5`).

**Flagged for the maintainer**: `man/tab_css.Rd`'s "Two workflows" section ships raw markdown
(`**bold**`) into the help page, and `document()` emits 5 "could not resolve link" warnings whose topics
(`1`, `data-bs-theme=light`, …) are exactly the bracketed tokens of `tab_css(theme = "auto")`'s OUTPUT —
roxygen appears to EVALUATE the ```` ```{r, results="asis"} ```` chunk inside `\preformatted{}` at
document() time. **Pre-existing since 13d, reproduces at HEAD** (verified on a clean HEAD checkout).

---

#### Phase 14k — `theme = "auto"` resolution + the Positron Viewer (DONE — 2026-07-17)

Both Viewer defects fixed. Full suite **FAIL 0 | WARN 0 | PASS 3090**; `document()` clean (0 warnings,
was 89); **NO golden regeneration of any kind** — not one `_golden`/`_color_golden`/`_snaps` file moved.
Browser sample: `dev/review_manual/phase14k_viewer_page.html`. Full record: decisions **§41**.

- **THE SPLIT**: `"auto"` = *follow the reader — resolved by whoever can actually know*. A file or a
  knit keeps the 4-layer cascade (the browser is right there). An interactive Viewer print resolves in
  **R**, because the Viewer is an Electron webview whose `@media (prefers-color-scheme)` reports the
  **OS**, not Positron's theme — finding 7. `knit_print` is deliberately NOT overridden (dispatch walks
  the class vector to `knit_print.kableExtra`), so a Quarto page is never repainted.
- **THE ONE RULE**: *tabxplor paints a page only when tabxplor's own stylesheet ships with the table* —
  the 13d/14j legend discriminator. `engine == "html" && nzchar(css)` closes three holes at once, each
  of which would have made a table UNREADABLE, not merely ugly: `css = FALSE` (no stylesheet reaches the
  Viewer → a dark pane around an unstyled black-on-white table), the kableExtra engine (its
  `kable_material_dark` paints `#363640`, two-tone on our `#222222`; its degrade returns a bare `kbl()`),
  and it leaves the html degrade needing no guard (`render_html_degrade()` emits `class="tabxplor-tab"`).
- **No new mechanism**: `<div data-theme="dark">` makes the print page an explicit host toggle, so
  cascade layers 3/4 (0,2,x) beat the `@media` layer (0,1,x) both ways. Emitted only under `"auto"` —
  its absence proves the detector cannot leak into an explicit theme. No `!important` either:
  `save_html()` puts its `body{background-color:white}` + bootstrap in `<head>`, ours rides in the body.
- **`tx_page_style(theme)`** (R/tab-css.R) = the chrome of a page WE build; exactly two callers —
  `print.tabxplor_kable()` (passes a resolved theme) and `tab_html_string(standalone=TRUE)` (passes the
  intent, so `"auto"` keeps the `@media` cascade: that file is opened elsewhere).
  **`tx_kable_page(html, theme, detected = tx_detect_theme())`** (R/tab-render-html.R) = the pure seam;
  the probe is a DEFAULT ARG (the `tab-theme-detect.R` idiom), which is the only way to test this at all
  — testthat is never `interactive()`, so the gated-ON branch is unreachable from the suite.
- **Amendments**: item 2 (option default → auto) **reversed** — see settled decision 3 above. Item 4
  (dark tooltips) **skipped**, keeping 14j's geometry-only rule; the look, if it ever lands, is settled:
  both match the table (`#222222`/`#CECDC3`/1px `#707070`), in `tx_page_style()` only. Item 5 (no
  `vscode-*` hooks) **confirmed and recorded** beside `tx_dark_hooks`: the Viewer is a cross-origin
  webview iframe, so those hooks could never fire. The roadmap's OS-toggle live-check is **superseded**:
  the editor now wins by design, because the editor is the pane around the table.
- **Fixed in passing (§40's flag, pre-existing since 13d, verified on a clean HEAD clone)**: roxygen2
  (>= 7.1) EVALUATES a ` ```{r} ` chunk written in markdown, and `?tab_css`'s "Two workflows" section had
  one inside a raw-Rd `\preformatted{}` purely to SHOW it — so `document()` ran `tab_css()`, pasted the
  whole stylesheet into the help page, emitted **89** link warnings (one per bracketed CSS token) and
  leaked literal `**bold**`. Fixed with a four-backtick fence and no `{r}` info string. **89 → 0.**

---

#### Phase 14l — Excel, pass 2 (DONE — 2026-07-17)

Five items; full suite **FAIL 0 | WARN 0 | PASS 3134**; `document()` clean; **zero golden/snapshot churn
of any kind** (the acceptance gate — default `color_type` was already `"text"` everywhere, bg_legend is
legend-only). Full record: decisions **§42**. Two findings were PROVEN not guessed, and one contradicted
the roadmap plan.

- **Fonts** — the bug was PROVEN by unzipping `Excel_test.xlsx`: numbers were named `DejaVu Sans` yet
  drawn Condensed because `openxlsx2::create_font()` defaults `scheme = "minor"` (= "the theme's body
  font"), so Excel resolved from the theme (Condensed, written by `xlb_base_font`) and ignored the name.
  Fix = `scheme = ""` in the ONE `create_font()` call (`xl_style_registrar$font_id`). Fonts exposed as
  `options(tabxplor.xl_font_text / xl_font_num)`. Did NOT flip the base font (would widen every column —
  Excel measures width in the base-font digit). Honest limit: xlsx has no fallback list; the option is
  the escape hatch. One `scheme` survives (font 0, openxlsx2's base font — correct).
- **Title** — dependent-first, decided by the fmt `type` (`tab_title_rows_first()`: flip only when every
  directional col is `"col"`, so a mean/coef never votes); `max` 3→2. `tab_get_titles()`'s unused first
  param carried the table. `tab_reg` still mis-titles (no recorded `vars`; flagged, out of scope).
- **Legend chroma** — measurement CONTRADICTED the plan: APCA Lc is lightness-driven, so chroma alone
  can't fix faintness, and k>2.5 at by=0.2 caps out the gamut and flattens the ladder. Shipped
  `darken_for_legend(by=0.30, chroma_boost=2)` (Lc 55–75, in-gamut, proportions exact); constants
  regenerated by the tool; preview `dev/make_legend_preview.R` → `phase14l_legend.html`.
- **sd width** — `roles$sd_cols` (ONE definition, ungated by `var_names`), `tab_xl` width
  `max(5, colwidth*0.6)`.
- **`color_type` deprecated + inert** (~79 mentions): option + 7 public args + ~9 internal formals + 4
  branches → text family. Fixed the live `tab_xl` vs `tab_export` option inconsistency. `deprecate_warn`
  (not `_soft`) for the option (reaches indirect callers, dedups). Kept `get_color_style(type=)`,
  `set_color_style(type=)` custom_palette routing, `fmt_get_color_code(type=)`. Plan cross-check caught
  the A4 forwards (would have flooded snapshots with spurious warnings — deleted all four) + the A5
  sentinel-sequencing on `tab_xl`.

---

#### Phase 14m-ii — Monospace number font + number font conditional on significance stars (DONE)

Full suite **FAIL 0 | WARN 0 | PASS 3159**; `document()` clean; **no `.rds` golden / no snapshot moved**.
Full record: decisions **§44 + §44b**. The number font is now **conditional on stars**: proportional
**DejaVu Sans** by default, a monospace **Cascadia Mono** only when the table SHOWS significance stars
(where a proportional `*` breaks alignment). Trigger = `roles$has_stars` (computed in the prep). html:
`tab_css()` ships both `.tx-num` (DejaVu) and `.tx-has-stars .tx-num` (Cascadia + a body-only 1.1em size
bump, row height unchanged) and `render_html_engine()` adds the `tx-has-stars` class to the `<table>`;
Excel: `tab_xl()` gains `font_num_stars`, chosen per table; tab_plot: whole-body mono only when starred.
Options: `tabxplor.tab_kable_num_font(_stars)`, `tabxplor.xl_font_num(_stars)`, `tabxplor.plot_num_font`.
**L4** needs no code (star-padding works in mono). **Item A** (`tab_md()` figure-space) and **L5** (footer
`gof`/`pvalue` cells drop out of star-padding) are unchanged, orthogonal to the font — `_snaps/golden.md`
moved 48 lines (proven the pure ASCII→U+2007 swap in `n`-rows); `_snaps/render-html.md` did NOT move (its
snapshots strip the `<style>`, and the plain snapshot tables carry no `tx-has-stars`).

**Flagged**: (1) **tab_plot** whole-body mono (ggpubr 1.0.0 has no per-column font) fires only on a
starred plot now; reverts with `plot_num_font = ""`. (2) **Numbering tangle** : let’s say this it `14m-ii`, and next is `14m-iii`


#### Phase 14m-iii — `tab_md()`, pass 2 — (DONE)

Full design + specificity math + the verified pandoc constraints: **`dev/tabxplor_1.4.0_decisions.md`
§43** (read first). Findings 9 (spacer/separator cells render as ugly `<td>`s / literal dashes) + 10 (the
host draws a black border under every row) are ONE problem: `.tabxplor-tab` was built for the **html
engine** (where `.tabxplor-tab` IS the `<table>` and WE draw every border via per-cell classes); in **md**
`.tabxplor-tab` is a `<div>` WRAPPING a pandoc `<table>` we cannot class, so the HOST (Quarto/Bootstrap)
draws the borders and our `border-color` rule recolours them black. Confirmed against the maintainer's
real `tab_md_test_2.htm`.


**Organizing lever**: `.tabxplor-tab table …` is an **md-only selector** (needs a `table` descendant of
the div) — it never matches the html engine (where `.tabxplor-tab` IS the table), so md gets its own
chrome with zero risk to the html engine, no positional/`nth-child` rule (13d table-independence holds).

**Maintainer decisions (this session)** — (1) **blank-row separators**, not `.sep` dash rows: a rule is a
fully-empty row collapsed to a 1px border in CSS, no pandoc marker token in the raw `.md` (supersedes the
maintainer's own dash-row drawing); (2) **GFM-clean when plain**: the pandoc scaffold (the `:::` div +
the border-taming CSS) is gated on `styled = do_color || isTRUE(css)`; a plain uncoloured `tab_md()` stays
**byte-identical**.

**The mechanism (styled path only), four rules scoped `.tabxplor-tab table`** (details + specificity §43):
1. **Tame host borders** (10): `.tabxplor-tab table td,th{border-width:0;}` — width-only (does NOT touch
   the §40 `border-color` contract; a 0-width border never renders). Specificity (0,1,2) beats Bootstrap's
   `.table>:not(caption)>*>*` (0,1,1); place it **before** `.tabxplor-tab thead th` (tie → source order)
   so the header underline survives.
2. **Block rules as collapsed blank rows** (9 + the col_var-name underline): inject a fully-empty row after
   the col_var-name row and at each `roles$new_group` boundary;
   `.tabxplor-tab table tbody tr:not(:has(td:not(:empty)))>*{border-top-style:solid;border-top-width:1px;padding:0;line-height:0;}`
   (border colour from the existing rule → theme-aware; pandoc keeps a fully-blank row as `<tr>` of
   `:empty` cells — verified).
3. **Collapse spacers** (9): `.tabxplor-tab table td:empty,th:empty{padding:0;}`.
4. **Decouple the `::: {.tabxplor-tab}` div from `<style>`**: emit the div whenever `styled` (not only
   `css = TRUE`), so the doc-level `tab_css()` workflow reaches the table; `<style>` still ships only with
   `css = TRUE`.

⚠ **DECISIVE 14m-i coupling (verified)**: a **figure-space** cell renders `<td> </td>` (NOT `:empty`); an
**ASCII / empty** cell renders `<td></td>` (`:empty`). So every `:empty` fix here REQUIRES blank/spacer
cells to stay ASCII-filled — former 14m-i's figure-space swap must be limited to padding **inside a value**
(thousands sep, `n=` alignment), never the pad of empty/spacer cells. **14v renamed 14m-i.**

**Cleanups**: the Step-12 dash-width arithmetic is MOOT (blank rows replace dash rows); remove the dead
`span` local ([tab_md.R ~L457](R/tab_md.R#L457)); `tab_md_css(tabs)` ignoring `tabs` is INTENTIONAL
(documented) — leave it.

**Verify** — a real pandoc/Bootstrap render (findings 9/10 gone; only provable in a Bootstrap host); a
fully-blank row survives as a `:has`-selected `<tr>`; the reset precedes `thead th`; the delimiter spacer
stays `-`; the gate (plain uncoloured = byte-identical, no `:::`; coloured carries the div even with
`css = FALSE`); no figure space in blank/spacer cells; a `levels="first"` + `tab_vars` snapshot.

**Flagged**: `:has()` (baseline since Dec 2023 → fine for 2026 Quarto; degrades to a blank gap row); the
plain path keeps dash separators (byte-clean) — unifying on blank rows there is a one-line gate.

---

#### Phase 14n — one Total row for several row_vars (DONE — 2026-07-17)

Both parts landed, DISPLAY-ONLY, in `R/tab_classes.R`; no fmt fields / attributes / public args; the core
`tab()` object keeps every Total row (`nrow(tab(...))` unchanged). Full suite **FAIL 0 | WARN 0 | PASS
3203**; `document()` clean; **no `.rds` golden and no `_snaps` moved** (both changes are display-only, and
no existing snapshot rendered a collapsing compacted table). Full record: decisions **§45**. Browser/Excel
samples: `dev/review_manual/phase14n_collapse.{html,xlsx}`.

- **Collapse (`tab_collapse_total_rows()`)** — the final step of `tab_materialize_extras()`, so it reaches
  the console + every export uniformly and all roles (`bold_rows` / `totblock_top`/`bottom` / `new_group` /
  references / tooltips) recompute on the collapsed table with ZERO per-backend code. Guard:
  `isTRUE(get_vars_attr()$compacted)` + `>= 2` Total rows — a single-row_var or a tab_vars table is never
  compacted, so both are untouched (a tab_vars table's per-subtable totals are real, not duplicates;
  `comp="all"` collapses via the same guard). Compares each block's whole **total BLOCK** (Total row +
  contiguous `"n"`/`"row_pct"` summary rows, gated to the same group; a `"pvalue"` row is block-specific
  and NOT swept in) "as displayed" via `format()` over EVERY fmt column — one canonical predicate for all
  backends. The BLOCK (not just the Total row) is what makes `pct="col"` correct: there the Total is always
  `"100%"` and the real base lives in the `n` row. Identical → drop all but the LAST block's total block
  (`tab[setdiff(seq_len(nrow), drop), ]`, global indices → class/attrs/grouping kept); different (only
  `na="drop"`) → keep all + `cli::cli_inform(.frequency="once")` naming `na="drop"`.
- **Per-block p-value rows (`tab_pvalue_lines()`)** — the `test` attr already carries a `row_var`
  discriminator, but the p-value rows were keyed on `tab_vars` only (empty for a compacted table), so two
  row_vars' tests collided into one col_var column → a `values not uniquely identified` list-col + a single
  mis-placed `row_var=NA` row. Fixed by keying on the table's GROUPING columns ∩ the test tibble (`row_var`
  for compacted, `tab_vars` otherwise → byte-identical there). **Also carries the `vars` attribute** through
  its `new_tab()` rebuild — a latent Phase 14d gap (the rebuild dropped `compacted`, which the collapse
  guard reads) that only this phase exposed. p-value rows SURVIVE the collapse: each variable keeps its own
  chi².

**Landmines / caveats (read before the next display-row change):**

- **`tab_pvalue_lines()`/`reg_footer_lines()` rebuild the tab with MORE rows via `new_tab()` and must
  re-list every table attribute by hand** (they cannot use `tab_restore()`, which preserves nrow). Phase
  14d added `vars` to `tab_attrs()` but NOT to these two rebuilds, so `compacted` was silently dropped
  after any materialised p-value row — invisible until a downstream reader (the collapse guard) needed it.
- **`add_n`/`add_pct`/`pvalue` summary rows are still detected by an English LABEL whitelist**
  (`{"n","row_pct","pvalue"}`, `R/tab-export-prep.R` `totblock_top/bottom`; the collapse reuses `"n"`/
  `"row_pct"`). The `row_pct` row's cells have display `"pct"` (indistinguishable from data by token), so a
  display-token sweep can't catch it — the real fix is a per-row role flag, still deferred.
- **The Phase 14a "one n row per sub-table" tests now assert the COLLAPSED count** under `na="keep"`; a
  non-collapsing `na="drop"` uneven fixture keeps the per-sub-table coverage. `test-render-html.R` /
  `test-tab_xl.R` "one-row block" fixtures moved off `levels=="Total"` (which the collapse drops) to a data
  level.
- **Not special-cased**: `add_n=FALSE` + `na="drop"` + `pct="row"` collapses silently if marginals round
  identical (follows the literal "identical as displayed" rule); a lone kept p-value row after a collapsed
  block still gets the `totblock` border box (cosmetic); transpose (14o) is unaffected (a transposed table
  has no `>= 2` Total ROWS → collapse no-ops; the flipped case is 14o's job).

##### Original plan (historical intent)

**Rule (settled)**: collapse when the per-block total rows are identical **as displayed** — same rendered
strings at the chosen digits. Otherwise keep them all and emit **one** message naming `na=` as the cause.
Rationale: the diffs and CI were computed per block beforehand and stay right, so a sub-tenth difference
behind the same printed "17%" is not a reason to show four identical-looking rows. Under `na="keep"` /
`"common_base"` / `"drop_all"` the totals are identical by construction; under `na="drop"` (the
maintainer's default) each row_var drops its own missing values, so they may genuinely differ.

**Design first (fresh session), thinking past the current implementation.** The framework was never
designed for several row_vars — `tab_compact()`'s synthetic `row_var`/`levels` columns are the scar, and
they are the root cause of findings 1, 2 and 8. Questions the design must answer **before** any code:

- **Where does the "as displayed" comparison live?** The rendered strings exist only in the prep — but
  Excel bypasses `format()` for values (it writes `get_num()` + a numFmt), so "as displayed" there means
  the numFmt-rounded value. One shared predicate for all four backends, or the rule silently diverges.
- **Display-only or build-time?** Display-only matches the 10i-B direction (add_n and p-value rows are
  already materialised by `tab_materialize_extras()`) and keeps the object honest: each block keeps its own
  reference row.
- The kept row is the **last** block's total, but the other blocks' `refrow` fields still point at their
  own (now hidden) rows. What then happens to bold / `tx-b`, the `totblock_top`/`bottom` borders, and the
  tooltips' `"ref"` marker?
- **tab_vars must keep their per-sub-table totals** (they are not duplicates — the review says so
  explicitly). And `comp = "all"`?
- **Do this BEFORE 14o**: one Total row → after the flip, one Total column, which is exactly what kills the
  `Total_DIPLOM` names the review saw.

**Verify** — `test-display-extras.R`: a gss_cat multi-row_var table with `na="keep"` collapses; a fixture
with genuinely different bases under `na="drop"` does not, and messages once; a tab_vars table is
unaffected; the collapse is display-only (`nrow(tab(...))` unchanged).

---

#### Phase 14o — transpose at the render level

**Why** — finding 8. `tab_transpose()` cannot be repaired at the object level; the review's own diagnosis
("colours must be calculated first from the not-transposed vctrs fields, then the transposition done not on
vctrs fields") is exactly right, and `Total_DIPLOM` is the tell.

**Design first (fresh session).** The flip belongs on the **render model**, where a cell is a string + slots
- roles and no per-column attribute is needed. Points to settle before code:

- `prep_one_table()` is per-**column** today (`ann` = a list per fmt column,
  [tab-export-prep.R:311-328](R/tab-export-prep.R#L311)). Transposing needs a per-**cell** matrix (text,
  text_slot, bg_slot, tooltip, bold, primary_nchar) + row/column role vectors. Decide: transpose a matrix
  built inside prep, or restructure `ann` into matrices for every backend.
- **Alignment**: `format()` pads per original column, and an original column becomes a transposed ROW.
  The composite inner-token alignment (`100% (n=  849)`) stays correct along that row — which is right,
  since a transposed column mixes variables. The **whole-cell** width must then be re-padded per transposed
  column.
- **Label columns**: the transposed table needs the (col_var, levels) pair mirroring (row_var, levels) —
  the review's "current first column name is CONCERTS, should be levels and second". Reuse 14i's `label_cols`.
- **Extras order**: `n` right after Total, numeric variables after both.
- `tab_transpose()` → `lifecycle::deprecate_soft` (settled). Re-point `test-transpose.R` (16 tests) at the
  render-level flip. Fix the stale "materialise → transpose" comments
  ([tab-export-prep.R:409-410](R/tab-export-prep.R#L409), [tab_xl.R:161-163](R/tab_xl.R#L161)) — 14d already
  reversed the order.

**Verify** — the pass-1 rule: `tab(pct="row") |> tab_export(transpose=TRUE)` renders like `tab(pct="col")`
for the 1×1 case; colours match the untransposed table cell-for-cell (the regression test that would have
caught finding 8); a mixed factor+numeric multi-row_var table transposes with no `Total_<var>` name and no
spurious numeric colour.

---

#### Phase 14 pass-3 roadmap Context (Phases 14p–14u)

`dev/review_manual/tab_manual_review_pass_3.R` is the maintainer's third hands-on review of tabxplor
1.4.0 on real survey data (`pc18` / `ct13_reg`) plus `gss_cat`. Its `#` comments are the spec. Phases
14a–14l are committed; 14m–14o are planned-but-unbuilt (design-first). This plan turns pass 3 into new
phases **14p–14u** (the maintainer pastes them into the CLAUDE.md roadmap; each phase = a fresh Claude
Code session; design-first phases start with a design task, not code).

Every defect was **reproduced and root-caused during planning** (three parallel Explore agents over the
color engine, `R/tab_reg.R`, and the tooltip/footer/`fct_recode_helper` paths). Several root causes were
new and change the shape of the fix. Tests must use `gss_cat`/`gss_cat`-derived data only — never `pc18`
or `ct13_reg` (confidential).

**Two mid-planning corrections from the maintainer (higher priority than the file's own items):**
- **A ≤1.3.1-breaking regression** not in the pass-3 file: `tab(relig)` and `tab(relig, pct="col")` — a
  single variable, no col_var — lost the `n` count column that 1.3.1 always showed; and the internal
  placeholder (`no_col_var`, sometimes the `Total` special name) is rendered as a col_var NAME (noise).
  "In the current state they would badly break past code from ≤1.3.1." → **Phase 14p** (elevated,
  do first). Same no-col_var `tab_plain(one_var)` shape as the `fct_recode_helper` bug.
- **The AME NA bug (Item E) is caused by ORDERED-FACTOR predictors, not by level names** — the maintainer
  verified it is not the `" - "` in the labels. `rincome` is `as.ordered()`. Fix: treat ordered factors
  as ordinary (unordered) factors in *predictors* (the `" - "` split found by the agent is a real but
  secondary latent fragility). → folded into **Phase 14r**.

**Settled with the maintainer this session (AskUserQuestion):**
1. **Empirical placement** — auto: **explicit columns when few** (binomial-coefficient, gaussian,
   poisson), **tooltip-only when many** (AME, multinomial). Statistically-adapted crude quantity per
   family.
2. **Number font** — make **DejaVu Sans Mono (monospace fallbacks) the default font for every
   number/fmt cell in every font-bearing export** (html engine, Excel, `tab_plot`), *always* (not only
   when stars are present). This is simpler and solider than the inline-block trick and dissolves the
   `*`-width problem: in a monospace font digits, `*`, `(`, `)`, `%`, space are all equal-width, so
   padding "just works". **md** keeps no font of its own → pad with figure space. **Text** (row labels,
   headers) stays DejaVu Sans Condensed — **except** Excel fmt-cells-shaped-as-text (ci="cell"/OR text),
   which get mono too (they carry stars). Revertible via options; the maintainer will visually review.

---

##### Root-cause table (for the implementing sessions)

| Item    | Symptom                                                                                                    | Verified root cause                                                                                                                                                                                                                                                                                                                                                   | File:line                                                      |
|---------|------------------------------------------------------------------------------------------------------------|-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|----------------------------------------------------------------|
| REG     | `tab(relig)` / `tab(relig, pct="col")` lost the `n` count column (≤1.3.1-breaking)                         | `tab_plain()`'s no-col_var block produces the `n` count column, but it does not survive the `tab_build`/assemble/10i-B pipeline to `tab()`'s output                                                                                                                                                                                                                   | `R/tab.R:3576-3594`                                            |
| REG     | internal placeholder (`no_col_var`, `Total`) shown as a col_var NAME                                       | placeholder col_var names are not blanked in the col_var-header render                                                                                                                                                                                                                                                                                                | `R/tab.R:3487`, `R/tab-export-prep.R` (`tab_col_var_header`)   |
| C       | `fct_recode_helper(freq=TRUE)` errors `object 'pct' not found`                                             | `tab_plain(df, one_var, pct="col")` — same no-col_var shape; the single fmt column is named `"n"` (the injected `no_col_var` level); code refs bare `pct`/`n`                                                                                                                                                                                                         | `R/utils.R:282-304`                                            |
| B/J     | `grey_non_signif` legend says "Grey: not significantly different from the Total row" — statistically FALSE | Under `grey_non_signif` a cell is coloured only if significant **AND** effect ≥ first break, so an uncoloured cell may be significant-but-small (some carry stars). Only guarantee: **coloured ⇒ significant**                                                                                                                                                        | `R/fmt_class.R:3197-3203`                                      |
| D/J     | reg footer (GOF) + some reference cells render greyed/faint                                                | Greying paints every uncoloured non-`ref_alltot` cell grey (deliberate, to make coloured cells pop). `gof` cells and reg reference cells are NOT in the `ref_alltot` exclusion                                                                                                                                                                                        | `R/tab-export-prep.R:96-100`, `R/tab-render-html.R:337-338`    |
| L6      | footer tooltip shows nonsense (AIC "63 785" → `+6378526%`)                                                 | `gof` cell stores the stat in the `diff` field; the tooltip's `diff:` fragment fires (no `display`-kind gate)                                                                                                                                                                                                                                                         | `R/tab_classes.R:2182-2188`                                    |
| D       | reg tooltip `n:` is the whole-model N                                                                      | `n = rep(nobs, n_rows)` broadcast to every coefficient row                                                                                                                                                                                                                                                                                                            | `R/tab_reg.R:798,806,1023,1034`                                |
| E       | some significant AME cells are NA (`$20000 - 24999`, …)                                                    | **PRIMARY (maintainer-confirmed): the predictor is an ORDERED factor** → non-treatment contrasts → the marginaleffects AME does not key per-level to the skeleton → NA. SECONDARY (latent): `reg_marginal()` splits the contrast on the first `" - "` (`sub(" - .*$", "", contrast)`), truncating levels containing `" - "`. OR keys by `term`, unaffected either way | `R/tab_reg.R:959`, key `:999-1002`                             |
| G       | multinomial: borders drawn between a model's category columns                                              | each category column gets a DISTINCT `col_var` (its own per-category label)                                                                                                                                                                                                                                                                                           | `R/tab_reg.R:1059-1072`                                        |
| K       | vector-of-dependents + list-of-models errors                                                               | the two modes are mutually exclusive; guard forbids the combination                                                                                                                                                                                                                                                                                                   | `R/tab_reg.R:1797-1801`                                        |
| L1      | predictor row order not "complete model last"                                                              | `union_predictors = unique(flatten(models))` = first-appearance order; no complete-model concept                                                                                                                                                                                                                                                                      | `R/tab_reg.R:1877,1900`                                        |
| L2      | `compare="baseline"` warns "not nested or N differs"                                                       | nesting tests ONE direction only (`all(t_ref %in% t_full)`); and each model drops NA on its OWN vars → different N                                                                                                                                                                                                                                                    | guard `R/tab_reg.R:1247-1253`; drop `:631-632`                 |
| L3      | model name shown twice (col_var span + column header)                                                      | col_var span always drawn; no "column name == its col_var" collapse                                                                                                                                                                                                                                                                                                   | `R/tab-export-prep.R` (`tab_col_var_header`/`tab_header_runs`) |
| A/L4/L5 | stars/padding misalign in rendered html + Excel                                                            | `*` ≠ digit-width in proportional DejaVu Sans; padding uses digit-width figure space                                                                                                                                                                                                                                                                                  | `R/fmt_class.R:2235-2243`, `R/tab_xl.R:459-468`                |

---

#### Phase 14p — single-variable / no-col_var table correctness (ELEVATED — do first)

The ≤1.3.1-breaking regression the maintainer flagged mid-planning, plus the two other defects that share
the no-col_var `tab_plain(one_var)` shape (`fct_recode_helper` C, and the placeholder col_var noise).
**Reproduce against installed tabxplor 1.3.1 FIRST**, then fix. Regression-lock everything with tests —
the maintainer says these "would badly break past code from ≤1.3.1".

**Why + what**

1. **Restore the `n` count column for a single variable / no col_var** (`tab(relig)`, `tab(relig,
   pct="col")`). `tab_plain()`'s no-col_var block (`R/tab.R:3576-3594`) DOES build the `n` count column
   (renamed from the total; `set_type("n")`, `set_display("n")`), but it does not reach `tab()`'s output
   — the `tab_build`/`tab_assemble`/Phase-10i-B pipeline strips it (likely conflated with the display-only
   `add_n` `n` column that 10i-B removed). Root-cause where it is dropped and **restore it** so a one-way
   frequency table shows counts as in 1.3.1, WITHOUT undoing 10i-B for real crosstabs (the crosstab add_n
   `n` stays display-only; the no-col_var `n` is primary content and must survive). Decide the default
   shape to match 1.3.1 (levels + `n`, plus the pct column when a pct mode is set).
2. **Never render an internal placeholder as a col_var name** (`no_col_var`; sometimes the `Total`
   special name). Blank any col_var whose value is an internal placeholder in the col_var-header model
   (`tab_col_var_header()`/`tab_render_vars()`, `R/tab-export-prep.R`; note the existing partial guard at
   `R/tab.R:3487`). This is the col-var twin of the 14i variable-name blanking and overlaps L3/14s — do the
   general "placeholder col_var names are noise → blank" rule here since `tab(relig)` is where it bites.
3. **`fct_recode_helper(freq=TRUE)`** (Item C, `R/utils.R:282-304`): rides on the fixed shape. Stop
   referencing bare `pct`/`n` columns; use the single fmt column (named `"n"`) + accessors
   `get_pct(col)`/`get_n(col)` (or `format(col)` / `format(get_n(col))`). `is_totrow`/`get_pct`/`get_n`
   are vectorised over an fmt column (`R/fmt_class.R:518,1329,1314`). If step 1 restores a real `n` count
   column for `tab_plain(one_var)`, prefer reading that.

**Verify** — reproduce `tab(relig)` / `tab(relig, pct="col")` and compare to installed 1.3.1: the `n`
count column is present; no header shows `no_col_var`/`Total` as a variable name.
`fct_recode_helper(gss_cat, all_of("rincome"))` runs without error. New/expanded tests:
`test-tab.R` (single-variable frequency table has an `n` column across pct modes + weighted; placeholder
never appears as a col_var name), `test-fct-recode-helper.R` (freq TRUE/FALSE on 1 var and several
`gss_cat` factors — exported, currently untested).

##### Done (2026-07-18)

All three landed. Full suite **FAIL 0 | WARN 0 | SKIP 4 | PASS 3212**; **no `.rds` golden and no
snapshot moved** (no existing snapshot rendered a bare `tab(one_var)`). Reproduced against **real CRAN
1.3.1** (installed in a temp lib — the machine's `1.3.1.9000` already carried the regression, so it was
useless as a reference). New `test-fct-recode-helper.R` (10); two new blocks in `test-tab.R`.

- **The `n` column was NOT dropped at build — it survives into `names(tab(relig))`.** The regression is
  at DISPLAY: `render_extras$add_n = TRUE` was set unconditionally, so `tab_materialize_extras()` ran
  `tab_add_n_pct()` + `tab_fold_addn_incell()`, whose first line returns `select(-any_of("n"))` when
  there is no `type == "row"` Total column to fold into — silently deleting the real frequency column.
  Fix ([R/tab.R](R/tab.R) `tab_assemble_tables`): gate the intent on a real col_var —
  `has_real_colvar = any(fmt & get_col_var(tab) != "no_col_var")`; a no-col_var table's `n`/`pct`/`wn`
  are primary content, not display extras, so `add_n`/`add_pct` are forced OFF (they stay ON for a
  numeric col_var, unchanged). This also means `add_n = FALSE` no longer drops the frequency `n` (it
  never should have — the `n` is not the add_n extra). The roadmap's "the fmt column is named `n`"
  diagnosis was wrong: the columns ARE `pct`/`n`, the object was fine, only the fold was wrong.
- **`no_col_var` sentinel** ([R/tab-export-prep.R](R/tab-export-prep.R)): added to the `real_col_vars`
  exclusion list (beside `all_col_vars`/`""`/`no`), so `tab_col_var_header()` never marks those columns
  `is_level` → no span label. One line; every backend (md/kable/html/xl) follows. (The "Total special
  name" case the review also named is already handled — a total column is excluded via `!totc`.)
- **`fct_recode_helper(freq = TRUE)`** ([R/utils.R](R/utils.R)): the real cause was **unqualified
  `filter`** — NOT imported, so it resolved to `stats::filter()`, which evaluated `!is_totrow(pct)`
  outside the data mask → "object 'pct' not found". Fixed by fully qualifying the non-base calls
  (`dplyr::filter`, `stringr::str_pad`/`str_length`, per the CLAUDE.md explicit-call rule); the columns
  `pct`/`n` were always there, so no accessor rewrite was needed.

---

#### Phase 14q — tab_reg readability: greying, footer, legend semantics

Groups Items **D (footer/ref greying)**, **J (ref greying + "why *** greyed" explanation)**,
**B (grey_non_signif legend)**, **I (ordinal Brant footer row)**. Colour/prep/footer only — NO tooltip
changes (those are 14r), so the two phases don't both touch the tooltip builder.

**Why + what**

1. **gof + reference cells escape greying.** Greying lives in `R/tab-export-prep.R:96-100`
   (`font = case_when(coloured ~ hex, ref_alltot ~ normal, TRUE ~ grey)`) and `R/tab-render-html.R:337`
   (`g1`/`g2` class). Add `display_primary(get_display(col)) %in% c("gof","blank")` to the "render normal"
   branch at BOTH sites so footer stats read black/bold. Reproduce and fix the reg **reference** cell
   greying (the "Emp. %" reference and the gaussian/OR reference show grey, must be black): confirm
   whether the reg reference row lands in `ref_alltot` (`get_reference(col,"all_totals")`) — the empirical
   `"Emp. %"` column is built with `ref="tot"` and may not set `in_refrow`, so it misses the exclusion.
   Fix by flagging the reg reference row (`as_refrow`/`in_refrow`) or extending the exclusion, whichever is
   cleaner. The maintainer's suggested "treat footer as total rows" is the same idea — but prefer the
   explicit `display`/`is-reference` gate over faking a total row (which would perturb other masks).
   Also, like in tab(), **reference row must by in bold**, including the text columns live "levels".
2. **grey_non_signif legend is false** (`R/fmt_class.R:3197-3203`). Reword the grey note so it is
   statistically true: the only guarantee is **coloured ⇒ significantly different from ‹ref› (‹method›)**;
   an uncoloured cell is *either* not significant *or* too small an effect to reach the first colour
   threshold. Propose EN wording: *"Coloured: significantly different from ‹the Total row› (‹Newcombe…›)
   and beyond the first colour threshold. Uncoloured: either not significant, or a difference too small
   to colour."* + FR (`po/R-fr.po`, recompile `.mo`). Do the terse console tag too (`:3133-3137`). Leave
   `guaranteed_effect` wording as-is (it is defensible) unless the same session confirms it also misreads.
   This *also answers Item J* ("why `***` but greyed") — significance ≠ colour is now stated; add a short
   sentence to `?tab`/`?color` (or the color-mode skill) so it is documented, not just legended.
3. **Brant PO p-value in the ordinal footer** (Item I). `reg_ordinal_diagnostic()`
   (`R/tab_reg.R:517-546`) already computes `bt["Omnibus","probability"]` but only warns and returns
   `invisible()`. Return the omnibus p; add a `brant_po = list(label = "Brant PO test", kind = "pvalue")`
   spec entry in `reg_footer_spec()` (`R/tab_classes.R:405-427`) + the `valid` list in
   `reg_footer_stats()` (`R/tab_reg.R:1217-1218`); emit a `brant_po` row from `reg_glance()`'s polr branch
   (or thread through `reg_gof_tibble()`). The `pvalue` kind renders in both `print_reg_footer` and
   `reg_footer_lines` with no extra work. Weighted (`svyolr`) → Brant degraded → skip the row.

**Verify** — a binomial/gaussian/OR reg table: footer stats and reference cells render black (not grey)
in console/kable/Excel; a significant-but-small cell stays uncoloured (intended) and the legend now says
so. Ordinal table shows a "Brant PO test p=…" footer line. Follow `/color-mode` for the legend edit.
Tests: `test-tab_reg-footer.R` (Brant row present for ordinal; reg reference + gof not greyed — assert on
`tab_export_prep()` roles / the render model, not a raw hex), `test-color-legend.R` (grey_non_signif
wording; add a FR case if the harness allows — see the CI gettext note in the roadmap).

##### Done (2026-07-18)

All three items landed. Full suite **FAIL 0 | WARN 0 | SKIP 4 | PASS 3230**; `document()` clean; **NO
golden and NO snapshot moved** (the reg tables + the grey_non_signif legend are not snapshotted; the
legend wording is asserted directly). Browser sample: `dev/review_manual/phase14q_reg_readability.html`.

- **Greying (Items D/J).** The root cause was NOT that gof cells miss the exclusion generically — it was
  a MISMATCH: the empirical `Emp. %` column carries `ref_type = "tot"` yet marks its reference CATEGORY
  via `in_refrow`, so `get_reference("all_totals")` (which returns the total ROW under ref = "tot")
  returned empty and greyed the reference cells. Introduced ONE shared "black anchor" concept:
  + `fmt_col_ann()` ([R/tab-export-prep.R](R/tab-export-prep.R)) now computes `keep_black = ref_alltot |
    is_refrow(col)` and drives `font`/`bold` off it (returns the mask too). For a crosstab `is_refrow`
    is a subset of `ref_alltot`, so byte-identical there — only reg reference columns change.
  + The GOF FOOTER rows are un-greyed at the TABLE level in `prep_one_table()`: a footer row is one where
    EVERY fmt cell is a footer stat (display `gof`/`pvalue`/`blank`). A crosstab chi2 pvalue row is NOT
    (its other cells stay `pct`), so this never touches a crosstab and needs no reg gate — and it catches
    the `pvalue` footer rows (LR vs null) that a per-cell `%in% c("gof","blank")` rule would have missed.
    The whole footer row goes black + bold (font + keep_black + `bold_rows` union so LABELS bold too).
  + The html engine ([R/tab-render-html.R](R/tab-render-html.R)) reads `a$keep_black` instead of
    `a$ref_alltot`; the console `pillar_shaft` greying ([R/fmt_class.R](R/fmt_class.R)) ORs `is_refrow(x)`
    into its `totals` exempt set. Deliberately kept `ann$ref_alltot` semantic (feeds the reference
    intercept + `tab_bold_rows`); the styling decision is the separate `keep_black`.
- **Legend (Item B).** The `grey_non_signif` prose note was statistically false. Rewrote to state the true
  guarantee — *"Coloured: significantly different from ‹ref› (‹method›), by at least the first colour
  threshold. Uncoloured: either not significant, or too small a difference to colour."* — EN + FR
  (`po/R-fr.po` + `.mo` recompiled), and documented under `color_signif` in `?tab`. The terse console tag
  (`[significant only]`) was left — it already describes the colouring rule correctly (coloured ⇒
  significant). `guaranteed_effect` left as-is (defensible). This also answers Item J's `***`-but-grey.
- **Brant (Item I).** `reg_ordinal_diagnostic()` now RETURNS the omnibus p (still warns); `reg_fit_ordinal`
  stashes it as `attr(fit, "brant_po")` (computed once, at fit time); `reg_glance()` emits a `brant_po`
  row for unweighted ordinal; `reg_footer_spec()` gains `brant_po = list(label = "Brant PO test", kind =
  "pvalue")` + the default/valid stats lists. Weighted (svyolr) has no Brant fit → attr absent → skipped.

**Landmine for the next reg session**: the footer-row detection ("all fmt cells are gof/pvalue/blank")
is the robust, language-independent alternative to the `reg_footer_labels()` English-label match that
`tot_block` still uses — a real per-row role flag would retire both, but that is deferred.

---

#### Phase 14r — tab_reg tooltips + the AME NA bug

Groups **L6 (remove footer tooltips)**, **D (row-level n)**, **E (OR always in tooltip)**, **E (AME NA
bug)**. Tooltip builder + `reg_marginal`. Do this **before 14t** (empirical builds on a correct AME).

**Why + what**

1. **AME NA bug — PRIMARY cause: ordered-factor predictors** (maintainer-confirmed). When a predictor is
   an ordered factor (e.g. `as.ordered(rincome)`), the model uses non-treatment (polynomial) contrasts,
   so the marginaleffects AME does not key per-level to the skeleton → NA (while the OR still shows). Fix:
   **treat ordered factors as ordinary (unordered) factors in PREDICTORS**, coerced uniformly and early
   (in `reg_prep`/`reg_apply_references`, before skeleton + fit + `reg_marginal`), so contrasts are
   treatment-style and OR/AME both key per-level. Only PREDICTORS are de-ordered; a `family="ordinal"`
   DEPENDENT stays ordered. **SECONDARY (latent hardening):** `reg_marginal()` (`R/tab_reg.R:959`) splits
   the contrast on the first `" - "` (`sub(" - .*$", "", ac$contrast)`), truncating an unordered level
   that itself contains `" - "`; key on marginaleffects' **structured columns** (or strip the *known*
   reference suffix) instead — same care for the `lnor` branch (`:958`, `[^)]+` breaks on a `)`). The join
   key is `:999-1002`. Add a `gss_cat` regression test with an **ordered-factor predictor** asserting the
   AME is non-NA where the OR is significant (and a secondary case with a `" - "` unordered level).
2. **Row-level n in the tooltip** (Item D). `reg_effect_column`/`reg_marginal_column` set
   `n = rep(nobs, n_rows)` (`R/tab_reg.R:798,806,1023,1034`) → every row shows the whole-model N (already
   in the footer). Pass the **per-row level n** where it exists (e.g. the empirical/level count), else
   `NA_integer_`. `cond_n` (`R/tab_classes.R:2274`) then drops the fragment automatically where NA.
3. **OR always in the tooltip** (Item E). Even under `effect="ame"`, keep the model OR available in the
   tooltip. Store the coefficient OR in the column's `or` field at build time (display stays the AME);
   `cond_or` (`R/tab_classes.R:2258-2262`, `type %in% c("col","row") & !is.na(get_or)`) then surfaces it.
   General principle the maintainer states: any fmt field that helps interpret the model is a tooltip
   candidate — but keep it read-only in the tooltip, never displayed.
4. **No tooltips on footer/gof rows** (L6). Gate `tab_kable_print_tooltip()` (`R/tab_classes.R:2147`) so a
   cell with `display_primary(get_display(x)) %in% c("gof","blank")` returns `""` (kills the nonsense
   `diff: +6378526%` on AIC). Do it once at the top of the builder (both engines call it).

**Verify** — an AME reg table: no NA AME where the OR is significant; tooltip shows OR + a row-level n (or
none); a footer cell has an empty tooltip. Snapshot regen limited to `_snaps/render-html.md` (tooltip
text). Tests in `test-tab_reg-display.R`.

##### Done (2026-07-18)

All four landed. Full suite **FAIL 0 | WARN 0 | SKIP 4 | PASS 3238**; **NO golden and NO snapshot moved**
(the reg tables + tooltips are not snapshotted). Sample: `dev/review_manual/phase14r_ame_tooltip.html`.

- **The AME NA bug has TWO independent causes, not one.** Verified: `marginaleffects::avg_comparisons()`
  produces the SAME `"Level - Reference"` labels + estimates for an ordered AND an unordered fit, so the
  ordered factor does NOT break the AME. The NA cells were the `" - "` SPLIT: `sub(" - .*$", "", contrast)`
  truncated `"$20000 - 24999 - $1000 to 2999"` → `"$20000"` → no skeleton match → NA (exactly the levels
  the maintainer flagged). The ordered factor SEPARATELY breaks the COEFFICIENT path: glm/polr give
  polynomial terms (`x.L`/`x.Q`) that don't align → an all-NA OR column (the "remove ordered to not break
  the model" the maintainer did by hand in Pass 4). So both the roadmap's PRIMARY (de-order) and SECONDARY
  (robust split) are real and both needed:
  + de-order in `reg_fit` ([R/tab_reg.R](R/tab_reg.R)): `factor(fct_drop(as.factor(.)), ordered = FALSE)`
    (was `as.factor()`, which KEEPS the ordered class). Predictors only; the ordinal outcome stays ordered.
  + `reg_marginal()` strips the KNOWN prefix + reference suffix by `substr` instead of splitting on the
    first `" - "` / first `")"` — handles a level containing `" - "` or `")"`. ⚠ The lnor contrast is
    `ln(odds(<Level>) / odds(<Ref>))` with a DOUBLE closing paren; the suffix must include both (a test
    caught the off-by-one).
- **Row-level n (D)**: the model effect columns (`reg_column` OR/β, `reg_marginal_column` AME) set
  `n = rep(NA_integer_, n_rows)` — the whole-model N is in the footer, not a per-cell tooltip. (⚠ `n`
  drives `fmt()`'s recycle size, so it must be `rep(NA, n_rows)`, not a scalar.) The empirical columns
  keep their real per-LEVEL n (`emp$emp_n`), which is what the maintainer wanted surfaced.
- **OR in the AME tooltip (E)**: the binomial single-outcome AME column carries the coefficient OR
  (`exp(tidy$estimate)`, keyed to the skeleton by term) in its `or` field via a new `reg_marginal_column
  (or_tip=)` arg. Read-only — the AME display / colour never read `or` (colour goldens byte-identical), so
  `cond_or` surfaces `OR: 0.42` on hover with zero display/colour impact.
- **No footer tooltips (L6)**: one line at the end of `tab_kable_print_tooltip()` blanks any cell whose
  display is `gof`/`blank` (kills the `diff: +6378526%` on an AIC stored in the `diff` field).

---

#### Phase 14s — tab_reg multinomial: one col_var per model + drop redundant name row

Groups **G** and **L3**. Both concern the col_var header of reg tables. Byte-identical for crosstabs.

**Why + what**

1. **One col_var per multinomial model** (Item G). `reg_columns_multinom()` (`R/tab_reg.R:1059-1072`)
   passes each per-category label as the column's `col_var`, so every category column is a distinct
   col_var → borders between them. Pass a **shared model id** (e.g. `sp$dependent` or the model's label) as
   `col_var` while keeping the per-category `lab` as the visible column NAME. Result: a spanning header
   names the model once over all its category columns, and inter-category borders disappear (borders are
   drawn between different col_vars). Apply the same to the MNL AME / vs-rest columns
   (`reg_marginal_column(col_var=…)` at `R/tab_reg.R:1435,1456`). The GOF footer keys by the make.unique'd
   output label (`fit_first_col`), so changing `col_var` is display/border-only and footer-safe.
2. **Drop the redundant variable-name row** (L3). Rule (maintainer's): if EVERY fmt column's own name
   equals its `col_var`, silently drop the col_var spanning-name row. Implement in the 14i/14j col_var
   header model (`tab_col_var_header()`/`tab_header_runs()`, `R/tab-export-prep.R`) so it composes with the
   existing `var_names` arg and touches no backend. This covers the single-model reg table where the
   column is named after the dependent and the col_var is the same. With (1) it also means a multinomial
   model's shared-col_var header shows once (meaningful) rather than duplicating each column name.

**Verify** — `tab_reg(gss_cat, "marital", c("race","rincome"), family="multinomial")` renders one span per
model, no borders between category columns; a single-model OR table shows no duplicate name row. Tests in
`test-tab_reg.R` + a render assertion in `test-render-html.R`/`test-export-prep.R` (`tab_header_runs`
collapse). Confirm crosstab goldens unchanged (the rule fires only when name==col_var for ALL columns —
a crosstab has level names ≠ col_var).

##### Done (2026-07-18)

Both landed. Full suite **FAIL 0 | WARN 0 | SKIP 4 | PASS 3243**; **NO golden and NO snapshot moved**
(no reg table is snapshotted; crosstab headers are byte-identical). Sample:
`dev/review_manual/phase14s_mnl.html`.

- **G (one col_var per MNL model)**: the three MNL column builders (`reg_columns_multinom`, the MNL AME
  per-category, the MNL "vs rest") pass `sp$label` (the unique model id) as the `col_var` while keeping
  the per-category `lab` as the visible NAME. Borders are drawn at col_var TRANSITIONS (`new_col_var`),
  so a shared col_var removes the inter-category border (verified: `new_col_var` no longer lists the 2nd
  category column) and the model name spans the categories once. The GOF footer keys by the output LABEL
  (`fit_first_col`), so col_var is display/border-only — footer-safe.
- **L3 (drop the redundant name row)**: in `tab_col_var_header()` ([R/tab-export-prep.R](R/tab-export-prep.R)),
  after the level-header rewrites, blank the whole span `label` when `all(clean[level] == col_var[level])`.
  ⚠ Compare the CLEAN (displayed) header, NOT the raw column name: a numeric col_var has raw name ==
  col_var ("tvhours") but a clean header of "mean (sd)", so comparing raw names would have wrongly dropped
  its span (and lost the variable name). A crosstab (level "Black" != col_var "race") is never affected;
  a single-model reg ("Married: OR" == "Married: OR") drops the span, showing the name once.

---

#### Phase 14t — DESIGN-FIRST: the empirical (crude) framework across families/effects

Groups **F (rename `empirical_OR`→`empirical` + cross-family)**, **D (empirical relation)**,
**H (multinomial×AME empirical hack)**. **Start with a design + web-research task in a fresh session,
out of the box** — the statistical content must be sound/standard, and the placement uses the vctrs
fields (`/vctrs-field`). Do **after 14r** (correct AME + tooltip infra).

**Design step (first, before code):**
- **Statistical framework — what is the "empirical" analogue per family?** The rule (maintainer): the
  empirical value is the crude quantity that *is* the modelised quantity when there is a single predictor.
  Web-research + settle, per family/effect (write the result into `dev/tabxplor_1.4.0_decisions.md` §37):
  + binomial coefficient → crude OR + crude % per level, diff from ref (today's `empirical_OR`).
  + binomial AME → observed % per level (predicted-prob analogue) + empirical diff from ref.
  + gaussian → mean per level of the predictor + diff of means from ref.
  + poisson/IRR → crude rate + rate-ratio from ref.
  + multinomial → observed category % + empirical diff (per category).
  Confirm this is the standard "unadjusted vs adjusted" comparison (good practice), not a bespoke thing.
- **Placement (settled: auto columns-when-few / tooltip-when-many).** Binomial-coefficient, gaussian,
  poisson → explicit `"Emp. …"` columns (reuse `reg_empirical_columns`, `R/tab_reg.R:883-904`). AME and
  multinomial → **tooltip only** (a column per category × empirical would explode the layout). Design the
  **field hack** for the tooltip case: store the empirical pct/diff in fmt fields not otherwise displayed
  for that column type so the tooltip surfaces them WITHOUT disturbing `tab()`/reg display or other
  tooltips (the maintainer's explicit worry). Candidate: the `ratio` field (or a clearly-reserved reg
  slot) read only by a new tooltip fragment gated on a reg marker. Resolve with `/vctrs-field`; do NOT add
  a new fmt field if an unused one suffices.
- **Rename** `empirical_OR` → `empirical` (hard rename, no soft-deprecate — new in 1.4.0). It becomes
  family/effect-general; drop the "single binary logistic (coefficient)" guard, replacing it with
  per-family/per-effect dispatch (columns vs tooltip). `trials` stays; the empirical binomial base is the
  weighted 2×2 as today.

**Then implement** the designed framework + tests (`test-tab_reg.R`): empirical columns for binomial-coef/
gaussian/poisson (parity vs a hand crude computation), empirical tooltip for AME/multinomial (the field
carries the right value; `tab()` tooltips unaffected — assert a crosstab tooltip is byte-identical).

**Caveat to flag to the maintainer:** the multinomial×AME empirical-in-tooltip is a genuinely marginal
feature (a rarely-read crude-vs-adjusted check on a crowded table). If the field hack proves fragile,
make it opt-in or defer — surface this during the design step rather than forcing a hack.

##### Done (partial) + DESIGN (2026-07-18) — full design in `dev/tabxplor_1.4.0_decisions.md` §45

The tooltip field-hack IS fragile (proven, not guessed), so per the maintainer's own guidance the
fragile parts are DEFERRED with a written design; the solid, colour-safe core landed. Full suite
**FAIL 0 | WARN 0 | SKIP 4 | PASS 3246**; `document()` clean; **no golden / no snapshot moved**.

- **LANDED (solid)**: `empirical_OR` → **`empirical`** (rename; `tab_reg()` keeps `empirical_OR =
  lifecycle::deprecated()` warning-alias, the wrappers took the new name). The binomial crude `Emp. %`
  (coloured by crude risk-diff) + `Emp. OR` columns now show for BOTH `effect = "coefficient"` and
  `effect = "ame"` (widened from coefficient-only — answers the review's "base % + empirical diff" and
  un-blocks the `ame + empirical` error). Non-binomial / multinomial: a MESSAGE + ignore, not an abort.
- **DEFERRED (needs a maintainer visual/design call, §45)**: (1) gaussian/poisson explicit crude columns
  — the `Emp. mean` colour is under-specified (a `type="mean"` `color="diff"` column needs a reference
  variance the crude path lacks; options in §45). (2) the multinomial×AME crude-in-tooltip — a REAL
  field conflict: the tooltip reads `ratio`/`ctr`/`mean` for row/mean columns, so any stash makes a
  spurious "ratio:"/"contrib:" line. A clean fix needs a dedicated reg-only tooltip field (shared-builder
  cost) — the maintainer flagged this feature "marginal", so it stays deferred/opt-in.
- ⚠ **The roadmap's "§37" for this never existed** — the design is now §45.

---

#### Phase 14u — DESIGN-FIRST: tab_reg model-comparison structure

Groups **K (dependents × models → list of tabs)**, **L1 (complete-model ordering)**, **L2 (bidirectional
nesting + `na="drop_all"`)**. **Start with a short design task** — the three interact (a per-dependent
list, each a model comparison, on a shared complete-case population).

**Design + what**

1. **Vector-of-dependents × list-of-models → a list of tabs** (K). Today the two modes are exclusive
   (guard `R/tab_reg.R:1797-1801`); `reg_build()` already handles a multi-spec comparison. Relax the guard
   and, when BOTH are given, loop dependents on the outside — each iteration builds `specs` from the model
   list with that dependent, calls `reg_build`, and the results are wrapped as a `tabxplor_tabs` list (so
   `tab_export("xl")` yields one sheet per dependent). `trials` must accept a **vector** (one per
   dependent). Decide the per-table labelling (model-name labels within each dependent's table).
2. **Complete-model predictor ordering** (L1). Where `union_predictors` is built (`R/tab_reg.R:1877/1900`,
   or before `reg_skeleton` at `:1407`): if one model's predictor set is a **superset of every other
   model's** (a "complete" model), reorder the union to that model's own order (placed at the end as the
   maintainer expects). If no complete model exists, keep first-appearance order. Everything downstream
   keys by `(var,level)`/`term` and follows the skeleton's `fct_inorder`, so reordering the union suffices.
3. **Bidirectional nesting + `na="drop_all"`** (L2). Two fixes for the "not nested or N differs" warning:
   + `reg_compare_guard()` (`R/tab_reg.R:1247-1253`) tests `all(t_ref %in% t_full)` only — also accept the
     reverse (`all(t_full %in% t_ref)`), so `baseline="complet"` (the baseline is the *superset*) is
     recognised as nested. Pick the LR direction from whichever is the sub-model.
   + Add opt-in **`na = "drop_all"`** (mirroring `tab()`): pre-compute a shared complete-case mask over the
     union of all specs' predictors + dependent + design vars, and fit every model on that population
     (`reg_fit` currently drops NA per-model at `:631-632`). Equal N then holds for genuinely-nested specs,
     enabling the LR test. Document that it changes ALL estimates (shared population), hence opt-in.

**Verify** — `tab_reg(gss_cat, c("married", <2nd binary>), list(a=…, b=…), family="binomial", trials=c(…))`
returns a list of tabs, `tab_export("xl")` writes one sheet each; a comparison with a superset baseline
runs an LR test (no AIC-fallback warning) under `na="drop_all"`; a complete model's predictors sit last.
Tests in `test-tab_reg.R` (list shape, ordering, nesting both directions, drop_all equal-N).

---





### Phase 15 – finalise jamovi module


#### Phase 15a – create Windows-side script to build and test .jmo files

I would want an automated script, Windows-side, that would : temporarily clone tabxplor current repo from github (not on D:/Statistiques/github, really a temp folder) ; load in R script with devtools, install package deps if needed, and install() jamovi module to build windows .jmo file.

```r
load_all() ; jmvtools::install(home = 'C:/Program Files/jamovi 2.7.37.0') ; load_all()
```


#### Phase 15b – jamovi UI `jmvtab_reg`

One user-friendly, fast, clear and simple regression analysis, starting from jmvtab template and adapting it to the regression functions and use case.
A "+" to add predictor subsets for `multi_logit`-style model comparison, selecting or selecting out among already chosen predictors.
Reuse patterns from jmvtab primarily. Customise .js to grey out options that are not possible with the other selected arguments or outcomes types. When relevant, reuse patterns from known regression jamovi modules.


#### Phase 15c — Jamovi UI French translation




### Last Phase — verif and package user-friendly documentation

#### Last Phase a – Bug corrections

See below for known bugs yet to fix.

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

#### Last Phase b – simplify main user-facing functions roxygen documentation

Simplify tab() and other main functions documentation, to make it more easily understandable and more helpful to students that are not statistical experts and may have difficulties with programming.

#### Last Phase c – Create several vignettes

The current vignette should be the basis for non-expert users, while also permitting expert users to understand what this package is really interesting for.

All the part about "programming with tabxplor" and its vctrs fields should come in their own vignette, and it must be uptaded and extended.

tab_logit should come with it’s own vignette.


#### Last Phase d – full `pkgdown` documentation + test coverage

Add test coverage to github actions.

Implement a full pkgdown documentation.
- Where ? On github pages ? Elsewhere with tidyverse ecosystem provided servers ?




### Reference — bugs, benchmarks, perf

#### Discovered bugs

- **A pre-existing golden drift, now baked in.** `n_ci_tabvars.rds` / `n_ci_tabvars_all.rds` have a
   `ci_sup` `NA` where the code now produces `NaN`. It **reproduces on unmodified HEAD** (so it is not
   Phase 14's), and `expect_equal`'s tolerance treats NA and NaN as equal, which is why no test ever
   saw it. Regenerating the goldens necessarily wrote it in. Worth a look: a NaN there may be a real
   edge in the mean-CI path (n≤1?), or merely cosmetic.

- **NEW (2026-07-16, seen live in jamovi on WSL; COSMETIC, pre-existing — not a migration issue).**
  A live `jmvtab` session prints, 3×, while the user adds the analysis and picks variables:
  *"! tabxplor formatting and colors skipped: the table has no tabxplor_fmt columns (not a
  tabxplor table). ℹ Rendering the plain table instead."* — the Phase 10c `tab_render_vars()`
  degrade path ([R/tab.R:2494](R/tab.R#L2494)). **The real tables are unaffected** (colours render
  correctly); this fires only on the transient degenerate shapes jamovi passes mid-selection.
  Reproduced (scripts in the C3 session; `jmvtab_build()` + `tab_kable(engine="html")`):
  + **`data` with 1 column, no vars selected** → emits the message **even though the built table
    HAS an fmt column** (`fmt=1/2`) ⇒ the degrade is reached on some *other* table inside
    `tab_kable`'s prep than the one returned, and **the message is misleading, not just noisy**.
    Start at `tab_export_prep()` / `tab_materialize_extras()`, not at `tab_render_vars()`.
  + **0-row `data` + named vars** → hard **ERROR** `"data is of length 0 (possibly after filter or
    na = 'drop_all')"` from `tab_plain()` via `purrr::pmap()` ([R/tab.R:1814](R/tab.R#L1814)).
    A 0-row table should degrade gracefully, not abort.
  + NOT the cause (each tested and cleared): the tier-3 **carrier** cache (all of fresh build /
    exact-tuple hit / digits re-apply / colour re-apply / `saveRDS` round-trip keep `fmt=4/5`);
    empty `row_vars`/`col_vars` against full data; `jmvtab_build()` itself.
  Fix in whichever phase next touches the exporter prep. Add the degenerate shapes to
  `test-edge-cases.R`.

In-code these are tagged for grep: `# KNOWN-BUG:` (bugs below), `# FIXME:` / `# FIXME(clarify):` / `# FIXME(future):` (suspect logic or future work, several tied to the Phase 5 color work), `# OBSOLETE:` (dead-code banners, e.g. the stale `tab_xl` duplicate). Fix each bug inside the phase that rewrites the relevant code, not as a separate pass.

- FIXED (Phase 1a): `fmt()` public constructor cast `totcol` into `refcol` (the `refcol` argument was silently ignored). Now casts `refcol`. Low impact (refcol is normally set internally).
- FIXED (Phase 7g-iii, golden-locked): two latent `ref` bugs surfaced by the reference picker. (1) `diff_index()` matched a level label as a REGEX, so a metacharacter label (e.g. `"$25000 or more"`) silently mismatched (the reported "picking the 2nd row_var does nothing" — `rincome` has `$` levels) and a substring label multi-matched — now EXACT-match-first, then regex. (2) `resolve_ref_vector()`'s `length(ref)==1` early return recycled even a NAMED length-1 ref, so `c(race = "Black")` leaked to every col_var — now only an UNNAMED length-1 recycles; a named one is name-matched. Both byte-identical on existing goldens (the goldens' refs are `first`/`tot`/non-substring labels).
- FIXED (Phase 6e, golden-locked; hardened Phase 7d-i): `tab_num(..., <tab_vars>, ci="cell")` used to error ("some columns don't belong to the data.table: [tab_var]") in the `tot="no"` grand-total-only grouping-set / `na="keep"` reorder path. 6e made the grand total a length-1 list so `num_rollup()` keeps every tab_var present; 7d-i added a defensive `intersect(tab_vars, names(tabs_tot))` guard at the reorder + an `expect_no_error` regression in `test-num-fuse-parity.R`. Locked by golden `n_ci_tabvars` / `n_ci_tabvars_all`, both `comp` modes.
- FIXED (Phase 14b): `tab_kable(engine = "html", popover = TRUE)` rendered its own escaped ATTRIBUTE STRING as the popover content (`data-content="data-toggle=&quot;popover&quot;..."`). `tab_kable_print_tooltip(popover = TRUE)` returned `kableExtra::spec_popover()`'s attributes from a *text* builder, and the html engine wrapped them again. Attributes now live only in `tab_tooltip_attrs()`; the arg is deleted. The same builder also ends a second drift: the html popover omitted `data-trigger`, so it needed a CLICK where kableExtra's opened on HOVER.
- FIXED (Phase 14b): the tooltip fragment join left a dangling `"f1: 5 ;"` / leading `"; f10: 5"` past 4 adjacent empty fragments — `str_replace_all(";  ; ", "; ")` matches non-overlapping, so the 3 repeats could not collapse a longer run. Latent (no cell reached 5 empties) until the 10th fragment made 9-empty runs reachable. Now an exact per-cell non-empty join.
- FIXED (2026-07-15, CI green-up): `tab_color_legend()`'s `lang` argument silently did nothing on **Linux** (`lang="fr"` returned English) — `Sys.setenv(LANGUAGE=)` alone can't switch gettext once glibc has cached a lookup. Now flushed via `flush_gettext_cache()` before/after/on-exit. Caught only because the snapshot tests SHIP and run on CI's Linux jobs. Cannot work under `LANG=C` (gettext ignores `LANGUAGE` there) — a documented gettext rule, not a package bug.
- FIXED (2026-07-15, CI green-up): 6 unqualified `globalVariables()` calls in `R/fmt_class.R` with `utils` declared nowhere — `pkgload::load_all()` crashed ("could not find function globalVariables") in any process without `utils` attached, e.g. a testthat parallel worker. Now `utils::globalVariables()` + `utils` in Imports. Latent since forever; surfaced by turning on `Config/testthat/parallel`.
- FIXED (2026-07-15, CI green-up): `test-tab_logit.R` "colour_signif='ignore'" asserted a symmetric OR break (`mag > 1.16`) against the **asymmetric** `mean_ratio` scale (`under` starts at 1.5 since Phase 13a) — wrong test; failed in isolation everywhere and on macOS CI, passing elsewhere only via a leaked global scale. Now derives the threshold per direction from the scale in force and pins it.
- **NOT a bug, but confirm the intent**: row labels are rendered with **U+202F narrow no-break spaces** in place of ASCII spaces by BOTH html engines (`"No answer"` -> `No<U+202F>answer`). Consistent across engines so it looks deliberate (keeps labels from wrapping), but it means HTML copy-paste yields NBSPs and any test comparing rendered cells to `levels()` silently matches nothing.
- `set_color_style(custom_palette=)` (`tab_classes.R` ~L3120): length check requires 10 but the message says 11 and 11 names (`pos1..neg5, ratio`) are applied — the `ratio` slot ends up valueless, so custom palettes are broken for the ratio color. Fix by accepting length 11.
- **FIXED (Phase 7e)**: `tab(data, >=2 row_vars, >=2 col_vars)` used to error "pct can't be recycled" for ANY `pct` (the multi×multi tables jmvtab drives). `tab()` recycles `pct` to a per-col_var vector (`pct = c(rep(pct, length(col_var)), ...)`), but `pct_vect` only broadcasts a per-col_var vector when there is exactly ONE row_var (branch B); with ≥2 row_vars it falls to the `else` stop. Fix: add a branch `is.character(pct) & length(pct) == length(col_vars)` → `rep(list(pct), length(row_vars))`. Pre-existing (reproduces pre-7d-ii on `git stash`); low impact (multi×multi + output_list); fix with the recycling code.
- `tab()` errors on a `data.table` **input** (works on tibble/data.frame). `tab(as.data.table(gss), marital, race)` → `tab_num()` "Selections can't have missing values" from `tidyselect::eval_select(col_vars, data)` (`tab.R` ~L3203) — under a data.table input the numeric-col_var index path (`as.character(col_vars)[col_vars_num]`, `tab.R` ~L1304) yields an NA selection. Low impact (users pass tibbles/data.frames; `tab()` does its own `setDT` on a narrowed copy internally). Discovered in the Phase 6b PoC (§26). Fix belongs with the Phase 2/6 aggregate-core / col_var-classification code, not a separate pass.
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

Flagged out of scope: weight column literally named `"wt"` → `num_moment_scan` name-collision crash;
`contrib` + `color_all_signif` colours nothing (contrib has no diff CI — pre-existing gap). (The
multi-row_var `pct`/`OR` length-mismatch warning + the mirai load_all crash were FIXED 2026-07-13, above.)

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


