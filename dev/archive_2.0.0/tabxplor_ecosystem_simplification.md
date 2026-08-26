# tabxplor ecosystem simplification — design analysis and propositions (2026-07-20)

This document is the end-of-2.0.0 design review the maintainer asked for: where the package is still missing integration, where code is duplicated, which attributes to simplify or add, which complexities are not worth their cost, and which white elephants to remove. It was produced by six parallel deep audits (fmt/colour/legend; the tab() build pipeline; the export stack; tab_reg + jamovi; classes/attributes/options; vignettes + dev history), every claim grepped or read in source, and the four most load-bearing defect claims re-verified by hand. It deliberately puts backward compatibility aside first (§2–§7), then reinstates it in the sequencing (§9): several propositions are **free only before the 2.0.0 CRAN release** and become deprecation projects the day after.

How to read: §1 grounds everything in real use. §2 names the five cross-cutting disease patterns. §3 lists concrete defects found during the audit (fix these regardless of any redesign). §4 is the single most important proposition — the missing key. §5 details propositions per subsystem. §6 is the honest white-elephant list. §7 is what NOT to do. §8 answers the maintainer's attribute questions directly. §9 sequences everything against the CRAN freeze and the French phase.

---

## 1. Ground truth — what tabxplor actually is

The vignettes, README and jamovi UI teach a sharp, small set of workflows (the *hot surface*): exploratory coloured crosstabs (`tab` + `pct` + `color`/`color_signif`, refs and comps), means, CIs/stars/display templates, tests and contributions, the four exporters, dplyr post-processing with `is_totrow()`, regression with `empirical = TRUE` crude-vs-model companions, and the jamovi live UI for literary students. The differentiators the internals must serve: **(a)** per-cell statistical metadata enabling lossless display switching; **(b)** colour helpers that read significance; **(c)** the crude-vs-model comparison no mainstream package offers; **(d)** the jamovi teaching path; **(e)** dplyr/tibble citizenship.

Two facts from that grounding should steer all simplification:

- **The hot surface is argument-thin.** Real usage flows through ~10 core + ~14 common arguments. Most of the remaining ~27 `tab()` formals are expert-real (fine) or shims/vestiges (candidates).
- **There is a large cold surface**: shipped, differentiator-grade features that no vignette teaches (`tab_counts()`, `tab_css()`, `transpose=`, `n_min=`, `split_var=`, `tab_spread()`…). Before cutting anything "unused", distinguish *undocumented* from *unwanted* — several cold features are recent and simply missing their teaching paragraph. Conversely, features that are cold **and** internally expensive are the true white elephants (§6).

---

## 2. Diagnosis — five cross-cutting patterns

The subsystem audits converge on five patterns. Every concrete proposition in §5 is an instance of one of them.

### 2.1 Roles are still guessed, not stored — the missing key

2.0.0 stored *statistical* metadata beautifully (18 fields, 10 column attributes, 8 table attributes). What it never stored is **role** metadata — "what is this row/column/cell *for*" — so a dozen sites reverse-engineer roles from rendered strings:

- Synthetic rows (n, row_pct, pvalue, gof) are recognised by **English label whitelists** in three places: `tab-export-prep.R:410-416` (with its own WARNING that jamovi's gettext labels silently break it), `tab_classes.R:1347` (`tab_collapse_total_rows`), `tab-transpose-render.R:181,187`.
- Regression columns are classified emp-vs-model by `startsWith(cn, "Emp.")` in the legend (`fmt_class.R:3583, 3171`) — a renamed column silently flips the legend wording.
- The Total column is detected by `startsWith(nm, "Total")` in `legend_ref_label` (`fmt_class.R:3187`) although `is_totcol()` exists.
- A p-value cell encodes its role in the `display` token plus a **magic `diff = -0.5`** whose only purpose is to trip the strongest under-colour slot (`tab-test-display.R:103`); it also writes `col_var = "chi2_cols"` which nothing ever reads.
- `tab_collapse_total_rows` compares **rendered `format()` strings** to decide structural equality (`tab_classes.R:1360-1362`).

This is the pattern that will collide head-on with **Phase 18h (French translation)**: anything that renders-then-matches-by-English breaks the day the labels are French. §4 makes fixing this the keystone.

### 2.2 One-model subsystems that stop halfway

2.0.0's best moves were "one source of truth + dumb renderers" (`tab_export_prep`, `format(syntax = "excel")`, `tab_footer_streams`, the legend MEASURES table, `tab_append_footer`). But each stopped short of its own logic:

- The **render model is roles-only**: every backend still calls `format()` itself with a hand-coherent flag set, re-derives headers (md hand-rolls its own spanning-run loop at `tab_md.R:473-505` while three backends consume the shared `tab_header_runs()`), and re-implements slot→hex three times (`ann`, `tab_xl.R:324-326`, `tab-css.R:169-177`).
- The **colour plan does not read MEASURES**: measure facts exist twice — as legend data (`MEASURES`, `fmt_class.R:3020`) and as 11 switch arms inside `fmt_color_plan` (`fmt_class.R:2712-2890`).
- The **settings resolver is not the single resolver**: `tab_resolve_settings()` exists, yet OR-ness is re-derived at ≥7 sites, `get_type()` is called 31× in tab.R alone, `tab_num` re-runs the whole colour/ci cascade behind a `.color_deprecate` flag, `finalize_color_spec` runs twice on the numeric path, and three normalizers plus a *manufactured* legacy string (`legacy_union` re-creating `"diff_ci"` for `color_measure_policy` to re-parse) form the colour system's remaining maze.
- The **ctx is an untyped bag** unpacked by `list2env()` in every stage, guarded by 39 `exists()` calls, with `ctx_update`'s NULL-preservation invariant maintained purely by comments at three sites.

### 2.3 The axis machinery is the top bug factory

Five documented bugs came from recycling arguments across the row_var × col_var axes with vectorised `&`/length heuristics (the multi×multi `pct` stop, the `pct & OR` recycle warning and its missed twin, the named-length-1 `ref` leak, the `na_num[[i]]` positional crash, the `ncol(M)` NULL crash). The machinery is spread over ≥8 code paths (`tab()` pre-recycle, tab_setup's 9+2 recycles, the 5-branch `pct_vect`, `ref_vect`, `tab_rowvar_ctxs`' "per-row_var iff length happens to equal n" guessing, `tab_pmap`, and the step wrappers' re-recycles). The dev history lesson is explicit: this bug class recurs until the axes *physically cannot* meet in one vectorised expression.

### 2.4 Parallel implementations drift — and the audit caught them drifting

Every rule maintained "in parallel, kept in sync by comment" eventually diverges. Verified drift found by this audit:

- `diff_index_mean` (`tab.R:4604`) **never received** the Phase 7g-iii exact-match-first fix its factor twin `diff_index` (`tab.R:6775`) got — a mean table with a regex-metacharacter reference label silently mismatches today.
- `fmt_col_attrs` (`tab.R:2949`) still lists **9** attributes — Phase 15e's `model_family` is missing, so footer materialisation strips it and mixed-family *exports* re-acquire the exact legend mislabel 15e fixed (§3, defect 1).
- `gtab_cast`/`gtab_ptype2` reconcile bind attributes **one-sidedly** while the plain-tab path merges via `tab_bind_attrs` — binding two grouped tabs keeps only one table's `test` rows.
- The star-field pad rule, the console bold/grey set, the "empirical CI matches the model CI" method literals vs `ci_settings`, and the three complete-case `drop_na()` recomputes are all sync-by-comment pairs waiting for the same fate.

The 16e lesson generalises: **group by the rendered thing, or derive both sides from one fact table** — never maintain two encodings.

### 2.5 Dead weight

~780 lines of commented-out corpses across the three big files (old tab_ci implementation, pillar_shaft relics, color_graph, the vctrs FAQ transcription), a dead function (`var_contrib`), a dead S3 method (`pillar_shaft.tab_chi2_fmt` — its class is constructed nowhere), a self-declared no-op (`ci_html_subscript`), unreachable signature values (`tab_num(na = "drop_fct"/"drop_num")` rejected by its own `stopifnot`), a dead vendored `path_sanitize`, and the ~650-line superseded dplyr-era trio (`tab_pct`/`tab_tot`/`tab_totaltab`) still living inside tab.R with ten sites keeping the retired `chi2 =` constructor alias alive.

---

## 3. Defects found during this audit — fix regardless of any redesign

Ranked by user impact. 1–4 were re-verified by hand in source.

| # | Defect                                                                                                                                                              | Site                                               | Impact                                      |
|---|---------------------------------------------------------------------------------------------------------------------------------------------------------------------|----------------------------------------------------|---------------------------------------------|
| 1 | `fmt_col_attrs` misses `model_family` (10th attr)                                                                                                                   | `tab.R:2949`                                       | mixed-family reg exports mislabel legend    |
| 2 | `vec_math` sum/mean keep only text-channel `color`, drop `color_signif` + `model_family`                                                                            | `fmt_class.R:~4500-4550`                           | summarised columns lose bg channel + policy |
| 3 | `diff_index_mean` lacks exact-match-first                                                                                                                           | `tab.R:4604` vs `:6775`                            | mean ref labels with `$`/`(` mismatch       |
| 4 | grouped-tab binds keep one side's `test`/subtext                                                                                                                    | `tab_classes.R:2846,2862`                          | silent test-row loss on `vec_rbind`         |
| 5 | p-value red never fires under `grey_non_signif` (gate reads the fake `ci_inf=0,ci_sup=0` bounds)                                                                    | `tab-test-display.R:103` + `fmt_class.R:2832-2837` | wrong colour on test rows                   |
| 6 | jmvtabreg inherits `jmv_col_fp`'s value-edit blind spot **undocumented** — can serve a stale fit                                                                    | `jmvtabreg-cache.R:18-19,140`                      | stale regression after data edit            |
| 7 | `color_breaks` is the only table attr dropped by every dplyr verb (not in `tab_attrs()`)                                                                            | `tab_classes.R:3739`                               | per-table breaks silently revert            |
| 8 | `tabxplor.output_kable` is a redundant second auto-kable mechanism and the only route to the KNOWN-BUG                                                              | `tab.R:2198,2247`                                  | crash under two-channel colour              |
| 9 | Stale docs: CLAUDE.md still says "all exporters call `fmt_color_selection()`"; repo-map line counts off; `tab-render-html.R:536` still says kableExtra is an Import | —                                                  | misleads next refactor                      |

Fix 1 by *deriving* `fmt_col_attrs` from one source (e.g. the `new_fmt()` formals minus the 18 field names) so an 11th attribute can never repeat this. Fix 2 by using `fmt_color_attr(x)` (as `+`/`-` already do) and passing all attributes. Fix 3 either directly or via the reference plan (§5.1). Fix 5 properly via the cell-role proposition (§4), or minimally by gating on the real `pvalue` field. Fix 8 by soft-deprecating the option toward `options(tabxplor.print = "kable")`.

---

## 4. The keystone proposition — a role model: everything knows what it is

The single highest-leverage change, and the one the French phase depends on. Three small additions, all internal, no fmt field surgery (the c-iii verdict stands — these are *new, internal-only* metadata, not field merges):

### 4.1 A `role` per synthetic row and column

Every row/column created by a materializer (`tab_add_n_pct`, `tab_append_footer`, the sd twin, the OR n-column, total rows) records its kind at creation — `"data" | "total" | "n" | "row_pct" | "pvalue" | "gof" | "sd"` — instead of being re-detected later from English labels or rendered strings. Cheapest viable carrier: a `row_roles`/`col_roles` entry inside the existing `vars` attribute (it already survives every verb via `tab_attrs()`), maintained by the materializers themselves. Retires: the three English whitelists, the rendered-string equality in `tab_collapse_total_rows`, the transpose's absorb heuristics, and makes the documented jamovi-gettext degrade impossible.

### 4.2 A cell role instead of the `-0.5` hack

P-value/GOF cells currently store their p in *three* fields (`pct`, `var`, plus a fake `diff = -0.5` steering the colour engine) while the real `pvalue` field goes unused for them. Give these cells their p in `pvalue`, colour them via one explicit rule in the colour plan (`sig_source = "pvalue"` — the same mechanism contrib already uses since Phase 18a), and delete the dead `pillar_shaft.tab_chi2_fmt` and the write-only `col_var = "chi2_cols"` marker. This also fixes defect 5 by construction.

### 4.3 A reg column role attribute

One more column attribute (`role = "model" | "emp" | ""`), written by `reg_build`, read by the legend adapters — replacing the three `startsWith("Emp.")`/`startsWith("Total")` name heuristics. Do it together with the `fmt_col_attrs` derivation fix (defect 1) so the 11th attribute is added safely in one motion. Cost: one `/vctrs-field` checklist pass; the attribute is internal-only (no exported getter needed initially).

What this buys beyond correctness: Phase h (French) stops being dangerous; the future `!`-weak-test glyph and partial-colour test cells (both documented futures) become display swaps; the staged materializer (§5.3) becomes possible because fold/drop decisions can be declarative per backend.

---

## 5. Propositions by subsystem

Each ranked list is ordered by impact ÷ churn. "Byte-identical target" means goldens must not move; "conscious regen" means one deliberate snapshot regeneration.

### 5.1 Build pipeline (tab.R and friends)

1. **Delete verified-dead weight** — `var_contrib()`, `drop_fct`/`drop_num`, `tab_last`, ~290 commented lines, `zscore_formula` moved to tab-agg.R. Zero risk.
2. **One axis-broadcast settings frame.** Replace the ≥8 recycling paths with a single per-(row_var × col_var) settings frame built at the boundary; `tab_rowvar_ctxs` slices frame rows instead of guessing by length; the tab_many list-of-lists `pct` grammar folds in as one input branch. This makes the §2.3 bug class *unrepresentable*. Byte-identical target; sentinels are test-parallel-parity and test-cache-keys.
3. **One reference plan per leaf.** Compute `(ref-row rule, ref_col_idx, ref2, comp_group)` once per leaf; `tab_apply_reference` stays the executor (the jmvtab tier-3 reref already consumes it verbatim); tab_num's inline `calculate_refrows` copy and `diff_index_mean` are deleted (fixing defect 3); tab_ci's built-table re-derivation chain (`detect_totcols`/`detect_refcol`/8-branch case_when) consumes the plan instead. Must preserve: `ref` reinterpreted by `pct`, per-row_var named refs, the col% collapse message (settled §4).
4. **Split tab_plain/tab_num into public wrapper + resolved-args core** (already endorsed by decisions §29 Finding 3). The pipeline calls the core with settings as-resolved; only the wrapper re-parses colour/ci. Removes the double `finalize_color_spec`, the `.color_deprecate` flag, the triple `stars` option read, and the leaves' duplicated `ref="auto"`/`comp` forcing.
5. **Quarantine the superseded trio** — move `tab_pct`/`tab_tot`/`tab_totaltab` + `pct_formula`/`diff_formula` + their repair machinery (~650 L) to `R/tab-steps-legacy.R`; retire the internal `chi2 =` constructor alias and `get_chi2()` reads (10 sites). Exports unchanged; tab.R drops below ~5000 L and its live pipeline becomes readable end-to-end.
6. **Shared leaf tails** — the verbatim totals-renaming tail, `tab_var_1lv` wrap, totrow/tottab derivation, and the six-copy placeholder-injection idiom extracted once for both leaves (~150 L).
7. **Type the ctx** — a constructor giving every field a default kills the 39 `exists()` guards and encodes `ctx_update`'s NULL rule in the helper instead of comments. Fold into whichever of 2/4 lands first.
8. **Soft-deprecate `tab_num(df=, num=)`** — the escape hatch keeps three copies of the pre-2.0.0 `weighted.mean` N-scan alive (~90 L) for 4 assertion lines of test usage. A post-hoc converter reading fmt fields serves the same need.

### 5.2 fmt / colour / legend (fmt_class.R)

1. **Defect fixes 1, 2, 5** (§3) — first, they are one-line-to-small.
2. **`get_ref_field(x, field)`** — one base-R helper replacing the four copy-pasted broadcast pipelines `get_ref_pct`/`get_ref_means`/`get_ref_var`/`get_mean_contrib` (~70 L, and a measured-pattern ~28× speedup on the colour hot path, per the `fmt_row_flag` precedent).
3. **`COLOR_MEASURES` fact table, merged with the legend's MEASURES.** Per measure: raw field, scale key per column kind, `sig_source ∈ {bounds, pvalue, none}`, totrow/refrow gates — leaving only the diff↔ratio bound rescale and the guaranteed-effect offset as explicit policy code. 11 switch arms → ~3; adding a measure becomes one row end-to-end (legend included); the `/color-mode` checklist shrinks accordingly. Byte-identical target (the plan is golden-locked).
4. **Finish "Step 4d"**: decode legacy colour strings (`diff_ci`/`after_ci`/`ci`) once at the argument boundary and thread only the decoded `(color, color_signif)` pair; delete `color_measure_policy`'s re-decoding and `legacy_union`'s string manufacture. High churn — the jamovi cache tuple includes the legacy string, so schedule with a jamovi phase and a schema bump.
5. **Canonicalise `rr` → `ratio`** as the internal token (read-side alias only) — deletes ~8 dual matches.
6. **A token registry for `format()`** (per token: source field, ×100, signed, big.mark, min-digits, excel-code class). The Excel arm already proves the masks reify as data. Do it only with the byte-identity harness, or fold into the next display change.
7. **Housekeeping** — delete `ci_html_subscript`, the dead pillar method, ~200 commented lines, merge `vec_ptype_abbr`/`vec_ptype_full`, single-source the `get_wn` NA→n fallback (4 copies).

### 5.3 Export stack

1. **Fold md onto the shared models** — use `tab_header_runs()` + prep's `new_col_var` instead of md's hand-rolled loops (`tab_md.R:257-268, 473-505`). Conscious md-snapshot regen, nothing given up.
2. **Finish the ann-hex unification for Excel** — the stale "Phase 10j-A-ii" TODO (`tab_xl.R:186-190`): consume the theme-resolved hex already in `ann`, drop xl's own `get_color_style()` lookups; slot→hex becomes single-sourced (with `tab-css.R` as the CSS-side reader of the same source).
3. **A `rd_footer(rd, medium, theme)` helper** — folds the 4× footer-invocation boilerplate and the 4× reg-caption fallback into the model boundary. Trivial.
4. **One staged materializer** — declare synthetic rows/cols as specs (kind + payload) with per-backend fold policies, replacing the current 6–8 passes and both create-then-delete cycles (the n column built then folded in-cell; total rows built then collapsed). Requires §4.1; highest structural payoff in the export stack; one conscious cross-backend regen.
5. **Transpose = a flipped call into a shared `roles_from()` builder** — extract `prep_one_table()`'s role assembly for both orientations; keep `tx_format_source_cols` (that constraint is physical). Fixes the already-present drift (transposed tables lose `reg_title` and `empirical_tips` today) and ends mirror maintenance.
6. **Quarantine → kill the kableExtra engine** (~380 L + `inst/tab.css` + `kable_tabxplor_style`'s 137 L). Precondition: make the html engine's Viewer print self-sufficient — today `print.tabxplor_kable` delegates to `print.kableExtra` for the tooltip JS deps, so the default engine's interactive tooltips silently depend on a Suggests package. Ship tabxplor's own htmlDependency (or degrade gracefully), then hard-deprecate the engine.
7. **Merge the twin console print methods** (one body, `out[3 + is_grouped]`) — ~55 L, byte-identical.
8. Do **not** force pillar through the render model (it owns layout); optionally let `pillar_shaft` accept an `ann`-style annotation so the console bold/grey rule has one implementation.

### 5.4 tab_reg

1. **`reg_wald_finalize()` + `align_to_skeleton()`** — the Wald est±crit·se→p-dual→exp block exists 3×, the `"\r"`-key skeleton alignment 5×. Byte-identical refactor, kills the CI↔p triple-statement.
2. **Spec as the unit of truth in `reg_build`** — drop the scalar family/do_exp/effect_shape/eff_word/color formals (15e already populates specs fully); collapse the 30-formal signature re-listed at 3 call sites into `(data, specs, shared)`. Removes the 19 `sp_get()` fallbacks. Internal-only (verified: reg_build has no caller outside tab_reg.R).
3. **Empirical fact table** — per (family, effect): column names, fmt shape, CI function + method, colour measure — driving one builder loop, with `ci_settings` **derived from the same rows**. Encodes the 16d "empirical CI matches the model CI" rule as data instead of two hand-synced sites; the four ~15-line isomorphic arms collapse. Multinomial tips stay a separate arm (different medium).
4. **Store the model frame (or row mask) once per fit** and thread it — the "same population as the model" guarantee currently rests on three textually-identical `drop_na()` calls.
5. **Single `reg_cleanup()`** for the 8× inlined `stri_replace_all_regex(x, cleannames_condition(), "")`.

### 5.5 jamovi

1. **One cache kernel** — extract store lifecycle + byte-bounded LRU + fetch-or-compute + a generic `jmv_fold_array()` into one internal module; jmvtab keeps its 3-tier keys, jmvtabreg its 2-tier, as configs. The stated decoupling reason justifies separate *stores*, not separate *implementations* (two LRUs with incompatible entry shapes, one O(n²)). Ride the designed schema-bump invalidation.
2. **Shared R6 helpers** — `.notice()`, `.render_html()`, the export-click block and the `jmv-weights` fold are verbatim ×2 across the two `.b.R` files.
3. **Document the fingerprint blind spot in jmvtabreg** + thread the `tabxplor.jmv_full_hash` escape hatch to both modules (defect 6).

### 5.6 Classes, attributes, options

1. **Adopt `tab_restore()` in the six hand-rolled restore blocks** (select/rename/rename_with/relocate/summarise/arrange) — ~35 L, removes the one place a future attribute can be forgotten.
2. **Unify the grouped/plain bind reconcile** (defect 4) — make `gtab_cast`/`gtab_ptype2` use `tab_bind_attrs`.
3. **Decide `color_breaks` carry before release** (defect 7) — either one line in `tab_attrs()` + a `new_tab()` formal (public → now-or-never) or loud documentation in `?tab`.
4. **The `meta` merge — a genuinely open now-or-never call.** CRAN 1.3.1's `new_tab()` is `(tabs, subtext, chi2, ...)`; the five 2.0.0-new scalar attrs (`render_extras`, `ci_settings`, `vars`, `empirical_tips`, `reg_meta`) are unreleased formals. Merging them into one `meta` list is ~80 real code sites of mechanical churn — payable **only now**; after release the 8-formal constructor is frozen forever. **Maintainer’s decision : merge now.**
5. **Option namespace pass on 2.0.0-new names only** — `kable_css` → `tab_kable_css` (alias kept), consider `console_theme`/`export_theme` aliases for the two non-parallel theme options, seed-or-delete the unseeded `jmv_full_hash`, let `always_add_css_in_tab_kable` die with `kable_tabxplor_style`, retire `output_kable` (defect 8).
6. **Keep the S3-per-verb model** — the empirical dispatch probe (dplyr 1.2.1) shows the reconstruct trio covers mutate/filter/slice/distinct/arrange and the *plain* class needs no methods at all, but grouped select/rename/relocate/summarise/ungroup genuinely require their explicit methods. The redundancy is in the bodies (fixed by 1), not the registrations.

---

## 6. White elephants — the honest list

"Cut now" = free while unreleased. "Deprecate" = CRAN etiquette required. "Keep" = suspicion checked and dismissed.

| Item                                                                                                           | Evidence                                                                        | Verdict                           |
|----------------------------------------------------------------------------------------------------------------|---------------------------------------------------------------------------------|-----------------------------------|
| `predicted_unadjusted` (~80 L)                                                                                 | column mathematically equals same-frame Emp. % (its own doc); 1 test; no jamovi | cut now; keep as test assertion   |
| `method = "profile"` per-coef LR refits                                                                        | 2 test mentions; silent Wald fallback in most families                          | keep                              |
| `tab_num(df=, num=)` escape hatch                                                                              | keeps 3 copies of the old N-scan for 4 test lines                               | cut now                           |
| `totcol` 5-grammar parser                                                                                      | names/indices/col-no grammars: zero test hits, no jamovi use                    | cut 3 of 5 grammars now           |
| `filter=` string arg on `tab()`                                                                                | zero test coverage; dplyr upstream serves it                                    | doc-deprecate                     |
| `.by_table` on `tab_many()`                                                                                    | test infrastructure as a public argument since 9c                               | make internal now                 |
| `tabxplor.output_kable`                                                                                        | redundant with `tabxplor.print="kable"`; carries the KNOWN-BUG                  | used in .Rmd /.qmd only, keep     |
| kableExtra engine (~380 L + tab.css)                                                                           | only tests select it; html engine is default                                    | keep as legacy                    |
| `conditional_format`, `n_min`/`hide_near_zero` on `tab_xl`                                                     | reserved/inert shells                                                           | drop now, before release          |
| `var_contrib()`, `pillar_shaft.tab_chi2_fmt`, `ci_html_subscript`, dead `path_sanitize`, `drop_fct`/`drop_num` | zero callers / unreachable / no-op                                              | delete now                        |
| `fct_clean`, `compare_levels`, `formats_SAS_to_R`                                                              | zero callers (formats_SAS_to_R = personal tooling)                              | delete or move to dev/            |
| `score_from_lv1`                                                                                               | exported, zero internal callers, no tests                                       | keep, test, document, in vignette |
| `tab_get_wrapped_dimensions`                                                                                   | exported, zero internal callers, no tests                                       | keep: personal use                |
| ~780 commented-out lines across the 3 big files                                                                | inventoried per file in the audits                                              | delete now                        |
| `tab_plot`                                                                                                     | superseded badge, yet 16e invested in it; sole consumer of ggpubr shims         | maintainer call: freeze           |
| `quasipoisson` family arm                                                                                      | near-redundant since auto-φ-scaling, but ~1 switch arm                          | keep (cheap)                      |
| Compound-formula escape hatch                                                                                  | guard-heavy but real power-user value                                           | keep, contain                     |
| `spread_vars`, `comp="all"`, `parallel=`, `pct="all_tabs"`, `levels="auto"`, `transpose=`, `get_data`          | verified in use (jamovi/tests/vignettes)                                        | keep — not elephants              |

The other half of the white-elephant story is the **cold-but-good** list (§1): `tab_counts()`, `tab_css()`, `n_min=`, `split_var=`, `transpose=` deserve a vignette paragraph, not a cut.


---

## 7. What NOT to do — anti-propositions

- **Do not route reg columns through the aggregate core.** A fitted model has no count aggregate; the fmt-direct construction is the correct seam, and the real sharing (CI engines, legend, footer, exporters, test grid) already exists.
- **Do not merge fmt fields or drop column attributes.** The c-iii verdict stands: all 18 fields are user contract, all column attributes are required for standalone-column rendering. §4's additions are new internal roles, not consolidations.
- **Do not replace the S3-per-verb model** (probe evidence in §5.6.6); dplyr has historically moved verbs between dispatch homes, so the explicit grouped methods are the hedge.
- **Do not force-merge the test-display rails** (console grid above the table vs export body rows) — evaluated in 16a as a net complexity add; share helpers, not rails.
- **Do not re-open the settled perf verdicts** — scan fusion stays removed, no second `tab()` core, the chi2 marshalling rewrite stays abandoned, `.fine` stays a jmvtab/tab_counts seam.
- **Do not chase `pct="col"` parity as a refactor side effect.** Its second-class status (deferred binary-OR mirror, invert-and-transpose guidance) is a *product* decision; if it changes, change it deliberately.

---

## 8. Direct answers to the maintainer's attribute questions

**Table-level attributes to simplify:** the five 2.0.0-new scalars are individually justified, but their *constructor surface* is the thing to decide now (§5.6.4 `meta` merge — now or never). `test` earns its place (the one attr needing `vec_rbind`); `ci_settings` should stop being hand-mirrored from `tab()`'s formals and instead be derived from the same fact rows the CI engine uses (§5.4.3). The internal `chi2 =` alias should finally die (§5.1.5). `color_breaks` must either join `tab_attrs()` or be loudly documented (§5.6.3).

**Column-level attributes to add:** `role` (`"model"/"emp"`) for reg columns (§4.3) — added together with deriving `fmt_col_attrs` from one source so the addition is structurally safe. Nothing else: the audit looked for more and found the existing 10 sufficient once roles exist.

**Cell-level:** no new field. The p-value/GOF cells should use the *existing* `pvalue` field honestly instead of the `-0.5` encoding (§4.2). The `display` token stays the role carrier for masked/blank cells — acceptable once the colour gate reads `pvalue`.

**Row/col-level (new concept):** the synthetic-kind flags inside `vars` (§4.1) — the missing metadata that lets export-prep, collapse, transpose and the future French labels stop guessing.

**Table-level to add:** a stored `caption` for crosstabs (today only reg tables have `reg_title`; a user caption never survives a pipeline). One `vars` sub-field, zero new attrs. Also: `tab_plain()` should write `vars` at build (it's free) so `tab_render_vars` stops guessing on step-built tables.

---

## 9. Sequencing against the release

### Now, before the CRAN freeze (free on unreleased surface, or defect fixes)

1. Defect fixes §3.1–3.8 + the doc corrections (§3.9).
2. Dead-weight deletions (§2.5, §6 "delete now" rows).
3. The now-or-never decisions: `meta` merge or conscious freeze; `color_breaks` carry; option renames/aliases; `output_kable` retirement; white-elephant cuts on unreleased args (`predicted_unadjusted`, `mnl_vsrest`, `totcol` grammars, `.by_table`, `conditional_format`/`n_min`/`hide_near_zero` ship-or-drop).
4. `tab_restore()` adoption + bind unification + twin-print merge (§5.6.1–2, §5.3.7) — small, byte-identical, protect the release.

### 1.4.x — mechanical, byte-identical targets

`get_ref_field`; `reg_wald_finalize`/`align_to_skeleton`/`reg_cleanup`; the `rd_footer` helper; md onto shared header runs; xl ann-hex completion; the jamovi cache kernel + R6 helpers; `rr`→`ratio`; shared leaf tails; typed ctx.

### 1.5 — structural (each one phase, each with the §Verification protocol)

The role model (§4) **before Phase 18h** if at all possible — it is the French phase's enabling move; then the axis settings frame; the reference plan; the leaf wrapper/core split; the staged materializer + transpose `roles_from`; `COLOR_MEASURES`; the empirical fact table + spec-as-truth `reg_build`; Step 4d with a jamovi schema bump; the superseded-trio quarantine; the kableExtra quarantine→kill.

### Maintainer choices

1. `meta` merge of the 8-formal `new_tab()` (§5.6.4)
2. Pre-release white-elephant cuts to take chosen in §6 "cut now" rows.
3. `tab_plot`: freeze as legacy.
4. Schedule the role model now.
