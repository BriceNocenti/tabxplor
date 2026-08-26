# The 2.0.0 evidence base

Everything here settled one question during the 2.0.0 cycle and was then superseded by the code it
produced. **Nothing was rewritten or renamed on the way in**, so a `dev/<name>.md` pointer in a
roadmap DONE summary resolves here, unchanged.

⚠ **Read the guide, not the study.** Where a durable half survives, it was rewritten from the final
design into one of the seven guides at `dev/` root; the file here is the measurement behind it, with
its dated status banners, its rejected alternatives and its open questions intact. Several also
carry `file:line` anchors and argument names that no longer exist.

**Three live items are buried in here** and are not history:

- `tabxplor_2.0.0_exported_functions_review.md` **§5** — the Tier 1/2 unexport proposals, argued and
  costed, awaiting a decision.
- `reg_profiles_ideal_types.md` — a post-2.0.0 feature, deliberately postponed, not abandoned.
- `tabxplor_missing_features_audit.md` **§2, §7** — who the user is, and what competing tools do.
  A 2026-07 snapshot: several of its gaps have since been filled.

## Where each durable half went

| archived study                                                    | its durable half now lives in                       |
|-------------------------------------------------------------------|-----------------------------------------------------|
| `tabxplor_2.0.0_jamovi_dev.md`, `jamovi_results_width.md`         | `dev/jamovi_module.md`                              |
| `color_ladders_balance.md`, `color_blind_palettes_guide.md`, `black_and_white_publication_palette.md` | `dev/colors.md`          |
| `chi2_cell_residuals_and_contributions.md`, `weights_only_design_effect_soundness.md`, `weights_framework_redesign.md` (Appendix A) | `dev/inference.md` |
| `model_vs_observed_gap_test.md`, `ordinal_one_column_effects.md`, `poisson_vs_logistic_binary_outcome.md`, `reg_interactions_and_predictor_terms.md` (§8), `regression_effect_plots.md`, `regression_assumptions_plots.md` | `dev/regression.md` |
| `tabxplor_jmvtab_cache_design.md`                                 | `R/jmvtab-cache.R`'s header — ⚠ and the two differ: the study specifies **five** tiers, the code ships **four**, with a different tier 3 |
| `tabxplor_argument_computation_map.md`                            | `TAB_ARGS` + `tab_resolve_settings()` — ⚠ the study catalogues six arguments that are no longer `tab()` formals |
| `reg_estimand_api_redesign.md`, `reg_family_measure_effect.md`     | `R/reg-estimand.R`'s header (the cascade, restated verbatim) |
| `reg_crude_adjusted_and_display_integration.md`                    | `R/tab-display.R`'s header                          |
| `tooltip_consistency_review.md`                                    | `TOOLTIP_LINES` — ⚠ the hover layer it reviews is gone; 15 fragments became one table |
| `tabxplor_reg_performance.md` (§5, the four fitting sites)         | `R/tab-parallel.R` and `R/tab_reg.R`'s headers      |

## The rest, by kind

**The decision logs** — `tabxplor_2.0.0_decisions.md` (the settled architecture decisions, §1–§26,
the grounding behind CLAUDE.md's phase bullets) · `ecosystem_keys_2.md` (the Phase 19 end-of-cycle
study) · `tabxplor_ecosystem_simplification.md` (its round-1 predecessor).

**The plans** — `tabxplor_phase19_ecosystem_integration.md`, `tabxplor_phase20_surface_integration.md`
(two "plan of plans"; ⚠ every `file:line` in them is a dated anchor) · `phase_21_roadmap.md` ·
`tabxplor_phase10_exporters.md` · `full_survey_design_scope.md`.

**The assessments and audits** — `tabxplor_phase19_assessment.md` · `tabxplor_phase19p_api_review.md`
· `reg_math_review_edge_cases.md` (an adversarial review against one commit) ·
`tabxplor_2.0.0_performance_review.md` (a measurement snapshot, regenerable).

**The stress tests** — `tabxplor_2.0.0_stress_test_report.md` and `…_2_reg_exports.md` ·
`reg_comparison_framework_stress_test.md` · `weights_framework_stress_test.md` and
`…_2_post_z16.md`.

**The superseded designs** — `new_colors_UI.md` and
`design_new_colors_UI_decision_process.md` (the colour framework's first two rounds) ·
`model_vs_observed_effect_colour.md` (superseded by the gap test) ·
`numeric_predictors_crude_counterparts.md` · `reg_estimand_api_redesign_follow_up.md` ·
`tabxplor_phase9b_fmt_display_only.md` · `empty_vctrs_fields_sparse_record.md` (a *rejected* change:
do not make the record sparse).

**The scripts** — one-off verification and measurement, each superseded by the two test suites:
`verify_ci_inclusion.R` · `verify_estimand_library.R` (⚠ needs an `.rds` of a table that no longer
exists) · `manual_review_131_vs_200.R` · `weights_stress_test.R` and `survey_design_measurements.R`
(the reproducers for the weights studies — they print, they do not assert) · `census_exports.R`.

**The subfolders** — `benchmarks/` (the phase-named one-off profilers and the dated result CSVs; the
durable harness stayed at `dev/benchmarks/`) and `review_manual/` (the frozen manual-review passes
and their output artifacts; `review.R`, `xl_review.R` and `legend_review.R` stayed at
`dev/review_manual/`).
