# This file is intentionally empty and can be deleted (`git rm R/tab_logit_2.R`).
#
# Phase 12a folded the live logistic-regression code into R/tab_logit.R and dropped the
# pre-1.4.0 parsnip/tidymodels draft that used to live here (readable_OR(), or_plot(),
# lm_plots(), the custom "svglm2" survey engine). The engine is now a direct
# stats::glm() / survey::svyglm() + broom::tidy() path.
#
# Deferred to a later display phase (not lost, just not ported yet): the finalfit-derived
# odds-ratio forest plot (or_plot) and the glm diagnostic plots (lm_plots).
