# This file is intentionally empty and can be deleted (`git rm R/tab_logit_2.R`).
#
# Phase 12c folded the regression code into R/tab_reg.R (the unified tab_reg() engine, with
# tab_logit()/multi_logit() as binomial-family wrappers). The engine is a direct
# stats::lm()/glm() / survey::svyglm() + broom::tidy() path.
#
# Deferred to a later display phase (not lost, just not ported yet): the finalfit-derived
# odds-ratio forest plot (or_plot) and the glm diagnostic plots (lm_plots).
