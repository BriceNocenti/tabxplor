# PURPOSE: The Phase 22f-ii visual review set -- 30 tables across both producers, one workbook, for
#          a manual read-through in Excel (and its html twin, to compare the two side by side).
# ROLE: Manual, maintainer-run. NOT a test: what it checks is layout, and only eyes can.
# USAGE (from the package root):  Rscript dev/review_manual/xl_review.R
#        then open  ~/xl_review.xlsx , ~/xl_review.html  and  ~/xl_review.md
# WHAT TO LOOK FOR, sheet by sheet (the sheet name says which case):
#   * the UNIT row under each header: "<row%>", "<n>", "<OR>" -- italic, grey, left, no rule above it,
#     and stated once per BLOCK (a Total column restates its own, the count beside it says "<n>")
#   * the index columns ("var", "levels"): bold variable names, merged with the unit cell below,
#     reading from the left whether rotated or not
#   * vertical rules: one per block -- and NONE between a Total column and its own count
#   * a ratio / odds-ratio cell: reads "1/2.11" or "/1.20", and is still a NUMBER (click it)
#   * an aside column: the console's own brackets, in grey, with no stars
#   * greyed-out cells: really grey, not black
#   * the title above and the legend below: one merged, wrapped block about an A4 width
#   * `checks`: the model-check pictures under the model they check

suppressMessages(devtools::load_all("~/github/tabxplor", quiet = TRUE))
g <- gss_cat_data_formatting()
data("tea", package = "FactoMineR")
tea <- tibble::as_tibble(tea) |>
  dplyr::mutate(dplyr::across(
    tidyselect::all_of(c("home", "work", "tearoom", "friends", "resto", "pub")),
    ~ if (stringr::str_detect(levels(.)[1], "^Not")) forcats::fct_rev(.) else .))

p <- c(rincome = "rincome", race = "race", age = "age")

tabs <- list(
  # --- 1-8: the crosstab basics, one display preset each -----------------------------------------
  "row%"            = tab(g, "rincome", "marital", pct = "row", color = TRUE),
  "col%"            = tab(g, "rincome", "marital", pct = "col", color = TRUE),
  "n only"          = tab(g, "rincome", "marital", n = "min"),
  "diff"            = tab(g, "rincome", "marital", pct = "row", display = "{diff}", color = TRUE),
  "ratio"           = tab(g, "rincome", "marital", pct = "row", ref = "tot", display = "{ratio}",
                          color = "ratio"),
  "OR"              = tab(g, "rincome", "marital", pct = "row", ref = 1, display = "{or}",
                          color = "or"),
  "contrib"         = tab(g, "rincome", "marital", pct = "row", test = TRUE, display = "{ctr}",
                          color = "contrib"),
  "pct + n"         = tab(g, "rincome", "marital", pct = "row", n = "range", color = TRUE),

  # --- 9-14: composite cells, i.e. one aside column each in Excel --------------------------------
  "pct (n)"         = tab(g, "rincome", "marital", pct = "row", display = "{pct} (n={n})"),
  "pct (OR)"        = tab(g, "rincome", "marital", pct = "row", ref = 1, display = "{pct} ({or})",
                          color = "or"),
  "OR (pct)"        = tab(g, "rincome", "marital", pct = "row", ref = 1, display = "or_base",
                          color = "or"),
  "est + ci"        = tab(g, "race", "marital", pct = "row", ci = "cell", display = "est_ci"),
  "base + moe"      = tab(g, "race", "marital", pct = "row", ci = "ref", display = "base_moe"),
  "pct (diff)"      = tab(g, "rincome", "marital", pct = "row", display = "{pct} ({diff})",
                          color = TRUE),

  # --- 15-20: numeric variables, shapes and the sd / cv asides -----------------------------------
  "mean (cv)"       = tab(g, "race", "tvhours", na = "drop_all"),
  "mean (sd)"       = tab(g, "race", "tvhours", na = "drop_all", display = "mean_sd"),
  "mean + ci"       = tab(g, "race", "tvhours", na = "drop_all", ci = "cell", display = "est_ci"),
  "numeric rows"    = tab(g, "age", "marital", pct = "row", color = TRUE),
  "shape quartiles" = tab(g, "age", "marital", pct = "row", shape = c(age = "quartiles")),
  "two col_vars"    = tab(g, "race", c("marital", "tvhours"), pct = "row", na = "drop_all",
                          color = TRUE),

  # --- 21-24: the table shapes -- several row_vars, tab_vars, a spread, levels = "first" ---------
  "several row_vars" = tab(g, c("rincome", "age"), "marital", pct = "row", color = TRUE),
  "tab_vars"         = tab(g, "rincome", "marital", tab_vars = "race", pct = "row", color = TRUE),
  "spread"           = tab(g, "rincome", "marital", tab_vars = "race", pct = "row",
                           spread_vars = "race", color = TRUE),
  "levels = first"   = tab(tea, "Sport", c("home", "work", "tearoom", "pub"), pct = "row",
                           levels = "first"),

  # --- 25-30: the regressions --------------------------------------------------------------------
  "reg binomial"    = tab_reg(g, outcome = "married", predictors = p, family = "binomial",
                              empirical = TRUE),
  "reg poisson"     = tab_reg(g, outcome = "tvhours", predictors = p, family = "poisson",
                              empirical = TRUE),
  "reg gaussian"    = tab_reg(g, outcome = "age", predictors = c("rincome", "race", "tvhours"),
                              family = "gaussian", empirical = TRUE),
  "reg log(OR)"     = tab_reg(g, outcome = "married", predictors = p, family = "binomial",
                              measure = "log", empirical = TRUE),
  "reg marginal RD" = tab_reg(g, outcome = "married", predictors = p, family = "binomial",
                              effect = "marginal", measure = "difference", empirical = TRUE),
  "reg adjustment"  = tab_reg(g, outcome = "tvhours", predictors = p, family = "poisson",
                              color = "adjustment")
)

tabs <- suppressWarnings(suppressMessages(tabs))
sheets <- substr(gsub("[^A-Za-z0-9 %()+=-]", "", names(tabs)), 1, 28)

suppressMessages(tab_export(unname(tabs), "xl", path = "~/xl_review.xlsx", replace = TRUE,
                            open = FALSE, sheets = "tabs", titles = names(tabs)))
# the html twin, to read the same 30 tables in the medium Excel is meant to match
h <- suppressMessages(tab_export(unname(tabs), "html", titles = names(tabs)))
writeLines(as.character(h), path.expand("~/xl_review.html"))

# ... and the markdown twin: the stylesheet ONCE at the top (a per-table one would repeat 30 times),
# then each table under its own heading, so the grid can be read as raw text or rendered by pandoc.
md <- purrr::imap_chr(tabs, function(t, nm) paste0(
  "\n\n## ", nm, "\n\n",
  paste(as.character(suppressMessages(
    tab_md(t, print = FALSE, css = FALSE, caption = nm))), collapse = "\n")))
writeLines(c("# tabxplor markdown review --- Phase 22f-ii", "",
             as.character(tab_css(format = "md")), md),
           path.expand("~/xl_review.md"))

# a 31st workbook: the model-check pictures, which are slow enough to keep apart
suppressMessages(tab_export(
  tab_reg(g, outcome = "married", predictors = p, family = "binomial", empirical = TRUE),
  "xl", path = "~/xl_review_checks.xlsx", replace = TRUE, open = FALSE, check = "auto"))

cli::cli_alert_success(
  "~/xl_review.xlsx, ~/xl_review.html, ~/xl_review.md, ~/xl_review_checks.xlsx")
