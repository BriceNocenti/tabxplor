## New version 2.0.0 : first submission

Major release. The public API is retro-compatible (superseded functions and
arguments are soft-deprecated, not removed); internals were rewritten around a
single aggregation core for speed and consistency. Main user-facing additions:
regression tables (`tab_reg()`), tables from pre-aggregated counts
(`tab_counts()`), HTML export (`tab_html()`, with `tab_kable()` kept as an
alias), a shared CSS stylesheet (`tab_css()`), captions (`set_caption()`),
survey-design and effect-size test options, labelled-data support, and a
French translation of all table legends. See NEWS.md.


## Test environments
* local WSL2 Ubuntu install, R 4.6.1

* github Actions : <FILL: link to the R-CMD-check run on the release PR>
   - macOS, R-release
   - Microsoft Windows Server, R-release
   - Ubuntu Linux LTS, R-devel
   - Ubuntu Linux LTS, R-release
   - Ubuntu Linux LTS, R-oldrel-1

* rhub: <FILL: link to the rhub workflow run>

## R CMD check results
* local:
    No ERRORs, no WARNINGs, no NOTEs.

* github Actions :
    No ERRORs, no WARNINGs, no NOTEs.

* win-builder
    No ERRORs, no WARNINGs, no NOTEs.

* rhub
    No ERRORs, no WARNINGs, no NOTEs.

## Downstream dependencies
The only downstream dependency is with my package `ggfacto`.
I made sure the last version of `ggfacto` works with this version of `tabxplor`.
