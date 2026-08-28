## New version 2.0.0 : first submission

Major release. The public API is retro-compatible (superseded functions and
arguments are soft-deprecated, not removed).


## Test environments
* local WSL2 Ubuntu install, R 4.6.1

* github Actions : [https://github.com/BriceNocenti/tabxplor/actions/runs/33195274382](https://github.com/BriceNocenti/tabxplor/actions/runs/33195274382)
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
