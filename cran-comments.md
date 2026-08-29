## New version 2.0.0 : first submission

Major release. The public API is retro-compatible (superseded functions and
arguments are soft-deprecated).


## Test environments
* local WSL2 Ubuntu install, R 4.6.1

* github Actions (PR merge) : [https://github.com/BriceNocenti/tabxplor/actions/runs/33243522983](https://github.com/BriceNocenti/tabxplor/actions/runs/33243522983)
  - macOS, R-release
  - Microsoft Windows Server, R-release
  - Ubuntu Linux LTS, R-devel
  - Ubuntu Linux LTS, R-release
  - Ubuntu Linux LTS, R-oldrel-1

* win-builder (R-devel): [https://win-builder.r-project.org/BpTORb79u0GB/](https://win-builder.r-project.org/BpTORb79u0GB/)

* rhub: [https://github.com/BriceNocenti/tabxplor/actions/runs/33245122778](https://github.com/BriceNocenti/tabxplor/actions/runs/33245122778)
  Platforms run: nosuggests, nold, atlas, mkl, donttest, ubuntu-next, ubuntu-release.
  (The compiler containers are not run: the package has no compiled code.)

## R CMD check results
* local (devtools::check(manual = TRUE)):
  - No Error, no Warning, no Note.

* github Actions :
  - No Error, no Warning, no Note.

* win-builder
  - No Error, no Warning, no Note.

* rhub
  - No Error, no Warning, no Note.

## Downstream dependencies
The only downstream dependency is with my package `ggfacto`.
I made sure the last version of `ggfacto` works with this version of `tabxplor`.
