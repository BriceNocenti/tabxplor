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

* win-builder (R-devel): <FILL: link to the win-builder result>

* rhub: <FILL: link to the rhub workflow run>
  Platforms run: nosuggests, nold, atlas, mkl, donttest, ubuntu-next, ubuntu-release.
  The compiler containers (clang*, gcc*, lto, *-asan, valgrind, rchk) are not run: the
  package has no compiled code, so they exercise a toolchain it never uses.

## R CMD check results
* local (devtools::check(manual = TRUE)):
    <FILL after the run>

* github Actions :
    <FILL after the run>

* win-builder
    <FILL after the run>

* rhub
    <FILL after the run>

## Downstream dependencies
The only downstream dependency is with my package `ggfacto`.
I made sure the last version of `ggfacto` works with this version of `tabxplor`.
