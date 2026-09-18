## New version 2.0.1

Patch release: bug fixes, a few new arguments, and no change to the public API.

## Test environments
* local WSL2 Ubuntu install, R 4.6.1

* github Actions (PR merge) : [https://github.com/BriceNocenti/tabxplor/actions/runs/35279552851](https://github.com/BriceNocenti/tabxplor/actions/runs/35279552851)
  - macOS, R-release
  - Microsoft Windows Server, R-release
  - Ubuntu Linux LTS, R-devel
  - Ubuntu Linux LTS, R-release
  - Ubuntu Linux LTS, R-oldrel-1

* win-builder (R-devel): [https://win-builder.r-project.org/AuBKwbjwP92P/](https://win-builder.r-project.org/AuBKwbjwP92P/)

* rhub: [https://github.com/BriceNocenti/tabxplor/actions/runs/35313555736](https://github.com/BriceNocenti/tabxplor/actions/runs/35313555736)
  Platforms run: nold, atlas, mkl, donttest, ubuntu-next, ubuntu-release.

## R CMD check results
* local (devtools::check(manual = TRUE, remote = TRUE, incoming = TRUE)):
  - No Error, no Warning, no Note.

* local, with `_R_CHECK_DEPENDS_ONLY_=true` (the `nosuggests` check):
  - No Error, no Warning, no Note.

* github Actions :
  - No Error, no Warning, no Note.

* win-builder
  - No Error, no Warning, no Note.

* rhub
  - No Error, no Warning, no Note.

## Downstream dependencies
The only downstream dependency is my own package `ggfacto`.
I ran `R CMD check` on `ggfacto` 0.3.2 (the CRAN version) against this version of
`tabxplor`: no Error, no Warning, and one Note which is `ggfacto`'s own (a missing
`Depends: R (>= 4.1.0)` declaration) and unrelated to `tabxplor`.
