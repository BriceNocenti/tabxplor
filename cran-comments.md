## New version 2.0.1

Patch release: bug fixes, a few new arguments, and no change to the public API.

It follows 2.0.0 closely, which I would not normally do. The reason is two silent
bugs in weighted and complex-survey data, both reported after 2.0.0 was published:
a variable whose name is not syntactic (for instance `Age group`, which an SPSS or
Stata import produces routinely) made design-based tests return NA with no message,
and made a weighted regression stop with a parse error. I would rather users did not
meet them.


## Test environments
* local WSL2 Ubuntu install, R 4.6.1

* github Actions (PR merge) : [https://github.com/BriceNocenti/tabxplor/actions/runs/35279552851](https://github.com/BriceNocenti/tabxplor/actions/runs/35279552851)
  - macOS, R-release
  - Microsoft Windows Server, R-release
  - Ubuntu Linux LTS, R-devel
  - Ubuntu Linux LTS, R-release
  - Ubuntu Linux LTS, R-oldrel-1

* win-builder (R-devel): [<FILL: the https://win-builder.r-project.org/... link from the email>](<FILL>)

* rhub: [https://github.com/BriceNocenti/tabxplor/actions/runs/35313555736](https://github.com/BriceNocenti/tabxplor/actions/runs/35313555736)
  Platforms run: nold, atlas, mkl, donttest, ubuntu-next, ubuntu-release.
  (The compiler containers are not run: the package has no compiled code. The `nosuggests`
  platform is run locally instead, with `_R_CHECK_DEPENDS_ONLY_=true`.)

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
