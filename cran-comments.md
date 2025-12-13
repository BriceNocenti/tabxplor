## New version 1.3.1 : first submission

Minor bug corrections (and associated Jamovi module UI change, 
not in uploaded package).


## Test environments
* local Windows 11 install, R devel
  
* github Actions : https://github.com/BriceNocenti/tabxplor/actions/runs/17969697656
   - macOS, R-release
   - Microsoft Windows Server, R-release
   - Ubuntu Linux LTS, R-devel
   - Ubuntu Linux LTS, R-release
   - Ubuntu Linux LTS, R-oldrel-1

* rhub: https://github.com/BriceNocenti/tabxplor/actions/runs/17970856766
 [CT]  clang19  [clang19]
   R Under development (unstable) (2025-01-30 r87669) on Ubuntu 22.04.5 LTS
   ghcr.io/r-hub/containers/clang19:latest
 [CT]  clang20  [clang20]
   R Under development (unstable) (2024-10-09 r87215) on Ubuntu 22.04.5 LTS
   ghcr.io/r-hub/containers/clang20:latest
 [CT]  ubuntu-clang  [r-devel-linux-x86_64-debian-clang]
   R Under development (unstable) (2025-01-31 r87670) on Ubuntu 22.04.5 LTS
   ghcr.io/r-hub/containers/ubuntu-clang:latest
 [CT]  ubuntu-gcc12  [r-devel-linux-x86_64-debian-gcc]
   R Under development (unstable) (2025-01-31 r87670) on Ubuntu 22.04.5 LTS
   ghcr.io/r-hub/containers/ubuntu-gcc12:latest
 [CT]  ubuntu-release  [r-release, r-release-linux-x86_64, ubuntu]
   R version 4.4.2 (2024-10-31) on Ubuntu 22.04.5 LTS
   ghcr.io/r-hub/containers/ubuntu-release:latest

## R CMD check results
* local Windows 11:
    No ERRORs, no WARNINGs, no NOTEs. 

* github Actions : 
    No ERRORs, no WARNINGs, no NOTEs. 

* win-builder
    No ERRORs, no WARNINGs, no NOTEs.

* rhub
    No ERRORs, no WARNINGs, no NOTEs.

## Downstream dependencies
The only downstream dependency is with my package `ggfacto`. 
I made sure last version of `ggfacto` works with this version of `tabxplor`.

