## New version (with bug corrections and few added functionalities)
This is the first submission (of the second version of tabxplor to be submitted to CRAN). 

## Test environments
* local Windows 10 install, R 4.1.1 and R devel (4.2.0)
* Ubuntu 20.04.3 (on github actions), R 4.1.1

* https://builder.r-hub.io :
   - Windows Server 2008 R2 SP1, R-devel 
   (Build ID: tabxplor_1.0.2.tar.gz-15166f72c5c34bd09870e1f045a83344)
   - Ubuntu Linux 20.04.1 LTS, R-release, GCC, 
   (Build ID: tabxplor_1.0.2.tar.gz-0824bc29cc924cda9577447e6777347c)
   - Fedora Linux, R-devel, clang, gfortran, 
   (Build ID: tabxplor_1.0.2.tar.gz-99ad91f5db294701b7ca5a9a32e6d143)

* https://win-builder.r-project.org/0L279RGSAhBD/ : 
   - Windows x86_64-w64-mingw32 (64-bit), R-devel

## R CMD check results
* local Windows 10 and Ubuntu 20.04 (on github actions) :
    No ERRORs, no WARNINGs, no NOTEs. 

* https://builder.r-hub.io : 
    No ERRORs, no WARNINGs, no NOTEs.

* https://win-builder.r-project.org : 
    No ERRORs, no WARNINGs, no NOTEs.

## Downstream dependencies
The only downstream dependency is with the package `ggfacto`, which have been submitted to CRAN by myself yesterday (=> they work fine together).
