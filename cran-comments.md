## New version (with bug corrections and few added functionalities)
This is the first submission (of the second version of tabxplor to be submitted to CRAN). 

## Test environments
* local Windows 10 install, R 4.1.2 and R devel (4.3.0)
* Ubuntu 20.04.3 (on github actions), R 4.1.1

* https://builder.r-hub.io : (version name was wrong but content was right)
   - Windows Server 2022, R-devel, 64 bit
   (Build ID: tabxplor_1.0.2.9002.tar.gz-a1195122ee0441f2b28584bc9eef2f2c)
   - Ubuntu Linux 20.04.1 LTS, R-release, GCC
   (Build ID: tabxplor_1.0.2.9002.tar.gz-a70fe5b1fe494d56a85887d0c05da3cd)
   - Fedora Linux, R-devel, clang, gfortran
   (Build ID: tabxplor_1.0.2.9002.tar.gz-d932174a226649d180e9a03c012e8856)

* https://win-builder.r-project.org/0L279RGSAhBD/ : 
   - Windows x86_64-w64-mingw32 (64-bit), R-release

## R CMD check results
* local Windows 10 and Ubuntu 20.04 (on github actions) :
    No ERRORs, no WARNINGs, no NOTEs. 

* https://builder.r-hub.io : 
    No ERRORs, no WARNINGs, no NOTEs.

* https://win-builder.r-project.org : 
    No ERRORs, no WARNINGs, no NOTEs.

## Downstream dependencies
The only downstream dependency is with my package `ggfacto`, which works well with the 
new version.
