## New version 1.1.0
This is the first submission of this version of tabxplor.


## Test environments
* local Windows 10 install, R 4.1.2 and R devel
* Ubuntu 20.04.4 LTS (on github actions), R 4.1.3

* https://builder.r-hub.io :
   - Windows Server 2022, R-devel, 64 bit
   (Build ID: tabxplor_1.1.0.tar.gz-0cdd2a3d908a4b5c9d52b79a5f22ed28)
   - Ubuntu Linux 20.04.1 LTS, R-release, GCC
   (Build ID: tabxplor_1.1.0.tar.gz-3ea82e499ca642c88ef66323202fb3b2)
   - Fedora Linux, R-devel, clang, gfortran
   (Build ID: tabxplor_1.1.0.tar.gz-927e2978668e414d89a76f11b417ea28)

* https://win-builder.r-project.org/Td05gkys9Fcd : 
   - Windows x86_64-w64-mingw32 (64-bit), R-devel

## R CMD check results
* local Windows 10 and Ubuntu 20.04 (on github actions) :
    No ERRORs, no WARNINGs, no NOTEs. 

* https://builder.r-hub.io : 
    No ERRORs, no WARNINGs. 
    
    1 NOTE, but only on Windows Server 2022 : « checking for detritus in the temp directory 
    ... Found the following files/directories: 'lastMiKTeXException' ». 
    I checked on the forums : it seems to come from the server. 

* https://win-builder.r-project.org : 
    No ERRORs, no WARNINGs, no NOTEs.

## Downstream dependencies
The only downstream dependency is with my package `ggfacto`, which works well with the 
new version.
