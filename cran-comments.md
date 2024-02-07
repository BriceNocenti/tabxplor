## New version 1.1.2 : first submission

Compared to 1.1.1, it's just minor bug corrections.


## Test environments
* local Windows 10 install, R devel
  
* github Actions : https://github.com/BriceNocenti/tabxplor/actions/runs/7586115180 
   - macOS 12.7.2, R-release
   - Microsoft Windows Server 2022, R-release
   - Ubuntu Linux 22.04.3 LTS, R-devel
   - Ubuntu Linux 22.04.3 LTS, R-release
   - Ubuntu Linux 22.04.3 LTS, R-oldrel-1

* https://builder.r-hub.io :
   - Windows Server 2022, R-devel, 64 bit
   https://builder.r-hub.io/status/tabxplor_1.1.2.tar.gz-211c9c0a8d4646be90c2d0754965d2ea
   - Ubuntu Linux 20.04.1 LTS, R-release, GCC
   https://builder.r-hub.io/status/tabxplor_1.1.2.tar.gz-cef17f9cba9349d2b88b4bcb7a469a3c
   - Fedora Linux, R-devel, clang, gfortran
   https://builder.r-hub.io/status/tabxplor_1.1.2.tar.gz-7474eec0175d4152a2e7e23da4558839
   
* https://win-builder.r-project.org/jRPaogf0SZ7Z: 
   - Windows Server 2022 x64, R-devel


## R CMD check results
* local Windows 10:
    No ERRORs, no WARNINGs, no NOTEs. 

* github Actions : 
    No ERRORs, no WARNINGs, no NOTEs. 

* https://builder.r-hub.io : 
    No ERRORs, no WARNINGs. 
    
    2 NOTEs (on Windows Server 2022) : 
      - « checking for detritus in the temp directory 
        ... Found the following files/directories: 'lastMiKTeXException' ». 
      - « checking for non-standard things in the check directory ... NOTE
        Found the following files/directories: ''NULL'' »
    I checked on the forums : the two former notes seems to come from the rhub server. 

    
* https://win-builder.r-project.org : 
    No ERRORs, no WARNINGs, 1 NOTE (New submission + Package was archived on CRAN).


## Downstream dependencies
The only downstream dependency is with my package `ggfacto`, which works well with the 
new version.
