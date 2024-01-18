## New version 1.1.1
1 NOTE : « New submission.
         CRAN repository db overrides: X-CRAN-Comment: Archived on 2022-09-06 
         as check problems were not corrected in time. »

tabxplor was removed from CRAN in september 2022 
 because « it failed its checks in a strict Latin-1 locale, 
 e.g. under Linux using LANG=en_US.iso88591 ». 
 Since there were close to no users of the package at that time, I didn't correct it then.
 I plan to use the pakcage for statistical teaching and communicate about it, so from 
 now on I expect to keep it in CRAN and do the necessary transformations in time. 
 I checked for encoding and found no non-ASCII/non Latin-1 characters.


## Test environments
* local Windows 10 install, R devel

* https://builder.r-hub.io :
   - Windows Server 2022, R-devel, 64 bit
   (Build ID: tabxplor_1.1.1.tar.gz-117901e917fa40cb8a634b4f0b930a5a)
   - Ubuntu Linux 20.04.1 LTS, R-release, GCC
   (Build ID: tabxplor_1.1.1.tar.gz-fb162797410a4e2b823626fd9093db0c)
   - Fedora Linux, R-devel, clang, gfortran
   (Build ID: tabxplor_1.1.1.tar.gz-ff6c901f8025410e88e2d58bdb71f1b0)

* https://win-builder.r-project.org/WNdIWx9698Vl: 
   - Windows Server 2022 x64, R-devel

* https://win-builder.r-project.org/LMI113o7S0rR: 
   - Windows Server 2022 x64, R 4.3.2 (2023-10-31 ucrt)

## R CMD check results
* local Windows 10:
    No ERRORs, no WARNINGs, no NOTEs. 

* https://builder.r-hub.io : 
    No ERRORs, no WARNINGs. 
    
    3 NOTE (only on Windows Server 2022) : 
      - New submission + Package was archived on CRAN
      
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
