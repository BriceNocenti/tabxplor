## New version 1.2.1 : second submission

Bug corrections and minor feature enhancements. 

The first submission broke the reverse dependency `ggfacto`. I made sure it now works 
 with both this version of `tabxplor` and the formers. 


## Test environments
* local Windows 10 install, R devel
  
* github Actions : https://github.com/BriceNocenti/tabxplor/actions/runs/11131328619
   - macOS 12.7.2, R-release
   - Microsoft Windows Server 2022, R-release
   - Ubuntu Linux 22.04.3 LTS, R-devel
   - Ubuntu Linux 22.04.3 LTS, R-release
   - Ubuntu Linux 22.04.3 LTS, R-oldrel-1

* https://win-builder.r-project.org/2K2pHd339tgU
   - Windows Server 2022 x64, R-devel


## R CMD check results
* local Windows 10:
    No ERRORs, no WARNINGs, no NOTEs. 

* github Actions : 
    No ERRORs, no WARNINGs, no NOTEs. 


* https://win-builder.r-project.org : 
    No ERRORs, no WARNINGs, no NOTEs.


## Downstream dependencies
The only downstream dependency is with my package `ggfacto`. I made sure it works with 
both this version of `tabxplor` and the formers. 

