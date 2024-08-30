## New version 1.2.0 : first submission

(Already submitted in the beginning of august, but it was CRAN's well deserved vacations.)


## Test environments
* local Windows 10 install, R devel
  
* github Actions : https://github.com/BriceNocenti/tabxplor/actions/runs/10252977573
   - macOS 12.7.2, R-release
   - Microsoft Windows Server 2022, R-release
   - Ubuntu Linux 22.04.3 LTS, R-devel
   - Ubuntu Linux 22.04.3 LTS, R-release
   - Ubuntu Linux 22.04.3 LTS, R-oldrel-1

* https://win-builder.r-project.org/VE1T7gUg0vH1
   - Windows Server 2022 x64, R-devel


## R CMD check results
* local Windows 10:
    No ERRORs, no WARNINGs, no NOTEs. 

* github Actions : 
    No ERRORs, no WARNINGs, no NOTEs. 


* https://win-builder.r-project.org : 
    No ERRORs, no WARNINGs, 1 NOTE : 
    - Package has a VignetteBuilder field but no prebuilt vignette index.


## Downstream dependencies
The only downstream dependency is with my package `ggfacto`, which works well with the 
new version.

