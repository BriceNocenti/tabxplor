## New version 1.2.0 : second submission

There was a "Package has a VignetteBuilder field but no prebuilt vignette index" note I
 thought was due to win-builder, but was in fact my fault : 
 corrected by removing "^build/" from .Rbuildignore.


## Test environments
* local Windows 10 install, R devel
  
* github Actions : https://github.com/BriceNocenti/tabxplor/actions/runs/10629699251
   - macOS 12.7.2, R-release
   - Microsoft Windows Server 2022, R-release
   - Ubuntu Linux 22.04.3 LTS, R-devel
   - Ubuntu Linux 22.04.3 LTS, R-release
   - Ubuntu Linux 22.04.3 LTS, R-oldrel-1

* https://win-builder.r-project.org/520eZbrsVNAA
   - Windows Server 2022 x64, R-devel


## R CMD check results
* local Windows 10:
    No ERRORs, no WARNINGs, no NOTEs. 

* github Actions : 
    No ERRORs, no WARNINGs, no NOTEs. 


* https://win-builder.r-project.org : 
    No ERRORs, no WARNINGs, no NOTEs.


## Downstream dependencies
The only downstream dependency is with my package `ggfacto`, which works well with the 
new version.

