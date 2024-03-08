## New version 1.1.3 : first submission

Compared to 1.1.2, it's just minor bug corrections.


## Test environments
* local Windows 10 install, R devel
  
* github Actions : https://github.com/BriceNocenti/tabxplor/actions/runs/7586115180 
   - macOS 12.7.2, R-release
   - Microsoft Windows Server 2022, R-release
   - Ubuntu Linux 22.04.3 LTS, R-devel
   - Ubuntu Linux 22.04.3 LTS, R-release
   - Ubuntu Linux 22.04.3 LTS, R-oldrel-1

* https://win-builder.r-project.org/jRPaogf0SZ7Z: 
   - Windows Server 2022 x64, R-devel


## R CMD check results
* local Windows 10:
    No ERRORs, no WARNINGs, no NOTEs. 

* github Actions : 
    No ERRORs, no WARNINGs, no NOTEs. 


* https://win-builder.r-project.org : 
    No ERRORs, no WARNINGs, no NOTE.


## Downstream dependencies
The only downstream dependency is with my package `ggfacto`, which works well with the 
new version.
