## New version 1.3.0 : 4th submission

I'm having difficulties to resolve some notes on CRAN Debian win-builder check 
"CPU time > 2.5 times elapsed time"
(which were not appearing on any github action or win-builder test). 
even when setting `Sys.setenv("OMP_THREAD_LIMIT" = 2)` 
to try avoid using more than 2 cores. 
I think they come from another package (data.table ?) 
since tabxplor doesn't use a specific parallelization setup. 
I'm retrying this submission to confirm that or not.


## Test environments
* local Windows 11 install, R devel
  
* github Actions : https://github.com/BriceNocenti/tabxplor/actions/runs/12949861764
   - macOS, R-release
   - Microsoft Windows Server, R-release
   - Ubuntu Linux LTS, R-devel
   - Ubuntu Linux LTS, R-release
   - Ubuntu Linux LTS, R-oldrel-1

* https://win-builder.r-project.org/tZY8e6F5i5kV
   - Windows Server 2022 x64, R-devel


## R CMD check results
* local Windows 11:
    No ERRORs, no WARNINGs, no NOTEs. 

* github Actions : 
    No ERRORs, no WARNINGs, no NOTEs. 

* https://win-builder.r-project.org : 
    No ERRORs, no WARNINGs, no NOTEs.


## Downstream dependencies
The only downstream dependency is with my package `ggfacto`. 
I made sure last version of `ggfacto` works with this version `tabxplor`.

