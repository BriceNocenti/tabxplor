## Resubmission
This is a resubmission. In this version I have:

* Passed from travis-ci to github actions, which removes the URL Not Found note in the travis readme badge.

## Test environments
* local Windows 10 install, R 4.1.1 and R devel (4.2.0)
* Ubuntu 20.04.3 (on github actions), R 4.1.1

* https://builder.r-hub.io :
   - Windows Server 2008 R2 SP1, R-devel 
   (Build ID: tabxplor_1.0.0.tar.gz-85c78a10c0a24f4b8cfde40d5c13749e)
   - Ubuntu Linux 20.04.1 LTS, R-release, GCC, 
   (Build ID: tabxplor_1.0.0.tar.gz-046dcbb21a50422fb853418e09a05bc0)
   - Fedora Linux, R-devel, clang, gfortran, 
   (Build ID: tabxplor_1.0.0.tar.gz-752c05a6ddd24d98a34d45d5691592c9)

* https://win-builder.r-project.org/k582lkjFeCFd/ : 
   - Windows x86_64-w64-mingw32 (64-bit), R-devel

## R CMD check results
* local Windows 10 and Ubuntu 16.04 (on github actions) :
    There were no ERRORs, WARNINGs or NOTEs. 

* https://builder.r-hub.io : 
    No ERRORs, no WARNINGs, 1 NOTE : 
        New submission

* https://win-builder.r-project.org : 
    No ERRORs, no WARNINGs, 1 NOTE : 
        New submission

## Downstream dependencies
There are currently no downstream dependencies for this package.
