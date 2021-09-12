## Resubmission
This is a second resubmission. In this version I have, as suggested by Uwe Ligges and Julia Haider (thanks to them) :

* Added `\value` and parameters to .Rd files regarding exported methods, and to functions with missing Rd-tags.

* Replaced `\dontrun{}` by `\donttest` when used in examples that can in fact be executed (tab_xl.Rd), since the 5 seconds limit is nearly reached.

* Ensured functions do not write by default or in examples/vignettes/tests in the user's home filespace, by using `tempdir()` (in `tab_xl`, which exports tables to Excel, and in tests and examples for this function).

* Passed from travis-ci to github actions, which removes the "URL Not Found" note in the travis readme badge.

## Test environments
* local Windows 10 install, R 4.1.1 and R devel (4.2.0)
* Ubuntu 20.04.3 (on github actions), R 4.1.1

* https://builder.r-hub.io :
   - Windows Server 2008 R2 SP1, R-devel 
   (Build ID: tabxplor_1.0.1.tar.gz-c68138b508ec4799a5d205b0c494328c)
   - Ubuntu Linux 20.04.1 LTS, R-release, GCC, 
   (Build ID: tabxplor_1.0.1.tar.gz-bbcf056bfae74d71b713b325c2ff1b60)
   - Fedora Linux, R-devel, clang, gfortran, 
   (Build ID: tabxplor_1.0.1.tar.gz-2cb8f9d1de234aec8f85a01c6f71a946)

* https://win-builder.r-project.org/k582lkjFeCFd/ : 
   - Windows x86_64-w64-mingw32 (64-bit), R-devel

## R CMD check results
* local Windows 10 and Ubuntu 20.04 (on github actions) :
    There were no ERRORs, WARNINGs or NOTEs. 

* https://builder.r-hub.io : 
    No ERRORs, no WARNINGs, 1 NOTE : 
        New submission

* https://win-builder.r-project.org : 
    No ERRORs, no WARNINGs, 1 NOTE : 
        New submission

## Downstream dependencies
There are currently no downstream dependencies for this package.
