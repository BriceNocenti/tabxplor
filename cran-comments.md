## New version 1.1.1 : 


# Third submission 

Thanks to Victoria Wimmer for her comment : 
« We still see:
Please add \value to .Rd files regarding exported methods and explain
the functions results in the documentation. Please write about the
structure of the output (class) and also what the output means. (If a
function does not return a value, please document that too, e.g.
\value{No return value, called for side effects} or similar)
Missing Rd-tags:
      vec_ptype2.double.tabxplor_fmt.Rd: \value
      vec_ptype2.tabxplor_fmt.integer.Rd: \value
» 
  Answer : sorry, it was a syntax problem in Roxygen (doubled comment) :   #' #' @return


# Second submission

Thank to Benjamin Altmann for his comments : 

1) "Only undirected quotation marks in DESCRIPTION (quotations, names, software names...)." 
  Answer : taken into account.

2) "Please add \value to .Rd files regarding exported methods and explain
the functions results in the documentation. Please write about the
structure of the output (class) and also what the output means. (If a
function does not return a value, please document that too, e.g.
\value{No return value, called for side effects} or similar)
Missing Rd-tags:
      pipe.Rd: \arguments,  \value
      tbl_format_footer.tabxplor_tab.Rd: \value
      vec_cast.character.tabxplor_fmt.Rd: \value
      vec_cast.integer.tabxplor_fmt.Rd: \value
      vec_cast.tabxplor_fmt.double.Rd: \value
      vec_cast.tabxplor_fmt.tabxplor_fmt.Rd: \value
      vec_ptype2.double.tabxplor_fmt.Rd: \value
      vec_ptype2.integer.tabxplor_fmt.Rd: \value
      vec_ptype2.tabxplor_fmt.integer.Rd: \value"
  Answer : I added the value missing in those Rd-tags, and others (and arguments for pipe.Rd). 

3) "Please ensure that your functions do not write by default or in your
examples/vignettes/tests in the user's home filespace (including the
package directory and getwd()). This is not allowed by CRAN policies.
Please omit any default path in writing functions. In your
examples/vignettes/tests you can write to tempdir(). -> R/tab_xl.R"
  Answer : checked that functions/examples/vignettes/tests only write in tempdir().

# First submission
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
  
* github Actions : https://github.com/BriceNocenti/tabxplor/actions/runs/7586115180 
   - macOS 12.7.2, R-release
   - Microsoft Windows Server 2022, R-release
   - Ubuntu Linux 22.04.3 LTS, R-devel
   - Ubuntu Linux 22.04.3 LTS, R-release
   - Ubuntu Linux 22.04.3 LTS, R-oldrel-1

* https://builder.r-hub.io :
   - Windows Server 2022, R-devel, 64 bit
   https://builder.r-hub.io/status/tabxplor_1.1.1.tar.gz-001807fcc2934f4b97ea4be05f2d681c
   - Ubuntu Linux 20.04.1 LTS, R-release, GCC
   https://builder.r-hub.io/status/tabxplor_1.1.1.tar.gz-6428111f280743a6be10efb251c32797
   - Fedora Linux, R-devel, clang, gfortran
   https://builder.r-hub.io/status/tabxplor_1.1.1.tar.gz-6f051813429240b49e9d07798cea3488

* https://win-builder.r-project.org/DB48DM5FSDyS: 
   - Windows Server 2022 x64, R-devel


## R CMD check results
* local Windows 10:
    No ERRORs, no WARNINGs, no NOTEs. 

* github Actions : 
    No ERRORs, no WARNINGs, no NOTEs. 

* https://builder.r-hub.io : 
    No ERRORs, no WARNINGs. 
    
    3 NOTEs (on Windows Server 2022) : 
      - New submission + Package was archived on CRAN
      - « checking for detritus in the temp directory 
        ... Found the following files/directories: 'lastMiKTeXException' ». 
      - « checking for non-standard things in the check directory ... NOTE
        Found the following files/directories: ''NULL'' »
    I checked on the forums : the two former notes seems to come from the rhub server. 
    
    3 NOTES (Ubuntu + Fedora) : 
     - New submission + Package was archived on CRAN
     - "checking examples ... [4s/18s] NOTE
       Examples with CPU (user + system) or elapsed time > 5s
            user system elapsed
       fmt 1.626   0.06   6.985"
     - "checking HTML version of manual ... NOTE
       Skipping checking HTML validation: no command 'tidy' found"
    
    
* https://win-builder.r-project.org : 
    No ERRORs, no WARNINGs, 1 NOTE (New submission + Package was archived on CRAN).


## Downstream dependencies
The only downstream dependency is with my package `ggfacto`, which works well with the 
new version.
