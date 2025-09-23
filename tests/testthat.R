library(testthat)
library(tabxplor)

# # CRAN OMP THREAD LIMIT
# threads_option <- Sys.getenv("OMP_THREAD_LIMIT")
#Sys.setenv("OMP_THREAD_LIMIT" = 2)



test_check("tabxplor")

# Sys.setenv("OMP_THREAD_LIMIT" = threads_option)
