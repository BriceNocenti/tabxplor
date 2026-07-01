# fmt record shape snapshot

    Code
      x <- fmt(1)
      cat("fields:\n")
    Output
      fields:
    Code
      print(vctrs::fields(x))
    Output
       [1] "n"         "display"   "digits"    "wn"        "pct"       "mean"     
       [7] "diff"      "ctr"       "var"       "ci"        "rr"        "or"       
      [13] "in_totrow" "in_tottab" "in_refrow"
    Code
      cat("\nfield types:\n")
    Output
      
      field types:
    Code
      print(vapply(vctrs::fields(x), function(f) typeof(vctrs::field(x, f)),
      character(1)))
    Output
                n     display      digits          wn         pct        mean 
        "integer" "character"   "integer"    "double"    "double"    "double" 
             diff         ctr         var          ci          rr          or 
         "double"    "double"    "double"    "double"    "double"    "double" 
        in_totrow   in_tottab   in_refrow 
        "logical"   "logical"   "logical" 
    Code
      cat("\ncolumn attributes:\n")
    Output
      
      column attributes:
    Code
      print(sort(setdiff(names(attributes(x)), c("names", "class", "row.names"))))
    Output
      [1] "ci_type"  "col_var"  "color"    "comp_all" "ref"      "refcol"   "totcol"  
      [8] "type"    

