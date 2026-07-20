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
       [7] "diff"      "ratio"     "ctr"       "var"       "ci_inf"    "ci_sup"   
      [13] "pvalue"    "or"        "tot_n"     "in_totrow" "in_tottab" "in_refrow"
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
             diff       ratio         ctr         var      ci_inf      ci_sup 
         "double"    "double"    "double"    "double"    "double"    "double" 
           pvalue          or       tot_n   in_totrow   in_tottab   in_refrow 
         "double"    "double"    "double"   "logical"   "logical"   "logical" 
    Code
      cat("\ncolumn attributes:\n")
    Output
      
      column attributes:
    Code
      print(sort(setdiff(names(attributes(x)), c("names", "class", "row.names"))))
    Output
       [1] "ci_type"      "col_var"      "color"        "color_signif" "comp_all"    
       [6] "model_family" "ref"          "refcol"       "role"         "totcol"      
      [11] "type"        

