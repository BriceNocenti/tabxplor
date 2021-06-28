# Create formated numbers class -----------

#Import vctrs in NAMESPACE :
#' Internal vctrs methods
#'
#' @import vctrs
#' @keywords internal
#' @name percent-vctrs
NULL

#' Create a vector of class formatted numbers.

#' @param x A double vector. For \code{\link{fmtn}} and \code{\link{as_fmtn}},
#' anything coercible to double.
#' @param digits The number of digits to print. It can then be changed
#' with \code{\link{set_digits}}.
#'
#' @return A numeric vector of class fmtn.
#' @export
#'
#' @examples num1 <- fmtn(c(0.7, 1.2, 7), type = c("n", "pct", "mean"), digits = c(0, 1, 2),
#' n = c(5, 10, 15), wn = c(4.7, 12.1, 13.9) )
#' num1
#' num1[1] + num1[2] + num1[3]
#' # To access the underlying data :
#' sum(num1[1], num1[2], num1[3]) %>% vctrs::vec_data()
#' # To access the underlying weighted counts :
#' fmtn(1, wn = 20) %>% vctrs::field("wn")



#' Create a vector of class formatted numbers.
#' @description fmtn vectors can be used to print each number with the chosen digits or as
#' percentages in a \code{\link[tablr]{tab}}, while keeping the underlying numeric data
#' for maths. It also keeps track of the counts and weighted counts beneath any percentage
#' or  mean, and can be used to calculate and print confidence intervals.
#' \code{fmtn} can use all standard operations, like +, -, sum(), or c(), using vctrs.
#' @param num A double vector. For \code{\link{fmtn}} and \code{\link{as_fmtn}},
#' anything coercible to double.
#' @param type The type of printing or background calculation, "n" for counts, "pct" for
#' percentages, "mean" for means.
#' @param digits The number of digits, as an integer or integer vector of the length of the
#' data. It can then be changed with \code{vctrs::`field<-`(x, "digits", value)}.
#' @param n The underlying count, as an integer vector of the same length, used with
#' type = "pct" to calculate confidence intervals.
#' @param wn The underlying weighted counts, as an integer vector of the same length. It
#' it used in certain operations on \code{\link{fmtn}}, like means.
#' @param sd The standards deviations, used with type = "mean" to calculate confidence
#' intervals.
#' @param ci The confidence intervals to print, if they have been calculated.
#'
#' @return A numeric vector of class fmtn.
#' @export
#'
#' @examples fmtn(0.7, "pct")
fmtn <- function(num = double(), type = "n", digits = 0L,
                 n = NA_integer_, wn = NA_real_, sd = NA_real_, ci = NA_real_) {
  num <- vec_cast(num, double()) #take anything coercible as a double
  type <- vec_recycle(vec_cast(type, character()), size = length(num))
  digits <- vec_recycle(vec_cast(digits, integer()), size = length(num))
  n  <- vec_recycle(vec_cast(n, integer()), size = length(num))
  wn <- vec_recycle(vec_cast(wn, double()), size = length(num))
  sd <- vec_recycle(vec_cast(sd, double()), size = length(num))
  ci <- vec_recycle(vec_cast(ci, double()), size = length(num))

  new_fmtn(num = num, type = type, digits = digits, n = n, wn = wn, sd = sd, ci = ci)
}



# Constructor :
#' @describeIn fmtn A constructor for class fmtn.
#' @export
new_fmtn <- function(num = double(), type = rep("n", length(num)), digits = rep(0L, length(num)),
                     n = rep(NA_integer_, length(num)), wn = rep(NA_real_, length(num)),
                     sd = rep(NA_real_, length(num)), ci = rep(NA_real_, length(num))) {
  stopifnot(all(type %in% c("n", "pct", "mean")))

  vec_assert(num, double()) #check type or size
  vec_assert(type, character())
  vec_assert(digits, integer())
  vec_assert(n, integer())
  vec_assert(wn, double())
  vec_assert(sd, double())
  vec_assert(ci, double())
  new_rcrd(list(num = num, type = type, digits = digits, n = n, wn = wn, sd = sd, ci = ci), class = "fmtn")
  #access with fields() n_fields() field() `field<-`() ; vec_data() return the tibble with all fields
}




#' @describeIn fmtn A test function for class fmtn.
#' @export
is_fmtn <- function(x) {
  inherits(x, "fmtn")
}


#' A function to convert vectors to class fmtn.
#' @param x A vector coercible to double, or a character vector with numbers.
#' @param ... The number of digits as an integer, to be passed to the method.
#'
#' @export
as_fmtn <- function(x, ...) {
  UseMethod("as_fmtn")
}

# @describeIn as_fmtn
#' @export
as_fmtn.default <- function(x, digits = rep(0L, length(x)), #type = rep("count", length(x)),
                            # n = rep(NA_integer_, length(x)), wn = rep(NA_real_, length(x)),
                            # sd = rep(NA_real_, length(x)), ci = rep(NA_real_, length(x)),
                            ...) {
  new_fmtn(vec_data(x))
}

# #' Work with number of digits for formatted numbers
# #' @param x A vector of class \code{\link{fmtn}}
# #' @return \code{\link{get_digits}} : an integer vector with the number of digits.
# #' @export
# get_digits <- function(x) as.integer(attr(x, "digits")) # Helper to extract the digits attribute.
# #' @rdname get_digits
# #' @param value The number of digits to print, as an integer.
# #' @return \code{\link{set_digits}} : a vector with the right number of digits.
# #' @export
# set_digits <- function(x, value) `attr<-`(x, "digits", vec_recycle(vec_cast(value, integer()), length(x))) # To set digits

#The first method for every class should almost always be a format() method.
#This should return a character vector the same length as x.

#' Print method for class fmtn
#'
#' @param x A fmtn object.
#' @param ... Other parameter.
#'
#' @return The fmtn printed.
#' @export
# @keywords internal
# @examples
format.fmtn <- function(x, ...) {
  #out <- formatC(signif(vec_data(x) * 100, get_digits(x))) #vec_data() correct printing problems
  #sprintf(paste0("%-0.", get_digits(x), "f"), x * 100)

  out <- field(x, "num")

  out[field(x, "type") == "pct"] <- out[field(x, "type") == "pct"] * 100

  out[!is.na(out)] <- sprintf(paste0("%-0.", field(x[!is.na(out)], "digits"), "f"), out[!is.na(out)]) %>%
    stringr::str_replace("^0.0+$|^-0.0+$", "0")
  out[field(x, "type") == "pct"] <- out[field(x, "type") == "pct"] %>% stringr::str_replace("^100.0+$", "100")

  out[is.na(out)] <- NA
  out[!is.na(out) & field(x, "type") == "pct"] <- paste0(out[!is.na(out) & field(x, "type") == "pct"], "%")
  out

  # out <- formatC(x, format = "f", big.mark = " ", digits = get_digits(x)) %>%
  #   stringr::str_replace("^0.0+$", "0")
  # out[is.na(x)] <- NA
  # out
}





#' Pillar_shaft method for class fmtn
#'
#' @param x A fmtn object.
#' @param ... Other parameter.
#'
#'
#' @return A fmtn printed in a pillar.
#' @importFrom pillar pillar_shaft
#' @export
pillar_shaft.fmtn <- function(x, ...) {
  out <- format(x) %>% stringr::str_replace("^0%$|^-0%$", crayon::silver("0%")) %>%  # 0 in gray
    stringr::str_replace("^0$|^0$", crayon::silver("0"))
  #out[is.na(x)] <- ""
  pillar::new_pillar_shaft_simple(out, align = "right", na = "")

  # pillar::new_pillar_shaft(
  #   list(deg = deg, deg_min = deg_min),
  #   width = pillar::get_max_extent(deg_min),
  #   min_width = pillar::get_max_extent(deg),
  #   class = "pillar_shaft_fmtn"
  # )
}

# format.pillar_shaft_fmtn <- function(x, width, ...) {
#   if (get_max_extent(x$deg_min) <= width) {
#     ornament <- x$deg_min
#   } else {
#     ornament <- x$deg
#   }
#
#   pillar::new_ornament(ornament, align = "right")
# }

# When methods don't work, there is still a way to convert fmtn to double :
# fmtn(0.712, 2) %>% vec_data() %>% vec_cast(double())




#Define abbreviated type name (for tibble::tibble headers)
#' @export
vec_ptype_abbr.fmtn <- function(x, ...) {
  "fmtn"
}

# #Include numbers of digits in the printed name
# #' @export
# vec_ptype_full.fmtn <- function(x, ...) {
#   #paste0("fmtn<", get_digits(x), ">")
#   paste0("fmtn", get_digits(x))
# }



#Make our fmtn class coercible with herself, and back and forth with double and integer vectors :
#' @export
vec_ptype2.fmtn.fmtn <- function(x, y, ...) new_fmtn()
#' @export
vec_ptype2.fmtn.double <- function(x, y, ...) new_fmtn() #double()
#' @export
vec_ptype2.double.fmtn <- function(x, y, ...) new_fmtn() #double()
#' @export
vec_ptype2.fmtn.integer <- function(x, y, ...) fmtn() #double()
#' @export
vec_ptype2.integer.fmtn <- function(x, y, ...) new_fmtn() #double()

# Conversions :
#' @export
vec_cast.fmtn.fmtn <- function(x, to, ...) x

#' @export
vec_cast.fmtn.double <- function(x, to, ...) new_fmtn(num = x)
#' @method vec_cast.double fmtn
#' @export
vec_cast.double.fmtn <- function(x, to, ...) field(x, "num")

#' @export
vec_cast.fmtn.integer  <- function(x, to, ...) new_fmtn(num = as.double(x))
#' @method vec_cast.integer fmtn
#' @export
vec_cast.integer.fmtn  <- function(x, to, ...) field(x, "num") %>% as.integer()

#' @method vec_cast.character fmtn
#' @export
vec_cast.character.fmtn  <- function(x, to, ...) format(x)


#Comparisons and sorting :
#' @export
vec_proxy_equal.fmtn <- function(x, ...) {
  field(x, "num")
}
#' @export
vec_proxy_compare.fmtn <- function(x, ...) {
  field(x, "num")
}

#Once you've implemented vec_ptype2() and vec_cast(), you get vec_c(), [<-, and [[<- implementations for free.
#You'll also get mostly correct behaviour for c().


#Arithmetic operations :

# Thank you very much it works perfectly (I had tried with ```@method```, but not consistently enougth to put it in the generic) !
# Just a detail : with ```vec_arith fmtn default``` , I have a "Warning: [D:\... ] @method  can have at most 2 words"
# I replaced with ```vec_arith.fmtn default``` and it worked.

#' Vec_arith method for fmtn
#' @param op Operation to do.
#'
#' @param x fmtn object.
#' @param y Second object.
#' @param ... Other parameter.
#'
#' @method vec_arith fmtn
#' @export
vec_arith.fmtn <- function(op, x, y, ...) {
  UseMethod("vec_arith.fmtn", y)
}

#' @method vec_arith.fmtn default
#' @export
vec_arith.fmtn.default <- function(op, x, y, ...) {
  vec_arith_base(op, field(x, "num"), vec_data(y))
  #stop_incompatible_op(op, x, y)
}

# positive_double <- function(n) n * sign(n)
# positive_integer <- function(n) as.integer(n * sign(n))

#' @method vec_arith.fmtn fmtn
#' @export
vec_arith.fmtn.fmtn <- function(op, x, y, ...) {
  switch(
    op,
    "+" = ,
    "-" = new_fmtn(num    = vec_arith_base(op, field(x, "num"   ), field(y, "num"   )),
                   type   = dplyr::if_else(field(x, "type"  ) == field(y, "type"),
                                           true      = field(x, "type"  ),
                                           false     = "n"                        ),
                   digits = pmax(field(x, "digits"), field(y, "digits")),
                   n      = vec_arith_base(op, field(x, "n"     ), field(y, "n"     )), #%>% positive_integer(),
                   wn     = vec_arith_base(op, field(x, "wn"    ), field(y, "wn"    )), #%>% positive_double(),
                   sd     = rep(NA_real_, length(x)),
                   ci     = rep(NA_real_, length(x))                                     ),

    "/" = ,
    "*" = new_fmtn(num    = vec_arith_base(op, field(x, "num"   ), field(y, "num"   )), #Remove multiplication ?
                   type   = dplyr::if_else(field(x, "type"  ) == field(y, "type"),
                                           true      = field(x, "type"  ),
                                           false     = "n"                         ),
                   digits = pmax(field(x, "digits"), field(y, "digits")),
                   n      = field(x, "n" ),
                   wn     = field(x, "wn"),
                   sd     = rep(NA_real_, length(x)),
                   ci     = rep(NA_real_, length(x))                                 ),
    stop_incompatible_op(op, x, y)
  )
}
#Distinguish between types of percentages c(row, col), use weight when it’s needed (pct, mean)

#' @method vec_arith.fmtn numeric
#' @export
vec_arith.fmtn.numeric <- function(op, x, y, ...) { #Just keep the number ?
  new_fmtn(num    = vec_arith_base(op, field(x, "num"), y),
           type   = field(x, "type"  ),
           digits = field(x, "digits"),
           n      = field(x, "n"     ),
           wn     = field(x, "wn"    ),
           sd     = field(x, "sd"    ),
           ci     = field(x, "ci"    )                     )
}


#' @method vec_arith.numeric fmtn
#' @export
vec_arith.numeric.fmtn <- function(op, x, y, ...) { #Just keep the number ?
  new_fmtn(num    = vec_arith_base(op, x, field(y, "num")),
           type   = field(y, "type"  ),
           digits = field(y, "digits"),
           n      = field(y, "n"     ),
           wn     = field(y, "wn"    ),
           sd     = field(y, "sd"    ),
           ci     = field(y, "ci"    )                     )
}

#' @method vec_arith.fmtn MISSING
#' @export
vec_arith.fmtn.MISSING <- function(op, x, y, ...) { #unary + and - operators
  switch(op,
         `-` = new_fmtn(num    = field(x, "num"   ) * -1,
                        type   = field(x, "type"  ),
                        digits = field(x, "digits"),
                        n      = field(x, "n"     ),
                        wn     = field(x, "wn"    ),
                        sd     = field(x, "sd"    ),
                        ci     = field(x, "ci"    )       ),
         `+` = x,
         stop_incompatible_op(op, x, y)
  )
}


#Mathematical operations :
#' @export
vec_math.fmtn <- function(.fn, .x, ...) {
  if (.fn == "sum") {
    new_fmtn(num    = vec_math_base("sum", field(.x, "num"), ...),
             type   = field(.x, "type") %>% .[1],
             digits = max(field(.x, "digits")),
             n      = vec_math_base("sum", field(.x, "n" ), ...),
             wn     = vec_math_base("sum", field(.x, "wn"), ...),
             sd     = NA_real_,
             ci     = NA_real_                                     )
  } else if (.fn == "mean") { #automatically calculate weighted mean if all is "pct" or "mean" #don’t work with pct yet (row or col) !!!
    if (( all(field(.x, "type") == "pct") | all(field(.x, "type") == "mean") ) & any(!is.na(field(.x, "wn")))) {
      new_fmtn(num    = vec_math_base("mean", field(.x, "num") * field(.x, "wn"), ...) / vec_math_base("sum", field(.x, "wn"), ...),
               type   = field(.x, "type") %>% .[1],
               digits = max(field(.x, "digits")),
               n      = vec_math_base("sum", field(.x, "n" ), ...),
               wn     = vec_math_base("sum", field(.x, "wn"), ...),
               sd     = NA_real_,
               ci     = NA_real_                                     )
    } else {
      new_fmtn(num    = vec_math_base("mean", field(.x, "num"), ...),
               type   = field(.x, "type") %>% .[1],
               digits = max(field(.x, "digits")),
               n      = vec_math_base("sum", field(.x, "n" ), ...),
               wn     = vec_math_base("sum", field(.x, "wn"), ...),
               sd     = NA_real_,
               ci     = NA_real_                                     )
    }
  } else {
    vec_math_base(.fn, field(.x, "num"), ...)
  }
  # switch(.fn,
  #        sum = ,
  #        mean = ,
  #        vec_math_base(.fn, .x, ...) )
}










# #Tests------------------------------------------------------------------------------------
# #test of class :
# fmtn(0.5) %>% class()
# fmtn(0.5) %>% is_fmtn()
# fmtn(0.5) %>% is.double()
# fmtn(0.5) %>% is.numeric()
#
# #test of printing :
# num1 <- fmtn(c(0.7, 1.2, 7), c("n", "pct", "mean"),
#              c(0,1,2) , n = c(5, 10, 15), wn = c(4.7, 12.1, 13.9) )
# num2 <- fmtn(c(0.3, 0.8, 3), c("n", "pct", "mean"),
#              c(2,1,0) , n = c(15, 10, 5), wn = c(13.9, 12.1, 4.7) )
# tibble::tibble(num1)
# fmtn(1)   %>% vec_data()
#
# #test of common type :
# vec_ptype_show(fmtn(0.255, "pct", 2), fmtn(0.988, "pct", 0))
# vec_ptype_show(fmtn(), double(), fmtn())
# vec_ptype_common(fmtn(0.255, "pct", 2), fmtn(0.988, "pct", 0))
# vec_ptype2(fmtn(0.255, "pct", 2), fmtn(0.988, "pct", 0))
#
# #test of conversion :
# vec_cast(fmtn(0.255, "pct", 2), fmtn(0.988, "pct", 0))
# vec_cast(5, fmtn())
# vec_cast(5L, fmtn())
# vec_cast(fmtn(6), double())
# vec_cast(fmtn(6), integer())
# vec_cast(fmtn(0.6005, "pct", 1), character())
# vec_cast(NA, fmtn())
#
# #test of combination :
# vec_c(fmtn(0.255, "pct", 2), fmtn(0.988, "pct", 0)) #%>% get_digits()
# c(fmtn(1), fmtn(2))
# vec_c(fmtn(3), 1)
# vec_c(fmtn(3), 1L)
# vec_c(NA, fmtn(4))
#
# #test of comparison :
# fmtn(1) == 1
# sort(c(fmtn(2), fmtn(1)))
#
# #test of arithmetics :
# (fmtn(0.5, "n"   , 0, 5 ,  5.1) + fmtn(0.25000001, "n"   , 0,  1,  1.5 )) %>% vec_data()
# (fmtn(0.5, "pct" , 1, 15, 15.1) - fmtn(0.25000001, "mean", 0,  2,  2.5 )) %>% vec_data()
# (fmtn(0.5, "pct" , 1, 15, 15.1) - fmtn(0.75000001, "mean", 0, 20, 20.5 )) %>% vec_data()
# (fmtn(0.5, "mean", 2, 25, 25.1) / fmtn(0.25000001, "pct" , 3,  3,  3.5 )) %>% vec_data()
# (fmtn(0.5, "n"   , 3, 35, 35.1) * fmtn(0.25000001, "pct" , 0,  4,  4.5 )) %>% vec_data()
# (fmtn(0.5, "pct" , 4, 45, 45.1) + 0.0077778) %>% vec_data()
# (fmtn(0.5, "mean", 3, 55, 55.1) - 1)
# (fmtn(0.5, "n"   , 2, 65, 65.1) / 2)
# fmtn(0.5, "pct"  ,-1, 75, 75.1) * 3
# fmtn(0.5) + 1
# 1 + fmtn(0.5, "pct")
# 1 - fmtn(0.5, "pct")
# 2 / fmtn(3  , "pct")
# 5 * fmtn(0.5, "n", 2)
# -fmtn(0.5, "pct")
# (fmtn(0.5) + 0.0077778) %>% as.double()
# sum(fmtn(1), fmtn(0.5)) %>% vec_data()
# mean(c(fmtn(1, "n", 2), fmtn(0.5, "n", 2))) %>% vec_data()



