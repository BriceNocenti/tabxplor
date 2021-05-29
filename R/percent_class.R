# Create percent class (https://vctrs.r-lib.org/articles/pillar.html)-----------

# Rename class tablr_percent ? -------------------------------------------------
# Create class numtab / numt, with a "type" attribute to print like pct ? ------

#Import vctrs in NAMESPACE :
#' Internal vctrs methods
#'
#' @import vctrs
#' @keywords internal
#' @name percent-vctrs
NULL



#' Create a vector of class pct.
#' @description pct vectors are easy to print as percentages in a
#' \code{\link[tibble]{tibble}}, and can be used in all standard operations,
#' like + or c(), using vctrs.
#' @param x A double vector. For \code{\link{pct}} and \code{\link{as_pct}},
#' anything coercible to double.
#' @param digits The number of digits to print. It can then be changed
#' with \code{\link{set_digits}}.
#'
#' @return A numeric vector of class pct.
#' @export
#'
#' @examples pct(0.7, 1)
#' as_pct("80%")
pct <- function(x = double(), digits = 0L) {
  x <- vctrs::vec_cast(x, double()) #take anything coercible as a double
  digits <- vctrs::vec_recycle(vctrs::vec_cast(digits, integer()), 1L)
  new_pct(x, digits)
}

# Constructor :
#' @describeIn pct A constructor for class pct.
#' @export
new_pct <- function(x = double(), digits = 0L) {
  vctrs::vec_assert(x, double()) #check type or size
  vctrs::vec_assert(digits, ptype = integer(), size = 1)
  vctrs::new_vctr(x, digits = digits, class = "pct", inherit_base_type = TRUE) #"vctrs_pct"
  }

#' @describeIn pct A test function for class pct.
#' @export
is_pct <- function(x) {
  inherits(x, "pct")
}


#' A function to convert vectors to class pct.
#' @param x A vector coercible to double, or a character vector with numbers.
#' @param ... The number of digits as an integer, to be passed to the method.
#'
#' @export
as_pct <- function(x, ...) {
  UseMethod("as_pct")
}

# @describeIn as_pct
#' @export
as_pct.default <- function(x, digits = 0L, ...) {
  digits <- vctrs::vec_recycle(vctrs::vec_cast(digits, integer()), 1L)
  vctrs::vec_cast(x, new_pct(digits = digits))
  #vctrs::vec_cast(x, new_pct())
}

# @describeIn as_pct there must be numbers in the vector
#' @export
as_pct.character <- function(x, digits = 0L, ...) {
  digits <- vctrs::vec_recycle(vctrs::vec_cast(digits, integer()), 1L)
  value <- as.numeric(gsub(" *% *$", "", x)) / 100
  new_pct(value, digits)
}

#' Work with number of digits
#' @param x A vector of class \code{\link{pct}} or \code{\link{decimal}}
#' @return \code{\link{get_digits}} : an integer vector with the number of digits.
#' @export
get_digits <- function(x) as.integer(attr(x, "digits")) # Helper to extract the digits attribute.
#' @rdname get_digits
#' @param value The number of digits to print, as an integer.
#' @return \code{\link{set_digits}} : a vector with the right number of digits.
#' @export
set_digits <- function(x, value) `attr<-`(x, "digits", as.integer(value)) # To set digits



#The first method for every class should almost always be a format() method.
#This should return a character vector the same length as x.

#' Print method for class pct
#'
#' @param x A pct object.
#' @param ... Other parameter.
#'
#' @return The pct printed.
#' @export
# @keywords internal
# @examples
format.pct <- function(x, ...) {
  #out <- formatC(signif(vctrs::vec_data(x) * 100, get_digits(x))) #vec_data() correct printing problems
  #sprintf(paste0("%-0.", get_digits(x), "f"), x * 100)
  out <- sprintf(paste0("%-0.", get_digits(x), "f"), vctrs::vec_data(x) * 100) %>%
    stringr::str_replace("^0.0+$|^-0.0+$", "0") %>% stringr::str_replace("^100.0+$", "100")
  out[is.na(x)] <- NA
  out[!is.na(x)] <- paste0(out[!is.na(x)], "%")
  out
}




#' Pillar_shaft method for class pct
#'
#' @param x A pct object.
#' @param ... Other parameter.
#'
#'
#' @return A pct printed in a pillar.
#' @importFrom pillar pillar_shaft
#' @export
pillar_shaft.pct <- function(x, ...) {
  out <- format(x) %>% stringr::str_replace("^0%$|^-0%$", crayon::silver("0%")) # 0 in gray
  #out[is.na(x)] <- ""
  pillar::new_pillar_shaft_simple(out, align = "right", na = "")

  # pillar::new_pillar_shaft(
  #   list(deg = deg, deg_min = deg_min),
  #   width = pillar::get_max_extent(deg_min),
  #   min_width = pillar::get_max_extent(deg),
  #   class = "pillar_shaft_pct"
  # )
}

# format.pillar_shaft_pct <- function(x, width, ...) {
#   if (get_max_extent(x$deg_min) <= width) {
#     ornament <- x$deg_min
#   } else {
#     ornament <- x$deg
#   }
#
#   pillar::new_ornament(ornament, align = "right")
# }

# When methods don't work, there is still a way to convert pct to double :
# pct(0.712, 2) %>% vctrs::vec_data() %>% vctrs::vec_cast(double())




#Define abbreviated type name (for tibble::tibble headers)
#' @export
vec_ptype_abbr.pct <- function(x, ...) {
  "pct"
}
#Include numbers of digits in the printed name
#' @export
vec_ptype_full.pct <- function(x, ...) {
  #paste0("pct<", get_digits(x), ">")
  paste0("pct", get_digits(x))
}

#Coerce to the maximum digits in combinations
#' @export
vec_ptype2.pct.pct <- function(x, y, ...) new_pct(digits = max(get_digits(x), get_digits(y)))
#' @export
vec_cast.pct.pct <- function(x, to, ...) new_pct(vctrs::vec_data(x), digits = get_digits(to))
#Make our pct class coercible with himself, and back and forth with double vectors :
# Combination (pct are kept in priority) :
#' @export
vec_ptype2.pct.double <- function(x, y, ...) x #double()
#' @export
vec_ptype2.double.pct <- function(x, y, ...) y #double()

# Conversion : doubles need "0.05" style, integers need "5", and characters "5%"

#' @export
vec_cast.pct.double <- function(x, to, ...) new_pct(x, digits = get_digits(to))
#' @method vec_cast.double pct
#' @export
vec_cast.double.pct <- function(x, to, ...) vctrs::vec_data(x)

#' @export
vec_cast.pct.integer  <- function(x, to, ...) new_pct(as.double(vctrs::vec_data(x)/100), digits = 0L)
#' @method vec_cast.integer pct
#' @export
vec_cast.integer.pct  <- function(x, to, ...) (vctrs::vec_data(x) * 100) %>% as.double() %>% as.integer()
#' @method vec_cast.character pct
#' @export
vec_cast.character.pct  <- function(x, to, ...) formatC(as.double(vctrs::vec_data(x)*100), get_digits(x), format = "f") #paste0(, "%")
#vec_cast.pct.character  <- function(x, to, ...) formatC(as.double(vctrs::vec_data(x)*100), get_digits(x), format = "f") #paste0(, "%")


#Once you've implemented vec_ptype2() and vec_cast(), you get vec_c(), [<-, and [[<- implementations for free.
#You'll also get mostly correct behaviour for c().


# Thank you very much it works perfectly (I had tried with ```@method```, but not consistently enougth to put it in the generic) !
# Just a detail : with ```vec_arith pct default``` , I have a "Warning: [D:\... ] @method  can have at most 2 words"
# I replaced with ```vec_arith.pct default``` and it worked.

#' Vec_arith method for pct
#' @param op Operation to do.
#'
#' @param x Pct object.
#' @param y Second object.
#' @param ... Other parameter.
#'
#' @method vec_arith pct
#' @export
vec_arith.pct <- function(op, x, y, ...) {
  UseMethod("vec_arith.pct", y)
}

#' @method vec_arith.pct default
#' @export
vec_arith.pct.default <- function(op, x, y, ...) {
  # new_pct(vctrs::vec_arith_base(op, x, y),
  #         digits = max(get_digits(x), get_digits(y)))
  vctrs::stop_incompatible_op(op, x, y)
}

#vctrs:::vec_arith.default # stop_incompatible_op(op, x, y)


#' @method vec_arith.pct pct
#' @export
vec_arith.pct.pct <- function(op, x, y, ...) {
  # new_pct(vctrs::vec_arith_base(op, x, y),
  #         digits = max(get_digits(x), get_digits(y)))
  switch(
    op,
    "+" = ,
    "-" = ,
    "/" = ,
    "*" = new_pct(vctrs::vec_arith_base(op, x, y)),
    vctrs::stop_incompatible_op(op, x, y)
  )
}

#' @method vec_arith.pct numeric
#' @export
vec_arith.pct.numeric <- function(op, x, y, ...) {
  new_pct(vctrs::vec_arith_base(op, x, y),
          digits = max(get_digits(x), get_digits(y)))
}


#' @method vec_arith.numeric pct
#' @export
vec_arith.numeric.pct <- function(op, x, y, ...) {
  new_pct(vctrs::vec_arith_base(op, x, y),
          digits = max(get_digits(x), get_digits(y)))
}

#' @method vec_arith.pct MISSING
#' @export
vec_arith.pct.MISSING <- function(op, x, y, ...) { #unary + and - operators
  switch(op,
         `-` = x * - 1,
         `+` = x,
         vctrs::stop_incompatible_op(op, x, y)
  )
}

#' @export
vec_math.pct <- function(.fn, .x, ...) {
  new_pct(vctrs::vec_math_base(.fn, .x, ...),
          digits = get_digits(.x)) #max(get_digits(x), get_digits(y))
  # switch(.fn,
  #        sum = ,
  #        mean = ,
  #        vctrs::vec_math_base(.fn, .x, ...) )
}



# x <- pct(c(seq(0, 1, length.out = 4), NA), 2)
# x
# tibble::tibble(x)
# #test de combinaison  :
# vctrs::vec_ptype_show(pct(), double(), pct())
# vctrs::vec_c(pct(0.5), 1)
# vctrs::vec_c(NA, pct(0.5))
# #test de conversion :
# vctrs::vec_cast(0.5, pct())
# vctrs::vec_cast(pct(0.5), double())
# c(pct(0.5), pct(1))
#
# pct(0.5, 0L) + pct(0.25000001)
# pct(0.5, 1L) - pct(0.25000001)
# pct(0.5, 2L) / pct(0.25000001)
# pct(0.5, 3L) * pct(0.25000001)
# pct(0.5, 4L) + 0.0077778
# pct(0.5, 5L) - 1
# pct(0.5, 0L) / 2
# pct(0.51778, -1) * 3 #Not working
# 1 + pct(0.5)
# 1 - pct(0.5)
# 2 / pct(0.5)
# 5 * pct(0.5)
# -pct(0.5)
# (pct(0.5) + 0.0077778) %>% as.double
# tibble::tibble(pct(0.5))
# sum(pct(1), pct(0.5))
# mean(pct(1), pct(0.5))
# #pct(1) == 1 #also work
#
# pct(0.5) %>% class()
# pct(0.555, 2L) %>% attributes()
# pct(0.5) %>% is_pct()
# pct(0.5) %>% is.double()
# pct(0.5) %>% is.numeric()
#
# as_pct(4)
# pct(0.045) %>% as.integer()
# pct(0.045) %>% as.character()
# pct(0.045, 1) %>% as.character()
# as_pct("5.541%", 3)




