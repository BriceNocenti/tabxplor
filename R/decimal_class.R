# Decimal class : print numeric with significant numbers -----------------------




#' Create a vector of class decimal
#' @description decimal vectors are easy to print with the wanted number
#' of digits in a  \code{\link[tibble]{tibble}}, and can be used in all
#' standard operations, like + or c(), using vctrs.
#' @param x A double vector. For \code{\link{decimal}} and
#' \code{\link{as_decimal}}, anything coercible to double.
#' @param digits The number of digits to print. It can then be changed
#' with \code{\link{set_digits}}.
#'
#' @return A numeric vector of class decimal.
#' @export
#'
#' @examples decimal(1.72, 1)
#' as_decimal(3.2)
decimal <- function(x = double(), digits = 2L) {
  x <- vctrs::vec_cast(x, double())
  digits <- vctrs::vec_recycle(vctrs::vec_cast(digits, integer()), 1L)

  new_decimal(x, digits = digits)
}
#Function to extract ant set digits attribute with pct class

#' @describeIn decimal A test function for class export
#' @export
is_decimal <- function(x) {
  inherits(x, "decimal")
}

#' @describeIn decimal A constructor for class export
#' @export
new_decimal <- function(x = double(), digits = 2L) {
  vctrs::vec_assert(x, ptype = double())
  vctrs::vec_assert(digits, ptype = integer(), size = 1)

  vctrs::new_vctr(x, digits = digits, class = "decimal", inherit_base_type = TRUE)
}

#The first method for every class should almost always be a format() method.
#This should return a character vector the same length as x.

#' Print method for class decimal
#'
#' @param x A decimal object.
#' @param ...
#'
#' @return The decimal printed.
#' @export
format.decimal <- function(x, ...) {
  #out <- sprintf(paste0("%-0.", get_digits(x), "f"), x) %>%
  out <- formatC(x, format = "f", big.mark = " ", digits = get_digits(x)) %>%
    stringr::str_replace("^0.0+$", "0")
  out[is.na(x)] <- NA
  out
}

#' Pillar_shaft method for class decimal
#'
#' @param x A decimal object.
#'
#' @return A decimal printed in a pillar.
#' @importFrom pillar pillar_shaft
#' @export
pillar_shaft.decimal <- function(x, ...) {
  out <- format(x) %>% stringr::str_replace("^0$", crayon::silver("0")) # 0 in gray
  #out[is.na(x)] <- ""
  pillar::new_pillar_shaft_simple(out, align = "right", na = "")
}

#Conversion functions :
#' A function to convert vectors to class decimal
#' @param x A vector coercible to double, or a character vector with numbers.
#' @param ... The number of digits as an integer, to be passed to the method.
#'
#' @export
as_decimal <- function(x, ...) {
  UseMethod("as_decimal")
}
#' @export
as_decimal.default <- function(x, ...) {
  vctrs::vec_cast(x, new_decimal())
}
# as_decimal.character <- function(x) {
#   value <- as.numeric(gsub(" *% *$", "", x)) / 100
#   new_decimal(value)
# }

#' @export
as.character.decimal <- function(x, ...) formatC(as.double(x), get_digits(x), format = "f")
#' @export
as.character.decimal <- NULL



#Abreviated printing of the class name
#' @export
vec_ptype_abbr.decimal <- function(x, ...) {
  "dec"
}
#Include number of digits in the printed name
#' @export
vec_ptype_full.decimal <- function(x, ...) {
  #paste0("decimal<", get_digits(x), ">")
  paste0("decimal", get_digits(x), "")
}

#Coerce to the maximum digits in combinations
#' @export
vec_ptype2.decimal.decimal <- function(x, y, ...) {
  new_decimal(digits = max(get_digits(x), get_digits(y)))
}
#' @export
vec_cast.decimal.decimal <- function(x, to, ...) {
  new_decimal(vctrs::vec_data(x), digits = get_digits(to))
}

#Priority to decimal over double :
#' @export
vec_ptype2.decimal.double <- function(x, y, ...) x
#' @export
vec_ptype2.double.decimal <- function(x, y, ...) y

#Priority to decimal over pct :
#' @export
vec_ptype2.decimal.pct <- function(x, y, ...) x
#' @export
vec_ptype2.pct.decimal <- function(x, y, ...) y

# vec_ptype2.pct.character <- NULL
# vec_ptype2.character.pct <- NULL

#Conversions :
#' @export
vec_cast.decimal.double  <- function(x, to, ...) new_decimal(x, digits = get_digits(to))
#' @export
vec_cast.double.decimal  <- function(x, to, ...) vctrs::vec_data(x)
#' @export
vec_cast.decimal.integer  <- function(x, to, ...) new_decimal(as.double(vctrs::vec_data(x)), digits = 0L)
#' @export
vec_cast.integer.decimal  <- function(x, to, ...) vctrs::vec_data(x) %>% as.double() %>% as.integer()
#' @export
vec_cast.character.decimal  <- function(x, to, ...) formatC(as.double(vctrs::vec_data(x)), get_digits(x), format = "f")
#' @export
vec_cast.pct.decimal  <- function(x, to, ...) new_pct(vctrs::vec_data(x), digits = get_digits(x))
#' @export
vec_cast.decimal.pct  <- function(x, to, ...) new_decimal(vctrs::vec_data(x), digits = get_digits(x)) #  + 2L ?



#Set arithmetic operations :
#' @export
vec_arith.decimal <- function(op, x, y, ...) {
  UseMethod("vec_arith.decimal", y)
}
#' @export
vec_arith.decimal.default <- function(op, x, y, ...) {
  vctrs::stop_incompatible_op(op, x, y)
}

#' @export
vec_arith.decimal.decimal <- function(op, x, y, ...) {
  new_decimal(vctrs::vec_arith_base(op, x, y),
              digits = max(get_digits(x), get_digits(y)))
  # switch(
  #   op,
  #   "+" = ,
  #   "-" = ,
  #   "/" = ,
  #   "*" = new_decimal(vctrs::vec_arith_base(op, x, y),
  #                       digits = max(get_digits(x), get_digits(y))),
  #   vctrs::stop_incompatible_op(op, x, y)
  # )
}

#' @export
vec_arith.decimal.numeric <- function(op, x, y, ...) {
  new_decimal(vctrs::vec_arith_base(op, x, y),
              digits = max(get_digits(x), get_digits(y)))
}
#' @export
vec_arith.numeric.decimal <- function(op, x, y, ...) {
  new_decimal(vctrs::vec_arith_base(op, x, y),
              digits = max(get_digits(x), get_digits(y)))
}

#' @export
vec_arith.pct.decimal <- function(op, x, y, ...) {
  new_pct(vctrs::vec_arith_base(op, x, y),
          digits = max(get_digits(x), get_digits(y) - 2L))
}

#' @export
vec_arith.decimal.pct <- function(op, x, y, ...) {
  new_pct(vctrs::vec_arith_base(op, x, y),
          digits = max(get_digits(x) - 2L, get_digits(y)))
}

#' @export
vec_arith.decimal.MISSING <- function(op, x, y, ...) { #unary + and - operators
  switch(op,
         `-` = x * - 1,
         `+` = x,
         vctrs::stop_incompatible_op(op, x, y)
  )
}

#' @export
vec_math.decimal <- function(.fn, .x, ...) {
  new_decimal(vctrs::vec_math_base(.fn, .x, ...), digits = get_digits(.x))
}


# #Tests :
# x <- decimal(runif(10), 1L)
# x
# tibble::tibble(x)
#
# #It conserves the entire number for format changes and calculations :
# decimal(1.763758943242, digits = 1) %>% `digits<-`(10)
#
# vctrs::vec_c(decimal(1/100, digits = 3), decimal(2/100, digits = 2))
#
# vctrs::vec_c(decimal(1, digits = 1), pi)
# vctrs::vec_c(pi, decimal(1, digits = 1))
#
# sum(decimal(0.5), decimal(0.5))
# decimal(3.5) + decimal(0.5)
# decimal(2.5, 1L) - decimal(0.5, 3L)
# decimal(2.5, 1L) / decimal(0.5, 3L)
# decimal(2.5, 1L) * decimal(0.5, 3L)
# decimal(3.5, 1L) + 0.5
# decimal(2.5, 1L) - 0.5
# decimal(2.5, 1L) / 0.5
# decimal(2.5, 3L) * 0.5
# decimal(0.5, 1L) + pct(0.75, 0L)
# decimal(0.5, 2L) - pct(0.75, 0L)
# decimal(0.5, 3L) / pct(0.75, 0L)
# decimal(0.5, 4L) * pct(0.75, 0L)
# +decimal(2.5, 0L)
# -decimal(2.5, 0L)
# decimal(0.5, 1L) %>% class()
#
# decimal(c(1.5, 2.5, 3.5, 17.5), 1) %>% var()

# decimal(4.5) %>% as.integer()
# as_decimal(4L)
# decimal(4.5) %>% as.character()






