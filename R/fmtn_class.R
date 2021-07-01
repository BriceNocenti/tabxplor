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
#' @param pct A double vector. For \code{\link{fmtn}} and \code{\link{as_fmtn}},
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
fmtn <- function(type      = "n",
                 n         = integer(),
                 wn        = rep(NA_real_, length(n)),
                 pct       = rep(NA_real_, length(n)),
                 digits    = rep(0L      , length(n)),
                 ctr       = rep(NA_real_, length(n)),
                 mean      = rep(NA_real_, length(n)),
                 sd        = rep(NA_real_, length(n)),
                 ci        = rep(NA_real_, length(n)),
                 in_totrow = rep(FALSE, length(n)),
                 in_tottab = rep(FALSE, length(n)),
                 col_var   = NA_character_,
                 total_col = FALSE,
                 pct_type  = NA_character_) {

  max_size <- list(n, wn, pct, digits, ctr, mean, sd, ci) %>% #type
    purrr::map_int(length) %>% max()

  type   <- vec_recycle(vec_cast(type  , character()), size = max_size)
  n      <- vec_recycle(vec_cast(n     , integer())  , size = max_size)
  wn     <- vec_recycle(vec_cast(wn    , double())   , size = max_size) #anything coercible as a double
  pct    <- vec_recycle(vec_cast(pct   , double())   , size = max_size)
  digits <- vec_recycle(vec_cast(digits, integer())  , size = max_size)
  ctr    <- vec_recycle(vec_cast(ctr   , double())   , size = max_size)
  mean   <- vec_recycle(vec_cast(mean  , double())   , size = max_size)
  sd     <- vec_recycle(vec_cast(sd    , double())   , size = max_size)
  ci     <- vec_recycle(vec_cast(ci    , double())   , size = max_size)

  in_totrow <- vec_recycle(vec_cast(in_totrow, logical()), size = max_size)
  in_tottab <- vec_recycle(vec_cast(in_tottab, logical()), size = max_size)

  col_var   <- vec_recycle(vec_cast(col_var  , character()), size = 1)
  total_col <- vec_recycle(vec_cast(total_col, logical()  ), size = 1)
  pct_type  <- vec_recycle(vec_cast(pct_type , character()), size = 1)

  new_fmtn(type = type, n = n, wn = wn, pct = pct, digits = digits,
           ctr = ctr, mean = mean, sd = sd, ci = ci,
           in_totrow = in_totrow, in_tottab = in_tottab,
           col_var = col_var, total_col = total_col, pct_type = pct_type)
}



# Constructor :
#' @describeIn fmtn A constructor for class fmtn.
#' @export
new_fmtn <- function(type      = "n",
                     n         = integer(),
                     wn        = rep(NA_real_, length(n)),
                     pct       = rep(NA_real_, length(n)),
                     digits    = rep(0L      , length(n)),
                     ctr       = rep(NA_real_, length(n)),
                     mean      = rep(NA_real_, length(n)),
                     sd        = rep(NA_real_, length(n)),
                     ci        = rep(NA_real_, length(n)),
                     in_totrow = rep(FALSE   , length(n)),
                     in_tottab = rep(FALSE   , length(n)),
                     col_var   = NA_character_,
                     total_col = FALSE,
                     pct_type  = NA_character_) {
   stopifnot(
    all(type %in% c("n", "wn", "pct", "pct_ci", "ctr", "mean", "mean_ci", "sd", "ci")),
    pct_type %in% c("row", "col", "all", "all_tabs", "mixed", NA_character_)
    )

  # list(type, n, wn, pct, digits, ctr, mean, sd, ci, col_var, total_col, pct_type) %>%
  #   purrr::map(print)
  # cat("\n")

  vec_assert(type, character()) #check type or size
  type <- vec_recycle(type, size = length(n))
  vec_assert(n     , integer())
  vec_assert(wn    , double())
  vec_assert(pct   , double())
  vec_assert(digits, integer())
  vec_assert(ctr   , double())
  vec_assert(mean  , double())
  vec_assert(sd    , double())
  vec_assert(ci    , double())

  vec_assert(in_totrow, logical())
  vec_assert(in_tottab, logical())

  vec_assert(col_var  , character(), size = 1)
  vec_assert(total_col, logical()  , size = 1)
  vec_assert(pct_type , character(), size = 1)

  new_rcrd(list(type = type, n = n, wn = wn, pct = pct, digits = digits,
                ctr = ctr, mean = mean, sd = sd, ci = ci,
                in_totrow = in_totrow, in_tottab = in_tottab),
           col_var = col_var, total_col = total_col, pct_type = pct_type, class = "fmtn")
  #access with fields() n_fields() field() `field<-`() ; vec_data() return the tibble with all fields
}

fmtn0 <- function(type = "n", digits = 0) {
  new_fmtn(type = type, n = 0L, digits = as.integer(digits))
  # switch (type,
  #   "n"       = new_fmtn(type = type, n = 0L,                           digits = as.integer(digits)),
  #   "wn"      = new_fmtn(type = type, n = 0L, wn = 0,                   digits = as.integer(digits)),
  #   "pct"     = ,
  #   "pct_ci"  = new_fmtn(type = type, n = 0L, wn = 0, pct = 0,          digits = as.integer(digits)),
  #   "ctr"     = new_fmtn(type = type, n = 0L, wn = 0, pct = 0, ctr = 0, digits = as.integer(digits)),
  #   "mean"    = ,
  #   "mean_ci" = new_fmtn(type = type, n = 0L, wn = 0, mean = 0, sd = 0, digits = as.integer(digits)),
  #   "sd"      = new_fmtn(type = type, n = 0L, wn = 0, mean = 0, sd = 0, digits = as.integer(digits)),
  #   "ci"      = new_fmtn(type = type, n = 0L, ci = 0,                   digits = as.integer(digits)),
  # )
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

# # @describeIn as_fmtn
# #' @export
# as_fmtn.default <- function(x, digits = rep(0L, length(x)), #type = rep("count", length(x)),
#                             # n = rep(NA_integer_, length(x)), wn = rep(NA_real_, length(x)),
#                             # sd = rep(NA_real_, length(x)), ci = rep(NA_real_, length(x)),
#                             ...) {
#   new_fmtn(vec_data(x))
# }



# Functions to work with formatted numbers------------------------------------------------
# to get and set the different fields directly

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

fmt_field_factory <- function(x) {
  function(fmtn) field(fmtn, x)
}
get_type   <- fmt_field_factory("type")
get_n      <- fmt_field_factory("n")
get_wn     <- function(fmtn) { #If there is no weighted counts, take counts
  out <- field(fmtn, "wn")
  if (any(is.na(out))) {
    counts <- field(fmtn, "n") %>% as.double()
    out[is.na(out)] <- counts[is.na(out)]
  }
  out
}
get_pct    <- fmt_field_factory("pct")
#get_pct_ci <- function(fmtn) field("pct")
get_digits <- fmt_field_factory("digits")
get_ctr    <- fmt_field_factory("ctr")
get_mean   <- fmt_field_factory("mean")
get_sd     <- fmt_field_factory("sd")
get_ci     <- fmt_field_factory("ci")
is_in_totrow  <- fmt_field_factory("in_totrow")
is_in_tottab  <- fmt_field_factory("in_tottab")

c(fmtn("n", 0), fmtn("n", 0)) %>% in_tottab()

get_col_var   <- purrr::attr_getter("col_var")
get_total_col <- purrr::attr_getter("total_col")
get_pct_type  <- purrr::attr_getter("pct_type")

tab_get_col_var   <- function(tabs) purrr::map(tabs, ~ get_col_var(.)  ) %>%
  purrr::map_if(purrr::map_lgl(., is.null), ~ NA_character_) %>% purrr::flatten_chr()
tab_get_total_col <- function(tabs) purrr::map(tabs, ~ get_total_col(.))  %>%
  purrr::map_if(purrr::map_lgl(., is.null), ~ FALSE) %>% purrr::flatten_lgl()
tab_get_pct_type  <- function(tabs) purrr::map(tabs, ~ get_pct_type(.) ) %>%
  purrr::map_if(purrr::map_lgl(., is.null), ~ NA_character_) %>% purrr::flatten_chr()

tab_is_fmtn <- function(tabs) purrr::map_lgl(tabs, is_fmtn)

#At variable level ?
is_row_pct <- function(tabs) {
  tidyr::replace_na(tab_get_pct_type(tabs) == "row", FALSE)
}
is_col_pct <- function(tabs) {
  tidyr::replace_na(tab_get_pct_type(tabs) == "col", FALSE)
}
is_all_pct <- function(tabs) {
  tidyr::replace_na(tab_get_pct_type(tabs) == "all", FALSE)
}
is_all_tabs_pct <- function(tabs) {
  tidyr::replace_na(tab_get_pct_type(tabs) == "all_tabs", FALSE)
}

fmt_set_field_factory <- function(x) {
  function(fmtn, value) {
    value <- vec_recycle(value, size = length(fmtn))
    `field<-`(fmtn, x, value)
  }
}
set_type   <- fmt_set_field_factory("type")
set_n      <- fmt_set_field_factory("n")
set_wn     <- fmt_set_field_factory("wn")
set_pct    <- fmt_set_field_factory("pct")
set_digits <- fmt_set_field_factory("digits")
set_ctr    <- fmt_set_field_factory("ctr")
set_mean   <- fmt_set_field_factory("mean")
set_sd     <- fmt_set_field_factory("sd")
set_ci     <- fmt_set_field_factory("ci")

fmt_set_attributes_factory <- function(attr) {
  function(fmtn, value) {
    #value <- vec_recycle(value, size = 1)
    `attr<-`(fmtn, attr, value)
  }
}
set_col_var   <- fmt_set_attributes_factory("col_var"  )
set_total_col <- fmt_set_attributes_factory("total_col")
set_pct_type  <- fmt_set_attributes_factory("pct_type" )

fmt_is_type_factory <- function(x, y = character()) {
  function(fmtn) {
    if (is_fmtn(fmtn)) {
      out <- get_type(fmtn) %in% c(x, y)
    } else {
      out <- rep(FALSE, length(fmtn))
    }
    out
  }
}
is_n      <- fmt_is_type_factory("n"              )
is_wn     <- fmt_is_type_factory("wn"             )
is_pct    <- fmt_is_type_factory("pct", "pct_ci"  )
is_pct_ci <- fmt_is_type_factory("pct_ci"         )
is_ctr    <- fmt_is_type_factory("ctr"            )
is_mean   <- fmt_is_type_factory("mean", "mean_ci")
is_mean_ci<- fmt_is_type_factory("mean_ci"        )
is_sd     <- fmt_is_type_factory("sd"             )
is_ci     <- fmt_is_type_factory("ci"             )

get_num <- function(x) {
  out  <- get_n(x)
  type <- get_type(x)
  nas <- is.na(type)
  out[!nas & type == "wn"     ] <- get_wn  (x)[!nas & type == "wn"     ]
  out[!nas & type == "pct"    ] <- get_pct (x)[!nas & type == "pct"    ]
  out[!nas & type == "pct_ci" ] <- get_pct (x)[!nas & type == "pct_ci" ]
  out[!nas & type == "ctr"    ] <- get_ctr (x)[!nas & type == "ctr"    ]
  out[!nas & type == "mean"   ] <- get_mean(x)[!nas & type == "mean"   ]
  out[!nas & type == "mean_ci"] <- get_mean(x)[!nas & type == "mean_ci"]
  out[!nas & type == "sd"     ] <- get_sd  (x)[!nas & type == "sd"     ]
  out[!nas & type == "ci"     ] <- get_ci  (x)[!nas & type == "ci"     ]
  #out[is.na(get_type(x))      ] <- rep(NA_character_, length(out[is.na(get_type(x))]))
  out
  }

set_num <- function(x, value) {
  out <- set_n(x, value)
  type <- get_type(x)
  nas <- is.na(type)
  out[!nas & type == "wn"  ] <- set_wn  (x, value)[!nas & type == "wn"  ]
  out[!nas & type == "pct" ] <- set_pct (x, value)[!nas & type == "pct" ]
  out[!nas & type == "ctr" ] <- set_ctr (x, value)[!nas & type == "ctr" ]
  out[!nas & type == "mean"] <- set_mean(x, value)[!nas & type == "mean"]
  out[!nas & type == "sd"  ] <- set_sd  (x, value)[!nas & type == "sd"  ]
  out[!nas & type == "ci"  ] <- set_ci  (x, value)[!nas & type == "ci"  ]
  out
  }


#Methods for formatted numbers -----------------------------------------------------------
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

  out    <- get_num(x)
  na_out <- is.na(out)

  type <- get_type(x)
  nas  <- is.na(type)
  pct_or_ci     <- !na_out & !nas & type %in% c("pct", "pct_ci", "ci")
  pct_or_pct_ci <- !na_out & !nas & type %in% c("pct", "pct_ci")
  pct_ci_only   <- !na_out & !nas & type == "pct_ci"
  n_wn          <- !na_out & !nas & type %in% c("n", "wn", "mean", "mean_ci", "sd")
  type_ci       <- !na_out & !nas & type == "ci"

  out[pct_or_ci] <- out[pct_or_ci] * 100
  out[!na_out] <- sprintf(paste0("%-0.", get_digits(x[!na_out]), "f"), out[!na_out]) %>%
    stringr::str_replace("^0.0+$|^-0.0+$", "0")
  out[n_wn] <- out[n_wn] %>% prettyNum(big.mark = " ", preserve.width = "individual")
  out[pct_or_pct_ci] <- out[pct_or_pct_ci] %>% stringr::str_replace("^100.0+$", "100")
  out[na_out] <- NA
  out[pct_or_pct_ci] <- paste0(out[pct_or_pct_ci], "%")

  if (any(pct_ci_only)) {
    pct_conf_int <- get_ci(x)
    pct_conf_int_pct_ci <-
      paste0(" ±",
             sprintf(paste0("%-0.", get_digits(x[pct_ci_only]) + 1, "f"),
                     pct_conf_int[pct_ci_only] * 100)) %>%
      stringr::str_remove("^ ±0$|^ ±0.0+$|^ ±-0.0+$") %>%
      stringr::str_pad(max(stringr::str_length(.)))

    out[pct_ci_only] <- paste0(out[pct_ci_only], pct_conf_int_pct_ci)
  }

  out[type == "ci" & out == 0] <- NA
  out[type_ci] <- paste0("±", out[type_ci], "%")

  out

  # out <- formatC(x, format = "f", big.mark = " ", digits = get_digits(x)) %>%
  #   stringr::str_replace("^0.0+$", "0")
  # out[is.na(x)] <- NA
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
  out <- format(x)
  na_out <- is.na(out)

  out[!na_out] <- out[!na_out] %>%
    stringr::str_replace("^0%$|^-0%$", crayon::silver("0%")) %>%  # 0 in gray
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
  type <- get_type(x) %>% unique()
  type <- ifelse(length(type) > 1, "mixed", type)

  paste0("fmtn-", type)
  #"fmtn"
}


# Include numbers of digits and types in the printed name
#' @export
vec_ptype_full.fmtn <- function(x, ...) {
  type <- get_type(x) %>% unique()
  type <- ifelse(length(type) > 1, "mixed", type)
  #digits <- get_digits(x) %>% range() %>% unique() %>% paste0(collapse = ":")

  paste0("fmtn-", type) #  paste0("fmtn-", type, "-", digits)
}


#Make our fmtn class coercible with herself, and back and forth with double and integer vectors :
#' @export
vec_ptype2.fmtn.fmtn    <- function(x, y, ...) new_fmtn(
  col_var   = dplyr::if_else(get_col_var(x)   == get_col_var(y),
                             true = get_col_var(x),
                             false  = vec_recycle("several_vars", length(get_col_var(x)))),
  total_col = dplyr::if_else(get_total_col(x) == get_total_col(y),
                             true = get_total_col(x),
                             false  = vec_recycle(FALSE, length(get_total_col(x)))),
  pct_type  = dplyr::if_else(get_pct_type(x)  == get_pct_type(y),
                             true = get_pct_type(x),
                             false  = vec_recycle("mixed", length(get_pct_type(x))))
)
#' @export
vec_ptype2.fmtn.double  <- function(x, y, ...) x # new_fmtn() #double()
#' @export
vec_ptype2.double.fmtn  <- function(x, y, ...) y # new_fmtn() #double()
#' @export
vec_ptype2.fmtn.integer <- function(x, y, ...) x # fmtn() #double()
#' @export
vec_ptype2.integer.fmtn <- function(x, y, ...) y # new_fmtn() #double()

# Conversions :
#' @export
vec_cast.fmtn.fmtn       <- function(x, to, ...) new_fmtn(type   = get_type  (x),
                                                          n      = get_n     (x),
                                                          wn     = get_wn    (x),
                                                          pct    = get_pct   (x),
                                                          digits = get_digits(x),
                                                          ctr    = get_ctr   (x),
                                                          mean   = get_mean  (x),
                                                          sd     = get_sd    (x),
                                                          ci     = get_ci    (x),

                                                          col_var   = get_col_var(to),
                                                          total_col = get_total_col(to),
                                                          pct_type  = get_pct_type(to)  )

#' @export
vec_cast.fmtn.double     <- function(x, to, ...) fmtn(type = "wn", wn = x,
                                                      col_var   = get_col_var(to),
                                                      total_col = get_total_col(to),
                                                      pct_type  = get_pct_type(to)  )
#' @method vec_cast.double fmtn
#' @export
vec_cast.double.fmtn     <- function(x, to, ...) get_num(x) %>% as.double() #field(x, "pct")

#' @export
vec_cast.fmtn.integer    <- function(x, to, ...) fmtn(n = x,
                                                      col_var   = get_col_var(to),
                                                      total_col = get_total_col(to),
                                                      pct_type  = get_pct_type(to)  ) #new_fmtn(pct = as.double(x))
#' @method vec_cast.integer fmtn
#' @export
vec_cast.integer.fmtn    <- function(x, to, ...) get_num(x) %>% as.integer() #field(x, "pct") %>% as.integer()

#' @method vec_cast.character fmtn
#' @export
vec_cast.character.fmtn  <- function(x, to, ...) format(x)


#Comparisons and sorting :
#' @export
vec_proxy_equal.fmtn   <- function(x, ...) {
  get_num(x)
}
#' @export
vec_proxy_compare.fmtn <- function(x, ...) {
  get_num(x)
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
  vec_arith_base(op, get_num(x), vec_data(y))
  #stop_incompatible_op(op, x, y)
}

# positive_double <- function(n) n * sign(n)
# positive_integer <- function(n) as.integer(n * sign(n))

#' @method vec_arith.fmtn fmtn
#' @export
vec_arith.fmtn.fmtn <- function(op, x, y, ...) {
  same_var <- get_col_var(x)  == get_col_var(y)
  if (!same_var) warning("operation ", op,
                         " over columns belonging to different variables : ",
                         get_col_var(x) , "/", get_col_var(y))
  x_pct <- get_pct_type(x)
  y_pct <- get_pct_type(y)
  same_pct <- x_pct == y_pct
  if (!same_pct) warning("operation ", op,
                         " over columns with different pct types : ",
                         get_pct_type(x), "/", get_pct_type(y))
  rep_NA_real <- rep(NA_real_, length(x))

  switch(
    op,
    "+" = ,
    "-" = new_fmtn(
      type   = get_type(x),      #dplyr::if_else(get_type(x) == get_type(x)), true = get_type(x), false = "n),
      n      = vec_arith_base(op, get_n(x)  , get_n(y)  ), #%>% positive_integer(),
      wn     = vec_arith_base(op, get_wn(x) , get_wn(y) ), #%>% positive_double(),
      pct    = ifelse(same_pct & x_pct != "col",
                      yes  = vec_arith_base(op, get_pct(x), get_pct(y)),
                      no = NA_real_) %>% tidyr::replace_na(NA_real_),
      digits = pmax(get_digits(x), get_digits(y)),
      ctr    = rep_NA_real, # ???
      mean   = vec_arith_base(op, get_mean(x) * get_wn(.x), get_mean(y) * get_wn(y)) /
        vec_arith_base("+", get_wn(x) , get_wn(y) ),# weighted mean
      sd     = rep_NA_real,
      ci     = rep_NA_real,

      col_var   = dplyr::if_else(same_var, get_col_var(x),
                                 vec_recycle("several_vars", length(get_col_var(x)))),
      total_col = FALSE,
      pct_type  = dplyr::if_else(same_pct, get_pct_type(x),
                                 vec_recycle("mixed", length(get_col_var(x))))
    ),
    "/" = ,
    "*" = new_fmtn(
      type   = get_type(x),
      n      = get_n(x)   ,
      wn     = get_wn(x)  ,
      pct    = vec_arith_base(op, get_pct(x), get_pct(x)), #Remove multiplication ?
      digits = pmax(get_digits(x), get_digits(y)),
      ctr    = rep_NA_real,
      mean   = rep_NA_real,
      sd     = rep_NA_real,
      ci     = rep_NA_real,

      col_var   = dplyr::if_else(same_var, get_col_var(x),
                                 vec_recycle("several_vars", length(get_col_var(x)))),
      total_col = FALSE,
      pct_type  = dplyr::if_else(same_pct, get_pct_type(x),
                                 vec_recycle("mixed", length(get_col_var(x))))
    ),
    stop_incompatible_op(op, x, y)
  )
}

#' @method vec_arith.fmtn numeric
#' @export
vec_arith.fmtn.numeric <- function(op, x, y, ...) {
  set_num(x, vec_arith_base(op, get_num(x), y))
  # new_fmtn(pct    = vec_arith_base(op, field(x, "pct"), y),
  #          type   = field(x, "type"  ),
  #          digits = field(x, "digits"),
  #          n      = field(x, "n"     ),
  #          wn     = field(x, "wn"    ),
  #          sd     = field(x, "sd"    ),
  #          ci     = field(x, "ci"    )                     )
}


#' @method vec_arith.numeric fmtn
#' @export
vec_arith.numeric.fmtn <- function(op, x, y, ...) {
  set_num(y, vec_arith_base(op, x, get_num(y)))
  # new_fmtn(pct    = vec_arith_base(op, x, field(y, "pct")),
  #          type   = field(y, "type"  ),
  #          digits = field(y, "digits"),
  #          n      = field(y, "n"     ),
  #          wn     = field(y, "wn"    ),
  #          sd     = field(y, "sd"    ),
  #          ci     = field(y, "ci"    )                     )
}

#' @method vec_arith.fmtn MISSING
#' @export
vec_arith.fmtn.MISSING <- function(op, x, y, ...) { #unary + and - operators
  switch(op,
         `-` = set_num(x, get_num(x) * -1),
           # new_fmtn(pct    = field(x, "pct"   ) * -1,
           #              type   = field(x, "type"  ),
           #              digits = field(x, "digits"),
           #              n      = field(x, "n"     ),
           #              wn     = field(x, "wn"    ),
           #              sd     = field(x, "sd"    ),
           #              ci     = field(x, "ci"    )       ),
         `+` = x,
         stop_incompatible_op(op, x, y)
  )
}


#Mathematical operations :
# (direct operations on counts,
# automatically calculate weighted means for pct and means, erase sd and ci)
#' @export
vec_math.fmtn <- function(.fn, .x, ...) {
  if (!is.na(get_pct_type(.x) ) & get_pct_type(.x) == "mixed") warning(
    "operation ", op,
    " within a variable mixing different types of percentages : "
  )

  switch(.fn,
         "sum" = new_fmtn(type   = get_type(.x)[1],
                          digits = min(get_digits(.x)),
                          n      = vec_math_base(.fn, get_n(.x)  , ...),
                          wn     = vec_math_base(.fn, get_wn(.x) , ...),
                          pct    = ifelse(get_pct_type(.x)  != "row",
                                          yes = vec_math_base(.fn, get_pct(.x), ...),
                                          no  = NA_real_) %>% tidyr::replace_na(NA_real_),
                          ctr    = NA_real_,
                          mean   = vec_math_base(.fn, get_mean(.x) * get_wn(.x), ...) /
                            vec_math_base("sum", get_wn(.x), ...),
                          sd     = NA_real_,
                          ci     = NA_real_,

                          col_var   = get_col_var(.x),
                          total_col = get_total_col(.x),
                          pct_type  = get_pct_type(.x)
         ),
         "mean" = new_fmtn(type   = get_type(.x)[1],
                           digits = max(get_digits(.x)),
                           n      = vec_math_base(.fn, get_n(.x)  , ...),
                           wn     = vec_math_base(.fn, get_wn(.x) , ...),
                           pct    = vec_math_base(.fn, get_pct(.x) * get_wn(.x), ...) /
                             vec_math_base("sum", get_wn(.x), ...),
                           ctr    = NA_real_,
                           mean   = vec_math_base(.fn, get_mean(.x) * get_wn(.x), ...) /
                             vec_math_base("sum", get_wn(.x), ...),
                           sd     = NA_real_,
                           ci     = NA_real_,

                           col_var   = get_col_var(.x),
                           total_col = get_total_col(.x),
                           pct_type  = get_pct_type(.x)
         ),
         vec_math_base(.fn, get_num(.x), ...) )
}













# #Tests (not the right syntax)-----------------------------------------------------------
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
#
#
# Ok from here ----
# x <- fmtn("pct", n = c(2, 1), pct = c(0.5, 1.5)) #wn = c(0.7, 2.4)
# y <- fmtn("n"  , n = c(3, 9), pct = c(0.5, 1.5)) #wn = c(0.7, 2.4)
# z <- c(x, y)
#
# x ; y ; z
#
# get_type(x)
# get_n(x)
# get_wn(x)
# get_pct(x)
# get_digits(x)
# get_ctr(x)
# get_mean(x)
# get_sd(x)
# get_ci(x)
#
#
# get_num(z)
# set_num(z, c(1, 2, 3, 4))
#
# sum(x) #
# sum(y) #
# sum(z) #
#
# (get_pct(x)[1]*get_n(x)[1] + get_pct(x)[2]*get_n(x)[2]) / (get_n(x)[1] + get_n(x)[2])
# get_n(y)[1] + get_n(y)[2]
# (get_pct(z)[1]*get_n(z)[1] + get_pct(z)[2]*get_n(z)[2] + get_pct(z)[3]*get_n(z)[3] + get_pct(z)[4]*get_n(z)[4]) / (get_n(z)[1] + get_n(z)[2] + get_n(z)[3] + get_n(z)[4])
#
# sum(x) %>% vec_data()
# sum(y) %>% vec_data()
# sum(z) %>% vec_data()
#
# is_pct(x)
# is_pct(y)
# is_pct(z)
# is_pct(c(1,2))
#
# is_n(x)
# is_n(y)
# is_n(z)


