# Create formated numbers class -----------
#Import vctrs in NAMESPACE :
#' Internal vctrs methods
#'
#' @import vctrs
#' @keywords internal
#' @name percent-vctrs
NULL

#' Create a vector of class formatted numbers.

#' @param x A double vector. For \code{\link{fmt}} and \code{\link{as_fmt}},
#' anything coercible to double.
#' @param digits The number of digits to print. It can then be changed
#' with \code{\link{set_digits}}.
#'
#' @return A numeric vector of class fmt.
#' @export
#'
#' @examples num1 <- fmt(c(0.7, 1.2, 7), type = c("n", "pct", "mean"), digits = c(0, 1, 2),
#' n = c(5, 10, 15), wn = c(4.7, 12.1, 13.9) )
#' num1
#' num1[1] + num1[2] + num1[3]
#' # To access the underlying data :
#' sum(num1[1], num1[2], num1[3]) %>% vctrs::vec_data()
#' # To access the underlying weighted counts :
#' fmt(1, wn = 20) %>% vctrs::field("wn")

#' Create a vector of class formatted numbers.
#' @description fmt vectors can be used to print each number with the chosen digits or as
#' percentages in a \code{\link[tablr]{tab}}, while keeping the underlying numeric data
#' for maths. It also keeps track of the counts and weighted counts beneath any percentage
#' or  mean, and can be used to calculate and print confidence intervals.
#' \code{fmt} can use all standard operations, like +, -, sum(), or c(), using vctrs.
#' @param pct A double vector. For \code{\link{fmt}} and \code{\link{as_fmt}},
#' anything coercible to double.
#' @param type The type of printing or background calculation, "n" for counts, "pct" for
#' percentages, "mean" for means.
#' @param digits The number of digits, as an integer or integer vector of the length of the
#' data. It can then be changed with \code{vctrs::`field<-`(x, "digits", value)}.
#' @param n The underlying count, as an integer vector of the same length, used with
#' type = "pct" to calculate confidence intervals.
#' @param wn The underlying weighted counts, as an integer vector of the same length. It
#' it used in certain operations on \code{\link{fmt}}, like means.
#' @param var The standards deviations, used with type = "mean" to calculate confidence
#' intervals.
#' @param ci The confidence intervals to print, if they have been calculated.
#'
#' @return A numeric vector of class fmt.
#' @export
#'
#' @examples fmt(0.7, "pct")
fmt <- function(type      = "n",
                n         = integer(),
                wn        = rep(NA_real_, length(n)),
                pct       = rep(NA_real_, length(n)),
                diff      = rep(NA_real_, length(n)),
                digits    = rep(0L      , length(n)),
                ctr       = rep(NA_real_, length(n)),
                mean      = rep(NA_real_, length(n)),
                var        = rep(NA_real_, length(n)),
                ci        = rep(NA_real_, length(n)),
                in_totrow = rep(FALSE, length(n)),
                in_tottab = rep(FALSE, length(n)),
                col_var   = NA_character_,
                totcol = FALSE, #nvls = integer(),
                pct_type  = NA_character_,
                ci_type   = NA_character_,
                color     = NA_character_,
                comp_all  = FALSE ) {

  max_size <- list(n, wn, pct, digits, ctr, mean, var, ci) %>% #type
    purrr::map_int(length) %>% max()

  type   <- vec_recycle(vec_cast(type  , character()), size = max_size)
  n      <- vec_recycle(vec_cast(n     , integer())  , size = max_size)
  wn     <- vec_recycle(vec_cast(wn    , double())   , size = max_size) #anything coercible as a double
  pct    <- vec_recycle(vec_cast(pct   , double())   , size = max_size)
  diff   <- vec_recycle(vec_cast(diff  , double())   , size = max_size)
  digits <- vec_recycle(vec_cast(digits, integer())  , size = max_size)
  ctr    <- vec_recycle(vec_cast(ctr   , double())   , size = max_size)
  mean   <- vec_recycle(vec_cast(mean  , double())   , size = max_size)
  var     <- vec_recycle(vec_cast(var  , double())   , size = max_size)
  ci     <- vec_recycle(vec_cast(ci    , double())   , size = max_size)

  in_totrow <- vec_recycle(vec_cast(in_totrow, logical()), size = max_size)
  in_tottab <- vec_recycle(vec_cast(in_tottab, logical()), size = max_size)

  col_var   <- vec_recycle(vec_cast(col_var  , character()), size = 1)
  totcol    <- vec_recycle(vec_cast(totcol, logical()  ), size = 1)
  #nlvs      <- vec_cast(nlvs, integer())
  pct_type  <- vec_recycle(vec_cast(pct_type , character()), size = 1)
  ci_type   <- vec_recycle(vec_cast(ci_type  , character()), size = 1)
  color     <- vec_recycle(vec_cast(color    , character()), size = 1)
  comp_all  <- vec_recycle(vec_cast(comp_all ,   logical()), size = 1)

  new_fmt(type = type, n = n, wn = wn, pct = pct, diff = diff, digits = digits,
          ctr = ctr, mean = mean, var = var, ci = ci,
          in_totrow = in_totrow, in_tottab = in_tottab,
          col_var = col_var, totcol = totcol, pct_type = pct_type,
          ci_type = ci_type, color = color, comp_all = comp_all) #nlvs = nlvs,
}



# Constructor :
#' @describeIn fmt A constructor for class fmt.
#' @export
new_fmt <- function(type      = "n",
                    n         = integer(),
                    wn        = rep(NA_real_, length(n)),
                    pct       = rep(NA_real_, length(n)),
                    diff      = rep(NA_real_, length(n)),
                    digits    = rep(0L      , length(n)),
                    ctr       = rep(NA_real_, length(n)),
                    mean      = rep(NA_real_, length(n)),
                    var       = rep(NA_real_, length(n)),
                    ci        = rep(NA_real_, length(n)),
                    in_totrow = rep(FALSE   , length(n)),
                    in_tottab = rep(FALSE   , length(n)),
                    col_var   = NA_character_,
                    totcol    = FALSE, #nlvs = integer(),
                    pct_type  = NA_character_,
                    ci_type   = NA_character_,
                    color     = NA_character_,
                    comp_all  = FALSE) {
  # stopifnot(
  #   all(type %in% c("n", "wn", "pct", "pct_ci", "ctr", "mean", "mean_ci", "var", "ci")),
  #   pct_type %in% c("row", "col", "all", "all_tabs", "mixed", NA_character_)
  # )

  # list(type, n, wn, pct, digits, ctr, mean, var, ci, col_var, totcol, pct_type) %>%
  #   purrr::map(print)
  # cat("\n")

  # vec_assert(type, character()) #check type or size
  type <- vec_recycle(type, size = length(n))
  # vec_assert(n     , integer()) #, size = length(n)
  # vec_assert(wn    , double() ) #, size = length(n)
  # vec_assert(pct   , double() ) #, size = length(n)
  # vec_assert(digits, integer()) #, size = length(n)
  # vec_assert(ctr   , double() ) #, size = length(n)
  # vec_assert(mean  , double() ) #, size = length(n)
  # vec_assert(var   , double() ) #, size = length(n)
  # vec_assert(ci    , double() ) #, size = length(n)
  #
  # vec_assert(in_totrow, logical())
  # vec_assert(in_tottab, logical())
  #
  # vec_assert(col_var , character(), size = 1)
  # vec_assert(totcol  , logical()  , size = 1)
  # # vec_assert(nlvs , integer)
  # vec_assert(pct_type, character(), size = 1)

  new_rcrd(list(type = type, n = n, wn = wn, pct = pct, diff = diff,
                digits = digits, ctr = ctr, mean = mean, var = var, ci = ci,
                in_totrow = in_totrow, in_tottab = in_tottab),
           col_var = col_var, totcol = totcol, pct_type = pct_type,
           ci_type = ci_type, color = color, comp_all = comp_all, class = "fmt")
  #access with fields() n_fields() field() `field<-`() ; vec_data() return the tibble with all fields
}

fmt0 <- function(type = "n", digits = 0) {
  new_fmt(type = type, n = 0L, digits = as.integer(digits))
  # switch (type,
  #   "n"       = new_fmt(type = type, n = 0L,                           digits = as.integer(digits)),
  #   "wn"      = new_fmt(type = type, n = 0L, wn = 0,                   digits = as.integer(digits)),
  #   "pct"     = ,
  #   "pct_ci"  = new_fmt(type = type, n = 0L, wn = 0, pct = 0,          digits = as.integer(digits)),
  #   "ctr"     = new_fmt(type = type, n = 0L, wn = 0, pct = 0, ctr = 0, digits = as.integer(digits)),
  #   "mean"    = ,
  #   "mean_ci" = new_fmt(type = type, n = 0L, wn = 0, mean = 0, var = 0, digits = as.integer(digits)),
  #   "var"      = new_fmt(type = type, n = 0L, wn = 0, mean = 0, var = 0, digits = as.integer(digits)),
  #   "ci"      = new_fmt(type = type, n = 0L, ci = 0,                   digits = as.integer(digits)),
  # )
}
#' @describeIn fmt A test function for class fmt.
#' @export
is_fmt <- function(x) {
  inherits(x, "fmt")
}

# #' A function to convert vectors to class fmt.
# #' @param x A vector coercible to double, or a character vector with numbers.
# #' @param ... The number of digits as an integer, to be passed to the method.
# #'
# #' @export
# as_fmt <- function(x, ...) {
#   UseMethod("as_fmt")
# }

# # @describeIn as_fmt
# #' @export
# as_fmt.default <- function(x, digits = rep(0L, length(x)), #type = rep("count", length(x)),
#                             # n = rep(NA_integer_, length(x)), wn = rep(NA_real_, length(x)),
#                             # var = rep(NA_real_, length(x)), ci = rep(NA_real_, length(x)),
#                             ...) {
#   new_fmt(vec_data(x))
# }



# Functions to work with formatted numbers------------------------------------------------
# to get and set the different fields directly

# #' Work with number of digits for formatted numbers
# #' @param x A vector of class \code{\link{fmt}}
# #' @return \code{\link{get_digits}} : an integer vector with the number of digits.
# #' @export
# get_digits <- function(x) as.integer(attr(x, "digits")) # Helper to extract the digits attribute.
# #' @rdname get_digits
# #' @param value The number of digits to print, as an integer.
# #' @return \code{\link{set_digits}} : a vector with the right number of digits.
# #' @export
# set_digits <- function(x, value) `attr<-`(x, "digits", vec_recycle(vec_cast(value, integer()), length(x))) # To set digits

fmt_field_factory <- function(x) {
  function(fmt) field(fmt, x)
}
get_type   <- fmt_field_factory("type")
get_n      <- fmt_field_factory("n")
get_wn     <- function(fmt) { #If there is no weighted counts, take counts
  out <- field(fmt, "wn")
  if (any(is.na(out))) {
    counts <- field(fmt, "n") %>% as.double()
    out[is.na(out)] <- counts[is.na(out)]
  }
  out
}
get_pct    <- fmt_field_factory("pct")
get_diff   <- fmt_field_factory("diff")
#get_pct_ci <- function(fmt) field("pct")
get_digits <- fmt_field_factory("digits")
get_ctr    <- fmt_field_factory("ctr")
get_mean   <- fmt_field_factory("mean")
get_var    <- fmt_field_factory("var")
get_ci     <- fmt_field_factory("ci")


#Detect cells in total rows (for fmt vectors values)
#' @export
is_totrow <- function(x, ...) UseMethod("is_totrow")
#' @export
is_totrow.default  <-  function(x, ...) rep(FALSE, length(x)) #{
#   ifelse(! is.null(field(x, "in_totrow")),
#          yes = field(x, "in_totrow"),
#          no  = vec_recycle(FALSE, length(x)))
# }
#' @export
is_totrow.fmt <- function(x, ...) field(x, "in_totrow")
#' @export
is_totrow.data.frame <- function(x, ..., partial = FALSE) {
  totrow_cells_test <- dplyr::ungroup(x) %>% dplyr::select(where(is_fmt)) %>%
    #dplyr::select(-ENCADRacm_Total, -SEXE_Total) %>%
    purrr::map_df(~ is_totrow(.))

  if (partial == TRUE) {
    totrow_cells_test %>%
      dplyr::rowwise() %>% dplyr::transmute(partial = any(dplyr::c_across()))  %>%
      dplyr::pull(partial)
  } else {
    test_result <- totrow_cells_test %>%
      dplyr::rowwise() %>%
      dplyr::transmute(complete = all(dplyr::c_across()),
                       partial  = any(dplyr::c_across()) & ! complete)
    if (any(test_result$partial)) {
      warning("partial total rows (with some fmt cells not tagged 'totrow') ",
              "were not taken into account ")
    }
    test_result$complete
  }
}

#Detect cells in total rows (for fmt vectors values)
#' @export
is_tottab <- function(x, ...) UseMethod("is_tottab")
#' @export
is_tottab.default  <-  function(x, ...) rep(FALSE, length(x)) #{
#   ifelse(! is.null(field(x, "in_tottab")),
#          yes = field(x, "in_tottab"),
#          no  = vec_recycle(FALSE, length(x)))
# }
#' @export
is_tottab.fmt <- function(x, ...) field(x, "in_tottab")
#' @export
is_tottab.data.frame <- function(x, ..., partial = FALSE) {
  tottab_cells_test <- dplyr::ungroup(x) %>% dplyr::select(where(is_fmt)) %>%
    purrr::map_df(~ is_tottab(.))

  if (partial == TRUE) {
    tottab_cells_test %>%
      dplyr::rowwise() %>% dplyr::transmute(partial = any(dplyr::c_across())) %>%
      dplyr::pull(partial)
  } else {
    test_result <- tottab_cells_test %>%
      dplyr::rowwise() %>%
      dplyr::transmute(complete = all(dplyr::c_across()),
                       partial  = any(dplyr::c_across()) & ! complete)
    if (any(test_result$partial)) {
      warning("partial total rows (with some fmt cells not tagged 'totrow') ",
              "were not taken into account ")
    }
    test_result$complete
  }
}




# Get names of column variable (for fmt or tab)
#' @export
get_col_var <- function(x, ...) UseMethod("get_col_var")
#' @export
get_col_var.default     <- function(x, ...) {
  ifelse(! is.null(purrr::attr_getter("col_var")(x)),
         yes = purrr::attr_getter("col_var")(x),
         no  = NA_character_)
}
#' @export
get_col_var.fmt <- purrr::attr_getter("col_var")
#' @export
get_col_var.data.frame <- function(x, ...) {
  purrr::map_chr(x, ~ get_col_var(.))
  #%>%
  #purrr::map_if(purrr::map(., is.na), ~ FALSE)
}



# Detect total columns (for fmt or tab)
#' @export
is_totcol <- function(x, ...) UseMethod("is_totcol")
#' @export
is_totcol.default     <- function(x, ...) {
  ifelse(! is.null(purrr::attr_getter("totcol")(x)),
         yes = purrr::attr_getter("totcol")(x),
         no  = FALSE)
}
#' @export
is_totcol.fmt <- purrr::attr_getter("totcol")
#' @export
is_totcol.data.frame <- function(x, ...) {
  purrr::map_lgl(x, ~ is_totcol(.))
}

#tab_get_nlvs <- purrr::attr_getter("nlvs")

# Get percentages types (for fmt or tab)
#' @export
get_pct_type <- function(x, ...) UseMethod("get_pct_type")
#' @export
get_pct_type.default     <- function(x, ...) {
  ifelse(! is.null(purrr::attr_getter("pct_type")(x)),
         yes = purrr::attr_getter("pct_type")(x),
         no  = NA_character_)
}
#' @export
get_pct_type.fmt <- purrr::attr_getter("pct_type")
#' @export
get_pct_type.data.frame <- function(x, ...) {
  purrr::map_chr(x, ~ get_pct_type(.))
}

# is_row_pct <- function(x) {
#   tidyr::replace_na(get_pct_type(x) == "row", FALSE)
# }
# is_col_pct <- function(x) {
#   tidyr::replace_na(get_pct_type(x) == "col", FALSE)
# }
# is_all_pct<- function(x) {
#   tidyr::replace_na(get_pct_type(x) == "all", FALSE)
# }
# is_all_tabs_pct<- function(x) {
#   tidyr::replace_na(get_pct_type(x) == "all_tabs", FALSE)
# }

# Get confidence intervals types (for fmt or tab)
#' @export
get_ci_type <- function(x, ...) UseMethod("get_ci_type")
#' @export
get_ci_type.default     <- function(x, ...) {
  ifelse(! is.null(purrr::attr_getter("ci_type")(x)),
         yes = purrr::attr_getter("ci_type")(x),
         no  = NA_character_)
}
#' @export
get_ci_type.fmt <- purrr::attr_getter("ci_type")
#' @export
get_ci_type.data.frame <- function(x, ...) {
  purrr::map_chr(x, ~ get_ci_type(.))
}


# Get color (for fmt or tab)
#' @export
get_color <- function(x, ...) UseMethod("get_color")
#' @export
get_color.default     <- function(x, ...) {
  ifelse(! is.null(purrr::attr_getter("color")(x)),
         yes = purrr::attr_getter("color")(x),
         no  = NA_character_)
}
#' @export
get_color.fmt <- purrr::attr_getter("color")
#' @export
get_color.data.frame <- function(x, ...) {
  purrr::map_chr(x, ~ get_color(.))
}

get_comp_all <- purrr::attr_getter("comp_all")


fmt_set_field_factory <- function(x) {
  function(fmt, value) {
    value <- vec_recycle(value, size = length(fmt))
    `field<-`(fmt, x, value)
  }
}
set_type   <- fmt_set_field_factory("type")
set_n      <- fmt_set_field_factory("n")
set_wn     <- fmt_set_field_factory("wn")
set_pct    <- fmt_set_field_factory("pct")
set_diff   <- fmt_set_field_factory("diff")
set_digits <- fmt_set_field_factory("digits")
set_ctr    <- fmt_set_field_factory("ctr")
set_mean   <- fmt_set_field_factory("mean")
set_var    <- fmt_set_field_factory("var")
set_ci     <- fmt_set_field_factory("ci")

as_totrow  <- function(fmt, value = TRUE) `field<-`(fmt, "in_totrow",
                                                    vec_recycle(value, length(fmt)))
as_tottab  <- function(fmt, value = TRUE) `field<-`(fmt, "in_tottab",
                                                    vec_recycle(value, length(fmt)))

set_col_var   <- function(fmt, value)        `attr<-`(fmt ,"col_var" , value)
as_totcol     <- function(fmt, value = TRUE) `attr<-`(fmt ,"totcol"  , value)
#set_nlvs      <- function(fmt, value)        `attr<-`(fmt, "nlvs"    , value)
set_pct_type  <- function(fmt, value)        `attr<-`(fmt ,"pct_type", value)
set_ci_type   <- function(fmt, value)        `attr<-`(fmt ,"ci_type" , value)
set_color     <- function(fmt, value)        `attr<-`(fmt ,"color"   , value)
set_comp      <- function(fmt, value = c("tab", "all")) {
  switch(value[1],
         "tab" = `attr<-`(fmt, "comp_all", FALSE),
         "all" = `attr<-`(fmt, "comp_all", TRUE ))
}

fmt_is_type_factory <- function(x, y = character()) {
  function(fmt) {
    if (is_fmt(fmt)) {
      out <- get_type(fmt) %in% c(x, y)
    } else {
      out <- rep(FALSE, length(fmt))
    }
    out
  }
}
is_n      <- fmt_is_type_factory("n"              )
is_wn     <- fmt_is_type_factory("wn"             )
is_pct    <- fmt_is_type_factory("pct", "pct_ci"  )
is_pct_ci <- fmt_is_type_factory("pct_ci"         )
is_diff   <- fmt_is_type_factory("diff"           )
is_ctr    <- fmt_is_type_factory("ctr"            )
is_mean   <- fmt_is_type_factory("mean", "mean_ci")
is_mean_ci<- fmt_is_type_factory("mean_ci"        )
is_var    <- fmt_is_type_factory("var"             )
is_ci     <- fmt_is_type_factory("ci"             )

get_num <- function(x) {
  out  <- get_n(x)
  type <- get_type(x)
  nas <- is.na(type)
  out[!nas & type == "wn"     ] <- get_wn  (x)[!nas & type == "wn"     ]
  out[!nas & type == "pct"    ] <- get_pct (x)[!nas & type == "pct"    ]
  out[!nas & type == "diff"   ] <- get_diff(x)[!nas & type == "diff"   ]
  out[!nas & type == "pct_ci" ] <- get_pct (x)[!nas & type == "pct_ci" ]
  out[!nas & type == "ctr"    ] <- get_ctr (x)[!nas & type == "ctr"    ]
  out[!nas & type == "mean"   ] <- get_mean(x)[!nas & type == "mean"   ]
  out[!nas & type == "mean_ci"] <- get_mean(x)[!nas & type == "mean_ci"]
  out[!nas & type == "var"    ] <- get_var (x)[!nas & type == "var"    ]
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
  out[!nas & type == "diff"] <- set_pct (x, value)[!nas & type == "diff"]
  out[!nas & type == "ctr" ] <- set_ctr (x, value)[!nas & type == "ctr" ]
  out[!nas & type == "mean"] <- set_mean(x, value)[!nas & type == "mean"]
  out[!nas & type == "var" ] <- set_var (x, value)[!nas & type == "var" ]
  out[!nas & type == "ci"  ] <- set_ci  (x, value)[!nas & type == "ci"  ]
  out
}


#Methods for formatted numbers -----------------------------------------------------------
#The first method for every class should almost always be a format() method.
#This should return a character vector the same length as x.

#' Print method for class fmt
#'
#' @param x A fmt object.
#' @param ... Other parameter.
#'
#' @return The fmt printed.
#' @export
# @keywords internal
# @examples
format.fmt <- function(x, ...) {
  #out <- formatC(signif(vec_data(x) * 100, get_digits(x))) #vec_data() correct printing problems
  #sprintf(paste0("%-0.", get_digits(x), "f"), x * 100)

  out    <- get_num(x)
  na_out <- is.na(out)

  type <- get_type(x)
  nas  <- is.na(type)

  ok <- !na_out & !nas

  type_of_ci <- get_ci_type(x)

  digits <- get_digits(x)
  pct_or_ci     <- ok & type %in% c("pct", "pct_ci", "diff", "ci", "ctr") &
    (!stringr::str_detect(type_of_ci, "_mean$") | is.na(type_of_ci))
  pct_or_pct_ci <- ok & type %in% c("pct", "pct_ci", "diff", "ctr")
  pct_ci_only   <- ok & type == "pct_ci"
  n_wn          <- ok & (type %in% c("n", "wn", "mean", "mean_ci", "var") |
                           (type == "ci" &
                              stringr::str_detect(type_of_ci, "_mean$")) )
  type_ci       <- ok & type == "ci"
  mean_ci_only  <- ok & type == "mean_ci"

  out[pct_or_ci] <- out[pct_or_ci] * 100
  out[!na_out] <- sprintf(paste0("%-0.", digits[!na_out], "f"), out[!na_out]) %>%
    stringr::str_replace("^0.0+$|^-0.0+$", "0")
  out[n_wn] <- out[n_wn] %>% prettyNum(big.mark = " ", preserve.width = "individual")
  out[pct_or_pct_ci] <- out[pct_or_pct_ci] %>% stringr::str_replace("^100.0+$", "100")
  out[na_out] <- NA
  out[pct_or_pct_ci] <- paste0(out[pct_or_pct_ci], "%")

  if (any(pct_ci_only) | any(mean_ci_only)) conf_int <- get_ci(x)

  if (any(pct_ci_only)) {
    pct_conf_int_pct_ci <-
      paste0(" ±",
             sprintf(paste0("%-0.", digits[pct_ci_only] + 1, "f"),
                     conf_int[pct_ci_only] * 100)) %>%
      stringr::str_remove("^ ±0$|^ ±0.0+$|^ ±-0.0+$|^ ±NA") %>%
      stringr::str_pad(max(stringr::str_length(.)))

    out[pct_ci_only] <- paste0(out[pct_ci_only], pct_conf_int_pct_ci)
  }

  if (any(mean_ci_only)) {
    conf_int <- get_ci(x)
    mean_conf_int_pct_ci <-
      paste0(" ±",
             sprintf(paste0("%-0.", digits[mean_ci_only], "f"),
                     conf_int[mean_ci_only])) %>%
      stringr::str_remove("^ ±0$|^ ±0.0+$|^ ±-0.0+$|^ ±NA") %>%
      stringr::str_pad(max(stringr::str_length(.)))

    out[mean_ci_only] <- paste0(out[mean_ci_only], mean_conf_int_pct_ci)
  }

  out[type_ci] <- switch(
    type_of_ci,
    "abs_mean"            = ,
    "diff_row_mean"       = ,
    "diff_row_all_mean"   = paste0("±", out[type_ci]),
    "spread_row_mean"     = ,
    "spread_row_all_mean" = paste0(out[type_ci]),

    "abs"                 = ,
    "diff_col"            = ,
    "diff_row"            = ,
    "diff_row_all"        = paste0("±", out[type_ci], "%"),
    "spread_col"          = ,
    "spread_row"          = ,
    "spread_row_all"      = paste0(out[type_ci], "%"),
    ""
  )

  out <- stringr::str_pad(out, max(stringr::str_length(out)))
  out
}
#print negative digits ----




#' Pillar_shaft method for class fmt
#'
#' @param x A fmt object.
#' @param ... Other parameter.
#'
#'
#' @return A fmt printed in a pillar.
#' @importFrom pillar pillar_shaft
#' @export
pillar_shaft.fmt <- function(x, ...) {

  out     <- format(x)
  na_out  <- is.na(out)

  type <- get_type(x)
  nas  <- is.na(type)
  ok   <- !na_out & !nas

  type_diff   <- ok & type == "diff"
  type_ci     <- ok & type == "ci"
  type_mean   <- ok & type == "mean"

  allmean <- all(type[ok] == "mean")

  type_of_ci  <- get_ci_type(x)
  pct         <- get_pct_type(x)
  color       <- get_color(x)

  totcol      <- is_totcol(x)
  totrows     <- is_totrow(x)

  if (any(type_ci) | any(type_diff)) ref_tot <-
    get_reference_total(x, mode = "cells")

  if (any(type_ci)) {
    totfmt <- set_type(x[type_ci], ifelse(allmean, "mean", "pct")) %>% format()
    out[type_ci] <-
      dplyr::if_else(ref_tot[type_ci], paste0("ref:", totfmt), out[type_ci])
  }

  if (any(type_diff)) {
    totpct <- set_type(x[type_diff], "pct") %>% format()
    out[type_diff] <-
      dplyr::if_else(ref_tot[type_diff], paste0("ref:", totpct), out[type_diff])
  }

  if (!is.na(color) & color != "no") {

    diff <- if (color %in% c("diff", "diff_ci", "after_ci") ) {
      get_diff(x)
    } else {
      vec_recycle(NA_real_, length(x))
    }

    ci <- if (color %in% c("diff_ci", "after_ci")) {
      get_ci(x)
    } else {
      vec_recycle(NA_real_, length(x))
    }

    ctr <- if (color == "contrib") {
      get_ctr(x)
    } else {
      vec_recycle(NA_real_, length(x))
    }

    brk <-
      switch(color,
             "diff"     = ,
             "diff_ci"  = if (allmean) mean_brk    else pct_brk   ,
             "after_ci" = if (allmean) mean_ci_brk else pct_ci_brk,
             "ctr"      = contrib_brk                              )

    brksup <-
      switch(color,
             "diff"     = ,
             "diff_ci"  = if (allmean) mean_brksup    else pct_brksup   ,
             "after_ci" = if (allmean) mean_ci_brksup else pct_ci_brksup,
             "ctr"      = contrib_brksup                                 )

    color_styles <-
      if (stringr::str_detect(color_styles_all[[1]], "^bg")) {
        switch(as.character(length(brk)),
               "2"  = c(3, 8)           ,
               "4"  = c(1, 3, 6, 8)     ,
               "6"  = c(1, 3, 5, 6, 8, 10),
               "8"  = c(1:3, 4, 6:8, 10),
               "10" = 1:10               )
      } else {
        switch(as.character(length(brk)),
               "2"  = c(5, 10) ,
               "4"  = c(3, 5, 8, 10)    ,
               "6"  = c(3:5, 8:10)      ,
               "8"  = c(2:5, 7:10)      ,
               "10" = 1:10               )
      }


    color_styles <- color_styles_all[color_styles]

    color_selection <-
      purrr::map2(brk, brksup,
                  ~ color_formula(type_of_ci, color = color, allmean = allmean,
                                  diff = diff, ci = ci, ctr = ctr,
                                  brk = .x, brksup = .y)
      ) %>% purrr::set_names(names(color_styles))

    unselected <- purrr::transpose(color_selection) %>%
      purrr::map_lgl(~ ! any(purrr::flatten_lgl(.)))

    out[ok] <-
      purrr::reduce2(.init = out[ok], .x = color_selection, .y = color_styles,
                     ~ dplyr::if_else(..2, rlang::exec(..3, ..1), ..1) )

    totals <- get_reference_total(x, mode = "all_totals") #c("cells", "lines")

    out[ok & unselected & !totals] <- fmtgrey3(out[ok & unselected & !totals])

  } else {
    #problem with bold in console : it offsets all column unaesthetically
    if (any(totrows)) out <- dplyr::if_else(totrows & ! totcol,
                                            crayon::underline(out), out)
    if (totcol)       out <- dplyr::if_else(totrows,
                                            paste0(crayon::underline(out), "|"),
                                            paste0(out, "|"))

    out[!na_out] <- out[!na_out] %>%
      stringr::str_replace("^0%$|^-0%$", crayon::silver("0%")) %>% # 0 in gray
      stringr::str_replace("^0$|^0$", crayon::silver("0"))
  }

  pillar::new_pillar_shaft_simple(out, align = "right", na = "")
}



color_formula <- function(type_of_ci, color, allmean,
                          diff, ci, ctr, brk, brksup) {
  res <-
    switch(
      color,
      "diff"     =
        if( (!allmean & brk >= 0) | (allmean & brk >= 1) ) {
          diff > brk & diff < brksup} else {
            diff < brk & diff > brksup},
      "diff_ci"  = switch(
        type_of_ci,
        "diff_col"           = ,
        "diff_row"           = ,
        "diff_row_all"       =
          if(brk >= 0) {diff > brk & diff < brksup & abs(diff) - ci > 0} else {
            diff < brk & diff > brksup & abs(diff) - ci > 0},
        "diff_row_mean"      = ,
        "diff_row_all_mean"  =
          if(brk >= 1) {diff > brk & diff < brksup & abs(diff) - ci > 0} else {
            diff < brk & diff > brksup & abs(diff) - ci > 0},
        "spread_col"         = ,
        "spread_row"         = ,
        "spread_row_all"     =
          if(brk >= 0) {diff > brk & diff < brksup & ci > 0} else {
            diff < brk & diff > brksup & ci > 0},
        "spread_row_mean"    = ,
        "spread_row_all_mean"=
          if(brk >= 1) {diff > brk & diff < brksup & ci > 0} else {
            diff < brk & diff > brksup & ci > 0},
      ),
      "after_ci" = switch(
        type_of_ci,
        "diff_col"           = ,
        "diff_row"           = ,
        "diff_row_all"       =
          if(brk >= 0) {
            diff >= 0  &  abs(diff) - ci > brk   &  abs(diff) - ci <  brksup
          } else {
            diff  < 0  &  abs(diff) - ci > -brk  &  abs(diff) - ci < -brksup
          },
        "diff_row_all_mean"  = ,
        "diff_row_mean"      =
          if(brk >= 1) {
            diff >= 1  &  abs(diff) > ci * brk  &  abs(diff) < ci * brksup
          } else {
            diff  < 1  &  abs(diff) < ci * brk  &  abs(diff) > ci * brksup
          },
        "spread_col"         = ,
        "spread_row"         = ,
        "spread_row_all"     =
          if(brk >= 0) {diff >= 0 & ci >  brk & ci < brksup} else {
            diff < 0 & ci > -brk & ci < -brksup},
        "spread_row_mean"    = ,
        "spread_row_all_mean"=
          if(brk >= 1) {diff >= 1 & ci >  brk & ci < brksup} else {
            diff < 1 & ci < brk & ci > brksup},
      ),
      "ctr"     = if (brk >= 0) {ctr > brk & ctr < brksup} else {
        ctr < brk & ctr > brksup},
      rep(FALSE, length(diff))
    )
  tidyr::replace_na(res, FALSE)
}


get_reference_total <- function(x, mode = c("cells", "lines", "all_totals")) {
  pct         <- get_pct_type(x)
  allmean     <- all(get_type(x) == "mean")
  comp_all    <- get_comp_all(x)
  totcol      <- is_totcol(x)
  totrows     <- is_totrow(x)
  tottab_line <- is_tottab(x) & totrows

  switch(mode[1],
         "cells"      = dplyr::case_when(
           (allmean | pct == "row") & !comp_all ~ totrows & !totcol     ,
           (allmean | pct == "row") &  comp_all ~ tottab_line & !totcol ,
           pct == "col"                         ~ totcol & !totrows     ,
           pct == "all"                         ~ totrows & totcol      ,
           pct == "all_tabs"                    ~ tottab_line & totcol  ,
           TRUE                                 ~ rep(FALSE, length(x)  )
         ),
         "lines"      = dplyr::case_when(
           (allmean | pct == "row") & !comp_all ~ totrows               ,
           (allmean | pct == "row") &  comp_all ~ tottab_line           ,
           pct == "col"                         ~ rep(totcol, length(x)),
           pct == "all"                         ~ totrows & totcol      ,
           pct == "all_tabs"                    ~ tottab_line & totcol  ,
           TRUE                                 ~ rep(FALSE, length(x)  )
         ),
         "all_totals" = if(comp_all) tottab_line | totcol else totrows | totcol,
  )
}



is_RStudio <- function() Sys.getenv("RSTUDIO") == "1"
#.Platform$GUI == "RStudio"

is_dark <- ifelse(is_RStudio(), rstudioapi::getThemeInfo()$dark, FALSE)

# format.pillar_shaft_fmt <- function(x, width, ...) {
#   if (get_max_extent(x$deg_min) <= width) {
#     ornament <- x$deg_min
#   } else {
#     ornament <- x$deg
#   }
#
#   pillar::new_ornament(ornament, align = "right")
# }

# When methods don't work, there is still a way to convert fmt to double :
# fmt(0.712, 2) %>% vec_data() %>% vec_cast(double())


#pct = "col" calculation doesn't work (add cols, 200% !)

#Define abbreviated type name (for tibble::tibble headers)
#' @export
vec_ptype_abbr.fmt <- function(x, ...) {
  type <- get_type(x) %>% unique()
  type <- ifelse(length(type) > 1, "mixed", type)
  if (type %in% c("pct", "diff")){
    paste0(type, "-", get_pct_type(x))
  } else if (type == "ci" ) {
    paste0(type, "-", get_ci_type(x))
  } else {
    paste0("fmt-", type)
  }
}


# Include numbers of digits and types in the printed name
#' @export
vec_ptype_full.fmt <- function(x, ...) {
  type <- get_type(x) %>% unique()
  type <- ifelse(length(type) > 1, "mixed", type)
  #digits <- get_digits(x) %>% range() %>% unique() %>% paste0(collapse = ":")

  paste0("fmt-", type) #  paste0("fmt-", type, "-", digits)
}


#Make our fmt class coercible with herself, and back and forth with double and integer vectors :
#' @export
vec_ptype2.fmt.fmt    <- function(x, y, ...) new_fmt(
  col_var   = dplyr::if_else(get_col_var(x) == get_col_var(y),
                             true = get_col_var(x),
                             false  = vec_recycle("several_vars", length(get_col_var(x)))),
  totcol = dplyr::if_else(is_totcol(x) == is_totcol(y),
                          true = is_totcol(x),
                          false  = vec_recycle(FALSE, length(is_totcol(x)))),
  # nlvs   = dplyr::if_else(tab_get_nlvs(x)  == tab_get_nlvs(y),
  #                         true = tab_get_nlvs(x),
  #                         false  = integer()     ),
  pct_type  = dplyr::if_else(get_pct_type(x) == get_pct_type(y),
                             true = get_pct_type(x),
                             false  = vec_recycle("mixed", length(get_pct_type(x)))),
  ci_type = dplyr::if_else(get_ci_type(x) == get_ci_type(y),
                           true = get_ci_type(x),
                           false  = vec_recycle(NA_character_, length(get_ci_type(x)))),
  color = dplyr::if_else(get_color(x) == get_color(y),
                         true = get_color(x),
                         false  = vec_recycle(NA_character_, length(get_color(x)))),
  comp_all = dplyr::if_else(get_comp_all(x) == get_comp_all(y),
                            true = get_comp_all(x),
                            false  = vec_recycle(FALSE, length(get_comp_all(x)))),
)
#' @export
vec_ptype2.fmt.double  <- function(x, y, ...) x # new_fmt() #double()
#' @export
vec_ptype2.double.fmt  <- function(x, y, ...) y # new_fmt() #double()
#' @export
vec_ptype2.fmt.integer <- function(x, y, ...) x # fmt() #double()
#' @export
vec_ptype2.integer.fmt <- function(x, y, ...) y # new_fmt() #double()

# Conversions :
#' @export
vec_cast.fmt.fmt       <- function(x, to, ...) new_fmt(type   = get_type  (x),
                                                       n      = get_n     (x),
                                                       wn     = get_wn    (x),
                                                       pct    = get_pct   (x),
                                                       diff   = get_diff  (x),
                                                       digits = get_digits(x),
                                                       ctr    = get_ctr   (x),
                                                       mean   = get_mean  (x),
                                                       var    = get_var   (x),
                                                       ci     = get_ci    (x),

                                                       in_totrow = is_totrow(x),
                                                       in_tottab = is_tottab(x),

                                                       col_var   = get_col_var(to),
                                                       totcol    = is_totcol(to),
                                                       #nlvs      = tab_get_nlvs(to),
                                                       pct_type  = get_pct_type(to),
                                                       ci_type   = get_ci_type(to),
                                                       color     = get_color(to),
                                                       comp_all  = get_comp_all(to)
)

#' @export
vec_cast.fmt.double     <- function(x, to, ...) fmt(type = "wn", wn = x,
                                                    col_var  = get_col_var(to),
                                                    totcol   = is_totcol(to),
                                                    #nlvs     = tab_get_nlvs(to),
                                                    pct_type = get_pct_type(to),
                                                    ci_type  = get_ci_type(to) ,
                                                    color    = get_color(to),
                                                    comp_all = get_comp_all(to)
)
#' @method vec_cast.double fmt
#' @export
vec_cast.double.fmt     <- function(x, to, ...) get_num(x) %>% as.double() #field(x, "pct")

#' @export
vec_cast.fmt.integer    <- function(x, to, ...) fmt(n = x,
                                                    col_var  = get_col_var(to),
                                                    totcol   = is_totcol(to),
                                                    #nlvs     = tab_get_nlvs(to),
                                                    pct_type = get_pct_type(to),
                                                    ci_type  = get_ci_type(to),
                                                    color    = get_color(to),
                                                    comp_all = get_comp_all(to)
) #new_fmt(pct = as.double(x))
#' @method vec_cast.integer fmt
#' @export
vec_cast.integer.fmt    <- function(x, to, ...) get_num(x) %>% as.integer() #field(x, "pct") %>% as.integer()

#' @method vec_cast.character fmt
#' @export
vec_cast.character.fmt  <- function(x, to, ...) format(x)


#Comparisons and sorting :
#' @export
vec_proxy_equal.fmt   <- function(x, ...) {
  get_num(x)
}
#' @export
vec_proxy_compare.fmt <- function(x, ...) {
  get_num(x)
}

#Once you've implemented vec_ptype2() and vec_cast(), you get vec_c(), [<-, and [[<- implementations for free.
#You'll also get mostly correct behaviour for c().


#Arithmetic operations :

# Thank you very much it works perfectly (I had tried with ```@method```, but not consistently enougth to put it in the generic) !
# Just a detail : with ```vec_arith fmt default``` , I have a "Warning: [D:\... ] @method  can have at most 2 words"
# I replaced with ```vec_arith.fmt default``` and it worked.

#' Vec_arith method for fmt
#' @param op Operation to do.
#'
#' @param x fmt object.
#' @param y Second object.
#' @param ... Other parameter.
#'
#' @method vec_arith fmt
#' @export
vec_arith.fmt <- function(op, x, y, ...) {
  UseMethod("vec_arith.fmt", y)
}

#' @method vec_arith.fmt default
#' @export
vec_arith.fmt.default <- function(op, x, y, ...) {
  vec_arith_base(op, get_num(x), vec_data(y))
  #stop_incompatible_op(op, x, y)
}

# positive_double <- function(n) n * sign(n)
# positive_integer <- function(n) as.integer(n * sign(n))

#' @method vec_arith.fmt fmt
#' @export
vec_arith.fmt.fmt <- function(op, x, y, ...) {
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
    "-" = new_fmt(
      type   = get_type(x),      #dplyr::if_else(get_type(x) == get_type(x)), true = get_type(x), false = "n),
      n      = vec_arith_base(op, get_n(x)  , get_n(y)  ), #%>% positive_integer(),
      wn     = vec_arith_base(op, get_wn(x) , get_wn(y) ), #%>% positive_double(),
      pct    = ifelse(same_pct & x_pct != "col",
                      yes  = vec_arith_base(op, get_pct(x), get_pct(y)),
                      no = NA_real_) %>% tidyr::replace_na(NA_real_), #NA_real_
      diff   = rep_NA_real,
      digits = pmax(get_digits(x), get_digits(y)),
      ctr    = rep_NA_real, # ???
      mean   = vec_arith_base(op, get_mean(x) * get_wn(x), get_mean(y) * get_wn(y)) /
        vec_arith_base("+", get_wn(x) , get_wn(y) ),# weighted mean
      var    = rep_NA_real,
      ci     = rep_NA_real,

      in_totrow = is_totrow(x) & is_totrow(y), # Just x ?
      in_tottab = is_tottab(x) & is_tottab(y),

      col_var   = dplyr::if_else(same_var, get_col_var(x),
                                 vec_recycle("several_vars", length(get_col_var(x)))),
      totcol    = FALSE,
      #nlvs      = dplyr::if_else(same_var, tab_get_nlvs(x), integer()),
      pct_type  = dplyr::if_else(same_pct, get_pct_type(x),
                                 vec_recycle("mixed", length(get_pct_type(x)))),
      ci_type   = dplyr::if_else(get_ci_type(x) == get_ci_type(y),
                                 get_ci_type(x),
                                 vec_recycle(NA_character_, length(get_ci_type(x)))),
      color     = dplyr::if_else(get_color(x) == get_color(y),
                                 get_color(x),
                                 vec_recycle(NA_character_, length(get_color(x)))),
      comp_all = dplyr::if_else(get_comp_all(x) == get_comp_all(y),
                                true = get_comp_all(x),
                                false  = vec_recycle(FALSE, length(get_comp_all(x)))),
    ),
    "/" = ,
    "*" = new_fmt(
      type   = get_type(x),
      n      = get_n(x)   ,
      wn     = get_wn(x)  ,
      pct    = vec_arith_base(op, get_pct(x), get_pct(x)), #Remove multiplication ?
      diff   = rep_NA_real,
      digits = pmax(get_digits(x), get_digits(y)),
      ctr    = rep_NA_real,
      mean   = rep_NA_real,
      var    = rep_NA_real,
      ci     = rep_NA_real,

      in_totrow = is_totrow(x),
      in_tottab = is_tottab(x),

      col_var   = dplyr::if_else(same_var, get_col_var(x),
                                 vec_recycle("several_vars", length(get_col_var(x)))),
      totcol    = FALSE,
      #nlvs      = dplyr::if_else(same_var, tab_get_nlvs(x), integer()),
      pct_type  = dplyr::if_else(same_pct, get_pct_type(x),
                                 vec_recycle("mixed", length(get_pct_type(x)))),
      ci_type   = dplyr::if_else(get_ci_type(x) == get_ci_type(y),
                                 get_ci_type(x),
                                 vec_recycle(NA_character_, length(get_ci_type(x)))),
      color     = dplyr::if_else(get_color(x) == get_color(y),
                                 get_color(x),
                                 vec_recycle(NA_character_, length(get_color(x)))),
      comp_all = dplyr::if_else(get_comp_all(x) == get_comp_all(y),
                                true = get_comp_all(x),
                                false  = vec_recycle(FALSE, length(get_comp_all(x)))),
    ),
    stop_incompatible_op(op, x, y)
  )
}

#' @method vec_arith.fmt numeric
#' @export
vec_arith.fmt.numeric <- function(op, x, y, ...) {
  set_num(x, vec_arith_base(op, get_num(x), y))
  # new_fmt(pct    = vec_arith_base(op, field(x, "pct"), y),
  #          type   = field(x, "type"  ),
  #          digits = field(x, "digits"),
  #          n      = field(x, "n"     ),
  #          wn     = field(x, "wn"    ),
  #          var     = field(x, "var"    ),
  #          ci     = field(x, "ci"    )                     )
}


#' @method vec_arith.numeric fmt
#' @export
vec_arith.numeric.fmt <- function(op, x, y, ...) {
  set_num(y, vec_arith_base(op, x, get_num(y)))
  # new_fmt(pct    = vec_arith_base(op, x, field(y, "pct")),
  #          type   = field(y, "type"  ),
  #          digits = field(y, "digits"),
  #          n      = field(y, "n"     ),
  #          wn     = field(y, "wn"    ),
  #          var     = field(y, "var"    ),
  #          ci     = field(y, "ci"    )                     )
}

#' @method vec_arith.fmt MISSING
#' @export
vec_arith.fmt.MISSING <- function(op, x, y, ...) { #unary + and - operators
  switch(op,
         `-` = set_num(x, get_num(x) * -1),
         # new_fmt(pct    = field(x, "pct"   ) * -1,
         #              type   = field(x, "type"  ),
         #              digits = field(x, "digits"),
         #              n      = field(x, "n"     ),
         #              wn     = field(x, "wn"    ),
         #              var     = field(x, "var"    ),
         #              ci     = field(x, "ci"    )       ),
         `+` = x,
         stop_incompatible_op(op, x, y)
  )
}


#Mathematical operations :
# (direct operations on counts,
# automatically calculate weighted means for pct and means, erase var and ci)
#' @export
vec_math.fmt <- function(.fn, .x, ...) {
  if (!is.na(get_pct_type(.x) ) & get_pct_type(.x) == "mixed") warning(
    "operation ", op,
    " within a variable mixing different types of percentages : "
  )

  switch(.fn,
         "sum" = new_fmt(type   = get_type(.x)[1],
                         digits = min(get_digits(.x)),
                         n      = vec_math_base(.fn, get_n(.x)  , ...),
                         wn     = vec_math_base(.fn, get_wn(.x) , ...),
                         pct    = ifelse(! get_pct_type(.x) %in% c("row", "col"),
                                         yes = vec_math_base(.fn, get_pct(.x), ...),
                                         no  = NA_real_) %>%
                           tidyr::replace_na(NA_real_),
                         diff   = NA_real_,
                         ctr    = NA_real_,
                         mean   = vec_math_base("sum", get_mean(.x) * get_wn(.x), ...) /
                           vec_math_base("sum", get_wn(.x), ...),
                         var    = NA_real_,
                         ci     = NA_real_,

                         in_totrow = all(is_totrow(.x)),
                         in_tottab = all(is_tottab(.x)), #any ?

                         col_var   = get_col_var(.x),
                         totcol    = is_totcol(.x),
                         #nlvs      = tab_get_nlvs(.x),
                         pct_type  = get_pct_type(.x),
                         ci_type   = get_ci_type(.x),
                         comp_all  = get_comp_all(.x)
         ),
         "mean" = new_fmt(type   = get_type(.x)[1],
                          digits = max(get_digits(.x)),
                          n      = vec_math_base("sum", get_n(.x)  , ...),
                          wn     = vec_math_base("sum", get_wn(.x) , ...),
                          pct    = vec_math_base("sum", get_pct(.x) * get_wn(.x), ...) /
                            vec_math_base("sum", get_wn(.x), ...),
                          diff   = NA_real_,
                          ctr    = NA_real_,
                          mean   = vec_math_base("sum", get_mean(.x) * get_wn(.x), ...) /
                            vec_math_base("sum", get_wn(.x), ...),
                          var    = NA_real_,
                          ci     = NA_real_,

                          in_totrow = FALSE,
                          in_tottab = all(is_tottab(.x)), #any ?

                          col_var   = get_col_var(.x),
                          totcol    = is_totcol(.x),
                          #nlvs      = tab_get_nlvs(.x),
                          pct_type  = get_pct_type(.x),
                          ci_type   = get_ci_type(.x),
                          color     = get_color(.x),
                          comp_all  = get_comp_all(.x)
         ),
         vec_math_base(.fn, get_num(.x), ...) )
}









# c(fmt("n", 0), fmt("n", 0)) %>% is_totrow()
# c(fmt("n", 0), fmt("n", 0)) %>% is_tottab()



#Colors for printing fmt -------------------------------------------------------
pos5  <- crayon::make_style(rgb(0, 5, 0, maxColorValue = 5),
                            colors = crayon::num_colors(forget = TRUE)) #hcl(120, 200, 100)
pos4  <- crayon::make_style(rgb(1, 5, 1, maxColorValue = 5))
pos3  <- crayon::make_style(rgb(3, 5, 1, maxColorValue = 5))
pos2  <- crayon::make_style(rgb(4, 5, 1, maxColorValue = 5))
pos1  <- crayon::make_style(rgb(4, 4, 1, maxColorValue = 5)) #5, 5, 1

neg5  <- crayon::make_style(rgb(5, 0, 0, maxColorValue = 5))
neg4  <- crayon::make_style(rgb(5, 1, 0, maxColorValue = 5))
neg3  <- crayon::make_style(rgb(5, 2, 1, maxColorValue = 5))
neg2  <- crayon::make_style(rgb(5, 3, 1, maxColorValue = 5))
neg1  <- crayon::make_style(rgb(4, 3, 2, maxColorValue = 5))

fmtgrey4 <- crayon::make_style(grey(0.9), grey = TRUE)
fmtgrey3 <- crayon::make_style(grey(0.7), grey = TRUE)
fmtgrey2 <- crayon::make_style(grey(0.6), grey = TRUE)
fmtgrey1 <- crayon::make_style(grey(0.4), grey = TRUE)

posb5  <- crayon::make_style(rgb(0, 0, 5, maxColorValue = 5)) #hcl(120, 200, 100)
posb4  <- crayon::make_style(rgb(0, 1, 5, maxColorValue = 5))
posb3  <- crayon::make_style(rgb(0, 3, 5, maxColorValue = 5))
posb2  <- crayon::make_style(rgb(1, 4, 5, maxColorValue = 5))
posb1  <- crayon::make_style(rgb(2, 5, 5, maxColorValue = 5))

negb5  <- crayon::make_style(rgb(5, 0, 0, maxColorValue = 5))
negb4  <- crayon::make_style(rgb(5, 0, 1, maxColorValue = 5))
negb3  <- crayon::make_style(rgb(5, 1, 1, maxColorValue = 5))
negb2  <- crayon::make_style(rgb(5, 1, 3, maxColorValue = 5))
negb1  <- crayon::make_style(rgb(5, 2, 3, maxColorValue = 5))



# cat("\n",
#     pos1("42%" ), neg1("42%\n" ),
#     pos2("42%" ), neg2("42%\n" ),
#     pos3("42%" ), neg3("42%\n" ),
#     pos4("42%" ), neg4("42%\n" ),
#     pos5("42%" ), neg5("42%\n" ),
#     "\n",
#     posb1("42%"), negb1("42%\n" ),
#     posb2("42%" ), negb2("42%\n" ),
#     posb3("42%" ), negb3("42%\n" ),
#     posb4("42%" ), negb4("42%\n" ),
#     posb5("42%" ), negb5("42%\n" ),
#     "\n",
#     "42%"       ,       "42%\n" ,
#     fmtgrey4("42%"), fmtgrey4("42%\n"),
#     fmtgrey3("42%"), fmtgrey3("42%\n"),
#     fmtgrey2("42%"), fmtgrey2("42%\n"),
#     fmtgrey1("42%"), fmtgrey1("42%\n") )

bgpos5   <- crayon::make_style(rgb(0, 5, 0, maxColorValue = 5), bg = TRUE) #hcl(120, 200, 100)
bgpos4   <- crayon::make_style(rgb(0, 4, 0, maxColorValue = 5), bg = TRUE)
bgpos3   <- crayon::make_style(rgb(0, 3, 0, maxColorValue = 5), bg = TRUE)
bgpos2   <- crayon::make_style(rgb(0, 2, 0, maxColorValue = 5), bg = TRUE)
bgpos1   <- crayon::make_style(rgb(0, 1, 0, maxColorValue = 5), bg = TRUE) #5, 5, 1

bgneg5   <- crayon::make_style(rgb(5, 0, 0, maxColorValue = 5), bg = TRUE)
bgneg4   <- crayon::make_style(rgb(4, 0, 0, maxColorValue = 5), bg = TRUE)
bgneg3   <- crayon::make_style(rgb(3, 0, 0, maxColorValue = 5), bg = TRUE)
bgneg2   <- crayon::make_style(rgb(2, 0, 0, maxColorValue = 5), bg = TRUE)
bgneg1   <- crayon::make_style(rgb(1, 0, 0, maxColorValue = 5), bg = TRUE)

bgposb5  <- crayon::make_style(rgb(0, 0, 5, maxColorValue = 5), bg = TRUE) #hcl(120, 200, 100)
bgposb4  <- crayon::make_style(rgb(0, 0, 4, maxColorValue = 5), bg = TRUE)
bgposb3  <- crayon::make_style(rgb(0, 0, 3, maxColorValue = 5), bg = TRUE)
bgposb2  <- crayon::make_style(rgb(0, 0, 2, maxColorValue = 5), bg = TRUE)
bgposb1  <- crayon::make_style(rgb(0, 0, 1, maxColorValue = 5), bg = TRUE)

bgnegb5  <- crayon::make_style(rgb(5, 0, 0, maxColorValue = 5), bg = TRUE)
bgnegb4  <- crayon::make_style(rgb(4, 0, 0, maxColorValue = 5), bg = TRUE)
bgnegb3  <- crayon::make_style(rgb(3, 0, 0, maxColorValue = 5), bg = TRUE)
bgnegb2  <- crayon::make_style(rgb(2, 0, 0, maxColorValue = 5), bg = TRUE)
bgnegb1  <- crayon::make_style(rgb(1, 0, 0, maxColorValue = 5), bg = TRUE)

# cat("\n",
#     bgpos1("42%"  ), bgneg1("42%\n" ),
#     bgpos2("42%"  ), bgneg2("42%\n" ),
#     bgpos3("42%"  ), bgneg3("42%\n" ),
#     bgpos4("42%"  ), bgneg4("42%\n" ),
#     bgpos5("42%"  ), bgneg5("42%\n" ),
#     "\n",
#     bgposb1("42%" ), bgnegb1("42%\n" ),
#     bgposb2("42%" ), bgnegb2("42%\n" ),
#     bgposb3("42%" ), bgnegb3("42%\n" ),
#     bgposb4("42%" ), bgnegb4("42%\n" ),
#     bgposb5("42%" ), bgnegb5("42%\n" ),
#     "\n",
#     "42%"       ,       "42%\n" ,
#     fmtgrey4("42%"), fmtgrey4("42%\n"),
#     fmtgrey3("42%"), fmtgrey3("42%\n"),
#     fmtgrey2("42%"), fmtgrey2("42%\n"),
#     fmtgrey1("42%"), fmtgrey1("42%\n") )




# pos4  <- crayon::make_style(hsv(122/360, 1   , 1  ),
#                             colors = crayon::num_colors(forget = TRUE)) #hcl(120, 200, 100)
# pos3  <- crayon::make_style(hsv(122/360, 0.75, 1  ))
# pos2  <- crayon::make_style(hsv(122/360, 0.50, 1  ))
# pos1  <- crayon::make_style(hsv(122/360, 0.20, 0.8))
# #pos5  <- crayon::make_style(hsv(122/360, 0.20, 0.8))
#
# neg4  <- crayon::make_style(hsv( 27/360, 1   , 1  ))
# neg3  <- crayon::make_style(hsv( 27/360, 0.75, 1  ))
# neg2  <- crayon::make_style(hsv( 27/360, 0.50, 1  ))
# neg1  <- crayon::make_style(hsv( 27/360, 0.20, 0.8))
# #neg5  <- crayon::make_style(hsv( 27/360, 0.2, 0.8))
#
# fmtgrey3 <- crayon::make_style(grey(0.9), grey = TRUE)
# fmtgrey2 <- crayon::make_style(grey(0.6), grey = TRUE)
# fmtgrey1 <- crayon::make_style(grey(0.4), grey = TRUE)
#
#
# posb4  <- crayon::make_style(hsv(210/360, 1    , 1  )) #hcl(120, 200, 100)
# posb3  <- crayon::make_style(hsv(210/360, 0.755, 1  ))
# posb2  <- crayon::make_style(hsv(210/360, 0.550, 1  ))
# posb1  <- crayon::make_style(hsv(210/360, 0.355, 0.8))
# #posb5  <- crayon::make_style(hsv(210/360, 0.2, 0.8))
#
# negb4  <- crayon::make_style(hsv(0      , 1   , 1  ))
# negb3  <- crayon::make_style(hsv(0      , 0.75, 1  ))
# negb2  <- crayon::make_style(hsv(0      , 0.55, 1  ))
# negb1  <- crayon::make_style(hsv(0      , 0.35, 0.8))
# #negb5  <- crayon::make_style(hsv(0      , 0.2, 0.8))
#
# # cat("\n",
# #     pos1("42%" ), neg1("42%\n" ),
# #     pos2("42%" ), neg2("42%\n" ),
# #     pos3("42%" ), neg3("42%\n" ),
# #     pos4("42%" ), neg4("42%\n" ),
# #     #pos5("42%" ), neg5("42%\n" ),
# #     "\n",
# #     posb1("42%"), negb1("42%\n" ),
# #     posb2("42%" ), negb2("42%\n" ),
# #     posb3("42%" ), negb3("42%\n" ),
# #     posb4("42%" ), negb4("42%\n" ),
# #     #posb5("42%" ), negb5("42%\n" ),
# #     "\n",
# #     "42%"       ,       "42%\n" ,
# #     fmtgrey1("42%"), fmtgrey1("42%\n"),
# #     fmtgrey2("42%"), fmtgrey2("42%\n"),
# #     fmtgrey3("42%"), fmtgrey3("42%\n") )
#
# # crayon::show_ansi_colors()



# #Background colors
# bgpos4   <- crayon::make_style(hsv(122/360, 1   , 1  ), bg = TRUE)
# bgpos3   <- crayon::make_style(hsv(122/360, 0.7 , 0.7), bg = TRUE)
# bgpos2   <- crayon::make_style(hsv(122/360, 0.50, 0.5), bg = TRUE)
# bgpos1   <- crayon::make_style(hsv(122/360, 0.50, 0.3), bg = TRUE)
# bgpos5   <- crayon::make_style(hsv(122/360, 0.20, 0.8), bg = TRUE)
#
# bgneg4   <- crayon::make_style(hsv( 27/360, 1   , 1  ), bg = TRUE)
# bgneg3   <- crayon::make_style(hsv( 27/360, 0.7 , 1  ), bg = TRUE)
# bgneg2   <- crayon::make_style(hsv( 27/360, 0.50, 0.6), bg = TRUE)
# bgneg1   <- crayon::make_style(hsv( 27/360, 0.50, 0.3), bg = TRUE)
# bgneg5   <- crayon::make_style(hsv( 27/360, 0.2 , 0.8), bg = TRUE)
#
# bgposb4  <- crayon::make_style(hsv(210/360, 1   , 1  ), bg = TRUE)
# bgposb3  <- crayon::make_style(hsv(210/360, 0.7 , 1  ), bg = TRUE)
# bgposb2  <- crayon::make_style(hsv(210/360, 0.50, 0.6), bg = TRUE)
# bgposb1  <- crayon::make_style(hsv(210/360, 0.50, 0.3), bg = TRUE)
# bgposb5  <- crayon::make_style(hsv(210/360, 0.30 , 0.8), bg = TRUE)
#
# bgnegb4  <- crayon::make_style(hsv(0      , 1   , 1  ), bg = TRUE)
# bgnegb3  <- crayon::make_style(hsv(0      , 0.7 , 1  ), bg = TRUE)
# bgnegb2  <- crayon::make_style(hsv(0      , 0.50, 0.6), bg = TRUE)
# bgnegb1  <- crayon::make_style(hsv(0      , 0.50, 0.3), bg = TRUE)
# bgnegb5  <- crayon::make_style(hsv(0      , 0.2  , 0.8), bg = TRUE)
#
#
# # cat("\n",
# #     bgpos1("42%"  ), bgneg1("42%\n"  ),
# #     bgpos2("42%"  ), bgneg2("42%\n"  ),
# #     bgpos3("42%"  ), bgneg3("42%\n"  ),
# #     bgpos4("42%"  ), bgneg4("42%\n"  ),
# #     #bgpos5$black("42%"  ), bgneg5$black("42%\n"  ),
# #     "\n",
# #     bgposb1("42%" ), bgnegb1("42%\n" ),
# #     bgposb2("42%" ), bgnegb2("42%\n" ),
# #     bgposb3("42%" ), bgnegb3("42%\n" ),
# #     bgposb4("42%" ), bgnegb4("42%\n" ),
# #     #bgposb5$black("42%" ), bgnegb5$black("42%\n" ),
# #     "\n",
# #     "42%"        ,       "42%\n"  ,
# #     fmtgrey1("42%") , fmtgrey1("42%\n" ),
# #     fmtgrey2("42%") , fmtgrey2("42%\n" ),
# #     fmtgrey3("42%") , fmtgrey3("42%\n" ) )

green_red <-
  c(pos1 = "pos1", pos2 = "pos2", pos3 = "pos3", pos4 = "pos4", pos5 = "pos5",
    neg1 = "neg1", neg2 = "neg2", neg3 = "neg3", neg4 = "neg4", neg5 = "neg5" )
blue_red <-
  c(pos1 = "posb1", pos2 = "posb2", pos3 = "posb3", pos4 = "posb4", pos5 = "posb5",
    neg1 = "negb1", neg2 = "negb2", neg3 = "negb3", neg4 = "negb4", neg5 = "negb5" )
green_red_bg <-
  c(pos1 = "bgpos1", pos2 = "bgpos2", pos3 = "bgpos3", pos4 = "bgpos4", pos5 = "bgpos5",
    neg1 = "bgneg1", neg2 = "bgneg2", neg3 = "bgneg3", neg4 = "bgneg4", neg5 = "bgneg5" )
blue_red_bg <-
  c(pos1 = "bgposb1", pos2 = "bgposb2", pos3 = "bgposb3", pos4 = "bgposb4", pos5 = "bgposb5",
    neg1 = "bgnegb1", neg2 = "bgnegb2", neg3 = "bgnegb3", neg4 = "bgnegb4", neg5 = "bgnegb5"  )
greys <-
  c(pos1 = "fmtgrey1", pos2 = "fmtgrey2", pos3 = "fmtgrey3", pos4 = "white")

tab_color_style <-
  function(styles = c("green_red", "blue_red",
                      "green_red_bg", "blue_red_bg")
  ) {
    assign("color_styles_all", rlang::eval_tidy(rlang::sym(styles[1])),
           pos = "package:tablr" #globalenv() #
    )
  }
color_styles_all <- green_red

#Color breaks for printing tabs ------------------------------------------------

pct_breaks      <- c(0.05, 0.1, 0.2, 0.3)
mean_breaks     <- c(1.15, 1.5, 2, 4)
contrib_breaks  <- c(1, 2, 5, 10)

pct_ci_breaks   <- pct_breaks - pct_breaks[1]
mean_ci_breaks  <- mean_breaks / mean_breaks[1]

pct_brksup      <- c(pct_breaks    [2:length(pct_breaks)    ], Inf)
mean_brksup     <- c(mean_breaks   [2:length(mean_breaks)   ], Inf)
contrib_brksup  <- c(contrib_breaks[2:length(contrib_breaks)], Inf)
pct_ci_brksup   <- c(pct_ci_breaks [2:length(pct_ci_breaks) ], Inf)
mean_ci_brksup  <- c(mean_ci_breaks[2:length(mean_ci_breaks)], Inf)

pct_brk         <- pct_breaks     %>% c(., -.)
mean_brk        <- mean_breaks    %>% c(., 1/.)
contrib_brk     <- contrib_breaks %>% c(., -.)
pct_ci_brk      <- pct_ci_breaks  %>% c(., -.) #not same as tab_xl
mean_ci_brk     <- mean_ci_breaks %>% c(., 1/.)

pct_brksup      <- pct_brksup     %>% c(., -.)
mean_brksup     <- mean_brksup    %>% c(., 1/.)
contrib_brksup  <- contrib_brksup %>% c(., -.)
pct_ci_brksup   <- pct_ci_brksup  %>% c(., -.) #not same as tab_xl
mean_ci_brksup  <- mean_ci_brksup %>% c(., 1/.)

#' @export
get_color_breaks <- function(brk) {
  if (missing(brk)) {
    return(
      list(pct_breaks = pct_breaks, mean_breaks = mean_breaks,
           contrib_breaks = contrib_breaks, pct_ci_breaks = pct_ci_breaks,
           mean_ci_breaks = mean_ci_breaks)
    )
  }
  switch (brk,
          "pct"     = pct_breaks    ,
          "mean"    = mean_breaks   ,
          "contrib" = contrib_breaks,
          "pct_ci"  = pct_ci_breaks ,
          "mean_ci" = mean_ci_breaks
  )
}

#' @export
set_color_breaks <- function(pct_breaks, mean_breaks, contrib_breaks) {

  if (!missing(pct_breaks)) {
    stopifnot(is.numeric(pct_breaks)     ,
              length(pct_breaks)     <= 5,
              all(pct_breaks         >= 0))
    pct_ci_breaks   <- pct_breaks - pct_breaks[1]
    pct_brk         <- pct_breaks     %>% c(., -.)
    pct_brksup      <- c(pct_breaks   [2:length(pct_breaks)    ], Inf)
    pct_brksup      <- pct_brksup     %>% c(., -.)
    pct_ci_brk      <- pct_ci_breaks  %>% c(., .)
    pct_ci_brksup   <- c(pct_ci_breaks[2:length(pct_ci_breaks) ], Inf)
    pct_ci_brksup   <- pct_ci_brksup  %>% c(., .)
    assign("pct_breaks"   , pct_breaks   , pos = "package:tablr")
    assign("pct_ci_breaks", pct_ci_breaks, pos = "package:tablr")
    assign("pct_brk"      , pct_brk      , pos = "package:tablr")
    assign("pct_brksup"   , pct_brksup   , pos = "package:tablr")
    assign("pct_ci_brk"   , pct_ci_brk   , pos = "package:tablr")
    assign("pct_ci_brksup", pct_ci_brksup, pos = "package:tablr")
  }

  if (!missing(mean_breaks)) {
    stopifnot(is.numeric(mean_breaks)    ,
              length(mean_breaks)    <= 5,
              all(mean_breaks        >= 0))
    mean_ci_breaks  <- mean_breaks / mean_breaks[1]
    mean_brk        <- mean_breaks    %>% c(., 1/.)
    mean_brksup     <- c(mean_breaks   [2:length(mean_breaks)   ], Inf)
    mean_brksup     <- mean_brksup    %>% c(., 1/.)
    mean_ci_brk     <- mean_ci_breaks %>% c(., 1/.)
    mean_ci_brksup  <- c(mean_ci_breaks[2:length(mean_ci_breaks)], Inf)
    mean_ci_brksup  <- mean_ci_brksup %>% c(., 1/.)
    assign("mean_breaks"   , mean_breaks   , pos = "package:tablr")
    assign("mean_ci_breaks", mean_ci_breaks, pos = "package:tablr")
    assign("mean_brk"      , mean_brk      , pos = "package:tablr")
    assign("mean_brksup"   , mean_brksup   , pos = "package:tablr")
    assign("mean_ci_brk"   , mean_ci_brk   , pos = "package:tablr")
    assign("mean_ci_brksup", mean_ci_brksup, pos = "package:tablr")
  }

  if (!missing(contrib_breaks)) {
    stopifnot(is.numeric(contrib_breaks) ,
              length(contrib_breaks) <= 5,
              all(contrib_breaks     >= 0))
    contrib_brk     <- contrib_breaks %>% c(., -.)
    contrib_brksup  <- c(contrib_breaks[2:length(contrib_breaks)], Inf)
    contrib_brksup  <- contrib_brksup %>% c(., -.)
    assign("contrib_breaks", contrib_breaks, pos = "package:tablr")
    assign("contrib_brk"   , contrib_brk   , pos = "package:tablr")
    assign("contrib_brksup", contrib_brksup, pos = "package:tablr")
  }
}

#' @keywords internal
get_full_color_breaks <- function() {
  list(pct_brk        = pct_brk       ,
       pct_brksup     = pct_brksup    ,
       pct_ci_brk     = pct_ci_brk    ,
       pct_ci_brksup  = pct_ci_brksup ,
       mean_brk       = mean_brk      ,
       mean_brksup    = mean_brksup   ,
       mean_ci_brk    = mean_ci_brk   ,
       mean_ci_brksup = mean_ci_brksup,
       contrib_brk    = contrib_brk   ,
       contrib_brksup = contrib_brksup )
}

# get_color_breaks()
#
# set_color_breaks(pct_breaks = c(0.05, 0.10, 0.15, 0.25, 0.35))

# get_full_color_breaks()

# pct_breaks     = c(0.05, 0.10, 0.15, 0.25, 0.35)
# mean_breaks    = c(1.15, 1.25, 1.5 , 2   , 4   )
# contrib_breaks = c(0.5 , 1   , 2   , 5   , 10  )



# #Tests (not the right syntax)-----------------------------------------------------------
# #test of class :
# fmt(0.5) %>% class()
# fmt(0.5) %>% is_fmt()
# fmt(0.5) %>% is.double()
# fmt(0.5) %>% is.numeric()
#
# #test of printing :
# num1 <- fmt(c(0.7, 1.2, 7), c("n", "pct", "mean"),
#              c(0,1,2) , n = c(5, 10, 15), wn = c(4.7, 12.1, 13.9) )
# num2 <- fmt(c(0.3, 0.8, 3), c("n", "pct", "mean"),
#              c(2,1,0) , n = c(15, 10, 5), wn = c(13.9, 12.1, 4.7) )
# tibble::tibble(num1)
# fmt(1)   %>% vec_data()
#
# #test of common type :
# vec_ptype_show(fmt(0.255, "pct", 2), fmt(0.988, "pct", 0))
# vec_ptype_show(fmt(), double(), fmt())
# vec_ptype_common(fmt(0.255, "pct", 2), fmt(0.988, "pct", 0))
# vec_ptype2(fmt(0.255, "pct", 2), fmt(0.988, "pct", 0))
#
# #test of conversion :
# vec_cast(fmt(0.255, "pct", 2), fmt(0.988, "pct", 0))
# vec_cast(5, fmt())
# vec_cast(5L, fmt())
# vec_cast(fmt(6), double())
# vec_cast(fmt(6), integer())
# vec_cast(fmt(0.6005, "pct", 1), character())
# vec_cast(NA, fmt())
#
# #test of combination :
# vec_c(fmt(0.255, "pct", 2), fmt(0.988, "pct", 0)) #%>% get_digits()
# c(fmt(1), fmt(2))
# vec_c(fmt(3), 1)
# vec_c(fmt(3), 1L)
# vec_c(NA, fmt(4))
#
# #test of comparison :
# fmt(1) == 1
# sort(c(fmt(2), fmt(1)))
#
# #test of arithmetics :
# (fmt(0.5, "n"   , 0, 5 ,  5.1) + fmt(0.25000001, "n"   , 0,  1,  1.5 )) %>% vec_data()
# (fmt(0.5, "pct" , 1, 15, 15.1) - fmt(0.25000001, "mean", 0,  2,  2.5 )) %>% vec_data()
# (fmt(0.5, "pct" , 1, 15, 15.1) - fmt(0.75000001, "mean", 0, 20, 20.5 )) %>% vec_data()
# (fmt(0.5, "mean", 2, 25, 25.1) / fmt(0.25000001, "pct" , 3,  3,  3.5 )) %>% vec_data()
# (fmt(0.5, "n"   , 3, 35, 35.1) * fmt(0.25000001, "pct" , 0,  4,  4.5 )) %>% vec_data()
# (fmt(0.5, "pct" , 4, 45, 45.1) + 0.0077778) %>% vec_data()
# (fmt(0.5, "mean", 3, 55, 55.1) - 1)
# (fmt(0.5, "n"   , 2, 65, 65.1) / 2)
# fmt(0.5, "pct"  ,-1, 75, 75.1) * 3
# fmt(0.5) + 1
# 1 + fmt(0.5, "pct")
# 1 - fmt(0.5, "pct")
# 2 / fmt(3  , "pct")
# 5 * fmt(0.5, "n", 2)
# -fmt(0.5, "pct")
# (fmt(0.5) + 0.0077778) %>% as.double()
# sum(fmt(1), fmt(0.5)) %>% vec_data()
# mean(c(fmt(1, "n", 2), fmt(0.5, "n", 2))) %>% vec_data()
#
#
# Ok from here ----
# x <- fmt("pct", n = c(2, 1), pct = c(0.5, 1.5)) #wn = c(0.7, 2.4)
# y <- fmt("n"  , n = c(3, 9), pct = c(0.5, 1.5)) #wn = c(0.7, 2.4)
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
# get_var(x)
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


