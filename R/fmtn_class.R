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
#' @examples num1 <- fmt(c(0.7, 1.2, 7), display = c("n", "pct", "mean"), digits = c(0, 1, 2),
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
#' @param display The display of printing or background calculation, "n" for counts, "pct" for
#' percentages, "mean" for means.
#' @param digits The number of digits, as an integer or integer vector of the length of the
#' data. It can then be changed with \code{vctrs::`field<-`(x, "digits", value)}.
#' @param n The underlying count, as an integer vector of the same length, used with
#' display = "pct" to calculate confidence intervals.
#' @param wn The underlying weighted counts, as an integer vector of the same length. It
#' it used in certain operations on \code{\link{fmt}}, like means.
#' @param var The standards deviations, used with display = "mean" to calculate confidence
#' intervals.
#' @param ci The confidence intervals to print, if they have been calculated.
#'
#' @return A numeric vector of class fmt.
#' @export
#'
#' @examples fmt(0.7, "pct")
fmt <- function(n         = integer(),
                type      = "n",

                digits    = rep(0L      , length(n)),
                display   = dplyr::case_when(
                  type == "mean"                                ~ "mean",
                  type %in% c("row", "col", "all", "all_tabs")  ~ "pct" ,
                  TRUE                                          ~ "n"    ),

                wn        = rep(NA_real_, length(n)),
                pct       = rep(NA_real_, length(n)),
                mean      = rep(NA_real_, length(n)),
                diff      = rep(NA_real_, length(n)),
                ctr       = rep(NA_real_, length(n)),
                var       = rep(NA_real_, length(n)),
                ci        = rep(NA_real_, length(n)),

                in_totrow = rep(FALSE, length(n)),
                in_tottab = rep(FALSE, length(n)),
                in_refrow = rep(FALSE, length(n)),


                comp_all  = NA   ,
                diff_type = ""   ,
                ci_type   = ""   ,
                col_var   = ""   ,
                totcol    = FALSE,
                refcol    = FALSE,
                color     = ""    ) {

  max_size <- list(n, wn, pct, digits, ctr, mean, var, ci) %>% #display
    purrr::map_int(length) %>% max()

  display <- vec_recycle(vec_cast(display, character()), size = max_size)
  n       <- vec_recycle(vec_cast(n      , integer())  , size = max_size)
  wn      <- vec_recycle(vec_cast(wn     , double())   , size = max_size) #anything coercible as a double
  pct     <- vec_recycle(vec_cast(pct    , double())   , size = max_size)
  diff    <- vec_recycle(vec_cast(diff   , double())   , size = max_size)
  digits  <- vec_recycle(vec_cast(digits , integer())  , size = max_size)
  ctr     <- vec_recycle(vec_cast(ctr    , double())   , size = max_size)
  mean    <- vec_recycle(vec_cast(mean   , double())   , size = max_size)
  var     <- vec_recycle(vec_cast(var    , double())   , size = max_size)
  ci      <- vec_recycle(vec_cast(ci     , double())   , size = max_size)

  in_totrow <- vec_recycle(vec_cast(in_totrow, logical()), size = max_size)
  in_tottab <- vec_recycle(vec_cast(in_tottab, logical()), size = max_size)
  in_refrow <- vec_recycle(vec_cast(in_refrow, logical()), size = max_size)

  type      <- vec_recycle(vec_cast(type     , character()), size = 1)
  comp_all  <- vec_recycle(vec_cast(comp_all , logical()  ), size = 1)
  diff_type <- vec_recycle(vec_cast(diff_type, character()), size = 1)
  ci_type   <- vec_recycle(vec_cast(ci_type  , character()), size = 1)
  col_var   <- vec_recycle(vec_cast(col_var  , character()), size = 1)
  totcol    <- vec_recycle(vec_cast(totcol   , logical()  ), size = 1)
  refcol    <- vec_recycle(vec_cast(totcol   , logical()  ), size = 1)
  color     <- vec_recycle(vec_cast(color    , character()), size = 1)

  new_fmt(n = n, display = display, digits = digits,
          wn = wn, pct = pct,  mean = mean,
          diff = diff, ctr = ctr,var = var, ci = ci,
          in_totrow = in_totrow, in_tottab = in_tottab, in_refrow = in_refrow,
          type = type, comp_all = comp_all,  diff_type = diff_type,
          ci_type = ci_type, col_var = col_var, totcol = totcol, refcol = refcol,
          color = color) #nlvs = nlvs,
}



# Constructor :
#' @describeIn fmt A constructor for class fmt.
#' @export
new_fmt <- function(n         = integer(),
                    type      = "n"          ,

                    digits    = rep(0L      , length(n)),
                    display   = dplyr::case_when(
                      type == "mean"                                ~ "mean",
                      type %in% c("row", "col", "all", "all_tabs")  ~ "pct" ,
                      TRUE                                          ~ "n"    ),

                    wn        = rep(NA_real_, length(n)),
                    pct       = rep(NA_real_, length(n)),
                    mean      = rep(NA_real_, length(n)),
                    diff      = rep(NA_real_, length(n)),
                    ctr       = rep(NA_real_, length(n)),
                    var       = rep(NA_real_, length(n)),
                    ci        = rep(NA_real_, length(n)),

                    in_totrow = rep(FALSE   , length(n)),
                    in_tottab = rep(FALSE   , length(n)),
                    in_refrow = rep(FALSE   , length(n)),

                    comp_all  = NA   ,
                    diff_type = ""   ,
                    ci_type   = ""   ,
                    col_var   = ""   ,
                    totcol    = FALSE,
                    refcol    = FALSE,
                    color     = ""
) {
  # stopifnot(
  #   all(display %in% c("n", "wn", "pct", "pct_ci", "ctr", "mean", "mean_ci", "var", "ci")),
  #   type %in% c("row", "col", "all", "all_tabs", "mixed", NA_character_)
  # )

  # list(display, n, wn, pct, digits, ctr, mean, var, ci, col_var, totcol, type) %>%
  #   purrr::map(print)
  # cat("\n")

  #vec_assert(display, character()) #check display or size
  display <- vec_recycle(display, size = length(n))
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
  # vec_assert(type    , character(), size = 1)
  # vec_assert(comp_all, logical()  , size = 1)
  # vec_assert(ci_type , character(), size = 1)
  # vec_assert(col_var , character(), size = 1)
  # vec_assert(totcol  , logical()  , size = 1)
  # vec_assert(color   , character(), size = 1)

  new_rcrd(
    list(n = n, display = display, digits = digits,
         wn = wn, pct = pct, mean = mean,
         diff = diff, ctr = ctr, var = var, ci = ci,
         in_totrow = in_totrow, in_tottab = in_tottab,
         in_refrow = in_refrow),
    type = type, comp_all = comp_all, diff_type = diff_type,
    ci_type = ci_type, col_var = col_var, totcol = totcol, refcol = refcol,
    color = color,  class = "fmt")
  #access with fields() n_fields() field() `field<-`() ; vec_data() return the tibble with all fields
}

fmt0 <- function(display = "n", digits = 0, type = "n") {
  new_fmt(n = 0L, display = display, digits = as.integer(digits), type = type)
  # switch (display,
  #   "n"       = new_fmt(display = display, n = 0L,                           digits = as.integer(digits)),
  #   "wn"      = new_fmt(display = display, n = 0L, wn = 0,                   digits = as.integer(digits)),
  #   "pct"     = ,
  #   "pct_ci"  = new_fmt(display = display, n = 0L, wn = 0, pct = 0,          digits = as.integer(digits)),
  #   "ctr"     = new_fmt(display = display, n = 0L, wn = 0, pct = 0, ctr = 0, digits = as.integer(digits)),
  #   "mean"    = ,
  #   "mean_ci" = new_fmt(display = display, n = 0L, wn = 0, mean = 0, var = 0, digits = as.integer(digits)),
  #   "var"      = new_fmt(display = display, n = 0L, wn = 0, mean = 0, var = 0, digits = as.integer(digits)),
  #   "ci"      = new_fmt(display = display, n = 0L, ci = 0,                   digits = as.integer(digits)),
  # )
}
#' @describeIn fmt A test function for class fmt.
#' @export
is_fmt <- function(x) {
  inherits(x, "fmt")
}

# switch(display,
#        "n")
#
# switch(type,
#        "mean",
#        "row",
#        "col",
#        "all",
#        "all_tabs",
#        "n"
# )
#
# switch(ci_type,
#        "cell",
#        "diff",
#        NA_character_)


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
# as_fmt.default <- function(x, digits = rep(0L, length(x)), #display = rep("count", length(x)),
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
get_display<- fmt_field_factory("display")
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



get_mean_contrib <- function(x) {
  comp    <- get_comp_all(x)
  totrows <- is_totrow(x)
  tottabs <- is_tottab(x)
  ctr     <- get_ctr(x)

  if (!any(totrows)) return(rep(NA_real_, length(x)))

  if (comp) {
    rep(ctr[totrows & tottabs], length(x))
  } else {
    tibble::tibble(
      ctr = ctr,
      gr = cumsum(as.integer(totrows)) - as.integer(totrows) ) %>%
      dplyr::mutate(nb = dplyr::row_number()) %>%
      dplyr::with_groups(gr, ~ dplyr::mutate(., nb = dplyr::last(nb))) %>%
      dplyr::mutate(mean_ctr = ctr[nb]) %>% dplyr::pull(mean_ctr)
  }
}

#' @keywords internal
get_ref_means <- function(x) {
  comp      <- get_comp_all(x)
  diff_type <- get_diff_type(x)

  refrows <- if (diff_type == "first") { is_refrow(x) } else { is_totrow(x) }
  tottabs <- is_tottab(x)
  mean    <- get_mean(x)

  if (comp) {
    rep(mean[refrows & tottabs], length(x))
  } else {
    tibble::tibble(
      mean = mean,
      gr = cumsum(as.integer(refrows)) - as.integer(refrows) ) %>%
      dplyr::mutate(nb = dplyr::row_number()) %>%
      dplyr::with_groups(gr, ~ dplyr::mutate(., nb = dplyr::last(nb))) %>%
      dplyr::mutate(ref_means = mean[nb]) %>% dplyr::pull(ref_means)
  }
}

#Detect cells in total rows
#' @export
is_totrow <- function(x, ...) UseMethod("is_totrow")
#' @export
is_totrow.default  <-  function(x, ...) rep(FALSE, length(x)) #{
#' @export
is_totrow.fmt <- function(x, ...) field(x, "in_totrow")
#' @export
is_totrow.data.frame <- function(x, ..., partial = FALSE) {
  totrow_cells_test <- dplyr::ungroup(x) %>% dplyr::select(where(is_fmt)) %>%
    purrr::map_df(~ is_totrow(.))

  if (partial == TRUE) {
    totrow_cells_test %>%
      dplyr::rowwise() %>% dplyr::transmute(partial = any(dplyr::c_across())) %>%
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

#Detect cells in reference rows
#' @export
is_refrow <- function(x, ...) UseMethod("is_refrow")
#' @export
is_refrow.default  <-  function(x, ...) rep(FALSE, length(x)) #{
#' @export
is_refrow.fmt <- function(x, ...) field(x, "in_refrow")
#' @export
is_refrow.data.frame <- function(x, ..., partial = TRUE) {
  refrow_cells_test <- dplyr::ungroup(x) %>% dplyr::select(where(is_fmt)) %>%
    purrr::map_df(~ is_refrow(.))

  if (partial == TRUE) {
    refrow_cells_test %>%
      dplyr::rowwise() %>% dplyr::transmute(partial = any(dplyr::c_across())) %>%
      dplyr::pull(partial)
  } else {
    test_result <- refrow_cells_test %>%
      dplyr::rowwise() %>%
      dplyr::transmute(complete = all(dplyr::c_across()),
                       partial  = any(dplyr::c_across()) & ! complete)
    if (any(test_result$partial)) {
      warning("partial total rows (with some fmt cells not tagged 'refrow') ",
              "were not taken into account ")
    }
    test_result$complete
  }
}

#Detect cells in total tables
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


# Get types (for fmt or tab)
#' @export
get_type <- function(x, ...) UseMethod("get_type")
#' @export
get_type.default     <- function(x, ...) {
  ifelse(! is.null(purrr::attr_getter("type")(x)),
         yes = purrr::attr_getter("type")(x),
         no  = "") #NA_character_
}
#' @export
get_type.fmt <- purrr::attr_getter("type")
#' @export
get_type.data.frame <- function(x, ...) purrr::map_chr(x, ~ get_type(.))


# cols_by_type_pct <- function(tabs) {
#   types <-
#     purrr::map_if(tabs, is_fmt, ~ get_type(.), .else = ~ NA_character_) %>%
#     purrr::map_chr(~ dplyr::if_else(length(unique(.)) > 1, "mixed", unique(.)))
#
#   purrr::map2_chr(get_type(tabs), types, function(.pct, .type)
#     dplyr::if_else(.type == "pct", .pct, .type)  )
# }


get_comp_all <- function(x, replace_na = TRUE) {
  comp <- attr(x, "comp_all", exact = TRUE)
  if (is.null(comp)) return(NA)
  if (replace_na & is.na(comp)) comp <- FALSE
  comp
}

# Get differences types (for fmt or tab)
#' @export
get_diff_type <- function(x, ...) UseMethod("get_diff_type")
#' @export
get_diff_type.default     <- function(x, ...) {
  ifelse(! is.null(purrr::attr_getter("diff_type")(x)),
         yes = purrr::attr_getter("diff_type")(x),
         no  = "") #NA_character_
}
#' @export
get_diff_type.fmt <- purrr::attr_getter("diff_type")
#' @export
get_diff_type.data.frame <- function(x, ...) {
  purrr::map_chr(x, ~ get_diff_type(.))
}


# Get confidence intervals types (for fmt or tab)
#' @export
get_ci_type <- function(x, ...) UseMethod("get_ci_type")
#' @export
get_ci_type.default     <- function(x, ...) {
  ifelse(! is.null(purrr::attr_getter("ci_type")(x)),
         yes = purrr::attr_getter("ci_type")(x),
         no  = "") #NA_character_
}
#' @export
get_ci_type.fmt <- purrr::attr_getter("ci_type")
#' @export
get_ci_type.data.frame <- function(x, ...) {
  purrr::map_chr(x, ~ get_ci_type(.))
}


# Get names of column variable (for fmt or tab)
#' @export
get_col_var <- function(x, ...) UseMethod("get_col_var")
#' @export
get_col_var.default     <- function(x, ...) {
  ifelse(! is.null(purrr::attr_getter("col_var")(x)),
         yes = purrr::attr_getter("col_var")(x),
         no  = "") #NA_character_
}
#' @export
get_col_var.fmt <- purrr::attr_getter("col_var")
#' @export
get_col_var.data.frame <- function(x, ...) purrr::map_chr(x, ~ get_col_var(.))

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
is_totcol.data.frame <- function(x, ...) purrr::map_lgl(x, ~ is_totcol(.))

# Detect reference columns (for fmt or tab)
#' @export
is_refcol <- function(x, ...) UseMethod("is_refcol")
#' @export
is_refcol.default     <- function(x, ...) {
  ifelse(! is.null(purrr::attr_getter("refcol")(x)),
         yes = purrr::attr_getter("refcol")(x),
         no  = FALSE)
}
#' @export
is_refcol.fmt <- purrr::attr_getter("refcol")
#' @export
is_refcol.data.frame <- function(x, ...) purrr::map_lgl(x, ~ is_refcol(.))


#For each column, detect which total column it depends on
detect_totcols <- function(tabs) {
  #detect totcols by col vars names, no position ? ----
  tot <- which(is_totcol(tabs))

  purrr::map(1:ncol(tabs), function(.i)
    tidyr::replace_na(names(tot[tot >= .i])[1], "")) %>%
    rlang::syms() %>%
    purrr::set_names(names(tabs))
}

detect_firstcol <- function(tabs) {
  col_vars <- get_col_var(tabs)
  firstcol <- which(col_vars != dplyr::lag(col_vars, default = NA_character_))
  if (any(col_vars == "all_col_vars")) firstcol <- firstcol %>%
    purrr::discard(names(.) == names(col_vars)[col_vars == "all_col_vars"])

  res <- purrr::map(1:ncol(tabs), function(.i)
    tidyr::replace_na(
      dplyr::last(names(firstcol[firstcol <= .i]) ),
      "")) %>%
    rlang::syms() %>%
    purrr::set_names(names(tabs))

  if (any(col_vars == "all_col_vars")) {
    #   res_all_col <- tabs[as.character(res[col_vars == "all_col_vars"])]
    #
    # if (get_type(res_all_col) == "mean") res[col_vars == "all_col_vars"] <-
    #     rlang::syms("")
    res[col_vars == "all_col_vars"] <- rlang::syms("")
  }
  res
}


# Get color (for fmt or tab)
#' @export
get_color <- function(x, ...) UseMethod("get_color")
#' @export
get_color.default     <- function(x, ...) {
  ifelse(! is.null(purrr::attr_getter("color")(x)),
         yes = purrr::attr_getter("color")(x),
         no  = "") #NA_character_
}
#' @export
get_color.fmt <- purrr::attr_getter("color")
#' @export
get_color.data.frame <- function(x, ...) {
  purrr::map_chr(x, ~ get_color(.))
}



fmt_set_field_factory <- function(x, cast) {
  function(fmt, value) {
    value <- vec_cast(value, cast) %>% vec_recycle(size = length(fmt))
    `field<-`(fmt, x, value)
  }
}
set_display <- fmt_set_field_factory("display", cast = character())
set_n       <- fmt_set_field_factory("n"      , cast = integer()  )
set_wn      <- fmt_set_field_factory("wn"     , cast = double()   )
set_pct     <- fmt_set_field_factory("pct"    , cast = double()   )
set_diff    <- fmt_set_field_factory("diff"   , cast = double()   )
set_digits  <- fmt_set_field_factory("digits" , cast = integer()  )
set_ctr     <- fmt_set_field_factory("ctr"    , cast = double()   )
set_mean    <- fmt_set_field_factory("mean"   , cast = double()   )
set_var     <- fmt_set_field_factory("var"    , cast = double()   )
set_ci      <- fmt_set_field_factory("ci"     , cast = double()   )

as_totrow  <- function(fmt, in_totrow = TRUE) {
  vec_assert(in_totrow, logical())
  `field<-`(fmt, "in_totrow", vec_recycle(in_totrow, length(fmt)))
}
as_refrow  <- function(fmt, in_refrow = TRUE) {
  vec_assert(in_refrow, logical())
  `field<-`(fmt, "in_refrow", vec_recycle(in_refrow, length(fmt)))
}
as_tottab  <- function(fmt, in_tottab = TRUE) {
  vec_assert(in_tottab, logical())
  `field<-`(fmt, "in_tottab", vec_recycle(in_tottab, length(fmt)))
}

set_col_var   <- function(fmt, col_var) {
  vec_assert(col_var, character(), size = 1)
  `attr<-`(fmt ,"col_var" , col_var)
}
as_totcol     <- function(fmt, totcol = TRUE) {
  vec_assert(totcol, logical(), size = 1)
  `attr<-`(fmt ,"totcol"  , totcol)
}
as_refcol     <- function(fmt, refcol = TRUE) {
  vec_assert(refcol, logical(), size = 1)
  `attr<-`(fmt ,"refcol"  , refcol)
}
set_type      <- function(fmt, type) {
  if (type %in% c("no", "", NA_character_)) type <- "n"
  stopifnot(type %in% c("row", "col", "all", "all_tabs", "mean", "n"))
  `attr<-`(fmt ,"type"    , type)
}
set_diff_type   <- function(fmt, diff_type) {
  stopifnot(diff_type %in% c("tot", "first", "no", "", NA_character_))
  `attr<-`(fmt ,"diff_type" , diff_type)
}
set_ci_type   <- function(fmt, ci_type) {
  stopifnot(ci_type %in% c("cell", "diff", "diff_row", "diff_col",
                           "no", "", NA_character_))
  `attr<-`(fmt ,"ci_type" , ci_type)
}
set_color     <- function(fmt, color) {
  if (color %in% c("no", "")) color <- NA_character_
  stopifnot(color %in% c("diff", "diff_ci", "after_ci", "contrib", "ci",
                         "", NA_character_))
  `attr<-`(fmt ,"color"   , color)
}
set_comp      <- function(fmt, value = c("tab", "all")) {
  `attr<-`(fmt, "comp_all", value == "all")
}

get_num <- function(x) {
  out     <- get_n(x)
  display <- get_display(x)
  nas     <- is.na(display)
  out[!nas & display == "wn"     ] <- get_wn  (x)[!nas & display == "wn"     ]
  out[!nas & display == "pct"    ] <- get_pct (x)[!nas & display == "pct"    ]
  out[!nas & display == "diff"   ] <- get_diff(x)[!nas & display == "diff"   ]
  out[!nas & display == "pct_ci" ] <- get_pct (x)[!nas & display == "pct_ci" ]
  out[!nas & display == "ctr"    ] <- get_ctr (x)[!nas & display == "ctr"    ]
  out[!nas & display == "mean"   ] <- get_mean(x)[!nas & display == "mean"   ]
  out[!nas & display == "mean_ci"] <- get_mean(x)[!nas & display == "mean_ci"]
  out[!nas & display == "var"    ] <- get_var (x)[!nas & display == "var"    ]
  out[!nas & display == "ci"     ] <- get_ci  (x)[!nas & display == "ci"     ]
  out
}

set_num <- function(x, value) {
  out     <- set_n(x, value)
  display <- get_display(x)
  nas     <- is.na(display)
  out[!nas & display == "wn"  ] <- set_wn  (x, value)[!nas & display == "wn"  ]
  out[!nas & display == "pct" ] <- set_pct (x, value)[!nas & display == "pct" ]
  out[!nas & display == "diff"] <- set_pct (x, value)[!nas & display == "diff"]
  out[!nas & display == "ctr" ] <- set_ctr (x, value)[!nas & display == "ctr" ]
  out[!nas & display == "mean"] <- set_mean(x, value)[!nas & display == "mean"]
  out[!nas & display == "var" ] <- set_var (x, value)[!nas & display == "var" ]
  out[!nas & display == "ci"  ] <- set_ci  (x, value)[!nas & display == "ci"  ]
  out
}


# Format/printing methods for class fmnt ---------------------------------------
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

  display <- get_display(x)
  nas  <- is.na(display)
  digits <- get_digits(x)

  ok <- !na_out & !nas

  type       <- get_type(x)
  ci_type <- get_ci_type(x)


  pct_or_ci     <- ok & display %in% c("pct", "pct_ci", "diff", "ci", "ctr") &
    !(display == "ci" & type == "mean")
  pct_or_pct_ci <- ok & display %in% c("pct", "pct_ci", "diff", "ctr")
  pct_ci_only   <- ok & display == "pct_ci"
  n_wn          <- ok & (display %in% c("n", "wn", "mean", "mean_ci", "var") |
                           (display == "ci" & type == "mean") )
  type_ci       <- ok & display == "ci"
  mean_ci_only  <- ok & display == "mean_ci"

  out[pct_or_ci] <- out[pct_or_ci] * 100
  out[!na_out] <- sprintf(paste0("%-0.", digits[!na_out], "f"), out[!na_out]) %>%
    stringr::str_replace("^0.0+$|^-0.0+$", "0")
  out[n_wn] <- out[n_wn] %>% prettyNum(big.mark = " ", preserve.width = "individual")
  out[pct_or_pct_ci] <- out[pct_or_pct_ci] %>% stringr::str_replace("^100.0+$", "100")
  out[na_out] <- NA
  out[pct_or_pct_ci] <- paste0(out[pct_or_pct_ci],"%") #pillar::style_subtle(

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

  out[type_ci] <- switch(type,
                         "n"       = ,
                         "mean"    = paste0("±", out[type_ci]),
                         "row"     = ,
                         "col"     = ,
                         "all"     = ,
                         "all_tabs"= paste0("±", out[type_ci], "%") )

  #out <- stringr::str_pad(out, max(stringr::str_length(out), na.rm = TRUE))
  out
}






#' Pillar_shaft method to print class fmt in a tibble::tibble column
#'
#' @param x A fmt object.
#' @param ... Other parameter.
#'
#'
#' @return A fmt printed in a pillar.
#' @importFrom pillar pillar_shaft
#' @export
pillar_shaft.fmt <- function(x, ...) {
  # print color type somewhere (and brk legend beneath ?) ----

  out     <- format(x)

  display <- get_display(x)
  nas  <- is.na(display)

  type <- get_type(x)
  comp <- get_comp_all(x)

  ci_type   <- get_ci_type(x)
  pct       <- get_type(x)
  color     <- get_color(x)

  totcol    <- is_totcol(x)
  totrows   <- is_totrow(x)
  tottabs   <- is_tottab(x)

  disp_diff <- display == "diff"
  disp_ci   <- display == "ci" & ci_type == "diff"
  disp_ctr  <- display == "ctr"

  if (any(disp_diff)) {
    ref     <- get_reference(x[disp_diff], mode = "cells")
    reffmt  <- set_display(x[disp_diff],
                           ifelse(type %in% c("n", "mean"), "mean", "pct")) %>%
      format() #%>% stringr::str_trim()
    out[disp_diff] <- dplyr::if_else(ref,
                                     paste0("ref:", reffmt),
                                     out[disp_diff])
  }

  if (any(disp_ci)) {
    ref     <- get_reference(x[disp_ci], mode = "cells")
    reffmt  <- set_display(x[disp_ci],
                           ifelse(type %in% c("n", "mean"), "mean", "pct")) %>%
      format()
    out[disp_ci] <- dplyr::if_else(ref,
                                   paste0("ref:x-", reffmt),
                                   out[disp_ci])
  }

  if (any(disp_ctr)) {
    mctr <- if (comp) {
      disp_ctr & totrows & tottabs & !totcol
    } else {
      disp_ctr & totrows & !totcol
    }
    out[mctr] <- paste0("mean:", stringr::str_trim(out[mctr])) %>%
      stringr::str_remove("mean:Inf%|NA")
  }

  if (color == "contrib" & !any(totrows)) warning(
    "cannot print color == 'contrib' with no total rows to store ",
    "information about mean contributions to variance"
  )  # store mean_contrib in a field of fmt ? ----

  na_out  <- is.na(out)
  ok      <- !na_out & !nas


  if (!is.na(color) & color != "" & !(color == "contrib" & !any(totrows))) {
    color_selection <- fmt_color_selection(x)

    color_styles <- get_color_styles(length(color_selection))


    color_styles <- color_styles_all[color_styles]

    unselected <- purrr::transpose(color_selection) %>%
      purrr::map_lgl(~ ! any(purrr::flatten_lgl(.)))

    out[ok] <-
      purrr::reduce2(.init = out[ok], .x = purrr::map(color_selection, ~ .[ok]),
                     .y = color_styles,
                     ~ dplyr::if_else(..2, rlang::exec(..3, ..1), ..1) )
    totals <- get_reference(x, mode = "all_totals") #c("cells", "lines")

    out[ok & unselected & !totals] <-  #fmtgrey3
      pillar::style_subtle(out[ok & unselected & !totals])

    #Columns with no color
  } else {
    # - problem with bold in console : it offsets all column unaesthetically

    # - use underline and | to make the imitate the borders of a table
    # if (any(totrows)) out <- dplyr::if_else(totrows & ! totcol,
    #                                         crayon::underline(out), out)
    # if (totcol)       out <- dplyr::if_else(totrows,
    #                                         paste0(crayon::underline(out), "|"),
    #                                         paste0(out, "|"))

    # - normal cells a bit grayer to see the totals better
    totals <- get_reference(x, mode = "all_totals")
    out[ok & !totals] <- fmtgrey4(out[ok & !totals])

    out[ok] <- out[ok] %>%
      stringr::str_replace("^0%$|^-0%$", pillar::style_subtle("0%")) %>% # 0 in gray
      stringr::str_replace("^0$|^0$", pillar::style_subtle("0"))
  }

  pillar::new_pillar_shaft_simple(out, align = "right", na = "")
}




fmt_color_selection <- function(x, force_color, force_breaks) {
  type    <- get_type (x)
  type_ci <- get_ci_type(x)
  color   <- if (missing(force_color)) get_color(x) else force_color

  if (!missing(force_breaks)) { #if missing, take them in env of package::tablr
    mean_brk       <- force_breaks$mean_brk
    pct_brk        <- force_breaks$pct_brk
    mean_ci_brk    <- force_breaks$mean_ci_brk
    pct_ci_brk     <- force_breaks$pct_ci_brk
    contrib_brk    <- force_breaks$contrib_brk
    mean_brksup    <- force_breaks$mean_brksup
    pct_brksup     <- force_breaks$pct_brksup
    mean_ci_brksup <- force_breaks$mean_ci_brksup
    pct_ci_brksup  <- force_breaks$pct_ci_brksup
    contrib_brksup <- force_breaks$contrib_brksup
  }

  diff <- if (color %in% c("diff", "diff_ci", "after_ci", "ci") ) {
    get_diff(x)
  } else {
    NA_real_ #vec_recycle(NA_real_, length(x))
  }

  ci <- if (color %in% c("diff_ci", "after_ci", "ci") & type_ci == "diff" ) {
    get_ci(x)
  } else {
    NA_real_
  }

  ref_means <- if (color %in% c("diff_ci", "after_ci", "ci") & type == "mean") {
    get_ref_means(x)
  } else {
    NA_real_
  }

  ctr <- if (color == "contrib") {
    get_ctr(x)
  } else {
    NA_real_
  }

  mean_ctr <- if (color == "contrib") {
    get_mean_contrib(x)
  } else {
    NA_real_
  }

  brk <-
    switch(color,
           "diff"     = ,
           "diff_ci"  = if (type == "mean") mean_brk    else pct_brk   ,
           "ci"       = if (type == "mean"){
             mean_ci_brk[c(1, length(mean_ci_brk)/2 + 1)]
           } else {
             pct_ci_brk[c(1, length(pct_ci_brk)/2 + 1)]
           } ,
           "after_ci" = if (type == "mean") mean_ci_brk else pct_ci_brk,
           "contrib"  = contrib_brk                                     )

  brksup <-
    switch(color,
           "diff"     = ,
           "diff_ci"  = if (type == "mean") mean_brksup    else pct_brksup   ,
           "ci"       = if (type == "mean") {
             mean_ci_brksup[c(length(mean_ci_brksup)/2, length(mean_ci_brksup))]
           }  else {
             pct_ci_brksup[c(length(pct_ci_brksup)/2, length(pct_ci_brksup))]
           },
           "after_ci" = if (type == "mean") mean_ci_brksup else pct_ci_brksup,
           "contrib"  = contrib_brksup                                        )

  purrr::map2(brk, brksup,
              ~ color_formula(type = type, color = color,
                              diff = diff, ci = ci, ref_means = ref_means,
                              ctr = ctr, mean_ctr = mean_ctr,
                              brk = .x, brksup = .y)
  ) %>% purrr::set_names(as.character(round(brk, 2)))
}


# diff >= 1                                              &
#   (1 + abs(1 - diff) ) * ref_means  >  (ref_means + ci) * brk[1]   &
#   abs(1 - diff) * ref_means  <  (ref_means + ci) * brksup[1]
#
#
# ref_means + abs(1 - diff) * ref_means  > (ref_means + ci) * brk[1]
# ref_means + abs(1 - diff) * ref_means  > ref_means * brk[1] + ci * brk[1]
# abs(1 - diff) * ref_means > ref_means * brk[1] - ref_means + ci * brk[1]
# abs(1 - diff) * ref_means > ref_means * (brk[1] - 1) + ci * brk[1]
#
# (ref + ci) * brk -ref
#
# abs(1 - diff) > brk[1] - 1 + ci/ref_means * brk[1]
# abs(1 - diff) + 1 > brk[1] * (ci/ref_means + 1)
#
# (1 + abs(1 - diff)) * ref_means   > ref_means * brk[1] + ci * brk[1]
#





color_formula <- function(type, color, diff, ci, ref_means,
                          ctr, mean_ctr, brk, brksup) {
  means <- type %in% c("mean", "n")

  res <-
    switch(
      color,
      "diff"     =
        if( (!means & brk >= 0) | (means & brk >= 1) ) {
          diff > brk & diff < brksup} else {
            diff < brk & diff > brksup},

      "diff_ci"  = dplyr::case_when(
        means & brk >= 1    ~ diff > brk & diff < brksup &
          abs(1 - diff) * ref_means - ci > 0,

        means & brk <  1    ~ diff < brk & diff > brksup &
          abs(1 - diff) * ref_means - ci > 0,

        !means & brk >= 0   ~ diff > brk & diff < brksup & abs(diff) - ci > 0,
        !means & brk <  0   ~ diff < brk & diff > brksup & abs(diff) - ci > 0 ),

      "ci"       = dplyr::case_when(
        means & (brk == 1)
        ~ diff >= 1  &  abs(1 - diff) * ref_means > ci,

        means & (brk == -1)
        ~ diff  < 1  &  abs(1 - diff) * ref_means > ci,

        !means & brk == 0 & brksup > 0
        ~ diff >= 0  &  abs(diff) > ci,

        !means & brk == 0 & brksup < 0
        ~ diff  < 0  &  abs(diff) > ci
      ),

      "after_ci" = dplyr::case_when(
        means & brk > 0
        ~ diff >= 1                                                         &
          ref_means + abs(1 - diff) * ref_means  > (ref_means + ci) * brk   &
          ref_means + abs(1 - diff) * ref_means  < (ref_means + ci) * brksup ,
        #wrong : abs(1 - diff) > ci * brk
        #wrong : abs(1 - diff) * ref_means  >  (ref_means + ci) * brk

        means & brk < 0
        ~ diff < 1                                                           &
          ref_means + abs(1 - diff) * ref_means  > (ref_means + ci) * -brk   &
          ref_means + abs(1 - diff) * ref_means  < (ref_means + ci) * -brksup ,

        !means & brk >= 0 & brksup > 0
        ~ diff >= 0  &  abs(diff) - ci > brk   &  abs(diff) - ci < brksup,

        !means & brk <  0 & brksup < 0
        ~ diff  < 0  &  abs(diff) - ci > -brk  &  abs(diff) - ci < -brksup),

      "contrib"     = if (brk >= 0) {
        ctr > brk * mean_ctr & ctr < brksup * mean_ctr
      } else {
        ctr < brk * mean_ctr & ctr > brksup * mean_ctr
      },
      rep(FALSE, length(diff))
    )

  tidyr::replace_na(res, FALSE)
}



tab_color_legend <- function(x, colored = TRUE) {
  color     <- get_color(x)
  type      <- get_type(x)
  diff_type <- get_diff_type(x)
  col_vars_levels <- tab_get_vars(x)$col_vars_levels %>%
    purrr::discard(names(.) == "all_col_vars")

  color_type <- col_vars_levels %>%
    purrr::map(~ get_color_type(color[.], type[.]) %>%
                 purrr::flatten_chr()) %>%
    purrr::map_chr(~ dplyr::if_else(
      condition = length(unique(.x)) == 1,
      true      = .x[1],
      false     = "mixed"                 )
    )

  diff_type <- col_vars_levels %>%
    purrr::map(~ diff_type[.]) %>%
    purrr::map_chr(~ dplyr::if_else(
      condition = length(unique(.x)) == 1,
      true      = .x[1],
      false     = "mixed"                 )
    )

  if (all(is.na(color_type) | color_type %in% c("", "no"))) return(NULL)

  breaks_with_op <- function(breaks, color_type) purrr::map_chr(
    breaks,
    ~ switch(
      color_type,
      "diff_mean"     = ,
      "diff_ci_mean"  = dplyr::if_else(
        condition = stringr::str_detect(.x, "^-"),
        true      = paste0("/", stringr::str_remove(.x, "^-")),
        false     = paste0("×", .x)
      ),
      "diff"          = ,
      "diff_ci"       = ,
      "after_ci"      = dplyr::if_else(
        condition = stringr::str_detect(.x, "^-"),
        true      = .x,
        false     = paste0("+", .x)
      ),
      "after_ci_mean" = paste0("×", stringr::str_remove(.x, "^-")),
      "contrib"       = paste0("×", stringr::str_remove(.x, "^-")),
      #"ci_mean"       = ,
      "ci"            = "",      #just 1 ?
      .x
    ) )

  color_formula_chr <- function(color_type, ref, sign, breaks) {
    purrr::map2_chr(breaks, sign, function(.breaks, .sign)
      switch(
        color_type,
        "diff_mean"     = , # 1/mean and sign /
        "diff"          = paste0("x", .sign, ref, " ", .breaks),
        "diff_ci_mean"  = ,
        "diff_ci"       = paste0("|x-", ref, "|>ci & x", .sign,
                                 ref, " ", .breaks),
        #"ci_mean"       = ,
        "ci"            = paste0("|x-", ref, "| > ci"),      #just 1 ?
        "after_ci_mean" = paste0(ref, " + |x-", ref, "| > (", ref, " + ci) ", .breaks), #×
        "after_ci"      = paste0("|x-", ref, "| > ci ", .breaks), #+ -
        "contrib"       = paste0("contrib > mean_ctr "     , .breaks),  #×
        character()
      ))
  }


  color_table <-
    tibble::tibble(color_type, diff_type, names = names(color_type)) %>%
    dplyr::filter(!is.na(color_type) & !color_type %in% c("no", "")) %>%
    dplyr::group_by(color_type) %>%
    dplyr::mutate(etc = dplyr::if_else(dplyr::row_number() > 3, ",...", "") ) %>%
    dplyr::slice(1:3) %>%
    dplyr::summarise(names = paste0(names, collapse = ", "),
                     etc = dplyr::first(etc),
                     diff_type = dplyr::first(diff_type),
                     .groups = "drop") %>%
    dplyr::mutate(
      names = paste0(names, etc)
    )

  if (colored == TRUE) color_table <- color_table %>%
    dplyr::mutate(names = stringr::str_pad(names,
                                           max(stringr::str_length(names)),
                                           side = "right"))

  color_table <- color_table %>%
    dplyr::mutate(
      breaks = brk_from_color(color_type),
      breaks = dplyr::if_else(color_type %in% c("diff_mean", "diff_ci_mean"),
                              true  = purrr::map(breaks,
                                                 ~ .[1:(length(.)/2)] %>%
                                                   c(., purrr::map(., `-`))
                              ),
                              false = breaks),
      breaks = dplyr::if_else(
        condition = color_type %in% c("diff", "diff_ci", "after_ci"),
        true      = purrr::map(breaks,
                               ~ paste0(sprintf("%1.0f",
                                                purrr::map_dbl(., ~ .[1]) * 100),
                                        "%") ),
        false     = purrr::map(breaks,
                               ~ sprintf("%1.3g", purrr::map_dbl(., ~ .[1]))),
      ) %>%
        purrr::map2(color_type, ~ breaks_with_op(.x, .y)),
      ref  = purrr::map_chr(diff_type, ~ switch(., "first" = "x1", "tot")),
      sign = purrr::map(breaks, ~ 1:length(.)) %>%
        purrr::map(~ dplyr::if_else(condition = . >= max(.)/2 +1,
                                    true      = " < ",
                                    false     = " > ")),
    )

  if (colored == TRUE) color_table <- color_table %>%
    dplyr::mutate(
      styles = purrr::map(breaks, ~ get_color_styles(length(.))),
      styles = purrr::map(styles, ~ color_styles_all[.]),
      breaks = purrr::map2(styles, breaks,
                           ~ purrr::map2_chr(.x, .y, rlang::exec) )
    )

  color_table <- color_table %>%
    dplyr::mutate(
      color_scale = list(color_type, ref, sign, breaks,
                         purrr::map(breaks, ~ 1:length(.))) %>%
        purrr::pmap(~ dplyr::if_else(
          condition = ..5 %in% c(1, max(..5)/2 + 1),
          true      = color_formula_chr(color_type = ..1, ref = ..2,
                                        sign = ..3, breaks = ..4),
          false     = ..4
        ))
    )

  color_table <-
    if (colored == TRUE) {
      color_table %>% dplyr::mutate(
        color_scale = purrr::map_chr(color_scale, ~ paste0(
          .,
          collapse = pillar::style_subtle("; ")          )
        ),
        names = pillar::style_subtle(paste0(names, ": "))
      )
    } else {
      color_table %>% dplyr::mutate(
        color_scale = purrr::map_chr(color_scale, ~ paste0(., collapse = "; " )),
        names = paste0(names, ": ")
      )
    }

  paste0(color_table$names, color_table$color_scale)
  # %>% cli::cat_line()
  # stringr::str_remove("\\+0.0+")
}
# tab_color_legend(tabs[[7]]) %>% cli::cat_line()
# tab_color_legend(tabs[[7]], colored = FALSE)


brk_from_color <- function(color_type) {
  purrr::map(color_type, ~
               switch(.x,
                      "diff_mean"     = ,
                      "diff_ci_mean"  = list(mean_brk, mean_brksup),
                      "after_ci_mean" = list(mean_ci_brk, mean_ci_brksup),
                      "diff"          = ,
                      "diff_ci"       = list(pct_brk, pct_brksup),
                      "after_ci"      = list(pct_ci_brk, pct_ci_brksup),
                      "contrib"       = list(contrib_brk, contrib_brksup),
                      "ci"            = ,
                      "ci_mean"       = list(0, Inf), #list(c(0, 0), c(Inf, -Inf)),
                      list()
               ) %>%
               purrr::transpose() %>%
               purrr::map(purrr::flatten_dbl)
  )
}

#' @keywords internal
get_color_type <- function(color, type) {
  purrr::map2(color, type, ~ dplyr::case_when(
    .x == "contrib" ~ "contrib",
    .x == "ci"      ~ "ci"     ,

    .x %in% c("diff", "diff_ci", "after_ci") & .y == "mean"
    ~ paste0(.x, "_mean"),

    .x %in% c("diff", "diff_ci", "after_ci") &
      .y %in% c("row", "col", "all", "all_tabs")
    ~ .x

  ) %>% purrr::set_names(names(.x)))
}

get_color_styles <- function(length) {
  if (stringr::str_detect(color_styles_all[[1]], "^bg")) {
    switch(as.character(length),
           "1"  = c(3)              ,
           "2"  = c(3, 8)           ,
           "4"  = c(1, 3, 6, 8)     ,
           "6"  = c(1, 3, 5, 6, 8, 10),
           "8"  = c(1:3, 4, 6:8, 10),
           "10" = 1:10               )
  } else {
    switch(as.character(length),
           "1"  = c(3)              ,
           "2"  = c(3, 8)           ,
           "4"  = c(3, 5, 8, 10)    ,
           "6"  = c(3:5, 8:10)      ,
           "8"  = c(2:5, 7:10)      ,
           "10" = 1:10               )
  }
}


# brk_from_color <- function(color, type) {
#   means <- type == "mean"
#   purrr::map2(color, means,
#               ~ switch(
#                 .x,
#                 "diff"          = ,
#                 "diff_ci"       = if (.y) {
#                   list(mean_brk, mean_brksup)
#                 } else {
#                   list(pct_brk, pct_brksup)
#                 },
#                 "after_ci"      = if (.y) {
#                   list(mean_ci_brk, mean_ci_brksup)
#                 } else {
#                   list(pct_ci_brk, pct_ci_brksup)
#                 },
#                 "contrib"       = list(contrib_brk, contrib_brksup),
#                 list()
#               ) %>%
#                 purrr::transpose() %>%
#                 purrr::map(purrr::flatten_chr)
#   )
# }

get_reference <- function(x, mode = c("cells", "lines", "all_totals")) {
  type        <- get_type(x)
  diff_type   <- get_diff_type(x)
  comp_all    <- get_comp_all(x)
  totcol      <- is_totcol(x)
  totrows     <- is_totrow(x)
  tottab_line <- is_tottab(x) & totrows

  refrows     <- is_refrow(x)
  refcol      <- is_refcol(x)
  tottab_ref  <- is_tottab(x) & refrows

  if (diff_type == "first") {
    switch(mode[1],
           "cells"      = dplyr::case_when(
             type %in% c("row", "mean") & !comp_all ~ refrows & !totcol     ,
             type %in% c("row", "mean") &  comp_all ~ tottab_ref & !totcol  ,
             type == "col"                          ~ refcol & !totrows     ,
             TRUE                                   ~ rep(FALSE, length(x)   )
           ),
           "lines"      = dplyr::case_when(
             type %in% c("row", "mean") & !comp_all ~ refrows               ,
             type %in% c("row", "mean") &  comp_all ~ tottab_ref            ,
             type == "col"                          ~ rep(refcol, length(x)),
             TRUE                                   ~ rep(FALSE, length(x)   )
           ),
           "all_totals" = dplyr::case_when(
             type %in% c("row", "mean") & !comp_all ~ refrows | totcol      ,
             type %in% c("row", "mean") &  comp_all ~ tottab_ref | totcol   ,
             type == "col"                          ~ totrows | refcol      ,
             TRUE                                   ~ rep(FALSE, length(x)   )
           )
    )

  } else {
    switch(mode[1],
           "cells"      = dplyr::case_when(
             type %in% c("row", "mean") & !comp_all ~ totrows & !totcol     ,
             type %in% c("row", "mean") &  comp_all ~ tottab_line & !totcol ,
             type == "col"                          ~ totcol & !totrows     ,
             type == "all"                          ~ totrows & totcol      ,
             type == "all_tabs"                     ~ tottab_line & totcol  ,
             TRUE                                   ~ rep(FALSE, length(x)   )
           ),
           "lines"      = dplyr::case_when(
             type %in% c("row", "mean") & !comp_all ~ totrows               ,
             type %in% c("row", "mean") &  comp_all ~ tottab_line           ,
             type == "col"                          ~ rep(totcol, length(x)),
             type == "all"                          ~ totrows & totcol      ,
             type == "all_tabs"                     ~ tottab_line & totcol  ,
             TRUE                                   ~ rep(FALSE, length(x)   )
           ),
           "all_totals" = dplyr::case_when(
             type %in% c("n", "col", "all") |
               (type %in% c("row", "mean") & !comp_all)
             ~ totrows | totcol,

             type == "all_tabs" | (type %in% c("row", "mean") &  comp_all)
             ~ tottab_line | totcol,
             # type == "col"                          ~ rep(totcol, length(x)),
             # type == "all"                          ~ totrows & totcol      ,
             # type == "all_tabs"                     ~ tottab_line & totcol  ,
             TRUE                                   ~ rep(FALSE, length(x)   )
           )
    )
  }
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


#Define abbreviated display name (for tibble::tibble headers)
#' @export
vec_ptype_abbr.fmt <- function(x, ...) {
  display  <- get_display(x) %>% unique()
  display  <- ifelse(length(display) > 1, "mixed", display)
  type     <- get_type(x)
  row_mean <- type %in% c("row", "mean")
  if (type %in% c("row", "col", "all", "all_tabs")) type <- paste0(type, "%")
  ci <- get_ci_type(x)
  if (display == "ci" & ci %in% c("cell", "diff")) display <- paste0("ci_", ci)

  out <- paste0(type, "-", display) %>%
    stringr::str_replace("^n-n", "n") %>%
    stringr::str_replace("^mean-mean", "mean") %>%
    stringr::str_replace("^mixed-mixed", "mixed") %>%
    stringr::str_replace("([^%]+%)-pct", "\\1") %>%
    stringr::str_remove("^NA")
  if (get_comp_all(x)) out <- paste0(out, "-all")

  out
}


# Include numbers of digits and types in the printed name
#' @export
vec_ptype_full.fmt <- function(x, ...) {
  display  <- get_display(x) %>% unique()
  display  <- ifelse(length(display) > 1, "mixed", display)
  type     <- get_type(x)
  row_mean <- type %in% c("row", "mean")
  if (type %in% c("row", "col", "all", "all_tabs")) type <- paste0(type, "%")
  ci <- get_ci_type(x)
  if (display == "ci" & ci %in% c("cell", "diff")) display <- paste0("ci_", ci)

  out <- paste0("fmt-", type, "-", display) %>%
    stringr::str_replace("-n-n", "-n") %>%
    stringr::str_replace("-mean-mean", "-mean") %>%
    stringr::str_replace("-mixed-mixed", "-mixed") %>%
    stringr::str_replace("([^%]+%)-pct", "\\1") %>%
    stringr::str_remove("-NA")
  if (get_comp_all(x)) out <- paste0(out, "-all")

  out
}



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
fmtgrey2 <- crayon::make_style(grey(0.5), grey = TRUE)
fmtgrey1 <- crayon::make_style(grey(0.3), grey = TRUE)

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

#Color breaks for printing fmt ------------------------------------------------

#calculate pct breaks based on the number of levels ? ----

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
mean_ci_brk     <- mean_ci_breaks %>% c(., -.) #then - again

pct_brksup      <- pct_brksup     %>% c(., -.)
mean_brksup     <- mean_brksup    %>% c(., 1/.)
contrib_brksup  <- contrib_brksup %>% c(., -.)
pct_ci_brksup   <- pct_ci_brksup  %>% c(., -.) #not same as tab_xl
mean_ci_brksup  <- mean_ci_brksup %>% c(., -.) #then - again

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
    pct_ci_brk      <- pct_ci_breaks  %>% c(., -.)
    pct_ci_brksup   <- c(pct_ci_breaks[2:length(pct_ci_breaks) ], Inf)
    pct_ci_brksup   <- pct_ci_brksup  %>% c(., -.)
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
    mean_ci_brk     <- mean_ci_breaks %>% c(., -.) #then - again
    mean_ci_brksup  <- c(mean_ci_breaks[2:length(mean_ci_breaks)], Inf)
    mean_ci_brksup  <- mean_ci_brksup %>% c(., -.) #then - again
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


#Coertion and convertion methods for formatted numbers -------------------------

#Make our fmt class coercible with herself, and back and forth with double and integer vectors :
#' @export
vec_ptype2.fmt.fmt    <- function(x, y, ...) {
  type_x       <- get_type(x)
  same_type    <- type_x == get_type(y)
  comp_x       <- get_comp_all(x, replace_na = FALSE)
  same_comp    <- comp_x == get_comp_all(y, replace_na = FALSE)
  diff_type_x  <- get_diff_type(x)
  same_diff_type <- diff_type_x == get_diff_type(y)
  ci_type_x    <- get_ci_type(x)
  same_ci_type <- ci_type_x == get_ci_type(y)
  col_var_x    <- get_col_var(x)
  same_col_var <- col_var_x == get_col_var(y)
  totcol_x     <- is_totcol(x)
  same_totcol  <- totcol_x == is_totcol(y)
  refcol_x     <- is_refcol(x)
  same_refcol  <- refcol_x == is_refcol(y)
  color_x      <- get_color(x)
  same_color   <- color_x == get_color(y)
  #l            <- length(x)

  new_fmt(
    type     = dplyr::if_else(same_type   , type_x   , "mixed"       ),
    comp_all = dplyr::if_else(same_comp   , comp_x   , FALSE         ),
    diff_type= dplyr::if_else(same_diff_type, diff_type_x, ""        ),
    ci_type  = dplyr::if_else(same_ci_type, ci_type_x, ""            ),
    col_var  = dplyr::if_else(same_col_var, col_var_x, "several_vars"),
    totcol   = dplyr::if_else(same_totcol , totcol_x , FALSE         ),
    refcol   = dplyr::if_else(same_refcol , refcol_x , FALSE         ),
    color    = dplyr::if_else(same_color  , color_x  , ""            )
  )
  # new_fmt(
  #   type     = dplyr::if_else(same_type, true  = type_x,
  #                             false = "mixed"),
  #   comp_all = dplyr::if_else(same_comp,
  #                             true  = comp_x,
  #                             false = vec_recycle(FALSE, l )),
  #   ci_type  = dplyr::if_else(same_ci_type,
  #                             true  = ci_type_x,
  #                             false = vec_recycle(NA_character_, l )),
  #   col_var  = dplyr::if_else(same_col_var,
  #                             true  = col_var_x,
  #                             false = vec_recycle("several_vars", l )),
  #   totcol   = dplyr::if_else(same_totcol,
  #                             true  = totcol_x,
  #                             false = vec_recycle(FALSE, l )),
  #   color    = dplyr::if_else(same_color,
  #                             true  = color_x,
  #                             false = vec_recycle(NA_character_, l))
  # )
}
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
vec_cast.fmt.fmt  <- function(x, to, ...)
  new_fmt(display   = get_display (x),
          n         = get_n       (x),
          wn        = get_wn      (x),
          pct       = get_pct     (x),
          diff      = get_diff    (x),
          digits    = get_digits  (x),
          ctr       = get_ctr     (x),
          mean      = get_mean    (x),
          var       = get_var     (x),
          ci        = get_ci      (x),

          in_totrow = is_totrow   (x),
          in_refrow = is_refrow   (x),
          in_tottab = is_tottab   (x),

          type      = get_type    (to),
          comp_all  = get_comp_all(to, replace_na = FALSE),
          diff_type = get_diff_type(to),
          ci_type   = get_ci_type (to),
          col_var   = get_col_var (to),
          totcol    = is_totcol   (to),
          refcol    = is_refcol   (to),
          color     = get_color   (to)

  )

#' @export
vec_cast.fmt.double   <- function(x, to, ...)
  fmt(n = NA_integer_            ,
      display = "wn", wn = x     ,
      type     = get_type    (to),
      comp_all = get_comp_all(to, replace_na = FALSE),
      diff_type = get_diff_type(to),
      ci_type  = get_ci_type (to),
      col_var  = get_col_var (to),
      totcol   = is_totcol   (to),
      refcol    = is_refcol   (to),
      color    = get_color   (to),

  )
#' @method vec_cast.double fmt
#' @export
vec_cast.double.fmt  <- function(x, to, ...) get_num(x) %>% as.double() #field(x, "pct")

#' @export
vec_cast.fmt.integer <- function(x, to, ...)
  fmt(n        = x               ,
      type     = get_type    (to),
      comp_all = get_comp_all(to, replace_na = FALSE),
      diff_type = get_diff_type(to),
      ci_type  = get_ci_type (to),
      col_var  = get_col_var (to),
      totcol   = is_totcol   (to),
      refcol    = is_refcol   (to),
      color    = get_color   (to)

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
  type_x       <- get_type(x)
  same_type    <- type_x == get_type(y)
  comp_x       <- get_comp_all(x, replace_na = FALSE)
  same_comp    <- comp_x == get_comp_all(y, replace_na = FALSE)
  diff_type_x  <- get_diff_type(x)
  same_diff_type <- diff_type_x == get_diff_type(y)
  ci_type_x    <- get_ci_type(x)
  same_ci_type <- ci_type_x == get_ci_type(y)
  col_var_x    <- get_col_var(x)
  same_col_var <- col_var_x == get_col_var(y)
  l            <- length(x)
  rep_NA_real  <- rep(NA_real_, l)

  if (!same_type) warning("operation ", op,
                          " over columns with different pct types, ",
                          "or mixing pct and means (",
                          type_x, "/", get_type(y), ")")
  if (!same_comp) warning("operation ", op,
                          " may mix calculations made on tabs and calculations ",
                          "made on all tabs (different 'comp_all')")
  if (!same_col_var) warning("operation ", op,
                             " over columns belonging to different variables(",
                             col_var_x , "/", get_col_var(y), ")")

  switch(
    op,
    "+" = ,
    "-" = new_fmt(
      display = get_display(x),      #dplyr::if_else(get_display(x) == get_display(x)), true = get_display(x), false = "n),
      n       = vec_arith_base(op, get_n(x)  , get_n(y)  ), #%>% positive_integer(),
      wn      = vec_arith_base(op, get_wn(x) , get_wn(y) ), #%>% positive_double(),
      pct     = ifelse(same_type & ! type_x %in% c("col", "mean", "n"),
                       yes  = vec_arith_base(op, get_pct(x), get_pct(y)),
                       no = NA_real_) %>% tidyr::replace_na(NA_real_), #NA_real_
      diff    = rep_NA_real,
      digits  = pmax(get_digits(x), get_digits(y)),
      ctr     = rep_NA_real, # ???
      mean    = vec_arith_base(op, get_mean(x) * get_wn(x), get_mean(y) * get_wn(y)) /
        vec_arith_base("+", get_wn(x) , get_wn(y) ),# weighted mean
      var     = rep_NA_real,
      ci      = rep_NA_real,

      in_totrow = is_totrow(x) & is_totrow(y), # Just x ?
      in_refrow = is_refrow(x) & is_refrow(y),
      in_tottab = is_tottab(x) & is_tottab(y),

      type     = dplyr::if_else(same_type   , type_x   , "mixed"       ),
      comp_all = dplyr::if_else(same_comp   , comp_x   , FALSE         ),
      diff_type= dplyr::if_else(same_diff_type, diff_type_x, ""        ),
      ci_type  = dplyr::if_else(same_ci_type, ci_type_x, ""            ),
      col_var  = dplyr::if_else(same_col_var, col_var_x, "several_vars"),
      totcol   = FALSE                                                  ,
      refcol   = FALSE                                                  ,
      color    = get_color(x)

      # type     = dplyr::if_else(same_type,
      #                           true  = type_x,
      #                           false = vec_recycle("mixed", l )),
      # comp_all = dplyr::if_else(same_comp,
      #                           true  = comp_x,
      #                           false = vec_recycle(FALSE, l )),
      # ci_type  = dplyr::if_else(same_ci_type,
      #                           true  = ci_type_x,
      #                           false = vec_recycle(NA_character_, l )),
      # col_var  = dplyr::if_else(same_col_var,
      #                           true  = col_var_x,
      #                           false = vec_recycle("several_vars", l )),
    ),
    "/" = ,
    "*" = new_fmt(
      display   = get_display(x),
      n      = get_n(x)   ,
      wn     = get_wn(x)  ,
      pct    = vec_arith_base(op, get_pct(x), get_pct(y)), #Remove multiplication ?
      diff   = rep_NA_real,
      digits = pmax(get_digits(x), get_digits(y)),
      ctr    = rep_NA_real,
      mean   = rep_NA_real,
      var    = rep_NA_real,
      ci     = rep_NA_real,

      in_totrow = is_totrow(x),
      in_refrow = is_refrow(x),
      in_tottab = is_tottab(x),

      type     = dplyr::if_else(same_type   , type_x   , "mixed"       ),
      comp_all = dplyr::if_else(same_comp   , comp_x   , FALSE         ),
      diff_type= dplyr::if_else(same_diff_type, diff_type_x, ""        ),
      ci_type  = dplyr::if_else(same_ci_type, ci_type_x, ""            ),
      col_var  = dplyr::if_else(same_col_var, col_var_x, "several_vars"),
      totcol   = FALSE                                                  ,
      refcol   = FALSE                                                  ,
      color    = get_color(x)

      # type     = dplyr::if_else(same_type,
      #                           true  = type_x,
      #                           false = vec_recycle("mixed", l )),
      # comp_all = dplyr::if_else(same_comp,
      #                           true  = comp_x,
      #                           false = vec_recycle(FALSE, l )),
      # ci_type  = dplyr::if_else(same_ci_type,
      #                           true  = ci_type_x,
      #                           false = vec_recycle(NA_character_, l )),
      # col_var  = dplyr::if_else(same_col_var,
      #                           true  = col_var_x,
      #                           false = vec_recycle("several_vars", l )),
    ),
    stop_incompatible_op(op, x, y)
  )
}

#' @method vec_arith.fmt numeric
#' @export
vec_arith.fmt.numeric <- function(op, x, y, ...) {
  set_num(x, vec_arith_base(op, get_num(x), y))
  # new_fmt(pct    = vec_arith_base(op, field(x, "pct"), y),
  #          display   = field(x, "display"  ),
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
  #          display   = field(y, "display"  ),
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
         #              display   = field(x, "display"  ),
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
  if (!is.na(get_type(.x) ) & get_type(.x) == "mixed") warning(
    "operation ", .fn,
    " within a variable mixing different types of percentages"
  )

  switch(.fn,
         "sum" = new_fmt(display   = get_display(.x)[1],
                         digits = min(get_digits(.x)),
                         n      = vec_math_base(.fn, get_n(.x)  , ...),
                         wn     = vec_math_base(.fn, get_wn(.x) , ...),
                         pct    = ifelse(! get_type(.x) %in% c("row", "col"),
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
                         in_refrow = all(is_refrow(.x)),
                         in_tottab = all(is_tottab(.x)), #any ?

                         type      = get_type    (.x),
                         comp_all  = get_comp_all(.x, replace_na = FALSE),
                         diff_type = get_diff_type(.x),
                         ci_type   = get_ci_type (.x),
                         col_var   = get_col_var (.x),
                         totcol    = is_totcol   (.x),
                         refcol    = is_refcol   (.x),
                         color     = get_color   (.x)
         ),
         "mean" = new_fmt(display = get_display(.x)[1],
                          digits  = max(get_digits(.x)),
                          n       = vec_math_base("sum", get_n(.x)  , ...),
                          wn      = vec_math_base("sum", get_wn(.x) , ...),
                          pct     = vec_math_base("sum", get_pct(.x) * get_wn(.x), ...) /
                            vec_math_base("sum", get_wn(.x), ...),
                          diff    = NA_real_,
                          ctr     = NA_real_,
                          mean    = vec_math_base("sum", get_mean(.x) * get_wn(.x), ...) /
                            vec_math_base("sum", get_wn(.x), ...),
                          var     = NA_real_,
                          ci      = NA_real_,

                          in_totrow = FALSE,
                          in_refrow = FALSE,
                          in_tottab = all(is_tottab(.x)), #any ?

                          type      = get_type    (.x),
                          comp_all  = get_comp_all(.x, replace_na = FALSE),
                          diff_type = get_diff_type(.x),
                          ci_type   = get_ci_type (.x),
                          col_var   = get_col_var (.x),
                          totcol    = is_totcol   (.x),
                          refcol    = is_refcol   (.x),
                          color     = get_color   (.x)
         ),
         vec_math_base(.fn, get_num(.x), ...) )
}









# c(fmt("n", 0), fmt("n", 0)) %>% is_totrow()
# c(fmt("n", 0), fmt("n", 0)) %>% is_tottab()




# Tests-------------------------------------------------------------------------
# #test of class :
# fmt(1)
# fmt(1) %>% class()
# fmt(1) %>% is_fmt()
# fmt(1) %>% is.double()
# fmt(1) %>% is.numeric()
#
# #test of printing :
# num1 <- fmt(n = c(5, 10, 15), type = "n", display = c("n", "row", "mean"),
#             wn = c(4.7, 12.1, 13.9), digits = 1, pct = c(NA, 0.63, NA),
#             mean = c(NA, NA, 27.3))
# num2 <- fmt(n = c(15, 10, 5), type = "row", display = c("n", "row", "mean"),
#             wn = c(13.9, 12.1, 4.7), digits = 0, pct = c(NA, 0.22, NA),
#             mean = c(NA, NA, 21))
# tibble::tibble(num1)
# fmt(1) %>% vec_data()
#
# #test of common type :
# vec_ptype_show(fmt(1, "row", pct = 0.255), fmt(2, "row", pct = 0.987))
# vec_ptype_show(fmt(), double(), fmt())
# vec_ptype_common(fmt(1, "row", pct = 0.255), fmt(2, "row", pct = 0.987))
# vec_ptype2(fmt(1, "row", pct = 0.255), fmt(2, "row", pct = 0.987))
# vec_ptype2(fmt(1, "row", pct = 0.255), fmt(2, "col", pct = 0.987))
#
# #test of conversion :
# vec_cast(fmt(1, "row", pct = 0.255), fmt(2, "row", pct = 0.987))
# vec_cast(5, fmt())
# vec_cast(5L, fmt())
# vec_cast(fmt(6), double())
# vec_cast(fmt(6), integer())
# vec_cast(fmt(1, "row", pct = 0.6005), character())
# vec_cast(NA, fmt())
#
# #test of combination :
# vec_c(fmt(1, "row", pct = 0.255), fmt(2, "row", pct = 0.987)) #%>% get_digits()
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
# (fmt(5 , "n"   , 0, wn = 5.1)               + fmt(  1, "n"   , 0, pct  = 0.25000001, wn =  1.5 )) %>% vec_data()
# (fmt(15, "row" , 1, pct =  0.55, wn = 15.1) - fmt(  2, "mean", 0, mean = 0.25000001, wn =  2.5 )) %>% vec_data()
# (fmt(15, "row" , 1, pct =  0.55, wn = 15.1) - fmt( 20, "mean", 0, mean = 0.75000001, wn = 20.5 )) %>% vec_data()
# (fmt(25, "row" , 2, pct =  0.55, wn = 25.1) / fmt(  3, "row" , 3, pct  = 0.25000001, wn =  3.5 )) %>% vec_data()
# (fmt(35, "row" , 3, pct =  0.55, wn = 35.1) * fmt(  4, "row" , 0, pct  = 0.25000001, wn =  4.5 )) %>% vec_data()
# (fmt(45, "row" , 4, pct =  0.55, wn = 45.1) + 0.0077778) %>% vec_data()
# (fmt(55, "mean", 3, mean = 0.55, wn = 55.1) - 1)
# (fmt(65, "n"   , 2, pct =  0.55, wn = 65.1) / 2)
# fmt(75, "row" ,-1, pct =  0.55, wn = 75.1) * 3
# fmt(1) + 1
# 1 + fmt(1, "row", pct = 0.12)
# 1 - fmt(1, "row", pct = 0.12)
# 2 / fmt(3, "row", pct = 0.12)
# 5 * fmt(1, "n", 2)
# -fmt(1, "row", pct = 0.12)
# (fmt(1) + 0.0077778) %>% as.double()
# sum(fmt(1), fmt(1)) #%>% vec_data()
# mean(c(fmt(1, "n", 2), fmt(1, "n", 2))) %>% vec_data()
#
#
# x <- fmt(n = c(2, 1), type = "row", pct = c(0.5, 1.5)) #wn = c(0.7, 2.4)
# y <- fmt(n = c(3, 9), type = "n"  , pct = c(0.5, 1.5)) #wn = c(0.7, 2.4)
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
