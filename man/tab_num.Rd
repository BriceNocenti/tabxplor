% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tab.R
\name{tab_num}
\alias{tab_num}
\title{Means table}
\usage{
tab_num(
  data,
  row_var,
  col_vars,
  tab_vars,
  wt,
  color = c("auto", "diff", "diff_ci", "after_ci"),
  na = c("keep", "drop", "drop_fct", "drop_num"),
  ref = "tot",
  comp = c("tab", "all"),
  ci = NULL,
  conf_level = 0.95,
  totaltab = "line",
  totaltab_name = "Ensemble",
  tot = NULL,
  total_names = "Total",
  subtext = "",
  digits = 0,
  num = FALSE,
  df = FALSE
)
}
\arguments{
\item{data}{A data frame.}

\item{row_var}{The row variable, which will be printed with one level per line. If
numeric, it will be used as a factor.}

\item{col_vars}{The numeric variables, which will appear in columns :
means and standard deviation are calculated for each levels of \code{row_var} and \code{tab_vars}.}

\item{tab_vars}{<\link[tidyr:tidyr_tidy_select]{tidy-select}> Tab variables :
a subtable is made for each combination of levels of the
selected variables. Leave empty to make a simple cross-table. All tab variables
are converted to factor.}

\item{wt}{A weight variable, of class numeric. Leave empty for unweighted results.}

\item{color}{\code{TRUE} print the color percentages and means based on cells differences from
totals or reference cell, as provided by \code{ref}. Default to \code{FALSE}, no colors.}

\item{na}{The policy to adopt for missing values in row and tab variables (factors),
as a single string.
\itemize{
\item \code{"keep"}: by default, \code{NA}'s of row and tab variables
are printed as an explicit \code{"NA"} level.
\item \code{"drop"}: remove \code{NA}'s in row and tab variables.
}
\code{NA}s in numeric variables are always removed when calculating means. For that reason
the \code{n} field of each resulting \code{\link{fmt}} column, used to calculate confidence
intervals, only takes into account the complete observations (without \code{NA}).
To drop all rows with \code{NA} in any numeric variable first, use \code{\link{tab_prepare}}
or \code{\link{tab_many}} with the \code{na_drop_all} argument.}

\item{ref}{The reference cell to calculate differences and ratios
(used to print \code{colors}) :
\itemize{
\item \code{"auto"}: by default, cell difference from the corresponding total
(rows or cols depending on \code{pct = "row"} or \code{pct = "col"}) is
used for \code{diff} ; cell ratio from the first line (or col) is use for \code{OR}
(odds ratio/relative risks ratio).
\item \code{"tot"}: totals are always used.
\item \code{"first"}: calculate cell difference or ratio from the first cell
of the row or column (useful to color temporal developments).
\item \code{n}: when \code{ref} is an integer, the nth row (or column) is used for comparison.
\item \code{"regex"}: when \code{ref} is a string, it it used as a regular expression,
to match with the names of the rows (or columns). Be precise enough to match only one
column or row, otherwise you get a warning message.
\item \code{"no"}: not use ref and not calculate diffs to gain calculation time.
}}

\item{comp}{Comparison level. When \code{tab_vars} are present, should the
contributions to variance be calculated for each subtable/group (by default,
\code{comp = "tab"}) ? Should they be calculated for the whole table
(\code{comp = "all"}) ?
\code{comp} must be set once and for all the first time you use \code{\link{tab_plain}},
\code{\link{tab_num}} or \code{\link{tab_chi2}} with rows, or \code{\link{tab_ci}}.}

\item{ci}{The type of confidence intervals to calculate, passed to \code{\link{tab_ci}}
(automatically added if needed for \code{color}).
\itemize{
\item \code{"cell"}: absolute confidence intervals of cells percentages.
\item \code{"diff"}: confidence intervals of the difference between a cell and the
relative total cell (or relative first cell when \code{ref = "first"}).
\item \code{"auto"}: \code{ci = "diff"} for means and row/col percentages,
\code{ci = "cell"} for frequencies ("all", "all_tabs").
}}

\item{conf_level}{The confidence level for the confidence intervals,
as a single numeric between 0 and 1. Default to 0.95 (95\%).}

\item{totaltab}{The total table,
if there are subtables/groups (i.e. when \code{tab_vars} is provided) :
\itemize{
\item \code{"line"}: by default, add a general total line (necessary for
calculations with \code{comp = "all"})
\item \code{"table"}: add a complete total table
(i.e. \code{row_var} by \code{col_vars} without \code{tab_vars}).
\item \code{"no"}: not to draw any total table.
}}

\item{totaltab_name}{The name of the total table, as a single string.}

\item{tot}{The totals :
\itemize{
\item \code{c("col", "row")} or \code{"both"} : by default, both total rows and total
columns.
\item \code{"row"}: only total rows.
\item \code{"col"}: only total column.
\item \code{"no"}: remove all totals (after calculations if needed).
}}

\item{total_names}{The names of the totals, as a character vector of length one or two.
Use syntax of type \code{c("Total row", "Total column")} to set different names for
rows and cols.}

\item{subtext}{A character vector to print rows of legend under the table.}

\item{digits}{The number of digits to print, as a single integer.}

\item{num}{Set to \code{TRUE} to obtain a table with normal numeric vectors (not \code{fmt}).}

\item{df}{Set to \code{TRUE} to obtain a plain data.frame (not a \code{tibble}),
with normal numeric vectors (not \code{fmt}). Useful, for example, to pass the table to
correspondence analysis with \pkg{FactoMineR}.}
}
\value{
A \code{tibble} of class \code{tabxplor_tab}. If \code{...} (\code{tab_vars})
are provided, a \code{tab} of class \code{tabxplor_grouped_tab}.
All non-text columns are \code{\link{fmt}} vectors of class \code{tabxplor_fmt},
storing all the data necessary to print formats and colors. Columns with \code{row_var}
and \code{tab_vars} are of class \code{factor} : every added \code{factor} will be
considered as a \code{tab_vars} and used for grouping. To add text columns without
using them in calculations, be sure they are of class \code{character}.
}
\description{
Cross categorical variables with numeric variables, and get a table
of means and standard deviations.
}
\examples{
\donttest{
data <- dplyr::storms \%>\% tab_prepare(category, wind, na_drop_all = wind)
tab_num(data, category, wind, tot = "row", color = "after_ci")
}
}
