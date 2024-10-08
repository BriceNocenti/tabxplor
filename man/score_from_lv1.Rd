% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{score_from_lv1}
\alias{score_from_lv1}
\title{Create a score variable from factors}
\usage{
score_from_lv1(data, name, vars_list)
}
\arguments{
\item{data}{A data.frame.}

\item{name}{The name of the variable to create.}

\item{vars_list}{The list of the factors to count
(only the first level is counted, as 1) ; as a character vector.}
}
\value{
The data.frame, with a new variable.
}
\description{
Create a score variable from factors
}
\examples{
data <- tibble::tibble(group = factor(c("G1", "G1", "G2", "G2", "G3", "G3")),
                       a = factor(c("Oui", "Oui", "Oui", "Oui", "Non", "Oui")),
                       b = factor(c("Oui", "Non", "Non", "Oui", "Non", "Oui")),
                       c = factor(c("Oui", "Oui", "Non", "Non", "Oui", "Oui")))
data |>
  score_from_lv1("score", vars_list = c("a", "b", "c")) |>
  tab(group, score, digits = 1)
}
