% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/add_dim_nest.R
\name{add_dim_nest}
\alias{add_dim_nest}
\alias{add_dim_nest,array-method}
\alias{add_dim_nest,Counts-method}
\title{Add a dimension representing a higher-level set
of categories}
\usage{
add_dim_nest(x, nest)

\S4method{add_dim_nest}{array}(x, nest)

\S4method{add_dim_nest}{Counts}(x, nest)

\S4method{add_dim_nest}{Counts}(x, nest)
}
\arguments{
\item{x}{An array, including a
\code{\link[dembase:DemographicArray-class]{DemographicArray}}.}

\item{nest}{A data.frame providing a mapping between
the higher-level and lower-level categories.}
}
\value{
An object with the same class as \code{x},
but with an extra dimension.
}
\description{
Add an extra dimension to an array \code{x}. Categories
for an existing dimension in \code{x} must nest within
categories for the new dimension. For instance,
if an existing dimension in \code{x} is districts,
the extra dimension might be the provinces that the
disticts belong to. The relationship between the
existing lower-level categories and the new
upper-level categories is described by data frame
\code{nest}.
}
\examples{
x <- array(1:6,
           dim = c(2, 3),
           dimnames = list(sex = c("Female", "Male"),
                           district = c("A", "B", "C")))
nest <- data.frame(province = c("X", "Y", "X"),
                   district = c("A", "B", "C"))
x
add_dim_nest(x = x, nest = nest)
}
\seealso{
\code{\link{zero_matrix_nest}} creates a matrix
describing the relationship between the upper-level and
lower-level categories.
}
