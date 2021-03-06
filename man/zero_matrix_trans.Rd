% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/zero_matrix_trans.R
\name{zero_matrix_trans}
\alias{zero_matrix_trans}
\title{Create a structural zero matrix representing general
transitions between states}
\usage{
zero_matrix_trans(trans, basename = "state")
}
\arguments{
\item{trans}{A named list.}

\item{basename}{Prefix used to construct dimension
names.  Defaults to \code{"state"}.}
}
\value{
A square matrix of \code{TRUE}s and \code{FALSE}s,
with dimnames constructed from \code{nest}.
}
\description{
Create a structural zero matrix representing showing,
permissible transitions between states, where, typically,
many transitions are prohibited. Examples include
transitions between marital statuses or between health
statuses.
}
\details{
\code{trans} is a named list. The names of the list
give the origin states, and the elements of the list
are character vectors with the destination states:
see below for examples.

If an origin state has no destination states (ie it is
an "absorbing" state), then the list element for that
origin can be a zero-length character vector, or
\code{NULL}.

Transitions from a state to itself are allowed: a
an element of \code{trans} can include its own name.
}
\examples{
trans <- list(Single = c("Married", "Separated"),
              Married = "Separated",
              Separated = "Married")
zero_matrix_trans(trans)
zero_matrix_trans(trans, basename = "marital_status")


trans <- list(Primary = c("Primary", "Secondary"), # transition to self
              Secondary = c("Secondary", "Tertiary"), # transition to self
              Tertiary = "Tertiary") # transition to self
zero_matrix_trans(trans)

trans <- list(Healthy = c("Sick", "Dead"),
              Sick = c("Healthy", "Dead"),
              Dead = NULL) # absorbing state
zero_matrix_trans(trans)

trans <- list(Healthy = c("Sick", "Dead"),
              Sick = c("Healthy", "Dead"),
              Dead = character()) # character rather than NULL
zero_matrix_trans(trans)
}
\seealso{
\code{zero_matrix_diag} deals with a common
special case.
}
