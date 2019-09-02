
#' Create a structural zero matrix representing general
#' transitions between states
#'
#' Create a structural zero matrix representing showing,
#' permissible transitions between states, where, typically,
#' many transitions are prohibited. Examples include
#' transitions between marital statuses or between health
#' statuses.
#'
#' \code{trans} is a named list. The names of the list
#' give the origin states, and the elements of the list
#' are character vectors with the destination states:
#' see below for examples.
#'
#' If an origin state has no destination states (ie it is
#' an "absorbing" state), then the list element for that
#' origin can be a zero-length character vector, or
#' \code{NULL}.
#'
#' Transitions from a state to itself are allowed: a
#' an element of \code{trans} can include its own name.
#' 
#' @param trans A named list.
#' @param basename Prefix used to construct dimension
#' names.  Defaults to \code{"state"}.
#'
#' @return A square matrix of \code{TRUE}s and \code{FALSE}s,
#' with dimnames constructed from \code{nest}.
#'
#' @seealso \code{zero_matrix_diag} deals with a common
#' special case.
#'
#' @examples
#' trans <- list(Single = c("Married", "Separated"),
#'               Married = "Separated",
#'               Separated = "Married")
#' zero_matrix_trans(trans)
#' zero_matrix_trans(trans, basename = "marital_status")
#' 
#' 
#' trans <- list(Primary = c("Primary", "Secondary"), # transition to self
#'               Secondary = c("Secondary", "Tertiary"), # transition to self
#'               Tertiary = "Tertiary") # transition to self
#' zero_matrix_trans(trans)
#'
#' trans <- list(Healthy = c("Sick", "Dead"),
#'               Sick = c("Healthy", "Dead"),
#'               Dead = NULL) # absorbing state
#' zero_matrix_trans(trans)
#'
#' trans <- list(Healthy = c("Sick", "Dead"),
#'               Sick = c("Healthy", "Dead"),
#'               Dead = character()) # character rather than NULL
#' zero_matrix_trans(trans)
#' @export
## HAS_TESTS
zero_matrix_trans <- function(trans, basename = "state") {
    demcheck::err_trans_list(x = trans,
                             name = "trans")
    demcheck::err_is_string(x = basename,
                            name = "basename")
    states <- names(trans)
    nrow_ans <- length(states)
    ncol_ans <- length(states)
    names_ans <- sprintf("%s_%s", basename, c("orig", "dest"))
    l <- list(states, states)
    dimnames_ans <- structure(l, names = names_ans)
    ans <- matrix(FALSE,
                  nrow = nrow_ans,
                  ncol = ncol_ans,
                  dimnames = dimnames_ans)
    lengths_trans <- sapply(trans, length)
    orig <- rep(states, times = lengths_trans)
    dest <- unlist(trans)
    i <- match(orig, states)
    j <- match(dest, states)
    ans[cbind(i, j)] <- TRUE
    ans
}
