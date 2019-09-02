
#' Create a diagonal structural zero matrix
#'
#' Create a structural zero matrix with \code{TRUE}
#' on the diagonal and \code{FALSE} everywhere else.
#' This matrix represents situtation where a movement
#' between any pair of distinct states is permitted,
#' but a movement from a state to itself is prohibited.
#'
#' Diagonal structural zero matrices are often
#' used when modelling migration, since migration statistics
#' often show movements between regions, but not movements
#' within regions.
#'
#' @inheritParams zero_matrix_trans
#' @param state A character vector, giving the names of the
#' states.
#'
#' @return A square matrix with \code{TRUE} on the
#' diagonal, \code{FALSE} elsewhere, and
#' dimnames constructed from \code{states}.
#'
#' @seealso \code{zero_matrix_trans} deals with the
#' general case.
#'
#' @examples
#' states <- c("Thailand", "Malaysia", "Singapore")
#' zero_matrix_diag(states)
#' zero_matrix_diag(states, basename = "country")
#' @export
## NO_TESTS
zero_matrix_diag <- function(state, basename = "state") {
    demcheck::err_character_complete(state)
    demcheck::err_is_string(x = basename,
                            name = "basename")
    nrow_ans <- length(state)
    ncol_ans <- length(state)
    l <- list(state, state)
    names_ans <- sprintf("%s_%s", basename, c("orig", "dest"))
    dimnames_ans <- structure(l, names = names_ans)
    ans <- matrix(FALSE,
                  nrow = nrow_ans,
                  ncol = ncol_ans,
                  dimnames = dimnames_ans)
    diag(ans) <- TRUE
    ans
}
