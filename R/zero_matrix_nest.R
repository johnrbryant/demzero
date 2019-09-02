
#' Create a structural zero matrix representing nested
#' categories
#'
#' Create a structural zero matrix representing a situation where
#' one set of categories is nested within another - eg
#' counties within states, or detailed occupations within
#' broad occupational categories.
#'
#' Nesting can be contrasted with crossing, where each
#' set of categories are independent of the others,
#' eg age vs sex.
#'
#' \code{nest} is a data.frame providing a mapping between
#' the higher-level categories and lower-level categories.
#' \code{nest} has two columns: one with the higher-level
#' categories, and one with the lower-level categories.
#' Each row gives the higher-level category associated with a
#' lower-level category. The order of the columns and rows
#' does not matter.
#'
#' Each lower-level category can belong to only one
#' higher-level category.
#'
#' \code{zero_matrix_nest} returns a logical matrix
#' indicating whether each lower-level category
#' nests within the corresponding higher-level category.
#'
#' @param nest A data.frame providing a mapping between
#' the higher-level and lower-level categories.
#'
#' @return A matrix of \code{TRUE}s and \code{FALSE}s,
#' with dimnames constructed from \code{nest}.
#'
#' @seealso Function \code{\link{add_dim_nest}} adds a
#' dimension representing the higher-level categories to
#' an array containing the lower-level categories.
#' 
#' @examples
#' nest <- data.frame(region = c("Asia", "Africa", "Africa"),
#'                    country = c("India", "Nigeria", "Zimbabwe"))
#' zero_matrix_nest(nest)
#'
#' nest <- data.frame(level2 = c("Bacterial infections",
#'                               "Viral infections",
#'                               "Infections caused by fungi",
#'                               "Lip, oral cavity, pharynx",
#'                               "Digestive organs"),
#'                    level1 = c("Infectious and parasitic diseases",
#'                               "Infectious and parasitic diseases",
#'                               "Infectious and parasitic diseases",
#'                               "Neoplasms",
#'                               "Neoplasms"))
#' zero_matrix_nest(nest)
#' @export
## HAS_TESTS
zero_matrix_nest <- function(nest) {
    nest <- demcheck::err_tdy_many_to_one(x = nest,
                                         name = "nest")
    demcheck::err_names_complete(x = nest,
                                 name = "nest")
    names_nest <- names(nest)
    has_duplicated_categ <- sapply(lapply(nest, duplicated), any)
    i_upper <- which(has_duplicated_categ)
    i_lower <- which(!has_duplicated_categ)
    vals_upper <- nest[[i_upper]]
    vals_lower <- nest[[i_lower]]
    vals_upper_unique <- unique(vals_upper)
    nrow_ans <- length(vals_upper_unique)
    ncol_ans <- length(vals_lower)
    names_ans <- names_nest[c(i_upper, i_lower)]
    l <- list(vals_upper_unique, vals_lower)
    dimnames_ans <- structure(l, names = names_ans)
    ans <- matrix(FALSE,
                  nrow = nrow_ans,
                  ncol = ncol_ans,
                  dimnames = dimnames_ans)
    j <- seq_along(vals_lower)
    i <- match(vals_upper, vals_upper_unique)
    ans[cbind(i, j)] <- TRUE
    ans
}
