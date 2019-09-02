
#' Add a dimension representing a higher-level set
#' of categories
#' 
#' Add an extra dimension to an array \code{x}. Categories
#' for an existing dimension in \code{x} must nest within
#' categories for the new dimension. For instance,
#' if an existing dimension in \code{x} is districts,
#' the extra dimension might be the provinces that the
#' disticts belong to. The relationship between the
#' existing lower-level categories and the new
#' upper-level categories is described by data frame
#' \code{nest}.
#'
#' @inheritParams zero_matrix_nest
#' @param x An array, including a
#' \code{\link[dembase:DemographicArray-class]{DemographicArray}}.
#'
#' @return An object with the same class as \code{x},
#' but with an extra dimension.
#'
#' @seealso \code{\link{zero_matrix_nest}} creates a matrix
#' describing the relationship between the upper-level and
#' lower-level categories.
#'
#' @examples
#' x <- array(1:6,
#'            dim = c(2, 3),
#'            dimnames = list(sex = c("Female", "Male"),
#'                            district = c("A", "B", "C")))
#' nest <- data.frame(province = c("X", "Y", "X"),
#'                    district = c("A", "B", "C"))
#' x
#' add_dim_nest(x = x, nest = nest)
#' @export
#' @rdname add_dim_nest
setGeneric("add_dim_nest",
           function(x, nest)
               standardGeneric("add_dim_nest"))

#' @export
#' @rdname add_dim_nest
setMethod("add_dim_nest",
          signature(x = "array"),
          function(x, nest) {
              demcheck::err_array_metadata_complete(x = x,
                                                    name = "x")
              dim_x <- dim(x)
              if (any(dim_x == 0L))
                  stop(gettextf("'%s' has zero-length dimensions",
                                "x"))
              nest <- demcheck::err_tdy_many_to_one(x = nest,
                                                    name = nest)
              demcheck::err_names_complete(x = nest,
                                           name = "nest")
              names_x <- names(dimnames(x))
              dimnames_x <- dimnames(x)
              names_nest <- names(nest)
              has_duplicated_categ <- sapply(lapply(nest, duplicated), any)
              i_nest_upper <- which(has_duplicated_categ)
              i_nest_lower <- which(!has_duplicated_categ)
              name_upper <- names_nest[[i_nest_upper]]
              name_lower <- names_nest[[i_nest_lower]]
              vals_nest_upper <- nest[[i_nest_upper]]
              vals_nest_lower <- nest[[i_nest_lower]]
              vals_upper_unique <- unique(vals_nest_upper)
              n_upper_unique <- length(vals_upper_unique)
              mar_lower <- match(name_lower, names_x, nomatch = 0L)
              x_has_name_lower <- mar_lower > 0L
              x_has_name_upper <- name_upper %in% names_x
              if (!x_has_name_lower)
                  stop(gettextf(paste("'%s' does not have a dimension corresponding to the \"%s\"",
                                      "column in '%s' holding lower-level categories"),
                                "x", name_lower, "nest"))
              if (x_has_name_upper)
                  stop(gettextf(paste("trying to add a dimension called \"%s\" to '%s',",
                                      "but '%s' already has a dimension called \"%s\""),
                                name_upper, "x", "x", name_upper))
              dimnames_x_lower <- dimnames_x[[mar_lower]]
              categ_in_x_not_in_nest <- !(dimnames_x_lower %in% vals_nest_lower)
              if (any(categ_in_x_not_in_nest)) {
                  i_categ_in_x_not_in_nest  <- match(TRUE, categ_in_x_not_in_nest)
                  stop(gettextf("category \"%s\" in dimension \"%s\" of '%s' not found in column \"%s\" of '%s'",
                                dimnames_x_lower[[i_categ_in_x_not_in_nest]],
                                name_lower,
                                "x",
                                name_lower,
                                "nest"))
              }
              indices_lower <- slice.index(x, MARGIN = mar_lower)
              upper_to_lower <- tapply(vals_nest_lower, vals_nest_upper, unique)
              upper_to_lower <- upper_to_lower[vals_upper_unique] # reorder
              upper_to_indices_lower <- lapply(upper_to_lower,
                                               match,
                                               table = dimnames_x_lower)
              ans <- rep(list(array(0L, dim = dim_x)),
                         times = n_upper_unique)
              for (i_upper in seq_len(n_upper_unique)) {
                  belongs_to_upper <- indices_lower %in% upper_to_indices_lower[[i_upper]]
                  ans[[i_upper]][belongs_to_upper] <- x@.Data[belongs_to_upper]
              }
              dim_ans <- c(dim_x, n_upper_unique)
              dimnames_upper <- structure(list(vals_upper_unique),
                                          names = name_upper)
              dimnames_ans <- c(dimnames_x, dimnames_upper)
              ans <- array(unlist(ans),
                           dim = dim_ans, 
                           dimnames = dimnames_ans)
              ans
          })



## Having this function and separate methods for "Counts"
## and "Values" is a hack, required by the fact that class "DemographicArray"
## does not inherit directly from class "array".  Will
## rewrite this when 'demarray' is ready.
add_dim_nest_DemographicArray <- function(x, nest) {
    .Data_ans <- add_dim_nest(x = x@.Data,
                              nest = nest)
    dimtypes_x <- dembase::dimtypes(x, use.names = FALSE)
    DimScales_x <- dembase::DimScales(x, use.names = FALSE)
    dimnames_ans <- dimnames(.Data_ans)
    names_ans <- names(dimnames_ans)
    n <- length(names_ans)
    name_extra <- names_ans[[n]]
    dimnames_extra <- dimnames_ans[[n]]
    dimtype_extra <- "state" ## TODO - maybe revisit this
    DimScale_extra <- methods::new("Categories",
                                   dimvalues = dimnames_extra)
    dimtypes_ans <- c(dimtypes_x, dimtype_extra)
    DimScales_ans <- c(DimScales_x, list(DimScale_extra))
    metadata_ans <- methods::new("MetaData",
                                 nms = names_ans,
                                 dimtypes = dimtypes_ans,
                                 DimScales = DimScales_ans)
    methods::new(class(x),
                 .Data = .Data_ans,
                 metadata = metadata_ans)
}
    

#' @export
#' @rdname add_dim_nest
setMethod("add_dim_nest",
          signature(x = "Counts"),
          function(x, nest) {
              add_dim_nest_DemographicArray(x = x,
                                            nest = nest)
          })


#' @export
#' @rdname add_dim_nest
setMethod("add_dim_nest",
          signature(x = "Counts"),
          function(x, nest) {
              add_dim_nest_DemographicArray(x = x,
                                            nest = nest)
          })
