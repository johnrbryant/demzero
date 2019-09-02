
context("add_dim_nest")

test_that("'add_dim_nest' works with ordinary arrays", {
    x <- array(1L,
               dim = 5L,
               dimnames = list(v2 = 1:5))
    nest <- data.frame(v1 = c("A", "A", "B", "A", "B"),
                       v2 = 1:5)
    ans_obtained <- add_dim_nest(x = x, nest = nest)
    ans_expected <- matrix(c(1L, 1L, 0L, 1L, 0L,
                             0L, 0L, 1L, 0L, 1L),
                           nrow = 5,
                           ncol = 2,
                           dimnames = list(v2 = as.character(1:5),
                                           v1 = c("A", "B")))
    expect_identical(ans_obtained, ans_expected)
    x <- array(1:10,
               dim = c(2, 5),
               dimnames = list(sex = c("f", "m"),
                               reg = 1:5))
    nest <- data.frame(reg = 1:5,
                       country = c("b", "b", "a", "c", "a"))
    ans_obtained <- add_dim_nest(x = x, nest = nest)
    expect_identical(dim(ans_obtained), c(2L, 5L, 3L))
    expect_identical(sum(ans_obtained[,,"a"]), sum(x[,c(3, 5)]))
    expect_identical(sum(ans_obtained[,,"b"]), sum(x[,c(1, 2)]))
    expect_identical(sum(ans_obtained[,,"c"]), sum(x[,4]))
    expect_identical(dimnames(ans_obtained)[[3]], c("b", "a", "c"))
})

test_that("'add_dim_nest' works with demographic arrays", {
    x <- dembase::Counts(array(1L,
                               dim = 5L,
                               dimnames = list(v2 = 1:5)))
    nest <- data.frame(v1 = c("A", "A", "B", "A", "B"),
                       v2 = 1:5)
    ans_obtained <- add_dim_nest(x = x, nest = nest)
    ans_expected <- dembase::Counts(matrix(c(1L, 1L, 0L, 1L, 0L,
                                             0L, 0L, 1L, 0L, 1L),
                                           nrow = 5,
                                           ncol = 2,
                                           dimnames = list(v2 = as.character(1:5),
                                                           v1 = c("A", "B"))))
    expect_identical(ans_obtained, ans_expected)
    x <- dembase::Values(array(1:10,
                               dim = c(2, 5),
                               dimnames = list(sex = c("f", "m"),
                                               reg = 1:5)))
    nest <- data.frame(reg = 1:5,
                       country = c("b", "b", "a", "c", "a"))
    ans_obtained <- add_dim_nest(x = x, nest = nest)
    expect_identical(dim(ans_obtained), c(2L, 5L, 3L))
    expect_identical(sum(ans_obtained[,,"a"]), sum(x[,c(3, 5)]))
    expect_identical(sum(ans_obtained[,,"b"]), sum(x[,c(1, 2)]))
    expect_identical(sum(ans_obtained[,,"c"]), sum(x[,4]))
    expect_identical(dimnames(ans_obtained)[[3]], c("b", "a", "c"))
})



