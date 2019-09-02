
context("zero_matrix_nest")

test_that("'zero_matrix_nest' returns correct matrix when supplied with valid inputs", {
    nest <- data.frame(v1 = c("A", "A", "B", "A", "B"),
                       v2 = 1:5)
    ans_obtained <- zero_matrix_nest(nest)
    ans_expected <- matrix(c(TRUE, TRUE, FALSE, TRUE, FALSE,
                             FALSE, FALSE, TRUE, FALSE, TRUE),
                           byrow = TRUE,
                           nrow = 2,
                           ncol = 5,
                           dimnames = list(v1 = c("A", "B"),
                                           v2 = as.character(1:5)))
    expect_identical(ans_obtained, ans_expected)
    nest <- data.frame(low = 1:3,
                       up = c("a", "b", "a"))
    ans_obtained <- zero_matrix_nest(nest)
    ans_expected <- matrix(c(TRUE, FALSE, TRUE,
                             FALSE, TRUE, FALSE),
                           byrow = TRUE,
                           nrow = 2,
                           ncol = 3,
                           dimnames = list(up = c("a", "b"),
                                           low = as.character(1:3)))
    expect_identical(ans_obtained, ans_expected)
})


