
context("zero_matrix_trans")

test_that("'zero_matrix_trans' returns correct matrix when supplied with valid inputs", {
    trans <- list(a = c("b", "c"),
                  b = "a",
                  c = NULL)
    ans_obtained <- zero_matrix_trans(trans)
    ans_expected <- matrix(c(FALSE, TRUE, TRUE,
                             TRUE, FALSE, FALSE,
                             FALSE, FALSE, FALSE),
                           byrow = TRUE,
                           nrow = 3,
                           ncol = 3,
                           dimnames = list(state_orig = c("a", "b", "c"),
                                           state_dest = c("a", "b", "c")))
    expect_identical(ans_obtained, ans_expected)
    trans <- list(a = c("b", "c"),
                  b = "a",
                  c = "c")
    ans_obtained <- zero_matrix_trans(trans, basename = "dim")
    ans_expected <- matrix(c(FALSE, TRUE, TRUE,
                             TRUE, FALSE, FALSE,
                             FALSE, FALSE, TRUE),
                           byrow = TRUE,
                           nrow = 3,
                           ncol = 3,
                           dimnames = list(dim_orig = c("a", "b", "c"),
                                           dim_dest = c("a", "b", "c")))
    expect_identical(ans_obtained, ans_expected)
})


