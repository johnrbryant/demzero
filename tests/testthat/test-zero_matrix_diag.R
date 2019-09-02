
context("zero_matrix_diag")

test_that("'zero_matrix_diag' returns correct matrix when supplied with valid inputs", {
    ans_obtained <- zero_matrix_diag(state = c("a", "b", "c"), basename = "reg")
    ans_expected <- matrix(c(TRUE, FALSE, FALSE,
                             FALSE, TRUE, FALSE,
                             FALSE, FALSE, TRUE),
                           byrow = TRUE,
                           nrow = 3,
                           ncol = 3,
                           dimnames = list(reg_orig = c("a", "b", "c"),
                                           reg_dest = c("a", "b", "c")))
    expect_identical(ans_obtained, ans_expected)
})


