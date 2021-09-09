## ---- test-linearAlgebra-test_positive_definite
testthat::test_that("linearAlgebra-test_positive_definite true", {
  testthat::expect_true(
    test_positive_definite(diag(2))
  )
})
testthat::test_that("linearAlgebra-test_positive_definite false", {
  testthat::expect_false(
    test_positive_definite(
      matrix(
        data = c(1, 2, 3, 2, 4, 5, 3, 5, 6),
        ncol = 3
      )
    )
  )
})
