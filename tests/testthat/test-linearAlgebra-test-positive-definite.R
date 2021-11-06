## ---- test-linearAlgebra-test-positive-definite
message("test-linearAlgebra-test-positive-definite")
testthat::test_that(
  "test-linearAlgebra-test-positive-definite true",
  {
    testthat::expect_true(
      test_positive_definite(diag(2))
    )
  }
)
testthat::test_that(
  "test-linearAlgebra-test-positive-definite false",
  {
    testthat::expect_false(
      test_positive_definite(
        matrix(
          data = c(1, 2, 3, 2, 4, 5, 3, 5, 6),
          ncol = 3
        )
      )
    )
  }
)
testthat::test_that(
  "test-linearAlgebra-test-positive-definite check error",
  {
    testthat::expect_error(
      .check_positive_definite(
        matrix(
          data = c(1, 2, 3, 2, 4, 5, 3, 5, 6),
          ncol = 3
        )
      )
    )
  }
)
