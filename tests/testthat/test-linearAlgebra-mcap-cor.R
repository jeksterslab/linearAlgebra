## ---- test-linearAlgebra-mcap-cor
message("test-linearAlgebra-mcap-cor")
testthat::test_that(
  "test-linearAlgebra-mcap-cor 2 by 2",
  {
    testthat::expect_equal(
      mcap_cor(2),
      matrix(
        data = c(
          0, 0.0, 0.0, 0,
          0, 0.5, 0.5, 0,
          0, 0.5, 0.5, 0,
          0, 0.0, 0.0, 0
        ),
        nrow = 4
      )
    )
  }
)
testthat::test_that(
  "test-linearAlgebra-mcap-cor 3 by 3",
  {
    testthat::expect_equal(
      mcap_cor(3),
      matrix(
        data = c(
          0, 0.0, 0.0, 0.0, 0, 0.0, 0.0, 0.0, 0,
          0, 0.5, 0.0, 0.5, 0, 0.0, 0.0, 0.0, 0,
          0, 0.0, 0.5, 0.0, 0, 0.0, 0.5, 0.0, 0,
          0, 0.5, 0.0, 0.5, 0, 0.0, 0.0, 0.0, 0,
          0, 0.0, 0.0, 0.0, 0, 0.0, 0.0, 0.0, 0,
          0, 0.0, 0.0, 0.0, 0, 0.5, 0.0, 0.5, 0,
          0, 0.0, 0.5, 0.0, 0, 0.0, 0.5, 0.0, 0,
          0, 0.0, 0.0, 0.0, 0, 0.5, 0.0, 0.5, 0,
          0, 0.0, 0.0, 0.0, 0, 0.0, 0.0, 0.0, 0
        ),
        nrow = 9
      )
    )
  }
)
testthat::test_that(
  "test-linearAlgebra-mcap-cor 4 by 4",
  {
    testthat::expect_equal(
      mcap_cor(4),
      matrix(
        data = c(
          0, 0.0, 0.0, 0.0, 0.0, 0, 0.0, 0.0, 0.0, 0.0, 0, 0.0, 0.0, 0.0, 0.0, 0,
          0, 0.5, 0.0, 0.0, 0.5, 0, 0.0, 0.0, 0.0, 0.0, 0, 0.0, 0.0, 0.0, 0.0, 0,
          0, 0.0, 0.5, 0.0, 0.0, 0, 0.0, 0.0, 0.5, 0.0, 0, 0.0, 0.0, 0.0, 0.0, 0,
          0, 0.0, 0.0, 0.5, 0.0, 0, 0.0, 0.0, 0.0, 0.0, 0, 0.0, 0.5, 0.0, 0.0, 0,
          0, 0.5, 0.0, 0.0, 0.5, 0, 0.0, 0.0, 0.0, 0.0, 0, 0.0, 0.0, 0.0, 0.0, 0,
          0, 0.0, 0.0, 0.0, 0.0, 0, 0.0, 0.0, 0.0, 0.0, 0, 0.0, 0.0, 0.0, 0.0, 0,
          0, 0.0, 0.0, 0.0, 0.0, 0, 0.5, 0.0, 0.0, 0.5, 0, 0.0, 0.0, 0.0, 0.0, 0,
          0, 0.0, 0.0, 0.0, 0.0, 0, 0.0, 0.5, 0.0, 0.0, 0, 0.0, 0.0, 0.5, 0.0, 0,
          0, 0.0, 0.5, 0.0, 0.0, 0, 0.0, 0.0, 0.5, 0.0, 0, 0.0, 0.0, 0.0, 0.0, 0,
          0, 0.0, 0.0, 0.0, 0.0, 0, 0.5, 0.0, 0.0, 0.5, 0, 0.0, 0.0, 0.0, 0.0, 0,
          0, 0.0, 0.0, 0.0, 0.0, 0, 0.0, 0.0, 0.0, 0.0, 0, 0.0, 0.0, 0.0, 0.0, 0,
          0, 0.0, 0.0, 0.0, 0.0, 0, 0.0, 0.0, 0.0, 0.0, 0, 0.5, 0.0, 0.0, 0.5, 0,
          0, 0.0, 0.0, 0.5, 0.0, 0, 0.0, 0.0, 0.0, 0.0, 0, 0.0, 0.5, 0.0, 0.0, 0,
          0, 0.0, 0.0, 0.0, 0.0, 0, 0.0, 0.5, 0.0, 0.0, 0, 0.0, 0.0, 0.5, 0.0, 0,
          0, 0.0, 0.0, 0.0, 0.0, 0, 0.0, 0.0, 0.0, 0.0, 0, 0.5, 0.0, 0.0, 0.5, 0,
          0, 0.0, 0.0, 0.0, 0.0, 0, 0.0, 0.0, 0.0, 0.0, 0, 0.0, 0.0, 0.0, 0.0, 0
        ),
        nrow = 16
      )
    )
  }
)
