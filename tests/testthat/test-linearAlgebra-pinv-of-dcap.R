## ---- test-linearAlgebra-pinv-of-dcap
lapply(
  X = seq_len(3),
  FUN = function(k,
                 text) {
    x <- toeplitz((k:1) / k)
    vechx <- .vech(x)
    testthat::test_that(
      text,
      {
        testthat::expect_equal(
          c(
            pinv_of_dcap(dim(x)[1]) %*% as.vector(x)
          ),
          vechx
        )
      }
    )
  },
  text = "test-linearAlgebra-pinv-of-dcap"
)
