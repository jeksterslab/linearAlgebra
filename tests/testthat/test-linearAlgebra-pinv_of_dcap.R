## ---- test-linearAlgebra-pinv_of_dcap
foo <- function(x,
                message) {
  vechx <- .vech(x)
  testthat::test_that(message, {
    testthat::expect_equal(
      c(
        pinv_of_dcap(dim(x)[1]) %*% as.vector(x)
      ),
      vechx
    )
  })
}
lapply(
  X = seq_len(10),
  FUN = function(k) {
    foo(
      x = toeplitz((k:1) / k),
      message = paste("test-linearAlgebra-pinv_of_dcap", k)
    )
  }
)
# clean environment
rm(
  foo
)
