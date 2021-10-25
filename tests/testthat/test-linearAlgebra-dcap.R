## ---- test-linearAlgebra-dcap
foo <- function(x,
                message) {
  testthat::test_that(message, {
    testthat::expect_equal(
      c(
        dcap(
          dim(x)[1]
        ) %*% .vech(x)
      ),
      c(x)
    )
  })
}
lapply(
  X = seq_len(10),
  FUN = function(k) {
    foo(
      x = toeplitz((k:1) / k),
      message = paste(
        "test-linearAlgebra-dcap",
        k
      )
    )
  }
)
# clean environment
rm(
  foo
)
