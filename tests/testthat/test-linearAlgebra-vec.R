## ---- test-linearAlgebra-vec
foo <- function(x,
                message) {
  testthat::test_that(message, {
    testthat::expect_equal(
      vec(x),
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
        "test-linearAlgebra-vec",
        k
      )
    )
  }
)
# clean environment
rm(
  foo
)
