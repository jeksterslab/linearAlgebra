## ---- test-linearAlgebra-vec-mean
foo <- function(x,
                message) {
  testthat::test_that(message, {
    testthat::expect_equal(
      vec_mean(x),
      mean(x)
    )
    testthat::expect_equal(
      vec_mean(.vec(x)),
      mean(x)
    )
  })
}
lapply(
  X = seq_len(10),
  FUN = function(k) {
    foo(
      x = toeplitz((k:1) / k),
      message = paste(
        "test-linearAlgebra-vec-mean",
        k
      )
    )
  }
)
# clean environment
rm(
  foo
)
