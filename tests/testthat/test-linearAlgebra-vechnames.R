## ---- test-linearAlgebra-vechnames
foo <- function(k,
                message) {
  varnames <- seq_len(k)
  testthat::test_that(message, {
    testthat::expect_equal(
      .vech(
        outer(
          X = varnames,
          Y = varnames,
          FUN = function(x, y) {
            paste0(x, ".", y)
          }
        )
      ),
      vechnames(varnames)
    )
  })
  testthat::test_that(message, {
    testthat::expect_equal(
      .vech(
        outer(
          X = varnames,
          Y = varnames,
          FUN = function(x, y) {
            paste0(x, ".", y)
          }
        )
      ),
      names(
        vech(
          toeplitz((k:1) / k),
          names = TRUE
        )
      )
    )
  })
}
lapply(
  X = seq_len(10),
  FUN = function(k) {
    foo(
      k = k,
      message = paste(
        "test-linearAlgebra-vechnames",
        k
      )
    )
  }
)
# clean environment
rm(
  foo
)
