## ---- test-linearAlgebra-vechsnames
foo <- function(k,
                message) {
  varnames <- seq_len(k)
  testthat::test_that(message, {
    testthat::expect_equal(
      .vechs(
        outer(
          X = varnames,
          Y = varnames,
          FUN = function(x, y) {
            paste0(x, ".", y)
          }
        )
      ),
      vechsnames(varnames)
    )
  })
  testthat::test_that(message, {
    testthat::expect_equal(
      .vechs(
        outer(
          X = varnames,
          Y = varnames,
          FUN = function(x, y) {
            paste0(x, ".", y)
          }
        )
      ),
      names(
        vechs(
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
        "test-linearAlgebra-vechsnames",
        k
      )
    )
  }
)
# clean environment
rm(
  foo
)
