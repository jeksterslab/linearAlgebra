## ---- test-linearAlgebra-vech
foo <- function(x,
                message) {
  testthat::test_that(message, {
    testthat::expect_equal(
      .sym_of_vech(
        vech(x),
        k = dim(x)[1]
      ),
      x
    )
  })
}
lapply(
  X = seq_len(10),
  FUN = function(k) {
    foo(
      x = toeplitz((k:1) / k),
      message = paste(
        "test-linearAlgebra-vech",
        k
      )
    )
  }
)
# clean environment
rm(
  foo
)
