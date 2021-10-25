## ---- test-linearAlgebra-vechs
foo <- function(x,
                message) {
  testthat::test_that(message, {
    testthat::expect_equal(
      .sym_of_vechs(
        vechs(x),
        k = dim(x)[1],
        diags = 1
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
        "test-linearAlgebra-vechs",
        k
      )
    )
  }
)
# clean environment
rm(
  foo
)
