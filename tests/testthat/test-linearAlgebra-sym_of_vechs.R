## ---- test-linearAlgebra-sym-of-vechs
foo <- function(x,
                message) {
  testthat::test_that(message, {
    testthat::expect_equal(
      sym_of_vechs(.vechs(x), diags = 1),
      x
    )
  })
}
bar <- function(k,
                message) {
  if (k %in% c(2, 4, 5, 7, 8, 9)) {
    testthat::test_that(paste(message, "error"), {
      testthat::expect_error(
        sym_of_vechs(rep(0, times = k), diags = 1)
      )
    })
  }
}
testthat::test_that("test-linearAlgebra-sym_of_vechs matrix", {
  testthat::expect_error(
    sym_of_vechs(matrix(0, 3), diags = 1)
  )
})
lapply(
  X = seq_len(10),
  FUN = function(k) {
    message <- paste(
      "test-linearAlgebra-sym_of_vechs",
      k
    )
    foo(
      x = toeplitz((k:1) / k),
      message = message
    )
    bar(
      k = k,
      message = message
    )
  }
)
# error coverage
testthat::test_that("test-linearAlgebra-sym-of-vechs error in diags", {
  testthat::expect_error(
    sym_of_vechs(
      1:3,
      diags = 1:2
    )
  )
})
# clean environment
rm(
  foo,
  bar
)
