## ---- test-linearAlgebra-sym-of-vech
foo <- function(x,
                message) {
  testthat::test_that(message, {
    testthat::expect_equal(
      sym_of_vech(.vech(x)),
      x
    )
  })
}
bar <- function(k,
                message) {
  if (k %in% c(2, 4, 5, 7, 8, 9)) {
    testthat::test_that(paste(message, "error"), {
      testthat::expect_error(
        sym_of_vech(rep(0, times = k))
      )
    })
  }
}
testthat::test_that("test-linearAlgebra-sym-of-vech matrix", {
  testthat::expect_error(
    sym_of_vech(matrix(0, 3))
  )
})
lapply(
  X = seq_len(10),
  FUN = function(k) {
    message <- paste("test-linearAlgebra-sym-of-vech", k)
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
# clean environment
rm(
  foo,
  bar
)
