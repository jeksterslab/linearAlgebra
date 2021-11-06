## ---- test-linearAlgebra-vechsnames
lapply(
  X = seq_len(3),
  FUN = function(k,
                 text) {
    text <- paste(
      text,
      k
    )
    message(text)
    varnames <- seq_len(k)
    testthat::test_that(
      text,
      {
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
      }
    )
    testthat::test_that(
      text,
      {
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
      }
    )
  },
  text = "test-linearAlgebra-vechsnames"
)
