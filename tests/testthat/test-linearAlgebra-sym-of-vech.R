## ---- test-linearAlgebra-sym-of-vech
lapply(
  X = seq_len(3),
  FUN = function(k,
                 text) {
    text <- paste(
      text,
      k
    )
    message(text)
    x <- toeplitz((k:1) / k)
    testthat::test_that(
      text,
      {
        testthat::expect_equal(
          sym_of_vech(.vech(x)),
          x
        )
      }
    )
    testthat::test_that(
      paste(text, "matrix"),
      {
        testthat::expect_error(
          sym_of_vech(x)
        )
      }
    )
    if (k %in% c(2, 4, 5, 7, 8, 9)) {
      testthat::test_that(
        paste(text, "error"),
        {
          testthat::expect_error(
            sym_of_vech(rep(0, times = k))
          )
        }
      )
    }
  },
  text = "test-linearAlgebra-sym-of-vech"
)
