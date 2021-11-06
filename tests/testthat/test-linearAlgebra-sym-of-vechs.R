## ---- test-linearAlgebra-sym-of-vechs
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
          sym_of_vechs(.vechs(x), diags = 1),
          x
        )
      }
    )
    testthat::test_that(
      paste(text, "error diags"),
      {
        testthat::expect_error(
          sym_of_vechs(
            .vechs(x),
            diags = rep(x = 1, times = dim(x)[1] + 1)
          )
        )
      }
    )
    testthat::test_that(
      paste(text, "error matrix"),
      {
        testthat::expect_error(
          sym_of_vechs(x, diags = 1)
        )
      }
    )
    if (k %in% c(2, 4, 5, 7, 8, 9)) {
      testthat::test_that(
        paste(text, "error"),
        {
          testthat::expect_error(
            sym_of_vechs(rep(0, times = k), diags = 1)
          )
        }
      )
    }
  },
  text = "test-linearAlgebra-sym_of_vechs"
)
