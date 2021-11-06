## ---- test-linearAlgebra-vech
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
          .sym_of_vech(
            vech(x),
            k = dim(x)[1]
          ),
          x
        )
      }
    )
  },
  text = "test-linearAlgebra-vech"
)
