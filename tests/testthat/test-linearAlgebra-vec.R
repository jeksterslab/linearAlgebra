## ---- test-linearAlgebra-vec
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
          vec(x),
          c(x)
        )
      }
    )
  },
  text = "test-linearAlgebra-vec"
)
