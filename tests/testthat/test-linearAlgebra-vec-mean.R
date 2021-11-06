## ---- test-linearAlgebra-vec-mean
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
          vec_mean(x),
          mean(x)
        )
        testthat::expect_equal(
          vec_mean(.vec(x)),
          mean(x)
        )
      }
    )
  },
  text = "test-linearAlgebra-vec-mean"
)
