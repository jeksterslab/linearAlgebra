## ---- test-linearAlgebra-vechs
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
          .sym_of_vechs(
            vechs(x),
            k = dim(x)[1],
            diags = 1
          ),
          x
        )
      }
    )
  },
  text = "test-linearAlgebra-vechs"
)
