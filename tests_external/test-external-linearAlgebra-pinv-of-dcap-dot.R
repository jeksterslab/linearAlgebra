## ---- test-external-linearAlgebra-pinv-of-dcap-dot
lapply(
  X = seq_len(10),
  FUN = function(k,
                 text) {
    text <- paste(
      text,
      k
    )
    message(text)
    x <- toeplitz((k:1) / k)
    dcapk <- .dcap(k)
    testthat::test_that(
      text,
      {
        testthat::expect_equal(
          .pinv_of_dcap(dcapk),
          MASS::ginv(dcapk)
        )
      }
    )
  },
  text = "test-external-linearAlgebra-pinv-of-dcap-dot"
)
