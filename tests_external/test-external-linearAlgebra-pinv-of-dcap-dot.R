## ---- test-external-linearAlgebra-pinv-of-dcap-dot
message("test-external-linearAlgebra-pinv-of-dcap-dot")
k <- sample(1:10, size = 1)
x <- toeplitz((k:1) / k)
dcapk <- .dcap(k)
testthat::test_that(
  "test-external-linearAlgebra-pinv-of-dcap-dot",
  {
    testthat::expect_equal(
      .pinv_of_dcap(dcapk),
      MASS::ginv(dcapk)
    )
  }
)
rm(
  x,
  k,
  dcapk
)
