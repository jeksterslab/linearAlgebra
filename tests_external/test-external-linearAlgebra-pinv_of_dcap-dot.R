## ---- test-external-linearAlgebra-pinv_of_dcap-dot
k <- sample(1:10, size = 1)
x <- toeplitz((k:1) / k)
dcapk <- .dcap(k)
testthat::test_that("test-external-linearAlgebra-pinv_of_dcap-dot", {
  testthat::expect_equal(
    .pinv_of_dcap(dcapk),
    MASS::ginv(dcapk)
  )
})
print(
  microbenchmark::microbenchmark(
    .pinv_of_dcap(dcapk),
    MASS::ginv(dcapk),
    times = 1000
  )
)
# clean environment
rm(
  x,
  k,
  dcapk
)
