## ---- test-benchmark-linearAlgebra-pinv-of-dcap-dot
k <- sample(1:10, size = 1)
x <- toeplitz((k:1) / k)
dcapk <- .dcap(k)
output <- microbenchmark::microbenchmark(
  .pinv_of_dcap(dcapk),
  MASS::ginv(dcapk),
  times = 1000
)
print(output)
ggplot2::autoplot(output)
rm(
  x,
  k,
  dcapk,
  output
)
