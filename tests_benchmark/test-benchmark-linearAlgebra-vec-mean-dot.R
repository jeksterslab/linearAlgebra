## ---- test-benchmark-linearAlgebra-vec-mean-dot
x <- rnorm(n = 10000)
output <- microbenchmark::microbenchmark(
  .vec_mean(x),
  vec_mean(x),
  mean(x),
  times = 1000
)
print(output)
ggplot2::autoplot(output)
rm(
  x,
  output
)
