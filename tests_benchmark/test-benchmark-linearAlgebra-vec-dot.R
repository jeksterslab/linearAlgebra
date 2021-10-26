## ---- test-benchmark-linearAlgebra-vec-dot
x <- matrix(
  data = 1.0,
  nrow = 1000,
  ncol = 1000
)
output <- microbenchmark::microbenchmark(
  .vec(x),
  c(x),
  as.vector(x),
  times = 1000
)
print(output)
ggplot2::autoplot(output)
rm(
  x,
  output
)
