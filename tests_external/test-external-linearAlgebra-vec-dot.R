## ---- test-external-linearAlgebra-vec-dot
x_i <- matrix(
  data = 1:10,
  ncol = 2
)
print(
  microbenchmark::microbenchmark(
    .vec(x_i),
    as.vector(x_i),
    times = 1000
  )
)
# clean environment
rm(
  x_i
)
