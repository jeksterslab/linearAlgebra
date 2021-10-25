## ---- test-external-linearAlgebra-vec-dot
x <- matrix(
  data = 1:10,
  ncol = 2
)
print(
  microbenchmark::microbenchmark(
    .vec(x),
    as.vector(x),
    times = 1000
  )
)
# clean environment
rm(
  x
)
