## ---- test-benchmark-linearAlgebra-vec-dot
lapply(
  X = 1,
  FUN = function(i,
                 n,
                 k,
                 times) {
    x <- matrix(
      data = 1.0,
      nrow = n,
      ncol = k
    )
    output <- microbenchmark::microbenchmark(
      `.vec` = .vec(x),
      `c` = c(x),
      `as.vector` = as.vector(x),
      times = times
    )
    print(output)
    ggplot2::autoplot(output)
  },
  n = 1000,
  k = 1000,
  times = 500
)
