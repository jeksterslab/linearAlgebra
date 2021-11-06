## ---- test-benchmark-linearAlgebra-vec-mean-dot
lapply(
  X = 1,
  FUN = function(i,
                 n,
                 times) {
    x <- rnorm(n = n)
    output <- microbenchmark::microbenchmark(
      `.vec_mean` = .vec_mean(x),
      `vec_mean` = vec_mean(x),
      `mean` = mean(x),
      times = times
    )
    print(output)
    ggplot2::autoplot(output)
  },
  n = 10000,
  times = 500
)
