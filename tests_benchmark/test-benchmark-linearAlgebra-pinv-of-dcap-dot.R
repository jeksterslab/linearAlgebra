## ---- test-benchmark-linearAlgebra-pinv-of-dcap-dot
lapply(
  X = seq_len(3),
  FUN = function(k,
                 times) {
    x <- toeplitz((k:1) / k)
    dcapk <- .dcap(k)
    output <- microbenchmark::microbenchmark(
      `.pinv_of_dcap` = .pinv_of_dcap(dcapk),
      `MASS::ginv` = MASS::ginv(dcapk),
      times = times
    )
    print(output)
    ggplot2::autoplot(output)
  },
  times = 500
)
