## ---- test-linearAlgebra-deltacapsq
lapply(
  X = seq_len(3),
  FUN = function(k,
                 n,
                 text) {
    text <- paste(
      text,
      k
    )
    message(text)
    x <- matrix(
      data = stats::rnorm(n = n * k),
      ncol = k
    ) %*% chol(
      toeplitz((k:1) / k)
    )
    mu <- colMeans(x)
    sigmacap <- stats::cov(x)
    if (k == 1) {
      sigmacap <- stats::var(x)
      testthat::test_that(
        text,
        {
          testthat::expect_equal(
            sum(
              deltacapsq(
                .vec(x),
                mu = mu,
                sigmacap = sigmacap
              )
            ),
            sum(
              mahalanobis(
                x,
                center = mu,
                cov = sigmacap
              )
            )
          )
        }
      )
    }
    testthat::test_that(
      text,
      {
        testthat::expect_equal(
          sum(
            deltacapsq(
              x,
              mu = mu,
              sigmacap = sigmacap
            )
          ),
          sum(
            mahalanobis(
              x,
              center = mu,
              cov = sigmacap
            )
          )
        )
      }
    )
  },
  n = 1000,
  text = "test-linearAlgebra-deltacapsq"
)
