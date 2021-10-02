## ---- test-linearAlgebra-vec
x_i <- matrix(
  data = 1:10,
  ncol = 2
)
answer_i <- as.vector(x_i)
result_i <- vec(x_i)
testthat::test_that("test-linearAlgebra-vec 5 by 2", {
  testthat::expect_equal(
    result_i,
    answer_i
  )
})
x_i <- matrix(
  c("a", "b", "b", "c"),
  ncol = 2
)
answer_i <- as.vector(x_i)
result_i <- vec(x_i)
testthat::test_that("test-linearAlgebra-vec 2 by 2", {
  testthat::expect_equal(
    result_i,
    answer_i
  )
})
n_i <- 100
k_i <- sample(
  1:10,
  size = 1
)
mu_i <- rep(
  x = 0,
  times = k_i
)
sigmacap_i <- matrix(
  runif(
    n = 1,
    min = 0,
    max = 1
  ),
  nrow = k_i,
  ncol = k_i
)
diag(sigmacap_i) <- 1
x_i <- matrix(
  data = rnorm(
    n = n_i * k_i
  ),
  nrow = n_i,
  ncol = k_i
) %*% (
  chol(sigmacap_i)
) + (
  matrix(
    data = 1,
    nrow = n_i,
    ncol = 1
  ) %*% mu_i
)
x_i <- cov(x_i)
answer_i <- as.vector(x_i)
result_i <- vec(x_i)
testthat::test_that("test-linearAlgebra-vec random cov", {
  testthat::expect_equal(
    result_i,
    answer_i
  )
})
# clean environment
rm(
  x_i,
  answer_i,
  result_i,
  n_i,
  k_i,
  mu_i,
  sigmacap_i
)
