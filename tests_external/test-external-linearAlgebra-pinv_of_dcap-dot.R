## ---- test-external-linearAlgebra-pinv_of_dcap-dot
x_i <- matrix(
  data = c(
    1.0, 0.5, 0.4,
    0.5, 1.0, 0.6,
    0.4, 0.6, 1.0
  ),
  ncol = 3
)
m_i <- dim(x_i)[1]
dcap_i <- .dcap(m_i)
answer_i <- MASS::ginv(dcap_i)
result_i <- .pinv_of_dcap(dcap_i)
testthat::test_that("test-external-linearAlgebra-pinv_of_dcap-dot", {
  testthat::expect_equal(
    result_i,
    answer_i
  )
})
print(
  microbenchmark::microbenchmark(
    .pinv_of_dcap(dcap_i),
    MASS::ginv(dcap_i),
    times = 1000
  )
)
# clean environment
rm(
  x_i,
  m_i,
  dcap_i,
  answer_i,
  result_i
)
