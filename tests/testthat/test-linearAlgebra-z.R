## ---- test-linearAlgebra-z
lapply(
  X = seq_len(3),
  FUN = function(k,
                 text) {
    text <- paste(
      text,
      k
    )
    message(text)
    x <- matrix(
      data = stats::runif(n = 10 * k),
      ncol = k
    )
    if (k == 1) {
      testthat::test_that(
        text,
        {
          testthat::expect_equal(
            sum(
              z(.vec(x))
            ),
            sum(
              scale(x)
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
            z(x)
          ),
          sum(
            scale(x)
          )
        )
      }
    )
  },
  text = "test-linearAlgebra-z"
)
