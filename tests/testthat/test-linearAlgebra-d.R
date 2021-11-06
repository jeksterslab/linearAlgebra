## ---- test-linearAlgebra-d
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
      data = stats::runif(
        n = n * k
      ),
      ncol = k
    )
    if (k == 1) {
      testthat::test_that(
        text,
        {
          testthat::expect_equal(
            sum(
              d(.vec(x))
            ),
            sum(
              scale(
                x,
                center = TRUE,
                scale = FALSE
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
            d(x)
          ),
          sum(
            scale(
              x,
              center = TRUE,
              scale = FALSE
            )
          )
        )
      }
    )
  },
  n = 10,
  text = "test-linearAlgebra-d"
)
