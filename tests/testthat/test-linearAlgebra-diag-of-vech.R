## ---- test-linearAlgebra-diag-of-vech
lapply(
  X = seq_len(3),
  FUN = function(k,
                 text) {
    text <- paste(
      text,
      k
    )
    message(text)
    x <- toeplitz((k:1) / k)
    # loc = FALSE
    diags <- rep(x = 1, times = dim(x)[1])
    testthat::test_that(
      paste(text, "loc = TRUE"),
      {
        testthat::expect_equal(
          diag_of_vech(
            .vech(x),
            loc = FALSE
          ),
          diags
        )
      }
    )
    # loc = TRUE
    vechx <- .vech(x)
    diags <- rep(x = NA, times = length(vechx))
    for (i in seq_along(vechx)) {
      if (vechx[i] == 1) {
        diags[i] <- i
      }
    }
    diags <- diags[complete.cases(diags)]
    testthat::test_that(
      paste(text, "loc = FALSE"),
      {
        testthat::expect_equal(
          diag_of_vech(
            .vech(x),
            loc = TRUE
          ),
          diags
        )
      }
    )
  },
  text = "test-linearAlgebra-diag-of-vech"
)
