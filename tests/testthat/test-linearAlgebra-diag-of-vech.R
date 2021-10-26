## ---- test-linearAlgebra-diag-of-vech
foo <- function(x,
                message) {
  # loc = FALSE
  diags <- rep(x = 1, times = dim(x)[1])
  testthat::test_that(paste(message, "loc = TRUE"), {
    testthat::expect_equal(
      diag_of_vech(
        .vech(x),
        loc = FALSE
      ),
      diags
    )
  })
  # loc = TRUE
  vechx <- .vech(x)
  diags <- rep(x = NA, times = length(vechx))
  for (i in seq_along(vechx)) {
    if (vechx[i] == 1) {
      diags[i] <- i
    }
  }
  diags <- diags[complete.cases(diags)]
  testthat::test_that(paste(message, "loc = FALSE"), {
    testthat::expect_equal(
      diag_of_vech(
        .vech(x),
        loc = TRUE
      ),
      diags
    )
  })
}
lapply(
  X = seq_len(10),
  FUN = function(k) {
    foo(
      x = toeplitz((k:1) / k),
      message = paste("test-linearAlgebra-diag-of-vech", k)
    )
  }
)
# clean environment
rm(
  foo
)
