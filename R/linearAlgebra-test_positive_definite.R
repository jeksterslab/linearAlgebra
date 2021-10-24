#' Test for a Positive Definite Matrix
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @inheritParams .test_positive_definite
#' @inherit .test_positive_definite description details references return
#'
#' @examples
#' # TRUE
#' test_positive_definite(diag(2))
#'
#' # FALSE
#' test_positive_definite(
#'   matrix(
#'     data = c(1, 2, 3, 2, 4, 5, 3, 5, 6),
#'     ncol = 3
#'   )
#' )
#' @family Linear Algebra Functions
#' @keywords linearAlgebra test
#' @export
test_positive_definite <- function(x,
                                   tol = 1e-8) {
  stopifnot(
    is.matrix(x),
    x == t(x)
  )
  .test_positive_definite(
    x = x,
    tol = tol
  )
}
