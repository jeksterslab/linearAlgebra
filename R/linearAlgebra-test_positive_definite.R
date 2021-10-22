#' Test for a Positive Definite Matrix
#'
#' Returns `TRUE` if input
#' is a positive definite matrix,
#' and `FALSE` otherwise.
#'
#' A
#' \eqn{k \times k}
#' symmetric matrix
#' \eqn{\mathbf{A}}
#' is positive definite
#' if all of its eigenvalues are positive.
#'
#' `A` is considered to be a positive definite matrix
#' if **NONE** of its eigenvalues are less than or equal to
#' a tolerance value.
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @param x Numeric matrix.
#' @param tol Numeric. Tolerance.
#'
#' @references
#' [Wikipedia: Definite matrix](https://en.wikipedia.org/wiki/Definite_matrix)
#'
#' @return Logical.
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
#' @export
#' @family Linear Algebra Functions
#' @keywords linearAlgebra test
test_positive_definite <- function(x,
                                   tol = 1e-8) {
  stopifnot(
    is.matrix(x),
    x == t(x)
  )
  return(
    !any(eigen(x, symmetric = TRUE, only.values = TRUE)$values <= tol)
  )
}
