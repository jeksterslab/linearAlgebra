#' Sanity Checks for Positive Definite Matrix
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @param eigen Eigen decomposition of a matrix.
#'   Output of the [base::eigen()] function.
#' @param tol Numeric. Tolerance.
#' @family Linear Algebra Functions
#' @keywords linearAlgebra test check
#' @noRd
.check_positive_definite <- function(eigen,
                                     tol = 1e-6) {
  stopifnot(
    !any(
      eigen$values <= tol
    )
  )
}
