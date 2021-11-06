#' Test for a Positive Definite Matrix - Dot Function
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @inherit test_positive_definite description details references return
#' @inheritParams test_positive_definite
#'
#' @param eigen Eigen decomposition of a matrix.
#'   Output of the [base::eigen()] function.
#'
#' @family Linear Algebra Functions
#' @keywords linearAlgebra test dot
#' @noRd
.test_positive_definite <- function(eigen,
                                    tol = 1e-6) {
  !any(
    eigen$values <= tol
  )
}
