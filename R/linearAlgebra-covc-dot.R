#' Consistent Estimator of the Covariance Matrix
#'
#' @inherit covc description details references return
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @param x Numeric matrix.
#'   Unbiased estimate of the covariance.
#' @param n Positive integer.
#'   Sample size.
#'
#' @family Covariance Functions
#' @keywords linearAlgebra covariance
#' @noRd
.covc <- function(x, n) {
  x * (n - 1) / n
}
