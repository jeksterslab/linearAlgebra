#' Vector Mean - Dot Function
#'
#' Calculates the mean of a vector.
#'
#' The vector mean is given by
#'
#' \deqn{
#'   \frac{1}{n}
#'   \left(
#'   \mathbf{1}_{n}^{\prime}
#'   \mathbf{x}
#'   \right)
#' }
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @param x Numeric vector
#' @family Vectorization Functions
#' @keywords linearAlgebra operation dot
#' @export
.vec_mean <- function(x) {
  sum(x) / length(x)
}
