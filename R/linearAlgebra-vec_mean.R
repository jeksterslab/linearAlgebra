#' Vector Mean
#'
#' Calculates the mean of a vector.
#' If the input is a matrix,
#' the function vectorizes the matrix
#' before calculating the mean.
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
#' @param x Numeric vector or `n` by `1` matrix.
#'
#' @returns A vector of length 1.
#'
#' @examples
#' x <- rnorm(100)
#' vec_mean(x)
#' vec_mean(matrix(x, ncol = 1))
#' @family Vectorization Functions
#' @keywords linearAlgebra operation
#' @export
vec_mean <- function(x) {
  if (is.matrix(x)) {
    return(
      .vec_mean_array(x)
    )
  }
  if (is.vector(x)) {
    return(
      .vec_mean(x)
    )
  }
}
