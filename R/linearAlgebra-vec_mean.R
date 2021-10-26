#' Vector Mean
#'
#' Calculates the mean of a vector.
#' If the input is a matrix,
#' the function vectorizes the matrix
#' before calculating the mean.
#'
#' @inherit .vec_mean details
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @param x Numeric vector or matrix.
#' @family Vectorization Functions
#' @keywords linearAlgebra operation
#' @export
vec_mean <- function(x) {
  if (is.vector(x)) {
    return(
      .vec_mean(x)
    )
  }
  .vec_mean(
    vec(x)
  )
}
