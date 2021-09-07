#' Vectorize
#'
#' Vectorize a matrix.
#'
#' Generates a vector of length \eqn{mn}
#' from the elements of an \eqn{m \times n} matrix.
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @param x Matrix.
#'
#' @references
#' [Wikipedia: Vectorization](https://en.wikipedia.org/wiki/Vectorization_(mathematics))
#'
#' @returns A vector.
#'
#' @examples
#' x <- matrix(
#'   data = c(
#'     1.0, 0.5, 0.4,
#'     0.5, 1.0, 0.6,
#'     0.4, 0.6, 1.0
#'   ),
#'   ncol = 3
#' )
#'
#' vec(x)
#' @export
#' @family Linear Algebra Functions
#' @keywords linearAlgebra vectorization
vec <- function(x) {
  return(
    as.vector(x)
  )
}
