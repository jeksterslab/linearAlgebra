#' Diagonals of A from vech(A) - Dot Function
#'
#' Diagonals of a matrix from its half-vectorization.
#'
#' Generates a vector of length
#' \eqn{k}
#' of diagonal elements or location in the input vector
#' of an
#' \eqn{k \times k}
#' matrix.
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @param x Vector.
#'   Half-vectorization of a `k` by `k` matrix.
#' @param loc Logical.
#'   If `loc = TRUE`, return the location of the diagonal elements
#'   in the input vector.
#'   If `loc = FALSE`, return the values of the diagonal elements.
#' @inheritParams .dcap
#'
#' @references
#'   [Wikipedia: Half-vectorization](https://en.wikipedia.org/wiki/Vectorization_(mathematics)#Half-vectorization)
#'
#' @returns A vector.
#'
#' @examples
#' A <- matrix(
#'   data = c(
#'     1.0, 0.5, 0.4,
#'     0.5, 1.0, 0.6,
#'     0.4, 0.6, 1.0
#'   ),
#'   ncol = 3
#' )
#' k <- dim(A)[1]
#' vechA <- c(1.0, 0.5, 0.4, 1.0, 0.6, 1.0)
#'
#' .diag_of_vech(vechA, k = k, loc = FALSE)
#' .diag_of_vech(vechA, k = k, loc = TRUE)
#' @family Vectorization Functions
#' @keywords linearAlgebra vectorization dot
#' @export
.diag_of_vech <- function(x,
                          k,
                          loc = FALSE) {
  if (length(x) == 1) {
    if (loc) {
      return(1)
    } else {
      return(x[1])
    }
  }
  j <- 0.5 * (
    2 * k * 1:k - 2 * k + 3 * 1:k - (1:k) * (1:k)
  )
  if (loc) {
    return(j)
  } else {
    return(x[j])
  }
}
