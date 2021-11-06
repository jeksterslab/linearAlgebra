#' Diagonals of A from vech(A)
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
#'
#' @references
#'   [Wikipedia: Half-vectorization](https://en.wikipedia.org/wiki/Vectorization_(mathematics)#Half-vectorization)
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
#' vechA <- c(1.0, 0.5, 0.4, 1.0, 0.6, 1.0)
#'
#' diag_of_vech(vechA, loc = FALSE)
#' diag_of_vech(vechA, loc = TRUE)
#' @family Vectorization Functions
#' @keywords linearAlgebra vectorization
#' @export
diag_of_vech <- function(x,
                         loc = FALSE) {
  k <- .check_vech(x, return_k = TRUE)
  stopifnot(is.logical(loc))
  .diag_of_vech(
    x,
    k = k,
    loc = loc
  )
}
