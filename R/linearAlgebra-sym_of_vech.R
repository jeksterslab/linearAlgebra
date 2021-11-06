#' Symmetric matrix A from vech(A)
#'
#' Symmetric matrix from its half-vectorization.
#'
#' Generates an
#' \eqn{k \times k}
#' symmetric matrix
#' from a
#' \eqn{\frac{1}{2}k(k + 1)}
#' vector.
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @param x Vector of length `0.5 * k(k + 1)`.
#'   Half-vectorization of a `k` by `k` matrix.
#'   \eqn{\mathrm{vech} \left( \mathbf{A}_{k \times k} \right)}.
#'
#' @inherit vec references
#'
#' @returns A matrix.
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
#' sym_of_vech(vechA)
#' @family Symmetric Functions
#' @keywords linearAlgebra symmetric
#' @export
sym_of_vech <- function(x) {
  .sym_of_vech(
    x = x,
    k = .check_vech(
      x,
      return_k = TRUE
    )
  )
}
