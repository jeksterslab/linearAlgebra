#' Half-Vectorize - Dot Function
#'
#' Apply a half-vectorization,
#' that is,
#' eliminate the upper diagonal elements of a
#' \eqn{k \times k}
#' matrix.
#'
#' The half-vectorization of a
#' \eqn{k \times k}
#' matrix
#' \eqn{\mathbf{A}},
#' given by
#' \eqn{
#'     \mathrm{vech} \left( \mathbf{A} \right)
#' },
#' is the
#' \eqn{
#'     \frac{1}{2}
#'     k
#'     \left(
#'     k + 1
#'     \right)
#'     \times
#'     1
#' }
#' vector obtained from the vectorization of
#' \eqn{\mathbf{A}},
#' given by
#' \eqn{
#'     \mathrm{vec} \left( \mathbf{A} \right)
#' },
#' where that all upper diagonal elements of
#' \eqn{\mathbf{A}}
#' are eliminated.
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @inheritParams .vec
#'
#' @inherit .vec references return
#'
#' @examples
#' A <- matrix(
#'   data = c(
#'     1.0, 0.5, 0.4,
#'     0.5, 1.0, 0.6,
#'     0.4, 0.6, 1.0
#'   ),
#'   nrow = 3
#' )
#'
#' .vech(A)
#' @family Vectorization Functions
#' @keywords linearAlgebra vectorization dot
#' @export
.vech <- function(x) {
  x[
    lower.tri(
      x = x,
      diag = TRUE
    )
  ]
}
