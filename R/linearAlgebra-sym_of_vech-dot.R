#' Symmetric matrix A from vech(A) - Dot Function
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
#' @param x Vector.
#'   Half-vectorization of a `k` by `k` matrix.
#' @inheritParams .dcap
#'
#' @inherit .vec references
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
#'   nrow = 3
#' )
#' vechA <- c(1.0, 0.5, 0.4, 1.0, 0.6, 1.0)
#'
#' sym_of_vech(vechA)
#' @family Symmetric Functions
#' @keywords linearAlgebra symmetric dot
#' @export
.sym_of_vech <- function(x,
                         k) {
  sym <- matrix(
    data = 0,
    nrow = k,
    ncol = k
  )
  sym[lower.tri(sym, diag = TRUE)] <- x
  sym[upper.tri(sym)] <- t(sym)[upper.tri(sym)]
  sym
}
