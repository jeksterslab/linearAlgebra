#' Symmetric matrix A from vechs(A) - Dot Function
#'
#' Symmetric matrix from its strict half-vectorization.
#'
#' Generates an
#' \eqn{k \times k}
#' symmetric matrix
#' from a
#' \eqn{\frac{1}{2}(k(k + 1)) - k}
#' vector.
#' The
#' \eqn{k \times 1}
#' vector of diagonal values should be supplied.
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @param x Vector.
#'   Strict half-vectorization of a `k` by `k` matrix.
#' @param diags Vector.
#'   Diagonal elements.
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
#' vechsA <- c(0.5, 0.4, 0.6)
#'
#' .sym_of_vechs(vechsA, k = 3, diags = 1)
#' @family Symmetric Functions
#' @keywords linearAlgebra symmetric dot
#' @export
.sym_of_vechs <- function(x,
                          k,
                          diags) {
  sym <- matrix(
    data = 0,
    nrow = k,
    ncol = k
  )
  sym[lower.tri(sym, diag = FALSE)] <- x
  sym[upper.tri(sym)] <- t(sym)[upper.tri(sym)]
  diag(sym) <- diags
  sym
}
