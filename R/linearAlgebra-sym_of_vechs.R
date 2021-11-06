#' Symmetric matrix A from vechs(A)
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
#' @param x Vector of length `0.5 * k(k + 1) - k`.
#'   Strict half-vectorization of a `k` by `k` matrix.
#'   \eqn{\mathrm{vechs} \left( \mathbf{A}_{k \times k} \right)}.
#' @param diags Vector.
#'   Diagonal elements.
#'
#' @inherit vec references
#'
#' @returns A matrix.
#'
#' @family Symmetric Functions
#' @keywords linearAlgebra symmetric
#' @export
sym_of_vechs <- function(x,
                         diags) {
  .sym_of_vechs(
    x = x,
    k = .check_vechs(
      x,
      diags = diags,
      return_k = TRUE
    ),
    diags = diags
  )
}
