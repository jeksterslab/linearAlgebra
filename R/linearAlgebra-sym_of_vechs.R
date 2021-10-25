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
#' @inheritParams vechs_check
#'
#' @references
#' [Wikipedia: Half-vectorization](https://en.wikipedia.org/wiki/Vectorization_(mathematics)#Half-vectorization)
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
#' vechsA <- c(0.5, 0.4, 0.6)
#'
#' sym_of_vechs(vechsA, diags = 1)
#' @family Symmetric Functions
#' @keywords linearAlgebra symmetric
#' @export
sym_of_vechs <- function(x, diags) {
  k <- vechs_check(
    x,
    diags = diags,
    return_k = TRUE
  )
  .sym_of_vechs(
    x = x,
    k = k,
    diags = diags
  )
}
