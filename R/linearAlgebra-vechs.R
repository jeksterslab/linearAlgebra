#' Strict Half-Vectorize
#'
#' Apply a strict half-vectorization,
#' that is, half-vectorize an
#' \eqn{k \times k}
#' matrix
#' without the diagonal elements.
#'
#' The strict half-vectorization of a
#' \eqn{k \times k}
#' matrix
#' \eqn{\mathbf{A}},
#' given by
#' \eqn{
#'     \mathrm{vechs} \left( \mathbf{A} \right)
#' },
#' is the
#' \eqn{
#'     \frac{1}{2}
#'     k
#'     \left(
#'     k + 1
#'     \right)
#'     -
#'     k
#'     \times
#'     1
#' }
#' vector obtained from
#' the vectorization of
#' \eqn{\mathbf{A}},
#' given by
#' \eqn{
#'     \mathrm{vec} \left( \mathbf{A} \right)
#' },
#' where that all diagonal and upper diagonal elements of
#' \eqn{\mathbf{A}}
#' are eliminated.
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @param x Matrix.
#' @inheritParams vech
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
#'
#' vechs(A)
#' @export
#' @family Vectorization Functions
#' @keywords linearAlgebra vectorization
vechs <- function(x,
                  names = FALSE,
                  sep = ".") {
  stopifnot(
    is.matrix(x),
    dim(x)[1] == dim(2),
    dim(x)[1] > 1
  )
  output <- x[lower.tri(x, diag = FALSE)]
  if (names) {
    varnames <- colnames(x)
    if (is.null(varnames)) {
      varnames <- paste0(seq_len(dim(x)[2]))
    }
    varnames <- vechsnames(varnames, sep = sep)
    names(output) <- varnames
  }
  return(output)
}
