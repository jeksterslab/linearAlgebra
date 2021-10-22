#' Half-Vectorize
#'
#' Half-vectorize a matrix.
#'
#' The half-vectorization of an
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
#' vector obtained from
#' the vectorization of
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
#' @param x Matrix.
#' @param names Logical.
#'   Add names.
#' @inheritParams vechnames
#'
#' @references
#' [Wikipedia: Half-vectorization](https://en.wikipedia.org/wiki/Vectorization_(mathematics)#Half-vectorization)
#'
#' Magnus, J. R., & Neudecker, H. (2019).
#' Matrix  differential  calculus with  applications  in  statistics  and  econometrics.
#' Wiley.
#' [https://doi.org/10.1002/9781119541219](https://doi.org/10.1002/9781119541219)
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
#' vech(A)
#' @export
#' @family Vectorization Functions
#' @keywords linearAlgebra vectorization
vech <- function(x,
                 names = FALSE,
                 sep = ".") {
  stopifnot(
    is.matrix(x),
    dim(x)[1] == dim(2)
    # checks for k = k
    # does not check for symmetry
    # only cares about the lower triangular values
  )
  output <- x[lower.tri(x, diag = TRUE)]
  if (names) {
    varnames <- colnames(x)
    if (is.null(varnames)) {
      varnames <- paste0(seq_len(dim(x)[2]))
    }
    varnames <- vechnames(varnames, sep = sep)
    names(output) <- varnames
  }
  return(output)
}
