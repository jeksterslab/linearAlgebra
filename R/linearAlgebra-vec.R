#' Vectorize
#'
#' Vectorize a matrix.
#'
#' The vectorization of an
#' \eqn{m \times n}
#' matrix
#' \eqn{\mathbf{A}},
#' given by
#' \eqn{
#'     \mathrm{vec} \left( \mathbf{A} \right)
#' },
#' is the
#' \eqn{mn \times 1}
#' vector obtained by stacking the elements of
#' \eqn{\mathbf{A}}
#' column-wise.
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @param x Matrix.
#'
#' @references
#'   [Wikipedia: Vectorization](https://en.wikipedia.org/wiki/Vectorization_(mathematics))
#'
#'   Magnus, J. R., & Neudecker, H. (2019).
#'   Matrix  differential  calculus with  applications  in  statistics  and  econometrics.
#'   Wiley.
#'   [https://doi.org/10.1002/9781119541219](https://doi.org/10.1002/9781119541219)
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
#'   nrow = 3
#' )
#'
#' vec(A)
#' @family Vectorization Functions
#' @keywords linearAlgebra vectorization
#' @export
vec <- function(x) {
  stopifnot(
    is.matrix(x)
  )
  .vec(x = x)
}
