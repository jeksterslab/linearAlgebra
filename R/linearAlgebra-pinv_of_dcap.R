#' The Moore-Penrose Inverse of the Duplication Matrix
#'
#' The Moore-Penrose inverse of the duplication matrix.
#'
#' The Moore-Penrose inverse of the duplication matrix
#' \eqn{\mathbf{D}_{k}}
#' is the
#' \eqn{
#'     \frac{1}{2} k \left( k + 1 \right) \times k^2
#' }
#' matrix given by
#'
#' \deqn{
#'     \mathbf{D}_{k}^{+}
#'     =
#'     \left(
#'     \mathbf{D}_{k}^{\prime}
#'     \mathbf{D}_{k}
#'     \right)^{-1}
#'     \mathbf{D}_{k}^{\prime}
#' }
#'
#' where
#'
#' \deqn{
#'    \mathbf{D}_{k}^{+}
#'    \mathrm{vec}
#'    \left(
#'    A
#'    \right)
#'    =
#'    \mathrm{vech}
#'    \left(
#'    \mathbf{A}
#'    \right)
#'    \quad
#'    \left(
#'    \mathbf{A} = \mathbf{A}^{\prime}
#'    \right)
#' }
#'
#' \eqn{
#'     \mathrm{vec} \left( \cdot \right)
#' }
#' is the vectorization of a matrix,
#' and
#' \eqn{
#'     \mathrm{vech} \left( \cdot \right)
#' }
#' is the half-vectorization of a matrix.
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @param x Numeric matrix
#'   The duplication matrix.
#'
#' @references
#' [Wikipedia: Duplication matrix](https://en.wikipedia.org/wiki/Duplication_and_elimination_matrices#Duplication_matrix)
#'
#' Magnus, J. R., & Neudecker, H. (1980).
#' The elimination matrix: Some lemmas and applications.
#' SIAM  Journal  on  Algebraic  Discrete  Methods,
#' 1(4),
#' 422--449.
#' [https://doi.org/10.1137/0601049](https://doi.org/10.1137/0601049)
#'
#' Magnus, J. R., & Neudecker, H. (2019).
#' Matrix  differential  calculus with  applications  in  statistics  and  econometrics.
#' Wiley.
#' [https://doi.org/10.1002/9781119541219](https://doi.org/10.1002/9781119541219)
#'
#' @returns A matrix.
#'
#' @examples
#' pinv_of_dcap(dcap(3))
#' @export
#' @family Symmetric Functions
#' @keywords linearAlgebra symmetric
pinv_of_dcap <- function(x) {
  stopifnot(
    is.matrix(x)
  )
  return(
    tcrossprod(
      solve(
        crossprod(x)
      ),
      x
    )
  )
}
