#' The Duplication Matrix
#'
#' Creates a duplication matrix.
#'
#' The duplication matrix
#' \eqn{\mathbf{D}_{k}}
#' is the
#' \eqn{
#'     k^2 \times \frac{1}{2} k \left( k + 1 \right)
#' }
#' matrix for a given
#' \eqn{k \times k}
#' symmetric matrix
#' \eqn{\mathbf{A}}
#' where
#'
#' \deqn{
#'    \mathbf{D}_{k} \mathrm{vech} \left( \mathbf{A} \right)
#'    =
#'    \mathrm{vec} \left( \mathbf{A} \right)
#'    \quad
#'    \left(
#'    \mathbf{A}
#'    =
#'    \mathbf{A}^{\prime}
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
#' @param k Positive integer.
#'   Dimension of the `k` by `k` matrix.
#'
#' @references
#'   [Wikipedia: Duplication matrix](https://en.wikipedia.org/wiki/Duplication_and_elimination_matrices#Duplication_matrix)
#'
#'   [Wikipedia: Vectorization](https://en.wikipedia.org/wiki/Vectorization_(mathematics))
#'
#'   Magnus, J. R., & Neudecker, H. (1980).
#'   The elimination matrix: Some lemmas and applications.
#'   SIAM  Journal  on  Algebraic  Discrete  Methods,
#'   1(4),
#'   422--449.
#'   [https://doi.org/10.1137/0601049](https://doi.org/10.1137/0601049)
#'
#'   Magnus, J. R., & Neudecker, H. (2019).
#'   Matrix  differential  calculus with  applications  in  statistics  and  econometrics.
#'   Wiley.
#'   [https://doi.org/10.1002/9781119541219](https://doi.org/10.1002/9781119541219)
#'
#' @returns A matrix.
#'
#' @examples
#' dcap(3)
#' @family Symmetric Functions
#' @keywords linearAlgebra symmetric
#' @export
dcap <- function(k) {
  .check_pos_scalar_int(k)
  .dcap(k)
}
