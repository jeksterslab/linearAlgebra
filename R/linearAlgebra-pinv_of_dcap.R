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
#' @inheritParams dcap
#' @inherit references return
#'
#' @examples
#' pinv_of_dcap(3)
#' @family Symmetric Functions
#' @keywords linearAlgebra symmetric
#' @export
pinv_of_dcap <- function(k) {
  .check_pos_scalar_int(k)
  .pinv_of_dcap(
    .dcap(k)
  )
}
