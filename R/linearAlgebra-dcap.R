#' Duplication Matrix
#'
#' Create a duplication matrix.
#'
#' The duplication matrix
#' \eqn{
#' \mathbf{D}_{k}
#' }
#' is the
#' \eqn{
#' k^2 \times \frac{k \left( k + 1 \right)}{2}
#' }
#' matrix
#' for a given
#' \eqn{
#' k \times k
#' }
#' symmetric matrix
#' \eqn{
#' \mathbf{A}
#' }
#' where
#'
#' \deqn{
#'    \mathbf{D}_{k} \mathrm{vech} \left( \mathbf{A} \right)
#'    =
#'    \mathrm{vec} \left( \mathbf{A} \right)
#' }
#'
#' \eqn{
#' \mathrm{vec} \left( \cdot \right)
#' }
#' is the vectorization of a matrix,
#' and
#' \eqn{
#' \mathrm{vech} \left( \cdot \right)
#' }
#' is the half-vectorization of a matrix.
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @param x Integer.
#'   Dimension of the symmetric matrix.
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
#' dcap(3)
#' @export
#' @family Linear Algebra Functions
#' @keywords linearAlgebra symmetric
dcap <- function(x) {
  stopifnot(
    is.vector(x),
    length(x) == 1
  )
  m <- as.integer(x)
  stopifnot(
    m > 0
  )
  sym <- diag(m)
  i <- seq_len(
    0.5 * m * (m + 1)
  )
  sym[lower.tri(sym, diag = TRUE)] <- i
  sym[upper.tri(sym)] <- t(sym)[upper.tri(sym)]
  return(
    outer(
      X = c(sym),
      Y = i,
      FUN = function(x, y) {
        ifelse(
          test = x == y,
          yes = 1,
          no = 0
        )
      }
    )
  )
}
