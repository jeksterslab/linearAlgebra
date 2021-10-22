#' Symmetric matrix A from vech(A)
#'
#' Symmetric matrix from its half-vectorization.
#'
#' Generates an
#' \eqn{k \times k}
#' symmetric matrix
#' from a
#' \eqn{\frac{1}{2}k(k + 1)}
#' vector.
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @param x Vector.
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
#' vechA <- c(1.0, 0.5, 0.4, 1.0, 0.6, 1.0)
#'
#' sym_of_vech(vechA)
#' @export
#' @family Symmetric Functions
#' @keywords linearAlgebra symmetric
sym_of_vech <- function(x) {
  # k = k by k dimensions of the symmetric matrix
  # length(x) = k(k + 1) / 2 solve for k
  # output = symmetric matrix output
  stopifnot(is.vector(x))
  k <- 0.5 * (sqrt(1 + 8 * length(x)) - 1)
  if (k %% 1 != 0) {
    stop("Length of \"x\" is not valid.")
  }
  output <- matrix(
    data = 0,
    nrow = k,
    ncol = k
  )
  output[lower.tri(output, diag = TRUE)] <- x
  output[upper.tri(output)] <- t(output)[upper.tri(output)]
  return(output)
}
