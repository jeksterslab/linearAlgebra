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
#' @param x Vector.
#' @param diags Vector.
#'   Diagonal elements.
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
#' @export
#' @family Symmetric Functions
#' @keywords linearAlgebra symmetric
sym_of_vechs <- function(x, diags) {
  # k = k by k dimensions of the symmetric matrix
  # length(x) = k(k + 1) / 2 solve for k
  # output = symmetric matrix output
  stopifnot(
    is.vector(x),
    is.vector(diags)
  )
  k <- 0.5 * (sqrt(1 + 8 * length(x)) + 1)
  if (k %% 1 != 0) {
    stop("Length of \"x\" is not valid.")
  }
  output <- matrix(
    data = 0,
    nrow = k,
    ncol = k
  )
  diags_k <- length(diags)
  stopifnot(diags_k == 1 || diags_k == k)
  output[lower.tri(output, diag = FALSE)] <- x
  output[upper.tri(output)] <- t(output)[upper.tri(output)]
  diag(output) <- diags
  return(output)
}
