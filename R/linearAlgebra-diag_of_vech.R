#' Diagonals of A from vech(A)
#'
#' Diagonals of a matrix from its half-vectorization.
#'
#' Generates a vector of length
#' \eqn{k}
#' of diagonal elements or location in the input vector
#' of an
#' \eqn{k \times k}
#' matrix.
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @param x Vector.
#' @param loc Logical.
#' If `loc = TRUE`, return the location of the diagonal elements
#' in the input vector.
#' If `loc = FALSE`, return the values of the diagonal elements.
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
#' diag_of_vech(vechA, loc = FALSE)
#' diag_of_vech(vechA, loc = TRUE)
#' @export
#' @family Vectorization Functions
#' @keywords linearAlgebra vectorization
diag_of_vech <- function(x,
                         loc = FALSE) {
  # k = k by k dimensions of the symmetric matrix
  # length(x) = k(k + 1) / 2 solve for k
  stopifnot(
    is.vector(x),
    is.logical(loc)
  )
  k <- 0.5 * (sqrt(1 + 8 * length(x)) - 1)
  if (k %% 1 != 0) {
    stop("Length of \"x\" is not valid.")
  }
  if (length(x) == 1) {
    if (loc) {
      return(1)
    } else {
      return(x[1])
    }
  }
  i <- sapply(
    X = seq_len(k),
    FUN = function(i, k) {
      k * (i - 1) + 1 - (
        (
          (i - 2) * (i - 1)
        ) / 2
      )
    },
    k = k
  )
  if (loc) {
    return(i)
  } else {
    return(x[i])
  }
}
