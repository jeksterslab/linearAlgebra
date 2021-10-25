#' Sanity Checks for the Half-Vectorization
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @param x Vector of length `0.5 * k(k + 1)`.
#'   Half-vectorization of a `k` by `k` matrix.
#'   \eqn{\mathrm{vech} \left( \mathbf{A}_{k \times k} \right)}.
#' @param return_k Logical.
#'   Return valid `k`.
#' @family Vectorization Functions
#' @keywords linearAlgebra check
#' @export
vech_check <- function(x,
                       return_k = FALSE) {
  if (!is.vector(x)) {
    stop(
      "The half-vectorization is not of class vector."
    )
  }
  k <- 0.5 * (
    sqrt(
      1 + 8 * length(x)
    ) - 1
  )
  if (k %% 1 != 0) {
    stop("Length of the half-vectorization is not valid.")
  }
  if (return_k) {
    return(k)
  }
}
