#' Sanity Checks for the k Dimension of a k by k Matrix
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @param k Positive integer.
#'   Dimension of the `k` by `k` matrix.
#' @family Symmetric Functions
#' @keywords linearAlgebra check
#' @export
k_check <- function(k) {
  stopifnot(
    is.vector(k),
    length(k) == 1,
    k > 0,
    k %% 1 == 0
  )
}
