#' Sanity Checks for the Strict Half-Vectorization
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @param x Vector of length `0.5 * k(k + 1) - k`.
#'   Strict half-vectorization of a `k` by `k` matrix.
#'   \eqn{\mathrm{vechs} \left( \mathbf{A}_{k \times k} \right)}.
#' @param diags Vector.
#'   Diagonal elements.
#' @inheritParams vech_check
#' @family Vectorization Functions
#' @keywords linearAlgebra check
#' @export
vechs_check <- function(x,
                        diags = NULL,
                        return_k = FALSE) {
  if (!is.vector(x)) {
    stop(
      "The strict half-vectorization is not of class vector."
    )
  }
  k <- 0.5 * (
    sqrt(
      1 + 8 * length(x)
    ) + 1
  )
  if (k %% 1 != 0) {
    stop("Length of the strict half-vectorization is not valid.")
  }
  if (!is.null(diags)) {
    stopifnot(
      is.vector(diags)
    )
    diagsk <- length(diags)
    if (!(diagsk == 1 || diagsk == k)) {
      stop(
        "Length of the diagonal elements is not valid."
      )
    }
  }
  if (return_k) {
    return(k)
  }
}
