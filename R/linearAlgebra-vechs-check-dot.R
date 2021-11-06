#' Sanity Checks for the Strict Half-Vectorization
#'
#' @author Ivan Jacob Agaloos Pesigan
#'

#' @inheritParams sym_of_vechs
#' @param diags Vector.
#'   Diagonal elements.
#' @inheritParams .check_vech
#' @family Vectorization Functions
#' @keywords linearAlgebra check
#' @noRd
.check_vechs <- function(x,
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
