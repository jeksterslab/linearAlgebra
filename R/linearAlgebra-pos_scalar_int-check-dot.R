#' Sanity Checks for Positive Integer Vector of Length 1
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @param x Positive integer.
#' @family Symmetric Functions
#' @keywords linearAlgebra check
#' @noRd
.check_pos_scalar_int <- function(x) {
  stopifnot(
    is.vector(x),
    length(x) == 1,
    x > 0,
    x %% 1 == 0
  )
}
