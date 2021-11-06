#' The Diagonal Pattern Matrix - Dot Function
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @inheritParams mcap_diag
#'
#' @family Symmetric Functions
#' @keywords linearAlgebra symmetric dot
#' @noRd
.mcap_diag <- function(k) {
  x <- diag(k)
  dim(x) <- NULL
  diag(x)
}
