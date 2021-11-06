#' The Correlation Pattern Matrix - Dot Function
#'
#' @inherit mcap_cor description details references return
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @param ms Numeric matrix.
#'   Symmetric pattern matrix.
#'
#' @family Symmetric Functions
#' @keywords linearAlgebra symmetric dot
#' @noRd
.mcap_cor <- function(ms) {
  ms[ms == 1] <- 0
  ms
}
