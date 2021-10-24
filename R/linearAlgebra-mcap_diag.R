#' The Diagonal Pattern Matrix
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @inheritParams .mcap_diag
#' @inherit .mcap_diag description details references return
#'
#' @examples
#' mcap_diag(3)
#' @family Symmetric Functions
#' @keywords linearAlgebra symmetric
#' @export
mcap_diag <- function(k) {
  stopifnot(
    is.vector(k),
    length(k) == 1,
    k > 0
  )
  .mcap_diag(
    as.integer(k)
  )
}
