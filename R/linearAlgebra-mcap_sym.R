#' The Symmetric Pattern Matrix
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @inherit .mcap_sym description details references return
#' @inheritParams .dcap
#'
#' @examples
#' mcap_sym(3)
#' @family Symmetric Functions
#' @keywords linearAlgebra symmetric
#' @export
mcap_sym <- function(k) {
  stopifnot(
    is.vector(k),
    length(k) == 1,
    k > 0
  )
  .mcap_sym(
    .dcap(
      as.integer(k)
    )
  )
}
