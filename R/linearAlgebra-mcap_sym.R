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
  k_check(k)
  .mcap_sym(
    .dcap(k)
  )
}
