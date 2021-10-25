#' The Duplication Matrix
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @inheritParams .dcap
#' @inherit .dcap description details references return
#'
#' @examples
#' dcap(3)
#' @family Symmetric Functions
#' @keywords linearAlgebra symmetric
#' @export
dcap <- function(k) {
  k_check(k)
  .dcap(k)
}
