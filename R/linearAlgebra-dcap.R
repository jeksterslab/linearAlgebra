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
  stopifnot(
    is.vector(k),
    length(k) == 1,
    k > 0
  )
  .dcap(
    as.integer(k)
  )
}
