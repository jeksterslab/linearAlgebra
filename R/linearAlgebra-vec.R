#' Vectorize
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @inheritParams .vec
#'
#' @inherit .vec description details references return
#'
#' @examples
#' A <- matrix(
#'   data = c(
#'     1.0, 0.5, 0.4,
#'     0.5, 1.0, 0.6,
#'     0.4, 0.6, 1.0
#'   ),
#'   nrow = 3
#' )
#'
#' vec(A)
#' @family Vectorization Functions
#' @keywords linearAlgebra vectorization
#' @export
vec <- function(x) {
  stopifnot(
    is.matrix(x)
  )
  .vec(x = x)
}
