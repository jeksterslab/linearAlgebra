#' Diagonals of A from vech(A)
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @inheritParams .diag_of_vech
#' @inherit .diag_of_vech description details references return
#'
#' @examples
#' A <- matrix(
#'   data = c(
#'     1.0, 0.5, 0.4,
#'     0.5, 1.0, 0.6,
#'     0.4, 0.6, 1.0
#'   ),
#'   ncol = 3
#' )
#' vechA <- c(1.0, 0.5, 0.4, 1.0, 0.6, 1.0)
#'
#' diag_of_vech(vechA, loc = FALSE)
#' diag_of_vech(vechA, loc = TRUE)
#' @family Vectorization Functions
#' @keywords linearAlgebra vectorization
#' @export
diag_of_vech <- function(x,
                         loc = FALSE) {
  stopifnot(
    is.vector(x),
    is.logical(loc)
  )
  k <- 0.5 * (
    sqrt(
      1 + 8 * length(x)
    ) - 1
  )
  if (k %% 1 != 0) {
    stop("Length of \"x\" is not valid.")
  }
  .diag_of_vech(
    x,
    k = k,
    loc = loc
  )
}
