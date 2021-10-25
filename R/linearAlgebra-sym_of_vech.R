#' Symmetric matrix A from vech(A)
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @inheritParams .sym_of_vech
#' @inherit .sym_of_vech description details references return
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
#' vechA <- c(1.0, 0.5, 0.4, 1.0, 0.6, 1.0)
#'
#' sym_of_vech(vechA)
#' @family Symmetric Functions
#' @keywords linearAlgebra symmetric
#' @export
sym_of_vech <- function(x) {
  k <- vech_check(x, return_k = TRUE)
  .sym_of_vech(
    x = x,
    k = k
  )
}
