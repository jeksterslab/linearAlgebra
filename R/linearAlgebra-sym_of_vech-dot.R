#' Symmetric matrix A from vech(A) - Dot Function
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @inherit sym_of_vech description details references return
#' @inheritParams sym_of_vech
#' @inheritParams dcap
#'
#' @family Symmetric Functions
#' @keywords linearAlgebra symmetric dot
#' @noRd
.sym_of_vech <- function(x,
                         k) {
  sym <- matrix(
    data = 0,
    nrow = k,
    ncol = k
  )
  sym[lower.tri(sym, diag = TRUE)] <- x
  sym[upper.tri(sym)] <- t(sym)[upper.tri(sym)]
  sym
}
