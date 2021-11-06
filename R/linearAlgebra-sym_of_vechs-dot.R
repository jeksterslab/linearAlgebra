#' Symmetric matrix A from vechs(A) - Dot Function
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @inherit sym_of_vechs description details references return
#' @inheritParams sym_of_vechs
#' @inheritParams dcap
#'
#' @family Symmetric Functions
#' @keywords linearAlgebra symmetric dot
#' @noRd
.sym_of_vechs <- function(x,
                          k,
                          diags) {
  sym <- matrix(
    data = 0,
    nrow = k,
    ncol = k
  )
  sym[lower.tri(sym, diag = FALSE)] <- x
  sym[upper.tri(sym)] <- t(sym)[upper.tri(sym)]
  diag(sym) <- diags
  sym
}
