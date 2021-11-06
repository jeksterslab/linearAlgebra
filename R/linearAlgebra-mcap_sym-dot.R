#' The Symmetric Pattern Matrix - Dot Function
#'
#' @inherit mcap_sym description details references return
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @param d Numeric matrix.
#'   Duplication matrix.
#'
#' @family Symmetric Functions
#' @keywords linearAlgebra symmetric dot
#' @noRd
.mcap_sym <- function(d) {
  d %*% tcrossprod(
    chol2inv(
      chol(
        crossprod(d)
      )
    ),
    d
  )
}
