#' The Moore-Penrose Inverse of the Duplication Matrix - Dot Function
#'
#' @inheritParams dcap
#' @inherit pinv_of_dcap description details references return
#'
#' @param d Numeric matrix.
#'   Duplication matrix.
#'
#' @family Symmetric Functions
#' @keywords linearAlgebra symmetric dot
#' @noRd
.pinv_of_dcap <- function(d) {
  tcrossprod(
    chol2inv(
      chol(
        crossprod(d)
      )
    ),
    d
  )
}
