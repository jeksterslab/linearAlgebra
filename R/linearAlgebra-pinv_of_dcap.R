#' The Moore-Penrose Inverse of the Duplication Matrix
#'
#' @inheritParams .dcap
#' @inherit .pinv_of_dcap description details references return
#'
#' @examples
#' pinv_of_dcap(3)
#' @family Symmetric Functions
#' @keywords linearAlgebra symmetric
#' @export
pinv_of_dcap <- function(k) {
  k_check(k)
  .pinv_of_dcap(
    .dcap(k)
  )
}
