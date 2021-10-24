#' The Correlation Pattern Matrix
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @inheritParams .dcap
#' @inheritParams .mcap_cor
#' @inherit .mcap_cor description details references return
#'
#' @examples
#' mcap_cor(3)
#' @family Symmetric Functions
#' @keywords linearAlgebra symmetric
#' @export
mcap_cor <- function(k) {
  stopifnot(
    is.vector(k),
    length(k) == 1,
    k > 0
  )
  .mcap_cor(
    ms = .mcap_sym(
      .dcap(
        as.integer(k)
      )
    )
  )
}
