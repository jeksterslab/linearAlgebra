#' Vector Names for Strict Half-Vectorization - Dot Function
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @inherit vechsnames description return
#' @inheritParams vechsnames
#'
#' @family Vectorization Functions
#' @keywords linearAlgebra vectorization dot
#' @noRd
.vechsnames <- function(x,
                        sep = ".") {
  sym <- outer(
    X = x,
    Y = x,
    FUN = function(x, y) {
      paste0(x, sep, y)
    }
  )
  .vechs(sym)
}
