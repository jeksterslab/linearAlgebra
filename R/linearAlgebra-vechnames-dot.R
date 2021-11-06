#' Vector Names for Half-Vectorization - Dot Function
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @inherit vechnames description return
#' @inheritParams vechnames
#'
#' @family Vectorization Functions
#' @keywords linearAlgebra vectorization
#' @noRd
.vechnames <- function(x,
                       sep = ".") {
  sym <- outer(
    X = x,
    Y = x,
    FUN = function(x, y) {
      paste0(x, sep, y)
    }
  )
  .vech(sym)
}
