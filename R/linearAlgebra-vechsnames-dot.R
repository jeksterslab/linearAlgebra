#' Vector Names for Strict Half-Vectorization - Dot Function
#'
#' Returns a character vector of length `0.5 * k * (k + 1) - k`
#' from an input vector `x` of length `k`.
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @inheritParams .vechnames
#' @inherit .vechnames return
#'
#' @examples
#' x <- diag(2)
#' colnames(x) <- rownames(x) <- c("x1", "x2")
#' vechsnames(colnames(x))
#'
#' x <- diag(3)
#' colnames(x) <- rownames(x) <- c("x1", "x2", "x3")
#' vechsnames(colnames(x))
#' @family Vectorization Functions
#' @keywords linearAlgebra vectorization
#' @export
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
