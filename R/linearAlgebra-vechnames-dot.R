#' Vector Names for Half-Vectorization - Dot Function
#'
#' Returns a character vector of length `0.5 * k * (k + 1)`
#' from an input vector `x` of length `k`.
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @param x Character vector of names of length `k`.
#' @param sep Character string.
#'   Separator for variable names.
#'
#' @returns A character vector.
#'
#' @examples
#' x <- diag(1)
#' colnames(x) <- rownames(x) <- "x1"
#' x
#' vechnames(colnames(x))
#'
#' x <- diag(2)
#' colnames(x) <- rownames(x) <- c("x1", "x2")
#' x
#' vechnames(colnames(x))
#'
#' x <- diag(3)
#' colnames(x) <- rownames(x) <- c("x1", "x2", "x3")
#' x
#' vechnames(colnames(x))
#' @family Vectorization Functions
#' @keywords linearAlgebra vectorization
#' @export
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
