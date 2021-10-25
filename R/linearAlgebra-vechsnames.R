#' Vector Names for Strict Half-Vectorization
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @inheritParams .vechsnames
#' @inherit .vechsnames return
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
vechsnames <- function(x,
                       sep = ".") {
  stopifnot(
    is.vector(x),
    length(sep) == 1
  )
  .vechsnames(
    x = x,
    sep = sep
  )
}
