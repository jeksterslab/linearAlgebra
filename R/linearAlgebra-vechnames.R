#' Vector Names for Half-Vectorization
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @inheritParams .vechnames
#' @inherit .vechnames description return
#'
#' @examples
#' x <- diag(1)
#' colnames(x) <- rownames(x) <- "x1"
#' vechnames(colnames(x))
#'
#' x <- diag(2)
#' colnames(x) <- rownames(x) <- c("x1", "x2")
#' vechnames(colnames(x))
#'
#' x <- diag(3)
#' colnames(x) <- rownames(x) <- c("x1", "x2", "x3")
#' vechnames(colnames(x))
#' @family Vectorization Functions
#' @keywords linearAlgebra vectorization
#' @export
vechnames <- function(x,
                      sep = ".") {
  stopifnot(
    is.vector(x),
    length(sep) == 1
  )
  .vechnames(
    x = x,
    sep = sep
  )
}
