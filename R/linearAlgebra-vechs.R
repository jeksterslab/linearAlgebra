#' Strict Half-Vectorize
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @inheritParams .vechs
#' @inheritParams vech
#' @inheritParams vechsnames
#'
#' @inherit .vechs description details references return
#'
#' @examples
#' A <- matrix(
#'   data = c(
#'     1.0, 0.5, 0.4,
#'     0.5, 1.0, 0.6,
#'     0.4, 0.6, 1.0
#'   ),
#'   nrow = 3
#' )
#'
#' vechs(A)
#' @family Vectorization Functions
#' @keywords linearAlgebra vectorization
#' @export
vechs <- function(x,
                  names = FALSE,
                  sep = ".") {
  stopifnot(
    is.matrix(x),
    dim(x)[1] == dim(2),
    dim(x)[1] > 1
  )
  output <- .vechs(x = x)
  if (names) {
    varnames <- colnames(x)
    if (is.null(varnames)) {
      varnames <- paste0(seq_len(dim(x)[2]))
    }
    varnames <- vechsnames(varnames, sep = sep)
    names(output) <- varnames
  }
  return(output)
}
