#' Vector Mean (Vector Input) - Dot Function
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @inherit vec_mean details return
#'
#' @param x Numeric vector.
#' @family Vectorization Functions
#' @keywords linearAlgebra operation dot
#' @noRd
.vec_mean <- function(x) {
  sum(x) / length(x)
}

#' Vector Mean (Array Input) - Dot Function
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @inherit vec_mean details return
#'
#' @param x Numeric array.
#' @family Vectorization Functions
#' @keywords linearAlgebra operation dot
#' @noRd
.vec_mean_array <- function(x) {
  dim(x) <- NULL
  sum(x) / length(x)
}
