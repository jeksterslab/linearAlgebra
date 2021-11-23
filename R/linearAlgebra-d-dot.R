#' Deviation from the Mean (Matrix or Data Frame Input) - Dot Function
#'
#' @inherit d description details return
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @param x Numeric matrix.
#'   Data matrix.
#' @param center Numeric vector.
#'   Center.
#' @param n Positive integer.
#'   Number of rows in the data matrix `x`.
#' @param k Positive integer.
#'   Number of columns in the data matrix `x`.
#'
#' @returns A matrix.
#'
#' @family Scaling Functions
#' @keywords linearAlgebra scaling dot
#' @noRd
.d <- function(x,
               center,
               n,
               k) {
  x - rep(
    x = center,
    times = rep(
      x = n,
      times = k
    )
  )
}

#' Deviation from the Mean (Vector Input) - Dot Function
#'
#' @inherit d description details return
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @param x Numeric vector.
#'   Data vector.
#' @param center Numeric vector of length 1.
#'   Center.
#'
#' @returns A vector.
#'
#' @family Scaling Functions
#' @keywords linearAlgebra scaling dot
#' @noRd
.d_vec <- function(x,
                   center) {
  matrix(
    data = x - center,
    ncol = 1
  )
}
