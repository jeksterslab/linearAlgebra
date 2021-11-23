#' Standarize (Matrix Input) - Dot Function
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @inherit z description details return
#' @param d Numeric matrix.
#'   Centered data.
#' @param scale Numeric vector.
#'   Scale.
#' @inheritParams .d
#'
#' @returns A matrix.
#'
#' @family Scaling Functions
#' @keywords linearAlgebra scaling dot
#' @noRd
.z <- function(d,
               scale,
               n,
               k) {
  d / rep(
    x = scale,
    times = rep(
      x = n,
      times = k
    )
  )
}

#' Standarize (Matrix Input) - Dot Function
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @inherit z description details return
#' @param d Numeric vector.
#'   Centered data.
#' @param scale Numeric vector of length 1.
#'   Scale.
#'
#' @returns A vector
#'
#' @family Scaling Functions
#' @keywords linearAlgebra scaling dot
#' @noRd
.z_vec <- function(d,
                   scale) {
  matrix(
    data = d / scale,
    ncol = 1
  )
}
