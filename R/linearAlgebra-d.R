#' Deviation from the Mean
#'
#' Calculates the deviation from the mean.
#'
#' The
#' \eqn{n \times 1}
#' vector of deviations from the mean
#' is given by
#'
#' \deqn{
#'     \mathbf{d}
#'     =
#'     \mathbf{x} - \bar{x}
#' }
#'
#' where
#' \eqn{\mathbf{x}}
#' is an
#' \eqn{n \times 1}
#' column vector
#' and
#' \eqn{\bar{x}}
#' is the mean of
#' \eqn{\mathbf{x}}.
#'
#' The
#' \eqn{n \times k}
#' matrix of deviations from the mean
#' is given by
#'
#' \deqn{
#'     \mathbf{D}
#'     =
#'     \mathbf{X} - \mathbf{1}_{n} \bar{\mathbf{x}}
#' }
#'
#' where
#' \eqn{\mathbf{X}}
#' is an
#' \eqn{n \times 1}
#' matrix,
#' \eqn{\mathbf{1}_{n}}
#' is an
#' \eqn{n \times 1}
#' column vector of ones,
#' and
#' \eqn{\bar{\mathbf{x}}}
#' is the
#' \eqn{k \times 1}
#' mean vector
#' of
#' \eqn{\mathbf{X}}.
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @param x Numeric matrix, data frame, or vector.
#'   Data matrix.
#'
#' @returns A matrix.
#'
#' @examples
#' x <- matrix(
#'   rnorm(n = 5 * 5, mean = 100, sd = 15),
#'   ncol = 5
#' )
#'
#' d(x)
#'
#' x <- rnorm(5)
#'
#' d(x)
#' @family Scaling Functions
#' @keywords linearAlgebra scaling
#' @export
d <- function(x) {
  stopifnot(
    is.matrix(x) || is.data.frame(x) || is.vector(x)
  )
  if (is.vector(x)) {
    return(
      .d_vec(
        x = x,
        center = sum(x) / length(x)
      )
    )
  }
  dims <- dim(x)
  .d(
    x = x,
    center = colMeans(x),
    n = dims[1],
    k = dims[2]
  )
}
