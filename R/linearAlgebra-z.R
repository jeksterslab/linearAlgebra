#' Standarize
#'
#' Calculates standardized scores.
#'
#' The standardized score for the
#' \eqn{j^{\mathrm{th}}}
#' column of a matrix
#' \eqn{\mathbf{X}}
#' is given by
#'
#' \deqn{\frac{\mathbf{x}_{j} - \bar{x}_{j}}{s_{j}}}
#'
#' where
#' \eqn{\mathbf{x}_{j}}
#' is the
#' \eqn{j^{\mathrm{th}}}
#' column of a matrix
#' \eqn{\mathbf{X}},
#' \eqn{\bar{x}_{j}}
#' is the mean of
#' \eqn{\mathbf{x}_{j}},
#' and
#' \eqn{s_{j}}
#' is the standard deviation of
#' \eqn{\mathbf{x}_{j}}.
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @param x Numeric matrix or vector.
#'   Data.
#'
#' @returns A matrix.
#'
#' @examples
#' x <- matrix(
#'   rnorm(n = 5 * 5, mean = 100, sd = 15),
#'   ncol = 5
#' )
#'
#' z(x)
#' @family Scaling Functions
#' @keywords linearAlgebra scaling
#' @export
z <- function(x) {
  stopifnot(
    is.matrix(x) || is.data.frame(x) || is.vector(x)
  )
  if (is.vector(x)) {
    return(
      .z_vec(
        d = x - (
          sum(x) / length(x)
        ),
        scale = stats::sd(x)
      )
    )
  }
  dims <- dim(x)
  n <- dims[1]
  k <- dims[2]
  .z(
    d = .d(
      x = x,
      center = colMeans(x),
      n = n,
      k = k
    ),
    scale = sqrt(
      diag(
        stats::cov(x)
      )
    ),
    n = n,
    k = k
  )
}
