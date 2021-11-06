#' Squared Mahalanobis Distance
#'
#' Calculates the squared Mahalanobis distance.
#'
#' The squared Mahalanobis distance is given by
#' \deqn{
#'     \boldsymbol{\Delta}^{2}
#'     =
#'     \left( \mathbf{x} - \boldsymbol{\mu} \right)^{\prime}
#'     \boldsymbol{\Sigma}^{-1}
#'     \left( \mathbf{x} - \boldsymbol{\mu} \right)
#' }
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @param x Numeric vector or matrix.
#' @param mu Numeric vector.
#'   Center
#'   \eqn{\boldsymbol{\mu}}.
#' @param sigmacap Numeric matrix.
#'   Covariance matrix
#'   \eqn{\boldsymbol{\Sigma}}.
#'
#' @references
#'   [Wikipedia: Mahalanobis distance](https://en.wikipedia.org/wiki/Mahalanobis_distance)
#'
#' @returns A vector.
#'
#' @examples
#' x <- matrix(
#'   data = stats::rnorm(n = 5 * 2),
#'   ncol = 2
#' )
#'
#' deltacapsq(
#'   x,
#'   mu = c(0, 0),
#'   sigmacap = diag(2)
#' )
#'
#' x <- rnorm(5)
#'
#' deltacapsq(
#'   x,
#'   mu = 0,
#'   sigmacap = 1
#' )
#' @family Scaling Functions
#' @keywords linearAlgebra scaling
#' @export
deltacapsq <- function(x,
                       mu,
                       sigmacap) {
  stopifnot(
    is.matrix(x) || is.vector(x)
  )
  if (is.vector(x)) {
    mu <- .vec(mu)
    sigmacap <- .vec(
      sigmacap
    )
    stopifnot(
      length(mu) == 1,
      length(sigmacap) == 1
    )
    return(
      .deltacapsq_vec(
        d = x - mu,
        qcap = 1 / sigmacap
      )
    )
  }
  .check_mu_sigmacap(
    mu = mu,
    sigmacap = sigmacap
  )
  dims <- dim(x)
  n <- dims[1]
  k <- dims[2]
  .deltacapsq(
    d = .d(
      x = x,
      center = mu,
      n = n,
      k = k
    ),
    qcap = chol2inv(
      chol(
        sigmacap
      )
    )
  )
}
