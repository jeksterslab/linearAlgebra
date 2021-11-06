#' Sanity Checks for mu and sigmacap
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @param mu Numeric vector.
#'   Mean vector
#'   \eqn{\boldsymbol{\mu}}.
#' @param sigmacap Numeric matrix.
#'   Covariance matrix
#'   \eqn{\boldsymbol{\Sigma}}.
#' @export
#' @family Scaling Functions
#' @keywords linearAlgebra scaling check
#' @noRd
.check_mu_sigmacap <- function(mu,
                               sigmacap) {
  # sigmacap - symmetric matrix
  stopifnot(
    is.matrix(sigmacap),
    sigmacap == t(sigmacap)
  )
  # mu - vector
  stopifnot(
    is.vector(mu)
  )
  # sigmacap and mu dimensions
  stopifnot(
    dim(sigmacap)[1] == length(mu)
  )
}
