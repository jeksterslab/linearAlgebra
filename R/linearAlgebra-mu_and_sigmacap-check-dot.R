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
#' @param return_k Logical.
#'   Return valid `k`.
#' @family Scaling Functions
#' @keywords linearAlgebra scaling check
#' @noRd
.check_mu_and_sigmacap <- function(mu,
                                   sigmacap,
                                   return_k = FALSE) {
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
  k <- dim(sigmacap)[1]
  stopifnot(
    dim(sigmacap)[1] == length(mu)
  )
  if (return_k) {
    return(k)
  }
}
