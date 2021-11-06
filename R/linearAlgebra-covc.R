#' Consistent Estimator of the Covariance Matrix
#'
#' Calculates the consistent estimate of the covariance matrix.
#'
#' Let
#' \eqn{\mathbf{X}}
#' be an \eqn{n \times k} data matrix.
#' The consistent estimator of the covariance matrix
#' \eqn{\mathbf{S}_{\mathrm{consistent}}}
#' of the data matrix
#' \eqn{X}
#' is given by
#'
#' \deqn{
#'     \mathbf{S}_{\mathrm{consistent}}
#'     =
#'     n^{-1}
#'     \mathbf{D}^{\prime}
#'     \mathbf{D}
#' }
#'
#' where \eqn{\mathbf{D}} is the matrix of deviation scores
#' given by
#'
#' \deqn{
#'     \mathbf{D}
#'     =
#'     \mathbf{X} - \mathbf{1}_{n} \bar{\mathbf{x}},
#' }
#'
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
#' An equivalent formula as a function of the unbiased estimator
#' is given by
#'
#' \deqn{
#'   \mathbf{S}_{\mathrm{consistent}}
#'   =
#'   \frac{n - 1}{n} \mathbf{S}_{\mathrm{unbiased}}
#' }
#'
#' where
#' \eqn{\mathbf{S}_{\mathrm{unbiased}}}
#' is given by
#'
#' \deqn{
#'   \mathbf{S}_{\mathrm{unbiased}}
#'   =
#'   \left(n - 1 \right)^{-1}
#'   \mathbf{D}^{\prime}
#'   \mathbf{D}
#' }.
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @param x Numeric matrix, data frame, or vector.
#'   Data matrix.
#'   If `x` is a vector
#'   it will be converted to an `n` by 1 matrix.
#'
#' @references
#'   [Wikipedia: Covariance](https://en.wikipedia.org/wiki/Covariance)
#'
#' @returns A matrix.
#'
#' @examples
#' x <- matrix(
#'   rnorm(n = 5 * 5, mean = 100, sd = 15),
#'   ncol = 5
#' )
#'
#' covc(x)
#'
#' x <- rnorm(5)
#'
#' covc(x)
#' @family Covariance Functions
#' @keywords linearAlgebra covariance
#' @export
covc <- function(x) {
  stopifnot(
    is.matrix(x) || is.data.frame(x) || is.vector(x)
  )
  if (is.vector(x)) {
    return(
      matrix(
        data = .covc(
          x = stats::var(x),
          n = length(x)
        ),
        ncol = 1
      )
    )
  }
  .covc(
    x = stats::cov(x),
    n = dim(x)[1]
  )
}
