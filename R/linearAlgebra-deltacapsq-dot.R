#' Squared Mahalanobis Distance (Matrix Input) - Dot Function
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @param d Numeric matrix.
#'   Centered data.
#' @param qcap Numeric matrix.
#'   Inverse of the scaling matrix
#'   \eqn{\mathbf{Q} = \boldsymbol{\Sigma}^{-1}}.
#'
#' @returns A vector.
#'
#' @family Scaling Functions
#' @keywords linearAlgebra scaling dot
#' @noRd
.deltacapsq <- function(d,
                        qcap) {
  rowSums(
    d %*% qcap * d
  )
}

#' Squared Mahalanobis Distance (Vector Input) - Dot Function
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @param d Numeric vector.
#'   Centered data.
#' @param qcap Numeric vector of length 1.
#'   Inverse of the scaling matrix
#'   \eqn{\mathbf{Q} = \boldsymbol{\Sigma}^{-1}}.
#'
#' @returns A vector.
#'
#' @family Scaling Functions
#' @keywords linearAlgebra scaling dot
#' @noRd
.deltacapsq_vec <- function(d,
                            qcap) {
  d^2 * qcap
}
