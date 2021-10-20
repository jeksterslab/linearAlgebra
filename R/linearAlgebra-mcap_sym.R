#' Symmetric Pattern Matrix
#'
#' Create a symmetric pattern matrix.
#'
#' \eqn{
#' \mathbf{M}_{k}
#' \left(
#' s
#' \right)
#' }
#' is the
#' \eqn{
#' k \times k
#' }
#' symmetric pattern matrix with
#'
#' \deqn{
#' 	\left(
#' 	\mathbf{M}_{k}
#' 	\left(
#' 	s
#' 	\right)
#' 	\right)_{ij, gh}
#' 	=
#' 	\begin{cases}
#' 		1
#' 		  &
#' 		\text{if}
#' 		\quad
#' 		i = j = g = h
#' 		,
#' 		\\
#' 		\frac{1}{2}
#' 		  &
#' 		\text{if}
#' 		\quad
#' 		\left( i, j \right) = \left( g, h \right)
#' 		\text{or}
#' 		\left( i, j \right) = \left( h, g \right)
#' 		,
#' 		\\
#' 		0
#' 		  &
#' 		\text{otherwise}
#' 		.
#' 	\end{cases}
#' }
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @param x Integer.
#'   Dimension of the symmetric matrix.
#'
#' @references
#'   Nel, D. G. (1985).
#'   A matrix derivation of the asymptotic covariance matrix of sample correlation coefficients.
#'   Linear Algebra and its Applications,
#'   67, 137--145.
#'   https://doi.org/10.1016/0024-3795(85)90191-0
#'
#' @returns A matrix.
#'
#' @examples
#' mcap_sym(3)
#' @importFrom MASS ginv
#' @export
#' @family Linear Algebra Functions
#' @keywords linearAlgebra symmetric
mcap_sym <- function(x) {
  kcap <- MASS::ginv(dcap(x))
  return(
    MASS::ginv(
      X = kcap
    ) %*% kcap
  )
}
