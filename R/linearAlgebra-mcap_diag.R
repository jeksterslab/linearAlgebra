#' Diagonal Pattern Matrix
#'
#' Create a diagonal pattern matrix.
#'
#' \eqn{
#' \mathbf{M}_{k}
#' \left(
#' d
#' \right)
#' }
#' is the
#' \eqn{
#' k \times k
#' }
#' diagonal pattern matrix with
#'
#' \deqn{
#' 	\left(
#' 	\mathbf{M}_{k}
#' 	\left(
#' 	d
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
#' mcap_diag(3)
#' @export
#' @family Linear Algebra Functions
#' @keywords linearAlgebra symmetric
mcap_diag <- function(x) {
  return(
    diag(
      as.vector(
        diag(x)
      )
    )
  )
}
