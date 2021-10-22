#' The Correlation Pattern Matrix
#'
#' Creates a correlation pattern matrix.
#'
#' \eqn{
#'     \mathbf{M}_{k}
#'     \left(
#'     c
#'     \right)
#' }
#' is the
#' \eqn{
#'     k \times k
#' }
#' correlation pattern matrix with
#'
#' \deqn{
#'     \left(
#' 	   \mathbf{M}_{k}
#' 	   \left(
#' 	   c
#' 	   \right)
#' 	   \right)_{ij, gh}
#' 	   =
#' 	   \begin{cases}
#'         \frac{1}{2}
#' 		   &
#' 		   \text{if}
#' 		   \quad
#' 		   \left( i, j \right) = \left( g, h \right)
#' 		   \text{or}
#' 		   \left( i, j \right) = \left( h, g \right),
#' 		   \quad
#' 		   i \neq j
#' 	       ,
#' 		   g \neq h, \\
#' 		   0
#' 		   &
#' 		   \text{if}
#' 		   \quad
#' 		   i = j = g = h
#' 	       ,
#' 		   \\
#' 		   0
#' 		   &
#' 		   \text{otherwise}
#' 		   .
#'     \end{cases}
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
#'   [https://doi.org/10.1016/0024-3795(85)90191-0](https://doi.org/10.1016/0024-3795(85)90191-0)
#'
#' @returns A matrix.
#'
#' @examples
#' mcap_cor(3)
#' @export
#' @family Symmetric Functions
#' @keywords linearAlgebra symmetric
mcap_cor <- function(x) {
  output <- mcap_sym(x)
  output[output == 1] <- 0
  return(output)
}
