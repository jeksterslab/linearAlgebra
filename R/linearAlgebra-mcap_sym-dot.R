#' The Symmetric Pattern Matrix - Dot Function
#'
#' Creates a symmetric pattern matrix.
#'
#' \eqn{
#'     \mathbf{M}_{k}
#'     \left(
#'     s
#'     \right)
#' }
#' is the
#' \eqn{
#'     k \times k
#' }
#' symmetric pattern matrix with
#'
#' \deqn{
#'     \left(
#' 	   \mathbf{M}_{k}
#' 	   \left(
#' 	   s
#' 	   \right)
#' 	   \right)_{ij, gh}
#'     =
#' 	   \begin{cases}
#' 	       1
#' 		   &
#' 		   \text{if}
#' 		   \quad
#' 		   i = j = g = h
#' 	       ,
#' 		   \\
#' 		   \frac{1}{2}
#' 		   &
#' 		   \text{if}
#' 		   \quad
#' 		   \left( i, j \right) = \left( g, h \right)
#' 		   \text{or}
#' 		   \left( i, j \right) = \left( h, g \right)
#' 		   ,
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
#' @param d Numeric matrix.
#'   Duplication matrix.
#'
#' @inherit .mcap_diag references return
#'
#' @examples
#' .mcap_sym(3)
#' @family Symmetric Functions
#' @keywords linearAlgebra symmetric dot
#' @export
.mcap_sym <- function(d) {
  d %*% tcrossprod(
    solve(
      crossprod(d)
    ),
    d
  )
}
