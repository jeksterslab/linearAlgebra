#' The Correlation Pattern Matrix - Dot Function
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
#' @param ms Numeric matrix.
#'   Symmetric pattern matrix.
#'
#' @inherit .mcap_diag references return
#'
#' @examples
#' .mcap_cor(.mcap_sym(3))
#' @family Symmetric Functions
#' @keywords linearAlgebra symmetric dot
#' @export
.mcap_cor <- function(ms) {
  ms[ms == 1] <- 0
  ms
}
