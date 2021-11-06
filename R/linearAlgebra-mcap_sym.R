#' The Symmetric Pattern Matrix
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
#' @inherit mcap_diag references return
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @inheritParams dcap
#'
#' @examples
#' mcap_sym(3)
#' @family Symmetric Functions
#' @keywords linearAlgebra symmetric
#' @export
mcap_sym <- function(k) {
  .check_pos_scalar_int(k)
  .mcap_sym(
    .dcap(k)
  )
}
