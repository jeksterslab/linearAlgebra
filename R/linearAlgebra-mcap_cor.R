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
#' @inherit mcap_diag references return
#'
#' @inheritParams dcap
#'
#' @examples
#' mcap_cor(3)
#' @family Symmetric Functions
#' @keywords linearAlgebra symmetric
#' @export
mcap_cor <- function(k) {
  .check_pos_scalar_int(k)
  .mcap_cor(
    ms = .mcap_sym(
      .dcap(k)
    )
  )
}
