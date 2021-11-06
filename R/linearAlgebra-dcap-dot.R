#' The Duplication Matrix - Dot Function
#'
#' @inherit dcap description details references return
#' @inheritParams dcap
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @family Symmetric Functions
#' @keywords linearAlgebra symmetric dot
#' @noRd
.dcap <- function(k) {
  sym <- matrix(
    0,
    nrow = k,
    ncol = k
  )
  q <- seq_len(
    0.5 * k * (k + 1)
  )
  sym[lower.tri(sym, diag = TRUE)] <- q
  sym[upper.tri(sym)] <- t(sym)[upper.tri(sym)]
  outer(
    X = .vec(sym),
    Y = q,
    FUN = function(x, y) {
      ifelse(
        test = x == y,
        yes = 1,
        no = 0
      )
    }
  )
}
