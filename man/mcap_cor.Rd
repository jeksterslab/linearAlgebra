% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/linearAlgebra-mcap_cor.R
\name{mcap_cor}
\alias{mcap_cor}
\title{The Correlation Pattern Matrix}
\usage{
mcap_cor(k)
}
\arguments{
\item{k}{Positive integer.
Dimension of the \code{k} by \code{k} matrix.}
}
\value{
A matrix.
}
\description{
Creates a correlation pattern matrix.
}
\details{
\eqn{
    \mathbf{M}_{k}
    \left(
    c
    \right)
}
is the
\eqn{
    k \times k
}
correlation pattern matrix with

\deqn{
    \left(
	   \mathbf{M}_{k}
	   \left(
	   c
	   \right)
	   \right)_{ij, gh}
	   =
	   \begin{cases}
        \frac{1}{2}
		   &
		   \text{if}
		   \quad
		   \left( i, j \right) = \left( g, h \right)
		   \text{or}
		   \left( i, j \right) = \left( h, g \right),
		   \quad
		   i \neq j
	       ,
		   g \neq h, \\
		   0
		   &
		   \text{if}
		   \quad
		   i = j = g = h
	       ,
		   \\
		   0
		   &
		   \text{otherwise}
		   .
    \end{cases}
}
}
\examples{
mcap_cor(3)
}
\references{
Nel, D. G. (1985).
A matrix derivation of the asymptotic covariance matrix of sample correlation coefficients.
Linear Algebra and its Applications,
67, 137--145.
https://doi.org/10.1016/0024-3795(85)90191-0
}
\seealso{
Other Symmetric Functions: 
\code{\link{dcap}()},
\code{\link{mcap_diag}()},
\code{\link{mcap_sym}()},
\code{\link{pinv_of_dcap}()},
\code{\link{sym_of_vechs}()},
\code{\link{sym_of_vech}()}
}
\author{
Ivan Jacob Agaloos Pesigan
}
\concept{Symmetric Functions}
\keyword{linearAlgebra}
\keyword{symmetric}
