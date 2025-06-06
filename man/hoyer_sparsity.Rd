% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cosim_itens_scales.R
\name{hoyer_sparsity}
\alias{hoyer_sparsity}
\title{Hoyer's Sparsity Measure}
\usage{
hoyer_sparsity(x)
}
\arguments{
\item{x}{A numeric vector.}
}
\value{
A numeric value between 0 and 1 representing the sparsity.
Returns 0 if the \eqn{L_2} norm of \code{x} is zero to avoid division by zero.
}
\description{
Computes Hoyer's sparsity metric, which quantifies the level of sparsity in a numeric vector.
The value ranges from 0 (completely dense) to 1 (maximally sparse).
}
\details{
Hoyer's sparsity is defined as:
\deqn{ \frac{\sqrt{n} - \frac{||x||_1}{||x||_2}}{\sqrt{n} - 1} }
where \eqn{n} is the length of the vector, \eqn{||x||_1} is the L1 norm,
and \eqn{||x||_2} is the L2 norm.
}
\examples{
hoyer_sparsity(c(1, 0, 0, 0))
hoyer_sparsity(rep(1, 4))

}
