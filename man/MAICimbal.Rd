\name{MAICimbal}
\alias{MAICimbal}
\title{
Maximum Allocation Imbalance for Covariates
}
\description{
Evaluate maximum of the allocation imbalance for all covariates
}
\usage{
MAICimbal(alloc, xmat)
}
\arguments{
  \item{alloc}{
vector of allocations to treatment (1) or control (0)
}
  \item{xmat}{
matrix describing covariates for each prospective observation
}
}
\value{
maximum of covariate imbalances
}
\references{
Lohr, S. and X. Zhu (2015). Randomized Sequential Individual Assignment in Social Experiments: Evaluating the Design Options Prospectively. Sociological Methods and Research. [Advance online publication: December 27, 2015] doi: 10.1177/0049124115621332.
}
\author{
Xiaoshu Zhu \email{xiaoshuzhu@westat.com} and Sharon Lohr
}
\seealso{
  \code{\link{WAICimbal}}
}
\examples{
alloc <- rbinom(60,1,.5)
xmat <- matrix(rbinom(240,1,.4),ncol=4)

MAICimbal(alloc, xmat) 
}
\keyword{ Allocation Imbalance }