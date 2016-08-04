\name{WAICimbal}
\alias{WAICimbal}
\title{
Weighted Average of Allocation Imbalance for Covariates
}
\description{
Evaluate weighted average of the allocation imbalance for all covariates
}
\usage{
WAICimbal(alloc, xmat, carwt)
}
\arguments{
  \item{alloc}{
vector of allocations to treatment (1) or control (0)
}
  \item{xmat}{
matrix describing covariates for each prospective observation
}
  \item{carwt}{
vector of weights to be accorded to respective covariates
}
}
\value{
Weighted average of covariate imbalances
}
\references{
Lohr, S. and X. Zhu (2015). Randomized Sequential Individual Assignment in Social Experiments: Evaluating the Design Options Prospectively. Sociological Methods and Research. [Advance online publication: December 27, 2015] doi: 10.1177/0049124115621332.
}
\author{
Xiaoshu Zhu \email{xiaoshuzhu@westat.com} and Sharon Lohr
}
\seealso{
  \code{\link{MAICimbal}}
}
\examples{
alloc <- rbinom(60,1,.5)
xmat <- matrix(rbinom(240,1,.4),ncol=4)
carwt <- c(.1,.3,.2,.4)

WAICimbal(alloc, xmat, carwt) 
}
\keyword{ Allocation Imbalance }