\name{covimbal}
\alias{covimbal}
\title{
Evaluate covariate imbalance using R-squared.
}
\description{
Evaluate imbalance in the covariates from the allocation by finding R-squared for a centered regression through the origin.
}
\usage{
covimbal(y, xmatc)
}
\arguments{
  \item{y}{
vector of 0's and 1's representing allocation
}
  \item{xmatc}{
centered X matrix
}
}
\value{
Value of R-squared from centered regression
}
\references{
Lohr, S. and X. Zhu (2015). Randomized Sequential Individual Assignment in Social Experiments: Evaluating the Design Options Prospectively. Sociological Methods and Research. [Advance online publication: December 27, 2015] doi: 10.1177/0049124115621332.
}
\author{
Xiaoshu Zhu \email{xiaoshuzhu@westat.com} and Sharon Lohr
}
\examples{
alloc <- rbinom(60,1,.5)
xmat <- matrix(rbinom(240,1,.4),ncol=4)
xmatc <- scale(xmat,center=TRUE,scale=FALSE) 

covimbal(alloc,xmatc)
}
\keyword{ Allocation Imbalance }
