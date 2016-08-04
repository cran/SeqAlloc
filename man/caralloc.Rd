\name{caralloc}
\alias{caralloc}
\title{
Sequential Allocation Using Covariate Adaptive Randomization
}
\description{
Performs the sequential allocation for the covariate-adjusted randomization (CAR) method of allocating observations in a randomized experiment. 
}
\usage{
caralloc(xmat, carwt, p, tol)
}
\arguments{
  \item{xmat}{
matrix or data frame of covariates for prospective enrollees in the experiment.
}
  \item{carwt}{
vector of weights
}
  \item{p}{
probability the next unit should be allocated to the experiment arm that currently has fewer observations. For CAR, use 0.5 < p < 1.
}
  \item{tol}{
tolerance for deviation from equal allocation. For CAR, set tol to be a small value, say 0.01. For CAIM, set tol to be the imbalance tolerance (d).
}
}
\value{
Vector with the allocation to treatment (denoted by 1) and control (denoted by 0)
}
\references{
Lohr, S. and X. Zhu (2015). Randomized Sequential Individual Assignment in Social Experiments: Evaluating the Design Options Prospectively. Sociological Methods and Research. [Advance online publication: December 27, 2015] doi: 10.1177/0049124115621332.

Pocock, S. J. and R. Simon (1975). Sequential Treatment Assignment with Balancing for Prognostic Factors in A Controlled Clinical Trial. Biometrics 31: 103-115.
}
\author{
Xiaoshu Zhu \email{xiaoshuzhu@westat.com} and Sharon Lohr
}
\examples{
sampsize <- 200
percent <- c(0.5,0.8,0.2,0.4)
carwt <- c(.4,.3,.2,.1)

set.seed(5798)

xmat <- matrix(rbinom(sampsize*length(percent),1,rep(percent,sampsize)),
              nrow=sampsize,ncol=length(percent),byrow=TRUE)
colnames(xmat) = c("C1","C2","C3","C4")
strat_factor = xmat[,1]*4 + xmat[,2]*2 + xmat[,4] + 1

caralloc(xmat,carwt,1,3)
}
\keyword{ Covariate-Adaptive Design }
\keyword{ Imbalance Tolerance }