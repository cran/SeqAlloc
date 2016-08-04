\name{weight_x}
\alias{weight_x}
\title{
Calculate weight matrix from matrix of covariates
}
\description{
Calculate a vector of weights relating vector xrow with the rows of xmat, according to the weights in vector carwt. For each row in xmat, the positions of agreement with xrow are found. The weight for that row of xmat is then the sum of the elements of carwt corresponding to the matches.
}
\usage{
weight_x(xrow, xmat, carwt)
}
\arguments{
  \item{xrow}{
vector used for matching
}
  \item{xmat}{
matrix of covariates
}
  \item{carwt}{
vector of weight for each match
}
}
\value{
Vector of weights relating vector xrow to the rows of xmat
}
\references{
Lohr, S. and X. Zhu (2015). Randomized Sequential Individual Assignment in Social Experiments: Evaluating the Design Options Prospectively. Sociological Methods and Research. [Advance online publication: December 27, 2015] doi: 10.1177/0049124115621332.
}
\author{
Xiaoshu Zhu \email{xiaoshuzhu@westat.com} and Sharon Lohr
}
\examples{
xrow <- c(1,0,1,1)
xmat <- matrix(rbinom(60,1,.4),ncol=4)
carwt <- c(.6,.4,.2,.1)

weight_x(xrow, xmat, carwt)
}
