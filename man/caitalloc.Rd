\name{caitalloc}
\alias{caitalloc}
\title{
Sequential Allocation for CAIT Method
}
\description{
Performs the sequential allocation for the covariate-adjusted-imbalance-tolerance (CAIT) method of allocating observations in a randomized experiment.
}
\usage{
caitalloc(carwt_x1, p, tol)
}
\arguments{
  \item{carwt_x1}{
matrix of dimension n x n, where n is the number of units to be randomly assigned to treatments. The (i,j) element of the matrix is the weighted match score of unit i and unit j. The matrix should be symmetric with diagonal elements of 1.
}
  \item{p}{
probability the next unit should be allocated to the experiment arm that currently has fewer observations. 
}
  \item{tol}{
imbalance tolerance for deviation from equal allocation. 
}
}
\value{
Vector with the allocation to treatment (denoted by 1) and control (denoted by 0)
}
\references{
Pocock, S.J. and R. Simon (1975). Sequential Treatment Assignment with Balancing for Prognostic Factors in a Controlled Clinical Trial. Biometrics 31, 103-115.

Lohr, S. and X. Zhu (2015). Randomized Sequential Individual Assignment in Social Experiments: Evaluating the Design Options Prospectively. Sociological Methods and Research. [Advance online publication: December 27, 2015] doi: 10.1177/0049124115621332.
}
\author{
Xiaoshu Zhu \email{xiaoshuzhu@westat.com} and Sharon Lohr
}
\note{
This function works for categorical as well as binary covariates. The matrix carwt_x1 can be created from an X matrix by using the function weight_x in this package.
}
\seealso{
  \code{\link{weight_x}}
}
\examples{
wtmat <- matrix(runif(10000), ncol=100) 
wtmat <- (wtmat * lower.tri(wtmat)) + t(wtmat * lower.tri(wtmat)) 
diag(wtmat) <- 1 

caitalloc(wtmat,1,3)
}
\keyword{ Covariate-Adaptive Design }
\keyword{ Imbalance Tolerance }
