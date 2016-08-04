\name{stratblockalloc}
\alias{stratblockalloc}
\title{
Stratified permuted block allocation
}
\description{
Allocates observations to treatment (1) or control (0) within strata
}
\usage{
stratblockalloc(n, blksize)
}
\arguments{
  \item{n}{
number of observations
}
  \item{blksize}{
size of each block: must be even number
}
}
\details{
Allocates observations within each block of size blksize so that half of the observations in each block are in the treatment group and the other half are in the control group.
}
\value{
vector of allocations to treatment (1) or control (0) group
}
\references{
Lohr, S. and X. Zhu (2015). Randomized Sequential Individual Assignment in Social Experiments: Evaluating the Design Options Prospectively. Sociological Methods and Research. [Advance online publication: December 27, 2015] doi: 10.1177/0049124115621332.
}
\author{
Xiaoshu Zhu \email{xiaoshuzhu@westat.com} and Sharon Lohr
}
\examples{
stratblockalloc(20,2)
}
\keyword{ Stratified Permuted Block Design }
