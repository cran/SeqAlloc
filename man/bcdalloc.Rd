\name{bcdalloc}
\alias{bcdalloc}
\title{
Biased Coin Design Allocation
}
\description{
Allocates observations sequentially using a biased coin design. If equal numbers have been allocated to treatment and control, the next observation is allocated to treatment with probability 1/2. If there are more units assigned to treatment than control, the next observation is allocated to control with probability p; if there are more units assigned to control than treatment, the next observation is allocated to treatment with probability p.
}
\usage{
bcdalloc(n, pbcd, tol = 1e-06)
}
\arguments{
  \item{n}{
number of observations
}
  \item{pbcd}{
probability p used for biased coin
}
  \item{tol}{
tolerance used for allocation
}
}
\value{
Vector of zeroes and ones indicating the allocation of each unit to treatment (1) or control (0)
}
\references{
Efron, B. (1971). Forcing a sequential experiment to be balanced. Biometrika, 58(3), 403-417.

Lohr, S. and X. Zhu (2015). Randomized Sequential Individual Assignment in Social Experiments: Evaluating the Design Options Prospectively. Sociological Methods and Research. [Advance online publication: December 27, 2015] doi: 10.1177/0049124115621332.
}
\author{
Xiaoshu Zhu \email{xiaoshuzhu@westat.com} and Sharon Lohr
}
\examples{
bcdalloc(20,0.7)
}
\keyword{ Biased Coin Design }