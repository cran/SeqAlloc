\name{SeqAlloc}
\alias{SeqAlloc}
\title{
Sequential Allocation for Prospective Experiments
}
\description{
Simulates results of allocations using complete randomization (CR), random allocation rule (RAR), biased coin design (BCD), permuted block design (PBD), stratified permuted block design (SPBD), covariate-adaptive randomization (CAR), big stick design (BSD), and covariate-adjusted imbalance tolerance (CAIT) designs. The order of the prospective enrollees is permuted for a preset number of iterations; for each iteration, the allocations are determined for each of the methods listed above. The allocations are then evaluated for balance on the covariates and for predictability (i.e., how well an observer could guess the next treatment assignment).
}
\usage{
SeqAlloc(xmat, carwt, strata = NULL, blksize, pbcd, pcar, bsdtol, caittol, 
         niter, seed = 12345)
}
\arguments{
  \item{xmat}{
matrix or data frame of covariates for prospective enrollees in the experiment. This matrix is to be used in CAR/CAIT methods, and should include strata or marginals of strata as columns
}
  \item{carwt}{
vector of weights to be used for CAR and CAIT methods
}
  \item{strata}{
vector of planned strata for study (if none, should be NULL)
}
  \item{blksize}{
vector of block sizes for PBDs and SPBDs
}
  \item{pbcd}{
probability for biased coin design (BCD) method
}
  \item{pcar}{
probability for CAR method
}
  \item{bsdtol}{
tolerance (d value) for BSD method
}
  \item{caittol}{
tolerance (d value) for CAIM method
}
  \item{niter}{
number of iterations for simulation
}
  \item{seed}{
random number seed, allows the allocation to be reproduced later
}
}
\value{
List containing summary statistics (minimum, 25th percentile, median, mean, 75th percentile, 90th percentile, 95th percentile, maximum) for evaluation measures, including AI, Rsquared, MAIC, WAIC, perccorr, and perccorr_strat.
  \item{schemes}{names of schemes evaluated}
  \item{AI}{value of overall allocation imblance defined as | Proportion of observations that are allocated to the treatment group - 0.5 |}
  \item{Rsquared}{value of R-squared from regression through the origin}
  \item{MAIC}{maximum of allocation imbalance for all covariates}
  \item{WAIC}{weighted average of allocation imbalance for all covariates}
  \item{perccorr}{percentage of allocations that an observer could guess correctly using the Blackwell-Hodges rule}
  \item{perccorr_strat}{percentage of allocations that an observer could guess correctly using the Blackwell-Hodges rule within each stratum}
  \item{carwt}{weights used in CAR and CAIM procedures}
}
\references{
Blackwell, David and J. L. Hodges (1957). Design for the Control of Selection Bias. Annals of Mathematical Statistics 28: 449-460.

Lohr, S. and X. Zhu (2015). Randomized Sequential Individual Assignment in Social Experiments: Evaluating the Design Options Prospectively. Sociological Methods and Research. [Advance online publication: December 27, 2015] doi: 10.1177/0049124115621332.

Rosenberger, W. F. and Lachin, J. M. (2004). Randomization in Clinical Trials: Theory and Practice. New York: Wiley.
}
\author{
Xiaoshu Zhu \email{xiaoshuzhu@westat.com} and Sharon Lohr
}
\note{
Because the program allows for sequential allocation methods, it can be slow when the data set and/or number of iterations is large.
}
\examples{
sampsize <- 200
percent <- c(0.5,0.8,0.2,0.4)
set.seed(200)

xmat <- matrix(rbinom(sampsize*length(percent),1,rep(percent,sampsize)),
              nrow=sampsize,ncol=length(percent),byrow=TRUE)
colnames(xmat) <- c("C1","C2","C3","C4")
strat_factor <- xmat[,2]*2 + xmat[,4] + 1

SeqAlloc(xmat,carwt=c(.4,.3,.2,.1),strata=strat_factor,blksize=c(2,6),
         pbcd=.7,pcar=.8,bsdtol=2,caittol=5,niter=10, seed = 20850)

}
\keyword{ Designed Experiment }
