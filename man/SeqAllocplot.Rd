\name{SeqAllocplot}
\alias{SeqAllocplot}
\title{
Plot the evaluation criteria for the designs
}
\description{
Provides boxplots and scatterplots of balance and predictability measures for candidate sequential allocations.
}
\usage{
SeqAllocplot(mysim, bporder = NULL, stratum = F, cexsize = 0.7)
}
\arguments{
  \item{mysim}{
output from function SeqAlloc
}
  \item{bporder}{
vector giving the randomization methods to be plotted, corresponding to the positions in mysim$schemes
}
  \item{stratum}{
logical variable of whether PBD designs should be plotted, default is FALSE
}
  \item{cexsize}{
size of characters in plot and axis, default is 0.7
}
}
\value{
Produces selected plots of predictability and balance for randomization schemes.Requires user to click window or press "enter" to progress through plots.
}
\references{
Lohr, S. and X. Zhu (2015). Randomized Sequential Individual Assignment in Social Experiments: Evaluating the Design Options Prospectively. Sociological Methods and Research. [Advance online publication: December 27, 2015] doi: 10.1177/0049124115621332
}
\author{
Xiaoshu Zhu \email{xiaoshuzhu@westat.com} and Sharon Lohr
}
\note{
These are example plots; the plotting code can be extracted from this function or the function is easily modified if different plots are desired.
}
\seealso{
 \code{\link{SeqAlloc}}
}
\examples{
sampsize <- 200
percent <- c(0.5,0.8,0.2,0.4)
set.seed(200)

xmat <- matrix(rbinom(sampsize*length(percent),1,rep(percent,sampsize)),
              nrow=sampsize,ncol=length(percent),byrow=TRUE)
colnames(xmat) <- c("C1","C2","C3","C4")
strat_factor <- xmat[,1]*4 + xmat[,2]*2 + xmat[,4] + 1

mysim <- SeqAlloc(xmat,carwt=c(.4,.3,.2,.1),strata=strat_factor,blksize=c(2,6),
                   pbcd=.7,pcar=.67,bsdtol=2,caittol=3,niter=10, seed = 30924)

SeqAllocplot(mysim,bporder = c(3,4,7,8), stratum = FALSE, cexsize=0.6)
}
