\name{summary95}
\alias{summary95}
\title{
Find summary statistics
}
\description{
Finds the Minimum, 25th percentile, Median, Mean, 75th percentile, 90th percentile, 95th percentile, and Maximum of the vector
}
\usage{
summary95(x)
}
\arguments{
  \item{x}{
vector for which summary statistics are desired
}
}
\details{
The code from the 'summary' function in R is modified to include the 90th and 95th percentiles
}
\value{
vector of summary statistics for data
}
\author{
Xiaoshu Zhu \email{xiaoshuzhu@westat.com} and Sharon Lohr, adapted from 'summary' function in R.
}
\examples{
summary95(rnorm(40))
}
