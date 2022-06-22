\name{LqDist}
\alias{LqDist}
\title{Calculation of the distance between two data sets}
\description{
 \code{LqDist} calculates the Wasserstein metric or Kolmogorov-Smirnov
 distance between two data sets. 
}
\usage{
LqDist(E1, E2, q=1) 
}
% hidden argument: old=FALSE
\arguments{
  \item{E1, E2}{Vectors of data between which a distance is to calculate.}
  \item{q}{Option for the distance measure. If \code{q} equals \eqn{1}, the Wasserstein metric will be calculated. If \code{q} equals \code{Inf}, the Kolmogorov-Smirnov distance will be calculated. Default option is \code{1}.}
}
\value{
  A positive number. 
}
\references{
 \yiqiDiss
}

\author{\both}

% library(qmvs); ?qmvs


\seealso{
 \code{\link{distance}},
}
  
\examples{
simdata1 <- sim.ny(par = c(30, 200, 250, 350), esterrorpar = c(-2.67, 0.0094,
0.0299, 0.0020, 1.13), c = 4, k = 12, N = 10000, pr = TRUE, seed = 0)

simdata2 <- sim.ny(par = c(30, 200, 250, 350), esterrorpar = c(-2.67, 0.0094,
0.0299, 0.0020, 1.13), c = 4, k = 12, N = 10000, pr = TRUE, seed =
12345)

simcorrectRT1 <- subset(simdata1, simdata1[,2] == TRUE, select = 1)

simcorrectRT2 <- subset(simdata2, simdata2[,2] == TRUE, select = 1)

LqDist(simcorrectRT1, simcorrectRT2, q=1) 
}

\keyword{queueing}
\keyword{visual search}
\keyword{dynamic}
\keyword{models}

