\name{sim}
\alias{sim}
\alias{sim.ny}
\title{Simulation of the queueing model of visual search}
\description{
 \code{sim.ny} simulates a data set containing the responses and
 response times on either target present or target absent trials under
 specified set size level in an experiment using standard visual search
 paradigm, whereby the non-decision time for yes and no options are
 represented by two different parameters.
}
\usage{
sim.ny(par, esterrorpar, c, k, pr, N, empRT, seed=0)
}
% hidden argument: old=FALSE
\arguments{
  \item{par}{\par}  
  \item{esterrorpar}{\esterrorpar}
  \item{c}{\c} 
  \item{k}{\k}
  \item{pr}{\pr}  
  \item{N}{\N}
 \item{empRT}{\empRT}
   \item{seed}{\seed}
}
\value{
  A \eqn{N \times 2} matrix. The first column contains the simulated response times
  and the second colum the corresponding simulated responses.
}
\references{
  \yiqiDiss
}

\author{\both}

% library(qmvs); ?qmvs


\seealso{
 \code{\link{queue}}
}
  
\examples{
sim.ny(par = c(30, 200, 250, 350),
       esterrorpar = c(-2.67, 0.0094, 0.0299, 0.0020, 1.13),
       c = 4,
       k = 12,
       pr = TRUE, 
       N = 10000, seed = 0)
}

\keyword{queueing}
\keyword{visual search}
\keyword{dynamic}
\keyword{models}

