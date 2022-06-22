\name{distance}
\alias{distance}
\alias{WM}
\alias{WMdiffresp}
\alias{WMdiffrespweight}
\alias{WMdiffrespshift}
\alias{WMdiffrespshiftweight}
\title{Calculation of the Wasserstein metric between an empirical data
  set and a data set similated by the queueing model of visual search} 
\description{
 \code{WM} calculates a distance between the empirical and simulated
 response time on a given number of trials in an experiment using
 standard visual search paradigm. 
 \code{WMdiffresp} takes both correct and incorrect response times into
 account. 
 \code{WMdiffrespweight} takes both correct and incorrect response times
 into account and weights the distances of correct and incoreect
 response times with the relative frequencies of the data. 
 \code{WMdiffrespshift} takes both correct and incorrect response times
 into account and assumes different non-decision times for no and yes
 responses. 
 \code{WMdiffrespshiftweight} takes both correct and incorrect response
 times into account assuming different non-decision times for no and yes
 responses and weights the distances with the relative frequencies of
 the data. 
}
\usage{
WM(par, esterrorpar, c, k, pr, N, empRT, old=FALSE) 
WMdiffresp(par, esterrorpar, c, k, pr, N, empRT, empresp, old=FALSE,
           seed=0) 
WMdiffrespweight(par, esterrorpar, c, k, pr, N, empRT, empresp,
           old=FALSE, seed=0) 
WMdiffrespshift(par, esterrorpar, c, k, pr, N, empRT, empresp,
                old=FALSE, seed=0) 
WMdiffrespshiftweight(par, esterrorpar, c, k, pr, N, empRT, empresp,
                      old=FALSE, sep_shift = TRUE, wcorrect = NULL, seed=0) 
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
  \item{empresp}{A vector of empirical responses collected under given
    target presence and set size condition.} 
  \item{old}{\old}
  \item{sep_shift}{Logical. Shall separate shifts be used for positive
    and negative answers?}
  \item{wcorrect}{Logical or \code{NULL}. Weighing of the positive
    responses in the convex
    combination of \eqn{L_q} distances between empirical and theoretical
    distributions for the positive and negativ answers separately.}
  \item{seed}{\seed}
}
\value{
  A positive number. 
  \code{WMdiffresp} returns the sum of the distances associated with
  correct and incorrect response times,  
  \code{WMdiffrespweight} the sum of the weighted distances. 
  \code{WMdiffrespshift} the sum of the distances associated with
  correct and incorrect response times, assuming different non-decision
  times for no and yes responses.
  \code{WMdiffrespshiftweight} the weighted sum. 
}
\references{
  \yiqiDiss
}

\author{\both}

% library(qmvs); ?qmvs


\seealso{
 \code{\link{queue}, \link{LqDist}, \link{sim}},
}
  
\examples{
simdata1 <- sim.ny(par = c(30, 200, 250, 350), esterrorpar = c(-2.67, 0.0094,
0.0299, 0.0020, 1.13), c = 4, k = 12, N = 10000, pr = TRUE, seed = 0)

simdata2 <- sim.ny(par = c(30, 200, 250, 350), esterrorpar = c(-2.67, 0.0094,
0.0299, 0.0020, 1.13), c = 4, k = 12, N = 10000, pr = TRUE, seed =
12345)

WM(par = c(30, 200, 300), esterrorpar = c(-2.67, 0.0094,
0.0299, 0.0020, 1.13), c = 4, k = 12, pr = TRUE, N = 10000, empRT =
simdata2[,1], old=FALSE)

WMdiffresp(par = c(30, 200, 300), esterrorpar = c(-2.67, 0.0094,
0.0299, 0.0020, 1.13), c = 4, k = 12, pr = TRUE, N = 10000, empRT =
simdata2[,1], empresp = simdata2[,2], old=FALSE)

WMdiffrespweight(par = c(30, 200, 300), esterrorpar = c(-2.67, 0.0094,
0.0299, 0.0020, 1.13), c = 4, k = 12, pr = TRUE, N = 10000, empRT =
simdata2[,1], empresp = simdata2[,2], old=FALSE)

WMdiffrespshift(par = c(30, 200, 250, 350), esterrorpar = c(-2.67, 0.0094,
0.0299, 0.0020, 1.13), c = 4, k = 12, pr = TRUE, N = 10000, empRT =
simdata2[,1], empresp = simdata2[,2], old=FALSE)

WMdiffrespshiftweight(par = c(30, 200, 250, 350), esterrorpar = c(-2.67, 0.0094,
0.0299, 0.0020, 1.13), c = 4, k = 12, pr = TRUE, N = 10000, empRT =
simdata2[,1], empresp = simdata2[,2], old=FALSE) 
}

\keyword{queueing}
\keyword{visual search}
\keyword{dynamic}
\keyword{models}

