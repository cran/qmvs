\name{queue}
\alias{queue}
\title{Simulation of the queueing model of visual search}
\description{
 \code{queue} simulates the response and response time on a single trial
 in an experiment using standard visual search paradigm.
} 
\usage{
queue(miat, mst, c, pr, L, misidd, misidt, arrival, serving, tposition,
      old=FALSE)
} 
% hidden argument: old=FALSE
\arguments{
  \item{miat}{A positive number representing the scale parameter of the
    exponential distribution of interarrival times, i.e., the mean
    interarrival time.}  
  \item{mst}{A positive number representing the scale parameter of the
    exponential distribution of service times, i.e., the mean service
    time.}  
  \item{c}{\c}  
  \item{pr}{\pr}   
  \item{L}{A natural number representing the number of stimuli that have
    been identified to terminate the queueing process.}
  \item{misidd}{Logical vector of length k.
    Here, \code{FALSE} means
    correct identification and \eqn{TRUE} misidentification of a distractor.} 
  \item{misidt}{Logical. Here,
    \code{FALSE} means correct identification and \code{TRUE}
    misidentification of the target.} 
  \item{arrival}{A vector of length set size. Elements must be
    exponentially distributed random numbers with rate 1.} 
  \item{serving}{A vector of length set size. Elements must be
    exponentially distributed random numbers with rate 1.} 
  \item{tposition}{A natural number representing the position of the
    target in the queue, in target present case less or equal to set
    size, in target absent case equal to set size +1.} 
  \item{old}{\old}  
}
\value{
  A numeric vector of length 5 indicating the number of visual items
  processed, the mean processing time of a single item, the maximum of
  processing time of a single item, the system response time and the response.
}

\author{\both}


\references{
  \yiqiDiss
  
  Moore C. M.,and Wolfe J. M. (2001)
  Getting beyond the serial/parallel debate in
  visual search: A hybrid approach. In Shapiro, K.L. \emph{The Limits of
  Attention: Temporal Constraints on Human Information
  Processing.} Oxford University Press 
  \doi{10.1093/acprof:oso/9780198505150.003.0009}
}

% library(qmvs); ?qmvs


\seealso{
 \code{\link{sim}, \link{distance}},
}
  
\examples{
queue(miat = 30, mst = 200, c = 4, pr = TRUE, L = 12,
      misidd = rep(0L, 12), misidt = 0, arrival = rexp(12),
      serving = rexp(12), tposition = 7, old=FALSE)
}

\keyword{queueing}
\keyword{visual search}
\keyword{dynamic}
\keyword{models}

