
## Authors 
## Martin Schlather, martin.schlather@uni-mannheim.de
##
##
## Copyright (C) 2015 -- 2021 Martin Schlather
##
## This program is free software; you can redistribute it and/or
## modify it under the terms of the GNU General Public License
## as published by the Free Software Foundation; either version 3
## of the License, or (at your option) any later version.
##
## This program is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with this program; if not, write to the Free Software
## Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.  

### differentiate RT with regard to correct/incorrect responses




LofK <- function(alpha, beta, k, HighPrecisionAssumed=TRUE) {
  x <- exp(alpha + beta*k)
  if (HighPrecisionAssumed) {
    stopifnot(all(x <= 1))
    k * (1 - x)
  } else k / (1 + x)
}


do_queue <- function(par, esterrorpar, c, k, pr, N, empRT, old=FALSE, queue) {
  miat <- par[1]
  mst <- par[2]
  Tres <- par[3]
  alpha <- esterrorpar[1]
  beta <- esterrorpar[2]
  a1 <- esterrorpar[3]
  a2 <- esterrorpar[4]
  b <- esterrorpar[5]
  lk <- LofK(alpha, beta, k) 
  L <- floor(lk) + rbinom(n = N, size = 1, prob = lk-floor(lk))
  p1 <- a1*k^(-b)
  p2 <- a2*k^b
  misidd <- matrix(as.integer(rbinom(n = N*k, size = 1, prob = p1)), nrow = k,
                   byrow = TRUE) ## !! note
  misidt <- rbinom(n = N, size = 1, prob = p2)
  arrival = matrix(ncol = N, rexp(N* k))
  serving = matrix(ncol = N, rexp(N* k))
  tpos = if (pr) sample.int(k, size = N, replace=TRUE) else rep(k+1, N)
  
  result <- queue(miat, mst, c, pr, L, misidd, misidt,
                  arrival, serving, tpos, old=old)
}



do_WMdiffresp <- function(par, esterrorpar, c, k, pr, N, empRT, empresp,
                          old=FALSE, queue, seed=0) {
  if (length(seed) > 0) set.seed(seed)
  Tres <- par[3]

  result <- do_queue(par, esterrorpar, c, k, pr, N, empRT,
                     old=old, queue=queue)
  
  simRT <- result[ , 4]
  simresp <- result[ , 5]
  WMresult <- LqDist(simRT[simresp==pr] + Tres, empRT[empresp==pr], 1) +
    LqDist(simRT[simresp==1-pr] + Tres, empRT[empresp==1-pr], 1)
  return(WMresult)
}

WMdiffresp <- function(par, esterrorpar, c, k, pr, N, empRT, empresp, old=FALSE,
                       seed=0)
  do_WMdiffresp(par, esterrorpar, c, k, pr, N, empRT, empresp, old=old, queue,
                seed=seed)

WM1diffresp <- function(par, esterrorpar, c, k, pr, N, empRT, empresp,old=FALSE,
                       seed=0)
  do_WMdiffresp(par, esterrorpar, c, k, pr, N, empRT, empresp, old=old, queue1,
                seed=seed)



WMdiffrespshiftweight <- function(par, esterrorpar, c, k, pr, N, empRT, empresp,
                                  old=FALSE, sep_shift=TRUE,  wcorrect=NULL,
                                  seed=0) {
  if (length(seed) > 0) set.seed(seed)
 
  result <- do_queue(par, esterrorpar, c, k, pr, N, empRT,
                     old=old, queue=queue)

  simRT <- result[ , 4]
  simresp <- result[ , 5]
  if (sep_shift) {
    Tresn <- par[3]
    Tresy <- par[4]
  } else {
    Tres <- par[3]
    Tresn <- Tresy <- Tres
  }
  simRT[simresp] <- simRT[simresp] + Tresy
  simRT[!simresp] <- simRT[!simresp] + Tresn

  if (length(wcorrect) == 0) {
    wcorrect = ( if (pr) sum(empresp) else 1-sum(empresp) ) / length(empresp)
  }

  WMresult <- LqDist(simRT[simresp==pr], empRT[empresp==pr], 1)*wcorrect +
    LqDist(simRT[simresp==1-pr], empRT[empresp==1-pr], 1)*(1-wcorrect)
  return(WMresult)
}


WMdiffrespshift <- function(par, esterrorpar, c, k, pr, N, empRT, empresp,
                            old=FALSE, seed=0) {
  ## Achtung! Faktor 2
  2 * WMdiffrespshiftweight(par, esterrorpar, c, k, pr, N, empRT, empresp,
                        old, sep_shift=TRUE,  wcorrect=0.5, seed=seed)

}

WMdiffrespweight <- function(par, esterrorpar, c, k, pr, N, empRT, empresp,
                             old=FALSE, seed=0) {
  WMdiffrespshiftweight(par, esterrorpar, c, k, pr, N, empRT, empresp,
                        old, sep_shift=FALSE,  wcorrect=NULL, seed=seed)
}
