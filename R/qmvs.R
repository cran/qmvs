
## Authors 
## Martin Schlather, martin.schlather@uni-mannheim.de
## Yiqi Li
##
## Copyright (C) 2015 -- 2021 Martin Schlather, Yiqi Li
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


# Main function
queue_old <- function(miat, mst, c, k, pr, L, misidd, misidt,
                      arrival, serving, tposition) {
  if (L == 0) return(rep(0, 5))
  
  if (pr) misidd[tposition] <- 0
  # determine when the queue will stop and the response
  stopat <- L
  resp <- FALSE
  if (!pr) {
    if (sum(misidd[1:L])!=0) { ## ?? L >=1 garantiert??
      stopat <- which(misidd!=0)[1]
      resp <- TRUE
    }
  } else {
    if (sum(misidd[1:L])==0) {
      if (resp <- misidt==0 && tposition<=L) {
        stopat <- tposition
#        resp <- TRUE
       }
    } else {
      resp <- TRUE
      stopat <-if (misidt==0) min(which(misidd!=0)[1], tposition) else which(misidd!=0)[1]
    }
  }
                                        #
  # initialization
  state <- rep(0, 4)
  out <- vector()
  # next-arr-time and next-dep-time at all servers initially set as infinite
  el <-  rep(Inf, c + 1)
  # index of arrived customers at the servers at current time t, 0 means that the corresponding server is free
  ix <- rep(0, c)
  # the arrival order of the departing customer
  depid <- 0
  # generate time point of the first arrival (state[2]=0)
  el[1] <- arrival[state[2] + 1] / ((k - state[2])/miat) ## rexp(1, rate = k/miat)
  # keep updating the system as long as arrivals or departures are still expected
  # and the most recent departed customer is not the termination criterion
  m <- el[1]

    idx <- NA
  while (m < Inf && ((!resp && state[3] < stopat) || (resp && depid!=stopat))) {
    #bookkeeping <- update(bookkeeping)
    # move to the time point when the next event will happen, whether it is an arr or a dep
    
    m <- min(el)

    
    # if it is an arrival, use updateA, if not, use updateD
    if (el[1] == m) {
      ##   ud <- updateA(state, out)
      # create a new data slot for the arr and dep time for the arriving customer 
      out <- cbind(out, c(Inf, Inf))
      # number of arrivals up to t and number of customers at t in system both increase by 1
      state <- c(m, state[-1] + c(1, 0, 1))
      # register the current time t as the arr time of the arriving customer
      out[1, state[2]] <- state[1]
      # genA
      # if not all customers have arrived yet, generate the time point when the next arrival will happen;
      # if all customers have arrived, no new arrival will happen anymore
      if (state[2] < k) {
        
        el[1] <- state[1] + arrival[state[2] + 1] / ((k - state[2])/miat) # rexp(1, rate = (k - state[2])/miat)##arrival==1
        
        stopifnot(is.finite(el[1]))
      } else {
        el[1] <- Inf
      }
      # situation where the just arrived customer does not have to wait + comment that I don't understand anymore??
      # find out which servers are currently free
      f <- which(ix == 0)
      # if the number of customers in system is not more than the number of servers and not all servers are occupied
      if (state[4] <= c && length(f) != 0) {
        # generate the dep time for the arriving customer and assign it to the free server with the smallest index
        el[f[1] + 1] <- state[1] + serving[state[2]] * mst # rexp(1, rate = 1/mst)
         # register the assignment in the index by logging the arrival order to the corresponding server
        ix[f[1]] <- state[2]
        ##cat("f=", f[1], "")
      }
    } else {
      ##ud <- updateD(state, out, el, ix, depid)
      # nt decreases by 1, dt increases by 1
      state <- c(m, state[-1] + c(0, 1, -1))
                                        # find out the index (arrival order) of the departing customer
      idx <- which(el == m) - 1
      depid <- ix[idx]
      # and register the current time t as his dep time
      out[2, depid] <- m
                                        # set the index at the corresponding server to 0, indicating that this server is becoming free
       stopifnot(depid > 0)
      
      ix[idx] <- 0
      f <- which(ix == 0)
##      cat("g=", f[1],  length(f), state[4] ,  c, "")
          
       # situation where there are arrived customers waiting for service 
      # as the departure is happening (so a server has become free after updating)
      if (state[4] >= c) {
        # the arrival order of the customer getting service is one after the number of
        # customers who have departured or being served
        ix[f[1]] <- state[3] + (c - length(f)) + 1
        # generate the departure time of the customer who is getting service 
        el[f[1] + 1] <- state[1] + serving[ix[f[1]]] * mst# rexp(1, rate = 1/mst)
##        cat(state[1], "f=", f[1], ix[f[1]], "s=", serving[ix[f[1]]], mst, "\n")
        
    } else {
                                        # if there is no customer waiting for service at t, all free servers are set to 'stand by'
##      cat("INF ! ", f);
        el[f + 1] <- Inf
      }

      }

  }

  deptimes <- out[2, ]
  srt <- if(!resp) max(deptimes[is.finite(deptimes)]) else out[2, stopat]
  # replace max(deptimes[is.finite(deptimes)]) with state[1]?
  wt <- out[2, ] - out[1, ]
  wt <- wt[is.finite(wt)]
  # check whether the number of departures is right
  stopifnot(length(wt) == state[3]) 
  if (srt != state[1]) stop("achtung!")
  output <- c(length(wt), mean(wt), max(wt), srt, resp)

  return(output)
}



#old = TRUE: Simuiation der Warteschlangen in R
#old = FALSE: in C
#old =2 : Berechnung beider + Vergleich ob das gleiche Eergebnis rauskommt (nur zum Debuggen fuer mich)




queue <- function(miat, mst, c, pr, L, misidd, misidt, arrival, serving,
                  tposition, old = FALSE) {
  if (old) {
    N <- ncol(arrival)
      
    resultOld <- matrix(NA, nrow = N, ncol = 5) 
    for (simu in 1:N) {
      resultOld[simu, ] <-
        queue_old(miat, mst, c, nrow(arrival),
                  pr, L[simu], misidd[, simu], misidt[simu],
                  arrival[, simu], serving[, simu], tposition[simu])
    }
    if (old != FALSE) ## not: == TRUE !!
      return(resultOld)
  }

  if (old != TRUE) ## not:  == FALSE !!
    result <- .Call(C_queue, as.double(miat), as.double(mst), as.integer(c),
                    as.integer(pr), as.integer(L),
                    misidd, as.integer(misidt),
                    arrival, serving, as.integer(tposition))
  
  if (old > 1) stopifnot(!all(resultOld == result))

  return(result)
}


queue_stablearrival <- function(miat, mst, c, k, pr, L, misidd, misidt,
                                arrival, serving, tposition) {
  if (L == 0) return(rep(0, 5))
  
  if (pr) misidd[tposition] <- 0
  # determine when the queue will stop and the response
  stopat <- L
  resp <- FALSE
  if (!pr) {
    if (sum(misidd[1:L])!=0) { ## ?? L >=1 garantiert??
      stopat <- which(misidd!=0)[1]
      resp <- TRUE
    }
  } else {
    if (sum(misidd[1:L])==0) {
      if (resp <- misidt==0 && tposition<=L) {
        stopat <- tposition
        #        resp <- TRUE
      }
    } else {
      resp <- TRUE
      stopat <-if (misidt==0) min(which(misidd!=0)[1], tposition) else which(misidd!=0)[1]
    }
  }
  #
  # initialization
  state <- rep(0, 4)
  out <- vector()
  # next-arr-time and next-dep-time at all servers initially set as infinite
  el <-  rep(Inf, c + 1)
  # index of arrived customers at the servers at current time t, 0 means that the corresponding server is free
  ix <- rep(0, c)
  # the arrival order of the departing customer
  depid <- 0
  # generate time point of the first arrival (state[2]=0)
  el[1] <- arrival[state[2] + 1] * miat ## rexp(1, rate = k/miat)
  # keep updating the system as long as arrivals or departures are still expected
  # and the most recent departed customer is not the termination criterion
  m <- el[1]
  
  idx <- NA
  #str(m)
  while (m < Inf && ((!resp && state[3] < stopat) || (resp && depid!=stopat))) {
    #bookkeeping <- update(bookkeeping)
    # move to the time point when the next event will happen, whether it is an arr or a dep
     
    m <- min(el)
    
    # if it is an arrival, use updateA, if not, use updateD
    if (el[1] == m) {
      ##   ud <- updateA(state, out)
      # create a new data slot for the arr and dep time for the arriving customer 
      out <- cbind(out, c(Inf, Inf))
      # number of arrivals up to t and number of customers at t in system both increase by 1
      state <- c(m, state[-1] + c(1, 0, 1))
      # register the current time t as the arr time of the arriving customer
      out[1, state[2]] <- state[1]
      # genA
      # if not all customers have arrived yet, generate the time point when the next arrival will happen;
      # if all customers have arrived, no new arrival will happen anymore
      if (state[2] < k) {
        
        el[1] <- state[1] + arrival[state[2] + 1] * miat # rexp(1, rate = (k - state[2])/miat)##arrival==1
        
        
        stopifnot(is.finite(el[1]))
      } else {
        el[1] <- Inf
      }
      # situation where the just arrived customer does not have to wait + comment that I don't understand anymore??
      # find out which servers are currently free
      f <- which(ix == 0)
      # if the number of customers in system is not more than the number of servers and not all servers are occupied
      if (state[4] <= c && length(f) != 0) {
        # generate the dep time for the arriving customer and assign it to the free server with the smallest index
        el[f[1] + 1] <- state[1] + serving[state[2]] * mst # rexp(1, rate = 1/mst)
         # register the assignment in the index by logging the arrival order to the corresponding server
        ix[f[1]] <- state[2]
        ##cat("f=", f[1], "")
      }
    } else {
      ##ud <- updateD(state, out, el, ix, depid)
      # nt decreases by 1, dt increases by 1
      state <- c(m, state[-1] + c(0, 1, -1))
      # find out the index (arrival order) of the departing customer
      idx <- which(el == m) - 1
      depid <- ix[idx]
      # and register the current time t as his dep time
      out[2, depid] <- m
      # set the index at the corresponding server to 0, indicating that this server is becoming free
       stopifnot(depid > 0)
      
      ix[idx] <- 0
      f <- which(ix == 0)
      ##      cat("g=", f[1],  length(f), state[4] ,  c, "")
      
      # situation where there are arrived customers waiting for service 
      # as the departure is happening (so a server has become free after updating)
      if (state[4] >= c) {
        # the arrival order of the customer getting service is one after the number of
        # customers who have departured or being served
        ix[f[1]] <- state[3] + (c - length(f)) + 1
        # generate the departure time of the customer who is getting service 
        el[f[1] + 1] <- state[1] + serving[ix[f[1]]] * mst# rexp(1, rate = 1/mst)
        ##        cat(state[1], "f=", f[1], ix[f[1]], "s=", serving[ix[f[1]]], mst, "\n")
        
      } else {
        # if there is no customer waiting for service at t, all free servers are set to 'stand by'
        ##      cat("INF ! ", f);
        el[f + 1] <- Inf
      }
     }
    
   }
  deptimes <- out[2, ]
  srt <- if(!resp) max(deptimes[is.finite(deptimes)]) else out[2, stopat]
  # replace max(deptimes[is.finite(deptimes)]) with state[1]?
  wt <- out[2, ] - out[1, ]
  wt <- wt[is.finite(wt)]
  # check whether the number of departures is right
  stopifnot(length(wt) == state[3]) 
  if (srt != state[1]) stop("achtung!")
  output <- c(length(wt), mean(wt), max(wt), srt, resp)
  
  return(output)
}


queue1 <- function(miat, mst, c, pr, L, misidd, misidt, arrival, serving,
                  tposition, old = FALSE) {
  if (old) {
    N <- ncol(arrival)
    
    resultOld <- matrix(NA, nrow = N, ncol = 5) 
    for (simu in 1:N) {
      resultOld[simu, ] <-
        queue_stablearrival(miat, mst, c, nrow(arrival),
                  pr, L[simu], misidd[, simu], misidt[simu],
                  arrival[, simu], serving[, simu], tposition[simu])
    }
    if (old != FALSE) ## not: == TRUE !!
      return(resultOld)
  }
  
  if (old != TRUE) ## not:  == FALSE !!
    result <- .Call(C_queue1, as.double(miat), as.double(mst), as.integer(c),
                    as.integer(pr), as.integer(L),
                    misidd, as.integer(misidt),
                    arrival, serving, as.integer(tposition))
  
  if (old > 1) stopifnot(!all(resultOld == result))
  
  return(result)
}


queue_sq <- function(miat, mst, c, k, pr, L, misidd, misidt,
                      arrival, serving, tposition) {
  if (L == 0) return(rep(0, 5))
  
  if (pr) misidd[tposition] <- 0
  # determine when the queue will stop and the response
  stopat <- L
  resp <- FALSE
  if (!pr) {
    if (sum(misidd[1:L])!=0) { ## ?? L >=1 garantiert??
      stopat <- which(misidd!=0)[1]
      resp <- TRUE
    }
  } else {
    if (sum(misidd[1:L])==0) {
      if (resp <- misidt==0 && tposition<=L) {
        stopat <- tposition
        #        resp <- TRUE
      }
    } else {
      resp <- TRUE
      stopat <-if (misidt==0) min(which(misidd!=0)[1], tposition) else which(misidd!=0)[1]
    }
  }
  # initialization
  state <- rep(0, 4)
  out <- vector()
  # next-arr-time and next-dep-time at all servers initially set as infinite
  el <-  rep(Inf, c + 1)
  # index of arrived customers at the servers at current time t, 0 means that the corresponding server is free
  ix <- rep(0, c)
  # the arrival order of the departing customer
  depid <- 0
  # generate time point of the first arrival (state[2]=0)
  el[1] <- arrival[state[2] + 1] / (sqrt(k - state[2])/miat) ## rexp(1, rate = k/miat)
  # keep updating the system as long as arrivals or departures are still expected
  # and the most recent departed customer is not the termination criterion
  m <- el[1]
  
   idx <- NA
  while (m < Inf && ((!resp && state[3] < stopat) || (resp && depid!=stopat))) {
    #bookkeeping <- update(bookkeeping)
    # move to the time point when the next event will happen, whether it is an arr or a dep
    
    m <- min(el)
       
    # if it is an arrival, use updateA, if not, use updateD
    if (el[1] == m) {
      ##   ud <- updateA(state, out)
      # create a new data slot for the arr and dep time for the arriving customer 
      out <- cbind(out, c(Inf, Inf))
      # number of arrivals up to t and number of customers at t in system both increase by 1
      state <- c(m, state[-1] + c(1, 0, 1))
      # register the current time t as the arr time of the arriving customer
      out[1, state[2]] <- state[1]
      # genA
      # if not all customers have arrived yet, generate the time point when the next arrival will happen;
      # if all customers have arrived, no new arrival will happen anymore
      if (state[2] < k) {
        
        el[1] <- state[1] + arrival[state[2] + 1] / (sqrt(k - state[2])/miat) # rexp(1, rate = (k - state[2])/miat)##arrival==1
                
        stopifnot(is.finite(el[1]))
      } else {
        el[1] <- Inf
      }
      # situation where the just arrived customer does not have to wait + comment that I don't understand anymore??
      # find out which servers are currently free
      f <- which(ix == 0)
      # if the number of customers in system is not more than the number of servers and not all servers are occupied
      if (state[4] <= c && length(f) != 0) {
        # generate the dep time for the arriving customer and assign it to the free server with the smallest index
        el[f[1] + 1] <- state[1] + serving[state[2]] * mst # rexp(1, rate = 1/mst)
         # register the assignment in the index by logging the arrival order to the corresponding server
        ix[f[1]] <- state[2]
        ##cat("f=", f[1], "")
      }
    } else {
      ##ud <- updateD(state, out, el, ix, depid)
      # nt decreases by 1, dt increases by 1
      state <- c(m, state[-1] + c(0, 1, -1))
      # find out the index (arrival order) of the departing customer
      idx <- which(el == m) - 1
      depid <- ix[idx]
      # and register the current time t as his dep time
      out[2, depid] <- m
      # set the index at the corresponding server to 0, indicating that this server is becoming free
      stopifnot(depid > 0)
      
      ix[idx] <- 0
      f <- which(ix == 0)
      ##      cat("g=", f[1],  length(f), state[4] ,  c, "")
      
      # situation where there are arrived customers waiting for service 
      # as the departure is happening (so a server has become free after updating)
      if (state[4] >= c) {
        # the arrival order of the customer getting service is one after the number of
        # customers who have departured or being served
        ix[f[1]] <- state[3] + (c - length(f)) + 1
        # generate the departure time of the customer who is getting service 
        el[f[1] + 1] <- state[1] + serving[ix[f[1]]] * mst# rexp(1, rate = 1/mst)
        ##        cat(state[1], "f=", f[1], ix[f[1]], "s=", serving[ix[f[1]]], mst, "\n")
        
      } else {
        # if there is no customer waiting for service at t, all free servers are set to 'stand by'
        ##      cat("INF ! ", f);
        el[f + 1] <- Inf
      }
      
     }
    
   }
  #
  deptimes <- out[2, ]
  srt <- if(!resp) max(deptimes[is.finite(deptimes)]) else out[2, stopat]
  # replace max(deptimes[is.finite(deptimes)]) with state[1]?
  wt <- out[2, ] - out[1, ]
  wt <- wt[is.finite(wt)]
  # check whether the number of departures is right
  stopifnot(length(wt) == state[3]) 
  if (srt != state[1]) stop("achtung!")
  output <- c(length(wt), mean(wt), max(wt), srt, resp)
  
  return(output)
}

queue2 <- function(miat, mst, c, pr, L, misidd, misidt, arrival, serving,
                   tposition, old = FALSE) {
  if (old) {
    N <- ncol(arrival)
    
    resultOld <- matrix(NA, nrow = N, ncol = 5) 
    for (simu in 1:N) {
      resultOld[simu, ] <-
        queue_sq(miat, mst, c, nrow(arrival),
                            pr, L[simu], misidd[, simu], misidt[simu],
                            arrival[, simu], serving[, simu], tposition[simu])
    }
    if (old != FALSE) ## not: == TRUE !!
      return(resultOld)
  }
  
  if (old != TRUE) ## not:  == FALSE !!
    result <- .Call(C_queue2, as.double(miat), as.double(mst), as.integer(c),
                    as.integer(pr), as.integer(L),
                    misidd, as.integer(misidt),
                    arrival, serving, as.integer(tposition))
  
  if (old > 1) stopifnot(!all(resultOld == result))
  
  return(result)
}
  

do_WM <- function(par, esterrorpar, c, k, pr, N, empRT, old=FALSE, queue,
                  Wasserstein = FALSE) {  
  result <-  do_queue(par, esterrorpar, c, k, pr, N, empRT,
                      old=old, queue=queue)
  
  simRT <- result[ , 4]
  Tres <- par[3]
  
  if (Wasserstein) {
    stop("'WassSqDistH' not included yet")
    #histemp <- data2hist(as.numeric(empRT))
    #histsim <- data2hist(as.numeric(simRT+Tres))
    #WMresult <- WassSqDistH(histemp, histsim)
  } else { 
    WMresult <- LqDist(simRT + Tres, empRT, 1)
  }
  
  return(WMresult)
}


WM <- function(par, esterrorpar, c, k, pr, N, empRT, old=FALSE)
  do_WM(par, esterrorpar, c, k, pr, N, empRT, old=old, queue)


WM1 <- function(par, esterrorpar, c, k, pr, N, empRT, old=FALSE)
  do_WM(par, esterrorpar, c, k, pr, N, empRT, old=old, queue1)


WM2 <- function(par, esterrorpar, c, k, pr, N, empRT, old=FALSE) 
   do_WM(par, esterrorpar, c, k, pr, N, empRT, old=old, queue)


LqDist <- function(E1, E2, q=1) {
  E1 <- sort(E1)
  E2 <- sort(E2)
  .Call(C_LqDist, as.double(E1), as.double(E2), as.double(q))
}
