
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


sim.ny <- function(par, esterrorpar, c, k, pr, N, empRT, seed=0) {
  if (length(seed) > 0) set.seed(seed)
  Tresn <- par[3]
  Tresy <- par[4]
   
  simdata <- do_queue(par, esterrorpar, c, k, pr, N, empRT,
                      old=FALSE, queue=queue)

  simRT <- simdata[,4] + Tresn + simdata[,5]*(Tresy-Tresn)
  simoutput <- cbind(simRT, simdata[,5])
  colnames(simoutput) <- c('simRT','simresponse')
  return(simoutput)
}
