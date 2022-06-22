/*
 Authors 
 Martin Schlather, martin.schlather@uni-mannheim.de

 Copyright (C) 2019 -- 2021 Martin Schlather

This program is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License
as published by the Free Software Foundation; either version 3
of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See thxie
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.  
*/

//
#include <R.h>
#include <Rinternals.h>
#include <Rdefines.h>
#include "def.h"
#include "Basic_utils.h"
#include "errors_messages.h"
#include "qmvs.h"


#define max_c 20
#define max_k 1000
//#define CORES 4

#define UNSET -1
#define MINI(A,B) (A)<(B) ? A : B
#define MAXI(A,B) (A)>(B) ? A : B

SEXP queue(SEXP Miat, SEXP Mst, SEXP C, SEXP Pr, SEXP LLL,
	   SEXP Misidd, SEXP MMisidt, SEXP AArrival, SEXP SServing,
	   SEXP TPosition) {
   int
     N = ncols(AArrival),
     k = nrows(AArrival),
     *LL = INTEGER(LLL),  // rbinom --- Warum > 1 ??
     *orig_misidd = INTEGER(Misidd), // rbinom
     *Misidt = INTEGER(MMisidt), // rbinom
     c = INTEGER(C)[0], // # server
     *Tposition = INTEGER(TPosition); // idx R codiert !
   bool pr = INTEGER(Pr)[0];
   double
     miat = REAL(Miat)[0],
     mst = REAL(Mst)[0],
     *Arrival = REAL(AArrival),  // k x N
     *Serving = REAL(SServing);
  
  if (N != ncols(SServing)) ERR0("not same number of cols");
  if (N != length(TPosition)) ERR0("number of cols does not match length");
  if (k != nrows(SServing)) ERR0("not same number of rowss");
  if (c > max_c) ERR0("too many servers");
  if (k > max_k) ERR0("too many customers");

  SEXP Result;
  PROTECT(Result = allocMatrix(REALSXP, N, 5));
  double
    *result = REAL(Result);
  int bytes = sizeof(int) * k,
    bytes_ix = sizeof(int) * (c+1);
  
  MEMSET(result, UNSET, N * 5 * sizeof(double));


  //#ifdef DO_PARALLEL
  //#pragma omp parallel for num_threads(CORES)  
  //#endif
  for (int simu=0; simu<N; simu++) {
    //    if (simu != 16) continue;
    //    printf("SIMU = %d ******\n", simu);
    int L  = LL[simu]; // rbinom
    if (L == 0) {
      result[simu + 0] = result[simu + N] = result[simu + 2 * N] = 
	result[simu + 3 * N] = result[simu + 4 * N] = 0.0;
      continue;
    }
    
    int
      misidd[max_k], // rbinom
      ix[max_c + 1], // values have R coding
      entered = 0, // state[2]
      left = 0, // state[3]
      insystem = 0, // state[4]
      tposition = Tposition[simu], // R codierung
      misidt = Misidt[simu],
      stopat = L,  // R codierung ok
      depid = 0, // R Codierung
       sum = 0,
      idx = UNSET;
    bool resp = false;
    double
      out1[max_k + 1],
      out2[max_k + 1],  
      el[max_c + 1], // el[c]: warteschlange
      *arrival = Arrival + simu * k,
      *serving = Serving + simu * k,
      time = 0.0; // state[1]
    
    // tposition = if (pr == 1) sample.int(k, size = 1) else k+1
    MEMCOPYX(misidd, orig_misidd + simu * k, bytes);
    MEMSET(ix, UNSET, bytes_ix);
    if (pr) misidd[tposition - 1] = 0;

    idx = UNSET;
    for (int i=0; i<L; i++) {
      sum += misidd[i];
      if (misidd[i] != 0 && idx == UNSET) idx = i + 1;
    }
    if (!pr) {
      if ((resp = sum != 0)) {
 	stopat = idx;
      }
    } else {
      if (sum==0) {
	if ((resp = misidt==0 && tposition<=L)) {
	  stopat = tposition;
 	}
      } else {
	resp = true;
	stopat = misidt==0 ? MINI(idx, tposition) : idx;
      }
    }

    for (int i=0; i<=c; el[i++]=RF_INF);
    for (int i=0; i<=k; i++) {
      out1[i] = RF_INF;
      out2[i]=RF_NEGINF;
    }

    double m = el[c] =//c:not a server, but the fact of entering a new customer
      arrival[entered] * miat / (double) (k - entered);
    // wo ist serving[0]x
 
    while (m < RF_INF &&
	   ((!resp && left < stopat) || (resp && depid!=stopat))) {

      m = RF_INF;
      idx = UNSET;
      for (int i=0; i<=c; i++) {
	if (el[i] < m) {
	  idx = i;
	  m = el[i];
	}
      }
      if (el[c] == m) { 
	time = m;
	entered++;
	insystem++;
	out1[entered] = time;
	el[c] = entered < k
	  ? time + arrival[entered] * miat / (double) (k - entered)
	  : RF_INF;
	if (insystem <= c) {
	  for (int f=0; f<c; f++) {
	    if (ix[f] == UNSET) {	      
	      el[f] = time + serving[entered - 1] * mst;
	      ix[f] = entered;
	      break;
	    }
	  }
	}
      } else {
	time = m;
        left++;
	insystem--;
	
	depid = ix[idx];

	if (depid <= 0) BUG;
	
	assert(depid > 0);
	out2[depid] = m;
	ix[idx] = UNSET;
	int f;
	for (f=0; f<c; f++) if (ix[f] == UNSET) break;
	int free_servers = f < c;
	for (int i=f+1; i<c; free_servers += (int) (ix[i++] == UNSET));
	if (insystem >= c) {
	  ix[f] = left + (c - free_servers) + 1;
	  el[f] = time + serving[ix[f] - 1] * mst;
	} else {
	  for ( ; f<c; f++) if (ix[f] == UNSET) {
	      el[f] = RF_INF;
	    }
	}
      }
    }
    
    double srt = 0.0,
      max = RF_NEGINF,
      mean = 0.0;
    int len = 0;
    for (int i=1; i<=k; i++) {
      srt = MAXI(srt, out2[i]);
      double wt = out2[i] - out1[i];
      if (R_finite(wt)) {
	len++;
	mean += wt;
	max = MAXI(max, wt);
      }
    }
    if (resp) srt = out2[stopat];
    if (len != left) {
      ERR0("programming error\n");
    }
    if (srt != time) ERR0("strange error -- pls contact maintainer");
    result[simu + 0] = (double) len;
    result[simu + N] = mean / (double) len; //
    result[simu + 2 * N] = max;
    result[simu + 3 * N] = srt;
    result[simu + 4 * N] = (double) resp;
  }

  UNPROTECT(1);

  return Result;
}


SEXP LqDist(SEXP E1, SEXP E2, SEXP Q) {
  int
    i1 = 0,
    i2 = 0,
    n1 = length(E1),
    n2 = length(E2);
  double
    t, t1, t2,
    invn1 = 1.0 / (double) n1,
    invn2 = 1.0 / (double) n2,
    v1 = 0.0,
    v2 = 0.0,
    *e1 = REAL(E1),
    *e2 = REAL(E2),
    q = REAL(Q)[0],
    Eins1 = 1 - 0.5 * invn1,
    Eins2 = 1 - 0.5 * invn2,
    sum = 0.0;
  
  if (e1[i1] < e2[i2]) {
    v1 = invn1;
    t = e1[i1++];
  } else {
    v2 = invn2;
    t = e2[i2++];
  }
  t1 = e1[i1++];
  t2 = e2[i2++];

  if (!R_finite(q)) {
    // kolmpgprov to dp
    while(v1 < Eins1 || v2 < Eins2) {/* !! Rundungsfehler -- nicht < 1*/
      double diff = FABS(v2 - v1);		
      if (t1 < t2) {				
	assert(t1 >= t);			
	sum = MAX(sum, diff);				
	v1 += invn1;					
	t = t1;						
	t1 = i1 < n1 ? e1[i1++] : RF_INF;	
      } else {
 	assert(t2 >= t);
	sum = MAX(sum, diff);
	v2 += invn2;
	t = t2;
	t2 = i2 < n2 ? e2[i2++] : RF_INF;	
     }
    }
  } else {

#define LOOP(X)					\
    while(v1 < Eins1 || v2 < Eins2) { /* !! Rundungsfehler -- nicht < 1*/ \
       double diff = FABS(v2 - v1);	\
       if (t1 < t2) {				\
	 assert(t1 >= t);			\
	 sum += X  * (t1 - t);				 \
	 if (false && X * (t1-t) < 0.0) PRINTF("dv=%f %f\n", diff, t1-t); \
	 v1 += invn1;							\
	 if (false) PRINTF("dv=%f %f %f\n", diff, t1-t, v1);		\
	 t = t1;							\
	 t1 = i1 < n1 ? e1[i1++] : RF_INF;				\
       } else {								\
	 assert(t2 >= t);						\
	 sum += X * (t2 - t);						\
	 if (false && X * (t2-t) < 0.0)	PRINTF("dv=%f %f\n", diff, t2-t); \
	 v2 += invn2;							\
	 if (false) PRINTF("dv=%f t2t=%f v2=%f\n", diff, t2-t, v2);	\
	 t = t2;							\
	 t2 = i2 < n2 ? e2[i2++] : RF_INF;				\
       }								\
       if (false) PRINTF("t=%f t1=%f t2=%f v1=%f v2=%f\n", t, t1, t2, v1,v2); \
    }									\
    
    if (q == 1) {
      LOOP(diff);
    } else if (q == 2) {
      LOOP(diff * diff);
    } else {
#define POWER POW(diff, q)
      LOOP(POWER);
    }
    
  }
  SEXP ans; 
  PROTECT(ans = allocVector(REALSXP, 1));
  //  REAL(ans)[0] = POW(sum, 1.0 / q);
  REAL(ans)[0] = sum;
  UNPROTECT(1);
  return ans;
}



SEXP queue1(SEXP Miat, SEXP Mst, SEXP C, SEXP Pr, SEXP LLL,
	   SEXP Misidd, SEXP MMisidt, SEXP AArrival, SEXP SServing,
	   SEXP TPosition) {
   int
     N = ncols(AArrival),
     k = nrows(AArrival),
     *LL = INTEGER(LLL),  // rbinom --- Warum > 1 ??
     *orig_misidd = INTEGER(Misidd), // rbinom
     *Misidt = INTEGER(MMisidt), // rbinom
     c = INTEGER(C)[0], // # server
     *Tposition = INTEGER(TPosition); // idx R codiert !
   bool pr = INTEGER(Pr)[0];
   double
     miat = REAL(Miat)[0],
     mst = REAL(Mst)[0],
     *Arrival = REAL(AArrival),  // k x N
     *Serving = REAL(SServing);
  
  if (N != ncols(SServing)) ERR0("not same number of cols");
  if (N != length(TPosition)) ERR0("number of cols does not match length");
  if (k != nrows(SServing)) ERR0("not same number of rowss");
  if (c > max_c) ERR0("too many servers");
  if (k > max_k) ERR0("too many customers");

  SEXP Result;
  PROTECT(Result = allocMatrix(REALSXP, N, 5));
  double
    *result = REAL(Result);
  int bytes = sizeof(int) * k,
    bytes_ix = sizeof(int) * (c+1);
  
  MEMSET(result, UNSET, N * 5 * sizeof(double));

  //#ifdef DO_PARALLEL
  //#pragma omp parallel for num_threads(CORES)  
  //#endif
  for (int simu=0; simu<N; simu++) {
    //    if (simu != 16) continue;
    //    printf("SIMU = %d ******\n", simu);
    int L  = LL[simu]; // rbinom
    if (L == 0) {
      result[simu + 0] = result[simu + N] = result[simu + 2 * N] = 
	result[simu + 3 * N] = result[simu + 4 * N] = 0.0;
      continue;
    }
    
    int
      misidd[max_k], // rbinom
      ix[max_c + 1], // values have R coding
      entered = 0, // state[2]
      left = 0, // state[3]
      insystem = 0, // state[4]
      tposition = Tposition[simu], // R codierung
      misidt = Misidt[simu],
      stopat = L,  // R codierung ok
      depid = 0, // R Codierung
       sum = 0,
      idx = UNSET;
    bool resp = false;
    double
      out1[max_k + 1],
      out2[max_k + 1],  
      el[max_c + 1], // el[c]: warteschlange
      *arrival = Arrival + simu * k,
      *serving = Serving + simu * k,
      time = 0.0; // state[1]
    
    // tposition = if (pr == 1) sample.int(k, size = 1) else k+1
    MEMCOPYX(misidd, orig_misidd + simu * k, bytes);
    MEMSET(ix, UNSET, bytes_ix);
    if (pr) misidd[tposition - 1] = 0;
    
    idx = UNSET;
    for (int i=0; i<L; i++) {
      sum += misidd[i];
      if (misidd[i] != 0 && idx == UNSET) idx = i + 1;
    }
    if (!pr) {
      if ((resp = sum != 0)) {
 	stopat = idx;
      }
    } else {
      if (sum==0) {
	if ((resp = misidt==0 && tposition<=L)) {
	  stopat = tposition;
 	}
      } else {
	resp = true;
	stopat = misidt==0 ? MINI(idx, tposition) : idx;
      }
    }

    for (int i=0; i<=c; el[i++]=RF_INF);
    for (int i=0; i<=k; i++) {
      out1[i] = RF_INF;
      out2[i]=RF_NEGINF;
    }
     
    double m = el[c] =//c:not a server, but the fact of entering a new customer
      arrival[entered] * miat;
    // wo ist serving[0]x
 
    while (m < RF_INF &&
	   ((!resp && left < stopat) || (resp && depid!=stopat))) {

      m = RF_INF;
      idx = UNSET;
      for (int i=0; i<=c; i++) {
	if (el[i] < m) {
	  idx = i;
	  m = el[i];
	}
      }
      if (el[c] == m) { 
	time = m;
	entered++;
	insystem++;
	out1[entered] = time;
	el[c] = entered < k ? time + arrival[entered] * miat : RF_INF;

	if (insystem <= c) {
	  for (int f=0; f<c; f++) {
	    if (ix[f] == UNSET) {	      
	      el[f] = time + serving[entered - 1] * mst;
	      ix[f] = entered;
	      break;
	    }
	  }
	}
      } else {
	time = m;
        left++;
	insystem--;
	
	depid = ix[idx];
	if (depid <= 0) BUG;
	
	assert(depid > 0);
	out2[depid] = m;
	ix[idx] = UNSET;
	int f;
	for (f=0; f<c; f++) if (ix[f] == UNSET) break;
	int free_servers = f < c;
	for (int i=f+1; i<c; free_servers += (int) (ix[i++] == UNSET));
	if (insystem >= c) {
	  ix[f] = left + (c - free_servers) + 1;
	  el[f] = time + serving[ix[f] - 1] * mst;
	} else {
	  for ( ; f<c; f++) if (ix[f] == UNSET) {
	      el[f] = RF_INF;
	    }
	}
      }
    }
    
    double srt = 0.0,
      max = RF_NEGINF,
      mean = 0.0;
    int len = 0;
    for (int i=1; i<=k; i++) {
      srt = MAXI(srt, out2[i]);
      double wt = out2[i] - out1[i];
      if (R_finite(wt)) {
	len++;
	mean += wt;
	max = MAXI(max, wt);
      }
    }
    if (resp) srt = out2[stopat];
    if (len != left) {
      ERR0("programming error\n");
    }
    if (srt != time) ERR0("Strange error -- please contact maintainer.");
    result[simu + 0] = (double) len;
    result[simu + N] = mean / (double) len; //
    result[simu + 2 * N] = max;
    result[simu + 3 * N] = srt;
    result[simu + 4 * N] = (double) resp;
  }

  UNPROTECT(1);

  return Result;
}




SEXP queue2(SEXP Miat, SEXP Mst, SEXP C, SEXP Pr, SEXP LLL,
	   SEXP Misidd, SEXP MMisidt, SEXP AArrival, SEXP SServing,
	   SEXP TPosition) {
   int
     N = ncols(AArrival),
     k = nrows(AArrival),
     *LL = INTEGER(LLL),  // rbinom --- Warum > 1 ??
     *orig_misidd = INTEGER(Misidd), // rbinom
     *Misidt = INTEGER(MMisidt), // rbinom
     c = INTEGER(C)[0], // # server
     *Tposition = INTEGER(TPosition); // idx R codiert !
   bool pr = INTEGER(Pr)[0];
   double
     miat = REAL(Miat)[0],
     mst = REAL(Mst)[0],
     *Arrival = REAL(AArrival),  // k x N
     *Serving = REAL(SServing);
  
  if (N != ncols(SServing)) ERR0("not same number of cols");
  if (N != length(TPosition)) ERR0("number of cols does not match length");
  if (k != nrows(SServing)) ERR0("not same number of rowss");
  if (c > max_c) ERR0("too many servers");
  if (k > max_k) ERR0("too many customers");

  SEXP Result;
  PROTECT(Result = allocMatrix(REALSXP, N, 5));
  double
    *result = REAL(Result);
  int bytes = sizeof(int) * k,
    bytes_ix = sizeof(int) * (c+1);
  
  MEMSET(result, UNSET, N * 5 * sizeof(double));


  //  N = 15; printf("N=%d\n", N); 

  //#ifdef DO_PARALLEL
  //#pragma omp parallel for num_threads(CORES)  
  //#endif
  for (int simu=0; simu<N; simu++) {
    //    if (simu != 16) continue;
    //    printf("SIMU = %d ******\n", simu);
    int L  = LL[simu]; // rbinom
    if (L == 0) {
      result[simu + 0] = result[simu + N] = result[simu + 2 * N] = 
	result[simu + 3 * N] = result[simu + 4 * N] = 0.0;
      continue;
    }
    
    int
      misidd[max_k], // rbinom
      ix[max_c + 1], // values have R coding
      entered = 0, // state[2]
      left = 0, // state[3]
      insystem = 0, // state[4]
      tposition = Tposition[simu], // R codierung
      misidt = Misidt[simu],
      stopat = L,  // R codierung ok
      depid = 0, // R Codierung
       sum = 0,
      idx = UNSET;
    bool resp = false;
    double
      out1[max_k + 1],
      out2[max_k + 1],  
      el[max_c + 1], // el[c]: warteschlange
      *arrival = Arrival + simu * k,
      *serving = Serving + simu * k,
      time = 0.0; // state[1]
    
    // tposition = if (pr == 1) sample.int(k, size = 1) else k+1
    MEMCOPYX(misidd, orig_misidd + simu * k, bytes);
    MEMSET(ix, UNSET, bytes_ix);
    if (pr) misidd[tposition - 1] = 0;

    idx = UNSET;
    for (int i=0; i<L; i++) {
      sum += misidd[i];
      if (misidd[i] != 0 && idx == UNSET) idx = i + 1;
    }
    if (!pr) {
      if ((resp = sum != 0)) {
 	stopat = idx;
      }
    } else {
      if (sum==0) {
	if ((resp = misidt==0 && tposition<=L)) {
	  stopat = tposition;
 	}
      } else {
	resp = true;
	stopat = misidt==0 ? MINI(idx, tposition) : idx;
      }
    }

    for (int i=0; i<=c; el[i++]=RF_INF);
    for (int i=0; i<=k; i++) {
      out1[i] = RF_INF;
      out2[i]=RF_NEGINF;
    }

    double m = el[c] =//c:not a server, but the fact of entering a new customer
      arrival[entered] * miat / SQRT((double) (k - entered));
    // wo ist serving[0]x
 
    while (m < RF_INF &&
	   ((!resp && left < stopat) || (resp && depid!=stopat))) {

      m = RF_INF;
      idx = UNSET;
      for (int i=0; i<=c; i++) {
	if (el[i] < m) {
	  idx = i;
	  m = el[i];
	}
      }
      if (el[c] == m) { 
	time = m;
	entered++;
	insystem++;
	out1[entered] = time;
	el[c] = entered < k ? time + arrival[entered] * miat / SQRT((double) (k - entered)) : RF_INF;

	if (insystem <= c) {
	  for (int f=0; f<c; f++) {
	    if (ix[f] == UNSET) {	      
	      el[f] = time + serving[entered - 1] * mst;
	      ix[f] = entered;
	      break;
	    }
	  }
	}
      } else {
	time = m;
        left++;
	insystem--;
	
	depid = ix[idx];
	if (depid <= 0) BUG;
	
	assert(depid > 0);
	out2[depid] = m;
	ix[idx] = UNSET;
	int f;
	for (f=0; f<c; f++) if (ix[f] == UNSET) break;
	int free_servers = f < c;
	for (int i=f+1; i<c; free_servers += (int) (ix[i++] == UNSET));
	if (insystem >= c) {
	  ix[f] = left + (c - free_servers) + 1;
	  el[f] = time + serving[ix[f] - 1] * mst;
	} else {
	  for ( ; f<c; f++) if (ix[f] == UNSET) {
	      el[f] = RF_INF;
	    }
	}
      }      
    }
    
    double srt = 0.0,
      max = RF_NEGINF,
      mean = 0.0;
    int len = 0;
    for (int i=1; i<=k; i++) {
      srt = MAXI(srt, out2[i]);
      double wt = out2[i] - out1[i];
      if (R_finite(wt)) {
	len++;
	mean += wt;
	max = MAXI(max, wt);
      }
    }
    if (resp) srt = out2[stopat];
    if (len != left) {
      ERR0("programming error\n");
    }
    if (srt != time) ERR0("strange error --- please contact maintainer");
    result[simu + 0] = (double) len;
    result[simu + N] = mean / (double) len; //
    result[simu + 2 * N] = max;
    result[simu + 3 * N] = srt;
    result[simu + 4 * N] = (double) resp;
  }

  UNPROTECT(1);

  return Result;
}
