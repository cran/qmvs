/*
 Authors 
 Martin Schlather, martin.schlather@uni-mannheim.de


 Copyright (C) 2021 -- 2021 Martin Schlather

This program is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License
as published by the Free Software Foundation; either version 3
of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.  
*/


#ifndef basic_rfutils_h
#define basic_rfutils_h 1

// RFU 3: vor Basic kein inttypes.h ( _INTTYPES_H )  oder aber RFdef_H==1;
// miraculix 0: vor Basic inttypes.h; Ausnahme initNerror.cc kleinkram.cc thuy.cc xport_import.cc zzz.cc
// adoption 0: vor Basic kein inttypes.h

#define RFU_VERSION 12

#if defined RFU_LOCAL || defined RF_VERSION || defined MIRACULIX_VERSION || defined ADOPTION_VERSION || defined HAPLOBLOCKER_VERSION
  #if defined OBSOLETE_RFU
    #undef OBSOLETE_RFU
  #endif
#else
  #if defined _INTTYPES_H
    #define obsolete_miraculix 1
  #endif
  #ifndef OBSOLETE_RFU
    #define OBSOLETE_RFU 1
  #endif
#endif


#define F77call F77_CALL // rename to control that USE_FC_LEN_T has been called
#ifdef __cplusplus
#define F77name extern "C" void F77_NAME // rename to control that USE_FC_LEN_T has been called
#else
#define F77name void F77_NAME 
#endif
#define F77dgesdd F77call(dgesdd)
#define F77dgemv F77call(dgemv)
#define F77ddot F77call(ddot)
#define F77dsyrk F77call(dsyrk)


#ifndef __cplusplus
#include <stdbool.h>
#endif

#include "def.h"
#include <inttypes.h>
#include <R.h>
#include <Rmath.h>
#include <Rinternals.h>

#include "AutoRandomFieldsUtils.h"

#define RFERROR error

#define RFERROR1(M,A) {errorstring_type E_AUX; \
    SPRINTF(E_AUX, M, A); RFERROR(E_AUX);}
#define RFERROR2(M,A,B) {errorstring_type E_AUX; \
    SPRINTF(E_AUX, M, A,B); RFERROR(E_AUX);}
#define RFERROR3(M,A,B,C) {errorstring_type E_AUX;\
    SPRINTF(E_AUX, M, A,B,C); RFERROR(E_AUX);}
#define RFERROR4(M,A,B,C,D) {errorstring_type E_AUX; \
    SPRINTF(E_AUX, M, A,B,C,D); RFERROR(E_AUX);}
#define RFERROR5(M,A,B,C,D,E) {errorstring_type E_AUX; \
    SPRINTF(E_AUX, M, A,B,C,D,E); RFERROR(E_AUX);}
#define RFERROR6(M,A,B,C,D,E,F) {errorstring_type E_AUX;\
    SPRINTF(E_AUX, M, A,B,C,D,E,F); RFERROR(E_AUX);}
#define RFERROR7(M,A,B,C,D,E,F,G) {errorstring_type E_AUX;\
    SPRINTF(E_AUX, M, A,B,C,D,E,F,G); RFERROR(E_AUX);}


#define MULTIMINSIZE(S) ((S) > 20)// in omp parallel in DO_PARALLEL
// #define MULTIMINSIZE(S) false
// #define MULTIMINSIZE(S) true


typedef char name_type[][MAXCHAR];
typedef enum usr_bool {
  // NOTE: if more options are included, change ExtendedBoolean in
  // userinterface.cc of RandomFields
  False=false, 
  True=true, 
  //Exception=2, // for internal use only
  Nan=INT_MIN
} usr_bool;


#define RF_NA NA_REAL 
#define RF_NAN R_NaN
#define RF_NEGINF R_NegInf
#define RF_INF R_PosInf
#define T_PI M_2_PI

#define OBSOLETENAME "obsolete" 

#define MAXINT 2147483647
#define MININT -2147483647
#define MAXUNSIGNED (MAXINT * 2) + 1
#define INFDIM MAXINT
#define INFTY INFDIM
#define PIDMODULUS 1000

#define LENGTH length // to avoid the unvoluntiered use of LENGTH defined by R
#define complex Rcomplex
#define DOT "."
#define GAUSS_RANDOM(SIGMA) rnorm(0.0, SIGMA)
#define UNIFORM_RANDOM unif_rand()
#define POISSON_RANDOM(x) rpois(x)
#define SQRT2 M_SQRT2
#define SQRTPI M_SQRT_PI
#define INVPI M_1_PI
#define PIHALF M_PI_2 
#define ONETHIRD 0.333333333333333333333333
#define TWOTHIRD 0.6666666666666666666666667
#define TWOPI 6.283185307179586476925286766559
#define INVLOG2 1.442695040888963
#define INVSQRTTWO 0.70710678118654752440084436210
#define INVSQRTTWOPI 0.39894228040143270286
#define SQRTTWOPI 2.5066282746310002416
#define SQRTINVLOG005 0.5777613700268771079749
//#define LOG05 -0.69314718055994528623
#define LOG3 1.0986122886681096913952452369225257046474905578227
#define LOG2 M_LN2
#define EULER_C 0.5772156649015328606065120900824024310421


#define EPSILON     0.00000000001
#define EPSILON1000 0.000000001

#define MIN(A,B) ((A) < (B) ? (A) : (B))
#define MAX(A,B) ((A) > (B) ? (A) : (B))

#define ACOS std::acos
#define ASIN std::asin
#define ATAN std::atan
#define FMIN fmin2
#define FMAX fmax2
#define ATANH std::atanh
#define ACOSH std::acosh
#define ASINH std::asinh
#define EXPM1 std::expm1
#define LOG1P std::log1p
#define FROUND fround
#define COS std::cos
#define EXP std::exp
#define FABS(X) std::fabs((double) X) // OK; keine Klammern um X!
#if ! defined MALLOCX
#define MALLOCX std::malloc
#define FLOOR std::floor
#define SQRT(X) std::sqrt((double) X) // OK
#define CEIL(X) std::ceil((double) X) // OK; keine Klammern um X!
#define FREEX std::free
#endif
#define LOG std::log
#define POW(X, Y) R_pow((double) X, (double) Y) // OK; keine Klammern um X!
#define SIGN(X) sign((double) X) // OK
#define SIN std::sin
#define STRCMP(A, B) std::strcmp(A, B) // OK
#define STRCPY(A, B) std::strcpy(A, B) // OK
#define STRLEN std::strlen
#define STRNCMP(A, B, C) std::strncmp(A, B, C) // OK
#define STRNCPY(A, B, N) strcopyN(A, B, N) // OK
#define TAN std::tan
#define MEMCOPYX std::memcpy
#define MEMMOVE std::memmove
#define MEMSET std::memset  
#define MEMCMP std::memcmp
#define AALLOC std::aligned_alloc
#define CALLOCX std::calloc
#define SPRINTF std::sprintf // Rprint 
#define ROUND(X) ownround((double) X) // OK
#define TRUNC(X) ftrunc((double) X) // OK; keine Klammern um X!
#define QSORT std::qsort

#define print NEVER_USE_print_or_PRINTF_WITHIN_PARALLEL /* // */

#if defined SCHLATHERS_MACHINE && defined DO_PARALLEL && defined OMP_H
#define PRINTF if (omp_get_num_threads() > 1) { error("\n\nnever use Rprintf/PRINTF within parallel constructions!!\n\n"); } else Rprintf // OK
#else
#define PRINTF Rprintf
#endif


#define R_PRINTLEVEL 1
#define C_PRINTLEVEL 1


#define MAXERRORSTRING 1000
typedef char errorstring_type[MAXERRORSTRING];


typedef unsigned int Uint;
typedef uint64_t Ulong;
typedef int64_t Long;






// not SCHLATHERS_MACHINE
#ifndef SCHLATHERS_MACHINE

#define INTERNALMSG SERR0("Sorry. This functionality doesn't exist currently. There is work in progress at the moment by the maintainer.")
#if ! defined assert
#define assert(X) {}
#endif
#define BUG {								\
    RFERROR4("Severe error occured in function '%.50s' (file '%.50s', line %d).%.200s", \
	     __FUNCTION__, __FILE__, __LINE__, CONTACT);			\
  }
#undef BUG
#define BUG ERR0("unexpected error occurred\n");


//#define MEMCOPY(A,B,C) {MEMCPY(A,B,C); printf("memcpy %.50s %d\n", __FILE__, __LINE__);}
#define MEMCOPY(A,B,C) MEMCOPYX(A,B,C)
#define AMALLOC(ELEMENTS, SIZE) AALLOC(SIZE, (SIZE) * (ELEMENTS))
#if ! defined MALLOC
#define MALLOC MALLOCX
#define FREE(X) if ((X) == NULL) {} else {FREEX(X); (X)=NULL;}
#endif
#define CALLOC CALLOCX
#define XCALLOC CALLOCX
//

#define UNCONDFREE(X) {FREEX(X); (X)=NULL;}
#endif // not SCHLATHERS_MACHINE



// SCHLATHERS_MACHINE
#ifdef SCHLATHERS_MACHINE 
#define MAXALLOC 1000000000L

// __extension__ unterdrueckt Fehlermeldung wegen geklammerter Argumente
#define INTERNALMSG {		\
    RFERROR4("made to be an internal function '%.50s' ('%.50s', line %d).", \
	     __FUNCTION__, __FILE__, __LINE__);				\
  }

#if ! defined assert
#define assert(X) if (__extension__ (X)) {} else 			\
    RFERROR4("'assert' failed in function '%.50s' (%.50s, line %d) %.200s.", \
	     __FUNCTION__, __FILE__, __LINE__, CONTACT)		   
#endif
#define SHOW_ADDRESSES 1
#define BUG { RFERROR3("BUG in '%.50s' of '%.50s' at line %d.\n",  __FUNCTION__, __FILE__, __LINE__);}

#define MEMCOPY(A,B,C) __extension__ ({ assert((A)!=NULL && (B)!=NULL && (C)>0 && (C)<=MAXALLOC); MEMCOPYX(A,B,C); })
//#define MEMCOPY(A,B,C) memory_copy(A, B, C)
#define CALLOC(X, Y) __extension__({assert((X)>0 && (Y)>0 && ((X) * (Y))<MAXALLOC); CALLOCX(X,Y);})
#define XCALLOC(X, Y) __extension__({assert((X)>0 && (Y)>0 && ((X) * (Y))<MAXALLOC); CALLOCX(X,Y);})
#if ! defined MALLOC
#define MALLOC(X) __extension__ ({assert((X)>0 && (X)<=MAXALLOC); MALLOCX(X);})
#define FREE(X) if ((X) == NULL) {} else {if (!SHOWFREE) {} else PRINTF("free %.50s %ld Line %d %s\n", #X, (Long) X, __LINE__, __FILE__); FREEX(X); (X)=NULL;}
#endif
#define UNCONDFREE(X) { if (!SHOWFREE) {} else PRINTF("(free in %s, line %d)\n", __FILE__, __LINE__); FREEX(X); (X)=NULL;}
#endif // SCHLATHERS_MACHINE


#if defined SCHLATHER_DEBUGGING
#undef MALLOC
#undef CALLOC
#undef XCALLOC
#define MALLOC(X) __extension__({if (!DOPRINT) {} else PRINTF("(MLLC %s, line %d)\n", __FILE__, __LINE__);assert((X)>0 && (X)<=3e9); MALLOCX(X);})
#define CALLOC(X, Y) __extension__({if (!DOPRINT) {} else PRINTF("(CLLC %s, line %d)\n",__FILE__, __LINE__);assert((X)>0 && (Y)>0 && ((X) * (Y)) <MAXALLOC); CALLOCX(X,Y);})
#define XCALLOC(X, Y) __extension__({if (!DOPRINT) {} else PRINTF("(CLLC %s, line %d)\n",__FILE__, __LINE__);assert((X)>0 && (Y)>0 && ((X) * (Y)) <MAXALLOC); CALLOCX(X,Y);})

// note that DEBUGINDOERR is redefined in MachineDebugging.h
#define DEBUGINFOERR {						\
    errorstring_type dummy_; STRCPY(dummy_, WHICH_ERRORSTRING);		\
    SPRINTF(WHICH_ERRORSTRING, "%.50s (%.50s, line %d)\n", dummy_, __FILE__, __LINE__); \
  }
#define DEBUGINFO if (!DOPRINT) {} else PRINTF("(currently at  %s, line %d)\n", __FILE__, __LINE__)

#else
#define DEBUGINFOERR if (PL < PL_ERRORS) {} else PRINTF("error: %s\n", WHICH_ERRORSTRING)
#define DEBUGINFO
#endif


#define PL_IMPORTANT 1 
#define PL_BRANCHING 2
#define PL_DETAILSUSER 3
#define PL_RECURSIVE 4
#define PL_STRUCTURE 5 // see also initNerror.ERROROUTOFMETHOD
#define PL_ERRORS  6 // only those that are caught internally

#define PL_FCTN_DETAILS 7  // R
#define PL_FCTN_SUBDETAILS 8

#define PL_COV_STRUCTURE 7 // C
#define PL_DIRECT_SEQU 8
#define PL_DETAILS 9
#define PL_SUBDETAILS 10



#ifdef SCHLATHERS_MACHINE
#ifndef SHOWFREE
#define SHOWFREE false
#endif
#ifndef DOPRINT
#define DOPRINT true
#endif

#define INITCORES 4
#define DO_TESTS true

#else// not schlather's machine
#define INITCORES 1
#define DO_TESTS false
#endif // not schlather's machine



#ifdef __GNUC__
#define VARIABLE_IS_NOT_USED __attribute__ ((unused))
#else
#define VARIABLE_IS_NOT_USED
#endif


#if __GNUC__ >= 7
#define FALLTHROUGH_OK __attribute__ ((fallthrough))
#else
#define FALLTHROUGH_OK   
#endif


#define UTILSINFO(M) if (!KEYT()->global_utils.basic.helpinfo) {} else PRINTF("%s\n(Note that you can unable this information by 'RFoptions(helpinfo=FALSE)'.)\n", M) // OK


#ifdef DO_PARALLEL
#define HAS_PARALLEL true
#else
#define HAS_PARALLEL false
#endif

#ifdef USEGPU
#define HAS_GPU true
#else
#define HAS_GPU false
#endif

#ifndef GPU_NEEDS  // not a proper installation
#define GPU_NEEDS Inone
#endif				   


#ifdef OBSOLETE_RFU
  #if defined SHOW_ADDRESSES
    #undef SHOW_ADDRESSES
  #endif

  #if ! defined RFU_NEED_OBSOLETE
    #undef FALLTHROUGH_OK
    #undef HAS_PARALLEL
  #endif
  extern int CORES;  // from RF V4 on in extern.h:
  #define LENMSG MAXERRORSTRING
  #define LENERRMSG MAXERRORSTRING
  #define nErrorLoc MAXERRORSTRING
  typedef char errorloc_type[MAXERRORSTRING];
  #define utilsparam utilsoption_type
  #define solve_param solve_options
  #if defined RFdef_H
    #define isGLOBAL NA_INTEGER
  #else
    #define isGLOBAL false
  #endif
  #ifdef _OPENMP
    #ifdef SCHLATHERS_MACHINE
      #define DO_PARALLEL 1
    #else
      #define DO_PARALLEL 1
    #endif
  #else
    #if defined DO_PARALLEL
      #undef DO_PARALLEL
    #endif
  #endif

  // #define LOCAL_MSG char MSG[LENERRMSG]
  #ifdef DO_PARALLEL
    #define LOCAL_ERRMSG2 char MSG2[LENERRMSG]
  #else  // not DO_PARALLEL
    #define LOCAL_ERRMSG2
  #endif

//#if defined ERR
//#undef ERR
//#endif

  #ifndef ERR
    #define ERR ERR0
  #endif

#else // NOT OBSOLETE
  #define USE_FC_LEN_T
  #define ATAN2 std::atan2
  #define COSH std::cosh
  #define SINH std::sinh
  #define TANH std::tanh
#endif




#endif
