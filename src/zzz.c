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
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.  
*/


//#include <R_ext/Rdynload.h> 
#include <Rinternals.h>
#include "qmvs.h"

#define none 0
//static R_NativePrimitiveArgTypetworeal[]= { REALSXP, REALSXP };
static const R_CMethodDef cMethods[]  = {
  {NULL, NULL, 0, none}
};

#define CALLDEF_DO(name, n) {#name, (DL_FUNC) &name, n}
static R_CallMethodDef callMethods[]  = {
  // in die respectiven C-Dateien muss adoption.h eingebunden sein
  CALLDEF_DO(queue, 10),
  CALLDEF_DO(queue1, 10),
  CALLDEF_DO(queue2, 10),
  CALLDEF_DO(LqDist, 3),
  {NULL, NULL, 0}
};




#define CALLABLE(FCTN)  R_RegisterCCallable("qmvs", #FCTN, (DL_FUNC)  FCTN)
void R_init_qmvs(DllInfo  *dll) {
  R_registerRoutines(dll, cMethods, // .C
		     callMethods,
		     NULL, // .Fortran
		     NULL); // ext
  R_useDynamicSymbols(dll, FALSE); // OK
}



void R_unload_qmvs(DllInfo *info) {
  // just to avoid warning from compiler on my computer
  /* Release resources. */
}

