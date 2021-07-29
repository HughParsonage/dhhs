#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* FIXME: 
   Check these declarations against the C/Fortran source code.
*/

/* .Call calls */
extern SEXP CDecode_fwalnum(SEXP, SEXP);
extern SEXP CDecode3202(SEXP);
extern SEXP CDetermine_fwalnum(SEXP, SEXP);
extern SEXP CEncode_fwalnum(SEXP, SEXP);
extern SEXP CEncode3202(SEXP);
extern SEXP CEncodeClassificationAcquired(SEXP, SEXP);
extern SEXP CEncodeYN(SEXP);
extern SEXP CValidate_fwalnum(SEXP, SEXP);
extern SEXP CValidate3202(SEXP);
extern SEXP Cwhich_isnt_int(SEXP);

static const R_CallMethodDef CallEntries[] = {
    {"CDecode_fwalnum",               (DL_FUNC) &CDecode_fwalnum,               2},
    {"CDecode3202",                   (DL_FUNC) &CDecode3202,                   1},
    {"CDetermine_fwalnum",            (DL_FUNC) &CDetermine_fwalnum,            2},
    {"CEncode_fwalnum",               (DL_FUNC) &CEncode_fwalnum,               2},
    {"CEncode3202",                   (DL_FUNC) &CEncode3202,                   1},
    {"CEncodeClassificationAcquired", (DL_FUNC) &CEncodeClassificationAcquired, 2},
    {"CEncodeYN",                     (DL_FUNC) &CEncodeYN,                     1},
    {"CValidate_fwalnum",             (DL_FUNC) &CValidate_fwalnum,             2},
    {"CValidate3202",                 (DL_FUNC) &CValidate3202,                 1},
    {"Cwhich_isnt_int",               (DL_FUNC) &Cwhich_isnt_int,               1},
    {NULL, NULL, 0}
};

void R_init_dhhs(DllInfo *dll)
{
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
