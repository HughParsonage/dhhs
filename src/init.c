#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* FIXME: 
   Check these declarations against the C/Fortran source code.
*/

/* .Call calls */
extern SEXP C_const_nchar(SEXP);
extern SEXP C_Decode2(SEXP, SEXP);
extern SEXP C_Encode2(SEXP, SEXP);
extern SEXP C_EncodeDateTime(SEXP, SEXP);
extern SEXP C_isLogical(SEXP);
extern SEXP C_rawBrack(SEXP, SEXP);
extern SEXP C_TabulateIntRaw(SEXP, SEXP);
extern SEXP C_yyyy_mm_dd(SEXP);
extern SEXP C_yyyy_mm_dd_HHMMSS_UTC(SEXP);
extern SEXP CC_Atoi(SEXP);
extern SEXP Ccheck_startsWith202(SEXP);
extern SEXP CClassification_filter(SEXP, SEXP);
extern SEXP CCoalesce0(SEXP, SEXP);
extern SEXP Ccurdle0(SEXP);
extern SEXP CDate2Int(SEXP);
extern SEXP CDecode_fwalnum(SEXP, SEXP);
extern SEXP Cdecode_State(SEXP, SEXP);
extern SEXP CDecode3202(SEXP);
extern SEXP CDetermine_fwalnum(SEXP, SEXP);
extern SEXP CEncode_fwalnum(SEXP, SEXP);
extern SEXP CEncode_RecordID_2109(SEXP);
extern SEXP Cencode_State(SEXP, SEXP);
extern SEXP CEncode3202(SEXP);
extern SEXP CEncodeYN(SEXP);
extern SEXP Cfast_nchar(SEXP);
extern SEXP Cfilter_2raw(SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP CFilter_STE_in(SEXP, SEXP, SEXP);
extern SEXP Cgrepl_09(SEXP);
extern SEXP Cgrepl_ClusterCategory(SEXP, SEXP);
extern SEXP Cgsub_09(SEXP);
extern SEXP CJ_Classification_RecordID_Date(SEXP, SEXP, SEXP, SEXP);
extern SEXP Cmax_nchar(SEXP);
extern SEXP CStandardMobile(SEXP, SEXP, SEXP);
extern SEXP Cuint2dbl(SEXP);
extern SEXP CValidate_fwalnum(SEXP, SEXP);
extern SEXP CValidate3202(SEXP);
extern SEXP Cwhich_isnt_int(SEXP);
extern SEXP isntRecordID2109(SEXP);

static const R_CallMethodDef CallEntries[] = {
    {"C_const_nchar",                   (DL_FUNC) &C_const_nchar,                   1},
    {"C_Decode2",                       (DL_FUNC) &C_Decode2,                       2},
    {"C_Encode2",                       (DL_FUNC) &C_Encode2,                       2},
    {"C_EncodeDateTime",                (DL_FUNC) &C_EncodeDateTime,                2},
    {"C_isLogical",                     (DL_FUNC) &C_isLogical,                     1},
    {"C_rawBrack",                      (DL_FUNC) &C_rawBrack,                      2},
    {"C_TabulateIntRaw",                (DL_FUNC) &C_TabulateIntRaw,                2},
    {"C_yyyy_mm_dd",                    (DL_FUNC) &C_yyyy_mm_dd,                    1},
    {"C_yyyy_mm_dd_HHMMSS_UTC",         (DL_FUNC) &C_yyyy_mm_dd_HHMMSS_UTC,         1},
    {"CC_Atoi",                         (DL_FUNC) &CC_Atoi,                         1},
    {"Ccheck_startsWith202",            (DL_FUNC) &Ccheck_startsWith202,            1},
    {"CClassification_filter",          (DL_FUNC) &CClassification_filter,          2},
    {"CCoalesce0",                      (DL_FUNC) &CCoalesce0,                      2},
    {"Ccurdle0",                        (DL_FUNC) &Ccurdle0,                        1},
    {"CDate2Int",                       (DL_FUNC) &CDate2Int,                       1},
    {"CDecode_fwalnum",                 (DL_FUNC) &CDecode_fwalnum,                 2},
    {"Cdecode_State",                   (DL_FUNC) &Cdecode_State,                   2},
    {"CDecode3202",                     (DL_FUNC) &CDecode3202,                     1},
    {"CDetermine_fwalnum",              (DL_FUNC) &CDetermine_fwalnum,              2},
    {"CEncode_fwalnum",                 (DL_FUNC) &CEncode_fwalnum,                 2},
    {"CEncode_RecordID_2109",           (DL_FUNC) &CEncode_RecordID_2109,           1},
    {"Cencode_State",                   (DL_FUNC) &Cencode_State,                   2},
    {"CEncode3202",                     (DL_FUNC) &CEncode3202,                     1},
    {"CEncodeYN",                       (DL_FUNC) &CEncodeYN,                       1},
    {"Cfast_nchar",                     (DL_FUNC) &Cfast_nchar,                     1},
    {"Cfilter_2raw",                    (DL_FUNC) &Cfilter_2raw,                    5},
    {"CFilter_STE_in",                  (DL_FUNC) &CFilter_STE_in,                  3},
    {"Cgrepl_09",                       (DL_FUNC) &Cgrepl_09,                       1},
    {"Cgrepl_ClusterCategory",          (DL_FUNC) &Cgrepl_ClusterCategory,          2},
    {"Cgsub_09",                        (DL_FUNC) &Cgsub_09,                        1},
    {"CJ_Classification_RecordID_Date", (DL_FUNC) &CJ_Classification_RecordID_Date, 4},
    {"Cmax_nchar",                      (DL_FUNC) &Cmax_nchar,                      1},
    {"CStandardMobile",                 (DL_FUNC) &CStandardMobile,                 3},
    {"Cuint2dbl",                       (DL_FUNC) &Cuint2dbl,                       1},
    {"CValidate_fwalnum",               (DL_FUNC) &CValidate_fwalnum,               2},
    {"CValidate3202",                   (DL_FUNC) &CValidate3202,                   1},
    {"Cwhich_isnt_int",                 (DL_FUNC) &Cwhich_isnt_int,                 1},
    {"isntRecordID2109",                (DL_FUNC) &isntRecordID2109,                1},
    {NULL, NULL, 0}
};

void R_init_dhhs(DllInfo *dll)
{
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
