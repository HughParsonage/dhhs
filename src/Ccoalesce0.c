#include "dhhs.h"

inline int c0(int x) {
  return x == NA_INTEGER ? 0 : x;
}

SEXP coalesce0int(SEXP x) {
  R_xlen_t N = xlength(x);
  const int * xp = INTEGER(x);
  SEXP ans = PROTECT(allocVector(INTSXP, N));
  int * ansp = INTEGER(ans);
  for (R_xlen_t i = 0; i < N; ++i) {
    ansp[i] = c0(xp[i]);
  }
  UNPROTECT(1);
  return ans;
}

SEXP CCoalesce0(SEXP x) {
  switch(TYPEOF(x)) {
  case INTSXP:
    return coalesce0int(x);
  }
  error("Unsupported type;");
}

SEXP Ccurdle0(SEXP x) {
  if (!isInteger(x)) {
    error("Curdle unsupported type.");
  }
  R_xlen_t N = xlength(x);
  const int * xp = INTEGER(x);
  SEXP ans = PROTECT(allocVector(INTSXP, N));
  int * restrict ansp = INTEGER(ans);
  for (R_xlen_t i = 0; i < N; ++i) {
    ansp[i] = (xp[i] == 0) ? NA_INTEGER : xp[i];
  }
  UNPROTECT(1);
  return ans;
}



