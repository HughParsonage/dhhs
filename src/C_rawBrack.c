#include "dhhs.h"

// expands xx[ii] as if ii were integer, but 0 correctly mapped to NA
// and more generally x[ii] is always the length of ii (since NA and
// non-positive values in ii are not possible).

SEXP C_rawBrack(SEXP xx, SEXP ii) {
  // # nocov start
  if (!isString(xx) || isntRaw(ii)) {
    error("`x` was type '%s' and `i` was type '%s' but must be character and raw respectively.",
          type2char(TYPEOF(xx)), type2char(TYPEOF(ii)));
  }
  if (length(xx) == 0 || length(xx) > 255) {
    error("length(xx) = %d but cannot exceed 255.", length(xx));
  }
  // # nocov end
  const SEXP * xp = STRING_PTR(xx);
  const unsigned char * ip = RAW(ii);
  R_xlen_t N = xlength(ii);
  SEXP ans = PROTECT(allocVector(STRSXP, N));

  for (R_xlen_t i = 0; i < N; ++i) {
    unsigned int ipi = ip[i];
    if (ipi == 0) {
      SET_STRING_ELT(ans, i, NA_STRING);
      continue;
    }
    SET_STRING_ELT(ans, i, xp[ipi - 1]);
  }
  UNPROTECT(1);
  return ans;
}


