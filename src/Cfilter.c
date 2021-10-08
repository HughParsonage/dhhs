#include "dhhs.h"

SEXP Cfilter_in(SEXP x, SEXP y, SEXP Not) {
  if (isntRaw(x) || isntRaw(y)) {
    error("Internal error(Cfilter_in): x or y not RAWSXP."); // # nocov
  }

  R_xlen_t N = xlength(x);
  int m = length(y);
  if (m > 255) {
    error("Internal error(Cfilter_in): m > 255 (likely duplicated elements to y)");
  }
  const bool no = asLogical(Not);
  const unsigned char * yp = RAW(y);
  // just lookup each value
  bool w[256] = {no ? 1 : 0};
  for (int j = 0; j < m; ++j) {
    unsigned int ypj = yp[j];
    w[ypj] = no ? 0 : 1;
  }
  const unsigned char * xp = RAW(x);
  SEXP ans = PROTECT(allocVector(LGLSXP, N));
  int * restrict ansp = LOGICAL(ans);
  for (R_xlen_t i = 0; i < N; ++i) {
    unsigned int xpi = xp[i];
    ansp[i] = w[xpi];
  }
  UNPROTECT(1);
  return ans;
}

