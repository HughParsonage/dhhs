#include "dhhs.h"

SEXP logicalN(R_xlen_t N, int a) {
  SEXP ans = PROTECT(allocVector(LGLSXP, N));
  int * restrict ansp = LOGICAL(ans);
  for (R_xlen_t i = 0; i < N; ++i) {
    ansp[i] = a;
  }
  UNPROTECT(1);
  return ans;
}


