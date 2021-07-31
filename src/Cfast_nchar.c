#include "dhhs.h"

int int_log2(uint32_t x) { return 31 - __builtin_clz(x|1); }

int fast_digit_count(uint32_t x) {
  static uint64_t table[] = {
    4294967296,  8589934582,  8589934582,  8589934582,  12884901788,
    12884901788, 12884901788, 17179868184, 17179868184, 17179868184,
    21474826480, 21474826480, 21474826480, 21474826480, 25769703776,
    25769703776, 25769703776, 30063771072, 30063771072, 30063771072,
    34349738368, 34349738368, 34349738368, 34349738368, 38554705664,
    38554705664, 38554705664, 41949672960, 41949672960, 41949672960,
    42949672960, 42949672960};
  return (x + table[int_log2(x)]) >> 32;
}


SEXP Cfast_nchar(SEXP x) {
  if (!isInteger(x) && !isString(x)) {
    return R_NilValue;
  }
  R_xlen_t N = xlength(x);

  SEXP ans = PROTECT(allocVector(INTSXP, N));
  int * restrict ansp = INTEGER(ans);
  switch(TYPEOF(x)) {
  case INTSXP: {
    const int * xp = INTEGER(x);
    for (R_xlen_t i = 0; i < N; ++i) {
      ansp[i] = fast_digit_count(xp[i]);
    }
  }
    break;
  case STRSXP: {
    const SEXP * xp = STRING_PTR(x);
    for (R_xlen_t i = 0; i < N; ++i) {
      ansp[i] = length(xp[i]);
    }
  }
    break;
  }
  UNPROTECT(1);
  return ans;
}
