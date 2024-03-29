#include "dhhs.h"

SEXP CEncodeYN(SEXP xx) {
  if (!isString(xx)) {
    error("x was type '%s' but must be a character vector.", type2char(TYPEOF(xx)));
  }
  R_xlen_t N = xlength(xx);
  SEXP ans = PROTECT(allocVector(LGLSXP, N));
  int * restrict ansp = INTEGER(ans);
  for (R_xlen_t i = 0; i < N; ++i) {
    ansp[i] = NA_LOGICAL;
    SEXP CX = STRING_ELT(xx, i);
    if (CX == NA_STRING) {
      continue;
    }
    int n = length(CX);
    if (n == 0 || n > 3) {
      continue;
    }
    const char * xi = CHAR(CX);
    const char x0 = xi[0];
    if (n == 1) {
      if (x0 == 'Y' || x0 == 'y') {
        ansp[i] = TRUE;
        continue;
      }
      if (x0 == 'N' || x0 == 'n') {
        ansp[i] = FALSE;
      }
      continue;
    }
    if (n == 2 &&
        (x0 == 'N' || x0 == 'n') &&
        (xi[1] == 'o' || xi[1] == 'O')) {
      ansp[i] = FALSE;
      continue;
    }

    if (n == 3) {
      char x1 = xi[1];
      char x2 = xi[2];
      if ((x0 == 'Y' || x0 == 'y') &&
          (x1 == 'e' || x1 == 'E') &&
          (x2 == 's' || x2 == 'S')) {
        ansp[i] = TRUE;
      }
    }
  }
  UNPROTECT(1);
  return ans;
}

SEXP C_isLogical(SEXP x) {
  if (isLogical(x)) {
    return ScalarLogical(1);
  }
  if (!isInteger(x) && !isReal(x)) {
    return ScalarLogical(0);
  }
  R_xlen_t N = xlength(x);
  if (TYPEOF(x) == INTSXP) {
    const int * xp = INTEGER(x);
    for (R_xlen_t i = 0; i < N; ++i) {
      int xpi = xp[i];
      if (xpi == 0 || xpi == 1 || xpi == NA_INTEGER) {
        continue;
      }
      return ScalarLogical(0);
    }
  } else {
    const double * xp = REAL(x);
    for (R_xlen_t i = 0; i < N; ++i) {
      double xpi = xp[i];
      if (ISNAN(xpi) || xpi == 0 || xpi == 1) {
        continue;
      }
      return ScalarLogical(0);
    }
  }

  return ScalarLogical(1);
}


