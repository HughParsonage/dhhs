#include "dhhs.h"

// Detects a fixed pattern in a comma-separated string
//   1:                          <NA> NA
//   2:     Accommodation and Housing 25
//   3:                         Other  5
//   4:            Workplace/Industry 18
//   5:                     Education  9
//   6: Hospitality and Entertainment 29
//   7:                 Food Industry 13
//   8:          Hospital/Health care 20
//   9:                     Childcare  9
//  10:         Aged/Residential care 21
//  11:           Disability Services 19
//  12:         Justice and Emergency 21
//  13:                    Laboratory 10

SEXP Cgrepl_ClusterCategory(SEXP x, SEXP Pattern) {
  if (!isString(x)) {
    error("`x` was type '%s' but must be a character vector",
          type2char(TYPEOF(x)));
  }
  if (!isString(Pattern)) {
    error("`Pattern` was type '%s' but must be a character vector",
          type2char(TYPEOF(Pattern)));
  }
  R_xlen_t N = xlength(x);
  int M = length(Pattern);
  if (M > 16) {
    error("Internal error: length(Pattern) > 16.");
  }
  const SEXP * xp = STRING_PTR(x);
  const SEXP * pp = STRING_PTR(Pattern);
  int nchar_patterns[16] = {0};
  for (int ii = 0; ii < M; ++ii) {
    nchar_patterns[ii] = length(pp[ii]);
  }

  SEXP ans = PROTECT(allocVector(LGLSXP, N));
  int * restrict ansp = LOGICAL(ans);
  for (R_xlen_t i = 0; i < N; ++i) {
    if (xp[i] == NA_STRING) {
      ansp[i] = 0;
      continue;
    }
    int n = length(xp[i]);
    const char * xi = CHAR(xp[i]);

    bool found = false;
    for (int j = 0; j < n - 1; ++j) {
      // Only look at start of string or at the first character
      // following a ", "
      if (found) {
        break;
      }
      if (j > 2 && xi[j - 2] != ',' && xi[j - 1] != ' ') {
        continue;
      }

      int n_remaining = n - j;

      for (int k = 0; k < M; ++k) {
        int nk = nchar_patterns[k];
        if (nk > n_remaining) {
          continue;
        }
        const char * pk = CHAR(pp[k]);
        found = true;
        for (int kj = 0; kj < nk; ++kj) {
          if (xi[j + kj] != pk[kj]) {
            found = false;
            break;
          }
        }
      }
    }
    ansp[i] = found;
  }
  UNPROTECT(1);
  return ans;
}
