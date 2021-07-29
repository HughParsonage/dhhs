#include "dhhs.h"

SEXP CEncodeClassificationAcquired(SEXP Classification, SEXP Acquired) {
  R_xlen_t N = xlength(Classification);
  if (N != xlength(Acquired)) {
    error("`xlength(Classification) = %llu`, but xlength(Acquired) = %llu ", N, xlength(Acquired));
  }
  if (!isString(Classification) || !isString(Acquired)) {
    error("`Classification` was type '%s' and Acquired was type '%s' but both must be character vectors.",
          type2char(TYPEOF(Classification)), type2char(TYPEOF(Acquired)));
  }

  const SEXP * cp = STRING_PTR(Classification);
  const SEXP * ap = STRING_PTR(Acquired);
  SEXP ans = PROTECT(allocVector(INTSXP, N));
  int * restrict ansp = INTEGER(ans);

  for (R_xlen_t i = 0; i < N; ++i) {
    int nci = length(cp[i]);
    int nai = length(ap[i]);

    const char * cpi = CHAR(cp[i]);
    const char * api = CHAR(ap[i]);
    // create two integers based on the (likely) value of acquired and classification
    unsigned int oi = nai == 2;
    switch(api[0]) {
    case 'T':
      oi = 2; // Travel overseas
      break;
    case 'C': // Contact with a confirmed case
      oi = 3;
      break;
    case 'A': // Acquired in Australia, unknown source
      oi = 4;
      break;
    case 'U': // Under investigation
      oi = 5;
      break;
    }

    /*
     *
      *              uClassifications s  n
      1:          Acquisition contact A 19
      2:               Casual contact C 14
      3:                    Confirmed C  9
      4:             Contact - active C 16
      5:                   Historical H 10
      6:               Not notifiable N 14
      7:                     Probable P  8
      8:                     Rejected R  8
      9:        Rejected - no testing R 21
     10:       Rejected after testing R 22
     11: Rejected - contact > 14 days R 28
     12:   Secondary contact - active S 26
     13: Secondary contact - rejected S 28
     */
    unsigned int oic = 0;
    switch(nci) {
    case 9:
      oic = 3; // Confirmed
      break;
    case 19:
      oic = 1;
      break;
    case 14:
      switch(cpi[0]) {
      case 'C':
        oic = 2;
        break;
      case 'N':
        oic = 6;
        break;
      }
      break;
    case 16:
      oic = 4;
      break;
    case 10:
      oic = 5;
      break;
    case 8:
      oic = 7 + (cpi[0] == 'R');
      break;
    case 21:
      oic = 9;
      break;
    case 22:
      oic = 10;
      break;
    case 28:
      oic = 11 + 2 * (cpi[0] == 'S');
      break;
    case 26:
      oic = 12;
      break;
    }

    oi <<= 16;
    oi += oic;
    ansp[i] = oi;
  }
  UNPROTECT(1);
  return ans;
}



