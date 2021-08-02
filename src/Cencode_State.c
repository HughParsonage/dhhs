#include "dhhs.h"

#define STE_NSW 1
#define STE_VIC 2
#define STE_QLD 3
#define STE_SA  4
#define STE_WA  5
#define STE_TAS 6
#define STE_NT  7
#define STE_ACT 8
#define STE_OT  9

SEXP Cencode_State(SEXP State, SEXP o) {
  R_xlen_t N = xlength(State);
  if (N != xlength(o)) {
    error("Internal error(Cencode_State): xlength(State) != xlength(o)");
  }
  if (!isString(State) || !isInteger(o)) {
    error("State and o were unexpected types.");
  }
  const SEXP * sp = STRING_PTR(State);
  int * op = INTEGER(o);
  for (R_xlen_t i = 0; i < N; ++i) {
    if (op[i]) {
      continue;
    }
    int n = length(sp[i]);
    if (sp[i] == NA_STRING) {
      continue;
    }
    const char * xp = CHAR(sp[i]);
    const char xp0 = toupper(xp[0]);

    int oi = STE_VIC; // Assume Victoria unless evidence suggests otherwise
    op[i] = STE_VIC;
    if (xp0 == 'V') {
      // likely VIC
      continue;
    }
    if (xp0 == 'N') {
      oi = STE_NSW;
      if (n > 1 && (xp[1] == 't' || xp[1] == 'T')) {
        oi = STE_NT;
      }
      op[i] = oi;
      continue;
    }
    if (xp0 == 'Q') {
      op[i] = STE_QLD;
      continue;
    }
    if (n == 2) {
      oi = xp0 == 'S' ? STE_SA : (xp0 == 'W' ? STE_WA : STE_OT);
      op[i] = oi;
      continue;
    }
    if (n == 3 && xp0 == 'U' && xp[1] == 'N' && xp[2] == 'K') {
      op[i] = 0;
      continue;
    }


    if (n >= 3) {
      const char penu = toupper(xp[n - 2]);
      const char last = toupper(xp[n - 1]);
      if (penu == 'I' && last == 'C') {
        continue;
      }
      if (penu == 'S' && last == 'W') {
        op[i] = STE_NSW;
        continue;
      }
    }


  }
  return o;
}

SEXP Cdecode_State(SEXP x, SEXP uSTE) {
  if (TYPEOF(x) != RAWSXP || !isString(uSTE)) {
    error("Internal error(Cdecode_STE): wrong types.");
  }
  if (xlength(uSTE) != 9) {
    error("Internal error: length(uSTE) != 9.");
  }
  R_xlen_t N = xlength(x);
  SEXP ans = PROTECT(allocVector(STRSXP, N));
  const unsigned char * xp = RAW(x);
  for (R_xlen_t i = 0; i < N; ++i) {
    unsigned int xpi = xp[i];
    if (xpi) {
      SET_STRING_ELT(ans, i, STRING_ELT(uSTE, xpi - 1));
    } else {
      SET_STRING_ELT(ans, i, NA_STRING);
    }
  }
  UNPROTECT(1);
  return ans;
}


