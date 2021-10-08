#include "dhhs.h"

SEXP C_CliniSevi2raw(SEXP Clini, SEXP Sevi) {
  if (isntEquiInt(Clini, Sevi)) {
    error("Internal error(C_CliniSevi2raw)[%d]", isntEquiInt(Clini, Sevi));
  }
  R_xlen_t N = xlength(Clini);
  const int * xp = INTEGER(Clini);
  const int * yp = INTEGER(Sevi);
  SEXP ans = PROTECT(allocVector(RAWSXP, N));
  unsigned char * ansp = RAW(ans);
  for (R_xlen_t i = 0; i < N; ++i) {
    unsigned int xpi = xp[i];
    unsigned int ypi = yp[i];
    xpi <<= 3;
    xpi += ypi;
    ansp[i] = xpi;
  }
  UNPROTECT(1);
  return ans;
}

SEXP filter_xraw(SEXP x, SEXP table) {
  R_xlen_t N = xlength(x);
  int M = length(table);
  const unsigned char * xp = RAW(x);
  const unsigned char * tp = RAW(table);

  SEXP ans = PROTECT(allocVector(LGLSXP, N));
  int * restrict ansp = LOGICAL(ans);
  for (R_xlen_t i = 0; i < N; ++i) {
    bool o = false;
    unsigned char xpi = xp[i];
    for (int j = 0; j < M; ++j) {
      unsigned char tpj = tp[j];
      if (xpi == tpj) {
        o = true;
        break;
      }
    }
    ansp[i] = o;
  }
  UNPROTECT(1);
  return ans;

}

SEXP Cfilter_2raw(SEXP x, SEXP y,
                  SEXP tablex, SEXP tabley,
                  SEXP And) {

  if (xlength(y) == 0 || xlength(tabley) == 0) {
    if (isntRaw(x) || isntRaw(tablex)) {
      error("Internal error(Cfilter_2raw): non-raw input");
    }
    return filter_xraw(x, tablex);
  }
  if (xlength(x) == 0 || xlength(tablex) == 0) {
    if (isntRaw(y) || isntRaw(tabley)) {
      error("Internal error(Cfilter_2raw): non-raw input");
    }
    return filter_xraw(y, tabley);
  }
  if (isntEquiRaw(x, y)) {
    error("Internal error(Cfilter_2raw[%d]):", isntEquiRaw(x, y));
  }
  if (isntRaw(tablex) || isntRaw(tabley)) {
    error("Internal error(Cfilter_2raw[%d]):", 10 + isntEquiRaw(tablex, tabley));
  }
  if (xlength(tablex) > INT_MAX ||
      xlength(tabley) > INT_MAX) {
    error("Internal error(Cfilter_ClinicalStatus_Severity): long vectors not supported in tables.");
  }
  R_xlen_t N = xlength(x);
  int Mx = length(tablex);
  int My = length(tabley);
  const bool et = asLogical(And);
  const unsigned char * xp = RAW(x);
  const unsigned char * yp = RAW(y);
  const unsigned char * txp = RAW(tablex);
  const unsigned char * typ = RAW(tabley);

  const unsigned char tx0 = txp[0];
  const unsigned char ty0 = typ[0];

  SEXP ans = PROTECT(allocVector(LGLSXP, N));
  int * restrict ansp = LOGICAL(ans);
  if (et) {
    for (R_xlen_t i = 0; i < N; ++i) {
      unsigned char xpi = xp[i];
      unsigned char ypi = yp[i];
      bool o = xpi == tx0 && ypi == ty0; // most common case (1,1)
      if (o) {
        ansp[i] = 1;
        continue;
      }
      for (int j = 0; j < Mx; ++j) {
        unsigned char txj = txp[j];
        if (txj == xpi) {
          o = true;
          break;
        }
      }
      if (!o) {
        ansp[i] = 0;
        continue;
      }
      o = false;
      for (int k = 0; k < My; ++k) {
        unsigned char tyk = typ[k];
        if (tyk == ypi) {
          o = true;
          break;
        }
      }
      ansp[i] = o;
    }
  } else {
    for (R_xlen_t i = 0; i < N; ++i) {
      unsigned char xpi = xp[i];
      unsigned char ypi = yp[i];
      bool o = xpi == tx0 || ypi == ty0; // most common case (1,1)
      if (o) {
        ansp[i] = 1;
        continue;
      }
      for (int j = 0; j < Mx; ++j) {
        unsigned char txj = txp[j];
        if (txj == xpi) {
          o = true;
          break;
        }
      }
      if (o) {
        ansp[i] = 1;
        continue;
      }
      for (int k = 0; k < My; ++k) {
        unsigned char tyk = typ[k];
        if (tyk == ypi) {
          o = true;
          break;
        }
      }
      ansp[i] = o;
    }
  }
  UNPROTECT(1);
  return ans;
}

SEXP Cwhere_ClinicalStatus_Severity(SEXP ez, SEXP tx, SEXP ty, SEXP And) {

  R_xlen_t N = xlength(ez);
  int Mx = length(tx);
  int My = length(ty);
  if (isntRaw(ez) || isntRawOrNull(tx) || isntRawOrNull(ty)) {
    error("(Cwhere_ClinicalStatus_Severity): Expected raw vectors.");
  }
  const bool et = asLogical(And);
  SEXP ans = PROTECT(allocVector(LGLSXP, N));
  int * restrict ansp = LOGICAL(ans);
  if (TYPEOF(tx) == NILSXP && TYPEOF(ty) == NILSXP) {
    // Unusual
    // all TRUE
    int * restrict ansp = LOGICAL(ans);
    for (R_xlen_t i = 0; i < N; ++i) {
      ansp[i] = 1;
    }
    UNPROTECT(1);
    return ans;
  }
  const unsigned char * xp = RAW(ez);




  if (TYPEOF(tx) == NILSXP) {
    const unsigned char * typ = RAW(ty);
    // All x will be valid
    for (R_xlen_t i = 0; i < N; ++i) {
      unsigned char xpi = xp[i];
      unsigned char csi = xpi & 7;
      bool o = false;
      for (int j = 0; j < My; ++j) {
        if (csi == typ[j]) {
          o = true;
          break;
        }
      }
      ansp[i] = o;
    }
    UNPROTECT(1);
    return ans;
  }
  if (TYPEOF(ty) == NILSXP) {
    const unsigned char * txp = RAW(tx);
    // All x will be valid
    for (R_xlen_t i = 0; i < N; ++i) {
      unsigned char xpi = xp[i];
      unsigned char csi = xpi >> 3;
      bool o = false;
      for (int j = 0; j < Mx; ++j) {
        if (csi == txp[j]) {
          o = true;
          break;
        }
      }
      ansp[i] = o;
    }
    UNPROTECT(1);
    return ans;
  }

  const unsigned char * txp = RAW(tx);
  const unsigned char * typ = RAW(ty);
  for (R_xlen_t i = 0; i < N; ++i) {
    unsigned int xpi = xp[i];
    unsigned int csi = xpi >> 3;
    unsigned int ssi = xpi & 7;
    bool o = false;
    for (int j = 0; j < Mx; ++j) {
      if (o) {
        break;
      }
      unsigned char txpj = txp[j];
      if (csi == txpj) {
        if (!et) {
          o = true;
          break;
        }
      } else {
        if (et) {
          continue;
        }
      }

      for (int k = 0; k < My; ++k) {
        unsigned char typk = typ[k];
        if (ssi == typk) {
          o = true;
          break;
        }
      }
    }
    ansp[i] = o;

  }
  UNPROTECT(1);
  return ans;
}
