#include "dhhs.h"



static void v1(const SEXP * xp, R_xlen_t N,
               const SEXP * yp, R_xlen_t M,
               unsigned int err,
               unsigned char AF09[256]) {
  bool dashes[36] = {0};
  dashes[8] = 1;
  dashes[13] = 1;
  dashes[18] = 1;
  dashes[23] = 1;
  // verify equilength and
  for (R_xlen_t i = 0; i < N; ++i) {
    int nxi = length(xp[i]);
    if (nxi != 36) {
      error("[%u] at position %lld, nxi != 36", err, i + 1);
    }
    const char * xi = CHAR(xp[i]);
    for (int j = 0; j < 36; ++j) {
      unsigned char xij = xi[j];
      if (dashes[j]) {
        continue;
      }
      unsigned int uxij = xij;
      if (!AF09[uxij]) {
        error("[%u] at position %lld, !AF09[uxij]", err, i + 1);
      }
    }

    int nyi = length(yp[i]);
    if (nyi != 36) {
      error("[%u] at position %lld, nxi != 36", err, i + 1);
    }
    const char * yi = CHAR(yp[i]);
    for (int j = 0; j < 36; ++j) {
      unsigned char yij = yi[j];
      if (dashes[j]) {
        continue;
      }
      unsigned int uyij = yij;
      if (!AF09[uyij]) {
        error("[%u] at position %lld, !AF09[uyij]", err, i + 1);
      }
    }
  }
}

static unsigned int af09_8(const char * x, unsigned char AF09[256]) {
  unsigned int o = AF09[((unsigned int)x[0])];
  for (int j = 1; j < 8; ++j) {
    o <<= 4;
    o += AF09[((unsigned int)x[j])];
  }
  return o;
}

SEXP Cmatch_fwid(SEXP xx, SEXP yy, SEXP Verify) {
  if (!isString(xx) || !isString(yy) || !isInteger(Verify)) {
    error("xx and yy must both be character."); // # nocov
  }
  R_xlen_t N = xlength(xx);
  R_xlen_t M = xlength(yy);
  const SEXP * xp = STRING_PTR(xx);
  const SEXP * yp = STRING_PTR(yy);

  unsigned char AF09[256] = {0};

  for (int i = 0, j = 1; i < 256; ++i) {
    unsigned char xi = i;
    if (isdigit(xi) || (xi >= 'A' && xi <= 'F')) {
      AF09[i] = j++;
    }
  }

  const int verify = asInteger(Verify);
  switch(verify) {
  case 1:
    v1(xp, N, yp, M, 101u, AF09);
    break;
  }
  if (M >= INT_MAX) {
    error("Long vectors not supported yet.");
  }

  // First 8 characters form a 32 bit integer
  unsigned int * uy = malloc(sizeof(int) * M);
  if (uy == NULL) {
    free(uy);
    error("uy could not be malloc'd");
  }
  for (R_xlen_t i = 0; i < M; ++i) {
    unsigned int oi = 0;
    const char * yi = CHAR(yp[i]);
    uy[i] = af09_8(yi, AF09);
  }

  int c0123[4096] = {0};

  for (int j = 1, k = 1; j < M; ++j) {
    unsigned int oi = 0;
    const char * yi = CHAR(yp[j - 1]);
    const char * yj = CHAR(yp[j]);
    c0123[k] = j;
    k += (yi[0] != yj[0]) && (yi[1] != yj[1]) && (yi[2] != yj[2]) && (yi[3] != yj[3]);
  }

  SEXP ans = PROTECT(allocVector(INTSXP, N));
  int * restrict ansp = INTEGER(ans);

  for (R_xlen_t i = 0; i < N; ++i) {
    ansp[i] = 0;
    const char * xi = CHAR(xp[i]);
    unsigned int uy_xi = af09_8(xi, AF09);

    for (R_xlen_t j = 0; j < M; ++j) {
      if (uy_xi == uy[j]) {
        ansp[i] = j + 1;
        break;
      }
    }
  }
  free(uy);
  UNPROTECT(1);
  return ans;

}
