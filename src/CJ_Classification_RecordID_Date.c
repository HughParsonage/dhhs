#include "dhhs.h"

SEXP CJ_Classification_RecordID_Date(SEXP ER, SEXP ED, SEXP EC, SEXP DateRange) {
  if (isntEquiInt(ER, ED)) {
    error("isntEquiInt: ER ED");
  }
  if (isntRaw(EC)) {
    error("EC was type 's' must be raw.");
  }

  if (isntEquiInt(ER, ED) || isntRaw(EC) || xlength(EC) != xlength(ER)) {
    error("XX");
  }
  if (!isInteger(DateRange) || xlength(DateRange) != 2) {
    error("Internal error: DateRange must be length-2 for integer.");
  }


  R_xlen_t N = xlength(ER);

  const int * er = INTEGER(ER);
  const int * ed = INTEGER(ED);
  const unsigned char * ec = RAW(EC);

  const int date0 = INTEGER_ELT(DateRange, 0);
  const int date1 = INTEGER_ELT(DateRange, 1);
  const int ndates = (date1 + 1u) - date0;

  Rprintf("Dates:\n");
  Rprintf("\t %d, %d, %d\n", date0, date1, ndates);

  R_xlen_t nRecord = 1;
  for (R_xlen_t i = 1; i < N; ++i) {
    nRecord += er[i] != er[i - 1];
  }
  R_xlen_t M = nRecord * ndates;

  Rprintf("nRecord = %lld \t=>\t M = %lld\n", nRecord, M);

  SEXP ans1 = PROTECT(allocVector(INTSXP, M));
  SEXP ans2 = PROTECT(allocVector(INTSXP, M));
  SEXP ans3 = PROTECT(allocVector(RAWSXP, M));

  int * restrict ans1p = INTEGER(ans1);
  int * restrict ans2p = INTEGER(ans2);
  unsigned char * restrict ans3p = RAW(ans3);
  memset(ans3p, 0, M);

  ans1p[0] = er[0];
  ans2p[0] = date0;
  ans3p[0] = ed[0] == date0 ? ec[0] : 0;

  // Loop through each record,date
  // when we reach a new record, we can
  // easily determine the next `ndates` rows.
  // The dates are just date0:date1
  // The records are just the current record
  // The first classification is 0 unless the
  // first date is date0.
  // The d-th classification is the previous
  // classification until the current date

  R_xlen_t RecordRank = 0;
  for (R_xlen_t i = 0; i < N; ++i) {
    int eri = er[i];
    int edi = ed[i];
    int jedi = edi - date0;
    unsigned char eci = ec[i];
    bool new_record = i == 0 || er[i] != er[i - 1];
    bool last_record = i == (N - 1) || (er[i] != er[i + 1]);
    R_xlen_t k0 = RecordRank * ndates;
    if (new_record) {
      for (int j = 0; j < ndates; ++j) {
        ans1p[k0 + j] = eri;
        ans2p[k0 + j] = date0 + j;
      }
    }

    int j0 = jedi;
    int j1 = (last_record ? date1 : ed[i + 1]) - date0;

    for (int j = j0; j <= j1; ++j) {
      ans3p[k0 + j] = eci;
    }
    RecordRank += new_record;
  }


  SEXP Ans = PROTECT(allocVector(VECSXP, 3));
  SET_VECTOR_ELT(Ans, 0, ans1);
  SET_VECTOR_ELT(Ans, 1, ans2);
  SET_VECTOR_ELT(Ans, 2, ans3);
  UNPROTECT(4);
  return Ans;
}

SEXP C_TabulateIntRaw(SEXP X, SEXP Y) {
  if (isntEquiInt(X, X) || isntRaw(Y)) {
    error("Wrong Types.");
  }
  R_xlen_t N = xlength(X);
  if (xlength(Y) != N) {
    error("xlength(Y) != N");
  }
  const int * xp = INTEGER(X);
  const unsigned char * yp = RAW(Y);
  const int minx = xp[0];
  const R_xlen_t maxx = xp[N - 1];
  const R_xlen_t unxx = maxx - minx + 1;

  R_xlen_t M = unxx * 256;
  SEXP ans = PROTECT(allocVector(INTSXP, M));
  int * restrict ansp = INTEGER(ans);
  for (R_xlen_t i = 0; i < M; ++i) {
    ansp[i] = 0;
  }
  for (R_xlen_t i = 0; i < N; ++i) {
    int xpi = xp[i];
    unsigned int jpi = xpi - minx;
    jpi <<= 8;
    unsigned char ypi = yp[i];
    unsigned int uypi = ypi;
    jpi += uypi;
    ansp[jpi] += 1;
  }
  UNPROTECT(1);
  return ans;
}


