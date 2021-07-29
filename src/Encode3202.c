#include "dhhs.h"

bool all_digits_4_12(const char * xi) {
  for (int j = 4; j < 12; ++j) {
    char xj = xi[j];
    if (!char_is_number(xj)) {
      return false;
    }
  }
  return true;
}

SEXP CValidate3202(SEXP x) {
  R_xlen_t N = xlength(x);
  int typeofx = TYPEOF(x);
  if (typeofx != STRSXP) {
    error("x is not a character.");
  }
  for (R_xlen_t i = 0; i < N; ++i) {
    SEXP CX = STRING_ELT(x, i);
    if (CX == NA_STRING) {
      continue;
    }
    int nchari = length(CX);
    const char * xi = CHAR(CX);

    if (nchari > 10) {
      if (nchari != 12) {
        error("PHESSID contains nchar(x[i]) > 10");
      }
      bool starts_with_3202 =
        (xi[0] == '3' && xi[1] == '2' && xi[2] == '0' && xi[3] >= '0' && xi[3] <= '9');
      if (!starts_with_3202 || !all_digits_4_12(xi)) {
        error("PHESSID contains nchar(xi) == 10 but not starting with 3202 or otherwise not digit.");
      }
    } else {
      if (!all_digits(xi, nchari)) {
        error("PHESSID contains non-digits");
      }
    }
  }
  SEXP ans = PROTECT(allocVector(LGLSXP, 1));
  LOGICAL(ans)[0] = TRUE;
  UNPROTECT(1);
  return ans;
}

SEXP CEncode3202(SEXP x) {
  R_xlen_t N = xlength(x);
  if (TYPEOF(x) != STRSXP) {
    error("x is not a character.");
  }
  SEXP ans = PROTECT(allocVector(INTSXP, N));
  int *restrict ansp = INTEGER(ans);

  for (R_xlen_t i = 0; i < N; ++i) {
    if (STRING_ELT(x, i) == NA_STRING) {
      ansp[i] = NA_INTEGER;
      continue;
    }
    const char * xi = CHAR(STRING_ELT(x, i));
    size_t nchari = strlen(xi);
    bool starts_with_3202 = false;
    if (nchari > 10) {
      if (nchari != 12) {
        error("PHESSID contains nchar(x[i]) > 10");
      }
      starts_with_3202 =
        (xi[0] == '3' && xi[1] == '2' && xi[2] == '0' && xi[3] >= '0' && xi[3] <= '9');
      if (!starts_with_3202 || !all_digits_4_12(xi)) {
        error("PHESSID contains nchar(xi) == 10 but not starting with 3202 or otherwise not digit.");
      }
    } else {
      if (!all_digits(xi, nchari)) {
        error("PHESSID contains non-digits");
      }
    }
    int o = 0;
    if (starts_with_3202) {
      o = char12_to_int(xi);
    } else {
      o = char2int(xi, nchari);
    }
    // for not starting with '3202' we still want
    // to preserve the order so we subtract a large
    // number from it.  We have established that
    // every number not starting with 3202 is no
    // larger than 1e9 (10 digits) and is nonnegative.
    ansp[i] = starts_with_3202 ? o : (o - 1e9);
  }
  UNPROTECT(1);
  return ans;
}

char* dig3202(int i, char b[]) {
  char const digit[] = "0123456789";
  char *p = b;
  bool pad0 = i >= 0;
  if (pad0) {
    *p++ = '3';
    *p++ = '2';
    *p++ = '0';
    *p++ = '2';
    for (int z = 4; z < 12; ++z) {
      *p++ = '0';
    }
  } else {
    i += (i <= 0) * 1e9;
    int shifter = i;
    do {
      ++p;
      shifter /= 10;
    } while (shifter);
  }
  *p = '\0';
  do {
    *--p = digit[i % 10];
    i /= 10;
  } while (i);
  return b;
}


SEXP CDecode3202(SEXP x) {
  R_xlen_t N = xlength(x);
  int typeofx = TYPEOF(x);
  if (typeofx != INTSXP) {
    error("x is not a integer.");
  }
  const int *xp = INTEGER(x);
  SEXP ans = PROTECT(allocVector(STRSXP, N));

  for (R_xlen_t i = 0; i < N; ++i) {
    if (xp[i] == NA_INTEGER) {
      SET_STRING_ELT(ans, i, NA_STRING);
      continue;
    }
    int nd = xp[i] <= 0 ? n_digits0(xp[i] + 1e9) : 13;
    char digits11[nd];
    char *oip = dig3202(xp[i], digits11);
    const char *coip = oip;
    SET_STRING_ELT(ans, i, mkCharCE(coip, CE_UTF8));
  }
  UNPROTECT(1);
  return ans;
}
