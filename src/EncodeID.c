#include "dhhs.h"

unsigned int alphnum2uint(char x) {
  if (x < '0' || x > 'z') {
    return 0U;
  }
  if (x <= '9') {
    return x - '0';
  }
  if (x <= 'Z') {
    return 10U + (x - 'A');
  }
  return 10U + 26U + (x - 'a');
}

SEXP CDetermine_fwalnum(SEXP x, SEXP MaxNchar) {
  if (TYPEOF(x) != STRSXP || TYPEOF(MaxNchar) != INTSXP) {
    error("Internal error(CDetermine_fwalnum): wrong types.");
  }
  R_xlen_t N = xlength(x);
  if (N == 0) {
    return R_NilValue;
  }
  unsigned char * not_na = malloc(sizeof(char) * N);
  for (R_xlen_t i = 0; i < N; ++i) {
    not_na[i] = STRING_ELT(x, i) != NA_STRING;
  }


  int max_nchar_tmp = 1;
  R_xlen_t first_non_na = 0;
  if (TYPEOF(MaxNchar) != INTSXP || INTEGER(MaxNchar)[0] == NA_INTEGER) {
    bool auto_maxnchar = TYPEOF(MaxNchar) == NILSXP;
    auto_maxnchar |= TYPEOF(MaxNchar) == INTSXP && INTEGER(MaxNchar)[0] == NA_INTEGER;
    if (auto_maxnchar) {
      // User has requested max_nchar to be based of first non-NA string
      max_nchar_tmp = length(STRING_ELT(x, first_non_na));
      while (first_non_na < N &&
             max_nchar_tmp == 2 &&
             STRING_ELT(x, first_non_na) == NA_STRING) {
        ++first_non_na;
        max_nchar_tmp = length(STRING_ELT(x, first_non_na));
      }
      if (first_non_na == N) {
        error("`n = NULL`, but x is full of NA.");
      }
    } else if (TYPEOF(MaxNchar) == REALSXP) {
      max_nchar_tmp = (unsigned int)asReal(MaxNchar);
    } else {
      error("MaxNChar wrong type.");
    }
  } else {
    max_nchar_tmp = asInteger(MaxNchar);
  }
  const int max_nchar = max_nchar_tmp;
  if (max_nchar < 0) {
    error("max_nchar is negative (possible NA).");
  }
  unsigned int any_nchar_ge = 0;
  unsigned int any_nchar_le = 0;
  // tbl[62*j + k] is 1 if string[k] is present at position j
  unsigned char * tbl = calloc(max_nchar * 62, sizeof(char));
  if (tbl == NULL) {
    error("(Cdetermine_const_width_alnum_encoding): Unable to allocate tbl.");
  }
  for (R_xlen_t i = 0; i < N; ++i) {
    if (STRING_ELT(x, i) == NA_STRING) {
      continue;
    }

    const char * xi = CHAR(STRING_ELT(x, i));
    unsigned int strleni = LENGTH(STRING_ELT(x, i));
    unsigned int base_tbl_j = 0;
    if (strleni == max_nchar) {
      for (unsigned int c = 0; c < max_nchar; ++c) {
        unsigned int tbl_i_a = alphnum2uint(xi[c]);
        tbl[base_tbl_j + tbl_i_a] = 1;
        base_tbl_j += 62U;
      }
    } else if (strleni < max_nchar) {
      any_nchar_le = i;
    } else {
      any_nchar_ge = i;
    }
  }
  if (any_nchar_le) {
    warning("Element %d had strings narrower than max_nchar.", any_nchar_le);
  }
  if (any_nchar_ge) {
    warning("Element %d had strings wider than max_nchar.", any_nchar_ge);
  }

  SEXP ans = PROTECT(allocVector(STRSXP, max_nchar));
  int c = 0;
  char string[] = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz";

  for (int j = 0; j < max_nchar; ++j) {
    unsigned int the_len = 1; // 1 for null terminator
    // the_len is the number of unique characters at position j throughout x
    for (unsigned int cc = 0; cc < 62; ++cc) {
      the_len += tbl[62 * j + cc];
    }
    char ansi[the_len];

    int k = 0; // position of ansi
    for (int cc = 0; cc < 62; ++cc, ++c) {
      unsigned char tbl_cc = tbl[62 * j + cc];
      if (tbl_cc) {
        ansi[k] = string[cc];
        ++k;
      }
    }
    ansi[the_len - 1] = '\0';
    const char * ansic = (const char *) ansi;
    SET_STRING_ELT(ans, j, mkCharCE(ansic, CE_UTF8));
  }

  free(tbl);
  UNPROTECT(1);
  return ans;
}

SEXP CValidate_fwalnum(SEXP x, SEXP ee) {
  if (TYPEOF(x) != STRSXP || TYPEOF(ee) != STRSXP || xlength(ee) >= INT_MAX) {
    error("Internal error(Cvalidate_encoding): wrong input types.");
  }
  R_xlen_t N = xlength(x);
  const int max_nchar = xlength(ee);
  // check:
  // each element of x has max_nchar elements;
  // each element has only the characters described at each position
  // (it's okay if not all characters are there)
  // Return 1-based index if invalid, 0 otherwise

  // Reconstruct tbl from elements
  unsigned char * tbl = calloc(max_nchar * 62, sizeof(char));
  if (tbl == NULL) {
    error("(Cvalidate_encoding): Unable to allocate tbl.");
  }

  // memoize
  // alphnum2uint
  unsigned int malphnum2uint[256] = {0};
  char string[] = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz";
  for (int i = 0; i < 62; ++i) {
    char si = string[i];
    unsigned int ui = alphnum2uint(si);
    unsigned char sui = (unsigned char)si;
    malphnum2uint[sui] = ui;
  }

  for (int j = 0; j < max_nchar; ++j) {
    // populate tbl
    // It is not faster if some positions are constant or unrestricted
    // to special these cases [malphnum2uint is too heroic]

    const char * ee_j = CHAR(STRING_ELT(ee, j));
    unsigned int strlenj = strlen(ee_j);

    for (int k = 0; k < strlenj; ++k) {
      // ee_j matches
      unsigned int ee_jk = ee_j[k];
      unsigned int dig = malphnum2uint[ee_jk];
      tbl[62 * j + dig] = 1;
    }
  }
  R_xlen_t o = 0;
  for (R_xlen_t i = 0; i < N; ++i) {
    // Don't check for NA -- faster to check at R level.
    const char * xi = CHAR(STRING_ELT(x, i));
    if (strlen(xi) != max_nchar) {
      o = i + 1;
      break;
    }

    for (unsigned int j = 0; j < max_nchar; ++j) {
      // any non alphanum character is valid
      unsigned char xij = xi[j];
      unsigned int k = malphnum2uint[xij];
      unsigned int tk = 62U * j + k;
      if (!tbl[tk]) {
        // if table hasn't been populated, then this character should not
        // be present.
        o = i + 1;
      }
    }

    if (o) {
      break;
    }
  }
  free(tbl);
  return o < INT_MAX ? ScalarInteger(o) : ScalarReal(o);
}

SEXP CEncode_fwalnum(SEXP x, SEXP ee) {
  if (TYPEOF(x) != STRSXP || TYPEOF(ee) != STRSXP || xlength(ee) >= INT_MAX) {
    error("Internal error(Calphnum_enc): wrong input types.");
  }
  R_xlen_t N = xlength(x);
  const int max_nchar = xlength(ee);

  // constant columns can be ignored
  bool char_non_const[max_nchar];
  int non_const = 0;
  for (int j = 0; j < max_nchar; ++j) {
    unsigned int j_len = length(STRING_ELT(ee, j));
    bool j_const = j_len != 1;
    char_non_const[j] = j_const;
    non_const += j_const;
  }
  unsigned int * J = malloc(sizeof(int) * non_const);
  if (J == NULL) {
    error("(Calphnum_enc)Unable to allocate J.");
  }
  for (int j = 0, k = 0; j < max_nchar; ++j) {
    J[k] = j;
    k += char_non_const[j];
  }

  // V[256 * j + i] = increment
  unsigned int * V = calloc(256 * non_const, sizeof(int));
  if (V == NULL) {
    error("(Calphnum_enc)Unable to allocate V.");
  }
  for (unsigned int k = 0, b = 1; k < non_const; ++k) {
    int j = J[k];
    const unsigned int ejn = length(STRING_ELT(ee, j));
    const char * ej = CHAR(STRING_ELT(ee, j));
    // loop through the string of ee[j] and assign V that value
    for (unsigned int c = 0; c < ejn; ++c) {
      unsigned char ejc = ej[c];
      unsigned int ejci = (unsigned int)ejc;
      V[256 * k + ejci] = c * b;
    }
    b *= ejn;
  }

  SEXP ans = PROTECT(allocVector(INTSXP, N));
  int * restrict ansp = INTEGER(ans);
  for (R_xlen_t i = 0; i < N; ++i) {
    if (STRING_ELT(x, i) == NA_STRING) {
      ansp[i] = NA_INTEGER;
      continue;
    }
    const char * xi = CHAR(STRING_ELT(x, i));
    unsigned int oi = 0;
    for (int k = 0; k < non_const; ++k) {
      unsigned int j = J[k];
      unsigned char xij = xi[j];
      unsigned int xiji = (unsigned int)xij;
      oi += V[256 * k + xiji];
    }
    ansp[i] = (int)oi;
  }
  free(J);
  free(V);
  UNPROTECT(1);
  return ans;
}

SEXP CDecode_fwalnum(SEXP x, SEXP ee) {
  if (TYPEOF(x) != INTSXP || TYPEOF(ee) != STRSXP || xlength(x) >= INT_MAX) {
    error("Internal error(Calphnum_dec): bad types.");
  }
  int max_nchar = xlength(ee);
  R_xlen_t N = xlength(x);

  bool char_non_const[max_nchar];
  unsigned int lens[max_nchar];
  int non_const = 0;
  for (int j = 0; j < max_nchar; ++j) {
    unsigned int j_len = length(STRING_ELT(ee, j));
    lens[j] = j_len;
    bool j_const = j_len != 1;
    char_non_const[j] = j_const;
    non_const += j_const;
  }
  SEXP ans = PROTECT(allocVector(STRSXP, N));

  unsigned int * J = malloc(sizeof(int) * non_const);
  if (J == NULL) {
    error("(Calphnum_dec)Unable to allocate J.");
  }
  for (int j = 0, k = 0; j < max_nchar; ++j) {
    J[k] = j;
    k += char_non_const[j];
  }
  const int * xp = INTEGER(x);
  int max_nchar1 = max_nchar + 1;

  char default_c[max_nchar1];
  for (int j = 0; j < max_nchar; ++j) {
    default_c[j] = CHAR(STRING_ELT(ee, j))[0];
  }
  default_c[max_nchar] = '\0';

  unsigned char C[max_nchar][62];
  for (int j = 0; j < max_nchar; ++j) {
    SEXP eej = STRING_ELT(ee, j);
    int len = length(eej);
    for (int i = 0; i < 62; ++i) {
      if (i < len) {
        C[j][i] = CHAR(STRING_ELT(ee, j))[i];
      } else {
        C[j][i] = '0';
      }
    }
  }

  for (R_xlen_t i = 0; i < N; ++i) {
    unsigned int xi = (unsigned int)xp[i];
    char oi[max_nchar1];
    memcpy(oi, default_c, sizeof(oi));
    for (unsigned int k = 0, b = 1; k < non_const; ++k) {
      int j = J[k];
      unsigned int lenj = lens[j];
      unsigned int c = (xi / b) % lenj;
      unsigned char cc = C[j][c];// CHAR(STRING_ELT(ee, j))[c];
      oi[j] = cc;
      b *= lenj;
    }
    oi[max_nchar] = '\0';
    const char * ansi = (const char *)oi;
    SET_STRING_ELT(ans, i, mkCharCE(ansi, CE_UTF8));
  }
  free(J);
  UNPROTECT(1);
  return ans;
}
