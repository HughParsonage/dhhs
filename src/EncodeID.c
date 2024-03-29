#include "dhhs.h"



bool digit_trailing_spaces(const char * x, int n) {
  if (x[n - 1] != ' ') {
    return false;
  }
  int j = n - 1;
  for (; j > 0; --j) {
    if (x[j] != ' ') {
      break;
    }
  }
  for (; j >= 0; --j) {
    if (!isdigit(x[j])) {
      return false;
    }
  }
  return true;
}

bool digit_leading_spaces(const char * x, int n) {
  bool digit = false;
  for (int i = 0; i < n; ++i) {
    if (digit && !isdigit(x[i])) {
      return false;
    }
    if (!digit && x[i] != ' ') {
      if (isdigit(x[i])) {
        digit = true;
      } else {
        return false;
      }
    }
  }
  return true;
}

bool only_digits_or_spaces(const char * x, int n) {
  for (int i = 0; i < n; ++i) {
    if (x[i] != ' ' && !isdigit(x[i])) {
      return false;
    }
  }
  return true;
}

int nchar_max(SEXP x) {
  if (!isString(x)) {
    error("x must be type character.");
  }
  int max = 0;
  R_xlen_t N = xlength(x);
  const SEXP * xp = STRING_PTR(x);
  for (R_xlen_t i = 0; i < N; ++i) {
    int n = length(xp[i]);
    if (n > max) {
      max = n;
    }
  }
  return max;
}



SEXP CDetermine_fwalnum(SEXP x, SEXP MaxNchar) {
  if (TYPEOF(x) != STRSXP || TYPEOF(MaxNchar) != INTSXP) {
    error("Internal error(CDetermine_fwalnum): wrong types.");
  }
  R_xlen_t N = xlength(x);
  if (N == 0) {
    return R_NilValue;
  }
  unsigned char * are_na = malloc(sizeof(char) * N);
  for (R_xlen_t i = 0; i < N; ++i) {
    are_na[i] = STRING_ELT(x, i) == NA_STRING;
  }
  const int user_max_nchar = asInteger(MaxNchar);
  const int max_nchar = user_max_nchar > 0 ? user_max_nchar : nchar_max(x);
  unsigned int any_nchar_ge = 0;
  unsigned int any_nchar_le = 0;
  // tbl[62*j + k] is 1 if string[k] is present at position j
  unsigned char * tbl = calloc(max_nchar * 62, sizeof(char));
  if (tbl == NULL) {
    error("(Cdetermine_const_width_alnum_encoding): Unable to allocate tbl.");
  }
  bool has_literal_numbers = false;
  for (R_xlen_t i = 0; i < N; ++i) {
    if (are_na[i] == 1) {
      continue;
    }

    const char * xi = CHAR(STRING_ELT(x, i));
    unsigned int strleni = LENGTH(STRING_ELT(x, i));
    unsigned int base_tbl_j = 0;
    if (strleni == max_nchar) {
      if (only_digits_or_spaces(xi, strleni)) {
        has_literal_numbers = true;
        continue; // ignore e.g. '1    '
      }
      for (unsigned int c = 0; c < max_nchar; ++c) {
        unsigned int tbl_i_a = alphnum2uint(xi[c]);
        tbl[base_tbl_j + tbl_i_a] = 1;
        base_tbl_j += 62U;
      }
    } else if (strleni < max_nchar) {
      has_literal_numbers = all_digits(xi, strleni);
      if (!has_literal_numbers) {
        any_nchar_le = i;
      }
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

  SEXP Ans = PROTECT(allocVector(VECSXP, 2));
  SET_VECTOR_ELT(Ans, 0, ans);
  SET_VECTOR_ELT(Ans, 1, ScalarLogical(has_literal_numbers));
  UNPROTECT(2);
  return Ans;
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

void minmax_nchar(SEXP x, int minmax[2]) {
  R_xlen_t N = xlength(x);
  if (N == 0) {
    minmax[0] = INT_MAX;
    minmax[1] = 0;
    return;
  }
  const SEXP * xp = STRING_PTR(x);
  int min = length(xp[0]);
  int max = length(xp[0]);
  for (R_xlen_t i = 1; i < N; ++i) {
    int n = length(xp[i]);
    min = (min > n) ? min : n;
    max = (min < n) ? max : n;
  }
  minmax[0] = min;
  minmax[1] = max;
}

SEXP CEncode_fwalnum(SEXP x, SEXP EE) {
  SEXP ee = VECTOR_ELT(EE, 0);
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
  // J is an array that gives the character position
  // of the k-th non-constant character
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
    int ni = length(STRING_ELT(x, i));
    if ((max_nchar >= 10 && ni < 10) || only_digits_or_spaces(xi, ni)) {
      // just do int
      ansp[i] = atoi(xi);
      continue;
    }

    unsigned int oi = 0;
    for (int k = 0; k < non_const; ++k) {
      unsigned int j = J[k];

      //  0123456 <- j if max_nchar
      //     0123 <- j
      //     3456
      // Consider max_nchar = 18 and j = 17 (i.e the last character)
      // for a char 2 string we want the last string too (i.e. 1)
      // for j = 16 we would want r = 0
      unsigned int r = j - (max_nchar - ni);
      if (r >= ni) {
        continue; // technically an error
      }
      unsigned char xij = xi[r];
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

SEXP CDecode_fwalnum(SEXP x, SEXP EE) {
  SEXP ee = VECTOR_ELT(EE, 0);
  SEXP ee1 = VECTOR_ELT(EE, 1);
  const bool use_atoi = asLogical(ee1);
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
    if (use_atoi && xi < 10000000) {
      int n_digit = n_digits0(xi);
      char oi[n_digit + 1];
      sprintf(oi, "%d", xi);
      const char * ansi = (const char *)oi;
      SET_STRING_ELT(ans, i, mkChar(ansi));
      continue;
    }
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

unsigned int e18j(char x, const char * e, int n) {
  if (n <= 1) {
    return 0;
  }
  unsigned int j = 0;
  while (e[j] != x) {
    ++j;
  }
  // while (j < n && e[j] != x) {
  //   ++j;
  // }
  return j;
}



SEXP C_Encode2(SEXP x, SEXP y) {
  R_xlen_t N = xlength(x);
  const SEXP * xp = STRING_PTR(x);
  if (length(y) != 18) {
    error("(C_Encode2): length(y) != 18.");
  }
  const SEXP * yp = STRING_PTR(y);

  const char * e00 = CHAR(yp[0]);
  const unsigned int n00 = length(yp[0]);
  const char * e01 = CHAR(yp[1]);
  const unsigned int n01 = length(yp[1]);
  const char * e02 = CHAR(yp[2]);
  const unsigned int n02 = length(yp[2]);
  const char * e03 = CHAR(yp[3]);
  const unsigned int n03 = length(yp[3]);
  const char * e04 = CHAR(yp[4]);
  const unsigned int n04 = length(yp[4]);
  const char * e05 = CHAR(yp[5]);
  const unsigned int n05 = length(yp[5]);
  const char * e06 = CHAR(yp[6]);
  const unsigned int n06 = length(yp[6]);
  const char * e07 = CHAR(yp[7]);
  const unsigned int n07 = length(yp[7]);
  const char * e08 = CHAR(yp[8]);
  const unsigned int n08 = length(yp[8]);
  const char * e09 = CHAR(yp[9]);
  const unsigned int n09 = length(yp[9]);
  const char * e10 = CHAR(yp[10]);
  const unsigned int n10 = length(yp[10]);
  const char * e11 = CHAR(yp[11]);
  const unsigned int n11 = length(yp[11]);
  const char * e12 = CHAR(yp[12]);
  const unsigned int n12 = length(yp[12]);
  const char * e13 = CHAR(yp[13]);
  const unsigned int n13 = length(yp[13]);
  const char * e14 = CHAR(yp[14]);
  const unsigned int n14 = length(yp[14]);
  const char * e15 = CHAR(yp[15]);
  const unsigned int n15 = length(yp[15]);
  const char * e16 = CHAR(yp[16]);
  const unsigned int n16 = length(yp[16]);
  const char * e17 = CHAR(yp[17]);
  const unsigned int n17 = length(yp[17]);


  SEXP ans = PROTECT(allocVector(INTSXP, N));
  int * restrict ansp = INTEGER(ans);

  for (R_xlen_t i = 0; i < N; ++i) {
    int n = length(xp[i]);
    const char * xi = CHAR(xp[i]);
    if (n != 18) {
      ansp[i] = atoi(xi);
      continue;
    }

    unsigned int o = 0;

    o *= n00;
    o += e18j(xi[0], e00, n00);
    o *= n01;
    o += e18j(xi[1], e01, n01);
    o *= n02;
    o += e18j(xi[2], e02, n02);
    o *= n03;
    o += e18j(xi[3], e03, n03);
    o *= n04;
    o += e18j(xi[4], e04, n04);
    o *= n05;
    o += e18j(xi[5], e05, n05);
    o *= n06;
    o += e18j(xi[6], e06, n06);
    o *= n07;
    o += e18j(xi[7], e07, n07);
    o *= n08;
    o += e18j(xi[8], e08, n08);
    o *= n09;
    o += e18j(xi[9], e09, n09);
    o *= n10;
    o += e18j(xi[10], e10, n10);
    o *= n11;
    o += e18j(xi[11], e11, n11);
    o *= n12;
    o += e18j(xi[12], e12, n12);
    o *= n13;
    o += e18j(xi[13], e13, n13);
    o *= n14;
    o += e18j(xi[14], e14, n14);
    o *= n15;
    o += e18j(xi[15], e15, n15);
    o *= n16;
    o += e18j(xi[16], e16, n16);
    o *= n17;
    o += e18j(xi[17], e17, n17);
    ansp[i] = o;
  }
  UNPROTECT(1);
  return ans;
}


SEXP C_Decode2(SEXP x, SEXP y) {
  R_xlen_t N = xlength(x);
  const int * xp = INTEGER(x);
  if (length(y) != 18) {
    error("(C_Encode2): length(y) != 18.");
  }
  const SEXP * yp = STRING_PTR(y);

  const char * e00 = CHAR(yp[0]);
  const unsigned int n00 = length(yp[0]);
  const char * e01 = CHAR(yp[1]);
  const unsigned int n01 = length(yp[1]);
  const char * e02 = CHAR(yp[2]);
  const unsigned int n02 = length(yp[2]);
  const char * e03 = CHAR(yp[3]);
  const unsigned int n03 = length(yp[3]);
  const char * e04 = CHAR(yp[4]);
  const unsigned int n04 = length(yp[4]);
  const char * e05 = CHAR(yp[5]);
  const unsigned int n05 = length(yp[5]);
  const char * e06 = CHAR(yp[6]);
  const unsigned int n06 = length(yp[6]);
  const char * e07 = CHAR(yp[7]);
  const unsigned int n07 = length(yp[7]);
  const char * e08 = CHAR(yp[8]);
  const unsigned int n08 = length(yp[8]);
  const char * e09 = CHAR(yp[9]);
  const unsigned int n09 = length(yp[9]);
  const char * e10 = CHAR(yp[10]);
  const unsigned int n10 = length(yp[10]);
  const char * e11 = CHAR(yp[11]);
  const unsigned int n11 = length(yp[11]);
  const char * e12 = CHAR(yp[12]);
  const unsigned int n12 = length(yp[12]);
  const char * e13 = CHAR(yp[13]);
  const unsigned int n13 = length(yp[13]);
  const char * e14 = CHAR(yp[14]);
  const unsigned int n14 = length(yp[14]);
  const char * e15 = CHAR(yp[15]);
  const unsigned int n15 = length(yp[15]);
  const char * e16 = CHAR(yp[16]);
  const unsigned int n16 = length(yp[16]);
  const char * e17 = CHAR(yp[17]);
  const unsigned int n17 = length(yp[17]);

  SEXP ans = PROTECT(allocVector(STRSXP, N));

  for (R_xlen_t i = 0; i < N; ++i) {
    unsigned int ei = xp[i];
    char o[19];
    o[17] = e17[ei % n17];
    ei /= n17;
    o[16] = e16[ei % n16];
    ei /= n16;
    o[15] = e15[ei % n15];
    ei /= n15;
    o[14] = e14[ei % n14];
    ei /= n14;
    o[13] = e13[ei % n13];
    ei /= n13;
    o[12] = e12[ei % n12];
    ei /= n12;
    o[11] = e11[ei % n11];
    ei /= n11;
    o[10] = e10[ei % n10];
    ei /= n10;
    o[9] = e09[ei % n09];
    ei /= n09;
    o[8] = e08[ei % n08];
    ei /= n08;
    o[7] = e07[ei % n07];
    ei /= n07;
    o[6] = e06[ei % n06];
    ei /= n06;
    o[5] = e05[ei % n05];
    ei /= n05;
    o[4] = e04[ei % n04];
    ei /= n04;
    o[3] = e03[ei % n03];
    ei /= n03;
    o[2] = e02[ei % n02];
    ei /= n02;
    o[1] = e01[ei % n01];
    ei /= n01;
    o[0] = e00[0];
    o[18] = '\0';
    SET_STRING_ELT(ans, i, mkChar((const char *)o));
  }
  UNPROTECT(1);
  return ans;
}

SEXP CC_Atoi(SEXP x) {
  const SEXP * xp = STRING_PTR(x);
  R_xlen_t N = xlength(x);
  SEXP ans = PROTECT(allocVector(INTSXP, N));
  int * restrict ansp = INTEGER(ans);
  for (R_xlen_t i = 0; i < N; ++i) {
    const char * xi = CHAR(xp[i]);
    char x0 = xi[0];
    if (x0 >= '1' && x0 <= '9') {
      ansp[i] = atoi(xi);
    } else {
      ansp[i] = NA_INTEGER;
    }
  }
  UNPROTECT(1);
  return ans;
}








