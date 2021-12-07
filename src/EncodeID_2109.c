#include "dhhs.h"

/*
 * $cipher
 [1] "5"
 [2] "0"
 [3] "0"
 [4] "2"
 [5] "P"
 [6] "0"
 [7] "0"
 [8] "0"
 [9] "0"
 [10] "0"
 [11] "DEFGHIJK"
 [12] "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"
 [13] "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"
 [14] "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"
 [15] "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"
 [16] "Q"
 [17] "A"
 [18] "135BDFHJLNPRTVXZ"
 */

static unsigned int u18(char x, const char * C) {
  for (int j = 15; j > 0; --j) {
    if (x == C[j]) {
      return j;
    }
  }
  return 0;
}

static unsigned int u11(char x, const char * C) {
  for (int j = 8; j > 0; --j) {
    if (x == C[j]) {
      return j;
    }
  }
  return 0;
}

static bool c11_valid(unsigned char x11) {
  // Assumes chars are contiguous (not technically true but for the most part)
  return x11 >= 'D' && x11 <= 'L';
}

static bool c18_valid(unsigned char x18) {
  const unsigned char valid_chars[] = "135BDFHJLNPRTVXZ";
  for (int j = 0; j < 16; ++j) {
    if (x18 == valid_chars[j]) {
      return true;
    }
  }
  return false;
}



R_xlen_t isnt_RecordID_2109(SEXP x) {
  if (!isString(x)) {
    return 1;
  }
  R_xlen_t N = xlength(x);
  if (N == 0) {
    return 0;
  }
  const SEXP * xp = STRING_PTR(x);
  unsigned char c11_validity[256] = {0}; // c11_validity[i] means i is a valid 11
  unsigned char c18_validity[256] = {0};
  for (int i = 0; i < 256; ++i) {
    unsigned char c11i = i;
    c11_validity[i] = c11i >= 'D' && c11i <= 'L';
    c18_validity[i] = c18_valid(c11i);
  }

  for (R_xlen_t i = 0; i < N; ++i) {
    int n = length(xp[i]);
    if (n > 18) {
      return i + 1;
    }
    const char * xi = CHAR(xp[i]);
    if (n < 18) {
      for (int j = 0; j < n; ++j) {
        if (!isdigit(xi[j])) {
          return i + 1;
        }
      }
    } else {
      unsigned char x0 = xi[0];
      unsigned char x1 = xi[1];
      unsigned char x2 = xi[2];
      unsigned char x3 = xi[3];
      unsigned char x4 = xi[4];
      unsigned char x5 = xi[5];
      unsigned char x6 = xi[6];
      unsigned char x7 = xi[7];
      unsigned char x8 = xi[8];
      unsigned char x9 = xi[9];
      // unsigned char x10 = xi[10];
      // unsigned char x11 = xi[11];
      // unsigned char x12 = xi[12];
      // unsigned char x13 = xi[13];
      // unsigned char x14 = xi[14];
      unsigned char x15 = xi[15];
      unsigned char x16 = xi[16];
      // unsigned char x17 = xi[17];

      unsigned char o = x0 ^ '5';
      o ^= x1 ^ '0';
      o ^= x2 ^ '0';
      o ^= x3 ^ '2';
      o ^= x4 ^ 'P';
      o ^= x5 ^ '0';
      o ^= x6 ^ '0';
      o ^= x7 ^ '0';
      o ^= x8 ^ '0';
      o ^= x9 ^ '0';
      o ^= x15 ^ 'Q';
      o ^= x16 ^ 'A';
      // unsigned char x11_15_18 = c11_validity[x10] &
      //   c18_validity[x17] &
      //   isalnum(x11) &
      //   isalnum(x12) &
      //   isalnum(x13) &
      //   isalnum(x14);
      //
      // o |= ~x11_15_18;

      if (o) {
        return i + 1;
      }
    }
  }
  return 0;
}

SEXP isntRecordID2109(SEXP x) {
  return ScalarInteger(isnt_RecordID_2109(x));
}

  // 2109 = RecordID as observed at 2021-09
SEXP CEncode_RecordID_2109(SEXP x) {
  unsigned int O18[256] = {0};
  unsigned int O11[256] = {0};
  unsigned int Aa9[256] = {0};
  for (int j = 0; j < 256; ++j) {
    O18[j] = u18(j, "135BDFHJLNPRTVXZ");
    O11[j] = u11(j, "DEFGHIJKL");
    Aa9[j] = alphnum2uint((unsigned char)j);
  }

  R_xlen_t N = xlength(x);
  const SEXP * xp = STRING_PTR(x);
  SEXP ans = PROTECT(allocVector(INTSXP, N));
  int * restrict ansp = INTEGER(ans);
  for (R_xlen_t i = 0; i < N; ++i) {
    int n = length(xp[i]);
    const char * xi = CHAR(xp[i]);
    if (n != 18) {
      ansp[i] = NA_INTEGER;
      continue;
    }
    unsigned int o = 0;
    unsigned int xi11 = (unsigned char)xi[10];
    unsigned int xi12 = (unsigned char)xi[11];
    unsigned int xi13 = (unsigned char)xi[12];
    unsigned int xi14 = (unsigned char)xi[13];
    unsigned int xi15 = (unsigned char)xi[14];
    unsigned int xi18 = (unsigned char)xi[18];
    o += O11[xi11];
    o *= 9;
    o += O18[xi18];
    o <<= 4;
    o += Aa9[xi12];
    o <<= 6;
    o += Aa9[xi13];
    o <<= 6;
    o += Aa9[xi14];
    o <<= 6;
    o += Aa9[xi15];
    ansp[i] = o;
  }
  UNPROTECT(1);
  return ans;
}
