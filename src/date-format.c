#include "dhhs.h"

// determine the format of a single string
#define YYYYMMDD 1
#define YYYY_MM_DD 2
#define HHMMSS 65

// YYYYMMDDHHMMSS
#define datetime_fmt1 1
// YYYY-MM-DD HH:MM:SS
#define datetime_fmt2 2
// YYYY-MM-DDHH:MM:SS
#define datetime_fmt3 3
// YYYY-MM-DDHH:MM:SS +HHHH
#define datetime_fmt4 4
// YYYY-MM-DD HH:MM:SS +HHHH
#define datetime_fmt5 5


unsigned int fmt_date(const char * x, int n) {
  if (n < 8) {
    return 0;
  }
  if (n == 8) {
    return YYYYMMDD;
  }
  bool all_digits = true;
  for (int j = 0; j < 8; ++j) {
    if (!isdigit(x[j])) {
      all_digits = false;
      break;
    }
  }
  return all_digits ? YYYYMMDD : YYYY_MM_DD;
}

static bool digitsat(const char * x,
                     int y0,
                     int m0,
                     int d0,
                     int H0,
                     int M0,
                     int S0) {
  return
  isdigit(x[y0]) &&
    isdigit(x[m0]) &&
    isdigit(x[d0]) &&
    isdigit(x[H0]) &&
    isdigit(x[M0]) &&
    isdigit(x[S0]);
}

static void fmt_datetime(int YMD_DMS[6], const char * x, int n) {
  YMD_DMS[0] = 0;
  int j = 3, k = 1;
  while (++j < n && k < 6) {
    if (isdigit(x[j])) {
      YMD_DMS[k] = j;
      ++k, ++j;
    }
  }
}

static unsigned int encode_datetime_fmt(int YMD_DMS[6]) {
  unsigned int o = 0, b = 1;
  for (int k = 1; k < 6; ++k) {
    int d = YMD_DMS[k] - YMD_DMS[k - 1];
    o += d;
    b <<= 4;
  }
  return o;
}

static void decode_datetime_fmt(int YMD_DMS[6], unsigned int o) {
  unsigned int ox = o;
  for (int k = 6; k >= 1; --k) {
    int d = ox & 15;
    ox >>= 4;
    YMD_DMS[k] = d;
  }
  for (int k = 1; k < 6; ++k) {
    Rprintf("%d,", YMD_DMS[k]);
    YMD_DMS[k] += YMD_DMS[k - 1];
  }
}

SEXP Cfmt_datetime(SEXP x) {
  if (!isString(x)) {
    return R_NilValue;
  }
  const char * xi = CHAR(STRING_ELT(x, 0));
  int n = length(STRING_ELT(x, 0));
  int YMD_DMS[6] = {0};
  fmt_datetime(YMD_DMS, xi, n);
  unsigned int ox = encode_datetime_fmt(YMD_DMS);
  Rprintf("%u\n", ox);
  int ymd_dms[6] = {0};
  decode_datetime_fmt(ymd_dms, ox);
  for (int j = 0; j < 6; ++j) {
    Rprintf("%d, %d\n", YMD_DMS[j], ymd_dms[j]);
  }
  return R_NilValue;
}

