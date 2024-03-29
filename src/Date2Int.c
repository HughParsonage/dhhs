#include "dhhs.h"

const static int MONTHDAYS[13] = {0, 31, 28, 31,  30,  31,  30,  31,  31,  30,  31,  30, 31};
const static int MONTHDAYC[12] = {0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334};

const static int IDAY_2020 = 18262;
const static int IDAY_2021 = 18628;

int is_leap_year(int year) {
  if (year < 0 || year % 4) {
    return 0;
  }
  if (year % 100) {
    return 1;
  }
  if (year % 400) {
    return 0;
  }
  return 1;
}

bool endsWith2021(const char * x) {
  return x[9] == '1' && x[6] == '2' && x[7] == '0' && x[8] == '2';
}

SEXP CDate2Int(SEXP xx) {
  R_xlen_t N = xlength(xx);
  if (!isString(xx)) {
    return xx;
  }
  SEXP ans = PROTECT(allocVector(INTSXP, N));
  int * restrict ansp = INTEGER(ans);

  for (R_xlen_t i = 0; i < N; ++i) {
    int len = length(STRING_ELT(xx, i));
    if (len < 10) {
      ansp[i] = NA_INTEGER;
      continue;
    }

    const char * x = CHAR(STRING_ELT(xx, i));
    int mday = 0;
    int month = 0;
    int year = 0;
    mday += 10 * (x[0] - '0') + (x[1] - '0');
    month += (x[3] == '1' ? 10 : 0) + (x[4] - '0');
    int o = IDAY_2020 - 1;
    if (endsWith2021(x)) {
      o += 366; // 2020 was leap year
      o += MONTHDAYC[month - 1] + mday;
      ansp[i] = o;
      continue;
    }

    year += (x[6] - '0') * 1000;
    year += (x[7] - '0') * 100;
    year += (x[8] - '0') * 10;
    year += (x[9] - '0');

    int yday = MONTHDAYC[month - 1] + mday + (is_leap_year(year) && month > 2);
    if (year > 2020) {
      for (int y = 2020; y < year; ++y) {
        o += 365;
        o += is_leap_year(y);
      }
    }
    if (year < 2020) {
      for (int y = year; y < 2020; ++y) {
        o -= 365;
        o -= is_leap_year(y);
      }
    }
    o += yday;
    ansp[i] = o;
  }
  UNPROTECT(1);
  return ans;
}


SEXP C_yyyy_mm_dd(SEXP xx) {
  R_xlen_t N = xlength(xx);
  const SEXP * xp = STRING_PTR(xx);
  SEXP ans = PROTECT(allocVector(INTSXP, N));
  int * restrict ansp = INTEGER(ans);
  for (R_xlen_t i = 0; i < N; ++i) {
    int n = length(xp[i]);
    if (n < 10) {
      ansp[i] = NA_INTEGER;
      continue;
    }
    const char * x = CHAR(xp[i]);
    int year = atoi(x);
    if (year == 2021) {
      int o = IDAY_2021 - 1;
      int month = (x[5] == '1' ? 10 : 0) + (x[6] - '0');
      int mday = 10 * (x[8] - '0') + (x[9] - '0');
      o += MONTHDAYC[month - 1] + mday;
      ansp[i] = o;
      continue;
    }

    if (year == 2020) {
      int o = IDAY_2020 - 1;
      int month = (x[5] == '1' ? 10 : 0) + (x[6] - '0');
      int mday = 10 * (x[8] - '0') + (x[9] - '0');
      o += MONTHDAYC[month - 1] + mday + (month > 2);
      ansp[i] = o;
      continue;
    }
    int o = IDAY_2020 - 1;
    if (year > 2020) {
      for (int y = 2020; y < year; ++y) {
        o += 365;
        o += is_leap_year(y);
      }
    }
    if (year < 2020) {
      for (int y = year; y < 2020; ++y) {
        o -= 365;
        o -= is_leap_year(y);
      }
    }

    int month = (x[5] == '1' ? 10 : 0) + (x[6] - '0');
    int mday = 10 * (x[8] - '0') + (x[9] - '0');
    o += MONTHDAYC[month - 1] + mday;
    ansp[i] = o;
  }
  UNPROTECT(1);
  return ans;

}

#define UTC_2020_ZERO 1577836800
#define UTC_2021_ZERO 1609459200
#define UTC_2022_ZERO 1640995200


const static int UTC_2020_MONTHS[12] = {1577836800, 1580515200, 1583020800,
                                        1585699200, 1588291200, 1590969600,
                                        1593561600, 1596240000, 1598918400,
                                        1601510400, 1604188800, 1606780800};
const static int UTC_2021_MONTHS[12] = {1609459200, 1612137600, 1614556800,
                                        1617235200, 1619827200, 1622505600,
                                        1625097600, 1627776000, 1630454400,
                                        1633046400, 1635724800, 1638316800};
const static int UTC_2022_MONTHS[12] = {1640995200, 1643673600, 1646092800,
                                        1648771200, 1651363200, 1654041600,
                                        1656633600, 1659312000, 1661990400,
                                        1664582400, 1667260800, 1669852800};


typedef struct  {
  unsigned int yr2020 : 3;
  unsigned int month : 4;
  unsigned int day : 5;
  unsigned int hour : 5;
  unsigned int minute : 6;
  unsigned int second : 6;
} datetime2020;

static datetime2020 set_datetime2020(int n) {
  datetime2020 O;
  O.yr2020 = 0;
  O.month = 0;
  O.day = 0;
  O.hour = 0;
  O.minute = 0;
  O.day = 0;
  return O;
}

static int atoiyear(char a, char b, char c, char d) {
  int o = d - '0';
  o += 10 * (c - '0');
  o += 100 * (b - '0');
  o += 1000 * (a - '0');
  return o;
}

static datetime2020 char2datetime_UTC(const char * x, int n) {
  datetime2020 O = set_datetime2020(0);
  if (n < 14) {
    return O;
  }
  int digs[14] = {0};
  digs[1] = x[1] - '0';
  digs[2] = x[2] - '0';
  digs[3] = x[3] - '0';
  int k = 4;
  for (int j = 4; j < n; ++j) {
    if (isdigit(x[j])) {
      digs[k] = x[j] - '0';
      ++k;
      if (k >= 14) {
        break;
      }
    }
  }
  O.yr2020 = ((unsigned int)(x[3] - '0')) & 7;

  O.month = 10 * digs[4] + digs[5];
  O.day = 10 * digs[6] + digs[7];
  O.hour = 10 * digs[8] + digs[9];
  O.minute = 10 * digs[10] + digs[11];
  O.second = 10 * digs[12] + digs[13];
  return O;
}

int datetime2020i(datetime2020 O) {
  int o = UTC_2020_MONTHS[O.month - 1];
  o += 86400 * (O.day - 1);
  o += 3600 * O.hour;
  o += 60 * O.minute;
  o += O.second;
  return o;
}

int datetime2021i(datetime2020 O) {
  int o = UTC_2021_MONTHS[O.month - 1];
  o += 86400 * (O.day - 1);
  o += 3600 * O.hour;
  o += 60 * O.minute;
  o += O.second;
  return o;
}

int datetime2022i(datetime2020 O) {
  int o = UTC_2022_MONTHS[O.month - 1];
  o += 86400 * (O.day - 1);
  o += 3600 * O.hour;
  o += 60 * O.minute;
  o += O.second;
  return o;
}


int datetime_UTC(const char * x, int n) {
  if (n < 19) {
    // "2041-01-01 00:00:00"
    return NA_INTEGER;
  }

  datetime2020 O = char2datetime_UTC(x, n);
  if (O.yr2020 == 0) {
    return datetime2020i(O);
  }
  if (O.yr2020 == 1) {
    return datetime2021i(O);
  }
  if (O.yr2020 == 2) {
    return datetime2022i(O);
  }
  return NA_INTEGER;
}

#define fmt_YYYY_MM_DD_HHcMMcSS__pOFFSET 1
#define fmt_YYYY_MMDD_HHcMMcSS__pOFFSET 2

unsigned int char2type(char x) {
  if (isdigit(x)) {
    return 1;
  }
  if (x == '+') {
    return 2;
  }
  if (ispunct(x)) {
    return 3;
  }
  return 0;
}



unsigned int fmt_datetime(const char * x, int n) {
  unsigned int o = 0;
  unsigned int b = 4;
  switch(n) {
  case 34: {
  // "2021-09-16 12:51:45.0000000 +00:00"
  for (int j = 4; j < 34; ++j) {
    o += char2type(x[j]);
    o <<= 2;
  }
}
    break;

  }
  return o;
}

SEXP C_fmt_datetime(SEXP x) {
  R_xlen_t N = xlength(x);
  const SEXP * xp = STRING_PTR(x);
  SEXP ans = PROTECT(allocVector(INTSXP, N));
  int * restrict ansp = INTEGER(ans);
  for (R_xlen_t i = 0; i < N; ++i) {
    if (xp[i] == NA_STRING) {
      ansp[i] = NA_INTEGER;
      continue;
    }
    const char * xi = CHAR(xp[i]);
    int n = length(xp[i]);
    ansp[i] = fmt_datetime(xi, n);
  }
  UNPROTECT(1);
  return ans;
}

static int xoffset(const char * x, const int n, int j0) {
  for (int j = j0; j < n - 4; ++j) {
    char xj = x[j];
    if (xj == '+' || xj == '-') {
      int o = 10 * (x[j + 1] - '0') + (x[j + 2] - '0');
      o *= 60;
      if (isdigit(x[j + 3])) {
        o += 10 * (x[j + 3] - '0') + (x[j + 4] - '0');
      } else if (x[j + 3] == ':' && j + 5 < n) {
        o += 10 * (x[j + 4] - '0') + (x[j + 5] - '0');
      }
      o *= 60;
      return xj == '-' ? -o : o;
    }
  }
  return 0;
}



static void loc09(int YMD_HMS[6], const char * x, int n) {
  YMD_HMS[0] = 0;
  for (int j = 4, k = 1; (j < n && k < 6); ++j) {
    if (isdigit(x[j])) {
      YMD_HMS[k] = j;
      ++k;
      ++j;
    }
  }
}

static int datetime_const(int YMD_HMS[6], const char * x, const int n0) {
  datetime2020 O;
  O.yr2020 = x[3] - '0';
  O.month = (x[YMD_HMS[1]] == '1' ? 10 : 0) + (x[YMD_HMS[1] + 1] - '0');
  O.day = 10 * (x[YMD_HMS[2]] - '0') + (x[YMD_HMS[2] + 1] - '0');
  O.hour = 10 * (x[YMD_HMS[3]] - '0') + (x[YMD_HMS[3] + 1] - '0');
  O.minute = 10 * (x[YMD_HMS[4]] - '0') + (x[YMD_HMS[4] + 1] - '0');
  O.second = 10 * (x[YMD_HMS[5]] - '0') + (x[YMD_HMS[5] + 1] - '0');
  switch(x[3]) {
  case '0':
    return datetime2020i(O);
  case '1':
    return datetime2021i(O);
  case '2':
    return datetime2022i(O);
    break;
  }
  return NA_INTEGER;
}

static int joffset(const char * x, const int n0) {
  for (int j = 8; j < n0; ++j) {
    if (x[j] == '+' || x[j] == '-') {
      return j;
    }
  }
  return 0; // # nocov
}



SEXP yyyy_mm_dd_HHMMSS_UTC_const_nchar(SEXP x, const int n0) {
  R_xlen_t N = xlength(x);
  const SEXP * xp = STRING_PTR(x);
  const char * x0 = CHAR(xp[0]);
  int fmt = 0;
  SEXP ans = PROTECT(allocVector(INTSXP, N));
  int * restrict ansp = INTEGER(ans);
  const int offset0 = xoffset(x0, n0, 8);
  const int offsetj = offset0 ? joffset(x0, n0) : 0;
  int YMD_HMS[6] = {0};
  loc09(YMD_HMS, x0, n0);

  if (offsetj) {
    for (R_xlen_t i = 0; i < N; ++i) {
      const char * x = CHAR(xp[i]);
      const int n0 = length(xp[i]);
      int anspi = datetime_const(YMD_HMS, x, n0);
      int offseti = xoffset(x, n0, offsetj);
      ansp[i] = anspi - offseti; // subtract the displayed offset e.g. +11:00
    }
  } else {
    for (R_xlen_t i = 0; i < N; ++i) {
      ansp[i] = datetime_const(YMD_HMS, CHAR(xp[i]), length(xp[i]));
    }
  }
  UNPROTECT(1);
  return ans;
}

R_xlen_t which_startsWithout202(const SEXP * xp, R_xlen_t N, const bool narm) {
  if (narm) {
    for (R_xlen_t i = 0; i < N; ++i) {
      if (xp[i] == NA_STRING) {
        continue;
      }
      const char * xi = CHAR(xp[i]);
      bool other = (xi[0] != '2') || (xi[1] != '0') || (xi[2] != '2');
      if (other) {
        return i + 1;
      }
    }
  } else {
    for (R_xlen_t i = 0; i < N; ++i) {
      if (xp[i] == NA_STRING) {
        return i + 1;
      }
      const char * xi = CHAR(xp[i]);
      bool other = (xi[0] != '2') || (xi[1] != '0') || (xi[2] != '2');
      if (other) {
        return i + 1;
      }
    }
  }
  return 0;
}

SEXP Ccheck_startsWith202(SEXP x) {
  R_xlen_t N = xlength(x);
  const SEXP * xp = STRING_PTR(x);
  for (R_xlen_t i = 0; i < N; ++i) {
    if (xp[i] == NA_STRING) {
      continue;
    }
    const char * xi = CHAR(xp[i]);
    bool other = (xi[0] != '2') || (xi[1] != '0') || (xi[2] != '2');
    if (other) {
      error("At position %lld string did not start with 202.", i);
    }
  }
  return R_NilValue;
}

SEXP C_yyyy_mm_dd_HHMMSS_UTC(SEXP xx) {
  int nchar_const = is_const_nchar(xx);
  if (nchar_const > 0) {
    return yyyy_mm_dd_HHMMSS_UTC_const_nchar(xx, nchar_const);
  }
  R_xlen_t N = xlength(xx);
  const SEXP * xp = STRING_PTR(xx);
  SEXP ans = PROTECT(allocVector(INTSXP, N));
  int * restrict ansp = INTEGER(ans);

  for (R_xlen_t i = 0; i < N; ++i) {
    ansp[i] = datetime_UTC(CHAR(xp[i]), length(xp[i]));
  }

  UNPROTECT(1);
  return ans;
}



