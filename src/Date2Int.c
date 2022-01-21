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

int month2c(char m1, char m2) {
  if (m1 == '1') {
    if (m2 == '0') {
      return 10;
    }
    if (m2 == '1') {
      return 11;
    }
    if (m2 == '2') {
      return 12;
    }
  }
  return isdigit(m2) ? m2 - '0' : 1;

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
const static int SYD_2020_MONTHS[12] = {1577797200, 1580475600, 1582981200,
                                        1585659600, 1588255200, 1590933600,
                                        1593525600, 1596204000, 1598882400,
                                        1601474400, 1604149200, 1606741200};
const static int SYD_2021_MONTHS[12] = {1609419600, 1612098000, 1614517200,
                                        1617195600, 1619791200, 1622469600,
                                        1625061600, 1627740000, 1630418400,
                                        1633010400, 1635685200, 1638277200};
const static int SYD_2022_MONTHS[12] = {1640955600, 1643634000, 1646053200,
                                        1648731600, 1651327200, 1654005600,
                                        1656597600, 1659276000, 1661954400,
                                        1664546400, 1667221200, 1669813200};

const static int SYD_2020_YDAY_DAYLIGHT[2] = {96, 278};
const static int SYD_2021_YDAY_DAYLIGHT[2] = {94, 276};
const static int SYD_2022_YDAY_DAYLIGHT[2] = {94, 275};

int adjust_syd_daylight(int y, int m, int d, int h) {
  int o = 0;
  switch(y) {
  case 0:
    if (m == 4 && d >= 4) {
      if (d > 4 || h >= 3) {
        o += 60;
      }
    } else if (m == 10 && d >= 5) {
      if (d > 5 || h >= 3) {
        o -= 60;
      }
    }
    break;
  case 1:
    if (m == 4 && d >= 4) {
      if (d > 4 || h >= 3) {
        o += 60;
      }
    } else if (m == 10 && d >= 3) {
      if (d > 3 || h >= 3) {
        o -= 60;
      }
    }
    break;
  case 2:
    if (m == 4 && d >= 3) {
      if (d > 3 || h >= 3) {
        o += 60;
      }
    } else if (m == 10 && d >= 2) {
      if (d > 2 || h >= 3) {
        o -= 60;
      }
    }
    break;
  }
  return o;
}

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

static datetime2020 datetime2020_from_int(int x) {
  datetime2020 O;
  O.yr2020 = 2;
  O.month = 12;
  int x_if_0101 = SYD_2022_MONTHS[0]; // value of x if at midnight at start of month.
  // first determine the year and month
  if (x < SYD_2022_MONTHS[0]) {
    if (x < SYD_2021_MONTHS[0]) {
      O.yr2020 = 0;
      for (int m = 1; m < 11; ++m) {
        if (x < SYD_2020_MONTHS[m]) {
          O.month = m;
          x_if_0101 = SYD_2020_MONTHS[m - 1];
          break;
        }
      }
    } else {
      O.yr2020 = 1;
      for (int m = 1; m < 11; ++m) {
        if (x < SYD_2021_MONTHS[m]) {
          O.month = m;
          x_if_0101 = SYD_2021_MONTHS[m - 1];
          break;
        }
      }
    }
  } else {
    for (int m = 1; m < 11; ++m) {
      if (x < SYD_2022_MONTHS[m]) {
        O.month = m;
        x_if_0101 = SYD_2022_MONTHS[m - 1];
        break;
      }
    }
  }
  O.day = (x - x_if_0101)/ 86400 + 1;
  O.hour = (x - (x_if_0101 + (O.day - 1) * 86400)) / 3600;
  // O.minute = (x - (x_if_0101 + (O.day - 1) * 86400 + O.hour * 3600)) / 60;
  O.minute = 0;
  O.second = 0;
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

int datetimei_syd(datetime2020 O) {
  const int m = O.month;
  const int d = O.day;
  const int h = O.hour;
  int o = 0;
  switch (O.yr2020) {
  case 0:
    o += SYD_2020_MONTHS[m - 1] + adjust_syd_daylight(0, m, d, h);
    break;
  case 1:
    o += SYD_2021_MONTHS[m - 1] + adjust_syd_daylight(1, m, d, h);
    break;
  default:
    o += SYD_2022_MONTHS[m - 1] + adjust_syd_daylight(2, m, d, h);
    break;
  }
  o += 86400 * (d - 1);
  o += 3600 * h;
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

SEXP C_yyyymmdd_HHMMSS_UTC(SEXP xx) {
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

int datetime_UTC2(const char * x, const char * y, int nx, int ny) {
  datetime2020 O;
  O.yr2020 = x[3] - '0';
  O.month = month2c(x[5], x[6]);
  O.day = 10 * (x[8] - '0') + (x[9] - '0');
  O.hour = (y[0] == '1' ? 10 : 0) + (y[1] - '0');
  O.minute = 10 * (y[3] - '0') + (y[4] - '0');
  O.second = 0;
  if (y[6] != '0' || y[7] != '0') {
    O.second = 10 * (y[6] - '0') + (y[7] - '0');
  }
  switch(x[3]) {
  case '0':
    return datetime2020i(O);
  case '1':
    return datetime2021i(O);
  case '2':
    return datetime2022i(O);
  default:
    return NA_INTEGER;
  }
  return NA_INTEGER; // # nocov
}

int datetime_SYD2(const char * x, const char * y, int nx, int ny) {
  int yr = x[3] - '0';
  int mm = 10 * (x[5] - '0') + (x[6] - '0');
  int dd = 10 * (x[8] - '0') + (x[9] - '0');
  int HH = 10 * (y[0] - '0') + (y[1] - '0');
  int MM = 10 * (y[3] - '0') + (y[4] - '0');
  int SS = 10 * (y[6] - '0') + (y[7] - '0');
  int o = 0;
  switch(yr) {
  case 0:
    o += SYD_2020_MONTHS[mm - 1];
    break;
  case 1:
    o += SYD_2021_MONTHS[mm - 1];
    break;
  default:
    o += SYD_2022_MONTHS[mm - 1];
  }
  o += 86000 * (dd - 1);
  o += 3600 * HH;
  o += 60 * MM;
  o += SS;
  o += adjust_syd_daylight(yr, mm, dd, HH);
  return o;
}

SEXP do_yyyymmdd_HHMMSS_SYD(SEXP x) {
  R_xlen_t N = xlength(x);
  if (!isString(x)) {
    error("x was type '%s' but must be character.", type2char(TYPEOF(x)));
  }
  const SEXP * xp = STRING_PTR(x);
  SEXP ans = PROTECT(allocVector(INTSXP, N));
  int * restrict ansp = INTEGER(ans);

  for (R_xlen_t i = 0; i < N; ++i) {
    int n = length(xp[i]);
    if (n < 10) {
      ansp[i] = NA_INTEGER;
      continue;
    }
    const char * xi = CHAR(xp[i]);
    datetime2020 O = char2datetime_UTC(xi, n);
    ansp[i] = datetimei_syd(O);
  }
  UNPROTECT(1);
  return ans;

}


SEXP C_yyyymmdd_HHMMSS_SYD(SEXP x, SEXP y) {
  if (y == R_NilValue) {
    return do_yyyymmdd_HHMMSS_SYD(x);
  }
  return R_NilValue;

}

static unsigned int colon258(unsigned int * colon, const char * x) {
  unsigned int x3 = x[2], x5 = x[5];
  unsigned int c3 = colon[x3], c5 = colon[x5];
  return c3 + c5;
}

static unsigned int nondigits258(unsigned int * nondigits, const char * x) {
  unsigned int o = 0;
  for (int i = 0; i < 8; ++i) {
    o += nondigits[(unsigned int)x[i]];
  }
  return o;
}




SEXP C_isnt_HHcMMcSS(SEXP x, SEXP mm) {
  if (!isString(x)) {
    return ScalarLength(1);
  }
  R_xlen_t N = xlength(x);
  const SEXP * xp = STRING_PTR(x);

  // Verify HH:MM:SS
  unsigned int colon[256] = {0};
  const unsigned int colon_uc = (unsigned char)':';
  for (int i = 0; i < 256; ++i) {
    colon[i] = (i == colon_uc) ? 0 : 1;
  }
  unsigned int nondigits[256] = {0};
  for (int i = 0; i < 256; ++i) {
    nondigits[i] = !isdigit((unsigned char)i);
  }
  nondigits[colon_uc] = 0;

  for (R_xlen_t i = 0; i < N; ++i) {
    if (8 != length(xp[i])) {
      return ScalarLength(i + 1);
    }
    const char * xi = CHAR(xp[i]);
    unsigned int o = nondigits258(nondigits, xi) + colon258(colon, xi);
    if (o) {
      return ScalarLength(i + 1);
    }

  }
  return ScalarInteger(0);
}


SEXP C_basicTime(SEXP x, SEXP Round) {
  // round to nearest minute
  if (!isString(x)) {
    error("`x` was type '%s' but must be a STRSXP", type2char(TYPEOF(x)));
  }
  R_xlen_t N = xlength(x);
  const SEXP * xp = STRING_PTR(x);
  const int round = asInteger(Round);
  SEXP ans = PROTECT(allocVector(INTSXP, N));
  int * restrict ansp = INTEGER(ans);
  for (R_xlen_t i = 0; i < N; ++i) {
    if (8 != length(xp[i])) {
      ansp[i] = NA_INTEGER;
      continue;
    }
    const char * xi = CHAR(xp[i]);
    int o = 0;
    int HH = 10 * (xi[0] - '0') + (xi[1] - '0');
    o += HH * 60;
    if (round >= 60) {
      ansp[i] = o;
      continue;
    }
    if (round == 10) {
      o += 10 * (xi[3] - '0');
      ansp[i] = o;
      continue;
    }
  }
  UNPROTECT(1);
  return ans;
}


SEXP C_Seconds2String(SEXP x) {
  R_xlen_t N = xlength(x);
  if (N <= 0) {
    return R_NilValue;
  }
  const int * xp = INTEGER(x);
  int ox = xp[0];
  datetime2020 O = datetime2020_from_int(ox);
  Rprintf("%d-%d-%d %d:%d\n", O.yr2020 + 2020, O.month, O.day, O.hour, O.minute, O.second);
  return R_NilValue;


}




