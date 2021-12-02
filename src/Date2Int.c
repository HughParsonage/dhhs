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

SEXP C_yyyy_mm_dd_HHMMSS_UTC(SEXP xx) {
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



