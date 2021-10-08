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
