#include "dhhs.h"

const static int MONTHDAYS[13] = {0, 31, 28, 31,  30,  31,  30,  31,  31,  30,  31,  30, 31};
const static int MONTHDAYC[12] = {0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334};

const static int IDAY_2020 = 18262;

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

SEXP CDate2Int(SEXP xx) {
  R_xlen_t N = xlength(xx);
  if (!isString(xx)) {
    return xx;
  }
  SEXP ans = PROTECT(allocVector(INTSXP, N));
  int * restrict ansp = INTEGER(ans);

  for (R_xlen_t i = 0; i < N; ++i) {
    int len = length(STRING_ELT(xx, i));
    if (STRING_ELT(xx, i) == NA_STRING || len != 10) {
      ansp[i] = NA_INTEGER;
      continue;
    }

    const char * x = CHAR(STRING_ELT(xx, i));
    int mday = 0;
    int month = 0;
    int year = 0;

    mday += 10 * (x[0] - '0') + (x[1] - '0');
    month += (x[3] == '1' ? 10 : 0) + (x[4] - '0');
    year += (x[6] - '0') * 1000;
    year += (x[7] - '0') * 100;
    year += (x[8] - '0') * 10;
    year += (x[9] - '0');

    int is_leap_yr = ((year & 3u) == 0) && (year % 100);
    is_leap_yr |= (year % 400) == 0;

    int yday = MONTHDAYC[month - 1] + mday + (is_leap_year(year) && month > 2);
    int years_abv_2020 = year - 2020;
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
    o += yday;
    ansp[i] = o;
  }
  UNPROTECT(1);
  return ans;
}
