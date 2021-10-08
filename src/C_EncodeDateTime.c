#include "dhhs.h"

#define typename(x) _Generic((x),                                                 \
_Bool: "_Bool",                  unsigned char: "unsigned char",                  \
char: "char",                     signed char: "signed char",                     \
short int: "short int",         unsigned short int: "unsigned short int",         \
int: "int",                     unsigned int: "unsigned int",                     \
long int: "long int",           unsigned long int: "unsigned long int",           \
long long int: "long long int", unsigned long long int: "unsigned long long int", \
float: "float",                         double: "double",                         \
long double: "long double",                   char *: "pointer to char",          \
void *: "pointer to void",                int *: "pointer to int",                \
default: "other")

SEXP C_EncodeDateTime(SEXP x, SEXP y, SEXP HzPerDay) {
  const static int IDAY_2021 = 18628;
  const int hz_per_day = asInteger(HzPerDay) > 0 ? asInteger(HzPerDay) : 24;
  R_xlen_t N = xlength(x);
  if (xlength(y) != N) {
    error("`length(x) = %ll` yet `length(y) = %ll`. Lengths must be equal.",
          xlength(x), xlength(y));
  }
  if (!isString(y)) {
    error("`y` was type '%s' but must be a character vector.", type2char(TYPEOF(y)));
  }

  const SEXP * yp = STRING_PTR(y);

  SEXP ans = PROTECT(allocVector(INTSXP, N));
  int * restrict ansp = INTEGER(ans);
  for (R_xlen_t i = 0; i < N; ++i) {
    int n = length(yp[i]);
    if (n != 8) {
      ansp[i] = NA_INTEGER;
      continue;
    }
    int day_seconds = 0;
    int hr, min, sec = 0;
    const char * w = CHAR(yp[i]);
    hr = 10 * (w[0] - '0') + (w[1] - '0');
    min = 10 * (w[3] - '0') + (w[4] - '0');
    sec = 10 * (w[6] - '0') + (w[7] - '0');

    int seconds_since_midnight;


  }




  return R_NilValue;
}

