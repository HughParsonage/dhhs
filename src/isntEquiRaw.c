#include "dhhs.h"

bool isntRaw(SEXP x) {
  return TYPEOF(x) != RAWSXP;
}

bool isntRawOrNull(SEXP x) {
  return TYPEOF(x) != RAWSXP && TYPEOF(x) != NILSXP;
}

int isntEquiRaw(SEXP x, SEXP y) {
  if (isntRaw(x)) {
    return 1;
  }
  if (isntRaw(y)) {
    return 2;
  }
  if (xlength(x) != xlength(y)) {
    return 3;
  }
  return 0;
}

int isntEquiInt(SEXP x, SEXP y) {
  if (TYPEOF(x) != INTSXP) {
    return 1;
  }
  if (TYPEOF(y) != INTSXP) {
    return 2;
  }
  if (xlength(x) != xlength(y)) {
    return 3;
  }
  return 0;

}

