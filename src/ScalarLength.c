#include "dhhs.h"

SEXP ScalarLength(R_xlen_t x) {
  return x < INT_MAX ? ScalarInteger(x) : ScalarReal(x);
}
