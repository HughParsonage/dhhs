#include "dhhs.h"

static int tens[10] = {1, 10, 100, 1000, 10000, 100000, 1000000, 10000000, 100000000, 1000000000};

bool char_is_number(char x) {
  unsigned int ux = x - '0';
  return ux <= 9;
}

int char12_to_int(const char * x) {
  int o = 0;
  int ten = 1;
  for (int i = 11; i >= 4; --i) {
    o += ten * (x[i] - '0');
    ten *= 10;
  }
  return o;
}

int char2int(const char * x, int s) {
  int o = 0;
  int ten = 1;
  for (int i = s - 1; i >= 0; --i) {
    o += ten * (x[i] - '0');
    ten *= 10;
  }
  return o;
}

bool all_digits(const char * x, int nchari) {
  for (int j = 0; j < nchari; ++j) {
    if (!char_is_number(x[j])) {
      return false;
    }
  }
  return true;
}

int ipow10(int n) {
  unsigned int j = n % 10U;
  return tens[j];
}

int n_digits0(unsigned int x) {
  if (x >= 1000000000U) return 10;
  if (x >= 100000000U)  return 9;
  if (x >= 10000000U)   return 8;
  if (x >= 1000000U)    return 7;
  if (x >= 100000U)     return 6;
  if (x >= 10000U)      return 5;
  if (x >= 1000U)       return 4;
  if (x >= 100U)        return 3;
  if (x >= 10U)         return 2;
  return 1;
}

int n_chars(int x) {
  if (x == 0) {
    return 1;
  } else if (x > 0) {
    if (x >= 1000000000U) return 10;
    if (x >= 100000000U)  return 9;
    if (x >= 10000000U)   return 8;
    if (x >= 1000000U)    return 7;
    if (x >= 100000U)     return 6;
    if (x >= 10000U)      return 5;
    if (x >= 1000U)       return 4;
    if (x >= 100U)        return 3;
    if (x >= 10U)         return 2;
    return 1;
  } else {
    if (x == NA_INTEGER) return NA_INTEGER;
    if (x <= -1000000000) return 11;
    if (x <= -100000000)  return 10;
    if (x <= -10000000)   return 9;
    if (x <= -1000000)    return 8;
    if (x <= -100000)     return 7;
    if (x <= -10000)      return 6;
    if (x <= -1000)       return 5;
    if (x <= -100)        return 4;
    if (x <= -10)         return 3;
  }
  return 2;
}

