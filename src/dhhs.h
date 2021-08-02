#ifndef dhhs_H
#define dhhs_H

#include <R.h>
#define USE_RINTERNALS
#include <Rinternals.h>
#include <stdint.h> // for uint64_t rather than unsigned long long
#include <stdbool.h>
#include <math.h>
#include <ctype.h>

// character.c
bool char_is_number(char x);
int char12_to_int(const char * x);
int char2int(const char * x, int s);
bool all_digits(const char * x, int nchari) ;
int n_digits0(unsigned int x);
int char2number(char x);

#endif
