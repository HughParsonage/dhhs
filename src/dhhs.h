#ifndef dhhs_H
#define dhhs_H

#include <R.h>
#define USE_RINTERNALS
#include <Rinternals.h>
#include <stdint.h> // for uint64_t rather than unsigned long long
#include <stdbool.h>
#include <math.h>
#include <ctype.h>

// allocate.c
SEXP logicalN(R_xlen_t N, int a);

// character.c
bool char_is_number(char x);
int char12_to_int(const char * x);
int char2int(const char * x, int s);
bool all_digits(const char * x, int nchari) ;
int n_digits0(unsigned int x);
int char2number(char x);
unsigned int alphnum2uint(char x);

// isntEquiRaw
int isntEquiRaw(SEXP x, SEXP y);
bool isntRaw(SEXP x);
bool isntRawOrNull(SEXP x);
int isntEquiInt(SEXP x, SEXP y);

// Cfast_nchar
int is_const_nchar(SEXP x);

// ScalarLength.c
SEXP ScalarLength(R_xlen_t x);

#endif
