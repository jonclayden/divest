#ifndef _R_PRINT_H_
#define _R_PRINT_H_

#include <stdarg.h>

#ifdef HAVE_R

#define R_USE_C99_IN_CXX

#include <R_ext/Print.h>

#define printMessage Rprintf
#define printWarning(...) { REprintf("Warning: "); REprintf(__VA_ARGS__); }
#define printError(...) { REprintf("Error: "); REprintf(__VA_ARGS__); }

#else

#define printMessage printf
#define printWarning(...) { printf("Warning: "); printf(__VA_ARGS__); }
#define printError(...) { printf("Error: "); printf(__VA_ARGS__); }

#endif

#endif
