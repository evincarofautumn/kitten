#ifndef DEBUG_H
#define DEBUG_H
#include <assert.h>
#include <stdio.h>

#ifdef DEBUG
  #define trace(...) fprintf(stderr, __VA_ARGS__)
  void global_alloc(void);
  void global_free(void);
#else
  #define trace(...)
  #define global_alloc()
  #define global_free()
#endif

#endif
