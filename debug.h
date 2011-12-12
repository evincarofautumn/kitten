#ifndef DEBUG_H
#define DEBUG_H
#include <assert.h>
#include <stdio.h>

#ifdef DEBUG
  #define trace printf
  void global_alloc(void);
  void global_free(void);
#else
  #define trace
  #define global_alloc()
  #define global_free()
#endif

#endif
