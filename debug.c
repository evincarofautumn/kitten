#include "debug.h"
#ifdef DEBUG

static int global_reference_count;

void global_alloc(void) {
  ++global_reference_count;
}

void global_free(void) {
  --global_reference_count;
}

#endif
