#ifndef KITTEN_H
#define KITTEN_H

#include "builtins.h"
#include "debug.h"
#include "types.h"

#define DECLARATION(NAME) \
  void kitten_##NAME(Boxed stack, Boxed definitions);

KITTEN_BUILTINS(DECLARATION, DECLARATION)

#undef DECLARATION

void push(Boxed stack, Boxed reference);

/*
 * Literals.
 *
 * TODO: Use static initialization for literals.
 */
#define MKF(VALUE)       float_new(VALUE)
#define MKI(VALUE)       integer_new(INT64_C(VALUE))
#define MKQ(SIZE, ...)   quotation_new(SIZE, __VA_ARGS__)
#define MKW(VALUE)       word_new(-VALUE - 1)
#define PUSHF(VALUE)     quotation_push(stack, MKF(VALUE));
#define PUSHI(VALUE)     quotation_push(stack, MKI(VALUE));
#define PUSHQ(SIZE, ...) quotation_push(stack, MKQ(SIZE, __VA_ARGS__));

/*
 * Convert the name of a built-in word to a call to its definition.
 */
#define BUILTIN(NAME) map[WORD_##NAME](stack, definitions);

/*
 * Create definitions.
 */
#define DEF(BODY) quotation_push(definitions, BODY);

/*
 * Invoke definitions.
 *
 * TODO: Use a less fragile representation of definition bindings.
 */
#define DO(ID) do {                        \
  assert(ID >= 0);                         \
  word_apply(-ID - 1, stack, definitions); \
} while (0);

/*
 * Skeleton program.
 */
#define KITTEN_PROGRAM(...)             \
int main(int argc, char** argv) {       \
  Boxed stack = quotation_new(0);       \
  Boxed definitions = quotation_new(0); \
  __VA_ARGS__                           \
  boxed_free(definitions);              \
  boxed_free(stack);                    \
  return 0;                             \
}

#endif
