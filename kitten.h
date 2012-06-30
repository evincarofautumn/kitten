#ifndef KITTEN_H
#define KITTEN_H
#include "debug.h"
#include "types.h"

#define DECLARATION(NAME) \
  void kitten_##NAME(Boxed stack, Boxed definitions)

DECLARATION(add);
DECLARATION(apply);
DECLARATION(compose);
DECLARATION(div);
DECLARATION(dup);
DECLARATION(eq);
DECLARATION(ge);
DECLARATION(gt);
DECLARATION(if);
DECLARATION(isf);
DECLARATION(isi);
DECLARATION(isq);
DECLARATION(isw);
DECLARATION(le);
DECLARATION(length);
DECLARATION(lt);
DECLARATION(mod);
DECLARATION(mul);
DECLARATION(ne);
DECLARATION(pop);
DECLARATION(putc);
DECLARATION(quote);
DECLARATION(sub);
DECLARATION(swap);
DECLARATION(trace);
DECLARATION(write);

#undef DECLARATION

void push(Boxed stack, Boxed reference);

/*
 * Literals.
 *
 * TODO: Use static initialization for literals.
 */
#define MKF(a)        float_new(a)
#define MKI(a)        integer_new(INT64_C(a))
#define MKQ(n, ...)   quotation_new(n, __VA_ARGS__)
#define MKW(a)        word_new(-a - 1)
#define PUSHF(a)      quotation_push(stack, MKF(a));
#define PUSHI(a)      quotation_push(stack, MKI(a));
#define PUSHQ(n, ...) quotation_push(stack, MKQ(n, __VA_ARGS__));

/*
 * Built-in words.
 *
 * TODO: Eliminate repetition.
 */
#define BUILTIN(w)  map[WORD_##w](stack, definitions);
#define ADD         BUILTIN(ADD)
#define APPLY       BUILTIN(APPLY)
#define COMPOSE     BUILTIN(COMPOSE)
#define DIV         BUILTIN(DIV)
#define DUP         BUILTIN(DUP)
#define EQ          BUILTIN(EQ)
#define GE          BUILTIN(GE)
#define GT          BUILTIN(GT)
#define IF          BUILTIN(IF)
#define ISF         BUILTIN(ISF)
#define ISI         BUILTIN(ISI)
#define ISQ         BUILTIN(ISQ)
#define ISW         BUILTIN(ISW)
#define LE          BUILTIN(LE)
#define LENGTH      BUILTIN(LENGTH)
#define LT          BUILTIN(LT)
#define MOD         BUILTIN(MOD)
#define MUL         BUILTIN(MUL)
#define NE          BUILTIN(NE)
#define POP         BUILTIN(POP)
#define PUTC        BUILTIN(PUTC)
#define QUOTE       BUILTIN(QUOTE)
#define SUB         BUILTIN(SUB)
#define SWAP        BUILTIN(SWAP)
#define TRACE       BUILTIN(TRACE)
#define WRITE       BUILTIN(WRITE)

/*
 * Word literals.
 *
 * TODO: Eliminate repetition and allow sharing.
 */
#define WADD        word_new(WORD_ADD)
#define WAPPLY      word_new(WORD_APPLY)
#define WCOMPOSE    word_new(WORD_COMPOSE)
#define WDIV        word_new(WORD_DIV)
#define WDUP        word_new(WORD_DUP)
#define WEQ         word_new(WORD_EQ)
#define WGE         word_new(WORD_GE)
#define WGT         word_new(WORD_GT)
#define WIF         word_new(WORD_IF)
#define WISF        word_new(WORD_ISF)
#define WISI        word_new(WORD_ISI)
#define WISQ        word_new(WORD_ISQ)
#define WISW        word_new(WORD_ISW)
#define WLE         word_new(WORD_LE)
#define WLENGTH     word_new(WORD_LENGTH)
#define WLT         word_new(WORD_LT)
#define WMOD        word_new(WORD_MOD)
#define WMUL        word_new(WORD_MUL)
#define WNE         word_new(WORD_NE)
#define WPOP        word_new(WORD_POP)
#define WPUTC       word_new(WORD_PUTC)
#define WQUOTE      word_new(WORD_QUOTE)
#define WSUB        word_new(WORD_SUB)
#define WSWAP       word_new(WORD_SWAP)
#define WTRACE      word_new(WORD_TRACE)
#define WWRITE      word_new(WORD_WRITE)

/*
 * Create definitions.
 */
#define DEF(a) quotation_push(definitions, a);

/*
 * Invoke definitions.
 *
 * TODO: Use a less fragile representation of definition bindings.
 */
#define DO(a) do {                        \
  assert(a >= 0);                         \
  word_apply(-a - 1, stack, definitions); \
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
