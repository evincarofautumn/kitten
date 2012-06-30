#ifndef KITTEN_H
#define KITTEN_H
#include "debug.h"
#include "types.h"

#define DECLARATION(NAME) \
	void kitten_##NAME(Boxed stack, Boxed definitions)

DECLARATION(add);
DECLARATION(div);
DECLARATION(mod);
DECLARATION(mul);
DECLARATION(sub);

DECLARATION(apply);
DECLARATION(apply);
DECLARATION(compose);
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
DECLARATION(ne);
DECLARATION(pop);
DECLARATION(quote);
DECLARATION(swap);
DECLARATION(write);
DECLARATION(putc);
DECLARATION(trace);

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
#define PUSHF(a)      push(stack, MKF(a));
#define PUSHI(a)      push(stack, MKI(a));
#define PUSHQ(n, ...) push(stack, MKQ(n, __VA_ARGS__));
/*
 * Built-in words.
 *
 * TODO: Eliminate repetition.
 */
#define BUILTIN(w)  map[WORD_##w](stack, definitions);
#define DUP         BUILTIN(DUP)
#define SWAP        BUILTIN(SWAP)
#define POP         BUILTIN(POP)
#define QUOTE       BUILTIN(QUOTE)
#define COMPOSE     BUILTIN(COMPOSE)
#define APPLY       BUILTIN(APPLY)
#define ADD         BUILTIN(ADD)
#define SUB         BUILTIN(SUB)
#define MUL         BUILTIN(MUL)
#define DIV         BUILTIN(DIV)
#define MOD         BUILTIN(MOD)
#define LENGTH      BUILTIN(LENGTH)
#define ISF         BUILTIN(ISF)
#define ISI         BUILTIN(ISI)
#define ISQ         BUILTIN(ISQ)
#define ISW         BUILTIN(ISW)
#define EQ          BUILTIN(EQ)
#define NE          BUILTIN(NE)
#define LT          BUILTIN(LT)
#define GE          BUILTIN(GE)
#define GT          BUILTIN(GT)
#define LE          BUILTIN(LE)
#define IF          BUILTIN(IF)
#define WRITE       BUILTIN(WRITE)
#define PUTC        BUILTIN(PUTC)
#define TRACE       BUILTIN(TRACE)
/* Word literals. */
#define WDUP        word_new(WORD_DUP)
#define WSWAP       word_new(WORD_SWAP)
#define WPOP        word_new(WORD_POP)
#define WQUOTE      word_new(WORD_QUOTE)
#define WCOMPOSE    word_new(WORD_COMPOSE)
#define WAPPLY      word_new(WORD_APPLY)
#define WLENGTH     word_new(WORD_LENGTH)
#define WADD        word_new(WORD_ADD)
#define WSUB        word_new(WORD_SUB)
#define WMUL        word_new(WORD_MUL)
#define WDIV        word_new(WORD_DIV)
#define WMOD        word_new(WORD_MOD)
#define WISF        word_new(WORD_ISF)
#define WISI        word_new(WORD_ISI)
#define WISQ        word_new(WORD_ISQ)
#define WISW        word_new(WORD_ISW)
#define WEQ         word_new(WORD_EQ)
#define WNE         word_new(WORD_NE)
#define WLT         word_new(WORD_LT)
#define WGE         word_new(WORD_GE)
#define WGT         word_new(WORD_GT)
#define WLE         word_new(WORD_LE)
#define WIF         word_new(WORD_IF)
#define WWRITE      word_new(WORD_WRITE)
#define WPUTC       word_new(WORD_PUTC)
#define WTRACE      word_new(WORD_TRACE)

/*
 * Create and invoke definitions.
 *
 * TODO: Use a less fragile representation of definition bindings.
 */

#define DEF(a) quotation_push(definitions, a);

#define DO(a) do {                        \
  assert(a >= 0);                         \
  word_apply(-a - 1, stack, definitions); \
} while (0);

/* Skeleton program. */

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
