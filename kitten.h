#ifndef KITTEN_H
#define KITTEN_H
#include "debug.h"
#include "types.h"

#define OPERATOR_DECLARATION(NAME) \
	void kitten_##NAME(Boxed stack, Boxed definitions)

OPERATOR_DECLARATION(add);
OPERATOR_DECLARATION(div);
OPERATOR_DECLARATION(mod);
OPERATOR_DECLARATION(mul);
OPERATOR_DECLARATION(sub);

#undef OPERATOR_DECLARATION

#define BUILTIN_DECLARATION(name) \
void kitten_##name(Boxed stack, Boxed definitions)

BUILTIN_DECLARATION(apply);
BUILTIN_DECLARATION(apply);
BUILTIN_DECLARATION(compose);
BUILTIN_DECLARATION(dup);
BUILTIN_DECLARATION(eq);
BUILTIN_DECLARATION(ge);
BUILTIN_DECLARATION(gt);
BUILTIN_DECLARATION(if);
BUILTIN_DECLARATION(isf);
BUILTIN_DECLARATION(isi);
BUILTIN_DECLARATION(isq);
BUILTIN_DECLARATION(isw);
BUILTIN_DECLARATION(le);
BUILTIN_DECLARATION(lt);
BUILTIN_DECLARATION(ne);
BUILTIN_DECLARATION(pop);
BUILTIN_DECLARATION(quote);
BUILTIN_DECLARATION(swap);
BUILTIN_DECLARATION(write);

#undef BUILTIN_DECLARATION

void push(Boxed stack, Boxed reference);

/* Literals. */
#define MKF(a)        float_new(a)
#define MKI(a)        integer_new(INT64_C(a))
#define MKQ(n, ...)   quotation_new(n, __VA_ARGS__)
#define MKW(a)        word_new(-a - 1)
#define PUSHF(a)      push(stack, MKF(a));
#define PUSHI(a)      push(stack, MKI(a));
#define PUSHQ(n, ...) push(stack, MKQ(n, __VA_ARGS__));
/* Built-in words. */
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
/* Word literals. */
#define WDUP        word_new(WORD_DUP)
#define WSWAP       word_new(WORD_SWAP)
#define WPOP        word_new(WORD_POP)
#define WQUOTE      word_new(WORD_QUOTE)
#define WCOMPOSE    word_new(WORD_COMPOSE)
#define WAPPLY      word_new(WORD_APPLY)
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

/* Create and invoke definitions. */

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
