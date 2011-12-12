#include <stdio.h>
#include "debug.h"
#include "kitten.h"
#include "types.h"

/* Literals. */
#define PUSHF(a)      push(stack, float_new(a));
#define PUSHI(a)      push(stack, integer_new(a));
#define PUSHQ(n, ...) push(stack, quotation_new(n, __VA_ARGS__));
/* Built-in words. */
#define DUP         map[WORD_DUP]       (stack);
#define SWAP        map[WORD_SWAP]      (stack);
#define POP         map[WORD_POP]       (stack);
#define QUOTE       map[WORD_QUOTE]     (stack);
#define COMPOSE     map[WORD_COMPOSE]   (stack);
#define APPLY       map[WORD_APPLY]     (stack);
#define ADD         map[WORD_ADD]       (stack);
#define SUB         map[WORD_SUB]       (stack);
#define MUL         map[WORD_MUL]       (stack);
#define DIV         map[WORD_DIV]       (stack);
#define MOD         map[WORD_MOD]       (stack);
#define ISF         map[WORD_ISF]       (stack);
#define ISI         map[WORD_ISI]       (stack);
#define ISQ         map[WORD_ISQ]       (stack);
#define ISW         map[WORD_ISW]       (stack);
#define EQ          map[WORD_EQ]        (stack);
#define NE          map[WORD_NE]        (stack);
#define LT          map[WORD_LT]        (stack);
#define GE          map[WORD_GE]        (stack);
#define GT          map[WORD_GT]        (stack);
#define LE          map[WORD_LE]        (stack);
#define WRITE       map[WORD_WRITE]     (stack);
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
#define WWRITE      word_new(WORD_WRITE)

int main(int argc, char** argv) {
  Boxed stack = quotation_new(0);
  PUSHF(10.0) PUSHF(8.0) DIV WRITE
  printf("\n");
  boxed_free(stack);
  return 0;
}
