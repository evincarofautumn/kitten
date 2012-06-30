#include "kitten.h"
#include "debug.h"
#include <math.h>
#include <inttypes.h>
#include <stdio.h>

Boxed pop  (Boxed stack);
void  push (Boxed stack, Boxed reference);
Boxed top  (Boxed stack);

#define OPERATOR_IMPLEMENTATION(NAME, SYMBOL)                               \
void kitten_##NAME(Boxed stack, Boxed definitions) {                        \
  Boxed unpromoted_b = pop(stack);                                          \
  Boxed unpromoted_a = pop(stack);                                          \
  Boxed a;                                                                  \
  Boxed b;                                                                  \
  int compatible_types = boxed_promote(unpromoted_a, unpromoted_b, &a, &b); \
  assert(compatible_types);                                                 \
  assert(is_numeric(a) && is_numeric(b));                                   \
  switch (boxed_type(a)) {                                                  \
  case FLOAT:                                                               \
    push(stack, float_new(float_unbox(a) SYMBOL float_unbox(b)));           \
    break;                                                                  \
  case INTEGER:                                                             \
    push(stack, integer_new(integer_unbox(a) SYMBOL integer_unbox(b)));     \
    break;                                                                  \
  default:                                                                  \
    break;                                                                  \
  }                                                                         \
}

OPERATOR_IMPLEMENTATION(add, +)
OPERATOR_IMPLEMENTATION(sub, -)
OPERATOR_IMPLEMENTATION(mul, *)
OPERATOR_IMPLEMENTATION(div, /)

#undef OPERATOR_IMPLEMENTATION

/* Unfortunate specialization for floating-point modulus. */
void kitten_mod(Boxed stack, Boxed definitions) {
  Boxed unpromoted_b = pop(stack);
  Boxed unpromoted_a = pop(stack);
  Boxed a;
  Boxed b;
  int compatible_types = boxed_promote(unpromoted_a, unpromoted_b, &a, &b);
  assert(compatible_types);
  assert(is_numeric(a) && is_numeric(b));
  switch (boxed_type(a)) {
  case FLOAT:
    push(stack, float_new(fmod(float_unbox(a), float_unbox(b))));
    break;
  case INTEGER:
    push(stack, integer_new(integer_unbox(a) % integer_unbox(b)));
    break;
  default:
    break;
  }
}

void kitten_apply(Boxed stack, Boxed definitions) {
  assert(stack);
  assert(is_quotation(stack));
  Boxed a = pop(stack);
  quotation_apply(stack, a, definitions);
  boxed_free(a);
}

void kitten_compose(Boxed stack, Boxed definitions) {
  assert(stack);
  assert(is_quotation(stack));
  Boxed a = pop(stack);
  if (top(stack)->count == 1) {
    quotation_append(top(stack), a);
  } else {
    Boxed b = pop(stack);
    assert(is_quotation(b));
    Boxed c = boxed_clone(b);
    boxed_free(b);
    quotation_append(c, a);
    push(stack, c);
  }
  boxed_free(a);
}

void kitten_dup(Boxed stack, Boxed definitions) {
  assert(stack);
  assert(is_quotation(stack));
  Boxed a = boxed_copy(top(stack));
  quotation_push(stack, a);
}

void kitten_eq(Boxed stack, Boxed definitions) {
  assert(stack);
  assert(is_quotation(stack));
  Boxed b = pop(stack);
  Boxed a = pop(stack);
  push(stack, integer_new(boxed_compare(a, b) == 0));
}

void kitten_ge(Boxed stack, Boxed definitions) {
  assert(stack);
  assert(is_quotation(stack));
  Boxed b = pop(stack);
  Boxed a = pop(stack);
  push(stack, integer_new(boxed_compare(a, b) >= 0));
}

void kitten_gt(Boxed stack, Boxed definitions) {
  assert(stack);
  assert(is_quotation(stack));
  Boxed b = pop(stack);
  Boxed a = pop(stack);
  push(stack, integer_new(boxed_compare(a, b) > 0));
}

void kitten_if(Boxed stack, Boxed definitions) {
  assert(stack);
  assert(is_quotation(stack));
  Boxed else_branch = pop(stack);
  Boxed then_branch = pop(stack);
  Boxed condition = pop(stack);
  assert(is_integer(condition));
  if (integer_unbox(condition)) {
    boxed_free(else_branch);
    push(stack, then_branch);
    kitten_apply(stack, definitions);
  } else {
    boxed_free(then_branch);
    push(stack, else_branch);
    kitten_apply(stack, definitions);
  }
}

void kitten_isf(Boxed stack, Boxed definitions) {
  assert(stack);
  assert(is_quotation(stack));
  Boxed a = pop(stack);
  push(stack, integer_new(is_float(a)));
  boxed_free(a);
}

void kitten_isi(Boxed stack, Boxed definitions) {
  assert(stack);
  assert(is_quotation(stack));
  Boxed a = pop(stack);
  push(stack, integer_new(is_integer(a)));
  boxed_free(a);
}

void kitten_isq(Boxed stack, Boxed definitions) {
  assert(stack);
  assert(is_quotation(stack));
  Boxed a = pop(stack);
  push(stack, integer_new(is_quotation(a)));
  boxed_free(a);
}

void kitten_isw(Boxed stack, Boxed definitions) {
  assert(stack);
  assert(is_quotation(stack));
  Boxed a = pop(stack);
  push(stack, integer_new(is_word(a)));
  boxed_free(a);
}

void kitten_le(Boxed stack, Boxed definitions) {
  assert(stack);
  assert(is_quotation(stack));
  Boxed b = pop(stack);
  Boxed a = pop(stack);
  push(stack, integer_new(boxed_compare(a, b) <= 0));
}

void kitten_lt(Boxed stack, Boxed definitions) {
  assert(stack);
  assert(is_quotation(stack));
  Boxed b = pop(stack);
  Boxed a = pop(stack);
  push(stack, integer_new(boxed_compare(a, b) < 0));
}

void kitten_length(Boxed stack, Boxed definitions) {
  assert(stack);
  assert(is_quotation(stack));
  Boxed a = pop(stack);
  push(stack, integer_new(quotation_size(a)));
  boxed_free(a);
}

void kitten_ne(Boxed stack, Boxed definitions) {
  assert(stack);
  assert(is_quotation(stack));
  Boxed b = pop(stack);
  Boxed a = pop(stack);
  push(stack, integer_new(boxed_compare(a, b) != 0));
}

void kitten_pop(Boxed stack, Boxed definitions) {
  assert(stack);
  assert(is_quotation(stack));
  boxed_free(quotation_pop(stack));
}

void kitten_quote(Boxed stack, Boxed definitions) {
  assert(stack);
  assert(is_quotation(stack));
  Boxed a = pop(stack);
  push(stack, quotation_new(1, a));
}

void kitten_swap(Boxed stack, Boxed definitions) {
  assert(stack);
  assert(is_quotation(stack));
  Boxed b = pop(stack);
  Boxed a = pop(stack);
  push(stack, b);
  push(stack, a);
}

void kitten_trace(Boxed stack, Boxed definitions) {
  assert(stack);
  assert(is_quotation(stack));
  printf("[ ");
  int i;
  for (i = 0; i < quotation_size(stack); ++i) {
    Boxed current = quotation_data(stack)[i];
    switch (boxed_type(current)) {
    case FLOAT:
      printf("%p:%fF ", current, float_value(current));
      break;
    case INTEGER:
      printf("%p:%" PRId64 "I ", current, integer_value(current));
      break;
    case WORD:
      printf("%p:%dW ", current, word_value(current));
      break;
    case QUOTATION:
      printf("%p:", current);
      kitten_trace(current, definitions);
      break;
    }
  }
  printf("] ");
}

void kitten_write(Boxed stack, Boxed definitions) {
  assert(stack);
  assert(is_quotation(stack));
  Boxed a = pop(stack);
  boxed_write(a);
  boxed_free(a);
}

void kitten_putc(Boxed stack, Boxed definitions) {
  assert(stack);
  assert(is_quotation(stack));
  assert(is_integer(top(stack)));
  Boxed a = pop(stack);
  boxed_putc(a);
  boxed_free(a);
}

Boxed pop(Boxed stack) {
  assert(stack);
  assert(is_quotation(stack));
  return quotation_pop(stack);
}

void push(Boxed stack, Boxed reference) {
  assert(stack);
  assert(is_quotation(stack));
  quotation_push(stack, reference);
}

Boxed top(Boxed stack) {
  assert(stack);
  assert(is_quotation(stack));
  return quotation_top(stack);
}
