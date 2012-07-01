#include "kitten.h"

#include "debug.h"

#include <inttypes.h>
#include <math.h>
#include <stdio.h>

#define OPERATOR_IMPLEMENTATION(NAME, SYMBOL)                               \
void kitten_##NAME(Boxed stack, Boxed definitions) {                        \
  Boxed unpromoted_b = quotation_pop(stack);                                \
  Boxed unpromoted_a = quotation_pop(stack);                                \
  Boxed a;                                                                  \
  Boxed b;                                                                  \
  int compatible_types = boxed_promote(unpromoted_a, unpromoted_b, &a, &b); \
  assert(compatible_types);                                                 \
  assert(is_numeric(a) && is_numeric(b));                                   \
  switch (boxed_type(a)) {                                                  \
  case FLOAT:                                                               \
    quotation_push(stack,                                                   \
      float_new(float_unbox(a) SYMBOL float_unbox(b)));                     \
    break;                                                                  \
  case INTEGER:                                                             \
    quotation_push(stack,                                                   \
      integer_new(integer_unbox(a) SYMBOL integer_unbox(b)));               \
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

void kitten_apply(Boxed stack, Boxed definitions) {
  assert(stack);
  assert(is_quotation(stack));
  Boxed a = quotation_pop(stack);
  quotation_apply(stack, a, definitions);
  boxed_free(a);
}

void kitten_compose(Boxed stack, Boxed definitions) {
  assert(stack);
  assert(is_quotation(stack));
  Boxed a = quotation_pop(stack);
  if (quotation_top(stack)->count == 1) {
    quotation_append(quotation_top(stack), a);
  } else {
    Boxed b = quotation_pop(stack);
    assert(is_quotation(b));
    Boxed c = boxed_clone(b);
    boxed_free(b);
    quotation_append(c, a);
    quotation_push(stack, c);
  }
  boxed_free(a);
}

void kitten_dup(Boxed stack, Boxed definitions) {
  assert(stack);
  assert(is_quotation(stack));
  Boxed a = boxed_copy(quotation_top(stack));
  quotation_push(stack, a);
}

void kitten_eq(Boxed stack, Boxed definitions) {
  assert(stack);
  assert(is_quotation(stack));
  Boxed b = quotation_pop(stack);
  Boxed a = quotation_pop(stack);
  quotation_push(stack, integer_new(boxed_compare(a, b) == 0));
}

void kitten_ge(Boxed stack, Boxed definitions) {
  assert(stack);
  assert(is_quotation(stack));
  Boxed b = quotation_pop(stack);
  Boxed a = quotation_pop(stack);
  quotation_push(stack, integer_new(boxed_compare(a, b) >= 0));
}

void kitten_gt(Boxed stack, Boxed definitions) {
  assert(stack);
  assert(is_quotation(stack));
  Boxed b = quotation_pop(stack);
  Boxed a = quotation_pop(stack);
  quotation_push(stack, integer_new(boxed_compare(a, b) > 0));
}

void kitten_if(Boxed stack, Boxed definitions) {
  assert(stack);
  assert(is_quotation(stack));
  Boxed else_branch = quotation_pop(stack);
  Boxed then_branch = quotation_pop(stack);
  Boxed condition = quotation_pop(stack);
  assert(is_integer(condition));
  if (integer_unbox(condition)) {
    boxed_free(else_branch);
    quotation_push(stack, then_branch);
    kitten_apply(stack, definitions);
  } else {
    boxed_free(then_branch);
    quotation_push(stack, else_branch);
    kitten_apply(stack, definitions);
  }
}

void kitten_isf(Boxed stack, Boxed definitions) {
  assert(stack);
  assert(is_quotation(stack));
  Boxed a = quotation_pop(stack);
  quotation_push(stack, integer_new(is_float(a)));
  boxed_free(a);
}

void kitten_isi(Boxed stack, Boxed definitions) {
  assert(stack);
  assert(is_quotation(stack));
  Boxed a = quotation_pop(stack);
  quotation_push(stack, integer_new(is_integer(a)));
  boxed_free(a);
}

void kitten_isq(Boxed stack, Boxed definitions) {
  assert(stack);
  assert(is_quotation(stack));
  Boxed a = quotation_pop(stack);
  quotation_push(stack, integer_new(is_quotation(a)));
  boxed_free(a);
}

void kitten_isw(Boxed stack, Boxed definitions) {
  assert(stack);
  assert(is_quotation(stack));
  Boxed a = quotation_pop(stack);
  quotation_push(stack, integer_new(is_word(a)));
  boxed_free(a);
}

void kitten_le(Boxed stack, Boxed definitions) {
  assert(stack);
  assert(is_quotation(stack));
  Boxed b = quotation_pop(stack);
  Boxed a = quotation_pop(stack);
  quotation_push(stack, integer_new(boxed_compare(a, b) <= 0));
}

void kitten_lt(Boxed stack, Boxed definitions) {
  assert(stack);
  assert(is_quotation(stack));
  Boxed b = quotation_pop(stack);
  Boxed a = quotation_pop(stack);
  quotation_push(stack, integer_new(boxed_compare(a, b) < 0));
}

void kitten_length(Boxed stack, Boxed definitions) {
  assert(stack);
  assert(is_quotation(stack));
  Boxed a = quotation_pop(stack);
  quotation_push(stack, integer_new(quotation_size(a)));
  boxed_free(a);
}

/* Unfortunate specialization for floating-point modulus. */
void kitten_mod(Boxed stack, Boxed definitions) {
  Boxed unpromoted_b = quotation_pop(stack);
  Boxed unpromoted_a = quotation_pop(stack);
  Boxed a;
  Boxed b;
  int compatible_types = boxed_promote(unpromoted_a, unpromoted_b, &a, &b);
  assert(compatible_types);
  assert(is_numeric(a) && is_numeric(b));
  switch (boxed_type(a)) {
  case FLOAT:
    quotation_push(stack, float_new(fmod(float_unbox(a), float_unbox(b))));
    break;
  case INTEGER:
    quotation_push(stack, integer_new(integer_unbox(a) % integer_unbox(b)));
    break;
  default:
    break;
  }
}

void kitten_ne(Boxed stack, Boxed definitions) {
  assert(stack);
  assert(is_quotation(stack));
  Boxed b = quotation_pop(stack);
  Boxed a = quotation_pop(stack);
  quotation_push(stack, integer_new(boxed_compare(a, b) != 0));
}

void kitten_pop(Boxed stack, Boxed definitions) {
  assert(stack);
  assert(is_quotation(stack));
  boxed_free(quotation_pop(stack));
}

void kitten_putc(Boxed stack, Boxed definitions) {
  assert(stack);
  assert(is_quotation(stack));
  assert(is_integer(quotation_top(stack)));
  Boxed a = quotation_pop(stack);
  boxed_putc(a);
  boxed_free(a);
}

void kitten_quote(Boxed stack, Boxed definitions) {
  assert(stack);
  assert(is_quotation(stack));
  Boxed a = quotation_pop(stack);
  quotation_push(stack, quotation_new(1, a));
}

void kitten_swap(Boxed stack, Boxed definitions) {
  assert(stack);
  assert(is_quotation(stack));
  Boxed b = quotation_pop(stack);
  Boxed a = quotation_pop(stack);
  quotation_push(stack, b);
  quotation_push(stack, a);
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
    default:
      break;
    }
  }
  printf("] ");
}

void kitten_write(Boxed stack, Boxed definitions) {
  assert(stack);
  assert(is_quotation(stack));
  Boxed a = quotation_pop(stack);
  boxed_write(a);
  boxed_free(a);
}
