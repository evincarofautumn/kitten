#include "kitten.h"
#include "debug.h"
#include <math.h>
#include <stdio.h>

Boxed pop  (Boxed stack);
void  push (Boxed stack, Boxed reference);
Boxed top  (Boxed stack);

#define OPERATOR_IMPLEMENTATION(NAME, SYMBOL)                               \
void kitten_##NAME(Boxed stack) {                                           \
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
void kitten_mod(Boxed stack) {
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

void kitten_apply(Boxed stack) {
  assert(stack);
  assert(is_quotation(stack));
  Boxed a = pop(stack);
  quotation_apply(stack, a);
  boxed_free(a);
}

void kitten_compose(Boxed stack) {
  assert(stack);
  assert(is_quotation(stack));
  Boxed a = pop(stack);
  quotation_append(top(stack), a);
  boxed_free(a);
}

void kitten_dup(Boxed stack) {
  assert(stack);
  assert(is_quotation(stack));
  Boxed a = boxed_copy(top(stack));
  quotation_push(stack, a);
}

void kitten_eq(Boxed stack) {
  assert(stack);
  assert(is_quotation(stack));
  Boxed b = pop(stack);
  Boxed a = pop(stack);
  push(stack, integer_new(boxed_compare(a, b) == 0));
}

void kitten_ge(Boxed stack) {
  assert(stack);
  assert(is_quotation(stack));
  Boxed b = pop(stack);
  Boxed a = pop(stack);
  push(stack, integer_new(boxed_compare(a, b) >= 0));
}

void kitten_gt(Boxed stack) {
  assert(stack);
  assert(is_quotation(stack));
  Boxed b = pop(stack);
  Boxed a = pop(stack);
  push(stack, integer_new(boxed_compare(a, b) > 0));
}

void kitten_if(Boxed stack) {
  assert(stack);
  assert(is_quotation(stack));
  Boxed else_branch = pop(stack);
  Boxed then_branch = pop(stack);
  Boxed condition = pop(stack);
  assert(is_integer(condition));
  if (integer_unbox(condition)) {
    boxed_free(else_branch);
    push(stack, then_branch);
    kitten_apply(stack);
  } else {
    boxed_free(then_branch);
    push(stack, else_branch);
    kitten_apply(stack);
  }
}

void kitten_isf(Boxed stack) {
  assert(stack);
  assert(is_quotation(stack));
  Boxed a = pop(stack);
  push(stack, integer_new(is_float(a)));
  boxed_free(a);
}

void kitten_isi(Boxed stack) {
  assert(stack);
  assert(is_quotation(stack));
  Boxed a = pop(stack);
  push(stack, integer_new(is_integer(a)));
  boxed_free(a);
}

void kitten_isq(Boxed stack) {
  assert(stack);
  assert(is_quotation(stack));
  Boxed a = pop(stack);
  push(stack, integer_new(is_quotation(a)));
  boxed_free(a);
}

void kitten_isw(Boxed stack) {
  assert(stack);
  assert(is_quotation(stack));
  Boxed a = pop(stack);
  push(stack, integer_new(is_word(a)));
  boxed_free(a);
}

void kitten_le(Boxed stack) {
  assert(stack);
  assert(is_quotation(stack));
  Boxed b = pop(stack);
  Boxed a = pop(stack);
  push(stack, integer_new(boxed_compare(a, b) <= 0));
}

void kitten_lt(Boxed stack) {
  assert(stack);
  assert(is_quotation(stack));
  Boxed b = pop(stack);
  Boxed a = pop(stack);
  push(stack, integer_new(boxed_compare(a, b) < 0));
}

void kitten_ne(Boxed stack) {
  assert(stack);
  assert(is_quotation(stack));
  Boxed b = pop(stack);
  Boxed a = pop(stack);
  push(stack, integer_new(boxed_compare(a, b) != 0));
}

void kitten_pop(Boxed stack) {
  assert(stack);
  assert(is_quotation(stack));
  boxed_free(quotation_pop(stack));
}

void kitten_quote(Boxed stack) {
  assert(stack);
  assert(is_quotation(stack));
  Boxed a = pop(stack);
  push(stack, quotation_new(1, a));
}

void kitten_swap(Boxed stack) {
  assert(stack);
  assert(is_quotation(stack));
  Boxed b = pop(stack);
  Boxed a = pop(stack);
  push(stack, b);
  push(stack, a);
}

void kitten_write(Boxed stack) {
  assert(stack);
  assert(is_quotation(stack));
  if (is_integer(top(stack))) {
    Boxed a = pop(stack);
    Integer value = integer_unbox(a);
    printf("%li", value);
  } else if (is_float(top(stack))) {
    Boxed a = pop(stack);
    Float value = float_unbox(a);
    printf("%f", value);
  }
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
