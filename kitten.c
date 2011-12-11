#include "kitten.h"
#include "debug.h"
#include <math.h>
#include <stdio.h>

Boxed pop  (Boxed stack);
void  push (Boxed stack, Boxed reference);
Boxed top  (Boxed stack);

#define OPERATOR_IMPLEMENTATION(NAME, SYMBOL, TYPE) \
void kitten_##NAME(Boxed stack) { \
  Boxed b = pop(stack); \
  Boxed a = pop(stack); \
  if (a->count == 1) { \
    assert(is_##TYPE(a)); \
    assert(is_##TYPE(b)); \
    a->value->data.as_##TYPE SYMBOL##= b->value->data.as_##TYPE; \
    boxed_free(b); \
    push(stack, a); \
  } else { \
    push(stack, TYPE##_new(TYPE##_unbox(a) SYMBOL TYPE##_unbox(b))); \
  } \
}

OPERATOR_IMPLEMENTATION(addf, +, float)
OPERATOR_IMPLEMENTATION(addi, +, integer)
OPERATOR_IMPLEMENTATION(subf, -, float)
OPERATOR_IMPLEMENTATION(subi, -, integer)
OPERATOR_IMPLEMENTATION(mulf, *, float)
OPERATOR_IMPLEMENTATION(muli, *, integer)
OPERATOR_IMPLEMENTATION(divf, /, float)
OPERATOR_IMPLEMENTATION(divi, /, integer)
OPERATOR_IMPLEMENTATION(modi, %, integer)

#undef OPERATOR_IMPLEMENTATION

/* Unfortunate specialization for floating-point modulus. */
void kitten_modf(Boxed stack) {
  assert(stack);
  assert(is_quotation(stack));
  Boxed b = pop(stack);
  Boxed a = pop(stack);
  if (a->count == 1) {
    assert(is_float(a));
    assert(is_float(b));
    a->value->data.as_float = fmod
      (a->value->data.as_float, b->value->data.as_float);
    boxed_free(b);
    push(stack, a);
  } else {
    push(stack, float_new(fmod(float_unbox(a), float_unbox(b))));
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
