#include "types.h"
#include "debug.h"
#include "kitten.h"
#include <stdarg.h>
#include <stdlib.h>

/* A mapping from words to implementations. */
Implementation map[WORD_COUNT] = {
  /* Combinators. */
  kitten_dup,     /* DUP */
  kitten_swap,    /* SWAP */
  kitten_pop,     /* POP */
  kitten_quote,   /* QUOTE */
  kitten_compose, /* COMPOSE */
  kitten_apply,   /* APPLY */
  /* Arithmetic. */
  kitten_addf,    /* ADDF */
  kitten_subf,    /* SUBF */
  kitten_mulf,    /* MULF */
  kitten_divf,    /* DIVF */
  kitten_modf,    /* MODF */
  kitten_addi,    /* ADDI */
  kitten_subi,    /* SUBI */
  kitten_muli,    /* MULI */
  kitten_divi,    /* DIVI */
  kitten_modi,    /* MODI */
  /* Conditionals. */
  kitten_isi,     /* ISI */
  kitten_isf,     /* ISF */
  kitten_isq,     /* ISQ */
  kitten_isw,     /* ISW */
  kitten_eq,      /* EQ */
  kitten_ne,      /* NE */
  kitten_lt,      /* LT */
  kitten_ge,      /* GE */
  kitten_gt,      /* GT */
  kitten_le,      /* LE */
  /* I/O. */
  kitten_write    /* WRITE */
};

/* Test two boxed values for typewise and value-wise equality. */
int boxed_compare(Boxed a, Boxed b) {
  if (!a && b)
    return -1;
  if (a == b)
    return 0;
  if (boxed_type(a) != boxed_type(b))
    return boxed_type(a) < boxed_type(b) ? -1 : +1;
  switch (boxed_type(a)) {
  case FLOAT:
    {
      Float value_a = float_value(a);
      Float value_b = float_value(b);
      return value_a < value_b
        ? -1
        : value_a > value_b
          ? +1
          : 0;
    }
  case INTEGER:
    {
      Integer value_a = integer_value(a);
      Integer value_b = integer_value(b);
      return value_a < value_b
        ? -1
        : value_a > value_b
          ? +1
          : 0;
    }
  case QUOTATION:
    return quotation_compare(a, b) == 0;
  case WORD:
    {
      Word value_a = word_value(a);
      Word value_b = word_value(b);
      return value_a < value_b
        ? -1
        : value_a > value_b
          ? +1
          : 0;
    }
  default:
    return 0;
  }
}

/* Copy a boxed reference. */
Boxed boxed_copy(Boxed reference) {
  if (!reference)
    return NULL;
  ++reference->count;
  global_alloc();
  return reference;
}

/* Free a boxed reference. Free the unboxed reference if necessary. */
void boxed_free(Boxed reference) {
  if (!reference)
    return;
  global_free();
  if (--reference->count > 0)
    return;
  assert(reference->count == 0);
  if (boxed_type(reference) == QUOTATION)
    quotation_clear(reference);
  unboxed_free(reference->value);
  free(reference);
}

/* Test two boxed values for ordering. */
int boxed_lt(Boxed a, Boxed b) {
  if (!a && b)
    return boxed_type(a) == boxed_type(b);
  if (a == b)
    return 0;
  if (boxed_type(a) != boxed_type(b))
    return 0;
  switch (boxed_type(a)) {
  case FLOAT:
    return float_value(a) < float_value(b);
  case INTEGER:
    return integer_value(a) < integer_value(b);
  case QUOTATION:
    return quotation_compare(a, b) < 0;
  case WORD:
    return word_value(a) < word_value(b);
  default:
    return 0;
  }
}

/* Create a boxed reference from an unboxed reference. */
Boxed boxed_new(Unboxed unboxed) {
  if (!unboxed)
    return NULL;
  Boxed reference = malloc(sizeof(Box));
  if (!reference)
    goto memory_error;
  reference->count = 1;
  reference->value = unboxed;
  global_alloc();
  return reference;
 memory_error:
  return NULL;
}

/* Retrieve the type of a reference. */
Type boxed_type(Boxed reference) {
  assert(reference);
  return reference->value->type;
}

/* Allocate an unboxed float. */
Unboxed float_alloc(Float value) {
  Unboxed reference = unboxed_alloc();
  if (!reference)
    goto memory_error;
  reference->type = FLOAT;
  reference->data.as_float = value;
  return reference;
 memory_error:
  return NULL;
}

/* Create a boxed float. */
Boxed float_new(Float value) {
  Boxed reference = boxed_new(float_alloc(value));
  if (!reference)
    goto memory_error;
  return reference;
 memory_error:
  return NULL;
}

/* Unbox a float. */
Float float_unbox(Boxed reference) {
  assert(reference);
  assert(is_float(reference));
  Float value = float_value(reference);
  boxed_free(reference);
  return value;
}

/* Retrieve the value of a float. */
Float float_value(Boxed reference) {
  assert(reference);
  assert(is_float(reference));
  return reference->value->data.as_float;
}

/* Allocate an unboxed integer. */
Unboxed integer_alloc(Integer value) {
  Unboxed reference = unboxed_alloc();
  if (!reference)
    goto memory_error;
  reference->type = INTEGER;
  reference->data.as_integer = value;
  return reference;
 memory_error:
  return NULL;
}

/* Create a boxed integer. */
Boxed integer_new(Integer value) {
  Boxed reference = boxed_new(integer_alloc(value));
  if (!reference)
    goto memory_error;
  return reference;
 memory_error:
  return NULL;
}

/* Unbox an integer. */
Integer integer_unbox(Boxed reference) {
  assert(reference);
  assert(is_integer(reference));
  Integer value = integer_value(reference);
  boxed_free(reference);
  return value;
}

/* Retrieve the value of an integer. */
Integer integer_value(Boxed reference) {
  assert(reference);
  assert(is_integer(reference));
  return reference->value->data.as_integer;
}

/* Test whether a value is an integer. */
int is_integer(Boxed reference) {
  return reference && boxed_type(reference) == INTEGER;
}

/* Test whether a value is a float. */
int is_float(Boxed reference) {
  return reference && boxed_type(reference) == FLOAT;
}

/* Test whether a value is a quotation. */
int is_quotation(Boxed reference) {
  return reference && boxed_type(reference) == QUOTATION;
}

/* Test whether a value is a word. */
int is_word(Boxed reference) {
  return reference && boxed_type(reference) == WORD;
}

/* Allocate an unboxed quotation. */
Unboxed quotation_alloc(int size) {
  assert(size >= 0);
  Unboxed reference = unboxed_alloc();
  if (!reference)
    goto error_allocating_value;
  reference->type = QUOTATION;
  reference->data.as_quotation.size = size;
  reference->data.as_quotation.capacity = size;
  if (size) {
    reference->data.as_quotation.data = malloc(size * sizeof(Boxed));
    if (!reference->data.as_quotation.data)
      goto error_allocating_contents;
  } else {
    reference->data.as_quotation.data = NULL;
  }
  return reference;
 error_allocating_contents:
  unboxed_free(reference);
 error_allocating_value:
  return NULL;
}

/* Copy all values from one quotation to the end of another. */
void quotation_append(Boxed destination, Boxed source) {
  assert(destination);
  assert(source);
  assert(is_quotation(destination));
  assert(is_quotation(source));
  int i;
  for (i = 0; i < quotation_size(source); ++i)
    quotation_push(destination, boxed_copy(quotation_data(source)[i]));
}

/* Apply one quotation to another. */
void quotation_apply(Boxed destination, Boxed source) {
  assert(destination);
  assert(source);
  assert(is_quotation(destination));
  assert(is_quotation(source));
  int i;
  for (i = 0; i < quotation_size(source); ++i) {
    if (is_word(quotation_data(source)[i]))
      word_apply(word_value(quotation_data(source)[i]), destination);
    else
      quotation_push(destination, boxed_copy(quotation_data(source)[i]));
  }
}

/* Empty the contents of a quotation. */
void quotation_clear(Boxed quotation) {
  assert(quotation);
  assert(is_quotation(quotation));
  int i;
  for (i = 0; i < quotation_size(quotation); ++i)
    boxed_free(quotation_data(quotation)[i]);
  free(quotation_data(quotation));
  quotation->value->data.as_quotation.size = 0;
  quotation->value->data.as_quotation.capacity = 0;
  quotation->value->data.as_quotation.data = NULL;
}

/* Determine the lexicographic ordering of two quotations. */
int quotation_compare(Boxed a, Boxed b) {
  if (!a && b) {
    assert(is_quotation(b));
    return -1;
  }
  if (a == b)
    return 0;
  assert(is_quotation(a));
  assert(is_quotation(b));
  int a_size = quotation_size(a);
  int b_size = quotation_size(b);
  Boxed *a_data = quotation_data(a);
  Boxed *b_data = quotation_data(b);
  int minimum = a_size < b_size ? a_size : b_size;
  int i;
  for (i = 0; i < minimum; ++i) {
    int test = boxed_compare(a_data[i], b_data[i]);
    if (test < 0)
      return -1;
    if (test > 0)
      return +1;
  }
  return a_size == b_size
    ? 0
    : a_size < b_size
      ? -1
      : +1;
}

/* Retrieve the data from a quotation. */
Boxed *quotation_data(Boxed quotation) {
  assert(quotation);
  assert(is_quotation(quotation));
  return quotation->value->data.as_quotation.data;
}

/* Create a boxed quotation. */
Boxed quotation_new(int size, ...) {
  Boxed reference = boxed_new(quotation_alloc(size));
  if (!reference)
    goto error_allocating_reference;
  va_list arguments;
  va_start(arguments, size);
  int i;
  for (i = 0; i < size; ++i)
    reference->value->data.as_quotation.data[i] = va_arg(arguments, Boxed);
  va_end(arguments);
  return reference;
 error_allocating_reference:
  return NULL;
}

/* Add a reference to the end of a quotation. NOTE: Takes ownership of the
   reference, i.e., does not automatically call boxed_copy(). */
void quotation_push(Boxed quotation_reference, Boxed reference) {
  assert(quotation_reference);
  assert(is_quotation(quotation_reference));
  Quotation *quotation = &quotation_reference->value->data.as_quotation;
  int capacity = quotation->capacity == 0
    ? 1
    : quotation->size == quotation->capacity
      ? quotation->capacity * 2
      : 0;
  if (capacity) {
    Boxed *data = realloc(quotation->data, capacity * sizeof(Boxed));
    if (!data)
      return;
    quotation->capacity = capacity;
    quotation->data = data;
  }
  quotation->data[quotation->size++] = reference;
}

/* Remove and retrieve the last reference in a quotation. */
Boxed quotation_pop(Boxed quotation_reference) {
  assert(quotation_reference);
  assert(is_quotation(quotation_reference));
  Quotation *quotation = &quotation_reference->value->data.as_quotation;
  assert(quotation->size > 0);
  return quotation->data[--quotation->size];
}

/* Retrieve the size of a quotation. */
int quotation_size(Boxed quotation) {
  assert(quotation);
  assert(is_quotation(quotation));
  return quotation->value->data.as_quotation.size;
}

/* Retrieve the last reference from a quotation. NOTE: Does not automatically
   call boxed_copy(). */
Boxed quotation_top(Boxed quotation_reference) {
  assert(quotation_reference);
  assert(is_quotation(quotation_reference));
  Quotation *quotation = &quotation_reference->value->data.as_quotation;
  if (!quotation->size)
    return NULL;
  return quotation->data[quotation->size - 1];
}

/* Create a raw unboxed reference. */
Unboxed unboxed_alloc(void) {
  return malloc(sizeof(Value));
}

/* Free an unboxed reference. */
void unboxed_free(Unboxed reference) {
  free(reference);
}

/* Allocate an unboxed word. */
Unboxed word_alloc(Word value) {
  Unboxed reference = unboxed_alloc();
  if (!reference)
    goto memory_error;
  reference->type = WORD;
  reference->data.as_word = value;
  return reference;
 memory_error:
  return NULL;
}

void word_apply(Word word, Boxed stack) {
  assert(stack);
  assert(is_quotation(stack));
  map[word](stack);
}

/* Create a boxed word. */
Boxed word_new(Word value) {
  Boxed reference = boxed_new(word_alloc(value));
  if (!reference)
    goto memory_error;
  return reference;
 memory_error:
  return NULL;
}

/* Unbox a word. */
Word word_unbox(Boxed reference) {
  assert(reference);
  assert(is_word(reference));
  Word value = word_value(reference);
  boxed_free(reference);
  return value;
}

/* Retrieve the value of a word. */
Word word_value(Boxed reference) {
  assert(reference);
  assert(is_word(reference));
  return reference->value->data.as_word;
}
