#include "types.h"

#include "builtins.h"
#include "debug.h"
#include "kitten.h"

#include <stdarg.h>
#include <stdlib.h>
#include <inttypes.h>

void utf8_append(uint32_t character, uint8_t *buffer);

/*
 * A mapping from words to implementations.
 *
 * TODO: Remove repetition.
 */
Implementation map[WORD_COUNT] = {
# define INIT(NAME) kitten_##NAME,
# define LAST(NAME) kitten_##NAME

  KITTEN_BUILTINS(INIT, LAST)

# undef LAST
# undef INIT
};

/*
 * Makes a (single-level-) deep copy of a boxed reference.
 *
 * OWNERSHIP: GIVE
 *
 * NOTE: References within quotations are cloned using boxed_copy() rather than
 * boxed_clone(), so that the cloned quotations are distinct but share elements.
 *
 * TODO: Assess whether single-level-deep copying is in fact desirable.
 */
Boxed boxed_clone(Boxed reference) {
  if (!reference)
    return NULL;
  switch (boxed_type(reference)) {
  case FLOAT:
    return float_new(float_value(reference));
  case INTEGER:
    return integer_new(integer_value(reference));
  case QUOTATION:
    {
      Boxed result = quotation_new(0);
      quotation_append(result, reference);
      return result;
    }
  case WORD:
    return word_new(word_value(reference));
  }
  return NULL;
}

/*
 * Tests two boxed values for typewise and value-wise equality.
 *
 * OWNERSHIP: TAKE
 */
int boxed_compare(Boxed unpromoted_a, Boxed unpromoted_b) {
  if (!unpromoted_a && unpromoted_b)
    return -1;
  if (unpromoted_a == unpromoted_b)
    return 0;
  Boxed a;
  Boxed b;
  int compatible_types = boxed_promote(unpromoted_a, unpromoted_b, &a, &b);
  if (!compatible_types)
    return boxed_type(unpromoted_a) < boxed_type(unpromoted_b)
      ? -1
      : boxed_type(unpromoted_b) < boxed_type(unpromoted_a)
        ? +1
        : 0;
  switch (boxed_type(a)) {
  case FLOAT:
    {
      Float value_a = float_unbox(a);
      Float value_b = float_unbox(b);
      return value_a < value_b
        ? -1
        : value_a > value_b
          ? +1
          : 0;
    }
  case INTEGER:
    {
      Integer value_a = integer_unbox(a);
      Integer value_b = integer_unbox(b);
      return value_a < value_b
        ? -1
        : value_a > value_b
          ? +1
          : 0;
    }
  case QUOTATION:
    {
      int result = quotation_compare(a, b);
      boxed_free(a);
      boxed_free(b);
      return result;
    }
  case WORD:
    {
      Word value_a = word_unbox(a);
      Word value_b = word_unbox(b);
      return value_a < value_b
        ? -1
        : value_a > value_b
          ? +1
          : 0;
    }
  }
  return 0;
}

/*
 * Makes a shallow copy of a boxed reference by simply incrementing its
 * reference count.
 *
 * OWNERSHIP: GIVE
 */
Boxed boxed_copy(Boxed reference) {
  if (!reference)
    return NULL;
  ++reference->count;
  global_alloc();
  return reference;
}

/*
 * Frees a boxed reference by decrementing its reference count and freeing the
 * unboxed reference if necessary.
 *
 * OWNERSHIP: TAKE
 */
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

/*
 * Creates a boxed reference from an unboxed reference. Returns NULL if its
 * argument is NULL or if allocation of a boxed reference fails.
 *
 * OWNERSHIP: GIVE
 */
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

/*
 * Performs type promotion of boxed values according to the following rules:
 *
 *   First     Second       First     Second
 *   float     float     -> float     float
 *   float     integer   -> float     float
 *   integer   float     -> float     float
 *   integer   integer   -> integer   integer
 *   quotation quotation -> quotation quotation
 *   quotation *         ->
 *   word      word      -> word
 *   word      *         ->
 *   *         quotation ->
 *   *         word      ->
 *
 * Supplies promoted values as out parameters, either as new values or as boxed
 * copies. Returns whether the given values are of compatible type.
 *
 * OWNERSHIP: GIVE
 *
 * TODO: Make this function obsolete by removing implicit conversions.
 */
int boxed_promote(Boxed unpromoted_a, Boxed unpromoted_b, Boxed *promoted_a,
  Boxed *promoted_b) {
  assert(unpromoted_a);
  assert(unpromoted_b);
  assert(promoted_a);
  assert(promoted_b);
  switch (boxed_type(unpromoted_a)) {
  case FLOAT:
    switch (boxed_type(unpromoted_b)) {
    case FLOAT:
      *promoted_a = boxed_copy(unpromoted_a);
      *promoted_b = boxed_copy(unpromoted_b);
      return 1;
    case INTEGER:
      *promoted_a = boxed_copy(unpromoted_a);
      *promoted_b = float_new(integer_value(unpromoted_b));
      return 1;
    case QUOTATION:
    case WORD:
      return 0;
    }
  case INTEGER:
    switch (boxed_type(unpromoted_b)) {
    case FLOAT:
      *promoted_a = float_new(integer_value(unpromoted_a));
      *promoted_b = boxed_copy(unpromoted_b);
      return 1;
    case INTEGER:
      *promoted_a = boxed_copy(unpromoted_a);
      *promoted_b = boxed_copy(unpromoted_b);
      return 1;
    case QUOTATION:
    case WORD:
      return 0;
    }
  case QUOTATION:
    switch (boxed_type(unpromoted_b)) {
    case QUOTATION:
      *promoted_a = boxed_copy(unpromoted_a);
      *promoted_b = boxed_copy(unpromoted_b);
      return 1;
    default:
      return 0;
    }
  case WORD:
    switch (boxed_type(unpromoted_b)) {
    case WORD:
      *promoted_a = boxed_copy(unpromoted_a);
      *promoted_b = boxed_copy(unpromoted_b);
      return 1;
    default:
      return 0;
    }
  }
  return 0;
}

/*
 * Write a UTF-32 code point to standard output as UTF-8.
 *
 * OWNERSHIP: NONE
 */
void boxed_putc(Boxed reference) {
  assert(reference);
  assert(is_integer(reference));
  Integer character = integer_value(reference);
  assert(character >= 0 && character <= UINT32_MAX);
  char buffer[5] = { 0 };
  utf8_append(character, (uint8_t*)buffer);
  printf("%s", buffer);
}

/*
 * Retrieve the type of a boxed reference.
 *
 * OWNERSHIP: NONE
 */
Type boxed_type(Boxed reference) {
  assert(reference);
  return reference->value->type;
}

/*
 * Writes a boxed value to standard output. Integers and floats are written as
 * such, while quotations are always interpreted as strings.
 *
 * OWNERSHIP: NONE
 *
 * TODO: Split this up into type-specific functions to avoid interpreting
 * non-strings as strings.
 */
void boxed_write(Boxed reference) {
  assert(reference);
  switch (boxed_type(reference)) {
  case INTEGER:
    printf("%" PRId64, integer_value(reference));
    break;
  case FLOAT:
    printf("%f", float_value(reference));
    break;
  case QUOTATION:
    {
      Boxed* data = quotation_data(reference);
      int size = quotation_size(reference);
      for (int i = 0; i < size; ++i)
        boxed_putc(data[i]);
    }
    break;
  default:
    break;
  }
}

/*
 * Allocate an unboxed float.
 *
 * OWNERSHIP: NONE
 */
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

/*
 * Create a boxed float.
 *
 * OWNERSHIP: GIVE
 */
Boxed float_new(Float value) {
  Boxed reference = boxed_new(float_alloc(value));
  if (!reference)
    goto memory_error;
  return reference;
 memory_error:
  return NULL;
}

/*
 * Unbox a float and return its value.
 *
 * OWNERSHIP: TAKE
 */
Float float_unbox(Boxed reference) {
  assert(reference);
  assert(is_float(reference));
  Float value = float_value(reference);
  boxed_free(reference);
  return value;
}

/*
 * Retrieve a copy of the value of a float.
 *
 * OWNERSHIP: NONE
 */
Float float_value(Boxed reference) {
  assert(reference);
  assert(is_float(reference));
  return reference->value->data.as_float;
}

/*
 * Allocate an unboxed integer.
 *
 * OWNERSHIP: NONE
 */
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

/*
 * Create a boxed integer.
 *
 * OWNERSHIP: GIVE
 */
Boxed integer_new(Integer value) {
  Boxed reference = boxed_new(integer_alloc(value));
  if (!reference)
    goto memory_error;
  return reference;
 memory_error:
  return NULL;
}

/*
 * Unbox an integer and return its value.
 *
 * OWNERSHIP: TAKE
 */
Integer integer_unbox(Boxed reference) {
  assert(reference);
  assert(is_integer(reference));
  Integer value = integer_value(reference);
  boxed_free(reference);
  return value;
}

/*
 * Retrieve a copy of the value of an integer.
 *
 * OWNERSHIP: NONE
 */
Integer integer_value(Boxed reference) {
  assert(reference);
  assert(is_integer(reference));
  return reference->value->data.as_integer;
}

/*
 * Tests whether a value is an integer.
 *
 * OWNERSHIP: NONE
 */
int is_integer(Boxed reference) {
  return reference && boxed_type(reference) == INTEGER;
}

/*
 * Tests whether a value is a float.
 *
 * OWNERSHIP: NONE
 */
int is_float(Boxed reference) {
  return reference && boxed_type(reference) == FLOAT;
}

/*
 * Tests whether a value is numeric.
 *
 * OWNERSHIP: NONE
 */
int is_numeric(Boxed reference) {
  return is_integer(reference) || is_float(reference);
}

/*
 * Tests whether a value is a quotation.
 *
 * OWNERSHIP: NONE
 */
int is_quotation(Boxed reference) {
  return reference && boxed_type(reference) == QUOTATION;
}

/*
 * Tests whether a value is a word.
 *
 * OWNERSHIP: NONE
 */
int is_word(Boxed reference) {
  return reference && boxed_type(reference) == WORD;
}

/*
 * Allocate an unboxed quotation.
 *
 * OWNERSHIP: NONE
 */
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

/*
 * Make shallow copies of all values in one quotation and append them to
 * another quotation.
 *
 * OWNERSHIP: NONE
 */
void quotation_append(Boxed target, Boxed source) {
  assert(target);
  assert(source);
  assert(is_quotation(target));
  assert(is_quotation(source));
  const int size = quotation_size(source);
  int i;
  for (i = 0; i < size; ++i)
    quotation_push(target, boxed_copy(quotation_data(source)[i]));
}

/*
 * Apply one quotation to another.
 *
 * OWNERSHIP: NONE
 */
void quotation_apply(Boxed target, Boxed source, Boxed definitions) {
  assert(target);
  assert(source);
  assert(is_quotation(target));
  assert(is_quotation(source));
  int i;
  for (i = 0; i < quotation_size(source); ++i) {
    if (is_word(quotation_data(source)[i]))
      word_apply(word_value(quotation_data(source)[i]), target, definitions);
    else
      quotation_push(target, boxed_copy(quotation_data(source)[i]));
  }
}

/*
 * Empty a quotation, freeing all of its contents, deallocating its buffer, and
 * setting its size and capacity to zero.
 *
 * OWNERSHIP: TAKE
 */
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

/*
 * Determine the lexicographic ordering of two quotations using boxed_compare().
 */
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

/*
 * Retrieve the data from a quotation.
 *
 * OWNERSHIP: NONE
 */
Boxed *quotation_data(Boxed quotation) {
  assert(quotation);
  assert(is_quotation(quotation));
  return quotation->value->data.as_quotation.data;
}

/*
 * Create a boxed quotation from an initial size and set of values.
 *
 * OWNERSHIP: GIVE
 */
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

/*
 * Add a reference to the end of a quotation.
 *
 * OWNERSHIP: TAKE
 */
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

/*
 * Remove and return the last reference in a quotation.
 *
 * OWNERSHIP: GIVE
 */
Boxed quotation_pop(Boxed quotation_reference) {
  assert(quotation_reference);
  assert(is_quotation(quotation_reference));
  Quotation *quotation = &quotation_reference->value->data.as_quotation;
  assert(quotation->size > 0);
  return quotation->data[--quotation->size];
}

/*
 * Retrieve the size of a quotation.
 *
 * OWNERSHIP: NONE
 */
int quotation_size(Boxed quotation) {
  assert(quotation);
  assert(is_quotation(quotation));
  return quotation->value->data.as_quotation.size;
}

/*
 * A convenience function to retrieve the last reference in a quotation without
 * altering its reference count.
 *
 * OWNERSHIP: NONE
 */
Boxed quotation_top(Boxed quotation_reference) {
  assert(quotation_reference);
  assert(is_quotation(quotation_reference));
  Quotation *quotation = &quotation_reference->value->data.as_quotation;
  if (!quotation->size)
    return NULL;
  return quotation->data[quotation->size - 1];
}

/*
 * Create a raw unboxed reference.
 */
Unboxed unboxed_alloc(void) {
  return malloc(sizeof(Value));
}

/*
 * Free an unboxed reference.
 */
void unboxed_free(Unboxed reference) {
  free(reference);
}

/*
 * Append a UTF-32 code point to a buffer as UTF-8.
 *
 * TODO: Migrate to a more sensible location.
 */
void utf8_append(uint32_t code_point, uint8_t *result) {
  if (code_point < 0x80) {
    *result++ = (uint8_t)(code_point);
  } else if (code_point < 0x800) {
    *result++ = (uint8_t)(((code_point >> 6)       ) | 0xc0);
    *result++ = (uint8_t)(((code_point     ) & 0x3f) | 0x80);
  } else if (code_point < 0x10000) {
    *result++ = (uint8_t)(((code_point >> 12)       ) | 0xe0);
    *result++ = (uint8_t)(((code_point >>  6) & 0x3f) | 0x80);
    *result++ = (uint8_t)(((code_point      ) & 0x3f) | 0x80);
  } else {
    *result++ = (uint8_t)(((code_point >> 18)       ) | 0xf0);
    *result++ = (uint8_t)(((code_point >> 12) & 0x3f) | 0x80);
    *result++ = (uint8_t)(((code_point >>  6) & 0x3f) | 0x80);
    *result++ = (uint8_t)(((code_point      ) & 0x3f) | 0x80);
  }
}

/*
 * Allocate an unboxed word.
 */
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

/*
 * Apply a word to a quotation.
 */
void word_apply(Word word, Boxed stack, Boxed definitions) {
  assert(stack);
  assert(is_quotation(stack));
  if (word < 0) {
    assert(-word - 1 < quotation_size(definitions));
    quotation_apply
      (stack, quotation_data(definitions)[-word - 1], definitions);
  } else {
    map[word](stack, definitions);
  }
}

/*
 * Create a boxed word.
 *
 * OWNERSHIP: GIVE
 */
Boxed word_new(Word value) {
  Boxed reference = boxed_new(word_alloc(value));
  if (!reference)
    goto memory_error;
  return reference;
 memory_error:
  return NULL;
}

/*
 * Unbox a word and return a copy of its value.
 *
 * OWNERSHIP: TAKE
 */
Word word_unbox(Boxed reference) {
  assert(reference);
  assert(is_word(reference));
  Word value = word_value(reference);
  boxed_free(reference);
  return value;
}

/*
 * Retrieve a copy of the value of a word.
 *
 * OWNERSHIP: NONE
 */
Word word_value(Boxed reference) {
  assert(reference);
  assert(is_word(reference));
  return reference->value->data.as_word;
}
